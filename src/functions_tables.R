##############################
#         Functions          #
##############################

#df<-arrow::read_parquet("./data/surveys/exemple.parquet")

#### écart de prédiction entre les codages correctes et les codages prédits ####

table_ecart <- function(df, target, prediction, niveau){
  table_ecart <- df %>%
    mutate(target_niv = truncate_positions(target, threshold = niveau),
           prediction_niv = truncate_positions(prediction,threshold =niveau)) %>%
    mutate(ind_ecart = ifelse(target_niv==prediction_niv,1,0)) %>%
    group_by(target_niv) %>%
    summarise(freq_predit = sum(ind_ecart) / n(),
              total = n()) %>%
    arrange(freq_predit)
  
  return(table_ecart)
}

#### Tableau metriques seuils ####

compute_metrics_par_seuil <- function(
    df,
    niveau = 4,
    seuil_min = 0,
    seuil_max = 1,
    nb_points = 100,
    tx_erreur_humaine = 0,
    volume_globale = 100,
    tx_recours_liste = 0,
    seed = 42
) {
  
  seuils <- seq(seuil_min, seuil_max, by = 1 / nb_points)
  
  if (is.null(df$weight)) df$weight <- 1
  
  # Pré-calculer une seule fois les labels tronqués
  target_niv     <- truncate_positions(df$target,niveau)
  prediction_niv <- truncate_positions(df$prediction, niveau)
  classes        <- sort(unique(target_niv))
  correct        <- target_niv == prediction_niv

  # --- Simulation du re-codage humain -------------------------------
  # Simulée UNE SEULE FOIS hors de la boucle sur les seuils : chaque
  # observation a un destin fixe (reprise correcte ou erreur simulée).
  # L'erreur simulée est un code de nomenclature valide (une classe de
  # `classes`) DIFFÉRENT du code vrai : sinon l'erreur se compense et le
  # taux d'erreur simulé serait sous-estimé.
  recodage_humain <- target_niv
  if (tx_erreur_humaine > 0 && length(classes) > 1) {
    seed_saved <- .Random.seed
    if (!is.null(seed)) set.seed(seed)
    n_obs <- length(df$score)
    err   <- runif(n_obs) < tx_erreur_humaine
    if (any(err)) {
      k   <- length(classes)
      idx_true <- match(target_niv, classes)
      draws    <- sample(k, sum(err), replace = TRUE)
      # Si le tirage tombe sur la classe vraie, on décale d'un rang pour
      # garantir une classe différente (cercle modulo k).
      fix    <- (draws - 1) %% k + 1
      draws[draws == idx_true[err]] <- fix[draws == idx_true[err]]
      recodage_humain[err] <- classes[draws]
    }
    # Si length(classes) == 1, une erreur devrait être un code différent
    # de la classe vraie, ce qui est impossible : on laisse l'observation
    # correctement re-codée (pas d'erreur simulée).
    
    # Restauration explicite de l'état du RNG (le snapshot est fait en
    # tête de bloc). Les handlers on.exit ne survivant pas à la fin du
    # bloc if, on restaure à la main.
    .Random.seed <- seed_saved
  }
  
  results <- lapply(seuils, function(s) {
    
    # Calcul taux correct avec score sous seuil par seuil
    idx_reprise <- df$score < s
    correct_brut <- target_niv == prediction_niv
    invacc_ss_score <- if (sum(idx_reprise) == 0) NA_real_ else {
      1 - sum(df$weight[idx_reprise & correct_brut]) / sum(df$weight[idx_reprise])
    }
    
    # Prédiction ajustée selon le seuil : auto (score >= s) ou re-codage
    # humain simulé, précalculé une fois pour toutes hors de la boucle.
    pred_adj <- ifelse(df$score >= s, prediction_niv, recodage_humain)
    idx_auto <- df$score >= s
    
    # --- Métriques pour accuracy_codages_auto_variation_seuil ---
    prop_auto     <- mean(idx_auto)
    accuracy_auto <- if (sum(idx_auto) == 0) NA_real_ else {
      sum(df$weight[idx_auto & correct]) / sum(df$weight[idx_auto])
    }
    
    # --- Métriques pour table_metric ---
    per_class <- vapply(classes, function(cls) {
      tp      <- sum(pred_adj == cls & target_niv == cls)
      fp      <- sum(pred_adj == cls & target_niv != cls)
      fn      <- sum(pred_adj != cls & target_niv == cls)
      tn      <- sum(pred_adj != cls & target_niv != cls)
      support <- tp + fn
      prec    <- if ((tp + fp) == 0) 0 else tp / (tp + fp)
      rec     <- if ((tp + fn) == 0) 0 else tp / (tp + fn)
      fpr     <- if ((fp + tn) == 0) 0 else fp / (fp + tn)
      f1      <- if ((prec + rec) == 0) 0 else 2 * prec * rec / (prec + rec)
      c(prec, rec, fpr, f1, support)
    }, numeric(5))
    
    w_cls <- per_class[5, ] / sum(per_class[5, ])
    
    precision_glob <- sum(per_class[1, ] * w_cls)
    recall_glob    <- sum(per_class[2, ] * w_cls)
    fpr_glob       <- sum(per_class[3, ] * w_cls)
    f1_glob        <- if ((precision_glob + recall_glob) == 0) 0 else {
      2 * precision_glob * recall_glob / (precision_glob + recall_glob)
    }
    
    
    data.frame(
      seuils                  = s,
      # Pour plot_accuracy_volume / plot_pour_fixation_seuil
      seuil                   = s,
      proportion_codages_auto = prop_auto,
      accuracy_codages_auto   = accuracy_auto,
      # Pour plot_metrics / résumé
      accuracy                = mean(pred_adj == target_niv),
      precision               = precision_glob,
      recall                  = recall_glob,
      fpr                     = fpr_glob,
      f1                      = f1_glob,
      obs_reprise             = sum(!idx_auto),
      tx_reprise              = mean(!idx_auto),
      invacc_ss_score         = invacc_ss_score
    )%>%
      mutate(
        accuracy_glob = accuracy * (1-tx_recours_liste) + 1 * tx_recours_liste,
        tx_reprise_glob = tx_reprise * (1-tx_recours_liste) + 0 * tx_recours_liste,
        obs_reprise_glob = round(tx_reprise * (1-tx_recours_liste) * volume_globale,0)
      )
  })
  
  bind_rows(results)
}


accuracy_codages_auto_variation_seuil <- function(
    targets, predictions, scores, weights = NULL, nb_positions_max = 4, ...
) {
  df <- data.frame(target = targets, prediction = predictions, 
                   score = scores, weight = if(is.null(weights)) 1 else weights)
  compute_metrics_par_seuil(df, niveau = nb_positions_max)
}

#### Extraction matrice de confusion ----

extract_conf_mat <- function(df,
                             target = "target",
                             prediction = "prediction",
                             niveau = 2) {
  
  result <- conf_mat_plot(df, target, prediction, niveau = niveau) %>%
    filter(Truth != Prediction & (Freq >= 10 | prop > 0.5)) %>%
    arrange(desc(prop))%>%
    mutate(prop = sprintf("%.1f%%", prop * 100))%>%
    select(Truth, Prediction, Freq, prop)
  return(result)
  
}

