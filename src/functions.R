###################
#### Functions ####
###################

compter_valeurs_manquantes <- function(x) {
  sum(is.na(x) | (is.character(x) & trimws(x) == ""))
}

conf_mat_plot <- function(df, 
                          target = "target", 
                          prediction = "prediction",
                          niveau = 2){
  
  library(dplyr)
  library(yardstick)
  
  # --- sécurité ---
  stopifnot(target %in% names(df))
  stopifnot(prediction %in% names(df))
  
  # extraire vecteurs
  target_vec <- as.character(df[[target]])
  pred_vec   <- as.character(df[[prediction]])
  
  # appliquer ta fonction (sans la modifier)
  df <- df %>%
    mutate(
      target2     = truncate_positions(target_vec, niveau),
      prediction2 = truncate_positions(pred_vec, niveau)
    )
  
  # niveaux cohérents
  lvls <- sort(unique(c(df$target2, df$prediction2)))
  
  # confusion matrix
  cm <- yardstick::conf_mat(
    df %>% mutate(
      target = factor(target2, levels = lvls),
      prediction = factor(prediction2, levels = lvls)
    ),
    truth = target,
    estimate = prediction
  )
  
  # normalisation ligne
  cm_norm <- cm$table %>%
    as.data.frame() %>%
    group_by(Truth) %>%
    mutate(prop = Freq / sum(Freq)) %>%
    ungroup()
  
  # ordre par qualité du modèle (diagonale)
  order_class <- cm_norm %>%
    filter(Truth == Prediction) %>%
    arrange(desc(prop)) %>%
    pull(Truth)
  
  cm_norm$Truth      <- factor(cm_norm$Truth, levels = order_class)
  cm_norm$Prediction <- factor(cm_norm$Prediction, levels = order_class)
  
  return(cm_norm)
}


# Calcul métriques

# nettoyage des variables contenant le code de la nomenclature
clean_code_column <- function(x) {
  x <- as.character(x)
  x <- trimws(x)          # supprime espaces avant/après
  x
}

# Normalize code and truncate positions
truncate_positions <- function(code, threshold = 4) {
  # Vectorisation native
  if (length(code) > 1) {
    return(sapply(code, truncate_positions, threshold = threshold))
  }
  stopifnot(is.character(code))
  stopifnot(nchar(code) <= 4)
  stopifnot(threshold >= 0 & threshold <= 4)
  
  if (code %in% c("1", "10", "100", "1000")) {
    if (threshold >= 2) {
      return("10")
    } else if (threshold == 1) {
      return("1")
    }
    return("")
  }
  if (code == "NC" || code == "") {
    return("")
  }
  
  i <- nchar(code) - 1
  while (i > -1 && substr(code, i + 1, i + 1) == "0") {
    i <- i - 1
  }
  return(substr(code, 1, min(threshold, i + 1)))
}


compte_positions <- function(code) {
  return(nchar(truncate_positions(code, 4)))
}
