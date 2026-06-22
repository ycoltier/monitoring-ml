##############################
#          Imports           #
##############################

# Packages

library(arrow)
library(dplyr)
library(ggplot2)
library(purrr)
library(scales)


##############################
#         Functions          #
##############################

# Plots

# Représente le taux de convergence des données codées automatiquement en fonction du volume de codages automatiques (et donc indirectement du seuil de confiance)
plot_accuracy_volume <- function(
    df,
    nb_positions_max = 4
){
  
  ggplot(df, aes(
    x = proportion_codages_auto,
    y = accuracy_codages_auto)
  ) +
    geom_line() +
    labs(
      x = "Proportion de codages automatiques",
      y = "Taux de codages automatiques corrects",
      title = "Qualité de la codification / proportion de codages automatiques"
    ) +
    scale_x_continuous(labels = percent_format(accuracy = 1)) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    theme_minimal() +
    theme(legend.title = element_blank())
}


# Counts the weighted occurrences of each PCS code in the given sequence
count_apparitions_codes <- function(codes, weights) {
  apparitions <- setNames(rep(0, length(unique(codes))), unique(codes))
  
  for (i in seq_along(codes)) {
    apparitions[codes[i]] <- apparitions[codes[i]] + weights[i]
  }
  
  return(data.frame(code = names(apparitions), weight = apparitions))
}


# Compares the distribution of "true" codes and predicted ones
plot_distribution_cs <- function(targets, predictions, weights = NULL, nb_positions_max = 2) {
  stopifnot(length(targets) == length(predictions))
  stopifnot(is.null(weights) || length(weights) == length(targets))
  
  if (is.null(weights)) {
    weights <- rep(1, length(targets))
  }
  
  distrib_targets <- count_apparitions_codes(
    sapply(targets, truncate_positions, threshold = nb_positions_max),
    weights
  )
  distrib_preds <- count_apparitions_codes(
    sapply(predictions, truncate_positions, threshold = nb_positions_max),
    weights
  )
  
  distributions <- merge(
    distrib_targets,
    distrib_preds,
    by = "code",
    all = TRUE,
    suffixes = c("_target", "_prediction")
  )
  
  distributions[is.na(distributions)] <- 0
  distributions$code <- truncate_positions(distributions$code, nb_positions_max)
  distributions <- distributions[order(distributions$code),]
  
  ggplot(distributions, aes(x = code)) +
    geom_bar(aes(y = weight_target, fill = "Codages initiaux"), stat = "identity", position = position_nudge(x = -0.15), width = 0.3) +
    geom_bar(aes(y = weight_prediction, fill = "Prédictions FastText"), stat = "identity", position = position_nudge(x = 0.15), width = 0.3) +
    scale_fill_manual(name = "Légende", values = c("Codages initiaux" = "blue", "Prédictions FastText" = "red")) +
    labs(x = "Code", y = "Quantité", title = "Distribution des codes") +
    theme_minimal() +
    theme(legend.title = element_blank()) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.5, linetype = "solid", alpha = 0.2)
}


# Prints the density distribution of the given scores
plot_distribution_scores <- function(scores, weights = NULL, threshold = 0.1) {
  stopifnot(is.null(weights) || length(weights) == length(scores))
  
  if (is.null(weights)) {
    weights <- rep(1, length(scores))
  }
  
  df <- data.frame(scores = scores, weights = weights)
  
  ggplot(df, aes(x = scores, weight = weights)) +
    geom_density(fill = "blue", alpha = 0.5, adjust = 1, bw = "nrd0", trim = TRUE, na.rm = TRUE) +
    geom_vline(xintercept = threshold, linetype = "dashed", color = "black") +
    annotate("text", label=paste("Seuil à", threshold), x=threshold-0.02, y=0.1, angle=90, hjust=0) +
    labs(
      title = "Distribution du score",
      x = "Score",
      y = "Densité"
    ) +
    xlim(-0.05, 1.05) +
    theme_minimal() +
    theme(
      legend.title = element_blank(),
      plot.title = element_text(hjust = 0.5)
    )
}


# Prints the density distribution of the scores, differentiating correct and incorrect predictions
plot_distribution_scores_knowing_correct_codes <- function(targets, predictions, scores, weights = NULL, nb_positions_max = 4, threshold = 0.1) {
  stopifnot(length(targets) == length(predictions))
  stopifnot(length(targets) == length(scores))
  stopifnot(is.null(weights) || length(weights) == length(targets))
  
  if (is.null(weights)) {
    weights <- rep(1, length(targets))
  }
  
  targets <- as.character(targets)
  predictions <- as.character(predictions)
  is_correct <- mapply(
    function(target, prediction) {
      truncate_positions(target, nb_positions_max) == truncate_positions(prediction, nb_positions_max)
    },
    targets,
    predictions
  )
  
  df <- data.frame(scores = scores, weights = weights, is_correct = factor(is_correct, levels = c(TRUE, FALSE), labels = c("Correcte", "Incorrecte")))
  
  ggplot(df, aes(x = scores, fill = is_correct, weight = weights)) +
    geom_density(alpha = 0.5, adjust = 1, bw = "nrd0", trim = TRUE, na.rm = TRUE, position = "identity") +
    geom_vline(xintercept = threshold, linetype = "dashed", color = "black") +
    annotate("text", label=paste("Seuil à", threshold), x=threshold-0.02, y=0.1, angle=90, hjust=0) +
    labs(
      title = paste("Distribution du score -", nb_positions_max, "positions"),
      x = "Score",
      y = "Densité",
      fill = paste("Pred (sur", nb_positions_max, "pos.)")
    ) +
    xlim(-0.05, 1.05) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.title = element_text(hjust = 0.5)
    )
}


# Plots the visualizations that may help fix the score threshold
plot_pour_fixation_seuil <- function(df_metrics_par_seuil, metric_y = "accuracy_codages_auto", threshold = 0.1) {
  stopifnot(metric_y %in% colnames(df_metrics_par_seuil))
  
  ggplot(df_metrics_par_seuil, aes(x = seuil, y = .data[[metric_y]])) +
    geom_line(color = 'blue', linetype = 'solid', linewidth = 1) +
    geom_vline(xintercept = threshold, linetype = 'dashed', color = 'black') +
    annotate("text", label=paste("Seuil à", threshold), x=threshold-0.02,
             y=0.5, angle=90, hjust=0) +
    labs(
      title = paste(metric_y, "en fonction du seuil de score fixé"),
      x = 'Seuil de score choisi',
      y = metric_y
    ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "bottom"
    )
}


# Plots the proportion of observations for which the true code is within 
# the top k predictions of the model for varying k
plot_presence_target_dans_top_5 <- function(df) {
  
  proportions <-df %>%
    mutate(
      match_1 = target %in% preds_1,
      match_2 = match_1 | target %in% preds_2,
      match_3 = match_2 | target %in% preds_3,
      match_4 = match_3 | target %in% preds_4,
      match_5 = match_4 | target %in% preds_5
    ) %>%
    summarise(
      prop_1 = mean(match_1),
      prop_2 = mean(match_2),
      prop_3 = mean(match_3),
      prop_4 = mean(match_4),
      prop_5 = mean(match_5)
    ) %>%
    pivot_longer(cols = everything(), names_to = "Top_N", values_to = "Proportion")
  
  # Remplacement des noms pour affichage propre
  proportions <- proportions %>%
    mutate(Top_N = gsub("prop_", "Top ", Top_N))
  
  ggplot(proportions, aes(x = Top_N, y = Proportion, fill = Top_N)) +
    geom_bar(stat = "identity", fill = 'skyblue') +
    geom_text(aes(label = scales::percent(Proportion, accuracy = 1)), vjust = -0.5) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      title = "Proportion des observations où le code annoté est dans le top 5",
      x = "Top 5 prédictions",
      y = "Proportion (%)"
    ) +
    theme_minimal()
}

#### Plot for precision, recall and F1 Score ####

plot_metrics <- function(df, threshold){
  ggplot(df, aes(x = seuils))+
    geom_line(aes(y = precision, colour = 'Precision'))+
    geom_line(aes(y = recall, colour = 'Recall'))+
    geom_line(aes(y = f1, colour = 'F1-Score'))+
    geom_vline(xintercept = threshold, linetype = 'dashed', color = 'black') +
    annotate("text", label=paste("Seuil à", threshold), x=threshold, 
             y=min(df$precision)+0.05, angle=90, hjust=0)+
    scale_color_manual(name = "Métriques", 
                       values = c("Precision" = "black", 
                                  "Recall" = "blue",
                                  "F1-Score" = "red"))+
    scale_y_continuous(labels = scales::percent_format(accuracy = 2),
                       breaks=seq(0,1,by=0.1))+
    scale_x_continuous(breaks=seq(0,1,by=0.1))+labs(
      title = "Métriques en fonction du seuil de confiance",
      x = "seuil de confiance",
      y = "Taux (%)"
    )+
    theme_minimal() + 
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "right"
    )
  
}


#### Plot for accuracy below score ####

plot_acc_ss_score <- function(df){
  ggplot(df, aes(x = seuils))+
    geom_line(aes(y = invacc_ss_score))+
    scale_y_continuous(labels = scales::percent_format(accuracy = 2),
                       breaks=seq(0,1,by=0.1))+
    scale_x_continuous(breaks=seq(0,1,by=0.1))+labs(
      title = "Taux de mal prédits sous le seuil de confiance",
      x = "seuil de confiance",
      y = "Taux (%)"
    )+
    theme_minimal() + 
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "right"
    )
}

#### Plot pour matrice confusion ----

plot_conf_mat <- function(df,
                          target = "target",
                          prediction = "prediction",
                          niveau = 2){
  ggplot(conf_mat_plot(df, target, prediction, niveau), aes(Prediction, Truth, fill = prop)) +
    geom_tile(color = "grey85") +
    scale_fill_gradient(low = "white", high = "#2c7be5") +
    geom_text(aes(label = sprintf("%.1f", prop)), size = 3) +
    labs(fill = "Proportion") +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    )
}
