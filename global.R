options(encoding = "UTF-8")

#### Chargement des packages ----
required_packages <- c("shiny", "shinyjs", "DT", "plotly", "arrow", "digest", 
                       "tidyverse", "waiter", "yardstick", "gt")

for(pkg in required_packages) {
  if(!require(pkg, character.only = TRUE)){
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

options(shiny.trace = FALSE)

#### Chargement des fonctions ####
source(file = "src/functions.R")
source(file = "src/functions_tables.R")
source(file = "src/functions_plots.R")


#### Configuration des répertoires de données ####
DATA_DIR  <- "data/surveys"
CACHE_DIR <- "data/cache"

dir.create(DATA_DIR,  showWarnings = FALSE, recursive = TRUE)
dir.create(CACHE_DIR, showWarnings = FALSE, recursive = TRUE)

#### Fonctions de gestion des données Parquet ####


# Sauvegarder une enquête
save_survey <- function(data, survey_name) {
  filepath <- file.path(DATA_DIR, paste0(survey_name, ".parquet"))
  arrow::write_parquet(data, filepath)
  invalidate_cache(survey_name)
  return(TRUE)
}

# Charger une enquête
load_survey <- function(survey_name) {
  filepath <- file.path(DATA_DIR, paste0(survey_name, ".parquet"))
  if (file.exists(filepath)) {
    return(arrow::read_parquet(filepath))
  }
  return(NULL)
}

# Charger toutes les enquêtes (pour "Ensemble")
load_all_surveys <- function() {
  surveys <- setdiff(list_surveys(), "exemple")
  if (length(surveys) == 0) return(NULL)
  all_data <- lapply(surveys, load_survey)
  return(bind_rows(all_data))
}

# Lister toutes les enquêtes
list_surveys <- function() {
  files <- list.files(DATA_DIR, pattern = "\\.parquet$")
  return(gsub("\\.parquet$", "", files))
}

# Supprimer une enquête
delete_survey <- function(survey_name) {
  filepath <- file.path(DATA_DIR, paste0(survey_name, ".parquet"))
  if (file.exists(filepath)) file.remove(filepath)
  invalidate_cache(survey_name)
  return(TRUE)
}

#### Fonctions de mise en cache ####

# Générer une clé de cache unique basée sur les données et le niveau
get_cache_key <- function(data, 
                          survey_name, 
                          niveau, 
                          tx_erreur_humaine = 0,
                          volume_globale = 100,
                          tx_recours_liste = 0) {
  data_hash <- digest::digest(list(
    nrow(data),
    sum(data$score,  na.rm = TRUE),
    sum(data$weight, na.rm = TRUE),
    niveau,
    tx_erreur_humaine,
    volume_globale,
    tx_recours_liste
  ))
  return(paste0(survey_name, "_niv", niveau, "_", data_hash))
}

# Écrire dans le cache et nettoyer les anciens fichiers
write_cache <- function(result, cache_key) {
  cache_file <- file.path(CACHE_DIR, paste0(cache_key, ".parquet"))
  arrow::write_parquet(result, cache_file)
  
  # Nettoyer les anciens caches pour ce même couple enquête+niveau
  prefix <- sub("_[^_]+$", "", cache_key)  # retire le hash final
  old_caches <- setdiff(
    list.files(CACHE_DIR, pattern = paste0("^", prefix, "_.*\\.parquet$"), full.names = TRUE),
    cache_file
  )
  if (length(old_caches) > 0) file.remove(old_caches)
  
  return(invisible(TRUE))
}

# Lire depuis le cache
read_cache <- function(cache_key) {
  cache_file <- file.path(CACHE_DIR, paste0(cache_key, ".parquet"))
  if (file.exists(cache_file)) return(arrow::read_parquet(cache_file))
  return(NULL)
}

# Invalider tout le cache d'une enquête (après save ou delete)
invalidate_cache <- function(survey_name) {
  cache_files <- list.files(
    CACHE_DIR,
    pattern = paste0("^", survey_name, "_|^Ensemble_"),
    full.names = TRUE
  )
  if (length(cache_files) > 0) {
    file.remove(cache_files)
    message(paste0("Cache invalidé pour : ", survey_name))
  }
  return(invisible(TRUE))
}

# Fonction principale : récupère depuis le cache ou calcule
get_cached_metrics <- function(data, 
                               survey_name, 
                               niveau = 4, 
                               tx_erreur_humaine = 0,
                               volume_globale = 100,
                               tx_recours_liste = 0) {
  cache_key <- get_cache_key(data, 
                             survey_name, 
                             niveau,
                             tx_erreur_humaine, 
                             volume_globale, 
                             tx_recours_liste)
  
  cached <- read_cache(cache_key)
  if (!is.null(cached)) {
    message(paste0("Cache hit  : ", survey_name, " (niveau ", niveau, ")"))
    return(cached)
  }
  
  message(paste0("Cache miss : calcul en cours pour ", survey_name, " (niveau ", niveau, ")"))
  result <- compute_metrics_par_seuil(data, 
                                      niveau = niveau,
                                      tx_erreur_humaine = tx_erreur_humaine,
                                      volume_globale = volume_globale, 
                                      tx_recours_liste = tx_recours_liste
  )
  write_cache(result, cache_key)
  
  return(result)
}

clean_cache <- function() {
  cache_files <- list.files(CACHE_DIR, full.names = TRUE)
  files_to_delete <- cache_files[!grepl("README", basename(cache_files))]
  if (length(files_to_delete) > 0) file.remove(files_to_delete)
  return(invisible(TRUE))
}

#### Initialisation : Créer l'enquête exemple ####
if (!file.exists(file.path(DATA_DIR, "exemple.parquet"))) {
  data_exemple <- data.frame(
    prediction = c("42B7", "63E1", "3000", "43D1", "55A1"),
    target     = c("63B7", "63E1", "37B2", "43D1", "55A1"),
    score      = c(0.251453102, 0.488865614, 0.003985927, 0.264889896, 0.635989547),
    weight     = c(5, 4, 1, 1, 5),
    stringsAsFactors = FALSE
  )
  save_survey(data_exemple, "exemple")
}

enquetes     <- list_surveys()
names_column <- list("")