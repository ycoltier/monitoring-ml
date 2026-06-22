server <- function(input, output, session) {
  
  # Désactiver les boutons au démarrage
  disable("treatData")
  disable("loadDataLocal")
  disable("sigle_enquete")
  
  #### LECTURE DONNEES ####
  
  data_consult <- reactiveVal(NULL)
  data_treat <- reactiveVal(NULL)
  data_reactive <- reactiveVal(NULL)
  current_survey <- reactiveVal(NULL)
  
  observeEvent(input$loadData, {
    
    # ---- RÉINITIALISATION ----
    output$check_result <- renderText("")
    output$check_save <- renderText("")
    data_treat(NULL)
    disable("loadDataLocal")
    # ---- FIN RÉINITIALISATION ----
    
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    
    ext <- tools::file_ext(inFile$name)
    
    if (ext == "parquet") {
      df <- arrow::read_parquet(inFile$datapath)
      
      if (!is.data.frame(df)) {
        stop("Le fichier chargé n'est pas un data.frame valide.")
      }
      
      # Traitement des colonnes liste
      list_column_name <- NULL
      for (col_name in names(df)) {
        if (inherits(df[[col_name]], "list")) {
          list_column_name <- col_name
          break
        }
      }
      
      if (!is.null(list_column_name)) {
        df <- df %>%
          mutate(row_id = row_number()) %>%
          unnest(cols = c({{ list_column_name }})) %>%
          group_by(row_id) %>%
          mutate(name = paste0(substr(list_column_name,
                                      nchar(list_column_name)-4,
                                      nchar(list_column_name)),
                               "_", row_number())) %>%
          pivot_wider(names_from = name, values_from = {{ list_column_name }}) %>%
          ungroup() %>%
          select(-row_id)
      }
      
    } else if (ext == "csv") {
      tryCatch({
        df <- read.csv(inFile$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      }, error = function(e) {
        stop(safeError(e))
      })
    } else {
      return(NULL)
    }
    
    # Ajouter colonne weight si absente
    if (!("weight" %in% colnames(df))) {
      df$weight <- 1
    }
    
    data_consult(df)
    
    names_column <- colnames(df)
    
    updateSelectInput(session, "rename_prediction", choices = c('prediction', names_column))
    updateSelectInput(session, "rename_target", choices = c('target', names_column))
    updateSelectInput(session, "rename_score", choices = c('score', names_column))
    updateSelectInput(session, "rename_weight", choices = c('weight', names_column))
    
    enable("treatData")
  })
  
  output$table <- renderDT({
    req(data_consult())
    datatable(data_consult(), editable = FALSE)
  })
  
  #### Renommage des variables ####
  
  observeEvent(input$treatData, {
    df <- tryCatch({
      data_consult() %>%
        dplyr::rename(
          prediction = input$rename_prediction,
          target = input$rename_target,
          score = input$rename_score,
          weight = input$rename_weight
        ) %>%
        dplyr::select(prediction, target, score, weight)
    }, error = function(e) {
      output$check_result <- renderText(
        "⚠️ Problème dans la sélection des variables !"
      )
      return(NULL)
    })
    
    required_columns <- c("prediction", "target", "score", "weight")
    
    missing_cols <- setdiff(required_columns, colnames(df))
    
    na_par_col <- sapply(df[required_columns], compter_valeurs_manquantes)
    
    cols_avec_na <- na_par_col[na_par_col > 0]
    
    if (length(missing_cols) > 0) {
      output$check_result <- renderText({
        paste("⚠️ Colonnes manquantes :", paste(missing_cols, collapse = ", "))
      })
      disable("loadDataLocal")
      disable("sigle_enquete")
      data_treat(NULL)
    } else if (length(cols_avec_na) > 0) {
      output$check_result <- renderText(paste0(
        "⚠️ Import bloqué : valeurs manquantes ou vides détectées :\n",
        paste0("  - ", names(cols_avec_na), " : ", cols_avec_na, " valeur(s)", collapse = "\n"),
        "\n\nVeuillez corriger le fichier source avant de réimporter."
      ))
      disable("loadDataLocal")
      disable("sigle_enquete")
      data_treat(NULL)
    } else {
      output$check_result <- renderText({
        "✅ Toutes les colonnes requises sont présentes !"
      })
      enable("loadDataLocal")
      enable("sigle_enquete")
      data_treat(df)
    }
  })
  
  output$table_vf <- renderDT({
    req(data_treat())
    datatable(data_treat(), editable = FALSE)
  })
  
  #### Sauvegarde locale (Parquet) ####
  
  observeEvent(input$loadDataLocal, {
    req(data_treat(), input$sigle_enquete)
    
    data_to_save <- data.frame(
      prediction = data_treat()$prediction,
      target = data_treat()$target,
      score = as.numeric(data_treat()$score),
      weight = as.numeric(data_treat()$weight),
      stringsAsFactors = FALSE
    )
    
    save_survey(data_to_save, input$sigle_enquete)
    
    # Actualiser la liste
    enquetes <- list_surveys()
    updateSelectInput(session, "selectsurvey", choices = c(enquetes, "Ensemble"))
    updateSelectInput(session, "selectsurvey2", choices = enquetes)
    updateSelectInput(session, "selectsurvey3", choices = c(enquetes, "Ensemble"))
    
    output$check_save <- renderText({
      paste0("✅ L'enquête '", input$sigle_enquete, "' a été sauvegardée !")
    })
  })
  
  #### Charger les données ####
  
  # Synchronisation selectsurvey -> selectsurvey3
  observeEvent(input$selectsurvey, {
    updateSelectInput(session, "selectsurvey3", selected = input$selectsurvey)
  }, ignoreInit = TRUE)
  
  # Synchronisation selectsurvey3 -> selectsurvey
  observeEvent(input$selectsurvey3, {
    updateSelectInput(session, "selectsurvey", selected = input$selectsurvey3)
  }, ignoreInit = TRUE)
  
  
  # Cache pour les données
  data_cache <- reactiveValues(loaded = list())
  
  observe({
    req(input$selectsurvey)
    
    # Vérifier le cache
    if (!is.null(data_cache$loaded[[input$selectsurvey]])) {
      data_reactive(data_cache$loaded[[input$selectsurvey]])
      current_survey(input$selectsurvey)
      return()
    }
    
    # Charger depuis Parquet
    if (input$selectsurvey == "Ensemble") {
      data <- load_all_surveys()
    } else {
      data <- load_survey(input$selectsurvey)
    }
    
    if (!is.null(data)) {
      data_cache$loaded[[input$selectsurvey]] <- data
      data_reactive(data)
      current_survey(input$selectsurvey)
      updateNumericInput(session, "volume_enquete", value = nrow(data))
    }
  })
  
  #### Supprimer les données ####
  
  observeEvent(input$DeleteData, {
    req(input$selectsurvey2)
    
    delete_survey(input$selectsurvey2)
    
    output$check_delete <- renderText({
      paste0("✅ L'enquête '", input$selectsurvey2, "' a été supprimée !")
    })
    
    # Invalider le cache
    data_cache$loaded[[input$selectsurvey2]] <- NULL
    data_cache$loaded[["Ensemble"]] <- NULL
    
    # Actualiser la liste
    enquetes <- list_surveys()
    updateSelectInput(session, "selectsurvey", choices = c(enquetes, "Ensemble"))
    updateSelectInput(session, "selectsurvey2", choices = enquetes)
    updateSelectInput(session, "selectsurvey3", choices = c(enquetes, "Ensemble"))
  })
  
  observeEvent(input$cleanCache, {
    clean_cache()
    output$check_clean <- renderText({
      "✅ Cache vidé !"
    })
  })
  
  #### Dataframes avec cache ####
  
  table_ecart_reactive <- reactiveVal(NULL)
  table_accuracy <- reactiveVal(NULL)
  
  observeEvent(data_reactive(), {
    req(data_reactive(), current_survey())
    
    metrics <- get_cached_metrics(
      data_reactive(),
      current_survey(),
      niveau = 4  # niveau fixe pour le taux d'accuracy global,
    )
    table_accuracy(metrics)
  })
  
  observe({
    req(data_reactive(), choix_niveau_debounced())
    
    table_ecart_reactive(table_ecart(
      df = data_reactive(),
      target = target,
      prediction = prediction,
      niveau = choix_niveau_debounced()
    ))
    
    output$table_ecart_niv <- renderDT({
      req(table_ecart_reactive())
      datatable(table_ecart_reactive(),
                colnames = c("Nomenclature", "Taux bonnes prédictions", "Nombre d'observations"),
                editable = FALSE) %>%
        formatPercentage('freq_predit', digits = 2)
    })
  })
  
  #### Choix seuil et niveau avec debounce ####
  
  choix_seuil <- reactiveVal(NULL)
  choix_niveau <- reactiveVal(NULL)
  
  # Debounce pour éviter trop de recalculs
  choix_seuil_debounced <- debounce(reactive(input$choix_seuil), 300)
  choix_niveau_debounced <- debounce(reactive(input$choix_niveau), 300)
  
  observeEvent(c(input$choix_niveau, input$selectsurvey), {
    waiter_show(
      id = "plot_evol_qualite_box",
      html = spin_fading_circles()
    )
  }, ignoreInit = TRUE)
  
  observeEvent(c(input$choix_niveau_2,input$choix_seuil_2), {
    waiter_show(
      id = "plot_metrics_box",
      html = spin_fading_circles()
    )
    
    waiter_show(
      id = "plot_acc_ss_score_box",
      html = spin_fading_circles()
    )
    
    waiter_show(
      id = "resume_box",
      html = spin_fading_circles()
    )
    
  }, ignoreInit = TRUE)
  
  choix_seuil_2_debounced <- debounce(reactive(input$choix_seuil_2), 300)
  choix_niveau_2_debounced <- debounce(reactive(input$choix_niveau_2), 300)
  
  # Synchronisation choix_niveau -> choix_niveau_2
  observeEvent(input$choix_niveau, {
    updateSelectInput(session, "choix_niveau_2", selected = input$choix_niveau)
  }, ignoreInit = TRUE)
  
  # Synchronisation choix_niveau_2 -> choix_niveau
  observeEvent(input$choix_niveau_2, {
    updateSelectInput(session, "choix_niveau", selected = input$choix_niveau_2)
  }, ignoreInit = TRUE)
  
  #### Informations ####
  
  output$nbre_obs <- renderText({
    req(data_reactive(), input$selectsurvey)
    paste0("Nombre observations : ", nrow(data_reactive()))
  })
  
  output$survey_choose <- renderText({
    req(input$selectsurvey)
    paste0("L'enquête sélectionnée est : ", input$selectsurvey)
  })
  
  output$metric <- renderUI({
    req(data_reactive(), metrics_monitoring_reactive())
    
    m <- metrics_monitoring_reactive()%>%
      filter(seuils == min(seuils))
    
    HTML(paste0(
      "<hr><strong>Accuracy : ", sprintf("%.2f%%", m$accuracy * 100),
      "<hr><strong>Précision : ", sprintf("%.2f%%", m$precision * 100),
      "<br>Recall : ", sprintf("%.2f%%", m$recall * 100),
      "<br><br>F1 Score : ", sprintf("%.2f%%", m$f1 * 100),
      " </strong>"
    ))
  })
  
  
  #### GRAPHIQUES (avec debounce) ####
  
  metrics_monitoring_reactive <- reactive({
    req(data_reactive(), choix_niveau_debounced())
    
    get_cached_metrics(
      data_reactive(),
      current_survey(),
      niveau = choix_niveau_debounced()
    )
  }) %>% debounce(300)
  
  observe({
    req(data_reactive(), metrics_monitoring_reactive())
    
    output$plot_evol_qualite <- renderPlotly({
      
      on.exit(waiter::waiter_hide(id = "plot_evol_qualite_box"), add = TRUE)
      
      plot_accuracy_volume(metrics_monitoring_reactive())
      
    })
  })
  
  observe({
    req(data_reactive(), choix_niveau_debounced())
    
    output$distribution_niv <- renderPlotly(
      plot_distribution_cs(
        data_reactive()$target,
        data_reactive()$prediction,
        data_reactive()$weight,
        nb_positions_max = choix_niveau_debounced()
      )
    )
  })
  
  
  observe({
    req(data_reactive(), choix_seuil_debounced(), choix_niveau_debounced())
    
    output$distribution_scores <- renderPlotly(
      plot_distribution_scores(
        data_reactive()$score,
        data_reactive()$weight,
        threshold = choix_seuil_debounced()
      )
    )
    
    output$distribution_scores_2 <- renderPlotly(
      plot_distribution_scores_knowing_correct_codes(
        data_reactive()$target,
        data_reactive()$prediction,
        data_reactive()$score,
        data_reactive()$weight,
        nb_positions_max = choix_niveau_debounced(),
        threshold = choix_seuil_debounced()
      )
    )
    
  })
  
  observe({
    req(data_reactive(), choix_seuil_debounced(), metrics_monitoring_reactive())
    
    output$plot_fixseuil_acc <- renderPlotly(
      plot_pour_fixation_seuil(
        metrics_monitoring_reactive(),
        metric_y = "accuracy_codages_auto",
        threshold = choix_seuil_debounced()
      )
    )
    
    output$plot_fixseuil_pro <- renderPlotly(
      plot_pour_fixation_seuil(
        metrics_monitoring_reactive(),
        metric_y = "proportion_codages_auto",
        threshold = choix_seuil_debounced()
      )
    )
    
  })
  
  #### Matrice de confusion ----
  
  observe({
    req(data_reactive())
    output$plot_conf_mat <- renderPlotly({
      
      p <-plot_conf_mat(df = data_reactive(),
                        niveau = 2)
      
      ggplotly(p) %>%
        layout(
          margin = list(l = 200, b = 200)
        )
    })
    
    output$conf_mat_extract <- render_gt({
      extract_conf_mat(df=data_reactive(),
                       niveau = 2)%>%
        head(10)
    })
    
  })
  
  
  ##### Aide au choix du seuil ----
  
  output$aide <- renderUI({
    HTML('
      <strong>Précision :</strong> 
      <br>
      La précision mesure la capacité du modèle à ne pas se tromper quand il prédit une classe.
      <br>
      Calcul : VP ÷ (VP + FP) = bien prédits de la classe / total prédits dans la classe
      <br>
      Interprétation : 
      <br>
      Plus la précision est élevée, plus le modèle minimise le nombre de Faux Positif.
      <hr>
      <strong>Recall :</strong> 
      <br>
      Le recall (ou sensibilité) mesure la capacité du modèle à retrouver toutes les observations d\'une classe.
      <br>
      Calcul : VP ÷ (VP + FN) = bien prédits de la classe / total réel de la classe
      <br>
      Interprétation : 
      <br>
      Plus le recall est élevé, plus le modèle maximise le nombre de Vrai Positifs.
      <br>
      En classification multiclasse macro-pondérée, le recall global est égal à l\'accuracy
      <hr>
      <strong>F1 Score :</strong> 
      <br>
      Moyenne harmonique de la précision et du recall
      <br>
      Calcul : 2 x (recall x precision) ÷ (recall + precision)
      <br>
      Interprétation : 
      <br>
      Plus le F1 Score est élevé, plus le modèle est performant.
      <hr>
      Les métriques calculées sont des métriques globales qui sont une moyenne
      des métriques par classe, pondérées par le nombre d\'observation de chaque classe
     ')
  })
  
  
  metrics_reactive <- reactive({
    req(data_reactive(), choix_niveau_2_debounced())
    
    get_cached_metrics(
      data_reactive(),
      current_survey(),
      niveau = choix_niveau_2_debounced(),
      tx_erreur_humaine = input$choix_erreur_humain / 100,
      volume_globale = input$volume_enquete,
      tx_recours_liste = input$choix_tx_recours_liste / 100
    )
  }) %>% debounce(300)
  
  seuil_max_reactive <- reactive({
    req(data_reactive(), choix_seuil_2_debounced())
    
    choix_seuil_2_debounced()
    
  }) %>% debounce(300)
  
  output$plot_metrics <- renderPlotly({
    
    on.exit(waiter::waiter_hide(id = "plot_metrics_box"), add = TRUE)
    
    plot_metrics(metrics_reactive(), choix_seuil_2_debounced())
  })
  
  output$plot_acc_ss_score <- renderPlotly({
    
    on.exit(waiter::waiter_hide(id = "plot_acc_ss_score_box"), add = TRUE)
    
    df_pr_graphique <- metrics_reactive()%>%
      filter(seuils<=seuil_max_reactive())
    
    plot_acc_ss_score(df_pr_graphique)
  })
  
  output$resume <- renderDT({
    
    on.exit(waiter::waiter_hide(id = "resume_box"), add = TRUE)
    
    metrics_reactive() %>%
      dplyr::filter(seuils<=seuil_max_reactive())%>%
      arrange(desc(seuils)) %>%
      mutate(
        accuracy = sprintf("%.1f%%", accuracy * 100),
        precision = sprintf("%.1f%%", precision * 100),
        recall = sprintf("%.1f%%", recall * 100),
        f1_score = sprintf("%.1f%%", f1 * 100),
        tx_reprise = sprintf("%.1f%%", tx_reprise * 100),
        obs_reprise = sprintf("%s", format(obs_reprise, big.mark=" ")),
        accuracy_glob = sprintf("%.1f%%", accuracy_glob * 100),
        tx_reprise_glob = sprintf("%.1f%%", tx_reprise_glob * 100),
        obs_reprise_glob = sprintf("%s", format(obs_reprise_glob, big.mark=" "))
      )%>%
      select(seuils, accuracy_glob,
             tx_reprise_glob, obs_reprise_glob)
  })
  
  session$onSessionEnded(function() {
    stopApp()
  })
}