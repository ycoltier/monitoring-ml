ui <- fluidPage(
  useShinyjs(),
  use_waiter(),
  
  titlePanel(h1("Outil de monitoring", align = "center")),
  
  tabsetPanel(
    #### Onglet Administration ####
    tabPanel("Administration", sidebarLayout(
      sidebarPanel(
        fileInput("file1", "Importer un fichier csv ou parquet",
                  accept = c(".csv", ".parquet")),
        
        fluidRow(
          actionButton("loadData", "Voir les données"),
          actionButton("treatData", "Traiter les données"),
          actionButton("loadDataLocal", "Sauvegarder localement"),
          textInput("sigle_enquete", "Sigle de l'enquête", value = "CDV")
        ),
        
        verbatimTextOutput("check_result"),
        verbatimTextOutput("check_save"),
        
        tags$hr(),
        
        h3("Paramètres CSV"),
        checkboxInput("header", "Header", TRUE),
        radioButtons("sep", "Separator",
                     choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                     selected = ","),
        radioButtons("quote", "Quote",
                     choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"),
                     selected = '"'),
        
        tags$hr(),
        
        h3("Suppression d'une enquête"),
        selectInput('selectsurvey2', 'Sélectionner l\'enquête', choices = enquetes),
        actionButton("DeleteData", "Supprimer !"),
        verbatimTextOutput("check_delete"),
        
        tags$hr(),
        h3("Nettoyage du cache"),
        actionButton("cleanCache", "Vider le cache"),
        verbatimTextOutput("check_clean"),
        
        width = 3
      ),
      
      mainPanel(
        h3("Données brutes"),
        DTOutput("table"),
        tags$hr(),
        fluidRow(
          column(3, selectInput('rename_prediction', 'variable "prediction" : ', choices = names_column)),
          column(3, selectInput('rename_target', 'variable "target" : ', choices = names_column)),
          column(3, selectInput('rename_score', 'variable "score" : ', choices = names_column)),
          column(3, selectInput('rename_weight', 'variable "weight" : ', choices = names_column))
        ),
        h3("Données traitées"),
        DTOutput("table_vf")
      )
    )),
    
    #### Onglet Performances ####
    tabPanel("Monitoring", sidebarLayout(
      sidebarPanel(
        selectInput('selectsurvey', 'Sélectionner l\'enquête',
                    choices = c(enquetes, "Ensemble")),
        tags$hr(),
        sliderInput("choix_seuil", "Sélectionnez un niveau de seuil de confiance :",
                    min = 0, max = 1, value = 0.5, step = 0.01),
        sliderInput("choix_niveau", "Sélectionnez un niveau de nomenclature :",
                    min = 1, max = 4, value = 1, step = 1),
        tags$hr(),
        textOutput('nbre_obs'),
        tags$hr(),
        htmlOutput('metric'),
        h3('Aide'),
        htmlOutput('aide'),
        width = 3
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Performances",
                   fluidRow(div(
                     id = "plot_evol_qualite_box",
                     h2('Qualité'),
                     plotlyOutput('plot_evol_qualite')
                   )
                   ),
                   fluidRow(
                     h2("Distribution"),
                     column(6, 
                            h3('Histogramme'), 
                            plotlyOutput('distribution_niv')),
                     column(6,
                            h3('Tableau des écarts de prédictions'),
                            DTOutput('table_ecart_niv'))
                   )
          ),
          tabPanel("Seuil de confiance",
                   fluidRow(h2("Distribution lissée")),
                   fluidRow(
                     column(6,
                            h3('Distribution score'),
                            plotlyOutput('distribution_scores')),
                     column(6,
                            h3('Distribution score selon valeur seuil'),
                            plotlyOutput('distribution_scores_2'))
                   ),
                   
                   fluidRow(
                     column(6,
                            h3('Taux d\'accuracy en fonction du seuil choisi'),
                            plotlyOutput('plot_fixseuil_acc')),
                     column(6,
                            h3('Proportion de codage automatique en fonction du seuil choisi'),
                            plotlyOutput('plot_fixseuil_pro'))
                   )
          ),
          tabPanel("Matrice de confusion au niveau 2",
                   fluidRow(plotlyOutput('plot_conf_mat', , height = "900px")),
                   fluidRow(
                     h3('Top 10 des confusions remarquables au niveau 2'),
                     gt_output('conf_mat_extract'))
                   
          )
        )
      )
    )),
    #### Onglet Tableau récapitulatif ####
    tabPanel("Aide choix seuil", sidebarLayout(
      sidebarPanel(
        selectInput('selectsurvey3', 'Sélectionner l\'enquête',
                    choices = c(enquetes, "Ensemble")),
        tags$hr(),
        sliderInput("choix_niveau_2", "Sélectionnez un niveau de nomenclature :",
                    min = 1, max = 4, value = 1, step = 1),
        tags$hr(),
        sliderInput("choix_seuil_2", "Sélectionnez un niveau de seuil de confiance :",
                    min = 0, max = 1, value = 0.20, step = 0.05),
        tags$hr(),
        tags$hr(),
        numericInput("volume_enquete", "Volume de l'enquête (nombre total à coder)", 
                     value = 0),
        tags$hr(),
        sliderInput("choix_tx_recours_liste", "Sélectionnez un taux de recours à la liste :",
                    min = 0, max = 100, value = 0, step = 10, post = "%"),
        tags$hr(),
        sliderInput("choix_erreur_humain", "Sélectionnez un taux d'erreur humaine :",
                    min = 0, max = 100, value = 0, step = 5, post = "%"),
        tags$hr(),
        width = 2
      ),
      
      mainPanel(
        fluidRow(
          div(id = "plot_metrics_box", plotlyOutput("plot_metrics"))
        ),
        fluidRow(
          column(6, 
                 h3("Tableau de référence"), 
                 div(id = "resume_box", DTOutput("resume"))
          ),
          column(1),
          column(5,
                 div(id = "plot_acc_ss_score_box", plotlyOutput("plot_acc_ss_score"))
          )
        )
        
      )
    )
    )
  )
)