library(shiny)
library(shinydashboard)
library(shinyBS)
library(leaflet)
library(DT)
library(shinyjs)
library(tidyr)

source("helpersApiBd.R")
header <- dashboardHeader(title = "JobAnalysis",
                          dropdownMenuOutput("notifications"))

sidebar <- dashboardSidebar(

    sidebarMenu(
        menuItem("Tableau de bord", tabName = "dashboard",icon = icon("dashboard")),
        radioButtons(inputId = "filtreID",
                     label = "Filtre",
                     choices = list("Domaine" = "code_secteur",
                                    "Nature Contrat" = "code_nature_contrat",
                                    "Type Contrat" = "code_type_contrat")),

        radioButtons(inputId = "filtreAnne",
                     label = "Annee",
                     choices = list("All" = "All",
                                    "2021" = 2021,
                                    "2022" = 2022)),

        menuItem("Carte", icon = icon("globe"), tabName = "map", badgeLabel = "geo", badgeColor = "green"),

        menuItem("Données", icon = icon("th"), tabName = "dimension"),

        menuItem("Analyse corpus", icon = icon("th"),
                 menuSubItem("Frequence de mots", tabName = "statistique"),
                 menuSubItem("Association de mots", tabName = "association"),
                 menuSubItem("Analyse correspondance", tabName = "AC"),
                 menuSubItem("LDA", tabName = "lda"),
                 menuSubItem("Apprentisage supervisé", tabName = "app")
                 ),

        numericInput(inputId = "select_topn",
                     label = "Nombre d'occurence",
                     value = 10, min = 3, max = 30, step=1),

        selectInput("secteur","Domaine d'activité",choices = unique(c("Tous les domaines",getPost()$libelle_secteur)),selected = 'Tous les domaines'),

        menuItem("Charger Données", icon = icon("th"), tabName = "data"),

        actionButton("showData", "Show client data")
    )
)

body <- dashboardBody(
    bsModal(id = "corpusData", title = "Corpus Data",
            trigger = "showData",
            fluidRow(
                column(6,numericInput(inputId = "Debut", label = "Debut Plage", value = 0, min = 0, max = 1000, step=1)),
                column(6,numericInput(inputId = "Fin", label = "Fin de plage", value = 149, min = 50, max = 1000, step=1))
            ),
            fluidRow(
                column(12,selectInput("domaine","Domaine d'activité",choices = unique(getDataFromTable('secteursActivites')$libelle),selected = 'Assurance'))
            )
            #verbatimTextOutput("corpusdataText")
    ),

    tabItems(
        tabItem(tabName = "dashboard",
                fluidRow(
                    infoBoxOutput(width = 4, "entreprises"),
                    infoBoxOutput(width = 4, "emplois"),
                    infoBoxOutput(width = 4, "percentNew"),
                    #infoBox(width = 3, "Shiny version", "0.12",icon = icon("desktop"))
                ),

                fluidRow(
                    box(width=6, plotOutput("plot1", height = 250)),
                    box(width = 6, plotOutput("plot3", height = 250)),
                    box(width = 6, plotOutput("plot2", height = 250)),
                    box(width = 6, plotOutput("plot4", height = 250)),
                )
        ),

        tabItem(tabName = "map",
                box(width = 12, leafletOutput("mymap", height = 650))
        ),

        tabItem(tabName = "dimension",
                box(width=12,
                    fluidRow(
                        column(12,
                               mainPanel( width=12,
                                          tabsetPanel(id = "theTabs",
                                                      tabPanel("Emploi", dataTableOutput("emploiTable"), value="TabSecteur"),
                                                      tabPanel("Entreprise", dataTableOutput("entrepriseTable")),
                                                      tabPanel("Secteur", dataTableOutput("secteurTable"), value="TabSecteur"),
                                                      tabPanel("Nature contrat", dataTableOutput("NatureContratTable"), value="Tab"),
                                                      tabPanel("Type contrat", dataTableOutput("typeContratTable"),  value = "type contrat"),
                                                      tabPanel("Commune", dataTableOutput("communeTable"), value = "commune"),
                                                      tabPanel("Departement", dataTableOutput("departementTable"),  value = "departement"),
                                                      tabPanel("Region", dataTableOutput("regionTable"),  value = "region")
                                          )
                               )
                        )
                    ),
                ),
        ),
        tabItem(tabName="AC",
                box(width=8, plotOutput("plot_s2", height = 250)),
                box(width=4, sliderInput("max","Maximum Number of Words:", min = 1,  max = 300,  value = 100)),
                box(width=8, plotly::plotlyOutput("plot_ac", height = 330)),
                box(width=4,
                    selectInput(
                        inputId = "domaine",
                        label = "Liste des domaines d'activités:",
                        choices = unique(getPost()$code_libellesecteur),
                        selected = 99,
                        size = 13,
                        selectize = FALSE)
                ),
        ),

        tabItem(tabName = "statistique",
                box(width=12, title="Occurences de mots",
                    fluidRow(
                        column(6, plotly::plotlyOutput("plot_s1")),
                        column(6, dataTableOutput("table1"))
                    )
                ),
        ),
        tabItem(tabName="association",
                box(width=12,
                    fluidRow(
                        column(4,selectInput("field","Champ de corpus",choices = c("description","compétence"),selected = 'missions')),
                        column(4,selectInput("mots","choisir mot",choices = c("gestion","techniques","formation","expérience","poste","missions","outils","qualité","formation","équipe","service","informatique","assurance","charge","projet","développement","travail","suivi","client"),selected = 'missions')),
                        column(4, sliderInput(inputId = "plage",label = "Plage de mots", value = 20, min = 10, max = 200, step=1)),
                    )
                ),
                box(width = 12,
                    fluidRow(
                        column(12, dataTableOutput("table_assoc")),
                    )
                )
        ),
        tabItem(tabName="lda",
                box(width=12,
                    fluidRow(
                        column(4,selectInput("kchoice","Champ de corpus",choices = c("fixed k","find best k"),selected = 'find best k')),
                        column(4, sliderInput(inputId = "kfixed",label = "Nombre de topics", value = 2, min = 2, max = 8, step=1)),
                        column(4, sliderInput(inputId = "iter",label = "Nombre d'iterations", value = 100, min = 100, max = 1000, step=50)),
                    )
                ),
                box(width = 12,
                    fluidRow(
                        column(12, plotOutput("plot_lda", height=400)),
                    )
                )
        ),

        tabItem(tabName="app",
                box(width=12,
                    fluidRow(
                        column(4, selectInput("modalite1","One versus All",choices = unique(c("Choice one versus all",getPost()$libelle_secteur)),selected = 'Choice one versus all')),
                        column(4,selectInput("algo","Algorithme",choices = unique(c('decision tree', 'svm','Choice algorithm')),selected = 'Choice algorithm')),
                        column(4, sliderInput(inputId = "iter",label = "Nombre d'iterations", value = 100, min = 100, max = 1000, step=50)),
                    )
                ),
                box(width = 12,
                    fluidRow(
                        column(12, plotOutput("plot", height=400)),
                    )
                )
        )


    ),
    useShinyjs()
)

dashboardPage(header, sidebar, body, skin = "green")
############## fin UI##########
