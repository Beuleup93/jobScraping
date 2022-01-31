library(shiny)
library(shinydashboard)
library(shinyBS)
library(leaflet)
library(DT)
library(shinyjs)

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
        menuItem("Analyse temporelle", icon = icon("th"), tabName = "analyse"),
        menuItem("Carte", icon = icon("globe"), tabName = "map", badgeLabel = "geo", badgeColor = "green"),
        #menuItem("Dimension", icon = icon("th"),
                 #menuSubItem("Nature Contrat", tabName = "entreprise")),
        menuItem("Données", icon = icon("th"), tabName = "dimension"),
        menuItem("Analyse corpus", icon = icon("th"), tabName = "statistique"),
        numericInput(inputId = "select_topn",
                     label = "Nombre d'occurence",
                     value = 10, min = 3, max = 30, step=1),
        selectInput("secteur","Domaine d'activité",choices = unique(getPost()$libelle_secteur),selected = 'Assurance'),
        #sliderInput("max","Maximum Number of Words:", min = 1,  max = 300,  value = 100),
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
                    infoBoxOutput(width = 3, "entreprises"),
                    infoBoxOutput(width = 3, "emplois"),
                    infoBoxOutput(width = 3, "percentNew"),
                    infoBox(width = 3, "Shiny version", "0.12",icon = icon("desktop"))
                ),

                fluidRow(
                    box(width=6, plotOutput("plot1", height = 250)),
                    box(width = 6, plotOutput("plot3", height = 250)),
                    box(width = 6, plotOutput("plot2", height = 250)),


                )
        ),

        tabItem(tabName = "map",
                box(width = 12, leafletOutput("mymap", height = 650)),
                #box(width = 6, leafletOutput("leaflet")),
                #box(width = 6, leafletOutput("leaflet"))
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
        tabItem(tabName = "statistique",
                box(width=12, title="Occurences de mots",
                    fluidRow(
                        column(6,plotly::plotlyOutput("plot_s1")),
                        column(6, dataTableOutput("table1"))
                        #column(6, plotOutput("plot_s2"))
                    )
                ),
                #box(width=6, plotlyOutput("plot_s4", height = 300))

        )
    ),
    useShinyjs()
)

dashboardPage(header, sidebar, body, skin = "green")
