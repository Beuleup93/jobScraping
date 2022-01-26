library(shiny)
library(shinydashboard)
library(shinyBS)
library(leaflet)
library(DT)
library(shinyjs)

header <- dashboardHeader(title = "JobAnalysis",
                          dropdownMenuOutput("notifications"))

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Tableau de bord", tabName = "dashboard",icon = icon("dashboard")),
        radioButtons(inputId = "filtreID",
                     label = "Filtre",
                     choices = list("Secteur" = "code_secteur",
                                    "NatureContrat" = "code_nature_contrat",
                                    "TypeContrat" = "code_type_contrat")),

        menuItem("Carte", icon = icon("globe"), tabName = "map", badgeLabel = "geo", badgeColor = "green"),
        #menuItem("Dimension", icon = icon("th"),
                 #menuSubItem("Nature Contrat", tabName = "entreprise")),
        menuItem("Dimensions", icon = icon("th"), tabName = "dimension"),
        menuItem("Statitique Global", icon = icon("th"), tabName = "statistique"),
        actionButton("showData", "Show client data")
    )
)

body <- dashboardBody(
    tabItems(
        bsModal(id = "corpusData", title = "Corpus Data",
                trigger = "showData",
                verbatimTextOutput("corpusdataText")),

        tabItem(tabName = "dashboard",
                fluidRow(
                    infoBoxOutput(width = 3, "entreprises"),
                    infoBoxOutput(width = 3, "emplois"),
                    infoBoxOutput(width = 3, "percentNew"),
                    infoBox(width = 3, "Shiny version", "0.12",icon = icon("desktop"))
                ),

                fluidRow(
                    box(plotOutput("plot1", height = 250)),
                    box(width = 2, p(id = "selection", "Values")),
                    #box(width = 5, plotOutput("histogram"))
                )
        ),

        tabItem(tabName = "map",
                #box(width = 6, plotOutput("ggplotMap")),
                #box(width = 6, leafletOutput("leaflet"))
                ),

        tabItem(tabName = "dimension",
                mainPanel(
                    tabsetPanel(id = "theTabs",
                                tabPanel("secteur", dataTableOutput("secteurTable"), value="TabSecteur",),
                                tabPanel("Nature contrat", dataTableOutput("NatureContratTable"), value="Tab"),
                                tabPanel("Type contrat", dataTableOutput("typeContratTable"),  value = "type contrat"),
                                tabPanel("Commune", dataTableOutput("communeTable"),  value = "commune"),
                                tabPanel("Departement", dataTableOutput("departementTable"),  value = "departement"),
                                tabPanel("Region", dataTableOutput("regionTable"),  value = "region")
                    )
                )

                #box(width = 6, leafletOutput("leaflet"))
        )
    ),
    useShinyjs()
)

dashboardPage(header, sidebar, body, skin = "green")
