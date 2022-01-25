library(shiny)
library(shinydashboard)
library(shinyBS)
library(leaflet)

header <- dashboardHeader(title = "JobAnalysis",
                          dropdownMenuOutput("notifications"))

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Tableau de bord", tabName = "dashboard",icon = icon("dashboard")),
        menuItem("Carte", icon = icon("globe"), tabName = "map", badgeLabel = "geo", badgeColor = "green"),
        menuItem("Dimension", icon = icon("th"),
                 menuSubItem("Nature Contrat", tabName = "entreprise"),
                 menuSubItem("Nature Contrat", tabName = "natureContrat"),
                 menuSubItem("Type Contrat", tabName = "typeContrat"),
                 menuSubItem("Qualification", tabName = "qualification")
                 ),
        menuItem("Statitique Global", icon = icon("th"), tabName = "statistique")
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "dashboard",
                fluidRow(
                    infoBoxOutput(width = 3, "entreprises"),
                    infoBoxOutput(width = 3, "emplois"),
                    infoBoxOutput(width = 3, "percentNew"),
                    infoBox(width = 3, "Shiny version", "0.12",icon = icon("desktop"))
                ),

                fluidRow(
                    box(plotOutput("plot1", height = 250)),
                    box(title = "Controls", sliderInput("slider", "Number of observations:", 1, 100, 50))

                )
        ),

        tabItem(tabName = "map",
                )
    )
)

dashboardPage(header, sidebar, body, skin = "green")
