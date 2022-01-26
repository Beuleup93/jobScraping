library(shiny)
library(shinyjs)
library(dplyr)
library(ggplot2)
library(rgdal)
library(RColorBrewer)
library(googleVis)
library(leaflet)

suppressPackageStartupMessages(library(googleVis))
source("helpersApiBd.R")

shinyServer(function(input, output, session) {
    # icons
    output$entreprises <- renderInfoBox({
        infoBox("entreprises", getNumberOfRows("entreprise")[1,],
                icon = icon("calendar", lib = "font-awesome"),
                color = "blue",
                fill = TRUE)
    })

    output$emplois <- renderInfoBox({
        infoBox("emplois", getNumberOfRows("POST")[1,],
                icon = icon("user"),
                color = "purple",
                fill = TRUE)
    })

    output$percentNew <- renderInfoBox({
        infoBox("Secteur d'activité",getDistinctSecteur("POST","code_secteur")[1,],
            icon = icon("pie-chart"),
            color = "yellow",
            fill = TRUE)
    })

    output$notifications <- renderMenu({
        users <- 1245
        newusers <- 24
        notifData <- data.frame("number" = c(users, newusers),
                                "text" = c(" users", "% new users"),
                                "icon"= c("users", "user"))

        notifs <- apply(notifData, 1, function(row) {
            notificationItem(text = paste0(row[["number"]], row[["text"]]),
                             icon = icon(row[["icon"]]))
        })

        dropdownMenu(type = "notifications", .list = notifs)
    })

    output$plot1 <- renderPlot({
        if(input$filtreID == "code_secteur"){
            getPostBySecteur() %>% ggplot(aes(x = code_secteur)) +
                geom_bar(fill="#226D68") +
                ggtitle("Repartition des emplois selon le secteur d'activité")

        }else if(input$filtreID=="code_nature_contrat"){
            getPostBySecteur() %>% ggplot(aes(x = code_nature_contrat)) +
                geom_bar(fill="#5D7052") +
                ggtitle("Repartition des emplois selon la nature des contrat")

        }else{
            getPostBySecteur() %>% ggplot(aes(x = code_type_contrat)) +
                geom_bar(fill="#26474E") +
                ggtitle("Repartition des emplois selon le type de contrat")
        }

    })

    # DataTable
    output$secteurTable = renderDataTable({
        # Cas ou on personnalise le Datable de JQuery
        #getPostBySecteur() %>%
            #select(c(code_secteur, libelle_secteur)) %>%
            #distinct(libelle_secteur, .keep_all= TRUE)


        datatable(
            getPostBySecteur() %>%
                select(c(code_secteur, libelle_secteur)) %>%
                distinct(libelle_secteur, .keep_all= TRUE),
            options =
                list(pagingType = "simple"))
            #colnames = c("Code", "Libelle"),
            #caption = "Secteur details", filter = "top",
            #options = list(
                #pageLength = 10,
                #lengthMenu = c(10, 20))
        #)
  })
})
