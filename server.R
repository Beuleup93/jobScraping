
library(shiny)
library(dplyr)
library(ggplot2)
library(rgdal)
library(RColorBrewer)
library(googleVis)
library(leaflet)
suppressPackageStartupMessages(library(googleVis))

shinyServer(function(input, output, session) {

    # icons
    output$entreprises <- renderInfoBox({
        infoBox("entrprises", 100 - 50,
                icon = icon("calendar", lib = "font-awesome"),
                color = "blue",
                fill = TRUE)
    })

    output$emplois <- renderInfoBox({
        infoBox("emplois", 1245,
                icon = icon("user"),
                color = "purple",
                fill = TRUE)
    })

    output$percentNew <- renderInfoBox({
        infoBox("New job",24,
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

    #set.seed(122)
    histdata <- rnorm(500)
    output$plot1 <- renderPlot({
        data <- histdata[seq_len(input$slider)]
        hist(data)
    })

})
