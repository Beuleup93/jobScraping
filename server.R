library(shiny)
library(shinyjs)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(rgdal)
library(RColorBrewer)
library(googleVis)
library(leaflet)
library(plotly)
library(ggthemes)
library(wordcloud)
library(wordcloud2)


suppressPackageStartupMessages(library(googleVis))
source("helpersApiBd.R")

shinyServer(function(input, output, session) {

  getReactiveData <- reactive({
    data <- getPost(input$secteur)
    return(data)
  })

  getReactiveData2 <- reactive({
    data <- getPost()
    return(data)
  })

  getCorpusData <- reactive({
    data <- processingCorpus(getReactiveData())
    return(data)
  })

  getReactiveWordCount<- reactive({
    tmp = processingCorpus(getReactiveData())
    dd = tmp$dict_terme
    return(dd)
  })

  getDataLeaflet<- reactive({
    data <- df_leaflet(getReactiveData())
    return(data)
  })

  v <- reactive({
    shiny::validate(
      need(input$select_topn >= 2 & input$select_topn <= 50, "S'il vous plait veuiller choisir un nombre entre 3 et 50")
    )
  })

  getACData <- reactive({
    dataAC = getDataforAC(getReactiveData2())
    return(dataAC)
  })

    # icons
    output$entreprises <- renderInfoBox({
        infoBox("entreprises", getNumberOfRows("entreprise",input$secteur,"ref_entreprise")[1,],
                icon = icon("calendar", lib = "font-awesome"),
                color = "blue",
                fill = TRUE)
    })

    output$emplois <- renderInfoBox({
        infoBox("emplois", getNumberOfRows("POST", input$secteur, "ID")[1,],
                icon = icon("user"),
                color = "purple",
                fill = TRUE)
    })

    output$percentNew <- renderInfoBox({
        infoBox("Domaines",getDistinctSecteur("POST","code_secteur")[1,],
            icon = icon("pie-chart"),
            color = "yellow",
            fill = TRUE)
    })



    output$mymap <- renderLeaflet({

      getDataLeaflet() %>% leaflet() %>%
        setView(lng = 1.888334, lat = 46.60335, zoom = 5)  %>% #setting the view over ~ center of North America
        addTiles() %>%
        addMarkers(lng = ~lon,
                   lat = ~lat,
                   popup = ~paste(nom_region," ",count))
    })

    output$plot1 <- renderPlot({
        if(input$filtreID == "code_secteur"){
          getReactiveData2() %>% group_by(code_secteur)%>%
            summarise(count = n()) %>%
            ggplot() +
            geom_col(aes(x = code_secteur, y= count), fill = "#226D68", width=.6) +
            ggtitle("Repartition des emplois selon le secteur d'activité")

        }else if(input$filtreID=="code_nature_contrat"){
          getReactiveData2() %>% group_by(libelle_nature)%>%
            summarise(count = n()) %>%
            ggplot() +
            geom_col(aes(y = libelle_nature, x= count), fill = "#226D68", width=.6) +
            ggtitle("Repartition des emplois selon la nature des contrat")

        }else{
          getReactiveData2() %>% ggplot(aes(x = code_type_contrat)) +
                geom_bar(fill="#26474E") +
                ggtitle("Repartition des emplois selon le type de contrat")
        }

    })

    output$plot2 <- renderPlot({
        getReactiveData() %>% group_by(nom_region) %>%
          summarise(count = n()) %>%
          ggplot() +
          geom_col(aes(x = count, y= nom_region), fill = "#23798E", width=.6) +
          ggtitle("Repartition des emplois selon les régions")
    })


    output$plot3 <- renderPlot({
        getReactiveData() %>%
        group_by(libelle_qualification)%>%
          summarise(count = n()) %>%
          filter(libelle_qualification != 'NULL') %>%
          ggplot() +
          geom_col(aes(x = count, y=libelle_qualification), fill = "#375D81", width=.6) +
          ggtitle("Repartition des emplois selon la qualification")
    })

    output$plot3 <- renderPlot({
      getReactiveData() %>%
        group_by(libelle_qualification)%>%
        summarise(count = n()) %>%
        filter(libelle_qualification != 'NULL') %>%
        ggplot() +
        geom_col(aes(x = count, y=libelle_qualification), fill = "#375D81", width=.6) +
        ggtitle("Repartition des emplois selon la qualification")
    })

    output$plot4 <- renderPlot({
      #df <-getReactiveData()
      #df = as.data.frame(getHoursByContrat(df))
      #df$dureeTravailLibelle = paste(df$dureeTravailLibelle,"H", sep='')
      #df %>%  ggplot()+
        #geom_col(aes(x = count, y=dureeTravailLibelle), fill = "#375D81", width=.6) +
        #ggtitle("Répartition du temps de travail en heures")
      Graph_Experience_Qualification(getReactiveData(), input$secteur)
    })


    output$plot_s1 <- renderPlotly({
      # Get data
      v()
      dd = getReactiveWordCount()
      dd$color = sprintf("%s","color")
      # Create visuaization
      p <- ggplot(data=head(n=input$select_topn,dd), aes(x=word, y=n,fill=color)) +
        geom_col()+
        ylab("count") +
        coord_flip() +
        theme_minimal() +
        ggtitle(paste("Top mots", input$secteur)) +
        geom_blank()+
        scale_fill_manual(values=c("#23798E"))
      ggplotly(p)
    })

    output$table1 <- renderDataTable({
      # Validate for table
      v()
      head(n=input$select_topn,getReactiveWordCount())
    })

    output$plot_s2 <- renderPlot({
      tmp = getCorpusData()
      dico_terme = tmp$dict_terme
      wordcloud(words=dico_terme$word,freq=dico_terme$n, min.freq=10,max.word=input$max,colors = brewer.pal(8,'Dark2'))
    })

    # DataTable
  output$emploiTable = renderDataTable({
      datatable(
        getReactiveData() %>%
          select(c(nom, intitule,libelle_secteur, libelle_type,libelle_qualification,nom_commune,nom_dept)),
        options =list(pagingType = "simple",pageLength = 5))
    })

  output$entrepriseTable = renderDataTable({
    datatable(
      getReactiveData() %>%
        select(c(intitule,nom, url,logo)),
      options =list(pagingType = "simple",pageLength = 8))
  })

  output$secteurTable = renderDataTable({
        datatable(
          getReactiveData() %>%
                select(c(code_secteur, libelle_secteur)) %>%
                distinct(libelle_secteur, .keep_all= TRUE),
                options =list(pagingType = "simple"))
  })

  output$NatureContratTable = renderDataTable({
      datatable(
        getReactiveData() %>%
          select(c(code_nature_contrat, libelle_nature)) %>%
          distinct(libelle_nature, .keep_all= TRUE),
        options =list(pagingType = "simple"))
    })

  output$typeContratTable = renderDataTable({
    datatable(
      getReactiveData() %>%
        select(c(code_type_contrat, libelle_type)) %>%
        distinct(libelle_type, .keep_all= TRUE),
      options =list(pagingType = "simple"))
  })

  output$communeTable = renderDataTable({
    datatable(
      getReactiveData() %>%
        select(c(code_commune, nom_commune)) %>%
        distinct(nom_commune, .keep_all= TRUE),
      options =list(pagingType = "simple"))
  })

  output$departementTable = renderDataTable({
    datatable(
      getReactiveData() %>%
        select(c(code_dept, nom_dept)) %>%
        distinct(nom_dept, .keep_all= TRUE),
      options =list(pagingType = "simple"))
  })

  output$regionTable = renderDataTable({
    datatable(
      getReactiveData() %>%
        select(c(code_region, nom_region)) %>%
        distinct(nom_region, .keep_all= TRUE),
      options =list(pagingType = "simple"))
  })

  output$plot_ac <- renderPlotly({
    domaine_ca <- CA(getACData() %>% as.data.frame() %>%
                       column_to_rownames("Group.1"), ncp = 1000, graph = FALSE)
    domaine_ca_coord = domaine_ca$row$coord
    domaine_ca_df = tibble(doc_id = row.names(domaine_ca_coord),
                           Dim1 = domaine_ca_coord[,1], Dim2=domaine_ca_coord[,2],
                           domaine = str_sub(doc_id,1L, 2L))
    p <- ggplot(data=domaine_ca_df, aes(x=Dim1, y=Dim2,color=domaine)) +
      geom_path()+
      geom_point()+
      ylab("composante 2")+
      xlab("composante 1")+
      coord_equal(xlim=c(-1,1), ylim=c(-1,1))+
      ggtitle(paste("Analyse des correspondance et visualisation")) +
      scale_fill_manual(values=c("#23798E"))
    ggplotly(p)
  })

})
