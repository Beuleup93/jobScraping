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


suppressPackageStartupMessages(library(googleVis))
source("helpersApiBd.R")

shinyServer(function(input, output, session) {

  getReactivedfToCorpus <- reactive({
    data <- dfToCorpusField(getPost(), input$field, input$plage)
    return(data)
  })

  getReactiveData <- reactive({
    data <- getPost(input$secteur)
    if(input$filtreAnne != 'All'){
      data = decompose_date(data)
      return(data[data$annee==input$filtreAnne,])
    }
    return(data)
  })

  supervisedLearningReactive <- reactive({
    mod = supervised_learning(getReactiveData(),positive_mod = input$modalite_pos, algo=input$algo, taille=input$taille)
    return(mod)
  })

  getReactiveData2 <- reactive({
    data <- getPost()
    if(input$filtreAnne != 'All'){
      data = decompose_date(data)
     return(data[data$annee==input$filtreAnne,])
    }
    return(data)
  })

  getCorpusData <- reactive({
    data <- processingCorpus(getReactiveData())
    if(input$filtreAnne != 'All'){
      data = decompose_date(data)
      return(data[data$annee==input$filtreAnne,])
    }
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
      need(input$select_topn >= 2 & input$select_topn <= 50, "S'il vous plait veuillez choisir un nombre entre 3 et 50")
    )
  })

  error_msg <- reactive({
    shiny::validate(
      need(input$modalite_pos != 'Choisir modalité positive' & input$algo != 'Choisir algorithme', "S'il vous plait veuiller correctement parametrer l'algorithme")
    )
  })



  getACData <- reactive({
    dataAC = getDataforAC(getReactiveData2())
    return(dataAC)
  })
    # red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
    # icons
    output$entreprises <- renderInfoBox({
        infoBox("entreprises", getNumberOfRows(domaine=input$secteur,colonne_name = 'ref_entreprise',annee=input$filtreAnne),
                icon = icon("calendar", lib = "font-awesome"),
                color = "teal",
                fill = TRUE)
    })

    output$emplois <- renderInfoBox({
        infoBox("emplois", getNumberOfRows(domaine=input$secteur,colonne_name ='ID',annee=input$filtreAnne),
                icon = icon("user"),
                color = "olive",
                fill = TRUE)
    })

    output$percentNew <- renderInfoBox({
        infoBox("Domaines",getNumberOfRows(domaine=input$secteur,colonne_name = 'code_secteur',annee=input$filtreAnne),
            icon = icon("pie-chart"),
            color = "navy",
            fill = TRUE)
    })

    output$mymap <- renderLeaflet({

      getDataLeaflet() %>% leaflet() %>%
        setView(lng = 1.888334, lat = 46.60335, zoom = 5)  %>% #setting the view over ~ France
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
            geom_col(aes(x = reorder(code_secteur,-count) , y= count), fill = "#03224C", width=.6) + #226D68
            xlab("Code du secteur") +
            ylab("Nombre d'offres") +
            ggtitle("Répartition des emplois selon le secteur d'activité")

        }else if(input$filtreID=="code_nature_contrat"){
          getReactiveData2() %>% group_by(libelle_nature)%>%
            summarise(count = n()) %>%
            ggplot() +
            geom_col(aes(y = reorder(libelle_nature,-count), x= count), fill = "#226D68", width=.6) + #117A65
            xlab("Nombre d'offres") +
            ylab("Nature du contrat") +
            ggtitle("Répartition des emplois selon la nature des contrats")
        }else{
          getReactiveData2() %>% ggplot(aes(x = code_type_contrat)) +  #117A65
                geom_bar(fill="#226D68") +
                xlab("Type de contrat") +
                ylab("Nombre d'offres") +
                ggtitle("Répartition des emplois selon le type de contrat")
        }
    })

    output$plot2 <- renderPlot({
        getReactiveData() %>% group_by(nom_region) %>%
          summarise(count = n()) %>%
          ggplot() +
          geom_col(aes(x = count, y= reorder(nom_region,-count)), fill = "#226D68", width=.6) + #117A65
          xlab("Nombre d'offres") +
          ylab("Régions") +
          ggtitle("Répartition des emplois selon les régions")
    })

    output$plot3 <- renderPlot({
        getReactiveData() %>%
        group_by(libelle_qualification)%>%
          summarise(count = n()) %>%
          filter(libelle_qualification != 'NULL') %>%
          ggplot() +

          geom_col(aes(x = count, y= reorder(libelle_qualification,-count)), fill = "#226D68", width=.6) + ##117A65
          xlab("Nombre d'offres") +
          ylab("Qualification") +
          ggtitle("Répartition des emplois selon la qualification")
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
        ggtitle(paste("Top des mots","de", input$secteur)) +
        ylab("Nombre d'occurences") +
        xlab("Mots") +
        geom_blank()+
        scale_fill_manual(values=c("#117A65")) ##226D68
      ggplotly(p)
    })

    output$table_assoc <- renderDataTable({
      # Validate for table
      tmp = getReactivedfToCorpus()
      d = as.data.frame(findAssocs(tmp$dtm, terms = input$mots, corlimit = 0.3))
      d
    })

    output$table1 <- renderDataTable({
      # Validate for table
      v()
      head(n=input$select_topn,getReactiveWordCount())
    })

    output$plot_s2 <- renderPlot({
      tmp = getCorpusData()
      dico_terme = tmp$dict_terme
      wordcloud(words = dico_terme$word,freq=dico_terme$n, min.freq=5, max.word=input$max,colors = brewer.pal(8,'Dark2'))
    })

    # DataTable
  output$emploiTable = renderDataTable({
      datatable(
        getReactiveData() %>%
          select(c(nom, intitule,libelle_secteur, libelle_type,libelle_qualification,nom_commune)),
        options =list(pagingType = "simple",pageLength = 5))
    })

  output$entrepriseTable = renderDataTable({
    datatable(
      getReactiveData() %>%
        select(c(intitule,nom, url)),
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
      ggtitle(paste("Analyse des correspondances et visualisation")) +
      scale_fill_manual(values=c("#226D68"))
    ggplotly(p)
  })

  output$plot_lda <- renderPlot({
    if(input$kchoice=='Choisir le k'){
      lda_fixed_k(getReactiveData(),k=input$kfixed, iter=input$iter)
    }else{
      lda_best_k(getReactiveData(), iter=input$iter)
    }
  })
  output$plot_tree <- renderPlot({
    if(input$modalite_pos != 'Choisir modalité positive' & input$algo != 'Choisir algorithme'){
      if(input$algo== 'Arbre de décision'){
        tree = supervisedLearningReactive()
        rpart.plot(tree)
      }else if(input$algo== 'SVM'){
        svm = supervisedLearningReactive()
        plot(svm)

      }else{
        rn = supervisedLearningReactive()
        plot(rn)
      }
    }else{
      error_msg()
    }
  })

  output$table_result = renderDataTable({
    if(input$modalite_pos != 'Choisir modalité positive' & input$algo != 'Choisir algorithme'){
      if(input$algo == 'Arbre de décision'){
        tree = supervisedLearningReactive()
        dd = as.data.frame(tree$variable.importance)
        dd
      }else if(input$algo == 'SVM'){
        svm = supervisedLearningReactive()
        subset(as.data.frame(svm$results), select=c(C,sigma,Accuracy))
      }else{
        rn = supervisedLearningReactive()
        as.data.frame(rn$results)
      }
    }else{
      error_msg()
    }
  })

  observeEvent(input$action, {
    # Check that data object exists and is data frame.
    # L'ecart ne doit pas depasser 150
    if ((input$Debut >= 0 && input$Debut<850) && (input$Debut < input$Fin) && (input$Fin - input$Debut < 150)) {
      # Appel methode  API et chargement dans la BD
      output$info <- renderPrint({
        'Chargement des données en cours'
      })
      # FONCTION DIFICILE À AUTOMATISER DU AU FAIT QUE L'API CHANGE SOUVENT DE CONFIGURATION
      # ÇA MARCHER JUSQU'ICI, MAIS JE LE COMMENTE AU CAS OU, AVANT QUE ÇA BUG.

      #loadJobApiInDB(input$Debut,input$Fin,input$secteur)
      output$info <- renderPrint({
        'Données chargées avec succes dans la BD'
      })

    } else {
      output$info <- renderPrint({
        'Erreur de chargement, veuillez verifier les logs!!'
      })
    }
  })

})
