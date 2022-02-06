# Chargement des librairies
library(tidytext)
library(httr)
library(jsonlite)
library(RMySQL)
library(dplyr)
library(tidyr)
library(wordcloud)
library(stringr)
library(tm)
library(plotly)
library(leaflet)
library(FactoMineR)
library(qdap)
library(ggplot2)
library(scales)
library(lubridate)
library(GGally)
library(topicmodels)
library(forcats)



`%>%` <- magrittr::`%>%`

# Parametres de connexion API
options(api = list(
  "urlToken" = "https://entreprise.pole-emploi.fr/connexion/oauth2/access_token?realm=/partenaire",
  "urlRegions" = "https://geo.api.gouv.fr/regions",
  "urlCommunes" = "https://geo.api.gouv.fr/communes",
  "urlDepartement" = "https://geo.api.gouv.fr/departements",
  "grant_type" = "client_credentials",
  "client_id" = "PAR_rwebscraping_d2ae1885e3c2634ba1cef0fddd4ae4e06ca365f9f4834d9327e628c27bb0003d",
  "client_secret" = "44170cb8d29501da958fadfed52531832ad108728a0ab09297f36ca59077be2b",
  "scope" = "api_offresdemploiv2 application_PAR_rwebscraping_d2ae1885e3c2634ba1cef0fddd4ae4e06ca365f9f4834d9327e628c27bb0003d o2dsoffre"
))

# Parametres de connexion BD
options(mysql = list(
  "host" = "127.0.0.1",
  "port" = 8889,
  "user" = "root",
  "password" = "root",
  "databaseName" = "bd_emploi"
))

# Function to connect in BD
getSingleConnexion <- function(){
  db <- dbConnect(MySQL(),
                  dbname = options()$mysql$databaseName,
                  host = options()$mysql$host,
                  port = options()$mysql$port,
                  user = options()$mysql$user,
                  password = options()$mysql$password)
  return(db)
}

# Function to get access token from API
generateToken <- function(url){
  body = list(grant_type =  options()$api$grant_type,
              client_id = options()$api$client_id,
              client_secret = options()$api$client_secret,
              scope = options()$api$scope)
  query = POST(options()$api$urlToken, body=body, encode = "form")
  return(query)
}

# Test Acess Token
r = generateToken(urlToken)
attributes(r)
print(httr::content(r))
print(paste("Token: ",httr::content(r)$access_token))
print(paste("Expire in:",httr::content(r)$expires_in/60,"min"))

# Test connexion BD
con = getSingleConnexion()
summary(con)


# Code de chargement des données dans la BD
loadRegionInBdRegionFromApi <- function(){
  # Get Region From DataGouv API
  df = jsonlite::fromJSON(options()$api$urlRegions)
  # Open connection to the database
  db <- getSingleConnexion()
  apply(df, 1, function(row){
    # Build Query
    query <-sprintf(
      "INSERT INTO %s (%s,%s) VALUES ('%s','%s')",
      "region",
      paste("code_region", collapse = ", "),
      paste("nom_region", collapse = ", "),
      paste(row[2], collapse = ", "),
      paste(gsub("'", "", row[1]) , collapse = ", "))
    # Insert Row In DB
    dbSendQuery(db, "SET NAMES utf8mb4;")
    dbSendQuery(db, "SET CHARACTER SET utf8mb4;")
    res = dbGetQuery(db, query)
  })
  dbDisconnect(db)
}

# Insertion des régions dans la base
#loadRegionInBdRegionFromApi()
#__________________________________

loadDepartementInBdFromApi<-function(){
  # Get Region From DataGouv API
  df = jsonlite::fromJSON(options()$api$urlDepartement)
  # Open connection to the database
  db <- getSingleConnexion()
  apply(df, 1, function(row){
    # Build Query
    query <-sprintf(
      "INSERT INTO %s (%s,%s,%s) VALUES ('%s','%s','%s')",
      "departement",
      paste("code_dept", collapse = ", "),
      paste("code_region", collapse = ", "),
      paste("nom_dept", collapse = ", "),
      paste(row[2], collapse = ", "),
      paste(row[3] , collapse = ", "),
      paste(gsub("'", "", row[1]), collapse = ", "))
    # Insert Row In DB
    dbSendQuery(db, "SET NAMES utf8mb4;")
    dbSendQuery(db, "SET CHARACTER SET utf8mb4;")
    dbGetQuery(db, query)
  })
  dbDisconnect(db)
}

# Insertion des régions dans la base
#loadDepartementInBdFromApi()
#____________________________

loadCommuneInBdFromApi <- function(){
  # Get Region From DataGouv API
  df = jsonlite::fromJSON(options()$api$urlCommunes)
  # Open connection to the database
  db <- getSingleConnexion()
  apply(df, 1, function(row){
    # Build Query
    query <-sprintf(
      "INSERT INTO %s (%s,%s,%s,%s) VALUES ('%s','%s','%s','%s')",
      "commune",
      paste("code_commune", collapse = ", "),
      paste("code_dept", collapse = ", "),
      paste("nom_commune", collapse = ", "),
      paste("code_region", collapse = ", "),
      paste(row[2], collapse = ", "),
      paste(row[3] , collapse = ", "),
      paste(gsub("'", "", row[1]), collapse = ", "),
      paste(row[4] , collapse = ", "))

    # Insert Row In DB
    dbSendQuery(db, "SET NAMES utf8mb4;")
    dbSendQuery(db, "SET CHARACTER SET utf8mb4;")
    res = dbGetQuery(db, query)
  })
  dbDisconnect(db)
}

#loadCommuneInBdFromApi(r)

# Insertion des secteurs d'activité dans la base
#token = generateToken(urlPost)
loadSecteurActInBdFromApi<- function(token){
  url ="https://api.emploi-store.fr/partenaire/offresdemploi/v2/referentiel/secteursActivites"
  authorization = sprintf("%s %s", content(token)$token_type, content(token)$access_token)
  response = GET(url, add_headers(Authorization = authorization))
  df = jsonlite::fromJSON(toJSON(content(response)))

  # build request
  db <- getSingleConnexion()
  apply(df, 1, function(row){
    # Build Query
    query <-sprintf(
      "INSERT INTO %s (%s,%s) VALUES ('%s','%s')",
      "secteursActivites",
      paste("code_secteur", collapse = ", "),
      paste("libelle", collapse = ", "),
      paste(row[1], collapse = ", "),
      paste(gsub("'", "", row[2]) , collapse = ", "))

    # Insert secteur In DB
    dbSendQuery(db, "SET NAMES utf8mb4;")
    dbSendQuery(db, "SET CHARACTER SET utf8mb4;")
    res = dbGetQuery(db, query)
  })
  dbDisconnect(db)
}

# Load Secteur d'activité
#loadSecteurActInBdFromApi(token)
#____________________________

# Insertion des types de contrat dans la base
loadTypeContratInBdFromApi<- function(token){
  url = "https://api.emploi-store.fr/partenaire/offresdemploi/v2/referentiel/typesContrats"
  authorization = sprintf("%s %s", content(token)$token_type, content(token)$access_token)
  response = GET(url, add_headers(Authorization = authorization))
  df = jsonlite::fromJSON(toJSON(content(response)))

  # get single connexion DB
  db <- getSingleConnexion()

  apply(df, 1, function(row){
    # Build Query
    query <-sprintf(
      "INSERT INTO %s (%s,%s) VALUES ('%s','%s')",
      "typeContrat",
      paste("code_type_contrat", collapse = ", "),
      paste("libelle", collapse = ", "),
      paste(row[1], collapse = ", "),
      paste(gsub("'", "", row[2]) , collapse = ", "))

    # Insert secteur In DB
    dbSendQuery(db, "SET NAMES utf8mb4;")
    dbSendQuery(db, "SET CHARACTER SET utf8mb4;")
    res = dbGetQuery(db, query)
  })
  dbDisconnect(db)
}

# Load Secteur d'activité
#loadTypeContratInBdFromApi(token)
#____________________________

findNatureContratByLibelle<- function(libelle){
  db <- getSingleConnexion()
  query <- sprintf("SELECT * FROM natureContrat where libelle = '%s'", paste(gsub("'", "", libelle), collapse = ", "))
  dbSendQuery(db, "SET NAMES utf8mb4;")
  dbSendQuery(db, "SET CHARACTER SET utf8mb4;")
  res = dbGetQuery(db, query)
  dbDisconnect(db)
  return(res)
}

# Insertion des nature de contrat dans la base
loadNatureContratInBdFromApi<- function(token){
  url = "https://api.emploi-store.fr/partenaire/offresdemploi/v2/referentiel/naturesContrats";
  authorization = sprintf("%s %s", content(token)$token_type, content(token)$access_token)
  response = GET(url, add_headers(Authorization = authorization))
  df = jsonlite::fromJSON(toJSON(content(response)))

  # get single connexion DB
  db <- getSingleConnexion()

  apply(df, 1, function(row){
    # Build Query
    query <-sprintf(
      "INSERT INTO %s (%s,%s) VALUES ('%s','%s')",
      "natureContrat",
      paste("code_natureContrat", collapse = ", "),
      paste("libelle", collapse = ", "),
      paste(row[1], collapse = ", "),
      paste(gsub("'", "", row[2]) , collapse = ", "))
    # Insert secteur In DB
    dbSendQuery(db, "SET NAMES utf8mb4;")
    dbSendQuery(db, "SET CHARACTER SET utf8mb4;")
    res = dbGetQuery(db, query)
  })
  dbDisconnect(db)
}

#____________________________ JOB
loadJobFromApi<- function(token, rangeDebut=NULL,rangeFin=NULL, secteurActivite=NULL){
  # 01 France, secteur d'activité: 62(Programmation, conseil et autres activités informatiques)
  #26(Fabrication de produits informatiques, électroniques et optiques) 63(service information)
  #61: telecommunication, 60:Programmation et diffusion, 65: assurrance, 72: Recherche-développement scientifique
  url <- sprintf("https://api.emploi-store.fr/partenaire/offresdemploi/v2/offres/search?paysContinent=01&range=%s-%s&secteurActivite=%s",rangeDebut,rangeFin,secteurActivite)
  #url = "https://api.emploi-store.fr/partenaire/offresdemploi/v2/offres/search?paysContinent=01&range=300-449&secteurActivite=61"
  authorization = sprintf("%s %s", content(token)$token_type, content(token)$access_token)
  response = GET(url, add_headers(Authorization = authorization), Encoding="UTF-8")
  df = jsonlite::fromJSON(toJSON(content(response)))
  # On supprime les jobs sans nom d'entreprise ou localisation sans
  df = df$resultats
  df = df[-which(df$entreprise$nom=='NULL' | df$lieuTravail$commune=='NULL'),]
  entrepriseDf = df$entreprise %>% distinct(nom, .keep_all= TRUE)
  entrepriseDf = subset(entrepriseDf, select = c(nom,description,url,logo))

  # Build data frame POST
  dataf = cbind(df$entreprise$nom, df$lieuTravai$commune, df$lieuTravai$libelle, df$salaire$libelle)
  colnames(dataf) <- c("nomentreprise", "codecommune", "libellelieuTravail", "libellesalaire")
  tmp = cbind(df,dataf)
  if(is.element("competences", colnames(tmp)) ){
    dfPost = subset(tmp, select = c(id, nomentreprise, secteurActivite, typeContrat, natureContrat, qualificationLibelle,
                                    codecommune, dateCreation, dateActualisation, intitule, description, libellesalaire,
                                    experienceExige, experienceLibelle, dureeTravailLibelle, dureeTravailLibelleConverti,
                                    nombrePostes, alternance, libellelieuTravail, competences))
  }
  else{
    tmp$competences = "NULL"
    dfPost = subset(tmp, select = c(id, nomentreprise, secteurActivite, typeContrat, natureContrat, qualificationLibelle,
                                    codecommune, dateCreation, dateActualisation, intitule, description, libellesalaire,
                                    experienceExige, experienceLibelle, dureeTravailLibelle, dureeTravailLibelleConverti,
                                    nombrePostes, alternance, libellelieuTravail, competences))
  }

  # Code pour inserer les entreprises
  loadCompany(entrepriseDf)
  # Code pour inserer les posts
  savePost(dfPost)
  return (list(en = entrepriseDf, dfPost = dfPost))
}

loadAllJobFromAPI<- function(token, secteurActivite){
  rangeDebut <- 0
  rangeFin <- 149
  while(rangeDebut<200 & rangeDebut < rangeFin){
    loadJobFromApi(token, rangeDebut, rangeFin, secteurActivite)
    rangeDebut = rangeFin+1
    rangeFin = rangeFin+150
  }
  print("Chargement terminé")
}
#_________Chargement_______
#r = generateToken(urlPost)
#loadAllJobFromAPI(r, 86) # 61: telecommunication, 65 Assurance

#__________Verifier que le poste n'existe pas déja dans la base_________________
findPostById <- function(id){
  db <- getSingleConnexion()
  query <- sprintf("SELECT * FROM POST where ID = '%s'", paste(id, collapse = ", "))
  dbSendQuery(db, "SET NAMES utf8mb4;")
  dbSendQuery(db, "SET CHARACTER SET utf8mb4;")
  res = dbGetQuery(db, query)
  dbDisconnect(db)
  return(res)
}

findCommuneByCode <- function(code){
  db <- getSingleConnexion()
  query <- sprintf("SELECT * FROM commune where code_commune = '%s'", paste(code, collapse = ", "))
  dbSendQuery(db, "SET NAMES utf8mb4;")
  dbSendQuery(db, "SET CHARACTER SET utf8mb4;")
  res = dbGetQuery(db, query)
  dbDisconnect(db)
  return(res)
}

#____________________INSERTION DES EMPLOIS DANS LA TABLE DE FAIT________________
savePost<- function(df){
  tz <- Sys.timezone()
  apply(df, 1, function(row){
    # Verifier si le post ne figure pas deja dans la base
    res = findPostById(row[1])
    commune = findCommuneByCode(row[7])
    entreprise = findEntrepriseByName(str_to_upper(gsub("'", "", row[2])))
    natureContrat = findNatureContratByLibelle(row[5])

    if(dim(res)[1]==0 & (dim(commune)[1]!=0 & dim(entreprise)[1]!=0 & dim(entreprise)[1]!=0 & length(row[4])>0)){
      # Construire le champs competence
      if(row[length(row)] != 'NULL'){
        df = as.data.frame(row[length(row)])
        competences = paste(as.character(df[,2]), collapse = " ")
      }else{
        competences = ""
      }
      if(length(findNatureContratByLibelle(row[5])$code_natureContrat) > 0){
        code_natureContrat = findNatureContratByLibelle(row[5])$code_natureContrat
      }else{
        code_natureContrat = "E1"
      }
      # Build Query
      query <-sprintf(
        "INSERT INTO POST (%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s) VALUES ('%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%s')",
        paste("id", collapse = ", "),
        paste("ref_entreprise", collapse = ", "),
        paste("code_secteur", collapse = ", "),
        paste("code_type_contrat", collapse = ", "),
        paste("code_nature_contrat", collapse = ", "),
        paste("libelle_qualification", collapse = ", "),
        paste("code_commune", collapse = ", "),
        paste("dateCreation", collapse = ", "),
        paste("dateActualisation", collapse = ", "),
        paste("intitule", collapse = ", "),
        paste("description", collapse = ", "),
        paste("libelleSalaire", collapse = ", "),
        paste("experienceExige", collapse = ", "),
        paste("experienceLibelle", collapse = ", "),
        paste("dureeTravailLibelle", collapse = ", "),
        paste("dureeTravailConverti", collapse = ", "),
        paste("nombrePostes", collapse = ", "),
        paste("alternance", collapse = ", "),
        paste("libelle_lieu", collapse = ", "),
        paste("competences", collapse = ", "),
        paste(row[1], collapse = ", "), # id
        paste(str_to_upper(gsub("'", "", row[2])), collapse = ", "), # nom_entreprise
        paste(row[3], collapse = ", "), # secteurActivite
        paste(row[4], collapse = ", "), # typeContrat
        paste(code_natureContrat, collapse = ", "), # natureContrat
        paste(gsub("'", "", row[6]), collapse = ", "), # libelle_qualification
        paste(row[7], collapse = ", "), # code_commune
        paste(strptime(as.character(row[8]), tz = tz, format = "%Y-%m-%dT%H:%M:%OSZ"), collapse = ", "), # dateCreation
        paste(strptime(as.character(row[9]), tz = tz, format = "%Y-%m-%dT%H:%M:%OSZ"), collapse = ", "), # dateActualisation
        paste(gsub("'", "", row[10]), collapse = ", "), # intitule
        paste(gsub("'", "", row[11]), collapse = ", "), # description
        paste(gsub("'", "", row[12]), collapse = ", "), # libelleSalaire
        paste(gsub("'", "", row[13]), collapse = ", "), # experienceExige
        paste(gsub("'", "", row[14]), collapse = ", "), # experienceLibelle
        paste(gsub("'", "", row[15]), collapse = ", "), # dureeTravailLibelle
        paste(gsub("'", "", row[16]), collapse = ", "), # dureeTravailLibelleConverti
        paste(row[17], collapse = ", "), # nombrePostes
        paste(row[18], collapse = ", "), # alternance
        paste(gsub("'", "", row[19]), collapse = ", "), #libelle_lieu
        paste(gsub("'", "", competences), collapse = ", "))
      print(query)
      # Insert secteur In DB
      db <- getSingleConnexion()
      dbSendQuery(db, "SET NAMES utf8mb4;")
      dbSendQuery(db, "SET CHARACTER SET utf8mb4;")
      res = dbGetQuery(db, query)
      dbDisconnect(db)
    }
  })
}
#_______________________________________________________________________________

# Verifier si l'entreprise n'existe pas dans la base____________________________
findEntrepriseByName<- function(nom){
  db <- getSingleConnexion()
  query <- sprintf("SELECT * FROM entreprise where nom = '%s'", paste(nom, collapse = ", "))
  dbSendQuery(db, "SET NAMES utf8mb4;")
  dbSendQuery(db, "SET CHARACTER SET utf8mb4;")
  res = dbGetQuery(db, query)
  dbDisconnect(db)
  return(res)
}

loadCompany <- function(df){
  apply(df, 1, function(row){
    # verifions si l'entreprise existe deja dans la base
    res = findEntrepriseByName(str_to_upper(gsub("'", "", row[1])))
    if(dim(res)[1] == 0){
      # Build Query
      query <-sprintf(
        "INSERT INTO %s (%s,%s,%s,%s) VALUES ('%s','%s','%s','%s')",
        "entreprise",
        paste("nom", collapse = ", "),
        paste("description", collapse = ", "),
        paste("url", collapse = ", "),
        paste("logo", collapse = ", "),
        paste(str_to_upper(gsub("'", "", row[1])), collapse = ", "),
        paste(gsub("'", "", row[2]), collapse = ", "),
        paste(gsub("'", "", row[3]), collapse = ", "),
        paste(gsub("'", "", row[4]), collapse = ", "))
      print(query)
      # Insert secteur In DB
      db <- getSingleConnexion()
      dbSendQuery(db, "SET NAMES utf8mb4;")
      dbSendQuery(db, "SET CHARACTER SET utf8mb4;")
      res = dbGetQuery(db, query)
      dbDisconnect(db)
    }
  })
}
#_______________________________________________________________________________

#_______________________________________________________________________________
#r = generateToken(urlPost) # Generate token
#dfList = loadJobFromApi(r) # Get data from API and insert in BD
#entrepriseDf = dfList$en # Liste des entreprise
#dfPost = dfList$dfPost # Liste des posts recuperer
#_______________________________________________________________________________

# Recuperer le nombre de ligne chargée
getNumberOfRows<- function(table, domaine="Tous les domaines", colonne_name=NA, annee){
  if(domaine=='Tous les domaines'){
    query <- sprintf("SELECT COUNT(*) FROM %s", paste(table, collapse = ", "))
  }else{
    query <- sprintf("SELECT count(distinct %s) FROM POST p
                   INNER JOIN secteursActivites s ON p.code_secteur = s.code_secteur
                   WHERE s.libelle = '%s' ",colonne_name,domaine)
  }
  db <- getSingleConnexion()
  dbSendQuery(db, "SET NAMES utf8mb4;")
  dbSendQuery(db, "SET CHARACTER SET utf8mb4;")
  res = dbGetQuery(db, query)
  dbDisconnect(db)
  return(res)
}
getNumberOfRows2 <- function(domaine="Tous les domaines", colonne_name=NA, annee='All'){
  df = getPost(domaine)
  df = decompose_date(df)

  if(annee != 'All'){
    df = df[df$annee==annee,]
  }
  if(!is.na(colonne_name)){
    nb <- length(unique(df[,c(colonne_name)]))
  }
  return(nb)
}

getDistinctSecteur<- function(table, colonne){
  db <- getSingleConnexion()
  query <- sprintf("SELECT count(distinct %s) FROM %s",colonne ,table)
  res = dbGetQuery(db, query)
  dbDisconnect(db)
  return(res)
}



getDataFromTable<- function(table){
  db <- getSingleConnexion()
  query <- sprintf("SELECT * FROM %s",table)
  dbSendQuery(db, "SET NAMES utf8mb4;")
  dbSendQuery(db, "SET CHARACTER SET utf8mb4;")
  res = dbGetQuery(db, query)
  dbDisconnect(db)
  return(res)
}

# repartition des jobs selon les secteurs
getPost<- function(domaine = "Tous les domaines"){
  if(domaine == 'Tous les domaines'){
    query <- sprintf("SELECT p.ID, p.ref_entreprise, p.intitule, p.description, p.competences, p.libelle_qualification, p.code_secteur, p.dureeTravailLibelle, p.dateCreation,p.experienceExige, s.libelle as libelle_secteur,
                    p.code_nature_contrat,n.libelle as libelle_nature, p.code_type_contrat, t.libelle as libelle_type, r.nom_region,
                    r.code_region, d.code_dept, d.nom_dept, c.code_commune, c.nom_commune, e.nom, e.description as summary, e.url,e.logo FROM POST p
                            INNER JOIN entreprise e ON p.ref_entreprise = e.nom
                            INNER JOIN secteursActivites s ON p.code_secteur = s.code_secteur
                            INNER JOIN natureContrat n ON p.code_nature_contrat = n.code_natureContrat
                            INNER JOIN typeContrat t ON p.code_type_contrat = t.code_type_contrat
                            INNER JOIN commune c ON p.code_commune = c.code_commune
                            INNER JOIN departement d ON c.code_dept = d.code_dept
                            INNER JOIN region r ON d.code_region = r.code_region")
  }else{
    query <- sprintf("SELECT p.ID,p.ref_entreprise, p.intitule, p.description, p.competences, p.libelle_qualification, p.code_secteur, p.dureeTravailLibelle, p.dateCreation, p.experienceExige, s.libelle as libelle_secteur,
                    p.code_nature_contrat,n.libelle as libelle_nature, p.code_type_contrat, t.libelle as libelle_type, r.nom_region,
                    r.code_region, d.code_dept, d.nom_dept, c.code_commune, c.nom_commune, e.nom, e.description as summary, e.url,e.logo FROM POST p
                            INNER JOIN entreprise e ON p.ref_entreprise = e.nom
                            INNER JOIN secteursActivites s ON p.code_secteur = s.code_secteur
                            INNER JOIN natureContrat n ON p.code_nature_contrat = n.code_natureContrat
                            INNER JOIN typeContrat t ON p.code_type_contrat = t.code_type_contrat
                            INNER JOIN commune c ON p.code_commune = c.code_commune
                            INNER JOIN departement d ON c.code_dept = d.code_dept
                            INNER JOIN region r ON d.code_region = r.code_region
                            WHERE s.libelle = '%s'",domaine)
  }

  db <- getSingleConnexion()
  dbSendQuery(db, "SET NAMES utf8mb4;")
  dbSendQuery(db, "SET CHARACTER SET utf8mb4;")
  res = dbGetQuery(db, query)
  dbDisconnect(db)
  res$code_libellesecteur = paste(res$code_secteur,res$libelle_secteur, sep=" ")
  return(res)
}

#df = getPost()
#colnames(df)

processingCorpus <- function(df, secteur_activite=NA){
  # stopword
  stopwordd = as.data.frame(jsonlite::fromJSON("stop_words_french.json"))
  colnames(stopwordd) = c("stwd")

  #if(!is.na(secteur_activite)){
    # data of secteur activite
    #df = df[df$libelle_secteur==secteur_activite,]
  #}
  # Joindre la colonne description intitule et competences postes
  #df = unite(df, text, description, competences, sep = " ")
  df$text = paste(df$description, df$competences, sep=" ")

  # tible data frame
  df_tible <- tibble(line=1:nrow(df),text=df$text)

  # Nettoyage et tokenisation et lemmatisation avec SnowballC
  clean_df <- df_tible %>%
    mutate(text=gsub(x=text,pattern="[0-9]",replacement="")) %>%
    mutate(text=gsub(x=text,pattern="\n",replacement="")) %>%
    mutate(text=str_to_lower(text)) %>%
    unnest_tokens(output=word,input=text) %>%
    filter(!word %in% stopwordd$stwd) %>%
    select(line,word)
  # dictionnaire terme
  dico_terme <- clean_df %>%
    count(word,sort=TRUE)

  #wordcloud
  # wordcld = wordcloud(words=dico_terme$word,freq=dico_terme$n, min.freq=10, max.word=50,colors = brewer.pal(8,'Dark2'), random.order = FALSE, scale = c(3,.5))

  #comptage des termes par document
  matrice_dtm <- clean_df %>%
    group_by(line,word) %>%
    summarize(freq=n()) %>%
    cast_dtm(document = line, term = word, value = freq) ##"cast" en MDT (pondération = fréquence) #autre pondération possible, ex. TF-IDF

  return(list(df=df, dict_terme = dico_terme, matrice_dt = matrice_dtm))
}



getDataforAC <- function(data){
  res = processingCorpus(data)
  mdt_matrix = as.matrix(res$matrice_dt)
  mat_pond <- ifelse(mdt_matrix>0,1,0)
  df_pond <- as.data.frame(mat_pond)
  sum_per_secteur_act <- aggregate(x=df_pond,by=list(data$code_secteur),sum)
  return(sum_per_secteur_act)
}

## fonction permettant de trouver les longitudes et latitudes
## d'un vecteur contenant plusieurs adresses
if (!(require(jsonlite))) install.packages("jsonlite")
geocodeGratuit <- function(adresses){
  # adresses est un vecteur contenant toutes les adresses sous forme de chaine de caracteres
  nominatim_osm <- function(address = NULL){
    ## details: http://wiki.openstreetmap.org/wiki/Nominatim
    ## fonction nominatim_osm proposée par D.Kisler
    if(suppressWarnings(is.null(address)))  return(data.frame())
    tryCatch(
      d <- jsonlite::fromJSON(
        gsub('\\@addr\\@', gsub('\\s+', '\\%20', address),
             'http://nominatim.openstreetmap.org/search/@addr@?format=json&addressdetails=0&limit=1')
      ), error = function(c) return(data.frame())
    )
    if(length(d) == 0) return(data.frame())
    return(c(as.numeric(d$lon), as.numeric(d$lat)))
  }
  tableau <- t(sapply(adresses,nominatim_osm))
  colnames(tableau) <- c("lon","lat")
  return(tableau)
}

getCoordonneesDept<- function(df){
  dept = unique(df$nom_dept)
  coor_dept= geocodeGratuit(dept)
  return(as.data.frame(coor_dept))
}

getCoordonneesRegion<- function(df){
  region = unique(df$nom_region)
  coor_reg= geocodeGratuit(region)
  return(as.data.frame(coor_reg))
}

getCoordonneesCommune<- function(df){
  communes = unique(df$nom_commune)
  coor_commune = geocodeGratuit(communes)
  return(as.data.frame(coor_commune))
}

df_leaflet<- function(df){
  df = subset(df, select=c(nom_region))
  df = as.data.frame(df %>% group_by(nom_region) %>% summarise(count = n()))
  coord = getCoordonneesRegion(df)
  df$lon = coord$lon
  df$lat = coord$lat
  return(df)
}

dfToCorpusField <- function(df, field="description", plage=100){
  df = head(n=plage, df)
  if(field =="description"){
    all_text_field <- c(df$description)
  }else if(field=="compétence"){
    all_text_field <- c(df$competences)
  }else{
    all_text_desc <- paste(df$description, collapse = "")
    all_competences<- paste(df$competences, collapse = "")
    all_text_field <- c(all_text_desc,all_competences)
  }
  # Clean text field
  all_text_field <- VectorSource(all_text_field)
  all_text_corpus <- VCorpus(all_text_field)
  all_clean <- clean_corpus(all_text_corpus)
  return(list(dtm=DocumentTermMatrix(all_clean), tdm=TermDocumentMatrix(all_clean)))
}

clean_corpus <- function(corpus, field = "description"){
  stopwordd = as.data.frame(jsonlite::fromJSON("stop_words_french.json"))
  colnames(stopwordd) = c("stwd")
  corpus <- tm::tm_map(corpus, removePunctuation)
  corpus <- tm::tm_map(corpus, stripWhitespace)
  corpus <- tm::tm_map(corpus, removeNumbers)
  corpus <- tm::tm_map(corpus, content_transformer(tolower))
  corpus <- tm::tm_map(corpus, removeWords, stopwords::stopwords(language = "fr"))
  corpus <- tm::tm_map(corpus,removeWords,c("contre","cpg","csv","€jour","audelà","aujourdhui","auquel","emploi","ère","cqrs","connaissancesexpertise","les","des","dun","dune","daux","dau","cétait"))
  #corpus <- tm::tm_map(corpus, stemDocument)
  return(corpus)
}

tdm_dtm<- function(clean_corp){
  # generate TDM (terme en ligne et document en clonne)
  tdm <- TermDocumentMatrix(clean_corp)
  # Generate DTM (document en ligne et terme en colonne)
  dtm <- DocumentTermMatrix(clean_corp)
  return(list(tdm =tdm, dtm=dtm) )
}

term_frequency_with_tdm <- function(tdm,top){
  tdm_to_matrix <- as.matrix(tdm)
  # Sum rows and sort by frequency
  term_frequency <- rowSums(tdm_to_matrix)
  term_frequency <- sort(term_frequency,decreasing = TRUE)
  # Create a barplot
  return(barplot(term_frequency[1:top], col = "tan",las = 2))
}

term_frequency_with_qdap<- function(vecteur_text,top){
  frequency <- freq_terms(
    vecteur_text,
    top = top,
    at.least = 3,
    stopwords = "Top200Words"
  )
  # Plot term frequencies
  return(plot(frequency))
}

getHoursByContrat = function(posts){
  posts <- normalize_heurestravail(posts)
  # Sélection des données
  nbHeuresTravail <- posts %>% group_by(dureeTravailLibelle)%>%
    summarise(count = n()) %>%
    filter(dureeTravailLibelle > 23)
  return(nbHeuresTravail)
}

normalize_heurestravail = function(posts){
  posts$dureeTravailLibelle = gsub("[[[:alpha:]]","",df$dureeTravailLibelle)
  posts$dureeTravailLibelle = str_sub(posts$dureeTravailLibelle,start = 1, end = 2)
  posts[posts == ''] <- NA
  posts[posts == ' '] <- NA
  posts = na.omit(posts)
  posts$dureeTravailLibelle = as.integer(posts$dureeTravailLibelle)
  return(posts)
}

decompose_date = function(posts) {
  posts$dateCreation = strptime(posts$dateCreation, format = "%Y-%m-%d %H:%M:%S")
  # Séparation en année, mois, jour
  posts$annee = year(posts$dateCreation)
  posts$mois = month(posts$dateCreation)
  posts$jour = day(posts$dateCreation)
  #On supprime l'heure de date de création
  posts$dateCreation = as.Date(posts$dateCreation,format = "%D")
  return(posts)
}

filterNbEntityByYear <- function(posts, year='all'){
  if(year !='all'){
    p <- posts %>%
      filter(annee==year)
    return(dim(p)[1])
  }
}

Graph_Experience_Qualification = function(posts, secteur='Tous les dommaines'){
  posts$experienceExige = ifelse(posts$experienceExige=="E","Expérience exigée","Débutant accepté")
  posts$experienceExige = as.factor(posts$experienceExige)
  # Transformation en facteur du libelle qualification
  posts$libelle_qualification = as.factor(posts$libelle_qualification)
  # Sélection des données non vides pour la qualification
  qualification <- posts %>% group_by(libelle_qualification)%>%
    filter(libelle_qualification != 'NULL')
  if(secteur != 'Tous les domaines'){
    qualification <- qualification %>%
      filter(libelle_secteur == secteur)
  }
  # Graphique
  ggplot(qualification, aes(x = libelle_qualification, fill = experienceExige)) +
    geom_bar(position = "fill") +
    # Ajout du texte
    geom_text(aes(by = libelle_qualification), stat = "prop", position = position_fill(.5),size=3) +
    # Titre des axes
    xlab("Qualifications") +
    ylab("Proportion") +
    # Titre de la légende + titre général
    labs(fill = "Expérience demandée",title = "Expériences demandées en fonction des qualifications") +
    # Affichage des valeurs sur les barres
    scale_y_continuous(labels = percent) +
    # Affichage incliné des noms des qualifications
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

lda_fixed_k <- function(df, k=2, iter=100){
  res = processingCorpus(head(n=100,df))
  dtm = as.matrix(res$matrice_dt)
  # Initial run
  mod = LDA(x=dtm, method="Gibbs", k=k,
            control=list(alpha=0.5, seed=12345, iter=iter, keep=1))
  # Resumed run
  mod2 = LDA(x=dtm, model=mod,
             control=list(thin=1, seed=10000, iter=iter)) %>%
    tidy(matrix = "beta")

  word_probs <- mod2 %>%
    group_by(topic) %>%
    top_n(15, beta) %>%
    ungroup() %>%
    mutate(term2 = fct_reorder(term, beta))

  ggplot(word_probs, aes(term2,beta,fill = as.factor(topic)))+
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip()
}

lda_best_k <- function(df,iter=100){
  res = processingCorpus(head(n=100,df))
  dtm = as.matrix(res$matrice_dt)
  mod_log_lik = numeric(10)
  mod_perplexity = numeric(10)
  for (i in 2:10) {
    mod = LDA(dtm, k=i, method="Gibbs",
              control=list(alpha=0.5, iter=iter, seed=12345, thin=1))
    mod_log_lik[i] = logLik(mod)
    mod_perplexity[i] = perplexity(mod, dtm)
  }
  number_clusters = 2:10
  ggplot(data.frame(number_of_cluster=2:10,mod_perplexity=mod_perplexity[2:10]),
         aes(x= number_of_cluster, y=mod_perplexity))+
    geom_point(size=2, shape=23,color='red')+
    geom_line()
}













