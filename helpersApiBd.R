# Chargement des librairies
library(httr)
library(jsonlite)
library(RMySQL)
library(dplyr)
library(stringr)


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
r = generateToken(urlPost)
print(content(r))
print(paste("Token: ",content(r)$access_token))
print(paste("Expire in:",content(r)$expires_in/60,"min"))

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
getNumberOfRows<- function(table){
  db <- getSingleConnexion()
  query <- sprintf("SELECT COUNT(*) FROM %s", paste(table, collapse = ", "))
  res = dbGetQuery(db, query)
  dbDisconnect(db)
  return(res)
}

getDistinctSecteur<- function(table, colonne){
  db <- getSingleConnexion()
  query <- sprintf("SELECT count(distinct %s) FROM %s",colonne ,table)
  res = dbGetQuery(db, query)
  dbDisconnect(db)
  return(res)
}

# repartition des jobs selon les secteurs
getPostBySecteur<- function(){
  db <- getSingleConnexion()
  query <- query <- sprintf("SELECT p.code_secteur,s.libelle as libelle_secteur,p.code_nature_contrat,n.libelle as libelle_nature, p.code_type_contrat, t.libelle as libelle_type FROM POST p
                            INNER JOIN secteursActivites s ON p.code_secteur = s.code_secteur
                            INNER JOIN natureContrat n ON p.code_nature_contrat = n.code_natureContrat
                            INNER JOIN typeContrat t ON p.code_type_contrat = t.code_type_contrat")
  dbSendQuery(db, "SET NAMES utf8mb4;")
  dbSendQuery(db, "SET CHARACTER SET utf8mb4;")
  res = dbGetQuery(db, query)
  dbDisconnect(db)
  return(res)
}


getPostBySecteur() %>% ggplot(aes(x = code_secteur)) +
  geom_bar(fill="#226D68") +
  ggtitle("Repartition des Posts selon les secteurs")
















