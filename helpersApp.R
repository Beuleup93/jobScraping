# Chargement des librairies
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







# TESTE
db <- getSingleConnexion()
table1 = "post"
table2 = "secteursActivites"
colonne = 'libelle'
query <- sprintf("select libelle, count(libelle) from POST p inner join secteursActivites s on p.code_secteur = s.code_secteur GROUP BY libelle")
res = dbGetQuery(db, query)
dbDisconnect(db)
dim(res)
if(dim(res)[1] ==0){
  print("TRUE")
}

db <- getSingleConnexion()
query <-sprintf(
  "INSERT INTO %s (%s,%s,%s,%s,%s) VALUES ('%s','%s','%s','%s','%s')",
  "entreprise",
  paste("nom", collapse = ", "),
  paste("url", collapse = ", "),
  paste("entrepriseAdaptee", collapse = ", "),
  paste("description", collapse = ", "),
  paste("logo", collapse = ", "),
  paste("ATOS", collapse = ", "),
  paste("atos.com", collapse = ", "),
  paste(1, collapse = ", "),
  paste("desc", collapse = ", "),
  paste("logo", collapse = ", "))
res = dbGetQuery(db, query)
dbDisconnect(db)
#ALTER TABLE tablename AUTO_INCREMENT = 1
