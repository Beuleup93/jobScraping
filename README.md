### Development of an interactive R-Shiny application for the analysis of job offers using Text Mining techniques

### Description

This project is part of the Text Mining course of the Master 2 SISE. The RShiny application must restore key information from job offers using text mining methods.
We want to analyze the job offers according to several criteria such as the type of job, the required skills, the required experience... The interactive web application R-Shiny must be a per- formant support to guide the user in the exploration and analysis of the corpus.

### Terms of reference

* Whenever possible, job postings will be "pulled" using "web scraping" techniques (i.e. not manually) (e.g. rvest, etc.). The sources and the procedure used should be described in detail in the report.
* This procedure is intended to feed a database, which should be modeled as a warehouse (facts, dimensions). database is stored in a free DBMS (e.g. MySQL, SQLite, etc.).
* The R-Shiny application must be directly linked to the database.
* The analysis must integrate a regional/territorial dimension, we expect to see interactive cartographic representations in the application.
* The application should be as dynamic as possible, interactive graphics (e.g. plotly, etc.) will be appreciated.
* Deliverable: pdf report written in LaTeX, the corpus (the base), the R source code of the Shiny application, the installation files.

### Data retrieval and use via API

* To retrieve the job offers, we used the API of pole emploi https://pole-emploi.io/data/api/offres-emploi?tabgroup-api=documentation&doc-section=api-doc-section-rechercher-par-critères.
* For the geographic breakdown, the API of data.gouv https://www.data.gouv.fr/fr/ .

### Using of the API pole emploi

To use an API, you must first have :
- declared an application,
- subscribed to an API in order to obtain a client ID and a secret key.
<br/> 
This information must be kept carefully, because it is necessary to request an API. You can find them at any time on the configuration page of your application accessible from the dashboard.
To be able to use the Pôle Emploi API, we need to generate an access token with the client identifier and the secret key provided beforehand, which expires after 24 minutes. Once we have the access token, we can query the API and retrieve the job offers in JSON format. The figure below illustrates the principle of generating the token:
<br/> 
<div align="center">
<img width="604" alt="Capture d’écran 2022-04-03 à 12 33 09" src="https://user-images.githubusercontent.com/31353252/161428443-0c007b3b-81b6-44a5-81b9-d792fe1687cf.png">
</div>
<br/> 

### API consumption with Rstudio

We have created a code that allows to connect to the API from R. We have encapsulated in a function the connection links and all the parameters to query the API, generate an access token and through it, we retrieve the Json object that we then transform into dataframe.
This dataframe will then be processed to be integrated into the database to which the Shiny application will connect. From there, the data are stored in the database, all the links between API, database and R script are realized.

### Constitution of the database

We have retrieved the data of job offers and geographic breakdowns using APIs and we need to store them in a database. To do this, we first created the data model by analyzing the data returned by the APIs. Then we automated the process of retrieving the data from the APIs and loading it into the database (MYSQL).
<br/> 
**Data Model**
<div align="center">
<img width="300" alt="Capture d’écran 2022-04-03 à 13 00 25" src="https://user-images.githubusercontent.com/31353252/161429334-ad3f1186-27aa-40aa-becd-be56e52793da.png">
</div>
<br/> 

**Loaded Data in MySQL Database**
<div align="center">
<img width="500" alt="Capture d’écran 2022-04-03 à 13 05 51" src="https://user-images.githubusercontent.com/31353252/161429610-47c35bfe-93cc-46dd-939d-cc987271dc61.png">
</div>
<br/> 

<div align="center">
<img width="500" alt="Capture d’écran 2022-04-03 à 13 06 29" src="https://user-images.githubusercontent.com/31353252/161429616-9c475b55-14ca-45b0-a48b-ee0a2f54a57b.png">
</div>
<br/> 

### Presentation of the RShiny application

#### Architecture of the application

<div align="center">
<img width="775" alt="Capture d’écran 2022-04-03 à 13 11 21" src="https://user-images.githubusercontent.com/31353252/161429986-414241c4-3766-4584-a75d-3d1e0afbbc42.png">
</div>
<br/> 

#### RShiny application

<div align="center">
<img width="750" alt="Capture d’écran 2022-04-03 à 17 48 17" src="https://user-images.githubusercontent.com/31353252/161441328-bb693d52-0da1-4357-8143-3c384e4e1135.png">

<img width="750" alt="Capture d’écran 2022-04-03 à 17 51 24" src="https://user-images.githubusercontent.com/31353252/161441335-3e27d225-abcf-4293-a4d6-2774ff093316.png">

<img width="750" alt="Capture d’écran 2022-04-03 à 17 51 50" src="https://user-images.githubusercontent.com/31353252/161441341-df661ced-02e7-4fb5-b5e9-5a0b83c500cf.png">

<img width="750" alt="Capture d’écran 2022-04-03 à 17 52 07" src="https://user-images.githubusercontent.com/31353252/161441356-fcf7e1b7-c761-4045-b28d-80ae51cfd914.png">

<img width="750" alt="Capture d’écran 2022-04-03 à 17 52 36" src="https://user-images.githubusercontent.com/31353252/161441362-d0e8d863-abbe-473b-83f2-a6aedac73378.png">
</div>
<br/>
