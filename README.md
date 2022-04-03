### Development of an interactive R-Shiny application for the analysis of job offers using Text Mining techniques

### DESCRIPTION

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
- subscribed to an API in order to obtain a client ID and a secret key.<br/> 
This information must be kept carefully, because it is necessary to request an API. You can find them at any time on the configuration page of your application accessible from the dashboard.
To be able to use the Pôle Emploi API, we need to generate an access token with the client identifier and the secret key provided beforehand, which expires after 24 minutes. Once we have the access token, we can query the API and retrieve the job offers in JSON format. The figure below illustrates the principle of generating the token:
<br/> 
<div align="center">
<img width="604" alt="Capture d’écran 2022-04-03 à 12 33 09" src="https://user-images.githubusercontent.com/31353252/161428443-0c007b3b-81b6-44a5-81b9-d792fe1687cf.png">
</div>
<br/> 
