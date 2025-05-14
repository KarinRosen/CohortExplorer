CohortExplorer
In English below.

Kirjeldus
R-pakett valmis bakalaureusetöö raames 2025. aastal. 
Bakalaureusetöö eesmärk oli arendada tööriist, mis kirjeldaks kohorte Observational Medical Outcomes Partnership (OMOP) kujul terviseandmetel.
R-paketi töövoog sisaldab andmebaasipäringute loomist ja teostamist ning saadud andmete esitamist interaktiivsete joonistega. R-paketi sisendiks on tööriistaga ATLAS eeldefineeritud kohordid.

Kasutamine
Eeldused
Olemasolev R töökoht
Ligipääs OMOP CDM kujul andmebaasile
Ligipääs tööriistale ATLAS
Juhend
Kõigepealt peab olemas olema kohort, mida kirjeldama hakatakse ning peab looma andmebaasiühenduse.
Kasutamiseks tuleb sisestada failis db_connect.R puuduvad andmebaasiväärtused. Töövoo käivitamiseks kasuta app.R faili.
Puuduolevad paketid laetakse rakenduse käivitamisel automaatselt.

Description
The R package was developed in 2025 as part of a bachelor's thesis.
The goal of the thesis was to develop a tool that describes cohorts based on health data structured according to the Observational Medical Outcomes Partnership (OMOP) Common Data Model (CDM).
The workflow of the R package includes generating and executing database queries, as well as presenting the resulting data using interactive plots. The input for the R package consists of cohorts predefined with the ATLAS tool.

Usage
Requirements

An existing R environment

Access to a database in OMOP CDM format

Access to the ATLAS tool

Instructions
First, a cohort to be described must exist, and a database connection must be established.
To use the package, fill in the missing database connection details in the db_connect.R file.
To start the workflow, use the app.R file.
Any missing R packages will be automatically installed when the application is launched.
