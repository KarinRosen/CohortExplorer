# CohortExplorerICD

## Kirjeldus
**CohortExplorerICD** on R-pakett, mis valmis 2025. aastal bakalaureusetöö raames.  
Bakalaureusetöö eesmärk oli arendada tööriist, mis kirjeldaks kohorte Observational Medical Outcomes Partnership (OMOP) kujul terviseandmetel.

R-paketi töövoog sisaldab:
- Andmebaasipäringute loomist ja teostamist
- Saadud andmete esitamist interaktiivsete joonistega

R-paketi sisendiks on tööriistaga **ATLAS** eeldefineeritud kohordid.

## Kasutamine

### Eeldused
- Olemasolev R töökoht
- Ligipääs OMOP CDM kujul andmebaasile
- Ligipääs tööriistale ATLAS

### Juhend
1. Veendu, et olemas on kohort, mida soovid kirjeldada.
2. Loo andmebaasiühendus, täites `db_connect.R` failis puuduvad andmebaasiväärtused.
3. Käivita töövoog kasutades `app.R` faili.

Puuduvad R-paketid paigaldatakse rakenduse käivitamisel automaatselt.

---

# CohortExplorerICD

## Description
*CohortExplorerICD* is an R package developed in 2025 as part of a bachelor's thesis project.  
The goal of the thesis was to develop a tool for describing cohorts based on health data structured according to the **Observational Medical Outcomes Partnership (OMOP) Common Data Model (CDM)**.

The workflow of the R package includes:
- Generating and executing database queries
- Presenting the resulting data using interactive plots

The input for the package consists of cohorts predefined using the **ATLAS** tool.

## Usage

### Requirements
- An existing R environment
- Access to a database in OMOP CDM format
- Access to the ATLAS tool

### Instructions
1. Ensure that a cohort to be analyzed already exists.
2. Establish a database connection by filling in the missing database connection details in the `db_connect.R` file.
3. Launch the workflow by running the `app.R` file.

Any missing R packages will be automatically installed when the application is launched.
