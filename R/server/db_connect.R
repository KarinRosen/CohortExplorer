readRenviron("~/.Renviron")

DATABASE <- paste(Sys.getenv("DB_HOST"), "/", Sys.getenv("DB_NAME"), sep = "")
DB_USER <- Sys.getenv("DB_USER")
DB_PASSWORD <- Sys.getenv("DB_PASSWORD")
DB_PORT <- Sys.getenv("DB_PORT")
JDBC_JAR <- Sys.getenv("DATABASE_CONNECTOR_JAR")

connectionDetails <- createConnectionDetails(
  dbms = "postgresql",
  server = DATABASE,
  user = DB_USER,
  password = DB_PASSWORD,
  port = DB_PORT,
  pathToDriver = dirname(JDBC_JAR)
)

con <- DatabaseConnector::connect(connectionDetails)

source_schema <- "insert_your_own"       # OMOP schema
cohort_schema <- "insert_your_own"      # Cohort schema
target_schema <- "insert_your_own"     # Your own schema
cohort_id <- 0           # Cohort ID, insert  your own


#' Title
#'
#' @param msg
#'
#' @return
#' @export
#'
#' @examples
log_message <- function(msg) {
  if (!dir.exists("logs")) dir.create("logs")
  write(paste(Sys.time(), msg), file = "logs/app_log.txt", append = TRUE)
}

log_message("Crating miniversion of OMOP database")

# Creating table person from cohort
log_message("Copying table: person")
dbExecute(con, glue("
  DROP TABLE IF EXISTS {target_schema}.person;
  CREATE TABLE {target_schema}.person AS
  SELECT * FROM {source_schema}.person AS p
  WHERE person_id IN (SELECT subject_id FROM {cohort_schema}.cohort WHERE cohort_definition_id = {cohort_id});
"))

# Creating other tables connected to person
related_tables <- list(
  "condition_occurrence",
  "procedure_occurrence",
  "visit_occurrence",
  "death",
  "observation"
)

for (table in related_tables) {
  log_message(glue("Copying table: {table}"))
  dbExecute(con, glue("
    DROP TABLE IF EXISTS {target_schema}.{table};
    CREATE TABLE {target_schema}.{table} AS
    SELECT * FROM {source_schema}.{table}
    WHERE person_id IN (SELECT person_id FROM {target_schema}.person);
  "))
}

# Copying concept table connected to cohort
log_message("Copying relevant concepts")
dbExecute(con, glue("
  DROP TABLE IF EXISTS {target_schema}.concept;
  CREATE TABLE {target_schema}.concept AS
  SELECT * FROM {source_schema}.concept
  WHERE concept_id IN (
    SELECT DISTINCT condition_concept_id FROM {target_schema}.condition_occurrence
    UNION
    SELECT DISTINCT procedure_concept_id FROM {target_schema}.procedure_occurrence
  );
"))

# Control if tables are created
tables <- dbListTables(con, schema = target_schema)
log_message(glue("Tables are created"))


#' Get possibly cohort-defining diagnosis codes
#'
#' @param con
#' @param cohort_id
#' @param cohort_schema
#' @param target_schema
#'
#' @return
#' @export
#'
#' @examples
getCohortStartDiagnoses <- function(con, cohort_id, cohort_schema, target_schema) {
  query <- glue(
    "
    SELECT DISTINCT co.condition_source_value AS icd10_code
    FROM {target_schema}.condition_occurrence co
    JOIN {cohort_schema}.cohort c
      ON co.person_id = c.subject_id
     AND co.condition_start_date = c.cohort_start_date
    WHERE c.cohort_definition_id = {cohort_id}
      AND co.condition_source_value IS NOT NULL
      AND LENGTH(co.condition_source_value) BETWEEN 3 AND 6
    "
  )
  dbGetQuery(con, query)
}

#' Get comorbid diagnoses 1 y before
#'
#' @param con
#' @param cohort_id
#' @param cohort_schema
#' @param target_schema
#'
#' @return
#' @export
#'
#' @examples
getComorbidDiagnoses <- function(con, cohort_id, cohort_schema, target_schema) {
  query <- glue(
    "
     SELECT
      c.subject_id AS person_id,
      c.cohort_start_date,
      p.gender_concept_id,
      p.year_of_birth,
      EXTRACT(YEAR FROM c.cohort_start_date) - p.year_of_birth AS age,
      co.condition_start_date,
      co.condition_source_value AS icd10_code
    FROM {cohort_schema}.cohort c
    JOIN {target_schema}.condition_occurrence co
      ON c.subject_id = co.person_id
    JOIN {target_schema}.person p
      ON c.subject_id = p.person_id
    WHERE c.cohort_definition_id = {cohort_id}
      AND co.condition_start_date <= c.cohort_start_date
      AND co.condition_start_date >= c.cohort_start_date - 365
    "
  )
  dbGetQuery(con, query)
}

#' Get all comorbid diagnoses before cohort start date
#'
#' @param con
#' @param cohort_id
#' @param within_1y
#' @param cohort_schema
#' @param target_schema
#'
#' @return
#' @export
#'
#' @examples
getPreCohortDiagnoses <- function(con, cohort_id, within_1y = FALSE, cohort_schema, target_schema) {
  condition <- if (within_1y == TRUE) {
    "AND co.condition_start_date >= c.cohort_start_date - INTERVAL '365 days'"
  } else {
    ""
  }
  query <- glue(
    "
    SELECT DISTINCT co.condition_source_value AS icd10_code
    FROM {target_schema}.condition_occurrence co
    JOIN {cohort_schema}.cohort c
      ON co.person_id = c.subject_id
    WHERE c.cohort_definition_id = {cohort_id}
      AND co.condition_start_date <= c.cohort_start_date
      AND co.condition_source_value IS NOT NULL
      AND LENGTH(co.condition_source_value) BETWEEN 3 AND 6
      {condition}
    "
  )
  dbGetQuery(con, query)
}

#' Get diagnoses that are connected with visits
#'
#' @param con
#' @param cohort_id
#' @param cohort_schema
#' @param target_schema
#'
#' @return
#' @export
#'
#' @examples
getVisitPeriodDiagnoses <- function(con, cohort_id, cohort_schema, target_schema) {
  query <- glue(
    "
    SELECT DISTINCT co.condition_source_value AS icd10_code
    FROM {target_schema}.condition_occurrence co
    JOIN {cohort_schema}.cohort c
      ON co.person_id = c.subject_id
    WHERE c.cohort_definition_id = {cohort_id}
      AND co.condition_start_date BETWEEN c.cohort_start_date - INTERVAL '365 days'
                                     AND c.cohort_start_date + INTERVAL '365 days'
      AND co.condition_source_value IS NOT NULL
      AND LENGTH(co.condition_source_value) BETWEEN 3 AND 6
    "
  )
  dbGetQuery(con, query)
}

#' Get all diagnoses
#'
#' @param con
#' @param cohort_id
#' @param cohort_schema
#' @param target_schema
#'
#' @return
#' @export
#'
#' @examples
getCohortDiagnoses <- function(con, cohort_id, cohort_schema, target_schema) {
  query <- glue("
    SELECT c.person_id, c.condition_source_value AS icd10_code,
           co.cohort_start_date, c.condition_start_date,
           (c.condition_start_date - co.cohort_start_date) AS days_from_cohort
    FROM {target_schema}.condition_occurrence c
    JOIN {cohort_schema}.cohort co ON c.person_id = co.subject_id
    WHERE co.cohort_definition_id = {cohort_id}
      AND c.condition_source_value IS NOT NULL
  ")
  return(dbGetQuery(con, query))
}

#' Get full cohort data
#'
#' @param con
#' @param cohort_id
#' @param cohort_schema
#' @param target_schema
#'
#' @return
#' @export
#'
#' @examples
getFullCohortData <- function(con, cohort_id, cohort_schema, target_schema) {
  query <- glue("
    SELECT p.person_id, p.gender_concept_id,
           CASE WHEN p.gender_concept_id = 8507 THEN 'Male' ELSE 'Female' END AS gender,
           EXTRACT(YEAR FROM co.cohort_start_date) - p.year_of_birth AS age,
           co.cohort_start_date AS cohort_start_date
    FROM {cohort_schema}.cohort co
    JOIN {target_schema}.person p ON co.subject_id = p.person_id
    WHERE co.cohort_definition_id = {cohort_id}
  ")
  return(dbGetQuery(con, query))
}


#' Get diagnosis source
#'
#' @param con
#' @param cohort_id
#' @param cohort_schema
#' @param target_schema
#'
#' @return
#' @export
#'
#' @examples
getDiagnosisSource <- function(con, cohort_id, cohort_schema, target_schema) {
  query <- glue::glue("
    SELECT
  c.subject_id,
  c.cohort_start_date,
  co.condition_start_date,
  co.condition_source_value AS icd10_code,
  p.gender_concept_id,
  EXTRACT(YEAR FROM c.cohort_start_date) - p.year_of_birth AS age,
  CASE
    WHEN co.condition_type_concept_id = 32810 THEN 'Claim'
    WHEN co.condition_type_concept_id = 32817 THEN 'EHR'
    WHEN co.condition_type_concept_id = 32869 THEN 'Pharmacy'
    ELSE 'Other'
  END AS diagnosis_source
FROM {cohort_schema}.cohort c
JOIN {target_schema}.condition_occurrence co
  ON c.subject_id = co.person_id
  AND c.cohort_start_date = co.condition_start_date
JOIN {target_schema}.person p
  ON c.subject_id = p.person_id
WHERE c.cohort_definition_id = {cohort_id}

  ")

  dbGetQuery(con, query)
}


#' Get visits that are connected with procedures and observations connected to EMO
#'
#' @param con
#' @param cohort_id
#' @param cohort_schema
#' @param target_schema
#'
#' @return
#' @export
#'
#' @examples
getEmoVisits <- function(con, cohort_id, cohort_schema, target_schema) {
  query <- glue::glue("

SELECT
  po.person_id,
  po.visit_occurrence_id,
  NULL AS visit_concept_id,
  po.procedure_date AS visit_start_date,
  c.cohort_start_date,
  DATEDIFF(DAY, c.cohort_start_date, po.procedure_date) AS days_from_cohort,
  'ER' AS visit_type,
  NULL AS specialty_concept_id,
  NULL AS specialty_name,
  co.condition_source_value AS icd10_code
FROM {cohort_schema}.cohort c
JOIN {target_schema}.procedure_occurrence po ON c.subject_id = po.person_id
LEFT JOIN {target_schema}.condition_occurrence co ON po.visit_occurrence_id = co.visit_occurrence_id
WHERE c.cohort_definition_id = {cohort_id}
  AND po.procedure_concept_id IN (4205502, 4056945, 4158569, 4250892, 4075112)
  AND ABS(DATEDIFF(DAY, c.cohort_start_date, po.procedure_date)) <= 365

UNION ALL

SELECT
  o.person_id,
  o.visit_occurrence_id,
  NULL AS visit_concept_id,
  o.observation_date AS visit_start_date,
  c.cohort_start_date,
  DATEDIFF(DAY, c.cohort_start_date, o.observation_date) AS days_from_cohort,
  'ER' AS visit_type,
  NULL AS specialty_concept_id,
  NULL AS specialty_name,
  co.condition_source_value AS icd10_code
FROM {cohort_schema}.cohort c
JOIN {target_schema}.observation o ON c.subject_id = o.person_id
LEFT JOIN {target_schema}.condition_occurrence co ON o.visit_occurrence_id = co.visit_occurrence_id
WHERE c.cohort_definition_id = {cohort_id}
  AND o.observation_concept_id IN (42689522, 42690664, 42690500, 42689520)
  AND ABS(DATEDIFF(DAY, c.cohort_start_date, o.observation_date)) <= 365;

  ")
  dbGetQuery(con, query)
}

#' Get visit providers
#'
#' @param con
#' @param cohort_id
#' @param cohort_schema
#' @param target_schema
#'
#' @return
#' @export
#'
#' @examples
getProviderVisits <- function(con, cohort_id, cohort_schema, target_schema) {
  query <- glue::glue("
  SELECT
  vo.visit_occurrence_id,
  vo.person_id,
  vo.visit_concept_id,
  vo.visit_source_value,
  c.cohort_start_date,
  vo.visit_start_date,
  EXTRACT(YEAR FROM c.cohort_start_date) - p.year_of_birth AS age,
  DATEDIFF(DAY, c.cohort_start_date, vo.visit_start_date) AS days_from_cohort,
  p.gender_concept_id,
  cod.condition_source_value AS icd10_code,
  CASE
    WHEN vo.visit_source_value IN ('A', 'E') THEN 'Outpatient specialist care'
    WHEN vo.visit_source_value = 'L' THEN 'Inpatient nursing care'
    WHEN vo.visit_source_value = 'P' THEN 'Family doctor'
    WHEN vo.visit_source_value = 'S' THEN 'Inpatient specialist care'
    ELSE 'Other'
  END AS visit_type
FROM {target_schema}.visit_occurrence vo
JOIN {cohort_schema}.cohort c
  ON vo.person_id = c.subject_id
  AND vo.visit_start_date BETWEEN c.cohort_start_date - INTERVAL '1 year'
                              AND c.cohort_start_date + INTERVAL '1 year'
JOIN {target_schema}.person p
  ON vo.person_id = p.person_id
LEFT JOIN {target_schema}.condition_occurrence cod
  ON vo.visit_occurrence_id = cod.visit_occurrence_id
WHERE c.cohort_definition_id = {cohort_id}
  AND vo.visit_type_concept_id = 32810

  ")
  dbGetQuery(con, query)
}

#' Get claim source breakdown
#'
#' @param con
#' @param cohort_id
#' @param cohort_schema
#' @param target_schema
#'
#' @return
#' @export
#'
#' @examples
getClaimSourceBreakdown <- function(con, cohort_id, cohort_schema, target_schema) {
  query <- glue::glue("
  SELECT
    co.person_id,
    co.condition_occurrence_id,
    co.condition_concept_id,
    co.condition_start_date,
    co.condition_type_concept_id,
    co.visit_occurrence_id,
    vo.visit_start_date,
    vo.visit_type_concept_id,
    vo.visit_source_value,
    co.condition_source_value AS icd10_code,
    CASE
      WHEN vo.visit_source_value IN ('A', 'E') THEN 'Outpatient specialist care'
      WHEN vo.visit_source_value = 'L' THEN 'Inpatient nursing care'
      WHEN vo.visit_source_value = 'P' THEN 'Family doctor'
      WHEN vo.visit_source_value = 'S' THEN 'Inpatient specialist care'
      ELSE 'Other'
    END AS visit_source_type,
    ch.cohort_start_date,
    EXTRACT(YEAR FROM ch.cohort_start_date) - p.year_of_birth AS age,
    p.gender_concept_id
  FROM {target_schema}.condition_occurrence co
  JOIN {target_schema}.visit_occurrence vo
    ON co.visit_occurrence_id = vo.visit_occurrence_id
  JOIN {cohort_schema}.cohort ch
    ON co.person_id = ch.subject_id
    AND ch.cohort_definition_id = {cohort_id}
    AND co.condition_start_date = ch.cohort_start_date
  JOIN {target_schema}.person p
    ON co.person_id = p.person_id
  WHERE vo.visit_type_concept_id = 32810

")
  dbGetQuery(con, query)
}

#' Get death info
#'
#' @param con
#' @param cohort_id
#' @param cohort_schema
#' @param target_schema
#'
#' @return
#' @export
#'
#' @examples
getDeathAfterCohort <- function(con, cohort_id, cohort_schema, target_schema) {
  query <- glue("
    SELECT c.subject_id,
           d.death_date,
           c.cohort_start_date,
           p.gender_concept_id,
           p.year_of_birth,
           EXTRACT(YEAR FROM c.cohort_start_date) - p.year_of_birth AS age,
           co.condition_source_value
    FROM {cohort_schema}.cohort c
    JOIN {target_schema}.person p ON c.subject_id = p.person_id
    JOIN {target_schema}.death d ON c.subject_id = d.person_id
    LEFT JOIN {target_schema}.condition_occurrence co
      ON c.subject_id = co.person_id AND co.condition_start_date = c.cohort_start_date
    WHERE c.cohort_definition_id = {cohort_id}
      AND d.death_date >= c.cohort_start_date
  ")
  dbGetQuery(con, query)
}


cohort_data <- getFullCohortData(con, cohort_id, cohort_schema,target_schema)
cohort_diagnoses <- getCohortDiagnoses(con, cohort_id, cohort_schema, target_schema)
cohort_diagnosis_source <- getDiagnosisSource(con, cohort_id, cohort_schema, target_schema)
emo_visits_data <- getEmoVisits(con, cohort_id, cohort_schema, target_schema)
death_data <- getDeathAfterCohort(con, cohort_id, cohort_schema, target_schema)
provider_visits_data <- getProviderVisits(con, cohort_id, cohort_schema, target_schema)
cohort_start_dx <- getCohortStartDiagnoses(con, cohort_id, cohort_schema,target_schema)
all_comorbid_dx <- getPreCohortDiagnoses(con, cohort_id, within_1y = FALSE,cohort_schema,target_schema)
comorbid_1y_dx <- getPreCohortDiagnoses(con, cohort_id, within_1y = TRUE, cohort_schema,target_schema)
visit_dx <- getVisitPeriodDiagnoses(con, cohort_id, cohort_schema,target_schema)
comorbid_diagnoses <- getComorbidDiagnoses(con, cohort_id, cohort_schema,target_schema)
cohort_claim_source <- getClaimSourceBreakdown(con, cohort_id, cohort_schema,target_schema)


