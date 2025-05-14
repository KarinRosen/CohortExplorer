source("server/plot_helpers.R")
source("server/db_connect.R")

#' Server function
#'
#' @param input
#' @param output
#' @param session
#'
#' @return
#' @export
#'
#' @examples
server <- function(input, output, session) {
  message("🟢 Shiny server starting!")
  observe({
    updatePickerInput(session, "primaryDiagnosis", choices = cohort_start_dx$icd10_code)
    updatePickerInput(session, "comorbidDiagnoses", choices = all_comorbid_dx$icd10_code)

    updatePickerInput(session, "primaryDiagnosis_comorbid", choices = cohort_start_dx$icd10_code)


    updatePickerInput(session, "comorbidDiagnoses1y", choices = if (input$icd_grouping) substr(comorbid_1y_dx$icd10_code, 1, 3) else comorbid_1y_dx$icd10_code)
    updatePickerInput(session, "primaryDiagnosis_visits", choices = cohort_start_dx$icd10_code)

    updatePickerInput(session, "visitDiagnoses", choices = visit_dx$icd10_code)
    updatePickerInput(session, "visitcomorbidDiagnoses", choices = all_comorbid_dx$icd10_code)
    updatePickerInput(session, "anyDiagnosisBefore", choices = all_comorbid_dx$icd10_code)
  })



  output$ageFilter<- renderUI({
    step_val <- as.numeric(input$age_grouping)

    sliderInput("ageFilter", NULL,
                min = 0, max = 120, value = c(0, 120),  step = step_val)
  })


  output$dynamicTimeSlider <- renderUI({
    if (input$selectedTab == "cohort_analysis" | input$selectedTab == "diagnoses" ) {
      return(NULL)
    } else if (input$selectedTab == "mortality") {

      sliderInput("death_month_range", " ",
                  min = 0, max = 48, value = c(0, 48), step = 1)


    }else if (input$selectedTab == "comorbid_diagnoses") {
      sliderInput("timeFilter", " ",
                  min = -12, max = 0, value = c(-12, 0), step = 1, post = " months")

    }else {
      sliderInput("timeFilter", " ",
                  min = -12, max = 12, value = c(-12, 12), step = 1, post = " months")
    }
  })

  filtered_cohort <- reactive({
    age_range <- input$ageFilter
    if (is.null(age_range) || length(age_range) != 2) {
      age_range <- c(0, 120)
    }

    data <- cohort_data %>%
      filter(age >= age_range[1], age <= age_range[2])


    data <- data %>%
      mutate(
        gender = case_when(
          gender_concept_id == 8507 ~ "Male",
          gender_concept_id == 8532 ~ "Female",
          TRUE ~ "Other"
        )
      )

    if (input$gender_display == "male") {
      data <- data %>% filter(gender == "Male")
    } else if (input$gender_display == "female") {
      data <- data %>% filter(gender == "Female")
    }

    if (input$selectedTab %in% c("cohort_analysis", "diagnoses")) {
      if (!is.null(input$primaryDiagnosis)) {
        ids <- cohort_diagnoses %>%
          filter(condition_start_date == cohort_start_date, icd10_code %in% input$primaryDiagnosis) %>%
          pull(person_id)
        data <- data %>% filter(person_id %in% ids)
      }
      if (!is.null(input$comorbidDiagnoses)) {
        ids <- cohort_diagnoses %>%
          filter(condition_start_date < cohort_start_date, icd10_code %in% input$comorbidDiagnoses) %>%
          pull(person_id)
        data <- data %>% filter(person_id %in% ids)
      }
    }

    if (input$selectedTab == "visits") {
      if (!is.null(input$visitcomorbidDiagnoses)) {
        ids <- cohort_diagnoses %>%
          filter(condition_start_date <= cohort_start_date, icd10_code %in% input$visitcomorbidDiagnoses) %>%
          pull(person_id)
        data <- data %>% filter(person_id %in% ids)
      }

    }
    if (input$selectedTab == "comorbid_diagnoses") {
      if (!is.null(input$primaryDiagnosis_comorbid)) {
        ids <- cohort_diagnoses %>%
          filter(condition_start_date == cohort_start_date, icd10_code %in% input$primaryDiagnosis_comorbid) %>%
          pull(person_id)
        data <- data %>% filter(person_id %in% ids)
      }
      if (!is.null(input$comorbidDiagnoses1y)) {
        ids <- cohort_diagnoses %>%
          filter(condition_start_date < cohort_start_date &
                   condition_start_date >= cohort_start_date - 365,
                 icd10_code %in% input$comorbidDiagnoses1y) %>%
          pull(person_id)

        data <- data %>% filter(person_id %in% ids)
      }
    }

    if (input$selectedTab == "mortality") {
      if (!is.null(input$anyDiagnosisBefore) && length(input$anyDiagnosisBefore) > 0) {
        ids <- cohort_diagnoses %>%
          filter(condition_start_date < cohort_start_date,
                 icd10_code %in% input$anyDiagnosisBefore) %>%
          pull(person_id)
        data <- data %>% filter( person_id%in% ids)
      }
    }

    return(data)
  })

  age_stats <- reactive({
    data <- filtered_cohort() %>%
      filter(!is.na(age))

    mean_age <- mean(data$age)
    n <- nrow(data)
    se <- sd(data$age) / sqrt(n)
    ci_lower <- mean_age - 1.96 * se
    ci_upper <- mean_age + 1.96 * se
    median_age <- median(data$age)

    return(list(
      mean_age = mean_age,
      median_age = median_age,
      ci_upper = ci_upper,
      ci_lower = ci_lower
    ))
  })



  filtered_diagnosis_source <- reactive({
    df <- cohort_diagnosis_source %>%
      select(subject_id, icd10_code, condition_start_date, diagnosis_source) %>%
      inner_join(
        filtered_cohort() %>% select(person_id, cohort_start_date, age, gender_concept_id),
        by = c("subject_id" = "person_id")
      ) %>%
      filter(condition_start_date == cohort_start_date)

    return(df)
  })


  filtered_claim_source <- reactive({
    emo_ids <- emo_visits_data %>%
      distinct(visit_occurrence_id) %>%
      pull(visit_occurrence_id)
    data <- cohort_claim_source %>%
      inner_join(filtered_cohort(), by = "person_id") %>%
      mutate(
        visit_source_type = case_when(
          visit_occurrence_id %in% emo_ids ~ "Emergency room",
          visit_source_type == "Outpatient specialist care" ~ "Outpatient specialist care",
          visit_source_type == "Inpatient specialist care" ~ "Inpatient specialist care",
          visit_source_type == "Inpatient nursing care" ~ "Inpatient nursing care",
          visit_source_type == "Family doctor" ~ "Family doctor",
          TRUE ~ "Other"
        )
      )
    if (!is.null(input$primaryDiagnosis)) {
      data <- data %>% filter(icd10_code %in% input$primaryDiagnosis)
    }
    data <- data %>% distinct(visit_occurrence_id, .keep_all = TRUE)
    data
  })

  filtered_comorbidities <- reactive({
    df <- comorbid_diagnoses
    time_range <- input$timeFilter
    if (is.null(time_range) || length(time_range) != 2) {
      time_range <- c(-12, 0)
    }

    df <- df %>%
      mutate(months_from_cohort = as.integer(( condition_start_date- cohort_start_date ) / 30)) %>%
      filter(months_from_cohort >= time_range[1], months_from_cohort <= time_range[2])

    # Age filter

    age_range <- input$ageFilter
    if (is.null(age_range) || length(age_range) != 2) {
      age_range <- c(0, 120)
    }

    df <- df %>% filter(age >= age_range[1], age <= age_range[2])
    # Gender filter
    df <- df %>% filter(
      input$gender_display == "combined" |
        (input$gender_display == "male" & gender_concept_id == 8507) |
        (input$gender_display == "female" & gender_concept_id == 8532)
    )
    total <- n_distinct(df$person_id)

    df <- df %>%
      mutate(icd10_group = if (input$icd_grouping) substr(icd10_code, 1, 3) else icd10_code)

    if (!is.null(input$primaryDiagnosis_comorbid) && length(input$primaryDiagnosis_comorbid) > 0) {
      primary <- if (input$icd_grouping) substr(input$primaryDiagnosis_comorbid, 1, 3) else input$primaryDiagnosis_comorbid
      df <- df %>% filter(!(icd10_group %in% primary))
    }

    if (is.null(input$comorbidDiagnoses1y) || length(input$comorbidDiagnoses1y) == 0) {
      # Top 10
      df %>%
        distinct(person_id, icd10_group) %>%
        count(icd10_group, name = "persons") %>%
        mutate(percent = round(persons / total * 100, 1)) %>%
        arrange(desc(persons)) %>%
        slice_head(n = 10)
    } else {
      # Only chosen codes
      df %>%
        filter(icd10_group %in% input$comorbidDiagnoses1y) %>%
        distinct(person_id, icd10_group) %>%
        count(icd10_group, name = "persons") %>%
        mutate(percent = round(persons / total * 100, 1)) %>%
        arrange(desc(persons))
    }
  })


  # Reactive fitering
  filtered_visits <- reactive({
    time_range <- input$timeFilter
    if (is.null(time_range) || length(time_range) != 2) {
      time_range <- c(-12, 12)
    }


    emo_ids <- emo_visits_data %>%
      distinct(visit_occurrence_id) %>%
      pull(visit_occurrence_id)

    df <- provider_visits_data %>%
      inner_join(filtered_cohort(), by = "person_id") %>%
      mutate(
        visit_type = case_when(
          visit_occurrence_id %in% emo_ids ~ "Emergency room",
          visit_type == "Outpatient specialist care" ~ "Outpatient specialist care",
          visit_type == "Inpatient specialist care" ~ "Inpatient specialist care",
          visit_type == "Inpatient nursing care" ~ "Inpatient nursing care",
          visit_type == "Family doctor" ~ "Family doctor",
          TRUE ~ "Other"
        )
      )
    df <- df %>%
      mutate(months_from_cohort = as.integer(days_from_cohort / 30)) %>%
      filter(
        months_from_cohort >= time_range[1],
        months_from_cohort <= time_range[2]
      )

    return(df)
  })



  visits_plot_data <- reactive({
    df <- filtered_visits()
    # diagnose filter
    if (!is.null(input$visitDiagnoses)) {
      df <- df %>%
        filter(icd10_code %in% input$visitDiagnoses)
    }
    total_subjects <- length(unique(df$person_id))

    # unique visits
    df_unique <- df %>% distinct(person_id, visit_occurrence_id, visit_type)

    # visits per person
    df_summary <- df_unique %>%
      group_by(person_id, visit_type) %>%
      summarise(n_visits = n_distinct(visit_occurrence_id), .groups = "drop")

    # summaries per visit_type
    df_summary %>%
      group_by(visit_type) %>%
      summarise(
        persons = n_distinct(person_id),
        visits = sum(n_visits),
        max_visits = as.numeric(max(n_visits)),
        min_visits = as.numeric(min(n_visits)),
        median_visits = as.numeric(median(n_visits)),
        mean_visits = round(mean(n_visits), 1),
        .groups = "drop"
      ) %>%
      mutate(
        percent_of_cohort = round((persons / total_subjects) * 100, 1)
      )
  })


    filtered_death <- reactive({
      df <- death_data
      age_range <- input$ageFilter
      if (is.null(age_range) || length(age_range) != 2) {
        age_range <- c(0, 120) #default
      }

      time_range <- input$death_month_range
      if (is.null(time_range) || length(time_range) != 2) {
        time_range <- c(0, 48)
      }


      df <- df %>%
        distinct(subject_id, .keep_all = TRUE) %>%
        filter(
          age >= age_range[1],
          age <= age_range[2]
        ) %>%
        mutate(
          gender = case_when(
            gender_concept_id == 8507 ~ "Male",
            gender_concept_id == 8532 ~ "Female",
            TRUE ~ "Other"
          ),
          months_since_cohort = as.integer((death_date - cohort_start_date) / 30)
        ) %>%
        filter(
          months_since_cohort >= time_range[1],
          months_since_cohort <= time_range[2]
        )
      if (!is.null(input$anyDiagnosisBefore) && length(input$anyDiagnosisBefore) > 0) {
        ids <- cohort_diagnoses %>%
          filter(condition_start_date < cohort_start_date,
                 icd10_code %in% input$anyDiagnosisBefore) %>%
          pull(person_id)
        df <- df %>% filter( subject_id%in% ids)
      }


      df <- df %>%
        filter(
          input$gender_display == "both" |
            input$gender_display == "combined" |
            (input$gender_display == "male" & gender == "Male") |
            (input$gender_display == "female" & gender == "Female")
        )

      return(df)
    })




  ##########################
  #                        #
  #   First page outputs   #
  #                        #
  ##########################
  output$cohortCountBox <- renderValueBox({
    valueBox(
      value = nrow(cohort_data),
      subtitle = "All people count in cohort",
      icon = icon("users"),
      color = "light-blue"
    )
  })

  output$filteredCountBox <- renderValueBox({
    valueBox(

      value = ifelse(nrow(filtered_cohort()) < 5, NA, nrow(filtered_cohort())),
      subtitle = "People count after filtering",
      icon = icon("filter"),
      color = "light-blue"
    )
  })

  output$genderBox<- renderValueBox({
    male <- filtered_cohort() %>% filter(gender == "Male") %>% nrow()
    female <- filtered_cohort() %>% filter(gender == "Female") %>% nrow()
    valueBox(
      value = paste0(ifelse(male < 5, NA, male), " / ", ifelse(female < 5, NA, female)),
      subtitle = "Males count / Females count",
      icon = icon("filter"),
      color = "light-blue"
    )
  })

  output$genderPlot <- renderPlotly({
    plotGenderDistribution(filtered_cohort())
  })

  output$cohortYearByGender <- renderPlotly({
    plotCohortStartYearByGender(
      filtered_cohort(),
      gender_option = input$gender_display
    )
  })


  output$meanAgeBox <- renderValueBox({
    valueBox(
      value = round(age_stats()$mean_age, 1),
      subtitle = "Mean age",
      icon = icon("user"),
      color = "light-blue"
    )
  })

  output$ageCIBox <- renderValueBox({
    valueBox(
      value =  paste0(round(age_stats()$ci_lower, 1), "–", round(age_stats()$ci_upper, 1)),
      subtitle = "95% confidence interval of mean age%",
      icon = icon("percent"),
      color = "light-blue"
    )
  })

  output$medianAgeBox <- renderValueBox({
    valueBox(
      value = round(age_stats()$median_age, 1),
      subtitle = "Median age",
      icon = icon("user"),
      color = "light-blue"
    )
  })
  output$ageGroupedPlot <- renderPlotly({
    plotAgeGrouped(
      filtered_cohort(),
      group_size = as.numeric(input$age_grouping),
      gender_option = input$gender_display
    )
  })


  ##########################
  #                        #
  #   Second page outputs  #
  #                        #
  ##########################

  output$plotDiagnosisVennHover <- renderPlotly({
    plotDiagnosisVennHover(filtered_diagnosis_source())
  })

  output$diagnosisSourceSimplePlot <- renderPlotly({
    plotDiagnosisSourceSimple(filtered_diagnosis_source())
  })

  output$claimSourcePlot <- renderPlotly({
    plotClaimSourceBreakdown(filtered_claim_source())
  })


  ##########################
  #                        #
  #   Third page outputs   #
  #                        #
  ##########################

  output$comorbidityPlot <- renderPlotly({
    plot_comorbid_distribution(filtered_comorbidities())
  })

  ###########################
  #                         #
  #   Fourth page outputs   #
  #                         #
  ###########################

  observe({
    emo_ids <- emo_visits_data %>%
      distinct(visit_occurrence_id) %>%
      pull(visit_occurrence_id)

    df_all <- provider_visits_data %>%
      left_join(filtered_cohort(), by = "person_id") %>%
      mutate(
        visit_type = case_when(
          visit_occurrence_id %in% emo_ids ~ "Emergency room",
          visit_type == "Outpatient specialist care" ~ "Outpatient specialist care",
          visit_type == "Inpatient specialist care" ~ "Inpatient specialist care",
          visit_type == "Inpatient nursing care" ~ "Inpatient nursing care",
          visit_type == "Family doctor" ~ "Family doctor",
          TRUE ~ "Other"
        )
      )

    total_persons <- n_distinct(df_all$person_id)
    total_visits <- n_distinct(df_all$visit_occurrence_id)


    # Peale üldiseid filtreid
    df_filtered <- filtered_visits()
    filtered_persons <- n_distinct(df_filtered$person_id)
    filtered_visits <- n_distinct(df_filtered$visit_occurrence_id)

    diagnosis_filtered <- if (!is.null(input$visitDiagnoses)) {
      df_filtered  %>% filter(icd10_code %in% input$visitDiagnoses)
    } else {
      df_filtered
    }

    diagnosis_filtered_persons <- n_distinct(diagnosis_filtered$person_id)
    diagnosis_filtered_visits <- n_distinct(diagnosis_filtered$visit_occurrence_id)

    # ValueBoxid

    output$box_total_persons <- renderValueBox({
      val <- total_persons
      valueBox(ifelse(val < 5, NA, val), "All people", icon = icon("users"), color = "light-blue")
    })

    output$box_total_visits <- renderValueBox({
      val <- total_visits
      valueBox(ifelse(val < 5, NA, val), "All visits", icon = icon("hospital"), color = "light-blue")
    })

    output$box_filtered_persons <- renderValueBox({
      val <- filtered_persons
      valueBox(ifelse(val < 5, NA, val), "People after filters", icon = icon("user-check"), color = "light-blue")
    })

    output$box_filtered_visits <- renderValueBox({
      val <- filtered_visits
      valueBox(ifelse(val < 5, NA, val), "Visits after filters", icon = icon("clipboard-check"), color = "light-blue")
    })

    output$box_diag_persons <- renderValueBox({
      val <- diagnosis_filtered_persons
      valueBox(ifelse(val < 5, NA, val), "People after filters with chosen diagnosis", icon = icon("user-md"), color = "light-blue")
    })

    output$box_diag_visits <- renderValueBox({
      val <- diagnosis_filtered_visits
      valueBox(ifelse(val < 5, NA, val), "Visits after filters with chosen diagnosis", icon = icon("file-medical"), color = "light-blue")
    })
  })


  output$visitPlot <- renderPlotly({
    plot_visit_distribution(visits_plot_data())
  })
  output$visit_distribution_table <- DT::renderDataTable({
    table_data <- create_visit_distribution_table(visits_plot_data())

    DT::datatable(
      table_data,
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        pageLength = 10,
        autoWidth = TRUE,
        scrollX = TRUE,
        searching = FALSE
      ),
      class = 'stripe hover',
      rownames = FALSE
    )
  })

  ##########################
  #                        #
  #   Fifth page outputs   #
  #                        #
  ##########################

deathStats <- reactive({
  total_deaths <- death_data %>%
    distinct(subject_id) %>%
    nrow()

  filtered <- filtered_death()
  filtered_deaths <- nrow(filtered)
  return(list(total_deaths = total_deaths, filtered_deaths = filtered_deaths))
})

output$cohortCountBoxDeath <- renderValueBox({
  valueBox(
    value = nrow(cohort_data),
    subtitle = "All people count in cohort",
    icon = icon("users"),
    color = "light-blue"
  )
})

output$filteredCountBoxDeath <- renderValueBox({
  count <- nrow(filtered_cohort())

  if (count < 5) {
    valueBox(
      value = NA,
      subtitle = "Less than 5 person",
      icon = icon("filter"),
      color = "light-blue"
    )
  } else {
    valueBox(
      value = count,
      subtitle = "People count after filtering",
      icon = icon("filter"),
      color = "light-blue"
    )
  }
})

output$deathCountBox <- renderValueBox({
  valueBox(
    value = deathStats()$total_deaths,
    subtitle = "All deaths in cohort",
    icon = icon("users"),
    color = "light-blue"
  )
})

output$filteredDeathCountBox <- renderValueBox({
  count <- deathStats()$filtered_deaths

  if (count < 5) {
    valueBox(
      value = NA,
      subtitle = "Less than 5 person",
      icon = icon("filter"),
      color = "light-blue"
    )
  } else {
    valueBox(
      value = count,
      subtitle = "Death count after filtering",
      icon = icon("filter"),
      color = "light-blue"
    )
  }
})

output$filteredDeathPercentBox <- renderValueBox({
  count <- deathStats()$filtered_deaths
  if (count < 5) {
    valueBox(
      value = NA,
      subtitle = "Less than 5 person",
      icon = icon("filter"),
      color = "light-blue"
    )
  } else {
  valueBox(
    value = round((deathStats()$filtered_deaths / nrow(filtered_cohort()) *100),1),
    subtitle = "Death percent of filtered cohort",
    icon = icon("percent"),
    color = "light-blue"
    )}
})

output$allDeathPercentBox <- renderValueBox({
  valueBox(
    value = round((deathStats()$total_deaths / nrow(cohort_data) *100), 1),
    subtitle = "Death percent of all the people in cohort",
    icon = icon("percent"),
    color = "light-blue"
  )
})

output$deathPlot <- renderPlotly({
  plotDeathByMonth(
    data = filtered_death(),
    gender_option = input$gender_display,
    months_since_cohort = input$death_month_range,
    cohort_data_filtered = filtered_cohort()
  )
})}
