
#' Gender plot logic
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
plotGenderDistribution <- function(data) {
  gender_df <- data %>%
    count(gender, name = "n") %>%
    filter(n > 5)

  if (nrow(gender_df) == 0) {
    return(plotly::plot_ly(
      type = "pie",
      labels = "No data",
      values = 1,
      textinfo = "label",
      hole = 0.6
    ))
  }
  gender_df <- gender_df %>%
    mutate(
      total = sum(n),
      percent = n / total * 100,
      label_text = paste0(
        gender, "\n",
        "People: ", n, "\n",
        "Proportion: ", round(percent, 1), "%"
      )
    )
  gender_colors <- c("Male" = "#0073B7", "Female" = "#FFE9CE")

  plotly::plot_ly(
    gender_df,
    labels = ~gender,
    values = ~n,
    type = "pie",
    text = ~label_text,
    textposition = "outside",
    textinfo = "percent",
    hoverinfo = "text",
    hole = 0.6,
    marker = list(colors = gender_colors[gender_df$gender])
  ) %>%
    layout(
      title = list(
        text = "Gender distribution",
        x = 0,
        y = 1,
        xanchor = "left",
        font = list(size = 25)
      ),
      showlegend =  TRUE
    )
}


#
#' Age plot logic
#'
#' @param data
#' @param group_size
#' @param gender_option
#'
#' @return
#' @export
#'
#' @examples
plotAgeGrouped <- function(data, group_size, gender_option = "combined") {
  if (nrow(data) < 5) {
    return(ggplotly(
      ggplot() +
        annotate("text", x = 1, y = 1, label = "Not enough people n<5", size = 6) +
        theme_void()
    ))
  }
  if (gender_option == "female") {
    data <- data %>% filter(gender == "Female")
  } else if (gender_option == "male") {
    data <- data %>% filter(gender == "Male")
  }

  data <- data %>%
    mutate(age_group = if (group_size == 1) {
      as.character(age)
    } else {
      paste0(floor(age / group_size) * group_size, "-",
             floor(age / group_size) * group_size + group_size - 1)
    })

  subtitle_text <- dplyr::case_when(
    gender_option == "female" ~ "(females)",
    gender_option == "male" ~ "(males)",
    gender_option == "combined" ~ "(males/females)",
    gender_option == "both" ~ "(males/females)"
  )
  full_title <- paste0("Age distribution ", subtitle_text)



  if (gender_option %in% c("combined", "female", "male")) {
    df <- data %>%
      count(age_group, name = "n") %>%
      filter(n >= 5) %>%
      mutate(
        percent = n / sum(n) * 100,
        ci = map2(n, sum(n), ~ suppressWarnings(prop.test(.x, .y, correct = FALSE)$conf.int * 100)),
        ci_lower = map_dbl(ci, function(x) x[1]),
        ci_upper = map_dbl(ci, function(x) x[2]),
        label = paste0(
          "Age: ", age_group,
          "<br>N: ", n,
          "<br>Percent: ", round(percent, 1), "%",
          "<br>95% CI: [", round(ci_lower, 1), "% – ", round(ci_upper, 1), "%]"
        )
      )


    p <- ggplot(df, aes(x = age_group, y = percent, text = label)) +
      geom_col(fill = "#0073B7", alpha = 0.8) +
      geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = "black") +
      labs(
        title = full_title,
        x = "Age",
        y = "Percent (%)"
      ) +
      theme_minimal()+
      theme(plot.title = element_text(size = 20))

  }

  else {
    # Genders separately
    df <- data %>%
      count(age_group, gender, name = "n") %>%
      group_by(age_group) %>%
      mutate(
        percent = n / sum(n) * 100,
        ci = map2(n, sum(n), ~ suppressWarnings(prop.test(.x, .y, correct = FALSE)$conf.int * 100)),
        ci_lower = map_dbl(ci, function(x) x[1]),
        ci_upper = map_dbl(ci, function(x) x[2]),
        label = paste0(
          "Age: ", age_group,
          "<br>Gender: ", gender,
          "<br>N: ", n,
          "<br>Percent: ", round(percent, 1), "%",
          "<br>95% CI: [", round(ci_lower, 1), "% – ", round(ci_upper, 1), "%]"
        )
      ) %>%
      filter(n >= 5) %>%
      ungroup()

    p <- ggplot(df, aes(x = age_group, y = percent, fill = gender, text = label)) +
      geom_col(position = "stack", alpha = 0.9) +
      scale_fill_manual(values = c("Male" = "#0073B7", "Female" = "#FFE9CE")) +
      labs(
        title = full_title,
        x = "Age",
        y = "Percent (%)",
        fill = "Gender",
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      theme(plot.title = element_text(size = 20))

  }

  ggplotly(p, tooltip = "text")
}


#' Cohort start yearly distribution plot logic
#'
#' @param data
#' @param gender_option
#'
#' @return
#' @export
#'
#' @examples
plotCohortStartYearByGender <- function(data, gender_option = "combined") {

  if (nrow(data) < 5) {
    return(ggplotly(
      ggplot() +
        annotate("text", x = 1, y = 1, label = "Not enough people n<5", size = 6) +
        theme_void()
    ))
  }

  data <- data %>%
    mutate(cohort_year = lubridate::year(cohort_start_date))

  subtitle_text <- dplyr::case_when(
    gender_option == "female" ~ "(females)",
    gender_option == "male" ~ "(males)",
    gender_option == "combined" ~ "(males/females)",
    gender_option == "both" ~ "(males/females)"
  )
  full_title <- paste0("Cohort start year distribution\n", subtitle_text)


  if (gender_option == "female") {
    data <- data %>% filter(gender == "Female")
  } else if (gender_option == "male") {
    data <- data %>% filter(gender == "Male")
  }

  # Combined view (single color bar)
  if (gender_option %in% c("combined", "female", "male")) {
    df <- data %>%
      count(cohort_year, name = "n") %>%
      filter(n >= 5) %>%
      mutate(
        percent = n / sum(n) * 100,
        label = paste0(
          "Year: ", cohort_year,
          "<br>N: ", n,
          "<br>Percent: ", round(percent, 1), "%"
        )
      )


    p <- ggplot(df, aes(x = as.factor(cohort_year), y = percent, text = label)) +
      geom_col(fill = "#0073B7", alpha = 0.8) +
      labs(
        title = full_title,
        x = "Year",
        y = "Percent (%)"
      ) +
      theme_minimal()+
      theme(plot.title = element_text(size = 20))

  } else {

    # Stacked gender view
    df <- data %>%
      count(cohort_year, gender, name = "n") %>%
      group_by(cohort_year) %>%
      mutate(
        percent = n / sum(n) * 100,
        label = paste0(
          "Year: ", cohort_year,
          "<br>Gender: ", gender,
          "<br>N: ", n,
          "<br>Percent: ", round(percent, 1), "%"
        )
      ) %>%
      filter(n >= 5) %>%
      ungroup()

    p <- ggplot(df, aes(x = as.factor(cohort_year), y = percent, fill = gender, text = label)) +
      geom_col(position = "stack", alpha = 0.9) +
      scale_fill_manual(values = c("Male" = "#0073B7", "Female" = "#FFE9CE")) +
      labs(
        title = full_title,
        x = "Year",
        y = "Percent (%)",
        fill = "Gender"
      ) +
      theme_minimal()+
      theme(plot.title = element_text(size = 20))
  }

  ggplotly(p, tooltip = "text")
}

#' Diagnosis source plot logic
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
plotDiagnosisSourceSimple <- function(data) {
  cohort_size <- data %>%
    select(subject_id) %>%
    distinct() %>%
    nrow()

  df <- data %>%
    filter(condition_start_date == cohort_start_date) %>%
    select(subject_id, diagnosis_source) %>%
    distinct() %>%
    mutate(source = diagnosis_source) %>%
    count(source, name = "n") %>%
    mutate(percent = round(100 * n / cohort_size, 1)) %>%
    filter(n > 5)

  if (nrow(df) == 0) {
    return(ggplotly(
      ggplot() +
        annotate("text", x = 1, y = 1, label = "Not enough people n<=5", size = 6) +
        theme_void()
    ))
  }

  p <- ggplot(df, aes(x = reorder(source,  -n), y = n, fill = source,
                      text = paste0("N = ", n, "\nPercent from cohort = ", percent, "%"))) +
    geom_col() +
    scale_fill_manual(values = c("EHR" = "#4059AD", "Pharmacy" = "#deebf7", "Claim" = "#3182bd")) +
    labs(
      title = "Diagnosis sources",
      x = "Source",
      y = "Number of patients"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(size = 20))

  ggplotly(p, tooltip = "text")
}



#' Claim souce plot logic
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
plotClaimSourceBreakdown <- function(data) {
  plot_data <- data %>%
    count(visit_source_type) %>%
    filter(n > 5) %>%
    mutate(percent = round(100 * n / sum(n), 1),
           label = paste0(visit_source_type, "<br>N: ", n, "<br>", percent, "%"))


  if (nrow(plot_data) == 0) {
    return(ggplotly(
      ggplot() +
        annotate("text", x = 1, y = 1, label = "Not enough people n<5", size = 6) +
        theme_void()
    ))
  } else {
    p <- ggplot(plot_data, aes(x = reorder(visit_source_type, -n), y = n, fill = visit_source_type, text = label)) +
      geom_col() +
      labs(
        title = "Claim source breakdown",
        x = "Visit source type",
        y = "Number of visits"
      ) +
      scale_fill_manual(values = c(
        "Outpatient specialist care" = "#08519c",
        "Family doctor" = "#4059AD",
        "Inpatient specialist care" = "#deebf7",
        "Inpatient nursing care" = "#3182bd",
        "Other" = "gray",
        "Emergency room" = "#0073B7"
      ))+
      theme_minimal() +
      theme(legend.position = "none")+
      theme(plot.title = element_text(size = 20))

    ggplotly(p, tooltip = "text") %>%
      layout(
        title = list(text = ""),
        annotations = list(
          list(
            text = "Claim source breakdown",
            x = 0,
            y = 1.05,
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            align = "left",
            xanchor = "left",
            yanchor = "bottom",
            font = list(size = 20)
          )
        )
      )
  }
}


#' Venn diagram logic for diagnosis source breakdown
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
plotDiagnosisVennHover <- function(df) {


  make_circle <- function(center = c(0,0), r = 1.5, npoints = 100){
    theta <- seq(0, 2*pi, length.out = npoints)
    data.frame(
      x = center[1] + r * cos(theta),
      y = center[2] + r * sin(theta)
    )
  }

  if (is.null(df) || nrow(df) == 0) {
    df <- data.frame(subject_id = character(), diagnosis_source = character())
  }

  df <- df %>%
    mutate(
      EHR = grepl("EHR", diagnosis_source, ignore.case = TRUE),
      Claim = grepl("Claim", diagnosis_source, ignore.case = TRUE),
      Pharmacy = grepl("Pharmacy", diagnosis_source, ignore.case = TRUE)
    )

  venn_input <- list(
    EHR = unique(df$subject_id[df$EHR]),
    Claim = unique(df$subject_id[df$Claim]),
    Pharmacy = unique(df$subject_id[df$Pharmacy])
  )

  only_ehr <- setdiff(venn_input$EHR, union(venn_input$Claim, venn_input$Pharmacy))
  only_claim <- setdiff(venn_input$Claim, union(venn_input$EHR, venn_input$Pharmacy))
  only_pharmacy <- setdiff(venn_input$Pharmacy, union(venn_input$EHR, venn_input$Claim))
  ehr_claim <- setdiff(intersect(venn_input$EHR, venn_input$Claim), venn_input$Pharmacy)
  ehr_pharmacy <- setdiff(intersect(venn_input$EHR, venn_input$Pharmacy), venn_input$Claim)
  claim_pharmacy <- setdiff(intersect(venn_input$Claim, venn_input$Pharmacy), venn_input$EHR)
  all_three <- Reduce(intersect, venn_input)

  label_data <- data.frame(
    category = c(
      "EHR", "Claim", "Pharmacy",
      "EHR+Claim", "EHR+Pharmacy", "Claim+Pharmacy",
      "EHR+Claim+Pharmacy"
    ),
    count = c(
      length(only_ehr),
      length(only_claim),
      length(only_pharmacy),
      length(ehr_claim),
      length(ehr_pharmacy),
      length(claim_pharmacy),
      length(all_three)
    ),
    x = c(-1, 1, 0, 0, -0.8, 0.8, 0),
    y = c(1, 1, -0.8, 0.5, -0.2, -0.2, 0)
  ) %>%
    mutate(hover = ifelse(
      count < 5,
      paste0("Category: ", category, "<br>Not enough people (n<5)"),
      paste0("Category: ", category, "<br>N: ", count)
    ))


  circles_df <- rbind(
    make_circle(center = c(-1, 1), r = 1.5) %>% mutate(group = "EHR"),
    make_circle(center = c(1, 1), r = 1.5) %>% mutate(group = "Claim"),
    make_circle(center = c(0, -0.8), r = 1.5) %>% mutate(group = "Pharmacy")
  )


  p <- ggplot() +
    geom_polygon(data = circles_df, aes(x = x, y = y, group = group, fill = group),
                 alpha = 0.8, color = "black", linewidth = 0.1) +
    geom_point(data = label_data, aes(x = x, y = y, text = hover), color = "transparent", size = 20) +
    geom_text(data = data.frame(
      x = c(-2, 2, 0),
      y = c(2.5, 2.5, -2.5),
      label = c("EHR", "Claim", "Pharmacy")
    ), aes(x = x, y = y, label = label), size = 6, fontface = "bold") +
    scale_fill_manual(values = c("EHR" = "#4059AD", "Pharmacy" = "#deebf7","Claim" = "#3182bd")) +
    theme_void() +
    theme(
      legend.position = "none",
      plot.background = element_rect(fill = "white", color = NA)
    )


  ggplotly(p, tooltip = "hover")%>%
    layout(
      hoverlabel = list(bgcolor = "white"),
      paper_bgcolor = 'white',
      plot_bgcolor = 'white',
      title = list(
        text = "Diagnosis sources overlap",
        y = 0.99,
        x = 0,
        xanchor = "left",
        font = list(size = 27)
      ),
      margin = list(t = 50, r = 20, b = 20, l = 20),
      xaxis = list(
        range = c(-4, 4),
        showgrid = FALSE,
        zeroline = FALSE,
        visible = FALSE
      ),
      yaxis = list(
        scaleanchor = "x",
        range = c(-4, 4),
        showgrid = FALSE,
        zeroline = FALSE,
        visible = FALSE
      )
    )
}


#' Comorbid diagnoses plot logic
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
plot_comorbid_distribution <- function(data) {
  data_filtered <- data %>% filter(persons >= 5)

  if (nrow(data_filtered) == 0) {
    return(plotly::plot_ly() %>%
             plotly::layout(
               title = list(
                 text = "Comorbid conditions distribution",
                 x = 0,
                 xanchor = "left"
               ),
               annotations = list(
                 list(
                   text = "Not enough people n<5",
                   x = 0.5,
                   y = 0.5,
                   showarrow = FALSE,
                   font = list(size = 16)
                 )
               ),
               xaxis = list(showticklabels = FALSE, zeroline = FALSE),
               yaxis = list(showticklabels = FALSE, zeroline = FALSE)
             ))
  }

  plot_height <- max(600, nrow(data_filtered) * 20)

  plot_ly(
    data = data_filtered,
    x = ~persons,
    y = ~reorder(icd10_group, persons),
    type = "bar",
    orientation = "h",
    marker = list(color = ~persons, colorscale = list(
      c(0, "#deebf7"),
      c(0.5, "#3182bd"),
      c(1, "#003366")
    )),  # or your preferred scale
    text = ~paste0(
      "ICD-10: ", icd10_group, "<br>",
      "People: ", persons, "<br>",
      "% of cohort: ", percent, "%"
    ),
    hoverinfo = "text",
    textposition = "none"
  ) %>% layout(
    title = list(
      text = "Comorbid conditions distribution",
      x = 0,
      y=1,
      xanchor = "left",
      font = list(size = 25)
    ),
    xaxis = list(title = "People count"),
    yaxis = list(title = "ICD-10 code", showticklabels = TRUE),
    margin = list(l = 150),
    height = plot_height
  )
}



#' Visits plot function
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
plot_visit_distribution <- function(data) {

  filtered_data <- data %>% filter(!is.na(persons) & persons > 5)
  if (nrow(filtered_data) == 0) {
    return(plotly::plot_ly() %>%
             plotly::layout(
               title = list(text = "Not enough people n<5", x = 0.5)
             ))
  }

  plotly::plot_ly(
    data = filtered_data,
    x = ~visit_type,
    y = ~persons,
    type = "bar",
    color = ~visits,
    colors = colorRampPalette(c("#deebf7", "#08519c"))(100),
    text = ~paste0(
      "Type: ", visit_type, "<br>",
      "People: ", persons, "<br>",
      "Visits: ", visits, "<br>",
      "Average per person: ", mean_visits, "<br>",
      "Median: ", median_visits, "<br>",
      "Max: ", max_visits, "<br>",
      "Min: ", min_visits, "<br>",
      "Percent from cohort: ", percent_of_cohort, "%"
    ),
    hoverinfo = "text",
    textposition = "none"
  ) %>% plotly::layout(
    yaxis = list(title = "People count"),
    xaxis = list(title = "Visit type"),
    title = list(
      text = "Visits distribution (from claim)",
      x = 0,
      y = 1,
      xanchor = "left",
      font = list(size = 25)
    )
  )
}

create_visit_distribution_table <- function(data) {
  filtered_data <- data %>%
    dplyr::filter(!is.na(persons) & persons > 5)

  visit_table <- filtered_data %>%
    dplyr::select(
      `Visit type` = visit_type,
      `People` = persons,
      `Visits` = visits,
      `Average per person` = mean_visits,
      `Median` = median_visits,
      `Max` = max_visits,
      `Min` = min_visits,
      `Percent from cohort (%)` = percent_of_cohort
    )

  return(visit_table)
}



#' Death plot function
#'
#' @param data
#' @param gender_option
#' @param months_since_cohort
#' @param cohort_data_filtered
#'
#' @return
#' @export
#'
#' @examples
plotDeathByMonth <- function(data, gender_option = "combined", months_since_cohort, cohort_data_filtered) {
  if (nrow(data) < 5) {
    p <- ggplot() +
      annotate("text", x = 1, y = 1, label = "Not enough people n<5", size = 5, color = "black") +
      theme_void()
    return(ggplotly(p))
  }
  cohort_size <- nrow(cohort_data_filtered)

  if (gender_option %in% c("combined", "male", "female")) {
    agg_all <- data %>%
      count(months_since_cohort, name = "n") %>%
      arrange(months_since_cohort) %>%
      mutate(
        percent = round(n / cohort_size * 100, 1),
        cum_n = cumsum(n),
        cum_percent = round(cum_n / cohort_size * 100, 1),
        label = paste0(
          "Month: ", months_since_cohort, "<br>",
          "Percent: ", percent, " %<br>",
          "Cumulative: ", cum_percent, " %<br>",
          "Count: ", n
        )
      )


    agg <- agg_all %>% filter(n > 5)

    if (nrow(agg) == 0) {
      p <- ggplot() +
        annotate("text", x = 1, y = 1, label = "Not enough people n<5", size = 5, color = "black") +
        theme_void()
      return(ggplotly(p))
    }

    p <- ggplot(agg, aes(x = factor(months_since_cohort), y = percent, text = label, group = 1)) +
      geom_col(fill = "#08519c", alpha = 0.8) +
      labs(
        title = "Deaths after cohort start",
        x = "Month after",
        y = "Deaths percent (%)"
      ) +
      theme_minimal()+
      theme(plot.title = element_text(size = 20))

    return(ggplotly(p, tooltip = "text"))

  } else {
    df <- data %>%
      count(months_since_cohort, gender, name = "n") %>%
      mutate(original_n = n) %>%
      filter(n > 5)

    if (nrow(df) == 0) {
      p <- ggplot() +
        annotate("text", x = 1, y = 1, label = "Not enough people n<5", size = 5, color = "black") +
        theme_void()
      return(ggplotly(p))
    }

    total <- df %>%
      group_by(months_since_cohort) %>%
      summarise(total = sum(n), .groups = "drop")

    df <- df %>%
      left_join(total, by = "months_since_cohort") %>%
      mutate(
        percent = n / total * 100,
        label = paste0(
          "Month: ", months_since_cohort,
          "<br>Gender: ", gender,
          "<br>Percent: ", round(percent, 1), "%",
          "<br>Count: ", n
        )
      )

    p <- ggplot(df, aes(x = months_since_cohort, y = percent, fill = gender, text = label)) +
      geom_col(position = "stack", alpha = 0.9) +
      scale_fill_manual(values = c("Male" = "#08519c", "Female" = "#FFE9CE")) +
      labs(
        title = "Deaths after cohort start",
        x = "Month",
        y = "Percent (%)",
        fill = "Gender"
      ) +
      theme_minimal()+
      theme(plot.title = element_text(size = 20))

    return(ggplotly(p, tooltip = "text"))
  }
}



