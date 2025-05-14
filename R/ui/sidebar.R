#####################################
#
#
#
# UI sidebars for every tab in Shiny
#
#
#
#####################################

sidebar <- dashboardSidebar(
  #styles
  tags$style(type = 'text/css',
    '.skin-blue .main-sidebar {
      background-color: #343a40 !important;
        color: white;
    }',
    '.sidebar-padding {
      padding: 10px;
    }'
),

  width = 300,
  sidebarMenu(
    menuItem("Filters", tabName = "filters", icon = icon("sliders-h")),

    div(class = "sidebar-section sidebar-padding",

        # Tab 1 & 2
        conditionalPanel(
          condition = "input.selectedTab == 'cohort_analysis' || input.selectedTab == 'diagnoses'",
          h4("Cohort defining diagnoses",icon("info-circle", id = "info1"),
          bsTooltip("info1", "Choose cohort defining diagnoses. The options are all diagnoses on the cohort start date. Default all people.",
                    placement = "bottom", trigger = "hover")),
          pickerInput("primaryDiagnosis",
                      choices = character(0), multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `live-search` = TRUE,
                        `selected-text-format` = "count > 3"
                      )),

          h4("Comorbid diagnoses",icon("info-circle", id = "info2"),
             bsTooltip("info2", "Choose comorbid diagnoses. Options are any diagnosis before cohort start date.",
                       placement = "bottom", trigger = "hover")),
          pickerInput("comorbidDiagnoses",
                      choices = character(0), multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `live-search` = TRUE,
                        `selected-text-format` = "count > 3"
                      ))
        ),

        # Tab 3 sidebars
        conditionalPanel(
          condition = "input.selectedTab == 'comorbid_diagnoses'",
          h4("Cohort defining diagnoses",icon("info-circle", id = "comorbidInfo"),
             bsTooltip("comorbidInfo", "Choose cohort defining diagnoses. The options are not in the comorbid diagnosis plot.",
                       placement = "bottom", trigger = "hover")),
          pickerInput("primaryDiagnosis_comorbid",
                      choices = character(0), multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `live-search` = TRUE,
                        `selected-text-format` = "count > 3"
                      )),
          checkboxInput("icd_grouping", "Group by the first three characters", value = TRUE),
          h4("Comorbid diagnoses",icon("info-circle", id = "comorbid2"),
             bsTooltip("comorbid2", "Choose comorbid diagnoses. Options are any diagnosis one year before cohort start date .",
                       placement = "bottom", trigger = "hover")),
          pickerInput("comorbidDiagnoses1y",
                      choices = character(0), multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `live-search` = TRUE,
                        `selected-text-format` = "count > 3"
                      ))
        ),

        # Tab 4 sidebars
        conditionalPanel(
          condition = "input.selectedTab == 'visits'",

          h4("Comorbid diagnoses",icon("info-circle", id = "visits1"),
             bsTooltip("visits1", "Choose comorbid diagnoses. Options are any diagnosis before cohort start date .",
                       placement = "bottom", trigger = "hover")),
          pickerInput("visitcomorbidDiagnoses",
                      choices = character(0),
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `live-search` = TRUE,
                        `selected-text-format` = "count > 3"
                      )
          ),

          h4("Visit-related diagnoses",icon("info-circle", id = "visits2"),
             bsTooltip("visits2", "Choose diagnoses to show only visits with diagnoses codes.",
                       placement = "bottom", trigger = "hover")),
          pickerInput("visitDiagnoses",
                      choices = character(0), multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `live-search` = TRUE,
                        `selected-text-format` = "count > 3"
                      ))
        ),

        # Tab 5
        conditionalPanel(
          condition = "input.selectedTab == 'mortality'",
          h4("Comorbid diagnoses",icon("info-circle", id = "deathinfo"),
             bsTooltip("deathinfo", "Choose comorbid diagnoses. Options are any diagnosis before cohort start date .",
                       placement = "bottom", trigger = "hover")),
          pickerInput("anyDiagnosisBefore",
                      choices = character(0),
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `live-search` = TRUE,
                        `selected-text-format` = "count > 3"
                      )
          )
        )

    ),
    #Tab 1
    div(class = "sidebar-section sidebar-padding" ,
        conditionalPanel(
        condition = "input.selectedTab == 'cohort_analysis'",
        h4("Grouping age by:"),
        radioButtons(
          inputId = "age_grouping",
          label = NULL,
          choices = c("1 year" = 1, "5 years" = 5, "10 years" = 10),
          selected = 1
        )),
        h4("Choose age:"),
        uiOutput("ageFilter")
    ),
    # Tab 1 & 5
    div(class = "sidebar-section sidebar-padding",
        h4("Choose gender:"),
        conditionalPanel(
        condition = "input.selectedTab == 'cohort_analysis'|| input.selectedTab == 'mortality'",
        selectInput(
          inputId = "gender_display",
          label = NULL,
          choices = c(
            "Females and males (separate)" = "both",
            "Only males" = "male",
            "Only females" = "female",
            "Females and males " = "combined"
          ),
          selected = "combined"
        )
    ),
    #Tab 2 & 3 & 4
    conditionalPanel(
      condition = "input.selectedTab != 'cohort_analysis'&& input.selectedTab != 'mortality'",
      selectInput(
        inputId = "gender_display",
        label = NULL,
        choices = c(
          "Only males" = "male",
          "Only females" = "female",
          "Females and males " = "combined"
        ),
        selected = "combined"
      )
    )),
    #Tab 4
    div(class = "sidebar-section sidebar-padding",
    conditionalPanel(
      condition = "input.selectedTab == 'visits'",
        h4("Time before and after cohort start:"),

    ),
    #Tab 3
    conditionalPanel(
      condition = "input.selectedTab == 'comorbid_diagnoses'",
          h4("Time before cohort start:")
      ),
    # Tab 5
    conditionalPanel(
      condition = "input.selectedTab == 'mortality'",
          h4("Time after cohort start:")
      ),
    uiOutput("dynamicTimeSlider"))
  )
)
