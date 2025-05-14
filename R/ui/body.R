##################################
#
#
#
# UI body for every tab in Shiny
#
#
#
##################################

body <- dashboardBody(
  #
  #styles
  #
  tags$style(type = 'text/css',

             '.navbar-default .navbar-brand{color: white !important;}',
             '.content {background-color: white !important;}'
  ),
  tags$style(HTML("
    .dataTables_wrapper {
      width: 100% !important;
      margin: 0 !important;
      text-align: left !important;
    }
    table.dataTable {
      margin: 0 !important;
    }
    table.dataTable thead th,
    table.dataTable tbody td {
      text-align: left !important;
      font-size: 14px;
    }
  "),
  tags$head(
    tags$style(HTML("
        .nav-tabs > li > a {
          font-size: 18px;
        }
      ")))),
  #
  # Tab 1
  #
  tabsetPanel(id = "selectedTab",
              tabPanel(title = tagList(
                "Demographic Summary",
                tags$span("\u2139", title = "If certain results are not displayed, it is due to data protection principles—values representing five or fewer individuals are not shown to ensure privacy.", style = "cursor: help; margin-left: 5px;")
              ), value = "cohort_analysis",
                       br(),
                       br(),
                       fluidRow(
                         valueBoxOutput("cohortCountBox"),
                         valueBoxOutput("filteredCountBox"),
                       valueBoxOutput("genderBox")),
                       br(),
                       br(),
                       br(),
                        fluidRow(
                         column(width = 6,
                                withSpinner(plotlyOutput("genderPlot", height = "500px"), color = "#add8e6")
                         ),
                         column(width = 6,
                                withSpinner(plotlyOutput("cohortYearByGender", height = "500px"), color = "#add8e6")
                         )),
                       br(),
                       br(),

                       fluidRow(
                         valueBoxOutput("meanAgeBox"),
                         valueBoxOutput("ageCIBox"),
                         valueBoxOutput("medianAgeBox")
                       ),
                       br(),
                       br(),
                       withSpinner(plotlyOutput("ageGroupedPlot", height = "500px"),color = "#add8e6")

              ),
              #
              #Tab 2
              #
              tabPanel(title = tagList(
                "Diagnosis sources",
                tags$span("\u2139", title = "In the first chart (Diagnosis Sources), individuals may appear more than once if they have diagnoses from multiple sources. The adjacent Venn diagram shows how these individuals are distributed across sources — whether they received diagnoses from one, two, or all available sources. If certain results are not displayed, it is due to data protection principles—values representing five or fewer individuals are not shown to ensure privacy.", style = "cursor: help; margin-left: 5px;")
              ), value = "diagnoses",
                       br(),
                       br(),
                       fluidRow(
                         column(width = 7,withSpinner(plotlyOutput("diagnosisSourceSimplePlot", height = "700px"),color = "#add8e6")),
                         column(width = 5,withSpinner(plotlyOutput("plotDiagnosisVennHover", height = "700px"),color = "#add8e6")),
                         ),
                       br(),
                       br(),
                       withSpinner(plotlyOutput("claimSourcePlot", height = "600px"),color = "#add8e6")

              ),
              #
              # Tab 3
              #
              tabPanel(title = tagList(
                "Comorbid diagnoses",
                tags$span("\u2139", title = "If certain results are not displayed, it is due to data protection principles—values representing five or fewer individuals are not shown to ensure privacy.", style = "cursor: help; margin-left: 5px;")
              ), value = "comorbid_diagnoses",
                       br(),
                       br(),
                       withSpinner(plotlyOutput("comorbidityPlot", height = "600px"),color = "#add8e6")
              ),

              #
              # Tab 4
              #
              tabPanel(title = tagList(
                "Visits",
                tags$span("\u2139", title = "If certain results are not displayed, it is due to data protection principles—values representing five or fewer individuals are not shown to ensure privacy.", style = "cursor: help; margin-left: 5px;")
              ), value = "visits",
                       br(),
                       br(),
                       fluidRow(
                         valueBoxOutput("box_total_persons"),
                         valueBoxOutput("box_filtered_persons"),
                         valueBoxOutput("box_diag_persons"),
                         valueBoxOutput("box_total_visits"),
                         valueBoxOutput("box_filtered_visits"),
                         valueBoxOutput("box_diag_visits")
                       ),
                       br(),
                       br(),

                       withSpinner(plotlyOutput("visitPlot", height = "600px"),color = "#add8e6"),
                       br(),
                       br(),
                       tags$div(
                         style = "width: 100%; overflow-x: auto; text-align: left;",
                         tags$h4("Label data table"),
                         withSpinner(DT::dataTableOutput("visit_distribution_table",height = "800px"),color = "#add8e6")
                       )

              ),

              #
              # Tab 5
              #

              tabPanel(title = tagList(
                "Deaths",
                tags$span("\u2139", title = "If certain results are not displayed, it is due to data protection principles—values representing five or fewer individuals are not shown to ensure privacy.", style = "cursor: help; margin-left: 5px;")
              ), value = "mortality",
                       br(),
                       br(),
                       fluidRow(
                         valueBoxOutput("cohortCountBoxDeath"),
                         valueBoxOutput("deathCountBox"),
                         valueBoxOutput("allDeathPercentBox"),
                         valueBoxOutput("filteredCountBoxDeath"),
                         valueBoxOutput("filteredDeathCountBox"),
                         valueBoxOutput("filteredDeathPercentBox")),
                       br(),
                       br(),
                       br(),
                       withSpinner(plotlyOutput("deathPlot", height = "600px"),color = "#add8e6")

              )))


