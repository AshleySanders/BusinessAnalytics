library(shiny)

shinyUI(fluidPage(
  titlePanel("Telecom Customer Churn Analysis Dashboard"),

  sidebarLayout(
    sidebarPanel(
      selectInput("contractInput", "Contract Type:",
                  choices = c("All", "Month-to-month", "One year", "Two year"),
                  selected = "All"),

      selectInput("internetInput", "Internet Service:",
                  choices = c("All", "Fiber optic", "DSL", "No"),
                  selected = "All"),

      selectInput("seniorInput", "Senior Citizen:",
                  choices = c("All", "Yes", "No"),
                  selected = "All"),

      hr(),
      h4("Prediction Tool"),
      numericInput("tenureInput", "Customer Tenure (Months):", min=0, max=72, value=12),
      numericInput("monthlyChargesInput", "Monthly Charges ($):", min=10, max=120, value=50),
      actionButton("predictBtn", "Predict Churn"),
      br(),
      br(),
      verbatimTextOutput("predictionOutput")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Overview", plotOutput("churnPlot")),
        tabPanel("Insights", plotOutput("contractPlot"), plotOutput("internetPlot")),
        tabPanel("Data Table", dataTableOutput("dataTable"))
      )
    )
  )
))
