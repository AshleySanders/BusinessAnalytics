library(shiny)
library(tidyverse)
library(randomForest)

shinyServer(function(input, output) {

  # Load data and model
  telco <- readRDS("telco_clean.rds")
  model <- readRDS("rf_model.rds")

  # Reactive data filtering
  filteredData <- reactive({
    data <- telco
    if (input$contractInput != "All") {
      data <- data %>% filter(Contract == input$contractInput)
    }
    if (input$internetInput != "All") {
      data <- data %>% filter(InternetService == input$internetInput)
    }
    if (input$seniorInput != "All") {
      data <- data %>% filter(SeniorCitizen == input$seniorInput)
    }
    data
  })

  # Churn Overview Plot
  output$churnPlot <- renderPlot({
    ggplot(filteredData(), aes(Churn, fill=Churn)) +
      geom_bar() +
      labs(title="Customer Churn Overview", y="Count") +
      theme_minimal()
  })

  # Churn by Contract Type Plot
  output$contractPlot <- renderPlot({
    ggplot(filteredData(), aes(Contract, fill=Churn)) +
      geom_bar(position="fill") +
      labs(title="Churn by Contract Type", y="Proportion") +
      theme_minimal()
  })

  # Churn by Internet Service Plot
  output$internetPlot <- renderPlot({
    ggplot(filteredData(), aes(InternetService, fill=Churn)) +
      geom_bar(position="fill") +
      labs(title="Churn by Internet Service", y="Proportion") +
      theme_minimal()
  })

  # Display Data Table
  output$dataTable <- renderDataTable({
    filteredData()
  })

  # Predictive Tool
  observeEvent(input$predictBtn, {
    new_data <- data.frame(
      tenure = input$tenureInput,
      MonthlyCharges = input$monthlyChargesInput,
      TotalCharges = input$tenureInput * input$monthlyChargesInput,
      gender = "Male", # Example static defaults
      SeniorCitizen = "No",
      Partner = "No",
      Dependents = "No",
      PhoneService = "Yes",
      MultipleLines = "No",
      InternetService = "Fiber optic",
      OnlineSecurity = "No",
      OnlineBackup = "No",
      DeviceProtection = "No",
      TechSupport = "No",
      StreamingTV = "No",
      StreamingMovies = "No",
      Contract = "Month-to-month",
      PaperlessBilling = "Yes",
      PaymentMethod = "Electronic check"
    )

    pred <- predict(model, new_data, type="prob")
    output$predictionOutput <- renderText({
      paste0("Probability of churn: ", round(pred[,"Yes"]*100,2), "%")
    })
  })
})
