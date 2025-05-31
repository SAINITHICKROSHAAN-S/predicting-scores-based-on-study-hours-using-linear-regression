library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("📚 Student Score Predictor using Study Hours"),

  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = ".csv"),
      numericInput("input_hours", "Enter Study Hours:", 
                   value = 5, min = 1, max = 10, step = 0.25),
      hr(),
      h4("📊 Predicted Score:"),
      verbatimTextOutput("predicted_score"),
      hr(),
      h5("ℹ Make sure your CSV has columns: Study_Hours, Scores")
    ),

    mainPanel(
      plotOutput("regressionPlot")
    )
  )
)

server <- function(input, output) {
  data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath)
    validate(
      need(all(c("Study_Hours", "Scores") %in% colnames(df)), 
           "CSV must contain 'Study_Hours' and 'Scores' columns.")
    )
    df
  })

  model <- reactive({
    df <- data()
    lm(Scores ~ Study_Hours, data = df)
  })

  prediction <- reactive({
    req(data(), input$input_hours)
    predict(model(), newdata = data.frame(Study_Hours = input$input_hours))
  })

  output$predicted_score <- renderText({
    req(prediction())
    paste("Predicted score for", input$input_hours, "hours of study is:", round(prediction(), 2))
  })

  output$regressionPlot <- renderPlot({
    req(data())
    df <- data()
    df$Predicted <- predict(model())

    ggplot(df, aes(x = Study_Hours, y = Scores)) +
      geom_point(color = "blue", size = 3) +
      geom_line(aes(y = Predicted), color = "red", size = 1.2) +
      geom_point(aes(x = input$input_hours, y = prediction()), color = "darkgreen", size = 4) +
      labs(
        title = "📈 Study Hours vs Scores (with Prediction)",
        x = "Study Hours",
        y = "Score"
      ) +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
```