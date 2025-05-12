library(shiny)
library(tidyverse)
library(ggplot2)
library(broom)
library(car)

# Cargar datasets por separado
student_mat <- read.csv("data/student-por.csv", sep = ";")
student_por <- read.csv("data/student-mat.csv", sep = ";")

student_mat <- subset(student_mat, G3 != 0)
student_por <- subset(student_por, G3 != 0)

student_mat$asignatura <- "MAT"
student_por$asignatura <- "POR"

complete_data <- rbind(student_mat, student_por)
#

# UI
ui <- fluidPage(
  titlePanel("Análisis de Regresión Lineal: G2 vs G3"),
  sidebarLayout(
    sidebarPanel(
      selectInput("subject", "Selecciona la asignatura:",
                  choices = c("Matemáticas", "Portugués")),
      sliderInput("g2_input", "Ingresa una nota de G2 para predecir G3:",
                  min = 0, max = 20, value = 10),
      checkboxInput("show_outliers", "Mostrar sin outliers", value = TRUE)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Resumen del Modelo", 
                 verbatimTextOutput("model_summary")),
        tabPanel("Diagnóstico del Modelo", 
                 plotOutput("diagnostic_plot")),
        tabPanel("Gráfico de Regresión", 
                 plotOutput("regression_plot")),
        tabPanel("Predicción",
                 h4("Predicción de G3 basada en G2:"),
                 verbatimTextOutput("prediction"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  data_filtered <- reactive({
    df <- if (input$subject == "Matemáticas") {
      student_mat
    } else {
      student_por
    }
    
    # Eliminar outliers si se selecciona la opción
    if (input$show_outliers) {
      iqr <- IQR(df$G3)
      q1 <- quantile(df$G3, 0.25)
      q3 <- quantile(df$G3, 0.75)
      df <- df[df$G3 >= (q1 - 1.5 * iqr) & df$G3 <= (q3 + 1.5 * iqr), ]
    }
    df
  })
  
  modelo <- reactive({
    lm(G3 ~ G2, data = data_filtered())
  })
  
  output$model_summary <- renderPrint({
    summary(modelo())
  })
  
  output$diagnostic_plot <- renderPlot({
    par(mfrow = c(2, 2))
    plot(modelo())
  })
  
  output$regression_plot <- renderPlot({
    ggplot(data_filtered(), aes(x = G2, y = G3)) +
      geom_point(alpha = 0.7, color = "#2C3E50") +
      geom_smooth(method = "lm", se = TRUE, color = "#E74C3C") +
      labs(title = paste("Regresión lineal -", input$subject),
           x = "Nota G2", y = "Nota G3") +
      theme_minimal()
  })
  
  output$prediction <- renderPrint({
    nuevo_valor <- data.frame(G2 = input$g2_input)
    pred <- predict(modelo(), newdata = nuevo_valor, interval = "prediction")
    pred
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
