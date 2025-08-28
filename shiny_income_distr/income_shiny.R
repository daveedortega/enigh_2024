library(shiny)
library(tidyverse)

concentrado_trabajadores <- read_rds("labour_income_distr.rds")

ui <- fluidPage(
  numericInput("user_salary", "Tu salario mensual neto:", 8800, min = 0, step = 500),
  plotOutput("income_plot")
)

server <- function(input, output, session) {
  
  output$income_plot <- renderPlot({
    
    user_input <- input$user_salary
    
    thing <- concentrado_trabajadores %>%
      summarise(
        cdf = sum(factor * (ingreso_mensual_pp < user_input), na.rm = TRUE) /
          sum(factor, na.rm = TRUE)
      ) %>%
      pull(cdf)
    
    concentrado_trabajadores %>% 
      ggplot(aes(ingreso_mensual_pp, weight = factor)) +
      geom_histogram(bins = 100, fill = "gray70", color = "white") +
      
      geom_vline(aes(xintercept = 11386, color = "SALARIO PROMEDIO"), 
                 linetype = "dashed", linewidth = 1) +
      geom_vline(aes(xintercept = 19972.8, color = "X DECIL"), 
                 linetype = "dashed", linewidth = 1) +
      geom_vline(aes(xintercept = user_input, color = "TÚ"), 
                 linetype = "dashed", linewidth = 1) +
      
      scale_color_manual(
        name = "Referencia",
        values = c("SALARIO PROMEDIO" = "green", 
                   "X DECIL" = "blue", 
                   "TÚ" = "red")
      ) +
      theme_minimal() +
      labs(
        x = "", y = "", 
        title = paste("Tú ganas más que el", round(thing*100, 1), "% de los mexicanos"), 
        subtitle = "Remuneración mensual, pesos de 2024", 
        caption = "Elaboración propia, ENIGH 2024 - INEGI"
      ) +
      theme(
        plot.title = element_text(size = 28, face = "bold"), 
        plot.subtitle = element_text(size = 20)
      )
  })
}

shinyApp(ui, server)
