library(shiny)
library(ggplot2)
library(plotly)

# Função para moda
moda <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

ui <- fluidPage(
  
  titlePanel("Média, Mediana e Moda em Dados Agropecuários"),
  
  sidebarLayout(
    sidebarPanel(
      
      h4("Simulação dos dados"),
      
      sliderInput(
        "n",
        "Número de propriedades rurais:",
        min = 30, max = 500, value = 120, step = 10
      ),
      
      sliderInput(
        "assimetria",
        "Assimetria da distribuição:",
        min = -3, max = 3, value = 0, step = 0.5
      ),
      
      checkboxInput(
        inputId = "mostrar_medidas",
        label = "Mostrar média, mediana e moda no gráfico",
        value = TRUE
      ),
      
      
      checkboxInput(
        "outlier",
        "Adicionar evento extremo (outlier)",
        value = FALSE
      ),
      
      hr(),
      helpText("Variável: produtividade de milho (t/ha)")
    ),
    
    mainPanel(
      plotlyOutput("grafico", height = "450px"),
      hr(),
      tableOutput("medidas"),
      hr(),
      verbatimTextOutput("interpretacao")
    )
  )
)

server <- function(input, output) {
  
  dados <- reactive({
    set.seed(123)
    
    x <- rnorm(input$n, mean = 6, sd = 1)
    
    # Assimetria (situações reais do agro)
    if (input$assimetria != 0) {
      x <- x + input$assimetria * abs(rnorm(input$n, 0, 0.6))
    }
    
    # Outlier agropecuário
    if (input$outlier) {
      x[1] <- ifelse(input$assimetria >= 0, 14, 1.2)
    }
    
    x
  })
  
  output$grafico <- renderPlotly({
    
    x <- dados()
    
    media   <- mean(x)
    mediana <- median(x)
    moda_x  <- moda(round(x, 1))
    
    df <- data.frame(x)
    
    p <- ggplot(df, aes(x)) +
      
      # Histograma em FREQUÊNCIA ABSOLUTA
      geom_histogram(
        aes(y = after_stat(count)),
        bins = 25,
        fill = "lightgreen",
        color = "black"
      ) +
      
      # Medidas de tendência central
      geom_vline(xintercept = media,   color = "blue",   linewidth = 1.3) +
      geom_vline(xintercept = mediana, color = "red",    linewidth = 1.3) +
      geom_vline(xintercept = moda_x,  color = "purple", linewidth = 1.3) +
      
      labs(
        x = "Produtividade de milho (t/ha)",
        y = "Número de propriedades",
        title = "Distribuição da produtividade agrícola"
      ) +
      theme_minimal()
    
    p_plotly <- ggplotly(p) %>%
      layout(
        yaxis = list(
          tickmode = "linear",
          dtick = 2
        )
      )
    
    
    # Annotations condicionais (média, mediana e moda)
    if (input$mostrar_medidas) {
      
      p_plotly <- p_plotly %>%
        layout(
          annotations = list(
            
            list(
              x = media,
              y = 1,
              xref = "x",
              yref = "paper",
              text = paste0("Média = ", round(media, 2)),
              showarrow = FALSE,
              font = list(color = "blue", size = 12),
              bgcolor = "rgba(0,0,255,0.1)"
            ),
            
            list(
              x = mediana,
              y = 0.95,
              xref = "x",
              yref = "paper",
              text = paste0("Mediana = ", round(mediana, 2)),
              showarrow = FALSE,
              font = list(color = "red", size = 12),
              bgcolor = "rgba(255,0,0,0.1)"
            ),
            
            list(
              x = moda_x,
              y = 0.9,
              xref = "x",
              yref = "paper",
              text = paste0("Moda = ", round(moda_x, 2)),
              showarrow = FALSE,
              font = list(color = "purple", size = 12),
              bgcolor = "rgba(128,0,128,0.1)"
            )
          )
        )
    }
    
    p_plotly
  })
  
  
  
  output$medidas <- renderTable({
    
    x <- dados()
    
    data.frame(
      Medida = c("Média", "Mediana", "Moda"),
      Valor = round(c(mean(x), median(x), moda(round(x, 1))), 2)
    )
  })
  
  output$interpretacao <- renderText({
    
    x <- dados()
    
    paste(
      "INTERPRETAÇÃO DIDÁTICA\n",
      "• A MÉDIA é sensível a eventos extremos.\n",
      "• A MEDIANA é robusta e muito usada em dados agropecuários.\n",
      "• A MODA representa a produtividade mais comum.\n\n",
      "Em situações com seca, pragas ou poucas fazendas muito produtivas,\n",
      "é comum que média, mediana e moda não coincidam."
    )
  })
}

shinyApp(ui = ui, server = server)