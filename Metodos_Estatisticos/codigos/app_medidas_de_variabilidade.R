library(shiny)
library(ggplot2)
library(plotly)

ui <- fluidPage(
  
  titlePanel("Medidas de Dispersão em Dados Agropecuários"),
  
  sidebarLayout(
    sidebarPanel(
      
      h4("Simulação dos dados"),
      
      sliderInput(
        "n",
        "Número de propriedades rurais:",
        min = 30, max = 500, value = 120, step = 10
      ),
      
      sliderInput(
        "variabilidade",
        "Nível de variabilidade produtiva:",
        min = 0.3, max = 2.5, value = 1, step = 0.1
      ),
      
      checkboxInput(
        "outlier",
        "Adicionar evento extremo (outlier)",
        value = FALSE
      ),
      
      hr(),
      helpText("Variável: produtividade de soja (t/ha)")
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
    
    x <- rnorm(
      input$n,
      mean = 3.5,                 # média realista da soja
      sd   = input$variabilidade  # controla dispersão
    )
    
    # Outlier agropecuário (seca severa ou talhão excepcional)
    if (input$outlier) {
      x[1] <- ifelse(
        input$variabilidade > 1.2,
        0.8,   # quebra de safra
        6.5    # produtividade excepcional
      )
    }
    
    x
  })
  
  output$grafico <- renderPlotly({
    
    x <- dados()
    
    media <- mean(x)
    minimo <- min(x)
    maximo <- max(x)
    
    df <- data.frame(x)
    
    p <- ggplot(df, aes(x)) +
      geom_histogram(
        aes(y = after_stat(count)),
        bins = 25,
        fill = "wheat",
        color = "black"
      ) +
      
      geom_vline(xintercept = minimo, color = "darkred",  linewidth = 1.2) +
      geom_vline(xintercept = maximo, color = "darkred",  linewidth = 1.2) +
      geom_vline(xintercept = media,  color = "blue",     linewidth = 1.3) +
      
      labs(
        x = "Produtividade de soja (t/ha)",
        y = "Número de propriedades",
        title = "Distribuição da produtividade agrícola"
      ) +
      theme_minimal()
    
    ggplotly(p) %>%
      layout(
        yaxis = list(
          tickmode = "linear",
          dtick = 2
        ),
        annotations = list(
          
          list(
            x = minimo,
            y = 1,
            xref = "x",
            yref = "paper",
            text = "Mínimo",
            showarrow = FALSE,
            font = list(color = "darkred")
          ),
          
          list(
            x = maximo,
            y = 1,
            xref = "x",
            yref = "paper",
            text = "Máximo",
            showarrow = FALSE,
            font = list(color = "darkred")
          ),
          
          list(
            x = media,
            y = 0.95,
            xref = "x",
            yref = "paper",
            text = "Média",
            showarrow = FALSE,
            font = list(color = "blue")
          )
        )
      )
  })
  
  output$medidas <- renderTable({
    
    x <- dados()
    
    data.frame(
      Medida = c(
        "Amplitude",
        "Variância",
        "Desvio padrão",
        "Coeficiente de variação (%)"
      ),
      Valor = round(c(
        max(x) - min(x),
        var(x),
        sd(x),
        sd(x) / mean(x) * 100
      ), 3)
    )
  })
  
  output$interpretacao <- renderText({
    
    paste(
      "INTERPRETAÇÃO DIDÁTICA\n",
      "• A AMPLITUDE mostra o intervalo total de variação.\n",
      "• A VARIÂNCIA e o DESVIO PADRÃO medem a dispersão em torno da média.\n",
      "• O COEFICIENTE DE VARIAÇÃO permite comparar variabilidade entre culturas.\n\n",
      "Na pesquisa agropecuária, CVs elevados indicam alta heterogeneidade\n",
      "entre propriedades ou talhões."
    )
  })
}

shinyApp(ui = ui, server = server)
