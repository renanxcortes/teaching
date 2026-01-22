library(shiny)
library(ggplot2)

# Função para calcular a moda
moda <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

ui <- fluidPage(
  
  titlePanel("Medidas de Tendência Central em Dados Agropecuários"),
  
  sidebarLayout(
    sidebarPanel(
      
      h4("Simulação de dados"),
      
      sliderInput(
        inputId = "n",
        label = "Número de propriedades rurais:",
        min = 30,
        max = 500,
        value = 100,
        step = 10
      ),
      
      sliderInput(
        inputId = "assimetria",
        label = "Assimetria da distribuição:",
        min = -3,
        max = 3,
        value = 0,
        step = 0.5
      ),
      
      checkboxInput(
        inputId = "outlier",
        label = "Adicionar outlier (evento extremo)",
        value = FALSE
      ),
      
      hr(),
      helpText("Variável: produtividade de milho (t/ha)")
    ),
    
    mainPanel(
      
      plotOutput("histograma", height = "400px"),
      
      hr(),
      
      tableOutput("medidas"),
      
      hr(),
      
      h4("Interpretação didática"),
      verbatimTextOutput("interpretacao")
    )
  )
)

server <- function(input, output) {
  
  dados <- reactive({
    
    set.seed(123)
    
    # Base: produtividade média de milho no Brasil (~5 a 6 t/ha)
    base <- rnorm(input$n, mean = 6, sd = 1)
    
    # Ajuste de assimetria
    if (input$assimetria != 0) {
      base <- base + input$assimetria * abs(rnorm(input$n, 0, 0.7))
    }
    
    # Outlier agropecuário
    if (input$outlier) {
      # Outlier default gerado pelo Chat GPT
      # base[1] <- ifelse(input$assimetria >= 0, 14, 1.5)
      
      # Outlier mais extremo
      base[1] <- ifelse(input$assimetria >= 0, 16, 0.1)
    }
    
    base
  })
  
  output$histograma <- renderPlot({
    
    x <- dados()
    
    media <- mean(x)
    mediana <- median(x)
    moda_x <- moda(round(x, 1))
    
    ggplot(data.frame(x), aes(x)) +
      geom_histogram(aes(y = after_stat(density)),
                     bins = 25,
                     fill = "lightgreen",
                     color = "black") +
      geom_density(color = "darkgreen", linewidth = 1) +
      geom_vline(xintercept = media, color = "blue", linewidth = 1.2) +
      geom_vline(xintercept = mediana, color = "red", linewidth = 1.2) +
      geom_vline(xintercept = moda_x, color = "purple", linewidth = 1.2) +
      labs(
        x = "Produtividade de milho (t/ha)",
        y = "Densidade",
        title = "Distribuição da produtividade agrícola"
      ) +
      annotate("text", x = media, y = 0, label = "Média", 
               color = "blue", angle = 90, vjust = -0.5) +
      annotate("text", x = mediana, y = 0, label = "Mediana", 
               color = "red", angle = 90, vjust = -0.5) +
      annotate("text", x = moda_x, y = 0, label = "Moda", 
               color = "purple", angle = 90, vjust = -0.5) +
      theme_minimal()
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
      "• A MÉDIA representa a produtividade média das propriedades.\n",
      "• A MEDIANA divide as propriedades em duas metades.\n",
      "• A MODA indica a produtividade mais frequente.\n\n",
      "Quando há assimetria ou outliers (ex.: seca severa ou safra excepcional),\n",
      "a média tende a ser puxada, enquanto a mediana é mais robusta.\n",
      "Isso é comum em dados agropecuários reais."
    )
  })
}

shinyApp(ui = ui, server = server)