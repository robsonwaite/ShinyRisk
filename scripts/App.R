#-----------------------------------------------------------------------------------------------------------------------------------------------
# Versão: 1.0
# Data:
# Titulo: Site em Shiny para visualização de dados Financeiros Yahoo 
# Autor: Robson Waite
#-----------------------------------------------------------------------------------------------------------------------------------------------

# Pacotes  -------------------------------------------------------------------------------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyBS)
library(quantmod)

# Funções --------------------------------------------------------------------------------------------------------------------------------------

lista = list('PETR3.SA','MGLU3.SA','PETR4.SA','VALE3.SA','VVAR3.SA','WEGE3.SA')

ui =  navbarPage(
  theme = shinytheme("slate"),
  title = 'MCRN',
    tabPanel('IBOVESPA',icon = icon("bar-chart-o"),
      fluidRow(
        column(width = 2, 
          selectInput("asset1", h3("Select Asset's"), choices = lista),
          actionButton('button1', h4('Go')),
        ),  
        column(width = 2,
         dateInput("start",h3("Start Data Period"),value = "2019-01-01"),   
         dateInput("end",h3("End Data Period"),value = "2019-12-31"),
         
        ),
        column(width = 2,
          # checkboxInput(inputId = 'opcao1', label = 'Bar Chart', value = TRUE),
          # checkboxInput(inputId = 'opcao2', label = 'Bar Chart + Bands', value = TRUE),
          # checkboxInput(inputId = 'opcao3', label = 'Bar Chart + MACD', value = TRUE),
         checkboxGroupInput("opcoes", label = h3("Options"),
                                  choices = list("Bar Chart" = 1, "Bar Chart + Bands" = 2, "Bar Chart + MACD" = 3),
                                  selected = 1),
        )      
      ),
      fluidRow(
        column(width = 10,
          #plotOutput('asset1Result'), plotar o grafico dentro do layout do site
          tags$head(tags$style("#modalExample .modal-dialog{ width:90%;}")),
          bsModal("modalExample", "Results", 'button1', plotOutput("asset1Result")) # plotar o grafico em uma pop-up
        )
      ) 
    )
)


server = function(input, output, session) {
  
  sdados = eventReactive(input$button1, {
    start = as.Date(input$start)
    end = as.Date(input$end)
    if (input$opcoes == 1) {
      barChart(quantmod::getSymbols(input$asset1, src = "yahoo", from = start, to = end, auto.assign = F, warnings = F), name = input$asset1)
    } else {
      if (input$opcoes == 2) {
        barChart(quantmod::getSymbols(input$asset1, src = "yahoo", from = start, to = end, auto.assign = F, warnings = F), name = input$asset1)
        addBBands()
      } else {
          barChart(quantmod::getSymbols(input$asset1, src = "yahoo", from = start, to = end, auto.assign = F, warnings = F), name = input$asset1)
          addMACD()
        }
    }
  })
  
  output$asset1Result = renderPlot({sdados()})
  
 
}

shinyApp(ui, server)


