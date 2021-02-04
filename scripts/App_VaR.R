#-----------------------------------------------------------------------------------------------------------------------------------------------
# Versão: 1.0
# Data:
# Titulo: APP de Calculo do VAR
# Autor: Robson Waite
#-----------------------------------------------------------------------------------------------------------------------------------------------

# Pacotes  -------------------------------------------------------------------------------------------------------------------------------------

library(shiny)                         # App para web
library(shinydashboard)                # Add Dashboard ao Shiny
library(shinythemes)                   # Add Tema ao Shiny
library(shinyBS)                       # Cria Pop Up
library(quantmod)                      # Pacote Financeiro
library(formattable)                   # Formtação de tabela 

# Funções --------------------------------------------------------------------------------------------------------------------------------------
# Calculo do Retorno Geometrico ---------------------------------------------------------------------------------------------------------------

geom_return = function(dado){
  dado =dado[complete.cases(dado),]                                   # Remove linhas de dados em branco
  dado$geom_return = 0                                                # Cria uma coluna para inserir os dados de retorno geometrico
  for(a in 2:nrow(dado)){
    h = a-1
    dado[a,'geom_return'] = log(dado[[a,4]]/dado[[h,4]])
  }
  return(as.data.frame(dado[,7]))
}
# Gerador de preços de fechamento --------------------------------------------------------------------------------------------------------------

getCloseprice = function(listaDeAssets, start){
  listPrice = c()
  for(asset in listaDeAssets){
    dado = try(quantmod::getSymbols(asset, src = "yahoo", from = start, to = (start+10), auto.assign = F, warnings = F))
    if(class(dado)[1] == 'try-error') {
      warning(paste('Erro ao tentar encontrar o arquivo:',asset))
    } else {
      message(paste(asset, 'O preço foi inserido na base de dados'))
      listPrice = append(listPrice, dado[[1,4]])
      message('Pausa de 0.5 segundos')
      Sys.sleep(.5)
    }
  }
  return(listPrice)
}

# Gerador de Dados do Mercado <= Retorno Geometrico ------------------------------------------------------------------------------------------ 

geraBMF = function(listaDeAssets, start, end){
  listaDeDados = data.frame()
  for(asset in listaDeAssets){
    dado = try(quantmod::getSymbols(asset, src = "yahoo", from = start, to = end, auto.assign = F, warnings = F))
    if(class(dado)[1] == 'try-error') {
      warning(paste('Erro ao tentar encontrar o arquivo:',asset))
    } else {
      message(paste(asset, ' Foi Carregado na base de dados'))
      dado = geom_return(dado)
      colnames(dado) = c(asset)
      if(length(listaDeDados) == 0){
        listaDeDados = cbind(dado)
      } else { 
        listaDeDados = cbind(listaDeDados, dado)
      }
    }
    message('Pausa de 0.5 segundos')
    Sys.sleep(.5)
  }
  return(listaDeDados)
}

# OBS: Essa função emprega uma lista de codigos de ações do IBOV ou de qualquer mercado listado no yahoo finance, no formato de strings.
# seu retorno é um dataframe composto dos retornos geometricos de cada ação selecionada.

# Calculo do VaR - Unitario ------------------------------------------------------------------------------------------------------------------------

cvar = function(valorPortifolio, niveldeConfi,dado){
  #Variaveis
  valorP = valorPortifolio                                                        # Valor do Portifolio;
  nivelConfi = qnorm(niveldeConfi)                                                # Nivel de Confiança;
  mediaRetorno = mean(dado)                                                       # Média do Retorno Geometrico;
  desvpadRetorno = sd(dado)                                                       # Desvio Padrão do Retorno Geometrico; 
  cvar = valorP - exp(mediaRetorno + nivelConfi*desvpadRetorno + log(valorP))     # VaR paametrico estimado pelo LogNormal;
  return(-cvar)
}

# Calculo da Matrix de Correlação ------------------------------------------------------------------------------------------------------------------

calCorrelacao = function(listaDeDados, listaDeAssets){
  
  correlacao = matrix(ncol = length(listaDeDados), nrow = length(listaDeDados))
  
  for (l in 1:length(listaDeDados)) {                                             # Matrix de Correlação dos retornos.   
    for(c in 1:length(listaDeDados)){
      correlacao[l,c] = cor(listaDeDados[,l], listaDeDados[,c]) 
    }
  }
  
  colnames(correlacao) = c(listaDeAssets)
  row.names(correlacao) = c(listaDeAssets)
  return(correlacao)
}

# OBS: Essa função emprega a saida da geraBMF 'listaDeDados'

# Calculo do VaR - portifolio <= calculo do Var - Unitario  -----------------------------------------------------------------------------------

pvar = function(listaDeDados,listaDeValores,niveldeConfi,listaDeAssets){
  # Calculo da Matrix de Correlação ------------------------------------------------------------------------------------------------------------------
  
  correlacao = calCorrelacao(listaDeDados,listaDeAssets)
  
  # Calculo dos VaR's individuais ------------------------------------------------------------------------------------------------------------------  
  vetorVaR = c()
  
  for(c in 1:ncol(listaDeDados)){
    vetorVaR[c] = cvar(valorPortifolio = listaDeValores[[c]], niveldeConfi = niveldeConfi, dado = listaDeDados[,c])
  }
  
  valueAtRisk = sqrt(vetorVaR%*%correlacao%*%vetorVaR)
  result = data.frame(listaDeValores, vetorVaR)
  result$var = valueAtRisk[1,1]
  assetNames = data.frame(matrix(unlist(listaDeAssets), ncol = length(1), byrow=T))
  result = cbind(assetNames, result)
  colnames(result) = c('Assets','Value','Individualized VaR', 'VaR Portfolio')
  
  return(result)
}

# Utilitarios ----------------------------------------------------------------------------------------------------------------------------------

lista = list("ABEV3.SA","AZUL4.SA","B3SA3.SA","BBAS3.SA","BBDC4.SA","BBSE3.SA","BPAC11.SA","BRDT3.SA","BRFS3.SA",
             "BRML3.SA","BTOW3.SA","CIEL3.SA","CCRO3.SA","CMIG4.SA","COGN3.SA","CSNA3.SA","CVCB3.SA","CYRE3.SA",
             "ELET3.SA","EQTL3.SA","GGBR4.SA","GNDI3.SA","GOAU4.SA","GOLL4.SA","IRBR3.SA","ITSA4.SA","ITUB4.SA",
             "JBSS3.SA","KLBN11.SA","LAME4.SA","MGLU3.SA","LREN3.SA","MULT3.SA","MRFG3.SA","PETR3.SA","PETR4.SA",
             "NTCO3.SA","PRIO3.SA","RADL3.SA","RAIL3.SA","RENT3.SA","SBSP3.SA","SULA11.SA","SUZB3.SA","TOTS3.SA",
             "UGPA3.SA","USIM5.SA","VALE3.SA","VVAR3.SA","WEGE3.SA")

# Shniy - Interface Config ---------------------------------------------------------------------------------------------------------------------


ui =  navbarPage(
  theme = shinytheme("slate"),
  title = 'Risk Management',
  
  # Tab 01 --------------------------------------------------------------------------------------------------------------------------------------
  
  tabPanel('V.A.R',icon = icon("bar-chart-o"),
           fluidRow(
             column(width = 2,
                    selectInput("asset11", h3("Select Asset's"), choices = lista, selected = sample(lista,1)[[1]]),
             ),
             column(width = 2,
                    numericInput("price11", h3("Price"),value = 0)
             ),
             column(width = 2,
                    numericInput("amount11", h3("Amount"),value = 1000)
             ),
             column(width = 2,
                    dateInput("start1",h3("Start Data Period"),value = "2020-01-01"),   
                    dateInput("end1",h3("End Data Period"),value = "2020-12-31"),
                    sliderInput('nivelConfi1', label = 'Confidence Level', min = 0.50, max = 0.99, step =0.05, value = 0.95),
             ),
             column(width = 2,
                    actionButton('button2', h4('Calculate')),      
             )      
           ),
           fluidRow(
             column(width = 10,
                    tags$head(tags$style("#modalExample1 .modal-dialog{ width:30%;}")),
                    # bsModal("modalExample", "Wait for results ...", 'button1', tableOutput("VarResult1")) # plotar o grafico em uma pop-up
                    bsModal("modalExample1", "Wait for results ...", 'button2', formattableOutput("VarResult2")) # plotar o grafico em uma pop-up
                    
             )
           ) 
  ),
  #Tab 02 ---------------------------------------------------------------------------------------------------------------------------------------
  
  tabPanel('Portfolio (05) V.A.R',icon = icon("bar-chart-o"),
           fluidRow(
             column(width = 2, 
                    selectInput("asset12", h3("Select Asset's"), choices = lista, selected = sample(lista[46:50], 1)[[1]]),
                    selectInput("asset13",  label = NULL, choices = lista, selected = sample(lista[1:5], 1)[[1]]),
                    selectInput("asset14",  label = NULL, choices = lista, selected = sample(lista[6:10], 1)[[1]]),
                    selectInput("asset15",  label = NULL, choices = lista, selected = sample(lista[11:15], 1)[[1]]),
                    selectInput("asset16",  label = NULL, choices = lista, selected = sample(lista[16:20], 1)[[1]]),
             ),
             column(width = 2,
                    numericInput("price12", h3("Price"),value = 0),
                    numericInput("price13",label = NULL,value = 0),
                    numericInput("price14",label = NULL,value = 0),
                    numericInput("price15",label = NULL,value = 0),
                    numericInput("price16",label = NULL,value = 0),

             ),
             column(width = 2,
                    numericInput("amount12", h3("Amount"),value = 1000),
                    numericInput("amount13", label = NULL,value = 1000),
                    numericInput("amount14", label = NULL,value = 1000),
                    numericInput("amount15", label = NULL,value = 1000),
                    numericInput("amount16", label = NULL,value = 1000),

             ),
             column(width = 2,
                    dateInput("start3",h3("Start Data Period"),value = "2020-01-01"),   
                    dateInput("end3",h3("End Data Period"),value = "2020-12-31"),
                    sliderInput('nivelConfi3', label = 'Confidence Level', min = 0.50, max = 0.99, step =0.05, value = 0.95),
                    checkboxInput('checkPrice2', label = 'Edit Price On', value = F),
             ),
             column(width = 2,
                    actionButton('button3', h4('Calculate')),      
             )      
           ),
           fluidRow(
             column(width = 10,
                    tags$head(tags$style("#modalExample3 .modal-dialog{ width:30%;}")),
                    # bsModal("modalExample", "Wait for results ...", 'button1', tableOutput("VarResult1")) # plotar o grafico em uma pop-up
                    bsModal("modalExample3", "Wait for results ...", 'button3', formattableOutput("VarResult3")) # plotar o grafico em uma pop-up
                    
             )
           ) 
  ),
  #Tab 03 ---------------------------------------------------------------------------------------------------------------------------------------
  
  tabPanel('Portfolio (10) V.A.R',icon = icon("bar-chart-o"),
           fluidRow(
             column(width = 2, 
              selectInput("asset1", h3("Select Asset's"), choices = lista, selected = sample(lista[46:50], 1)[[1]]),
              selectInput("asset2",  label = NULL, choices = lista, selected = sample(lista[1:5], 1)[[1]]),
              selectInput("asset3",  label = NULL, choices = lista, selected = sample(lista[6:10], 1)[[1]]),
              selectInput("asset4",  label = NULL, choices = lista, selected = sample(lista[11:15], 1)[[1]]),
              selectInput("asset5",  label = NULL, choices = lista, selected = sample(lista[16:20], 1)[[1]]),
              selectInput("asset6",  label = NULL, choices = lista, selected = sample(lista[21:25], 1)[[1]]),
              selectInput("asset7",  label = NULL, choices = lista, selected = sample(lista[26:30], 1)[[1]]),
              selectInput("asset8",  label = NULL, choices = lista, selected = sample(lista[31:35], 1)[[1]]),
              selectInput("asset9",  label = NULL, choices = lista, selected = sample(lista[36:40], 1)[[1]]),
              selectInput("asset0",  label = NULL, choices = lista, selected = sample(lista[41:45], 1)[[1]]),
              
             ),
             column(width = 2,
              numericInput("price1", h3("Price"),value = 0),
              numericInput("price2",label = NULL,value = 0),
              numericInput("price3",label = NULL,value = 0),
              numericInput("price4",label = NULL,value = 0),
              numericInput("price5",label = NULL,value = 0),
              numericInput("price6",label = NULL,value = 0),
              numericInput("price7",label = NULL,value = 0),
              numericInput("price8",label = NULL,value = 0),
              numericInput("price9",label = NULL,value = 0),
              numericInput("price0",label = NULL,value = 0),
             ),
             column(width = 2,
              numericInput("amount1", h3("Amount"),value = 1000),
              numericInput("amount2", label = NULL,value = 1000),
              numericInput("amount3", label = NULL,value = 1000),
              numericInput("amount4", label = NULL,value = 1000),
              numericInput("amount5", label = NULL,value = 1000),
              numericInput("amount6", label = NULL,value = 1000),
              numericInput("amount7", label = NULL,value = 1000),
              numericInput("amount8", label = NULL,value = 1000),
              numericInput("amount9", label = NULL,value = 1000),
              numericInput("amount0", label = NULL,value = 1000),
             ),
             column(width = 2,
              dateInput("start",h3("Start Data Period"),value = "2020-01-01"),   
              dateInput("end",h3("End Data Period"),value = "2020-12-31"),
              sliderInput('nivelConfi', label = 'Confidence Level', min = 0.50, max = 0.99, step =0.05, value = 0.95),
              checkboxInput('checkPrice', label = 'Edit Price On', value = F),
             ),
             column(width = 2,
              actionButton('button1', h4('Calculate')),      
             )      
           ),
             fluidRow(
               column(width = 10,
                tags$head(tags$style("#modalExample .modal-dialog{ width:30%;}")),
                # bsModal("modalExample", "Wait for results ...", 'button1', tableOutput("VarResult1")) # plotar o grafico em uma pop-up
                bsModal("modalExample", "Wait for results ...", 'button1', formattableOutput("VarResult1")) # plotar o grafico em uma pop-up
                
              )
             ) 
  )
) 

# Servidor -------------------------------------------------------------------------------------------------------------------------------------------------


server = function(input, output, session) {
  
  varTable = eventReactive(input$button1, {
    
    start = as.Date(input$start)
    end = as.Date(input$end)
    
    asset = list(input$asset1,input$asset2,input$asset3,input$asset4,input$asset5,input$asset6,input$asset7,input$asset8,input$asset9,input$asset0)
    
    if (input$checkPrice) {
      price = c(input$price1,input$price2,input$price3,input$price4,input$price5,input$price6,input$price7,input$price8,input$price9,input$price0)
    }else {
      price = getCloseprice(asset, start)
    }
    amount = c(input$amount1,input$amount2,input$amount3,input$amount4,input$amount5,input$amount6,input$amount7,input$amount8,input$amount9,input$amount0)
    
    listaDeValores = price*amount      # Valor do Portifolio
    
    listaDeDados = geraBMF(listaDeAssets = asset, start = start, end = end)
    
    VaRport = pvar(listaDeDados = listaDeDados,listaDeValores = listaDeValores, niveldeConfi = input$nivelConfi,listaDeAssets = asset)
    
    return(VaRport)
    
  })
  
  varTable2 = eventReactive(input$button3, {
    
    start = as.Date(input$start3)
    end = as.Date(input$end3)
    
    asset = list(input$asset12,input$asset13,input$asset14,input$asset15,input$asset16)
    
    if (input$checkPrice) {
      price = c(input$price12,input$price13,input$price14,input$price15,input$price16)
    }else {
      price = getCloseprice(asset, start)
    }
    amount = c(input$amount12,input$amount13,input$amount14,input$amount15,input$amount16)
    
    listaDeValores = price*amount      # Valor do Portifolio
    
    listaDeDados = geraBMF(listaDeAssets = asset, start = start, end = end)
    
    VaRport = pvar(listaDeDados = listaDeDados,listaDeValores = listaDeValores, niveldeConfi = input$nivelConfi,listaDeAssets = asset)
    
    return(VaRport)
    
  })
  
  indVarTable = eventReactive(input$button2, {
    
    start = as.Date(input$start1)
    end = as.Date(input$end1)
    asset = list(input$asset11)
    amount = input$amount11
    price = input$price11
    nivelDeConfi = input$nivelConfi1
    valor = amount*price 
    dados = geraBMF(asset, start, end)
    varInd = cvar(valor,nivelDeConfi,dados[,1])
    resultado = data.frame(matrix(data = c(asset,price,valor,round(varInd,2)), nrow = 1,ncol = 4))
    colnames(resultado) = c('Asset','Price','Amount', 'V.A.R')
    return(resultado)
  })
  
  
  
  # output$VarResult1 = renderTable({varTable()})
  
  output$VarResult2 = renderFormattable({formattable(indVarTable())}) #Resultados da Aba 1
  output$VarResult1 = renderFormattable({formattable(varTable())}) #Resultados da Aba 3
  output$VarResult3 = renderFormattable({formattable(varTable2())}) #Resultados da Aba 2
  
}

shinyApp(ui, server)

# - Fim ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


# Teste de funções 
# 
# 
# start = as.Date('2021-01-04')
# end = as.Date('2021-01-15')
# 
# lista = list("ABEV3.SA")
# 
# dado = geraBMF(lista, start, end)
# 
# portifolio = getCloseprice(lista, start)
# qPort = c(1000)
# qPort = 10000
# 
# datad =cvar(qPort, .95, dado[,1])
# 
# 
# resultado = pvar(dado, qPort, 0.95, lista)
# 
# assetNames = data.frame(matrix(unlist(lista), ncol = length(1), byrow=T))
