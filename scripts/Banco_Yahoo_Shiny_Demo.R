#-----------------------------------------------------------------------------------------------------------------------------------------------
# Versão: 1.0
# Data:
# Titulo: Analise do VAR com yahoo finance
# Autor: Robson Waite
#-----------------------------------------------------------------------------------------------------------------------------------------------

setwd('C:/Arquivos/ShinyRisk')

arquivos = './arquivos/'

# Pacotes  -------------------------------------------------------------------------------------------------------------------------------------

library(quantmod)
library(shiny)
library(shinydashboard)

# Banco de dados => Yahoo Finance --------------------------------------------------------------------------------------------------------------

tickets = read.csv(file = paste0(arquivos,'tickets.csv'), header = F) # Implementar => Identifcar as ações pelos rownames.

start = as.Date('2019-01-01') # inputs
end = as.Date('2019-12-31')   # inputs 


#listaDeAssets = list(input1,input2,input3,input4,input5,input6,input7,input8,input9,input10) # => Coletar os Inputs.

listaDeAssets = list('LAME4.SA','ABEV3.SA','ITUB4.SA','SBSP3.SA','VALE3.SA','PETR4.SA','JBSS3.SA','EXXO34.SA','ELET3.SA','TIET3.SA')

# Função Geradora de Base de dados -------------------------------------------------------------------------------------------------------------

geraBMF = function(listaDeAssets, start, end){
  listaDeDados = data.frame()
    for(asset in listaDeAssets){
      dado = try(quantmod::getSymbols(asset, src = "yahoo", from = start, to = end, auto.assign = F, warnings = F))
      if(class(dado) == 'try-error') {
        warning(paste('Erro ao tentar encontrar o arquivo:',asset))
      } else {
        message(paste(asset, ' Foi Carregado na base de dados'))
        dado = geom_return(dado)
        colnames(dado) = c(asset)
          if(length(listaDeDados) == 0){
            listaDeDados = cbind(dado)
          } else { 
            listaDeDados = cbind(dado, listaDeDados)
            }
        }
    }
  return(listaDeDados)
}

# Calculo do Retorno Geometrico ----------------------------------------------------------------------------------------------------------------

geom_return = function(dado){
 dado =dado[complete.cases(dado),]                                   # Remove linhas de dados em branco
 dado$geom_return = 0                                                # Cria uma coluna para inserir os dados de retorno geometrico
  for(a in 2:nrow(dado)){
   h = a-1
   dado[a,'geom_return'] = log(dado[[a,4]]/dado[[h,4]])
  }
 return(as.data.frame(dado[,7]))
}


# Calculo do Valor do Portifolio --------------------------------------------------------------------------------------------------------------------

 # => Considerando o Valor do Fechamento.

valorPortifolio = function(dado,data,quantidade){
  vfechamento = subset(dado, rownames(dado) == data)     # Busca Pelo Valor de fechamento selecionado pela data
  valor = vfechamento[,4]*quantidade                     # Calculo do valor do Portifolio.
  return(valor)
}



valorPdado = preco*quantidade


listaDeValores = c(1000,1000,1000,1000,1000,1000,1000,1000,1000,1000) # => Coletar os inputs.
listaDeQuantidades = c(0.5,2,0.5,2,0.5,2,0.5,2,0.5,2)



# Calculo do VaR - Unitario ------------------------------------------------------------------------------------------------------------------------

cvar = function(valorPortifolio,niveldeConfi,dado){
  #Variaveis
    valorP = valorPortifolio                        # Valor do Portifolio
    nivelConfi = qnorm(niveldeConfi)                # Nivel de Confiança 
    mediaRetorno = mean(dado)           # Média do Retorno Geometrico
    desvpadRetorno = sd(dado)           # Desvio Padrão do Retorno Geometrico  
    
  cvar = valorP - exp(mediaRetorno + nivelConfi*desvpadRetorno + log(valorP))     # VaR paametrico estimado pelo LogNormal.
  return(-cvar)
}

# Calculo da Matrix de Correlação ------------------------------------------------------------------------------------------------------------------

calCorrelacao = function(listaDeDados, listaDeAssets){
  
  correlacao = matrix(ncol = length(listaDeDados), nrow = length(listaDeDados))
  
  for (l in 1:length(listaDeDados)) {                                                                                        # Matrix de Correlação dos retornos.   
    for(c in 1:length(listaDeDados)){
      correlacao[l,c] = cor(listaDeDados[,l], listaDeDados[,c]) 
    }
  }
  
  colnames(correlacao) = c(listaDeAssets)
  row.names(correlacao) = c(listaDeAssets)
  return(correlacao)
}


# Calculo do VaR - portifolio ----------------------------------------------------------------------------------------------------------------------


vetorVaR = c()

pvar = function(listaDeDados,listaDeValores,niveldeConfi){
  # Calculo da Matrix de Correlação ------------------------------------------------------------------------------------------------------------------
  
  correlacao = matrix(ncol = length(listaDeDados), nrow = length(listaDeDados))
  
  for (l in 1:length(listaDeDados)) {                                                                                        # Matrix de Correlação dos retornos.   
    for(c in 1:length(listaDeDados)){
      correlacao[l,c] = cor(listaDeDados[,l], listaDeDados[,c]) 
    }
  }
  
  # Calculo dos VaR's individuais ------------------------------------------------------------------------------------------------------------------  
  vetorVaR = c()
 
  for(c in 1:ncol(listaDeDados)){
    vetorVaR[c] = cvar(valorPortifolio = listaDeValores[[c]], niveldeConfi = niveldeConfi, dado = listaDeDados[,c])
  }
  
  valueAtRisk = sqrt(vetorVaR%*%correlacao%*%vetorVaR)
  
}



  # Shiny app #### ---------------------------------------------------------------------------------------------------------------------------------  


library(corrplot)
library(RColorBrewer)

Mcorr = corrplot(correlacao, method = 'color')









