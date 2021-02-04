#-----------------------------------------------------------------------------------------------------------------------------------------------
# Versão: 1.0
# Data:
# Titulo: Site em Shiny para visualização de data 
# Autor: Robson Waite
#-----------------------------------------------------------------------------------------------------------------------------------------------

setwd('C:/Users/Robson/Desktop/Projetos/ShinyRisk')

arquivos = './arquivos/'
# Pacotes  -------------------------------------------------------------------------------------------------------------------------------------
  library(dplyr)
  library(quantmod)


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
  
  # Gerador de Dados do Mercado <= Retorno Geometrico ------------------------------------------------------------------------------------------ 
  
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
        message('Pausa de 1.5 segundos')
        Sys.sleep(1.5)
      }
      return(listaDeDados)
    }
    # OBS: Essa função emprega uma lista de codigos de ações do IBOV ou de qualquer mercado listado no yahoo finance, no formato de strings.
    # seu retorno é um dataframe composto dos retornos geometricos de cada ação selecionada.

lista_de_assets = c('PETR3.SA','PETR4.SA','VALE3.SA','MGLU3.SA','VVAR3.SA')
start = "2019-01-01"
end = "2019-12-31"


data = geraBMF(lista_de_assets, start, end)


coleta_dados = function(listaDeAssets, start, end){
  listaDeDados = data.frame()
  for(asset in listaDeAssets){
    dado = try(quantmod::getSymbols(asset, src = "yahoo", from = start, to = end, auto.assign = F, warnings = F))
    if(class(dado) == 'try-error') {
      warning(paste('Erro ao tentar encontrar o arquivo:',asset))
    } else {
      message(paste(asset, ' Foi Carregado na base de dados'))
      colnames(dado) = c(asset)
      if(length(listaDeDados) == 0){
        listaDeDados = cbind(dado)
      } else { 
        listaDeDados = cbind(dado, listaDeDados)
      }
    }
    message('Pausa de 1.5 segundos')
    Sys.sleep(1.5)
  }
  return(listaDeDados)
}

try(quantmod::getSymbols('PETR4.SA', src = "yahoo", from = start, to = end, auto.assign = T, warnings = F))
windows()

barChart(quantmod::getSymbols('PETR4.SA', src = "yahoo", from = start, to = end, auto.assign = F, warnings = F), name = 'PETR4.SA' ) 

addBBands()
addMACD() 
