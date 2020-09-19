#-----------------------------------------------------------------------------------------------------------------------------------------------
# Versão: 1.0
# Data:
# Titulo: Analise do VAR com yahoo finance
# Autor: Robson Waite
#-----------------------------------------------------------------------------------------------------------------------------------------------

setwd('C:/Arquivos/ShinyRisk')

arquivos = './arquivos/'

# Pacotes  -------------------------------------------------------------------------------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(shinythemes)

library(quantmod)
library(corrplot)
library(ggplot2)

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
          message('Pausa de 5 segundos')
          Sys.sleep(5)
          }
        return(listaDeDados)
      }
          # OBS: Essa função emprega uma lista de codigos de ações do IBOV ou de qualquer mercado listado no yahoo finance, no formato de strings.
          # seu retorno é um dataframe composto dos retornos geometricos de cada ação selecionada.

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
        colnames(result) = c('Value','Individualized VaR', 'VaR Portfolio')
        return(result)
      }

  # Teste - Variação do Coeficiente de Confiança --------------------------------------------------------------------------------------------------
    
    clvar = function(listaDeDados,listaDeValores,listaDeAssets){
      correlacao = calCorrelacao(listaDeDados,listaDeAssets)
      listNivelConfi = c(0.1,0.15,0.2,0.25,0.30,0.35,0.4,0.45 ,0.50 ,0.55 ,0.60 ,0.65 ,0.7 ,0.75, 0.80, 0.85 ,0.9 ,0.95, 0.99)
      listVar = c()
      i = 1
      vetorVaR = c()
      for(nivel in listNivelConfi){
        for(c in 1:ncol(listaDeDados)){
          vetorVaR[c] = cvar(listaDeValores[[c]],nivel,dado = listaDeDados[,c])
        }
        listVar[i] = sqrt(vetorVaR%*%correlacao%*%vetorVaR)
        i = i + 1
      }
      dados = data.frame(listNivelConfi, listVar)
      return(dados)
      # plot = ggplot2::ggplot(dados, aes(x =listNivelConfi, y = listVar),  color=cut, shape=color) +                                      # Geração de grafico
      #   geom_line(aes(y=listVar), size = 1, color = "Black") +
      #   scale_y_continuous(breaks = round(seq(min(dados$listVar), max(dados$listVar), by = mean(dados$listVar)/2),0))+
      #   scale_x_continuous(breaks = listNivelConfi)
      
      
    }

# Shiny App -------------------------------------------------------------------------------------------------------------------------------

tickets = read.csv(file = paste0(arquivos,'ticker2.csv'), header = T) 

lista = list("B3SA3.SA","BTOW3.SA","CSAN3.SA","EZTC3.SA","GGBR4.SA", "AALR3.SA","AALR3F.SA","AAPL34.SA","AAPL34F.SA","ABTT34.SA","ABTT34F.SA","AFLT3.SA","AFLT3F.SA","AGRO3.SA","AGRO3F.SA","ALPA3F.SA","ALPA4F.SA","ALPK3.SA","ALSO3.SA","ALUP11.SA","ALUP11F.SA","ALUP3.SA","ALUP3F.SA","ALUP4.SA","ALUP4F.SA","AMAR3.SA","AMAR3F.SA","AMBP3.SA","AMZO34F.SA","ANIM3F.SA","APER3.SA","APER3F.SA","ARMT34.SA","ARMT34F.SA","ARZZ3F.SA","ATTB34.SA","ATTB34F .SA","AVON34.SA","AVON34F.SA","AXPB34.SA","AXPB34F.SA","AZUL4.SA","BALM3.SA","BALM3F .SA","BALM4.SA","BALM4F.SA","BBAS11.SA","BBAS12.SA","BBAS3.SA","BBAS3F.SA","BBDC3.SA","BBDC4.SA","BBDC4F.SA","BBRK3.SA","BBRK3F.SA","BBSE3.SA","BDLL3.SA","BDLL3F .SA","BDLL4.SA","BDLL4F .SA","BEEF11.SA","BEEF3.SA","BEEF3F.SA","BIDI11.SA","BIDI3.SA","BIDI4.SA","BIOM3.SA","BIOM3F .SA","BMGB11.SA","BMGB4.SA","BMYB34.SA","BMYB34F.SA","BOAC34.SA","BOAC34F.SA","BOEI34.SA","BOEI34F.SA","BPAN4.SA","BPAN4F.SA","BPAR3.SA","BPAR3F .SA","BRAP3.SA","BRAP3F.SA","BRAP4.SA","BRAP4F.SA","BRDT3.SA","BRFS3.SA","BRKM3.SA","BRKM5.SA","BRKM5F.SA","BRKM6.SA","BRML3.SA","BRPR3.SA","BRPR3F.SA","BRSR3.SA","BRSR3F.SA","BRSR5.SA","BRSR5F.SA","BRSR6.SA","BRSR6F.SA","BSEV3.SA","BSEV3F.SA","BSLI3.SA","BSLI3F .SA","BSLI4.SA","BSLI4F.SA","BTTL3.SA","BTTL3F.SA","CAML3.SA","CAML3F.SA","CARD3.SA","CARD3F.SA","CASN3.SA","CASN3F .SA","CASN4.SA","CASN4F .SA","CATP34.SA","CATP34F.SA","CCRO3.SA","CEAB3.SA","CEBR3.SA","CEBR3F .SA","CEBR5.SA","CEBR5F .SA","CEBR6.SA","CEBR6F .SA","CEDO3F .SA","CEDO4F .SA","CEED3.SA","CEED3F .SA","CEED4.SA","CEED4F.SA","CEGR3.SA","CEGR3F.SA","CEPE3.SA","CEPE3F .SA","CEPE5.SA","CEPE5F .SA","CEPE6.SA","CEPE6F.SA","CESP3.SA","CESP3F.SA","CESP5.SA","CESP6.SA","CGAS3.SA","CGAS3F.SA","CGAS5.SA","CGAS5F.SA","CGRA3.SA","CGRA3F.SA","CGRA4.SA","CGRA4F.SA","CHVX34.SA","CHVX34F.SA","CLSC3.SA","CLSC3F.SA","CLSC4.SA","CLSC4F.SA","CMCS34F.SA","CMIG3.SA","CMIG3F.SA","CMIG4.SA","COCA34.SA","COCA34F.SA","COCE3.SA","COCE3F.SA","COCE5.SA","COCE5F.SA","COCE6.SA","COCE6F.SA","COGN3.SA","COGN3F.SA","COLG34.SA","COLG34F.SA","COPH34.SA","COPH34.SA","CPFE3.SA","CPFE3F.SA","CPLE3.SA","CPLE3F.SA","CPLE5.SA","CPLE5F.SA","CPLE6.SA","CPLE6F.SA","CPRE3.SA","CPRE3F.SA","CRFB3.SA","CRFB3F.SA","CSCO34.SA","CSCO34F.SA","CSNA3.SA","CSNA3F.SA","CTGP34.SA","CTGP34F.SA","CTKA3.SA","CTKA3F.SA","CTKA4.SA","CTKA4F.SA","CTNM3.SA","CTNM3F.SA","CTNM4.SA","CTNM4F.SA","CVCB3.SA","CYRE3.SA","DIRR3.SA","DIRR3F.SA","DMMO11.SA","DMMO3.SA","DMMO3F.SA","DMVF3.SA","EALT3.SA","EALT3F.SA","EALT4.SA","EALT4F.SA","EBAY34.SA","EBAY34F .SA","EEEL3.SA","EEEL3F .SA","EEEL4.SA","EEEL4F .SA","EGIE3.SA","EGIE3F.SA","ENAT3.SA","ENEV3.SA","ENEV3F.SA","ESTR3.SA","ESTR3F.SA","ESTR4.SA","ESTR4F.SA","ETER3.SA","ETER3F.SA","EUCA3.SA","EUCA3F.SA","EUCA4.SA","EUCA4F.SA","EVEN3.SA","EVEN3F.SA","EXXO34.SA","EXXO34F .SA","EZTC3.SA","EZTC3F.SA","FCXO34.SA","FCXO34F.SA","FDMO34.SA","FDMO34F.SA","FDXB34.SA","FDXB34F.SA","FESA3.SA","FESA3F.SA","FESA4.SA","FESA4F.SA","FLRY3.SA","FRAS3.SA","FRAS3F.SA","GBIO33.SA","GBIO33F.SA","GEOO34.SA","GEOO34F .SA","GEPA3.SA","GEPA3F.SA","GEPA4.SA","GEPA4F.SA","GFSA3.SA","GFSA3F.SA","GOAU4.SA","GOL4F.SA","GOLL11.SA","GOLL4.SA","GRND3.SA","GRND3F.SA","GSGI34.SA","GSGI34F .SA","GSHP3.SA","GSHP3F.SA","HALI34.SA","HALI34F.SA","HBOR3.SA","HBOR3F.SA","HGTX3F.SA","HOME34.SA","HOME34F .SA","HONB34.SA","HONB34F.SA","HOOT4.SA","HPQB34.SA","HPQB34F .SA","HYPE3.SA","IBMB34.SA","IBMB34F .SA","IGBR3.SA","IGBR3F.SA","IGTA3.SA","INEP3.SA","INEP3F.SA","INEP4.SA","INEP4F.SA","IRBR3.SA","ITLC34.SA","ITLC34F .SA","ITSA3F.SA","ITSA4.SA","ITSA4F.SA","ITUB3.SA","ITUB3F.SA","ITUB4.SA","ITUB4F.SA","IVPR3B.SA","IVPR4B.SA","JBSS3.SA","JBSS3F.SA","JHSF3.SA","JHSF3F.SA","JNJB34.SA","JNJB34F.SA","JPMC34.SA","JPMC34F .SA","KLBN11.SA","KLBN11F.SA","KLBN3.SA","KLBN3F.SA","KLBN4.SA","KLBN4F.SA","LAME3.SA","LAME3F.SA","LAME4.SA","LAME4F.SA","LCAM3.SA","LCAM3F.SA","LEVE3.SA","LEVE3F.SA","LINX3.SA","LINX3F.SA","LIQO3.SA","LJQQ3.SA","LLIS3.SA","LLIS3F.SA","LMTB34.SA","LMTB34F.SA","LOGG3.SA","LPSB3.SA","LPSB3F.SA","LREN3.SA","LREN3F.SA","LUPA3.SA","LUPA3F.SA","LWSA3.SA","MCDC34.SA","MCDC34F.SA","MDIA3.SA","MDIA3F.SA","MDNE3.SA","MGLU3.SA","MGLU3F.SA","MMMC34.SA","MMMC34F .SA","MMXM11.SA","MMXM11F.SA","MMXM3.SA","MMXM3F.SA","MNDL3.SA","MNDL3F.SA","MOVI3.SA","MOVI3F.SA","MRCK34.SA","MRCK34F .SA","MRFG3.SA","MRFG3F.SA","MRVE3.SA","MRVE3F.SA","MSBR34.SA","MSBR34F .SA","MSCD34.SA","MSCD34F.SA","MSFT34.SA","MSFT34F .SA","MTRE3.SA","MULT3.SA","MULT3F.SA","MYPK3.SA","MYPK3F.SA","NEMO3.SA","NEMO5.SA","NEMO5F.SA","NEMO6.SA","NEOE3.SA","NFLX34.SA","NFLX34F .SA","NIKE34.SA","NIKE34F.SA","NTCO3.SA","NTCO3F.SA","ODPV3.SA","ODPV3F.SA","OFSA3.SA","OFSA3F.SA","OIBR.SA","OIBR4.SA","OIBR4F.SA","ORCL34.SA","ORCL34F .SA","OSXB3.SA","OSXB3F.SA","PARD3.SA","PCAR3.SA","PCAR3F.SA","PCAR4F.SA","PDGR3.SA","PDGR3F.SA","PEPB34.SA","PEPB34F .SA","PETR3.SA","PETR3F.SA","PETR4.SA","PETR4F.SA","PFIZ34.SA","PFIZ34F .SA","PGCO34.SA","PGCO34F.SA","PMAM3.SA","PMAM3F.SA","PNVL3.SA","PNVL3F.SA","PNVL4.SA","PNVL4F.SA","POMO3.SA","POMO3F.SA","POMO4.SA","POMO4F.SA","POSI3.SA","POSI3F.SA","PRIO3.SA","PRIO3F.SA","PSSA3.SA","PSSA3F.SA","PTBL3F.SA","QCOM34.SA","QCOM34F.SA","QUAL3.SA","QUAL3F.SA","RADL3.SA","RADL3F.SA","RAIL3.SA","RAIL3F.SA","RANI3.SA","RANI3F .SA","RANI4.SA","RANI4F .SA","RAPT3.SA","RAPT3F.SA","RAPT4.SA","RAPT4F.SA","RDNI3.SA","RDNI3F.SA","RENT3.SA","RENT3F.SA","RIVA3.SA","RLOG3.SA","RLOG3F.SA","RNEW11.SA","RNEW11F.SA","RNEW11F.SA","RNEW3.SA","RNEW4.SA","RNEW4F.SA","RPMG3.SA","RPMG3F.SA","RSID3.SA","RSID3F.SA","SANB11.SA","SANB11F.SA","SANB3.SA","SANB3F.SA","SANB4.SA","SANB4F.SA","SAPR11.SA","SAPR11F.SA","SAPR3.SA","SAPR3F.SA","SAPR4.SA","SAPR4F.SA","SBSP3.SA","SBSP3F.SA","SBUB34.SA","SBUB34F .SA","SCAR3.SA","SCAR3F.SA","SEER3.SA","SHUL3.SA","SHUL4.SA","SHUL4F.SA","SLBG34.SA","SLBG34F .SA","SLED3.SA","SLED3F.SA","SLED4.SA","SLED4F.SA","SMLS3.SA","SMLS3F.SA","SMTO3.SA","SMTO3F.SA","SOMA3.SA","STBP3.SA","STBP3F.SA","SUZB3.SA","SUZB3F.SA","TAEE11.SA","TAEE3.SA","TAEE4.SA","TASA3.SA","TASA3F.SA","TASA4.SA","TASA4F.SA","TCSA3.SA","TCSA3F.SA","TELB3.SA","TELB3F.SA","TELB4.SA","TELB4F.SA","TIET11.SA","TIET11F.SA","TIET3.SA","TIET3F.SA","TIET4.SA","TIET4F.SA","TIMP3.SA","TIMP3F.SA","TOTS3.SA","TOTS3F .SA","TPIS3.SA","TPIS3F.SA","TRPL3.SA","TRPL3F.SA","TRPL4.SA","TRPL4F.SA","TSNF34F.SA","TWTR34.SA","TWTR34F.SA","TXSA34.SA","TXSA34F.SA","U1AI34.SA","U1AI34F.SA","U1AL34.SA","U1AL34F.SA","U1BE34.SA","U1BE34F.SA","U1DR34.SA","U1DR34F.SA","U1HS34.SA","U1HS34F.SA","U1LT34.SA","U1LT34F.SA","U1NM34.SA","U1NM34F.SA","U1RI34.SA","U1RI34F.SA","UBSG34.SA","UBSG34F.SA","UGPA3.SA","UGPA3F.SA","ULEV34.SA","ULEV34F.SA","UNHH34.SA","UNHH34F.SA","UNIP3.SA","UNIP3F.SA","UNIP5.SA","UNIP5F.SA","UNIP6.SA","UNIP6F.SA","UPAC34.SA","UPAC34F.SA","UPSS34.SA","UPSS34F .SA","USBC34.SA","USBC34F.SA","USIM3.SA","USIM5.SA","USIM6.SA","USSX34F.SA","V1AR34.SA","V1AR34F.SA","V1MC34.SA","V1MC34F.SA","V1NO34.SA","V1NO34F.SA","V1RS34.SA","V1RS34F.SA","V1TA34.SA","V1TA34F.SA","VALE3.SA","VALE5.SA","VERZ34.SA","VERZ34F.SA","VFCO34.SA","VFCO34F.SA","VISA34.SA","VISA34F .SA","VIVA3.SA","VIVT3.SA","VIVT3F.SA","VIVT4.SA","VIVT4F.SA","VLID3.SA","VLID3F.SA","VLOE34.SA","VLOE34F.SA","VLYB34.SA","VLYB34F.SA","VRSN34.SA","VRSN34F.SA","VRTX34.SA","VRTX34F.SA","VVAR3.SA","VVAR3F.SA","W1AB34.SA","W1AB34F.SA","W1DA34.SA","W1DA34F.SA","W1DC34.SA","W1DC34F.SA","W1EC34.SA","W1EC34F.SA","W1EL34.SA","W1EL34F.SA","W1HR34.SA","W1HR34F.SA","W1LT34.SA","W1LT34F.SA","W1MB34.SA","W1MB34F.SA","W1MC34.SA","W1MC34F.SA","W1RK34.SA","W1RK34F.SA","W1YC34.SA","W1YC34F.SA","W1YN34.SA","W1YN34F.SA","WABC34.SA","WABC34F.SA","WALM34.SA","WALM34F .SA","WATC34.SA","WATC34F.SA","WEGE3.SA","WEGE3F.SA","WFCO34.SA","WFCO34F .SA","WGBA34.SA","WGBA34F.SA","WHRL3.SA","WHRL3F.SA","WHRL4.SA","WHRL4F .SA","WUNI34.SA","WUNI34F.SA","X1EL34.SA","X1EL34F.SA","X1LN34.SA","X1LN34F.SA","X1YL34.SA","X1YL34F.SA","XRAY34.SA","XRAY34F.SA","XRXB34.SA","XRXB34F .SA","YDUQ3.SA","YUMR34.SA","YUMR34F.SA","Z1BH34.SA","Z1BH34F.SA","Z1IO34.SA","Z1IO34F.SA","Z1TS34.SA","Z1TS34F.SA")
  
  

# Shniy - Interface Config ----------------------------------------------------------------------------------------------------------------

ui =  navbarPage(
  # theme = shinytheme("slate"),
  # 
  title = 'Value At Risk',
  
  #tabPanel('Sobre', h1('Em Construção')),
  
  tabPanel('IBOVESPA',
           fluidRow(
               column(width = 2, 
                 selectInput("asset1", h3("Select Asset's"), choices = lista, selected = 'CYRE3.SA'),
                 selectInput("asset2",  label = NULL, choices = lista, selected = 'EZTC3.SA'),
                 selectInput("asset3",  label = NULL, choices = lista, selected = 'MGLU3.SA'),
                 selectInput("asset4",  label = NULL, choices = lista, selected = 'PETR4.SA'),
                 selectInput("asset5",  label = NULL, choices = lista, selected = 'VALE3.SA'),
                 selectInput("asset6",  label = NULL, choices = lista, selected = 'VVAR3.SA'),
                 selectInput("asset7",  label = NULL, choices = lista, selected = 'WEGE3.SA'),
                 selectInput("asset8",  label = NULL, choices = lista, selected = 'B3SA3.SA'),
                 selectInput("asset9",  label = NULL, choices = lista, selected = 'BTOW3.SA'),
                 selectInput("asset0",  label = NULL, choices = lista, selected = 'CSAN3.SA')
                ),
               column(width = 2,
                 numericInput("price1", h3("Price"),value = 32.80),
                 numericInput("price2",label = NULL,value = 12.87),
                 numericInput("price3",label = NULL,value = 24.23),
                 numericInput("price4",label = NULL,value = 53.23),
                 numericInput("price5",label = NULL,value = 62.60),
                 numericInput("price6",label = NULL,value = 22.93),
                 numericInput("price7",label = NULL,value = 24.72),
                 numericInput("price8",label = NULL,value = 58.39),
                 numericInput("price9",label = NULL,value = 31.75),
                 numericInput("price0",label = NULL,value = 2.98),
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
                 dateInput("start",h3("Start Data Period"),value = "2019-01-01"),   
                 dateInput("end",h3("End Data Period"),value = "2019-12-31"),
                 sliderInput('nivelConfi', label = 'Confidence Level', min = 0.50, max = 0.99, step =0.05, value = 0.95),
                 dateInput("startTest",h3("Start Test Data Period"),value = "2019-01-01"),   
                 dateInput("endTest",h3("End Test Data Period"),value = "2019-12-31"),
                 ),
               column(width = 2,
                 actionButton('button1', h4('Calculate VaR')),
                 actionButton('button2', h4('Calculate Correlation Matrix')),
                 actionButton('button3', h4('Calculate VaR Plot'))
                 ),
            ),
             fluidRow(
                column(width = 4,
                  tableOutput('VarResult1')
                ),
                column(width = 8,
                  plotOutput('VarResult2')
                )
             ),
             fluidRow(
                column(width = 4,
                  plotOutput('matrixPlot1')
                ),
                column(width = 4,
                  plotOutput('matrixPlot2')
                ),
                column(width = 4,
                  plotOutput('matrixPlot3')
                )
             )
  )
)

# Shiny Server Config ----------------------------------------------------------------------------------------------------------------------------



server <- function(input, output, session) {

  varTable = eventReactive(input$button1, {
    
    asset = list(input$asset1,input$asset2,input$asset3,input$asset4,input$asset5,input$asset6,input$asset7,input$asset8,input$asset9,input$asset0)
    
    price = c(input$price1,input$price2,input$price3,input$price4,input$price5,input$price6,input$price7,input$price8,input$price9,input$price0)
    
    amount = c(input$amount1,input$amount2,input$amount3,input$amount4,input$amount5,input$amount6,input$amount7,input$amount8,input$amount9,input$amount0)
    
    start = as.Date(input$start)
    end = as.Date(input$end)
    
    listaDeValores = price*amount      # Valor do Portifolio
      
    listaDeDados = geraBMF(listaDeAssets = asset, start = start, end = end)
    
    VaRport = pvar(listaDeDados = listaDeDados,listaDeValores = listaDeValores, niveldeConfi = input$nivelConfi,listaDeAssets = asset)
    
    return(VaRport)
    
  })
  
  varPlot = eventReactive(input$button3, {
    
    asset = list(input$asset1,input$asset2,input$asset3,input$asset4,input$asset5,input$asset6,input$asset7,input$asset8,input$asset9,input$asset0)
    
    price = c(input$price1,input$price2,input$price3,input$price4,input$price5,input$price6,input$price7,input$price8,input$price9,input$price0)
    
    amount = c(input$amount1,input$amount2,input$amount3,input$amount4,input$amount5,input$amount6,input$amount7,input$amount8,input$amount9,input$amount0)
    
    start = as.Date(input$start)
    end = as.Date(input$end)
    
    listaDeValores = price*amount      # Valor do Portifolio
    
    listaDeDados = geraBMF(listaDeAssets = asset, start = start, end = end)
    VarPlot = clvar(listaDeDados,listaDeValores,asset)
    
    return(VarPlot)
  })
  
  
  corrMatrix = eventReactive(input$button2,{
    
    asset = list(input$asset1,input$asset2,input$asset3,input$asset4,input$asset5,input$asset6,input$asset7,input$asset8,input$asset9,input$asset0)
    start = as.Date(input$start)
    end = as.Date(input$end)
    listaDeDados = geraBMF(listaDeAssets = asset, start = start, end = end)
    matrixCorr = calCorrelacao(listaDeDados, asset)
    return(matrixCorr)
  })
  
  

  
  output$VarResult1 = renderTable({varTable()})
  output$VarResult2 = renderPlot({ggplot2::ggplot(varPlot(), aes(x =listNivelConfi, y = listVar),  color=cut, shape=color) +                                      # Geração de grafico
               geom_line(aes(y=listVar), size = 1, color = "Black") +
               scale_y_continuous(breaks = round(seq(min(varPlot()$listVar), max(varPlot()$listVar), by = mean(varPlot()$listVar)/2),0))+
               scale_x_continuous(breaks = listNivelConfi)
  })
  
  output$matrixPlot1 = renderPlot({corrplot(corrMatrix(), order = 'hclust', col = gray.colors(100), tl.col = 'black')})
  output$matrixPlot2 = renderPlot({corrplot(corrMatrix(), method = 'color', order = 'FPC',type = 'upper', col = gray.colors(100), tl.col = 'black')})
  output$matrixPlot3 = renderPlot({corrplot(corrMatrix(), method = 'number', order = 'FPC',type = 'upper', col = gray.colors(100), tl.col = 'black')})
  
  
}




shinyApp(ui, server)