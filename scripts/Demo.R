
require(quantmod)
options("getSymbols.warning4.0"=FALSE)

start = as.Date('2021-01-04')
end = as.Date('2021-01-06')

quantmod::getSymbols('PETR3.SA', src = "yahoo", from = start, to = end, auto.assign = T, warnings = F)
is.OHLC(PETR3.SA)  # does the data contain at least OHL and C?
seriesHi(PETR3.SA)  # where and what was the high point 
OpCl(PETR3.SA) #daily percent change open to close

OpOp(PETR3.SA)

HiCl(PETR3.SA) # Variação percentual do maior valor p/ valor de fechamento
Cl(PETR3.SA) # Valor de Fechamento
Lag(Cl(PETR3.SA)) # Fechamento do dia anterior

dailyReturn(PETR3.SA)




quantmod::getSymbols('BVSP', src = "yahoo", from = start, to = end, auto.assign = T, warnings = F)

chartSeries(BVSP, TA ="addBBands();addCCI()")
