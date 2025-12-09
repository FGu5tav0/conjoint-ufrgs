library(conjoint) # pacote

data(tea) #dados
str(tprof)
tlevn #ver as categorias
caModel(y = tprefm[1, ], x = tprof) #Modelo para 1 participante
caUtilities(y = tprefm[1, ], x = tprof, z = tlevn) #utilidades
caPartUtilities(y = tprefm[1:10, ], x = tprof, z = tlevn) #utilidade para cada indivíduo
Conjoint(y = tpref, x = tprof, z = tlevn) #modelo global
A = caSegmentation(y = tpref, x = tprof, c = 3) #análise cluster
A
