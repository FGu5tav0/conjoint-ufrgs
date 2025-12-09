library(conjoint)


cols <- paste0(rep("profil", 9), 1:9)
tprfm <- matrix(ncol = length(cols), nrow = 2)

colnames(tprfm) <- cols
rownames(tprfm) <- c("day", "kayn")

tprfm[1, ] <- c(4, 1, 3, 7, 6, 9, 2, 5, 8)
tprfm[2, ] <- c(8, 5, 7, 4, 2, 1, 3, 6, 9)


# dados
stimuli <- expand.grid(
  Orientacao = c("Semanal", "Mensal", "Sob demanda"),
  Artigos = c("Até 2 artigos", "De 3 a 5 artigos", "Mais de 5 artigos"),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

# Adiciona número do estímulo
stimuli$Estimulo <- 1:nrow(stimuli)

# Reordena colunas
stimuli <- stimuli[, c("Estimulo", "Orientacao", "Artigos")]

stimuli


# 2. Criando o data.frame com todas as combinações

# Se você já tiver o objeto criado como texto, pode converter todas as colunas assim:
# df_fatorial[] <- lapply(df_fatorial, factor)

stimuli_label <- stimuli
cols_cat <- c("Orientacao", "Artigos")

as.integer(factor(stimuli[[cols_cat[1]]]))

for (col in cols_cat) {
  stimuli_label[[paste0(col, "_code")]] <- as.integer(factor(stimuli[[col]]))
}

tprof <- stimuli_label[, 4:5]


tlevn <- data.frame(
  levels = c(
    "Semanal",
    "Mensal",
    "Sob demanda",
    "Até 2 artigos",
    "De 3 a 5 artigos",
    "Mais de 5 artigos"
  )
)


caModel(y = tprfm[1, ], x = tprof) #Modelo para 1 participante

caUtilities(y = tprefm[1, ], x = tprof, z = tlevn) #utilidades
caPartUtilities(y = tprefm[1:2, ], x = tprof, z = tlevn) #utilidade para cada indivíduo
Conjoint(y = tpref, x = tprof, z = tlevn) #modelo global
A = caSegmentation(y = tprefm, x = tprof, c = 2) #análise cluster
A
