library("readxl")
library("dplyr")

#TODO in base data extraction
#Aggregate data read from files with multiple relevant sheets (see renumeracao_mensal)
#maybe (to check how rstan and others used functions) change from column to rows (see renumeracao_mensal)

pib <- read_excel("PIB real.xls")
#remove columns
pib <- subset(pib, select = -c(3))
#remove rows
pib <- pib[-c(1, 2, 3, 4, 5, 6, 7, 8, 69, 70, 71, 72, 73), ]
pib_2 <- pib_2[-seq()]


emprego_terciario_percentagem <- read_excel("Percentagem de emprego no setor terciário.xls")
#remove columns
emprego_terciario_percentagem <- subset(
  emprego_terciario_percentagem, 
  select = -c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30) 
  )
#remove rows
emprego_terciario_percentagem <- emprego_terciario_percentagem[-c(1, 2, 3, 4, 5, 7, 9, 10, 11, 12, 13, 14, 15, 16), ]


#TODO: group month info into one column; summarize into yearly or trimestral
jovens_ativos <- read_excel("Proporção de jovens na população ativa.xls")
#remove columns
jovens_ativos <- subset(jovens_ativos, select = -c(2, 4))
#remove rows
jovens_ativos <- jovens_ativos[-c(1, 2, 3, 4, 5, 6, 7, 8, 369, 370, 371, 372, 373, 374, 375, 376), ]


renumeracao_mensal <- read_excel("Remuneração média mensal bruta.xls")
renumeracao_mensal_2 <- read_excel("Remuneração média mensal bruta.xls", sheet = 2)
#remove columns
renumeracao_mensal <- subset(renumeracao_mensal, select = -seq(2, 256, 2))
renumeracao_mensal_2 <- subset(renumeracao_mensal_2, select = -seq(2, 196, 2))
#remove rows
renumeracao_mensal <- renumeracao_mensal[seq(6, 10, 1),]
renumeracao_mensal <- renumeracao_mensal[-c(2, 4), ]
renumeracao_mensal_2 <- renumeracao_mensal_2[seq(6, 10, 1),]
renumeracao_mensal_2 <- renumeracao_mensal_2[-c(2, 4), ]


desemprego_sexo_etario_nacionalidade <- read_excel("Taxa de desemprego por sexo, grupo etário e nacionalidade.xls")
#remove columns
desemprego_sexo_etario_nacionalidade <- subset(desemprego_sexo_etario_nacionalidade, select = -seq(2, 256, 2))
#remove rows
desemprego_sexo_etario_nacionalidade <- desemprego_sexo_etario_nacionalidade[seq(6, 10, 1), ]
desemprego_sexo_etario_nacionalidade <- desemprego_sexo_etario_nacionalidade[c(1, 3, 5), ]


#test;
desemprego_sexo_etario_nacionalidade <- subset(desemprego_sexo_etario_nacionalidade, select = seq(2, 158, 3))
print(desemprego_sexo_etario_nacionalidade)
