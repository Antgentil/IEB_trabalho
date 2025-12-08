# carregar o pacote readxl para poder ler ficheiros Excel
library(readxl)

# ler o ficheiro Excel Base_Dados_Final.xlsx da diretoria de trabalho
dados <- read_excel("Base_Dados_Final.xlsx")

na.fail(dados)

# converter character para numerico
dados$Rendimento_medio_liquido <- as.numeric(dados$Rendimento_medio_liquido)
dados$Taxa_Desemprego <- as.numeric(dados$Taxa_Desemprego)
dados$Ano <- as.numeric(sub(".* de (\\d{4})", "\\1", dados$Período))
dados$Trim <- as.numeric(substr(dados$Período, 1, 1))

dados$Período

# ver as primeiras linhas da base de dados para confirmar que ficou tudo correto
head(dados)

# ver a estrutura da base de dados (tipos das colunas, numero de observacoes, etc.)
str(dados)

# --------------------------------------------------------------------------
# Regressao linear classica (lm()), que serve para diagnostico preliminar.
# --------------------------------------------------------------------------

library(lubridate)


str(dados)

modelo1 <- lm(Taxa_Desemprego ~ PIB_real + Proporção_15_24 +
                Emprego_Terciario + Rendimento_medio_liquido,
              data = dados)

summary(modelo1)

plot(modelo1)


# --------------------------------------------------------------------------
# Comecar a parte Bayesiana
# --------------------------------------------------------------------------

library(rstanarm)


modelo_bayes <- stan_glm(
  Taxa_Desemprego ~ PIB_real + Proporção_15_24 + Emprego_Terciario + Rendimento_medio_liquido,
  data = dados,
  family = gaussian(),
  prior = normal(0, 10),             # prior para os betas
  prior_intercept = normal(0, 20),   # prior para intercepto
  chains = 4,                        # numero de cadeias MCMC
  iter = 4000,                       # nº total de iteracoes
  warmup = 1000,                     # burn-in
  seed = 123
)
#head(as.data.frame(modelo_bayes), 5)

# Ver resultados
summary(modelo_bayes)
plot(modelo_bayes)        # intervalos posteriores
posterior_interval(modelo_bayes)


# Diagnostico da convergencia
library(bayesplot)

#  Rhat and neff as abaixo como gráficos
neff_ratio(modelo_bayes)
rhat(modelo_bayes)

#Não utilizado, pode ser útil depois
#mcmc_dens_overlay(modelo_bayes)


mcmc_trace(modelo_bayes)        # Cadeias — deve haver mistura
#todo encontrar forma de dar nome às linhas, deve ser especialmente útil para slides
mcmc_rhat(rhat(modelo_bayes))             # R-hat ≈ 1
mcmc_neff(neff_ratio(modelo_bayes))       # tamanho efetivo das amostras



#previsoes
previsoes <- posterior_predict(modelo_bayes, dados) #cada coluna contem previsoes de um semestre

prediction_mean <- apply(previsoes, 2, mean)        # previsoes pontuais
apply(previsoes, 2, quantile, c(0.025, 0.975))       # intervalo credível

plot(prediction_mean, type = "b", col = "blue", pch = 16,
     xlab = "Trimestre", ylab = "Percentagem %", main = "Comparação de Original vs previsão ") 
lines(dados$Taxa_Desemprego, type = "b", col = "red", pch = 17)  
legend("topright", legend = c("Previsão Bayes", "Dados desemprego"), 
         col = c("blue", "red"), lty = 1, pch = c(16, 17))

var(prediction_mean - dados$Taxa_Desemprego)






