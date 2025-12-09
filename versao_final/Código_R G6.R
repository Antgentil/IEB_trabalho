

# ---- Pacotes necessários ----
#Biblioteca
library(dplyr)
library(stringr)
library(tidyverse)
library(lubridate)
library(zoo)
library(janitor)
library(brms)
library(bayesplot)
library(loo)
library(posterior)
library(broom)
library(bayesrules)
library(rstan)
library(rstanarm)
library(tidybayes)
library(broom.mixed)

# ---- Ler e preparar os dados ----
dados <- read_excel("Base_Dados_Final G6.xlsx")

dados <- dados %>%
  arrange(desc(row_number())) %>%      # inverte a ordem das linhas
  mutate(Trimestre = row_number())    # índice sequencial do mais antigo para o mais recente

head(dados) # Visualizar primeiras linhas

## Verificar se existem valores omissos
na.fail(dados) #se correr é porque não há valores omissos


# ---- Fazer a Análise Exploratória ----

## Análise Descritiva
library(fBasics)
stats <-basicStats(dados %>% select(-Período, -Trimestre))

# Ficar só com o essencial para caber bem na página
stats_sel <- stats[c("nobs", "Mean", "Stdev", "Variance","Minimum", "Maximum"), ]

# Transpor: variáveis em linhas, estatísticas em colunas
stats_t <- t(stats_sel)
stats_t

## Gráficos de linha ao longo do tempo (Série Temporal)
library(ggplot2)

# PIB Real ao longo do tempo
ggplot(dados, aes(x = Trimestre, y = PIB_real)) +
  geom_line(color = "lightpink", size = 1) +
  geom_point(color = "orange") +
  labs(title = "Evolução do PIB Real", x = "Trimestre", y = "PIB Real") +
  theme_minimal()

# Proporção da população 15-24 anos
ggplot(dados, aes(x = Trimestre, y = Proporção_15_24)) +
  geom_line(color = "lightgreen", size = 1) +
  geom_point(color = "darkgreen") +
  labs(title = "Proporção da população 15-24 anos", x = "Trimestre", y = "Proporção") +
  theme_minimal()

# Emprego no setor terciário
ggplot(dados, aes(x = Trimestre, y = Emprego_Terciario)) +
  geom_line(color = "lightblue", size = 1) +
  geom_point(color = "turquoise") +
  labs(title = "Emprego no setor terciário (%)", x = "Trimestre", y = "Emprego Terciário") +
  theme_minimal()

# Rendimento médio líquido
ggplot(dados, aes(x = Trimestre, y = Rendimento_medio_liquido)) +
  geom_line(color = "violet", size = 1) +
  geom_point(color ="purple") +
  labs(title = "Rendimento Médio Líquido (€)", x = "Trimestre", y = "Rendimento") +
  theme_minimal()

# Taxa de desemprego
ggplot(dados, aes(x = Trimestre, y = Taxa_Desemprego)) +
  geom_line(color = "pink", size = 1) +
  geom_point(color = "darkred") +
  labs(title = "Taxa de Desemprego (%)", x = "Trimestre", y = "Taxa de Desemprego") +
  theme_minimal()

## Correlação

# Correlação simples
cor(dados %>% select(-Período, -Trimestre))

#Gráfico de Correlação
library(corrplot)
corrplot(cor(dados %>% select(-Período, -Trimestre)), method = "circle", type = "upper")

# ---- Calcular média da taxa de desemprego (para a priori do intercepto) ----
media_desemprego <- mean(dados$Taxa_Desemprego)

# ---- Ajustar o modelo bayesiano (parte pesada) ----
set.seed(123)

modelo_bayes <- stan_glm(
  Taxa_Desemprego ~ PIB_real + Proporção_15_24 + Emprego_Terciario + Rendimento_medio_liquido,
  data = dados,
  family = gaussian(),
  prior = normal(0, 1),
  prior_intercept = normal(media_desemprego, 8),
  chains = 4,
  iter = 4000,
  warmup = 1000,
  seed = 123
)

summary(modelo_bayes)

# ---- Diagnóstico de Convergência ----

# Rhat
bayesplot::rhat(modelo_bayes)

# Neff Ratio
neff_ratio(modelo_bayes)

# Trace plots of parallel chains
mcmc_trace(modelo_bayes, size = 0.1)

# Density plots of parallel chains
mcmc_dens_overlay(modelo_bayes)

# Autocorrelação
bayesplot::mcmc_acf_bar(modelo_bayes)


# ---- Posterior Predictive Check - Avaliação de Modelos ----

pp_check(modelo_bayes)


# ---- Seleção de Modelos ----

## Modelo adicionados
#Modelo 2
modelo_2 <- stan_glm(
  Taxa_Desemprego ~ Proporção_15_24 + Emprego_Terciario + Rendimento_medio_liquido,
  data = dados,
  family = gaussian(),
  prior = normal(0, 1),
  chains = 4,
  iter = 4000,
  warmup = 1000,
  seed = 123
)

#Modelo 3
modelo_3 <- stan_glm(
  Taxa_Desemprego ~ PIB_real + Emprego_Terciario + Rendimento_medio_liquido ,
  data = dados,
  family = gaussian(),
  prior = normal(0, 1),
  chains = 4,
  iter = 4000,
  warmup = 1000,
  seed = 123
)

#Modelo 4
modelo_4 <- stan_glm(
  Taxa_Desemprego ~ Proporção_15_24 + Emprego_Terciario + PIB_real ,
  data = dados,
  family = gaussian(),
  prior = normal(0, 1),
  chains = 4,
  iter = 4000,
  warmup = 1000,
  seed = 123
)

#Modelo 5
modelo_5 <- stan_glm(
  Taxa_Desemprego ~  Emprego_Terciario ,
  data = dados,
  family = gaussian(),
  prior = normal(0, 1),
  chains = 4,
  iter = 4000,
  warmup = 1000,
  seed = 123
)

# Avaliação com LOO
library(loo)
loo_1 <- loo(modelo_bayes)
loo_2 <- loo(modelo_2)
loo_3 <- loo(modelo_3)
loo_4 <- loo(modelo_4)
loo_5 <- loo(modelo_5)

loo_compare(loo_1, loo_2, loo_3, loo_4, loo_5)

# ---- Intrepertação da Posteriori ----

# Resumo do posterior com intervalos credíveis de 80%
library(broom.mixed)
posterior_summary <- tidy(
  modelo_bayes,
  effects = c("fixed", "aux"),
  conf.int = TRUE,
  conf.level = 0.80
)

posterior_summary


# Plotar 56 modelos plausíveis sobre os dados
library(tidybayes)
library(ggplot2)
dados %>%
  add_fitted_draws(modelo_bayes, n = 56) %>%
  ggplot(aes(x = PIB_real, y = Taxa_Desemprego)) +
  geom_line(aes(y = .value, group = .draw), alpha = 0.15) +
  geom_point(size = 0.7)

# ---- Previsões Posteriori ----

previsoes <- posterior_predict(modelo_bayes, dados) 

#Previsões pontuais (média das amostras MCMC)
prediction_mean <- apply(previsoes, 2, mean)
prediction_mean

# Intervalos de credibilidade a 95%
posterior_interval(previsoes, prob = 0.95)   

# Gráfico comparativo: valores previstos vs observados
plot(prediction_mean, type = "b", col = "pink", pch = 20,
     xlab = "Trimester", ylab = "Percentagem %", main = "Comparação de Original vs previsão ")
lines(dados$Taxa_Desemprego, type = "b", col = "violet", pch = 21)
legend("topright", legend = c("Previsão Bayes", "Dados desemprego"),
       col = c("pink", "violet"), lty = 1, pch = c(20, 21))