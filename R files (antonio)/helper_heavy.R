# helper_heavy.R

# ---- Pacotes necessários ----
library(readxl)
library(dplyr)
library(rstanarm)

# ---- Ler e preparar os dados ----
dados <- read_excel("Base_Dados_Final.xlsx")

dados <- dados %>%
  arrange(desc(row_number())) %>%      # inverte a ordem das linhas
  mutate(Trimestre = row_number())    # índice sequencial do mais antigo para o mais recente

# ---- Guardar os dados já tratados ----
saveRDS(dados, "dados_tratados.rds")

# ---- Calcular média da taxa de desemprego (para a priori do intercepto) ----
media_desemprego <- mean(dados$Taxa_Desemprego)

# ---- Ajustar o modelo bayesiano (parte pesada) ----
set.seed(123)

modelo_bayes <- stan_glm(
  Taxa_Desemprego ~ PIB_real + Proporção_15_24 + Emprego_Terciario + Rendimento_medio_liquido,
  data = dados,
  family = gaussian(),
  prior = normal(0, 1),
  prior_intercept = normal(media_desemprego, 3),
  chains = 4,
  iter = 4000,
  warmup = 1000,
  seed = 123
)

# ---- Guardar o modelo para usar no Rmd ----
saveRDS(modelo_bayes, "modelo_bayes.rds")
