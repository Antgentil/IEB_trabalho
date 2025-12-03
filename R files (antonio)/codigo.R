# instalar o pacote readxl (so precisa de ser feito uma vez na maquina)
install.packages("readxl")

# carregar o pacote readxl para poder ler ficheiros Excel
library(readxl)

# ler o ficheiro Excel Base_Dados_Final.xlsx da diretoria de trabalho
dados <- read_excel("Base_Dados_Final.xlsx")

# nas colunas importadas do Excel os NA vieram como texto "NA"
# o R trata texto como character, e funcoes numericas (como as regressões) nao aceitam character
# por isso primeiro substituimos a string "NA" por verdadeiro NA (missing value reconhecido pelo R)

# coluna da remuneracao liquida: trocar string "NA" por NA
dados$`Rendimento_medio_liquido (euros)`[
  dados$`Rendimento_medio_liquido (euros)` == "NA"
] <- NA

# converter a coluna de remuneracao liquida de character para numerico
# isto e necessario porque read_excel le misturas de numeros e texto como character
# e modelos de regressao exigem variaveis numericas
dados$`Rendimento_medio_liquido (euros)` <-
  as.numeric(dados$`Rendimento_medio_liquido (euros)`)

# coluna da taxa de desemprego: trocar string "NA" por NA
dados$Taxa_Desemprego[dados$Taxa_Desemprego == "NA"] <- NA

# converter a coluna da taxa de desemprego para numerico pelas mesmas razoes
dados$Taxa_Desemprego <- as.numeric(dados$Taxa_Desemprego)

# arredondar a taxa de desemprego a 1 casa decimal
# isto elimina restos de precisao binaria (ex: 8.1999999) que surgem na conversao para numerico
# e garante que os valores ficam com a mesma escala que aparece nas tabelas do INE
dados$Taxa_Desemprego <- round(dados$Taxa_Desemprego, 1)

# ver as primeiras linhas da base de dados para confirmar que ficou tudo correto
head(dados)

# ver a estrutura da base de dados (tipos das colunas, numero de observacoes, etc.)
str(dados)



# --------------------------------------------------------------------------
# Regressao linear classica (lm()), que serve para diagnostico preliminar.
# --------------------------------------------------------------------------


# criar uma coluna com ano e trimestre em formato reconhecido pelo R
install.packages("lubridate")
library(lubridate)

dados$Ano <- as.numeric(sub(".* de (\\d{4})", "\\1", dados$`Período de referência dos dados`))
dados$Trim <- as.numeric(substr(dados$`Período de referência dos dados`, 1, 1))



modelo1 <- lm(Taxa_Desemprego ~ PIB_real + Proporção_15_24 +
                Emprego_Terciario + `Rendimento_medio_liquido (euros)`,
              data = dados)

summary(modelo1)

plot(modelo1)



# --------------------------------------------------------------------------
# Comecar a parte Bayesiana
# --------------------------------------------------------------------------

install.packages("rstanarm")
library(rstanarm)


# Definir um modelo com priors fracos (nao informativos)
modelo_bayes <- stan_glm(
  Taxa_Desemprego ~ PIB_real + Proporção_15_24 + Emprego_Terciario + `Rendimento_medio_liquido (euros)`,
  data = dados,
  family = gaussian(),
  prior = normal(0, 10),             # prior para os betas
  prior_intercept = normal(0, 20),   # prior para intercepto
  chains = 4,                        # numero de cadeias MCMC
  iter = 4000,                       # nº total de iteracoes
  warmup = 1000,                     # burn-in
  seed = 123
)

# Ver resultados
summary(modelo_bayes)
plot(modelo_bayes)        # intervalos posteriores
posterior_interval(modelo_bayes)


# Diagnostico da convergencia

library(bayesplot)

mcmc_trace(as.array(modelo_bayes))        # Cadeias — deve haver mistura
mcmc_rhat(rhat(modelo_bayes))             # R-hat ≈ 1
mcmc_neff(neff_ratio(modelo_bayes))       # tamanho efetivo das amostras


# Previsoes Bayesianas

previsoes <- posterior_predict(modelo_bayes)
apply(previsoes, 2, mean)                            # previsoes pontuais
apply(previsoes, 2, quantile, c(0.025, 0.975))       # intervalo credível



