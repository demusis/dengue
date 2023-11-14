library(tidyr)
library(dplyr)
library(imputeTS)
library(lubridate)
library(prophet) 


#
# Modelo por semana epidemiológica - Prophet
#

# Carregar os dados
dengue_data <- read_csv("dados_semana.csv", na = c("-"))

# Transformar os dados para o formato longo, remover NA, e ajustar para o Prophet
dengue_long <- dengue_data %>%
  gather(key = "semana_epidemiologica", value = "y", -Ano) %>%
  mutate(semana_epidemiologica = as.numeric(gsub("\\D", "", semana_epidemiologica)),
         primeiro_dia_ano = as.Date(paste(Ano, "-01-01", sep = "")),
         ds = primeiro_dia_ano + days((semana_epidemiologica - 1) * 7)) %>%
  filter(!is.na(y)) %>%
  select(ds, y) %>%
  arrange(ds)

# Aplicar o modelo Prophet
m <- prophet(dengue_long)
future <- make_future_dataframe(m, periods = 30)

forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

plot(m, forecast)

prophet_plot_components(m, forecast)

dyplot.prophet(m, forecast)

#
# Modelo por semana epidemiológica - Prophet
#

# Transformar os dados para o formato longo
dengue_long <- dengue_data %>%
  gather(key = "semana_epidemiologica", value = "y", -Ano) %>%
  mutate(semana_epidemiologica = as.numeric(gsub("\\D", "", semana_epidemiologica)),
         primeiro_dia_ano = as.Date(paste(Ano, "-01-01", sep = "")),
         ds = primeiro_dia_ano + days((semana_epidemiologica - 1) * 7)) %>%
  # filter(!is.na(y)) %>%
  select(ds, y) %>%
  arrange(ds)

# Preencher falhas com o Filtro de Kalman
dengue_long$y <- na.kalman(dengue_long$y)

# Converter para série temporal
dengue_ts <- ts(dengue_long$y, start = c(year(min(dengue_long$ds)), week(min(dengue_long$ds))), frequency = 52)
dengue_ts

# Criar e treinar o modelo ARIMA
modelo_nn <- nnetar(dengue_ts, 
                    size = 50, 
                    decay = 0.1, 
                    maxit = 100, 
                    P = 12)
modelo_nn

# Fazer previsões (ajuste conforme necessário)
previsao <- forecast(modelo_nn, h = 12)

# Visualizar a previsão
plot(previsao)



#
# Modelo por mês - Prophet
#

library(tidyr)
library(dplyr)
library(lubridate)
library(prophet)  # Garanta que o pacote prophet esteja instalado

# Carregar os dados
dengue_data <- read_csv("dados_mes.csv", na = c("-"))

# Transformar os dados para o formato longo e preparar para o Prophet
dengue_long <- dengue_data %>%
  gather(key = "mes", value = "y", Jan:Dez) %>%
  mutate(mes = match(mes, c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")),
         mes = ifelse(mes < 10, paste0("0", mes), as.character(mes)),
         ds = paste(Ano, mes, "01", sep = "-"),
         ds = ymd(ds)) %>%
  select(ds, y) %>%
  filter(!is.na(y))

# Criar e treinar o modelo Prophet
m <- prophet(dengue_long, uncertainty.samples = 20)

# Fazer previsões (ajuste conforme necessário)
future <- make_future_dataframe(m, periods = 90)
forecast <- predict(m, future)

# Visualizar a previsão
plot(m, forecast)


write.csv(dengue_long, "dengue_reorg_mes.csv")

#
# Modelo por mês - NN
#

# Preparação dos dados
dengue_long <- dengue_data %>%
  gather(key = "mes", value = "y", Jan:Dez) %>%
  mutate(mes = match(mes, c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")),
         mes = ifelse(mes < 10, paste0("0", mes), as.character(mes)),
         ds = paste(Ano, mes, "01", sep = "-"),
         ds = ymd(ds)) %>%
  select(ds, y) %>%
  arrange(ds)
  # filter(!is.na(y))

# Preencher falhas com o Filtro de Kalman
dengue_long$y <- na.kalman(dengue_long$y)

# Converter para série temporal
dengue_ts <- ts(dengue_long$y, start = c(year(min(dengue_long$ds)), month(min(dengue_long$ds))), frequency = 12)

# Criar e treinar o modelo ARIMA
modelo_nn <- nnetar(dengue_ts, 
                    size = 200, 
                    decay = 0.1, 
                    maxit = 100, 
                    P = 12)
modelo_nn

# Fazer previsões (ajuste conforme necessário)
previsao <- forecast(modelo_nn, h = 12)

# Visualizar a previsão
plot(previsao)
