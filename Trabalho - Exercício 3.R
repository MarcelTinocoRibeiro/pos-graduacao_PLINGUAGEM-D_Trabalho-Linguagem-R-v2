# Lendo URL e atribuindo a tabela contida em CSV dentro da varíavel "df"
library(readr)
df = read.csv2("https://api.bcb.gov.br/dados/serie/bcdata.sgs.24363/dados?formato=csv")

# Carregando bibliotecas com funções de exibição de dados 
library(tidyverse)
library(ggplot2) 

# Transformando o campo de texto para data
df <- df %>% mutate(data = as.Date(data, format = "%d/%m/%Y"))
# Adicionando campos "mes" e "ano" para exibição respectiva
df <- df %>% mutate(ano = year(data), mes = month(data))


df %>%
  summarize(
    minimo = min(valor),
    maximo = max(valor),
    media = mean(valor),
    mediana = median(valor),
    desvio_padrao = sd(valor),
    Q1 = quantile(valor, 0.25),
    Q3 = quantile(valor, 0.75),
    amp_interquartilica = Q3 - Q1,
    lim_inf = Q1 - 1.5 * amp_interquartilica,
    lim_sup = Q3 + 1.5 * amp_interquartilica,
    outliers = sum(valor < Q1 - 1.5 * amp_interquartilica | valor > Q3 + 1.5 * amp_interquartilica)
  )
# Não existem outliers pois nenhum valor se encontra abaixo de 95,535 e nem acima de 169,215
