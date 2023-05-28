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

## Caso fosse entre 2009 e 2011
# Filtrando os dados apenas dos anos selecionados (2009 a 2011)
# df_2009_to_2011 <- df %>% filter(year(data) >= 2009 & year(data) <= 2011)

month_labels = c("Janeiro", "Fevereiro", "Março",
                 "Abril", "Maio", "Junho",
                 "Julho", "Agosto", "Setembro",
                 "Outubro", "Novembro", "Dezembro") # PT-BR

# month_labels = month.name #---> build-in months # ENG
# month_labels = df$mes #---> meses numericamente representados de 1 a 12 # ENG

# Exibindo gráfico em barras por ano com "subbarras" mensais
ggplot(df, aes(x = ano, y = valor, fill = factor(mes))) +
  geom_bar(stat = "identity") +
  labs(x = "Ano", y = "Valor", title = "Gráfico de Barras por Ano e Mês") + 
  scale_fill_discrete(name = "Mês", labels = month_labels)