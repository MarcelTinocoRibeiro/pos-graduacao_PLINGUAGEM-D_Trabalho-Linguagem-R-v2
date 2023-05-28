# Lendo URL e atribuindo a tabela contida em CSV dentro da varíavel "df"
library(readr)
df = read.csv2("https://api.bcb.gov.br/dados/serie/bcdata.sgs.24363/dados?formato=csv")

# Carregando bibliotecas com funções de exibição de dados 
library(tidyverse)
library(ggplot2)

# Transformando o campo de texto para data
df <- df %>% mutate(data = as.Date(data, format = "%d/%m/%Y"))

# Filtrando os dados apenas dos anos selecionados (2009 a 2011)
df_2009_to_2011 <- df %>% filter(year(data) >= 2009 & year(data) <= 2011)

# Gerando gráfico de linha com base nos anos filtrados e os valores
ggplot(df_2009_to_2011, aes(x = data, y = valor)) + geom_line(colour="blue") +
geom_point(colour="black") + labs(x = "Ano", y = "Valor", title = "Série Temporal (2009-2011)")

