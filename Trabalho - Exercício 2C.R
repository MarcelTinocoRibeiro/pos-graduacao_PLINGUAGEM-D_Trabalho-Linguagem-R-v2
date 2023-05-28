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


df_2009_to_2011 <- df %>% filter(year(data) >= 2009 & year(data) <= 2011)

# Usamos position="dodge" para enfileirar as barras ao invés de empilhar
ggplot(df_2009_to_2011, aes(x = factor(mes), y = valor, fill = as.factor(ano))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Mês", y = "Valor", title = "Gráfico de Barras Lado a Lado por Mês (2009-2011)") +
  scale_fill_discrete(name = "Ano")