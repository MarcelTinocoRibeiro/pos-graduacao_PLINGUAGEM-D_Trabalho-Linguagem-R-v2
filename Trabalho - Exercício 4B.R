# Lendo URL e atribuindo a tabela contida em CSV dentro da varíavel "df"
library(readr)
df = read.csv2("https://api.bcb.gov.br/dados/serie/bcdata.sgs.24363/dados?formato=csv")

# Carregando bibliotecas com funções de exibição de dados 
library(tidyverse)
library(ggpubr)

# Transformando o campo de texto para data
df <- df %>% mutate(data = as.Date(data, format = "%d/%m/%Y"))

# Filtrando os dados apenas dos anos selecionados (2010 a 2011)
df_2010_to_2011 <- df %>% filter(year(data) %in% c(2010, 2011))


# Visualizar dados sem a Regressão Polinomial
ggplot(df_2010_to_2011, aes(data, valor)) +
  labs(title = 'Dados sem a Regressão Polinomial',
       y = 'Valor',
       x = 'Mês')+
  geom_point() +
  stat_smooth()

# Visualizar dados com a Regressão Polinomial
ggplot(df_2010_to_2011, aes(data, valor)) +
  labs(title = 'Dados com a Regressão Polinomial',
       y = 'Valor',
       x = 'Mês')+
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE))