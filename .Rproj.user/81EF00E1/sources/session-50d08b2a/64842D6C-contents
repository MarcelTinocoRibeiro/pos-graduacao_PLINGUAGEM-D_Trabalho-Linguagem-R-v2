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

# Definido variáveis dependente e independente
# dados <- data.frame(func_resposta = df_2010_to_2011$valor,
#                     variavel_independente = df_2010_to_2011$data)

# Função de regressão linear
# func_regressao_linear <- lm(func_resposta~variavel_independente,data = dados)


# Exibindo gráfico utilizando a regressão linear
ggplot(df_2010_to_2011, aes(data, valor)) +
  labs(title = 'Regressão Linear dos anos de 2010 e 2011',
       y = 'Valor',
       x = 'Tempo') +
  geom_point(colour = "black", size = 2) +
  stat_smooth(method = lm)

