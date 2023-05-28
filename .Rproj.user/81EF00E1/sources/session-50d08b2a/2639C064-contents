# Ex.1
# Lendo URL e atribuindo a tabela contida em CSV dentro da varíavel "df"
library(readr)
df = read.csv2("https://api.bcb.gov.br/dados/serie/bcdata.sgs.24363/dados?formato=csv")


# Mostrando dados dos 2 primeiros anos
df[1:24,]
# Ex.1
# Ex.2a
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
# Ex.2a
# Ex.2b
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
# Ex.2b
# Ex.2c
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
# Ex.2c
# Ex.3
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
# Ex.3
# Ex.4a
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
# Ex.4a
# Ex.4b
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
# Ex.4b
# Ex.5
# Não existem outliers na base de acordo com os cálculos estatísticos interquartílicos
# Nota-se aumento dos valores ao decorrer dos anos
# Nota-se que existem meses específicos a cada ano que os valores possuem certas tendências de quedas (primeiro trimestre) ou altas (terceiro trimestre)
# O valor máximo foi atingido no ano de 2013



# GitHub: https://github.com/MarcelTinocoRibeiro/pos-graduacao_Prog-Ling-R_Trabalho-Linguagem-R-v2

# Ex.5