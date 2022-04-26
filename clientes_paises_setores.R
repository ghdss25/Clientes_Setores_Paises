setwd("/home/gustavo/Projeto de Dados/Análise_R/Clientes_Paises_Setores")
getwd()

library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr)

clientes <- read.csv("clientes.txt")
paises <- read.csv("paises.txt")
setores <- read.csv("setores.txt")

## Inserir novas colunas e unifica os dados para essas novas colunas 
clientes <- clientes %>%
  rename(codigo = Codigo_Cliente) %>%
  mutate(Codigo_Cliente = str_sub(codigo,1,5),
         Codigo_Pais = str_sub(codigo, 6,7),
         Codigo_Setor = str_sub(codigo, 8,11))

## Remove a primeira coluna do dataset 
clientes <- clientes[,-c(1)]

View(clientes)
View(paises)
View(setores)

## Instalando o pacote do R Sqlite 
install.packages("RSQLite")
library(RSQLite)
library(DBI)

## Conexão com o banco de dados 
db <- dbConnect(SQLite(), dbname="/home/gustavo/Projeto de Dados/Análise_R/Clientes_Paises_Setores.sqlite3")

## Conexão escrita das Colunas para virar um Banco de dados 
dbWriteTable(db, "clientes", clientes, overwrite=TRUE)
dbWriteTable(db, "paises", paises, overwrite=TRUE)
dbWriteTable(db, "setores", setores, overwrite=TRUE)

## Selecionando as colunas para a consulta de dados
clientes_db <- dbGetQuery(db, "SELECT * FROM clientes")
paises_db <- dbGetQuery(db, "SELECT * FROM paises")
setores_db <- dbGetQuery(db, "SELECT * FROM setores")

View(clientes_db)
View(paises_db)
View(setores_db)

## Junção de Tabelas para consultas de dados de clientes e Paises 
myquery = "SELECT c.Codigo_Pais, p.País, s.Setor FROM clientes c
           INNER JOIN paises p ON c.Codigo_Pais = p.Código
           INNER JOIN setores s ON c.Codigo_Setor = s.Código"

consulta_cliente_pais_setor <- dbGetQuery(db, myquery)

View(consulta_cliente_pais_setor)

## Contagem de Setores por Setor
myquery1 = "SELECT s.Setor, COUNT(c.Codigo_Setor) FROM clientes c, setores s
WHERE c.Codigo_Setor = s.Código GROUP BY s.Setor"

consulta_cliente_Setor <- dbGetQuery(db, myquery1)

View(consulta_cliente_Setor)

# Gráfico 1 - Contagem de Setores 
esquisser(consulta_cliente_Setor)

library(ggplot2)

ggplot(consulta_cliente_Setor) +
  aes(x = `COUNT(c.Codigo_Setor)`, y = Setor) +
  geom_boxplot(fill = "#ED0000") +
  labs(x = "Quantidade", y = "Setores", title = "Quantidade de Setores ", subtitle = "Contagem de Setores ", 
       caption = "Opção de Setores por quantidade") +
  theme_dark() +
  theme(plot.title = element_text(face = "bold", 
                                  hjust = 0.5), plot.subtitle = element_text(face = "bold", hjust = 0.5))

## Contagem de Códigos de País por Páis
myquery2 = "SELECT p.País, COUNT(c.Codigo_Pais) FROM clientes c, paises p 
WHERE c.Codigo_Pais = p.Código GROUP BY p.País"

consulta_cliente_Pais <- dbGetQuery(db, myquery2)

View(consulta_cliente_Pais)

# Gráfico 2 - Contagem de Países 
data <- data.frame(
  
  contagem = consulta_cliente_Pais$`COUNT(c.Codigo_Pais)`, 
  pais = consulta_cliente_Pais$País
)

ggplot(data, aes(x=pais, y=contagem)) + 
  labs(title = "Contagem de Códigos da Coluna Páis") + 
  geom_bar(stat = "identity") + coord_flip()


