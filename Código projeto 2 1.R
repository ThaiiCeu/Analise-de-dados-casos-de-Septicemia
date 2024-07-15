
# Chamando os pacotes instalados
library(ggplot2)
library(dplyr)

# Chamando a base de dados
setwd("C:/Users/luiz1/OneDrive/Área de Trabalho/Projeto 2")
DOSP <- read.csv('DOSP.csv')

#Arrumando data
adicionar_zero <- function(valor) {
  if (nchar(as.character(valor)) == 7) {
    paste0("0", valor)
  } else {
    as.character(valor)
  }
}

DOSP$DTOBITO <- as.Date(sapply(DOSP$DTOBITO, function(x) {
  ifelse(nchar(as.character(x)) == 7, adicionar_zero(x), as.character(x))
}), format = "%d%m%Y")

#Filtrando os casos de Septicemia
DOSP$CAUSABAS

DOSP <- DOSP %>% filter(SEXO %in% c(1, 2))

DOSP_SEPT = DOSP %>% filter( CAUSABAS == "A021" | CAUSABAS == "A227" | CAUSABAS == "A267" | CAUSABAS == "A327" | CAUSABAS == "A40" | CAUSABAS == "A400" | CAUSABAS == "A401" | CAUSABAS == "A402" | CAUSABAS == "A403" | CAUSABAS == "A408" | CAUSABAS == "A409" | CAUSABAS == "A41" | CAUSABAS == "A410" | CAUSABAS == "A411" | CAUSABAS == "A412" | CAUSABAS == "A413" | CAUSABAS == "A414" | CAUSABAS == "A415" | CAUSABAS == "A418" | CAUSABAS == "A419" | CAUSABAS == "A427" | CAUSABAS == "B377" | CAUSABAS == "P36" | CAUSABAS == "P360" | CAUSABAS == "A361" | CAUSABAS == "A362" | CAUSABAS == "A363" | CAUSABAS == "A364" | CAUSABAS == "A365" | CAUSABAS == "A368" | CAUSABAS == "A369")

table(DOSP_SEPT$CAUSABAS)

SEPT = str_sub(DOSP_SEPT$CAUSABAS, end = 1)
SEPT = recode(SEPT, "A" = "Septicemia" , "B" = "Septicemia" , "P" = "Septicemia" )

DOSP_SEPT$CAUSABAS = str_sub(DOSP_SEPT$CAUSABAS, end = 1)
DOSP_SEPT$CAUSABAS = recode(DOSP_SEPT$CAUSABAS, "A" = "Septicemia" , "B" = "Septicemia" , "P" = "Septicemia" )

barplot(table(DOSP_SEPT$IDADE), main = "Idade", ylab = "Pessoas", xlab = "Idade")
table(DOSP_SEPT$RACACOR)

table(DOSP_SEPT$SEXO, DOSP_SEPT$CAUSABAS)

ordem = sort(table(DOSP_SEPT$OCUP), decreasing = TRUE) 
head(ordem, 10)

ordemA = sort(table(DOSP_SEPT$LINHAA), decreasing = TRUE)
head(ordemA, 10)

ordem1 = sort(table(DOSP_SEPT$DTOBITO), decreasing = TRUE)
head(ordem1, 10)

#Começando o gráfico de séries
colunas <- names(SEPT_SP)
resultado <- filter(DOSP_SEPT, CAUSABAS %in% colunas) %>%
  select(DTOBITO, CAUSABAS, SEXO)

p1 <- filter(resultado, CAUSABAS == "Septicemia")

# Fltrando apenas os sexos 1 e 2

# Criar uma nova coluna com o mês correspondente
p1$MES <- format(as.Date(p1$DTOBITO), "%Y-%m")

# Contar o número de óbitos por mês e por sexo
contagem <- p1 %>%
  group_by(MES, SEXO) %>%
  summarize(n = n())

# Converter a coluna MES para o formato Date
contagem$MES <- as.Date(paste0(contagem$MES, "-01"))

# Criar o gráfico de série temporal com linhas para cada sexo
Septicemia <- ggplot(contagem, aes(x = MES, y = n, color = factor(SEXO))) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y", date_breaks = "8 month") +
  xlab("Mês") +
  ylab("Número de óbitos") +
  ggtitle("Série Temporal de Óbitos por Septicemia") +
  scale_color_discrete(name = "Sexo", labels = c("1" = "Masculino", "2" = "Feminino")) +
  theme_minimal()


Septicemia
