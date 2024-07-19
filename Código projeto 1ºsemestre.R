
# Chamando os pacotes instalados
library(ggplot2)
library(dplyr)
library(stringr)

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

DOSP_SEPT = DOSP %>% filter( CAUSABAS == "A021" | CAUSABAS == "A227" | CAUSABAS == "A267" | CAUSABAS == "A327" | CAUSABAS == "A40" | CAUSABAS == "A400" | CAUSABAS == "A401" | CAUSABAS == "A402" | CAUSABAS == "A403" | CAUSABAS == "A408" | CAUSABAS == "A409" | CAUSABAS == "A41" | CAUSABAS == "A410" | CAUSABAS == "A411" | CAUSABAS == "A412" | CAUSABAS == "A413" | CAUSABAS == "A414" | CAUSABAS == "A415" | CAUSABAS == "A418" | CAUSABAS == "A419" | CAUSABAS == "A427" | CAUSABAS == "B377" | CAUSABAS == "P36" | CAUSABAS == "P360" | CAUSABAS == "P361" | CAUSABAS == "P362" | CAUSABAS == "P363" | CAUSABAS == "P364" | CAUSABAS == "P365" | CAUSABAS == "P368" | CAUSABAS == "P369")

SEPT = str_sub(DOSP_SEPT$CAUSABAS, end = 1)
SEPT = recode(SEPT, "A" = "Septicemia" , "B" = "Septicemia" , "P" = "Septicemia" )

SEPT_tab = table(DOSP_SEPT$CAUSABAS)

DOSP_SEPT$CAUSABAS = str_sub(DOSP_SEPT$CAUSABAS, end = 1)
DOSP_SEPT$CAUSABAS = recode(DOSP_SEPT$CAUSABAS, "A" = "Septicemia" , "B" = "Septicemia" , "P" = "Septicemia" )

barplot(table(DOSP_SEPT$IDADE), main = "Idade", ylab = "Pessoas", xlab = "Idade")
table(DOSP_SEPT$RACACOR)

SEPT_tab_2 = table(DOSP_SEPT$CAUSABAS)

#Começando o gráfico de séries
colunas <- names(SEPT_tab_2)
resultado <- filter(DOSP_SEPT, CAUSABAS %in% colunas) %>%
  select(DTOBITO, CAUSABAS, SEXO, IDADE)

p1 <- filter(resultado, CAUSABAS == "Septicemia")

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


estatics_plots_num <- function(data) {
  # Verificar se a entrada é um dataframe
  if (!is.data.frame(data)) {
    stop("O argumento fornecido não é um dataframe.")
  }
  
  # Percorrer cada coluna do dataframe
  for (colname in names(data)) {
    column <- data[[colname]]
    
    # Verificar se a coluna é numérica
    if (is.numeric(column)) {
      cat("Estatísticas para a coluna:", colname, "\n")
      
      # Calcular e imprimir estatísticas descritivas
      summary_stats <- summary(column)
      print(summary_stats)
      
      # Criar histograma
      hist(column, main = paste("Histograma da coluna:", colname), xlab = colname, ylab = "Frequência")
      
      # Criar boxplot
      boxplot(column, main = paste("Boxplot da coluna:", colname), ylab = colname)
      
      cat("\n\n")  # Adicionar separador entre as colunas
    }
  }
}


