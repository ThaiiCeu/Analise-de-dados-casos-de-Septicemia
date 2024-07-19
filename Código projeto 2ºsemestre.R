
library(dplyr) # Para manipulação de dados
library(purrr) # Para aplicar funções em listas
library(microdatasus) # Para pegar todas as UF's do Brasil pela doença que estamos estudando (Septicemaia)
library(ggplot2)
library(stringr)


# Função para baixar e filtrar dados para um intervalo de anos e uma UF
fetch_and_filter_data <- function(start_year, end_year, uf) {
  # Lista para armazenar os dados de cada ano
  data_list <- list()
  
  # Códigos de septicemia
  septicemia_codes <- c("A021", "A227", "A267", "A327", "A40", "A400", "A401", "A402", "A403", "A408", 
                        "A409", "A41", "A410", "A411", "A412", "A413", "A414", "A415", "A418", "A419", 
                        "A427", "B377", "P36", "P360", "P361", "P362", "P363", "P364", "P365", "P368", 
                        "P369")
  
  for (year in start_year:end_year) {
    # Tenta baixar os dados para o ano e UF especificados
    data_sim <- tryCatch({
      fetch_datasus(year_start = as.numeric(year),  # Garante que year_start é numérico
                    year_end = as.numeric(year),    # Garante que year_end é numérico
                    uf = uf,
                    information_system = "SIM-DO")
    }, error = function(e) {
      message(sprintf("Erro ao baixar dados para o ano %d e UF %s: %s", year, uf, e$message))
      NULL
    })
    
    # Verifica se os dados foram baixados com sucesso
    if (!is.null(data_sim)) {
      # Filtra os dados para incluir apenas os códigos de septicemia presentes na base
      data_filtered <- data_sim %>%
        filter(CAUSABAS %in% septicemia_codes)
      
      # Adiciona os dados filtrados à lista
      data_list[[as.character(year)]] <- data_filtered
    } else {
      # Adiciona um aviso ao resultado se não houver dados
      message(sprintf("Nenhum dado disponível para o ano %d e UF %s", year, uf))
      data_list[[as.character(year)]] <- tibble()  # Adiciona um data frame vazio para garantir que todos os anos sejam considerados
    }
  }
  
  # Combina todos os dados filtrados em um único data frame
  data_combined <- bind_rows(data_list, .id = "year")
  
  return(data_combined)
}

# Ele não filtrou todos os anos, alguns ele deixou para trás, pois estava dando erro de conectividade com o a plataforma do SUS
# Caso for usar o código para pegar os dados específicos, prestar atenção nos possívels erros
# No meu caso, ele não pegou os dados de SE2017, SC2022, RS2017, PI2016, MS2017, MG2017, DF2016 e BA2016.
# Lembrando que, no meu caso, ele não pegou essas bases, mas no seu caso, ele pode não pegar outras bases.
# Fique atento!


# Lista com todas as UFs
ufs <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO")

# Intervalo de anos
start_year <- 2015
end_year <- 2022

# Baixando e filtrando os dados para cada UF
data_list <- map(ufs, ~ fetch_and_filter_data(start_year, end_year, .x))

data_combined <- bind_rows(data_list, .id = "UF")

#Encontrando o arquivo

setwd("C:/Users/luiz1/OneDrive/Área de Trabalho/Projeto 2")
DO_BR_SEPT <- read.csv('dados_septicemia_2015_2022_BR.csv')

#Arrumando data
adicionar_zero <- function(valor) {
  if (nchar(as.character(valor)) == 7) {
    paste0("0", valor)
  } else {
    as.character(valor)
  }
}

DO_BR_SEPT$DTOBITO <- as.Date(sapply(DO_BR_SEPT$DTOBITO, function(x) {
  ifelse(nchar(as.character(x)) == 7, adicionar_zero(x), as.character(x))
}), format = "%d%m%Y")

DO_BR_SEPT <- DO_BR_SEPT %>% filter(SEXO %in% c(1, 2))

SEPT = str_sub(DO_BR_SEPT$CAUSABAS, end = 1)
SEPT = recode(SEPT, "A" = "Septicemia" , "B" = "Septicemia" , "P" = "Septicemia" )


SEPT_tab = table(DO_BR_SEPT$CAUSABAS)

DO_BR_SEPT$CAUSABAS = str_sub(DO_BR_SEPT$CAUSABAS, end = 1)
DO_BR_SEPT$CAUSABAS = recode(DO_BR_SEPT$CAUSABAS, "A" = "Septicemia" , "B" = "Septicemia" , "P" = "Septicemia" )

SEPT_tab_2 = table(DO_BR_SEPT$CAUSABAS)

barplot(table(DO_BR_SEPT$IDADE))
table(DO_BR_SEPT$SEXO)


#Começando o gráfico de séries
colunas <- names(SEPT_tab_2)
resultado <- filter(DO_BR_SEPT, CAUSABAS %in% colunas) %>%
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
  ggtitle("Série Temporal de Óbitos por Septicemia no Brasil") +
  scale_color_discrete(name = "Sexo", labels = c("1" = "Masculino", "2" = "Feminino")) +
  theme_minimal()

Septicemia
######################################################################

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

