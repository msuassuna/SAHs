rm(list = ls())

# Carrega todos os pacotes que serão utilizados
library(downloader)
library(tidyr)
library(readr)
library(dplyr)
library(XML)
library(plotly)
library(stringr)
library(lubridate)
library(RCurl)
library(png)
library(ggplot2);cat('\014')

# Lê arquivos com as informações das estações que serão usadas no script
Ests <- read_csv2("G:/Alertas/tabelas/Est_SACE.csv",
                  locale = readr::locale(encoding = "latin1")) %>%
  filter(!duplicated(Codigo)) %>%
  filter(str_detect(Codigo, "^6|^13|^15|^14")) %>%
  filter(!(Codigo %in% c("13439000")))
Codigos <- Ests$Codigo
Nomes <- Ests$Nome

# Funções dos modelos de previsão
source("G:/Alertas/scripts/funcoes.R")
source("G:/Alertas/scripts/modelos.R")
source("G:/Alertas/scripts/curvasChave.R")

referencia <- read_csv2("G:/Alertas/tabelas/referencia.csv", locale = readr::locale(encoding = "latin1"))

#################
#################
# Baixa dados Hidrológicos, histórico e telemétricos
#################
#################

# Data a partir da qual os dados telemétricos serão baixados
DataInic <- paste0("01/01/",year(Sys.Date()))

# Baixa dados do HidroWeb
source("G:/Alertas/scripts/downloadHidroweb.R")

# Estações que têm PCD, mas que vamos usar dados de cota online
Manual <- c(66825000, 67100000)

eval(parse("G:/Alertas/scripts/leTelemetria.R", encoding = "UTF-8"))

# Correções manuais
source("G:/Alertas/scripts/correcoes.R")

source("G:/Alertas/scripts/tabelaSumario.R")

########################################################################
########################################################################
##
## Faz o gráfico com a médias máximas e mínimas diárias
## Resulta desse bloco: figuras com variação sazonal dos níveis
##
########################################################################
########################################################################

Cota_Passado <- array(NA, nrow(Tab_Boletim))
Cota_Maxima <- array(NA, nrow(Tab_Boletim))
Cota_Mediana <- array(NA, nrow(Tab_Boletim))
Ano_Maxima <- array(NA, nrow(Tab_Boletim))
SPI <- array(NA, nrow(Tab_Boletim))

# Essa função apaga todas as figuras que estão na pasta. Atenção!
unlink(paste0("G:/Alertas/figuras/sazonal/", dir("G:/Alertas/figuras/sazonal")))
unlink(paste0("D:/Drives compartilhados/SAH Brasil/figuras/sazonal/",
              dir("D:/Drives compartilhados/SAH Brasil/figuras/sazonal")))

# est <- which(Codigos == "14710000")
for(est in 1:length(Codigos)){
  
  if(Codigos[est] %in% c("13439000","13572000","76900000","50660000",
                         "14528000","47771000","49031000", "13568000",
                         "66125000","66945000","66900000","66960008",
                         "66600000","66470000",
                         "15700000","15627000")) next

  Estacao <- Codigos[est]
  Nome <- Nomes[est]
  
  eval(parse("G:/Alertas/scripts/graficoSazonal.R", encoding = "UTF-8"))
  
  print(paste(Nome,Estacao))
}

Tab_Boletim$Passado <- Cota_Passado
Tab_Boletim$C_Max <- Cota_Maxima
Tab_Boletim$Ano_Max <- Ano_Maxima
Tab_Boletim$Mediana <- Cota_Mediana
Tab_Boletim$SPI <- SPI

# Tabela SAH Acre
write.csv2(Tab_Boletim %>%
             filter(Estacao %in% c("13600002","13550000","13470000","13450000","13568000","13578000")),
           "G:/Alertas/tabelaSaida/Tabela_Acre.csv", row.names = FALSE)

write.csv2(Tab_Boletim %>%
             filter(Estacao %in% c("13600002","13550000","13470000","13450000","13568000","13578000")),
           "D:/Drives compartilhados/SAH Brasil/tabelas/Tabela_Acre.csv", row.names = FALSE)

# Tabela SAH Madeira
Coordenadas <- read.csv2("G:/Alertas/tabelas/streamGauges_PV.csv") %>%
  dplyr::select(Site, lat, lon, DA)
target <- c("15400000", "15250000", "15318000", "15326000",
            "15560000", "15630000")

Coordenadas <- merge(Coordenadas, Tab_Boletim %>% 
                       filter(Estacao %in% c("15400000","15250000","15318000",
                                             "15326000","15630000",
                                             "15560000","15430000","15200000")) %>%
                       filter(complete.cases(Horario_Brasilia)), all.y = TRUE,
                     by.x = "Site", by.y = "Estacao") %>%
  arrange(factor(Site, levels = target))

write.csv2(Coordenadas,
           "G:/Alertas/tabelaSaida/Tabela_Madeira.csv", row.names = FALSE, na = "")

write.csv2(Coordenadas,
           "D:/Drives compartilhados/SAH Brasil/tabelas/Tabela_Madeira.csv", row.names = FALSE, na = "")

# Tabela SAH Branco
target <- c("BOA VISTA", "CARACARAI","MARACA","FAZENDA CAJUPIRANGA",
            "FAZENDA PASSARAO","PONTE DO TACUTU","VILA SURUMU",
            "FE E ESPERANCA","MUCAJAI")

write.csv2(Tab_Boletim %>% 
             filter(str_detect(Estacao, "^14")) %>%
             arrange(factor(Nome, levels = target)) %>%
             filter(complete.cases(Horario_Brasilia)),
           "D:/Drives compartilhados/SAH Brasil/tabelas/Tabela_Branco.csv", row.names = FALSE, na = "")

write.csv2(Tab_Boletim %>% 
             filter(str_detect(Estacao, "^14")) %>%
             arrange(factor(Nome, levels = target)) %>%
             filter(complete.cases(Horario_Brasilia)),
           "G:/Alertas/tabelaSaida/Tabela_Branco.csv", row.names = FALSE, na = "")

# Tabela SAH Paraguai
Coordenadas <- read.csv("G:/Alertas/tabelas/streamGauges.csv") %>%
  dplyr::select(Site, lat, lon, DA)

Coordenadas <- merge(Coordenadas, Tab_Boletim %>% 
                       filter(str_detect(Estacao, "^6")) %>%
                       arrange(Estacao) %>%
                       filter(complete.cases(Horario_Brasilia)), all.y = TRUE,
                     by.x = "Site", by.y = "Estacao")

write.csv2(Coordenadas,
           "D:/Drives compartilhados/SAH Brasil/tabelas/Tabela_Paraguai.csv", row.names = FALSE, na = "")

write.csv2(Coordenadas,
           "G:/Alertas/tabelaSaida/Tabela_Paraguai.csv", row.names = FALSE, na = "")

########################################################################
########################################################################
##
## A partir daqui, as previsões são feitas.
## Modelos Cota-cota
##
########################################################################
########################################################################

unlink(paste0("G:/Alertas/figuras/previsoes/",
              dir("G:/Alertas/figuras/previsoes")))

unlink(paste0("D:/Drives compartilhados/SAH Brasil/figuras/previsoes/",
              dir("D:/Drives compartilhados/SAH Brasil/figuras/previsoes")))


#####
#
# Roda Previsão Ladário
#
#####
est <- which(Codigos == 66825000)
H <- 28
k <- 20
eval(parse("G:/Alertas/scripts/cota_LADARIO.R", encoding = "UTF-8"))
Anos
round(Prev_Ladario)

#####
#
# Roda Previsão Porto Murtinho
#
#####
est <- which(Codigos == 67100000)
H <- 28
k <- 20
eval(parse("G:/Alertas/scripts/cota_PORTOMURTINHO.R", encoding = "UTF-8"))
Anos
round(Prev_PMurtinho)

#####
#
# Roda Previsão Forte Coimbra
#
#####
est <- which(Codigos == 66970000)
H <- 28
k <- 20
eval(parse("G:/Alertas/scripts/cota_FORTECOIMBRA.R", encoding = "UTF-8"))
Anos
round(Prev_FCoimbra)

#####
#
# Roda Previsão Cáceres
#
#####
est <- which(Codigos == 66070004)
H <- 14
k <- 20
eval(parse("G:/Alertas/scripts/cota_CACERES.R", encoding = "UTF-8"))
Anos
round(Prev_Caceres)

Previsoes <- rbind(Prev_Caceres,
                   Prev_Ladario,
                   Prev_FCoimbra,
                   Prev_PMurtinho) %>%
  round() %>%
  as.data.frame()
names(Previsoes) <- c("Dia + 7","Dia + 14","Dia + 21","Dia + 28")
write.csv2(Previsoes, "G:/Alertas/tabelaSaida/Previsoes_Pantanal.csv", na = "-")
write.csv2(Previsoes, "D:/Drives compartilhados/SAH Brasil/tabelas/Previsoes_Pantanal.csv", na = "-")

#####
#
# Roda Cota-Cota CARACARAÍ
#
#####
est <- which(Codigos == 14710000)
est2 <- which(Codigos == 14620000)
Dias_eixo_x <- 15
eval(parse("G:/Alertas/scripts/cota_cota_CARACARAI.R", encoding = "UTF-8"))
tail(prev_CRC_36[complete.cases(prev_CRC_36)],1)

#####
#
# Roda Cota-Cota BOA VISTA
#
#####
est <- which(Codigos == 14620000)
est2 <- which(Codigos == 14527000)
Dias_eixo_x <- 10
eval(parse("G:/Alertas/scripts/cota_cota_BOAVISTA.R", encoding = "UTF-8"))
tail(prev_BOAVISTA_17[complete.cases(prev_BOAVISTA_17)],1)

#####
#
# Roda Cota-Cota RIO BRANCO
#
#####
est <- which(Codigos == 13600002)
est2 <- which(Codigos == 13550000)
Dias_eixo_x <- 10
eval(parse("G:/Alertas/scripts/cota_cota_RBR.R", encoding = "UTF-8"))
tail(prev_RBR_6[complete.cases(prev_RBR_6)],1)
tail(prev_RBR_12[complete.cases(prev_RBR_12)],1)
tail(prev_RBR_24[complete.cases(prev_RBR_24)],1)
# source("funcoes_v2/ArquivosGoogle_RIOBRANCO.R")


#####
#
# Roda Cota-Cota XAPURI
#
#####
est <- which(Codigos == 13550000)
est2 <- which(Codigos == 13470000)
Dias_eixo_x <- 10
# Roda Cota-Cota Xapuri
eval(parse("G:/Alertas/scripts/cota_cota_XPU.R", encoding = "UTF-8"))
tail(prev_XPU_18[complete.cases(prev_XPU_18)],1)

#####
#
# Roda PORTO VELHO
#
#####

# Vazão-vazão em Porto Velho usando JJB e MorNova
est <- which(Codigos==15400000)
est2 <- which(Codigos==15318000)
est3 <- which(Codigos==15326000)
Dias_eixo_x <- 20
correcao <- 0
eval(parse("G:/Alertas/scripts/vazao_vazao_PVLH.R", encoding = "UTF-8"))
x <- curvaChave_QN_PV(tail(prev_30[complete.cases(prev_30)],1))
ultimoDado <- tail(Dados_List$Nivel.x, 1)
tendencia <- ifelse(x - ultimoDado > 5, "subida", ifelse(x - ultimoDado < -5, "descida", "estabilidade"))
hora <- round_date(Sys.time()+30*3600, "hour")
paste("Nível com tendência de",tendencia,"podendo atingir a cota de",round(x),"cm até às", format(hora,"%H"), "horas do dia", format(hora,"%d/%m"))

curvaChave_NQ_PV(1524)

# source("G:/Alertas/scripts/ArquivosGoogle_PORTOVELHO.R")



#####
#
# Roda Previsão GUAJARA-MIRIM
#
#####
est <- which(Codigos == 15250000)
H <- 4
k <- 20
Dias_eixo_x <- 10
eval(parse("G:/Alertas/scripts/cota_GUAJARAMIRIM.R", encoding = "UTF-8"))
Anos
round(Prev_Guajara)

#source("funcoes_v2/ArquivosGoogle_GUAJARAMIRIM.R")

########################################################################
########################################################################
##
## Resume chuvas Pantanal
##
########################################################################
########################################################################

###
# Alerta Pantanal
###

eval(parse("G:/Alertas/scripts/balancoPantanal.R", encoding = "UTF-8"))



########################################################################
########################################################################
##
## Upload para o GitHub
##
########################################################################
########################################################################

library(httr)
library(openssl)

# Set your personal access token and repository details
access_token <- "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"
repo_owner <- "msuassuna"
repo_name <- "SAHs"

# Create the repository
#create_repo <- function() {
#  url <- paste0("https://api.github.com/msuassuna")
  
#  body <- list(name = repo_name)
#  headers <- c(Authorization = paste("Bearer", access_token))
  
#  response <- POST(url, body = body, add_headers(headers))
  
#  if (response$status_code == 201) {
#    print("Repository created successfully.")
#  } else {
#    stop("Error creating repository.")
#  }
#}

#create_repo()

#all_plot_file <- dir("G:/Alertas/figuras/sazonal")

#i <- 1

#this_plot_file <- all_plot_file[i]

# Upload the plot file to GitHub
#upload_plot <- function() {
#  url <- paste0("https://api.github.com/repos/", repo_owner, "/", repo_name, "/contents/", this_plot_file)
  
#  plot_file <- paste0("G:/Alertas/figuras/sazonal/",this_plot_file)
  
#  body <- list(
#    path = plot_file,
#    message = "Upload plot",
#    content = base64_encode(readBin(plot_file, "raw")),
#    branch = "main"
#  )
  
#  headers <- c(Authorization = paste("Bearer", access_token))
  
#  response <- PUT(url, body = body, add_headers(headers))
  
#  if (response$status_code == 201) {
#    print("Plot uploaded successfully.")
#  } else {
#    stop("Error uploading plot.")
#  }
#}

#upload_plot()

# Push changes to the repository
#push_changes <- function() {
#  url <- paste0("https://api.github.com/repos/", repo_owner, "/", repo_name, "/git/refs")
  
#  body <- list(ref = "refs/heads/main", sha = "master")
#  headers <- c(Authorization = paste("Bearer", access_token))
  
#  response <- POST(url, body = body, add_headers(headers))
  
#  if (response$status_code == 201) {
#    print("Changes pushed successfully.")
#  } else {
#    stop("Error pushing changes.")
#  }
#}

#push_changes()
