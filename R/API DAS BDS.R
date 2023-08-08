library(tidyverse)
library(lubridate)
library(dplyr)
library(readr)
library(patchwork)
library(leaflet)
library(geobr)
library(readxl)
library(sf)
library(rgdal)
library(httr)
library(jsonlite)

############################################

URL_BASE <- "http://siatfm.cgna.intraer/api/performance?apiUser=silveiralss&apiToken=7a11719421134986058a25ea58a3b002e2c185d5&area="

BDS_KPI_01 <- GET("http://siatfm.cgna.intraer/api/performance?apiUser=silveiralss&apiToken=7a11719421134986058a25ea58a3b002e2c185d5&area=KPI_01")
BDS_KPI_01_TEXT <- content(BDS_KPI_01, "text")
BDS_KPI_01_JSON <- fromJSON(BDS_KPI_01_TEXT,flatten = TRUE)
BDS_KPI_01_DF <- as.data.frame(BDS_KPI_01_JSON) %>%
  select(movimentos.ano,movimentos.percentual_ano, movimentos.regiao,movimentos.aero,movimentos.jan:movimentos.dez) %>% 
  mutate(movimentos.percentual_ano=movimentos.percentual_ano, movimentos.jan=movimentos.jan,
         movimentos.fev=movimentos.fev, movimentos.mar=movimentos.mar,
         movimentos.abr=movimentos.abr, movimentos.mai=movimentos.mai,
         movimentos.jun=movimentos.jun, movimentos.jul=movimentos.jul,
         movimentos.ago=movimentos.ago, movimentos.set=movimentos.set,
         movimentos.out=movimentos.out, movimentos.nov=movimentos.nov,
         movimentos.dez=movimentos.dez)
COL_NAMES_KPI01 <- c("ANO", "PONTUALIDADE_ANO","REGIONAL","AERO","JAN","FEV","MAR","ABR", "MAI", "JUN", "JUL", "AGO", "SET", "OUT", "NOV", "DEZ")

colnames(BDS_KPI_01_DF) <- COL_NAMES_KPI01

######################

BDS_KPI_01_REGIONAL <- GET("http://siatfm.cgna.intraer/api/performance?apiUser=silveiralss&apiToken=7a11719421134986058a25ea58a3b002e2c185d5&area=KPI_01&regional=s")
BDS_KPI_01_TEXT_REGIONAL <- content(BDS_KPI_01_REGIONAL, "text")
BDS_KPI_01_JSON_REGIONAL <- fromJSON(BDS_KPI_01_TEXT_REGIONAL,flatten = TRUE)
BDS_KPI_01_REGIONAL_DF <- as.data.frame(BDS_KPI_01_JSON_REGIONAL)







########

BDS_KPI_02 <- GET("http://siatfm.cgna.intraer/api/performance?apiUser=silveiralss&apiToken=7a11719421134986058a25ea58a3b002e2c185d5&area=KPI_02")
BDS_KPI_02_TEXT <- content(BDS_KPI_02, "text")
BDS_KPI_02_JSON <- fromJSON(BDS_KPI_02_TEXT,flatten = TRUE)
BDS_KPI_02_DF <- as.data.frame(BDS_KPI_02_JSON)

########

BDS_KPI_02_REGIONAL <- GET("http://siatfm.cgna.intraer/api/performance?apiUser=silveiralss&apiToken=7a11719421134986058a25ea58a3b002e2c185d5&area=KPI_02&regional=s")
BDS_KPI_02_REGIONAL_TEXT <- content(BDS_KPI_02_REGIONAL, "text")
BDS_KPI_02_REGIONAL_JSON <- fromJSON(BDS_KPI_02_REGIONAL_TEXT,flatten = TRUE)
BDS_KPI_02_REGIONAL_DF <- as.data.frame(BDS_KPI_02_REGIONAL_JSON)


#########

BDS_KPI_06 <- GET("http://siatfm.cgna.intraer/api/performance?apiUser=silveiralss&apiToken=7a11719421134986058a25ea58a3b002e2c185d5&area=KPI_06")
BDS_KPI_06_TEXT <- content(BDS_KPI_06, "text")
BDS_KPI_06_JSON <- fromJSON(BDS_KPI_06_TEXT,flatten = TRUE)
BDS_KPI_06_DF <- as.data.frame(BDS_KPI_06_JSON)

##########


BDS_IDBR_06_TWR <- GET("http://siatfm.cgna.intraer/api/performance?apiUser=silveiralss&apiToken=7a11719421134986058a25ea58a3b002e2c185d5&area=IDBR_06&regional=TWR")
BDS_IDBR_06_TWR_TEXT <- content(BDS_IDBR_06_TWR, "text")
BDS_IDBR_06_TWR_JSON <- fromJSON(BDS_IDBR_06_TWR_TEXT,flatten = TRUE)
BDS_IDBR_06_TWR_DF <- as.data.frame(BDS_IDBR_06_TWR_JSON)

############

BDS_IDBR_06_APP <- GET("http://siatfm.cgna.intraer/api/performance?apiUser=silveiralss&apiToken=7a11719421134986058a25ea58a3b002e2c185d5&area=IDBR_06&regional=APP")
BDS_IDBR_06_APP_TEXT <- content(BDS_IDBR_06_APP, "text")
BDS_IDBR_06_APP_JSON <- fromJSON(BDS_IDBR_06_APP_TEXT,flatten = TRUE)
BDS_IDBR_06_APP_DF <- as.data.frame(BDS_IDBR_06_APP_JSON)

############

BDS_IDBR_06_ACC <- GET("http://siatfm.cgna.intraer/api/performance?apiUser=silveiralss&apiToken=7a11719421134986058a25ea58a3b002e2c185d5&area=IDBR_06&regional=ACC")
BDS_IDBR_06_ACC_TEXT <- content(BDS_IDBR_06_ACC, "text")
BDS_IDBR_06_ACC_JSON <- fromJSON(BDS_IDBR_06_ACC_TEXT,flatten = TRUE)
BDS_IDBR_06_ACC_DF <- as.data.frame(BDS_IDBR_06_ACC_JSON)


##########







BDS_KPI_08_C40 <- GET("http://siatfm.cgna.intraer/api/performance?apiUser=silveiralss&apiToken=7a11719421134986058a25ea58a3b002e2c185d5&area=KPI_08_C40")
BDS_KPI_08_C40_TEXT <- content(BDS_KPI_08_C40, "text")
BDS_KPI_08_C40_JSON <- fromJSON(BDS_KPI_08_C40_TEXT,flatten = TRUE)
BDS_KPI_08_C40_DF <- as.data.frame(BDS_KPI_08_C40_JSON)%>%
  select(movimentos.ano,movimentos.percentual_ano, movimentos.regiao,movimentos.aero,movimentos.jan:movimentos.dez)

COL_NAMES_KPI08_c40 <- c("ANO", "PONTUALIDADE_ANO","REGIONAL","AERO","JAN","FEV","MAR","ABR", "MAI", "JUN", "JUL", "AGO", "SET", "OUT", "NOV", "DEZ")

colnames(BDS_KPI_08_C40_DF) <- COL_NAMES_KPI08_c40

##########

BDS_KPI_08_C100 <- GET("http://siatfm.cgna.intraer/api/performance?apiUser=silveiralss&apiToken=7a11719421134986058a25ea58a3b002e2c185d5&area=KPI_08_C100")
BDS_KPI_08_C100_TEXT <- content(BDS_KPI_08_C100, "text")
BDS_KPI_08_C100_JSON <- fromJSON(BDS_KPI_08_C100_TEXT,flatten = TRUE)
BDS_KPI_08_C100_DF <- as.data.frame(BDS_KPI_08_C100_JSON)%>%
  select(movimentos.ano,movimentos.percentual_ano, movimentos.regiao,movimentos.aero,movimentos.jan:movimentos.dez)

COL_NAMES_KPI08_c100 <- c("ANO", "PONTUALIDADE_ANO","REGIONAL","AERO","JAN","FEV","MAR","ABR", "MAI", "JUN", "JUL", "AGO", "SET", "OUT", "NOV", "DEZ")

colnames(BDS_KPI_08_C100_DF) <- COL_NAMES_KPI08_c100

##########
#UNIÃO DO KPI08 C40COM C100

BDS_KPI08_C40_C100 <- bind_cols(BDS_KPI_08_C40_DF$ANO,
                                BDS_KPI_08_C40_DF$REGIONAL,
                                BDS_KPI_08_C40_DF$AERO,
                                BDS_KPI_08_C40_DF$PONTUALIDADE_ANO,
                                BDS_KPI_08_C100_DF$PONTUALIDADE_ANO)

COL_NAMES_KPI08_c40_c100 <- c("ANO","REGIONAL","AERO", "PONTUALIDADE_ANO_C40",
                              "PONTUALIDADE_ANO_C100")

colnames(BDS_KPI08_C40_C100) <- COL_NAMES_KPI08_c40_c100
#########################################################
#BDS KPI 08 POR REGIONAL

BDS_KPI_08_C40_REGIONAL <- GET("http://siatfm.cgna.intraer/api/performance?apiUser=silveiralss&apiToken=7a11719421134986058a25ea58a3b002e2c185d5&area=KPI_08_C40&regional=s")
BDS_KPI_08_C40_REGIONAL_TEXT <- content(BDS_KPI_08_C40_REGIONAL, "text")
BDS_KPI_08_C40_REGIONAL_JSON <- fromJSON(BDS_KPI_08_C40_REGIONAL_TEXT,flatten = TRUE)
BDS_KPI_08_C40_REGIONAL_DF <- as.data.frame(BDS_KPI_08_C40_REGIONAL_JSON)%>%
  select(movimentos.ano,movimentos.percentual_ano, movimentos.regiao,movimentos.aero,movimentos.jan:movimentos.dez)

COL_NAMES_KPI08_c40_REGIONAL <- c("ANO", "PONTUALIDADE_ANO","REGIONAL","AERO","JAN","FEV","MAR","ABR", "MAI", "JUN", "JUL", "AGO", "SET", "OUT", "NOV", "DEZ")

colnames(BDS_KPI_08_C40_REGIONAL_DF) <- COL_NAMES_KPI08_c40_REGIONAL
################
BDS_KPI_08_C100_REGIONAL <- GET("http://siatfm.cgna.intraer/api/performance?apiUser=silveiralss&apiToken=7a11719421134986058a25ea58a3b002e2c185d5&area=KPI_08_C100&regional=s")
BDS_KPI_08_C100_REGIONAL_TEXT <- content(BDS_KPI_08_C100_REGIONAL, "text")
BDS_KPI_08_C100_REGIONAL_JSON <- fromJSON(BDS_KPI_08_C100_REGIONAL_TEXT,flatten = TRUE)
BDS_KPI_08_C100_REGIONAL_DF <- as.data.frame(BDS_KPI_08_C100_REGIONAL_JSON)%>%
  select(movimentos.ano,movimentos.percentual_ano, movimentos.regiao,movimentos.aero,movimentos.jan:movimentos.dez)

COL_NAMES_KPI08_c100_REGIONAL <- c("ANO", "PONTUALIDADE_ANO","REGIONAL","AERO","JAN","FEV","MAR","ABR", "MAI", "JUN", "JUL", "AGO", "SET", "OUT", "NOV", "DEZ")

colnames(BDS_KPI_08_C100_REGIONAL_DF) <- COL_NAMES_KPI08_c100_REGIONAL
#########################
#unindo o KPI 08 por REGIONAL

BDS_KPI08_C40_C100_REGIONAL <- bind_cols(BDS_KPI_08_C40_REGIONAL_DF$ANO,
                                         BDS_KPI_08_C40_REGIONAL_DF$REGIONAL,
                                         BDS_KPI_08_C40_REGIONAL_DF$AERO,
                                         BDS_KPI_08_C40_REGIONAL_DF$PONTUALIDADE_ANO,
                                         BDS_KPI_08_C100_REGIONAL_DF$PONTUALIDADE_ANO)

COL_NAMES_KPI08_c40_c100_REGIONAL <- c("ANO","REGIONAL","AERO", "PONTUALIDADE_ANO_C40",
                                       "PONTUALIDADE_ANO_C100")

colnames(BDS_KPI08_C40_C100_REGIONAL) <- COL_NAMES_KPI08_c40_c100_REGIONAL
BDS_KPI08_C40_C100_REGIONAL<- BDS_KPI08_C40_C100_REGIONAL %>% pivot_longer(cols = PONTUALIDADE_ANO_C40:PONTUALIDADE_ANO_C100,
                                                                           names_to = "C40_100",
                                                                           values_to = "ADICIONAL")



#BDS_KPI08_C40_C100_REGIONAL %>% mutate("40"="40", "100"="100") %>% 
#  unite()



#########################################################
#foi necessário criar uma BDS mensal para o KPI 08



BDS_KPI_08_C40_MES <- BDS_KPI_08_C40_DF %>% 
  pivot_longer(cols = JAN:DEZ,
               names_to = "MES",
               values_to = "ADICIONAL") %>% 
  mutate("XXX"= "40") %>% 
  unite("AERO", c("AERO","XXX"))

BDS_KPI_08_C100_MES <- BDS_KPI_08_C100_DF %>% 
  pivot_longer(cols = JAN:DEZ,
               names_to = "MES",
               values_to = "ADICIONAL")%>% 
  mutate("XXX"= "100") %>% 
  unite("AERO", c("AERO","XXX"))


BDS_KPI_08_MES <- bind_rows(BDS_KPI_08_C40_MES,BDS_KPI_08_C100_MES)









##########

BDS_KPI_09 <- GET("http://siatfm.cgna.intraer/api/performance?apiUser=silveiralss&apiToken=7a11719421134986058a25ea58a3b002e2c185d5&area=KPI_09")
BDS_KPI_09_TEXT <- content(BDS_KPI_09, "text")
BDS_KPI_09_JSON <- fromJSON(BDS_KPI_09_TEXT,flatten = TRUE)
BDS_KPI_09_DF <- as.data.frame(BDS_KPI_09_JSON)

##########

BDS_KPI_10 <- GET("http://siatfm.cgna.intraer/api/performance?apiUser=silveiralss&apiToken=7a11719421134986058a25ea58a3b002e2c185d5&area=KPI_10")
BDS_KPI_10_TEXT <- content(BDS_KPI_10, "text")
BDS_KPI_10_JSON <- fromJSON(BDS_KPI_10_TEXT,flatten = TRUE)
BDS_KPI_10_DF <- as.data.frame(BDS_KPI_10_JSON)

##########

BDS_KPI_11 <- GET("http://siatfm.cgna.intraer/api/performance?apiUser=silveiralss&apiToken=7a11719421134986058a25ea58a3b002e2c185d5&area=KPI_11")
BDS_KPI_11_TEXT <- content(BDS_KPI_11, "text")
BDS_KPI_11_JSON <- fromJSON(BDS_KPI_11_TEXT,flatten = TRUE)
BDS_KPI_11_DF <- as.data.frame(BDS_KPI_11_JSON)

##########

BDS_KPI_13 <- GET("http://siatfm.cgna.intraer/api/performance?apiUser=silveiralss&apiToken=7a11719421134986058a25ea58a3b002e2c185d5&area=KPI_13")
BDS_KPI_13_TEXT <- content(BDS_KPI_13, "text")
BDS_KPI_13_JSON <- fromJSON(BDS_KPI_13_TEXT,flatten = TRUE)
BDS_KPI_13_DF <- as.data.frame(BDS_KPI_13_JSON)

##########
BDS_KPI_13_REGIONAL <- GET("http://siatfm.cgna.intraer/api/performance?apiUser=silveiralss&apiToken=7a11719421134986058a25ea58a3b002e2c185d5&area=KPI_13&regional=s")
BDS_KPI_13_REGIONAL_TEXT <- content(BDS_KPI_13_REGIONAL, "text")
BDS_KPI_13_REGIONAL_JSON <- fromJSON(BDS_KPI_13_REGIONAL_TEXT,flatten = TRUE)
BDS_KPI_13_REGIONAL_DF <- as.data.frame(BDS_KPI_13_REGIONAL_JSON)

##########

BDS_KPI_14 <- GET("http://siatfm.cgna.intraer/api/performance?apiUser=silveiralss&apiToken=7a11719421134986058a25ea58a3b002e2c185d5&area=KPI_14")
BDS_KPI_14_TEXT <- content(BDS_KPI_14, "text")
BDS_KPI_14_JSON <- fromJSON(BDS_KPI_14_TEXT,flatten = TRUE)
BDS_KPI_14_DF <- as.data.frame(BDS_KPI_14_JSON)%>%
  select(movimentos.ano,movimentos.percentual_ano, movimentos.regiao,movimentos.aero,movimentos.jan:movimentos.dez) %>% 
  mutate(movimentos.percentual_ano=movimentos.percentual_ano, movimentos.jan=movimentos.jan,
         movimentos.fev=movimentos.fev, movimentos.mar=movimentos.mar,
         movimentos.abr=movimentos.abr, movimentos.mai=movimentos.mai,
         movimentos.jun=movimentos.jun, movimentos.jul=movimentos.jul,
         movimentos.ago=movimentos.ago, movimentos.set=movimentos.set,
         movimentos.out=movimentos.out, movimentos.nov=movimentos.nov,
         movimentos.dez=movimentos.dez)
COL_NAMES_KPI14 <- c("ANO", "PONTUALIDADE_ANO","REGIONAL","AERO","JAN","FEV","MAR","ABR", "MAI", "JUN", "JUL", "AGO", "SET", "OUT", "NOV", "DEZ")

colnames(BDS_KPI_14_DF) <- COL_NAMES_KPI14
####################

PONT_ARR_DEP <- bind_cols(BDS_KPI_14_DF$ANO,BDS_KPI_14_DF$AERO,BDS_KPI_14_DF$PONTUALIDADE_ANO,BDS_KPI_01_DF$PONTUALIDADE_ANO)
COL_NAMES_ARR_DEP <- c("ANO","AERO", "PONT.ARR","PONT.DEP")
colnames(PONT_ARR_DEP) <- COL_NAMES_ARR_DEP

PONT_ARR_DEP <- PONT_ARR_DEP %>% filter(ANO==year(Sys.Date()))


##########

BDS_KPI_15 <- GET("http://siatfm.cgna.intraer/api/performance?apiUser=silveiralss&apiToken=7a11719421134986058a25ea58a3b002e2c185d5&area=KPI_15")
BDS_KPI_15_TEXT <- content(BDS_KPI_15, "text")
BDS_KPI_15_JSON <- fromJSON(BDS_KPI_15_TEXT,flatten = TRUE)
BDS_KPI_15_DF <- as.data.frame(BDS_KPI_15_JSON)

##########

BDS_KPI_TOTAL_AZ <- GET("http://siatfm.cgna.intraer/api/performance?apiUser=silveiralss&apiToken=7a11719421134986058a25ea58a3b002e2c185d5&area=Total&regional=SBAZ")
BDS_KPI_TOTAL_AZ_TEXT <- content(BDS_KPI_TOTAL_AZ, "text")
BDS_KPI_TOTAL_AZ_JSON <- fromJSON(BDS_KPI_TOTAL_AZ_TEXT,flatten = TRUE)
BDS_KPI_TOTAL_AZ_DF <- as.data.frame(BDS_KPI_TOTAL_AZ_JSON)

##########

BDS_KPI_TOTAL_AO <- GET("http://siatfm.cgna.intraer/api/performance?apiUser=silveiralss&apiToken=7a11719421134986058a25ea58a3b002e2c185d5&area=Total&regional=SBAO")
BDS_KPI_TOTAL_AO_TEXT <- content(BDS_KPI_TOTAL_AO, "text")
BDS_KPI_TOTAL_AO_JSON <- fromJSON(BDS_KPI_TOTAL_AO_TEXT,flatten = TRUE)
BDS_KPI_TOTAL_AO_DF <- as.data.frame(BDS_KPI_TOTAL_AO_JSON)

##########

BDS_KPI_TOTAL_BS <- GET("http://siatfm.cgna.intraer/api/performance?apiUser=silveiralss&apiToken=7a11719421134986058a25ea58a3b002e2c185d5&area=Total&regional=SBBS")
BDS_KPI_TOTAL_BS_TEXT <- content(BDS_KPI_TOTAL_BS, "text")
BDS_KPI_TOTAL_BS_JSON <- fromJSON(BDS_KPI_TOTAL_BS_TEXT,flatten = TRUE)
BDS_KPI_TOTAL_BS_DF <- as.data.frame(BDS_KPI_TOTAL_BS_JSON)

##########

BDS_KPI_TOTAL_CW <- GET("http://siatfm.cgna.intraer/api/performance?apiUser=silveiralss&apiToken=7a11719421134986058a25ea58a3b002e2c185d5&area=Total&regional=SBCW")
BDS_KPI_TOTAL_CW_TEXT <- content(BDS_KPI_TOTAL_CW, "text")
BDS_KPI_TOTAL_CW_JSON <- fromJSON(BDS_KPI_TOTAL_CW_TEXT,flatten = TRUE)
BDS_KPI_TOTAL_CW_DF <- as.data.frame(BDS_KPI_TOTAL_CW_JSON)

##########

BDS_KPI_TOTAL_RE <- GET("http://siatfm.cgna.intraer/api/performance?apiUser=silveiralss&apiToken=7a11719421134986058a25ea58a3b002e2c185d5&area=Total&regional=SBRE")
BDS_KPI_TOTAL_RE_TEXT <- content(BDS_KPI_TOTAL_RE, "text")
BDS_KPI_TOTAL_RE_JSON <- fromJSON(BDS_KPI_TOTAL_RE_TEXT,flatten = TRUE)
BDS_KPI_TOTAL_RE_DF <- as.data.frame(BDS_KPI_TOTAL_RE_JSON)

##########

BDS_KPI_TOTAL_Brasil <- GET("http://siatfm.cgna.intraer/api/performance?apiUser=silveiralss&apiToken=7a11719421134986058a25ea58a3b002e2c185d5&area=Total&regional=Brasil")
BDS_KPI_TOTAL_Brasil_TEXT <- content(BDS_KPI_TOTAL_Brasil, "text")
BDS_KPI_TOTAL_Brasil_JSON <- fromJSON(BDS_KPI_TOTAL_Brasil_TEXT,flatten = TRUE)
BDS_KPI_TOTAL_Brasil_DF <- as.data.frame(BDS_KPI_TOTAL_Brasil_JSON)

#########

BDS_KPI_MEDIA <- GET("http://siatfm.cgna.intraer/api/performance?apiUser=silveiralss&apiToken=7a11719421134986058a25ea58a3b002e2c185d5&area=Media")
BDS_KPI_MEDIA_TEXT <- content(BDS_KPI_MEDIA, "text")
BDS_KPI_MEDIA_JSON <- fromJSON(BDS_KPI_MEDIA_TEXT,flatten = TRUE)
BDS_KPI_MEDIA_DF <- as.data.frame(BDS_KPI_MEDIA_JSON)

######### 
#BDS PARA MOVIMENTOS DA AVIAÇÃO COMERCIAL DOMÉSTICA (01)

BDS_KPI_COMERCIAL_NAC <- GET("http://siatfm.cgna.intraer/api/performance?apiUser=silveiralss&apiToken=7a11719421134986058a25ea58a3b002e2c185d5&area=Comercial-Nac")
BDS_KPI_COMERCIAL_NAC_TEXT <- content(BDS_KPI_COMERCIAL_NAC, "text")
BDS_KPI_COMERCIAL_NAC_JSON <- fromJSON(BDS_KPI_COMERCIAL_NAC_TEXT,flatten = TRUE)
BDS_KPI_COMERCIAL_NAC_DF <- as.data.frame(BDS_KPI_COMERCIAL_NAC_JSON) %>% 
  select(movimentos.ano, movimentos.total,movimentos.jan:movimentos.dez)
COL.NAMES.MOV <- c("ANO", "TOTAL","JAN","FEV","MAR","ABR", "MAI", "JUN", "JUL", "AGO", "SET", "OUT", "NOV", "DEZ")
colnames(BDS_KPI_COMERCIAL_NAC_DF) <- COL.NAMES.MOV

#AJUSTES NA BDS PARA CRIAR O GRÁFICO
MOV_COM_DOMESTC <- BDS_KPI_COMERCIAL_NAC_DF %>% 
  pivot_longer(cols = JAN:DEZ,
               names_to = "MES",
               values_to = "MOV") %>% 
  mutate(MES=factor(MES,levels = c("JAN","FEV","MAR","ABR","MAI","JUN","JUL","AGO","SET","OUT","NOV","DEZ")), ANO=as.character(ANO))




#########
#BDS PARA MOVIMENTOS DA AVIAÇÃO COMERCIAL INTERNACIONAL (02)

BDS_KPI_COMERCIAL_INT <- GET("http://siatfm.cgna.intraer/api/performance?apiUser=silveiralss&apiToken=7a11719421134986058a25ea58a3b002e2c185d5&area=Comercial-Int")
BDS_KPI_COMERCIAL_INT_TEXT <- content(BDS_KPI_COMERCIAL_INT, "text")
BDS_KPI_COMERCIAL_INT_JSON <- fromJSON(BDS_KPI_COMERCIAL_INT_TEXT,flatten = TRUE)
BDS_KPI_COMERCIAL_INT_DF <- as.data.frame(BDS_KPI_COMERCIAL_INT_JSON)%>% 
  select(movimentos.ano, movimentos.total,movimentos.jan:movimentos.dez)
COL.NAMES.MOV <- c("ANO", "TOTAL","JAN","FEV","MAR","ABR", "MAI", "JUN", "JUL", "AGO", "SET", "OUT", "NOV", "DEZ")
colnames(BDS_KPI_COMERCIAL_INT_DF) <- COL.NAMES.MOV

#AJUSTES NA BDS PARA CRIAR O GRÁFICO
MOV_COM_INT <- BDS_KPI_COMERCIAL_INT_DF %>%
  pivot_longer(cols = JAN:DEZ,
               names_to = "MES",
               values_to = "MOV") %>% 
  mutate(MES=factor(MES,levels = c("JAN","FEV","MAR","ABR","MAI","JUN","JUL","AGO","SET","OUT","NOV","DEZ")), ANO=as.character(ANO))


#########

BDS_KPI_IDBR_06 <- GET("http://siatfm.cgna.intraer/api/performance?apiUser=silveiralss&apiToken=7a11719421134986058a25ea58a3b002e2c185d5&area=IDBR_06")
BDS_KPI_IDBR_06_TEXT <- content(BDS_KPI_IDBR_06, "text")
BDS_KPI_IDBR_06_JSON <- fromJSON(BDS_KPI_IDBR_06_TEXT,flatten = TRUE)
BDS_KPI_IDBR_06_DF <- as.data.frame(BDS_KPI_IDBR_06_JSON)

#########

BDS_KPI_MIN_ATRASO <- GET("http://siatfm.cgna.intraer/api/performance?apiUser=silveiralss&apiToken=7a11719421134986058a25ea58a3b002e2c185d5&area=MIN_ATRASO")
BDS_KPI_MIN_ATRASO_TEXT <- content(BDS_KPI_MIN_ATRASO, "text")
BDS_KPI_MIN_ATRASO_JSON <- fromJSON(BDS_KPI_MIN_ATRASO_TEXT,flatten = TRUE)
BDS_KPI_MIN_ATRASO_DF <- as.data.frame(BDS_KPI_MIN_ATRASO_JSON)

#########

BDS_KPI_DENSIDADE <- GET("http://siatfm.cgna.intraer/api/performance?apiUser=silveiralss&apiToken=7a11719421134986058a25ea58a3b002e2c185d5&area=DENSIDADE")
BDS_KPI_DENSIDADE_TEXT <- content(BDS_KPI_DENSIDADE, "text")
BDS_KPI_DENSIDADE_JSON <- fromJSON(BDS_KPI_DENSIDADE_TEXT,flatten = TRUE)
BDS_KPI_DENSIDADE_DF <- as.data.frame(BDS_KPI_DENSIDADE_JSON)


#####################################################################################################################################################
cores <- c("#B0C4DE",
           "#708090",
           "#4682B4",
           "#191970")

#######################################
#UNIÃO DAS BDS TOTAL(POR FIR) PARA CIAÇÃO DO GRÁFICO "TOTAL DE MOVIMENTOS SISCEAB E POR FIR" (03)


TOTAL_MOV_SISC_FIR <- bind_rows(BDS_KPI_TOTAL_Brasil_DF, 
                                BDS_KPI_TOTAL_BS_DF, 
                                BDS_KPI_TOTAL_CW_DF, 
                                BDS_KPI_TOTAL_RE_DF, 
                                BDS_KPI_TOTAL_AZ_DF, 
                                BDS_KPI_TOTAL_AO_DF) %>% 
  select(movimentos.fir, movimentos.ano, movimentos.total) %>%
  filter(movimentos.ano!='2018')

#MUDANDO O NOME DAS COLUNAS
COL.NAMES.TOTAL <- c("FIR","ANO", "TOTAL") 
colnames(TOTAL_MOV_SISC_FIR) <- COL.NAMES.TOTAL

#AJUSTES NA BDS PARA CRIAR O GRÁFICO
TOTAL_MOV_SISC_FIR <- TOTAL_MOV_SISC_FIR %>%
  mutate(FIR = case_when(FIR == "BRASIL" ~ "SISCEAB",
                         FIR == "SBBS" ~ "FIR Brasília",
                         FIR == "SBCW" ~ "FIR Curitiba",
                         FIR == "SBRE" ~ "FIR Recife",
                         FIR == "SBAZ" ~ "FIR Amazônica",
                         FIR == "SBAO" ~ "FIR Atlântico"), ANO=as.character(ANO)) %>%
  mutate(FIR= factor(FIR))


#########################################

#BDS PARA CRIAÇÃO DO GRÁFICO DE MOVIMENTOS DO ANO POR MES SISCEAB (04)


TOTAL_MOV_MES_SISCEAB <- BDS_KPI_TOTAL_Brasil_DF %>%
  select(movimentos.ano,movimentos.total:movimentos.dez) %>% 
  filter(movimentos.ano!='2018')
colnames(TOTAL_MOV_MES_SISCEAB) <- COL.NAMES.MOV

#AJUSTES NA BDS PARA CRIAR O GRÁFICO
TOTAL_MES_SISCEAB <- TOTAL_MOV_MES_SISCEAB %>%
  pivot_longer(cols = JAN:DEZ,
               names_to = "MES",
               values_to = "MOV") %>% 
  mutate(MES=factor(MES,levels = c("JAN","FEV","MAR","ABR","MAI","JUN","JUL","AGO","SET","OUT","NOV","DEZ")), ANO=as.character(ANO))



#########################################
# BDS PARA CRIAÇÃO DO GRÁFICO DE DENSIDADE DO TRÁFEGO POR REGIONAL(05)

BDS_DENSIDADE <- BDS_KPI_DENSIDADE_DF %>% 
  filter(movimentos.ano == year(Sys.Date())-1) %>% 
  separate(movimentos.chave, into = c("FIR","CHAVE"), sep = "-") %>% 
  select(FIR, movimentos.ano,movimentos.percentual_ano, movimentos.jan:movimentos.dez) %>% 
  mutate(FIR = case_when(FIR == "BRASIL" ~ "SISCEAB",
                         FIR == "SBBS" ~ "CINDACTA I",
                         FIR == "SBCW" ~ "CINDACTA II",
                         FIR == "SBRE" ~ "CINDACTA III",
                         FIR == "SBAZ" ~ "CINDACTA IV",
                         FIR == "SBAO" ~ "FIR Atlântico",
                         FIR == "CRCEA" ~ "CRCEA-SE"))

COL.NAMES.DENSI <- c("FIR","ANO", "PERCENT.ANO","JAN","FEV","MAR","ABR", "MAI", "JUN", "JUL", "AGO", "SET", "OUT", "NOV", "DEZ")
colnames(BDS_DENSIDADE) <- COL.NAMES.DENSI

DENSIDADE_C1 <- BDS_DENSIDADE[5,3]
DENSIDADE_C2 <- BDS_DENSIDADE[6,3]
DENSIDADE_C3 <- BDS_DENSIDADE[7,3]
DENSIDADE_C4 <- BDS_DENSIDADE[4,3]
DENSIDADE_SISCEAB <- BDS_DENSIDADE[2,3]
DENSIDADE_CRCEA <- BDS_DENSIDADE[1,3]

##########################################
#LAT LONG DOS MAPAS

VERTICES_SBCW <- read_excel("C:/Users/silveiralss/OneDrive - DECEA/Documentos/Relatório/Relatório/data/VERTICES_SBCW.xlsx") %>% 
  mutate(REGIONAL= "CINDACTA II", DENSIDADE= DENSIDADE_C2)
VERTICES_SBAZ <- read_excel("C:/Users/silveiralss/OneDrive - DECEA/Documentos/Relatório/Relatório/data/VERTICES_SBAZ.xlsx") %>% 
  mutate(REGIONAL= "CINDACTA IV", DENSIDADE=DENSIDADE_C4)
VERTICES_SBBS <- read_excel("C:/Users/silveiralss/OneDrive - DECEA/Documentos/Relatório/Relatório/data/VERTICES_SBBS.xlsx") %>% 
  mutate(REGIONAL="CINDACTA I", DENSIDADE=DENSIDADE_C1)
VERTICES_CRCEA_SE <- read_excel("C:/Users/silveiralss/OneDrive - DECEA/Documentos/Relatório/Relatório/data/VERTICES_CRCEA_SE.xlsx") %>% 
  mutate(REGIONAL="CRCEA-SE", DENSIDADE=DENSIDADE_CRCEA)
VERTICES_SBRE <- read_excel("C:/Users/silveiralss/OneDrive - DECEA/Documentos/Relatório/Relatório/data/VERTICES_SBRE.xlsx") %>% 
  mutate(REGIONAL="CINDACTA III", DENSIDADE=DENSIDADE_C3)

JURISDICAO <- bind_rows(VERTICES_SBAZ,VERTICES_SBBS,VERTICES_SBCW,VERTICES_SBRE)


TEXTOJURISDIÇÃO <- data.frame(X=c(-16.34,-27.29,-9.23,-5.54,-20), Y=c(-49.14,-51.41,-39.29,-59.52,-70), 
                              VALOR=c(DENSIDADE_C1,DENSIDADE_C2,DENSIDADE_C3,DENSIDADE_C4, DENSIDADE_SISCEAB))#DF criado para gerar a legenda
TEXTOJURISDIÇÃOCRCEA <- data.frame(X=c(-25), Y=c(-35), VALOR=c(DENSIDADE_CRCEA))
LINHATEXTOJURISDIÇÃOCRCEA <- data.frame(x1=-35, x2= -45.45, y1=-25, y2= -23.33)  
CORESJURISDICAO <- c("#FFFF99","#007FFF","#CC0000","#215E21","#4F2F4F") #cores do mapa


