#movimentos

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
######################################################



#instalar e rodar ggimage para colocar imagens no gráfico, para colocar a imagem usar a função geom_image


######################



#######################

#gráfico MOVIMENTO DA AVIAÇÃO COMERCIAL DOMÉSTICA (1)

MOV_COM_DOMESTC %>% filter(ANO %in% c(2019, 2022)) %>%                                              #pega a bds e filtra o ano de 2019 e 2022 na coluna ano
ggplot()+                                                                                                 #chama o pacote ggplot2
  geom_col(aes(x=MES, y=MOV, fill = ANO), position = position_dodge(preserve = "single"))+                #cria as colunas (position = position_dodge(preserve = "single") mantém o tamanho das colunas
  scale_y_continuous(breaks = NULL, limits = c(0,160000))+                                                #configura o eixo y breaks são os números que vão aparecer no eixo, limits são o menor número e o maior número da escala 
  geom_text(aes(x=MES, y=MOV, label = MOV, group = ANO), size = 3,                                        #cria texto sem fundo
            hjust = -0.1,
            position = position_dodge(width = 0.9),angle = 90)+
  theme_minimal() +                                                                                       #theme_ é o fundo do gráfico
  theme(legend.position = "top"                                                                           #theme() é usado para ajustar a posição, tamanho,estilo e outras configurações de texto do gráfico 
        ,legend.title    = element_text(size = 8) 
        ,legend.text     = element_text(size = 8)
        ,legend.key.size = unit(0.3, "cm")
  ) +
  labs(title = "Movimentos da aviação comercial doméstica", caption = "Fonte: TATIC FLOW", x = NULL, y = NULL   #labs() é o texto propriamente dito
       ,fill = "ANO") +
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        plot.title = element_text(size = 20L,
                                  face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 12, color = "#8B0000"),
        plot.caption = element_text(hjust = .5,
                                    face = "bold"))+
  scale_fill_manual(values = cores)                                                                       
################################################
############################################# copiar e colar no markdown

BDS_VAR_2019 <- MOV_COM_DOMESTC %>% filter(ANO %in% c(2019), MES=="JAN")
BDS_VAR_2022 <- MOV_COM_DOMESTC %>% filter(ANO %in% c(2022), MES=="JAN")
BDS_VAR_ANO <- bind_rows(BDS_VAR_2019,BDS_VAR_2022) %>% 
  transmute(ANO=ANO, TOTAL=TOTAL,VAR=(BDS_VAR_2022$TOTAL-BDS_VAR_2019$TOTAL)/BDS_VAR_2019$TOTAL) 
  
  
GFVARANO <- ggplot(BDS_VAR_ANO)+                                                                                                 
  geom_col(aes(x=ANO, y=TOTAL, fill = ANO),width = .5, position = position_dodge(preserve = "single"))+                
  scale_y_continuous(breaks = NULL)+ 
  scale_x_discrete(expand = expansion(add = .5))+
  geom_text(aes(x=ANO, y=TOTAL/2, label = TOTAL, group = ANO), size = 4,                      
            position = position_dodge(width = 0.9),angle = 90)+
  theme_minimal() +                                                                                       
  theme(legend.position = "top"                                                                            
        ,legend.title    = element_text(size = 8) 
        ,legend.text     = element_text(size = 8)
        ,legend.key.size = unit(0.3, "cm")
  ) +
  labs(title = "Variação anual",subtitle = scales::percent(round(BDS_VAR_ANO$VAR,2)), x = NULL, y = NULL   
       ) +
  theme(legend.position = "none",
        legend.title=element_blank(),
        plot.title = element_text(size = 13,
                                  face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 11, colour = "red",hjust = .5, vjust = -3,face = "bold"))+
  scale_fill_manual(values = cores)                 




GFVARMES <- MOV_COM_DOMESTC %>% filter(ANO %in% c(2019, 2022)) %>% 
  ggplot()+
  geom_col(aes(x=MES, y=MOV, fill = ANO), position = position_dodge(preserve = "single"))+
  scale_y_continuous(breaks = NULL, limits = c(0,160000)) +
  geom_text(aes(x=MES, y=1000, label = MOV, group = ANO), size = 3.7, angle = 90,
            hjust = -0.1,
            position = position_dodge(width = 0.9)) +
  geom_text(data=MOV_COM,aes(x=MES, y=155000, label = scales::percent(VAR,accuracy = 0.1)), size = 3.5,color="red",
            hjust = 0.5,
            position = position_dodge(width = 0.9),fontface="bold") +
  theme_minimal() +
  theme(legend.position = "top"
        ,legend.title    = element_text(size = 8) 
        ,legend.text     = element_text(size = 8)
        ,legend.key.size = unit(0.3, "cm")
  ) +
  labs(title = "Movimentos da aviação comercial doméstica", caption = "Fonte: TATIC FLOW", x = NULL, y = NULL
       ,fill = "ANO") +
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        plot.title = element_text(size = 18,
                                  face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 12, color = "#8B0000"),
        plot.caption = element_text(hjust = .5,
                                    face = "bold"))+
  scale_fill_manual(values = cores)

gridExtra::grid.arrange(GFVARMES,GFVARANO,widths=c(6,1))

##################################################
#####################################
#GRÁFICO MOVIMENTO AVIAÇÃO COMERCIAL INTERNACIONAL (2)

MOV_COM_INT%>% filter(ANO == "2019" |ANO == "2022") %>%
ggplot()+
  geom_col(aes(x=MES, y=MOV, fill = ANO), position = position_dodge(preserve = "single"))+
  scale_y_continuous(breaks = NULL, limits = c(0,20000))+
  geom_text(aes(x=MES, y=MOV, label = MOV, group = ANO), size = 3,
            hjust = -0.1,
            position = position_dodge(width = 0.9),angle = 90)+
  theme_minimal() +
  theme(legend.position = "top"
        ,legend.title    = element_text(size = 8) 
        ,legend.text     = element_text(size = 8)
        ,legend.key.size = unit(0.3, "cm")
  ) +
  labs(title = "Movimentos da aviação comercial internacional", caption = "Fonte: TATIC FLOW", x = NULL, y = NULL
       ,fill = "ANO") +
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        plot.title = element_text(size = 20L,
                                  face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 12, color = "#8B0000"),
        plot.caption = element_text(hjust = .5,
                                    face = "bold"))+
  scale_fill_manual(values = cores)

#ggsave(filename = "C:/Users/silveiralss/OneDrive - DECEA/Documentos/Relatório/Relatório/Imagens/gf2.png",height = 5) # a função ggsave salva o gráfico como imagem no endereço especificado
######################################################
#GRÁFICO DE MOVIMENTOS SISCEAB E FIR (4)

TOTAL_MOV_SISC_FIR%>% filter(ANO == "2019" |ANO == "2022") %>%
ggplot()+
  geom_col(aes(x=FIR, y=TOTAL, fill = ANO), position = "dodge")+
  scale_y_continuous(breaks = NULL,limits = c(0,1800000))+
  geom_text(aes(x=FIR, y=TOTAL, label = TOTAL, group = ANO), size = 3,
            hjust = -0.1,
            position = position_dodge(width = 0.9),angle = 90)+
  theme_minimal() +
  theme(legend.position = "top"
        ,legend.title    = element_text(size = 8) 
        ,legend.text     = element_text(size = 8)
        ,legend.key.size = unit(0.3, "cm")
  ) +
  labs(title = "Total de movimentos SISCEAB e por FIR", caption = "Fonte: SETA", x = NULL, y = NULL
       ,fill = "ANO") +
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        plot.title = element_text(size = 20L,
                                  face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 12, color = "#8B0000"),
        plot.caption = element_text(hjust = .5,
                                    face = "bold"))+
  scale_fill_manual(values = cores)

######################################################

#GRÁFICO COMPARATIVO POR MÊS DE TOTAL DE OPERAÇÕES ARR E DEP NO SISCEAB (03)


TOTAL_MES_SISCEAB%>% filter(ANO == "2019" |ANO == "2022") %>%
ggplot()+
  geom_col(aes(x=MES, y=MOV, fill = ANO), position = position_dodge(preserve = "single"))+
  scale_y_continuous(breaks = NULL, limits = c(0,170000))+
  geom_text(aes(x=MES, y=MOV, label = MOV, group = ANO), size = 3,
            hjust = -0.1,
            position = position_dodge(width = 0.9),angle = 90)+
  theme_minimal() +
  theme(legend.position = "top"
        ,legend.title    = element_text(size = 8) 
        ,legend.text     = element_text(size = 8)
        ,legend.key.size = unit(0.3, "cm")
  ) +
  labs(title = "Total de movimentos SISCEAB por mês", caption = "Fonte: SETA", x = NULL, y = NULL
       ,fill = "ANO") +
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        plot.title = element_text(size = 20L,
                                  face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 12, color = "#8B0000"),
        plot.caption = element_text(hjust = .5,
                                    face = "bold"))+
  scale_fill_manual(values = cores)




######################################################

########################################
#GRAFICO DE DENSIDADE DE TRÁFEGO (5)

#importação da BDS dos pontos geográficos
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

JURISDICAO <- bind_rows(VERTICES_SBAZ,VERTICES_SBBS,VERTICES_SBCW,VERTICES_SBRE) #colando as colunas


TEXTOJURISDIÇÃO <- data.frame(X=c(-16.34,-27.29,-9.23,-5.54,-20), Y=c(-49.14,-51.41,-39.29,-59.52,-70), 
                              VALOR=c(DENSIDADE_C1,DENSIDADE_C2,DENSIDADE_C3,DENSIDADE_C4, DENSIDADE_SISCEAB))#DF criado para gerar a legenda, foi necessário criar esse df para escolher os pontos onde a legenda aparecerá no mapa

TEXTOJURISDIÇÃOCRCEA <- data.frame(X=c(-25), Y=c(-35), VALOR=c(DENSIDADE_CRCEA))# o CRCEA foi criado separadamente para ficar em uma outra camada

LINHATEXTOJURISDIÇÃOCRCEA <- data.frame(x1=-35, x2= -45.45, y1=-25, y2= -23.33)  

CORESJURISDICAO <- c("#FFFF99","#007FFF","#CC0000","#215E21","#4F2F4F") #cria as cores do mapa



ggplot(data = read_state())+
  geom_sf()+
  geom_polygon(data = JURISDICAO, aes(x=LONGITUDE,y = LATITUDE, fill=REGIONAL),color="#666666",alpha=0.5)+
  geom_polygon(data = VERTICES_CRCEA_SE, aes(x=LONGITUDE,y = LATITUDE, fill=REGIONAL),color="#666666",alpha=0.8)+
  geom_point(data = TEXTOJURISDIÇÃO, aes(x=Y, y=X))+
  geom_segment(LINHATEXTOJURISDIÇÃOCRCEA,mapping=aes(x=x1, y=y1, xend=x2, yend=y2))+
  geom_label(data = TEXTOJURISDIÇÃO,aes(x=Y,y = X, label = VALOR),size=4,label.padding = unit(0.20, "lines"),label.r = unit(0.5, "lines"))+
  geom_label(data = TEXTOJURISDIÇÃOCRCEA,aes(x=Y,y = X, label = VALOR),size=4,label.padding = unit(0.20, "lines"),label.r = unit(0.5, "lines"))+
  theme_minimal()+
  scale_fill_manual(values = CORESJURISDICAO)






#até aqui está tudo pronto
# logo abaixo tem o início do mapa com as terminais




###############################################

VERTICES_TMA_BRASIL <- read_excel("C:/Users/silveiralss/OneDrive - DECEA/Documentos/Relatório/Relatório/data/VERTICES_TMA_BRASIL.xlsx")

TMA_SBWA <- VERTICES_TMA_BRASIL %>% dplyr::filter(ident == "SBWA")
TMA_SBWQ<- VERTICES_TMA_BRASIL %>% dplyr::filter(ident == "SBWQ")
TMA_SBWN<- VERTICES_TMA_BRASIL %>% dplyr::filter(ident == "SBWN")
TMA_SBWX<- VERTICES_TMA_BRASIL %>% dplyr::filter(ident == "SBWX")
TMA_SBXK<- VERTICES_TMA_BRASIL %>% dplyr::filter(ident == "SBXK")
TMA_SBWB<- VERTICES_TMA_BRASIL %>% dplyr::filter(ident == "SBWB")
TMA_SBXJ<- VERTICES_TMA_BRASIL %>% dplyr::filter(ident == "SBXJ")
TMA_SBXD<- VERTICES_TMA_BRASIL %>% dplyr::filter(ident == "SBXD")
TMA_SBWS<- VERTICES_TMA_BRASIL %>% dplyr::filter(ident == "SBWS")
TMA_SBXE<- VERTICES_TMA_BRASIL %>% dplyr::filter(ident == "SBXE")
TMA_SBWZ<- VERTICES_TMA_BRASIL %>% dplyr::filter(ident == "SBWZ")
TMA_SBXT<- VERTICES_TMA_BRASIL %>% dplyr::filter(ident == "SBXT")
TMA_SBWF<- VERTICES_TMA_BRASIL %>% dplyr::filter(ident == "SBWF")
TMA_SBXM<- VERTICES_TMA_BRASIL %>% dplyr::filter(ident == "SBXM")
TMA_SBXA<- VERTICES_TMA_BRASIL %>% dplyr::filter(ident == "SBXA")
TMA_SBXS<- VERTICES_TMA_BRASIL %>% dplyr::filter(ident == "SBXS")
TMA_SBWL<- VERTICES_TMA_BRASIL %>% dplyr::filter(ident == "SBWL")
TMA_SBWK<- VERTICES_TMA_BRASIL %>% dplyr::filter(ident == "SBWK")
TMA_SBWR<- VERTICES_TMA_BRASIL %>% dplyr::filter(ident == "SBWR")
TMA_SBXW<- VERTICES_TMA_BRASIL %>% dplyr::filter(ident == "SBXW")
TMA_SBXU<- VERTICES_TMA_BRASIL %>% dplyr::filter(ident == "SBXU")
TMA_SBWH<- VERTICES_TMA_BRASIL %>% dplyr::filter(ident == "SBWH")
TMA_SBXR<- VERTICES_TMA_BRASIL %>% dplyr::filter(ident == "SBXR")
TMA_SBWE<- VERTICES_TMA_BRASIL %>% dplyr::filter(ident == "SBWE")
TMA_SBWJ<- VERTICES_TMA_BRASIL %>% dplyr::filter(ident == "SBWJ")
TMA_SBXQ<- VERTICES_TMA_BRASIL %>% dplyr::filter(ident == "SBXQ")
TMA_SBWU<- VERTICES_TMA_BRASIL %>% dplyr::filter(ident == "SBWU")
TMA_SBXO<- VERTICES_TMA_BRASIL %>% dplyr::filter(ident == "SBXO")
TMA_SBXG<- VERTICES_TMA_BRASIL %>% dplyr::filter(ident == "SBXG")
TMA_SBXP<- VERTICES_TMA_BRASIL %>% dplyr::filter(ident == "SBXP")
TMA_SBWT<- VERTICES_TMA_BRASIL %>% dplyr::filter(ident == "SBWT")
TMA_SBXF<- VERTICES_TMA_BRASIL %>% dplyr::filter(ident == "SBXF")
TMA_SBWP<- VERTICES_TMA_BRASIL %>% dplyr::filter(ident == "SBWP")
TMA_SBWM<- VERTICES_TMA_BRASIL %>% dplyr::filter(ident == "SBWM")
TMA_SBXL<- VERTICES_TMA_BRASIL %>% dplyr::filter(ident == "SBXL")
TMA_SBWI<- VERTICES_TMA_BRASIL %>% dplyr::filter(ident == "SBWI")
TMA_SBXN<- VERTICES_TMA_BRASIL %>% dplyr::filter(ident == "SBXN")
TMA_SBWG<- VERTICES_TMA_BRASIL %>% dplyr::filter(ident == "SBWG")
TMA_SBWY<- VERTICES_TMA_BRASIL %>% dplyr::filter(ident == "SBWY")
TMA_SBWV<- VERTICES_TMA_BRASIL %>% dplyr::filter(ident == "SBWV")
TMA_SBXB<- VERTICES_TMA_BRASIL %>% dplyr::filter(ident == "SBXB")

##########################


ggplot(data = read_state())+
  geom_sf()+
  geom_polygon(data = JURISDICAO, aes(x=LONGITUDE,y = LATITUDE, fill=REGIONAL),color="#666666",alpha=0.5)+
  geom_polygon(data = VERTICES_CRCEA_SE, aes(x=LONGITUDE,y = LATITUDE, fill=REGIONAL),color="#666666",alpha=0.8)+
  geom_polygon(data = TMA_SBWA, aes(x=LONGITUDE,y = LATITUDE),color="black")+
  geom_polygon(data = TMA_SBWQ, aes(x=LONGITUDE,y = LATITUDE),color="black")+
  geom_polygon(data = TMA_SBWN, aes(x=LONGITUDE,y = LATITUDE),color="black")+
  geom_polygon(data = TMA_SBWX, aes(x=LONGITUDE,y = LATITUDE),color="black")+
  geom_polygon(data = TMA_SBXK, aes(x=LONGITUDE,y = LATITUDE),color="black")+
  geom_polygon(data = TMA_SBWB, aes(x=LONGITUDE,y = LATITUDE),color="black")+
  geom_polygon(data = TMA_SBXJ, aes(x=LONGITUDE,y = LATITUDE),color="black")+
  geom_polygon(data = TMA_SBXD, aes(x=LONGITUDE,y = LATITUDE),color="black")+
  geom_polygon(data = TMA_SBWS, aes(x=LONGITUDE,y = LATITUDE),color="black")+
  geom_polygon(data = TMA_SBXE, aes(x=LONGITUDE,y = LATITUDE),color="black")+
  geom_polygon(data = TMA_SBWZ, aes(x=LONGITUDE,y = LATITUDE),color="black")+
  geom_polygon(data = TMA_SBXT, aes(x=LONGITUDE,y = LATITUDE),color="black")+
  geom_polygon(data = TMA_SBWF, aes(x=LONGITUDE,y = LATITUDE),color="black")+
  geom_polygon(data = TMA_SBXM, aes(x=LONGITUDE,y = LATITUDE),color="black")+
  geom_polygon(data = TMA_SBXA, aes(x=LONGITUDE,y = LATITUDE),color="black")+
  geom_polygon(data = TMA_SBXS, aes(x=LONGITUDE,y = LATITUDE),color="black")+
  geom_polygon(data = TMA_SBWL, aes(x=LONGITUDE,y = LATITUDE),color="black")+
  geom_polygon(data = TMA_SBWK, aes(x=LONGITUDE,y = LATITUDE),color="black")+
  geom_polygon(data = TMA_SBWR, aes(x=LONGITUDE,y = LATITUDE),color="black")+
  geom_polygon(data = TMA_SBXW, aes(x=LONGITUDE,y = LATITUDE),color="black")+
  geom_polygon(data = TMA_SBWH, aes(x=LONGITUDE,y = LATITUDE),color="black")+
  geom_polygon(data = TMA_SBXR, aes(x=LONGITUDE,y = LATITUDE),color="black")+
  geom_polygon(data = TMA_SBWE, aes(x=LONGITUDE,y = LATITUDE),color="black")+
  geom_polygon(data = TMA_SBWJ, aes(x=LONGITUDE,y = LATITUDE),color="black")+
  geom_polygon(data = TMA_SBXQ, aes(x=LONGITUDE,y = LATITUDE),color="black")+
  geom_polygon(data = TMA_SBWU, aes(x=LONGITUDE,y = LATITUDE),color="black")+
  geom_polygon(data = TMA_SBXO, aes(x=LONGITUDE,y = LATITUDE),color="black")+
  geom_polygon(data = TMA_SBXG, aes(x=LONGITUDE,y = LATITUDE),color="black")+
  geom_polygon(data = TMA_SBXP, aes(x=LONGITUDE,y = LATITUDE),color="black")+
  geom_polygon(data = TMA_SBWT, aes(x=LONGITUDE,y = LATITUDE),color="black")+
  geom_polygon(data = TMA_SBXF, aes(x=LONGITUDE,y = LATITUDE),color="black")+
  geom_polygon(data = TMA_SBWP, aes(x=LONGITUDE,y = LATITUDE),color="black")+
  geom_polygon(data = TMA_SBWM, aes(x=LONGITUDE,y = LATITUDE),color="black")+
  geom_polygon(data = TMA_SBXL, aes(x=LONGITUDE,y = LATITUDE),color="black")+
  geom_polygon(data = TMA_SBWI, aes(x=LONGITUDE,y = LATITUDE),color="black")+
  geom_polygon(data = TMA_SBXN, aes(x=LONGITUDE,y = LATITUDE),color="black")+
  geom_polygon(data = TMA_SBWG, aes(x=LONGITUDE,y = LATITUDE),color="black")+
  geom_polygon(data = TMA_SBWY, aes(x=LONGITUDE,y = LATITUDE),color="black")+
  geom_polygon(data = TMA_SBWV, aes(x=LONGITUDE,y = LATITUDE),color="black")+
  geom_polygon(data = TMA_SBXB, aes(x=LONGITUDE,y = LATITUDE),color="black")+
  theme_void()+
  labs(x=NULL, y=NULL)+
  scale_fill_manual(values = CORESJURISDICAO)+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none")
 
  




ggplot()+
  geom_polygon(data= TMA_SBXS, aes(x=LONGITUDE,y = LATITUDE))
geom_polygon(data = TMA_SBWA, aes(x=LONGITUDE,y = LATITUDE))+#geom_polygon(data = VERTICES_CRCEA_SE, aes(x=LONGITUDE,y = LATITUDE, fill=REGIONAL),color="#666666",alpha=0.8)+
  #geom_point(data = TEXTOJURISDIÇÃO, aes(x=Y, y=X))+
  #geom_segment(LINHATEXTOJURISDIÇÃOCRCEA,mapping=aes(x=x1, y=y1, xend=x2, yend=y2))+
  #geom_label(data = TEXTOJURISDIÇÃO,aes(x=Y,y = X, label = VALOR),size=4,label.padding = unit(0.20, "lines"),label.r = unit(0.5, "lines"))+
  #geom_label(data = TEXTOJURISDIÇÃOCRCEA,aes(x=Y,y = X, label = VALOR),size=4,label.padding = unit(0.20, "lines"),label.r = unit(0.5, "lines"))+
  theme_minimal()
  #scale_fill_manual(values = CORESJURISDICAO)














#MAPA_TMA_BRASIL <- 
leaflet() %>% addTiles() %>%
  addPolygons(data= TMA_SBWA, lat = TMA_SBWA$LATITUDE, lng = TMA_SBWA$LONGITUDE,weight = 1) %>%
  addPolygons(data= TMA_SBWQ, lat = TMA_SBWQ$LATITUDE, lng = TMA_SBWQ$LONGITUDE,weight = 1) %>%
  addPolygons(data= TMA_SBWN, lat = TMA_SBWN$LATITUDE, lng = TMA_SBWN$LONGITUDE,weight = 1) %>% 
  addPolygons(data= TMA_SBWX, lat = TMA_SBWN$LATITUDE, lng = TMA_SBWX$LONGITUDE,weight = 1) %>%
  addPolygons(data= TMA_SBXK, lat = TMA_SBXK$LATITUDE, lng = TMA_SBXK$LONGITUDE,weight = 1) %>%
  addPolygons(data= TMA_SBWB, lat = TMA_SBWB$LATITUDE, lng = TMA_SBWB$LONGITUDE,weight = 1) %>%
  addPolygons(data= TMA_SBXJ, lat = TMA_SBXJ$LATITUDE, lng = TMA_SBXJ$LONGITUDE,weight = 1) %>%
  addPolygons(data= TMA_SBXD, lat = TMA_SBXD$LATITUDE, lng = TMA_SBXD$LONGITUDE,weight = 1) %>%
  addPolygons(data= TMA_SBXE, lat = TMA_SBXE$LATITUDE, lng = TMA_SBXE$LONGITUDE,weight = 1) %>%
  addPolygons(data= TMA_SBWZ, lat = TMA_SBWZ$LATITUDE, lng = TMA_SBWZ$LONGITUDE,weight = 1) %>%
  addPolygons(data= TMA_SBXT, lat = TMA_SBXT$LATITUDE, lng = TMA_SBXT$LONGITUDE,weight = 1) %>%
  addPolygons(data= TMA_SBWF, lat = TMA_SBWF$LATITUDE, lng = TMA_SBWF$LONGITUDE,weight = 1) %>%
  addPolygons(data= TMA_SBXM, lat = TMA_SBXM$LATITUDE, lng = TMA_SBXM$LONGITUDE,weight = 1) %>%
  addPolygons(data= TMA_SBXA, lat = TMA_SBXA$LATITUDE, lng = TMA_SBXA$LONGITUDE,weight = 1) %>%
  addPolygons(data= TMA_SBXS, lat = TMA_SBXS$LATITUDE, lng = TMA_SBXS$LONGITUDE,weight = 1) %>%
  addPolygons(data= TMA_SBWL, lat = TMA_SBWL$LATITUDE, lng = TMA_SBWL$LONGITUDE,weight = 1) %>%
  addPolygons(data= TMA_SBWK, lat = TMA_SBWK$LATITUDE, lng = TMA_SBWK$LONGITUDE,weight = 1) %>%
  addPolygons(data= TMA_SBWR, lat = TMA_SBWR$LATITUDE, lng = TMA_SBWR$LONGITUDE,weight = 1) %>%
  addPolygons(data= TMA_SBXW, lat = TMA_SBXW$LATITUDE, lng = TMA_SBXW$LONGITUDE,weight = 1) %>%
  addPolygons(data= TMA_SBXU, lat = TMA_SBXU$LATITUDE, lng = TMA_SBXU$LONGITUDE,weight = 1) %>%
  addPolygons(data= TMA_SBWH, lat = TMA_SBWH$LATITUDE, lng = TMA_SBWH$LONGITUDE,weight = 1) %>%
  addPolygons(data= TMA_SBXR, lat = TMA_SBXR$LATITUDE, lng = TMA_SBXR$LONGITUDE,weight = 1) %>%
  addPolygons(data= TMA_SBWE, lat = TMA_SBWE$LATITUDE, lng = TMA_SBWE$LONGITUDE,weight = 1) %>%
  addPolygons(data= TMA_SBWJ, lat = TMA_SBWJ$LATITUDE, lng = TMA_SBWJ$LONGITUDE,weight = 1) %>%
  addPolygons(data= TMA_SBXQ, lat = TMA_SBXQ$LATITUDE, lng = TMA_SBXQ$LONGITUDE,weight = 1) %>%
  addPolygons(data= TMA_SBWU, lat = TMA_SBWU$LATITUDE, lng = TMA_SBWU$LONGITUDE,weight = 1) %>%
  addPolygons(data= TMA_SBXO, lat = TMA_SBXO$LATITUDE, lng = TMA_SBXO$LONGITUDE,weight = 1) %>%
  addPolygons(data= TMA_SBXG, lat = TMA_SBXG$LATITUDE, lng = TMA_SBXG$LONGITUDE,weight = 1) %>%
  addPolygons(data= TMA_SBXP, lat = TMA_SBXP$LATITUDE, lng = TMA_SBXP$LONGITUDE,weight = 1) %>%
  addPolygons(data= TMA_SBWT, lat = TMA_SBWT$LATITUDE, lng = TMA_SBWT$LONGITUDE,weight = 1) %>%
  addPolygons(data= TMA_SBXF, lat = TMA_SBXF$LATITUDE, lng = TMA_SBXF$LONGITUDE,weight = 1) %>%
  addPolygons(data= TMA_SBWP, lat = TMA_SBWP$LATITUDE, lng = TMA_SBWP$LONGITUDE,weight = 1) %>%
  addPolygons(data= TMA_SBWM, lat = TMA_SBWM$LATITUDE, lng = TMA_SBWM$LONGITUDE,weight = 1) %>%
  addPolygons(data= TMA_SBXL, lat = TMA_SBXL$LATITUDE, lng = TMA_SBXL$LONGITUDE,weight = 1) %>%
  addPolygons(data= TMA_SBWI, lat = TMA_SBWI$LATITUDE, lng = TMA_SBWI$LONGITUDE,weight = 1) %>%
  addPolygons(data= TMA_SBXN, lat = TMA_SBXN$LATITUDE, lng = TMA_SBXN$LONGITUDE,weight = 1) %>%
  addPolygons(data= TMA_SBWG, lat = TMA_SBWG$LATITUDE, lng = TMA_SBWG$LONGITUDE,weight = 1) %>%
  addPolygons(data= TMA_SBWY, lat = TMA_SBWY$LATITUDE, lng = TMA_SBWY$LONGITUDE,weight = 1) %>%
  addPolygons(data= TMA_SBWV, lat = TMA_SBWV$LATITUDE, lng = TMA_SBWV$LONGITUDE,weight = 1) %>%
  addPolygons(data= TMA_SBXB, lat = TMA_SBXB$LATITUDE, lng = TMA_SBXB$LONGITUDE,weight = 1)



######################################

leaflet() %>% addTiles() %>% addPolygons(data = VERTICES_TMA_BRASIL, lng = VERTICES_TMA_BRASIL$LONGITUDE, lat = VERTICES_TMA_BRASIL$LATITUDE, fill = VERTICES_TMA_BRASIL$nam)




########################################################################################

PBWGKPI15 %>% filter(Date=="2021") %>% 
  ggplot()+
  geom_col(aes(x= reorder(ROTA, Arrivals), y=Arrivals), fill = "#4682B4")+
  geom_text(aes(x= reorder(ROTA, Arrivals), y=Arrivals,label = round(PBWGKPI15$Arrivals,2)))+
  scale_y_continuous(breaks = 2:7)+
  theme_minimal()+
  theme(axis.text.x=element_text(angle = 90, hjust = 0))+
  labs(title = "Rank rotas", caption ="FONTE: ", x=NULL, y=NULL)+
  theme(
    plot.title = element_text(size = 20,
                              face = "bold",
                              hjust = 0.5),
    plot.caption = element_text(size = 10, face = "bold",hjust = 0.5))+
  coord_flip()

################################################
PBWG_BRA_airport_traffic <- read_csv("C:/Users/silveiralss/OneDrive - DECEA/Documentos/Relatório/Relatório/data/PBWG-BRA-airport-traffic.csv")
RANKROTA <- PBWG_BRA_airport_traffic %>%
  mutate(MOV=ARRS_DOM+DEPS_DOM) %>%
  mutate(APTO = as.factor(APT_ICAO)) %>% 
  group_by(APTO) %>%
  summarise(MOV=sum(MOV)) %>%
  arrange(desc(MOV))



ggplot(RANKROTA)+
  geom_col(aes(x=reorder(APTO, -MOV),y=MOV), fill = "#000080")+
  scale_y_continuous(breaks = NULL)+
  geom_label(aes(x=reorder(APTO, -MOV),y=MOV, label = MOV))+
  labs(title = "Ranking de movimentos dos aeroportos em 2021", caption ="FONTE:", x=NULL, y=NULL)+
  theme(
    plot.title = element_text(size = 20,
                              face = "bold",
                              hjust = 0.5),
    plot.caption = element_text(size = 10, face = "bold",hjust = 0.5))+
  theme_minimal()


