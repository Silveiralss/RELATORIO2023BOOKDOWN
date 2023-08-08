# Pontualidade de Taxi

library(readr)
library(tidyverse)
library(lubridate)
library(dplyr)
library(plotly)
library(ggrepel)




cores <- c("#B0C4DE",
           "#708090",
           "#4682B4",
           "#191970")




#############################################################################################################################################################


#gráficos com API 

#TAXIOUT SBGR 2022(7)

BDS_KPI_02_DF %>% filter(movimentos.ano == year(Sys.Date())) %>% 
ggplot()+
  geom_col(aes(x=movimentos.aero, y=movimentos.percentual_ano, fill = as.factor(movimentos.ano)),fill="#191970", position = "dodge")+
  geom_text(aes(x=movimentos.aero, y=movimentos.percentual_ano, label = round(movimentos.percentual_ano,2), group = movimentos.ano),size = 3,
            hjust = -0.1,
            position = position_dodge(width = 0.9),angle = 90)+
  geom_hline(yintercept = 3, color = "red")+
  theme_minimal()+
  labs(title = "Tempo adicional de TAXI_OUT por aeroporto (2022)", caption = "Fonte: BINTRA", x = NULL, y = "Tempo adicional de taxi-out (min)"
       ,fill = "ANO") +
  theme(plot.title = element_text(size = 20L,
                                  face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 12, color = "#8B0000"),
        plot.caption = element_text(hjust = .5,
                                    face = "bold"))+ #se precisar rotacionar o texto X usar (axis.text = element_text(angle = 90)) dentro do theme
  scale_fill_manual(values = cores)


###################################################################
#GRÁFICO DE TEMPO ADICIONAL DE TAXI-OUT POR REGIONAL(8)

ggplot(BDS_KPI_02_REGIONAL_DF)+
  geom_col(aes(x=movimentos.aero, y=movimentos.percentual_ano, fill = as.factor(movimentos.ano)), position = "dodge")+
  geom_text(aes(x=movimentos.aero, y=movimentos.percentual_ano, label = round(movimentos.percentual_ano,2), group = movimentos.ano),size = 3,
            hjust = -0.1,
            position = position_dodge(width = 0.9),angle = 90)+
  geom_hline(yintercept = 3, color = "red")+
  theme_minimal() +
  labs(title = "Tempo adicional de TAXI_OUT por REGIONAL", caption = "Fonte: BINTRA", x = NULL, y = "average additional taxi-out time"
       ,fill = "ANO") +
  theme(legend.position = "bottom",
        legend.title=element_blank()
        ,legend.text     = element_text(size = 8)
        ,legend.key.size = unit(0.3, "cm"),
        plot.title = element_text(size = 20L,
                                  face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 12, color = "#8B0000"),
        plot.caption = element_text(hjust = .5,
                                    face = "bold"))+
  scale_fill_manual(values = cores)
######################################################
#GRÁFICO TEMPO ADICIONAL DE TAXI OUT FACETADO POR AEROPORTO(9)

ggplot(BDS_KPI_02_DF, aes(x=movimentos.ano, y=movimentos.percentual_ano, colour=movimentos.aero, fill=movimentos.aero))+
  geom_line(size=1)+
  geom_point()+
  geom_hline(yintercept = 3, color = "red")+
  theme_light()+
  facet_wrap(~movimentos.aero,ncol = 4)+
  theme (legend.position = "none",strip.text = element_text(face = "italic", color = "black"))+
  labs(title = "Tempo adicional de TAXI_OUT por aeroporto", caption = "Fonte: BINTRA", x = "ANO", y = "Tempo adicional de taxi-out (min)")+
  theme(plot.title = element_text(size = 20L,
                                  face = "bold",
                                  hjust = 0.5))

######################################################
#GRAFICO TEMPO ADICIONAL DE TAXI-OUT SBGR(10)

BDS_KPI_02_DF %>% filter(movimentos.aero=="SBGR") %>% 
  ggplot()+
  geom_col(aes(x=movimentos.ano, y=movimentos.percentual_ano,fill=as.factor(movimentos.ano)), position = "dodge")+
  geom_text(aes(x=movimentos.ano, y=movimentos.percentual_ano, label = round(movimentos.percentual_ano,2), group = movimentos.ano),size = 5,
            vjust=-0.5,
            position = position_dodge(width = 0.9))+
  geom_hline(yintercept = 3, color = "red")+
  theme_minimal() +
  labs(title = "Tempo adicional de TAXI_OUT (SBGR)", caption = "Fonte: BINTRA", x = NULL, y = "Tempo adicional de taxi-out (min)"
       ,fill = "ANO")+
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        legend.text     = element_text(size = 8)
        ,legend.key.size = unit(0.3, "cm"),
        plot.title = element_text(size = 20L,
                                  face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 12, color = "#8B0000"),
        plot.caption = element_text(hjust = .5,
                                    face = "bold"))+ #se precisar rotacionar o texto X usar (axis.text = element_text(angle = 90)) dentro do theme
  scale_fill_manual(values = cores)

######################################################

#TAXI-IN SBGR 2022(11)

BDS_KPI_13_DF %>% filter(movimentos.ano==year(Sys.Date())) %>% 
  ggplot()+
  geom_col(aes(x=movimentos.aero, y=movimentos.percentual_ano, fill = as.factor(movimentos.ano)),fill="#191970", position = "dodge")+
  geom_text(aes(x=movimentos.aero, y=movimentos.percentual_ano, label = round(movimentos.percentual_ano,2), group = movimentos.ano),size = 3,
            hjust = -0.1,
            position = position_dodge(width = 0.9),angle = 90)+
  geom_hline(yintercept = 3, color = "red")+
  theme_minimal()+
  labs(title = "Tempo adicional de TAXI_IN por aeroporto (2022)", caption = "Fonte: BINTRA", x = NULL, y = "Tempo adicional de taxi-out (min)"
       ,fill = "ANO") +
  theme(plot.title = element_text(size = 20L,
                                  face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 12, color = "#8B0000"),
        plot.caption = element_text(hjust = .5,
                                    face = "bold"))+ #se precisar rotacionar o texto X usar (axis.text = element_text(angle = 90)) dentro do theme
  scale_fill_manual(values = cores)


###################################################################
#GRÁFICO DE TEMPO ADICIONAL DE TAXI-IN POR REGIONAL(12)

ggplot(BDS_KPI_13_REGIONAL_DF)+
  geom_col(aes(x=movimentos.aero, y=movimentos.percentual_ano, fill = as.factor(movimentos.ano)), position = "dodge")+
  geom_text(aes(x=movimentos.aero, y=movimentos.percentual_ano, label = round(movimentos.percentual_ano,2), group = movimentos.ano),size = 3,
            hjust = -0.1,
            position = position_dodge(width = 0.9),angle = 90)+
  geom_hline(yintercept = 3, color = "red")+
  theme_minimal() +
  labs(title = "Tempo adicional de TAXI_IN por REGIONAL", caption = "Fonte: BINTRA", x = NULL, y = "average additional taxi-out time"
       ,fill = "ANO") +
  theme(legend.position = "bottom",
        legend.title=element_blank()
        ,legend.text     = element_text(size = 8)
        ,legend.key.size = unit(0.3, "cm"),
        plot.title = element_text(size = 20L,
                                  face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 12, color = "#8B0000"),
        plot.caption = element_text(hjust = .5,
                                    face = "bold"))+
  scale_fill_manual(values = cores)
######################################################
#GRÁFICO TEMPO ADICIONAL DE TAXI-IN FACETADO POR AEROPORTO(13)

ggplot(BDS_KPI_13_DF, aes(x=movimentos.ano, y=movimentos.percentual_ano, colour=movimentos.aero, fill=movimentos.aero))+
  geom_line(size=1)+
  geom_point()+
  geom_hline(yintercept = 3, color = "red")+
  theme_light()+
  facet_wrap(~movimentos.aero,ncol = 4)+
  theme (legend.position = "none",strip.text = element_text(face = "italic", color = "black"))+
  labs(title = "Tempo adicional de TAXI_IN por aeroporto", caption = "Fonte: BINTRA", x = "ANO", y = "Tempo adicional de taxi-out (min)")+
  theme(plot.title = element_text(size = 20L,
                                  face = "bold",
                                  hjust = 0.5))

######################################################
#GRAFICO TEMPO ADICIONAL DE TAXI-OUT SBGR(14)

BDS_KPI_13_DF %>% filter(movimentos.aero=="SBGR") %>% 
  ggplot()+
  geom_col(aes(x=movimentos.ano, y=movimentos.percentual_ano,fill=as.factor(movimentos.ano)), position = "dodge")+
  geom_text(aes(x=movimentos.ano, y=movimentos.percentual_ano, label = round(movimentos.percentual_ano,2), group = movimentos.ano),size = 5,
            vjust=-0.5,
            position = position_dodge(width = 0.9))+
  geom_hline(yintercept = 3, color = "red")+
  theme_minimal() +
  labs(title = "Tempo adicional de TAXI_IN (SBGR)", caption = "Fonte: BINTRA", x = NULL, y = "Tempo adicional de taxi-out (min)"
       ,fill = "ANO")+
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        legend.text     = element_text(size = 8)
        ,legend.key.size = unit(0.3, "cm"),
        plot.title = element_text(size = 20L,
                                  face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 12, color = "#8B0000"),
        plot.caption = element_text(hjust = .5,
                                    face = "bold"))+ #se precisar rotacionar o texto X usar (axis.text = element_text(angle = 90)) dentro do theme
  scale_fill_manual(values = cores)


###################################################################
#GRÁFICO DE TEMPO ADICIONAL DE TAXI-IN/OUT POR REGIONAL(15)

TAXI_IN_OUT_REGIONAL <- bind_cols(BDS_KPI_02_REGIONAL_DF$movimentos.ano,
                                  BDS_KPI_02_REGIONAL_DF$movimentos.aero,
                                  BDS_KPI_02_REGIONAL_DF$movimentos.percentual_ano,
                                  BDS_KPI_13_REGIONAL_DF$movimentos.percentual_ano)

TAXI_IN_OUT_REGIONAL_COL_NAMES <- c('movimentos.ano','movimentos.aero','movimentos.percentual_ano_out','movimentos.percentual_ano_in')
colnames(TAXI_IN_OUT_REGIONAL) <- TAXI_IN_OUT_REGIONAL_COL_NAMES
TAXI_IN_OUT_REGIONAL <- TAXI_IN_OUT_REGIONAL %>% pivot_longer(cols = movimentos.percentual_ano_out:movimentos.percentual_ano_in,
                                      names_to = "IN_OUT",
                                      values_to = "ADICIONAL")

TAXI_IN_OUT_REGIONAL %>%
  filter(movimentos.ano == year(Sys.Date())) %>% 
ggplot()+
  geom_col(aes(x=movimentos.aero, y=ADICIONAL, fill = IN_OUT), position = "dodge")+
  geom_text(aes(x=movimentos.aero, y=ADICIONAL, label = round(ADICIONAL,2), group = IN_OUT),size = 3,
            hjust=-0.2,
            position = position_dodge(width = 0.9),angle = 90)+
  geom_hline(yintercept = 3, color = "red")+
  theme_minimal() +
  labs(title = "Tempo adicional de TAXI_IN/OUT por REGIONAL(2022)", caption = "Fonte: BINTRA", x = NULL, y = "average additional taxi in/out time (min)"
       ,fill = "ANO") +
  theme(legend.position = "bottom",
        legend.title=element_blank()
        ,legend.text     = element_text(size = 8)
        ,legend.key.size = unit(0.3, "cm"),
        plot.title = element_text(size = 20L,
                                  face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 12, color = "#8B0000"),
        plot.caption = element_text(hjust = .5,
                                    face = "bold"))+
  scale_fill_manual(values = cores,labels=c("TAXI-IN", "TAXI-OUT"))
