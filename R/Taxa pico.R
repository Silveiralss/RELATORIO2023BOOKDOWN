library(tidyverse)
library(lubridate)
library(dplyr)
library(plotly)
library(readr)
library(patchwork)
library(leaflet)
library(geobr)
library(readxl)
library(sf)
library(rgdal)
library(httr)
library(jsonlite)




#TAXA PICO DE CHEGADA KPI-10(26)

BDS_KPI_10_DF%>% dplyr::filter(movimentos.ano == "2019" |movimentos.ano == "2022") %>%
  ggplot(aes(x = movimentos.aero, y = movimentos.jan, fill = as.factor(movimentos.ano)))+
  geom_col(position=position_dodge(preserve = "single")) +
   facet_grid(cols = vars(movimentos.regiao),switch= "x", scales = "free_x") +
  labs(fill=NULL)+
  scale_y_continuous(limits = c(0,55))+
  geom_text(aes( y = movimentos.jan, label = movimentos.jan, group = movimentos.ano, hjust = -.15, angle = 90)
            , position = position_dodge(width=.9), size = 3, color = "black")+
  scale_fill_manual(values = cores)+
  labs(title = "Taxa pico de chegada", caption = "Fonte: TATIC FLOW", x = NULL, y = "Tacha pico") +
  theme_minimal()+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 18L,
                                  face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 12, color = "#8B0000"),
        plot.caption = element_text(hjust = .5,
                                    face = "bold"))

##############################


# CAPACIDADE DE ARR x TAXA PICO (27)

BDS_KPI_10_DF%>% dplyr::filter(movimentos.ano == "2022") %>%
  ggplot(aes(x = movimentos.aero, y = movimentos.jan, fill = as.factor(movimentos.ano)))+
  geom_point(shape=24, color="blue") +
  geom_point(data = BDS_KPI_09_DF%>% dplyr::filter(movimentos.ano == "2022"), aes(x=movimentos.aero, y=movimentos.jan), shape=15, color="red",size=2.5)+
  facet_grid(cols = vars(movimentos.regiao),switch= "x", scales = "free_x") +
  labs(fill=NULL)+
    scale_y_continuous(limits = c(0,55))+
  geom_text(aes( y = movimentos.jan, label = movimentos.jan, group = movimentos.ano, vjust=-0.8)
            , position = position_dodge(width=.9), size = 3, color = "black")+
  labs(title = "Capacidade de ARR X taxa pico", caption = "Fonte: CGNA e TATIC FLOW", x = NULL, y = "Tacha pico e capacidade de pouso") +
  theme_minimal()+
  theme(legend.position = "none",
        plot.title = element_text(size = 18L,
                                  face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 12, color = "#8B0000"),
        plot.caption = element_text(hjust = .5,
                                    face = "bold"))
