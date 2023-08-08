library(tidyverse)
library(lubridate)
library(dplyr)
library(plotly)
library(readr)
library(patchwork)
library(leaflet)
library(geobr)
library(readxl)


#GRÁFICOS COM API

#Tempo adicional em TMA em 2021(33)
#para trocar o ano basta retirar o (-1)


BDS_KPI08_C40_C100 %>% filter(ANO==year(Sys.Date())-1) %>% 
  pivot_longer(cols = PONTUALIDADE_ANO_C40:PONTUALIDADE_ANO_C100,
               names_to = "C40_C100",
               values_to = "ADICIONAL") %>% 
  ggplot(aes(x = AERO, y = ADICIONAL, fill = as.factor(C40_C100)))+
  geom_col(position=position_dodge(preserve = "single")) +
  facet_grid(cols = vars(REGIONAL),switch= "x", scales = "free_x") +
  geom_text(aes( y = ADICIONAL, label = ADICIONAL), hjust = -.15,vjust=-.5, angle = 60, position = position_dodge(width=.9), size = 3, color = "black")+
  scale_y_continuous(limits = c(0,6))+
  scale_fill_manual(values = c("#B0C4DE","#4682B4"),labels=c("C100", "C40"))+
  labs(fill=NULL, title = "Tempo adicional em TMA em 2021", caption = "Fonte: PCICEA", x = NULL, y = "Tempo adicional em minutos") +
  theme_minimal()+
  theme(legend.position = "bottom",
    plot.title = element_text(size = 18,
                              face = "bold",
                              hjust = 0.5),
    plot.caption = element_text(hjust = .5,
                                face = "bold"))
########################################################################
 #Tempo adicional em TMA mensal em SBGR (34)
 
  
  
  
  BDS_KPI_08_MES %>% filter(AERO=="SBGR_40"| AERO=="SBGR_100") %>% 
    mutate(MES=factor(MES,levels = c("JAN","FEV","MAR","ABR","MAI","JUN","JUL","AGO","SET","OUT","NOV","DEZ")), ANO=as.character(ANO)) %>% 
    ggplot(aes(x = MES, y = ADICIONAL, fill = as.factor(AERO)))+
    geom_col(position=position_dodge(preserve = "single")) +
    #facet_grid(cols = vars(REGIONAL),switch= "x", scales = "free_x") +
    geom_text(aes( y = ADICIONAL, label = ADICIONAL), hjust = -.15,vjust=-.5, angle = 60, position = position_dodge(width=.9), size = 3, color = "black")+
    scale_y_continuous(limits = c(0,6))+
    scale_fill_manual(values = c("#B0C4DE","#4682B4"),labels=c("C100", "C40"))+
    labs(fill=NULL, title = "Tempo adicional em TMA mensal em SBGR", caption = "Fonte: PCICEA", x = NULL, y = NULL) +
    theme_minimal()+
    theme(legend.position = "bottom",
      plot.title = element_text(size = 18,
                                face = "bold",
                                hjust = 0.5),
      plot.caption = element_text(hjust = .5,
                                  face = "bold"))
  
  
  ########################################################################
  #Tempo adicional em TMA mensal em SBCF (35)
  
  
  
  BDS_KPI_08_MES %>% filter(AERO=="SBCF_40"| AERO=="SBCF_100") %>% 
    mutate(MES=factor(MES,levels = c("JAN","FEV","MAR","ABR","MAI","JUN","JUL","AGO","SET","OUT","NOV","DEZ")), ANO=as.character(ANO)) %>% 
    ggplot(aes(x = MES, y = ADICIONAL, fill = as.factor(AERO)))+
    geom_col(position=position_dodge(preserve = "single")) +
    #facet_grid(cols = vars(REGIONAL),switch= "x", scales = "free_x") +
    geom_text(aes( y = ADICIONAL, label = ADICIONAL), hjust = -.15,vjust=-.5, angle = 60, position = position_dodge(width=.9), size = 3, color = "black")+
    scale_y_continuous(limits = c(0,5))+
    scale_fill_manual(values = c("#B0C4DE","#4682B4"),labels=c("C100", "C40"))+
    labs(fill=NULL, title = "Tempo adicional em TMA mensal em SBCF", caption = "Fonte: PCICEA", x = NULL, y = NULL) +
    theme_minimal()+
    theme(legend.position = "bottom",
      plot.title = element_text(size = 18,
                                face = "bold",
                                hjust = 0.5),
      plot.caption = element_text(hjust = .5,
                                  face = "bold"))
  
  
  
################################################################################
  
#Tempo adicional em TMA por Regional e SISCEAB em 2021 (36)

  ggplot(BDS_KPI08_C40_C100_REGIONAL,aes(x = AERO, y = ADICIONAL, fill = as.factor(C40_100)))+
    geom_col(position=position_dodge(preserve = "single"))+
    geom_text(aes( y = ADICIONAL, label = ADICIONAL), hjust = -.15,vjust=-.5, angle = 60, position = position_dodge(width=.9), size = 3, color = "black")+
    scale_y_continuous(limits = c(0,5))+
    scale_fill_manual(values = c("#B0C4DE","#4682B4"),labels=c("C100", "C40"))+
    labs(fill=NULL, title = "Tempo adicional em TMA por Regional e SISCEAB em 2021", caption = "Fonte: PCICEA", x = NULL, y = "Tempo adicional em minutos") +
    theme_minimal()+
    theme(legend.position = "bottom",
      plot.title = element_text(size = 18,
                                face = "bold",
                                hjust = 0.5),
      plot.caption = element_text(hjust = .5,
                                  face = "bold"))
  
  
  
  
  
  
  
  
  
  