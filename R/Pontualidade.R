# Pontualidade
library(tidyverse)
library(lubridate)
library(dplyr)
library(plotly)
library(readr)
library(patchwork) # este pacote soma os gráficos
######################################


##################################################################################




#####################################################################################################################################################

# GRÁFICOS COM API

##############################

#PONTUALIDADE DE PARTIDA KPI-01(6)

BDS_KPI_01_DF%>% filter(ANO == "2019" |ANO == "2022") %>%
ggplot(aes(x = AERO, y = PONTUALIDADE_ANO, fill = as.factor(ANO)))+
  geom_col(position=position_dodge(preserve = "single")) +
  #geom_hline(yintercept = mean(BDS_KPI_01_DF$PONTUALIDADE_ANO), color = "red")+
  facet_grid(cols = vars(REGIONAL),switch= "x", scales = "free_x") +
  labs(fill=NULL) +
  scale_y_continuous(labels = NULL,limits = c(0,100))+
  geom_text(aes( y = PONTUALIDADE_ANO, label = scales::percent(PONTUALIDADE_ANO, accuracy = 1,scale = 1), group = ANO, hjust = -.15, angle = 90)
            , position = position_dodge(width=.9), size = 3, color = "black", fontface = "bold")+
  scale_fill_manual(values = cores)+
  labs(title = "Pontualidade de partida por aeroportos", caption = "Fonte: SIROS e TATIC FLOW", x = NULL, y = NULL) +
  theme_minimal()+
  theme(legend.position = "bottom",
    plot.title = element_text(size = 18L,
                              face = "bold",
                              hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "#8B0000"),
    plot.caption = element_text(hjust = .5,
                                face = "bold"))



############################################


#PONTUALIDADE DE PARTIDA KPI-01 MODELO 2 (6B) (FACETADO)

ggplot(BDS_KPI_01_DF,aes(x=as.factor(ANO), y=PONTUALIDADE_ANO, colour=AERO, fill=AERO,group = AERO))+
  geom_line(size=1)+
  geom_point()+
  theme_light()+
  facet_wrap(~AERO,ncol = 4)+
  scale_y_continuous(labels = scales::label_percent(scale = 1))+
  theme (legend.position = "none",strip.text = element_text(face = "italic", color = "black"))+
  labs(title = "Pontualidade de partida por aeroportos", caption = "Fonte: SIROS e TATIC FLOW", x = "ANO", y = NULL)+
  theme(plot.title = element_text(size = 18L,
                                  face = "bold",
                                  hjust = 0.5))

###################################################
#PONTUALIDADE DE PARTIDA POR REGIONAL (7)
###############
ggplot(BDS_KPI_01_REGIONAL_DF)+
  geom_col(aes(x=movimentos.aero, y=movimentos.percentual_ano, fill = as.factor(movimentos.ano)), position = "dodge")+
  geom_text(aes(x=movimentos.aero, y=movimentos.percentual_ano, label = scales::percent(movimentos.percentual_ano, accuracy = 1,scale = 1), group = movimentos.ano),size = 3,
            hjust = -0.1,
            position = position_dodge(width = 0.9),angle = 90)+
  theme_minimal() +
  labs(title = "Pontualidade de partida por REGIONAL", caption = "Fonte: SIROS e TATIC FLOW", x = NULL, y = NULL
       ,fill = "ANO") +
  theme(legend.position = "bottom",
        legend.title=element_blank()
        ,legend.text     = element_text(size = 8)
        ,legend.key.size = unit(0.3, "cm"),
        plot.title = element_text(size = 18L,
                                  face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 12, color = "#8B0000"),
        plot.caption = element_text(hjust = .5,
                                    face = "bold"))+
  scale_y_continuous(labels = NULL, limits = c(0,100))+
  scale_fill_manual(values = cores)
############################################################





##############################

#PONTUALIDADE DE chegada KPI-14(8)

BDS_KPI_14_DF%>% filter(ANO == "2019" |ANO == "2022") %>%
  ggplot(aes(x = AERO, y = PONTUALIDADE_ANO, fill = as.factor(ANO)))+
  geom_col(position=position_dodge(preserve = "single"))+
  geom_hline(yintercept = mean(BDS_KPI_01_DF$PONTUALIDADE_ANO), color = "red")+
  facet_grid(cols = vars(REGIONAL),switch= "x", scales = "free_x")+
  labs(fill=NULL)+
  scale_y_continuous(labels = NULL,limits = c(0,100))+
  #scale_y_continuous(labels = scales::label_percent(), limits = c(0,0.99))+
  geom_text(aes( y = PONTUALIDADE_ANO, label = scales::percent(PONTUALIDADE_ANO/100, accuracy = 1), group = ANO, hjust = -.15, angle = 90)
            , position = position_dodge(width=.9), size = 3, color = "black")+
  scale_fill_manual(values = cores)+
  labs(title = "Pontualidade de chegada por aeroportos", caption = "Fonte: SIROS e TATIC FLOW", x = NULL, y = NULL) +
  theme_minimal()+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 18L,
                                  face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 12, color = "#8B0000"),
        plot.caption = element_text(hjust = .5,
                                    face = "bold"))
######################################
#PONTUALIDADE DE chegada KPI-14(9) gráfico 8 facetado

ggplot(BDS_KPI_14_DF,aes(x=as.factor(ANO), y=PONTUALIDADE_ANO, colour=AERO, fill=AERO,group = AERO))+
  geom_line(size=1)+
  geom_point()+
  theme_light()+
  facet_wrap(~AERO,ncol = 4)+
  scale_y_continuous(labels = scales::label_percent(scale = 1))+
  theme (legend.position = "none",strip.text = element_text(face = "italic", color = "black"))+
  labs(title = "Pontualidade de chegada por aeroportos", caption = "Fonte: SIROS e TATIC FLOW", x = "ANO", y = NULL)+
  theme(plot.title = element_text(size = 18L,
                                  face = "bold",
                                  hjust = 0.5))




#######################################
#PONTUALIDADE DE PARTIDA SBGR(10)

BDS_KPI_01_DF %>% filter(AERO=="SBGR" & ANO==year(Sys.Date())) %>% 
  pivot_longer(cols = JAN:DEZ,
               names_to = "MES",
               values_to = "PONT") %>%
  mutate(MES=factor(MES,levels = c("JAN","FEV","MAR","ABR","MAI","JUN","JUL","AGO","SET","OUT","NOV","DEZ"))) %>% 
  ggplot(aes(x = MES, y = PONT, fill = as.factor(ANO)))+
  geom_col(position=position_dodge(preserve = "single")) +
  #geom_hline(yintercept = mean(BDS_KPI_01_DF$PONTUALIDADE_ANO), color = "red")+
  #facet_grid(cols = vars(REGIONAL),switch= "x", scales = "free_x") +
  labs(fill="Ano") +
  scale_y_continuous(labels = scales::label_percent(scale = 1), limits = c(0,100))+
  geom_text(aes( y = PONT, label = scales::percent(PONT, accuracy = 1, scale = 1), group = ANO, hjust = -.15, angle = 90)
            , position = position_dodge(width=.9), size = 3, color = "black", fontface = "bold")+
  scale_fill_manual(values = cores)+
  labs(title = "Pontualidade de partida SBGR (2022)", caption = "Fonte: SIROS e TATIC FLOW", x = NULL, y = NULL) +
  theme_minimal()+
  theme(legend.position = "none",
    plot.title = element_text(size = 18L,
                              face = "bold",
                              hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "#8B0000"),
    plot.caption = element_text(hjust = .5,
                                face = "bold"))

######################################
#PONTUALIDADE DE PARTIDA SBSP(11)

BDS_KPI_01_DF %>% filter(AERO=="SBSP" & ANO==year(Sys.Date())) %>% 
  pivot_longer(cols = JAN:DEZ,
               names_to = "MES",
               values_to = "PONT") %>%
  mutate(MES=factor(MES,levels = c("JAN","FEV","MAR","ABR","MAI","JUN","JUL","AGO","SET","OUT","NOV","DEZ"))) %>% 
  ggplot(aes(x = MES, y = PONT, fill = as.factor(ANO)))+
  geom_col(position=position_dodge(preserve = "single")) +
  #geom_hline(yintercept = mean(BDS_KPI_01_DF$PONTUALIDADE_ANO), color = "red")+
  #facet_grid(cols = vars(REGIONAL),switch= "x", scales = "free_x") +
  labs(fill="Ano") +
  scale_y_continuous(labels = scales::label_percent(scale = 1), limits = c(0,100))+
  geom_text(aes( y = PONT, label = scales::percent(PONT, accuracy = 1,scale = 1), group = ANO, hjust = -.15, angle = 90)
            , position = position_dodge(width=.9), size = 3, color = "black", fontface = "bold")+
  scale_fill_manual(values = cores)+
  labs(title = "Pontualidade de partida SBSP (2022)", caption = "Fonte: SIROS e TATIC FLOW", x = NULL, y = NULL) +
  theme_minimal()+
  theme(legend.position = "none",
    plot.title = element_text(size = 18L,
                              face = "bold",
                              hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "#8B0000"),
    plot.caption = element_text(hjust = .5,
                                face = "bold"))
############################################
#Dispesão dos aeroportos na pontualidadede DEP e ARR(12)




  ggplot(PONT_ARR_DEP, aes(x=PONT.ARR, y= PONT.DEP))+
  geom_point(aes(colour = factor(AERO)), size = 3)+
  scale_y_continuous(labels = scales::label_percent(scale = 1), n.breaks = 6)+
  scale_x_continuous(labels = scales::label_percent(scale = 1), n.breaks = 8)+
  geom_text(label = PONT_ARR_DEP$AERO, vjust = -1, size=3)+
  labs(title = "Dispesão dos aeroportos na pontualidadede DEP e ARR", caption = "Fonte: SIROS e TATIC FLOW", x = NULL, y = NULL) +
  theme_minimal()+
  theme(legend.position = "none",
        plot.title = element_text(size = 18L,
                                  face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 12, color = "#8B0000"),
        plot.caption = element_text(hjust = .5,
                                    face = "bold"))
  
  
  #######################################
  #PONTUALIDADE DE CHEGADA SBGR(13)
  
  BDS_KPI_14_DF %>% filter(AERO=="SBGR" & ANO==year(Sys.Date())) %>% 
    pivot_longer(cols = JAN:DEZ,
                 names_to = "MES",
                 values_to = "PONT") %>%
    mutate(MES=factor(MES,levels = c("JAN","FEV","MAR","ABR","MAI","JUN","JUL","AGO","SET","OUT","NOV","DEZ"))) %>% 
    ggplot(aes(x = MES, y = PONT, fill = as.factor(ANO)))+
    geom_col(position=position_dodge(preserve = "single")) +
    #geom_hline(yintercept = mean(BDS_KPI_01_DF$PONTUALIDADE_ANO), color = "red")+
    #facet_grid(cols = vars(REGIONAL),switch= "x", scales = "free_x") +
    scale_y_continuous(labels = scales::label_percent(scale = 1), limits = c(0,100))+
    geom_text(aes( y = PONT, label = scales::percent(PONT, accuracy = 1, scale = 1), group = ANO, hjust = -.15, angle = 90)
              , position = position_dodge(width=.9), size = 3, color = "black", fontface = "bold")+
    scale_fill_manual(values = cores)+
    labs(title = "Pontualidade de chegada SBGR", caption = "Fonte: SIROS e TATIC FLOW", x = NULL, y = NULL) +
    theme_minimal()+
    theme(legend.position = "none",
      plot.title = element_text(size = 20L,
                                face = "bold",
                                hjust = 0.5),
      plot.subtitle = element_text(size = 12, color = "#8B0000"),
      plot.caption = element_text(hjust = .5,
                                  face = "bold"))

###############################################################################################################################################################################


PBWG_BRA_punctuality_ADR2 <- read_csv("C:/Users/silveiralss/OneDrive - DECEA/Documentos/Relatório/Relatório/data-raw/PBWG-BRA-punctuality_ADR2.csv")
PBWG_BRA_PUNCT_ADR <- read_csv("C:/Users/silveiralss/OneDrive - DECEA/Documentos/Relatório/Relatório/data-raw/PBWG-BRA-PUNCT_ADR.csv")

  BDS_PONTUALIDADE_ARR <- PBWG_BRA_punctuality_ADR2 %>% 
    filter(PHASE =="ARR") %>% 
    separate(DATE,into =c("ANO", "M"), sep = "-") %>%
    group_by(APT_ICAO, ANO, PHASE) %>% 
     summarise(`(-1e+09.-60]`=sum(`(-1e+09.-60]`),
               `(-60.-55]`=sum(`(-60.-55]`),
               `(-55.-50]`=sum(`(-55.-50]`),
               `(-50.-45]`=sum(`(-50.-45]`),
               `(-45.-40]`=sum(`(-45.-40]`),
               `(-40.-35]`=sum(`(-40.-35]`),
               `(-35.-30]`=sum(`(-35.-30]`),
               `(-30.-25]`=sum(`(-30.-25]`),
               `(-25.-20]`=sum(`(-25.-20]`),
               `(-20.-15]`=sum(`(-20.-15]`),
               `(-15.-10]`=sum(`(-15.-10]`),
               `(-10.-5]`=sum(`(-10.-5]`),
               `(-5.0]`=sum(`(-5.0]`),
               `(0.5]`=sum(`(0.5]`),
               `(5.10]`=sum(`(5.10]`),
               `(10.15]`=sum(`(10.15]`),
               `(15.20]`=sum(`(15.20]`),
               `(20.25]`=sum(`(20.25]`),
               `(25.30]`=sum(`(25.30]`),
               `(30.35]`=sum(`(30.35]`),
               `(35.40]`=sum(`(35.40]`),
               `(40.45]`=sum(`(40.45]`),
               `(45.50]`=sum(`(45.50]`),
               `(50.55]`=sum(`(50.55]`),
               `(55.60]`=sum(`(55.60]`),
               `(60.1e+09]`=sum(`(60.1e+09]`),
               EARLY_M15M05=sum(EARLY_M15M05),
               EARLY_M05M00=sum(EARLY_M05M00), 
               LATE_P00P05=sum(LATE_P00P05),
               LATE_P05P15=sum(LATE_P05P15),
               WITHIN_M05P05=sum(WITHIN_M05P05),
               WITHIN_M15P15=sum(WITHIN_M15P15)) %>% 
    mutate(SOMA=`(-1e+09.-60]`+ `(-60.-55]`+
        `(-55.-50]`+ `(-50.-45]`+
        `(-45.-40]`+ `(-40.-35]`+
        `(-35.-30]`+ `(-30.-25]`+
        `(-25.-20]`+ `(-20.-15]`+
        `(-15.-10]`+`(-10.-5]`+
        `(-5.0]`+`(0.5]`+
        `(5.10]`+`(10.15]`+
        `(15.20]`+ `(20.25]`+
        `(25.30]`+ `(30.35]`+
        `(35.40]`+ `(40.45]`+
        `(45.50]`+ `(50.55]`+
        `(55.60]`+ `(60.1e+09]`,
      EARLY=(`(-1e+09.-60]`+ `(-60.-55]`+
           `(-55.-50]`+ `(-50.-45]`+
           `(-45.-40]`+ `(-40.-35]`+
           `(-35.-30]`+ `(-30.-25]`+
           `(-25.-20]`+ `(-20.-15]`)/SOMA,
           EARLY_15_5=(`(-15.-10]`+`(-10.-5]`)/SOMA,
           WITHIN_5=(`(-5.0]`+`(0.5]`)/SOMA,
           LATE_5_15=(`(5.10]`+`(10.15]`)/SOMA,
           LATE=(`(15.20]`+ `(20.25]`+
           `(25.30]`+ `(30.35]`+
           `(35.40]`+ `(40.45]`+
           `(45.50]`+ `(50.55]`+
           `(55.60]`+ `(60.1e+09]`)/SOMA)%>% 
    pivot_longer(cols = EARLY:LATE
                        , names_to = "SLOT"
                        , values_to = "FLIGHTS") %>% 
    mutate(SLOT=factor(SLOT, levels = c("LATE","LATE_5_15","WITHIN_5","EARLY_15_5","EARLY")))
  
CORESPONT <- c("#FF0000","#FF8C00","#FFDAB9","#87CEFA","#1E90FF")
  

  
  for (i in c("2019","2020","2021","2022")) {
    
    #png(filename = paste("C:/Users/silveiralss/OneDrive - DECEA/Documentos/Relatório/Relatório/Imagens/",i,".png"))
        
    print(ggplot(BDS_PONTUALIDADE_ARR %>% filter(ANO==i), aes(x = APT_ICAO, y = FLIGHTS, fill = SLOT))+
            geom_col(position = "fill")+
            scale_fill_manual(values = CORESPONT)+
            scale_y_continuous(labels = scales::label_percent())+
            coord_flip()+
            geom_text(aes(x = APT_ICAO, y = FLIGHTS, label = scales::percent(FLIGHTS,accuracy = 1)),size = 4, position = "fill")+
            theme_minimal()+
            labs(title = paste("Pontualidade ARR em",i,sep = " "), caption = "Fonte: DECEA", x = NULL, y = NULL
                 ,fill = NULL)+
            theme(legend.position = "bottom",
                  legend.title=element_blank(),
                  legend.text     = element_text(size = 8)
                  ,legend.key.size = unit(0.3, "cm"),
                  plot.title = element_text(size = 18L,
                                            face = "bold",
                                            hjust = 0.5),
                  plot.subtitle = element_text(size = 12, color = "#8B0000"),
                  plot.caption = element_text(hjust = .5,
                                              face = "bold")))
    #dev.off()
    
  }
  
  
############################################################################################################
  
  BDS_PONTUALIDADE_DEP <- PBWG_BRA_punctuality_ADR2 %>% 
  filter(PHASE =="DEP") %>% 
  separate(DATE,into =c("ANO", "M"), sep = "-") %>%
  group_by(APT_ICAO, ANO, PHASE) %>% 
  summarise(`(-1e+09.-60]`=sum(`(-1e+09.-60]`),
            `(-60.-55]`=sum(`(-60.-55]`),
            `(-55.-50]`=sum(`(-55.-50]`),
            `(-50.-45]`=sum(`(-50.-45]`),
            `(-45.-40]`=sum(`(-45.-40]`),
            `(-40.-35]`=sum(`(-40.-35]`),
            `(-35.-30]`=sum(`(-35.-30]`),
            `(-30.-25]`=sum(`(-30.-25]`),
            `(-25.-20]`=sum(`(-25.-20]`),
            `(-20.-15]`=sum(`(-20.-15]`),
            `(-15.-10]`=sum(`(-15.-10]`),
            `(-10.-5]`=sum(`(-10.-5]`),
            `(-5.0]`=sum(`(-5.0]`),
            `(0.5]`=sum(`(0.5]`),
            `(5.10]`=sum(`(5.10]`),
            `(10.15]`=sum(`(10.15]`),
            `(15.20]`=sum(`(15.20]`),
            `(20.25]`=sum(`(20.25]`),
            `(25.30]`=sum(`(25.30]`),
            `(30.35]`=sum(`(30.35]`),
            `(35.40]`=sum(`(35.40]`),
            `(40.45]`=sum(`(40.45]`),
            `(45.50]`=sum(`(45.50]`),
            `(50.55]`=sum(`(50.55]`),
            `(55.60]`=sum(`(55.60]`),
            `(60.1e+09]`=sum(`(60.1e+09]`),
            EARLY_M15M05=sum(EARLY_M15M05),
            EARLY_M05M00=sum(EARLY_M05M00), 
            LATE_P00P05=sum(LATE_P00P05),
            LATE_P05P15=sum(LATE_P05P15),
            WITHIN_M05P05=sum(WITHIN_M05P05),
            WITHIN_M15P15=sum(WITHIN_M15P15)) %>% 
  mutate(SOMA=`(-1e+09.-60]`+ `(-60.-55]`+
           `(-55.-50]`+ `(-50.-45]`+
           `(-45.-40]`+ `(-40.-35]`+
           `(-35.-30]`+ `(-30.-25]`+
           `(-25.-20]`+ `(-20.-15]`+
           `(-15.-10]`+`(-10.-5]`+
           `(-5.0]`+`(0.5]`+
           `(5.10]`+`(10.15]`+
           `(15.20]`+ `(20.25]`+
           `(25.30]`+ `(30.35]`+
           `(35.40]`+ `(40.45]`+
           `(45.50]`+ `(50.55]`+
           `(55.60]`+ `(60.1e+09]`,
         EARLY=(`(-1e+09.-60]`+ `(-60.-55]`+
                  `(-55.-50]`+ `(-50.-45]`+
                  `(-45.-40]`+ `(-40.-35]`+
                  `(-35.-30]`+ `(-30.-25]`+
                  `(-25.-20]`+ `(-20.-15]`)/SOMA,
         EARLY_15_5=(`(-15.-10]`+`(-10.-5]`)/SOMA,
         WITHIN_5=(`(-5.0]`+`(0.5]`)/SOMA,
         LATE_5_15=(`(5.10]`+`(10.15]`)/SOMA,
         LATE=(`(15.20]`+ `(20.25]`+
                 `(25.30]`+ `(30.35]`+
                 `(35.40]`+ `(40.45]`+
                 `(45.50]`+ `(50.55]`+
                 `(55.60]`+ `(60.1e+09]`)/SOMA)%>% 
  pivot_longer(cols = EARLY:LATE
               , names_to = "SLOT"
               , values_to = "FLIGHTS") %>% 
  mutate(SLOT=factor(SLOT, levels = c("LATE","LATE_5_15","WITHIN_5","EARLY_15_5","EARLY")))
  
    ##################
  
  for (i in c("2019","2020","2021","2022")) {
    
    print(ggplot(BDS_PONTUALIDADE_DEP %>% filter(ANO==i), aes(x = APT_ICAO, y = FLIGHTS, fill = SLOT))+
            geom_col(position = "fill")+
            scale_fill_manual(values = CORESPONT)+
            scale_y_continuous(labels = scales::label_percent())+
            coord_flip()+
            geom_text(aes(x = APT_ICAO, y = FLIGHTS, label = scales::percent(FLIGHTS,accuracy = 1)),size = 4, position = "fill")+
            theme_minimal()+
            labs(title = paste("Pontualidade DEP em",i,sep = " "), caption = "Fonte: DECEA", x = NULL, y = NULL
                 ,fill = NULL)+
            theme(legend.position = "bottom",
                  legend.title=element_blank(),
                  legend.text     = element_text(size = 8)
                  ,legend.key.size = unit(0.3, "cm"),
                  plot.title = element_text(size = 18L,
                                            face = "bold",
                                            hjust = 0.5),
                  plot.subtitle = element_text(size = 12, color = "#8B0000"),
                  plot.caption = element_text(hjust = .5,
                                              face = "bold"))
    )
    
  }
  
  
  ###############################################################################################################
 

  

  
  
  
  
  

  
  
  
  
  
  
  
  
  
  
  
 