library(tidyverse)
library(lubridate)
library(dplyr)
library(plotly)
library(readr)
library(patchwork)
library(ggbump)



##########################
#################################################################################################################################################
#GRÁFICOS COM API

#VARIABILIDADE DO TEMPO DE VOO 70%
#o API está faltando a quantidade de movimentos,já falei com o Leonardo para colocar esse dado. Quando tiver o dado
# realizar um filter > 4000 na coluna movimentos 

#Variabilidade do tempo de voo 70% (14)



BDS_KPI_15_DF %>% filter(movimentos.ano=="2019"|movimentos.ano=="2022") %>% 
  ggplot()+
  geom_col(aes(x=movimentos.fluxo, y=movimentos.percentual_ano_70, fill = as.factor(movimentos.ano)), position = position_dodge(preserve = "single"))+
  scale_y_continuous(limits = c(0,10))+
  theme_minimal()+
  labs(fill=NULL)+
  geom_text(aes(x=movimentos.fluxo, y=movimentos.percentual_ano_70, label = round(movimentos.percentual_ano_70,1), group = movimentos.ano),size = 3, 
            hjust = -0.1,
            position = position_dodge(width = 0.9),angle = 90)+
  theme(axis.text.x=element_text(angle = 90, hjust = 0))+
  labs(title = "Variabilidade do tempo de voo 70%", caption ="FONTE: VRA", x=NULL, y="Variação do tempo de voo (min)")+
  theme(legend.position = "bottom",
    plot.title = element_text(size = 18,
                              face = "bold",
                              hjust = 0.5),
    plot.caption = element_text(size = 10, face = "bold",hjust = 0.5))+
  scale_fill_manual(values = c("#B0C4DE", "#708090", "#4682B4"))

#############################################################################
#GRÁFICO COM VARIABILIDADE 70% MODELO RANKING (15)


BDS_KPI_15_DF %>% filter(movimentos.ano==year(Sys.Date())-1) %>%
ggplot()+
  geom_col(aes(x= reorder(movimentos.fluxo, -movimentos.percentual_ano_70), y=movimentos.percentual_ano_70), fill = "#4682B4")+
  geom_text(aes(x=movimentos.fluxo, y=movimentos.percentual_ano_70, label = round(movimentos.percentual_ano_70,1), group = movimentos.ano),size = 3, 
            hjust = -0.1,
            position = position_dodge(width = 0.9),angle = 90)+
  scale_y_continuous(limits = c(0,10))+
  theme_minimal()+
  theme(axis.text.x=element_text(angle = 90, hjust = 0))+
  labs(title = "Ranking variabilidade 70%", caption ="FONTE: VRA", x=NULL, y="variação do tempo de voo(min)")+
  theme(
    plot.title = element_text(size = 18,
                              face = "bold",
                              hjust = 0.5),
    plot.caption = element_text(size = 10, face = "bold",hjust = 0.5))


###############################

#Rotas mais voadas em 2022(40)

BDS_KPI_15_DF %>% filter(movimentos.ano=="2022"&movimentos.total_movimentos>2500) %>% 
  ggplot() +
  geom_col(aes(reorder(movimentos.fluxo,movimentos.total_movimentos), movimentos.total_movimentos), fill = "#191970") +
  geom_label(aes(x = movimentos.fluxo, y = movimentos.total_movimentos, label = movimentos.total_movimentos), color = "#112446") +
  scale_y_continuous(labels = NULL)+
  coord_flip() +
  labs(title = "Rotas mais voadas em 2022", y = "quantidade de voos", x = "Rota") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = 15),
    axis.title.x = element_text(size = 15,
                                face = "bold"))
