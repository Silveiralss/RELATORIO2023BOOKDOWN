




#Horas de LOGIN X horas de escala(39)

BDS_IDBR_06_ACC_DF %>% filter(movimentos.percentual_ano>0) %>% 
  mutate(movimentos.aero=case_when(movimentos.aero=="SBBS"~"ACC-BS",
                                   movimentos.aero=="SBCW"~"ACC-CW",
                                   movimentos.aero=="SBRE"~"ACC-RE",
                                   movimentos.aero=="SBAZ"~"ACC-AZ",
                                   movimentos.aero=="SBAO"~"ACC-AO")) %>%
ggplot()+
  geom_col(aes(x=movimentos.aero, y=movimentos.percentual_ano, fill = as.factor(movimentos.ano)), position = "dodge")+
  facet_grid(cols = vars(movimentos.regiao),switch= "x", scales = "free_x") +
  geom_text(aes(x=movimentos.aero, y=movimentos.percentual_ano, label = scales::percent(movimentos.percentual_ano, accuracy = 1,scale = 1), group = movimentos.ano),size = 3,
            hjust = -0.1,
            position = position_dodge(width = 0.9),angle = 90)+
  theme_minimal() +
  labs(title = "Horas de LOGIN X horas de escala", caption = "Fonte: DECEA", x = NULL, y = NULL
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


#########

#Horas de LOGIN X horas de escala por APP(40)

BDS_IDBR_06_APP_DF %>% filter(movimentos.percentual_ano>0) %>% 
  filter(movimentos.ano=="2019"|movimentos.ano=="2022") %>% 
   ggplot()+
  geom_col(aes(x=movimentos.aero, y=movimentos.percentual_ano, fill = as.factor(movimentos.ano)), position = "dodge")+
  facet_grid(cols = vars(movimentos.regiao),switch= "x", scales = "free_x") +
  geom_text(aes(x=movimentos.aero, y=movimentos.percentual_ano, label = scales::percent(movimentos.percentual_ano, accuracy = 1,scale = 1), group = movimentos.ano),size = 3,
            hjust = -0.1,
            position = position_dodge(width = 0.9),angle = 90)+
  theme_minimal() +
  labs(title = "Horas de LOGIN X horas de escala por APP", caption = "Fonte: DECEA", x = NULL, y = NULL
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


############



#Horas de LOGIN X horas de escala por TWR(41)

BDS_IDBR_06_TWR_DF %>% filter(movimentos.percentual_ano>0) %>% 
  filter(movimentos.ano=="2019"|movimentos.ano=="2022") %>% 
  ggplot()+
  geom_col(aes(x=movimentos.aero, y=movimentos.percentual_ano, fill = as.factor(movimentos.ano)), position = "dodge")+
  facet_grid(cols = vars(movimentos.regiao),switch= "x", scales = "free_x") +
  geom_text(aes(x=movimentos.aero, y=movimentos.percentual_ano, label = scales::percent(movimentos.percentual_ano, accuracy = 1,scale = 1), group = movimentos.ano),size = 3,
            hjust = -0.1,
            position = position_dodge(width = 0.9),angle = 90)+
  theme_minimal() +
  labs(title = "Horas de LOGIN X horas de escala por TWR", caption = "Fonte: DECEA", x = NULL, y = NULL
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
