




#CAPACIDADE DECLARADA DE ARR KPI-09(25)

BDS_KPI_09_DF %>% filter(movimentos.ano=="2019"| movimentos.ano=="2022") %>% 
  ggplot(aes(x = movimentos.aero, y = movimentos.jan, fill = as.factor(movimentos.ano)))+
  geom_col(position=position_dodge(preserve = "single")) +
  facet_grid(cols = vars(movimentos.regiao),switch= "x", scales = "free_x") +
  labs(fill=NULL) +
  scale_y_continuous(limits = c(0,60), breaks = c(15,20,50))+
  geom_text(aes( y = movimentos.jan, label = movimentos.jan, group = movimentos.ano, hjust = -.15, angle = 90)
            , position = position_dodge(width=.9), size = 3, color = "black")+
  scale_fill_manual(values = cores)+
  labs(title = "Capacidade declarada de ARR", caption = "Fonte: CGNA", x = NULL, y = "Capacidade de pouso") +
  theme_minimal()+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 18L,
                                  face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 12, color = "#8B0000"),
        plot.caption = element_text(hjust = .5,
                                    face = "bold"))
