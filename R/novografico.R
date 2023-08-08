













































AAAAAAA <- BDS_KPI_01_DF %>% filter(ANO%in%c(2022,2023))

ggplot(BDS_KPI_01_DF %>% filter(ANO%in%c(2022,2023)))+
  geom_col(aes(x=AERO, y=PONTUALIDADE_ANO, fill = as.factor(ANO)), position = "dodge")+
  geom_hline(aes(yintercept = 80,  lty='Target (80%)'),color="red")+
  scale_linetype(name = NULL)+
  facet_grid(cols = vars(REGIONAL),switch= "y", scales = "free_x", space = "free_x")+
  geom_text(data= BDS_KPI_01_DF %>% filter(ANO%in%c("2021")),aes(x=AERO,
                                                                 y=PONTUALIDADE_ANO, label =      percent(round(PONTUALIDADE_ANO,2), 
                                                                                                          scale= 1,accuracy = 0.1), group = ANO),size = 3.2, fontface="bold",
            hjust = -0.1, vjust=-1.8
            ,position = position_dodge(width = 0.9),angle = 90)+
  geom_text(data= BDS_KPI_01_DF %>% filter(ANO%in%c( "2022")),aes(x=AERO,
                                                                  y=PONTUALIDADE_ANO, label =      percent(round(PONTUALIDADE_ANO,2),
                                                                                                           scale= 1,accuracy = 0.1), group = ANO),size = 3.2, fontface="bold",
            hjust = -0.1
            ,position = position_dodge(width = 0.9),angle = 90)+
  geom_text(data= BDS_KPI_01_DF %>% filter(ANO%in%c("2023")),
            aes(x=AERO, y=PONTUALIDADE_ANO, label =      percent(round(PONTUALIDADE_ANO,2),
                                                                 scale= 1,accuracy = 0.1), group = ANO),size = 3.2,
            hjust = -0.1, vjust=2.4, fontface="bold"
            ,position = position_dodge(width = 0.9),angle = 90)+
  coord_cartesian(ylim = c(70, 100), expand = FALSE)+
  scale_y_continuous(limits = c(-10,100), labels = percent_format(scale=1))+
  scale_fill_manual(values = cores)+
  labs(fill=NULL, x = NULL, y = "Departure punctuality")+
  theme(legend.position = "bottom"
        ,legend.text     = element_text(size = 12),
        axis.text.x= element_text(size = 12),
        axis.title.y = element_text(size = 18),
        strip.text = element_text(size = 15),
  )
















BDS_15_2022_FIR <- BDS_KPI_15_DF %>% filter(movimentos.ano %in% c(2022))%>%
  group_by(movimentos.fluxo)%>%group_by(movimentos.fluxo)%>%
  arrange(movimentos.fluxo)
BDS_15_2023_FIR <- BDS_KPI_15_DF %>% filter(movimentos.ano %in% c(ANO.RELATORIO))%>%
  group_by(movimentos.fluxo)%>%group_by(movimentos.fluxo)%>%
  arrange(movimentos.fluxo)

BDS_VAR_15_FIR <- bind_cols(BDS_15_2022_FIR,BDS_15_2023_FIR) %>% 
  mutate(VAR=(movimentos.total_movimentos...32-movimentos.total_movimentos...9)/movimentos.total_movimentos...9)%>% mutate(COR = as.factor(ifelse(VAR > 0, yes = 1, no = 0)))

COLOCACAO <- data.frame(COLOCACAO=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))



BDS_VAR_15_FIR %>% filter(movimentos.total_movimentos...32>2000)%>%
  ggplot()+
  geom_col(aes(x = reorder(movimentos.fluxo...8, movimentos.total_movimentos...32), y = VAR, fill = COR), 
           na.rm = TRUE, color = "black",width = 0.20) +
  theme_void()+
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits=c(-1.8,1.5),labels = scales::label_percent(scale = 100))+
  scale_x_discrete(breaks = NULL)+
  guides(fill = FALSE) +
  geom_text(aes(x = movimentos.fluxo...8, y = VAR, label=scales::percent(VAR,accuracy = 0.1),hjust = ifelse(VAR < 0, 1.25, -0.25)),size= 3.5)+
  labs(title= "Var. 2022/2023",x = NULL, y = NULL) +
  scale_fill_manual(values = c("dodgerblue4","firebrick"))+
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5),
        plot.caption = element_text(size = 31))+coord_flip()





ggplot(BDS_VAR_15_FIR%>% filter(movimentos.total_movimentos...32>2000) ,
                  aes(x=movimentos.fluxo...31,y=movimentos.total_movimentos...32))+
  geom_col(aes(reorder(movimentos.fluxo...31,movimentos.total_movimentos...32),
               movimentos.total_movimentos...32), fill = "#191970")+
  geom_label(aes(x = movimentos.fluxo...31, y = movimentos.total_movimentos...32,
                 label =format(movimentos.total_movimentos...32,big.mark=".") ),
             color = "#112446",hjust=1.5, size=3)
  geom_text(aes(x = 16:1, y = -200,
                label =1:16),
            color = "#112446",hjust=1.5, size=3)+
  scale_y_continuous(limits=c(-500,19000), labels = NULL)+
  labs(y = NULL, x = "Most flown routes", title = " ") +
  theme_classic() +
  theme(plot.title = element_text(size = 10),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15,
                                    face = "bold"))+
  coord_flip()































ggplot(BDS_PONTUALIDADE_ARR %>% filter(ANO=="2022"), aes(x = APT_ICAO, y = FLIGHTS, fill = SLOT))+
  geom_col(position = position_stack(reverse = TRUE))+
  #facet_grid(rows = vars(APT_GROUP),switch= "y", scales = "free_y", space = "free_y")+
  scale_fill_manual(values = CORESPONT, labels=c(LEG_ING))+
  scale_y_continuous(labels = scales::label_percent())+
  geom_label(aes(x = APT_ICAO, y = FLIGHTS, label = scales::percent(FLIGHTS,accuracy = 1) ),fill="white",label.padding = unit(0.15,"lines"),size = 3, position = position_stack(vjust = .8,reverse = TRUE),show.legend = FALSE, fontface="bold")+
  coord_flip()+
  theme_bw()+
  labs( y = NULL, x = "Arrival Punctuality"
        ,fill = NULL)+
  theme(axis.text.x = element_text(colour = "transparent"),
        legend.position = c(.43,0),
        legend.direction = "horizontal",
        legend.justification = c("center", "top"),
        legend.backgrou= element_rect(fill="transparent"),
        legend.title=element_blank(),
        legend.text     = element_text(size = 8)
        ,legend.key.size = unit(0.3, "cm"),
        plot.title = element_text(size = 18L,
                                  face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 12, color = "#8B0000"),
        plot.caption = element_text(hjust = .5,
                                    face = "bold"))






















ggplot(BDS_VAR_ANO_DOM %>% filter(ANO==ANO.RELATORIO) %>% 
         mutate(COR = as.factor(ifelse(VAR > 0, yes = 1, no = 0))))+
  geom_col(aes(x = ANO, y = VAR, fill = COR), 
           na.rm = TRUE, color = "black",width = 0.3) +
  theme_minimal()+
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks=NULL,limits=c(-1,1),labels = scales::label_percent(scale = 100))+
  scale_x_discrete(breaks = NULL,expand = expansion(add = 1.5))+
  guides(fill = FALSE) +
  geom_text(aes(x = ANO, y = VAR, label=scales::percent(VAR,accuracy = 0.1),vjust = ifelse(VAR < 0, 1.25, -0.25)),size=3)+
  labs(x = NULL, y = NULL) +
  scale_fill_manual(values = c("firebrick","dodgerblue4"))+
  theme(panel.spacing = unit(1, "cm", data = NULL))



























ggplot(BDS_VAR_ANO_DOM)+                                                                                                 
  geom_col(aes(x=ANO, y=TOTAL, fill = ANO),width = .9, position = position_dodge(preserve = "single"))+                
  scale_y_continuous(limits = c(0,1500000),labels = comma_format(big.mark = "."))+ 
  scale_x_discrete(expand = expansion(add = .7))+
  geom_richtext(aes(x=ANO, y=290000, label= format(TOTAL,big.mark="."), group=ANO),size=5.5, position = position_dodge(width = .9), angle=90)+
  theme_classic() + 
  theme(panel.background=element_rect(fill = "#F0F8FF")
  ) +
  labs(caption = "  ", x = NULL, y = NULL   
  ) +
  theme(legend.position = "none",
        plot.caption = element_text(size = 28))+
  scale_fill_manual(values = cores)           






























linha.teste = data.table (movimentos.ano = 2021,
                          movimentos.percentual_ano = 0,
                          movimentos.regiao = "CINDACTA III",
                          movimentos.aero = "APP RF")

IDBRTESTE <- BDS_IDBR_06_APP_DF  %>% select(movimentos.ano, movimentos.percentual_ano,movimentos.regiao,movimentos.aero)
  TESTE <- bind_rows(IDBRTESTE,linha.teste)








IDBR_06_VAR_ACC%>%filter(...1!="BRASIL",...1!="CRCEA-SE")%>%
  ggplot()+
  geom_col(aes(x = ...1, y = VAR, fill = CORC),
           na.rm = TRUE, color = "black",width = 0.35) +
  theme_minimal()+
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits=c(-1,1),labels = scales::label_percent(scale = 100))+
  #coord_cartesian(xlim = c(1.05,19.8))+
  guides(fill = "none") +
  geom_text(aes(x = ...1, y = VAR, label=scales::percent(VAR,accuracy = 0.1),vjust = ifelse(VAR < 0, 1.25, -0.25)),size= 4)+
  labs(x = NULL, y = "Var. 2021/2022") +
  scale_fill_manual(values = c( "dodgerblue4","firebrick"))+
  theme(axis.text.x = element_blank(),
        axis.title.y = element_text(size = 15))






















ggplot(BDS_DEP.ARR_REG, aes(x=ARR, y= DEP))+
  geom_image(data = BDS_DEP.ARR_REG,
             aes(x=ARR, y= DEP),color="#191970",
             image="../Relatório/Imagens/twr3.png", size=0.025)+
  geom_hline(aes(yintercept = 80,  lty='Meta (80%)'),color="red")+
  geom_vline(xintercept = 80, color="red")+
  scale_y_continuous(labels = scales::label_percent(scale = 1,accuracy = 1),breaks = c(70,75,80,85,90,95,100), limits = c(75,90))+
  scale_x_continuous(labels = scales::label_percent(scale = 1,accuracy = 1), limits = c(55,80))+
  geom_text_repel(label = BDS_DEP.ARR_REG$REGIONAL,nudge_y = .1,size=3)+
  labs( x = "Pontualidade de chegada", y = "Pontualidade de partida") +
  theme_light()+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 12, color = "#8B0000"),
        plot.caption = element_text(hjust = .5,
                                    face = "bold"),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12))

























ggplot(data = read_state())+
  geom_sf()+
  geom_polygon(data = JURISDICAO, aes(x=LONGITUDE,y = LATITUDE, fill=REGIONAL),color="#666666",alpha=0.5)+
  geom_polygon(data = VERTICES_CRCEA_SE, aes(x=LONGITUDE,y = LATITUDE, fill=REGIONAL),color="#666666",alpha=0.8)+
  geom_point(data = TEXTOJURISDIÇÃO, aes(x=Y, y=X))+
  geom_segment(LINHATEXTOJURISDIÇÃOCRCEA,mapping=aes(x=x1, y=y1, xend=x2, yend=y2))+
  geom_label(data = TEXTOJURISDIÇÃO,aes(x=Y,y = X, label = VALOR),size=4,label.padding = unit(0.20, "lines"),label.r = unit(0.5, "lines"))+
  geom_label(data = TEXTOJURISDIÇÃOCRCEA,aes(x=Y,y = X, label = VALOR),size=4,label.padding = unit(0.20, "lines"),label.r = unit(0.5, "lines"))+
  geom_label(aes(x=-70, y=-24, label="SISCEAB"),label.r = unit(0.5, "lines"))+
  theme_void()+
  scale_fill_manual(values = CORESJURISDICAO)








facet_grid(cols = vars(REGIONAL),switch= "y", scales = "free_x", space = "free_x")


  ggplot(BDS_PONTUALIDADE_ARR %>% filter(ANO=="2022"), aes(x = APT_ICAO, y = FLIGHTS, fill = SLOT))+
  geom_col(position = position_stack(reverse = TRUE))+
    facet_grid(rows = vars(APT_GROUP),switch= "y", scales = "free_y", space = "free_y")+
  scale_fill_manual(values = CORESPONT)+
  scale_y_continuous(labels = scales::label_percent())+
  geom_label(aes(x = APT_ICAO, y = FLIGHTS, label = scales::percent(FLIGHTS,accuracy = 1) ),fill="white",label.padding = unit(0.15,"lines"),size = 3, position = position_stack(vjust = .8,reverse = TRUE),show.legend = FALSE, fontface="bold")+
  coord_flip()+
  theme_bw()+
  labs( y = NULL, x = "Pontualidade de chegada"
        ,fill = NULL)+
  theme(legend.position = "bottom",
        legend.justification = c("center", "top"),
        legend.title=element_blank(),
        legend.text     = element_text(size = 6)
        ,legend.key.size = unit(0.3, "cm"),
        plot.title = element_text(size = 18L,
                                  face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 12, color = "#8B0000"),
        plot.caption = element_text(hjust = .5,
                                    face = "bold"))



















  
  ggplot(BDS_VAR_C40_C100_DOM)+ 
  
  geom_col(aes(x = ...2, y = VARC100, fill = CORC100), 
           na.rm = TRUE, color = "black",width = 0.35) +
  theme_minimal()+
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits=c(-1,1),labels = scales::label_percent(scale = 100))+
  #coord_cartesian(xlim = c(1.05,19.8))+
  guides(fill = "none") +
  geom_text(aes(x = ...2, y = VARC100, label=scales::percent(VARC100,accuracy = 0.1),vjust = ifelse(VARC100 < 0, 1.25, -0.25)),size= 4)+
  labs(x = NULL, y = "Var. 2021/2022") +
  scale_fill_manual(values = c("firebrick", "dodgerblue4"))+
  theme(axis.text.x = element_blank(),
        axis.title.y = element_text(size = 15))












ggplot(data=BDS_KPI08_C40_C100%>%filter(ANO!=2023) )+
  geom_col(aes(x = AERO, y = PONTUALIDADE_ANO_C100,fill = as.factor(ANO)),
           position="dodge")+
  facet_grid(cols = vars(REGIONAL),switch= "y", scales = "free_x", space = "free_x")+
  geom_hline(data=BDS_KPI08_C40_C100 ,
             aes(yintercept = 4,  lty='Meta (4 min)'),color="red")+
  scale_linetype(name = NULL)+
  geom_text(data=BDS_KPI08_C40_C100%>%filter(ANO!=2023),aes(x=AERO,y=PONTUALIDADE_ANO_C100, 
                                                            label =      PONTUALIDADE_ANO_C100,
                                                            group = ANO),size = 3.2, fontface="bold",
            hjust = 0.5, vjust=-.5
            ,position = position_dodge(width = 0.9))+
  #scale_y_continuous(limits = c(0,6))+
  scale_fill_manual(values=c("#B0C4DE","#4682B4"))+
  labs(fill=NULL,x = NULL, y = "Tempo adicional em TMA (min) - C-100") +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 18,
                                  face = "bold",
                                  hjust = 0.5),
        plot.caption = element_text(hjust = .5,
                                    face = "bold"),
        axis.title.y = element_text(size = 18),
        axis.text.x = element_text(size = 8)
        ,legend.text     = element_text(size = 12))




















ggplot(BDS_VAR_C40_C100_DOM)+ 
  
  geom_col(aes(x = AERO, y = VARC100, fill = CORC100), 
           na.rm = TRUE, color = "black",width = 0.35) +
  theme_minimal()+
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits=c(-1,1),labels = scales::label_percent(scale = 100))+
  #coord_cartesian(xlim = c(1.05,19.8))+
  guides(fill = "none") +
  geom_text(aes(x = AERO, y = VARC100, label=scales::percent(VARC100,accuracy = 0.1),vjust = ifelse(VARC100 < 0, 1.25, -0.25)),size= 4)+
  labs(x = NULL, y = "Var. 2021/2022") +
  scale_fill_manual(values = c( "dodgerblue4","firebrick"))+
  theme(axis.text.x = element_blank(),
        axis.title.y = element_text(size = 15))


 




















# ggplot(BDS_KPI_01_DF %>% filter(ANO!="2023" & ANO!="2019"))+
#   geom_col(aes(x=AERO, y=PONTUALIDADE_ANO, fill = as.factor(ANO)), position = "dodge")+
#   geom_hline(aes(yintercept = 80,  lty='Meta (80%)'),color="red")+
#   scale_linetype(name = NULL)+
#   facet_grid(cols = vars(REGIONAL),switch= "y", scales = "free_x", space = "free_x")+
aes(x=AERO,
    y=PONTUALIDADE_ANO, label =      percent(round(PONTUALIDADE_ANO,2), 
                                             scale= 1,accuracy = 0.1), group = ANO)


 

  ggplot()+
  geom_col(data=BDS_KPI08_C40_C100 %>% filter(ANO%in%c("2022","2021")),
           aes(x = AERO, y = PONTUALIDADE_ANO_C40, fill = as.factor(ANO)),
           position="dodge") +
  facet_grid(cols = vars(REGIONAL),switch= "y", scales = "free_x", space = "free_x")+
    geom_hline(data=BDS_KPI08_C40_C100 %>% filter(ANO%in%c("2022","2021")),
               aes(yintercept = 4,  lty='Meta (4 min)'),color="red")+
    scale_linetype(name = NULL)+
    geom_text(data=BDS_KPI08_C40_C100 %>% filter(ANO%in%c("2022","2021")),
              aes(x = AERO, y = PONTUALIDADE_ANO_C40,group = ANO, label=PONTUALIDADE_ANO_C40),
              position = position_dodge(width = 0.9), vjust=-.5, size=4, fontface="bold")+
  scale_y_continuous(limits = c(0,6))+
  scale_fill_manual(values = c("#B0C4DE","#4682B4"),labels=c("C100", "C40"))+
  labs(fill=NULL,x = NULL, y = "Tempo adicional em TMA (min) - C-40") +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 18,
                                  face = "bold",
                                  hjust = 0.5),
        plot.caption = element_text(hjust = .5,
                                    face = "bold"),
        axis.title.y = element_text(size = 18),
        axis.text.x = element_text(size = 8)
        ,legend.text     = element_text(size = 12))



  # geom_hline(aes(yintercept = 4,  lty='Meta (4 min)'),color="red")
  # scale_linetype(name = NULL)


pivot_longer(cols = PONTUALIDADE_ANO_C40:PONTUALIDADE_ANO_C100,
             names_to = "C40_C100",
             values_to = "ADICIONAL") %>% 












BDS_VAR_15_FIR %>% filter(movimentos.total_movimentos...32>3500) %>% 
  ggplot() +
  geom_col(aes(reorder(movimentos.fluxo...8,movimentos.total_movimentos...32),
               movimentos.total_movimentos...32), fill = "#191970") +
  geom_label(aes(x = movimentos.fluxo...8, y = movimentos.total_movimentos...32,
                 label =format(movimentos.total_movimentos...32,big.mark=".") ),
             color = "#112446",hjust=1.5, size=3) +
  geom_text(aes(x = movimentos.fluxo...8, y = -500,
                label =c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,
                         29,30,31,32,33,34,35,36,37,38,39,40) ),
            color = "#112446", size=3, fontface="bold")+
  scale_y_continuous(limits=c(-500,19000), labels = NULL)+
  labs(y = NULL, x = "Rotas mais voadas", title = " ") +
  theme_classic() +
  theme(plot.title = element_text(size = 10),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15,
                                    face = "bold"))+
  coord_flip()














BDS_VAR_15_FIR %>% filter(movimentos.total_movimentos...32>6000)%>%
ggplot()+
  geom_col(aes(x = reorder(movimentos.fluxo...8, movimentos.total_movimentos...32), y = VAR, fill = COR), 
           na.rm = TRUE, color = "black",width = 0.20) +
  theme_void()+
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits=c(-1.8,1.5),labels = scales::label_percent(scale = 100))+
  scale_x_discrete(breaks = NULL)+
  guides(fill = FALSE) +
  geom_text(aes(x = movimentos.fluxo...8, y = VAR, label=scales::percent(VAR,accuracy = 0.1),hjust = ifelse(VAR < 0, 1.25, -0.25)),size= 3.5)+
  labs(caption= "  ",title= "Var. 2019/2022",x = NULL, y = NULL) +
  scale_fill_manual(values = c("dodgerblue4","firebrick"))+
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5),
        plot.caption = element_text(size = 31))+coord_flip()
























ggplot(BDS_DEP.ARR_REG, aes(x=ARR, y= DEP))+
  geom_image(data = BDS_DEP.ARR_REG,
             aes(x=ARR, y= DEP),color="#191970",
             image="../Relatório/Imagens/twr3.png", size=0.025)+
  geom_hline(aes(yintercept = 80,  lty='Meta (80%)'),color="red")+
  geom_vline(xintercept = 80, color="red")+
  scale_y_continuous(labels = scales::label_percent(scale = 1,accuracy = 1),breaks = c(70,75,80,85,90,95,100), limits = c(75,90))+
  scale_x_continuous(labels = scales::label_percent(scale = 1,accuracy = 1), limits = c(55,80))+
  geom_text_repel(label = BDS_DEP.ARR_REG$REGIONAL,nudge_y = .1,size=3)+
  labs( x = "Pontualidade de chegada", y = "Pontualidade de partida") +
  theme_light()+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 12, color = "#8B0000"),
        plot.caption = element_text(hjust = .5,
                                    face = "bold"),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12))







































BDS_KPI_15_DF %>% filter(movimentos.ano == "2022") %>% mutate(media=mean(movimentos.percentual_ano_70))
BDS_TAXIIN_2019 %>% mutate(media=mean(movimentos.percentual_ano))




MOVIMENTOS_AERO <- read_excel("C:/Users/silveiralss/OneDrive - DECEA/Documentos/Relatório/Relatório/data/MOVIMENTOS_AERO.xlsx")


ggplot(dataframe, aes(x = weight, y = height,
                      size = population,
                      color = cities))+
  
  # specifying the transparence of the bubbles
  # where value closer to 1 is fully opaque and
  # value closer to 0 is completely transparent
  geom_point(alpha = 0.7)+
  
  # setting the scale of sizes of the bubbles using
  # range parameter where the smallest size is 0.1
  # and the largest one is 10
  # name of the size legend is Population
  scale_size(range = c(0.1, 10), name = "Population")+
  
  # specifying the title for the plot
  ggtitle("Height and Weight Data of 4 Cities")+
  
  # code to center the title which is left aligned
  # by default
  theme(plot.title = element_text(hjust = 0.5))





######################################################


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
  scale_fill_manual(values = CORESJURISDICAO)+
   geom_point(data=NOME_APP_C3,aes(x=LONG, y=LAT),alpha=0)+
  geom_text(data = NOME_APP_C3, aes(x=LONG, y=LAT, label=NUMERO), size=3,hjust=-.7, fontface = "bold")+
  geom_label(aes(x = -30, y = 1,label = "             3.1-TERESINA           3.2-FORTALEZA
  3.3-NATAL               3.4-RECIFE
        3.5-MACEIO             3.6-ARACAJU
  3.7-SALVADOR      3.8-ILHÉUS
     3.9-P.SEGURO         3.11-VITÓRIA"),size=3,
             fill = "#CD5C5C")+
  geom_label(aes(x = -60, y = 8,label = "    4.1-B.VISTA          4.2-MANAUS
     4.3-SANTARÉM     4.4-MACAPÁ
      4.5-BELÉM              4.6-MARABÁ
    4.7-CUIABÁ            4.8-P.VELHO
           4.9-R.BRANCO       4.10-AMAZÔNICA"),size=3,
fill = "#2E8B57")+
  geom_label(aes(x = -65, y = -20,label = "         1.1-PALMAS             1.2-BRASÍLIA
               1.3-ANÁPOLIS          1.4-B.HORIZONTE
          1.5-UBERLÂNDIA      1.6-UBERABA
      1.7-ACADEMIA           1.8-BAURU"),size=3,
  fill = "#F0E68C")+
  geom_label(aes(x = -32, y = -20,label = "  5.1-S.PAULO
         5.2 R.DE JANEIRO
5.3-MACAÉ"),size=3,
     fill = "#9370DB")+
  geom_label(aes(x = -35, y = -29,label = "       2.1-C.GRANDE                      2.2-P.PRUDENTE      
 2.3-LONDRINA                     2.4-CURITIBA     
   2.5-FLORIANÓPOLIS            2.6-P.ALEGRE      
           2.7-S.MARIA                        2.8-P.DE LOS LIBRES    
2.9-FOZ                                                         "),size=3,
          fill = "#87CEFA")+
  theme_void()+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none")+
    labs(x=NULL, y=NULL)

   












NOME_APP_C3 <- read_excel("C:/Users/silveiralss/OneDrive - DECEA/Documentos/Relatório/Relatório/data/NOME APP C3.xlsx")



ggplot()+
  geom_point(data=NOME_APP_C3,aes(x=LONG, y=LAT, fill= LEGENDA))





########################################


BDS_KPI_15_DF %>% filter(movimentos.ano%in% c(2022)&movimentos.total_movimentos>2500) %>%  
  ggplot()+
  geom_col(aes(x=reorder(movimentos.fluxo, -movimentos.percentual_ano_70), y=movimentos.percentual_ano_70, fill = as.factor(movimentos.ano)), 
           position = position_dodge(preserve = "single"))+
  scale_y_continuous(limits = c(0,10))+
  theme_minimal()+
  labs(fill=NULL)+
  geom_text(aes(x=movimentos.fluxo, y=movimentos.percentual_ano_70, label = round(movimentos.percentual_ano_70,1), group = movimentos.ano),size = 3,vjust=.5,hjust=-.5,
            position = position_dodge(width = 0.9))+
  geom_text(data=VAR15,aes(x=ROTA, y=9, label = scales::percent(VAR,accuracy = 0.1)), size = 3,color=ifelse(VAR15$VAR>=0,"blue","red"),
            hjust = 0.5,
            position = position_dodge(width = 0.9),fontface="bold")+
  theme(axis.text.x=element_text(angle = 90, hjust = 0),
        plot.margin = margin(1,0,0,0,"cm"),
        axis.title.x= element_text(vjust = 211, size = 15))+
  labs(caption ="FONTE: VRA", x=NULL, y="Variação do tempo de voo (min)                             Variação 19/22")+
  theme(legend.position = "none",
        plot.title = element_text(size = 18,
                                  face = "bold",
                                  hjust = 0.5),
        plot.caption = element_text(size = 10, face = "bold",hjust = 0.5))+
  scale_fill_manual(values = c("#B0C4DE", "#708090", "#4682B4"))+
  coord_flip()











##############################################################


MOV_GERAL <- BDS_KPI_GERAL_DF %>% select(movimentos.ano, movimentos.total,movimentos.jan:movimentos.dez) %>% 
  group_by(movimentos.ano) %>% summarise(movimentos.total = sum(movimentos.total),
                                         movimentos.jan = sum(movimentos.jan),
                                         movimentos.fev = sum(movimentos.fev),
                                         movimentos.mar = sum(movimentos.mar),
                                         movimentos.abr = sum(movimentos.abr),
                                         movimentos.mai = sum(movimentos.mai),
                                         movimenos.jun = sum(movimentos.jun),
                                         movimentos.jul = sum(movimentos.jul),
                                         movimentos.ago = sum(movimentos.ago),
                                         movimentoss.set = sum(movimentos.set),
                                         movimentos.out = sum(movimentos.out),
                                         movimentos.nov = sum(movimentos.nov),
                                         movimentos.dez = sum(movimentos.dez)) %>% 
  `colnames<-`(COL.NAMES.MOV) %>% pivot_longer(cols = JAN:DEZ,
                                               names_to = "MES",
                                               values_to = "MOV") %>% 
  mutate(ANO=as.factor(ANO),MES= factor(MES, levels = c("JAN","FEV","MAR","ABR","MAI","JUN","JUL","AGO","SET","OUT","NOV","DEZ")))



#GRÁFICO MOVIMENTO AVIAÇÃO COMERCIAL INTERNACIONAL (2)
#AJUSTAR A BDS PARA REPLICAR O GRÁFICO PARA OUTROS ANOS




BDS_VAR_2019_GERAL <- MOV_GERAL %>% filter(ANO %in% c(2019), MES=="JAN")
BDS_VAR_2022_GERAL <- MOV_GERAL %>% filter(ANO %in% c(2022), MES=="JAN")
BDS_VAR_ANO_GERAL <- bind_rows(BDS_VAR_2019_GERAL,BDS_VAR_2022_GERAL) %>% 
  transmute(ANO=as.factor(ANO), TOTAL=TOTAL,VAR=(BDS_VAR_2022_GERAL$TOTAL-BDS_VAR_2019_GERAL$TOTAL)/BDS_VAR_2019_GERAL$TOTAL)

BDS_GERAL_VAR <- bind_cols(MOV_GERAL %>% filter(ANO %in% c(2019)),MOV_GERAL %>% filter(ANO %in% c(2022)) %>% select(MOV)) %>% 
  transmute(TOTAL=TOTAL,ANO=MES,MOV19=MOV...4,MOV22=MOV...5) %>% mutate(VAR= (MOV22-MOV19)/MOV19)%>% 
  mutate(ANO= factor(ANO, levels = c("JAN","FEV","MAR","ABR","MAI","JUN","JUL","AGO","SET","OUT","NOV","DEZ")),
         COR = as.factor(ifelse(VAR > 0, yes = 1, no = 0)))



GFVARANO_GERAL <-  
  ggplot(BDS_VAR_ANO_GERAL)+                                                                                                 
  geom_col(aes(x=ANO, y=TOTAL, fill = ANO),width = .5, position = position_dodge(preserve = "single"))+                
  scale_y_continuous(labels = comma_format(big.mark = "."))+ 
  scale_x_discrete(expand = expansion(add = .7))+
  geom_richtext(aes(x=ANO, y=TOTAL/4, label= format(TOTAL,big.mark="."), group=ANO),size=3, position = position_dodge(width = .9), angle=90)+
  theme_classic() + 
  theme(panel.background=element_rect(fill = "#F0F8FF")
  ) +
  labs(caption = "  ", x = NULL, y = NULL   
  ) +
  theme(legend.position = "none",
        plot.caption = element_text(size = 43))+
  scale_fill_manual(values = cores)


GFVARMES_GERAL <- 
  MOV_GERAL %>% filter(ANO %in% c(2019, 2022)) %>%
  ggplot()+
  geom_col(aes(x=MES, y=MOV, fill = ANO), position = position_dodge(preserve = "single"))+
  scale_y_continuous(labels = comma_format(big.mark = ".")) +
  coord_cartesian(xlim = c(1,12))+
  geom_richtext(aes(x=MES, y=2000, label = ifelse(MOV>999,format(MOV,big.mark="."),MOV), group=ANO),size=3, position = position_dodge(width = .9), angle=90)+
  theme_classic()+
  theme(legend.title    = element_text(size = 8) 
        ,legend.text     = element_text(size = 8)
        ,legend.key.size = unit(0.3, "cm")
  ) +
  labs(caption = "Fonte: TATIC FLOW", x = NULL, y = "Movimentos"
       ,fill = "ANO") +
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        plot.caption = element_text(hjust = .65,
                                    face = "bold"))+
  scale_fill_manual(values = cores)             







GFVARGERAL_MES <- 
  ggplot(BDS_GERAL_VAR)+
  geom_col(aes(x = ANO, y = VAR, fill = COR), 
           na.rm = TRUE, color = "black",width = 0.30) +
  theme_minimal()+
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits=c(-1.8,1.5),labels = scales::label_percent(scale = 100))+
  scale_x_discrete(breaks = NULL)+
  coord_cartesian(xlim = c(1,12))+
  guides(fill = FALSE) +
  geom_text(aes(x = ANO, y = VAR, label=scales::percent(VAR,accuracy = 0.1),vjust = ifelse(VAR < 0, 1.25, -0.25)),size= 3.5)+
  labs(title= "Mensal",x = NULL, y = "Variação 2022 / 2019") +
  scale_fill_manual(values = c("firebrick", "dodgerblue4"))+
  theme(panel.spacing = unit(1, "cm", data = NULL),
        plot.title = element_text(size = 12,
                                  face = "bold",
                                  hjust = 0.5))


GFVARGERAL_ANO <- 
  ggplot(BDS_VAR_ANO_GERAL %>% filter(ANO=="2022") %>% 
           mutate(COR = as.factor(ifelse(VAR > 0, yes = 1, no = 0))))+
  geom_col(aes(x = ANO, y = VAR, fill = COR), 
           na.rm = TRUE, color = "black",width = 0.30) +
  theme_minimal()+
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks=NULL,limits=c(-1.5,1.5),labels = scales::label_percent(scale = 100))+
  scale_x_discrete(breaks = NULL,expand = expansion(add = 1.5))+
  guides(fill = FALSE) +
  geom_text(aes(x = ANO, y = VAR, label=scales::percent(VAR,accuracy = 0.1),vjust = ifelse(VAR < 0, 1.25, -0.25)),size=3.5)+
  labs(title= "Anual",x = NULL, y = NULL) +
  scale_fill_manual(values = c("firebrick", "dodgerblue4"))+
  theme(panel.spacing = unit(1, "cm", data = NULL),
        plot.title = element_text(size = 12,
                                  face = "bold",
                                  hjust = 0.5))


grid.arrange(GFVARGERAL_MES,GFVARGERAL_ANO,GFVARMES_GERAL,GFVARANO_GERAL, ncol=2,widths=c(4,1),heights= c(1.4,4))%>% 
  ggsave(filename = "C:/Users/silveiralss/OneDrive - DECEA/Documentos/Relatório/Relatório/Imagens/gf49.png",height = 18,width=25, units = "cm")







###########################################################

Dados_Base34_2022 <- read_excel("C:/Users/silveiralss/OneDrive - DECEA/Documentos/Relatório/Relatório/data/Dados_Base34_2022.xlsx", 
                                col_types = c("date", "text", "numeric"))
View(Dados_Base34_2022)





Dados_Base34_2022 <-Dados_Base34_2022  %>%
  mutate( ROLLSEV = rollmean(x=Total, k=7, fill = NA))

Dados_Base34_2022 %>% ggplot(aes(x=Data)) +
  geom_point(aes(y = Total), color= "#4682B4",size=.5) +
  geom_line(aes(y = ROLLSEV), color = "royalblue4") +
  labs( x = "Date", y = "Departures") +
  theme_minimal()








ggplot() + geom_bar(aes(x=date, y=count, fill="Casos"), stat = "identity", position = position_dodge(1))+
  ggtitle(paste(BAn$state[1])) +
  labs(subtitle = paste("Média móvel de 7 dias = ",round(last(na.omit(sm)),0), "óbitos")) +
  geom_line(aes(x=date,y=mm, fill="Média Movel"), color="blue", size=1.5) + 
  #  ylim(0, 150)+
  ylab("Óbitos") + 
  xlab("")+
  scale_x_date(
    date_minor_breaks = "1 day",
    breaks = "2 week",
    date_labels = "%d-%b-%Y") + 
  theme_classic() + 
  theme(legend.position = "none",
        text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1)) 











###################################################################
#GRÁFICO IDBR 06

BDS_IDBR_06_ACC_DF %>%
  filter(movimentos.ano == 2022& ACC%in%c("ACC Brasília","ACC Curitiba","ACC Recife","ACC Amazônico","ACC Atlântico")) %>% 
  ggplot()+
  geom_col(aes(x=ACC, y=movimentos.percentual_ano, fill = ACC), position = "dodge",fill="#4682B4")+
  geom_text(aes(x=ACC, y=movimentos.percentual_ano, label = percent(round(movimentos.percentual_ano)/100), group = ACC),size = 3,
            vjust=-0.4,
            position = position_dodge(width = 0.9))+
  scale_y_continuous(limits = c(0,100))+
  theme_void() +
  labs(title = "IDBR 06 por ACC", caption = "Fonte: SAGITARIO", x = NULL, y = NULL
       ,fill = "ANO") +
  theme(legend.position = "none",
        legend.title=element_blank()
        ,legend.text     = element_text(size = 8)
        ,legend.key.size = unit(0.3, "cm"),
        plot.title = element_text(size = 18,
                                  face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 12, color = "#8B0000"),
        plot.caption = element_text(hjust = .5,
                                    face = "bold"))






















#####################################


VAR15_2022 <- BDS_KPI_15_DF %>% filter(movimentos.ano=="2022") %>% filter(movimentos.total_movimentos>3500)#QUANDO NECESSÁRIO É SÓ SUBSTITUIR O "2000" PELO VALOR NECESSÁRIO
VAR15_2019 <- BDS_KPI_15_DF %>% filter(movimentos.ano=="2019") %>% filter(movimentos.fluxo%in%VAR15_2022$movimentos.fluxo)
VAR15 <- bind_cols(VAR15_2019,VAR15_2022$movimentos.percentual_ano_70,VAR15_2022$movimentos.total_movimentos) %>% 
  #FOI CRIADO O VAR15 PARA CRIAR O LABEL DE VARIAÇÃO 
  transmute(ROTA=movimentos.fluxo,VAR19=movimentos.percentual_ano_70,VAR22=...24,MOVIMENTOS.19=movimentos.total_movimentos, MOVIMENTOS.22=...25 ) %>% 
  mutate(VAR= (VAR22-VAR19)/VAR19, VAR.ANO=(MOVIMENTOS.22-MOVIMENTOS.19)/MOVIMENTOS.19)



BDS_KPI_15_DF %>% filter(movimentos.ano=="2022"&movimentos.total_movimentos>3500) %>% 
  ggplot() +
  geom_col(aes(reorder(movimentos.fluxo,movimentos.total_movimentos), movimentos.total_movimentos), fill = "#191970") +
  geom_label(aes(x = movimentos.fluxo, y = movimentos.total_movimentos, label = movimentos.total_movimentos), color = "#112446",hjust=1.5, size=3) +
  geom_text(data=VAR15,aes(x=ROTA, y=14000, label = scales::percent(VAR.ANO,accuracy = 0.1)), size = 3,color= ifelse(VAR15$VAR.ANO>=0,"blue","red"),
            hjust = 0.5,
            position = position_dodge(width = 0.9),fontface="bold")+
  scale_y_continuous(limits=c(0,15000), labels = NULL)+
  coord_flip() +
  labs(title = "Rotas mais voadas em 2022", y = "quantidade de voos", x = "Rota", subtitle = "2019/2022") +
  theme_minimal() +
  theme(plot.subtitle = element_text(face = "bold",hjust = 0.95),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = 15),
    axis.title.x = element_text(size = 15,
                                face = "bold"))







##########################################
BDS_KPI_COMER <- GET("http://siatfm.cgna.intraer/api/performance?apiUser=silveiralss&apiToken=7a11719421134986058a25ea58a3b002e2c185d5&area=Comercial")
BDS_KPI_COMER_TEXT <- content(BDS_KPI_COMER, "text")
BDS_KPI_COMER_JSON <- fromJSON(BDS_KPI_COMER_TEXT,flatten = TRUE)
BDS_KPI_COMER_DF <- as.data.frame(BDS_KPI_COMER_JSON)
BDS.COMER <- BDS_KPI_COMER_DF %>% select(movimentos.ano, movimentos.aero: movimentos.dez) %>% mutate(TIPO= "Comercial",REGIONAL = case_when(movimentos.aero=="SBBE"~"CINDACTA IV",
                                                                                                                                      movimentos.aero=="SBBI"~"CINDACTA II",
                                                                                                                                      movimentos.aero=="SBBR"~"CINDACTA I",
                                                                                                                                      movimentos.aero=="SBCF"~"CINDACTA I",
                                                                                                                                      movimentos.aero=="SBCG"~"CINDACTA I",
                                                                                                                                      movimentos.aero=="SBCO"~"CINDACTA II",
                                                                                                                                      movimentos.aero=="SBCT"~"CINDACTA II",
                                                                                                                                      movimentos.aero=="SBCY"~"CINDACTA I",
                                                                                                                                      movimentos.aero=="SBEG"~"CINDACTA IV",
                                                                                                                                      movimentos.aero=="SBFI"~"CINDACTA II",
                                                                                                                                      movimentos.aero=="SBFL"~"CINDACTA II",
                                                                                                                                      movimentos.aero=="SBBE"~"CINDACTA IV",
                                                                                                                                      movimentos.aero=="SBFN"~"CINDACTA III",
                                                                                                                                      movimentos.aero=="SBFZ"~"CINDACTA III",
                                                                                                                                      movimentos.aero=="SBGL"~"CRCEA-SE",
                                                                                                                                      movimentos.aero=="SBGR"~"CRCEA-SE",
                                                                                                                                      movimentos.aero=="SBKP"~"CRCEA-SE",
                                                                                                                                      movimentos.aero=="SBME"~"CINDACTA II",
                                                                                                                                      movimentos.aero=="SBMN"~"CINDACTA IV",
                                                                                                                                      movimentos.aero=="SBMO"~"CINDACTA III",
                                                                                                                                      movimentos.aero=="SBMT"~"CRCEA-SE",
                                                                                                                                      movimentos.aero=="SBNT"~"CINDACTA III",
                                                                                                                                      movimentos.aero=="SBPA"~"CINDACTA II",
                                                                                                                                      movimentos.aero=="SBPS"~"CINDACTA III",
                                                                                                                                      movimentos.aero=="SBSV"~"CINDACTA III",
                                                                                                                                      movimentos.aero=="SBPV"~"CINDACTA IV",
                                                                                                                                      movimentos.aero=="SBRB"~"CINDACTA IV",
                                                                                                                                      movimentos.aero=="SBRF"~"CINDACTA III",
                                                                                                                                      movimentos.aero=="SBRJ"~"CRCEA-SE",
                                                                                                                                      movimentos.aero=="SBSC"~"CINDACTA II",
                                                                                                                                      movimentos.aero=="SBSJ"~"CRCEA-SE",
                                                                                                                                      movimentos.aero=="SBSL"~"CINDACTA III",
                                                                                                                                      movimentos.aero=="SBSM"~"CINDACTA II",
                                                                                                                                      movimentos.aero=="SBSP"~"CRCEA-SE",
                                                                                                                                      movimentos.aero=="SBYS"~"CINDACTA I"))


BDS_KPI_GERAL <- GET("http://siatfm.cgna.intraer/api/performance?apiUser=silveiralss&apiToken=7a11719421134986058a25ea58a3b002e2c185d5&area=Geral")
BDS_KPI_GERAL_TEXT <- content(BDS_KPI_GERAL, "text")
BDS_KPI_GERAL_JSON <- fromJSON(BDS_KPI_GERAL_TEXT,flatten = TRUE)
BDS_KPI_GERAL_DF <- as.data.frame(BDS_KPI_GERAL_JSON)
BDS.GERAL <- BDS_KPI_GERAL_DF %>% select(movimentos.ano, movimentos.aero: movimentos.dez) %>% mutate(TIPO= "Geral",REGIONAL = case_when(movimentos.aero=="SBBE"~"CINDACTA IV",
                                                                                                                                      movimentos.aero=="SBBI"~"CINDACTA II",
                                                                                                                                      movimentos.aero=="SBBR"~"CINDACTA I",
                                                                                                                                      movimentos.aero=="SBCF"~"CINDACTA I",
                                                                                                                                      movimentos.aero=="SBCG"~"CINDACTA I",
                                                                                                                                      movimentos.aero=="SBCO"~"CINDACTA II",
                                                                                                                                      movimentos.aero=="SBCT"~"CINDACTA II",
                                                                                                                                      movimentos.aero=="SBCY"~"CINDACTA I",
                                                                                                                                      movimentos.aero=="SBEG"~"CINDACTA IV",
                                                                                                                                      movimentos.aero=="SBFI"~"CINDACTA II",
                                                                                                                                      movimentos.aero=="SBFL"~"CINDACTA II",
                                                                                                                                      movimentos.aero=="SBBE"~"CINDACTA IV",
                                                                                                                                      movimentos.aero=="SBFN"~"CINDACTA III",
                                                                                                                                      movimentos.aero=="SBFZ"~"CINDACTA III",
                                                                                                                                      movimentos.aero=="SBGL"~"CRCEA-SE",
                                                                                                                                      movimentos.aero=="SBGR"~"CRCEA-SE",
                                                                                                                                      movimentos.aero=="SBKP"~"CRCEA-SE",
                                                                                                                                      movimentos.aero=="SBME"~"CINDACTA II",
                                                                                                                                      movimentos.aero=="SBMN"~"CINDACTA IV",
                                                                                                                                      movimentos.aero=="SBMO"~"CINDACTA III",
                                                                                                                                      movimentos.aero=="SBMT"~"CRCEA-SE",
                                                                                                                                      movimentos.aero=="SBNT"~"CINDACTA III",
                                                                                                                                      movimentos.aero=="SBPA"~"CINDACTA II",
                                                                                                                                      movimentos.aero=="SBPS"~"CINDACTA III",
                                                                                                                                      movimentos.aero=="SBSV"~"CINDACTA III",
                                                                                                                                      movimentos.aero=="SBPV"~"CINDACTA IV",
                                                                                                                                      movimentos.aero=="SBRB"~"CINDACTA IV",
                                                                                                                                      movimentos.aero=="SBRF"~"CINDACTA III",
                                                                                                                                      movimentos.aero=="SBRJ"~"CRCEA-SE",
                                                                                                                                      movimentos.aero=="SBSC"~"CINDACTA II",
                                                                                                                                      movimentos.aero=="SBSJ"~"CRCEA-SE",
                                                                                                                                      movimentos.aero=="SBSL"~"CINDACTA III",
                                                                                                                                      movimentos.aero=="SBSM"~"CINDACTA II",
                                                                                                                                      movimentos.aero=="SBSP"~"CRCEA-SE",
                                                                                                                                      movimentos.aero=="SBYS"~"CINDACTA I"))



BDS_KPI_MIL <- GET("http://siatfm.cgna.intraer/api/performance?apiUser=silveiralss&apiToken=7a11719421134986058a25ea58a3b002e2c185d5&area=Militar")
BDS_KPI_MIL_TEXT <- content(BDS_KPI_MIL, "text")
BDS_KPI_MIL_JSON <- fromJSON(BDS_KPI_MIL_TEXT,flatten = TRUE)
BDS_KPI_MIL_DF <- as.data.frame(BDS_KPI_MIL_JSON)
BDS.MIL <- BDS_KPI_MIL_DF %>% select(movimentos.ano, movimentos.aero: movimentos.dez) %>% mutate(TIPO= "Militar",REGIONAL = case_when(movimentos.aero=="SBBE"~"CINDACTA IV",
                                                                                                                      movimentos.aero=="SBBI"~"CINDACTA II",
                                                                                                                      movimentos.aero=="SBBR"~"CINDACTA I",
                                                                                                                      movimentos.aero=="SBCF"~"CINDACTA I",
                                                                                                                      movimentos.aero=="SBCG"~"CINDACTA I",
                                                                                                                      movimentos.aero=="SBCO"~"CINDACTA II",
                                                                                                                      movimentos.aero=="SBCT"~"CINDACTA II",
                                                                                                                      movimentos.aero=="SBCY"~"CINDACTA I",
                                                                                                                      movimentos.aero=="SBEG"~"CINDACTA IV",
                                                                                                                      movimentos.aero=="SBFI"~"CINDACTA II",
                                                                                                                      movimentos.aero=="SBFL"~"CINDACTA II",
                                                                                                                      movimentos.aero=="SBBE"~"CINDACTA IV",
                                                                                                                      movimentos.aero=="SBFN"~"CINDACTA III",
                                                                                                                      movimentos.aero=="SBFZ"~"CINDACTA III",
                                                                                                                      movimentos.aero=="SBGL"~"CRCEA-SE",
                                                                                                                      movimentos.aero=="SBGR"~"CRCEA-SE",
                                                                                                                      movimentos.aero=="SBKP"~"CRCEA-SE",
                                                                                                                      movimentos.aero=="SBME"~"CINDACTA II",
                                                                                                                      movimentos.aero=="SBMN"~"CINDACTA IV",
                                                                                                                      movimentos.aero=="SBMO"~"CINDACTA III",
                                                                                                                      movimentos.aero=="SBMT"~"CRCEA-SE",
                                                                                                                      movimentos.aero=="SBNT"~"CINDACTA III",
                                                                                                                      movimentos.aero=="SBPA"~"CINDACTA II",
                                                                                                                      movimentos.aero=="SBPS"~"CINDACTA III",
                                                                                                                      movimentos.aero=="SBSV"~"CINDACTA III",
                                                                                                                      movimentos.aero=="SBPV"~"CINDACTA IV",
                                                                                                                      movimentos.aero=="SBRB"~"CINDACTA IV",
                                                                                                                      movimentos.aero=="SBRF"~"CINDACTA III",
                                                                                                                      movimentos.aero=="SBRJ"~"CRCEA-SE",
                                                                                                                      movimentos.aero=="SBSC"~"CINDACTA II",
                                                                                                                      movimentos.aero=="SBSJ"~"CRCEA-SE",
                                                                                                                      movimentos.aero=="SBSL"~"CINDACTA III",
                                                                                                                      movimentos.aero=="SBSM"~"CINDACTA II",
                                                                                                                      movimentos.aero=="SBSP"~"CRCEA-SE",
                                                                                                                      movimentos.aero=="SBYS"~"CINDACTA I"))





PARTICIPACAO.AERO <- bind_rows(BDS.MIL, BDS.GERAL,BDS.COMER) %>% 
  group_by(movimentos.ano, REGIONAL, TIPO, movimentos.aero) %>% 
  summarise(TOTAL=sum(movimentos.total),
            JAN=sum(movimentos.jan),
            FEV=sum(movimentos.fev),
            MAR= sum(movimentos.mar),
            ABR=sum(movimentos.abr),
            MAI=sum(movimentos.mai),
            JUN=sum(movimentos.jun),
            JUL=sum(movimentos.jul),
            AGO=sum(movimentos.ago),
            SET=sum(movimentos.set),
            OUT=sum(movimentos.out),
            NOV=sum(movimentos.nov),
            DEZ=sum(movimentos.dez))

PARTICIPACAO.TOTAL <- bind_rows(BDS.MIL, BDS.GERAL,BDS.COMER) %>% 
  group_by(movimentos.ano, REGIONAL, TIPO) %>% 
  summarise(QTD=sum(movimentos.total),
            JAN=sum(movimentos.jan),
            FEV=sum(movimentos.fev),
            MAR= sum(movimentos.mar),
            ABR=sum(movimentos.abr),
            MAI=sum(movimentos.mai),
            JUN=sum(movimentos.jun),
            JUL=sum(movimentos.jul),
            AGO=sum(movimentos.ago),
            SET=sum(movimentos.set),
            OUT=sum(movimentos.out),
            NOV=sum(movimentos.nov),
            DEZ=sum(movimentos.dez)) %>% 
  select(movimentos.ano, REGIONAL,TIPO, QTD) %>% 
  mutate(TOTAL=sum(QTD)) %>% 
  mutate(PRC=round(QTD/TOTAL,digits = 2))




ggplot(PARTICIPACAO.TOTAL,aes(x = TIPO, y = PRC, fill = as.factor(movimentos.ano)))+
  geom_col(position=position_dodge(preserve = "single")) +
  facet_grid(cols = vars(REGIONAL),switch= "x", scales = "free_x", space = "free_x") +
  labs(fill=NULL) +
  scale_y_continuous(breaks = NULL)+
  geom_text(aes( y = PRC, label = PRC, group = movimentos.ano, vjust = -.4)
            , position = position_dodge(width=.9), size = 2.5, color = "black")+
  scale_fill_manual(values = cores)+
  labs(title = "Participação dos segmentos de aviação", caption = "Fonte: TATIC", x = NULL, y = NULL) +
  theme_minimal()+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 18,
                                  face = "bold",
                                  hjust = 0.5),
        plot.caption = element_text(hjust = .5,
                                    face = "bold"),
        axis.text.x = element_text(size = 7))





###################################################################################################





BDS_KPI_TOTAL <- GET("http://siatfm.cgna.intraer/api/performance?apiUser=silveiralss&apiToken=7a11719421134986058a25ea58a3b002e2c185d5&area=Total")
BDS_KPI_TOTAL_TEXT <- content(BDS_KPI_TOTAL, "text")
BDS_KPI_TOTAL_JSON <- fromJSON(BDS_KPI_TOTAL_TEXT,flatten = TRUE)
BDS_KPI_TOTAL_DF <- as.data.frame(BDS_KPI_TOTAL_JSON)


BDS_KPI_TOTAL_DF %>% filter(movimentos.ano=="2022"& movimentos.aero!= "NA") %>% 
  ggplot() +
  geom_col(aes(reorder(movimentos.aero,movimentos.total), movimentos.total), fill = "#708090") +
  geom_label(aes(x = movimentos.aero, y = movimentos.total, label = movimentos.total), hjust= -.1,color = "#112446") +
  scale_y_continuous(labels = NULL)+
  coord_flip() +
  labs(title = "Ranking de movimentos dos aeroportos", y = "quantidade de voos") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 15,
                                face = "bold"))














#################





ggplot(PONT_ARR_DEP, aes(x=PONT.ARR, y= PONT.DEP))+
  geom_image(data = PONT_ARR_DEP,
             aes(x=PONT.ARR, y= PONT.DEP,colour = factor(AERO)),
                 image="C:/Users/silveiralss/OneDrive - DECEA/Documentos/Relatório/Relatório/Imagens/twr3.png", size=0.025)+
  scale_y_continuous(labels = scales::label_percent(scale = 1,accuracy = 1), n.breaks = 6)+
  scale_x_continuous(labels = scales::label_percent(scale = 1,accuracy = 1), n.breaks = 8)+
  geom_text_repel(label = PONT_ARR_DEP$AERO, nudge_y = 1.5,size=3)+
  labs(title = "Dispesão dos aeroportos na pontualidadede DEP e ARR", caption = "Fonte: SIROS e TATIC FLOW", x = "ARR", y = "DEP") +
  theme_minimal()+
  theme(legend.position = "none",
        plot.title = element_text(size = 18,
                                  face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 12, color = "#8B0000"),
        plot.caption = element_text(hjust = .5,
                                    face = "bold"))












###################################################################################

#gráfico de capacidade de ARR x TAXA PICO 

###################################################################################



fundoceu <- readJPEG(system.file("C:/Users/silveiralss/OneDrive - DECEA/Documentos/Relatório/Relatório/Imagens/ceu.jpg"))





  ggplot()+
    geom_image(data = BDS_KPI_10_DF%>% dplyr::filter(movimentos.ano == "2022"),
               aes(x = movimentos.aero, y = movimentos.jan, fill = as.factor(movimentos.ano)),
               image="C:/Users/silveiralss/OneDrive - DECEA/Documentos/Relatório/Relatório/Imagens/ceu2.jpg",size=2.2)+
    geom_segment(data = BDS_KPI_09_DF%>% dplyr::filter(movimentos.ano == "2022"),
                 aes(x=movimentos.aero, y=3,xend=movimentos.aero,yend=movimentos.jan),linetype="dotted")+
    geom_image(data = BDS_KPI_10_DF%>% dplyr::filter(movimentos.ano == "2022"),
               aes(x = movimentos.aero, y = movimentos.jan, fill = as.factor(movimentos.ano),
            image="C:/Users/silveiralss/OneDrive - DECEA/Documentos/Relatório/Relatório/Imagens/aviao2.png"))+
    geom_text(data = BDS_KPI_10_DF%>% dplyr::filter(movimentos.ano == "2022"),
              aes( x = movimentos.aero,y = movimentos.jan, label = movimentos.jan, group = movimentos.ano, vjust=-0.8)
              , size = 4,vjust=4, color = "black")+
    geom_image(data = BDS_KPI_09_DF%>% dplyr::filter(movimentos.ano == "2022"),
               aes(x=movimentos.aero, y=movimentos.jan, image="C:/Users/silveiralss/OneDrive - DECEA/Documentos/Relatório/Relatório/Imagens/pista.png"), size=.04)+
    geom_text(data = BDS_KPI_09_DF%>% dplyr::filter(movimentos.ano == "2022"),
              aes(x=movimentos.aero, y=movimentos.jan, label = movimentos.jan, group = movimentos.ano, vjust=-0.8)
              , vjust=-2,size = 4, color = "black")+
    scale_y_continuous(breaks = NULL, limits = c(-5,55))+
    theme_minimal()+
    labs(fill=NULL)+
    labs(title = "Capacidade de ARR X taxa pico", caption = "Fonte: CGNA e TATIC FLOW", x = NULL, y = "Taxa pico e capacidade de pouso") +
    theme_minimal()+
    theme(legend.position = "none",
          plot.title = element_text(size = 18,
                                    face = "bold",
                                    hjust = 0.5),
          plot.subtitle = element_text(size = 12, color = "#8B0000"),
          plot.caption = element_text(hjust = .5,
                                      face = "bold"),
          panel.spacing = unit(.1, "cm", data = NULL),
          axis.text.x = element_text(size = 7),
          strip.text.x = element_text(size = 8))
  
  
  
  
  
  
  
###############################################################################################################  
  
  
  
  
  geom_point(data = BDS_KPI_10_DF%>% dplyr::filter(movimentos.ano == "2022"),aes(x = movimentos.aero, y = movimentos.jan, fill = as.factor(movimentos.ano))) #+
  geom_text(data = BDS_KPI_10_DF%>% dplyr::filter(movimentos.ano == "2022"),aes( x = movimentos.aero,y = movimentos.jan, label = movimentos.jan, group = movimentos.ano, vjust=-0.8)
            , size = 3, color = "black")
  
  
  
  
  
  
  +
  geom_point(data = BDS_KPI_09_DF%>% dplyr::filter(movimentos.ano == "2022"), aes(x=movimentos.aero, y=movimentos.jan), shape=15, color="red",size=2.5)+
    geom_text(data = BDS_KPI_09_DF%>% dplyr::filter(movimentos.ano == "2022"), aes(x=movimentos.aero, y=movimentos.jan, label = movimentos.jan, group = movimentos.ano, vjust=-0.8)
              , size = 3, color = "black")+
  facet_grid(cols = vars(movimentos.regiao),switch= "x", scales = "free_x",space = "free_x") +
  labs(fill=NULL)+
  labs(title = "Capacidade de ARR X taxa pico", caption = "Fonte: CGNA e TATIC FLOW", x = NULL, y = "Tacha pico e capacidade de pouso") +
  theme_minimal()+
  theme(legend.position = "none",
        plot.title = element_text(size = 18,
                                  face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 12, color = "#8B0000"),
        plot.caption = element_text(hjust = .5,
                                    face = "bold"),
        panel.spacing = unit(.5, "cm", data = NULL),
        axis.text.x = element_text(size = 7),
        strip.text.x = element_text(size = 8))












##########################################################################
TAXI_IN_OUT_AERO <- bind_cols(BDS_KPI_02_DF$movimentos.ano,
                                  BDS_KPI_02_DF$movimentos.aero,
                                  BDS_KPI_02_DF$movimentos.percentual_ano,
                                  BDS_KPI_13_DF$movimentos.percentual_ano)

TAXI_IN_OUT_AERO_COL_NAMES <- c('movimentos.ano','movimentos.aero','movimentos.percentual_ano_out','movimentos.percentual_ano_in')
colnames(TAXI_IN_OUT_AERO) <- TAXI_IN_OUT_AERO_COL_NAMES
TAXI_IN_OUT_AERO <- TAXI_IN_OUT_AERO %>% pivot_longer(cols = movimentos.percentual_ano_out:movimentos.percentual_ano_in,
                                                              names_to = "IN_OUT",
                                                              values_to = "ADICIONAL")

TAXI_IN_OUT_AERO %>%
  filter(movimentos.ano == year(Sys.Date())) %>% 
  ggplot()+
  geom_col(aes(x=movimentos.aero, y=ADICIONAL, fill = IN_OUT), position = "dodge")+
  geom_text(aes(x=movimentos.aero, y=ADICIONAL, label = round(ADICIONAL,2), group = IN_OUT),size = 3,
            vjust=-0.4,
            position = position_dodge(width = 0.9))+
  geom_hline(yintercept = 3, color = "red")+
  scale_y_continuous(limits = c(0,5))+
  theme_minimal() +
  labs(title = "Tempo adicional de TAXI_IN/OUT por aeroporto (2022)", caption = "Fonte: BINTRA", x = NULL, y = "average additional taxi in/out time (min)"
       ,fill = "ANO") +
  theme(legend.position = "bottom",
        legend.title=element_blank()
        ,legend.text     = element_text(size = 8)
        ,legend.key.size = unit(0.3, "cm"),
        plot.title = element_text(size = 18,
                                  face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 12, color = "#8B0000"),
        plot.caption = element_text(hjust = .5,
                                    face = "bold"))+
  scale_fill_manual(values = cores,labels=c("TAXI-IN", "TAXI-OUT"))






######################################
país	ano	classificação
Índia	2011	4
Índia	2012	2
Índia	2013	2
Suécia	2011	3
Suécia	2012	1
Suécia	2013	4




ggplot(df, aes(year, rank, color = country)) +
  geom_point(size = 7) +
  geom_text(data = df %>% filter(year == min(year)),
            aes(x = year - .1, label = country), size = 5, hjust = 1) +
  geom_text(data = df %>% filter(year == max(year)),
            aes(x = year + .1, label = country), size = 5, hjust = 0) +
  geom_bump(size = 2, smooth = 8) +
  scale_x_continuous(limits = c(2010.6, 2013.4),
                     breaks = seq(2011, 2013, 1)) +
  theme_minimal_grid(font_size = 14, line_size = 0) +
  theme(legend.position = "none",
        panel.grid.major = element_blank()) +
  labs(y = "RANK",
       x = NULL) +
  scale_y_reverse() +
  scale_color_manual(values = wes_palette(n = 4, name = "GrandBudapest1"))







#######################################
BDS_TAXIIN_2019 <- BDS_KPI_13_DF %>% filter(movimentos.ano %in% c(2019))
BDS_TAXIIN_2022 <- BDS_KPI_13_DF %>% filter(movimentos.ano %in% c(2022))
BDS_VAR_TAXIIN <- bind_cols(BDS_TAXIIN_2019,BDS_TAXIIN_2022) %>% 
  transmute(PONTUALIDADE_19=movimentos.percentual_ano...6, AERO=movimentos.aero...8,
            PONTUALIDADE_22= movimentos.percentual_ano...27, VAR=(PONTUALIDADE_22-PONTUALIDADE_19)/PONTUALIDADE_19) %>% 
  mutate(COR = as.factor(ifelse(VAR > 0, yes = 1, no = 0))) %>% 
  mutate(AERO=factor(AERO, levels = c("SBBR","SBCF","SBCG","SBCT","SBFI","SBFL","SBPA",
                                      "SBFZ","SBMO","SBPS","SBRF","SBSV","SBBE","SBCY","SBEG",
                                      "SBGL","SBGR","SBKP","SBRJ","SBSP")))

VELOCIMETROTAXIIN <- data.frame(PONTUALIDADE=c("PONT", "DIF"), 
                                 MEDIA=c(mean(BDS_VAR_TAXIOUT$PONTUALIDADE_22),
                                         100-mean(BDS_VAR_TAXIOUT$PONTUALIDADE_22)))







GFTAXIIN <- 
  BDS_KPI_13_DF %>% filter(movimentos.ano == "2022") %>%
  ggplot()+
  geom_col(aes(x=movimentos.aero,y =movimentos.percentual_ano,fill = as.factor(movimentos.ano)),width = .5)+
  facet_grid(cols = vars(movimentos.regiao),switch= "x", scales = "free_x", space = "free_x")+
  scale_y_continuous(limits = c(-.13,BDS_KPI_13_DF$movimentos.percentual_ano+1))+
  geom_text(aes(x=movimentos.aero,y =-.13, label=movimentos.aero),size=3)+
  geom_richtext(aes( x=movimentos.aero, y = movimentos.percentual_ano/2, label = movimentos.percentual_ano,
                     hjust = .5),size = 3, color = "black",angle=90)+
  scale_fill_manual(values = cores)+
  labs(fill=NULL, x = NULL, y = "Tempo adicional de taxi-in (min)")+
  theme_minimal()+
  theme(legend.position = "none",
        axis.text.x = element_blank())

#gráfico de regional
GFTAXIINREG <- 
  BDS_KPI_13_REGIONAL_DF %>% filter(movimentos.ano%in%c(2019,2022)) %>% 
  ggplot()+
  geom_col(aes(x=movimentos.aero, y=movimentos.percentual_ano, fill = as.factor(movimentos.ano)), position = "dodge")+
  geom_text(aes(x=movimentos.aero, y=movimentos.percentual_ano, label = round(movimentos.percentual_ano,2), group = movimentos.ano),size = 3,
            hjust = -0.1,
            position = position_dodge(width = 0.9),angle = 90)+
  scale_y_continuous(limits = c(0,BDS_KPI_13_DF$movimentos.percentual_ano+1))+
  theme_minimal() +
  labs(caption = "Fonte: BINTRA", x = NULL, y = "Tempo adicional de taxi-in (min)"
       ,fill = "ANO") +
  theme(legend.position = "bottom",
        legend.title=element_blank()
        ,legend.text     = element_text(size = 8)
        ,legend.key.size = unit(0.3, "cm"),
        plot.subtitle = element_text(size = 12, color = "#8B0000"),
        plot.caption = element_text(hjust = .5,
                                    face = "bold"))+
  scale_fill_manual(values = cores)



GFVARTAXIIN <- 
  ggplot(BDS_VAR_TAXIIN)+
  geom_col(aes(x = AERO, y = VAR, fill = COR), 
           na.rm = TRUE, color = "black",width = 0.35) +
  theme_minimal()+
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits=c(-1,1),labels = scales::label_percent(scale = 100))+
  coord_cartesian(xlim = c(1.05,19.8))+
  guides(fill = "none") +
  geom_text(aes(x = AERO, y = VAR, label=scales::percent(VAR,accuracy = 0.1),vjust = ifelse(VAR < 0, 1.25, -0.25)),size= 3)+
  labs(x = NULL, y = "Variação 2022 / 2019") +
  scale_fill_manual(values = c("firebrick", "dodgerblue4"))+
  theme(axis.text.x = element_blank())


GFVARTAXIIN + GFTAXIIN+GFTAXIINREG +plot_layout(ncol=1,nrow = 3,heights = c(1.4,3,3),widths = c(4,1))









#####################################################
BDS_TAXIOUT_2019 <- BDS_KPI_02_DF %>% filter(movimentos.ano %in% c(2019))
BDS_TAXIOUT_2022 <- BDS_KPI_02_DF %>% filter(movimentos.ano %in% c(2022))
BDS_VAR_TAXIOUT <- bind_cols(BDS_TAXIOUT_2019,BDS_TAXIOUT_2022) %>% 
  transmute(PONTUALIDADE_19=movimentos.percentual_ano...6, AERO=movimentos.aero...8,
            PONTUALIDADE_22= movimentos.percentual_ano...27, VAR=(PONTUALIDADE_22-PONTUALIDADE_19)/PONTUALIDADE_19) %>% 
  mutate(COR = as.factor(ifelse(VAR > 0, yes = 1, no = 0))) %>% 
  mutate(AERO=factor(AERO, levels = c("SBBR","SBCF","SBCG","SBCT","SBFI","SBFL","SBPA",
                                      "SBFZ","SBMO","SBPS","SBRF","SBSV","SBBE","SBCY","SBEG",
                                      "SBGL","SBGR","SBKP","SBRJ","SBSP")))

VELOCIMETROTAXIOUT <- data.frame(PONTUALIDADE=c("PONT", "DIF"), 
                                 MEDIA=c(mean(BDS_VAR_TAXIOUT$PONTUALIDADE_22),
                                         100-mean(BDS_VAR_TAXIOUT$PONTUALIDADE_22)))







GFTAXIOUT <- 
BDS_KPI_02_DF %>% filter(movimentos.ano == "2022") %>%
  ggplot()+
  geom_col(aes(x=movimentos.aero,y =movimentos.percentual_ano,fill = as.factor(movimentos.ano)),width = .5)+
  facet_grid(cols = vars(movimentos.regiao),switch= "x", scales = "free_x", space = "free_x")+
  scale_y_continuous(limits = c(-.13,BDS_KPI_02_DF$movimentos.percentual_ano+1))+
  geom_text(aes(x=movimentos.aero,y =-.13, label=movimentos.aero),size=3)+
  geom_richtext(aes( x=movimentos.aero, y = movimentos.percentual_ano/2, label = movimentos.percentual_ano,
                     hjust = .5),size = 3, color = "black",angle=90)+
  scale_fill_manual(values = cores)+
  labs(fill=NULL, x = NULL, y = "Tempo adicional de taxi-out(min)")+
  theme_minimal()+
  theme(legend.position = "none",
        axis.text.x = element_blank())

#gráfico de regional
GFTAXIOUTREG <- 
BDS_KPI_02_REGIONAL_DF %>% filter(movimentos.ano%in%c(2019,2022)) %>% 
ggplot()+
  geom_col(aes(x=movimentos.aero, y=movimentos.percentual_ano, fill = as.factor(movimentos.ano)), position = "dodge")+
  geom_text(aes(x=movimentos.aero, y=movimentos.percentual_ano, label = round(movimentos.percentual_ano,2), group = movimentos.ano),size = 3,
            hjust = -0.1,
            position = position_dodge(width = 0.9),angle = 90)+
  scale_y_continuous(limits = c(0,6))+
  theme_minimal() +
  labs(caption = "Fonte: BINTRA", x = NULL, y = "Tempo adicional de taxi-out (min)"
       ,fill = "ANO") +
  theme(legend.position = "bottom",
        legend.title=element_blank()
        ,legend.text     = element_text(size = 8)
        ,legend.key.size = unit(0.3, "cm"),
        plot.subtitle = element_text(size = 12, color = "#8B0000"),
        plot.caption = element_text(hjust = .5,
                                    face = "bold"))+
  scale_fill_manual(values = cores)



GFVARTAXIOUT <- 
ggplot(BDS_VAR_TAXIOUT)+
  geom_col(aes(x = AERO, y = VAR, fill = COR), 
           na.rm = TRUE, color = "black",width = 0.35) +
  theme_minimal()+
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits=c(-1,1),labels = scales::label_percent(scale = 100))+
  coord_cartesian(xlim = c(1.05,19.8))+
  guides(fill = "none") +
  geom_text(aes(x = AERO, y = VAR, label=scales::percent(VAR,accuracy = 0.1),vjust = ifelse(VAR < 0, 1.25, -0.25)),size= 3)+
  labs(x = NULL, y = "Variação 2022 / 2019") +
  scale_fill_manual(values = c("firebrick", "dodgerblue4"))+
  theme(axis.text.x = element_blank())


GFVARTAXIOUT + GFTAXIOUT+GFTAXIOUTREG +plot_layout(ncol=1,nrow = 3,heights = c(1.4,3,3),widths = c(4,1))

#########################

BDS_KPI_15_DF %>% filter(movimentos.ano%in% c(2022)&movimentos.total_movimentos>2300) %>% 
  ggplot()+
  geom_point(aes(x=reorder(movimentos.fluxo, -movimentos.percentual_ano_70), y=movimentos.total_movimentos))+
  geom_col(aes(x=reorder(movimentos.fluxo, -movimentos.percentual_ano_70), y=movimentos.percentual_ano_70, fill = as.factor(movimentos.ano)), position = position_dodge(preserve = "single"))+
  geom_point(aes(x=movimentos.fluxo, y=movimentos.total_movimentos))+
  scale_y_continuous(limits = c(0,10))+
  theme_minimal()+
  labs(fill=NULL)+
  geom_text(aes(x=movimentos.fluxo, y=movimentos.percentual_ano_70, label = round(movimentos.percentual_ano_70,1), group = movimentos.ano),size = 3,vjust=-.8,
            position = position_dodge(width = 0.9))+
  geom_text(data=VAR15,aes(x=ROTA, y=9, label = scales::percent(VAR,accuracy = 0.1)), size = 3,color="red",
            hjust = 0.5,
            position = position_dodge(width = 0.9),fontface="bold")+
  theme(axis.text.x=element_text(angle = 90, hjust = 0))+
  labs(title = "Variabilidade do tempo de voo 70%", caption ="FONTE: VRA", x=NULL, y="Variação do tempo de voo (min)")+
  theme(legend.position = "none",
        plot.title = element_text(size = 18,
                                  face = "bold",
                                  hjust = 0.5),
        plot.caption = element_text(size = 10, face = "bold",hjust = 0.5))+
  scale_fill_manual(values = c("#B0C4DE", "#708090", "#4682B4"))




#######################

BDS_KPI_02_DF %>% filter(movimentos.ano == year(Sys.Date())) %>% 
  ggplot()+
  geom_col(aes(x=movimentos.aero, y=movimentos.percentual_ano, fill = as.factor(movimentos.ano)),fill="#191970", position = "dodge")+
  geom_text(aes(x=movimentos.aero, y=movimentos.percentual_ano, label = round(movimentos.percentual_ano,2), group = movimentos.ano),size = 2.5,
            hjust = -0.1,
            position = position_dodge(width = 0.9),angle = 90)+
  geom_hline(yintercept = 3, color = "red")+
  scale_y_continuous(limits = c(0,5))+
  theme_minimal()+
  labs(title = "Tempo adicional de TAXI_OUT por aeroporto (2022)", caption = "Fonte: BINTRA", x = NULL, y = "Tempo adicional de taxi-out (min)"
       ,fill = "ANO") +
  theme(plot.title = element_text(size = 18,
                                  face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 12, color = "#8B0000"),
        plot.caption = element_text(hjust = .5,
                                    face = "bold"),
        axis.text.x=element_text(size = 8))+ #se precisar rotacionar o texto X usar (axis.text = element_text(angle = 90)) dentro do theme
  scale_fill_manual(values = cores)




################################
#PONTUALIDADE DE chegada 

BDS_PONT_2019_CHEGADA <- BDS_KPI_14_DF %>% filter(ANO %in% c(2019))
BDS_PONT_2022_CHEGADA <- BDS_KPI_14_DF %>% filter(ANO %in% c(2022))
BDS_VAR_PONT_CHEGADA <- bind_cols(BDS_PONT_2019_CHEGADA,BDS_PONT_2022_CHEGADA) %>% 
  transmute(PONTUALIDADE_19=PONTUALIDADE_ANO...2, AERO=AERO...4,
            PONTUALIDADE_22= PONTUALIDADE_ANO...18, VAR=(PONTUALIDADE_22-PONTUALIDADE_19)/PONTUALIDADE_19) %>% 
  mutate(COR = as.factor(ifelse(VAR > 0, yes = 1, no = 0))) %>% 
  mutate(AERO=factor(AERO, levels = c("SBBR","SBCF","SBCG","SBCT","SBFI","SBFL","SBPA",
                                      "SBFZ","SBMO","SBPS","SBRF","SBSV","SBBE","SBCY","SBEG",
                                      "SBGL","SBGR","SBKP","SBRJ","SBSP")))

VELOCIMETRO_CHEGADA <- data.frame(PONTUALIDADE=c("PONT", "DIF"), 
                          MEDIA=c(mean(BDS_VAR_PONT_CHEGADA$PONTUALIDADE_22),
                                  100-mean(BDS_VAR_PONT_CHEGADA$PONTUALIDADE_22)))


#GFFF_CHEGADA <-
  BDS_KPI_14_DF %>% 
  filter(ANO == "2022") %>% 
  ggplot()+
  geom_col(aes(x=AERO,y =PONTUALIDADE_ANO,fill = as.factor(ANO)),width = .5)+
  facet_grid(cols = vars(REGIONAL),switch= "x", scales = "free_x", space = "free_x")+
  scale_y_continuous(limits = c(-10,BDS_KPI_01_DF$PONTUALIDADE_ANO))+
  geom_text(aes(x=AERO,y =-7, label=AERO),size=3)+
  geom_richtext(aes( x=AERO, y = PONTUALIDADE_ANO/2, label = scales::percent(PONTUALIDADE_ANO, scale = 1, accuracy = 1),
                     hjust = .5),size = 3, color = "black",angle=90)+
  scale_fill_manual(values = cores)+
  labs(fill=NULL, x = NULL, y = "Pontualidade na chegada")+
  theme_minimal()+
  theme(legend.position = "none",
        axis.text.x = element_blank())


GFPONT_VAR_CHEGADA <- 
  ggplot(BDS_VAR_PONT_CHEGADA)+
  geom_col(aes(x = AERO, y = VAR, fill = COR), 
           na.rm = TRUE, color = "black",width = 0.35) +
  theme_minimal()+
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits=c(-.5,.5),labels = scales::label_percent(scale = 100))+
  coord_cartesian(xlim = c(1.05,19.8))+
  guides(fill = "none") +
  geom_text(aes(x = AERO, y = VAR, label=scales::percent(VAR,accuracy = 0.1),vjust = ifelse(VAR < 0, 1.25, -0.25)),size= 3)+
  labs(x = NULL, y = "Variação 2022 / 2019") +
  scale_fill_manual(values = c("firebrick", "dodgerblue4"))+
  theme(axis.text.x = element_blank())

GF_MEDIA_CHEGADA <- 
  ggplot(VELOCIMETRO_CHEGADA,aes(x="",y=MEDIA,fill=PONTUALIDADE))+
  geom_bar(stat = "identity")+
  geom_text(aes(x="", y=MEDIA, label = scales::percent(MEDIA, accuracy = 1,scale = 1)),position = position_stack(vjust = 0.5),size = 10)+
  labs(title = "Pontualidade média")+
  coord_polar(theta = "y", start = 0)+
  theme_void()+
  scale_fill_manual(values = c("white","#228B22"))+
  
  theme(legend.position = "none",
        legend.title=element_blank(),
        plot.title = element_text(size = 10,
                                  face = "bold",
                                  hjust = 0.5))

GFPONT_REG_CHEGADA <- 
  BDS_KPI_14_REGIONAL_DF %>% filter(movimentos.ano%in%c(2020,2021) &movimentos.aero!="SISCEAB") %>% 
  ggplot()+
  geom_col(aes(x=movimentos.aero, y=movimentos.percentual_ano, fill = as.factor(movimentos.ano)), position = "dodge",width = .5)+
  geom_text(aes(x=movimentos.aero, y=movimentos.percentual_ano, label = scales::percent(movimentos.percentual_ano, accuracy = 1,scale = 1), 
                group = movimentos.ano),size = 3,vjust=-1,
            position = position_dodge(width = 0.5))+
  theme_minimal() +
  labs( caption = "Fonte: SIROS e TATIC FLOW", x = NULL, y = NULL
        ,fill = "ANO") +
  theme(legend.position = "bottom",
        legend.title=element_blank()
        ,legend.text     = element_text(size = 8)
        ,legend.key.size = unit(0.3, "cm"),
        plot.subtitle = element_text(size = 12, color = "#8B0000"),
        plot.caption = element_text(hjust = .5,
                                    face = "bold"))+
  scale_y_continuous(labels = NULL, limits = c(0,100))+
  scale_fill_manual(values = cores)

SPACE <- plot_spacer()

GFPONT_VAR_CHEGADA +SPACE+ GFFF_CHEGADA+GF_MEDIA_CHEGADA+GFPONT_REG_CHEGADA +SPACE+plot_layout(ncol=2,heights = c(1.4,3,3),widths = c(4,1)) 





  



####################################################




fig <- plotly::plot_ly(
  domain = list(x = c(0, 1), y = c(0, 1)),
  value = 93,
  number = list(suffix = "%"),
  title = list(text = "Pontualidade"),
  type = "indicator",
  mode = "gauge+number+delta",
  delta = list(reference = 90),
  gauge = list(
    bar = list(color = "darkblue"),
    axis =list(range = list(NULL, 100)),
    steps = list(
      list(range = c(0, 75), color = "red"),
      list(range = c(75, 85), color = "yellow"),
      list(range = c(85,100 ), color = "green")),
    threshold = list(
      line = list(color = "gray", width = 4),
      thickness = 0.75,
      value = "93%")))


fig









##################


GFFF <-
  BDS_KPI_01_DF %>% 
  filter(ANO == "2022") %>% 
  ggplot()+
  geom_col(aes(x=AERO,y =PONTUALIDADE_ANO,fill = as.factor(ANO)),width = .5)+
  facet_grid(cols = vars(REGIONAL),switch= "x", scales = "free_x", space = "free_x")+
  scale_y_continuous(limits = c(-10,BDS_KPI_01_DF$PONTUALIDADE_ANO))+
  geom_text(aes(x=AERO,y =-7, label=AERO),size=3)+
  geom_richtext(aes( x=AERO, y = PONTUALIDADE_ANO/2, label = scales::percent(PONTUALIDADE_ANO, scale = 1, accuracy = 1),
                     hjust = .5),size = 3, color = "black",angle=90)+
  scale_fill_manual(values = cores)+
  labs(fill=NULL, x = NULL, y = "Pontualidade na partida")+
  theme_minimal()+
  theme(legend.position = "none",
        axis.text.x = element_blank())


GFPONT_VAR <- 
  ggplot(BDS_VAR_PONT)+
  geom_col(aes(x = AERO, y = VAR, fill = COR), 
           na.rm = TRUE, color = "black",width = 0.35) +
  theme_minimal()+
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits=c(-.5,.5),labels = scales::label_percent(scale = 100))+
  coord_cartesian(xlim = c(1.05,19.8))+
  guides(fill = "none") +
  geom_text(aes(x = AERO, y = VAR, label=scales::percent(VAR,accuracy = 0.1),vjust = ifelse(VAR < 0, 1.25, -0.25)),size= 3)+
  labs(x = NULL, y = "Variação 2022 / 2019") +
  scale_fill_manual(values = c("firebrick", "dodgerblue4"))+
  theme(axis.text.x = element_blank())

#GF_MEDIA <- 
  ggplot(BDS_VAR_PONT)+
  geom_image(aes(x=5,5),image="C:/Users/silveiralss/OneDrive - DECEA/Documentos/Relatório/Relatório/Imagens/pontualidade.png",size=.5)+
  geom_text(aes(x = 5, y = 5, label=percent(mean(PONTUALIDADE_22),scale = 1)),size=8, vjust=2.3)+
  geom_text(aes(x = 5, y = 5, label="Média anual"),size=8, vjust=-2)+
  theme_void()





GFPONT_REG <- 
  BDS_KPI_01_REGIONAL_DF %>% filter(movimentos.ano%in%c(2020,2021) &movimentos.aero!="SISCEAB") %>% 
ggplot()+
  geom_col(aes(x=movimentos.aero, y=movimentos.percentual_ano, fill = as.factor(movimentos.ano)), position = "dodge",width = .5)+
  geom_text(aes(x=movimentos.aero, y=movimentos.percentual_ano, label = scales::percent(movimentos.percentual_ano, accuracy = 1,scale = 1), 
                group = movimentos.ano),size = 3,vjust=-1,
            position = position_dodge(width = 0.5))+
  theme_minimal() +
  labs( caption = "Fonte: SIROS e TATIC FLOW", x = NULL, y = NULL
       ,fill = "ANO") +
  theme(legend.position = "bottom",
        legend.title=element_blank()
        ,legend.text     = element_text(size = 8)
        ,legend.key.size = unit(0.3, "cm"),
        plot.subtitle = element_text(size = 12, color = "#8B0000"),
        plot.caption = element_text(hjust = .5,
                                    face = "bold"))+
  scale_y_continuous(labels = NULL, limits = c(0,100))+
  scale_fill_manual(values = cores)

SPACE <- plot_spacer()

GFPONT_VAR +SPACE+ GFFF+GF_MEDIA+GFPONT_REG +SPACE+plot_layout(ncol=2,heights = c(1.4,3,3),widths = c(4,1))





VELOCIMETRO <- data.frame(PONTUALIDADE=c("PONT", "DIF"), MEDIA=c(mean(BDS_VAR_PONT$PONTUALIDADE_22),100-mean(BDS_VAR_PONT$PONTUALIDADE_22)))






#GF_MEDIA <- 
  ggplot(VELOCIMETRO,aes(x="",y=MEDIA,fill=PONTUALIDADE))+
  geom_bar(stat = "identity")+
  geom_text(aes(x="", y=MEDIA, label = scales::percent(MEDIA, accuracy = 1,scale = 1)),position = position_stack(vjust = 0.5),size = 10)+
  labs(title = "Pontualidade média")+
  coord_polar(theta = "y", start = 0)+
  theme_void()+
scale_fill_manual(values = c("white","#228B22"))+
  
  theme(legend.position = "none",
        legend.title=element_blank(),
        plot.title = element_text(size = 10,
                                  face = "bold",
                                  hjust = 0.5))
       

  gauge(VELOCIMETRO[1,2], min=0,max=100,symbol = '%',gaugeSectors(success = c(60,100),warning = c(40,59),danger = c(0,39))) 
  
  
  
  
  

############################
#GRÁFICO DE MOVIMENTOS SISCEAB E FIR (4)

GFVAR_FIR <- 
  TOTAL_MOV_SISC_FIR%>% filter(ANO %in%c(2019,2022)& FIR!="SISCEAB") %>%
  ggplot()+
  geom_col(aes(x=FIR, y=TOTAL, fill = ANO), position = "dodge", width = .3)+
  scale_y_continuous(breaks = NULL,limits = c(0,550000))+
  
  geom_label(aes(x=FIR, y=ifelse(TOTAL < 100000, TOTAL, TOTAL/2),group=ANO, label = format(TOTAL, big.mark=".")), size = 3,
             hjust = -0.1,
             position = position_dodge(width = 0.3),angle = 90)+
  theme_minimal() +
  theme(legend.title    = element_text(size = 8) 
        ,legend.text     = element_text(size = 8)
        ,legend.key.size = unit(0.3, "cm")
  ) +
  labs(title = "Movimentos anual ", caption = "Fonte: SETA", x = NULL, y = NULL
       ,fill = "ANO") +
  theme(legend.position = "none",
        legend.title=element_blank(),
        plot.title = element_text(size = 10,
                                  face = "bold",
                                  hjust = 0.5))+
  scale_fill_manual(values = cores)+
  coord_flip()



BDS_ANO_2019_FIR <- TOTAL_MOV_SISC_FIR %>% filter(ANO %in% c(2019))
BDS_ANO_2022_FIR <- TOTAL_MOV_SISC_FIR %>% filter(ANO %in% c(2022))
BDS_VAR_ANO_FIR <- bind_cols(BDS_ANO_2019_FIR,BDS_ANO_2022_FIR) %>% 
  transmute(FIR=FIR...1, TOTAL19=TOTAL...3,TOTAL22= TOTAL...6, VAR=(TOTAL22-TOTAL19)/TOTAL19) %>% 
  mutate(COR = as.factor(ifelse(VAR > 0, yes = 1, no = 0)))


GFVAR_FIR_ANO <- 
BDS_VAR_ANO_FIR %>% filter(FIR!="SISCEAB") %>% 
ggplot()+
  geom_col(aes(x = FIR, y = VAR, fill = COR), 
           na.rm = TRUE, color = "black",width = 0.20) +
  theme_void()+
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits=c(-1.8,1.5),labels = scales::label_percent(scale = 100))+
  scale_x_discrete(breaks = NULL)+
  guides(fill = FALSE) +
  geom_text(aes(x = FIR, y = VAR, label=scales::percent(VAR,accuracy = 0.1),hjust = ifelse(VAR < 0, 1.25, -0.25)),size= 3.5)+
  labs(title= "Variação 2022 / 2019",x = NULL, y = NULL) +
  scale_fill_manual(values = c("firebrick", "dodgerblue4"))+
  theme(plot.title = element_text(size = 10,
                                  face = "bold",
                                  hjust = 0.5))+
  coord_flip()


###################

VAR_MES_FIR <- bind_rows(BDS_KPI_TOTAL_Brasil_DF, 
          BDS_KPI_TOTAL_BS_DF, 
          BDS_KPI_TOTAL_CW_DF, 
          BDS_KPI_TOTAL_RE_DF, 
          BDS_KPI_TOTAL_AZ_DF, 
          BDS_KPI_TOTAL_AO_DF) %>% 
  transmute(FIR=movimentos.fir,ANO=movimentos.ano,TOTAL=movimentos.total,
            JAN=movimentos.jan,
            FEV=movimentos.fev,
            MAR=movimentos.mar,
            ABR=movimentos.abr,
            MAI=movimentos.mai,
            JUN=movimentos.jun,
            JUL=movimentos.jul,
            AGO=movimentos.ago,
            SET=movimentos.set,
            OUT=movimentos.out,
            NOV=movimentos.nov,
            DEZ=movimentos.dez) %>% 
  pivot_longer(cols = JAN:DEZ,
               names_to = "MES",
               values_to = "MOV") %>% 
  mutate(MES=factor(MES,levels = c("JAN","FEV","MAR","ABR","MAI","JUN","JUL","AGO","SET","OUT","NOV","DEZ"))) %>% 
  mutate(FIR = case_when(FIR == "BRASIL" ~ "SISCEAB",
                         FIR == "SBBS" ~ "FIR Brasília",
                         FIR == "SBCW" ~ "FIR Curitiba",
                         FIR == "SBRE" ~ "FIR Recife",
                         FIR == "SBAZ" ~ "FIR Amazônica",
                         FIR == "SBAO" ~ "FIR Atlântico"), ANO=as.character(ANO)) %>%
  mutate(FIR= factor(FIR, levels = c("FIR Brasília","FIR Curitiba","FIR Recife","FIR Amazônica","FIR Atlântico","SISCEAB")))

#######3







GFVAR_FIR_MES <- 
VAR_MES_FIR %>% filter(ANO=="2022"&FIR!="SISCEAB") %>% 
  ggplot()+
  geom_line(aes(x=MES, y=MOV,group=FIR, color=FIR),size=1.1)+
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ","),breaks = seq(0, 60000, by = 5000), limits = c(0,60000))+
  scale_color_manual(values = CORESJURISDICAO)+
  theme_classic() +
  labs(title= "Movimentos mensal por FIR", x = NULL, y = "Movimentos"
       ,fill = NULL) +
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        legend.key.size = unit(7,"mm"),
        plot.title = element_text(size = 10,
                                  face = "bold",
                                  hjust = 0.5))+
  scale_fill_manual(values = CORESJURISDICAO)






grid.arrange(GFVAR_FIR_MES,GFVAR_FIR,GFVAR_FIR_ANO, ncol=3,widths=c(3,1,1))














###########################################
#GRÁFICO COMPARATIVO POR MÊS DE TOTAL DE OPERAÇÕES ARR E DEP NO SISCEAB (03)

BDS_ANO_2019_SIS <- TOTAL_MES_SISCEAB %>% filter(ANO %in% c(2019), MES=="JAN")
BDS_ANO_2022_SIS <- TOTAL_MES_SISCEAB %>% filter(ANO %in% c(2022), MES=="JAN")
BDS_VAR_ANO_SIS <- bind_rows(BDS_ANO_2019_SIS,BDS_ANO_2022_SIS) %>% 
  transmute(ANO=ANO, TOTAL=TOTAL,VAR=(BDS_ANO_2022_SIS$TOTAL-BDS_ANO_2019_SIS$TOTAL)/BDS_ANO_2019_SIS$TOTAL)

GFVARANO_SIS <-  
ggplot(BDS_VAR_ANO_SIS)+                                                                                                 
  geom_col(aes(x=ANO, y=TOTAL, fill = ANO),width = .5, position = position_dodge(preserve = "single"))+                
  scale_y_continuous(limits = c(0,1700000),labels = comma_format(big.mark = "."))+ 
  scale_x_discrete(expand = expansion(add = .7))+
  geom_richtext(aes(x=ANO, y=300000, label= TOTAL, group=ANO),size=3, position = position_dodge(width = .9), angle=90)+
  theme_classic() + 
  theme(panel.background=element_rect(fill = "#F0F8FF")
  ) +
  labs(caption = "  ", x = NULL, y = NULL   
  ) +
  theme(legend.position = "none",
        plot.caption = element_text(size = 43))+
  scale_fill_manual(values = cores)


#GFVARMES_SIS <- 
TOTAL_MES_SISCEAB %>% filter(ANO %in% c(2019, 2022)) %>%
  ggplot()+
  geom_col(aes(x=MES, y=MOV, fill = ANO), position = position_dodge(preserve = "single"))+
  scale_y_continuous(limits = c(0,150000),labels = comma_format(big.mark = ".")) +
  coord_cartesian(xlim = c(1,12.5))+
  geom_richtext(aes(x=MES, y=15000, label = ifelse(MOV>999,format(MOV,big.mark="."),MOV) , group=ANO),size=3, position = position_dodge(width = .9), angle=90)+
  theme_classic()+
  theme(legend.title    = element_text(size = 8) 
        ,legend.text     = element_text(size = 8)
        ,legend.key.size = unit(0.3, "cm")
  ) +
  labs(caption = "Fonte: TATIC FLOW", x = NULL, y = "Movimentos"
       ,fill = "ANO") +
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        plot.caption = element_text(hjust = .65,
                                    face = "bold"))+
  scale_fill_manual(values = cores)




 
TOTAL_MES_2019_SIS <- TOTAL_MES_SISCEAB %>% filter(ANO=="2019")
TOTAL_MES_2022_SIS <- TOTAL_MES_SISCEAB %>% filter(ANO=="2022")
TOTALMES_SIS <- bind_cols(TOTAL_MES_2019_SIS,TOTAL_MES_2022_SIS$MOV) %>% #FOI CRIADO O MOV_COM_DOM PARA CRIAR O LABEL DE VARIAÇÃO 
  transmute(ANO=ANO,TOTAL=TOTAL,MES=MES,MOV19=MOV,MOV22=...5) %>% mutate(VAR= (MOV22-MOV19)/MOV19)




MOV_VAR_SIS <-    TOTALMES_SIS %>% 
  transmute(ANO = MES,VAR=VAR) %>% 
  mutate(ANO= factor(ANO, levels = c("JAN","FEV","MAR","ABR","MAI","JUN","JUL","AGO","SET","OUT","NOV","DEZ")),
         COR = as.factor(ifelse(VAR > 0, yes = 1, no = 0)))



#GFVARDOM_MES_SIS <- 
ggplot(MOV_VAR_SIS)+
  geom_col(aes(x = ANO, y = VAR, fill = COR), 
           na.rm = TRUE, color = "black",width = 0.30) +
  theme_minimal()+
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits=c(-1.8,1.5),labels = scales::label_percent(scale = 100))+
  scale_x_discrete(breaks = NULL)+
  coord_cartesian(xlim = c(1,12.5))+
  guides(fill = FALSE) +
  geom_text(aes(x = ANO, y = VAR, label=scales::percent(VAR,accuracy = 0.1),vjust = ifelse(VAR < 0, 1.25, -0.25)),size= 3.5)+
  labs(title= "Mensal",x = NULL, y = "Variação 2022 / 2019") +
  scale_fill_manual(values = c("firebrick", "dodgerblue4"))+
  theme(panel.spacing = unit(1, "cm", data = NULL),
        plot.title = element_text(size = 12,
                                  face = "bold",
                                  hjust = 0.5))


GFVARDOM_ANO <- 
  ggplot(BDS_VAR_ANO_SIS %>% filter(ANO=="2022") %>% 
           mutate(COR = as.factor(ifelse(VAR > 0, yes = 1, no = 0))))+
  geom_col(aes(x = ANO, y = VAR, fill = COR), 
           na.rm = TRUE, color = "black",width = 0.30) +
  theme_minimal()+
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks=NULL,limits=c(-1.5,1.5),labels = scales::label_percent(scale = 100))+
  scale_x_discrete(breaks = NULL,expand = expansion(add = 1.5))+
  guides(fill = FALSE) +
  geom_text(aes(x = ANO, y = VAR, label=scales::percent(VAR,accuracy = 0.1),vjust = ifelse(VAR < 0, 1.25, -0.25)),size=3.5, vjust=1.5)+
  labs(title= "Anual",x = NULL, y = NULL) +
  scale_fill_manual(values = c("firebrick", "dodgerblue4"))+
  theme(panel.spacing = unit(1, "cm", data = NULL),
        plot.title = element_text(size = 12,
                                  face = "bold",
                                  hjust = 0.5))


  grid.arrange(GFVARDOM_MES_SIS,GFVARDOM_ANO,GFVARMES_SIS,GFVARANO_SIS, ncol=2,widths=c(4,1),heights= c(1.4,4))



###################################################


BDS_VAR_2019_DOM <- MOV_COM_DOMESTC %>% filter(ANO %in% c(2019), MES=="JAN")
BDS_VAR_2022_DOM <- MOV_COM_DOMESTC %>% filter(ANO %in% c(2022), MES=="JAN")
BDS_VAR_ANO_DOM <- bind_rows(BDS_VAR_2019_DOM,BDS_VAR_2022_DOM) %>% 
  transmute(ANO=ANO, TOTAL=TOTAL,VAR=(BDS_VAR_2022_DOM$TOTAL-BDS_VAR_2019_DOM$TOTAL)/BDS_VAR_2019_DOM$TOTAL)

#GFVARANO <-  
  ggplot(BDS_VAR_ANO_DOM)+                                                                                                 
  geom_col(aes(x=ANO, y=TOTAL, fill = ANO),width = .5, position = position_dodge(preserve = "single"))+                
  scale_y_continuous(limits = c(0,1500000),labels = comma_format(big.mark = "."))+ 
  scale_x_discrete(expand = expansion(add = .7))+
  geom_richtext(aes(x=ANO, y=300000, label= TOTAL, group=ANO),size=3, position = position_dodge(width = .9), angle=90)+
  theme_classic() + 
  theme(panel.background=element_rect(fill = "#F0F8FF")
  ) +
  labs(caption = "  ", x = NULL, y = NULL   
  ) +
  theme(legend.position = "none",
        plot.caption = element_text(size = 43))+
  scale_fill_manual(values = cores)                 




#GFVARMES <- 
  MOV_COM_DOMESTC %>% filter(ANO %in% c(2019, 2022)) %>%
  ggplot()+
  geom_col(aes(x=MES, y=MOV, fill = ANO), position = position_dodge(preserve = "single"))+
  scale_y_continuous(limits = c(0,150000),labels = comma_format(big.mark = ".")) +
  coord_cartesian(xlim = c(1,12.5))+
  geom_richtext(aes(x=MES, y=15000, label = ifelse(MOV>999,format(MOV,big.mark="."),MOV) , group=ANO),size=3, position = position_dodge(width = .9), angle=90)+
    theme_classic()+
  theme(legend.title    = element_text(size = 8) 
        ,legend.text     = element_text(size = 8)
        ,legend.key.size = unit(0.3, "cm")
  ) +
  labs(caption = "Fonte: TATIC FLOW", x = NULL, y = "Movimentos"
       ,fill = "ANO") +
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        plot.caption = element_text(hjust = .65,
                                    face = "bold"))+
  scale_fill_manual(values = cores)


#######################
MOV_COM_DOM_VAR <-    MOV_COM_DOM %>% 
  transmute(ANO = MES,VAR=VAR) %>% 
  mutate(ANO= factor(ANO, levels = c("JAN","FEV","MAR","ABR","MAI","JUN","JUL","AGO","SET","OUT","NOV","DEZ")),
         COR = as.factor(ifelse(VAR > 0, yes = 1, no = 0)))


  
#GFVARDOM_MES <- 
    ggplot(MOV_COM_DOM_VAR)+
  geom_col(aes(x = ANO, y = VAR, fill = COR), 
           na.rm = TRUE, color = "black",width = 0.30) +
  theme_minimal()+
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits=c(-1.8,1.5),labels = scales::label_percent(scale = 100))+
    scale_x_discrete(breaks = NULL)+
  coord_cartesian(xlim = c(1,12.5))+
  guides(fill = FALSE) +
    geom_text(aes(x = ANO, y = VAR, label=scales::percent(VAR,accuracy = 0.1)),size= 3.5,vjust=1.5)+
  labs(title= "Mensal",x = NULL, y = "Variação 2022 / 2019") +
  scale_fill_manual(values = c("firebrick", "dodgerblue4"))+
    theme(panel.spacing = unit(1, "cm", data = NULL),
          plot.title = element_text(size = 12,
                                    face = "bold",
                                    hjust = 0.5))


GFVARDOM_ANO <- 
  ggplot(BDS_VAR_ANO_DOM %>% filter(ANO=="2022") %>% 
                                      mutate(COR = as.factor(ifelse(VAR > 0, yes = 1, no = 0))))+
  geom_col(aes(x = ANO, y = VAR, fill = COR), 
           na.rm = TRUE, color = "black",width = 0.30) +
  theme_minimal()+
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks=NULL,limits=c(-1.5,1.5),labels = scales::label_percent(scale = 100))+
  scale_x_discrete(breaks = NULL,expand = expansion(add = 1.5))+
  guides(fill = FALSE) +
  geom_text(aes(x = ANO, y = VAR, label=scales::percent(VAR,accuracy = 0.1)),size=3.5, vjust=1.5)+
  labs(title= "Anual",x = NULL, y = NULL) +
  scale_fill_manual(values = c("firebrick", "dodgerblue4"))+
  theme(panel.spacing = unit(1, "cm", data = NULL),
        plot.title = element_text(size = 12,
                                  face = "bold",
                                  hjust = 0.5))
  
  grid.arrange(GFVARDOM_MES,GFVARDOM_ANO,GFVARMES,GFVARANO, ncol=2,widths=c(4,1),heights= c(1.4,4))

#######################################################





BDS_VAR_2019_INT <- MOV_COM_INT %>% filter(ANO %in% c(2019), MES=="JAN")
BDS_VAR_2022_INT <- MOV_COM_INT %>% filter(ANO %in% c(2022), MES=="JAN")
BDS_VAR_ANO_INT <- bind_rows(BDS_VAR_2019_INT,BDS_VAR_2022_INT) %>% 
  transmute(ANO=ANO, TOTAL=TOTAL,VAR=(BDS_VAR_2022_INT$TOTAL-BDS_VAR_2019_INT$TOTAL)/BDS_VAR_2019_INT$TOTAL) 

GFVARANO_INT <-  
  ggplot(BDS_VAR_ANO_INT)+                                                                                                 
  geom_col(aes(x=ANO, y=TOTAL, fill = ANO),width = .5, position = position_dodge(preserve = "single"))+                
  scale_y_continuous(limits = c(0,180000))+ 
  scale_x_discrete(expand = expansion(add = .7))+
  geom_richtext(aes(x=ANO, y=20000, label= TOTAL, group=ANO),size=3, position = position_dodge(width = .9), angle=90)+
  theme_classic() + 
  theme(panel.background=element_rect(fill = "#F0F8FF")
  ) +
  labs(caption = "  ", x = NULL, y = NULL   
  ) +
  theme(legend.position = "none",
        plot.caption = element_text(size = 43))+
  scale_fill_manual(values = cores)


  GFVARMES_INT <- 
    MOV_COM_INT %>% filter(ANO %in% c(2019, 2022)) %>%
    ggplot()+
    geom_col(aes(x=MES, y=MOV, fill = ANO), position = position_dodge(preserve = "single"))+
    scale_y_continuous(limits = c(0,16000)) +
    coord_cartesian(xlim = c(1,12.5))+
    geom_richtext(aes(x=MES, y=2000, label = MOV, group=ANO),size=3, position = position_dodge(width = .9), angle=90)+
    theme_classic()+
    theme(legend.title    = element_text(size = 8) 
          ,legend.text     = element_text(size = 8)
          ,legend.key.size = unit(0.3, "cm")
    ) +
    labs(caption = "Fonte: TATIC FLOW", x = NULL, y = "Movimentos"
         ,fill = "ANO") +
    theme(legend.position = "bottom",
          legend.title=element_blank(),
          plot.caption = element_text(hjust = .65,
                                      face = "bold"))+
    scale_fill_manual(values = cores)             



    MOV_COM_INT_VAR2 <-    MOV_COM_INT_VAR %>% 
      transmute(ANO = MES,VAR=VAR) %>% 
      mutate(ANO= factor(ANO, levels = c("JAN","FEV","MAR","ABR","MAI","JUN","JUL","AGO","SET","OUT","NOV","DEZ")),
             COR = as.factor(ifelse(VAR > 0, yes = 1, no = 0)))
    
    
    
    GFVARINT_MES <- 
      ggplot(MOV_COM_INT_VAR2)+
      geom_col(aes(x = ANO, y = VAR, fill = COR), 
               na.rm = TRUE, color = "black",width = 0.30) +
      theme_minimal()+
      geom_hline(yintercept = 0) +
      scale_y_continuous(limits=c(-1.8,1.5),labels = scales::label_percent(scale = 100))+
      scale_x_discrete(breaks = NULL)+
      coord_cartesian(xlim = c(1,12.5))+
      guides(fill = FALSE) +
      geom_text(aes(x = ANO, y = VAR, label=scales::percent(VAR,accuracy = 0.1)),size= 3.5,vjust=1.5)+
      labs(title= "Mensal",x = NULL, y = "Variação 2022 / 2019") +
      scale_fill_manual(values = c("firebrick", "dodgerblue4"))+
      theme(panel.spacing = unit(1, "cm", data = NULL),
            plot.title = element_text(size = 12,
                                      face = "bold",
                                      hjust = 0.5))
    
    
    GFVARINT_ANO <- 
      ggplot(BDS_VAR_ANO_INT %>% filter(ANO=="2022") %>% 
               mutate(COR = as.factor(ifelse(VAR > 0, yes = 1, no = 0))))+
      geom_col(aes(x = ANO, y = VAR, fill = COR), 
               na.rm = TRUE, color = "black",width = 0.30) +
      theme_minimal()+
      geom_hline(yintercept = 0) +
      scale_y_continuous(breaks=NULL,limits=c(-1.5,1.5),labels = scales::label_percent(scale = 100))+
      scale_x_discrete(breaks = NULL,expand = expansion(add = 1.5))+
      guides(fill = FALSE) +
      geom_text(aes(x = ANO, y = VAR, label=scales::percent(VAR,accuracy = 0.1)),size=3.5, vjust=1.5)+
      labs(title= "Anual",x = NULL, y = NULL) +
      scale_fill_manual(values = c("firebrick", "dodgerblue4"))+
      theme(panel.spacing = unit(1, "cm", data = NULL),
            plot.title = element_text(size = 12,
                                      face = "bold",
                                      hjust = 0.5))
    
    grid.arrange(GFVARINT_MES,GFVARINT_ANO,GFVARMES_INT,GFVARANO_INT, ncol=2,widths=c(4,1),heights= c(1.4,4))





#######################################################


ggplot(BDS_PONTUALIDADE_ARR %>% filter(ANO=="2022"), aes(x = APT_ICAO, y = FLIGHTS, fill = SLOT))+
  geom_col(position = position_stack(reverse = TRUE))+
  scale_fill_manual(values = CORESPONT)+
  scale_y_continuous(labels = scales::label_percent())+
  geom_label(aes(x = APT_ICAO, y = FLIGHTS, label = scales::percent(FLIGHTS,accuracy = 1)),size = 3, position = position_stack(vjust = .8,reverse = TRUE),show.legend = FALSE)+
  coord_flip()+
  theme_minimal()+
  labs(title = paste("Pontualidade ARR em 2022"), caption = "Fonte: DECEA", x = NULL, y = NULL
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


#################################################################


BDS_KPI_15_DF %>% filter(movimentos.ano%in% c(2022)&movimentos.total_movimentos>2300) %>% 
  ggplot()+
  geom_col(aes(x=movimentos.fluxo, y=movimentos.percentual_ano_70, fill = as.factor(movimentos.ano)), position = position_dodge(preserve = "single"))+
  geom_point(aes(x=movimentos.fluxo, y=movimentos.total_movimentos))+
  scale_y_continuous(limits = c(0,10))+
  theme_minimal()+
  labs(fill=NULL)+
  geom_text(aes(x=movimentos.fluxo, y=movimentos.percentual_ano_70, label = round(movimentos.percentual_ano_70,1), group = movimentos.ano),size = 3,vjust=-.8,
            position = position_dodge(width = 0.9))+
  geom_text(data=VAR15,aes(x=ROTA, y=9, label = scales::percent(VAR,accuracy = 0.1)), size = 3,color="red",
            hjust = 0.5,
            position = position_dodge(width = 0.9),fontface="bold")+
  theme(axis.text.x=element_text(angle = 90, hjust = 0))+
  labs(title = "Variabilidade do tempo de voo 70%", caption ="FONTE: VRA", x=NULL, y="Variação do tempo de voo (min)")+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 18,
                                  face = "bold",
                                  hjust = 0.5),
        plot.caption = element_text(size = 10, face = "bold",hjust = 0.5))+
  scale_fill_manual(values = c("#B0C4DE", "#708090", "#4682B4"))






BDS_KPI_15_DF %>% filter(movimentos.ano%in% c(2022)&movimentos.total_movimentos>2300) %>% 
  ggplot()+
  geom_point(aes(x=movimentos.fluxo, y=movimentos.total_movimentos))+
  scale_y_continuous(breaks = c(2000,3000,7000))

#####################
TOTAL_MOV_SISC_FIR %>% filter(FIR=="SISCEAB"& ANO%in%c("2019","2022")) %>% 
  ggplot()+
geom_col(aes(x=ANO, y=TOTAL, fill = ANO),width = .5, position = position_dodge(preserve = "single"))+                
  scale_y_continuous(breaks = NULL, limits = c(0,1700000))+ 
  scale_x_discrete(expand = expansion(add = .7))+
  geom_text(aes(x=ANO, y=TOTAL/2, label = format(TOTAL, big.mark = ".", scientific = FALSE), group = ANO), size = 4,                      
            position = position_dodge(width = 0.9),angle = 90)+
  theme_classic()+ 
  theme(panel.background=element_rect(fill = "#F0F8FF")
        ,legend.title    = element_text(size = 8) 
        ,legend.text     = element_text(size = 8)
        ,legend.key.size = unit(0.3, "cm")
  ) +
  #labs(title = "Variação anual",subtitle = scales::percent(round(BDS_VAR_ANO_INT$VAR,2)),caption = "  ", x = NULL, y = NULL) +
  theme(legend.position = "none",
        legend.title=element_blank(),
        plot.title = element_text(size = 13,
                                  face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 7, colour = "red",hjust = .5, vjust = -7,face = "bold"),
        plot.caption = element_text(size = 43))+
  scale_fill_manual(values = cores) 



