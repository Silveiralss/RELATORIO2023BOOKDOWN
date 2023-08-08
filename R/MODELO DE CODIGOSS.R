
PBWG_BRA_punctuality <- read_csv("C:/Users/silveiralss/OneDrive - DECEA/Documentos/Relatório/Relatório/data/PBWG-BRA-punctuality_ADR2.csv") %>%
  mutate(APT_ICAO = as.factor(APT_ICAO), DATE = ym(DATE), PHASE = as.factor(PHASE)) %>%
  separate(DATE, c("ANO", "MES", "DIA"), sep = "-")
############################################################

cores <- c("#B0C4DE",
           "#708090",
           "#4682B4",
           "#191970")
############################


PONT <- PBWG_BRA_punctuality %>%
  select(APT_ICAO, ANO, PHASE,N_VALID,"(-1e+09.-60]","(-60.-55]","(-55.-50]",
         "(-50.-45]","(-45.-40]","(-40.-35]","(-35.-30]","(-30.-25]",
         "(-25.-20]","(-20.-15]","(-15.-10]","(-10.-5]","(-5.0]",
         "(0.5]","(5.10]","(10.15]","(15.20]","(20.25]","(25.30]",
         "(30.35]","(35.40]","(40.45]","(45.50]","(50.55]",
         "(55.60]","(60.1e+09]") %>% 
  group_by(APT_ICAO, ANO, PHASE) %>%
  summarise(N_VALID = sum(N_VALID)
            ,`-60` = sum(`(-1e+09.-60]`)
            , `-55` = sum(`(-60.-55]`)
            , `-50` = sum(`(-55.-50]`)
            ,`-45` = sum(`(-50.-45]`)
            , `-40` = sum(`(-45.-40]`)
            , `-35` = sum(`(-40.-35]`)
            ,`-30` = sum(`(-35.-30]`)
            ,`-25` = sum(`(-30.-25]`)
            , `-20` = sum(`(-25.-20]`)
            , `-15` = sum(`(-20.-15]`)
            , `-10` = sum(`(-15.-10]`)
            , `-5` = sum(`(-10.-5]`)
            , `-1` = sum(`(-5.0]`)
            , `+1` = sum(`(0.5]`)
            , `+5` = sum(`(5.10]`)
            , `+10` = sum(`(10.15]`)
            , `+15` = sum(`(15.20]`)
            , `+20` = sum(`(20.25]`)
            , `+25` = sum(`(25.30]`)
            , `+30`= sum(`(30.35]`)
            , `+35` = sum(`(35.40]`)
            , `+40` = sum(`(40.45]`)
            , `+45` = sum(`(45.50]`)
            , `+50` = sum(`(50.55]`)
            , `+55` = sum(`(55.60]`)
            , `+60` = sum(`(60.1e+09]`))%>%
  mutate(SOMA = `-60`+`-55`
         + `-50`+ `-45`
         + `-40`+ `-35`
         + `-30`+ `-25`
         + `-20`+ `-15`
         +`-10`+ `-5`
         +`-1`+`+1`
         +`+5`+ `+10`
         +`+15`+ `+20`
         + `+25`+ `+30`
         + `+35`+ `+40`
         + `+45`+ `+50`
         + `+55`+ `+60`) %>% 
  mutate(Early = (`-60`+ `-55`
                  + `-50`+ `-45`
                  + `-40`+ `-35`
                  + `-30`+ `-25`
                  + `-20`+ `-15`)/SOMA,
         Early15 = (`-10`+ `-5`)/SOMA,
         Within5 = (`-1`+ `+1`)/SOMA,
         Late5_15 = (`+5`+ `+10`)/SOMA,
         Late = (`+15`+ `+20`
                 + `+25`+ `+30`
                 + `+35`+ `+40`
                 + `+45`+ `+50`
                 + `+55`+ `+60`)/SOMA)







########################################################


PONT_ARR <- PONT %>%
  filter(PHASE == "ARR") %>%
  select(APT_ICAO, ANO, Early,Early15, Within5, Late5_15, Late) %>%
  tidyr::pivot_longer(cols = Early:Late
                      , names_to = "SLOT"
                      , values_to = "FLIGHTS")


P1 <- PONT_ARR %>% filter(ANO == 2021) %>% 
  ggplot(data = PONT_ARR , mapping = aes(x = APT_ICAO, y = FLIGHTS, fill = SLOT)) +
  geom_col(position = "fill") +
  scale_fill_manual(name = NULL, labels = c("Late", "Late 5-15", "Within 5", "Early 15-5", "Early"), values = RColorBrewer::brewer.pal(5, "RdYlBu")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Pontualidade de CHEGADA", caption = "Fonte: SIROS e TATIC FLOW", x = NULL, y = NULL) +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 20,
                              face = "bold",
                              hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "#8B0000"),
    plot.caption = element_text(hjust = .5,
                                face = "bold"))+
  theme( legend.position = "top"
         ,legend.title    = element_text(size = 8) 
         ,legend.text     = element_text(size = 8)
         ,legend.key.size = unit(0.3, "cm")) +
  labs(x = NULL, y = NULL) +
  coord_flip()

P1


#####



################################################################################

PONT_DEP <- PONT %>%
  filter(PHASE == "DEP") %>%
  select(APT_ICAO, ANO, Early,Early15, Within5, Late5_15, Late) %>%
  tidyr::pivot_longer(cols = Early:Late
                      , names_to = "SLOT"
                      , values_to = "FLIGHTS")


P2 <-PONT_DEP %>% filter(ANO == 2021) %>% 
  ggplot(data = PONT_DEP , mapping = aes(x = APT_ICAO, y = FLIGHTS, fill = SLOT)) +
  geom_col(position = "fill") +
  scale_fill_manual(name = NULL, labels = c("Late", "Late 5-15", "Within 5", "Early 15-5", "Early"), values = RColorBrewer::brewer.pal(5, "RdYlBu")) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Pontualidade de SAÍDA", caption = "Fonte: SIROS e TATIC FLOW", x = NULL, y = NULL) +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 20,
                              face = "bold",
                              hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "#8B0000"),
    plot.caption = element_text(hjust = .5,
                                face = "bold"))+
  theme( legend.position = "top"
         ,legend.title    = element_text(size = 8) 
         ,legend.text     = element_text(size = 8)
         ,legend.key.size = unit(0.3, "cm")) +
  labs(x = NULL, y = NULL) +
  coord_flip()
P2

P1+P2

########################################################


#########################################################################################


################################################



#############################################################################

B42 <- PONT %>%
  ungroup() %>% 
  select(`-60`,`-55`
         , `-50`, `-45`
         , `-40`, `-35`
         , `-30`, `-25`
         , `-20`, `-15`
         ,`-10`, `-5`
         ,`-1`,`+1`
         ,`+5`, `+10`
         ,`+15`, `+20`
         , `+25`, `+30`
         , `+35`, `+40`
         , `+45`, `+50`
         , `+55`, `+60`) %>%
  colSums() %>%
  as.data.frame() %>% 
  rename(MOVIMENTOS = ".") %>%
  mutate(TEMPO = as.factor(c("-60","-55","-50","-45"
                             ,"-40","-35"
                             , "-30", "-25"
                             , "-20", "-15"
                             ,"-10", "-5"
                             ,"-1","+1"
                             ,"+5", "+10"
                             ,"+15", "+20"
                             , "+25", "+30"
                             , "+35", "+40"
                             , "+45", "+50"
                             , "+55", "+60"))) %>%
  mutate(ATRASO = case_when(TEMPO == "-1" ~ "Exato",
                            TEMPO == "+1" ~ "Exato",
                            TEMPO == "+5" ~ "Pontual",
                            TEMPO == "-5" ~ "Pontual",
                            TEMPO == "+10" ~ "Pontual",
                            TEMPO == "-10" ~ "Pontual",
                            TEMPO == "-15" ~ "Adiantado",
                            TEMPO == "+15" ~ "Atrasado",
                            TEMPO == "-20" ~ "Adiantado",
                            TEMPO == "-25" ~ "Adiantado",
                            TEMPO == "-30" ~ "Adiantado",
                            TEMPO == "-35" ~ "Adiantado",
                            TEMPO == "-40" ~ "Adiantado",
                            TEMPO == "-45" ~ "Adiantado",
                            TEMPO == "-50" ~ "Adiantado",
                            TEMPO == "-55" ~ "Adiantado",
                            TEMPO == "-60" ~ "Adiantado",
                            TEMPO == "+15" ~ "Atrasado",
                            TEMPO == "+20" ~ "Atrasado",
                            TEMPO == "+25" ~ "Atrasado",
                            TEMPO == "+30" ~ "Atrasado",
                            TEMPO == "+35" ~ "Atrasado",
                            TEMPO == "+40" ~ "Atrasado",
                            TEMPO == "+45" ~ "Atrasado",
                            TEMPO == "+50" ~ "Atrasado",
                            TEMPO == "+55" ~ "Atrasado",
                            TEMPO == "+60" ~ "Atrasado"
  ))

B42$TEMPO <- factor(B42$TEMPO, levels = c("-60","-55","-50","-45"
                                          ,"-40","-35"
                                          , "-30", "-25"
                                          , "-20", "-15"
                                          ,"-10", "-5"
                                          ,"-1","+1"
                                          ,"+5", "+10"
                                          ,"+15", "+20"
                                          , "+25", "+30"
                                          , "+35", "+40"
                                          , "+45", "+50"
                                          , "+55", "+60"))






ggplot(B42, aes(x=TEMPO, y = MOVIMENTOS))+
  geom_col(aes(fill= ATRASO))+
  labs(fill="ATRASO") +
  scale_y_continuous(labels = scales::label_number())+
  scale_fill_manual(values = c("Adiantado" = "#A0522D",
                               "Pontual" = "#BDB76B",
                               "Exato" = "#9ACD32",
                               "Atrasado" = "#A0522D"))+
  labs(title = "Figura 42 - Distribuição da pontualidade em 2021", caption = "Fonte: SIROS e TATIC FLOW",  x = NULL) +
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        plot.title = element_text(size = 20L,
                                  face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 12, color = "#8B0000"),
        plot.caption = element_text(hjust = .5,
                                    face = "bold"))


#################################################################################


BB <- PBWG_BRA_punctuality %>%
  select(APT_ICAO, ANO, MES, PHASE, N_VALID,"(-1e+09.-60]","(-60.-55]","(-55.-50]",
         "(-50.-45]","(-45.-40]","(-40.-35]","(-35.-30]","(-30.-25]",
         "(-25.-20]","(-20.-15]","(-15.-10]","(-10.-5]","(-5.0]",
         "(0.5]","(5.10]","(10.15]","(15.20]","(20.25]","(25.30]",
         "(30.35]","(35.40]","(40.45]","(45.50]","(50.55]",
         "(55.60]","(60.1e+09]") %>% 
  group_by(APT_ICAO, ANO, MES, PHASE) %>%
  summarise(N_VALID = sum(N_VALID)
            ,`-60` = sum(`(-1e+09.-60]`)
            , `-55` = sum(`(-60.-55]`)
            , `-50` = sum(`(-55.-50]`)
            ,`-45` = sum(`(-50.-45]`)
            , `-40` = sum(`(-45.-40]`)
            , `-35` = sum(`(-40.-35]`)
            ,`-30` = sum(`(-35.-30]`)
            ,`-25` = sum(`(-30.-25]`)
            , `-20` = sum(`(-25.-20]`)
            , `-15` = sum(`(-20.-15]`)
            , `-10` = sum(`(-15.-10]`)
            , `-5` = sum(`(-10.-5]`)
            , `-1` = sum(`(-5.0]`)
            , `+1` = sum(`(0.5]`)
            , `+5` = sum(`(5.10]`)
            , `+10` = sum(`(10.15]`)
            , `+15` = sum(`(15.20]`)
            , `+20` = sum(`(20.25]`)
            , `+25` = sum(`(25.30]`)
            , `+30`= sum(`(30.35]`)
            , `+35` = sum(`(35.40]`)
            , `+40` = sum(`(40.45]`)
            , `+45` = sum(`(45.50]`)
            , `+50` = sum(`(50.55]`)
            , `+55` = sum(`(55.60]`)
            , `+60` = sum(`(60.1e+09]`))%>%
  mutate(SOMA = `-60`+`-55`
         + `-50`+ `-45`
         + `-40`+ `-35`
         + `-30`+ `-25`
         + `-20`+ `-15`
         +`-10`+ `-5`
         +`-1`+`+1`
         +`+5`+ `+10`
         +`+15`+ `+20`
         + `+25`+ `+30`
         + `+35`+ `+40`
         + `+45`+ `+50`
         + `+55`+ `+60`) %>% 
  mutate(Early = (`-60`+ `-55`
                  + `-50`+ `-45`
                  + `-40`+ `-35`
                  + `-30`+ `-25`
                  + `-20`+ `-15`)/SOMA,
         Early15 = (`-10`+ `-5`)/SOMA,
         Within5 = (`-1`+ `+1`)/SOMA,
         Late5_15 = (`+5`+ `+10`)/SOMA,
         Late = (`+15`+ `+20`
                 + `+25`+ `+30`
                 + `+35`+ `+40`
                 + `+45`+ `+50`
                 + `+55`+ `+60`)/SOMA) %>% 
  filter(ANO =="2021") %>% 
  mutate(MES = case_when(MES == "01" ~ "JAN",
                         MES == "02" ~ "FEV",
                         MES == "03" ~ "MAR",
                         MES == "04" ~ "ABR",
                         MES == "05" ~ "MAI",
                         MES == "06" ~ "JUN",
                         MES == "07" ~ "JUL",
                         MES == "08" ~ "AGO",
                         MES == "09" ~ "SET",
                         MES == "10" ~ "OUT",
                         MES == "11" ~ "NOV",
                         MES == "12" ~ "DEZ"))


######
BB_ARR <- BB %>%  filter(PHASE =="ARR") %>% ungroup %>%
  group_by(MES) %>% summarise_at(vars(`-60`,`-55`
                                      , `-50`, `-45`
                                      , `-40`, `-35`
                                      , `-30`, `-25`
                                      , `-20`, `-15`
                                      ,`+15`, `+20`
                                      , `+25`, `+30`
                                      , `+35`, `+40`
                                      , `+45`, `+50`
                                      , `+55`, `+60`),                 
                                 list(. = sum)) %>% 
  transmute(MES = MES, ADIANTADO = `-60_.`+`-55_.`+`-50_.`+`-45_.`+`-40_.`+`-35_.`+`-30_.`+`-25_.`+`-20_.`+`-15_.`,
            ATRASADO = `+15_.`+`+20_.`+`+25_.`+`+30_.`+`+35_.`+`+40_.`+`+45_.`+`+50_.`+`+60_.`) %>%
  mutate(MES = MES, ADIANTADO_PCT = ADIANTADO/sum(ADIANTADO,ATRASADO)*100, 
         ATRASADO_PCT = ATRASADO/sum(ADIANTADO,ATRASADO)*100) %>% pivot_longer(4:5,names_to = "OBS", values_to = "TIME")

BB_ARR$MES <- factor(BB_ARR$MES, levels = c("JAN", "FEV",
                                            "MAR", "ABR",
                                            "MAI", "JUN",
                                            "JUL", "AGO",
                                            "SET", "OUT",
                                            "NOV", "DEZ"))









###### 

BB_DEP <- BB %>%  filter(PHASE =="DEP") %>% ungroup %>%
  group_by(MES) %>% summarise_at(vars(`-60`,`-55`
                                      , `-50`, `-45`
                                      , `-40`, `-35`
                                      , `-30`, `-25`
                                      , `-20`, `-15`
                                      ,`+15`, `+20`
                                      , `+25`, `+30`
                                      , `+35`, `+40`
                                      , `+45`, `+50`
                                      , `+55`, `+60`),                 
                                 list(. = sum)) %>% 
  transmute(MES = MES, ADIANTADO = `-60_.`+`-55_.`+`-50_.`+`-45_.`+`-40_.`+`-35_.`+`-30_.`+`-25_.`+`-20_.`+`-15_.`,
            ATRASADO = `+15_.`+`+20_.`+`+25_.`+`+30_.`+`+35_.`+`+40_.`+`+45_.`+`+50_.`+`+60_.`) %>%
  mutate(MES = MES, ADIANTADO_PCT = ADIANTADO/sum(ADIANTADO,ATRASADO)*100, 
         ATRASADO_PCT = ATRASADO/sum(ADIANTADO,ATRASADO)*100) %>% pivot_longer(4:5,names_to = "OBS", values_to = "TIME")

BB_DEP$MES <- factor(BB_DEP$MES, levels = c("JAN", "FEV",
                                            "MAR", "ABR",
                                            "MAI", "JUN",
                                            "JUL", "AGO",
                                            "SET", "OUT",
                                            "NOV", "DEZ"))







B33 <- PONT %>%
  select(APT_ICAO, ANO, PHASE, N_VALID, Early15, Within5, Late5_15, SOMA) %>% 
  filter(PHASE == "DEP", ANO < 2022) %>%
  mutate(REGIONAL = case_when(APT_ICAO == "SBBR" ~ "CINDACTA I",
                              APT_ICAO == "SBCF" ~ "CINDACTA I",
                              APT_ICAO == "SBGR" ~ "CRCEA-SE",
                              APT_ICAO == "SBKP" ~ "CRCEA-SE",
                              APT_ICAO == "SBRF" ~ "CINDACTA III",
                              APT_ICAO == "SBRJ" ~ "CRCEA-SE",
                              APT_ICAO == "SBSP" ~ "CRCEA-SE",
                              APT_ICAO == "SBSV" ~ "CINDACTA III",
                              APT_ICAO == "SBCT" ~ "CINDACTA II",
                              APT_ICAO == "SBFL" ~ "CINDACTA II",
                              APT_ICAO == "SBPA" ~ "CINDACTA II"),.before = APT_ICAO) %>%
  mutate(PONTUALDEP = Early15+Within5+Late5_15, .before = Early15)


ggplot(B33, aes(x = APT_ICAO, y = PONTUALDEP, fill = as.factor(ANO)))+
  geom_col(position = position_dodge2()) +
  geom_hline(yintercept = mean(B33$PONTUALDEP), color = "red")+
  facet_grid(cols = vars(REGIONAL),switch= "x", scales = "free_x") +
  labs(fill="Ano") +
  scale_y_continuous(labels = scales::label_percent())+
  geom_text(aes( y = PONTUALDEP, label = scales::percent(PONTUALDEP, accuracy = 1), group = ANO, hjust = -.15, angle = 90)
            , position = position_dodge(width=.9), size = 4, color = "black", fontface = "bold")+
  scale_fill_manual(values = cores)+
  labs(title = "Figura 33 - Pontualidade de partida por aeroportos", caption = "Fonte: SIROS e TATIC FLOW", x = NULL, y = NULL) +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 20L,
                              face = "bold",
                              hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "#8B0000"),
    plot.caption = element_text(hjust = .5,
                                face = "bold"))




#####################################


PBWG_BRA_Variability_ADR <- read_csv("C:/Users/silveiralss/OneDrive - DECEA/Documentos/Relatório/Relatório/data/PBWG-BRA-Variability_ADR.csv",
                                     locale = locale(decimal_mark = ","))
PBWGKPI15 <- PBWG_BRA_Variability_ADR %>%mutate(NOVO = `(P85-P15)/2`*Arrivals)%>%
  mutate(ADEP = as.factor(ADEP), ADES = as.factor(ADES), Date = ym(Date), INDICADOR = `(P85-P15)/2`) %>%
  group_by(ROTA = as.factor(paste(ADEP,ADES, sep = "->")),Date = year(Date)) %>%  filter(Date == 2021) %>% summarise(ROTA, Date, Arrivals = sum(Arrivals), NOVO = sum(NOVO)) %>%
  mutate(IND = round(NOVO/Arrivals, digits = 1)) %>% distinct(ROTA, .keep_all = TRUE) %>% arrange(desc(Arrivals)) %>% head(30)



ggplot(data = PBWGKPI15)+
  geom_col(aes(x= reorder(ROTA, -IND), y=IND), fill = "#4682B4")+
  geom_text(aes(x=ROTA, y=IND, label = IND, group = Date),size = 3, 
            hjust = -0.1,
            position = position_dodge(width = 0.9),angle = 90)+
  scale_y_continuous(breaks = 2:7)+
  theme_minimal()+
  theme(axis.text.x=element_text(angle = 90, hjust = 0))+
  labs(title = "KPI 15 com variante 70%", caption ="FONTE: VRA", x=NULL, y="variação do tempode voo(min)")+
  theme(
    plot.title = element_text(size = 20,
                              face = "bold",
                              hjust = 0.5),
    plot.caption = element_text(size = 10, face = "bold",hjust = 0.5))



PBWG46 <- PBWG_BRA_Variability_ADR %>%mutate(NOVO = `(P85-P15)/2`*Arrivals)%>%
  mutate(ADEP = as.factor(ADEP), ADES = as.factor(ADES), Date = ym(Date), INDICADOR = `(P85-P15)/2`) %>%
  group_by(ROTA = as.factor(paste(ADEP,ADES, sep = "->")),Date = year(Date)) %>%
  summarise(ROTA, Date, Arrivals = sum(Arrivals), NOVO = sum(NOVO)) %>%
  mutate(IND = round(NOVO/Arrivals, digits = 1)) %>% distinct(ROTA, .keep_all = TRUE) %>%
  arrange(desc(Arrivals)) %>%
  mutate(Date = as.factor(Date)) %>% group_by(Date) %>%
  filter(ROTA == "SBRJ->SBSP" | ROTA == "SBSP->SBRJ" | ROTA == "SBGR->SBRF" | ROTA == "SBRF->SBGR" | ROTA == "SBPA->SBGR"
         | ROTA == "SBGR->SBPA" | ROTA == "SBFZ->SBGR" | ROTA == "SBGR->SBFZ" | ROTA == "SBGR->SBSV" | ROTA == "SBSV->SBGR") %>%
  filter(Date!="2022")


ggplot(PBWG46)+
  geom_col(aes(x=ROTA, y=IND, fill = Date), position = "dodge")+
  scale_y_continuous(breaks = 2:7)+
  theme_minimal()+
  geom_text(aes(x=ROTA, y=IND, label = IND, group = Date),size = 3, 
            hjust = -0.1,
            position = position_dodge(width = 0.9),angle = 90)+
  theme(axis.text.x=element_text(angle = 90, hjust = 0))+
  labs(title = "variante 70%", caption ="FONTE: VRA", x=NULL, y=NULL)+
  theme(
    plot.title = element_text(size = 20,
                              face = "bold",
                              hjust = 0.5),
    plot.caption = element_text(size = 10, face = "bold",hjust = 0.5))+
  scale_fill_manual(values = c("#B0C4DE", "#708090", "#4682B4"))



BBUMP1 <- PBWG46 %>% filter(Date=="2021") %>%
  mutate(RANK = order(Arrivals, decreasing = TRUE), .before = ROTA)
BBUMP2 <- PBWG46 %>% filter(Date=="2020") %>%
  mutate(RANK = order(Arrivals, decreasing = TRUE), .before = ROTA)
BBUMP3 <- PBWG46 %>% filter(Date=="2019") %>%
  mutate(RANK = order(Arrivals, decreasing = TRUE), .before = ROTA)
BBUMP <- bind_rows(BBUMP1,BBUMP2,BBUMP3) %>% 
  mutate(Date=as.double(Date), RANK = as.numeric(RANK))


ggplot(BBUMP, aes(Date, RANK, color = ROTA)) +
  geom_point(size = 3)+
  geom_bump(size = 2, smooth = 10)+
  scale_x_continuous ( breaks = 2019 : 2021 , minor_breaks = 2019 : 2021 , expand = c ( .2 , .2 ))+
  scale_y_reverse(breaks = BBUMP$RANK, labels= BBUMP$ROTA)+
  theme_minimal()+
  theme(legend.position = "none",
        panel.grid.major = element_blank())+
  labs(y = NULL ,
       x = "2019                            2020                            2021")



#ESSA BDS É ANTIGA (SEM API) ESTÁ COMENTADA PARA SERVIR DE CONSULTA FUTURA DE CÓDIGOS

#PBWG_BRA_region_traffic <- read_csv("C:/Users/silveiralss/OneDrive - DECEA/Documentos/Relatório/Relatório/data/PBWG-BRA-region-traffic.csv") %>% 
# separate(DATE, into = c("ANO", "MES"), sep = "-") %>% mutate(MES = case_when(MES == "01" ~ "JAN",
#                                                                  MES == "02" ~ "FEV",
#                                                                 MES == "03" ~ "MAR",
#                                                                   MES == "04" ~ "ABR",
#                                                                  MES == "05" ~ "MAI",
#                                                                 MES == "06" ~ "JUN",
#                                                                MES == "07" ~ "JUL",
#                                                               MES == "08" ~ "AGO",
#                                                              MES == "09" ~ "SET",
#                                                             MES == "10" ~ "OUT",
#                                                            MES == "11" ~ "NOV",
#                                                           MES == "12" ~ "DEZ"))


#MOV_COM <- PBWG_BRA_region_traffic %>%
# mutate(MES=factor(MES,levels = c("JAN","FEV","MAR","ABR","MAI","JUN","JUL","AGO","SET","OUT","NOV","DEZ"))) %>% 
#group_by(ANO,MES) %>%
#summarise(ARRS=sum(ARRS),DEPS=sum(DEPS),HEAVY=sum(HEAVY),MED=sum(MED),LIGHT=sum(LIGHT),ARRS_DOM=sum(ARRS_DOM), DEPS_DOM=sum(DEPS_DOM))



#PBWG_BRA_airport_traffic <- read_csv("C:/Users/silveiralss/OneDrive - DECEA/Documentos/Relatório/Relatório/data/PBWG-BRA-airport-traffic.csv")

#rm(MOV_COM)






##################################################################



#Variabilidade de tempo

library(tidyverse)
library(lubridate)
library(dplyr)
library(plotly)
library(readr)
library(patchwork) # este pacote soma os gráficos
library(ggbump)

PBWG_BRA_Variability_ADR <- read_csv("C:/Users/silveiralss/OneDrive - DECEA/Documentos/Relatório/Relatório/data/PBWG-BRA-Variability_ADR.csv", 
                                     locale = locale(decimal_mark = ","))




PBWGKPI15 <- PBWG_BRA_Variability_ADR %>%mutate(NOVO = `(P85-P15)/2`*Arrivals)%>%
  mutate(ADEP = as.factor(ADEP), ADES = as.factor(ADES), Date = ym(Date), INDICADOR = `(P85-P15)/2`) %>% 
  group_by(ROTA = as.factor(paste(ADEP,ADES, sep = "->")),Date = year(Date)) %>%  filter(Date == 2021) %>% summarise(ROTA, Date, Arrivals = sum(Arrivals), NOVO = sum(NOVO)) %>% 
  mutate(IND = round(NOVO/Arrivals, digits = 1)) %>% distinct(ROTA, .keep_all = TRUE) %>% arrange(desc(Arrivals)) %>% head(30)


ggplot(data = PBWGKPI15)+
  geom_col(aes(x= reorder(ROTA, -IND), y=IND), fill = "#4682B4")+
  scale_y_continuous(breaks = 2:7)+
  theme_minimal()+
  theme(axis.text.x=element_text(angle = 90, hjust = 0))+
  labs(title = "KPI 15 com variante 70%", caption ="FONTE: VRA", x=NULL, y="variação do tempode voo(min)")+
  theme(
    plot.title = element_text(size = 20,
                              face = "bold",
                              hjust = 0.5),
    plot.caption = element_text(size = 10, face = "bold",hjust = 0.5))


#######################################################################################


PBWG46 <- PBWG_BRA_Variability_ADR %>%mutate(NOVO = `(P85-P15)/2`*Arrivals)%>%
  mutate(ADEP = as.factor(ADEP), ADES = as.factor(ADES), Date = ym(Date), INDICADOR = `(P85-P15)/2`) %>% 
  group_by(ROTA = as.factor(paste(ADEP,ADES, sep = "->")),Date = year(Date)) %>%
  summarise(ROTA, Date, Arrivals = sum(Arrivals), NOVO = sum(NOVO)) %>% 
  mutate(IND = round(NOVO/Arrivals, digits = 1)) %>% distinct(ROTA, .keep_all = TRUE) %>%
  arrange(desc(Arrivals)) %>% 
  mutate(Date = as.factor(Date)) %>% group_by(Date) %>%
  filter(ROTA == "SBRJ->SBSP" | ROTA == "SBSP->SBRJ" | ROTA == "SBGR->SBRF" | ROTA == "SBRF->SBGR" | ROTA == "SBPA->SBGR"
         | ROTA == "SBGR->SBPA" | ROTA == "SBFZ->SBGR" | ROTA == "SBGR->SBFZ" | ROTA == "SBGR->SBSV" | ROTA == "SBSV->SBGR") %>%
  filter(Date!="2022")

ggplot(PBWG46)+
  geom_col(aes(x=ROTA, y=IND, fill = Date), position = "dodge")+
  scale_y_continuous(breaks = 2:7)+
  theme_minimal()+
  #geom_text(aes(x=ROTA, y = IND, label=PBWG46$IND))+
  theme(axis.text.x=element_text(angle = 90, hjust = 0))+
  labs(title = "variante 70%", caption ="FONTE: VRA", x=NULL, y=NULL)+
  theme(
    plot.title = element_text(size = 20,
                              face = "bold",
                              hjust = 0.5),
    plot.caption = element_text(size = 10, face = "bold",hjust = 0.5))+
  scale_fill_manual(values = c("#B0C4DE", "#708090", "#4682B4"))


#################################################################################







BBUMP1 <- PBWG46 %>% filter(Date=="2021") %>% 
  mutate(RANK = order(Arrivals, decreasing = TRUE), .before = ROTA)
BBUMP2 <- PBWG46 %>% filter(Date=="2020") %>% 
  mutate(RANK = order(Arrivals, decreasing = TRUE), .before = ROTA)
BBUMP3 <- PBWG46 %>% filter(Date=="2019") %>% 
  mutate(RANK = order(Arrivals, decreasing = TRUE), .before = ROTA)

BBUMP <- bind_rows(BBUMP1,BBUMP2,BBUMP3)


ggplot(BBUMP, aes(x = Date, y = RANK, color=ROTA)) +
  geom_point(size = 8) +
  geom_bump(size = 5)


# usar scale_y_continuous(sec.axis = sec_axis(~ . * 1, name = "Flights")) ou scale_y_continuous(sec.axis = dup_axis())
#para criar um segundo eixo y no lado direito.











