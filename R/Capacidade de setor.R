



VERTICES_SETORES_FIR_SET22 <- read_excel("C:/Users/silveiralss/OneDrive - DECEA/Documentos/Relatório/Relatório/data/VERTICES_SETORES_FIR_SET22.xlsx")


PONT_TEXTO_BS <- data.frame(Y=c(-22.16,-21.35,-22.05,-20.03,-20.23,-20.33,-17.23,-17.35,-13.37,-14.30,-14.51,-16.48,-17.50,-18.51,-17.37,-20.20,-22.17),
                            X=c(-45.62,-46.08,-47.29,-46.30,-47.58,-49.30,-48.46,-51.46,-52.14,-49.37,-46.10,-46.39,-47.01,-45.23,-43.35,-44.50,-48.56), 
                            SETOR=c("BS-S1","BS-S2","BS-S3","BS-S4","BS-S5","BS-S6","BS-S7","BS-S8","BS-S9","BS-S10","BS-S11","BS-S12","BS-S13","BS-S14","BS-S15","BS-S16","BS-S17"), 
                            CHS=c(46,46,46,47,47,48,47,49,49,49,49,47,47,46,45,46,47))

#Capacidade Horária de Setor FIR SBBS(28)

VERTICES_SETORES_FIR_SBBS <- VERTICES_SETORES_FIR_SET22 %>% filter(relatedfir=="SBBS")
ggplot()+
  geom_polygon(data=VERTICES_SETORES_FIR_SBBS %>% filter(nam=="SECTOR 1"),aes(x=LONGITUDE,y = LATITUDE),fill ="#FFFF99", colour= "#BDB76B",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBBS %>% filter(nam=="SECTOR 2"),aes(x=LONGITUDE,y = LATITUDE),fill ="#FFFF99", colour= "#BDB76B",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBBS %>% filter(nam=="SECTOR 3"),aes(x=LONGITUDE,y = LATITUDE),fill ="#FFFF99", colour= "#BDB76B",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBBS %>% filter(nam=="SECTOR 4"),aes(x=LONGITUDE,y = LATITUDE),fill ="#FFFF99", colour= "#BDB76B",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBBS %>% filter(nam=="SECTOR 5"),aes(x=LONGITUDE,y = LATITUDE),fill ="#FFFF99", colour= "#BDB76B",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBBS %>% filter(nam=="SECTOR 6"),aes(x=LONGITUDE,y = LATITUDE),fill ="#FFFF99", colour= "#BDB76B",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBBS %>% filter(nam=="SECTOR 7"),aes(x=LONGITUDE,y = LATITUDE),fill ="#FFFF99", colour= "#BDB76B",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBBS %>% filter(nam=="SECTOR 8"),aes(x=LONGITUDE,y = LATITUDE),fill ="#FFFF99", colour= "#BDB76B",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBBS %>% filter(nam=="SECTOR 9"),aes(x=LONGITUDE,y = LATITUDE),fill ="#FFFF99", colour= "#BDB76B",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBBS %>% filter(nam=="SECTOR 10"),aes(x=LONGITUDE,y = LATITUDE),fill ="#FFFF99", colour= "#BDB76B",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBBS %>% filter(nam=="SECTOR 11"),aes(x=LONGITUDE,y = LATITUDE),fill ="#FFFF99", colour= "#BDB76B",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBBS %>% filter(nam=="SECTOR 12"),aes(x=LONGITUDE,y = LATITUDE),fill ="#FFFF99", colour= "#BDB76B",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBBS %>% filter(nam=="SECTOR 13"),aes(x=LONGITUDE,y = LATITUDE),fill ="#FFFF99", colour= "#BDB76B",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBBS %>% filter(nam=="SECTOR 14"),aes(x=LONGITUDE,y = LATITUDE),fill ="#FFFF99", colour= "#BDB76B",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBBS %>% filter(nam=="SECTOR 15"),aes(x=LONGITUDE,y = LATITUDE),fill ="#FFFF99", colour= "#BDB76B",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBBS %>% filter(nam=="SECTOR 16"),aes(x=LONGITUDE,y = LATITUDE),fill ="#FFFF99", colour= "#BDB76B",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBBS %>% filter(nam=="SECTOR 17"),aes(x=LONGITUDE,y = LATITUDE),fill ="#FFFF99", colour= "#BDB76B",alpha=0.5)+
  geom_text(data = PONT_TEXTO_BS,aes(x=X,y = Y,label= CHS), size=5,hjust=1)+
  geom_text(data = PONT_TEXTO_BS,aes(x=X,y = Y,label= SETOR), size=3,vjust=2.1)+
  theme_void()+
  labs(title = "Capacidade Horária de Setor FIR SBBS", caption = "Fonte: CGNA", x = NULL, y = NULL)+
  theme(plot.title = element_text(size = 18L,
                                  face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 12, color = "#8B0000"),
        plot.caption = element_text(hjust = .5,
                                    face = "bold"))
  
################################

#CAPACIDADE POR SETOR CW(29)


PONT_TEXTO_CW <- data.frame(Y=c(-31.56,-29.42,-27.58,-28.41,-25.37,-26.27,-24.51,-22.30,-22.22,-24.31,-26.10,-24.22,-23.15,-21.35,-19.08,-20.04,-22.34,-25.23,-24.40,-26.45),
                            X=c(-51.09,-54.21,-51.14,-48.19,-47.23,-45.50,-43.05,-40.46,-44.03,-47.35,-48.60,-48.40,-49.34,-52.06,-53.45,-56.41,-54.22,-52.49,-50.34,-50.09), 
                            SETOR=c("CW-S1","CW-S2","CW-S3","CW-S4","CW-S5","CW-S6","CW-S6F","CW-S7","CW-S8","CW-S9","CW-S9F","CW-S10","CW-S11","CW-S12","CW-S13","CW-S14","CW-S15","CW-S16","CW-S17","CW-S18"), 
                            CHS=c(44,44,48,49,48,44,"*",49,48,48,"*",48,44,44,44,49,49,49,44,48))



VERTICES_SETORES_FIR_SBCW <- VERTICES_SETORES_FIR_SET22 %>% filter(relatedfir=="SBCW")
ggplot()+
  geom_polygon(data=VERTICES_SETORES_FIR_SBCW %>% filter(nam=="SECTOR 1"),aes(x=LONGITUDE,y = LATITUDE),fill ="#007FFF", colour= "#4169E1",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBCW %>% filter(nam=="SECTOR 2"),aes(x=LONGITUDE,y = LATITUDE),fill ="#007FFF", colour= "#4169E1",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBCW %>% filter(nam=="SECTOR 03"),aes(x=LONGITUDE,y = LATITUDE),fill ="#007FFF", colour= "#4169E1",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBCW %>% filter(nam=="SECTOR 4"),aes(x=LONGITUDE,y = LATITUDE),fill ="#007FFF", colour= "#4169E1",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBCW %>% filter(nam=="SECTOR 05"),aes(x=LONGITUDE,y = LATITUDE),fill ="#007FFF", colour= "#4169E1",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBCW %>% filter(nam=="SECTOR 6"),aes(x=LONGITUDE,y = LATITUDE),fill ="#007FFF", colour= "#4169E1",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBCW %>% filter(nam=="SECTOR 6F"),aes(x=LONGITUDE,y = LATITUDE),fill ="#007FFF", colour= "#4169E1",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBCW %>% filter(nam=="SECTOR 7"),aes(x=LONGITUDE,y = LATITUDE),fill ="#007FFF", colour= "#4169E1",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBCW %>% filter(nam=="SECTOR 8"),aes(x=LONGITUDE,y = LATITUDE),fill ="#007FFF", colour= "#4169E1",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBCW %>% filter(nam=="SECTOR 9"),aes(x=LONGITUDE,y = LATITUDE),fill ="#007FFF", colour= "#4169E1",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBCW %>% filter(nam=="SECTOR 9F"),aes(x=LONGITUDE,y = LATITUDE),fill ="#007FFF", colour= "#4169E1",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBCW %>% filter(nam=="SECTOR 10"),aes(x=LONGITUDE,y = LATITUDE),fill ="#007FFF", colour= "#4169E1",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBCW %>% filter(nam=="SECTOR 11"),aes(x=LONGITUDE,y = LATITUDE),fill ="#007FFF", colour= "#4169E1",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBCW %>% filter(nam=="SECTOR 12"),aes(x=LONGITUDE,y = LATITUDE),fill ="#007FFF", colour= "#4169E1",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBCW %>% filter(nam=="SECTOR 13"),aes(x=LONGITUDE,y = LATITUDE),fill ="#007FFF", colour= "#4169E1",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBCW %>% filter(nam=="SECTOR 14"),aes(x=LONGITUDE,y = LATITUDE),fill ="#007FFF", colour= "#4169E1",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBCW %>% filter(nam=="SECTOR 15"),aes(x=LONGITUDE,y = LATITUDE),fill ="#007FFF", colour= "#4169E1",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBCW %>% filter(nam=="SECTOR 16"),aes(x=LONGITUDE,y = LATITUDE),fill ="#007FFF", colour= "#4169E1",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBCW %>% filter(nam=="SECTOR 17"),aes(x=LONGITUDE,y = LATITUDE),fill ="#007FFF", colour= "#4169E1",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBCW %>% filter(nam=="SECTOR 18"),aes(x=LONGITUDE,y = LATITUDE),fill ="#007FFF", colour= "#4169E1",alpha=0.5)+
  geom_text(data = PONT_TEXTO_CW,aes(x=X,y = Y,label= CHS), size=5,hjust=1)+
  geom_text(data = PONT_TEXTO_CW,aes(x=X,y = Y,label= SETOR), size=3,vjust=2.1)+
  theme_void()+
  labs(title = "Capacidade Horária de Setor FIR SBCW", caption = "Fonte: CGNA", x = NULL, y = NULL)+
  theme(plot.title = element_text(size = 18L,
                                  face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 12, color = "#8B0000"),
        plot.caption = element_text(hjust = .5,
                                    face = "bold"))

#################################################################################
#SETORES RE


PONT_TEXTO_RE <- data.frame(Y=c(-2.08,-4.38,-5.01,-7.04,-8.08,-9.07,-9.56,-11.29,-11.27,-13.29,-13.27,-15.29,-16.59,-18.29,-19.10),
                            X=c(-38.14,-34.07,-39.51,-36.06,-43.08,-39.59,-36.15,-37.46,-45.20,-42.44,-38.47,-41.01,-39.11,-41.22,-39.26), 
                            SETOR=c("RE-S1","RE-S2","RE-S3","RE-S4","RE-S5","RE-S6","RE-S7","RE-S8","RE-S9","RE-S10","RE-S11","RE-S12","RE-S13","RE-S14","RE-S15"), 
                            CHS=c(40,33,38,34,29,27,48,48,33,33,56,37,20,55,56))



VERTICES_SETORES_FIR_SBRE <- VERTICES_SETORES_FIR_SET22 %>% filter(relatedfir=="SBRE")
ggplot()+
  geom_polygon(data=VERTICES_SETORES_FIR_SBRE %>% filter(nam=="SECTOR 1"),aes(x=LONGITUDE,y = LATITUDE),fill ="#CC0000", colour= "#DC143C",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBRE %>% filter(nam=="SECTOR 2"),aes(x=LONGITUDE,y = LATITUDE),fill ="#CC0000", colour= "#DC143C",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBRE %>% filter(nam=="SECTOR 3"),aes(x=LONGITUDE,y = LATITUDE),fill ="#CC0000", colour= "#DC143C",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBRE %>% filter(nam=="SECTOR 4"),aes(x=LONGITUDE,y = LATITUDE),fill ="#CC0000", colour= "#DC143C",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBRE %>% filter(nam=="SECTOR 5"),aes(x=LONGITUDE,y = LATITUDE),fill ="#CC0000", colour= "#DC143C",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBRE %>% filter(nam=="SECTOR 6"),aes(x=LONGITUDE,y = LATITUDE),fill ="#CC0000", colour= "#DC143C",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBRE %>% filter(nam=="SECTOR 7"),aes(x=LONGITUDE,y = LATITUDE),fill ="#CC0000", colour= "#DC143C",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBRE %>% filter(nam=="SECTOR 8"),aes(x=LONGITUDE,y = LATITUDE),fill ="#CC0000", colour= "#DC143C",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBRE %>% filter(nam=="SECTOR 9"),aes(x=LONGITUDE,y = LATITUDE),fill ="#CC0000", colour= "#DC143C",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBRE %>% filter(nam=="SECTOR 10"),aes(x=LONGITUDE,y = LATITUDE),fill ="#CC0000", colour= "#DC143C",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBRE %>% filter(nam=="SECTOR 11"),aes(x=LONGITUDE,y = LATITUDE),fill ="#CC0000", colour= "#DC143C",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBRE %>% filter(nam=="SECTOR 12"),aes(x=LONGITUDE,y = LATITUDE),fill ="#CC0000", colour= "#DC143C",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBRE %>% filter(nam=="SECTOR 13"),aes(x=LONGITUDE,y = LATITUDE),fill ="#CC0000", colour= "#DC143C",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBRE %>% filter(nam=="SECTOR 14"),aes(x=LONGITUDE,y = LATITUDE),fill ="#CC0000", colour= "#DC143C",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBRE %>% filter(nam=="SECTOR 15"),aes(x=LONGITUDE,y = LATITUDE),fill ="#CC0000", colour= "#DC143C",alpha=0.5)+
  geom_text(data = PONT_TEXTO_RE,aes(x=X,y = Y,label= CHS), size=5,hjust=1)+
  geom_text(data = PONT_TEXTO_RE,aes(x=X,y = Y,label= SETOR), size=3,vjust=2.1)+
  theme_void()+
  labs(title = "Capacidade Horária de Setor FIR SBRE", caption = "Fonte: CGNA", x = NULL, y = NULL)+
  theme(plot.title = element_text(size = 18L,
                                  face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 12, color = "#8B0000"),
        plot.caption = element_text(hjust = .5,
                                    face = "bold"))

##########################################
#SETORES AZ (31)

PONT_TEXTO_AZ <- data.frame(Y=c(-6.59,-2.11,-0.25,-1.34,-7.35,-11.16,-7.33,-4.57,0.26,-2.12,-5.30,-8.42,-9.53,-12.30,-15.25),
                            X=c(-47.07,-45.58,-51.34,-55.08,-51.17,-55.05,-59.32,-56.54,-60.59,-65.29,-70.28,-67.36,-62.30,-59.20,-55.49), 
                            SETOR=c("AZ-S1","AZ-S2","AZ-S3","AZ-S4","AZ-S5","AZ-S6","AZ-S7","AZ-S8","AZ-S9","AZ-S10","AZ-S11","AZ-S12","AZ-S13","AZ-S14","AZ-S15"), 
                            CHS=c(29,49,27,26,26,23,33,31,22,19,26,27,25,27,46))



VERTICES_SETORES_FIR_SBAZ <- VERTICES_SETORES_FIR_SET22 %>% filter(relatedfir=="SBAZ")
ggplot()+
  geom_polygon(data=VERTICES_SETORES_FIR_SBAZ %>% filter(nam=="SECTOR 1"),aes(x=LONGITUDE,y = LATITUDE),fill ="#215E21", colour= "#2E8B57",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBAZ %>% filter(nam=="SECTOR 2"),aes(x=LONGITUDE,y = LATITUDE),fill ="#215E21", colour= "#2E8B57",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBAZ %>% filter(nam=="SECTOR 3"),aes(x=LONGITUDE,y = LATITUDE),fill ="#215E21", colour= "#2E8B57",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBAZ %>% filter(nam=="SECTOR 4"),aes(x=LONGITUDE,y = LATITUDE),fill ="#215E21", colour= "#2E8B57",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBAZ %>% filter(nam=="SECTOR 5"),aes(x=LONGITUDE,y = LATITUDE),fill ="#215E21", colour= "#2E8B57",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBAZ %>% filter(nam=="SECTOR 6"),aes(x=LONGITUDE,y = LATITUDE),fill ="#215E21", colour= "#2E8B57",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBAZ %>% filter(nam=="SECTOR 7"),aes(x=LONGITUDE,y = LATITUDE),fill ="#215E21", colour= "#2E8B57",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBAZ %>% filter(nam=="SECTOR 8"),aes(x=LONGITUDE,y = LATITUDE),fill ="#215E21", colour= "#2E8B57",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBAZ %>% filter(nam=="SECTOR 9"),aes(x=LONGITUDE,y = LATITUDE),fill ="#215E21", colour= "#2E8B57",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBAZ %>% filter(nam=="SECTOR 10"),aes(x=LONGITUDE,y = LATITUDE),fill ="#215E21", colour= "#2E8B57",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBAZ %>% filter(nam=="SECTOR 11"),aes(x=LONGITUDE,y = LATITUDE),fill ="#215E21", colour= "#2E8B57",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBAZ %>% filter(nam=="SECTOR 12"),aes(x=LONGITUDE,y = LATITUDE),fill ="#215E21", colour= "#2E8B57",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBAZ %>% filter(nam=="SECTOR 13"),aes(x=LONGITUDE,y = LATITUDE),fill ="#215E21", colour= "#2E8B57",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBAZ %>% filter(nam=="SECTOR 14"),aes(x=LONGITUDE,y = LATITUDE),fill ="#215E21", colour= "#2E8B57",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBAZ %>% filter(nam=="SECTOR 15"),aes(x=LONGITUDE,y = LATITUDE),fill ="#215E21", colour= "#2E8B57",alpha=0.5)+
  geom_text(data = PONT_TEXTO_AZ,aes(x=X,y = Y,label= CHS), size=5,hjust=1)+
  geom_text(data = PONT_TEXTO_AZ,aes(x=X,y = Y,label= SETOR), size=3,vjust=2.1)+
  theme_void()+
  labs(title = "Capacidade Horária de Setor FIR SBAZ", caption = "Fonte: CGNA", x = NULL, y = NULL)+
  theme(plot.title = element_text(size = 18L,
                                  face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 12, color = "#8B0000"),
        plot.caption = element_text(hjust = .5,
                                    face = "bold"))



####################################################

# SETORES AO (32)



PONT_TEXTO_AO <- data.frame(Y=c(2.24,4.50,2.31,0.42,-0.04,-3.56,-12.51,-26.00,-10.27),
                            X=c(-44.14,-37.37,-35.05,-34.09,-31.01,-28.59,-34.53,-27.14,-19.05), 
                            SETOR=c("AO-S1A","AO-S1B","AO-S1C","AO-S2A","AO-S2B","AO-S2C","AO-S3A","AO-S3B","AO-S3C"), 
                            CHS=c(13,"*","*","*", 18,"*",13,"*","*"))



VERTICES_SETORES_FIR_SBAO <- VERTICES_SETORES_FIR_SET22 %>% filter(relatedfir=="SBAO")
ggplot()+
  geom_polygon(data=VERTICES_SETORES_FIR_SBAO %>% filter(nam=="SECTOR 1A"),aes(x=LONGITUDE,y = LATITUDE),fill ="#DCDCDC", colour= "#A9A9A9",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBAO %>% filter(nam=="SECTOR 1B"),aes(x=LONGITUDE,y = LATITUDE),fill ="#DCDCDC", colour= "#A9A9A9",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBAO %>% filter(nam=="SECTOR 1C"),aes(x=LONGITUDE,y = LATITUDE),fill ="#DCDCDC", colour= "#A9A9A9",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBAO %>% filter(nam=="SECTOR 2A"),aes(x=LONGITUDE,y = LATITUDE),fill ="#DCDCDC", colour= "#A9A9A9",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBAO %>% filter(nam=="SECTOR 2B"),aes(x=LONGITUDE,y = LATITUDE),fill ="#DCDCDC", colour= "#A9A9A9",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBAO %>% filter(nam=="SECTOR 2C"),aes(x=LONGITUDE,y = LATITUDE),fill ="#DCDCDC", colour= "#A9A9A9",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBAO %>% filter(nam=="SECTOR 3A"),aes(x=LONGITUDE,y = LATITUDE),fill ="#DCDCDC", colour= "#A9A9A9",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBAO %>% filter(nam=="SECTOR 3B"),aes(x=LONGITUDE,y = LATITUDE),fill ="#DCDCDC", colour= "#A9A9A9",alpha=0.5)+
  geom_polygon(data=VERTICES_SETORES_FIR_SBAO %>% filter(nam=="SECTOR 3C"),aes(x=LONGITUDE,y = LATITUDE),fill ="#DCDCDC", colour= "#A9A9A9",alpha=0.5)+
  geom_text(data = PONT_TEXTO_AO,aes(x=X,y = Y,label= CHS), size=5,hjust=1)+
  geom_text(data = PONT_TEXTO_AO,aes(x=X,y = Y,label= SETOR), size=3,vjust=2.1)+
  theme_void()+
  labs(title = "Capacidade Horária de Setor FIR SBAO", caption = "Fonte: CGNA", x = NULL, y = NULL)+
  theme(plot.title = element_text(size = 18,
                                  face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 12, color = "#8B0000"),
        plot.caption = element_text(hjust = .5,
                                    face = "bold"))

