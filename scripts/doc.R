#Análises prliminares do projeto de Doutorado  PPGI-EA
#O USO DE GRAVADORES AUTÔNOMOS PARA MEDIR A DIVERSIDADE DE AVES EM PAISAGENS AGRÍCOLAS
#Guilherme Cavicchioli da Silva e colaboradores - outubro de 2022.

#####Carregando pacotes########## 
library(remotes)
#install.packages("devtools")
library(dplyr)
library(reshape)
library(vegan)
library(xlsx)
library(ggplot2)
library(openxlsx)
library(here)
library(tidyverse)
library(iNEXT)
library(viridis)
library(SpadeR)
library(iNEXT.3D)

#######importando dados####
#install_github("AnneChao/iNEXT.3D")
bd <- openxlsx::read.xlsx(here::here("data","bd_pontos.xlsx"),sheet = 1)
bd[,c("value")] <- as.integer(bd[,"value"])
bd [is.na(bd)]<-0

#Filtrando NIDs,não-válidos, pontos feitos a mais, etc
bd_gravadores<- dplyr::filter(bd, !NID %in% "1"  & !Id_so_genero %in% "1" & campanha=="angatuba_1") # & !Checklist %in% "1") # & H == "F") # # & 
names(bd_gravadores)
tb_gravadores <-bd_gravadores  

######## Cobertura amostral e Números de Hill##########

#data(ChaoSpeciesData)
#ChaoSpeciesData$Inci_count




##Total Angatuba
total_ang <- openxlsx::read.xlsx(here::here("data","total_pontos.xlsx"),sheet = 1, rowNames = T)
total_ang [is.na(total_ang)]<-0
row.names(total_ang) <- total_ang[,1]
total_ang <- as.data.frame(total_ang[,-1])
chao_total_ang<-SpadeR::ChaoSpecies(total_ang, datatype = "incidence_freq", k = 10, conf = 0.95)


td_total_ang<-iNEXT3D(
  total_ang,
  diversity = "TD",
  q = c(0, 1, 2),
  datatype = "incidence_freq",
  size = NULL,
  endpoint = NULL,
  knots = 40,
  nboot = 50,
  conf = 0.95)

plotD_total_ang<-SpadeR::Diversity(total_ang, datatype = "incidence_freq")

plotTD_total_ang_inext3d <-ggiNEXT3D(td_total_ang, type=3)



##Ponto 1
ponto1 <- openxlsx::read.xlsx(here::here("data","ponto1.xlsx"),sheet = 1)
row.names(ponto1) <- ponto1[,1]
ponto1 <- as.data.frame(ponto1[,-1])
chao_p1<-SpadeR::ChaoSpecies(ponto1, datatype = "incidence_freq", k = 10, conf = 0.95)


td_ponto1<-iNEXT3D(
  ponto1,
  diversity = "TD",
  q = c(0, 1, 2),
  datatype = "incidence_freq",
  size = NULL,
  endpoint = NULL,
  knots = 40,
  nboot = 50,
  conf = 0.95)

plotD_ponto1<-SpadeR::Diversity(ponto1, datatype = "incidence_freq")

plotTD_ponto1_inext3d <-ggiNEXT3D(td_ponto1, type=1)

###ponto 18
ponto18 <- openxlsx::read.xlsx(here::here("data","ponto18.xlsx"),sheet = 1)
row.names(ponto18) <- ponto18[,1]
ponto18 <- as.data.frame(ponto18[,-1])
chao_p18<-SpadeR::ChaoSpecies(ponto18, datatype = "incidence_freq", k = 10, conf = 0.95)


td_ponto18<-iNEXT3D(
  ponto18,
  diversity = "TD",
  q = c(0, 1, 2),
  datatype = "incidence_freq",
  size = NULL,
  endpoint = NULL,
  knots = 40,
  nboot = 50,
  conf = 0.95)
plotD_ponto18<-SpadeR::Diversity(ponto18, datatype = "incidence_freq")
plotTD_ponto18_inext3d <-ggiNEXT3D(td_ponto18, type=c(1,3))


#####Ponto 22

ponto22 <- openxlsx::read.xlsx(here::here("data","ponto22.xlsx"),sheet = 1)
row.names(ponto22) <- ponto22[,1]
ponto22 <- as.data.frame(ponto22[,-1])
chao_p22<-SpadeR::ChaoSpecies(ponto22, datatype = "incidence_freq", k = 10, conf = 0.95)


td_ponto22<-iNEXT3D(
  ponto22,
  diversity = "TD",
  q = c(0, 1, 2),
  datatype = "incidence_freq",
  size = NULL,
  endpoint = NULL,
  knots = 40,
  nboot = 50,
  conf = 0.95)
plotD_ponto22<-SpadeR::Diversity(ponto22, datatype = "incidence_freq")

plotTD_ponto22_inext3d <-ggiNEXT3D(td_ponto22, type= c(1,3))

####Ponto 31

ponto31 <- openxlsx::read.xlsx(here::here("data","ponto31.xlsx"),sheet = 1)
row.names(ponto31) <- ponto31[,1]
ponto31 <- as.data.frame(ponto31[,-1])
chao_p1<-SpadeR::ChaoSpecies(ponto31, datatype = "incidence_freq", k = 10, conf = 0.95)


td_ponto31<-iNEXT3D(
  ponto31,
  diversity = "TD",
  q = c(0, 1, 2),
  datatype = "incidence_freq",
  size = NULL,
  endpoint = NULL,
  knots = 40,
  nboot = 50,
  conf = 0.95)

plotD_ponto31<-SpadeR::Diversity(ponto31, datatype = "incidence_freq")

plotTD_ponto31_inext3d <-ggiNEXT3D(td_ponto31, type=1)






#####################################################################

#Análises da comunidade
pontos <- cast(bd_gravadores, Specie ~ ponto, value = "value", sum, margins = "grand_col")
#write.csv2(AMs_tot, "riq_uniq.csv")
#Riq_flor <- cast(filter(bd_cafezal, !familia %in% "1" & origem %in% "Ambiens" & !Checklist %in% "1" & !Tratamento %in% "NA" & H == "F"), Species ~ Tratamento, value = "value", sum, margins = "grand_col")
#criando data frame para o graf
Riq <- data.frame(rbind(specnumber(t(pontos))))#,specnumber(t(Riq_flor))
#rownames(Riq) <- c("Riqueza total", "Pontos de Escuta", "Redes Ornitológicas")#, "Riqueza florestais"
colnames(Riq) <- c("P1", "P18", "P22", "P31", "P33","P34","Total")
#Plotando
par(mfrow=c(1,1))
par(mar = c(2,4,0,1))
z = barplot(as.matrix(Riq), ylab="Número de espécies", xlab= c("P1", "P18", "P22", "P31","P33","P34","Total"), col=c("#FDE725FF","#E6E2C3", "#7AD151FF","#227C70","#2A788EFF","#205295", "#0A2647" ), beside=TRUE,  legend.text = TRUE, args.legend = list(bty = "n",cex=0.8),ylim = c(0,max(Riq)*1.3))

#ylim = c(0,max(AMs_riq)*1.4)

text(z,as.matrix(Riq),labels = as.matrix(Riq), pos = 3, cex = 0.8)


# NMDS-----
pontos_MDS <- cast(bd_gravadores, Specie ~ amostra2, value = "value", sum)#chama os dados
pontos_MDS [is.na(pontos_MDS)]<-0
pontos_MDS <- t(pontos_MDS) #t#chama os dados
pontos_MDS[is.na(pontos_MDS)]<-0

#Só florestais
pontos_MDS_F <- cast(filter(bd_2021, !familia %in% "1" & !Checklist %in% "1"  & H == "F"), Species ~ Gleba, value = "value", sum)#chama os dados
pontos_MDS_F [is.na(pontos_MDS_F)]<-0
pontos_MDS_F <- t(pontos_MDS_F) #t#chama os dados
pontos_MDS_F[is.na(pontos_MDS_F)]<-0

library(vegan) #carrega o pacote vegan
NMDS = metaMDS(pontos_MDS,k=2,trymax=100) # The number of reduced dimensions
NMDS

NMDS_F = metaMDS(pontos_MDS_F,k=2,trymax=100) # The number of reduced dimensions
NMDS_F

#library(xlsx)
#write.xlsx(NMDS_scores_todas,"NMDS_scores_todas)2020-1.xlsx")
#stressplot(NMDS)
par(mfrow = c(1,1))
par(mar=c(4,4,1,1))
ordiplot(NMDS, type="n",xlim=c(-1,1),ylim=c(-0.9,0.9), cex.main = 0.8, xlab = "")
orditorp(NMDS,display="sites", col=c(rep("#FDE725FF",15),rep("#E6E2C3",15),rep("#7AD151FF",15),rep("#227C70",16),rep("#2A788EFF",15),rep("#205295",7)),
         air=0.01,cex=0.8,xlim=c(-1,1))
treat=c(rep("P1",15),rep("P18",15),rep("P22",15),rep("P31",16),rep("P33",15),rep("P34",15),rep("P34",7))
ordihull(NMDS,groups=treat,draw="polygon",col=c("#FDE725FF","#E6E2C3", "#7AD151FF","#227C70","#2A788EFF","#205295"),label=F)





##Test richness por amostra

Riq_amostra <- cast(tb_gravadores, Specie ~ amostra2, value = "value", sum)#Pré supressão

Riq_amostra <- specnumber(t(Riq_amostra))
#plotando
par(mfrow = c(1,1))
par(mar = c(0,0,1.5,2), oma = (c(6,4.6,0,0)))
plot(Riq_amostra , ylim = c(0,max(Riq_amostra)*1.3), xlab = "", ylab = "Número de espécies", pch = 21, bg = "black", main = "Todas as espécies", cex.main = 1, col = diamond_color_colors[diamonds$color])
lines(predict(lm(Riq_amostra ~ seq(1:66))) ~ seq(1:66), col = "black")


color_pallet_function <- colorRampPalette(
  colors = c("red", "orange", "blue"),
  space = "Lab"
)
numb_colors <- 4
point_colors <- color_pallet_function(numb_colors)
##ggplot
riq_amostra_pivot <- tidyr::pivot_longer(data = tb_gravadores [ , c(1,3,23)],
                                         cols =  'ponto','amostra2',
                                         names_to = "Espécie",
                                         values_to = "N") 
# contagens de valores para mais de uma coluna
riq_amostra_count <- riq_amostra_pivot %>% 
  dplyr::count(amostra2, N)

ggplot(riq_amostra_count,aes(x=amostra2,y=n,color=as.factor(N))) +
  geom_point(size = 3)  




# Prepara a tabela  
ponto18 <- cast(filter(bd_2021, !fam %in% "1",ponto =="18"), Specie ~ ponto, value = "value", sum)
row.names(ponto1) <- ponto18[,1]
ponto18 <- as.data.frame(ponto18[,-1])

write.xlsx(as.data.frame(check),here::here("data","ponto1.xlsx"), row.names = F)

out_sc_ind <- iNEXT(check, q=c(0,1,2), datatype="incidence")
plotTD_ponto1 <-ggiNEXT3D(td_ponto1, type=1)
plotD_ponto1 <- plotD_ponto1 + labs(x="Ordem q",y="Diversidade (Números de Hill)",label = T) + 
  #facet_wrap(.~order,labeller = labeller(order = labels))+
  scale_color_manual(values = c("dark green","deepskyblue4", "gray"))+
  scale_fill_manual(values = c("dark green","deepskyblue4", "gray"))+  
  #guides(linetype = FALSE,shape = FALSE) +
  #scale_shape_manual(values=c(19,19,19)) +
  theme_light() + theme(panel.grid = element_blank(),
                        #legend.position = c(0.7,.12),
                        legend.title = element_blank(),
                        legend.direction = "vertical") +
  geom_label( label = out$DataInfo,  nudge_x , nudge_y,   check_overlap,  label.padding,  label.size,  color,  fill )
theme_bw(base_size = 15)

out_sc_ind$AsyEst