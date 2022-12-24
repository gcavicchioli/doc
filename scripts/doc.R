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
bd_gravadores<- dplyr::filter(bd, !NID %in% "1"  & !Id_so_genero %in% "1" & Técnica=="Gravadores") # & !Checklist %in% "1") # & H == "F") # # & 
names(bd_gravadores)
tb_gravadores <-bd_gravadores  

######## Cobertura amostral e Números de Hill##########

#data(ChaoSpeciesData)
#ChaoSpeciesData$Inci_count
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