library(tidyverse) #General everything
library(RColorBrewer) #plot colors
library(ggplot2) #General plotting
library(ggrepel) #For adding text labels that repel away from data points
library(calecopal) #Color palette
library(shiny) #Runs shiny
library(shinythemes) #Shiny theme for the page
library(shinyWidgets) #Widgets
library(scales) #SSD - Use the percent format
library(reshape2) #Overview tab - melts bars together
library(ssdtools) #SSD package
library(DT) #Build HTML data tables
library(plotly) #Make plots interactive
library(viridis) #Colors
library(scales) #To use "percent" function
library(shinyjs)
library(tigerstats)#Exploration tab - reset bu

aoc <- read_csv("Humans_Clean_Final.csv", guess_max = 10000)
replace_na(list(size.category = 0, shape = "Not Reported", polymer = "Not Reported", exposure.route = "Not Applicable", life.stage = "Not Reported"))
polydf<-rowPerc(xtabs( ~polymer +effect, aoc)) #pulls polymers by effect 
polyf<-as.data.frame(polydf)%>% #Makes data frame 
  filter(effect %in% c("Y","N"))%>% #Sorts into Yes and No
  rename(Type = "polymer")%>%#rename so future columns have same name 
  mutate(Type = case_when(
    Type == "PA" ~ "Polyamide",
    Type == "PE" ~ "Polyethylene",
    Type == "PMMA" ~ "Polymethylmethacrylate",
    Type == "PP" ~ "Polypropylene",
    Type == "PS" ~ "Polystyrene",
    Type == "PUR" ~ "Polyurathane",
    Type == "PVC" ~ "Polyvinylchloride",
    Type == "TR" ~ "Tire Rubber"))%>%
  mutate_if(is.numeric, round,0)%>% #rounds percents 
  mutate(plot="Polymer") # change column name for check list
Endpoints<-xtabs(~polymer +effect ,aoc) #Pulls all study obs. for polymer from dataset
polyfinal<- data.frame(cbind(polyf, Endpoints))%>% #adds it as a column
  rename(Endpoints='Freq.1')%>% #renames column
  rename(category='polymer')#renames column

polyfinal

polydf

sizedf<-rowPerc(xtabs(~size.category +effect, aoc))
sizef<-as.data.frame(sizedf)%>%
  filter(effect %in% c("Y","N"))%>%
  mutate(size.category = case_when(
    size.category == 1 ~ "1nm < 100nm",
    size.category == 2 ~ "100nm < 1µm",
    size.category == 3 ~ "1µm < 100µm",
    size.category == 4 ~ "100µm < 1mm",
    size.category == 0 ~ "Not Reported"))%>%
  rename(Type = "size.category")%>%
  mutate_if(is.numeric, round,0)%>%
  mutate(plot="Size")
study_s<-xtabs(~size.category +effect ,aoc)
sizefinal<- data.frame(cbind(sizef, study_s))%>% 
  rename(Endpoints='Freq.1')%>%
  rename(category='size.category')

sizefinal

shapedf<-rowPerc(xtabs(~shape + effect, aoc))
shapef<-as.data.frame(shapedf)%>%
  filter(effect %in% c("Y","N"))%>%
  rename(Type="shape")%>%
  mutate_if(is.numeric, round,0)%>%
  mutate(plot="Shape")%>%
  mutate(Type = case_when(
    Type == "fragment" ~ "Fragment",
    Type == "sphere" ~ "Sphere",
    Type == "NA" ~ "Not Reported"))
study_sh<-xtabs(~shape + effect,aoc)
shapefinal<- data.frame(cbind(shapef, study_sh))%>% 
  rename(Endpoints='Freq.1')%>%
  rename(category='shape')
shapefinal


lvl1df<-rowPerc(xtabs(~lvl1 +effect, aoc))
lvl1f<-as.data.frame(lvl1df)%>%
  filter(effect %in% c("Y","N"))%>%
  rename(Type= "lvl1")%>%
  mutate_if(is.numeric, round,0)%>%
  mutate(plot="Lvl1")%>%
  mutate(Type = case_when(
    Type == "alimentary.excretory" ~ "Alimentary, Excretory",
    Type == "behavior.sense.neuro" ~ "Behavioral, Sensory, Neurological",
    Type == "cell.growth.proliferation" ~ "Cell Growth and Proliferation",
    Type == "cell.morphology.structure" ~ "Cell Morphology and Structure",
    Type == "circulatory" ~ "Circulatory",
    Type == "cytotoxicity" ~ "Cytotoxicity",
    Type == "endocrine.signaling" ~ "Endocrine Signaling",
    Type == "fitness" ~ "Fitness",
    Type == "immune" ~ "Immune",
    Type == "metabolism" ~ "Metabolism",
    Type == "microbiome" ~ "Microbiome",
    Type == "respiratory" ~ "Respiratory",
    Type == "stress" ~ "Stress"))
study_l<-xtabs(~lvl1 +effect,aoc)
lvl1final<- data.frame(cbind(lvl1f, study_l))%>% 
  rename(Endpoints='Freq.1')%>%
  rename(category='lvl1')

lvl1final

lifedf<-rowPerc(xtabs(~life.stage +effect, aoc))
lifef<-as.data.frame(lifedf)%>%
  filter(effect %in% c("Y","N"))%>%
  rename(Type= "life.stage")%>%
  mutate_if(is.numeric, round,0)%>%
  mutate(plot="Life.stage")%>%
  mutate(Type = case_when(
    Type == "early,f1"~"Early, F1 Generation",
    Type == "early,f2"~"Early, F2 Generation",
    Type == "juvenile"~"Juvenile",
    Type == "adult"~"Adult",
    Type == "Not Reported"~"Not Reported"))
studyli<-xtabs(~life.stage +effect ,aoc)
lifefinal<- data.frame(cbind(lifef, studyli))%>% 
  rename(Endpoints='Freq.1')%>%
  rename(category='life.stage')

studyli
lifefinal
lifedf

vivodf<-rowPerc(xtabs(~invitro.invivo +effect, aoc))
vivof<-as.data.frame(vivodf)%>%
  filter(effect %in% c("Y","N"))%>%
  rename(Type= "invitro.invivo")%>%
  mutate_if(is.numeric, round,0)%>%
  mutate(plot="Invivo.invivo")%>%
  mutate(Type = case_when(
    Type=="invivo"~"In Vivo",
    Type=="invitro"~"In Vitro"))
study_v<-xtabs(~invitro.invivo +effect,aoc)
vivofinal<- data.frame(cbind(vivof, study_v))%>% 
  rename(Endpoints='Freq.1')%>%
  rename(category='invitro.invivo')
vivofinal


routedf<-rowPerc(xtabs(~exposure.route +effect, aoc))
routef<-as.data.frame(routedf)%>%
  filter(effect %in% c("Y","N"))%>%
  rename(Type= "exposure.route")%>%
  mutate_if(is.numeric, round,0)%>%
  mutate(plot="Exposure.route")%>%
  mutate(Type = case_when(
    Type == "dermal" ~ "Dermal",
    Type == "food" ~ "Food",
    Type == "gavage" ~ "Gavage",
    Type == "gestation" ~ "Gestation",
    Type == "gestation,lactation" ~ "Gestation & Lactation",
    Type == "inhalation" ~ "Inhalation",
    Type == "intratracheal.instillation" ~ "Intratracheal Instillation",
    Type == "iv.injection" ~ "IV Injection",
    Type == "drinking.water" ~ "Drinking Water",
    Type ==  "Not Applicable"~"Not Applicable"))
study_r<-xtabs(~exposure.route +effect,aoc)
routefinal<- data.frame(cbind(routef, study_r))%>% 
  rename(Endpoints='Freq.1')%>%
  rename(category='exposure.route')
lifefinal
routefinal
polyfinal
sizefinal

A<-rbind(polyfinal,sizefinal)
B<-rbind(A,shapefinal)
C<-rbind(B,lvl1final)
D<-rbind(C,lifefinal)
G<-rbind(D,vivofinal)
Final_effect_dataset<-rbind(G,routefinal)

Final_effect_datasetH<-Final_effect_dataset%>%
  mutate(plot_f=factor(plot))

Final_effect_datasetH

write.csv(Final_effect_datasetH, "Final_effect_datasetH.csv")
