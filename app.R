#### Aquatic Microplastics Toxicology Shiny App
#### File created: September 23, 2020
#### Code contributors: Heili Lowman, Leah Thornton Hampton, Scott Coffin, Emily Darin

#### Setup ####

# Anything that should only happen ONCE should be placed in this setup section, prior to the actual shiny structure.

# Load packages
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
library(shinyjs) #Exploration tab - reset button
library(tigerstats) #row percent values 
library(ggbeeswarm) #plot all points nicely

# Load finalized dataset.

human <- read_csv("Humans_Clean_Final.csv", guess_max = 10000)

#### Introduction Setup ####

# All text inputs below.

#### Overview Human Setup ####

#Set up for polymer overview plot
replace_na(list(size.category = 0, shape = "Not Reported", polymer = "Not Reported", exposure.route = "Not Applicable", life.stage = "Not Reported"))
polydf<-rowPerc(xtabs( ~polymer +effect, human)) #pulls polymers by effect 
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
Endpoints<-xtabs(~polymer +effect ,human) #Pulls all study obs. for polymer from dataset
polyfinal<- data.frame(cbind(polyf, Endpoints))%>% #adds it as a column
  rename(Endpoints='Freq.1')%>% #renames column
  rename(category='polymer')%>%
  mutate(logEndpoints = log(Endpoints))%>%
  rename(Percent = Freq)#renames column#renames column

#Set up for size overview plot
sizedf<-rowPerc(xtabs(~size.category +effect, human))
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
study_s<-xtabs(~size.category +effect ,human)
sizefinal<- data.frame(cbind(sizef, study_s))%>% 
  rename(Endpoints='Freq.1')%>%
  rename(category='size.category')%>%
  mutate(logEndpoints = log(Endpoints))%>%
  rename(Percent = Freq)#renames column

#Set up for shape overview plot
shapedf<-rowPerc(xtabs(~shape + effect, human))
shapef<-as.data.frame(shapedf)%>%
  filter(effect %in% c("Y","N"))%>%
  rename(Type="shape")%>%
  mutate_if(is.numeric, round,0)%>%
  mutate(plot="Shape")%>%
  mutate(Type = case_when(
    Type == "fragment" ~ "Fragment",
    Type == "sphere" ~ "Sphere",
    Type == "NA" ~ "Not Reported"))
study_sh<-xtabs(~shape + effect,human)
shapefinal<- data.frame(cbind(shapef, study_sh))%>% 
  rename(Endpoints='Freq.1')%>%
  rename(category='shape')%>%
  mutate(logEndpoints = log(Endpoints))%>%
  rename(Percent = Freq)#renames column

#Set up for lvl1 overview plot
lvl1df<-rowPerc(xtabs(~lvl1 +effect, human))
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
study_l<-xtabs(~lvl1 +effect, human)
lvl1final<- data.frame(cbind(lvl1f, study_l))%>% 
  rename(Endpoints='Freq.1')%>%
  rename(category='lvl1')%>%
  mutate(logEndpoints = log(Endpoints))%>%
  rename(Percent = Freq)#renames column

#Set up for life stage overview plot
lifedf<-rowPerc(xtabs(~life.stage +effect, human))
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
studyli<-xtabs(~life.stage +effect ,human)
lifefinal<- data.frame(cbind(lifef, studyli))%>% 
  rename(Endpoints='Freq.1')%>%
  rename(category='life.stage')%>%
  mutate(logEndpoints = log(Endpoints))%>%
  rename(Percent = Freq)#renames column

#Set up for in vitro in vivo overview plot
vivodf<-rowPerc(xtabs(~invitro.invivo +effect, human))
vivof<-as.data.frame(vivodf)%>%
  filter(effect %in% c("Y","N"))%>%
  rename(Type= "invitro.invivo")%>%
  mutate_if(is.numeric, round,0)%>%
  mutate(plot="Invivo.invivo")%>%
  mutate(Type = case_when(
    Type=="invivo"~"In Vivo",
    Type=="invitro"~"In Vitro"))
study_v<-xtabs(~invitro.invivo +effect, human)
vivofinal<- data.frame(cbind(vivof, study_v))%>% 
  rename(Endpoints='Freq.1')%>%
  rename(category='invitro.invivo')%>%
  mutate(logEndpoints = log(Endpoints))%>%
  rename(Percent = Freq)#renames column

#Test Set up for plot type widget

#in vitro/in vivo by year and measurement
vivodf_year_measurement<-rowPerc(xtabs(~invitro.invivo +year, human)) %>%
  as.data.frame()%>%
  filter(year!="Total") %>% #supress Total column to be able to cbind later
  rename(Type= "invitro.invivo")%>%
  mutate_if(is.numeric, round,0)%>%
  mutate(plot="Invivo.invivo")%>%
  mutate(Type = case_when(
    Type=="invivo"~"In Vivo",
    Type=="invitro"~"In Vitro"))
study_v_year<-as.data.frame(xtabs(~invitro.invivo +year, human))
vivoFinal_year<- data.frame(cbind(vivodf_year_measurement, study_v_year))%>%
  rename(Endpoints='Freq.1')%>%
  rename(category='invitro.invivo')%>%
  mutate(logEndpoints = log(Endpoints))%>%
  rename(Percent = Freq)#renames column

#in vitro/in vivo by year and study
vivoFinal_year_study<-human %>%
  group_by(invitro.invivo, year) %>%
  summarize(studyCount = n_distinct(doi)) %>%
  mutate(freq = 100 * studyCount / sum(studyCount)) %>%
  as.data.frame()%>%
  rename(Type= "invitro.invivo")%>%
  mutate_if(is.numeric, round,0)%>%
  mutate(plot="Invivo.invivo")%>%
  mutate(Type = case_when(
    Type=="invivo"~"In Vivo",
    Type=="invitro"~"In Vitro")) %>%
  rename(Studies='studyCount')%>%
  mutate(logStudies = log(Studies))%>%
  rename(Percent = freq)#renames column

#Set up for exposure route overview plot
routedf<-rowPerc(xtabs(~exposure.category +effect, human))
routef<-as.data.frame(routedf)%>%
  filter(effect %in% c("Y","N"))%>%
  rename(Type= "exposure.category")%>%
  mutate_if(is.numeric, round,0)%>%
  mutate(plot="Exposure.category")%>%
  mutate(Type = case_when(
    Type == "Dermal" ~ "Dermal",
    Type == "Ingestion" ~ "Ingestion",
    Type == "Inhalation" ~ "Inhalation",
    Type == "IV Injection" ~ "IV Injection",
    Type == "In Vitro" ~ "In Vitro"))
study_r<-xtabs(~exposure.category +effect,human)
routefinal<- data.frame(cbind(routef, study_r))%>% 
  rename(Endpoints='Freq.1')%>%
  rename(category='exposure.category')%>%
  mutate(logEndpoints = log(Endpoints))%>%
  rename(Percent = Freq)#renames column

# Set default theme for overview plots
overviewTheme <- function(){
  theme_classic() %+replace%
    theme(text = element_text(size=17), plot.title = element_text(hjust = 0.5, face="bold",size=20),legend.position = "right",
          axis.ticks= element_blank(),
          axis.text.x = element_text(),
          axis.text.y = element_blank(),
          axis.title.x = element_blank() ) }

#### Exploration Human Setup ####

human_v1 <- human %>% # start with original data set
  #full dataset filters.
  mutate(effect_h_f = factor(case_when(effect == "Y" ~ "Yes",
                                       effect == "N" ~ "No"),
                             levels = c("No", "Yes"))) %>% #Note: this drops effect metric data which is designated as 'NA' for effect
  
  # removing NAs to make data set nicer
  replace_na(list(size.category = 0, shape = "Not Reported", polymer = "Not Reported", exposure.route = "Not Applicable", life.stage = "Not Reported")) 

human_setup <- human_v1 %>% # start with original data set
  mutate(size_h_f = factor(case_when(
    size.category == 1 ~ "1nm < 100nm",
    size.category == 2 ~ "100nm < 1µm",
    size.category == 3 ~ "1µm < 100µm",
    size.category == 4 ~ "100µm < 1mm",
    size.category == 0 ~ "Not Reported"), 
    levels = c("1nm < 100nm", "100nm < 1µm", "1µm < 100µm", "100µm < 1mm", "Not Reported"))) %>% #Renames for widget
  mutate(shape_h_f = factor(case_when(
    shape == "fragment" ~ "Fragment",
    shape == "sphere" ~ "Sphere",
    shape == "Not Reported" ~ "Not Reported"),
    levels = c("Fragment", "Sphere", "Not Reported"))) %>% #Renames for widget
  mutate(poly_h_f = factor(case_when(
    polymer == "PA" ~ "Polyamide",
    polymer == "PE" ~ "Polyethylene",
    polymer == "PMMA" ~ "Polymethylmethacrylate",
    polymer == "PP" ~ "Polypropylene",
    polymer == "PS" ~ "Polystyrene",
    polymer == "PUR" ~ "Polyurathane",
    polymer == "PVC" ~ "Polyvinylchloride",
    polymer == "TR" ~ "Tire Rubber",
    polymer == "Not Reported" ~ "Not Reported"))) %>% #Renames for widget
  mutate(lvl1_h_f = factor(case_when(lvl1 == "alimentary.excretory" ~ "Alimentary, Excretory",
                                     lvl1 == "behavior.sense.neuro" ~ "Behavioral, Sensory, Neurological",
                                     lvl1 == "cell.growth.proliferation" ~ "Cell Growth and Proliferation",
                                     lvl1 == "cell.morphology.structure" ~ "Cell Morphology and Structure",
                                     lvl1 == "circulatory" ~ "Circulatory",
                                     lvl1 == "cytotoxicity" ~ "Cytotoxicity",
                                     lvl1 == "endocrine.signaling" ~ "Endocrine Signaling",
                                     lvl1 == "fitness" ~ "Fitness",
                                     lvl1 == "immune" ~ "Immune",
                                     lvl1 == "metabolism" ~ "Metabolism",
                                     lvl1 == "microbiome" ~ "Microbiome",
                                     lvl1 == "respiratory" ~ "Respiratory",
                                     lvl1 == "stress" ~ "Stress"))) %>% #Renames for widget
  # Level 2 Data tidying
  mutate(lvl2_h_f = factor(case_when(lvl2 == "actinobacteria" ~ "Actinobacteria",
                                     lvl2 == "amino.acid.metabolism" ~ "Amino Acid Metabolism",
                                     lvl2 == "apoptosis.cell.cycle"~"Apoptosis and Cell Cycle",
                                     lvl2 == "bacteroidetes"~ "Bacteriodetes",
                                     lvl2 == "bile.acid" ~ "Bile Acid",
                                     lvl2 == "blood.gen" ~ "Blood",
                                     lvl2 == "body.condition"~"Body Condition",
                                     lvl2 == "brain.histo" ~ "Brain Histological Abnormalities",
                                     lvl2 == "carb.metabolism"~"Carb Metabolism",
                                     lvl2 == "cell.aggregation"~"Cell Aggregation",
                                     lvl2 == "cell.membrane"~"Cell Membrane",
                                     lvl2 == "circulatory"~"Circulatory",
                                     lvl2 == "complement"~"Complement",
                                     lvl2 == "coordination"~"Coordination",
                                     lvl2 == "cytotoxicity"~"Cytotoxicity",
                                     lvl2 == "development" ~ "Development",
                                     lvl2 == "digestive.tract.histo"~"Digestive Tract Histological Abnormalities",
                                     lvl2 == "diversity"~ "Diversity",
                                     lvl2 == "dna.damage" ~ "DNA Damage",
                                     lvl2 == "energy.metabolism" ~ "Energy Metabolism",
                                     lvl2 == "exploration" ~ "Exploration",
                                     lvl2 == "firmicutes"~ "Firmicutes",
                                     lvl2 == "gametes" ~ "Gametes",
                                     lvl2 == "gen.stress" ~ "General Stress",
                                     lvl2 == "hemolysis" ~ "Hemolysis",
                                     lvl2 == "immune.cells"~"Immune Cells",
                                     lvl2 == "immune.other"~"Immune Other ",
                                     lvl2 == "inflammation" ~ "Inflammation",
                                     lvl2 == "intestinal.inflammation" ~ "Intestinal Inflammation",
                                     lvl2 == "intestinal.ion.transport" ~ "Intestinal Ion Transport",
                                     lvl2 == "intestinal.mucus.secretion" ~ "Intestinal Mucus Secretion",
                                     lvl2 == "intestinal.permeability" ~ "Intestinal Permeability",
                                     lvl2 == "intestinal.tight.junctions" ~ "Intestinal Tight Junctions",
                                     lvl2 == "kidney.histo"~"Kidney Histological abnormalities",
                                     lvl2 == "lipid.metabolism"~"Lipid Metabolism",
                                     lvl2 == "liver.histo"~"Liver Histological Abnormalities",
                                     lvl2 == "locomotion"~"Locomotion",
                                     lvl2 == "lungs.histo" ~ "Lung Histological Abnormalities",
                                     lvl2 == "lysosome" ~ "Lyosome",
                                     lvl2 == "melainabacteria" ~ "Melainabacteria",
                                     lvl2 == "morphology.gen" ~ "General Morphology",
                                     lvl2 == "mortality"~"Mortality",
                                     lvl2 == "nervous.system"~"Nervous System",
                                     lvl2 == "nucleus" ~ "Nucleus",
                                     lvl2 == "oxidative.stress"~"Oxidative Stress",
                                     lvl2 == "patescibacteria" ~ "Patescibacteria",
                                     lvl2 == "permeability" ~ "Permeability",
                                     lvl2 == "proliferation" ~ "Proliferation",
                                     lvl2 == "proteobacteria"~"Protebacteria",
                                     lvl2 == "reproduction" ~ "Reproduction",
                                     lvl2 == "reproductive.tissue" ~ "Reproductive Tissues",
                                     lvl2 == "respiration"~"Respiration",
                                     lvl2 == "spleen.histo" ~ "Spleen Histological Abnormalities",
                                     lvl2 == "tenericutes" ~ "Tenericutes",
                                     lvl2 == "thyroid" ~ "Thyroid",
                                     lvl2 == "verrucomicrobiae" ~ "Verrucomicrobiae",
                                     lvl2 == "vision" ~ "Vision"))) %>% #Renames for widget
  mutate(bio_h_f = factor(case_when(bio.org == "cell"~"Cell", #Renames for widget
                                    bio.org == "organism"~"Organism",
                                    bio.org == "subcell"~"Subcell",
                                    bio.org == "tissue" ~ "Tissue")))%>%
  mutate(vivo_h_f = factor(case_when(invitro.invivo == "invivo"~"In Vivo",
                                     invitro.invivo == "invitro"~"In Vitro")))%>% ##Renames for widget 
  mutate(life_h_f = factor(case_when(life.stage == "early,f1"~"Early, F1 Generation",
                                     life.stage == "early,f2"~"Early, F2 Generation",
                                     life.stage == "juvenile"~"Juvenile",
                                     life.stage == "adult"~"Adult",
                                     life.stage == "Not Reported"~"Not Reported")))%>% #Renames for widget
  mutate(exposure_route_h_f = factor(case_when(exposure.route == "drinking.water" ~ "Drinking Water",
                                               exposure.route == "food" ~ "Food",
                                               exposure.route == "gavage" ~ "Gavage",
                                               exposure.route == "gestation" ~ "Gestation",
                                               exposure.route == "gestation,lactation" ~ "Gestation & Lactation",
                                               exposure.route ==  "Not Applicable"~"Not Applicable (in vitro)")))%>% #Renames for widget - only categories included under 
                                                                                                                      #ingestion and in vitro are included (we don't want other 
                                                                                                                      #routes of exposure plotted in exploration because there is so little data)
  mutate(species_h_f = factor(case_when(species == "aries"~"(Sheep) Ovis aries",
                                        species == "sapiens"~"(Human) Homo sapiens",
                                        species == "musculus"~"(Mouse) Mus musculus",
                                        species == "cuniculus"~"(Rabbit) Oryctolagus cuniculus",
                                        species == "domesticus" ~ "(Pig) Sus domesticus",
                                        species == "norvegicus"~"(Rat) Rattus norvegicus"))) %>%  #Renames for widget
  mutate(tier_zero_particle_f = factor(case_when(particle.tier.zero == "Fail" ~ "Red Criteria Failed",
                                                 particle.tier.zero == "Pass" ~ "Red Criteria Passed"))) %>% 
  mutate(tier_zero_design_f = factor(case_when(design.tier.zero == "Fail" ~ "Red Criteria Failed",
                                                 design.tier.zero == "Pass" ~ "Red Criteria Passed"))) %>% 
  mutate(tier_zero_risk_f = factor(case_when(risk.tier.zero == "Fail" ~ "Red Criteria Failed",
                                                 risk.tier.zero == "Pass" ~ "Red Criteria Passed"))) 

#### User Interface ####

ui <- fluidPage(theme = shinytheme("flatly"),  
                
# App title
titlePanel(tagList(span((h1("Microplastics Toxicity Database: Mammals"))),
           span(actionButton("database_link", label="Go to Aquatic Organisms Database", class = "btn-primary", onclick ="window.open('https://sccwrp.shinyapps.io/aq_mp_tox_shiny/', '_blank')", style = "float:right")))
           ),
                
# Title panel subtext
tags$div("This website is only intended for use by invited participants of the Microplastics Health Effects Workshop."),
                
br(), # line break
                
# Main panel for displaying outputs
mainPanel(width = 12,
                          
# Output: set of 6 tabs
tabsetPanel(type = "tabs",
                                      
#### Introduction UI ####        
tabPanel("1: Introduction", 
                                               
br(), 
h3("What is the Microplastics Toxicity Database?", align = "center"), #Section 1
                                               
strong(p("This database is a repository for microplastics toxicity data that will be used to generate key graphics for the Microplastics Health Effects Workshop.")), 
                                               
p("This web application allows users to explore toxicity data using an intuitive interface while retaining the diversity and complexity inherent 
   to microplastics. Data is extracted from existing, peer-reviewed manuscripts containing toxicity data pertaining to microplastics."),
                                               
p("Use the numbered tabs at the top of the page to navigate to each section. Each section provides different information or data visualization options. More specific instructions may be found within each section."),
                                               
h3("Why was the Microplastics Toxicity Database and Web Application created?", align = "center"), #Section 2
                                               
p("The database and application tools have been created for use by the participants of the ", a(href = "https://www.sccwrp.org/about/research-areas/additional-research-areas/
trash-pollution/microplastics-health-effects-webinar-series/", 'Microplastics Health Effects Workshop',.noWS = "outside"),".The purpose of this workshop is to identify the most sensitive and biologically critical endpoints associated with microplastics exposure, 
prioritize which microplastics characteristics (e.g., size, shape, polymer) that are of greatest biological concern, and identify 
critical thresholds for each at which those biological effects become pronounced. Workshop participants will also make reccomendations for future
research investments. Workshop findings will be published in a special issue of ", a(href ="https://microplastics.springeropen.com/", 'Microplastics and Nanoplastics', .noOWs = "outside"),". 
These findings will be used directly by the state of California to fulfill ", a(href = "https://www.sccwrp.org/about/research-areas/
additional-research-areas/trash-pollution/microplastics-health-effects-webinar-series/history-california-microplastics-legislation/", 'legislative mandates',.noWS = "outside")," regarding the management of microplastics in drinking water and the aquatic environment."),
                                               
h3("Contributors", align = "center"), #Section 4: Contributors list with links to twitter and github
                                               
p(align = "center", a(href = "https://www.sccwrp.org/about/staff/leah-thornton-hampton/", 'Dr. Leah Thornton Hampton'),", Southern California Coastal Water Research Project ", 
tags$a(href="https://twitter.com/DrLeahTH", tags$img(src="twitter.png", width="2%", height="2%")), tags$a(href="https://github.com/leahth", tags$img(src="github.png", width="2%", height="2%"))),
p(align = "center", a(href = "https://www.sccwrp.org/about/staff/heili-lowman/", 'Dr. Heili Lowman'),", Southern California Coastal Water Research Project ",
tags$a(href="https://twitter.com/heili_lowman", tags$img(src="twitter.png", width="2%", height="2%")), tags$a(href="https://github.com/hlowman", tags$img(src="github.png", width="2%", height="2%"))), 
p(align = "center", a(href = "https://agency.calepa.ca.gov/staffdirectory/detail.asp?UID=69294&BDO=7&VW=DET&SL=S", 'Dr. Scott Coffin'),", California State Water Resources Control Board", 
tags$a(href="https://twitter.com/DrSCoffin", tags$img(src="twitter.png", width="2%", height="2%")), tags$a(href="https://github.com/ScottCoffin", tags$img(src="github.png", width="2%", height="2%"))),
p(align = "center", a(href = "https://www.sfei.org/users/liz-miller", 'Dr. Ezra Miller'),", Aquatic Science Center"),
p(align = "center", a(href = "https://rochmanlab.com/people/", 'Dr. Ludovic Hermabessiere'),", University of Toronto", 
tags$a(href="https://twitter.com/HermabessiereL", tags$img(src="twitter.png", width="2%", height="2%"))),
p(align = "center", a(href = "https://rochmanlab.com/people/", 'Hannah De Frond'),", University of Toronto", 
tags$a(href="https://twitter.com/HanDefrond", tags$img(src="twitter.png", width="2%", height="2%"))),
p(align = "center", "Emily Darin, Southern California Coastal Water Research Project",
tags$a(href="https://github.com/EmilyDarin", tags$img(src="github.png", width="2%", height="2%"))),
p(align = "center", "Syd Kotar, Southern California Coastal Water Research Project"),
p(align = "center", "Sarah Khan, Southern California Coastal Water Research Project"),
p(align = "center", a(href = "https://www.wur.nl/en/Persons/Bart-prof.dr.-AA-Bart-Koelmans.htm", 'Dr. Bart Koelmans'),", Wageningen University",
tags$a(href="https://twitter.com/MicroplasticLab", tags$img(src="twitter.png", width="2%", height="2%"))),
p(align = "center", a(href = "https://rochmanlab.com/", 'Dr. Chelsea Rochman'),", University of Toronto",
tags$a(href="https://twitter.com/ChelseaRochman", tags$img(src="twitter.png", width="2%", height="2%"))),
p(align = "center", a(href = "https://www.sccwrp.org/about/staff/alvina-mehinto/", 'Dr. Alvina Mehinto'),", Southern California Coastal Water Research Project"), 
p(align = "center", a(href = "https://www.sccwrp.org/about/staff/steve-weisberg/", 'Dr. Steve Weisberg'),", Southern California Coastal Water Research Project"), 
                                               
#Logos with links to organizations
                                               
splitLayout(align = "center", 
    tags$a(href="https://www.waterboards.ca.gov", tags$img(src="waterboard.png", width = "100%", height = "100%")),
    tags$a(href="https://www.sccwrp.org", tags$img(src="sccwrp.png", width = "100%", height = "100%")),
    tags$a(href="https://www.utoronto.ca", tags$img(src="toronto.png", width = "100%", height = "100%")),
    tags$a(href="https://www.sfei.org/", tags$img(src="sfei.png", width = "100%", height = "100%"))),
    br(), 
                                               
verbatimTextOutput(outputId = "Introduction1")),

#### Overview Human UI ####
                                      
tabPanel("2: Overview", 
br(), 
h3("Overview of Toxicological Effects in Human Systems", align = "center"),
br(),
p("Each bar displays the total number of measured endpoints where a statistically signifcant effect was detected (Y) or where a measurement was made but a significant effect was not detected (N)."), 
br(),
p("Detailed descriptions of data categories may be found under the Resources tab."),
br(),
#Plot type widget
# selectInput(inputId = "overview.type", "Overview Type (currently only applies to in vitro/in vivo plot):",
#              list("measurements and types" = "measurementsAndTypes", "studies and types" = "studiesAndTypes", "measurements and years" = "measurementsAndYears", "studies and years" = "studiesAndYears")),

column(width = 12,
       column(width = 12,
              plotOutput(outputId = "exposure_plot"),
              br())), 

# column(width = 12,
#        column(width = 2,
#               tableOutput('studies'),
#               br()), 
# 
#        column(width = 2,
#               tableOutput('measurements'),
#               br())),

column(width = 12,
       column(width = 6,
              plotOutput(outputId = "vivo_plot"),
              br()), 
       
       column(width = 6,
              plotOutput(outputId = "life_plot"),
              br())), 

column(width = 12,
       
       column(width = 4,
              plotOutput(outputId = "polymer_plot"),
              br()), 
       
       column(width = 4,
              plotOutput(outputId = "shape_plot"),
              br()), 

       column(width = 4,
              plotOutput(outputId = "size_plot"),
              br()))),
       
#### Exploration Human UI ####

tabPanel("3: Exploration",
                                               
shinyjs::useShinyjs(), # requires package for "reset" button, DO NOT DELETE - make sure to add any new widget to the reset_input in the server
id = "heili-tab", # adds ID for resetting Heili's tab's filters
                                               
h3("Exploration of Toxicological Effects in Mammalian Systems", align = "center"),
br(), 
p("Each figure displays a different metric along the y-axis - broad endpoint category, specific endpoint category, size, shape, and polymer, respectively.
  The values in the parentheses represent the number of measurements and studies, respectively, of each metric along the y-axis."),
br(),
p("The data displayed in these figures only display data from in vitro studies or in vivo studies where the initial exopsure route was ingestion and doses were reported as mass or counts per volume - other dosing units (e.g., particle mass/food mass) 
   are not displayed but are available in the complete database file."),
br(),
p("Filter the data: The data may be filtered using the drop-down menus located below. Then, click the 'Update Filters' button to refresh the data displayed according to your selections."),
br(),
p(strong("The data may be filtered by 'red criteria' for particle characterization, experimental design and applicability for risk assessment. More information about quality screening may be found in the Resources tab. ")),
br(), 
p("Change the plot type: The data may be visualized as a boxplot, violin plot or beeswarm plot using the drop-down menu below. Users may also visualize all individual data points by using the checkbox."),
br(), 
p("Download the data: Click the 'Download Data' button to retrieve the selected dataset as a '.csv' file."),
br(),
                                               
                           
                           # widget headers
                           column(width=12,
                                  
                                  column(width = 3,
                                         h4("Effects")),
                                  
                                  column(width = 3,
                                         h4("Particle Characteristics")),
                                  
                                  column(width = 3,
                                         h4("Biological Factors")),
                                  
                                  column(width = 3,
                                         h4("Quality Criteria"))),
                                 
                           # widgets
                           column(width = 12,
                                  
                                  column(width = 3,
                                         pickerInput(inputId = "lvl1_h_check", # endpoint checklist
                                                     label = "Broad Endpoint Category:", 
                                                     choices = levels(human_setup$lvl1_h_f),
                                                     selected = levels(human_setup$lvl1_h_f),
                                                     options = list(`actions-box` = TRUE), # option to de/select all
                                                     multiple = TRUE)), # allows for multiple inputs
                                  column(width = 3,
                                         pickerInput(inputId = "poly_h_check", # polymer checklist
                                                     label = "Polymer:", 
                                                     choices = levels(human_setup$poly_h_f),
                                                     selected = levels(human_setup$poly_h_f),
                                                     options = list(`actions-box` = TRUE), 
                                                     multiple = TRUE)),
                                  
                                  column(width = 3,
                                         pickerInput(inputId = "exposure_route_h_check", # polymer checklist
                                                     label = "Exposure Route:", 
                                                     choices = levels(human_setup$exposure_route_h_f),
                                                     selected = levels(human_setup$exposure_route_h_f),
                                                     options = list(`actions-box` = TRUE), 
                                                     multiple = TRUE)),
                                  
                                  column(width = 3,
                                         pickerInput(inputId = "particle_tier_zero_h_check", # polymer checklist
                                                     label = "Particle Characterization:", 
                                                     choices = levels(human_setup$tier_zero_particle_f),
                                                     selected = levels(human_setup$tier_zero_particle_f),
                                                     options = list(`actions-box` = TRUE), 
                                                     multiple = TRUE))),

                           # New row of widgets
                           column(width = 12,
                                  
                                  column(width = 3,
                                         htmlOutput("secondSelection")), # dependent endpoint checklist
                                  
                                  column(width = 3,
                                         pickerInput(inputId = "shape_h_check", # shape checklist
                                                     label = "Shape:", 
                                                     choices = levels(human_setup$shape_h_f),
                                                     selected = levels(human_setup$shape_h_f),
                                                     options = list(`actions-box` = TRUE), 
                                                     multiple = TRUE)),
                           
                                 column(width = 3,
                                        pickerInput(inputId = "bio_h_check", # bio org checklist
                                                    label = "Level of Biological Organization", 
                                                    choices = levels(human_setup$bio_h_f),
                                                    selected = levels(human_setup$bio_h_f),
                                                    options = list(`actions-box` = TRUE),
                                                    multiple = TRUE)),
                                 
                                 column(width = 3,
                                        pickerInput(inputId = "design_tier_zero_h_check", # polymer checklist
                                                    label = "Experimental Design:", 
                                                    choices = levels(human_setup$tier_zero_design_f),
                                                    selected = levels(human_setup$tier_zero_design_f),
                                                    options = list(`actions-box` = TRUE), 
                                                    multiple = TRUE))),

                           # New row of widgets
                           column(width = 12,
                                  
                                  column(width = 3,
                                         pickerInput(inputId = "effect_h_check",  # Effect Yes/No widget
                                                     label = "Effect:",
                                                     choices = levels(human_setup$effect_h_f),
                                                     selected = levels(human_setup$effect_h_f),
                                                     options = list(`actions-box` = TRUE),
                                                     multiple = TRUE)),
                                  
                                  column(width = 3,
                                         pickerInput(inputId = "size_h_check", # Environment checklist
                                                     label = "Size Category:", 
                                                     choices = levels(human_setup$size_h_f),
                                                     selected = levels(human_setup$size_h_f),
                                                     options = list(`actions-box` = TRUE), 
                                                     multiple = TRUE)),
                                  
                                  column(width = 3,
                                         pickerInput(inputId = "life_h_check", # life stage checklist
                                                     label = "Life Stages:", 
                                                     choices = levels(human_setup$life_h_f),
                                                     selected = levels(human_setup$life_h_f),
                                                     options = list(`actions-box` = TRUE), 
                                                     multiple = TRUE)),
                                  column(width = 3,
                                         pickerInput(inputId = "risk_tier_zero_h_check", # polymer checklist
                                                     label = "Applicability for Risk Assessment:", 
                                                     choices = levels(human_setup$tier_zero_risk_f),
                                                     selected = levels(human_setup$tier_zero_risk_f),
                                                     options = list(`actions-box` = TRUE), 
                                                     multiple = TRUE))),

                            column(width = 12,
       
                                  column(width = 3, offset = 6, 
                                         pickerInput(inputId = "species_h_check", # polymer checklist
                                                     label = "Species:", 
                                                     choices = levels(human_setup$species_h_f),
                                                     selected = levels(human_setup$species_h_f),
                                                     options = list(`actions-box` = TRUE), 
                                                     multiple = TRUE))),
                           
                           # New row of widgets
                           #column(width=12,
                                  
                                  #column(width = 3),
                                  
                                  #Slider Widget - commented out for now
                                  #column(width = 3,
                                  #sliderInput("range", # Allows for max input
                                  #label = "Particle Size (µm):", #Labels widget
                                  #min = 0, max = 4000, value = 4000)),

                                radioButtons(inputId = "dose_check", # dosing units
                                             label = "Particles/L or mg/mL:",
                                             choices = c("Particles/L", "mg/mL"),
                                             selected = "mg/mL"),
                                
                                p("Concentrations may be reported in mass/volume or particle #/volume (or sometimes both). Using methods described in", a(href ="https://pubs.acs.org/doi/10.1021/acs.est.0c02982", "Koelmans et. al (2020)"), " units have been converted."),
                                
                                radioButtons(inputId = "Rep_Con_rad",
                                             label = "Do you want to use just the reported, just the converted, or all exposure concentrations?",
                                             choices = c("reported", "converted", "all"),
                                             selected = "all"),
                                selectInput(inputId = "plot.type", "Plot Type:", 
                                            list(boxplot = "boxplot", violin = "violin", beeswarm = "beeswarm") #need to fix, just comment out for now
                                            ),
                                checkboxInput(inputId = "show.points", "Show All Points", FALSE),


                           # New row of widgets
                           column(width=12,
                                  column(width = 3,
                                         actionButton("go", "Update Filters", class = "btn-success")), # adds update action button
                                  # "go" is the internal name to refer to the button
                                  # "Update Filters" is the title that appears on the app
                                  
                                  column(width = 3,
                                         downloadButton("downloadData", "Download Data", class = "btn-info")), # adds download button
                                  
                                  # "downloadData" is the internal name
                                  # "Download Data" is the title that appears on the button
                                  
                                  column(width = 3,
                                         actionButton("reset_input", "Reset Filters"))), # adds update button
                           
                           # "Reset_input" is the internal name
                           # "Reset Filter" is the title that appears on the button  
                           
                           # New row
                           column(width=12,  
                                  column(width = 3,
                                         br(),
                                         strong(p("To Begin: Click the 'Update Filters' button above.")),
                                         br()),
                                  column(width = 3),
                                  column(width = 3,
                                         br(),
                                         strong(p("To Reset: Click the 'Reset Filters' button above, followed by the 'Update Filters' button to the left.")),
                                         br())), 
                           
                           # New row
                           column(width = 12,
                                  hr()), # adds divider
                           
                           #column(width = 12,
                           #plotOutput(outputId = "organism_plot_react"),
                           #br())), 
                           
                           column(width = 12,
                                  
                                  column(width = 12,
                                         plotOutput(outputId = "lvl_h_plot_react", height = "600px"),
                                         br())), 
                                
                                
                           column(width = 12,
                                  
                                  column(width = 12,
                                         plotOutput(outputId = "lvl2_h_plot_react", height = "600px"),
                                         br())), 
                                  
                           column(width = 12,
                                  
                                  column(width = 12,
                                         plotOutput(outputId = "exposure_route_h_plot_react", height = "600px"),
                                         br())), 
                             
                           column(width = 12,
                                  
                                  column(width = 12,
                                         plotOutput(outputId = "size_h_plot_react", height = "600px"),
                                         br())), 
                                 
                           column(width = 12,
                                  
                                  column(width = 12,
                                         plotOutput(outputId = "shape_h_plot_react", height = "600px"),
                                         br())), 

                           column(width = 12,
                                  
                                  column(width = 12,
                                         plotOutput(outputId = "poly_h_plot_react", height = "600px"),
                                         br()))),
                                  
                                  
                  
#### Resources UI ####

tabPanel("4: Resources", 
         br(),     
         h3(align = "center", a(href = "https://sccwrp-my.sharepoint.com/:b:/g/personal/leahth_sccwrp_org/EYUFX1dOfSdGuHSfrUDcnewBxgttfTCOwom90hrt5nx1FA?e=jFXEyQ", 'Data Category Descriptions')),
         br(),
         h3(align = "center", a(href = "https://sccwrp-my.sharepoint.com/:b:/g/personal/leahth_sccwrp_org/EWr13IwjTIZHsqzq5vIkPswBsECP2riSv0rkj5_iEf56vQ?e=auae3g", 'Quality Screening: Red Criteria')),
         br(),
         h3(align = "center", a(href = "https://sccwrp-my.sharepoint.com/:b:/g/personal/leahth_sccwrp_org/EdKsPlTjls9FtDrwqisZmAEBi7tZQYyywL3qml9y-fR25g?e=pHJkGY", 'Mammalian Study List')),
         br(),
         h3(align = "center", a(href = "https://sccwrp-my.sharepoint.com/:b:/g/personal/leahth_sccwrp_org/EXf0crCKDPVHo5xBEdw4PQwBxA8cnu0x4WY477CuEzZcPw?e=qs00V3", 'Dose Conversion Methods'))),
         
#### Contact UI ####

tabPanel("5: Contact", 
         br(),
         h4("For scientific questions or access to the complete database, please contact Dr. Leah Thornton Hampton (leahth@sccwrp.org)."),
         br(),
         h4("If you encounter technical problems with the web application, please contact Emily Darin (Emily.Darin@student.csulb.edu)."),
         
         verbatimTextOutput(outputId = "Leah3"))

#following three parentheses close out UI. Do not delete. 
)))   


#### Server ####
server <- function(input, output) {
  
  #### Introduction S ####
  
  # Introduction does not have any reactive features.
  
  
  #### Overview Human S ####

#Test code for reactive plots based on measurements or study types  
  
#   output$polymer_plot <- renderPlot({
# #make distinct plots for measurements or studies
# 
# #make plot for studies
# if(input$overview.type == "studies"){
#   p <- ggplot(polyfinal,aes(fill=effect, y= logEndpoints, x= Type, Percent=Percent)) +
#     geom_bar(position="stack", stat="identity") +
#     geom_text(aes(label= paste0(Endpoints)), position = position_stack(vjust = 0.5),colour="black") +
#     scale_fill_manual(values = cal_palette("seagrass"))+
#     ylab("Number of Studies") +
#     labs(fill="Effect") +
#     ggtitle("Polymer Type") +
#     guides(x = guide_axis(angle = 45))+
#     overviewTheme()
# }
# 
#     else{
#       # generate plot
#       p <- ggplot(polyfinal,aes(fill=effect, y= logEndpoints, x= Type, Percent=Percent)) +
#         geom_bar(position="stack", stat="identity") +
#         geom_text(aes(label= paste0(Endpoints)), position = position_stack(vjust = 0.5),colour="black") +
#         scale_fill_manual(values = cal_palette("seagrass"))+
#         ylab("Number of Endpoints Measured") +
#         labs(fill="Effect") +
#         ggtitle("Polymer Type") +
#         guides(x = guide_axis(angle = 45))+
#         overviewTheme()
#     }
# 
# print(p)
#   })
# 
#   #in vitro/in vivo plot
#   output$vivo_plot <- renderPlot({
# 
# 
#     #measurements and types
#     if(input$overview.type == "measurementsAndTypes"){
#       # generate plot
#       p <- ggplot(vivofinal,aes(fill=effect, y= logEndpoints, x= Type, Percent=Percent)) +
#         geom_bar(position="stack", stat="identity") +
#         geom_text(aes(label= paste0(Endpoints)), position = position_stack(vjust = 0.5),colour="black") +
#         scale_fill_manual(values = cal_palette("lupinus"))+
#         ylab("Number of Endpoints Measured") +
#         labs(fill="Effect") +
#         ggtitle("In Vitro or In Vivo")+
#         guides(x = guide_axis(angle = 45))+
#         overviewTheme()
#       }
#     #measurements and years
#     if(input$overview.type == "measurementsAndYears" ){
#       # generate plot
#       p <- ggplot(vivoFinal_year,aes(fill=Type, y= logEndpoints, x= year, Percent=Percent)) +
#         geom_bar(position="stack", stat="identity") +
#         geom_text(aes(label= paste0(Endpoints)), position = position_stack(vjust = 0.5),colour="black") +
#         scale_fill_manual(values = cal_palette("lupinus"))+
#         ylab("Number of Endpoints Measured") +
#         labs(fill="Study Type") +
#         ggtitle("In Vitro or In Vivo Measurements by Year")+
#         guides(x = guide_axis(angle = 45))+
#         overviewTheme()
#       #currently displays years as a factor. can't decide if to switch to numeric or not
#       }
# 
#     #studies and types
#     if(input$overview.type == "studiesAndTypes"){
#       p <- ggplot(vivoFinal_type_study,aes(fill=effect, y= Studies, x= Type, Percent=Percent)) +
#         geom_bar(position="stack", stat="identity") +
#         geom_text(aes(label= paste0(Studies)), position = position_stack(vjust = 0.5),colour="black") +
#         scale_fill_manual(values = cal_palette("lupinus"))+
#         ylab("Number of Studies") +
#         labs(fill="Effect") +
#         ggtitle("In Vitro or In Vivo Studies by Type")+
#         guides(x = guide_axis(angle = 45))+
#         overviewTheme()
#     }
# 
#     #studies and years
#     if(input$overview.type == "studiesAndYears" ){
#       # generate plot
#       p <- ggplot(vivoFinal_year_study,aes(fill=Type, y= Studies, x= year, Percent=Percent)) +
#         geom_bar(position="stack", stat="identity") +
#         scale_x_continuous(breaks = seq(from = 1993, to = 2020, by = 1 ))+ #show all dates
#         geom_text(aes(label= paste0(Studies)), position = position_stack(vjust = 0.5),colour="black") +
#         scale_fill_manual(values = cal_palette("lupinus"))+
#         ylab("Number of Studies") +
#         labs(fill="Study Type") +
#         ggtitle("In Vitro or In Vivo Studies by Year")+
#         guides(x = guide_axis(angle = 45))+
#         overviewTheme()
#     }
# 
#     print(p)
#   })


  #Polymer category plot
  output$polymer_plot <- renderPlot({

    # generate plot
    ggplot(polyfinal,aes(fill=effect, y= logEndpoints, x= Type, Percent=Percent)) +
      geom_bar(position="stack", stat="identity") +
      geom_text(aes(label= paste0(Endpoints)), position = position_stack(vjust = 0.5),colour="black", size = 5) +
      scale_fill_manual(values = cal_palette("seagrass"))+
      theme_classic() +
      ylab("Number of Endpoints Measured") +
      labs(fill="Effect") +
      ggtitle("Polymer Type")+
      guides(x = guide_axis(angle = 45))+
      theme(text = element_text(size=17),plot.title = element_text(hjust = 0.5, face="bold"))+
      theme(legend.position = "right",
            axis.ticks= element_blank(),
            axis.text.x = element_text(),
            axis.text.y = element_blank(),
            axis.title.x = element_blank())
  })

  
  #In vivo in vitro plot
  output$vivo_plot <- renderPlot({

    # generate plot
    ggplot(vivofinal,aes(fill=effect, y= logEndpoints, x= Type, Percent=Percent)) +
      geom_bar(position="stack", stat="identity") +
      geom_text(aes(label= paste0(Endpoints)), position = position_stack(vjust = 0.5),colour="black", size = 5) +
      scale_fill_manual(values = cal_palette("lupinus"))+
      theme_classic() +
      ylab("Number of Endpoints Measured") +
      labs(fill="Effect") +
      ggtitle("In Vivo/In Vitro")+
      guides(x = guide_axis(angle = 45))+
      theme(text = element_text(size=17),plot.title = element_text(hjust = 0.5, face="bold"))+
      theme(legend.position = "right",
            axis.ticks= element_blank(),
            axis.text.x = element_text(),
            axis.text.y = element_blank(),
            axis.title.x = element_blank())
  })

  #Size category plot
  output$size_plot <- renderPlot({
    
    # generate plot
    ggplot(sizefinal,aes(fill=effect, y= logEndpoints, x= Type, Percent=Percent)) +
      geom_bar(position="stack", stat="identity") +
      geom_text(aes(label= paste0(Endpoints)), position = position_stack(vjust = 0.5),colour="black", size = 5) +
      scale_fill_manual(values = cal_palette("bigsur2"))+
      theme_classic() +
      ylab("Number of Endpoints Measured") +
      labs(fill="Effect") +
      ggtitle("Particle Size")+
      guides(x = guide_axis(angle = 45))+
      theme(text = element_text(size=17),plot.title = element_text(hjust = 0.5, face="bold"))+
      theme(legend.position = "right",
            axis.ticks= element_blank(),
            axis.text.x = element_text(),
            axis.text.y = element_blank(),
            axis.title.x = element_blank())
  })
  
  #Shape plot
  output$shape_plot <- renderPlot({
    
    # generate plot
    ggplot(shapefinal,aes(fill=effect, y= logEndpoints, x= Type, Percent=Percent)) +
      geom_bar(position="stack", stat="identity") +
      geom_text(aes(label= paste0(Endpoints)), position = position_stack(vjust = 0.5),colour="black", size = 5) +
      scale_fill_manual(values = cal_palette("vermillion"))+
      theme_classic() +
      ylab("Number of Endpoints Measured") +
      labs(fill="Effect") +
      ggtitle("Plastic Shapes")+
      guides(x = guide_axis(angle = 45))+
      theme(text = element_text(size=17),plot.title = element_text(hjust = 0.5, face="bold"))+
      theme(legend.position = "right",
            axis.ticks= element_blank(),
            axis.text.x = element_text(),
            axis.text.y = element_blank(),
            axis.title.x = element_blank())
  })
  
  #Life stage plot
  output$life_plot <- renderPlot({
    
    # generate plot
    ggplot(lifefinal,aes(fill=effect, y= logEndpoints, x= Type, Percent=Percent)) +
      geom_bar(position="stack", stat="identity") +
      geom_text(aes(label= paste0(Endpoints)), position = position_stack(vjust = 0.5),colour="black", size = 5) +
      scale_fill_manual(values = cal_palette("lake"))+
      theme_classic() +
      ylab("Number of Endpoints Measured") +
      ggtitle("Life Stage")+
      labs(fill="Effect") +
      guides(x = guide_axis(angle = 45))+
      theme(text = element_text(size=17),plot.title = element_text(hjust = 0.5, face="bold"))+
      theme(legend.position = "right",
            axis.ticks= element_blank(),
            axis.text.x = element_text(),
            axis.text.y = element_blank(),
            axis.title.x = element_blank())
  })
  
  #Exposure category plot
  output$exposure_plot <- renderPlot({
    
    # generate plot
    ggplot(routefinal,aes(fill=effect, y= logEndpoints, x= Type, Percent=Percent)) +
      geom_bar(position="stack", stat="identity") +
      geom_text(aes(label= paste0(Endpoints)), position = position_stack(vjust = 0.5),colour="black", size = 5) +
      scale_fill_manual(values = cal_palette("wetland"))+
      theme_classic() +
      ylab("Number of Endpoints Measured") +
      labs(fill="Effect") +
      ggtitle("Exposure Route")+
      guides(x = guide_axis(angle = 45))+
      theme(text = element_text(size=17),plot.title = element_text(hjust = 0.5, face="bold"))+
      theme(legend.position = "right",
            axis.ticks= element_blank(),
            axis.text.x = element_text(),
            axis.text.y = element_blank(),
            axis.title.x = element_blank())
  })
  
  #### Exploration Human S ####
  
 #Create dependent dropdown checklists: select lvl2 by lvl1.
  output$secondSelection <- renderUI({
    
    lvl1_h_c <- input$lvl1_h_check # assign level values to "lvl1_c"
    
    human_new <- human_setup %>% # take original dataset
      filter(lvl1_h_f %in% lvl1_h_c) %>% # filter by level inputs
      mutate(lvl2_f_new = factor(as.character(lvl2_h_f))) # new subset of factors
    
    pickerInput(inputId = "lvl2_h_check", 
                label = "Specific Endpoint within Broad Category:", 
                choices = levels(human_new$lvl2_f_new),
                selected = levels(human_new$lvl2_f_new),
                options = list(`actions-box` = TRUE),
                multiple = TRUE)})
  
  # Create new dataset based on widget filtering and adjusted to reflect the presence of the "update" button.
  human_filter <- eventReactive(list(input$go),{
    # eventReactive explicitly delays activity until you press the button
    # use the inputs to create a new dataset that will be fed into the renderPlot calls below
    
    # every selection widget should be represented as a new variable below
    lvl1_h_c <- input$lvl1_h_check # assign level values to "lvl1_c"
    lvl2_h_c <- input$lvl2_h_check # assign lvl2 values to "lvl2_c"
    bio_h_c <- input$bio_h_check # assign bio values to "bio_c"
    effect_h_c <- input$effect_h_check # assign effect values to "effect_c"
    life_h_c <- input$life_h_check #assign values to "life_check"
    poly_h_c <- input$poly_h_check # assign values to "poly_c"
    shape_h_c <- input$shape_h_check # assign values to "shape_c" 
    size_h_c <- input$size_h_check # assign values to "size_c"
    exposure_route_h_c<-input$exposure_route_h_check#assign values to exposure
    species_h_c<-input$species_h_check #assign values to "species_h_c"
    particle_tier_zero_h<-input$particle_tier_zero_h_check #assign values to "particle_tier_zero_h"
    design_tier_zero_h<-input$design_tier_zero_h_check #assign values to "design_tier_zero_h"
    risk_tier_zero_h<-input$risk_tier_zero_h_check #assign values to "risk_tier_zero_h"
    dose_check <- input$dose_check #renames selection from radio button
    Rep_Con_rad <- input$Rep_Con_rad #use nominal or calculated exposure concentrations. Options are TRUE (calculated) or FALSE (reported)
    
    #filter out reported, calcualted, or all based on checkbox and make new variable based on mg/L or particles/mL
    if(Rep_Con_rad == "reported" & dose_check == "mg/mL"){
      human_setup <- human_setup %>% 
        filter(dose.mg.mL.master.reported.converted == "reported") %>% 
        mutate(dose_new = dose.mg.mL.master)}
    
    if(Rep_Con_rad == "converted" & dose_check == "mg/mL"){
      human_setup <- human_setup %>%
        filter(dose.mg.mL.master.reported.converted == "converted") %>% 
        mutate(dose_new = dose.mg.mL.master)}
    
    if(Rep_Con_rad == "all" & dose_check == "mg/mL"){
      human_setup <- human_setup %>%
        mutate(dose_new = dose.mg.mL.master)}
    
    if(Rep_Con_rad == "reported" & dose_check == "Particles/L"){
      human_setup <- human_setup %>%
        filter(dose.particles.L.master.reported.converted == "reported") %>% 
        mutate(dose_new = dose.particles.L.master)}
    
    if(Rep_Con_rad == "converted" & dose_check == "Particles/L"){
      human_setup <- human_setup %>%
        filter(dose.particles.L.master.reported.converted == "converted") %>% 
        mutate(dose_new = dose.particles.L.master)} 
    
    if(Rep_Con_rad == "all" & dose_check == "Particles/L"){
      human_setup <- human_setup %>%
        mutate(dose_new = dose.particles.L.master)}
    
    human_setup %>% # take original dataset
      #filter(vivo_h_f %in% vivo_h_c) %>% #filter by invivo or invitro
      filter(lvl1_h_f %in% lvl1_h_c) %>% # filter by level inputs
      filter(lvl2_h_f %in% lvl2_h_c) %>% #filter by level 2 inputs 
      filter(bio_h_f %in% bio_h_c) %>% #filter by bio organization
      filter(effect_h_f %in% effect_h_c) %>% #filter by effect
      filter(life_h_f %in% life_h_c) %>% #filter by life stage
      filter(poly_h_f %in% poly_h_c) %>% #filter by polymer
      filter(shape_h_f %in% shape_h_c) %>% #filter by shape
      filter(size_h_f %in% size_h_c) %>% #filter by size class
      filter(exposure_route_h_f %in% exposure_route_h_c)%>% #filter by exposure route
      filter(species_h_f %in% species_h_c) %>% #filter by species
      filter(tier_zero_particle_f %in% particle_tier_zero_h) %>% #filter by particle red quality criteria
      filter(tier_zero_design_f %in% design_tier_zero_h) %>% #filter by experimental design red quality criteria
      filter(tier_zero_risk_f %in% risk_tier_zero_h) #filter by risk assessment red quality criteria
    
      #filter(size.length.um.used.for.conversions <= range_n) #For size slider widget - currently commented out
    
  })
  
  output$caption<-renderText({ #rename plot types in UI
    switch(input$plot.type,
           "boxplot" 	= 	"Boxplot",
           "violin" = "Violin Plot",
           "beeswarm" = "Beeswarm",
           "bar" 		=	"Bar graph")
  })
  
  # Use newly created dataset from above to generate plots for size, shape, polymer, and endpoint plots on four different rows.
  
  
  # Size Plot
  output$size_h_plot_react <- renderPlot({
    
    #plot types
    plot.type<-switch(input$plot.type,
                      "boxplot" 	= geom_boxplot(alpha = 0.7, aes(color = effect_h_f)),
                      "violin" = geom_violin(alpha = 0.7, aes(color = effect_h_f)),
                      "beeswarm" = geom_quasirandom(alpha = 0.7, aes(color = effect_h_f), method = "smiley", groupOnX = FALSE, cex = 2) #groupOnX specifies groups on y axis
                        )
    #Create new dataset to gather number of studies and measurements by size
    human_size1 <- human_filter() %>%
      drop_na(dose_new) %>%
      group_by(size_h_f, vivo_h_f, effect_h_f) %>% 
      summarize(dose_new = quantile(dose_new, .1),
                measurements = n(),
                studies = n_distinct(article))
      
    #Render reactive plot
      p <- ggplot(human_filter(), aes(x = dose_new, y = size_h_f, fill = effect_h_f)) + #define base ggplot
        plot.type + #adds user-defined geom()
        scale_x_log10() +
        scale_color_manual(values = c("#A1CAF6", "#4C6FA1")) +
        scale_fill_manual(values = c("#A1CAF6", "#4C6FA1")) +
        geom_label_repel(data = human_size1, 
                         aes(label = paste("(",measurements,",",studies,")")),
                         hjust = 0,
                         direction = "y", 
                         nudge_x = 1000000000,
                         segment.colour = NA, size = 3.5, show.legend = FALSE) +
        theme_classic() +
        theme(text = element_text(size=18), 
              legend.position = "right") +
        labs(x = input$dose_check,
             y = "Size",
             color = "Effect?",
             fill = "Effect?",
             caption = (input$Rep_Con_rad))+
        facet_wrap(~vivo_h_f)%>%
        req(nrow(human_filter()) > 0) #Suppresses facet_wrap error message
      
      if(input$show.points==TRUE & (input$plot.type == "boxplot" || input$plot.type == "violin")){
        p<-p+geom_point(aes(color = effect_h_f), alpha=0.8, position = 'jitter')
      }
      
    else {
    p
    }
    print(p)
  })
  
  # Shape Plot
  
  output$shape_h_plot_react <- renderPlot({
    
    #plot types
    plot.type<-switch(input$plot.type,
                      "boxplot" 	= geom_boxplot(alpha = 0.7, aes(color = effect_h_f)),
                      "violin" = geom_violin(alpha = 0.7, aes(color = effect_h_f)),
                     "beeswarm" = geom_quasirandom(alpha = 0.7, aes(color = effect_h_f), method = "smiley", groupOnX = FALSE, cex = 2)) #groupOnX specifies groups on y axis)
    
    human_shape1 <- human_filter() %>%
      drop_na(dose_new) %>%
      group_by(shape_h_f, vivo_h_f, effect_h_f) %>% 
      summarize(dose_new = quantile(dose_new, .1),
                measurements = n(),
                studies = n_distinct(article))
    #build plot
    p <- ggplot(human_filter(), aes(x = dose_new, y = shape_h_f, fill = effect_h_f)) +
      scale_x_log10() +
      plot.type + #adds user-defined geom()
      scale_color_manual(values = c("#C7EAE5","#35978F")) +
      scale_fill_manual(values = c("#C7EAE5", "#35978F")) +
      geom_label_repel(data = human_shape1, 
                       aes(label = paste("(",measurements,",",studies,")")),
                       hjust = 0,
                       direction = "y", 
                       nudge_x = 1000000000,
                       segment.colour = NA, size = 3.5, show.legend = FALSE) +
      theme_classic() +
      theme(text = element_text(size=18), 
            legend.position = "right") +
      labs(x = input$dose_check,
           y = "Shape",
           color = "Effect?",
           fill = "Effect?",
           caption = (input$Rep_Con_rad))+
      facet_wrap(~vivo_h_f)%>%
      req(nrow(human_filter()) > 0) #Suppresses facet_wrap error message
    
    if(input$show.points==TRUE & (input$plot.type == "boxplot" || input$plot.type == "violin")){
      p<-p+geom_point(aes(color = effect_h_f), alpha=0.5, position = 'jitter')
    }
    
    else {
      p
    }
    print(p)
    
  })
  
  # Polymer Plot
  
  output$poly_h_plot_react <- renderPlot({
    
    #plot types
    plot.type<-switch(input$plot.type,
                      "boxplot" 	= geom_boxplot(alpha = 0.7, aes(color = effect_h_f)),
                      "violin" = geom_violin(alpha = 0.7, aes(color = effect_h_f)),
                      "beeswarm" = geom_quasirandom(alpha = 0.7, aes(color = effect_h_f), method = "smiley", groupOnX = FALSE, cex = 2)) #groupOnX specifies groups on y axis)
    human_poly1 <- human_filter() %>%
      drop_na(dose_new) %>%
      group_by(poly_h_f, vivo_h_f, effect_h_f) %>% 
      summarize(dose_new = quantile(dose_new, .1),
                measurements = n(),
                studies = n_distinct(article))
    #build plot
    p <- ggplot(human_filter(), aes(x = dose_new, y = poly_h_f, fill = effect_h_f)) +
      scale_x_log10() +
      plot.type + #adds user-defined geom()
      scale_color_manual(values = c("#FAB455", "#A5683C")) +
      scale_fill_manual(values = c("#FAB455", "#A5683C")) +
      geom_label_repel(data = human_poly1, 
                       aes(label = paste("(",measurements,",",studies,")")),
                       hjust = 0,
                       direction = "y", 
                       nudge_x = 1000000000,
                       segment.colour = NA, size = 3.5, show.legend = FALSE) +
      theme_classic() +
      theme(text = element_text(size=18),
            legend.position = "right") +
      labs(x = input$dose_check,
           y = "Polymer",
           color = "Effect?",
           fill = "Effect?",
           caption = (input$Rep_Con_rad))+
      facet_wrap(~vivo_h_f)%>%
      req(nrow(human_filter()) > 0) #Suppresses facet_wrap error message
    
    if(input$show.points==TRUE & (input$plot.type == "boxplot" || input$plot.type == "violin")){
      p<-p+geom_point(aes(color = effect_h_f), alpha=0.5, position = 'jitter')
    }
    
    else {
      p
    }
    print(p)
  })
  
  # Endpoint Plot
  
  output$lvl_h_plot_react <- renderPlot({
    
    #plot types
    plot.type<-switch(input$plot.type,
                      "boxplot" 	= geom_boxplot(alpha = 0.7, aes(color = effect_h_f)),
                      "violin" = geom_violin(alpha = 0.7, aes(color = effect_h_f)),
                      "beeswarm" = geom_quasirandom(alpha = 0.7, aes(color = effect_h_f), method = "smiley", groupOnX = FALSE, cex = 2)) #groupOnX specifies groups on y axis)
    #build plot 
    human_lvl1 <- human_filter() %>%
    drop_na(dose_new) %>%
      group_by(lvl1_h_f, vivo_h_f, effect_h_f) %>% 
      summarize(dose_new = quantile(dose_new, .1),
                measurements = n(),
                studies = n_distinct(article))
    
    p <- ggplot(human_filter(), aes(x = dose_new, y = lvl1_h_f, fill = effect_h_f)) +
      scale_x_log10() +
      plot.type + #adds user-defined geom()
      scale_color_manual(values = c("#A99CD9", "#6C568C")) +
      scale_fill_manual(values = c("#A99CD9", "#6C568C")) +
      geom_label_repel(data = human_lvl1, 
                       aes(label = paste("(",measurements,",",studies,")")),
                       hjust = 0,
                       direction = "y", 
                       nudge_x = 1000000000,
                       segment.colour = NA, size = 3.5, show.legend = FALSE) +
      theme_classic() +
      theme(text = element_text(size=18),
            legend.position = "right") +
      labs(x = input$dose_check,
           y = "Endpoint",
           color = "Effect?",
           fill = "Effect?",
           caption = (input$Rep_Con_rad))+
      facet_wrap(~vivo_h_f)%>%
      req(nrow(human_filter()) > 0) #Suppresses facet_wrap error message
    
    if(input$show.points==TRUE & (input$plot.type == "boxplot" || input$plot.type == "violin")){
      p<-p+geom_point(aes(color = effect_h_f), alpha=0.5, position = 'jitter')
    }
    
    else {
      p
    }
    print(p)
    
  })
  
  #Lvl2 Plot 
  
  output$lvl2_h_plot_react <- renderPlot({
    #plot types
    plot.type<-switch(input$plot.type,
                      "boxplot" 	= geom_boxplot(alpha = 0.7, aes(color = effect_h_f)),
                      "violin" = geom_violin(alpha = 0.7, aes(color = effect_h_f)),
                      "beeswarm" = geom_quasirandom(alpha = 0.7, aes(color = effect_h_f), method = "smiley", groupOnX = FALSE, cex = 2)) #groupOnX specifies groups on y axis)
    
    human_lvl21 <- human_filter() %>%
      drop_na(dose_new) %>%
      group_by(lvl2_h_f, vivo_h_f, effect_h_f) %>% 
      summarize(dose_new = quantile(dose_new, .1),
                measurements = n(),
                studies = n_distinct(article))
    
    
    #build plot
   p<-  ggplot(human_filter(), aes(x = dose_new, y = lvl2_h_f, fill = effect_h_f)) +
      scale_x_log10() +
      plot.type + #adds user-defined geom()
      scale_color_manual(values = c("#A99CD9", "#6C568C")) +
      scale_fill_manual(values = c("#A99CD9", "#6C568C")) +
     geom_label_repel(data = human_lvl21, 
                      aes(label = paste("(",measurements,",",studies,")")),
                      hjust = 0,
                      direction = "y", 
                      nudge_x = 1000000000,
                      segment.colour = NA, size = 3.5, show.legend = FALSE) +
      theme_classic() +
      theme(text = element_text(size=18),
            legend.position = "right") +
      labs(x = input$dose_check,
           y = "Specific Endpoint",
           color = "Effect?",
           fill = "Effect?",
           caption = (input$Rep_Con_rad))+
      facet_wrap(~vivo_h_f)%>%
      req(nrow(human_filter()) > 0) #Suppresses facet_wrap error message
   
   if(input$show.points==TRUE & (input$plot.type == "boxplot" || input$plot.type == "violin")){
     p<-p+geom_point(aes(color = effect_h_f), alpha=0.5, position = 'jitter')
   }
   
   else {
     p
   }
   print(p)
  })
  
  
  #exposure route 
  
  output$exposure_route_h_plot_react <- renderPlot({
    #plot types
    plot.type<-switch(input$plot.type,
                      "boxplot" 	= geom_boxplot(alpha = 0.7, aes(color = effect_h_f)),
                      "violin" = geom_violin(alpha = 0.7, aes(color = effect_h_f)),
                      "beeswarm" = geom_quasirandom(alpha = 0.7, aes(color = effect_h_f), method = "smiley", groupOnX = FALSE, cex = 2)) #groupOnX specifies groups on y axis)
    
    human_exposure1 <- human_filter() %>%
      drop_na(dose_new) %>%
      group_by(exposure_route_h_f, vivo_h_f, effect_h_f) %>% 
      summarize(dose_new = quantile(dose_new, .1),
                measurements = n(),
                studies = n_distinct(article))
    
    #build plot
    p<- ggplot(human_filter(), aes(x = dose_new, y = exposure_route_h_f, fill = effect_h_f)) +
      scale_x_log10() +
      plot.type + #adds user-defined geom()
      scale_color_manual(values = c("#C7EAE5","#35978F")) +
      scale_fill_manual(values = c("#C7EAE5", "#35978F")) +
      geom_label_repel(data = human_exposure1, 
                       aes(label = paste("(",measurements,",",studies,")")),
                       hjust = 0,
                       direction = "y", 
                       nudge_x = 1000000000,
                       segment.colour = NA, size = 3.5, show.legend = FALSE) +
      theme_classic() +
      theme(text = element_text(size=18), 
            legend.position = "right") +
      labs(x = input$dose_check,
           y = "Exposure Route",
           color = "Effect?",
           fill = "Effect?",
           caption = (input$Rep_Con_rad))+
      facet_wrap(~vivo_h_f)%>%
      req(nrow(human_filter()) > 0) #Suppresses facet_wrap error message
    
    if(input$show.points==TRUE & (input$plot.type == "boxplot" || input$plot.type == "violin")){
      p<-p+geom_point(aes(color = effect_h_f), alpha=0.5, position = 'jitter')
    }
    
    else {
      p
    }
    print(p)
  })
  
  # Create downloadable csv of filtered dataset.
  # Removed columns created above so the dataset matches Leah's original dataset.
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(human_filter() %>%
                  select(-c(effect_h_f, size_h_f, shape_h_f, poly_h_f, lvl1_h_f, lvl2_h_f, bio_h_f, vivo_h_f, life_h_f, exposure_route_h_f, species_h_f)), 
                file, row.names = FALSE)
    }
  )
  
  # Create "reset" button to revert all filters back to what they began as.
  # Need to call all widgets individually by their ids.
  # See https://stackoverflow.com/questions/44779775/reset-inputs-with-reactive-app-in-shiny for more information.
  observeEvent(input$reset_input, {
    shinyjs::reset("lvl1_h_check")
    shinyjs::reset("lvl2_h_check")
    shinyjs::reset("poly_h_check")
    shinyjs::reset("shape_h_check")
    shinyjs::reset("effect_h_check")
    shinyjs::reset("size_h_check")
    shinyjs::reset("life_h_check")
    shinyjs::reset("bio_h_check")
    shinyjs::reset("vivo_h_check")
    shinyjs::reset("exposure_route_h_check")
    shinyjs::reset("species_h_check")
    shinyjs::reset("particle_tier_zero_h_check")
    shinyjs::reset("design_tier_zero_h_check")
    shinyjs::reset("risk_tier_zero_h_check")
    
  }) #If we add more widgets, make sure they get added here.   
  
} #Server end

#### Full App ####
shinyApp(ui = ui, server = server)

# End of R Shiny app script.
