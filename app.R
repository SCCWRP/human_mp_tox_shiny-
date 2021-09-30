#### Aquatic Microplastics Toxicology Shiny App
#### File created: September 23, 2020
#### Code contributors: Heili Lowman, Leah Thornton Hampton, Scott Coffin, Emily Darin

#### Setup ####

# Load packages
library(tidyverse) #General everything
library(shinydashboard)
library(RColorBrewer) #plot colors
library(ggplot2) #General plotting
library(ggrepel) #For adding text labels that repel away from data points
library(calecopal) #Color palette
library(shiny) #Runs shiny
library(shinythemes) #Shiny theme for the page
library(shinyWidgets) #Widgets
library(scales) #SSD - Use the percent format
library(reshape2) #Overview tab - melts bars together
library(DT) #Build HTML data tables
library(plotly) #Make plots interactive
library(viridis) #Colors
library(scales) #To use "percent" function
library(shinyjs) #Exploration tab - reset button
library(tigerstats) #row percent values 
library(ggbeeswarm) #plot all points nicely
library(collapsibleTree) #plot type for endpoint category tree
library(ggdark) #dark mode ggplot
library(ggsci) #color palettes
library(RColorBrewer) #color palette
library(viridis) #Colors

# Load finalized dataset.

human <- read_csv("Humans_Clean_Final.csv", guess_max = 10000)

#### Welcome Setup ####

# All text inputs below.

#### Overview Human Setup ####

#Set up for polymer overview plot
polydf<-rowPerc(xtabs( ~polymer +effect, human)) #pulls polymers by effect 
polyf<-as.data.frame(polydf)%>% #Makes data frame 
  replace_na(list(polymer = "Not Reported")) %>%  
  mutate(effect = case_when(effect == "Y" ~ "Yes",
                            effect == "N" ~ "No")) %>% 
  filter(effect %in% c("Yes","No"))%>% #Sorts into Yes and No
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
  mutate(effect = case_when(effect == "Y" ~ "Yes",
                            effect == "N" ~ "No")) %>% 
  filter(effect %in% c("Yes","No"))%>% #Sorts into Yes and No
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
  mutate(effect = case_when(effect == "Y" ~ "Yes",
                            effect == "N" ~ "No")) %>% 
  filter(effect %in% c("Yes","No"))%>% #Sorts into Yes and No
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
  mutate(effect = case_when(effect == "Y" ~ "Yes",
                            effect == "N" ~ "No")) %>% 
  filter(effect %in% c("Yes","No"))%>% #Sorts into Yes and No
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
  replace_na(list(life.stage = "Not Reported")) %>% 
  mutate(effect = case_when(effect == "Y" ~ "Yes",
                            effect == "N" ~ "No")) %>% 
  filter(effect %in% c("Yes","No"))%>% #Sorts into Yes and No
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
  mutate(effect = case_when(effect == "Y" ~ "Yes",
                            effect == "N" ~ "No")) %>%
  filter(effect %in% c("Yes","No"))%>% #Sorts into Yes and No
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

# #Test Set up for plot type widget
# 
# #in vitro/in vivo by year and measurement
# vivodf_year_measurement<-rowPerc(xtabs(~invitro.invivo +year, human)) %>%
#   as.data.frame()%>%
#   filter(year!="Total") %>% #supress Total column to be able to cbind later
#   rename(Type= "invitro.invivo")%>%
#   mutate_if(is.numeric, round,0)%>%
#   mutate(plot="Invivo.invivo")%>%
#   mutate(Type = case_when(
#     Type=="invivo"~"In Vivo",
#     Type=="invitro"~"In Vitro"))
# study_v_year<-as.data.frame(xtabs(~invitro.invivo +year, human))
# vivoFinal_year<- data.frame(cbind(vivodf_year_measurement, study_v_year))%>%
#   rename(Endpoints='Freq.1')%>%
#   rename(category='invitro.invivo')%>%
#   mutate(logEndpoints = log(Endpoints))%>%
#   rename(Percent = Freq)#renames column
# 
# #in vitro/in vivo by year and study
# vivoFinal_year_study<-human %>%
#   group_by(invitro.invivo, year) %>%
#   summarize(studyCount = n_distinct(doi)) %>%
#   mutate(freq = 100 * studyCount / sum(studyCount)) %>%
#   as.data.frame()%>%
#   rename(Type= "invitro.invivo")%>%
#   mutate_if(is.numeric, round,0)%>%
#   mutate(plot="Invivo.invivo")%>%
#   mutate(Type = case_when(
#     Type=="invivo"~"In Vivo",
#     Type=="invitro"~"In Vitro")) %>%
#   rename(Studies='studyCount')%>%
#   mutate(logStudies = log(Studies))%>%
#   rename(Percent = freq)#renames column

#Set up for exposure route overview plot
routedf<-rowPerc(xtabs(~exposure.category +effect, human))
routef<-as.data.frame(routedf)%>%
  mutate(effect = case_when(effect == "Y" ~ "Yes",
                            effect == "N" ~ "No")) %>% 
  filter(effect %in% c("Yes","No"))%>% #Sorts into Yes and No
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

# # Set default theme for overview plots
# overviewTheme <- function(){
#   theme_classic() %+replace%
#     theme(text = element_text(size=17), plot.title = element_text(hjust = 0.5, face="bold",size=20),legend.position = "right",
#           axis.ticks= element_blank(),
#           axis.text.x = element_text(),
#           axis.text.y = element_blank(),
#           axis.title.x = element_blank() ) }

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
                                     lvl2 == "anxiety" ~ "Anxiety",
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
                                     lvl2 == "feeding" ~ "Feeding", 
                                     lvl2 == "firmicutes"~ "Firmicutes",
                                     lvl2 == "gen.stress" ~ "General Stress",
                                     lvl2 == "heart.tissue" ~ "Heart Tissue",
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
                                     lvl2 == "liver.kidney.products" ~ "Liver & Kidney Products",
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
                                     lvl2 == "predator.avoidance" ~ "Predator Avoidance",
                                     lvl2 == "proliferation" ~ "Proliferation",
                                     lvl2 == "proteobacteria"~"Protebacteria",
                                     lvl2 == "reproduction" ~ "Reproduction",
                                     lvl2 == "reproductive.tissue" ~ "Reproductive Tissues",
                                     lvl2 == "reproductive.endocrine.signaling" ~ "Reproductive Endocrine Signaling",
                                     lvl2 == "respiration"~"Respiration",
                                     lvl2 == "spleen.histo" ~ "Spleen Histological Abnormalities",
                                     lvl2 == "tenericutes" ~ "Tenericutes",
                                     lvl2 == "thyroid" ~ "Thyroid",
                                     lvl2 == "verrucomicrobiae" ~ "Verrucomicrobiae",
                                     lvl2 == "vision" ~ "Vision"))) %>% #Renames for widget
  #Level 3 Data Tidying
  mutate(lvl3_h_f = factor(case_when(lvl3 == "3hydroxyhexadecanoylcarnitine.con" ~ "3 Hydroxyhexadecanoylcarnitine Concentration",
                                     lvl3 == "3hydroxyhexadecenoylcarnitine.con" ~ "3 Hydroxyhexadecenoylcarnitine Concentration",
                                     lvl3 == "3hydroxyoctadecenoylcarnitine.con" ~ "3 Hydroxyoctadecenoylcarnitine Concentration",
                                     lvl3 == "3hydroxytetradecanoylcarnitine.con" ~ "3 Hydroxytetradecanoylcarnitine Concentration",
                                     lvl3 == "a.proteobacteria.genomicdna" ~ "a Proteobacteria Genomic DNA",
                                     lvl3 == "aat.proteinexpression" ~ "aat protein expression",
                                     lvl3 == "abcb11.mrnaexpression" ~ "abcb11 mRNA expression",
                                     lvl3 == "abp.mrnaexpression" ~ "abp mRNA expression",
                                     lvl3 == "acc.mrnaexpression" ~ "acc mRNA expression",
                                     lvl3 == "acetylcarnitine.con" ~ "Acetylcarnitine Concentration",
                                     lvl3 == "ache.activity" ~ "AchE Activity",
                                     lvl3 == "acl.mrnaexpression" ~ "acl mRNA expression",
                                     lvl3 == "acox.mrnaexpression" ~ "acox mRNA expression",
                                     lvl3 == "actinobacteria.genomicdna" ~ "Actinobacteria Genomic DNA",
                                     lvl3 == "adipylcarnite.con" ~ "Adipylcarnite Concentration",
                                     lvl3 == "akkermansia.geonmicdna" ~ "Akkermansia Genomic DNA",
                                     lvl3 == "alanine.con" ~ "Alanine Concentration",
                                     lvl3 == "alpha.smooth.muscle.actin.protein.expression.reproduction" ~ "Alpha Smooth Muscle Actin Protein Expression (Reproductive Tissue)",
                                     lvl3 == "alpha.smooth.muscle.actin.protein.expression.cardio" ~ "Alpha Smooth Muscle Actin Protein Expression (Cardiovascular Tissue)",
                                     lvl3 == "alt.con" ~ "Alanine Aminotransferase Concentration",
                                     lvl3 == "alternation" ~ "Alternation",
                                     lvl3 == "annexinv" ~ "Annexin V",
                                     lvl3 == "annexinv.binding" ~ "Annexin V Binding",
                                     lvl3 == "ano1.mrnaexpression" ~ "ano1 mRNA expression",
                                     lvl3 == "anti.mullerian.hormone.con" ~ "Anti Mullerian Hormone Concentration",
                                     lvl3 == "ap1.proteinexpression" ~ "ap1 protein expression",
                                     lvl3 == "apoptosis" ~ "Apoptosis",
                                     lvl3 == "arginine.con" ~ "Arginine Concentration",
                                     lvl3 == "asc.proteinexpression" ~ "ASC protein expression",
                                     lvl3 == "ast.con" ~ "Aspartate Transaminase Concentration",
                                     lvl3 == "atp.con" ~ "ATP Concentration",
                                     lvl3 == "bcatenin.proteinexpression.reproduction" ~ "Beta Catenin Protein Expression (Reproductive Tissue)",
                                     lvl3 == "p-bcatenin.proteinexpression.reproduction" ~ "Phosphorylated Beta Catenin Protein Expression (Reproductive Tissue)",
                                     lvl3 == "bcatenin.proteinexpression.cardio" ~ "Beta Catenin Protein Expression (Cardiovascular Tissue)",
                                     lvl3 == "p-bcatenin.proteinexpression.cardio" ~ "Phosphorylated Beta Catenin Protein Expression (Cardiovascular Tissue)",
                                     lvl3 == "bcl2.proteinexpression" ~ "BCL2 Protein Expression",
                                     lvl3 == "blood.testis.barrier.disruption" ~ "Blood Testes Barrier Disruption",
                                     lvl3 == "b.cells" ~ "B Cells",
                                     lvl3 == "b.proteobacteria.genomicdna" ~ "B Proteobacteria Genomic DNA",
                                     lvl3 == "bacteriodetes.genomicdna" ~ "Bacteriodetes Genomic DNA",
                                     lvl3 == "ballooning.degeneration" ~ "Ballooning Degeneration",
                                     lvl3 == "basophil.count" ~ "Basophil Count",
                                     lvl3 == "bax.bcl2.proteinexpression" ~ "BAX/BCL2 protein expression",
                                     lvl3 == "bax.proteinexpression" ~ "BAX protein expression",
                                     lvl3 == "beam.crossings" ~ "Beam Crossings",
                                     lvl3 == "bgalactosidase.staining" ~ "B Galactosidase Staining",
                                     lvl3 == "bileacid.con" ~ "Bile Acid Concentration",
                                     lvl3 == "blautia.genomicdna" ~ "Blautia Genomic DNA",
                                     lvl3 == "body.weight" ~ "Body Weight",
                                     lvl3 == "brain.histo" ~ "Brain Histology",
                                     lvl3 == "butyrylcarnitine.con" ~ "Butyrylcarnitine Concentration",
                                     lvl3 == "c0.c16.c18.ratio" ~ "C0/C16/C18 Ratio",
                                     lvl3 == "callogen.proteinexpression" ~ "Callogen Protein Expression",
                                     lvl3 == "callogen3.proteinexpression" ~ "Callogen 3 Protein Expression",
                                     lvl3 == "caspase.activation" ~ "Caspase Activation",
                                     lvl3 == "casp3.con" ~ "Caspase 3 Concentration",
                                     lvl3 == "caspase3.activity" ~ "Caspase 3 Activity",
                                     lvl3 == "caspase1.mrnaexpression" ~ "Caspase 1 mRNA expression",
                                     lvl3 == "caspase1.proteinexpression" ~ "Caspase 1 protein expression",
                                     lvl3 == "caspase3.proteinexpression" ~ "Caspase 3 protein expression",
                                     lvl3 == "caspase8.proteinexpression" ~ "Caspase 8 protein expression",
                                     lvl3 == "caspase9.proteinexpression" ~ "Caspase 9 protein expression",
                                     lvl3 == "cat.activity" ~ "Catalase Activity",
                                     lvl3 == "ccl22.mrnaexpression" ~ "ccl22 mRNA expression",
                                     lvl3 == "ccnd.mrnaexpression" ~ "ccnd mRNA expression",
                                     lvl3 == "ccnd1.mrnaexpression" ~ "ccnd1 mRNA expression",
                                     lvl3 == "ccne.mrnaexpresion" ~ "ccne mRNA expression",
                                     lvl3 == "ccne1.mrnaexpression" ~ "ccne1 mRNA expression",
                                     lvl3 == "cd11b-.cd11c+.ratio" ~ "CD11b-/CD11c+ Ratio",
                                     lvl3 == "cd11b.proteinexpression" ~ "CD11b protein expression",
                                     lvl3 == "cd11b+.cd11c-.ratio" ~ "CD11b+/CD11c- Ratio",
                                     lvl3 == "cd11b+.cd11c+.ratio" ~ "CD11b+/CD11c+ Ratio",
                                     lvl3 == "cd14.proteinexpression" ~ "CD14 protein expression",
                                     lvl3 == "cd163.proteinexpression" ~ "CD163 protein expression",
                                     lvl3 == "cd200r.proteinexpression" ~ "CD200R protein expression",
                                     lvl3 == "cd206.mrnaexpression" ~ "CD 206 mRNA expression",
                                     lvl3 == "cd209.mrnaexpression" ~ "CD209 mRNA expression",
                                     lvl3 == "cd4+.cd8+.ratio" ~ "CD4+/CD8+ Ratio",
                                     lvl3 == "cd45.proteinexpression" ~ "CD45 protein expression",
                                     lvl3 == "cd61.proteinexpression" ~ "CD61 protein expression",
                                     lvl3 == "cd86.proteinexpression" ~ "CD86 protein expression",
                                     lvl3 == "cdk4.mrnaexpression" ~ "CDK4 mRNA expression",
                                     lvl3 == "cell.adhesion" ~ "Cell Adhesion",
                                     lvl3 == "cell.aggregation.size" ~ "Cell Aggregate Size",
                                     lvl3 == "cell.aggregation.small" ~ "Cell Aggregation (Small)",
                                     lvl3 == "cell.cycle" ~ "Cell Cycle",
                                     lvl3 == "cell.cycle.g0g1" ~ "Cell Cycle G0G1 Phase",
                                     lvl3 == "cell.cycle.g2m" ~ "Cell Cycle G2M Phase",
                                     lvl3 == "cell.cycle.s" ~ "Cell Cycle S Phase",
                                     lvl3 == "cell.cycle.subg0g1" ~ "Cell Cycle Sub G0G1 Phase",
                                     lvl3 == "cell.membrane.dam" ~ "Cell Membrane Damage",
                                     lvl3 == "cell.morphology" ~ "Cell Morphology",
                                     lvl3 == "cellular.hemoglobin.con" ~ "Cellular Hemoglobin Concentration",
                                     lvl3 == "cftr.mrnaexpression" ~ "cftr mRNA expression",
                                     lvl3 == "cftr.proteinexpression" ~ "cftr protein expression",
                                     lvl3 == "chao.diversity.index" ~ "Chao Diversity Index",
                                     lvl3 == "cholesterol.con" ~ "Cholesterol Concentration",
                                     lvl3 == "chrebp.mrnaexpression" ~ "chrebp mRNA expression",
                                     lvl3 == "citrulline.con" ~ "Citrulline Concentration",
                                     lvl3 == "ckmb.con" ~ "Creatine Kinase Myocardial Band",
                                     lvl3 == "claudin11.proteinexpression" ~ "claudin11 protein expression",
                                     lvl3 == "cleaved.caspase1.proteinexpression" ~ "Cleaved Caspase 1 protein expression",
                                     lvl3 == "cleaved.caspase3.proteinexpression" ~ "Cleaved Caspase 3 protein expression",
                                     lvl3 == "cleaved.gsdmd.proteinexpression" ~ "Cleaved Gasdermin D protein expression",
                                     lvl3 == "claudin1.mrnaexpression" ~ "claudin1 mRNA expression",
                                     lvl3 == "clostridiales.genomicdna" ~ "Clostridiales Genomic DNA",
                                     lvl3 == "cluster.score" ~ "Cluster Score",
                                     lvl3 == "cmyc.mrnaexpression" ~ "cmyc mRNA expression",
                                     lvl3 == "coar.mrnaexpression" ~ "coar mRNA expression",
                                     lvl3 == "coas.mrnaexpression" ~ "coas mRNA expression",
                                     lvl3 == "collagenous.fiber.staining" ~ "Collagenous Fiber Staining",
                                     lvl3 == "complement.activation.bb" ~ "Complement Activation Bb",
                                     lvl3 == "complement.activation.c3a" ~ "Complement Activation C3a",
                                     lvl3 == "complement.activation.c4d" ~ "Complement Activation C4d",
                                     lvl3 == "complement.activation.sc5b9" ~ "Complement Activation sc5b9",
                                     lvl3 == "complement.activiation.c5b9" ~ "Complement Activation c5b9",
                                     lvl3 == "complement.activiation.ic3b" ~ "Complement Activation ic3b",
                                     lvl3 == "connexin43.proteinexpression" ~ "Connexin 43 protein expression",
                                     lvl3 == "ctnl.con" ~ "Cardiac Troponin I",
                                     lvl3 == "cpt1a.mrnaexpression" ~ "cpt1a mRNA expression",
                                     lvl3 == "creatine.con" ~ "Creatine Concentration",
                                     lvl3 == "cs.mrnaexpression" ~ "cs mRNA expression",
                                     lvl3 == "cxcl10.con" ~ "cxc10 concentration",
                                     lvl3 == "cxcl2.mrnaexpression" ~ "cxcl2 mRNA expression",
                                     lvl3 == "cxcl10.mrnaexpression" ~ "cxc10 mRNA expression",
                                     lvl3 == "cyclind.proteinexpression" ~ "Cylin D protein expression",
                                     lvl3 == "cyclind1.mrnaexpression" ~ "cyclin d1 mRNA expression",
                                     lvl3 == "cyclind3.proteinexpression" ~ "cyclin d3 protein expression",
                                     lvl3 == "cycline.proteinexpression" ~ "cyclin e protein expression",
                                     lvl3 == "cyp27a1.mrnaexpression" ~ "cyp27a1 mRNA expression",
                                     lvl3 == "cyp7a1.mrnaexpression" ~ "cyp7a1 mRNA expression",
                                     lvl3 == "cyp8b1.mrnaexpression" ~ "cyp8b1 mRNA expression",
                                     lvl3 == "cytochromec.proteinexpression" ~ "Cytochrome C protein expression",
                                     lvl3 == "cytotoxicity" ~ "Cytoxicity",
                                     lvl3 == "death.after.birth" ~ "Death After Birth",
                                     lvl3 == "decadienoylcarnitine.con" ~ "Decadienoylcarnitine Concentration",
                                     lvl3 == "decanoylcarnitine.con" ~ "Decanoylcarnitine Concentration",
                                     lvl3 == "decenoylcarnitine.con" ~ "decenoylcarnitine Concentration",
                                     lvl3 == "desulfovibrio.genomicdna" ~ "Desulfovibrio Genomic DNA",
                                     lvl3 == "dgat1.mrnaexpression" ~ "dgat1 mRNA expression",
                                     lvl3 == "dgat2.mrnaexpression" ~ "dgat2 mRNA expression",
                                     lvl3 == "d.lactate.con" ~ "D Lactate Concentration",
                                     lvl3 == "dao.activity" ~ "DAO Activity",
                                     lvl3 == "dazl.mrnaexpression" ~ "dazl mRNA expression",
                                     lvl3 == "dna.damage" ~ "DNA Damage",
                                     lvl3 == "dodecanoylcarnitine.con" ~ "Dodecanoylcarnitine Concentration",
                                     lvl3 == "dodecenoylcarnitine.con" ~ "Dodecenoylcarnitine Concentration",
                                     lvl3 == "dr5.proteinexpression" ~ "dr5 protein expression",
                                     lvl3 == "dubosiella.genomicdna" ~ "dubosiella Genomic DNA",
                                     lvl3 == "duodenum.histo" ~ "Duodenum Histology",
                                     lvl3 == "edema" ~ "Edema",
                                     lvl3 == "eosinophil.count" ~ "Eosinophil Count",
                                     lvl3 == "epididymis.histo" ~ "Epididymis Histology",
                                     lvl3 == "epididymis.somatic.index" ~ "Epididymis Somatic Index",
                                     lvl3 == "epithelial.cell.count" ~ "Epithelial Cell Count",
                                     lvl3 == "erk.mrnaexpression" ~ "erk mRNA expression",
                                     lvl3 == "erk1.mrnaexpression" ~ "erk1 mRNA expression",
                                     lvl3 == "erythema" ~ "Erythema",
                                     lvl3 == "fabp1.mrnaexpression" ~ "fabp1 mRNA expression",
                                     lvl3 == "fas.mrnaexpression" ~ "fas mRNA expression",
                                     lvl3 == "fat.index" ~ "Fat Index",
                                     lvl3 == "fat.mrnaexpression" ~ "fat mRNA expression",
                                     lvl3 == "fat.vacuoles" ~ "Fat Vacuoles",
                                     lvl3 == "fibronectin.proteinexpression.reproduction" ~ "Fibronectin protein expression (Reproductive Tissue)",
                                     lvl3 == "fibronectin.proteinexpression.cardio" ~ "Fibronectin protein expression (Cardiovascular Tissue)",
                                     lvl3 == "focaladhesionkinase.proteinexpression" ~ "Focal Adhesion Kinase protein expression",
                                     lvl3 == "folliclestimulatinghormone.con" ~ "Follicle Stimulating Hormone Concentration",
                                     lvl3 == "fsh.mrnaexpression" ~ "Follicle Stimulating Hormone mRNA expression",
                                     lvl3 == "food.intake" ~ "Food Intake",
                                     lvl3 == "fatp2.mrnaexpression" ~ "fatp2 mRNA expression",
                                     lvl3 == "firmicutes.genomicdna" ~ "Firmicutes Genomic DNA",
                                     lvl3 == "freecarnitine.con" ~ "Free Carnitine Concentration",
                                     lvl3 == "ft3.con" ~ "Free T3 Concentration",
                                     lvl3 == "ft3.ft4.ratio" ~ "Free T3/Free T4 Ratio",
                                     lvl3 == "ft4.con" ~ "Free T4 Concentration",
                                     lvl3 == "fumarylacetoacetatehydrolase.con" ~ "Fumarylacetoacetatehydrolase Concentration",
                                     lvl3 == "fxr.mrnaexpression" ~ "fxr mRNA expression",
                                     lvl3 == "gcsf.release" ~ "gcsf mRNA expression",
                                     lvl3 == "gestation.period" ~ "Gestation Period",
                                     lvl3 == "germinal.epithelium.height" ~ "Germinal Epithelium Cell Height",
                                     lvl3 == "gk.mrnaexpression" ~ "gk mRNA expression",
                                     lvl3 == "glucose.con" ~ "Glucose Concentration",
                                     lvl3 == "glut2.mrnaexpression" ~ "glut2 mRNA expression",
                                     lvl3 == "glutamicoxaloacetictransminase.con" ~ "Glutamicoxaloacetictransminase Concentration",
                                     lvl3 == "glutamicpyruvictransaminase.con" ~ "Glutamicpyruvictransaminase Concentration",
                                     lvl3 == "glutarylcarnitine.con" ~ "Glutarylcarnitine Concentration",
                                     lvl3 == "glycine.con" ~ "Glycine Concentration",
                                     lvl3 == "gpat.mrnaexpression" ~ "gpat mRNA expression",
                                     lvl3 == "gpx.activity" ~ "Glutathione Peroxidase Activity",
                                     lvl3 == "gsh.con" ~ "Glutathione Concentration",
                                     lvl3 == "gsdmd.proteinexpression" ~ "Gasdermin D protein expression",
                                     lvl3 == "gstp1.mrnaexpression" ~ "gstp1 mRNA expression",
                                     lvl3 == "gut.mucus.secretion" ~ "Gut Muscus Secretion",
                                     lvl3 == "heart.histo" ~ "Heart Histology",
                                     lvl3 == "heart.rate" ~ "Heart Rate",
                                     lvl3 == "hematocrit" ~ "Hematocrit",
                                     lvl3 == "hemoglobin.con" ~ "Hemoglobulin Concentration",
                                     lvl3 == "hemoglobin.distribution.width" ~ "Hemoglobulin Distribution Width",
                                     lvl3 == "hemolysis" ~ "Hemolysis",
                                     lvl3 == "hexadecenoylcarnitine.con" ~ "Hexadecenoylcarnitine Concentration",
                                     lvl3 == "hexanoylcarnitine.con" ~ "Hexanoylcarnitine Concentration",
                                     lvl3 == "hexaoylcarnitine.con" ~ "Hexaoylcarnitine Concentration",
                                     lvl3 == "highdensitylipoprotein.con" ~ "HDL Concentration",
                                     lvl3 == "histamine.release" ~ "Histamine Release",
                                     lvl3 == "histone.acetylation" ~ "Histone Acetylation",
                                     lvl3 == "ho1.mrnaexpression" ~ "ho1 mRNA expression",
                                     lvl3 == "ho1.proteinexpression" ~ "ho1 protein expression",
                                     lvl3 == "hsp70.mrnaexpression" ~ "hsp70 mRNA expression",
                                     lvl3 == "ic3b.con" ~ "iC3b Concentration",
                                     lvl3 == "ifny.con" ~ "Interferon y Concentration",
                                     lvl3 == "IgA.con" ~ "IgA Concentration",
                                     lvl3 == "IgE.con" ~ "IgE Concentration",
                                     lvl3 == "IgG.con" ~ "IgG Concentration",
                                     lvl3 == "IgM.con" ~ "IgM Concentration",
                                     lvl3 == "ikba.proteinexpression" ~ "ikba protein expression",
                                     lvl3 == "il1.mrnaexpression" ~ "il1 mRNA expression",
                                     lvl3 == "il1.release" ~ "IL1 Release",
                                     lvl3 == "il10.release" ~ "IL10 Release",
                                     lvl3 == "il1a.release" ~ "IL1a Release",
                                     lvl3 == "il1b.mrnaexpression" ~ "IL1b mRNA expression",
                                     lvl3 == "il1b.proteinexpression" ~ "IL1b protein expression",
                                     lvl3 == "il1b.release" ~ "IL1b Release",
                                     lvl3 == "il1b.con" ~ "IL1b Concentration",
                                     lvl3 == "il2.release" ~ "IL2 Release",
                                     lvl3 == "il5.release" ~ "IL5 Release",
                                     lvl3 == "il6.proteinexpression" ~ "IL6 protein expression",
                                     lvl3 == "il6.mrnaexpression" ~ "IL6 mRNA expression",
                                     lvl3 == "il6.release" ~ "IL6 Release",
                                     lvl3 == "il6.con" ~ "Interleukin 6 Concentration",
                                     lvl3 == "il8.mrnaexpression" ~ "IL8 mRNA expression",
                                     lvl3 == "il8.proteinexpression" ~ "IL8 protein expression",
                                     lvl3 == "il18.proteinexpression" ~ "IL18 protein expression",
                                     lvl3 == "il8.release" ~ "IL8 Release",
                                     lvl3 == "il8.con" ~ "IL8 Concentration",
                                     lvl3 == "il18.con" ~ "IL18 Concentration",
                                     lvl3 == "il9.release" ~ "IL9 Release",
                                     lvl3 == "immune.cell.proliferation" ~ "Immune Cell Proliferation",
                                     lvl3 == "immune.cell.protein.con" ~ "Immune Cell Protein Concentration",
                                     lvl3 == "inflammatory.cell.infiltration" ~ "Inflammatory Cell Infiltration",
                                     lvl3 == "intestinal.permeability" ~ "Intestinal Permeability",
                                     lvl3 == "intestinal.tissue.inflammation" ~ "Intestinal Tissue Inflammation",
                                     lvl3 == "inos.mrnaexpression" ~ "iNOS mRNA expression",
                                     lvl3 == "intracellular.ca.level" ~ "Intracellular Calcium Levels",
                                     lvl3 == "invariant.natural.killer.cells" ~ "Invariant Natural Killer Cells",
                                     lvl3 == "irf5.proteinexpression" ~ "irf5 protein expression",
                                     lvl3 == "isovalerylcarnitine.con" ~ "Isovalerylcarnitine Concentration",
                                     lvl3 == "keap1.proteinexpression" ~ "keap1 protein expression",
                                     lvl3 == "kidney.tissue.inflammation" ~ "Kidney Tissue Inflammation",
                                     lvl3 == "ki67.mrnaexpression" ~ "ki67 mRNA expression",
                                     lvl3 == "kidney.histo" ~ "Kidney Histology",
                                     lvl3 == "klf4.mrnaexpression" ~ "klf4 mRNA expression",
                                     lvl3 == "lactobacillus.genomicdna" ~ "Lactobacillus Genomic DNA",
                                     lvl3 == "large.intestine.histo" ~ "Large Intestine Histology",
                                     lvl3 == "ldh.activity" ~ "Lactate Dehydrogenase Activity",
                                     lvl3 == "ldh.con" ~ "Lactate Dehydrogenase Concentration",
                                     lvl3 == "ldh.release" ~ "Lactate Dehydrogenase Release",
                                     lvl3 == "leucine.con" ~ "Leucine Concentration",
                                     lvl3 == "leukocyte.adhesion" ~ "Leukocyte Adhesion",
                                     lvl3 == "lh.mrnaexpression"~ "lh mRNA expression",
                                     lvl3 == "linoleylcarnitine.con" ~ "Linoleylcarnitine Concentration",
                                     lvl3 == "lipid.droplets" ~ "Lipid Droplets",
                                     lvl3 == "liver.histo" ~ "Liver Histology",
                                     lvl3 == "liver.index" ~ "Liver Index",
                                     lvl3 == "liver.tissue.inflammation" ~ "Liver Tissue Inflammation",
                                     lvl3 == "lowdensitylipoprotein.con" ~ "Low Density Lipoprotein Concentration",
                                     lvl3 == "lung.histo" ~ "Lung Histology",
                                     lvl3 == "lung.tissue.inflammation" ~ "Lung Tissue Inflammation",
                                     lvl3 == "luteinizinghormone.con" ~ "Luteinizinghormone Concentration",
                                     lvl3 == "lymphocyte.count" ~ "Lymphocyte Count",
                                     lvl3 == "lysosomal.integrity" ~ "Lysosomal Integrity",
                                     lvl3 == "lysosomal.protonation" ~ "Lyosomal Protonation",
                                     lvl3 == "macrophage.cell.count" ~ "Macrophage Count",
                                     lvl3 == "macrophage.polarization" ~ "Macrophage Polarization",
                                     lvl3 == "malonylcarnitine.con" ~ "Malonylcarnitine Concentration",
                                     lvl3 == "mcad.mrnaexpression" ~ "mcad mRNA expression",
                                     lvl3 == "mcp1.con" ~ "mcp1 Concentration",
                                     lvl3 == "mcp1.mrnaexpression" ~ "mcp1 mRNA expression",
                                     lvl3 == "mda.activity" ~ "Malondialdehyde Activity",
                                     lvl3 == "mda.con" ~ "Malondialdehyde Concentration",
                                     lvl3 == "mean.corpuscular.hemoglobin" ~ "Mean Corpuscular Hemoglobin",
                                     lvl3 == "mean.corpuscular.hemoglobin.con" ~ "Mean Corpuscular Hemoglobin Concentration",
                                     lvl3 == "mean.corpuscular.volume" ~ "Mean Corpuscular Volume",
                                     lvl3 == "mean.platelet.volume" ~ "Mean Platelet Volume",
                                     lvl3 == "mek.mrnaexpression" ~ "mek mRNA expression",
                                     lvl3 == "melainabacteria.genomicdna" ~ "Melainabacteria Genomic DNA",
                                     lvl3 == "mem.il12.release" ~ "Membrane IL12 Release",
                                     lvl3 == "mem.il1b.release" ~ "Membrane IL1B Release",
                                     lvl3 == "mem.il2.release" ~ "Membrane IL2 Release",
                                     lvl3 == "mem.tnfa.release" ~ "Membrane TNFa Release",
                                     lvl3 == "meprinb.mrnaexpression" ~ "meprinb mRNA expression",
                                     lvl3 == "methionine.con" ~ "Methionine Concentration",
                                     lvl3 == "methylmalonic.con" ~ "Methylmalonic Concentration",
                                     lvl3 == "mice.in.predator.field.of.view" ~ "Within Predator Field of View",
                                     lvl3 == "micronuclei.freq" ~ "Micronuclei Frequency",
                                     lvl3 == "mitochondria.membrane.potential" ~ "Mitochondrial Membrane Potential",
                                     lvl3 == "mitochondria.num" ~ "Number of Mitochondria",
                                     lvl3 == "mitochondrial.ros.prod" ~ "Mitochondria ROS Production",
                                     lvl3 == "monocyte.adhesion" ~ "Monocyte Adhesion",
                                     lvl3 == "monocyte.count" ~ "Monocyte Count",
                                     lvl3 == "mortality" ~ "Mortality",
                                     lvl3 == "mrp2.mrnaexpression" ~ "mrp2 mRNA expression",
                                     lvl3 == "mrp3.mrnaexpression" ~ "mrp3 mRNA expression",
                                     lvl3 == "mtp.mrnaexpression" ~ "mtp mRNA expression",
                                     lvl3 == "muc1.mrnaexpression" ~ "muc1 mRNA expression",
                                     lvl3 == "muc2.mrnaexpression" ~ "muc2 mRNA expression",
                                     lvl3 == "muc3.mrnaexpression" ~ "muc mRNA expression",
                                     lvl3 == "muribaculum.genomicdna" ~ "Muribaculum Genomic DNA",
                                     lvl3 == "nadh.levels" ~ "NADH Levels",
                                     lvl3 == "natural.killer.cells" ~ "Natural Killer Cell Count",
                                     lvl3 == "ncadherin.proteinexpression" ~ "N-cadherin protein expression",
                                     lvl3 == "neutrophil.count" ~ "Neutrophil Count",
                                     lvl3 == "nfkb.mrnaexpression" ~ "nfkB mRNA expression",
                                     lvl3 == "nfkb.proteinexpression" ~ "nfkB protein expression",
                                     lvl3 == "nfkb1.mrnaexpression" ~ "nfkb1 mRNA expression",
                                     lvl3 == "nfkbp75.proteinexpression" ~ "nfkbp75 protein expression",
                                     lvl3 == "nhe3.mrnaexpression" ~ "nhe3 mRNA expression",
                                     lvl3 == "nkcc1.mrnaexpression" ~ "nkcc1 mRNA expression",
                                     lvl3 == "nkcc1.proteinexpression" ~ "nkcc1 protein expression",
                                     lvl3 == "nlrp3.proteinexpression" ~ "nlrp3 protein expression",
                                     lvl3 == "nonestrifiedfattyacid.con" ~ "Nonestrified Fatty Acid Concentration",
                                     lvl3 == "nos2.proteinexpression" ~ "nos2 protein expression",
                                     lvl3 == "nqo1.proteinexpression" ~ "nqo1 protein expression",
                                     lvl3 == "nrf2.mrnaexpression" ~ "nrf2 mRNA expression",
                                     lvl3 == "nrf2.proteinexpression" ~ "nrf2 protein expression",
                                     lvl3 == "ntcp.mrnaexpression" ~ "ntcp mRNA expression",
                                     lvl3 == "nuclear.intensity" ~ "Nuclear Intensity",
                                     lvl3 == "nuclear.size" ~ "Nuclear Size",
                                     lvl3 == "nucleaus.cytoplasm.ratio" ~ "Nucleaus Cytoplasm Ratio",
                                     lvl3 == "number.of.growing.follicles" ~ "Number of Growing Follicles",
                                     lvl3 == "number.of.live.births" ~ "Number of Live Births",
                                     lvl3 == "occludin.proteinexpression" ~ "Occludin protein expression",
                                     lvl3 == "octanoylcarnitine.con" ~ "Octanoylcarnitine Concentration",
                                     lvl3 == "octenoylcarnitine.con" ~ "Octenoylcarnitine Concentration",
                                     lvl3 == "odoribacter.genomicdna" ~ "Odoribacter Genomic DNA",
                                     lvl3 == "oleylcarnitine.con" ~ "Oleylcarnitine Concentration",
                                     lvl3 == "open.arm.entries" ~ "Open Arm Entries",
                                     lvl3 == "open.arm.time" ~ "Open Arm Time",
                                     lvl3 == "open.field.test.anxiety.index" ~ "Anxiety Index (Open Field Test)",
                                     lvl3 == "open.field.test.distance.traveled" ~ "Distance Traveled (Open Field Test)",
                                     lvl3 == "open.field.test.locomotion.speed" ~ "Locomotion Speed (Open Field Test)",
                                     lvl3 == "opsonization" ~ "Opsonization",
                                     lvl3 == "ornithine.con" ~ "Ornithine Concentration",
                                     lvl3 == "ovary.histo" ~ "Ovary Histology",
                                     lvl3 == "p-ikba.proteinexpression" ~ "p-ikba protein expression",
                                     lvl3 == "p-nfkbp75.proteinexpression" ~ "p-nfkbp75 protein expression",
                                     lvl3 == "p38.mrnaexpression" ~ "p38 mRNA expression",
                                     lvl3 == "p38.phosphorylation" ~ "p38 Phosphorylation",
                                     lvl3 == "p38.proteinexpression" ~ "p38 protein expression",
                                     lvl3 == "p53.mrnaexpression" ~ "p53 mRNA expression",
                                     lvl3 == "p53.proteinexpression" ~ "p53 protein expression",
                                     lvl3 == "p70s6k.proteinexpression" ~ "p70s6k protein expression",
                                     lvl3 == "palmitoylcarnitine.con" ~ "Palmitoylcarnitine Concentration",
                                     lvl3 == "parabacteroides.genomicdna" ~ "Parabacteroides Genomic DNA",
                                     lvl3 == "patescibacteria.genomicdna" ~ "Patescibacteria Genomic DNA",
                                     lvl3 == "p-bcatenin.proteinexpression" ~ "p-Bcatenin protein expression",
                                     lvl3 == "pge2.release" ~ "pge2 Release",
                                     lvl3 == "phagocytosis" ~ "Phagocytosis",
                                     lvl3 == "phenylalanine.con" ~ "Phenylalanine Concentration",
                                     lvl3 == "p-ikba.proteinexpression" ~ "p-ikba protein expression",
                                     lvl3 == "pk.mrnaexpression" ~ "pk mRNA expression",
                                     lvl3 == "platelet.count" ~ "Platelet Count",
                                     lvl3 == "plzf.mrnaexpression" ~ "plzf mRNA expression",
                                     lvl3 == "pnfkb.nfkb.ratio.proteinexpression" ~ "p-nfkB/nfkB Ratio protein expression",
                                     lvl3 == "p-nfkb.proteinexpression" ~ "p-nfkB protein expression",
                                     lvl3 == "p-p38.proteinexpression" ~ "p-p38 protein expression",
                                     lvl3 == "ppara.mrnaexpression" ~ "ppar a mRNA expression",
                                     lvl3 == "pparr.mrnaexpression" ~ "ppar r mRNA expression",
                                     lvl3 == "ppary.mrnaexpression" ~ "ppar y mRNA expression",
                                     lvl3 == "ppary.con" ~ "ppar y Concentration",
                                     lvl3 == "proliferation" ~ "Proliferation",
                                     lvl3 == "immune.cell.proliferation" ~ "Immune Cell Proliferation",
                                     lvl3 == "proline.con" ~ "Proline Concentration",
                                     lvl3 == "propionylcarnitine.con" ~ "Propionylcarnitine Concentration",
                                     lvl3 == "protein.con" ~ "Protein Concentration",
                                     lvl3 == "immune.cell.protein.con" ~ "Immune Cell Protein Concentration",
                                     lvl3 == "proteobactria.genomicdna" ~ "Proteobactria Genomic DNA",
                                     lvl3 == "pulmonary.arterial.pressure" ~ "Pulmonary Arterial Pressure",
                                     lvl3 == "pyruvate.con" ~ "Pyruvate Concentration",
                                     lvl3 == "rantes.release" ~ "RANTES Release",
                                     lvl3 == "ras.mrnaexpression" ~ "ras mRNA expression",
                                     lvl3 == "red.blood.cell.count" ~ "Red Blood Cell Count",
                                     lvl3 == "red.blood.cell.distribution.width" ~ "Red Blood Cell Distribution Width",
                                     lvl3 == "renlb.mrnaexpression" ~ "renlb mRNA expression",
                                     lvl3 == "respiration.rate" ~ "Respiration Rate",
                                     lvl3 == "reticulocyte" ~ "Reticulocyte",
                                     lvl3 == "retnlb.mrnaexpression" ~ "retnlb mRNA expression",
                                     lvl3 == "ros.prod" ~ "ROS Production",
                                     lvl3 == "sc5b9.con" ~ "sc5b9 Concentration",
                                     lvl3 == "scd1.mrnaexpression" ~ "scd1 mRNA expression",
                                     lvl3 == "sdh.activity" ~ "Succinate dehydrogenase Activity",
                                     lvl3 == "seminal.vesicle.histo" ~ "Seminal Vesicle Histology",
                                     lvl3 == "seminiferous.tubules.diameter" ~ "Seminiferous Tubules Diameter",
                                     lvl3 == "sex.ratio" ~ "Sex Ratio",
                                     lvl3 == "shaking.frequency" ~ "Shaking Frequency",
                                     lvl3 == "shannon.index" ~ "Shannon Index",
                                     lvl3 == "slc26a3.mrnaexpression" ~ "slc26a3 mRNA expression",
                                     lvl3 == "slc26a6.mrnaexpression" ~ "slc26a6 mRNA expression",
                                     lvl3 == "slc26a6.proteinexpression" ~ "slc26a6 protein expression",
                                     lvl3 == "small.intestine.histo" ~ "Small Intestine Histology",
                                     lvl3 == "sod.activity" ~ "SOD Activity",
                                     lvl3 == "sod2.mrnaexpression" ~ "sod2 mRNA expression",
                                     lvl3 == "sperm.count" ~ "Sperm Count",
                                     lvl3 == "sperm.deformity" ~ "Sperm Deformity",
                                     lvl3 == "sperm.dna.damage" ~ "Sperm DNA damage",
                                     lvl3 == "sperm.maturity" ~ "Sperm Maturity",
                                     lvl3 == "sperm.motility" ~ "Sperm Motility",
                                     lvl3 == "sperm.viability" ~ "Sperm Viability",
                                     lvl3 == "spermatogenic.cell.con" ~ "Spermatogenic Cell Concentration",
                                     lvl3 == "spleen.histo" ~ "Spleen Histology",
                                     lvl3 == "spleen.tissue.inflammation" ~ "Spleen Tissue Inflammation",
                                     lvl3 == "srebp1c.mrnaexpression" ~ "srebp1c mRNA expression",
                                     lvl3 == "staphylococcus.genomicdna" ~ "Staphylococcus Genomic DNA",
                                     lvl3 == "stat1.phosphorylation" ~ "stat1 Phosphorylation",
                                     lvl3 == "stat6.phosphorylation" ~ "stat6 Phosphorylation",
                                     lvl3 == "stearoylcarnitine.con" ~ "Stearoylcarnitine Concentration",
                                     lvl3 == "stomach.histo" ~ "Stomach Histology",
                                     lvl3 == "succinylacetone.con" ~ "Succinylacetone Concentration",
                                     lvl3 == "suzuki.score" ~ "Suzuki Score",
                                     lvl3 == "t.cells" ~ "T Cell Count",
                                     lvl3 == "t3.con" ~ "T3 Concentration",
                                     lvl3 == "t3.t4.ratio" ~ "T3/T4 Ratio",
                                     lvl3 == "t4.con" ~ "T4 Concentration",
                                     lvl3 == "tenericutes.genomicdna" ~ "Tenericutes Genomic DNA",
                                     lvl3 == "testis.histo" ~ "Testis Histology",
                                     lvl3 == "testis.somatic.index" ~ "Testis Somatic Index",
                                     lvl3 == "testicular.capsule.diameter" ~ "Testicular Capsule Diameter",
                                     lvl3 == "testosterone.con" ~ "Testosterone Concentration",
                                     lvl3 == "tetradecadienoylcarnitine.con" ~ "Tetradecadienoylcarnitine Concentration",
                                     lvl3 == "tetradecanocarnitine.con" ~ "Tetradecanocarnitine Concentration",
                                     lvl3 == "tetradecenoylcarnitine.con" ~ "Tetradecenoylcarnitine Concentration",
                                     lvl3 == "tf.proteinexpression" ~ "tf protein expression",
                                     lvl3 == "tgfb.proteinexpression" ~ "tbfb protein expression",
                                     lvl3 == "tgfb1.mrnaexpression" ~ "tgfb1 mRNA expression",
                                     lvl3 == "tgfb1.release" ~ "tgfb1 Release",
                                     lvl3 == "th17.cell.count" ~ "TH17 Cell Count",
                                     lvl3 == "th17.treg.ratio" ~ "TH17/Treg Ratio",
                                     lvl3 == "thymus.histo" ~ "Thymus Histology",
                                     lvl3 == "tiglylcarnitine.con" ~ "Tiglycarnitine Concentration",
                                     lvl3 == "time.in.dark" ~ "Time in Dark",
                                     lvl3 == "time.rotarod" ~ "Time Rotarod",
                                     lvl3 == "intestinal.tissue.inflammation" ~ "Intestinal Tissue Inflammation",
                                     lvl3 == "kidney.tissue.inflammation" ~ "Kidney Tissue Inflammation",
                                     lvl3 == "liver.tissue.inflammation" ~ "Liver Tissue Inflammation",
                                     lvl3 == "vision.tissue.inflammation" ~ "Vision Tissue Inflammation",
                                     lvl3 == "lung.tissue.inflammation" ~ "Lung Tissue Inflammation",
                                     lvl3 == "spleen.tissue.inflammation" ~ "Spleen Tissue Inflammation",
                                     lvl3 == "tlr4.proteinexpression" ~ "tlr4 protein expression",
                                     lvl3 == "tnfa.mrnaexpression" ~ "tnfa mRNA expression",
                                     lvl3 == "tnfa.proteinexpression" ~ "tnfa protein expression",
                                     lvl3 == "tnfa.release" ~ "TNFa Release",
                                     lvl3 == "tnfa.con" ~ "TNFa Concentration",
                                     lvl3 == "total.arm.entries" ~ "Total Arm Entries",
                                     lvl3 == "total.arm.entries.5min" ~ "TOtal Arm Entries (5 min)",
                                     lvl3 == "transepithelial.elec.res.respiratory" ~ "Transepithelial Electric Resistance (Respiratory)",
                                     lvl3 == "transepithelial.elec.res.intestinal" ~ "Transepithelial Electric Resistance (Intestinal)",
                                     lvl3 == "transepithelial.elec.res" ~ "Transepithelial Electric Resistance",
                                     lvl3 == "treg.cell.count" ~ "Treg Count",
                                     lvl3 == "triglyceride.con" ~ "Triglyceride Concentration",
                                     lvl3 == "trpv1.mrnaexpression" ~ "trpv1 mRNA expression",
                                     lvl3 == "troponin1.con" ~ "Troponin 1 Concentration",
                                     lvl3 == "tsh.con" ~ "TSH Concentration",
                                     lvl3 == "tunel.staining" ~ "TUNEL staining",
                                     lvl3 == "tyrosine.con" ~ "Tyrosine Concentration",
                                     lvl3 == "unifrac.diversity" ~ "Unifrac Diversity",
                                     lvl3 == "uterus.histo" ~ "Uterus Histology",
                                     lvl3 == "valine.con" ~ "Valine Concentration",
                                     lvl3 == "verrucomicrobia.genomicdna" ~ "Verrucomicrobia Genomic DNA",
                                     lvl3 == "vision.tissue.inflammation" ~ "",
                                     lvl3 == "water.intake" ~ "Water Intake",
                                     lvl3 == "weight.gain" ~ "Weight Gain",
                                     lvl3 == "white.blood.cell.count" ~ "White Blood Cell Count",
                                     lvl3 == "wnt.proteinexpression" ~ "wnt protein expression",
                                     lvl3 == "y.proteobacteria.genomicdna" ~ "y Proteobacteria Genomic DNA",
                                     lvl3 == "zo1.mrnaexpression" ~ "zo1 mRNA expression",
                                     lvl3 == "zo1.proteinexpression" ~ "zo1 protein expression"))) %>% 
  mutate(bio_h_f = factor(case_when(bio.org == "cell"~"Cell", #Renames for widget
                                    bio.org == "organism"~"Organism",
                                    bio.org == "subcell"~"Subcell",
                                    bio.org == "tissue" ~ "Tissue")))%>%
  mutate(vivo_h_f = factor(case_when(invitro.invivo == "invivo"~"In Vivo",
                                     invitro.invivo == "invitro"~"In Vitro")))%>% ##Renames for widget 
  mutate(life_h_f = factor(case_when(life.stage == "early,f1"~"Early, F1 Generation",
                                     life.stage == "early,f2"~"Early, F2 Generation",
                                     life.stage == "adult"~"Adult",
                                     life.stage == "Not Reported"~"Not Reported")))%>% #Renames for widget
  mutate(exposure_route_h_f = factor(case_when(exposure.route == "drinking.water" ~ "Drinking Water",
                                               exposure.route == "food" ~ "Food",
                                               exposure.route == "gavage" ~ "Gavage",
                                               exposure.route == "gestation" ~ "Gestation",
                                               exposure.route == "gestation,lactation" ~ "Gestation & Lactation",
                                               exposure.route == "Not Applicable"~"In Vitro",
                                               exposure.route == "iv.injection" ~ "IV Injection",
                                               exposure.route == "inhalation" ~ "Inhalation",
                                               exposure.route == "intratracheal.instillation" ~ "Intratracheal Instillation")))%>%
  mutate(species_h_f = factor(case_when(species == "aries"~"(Sheep) Ovis aries",
                                        species == "sapiens"~"(Human) Homo sapiens",
                                        species == "musculus"~"(Mouse) Mus musculus",
                                        species == "cuniculus"~"(Rabbit) Oryctolagus cuniculus",
                                        species == "domesticus" ~ "(Pig) Sus domesticus",
                                        species == "norvegicus"~"(Rat) Rattus norvegicus"))) %>% 
  #Make factor for experiment type
  mutate(exp_type_f = factor(if_else(is.na(chem), "Particle Only", "Chemical Co-Exposure"))) %>% 
  
  #Mass
  mutate(dose.ug.mL.master = dose.mg.mL.master/1000) %>% 
  
  #Particles
  mutate(dose.particles.mL.master = dose.particles.L.master/1000) %>% 
  
  #Volume
  mutate(dose.um3.mL.master = (particle.volume.um * dose.particles.L.master)/1000) %>%  #calculate volume/mL
  
  # #Surface Area
  # mutate(dose.um2.mL.master = as.numeric(particle.surface.area.um2) * dose.particles.mL.master) %>% 
  # 
  # #Specific Surface Area
  # mutate(dose.um2.ug.mL.master = dose.um2.mL.master / (mass.per.particle.mg / 1000)) %>% #correct mg to ug
  
  #Additional tidying for nicer values
  mutate(authors = gsub(".", " & ", as.character(authors), fixed = TRUE)) %>% 
  mutate(exposure.media = gsub(".", " ", as.character(exposure.media), fixed = TRUE)) %>%
  mutate(detergent = gsub(".", " ", as.character(detergent), fixed = TRUE)) %>%
  mutate(chem = gsub(".", " ", as.character(chem), fixed = TRUE)) %>%
  mutate(exposure.route = gsub(".", " ", as.character(exposure.route), fixed = TRUE)) %>% 
  mutate(sol.rinse = gsub(".", " ", as.character(sol.rinse), fixed = TRUE)) %>%
  mutate(sol.rinse = if_else(sol.rinse == "N", "No", sol.rinse)) %>% 
  mutate(clean.method = gsub(".", " ", as.character(clean.method), fixed = TRUE)) %>% 
  mutate(clean.method = if_else(clean.method == "N", "Not Cleaned", clean.method)) %>% 
  mutate(particle.behavior = gsub(".", " ", as.character(particle.behavior), fixed = TRUE)) %>% 
  mutate(particle.behavior = if_else(particle.behavior == "N", "Not Evaluated", particle.behavior)) %>%
  mutate(tissue.distribution = gsub(".", " ", as.character(tissue.distribution), fixed = TRUE)) %>%   
  
  #Create factors for red criteria screening
  mutate(particle_red_criteria = factor(case_when(
    is.na(particle.1) ~ "Scoring Not Available",
    particle.1 == 0|particle.2 == 0|particle.3 == 0|particle.4 == 0 ~ "Fail",
    particle.1 != 0 & particle.2 != 0 & particle.3 != 0 & particle.4 != 0 ~ "Pass"))) %>%
  mutate(design_red_criteria = factor(case_when(
    is.na(design.1) ~ "Scoring Not Available",
    design.3 == 0|design.4 == 0|design.6 == 0|design.7 == 0|design.9 == 0|design.10 == 0|design.11 == 0 ~ "Fail",
    design.3 != 0 & design.4 != 0 & design.6 != 0 & design.7 != 0 & design.9 != 0 & design.10 != 0 & design.11 != 0 ~ "Pass"))) %>% 
  mutate(risk_red_criteria = factor(case_when(
    is.na(risk.1) ~ "Scoring Not Available",
    risk.2 == 0|risk.3 == 0|risk.5 == 0 ~ "Fail",
    risk.2 != 0 & risk.3 != 0 & risk.5 != 0 ~ "Pass")))  

#### Endpoint Category Setup ####

human_endpoint <- human_setup %>% 
  group_by(vivo_h_f, lvl1_h_f,lvl2_h_f,lvl3_h_f,bio_h_f) %>% 
  summarise()

#### Search Setup ####

human_search <- human_setup %>%
  #general
  dplyr::select(doi, authors, year, particle_red_criteria, design_red_criteria, risk_red_criteria, species_h_f, life_h_f, vivo_h_f, sex,
                #experimental parameters
                exp_type_f, exposure_route_h_f, mix, negative.control, reference.material, exposure.media, solvent, detergent,
                media.ph, media.sal, media.temp, media.temp.min, media.temp.max, exposure.duration.d, 
                treatment, replicates, sample.size, dosing.frequency, chem, chem.dose.ug.L.nominal, chem.dose.ug.L.measured, 
                chem.dose.umol.kg.bw.day, chem.dose.uM, 
                #reported doses
                dose.ug.g.food.nominal, dose.uM.nominal, dose.ug.cm2.nominal, dose.particles.day.nominal, dose.particles.kg.bw.nominal,
                dose.mg.day.nominal, dose.particles.L.nominal, dose.mg.L.air.nominal, dose.mg.kg.day.bw.nominal, 
                dose.cm2.mL.nominal, 
                dose.ug.g.food.measured, dose.uM.measured, dose.ug.cm2.measured, dose.particles.day.measured, 
                dose.mg.day.measured, dose.particles.L.measured, dose.mg.L.air.measured, dose.mg.kg.day.bw.measured, 
                dose.cm2.mL.measured,
                #master doses
                dose.particles.mL.master, dose.particles.L.master.reported.converted, dose.ug.mL.master, dose.mg.mL.master.reported.converted,
                dose.um3.mL.master, 
                #biological effects
                effect_h_f, direction, lvl1_h_f, lvl2_h_f, lvl3_h_f, bio_h_f, target.organelle.cell.tissue, 
                #particle characteristics
                poly_h_f, shape_h_f, density.g.cm3, density.reported.estimated, charge, zetapotential, zeta.potential.media, functional.group,
                size.length.um.used.for.conversion, size_h_f, particle.volume.um,
                mass.per.particle.mg, weathered.biofouled,
                #quality
                size.valid, polymer.valid, shape.valid, particle.source, sodium.azide, contaminant.screen, clean.method, sol.rinse, background.plastics,
                con.valid, particle.behavior, uptake.valid, tissue.distribution, fed) %>%  
  #rename 'master' dose columns so they don't get pivoted
  rename("particles/mL (master)" = dose.particles.mL.master, "particles/mL (master), reported or converted" = dose.particles.L.master.reported.converted,
         "μg/mL (master)" = dose.ug.mL.master, "μg/mL (master), reported or converted" = dose.mg.mL.master.reported.converted,
         "μm^3/mL (master)" = dose.um3.mL.master) %>% 
  #pivot non-master dose columns
  pivot_longer(cols = starts_with("dose"),
               names_to = "Original Dose Units",
               values_to = "Original Concentration") %>%  
  mutate(`Original Dose Units Nominal or Measured` = case_when(grepl("nominal", `Original Dose Units`) ~ "nominal",
                                                               grepl("measured", `Original Dose Units`) ~ "measured")) %>%   
  mutate(`Original Dose Units` = case_when(grepl("dose.ug.g.food", `Original Dose Units`) ~ "μg/g (food)",
                                           grepl("dose.uM", `Original Dose Units`) ~ "uM",
                                           grepl("dose.ug.cm2", `Original Dose Units`) ~ "μg/cm^2",
                                           grepl("dose.particles.day", `Original Dose Units`) ~ "particles/day",
                                           grepl("dose.particles.kg.bw", `Original Dose Units`) ~ "particles/kg (body weight)",
                                           grepl("dose.mg.day", `Original Dose Units`) ~ "mg/day",
                                           grepl("dose.particles.L", `Original Dose Units`) ~ "particles/L",
                                           grepl("dose.mg.L.air", `Original Dose Units`) ~ "mg/L (air)",
                                           grepl("dose.mg.kg.day.bw", `Original Dose Units`) ~ "mg/kg (body weight)/day",
                                           grepl("dose.cm2.mL", `Original Dose Units`) ~ "cm^2/mL"))   
#Turn all character strings into factors if they aren't already so they are searchable via dropdown
human_search[sapply(human_search, is.character)] <- lapply(human_search[sapply(human_search, is.character)], as.factor)

#### Study Screening Setup ####

human_quality <- human_setup %>%
  filter(particle_red_criteria != "Scoring Not Available") %>% 
  filter(design_red_criteria != "Scoring Not Available") %>% 
  filter(risk_red_criteria != "Scoring Not Available") %>% 
  mutate(Study = paste0(authors, " (", year,")")) %>%
  mutate(Study_plus = as.factor(paste0(authors, " (", year,")", " (",doi,")"))) %>%  
  distinct(Study, Study_plus, doi, particle.1, particle.2, particle.3, particle.4, particle.5, particle.6, particle.7,
           design.1, design.2, design.3, design.4, design.5, design.6, design.7, design.8, design.9, design.10, design.11, design.12, 
           design.12, design.13, risk.1, risk.2, risk.3, risk.4, risk.5, risk.6,
           lvl1_h_f, lvl2_h_f, bio_h_f, effect_h_f, life_h_f, poly_h_f, shape_h_f, size_h_f, species_h_f, exposure_route_h_f, 
           particle_red_criteria, design_red_criteria, risk_red_criteria) %>%     
  
  pivot_longer(!c(Study, Study_plus, doi, lvl1_h_f, lvl2_h_f, bio_h_f, effect_h_f, life_h_f, poly_h_f, shape_h_f, size_h_f, species_h_f, exposure_route_h_f,
                  particle_red_criteria, design_red_criteria, risk_red_criteria),
               names_to ="Criteria", 
               values_to ="Score") %>% 
  #Assign descriptions to numerical scores
  mutate(Score_f = factor(case_when(Score == 0 ~ "Inadequate",
                                    Score == 1 ~ "Adequate with Restrictions",
                                    Score == 2 ~ "Adequate"))) %>%
  #Assign each criteria to appropriate category
  mutate(Category = case_when(grepl("particle", Criteria) ~ "Particle Characterization",
                              grepl("design", Criteria) ~ "Experimental Design",
                              grepl("risk", Criteria) ~ "Risk Assessment")) %>%  
  #Set order of categories so they plot in correct order
  mutate(Category_f = factor(Category, levels = c("Particle Characterization", "Experimental Design", "Risk Assessment"))) %>%   
  #Assign descriptions to each criteria
mutate(Criteria = case_when(Criteria == "particle.1" ~ "Particle Size*",
                            Criteria == "particle.2" ~ "Particle Shape*",
                            Criteria == "particle.3" ~ "Polymer Type*",
                            Criteria == "particle.4" ~ "Particle Source*",
                            Criteria == "particle.5" ~ "Surface Chemistry",
                            Criteria == "particle.6" ~ "Chemical Purity",
                            Criteria == "particle.7" ~ "Microbial Contamination",
                            Criteria == "design.1" ~ "Concentration Units",
                            Criteria == "design.2" ~ "Particle Stability",
                            Criteria == "design.3" ~ "Test Vehicle*",
                            Criteria == "design.4" ~ "Administered Dose*",
                            Criteria == "design.5" ~ "Homogeneity of Exposure",
                            Criteria == "design.6" ~ "Administration Route*",
                            Criteria == "design.7" ~ "Test Species*",
                            Criteria == "design.8" ~ "Feeding/Housing Conditions",
                            Criteria == "design.9" ~ "Sample Size*",
                            Criteria == "design.10" ~ "Frequency/Duration of Exposure*",
                            Criteria == "design.11" ~ "Controls*",
                            Criteria == "design.12" ~ "Replicates",
                            Criteria == "design.13" ~ "Internal Dose Confirmation",
                            Criteria == "risk.1" ~ "Statistical Analysis",
                            Criteria == "risk.2" ~ "Endpoints*",
                            Criteria == "risk.3" ~ "Dose-Response*",
                            Criteria == "risk.4" ~ "Concentration Range",
                            Criteria == "risk.5" ~ "Effect Thresholds*",
                            Criteria == "risk.6" ~ "Test Particle Relevance")) %>%
  mutate(Criteria_f = factor(Criteria, levels = c("Test Particle Relevance","Effect Thresholds*","Concentration Range","Dose-Response*","Endpoints*","Statistical Analysis",
                                                  "Internal Dose Confirmation","Replicates","Controls*","Frequency/Duration of Exposure*","Sample Size*","Feeding/Housing Conditions",
                                                  "Test Species*","Administration Route*","Homogeneity of Exposure","Administered Dose*","Test Vehicle*","Particle Stability",
                                                  "Concentration Units","Microbial Contamination","Chemical Purity","Surface Chemistry","Particle Source*","Polymer Type*","Particle Shape*","Particle Size*")))
                                                     

#### User Interface ####

ui <- dashboardPage(
  
  dashboardHeader(title = "Toxicity of Microplastics Explorer", titleWidth = 400),
  
  dashboardSidebar(width = 175,
                   
                   sidebarMenu(
                     #Logo image
                     br(),
                     tags$img(src="main_logo_drop.png", width = "100%", height = "100%", style = 'display: block; margin-left: auto; margin-right: auto;'),
                     tags$div("Logo created by J.C. Leapman.", align = 'center', style = 'font-size: 10px; display: block; margin-left: auto; margin-right: auto;'), 
                     br(),         
                     #List of tabs
                     menuItem("Welcome", tabName = "Welcome", icon = icon("home")),
                     menuItem("Overview", tabName = "Overview", icon = icon("globe")),
                     menuItem("Search", tabName = "Search", icon = icon("search")),
                     menuItem("Exploration", tabName = "Exploration", icon = icon("chart-bar")),
                     menuItem("Study Screening", tabName = "Screening", icon = icon("check-circle")),
                     menuItem("Resources", tabName = "Resources", icon = icon("question-circle")),
                     menuItem("Contact", tabName = "Contact", icon = icon("envelope")),
                     br(),
                     br(),
                     #Twitter icon
                     menuItem("Aquatic Organisms", href = "https://sccwrp.shinyapps.io/aq_mp_tox_shiny/", icon = icon("fish")),
                     br(),
                     br(),
                     #Twitter icon
                     menuItem("Follow Us on Twitter!", href = "https://twitter.com/ToMExApp", icon = icon("twitter")))
                   
  ), #End dashboard sidebar
  
  dashboardBody(
    
    #extends background color automatically
    tags$head(tags$style(HTML('.content-wrapper { overflow: auto; }'))),
    
    tabItems(
                                      
#### Welcome UI ####        
tabItem(tabName = "Welcome", 
                                               
        #Header     
        h1("Welcome to the Toxicity of Microplastics Explorer,",br(),"Human Health Database!", align = 'center'),
        br(),
        
        
        box(status = "primary", width = 12,
            fluidRow(
              #top right box
              column(width = 12, 
                     
                     p(tags$img(src="welcome.png", width = "40%", height = "40%", style = "float:left; display: block; margin-left: auto; margin-right: 30px;")),
                     
                     h3("What is the Microplastics Toxicity Database?", align = "center"), 
                     
                     strong(p("This database is a repository for microplastics 
                      toxicity data that may inform possible effects on Human Health.")), 
                     
                     p("This web application allows users to explore toxicity 
                    data using an intuitive interface while retaining the diversity and complexity inherent 
                    to microplastics. Data is extracted from existing, peer-reviewed manuscripts containing 
                    toxicity data pertaining to microplastics."),
                     
                     p("Use the side panel on the left of the page to navigate to each section. Each section provides different information or data visualization options. 
                      More specific instructions may be found within each section.")))),
        
        #bottom left box  
        box(status = "primary", width = 12, 
            h3("Why was the Microplastics Toxicity Database and Web Application created?", align = "center"),
            
            p("The database and application tools have been created for use by the participants of the ", a(href = "https://www.sccwrp.org/about/
                      research-areas/additional-research-areas/
                      trash-pollution/microplastics-health-effects-webinar-series/", 'Microplastics Health Effects Workshop', 
                                                                                                            .noWS = "outside"),".The purpose of this workshop is to identify the most sensitive and biologically critical endpoints associated with microplastics exposure, 
                      prioritize which microplastics characteristics (e.g., size, shape, polymer) that are of greatest biological concern, and identify 
                      critical thresholds for each at which those biological effects become pronounced. Workshop participants will also make reccomendations for future
                      research investments. Workshop findings will be published in a special issue of ", a(href ="https://microplastics.springeropen.com/", 'Microplastics and Nanoplastics', .noOWs = "outside"),". 
                      These findings will be used directly by the state of California to fulfill ", a(href = "https://www.sccwrp.org/about/research-areas/
                      additional-research-areas/trash-pollution/microplastics-health-effects-webinar-series/history-california-microplastics-legislation/", 'legislative mandates', 
                                                                                                      .noWS = "outside")," regarding the management of microplastics in drinking water and the aquatic environment.")),
        
        #bottom right box  
        box(status = "primary", width = 12, 
            h3("Contributors", align = "center"), 
            
            p(align = "center", a(href = "https://www.sccwrp.org/about/staff/leah-thornton-hampton/", 'Dr. Leah Thornton Hampton'),", Southern California Coastal Water Research Project ", 
              tags$a(href="https://twitter.com/DrLeahTH", icon("twitter")), tags$a(href="https://github.com/leahth", icon("github"))),
            
            p(align = "center", "Heili Lowman, Southern California Coastal Water Research Project ",
              tags$a(href="https://twitter.com/heili_lowman", icon("twitter")), tags$a(href="https://github.com/hlowman", icon("github"))), 
            
            p(align = "center", a(href = "https://agency.calepa.ca.gov/staffdirectory/detail.asp?UID=69294&BDO=7&VW=DET&SL=S", 'Dr. Scott Coffin'),", California State Water Resources Control Board", 
              tags$a(href="https://twitter.com/DrSCoffin", icon("twitter")), tags$a(href="https://github.com/ScottCoffin", icon("github"))),
            
            p(align = "center", a(href = "https://www.sccwrp.org/about/staff/emily-darin/", 'Emily Darin'),", Southern California Coastal Water Research Project",
              tags$a(href="https://github.com/EmilyDarin", icon("github"))),
            
            p(align = "center", a(href = "https://www.sfei.org/users/liz-miller", 'Dr. Ezra Miller'),", Aquatic Science Center"),
            
            p(align = "center", a(href = "https://rochmanlab.com/people/", 'Dr. Ludovic Hermabessiere'),", University of Toronto", 
              tags$a(href="https://twitter.com/HermabessiereL", icon("twitter"))),
            
            p(align = "center", a(href = "https://rochmanlab.com/people/", 'Hannah De Frond'),", University of Toronto", 
              tags$a(href="https://twitter.com/HanDefrond", icon("twitter"))),
            
            p(align = "center", "Vera de Ruitjer, Wageningen University"),
            
            p(align = "center", "Samreen Siddiqui, Oregon State University"),
            
            p(align = "center", "Andrea Faltynkova, Norwegian University of Science and Technology"),
            
            p(align = "center", "Johannes Völker, Norwegian University of Science and Technology"),
            
            p(align = "center", "Laura Monclús Anglada, Norwegian University of Science and Technology"),
            
            p(align = "center", a(href = "https://www.sccwrp.org/about/staff/syd-kotar/", "Sydney Kotar"),", Southern California Coastal Water Research Project"),
            
            p(align = "center", a(href = "https://branderlab.net/", 'Dr. Susanne Brander'),", Oregon State University",
              tags$a(href="https://twitter.com/smbrander", icon("twitter"))),
            
            p(align = "center", a(href = "https://www.ntnu.edu/employees/martin.wagner", 'Dr. Martin Wagner'),", Norwegian University of Science and Technology",
              tags$a(href="https://twitter.com/martiwag", icon("twitter"))),
            
            p(align = "center", a(href = "https://www.wur.nl/en/Persons/Bart-prof.dr.-AA-Bart-Koelmans.htm", 'Dr. Bart Koelmans'),", Wageningen University",
              tags$a(href="https://twitter.com/MicroplasticLab", icon("twitter"))),
            
            p(align = "center", a(href = "https://rochmanlab.com/", 'Dr. Chelsea Rochman'),", University of Toronto",
              tags$a(href="https://twitter.com/ChelseaRochman", icon("twitter"))),
            
            p(align = "center", a(href = "https://www.sccwrp.org/about/staff/alvina-mehinto/", 'Dr. Alvine Mehinto'),", Southern California Coastal Water Research Project"),
            
            p(align = "center", a(href = "https://www.sccwrp.org/about/staff/steve-weisberg/", 'Dr. Steve Weisberg'),", Southern California Coastal Water Research Project")), 
        
        #Logos with links to organizations
        box(status = "primary", width = 12, align = "center",  
            splitLayout(align = "center", 
                        tags$a(href="https://www.waterboards.ca.gov", tags$img(src="waterboard.png", width = "100%", height = "100%")),
                        tags$a(href="https://www.sccwrp.org", tags$img(src="sccwrp.png", width = "100%", height = "100%")),
                        tags$a(href="https://www.utoronto.ca", tags$img(src="toronto.png", width = "100%", height = "100%")),
                        tags$a(href="https://www.sfei.org/", tags$img(src="sfei.png", width = "100%", height = "100%")))),
        
        
),

#### Overview UI ####
                                      
tabItem(tabName = "Overview", 
        
        box(title = "Database Overview", status = "primary", width = 12, collapsible = TRUE,
            
            p("Select tabs below to explore the database. Each bar displays the total number of measured endpoints where a 
          statistically signifcant effect was detected (Y) or where a measurement was made but a significant effect was not detected (N)."),
            
            br(),
            
            fluidRow(
              tabBox(width = 12,
                     tabPanel("Exposure Route", 
                              plotOutput(outputId = "exposure_plot"),
                     ),
                     
                     tabPanel(div(HTML("<i>In vitro</i> vs <i>In vivo</i>")),
                              plotOutput(outputId = "vivo_plot"),
                     ),
                     
                     tabPanel("Life Stage",
                              plotOutput(outputId = "life_plot"),
                     ),
                     
                     tabPanel("Endpoint Category",
                              plotOutput(outputId = "lvl1_plot"),
                     ),
                     
                     tabPanel("Polymer Type",
                              plotOutput(outputId = "polymer_plot"),
                     ),
                     
                     tabPanel("Particle Morphology",
                              plotOutput(outputId = "shape_plot"),
                     ),
                     
                     tabPanel("Particle Size",
                              plotOutput(outputId = "size_plot"),
                     )),
              
            ), #close fluid row
        ), #close box

        box(title = "Biological Endpoint Catgorization", status = "primary", width = 12, collapsible = TRUE,
            
            br(),
            p("This plot displays the categorization of measured endpoints in the database. Nodes correspond to the Broad Endpoint Category, 
        the Specific Endpoint Category, Endpoints and the level of biological organization from left to right. The widget 
        below may be used to select endpoints at various Biological Levels of Organization. Click nodes to expand and collapse the plot."),
            br(),
            
            fluidRow(
              
              column(width = 12,
                     
                     column(width = 3,
                            pickerInput(inputId = "bio_check_endpoint", # bio org checklist
                                        label = "Level of Biological Organization", 
                                        choices = levels(human_endpoint$bio_h_f),
                                        selected = levels(human_endpoint$bio_h_f),
                                        options = list(`actions-box` = TRUE),
                                        multiple = TRUE)),
              ), #closes out column
              
              column(width = 12,
                     
                     #Go button
                     column(width = 3,
                            actionButton("go_endpoint", "Plot Current Selection", icon("rocket"), style="color: #fff; background-color:  #117a65; border-color:  #0e6655")),
                     
              ), #closes out column
              
              column(width = 12,
                     #collapsible tree plot
                     collapsibleTree::collapsibleTreeOutput("plot", height = "400px"),
                     
              ), #closes out column
              
            ), #close fluid row
        ), #close box
        
), #close tab
       
#### Search UI ####

tabItem(tabName = "Search",
        
        box(title = "Search Database", status = "primary", width = 12,
            
            column(width = 12, 
                   dataTableOutput("databaseDataTable", height = "200px"))   
            
            
        ), #close box
        
),#close search tab

#### Exploration UI ####

tabItem(tabName = "Exploration",
                                               
        box(title = "Data Selection", status = "primary", width = 12, collapsible = TRUE,
            
            # shinyjs::useShinyjs(), # requires package for "reset" button, DO NOT DELETE - make sure to add any new widget to the reset_input in the server
            # id = "exploration", # adds ID for resetting filters
            
            fluidRow(
              tabBox(width = 12,
                     
                     tabPanel("Data Type",
                              
                              fluidRow(
                                #Data type selection
                                column(width = 4,
                                       pickerInput(inputId = "exp_type_check",
                                                   label = "Data Type:",
                                                   choices = levels(human_setup$exp_type_f),
                                                   selected = levels(human_setup$exp_type_f),
                                                   options = list(`actions-box` = TRUE),
                                                   multiple = TRUE)))
                              
                     ), #close tabpanel
                     
                     tabPanel("Effect", 
                              
                              fluidRow(
                                #Broad endpoint selection
                                column(width = 4,
                                       pickerInput(inputId = "lvl1_h_check", 
                                                   label = "Broad Endpoint Category:", 
                                                   choices = levels(human_setup$lvl1_h_f),
                                                   selected = levels(human_setup$lvl1_h_f),
                                                   options = list(`actions-box` = TRUE), 
                                                   multiple = TRUE)), 
                                
                                #Specific endpoint selection
                                column(width = 4,
                                       pickerInput(inputId = "lvl2_h_check", 
                                                   label = "Specific Endpoint Category:", 
                                                   choices = levels(human_setup$lvl2_h_f),
                                                   selected = levels(human_setup$lvl2_h_f),
                                                   options = list(`actions-box` = TRUE),
                                                   multiple = TRUE)),
                                
                                #Effect y/n selection
                                column(width = 4,
                                       pickerInput(inputId = "effect_h_check",  
                                                   label = "Effect:",
                                                   choices = levels(human_setup$effect_h_f),
                                                   selected = levels(human_setup$effect_h_f),
                                                   options = list(`actions-box` = TRUE),
                                                   multiple = TRUE))),
                              
                     ), #close tabpanel
                     
                     tabPanel("Biology", 
                              
                              fluidRow(
                                
                                #species selection
                                column(width = 4,
                                       pickerInput(inputId = "species_h_check", 
                                                   label = "Species:", 
                                                   choices = levels(human_setup$species_h_f),
                                                   selected = levels(human_setup$species_h_f),
                                                   options = list(`actions-box` = TRUE), 
                                                   multiple = TRUE),
                                       
                                       #biological organization selection
                                       pickerInput(inputId = "bio_h_check", 
                                                   label = "Level of Biological Organization", 
                                                   choices = levels(human_setup$bio_h_f),
                                                   selected = levels(human_setup$bio_h_f),
                                                   options = list(`actions-box` = TRUE),
                                                   multiple = TRUE)),
                                
                                #life stage selection
                                column(width = 4,
                                       pickerInput(inputId = "life_h_check", 
                                                   label = "Life Stages:", 
                                                   choices = levels(human_setup$life_h_f),
                                                   selected = levels(human_setup$life_h_f),
                                                   options = list(`actions-box` = TRUE), 
                                                   multiple = TRUE),     
                                       
                                       #exposure duration
                                       pickerInput(inputId = "exposure_route_h_check", 
                                                   label = "Exposure Route:", 
                                                   choices = levels(human_setup$exposure_route_h_f),
                                                   selected = levels(human_setup$exposure_route_h_f),
                                                   options = list(`actions-box` = TRUE), 
                                                   multiple = TRUE))),
                              
                     ), #close tabpanel
                     
                     tabPanel("Particles", 
                              
                              fluidRow(
                                #polymer selection
                                column(width = 4,
                                       pickerInput(inputId = "poly_h_check", 
                                                   label = "Polymer:", 
                                                   choices = levels(human_setup$poly_h_f),
                                                   selected = levels(human_setup$poly_h_f),
                                                   options = list(`actions-box` = TRUE), 
                                                   multiple = TRUE)),
                                
                                #shape selection
                                column(width = 4,
                                       pickerInput(inputId = "shape_h_check", 
                                                   label = "Shape:", 
                                                   choices = levels(human_setup$shape_h_f),
                                                   selected = levels(human_setup$shape_h_f),
                                                   options = list(`actions-box` = TRUE), 
                                                   multiple = TRUE)),
                                
                                #size category selection
                                column(width = 4,
                                       pickerInput(inputId = "size_h_check", # Environment checklist
                                                   label = "Size Category:", 
                                                   choices = levels(human_setup$size_h_f),
                                                   selected = levels(human_setup$size_h_f),
                                                   options = list(`actions-box` = TRUE), 
                                                   multiple = TRUE))),
                              
                     ), #close tabpanel
                     
                     tabPanel("Study Screening", 
                              
                              fluidRow(    
                                column(width = 12,
                                       p("Only in vivo data are included in the study screening dataset.")), 
                                
                                #particle red criteria
                                column(width = 4,
                                       pickerInput(inputId = "particle_criteria_check",
                                                   label = "Particle Criteria:",
                                                   choices = levels(human_setup$particle_red_criteria),
                                                   selected = levels(human_setup$particle_red_criteria),
                                                   options = list(`actions-box` = TRUE),
                                                   multiple = TRUE)),
                                
                                #design red criteria
                                column(width = 4,
                                       pickerInput(inputId = "design_criteria_check",
                                                   label = "Design Criteria:",
                                                   choices = levels(human_setup$design_red_criteria),
                                                   selected = levels(human_setup$design_red_criteria),
                                                   options = list(`actions-box` = TRUE),
                                                   multiple = TRUE)),
                                
                                #risk red criteria
                                column(width = 4,
                                       pickerInput(inputId = "risk_criteria_check",
                                                   label = "Risk Assessment Criteria:",
                                                   choices = levels(human_setup$risk_red_criteria),
                                                   selected = levels(human_setup$risk_red_criteria),
                                                   options = list(`actions-box` = TRUE),
                                                   multiple = TRUE))),

                     ), #close tabpanel
                     
                     tabPanel("Dose Metric",
                              
                              fluidRow(
                                column(width = 4,
                                       radioButtons(inputId = "dose_check", # dosing units
                                                  label = "Dose Metric:",
                                                  choices = c("Particles/mL", "µg/mL", "µm3/mL"),
                                                  selected = "µg/mL")),

                                column(width = 4,
                                       radioButtons(inputId = "Rep_Con_rad",
                                                    label = "Do you want to use just the reported, just the converted, or all exposure concentrations?",
                                                    choices = c("reported", "converted", "all"),
                                                    selected = "all"))),
                              
                     ), #close tabpanel
                     
                     tabPanel("Aesthetics", 
                              
                               fluidRow(
                                column(width = 4,
                                       selectInput(inputId = "plot.type", "Plot Type:",
                                                   list(boxplot = "boxplot", violin = "violin", beeswarm = "beeswarm"))),

                                column(width = 4,
                                       selectInput(inputId = "theme.type_exp", "Dark or Light Mode:",
                                                   list(light = "light", dark = "dark"))),

                                column(width = 4,
                                       selectInput(inputId = "color.type_exp", "Color Theme:",
                                                   list(default = "default", viridis = "viridis", brewer = "brewer", tron = "tron", locusZoom = "locusZoom", d3 = "d3", Nature = "Nature", JAMA = "JAMA")))),

                     ) #close tabpanel
                     
              ), #close tab box
            ), #close fluid row
            
            column(width = 3,
                   actionButton("go", "Plot Current Selection", icon("rocket"), style="color: #fff; background-color:  #117a65; border-color:  #0e6655")),
            
            column(width = 3,
                   actionButton("reset_input", "Reset Filters", icon("redo"), style="color: #fff; background-color: #f39c12; border-color: #d68910")), 
            
            column(width = 3,
                   downloadButton("downloadData", "Download Data (Excel File)", icon("download"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
            
        ), #close box
                                               
        box(title = "Data Visualization", status = "primary", width = 12,
            
            fluidRow(
              tabBox(width = 12,
                     
                     tabPanel("Exposure Route",
                              
                              fluidRow(
                                column(width = 12,      
                                       plotOutput(outputId = "exposure_route_h_plot_react", height = "600px")),
                                
                                column(width = 3,
                                       downloadButton("downloadexploration_exproute", "Download Plot", icon("download"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                              
                     ),#closes tab panel
                     
                     
                     tabPanel("Broad Endpoint Category",
                              
                              fluidRow(
                                column(width = 12,      
                                       plotOutput(outputId = "lvl_h_plot_react", height = "600px")),
                                
                                column(width = 3,
                                       downloadButton("downloadexploration_lvl1", "Download Plot", icon("download"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                              
                     ),#closes tab panel
                     
                     tabPanel("Specific Endpoint Category", 
                              
                              fluidRow(  
                                column(width = 12,
                                       plotOutput(outputId = "lvl2_h_plot_react", height = "auto")),
                                
                                column(width = 3,
                                       downloadButton("downloadexploration_lvl2", "Download Plot", icon("download"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                              
                     ),#closes tab panel
                     
                     tabPanel("Size",
                              
                              fluidRow(
                                column(width = 12,      
                                       plotOutput(outputId = "size_h_plot_react", height = "600px")),
                                
                                column(width = 3,
                                       downloadButton("downloadexploration_size", "Download Plot", icon("download"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                              
                     ),#closes tab panel
                     
                     tabPanel("Shape",
                              
                              fluidRow(
                                column(width = 12,      
                                       plotOutput(outputId = "shape_h_plot_react", height = "600px")),
                                
                                column(width = 3,
                                       downloadButton("downloadexploration_shape", "Download Plot", icon("download"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                              
                     ),#closes tab panel
                     
                     tabPanel("Polymer",
                              
                              fluidRow(
                                column(width = 12,      
                                       plotOutput(outputId = "poly_h_plot_react", height = "600px")),
                                
                                column(width = 3,
                                       downloadButton("downloadexploration_poly", "Download Plot", icon("download"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                              
                     ),#closes tab panel
                     br(),       
                     p("Data labels on the far right of each plot represent the number of measurements and studies, respectively.")
                     
              ), #closes tab box
            ), #closes fluid tab
        ), #closes box

), #close tab
                                  
#### Screening UI ####

tabItem(tabName = "Screening",
        
        box(title = "Data Selection", status = "primary", width = 12, collapsible = TRUE,
            
            shinyjs::useShinyjs(), # requires package for "reset" button, DO NOT DELETE - make sure to add any new widget to the reset_input in the server
            id = "screen", # adds ID for resetting filters
            
            p("This plot displays scores from the", a(href ="https://tger.co.uk/research", 'study prioritization screening tool', .noOWs = "outside"), "developed by Gouin et al. (2021).
            For more information, including the scoring rubric used, see Resources."),
            
            fluidRow(
              tabBox(width = 12, height = "200px",
                     
                     tabPanel("Data Type",
                              
                              "Only in vivo ingestion data are included in the study screening dataset."          
                              
                     ), #close tabpanel
                     
                     tabPanel("Effect", 
                              
                              #Broad endpoint selection
                              column(width = 4,
                                     pickerInput(inputId = "lvl1_h_quality", # endpoint checklist
                                                 label = "Broad Endpoint Category:",
                                                 choices = levels(human_quality$lvl1_h_f),
                                                 selected = levels(human_quality$lvl1_h_f),
                                                 options = list(`actions-box` = TRUE), # option to de/select all
                                                 multiple = TRUE)), # allows for multiple inputs
                              
                              #Specific endpoint selection
                              column(width = 4, #Specific endpoint selection
                                     pickerInput(inputId = "lvl2_h_quality", 
                                                 label = "Specific Endpoint Category:", 
                                                 choices = levels(human_quality$lvl2_h_f),
                                                 selected = levels(human_quality$lvl2_h_f),
                                                 options = list(`actions-box` = TRUE),
                                                 multiple = TRUE)),
                              
                              #Effect y/n selection
                              column(width = 4,
                                     pickerInput(inputId = "effect_h_quality", 
                                                 label = "Effect:",
                                                 choices = levels(human_quality$effect_h_f),
                                                 selected = levels(human_quality$effect_h_f),
                                                 options = list(`actions-box` = TRUE),
                                                 multiple = TRUE)),
                              
                     ), #close tabpanel
                     
                     tabPanel("Biology", 
                           
                              #species selection
                              column(width = 4,
                                     pickerInput(inputId = "species_h_quality", 
                                                 label = "Species:", 
                                                 choices = levels(human_quality$species_h_f),
                                                 selected = levels(human_quality$species_h_f),
                                                 options = list(`actions-box` = TRUE),
                                                 multiple = TRUE), 
                                     
                                     #biological organization selection
                                     pickerInput(inputId = "bio_h_quality", 
                                                 label = "Biological Organization:",
                                                 choices = levels(human_quality$bio_h_f),
                                                 selected = levels(human_quality$bio_h_f),
                                                 options = list(`actions-box` = TRUE),
                                                 multiple = TRUE)),
                              
                              #life stage selection
                              column(width = 4,
                                     pickerInput(inputId = "life_h_quality", 
                                                 label = "Life Stages:",
                                                 choices = levels(human_quality$life_h_f),
                                                 selected = levels(human_quality$life_h_f),
                                                 options = list(`actions-box` = TRUE),
                                                 multiple = TRUE),     
                                     
                                     #exposure duration
                                     pickerInput(inputId = "exposure_route_h_quality", 
                                                 label = "Exposure Route:",
                                                 choices = levels(human_quality$exposure_route_h_f),
                                                 selected = levels(human_quality$exposure_route_h_f),
                                                 options = list(`actions-box` = TRUE),
                                                 multiple = TRUE)),
                              
                     ), #close tabpanel
                     
                     tabPanel("Particles", 
                              
                              #polymer selection
                              column(width = 4,
                                     pickerInput(inputId = "poly_h_quality", 
                                                 label = "Polymer:",
                                                 choices = levels(human_quality$poly_h_f),
                                                 selected = levels(human_quality$poly_h_f),
                                                 options = list(`actions-box` = TRUE),
                                                 multiple = TRUE)),
                              
                              #shape selection
                              column(width = 4,
                                     pickerInput(inputId = "shape_h_quality", 
                                                 label = "Shape:",
                                                 choices = levels(human_quality$shape_h_f),
                                                 selected = levels(human_quality$shape_h_f),
                                                 options = list(`actions-box` = TRUE),
                                                 multiple = TRUE)),
                              
                              #size category selection
                              column(width = 4,
                                     pickerInput(inputId = "size_h_quality", 
                                                 label = "Size Category:",
                                                 choices = levels(human_quality$size_h_f),
                                                 selected = levels(human_quality$size_h_f),
                                                 options = list(`actions-box` = TRUE),
                                                 multiple = TRUE)),
                              
                     ), #close tabpanel
                     
                     tabPanel("Study Screening", 
                              
                              fluidRow(    
                                
                                #particle red criteria
                                column(width = 3,
                                       pickerInput(inputId = "particle_criteria_quality",
                                                   label = "Particle Criteria:",
                                                   choices = levels(human_setup$particle_red_criteria),
                                                   selected = levels(human_setup$particle_red_criteria),
                                                   options = list(`actions-box` = TRUE),
                                                   multiple = TRUE)),
                                
                                #design red criteria
                                column(width = 3,
                                       pickerInput(inputId = "design_criteria_quality",
                                                   label = "Design Criteria:",
                                                   choices = levels(human_setup$design_red_criteria),
                                                   selected = levels(human_setup$design_red_criteria),
                                                   options = list(`actions-box` = TRUE),
                                                   multiple = TRUE)),
                                
                                #risk red criteria
                                column(width = 3,
                                       pickerInput(inputId = "risk_criteria_quality",
                                                   label = "Risk Assessment Criteria:",
                                                   choices = levels(human_setup$risk_red_criteria),
                                                   selected = levels(human_setup$risk_red_criteria),
                                                   options = list(`actions-box` = TRUE),
                                                   multiple = TRUE)),
                              
                                #specfic study
                                column(width = 3,
                                       pickerInput(inputId = "study_plus_quality", 
                                                   label = "Study:",
                                                   choices = levels(human_quality$Study_plus),
                                                   selected = levels(human_quality$Study_plus),
                                                   options = list(`actions-box` = TRUE),
                                                   multiple = TRUE))),
                              
                     ) #close tabpanel
                     
              ), #close tab box
            ), #close fluid row
            
            column(width = 3,
                   actionButton("go_quality", "Plot Current Selection", icon("rocket"), style="color: #fff; background-color:  #117a65; border-color:  #0e6655")),
            
            column(width = 3,
                   actionButton("reset_quality", "Reset Filters", icon("redo"), style="color: #fff; background-color: #f39c12; border-color: #d68910")), 
            
        ), #close box
        
        box(title = "Visualize Data", status = "primary", width = 12,
            
            p("Use the cursor to zoom and hover over the plot to view additional information about each study. Some studies are not visible until zoomed in. 
              Alternatively, specific studies may be selected using the filter in the 'Study Screening' tab above."),
            br(),
            p("'Red Criteria' are indicated by (*). Scores of 0, 1, and 2 are respresented by red, grey, and blue tiles respectively."),
            br(),
            
            fluidRow(
              tabBox(width = 12,
                     
                     tabPanel("Particle Characterization",
                              
                              fluidRow(
                                
                                plotlyOutput("particle_plotly", height = "600px")), 
                              
                     ), #close tabpanel
                     
                     tabPanel("Experimental Design",
                              
                              fluidRow(
                                
                                plotlyOutput("design_plotly", height = "600px")), 
                                
                              ), #close tabpanel
                              
                    tabPanel("Risk Assessment",
                             
                             fluidRow(
                               
                               plotlyOutput("risk_plotly", height = "600px")),
                               
                             ) #close tabpanel
              ) #close tabbox
            ) #close fluidrow
          ), #close box
        
), #closes out tab

#### Resources UI ####

tabItem(tabName = "Resources", 
        
        
        box(title = "Resources", width = 6, status = "primary",     
            p(align = "center",a(href = "https://sccwrp-my.sharepoint.com/:b:/g/personal/leahth_sccwrp_org/EYUFX1dOfSdGuHSfrUDcnewBxgttfTCOwom90hrt5nx1FA?e=jFXEyQ", 'Data Category Descriptions')),
            br(),
            p(align = "center",a(href = "https://sccwrp-my.sharepoint.com/:b:/g/personal/leahth_sccwrp_org/EcJuT9TQeP1LkWAO81pE1DgBTxem_6m_gEh0DPXRexHHhA?e=LucgNJ", 'Study Screening Rubric')),
            br(),
            p(align = "center",a(href = "https://sccwrp-my.sharepoint.com/:b:/g/personal/leahth_sccwrp_org/ER4Blg_W9LtDi-2vJGUK6kcBOdZ-GTgA-HZV4swPwD4bJQ?e=WPHXdZ", 'Human Health Study List')),
            br(),
            p(align = "center",a(href = "https://sccwrp-my.sharepoint.com/:b:/g/personal/leahth_sccwrp_org/EXf0crCKDPVHo5xBEdw4PQwBxA8cnu0x4WY477CuEzZcPw?e=qs00V3", 'Dose Conversion Methods'))),
        
), #close tab

#### Contact UI ####

tabItem(tabName = "Contact", 
        
        box(title = "Contact", width = 6, status = "primary",
            p("For scientific questions, please contact Dr. Leah Thornton Hampton (leahth@sccwrp.org)."),
            br(),
            p("If you encounter technical problems with the web application, please contact Emily Darin (emilyd@sccwrp.org).")),
        
)#closes tab

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
#       p <- ggplot(vivoFinal_year_study,aes(fill=Type, y= Studies, x= as.numeric(year), Percent=Percent)) +
#         geom_bar(position="stack", stat="identity") +
#         scale_x_continuous(breaks = seq(from = 1993, to = 2021, by = 1 ))+ #show all dates
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
      guides(x = guide_axis(angle = 45))+
      theme(text = element_text(size=17))+
      theme(legend.position = "right",
            axis.ticks= element_blank(),
            axis.text.x = element_text(),
            axis.text.y = element_blank(),
            axis.title.x = element_blank())
  })
  
  #Broad endpoint category plot
  output$lvl1_plot <- renderPlot({
    
    # generate plot
    ggplot(lvl1final,aes(fill=effect, y= logEndpoints, x= Type, Percent=Percent)) +
      geom_bar(position="stack", stat="identity") +
      geom_text(aes(label= paste0(Endpoints)), position = position_stack(vjust = 0.5),colour="black", size = 5) +
      scale_fill_manual(values = cal_palette("lupinus"))+
      theme_classic() +
      ylab("Number of Endpoints Measured") +
      labs(fill="Effect") +
      guides(x = guide_axis(angle = 45))+
      theme(text = element_text(size=17))+
      theme(legend.position = "right",
            axis.ticks= element_blank(),
            axis.text.x = element_text(),
            axis.text.y = element_blank(),
            axis.title.x = element_blank())+
      scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15))
  })

  
  #In vivo in vitro plot
  output$vivo_plot <- renderPlot({

    # generate plot
    ggplot(vivofinal,aes(fill=effect, y= logEndpoints, x= Type, Percent=Percent)) +
      geom_bar(position="stack", stat="identity") +
      geom_text(aes(label= paste0(Endpoints)), position = position_stack(vjust = 0.5),colour="black", size = 5) +
      scale_fill_manual(values = cal_palette("kelp2"))+
      theme_classic() +
      ylab("Number of Endpoints Measured") +
      labs(fill="Effect") +
      guides(x = guide_axis(angle = 45))+
      theme(text = element_text(size=17))+
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
      guides(x = guide_axis(angle = 45))+
      theme(text = element_text(size=17))+
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
      guides(x = guide_axis(angle = 45))+
      theme(text = element_text(size=17))+
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
      labs(fill="Effect") +
      guides(x = guide_axis(angle = 45))+
      theme(text = element_text(size=17))+
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
      guides(x = guide_axis(angle = 45))+
      theme(text = element_text(size=17))+
      theme(legend.position = "right",
            axis.ticks= element_blank(),
            axis.text.x = element_text(),
            axis.text.y = element_blank(),
            axis.title.x = element_blank())
  })
  
  # Endpoint Category Plot
  
  human_filter_endpoint <- eventReactive(list(input$go_endpoint),{
    
    # biological organization widget
    bio_c_endpoint <- input$bio_check_endpoint # assign bio values to "bio_c"
    
    human_endpoint %>% # take original dataset
      filter(bio_h_f %in% bio_c_endpoint) #filter by bio organization
    
  })
  
  output$plot <- renderCollapsibleTree({
    
    collapsibleTree(human_filter_endpoint(), root = "Mammalian Database", hierarchy = c("vivo_h_f", "lvl1_h_f", "lvl2_h_f", "lvl3_h_f", "bio_h_f"),
                    fontSize = 12, zoomable = FALSE)
  })
  
  #### Search S ####
  
  output$databaseDataTable <- DT::renderDataTable(
    human_search,
    filter = "top",
    rownames = FALSE,
    extensions = c('Buttons'),
    options = list(
      pageLength = 25,
      dom = 'Brtip',
      buttons = list(I('colvis'), c('copy', 'csv', 'excel')),
      scrollY = 600,
      scrollX = TRUE,
      paging = TRUE,
      columnDefs = list(list(width = '100px', targets = "_all"))),
    colnames = c('DOI', 'Authors', 'Year', 'Particle Characterization "Red Criteria"', 'Experimental Design "Red Criteria"', 
                 'Risk Assessment "Red Criteria"','Species', 'Life Stage', 'In vitro/in vivo',
                 'Sex', 'Experiment Type', 'Exposure Route', 'Particle Mix?', 'Negative Control', 'Reference Particle', 'Exposure Media',
                 'Solvent', 'Detergent', 'pH', 'Salinity (ppt)', 'Temperature (Avg)', 'Temperature (Min)',
                 'Temperature (Max)', 'Exposure Duration (days)', 'Number of Doses', 'Replicates',
                 'Sample Size', 'Dosing Frequency', 'Chemicals Added', 'Added Chemical Dose μg/L (nominal)',
                 'Added Chemical Dose μg/L (measured)', 'Added Chemical Dose μmol/kg (body weight)/day', 'Added Chemical Dose uM',
                 'particles/mL (master)', 'particles/mL (master), reported or converted',
                 'μg/mL (master)', 'μg/mL (master), reported or converted', 'μm^3/mL (master)', 
                 'Effect', 'Direction', 'Broad Endpoint Category', 'Specific Endpoint Category',
                 'Endpoint', 'Level of Biological Organization', 'Target Organelle, Cell, or Tissue',
                 'Polymer', 'Shape', 'Density (g/cm^3)', 'Density, reported or estimated', 'Charge',
                 'Zeta Potential (mV)', 'Zeta Potential Media', 'Functional Group', 'Particle Length (μm)', 'Size Category',
                 'Particle Volume (μm^3)', 'Particle Mass (mg)',
                 'Weathered or Biofouled?', 'Size Validated?', 'Polymer Validated?', 'Shape Validated', 'Particle Source','Sodium Azide Present?',
                 'Screened for Chemical Contamination?', 'Particle Cleaning?', 'Solvent Rinse', 'Background Contamination Monitored?',
                 'Concentration Validated?', 'Particle Behavior', 'Uptake Validated?', 'Tissue Distribution', 'Organisms Fed?', 'Original Dose Units', 'Original Concentration', 'Original Dose Units Nominal or Measured'))
  
  #### Exploration Human S ####
  
  # Create new dataset based on widget filtering and adjusted to reflect the presence of the "update" button.
  human_filter <- eventReactive(list(input$go),{
    # eventReactive explicitly delays activity until you press the button
    # use the inputs to create a new dataset that will be fed into the renderPlot calls below
    
    # every selection widget should be represented as a new variable below
    lvl1_h_c <- input$lvl1_h_check 
    lvl2_h_c <- input$lvl2_h_check 
    bio_h_c <- input$bio_h_check 
    effect_h_c <- input$effect_h_check 
    life_h_c <- input$life_h_check 
    poly_h_c <- input$poly_h_check 
    shape_h_c <- input$shape_h_check 
    size_h_c <- input$size_h_check 
    exposure_route_h_c<-input$exposure_route_h_check
    species_h_c<-input$species_h_check 
    dose_check <- input$dose_check 
    Rep_Con_rad <- input$Rep_Con_rad 
    exp_type_c <- input$exp_type_check
    particle_criteria_c <- input$particle_criteria_check
    design_criteria_c <- input$design_criteria_check
    risk_criteria_c <- input$risk_criteria_check
    
    #filter out reported, calculated, or all based on checkbox and make new variable based on mg/L or particles/mL
    if(Rep_Con_rad == "reported" & dose_check == "µg/mL"){
      human_setup <- human_setup %>% 
        filter(dose.mg.mL.master.reported.converted == "reported") %>% 
        mutate(dose_new = dose.ug.mL.master)}
    
    if(Rep_Con_rad == "converted" & dose_check == "µg/mL"){
      human_setup <- human_setup %>%
        filter(dose.mg.mL.master.reported.converted == "converted") %>% 
        mutate(dose_new = dose.ug.mL.master)}
    
    if(Rep_Con_rad == "all" & dose_check == "µg/mL"){
      human_setup <- human_setup %>%
        mutate(dose_new = dose.ug.mL.master)}
    
    if(Rep_Con_rad == "reported" & dose_check == "Particles/mL"){
      human_setup <- human_setup %>%
        filter(dose.particles.L.master.reported.converted == "reported") %>% 
        mutate(dose_new = dose.particles.mL.master)}
    
    if(Rep_Con_rad == "converted" & dose_check == "Particles/mL"){
      human_setup <- human_setup %>%
        filter(dose.particles.L.master.reported.converted == "converted") %>% 
        mutate(dose_new = dose.particles.mL.master)} 
    
    if(Rep_Con_rad == "all" & dose_check == "Particles/mL"){
      human_setup <- human_setup %>%
        mutate(dose_new = dose.particles.mL.master)}
    
    if(Rep_Con_rad == "reported" & dose_check == "µm3/mL"){
      human_setup <- human_setup %>%
        filter(dose.particles.L.master.reported.converted == "reported") %>% 
        mutate(dose_new = dose.um3.mL.master)}
    
    if(Rep_Con_rad == "converted" & dose_check == "µm3/mL"){
      human_setup <- human_setup %>%
        filter(dose.particles.L.master.reported.converted == "converted") %>% 
        mutate(dose_new = dose.um3.mL.master)} 
    
    if(Rep_Con_rad == "all" & dose_check == "µm3/mL"){
      human_setup <- human_setup %>%
        mutate(dose_new = dose.um3.mL.master)}
    
    human_setup %>% # take original dataset
      filter(lvl1_h_f %in% lvl1_h_c) %>% # filter by level inputs
      filter(lvl2_h_f %in% lvl2_h_c) %>% #filter by level 2 inputs 
      filter(bio_h_f %in% bio_h_c) %>% #filter by bio organization
      filter(effect_h_f %in% effect_h_c) %>% #filter by effect
      filter(life_h_f %in% life_h_c) %>% #filter by life stage
      filter(poly_h_f %in% poly_h_c) %>% #filter by polymer
      filter(shape_h_f %in% shape_h_c) %>% #filter by shape
      filter(size_h_f %in% size_h_c) %>% #filter by size class
      filter(exposure_route_h_f %in% exposure_route_h_c)%>% #filter by exposure route
      filter(species_h_f %in% species_h_c) %>%   #filter by species
      filter(exp_type_f %in% exp_type_c) %>% 
      filter(particle_red_criteria %in% particle_criteria_c) %>% 
      filter(design_red_criteria %in% design_criteria_c) %>%
      filter(risk_red_criteria %in% risk_criteria_c) 
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
  size_h_plot_react <- eventReactive(list(input$go),{
  
    #plot types
    plot.type<-switch(input$plot.type,
                      "boxplot" 	= geom_boxplot(alpha = 0.8, aes(color = effect_h_f)),
                      "violin" = geom_violin(alpha = 0.8, aes(color = effect_h_f)),
                      "beeswarm" = geom_quasirandom(alpha = 0.8, aes(color = effect_h_f), 
                                                    method = "smiley", groupOnX = FALSE, cex = 2)) #groupOnX specifies groups on y axis
    
    #Theme type
    theme.type<-switch(input$theme.type_exp,
                       "light" 	= theme_classic(),
                       "dark" = dark_theme_bw()) 
    #color selection
    fill.type <- switch(input$color.type_exp,
                        "default" =  scale_fill_manual(values = c("#A1CAF6", "#4C6FA1")),
                        "viridis" = scale_fill_viridis(discrete = TRUE),
                        "brewer" =  scale_fill_brewer(palette = "Paired"),
                        "tron" = scale_fill_tron(),
                        "locusZoom" = scale_fill_locuszoom(),
                        "d3" = scale_fill_d3(),
                        "Nature" = scale_fill_npg(),
                        "JAMA" = scale_fill_jama())
    #color selection
    color.type <- switch(input$color.type_exp,
                         "default" =  scale_color_manual(values = c("#A1CAF6", "#4C6FA1")),
                         "viridis" = scale_color_viridis(discrete = TRUE),
                         "brewer" =  scale_color_brewer(palette = "Paired"),
                         "tron" = scale_color_tron(),
                         "locusZoom" = scale_color_locuszoom(),
                         "d3" = scale_color_d3(),
                         "Nature" = scale_color_npg(),
                         "JAMA" = scale_color_jama())
    
    #Create new dataset to gather number of studies and measurements by size
    human_size1 <- human_filter() %>%
      drop_na(dose_new) %>%
      group_by(size_h_f, vivo_h_f, effect_h_f) %>% 
      summarize(dose_new = quantile(dose_new, .1),
                measurements = n(),
                studies = n_distinct(article))
      
    #Render reactive plot
      p <- ggplot(human_filter(), aes(x = dose_new, y = size_h_f, fill = effect_h_f)) + 
        plot.type + 
        coord_trans(x = "log10") +
        scale_x_continuous(breaks = scales::trans_breaks("log10", function(x) 10^x, n = 10),
                           labels = trans_format("log10", scales::math_format(10^.x))) +
        scale_y_discrete(limits=rev)+
        color.type +
        fill.type +
        geom_text(data = human_size1, 
                  aes(label = paste("(",measurements,",",studies,")"),
                      y = size_h_f,
                      x = Inf,
                      hjust = 1),
                  position = position_dodge(.9),
                  size = 4.5, color = "black")+
        theme.type +
        theme(text = element_text(size=18), 
              legend.position = "right") +
        facet_wrap(~vivo_h_f)+
        labs(x = input$dose_check,
             y = "Size",
             color = "Effect?",
             fill = "Effect?",
             caption = (input$Rep_Con_rad))%>%
        req(nrow(human_filter()) > 0)
      
    print(p)
  
    })
  
  output$size_h_plot_react <- renderPlot({
    
    size_h_plot_react()
    
  })
  
  # Shape Plot
  
  shape_h_plot_react <- eventReactive(list(input$go),{
    
    #plot types
    plot.type<-switch(input$plot.type,
                      "boxplot" 	= geom_boxplot(alpha = 0.8, aes(color = effect_h_f)),
                      "violin" = geom_violin(alpha = 0.8, aes(color = effect_h_f)),
                      "beeswarm" = geom_quasirandom(alpha = 0.8, aes(color = effect_h_f), 
                                                    method = "smiley", groupOnX = FALSE, cex = 2)) #groupOnX specifies groups on y axis
    
    #Theme type
    theme.type<-switch(input$theme.type_exp,
                       "light" 	= theme_classic(),
                       "dark" = dark_theme_bw()) 
    #color selection
    fill.type <- switch(input$color.type_exp,
                        "default" =  scale_fill_manual(values = c("#C7EAE5","#35978F")),
                        "viridis" = scale_fill_viridis(discrete = TRUE),
                        "brewer" =  scale_fill_brewer(palette = "Paired"),
                        "tron" = scale_fill_tron(),
                        "locusZoom" = scale_fill_locuszoom(),
                        "d3" = scale_fill_d3(),
                        "Nature" = scale_fill_npg(),
                        "JAMA" = scale_fill_jama())
    #color selection
    color.type <- switch(input$color.type_exp,
                         "default" =  scale_color_manual(values = c("#C7EAE5","#35978F")),
                         "viridis" = scale_color_viridis(discrete = TRUE),
                         "brewer" =  scale_color_brewer(palette = "Paired"),
                         "tron" = scale_color_tron(),
                         "locusZoom" = scale_color_locuszoom(),
                         "d3" = scale_color_d3(),
                         "Nature" = scale_color_npg(),
                         "JAMA" = scale_color_jama())
    
    human_shape1 <- human_filter() %>%
      drop_na(dose_new) %>%
      group_by(shape_h_f, vivo_h_f, effect_h_f) %>% 
      summarize(dose_new = quantile(dose_new, .1),
                measurements = n(),
                studies = n_distinct(article))
    
    #Render reactive plot
    p <- ggplot(human_filter(), aes(x = dose_new, y = shape_h_f, fill = effect_h_f)) + 
      plot.type + 
      coord_trans(x = "log10") +
      scale_x_continuous(breaks = scales::trans_breaks("log10", function(x) 10^x, n = 10),
                         labels = trans_format("log10", scales::math_format(10^.x))) +
      scale_y_discrete(limits=rev)+
      color.type +
      fill.type +
      geom_text(data = human_shape1, 
                aes(label = paste("(",measurements,",",studies,")"),
                    y = shape_h_f,
                    x = Inf,
                    hjust = 1),
                position = position_dodge(.9),
                size = 4.5, color = "black")+
      theme.type +
      theme(text = element_text(size=18), 
            legend.position = "right") +
      facet_wrap(~vivo_h_f)+
      labs(x = input$dose_check,
           y = "Shape",
           color = "Effect?",
           fill = "Effect?",
           caption = (input$Rep_Con_rad))%>%
      req(nrow(human_filter()) > 0)
    
    print(p)
    
  })
  
  output$shape_h_plot_react <- renderPlot({
    
    shape_h_plot_react()
    
  })
  
  # Polymer Plot
  
  poly_h_plot_react <- eventReactive(list(input$go),{
    
    #plot types
    plot.type<-switch(input$plot.type,
                      "boxplot" 	= geom_boxplot(alpha = 0.8, aes(color = effect_h_f)),
                      "violin" = geom_violin(alpha = 0.8, aes(color = effect_h_f)),
                      "beeswarm" = geom_quasirandom(alpha = 0.8, aes(color = effect_h_f), 
                                                    method = "smiley", groupOnX = FALSE, cex = 2)) #groupOnX specifies groups on y axis
    
    #Theme type
    theme.type<-switch(input$theme.type_exp,
                       "light" 	= theme_classic(),
                       "dark" = dark_theme_bw()) 
    #color selection
    fill.type <- switch(input$color.type_exp,
                        "default" =  scale_fill_manual(values = c("#FAB455", "#A5683C")),
                        "viridis" = scale_fill_viridis(discrete = TRUE),
                        "brewer" =  scale_fill_brewer(palette = "Paired"),
                        "tron" = scale_fill_tron(),
                        "locusZoom" = scale_fill_locuszoom(),
                        "d3" = scale_fill_d3(),
                        "Nature" = scale_fill_npg(),
                        "JAMA" = scale_fill_jama())
    #color selection
    color.type <- switch(input$color.type_exp,
                         "default" =  scale_color_manual(values = c("#FAB455", "#A5683C")),
                         "viridis" = scale_color_viridis(discrete = TRUE),
                         "brewer" =  scale_color_brewer(palette = "Paired"),
                         "tron" = scale_color_tron(),
                         "locusZoom" = scale_color_locuszoom(),
                         "d3" = scale_color_d3(),
                         "Nature" = scale_color_npg(),
                         "JAMA" = scale_color_jama())
    
    human_poly1 <- human_filter() %>%
      drop_na(dose_new) %>%
      group_by(poly_h_f, vivo_h_f, effect_h_f) %>% 
      summarize(dose_new = quantile(dose_new, .1),
                measurements = n(),
                studies = n_distinct(article))
    
    #Render reactive plot
    p <- ggplot(human_filter(), aes(x = dose_new, y = poly_h_f, fill = effect_h_f)) + 
      plot.type + 
      coord_trans(x = "log10") +
      scale_x_continuous(breaks = scales::trans_breaks("log10", function(x) 10^x, n = 10),
                         labels = trans_format("log10", scales::math_format(10^.x))) +
      scale_y_discrete(limits=rev)+
      color.type +
      fill.type +
      geom_text(data = human_poly1, 
                aes(label = paste("(",measurements,",",studies,")"),
                    y = poly_h_f,
                    x = Inf,
                    hjust = 1),
                position = position_dodge(.9),
                size = 4.5, color = "black")+
      theme.type +
      theme(text = element_text(size=18), 
            legend.position = "right") +
      facet_wrap(~vivo_h_f)+
      labs(x = input$dose_check,
           y = "Polymer",
           color = "Effect?",
           fill = "Effect?",
           caption = (input$Rep_Con_rad))%>%
      req(nrow(human_filter()) > 0)
    
    print(p)
    
  })
  
  output$poly_h_plot_react <- renderPlot({
    
    poly_h_plot_react()
    
  })
  
  # Endpoint Plot
  
  lvl_h_plot_react <- eventReactive(list(input$go),{
    
    #plot types
    plot.type<-switch(input$plot.type,
                      "boxplot" 	= geom_boxplot(alpha = 0.8, aes(color = effect_h_f)),
                      "violin" = geom_violin(alpha = 0.8, aes(color = effect_h_f)),
                      "beeswarm" = geom_quasirandom(alpha = 0.8, aes(color = effect_h_f), 
                                                    method = "smiley", groupOnX = FALSE, cex = 2)) #groupOnX specifies groups on y axis
    #Theme type
    theme.type<-switch(input$theme.type_exp,
                       "light" 	= theme_classic(),
                       "dark" = dark_theme_bw()) 
    #color selection
    fill.type <- switch(input$color.type_exp,
                        "default" =  scale_fill_manual(values = c("#A99CD9", "#6C568C")),
                        "viridis" = scale_fill_viridis(discrete = TRUE),
                        "brewer" =  scale_fill_brewer(palette = "Paired"),
                        "tron" = scale_fill_tron(),
                        "locusZoom" = scale_fill_locuszoom(),
                        "d3" = scale_fill_d3(),
                        "Nature" = scale_fill_npg(),
                        "JAMA" = scale_fill_jama())
    #color selection
    color.type <- switch(input$color.type_exp,
                         "default" =  scale_color_manual(values = c("#A99CD9", "#6C568C")),
                         "viridis" = scale_color_viridis(discrete = TRUE),
                         "brewer" =  scale_color_brewer(palette = "Paired"),
                         "tron" = scale_color_tron(),
                         "locusZoom" = scale_color_locuszoom(),
                         "d3" = scale_color_d3(),
                         "Nature" = scale_color_npg(),
                         "JAMA" = scale_color_jama())
    
    human_lvl1 <- human_filter() %>%
    drop_na(dose_new) %>%
      group_by(lvl1_h_f, vivo_h_f, effect_h_f) %>% 
      summarize(dose_new = quantile(dose_new, .1),
                measurements = n(),
                studies = n_distinct(article))
    
    #Render reactive plot
    p <- ggplot(human_filter(), aes(x = dose_new, y = lvl1_h_f, fill = effect_h_f)) + 
      plot.type + 
      coord_trans(x = "log10") +
      scale_x_continuous(breaks = scales::trans_breaks("log10", function(x) 10^x, n = 10),
                         labels = trans_format("log10", scales::math_format(10^.x))) +
      scale_y_discrete(limits=rev)+
      color.type +
      fill.type +
      geom_text(data = human_lvl1, 
                aes(label = paste("(",measurements,",",studies,")"),
                    y = lvl1_h_f,
                    x = Inf,
                    hjust = 1),
                position = position_dodge(.9),
                size = 4.5, color = "black")+
      theme.type +
      theme(text = element_text(size=18), 
            legend.position = "right") +
      facet_wrap(~vivo_h_f)+
      labs(x = input$dose_check,
           y = "Broad Endpoint Category",
           color = "Effect?",
           fill = "Effect?",
           caption = (input$Rep_Con_rad))%>%
      req(nrow(human_filter()) > 0)
    
    print(p)
    
  })
  
  output$lvl_h_plot_react <- renderPlot({
    
    lvl_h_plot_react()
    
  })
  
  #Lvl2 Plot 
  
  lvl2_h_plot_react <- eventReactive(list(input$go),{
    
    #plot types
    plot.type<-switch(input$plot.type,
                      "boxplot" 	= geom_boxplot(alpha = 0.8, aes(color = effect_h_f)),
                      "violin" = geom_violin(alpha = 0.8, aes(color = effect_h_f)),
                      "beeswarm" = geom_quasirandom(alpha = 0.8, aes(color = effect_h_f), 
                                                    method = "smiley", groupOnX = FALSE, cex = 2)) #groupOnX specifies groups on y axis
    
    #Theme type
    theme.type<-switch(input$theme.type_exp,
                       "light" 	= theme_classic(),
                       "dark" = dark_theme_bw()) 
    #color selection
    fill.type <- switch(input$color.type_exp,
                        "default" =  scale_fill_manual(values = c("#A99CD9", "#6C568C")),
                        "viridis" = scale_fill_viridis(discrete = TRUE),
                        "brewer" =  scale_fill_brewer(palette = "Paired"),
                        "tron" = scale_fill_tron(),
                        "locusZoom" = scale_fill_locuszoom(),
                        "d3" = scale_fill_d3(),
                        "Nature" = scale_fill_npg(),
                        "JAMA" = scale_fill_jama())
    #color selection
    color.type <- switch(input$color.type_exp,
                         "default" =  scale_color_manual(values = c("#A99CD9", "#6C568C")),
                         "viridis" = scale_color_viridis(discrete = TRUE),
                         "brewer" =  scale_color_brewer(palette = "Paired"),
                         "tron" = scale_color_tron(),
                         "locusZoom" = scale_color_locuszoom(),
                         "d3" = scale_color_d3(),
                         "Nature" = scale_color_npg(),
                         "JAMA" = scale_color_jama())
    
    human_lvl2 <- human_filter() %>%
      drop_na(dose_new) %>%
      group_by(lvl2_h_f, vivo_h_f, effect_h_f) %>% 
      summarize(dose_new = quantile(dose_new, .1),
                measurements = n(),
                studies = n_distinct(article))
    
    
    #Render reactive plot
    p <- ggplot(human_filter(), aes(x = dose_new, y = lvl2_h_f, fill = effect_h_f)) + 
      plot.type + 
      coord_trans(x = "log10") +
      scale_x_continuous(breaks = scales::trans_breaks("log10", function(x) 10^x, n = 10),
                         labels = trans_format("log10", scales::math_format(10^.x))) +
      scale_y_discrete(limits=rev)+
      color.type +
      fill.type +
      geom_text(data = human_lvl2, 
                aes(label = paste("(",measurements,",",studies,")"),
                    y = lvl2_h_f,
                    x = Inf,
                    hjust = 1),
                position = position_dodge(.9),
                size = 4.5, color = "black")+
      theme.type +
      theme(text = element_text(size=18), 
            legend.position = "right") +
      facet_wrap(~vivo_h_f)+
      labs(x = input$dose_check,
           y = "Specific Endpoint Category",
           color = "Effect?",
           fill = "Effect?",
           caption = (input$Rep_Con_rad))%>%
      req(nrow(human_filter()) > 0)
    
    print(p)
    
  })
  
  output$lvl2_h_plot_react <- renderPlot(
    
    #dynamic plot height based on widget input
    height = function()if_else(600 < n_distinct(input$lvl2_h_check)*40, n_distinct(input$lvl2_h_check)*40, 600),
    
    {
      
      lvl2_h_plot_react()
      
    })
  
  
  #exposure route
  
  exposure_route_h_plot_react <- eventReactive(list(input$go),{
    
    #plot types
    plot.type<-switch(input$plot.type,
                      "boxplot" 	= geom_boxplot(alpha = 0.8, aes(color = effect_h_f)),
                      "violin" = geom_violin(alpha = 0.8, aes(color = effect_h_f)),
                      "beeswarm" = geom_quasirandom(alpha = 0.8, aes(color = effect_h_f), 
                                                    method = "smiley", groupOnX = FALSE, cex = 2)) #groupOnX specifies groups on y axis
    
    #Theme type
    theme.type<-switch(input$theme.type_exp,
                       "light" 	= theme_classic(),
                       "dark" = dark_theme_bw()) 
    #color selection
    fill.type <- switch(input$color.type_exp,
                        "default" =  scale_fill_manual(values = c("#C7EAE5","#35978F")),
                        "viridis" = scale_fill_viridis(discrete = TRUE),
                        "brewer" =  scale_fill_brewer(palette = "Paired"),
                        "tron" = scale_fill_tron(),
                        "locusZoom" = scale_fill_locuszoom(),
                        "d3" = scale_fill_d3(),
                        "Nature" = scale_fill_npg(),
                        "JAMA" = scale_fill_jama())
    #color selection
    color.type <- switch(input$color.type_exp,
                         "default" =  scale_color_manual(values = c("#C7EAE5","#35978F")),
                         "viridis" = scale_color_viridis(discrete = TRUE),
                         "brewer" =  scale_color_brewer(palette = "Paired"),
                         "tron" = scale_color_tron(),
                         "locusZoom" = scale_color_locuszoom(),
                         "d3" = scale_color_d3(),
                         "Nature" = scale_color_npg(),
                         "JAMA" = scale_color_jama())
    
    human_exposure1 <- human_filter() %>%
      drop_na(dose_new) %>%
      group_by(exposure_route_h_f, vivo_h_f, effect_h_f) %>% 
      summarize(dose_new = quantile(dose_new, .1),
                measurements = n(),
                studies = n_distinct(article))
    
    #Render reactive plot
    p <- ggplot(human_filter(), aes(x = dose_new, y = exposure_route_h_f, fill = effect_h_f)) + 
      plot.type + 
      coord_trans(x = "log10") +
      scale_x_continuous(breaks = scales::trans_breaks("log10", function(x) 10^x, n = 10),
                         labels = trans_format("log10", scales::math_format(10^.x))) +
      scale_y_discrete(limits=rev)+
      color.type +
      fill.type +
      geom_text(data = human_exposure1, 
                aes(label = paste("(",measurements,",",studies,")"),
                    y = exposure_route_h_f,
                    x = Inf,
                    hjust = 1),
                position = position_dodge(.9),
                size = 4.5, color = "black")+
      theme.type +
      theme(text = element_text(size=18), 
            legend.position = "right") +
      facet_wrap(~vivo_h_f)+
      labs(x = input$dose_check,
           y = "Exposure Route",
           color = "Effect?",
           fill = "Effect?",
           caption = (input$Rep_Con_rad))%>%
      req(nrow(human_filter()) > 0)
    
    print(p)
    
  })
  
  output$exposure_route_h_plot_react <- renderPlot({
    
    exposure_route_h_plot_react()
    
  })
  
  # Create downloadable png for exploration plots
  
  #exposure route
  output$downloadexploration_exproute <- downloadHandler(
    
    filename = function() {
      paste('Exposure_Route', Sys.Date(), '.png', sep='')
    },
    content = function(file) {
      ggsave(file, plot = exposure_route_h_plot_react(), width = 16, height = 8, device = 'png')
    })
  
  #size
  output$downloadexploration_size <- downloadHandler(
    
    filename = function() {
      paste('Size', Sys.Date(), '.png', sep='')
    },
    content = function(file) {
      ggsave(file, plot = size_h_plot_react(), width = 16, height = 8, device = 'png')
    })
  
  #shape
  output$downloadexploration_shape <- downloadHandler(
    
    filename = function() {
      paste('Shape', Sys.Date(), '.png', sep='')
    },
    content = function(file) {
      ggsave(file, plot = shape_h_plot_react(), width = 16, height = 8, device = 'png')
    })
  
  #polymer
  output$downloadexploration_poly <- downloadHandler(
    
    filename = function() {
      paste('Polymer', Sys.Date(), '.png', sep='')
    },
    content = function(file) {
      ggsave(file, plot = poly_h_plot_react(), width = 16, height = 8, device = 'png')
    })
  
  #lvl1
  output$downloadexploration_lvl1 <- downloadHandler(
    
    filename = function() {
      paste('Broad_Endpoint', Sys.Date(), '.png', sep='')
    },
    content = function(file) {
      ggsave(file, plot = lvl_h_plot_react(), width = 16, height = 8, device = 'png')
    })
  
  #lvl2
  output$downloadexploration_lvl2 <- downloadHandler(
    
    filename = function() {
      paste('Specific_Endpoint', Sys.Date(), '.png', sep='')
    },
    content = function(file) {
      
      ggsave(file, plot = lvl2_h_plot_react(), width = 30, height = 20, device = 'png')
    })
  
  # Create downloadable csv of filtered dataset.
  # Removed columns created above so the dataset matches Leah's original dataset.
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(human_filter() %>%
                  #Select columns
                  dplyr::select(c(doi, authors, year, species_h_f, life_h_f, vivo_h_f, sex,
                                  #experimental parameters
                                  exp_type_f, exposure_route_h_f, mix, negative.control, reference.material, exposure.media, solvent, detergent,
                                  media.ph, media.sal, media.temp, media.temp.min, media.temp.max, exposure.duration.d, 
                                  treatment, replicates, sample.size, dosing.frequency, chem,  
                                  #selected dose
                                  dose_new,
                                  #biological effects
                                  effect_h_f, direction, lvl1_h_f, lvl2_h_f, lvl3_h_f, bio_h_f, target.organelle.cell.tissue, 
                                  #particle characteristics
                                  poly_h_f, shape_h_f, density.g.cm3, density.reported.estimated, charge, zetapotential, zeta.potential.media, functional.group,
                                  size.length.um.used.for.conversion, size_h_f, particle.volume.um,
                                  mass.per.particle.mg, weathered.biofouled,
                                  #quality
                                  size.valid, polymer.valid, shape.valid, particle.source, sodium.azide, contaminant.screen, clean.method, sol.rinse, background.plastics,
                                  con.valid, particle.behavior, uptake.valid, tissue.distribution, fed)) %>%  
                  #Rename columns
                  dplyr::rename(c("DOI" = doi, "Authors" = authors, "Year" = year, "Species" = species_h_f, 
                                  "Life Stage" = life_h_f, "In vitro/in vivo" = vivo_h_f, "Sex" = sex,
                                  #experimental parameters
                                  "Experiment Type" = exp_type_f, "Exposure Route" = exposure_route_h_f, "Particle Mix?" = mix, "Negative Control" = negative.control,
                                  "Reference Particle" = reference.material, "Exposure Media" = exposure.media, "Solvent" = solvent, "Detergent" = detergent,
                                  "pH" = media.ph, "Salinity (ppt)" = media.sal, "Temperature (Avg)" = media.temp, "Temperature (Min)"= media.temp.min,
                                  "Temperature (Max)" = media.temp.max, "Exposure Duration (days)" = exposure.duration.d, "Number of Doses" = treatment, "Replicates" = replicates, "Sample Size" = sample.size, "Dosing Frequency" = dosing.frequency,
                                  "Chemicals Added" = chem, 
                                  #selected dose
                                  "Selected Dose" = dose_new,
                                  #biological effects
                                  "Effect" = effect_h_f, "Direction" = direction, "Broad Endpoint Category" = lvl1_h_f, "Specific Endpoint Category" = lvl2_h_f,
                                  "Endpoint" = lvl3_h_f, "Level of Biological Organization" = bio_h_f, "Target Organelle, Cell, or Tissue" = target.organelle.cell.tissue,
                                  #particle characteristics
                                  "Polymer" = poly_h_f, "Shape" = shape_h_f, "Density (g/cm^3)" = density.g.cm3, "Density, reported or estimated" = density.reported.estimated,
                                  "Charge" = charge, "Zeta Potential (mV)" = zetapotential, "Zeta Potential Media" = zeta.potential.media, "Functional Group" = functional.group,
                                  "Particle Length (μm)" = size.length.um.used.for.conversion, "Size Category" = size_h_f, "Particle Volume (μm^3)" = particle.volume.um,
                                  "Particle Mass (mg)" = mass.per.particle.mg, "Weathered or Biofouled?" = weathered.biofouled,
                                  #quality
                                  "Size Validated?" = size.valid, "Polymer Validated?" = polymer.valid, "Shape Validated" = shape.valid, "Particle Source" = particle.source,
                                  "Sodium Azide Present?" = sodium.azide, "Screened for Chemical Contamination?" = contaminant.screen, "Particle Cleaning?" = clean.method,
                                  "Solvent Rinse" = sol.rinse, "Background Contamination Monitored?" = background.plastics,
                                  "Concentration Validated?"  = con.valid, "Particle Behavior" = particle.behavior, "Uptake Validated?" = uptake.valid,
                                  "Tissue Distribution" = tissue.distribution, "Organisms Fed?" = fed)),
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
    shinyjs::reset("exp_type_check")
    shinyjs::reset("Rep_Con_rad")
    shinyjs::reset("dose_check")
    shinyjs::reset("particle_criteria_check")
    shinyjs::reset("design_criteria_check")
    shinyjs::reset("risk_criteria_check")
    
  }) #If we add more widgets, make sure they get added here.   
  

  
##### Study Screening S #####
  
quality_filtered <- eventReactive(list(input$go_quality),{

  # every selection widget should be represented as a new variable below
  lvl1_h_c <- input$lvl1_h_quality
  lvl2_h_c <- input$lvl2_h_quality
  bio_h_c <- input$bio_h_quality
  effect_h_c <- input$effect_h_quality
  life_h_c <- input$life_h_quality
  poly_h_c <- input$poly_h_quality
  shape_h_c <- input$shape_h_quality
  size_h_c <- input$size_h_quality
  species_h_c<-input$species_h_quality
  particle_criteria_c <- input$particle_criteria_quality
  design_criteria_c <- input$design_criteria_quality
  risk_criteria_c <- input$risk_criteria_quality
  study_plus_c <- input$study_plus_quality
  exposure_route_h_c<-input$exposure_route_h_quality
 
  #make summary dataset to display in heatmap below
  human_quality %>%
    filter(lvl1_h_f %in% lvl1_h_c) %>% 
    filter(lvl2_h_f %in% lvl2_h_c) %>% 
    filter(bio_h_f %in% bio_h_c) %>% 
    filter(effect_h_f %in% effect_h_c) %>% 
    filter(life_h_f %in% life_h_c) %>% 
    filter(poly_h_f %in% poly_h_c) %>% 
    filter(shape_h_f %in% shape_h_c) %>% 
    filter(size_h_f %in% size_h_c) %>% 
    filter(species_h_f %in% species_h_c) %>%  
    filter(particle_red_criteria %in% particle_criteria_c) %>% 
    filter(design_red_criteria %in% design_criteria_c) %>%
    filter(risk_red_criteria %in% risk_criteria_c) %>%
    filter(Study_plus %in% study_plus_c) %>%
    filter(exposure_route_h_f %in% exposure_route_h_c)

})
  
  #Create plot for quality screening scores from quality_filtered data
  particle_plotly <- eventReactive(list(input$go_quality),{
    
    #Technical
    particle <- quality_filtered() %>%
      filter(Category_f == "Particle Characterization") %>%  
      #summarize data for plotly
      group_by(Study_plus, Criteria_f, Score) %>%  
      summarise() %>%
      ungroup() %>%
      pivot_wider(names_from = Study_plus, 
                  values_from = Score) %>%   
      column_to_rownames(var="Criteria_f")  
    
    colnames(particle)<- gsub(" \\(10.*", "",colnames(particle))
    colnames(particle)<- gsub(" \\(doi.*", "",colnames(particle))
    
    particle <- particle %>% 
      as.matrix()
    
    #make plotly
    particle_p <- plot_ly(x=colnames(particle), y=rownames(particle), z = particle, type = "heatmap",
                      ygap = .4, xgap = .4,
                      colors = c("tomato", "ivory3", "dodgerblue"),
                      hoverinfo = 'text',
                      showscale = FALSE,
                      hovertemplate = paste(" Study:  %{x}<br>",
                                            "Criteria:  %{y}<br>",
                                            "Score:  %{z}<extra></extra>")) 
    
    particle_p <- particle_p %>% layout(
      title = 'Particle Characterization Criteria',
      xaxis = list(
        type = 'category',
        list(fixedrange = TRUE),
        tickfont = list(size = 10)),
      yaxis = list(tickfont = list(size = 10)))
    
    #print plot
    print(particle_p)
    
    
  })
  
  #Render plotly
  output$particle_plotly <- renderPlotly({
    
    particle_plotly()
    
  })
  
  #Create plot for quality screening scores from quality_filtered data
  design_plotly <- eventReactive(list(input$go_quality),{
    
    #Technical
    design <- quality_filtered() %>%
      filter(Category_f == "Experimental Design") %>%  
      #summarize data for plotly
      group_by(Study_plus, Criteria_f, Score) %>%  
      summarise() %>%
      ungroup() %>%
      pivot_wider(names_from = Study_plus, 
                  values_from = Score) %>%   
      column_to_rownames(var="Criteria_f")  
    
    colnames(design)<- gsub(" \\(10.*", "",colnames(design))
    colnames(design)<- gsub(" \\(doi.*", "",colnames(design))
    
    design <- design %>% 
      as.matrix()
    
    #make plotly
    design_p <- plot_ly(x=colnames(design), y=rownames(design), z = design, type = "heatmap",
                          ygap = .4, xgap = .4,
                          colors = c("tomato", "ivory3", "dodgerblue"),
                          hoverinfo = 'text',
                          showscale = FALSE,
                          hovertemplate = paste(" Study:  %{x}<br>",
                                                "Criteria:  %{y}<br>",
                                                "Score:  %{z}<extra></extra>")) 
    
    design_p <- design_p %>% layout(
      title = 'Experimental Design Criteria',
      xaxis = list(
        type = 'category',
        list(fixedrange = TRUE),
        tickfont = list(size = 10)),
      yaxis = list(tickfont = list(size = 10)))
    
    #print plot
    print(design_p)
    
    
  })
  
  #Render plotly
  output$design_plotly <- renderPlotly({
    
    design_plotly()
    
  })
  
  #Create plot for quality screening scores from quality_filtered data
  risk_plotly <- eventReactive(list(input$go_quality),{
    
    #Risk Assessment
    risk <- quality_filtered() %>%
      filter(Category_f == "Risk Assessment") %>%  
      #summarize data for plotly
      group_by(Study_plus, Criteria_f, Score) %>%  
      summarise() %>%
      ungroup() %>%  
      pivot_wider(names_from = Study_plus, 
                  values_from = Score) %>%   
      column_to_rownames(var="Criteria_f") 
    
    colnames(risk)<- gsub(" \\(10.*", "",colnames(risk))
    colnames(risk)<- gsub(" \\(doi.*", "",colnames(risk))
    
    risk <- risk %>% 
      as.matrix()
    
    #make plotly
    risk_p <- plot_ly(x=colnames(risk), y=rownames(risk), z = risk, type = "heatmap",
                      ygap = .4, xgap = .4,
                      colors = c("tomato", "ivory3", "dodgerblue"),
                      hoverinfo = 'text',
                      showscale = FALSE,
                      hovertemplate = paste(" Study:  %{x}<br>",
                                            "Criteria:  %{y}<br>",
                                            "Score:  %{z}<extra></extra>")) 
    
    risk_p <- risk_p %>% layout(
      title = 'Risk Assessment Criteria',
      xaxis = list(
        type = 'category',
        list(fixedrange = TRUE),
        tickfont = list(size = 10)),
      yaxis = list(tickfont = list(size = 10)))
    
    #print plots
    print(risk_p)
    
  })
  
  #Render plotly
  output$risk_plotly <- renderPlotly({
    
    risk_plotly()
    
  })

# Create "reset" button to revert all filters back to what they began as.
# Need to call all widgets individually by their ids.
# See https://stackoverflow.com/questions/44779775/reset-inputs-with-reactive-app-in-shiny for more information.
observeEvent(input$reset_quality, {
  shinyjs::reset("lvl1_h_quality")
  shinyjs::reset("lvl2_h_quality")
  shinyjs::reset("bio_h_quality")
  shinyjs::reset("effect_h_quality")
  shinyjs::reset("life_h_quality")
  shinyjs::reset("poly_h_quality")
  shinyjs::reset("shape_h_quality")
  shinyjs::reset("size_h_quality")
  shinyjs::reset("species_h_quality")
  shinyjs::reset("particle_criteria_quality")
  shinyjs::reset("design_criteria_quality")
  shinyjs::reset("risk_criteria_quality")
  shinyjs::reset("study_plus_quality")
  shinyjs::reset("exposure_route_h_quality")
  
}) #If we add more widgets, make sure they get added here.
  
} #Server end

#### Full App ####
shinyApp(ui = ui, server = server)

# End of R Shiny app script.
