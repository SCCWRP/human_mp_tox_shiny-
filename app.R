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
library(collapsibleTree) #plot type for endpoint category tree
library(hrbrthemes) #theme for screening plot

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
                                               exposure.route ==  "Not Applicable"~"Not Applicable (in vitro)")))%>% #Renames for widget - only categories included under 
                                                                                                                      #ingestion and in vitro are included (we don't want other 
                                                                                                                      #routes of exposure plotted in exploration because there is so little data)
  mutate(species_h_f = factor(case_when(species == "aries"~"(Sheep) Ovis aries",
                                        species == "sapiens"~"(Human) Homo sapiens",
                                        species == "musculus"~"(Mouse) Mus musculus",
                                        species == "cuniculus"~"(Rabbit) Oryctolagus cuniculus",
                                        species == "domesticus" ~ "(Pig) Sus domesticus",
                                        species == "norvegicus"~"(Rat) Rattus norvegicus")))  

#### Endpoint Category Setup ####

human_endpoint <- human_setup %>% 
  group_by(vivo_h_f, lvl1_h_f,lvl2_h_f,lvl3_h_f,bio_h_f) %>% 
  summarise()

#### User Interface ####

ui <- fluidPage(theme = shinytheme("flatly"),  
                
# App title
titlePanel(tagList(span((h1("Microplastics Toxicity Database: Human Health"))),
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
                                         h4("Biological Factors"))),
                                 
                                 
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
                                  
#### Endpoint Category UI ####

tabPanel("4: Endpoint Categorization", 
         h3("Endpoint Categorization of Toxicological Effects", align = "center"),
         br(),
         p("This plot displays the categorization of measured endpoints in the database. Nodes correspond to endpoints assessed in vitro and in vivo (orange), the Broad Endpoint Category (blue), 
         the Specific Endpoint Category (green), Endpoints (pink) and the level of biological organization (purple). Alternatively, the widget below may be used to select
         endpoints at various Biological Levels of Organization. Click nodes to expand and collapse the plot."),
         br(),
         
         column(width = 12,
                
                column(width = 3, 
                       pickerInput(inputId = "bio_check_endpoint", # bio org checklist
                                   label = "Level of Biological Organization", 
                                   choices = levels(human_endpoint$bio_h_f),
                                   selected = levels(human_endpoint$bio_h_f),
                                   options = list(`actions-box` = TRUE),
                                   multiple = TRUE)),
         ), #closes out button column
         
         column(width = 12,
                
                #Go button
                column(width = 3,
                       actionButton("go_endpoint", "Update Filters", class = "btn-success")), # adds update action button
                
         ), #closes out button column
         
         #collapsible tree plot
         collapsibleTreeOutput("plot", height = "800px"),
         
), #closes out tab

#### Study Screening UI ####

tabPanel("5: Study Screening", 
         h3("Study Screening Results for In Vivo Ingestion Studies", align = "center"),
         br(),
         p("This plot displays scores from the", a(href ="https://tger.co.uk/research", 'study prioritization screening tool', .noOWs = "outside"), "developed by Gouin et al. (In prep). For more information, including the scoring rubric used, see the document 'Study Screening Scoring Criteria' under the Resources tab."),
         br(),
         column(width = 12,
                # widget headers
                column(width=12,
                       
                       column(width = 3,
                              h4("Effects")),
                       
                       column(width = 3,
                              h4("Particle Characteristics")),
                       
                       column(width = 3,
                              h4("Biological Factors"))),
                
                
                # widgets
                column(width = 12,
                       
                       column(width = 3,
                              pickerInput(inputId = "lvl1_h_quality", # endpoint checklist
                                          label = "Broad Endpoint Category:", 
                                          choices = levels(human_setup$lvl1_h_f),
                                          selected = levels(human_setup$lvl1_h_f),
                                          options = list(`actions-box` = TRUE), # option to de/select all
                                          multiple = TRUE)), # allows for multiple inputs
                       column(width = 3,
                              pickerInput(inputId = "poly_h_quality", # polymer checklist
                                          label = "Polymer:", 
                                          choices = levels(human_setup$poly_h_f),
                                          selected = levels(human_setup$poly_h_f),
                                          options = list(`actions-box` = TRUE), 
                                          multiple = TRUE)),
                       
                       column(width = 3,  
                              pickerInput(inputId = "species_h_quality", # polymer checklist
                                          label = "Species:", 
                                          choices = levels(human_setup$species_h_f),
                                          selected = levels(human_setup$species_h_f),
                                          options = list(`actions-box` = TRUE), 
                                          multiple = TRUE))),
                
                
                # New row of widgets
                column(width = 12,
                       
                       column(width = 3,
                              htmlOutput("secondSelection_quality")), # dependent endpoint checklist
                       
                       column(width = 3,
                              pickerInput(inputId = "shape_h_quality", # shape checklist
                                          label = "Shape:", 
                                          choices = levels(human_setup$shape_h_f),
                                          selected = levels(human_setup$shape_h_f),
                                          options = list(`actions-box` = TRUE), 
                                          multiple = TRUE)),
                       
                       column(width = 3,
                              pickerInput(inputId = "bio_h_quality", # bio org checklist
                                          label = "Level of Biological Organization", 
                                          choices = levels(human_setup$bio_h_f),
                                          selected = levels(human_setup$bio_h_f),
                                          options = list(`actions-box` = TRUE),
                                          multiple = TRUE))),
                
                # New row of widgets
                column(width = 12,
                       
                       column(width = 3,
                              pickerInput(inputId = "effect_h_quality",  # Effect Yes/No widget
                                          label = "Effect:",
                                          choices = levels(human_setup$effect_h_f),
                                          selected = levels(human_setup$effect_h_f),
                                          options = list(`actions-box` = TRUE),
                                          multiple = TRUE)),
                       
                       column(width = 3,
                              pickerInput(inputId = "size_h_quality", # Environment checklist
                                          label = "Size Category:", 
                                          choices = levels(human_setup$size_h_f),
                                          selected = levels(human_setup$size_h_f),
                                          options = list(`actions-box` = TRUE), 
                                          multiple = TRUE)),
                       
                       column(width = 3,
                              pickerInput(inputId = "life_h_quality", # life stage checklist
                                          label = "Life Stages:", 
                                          choices = levels(human_setup$life_h_f),
                                          selected = levels(human_setup$life_h_f),
                                          options = list(`actions-box` = TRUE), 
                                          multiple = TRUE))),
                
                #Go Button and Reset Button
                
                column(width = 12,
                       br(),
                       column(width = 3,
                              actionButton("go_quality", "Update Filters", class = "btn-success")),
                       
                       column(width = 3,
                              actionButton("reset_quality", "Reset Filters"))),
                
                #Button Text
                
                column(width = 12,
                       column(width=3,  
                              strong(p("To Begin: Click the 'Update Filters' button above."))),
                       
                       column(width=3,  
                              strong(p("To Reset: Click the 'Reset Filters' button above, followed by the 'Update Filters' button to the left.")))),
                                          
                 
         ), #closes out button column
         br(),
         # build plotly
         
         fluidRow(
           column(12,plotlyOutput("quality_plot", height = "1500px")),
         
         ) # closes out fluidRow
         
         
         #This is the new tab for the quality screening figure
         
), #closes out tab
                  
#### Resources UI ####

tabPanel("6: Resources", 
         br(),     
         h3(align = "center", a(href = "https://sccwrp-my.sharepoint.com/:b:/g/personal/leahth_sccwrp_org/EYUFX1dOfSdGuHSfrUDcnewBxgttfTCOwom90hrt5nx1FA?e=jFXEyQ", 'Data Category Descriptions')),
         br(),
         h3(align = "center", a(href = "https://sccwrp-my.sharepoint.com/:b:/g/personal/leahth_sccwrp_org/EcJuT9TQeP1LkWAO81pE1DgBTxem_6m_gEh0DPXRexHHhA?e=LucgNJ", 'Study Screening Scoring Criteria')),
         br(),
         h3(align = "center", a(href = "https://sccwrp-my.sharepoint.com/:b:/g/personal/leahth_sccwrp_org/ER4Blg_W9LtDi-2vJGUK6kcBOdZ-GTgA-HZV4swPwD4bJQ?e=WPHXdZ", 'Human Health Study List')),
         br(),
         h3(align = "center", a(href = "https://sccwrp-my.sharepoint.com/:b:/g/personal/leahth_sccwrp_org/EXf0crCKDPVHo5xBEdw4PQwBxA8cnu0x4WY477CuEzZcPw?e=qs00V3", 'Dose Conversion Methods'))),
         
#### Contact UI ####

tabPanel("7: Contact", 
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
      filter(species_h_f %in% species_h_c)  #filter by species
      
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
                      "boxplot" 	= geom_boxplot(alpha = 0.8, aes(color = effect_h_f)),
                      "violin" = geom_violin(alpha = 0.8, aes(color = effect_h_f)),
                      "beeswarm" = geom_quasirandom(alpha = 0.8, aes(color = effect_h_f), method = "smiley", groupOnX = FALSE, cex = 2) #groupOnX specifies groups on y axis
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
                      "boxplot" 	= geom_boxplot(alpha = 0.8, aes(color = effect_h_f)),
                      "violin" = geom_violin(alpha = 0.8, aes(color = effect_h_f)),
                     "beeswarm" = geom_quasirandom(alpha = 0.8, aes(color = effect_h_f), method = "smiley", groupOnX = FALSE, cex = 2)) #groupOnX specifies groups on y axis)
    
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
                      "boxplot" 	= geom_boxplot(alpha = 0.8, aes(color = effect_h_f)),
                      "violin" = geom_violin(alpha = 0.8, aes(color = effect_h_f)),
                      "beeswarm" = geom_quasirandom(alpha = 0.8, aes(color = effect_h_f), method = "smiley", groupOnX = FALSE, cex = 2)) #groupOnX specifies groups on y axis)
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
                      "boxplot" 	= geom_boxplot(alpha = 0.8, aes(color = effect_h_f)),
                      "violin" = geom_violin(alpha = 0.8, aes(color = effect_h_f)),
                      "beeswarm" = geom_quasirandom(alpha = 0.8, aes(color = effect_h_f), method = "smiley", groupOnX = FALSE, cex = 2)) #groupOnX specifies groups on y axis)
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
                      "boxplot" 	= geom_boxplot(alpha = 0.8, aes(color = effect_h_f)),
                      "violin" = geom_violin(alpha = 0.8, aes(color = effect_h_f)),
                      "beeswarm" = geom_quasirandom(alpha = 0.8, aes(color = effect_h_f), method = "smiley", groupOnX = FALSE, cex = 2)) #groupOnX specifies groups on y axis)
    
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
                      "boxplot" 	= geom_boxplot(alpha = 0.8, aes(color = effect_h_f)),
                      "violin" = geom_violin(alpha = 0.8, aes(color = effect_h_f)),
                      "beeswarm" = geom_quasirandom(alpha = 0.8, aes(color = effect_h_f), method = "smiley", groupOnX = FALSE, cex = 2)) #groupOnX specifies groups on y axis)
    
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
    
  }) #If we add more widgets, make sure they get added here.   
  
  #### Endpoint Category S ####
  
  human_filter_endpoint <- eventReactive(list(input$go_endpoint),{
    
    # biological organization widget
    bio_c_endpoint <- input$bio_check_endpoint # assign bio values to "bio_c"
    
    human_endpoint %>% # take original dataset
      filter(bio_h_f %in% bio_c_endpoint) #filter by bio organization
    
  })
  
  output$plot <- renderCollapsibleTree({
    
    collapsibleTree(human_filter_endpoint(), root = "Mammalian Database", hierarchy = c("vivo_h_f", "lvl1_h_f", "lvl2_h_f", "lvl3_h_f", "bio_h_f"),
                    fontSize = 16, zoomable = FALSE,    
                    fill = c(
                      # The root
                      "seashell",
                      # vivo
                      rep("orange", length(unique(human_filter_endpoint()$vivo_h_f))),
                      # lvl1
                      rep("turquoise", length(unique(paste(human_filter_endpoint()$vivo_h_f, human_filter_endpoint()$lvl1_h_f)))),
                      # lvl2
                      rep("palegreen", length(unique(paste(human_filter_endpoint()$vivo_h_f,human_filter_endpoint()$lvl1_h_f, human_filter_endpoint()$lvl2_h_f)))),
                      # lvl3
                      rep("hotpink", length(unique(paste(human_filter_endpoint()$vivo_h_f, human_filter_endpoint()$lvl1_h_f, human_filter_endpoint()$lvl2_h_f, human_filter_endpoint()$lvl3_h_f)))),
                      # bio org
                      rep("orchid", length(unique(paste(human_filter_endpoint()$vivo_h_f,human_filter_endpoint()$lvl1_h_f, human_filter_endpoint()$lvl2_h_f, human_filter_endpoint()$lvl3_h_f, human_filter_endpoint()$bio_h_f))))))
  })
  
##### Quality Scores #####
  
  #Create dependent dropdown checklists: select lvl2 by lvl1.
  output$secondSelection_quality <- renderUI({
    
    lvl1_h_c <- input$lvl1_h_quality # assign level values to "lvl1_c"
    
    human_new <- human_setup %>% # take original dataset
      filter(lvl1_h_f %in% lvl1_h_c) %>% # filter by level inputs
      mutate(lvl2_f_new = factor(as.character(lvl2_h_f))) # new subset of factors
    
    pickerInput(inputId = "lvl2_h_quality", 
                label = "Specific Endpoint within Broad Category:", 
                choices = levels(human_new$lvl2_f_new),
                selected = levels(human_new$lvl2_f_new),
                options = list(`actions-box` = TRUE),
                multiple = TRUE)})
  
quality_filtered <- eventReactive(list(input$go_quality),{
  
  # every selection widget should be represented as a new variable below
  lvl1_h_c <- input$lvl1_h_quality # assign level values to "lvl1_c"
  lvl2_h_c <- input$lvl2_h_quality # assign lvl2 values to "lvl2_c"
  bio_h_c <- input$bio_h_quality # assign bio values to "bio_c"
  effect_h_c <- input$effect_h_quality # assign effect values to "effect_c"
  life_h_c <- input$life_h_quality #assign values to "life_quality"
  poly_h_c <- input$poly_h_quality # assign values to "poly_c"
  shape_h_c <- input$shape_h_quality # assign values to "shape_c" 
  size_h_c <- input$size_h_quality # assign values to "size_c"
  species_h_c<-input$species_h_quality #assign values to "species_h_c"#assign values to "species_h_c"
  
  #make summary dataset to display in heatmap below
  human_setup %>%
    #only in vivo Ingestion studies are scored
    filter(lvl1_h_f %in% lvl1_h_c) %>% # filter by level inputs
    filter(lvl2_h_f %in% lvl2_h_c) %>% #filter by level 2 inputs 
    filter(bio_h_f %in% bio_h_c) %>% #filter by bio organization
    filter(effect_h_f %in% effect_h_c) %>% #filter by effect
    filter(life_h_f %in% life_h_c) %>% #filter by life stage
    filter(poly_h_f %in% poly_h_c) %>% #filter by polymer
    filter(shape_h_f %in% shape_h_c) %>% #filter by shape
    filter(size_h_f %in% size_h_c) %>% #filter by size class
    filter(species_h_f %in% species_h_c) %>%   #filter by species
    mutate(Study = paste0(authors, " (", year,")")) %>% 
    distinct(Study, doi, genus, species, life_h_f, vivo_h_f, exposure.category, particle.1, particle.2, particle.3, particle.4, particle.5, particle.6, particle.7, 
             design.1, design.2, design.3, design.4, design.5, design.6, design.7, design.8, design.9, design.10, design.11, design.12, design.13,
             risk.1, risk.2, risk.3, risk.4, risk.5, risk.6) %>% 
    drop_na() %>% 
    pivot_longer(!c(Study, doi, genus, species, life_h_f, vivo_h_f, exposure.category),
                 names_to ="Criteria", 
                 values_to ="Score") %>%
    mutate(Score_f = factor(case_when(Score == 0 ~ "Inadequate",
                                      Score == 1 ~ "Adequate with Restrictions",
                                      Score == 2 ~ "Adequate"))) %>% 
    mutate(Category = case_when(Criteria == "particle.1" ~ "Particle Characteristics",
                                Criteria == "particle.2" ~ "Particle Characteristics",
                                Criteria == "particle.3" ~ "Particle Characteristics",
                                Criteria == "particle.4" ~ "Particle Characteristics",
                                Criteria == "particle.5" ~ "Particle Characteristics",
                                Criteria == "particle.6" ~ "Particle Characteristics",
                                Criteria == "particle.7" ~ "Particle Characteristics",
                                Criteria == "design.1" ~ "Experimental Design",
                                Criteria == "design.2" ~ "Experimental Design",
                                Criteria == "design.3" ~ "Experimental Design",
                                Criteria == "design.4" ~ "Experimental Design",
                                Criteria == "design.5" ~ "Experimental Design",
                                Criteria == "design.6" ~ "Experimental Design",
                                Criteria == "design.7" ~ "Experimental Design",
                                Criteria == "design.8" ~ "Experimental Design",
                                Criteria == "design.9" ~ "Experimental Design",
                                Criteria == "design.10" ~ "Experimental Design",
                                Criteria == "design.11" ~ "Experimental Design",
                                Criteria == "design.12" ~ "Experimental Design",
                                Criteria == "design.13" ~ "Experimental Design",
                                Criteria == "risk.1" ~ "Risk Assessment",
                                Criteria == "risk.2" ~ "Risk Assessment",
                                Criteria == "risk.3" ~ "Risk Assessment",
                                Criteria == "risk.4" ~ "Risk Assessment",
                                Criteria == "risk.5" ~ "Risk Assessment",
                                Criteria == "risk.6" ~ "Risk Assessment")) %>%
    #Set order of categories so they plot in correct order
    mutate(Category_f = factor(Category, levels = c("Particle Characteristics","Experimental Design", "Risk Assessment"))) %>% 
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
    #set order of criteria so theeeeey plot in correct order - they have to be in reverse here
    mutate(Criteria_f = factor(Criteria, levels = c("Test Particle Relevance","Effect Thresholds*","Concentration Range","Dose-Response*","Endpoints*","Statistical Analysis",
                                                    "Internal Dose Confirmation","Replicates","Controls*","Frequency/Duration of Exposure*","Sample Size*","Feeding/Housing Conditions",
                                                    "Test Species*","Administration Route*","Homogeneity of Exposure","Administered Dose*","Test Vehicle*","Particle Stability",
                                                    "Concentration Units","Microbial Contamination","Chemical Purity","Surface Chemistry","Particle Source*","Polymer Type*","Particle Shape*","Particle Size*"))) %>% 
    mutate(RedCriteria = case_when(Criteria == "particle.1" ~ "Y",
                                   Criteria == "particle.2" ~ "Y",
                                   Criteria == "particle.3" ~ "Y",
                                   Criteria == "particle.4" ~ "Y",
                                   Criteria == "particle.5" ~ "N",
                                   Criteria == "particle.6" ~ "N",
                                   Criteria == "particle.7" ~ "N",
                                   Criteria == "design.1" ~ "N",
                                   Criteria == "design.2" ~ "N",
                                   Criteria == "design.3" ~ "Y",
                                   Criteria == "design.4" ~ "Y",
                                   Criteria == "design.5" ~ "N",
                                   Criteria == "design.6" ~ "Y",
                                   Criteria == "design.7" ~ "Y",
                                   Criteria == "design.8" ~ "N",
                                   Criteria == "design.9" ~ "Y",
                                   Criteria == "design.10" ~ "Y",
                                   Criteria == "design.11" ~ "Y",
                                   Criteria == "design.12" ~ "N",
                                   Criteria == "design.13" ~ "N",
                                   Criteria == "risk.1" ~ "N",
                                   Criteria == "risk.2" ~ "Y",
                                   Criteria == "risk.3" ~ "Y",
                                   Criteria == "risk.4" ~ "N",
                                   Criteria == "risk.5" ~ "Y",
                                   Criteria == "risk.6" ~ "N")) %>% 
    mutate(RedCriteria_f = factor(RedCriteria, levels = c("Y","N")))

})
  
##### **Build Plotly -----
quality_plotly <- eventReactive(list(input$go_quality),{

#build ggplot from filtered dataset above
quality_filtered() %>%   
    ggplot(aes(Study, Criteria_f)) + 
    geom_tile(aes(fill = Score_f,
                  #define text for hover-over
                  text = paste("Study:", Study, "\n",
                               "Criteria:", Criteria_f, "\n",
                               "Category:", Category_f, "\n",
                               "Score:", Score_f, "\n",
                               "Organism:", genus, species, "\n",
                               "Life Stage:", life_h_f, "\n",
                               "Type:", vivo_h_f, "\n",
                               "Exposure:", exposure.category, "\n",
                               "DOI:", paste0(doi), "\n")),
                  color = "white", size = 0.25) +
    theme_ipsum() +
    scale_fill_manual(name = "Score",
                      values = c("dodgerblue4","deepskyblue1","#ebcccd")) +
    labs(title = "Screening & Prioritization Scores (In Vivo, Ingestion Studies Only)") +
    coord_cartesian(clip = "off") + # This keeps the labels from disappearing
    theme_minimal(base_size = 12) +
    scale_y_discrete(labels = label_wrap(30)) +
    facet_grid(Category_f ~ ., scales = "free", space = "free") +
    theme(
      #axis.text.y = element_text(face = ifelse(RedCriteria_f == "Y", "bold", "plain")), #I want to make the red criteria bold but this won't work :(
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.minor=element_blank(),
          panel.grid.major=element_blank(),
          axis.text.x = element_text(angle = 60, vjust = 0.5, hjust = .5),
          plot.title = element_text(hjust = 0.5)) %>% 
    req(nrow(quality_filtered()) > 0) #suppresses warning message text
})

##### **Render Plotly -----
output$quality_plot <- renderPlotly({
  ggplotly(quality_plotly(), tooltip = c("text"))
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

}) #If we add more widgets, make sure they get added here.
  
} #Server end

#### Full App ####
shinyApp(ui = ui, server = server)

# End of R Shiny app script.
