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

# Load finalized dataset.

human <- read_csv("Humans_Clean_Final.csv", guess_max = 10000)

#### Introduction Setup ####

# All text inputs below.

#### Overview Human Setup ####

Final_effect_dataset <- read_csv("Final_effect_datasetH.csv")%>%
  mutate(plot_f = case_when(
    plot_f == "Polymer" ~ "Polymer",
    plot_f == "Size" ~ "Size",
    plot_f == "Shape" ~ "Shape",
    plot_f == "Organism" ~ "Organism",
    plot_f == "Lvl1" ~ "Endpoint Category",
    plot_f == "Life.stage" ~ "Life Stage",
    plot_f == "Invivo.invivo" ~ "In Vivo or In Vitro",
    plot_f == "Exposure.route" ~ "Exposure Route"))%>%
  mutate(plot_f = factor(plot_f))%>%
  mutate(logEndpoints = log(Endpoints))%>%
  rename(Percent = Freq)

# Adding function for multiple graph output.
# Code adapted from https://gist.github.com/wch/5436415/ and comment at https://gist.github.com/wch/5436415/#gistcomment-1608976 .

# Creates function called "get_plot_output_list" where the input variable is "input_n".
get_plot_output_list <- function(input_n) {
  
  # For every value in "input_n", insert it as "i" into the function below and then save the full output into "plot_output_list":
  plot_output_list <- lapply(input_n, function(i) {
    
    # Render the individual plots      
    renderPlotly({
      
      # use the original dataset
      Final_effect_dataset %>%
        
        # filter by input
        filter(plot_f==i) %>%
        
        # generate plot
        ggplot(aes(fill=effect, y= logEndpoints, x=Type, Percent=Percent)) +
        geom_bar(position="stack", stat="identity") +
        geom_text(aes(label= paste0(Endpoints)), position = position_stack(vjust = 0.5),colour="black") +
        scale_fill_manual(values = cal_palette(case_when(i=="Polymer"~"wetland", i=="Organism"~"sbchannel", i=="Size"~"seagrass",i=="Shape"~"gayophytum",i=="Endpoint Category"~"figmtn",i=="Life Stage"~"dudleya",i=="Exposure Route"~"halfdome",i=="In Vivo or In Vitro"~"kelp2")))+
        theme_classic() +
        ylab("Number of Endpoints Measured") +
        labs(fill="Effect") +
        guides(x = guide_axis(n.dodge = 2)) +
        ggtitle(case_when(i=="Polymer"~"Polymer", i=="Organism"~"Organism", i=="Size"~"Particle Size",i=="Shape"~"Shape",i=="Endpoint Category"~"Endpoint Category",i=="Life Stage"~"Life Stage",i=="Exposure Route"~"Exposure Route",i=="In Vivo or In Vitro"~"In Vivo or In vitro"))+
        theme(plot.title = element_text(hjust = 0.5, face="bold"))+
        theme(legend.position = "right",
              axis.ticks= element_blank(),
              axis.text.x = element_text(angle=45, size = 10),
              axis.text.y = element_blank(),
              axis.title.x = element_blank())
      
      ggplotly(tooltip = 'Percent')%>%
        config(displayModeBar = FALSE)
      
    })
    
  })
  
  do.call(tagList, plot_output_list) # Need to call it as a list to display properly.
  
  return(plot_output_list) # Returns the full list of stored plots.
}






#### Exploration Human Setup ####

human_v1 <- human %>% # start with original dataset
  # full dataset filters.
  mutate(effect_h_f = factor(case_when(effect == "Y" ~ "Yes",
                                       effect == "N" ~ "No"),
                             levels = c("No", "Yes"))) %>%
  mutate(invitro_h_f = factor(case_when(invitro.invivo == "invitro"~"Invitro",
                                        invitro.in)))
  
  # removing NAs to make data set nicer
  replace_na(list(size.category = 0, shape = "Not Reported", polymer = "Not Reported", exposure.route = "Not Applicable", life.stage = "Not Reported")) 

human_setup <- human_v1 %>% # start with original dataset
  mutate(size_h_f = factor(case_when(
    size.category == 1 ~ "1nm < 100nm",
    size.category == 2 ~ "100nm < 1µm",
    size.category == 3 ~ "1µm < 100µm",
    size.category == 4 ~ "100µm < 1mm",
    size.category == 0 ~ "Not Reported"), 
    levels = c("1nm < 100nm", "100nm < 1µm", "1µm < 100µm", "100µm < 1mm", "Not Reported"))) %>% # creates new column with nicer names and order by size levels.
  # shape category data tidying.
  mutate(shape_h_f = factor(case_when(
    shape == "fragment" ~ "Fragment",
    shape == "sphere" ~ "Sphere",
    shape == "Not Reported" ~ "Not Reported"),
    levels = c("Fragment", "Sphere", "Not Reported"))) %>% # order our different shapes.
  # polymer category data tidying.
  mutate(poly_h_f = factor(case_when(
    polymer == "PA" ~ "Polyamide",
    polymer == "PE" ~ "Polyethylene",
    polymer == "PMMA" ~ "Polymethylmethacrylate",
    polymer == "PP" ~ "Polypropylene",
    polymer == "PS" ~ "Polystyrene",
    polymer == "PUR" ~ "Polyurathane",
    polymer == "PVC" ~ "Polyvinylchloride",
    polymer == "TR" ~ "Tire Rubber",
    polymer == "Not Reported" ~ "Not Reported"))) %>%
  # taxonomic category data tidying.
  
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
                                     lvl1 == "stress" ~ "Stress"))) %>% # creates new column with nicer names.
  # Level 2 Data tidying
  mutate(lvl2_h_f = factor(case_when(lvl2 == "actinobacteria" ~ "Actinobacteria",
                                     lvl2 == "amino.acid.metabolism" ~ "Amino Acid Metabolism",
                                     lvl2 == "apoptosis.cell.cycle"~"Apoptosis and Cell Cycle",
                                     lvl2 == "bacteroidetes"~ "Bacteriodetes",
                                     lvl2 == "bile.acid" ~ "Bile Acid",
                                     lvl2 == "body.condition"~"Body Condition",
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
                                     lvl2 == "gen.stress" ~ "General Stress",
                                     lvl2 == "hemolysis" ~ "Hemolysis",
                                     lvl2 == "immune.cells"~"Immune Cells",
                                     lvl2 == "immune.other"~"Immune Other ",
                                     lvl2 == "inflammation" ~ "Inflammation",
                                     lvl2 == "intestinal.inflammation" ~ "Intestinal Inflammation",
                                     lvl2 == "intestinal.ion.transport" ~ "Intestinal Ion Transport",
                                     lvl2 == "intestinal.muscus.secretion" ~ "Intestinal Mucus Secretion",
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
                                     lvl2 == "respiration"~"Respiration",
                                     lvl2 == "spleen.histo" ~ "Spleen Histological Abnormalities",
                                     lvl2 == "tenericutes" ~ "Tenericutes",
                                     lvl2 == "thyroid" ~ "Thyroid",
                                     lvl2 == "verrucomicrobiae" ~ "Verrucomicrobiae",
                                     lvl2 == "vision" ~ "Vision"))) %>% #Renames for widget
  mutate(bio_h_f = factor(case_when(bio.org == "cell"~"Cell", #Bio Org Data Tidying
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
  mutate(exposure_route_h_f = factor(case_when(exposure.route == "dermal" ~ "Dermal",
                                               exposure.route == "drinking.water" ~ "Drinking Water",
                                               exposure.route == "food" ~ "Food",
                                               exposure.route == "gavage" ~ "Gavage",
                                               exposure.route == "gestation" ~ "Gestation",
                                               exposure.route == "gestation,lactation" ~ "Gestation & Lactation",
                                               exposure.route == "inhalation" ~ "Inhalation",
                                               exposure.route == "intratracheal.instillation" ~ "Intratracheal Instillation",
                                               exposure.route == "iv.injection" ~ "IV Injection",
                                               exposure.route ==  "Not Applicable"~"Not Applicable (in vitro)")))

#### User Interface ####

ui <- fluidPage(theme = shinytheme("flatly"),  
                
                # App title
                titlePanel(h1("Microplastics Toxicity Database: Mammals")),
                
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
                                               
                                               strong(p("This database is a repository for microplastics 
                      toxicity data that will be used to generate key graphics for the Microplastics Health Effects Workshop.")), 
                                               
                                               p("This web application allows users to explore toxicity 
                    data using an intuitive interface while retaining the diversity and complexity inherent 
                    to microplastics. Data is extracted from existing, peer-reviewed manuscripts containing 
                    toxicity data pertaining to microplastics."),
                                               
                                               p("Use the numbered tabs at the top of the page to navigate to each section. Each section provides different information or data visualization options. 
                      More specific instructions may be found within each section."),
                                               
                                               h3("Why was the Microplastics Toxicity Database and Web Application created?", align = "center"), #Section 2
                                               
                                               p("The database and application tools have been created for use by the participants of the ", a(href = "https://www.sccwrp.org/about/
                      research-areas/additional-research-areas/
                      trash-pollution/microplastics-health-effects-webinar-series/", 'Microplastics Health Effects Workshop', 
                                                                                                                                               .noWS = "outside"),".The purpose of this workshop is to identify the most sensitive and biologically critical endpoints associated with microplastics exposure, 
                      prioritize which microplastics characteristics (e.g., size, shape, polymer) that are of greatest biological concern, and identify 
                      critical thresholds for each at which those biological effects become pronounced. Workshop participants will also make reccomendations for future
                      research investments. Workshop findings will be published in a special issue of ", a(href ="https://microplastics.springeropen.com/", 'Microplastics and Nanoplastics', .noOWs = "outside"),". 
                      These findings will be used directly by the state of California to fulfill ", a(href = "https://www.sccwrp.org/about/research-areas/
                      additional-research-areas/trash-pollution/microplastics-health-effects-webinar-series/history-california-microplastics-legislation/", 'legislative mandates', 
                                                                                                      .noWS = "outside")," regarding the management of microplastics in drinking water and the aquatic environment."),
                                               
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
                                                           tags$a(href="https://www.swccrp.org", tags$img(src="sccwrp.png", width = "100%", height = "100%")),
                                                           tags$a(href="https://www.utoronto.ca", tags$img(src="toronto.png", width = "100%", height = "100%")),
                                                           tags$a(href="https://www.sfei.org/", tags$img(src="sfei.png", width = "100%", height = "100%"))),
                                               
                                               br(), 
                                               
                                               verbatimTextOutput(outputId = "Introduction1")),

#### Overview Human UI ####
                                      
                                      tabPanel("2: Overview", 
                                               br(), 
                                               h3("Overview of Toxicological Effects in Human Systems", align = "center"),
                                               br(),
                                               p("Check the boxes below to visualize figures. Each bar displays the total number of measured endpoints within the database. Measured endpoints where a statistically signifcant effect was detected as indicated by 'Y' or where a measurement was made but a significant effect was not detected 'N'."), 
                                               br(),
                                               p("Use the drop down menu at the top of the page to visualize different figures. Hover the cursor over each stacked bar to display the number of measured endpoints that are currently included in the database. 
           Click on the legend to select data."),
                                               br(), 
                                               p("Detailed descriptions of data categories may be found under the Resources tab."),
                                               br(),
                                               
                                               pickerInput(inputId = "Emily_check", # endpoint checklist
                                                           label = "Overview", 
                                                           choices = levels(Final_effect_dataset$plot_f),
                                                           selected = levels(Final_effect_dataset$plot_f), 
                                                           options = list(`actions-box` = TRUE), # option to de/select all
                                                           multiple = TRUE), # allows for multiple inputs
                                               br(),
                                               
                                               uiOutput(outputId= "Emily_plot")),
                                      
                                      
#### Exploration Human UI ####
                                      tabPanel("3: Exploration",
                                               
                                               shinyjs::useShinyjs(), # requires package for "reset" button, DO NOT DELETE - make sure to add any new widget to the reset_input in the server
                                               id = "heili-tab", # adds ID for resetting Heili's tab's filters
                                               
                                               h3("Exploration of Toxicological Effects in Mammalian Systems", align = "center"),
                                               br(), 
                                               p("Each figure displays a different metric along the y-axis - broad endpoint category, specific endpoint category, size, shape, and polymer, respectively. All doses are displayed in mass per volume. Doses 
                    were either reported in mass per volume or converted from doses originally presented as particle count per volume."),
                                               br(),
                                               p("The data displayed in these figures are not filtered for quality and only display data where doses were reported as 
                      mass per volume - other dosing units (e.g., particle mass/food mass) 
                      are not displayed but are available in the complete database file."),
                                               br(), 
                                               p("Filter the data: The data may be filtered using the drop-down menus located below. Then, click the 'Update Filters' button to refresh the data displayed according to your selections."),
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
                                               
                                               #column(width = 3,
                                               #pickerInput(inputId = "organism_check", # organismal checklist
                                               #label = "Organisms:", 
                                               #choices = levels(human_setup$org_f),
                                               #selected = levels(human_setup$org_f),
                                               #options = list(`actions-box` = TRUE), 
                                               #multiple = TRUE))), 
                                               
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
                                               
                                               #column(width = 3,
                                               #pickerInput(inputId = "env_check", # Environment checklist
                                               #label = "Environment:", 
                                               #choices = levels(human_setup$env_f),
                                               #selected = levels(human_setup$env_f),
                                               #options = list(`actions-box` = TRUE), 
                                               #multiple = TRUE))), 
                                               
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
                                               
                                               # New row of widgets
                                               column(width=12,
                                                      
                                                      #column(width = 3),
                                                      
                                                      #Slider Widget - commented out for now
                                                      #column(width = 3,
                                                      #sliderInput("range", # Allows for max input
                                                      #label = "Particle Size (µm):", #Labels widget
                                                      #min = 0, max = 4000, value = 4000)),
                                                      
                                                       
                                                      
                                                      #In vitro/in vivo widget - commented out for now
                                                    column(width = 3, offset = 6,  
                                                      pickerInput(inputId = "vivo_h_check", 
                                                                         label = "In Vitro or In Vivo:",
                                                                         choices = levels(human_setup$vivo_h_f),
                                                                         selected = levels(human_setup$vivo_h_f),
                                                                         options = list(`actions-box` = TRUE),
                                                                         multiple = TRUE))),
                                               
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
                                                             plotOutput(outputId = "lvl_h_plot_react"),
                                                             br())), 
                                               
                                               column(width = 12,
                                                      
                                                      column(width = 12,
                                                             plotOutput(outputId = "lvl2_h_plot_react"),
                                                             br())), 
                                               
                                               column(width = 12,
                                                      
                                                      column(width = 12,
                                                             plotOutput(outputId = "size_h_plot_react"),
                                                             br())), 
                                               
                                               column(width = 12,
                                                      
                                                      column(width = 12,
                                                             plotOutput(outputId = "shape_h_plot_react"),
                                                             br())), 
                                               
                                               column(width = 12,
                                                      
                                                      column(width = 12,
                                                             plotOutput(outputId = "poly_h_plot_react"),
                                                             br()))), 
                                      
                                      
                                      #### Resources UI ####
                                      
                                      tabPanel("4: Resources", 
                                               br(),     
                                               h3(align = "center", a(href = "https://sccwrp-my.sharepoint.com/:b:/g/personal/leahth_sccwrp_org/EYUFX1dOfSdGuHSfrUDcnewBxgttfTCOwom90hrt5nx1FA?e=jFXEyQ", 'Data Category Descriptions')),
                                               br(),
                                               h3(align = "center", a(href = "https://sccwrp-my.sharepoint.com/:b:/g/personal/leahth_sccwrp_org/ES_FUiwiELtNpWgrPCS1Iw4Bkn3-aeiDjZxmtMLjg3uv3g?e=bmuNgG", 'Human Study List')),
                                               
                                               verbatimTextOutput(outputId = "Leah2")),
                                      
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
  
  # Effect plot code for check box 
  
  # Insert the right number of plot output objects into the page using the function from the setup section.
  output$Emily_plot <- renderUI({ 
    
    # Using user-provided selections.
    get_plot_output_list(input$Emily_check) 
    
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
    vivo_h_c <- input$vivo_h_check
    #range_n <- input$range # assign values to "range_n"
    
    human_setup %>% # take original dataset
      filter(vivo_h_f %in% vivo_h_c) %>% #filter by invivo or invitro
      filter(lvl1_h_f %in% lvl1_h_c) %>% # filter by level inputs
      filter(lvl2_h_f %in% lvl2_h_c) %>% #filter by level 2 inputs 
      filter(bio_h_f %in% bio_h_c) %>% #filter by bio organization
      filter(effect_h_f %in% effect_h_c) %>% #filter by effect
      filter(life_h_f %in% life_h_c) %>% #filter by life stage
      filter(poly_h_f %in% poly_h_c) %>% #filter by polymer
      filter(shape_h_f %in% shape_h_c) %>% #filter by shape
      filter(size_h_f %in% size_h_c) %>% #filter by size class
      filter(exposure_route_h_f %in% exposure_route_h_c) #filter by exposure route
      #filter(vivo_h_f %in% vivo_h_c) #filter by invivo or invitro
       
      #filter(size.length.um.used.for.conversions <= range_n) #For size slider widget - currently commented out
    
  })
  
  
  # Use newly created dataset from above to generate plots for size, shape, polymer, and endpoint plots on four different rows.
  
  
  # Size Plot
  
  output$size_h_plot_react <- renderPlot({
    
    ggplot(human_filter(), aes(x = dose.mg.mL.nominal, y = size_h_f)) +
      geom_boxplot(alpha = 0.7, aes(color = effect_h_f, fill = effect_h_f)) +
      scale_x_log10(breaks = c(0.00000001, 0.000001, 0.0001, 0.01, 1, 100), 
                    labels = c(0.00000001, 0.000001, 0.0001, 0.01, 1, 100)) +
      scale_color_manual(values = c("#A1CAF6", "#4C6FA1")) +
      scale_fill_manual(values = c("#A1CAF6", "#4C6FA1")) +
      theme_classic() +
      theme(text = element_text(size=18), 
            legend.position = "right") +
      labs(x = "Concentration (mg/mL)",
           y = "Size",
           color = "Effect?",
           fill = "Effect?")
    
  })
  
  
  # Shape Plot
  
  output$shape_h_plot_react <- renderPlot({
    
    ggplot(human_filter(), aes(x = dose.mg.mL.nominal, y = shape_h_f)) +
      scale_x_log10(breaks = c(0.00000001, 0.000001, 0.0001, 0.01, 1, 100), 
                    labels = c(0.00000001, 0.000001, 0.0001, 0.01, 1, 100)) +
      geom_boxplot(alpha = 0.7, aes(color = effect_h_f, fill = effect_h_f)) +
      scale_color_manual(values = c("#C7EAE5","#35978F")) +
      scale_fill_manual(values = c("#C7EAE5", "#35978F")) +
      theme_classic() +
      theme(text = element_text(size=18), 
            legend.position = "right") +
      labs(x = "Concentration (mg/mL)",
           y = "Shape",
           color = "Effect?",
           fill = "Effect?")
    
  })
  
  # Polymer Plot
  
  output$poly_h_plot_react <- renderPlot({
    
    ggplot(human_filter(), aes(x = dose.mg.mL.nominal, y = poly_h_f)) +
      scale_x_log10(breaks = c(0.00000001, 0.000001, 0.0001, 0.01, 1, 100), 
                    labels = c(0.00000001, 0.000001, 0.0001, 0.01, 1, 100)) +
      geom_boxplot(alpha = 0.7, aes(color = effect_h_f, fill = effect_h_f)) +
      scale_color_manual(values = c("#FAB455", "#A5683C")) +
      scale_fill_manual(values = c("#FAB455", "#A5683C")) +
      theme_classic() +
      theme(text = element_text(size=18),
            legend.position = "right") +
      labs(x = "Concentration (mg/mL)",
           y = "Polymer",
           color = "Effect?",
           fill = "Effect?")
    
  })
  
  # Endpoint Plot
  
  output$lvl_h_plot_react <- renderPlot({
    
    ggplot(human_filter(), aes(x = dose.mg.mL.nominal, y = lvl1_h_f)) +
      scale_x_log10(breaks = c(0.00000001, 0.000001, 0.0001, 0.01, 1, 100), 
                    labels = c(0.00000001, 0.000001, 0.0001, 0.01, 1, 100)) +
      geom_boxplot(alpha = 0.7, aes(color = effect_h_f, fill = effect_h_f)) +
      scale_color_manual(values = c("#A99CD9", "#6C568C")) +
      scale_fill_manual(values = c("#A99CD9", "#6C568C")) +
      theme_classic() +
      theme(text = element_text(size=18),
            legend.position = "right") +
      labs(x = "Concentration (mg/mL)",
           y = "Endpoint",
           color = "Effect?",
           fill = "Effect?")
    
  })
  
  #Lvl2 Plot 
  
  output$lvl2_h_plot_react <- renderPlot({
    
    ggplot(human_filter(), aes(x = dose.mg.mL.nominal, y = lvl2_h_f)) +
      scale_x_log10(breaks = c(0.00000001, 0.000001, 0.0001, 0.01, 1, 100), 
                    labels = c(0.00000001, 0.000001, 0.0001, 0.01, 1, 100)) +
      geom_boxplot(alpha = 0.7, aes(color = effect_h_f, fill = effect_h_f)) +
      scale_color_manual(values = c("#A99CD9", "#6C568C")) +
      scale_fill_manual(values = c("#A99CD9", "#6C568C")) +
      theme_classic() +
      theme(text = element_text(size=18),
            legend.position = "right") +
      labs(x = "Concentration (mg/mL)",
           y = "Specific Endpoint",
           color = "Effect?",
           fill = "Effect?")
    
  })
  
  # Create downloadable csv of filtered dataset.
  # Removed columns created above so the dataset matches Leah's original dataset.
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(human_filter() %>%
                  select(-c(effect_h_f, size_h_f, shape_h_f, poly_h_f, lvl1_h_f, lvl2_h_f, bio_h_f, vivo_h_f, life_h_f, exposure_route_h_f)), 
                file, row.names = FALSE)
    }
  )
  
  # Create "reset" button to revert all filters back to what they began as.
  # Need to call all widgets individually by their ids.
  # See https://stackoverflow.com/questions/44779775/reset-inputs-with-reactive-app-in-shiny for more information.
  observeEvent(input$reset_input, {
    shinyjs::reset("lvl1_h_check")
    shinyjs::reset("poly_h_check")
    shinyjs::reset("shape_h_check")
    shinyjs::reset("effect_h_check")
    shinyjs::reset("size_h_check")
    shinyjs::reset("life_h_check")
    shinyjs::reset("bio_h_check")
    shinyjs::reset("vivo_h_check")
    shinyjs::reset("exposure_route_h_check")
  }) #If we add more widgets, make sure they get added here.   
  
} #Server end

#### Full App ####
shinyApp(ui = ui, server = server)

# End of R Shiny app script.
