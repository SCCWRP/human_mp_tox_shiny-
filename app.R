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

##### Load finalized dataset prepped in RDAmaker.R ####
##### SAVE DATA #####
human <- readRDS("human.RDS")
human_endpoint <- readRDS("human_endpoint.RDS")
human_quality <- readRDS("human_quality.RDS")
human_search <- readRDS("human_search.RDS")
human_setup <- readRDS("human_setup.RDS")
human_v1 <- readRDS("human_v1.RDS")


##### Load functions #####
source("functions.R")


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
                     menuItem("Data Submission", tabName = "Submission", icon = icon("fas fa-file-upload")),
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
                     
                     p("A full length description of the database and web application is published in ", 
                       a(href = "https://www.springeropen.com/collections/sccwrp", 'Microplastics and Nanoplastics'),
                       ". To access the open access manuscript, ", a(href = "https://microplastics.springeropen.com/articles/10.1186/s43591-022-00032-4", 'click here'),"."),
                     
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
            
            p(align = "center", a(href = "https://agency.calepa.ca.gov/staffdirectory/detail.asp?UID=69294&BDO=7&VW=DET&SL=S", 'Dr. Scott Coffin'),", California State Water Resources Control Board", 
              tags$a(href="https://twitter.com/DrSCoffin", icon("twitter")), tags$a(href="https://github.com/ScottCoffin", icon("github"))),
            
            p(align = "center", a(href = "https://www.heililowman.com/", 'Dr. Heili Lowman'),", University of Nevada Reno ", 
              tags$a(href="https://twitter.com/heili_lowman", icon("twitter")), tags$a(href="https://github.com/hlowman", icon("github"))), 
            
            p(align = "center", a(href = "https://www.sccwrp.org/about/staff/emily-darin/", 'Emily Darin'),", Southern California Coastal Water Research Project",
              tags$a(href="https://github.com/EmilyDarin", icon("github"))),
            
            p(align = "center", a(href = "https://www.sfei.org/users/liz-miller", 'Dr. Ezra Miller'),", San Franciso Estuary Institute"),
            
            p(align = "center", a(href = "https://rochmanlab.com/people/", 'Dr. Ludovic Hermabessiere'),", University of Toronto", 
              tags$a(href="https://twitter.com/HermabessiereL", icon("twitter"))),
            
            p(align = "center", a(href = "https://rochmanlab.com/people/", 'Hannah De Frond'),", University of Toronto", 
              tags$a(href="https://twitter.com/HanDefrond", icon("twitter"))),
            
            p(align = "center", "Vera de Ruitjer, Wageningen University"),
            
            p(align = "center", "Dr. Samreen Siddiqui, Oregon State University"),
            
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
                                         strong("Warning:"),"Only 'Particle Only' in vivo ingestion data are included in the study screening dataset.", 
                                         br(),
                                         "'Red criteria' do not represent full scoring criteria. The full set of scoring criteria from Gouin et al. (In Review) may be downloaded via the Search tab or visualized via the Study Screening tab.",
                                         br(),
                                         br(),
                                  ),
                                
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
                                                  choices = c("Particles/mL", "µg/mL", "µm3/mL", "µm2/mL", "µm2/µg/mL"),
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
                              
                              "Only 'Particle Only' in vivo ingestion data are included in the study screening dataset."          
                              
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
                                   
                                  column(width = 12,
                                         strong("Warning:"),"Only 'Particle Only' in vivo ingestion data are included in the study screening dataset.", 
                                         br(),
                                         "'Red criteria' do not represent full scoring criteria. The full set of scoring criteria from Gouin et al. (In Review) may be downloaded via the Search tab or visualized via the Study Screening tab.",
                                         br(),
                                         br(),
                                  ),
                                
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
            p(align = "center",a(href = "https://sccwrp-my.sharepoint.com/:b:/g/personal/leahth_sccwrp_org/EeyE7n7JZdJPi_EYUD_D-dsBxNv5qlBtzwihmr9SbxH_Og?e=88hiV8", 'Data Category Descriptions')),
            br(),
            p(align = "center",a(href = "https://sccwrp-my.sharepoint.com/:b:/g/personal/leahth_sccwrp_org/EQ2GRL8-CB9NnYjqkB7_avABMy-gxRtMSsxD19VUe-0Rsg?e=nESD3x", 'Study Screening Rubric')),
            br(),
            p(align = "center",a(href = "https://sccwrp-my.sharepoint.com/:b:/g/personal/leahth_sccwrp_org/EXz5dUQtgKVMuB79N6YcwasBhKYwO1uhRAQRqumnRqSQuQ?e=OhOO6L", 'Human Health Study List')),
        )           
), #close tab

#### Data Submission UI ####

tabItem(tabName = "Submission", 
        
        box(title = "Data Submission", width = 6, status = "primary",
            p("To submit new data to ToMEx, download the data submission template using the link below. Complete the submission template
              using the embedded descriptions and the ", 
              a(href ="https://sccwrp-my.sharepoint.com/:b:/g/personal/leahth_sccwrp_org/EeyE7n7JZdJPi_EYUD_D-dsBxNv5qlBtzwihmr9SbxH_Og?e=Crfu6Z",
                'Data Category Descriptions', .noOWs = "outside"),
              ". Once the data submission template is completed, upload the completed template using the button below."),
            br(),
            p("For questions regarding data submission or to check to see if data from a specific study has already been uploaded to ToMEx, please email tomex@sccwrp.org"),
            br(),
            p(align = "center", downloadButton(href = "https://sccwrp-my.sharepoint.com/:x:/g/personal/leahth_sccwrp_org/EZ0Gvnn4BkdHsQ2VKVEQQnkBFToXfPW9r4qNX-qcZ5z_LA?e=eISa57&download=1", label = "Download Data Submission Template", icon("download"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
            br(),
            p(align = "center", actionButton(inputId = "submit", onclick = "window.open('https://sccwrp-my.sharepoint.com/:f:/g/personal/leahth_sccwrp_org/EhnzSiN8GqZFjnGpTbNJgskBGaWp0sVKtnB9nrqszAYoQA')", label = "Upload Completed Data Template", icon("file-upload"), style="color: #fff; background-color:  #117a65; border-color:  #0e6655"))),
        
), #close tab

#### Contact UI ####

tabItem(tabName = "Contact", 
        
        box(title = "Contact", width = 6, status = "primary",
            p("For scientific and technical questions, please email tomex@sccwrp.org."),
        ),
        
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
      columnDefs = list(list(width = '100px', targets = "_all"))))
    
  
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
    
    if(Rep_Con_rad == "reported" & dose_check == "µm2/mL"){
      human_setup <- human_setup %>%
        filter(dose.particles.L.master.reported.converted == "reported") %>% 
        mutate(dose_new = dose.um2.mL.master)}
    
    if(Rep_Con_rad == "converted" & dose_check == "µm2/mL"){
      human_setup <- human_setup %>%
        filter(dose.particles.L.master.reported.converted == "converted") %>% 
        mutate(dose_new = dose.um2.mL.master)} 
    
    if(Rep_Con_rad == "all" & dose_check == "µm2/mL"){
      human_setup <- human_setup %>%
        mutate(dose_new = dose.um2.mL.master)}
    
    if(Rep_Con_rad == "reported" & dose_check == "µm2/µg/mL"){
      human_setup <- human_setup %>%
        filter(dose.particles.L.master.reported.converted == "reported") %>% 
        mutate(dose_new = dose.um2.ug.mL.master)}
    
    if(Rep_Con_rad == "converted" & dose_check == "µm2/µg/mL"){
      human_setup <- human_setup %>%
        filter(dose.particles.L.master.reported.converted == "converted") %>% 
        mutate(dose_new = dose.um2.ug.mL.master)} 
    
    if(Rep_Con_rad == "all" & dose_check == "µm2/µg/mL"){
      human_setup <- human_setup %>%
        mutate(dose_new = dose.um2.ug.mL.master)}
    
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
