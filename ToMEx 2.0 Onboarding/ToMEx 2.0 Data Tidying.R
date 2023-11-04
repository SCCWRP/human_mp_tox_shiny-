#ToMEx 2.0 Data Tidying Script
#Date: 5/30/23
#Created By: Leah Thornton Hampton
#Description: Script to join validated data templates from ToMEx 2.0 exercise, re-structure to match ToMEx 1.0, bind to existing ToMEx 1.0 Database

#Load Packages
library(tidyverse)
library(readr)

#Set working directory
setwd("C:/Users/leahth/Documents/GitHub/human_mp_tox_shiny-/")
source("functions.R") # necessary for surface area, volume calculations

#### Extract Data from Submitted Templates ####

#Set working directory
setwd("C:/Users/leahth/Documents/GitHub/human_mp_tox_shiny-/ToMEx 2.0 Onboarding/Validated_Data/")

#Make a list of all files in folder - templates need to be saved as csv files first
file.list <- list.files(pattern='*.csv')

df.list <- lapply(file.list,function(x) {
  
  #Read data, skipping first line
  sheets <- read_csv(x, 
                     skip = 1,
                     #Specify column types
                     col_types = cols(
                       #General Information
                       DOI = col_character(),
                       Authors = col_character(),
                       Year = col_double(),
                       #Data Category 1: Test Organism
                       Species = col_character(),
                       `Life Stage` = col_character(),
                       `In vitro In vivo` = col_character(),
                       Sex = col_character(),
                       #Data Category 2: Experimental Parameters
                       `Experiment Type` = col_character(),
                       `Exposure Route` = col_character(),
                       `Particle Mix?` = col_character(),
                       `Negative Control` = col_character(),
                       `Reference Particle` = col_character(),
                       `Exposure Media` = col_character(),
                       Solvent = col_character(),
                       Detergent = col_character(),
                       `Media Salinity (ppt)` = col_character(),
                       `Media pH` = col_character(),
                       `Media Temp (Mean)` = col_character(),
                       `Media Temp Min` = col_character(),
                       `Media Temp Max` = col_character(),
                       `Exposure Duration (Days)` = col_double(),
                       `Recovery (Days)` = col_double(),
                       Treatments = col_double(),
                       Replicates = col_character(),
                       `Dosing Frequency` = col_double(),
                       `Sample Size` = col_character(),
                       `Nominal Dose - Mass` = col_double(),
                       `Nominal Dose - Mass Units` = col_character(),
                       `Nominal Dose - Particles` = col_double(),
                       `Nominal Dose - Particles Units` = col_character(),
                       `Nominal Dose Alternative Category` = col_character(),
                       `Nominal Dose - Alternative Type` = col_double(),
                       `Nominal Dose - Alternative Type Units` = col_character(),
                       `Nominal Chemicals Added` = col_character(),
                       `Nominal Added Chemical Dose` = col_double(),
                       `Nominal Added Chemical Dose Units` = col_character(),
                       `Measured Dose - Mass` = col_double(),
                       `Measured Dose - Mass Units` = col_character(),
                       `Measured Dose - Particles` = col_double(),
                       `Measured Dose - Particles Units` = col_character(),
                       `Measured Dose Alternative Category` = col_character(),
                       `Measured Dose Alternative` = col_double(),
                       `Measured Dose Alternative Units` = col_character(),
                       `Measured Chemicals Added` = col_character(),
                       `Measured Chemical Dose` = col_double(),
                       `Measured Chemical Dose Units` = col_character(),
                       #Data Category 3: Biological Effects
                       Effect = col_character(),
                       `Effect Metric` = col_character(),
                       Direction = col_character(),
                       `Broad Endpoint Category` = col_character(),
                       `Specific Endpoint Category` = col_character(),
                       Endpoint = col_character(),
                       `Level of Biological Organization` = col_character(),
                       `Target Cell or Tissue` = col_character(),
                       #Data Category 4: Particle Characteristics
                       Polymer = col_character(),
                       `Density (g/cm^3)` = col_double(),
                       `Density (Reported/Estimated)` = col_character(),
                       Shape = col_character(),
                       Charge = col_character(),
                       `Zeta Potential (mV)` = col_character(),
                       `Zeta Potential Media` = col_character(),
                       `Functional Group` = col_character(),
                       `Size Length mm Nominal` = col_double(),
                       `Size Length mm Nominal (minimum)` = col_double(),
                       `Size Length mm Nominal (maximum)` = col_double(),
                       `Size Length mm Measured` = col_double(),
                       `Size Length mm Measured (minimum)` = col_double(),
                       `Size Length mm Measured (Maximum)` = col_double(),
                       `Size Width mm Nominal` = col_double(),
                       `Size Width mm Nominal (minimum)` = col_double(),
                       `Size Width mm Nominal (maximum)` = col_double(),
                       `Size Width mm Measured` = col_double(),
                       `Size Width mm Measured (minimum)` = col_double(),
                       `Size Width mm Measured (maximum)` = col_double(),
                       `Size Height mm Nominal` = col_double(),
                       `Size Height mm Nominal (minimum)` = col_double(),
                       `Size Height mm Nominal (maximum)` = col_double(),
                       `Size Height mm Measured` = col_double(),
                       `Size Height mm Measured (minimum)` = col_double(),
                       `Size Height mm Measured (maximum)` = col_double(),
                       `Weathered or Biofouled?` = col_character(),
                       #Data Category 5: Quality Criteria
                       `Size Validated?` = col_character(),
                       `Shape Validated?` = col_character(),
                       `Polymer Validated?` = col_character(),
                       `Particle Source` = col_character(),
                       `Sodium Azide Present?` = col_character(),
                       `Screened for Chemical Contamination?` = col_character(),
                       `Particle Cleaning?` = col_character(),
                       `Solvent Rinse` = col_character(),
                       `Background Contamination Monitored?` = col_character(),
                       `Concentration Validated?` = col_character(),
                       `Particle Behavior` = col_character(),
                       `Uptake Validated?` = col_character(),
                       `Uptake Validation Method` = col_character(),
                       `Tissue Distribution` = col_character(),
                       `Organisms Fed?` = col_character(),
                       #Screening Criteria
                       `Particle 1` = col_double(),
                       `Particle 2` = col_double(),
                       `Particle 3` = col_double(),
                       `Particle 4` = col_double(),
                       `Particle 5` = col_double(),
                       `Particle 6` = col_double(),
                       `Particle 7` = col_double(),
                       `Design 1` = col_double(),
                       `Design 2` = col_double(),
                       `Design 3` = col_double(),
                       `Design 4` = col_double(),
                       `Design 5` = col_double(),
                       `Design 6` = col_double(),
                       `Design 7` = col_double(),
                       `Design 8` = col_double(),
                       `Design 9` = col_double(),
                       `Design 10` = col_double(),
                       `Design 11` = col_double(),
                       `Design 12` = col_double(),
                       `Design 13` = col_double(),
                       `Risk 1` = col_double(),
                       `Risk 2` = col_double(),
                       `Risk 3` = col_double(),
                       `Risk 4` = col_double(),
                       `Risk 5` = col_double(),
                       `Risk 6` = col_double(),
                       `Particle Total` = col_double(),
                       `Design Total` = col_double(),
                       `Risk Total` = col_double(),
                       `Overall Total` = col_double())) %>%
    #Drop any blank rows using empty DOI cell
    filter(!is.na(DOI))  
  
})

#Create one data frame from all templates
tomex2.0 <- bind_rows(df.list)

#change all column names to lowercase
names(tomex2.0) <- tolower(names(tomex2.0))

#remove unwanted character strings from DOI column
tomex2.0$doi <- gsub('https://dx.doi.org/','',tomex2.0$doi)
tomex2.0$doi <- gsub('https://doi.org/','',tomex2.0$doi)
tomex2.0$doi <- gsub('doi.org/','',tomex2.0$doi)
tomex2.0$doi <- gsub('https://','',tomex2.0$doi)

#Match Data Structure to ToMEx 1.0

#Set working directory
setwd("C:/Users/leahth/Documents/GitHub/human_mp_tox_shiny-/")

#Read in ToMEx 1.0 Tidy Data sets
human_setup <- readRDS("human_setup.RDS")

#### HUMAN SETUP ####

tomex2.0_human_setup <- tomex2.0 %>%
  replace_na(list(shape = "Not Reported", polymer = "Not Reported", `exposure route` = "Not Applicable", `life stage` = "Not Reported")) %>% 
  #Add source column
  add_column(source = "ToMEx 2.0", .before = "doi") %>%   
  #Use only last name of first author
  mutate(authors = word(authors,1,sep = ",")) %>% 
  arrange(authors) %>% 
  transform(article=as.numeric(factor(doi))+55) %>% 
  relocate(article, .after = doi) %>%   
  #Create factors for red criteria screening
  mutate(particle_red_criteria = factor(case_when(
    is.na(particle.1) ~ "Scoring Not Available",
    particle.1 == 0|particle.2 == 0|particle.3 == 0|particle.4 == 0 ~ "Fail",
    particle.1 != 0 & particle.2 != 0 & particle.3 != 0 & particle.4 != 0 ~ "Pass"))) %>%
  mutate(design_red_criteria = factor(case_when(
    is.na(design.1) ~ "Scoring Not Available",
    design.3 == 0|design.4 == 0|design.6 == 0|design.7 == 0|design.9 == 0|design.10 == 0|design.11 == 0 ~ "Fail",
    design.3 != 0 & design.4 != 0 & design.6 != 0 & design.7 != 0 & design.9 != 0 & design.10 != 0 & (design.11 != 0|is.na(design.11)) ~ "Pass"))) %>% 
  mutate(risk_red_criteria = factor(case_when(
    is.na(risk.1) ~ "Scoring Not Available",
    risk.2 == 0|risk.3 == 0|risk.5 == 0 ~ "Fail",
    risk.2 != 0 & risk.3 != 0 & risk.5 != 0 ~ "Pass"))) %>%  
  #Move screening scores up
  relocate(98:130, .after = article) %>% 
  #Factor species
  mutate(species = factor(species)) %>% 
  rename(species_h_f = species) %>%
  #Factor in vitro in vivo
  mutate(in.vitro.in.vivo = factor(in.vitro.in.vivo)) %>% 
  rename(vivo_h_f = in.vitro.in.vivo) %>%
  #Factor life stage
  mutate(life.stage = factor(life.stage)) %>% 
  rename(life_h_f = life.stage) %>%
  #Factor experiment type
  mutate(experiment.type = factor(experiment.type)) %>% 
  rename(exp_type_f = experiment.type) %>%
  #Factor exposure route
  mutate(exposure.route = factor(exposure.route)) %>% 
  rename(exposure_route_h_f = exposure.route) %>% 
  #Rename particle mix, reference particle, salinity, temp, exposure duration columns to match
  rename(treatment = treatments,
         mix = `particle.mix.`,
         reference.material = `reference.particle`,
         media.sal = `media.salinity..ppt.`,
         media.temp = `media.temp..mean.`,
         exposure.duration.d = `exposure.duration..days.`) %>%
  #Rename recovery column 
  rename(`Recovery (Days)` = `recovery..days.`) %>% 
  relocate(`Recovery (Days)`, .after = exposure.duration.d) %>% 
  #Dosing restructuring #ONBOARDING CHECK - ADD UNITS AS NEEDED TO CASE_WHEN STATEMENTS#
  #Count - Nominal
  mutate(dose.particles.mL.nominal = case_when(
    nominal.dose...particles.units == "p/mL" ~ nominal.dose...particles,
    nominal.dose...particles.units == "particles/ml" ~ nominal.dose...particles,
    nominal.dose...particles.units == "particles/mL" ~ nominal.dose...particles,
    nominal.dose...particles.units == "particles/m3" ~ nominal.dose...particles/1000000,
    nominal.dose...particles.units == "particles/L" ~ nominal.dose...particles/1000,
    nominal.dose...particles.units == "particles/l" ~ nominal.dose...particles/1000,
    nominal.dose...particles.units == "L" ~ nominal.dose...particles/1000
  )) %>% 
  relocate(dose.particles.mL.nominal, .after = sample.size) %>% 
  #Count - Measured
  mutate(dose.particles.mL.measured = case_when(
    measured.dose...particles.units == "p/mL" ~ measured.dose...particles,
    measured.dose...particles.units == "particles/ml" ~ measured.dose...particles,
    measured.dose...particles.units == "particles/mL" ~ measured.dose...particles,
    measured.dose...particles.units == "particles/m3" ~ measured.dose...particles/1000000,
    measured.dose...particles.units == "particles/L" ~ measured.dose...particles/1000,
    measured.dose...particles.units == "particles/l" ~ measured.dose...particles/1000,
    measured.dose...particles.units == "L" ~ measured.dose...particles/1000
  )) %>% 
  relocate(dose.particles.mL.measured, .after = dose.particles.mL.nominal) %>% 
  #Mass - Nominal
  mutate(dose.mg.L.nominal = case_when(
    nominal.dose...mass.units == "g/L" ~ nominal.dose...mass*1000,
    nominal.dose...mass.units == "Kg/L" ~ nominal.dose...mass*1000000,
    nominal.dose...mass.units == "mg/L" ~ nominal.dose...mass,
    nominal.dose...mass.units == "mg/l" ~ nominal.dose...mass,
    nominal.dose...mass.units == "ug/mL" ~ nominal.dose...mass,
    nominal.dose...mass.units == "µg/mL" ~ nominal.dose...mass,
    nominal.dose...mass.units == "ug/ml" ~ nominal.dose...mass,
    nominal.dose...mass.units == "µg/ml" ~ nominal.dose...mass,
    nominal.dose...mass.units == "g/mL" ~ nominal.dose...mass*1000000,
    nominal.dose...mass.units == "mg/mL" ~ nominal.dose...mass*1000,
    nominal.dose...mass.units == "ug/L" ~ nominal.dose...mass/1000,
    nominal.dose...mass.units == "µg/L" ~ nominal.dose...mass/1000,
    nominal.dose...mass.units == "ug/l" ~ nominal.dose...mass/1000,
    nominal.dose...mass.units == "µg/l" ~ nominal.dose...mass/1000,
    nominal.dose...mass.units == "ng/L" ~ nominal.dose...mass/1000000,
  )) %>% 
  relocate(dose.mg.L.nominal, .after = dose.particles.mL.nominal) %>% 
  #Mass - Measured
  mutate(dose.mg.L.measured = case_when(
    measured.dose...mass.units == "g/L" ~ measured.dose...mass*1000,
    measured.dose...mass.units == "Kg/L" ~ measured.dose...mass*1000000,
    measured.dose...mass.units == "mg/L" ~ measured.dose...mass,
    measured.dose...mass.units == "mg/l" ~ measured.dose...mass,
    measured.dose...mass.units == "ug/mL" ~ measured.dose...mass,
    measured.dose...mass.units == "µg/mL" ~ measured.dose...mass,
    measured.dose...mass.units == "ug/ml" ~ measured.dose...mass,
    measured.dose...mass.units == "µg/ml" ~ measured.dose...mass,
    measured.dose...mass.units == "g/mL" ~ measured.dose...mass*1000000,
    measured.dose...mass.units == "mg/mL" ~ measured.dose...mass*1000,
    measured.dose...mass.units == "ug/L" ~ measured.dose...mass/1000,
    measured.dose...mass.units == "µg/L" ~ measured.dose...mass/1000,
    measured.dose...mass.units == "ug/l" ~ measured.dose...mass/1000,
    measured.dose...mass.units == "µg/l" ~ measured.dose...mass/1000,
    measured.dose...mass.units == "ng/L" ~ measured.dose...mass/1000000,
  )) %>% 
  relocate(dose.mg.L.measured, .after = dose.particles.mL.measured) %>% 
  #Create master columns for dose and count - measured doses preferred
  mutate(dose.particles.mL.master = if_else(!is.na(dose.particles.mL.measured), dose.particles.mL.measured, dose.particles.mL.nominal)) %>% 
  relocate(dose.particles.mL.master, .after = dose.mg.L.measured) %>% 
  mutate(dose.mg.L.master = if_else(!is.na(dose.mg.L.measured), dose.mg.L.measured, dose.mg.L.nominal)) %>% 
  relocate(dose.mg.L.master, .after = dose.particles.mL.master)  %>% 
  #Mark that doses were reported (converted doses are to be added later in script)
  mutate(dose.particles.mL.master.converted.reported = if_else(!is.na(dose.particles.mL.master), "reported", NA_character_)) %>% 
  relocate(dose.particles.mL.master.converted.reported, .after = dose.particles.mL.master) %>% 
  mutate(dose.mg.mL.master.reported.converted = if_else(!is.na(dose.mg.L.master), "reported", NA_character_)) %>% 
  relocate(dose.mg.mL.master.reported.converted, .after = dose.mg.L.master) %>%   

  #Factor effect
  mutate(effect = factor(case_when(
    effect == "Yes" ~ "Yes",
    effect == "YES" ~ "Yes",
    effect == "yes" ~ "Yes",
    effect == "No" ~ "No",
    effect == "NO" ~ "No",
    effect == "no" ~ "No"))) %>% 
  rename(effect_h_f = effect) %>%
  #Remove effect metrics when there are less than 3 treatments
  mutate(effect.metric = as.character(effect.metric)) %>% 
  mutate(effect.metric = factor(ifelse(treatment < 3, NA_character_, effect.metric))) %>% 
  #Factor and rename endpoint columns
  mutate(broad.endpoint.category = factor(broad.endpoint.category)) %>% 
  rename(lvl1_h_f = broad.endpoint.category) %>%
  mutate(specific.endpoint.category = factor(specific.endpoint.category)) %>% 
  rename(lvl2_h_f = specific.endpoint.category) %>% 
  mutate(endpoint = factor(endpoint)) %>% 
  rename(lvl3_h_f = endpoint) %>% 
  #Factor and rename biological level of organization
  mutate(level.of.biological.organization = factor(level.of.biological.organization)) %>% 
  rename(bio_h_f = level.of.biological.organization) %>%  
  #Rename target cell or tissue column
  rename(target.organelle.cell.tissue = target.cell.or.tissue) %>% 
  #Rename density column
  rename(density.g.cm3 = density..g.cm.3.) %>% 
  rename(density.reported.estimated = density..reported.estimated.) %>% 
  #Factor shape
  mutate(shape = factor(shape)) %>% 
  rename(shape_h_f = shape) %>%  
  #Factor shape
  mutate(polymer = factor(polymer)) %>% 
  rename(poly_h_f = polymer) %>% 
  #Rename zeta potential column
  rename(zetapotential = zeta.potential..mv.,
         zeta.potential.media = zeta.potential.media) %>% 
  #Rename size columns
  rename(
    size.length.min.mm.nominal = size.length.mm.nominal..minimum.,
    size.length.max.mm.nominal = size.length.mm.nominal..maximum.,
    size.length.min.mm.measured = size.length.mm.measured..minimum.,
    size.length.max.mm.measured = size.length.mm.measured..maximum.,
    
    size.width.min.mm.nominal = size.width.mm.nominal..minimum.,
    size.width.max.mm.nominal = size.width.mm.nominal..maximum.,
    size.width.min.mm.measured = size.width.mm.measured..minimum.,
    size.width.max.mm.measured = size.width.mm.measured..maximum.,
    
    size.height.min.mm.nominal = size.height.mm.nominal..minimum.,
    size.height.max.mm.nominal = size.height.mm.nominal..maximum.,
    size.height.min.mm.measured = size.height.mm.measured..minimum.,
    size.height.max.mm.measured = size.height.mm.measured..maximum.) %>%
  #Select size lengths to be used for conversions
  mutate(size.length.um.used.for.conversions = case_when(
    !is.na(size.length.mm.measured) ~ size.length.mm.measured*1000,
    !is.na(size.length.min.mm.measured) ~ ((size.length.max.mm.measured + size.length.min.mm.measured)/2)*1000,
    !is.na(size.length.mm.nominal) ~ size.length.mm.nominal*1000,
    !is.na(size.length.min.mm.nominal) ~ ((size.length.max.mm.nominal + size.length.min.mm.nominal)/2)*1000)) %>% 
  relocate(size.length.um.used.for.conversions, .after = zeta.potential.media) %>% 
  #Select size widths to be used for conversions
  mutate(size.width.um.used.for.conversions = case_when(
    !is.na(size.width.mm.measured) ~ size.width.mm.measured*1000,
    !is.na(size.width.min.mm.measured) ~ ((size.width.max.mm.measured + size.width.min.mm.measured)/2)*1000,
    !is.na(size.width.mm.nominal) ~ size.width.mm.nominal*1000,
    !is.na(size.width.min.mm.nominal) ~ ((size.width.max.mm.nominal + size.width.min.mm.nominal)/2)*1000)) %>% 
  relocate(size.width.um.used.for.conversions, .after = size.length.um.used.for.conversions) %>% 
  #Add size category column 
  mutate(size_h_f = factor(case_when(
    size.length.um.used.for.conversions < 0.1 ~ "1nm < 100nm",
    size.length.um.used.for.conversions >= 0.1 & size.length.um.used.for.conversions < 1 ~ "100nm < 1µm",
    size.length.um.used.for.conversions >= 1 & size.length.um.used.for.conversions < 100 ~ "1µm < 100µm",
    size.length.um.used.for.conversions >= 100 & size.length.um.used.for.conversions < 1000 ~ "100µm < 1mm",
    size.length.um.used.for.conversions >= 1000 & size.length.um.used.for.conversions < 5000 ~ "1mm < 5mm",
    is.na(size.length.um.used.for.conversions) ~ "Not Reported"),  
    levels = c("1nm < 100nm", "100nm < 1µm", "1µm < 100µm", "100µm < 1mm", "Not Reported"))) %>% # creates new column with nicer names and order by size levels.
  relocate(size_h_f, .after = size.width.um.used.for.conversions) %>% 
  #Calculate particle surface area
  mutate(particle.surface.area.um2 = case_when(shape_h_f == "Sphere" ~ 4*pi*((size.length.um.used.for.conversions/2)^2),
                                               shape_h_f == "Fiber" & is.na(size.width.um.used.for.conversions) ~ SAfnx_fiber(width = 15, length = size.length.um.used.for.conversions), #assum 15 um width (kooi et al 2021)
                                               shape_h_f == "Fiber" & !is.na(size.width.um.used.for.conversions) ~ SAfnx_fiber(width = size.width.um.used.for.conversions, length = size.length.um.used.for.conversions), #if width is known
                                               shape_h_f == "Fragment" ~ SAfnx(a = size.length.um.used.for.conversions,
                                                                             b = 0.77 * size.length.um.used.for.conversions,
                                                                             c = 0.77 * 0.67 * size.length.um.used.for.conversions))) %>%
  relocate(particle.surface.area.um2, .after = size_h_f) %>% 
  #Calculate particle volume
  mutate(particle.volume.um3 = case_when(shape_h_f == "Sphere" ~ (4/3)*pi*((size.length.um.used.for.conversions/2)^3),
                                         shape_h_f == "Fiber" & is.na(size.width.um.used.for.conversions) ~ volumefnx_fiber(width = 15, length = size.length.um.used.for.conversions), #assume 15 um as width (kooi et al 2021)
                                         shape_h_f == "Fiber" & !is.na(size.width.um.used.for.conversions) ~ volumefnx_fiber(width = size.width.um.used.for.conversions, length = size.length.um.used.for.conversions), #if width reported
                                         shape_h_f == "Fragment" ~ volumefnx(R = 0.77, L = size.length.um.used.for.conversions))) %>% 
  relocate(particle.volume.um3, .after = particle.surface.area.um2) %>% 
  #Calculate particle mass
  mutate(mass.per.particle.mg = (particle.volume.um3*density.g.cm3)*0.000000001) %>% 
  relocate(mass.per.particle.mg, .after = particle.volume.um3) %>%
  ####
  #calculate dose metrics accordingly
  mutate(dose.surface.area.um2.mL.master = particle.surface.area.um2 * dose.particles.mL.master) %>% 
  mutate(particle.surface.area.um2.mg = particle.surface.area.um2 / mass.per.particle.mg) %>% 
  
  # create label for polydispersity
  mutate(polydispersity = case_when(
    is.na(size.length.min.mm.nominal) ~ "monodisperse",
    !is.na(size.length.min.mm.nominal) ~ "polydisperse")) %>% 
  
  ####prioritize measured parameters for conversions ###
  # minima
  mutate(size.length.min.um.used.for.conversions = case_when(
    is.na(size.length.min.mm.measured) ~ size.length.min.mm.nominal * 1000,
    !is.na(size.length.min.mm.measured) ~ size.length.min.mm.measured * 1000)) %>% 
  mutate(size.width.min.um.used.for.conversions = case_when(
    shape_h_f == "Sphere" ~ size.length.min.um.used.for.conversions, #all dims same
    shape_h_f == "Fiber" ~ 0.77 * size.length.min.um.used.for.conversions, #median holds for all particles (Kooi et al 2021)
    shape_h_f == "Not Reported" ~ 0.77 * size.length.min.um.used.for.conversions, # average width to length ratio in the marine environment (kooi et al 2021)
    shape_h_f == "Fragment" ~ 0.77 * size.length.min.um.used.for.conversions)) %>% # average width to length ratio in the marine environment (kooi et al 2021)
  mutate(size.height.min.um.used.for.conversions = case_when(
    shape_h_f == "Sphere" ~ size.length.min.um.used.for.conversions, #all dims same
    shape_h_f == "Not Reported" ~ 0.77 * 0.67 * size.length.min.um.used.for.conversions, # average width to length ratio in the marine environment (kooi et al 2021)
    shape_h_f == "Fiber" ~  0.77 * size.length.min.um.used.for.conversions, #height same as width for fibers
    shape_h_f == "Fragment" ~ 0.77 * 0.67 * size.length.min.um.used.for.conversions)) %>% # average width to length ratio in the marine environment AND average height to width ratio (kooi et al 2021)
  # maxima
  mutate(size.length.max.um.used.for.conversions = case_when(
    is.na(size.length.max.mm.measured) ~ size.length.max.mm.nominal * 1000,
    !is.na(size.length.max.mm.measured) ~ size.length.max.mm.measured * 1000)) %>% 
  mutate(size.width.max.um.used.for.conversions = case_when(
    shape_h_f == "Sphere" ~ size.length.max.um.used.for.conversions, #all dims same
    shape_h_f == "Fiber" ~ 0.77 * size.length.max.um.used.for.conversions, #median holds for all particles (Kooi et al 2021) #there are no fibers
    shape_h_f == "Not Reported" ~ 0.77 * size.length.max.um.used.for.conversions, # average width to length ratio in the marine environment (kooi et al 2021)
    shape_h_f == "Fragment" ~ 0.77 * size.length.max.um.used.for.conversions)) %>% # average width to length ratio in the marine environment (kooi et al 2021)
  mutate(size.height.max.um.used.for.conversions = case_when(
    shape_h_f == "Sphere" ~ size.length.max.um.used.for.conversions, #all dims same
    shape_h_f == "Not Reported" ~ 0.77 * 0.67 * size.length.max.um.used.for.conversions, # average width to length ratio in the marine environment (kooi et al 2021)
    shape_h_f == "Fiber" ~ 0.77 * size.length.max.um.used.for.conversions, #hieght same as width
    shape_h_f == "Fragment" ~ 0.77 * 0.67 * size.length.max.um.used.for.conversions)) %>%  # average width to length ratio in the marine environment AND average height to width ratio (kooi et al 2021)
  
  #calculate minimum and maximum surface area for polydisperse particles
  mutate(particle.surface.area.um2.min = SAfnx(a = size.length.min.um.used.for.conversions,
                                               b = size.width.min.um.used.for.conversions,
                                               c = size.height.min.um.used.for.conversions)) %>%
  mutate(particle.surface.area.um2.max = SAfnx(a = size.length.max.um.used.for.conversions,
                                               b = size.width.max.um.used.for.conversions,
                                               c = size.height.max.um.used.for.conversions)) %>% 
  #calculate minimum and maximum volume for polydisperse particles
  mutate(particle.volume.um3.min = volumefnx_poly(length = size.length.min.um.used.for.conversions,
                                                  width =  size.width.min.um.used.for.conversions)) %>% 
  mutate(particle.volume.um3.max = volumefnx_poly(length = size.length.max.um.used.for.conversions,
                                                  width = size.width.max.um.used.for.conversions)) %>% 
  #calculate minimum and maximum volume for polydisperse particles
  mutate(mass.per.particle.mg.min = massfnx_poly(length = size.length.min.um.used.for.conversions,
                                                 width = size.width.min.um.used.for.conversions,
                                                 p = density.g.cm3)) %>% #equation usess g/cm3
  mutate(mass.per.particle.mg.max = massfnx_poly(length = size.length.max.um.used.for.conversions,
                                                 width = size.width.max.um.used.for.conversions,
                                                 p = density.g.cm3)) %>%   #equation usess g/cm3
  
  #Volume
  mutate(dose.um3.mL.master = particle.volume.um3 * dose.particles.mL.master) %>%  #calculate volume/mL
  
  #Surface Area
  mutate(dose.um2.mL.master = as.numeric(particle.surface.area.um2) * dose.particles.mL.master) %>% 
  
  #Specific Surface Area
  mutate(dose.um2.ug.mL.master = dose.um2.mL.master / (mass.per.particle.mg / 1000)) %>% #correct mg to ug
  
  #Factor particle weathering
  mutate(weathered.or.biofouled. = factor(weathered.or.biofouled.)) %>% 
  #Rename columns to match/look nicer
  rename(dose.ug.mL.master = dose.mg.L.master,
         weathered.biofouled = weathered.or.biofouled.,
         size.valid = size.validated.,
         shape.valid = shape.validated.,
         polymer.valid = polymer.validated.,
         sodium.azide = sodium.azide.present.,
         contaminant.screen = screened.for.chemical.contamination.,
         clean.method = particle.cleaning.,
         sol.rinse = solvent.rinse,
         background.plastics = background.contamination.monitored.,
         con.valid = concentration.validated.,
         uptake.valid = uptake.validated.,
         uptake.valid.method = uptake.validation.method,
         fed = organisms.fed.,
         `Nominal Dose Alternative Category` = `nominal.dose.alternative.category`,
         `Nominal Dose Alternative Type` = `nominal.dose...alternative.type`,
         `Nominal Dose Alternative Type Units` = `nominal.dose...alternative.type.units`,
         `Measured Dose Alternative Category` = `measured.dose.alternative.category`,
         `Measured Dose Alternative Type` = `measured.dose.alternative`,
         `Measured Dose Alternative Type Units` = `measured.dose.alternative.units`) 

#Re-structure alternative dosing columns in human-setup
human_setup <- human_setup %>%
  mutate(uptake.valid.method = case_when(
    uptake.valid == "N" ~ NA_character_,
    !is.na(uptake.valid) ~ uptake.valid)) %>% 
  mutate(uptake.valid = ifelse(
    !is.na(uptake.valid.method),"Yes", "No")) %>% 
  #Nominal Alternative Doses
  mutate(`Nominal Dose Alternative Type` = case_when(
    !is.na(dose.ug.g.food.nominal) ~ dose.ug.g.food.nominal,
    !is.na(dose.uM.nominal) ~ dose.uM.nominal,
    !is.na(dose.ug.cm2.nominal) ~ dose.ug.cm2.nominal,
    !is.na(dose.particles.day.nominal) ~ dose.particles.day.nominal,
    !is.na(dose.particles.kg.bw.nominal) ~ dose.particles.kg.bw.nominal,
    !is.na(dose.mg.day.nominal) ~ dose.mg.day.nominal,
    !is.na(dose.mg.L.air.nominal) ~ dose.mg.L.air.nominal,
    !is.na(dose.mg.kg.day.bw.nominal) ~ dose.mg.kg.day.bw.nominal,
    !is.na(dose.cm2.mL.nominal) ~ dose.cm2.mL.nominal,
  )) %>%  
  mutate(`Nominal Dose Alternative Type Units` = case_when(
    !is.na(dose.ug.g.food.nominal) ~ "ug/g food",
    !is.na(dose.uM.nominal) ~ "uM",
    !is.na(dose.ug.cm2.nominal) ~ "ug/cm^2",
    !is.na(dose.particles.day.nominal) ~ "particles/day",
    !is.na(dose.particles.kg.bw.nominal) ~ "particles/kg body weight",
    !is.na(dose.mg.day.nominal) ~ "mg/day",
    !is.na(dose.mg.L.air.nominal) ~ "mg/L air",
    !is.na(dose.mg.kg.day.bw.nominal) ~ "mg/kg body weight/day",
    !is.na(dose.cm2.mL.nominal) ~ "cm^2/mL",
  )) %>% 
  #Add Other missing columns (most were non existent or NA in ToMEx 1.0)
  add_column(`Measured Dose Alternative Type` = NA_real_,
             `Measured Dose Alternative Type Units` = NA_character_,
             `Nominal Dose Alternative Category` = NA_character_,
             `Measured Dose Alternative Category` = NA_character_,
             effect.metric = NA_character_,
             measured.chemicals.added = NA_character_,
             measured.chemical.dose = NA_real_,
             measured.chemical.dose.units = NA_character_,
             size.width.um.used.for.conversions = NA_real_) %>% 
  mutate(dose.particles.mL.nominal = dose.particles.L.nominal/1000) %>% 
  mutate(dose.particles.mL.measured = dose.particles.L.measured/1000) %>% 
  mutate(dose.mg.L.nominal = dose.mg.mL.nominal*1000) %>% 
  mutate(dose.mg.L.measured = dose.mg.mL.measured*1000) %>% 
  rename(dose.particles.mL.master.converted.reported = dose.particles.L.master.reported.converted) %>% 
  rename(nominal.chemicals.added = chem) %>% 
  mutate(nominal.added.chemical.dose = case_when(
    !is.na(chem.dose.ug.L.nominal) ~ chem.dose.ug.L.nominal,
    !is.na(chem.dose.umol.kg.bw.day) ~ chem.dose.umol.kg.bw.day,
    !is.na(chem.dose.uM) ~ chem.dose.uM)) %>% 
  mutate(nominal.added.chemical.dose.units  = case_when(
    !is.na(chem.dose.ug.L.nominal) ~ "ug/L",
    !is.na(chem.dose.umol.kg.bw.day) ~ "umol/kg body weight/day",
    !is.na(chem.dose.uM) ~ "uM")) %>% 
  mutate(media.ph = as.character(media.ph)) %>% 
  mutate(media.temp = as.character(media.temp)) %>% 
  mutate(media.temp.min = as.character(media.temp.min)) %>% 
  mutate(media.temp.max = as.character(media.temp.max)) %>% 
  mutate(replicates = as.character(replicates)) %>% 
  mutate(sample.size = as.character(sample.size)) %>% 
  mutate(zetapotential = as.character(zetapotential)) %>% 
  mutate(zeta.potential.media = as.character(zeta.potential.media))
  
 
#Get names of relevant columns from tomex 2.0
names <- tomex2.0_human_setup %>%
  select(-c(nominal.dose...mass, nominal.dose...mass.units, nominal.dose...particles, nominal.dose...particles.units, 
            measured.dose...mass, measured.dose...mass.units, 
            measured.dose...particles, measured.dose...particles.units, 
            particle.total, design.total, risk.total, overall.total)) %>% 
  colnames() 

#Select columns from each data frame
tomex2.0_human_setup <- tomex2.0_human_setup %>%
  select(all_of(names))

human_setup <- human_setup %>%
  add_column(`Recovery (Days)` = NA_real_) %>% 
  select((names))

#Join rows
tomex2.0_human_setup_final <- bind_rows(human_setup, tomex2.0_human_setup)

##### QA/QC - FLAGGING STUDIES ####

# tomex2.0_human_setup_final <- tomex2.0_human_setup_final %>%
#   group_by(doi) %>% 
#   mutate(`Issue Flag` = case_when(
#     source == "ToMEx 2.0" & all(is.na(effect.metric)) & treatment >= 3 
#     ~ "Effect metrics missing.")) %>%
#   relocate(`Issue Flag`, .before = doi) %>% 
#   ungroup() %>% 
#   mutate(`Issue Flag` = case_when(
#     !is.na(`Issue Flag`) ~ `Issue Flag`,
#     source == "ToMEx 2.0" & exp_type_f == "Particle Only" & is.na(particle.1) 
#     ~ "Screening scores need to be completed for Particle Only type data.",
#     source == "ToMEx 2.0" & effect_h_f == "Yes" & is.na(direction)
#     ~ "Detected effects missing direction."
#   )) 

#Save RDS file
saveRDS(tomex2.0_human_setup_final, file = "human_setup_tomex2.RDS")

#### HUMAN ENDPOINT ####

tomex2.0_human_endpoint_final <- tomex2.0_human_setup_final %>% 
  group_by(vivo_h_f, lvl1_h_f,lvl2_h_f,lvl3_h_f,bio_h_f) %>% 
  summarise()

#Save RDS file
saveRDS(tomex2.0_human_endpoint_final, file = "human_endpoint_tomex2.RDS")

#### HUMAN SEARCH ####

tomex2.0_human_search_final <- tomex2.0_human_setup_final %>%
  #general
  dplyr::select(source, doi, authors, year, species_h_f, life_h_f, vivo_h_f, sex,
                #experimental parameters
                exp_type_f, exposure_route_h_f, mix, negative.control, reference.material, exposure.media, solvent, detergent,
                media.ph, media.sal, media.temp, media.temp.min, media.temp.max, exposure.duration.d, `Recovery (Days)`,
                treatment, replicates, sample.size, dosing.frequency, 
                #master doses
                dose.particles.mL.master, dose.particles.mL.master.converted.reported, dose.ug.mL.master, dose.mg.mL.master.reported.converted,
                dose.um3.mL.master, dose.um2.mL.master, dose.um2.ug.mL.master, 
                `Nominal Dose Alternative Category`, `Nominal Dose Alternative Type`, `Nominal Dose Alternative Type Units`,
                `Measured Dose Alternative Category`, `Measured Dose Alternative Type`, `Measured Dose Alternative Type Units`,
                #chemical doses
                nominal.chemicals.added, nominal.added.chemical.dose, nominal.added.chemical.dose.units,
                measured.chemicals.added, measured.chemical.dose, measured.chemical.dose.units,
                #biological effects
                effect_h_f, direction, lvl1_h_f, lvl2_h_f, lvl3_h_f, bio_h_f, target.organelle.cell.tissue, 
                #particle characteristics
                poly_h_f, shape_h_f, density.g.cm3, density.reported.estimated, charge, zetapotential, zeta.potential.media, functional.group,
                size.length.um.used.for.conversions, size_h_f, particle.volume.um3,
                mass.per.particle.mg, weathered.biofouled,
                #quality
                size.valid, polymer.valid, shape.valid, particle.source, sodium.azide, contaminant.screen, clean.method, sol.rinse, background.plastics,
                con.valid, particle.behavior, uptake.valid, tissue.distribution, fed,
                #scores
                particle_red_criteria, design_red_criteria, risk_red_criteria, 
                particle.1, particle.2, particle.3, particle.4, particle.5, particle.6, particle.7,
                design.1, design.2, design.3, design.4, design.5, design.6, design.7, design.8, design.9, design.10, design.11, design.12, design.13,
                risk.1, risk.2, risk.3, risk.4, risk.5, risk.6) %>% 
  rename('Source' = source,'DOI' = doi,'Authors' = authors, 'Year' = year, 'Species' = species_h_f, 'Life Stage' = life_h_f, 'In Vitro or In Vivo?' = vivo_h_f, 'Sex' = sex,
         
         'Experiment Type' = exp_type_f, 'Exposure Route' = exposure_route_h_f, 'Particle Mix?' = mix, 'Negative Control' = negative.control, 
         'Reference Particle' = reference.material, 'Exposure Media' = exposure.media, 'Solvent' = solvent, 'Detergent' = detergent, 
         'pH' = media.ph, 'Salinity (ppt)' = media.sal, 'Temperature (Avg)' = media.temp, 'Temperature (Min)' = media.temp.min,
         'Temperature (Max)' = media.temp.max, 'Exposure Duration (days)' = exposure.duration.d, 'Number of Doses' = treatment, 'Replicates' = replicates,
         'Sample Size' = sample.size, 'Dosing Frequency' = dosing.frequency,
         
         "particles/mL (master)" = dose.particles.mL.master, "particles/mL (master), reported or converted" = dose.particles.mL.master.converted.reported,
         "μg/mL (master)" = dose.ug.mL.master, "μg/mL (master), reported or converted" = dose.mg.mL.master.reported.converted,
         "μm^3/mL (master)" = dose.um3.mL.master, "μm^2/mL (master)" = dose.um2.mL.master, "μm/ug/mL (master)" = dose.um2.ug.mL.master,
         
         "Nominal Chemical Added" = nominal.chemicals.added, "Nominal Chemical Added Dose" = nominal.added.chemical.dose, "Nominal Chemical Added Dose Units" = nominal.added.chemical.dose.units,
         "Measured Chemical Added" = measured.chemicals.added, "Measured Chemical Added Dose" = measured.chemical.dose, "Measured Chemical Added Dose Units" = measured.chemical.dose.units, 
         
         "Effect" = effect_h_f, "Direction" = direction, "Broad Endpoint Category" = lvl1_h_f, "Specific Endpoint Category" = lvl2_h_f,
         "Endpoint" = lvl3_h_f, "Level of Biological Organization" = bio_h_f, "Target Organelle, Cell, or Tissue" = target.organelle.cell.tissue,
         
         "Polymer" = poly_h_f, "Shape" = shape_h_f, "Density (g/cm^3)" = density.g.cm3, "Density, reported or estimated" = density.reported.estimated,
         "Charge" = charge, "Zeta Potential (mV)" = zetapotential, "Zeta Potential Media" = zeta.potential.media, "Functional Group" = functional.group,
         "Particle Length (μm)" = size.length.um.used.for.conversions, 
         "Size Category" = size_h_f, "Particle Volume (μm^3)" = particle.volume.um3, "Particle Mass (mg)" = mass.per.particle.mg, "Weathered or Biofouled?" = weathered.biofouled,
         
         "Size Validated?" = size.valid, "Polymer Validated?" = polymer.valid, "Shape Validated" = shape.valid, "Particle Source" = particle.source,
         "Sodium Azide Present?" = sodium.azide, "Screened for Chemical Contamination?" = contaminant.screen, "Particle Cleaning?" = clean.method,
         "Solvent Rinse" = sol.rinse, "Background Contamination Monitored?" = background.plastics,
         "Concentration Validated?"  = con.valid, "Particle Behavior" = particle.behavior, "Uptake Validated?" = uptake.valid,
         "Tissue Distribution" = tissue.distribution, "Organisms Fed?" = fed,
         
         "Particle Red Criteria Pass" = particle_red_criteria, "Design Red Criteria Pass" = design_red_criteria, "Risk Red Criteria Pass" = risk_red_criteria,
         
         "Particle Size*" = particle.1, "Particle Shape*" = particle.2, "Polymer Type*" = particle.3, "Particle Source*" = particle.4, 
         "Surface Chemistry" = particle.5, "Chemical Purity" = particle.6, "Microbial Contamination" = particle.7, 
         
         "Concentration Units" = design.1, "Particle Stability" = design.2, "Test Vehicle*" = design.3, "Administered Dose*" = design.4, "Homogeneity of Exposure" = design.5,
         "Administration Route* (in vivo)/Test System (in vivo)" = design.6, "Test Species* (in vivo)/System Parameters (in vitro)" = design.7, "Feeding/Housing Conditions (in vivo)/Sample Size, Replicates (in vitro)" = design.8, 
         "Sample Size* (in vivo)/Frequency/Duration of Exposure (in vitro)" = design.9, 
         "Frequency,Duration of Exposure (in vivo)/Controls (in vitro)" = design.10, "Controls* (in vivo only)" = design.11, "Replicates (in vivo only)" = design.12, "Internal Dose Confirmation (in vivo only)" = design.13,
         
         "Statistical Analysis" = risk.1, "Endpoints" = risk.2, "Dose-Response*" = risk.3, "Concentration Range" = risk.4, "Effect Thresholds*" = risk.5, 
         "Test Particle Relevance" = risk.6)
  
#Turn all character strings into factors if they aren't already so they are searchable via dropdown
tomex2.0_human_search_final[sapply(tomex2.0_human_search_final, is.character)] <- lapply(tomex2.0_human_search_final[sapply(tomex2.0_human_search_final, is.character)], as.factor)

#Save RDS file
saveRDS(tomex2.0_human_search_final, file = "human_search_tomex2.RDS")

#### HUMAN QUALITY ####

tomex2.0_human_quality_final <- tomex2.0_human_setup_final %>%
  filter(exp_type_f == "Particle Only") %>% 
  filter(vivo_h_f == "In Vivo") %>% 
  filter(particle_red_criteria != "Scoring Not Available") %>% 
  filter(design_red_criteria != "Scoring Not Available") %>% 
  filter(risk_red_criteria != "Scoring Not Available") %>% 
  mutate(Study = paste0(authors, " (", year,")")) %>%
  mutate(Study_plus = as.factor(paste0(authors, " (", year,")", " (",doi,")"))) %>%  
  distinct(source, Study, Study_plus, doi, particle.1, particle.2, particle.3, particle.4, particle.5, particle.6, particle.7,
           design.1, design.2, design.3, design.4, design.5, design.6, design.7, design.8, design.9, design.10, design.11, design.12, 
           design.12, design.13, risk.1, risk.2, risk.3, risk.4, risk.5, risk.6,
           lvl1_h_f, lvl2_h_f, bio_h_f, effect_h_f, life_h_f, poly_h_f, shape_h_f, size_h_f, species_h_f, exposure_route_h_f, 
           particle_red_criteria, design_red_criteria, risk_red_criteria) %>%     
  
  pivot_longer(!c(source, Study, Study_plus, doi, lvl1_h_f, lvl2_h_f, bio_h_f, effect_h_f, life_h_f, poly_h_f, shape_h_f, size_h_f, species_h_f, exposure_route_h_f,
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

#Save RDS file
saveRDS(tomex2.0_human_quality_final, file = "human_quality_tomex2.RDS")


#End of Script

#quick stats
library(tidyverse)

# pubs <- as.data.frame(unique(human_setup_tomex2$doi))
# 
# study_types <- human_setup_tomex2 %>%
#   group_by(doi, exp_type_f) %>% 
#   summarise()
# 
# vivo <- human_setup_tomex2 %>% 
#   filter(vivo_h_f == "In Vivo")
# 
# species <- human_setup_tomex2 %>%
#   group_by(species_h_f) %>% 
#   summarise()
# 
# not_tidy <- tomex2.0_human_setup_final %>% 
#   filter(!is.na(`Issue Flag`)) 
# 
# not_tidy_studies <- not_tidy %>% 
#   group_by(source, doi,authors,`Issue Flag`) %>% 
#   summarise() 
