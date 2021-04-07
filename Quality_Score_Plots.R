#Quality Score Plots for meeting on 4/8/21
#Created by: Leah Thornton Hampton
#Date: 4/6/21

#Load Packages
library(tidyverse) #General everything
library(calecopal) #Color palette

#Import Data

human <- read_csv("Humans_Clean_Final.csv", guess_max = 10000)

#Data Tidying

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
  #Level 3 Data Tidying
  mutate(lvl3_h_f = factor(case_when(lvl3 == "3hydroxyhexadecanoylcarnitine.con" ~ "3 Hydroxyhexadecanoylcarnitine Concentration",
                                     lvl3 == "3hydroxyhexadecenoylcarnitine.con" ~ "3 Hydroxyhexadecenoylcarnitine Concentration",
                                     lvl3 == "3hydroxyoctadecenoylcarnitine.con" ~ "3 Hydroxyoctadecenoylcarnitine Concentration",
                                     lvl3 == "3hydroxytetradecanoylcarnitine.con" ~ "3 Hydroxytetradecanoylcarnitine Concentration",
                                     lvl3 == "a.proteobacteria.genomicdna" ~ "a Proteobacteria Genomic DNA",
                                     lvl3 == "aat.proteinexpression" ~ "aat protein expression",
                                     lvl3 == "abcb11.mrnaexpression" ~ "abcb11 mRNA expression",
                                     lvl3 == "acc.mrnaexpression" ~ "acc mRNA expression",
                                     lvl3 == "acetylcarnitine.con" ~ "Acetylcarnitine Concentration",
                                     lvl3 == "ache.activity" ~ "AchE Activity",
                                     lvl3 == "acl.mrnaexpression" ~ "acl mRNA expression",
                                     lvl3 == "acox.mrnaexpression" ~ "acox mRNA expression",
                                     lvl3 == "actinobacteria.genomicdna" ~ "Actinobacteria Genomic DNA",
                                     lvl3 == "adipylcarnite.con" ~ "Adipylcarnite Concentration",
                                     lvl3 == "akkermansia.geonmicdna" ~ "Akkermansia Genomic DNA",
                                     lvl3 == "alanine.con" ~ "Alanine Concentration",
                                     lvl3 == "alternation" ~ "Alternation",
                                     lvl3 == "annexinv" ~ "Annexin V",
                                     lvl3 == "annexinv.binding" ~ "Annexin V Binding",
                                     lvl3 == "ano1.mrnaexpression" ~ "ano1 mRNA expression",
                                     lvl3 == "ap1.proteinexpression" ~ "ap1 protein expression",
                                     lvl3 == "apoptosis" ~ "Apoptosis",
                                     lvl3 == "arginine.con" ~ "Arginine Concentration",
                                     lvl3 == "atp.con" ~ "ATP Concentration",
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
                                     lvl3 == "caspase.activation" ~ "Caspase Activation",
                                     lvl3 == "caspase3.activity" ~ "Caspase 3 Activity",
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
                                     lvl3 == "claudin1.mrnaexpression" ~ "claudin1 mRNA expression",
                                     lvl3 == "clostridiales.genomicdna" ~ "Clostridiales Genomic DNA",
                                     lvl3 == "cmyc.mrnaexpression" ~ "cmyc mRNA expression",
                                     lvl3 == "coar.mrnaexpression" ~ "coar mRNA expression",
                                     lvl3 == "coas.mrnaexpression" ~ "coas mRNA expression",
                                     lvl3 == "complement.activation.bb" ~ "Complement Activation Bb",
                                     lvl3 == "complement.activation.c3a" ~ "Complement Activation C3a",
                                     lvl3 == "complement.activation.c4d" ~ "Complement Activation C4d",
                                     lvl3 == "complement.activation.sc5b9" ~ "Complement Activation sc5b9",
                                     lvl3 == "complement.activiation.c5b9" ~ "Complement Activation c5b9",
                                     lvl3 == "complement.activiation.ic3b" ~ "Complement Activation ic3b",
                                     lvl3 == "cpt1a.mrnaexpression" ~ "cpt1a mRNA expression",
                                     lvl3 == "creatine.con" ~ "Creatine Concentration",
                                     lvl3 == "cs.mrnaexpression" ~ "cs mRNA expression",
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
                                     lvl3 == "il2.release" ~ "IL2 Release",
                                     lvl3 == "il5.release" ~ "IL5 Release",
                                     lvl3 == "il6.proteinexpression" ~ "IL6 protein expression",
                                     lvl3 == "il6.mrnaexpression" ~ "IL6 mRNA expression",
                                     lvl3 == "il6.release" ~ "IL6 Release",
                                     lvl3 == "il8.mrnaexpression" ~ "IL8 mRNA expression",
                                     lvl3 == "il8.proteinexpression" ~ "IL8 protein expression",
                                     lvl3 == "il8.release" ~ "IL8 Release",
                                     lvl3 == "il9.release" ~ "IL9 Release",
                                     lvl3 == "inos.mrnaexpression" ~ "iNOS mRNA expression",
                                     lvl3 == "intracellular.ca.level" ~ "Intracellular Calcium Levels",
                                     lvl3 == "invariant.natural.killer.cells" ~ "Invariant Natural Killer Cells",
                                     lvl3 == "irf5.proteinexpression" ~ "irf5 protein expression",
                                     lvl3 == "isovalerylcarnitine.con" ~ "Isovalerylcarnitine Concentration",
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
                                     lvl3 == "linoleylcarnitine.con" ~ "Linoleylcarnitine Concentration",
                                     lvl3 == "lipid.droplets" ~ "Lipid Droplets",
                                     lvl3 == "liver.histo" ~ "Liver Histology",
                                     lvl3 == "liver.index" ~ "Liver Index",
                                     lvl3 == "lowdensitylipoprotein.con" ~ "Low Density Lipoprotein Concentration",
                                     lvl3 == "lung.histo" ~ "Lung Histology",
                                     lvl3 == "lymphocyte.count" ~ "Lymphocyte Count",
                                     lvl3 == "lysosomal.integrity" ~ "Lysosomal Integrity",
                                     lvl3 == "lysosomal.protonation" ~ "Lyosomal Protonation",
                                     lvl3 == "macrophage.cell.count" ~ "Macrophage Count",
                                     lvl3 == "macrophage.polarization" ~ "Macrophage Polarization",
                                     lvl3 == "malonylcarnitine.con" ~ "Malonylcarnitine Concentration",
                                     lvl3 == "mcad.mrnaexpression" ~ "mcad mRNA expression",
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
                                     lvl3 == "neutrophil.count" ~ "Neutrophil Count",
                                     lvl3 == "nfkb.mrnaexpression" ~ "nfkB mRNA expression",
                                     lvl3 == "nfkb1.mrnaexpression" ~ "nfkb1 mRNA expression",
                                     lvl3 == "nfkbp75.proteinexpression" ~ "nfkbp75 protein expression",
                                     lvl3 == "nhe3.mrnaexpression" ~ "nhe3 mRNA expression",
                                     lvl3 == "nkcc1.mrnaexpression" ~ "nkcc1 mRNA expression",
                                     lvl3 == "nkcc1.proteinexpression" ~ "nkcc1 protein expression",
                                     lvl3 == "nonestrifiedfattyacid.con" ~ "Nonestrified Fatty Acid Concentration",
                                     lvl3 == "nos2.proteinexpression" ~ "nos2 protein expression",
                                     lvl3 == "nrf2.mrnaexpression" ~ "nrf2 mRNA expression",
                                     lvl3 == "nrf2.proteinexpression" ~ "nrf2 protein expression",
                                     lvl3 == "ntcp.mrnaexpression" ~ "ntcp mRNA expression",
                                     lvl3 == "nuclear.intensity" ~ "Nuclear Intensity",
                                     lvl3 == "nuclear.size" ~ "Nuclear Size",
                                     lvl3 == "nucleaus.cytoplasm.ratio" ~ "Nucleaus Cytoplasm Ratio",
                                     lvl3 == "number.of.live.births" ~ "Number of Live Births",
                                     lvl3 == "octanoylcarnitine.con" ~ "Octanoylcarnitine Concentration",
                                     lvl3 == "octenoylcarnitine.con" ~ "Octenoylcarnitine Concentration",
                                     lvl3 == "odoribacter.genomicdna" ~ "Odoribacter Genomic DNA",
                                     lvl3 == "oleylcarnitine.con" ~ "Oleylcarnitine Concentration",
                                     lvl3 == "open.arm.entries" ~ "Open Arm Entries",
                                     lvl3 == "open.arm.time" ~ "Open Arm Time",
                                     lvl3 == "opsonization" ~ "Opsonization",
                                     lvl3 == "ornithine.con" ~ "Ornithine Concentration",
                                     lvl3 == "ovary.histo" ~ "Ovary Histology",
                                     lvl3 == "p-ikba.proteinexpression" ~ "p-ikba protein expression",
                                     lvl3 == "p-nfkbp75.proteinexpression" ~ "p-nfkbp75 protein expression",
                                     lvl3 == "p38.mrnaexpression" ~ "p38 mRNA expression",
                                     lvl3 == "p53.mrnaexpression" ~ "p53 mRNA expression",
                                     lvl3 == "p53.proteinexpression" ~ "p53 protein expression",
                                     lvl3 == "p70s6k.proteinexpression" ~ "p70s6k protein expression",
                                     lvl3 == "palmitoylcarnitine.con" ~ "Palmitoylcarnitine Concentration",
                                     lvl3 == "parabacteroides.genomicdna" ~ "Parabacteroides Genomic DNA",
                                     lvl3 == "patescibacteria.genomicdna" ~ "Patescibacteria Genomic DNA",
                                     lvl3 == "pge2.release" ~ "pge2 Release",
                                     lvl3 == "phagocytosis" ~ "Phagocytosis",
                                     lvl3 == "phenylalanine.con" ~ "Phenylalanine Concentration",
                                     lvl3 == "pk.mrnaexpression" ~ "pk mRNA expression",
                                     lvl3 == "platelet.count" ~ "Platelet Count",
                                     lvl3 == "ppara.mrnaexpression" ~ "ppar a mRNA expression",
                                     lvl3 == "pparr.mrnaexpression" ~ "ppar r mRNA expression",
                                     lvl3 == "ppary.mrnaexpression" ~ "ppar y mRNA expression",
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
                                     lvl3 == "seminal.vesicle.histo" ~ "Seminal Vesicle Histology",
                                     lvl3 == "sex.ratio" ~ "Sex Ratio",
                                     lvl3 == "shannon.index" ~ "Shannon Index",
                                     lvl3 == "slc26a3.mrnaexpression" ~ "slc26a3 mRNA expression",
                                     lvl3 == "slc26a6.mrnaexpression" ~ "slc26a6 mRNA expression",
                                     lvl3 == "slc26a6.proteinexpression" ~ "slc26a6 protein expression",
                                     lvl3 == "small.intestine.histo" ~ "Small Intestine Histology",
                                     lvl3 == "sod.activity" ~ "SOD Activity",
                                     lvl3 == "sod2.mrnaexpression" ~ "sod2 mRNA expression",
                                     lvl3 == "sperm.count" ~ "Sperm Count",
                                     lvl3 == "sperm.deformity" ~ "Sperm Deformity",
                                     lvl3 == "spleen.histo" ~ "Spleen Histology",
                                     lvl3 == "srebp1c.mrnaexpression" ~ "srebp1c mRNA expression",
                                     lvl3 == "staphylococcus.genomicdna" ~ "Staphylococcus Genomic DNA",
                                     lvl3 == "stat1.phosphorylation" ~ "stat1 Phosphorylation",
                                     lvl3 == "stat6.phosphorylation" ~ "stat6 Phosphorylation",
                                     lvl3 == "stearoylcarnitine.con" ~ "Stearoylcarnitine Concentration",
                                     lvl3 == "stomach.histo" ~ "Stomach Histology",
                                     lvl3 == "succinylacetone.con" ~ "Succinylacetone Concentration",
                                     lvl3 == "t.cells" ~ "T Cell Count",
                                     lvl3 == "t3.con" ~ "T3 Concentration",
                                     lvl3 == "t3.t4.ratio" ~ "T3/T4 Ratio",
                                     lvl3 == "t4.con" ~ "T4 Concentration",
                                     lvl3 == "tenericutes.genomicdna" ~ "Tenericutes Genomic DNA",
                                     lvl3 == "testis.histo" ~ "Testis Histology",
                                     lvl3 == "testis.somatic.index" ~ "Testis Somatic Index",
                                     lvl3 == "tetradecadienoylcarnitine.con" ~ "Tetradecadienoylcarnitine Concentration",
                                     lvl3 == "tetradecanocarnitine.con" ~ "Tetradecanocarnitine Concentration",
                                     lvl3 == "tetradecenoylcarnitine.con" ~ "Tetradecenoylcarnitine Concentration",
                                     lvl3 == "tf.proteinexpression" ~ "tf protein expression",
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
                                     lvl3 == "total.arm.entries" ~ "Total Arm Entries",
                                     lvl3 == "total.arm.entries.5min" ~ "TOtal Arm Entries (5 min)",
                                     lvl3 == "transepithelial.elec.res.respiratory" ~ "Transepithelial Electric Resistance (Respiratory)",
                                     lvl3 == "transepithelial.elec.res.intestinal" ~ "Transepithelial Electric Resistance (Intestinal)",
                                     lvl3 == "transepithelial.elec.res" ~ "Transepithelial Electric Resistance",
                                     lvl3 == "treg.cell.count" ~ "Treg Count",
                                     lvl3 == "triglyceride.con" ~ "Triglyceride Concentration",
                                     lvl3 == "trpv1.mrnaexpression" ~ "trpv1 mRNA expression",
                                     lvl3 == "tsh.con" ~ "TSH Concentration",
                                     lvl3 == "tyrosine.con" ~ "Tyrosine Concentration",
                                     lvl3 == "unifrac.diversity" ~ "Unifrac Diversity",
                                     lvl3 == "uterus.histo" ~ "Uterus Histology",
                                     lvl3 == "valine.con" ~ "Valine Concentration",
                                     lvl3 == "verrucomicrobia.genomicdna" ~ "Verrucomicrobia Genomic DNA",
                                     lvl3 == "white.blood.cell.count" ~ "White Blood Cell Count",
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



#Count how many studies passed and failed each red criteria category

Red_Criteria <- human_setup %>%
  distinct(article, vivo_h_f, tier_zero_particle_f, tier_zero_design_f, tier_zero_risk_f) %>%
  #particle quality
  group_by(tier_zero_particle_f, vivo_h_f) %>% 
  mutate(quality_particle = n_distinct(article, tier_zero_particle_f)) %>% 
  ungroup() %>% 
  #design quality
  group_by(tier_zero_design_f, vivo_h_f) %>% 
  mutate(quality_design = n_distinct(article, tier_zero_design_f)) %>% 
  ungroup() %>%
  #risk quality
  group_by(tier_zero_risk_f, vivo_h_f) %>% 
  mutate(quality_risk = n_distinct(article, tier_zero_risk_f)) %>% 
  ungroup() 

####Red Criteria####

#Plot particle quality
Particle <- Red_Criteria %>% 
  distinct(tier_zero_particle_f, vivo_h_f, quality_particle) %>% 
  ggplot(aes(x = tier_zero_particle_f, y = quality_particle, fill = tier_zero_particle_f)) +
  geom_bar(stat = "identity") +
  facet_wrap(~vivo_h_f)+
  scale_fill_manual(values = c("#BD382F", "#1CA385")) +
  theme_test()+
  theme(axis.title.x = element_blank(),
        text = element_text(size = 16),
        legend.title = element_blank(),
        legend.position = "none")+
  ylim(0,35)+
  labs(title = "Particle Characteristics", y = "Number of Studies")

plot(Particle)

#Plot design quality
Design <- Red_Criteria %>% 
  distinct(tier_zero_design_f, vivo_h_f, quality_design) %>%   
  ggplot(aes(x = tier_zero_design_f, y = quality_design, fill = tier_zero_design_f)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#BD382F", "#1CA385")) +
  facet_wrap(~vivo_h_f)+
  theme_test()+
  theme(axis.title.x = element_blank(),
        text = element_text(size = 16),
        legend.title = element_blank(),
        legend.position = "none")+
  ylim(0,35)+
  labs(title = "Experimental Design", y = "Number of Studies")

plot(Design)

#Plot risk quality
Risk <- Red_Criteria %>% 
  distinct(tier_zero_risk_f, vivo_h_f, quality_risk) %>%  
  ggplot(aes(x = tier_zero_risk_f, y = quality_risk, fill = tier_zero_risk_f)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#BD382F", "#1CA385")) +
  theme_test()+
  theme(axis.title.x = element_blank(),
        text = element_text(size = 16),
        legend.title = element_blank(),
        legend.position = "none")+
 # ylim(0,35)+
  labs(title = "Risk Assessment", subtitle = "In Vivo Data Only", y = "Number of Studies")

plot(Risk)

#Pass all criteria
All <- Red_Criteria %>% 
  filter(!vivo_h_f == "In Vitro") %>% 
  group_by(article, tier_zero_particle_f, tier_zero_design_f, tier_zero_risk_f) %>% 
  summarise() %>% 
  mutate(all = if_else(tier_zero_particle_f == "Red Criteria Passed" & 
                       tier_zero_design_f == "Red Criteria Passed" & 
                       tier_zero_risk_f == "Red Criteria Passed", "Y", "N")) %>% 
  ungroup() %>%
  group_by(all) %>% 
  mutate(all_count = n_distinct(article, all)) %>%
  distinct(all, all_count) %>% 
  ggplot(aes(x = all, y = all_count, fill = factor(all_count))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c( "#1CA385", "#BD382F")) +
  theme_test()+
  theme(axis.title.x = element_blank(),
        text = element_text(size = 16),
        legend.title = element_blank(),
        legend.position = "none")+
  labs(title = "Pass All Red Criteria", subtitle = "In Vivo Data Only", y = "Number of Studies")


plot(All)

###Quality Histograms###

Particle_Score <- human_setup %>%
  distinct(doi, authors, year, particle.quality) %>% 
  drop_na() %>%
  ggplot(aes(x = reorder(paste(authors, year), particle.quality), y = particle.quality)) +
  geom_bar(stat = "identity", fill = "darkcyan") +
  theme_test() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = .5), axis.title.x = element_blank())+
  geom_hline(aes(yintercept = 14), linetype = "dotted", size = 1, color = 'darkgreen')+
  labs(title = "Particle Characteristics", subtitle = "Maximum Score = 14", caption = "Dotted line displays max score", y = "Score")

plot(Particle_Score)  

Design_Score <- human_setup %>%
  distinct(doi, authors, year, experimental.design.quality) %>% 
  drop_na() %>%
  ggplot(aes(x = reorder(paste(authors, year), experimental.design.quality), y = experimental.design.quality)) +
  geom_bar(stat = "identity", fill = "navy") +
  theme_test() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = .5), axis.title.x = element_blank())+
  geom_hline(aes(yintercept = 32), linetype = "dotted", size = 1, color = 'darkgreen')+
  labs(title = "Experimental Design", subtitle = "Maximum Score = 32", caption = "Dotted line displays max score", y = "Score")

plot(Design_Score)

Risk_Score <- human_setup %>%
  distinct(doi, authors, year, risk.quality) %>% 
  drop_na() %>%
  ggplot(aes(x = reorder(paste(authors, year), risk.quality), y = risk.quality)) +
  geom_bar(stat = "identity", fill = "plum") +
  theme_test() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = .5), axis.title.x = element_blank())+
  geom_hline(aes(yintercept = 12), linetype = "dotted", size = 1, color = 'darkgreen')+
  labs(title = "Risk Assessment", subtitle = "Maximum Score = 12", caption = "Dotted line displays max score", y = "Score")

plot(Risk_Score)

Total_Score <- human_setup %>%
  distinct(doi, authors, year, total.score.quality) %>% 
  drop_na() %>%
  ggplot(aes(x = reorder(paste(authors, year), total.score.quality), y = total.score.quality)) +
  geom_bar(stat = "identity", fill = "darkmagenta") +
  theme_test() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = .5), axis.title.x = element_blank())+
  geom_hline(aes(yintercept = 58), linetype = "dotted", size = 1, color = 'darkgreen')+
  labs(title = "Total Score", subtitle = "Maximum Score = 58", caption = "Dotted line displays max score", y = "Score")

plot(Total_Score)

