#Load Packages
OS<-.Platform$OS.type
if(OS=="unix"){
  Packages<-"/Users/stephensong/Documents/GitHub/R-repository/Default_Packages_to_Load.R"
}else if(OS=="windows"){
  Packages<-"C:/Users/steph/Onedrive/Documents/GitHub/R repository/Default_Packages_to_Load.R"
}else{
  print("ERROR: Packages NOT LOADED!!")
}
source(Packages)

#OS settings
OS<-.Platform$OS.type
if(OS=="unix"){
  workdirectory<-"/Users/stephensong/Documents/GitHub/dissertationdata/Publication papers" # MAC file path
}else if(OS=="windows"){
  workdirectory<-"C:/Users/steph/Onedrive/Documents/GitHub/VRecommerce" # windows file path
}else{
  print("ERROR: OS could not be identified")
}

#setwd
setwd(workdirectory)
getwd()
df<-read.csv("./MAIN.csv")

#Measurement Model
MM <- constructs(
  reflective("D_MAL", multi_items("D_MAL", 1:3)),
  reflective("D_INCOMP", multi_items("D_INCOMP", 1:3)),
  reflective("D_DECT", multi_items("D_DECT", 1:3)),
  reflective("MAL", multi_items("MAL", 1:3)),
  reflective("INCOMP", multi_items("INCOMP", 1:3)),
  reflective("DECT", multi_items("DECT", 1:3)),
  composite("SOURCE", multi_items("SOURCE", 1:4), weights = correlation_weights),
  composite("FakeNews", multi_items("FAKE", 1:4, weights = correlation_weights)),
  reflective("MACHINE",   single_item("AttCHK1R")),
  higher_composite("D_DISTRUST", c("D_MAL", "D_INCOMP", "D_DECT"), orthogonal, mode_A),
  higher_composite("DISTRUST", c("MAL", "INCOMP", "DECT"), orthogonal, mode_A)
)

# define structural model
SM1 <- relationships(
  paths(from = "MACHINE",      to = c("SOURCE", "DISTRUST")),
  paths(from = "SOURCE",        to = "FakeNews"),
  paths(from = "D_DISTRUST",   to = "DISTRUST"),
  paths(from = "DISTRUST",  to = "FakeNews")
)

SM2 <- relationships(
  paths(from = "MACHINE",      to = c("SOURCE", "MAL", "INCOMP", "DECT")),
  paths(from = "SOURCE",        to = "FakeNews"),
  paths(from = "MAL",  to = "FakeNews"),
  paths(from = "INCOMP",  to = "FakeNews"),
  paths(from = "DECT",  to = "FakeNews")
)

#Run PLS
PLSdf <- estimate_pls(
  data = df,
  measurement_model = MM,
  structural_model = SM2,
  inner_weights = path_weighting
)

#bootstrapping
boot_mobi_pls <- bootstrap_model(seminr_model = PLSdf,
                                 nboot = 1000,
                                 cores = 2)