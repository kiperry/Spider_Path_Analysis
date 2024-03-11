library(lavaan)
library(semPlot)


#####################  PATH ANALYSIS  ####################


setwd("c:/Users/Yvan/Desktop/forKayla/")
main_data = read.csv("PathAnalysisData.csv")

## dotplot (Cleveland's) showed dispersion across all variables, hence variables were log-transformed
log_data = log(main_data[,-1:-2])

## We started with a model that included all variables # PreyAbundance was removed to improve the model


### Final Model

ALLspider5b <- "
VegBiomass ~ SoilClay
PreyBreadth ~ VegBiomass
OrbShtSpi ~ SoilPollution + VegBiomass + LSdiv200
GroundSpi ~ SoilPollution + PreyBreadth + LSdiv200
## residual correlations
"
# Fit model
fit_ALLspider5b = 
  lavaan(ALLspider5b, data = log_data, auto.var = T, auto.fix.first = T, auto.cov.lv.x = T)

# Summary statistics
summary(fit_ALLspider5b, standardized = T, fit.measures = T, modindices = F)

# Generate modification indices
mi_ALLspider5b <- modindices(fit_ALLspider5b)  
head(mi_ALLspider5b[order(mi_ALLspider5b$mi, decreasing = T), ], 10)

# Create SEM diagram
semPaths(fit_ALLspider5b, "std", bifactor = "g", layout = "tree2", residuals = F, exoCov = F)


#### INDIRECT effects
# based on  https://nmmichalak.github.io/nicholas_michalak/blog_entries/2018/nrg01/nrg01.html

mod2 = "
VegBiomass ~ a * SoilClay
PreyBreadth ~ b * VegBiomass
OrbShtSpi ~  SoilPollution + c * VegBiomass + LSdiv200
GroundSpi ~  SoilPollution + d * PreyBreadth + LSdiv200

clay.biom.web: = a * c
clay.biom.breadth.hunt: = a * b * d
biomass.breadth.hunt: = b * d
"
semod2 = sem(mod2, data = log_data, se = "bootstrap", bootstrap = 100) # 1 min for every 1000
summary(semod2, standardized = TRUE)
