library(jtools)

setwd("c:/Users/Yvan/Desktop/forKayla/")
spiders = read.csv("PathAnalysisData.csv")


spiders2 = log(spiders[,3:12])
head(spiders2)


#HUNTING model
mod2 = lm(GroundSpi ~ SoilPollution + PreyBreadth + LSdiv200, spiders2)
summary(mod2)

#Breadth
##jpeg("HUNTING_PreyBreadth.#jpeg", w=5, h=5, units='in', res=350)
effect_plot(mod2, pred = PreyBreadth, interval=T, plot.points=T, partial.residuals=T,
            point.color="red", point.size=4.0, 
            x.label="Prey  Breadth" , y.label="Hunting-Spider  Abundance")+
  theme_apa(legend.pos = "right", legend.use.title = T,
            legend.font.size = 17, x.font.size = 17, y.font.size = 17,
            facet.title.size = 17, remove.y.gridlines = TRUE,
            remove.x.gridlines = TRUE)
##dev.off()              

#Pollution
#jpeg("HUNTING_Pollution.#jpeg", w=5, h=5, units='in', res=350)
effect_plot(mod2, pred = SoilPollution, interval=T, plot.points=T, partial.residuals=T,
            point.color="red", point.size=4.0,
            x.label="Soil  Pollution  Index" , y.label="Hunting-Spider  Abundance")+
  theme_apa(legend.pos = "right", legend.use.title = T,
            legend.font.size = 17, x.font.size = 17, y.font.size = 17,
            facet.title.size = 17, remove.y.gridlines = TRUE,
            remove.x.gridlines = TRUE)
#dev.off()              

#Lscp Diversity
#jpeg("HUNTING_LSdiversity.#jpeg", w=5, h=5, units='in', res=350)
effect_plot(mod2, pred = LSdiv200, interval=T, plot.points=T, partial.residuals=T,
            point.color="red", point.size=4.0,
            x.label="Landscape  Diversity" , y.label="Hunting-Spider  Abundance")+
  theme_apa(legend.pos = "right", legend.use.title = T,
            legend.font.size = 17, x.font.size = 17, y.font.size = 17,
            facet.title.size = 17, remove.y.gridlines = TRUE,
            remove.x.gridlines = TRUE)
#dev.off()              



#WEB model
mod5 = lm(OrbShtSpi ~ SoilPollution + VegBiomass + LSdiv200, spiders2)
summary(mod5)

#Biomass
#jpeg("Web_Biomass.#jpeg", w=5, h=5, units='in', res=350)
effect_plot(mod5, pred = VegBiomass, interval=T, plot.points=T, partial.residuals=T,
            point.color="blue", point.size=4.0, 
            x.label="Vegetation  Biomass" , y.label="Web-Spider  Abundance")+
              theme_apa(legend.pos = "right", legend.use.title = T,
                        legend.font.size = 17, x.font.size = 17, y.font.size = 17,
                        facet.title.size = 17, remove.y.gridlines = TRUE,
                        remove.x.gridlines = TRUE)
#dev.off()              

#Pollution
#jpeg("Web_Pollution.#jpeg", w=5, h=5, units='in', res=350)
effect_plot(mod5, pred = SoilPollution, interval=T, plot.points=T, partial.residuals=T,
            point.color="blue", point.size=4.0,
            x.label="Soil  Pollution  Index" , y.label="Web-Spider  Abundance")+
  theme_apa(legend.pos = "right", legend.use.title = T,
            legend.font.size = 17, x.font.size = 17, y.font.size = 17,
            facet.title.size = 17, remove.y.gridlines = TRUE,
            remove.x.gridlines = TRUE)
#dev.off()              

#Lscp Diversity
#jpeg("Web_LSdiversity.#jpeg", w=5, h=5, units='in', res=350)
effect_plot(mod5, pred = LSdiv200, interval=T, plot.points=T, partial.residuals=T,
            point.color="blue", point.size=4.0,
            x.label="Landscape  Diversity" , y.label="Web-Spider  Abundance")+
  theme_apa(legend.pos = "right", legend.use.title = T,
            legend.font.size = 17, x.font.size = 17, y.font.size = 17,
            facet.title.size = 17, remove.y.gridlines = TRUE,
            remove.x.gridlines = TRUE)
#dev.off()              


## Biomass vs Clay
mod6 = lm(VegBiomass ~ SoilClay, spiders2)
summary(mod0)

#jpeg("Biomass_Clay.#jpeg", w=5, h=5, units='in', res=350)
effect_plot(mod6, pred = SoilClay, interval=T, plot.points=T, point.color="darkgreen", point.size=4.0,
            x.label="Clay  in  Soil" , y.label="Plant  Biomass")+
  theme_apa(legend.pos = "right", legend.use.title = T,
            legend.font.size = 17, x.font.size = 17, y.font.size = 17,
            facet.title.size = 17, remove.y.gridlines = TRUE,
            remove.x.gridlines = TRUE)
#dev.off()



mod7 = lm(VegBiomass ~ Trt, spiders)
#jpeg("VegBiomass_Treatment.#jpeg", w=5, h=5, units='in', res=350)
effect_plot(mod7, pred = Trt, interval=T, plot.points=T, point.color="darkgreen", point.size=4.0,
            x.label="VacLot                     Prairie" , y.label="Vegetation  Biomass")+
  theme_apa(legend.font.size = 17, x.font.size = 17, y.font.size = 17,
            facet.title.size = 17, remove.y.gridlines = TRUE,
            remove.x.gridlines = TRUE)
#dev.off()

mod8 = lm(SoilPollution ~ Trt, spiders)
#jpeg("SoilPollution_Treatment.#jpeg", w=5, h=5, units='in', res=350)
effect_plot(mod8, pred = Trt, interval=T, plot.points=T, point.color="darkgreen", point.size=4.0,
            x.label="VacLot                     Prairie" , y.label="Soil  Pollution  Index")+
  theme_apa(legend.pos = "right", legend.use.title = T,
            legend.font.size = 17, x.font.size = 17, y.font.size = 17,
            facet.title.size = 17, remove.y.gridlines = TRUE,
            remove.x.gridlines = TRUE)
#dev.off()


mod9 = lm(PreyBreadth ~ Trt, spiders)
#jpeg("PreyBreadth_Treatment.#jpeg", w=5, h=5, units='in', res=350)
effect_plot(mod9, pred = Trt, interval=T, plot.points=T, point.color="darkgreen", point.size=4.0,
            x.label="VacLot                     Prairie" , y.label="Prey  Breadth")+
  theme_apa(legend.pos = "right", legend.use.title = T,
            legend.font.size = 17, x.font.size = 17, y.font.size = 17,
            facet.title.size = 17, remove.y.gridlines = TRUE,
            remove.x.gridlines = TRUE)
#dev.off()

