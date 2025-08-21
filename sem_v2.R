
###SEM
library(lavaan)
forsem <- onlymanipulation

which(is.na(forsem$c_0001))
#no missings


#with (numerical) dummy variable 
#state need for security 



##add active media use as a moderator

dmcforsem <- forsem

#double mean centering
library(semTools)
dmcforsem <- indProd(dmcforsem, var1= c("ssec1", "ssec2", "ssec3"),
                     var2=c("tsec1", "tsec2", "tsec3"),
                     match = FALSE , meanC = TRUE ,
                     residualC = FALSE , doubleMC = TRUE) 
dmcforsem <- indProd(forsem, var1= c("ssec"),
                     var2=c("tsec"),
                     match = FALSE , meanC = TRUE ,
                     residualC = FALSE , doubleMC = TRUE) 
set.seed(545)
med.model <- '
  stateneedsecurity =~ ssec1+ssec2+ssec3
  Mainstream=~narrative_2  +narrative_4  +narrative_6  +narrative_8  
  Micronarratives=~narrative_1+narrative_3+narrative_5+ narrative_7 
  traitneedsecurity=~ tsec1+tsec2+tsec3
  Micronarratives ~b1*stateneedsecurity#+traitneedsecurity
  Mainstream ~b2*stateneedsecurity#+traitneedsecurity
  #ssectsec=~ssec1.tsec1+ssec2.tsec1+ssec3.tsec1+ssec2.tsec1+ssec2.tsec2+ssec2.tsec3+
  #ssec3.tsec1+ssec3.tsec2+ssec3.tsec3


  stateneedsecurity ~ a1*gr_1+ssec.tsec+traitneedsecurity
  Micronarratives ~ gr_1
  Mainstream ~gr_1
  Mainstream ~~Micronarratives

  ind1 := a1*b1
  ind2 := a1*b2
  

ssec2.tsec2 ~~ ssec3.tsec2
ssec2.tsec3 ~~ ssec3.tsec3
ssec2.tsec2	~~	ssec2.tsec3
ssec3.tsec1	~~	ssec3.tsec3
ssec2.tsec2	~~	ssec3.tsec3
ssec3.tsec1	~~	ssec3.tsec2
ssec2.tsec3	~~	ssec3.tsec2
ssec3.tsec1	~~	ssec2.tsec3
ssec3.tsec1	~~	ssec2.tsec2
ssec2.tsec1	~~	ssec2.tsec3
ssec2.tsec1	~~	ssec3.tsec3
ssec2.tsec1	~~	ssec2.tsec2
ssec2.tsec1	~~	ssec3.tsec2
ssec3.tsec2	~~	ssec3.tsec3




'

med.fit <- sem(med.model, data = dmcforsem, estimator = "ML", missing = "FIML",
               se = "bootstrap",bootstrap = 50L, parallel ="snow")
summary(med.fit, fit.measures=T, standardized = T, rsquare=TRUE)
estimates_med.fit <- parameterEstimates(med.fit, standardized=TRUE, boot.ci.type="perc", level=0.95,
                                        zstat = FALSE, pvalue = FALSE, output = "data.frame")

View(estimates_med.fit)
View(mi)
mi<- modificationindices(med.fit)
estimates_med.fit[62:63, ]

cor(forsem$tsec, forsem$ssec, use = "pairwise.complete.obs")

m1 <- lm(micronarratives~ssec, data=forsem)
summary(m1)

m2 <- lm(micronarratives~ssec+ tsec, data=forsem)
summary(m2)

m3 <- lm(micronarratives~tsec, data=forsem)
summary(m3)

m4 <- lm(ssec~tsec, data=forsem)
summary(m4)

anova(m1, m2)

car::avPlots(m2)

#with single factor

n.model <- '
  Mainstream=~narrative_2  +narrative_4  +narrative_6  +narrative_8  
  Micronarratives=~narrative_1+narrative_3+narrative_5+ narrative_7 

  Micronarratives ~ gr_1
  Mainstream ~gr_1
  Mainstream ~~Micronarratives


'

n.fit <- sem(n.model, data = forsem, estimator = "ML")
summary(n.fit, fit.measures=T, standardized = T, rsquare=TRUE)
estimates_n.fit <- parameterEstimates(n.fit, standardized=TRUE, boot.ci.type="perc", level=0.95,
                                        zstat = FALSE, pvalue = FALSE, output = "data.frame")

View(estimates_n.fit)
estimates_n.fit[61:62, ]

#visualisation
library(semptools)
library(semPlot)

semPaths(med1.fit,what= "std", layout = "tree", residuals = FALSE,
         nCharNodes = 0, fade= FALSE, sizeLat = 9, sizeMan = 9, label.cex = 1.4, edge.label.cex =1,
         intercepts = F, equalizeManifests=F, exoCov = F)

anova(med1.fit,med.fit)

pm_no_covs <- semptools::drop_nodes(
  semPlotModel(med.fit),
  c("ssec1", "ssec2", "ssec3", "narrative_2", "narrative_4", "narrative_6",
    "narrative_8","tsec1","tsec2","tsec3", "narrative_1",
    "narrative_3", "narrative_5", "narrative_7" ))

pfad_layout<- tidySEM::get_layout("traitneedsecurity","", "", "","Micronarratives",
                         "","", "","","",
                         "stateneedsecurity","","","","",
                         "","man_finance","","","Mainstream",
                         "gr_1","","","","",
                         "ssec1", "ssec2", "ssec3", "narrative_2", "narrative_4", 
                         "narrative_6","narrative_8","tsec1","tsec2","tsec3", 
                         "narrative_1","narrative_3", "narrative_5", "narrative_7", "",
                         rows = 8)


pm <- semPaths(pm_no_covs, what= "std", layout = pfad_layout, residuals = FALSE,
               nCharNodes = 0, fade= FALSE, sizeLat = 9, sizeMan = 9, label.cex = 1.4, edge.label.cex =1,
               intercepts = F, equalizeManifests=F, exoCov = F)


#add statistical significance asteriscs
library(semptools)
my_label_list <- list(list(node = "traitneedsecurity", to = "(T) Need for \nsecurity"),
                      list(node = "stateneedsecurity", to = "(S) Need for \nsecurity"),
                      list(node = "Micronarratives", to = "Anti-mainstream \nnarratives"),
                      list(node = "Mainstream", to = "Mainstream \nnarratives"),
                      list(node = "gr_1", to = "Group \n(vol=0, mand=1"))

p_pa2 <- mark_sig(pm, med.fit)
p_pa2 <- change_node_label(p_pa2, my_label_list)

plot(p_pa2)



##add trust as a buffer 

tmed.model <- '
  stateneedsecurity =~ ssec1+ssec2+ssec3
  Mainstream=~narrative_2  +narrative_4  +narrative_6  +narrative_8  
  Micronarratives=~narrative_1+narrative_3+narrative_5+ narrative_7 
  traitneedsecurity=~ tsec1+tsec2+tsec3

Micronarratives ~b1*stateneedsecurity+trust5+gr_1
  Mainstream ~b2*stateneedsecurity+trust5+gr_1
  ssectsec=~ssec1.tsec1+ssec2.tsec1+ssec3.tsec1+ssec2.tsec1+ssec2.tsec2+ssec2.tsec3+
  ssec3.tsec1+ssec3.tsec2+ssec3.tsec3

  stateneedsecurity ~ a1*gr_1+ssectsec+traitneedsecurity
  stateneedsecurity ~ a2*trust5

  ssec2.tsec2 ~~ ssec3.tsec2
ssec2.tsec3 ~~ ssec3.tsec3
ssec2.tsec2	~~	ssec2.tsec3
ssec3.tsec1	~~	ssec3.tsec3
ssec2.tsec2	~~	ssec3.tsec3
ssec3.tsec1	~~	ssec3.tsec2
ssec2.tsec3	~~	ssec3.tsec2
ssec3.tsec1	~~	ssec2.tsec3
ssec3.tsec1	~~	ssec2.tsec2
ssec2.tsec1	~~	ssec2.tsec3
ssec2.tsec1	~~	ssec3.tsec3
ssec2.tsec1	~~	ssec2.tsec2
ssec2.tsec1	~~	ssec3.tsec2
ssec3.tsec2	~~	ssec3.tsec3

      
ind1 := a2*b1
ind2 := a2*b2
      
ind3 := a1*b1
ind4 := a1*b2

'


tmed.fit <- sem(tmed.model, data = dmcforsem, estimator = "ML", missing = "FIML",
                se = "bootstrap",bootstrap = 5000L, parallel ="snow")
summary(tmed.fit, fit.measures=T, standardized = T, rsquare=TRUE)
estimates_tmed.fit <- parameterEstimates(tmed.fit, standardized=TRUE, boot.ci.type="perc", level=0.95,
                                         zstat = FALSE, pvalue = FALSE, output = "data.frame")

View(estimates_tmed.fit)

estimates_tmed.fit[105:108, ]




tm_no_covs <- semptools::drop_nodes(
  semPlotModel(tmed.fit),
  c("ssec1", "ssec2", "ssec3", "narrative_2", "narrative_4", "narrative_6",
    "narrative_8","tsec1","tsec2","tsec3", "narrative_1",
    "narrative_3", "narrative_5", "narrative_7" ))

pfad_layout<- get_layout("","traitneedsecurity", "", "","Micronarratives",
                         "trust5","", "","","",
                         "","stateneedsecurity","","","",
                         "gr_1","","","","Mainstream",
                         "ssec1", "ssec2", "ssec3", "narrative_2", "narrative_4", 
                         "narrative_6","narrative_8","tsec1","tsec2","tsec3", 
                         "narrative_1","narrative_3", "narrative_5", "narrative_7", "",
                         rows = 7)


tm <- semPaths(tm_no_covs, what= "std", layout = pfad_layout, residuals = FALSE,
               nCharNodes = 0, fade= FALSE, sizeLat = 9, sizeMan = 9, label.cex = 1.6, edge.label.cex =1,
               intercepts = F, equalizeManifests=F, exoCov = F)
my_label_list <- list(list(node = "traitneedsecurity", to = "(T) Need for \nsecurity"),
                      list(node = "stateneedsecurity", to = "(S) Need for \nsecurity"),
                      list(node = "trust5", to = "Trust in \nthe government"),
                      list(node = "Micronarratives", to = "Anti-mainstream \nnarratives"),
                      list(node = "Mainstream", to = "Mainstream \nnarratives"),
                      list(node = "gr_1", to = "Group \n(vol=0, mand=1"))
          

#add statistical significance asteriscs
library(semptools)

tp_pa2 <- mark_sig(tm, tmed.fit)
tp_pa2 <- change_node_label(tp_pa2, my_label_list)
plot(tp_pa2)

##add active media use as a moderator

dmcforsem <- forsem

#double mean centering
library(semTools)
dmcforsem <- indProd(dmcforsem, var1= c("ssec1", "ssec2", "ssec3"),
                     var2=c("med_act"),
                     match = FALSE , meanC = TRUE ,
                     residualC = FALSE , doubleMC = TRUE) 
amed.model <- '
stateneedsecurity =~ ssec1+ssec2+ssec3
  Mainstream=~narrative_2  +narrative_4  +narrative_6  +narrative_8  
  Micronarratives=~narrative_1+narrative_3+narrative_5+ narrative_7 
  traitneedsecurity=~ tsec1+tsec2+tsec3
  interaction=~ ssec1.med_act +ssec2.med_act +ssec3.med_act 
  
  
  
  Micronarratives ~stateneedsecurity+traitneedsecurity+trust5+med_act+interaction+gr_1
  Mainstream ~stateneedsecurity+traitneedsecurity+trust5+med_act+interaction+gr_1
  med_act ~ stateneedsecurity

  stateneedsecurity ~ gr_1
  stateneedsecurity ~ trust5
  traitneedsecurity ~ trust5
      
Micronarratives~~Mainstream
stateneedsecurity~~traitneedsecurity

'

amed.fit <- sem(amed.model, data = dmcforsem, estimator = "MLM")
summary(amed.fit, fit.measures=T, standardized = T, rsquare=TRUE)
estimates_amed.fit <- parameterEstimates(amed.fit, standardized=TRUE, boot.ci.type="perc", level=0.95,
                                         zstat = FALSE, pvalue = FALSE, output = "data.frame")

View(estimates_amed.fit)

estimates_amed.fit[71:72, ]




am_no_covs <- semptools::drop_nodes(
  semPlotModel(amed.fit),
  c("ssec1", "ssec2", "ssec3", "narrative_2", "narrative_4", "narrative_6",
    "narrative_8","tsec1","tsec2","tsec3", "narrative_1",
    "narrative_3", "narrative_5", "narrative_7", "ssec1.med_act","ssec2.med_act","ssec3.med_act"))

a_pfad_layout<- get_layout("","","traitneedsecurity", "", "Micronarratives","",
                         "","med_act","", "","","interaction",
                         "","","","","","",
                         "gr_1","stateneedsecurity","","","","",
                         "","","","","","",
                         "","trust5","","","Mainstream","",
                         "","","","","","",
                         "","ssec1", "ssec2", "ssec3", "narrative_2", "narrative_4", 
                         "","narrative_6","narrative_8","tsec1","tsec2","tsec3", 
                         "","narrative_1","narrative_3", "narrative_5", "narrative_7", "",
                         "","ssec1.med_act","ssec2.med_act","ssec3.med_act", "","",
                         rows = 11)


am <- semPaths(am_no_covs, what= "std", layout = a_pfad_layout, residuals = FALSE,
               nCharNodes = 0, fade= FALSE, sizeLat = 8, sizeMan = 8, label.cex = 1.5, edge.label.cex =1,
               intercepts = F, equalizeManifests=F, exoCov = F)


#add statistical significance asteriscs
library(semptools)
my_label_list <- list(list(node = "traitneedsecurity", to = "(T) Need for \nsecurity"),
                      list(node = "stateneedsecurity", to = "(S) Need for \nsecurity"),
                      list(node = "trust5", to = "Trust in \nthe government"),
                      list(node = "med_act", to = "Active \nmedia use"),
                      list(node = "interaction", to = "Interaction \nterm"),
                      list(node = "Micronarratives", to = "Anti-mainstream \nnarratives"),
                      list(node = "Mainstream", to = "Mainstream \nnarratives"),
                      list(node = "gr_1", to = "Group \n(vol=0, mand=1)"))


ap_pa2 <- mark_sig(am, amed.fit)
ap_pa2 <- change_node_label(ap_pa2, my_label_list)

plot(ap_pa2)




#state need for freedom without mediators and moderators
fdmcforsem <- forsem

library(semTools)
fdmcforsem <- indProd(fdmcforsem, var1= c("sfree1", "sfree2", "sfree3"),
                     var2=c("tfree1", "tfree2", "tfree3"),
                     match = FALSE , meanC = TRUE ,
                     residualC = FALSE , doubleMC = TRUE) 
set.seed(545)

freed.model <- '
  stateneedfreedom =~sfree1+sfree2+sfree3
  Mainstream=~narrative_2  +narrative_4  +narrative_6  +narrative_8  
  Micronarratives=~narrative_1+narrative_3+narrative_5+ narrative_7 
  traitneedfreedom=~tfree1+ tfree2+ tfree3
  sfreetfree=~sfree1.tfree1+sfree2.tfree1+sfree3.tfree1+sfree2.tfree1+sfree2.tfree2+sfree2.tfree3+
  sfree3.tfree1+sfree3.tfree2+sfree3.tfree3
  
  Micronarratives ~b1*stateneedfreedom+gr_1
  Mainstream ~b2*stateneedfreedom+gr_1

  stateneedfreedom ~ a1*gr_1+traitneedfreedom+sfreetfree

  ind1 := a1*b1
  ind2 := a1*b2
  
  Micronarratives~~Mainstream

sfree2.tfree2	~~	sfree3.tfree2
sfree2.tfree2	~~	sfree2.tfree3
sfree3.tfree2	~~	sfree3.tfree3
sfree1.tfree1	~~	sfree2.tfree1
sfree2.tfree3	~~	sfree3.tfree2
sfree2.tfree2	~~	sfree3.tfree3
sfree2.tfree1	~~	sfree3.tfree3
sfree2.tfree1	~~	sfree3.tfree2
sfree2.tfree3	~~	sfree3.tfree3
sfree1.tfree1 ~~ sfree2.tfree3

'




freed.fit <- sem(freed.model, data = fdmcforsem, estimator = "ML", missing = "FIML",
                 se = "bootstrap",bootstrap = 5000L, parallel ="snow", check.gradient = TRUE)
summary(freed.fit, fit.measures=T, standardized = T, rsquare=TRUE)
estimates_freed.fit <- parameterEstimates(freed.fit, standardized=TRUE, boot.ci.type="perc", level=0.95,
                                          zstat = FALSE, pvalue = FALSE, output = "data.frame")

estimates_freed.fit[61:62, ]
mif<- modificationindices(freed.fit)
View(mif)
lavInspect(freed.fit, "cov.lv")

fm_no_covs <- semptools::drop_nodes(
  semPlotModel(freed.fit),
  c("sfree1", "sfree2", "sfree3", "narrative_2", "narrative_4", "narrative_6",
    "narrative_8","tfree1","tfree2","tfree3", "narrative_1",
    "narrative_3", "narrative_5", "narrative_7"))
f_pfad_layout<- get_layout("traitneedfreedom","", "", "","Micronarratives",
                           "","", "","","",
                           "stateneedfreedom","","","","",
                           "","","","","Mainstream",
                           "gr_1","","","","",
                           "sfree1", "sfree2", "sfree3", "narrative_2", "narrative_4", 
                           "narrative_6","narrative_8","tfree1","tfree2","tfree3", 
                           "narrative_1","narrative_3", "narrative_5", "narrative_7", "",
                           rows = 8)

fm <- semPaths(fm_no_covs, what= "std", layout = f_pfad_layout, residuals = FALSE,
               nCharNodes = 0, fade= FALSE, sizeLat = 8, label.cex = 1.2, edge.label.cex =1,
               intercepts = F, equalizeManifests=F, exoCov = F)


#add statistical significance asteriscs
library(semptools)

fp_pa2 <- mark_sig(fm, freed.fit)
plot(fp_pa2)



#need for freedom with trust as buffer 

frtr.model <- '
  needfreedom =~ sfree1+sfree2+sfree3
  Mainstream =~ narrative_2+narrative_4+narrative_6+narrative_8  
  Micronarratives =~ narrative_1+narrative_3+narrative_5+ narrative_7 
  traitneedfreedom =~ tfree1+ tfree2+ tfree3
  sfreetfree =~ sfree1.tfree1+sfree2.tfree1+sfree3.tfree1+sfree2.tfree1+sfree2.tfree2+sfree2.tfree3+
  sfree3.tfree1+sfree3.tfree2+sfree3.tfree3

  Micronarratives ~b1*needfreedom+gr_1+trust5
  Mainstream ~b2*needfreedom+gr_1+trust5
  needfreedom ~ a2*gr_1+a1*trust5+sfreetfree+traitneedfreedom

  Micronarratives~~Mainstream
      
  ind1 := a1*b1
  ind2 := a1*b2
  ind3 := a2*b1
  ind4 := a2*b2
sfree2.tfree3	~~	sfree3.tfree3
sfree2.tfree2	~~	sfree3.tfree2
sfree2.tfree2	~~	sfree2.tfree3
sfree3.tfree2	~~	sfree3.tfree3
sfree1.tfree1	~~	sfree2.tfree1
sfree2.tfree3	~~	sfree3.tfree2
sfree2.tfree2	~~	sfree3.tfree3
sfree2.tfree1	~~	sfree3.tfree3
sfree2.tfree1	~~	sfree3.tfree2

'

frtr.fit <- sem(frtr.model, data = fdmcforsem, estimator = "ML", missing = "FIML",
                se = "bootstrap",bootstrap = 5000L, parallel ="snow")
summary(frtr.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)
estimates_frtr.fit <- parameterEstimates(frtr.fit, standardized=TRUE, boot.ci.type="perc", level=0.95,zstat = FALSE, pvalue = FALSE, output = "data.frame")

estimates_frtr.fit[67:68, ]

mit<- modificationindices(frtr.fit)
View(mit)


#need for freedom with active media use as moderator
fdmcforsem <- forsem

#double mean centering
library(semTools)
fdmcforsem <- indProd(fdmcforsem, var1= c("sfree1", "sfree2", "sfree3"),
                     var2=c("med_act"),
                     match = FALSE , meanC = TRUE ,
                     residualC = FALSE , doubleMC = TRUE) 
framed.model <- '
  stateneedfreedom =~ sfree1+sfree2+sfree3
  Mainstream=~narrative_2  +narrative_4  +narrative_6  +narrative_8  
  Micronarratives=~narrative_1+narrative_3+narrative_5+ narrative_7 
  traitneedfreedom=~ tfree1+tfree2+tfree
  interaction=~ sfree1.med_act +sfree2.med_act +sfree3.med_act 
  
  Micronarratives ~stateneedfreedom+traitneedfreedom+trust5+med_act+interaction+gr_1
  Mainstream ~stateneedfreedom+traitneedfreedom+trust5+med_act+interaction+gr_1
  med_act ~ stateneedfreedom

  stateneedfreedom ~ gr_1
  stateneedfreedom ~ trust5
  traitneedfreedom ~ trust5
  
  stateneedfreedom~~traitneedfreedom
  Micronarratives~~Mainstream
  
'

framed.fit <- sem(framed.model, data = fdmcforsem, estimator = "MLM")
summary(framed.fit, fit.measures=T, standardized = T, rsquare=TRUE)
estimates_framed.fit <- parameterEstimates(framed.fit, standardized=TRUE, boot.ci.type="perc", level=0.95,
                                         zstat = FALSE, pvalue = FALSE, output = "data.frame")

View(estimates_framed.fit)

estimates_framed.fit[71:72, ]

fm_no_covs <- semptools::drop_nodes(
  semPlotModel(freed.fit),
  c("sfree1", "sfree2", "sfree3", "narrative_2", "narrative_4", "narrative_6",
    "narrative_8","tfree1","tfree2","tfree3", "narrative_1",
    "narrative_3", "narrative_5", "narrative_7", "sfree1.med_act","sfree2.med_act","sfree3.med_act"))

f_pfad_layout<- get_layout("","","traitneed /n freedom", "", "Micronarratives","",
                           "","med_act","", "","","interaction",
                           "","","","","","",
                           "gr_1","stateneedfreedom","","","","",
                           "","","","","","",
                           "","trust5","","","Mainstream","",
                           "","","","","","",
                           
                           "","sfree1", "sfree2", "sfree3", "narrative_2", "narrative_4", 
                           "","narrative_6","narrative_8","tfree1","tfree2","tfree3", 
                           "","narrative_1","narrative_3", "narrative_5", "narrative_7", "",
                           "","sfree1.med_act","sfree2.med_act","sfree3.med_act", "","",
                           rows = 11)


fm <- semPaths(fm_no_covs, what= "std", layout = a_pfad_layout, residuals = FALSE,
               nCharNodes = 0, fade= FALSE, sizeLat = 8, label.cex = 1.2, edge.label.cex =1,
               intercepts = F, equalizeManifests=F, exoCov = F)

#you should now have 3 SEMs each for need for security and need for freedom 

#ignore

set.seed(12345)
med.model <- '
  stateneedsecurity =~ ssec1+ssec2+ssec3
  Mainstream=~narrative_2  +narrative_4  +narrative_6  +narrative_8  
  Micronarratives=~narrative_1+narrative_3+narrative_5+ narrative_7 
  traitneedsecurity=~ tsec1+tsec2+tsec3
  Micronarratives ~b1*stateneedsecurity+traitneedsecurity+man_finance
  Mainstream ~b2*stateneedsecurity+traitneedsecurity+man_finance


  stateneedsecurity ~ a1*gr_1+man_finance
  Micronarratives ~ gr_1
  Mainstream ~gr_1
  Mainstream ~~Micronarratives
  stateneedsecurity~~traitneedsecurity
  man_finance~gr_1
  
  ind1 := a1*b1
  ind2 := a1*b2

'

med.fit <- sem(med.model, data = forsem, estimator = "ML", missing = "FIML",
               se = "bootstrap",bootstrap = 500L, parallel ="snow")
summary(med.fit, fit.measures=T, standardized = T, rsquare=TRUE)
estimates_med.fit <- parameterEstimates(med.fit, standardized=TRUE, boot.ci.type="perc", level=0.95,
                                        zstat = FALSE, pvalue = FALSE, output = "data.frame")

View(estimates_med.fit)
estimates_med.fit[61:62, ]


a <- aov(man_finance~gr_1, data= forsem)
summary(a)



freed.model <- '
  stateneedfreedom =~ sfree1+sfree2+sfree3
  Mainstream=~narrative_2  +narrative_4  +narrative_6  +narrative_8  
  Micronarratives=~narrative_1+narrative_3+narrative_5+ narrative_7 
  traitneedfreedom=~  tfree1+ tfree2+ tfree3

  Micronarratives ~b1*stateneedfreedom+traitneedfreedom+man_finance
  Mainstream ~b2*stateneedfreedom+traitneedfreedom+man_finance


  stateneedfreedom ~ a1*gr_1+man_finance
  Micronarratives ~ gr_1
  Mainstream ~gr_1
  man_finance~gr_1

  ind1 := a1*b1
  ind2 := a1*b2
  
  Micronarratives~~Mainstream
  stateneedfreedom~~traitneedfreedom

'

freed.fit <- sem(freed.model, data = forsem, estimator = "ML", missing = "FIML",
                 se = "bootstrap",bootstrap = 500L, parallel ="snow")
summary(freed.fit, fit.measures=T, standardized = T, rsquare=TRUE)




fm_no_covs <- semptools::drop_nodes(
  semPlotModel(freed.fit),
  c("sfree1", "sfree2", "sfree3", "narrative_2", "narrative_4", "narrative_6",
    "narrative_8","tfree1","tfree2","tfree3", "narrative_1",
    "narrative_3", "narrative_5", "narrative_7"))
f_pfad_layout<- tidySEM::get_layout("traitneedfreedom","", "", "","Micronarratives",
                           "","", "","","",
                           "stateneedfreedom","","","","",
                           "","man_finance","","","Mainstream",
                           "gr_1","","","","",
                           "sfree1", "sfree2", "sfree3", "narrative_2", "narrative_4", 
                           "narrative_6","narrative_8","tfree1","tfree2","tfree3", 
                           "narrative_1","narrative_3", "narrative_5", "narrative_7", "",
                           rows = 8)

fm <- semPaths(fm_no_covs, what= "std", layout = f_pfad_layout, residuals = FALSE,
               nCharNodes = 0, fade= FALSE, sizeLat = 8, label.cex = 1.2, edge.label.cex =1,
               intercepts = F, equalizeManifests=F, exoCov = F)


#add statistical significance asteriscs
library(semptools)

fp_pa2 <- mark_sig(fm, freed.fit)
plot(fp_pa2)



CTT::reliability(ssec, itemal = TRUE, NA.Delete = TRUE, ml = TRUE)
CTT::reliability(sfree, itemal = TRUE, NA.Delete = TRUE, ml = TRUE)
