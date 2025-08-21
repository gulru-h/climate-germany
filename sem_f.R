
###SEM
library(lavaan)
forsem <- onlymanipulation

which(is.na(forsem$c_0001))
#no missings


#with (numerical) dummy variable 
#state need for security 

 
set.seed(545)
med.model <- '
  stateneedsecurity =~ ssec1+ssec2+ssec3
  Mainstream=~narrative_2+narrative_4+narrative_6+narrative_8  
  Micronarratives=~narrative_1+narrative_3+narrative_5+narrative_7 
  traitneedsecurity=~ tsec1+tsec2+tsec3
  finance =~finan1+finan2+finan3
  
  Micronarratives ~b1*stateneedsecurity+traitneedsecurity+gr_1+finance
  Mainstream ~b2*stateneedsecurity+traitneedsecurity+gr_1+finance

  stateneedsecurity ~ a1*gr_1+traitneedsecurity+finance

  Mainstream~~Micronarratives

  ind1 := a1*b1
  ind2 := a1*b2

'

med.fit <- sem(med.model, data = forsem, estimator = "ML", missing = "FIML",
               se = "bootstrap",bootstrap = 5000L, 
               parallel ="multicore", verbose= T)
summary(med.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)
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


##add trust as a buffer 

tmed.model <- '
  stateneedsecurity =~ ssec1+ssec2+ssec3
  Mainstream=~narrative_2  +narrative_4  +narrative_6  +narrative_8  
  Micronarratives=~narrative_1+narrative_3+narrative_5+ narrative_7 
  traitneedsecurity=~ tsec1+tsec2+tsec3
  finance =~finan1+finan2+finan3


  Micronarratives ~b1*stateneedsecurity+trust5+gr_1+traitneedsecurity+finance
  Mainstream ~b2*stateneedsecurity+trust5+gr_1+traitneedsecurity+finance
  stateneedsecurity ~ a1*gr_1+traitneedsecurity+a2*trust5+finance
  trust5 ~ traitneedsecurity+finance

  ind1 := a2*b1
  ind2 := a2*b2
        
  ind3 := a1*b1
  ind4 := a1*b2

'


tmed.fit <- sem(tmed.model, data = forsem, estimator = "ML",
                missing = "FIML",se = "bootstrap",bootstrap = 5000L, 
                parallel ="multicore", verbose= F)
summary(tmed.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)
estimates_tmed.fit <- parameterEstimates(tmed.fit, standardized=TRUE, boot.ci.type="perc", level=0.95,
                                         zstat = FALSE, pvalue = FALSE, output = "data.frame")

View(estimates_tmed.fit)

estimates_tmed.fit[105:108, ]



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
  med_act ~ traitneedsecurity

  stateneedsecurity ~ gr_1+traitneedsecurity+med_act
  stateneedsecurity ~ trust5
  trust5~traitneedsecurity
      
Micronarratives~~Mainstream
'

amed.fit <- sem(amed.model, data = dmcforsem, estimator = "MLM")
summary(amed.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci =T)
estimates_amed.fit <- parameterEstimates(amed.fit, standardized=TRUE, boot.ci.type="perc", level=0.95,
                                         zstat = FALSE, pvalue = FALSE, output = "data.frame")

View(estimates_amed.fit)

estimates_amed.fit[71:72, ]

#state need for freedom without mediators and moderators

set.seed(545)

freed.model <- '
  stateneedfreedom =~sfree1+sfree2+sfree3
  Mainstream=~narrative_2  +narrative_4  +narrative_6  +narrative_8  
  Micronarratives=~narrative_1+narrative_3+narrative_5+ narrative_7 
  traitneedfreedom=~tfree1+ tfree2+ tfree3
  finance =~finan1+finan2+finan3

  
  Micronarratives ~b1*stateneedfreedom+gr_1+traitneedfreedom+finance
  Mainstream ~b2*stateneedfreedom+gr_1+traitneedfreedom+finance

  stateneedfreedom ~ a1*gr_1+traitneedfreedom+finance

  ind1 := a1*b1
  ind2 := a1*b2
  
  Micronarratives~~Mainstream

'


freed.fit <- sem(freed.model, data = forsem, estimator = "ML", missing = "FIML",
                 se = "bootstrap",bootstrap = 5000L, 
                 parallel ="multicore", verbose= T)
summary(freed.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)
estimates_freed.fit <- parameterEstimates(freed.fit, standardized=TRUE, boot.ci.type="perc", level=0.95,
                                          zstat = FALSE, pvalue = FALSE, output = "data.frame")

estimates_freed.fit[61:62, ]
mif<- modificationindices(freed.fit)
View(mif)
lavInspect(freed.fit, "cov.lv")


#need for freedom with trust as buffer 

frtr.model <- '
  needfreedom =~ sfree1+sfree2+sfree3
  Mainstream =~ narrative_2+narrative_4+narrative_6+narrative_8  
  Micronarratives =~ narrative_1+narrative_3+narrative_5+ narrative_7 
  traitneedfreedom =~ tfree1+ tfree2+ tfree3
  finance =~finan1+finan2+finan3

  Micronarratives ~b1*needfreedom+gr_1+trust5+traitneedfreedom+finance
  Mainstream ~b2*needfreedom+gr_1+trust5+traitneedfreedom+finance
  needfreedom ~ a2*gr_1+a1*trust5+traitneedfreedom+finance
  trust5~traitneedfreedom+finance

  Micronarratives~~Mainstream
      
  ind1 := a1*b1
  ind2 := a1*b2
  ind3 := a2*b1
  ind4 := a2*b2


'

frtr.fit <- sem(frtr.model, data = forsem, estimator = "ML", missing = "FIML",
                se = "bootstrap",bootstrap = 5000L, 
                parallel ="multicore", verbose= T)
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
  traitneedfreedom=~ tfree1+tfree2+tfree3
  interaction=~ sfree1.med_act +sfree2.med_act +sfree3.med_act 
  
  Micronarratives ~stateneedfreedom+traitneedfreedom+trust5+med_act+interaction+gr_1
  Mainstream ~stateneedfreedom+traitneedfreedom+trust5+med_act+interaction+gr_1
  med_act ~ traitneedfreedom

  stateneedfreedom ~ gr_1+traitneedfreedom+med_act
  stateneedfreedom ~ trust5
  trust5~ traitneedfreedom 
  
  Micronarratives~~Mainstream
  
'

framed.fit <- sem(framed.model, data = fdmcforsem, estimator = "MLM")
summary(framed.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)
estimates_framed.fit <- parameterEstimates(framed.fit, standardized=TRUE, boot.ci.type="perc", level=0.95,
                                         zstat = FALSE, pvalue = FALSE, output = "data.frame")

View(estimates_framed.fit)

estimates_framed.fit[71:72, ]


#you should now have 3 SEMs each for need for security and need for freedom 

#ignore#####

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
