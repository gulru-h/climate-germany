
###SEM
library(lavaan)
forsem <- onlymanipulation

which(is.na(forsem$c_0001))

center_scale <- function(x) {
  scale(x, scale = T, center=T)
}

#forsem_s <- center_scale(forsem[, c(6:57, 64:73)])
#forsem_s <- data.frame(forsem_s, forsem[, c(1:5, 57:63, 74:77)])


#with dummy variable 

set.seed(12345)
med.model <- '
  stateneedsecurity =~ ssec1+ssec2+ssec3
  Mainstream=~narrative_2  +narrative_4  +narrative_6  +narrative_8  
  Micronarratives=~narrative_1+narrative_3+narrative_5+ narrative_7 
  traitneedsecurity=~ tsec1+tsec2+tsec3
  Micronarratives ~b1*stateneedsecurity+traitneedsecurity
  Mainstream ~b2*stateneedsecurity+traitneedsecurity


  stateneedsecurity ~ a1*gr_1
  Micronarratives ~ gr_1
  Mainstream ~gr_1
  Mainstream ~~Micronarratives
  
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


library(semptools)
library(semPlot)

pm_no_covs <- semptools::drop_nodes(
  semPlotModel(med.fit),
  c("ssec1", "ssec2", "ssec3", "narrative_2", "narrative_4", "narrative_6",
    "narrative_8","tsec1","tsec2","tsec3", "narrative_1",
    "narrative_3", "narrative_5", "narrative_7" ))

pfad_layout<- get_layout("traitneedsecurity","", "", "","Micronarratives",
                         "","", "","","",
                         "stateneedsecurity","","","","",
                         "","","","","Mainstream",
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



##add trust 


tmed.model <- '
  stateneedsecurity =~ ssec1+ssec2+ssec3
  Mainstream=~narrative_2  +narrative_4  +narrative_6  +narrative_8  
  Micronarratives=~narrative_1+narrative_3+narrative_5+ narrative_7 
  traitneedsecurity=~ tsec1+tsec2+tsec3

Micronarratives ~b1*stateneedsecurity+traitneedsecurity+trust5+gr_1
  Mainstream ~b2*stateneedsecurity+traitneedsecurity+trust5+gr_1


  stateneedsecurity ~ gr_1
  stateneedsecurity ~ a1*trust5
  traitneedsecurity ~ trust5
      
ind1 := a1*b1
ind2 := a1*b2

'


tmed.fit <- sem(tmed.model, data = forsem, estimator = "ML", missing = "FIML",
                se = "bootstrap",bootstrap = 500L, parallel ="snow")
summary(tmed.fit, fit.measures=T, standardized = T, rsquare=TRUE)
estimates_tmed.fit <- parameterEstimates(tmed.fit, standardized=TRUE, boot.ci.type="perc", level=0.95,
                                         zstat = FALSE, pvalue = FALSE, output = "data.frame")

View(estimates_tmed.fit)

estimates_tmed.fit[68:69, ]
estimates_tmed.fit[23, ]




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


##add active media use 

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
  
  
  
  Micronarratives ~stateneedsecurity+traitneedsecurity+trust5+med_act+interaction
  Mainstream ~stateneedsecurity+traitneedsecurity+trust5+med_act+interaction
  med_act ~ stateneedsecurity

  stateneedsecurity ~ gr_1
  stateneedsecurity ~ trust5
  traitneedsecurity ~ trust5
      

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




#freedom






freed.model <- '
  stateneedfreedom =~ sfree1+sfree2+sfree3
  Mainstream=~narrative_2  +narrative_4  +narrative_6  +narrative_8  
  Micronarratives=~narrative_1+narrative_3+narrative_5+ narrative_7 
  traitneedfreedom=~  tfree1+ tfree2+ tfree3

  Micronarratives ~b1*stateneedfreedom+traitneedfreedom
  Mainstream ~b2*stateneedfreedom+traitneedfreedom


  stateneedfreedom ~ a1*gr_1
  Micronarratives ~ gr_1
  Mainstream ~gr_1

  ind1 := a1*b1
  ind2 := a1*b2

  

'

freed.fit <- sem(freed.model, data = forsem, estimator = "ML", missing = "FIML",
                 se = "bootstrap",bootstrap = 500L, parallel ="snow")
summary(freed.fit, fit.measures=T, standardized = T, rsquare=TRUE)
estimates_freed.fit <- parameterEstimates(freed.fit, standardized=TRUE, boot.ci.type="perc", level=0.95,
                                          zstat = FALSE, pvalue = FALSE, output = "data.frame")

estimates_freed.fit[61:62, ]



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




frtr.model <- '
  needfreedom =~ sfree1+sfree2+sfree3
  Mainstream=~narrative_2  +narrative_4  +narrative_6  +narrative_8  
  Micronarratives=~narrative_1+narrative_3+narrative_5+ narrative_7 
  traitneedfreedom=~  tfree1+ tfree2+ tfree3

  Micronarratives ~b1*needfreedom+traitneedfreedom
  Mainstream ~b2*needfreedom+traitneedfreedom


  needfreedom ~ gr_1
  Micronarratives ~ gr_1
  Mainstream ~gr_1

  ind1 := a1*b1
  ind2 := a1*b2

  needfreedom ~ a1*trust5
  traitneedfreedom ~ trust5
      
    

'

frtr.fit <- sem(frtr.model, data = forsem, estimator = "ML", missing = "FIML",
                se = "bootstrap",bootstrap = 5000L, parallel ="snow")
summary(frtr.fit, fit.measures=T, standardized = T, rsquare=TRUE)
estimates_frtr.fit <- parameterEstimates(frtr.fit, standardized=TRUE, boot.ci.type="perc", level=0.95,zstat = FALSE, pvalue = FALSE, output = "data.frame")

estimates_frtr.fit[66:67, ]






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
  
  
  
  Micronarratives ~stateneedfreedom+traitneedfreedom+trust5+med_act+interaction
  Mainstream ~stateneedfreedom+traitneedfreedom+trust5+med_act+interaction
  med_act ~ stateneedfreedom

  stateneedfreedom ~ gr_1
  stateneedfreedom ~ trust5
  traitneedfreedom ~ trust5
      

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


#######kable###### 



table_fit <- matrix(NA, nrow = 6, ncol = 9)
colnames(table_fit) = c("Model", "X2", "df", "CFI", "RMSEA", "SRMR", "X2 Diff",
                        "Pr(>X2)", "AIC")
table_fit[1, ] <- c("Overall Model", round(fitmeasures(t.fit, 
                                                       c("chisq", "df", "cfi",
                                                         "rmsea", "srmr")),3),
                    NA, NA, round((a[1,2])))

table_fit[2, ] <- c("Micronarratives ~0*trust5", round(fitmeasures(t1.fit, 
                                                                   c("chisq", "df", "cfi",
                                                                     "rmsea", "srmr")),3),
                    round((t1[2,5]),3), round((t1[2,7]),3), round((t1[2,2])))
table_fit[3, ] <- c("Mainstream ~0*trust5", round(fitmeasures(t3.fit, 
                                                              c("chisq", "df", "cfi",
                                                                "rmsea", "srmr", "")),3), 
                    round((t3[2,5]),3), round((t3[2,7]),3), round((t3[2,2])))
table_fit[4, ] <- c(" stateneedsecurity ~ 0*gr_2", round(fitmeasures(t4.fit, 
                                                                c("chisq", "df", "cfi",
                                                                  "rmsea", "srmr", "")),3), 
                    round((t4[2,5]),3), round((t4[2,7]),3), round((t4[2,2])))
table_fit[5, ] <- c(" stateneedsecurity ~ 0*trust5", round(fitmeasures(t5.fit, 
                                                                  c("chisq", "df", "cfi",
                                                                    "rmsea", "srmr", "")),3), 
                    round((t5[2,5]),3), round((t5[2,7]),3), round((t5[2,2])))
table_fit[6, ] <- c("traitneedsecurity ~ 0*trust5", round(fitmeasures(t6.fit, 
                                                                      c("chisq", "df", "cfi",
                                                                        "rmsea", "srmr", "")),3), 
                    round((t6[2,5]),3), round((t6[2,7]),3), round((t6[2,2])))


kable(table_fit)




free.model <- '
  needfreedom =~ sfree1+sfree2+sfree3
  Mainstream=~narrative_2  +narrative_4  +narrative_6  +narrative_8  
  Micronarratives=~narrative_1+narrative_3+narrative_5+ narrative_7 
  traitneedfreedom=~  tfree1+ tfree2+ tfree3

  Micronarratives ~b1*needfreedom+traitneedfreedom+traitsecurity+b3*security
  Mainstream ~b2*needfreedom+traitneedfreedom+traitsecurity+b4*security


  needfreedom ~ a1*gr_2
  Micronarratives ~ gr_2
  Mainstream ~gr_2
  security ~a2*gr_2
  
  
  traitsecurity=~ tsec1+tsec2+tsec3
  security =~ ssec1+ssec2+ssec3
  ind1 := a1*b1
  ind2 := a1*b2
  ind3 := a2*b3
  ind4 := a2*b4


  

'

free.fit <- sem(free.model, data = forsem, estimator = "ML", missing = "FIML",
                se = "bootstrap",bootstrap = 500L, parallel ="snow")
summary(free.fit, fit.measures=T, standardized = T, rsquare=TRUE)
estimates_free.fit <- parameterEstimates(free.fit, standardized=TRUE, boot.ci.type="perc", level=0.95,
                                         zstat = FALSE, pvalue = FALSE, output = "data.frame")

View(estimates_free.fit)
estimates_free.fit[89:92, ]
pfad_layout<- get_layout("","security", "", "","Micronarratives",
                         "","", "","","",
                         "","","","","",
                         "gr_2","","","","Mainstream",
                         "","needfreedom","","","",
                         rows = 5)

tidySEM::graph_sem(model = free.fit, layout = pfad_layout) 

b<- aov(micronarratives~gr, data=mand)
summary(b)

describeBy(micronarratives~gr, data = mand)
