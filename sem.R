
###SEM

forsem <- onlymanipulation

which(is.na(forsem$c_0001))

center_scale <- function(x) {
  scale(x, scale = T, center=T)
}

forsem_s <- center_scale(forsem[, c(6:57, 64:73)])

forsem_s <- data.frame(forsem_s, forsem[, c(1:5, 57:63, 74:77)])


#with dummy variable 


m.model <- '
  needsecurity =~ ssec1+ssec2+ssec3
  Mainstream=~narrative_2  +narrative_4  +narrative_6  +narrative_8  
  Micronarratives=~narrative_1+narrative_3+narrative_5+ narrative_7 
  traitneedsecurity=~ tsec1+tsec2+tsec3

  Micronarratives ~needsecurity+traitneedsecurity
  Mainstream ~needsecurity+traitneedsecurity


  needsecurity ~ gr_2
  Micronarratives ~ gr_2
  Mainstream ~gr_2
  

'
m.fit <- sem(m.model, data = forsem, estimator = "MLR", missing = "FIML")
summary(m.fit, fit.measures=T, standardized = T)


med.model <- '
  needsecurity =~ ssec1+ssec2+ssec3
  Mainstream=~narrative_2  +narrative_4  +narrative_6  +narrative_8  
  Micronarratives=~narrative_1+narrative_3+narrative_5+ narrative_7 
  traitneedsecurity=~ tsec1+tsec2+tsec3
  Micronarratives ~b1*needsecurity+traitneedsecurity
  Mainstream ~b2*needsecurity+traitneedsecurity


  needsecurity ~ a1*gr_1
  Micronarratives ~ gr_1
  Mainstream ~gr_1
  
ind1 := a1*b1
ind2 := a1*b2

'

med.fit <- sem(med.model, data = forsem_s, estimator = "ML", missing = "FIML",
               se = "bootstrap",bootstrap = 500L, parallel ="snow")
summary(med.fit, fit.measures=T, standardized = T, rsquare=TRUE)
estimates_med.fit <- parameterEstimates(med.fit, standardized=TRUE, boot.ci.type="perc", level=0.95,
                                        zstat = FALSE, pvalue = FALSE, output = "data.frame")

View(estimates_med.fit)
estimates_med.fit[61:62, ]



med1.model <- '
  needsecurity =~ ssec1+ssec2+ssec3
  Mainstream=~narrative_2  +narrative_4  +narrative_6  +narrative_8  
  Micronarratives=~narrative_1+narrative_3+narrative_5+ narrative_7 
  traitneedsecurity=~ tsec1+tsec2+tsec3
  needfreedom =~sfree1+sfree2+sfree3
  Micronarratives ~needfreedom +needsecurity
  Mainstream ~needfreedom+needsecurity

  needfreedom~needsecurity
  needsecurity ~ gr_1
    needfreedom ~ gr_1
  Micronarratives ~ gr_2
  Mainstream ~gr_2
  

'

med1.fit <- sem(med1.model, data = forsem, estimator = "ML")
summary(med1.fit, fit.measures=T, standardized = T, rsquare=TRUE)
estimates_med.fit <- parameterEstimates(med.fit, standardized=TRUE, boot.ci.type="perc", level=0.95,
                                        zstat = FALSE, pvalue = FALSE, output = "data.frame")


pfad_layout<- get_layout("needsecurity","", "", "","Micronarratives",
                         "","", "","","",
                         "","needfreedom","","","",
                         "","","","","Mainstream",
                         "gr_2","","","","",
                         rows = 5)

tidySEM::graph_sem(model = med1.fit, layout = pfad_layout) 

pfad_layout<- get_layout("","", "", "","Micronarratives",
                         "","needsecurity", "","","",
                         "","","","","",
                         "","","","","Mainstream",
                         "gr_1","","","","",
                         rows = 5)

tidySEM::graph_sem(model = med.fit, layout = pfad_layout) 

library(knitr)


table_fit <- matrix(NA, nrow = 5, ncol = 9)
colnames(table_fit) = c("Model", "X2", "df", "CFI", "RMSEA", "SRMR", "X2 Diff",
                        "Pr(>X2)", "AIC")
table_fit[1, ] <- c("Overall Model", round(fitmeasures(m.fit, 
                                                       c("chisq", "df", "cfi",
                                                         "rmsea", "srmr")),3),
                    NA, NA, round((a[1,2])))

table_fit[2, ] <- c("Constrain all ~ 0*gr", round(fitmeasures(m.fit_a, 
                                                              c("chisq", "df", "cfi",
                                                                "rmsea", "srmr")),3),
                    round((a[2,5]),3), round((a[2,7]),3), round((a[2,2])))
table_fit[3, ] <- c("Constrain ssec~0*gr", round(fitmeasures(m.fit_b, 
                                                             c("chisq", "df", "cfi",
                                                               "rmsea", "srmr")),3),
                    round((b[2,5]),3), round((b[2,7]),3),round((b[2,2])))
table_fit[4, ] <- c("Constrain Micro~0*gr", round(fitmeasures(m.fit_c, 
                                                              c("chisq", "df", "cfi",
                                                                "rmsea", "srmr")),3),
                    round((c[2,5]),3), round((c[2,7]),3),round((c[2,2])))
table_fit[5, ] <- c("Constrain mainstream~0*gr", round(fitmeasures(m.fit_d, 
                                                                   c("chisq", "df", "cfi",
                                                                     "rmsea", "srmr", "")),3), 
                    round((d[2,5]),3), round((d[2,7]),3), round((d[2,2])))


kable(table_fit)




##add trust 


t.model <- '
  needsecurity =~ ssec1+ssec2+ssec3
  Mainstream=~narrative_2  +narrative_4  +narrative_6  +narrative_8  
  Micronarratives=~narrative_1+narrative_3+narrative_5+ narrative_7 
  traitneedsecurity=~ tsec1+tsec2+tsec3
  
  Micronarratives ~needsecurity+traitneedsecurity+trust5+gr_2
  Mainstream ~needsecurity+traitneedsecurity+trust5+gr_2


  needsecurity ~ gr_2
  needsecurity ~ trust5
  traitneedsecurity ~ trust5
'
t.fit <- sem(t.model, data = forsem, estimator = "MLR", missing = "FIML",
             group.equal = c("loadings", "intercepts", "means", "residuals", "residual.covariances", "lv.variances", "lv.covariances"))
summary(t.fit, fit.measures=T, standardized = T)


tmed.model <- '
  needsecurity =~ ssec1+ssec2+ssec3
  Mainstream=~narrative_2  +narrative_4  +narrative_6  +narrative_8  
  Micronarratives=~narrative_1+narrative_3+narrative_5+ narrative_7 
  traitneedsecurity=~ tsec1+tsec2+tsec3

Micronarratives ~b1*needsecurity+traitneedsecurity+trust5+gr_2
  Mainstream ~b2*needsecurity+traitneedsecurity+trust5+gr_2


  needsecurity ~ gr_2
  needsecurity ~ a1*trust5
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







pfad_layout<- get_layout("","traitneedsecurity", "", "","Micronarratives",
                         "trust5","", "","","",
                         "","","","","",
                         "gr_2","","","","Mainstream",
                         "","needsecurity","","","",
                         rows = 5)

tidySEM::graph_sem(model = tmed.fit, layout = pfad_layout) 

##add active media use 


a.model <- '
     needsecurity =~ ssec1+ssec2+ssec3
  Mainstream=~narrative_2  +narrative_4  +narrative_6  +narrative_8  
  Micronarratives=~narrative_1+narrative_3+narrative_5+ narrative_7 
  traitneedsecurity=~ tsec1+tsec2+tsec3
  
  Micronarratives ~needsecurity+traitneedsecurity+med_act+trust5
  Mainstream ~needsecurity+traitneedsecurity+med_act+trust5
   med_act~needsecurity
  
  needsecurity ~ gr_2
  needsecurity ~ trust5
  

  needsecurity ~ gr_2
  needsecurity ~ trust5
  traitneedsecurity ~ trust5

'
a.fit <- sem(a.model, data = forsem, estimator = "MLR", missing = "FIML",
             group.equal = c("loadings", "intercepts", "means", "residuals", "residual.covariances", "lv.variances", "lv.covariances"))
summary(a.fit, fit.measures=T, standardized = T)

dmcforsem <- forsem

#double mean centering
dmcforsem <- indProd(dmcforsem, var1= c("ssec1", "ssec2", "ssec3"),
                     var2=c("med_act"),
                     match = FALSE , meanC = TRUE ,
                     residualC = FALSE , doubleMC = TRUE) 
amed.model <- '
needsecurity =~ ssec1+ssec2+ssec3
  Mainstream=~narrative_2  +narrative_4  +narrative_6  +narrative_8  
  Micronarratives=~narrative_1+narrative_3+narrative_5+ narrative_7 
  traitneedsecurity=~ tsec1+tsec2+tsec3
  int=~ ssec1.med_act +ssec2.med_act +ssec3.med_act 
  
  
  
  Micronarratives ~needsecurity+traitneedsecurity+trust5+med_act+int
  Mainstream ~needsecurity+traitneedsecurity+trust5+med_act+int
  med_act ~ needsecurity

  needsecurity ~ gr_2
  needsecurity ~ trust5
  traitneedsecurity ~ trust5
      

'

amed.fit <- sem(amed.model, data = dmcforsem, estimator = "MLM")
summary(amed.fit, fit.measures=T, standardized = T, rsquare=TRUE)
estimates_amed.fit <- parameterEstimates(amed.fit, standardized=TRUE, boot.ci.type="perc", level=0.95,
                                         zstat = FALSE, pvalue = FALSE, output = "data.frame")

View(estimates_amed.fit)

estimates_amed.fit[71:72, ]



pfad_layout<- get_layout("","", "med_act", "","Micronarratives",
                         "","", "","","",
                         "","int","","","",
                         "","","","","Mainstream",
                         "","needsecurity","","","",
                         rows = 5)

tidySEM::graph_sem(model = amed.fit, layout = pfad_layout) 





#freedom



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





freed.model <- '
  needfreedom =~ sfree1+sfree2+sfree3
  Mainstream=~narrative_2  +narrative_4  +narrative_6  +narrative_8  
  Micronarratives=~narrative_1+narrative_3+narrative_5+ narrative_7 
  traitneedfreedom=~  tfree1+ tfree2+ tfree3

  Micronarratives ~b1*needfreedom+traitneedfreedom
  Mainstream ~b2*needfreedom+traitneedfreedom


  needfreedom ~ a1*gr_2
  Micronarratives ~ gr_2
  Mainstream ~gr_2

  ind1 := a1*b1
  ind2 := a1*b2

  

'

freed.fit <- sem(freed.model, data = forsem, estimator = "ML", missing = "FIML",
                 se = "bootstrap",bootstrap = 500L, parallel ="snow")
summary(freed.fit, fit.measures=T, standardized = T, rsquare=TRUE)
estimates_freed.fit <- parameterEstimates(freed.fit, standardized=TRUE, boot.ci.type="perc", level=0.95,
                                          zstat = FALSE, pvalue = FALSE, output = "data.frame")

estimates_freed.fit[61:62, ]
pfad_layout<- get_layout("","traitneedfreedom", "", "","Micronarratives",
                         "","", "","","",
                         "","","","","",
                         "gr_2","","","","Mainstream",
                         "","needfreedom","","","",
                         rows = 5)

tidySEM::graph_sem(model = freed.fit, layout = pfad_layout) 




pfad_layout<- get_layout("","", "", "","Micronarratives",
                         "security","", "","","",
                         "","","","","",
                         "gr_2","","","","Mainstream",
                         "","needfreedom","","","",
                         rows = 5)

tidySEM::graph_sem(model = free.fit, layout = pfad_layout) 


anova(free.fit, freed.fit)

anova(free.fit, se.fit)


View(forsem)

##narratives
#perform EFA
narr <- subset(forsem[,c(27:32)])
library(RcmdrMisc)

rcorr.adjust(narr) # This function is build into R Commander.

## If you want to run this before eliminating missing values use: 
# rcorr.adjust(mydata, use="pairwise.complete.obs") 

write.csv(cor(narr)>0.8, file="Suspect_Correlations.csv")
write.csv(cor(narr), file="Correlation_Values.csv")

library(psych)

KMO(narr)
cortest.bartlett(narr)

ev <- eigen(cor(narr)) # get eigenvalues
ev$values

scree(narr, pc=FALSE)
fa.parallel(narr, fa="fa")

Nfacs <- 1 

fit <- factanal(narr, Nfacs, rotation="promax")


print(fit, digits=2, cutoff=0.3, sort=TRUE)

load <- fit$loadings[,1:2]
plot(load,type="n") # set up plot
text(load,labels=names(narr),cex=.7)







try.model <- '
  state =~ sfree1+sfree2+sfree3+ssec1+ssec2+ssec3
  Mainstream=~narrative_2  +narrative_4  +narrative_6  +narrative_8  
  Micronarratives=~narrative_1+narrative_3+narrative_5+ narrative_7 
  trait=~  tfree1+ tfree2+ tfree3+tsec1+tsec2+tsec3

  Micronarratives ~b1*state+trait
  Mainstream ~b2*state+trait


  state ~ a1*gr_2
  Micronarratives ~ gr_2
  Mainstream ~gr_2
  
  
  ind1 := a1*b1
  ind2 := a1*b2


'

try.fit <- sem(try.model, data = forsem, estimator = "ML", missing = "FIML",
               se = "bootstrap",bootstrap = 500L, parallel ="snow")
summary(try.fit, fit.measures=T, standardized = T, rsquare=TRUE)
estimates_free.fit <- parameterEstimates(free.fit, standardized=TRUE, boot.ci.type="perc", level=0.95,
                                         zstat = FALSE, pvalue = FALSE, output = "data.frame")



pfad_layout<- get_layout("","", "", "","Micronarratives",
                         "state","", "","","",
                         "","","","","",
                         "gr_2","","","","Mainstream",
                         "","trait","","","",
                         rows = 5)

tidySEM::graph_sem(model = try.fit, layout = pfad_layout) 





cfa <- '
  sec =~ ssec1+ssec2+ssec3
  free=~ sfree1+sfree2+sfree3
'

cfa <- sem(try.model, data = forsem, estimator = "ML", missing = "FIML",
           se = "bootstrap",bootstrap = 500L, parallel ="snow")
summary(try.fit, fit.measures=T, standardized = T, rsquare=TRUE)
estimates_free.fit <- parameterEstimates(free.fit, standardized=TRUE, boot.ci.type="perc", level=0.95,
                                         zstat = FALSE, pvalue = FALSE, output = "data.frame")



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
table_fit[4, ] <- c(" needsecurity ~ 0*gr_2", round(fitmeasures(t4.fit, 
                                                                c("chisq", "df", "cfi",
                                                                  "rmsea", "srmr", "")),3), 
                    round((t4[2,5]),3), round((t4[2,7]),3), round((t4[2,2])))
table_fit[5, ] <- c(" needsecurity ~ 0*trust5", round(fitmeasures(t5.fit, 
                                                                  c("chisq", "df", "cfi",
                                                                    "rmsea", "srmr", "")),3), 
                    round((t5[2,5]),3), round((t5[2,7]),3), round((t5[2,2])))
table_fit[6, ] <- c("traitneedsecurity ~ 0*trust5", round(fitmeasures(t6.fit, 
                                                                      c("chisq", "df", "cfi",
                                                                        "rmsea", "srmr", "")),3), 
                    round((t6[2,5]),3), round((t6[2,7]),3), round((t6[2,2])))


kable(table_fit)



