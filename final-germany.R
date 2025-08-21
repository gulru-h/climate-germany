getwd()
renv::init()
renv::snapshot()

usethis::create_github_token(description = "climate-germany")
gitcreds::gitcreds_set() 
usethis::use_github()
usethis::gh_token_help()

#"X:/LS-KESSELS/ALLGEMEIN/Gülru/digipatch/klima/klima"
library(careless)
library(ggplot2)
library(car)
library(psych)
library(dplyr)
library(gtsummary)
library(lavaan)
library(tidySEM)
library(haven)
library(semTools)
library(corrplot)
library(lavaan)
library(ggplot2)
library(ggeffects)

##########DO NOT TOUCH################
########Datenbereinigung######

#raw <- read.csv("data8.csv", sep = ";", na.strings = "-99")  #only the completes

#raw1 <- read.csv("data3.csv", sep = ";", na.strings = "-99")  #only the completes

#raw[c(1:189), ]$c_0001[raw[c(1:189), ]$c_0001==1 | raw[c(1:189), ]$c_0001==3] <- 1
#raw[c(189:236), ]$c_0001 <- 1

#raw1[c(519:711), ]$c_0001[raw1[c(519:711), ]$c_0001==1 | raw1[c(519:711), ]$c_0001==3] <- 1
#raw1 <- raw1[c(13:711),]

#raw1$c_0001

#colnames(raw1) <- colnames(raw)    

#raw <- rbind(raw1, raw)
#935
#write.csv(raw, "raw.csv")

raw2 <- read.csv("raw.csv")
raw <- raw2[, 2:146]

clean <- raw
clean$country <- "DE"

table(clean$c_0001)
#eigentümer check 
which(clean$v_114 == 0) #check successful -- all own a house/apartment

#group sizes
table(clean$c_0001)
#   1   2   3 
#470 295 170 

#number of completes
nrow(clean)
#922 complete cases
#935

#remove empty columns
clean <- clean[,-c(18:29)]


#combine variables

#finance manipulation
sub <- subset(clean[,c(40:41)])
clean$man_finance <- apply(sub, 1, sum, na.rm=T)

#efficacy
sub <- subset(clean[,c(42:43)])
clean$man_eff <- apply(sub, 1, sum, na.rm=T)

#mancheck1
sub <- subset(clean[,c(54, 56)])
clean$man_check1 <- apply(sub, 1, sum, na.rm=T)

#mancheck2
sub <- subset(clean[,c(55, 57)])
clean$man_check2 <- apply(sub, 1, sum, na.rm=T)



#remove more irrelevant columns 
clean <- clean[,-c(74:133)]
clean <- clean[,-c(41,43,56,57)]
clean <- clean[,-c(1:6)]
table(clean$c_0001)

#checking ranges

#age
range(clean$age)  #30 to 88
which(clean$age == 722)  #correct age
clean[266,]$age <- 72
mean(clean$age, na.rm = T)


#gender
table(clean$gender)
#169 women 336 men

#ses
range(clean$SES, na.rm = T)
hist(clean$SES)
#normal-ish

#pol
hist(clean$pol)
table(clean$pol)
#1CDU/CSU
#2SPD
#3Bündnis 90/Die Grünen
#4FDP
#5Die Linke
#6Bündnis Sahra Wagenknecht (BSW)
#7AfD
#8Andere Partei
#9Ich würde nicht wählen
#10Ich möchte dazu keine Angabe machen"


####build means####

finan <- subset(clean[,c(13:15)])
clean$finan <- apply(finan, 1, mean, na.rm=T)

tsec <- subset(clean[,c(21:23)])
clean$tsec <- apply(tsec, 1, mean, na.rm=T)

tfree <- subset(clean[,c(24:26)])
clean$tfree <- apply(tfree, 1, mean, na.rm=T)

ssec <- subset(clean[,c(27:29)])
clean$ssec <- apply(ssec, 1, mean, na.rm=T)

sfree <- subset(clean[,c(30:32)])
clean$sfree <- apply(sfree, 1, mean, na.rm=T)

man_acc <- subset(clean[,c(36,37)])
clean$man_acc <- apply(man_acc, 1, mean, na.rm=T)

#save manipulation indicator as a factor
class(clean$c_0001)
clean$c_0001 <- as.factor(clean$c_0001)


#attention check
clean$v_113[clean$att==2 & clean$c_0001==1] <- 1
clean$v_113[clean$att==1 & clean$c_0001==2] <- 2
clean$v_113[clean$att==3]<- 3

clean1 <- subset(clean, clean$v_113 == 0)


table(clean$v_113)

table(clean1$c_0001)
#1   2   3 
#223 233 169 

#check if manipulation worked
which(clean1[clean1$c_0001 == 1:2, ]$man_check1> 1)

table(clean1[clean1$c_0001 !=3, ]$man_check1 > 1)
table(clean1[clean1$c_0001 !=3, ]$man_check2 > 1)
table(clean1[clean1$c_0001 !=3, ]$man_check1 > 1 & clean1[clean1$c_0001 !=3, ]$man_check2 > 1)


clean1[clean1$c_0001 ==3, ]$man_check1> 1

table(clean1$man_check1==1 & clean1$c_0001 != 3) 
table(clean1$man_check2==1 & clean1$c_0001 != 3) 

cond1 = clean1$c_0001!=3
cond2 = clean1$man_check1 > 1
cond3 = clean1$man_check2 > 1

cond = (cond1 & cond2 & cond3) | !cond1
mand <- clean1[cond,]

table(mand[mand$c_0001 !=3, ]$man_check2 > 1)

table(mand$c_0001)
#  1   2   3 
#188 167 169 

###FROM HERE ON OUT USE mand FOR CALCULATIONS #####

#detecting careless answers 

#checking duration
mand$duration_m <- mand$duration/60
summary(mand$duration_m)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.767   7.867  10.900  13.092  14.704 119.400 

sd(mand$duration_m)
# 9.72077

#due to the age of the participants we decided to only remove the too quick 
#answers 
#sd and mean without outliers

fast <- mand[mand$duration_m <45, ]

summary(fast$duration_m)
sd(fast$duration_m)

low <- mean(fast$duration_m)-sd(fast$duration_m)

which(mand$duration_m <   5.593478)
mand <- mand[mand$duration_m >  5.593478, ]
# n = 480

summary(mand$duration_m)
ggplot(mand, aes(y = duration_m)) + geom_boxplot()


#creating a manipulation only group 

table(mand$c_0001)
#  1   2   3 
#173 156 151 
##########DO NOT TOUCH#######


#check if anyone in the sample already had exclusively climate friendly heaters 
table(mand[ , mand$heiz1==1 & mand[, c(40:45)] == 0]$heiz1)
mand$heiz1[mand$heiz1==2 & mand[, c(40:44)] == 0]

#make the grouping variable ordered
mand$c_0001 <- ordered(mand$c_0001, levels = c("1", "2", "3"))

##narratives
#perform EFA
narr <- subset(mand[,c(38:45)])
library(RcmdrMisc)

rcorr.adjust(narr) 

write.csv(cor(narr)>0.8, file="Suspect_Correlations.csv")
write.csv(cor(narr), file="Correlation_Values.csv")

library(psych)

KMO(narr)
cortest.bartlett(narr)

ev <- eigen(cor(narr)) # get eigenvalues
ev$values

scree(narr, pc=FALSE)
fa.parallel(narr, fa="fa")

Nfacs <- 2  

fit <- factanal(narr, Nfacs, rotation="promax")


print(fit, digits=2, cutoff=0.3, sort=TRUE)

load <- fit$loadings[,1:2]
plot(load,type="n") # set up plot
text(load,labels=names(narr),cex=.7)

#subsetting and building means 

micronarratives <- subset(mand[,c(38,40,42,44)])
mand$micronarratives <- apply(micronarratives, 1, mean, na.rm=T)

mainstream <- subset(mand[,c(39,41,43,45)])
mand$mainstream <- apply(mainstream, 1, mean, na.rm=T)


#renaming things
mand<- rename(mand, gr = c_0001)
#create a manipulation only group without the control group
rm(onlymanipulation)
onlymanipulation <- mand[mand$gr ==1 |mand$gr ==2 , ]

#create the two levels 
onlymanipulation$gr <- ordered(onlymanipulation$gr, levels = c("1", "2"))


library(fastDummies)
# Create dummy variable
onlymanipulation <- fastDummies::dummy_cols(onlymanipulation, 
                                            select_columns = "gr")
mand <- fastDummies::dummy_cols(mand, select_columns = "gr")

#to make comparisons easier with Poland
onlymanipulation$gr_1 <- onlymanipulation$gr_2 # obligatory: group 1 is mandatory
#onlymanipulation$gr_2 <- onlymanipulation$gr_1 #voluntary: group 2 is voluntary

onlymanipulation$gr_a[onlymanipulation$gr == 1] <- 2
onlymanipulation$gr_a[onlymanipulation$gr == 2] <- 1

onlymanipulation$gr_a <- as.factor(onlymanipulation$gr_a)


save(mand, file="mand.RData")

#done with cleaning plus data wrangling 


####analyses
##anova
#1- freiwillig
#2- verpflichtend
#3 - control

a <- aov(ssec ~ gr+tsec+finan, data=mand)
summary(a)
aa <- aov(ssec ~ gr, data=mand)
summary(aa)
eta_squared(a)
library(effectsize)
eta_squared(aa)
TukeyHSD(aa)
cohen.d(ssec ~ gr, data=mand)
#only manipulation

b <- aov(ssec ~ gr+tsec+finan, data=onlymanipulation)
summary(b)
bb <- aov(ssec ~ gr, data=onlymanipulation)
TukeyHSD(bb)
eta_squared(b)
cohens_d(ssec ~ gr, data=onlymanipulation) #-.13

#comparisons with control are significant

c <- aov(sfree ~ gr + tfree +finan , data=mand)
summary(c)
eta_squared(c)
TukeyHSD(c)
#only manipulation
d <- aov(sfree ~ gr_a, data=onlymanipulation)
summary(d)
TukeyHSD(d)
dfree<- cohen.d(sfree ~ gr_a, data=onlymanipulation)
summary(dfree)

#make the grouping variable ordered
mand$c_0001 <- ordered(mand$c_0001, levels = c("1", "2", "3"))


#anova + pairwise comparisons
mainst <- aov(mainstream ~ gr, data=mand)
summary(mainst)
TukeyHSD(mainst)

micron <- aov(micronarratives ~ gr, data=mand)
summary(micron)
TukeyHSD(micron)


#(exploratory) H3:The strength of the relationship between trust in 
#institutions and perceived threat to security will vary by experimental group.

tma<- lm(mainstream~trust5, data=onlymanipulation)
summary(tma)
standardise(tma)

tmi<- lm(micronarratives~trust5, data=onlymanipulation)
summary(tmi)
standardise(tmi)

#will be added to the SEM


#(exploratory) H4: Active media use will moderate the relationship between 
#perceived threat to security and the choice of micro and mainstream narratives.

m3 <- lm(micronarratives~ ssec*med_act+tsec+finan, data=onlymanipulation)
summary(m3)
standardise(m3)
ggpredict(m3, terms = c("ssec[1:7 by=0.1]", "med_act[1:6 by=2]")) |> plot()

m3.1 <- lm(mainstream~ ssec*med_act+tsec+finan, data=onlymanipulation)
summary(m3.1)
ggpredict(m3.1, terms = c("ssec[1:7 by=0.1]", "med_act[1:6 by=2]")) |> plot()

m4 <- lm(micronarratives~ sfree*med_act+tfree+finan, data=onlymanipulation)
summary(m4)
standardise(m4)
ggpredict(m3, terms = c("ssec[1:7 by=0.1]", "med_act[1:6 by=2]")) |> plot()


m4.1 <- lm(mainstream~ sfree*med_act+tfree+finan, data=onlymanipulation)
summary(m4.1)
standardise(m4.1)
ggpredict(m3.1, terms = c("ssec[1:7 by=0.1]", "med_act[1:6 by=2]")) |> plot()

#no active media use effects 

#H5: The perceived effectiveness of the proposed policy will not differ between 
#the mandatory and the voluntary conditions.
a <- aov(man_eff~gr, data=onlymanipulation)
eta_squared(a)
summary(a)
TukeyHSD(a)
#mandatory policy seen as more effective

#H6: Participants in the mandatory condition will perceive the policy to be less 
#acceptable in comparison to those in the voluntary and control conditions.

b <- aov(man_acc~gr, data=onlymanipulation)
summary(b)
TukeyHSD(b)
eta_squared(b)

#both similar


###SEM Time ####
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


#without the needs (base model)

n.model <- '
  Mainstream=~narrative_2  +narrative_4  +narrative_6  +narrative_8  
  Micronarratives=~narrative_1+narrative_3+narrative_5+ narrative_7 

  Micronarratives ~ gr_1
  Mainstream ~gr_1
  Mainstream ~~ Micronarratives

'

n.fit <- sem(n.model, data = forsem, estimator = "ML")
summary(n.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci= T)


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
  finance =~finan1+finan2+finan3

  
  Micronarratives ~stateneedsecurity+traitneedsecurity+trust5+med_act+interaction+gr_1+finance
  Mainstream ~stateneedsecurity+traitneedsecurity+trust5+med_act+interaction+gr_1+finance
  med_act ~ traitneedsecurity+finance

  stateneedsecurity ~ gr_1+traitneedsecurity+med_act+trust5+finance
  trust5~traitneedsecurity+finance
      
  Micronarratives~~Mainstream
'

amed.fit <- sem(amed.model, data = dmcforsem, estimator = "MLM")
summary(amed.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci =T)


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
    
  Micronarratives~~Mainstream

  ind1 := a1*b1
  ind2 := a1*b2


'


freed.fit <- sem(freed.model, data = forsem, estimator = "ML", missing = "FIML",
                 se = "bootstrap",bootstrap = 5000L, 
                 parallel ="multicore", verbose= T)
summary(freed.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)


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
  finance =~finan1+finan2+finan3

  Micronarratives ~stateneedfreedom+traitneedfreedom+trust5+med_act+interaction+gr_1+finance
  Mainstream ~stateneedfreedom+traitneedfreedom+trust5+med_act+interaction+gr_1+finance
  med_act ~ traitneedfreedom+finance

  stateneedfreedom ~ gr_1+traitneedfreedom+med_act+trust5+finance
  trust5~ traitneedfreedom +finance 
  
  Micronarratives~~Mainstream
  
'

framed.fit <- sem(framed.model, data = fdmcforsem, estimator = "MLM")
summary(framed.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)



#you should now have 3 SEMs each for need for security and need for freedom 



#with different groups (experimental vs. control)

med.model <- '
  stateneedsecurity =~ ssec1+ssec2+ssec3
  Mainstream=~narrative_2+narrative_4+narrative_6+narrative_8  
  Micronarratives=~narrative_1+narrative_3+narrative_5+narrative_7 
  traitneedsecurity=~ tsec1+tsec2+tsec3
  finance =~finan1+finan2+finan3
  
  Micronarratives ~b1*stateneedsecurity+traitneedsecurity+gr_3+finance
  Mainstream ~b2*stateneedsecurity+traitneedsecurity+gr_3+finance
  stateneedsecurity ~ a1*gr_3+traitneedsecurity+finance
  
  Mainstream~~Micronarratives

  ind1 := a1*b1
  ind2 := a1*b2

'

med.fit <- sem(med.model, data = mand, estimator = "ML", missing = "FIML",
               se = "bootstrap",bootstrap = 5000L, 
               parallel ="multicore", verbose= T)
summary(med.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)









