getwd()

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

##########DO NOT TOUCH################
########Datenbereinigung######

raw <- read.csv("data8.csv", sep = ";", na.strings = "-99")  #only the completes

raw1 <- read.csv("data3.csv", sep = ";", na.strings = "-99")  #only the completes

raw[c(1:189), ]$c_0001[raw[c(1:189), ]$c_0001==1 | raw[c(1:189), ]$c_0001==3] <- 1
raw[c(189:236), ]$c_0001 <- 1

raw1[c(519:711), ]$c_0001[raw1[c(519:711), ]$c_0001==1 | raw1[c(519:711), ]$c_0001==3] <- 1
raw1 <- raw1[c(13:711),]

raw1$c_0001

colnames(raw1) <- colnames(raw)    

raw <- rbind(raw1, raw)
?rbind
#935
clean <- raw
clean$country <- "DE"

table(clean$c_0001)
#eigentümer check 
which(clean$v_114 == 0) #check successful -- all own a house/apartment

#group sizes
table(clean$c_0001)
#   1   2   3 
#462 290 170 
#458 290 174 
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
#  1.767   7.867  10.917  13.098  14.708 119.400 

sd(mand$duration_m)
# 9.730499

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


# Load the library
library(fastDummies)


# Create dummy variable
onlymanipulation <- fastDummies::dummy_cols(onlymanipulation, 
                                            select_columns = "gr")
mand <- fastDummies::dummy_cols(mand, 
                                            select_columns = "gr")

#to make comparisons easier
onlymanipulation$gr_1 <- onlymanipulation$gr_2 # obligatory
onlymanipulation$gr_2 <- onlymanipulation$gr_1 #voluntary

onlymanipulation$gr_a[onlymanipulation$gr == 1] <- 2
onlymanipulation$gr_a[onlymanipulation$gr == 2] <- 1

onlymanipulation$gr_a <- as.factor(onlymanipulation$gr_a)


save(mand, file="mand.RData")

#done yeehaw

