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

table(clean$c_0001)
#eigentümer check 
which(clean$v_114 == 0) #check successful -- all own a house/apartment

#group sizes
h <- table(clean$c_0001)
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
hist(clean$age, freq=T, plot = T)

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
#219 229 173
#223 233 169 

#check if manipulation worked
which(clean1[clean1$c_0001 == 1:2, ]$man_check1> 1)

table(clean1[clean1$c_0001 !=3, ]$man_check1 > 1)
table(clean1[clean1$c_0001 !=3, ]$man_check2 > 1)
table(clean1[clean1$c_0001 !=3, ]$man_check1 > 1 & clean1[clean1$c_0001 !=3, ]$man_check2 > 1)


clean1[clean1$c_0001 ==3, ]$man_check1> 1

table(clean1$man_check1==1 & clean1$c_0001 != 3) 
table(clean1$man_check2==1 & clean1$c_0001 != 3) 
rm(mand)

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

###descriptives 

mand$pol <- as.factor(mand$pol)
mand[,16:20] <- lapply(mand[,16:20],as.numeric)
?as.numeric


##descriptives

table1 <-
  mand |> 
  gtsummary::tbl_summary(include = c(age, gender, SES),
                         by= NULL,
                         statistic = list(gtsummary::all_continuous() ~ "{mean} ({p25}, {p75})",
                                          gtsummary::all_categorical() ~"{n} ({p}%)"),)

table1 <-
  mand |> 
  gtsummary::tbl_summary(include = c(pol),
                         by= NULL,
                         statistic = list(gtsummary::all_continuous() ~ "{median} ({p25}, {p75})",
                                          gtsummary::all_categorical() ~"{n} ({p}%)"),)



#check if anyone in the sample already had exclusively climate friendly heaters 
table(mand[ , mand$heiz1==1 & mand[, c(40:45)] == 0]$heiz1)


mand$heiz1[mand$heiz1==2 & mand[, c(40:44)] == 0]

##correlations
#method1
cors <- subset(mand[,c(9:12, 34, 61, 64:69, 71:72, 16:20 )])
cors1 <- cors[mand$c_0001==1, ]
cors2 <- cors[mand$c_0001==2, ]
cors3 <- cors[mand$c_0001==3, ]


center_scale <- function(x) {
  scale(x, scale = FALSE)
}

# apply it
center_scale(cors)

M <- cor(cors)
M1<- cor(cors1)
M2<- cor(cors2)
M3<- cor(cors3)
corrplot(M1, method="number")
corrplot(M2, method="number")
corrplot(M3, method="number")
#-0.306434250

?corrplot

colnames(M) <- c("Left Wing Media", "Right Wing Media",
                 "Social Media", "Active media use", "Financial Burden",
                 "SES", "Financial Situation",
                 "(T)Need for Security", "(T)Need for Freedom", 
                 "(S)Need for Security", "(S)Need for Freedom",
                 "Acceptability","micronarratives",
                 "mainstream", "Science", "Popular media", "Others on Social Media",
                 "EU", "Germany")
rownames(M) <- c("Left Wing Media", "Right Wing Media",
                 "Social Media", "Active media use", "Financial Burden",
                 "SES", "Financial Situation",
                 "(T)Need for Security", "(T)Need for Freedom", 
                 "(S)Need for Security", "(S)Need for Freedom",
                 "Acceptability","micronarratives",
                 "mainstream", "Science", "Popular media", "Others on Social Media",
                 "EU", "Germany")



PLO<- corrplot(M, method="number", )

#method 2

df.corr <- psych::corr.test(cors, adjust = "none")

inter_corr_r <- df.corr$stars
inter_corr_r <- cbind(var = rownames(inter_corr_r), inter_corr_r)
inter_corr_r <- as.data.frame(inter_corr_r)

ft.corr <- flextable(inter_corr_r) |> 
  set_header_labels(var = "")
ft.corr


#method3



# centering with 'scale()'
center_scale <- function(x) {
  scale(x, center= T, scale = T)
}

# apply it
cors_s <- center_scale(cors)


my_fn <- function(data, mapping, ...){
  p <- ggplot(data = cors_s, mapping = mapping) + 
    geom_point(size= .1) + 
    theme(strip.text.x = element_text(size = .1),
          strip.text.y = element_text(size = .1)) +
    geom_smooth(method=lm, fill="blue", color="blue", ...)
  p
}

my_fn <- function(data, mapping, ...){
  p <- ggplot(data = cors_s, mapping = mapping) + 
    geom_point(size=.1) + 
    geom_smooth(method=lm, fill="blue", color="blue", ...)
  p
}
library(GGally)
g = ggpairs(cors_s, lower = list(continuous = my_fn),
            upper = list(continuous = wrap(ggally_cor, alignPercent = 0.8, digits=2)),
            columnLabels = c("Left Wing Media", "Right Wing Media",
                             "Social Media", "Active media use", "Financial Burden",
                             "SES", "Financial Situation",
                             "(T)Need for Security", "(T)Need for Freedom", 
                             "(S)Need for Security", "(S)Need for Freedom",
                             "Acceptability","micronarratives",
                             "mainstream", "Science", "Popular media", "Others on Social Media",
                             "EU", "Germany"
                             
            )) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g




####analyses
##anova
#1- freiwillig
#2- verpflichtend
#3 - control


#3 group
groupcom <- aov(ssec ~ c_0001, data=forsem_s)
summary(groupcom)
TukeyHSD(groupcom)

#only manipulation
groupcom_man <- aov(ssec ~ c_0001+tsec, data=onlymanipulation)
summary(groupcom_man)
TukeyHSD(groupcom_man)

#comparisons with control are significant


#3 group
groupcom_free <- aov(sfree ~ c_0001, data=mand)
summary(groupcom_free)
TukeyHSD(groupcom_free)

class()

#only manipulation
groupcom_manfree <- aov(sfree ~ c_0001, data=onlymanipulation)
summary(groupcom_manfree)
TukeyHSD(groupcom_manfree)

##############pairwise comparisons
gtsummary::theme_gtsummary_mean_sd()

# function to add pairwise copmarisons to `tbl_summary()`
add_stat_pairwise <- function(data, variable, by, ...) {
  # calculate pairwise p-values
  pw <- pairwise.t.test(data[[variable]], data[[by]], p.adj = "none")
  
  # convert p-values to list
  index <- 0L
  p.value.list <- list()
  for (i in seq_len(nrow(pw$p.value))) {
    for (j in seq_len(nrow(pw$p.value))) {
      index <- index + 1L
      
      p.value.list[[index]] <- 
        c(pw$p.value[i, j]) %>%
        setNames(glue::glue("**{colnames(pw$p.value)[j]} vs. {rownames(pw$p.value)[i]}**"))
    }
  }
  
  # convert list to data frame
  p.value.list %>% 
    unlist() %>%
    purrr::discard(is.na) %>%
    t() %>%
    as.data.frame() %>%
    # formatting/roundign p-values
    dplyr::mutate(dplyr::across(everything(), gtsummary::style_pvalue))
}

mand %>%
  select(ssec, c_0001, tsec) %>%
  gtsummary::tbl_summary(by = c_0001, missing = "no") %>%
  # add pariwaise p-values
  gtsummary::add_stat(everything() ~ add_stat_pairwise) %>%
  print()

?as_kable()


#anova results 
tbl <- 
  mand %>%
  select(ssec, c_0001, tsec) %>%
  gtsummary::tbl_summary(
    by = c_0001, 
    missing = "no"
  ) %>%
  gtsummary::add_p(gtsummary::all_continuous() ~ "oneway.test") %>%
  # add a header (which also unhides a hidden column)
  gtsummary::modify_header(statistic ~ "**Test Statistic**") %>%
  # add a function to format the column
  gtsummary::modify_fmt_fun(statistic ~ gtsummary::style_sigfig)
tbl |>
  gtsummary::modify_header(label = "**Variable**", p.value = "**P**") 


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

#anova + pairwise comparisons
mainst <- aov(mainstream ~ c_0001, data=mand)
summary(mainst)
TukeyHSD(mainst)

micron <- aov(micronarratives ~ c_0001, data=mand)
summary(micron)
TukeyHSD(micron)

#create a manipulation only group without the control group
rm(onlymanipulation)
onlymanipulation <- mand[mand$c_0001 ==1 |mand$c_0001 ==2 , ]

onlymanipulation$man_eff<- as.numeric(onlymanipulation$man_eff)
tbl <- 
  onlymanipulation %>%
  gtsummary::select(man_eff, c_0001) %>%
  gtsummary::tbl_summary(
    by = c_0001, 
    missing = "no"
  ) %>%
  gtsummary::add_p(gtsummary::all_continuous() ~ "oneway.test") %>%
  # add a header (which also unhides a hidden column)
  gtsummary::modify_header(statistic ~ "**Test Statistic**") %>%
  # add a function to format the column
  gtsummary::modify_fmt_fun(statistic ~ gtsummary::style_sigfig)
tbl |>
  gtsummary::modify_header(label = "**Variable**", p.value = "**P**") 

class(mand$c_0001)
mand$c_0001

#create the two levels 
onlymanipulation$c_0001 <- ordered(onlymanipulation$c_0001, levels = c("1", "2"))


##############pairwise comparisons

gtsummary::theme_gtsummary_mean_sd()

# function to add pairwise comparisons to `tbl_summary()`
add_stat_pairwise <- function(data, variable, by, ...) {
  # calculate pairwise p-values
  pw <- pairwise.t.test(data[[variable]], data[[by]], p.adj = "none")
  
  # convert p-values to list
  index <- 0L
  p.value.list <- list()
  for (i in seq_len(nrow(pw$p.value))) {
    for (j in seq_len(nrow(pw$p.value))) {
      index <- index + 1L
      
      p.value.list[[index]] <- 
        c(pw$p.value[i, j]) %>%
        setNames(glue::glue("**{colnames(pw$p.value)[j]} vs. {rownames(pw$p.value)[i]}**"))
    }
  }
  
  # convert list to data frame
  p.value.list %>% 
    unlist() %>%
    purrr::discard(is.na) %>%
    t() %>%
    as.data.frame() %>%
    # formatting/roundign p-values
    dplyr::mutate(dplyr::across(everything(), gtsummary::style_pvalue))
}

onlymanipulation %>%
  select(micronarratives, c_0001) %>%
  gtsummary::tbl_summary(by = c_0001, missing = "no") %>%
  # add pariwaise p-values
  gtsummary::add_stat(everything() ~ add_stat_pairwise) %>%
  print()

?as_kable()
??ddply

tbl <- 
  onlymanipulation %>%
  select(mainstream, c_0001) %>%
  gtsummary::tbl_summary(
    by = c_0001, 
    missing = "no"
  ) %>%
  gtsummary::add_p(gtsummary::all_continuous() ~ "oneway.test") %>%
  # add a header (which also unhides a hidden column)
  gtsummary::modify_header(statistic ~ "**Test Statistic**") %>%
  # add a function to format the column
  gtsummary::modify_fmt_fun(statistic ~ gtsummary::style_sigfig)
tbl |>
  gtsummary::modify_header(label = "**Variable**", p.value = "**P**") 


#trajectory of need for security values
list(mand[mand$c_0001==3,]$ssec)

library(plyr)
legend_title <- "Group"
cdat <- ddply(mand, "c_0001", summarise, rating.mean=mean(ssec))
cdat
ggplot(mand, aes(x=ssec, color=c_0001)) + geom_density()+
  labs(color = "Group")+
  geom_vline(data=cdat, aes(xintercept=rating.mean,  colour=c_0001),linetype="dashed", linewidth=1)+
  xlab("Need for Security (State)")+
  scale_color_manual(labels = c("voluntary", "mandatory", "control"), values = c("blue", "red","lightgreen")) 




#H5: The perceived effectiveness of the proposed policy will not differ between 
#the mandatory and the voluntary conditions.
a <- aov(man_eff~c_0001, data=onlymanipulation)
summary(a)
TukeyHSD(a)
#mandatory policy seen as more effective

library(plyr)
legend_title <- "Group"
cdat <- ddply(onlymanipulation, "c_0001", summarise, rating.mean=mean(man_eff))
cdat
ggplot(onlymanipulation, aes(x=man_eff, color=c_0001)) + geom_density()+
  labs(color = "Group")+
  geom_vline(data=cdat, aes(xintercept=rating.mean,  colour=c_0001),linetype="dashed", linewidth=1)+
  xlab("Need for Security (State)")+
  scale_color_manual(labels = c("voluntary", "mandatory", "control"), values = c( "red","lightgreen")) 

with(onlymanipulation, aggregate(man_acc ~ c_0001, FUN = mean))

#H6: Participants in the mandatory condition will perceive the policy to be less 
#acceptable in comparison to those in the voluntary and control conditions.

b <- aov(man_acc~c_0001, data=onlymanipulation)
summary(b)
TukeyHSD(b)


library(plyr)
legend_title <- "Group"
cdat <- ddply(onlymanipulation, "c_0001", summarise, rating.mean=mean(man_acc))
cdat
ggplot(onlymanipulation, aes(x=man_acc, color=c_0001)) + geom_density()+
  labs(color = "Group")+
  geom_vline(data=cdat, aes(xintercept=rating.mean,  colour=c_0001),linetype="dashed", linewidth=1)+
  xlab("Need for Security (State)")+
  scale_color_manual(labels = c("voluntary", "mandatory", "control"), values = c( "red","lightgreen")) 




#(exploratory) H3:The strength of the relationship between trust in 
#institutions and perceived threat to security will vary by experimental group.
install.packages("fastDummies")

# Load the library
library(fastDummies)


# Create dummy variable
onlymanipulation <- fastDummies::dummy_cols(onlymanipulation, 
                                            select_columns = "c_0001")

#to make comparisons easier
onlymanipulation$gr_1 <- onlymanipulation$c_0001_2 # obligatory
onlymanipulation$gr_2 <- onlymanipulation$c_0001_1 #voluntary

mand <- dummy_cols(mand, select_columns = "c_0001")


#1- Science
#2 - Public media
#3 - Ppl on social media
#4 -EU
#5 - Germany
?dummy_cols

library(ggeffects)

#two group
m2 <- lm(ssec~ gr_1*trust4+tsec, data=forsem_s)
summary(m2)
ggpredict(m2, terms = c("trust4[1:7 by=0.1]", "gr_1")) |> plot()

#non-sig
m2.1 <- lm(ssec~ gr_1*trust5, data=forsem_s)
summary(m2.1)

ggpredict(m2.1, terms = c("trust5[1:7 by=0.1]", "gr_1")) |> plot()

m3 <- lm(sfree~ gr_1*trust5, data=forsem_s)
summary(m3)

ggpredict(m3, terms = c("trust5[1:7 by=0.1]", "gr_1")) |> plot()





#(exploratory) H4: Active media use will moderate the relationship between 
#perceived threat to security and the choice of micro and mainstream narratives.

m3 <- lm(micronarratives~ ssec*med_act+tsec, data=onlymanipulation)
summary(m3)
ggpredict(m3, terms = c("ssec[1:7 by=0.1]", "med_act[1:6 by=2]")) |> plot()
#non-sig

m3.1 <- lm(mainstream~ ssec*med_act+tsec, data=onlymanipulation)
summary(m3.1)
ggpredict(m3.1, terms = c("ssec[1:7 by=0.1]", "med_act[1:6 by=2]")) |> plot()

#non-sig

#direct
path_a <- lm(mainstream ~ gr_1, data=onlymanipulation)
summary(path_a)

path_b_c <- lm(mainstream ~ gr_1+ssec, data=onlymanipulation)
summary(path_b_c)

library(mediation)

results <- mediate(path_a, path_b_c, 
                   treat = "gr_1", mediator = "ssec", 
                   boot = TRUE)
summary(results)


#direct
path_a1 <- lm(micronarratives ~ ssec, data=onlymanipulation)
summary(path_a1)

path_b1_c1 <- lm(micronarratives ~ ssec*trust5, data=onlymanipulation)
summary(path_b1_c1)

library(mediation)

results <- mediate(path_a1, path_b1_c1, 
                   treat = "trust5", mediator = "ssec", 
                   boot = TRUE)
summary(results)







#direct
path_a1 <- lm(ssec ~ gr_1, data=onlymanipulation)
summary(path_a1)

path_b1_c1 <- lm(ssec ~ gr_1+trust5, data=onlymanipulation)
summary(path_b1_c1)

library(mediation)

results <- mediate(path_a1, path_b1_c1, 
                   treat = "trust5", mediator = "ssec", 
                   boot = TRUE)
summary(results)



path_a1 <- lm(micronarratives ~ ssec, data=onlymanipulation)
summary(path_a1)

path_b1_c1 <- lm(micronarratives ~ ssec*trust5, data=onlymanipulation)
summary(path_b1_c1)

library(mediation)

results <- mediate(path_a1, path_b1_c1, 
                   treat = "trust5", mediator = "ssec", 
                   boot = TRUE)
summary(results)






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





cor(mand$tsec, mand$finan) #-0.4135803
cor(mand$tsec, mand$meduse_3) #-0.1092648
cor(mand$tsec, mand$medtime_3) #-0.1092648
all <- mand[, c(6:20,34,35, 61, 63:72, 76)]
c_all<- cor(all)
library(corrplot)
corrplot(c_all, method="number")

#for wp1
wp1 <- lm(tsec~finan+trust4, data=mand )
summary(wp1)


mand$pol_r <- dplyr::recode(mand$pol, `1` = 6, `2` = 4, `3` = 2, `4`=5,
                            `5` = 1, `6` = 3, `7` = 7, `8` = -99, `9` = -99, `10` = -99)

mand$pol_r[mand$pol_r == "-99"] <- NA 

cor(mand$pol_r, mand$tsec, use = "pairwise.complete.obs")
0.1400118



library(GGally)


my_fn <- function(data, mapping, ...){
  p <- ggplot(data = cors_s, mapping = mapping) + 
    geom_point(size= .1) + 
    theme(strip.text.x = element_text(size = .2),
          strip.text.y = element_text(size = .2)) +
    geom_smooth(method=lm, fill="blue", color="blue", ...)
  p
}

my_fn <- function(data, mapping, ...){
  p <- ggplot(data = all, mapping = mapping) + 
    geom_point(size=.1) + 
    geom_smooth(method=lm, fill="blue", color="blue", ...)
  p
}

g = ggpairs(all, lower = list(continuous = my_fn),
            upper = list(continuous = wrap(ggally_cor, alignPercent = 0.8, digits=2))) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g



cor_5 <- rcorr(as.matrix(all))
M <- cor_5$r
p_mat <- cor_5$P
corrplot(M, type = "upper", order = "hclust", 
         p.mat = p_mat, sig.level = 0.01, method = "number")

corrplot(c_all, method = "color", col = NULL,  
         type = "upper", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "darkblue", tl.srt = 45, #Text label color and rotation
         # Combine with significance level
         sig.level = 0.05,  
         # hide correlation coefficient on the principal diagonal
         diag = FALSE 
)
?corrplot()

#####ignore for sanity#####













est_IE_means <- parameterEstimates(m.fit1)[334:338, "est"]

VCOV_Ex2FittedMeasEquivModel <- vcov(m.fit1)
?vcov
VCOV_IE_means <- VCOV_Ex2FittedMeasEquivModel[249:253, 249:253]




pfad_layout<- get_layout("1", "", "","",
                         "traitsecurity", "Micronarratives","",
                         "security","","",
                         "0","Mainstream","",
                         rows = 4)

tidySEM::graph_sem(model = m.fit1, layout = pfad_layout) 






onlymanipulation$voluntary<- as.factor(onlymanipulation$"0")
onlymanipulation$mandatory<- as.factor(onlymanipulation$"1")







lm1<- lm(micronarratives~sfree+ssec+gr_2, data=onlymanipulation )
summary(lm1)



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





summary(lm(mainstream~ssec*med_act, data= forsem_s))

summary(lm(mainstream~sfree*med_act, data= forsem_s))









summary(lm(micronarratives~sfree*med_act, data= forsem_s))

summary(lm(mainstream~sfree*med_act, data= forsem_s))






#######codes###### (for bilendi--irrelevant otherwise)

codes <- raw$p_0001

codes <- rawcodes <- as.character(codes)
write.csv(codes, "codes.csv")
codes

codes_after <- mand$p_0001

codes_after <- as.character(codes_after)
write.csv(codes_after, "codes_after.csv")


