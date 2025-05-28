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

####analyses
##anova
#1- freiwillig
#2- verpflichtend
#3 - control



#only manipulation
groupcom_man <- aov(ssec ~ gr_a + tsec, data=onlymanipulation)
summary(groupcom_man)
TukeyHSD(groupcom_man)

ggplot(groupcom_man, aes(gr_a, ssec)) + 
  geom_boxplot() +
  labs(x="Group (1= mandatory, 2 = voluntary)", y="(S) Need for security")+
  scale_fill_brewer(palette = "Pastel1")

#comparisons with control are significant

#only manipulation
groupcom_manfree <- aov(sfree ~ gr_a + tfree, data=onlymanipulation)
summary(groupcom_manfree)
TukeyHSD(groupcom_manfree)

ggplot(groupcom_manfree, aes(gr_a, sfree)) + 
  geom_boxplot() +
  labs(x="Group (1= mandatory, 2 = voluntary)", y="(S) Need for freedom")+
  scale_fill_brewer(palette = "Pastel1")
#make the grouping variable ordered
mand$c_0001 <- ordered(mand$c_0001, levels = c("1", "2", "3"))


#anova + pairwise comparisons
mainst <- aov(mainstream ~ c_0001, data=mand)
summary(mainst)
TukeyHSD(mainst)

micron <- aov(micronarratives ~ c_0001, data=mand)
summary(micron)
TukeyHSD(micron)


##############pairwise comparisons

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





