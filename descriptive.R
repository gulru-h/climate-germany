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

###descriptives 

mand$pol <- as.factor(mand$pol)
mand[,16:20] <- lapply(mand[,16:20],as.numeric)
?as.numeric

sd(mand$SES)

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

#description per group
manddes <- mand[,c(2, 9:12, 34, 61, 64:70, 71:72, 16:20, 59, 61, 62, 63)]
des.mat <- describeBy(manddes ~ gr,
                      mat=TRUE,digits=2)


##correlations
#method1
cors <- subset(mand[,c(2, 9:12, 34, 61, 65:70, 72:73, 20)])
cors1 <- cors[mand$c_0001==1, ]
cors2 <- cors[mand$c_0001==2, ]
cors3 <- cors[mand$c_0001==3, ]

corssmall <- subset(onlymanipulation[,c(2, 9:12, 34, 61, 65:70, 72:73, 20)])



names(onlymanipulation)


#correlation matrix by group
cors %>% 
  nest_by(gr) %>% 
  summarise(CorMat = cor(cors))


item_classes <- lapply(cors, class)


cors$gr <- as.numeric(cors$gr)

#visualisation
colnames(corssmall) <- displaynames

# Split data by group
grouped_data <- split(corssmall, corssmall$gr)

# Create correlation matrix for each group
cor_matrices <- lapply(grouped_data, function(grouped_df) {
  cor(grouped_df[, 2:16])  # Exclude the grouping column
})

# View the result
cor_matrices


library(ggplot2)
library(ggcorrplot)

# Compute and plot for each group
for (gr in names(grouped_data)) {
  cor_mat <- cor(grouped_data[[gr]][, 2:16])
  
  print(ggcorrplot(cor_mat, 
                   lab = TRUE, 
                   title = paste("Correlation - Group", gr)))
}



#install.packages("gridExtra")
library(gridExtra)

plots <- lapply(names(grouped_data), function(gr) {
  cor_mat <- cor(grouped_data[[gr]][, 2:16])
  ggcorrplot(cor_mat, lab = TRUE, title = paste("Group", gr))
})

# Arrange side by side
do.call(gridExtra::grid.arrange, c(plots, ncol = length(plots)))


displaynames<- c("gr", "Left Wing Media", "Right Wing Media",
                   "Social Media", "Active media use", "Financial Burden",
                   "SES", "Financial Situation",
                   "(T)Need for Security", "(T)Need for Freedom", 
                   "(S)Need for Security", "(S)Need for Freedom",
                   "Acceptability","Anti-mainstream narratives",
                   "Mainstream Narratives", "Trust in the government")

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


