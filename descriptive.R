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

#description per group
manddes <- mand[,c(2, 9:12, 34, 61, 64:70, 71:72, 16:20, 59, 61, 62, 63)]
des.mat <- describeBy(manddes ~ gr,
                      mat=TRUE,digits=2)


##correlations
#method1
#cors <- subset(mand[,c(2, 9:12, 34, 61, 65:70, 72:73, 20)])
cors <- subset(mand[,c(2, 34, 68, 69, 66,67, 20, 9, 72, 73, 35, 70)])

cors1 <- cors[cors$gr==1, ]
cors2 <- cors[cors$gr==2, ]
cors3 <- cors[cors$gr==3, ]

# Split data by group
grouped_data <- split(corssmall, corssmall$gr)

# Create correlation matrix for each group
cor_matrices <- lapply(grouped_data, function(grouped_df) {
  cor(grouped_df[, 2:16])  # excluding the grouping column
})

# View the result
cor_matrices


#visualisation
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

#ungrouped
ggcorrplot(cor_mat, lab = TRUE, type = "upper")


#method 2

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


a <- aov(age~ssec+tsec, data=forsem)
summary(a)
