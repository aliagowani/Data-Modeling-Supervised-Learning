library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(purrr)
library(lessR)
library(ggthemes)
library(kableExtra)
library(knitr)
library(summarytools)
library(inspectdf)
library(ggcorrplot)
library(GGally)
library(ggthemes)
library(gridExtra)
library(plotly)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(skimr)
library(broom)
library(dplyr)
library(scales)



##################

mydata <- read.csv(file="NutritionStudy.csv", head=TRUE, sep=",")

options(show.signif.stars=TRUE)

mydata<-data.frame(mydata)

str(mydata)
head(mydata)
names(mydata)
skim(mydata)

print(dfSummary(mydata, max.distinct.values = 10, graph.col = TRUE), file = 'dfsummary.html')

DataExplorer::create_report(mydata)

library(inspectdf)
x <- inspect_types(mydata)
show_plot(x)

x <- inspect_mem(mydata)
show_plot(x)

x <- inspect_na(mydata)
show_plot(x)

x <- inspect_cor(mydata)
show_plot(x)

x <- inspect_imb(mydata)
show_plot(x)

x <- inspect_num(mydata)
show_plot(x)

x <- inspect_cat(mydata)
show_plot(x)


# Task 1
ggplot(mydata, aes_string(x=mydata$Cholesterol, y=mydata$Fiber)) +
  geom_point(color="#AEC7E8", size=1, alpha=0.7) +
  ggtitle("Scatter Plot of Cholesterol vs Fiber with Regression Line") +
  geom_hline(yintercept=0, size=0.06) + 
  geom_smooth(method=lm, alpha=0.25, color="#FFBB78", fill="#FFBB78") +
  labs(title = paste("Adj R2 = ", signif(summary(modeldata)$adj.r.squared, 5),
                     "Intercept =", signif(modeldata$coef[[1]], 5),
                     " Slope =", signif(modeldata$coef[[2]], 5),
                     " P =", signif(summary(modeldata)$coef[2, 4], 5))) +
  labs(x = "Cholesterol", y = "Fiber") + 
  theme(plot.title = element_text(size=11, face = "bold"))

model1 <- lm(Cholesterol ~ Fiber, data=mydata)

ggcorr(mydata, palette = "RdBu", label = TRUE, digits = 3)

ggcorr(mydata,
       label = TRUE,
       label_alpha = FALSE,
       label_round = 3,
       hjust = .85,
       layout.exp = 3,
       method = c("pairwise","pearson"),
       nbreaks = 4,
       palette="RdGy",
       label_color = "white")

ggcorr(mydata, nbreaks = 4, palette = "RdGy", label = TRUE, label_round=3, label_size = 3, label_color = "white")


# Task 2
modeldata = model1

ggplot(data=modeldata, aes(.fitted, .resid)) + 
  geom_point(color="#AEC7E8", size=2, alpha=0.7) +
  geom_smooth(method="loess", alpha=0.55, color="#FFBB78", fill="#FFBB78") +
  geom_hline(yintercept=0, linetype="dashed", color="#AEC7E8") +
  xlab("Fitted values") +
  ylab("Residuals") +
  ggtitle("Residual vs Fitted Plot") +
  theme(plot.title = element_text(size=11, face = "bold"))


ggplot(data=modeldata, aes(qqnorm(.stdresid)[[1]], .stdresid)) + 
  geom_point(na.rm = TRUE, color="#AEC7E8", size=2, alpha=0.5) +
  geom_abline(aes(qqline(.stdresid)), intercept=0, color="#FFBB78") + 
  xlab("Theoretical Quantiles") + 
  ylab("Standardized Residuals") + 
  ggtitle("Normal Q-Q") +
  theme(plot.title = element_text(size=11, face = "bold"))

ggplot(data=modeldata, aes(.hat, .stdresid)) + 
  geom_point(aes(size=.cooksd, colour=.cooksd), na.rm=TRUE) + 
  geom_smooth(method="loess", alpha=0.55, color="#AEC7E8", fill="#FFBB78", na.rm = TRUE) + 
  xlab("Leverage") +
  ylab("Standardized Residuals") + 
  ggtitle("Residual vs Leverage Plot") + 
  scale_size_continuous("Cook's Distance", range=c(1,5)) + 
  scale_colour_gradient(low = "#AEC7E8", high = "#D62728") +
  theme(legend.position="bottom") +
  theme(plot.title = element_text(size=11, face = "bold"))


ggplot(data=modeldata, aes(seq_along(.cooksd), .cooksd)) + 
  geom_bar(stat="identity", position="identity", aes(fill = .cooksd > 4/nobs(modeldata))) + 
  geom_hline(yintercept=4/nobs(modeldata), linetype="dashed", color="#AEC7E8") +
  xlab("Observation Number") + 
  ylab("Cook's distance") + 
  ggtitle("Cook's distance", subtitle = paste("Threshold", round(4/nobs(modeldata),4), "and Number of Observations:", nobs(modeldata))) + 
  scale_fill_manual(values = c('#7F7F7F', '#D62728'), name = "Cook's Distance above threshold: ")  + 
  theme(legend.position="bottom") +
  geom_text(aes(label=ifelse((.cooksd > 4/nobs(modeldata)),mydata$ID,"")),
            hjust=-0.1,vjust=-0.1,size=2.5) +
  theme(plot.title = element_text(size=11, face = "bold"), plot.subtitle = element_text(size=10))


# Task 3

# Turn continous variable into a categorical variable
mydata2 <- mydata %>%
  mutate(AlcoholLevels = case_when(Alcohol == 0 ~ "None",
                                   Alcohol < 10 ~ "Some",
                                   Alcohol >= 10 ~ "Alot"))

# Create dummy variables from categorical values
mydata2$AlcoholLevel_None <- ifelse(mydata2$AlcoholLevels == "None",1,0)
mydata2$AlcoholLevel_Some <- ifelse(mydata2$AlcoholLevels == "Some",1,0)
mydata2$AlcoholLevel_Alot <- ifelse(mydata2$AlcoholLevels == "Alot",1,0)

model2 <- lm(Cholesterol ~ Fiber + AlcoholLevel_Some + AlcoholLevel_Alot, data=mydata2)

ggplot(data=mydata2,aes(x = factor(mydata2$AlcoholLevels, level=c("None", "Some", "Alot")), y = Cholesterol, fill = as.factor(AlcoholLevels))) + 
  geom_boxplot(outlier.colour = "#F28E2B") + 
  scale_fill_manual(values = c("#FF9D9A", "#8CD17D", "#F1CE63"), name = "Alcohol Levels: ", labels = c("A Lot (>10)", "None (=0)", "Some (0 < x < 10)")) +
  theme(legend.position = "bottom") +
  xlab("Alcohol Levels") +
  ylab("Cholesterol") +
  theme(plot.title = element_text(size=11, face = "bold"), plot.subtitle = element_text(size=10)) +
  ggtitle("Box Plot of Cholesterol vs. Alcohol ")

ggplot(data=mydata2, aes(x = Fiber, y = Cholesterol, color=AlcoholLevels)) +
  geom_point(alpha = .7, size = 2) +
  scale_color_manual(values = c("#FF9D9A", "#8CD17D", "#F1CE63"), name = "Alcohol Levels: ", labels = c("A Lot (>10)", "None (=0)", "Some (0 < x < 10)")) +
  theme(legend.position = "bottom") +
  xlab("Fiber") +
  ylab("Cholesterol") +
  theme(plot.title = element_text(size=11, face = "bold"), plot.subtitle = element_text(size=10)) +
  ggtitle("Fiber vs. Cholesterol with Alcohol Levels")
  
ggplot(data=modeldata, aes(.fitted, .resid)) + 
  geom_point(color="#C5B0D5", size=2, alpha=0.7) +
  geom_smooth(method="loess", alpha=0.55, color="#FFBB78", fill="#FFBB78") +
  geom_hline(yintercept=0, linetype="dashed", color="#C5B0D5") +
  xlab("Fitted values") +
  ylab("Residuals") +
  ggtitle("Residual vs Fitted Plot") +
  theme(plot.title = element_text(size=11, face = "bold"))

ggplot(data=modeldata, aes(qqnorm(.stdresid)[[1]], .stdresid)) + 
  geom_point(na.rm = TRUE, color="#C5B0D5", size=2, alpha=0.5) +
  geom_abline(aes(qqline(.stdresid)), intercept=0, color="#FFBB78") + 
  xlab("Theoretical Quantiles") + 
  ylab("Standardized Residuals") + 
  ggtitle("Normal Q-Q") +
  theme(plot.title = element_text(size=11, face = "bold"))

ggplot(data=modeldata, aes(.hat, .stdresid)) + 
  geom_point(aes(size=.cooksd, colour=.cooksd), na.rm=TRUE) + 
  geom_smooth(method="loess", alpha=0.55, color="#C5B0D5", fill="#C5B0D5", na.rm = TRUE) + 
  xlab("Leverage") +
  ylab("Standardized Residuals") + 
  ggtitle("Residual vs Leverage Plot") + 
  scale_size_continuous("Cook's Distance", range=c(1,5)) + 
  scale_colour_gradient(low = "#C5B0D5", high = "#D62728") +
  theme(legend.position="bottom") +
  theme(plot.title = element_text(size=11, face = "bold"))

ggplot(data=modeldata, aes(seq_along(.cooksd), .cooksd)) + 
  geom_bar(stat="identity", position="identity", aes(fill = .cooksd > 4/nobs(modeldata))) + 
  geom_hline(yintercept=4/nobs(modeldata), linetype="dashed", color="#C5B0D5") +
  xlab("Observation Number") + 
  ylab("Cook's distance") + 
  ggtitle("Cook's distance", subtitle = paste("Threshold", round(4/nobs(modeldata),4), "and Number of Observations:", nobs(modeldata))) + 
  scale_fill_manual(values = c('#7F7F7F', '#D62728'), name = "Cook's Distance above threshold: ")  + 
  theme(legend.position="bottom") +
  geom_text(aes(label=ifelse((.cooksd > 4/nobs(modeldata)),mydata2$ID,"")),
            hjust=-0.1,vjust=-0.1,size=2.5) +
  theme(plot.title = element_text(size=11, face = "bold"), plot.subtitle = element_text(size=10))

# Task 4

mydata3 <- mydata2
mydata3$pred <- predict(model2)

ggplot(data=mydata3, aes(x = Fiber, y = Cholesterol, color=AlcoholLevels)) +
  geom_point(alpha = .7, size = 2) +
  scale_color_manual(values = c("#FF9D9A", "#8CD17D", "#F1CE63"), name = "Alcohol Levels: ", labels = c("A Lot (>10)", "None (=0)", "Some (0 < x < 10)")) +
  theme(legend.position = "bottom") +
  xlab("Fiber") +
  ylab("Cholesterol") +
  theme(plot.title = element_text(size=11, face = "bold"), plot.subtitle = element_text(size=10)) +
  ggtitle("Fiber vs. Cholesterol with Alcohol Levels")

ggplot(data=mydata3, aes(x = Fiber, y = pred, color=AlcoholLevels)) +
  geom_point(alpha = .7, size = 2) +
  scale_color_manual(values = c("#FF9D9A", "#8CD17D", "#F1CE63"), name = "Alcohol Levels: ", labels = c("A Lot (>10)", "None (=0)", "Some (0 < x < 10)")) +
  theme(legend.position = "bottom") +
  xlab("Fiber") +
  ylab("Predicted Value") +
  theme(plot.title = element_text(size=11, face = "bold"), plot.subtitle = element_text(size=10)) +
  ggtitle("Fiber vs. Predicted with Alcohol Levels")


# Task 5
mydata4 <- mydata3 %>%
  mutate(Fiber_AlcoNone = Fiber * AlcoholLevel_None,
         Fiber_AlcoSome = Fiber * AlcoholLevel_Some,
         Fiber_AlcoAlot = Fiber * AlcoholLevel_Alot
  )

model3 <- lm(Cholesterol ~ Fiber + AlcoholLevel_Some + AlcoholLevel_Alot + Fiber_AlcoSome + Fiber_AlcoAlot, data = mydata4)

mydata4$pred <- predict(model3)

ggplot(data=mydata4, aes(x = Fiber, y = pred, color=AlcoholLevels)) +
  geom_point(alpha = .7, size = 2) +
  scale_color_manual(values = c("#FF9D9A", "#8CD17D", "#F1CE63"), name = "Alcohol Levels: ", labels = c("A Lot (>10)", "None (=0)", "Some (0 < x < 10)")) +
  theme(legend.position = "bottom") +
  xlab("Fiber") +
  ylab("Predicted Value") +
  theme(plot.title = element_text(size=11, face = "bold"), plot.subtitle = element_text(size=10)) +
  ggtitle("Predicted Cholesterol vs. Fiber with Alcohol Levels")

ggplot(data=modeldata, aes(.hat, .stdresid)) + 
  geom_point(aes(size=.cooksd, colour=mydata4$AlcoholLevels), na.rm=TRUE) + 
  geom_smooth(method="loess", alpha=0.55, color="#BAB0AC", fill="#BAB0AC", na.rm = TRUE) + 
  xlab("Leverage") +
  ylab("Standardized Residuals") + 
  ggtitle("Residual vs Leverage Plot") + 
  scale_color_manual(values = c("#FF9D9A", "#8CD17D", "#F1CE63"), name = "Alcohol Level:") +
  theme(legend.position="bottom") +
  theme(plot.title = element_text(size=11, face = "bold")) +
  theme(legend.key=element_blank()) 

ggplot(data=modeldata, aes(seq_along(.cooksd), .cooksd)) + 
  geom_bar(stat="identity", position="identity", aes(fill = .cooksd > 4/nobs(modeldata))) + 
  geom_hline(yintercept=4/nobs(modeldata), linetype="dashed", color="#BAB0AC") +
  xlab("Observation Number") + 
  ylab("Cook's distance") + 
  ggtitle("Cook's distance", subtitle = paste("Threshold", round(4/nobs(modeldata),4), "and Number of Observations:", nobs(modeldata))) + 
  scale_fill_manual(values = c('#7F7F7F', '#D62728'), name = "Cook's Distance above threshold: ")  + 
  theme(legend.position="bottom") +
  geom_text(aes(label=ifelse((.cooksd > 4/nobs(modeldata)),mydata4$ID,"")),
            hjust=-0.1,vjust=-0.1,size=2.5) +
  theme(plot.title = element_text(size=11, face = "bold"), plot.subtitle = element_text(size=10))


# Task 7

# Turn continous variable into a categorical variable
mydata5 <- mydata4

# Create dummy variables from categorical values
mydata5$Smoke_Yes <- ifelse(mydata5$Smoke == "Yes",1,0)
mydata5$Smoke_No <- ifelse(mydata5$Smoke == "No",1,0)

mydata5 <- mydata5 %>%
mutate(Fiber_Smoke_Yes = Fiber * Smoke_Yes)

model4_smoke <- lm(Cholesterol ~ Fiber + Smoke_Yes, data = mydata5)
mydata5$pred_smoke <- predict(model4_smoke)
summary(model4_smoke)

model4_smoke_int <- lm(Cholesterol ~ Fiber + Smoke_Yes + Fiber_Smoke_Yes, data = mydata5)
summary(model4_smoke_int)

partial_f_test(full_mod = model4_smoke_int, partial_mod = model4_smoke)

smoke1 <- ggplot(data=mydata5, aes(x = Fiber, y = Cholesterol, color=Smoke)) +
  geom_point(alpha = .7, size = 2) +
  scale_color_manual(values = c("#8CD17D", "#FF9D9A"), name = "Smoke: ", labels = c("No", "Yes")) +
  theme(legend.position = "bottom") +
  xlab("Fiber") +
  ylab("Cholesterol") +
  theme(plot.title = element_text(size=11, face = "bold"), plot.subtitle = element_text(size=10)) +
  ggtitle("Fiber vs. Cholesterol with Smoke") +
  theme(legend.key=element_blank()) +
  geom_smooth(method=lm, alpha=0.25, color="#A0CBE8", fill="#A0CBE8")
  
smoke2 <- ggplot(data=mydata5, aes(x = Fiber, y = pred_smoke, color=Smoke)) +
  geom_point(alpha = .7, size = 2) +
  scale_color_manual(values = c("#8CD17D", "#FF9D9A"), name = "Smoke: ", labels = c("No", "Yes")) +
  theme(legend.position = "bottom") +
  xlab("Fiber") +
  ylab("Predicted Value") +
  theme(plot.title = element_text(size=11, face = "bold"), plot.subtitle = element_text(size=10)) +
  ggtitle("Fiber vs. Predicted with Smoke") +
  theme(legend.key=element_blank()) 

require(gridExtra)
grid.arrange(smoke1, smoke2, ncol=2)

## Vitamin
# Create dummy variables from categorical values
mydata5$Vitamin_Reg <- ifelse(mydata5$VitaminUse == "Regular",1,0)
mydata5$Vitamin_No <- ifelse(mydata5$VitaminUse == "No",1,0)
mydata5$Vitamin_Occ <- ifelse(mydata5$VitaminUse == "Occasional",1,0)

mydata5 <- mydata5 %>%
  mutate(Fiber_Vitamin_Reg = Fiber * Vitamin_Reg,
         Fiber_Vitamin_Occ = Fiber * Vitamin_Occ)
         
model4_vitamin <- lm(Cholesterol ~ Fiber + Vitamin_Reg + Vitamin_Occ, data = mydata5)
mydata5$pred_vitamin <- predict(model4_vitamin)
summary(model4_vitamin)

model4_vitamin_int <- lm(Cholesterol ~ Fiber + Vitamin_Reg + Vitamin_Occ + Fiber_Vitamin_Reg + Fiber_Vitamin_Occ, data = mydata5)
summary(model4_vitamin_int)

partial_f_test(full_mod = model4_vitamin_int, partial_mod = model4_vitamin)

vitamin1 <- ggplot(data=mydata5, aes(x = Fiber, y = Cholesterol, color=VitaminUse)) +
  geom_point(alpha = .9, size = 2) +
  scale_color_manual(values = c("#D4A6C8", "#86BCB6", "#FFBE7D"), name = "Vitamin: ", labels = c("No", "Occasional", "Regular")) +
  theme(legend.position = "bottom") +
  xlab("Fiber") +
  ylab("Cholesterol") +
  theme(plot.title = element_text(size=11, face = "bold"), plot.subtitle = element_text(size=10)) +
  ggtitle("Fiber vs. Cholesterol with Vitamin") +
  theme(legend.key=element_blank()) +
  geom_smooth(method=lm, alpha=0.25, color="#A0CBE8", fill="#A0CBE8")

vitamin2 <- ggplot(data=mydata5, aes(x = Fiber, y = pred_vitamin, color=VitaminUse)) +
  geom_point(alpha = .7, size = 2) +
  scale_color_manual(values = c("#D4A6C8", "#86BCB6", "#FFBE7D"), name = "Vitamin: ", labels = c("No", "Occasional", "Regular")) +
  theme(legend.position = "bottom") +
  xlab("Fiber") +
  ylab("Predicted Value") +
  theme(plot.title = element_text(size=11, face = "bold"), plot.subtitle = element_text(size=10)) +
  ggtitle("Fiber vs. Predicted with Vitamin") +
  theme(legend.key=element_blank()) 

require(gridExtra)
grid.arrange(vitamin1, vitamin2, ncol=2)



## Gender
# Create dummy variables from categorical values
mydata5$Gender_Male <- ifelse(mydata5$Gender == "Male",1,0)
mydata5$Gender_Female <- ifelse(mydata5$Gender == "Female",1,0)

mydata5 <- mydata5 %>%
  mutate(Fiber_Gender_Male = Fiber * Gender_Male)

model4_gender <- lm(Cholesterol ~ Fiber + Gender_Male, data = mydata5)
mydata5$pred_gender <- predict(model4_gender)
summary(model4_gender)

model4_gender_int <- lm(Cholesterol ~ Fiber + Gender_Male + Fiber_Gender_Male, data = mydata5)
summary(model4_gender_int)

partial_f_test(full_mod = model4_gender_int, partial_mod = model4_gender)

vitamin1 <- ggplot(data=mydata5, aes(x = Fiber, y = Cholesterol, color=Gender)) +
  geom_point(alpha = .9, size = 2) +
  scale_color_manual(values = c("#FABFD2", "#A0CBE8"), name = "Gender: ", labels = c("Female", "Male")) +
  theme(legend.position = "bottom") +
  xlab("Fiber") +
  ylab("Cholesterol") +
  theme(plot.title = element_text(size=11, face = "bold"), plot.subtitle = element_text(size=10)) +
  ggtitle("Fiber vs. Cholesterol with Gender") +
  theme(legend.key=element_blank()) +
  geom_smooth(method=lm, alpha=0.25, color="#8CD17D", fill="#8CD17D")

vitamin2 <- ggplot(data=mydata5, aes(x = Fiber, y = pred_gender, color=Gender)) +
  geom_point(alpha = .7, size = 2) +
  scale_color_manual(values = c("#FABFD2", "#A0CBE8"), name = "Gender: ", labels = c("Female", "Male")) +
  theme(legend.position = "bottom") +
  xlab("Fiber") +
  ylab("Predicted Value") +
  theme(plot.title = element_text(size=11, face = "bold"), plot.subtitle = element_text(size=10)) +
  ggtitle("Fiber vs. Predicted with Gender") +
  theme(legend.key=element_blank()) 

require(gridExtra)
grid.arrange(vitamin1, vitamin2, ncol=2)



## Fat & Gender
# Create dummy variables from categorical values
# mydata5$Gender_Male <- ifelse(mydata5$Gender == "Male",1,0)
# mydata5$Gender_Female <- ifelse(mydata5$Gender == "Female",1,0)
# 
# mydata5 <- mydata5 %>%
#   mutate(Fat_Gender_Male = Fat * Gender_Male)

model4_fat <- lm(Cholesterol ~ Gender_Male + Fat, data = mydata5)
mydata5$pred_fat <- predict(model4_fat)
summary(model4_fat)

model4_fat_int <- lm(Cholesterol ~ Gender_Male + Fiber_Gender_Male + Fat, data = mydata5)
summary(model4_fat_int)

partial_f_test(full_mod = model4_fat_int, partial_mod = model4_fat)

fat1 <- ggplot(data=mydata5, aes(x = Fat, y = Cholesterol, color=Gender)) +
  geom_point(alpha = .9, size = 2) +
  scale_color_manual(values = c("#FABFD2", "#A0CBE8"), name = "Gender: ", labels = c("Female", "Male")) +
  theme(legend.position = "bottom") +
  xlab("Fat") +
  ylab("Cholesterol") +
  theme(plot.title = element_text(size=11, face = "bold"), plot.subtitle = element_text(size=10)) +
  ggtitle("Fat vs. Cholesterol with Gender") +
  theme(legend.key=element_blank()) +
  geom_smooth(method=lm, alpha=0.25, color="#8CD17D", fill="#8CD17D")

fat2 <- ggplot(data=mydata5, aes(x = Fat, y = pred_fat, color=Gender)) +
  geom_point(alpha = .7, size = 2) +
  scale_color_manual(values = c("#FABFD2", "#A0CBE8"), name = "Gender: ", labels = c("Female", "Male")) +
  theme(legend.position = "bottom") +
  xlab("Fat") +
  ylab("Predicted Value") +
  theme(plot.title = element_text(size=11, face = "bold"), plot.subtitle = element_text(size=10)) +
  ggtitle("Fat vs. Predicted with Gender") +
  theme(legend.key=element_blank()) 

require(gridExtra)
grid.arrange(fat1, fat2, ncol=2)


show_col(tableau_color_pal('Tableau 20')(20))

modeldata = model1
modeldata = model2
modeldata = model3
modeldata = model4
modeldata = Full_Model
modeldata = Reduced_Model
modeldata = Full_Model_SG
modeldata = model4_smoke

anova(modeldata)
summary(modeldata)
confint(modeldata)

omnibus_f(modeldata)
regression_diagnostics(modeldata)

par(mfrow=c(1,1))  # visualize four graphs at once
plot(modeldata)


library(fastDummies)
# Recode the categorical variables (e.g.: VitaminUse, etc.)
mydata2 <- mydata %>%
  mutate(
    VitaminUse_Code = case_when(VitaminUse == "No" ~ 0,
                                 VitaminUse == "Occasional" ~ 1,
                                 VitaminUse == "Regular" ~ 2),
    Smoke_Code = case_when(Smoke == "No" ~ 0,
                           Smoke == "Yes" ~ 1),
    Gender_Code = case_when(Gender == "Male" ~ 1,
                            Gender == "Female" ~ 2),
    )

mydata4 <- mydata %>%
  mutate(
    VitaminReg_Eff = case_when(VitaminUse == "Regular" ~ 1,
                               VitaminUse == "No" ~ -1,
                               VitaminUse == "Occasional" ~ 0),
    VitaminOcc_Eff = case_when(VitaminUse == "Regular" ~ 0,
                               VitaminUse == "No" ~ -1,
                               VitaminUse == "Occasional" ~ 1)
  )

# Model 5 Alcohol Levels
mydata5 <- mydata %>%
  mutate(AlcoholLevels = case_when(Alcohol == 0 ~ 0,
                                   Alcohol < 10 ~ 1,
                                   Alcohol >= 10 ~ 2))
# Model 5 Effects Encoding
mydata5 <- mydata5 %>%
  mutate(
    Alcohol_0 = case_when(AlcoholLevels == 0 ~ 1,
                          AlcoholLevels == 1 ~ 0,
                          AlcoholLevels == 2 ~ -1),
    Alcohol_1 = case_when(AlcoholLevels == 0 ~ 0,
                          AlcoholLevels == 1 ~ 1,
                          AlcoholLevels == 2 ~ -1)
  )

# Model 5 Vitamin Eff Coding
mydata5 <- mydata5 %>%
  mutate(
    VitaminUse_Code = case_when(VitaminUse == "No" ~ 0,
                                VitaminUse == "Occasional" ~ 1,
                                VitaminUse == "Regular" ~ 2),
    VitaminReg_Eff = case_when(VitaminUse == "Regular" ~ 1,
                               VitaminUse == "No" ~ -1,
                               VitaminUse == "Occasional" ~ 0),
    VitaminOcc_Eff = case_when(VitaminUse == "Regular" ~ 0,
                               VitaminUse == "No" ~ -1,
                               VitaminUse == "Occasional" ~ 1)
  )

# Model 5 Pairwise Interaction Variables
mydata5 <-  mydata5 %>%
  mutate(
    vitReg_alco0 = VitaminReg_Eff * Alcohol_0,
    vitReg_alco1 = VitaminReg_Eff * Alcohol_1,
    vitOcc_alco0 = VitaminOcc_Eff * Alcohol_0,
    vitOcc_alco1 = VitaminOcc_Eff * Alcohol_1
  )

# rpivotTable(mydata5,width="100%", height="400px")



head(mydata2, 5) %>%
  kable(digits = 4) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "responsive"), fixed_thead = T) %>%
  save_kable(file = "mydata2.html", self_contained = T)

library(scales)
tableau20 <- c('#1F77B4', '#AEC7E8', '#FF7F0E', '#FFBB78',
               '#2CA02C', '#98DF8A', '#D62728', '#FF9896',
               '#9467BD', '#C5B0D5', '#8C564B', '#C49C94',
               '#E377C2', '#F700C2', '#7F7F7F', '#C7C7C7',
               '#BCBD22', '#DBDB8D', '#17BECF', '#9EDAE5')
show_col(tableau20)
show_col(tableau_color_pal('Tableau 20')(20))


# For the VITAMIN categorical variable, fit a simple linear model that uses the categorical variable to predict the response variable Y=CHOLESTEROL
model1 <- lm(Cholesterol ~ VitaminUse, data=mydata2)
model2 <- lm(Cholesterol ~ VitaminUse_Code, data=mydata2)

mydata3 <- fastDummies::dummy_cols(mydata, select_columns = "VitaminUse")
model3 <- lm(Cholesterol ~ VitaminUse_Occasional + VitaminUse_Regular, data=mydata3)

model4 <- lm(Cholesterol ~ VitaminOcc_Eff + VitaminReg_Eff, data=mydata4)

# Model 5 (Also known as Full_Model)
Full_Model <- lm(Cholesterol ~ vitReg_alco0 + vitReg_alco1 + vitOcc_alco0 + vitOcc_alco1 + VitaminReg_Eff + Alcohol_0 + VitaminOcc_Eff + Alcohol_1, data = mydata5)

# Model 5 (Also known as Reduced_Model)
Reduced_Model <- lm(Cholesterol ~ VitaminReg_Eff + VitaminOcc_Eff + Alcohol_0 + Alcohol_1, data = mydata5)

# Model 7 (Also known as Full_Model with Smoke and Gender)
Full_Model_SG <- lm(Cholesterol ~ vitReg_alco0 + vitReg_alco1 + vitOcc_alco0 + vitOcc_alco1 + VitaminReg_Eff + Alcohol_0 + VitaminOcc_Eff + Alcohol_1 + Smoke + Gender, data = mydata5)


modeldata = model1
modeldata = model2
modeldata = model3
modeldata = model4
modeldata = Full_Model
modeldata = Reduced_Model
modeldata = Full_Model_SG

anova(modeldata)
summary(modeldata)
confint(modeldata)

omnibus_f(modeldata)
regression_diagnostics(modeldata)

par(mfrow=c(1,1))  # visualize four graphs at once
plot(modeldata)

ggplot(data=modeldata, aes(VitaminUse_Code, Cholesterol)) + 
  geom_point(color="#9467BD", size=2, alpha=0.7) +
  geom_smooth(method="loess", alpha=0.55, color="#FFBB78", fill="#FFBB78") +
  xlab("VitaminUse_Code") +
  ylab("Cholesterol") +
  ggtitle("Cholesterol vs VitaminUse_Code")

ggplot(data=modeldata, aes(.fitted, .resid)) + 
  geom_point(color="#8C564B", size=2, alpha=0.7) +
  geom_smooth(method="loess", alpha=0.55, color="#FFBB78", fill="#FFBB78") +
  geom_hline(yintercept=0, linetype="dashed", color="#8C564B") +
  xlab("Fitted values") +
  ylab("Residuals") +
  ggtitle("Residual vs Fitted Plot")

ggplot(data=modeldata, aes(qqnorm(.stdresid)[[1]], .stdresid)) + 
  geom_point(na.rm = TRUE, color="#8C564B", size=2, alpha=0.5) +
  geom_abline(aes(qqline(.stdresid)), intercept=0, color="#FFBB78") + 
  xlab("Theoretical Quantiles") + 
  ylab("Standardized Residuals") + 
  ggtitle("Normal Q-Q")

ggplot(data=modeldata, aes(.fitted, sqrt(abs(.stdresid)))) + 
  geom_point(na.rm=TRUE, color="#9467BD", size=2, alpha=0.5) + 
  geom_smooth(method="loess", alpha=0.55, color="#17BECF", fill="#FFBB78", na.rm = TRUE) +
  xlab("Fitted Value") + 
  ylab(expression(sqrt("|Standardized residuals"))) + 
  ggtitle("Scale-Location")

ggplot(data=modeldata, aes(seq_along(.cooksd), .cooksd)) + 
  geom_bar(stat="identity", position="identity", aes(fill = .cooksd > 4/nobs(modeldata))) + 
  geom_hline(yintercept=4/nobs(modeldata), linetype="dashed", color="#9467BD") +
  xlab("Observation Number") + 
  ylab("Cook's distance") + 
  ggtitle("Cook's distance", subtitle = paste("Threshold", round(4/nobs(modeldata),4), "and Number of Observations:", nobs(modeldata))) + 
  scale_fill_manual(values = c('#7F7F7F', '#FF7F0E'))  + 
  theme(legend.position="bottom") +
  geom_text(aes(label=ifelse((.cooksd > 4/nobs(modeldata)),mydata4$ID,"")),
            hjust=-0.1,vjust=-0.1,size=2.5) 

ggplot(data=modeldata, aes(.hat, .stdresid)) + 
  geom_point(aes(size=.cooksd, colour=.cooksd), na.rm=TRUE) + 
  geom_smooth(method="loess", alpha=0.55, color="#8C564B", fill="#FFBB78", na.rm = TRUE) + 
  xlab("Leverage") +
  ylab("Standardized Residuals") + 
  ggtitle("Residual vs Leverage Plot") + 
  scale_size_continuous("Cook's Distance", range=c(1,5)) + 
  scale_colour_gradient(low = "#8C564B", high = "#D62728") +
  theme(legend.position="bottom")

ggplot(data=modeldata, aes(.hat, .cooksd)) + 
  geom_point(aes(colour=.cooksd), na.rm=TRUE) + 
  stat_smooth(method="loess", na.rm=TRUE, color="#17BECF", fill="#FFBB78") + 
  xlab("Leverage hii") + 
  ylab("Cook's Distance") + 
  ggtitle("Cook's dist vs Leverage hii/(1-hii)") + 
  scale_colour_gradient(low = "#17BECF", high = "#D62728") +
  geom_abline(slope=seq(0,3,0.5), color="#C7C7C7", linetype="dashed")

ggplot(data=mydata2,aes(VitaminUse,Cholesterol,fill=as.factor(AlcoholLevels))) + 
  geom_boxplot(outlier.colour = "#E377C2") + 
  scale_fill_manual(values = c("#FF9896", "#DBDB8D", "#AEC7E8"), name = "Alcohol Levels: ", labels = c("=0", "<10", ">=10")) +
  theme(legend.position = "bottom")

ggplot(mydata5, aes(x = VitaminUse, y = Cholesterol)) + 
stat_summary(aes(group = as.factor(AlcoholLevels), color = as.factor(AlcoholLevels)), fun.y = "mean", geom = "line", size = 2) +
  theme(legend.position = "bottom") + 
  scale_color_manual(values = c("#FF9896", "#DBDB8D", "#AEC7E8"), name = "Alcohol Levels: ", labels = c("=0", "<10", ">=10"))


# Smoker and Gender
plot1<-ggplot(mydata5, aes(x = Smoke, y = Cholesterol)) + 
  stat_summary(aes(group = as.factor(Gender), color = as.factor(Gender)), fun.y = "mean", geom = "line", size = 1) +
  theme(legend.position = "bottom") + 
  scale_color_manual(values = c("#E377C2", "#AEC7E8"), name = "Gender: ") +
  ggtitle("Interaction: Smoker and Gender, modeling Cholesterol") +
  theme(plot.title = element_text(size=10))

# Vitamin and Gender
plot2<-ggplot(mydata5, aes(x = VitaminUse, y = Cholesterol)) + 
  stat_summary(aes(group = as.factor(Gender), color = as.factor(Gender)), fun.y = "mean", geom = "line", size = 1) +
  theme(legend.position = "bottom") + 
  scale_color_manual(values = c("#E377C2", "#AEC7E8"), name = "Gender: ") +
  ggtitle("Interaction: Vitamin and Gender, modeling Cholesterol") +
  theme(plot.title = element_text(size=10))

# Alcohol and Gender
plot3<-ggplot(mydata5, aes(x = as.factor(AlcoholLevels), y = Cholesterol)) + 
  stat_summary(aes(group = as.factor(Gender), color = as.factor(Gender)), fun.y = "mean", geom = "line", size = 1) +
  theme(legend.position = "bottom") + 
  scale_color_manual(values = c("#E377C2", "#AEC7E8"), name = "Gender: ") +
  scale_x_discrete(labels=c("0" = "=0", 
                            "1" = "<10",
                            "2" = ">=10"), name="Alcohol Level") +
  ggtitle("Interaction: Alcohol and Gender, modeling Cholesterol") +
  theme(plot.title = element_text(size=10))

# Vitamin and Smoker
plot4<-ggplot(mydata5, aes(x = VitaminUse, y = Cholesterol)) + 
  stat_summary(aes(group = as.factor(Smoke), color = as.factor(Smoke)), fun.y = "mean", geom = "line", size = 1) +
  theme(legend.position = "bottom") + 
  scale_color_manual(values = c("#98DF8A", "#FF9896"), name = "Smoker: ") +
  ggtitle("Interaction: Vitamin and Smoker, modeling Cholesterol") +
  theme(plot.title = element_text(size=10))

# Alcohol and Smoker
plot5<-ggplot(mydata5, aes(x = as.factor(AlcoholLevels), y = Cholesterol)) + 
  stat_summary(aes(group = as.factor(Smoke), color = as.factor(Smoke)), fun.y = "mean", geom = "line", size = 1) +
  theme(legend.position = "bottom") + 
  scale_color_manual(values = c("#8CD17D", "#FF9D9A"), name = "Smoker: ") +
  scale_x_discrete(labels=c("0" = "=0", 
                            "1" = "<10",
                            "2" = ">=10"), name="Alcohol Level") +
  ggtitle("Interaction: Alcohol and Smoker, modeling Cholesterol") +
  theme(plot.title = element_text(size=10))

# Boxplot Smoke and Gender
plot6 <- ggplot(data=mydata5,aes(Smoke,Cholesterol,fill=as.factor(Gender))) + 
  geom_boxplot(outlier.colour = "#F28E2B") + 
  scale_fill_manual(values = c("#E377C2", "#AEC7E8"), name = "Gender: ") +
  ggtitle("Box Plot: Smoke and Gender, modeling Cholesterol") +
  theme(legend.position = "bottom") +
  theme(plot.title = element_text(size=10))


require(gridExtra)
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, ncol=2)

library("scales")
show_col(tableau_color_pal('Tableau 20')(20))

ggplot(data=mydata5,aes(Smoke,Cholesterol,fill=as.factor(Gender))) + 
  geom_boxplot(outlier.colour = "#F28E2B") + 
  scale_fill_manual(values = c("#E377C2", "#AEC7E8"), name = "Gender: ") +
  theme(legend.position = "bottom")