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
x <- inspect_na(mydata)
show_plot(x)

x <- inspect_num(mydata)
show_plot(x)

x <- inspect_cat(mydata)
show_plot(x)

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
  ylab(expression(sqrt("|Standardized residuals|"))) + 
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

ggplot(data=mydata5,aes(VitaminUse,Cholesterol,fill=as.factor(AlcoholLevels))) + 
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
