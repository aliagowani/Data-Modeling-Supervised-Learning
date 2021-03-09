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


##################

setwd("")

mydata <- read.csv(file="ames_housing_data.csv", head=TRUE, sep=",")

options(show.signif.stars=TRUE)

mydata<-data.frame(mydata)

str(mydata)
head(mydata)
names(mydata)

mydata$TotalFloorSF <- mydata$FirstFlrSF + mydata$SecondFlrSF
mydata$HouseAge <- mydata$YrSold - mydata$YearBuilt
mydata$QualityIndex <- mydata$OverallQual * mydata$OverallCond
mydata$logSalePrice <- log(mydata$SalePrice)
mydata$price_sqft <- mydata$SalePrice/mydata$TotalFloorSF
summary(mydata$price_sqft)
hist(mydata$price_sqft)
subdat <- subset(mydata, select=c("TotalFloorSF","HouseAge","QualityIndex","price_sqft", "SalePrice","LotArea","BsmtFinSF1","Neighborhood","HouseStyle","LotShape","OverallQual","logSalePrice","TotalBsmtSF","HouseStyle"))

subdat2 <- subset(mydata, select=c("TotalFloorSF","HouseAge","QualityIndex","price_sqft", "SalePrice","LotArea","BsmtFinSF1","Neighborhood","HouseStyle","LotShape","OverallQual","logSalePrice","TotalBsmtSF","HouseStyle"))

str(subdat)

subdatnum <- subset(mydata, select=c("TotalFloorSF","HouseAge","QualityIndex", "SalePrice","LotArea","OverallQual","logSalePrice"))


#####################################################################
############### Gowani Modeling Assignment 2 ########################
#####################################################################

names(subdatnum)


print(dfSummary(mydata_filtered, max.distinct.values = 10, graph.col = TRUE), file = 'dfsummary.html')


library(dplyr)
mydata_filtered <- filter(mydata, BldgType == "1Fam")
mydata_filtered <- filter(mydata_filtered, Zoning == "RH" | Zoning == "RL" | Zoning == "RP" | Zoning == "RM")
mydata_filtered <- mydata_filtered %>% filter(SalePrice %in% (50000:500000))

subdat7 <- subset(mydata_filtered, select=c("TotalFloorSF","HouseAge","QualityIndex","price_sqft", "SalePrice","LotArea","BsmtFinSF1","Neighborhood","HouseStyle","LotShape","OverallQual","logSalePrice","TotalBsmtSF","HouseStyle", "Zoning"))

subdat7 <- subdat7[which(subdat7$TotalFloorSF < 4000),]




summary(subdat$SalePrice)
hist(subdat$SalePrice)

DataExplorer::create_report(mydata_filtered)

ggcorr(subdat,
       label = TRUE,
       label_alpha = TRUE,
       label_round = 3,
       hjust = .85,
       layout.exp = 2,
       method = c("pairwise","pearson"),
       nbreaks = 3,
       palette="Pastel2")

require(GGally)
ggpairs(subdat)

require(lattice)
pairs(subdat, pch = 21)

require(corrplot)
mcor <- cor(subdata)
corrplot(mcor, method="shade", shade.col=NA, tl.col="black",tl.cex=0.5)

DataExplorer::create_report(subdat)

library(inspectdf)
x <- inspect_na(subdat)
show_plot(x)

print(dfSummary(subdat), file = 'dfsummary.html')

x <- inspect_num(subdat)
show_plot(x)

x <- inspect_cat(subdat)
show_plot(x)

# Part A 1
ggplot(subdat2, aes_string(x=TotalFloorSF, y=SalePrice)) +
  geom_point(color="#AEC7E8", size=1, alpha=0.7) +
  ggtitle("Scatter Plot of Sale Price vs Total Floor SF with Regresion Line") +
  geom_hline(yintercept=0, size=0.06) + 
  geom_smooth(method=lm, alpha=0.25, color="#FFBB78", fill="#FFBB78") +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(x = "Total Floor SF", y = "Sale Price ($)")

model_a1 <- lm(subdat2$SalePrice ~ subdat2$TotalFloorSF, data=subdat2)
anova(model_a1)
summary(model_a1)
par(mfrow=c(1,1))  # visualize four graphs at once
plot(model_a1)


SLRresult = lm(SalePrice ~ TotalFloorSF, data=subdat2)#subdat2
anova(SLRresult)
summary(SLRresult)
par(mfrow=c(1,1))  # visualize four graphs at once
plot(SLRresult)
task5_m1_sum <- summary(SLRresult)



pred <- as.data.frame(predict(SLRresult,subdat2,interval="prediction"))
str(pred)
head(pred)
subdat2 <- cbind(subdat2,pred)
str(subdat2)
head(subdat2)
subdat2 <- subset( subdat2, select = -lwr)
subdat2 <- subset( subdat2, select = -upr)
library(reshape)
subdat2 <- rename(subdat2, c(fit="fitSLR"))

model_1_resid <- subdat2$SalePrice - subdat2$fitSLR
mean_resid <- mean(model_1_resid)
std_resid <- sd(model_1_resid)
model_1_std_resid <- (model_1_resid - mean_resid) / std_resid

subdat2 <- cbind(subdat2,model_1_std_resid)


# Model a1 Pred vs Residual
ggplot(subdat2, aes(x=subdat2$fitSLR, y=model_1_std_resid)) +
  geom_point(color="#98DF8A", size=1, alpha=0.7) +
  ggtitle("Scatter Plot of Standaredized Residuals vs Predicted Values") +
  geom_hline(yintercept=0, size=0.06) + 
  geom_smooth(method=lm, alpha=0.25, color="#FFBB78", fill="#FFBB78") +
  scale_x_continuous(labels = scales::dollar_format()) +
  labs(x = "Predicted Values", y = "Standaredized Residuals")

# Model a1 histogram
ggplot(data=subdat2, aes(x=subdat2$model_1_std_resid, color='#E377C2')) + 
  geom_histogram(color="#E377C2", fill="#E377C2", binwidth = .5, alpha=0.7) +
  labs(x = "Value", y = "Count")



######### Model A2 ###########

SLRresult3 = lm(SalePrice ~ OverallQual, data=subdat3)#subdat2
anova(SLRresult3)
summary(SLRresult3)
par(mfrow=c(1,1))  # visualize four graphs at once
plot(SLRresult3)

pred3 <- as.data.frame(predict(SLRresult3,subdat3,interval="prediction"))
str(pred3)
head(pred3)
subdat3 <- cbind(subdat3,pred3)
str(subdat3)
head(subdat3)
subdat3 <- subset( subdat3, select = -lwr)
subdat3 <- subset( subdat3, select = -upr)
library(reshape)
subdat3 <- rename(subdat3, c(fit="fitSLR"))

model_a2_resid <- subdat3$SalePrice - subdat3$fitSLR
mean_resid <- mean(model_a2_resid)
std_resid <- sd(model_a2_resid)
model_a2_std_resid <- (model_a2_resid - mean_resid) / std_resid

subdat3 <- cbind(subdat3,model_a2_std_resid)

# plot Model A2
ggplot(subdat3, aes(x = subdat3$OverallQual, y = subdat3$SalePrice)) +
  geom_point(color="#9467BD", size=1, alpha=0.7) +
  geom_smooth(method=lm, alpha=0.25, color="#FFBB78", fill="#FFBB78") +
  ggtitle("Model A2: Sale Price vs Overall Quality") +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(x = "Overall Quality", y = "Sale Price ($)")

# Model a2 Pred vs Residual
ggplot(subdat3, aes(x=subdat3$fitSLR, y=model_a2_std_resid)) +
  geom_point(color="#9467BD", size=1, alpha=0.7) +
  ggtitle("Scatter Plot of Standaredized Residuals vs Predicted Values") +
  geom_hline(yintercept=0, size=0.06) + 
  geom_smooth(method=lm, alpha=0.25, color="#FFBB78", fill="#FFBB78") +
  scale_x_continuous(labels = scales::dollar_format()) +
  labs(x = "Predicted Values", y = "Standaredized Residuals")

# Model a2 histogram
ggplot(data=subdat3, aes(x=subdat3$model_a2_std_resid, color='#9467BD')) + 
  geom_histogram(color="#9467BD", fill="#9467BD", binwidth = .5, alpha=0.7) +
  labs(x = "Value", y = "Count")

####### Model A2 Complete ########

show_col(tableau20)
show_col(tableau20_light)
show_col(tableau20_dark)

########### Model 3 ##############

SLRresult4 = lm(SalePrice ~ OverallQual + TotalFloorSF, data=subdat4)#subdat2
anova(SLRresult4)
summary(SLRresult4)
par(mfrow=c(1,1))  # visualize four graphs at once
plot(SLRresult4)
task5_m3_sum <- summary(SLRresult4)


pred4 <- as.data.frame(predict(SLRresult4,subdat4,interval="prediction"))
str(pred4)
head(pred4)
subdat4 <- cbind(subdat4,pred4)
str(subdat4)
head(subdat4)
subdat4 <- subset( subdat4, select = -lwr)
subdat4 <- subset( subdat4, select = -upr)
library(reshape)
subdat4 <- rename(subdat4, c(fit="fitSLR"))

model_4_resid <- subdat4$SalePrice - subdat4$fitSLR
mean_resid <- mean(model_4_resid)
std_resid <- sd(model_4_resid)
model_4_std_resid <- (model_4_resid - mean_resid) / std_resid

subdat4 <- cbind(subdat4,model_4_std_resid)

# Model 3 Pred vs Residual
ggplot(subdat4, aes(x=subdat4$fitSLR, y=model_4_std_resid)) +
  geom_point(color="#8C564B", size=1, alpha=0.7) +
  ggtitle("Scatter Plot of Standaredized Residuals vs Predicted Values") +
  geom_hline(yintercept=0, size=0.06) + 
  geom_smooth(method=lm, alpha=0.25, color="#FFBB78", fill="#FFBB78") +
  scale_x_continuous(labels = scales::dollar_format()) +
  labs(x = "Predicted Values", y = "Standaredized Residuals")

# Model 3 histogram
ggplot(data=subdat4, aes(x=subdat4$model_4_std_resid, color='#8C564B')) + 
  geom_histogram(color="#8C564B", fill="#8C564B", binwidth = .5, alpha=0.7) +
  labs(x = "Value", y = "Count")

########### Model 3 Done ##############


########### Model 4 ##############

SLRresult5 = lm(SalePrice ~ OverallQual + TotalFloorSF + TotalBsmtSF, data=subdat5)#subdat2
anova(SLRresult5)
summary(SLRresult5)
par(mfrow=c(1,1))  # visualize four graphs at once
plot(SLRresult5)
task5_m4_sum <- summary(SLRresult5)


pred5 <- as.data.frame(predict(SLRresult5,subdat5,interval="prediction"))
str(pred5)
head(pred5)
subdat5 <- cbind(subdat5,pred5)
str(subdat5)
head(subdat5)
subdat5 <- subset( subdat5, select = -lwr)
subdat5 <- subset( subdat5, select = -upr)
library(reshape)
subdat5 <- rename(subdat5, c(fit="fitSLR"))

model_5_resid <- subdat5$SalePrice - subdat5$fitSLR
mean_resid <- mean(model_5_resid)
std_resid <- sd(model_5_resid)
model_5_std_resid <- (model_5_resid - mean_resid) / std_resid

subdat5 <- cbind(subdat5,model_5_std_resid)

# Model 4 Pred vs Residual
ggplot(subdat5, aes(x=subdat5$fitSLR, y=model_5_std_resid)) +
  geom_point(color="#17BECF", size=1, alpha=0.7) +
  ggtitle("Scatter Plot of Standaredized Residuals vs Predicted Values") +
  geom_hline(yintercept=0, size=0.06) + 
  geom_smooth(method=lm, alpha=0.25, color="#FFBB78", fill="#FFBB78") +
  scale_x_continuous(labels = scales::dollar_format()) +
  labs(x = "Predicted Values", y = "Standaredized Residuals")

# Model 4 histogram
ggplot(data=subdat5, aes(x=subdat5$model_5_std_resid, color='#17BECF')) + 
  geom_histogram(color="#17BECF", fill="#17BECF", binwidth = .5, alpha=0.7) +
  labs(x = "Value", y = "Count")

########### Model 4 Done ##############


########### Task 6 ##############

### Model 1
task6_model_a1 <- lm(logSalePrice ~ TotalFloorSF, data=subdat2)
anova(task6_model_a1)
summary(task6_model_a1)
task6_m1_sum <- summary(task6_model_a1)

### Model 3
task6_SLRresult4 = lm(logSalePrice ~ OverallQual + TotalFloorSF, data=subdat4)
anova(task6_SLRresult4)
summary(task6_SLRresult4)
task6_m3_sum <- summary(task6_SLRresult4)

### Model 4
task6_SLRresult5 = lm(logSalePrice ~ OverallQual + TotalFloorSF + TotalBsmtSF, data=subdat5)
anova(task6_SLRresult5)
summary(task6_SLRresult5)
task6_m4_sum <- summary(task6_SLRresult5)


# Summary Table
task6_summary_df <- tibble(
                      "Model" = c("Model_1", "Model_3", "Model_4"),
                      "Model Sale Price R-Squared" = c(
                        task5_m1_sum$r.squared,
                        task5_m3_sum$r.squared,
                        task5_m4_sum$r.squared),
                      "Model Log Sale Price R-Squared" = c(
                        task6_m1_sum$r.squared,
                        task6_m3_sum$r.squared,
                        task6_m4_sum$r.squared),
                      "Model Sale Price Adj. R-Squared" = c(
                        task5_m1_sum$adj.r.squared,
                        task5_m3_sum$adj.r.squared,
                        task5_m4_sum$adj.r.squared),
                      "Model Log Sale Price Adj. R-Squared" = c(
                        task6_m1_sum$adj.r.squared,
                        task6_m3_sum$adj.r.squared,
                        task6_m4_sum$adj.r.squared),
                      )

# Output
knitr::kable(task6_summary_df)
view(task6_summary_df)

knitr::kable(xdata)
view(xdata)

task6_summary_df_t <- transpose(task6_summary_df)

rownames(task6_summary_df_t) <- colnames(task6_summary_df)
colnames(task6_summary_df_t) <- rownames(task6_summary_df)
task6_summary_df_t

ggplot(task6_summary_df, aes(x=task6_summary_df$`Model Sale Price R-Squared`)) +
  geom_bar(color="#17BECF")

Model_Name <- c(rep("Model 1" , 4) , rep("Model 3" , 4) , rep("Model 4" , 4))
Statistic <- rep(c("Sale Price R^2" , "Sale Price Adj. R^2", "Log Sale Price R^2" , "Log Sale Price Adj. R^2") , 3)
value <- c(task5_m1_sum$r.squared, 
           task5_m3_sum$r.squared, 
           task5_m4_sum$r.squared, 
           task5_m1_sum$adj.r.squared, 
           task5_m3_sum$adj.r.squared, 
           task5_m4_sum$adj.r.squared, 
           task6_m1_sum$r.squared, 
           task6_m3_sum$r.squared, 
           task6_m4_sum$r.squared, 
           task6_m1_sum$adj.r.squared, 
           task6_m3_sum$adj.r.squared, 
           task6_m4_sum$adj.r.squared)
xdata <- data.frame(Model_Name,Statistic,value)

# Grouped
ggplot(xdata, aes(x=Model_Name)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_discrete('Statistic') + 
  guides(fill = guide_legend(reverse = TRUE))

filter_xdata1 <- filter(xdata, Model_Name=="Model 1")
plot1 <- ggplot(filter_xdata1, aes(fill=Statistic, y=value, x=Model_Name)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_discrete('Statistic') + 
  guides(fill = guide_legend(reverse = TRUE))

filter_xdata2 <- filter(xdata, Model_Name=="Model 3")
plot2 <- ggplot(filter_xdata2, aes(fill=Statistic, y=value, x=Model_Name)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_discrete('Statistic') + 
  guides(fill = guide_legend(reverse = TRUE))

filter_xdata3 <- filter(xdata, Model_Name=="Model 4")
plot3 <- ggplot(filter_xdata3, aes(fill=Statistic, y=value, x=Model_Name)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_discrete('Statistic') + 
  guides(fill = guide_legend(reverse = TRUE))

require(gridExtra)
grid.arrange(plot1, plot2, plot3, ncol=3)


ggplot(xdata, aes(fill=Statistic, y=value, x=Model_Name)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_discrete('Statistic') + 
  guides(fill = guide_legend(reverse = TRUE))

plot7 <- ggplot(data=subdat7, aes(x=subdat7$SalePrice, color='#2CA02C')) + 
  geom_histogram(color="#2CA02C", fill="#2CA02C", bins = 30, alpha=0.7) +
  labs(x = "Value", y = "Count")

plot8 <- ggplot(data=subdat7, aes(x=subdat7$logSalePrice, color='#2CA02C')) + 
  geom_histogram(color="#2CA02C", fill="#2CA02C", bins = 30, alpha=0.7) +
  labs(x = "Value", y = "Count")

require(gridExtra)
grid.arrange(plot7, plot8, ncol=2)

show_col(tableau20)



########### Model 4 Done ##############

########## Task 8 ##################
MLRresult = lm(SalePrice ~ TotalFloorSF+OverallQual+TotalBsmtSF, data=subdat7)
anova(MLRresult)
summary(MLRresult)

par(mfrow=c(2,2))  # visualize four graphs at once
plot(MLRresult)

pred <- as.data.frame(predict(MLRresult,subdat7))
names(pred)
library(reshape)
pred3 <- rename(pred, c("predict(MLRresult, subdat)" = "prd"))
subdat7$pred <- pred3$prd
subdat7$res <- subdat7$SalePrice - subdat7$pred
subdat7$absres <- abs(subdat7$res)
MAE <- mean(subdat7$absres)
MAE

require(ggplot2)
ggplot(subdat, aes(x=OverallQual, y=res)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Residual vs OverallQual") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) + 
  geom_smooth(method=lm,se=FALSE)  ## method=lm, se=FALSE ###

################################################################
############### Log Transformation #############################

MLRLogresult = lm(logSalePrice ~ TotalFloorSF+OverallQual, data=subdatnum)
anova(MLRLogresult)
summary(MLRLogresult)
par(mfrow=c(2,2))  # visualize four graphs at once
plot(MLRLogresult)

pred <- as.data.frame(predict(MLRLogresult,subdatnum,interval="prediction"))
str(pred)
head(pred)
subdatnum <- cbind(subdatnum,pred)
subdatnum <- subset( subdatnum, select = -lwr)
subdatnum <- subset( subdatnum, select = -upr)
str(subdatnum)
head(subdatnum)
subdatnum <- rename(subdatnum, c(fit="fitMLRLog"))
subdatnum$reslog <- subdatnum$logSalePrice - subdatnum$fitMLRLog
MAE <- mean(abs(subdatnum$reslog))
MAE

head(subdatnum)

library(car)
vif(MLRLogresult)
par(mfrow=c(1,1))
influencePlot(MLRLogresult,	id.method="identify", main="Influence Plot", 
              sub="Circle size is proportial to Cook's Distance")


summary(inflm.MLRLog <- influence.measures(MLRLogresult))
dffitslog <- dffits(MLRLogresult)
subdatnum <- cbind(subdatnum,dffitslog)
str(subdatnum)

ggplot(subdatnum, aes(x=OverallQual, y=reslog)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Residual vs OverallQual") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) + 
  geom_smooth(method=lm,se=FALSE)  ## method=lm, se=FALSE ###

################ influential points removed  #######
subdatnum$absdf <- abs(subdatnum$dffitslog)
head(subdatnum)
subdatnuminf <- subdatnum[which(subdatnum$absdf < 0.064),]

MLRLogresult = lm(logSalePrice ~ TotalFloorSF+OverallQual+TotalBsmtSF, data=subdat7)
anova(MLRLogresult)
summary(MLRLogresult)

# Run Combination of Variables to see highest adjust r square
require(MuMIn)

globalmodel <- lm(logSalePrice ~ + TotalFloorSF + HouseAge + QualityIndex + SalePrice + LotArea + OverallQual, data = subdat2)

combinations <- dredge(globalmodel, m.lim = c(NA, 2), extra = c("adjR^2", F = function(x)
  summary(x)$fstatistic[[1]]))

# options(na.action = "na.omit")
# options(na.action = "na.fail")
print(combinations)

coefTable(combinations)

coefTable(combinations)[1]

combinations %>%
  kable(digits = 4) %>%
  kable_styling() %>%
  save_kable(file = "combinations4.html", self_contained = T)



combinations <- dredge(globalmodel, m.lim = c(NA, 3), extra = c("R^2", AdjustedRSqrd = function(x)
  summary(x)$adj.r.squared[[1]]))

# options(na.action = "na.omit")
# options(na.action = "na.fail")
print(combinations)

coefTable(combinations)

summary(globalmodel)$r.squared


#####################################################################
############### Gowani Modeling Assignment 2 ########################
########################### Done ####################################


#####################################################################
######################### Assignment 1 ##############################
#####################################################################

#################################################################
################## univariate EDA ##############################
###############################################################
require(ggplot2)
ggplot(subdat) +
  geom_bar( aes(LotShape) ) +
  ggtitle("Number of houses per Lotshape") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(subdat, aes(x=SalePrice)) + 
  geom_histogram(color="black", binwidth= 10000) +
  labs(title="Distribution of Sale Price") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(subdat, aes(x=TotalFloorSF)) + 
  geom_histogram(color="black", binwidth= 100) +
  labs(title="Distribution of TotalFloorSF") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(subdat, aes(x=QualityIndex)) + 
  geom_histogram(color="black", binwidth= 10) +
  labs(title="Distribution of QualityIndex") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
#######################################################################
########### bivariate EDA ########################################
###################################################################
ggplot(subdat, aes(x=TotalFloorSF, y=QualityIndex)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of Total Floor SF vs QualityIndex") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(subdat, aes(x=TotalFloorSF, y=HouseAge)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of Total Floor SF vs HouseAge") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(subdat, aes(x=LotShape, y=HouseAge)) + 
  geom_boxplot(fill="blue") +
  labs(title="Distribution of HouseAge") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

############################################################
################ model focussed EDA #######################
###########################################################

ggplot(subdat, aes(x=TotalFloorSF, y=SalePrice)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Sale Price vs Total Floor SF") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) +
  geom_smooth(method=lm, se=FALSE)  ## method=lm, se=FALSE ###

ggplot(subdat, aes(x=QualityIndex, y=SalePrice)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of Sale Price vs QualityIndex") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) 

ggplot(subdat, aes(x=LotShape, y=SalePrice)) + 
  geom_boxplot(fill="blue") +
  labs(title="Distribution of Sale Price") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

#####################################################################
############# EDA for multiple variables ###########################
##################################################################
require(GGally)
ggpairs(subdat)

require(lattice)
pairs(subdat, pch = 21)

require(corrplot)
mcor <- cor(subdatnum)
corrplot(mcor, method="shade", shade.col=NA, tl.col="black",tl.cex=0.5)

#####################################################################
############# Define the sample data ###########################
##################################################################

subdat2 <- subdat[which(subdat$TotalFloorSF < 4000),]

###################################################################
##################  Assignment 2  ################################
#################################################################

attach(subdat2)

ggplot(subdat2, aes(x=TotalFloorSF, y=SalePrice)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Sale Price vs Total Floor SF") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) + 
  geom_smooth(method=lm,se=FALSE)  ## method=lm, se=FALSE ###

ggplot(subdat2, aes(x=QualityIndex, y=SalePrice)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of Sale Price vs QualityIndex") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) +
  geom_smooth(method=lm)  ## method=lm, se=FALSE ###


# 3D Scatterplot with Coloring and Vertical Lines
# and Regression Plane 
library(scatterplot3d)
attach(subdat2)
s3d <-scatterplot3d(TotalFloorSF,QualityIndex,SalePrice,pch=16, 
                    highlight.3d=TRUE,type="h", main="3D Scatterplot")
fit <- lm(SalePrice ~ TotalFloorSF + QualityIndex) 
s3d$plane3d(fit)

library(Rcmdr)
attach(subdat2)
scatter3d(SalePrice,TotalFloorSF,QualityIndex)

############## fitting a SLR ###################################

SLRresult = lm(SalePrice ~ TotalFloorSF, data=subdatnum)#subdat2
anova(SLRresult)
summary(SLRresult)
par(mfrow=c(1,1))  # visualize four graphs at once
plot(SLRresult)

pred <- as.data.frame(predict(SLRresult,subdatnum,interval="prediction"))
str(pred)
head(pred)
subdatnum <- cbind(subdatnum,pred)
str(subdatnum)
head(subdatnum)
subdatnum <- subset( subdatnum, select = -lwr)
subdatnum <- subset( subdatnum, select = -upr)
library(reshape)
subdatnum <- rename(subdatnum, c(fit="fitSLR"))


############## fitting a MLR ###################################

MLRresult = lm(SalePrice ~ TotalFloorSF+OverallQual, data=subdatnum)
anova(MLRresult)
summary(MLRresult)
par(mfrow=c(2,2))  # visualize four graphs at once
plot(MLRresult)

pred <- as.data.frame(predict(MLRresult,subdatnum,interval="prediction"))
str(pred)
head(pred)
subdatnum <- cbind(subdatnum,pred)
subdatnum <- subset( subdatnum, select = -lwr)
subdatnum <- subset( subdatnum, select = -upr)
str(subdatnum)
head(subdatnum)
subdatnum <- rename(subdatnum, c(fit="fitMLR"))
subdatnum$res <- subdatnum$SalePrice - subdatnum$fitMLR

head(subdatnum)

###################################################################
##################### Assignment 3  ################################
#################################################################

################  MAE calculation ###################################
MLRresult = lm(SalePrice ~ TotalFloorSF+OverallQual, data=subdat)
anova(MLRresult)
summary(MLRresult)

par(mfrow=c(2,2))  # visualize four graphs at once
plot(MLRresult)

pred <- as.data.frame(predict(MLRresult,subdat))
names(pred)
library(reshape)
pred <- rename(pred, c("predict(MLRresult, subdat)" = "prd"))
subdat$pred <- pred$prd
subdat$res <- subdat$SalePrice - subdat$pred
subdat$absres <- abs(subdat$res)
MAE <- mean(subdat$absres)
MAE

require(ggplot2)
ggplot(subdat, aes(x=OverallQual, y=res)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Residual vs OverallQual") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) + 
  geom_smooth(method=lm,se=FALSE)  ## method=lm, se=FALSE ###

################################################################
############### Log Transformation #############################

MLRLogresult = lm(logSalePrice ~ TotalFloorSF+OverallQual, data=subdatnum)
anova(MLRLogresult)
summary(MLRLogresult)
par(mfrow=c(2,2))  # visualize four graphs at once
plot(MLRLogresult)

pred <- as.data.frame(predict(MLRLogresult,subdatnum,interval="prediction"))
str(pred)
head(pred)
subdatnum <- cbind(subdatnum,pred)
subdatnum <- subset( subdatnum, select = -lwr)
subdatnum <- subset( subdatnum, select = -upr)
str(subdatnum)
head(subdatnum)
subdatnum <- rename(subdatnum, c(fit="fitMLRLog"))
subdatnum$reslog <- subdatnum$logSalePrice - subdatnum$fitMLRLog
MAE <- mean(abs(subdatnum$reslog))
MAE

head(subdatnum)

library(car)
vif(MLRLogresult)
par(mfrow=c(1,1))
influencePlot(MLRLogresult,	id.method="identify", main="Influence Plot", 
              sub="Circle size is proportial to Cook's Distance")


summary(inflm.MLRLog <- influence.measures(MLRLogresult))
dffitslog <- dffits(MLRLogresult)
subdatnum <- cbind(subdatnum,dffitslog)
str(subdatnum)

ggplot(subdatnum, aes(x=OverallQual, y=reslog)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Residual vs OverallQual") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) + 
  geom_smooth(method=lm,se=FALSE)  ## method=lm, se=FALSE ###

################ influential points removed  #######
subdatnum$absdf <- abs(subdatnum$dffitslog)
head(subdatnum)
subdatnuminf <- subdatnum[which(subdatnum$absdf < 0.064),]

MLRLogresult = lm(logSalePrice ~ TotalFloorSF+OverallQual, data=subdatnuminf)
anova(MLRLogresult)
summary(MLRLogresult)

############## analyze Neighborhood variable #########

require(ggplot2)
ggplot(subdat, aes(x=Neighborhood, y=SalePrice)) + 
  geom_boxplot(fill="blue") +
  labs(title="Distribution of Sale Price") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

library(plyr)
subdat1 <- ddply(subdat, .(Neighborhood), summarise, 
                 MAE = mean(absres))
subdat2 <- ddply(subdat, .(Neighborhood), summarise, 
                 MeanPrice = mean(SalePrice))
subdat3 <- ddply(subdat, .(Neighborhood), summarise, 
                 TotalPrice = sum(SalePrice))
subdat4 <- ddply(subdat, .(Neighborhood), summarise, 
                 TotalSqft = sum(TotalFloorSF))
subdat34 <- cbind(subdat3,subdat4)
subdat34$AvgPr_Sqft <- subdat34$TotalPrice/subdat34$TotalSqft

subdatall <- subdat1
subdatall$MeanPrice <- subdat2$MeanPrice
subdatall$AvgPr_Sqft <- subdat34$AvgPr_Sqft

require(ggplot2)
ggplot(subdatall, aes(x=AvgPr_Sqft, y=MeanPrice)) + 
  geom_point(color="blue", shape=1,size=3) +
  ggtitle("Scatter Plot") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

#### Clean up of the Neighborhood varaible  ########

subdat$NbhdGrp <-
  ifelse(subdat$price_sqft<=100, "grp1", 
         ifelse(subdat$price_sqft<=120, "grp2",
                ifelse(subdat$price_sqft<=140, "grp3",
                       "grp4"))) 

################ include categoriacl variable in the model #######

MLRresult = lm(SalePrice ~ TotalFloorSF+OverallQual+NbhdGrp, data=subdat)
anova(MLRresult)
summary(MLRresult)
pred <- as.data.frame(predict(MLRresult,subdat))
names(pred)
library(reshape)
pred <- rename(pred, c("predict(MLRresult, subdat)" = "prd"))
subdat$pred <- pred$prd
subdat$res <- subdat$SalePrice - subdat$pred
subdat$absres <- abs(subdat$res)
MAE <- mean(subdat$absres)
MAE

################# define dummy variables ###################

subdat$NbhdGrp1 <- 
  ifelse(subdat$NbhdGrp == "grp1", 1, 0)
subdat$NbhdGrp2 <- 
  ifelse(subdat$NbhdGrp == "grp2", 1, 0)
subdat$NbhdGrp3 <- 
  ifelse(subdat$NbhdGrp == "grp3", 1, 0)

MLRresult4 = lm(SalePrice ~ TotalFloorSF+OverallQual+NbhdGrp1+NbhdGrp2+NbhdGrp3, 
                data=subdat)
anova(MLRresult4)
summary(MLRresult4)



############################################################
############## assignment 5 #############################
#########################################################

# Set the seed on the random number generator so you get the same split every time that
# you run the code.
my.data <- subdat
set.seed(123)
my.data$u <- runif(n=dim(my.data)[1],min=0,max=1)

# Create train/test split;
train.df <- subset(my.data, u<0.70);
test.df  <- subset(my.data, u>=0.70);
names(train.df)

train.clean <- subset(train.df, select=c("TotalFloorSF","HouseAge",
                                         "OverallQual","LotArea","logSalePrice","BsmtFinSF1",
                                         "TotalBsmtSF","Style1","Style2"))

test.clean <- subset(test.df, select=c("TotalFloorSF","HouseAge",
                                       "OverallQual","LotArea","logSalePrice","BsmtFinSF1",
                                       "TotalBsmtSF","Style1","Style2"))


# Check your data split. The sum of the parts should equal the whole.
# Do your totals add up?
dim(my.data)[1]
dim(train.df)[1]
dim(test.df)[1]
dim(train.df)[1]+dim(test.df)[1]

train.clean <- na.omit(train.clean)
test.clean <- na.omit(test.clean)


# Define the upper model as the FULL model
upper.lm <- lm(logSalePrice ~ .,data=train.clean);
summary(upper.lm)

# Define the lower model as the Intercept model
lower.lm <- lm(logSalePrice ~ 1,data=train.clean);
summary(lower.lm)
# Need a SLR to initialize stepwise selection
sqft.lm <- lm(logSalePrice ~ TotalFloorSF,data=train.clean);
summary(sqft.lm)

# Note: There is only one function for classical model selection in R - stepAIC();
# stepAIC() is part of the MASS library.
# The MASS library comes with the BASE R distribution, but you still need to load it;
library(MASS)

# Call stepAIC() for variable selection

forward.lm <- stepAIC(object=lower.lm,scope=list(upper=upper.lm,lower=lower.lm),
                      direction=c('forward'));
summary(forward.lm)

backward.lm <- stepAIC(object=upper.lm,direction=c('backward'));
summary(backward.lm)

stepwise.lm <- stepAIC(object=sqft.lm,scope=list(upper=formula(upper.lm),lower=~1),
                       direction=c('both'));
summary(stepwise.lm)

junk.lm <- lm(logSalePrice ~ OverallQual + LotArea, data=train.clean)
summary(junk.lm)

library(car)
sort(vif(forward.lm),decreasing=TRUE)
sort(vif(backward.lm),decreasing=TRUE)
sort(vif(stepwise.lm),decreasing=TRUE)
sort(vif(junk.lm),decreasing=TRUE)

forward.test <- predict(forward.lm,newdata=test.clean);
backward.test <- predict(backward.lm,newdata=test.clean);
stepwise.test <- predict(stepwise.lm,newdata=test.clean);
junk.test <- predict(junk.lm,newdata=test.clean);

# Training Data
# Abs Pct Error
forward.pct <- abs(forward.lm$residuals)/train.clean$logSalePrice;
MAPE <- mean(forward.pct)
MAPE
backward.pct <- abs(backward.lm$residuals)/train.clean$logSalePrice;
MAPE <- mean(backward.pct)
MAPE
stepwise.pct <- abs(stepwise.lm$residuals)/train.clean$logSalePrice;
MAPE <- mean(stepwise.pct)
MAPE
junk.pct <- abs(junk.lm$residuals)/train.clean$logSalePrice;
MAPE <- mean(junk.pct)
MAPE

# Test Data
# Abs Pct Error
forward.testPCT <- abs(test.df$logSalePrice-forward.test)/test.df$logSalePrice;
MAPE <- mean(forward.testPCT)
MAPE
backward.testPCT <- abs(test.df$logSalePrice-backward.test)/test.df$logSalePrice;
MAPE <- mean(backward.testPCT)
MAPE
stepwise.testPCT <- abs(test.df$logSalePrice-stepwise.test)/test.df$logSalePrice;
MAPE <- mean(stepwise.testPCT)
MAPE
junk.testPCT <- abs(test.df$logSalePrice-junk.test)/test.df$logSalePrice;
MAPE <- mean(junk.testPCT)
MAPE


# Assign Prediction Grades training data;
forward.PredictionGrade <- ifelse(forward.pct<=0.10,'Grade 1: [0.0.10]',
                                  ifelse(forward.pct<=0.15,'Grade 2: (0.10,0.15]',
                                         ifelse(forward.pct<=0.25,'Grade 3: (0.15,0.25]',
                                                'Grade 4: (0.25+]')
                                  )					
)

forward.trainTable <- table(forward.PredictionGrade)
forward.trainTable/sum(forward.trainTable)


# Assign Prediction Grades test data;
forward.testPredictionGrade <- ifelse(forward.testPCT<=0.10,'Grade 1: [0.0.10]',
                                      ifelse(forward.testPCT<=0.15,'Grade 2: (0.10,0.15]',
                                             ifelse(forward.testPCT<=0.25,'Grade 3: (0.15,0.25]',
                                                    'Grade 4: (0.25+]')
                                      )					
)

forward.testTable <-table(forward.testPredictionGrade)
forward.testTable/sum(forward.testTable)
######################################################################
sub <- subset(subdat, select=c("TotalFloorSF","HouseAge",
                               "OverallQual","LotArea","logSalePrice","BsmtFinSF1",
                               "TotalBsmtSF","Style1","Style2"))

MLRresult1 = lm(logSalePrice ~ ., data=sub)

sub2 <- subset(subdat, select=c("TotalFloorSF","HouseAge",
                                "OverallQual","LotArea","logSalePrice","BsmtFinSF1",
                                "TotalBsmtSF"))

MLRresult2 = lm(logSalePrice ~ ., data=sub2)
anova(MLRresult1,MLRresult2)

anova(MLRresult1)
summary(MLRresult1)
par(mfrow=c(2,2))  # visualize four graphs at once
plot(MLRresult)

names(MLRresult)
head(MLRresult$df.residual)

inflm.MLRLog <- influence.measures(MLRresult)
names(inflm.MLRLog)
str(inflm.MLRLog)
inflmetrics <- as.data.frame(inflm.MLRLog$infmat)
dffit_df <- subset(inflmetrics, select= c(dffit))
sub$r1 <- row.names(sub)
dffit_df$r1 <- row.names(dffit_df)
subnew <- merge(sub, dffit_df, all=FALSE)

subnew <- subset(subnew, select= -c(r1))

subnew$absdffit <- abs(subnew$dffit)

subnewinf <- subnew[which(subnew$absdf < 0.064),]



MLRLogresult = lm(logSalePrice ~ TotalFloorSF+OverallQual+HouseAge+
                    LotArea+BsmtFinSF1+TotalBsmtSF+Style1+Style2,data=subnewinf)
anova(MLRLogresult)
summary(MLRLogresult)
par(mfrow=c(2,2))  # visualize four graphs at once
plot(MLRLogresult)





mydata$TotalSF <- mydata$FirstFlrSF + mydata$SecondFlrSF
mydata$HouseAge <- mydata$YrSold - mydata$YearBuilt
mydata$QualityIndex <- mydata$OverallQual * mydata$OverallCond
mydata$logSalePrice <- log(mydata$SalePrice)
mydata$price_sqft <- mydata$SalePrice / mydata$TotalSF

str(mydata)
head(mydata)
names(mydata)
skim(mydata)



# view(dfSummary(mydata, plain.ascii = FALSE, style = "grid", 
#               graph.magnif = 0.75, valid.col = FALSE, tmp.img.dir = "/tmp"))

print(dfSummary(mydata, max.distinct.values = 10, graph.col = TRUE), file = 'dfsummary.html')


dt <- summary(mydata)
dt <- t(dt)
dt %>%
  kable() %>%
  kable_styling() %>%
  save_kable(file = "table1.html", self_contained = T)

BarChart(mydata)

ggcorr(mydata_sub[1:10],
       label = TRUE,
       label_alpha = TRUE,
       label_round = 3,
       hjust = .85,
       layout.exp = 2,
       method = c("pairwise","pearson"),
       nbreaks = 3,
       palette="Pastel2")

model3_df <- mydata %>%
  select(
    SalePrice,
    FirstFlrSF,
    SecondFlrSF,
    GrLivArea,
    TotalBsmtSF,
    EnclosedPorch,
    GarageArea,
    LotFrontage,
    LotArea,
    MasVnrArea,
    WoodDeckSF)

variable_types <-
  c(
    "Continuous",
    "Continuous",
    "Continuous",
    "Continuous",
    "Continuous",
    "Continuous",
    "Continuous",
    "Continuous",
    "Continuous",
    "Continuous",
    "Continuous"
  )
variable_group <-
  c(
    "Target",
    "Interior",
    "Interior",
    "Interior",
    "Interior",
    "Interior",
    "Interior",
    "Exterior",
    "Exterior",
    "Exterior",
    "Exterior"
  )

model3_df_columns <- data.frame(
                        "Variable_Name" = colnames(model3_df),
                        "Variable_Type" = variable_types,
                        "Variable_Group" = variable_group)
  
model3_df_columns %>%
          kable() %>%
          kable_styling()

options(show.signif.stars=TRUE)


model3_lm <- lm(SalePrice ~ FirstFlrSF + SecondFlrSF + GrLivArea + 
                  TotalBsmtSF + EnclosedPorch + GarageArea,
                data = model3_df)
summary(model3_lm)
anova(model3_lm)
AIC(model3_lm)
tab_model(model3_lm)

((5827979153622 - 44670) / 6) / (44670 / (2928 - 6 - 1))

# Write ANOVA results to dataframe
anova_model3 <- anova(model3_lm)

# Extract SSY & SSE
SSY <- sum(anova_model3$`Sum Sq`)
SSE <- anova_model3$`Sum Sq`[7]

# Define k & n
k <- 6
# n <- nrow(mydata)
n <- 2928

# Calculate numerator and denominator
numerator_f <- (SSY - SSE) / k
denominator_f <-  SSE / (n - k - 1)

# Calculate F
F <- round(numerator_f / denominator_f)

# Output
print(paste("The F-statistic =", F))

tidy_model3_lm <- tidy(model3_lm)
tidy_model3_lm <- tidy(anova(model3_lm))

tidy_model3_lm %>%
  kable(digits = 3) %>%
  kable_styling() %>%
  save_kable(file = "tidy_model3_lm.html", self_contained = T)


plot_model(model3_lm)

model4_lm <- lm(SalePrice ~ FirstFlrSF + SecondFlrSF + GrLivArea +
                  TotalBsmtSF + EnclosedPorch + GarageArea + LotFrontage +
                  LotArea + MasVnrArea + WoodDeckSF,
                data = model3_df)
summary(model4_lm)
anova(model4_lm)
AIC(model4_lm)
tab_model(model3_lm, model4_lm, show.stat = TRUE)
tidy(model4_lm)

plot_model(model3_lm, model4_lm)

tab_model(model3_lm, model4_lm, show.se = TRUE, show.std = TRUE, show.stat = TRUE, string.ci = "Conf. Int (95%)", show.fstat = TRUE, string.stat = "t-statistic", show.ci = 0.95, digits = 3)


aov(model4_lm) %>%
  tidy() %>%
  kable(digits = 5, caption = "Model 4 LM") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  save_kable(file = "table1.html", self_contained = T)

summary(model4_lm) %>%
  tidy() %>%
  kable(digits = 5, caption = "Model 4 LM") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  save_kable(file = "table1.html", self_contained = T)

# SSE
sum(model4_lm$residuals^2)
sum(model3_lm$residuals^2)

# SSY
model4_lm_anova <- anova(model4_lm)
sum(model4_lm_anova$`Sum Sq`)

mydata_sub <- mydata %>%
  select(-c("State", "Region", "Population"))

mydata_sub <- data.frame(mydata_sub)

plt <- ggplot(mydata_sub, aes_string(x=names(mydata_sub)[10], y = mydata_sub$HouseholdIncome)) +
  geom_point(color="#1F77B4", size=4, alpha=0.5) +
  geom_hline(yintercept=0, size=0.06) + 
  geom_smooth(method=lm, alpha=0.25, color="#FF9896", fill="#FF9896") +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(x = names(mydata_sub)[10], y = "Household Income ($)")
print(plt)

plt <- ggplot(mydata_sub, aes_string(x=names(mydata_sub)[10], y = mydata_sub$HouseholdIncome)) +
  geom_point(aes(color=factor(mydata$Region)), size=4, alpha = .5) +
  geom_hline(yintercept=0, size=0.06) + 
  geom_smooth(method=lm, alpha=0.25, color="#FF9896", fill="#FF9896") +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(x = names(mydata_sub)[10], y = "Household Income ($)")
print(plt)


plt <- ggplot(mydata_sub, aes_string(x=names(mydata_sub)[10], y = mydata_sub$HouseholdIncome)) +
  geom_point(aes(color=factor(mydata$Region)), size=4, alpha=.34) +
  scale_colour_manual(values=tableau20_dark, name = "Regions:") +
  geom_hline(yintercept=20, size=0.06) + 
  geom_vline(xintercept=65, size=0.06) +
  geom_smooth(method=lm, alpha=0.25, color="#7F7F7F", fill = "#7F7F7F") +
  scale_y_continuous(labels = scales::dollar_format(), limits = c(20, 80)) +
  labs(x = names(mydata_sub)[10], y = "Household Income ($)") +
  theme(legend.position="bottom")
plt
ggplotly(plt)




library(scales)
tableau20 <- c('#1F77B4', '#AEC7E8', '#FF7F0E', '#FFBB78',
               '#2CA02C', '#98DF8A', '#D62728', '#FF9896',
               '#9467BD', '#C5B0D5', '#8C564B', '#C49C94',
               '#E377C2', '#F700C2', '#7F7F7F', '#C7C7C7',
               '#BCBD22', '#DBDB8D', '#17BECF', '#9EDAE5')
show_col(tableau20)

tableau20_dark <- c('#1F77B4', '#FF7F0E',
                    '#2CA02C', '#D62728',
                    '#9467BD', '#8C564B',
                    '#F700C2', '#7F7F7F',
                    '#BCBD22', '#17BECF')
show_col(tableau20_dark)

tableau20_light <- c('#AEC7E8', '#FFBB78',
                     '#98DF8A', '#FF9896',
                     '#C5B0D5', '#C49C94',
                     '#E377C2', '#C7C7C7',
                     '#DBDB8D', '#9EDAE5')
show_col(tableau20_light)


print(dfSummary(mydata, max.distinct.values = 10, graph.col = TRUE), file = 'dfsummary.html')

print(descr(mydata_sub, stats=c("n.valid","sd","mean","med","min","max"),transpose = TRUE), file = 'dfsummary_descr.html')



ggpairs(data=mydata_sub, palette = "RdBu",title = "Within Academic Variables",mapping = ggplot2::aes(color = 'default'),
        upper = list(continuous = wrap("density", 
        alpha = 0.5), combo = "box_no_facet"),
        lower = list(continuous = wrap("smooth", alpha = 0.3, color="#F1CE63"), combo = wrap("dot_no_facet", alpha = 0.7)),
        diag = list(alpha=.7))

ggcorr(mydata_sub, palette = "RdBu", label = TRUE)

ggcorr(mydata_sub[1:10],
       label = TRUE,
       label_alpha = TRUE,
       label_round = 3,
       hjust = .85,
       layout.exp = 2,
       method = c("pairwise","pearson"),
       nbreaks = 3,
       palette="Pastel2")
display.brewer.all() 


ggcorr(mydata_sub, nbreaks = 10, palette = "RdYlBu", geom = "tile")

df <- inspect_cor(mydata_sub, with_col = "HouseholdIncome")
df %>%
  kable() %>%
  kable_styling() %>%
  save_kable(file = "table1.html", self_contained = T)

# Model 1
model1 <- lm(HouseholdIncome ~ College, data = mydata_sub)
summary(model1)
anova(model1)

model1_slope <- cor(mydata_sub$College, mydata_sub$HouseholdIncome) * (sd(mydata_sub$HouseholdIncome) / sd(mydata_sub$College))

cat("Model 1 Slope: ", cor(mydata_sub$College, mydata_sub$HouseholdIncome) * (sd(mydata_sub$HouseholdIncome) / sd(mydata_sub$College)))

slope <- cor(mydata_sub$College, mydata_sub$HouseholdIncome) * (sd(mydata_sub$HouseholdIncome) / sd(mydata_sub$College))

model1_rsquared <- summary(model1)$r.squared

cat("Model 1 R Squared: ", summary(model1)$r.squared)

intercept <- mean(mydata_sub$HouseholdIncome) - (model1_slope * mean(mydata_sub$College))


mydata_sub_sec5<-data.frame(mydata_sub)

mydata_sub_sec5$model_1_pred <- predict.lm(model1, mydata_sub)
mydata_sub_sec5$model_1_residual <- mydata_sub_sec5$HouseholdIncome - mydata_sub_sec5$model_1_pred
mydata_sub_sec5$model_1_residual_sqrd <- mydata_sub_sec5$model_1_residual ^ 2
sum(mydata_sub_sec5$model_1_residual_sqrd)


# Sum of Squares Total
mydata_sub_sec5$model_1_mean_dev <-mydata_sub_sec5$HouseholdIncome - mean(mydata_sub_sec5$HouseholdIncome)
mydata_sub_sec5$model_1_mean_dev_sq <- mydata_sub_sec5$model_1_mean_dev ^ 2
sum(mydata_sub_sec5$model_1_mean_dev_sq)

# Sum of Squares due to Regression
mydata_sub_sec5$model_1_yhat_devbar <- mydata_sub_sec5$model_1_pred - mean(mydata_sub_sec5$HouseholdIncome)
mydata_sub_sec5$model_1_yhat_dev_sq <- mydata_sub_sec5$model_1_yhat_devbar ^ 2
sum(mydata_sub_sec5$model_1_yhat_dev_sq)

sum(mydata_sub_sec5$model_1_yhat_dev_sq) / sum(mydata_sub_sec5$model_1_mean_dev_sq)


# Model 2
model2 <- lm(HouseholdIncome ~ College + Insured, data = mydata_sub)
summary(model2)
anova(model2)
summary(model2)$r.squared - summary(model1)$r.squared 

# Model 3
model3 <- lm(HouseholdIncome ~ College + Insured + Smokers, data = mydata_sub)
summary(model3)
anova(model3)
summary(model3)$r.squared

# Model 4
model4 <- lm(HouseholdIncome ~ College + Insured + Smokers + PhysicalActivity, data = mydata_sub)
summary(model4)
anova(model4)
summary(model4)$r.squared

# Model 5
model5 <- lm(HouseholdIncome ~ College + Insured + Smokers + PhysicalActivity + TwoParents, data = mydata_sub)
summary(model5)
anova(model5)
summary(model5)$r.squared

# Model 6
model6 <- lm(HouseholdIncome ~ College + Insured + Smokers + PhysicalActivity + TwoParents + HeavyDrinkers, data = mydata_sub)
summary(model6)
anova(model6)
summary(model6)$r.squared

# Model 7
model7 <- lm(HouseholdIncome ~ College + Insured + Smokers + PhysicalActivity + TwoParents + HeavyDrinkers + Obese, data = mydata_sub)
summary(model7)
anova(model7)
summary(model7)$r.squared

# Model 8
model8 <- lm(HouseholdIncome ~ College + Insured + Smokers + PhysicalActivity + TwoParents + HeavyDrinkers + Obese + HighSchool, data = mydata_sub)
summary(model8)
anova(model8)
summary(model8)$r.squared

options(show.signif.stars=TRUE)

# Create table for Section 7
section7_table <- data.frame()
data.frame(section7_table, stringsAsFactors = TRUE)
Features <- c("College",
              "College + Insured", 
              "College + Insured + Smokers", 
              "College + Insured + Smokers + PhysicalActivity ",
              "College + Insured + Smokers + PhysicalActivity + TwoParents",
              "College + Insured + Smokers + PhysicalActivity + TwoParents + HeavyDrinkers",
              "College + Insured + Smokers + PhysicalActivity + TwoParents + HeavyDrinkers + Obese",
              "College + Insured + Smokers + PhysicalActivity + TwoParents + HeavyDrinkers + Obese + Highschool")

Features_R2 <- c(summary(model1)$r.squared, summary(model2)$r.squared, summary(model3)$r.squared, summary(model4)$r.squared, summary(model5)$r.squared, summary(model6)$r.squared, summary(model7)$r.squared, summary(model8)$r.squared)

Features_R2_Adjusted <- c(summary(model1)$adj.r.squared, summary(model2)$adj.r.squared, summary(model3)$adj.r.squared, summary(model4)$adj.r.squared, summary(model5)$adj.r.squared, summary(model6)$adj.r.squared, summary(model7)$adj.r.squared, summary(model8)$adj.r.squared)

# Join the variables to create a data frame
section7_table <- data.frame(Features, Features_R2, Features_R2_Adjusted)

# Create Kable Table
section7_table %>%
  kable(digits=4) %>%
  kable_styling() %>%
  save_kable(file = "section7_table.html", self_contained = T)


# Section 8
sec8_model <- lm(HouseholdIncome ~ College + Smokers, data = mydata_sub)
summary(sec8_model)
anova(sec8_model)

# Section 8 v2
sec8_model_v2 <- lm(HouseholdIncome ~ College + TwoParents + NonWhite, data = mydata_sub)
summary(sec8_model_v2)
anova(sec8_model_v2)

plt1 <- ggplot(mydata_sub, aes_string(x=names(mydata_sub)[3], y = mydata_sub$HouseholdIncome)) +
  geom_point(color="#FFBB78", size=4, alpha=0.6) +
  geom_hline(yintercept=0, size=0.06) + 
  geom_smooth(method=lm, alpha=0.35, color="#FF9896", fill="#FF9896") +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(x = names(mydata_sub)[3], y = "Household Income ($)")
print(plt1)

plt2 <- ggplot(mydata_sub, aes_string(x=names(mydata_sub)[4], y = mydata_sub$HouseholdIncome)) +
  geom_point(color="#FFBB78", size=4, alpha=0.6) +
  geom_hline(yintercept=0, size=0.06) + 
  geom_smooth(method=lm, alpha=0.35, color="#98DF8A", fill="#98DF8A") +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(x = names(mydata_sub)[4], y = "Household Income ($)")
print(plt2)

grid.arrange(plt1, plt2, ncol=2)


plt3 <- ggplot(mydata_sub, aes_string(x=names(mydata_sub)[3], y = mydata_sub$HouseholdIncome)) +
  geom_point(color="#FFBB78", size=4, alpha=0.6) +
  geom_hline(yintercept=0, size=0.06) + 
  geom_smooth(method=lm, alpha=0.35, color="#FF9896", fill="#FF9896") +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(x = names(mydata_sub)[3], y = "Household Income ($)")
print(plt3)

plt4 <- ggplot(mydata_sub, aes_string(x=names(mydata_sub)[7], y = mydata_sub$HouseholdIncome)) +
  geom_point(color="#FFBB78", size=4, alpha=0.6) +
  geom_hline(yintercept=0, size=0.06) + 
  geom_smooth(method=lm, alpha=0.35, color="#98DF8A", fill="#98DF8A") +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(x = names(mydata_sub)[7], y = "Household Income ($)")
print(plt4)

plt5 <- ggplot(mydata_sub, aes_string(x=names(mydata_sub)[9], y = mydata_sub$HouseholdIncome)) +
  geom_point(color="#FFBB78", size=4, alpha=0.6) +
  geom_hline(yintercept=0, size=0.06) + 
  geom_smooth(method=lm, alpha=0.35, color="#AEC7E8", fill="#AEC7E8") +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(x = names(mydata_sub)[9], y = "Household Income ($)")
print(plt5)

grid.arrange(plt3, plt4, plt5, ncol=3)


# Run Combination of Variables to see highest adjust r square
require(MuMIn)

globalmodel <- lm(HouseholdIncome ~ + HighSchool +College + Smokers + PhysicalActivity + Obese + NonWhite + HeavyDrinkers + TwoParents + Insured, data = mydata_sub)

combinations <- dredge(globalmodel, m.lim = c(NA, 2), extra = c("adjR^2", F = function(x)
  summary(x)$fstatistic[[1]]))

# options(na.action = "na.omit")
# options(na.action = "na.fail")
print(combinations)

coefTable(combinations)

coefTable(combinations)[1]

combinations %>%
  kable(digits = 4) %>%
  kable_styling() %>%
  save_kable(file = "combinations3.html", self_contained = T)



combinations <- dredge(globalmodel, m.lim = c(NA, 3), extra = c("R^2", AdjustedRSqrd = function(x)
  summary(x)$adj.r.squared[[1]]))

# options(na.action = "na.omit")
# options(na.action = "na.fail")
print(combinations)

coefTable(combinations)






summary(globalmodel)$r.squared

print(inspect_cor(mydata_sub, method = "pearson", with_col = NULL,
                  alpha = 0.05))

inspect_cor(mydata_sub, method = "pearson", with_col = NULL,
            alpha = 0.05)
show_plot(x)

Plot(ab_cir, percent_BF, enhance=TRUE)

ScatterPlot(ab_cir,percent_BF)

cr_brief(data=d, ab_cir, percent_BF)

y<-mydata$percent_BF
age<-mydata$Age
wgt<-mydata$Weight
hgt<-mydata$Height
neck<-mydata$neck_cir
chest<-mydata$chest_cir
ab<-mydata$ab_cir
hip<-mydata$hip_cir
thgh<-mydata$thigh_cir
knee<-mydata$knee_cir
ankl<-mydata$ankle_cir
bi<-mydata$bicep_cir
arm<-mydata$forearm_cir
wrst<-mydata$wrist_cir


cor(mydata)

fit1<-lm(y~ab)
summary(fit1)
anova(fit1)

fit2<-lm(y~ab+wgt)
summary(fit2)
anova(fit2)

fit_all<-lm(y~age+wgt+hgt+neck+chest+ab+hip+thgh+knee+ankl+bi+arm+wrst)
summary(fit_all)
anova(fit_all)


data <- data.frame(mydata)

mydata<-data.frame(Manatee_Data)
data <- data.frame(mydata)


year<-mydata$YEAR
codeyear<-mydata$CODEYEAR
boats<-mydata$BOATS
deaths<-mydata$DEATHS


plot(x=boats, y=deaths)
cor(x=boats,y=deaths)

abline(lm (deaths~boats))
myfit<-lm(deaths~boats)
myfit
summary(myfit)

anova(myfit)

library(lessR)

style("green")

ScatterPlot(YEAR, DEATHS)
cr.brief(DEATHS, YEAR, data=mydata)
myfit2<-reg(DEATHS~YEAR)
myfit2


Plot(CODEYEAR, DEATHS)
myfit3<-reg(DEATHS~CODEYEAR)
myfit3


myfit4<-reg(DEATHS~BOATS + CODEYEAR)
myfit4

library(scales)
show_col(tableau_color_pal('Tableau 20')(20))

library(ggplot2)
library(ggthemes)

p2 <- ggplot(mtcars, aes(x = wt, y = mpg, colour = factor(gear))) +
  geom_point() +
  ggtitle("Cars")

# Economist theme
# A theme that approximates the style of plots in The Economist magazine.
palettes <- ggthemes_data[["tableau"]][["color-palettes"]][["regular"]]
for (palname in names(palettes)) {
  pal <- tableau_color_pal(palname)
  max_n <- attr(pal, "max_n")
  show_col(pal(max_n))
  title(main = palname)
}

p2 <- p2 + scale_colour_tableau(palette = "Tableau 10")
ggplotly(p2)

p2 + scale_colour_tableau()

ggpairs(data=data, title = "Within Academic Variables")

scale_color_tablea

d <- data.frame(mydata)


mydata<-data.frame(bodyfat)
d <- data.frame(mydata)


library(lessR)

Plot(ab_cir, percent_BF, enhance=TRUE)

ScatterPlot(ab_cir,percent_BF)

cr_brief(data=d, ab_cir, percent_BF)

style("darkgreen")
style()
myfit2<-reg(percent_BF~ab_cir)
myfit2

set(theme=c("blue", "gray", "rose", "green", "gold", "red"),
    trans.pts=.5)

style(
  theme=c("colors", "lightbronze", "dodgerblue", "darkred", "gray",
          "gold", "darkgreen", "blue", "red", "rose", "green", "purple",
          "sienna", "brown", "orange", "white"))

theme_get()

showColors



library(summarytools)

install.packages("summarytools")

mydata <- data.frame(ames_housing_data)

str(mydata)
head(mydata)
names(mydata)

data.types <- data.frame(Type = c('Nominal', 'Ordinal', 'Discrete', 'Continious', 'Identifiers'), Count = c(23, 23, 14, 20, 2))

p <- ggplot(data.types, aes(x = Type, y = Count)) +
  labs(title = "Ames Data Set: Data Types", size = 18) +
  theme(legend.position = "none", axis.text.x = element_text(size = 18), axis.text.y = element_text(size = 18), axis.title = element_text(size = 18, face = "bold"), plot.title = element_text(size = 18, face = "bold")) +
  xlab("Data Type") +
  ylab("Data Count") +
  geom_col(aes(fill = Type), width = 0.5)
p

p <- ggplot(mydata, aes(x=factor(BldgType))) +
  labs(title = "Ames Data Set: Buiding Types", size = 18) +
  theme(legend.position = "none", axis.text.x = element_text(size = 18), axis.text.y = element_text(size = 18), axis.title = element_text(size = 18, face = "bold"), plot.title = element_text(size = 18, face = "bold")) +
  xlab("Building Type") +
  ylab("Building Count") +
  stat_count(aes(label=..count..), vjust=-.5, 
             geom="text", position="identity") +
  geom_bar(stat="count", width=0.5, fill="steelblue")
p

summary(mydata)
hist(mydata$SalePrice)


str(building.info)

Plot(Age, percent_BF, enhance=TRUE)
Plot(Weight, percent_BF, data=mydata, enhance=TRUE)
Plot(ab_cir, percent_BF, enhance=FALSE)

Histogram(Age,enhance=TRUE)
Histogram(mydata)

SummaryStats(mydata)

BarChart(Weight)

view(freq(mydata$agegroup, plain.ascii = FALSE, style = "rmarkdown"))
view(dfSummary(mydata, plain.ascii = FALSE, style = "grid", 
          graph.magnif = 0.75, valid.col = FALSE, tmp.img.dir = "/tmp"))

view(dfSummary(mydata))
view(dfSummary(mydata), results='asis', style='grid')
view(freq(mydata))
view(descr(mydata))
print(dfSummary(mydata), file = '~/Documents/tobacco_summary.html')

print(dfSummary(mydata$SalePrice), method = 'viewer') 

view(ctable(mydata$GarageType, mydata$BldgType))

view(freq(mydata_filtered_20$SubClass))

view(skim(mydata))

skim(mydata)

skim(mydata_filtered_20$SubClass) %>%
  summary()

skim(mydata_filtered_20, SubClass)

view(mydata %>%
  skim())

install.packages("devtools")
library(devtools)
devtools::install_github("ropensci/visdat")
library(visdat)

vis_miss(mydata_filtered_20)
vis_dat(mydata_filtered_20)

#First install devtools to allow you to install inspectdf from github
#install.packages("devtools")
library(devtools)

#install and load the package - https://github.com/alastairrushworth/inspectdf

#devtools::install_github("alastairrushworth/inspectdf")
library(inspectdf)

#install.packages("tidyverse")
library(tidyverse)

#install.packages("readr")
library(readr)

inspect_types(mydata, show_plot = TRUE)
inspect_mem(mydata, show_plot = TRUE)

inspect_cor(mydata, method = "pearson", with_col = NULL,
            alpha = 0.05)
inspect_num(mydata, df2 = NULL, breaks = 20, include_int = TRUE)

data("starwars", package = "dplyr")

# categorical plot
x <- inspect_types(mydata)
show_plot(x)

x <- inspect_na(mydata_filtered_20)
show_plot(x)

x <- inspect_types(mydata)
show_plot(x)

inspect_types() summary of column types
inspect_mem() summary of memory usage of columns
inspect_na() columnwise prevalence of missing values
inspect_cor() correlation coefficients of numeric columns
inspect_imb() feature imbalance of categorical columns
inspect_num() summaries of numeric columns
inspect_cat() summaries of categorical columns

library(tidyr)
library(ggplot2)

mtcars %>%
  gather(-mydata$SalePrice, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = mpg)) +
  facet_wrap(~ var, scales = "free") +
  geom_point() +
  stat_smooth()


library(DataExplorer)
DataExplorer::create_report(mydata)

library(dplyr)
mydata_filtered <- filter(mydata, BldgType == "1Fam")
mydata_filtered_defined <- mydata_filtered %>% filter(SalePrice %in% (50000:500000))
summary(mydata_filtered_defined$SalePrice)
hist(mydata_filtered_defined$SalePrice)


mydata_filtered_20 <- select(mydata_filtered, SalePrice, TotalFloorSF, HouseAge, QualityIndex, 
                             TotalBath, SubClass, LotArea, Utilities, Neighborhood, 
                             BldgType, HouseStyle, ExterCond, Exterior1, ExterQual, 
                             Foundation, BsmtCond, KitchenQual, Functional, GarageType,
                             PoolArea)

mydata_filt_def_20_plus_SalePrice <- select(mydata_filtered_defined, SalePrice, TotalFloorSF, HouseAge, QualityIndex, TotalBath, SubClass, LotArea, Utilities, Neighborhood, BldgType, HouseStyle, ExterCond, Exterior1, ExterQual, Foundation, BsmtCond, KitchenQual, Functional, GarageType, Condition1, PoolArea)

x <- inspect_na(mydata_filt_def_20_plus_SalePrice)
show_plot(x)

print(dfSummary(mydata_filt_def_20_plus_SalePrice), file = 'dfsummary.html')

x <- inspect_num(mydata_filt_def_20_plus_SalePrice)
show_plot(x)

x <- inspect_cat(mydata_filt_def_20_plus_SalePrice)
show_plot(x)

as.data.frame(table(unlist(mydata_filt_def_20_plus_SalePrice$LotArea)))
hist(mydata_filt_def_20_plus_SalePrice$LotArea)

mydata_filt_def_10_plus_SalePrice <- select(mydata_filt_def_20_plus_SalePrice, SalePrice, TotalFloorSF, HouseAge, QualityIndex, Neighborhood, BldgType, HouseStyle, Exterior1, Foundation, GarageType, Condition1)

mydata_filt_def_10_plus_logSalePrice <- select(mydata_filtered, SalePrice, TotalFloorSF, HouseAge, QualityIndex, Neighborhood, BldgType, HouseStyle, Exterior1, Foundation, GarageType, Condition1, logSalePrice)

hist(mydata_filt_def_10_plus_logSalePrice$SalePrice)
hist(mydata_filt_def_10_plus_logSalePrice$logSalePrice)


ggplot(data = mydata_filt_def_10_plus_SalePrice, aes(x=Foundation, y=Neighborhood, fill=Neighborhood)) + 
  geom_tile()

ggplot(mydata_filt_def_10_plus_SalePrice, aes(y = SalePrice)) +
  geom_boxplot(outlier.colour = "steelblue", fill = "forestgreen", outlier.shape = 16,
               outlier.size = 2, notch = FALSE) +
  coord_flip() +
  labs(x = "Houses", y = "Sale Price ($)") +
  scale_y_continuous(labels = scales::dollar_format())

ggplot(mydata_filt_def_10_plus_logSalePrice, aes(y = logSalePrice)) +
  geom_boxplot(outlier.colour = "steelblue", fill = "forestgreen", outlier.shape = 16,
               outlier.size = 2, notch = FALSE) +
  coord_flip() +
  labs(x = "Houses", y = "Sale Price ($)") +
  scale_y_continuous(labels = scales::dollar_format())

ggplot(mydata_filt_def_10_plus_SalePrice) +
  geom_boxplot(aes(x = Neighborhood, y = SalePrice, fill = Neighborhood)) +
  theme(legend.position = "none") +
  coord_flip() +
  labs(x = "Neighborhoods", y = "Sale Price ($)") +
  scale_y_continuous(labels = scales::dollar_format())

ggplot(mydata_filt_def_10_plus_SalePrice) +
  geom_boxplot(aes(x = HouseStyle, y = SalePrice, fill = HouseStyle)) +
  theme(legend.position = "none") +
  coord_flip() +
  labs(x = "House Style", y = "Sale Price ($)") +
  scale_y_continuous(labels = scales::dollar_format())

age_saleprice_plot <- ggplot(mydata_filt_def_10_plus_SalePrice) +
  geom_point(aes(HouseAge, SalePrice), color = "forestgreen") +
  geom_smooth(aes(HouseAge, SalePrice), method = "gam", formula = y ~ splines::bs(x, 3), color = "gold") +
  labs(x = "House Age", y = "Sale Price ($)") +
  scale_y_continuous(labels = scales::dollar_format())

floorsf_saleprice_plot <- ggplot(mydata_filt_def_10_plus_SalePrice) +
  geom_point(aes(TotalFloorSF, SalePrice), color = "forestgreen") +
  geom_smooth(aes(TotalFloorSF, SalePrice), method = "auto", color = "gold") +
  labs(x = "Total Floor Square Feet", y = "Sale Price ($)") +
  scale_y_continuous(labels = scales::dollar_format())

require(gridExtra)
grid.arrange(age_saleprice_plot, floorsf_saleprice_plot, ncol=2)

age_logsaleprice_plot <- ggplot(mydata_filt_def_10_plus_logSalePrice) +
  geom_point(aes(HouseAge, logSalePrice), color = "forestgreen") +
  geom_smooth(aes(HouseAge, logSalePrice), method = "gam", formula = y ~ splines::bs(x, 3), color = "gold") +
  labs(x = "House Age", y = "Sale Price ($)") +
  scale_y_continuous(labels = scales::dollar_format())

floorsf_logsaleprice_plot <- ggplot(mydata_filt_def_10_plus_logSalePrice) +
  geom_point(aes(TotalFloorSF, logSalePrice), color = "forestgreen") +
  geom_smooth(aes(TotalFloorSF, logSalePrice), method = "auto", color = "gold") +
  labs(x = "Total Floor Square Feet", y = "Sale Price ($)") +
  scale_y_continuous(labels = scales::dollar_format())

require(gridExtra)
grid.arrange(age_logsaleprice_plot, floorsf_logsaleprice_plot, ncol=2)

ggplot(mydata_filt_def_10_plus_SalePrice, aes(x = TotalFloorSF, y = SalePrice)) +
  geom_point(shape=1) +
  geom_smooth(aes(Neighborhood, SalePrice), method = "auto", color = "gold") +
  labs(x = "Total Floor Square Feet", y = "Sale Price ($)") +
  scale_y_continuous(labels = scales::dollar_format())


saleprice_ggplot <- ggplot(mydata_filt_def_10_plus_logSalePrice) +
                      geom_histogram(aes(x = SalePrice, fill = SalePrice), fill = "steelblue") +
                      theme(legend.position = "none") +
                      labs(x = "Sale Price ($)", y = "Houses") +
                      scale_x_continuous(labels = scales::dollar_format())

logsaleprice_ggplot <- ggplot(mydata_filt_def_10_plus_logSalePrice) +
                        geom_histogram(aes(x = logSalePrice, fill = logSalePrice), fill = "forestgreen") +
                        theme(legend.position = "none") +
                        labs(x = "Log Sale Price ($)", y = "Houses") +
                        scale_x_continuous(labels = scales::dollar_format())

require(gridExtra)
grid.arrange(saleprice_ggplot, logsaleprice_ggplot, ncol=2)

ggplotly(logsaleprice_ggplot)

mydata_filt_def_20_plus_SalePrice %>%
  select_if(is.numeric) %>%
  cor() %>%
  heatmap()


# Overlay of Sale Price and Log Sale Price Histogram
ggplot(mydata_filt_def_10_plus_logSalePrice) +
  geom_histogram(aes(x = SalePrice, fill = SalePrice), fill = "steelblue") +
  geom_histogram(aes(x = logSalePrice, fill = logSalePrice), fill = "forestgreen") +
  theme(legend.position = "none") +
  labs(x = "Sale Price ($)", y = "Houses") +
  scale_x_continuous(labels = scales::dollar_format())



ggcorr(mydata_filt_def_20_plus_SalePrice, palette = "RdBu", label = TRUE)

x12 <- ggplot(mydata_filt_def_10_plus_logSalePrice) +
  geom_boxplot(aes(x = Neighborhood, y = logSalePrice, fill = Neighborhood)) +
  theme(legend.position = "none") +
  coord_flip() +
  labs(x = "Neighborhoods", y = "Log Sale Price ($)") +
  scale_y_continuous(labels = scales::dollar_format())

x12 <- ggplot(mydata_filt_def_10_plus_logSalePrice, aes(x=HouseStyle, y=logSalePrice, fill=HouseStyle)) + 
  geom_boxplot(alpha=0.3) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=2, color="gold", fill="gold")+
  theme(legend.position="none")+
  ggtitle("Figure 4 Boxplot of SalePrice by MSZoning")+
  theme(plot.title = element_text(hjust = 0.5))

ggplotly(x12)

ggplot(mydata_filt_def_10_plus_logSalePrice) +
  geom_boxplot(aes(x = HouseStyle, y = logSalePrice, fill = HouseStyle)) +
  theme(legend.position = "none") +
  coord_flip() +
  labs(x = "House Style", y = "Sale Price ($)") +
  scale_y_continuous(labels = scales::dollar_format())

mydata_filt_def_3_plus_logSalePrice <- select(mydata_filt_def_10_plus_logSalePrice, TotalFloorSF, Neighborhood, QualityIndex, logSalePrice)

ggcorr(mydata_filt_def_3_plus_logSalePrice, palette = "RdBu", label = TRUE)
ggcorr(mydata, palette = "RdBu", label = TRUE)


saleprice_ggplot <- ggplot(mydata_filt_def_10_plus_logSalePrice) +
  geom_histogram(aes(x = SalePrice, fill = SalePrice), fill = "steelblue") +
  theme(legend.position = "none") +
  labs(x = "Sale Price ($)", y = "Houses") +
  scale_x_continuous(labels = scales::dollar_format())



totalfloor_logsaleprice_plot <- ggplot(mydata_filt_def_3_plus_logSalePrice) +
  geom_point(aes(TotalFloorSF, logSalePrice), color = "steelblue") +
  geom_smooth(aes(TotalFloorSF, logSalePrice), method = "gam", formula = y ~ splines::bs(x, 3), color = "gold") +
  labs(x = "Total Floor Sq. Ft.", y = "Log Sale Price ($)") +
  scale_y_continuous(labels = scales::dollar_format())

quality_logsaleprice_plot <- ggplot(mydata_filt_def_3_plus_logSalePrice) +
  geom_point(aes(QualityIndex, logSalePrice), color = "steelblue") +
  geom_smooth(aes(QualityIndex, logSalePrice), method = "auto", color = "gold") +
  labs(x = "Quality Index", y = "Log Sale Price ($)") +
  scale_y_continuous(labels = scales::dollar_format())

require(gridExtra)
grid.arrange(quality_logsaleprice_plot, totalfloor_logsaleprice_plot, ncol=2)

neighborhood_logsaleprice_plot <- ggplot(mydata_filt_def_3_plus_logSalePrice) +
  geom_point(aes(Neighborhood, logSalePrice), color = "steelblue") +
  geom_smooth(aes(Neighborhood, logSalePrice), method = "auto", color = "gold") +
  labs(x = "Neighborhood", y = "Log Sale Price ($)") +
  scale_y_continuous(labels = scales::dollar_format())

options(scipen=999)

mydata$LotArea
mydata$SalePrice
mydata$PoolArea
mydata$HouseAge
mydata$QualityIndex
mydata$TotalBath
mydata$SubClass
mydata$LotFrontage
mydata$Utilities
mydata$Neighborhood
mydata$BldgType
mydata$HouseStyle
mydata$ExterCond
mydata$Exterior1
mydata$ExterQual
mydata$Foundation
mydata$BsmtQual
mydata$KitchenQual
mydata$Functional
mydata$GarageType
mydata$PoolQC


mydata

str(mydata)
head(mydata)
names(mydata)
mydata$TotalFloorSF <- mydata$FirstFlrSF + mydata$SecondFlrSF
mydata$HouseAge <- mydata$YrSold - mydata$YearBuilt
mydata$QualityIndex <- mydata$OverallQual * mydata$OverallCond
mydata$logSalePrice <- log(mydata$SalePrice)
mydata$price_sqft <- mydata$SalePrice/mydata$TotalFloorSF
mydata$TotalBath <- mydata$FullBath + (mydata$HalfBath / 2)
summary(mydata$price_sqft)
hist(mydata$price_sqft)
subdat <- subset(mydata, select=c("TotalFloorSF","HouseAge","QualityIndex",
                                  "price_sqft", "SalePrice","LotArea",
                                  "BsmtFinSF1","Neighborhood","HouseStyle",
                                  "LotShape","OverallQual","logSalePrice",
                                  "TotalBsmtSF","HouseStyle"))

str(subdat)


subdatnum <- subset(mydata, select=c("TotalFloorSF","HouseAge","QualityIndex",
                                     "SalePrice","LotArea","OverallQual","logSalePrice"))
#####################################################################
######################### Assignment 1 ##############################
#####################################################################

#################################################################
################## univariate EDA ##############################
###############################################################
require(ggplot2)
ggplot(subdat) +
  geom_bar( aes(LotShape) ) +
  ggtitle("Number of houses per Lotshape") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(subdat, aes(x=SalePrice)) + 
  geom_histogram(color="black", binwidth= 10000) +
  labs(title="Distribution of Sale Price") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(subdat, aes(x=TotalFloorSF)) + 
  geom_histogram(color="black", binwidth= 100) +
  labs(title="Distribution of TotalFloorSF") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(subdat, aes(x=QualityIndex)) + 
  geom_histogram(color="black", binwidth= 10) +
  labs(title="Distribution of QualityIndex") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
#######################################################################
########### bivariate EDA ########################################
###################################################################
ggplot(subdat, aes(x=TotalFloorSF, y=QualityIndex)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of Total Floor SF vs QualityIndex") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(subdat, aes(x=TotalFloorSF, y=HouseAge)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of Total Floor SF vs HouseAge") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(subdat, aes(x=LotShape, y=HouseAge)) + 
  geom_boxplot(fill="blue") +
  labs(title="Distribution of HouseAge") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

############################################################
################ model focussed EDA #######################
###########################################################

ggplot(subdat, aes(x=TotalFloorSF, y=SalePrice)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Sale Price vs Total Floor SF") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) +
  geom_smooth(method=lm, se=FALSE)  ## method=lm, se=FALSE ###

ggplot(subdat, aes(x=QualityIndex, y=SalePrice)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of Sale Price vs QualityIndex") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) 

ggplot(subdat, aes(x=LotShape, y=SalePrice)) + 
  geom_boxplot(fill="blue") +
  labs(title="Distribution of Sale Price") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

#####################################################################
############# EDA for multiple variables ###########################
##################################################################
require(GGally)
ggpairs(mydata)

require(lattice)
pairs(subdat, pch = 21)

require(corrplot)
mcor <- cor(subdatnum)
corrplot(mcor, method="shade", shade.col=NA, tl.col="black",tl.cex=0.5)


#####################################################################
############# Define the sample data ###########################
##################################################################

subdat2 <- subdat[which(subdat$TotalFloorSF < 4000),]

###################################################################
##################  Assignment 2  ################################
#################################################################

attach(subdat2)

ggplot(subdat2, aes(x=TotalFloorSF, y=SalePrice)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Sale Price vs Total Floor SF") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) + 
  geom_smooth(method=lm,se=FALSE)  ## method=lm, se=FALSE ###

ggplot(subdat2, aes(x=QualityIndex, y=SalePrice)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of Sale Price vs QualityIndex") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) +
  geom_smooth(method=lm)  ## method=lm, se=FALSE ###



# 3D Scatterplot with Coloring and Vertical Lines
# and Regression Plane 
library(scatterplot3d)
attach(subdat2)
s3d <-scatterplot3d(TotalFloorSF,QualityIndex,SalePrice,pch=16, 
                    highlight.3d=TRUE,type="h", main="3D Scatterplot")
fit <- lm(SalePrice ~ TotalFloorSF + QualityIndex) 
s3d$plane3d(fit)

library(Rcmdr)
attach(subdat2)
scatter3d(SalePrice,TotalFloorSF,QualityIndex)

############## fitting a SLR ###################################

SLRresult = lm(SalePrice ~ TotalFloorSF, data=subdatnum)#subdat2
anova(SLRresult)
summary(SLRresult)
par(mfrow=c(1,1))  # visualize four graphs at once


pred <- as.data.frame(predict(SLRresult,subdatnum,interval="prediction"))
str(pred)
head(pred)
subdatnum <- cbind(subdatnum,pred)
str(subdatnum)
head(subdatnum)
subdatnum <- subset( subdatnum, select = -lwr)
subdatnum <- subset( subdatnum, select = -upr)
library(reshape)
subdatnum <- rename(subdatnum, c(fit="fitSLR"))


############## fitting a MLR ###################################

MLRresult = lm(SalePrice ~ TotalFloorSF+OverallQual, data=subdatnum)
anova(MLRresult)
summary(MLRresult)
par(mfrow=c(2,2))  # visualize four graphs at once
plot(MLRresult)

pred <- as.data.frame(predict(MLRresult,subdatnum,interval="prediction"))
str(pred)
head(pred)
subdatnum <- cbind(subdatnum,pred)
subdatnum <- subset( subdatnum, select = -lwr)
subdatnum <- subset( subdatnum, select = -upr)
str(subdatnum)
head(subdatnum)
subdatnum <- rename(subdatnum, c(fit="fitMLR"))
subdatnum$res <- subdatnum$SalePrice - subdatnum$fitMLR

head(subdatnum)

###################################################################
##################### Assignment 3  ################################
#################################################################

################  MAE calculation ###################################
MLRresult = lm(SalePrice ~ TotalFloorSF+OverallQual, data=subdat)
anova(MLRresult)
summary(MLRresult)

par(mfrow=c(2,2))  # visualize four graphs at once
plot(MLRresult)

pred <- as.data.frame(predict(MLRresult,subdat))
names(pred)
library(reshape)
pred <- rename(pred, c("predict(MLRresult, subdat)" = "prd"))
subdat$pred <- pred$prd
subdat$res <- subdat$SalePrice - subdat$pred
subdat$absres <- abs(subdat$res)
MAE <- mean(subdat$absres)
MAE

require(ggplot2)
ggplot(subdat, aes(x=OverallQual, y=res)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Residual vs OverallQual") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) + 
  geom_smooth(method=lm,se=FALSE)  ## method=lm, se=FALSE ###

################################################################
############### Log Transformation #############################

MLRLogresult = lm(logSalePrice ~ TotalFloorSF+OverallQual, data=subdatnum)
anova(MLRLogresult)
summary(MLRLogresult)
par(mfrow=c(2,2))  # visualize four graphs at once
plot(MLRLogresult)

pred <- as.data.frame(predict(MLRLogresult,subdatnum,interval="prediction"))
str(pred)
head(pred)
subdatnum <- cbind(subdatnum,pred)
subdatnum <- subset( subdatnum, select = -lwr)
subdatnum <- subset( subdatnum, select = -upr)
str(subdatnum)
head(subdatnum)
subdatnum <- rename(subdatnum, c(fit="fitMLRLog"))
subdatnum$reslog <- subdatnum$logSalePrice - subdatnum$fitMLRLog
MAE <- mean(abs(subdatnum$reslog))
MAE

head(subdatnum)

library(car)
vif(MLRLogresult)
par(mfrow=c(1,1))
influencePlot(MLRLogresult,	id.method="identify", main="Influence Plot", 
              sub="Circle size is proportial to Cook's Distance")


summary(inflm.MLRLog <- influence.measures(MLRLogresult))
dffitslog <- dffits(MLRLogresult)
subdatnum <- cbind(subdatnum,dffitslog)
str(subdatnum)

ggplot(subdatnum, aes(x=OverallQual, y=reslog)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Residual vs OverallQual") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) + 
  geom_smooth(method=lm,se=FALSE)  ## method=lm, se=FALSE ###

################ influential points removed  #######
subdatnum$absdf <- abs(subdatnum$dffitslog)
head(subdatnum)
subdatnuminf <- subdatnum[which(subdatnum$absdf < 0.064),]

MLRLogresult = lm(logSalePrice ~ TotalFloorSF+OverallQual, data=subdatnuminf)
anova(MLRLogresult)
summary(MLRLogresult)

############## analyze Neighborhood variable #########

require(ggplot2)
ggplot(subdat, aes(x=Neighborhood, y=SalePrice)) + 
  geom_boxplot(fill="blue") +
  labs(title="Distribution of Sale Price") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

library(plyr)
subdat1 <- ddply(subdat, .(Neighborhood), summarise, 
                 MAE = mean(absres))
subdat2 <- ddply(subdat, .(Neighborhood), summarise, 
                 MeanPrice = mean(SalePrice))
subdat3 <- ddply(subdat, .(Neighborhood), summarise, 
                 TotalPrice = sum(SalePrice))
subdat4 <- ddply(subdat, .(Neighborhood), summarise, 
                 TotalSqft = sum(TotalFloorSF))
subdat34 <- cbind(subdat3,subdat4)
subdat34$AvgPr_Sqft <- subdat34$TotalPrice/subdat34$TotalSqft

subdatall <- subdat1
subdatall$MeanPrice <- subdat2$MeanPrice
subdatall$AvgPr_Sqft <- subdat34$AvgPr_Sqft

require(ggplot2)
ggplot(subdatall, aes(x=AvgPr_Sqft, y=MeanPrice)) + 
  geom_point(color="blue", shape=1,size=3) +
  ggtitle("Scatter Plot") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

#### Clean up of the Neighborhood varaible  ########

subdat$NbhdGrp <-
  ifelse(subdat$price_sqft<=100, "grp1", 
         ifelse(subdat$price_sqft<=120, "grp2",
                ifelse(subdat$price_sqft<=140, "grp3",
                       "grp4"))) 

################ include categoriacl variable in the model #######

MLRresult = lm(SalePrice ~ TotalFloorSF+OverallQual+NbhdGrp, data=subdat)
anova(MLRresult)
summary(MLRresult)
pred <- as.data.frame(predict(MLRresult,subdat))
names(pred)
library(reshape)
pred <- rename(pred, c("predict(MLRresult, subdat)" = "prd"))
subdat$pred <- pred$prd
subdat$res <- subdat$SalePrice - subdat$pred
subdat$absres <- abs(subdat$res)
MAE <- mean(subdat$absres)
MAE

################# define dummy variables ###################

subdat$NbhdGrp1 <- 
  ifelse(subdat$NbhdGrp == "grp1", 1, 0)
subdat$NbhdGrp2 <- 
  ifelse(subdat$NbhdGrp == "grp2", 1, 0)
subdat$NbhdGrp3 <- 
  ifelse(subdat$NbhdGrp == "grp3", 1, 0)

MLRresult4 = lm(SalePrice ~ TotalFloorSF+OverallQual+NbhdGrp1+NbhdGrp2+NbhdGrp3, 
                data=subdat)
anova(MLRresult4)
summary(MLRresult4)



############################################################
############## assignment 5 #############################
#########################################################

# Set the seed on the random number generator so you get the same split every time that
# you run the code.
my.data <- subdat
set.seed(123)
my.data$u <- runif(n=dim(my.data)[1],min=0,max=1)

# Create train/test split;
train.df <- subset(my.data, u<0.70);
test.df  <- subset(my.data, u>=0.70);
names(train.df)

train.clean <- subset(train.df, select=c("TotalFloorSF","HouseAge",
                                         "OverallQual","LotArea","logSalePrice","BsmtFinSF1",
                                         "TotalBsmtSF","Style1","Style2"))

test.clean <- subset(test.df, select=c("TotalFloorSF","HouseAge",
                                       "OverallQual","LotArea","logSalePrice","BsmtFinSF1",
                                       "TotalBsmtSF","Style1","Style2"))


# Check your data split. The sum of the parts should equal the whole.
# Do your totals add up?
dim(my.data)[1]
dim(train.df)[1]
dim(test.df)[1]
dim(train.df)[1]+dim(test.df)[1]

train.clean <- na.omit(train.clean)
test.clean <- na.omit(test.clean)


# Define the upper model as the FULL model
upper.lm <- lm(logSalePrice ~ .,data=train.clean);
summary(upper.lm)

# Define the lower model as the Intercept model
lower.lm <- lm(logSalePrice ~ 1,data=train.clean);
summary(lower.lm)
# Need a SLR to initialize stepwise selection
sqft.lm <- lm(logSalePrice ~ TotalFloorSF,data=train.clean);
summary(sqft.lm)

# Note: There is only one function for classical model selection in R - stepAIC();
# stepAIC() is part of the MASS library.
# The MASS library comes with the BASE R distribution, but you still need to load it;
library(MASS)

# Call stepAIC() for variable selection

forward.lm <- stepAIC(object=lower.lm,scope=list(upper=upper.lm,lower=lower.lm),
                      direction=c('forward'));
summary(forward.lm)

backward.lm <- stepAIC(object=upper.lm,direction=c('backward'));
summary(backward.lm)

stepwise.lm <- stepAIC(object=sqft.lm,scope=list(upper=formula(upper.lm),lower=~1),
                       direction=c('both'));
summary(stepwise.lm)

junk.lm <- lm(logSalePrice ~ OverallQual + LotArea, data=train.clean)
summary(junk.lm)

library(car)
sort(vif(forward.lm),decreasing=TRUE)
sort(vif(backward.lm),decreasing=TRUE)
sort(vif(stepwise.lm),decreasing=TRUE)
sort(vif(junk.lm),decreasing=TRUE)

forward.test <- predict(forward.lm,newdata=test.clean);
backward.test <- predict(backward.lm,newdata=test.clean);
stepwise.test <- predict(stepwise.lm,newdata=test.clean);
junk.test <- predict(junk.lm,newdata=test.clean);

# Training Data
# Abs Pct Error
forward.pct <- abs(forward.lm$residuals)/train.clean$logSalePrice;
MAPE <- mean(forward.pct)
MAPE
backward.pct <- abs(backward.lm$residuals)/train.clean$logSalePrice;
MAPE <- mean(backward.pct)
MAPE
stepwise.pct <- abs(stepwise.lm$residuals)/train.clean$logSalePrice;
MAPE <- mean(stepwise.pct)
MAPE
junk.pct <- abs(junk.lm$residuals)/train.clean$logSalePrice;
MAPE <- mean(junk.pct)
MAPE

# Test Data
# Abs Pct Error
forward.testPCT <- abs(test.df$logSalePrice-forward.test)/test.df$logSalePrice;
MAPE <- mean(forward.testPCT)
MAPE
backward.testPCT <- abs(test.df$logSalePrice-backward.test)/test.df$logSalePrice;
MAPE <- mean(backward.testPCT)
MAPE
stepwise.testPCT <- abs(test.df$logSalePrice-stepwise.test)/test.df$logSalePrice;
MAPE <- mean(stepwise.testPCT)
MAPE
junk.testPCT <- abs(test.df$logSalePrice-junk.test)/test.df$logSalePrice;
MAPE <- mean(junk.testPCT)
MAPE


# Assign Prediction Grades training data;
forward.PredictionGrade <- ifelse(forward.pct<=0.10,'Grade 1: [0.0.10]',
                                  ifelse(forward.pct<=0.15,'Grade 2: (0.10,0.15]',
                                         ifelse(forward.pct<=0.25,'Grade 3: (0.15,0.25]',
                                                'Grade 4: (0.25+]')
                                  )					
)

forward.trainTable <- table(forward.PredictionGrade)
forward.trainTable/sum(forward.trainTable)


# Assign Prediction Grades test data;
forward.testPredictionGrade <- ifelse(forward.testPCT<=0.10,'Grade 1: [0.0.10]',
                                      ifelse(forward.testPCT<=0.15,'Grade 2: (0.10,0.15]',
                                             ifelse(forward.testPCT<=0.25,'Grade 3: (0.15,0.25]',
                                                    'Grade 4: (0.25+]')
                                      )					
)

forward.testTable <-table(forward.testPredictionGrade)
forward.testTable/sum(forward.testTable)
######################################################################
sub <- subset(subdat, select=c("TotalFloorSF","HouseAge",
                               "OverallQual","LotArea","logSalePrice","BsmtFinSF1",
                               "TotalBsmtSF","Style1","Style2"))

MLRresult1 = lm(logSalePrice ~ ., data=sub)

sub2 <- subset(subdat, select=c("TotalFloorSF","HouseAge",
                                "OverallQual","LotArea","logSalePrice","BsmtFinSF1",
                                "TotalBsmtSF"))

MLRresult2 = lm(logSalePrice ~ ., data=sub2)
anova(MLRresult1,MLRresult2)

anova(MLRresult1)
summary(MLRresult1)
par(mfrow=c(2,2))  # visualize four graphs at once
plot(MLRresult)

names(MLRresult)
head(MLRresult$df.residual)

inflm.MLRLog <- influence.measures(MLRresult)
names(inflm.MLRLog)
str(inflm.MLRLog)
inflmetrics <- as.data.frame(inflm.MLRLog$infmat)
dffit_df <- subset(inflmetrics, select= c(dffit))
sub$r1 <- row.names(sub)
dffit_df$r1 <- row.names(dffit_df)
subnew <- merge(sub, dffit_df, all=FALSE)

subnew <- subset(subnew, select= -c(r1))

subnew$absdffit <- abs(subnew$dffit)

subnewinf <- subnew[which(subnew$absdf < 0.064),]



MLRLogresult = lm(logSalePrice ~ TotalFloorSF+OverallQual+HouseAge+
                    LotArea+BsmtFinSF1+TotalBsmtSF+Style1+Style2,data=subnewinf)
anova(MLRLogresult)
summary(MLRLogresult)
par(mfrow=c(2,2))  # visualize four graphs at once
plot(MLRLogresult)


