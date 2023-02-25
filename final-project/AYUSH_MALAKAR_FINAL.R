library(ggplot2)
library(caret)
library(GGally)
library(ggthemes)
library(broom)
library(dplyr)
library(bindrcpp)
library(caTools)
library(rattle)
library(RColorBrewer)
library(nnet)
library(rpart.plot)
library(ggplot2)
library(`c`ar)
library(dplyr)
library(lattice)
library(tidyr)
library(caret)
library(MASS)
library(broom)
library(ROCR)
library(corrplot)
library(ggcorrplot)

cleve <- read.csv('C:/r-final-data/processed-cleavland.csv', na = "?", stringsAsFactors = FALSE, header = FALSE)
hung  <- read.csv('C:/r-final-data/processed-hungarian.csv', na = "?", stringsAsFactors = FALSE, header = FALSE)
swiss  <- read.csv('C:/r-final-data/processed-switzerland.csv', na = "?", stringsAsFactors = FALSE, header = FALSE)
va  <- read.csv('C:/r-final-data/processed-va.csv', na = "?", stringsAsFactors = FALSE, header = FALSE)
h <- rbind(cleve, hung, swiss, va)
# Data Cleaning and Transformation
# With str and summarizeColumns (Table 1), we noticed the following anomalies:
# 
#   The target feature, target had a cardinality of 5, which should be 2 since target was desiganted as the binary target feature.
# Ten of the 14 features contained missing values. Notably, the features Slope, CA and Thal had 309, 611 and 486 missing values, respectively.
# trestbps (resting blood pressure) and chol (serum cholestrol) contained several data entries with values of 0 which are not possible for these diagnostic tests.


names(h) <- c('age', 'sex', 'cp', 'trestbps', 'chol', 'fbs', 'restecg', 'thalach', 'exang', 'oldpeak', 'slope', 'ca', 'thal', 'target')
str(h)


summarizeColumns(h) %>% knitr::kable( caption =  'Feature Summary before Data Preprocessing')

# Fourthly, in the target column clinicians had graded patients as either having no heart disease (value of 0) or displaying various degrees of heart disease (values 1 to 4). 
# We chose to group the data into 2 categories of 'no heart disease' (value of 0) and 'displaying heart disease' (value of 1) so it became binary.
# 
# It was noted that a higher proportion of people were diagnosed with heart disease (Table2). Therefore, we may require additional parameter-tuning in building models to cater for such an unbalanced class.

h$target[h$target == 2] <- 1
h$target[h$target == 3] <- 1
h$target[h$target == 4] <- 1




h <- h[!is.na(h$trestbps),]
h <- h[!is.na(h$chol),]
h <- h[!is.na(h$restecg),]
h <- h[!is.na(h$oldpeak),]
h <- h[!is.na(h$fbs),]
h <- [!is.na(h$thal),]
h <- [!is.na(h$slope,)] 


sapply(h[sapply(h, is.character)], table)

h[, sapply(h, is.character)] <- lapply( h[, sapply(h, is.character )], factor) 

summarizeColumns(h) %>% kable( caption = 'Feature Summary before Data Preprocessing' )
heart_data2 <- na.omit(h)

h$sex<-as.factor(h$sex)
h$cp<-as.factor(h$cp)
h$fbs<-as.factor(h$fbs)
h$exang<-as.factor(h$exang)
h$restecg<-as.factor(h$restecg)
h$slope<-as.factor(h$slope)
h$thal<-as.factor(h$thal)
h$target<-as.factor(h$target)

str(h)
levels(h$sex)[levels(h$sex)==0] <- "Female"
levels(h$sex)[levels(h$sex)==1] <- "Male"
levels(h$fbs)[levels(h$fbs)==0] <- "Fasting Blood Sugar <= 120"
levels(h$fbs)[levels(h$fbs)==1] <- "Fasting Blood Sugar > 120"
levels(h$thal)[levels(h$thal)==3] <- "Normal"
levels(h$thal)[levels(h$thal)==6] <- "Fixed Defect"
levels(h$thal)[levels(h$thal)==7] <- "Reversible Defect Thalassemia"
levels(h$target)[levels(h$target)==0] <- "Heart Disease"
levels(h$target)[levels(h$target)==1] <- "Healthy"
levels(h$exang)[levels(h$exang)==1] <- "Exercise Induced Angina"
levels(h$exang)[levels(h$exang)==0] <- "No Exercise Induced Angina"
levels(h$cp)[levels(h$cp)==1] <- "Typical Angina"
levels(h$cp)[levels(h$cp)==2] <- "Atypical Angina"
levels(h$cp)[levels(h$cp)==3] <- "Non-Angina Pain"
levels(h$cp)[levels(h$cp)==4] <- "asymptomatic"
levels(h$restecg)[levels(h$restecg)==0] <- "Rest ECG 0"
levels(h$restecg)[levels(h$restecg)==1] <- "Rest ECG 1"
levels(h$restecg)[levels(h$restecg)==2] <- "Rest ECG 2"
levels(h$slope)[levels(h$slope)==1] <- "Upsloaping"
levels(h$slope)[levels(h$slope)==2] <- "Flat"
levels(h$slope)[levels(h$slope)==3] <- "Downsloping"
sum(is.na(h))
summary(h)
graphColor <- c("firebrick2","springgreen2")

#Number of observations: Healthy and Heart Disease cases
ggplot(h,aes(target, fill=target)) +
  geom_bar(stat="count") +
  theme_economist()  +
  scale_fill_manual(values=graphColor) 

#Heart Disease is uniformly spread out across Age
ggplot(h,aes(age, fill=target)) +
  geom_histogram(aes(y=..density..),breaks=seq(0, 80, by=1), color="grey17") +
  geom_density(alpha=.1, fill="black")+
  facet_wrap(~target, ncol=1,scale="fixed") +
  theme_economist()  +
  scale_fill_manual(values=graphColor)+
  xlab("Age") +
  ylab("Density / Count") +
  ggtitle("Age Histogram")

#No major difference in Rest ECG for Healthy and Heart Disease patients

ggplot(h,aes(trestbps, fill=target)) +
  geom_histogram(aes(y=..density..),breaks=seq(90, 200, by=10), color="grey17") +
  geom_density(alpha=.1, fill="black")+
  facet_wrap(~target, ncol=1,scale="fixed") +
  theme_economist()  +
  scale_fill_manual(values=graphColor) +
  xlab("Resting Blood Pressure (in mm Hg on admission to the hospital") +
  ylab("Density / Count") +
  ggtitle("Rest ECG Histogram")

#More Heart Disease patients seem to have between 200 and 250 mg/dl
ggplot(h,aes(chol, fill=target)) +
  geom_histogram(breaks=seq(100, 600, by=25), color="grey17") +
  geom_density(alpha=.1, fill="black")+
  facet_wrap(~target, ncol=1,scale="fixed") +
  theme_economist()  +
  scale_fill_manual(values=graphColor) +
  xlab("Serum Cholestoral in mg/dl") +
  ylab("Density / Count") +
  ggtitle("Cholestoral Histogram")

#Heart Disease patients have higher maximum heart rate than healthy patients
ggplot(h,aes(thalach, fill=target)) +
  geom_histogram(aes(y=..density..),breaks=seq(70, 205, by=10), color="grey17") +
  geom_density(alpha=.1, fill="black")+
  facet_wrap(~target, ncol=1,scale="fixed") +
  theme_economist()  +
  scale_fill_manual(values=graphColor)+
  xlab("Maximum Heart Rate Achieved") +
  ylab("Density / Count") +
  ggtitle("Max Heart Rate Histogram")

#More Heart Disease patients have ST depression of 0.1

ggplot(h,aes(oldpeak, fill=target)) +
  geom_histogram(aes(y=..density..),breaks=seq(0, 7, by=0.1), color="grey17") +
  geom_density(alpha=.1, fill="black")+
  facet_wrap(~target, ncol=1,scale="fixed") +
  theme_economist()  +
  scale_fill_manual(values=graphColor) +
  ggtitle("ST Depression Histogram") +
  xlab("ST Depression Induced by Exercise Relative to Rest") +
  ylab("Density / Count")

#Almost all of the patients who have Heart Disease have 0 major vessels as observed by Fluroscopy
ggplot(h,aes(ca, fill=target)) +
  geom_histogram(aes(y=..density..),breaks=seq(0, 5, by=1), color="grey17") +
  geom_density(alpha=.1, fill="black")+
  facet_wrap(~target, ncol=1,scale="fixed") +
  theme_economist()  +
  scale_fill_manual(values=graphColor) +
  ggtitle("No. Major Vessels Histogram") +
  xlab("Number of Major Vessels (0-3) Colored by Flourosopy") +
  ylab("Density / Count")

#More females have Heart Disease
ggplot(h,aes(target, fill=target)) +
  geom_bar(stat="count") +
  facet_wrap(~sex, ncol=2,scale="fixed") +
  theme_economist()  +
  scale_fill_manual(values=graphColor) 

#More Heart Disease patients have chest pain type 1 or 2
ggplot(h,aes(target, fill=target)) +
  geom_bar(stat="count") +
  facet_wrap(~cp, ncol=2,scale="fixed") +
  theme_economist()  +
  scale_fill_manual(values=graphColor) 

#No difference in fasting blood sugar
ggplot(h,aes(target, fill=target)) +
  geom_bar(stat="count") +
  facet_wrap(~fbs, ncol=2,scale="fixed") +
  theme_economist()  +
  scale_fill_manual(values=graphColor) 

#Patients with Rest ECG 1 have more Heart Diseases
ggplot(h,aes(target, fill=target)) +
  geom_bar(stat="count") +
  facet_wrap(~restecg, ncol=3,scale="fixed") +
  theme_economist()  +
  scale_fill_manual(values=graphColor)

#Patients with no exercise induced angina have more Heart Disease
ggplot(h,aes(target, fill=target)) +
  geom_bar(stat="count") +
  facet_wrap(~exang, ncol=1,scale="fixed") +
  theme_economist()  +
  scale_fill_manual(values=graphColor)

#Peak excercise ST Slope 2(Flat) have more Heart Disease
ggplot(na.omit(h),aes(target, fill=target)) +
  geom_bar(stat="count") +
  facet_wrap(~slope, ncol=1,scale="fixed") +
  theme_economist()  +
  scale_fill_manual(values=graphColor)

#Fixed defect thalasemia has more Heart Disease

ggplot(na.omit(h),aes(target, fill=target)) +
  geom_bar(stat="count") +
  facet_wrap(~thal, ncol=2,scale="fixed") +
  theme_economist()  +
  scale_fill_manual(values=graphColor)

ggplot(h, aes(x=sex,y=chol))+
  geom_boxplot(fill="#D55E00")+
  xlab("Sex")+
  ylab("Chol")+
  facet_grid(~cp)

ggplot(h, aes(x=age,y=chol,color=sex, size=chol))+
  geom_point(alpha=0.7)+xlab("Age") +
  ylab("Cholestoral")+
  guides(fill = guide_legend(title = "Gender"))


cor_heart <- cor(heart_data2[,10:14])
cor_heart

corrplot(cor_heart, method = "ellipse", type="upper",)


#classification :use logistic regression
#data division into train and test set
set.seed(123)
train.index <- heart_data2$target %>% createDataPartition(p =0.8,list =F)
train.data <- heart_data2[train.index,]
test.data <- heart_data2[-train.index,]

#fitting a full logistic model for classification
full.mod <- glm(target~.,data =train.data,family =binomial)
summary(full.mod)

#checking the model accuracy
prob <- full.mod %>% predict(test.data,type ="response")
predicted.class1 <- ifelse(prob>0.5,1,0)
mean(predicted.class1==test.data$target)


#accuracy =0.86
#stepwise logistic regression in R

step.model <- full.mod %>% stepAIC(trace = F)
summary(step.model)
prob.step <- step.model %>% predict(test.data,type ="response")
predicted.class2 <- ifelse(prob.step>0.5,1,0) 
mean(predicted.class2==test.data$target)


#accuracy =0.83
#among the full.mod and step.model the secand one is best interms of accuracy.

#logistic regression diognostic
#taking only quantitative variable
model_check <- glm(target~.,data =heart_data2,family = binomial)
prob.check <- predict(model_check,type ="response")
my_data <- heart_data2 %>% select_if(is.numeric)
predictors <- colnames(my_data)
my_data <- my_data%>% mutate(logit = log(prob.check/(1-prob.check))) %>%
  gather(key = "predictors",value = "predicted.value",-logit)

#plotting the graph for cheking the linearity
ggplot(my_data,aes(x =logit,y =predicted.value)) + geom_point() + geom_smooth(method ="loess") + theme_classic() + theme_bw()+facet_wrap(~predictors,scale ="free_y")
#the graph shows that linearity is maintained for continous variable (not discrete variable like,ca,cp,sex,exang,etc)


plot(step.model,which=4,id.n =3)

#the plot of cook_sd shows that 102 index haves higher cook_sd
#checking for the standarg residuals error
model.data <- augment(step.model) %>% mutate(index =1:n())
model.data %>% top_n(3,.cooksd)
#in these case the standard residuals error<3 so that can't be considered as a influential point
#checking for multi-collinearity
car::vif(step.model)
#no multi-collinearity presents .


#choosing the best cut-off probabillity value to the model
res <- predict(step.model,type ="response")
ROCR_Pred <- prediction(res,train.data$target)
ROCR_perf <- performance(ROCR_Pred,"tpr","fpr")
plot(ROCR_perf,colorize=T,print.cutoffs.at =seq(0.1,by =0.1))
#from the graph the cut-off value = 0.6.

#here is the final model 
final.model <- glm(target~.,data =train.data,family =binomial) %>%stepAIC(trash =FALSE)
prob.final <- predict(final.model,test.data,type ="response")
predicted.class_final <- ifelse(prob.final>0.6,1,0)
mean(predicted.class_final==test.data$target)
#the model gives a accuracy of 84.74% and used for prediction









