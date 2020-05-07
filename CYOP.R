#Introduction
#installing required packages
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(pROC)) install.packages("pROC", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(xgboost)) install.packages("xgboost", repos = "http://cran.us.r-project.org")


library(corrplot)
library(caret)
library(data.table)
library(dplyr)
library(ggplot2)
library(randomForest)
library(pROC)
library(e1071)
library(xgboost)

#Lookup table to convert States to Regions
NE.name <- c("Connecticut","Maine","Massachusetts","New Hampshire",
             "Rhode Island","Vermont","New Jersey","New York",
             "Pennsylvania")
NE.abrv <- c("CT","ME","MA","NH","RI","VT","NJ","NY","PA")
NE.ref <- c(NE.name,NE.abrv)

IS.name <- c("Puerto Rico","Marshall Islands","Guam","Virgin Islands")
IS.abrv <- c("PR","MH","GU","VI")
IS.ref <- c(IS.name,IS.abrv)

MW.name <- c("Indiana","Illinois","Michigan","Ohio","Wisconsin",
             "Iowa","Kansas","Minnesota","Missouri","Nebraska",
             "North Dakota","South Dakota")
MW.abrv <- c("IN","IL","MI","OH","WI","IA","KS","MN","MO","NE",
             "ND","SD")
MW.ref <- c(MW.name,MW.abrv)

S.name <- c("Delaware","District of Columbia","Florida","Georgia",
            "Maryland","North Carolina","South Carolina","Virginia",
            "West Virginia","Alabama","Kentucky","Mississippi",
            "Tennessee","Arkansas","Louisiana","Oklahoma","Texas")
S.abrv <- c("DE","DC","FL","GA","MD","NC","SC","VA","WV","AL",
            "KY","MS","TN","AR","LA","OK","TX")
S.ref <- c(S.name,S.abrv)

W.name <- c("Arizona","Colorado","Idaho","New Mexico","Montana",
            "Utah","Nevada","Wyoming","Alaska","California",
            "Hawaii","Oregon","Washington")
W.abrv <- c("AZ","CO","ID","NM","MT","UT","NV","WY","AK","CA",
            "HI","OR","WA")
W.ref <- c(W.name,W.abrv)

region.list <- list(
  Northeast=NE.ref,
  Midwest=MW.ref,
  South=S.ref,
  West=W.ref,
  Island=IS.ref)
#The dataset is a small version of phsyican database from the medicare website. 
#Follwoing are the columns in the dataset
#phys <- read.csv("../data/phy_small.csv")
phys <- read.csv("https://raw.githubusercontent.com/arunchiri/CYO_project/master/data/phy_small.csv")
# our goal of this project to predict the Physician Quality Reporting System (PQRS) participation of the providers
colnames(phys)
#DATA ANALYSIS
#glimpse shows the vaiables and their type
glimpse(phys)
#summary shows the data summary 
summary(phys)
#checking for NA values 
apply(is.na(phys), 2, sum)
#The PQRS participation is showing as N : 7849 and Y : 2151. There is a strong correlation to the
#EHR (Electronic Health Records) participation with N: 7846 Y  : 2154 . Also there is some releation with 
#eRx (Electronic Prescribing Incentive Program) participation and Professional accepts Medicare Assignment
#when checking the zip code we can see that the zip codes are not in correct format. We are hoping to
#see the participation of PQRS becausse of the zip codes

glimpse(phys$Zip.Code)

#correcting the zipcodes which are not informat using charcter  formating 
zipcode_tst <- sprintf(ifelse(nchar(phys$Zip.Code) > 5, '%09d', '%05d%s'), phys$Zip.Code,"0000")
phys$Zip.Code <- zipcode_tst
summary(phys$Zip.Code)

#plot of PQRS participation
phys %>%
  group_by(Participating.in.PQRS) %>%
  tally() %>%
  ggplot(aes(x = Participating.in.PQRS, y = n,fill=Participating.in.PQRS)) +
  geom_bar(stat = "identity") +
  theme_minimal()+
  labs(x="PQRS Participation", y="Count of Participation")+
  ggtitle("PQRS Participation")+
  geom_text(aes(label = n), vjust = -0.5, position = position_dodge(0.9))


# Participation by gender
phys %>%
  ggplot(aes(x = Gender, group = Participating.in.PQRS)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), 
           stat="count", 
           alpha = 0.7) +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), 
            stat= "count", 
            vjust = -.5) +
  labs(y = "Percentage", fill= "Gender") +
  facet_grid(~Participating.in.PQRS) +
  theme_minimal()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Participating in PQRS")

# Participation by credential 
phys %>%
  ggplot(aes(x = Credential, group = Participating.in.PQRS)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), 
           stat="count", 
           alpha = 0.7) +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), 
            stat= "count", 
            vjust = -.5) +
  labs(y = "Percentage", fill= "Credential") +
  facet_grid(~Participating.in.PQRS) +
  theme_minimal()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = .5)) +
  ggtitle("Participating in PQRS")

# Participation by State is not showing any variation. We can exculde
phys %>%
  ggplot(aes(x = State, group = Participating.in.PQRS)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), 
           stat="count", 
           alpha = 0.7) +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), 
            stat= "count", 
            vjust = -.5) +
  labs(y = "Percentage", fill= "State") +
  facet_grid(~Participating.in.PQRS) +
  theme_minimal()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = .5)) +
  ggtitle("Participating in PQRS")


# Participation by eRx is showing variation. We can include since it's showing some relation
phys %>%
  ggplot(aes(x = Participating.in.eRx, group = Participating.in.PQRS)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), 
           stat="count", 
           alpha = 0.7) +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), 
            stat= "count", 
            vjust = -.5) +
  labs(y = "Percentage", fill= "Participating.in.eRx") +
  facet_grid(~Participating.in.PQRS) +
  theme_minimal()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = .5)) +
  ggtitle("Participating in PQRS")

# Participation by EHR is showing variation. We can include since it's showing some relation
phys %>%
  ggplot(aes(x = Participating.in.EHR, group = Participating.in.PQRS)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), 
           stat="count", 
           alpha = 0.7) +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), 
            stat= "count", 
            vjust = -.5) +
  labs(y = "Percentage", fill= "Participating.in.EHR") +
  facet_grid(~Participating.in.PQRS) +
  theme_minimal()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = .5)) +
  ggtitle("Participating in PQRS")

# PProfessional.accepts.Medicare.Assignment showing very less information. 
phys %>%  
  ggplot(aes(x = Professional.accepts.Medicare.Assignment, group = Participating.in.PQRS)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), 
           stat="count", 
           alpha = 0.7) +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), 
            stat= "count", 
            vjust = -.5) +
  labs(y = "Percentage", fill= "Professional.accepts.Medicare.Assignment") +
  facet_grid(~Participating.in.PQRS) +
  theme_minimal()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = .5)) +
  ggtitle("Participating in PQRS")


# Graduation.year showing some increasing information for the participation in PQRS. 
#So we can include this variable in our model
phys %>%
  ggplot(mapping = aes(x = Number.of.Group.Practice.members)) + 
  geom_histogram(aes(fill = Participating.in.PQRS), bins=20)+
  facet_grid(~Participating.in.PQRS) +
  labs(x="Number.of.Group.Practice.members", y="Participating.in.PQRS")+
  ggtitle("Participating.in.PQRS in regards to Number.of.Group.Practice.members")

# Graduation.year showing some increasing information for the participation in PQRS. 
#So we can include this variable in our model
phys %>%
  ggplot(mapping = aes(x = Graduation.year)) + 
  geom_histogram(aes(fill = Participating.in.PQRS), bins=20)+
  facet_grid(~Participating.in.PQRS) +
  labs(x="Graduation.year", y="Participating.in.PQRS")+
  ggtitle("Participating.in.PQRS in regards to Graduation.year")

#data cleaning & preparation
colnames(phys)

phys <- phys %>% mutate_if(is.integer, ~replace(., is.na(.), 0))

summary(phys)
phys_T <- phys
phys_T$grad_year_div <- as.factor(cut(phys_T$Graduation.year,breaks = c(0,1940,1950,1960,1970,1980,1990,2000,2010,2020), 
                            labels = c("1930","1940","1950","1960","1970","1980","1990","2000","2010")))

summary(phys_T$grad_year_div)
phys_T %>%
  ggplot(aes(x = grad_year_div, group = Participating.in.PQRS)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), 
           stat="count", 
           alpha = 0.7) +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), 
            stat= "count", 
            vjust = -.5) +
  labs(y = "Percentage", fill= "grad_year_div") +
  facet_grid(~Participating.in.PQRS) +
  theme_minimal()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = .5)) +
  ggtitle("Participating in PQRS")

phys_T$group_pract_member_count <- as.factor(cut(phys_T$Number.of.Group.Practice.members,breaks = c(-1,0,25,50,75,100,200,300,400,500,600,700,800,900,1000), 
                                      labels = c("0","10","25","50","75","100","200","300","400","500","600","700","800","900")))

summary(phys_T$group_pract_member_count)
glimpse(phys_T)

phys_T %>%
  ggplot(aes(x = group_pract_member_count, group = Participating.in.PQRS)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), 
           stat="count", 
           alpha = 0.7) +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), 
            stat= "count", 
            vjust = -.5) +
  labs(y = "Percentage", fill= "group_pract_member_count") +
  facet_grid(~Participating.in.PQRS) +
  theme_minimal()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = .5)) +
  ggtitle("Participating in PQRS")

#grouping the states under different regions. 
#first we are changing state codes to different levels. Random Forest will take only 53 level for a variable
#state have 56 levels so we are dividing the levels to regionwise. 
levels(phys_T$State)
nlevels(phys_T$State)
tst_regions <- sapply(phys_T$State, 
                      function(x) names(region.list)[grep(x,region.list)]) 
tst_regions <- as.factor(unlist(tst_regions))
#adding the regions to the dataframe
phys_T$regions <- tst_regions
#summary of the regions
summary(phys_T$regions)

#Removing unecessary columns to reduce complexity of the dataset
colnames(phys_T)
phys_F <- phys_T[c(8,9,37,38,39,40,41,42,43)]
#Data summary after triming the columns
summary(phys_F)
dim(phys_F)
colnames(phys_F)
#converting the column names to short forms.
#converting the column names to short forms.
names(phys_F)[3] <- "PAMA"
names(phys_F)[4] <- "eRx"
names(phys_F)[5] <- "PQRS"
names(phys_F)[6] <- "EHR"
names(phys_F)[7] <- "grad_year"
names(phys_F)[8] <- "GMPC" #group member practice count divisions
#verifying the column names after conversion 
colnames(phys_F)

#only selecting the important attributes which are coming under 53 levels or integers. 
summary(phys_F)
glimpse(phys_F)
#checking whether the levels of creditial to understand whether it is having only levels which are under limit.
levels(phys_F$Credential)
nlevels(phys_F$Credential)

# LEts make a correlation matrix for the data 
corrplot(cor(sapply(phys_F,as.integer)),method = "ellipse")

#Modeling using Random Forest

# Splitting into train & test
set.seed(30, sample.kind="Rounding")
indexes = sample(1:nrow(phys_F), size=0.8*nrow(phys_F))
RF_train <- phys_F[indexes,]
RF_test <- phys_F[-indexes,]
#training the model 
Rf.model <- randomForest(PQRS~.,RF_train, importance=TRUE,ntree=1000)
varImpPlot(Rf.model)
Rf.prd <- predict(Rf.model, newdata = RF_test)
confusionMatrix(RF_test$PQRS, Rf.prd)
plot.roc(as.numeric(RF_test$PQRS), as.numeric(Rf.prd),lwd=2, type="b",print.auc=TRUE,col ="blue")


#Modeling using Support Vector Machine
set.seed(30, sample.kind="Rounding")
indexes = sample(1:nrow(phys_F), size=0.8*nrow(phys_F))
SVM_train <- phys_F[indexes,]
SVM_test <- phys_F[-indexes,]

#tunning the parameters for SVM model 
tune_prm <- tune(svm,factor(PQRS)~.,data = SVM_train)

#training the model using train data 
SVM_model <- svm(SVM_train$PQRS~., data=SVM_train
                 ,type="C-classification", gamma=tune_prm$best.model$gamma
                 ,cost=tune_prm$best.model$cost
                 ,kernel="radial")
#predicting the values using the test dataset 
SVM_prd <- predict(SVM_model,newdata=SVM_test)
confusionMatrix(SVM_prd,SVM_test$PQRS)

#This model don't a good confusion matrix. Worst than the random forest. 

#ROC plot 
SVM_plot <- plot.roc (as.numeric(SVM_test$PQRS), as.numeric(SVM_prd),lwd=2, type="b", print.auc=TRUE,col ="blue")

#Clearing the objects from memory for XGBoost execution
rm(phys,phys_T,RF_train,SVM_train)
#XGBoost Model

#Tuning XBGTree using Caret Package
#Hyperparameters to tune:
#nrounds: Number of trees, default: 100
#max_depth: Maximum tree depth, default: 6
#eta: Learning rate, default: 0.3
#  gamma: Used for tuning of Regularization, default: 0
#  colsample_bytree: Column sampling, default: 1
#  min_child_weight: Minimum leaf weight, default: 1
#  subsample: Row sampling, default: 1
#  We'll break down the tuning of these into five sections:
#   
#   Fixing learning rate eta and number of iterations nrounds
#  Maximum depth max_depth and child weight min_child_weight
#  Setting column colsample_bytree and row sampling subsample
#  Experimenting with different gamma values
#  Reducing the learning rate eta

# set seed
set.seed(30, sample.kind="Rounding")
xgbData <- phys_F
indexes <- sample(1:nrow(xgbData), size=0.8*nrow(xgbData))
XGBtrain <- xgbData[indexes,]
XGBtest <- xgbData[-indexes,]
# note to start nrounds from 200, as smaller learning rates result in errors so
# big with lower starting points that they'll mess the scales
formula = PQRS~.
nrounds <- 1000

tune_grid <- expand.grid(
  nrounds = seq(from = 200, to = nrounds, by = 50),
  eta = c(0.025, 0.05, 0.1, 0.3),
  max_depth = c(2, 3, 4, 5, 6,8,10,15,20,25,30,35,40,50),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

tune_control <- caret::trainControl(
  method = "cv", # cross-validation
  number = 3, # with n folds 
  classProbs = TRUE
)

xgb_tune <- caret::train(
  formula,
  data = XGBtrain,
  trControl = tune_control,
  tuneGrid = tune_grid,
  method = "xgbTree"
)
predictions<-predict(xgb_tune,XGBtest)
confusionMatrix(predictions,XGBtest$PQRS) #0.799

#output of best Tune is nrounds = 200 eta = 0.05, max_dept = 2
xgb_tune$bestTune

ggplot(xgb_tune)

xgb_tune$bestTune$nrounds
xgb_tune$bestTune$max_depth
xgb_tune$bestTune$eta
xgb_tune$bestTune$min_child_weight
xgb_tune$bestTune$subsample
tune_grid2 <- expand.grid(
  nrounds = seq(from = 50, to = nrounds, by = 50), #700
  eta = xgb_tune$bestTune$eta,
  max_depth = c(1,2,3,5,10,15,20,25,30,35), #15
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = c(1, 2, 3), #3
  subsample = 1
)

xgb_tune2 <- caret::train(
  formula,
  data = XGBtrain,
  trControl = tune_control,
  tuneGrid = tune_grid2,
  method = "xgbTree",
  verbose = TRUE
)

ggplot(xgb_tune2)
predictions2<-predict(xgb_tune2,XGBtest)
confusionMatrix(predictions2,XGBtest$PQRS) #0.792
xgb_tune$bestTune
xgb_tune2$bestTune
tune_grid3 <- expand.grid(
  nrounds = seq(from = 50, to = nrounds, by = 50), #150
  eta = xgb_tune$bestTune$eta,
  max_depth = xgb_tune2$bestTune$max_depth,
  gamma = 0,
  colsample_bytree = c(0.4, 0.6, 0.8, 1.0), #0.8
  min_child_weight = xgb_tune2$bestTune$min_child_weight,
  subsample = c(0.5, 0.75, 1.0) #0.75
)

xgb_tune3 <- caret::train(
  formula,
  data = XGBtrain,
  trControl = tune_control,
  tuneGrid = tune_grid3,
  method = "xgbTree",
  verbose = TRUE
)

ggplot(xgb_tune3)
predictions3<-predict(xgb_tune3,XGBtest)
confusionMatrix(predictions3,XGBtest$PQRS) #.7925

xgb_tune$bestTune
xgb_tune2$bestTune
xgb_tune3$bestTune
tune_grid4 <- expand.grid(
  nrounds = seq(from = 50, to = nrounds, by = 50), #500
  eta = xgb_tune$bestTune$eta,
  max_depth = xgb_tune2$bestTune$max_depth,
  gamma = c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0), #0.1
  colsample_bytree = xgb_tune3$bestTune$colsample_bytree,
  min_child_weight = xgb_tune2$bestTune$min_child_weight,
  subsample = xgb_tune3$bestTune$subsample
)

xgb_tune4 <- caret::train(
  formula,
  data = XGBtrain,
  trControl = tune_control,
  tuneGrid = tune_grid4,
  method = "xgbTree",
  verbose = TRUE
)
ggplot(xgb_tune4)
predictions4<-predict(xgb_tune4,XGBtest)
confusionMatrix(predictions4,XGBtest$PQRS) #0.8015
xgb_tune$bestTune
xgb_tune2$bestTune
xgb_tune3$bestTune
xgb_tune4$bestTune

tune_grid5 <- expand.grid(
  nrounds = seq(from = 100, to = 500, by = 25), #300
  eta = c(0.01, 0.015, 0.025, 0.05, 0.1), #0.025
  max_depth = xgb_tune2$bestTune$max_depth,
  gamma = xgb_tune4$bestTune$gamma,
  colsample_bytree = xgb_tune3$bestTune$colsample_bytree,
  min_child_weight = xgb_tune2$bestTune$min_child_weight,
  subsample = xgb_tune3$bestTune$subsample
)
xgb_tune5 <- caret::train(
  formula,
  data = XGBtrain,
  trControl = tune_control,
  tuneGrid = tune_grid5,
  method = "xgbTree",
  verbose = TRUE
)

xgb_tune5$bestTune
xgb_tune4$bestTune
xgb_tune3$bestTune
xgb_tune2$bestTune
xgb_tune$bestTune

ggplot(xgb_tune5)
predictions5<-predict(xgb_tune5,XGBtest)
confusionMatrix(predictions5,XGBtest$PQRS) #0.802


#creating test and train datasets for XGBoost model

set.seed(30, sample.kind="Rounding")
xgbData <- phys_F
indexes <- sample(1:nrow(xgbData), size=0.8*nrow(xgbData))
XGBtrain <- xgbData[indexes,]
XGBtest <- xgbData[-indexes,]

formula = PQRS~.
fitControl <- trainControl(method="cv", number = 3,classProbs = TRUE )

#We are using the best tunned hyperparameters (mentioned in above section) in below model.
xgbGrid <- expand.grid(nrounds = 250,
                       max_depth = 1,
                       eta = .025,
                       gamma = 0.05,
                       colsample_bytree = .4,
                       min_child_weight = 1,
                       subsample = 0.75
)
XGB.model <- train(formula, data = XGBtrain,
                   method = "xgbTree"
                   ,trControl = fitControl
                   , verbose=0
                   , maximize=FALSE
                   ,tuneGrid = xgbGrid
)
importance <- varImp(XGB.model)
varImportance <- data.frame(Variables = row.names(importance[[1]]), 
                            Importance = round(importance[[1]]$Overall,2))

#We are now creating a rank variable based on importance of variables

rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance)) +
  geom_bar(stat='identity',colour="white", fill = "lightgreen") +
  geom_text(aes(x = Variables, y = 1, label = Rank),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Variables', title = 'Relative Variable Importance') +
  coord_flip() + 
  theme_bw()

#predicting using XGBoost model
XGB.prd <- predict(XGB.model,XGBtest)
confusionMatrix(XGB.prd, XGBtest$PQRS)
#plotting the ROC for XGBoost model
XGB.plot <- plot.roc (as.numeric(XGBtest$PQRS), as.numeric(XGB.prd),lwd=2, type="b", print.auc=TRUE,col ="blue")

## Result
#The different ROC plots to compare the AUC values
par(mfrow=c(2,3))
plot.roc (as.numeric(XGBtest$PQRS), as.numeric(XGB.prd),main="XGBoost",lwd=2, type="b", print.auc=TRUE, col ="blue")
plot.roc (as.numeric(SVM_test$PQRS), as.numeric(SVM_prd),main="SVM",lwd=2, type="b", print.auc=TRUE, col ="red")
plot.roc (as.numeric(RF_test$PQRS), as.numeric(Rf.prd), main="Random Forest",lwd=2, type="b", print.auc=TRUE, col ="seagreen")

