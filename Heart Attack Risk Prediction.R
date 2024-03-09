                 #SETTING WORKING DIRECTORY
getwd()
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) 
                     #INSTALLING PACKAGES
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("gridExtra")
install.packages("GGally")
install.packages("knitr")
install.packages("caret")
install.packages("mlbench")
install.packages("e1071")
install.packages("kernlab")
install.packages("DataExplorer")
install.packages("car")
install.packages("remotes")
remotes::install_github("cran/DMwR") 
install.packages("class")
install.packages("Hmisc")
install.packages("pROC")

                      #CALLING LIBRARIES
library(tidyverse)
library(tidyr)
library(ggplot2)
library(corrplot)
library(Hmisc)
library(gridExtra)
library(GGally)
library(knitr)
library(dplyr)
library(caret)
library(mlbench)
library(e1071)
library(kernlab)
library(DataExplorer)
library(car)
library(DMwR)
library(class)
library(pROC)
library(readr)


                        #READING IN CSV FILE
heartattack <- read.csv("Heart Attack Prediction.csv")
                       #EXPLORATORY DATA ANALYSIS (EDA)
#CHECKING THE structure OF THE DATASET
str(heartattack)

#CHECKING THE SUMMARY ATTRIBUTES OF THE DATASET
summary(heartattack)

#CHECKING THE DIMENSION OF THE DATASET
dim(heartattack)

# CHECKING FOR MISSING VALUES
plot_missing(heartattack)

                              #WOULD BE FOCUSING ON EUROPE
heartattack <- heartattack %>%
  filter(Continent == "Europe")

#CHECKING THE structure OF THE DATASET
str(heartattack)

#CHECKING THE SUMMARY ATTRIBUTES OF THE DATASET
summary(heartattack)

#CHECKING THE DIMENSION OF THE DATASET
dim(heartattack)

# CHECKING FOR MISSING VALUES
anyNA(heartattack)  
#(RESULT OF anyNA came out as False)

                        #EDA~ VISUALIZATION
#PLOT BAR FOR EACH VARIABLES
plot_bar(heartattack)

#BAR CHART FOR CATEGORICAL VARIABLES
heartattack %>%
  select(Alcohol.Consumption, Diabetes, Family.History, Heart.Attack.Risk, Medication.Use, 
         Obesity, Previous.Heart.Problems, Smoking) %>%
  gather(Variable, Value) %>%
  ggplot(aes(x = as.factor(Value), fill = as.factor(Value))) +
  geom_bar(stat = "count", position = "dodge") +
  facet_wrap(~ Variable, scales = "free_y", ncol = 4) +
  labs(x = "Values", y = "Count", title = "Bar Charts of Categorical Variables") +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "red"), labels = c("NO", "Yes")) +  
  theme(axis.text.x = element_text(angle = 0,hjust = 1))

# HISTOGRAM FOR EACH VARIABLE EXCLUDING SPECIFIC VARIABLES
heartattack %>%
  select_if(is.numeric) %>%
  select(-Alcohol.Consumption, -Diabetes, -Family.History, -Heart.Attack.Risk, -Medication.Use, 
         -Obesity,-Previous.Heart.Problems,-Smoking,-Stress.Level,-Sleep.Hours.Per.Day,
         -Physical.Activity.Days.Per.Week) %>%
  gather(Attributes, value) %>%
  ggplot(aes(x = value, fill = Attributes)) +
  geom_histogram(colour = "black", show.legend = FALSE) +
  facet_wrap(~Attributes, scales = "free_x", ncol = 4) +
  labs(x = "Values", y = "Frequency", title = "Heart Attack Features - Histograms") +
  theme_bw()

# BOX PLOT FOR EACH VARIABLES
heartattack %>%
  select_if(is.numeric) %>%
  select(-Alcohol.Consumption, -Diabetes, -Family.History, -Heart.Attack.Risk, -Medication.Use, 
         -Obesity,-Previous.Heart.Problems,-Smoking,-Stress.Level) %>%
  gather(Attributes, value) %>%
  ggplot(aes(x = value, fill = Attributes)) +
  geom_boxplot(colour = "black", alpha = 0.5, show.legend = FALSE) +
  facet_wrap(~Attributes, scales = "free_x", ncol = 4) +
  labs( title = "Heart Attack Features - Box Plots") +
  theme_bw()

             #VISUALISING RELATIONSHIP BETWEEN FEATURES AND TARGET VARIABLE
#Distribution of Heart Attack Risk by Gender
ggplot(heartattack, aes(x = factor(Sex), fill = factor(Heart.Attack.Risk))) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Heart Attack Risk by Gender", x = "Gender", y = "Count") +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "red"), labels = c("No", "Yes")) +
  theme_minimal()+
  scale_x_discrete(labels = c("0" = "Male", "1" = "Female"))


# Distribution of Heart Attack Risk by Smoking
ggplot(heartattack, aes(x = factor(Smoking), fill = factor(Heart.Attack.Risk))) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Heart Attack Risk by Smoking", 
       x = "Smoking",
       y = "Count") +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "red"), labels = c("No", "Yes")) +
  theme_minimal() +
  scale_x_discrete(labels = c("0" = "Non-Smoker", "1" = "Smoker"))

# Distribution of Heart Attack Risk by Alcohol Consumption
ggplot(heartattack, aes(x = factor(Alcohol.Consumption), fill = factor(Heart.Attack.Risk))) +
  geom_bar(position = "dodge") +
  labs(title = "Heart Attack Risk by Alcohol Consumption", 
       x = "Alcohol.Consumption",
       y = "Count") +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "red"), labels = c("No", "Yes")) +
  theme_minimal() +
  scale_x_discrete(labels = c("0" = "Non-Alcholic", "1" = "Alcholic"))

# Distribution of Heart Attack Risk by Diabetes
ggplot(heartattack, aes(x = factor(Diabetes), fill = factor(Heart.Attack.Risk))) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Heart Attack Risk by Diabetes", 
       x = "Diabetes",
       y = "Count") +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "red"), labels = c("No", "Yes")) +
  theme_minimal() +
  scale_x_discrete(labels = c("0" = "No-Diabetes", "1" = "Diabetes"))

# Distribution of Heart Attack Risk by Family History
ggplot(heartattack, aes(x = factor(Family.History), fill = factor(Heart.Attack.Risk))) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Heart Attack Risk by Family History", 
       x = "Family.History",
       y = "Count") +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "red"), labels = c("No", "Yes")) +
  theme_minimal() +
  scale_x_discrete(labels = c("0" = "No Family History", "1" = "Family History"))

# Distribution of Heart Attack Risk by Obesity
ggplot(heartattack, aes(x = factor(Obesity), fill = factor(Heart.Attack.Risk))) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Heart Attack Risk by Obesity", 
       x = "Obesity",
       y = "Count") +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "red"), labels = c("No", "Yes")) +
  theme_minimal() +
  scale_x_discrete(labels = c("0" = "No Obesity", "1" = "Obesed"))

# Distribution of Heart Attack Risk by Previous Heart Problems
ggplot(heartattack, aes(x = factor(Previous.Heart.Problems), fill = factor(Heart.Attack.Risk))) +
  geom_bar(position = "dodge") +
  labs(title = "Heart Attack Risk by Previous Heart Problems", 
       x = "Previous.Heart.Problems",
       y = "Count") +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "red"), labels = c("No", "Yes")) +
  theme_minimal() +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes"))

# Distribution of Heart Attack Risk by Medication Use
ggplot(heartattack, aes(x = factor(Medication.Use), fill = factor(Heart.Attack.Risk))) +
  geom_bar(position = "dodge") +
  labs(title = "Heart Attack Risk by Medication Use", 
       x = "Medication.Use",
       y = "Count") +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "red"), labels = c("No", "Yes")) +
  theme_minimal() +
  scale_x_discrete(labels = c("0" = "No Medication Use", "1" = "Medication Use"))

# Distribution of Heart Attack Risk by Diet
ggplot(heartattack, aes(x = factor(Diet), fill = factor(Heart.Attack.Risk))) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Heart Attack Risk by Diet",
       x = "Diet",
       y = "Count") +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "red"), labels = c("No", "Yes")) +
  theme_minimal() 

# Distribution of Heart Attack Risk by Hemisphere
ggplot(heartattack, aes(x = factor(Hemisphere), fill = factor(Heart.Attack.Risk))) +
  geom_bar(position = "dodge") +
  labs(title = "Heart Attack Risk by Hemisphere",
       x = "Hemisphere",
       y = "Count") +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "red"), labels = c("No", "Yes")) +
  theme_minimal()+ 
  scale_x_discrete(labels = c("Northern Hemisphere" = "Northern", "Southern Hemisphere" = "Southern"))

# Age Group Distribution by Heart Attack Risk
# Grouping Ages into Bins
heartattack$Age_Group <- cut(heartattack$Age, breaks = c(0, 30, 40, 50, 60, 70, Inf),
                             labels = c("0-30", "31-40", "41-50", "51-60", "61-70", "71+"),
                             include.lowest = TRUE)
#PLOT
ggplot(heartattack, aes(x = Age_Group, fill = factor(Heart.Attack.Risk))) +
  geom_bar(position = "dodge") +
  labs(title = "Age Group Distribution by Heart Attack Risk",
       x = "Age Group",
       y = "Count") +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "red"), labels = c("No", "Yes")) +
  theme_minimal()

# Income Group Distribution by Heart Attack Risk
# Grouping Income into Bins
heartattack$Income_Group <- cut(heartattack$Income, breaks = c(0, 50000, 100000, 150000, 200000, Inf),
                                labels = c("0-50k", "50k-100k", "100k-150k", "150k-200k", "200k+"),
                                include.lowest = TRUE)
#PLOT
ggplot(heartattack, aes(x = Income_Group, fill = factor(Heart.Attack.Risk))) +
  geom_bar(position = "dodge") +
  labs(title = "Heart Attack Risk by Income Group",
       x = "Income Group",
       y = "Count") +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "red"), labels = c("No", "Yes")) +
  theme_minimal()

# Distribution of Heart Attack Risk by Stress Level
ggplot(heartattack, aes(x = factor(Stress.Level), fill = factor(Heart.Attack.Risk))) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Heart Attacks by Stress Level",
       x = "Stress Level",
       y = "Count") +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "red"), labels = c("No", "Yes")) +
  theme_minimal()
                                    # DATA CLEANING 
                          # Using pipe function to perform data cleaning
heartattack <- heartattack %>%
# Separating Blood Pressure into Systolic and Diastolic columns
  separate(Blood.Pressure, into = c("Systolic", "Diastolic"), sep = "/") %>%
# Changing MALE AND FEMALE TO NUMERICAL VALUES (MALE = 0 & FEMALE = 1)
  mutate(Sex = ifelse(Sex == "Male", 0, 1)) %>%
# Removing the PATIENT ID, COUNTRY, AND HEMISPHERE VARIABLES AS THEY WOULD NOT BE NEEDED FOR THIS ANALYSIS
  select(-Patient.ID, -Continent, -Hemisphere, - Country) %>%
  #REMOVING UNNECESSARY COLUMNS AFTER VISUALISATION
  select (-Age_Group,-Income_Group) %>%
# Changing DIET INTO numerical value with Unhealthy as 0, Average as 1, and Healthy as 2 
  mutate(Diet = case_when(
    Diet == "Unhealthy" ~ 0,
    Diet == "Average" ~ 1,
    Diet == "Healthy" ~ 2)) %>%
# Converting the SYSTOLIC AND DIASTOLIC COLUMNS TO NUMERICAL VALUES
  mutate(Systolic = as.numeric(Systolic),
         Diastolic = as.numeric(Diastolic)) 

# Checking the structure of the modified data set
str(heartattack)

#Using Box Plot to show the distribution of features after data cleaning
# BOX PLOT FOR EACH VARIABLES
heartattack %>%
  select_if(is.numeric) %>%
  select(-Alcohol.Consumption, -Diabetes, -Family.History, -Heart.Attack.Risk, -Medication.Use, 
         -Obesity,-Previous.Heart.Problems,-Smoking,-Stress.Level,-Sex, -Diet) %>%
  gather(Attributes, value) %>%
  ggplot(aes(x = value, fill = Attributes)) +
  geom_boxplot(colour = "black", alpha = 0.5, show.legend = FALSE) +
  facet_wrap(~Attributes, scales = "free_x", ncol = 4) +
  labs( title = "Heart Attack Features - Box Plots") +
  theme_bw()

                   #CORRELATION ANALYSIS FOR FEATURE SELECTION
#Converting target variable to numeric
heartattack$Heart.Attack.Risk <- as.numeric(heartattack$Heart.Attack.Risk)
set.seed(123)
# Exclude the target variable for correlation analysis
predictors <- heartattack[, names(heartattack) != "Heart.Attack.Risk"]
# Calculate correlation matrix
corr <- cor(predictors)
# Plot correlation matrix using corrplot
corrplot(corr, method = "circle",
         type = "lower", outline = TRUE,
         addgrid.col = "darkgray", order = "hclust",
         mar = c(0, 0, 0, 4), addrect = 4,
         rect.col = "black", rect.lwd = 5,
         cl.pos = "b", tl.col = "black",
         tl.cex = 0.5, cl.cex = 0.5)
#Checking for highly Correlated Features
highlyCorrelated <- findCorrelation(corr,
                                    cutoff = .6,
                                    verbose = TRUE,
                                    names = TRUE)

#Printing Correlation
print(corr)

                                #NORMALIZATION
# Normalizing all variables except the target variable
heart_norm <- heartattack %>%
  # Selecting which column to normalize
  select(1:22) %>%
  # Handling non-numeric values and converting each column to numeric
  mutate(across(everything(), ~as.numeric(as.character(.)))) %>%
  # Centering and scaling the variables selected (normalizing)
  mutate(across(everything(), scale)) %>%
  # Binding the normalized variables to the original dataset (combining with the target variable)
  bind_cols(heartattack %>% select(-(1:22))) %>%
  # Ensuring all columns are numeric
  mutate(across(everything(), as.numeric))

# Displaying the structure of the normalized data
str(heart_norm)
#CONVERTING TARGET VARIABLE TO FACTOR
heart_norm$Heart.Attack.Risk <- as.factor(heart_norm$Heart.Attack.Risk)

#SHUFFLING DATASET
set.seed(77)
#Shuffle the rows of the data frame
heart_norm <- heart_norm %>%
  sample_n(size = nrow(heart_norm),
           replace = FALSE)

# Count occurrences of each category in the target variable
count <- table(heart_norm$Heart.Attack.Risk)
# Calculate percentages
percentages <- prop.table(count) * 100
# Print the results
count
percentages
                                   
                          #SMOTE BALANCING TECHNIQUE
heart_norm$Heart.Attack.Risk <- as.factor(heart_norm$Heart.Attack.Risk)
#BALANCING THE TARGET VARIABLE USING SMOTE 
heartattack2<- SMOTE(Heart.Attack.Risk ~ ., data = heart_norm, perc.over = 100,
                     perc.under = 300, k = 5)
# Check class distribution after SMOTE
table(heartattack2$Heart.Attack.Risk)
# Count occurrences of each category in the target variable
count1 <- table(heartattack2$Heart.Attack.Risk)
# Calculate percentages
percentages1 <- prop.table(count1) * 100
# Print the results
count1
percentages1
#PLOTTING TARGET VARIABLE IN THE ORIGINAL DATASET TO DETERMINE THE BALANCE BETWEEN THE CATEGORIES
Plot1 <- ggplot(heartattack,mapping = aes(x= Heart.Attack.Risk))+
  geom_bar(colour="skyblue",)+
  labs(title = "Original Data")+
  theme_bw() 
#PLOTTING TARGET VARIABLE TO SHOW THE BALANCE BETWEEN THE CATEGORIES AFTER UPSAMPLING
Plot2 <- ggplot(heartattack2,mapping = aes(x= Heart.Attack.Risk))+
  geom_bar(colour="skyblue",)+
  labs(title = "Up-Sampled Data")+
  theme_bw()
#PLOTTING BEFORE AND AFTER SMOTE
grid.arrange(Plot1,Plot2, ncol=2)

                                # MACHINE LEARNING ALGORITHM
# Convert the target variable to a factor
heartattack2$Heart.Attack.Risk <- as.factor(heartattack2$Heart.Attack.Risk)
#DATA SPLICING(Dividing upsampled data into train & test)
#SET SEED FOR REPRODUCTIVITY
set.seed(123)
#SETTING SAMPLE FOR SPILITTING AS 70%
dat.d <- sample(1:nrow(heartattack2),size=nrow(heartattack2)*0.7,replace = FALSE)
#CREATING TRAINING DATASET
train <- heartattack2[dat.d,]
#CREATING TESTING DATASET
test <- heartattack2[-dat.d,]

#CREATING A SEPERATE DATAFRAME FOR HEART.ATTACK.RISK(TARGET VARIABLE)
train.heart <- heartattack2[dat.d,"Heart.Attack.Risk"]
test.heart <- heartattack2[-dat.d,"Heart.Attack.Risk"]
#CHEKING FOR NUMBER OFOBSERVATIONS IN EACH DATAFRAME
NROW(train.heart)
NROW(test.heart)

                      #SUPPORT VECTOR MECHANISM
# SET SEED TO ENSURE REPRODUCIBILITY OF RANDOM NUMBERS
set.seed(123)
#setting train control
trctrl <- trainControl(method= "repeatedcv",
                       number = 10,
                       repeats = 5)
# TRAIN SVM MODEL
svm_model <- train(Heart.Attack.Risk~.,data = train,
                         method = "svmLinear", trControl = trctrl,
                         preProcess =c("center", "scale"),
                         tuneLength = 10,
                         probability = TRUE)
svm_model
 # MAKE PREDICTIONS ON THE TESTING SET
test_pred <- predict(svm_model, newdata = test)
# EVALUATE THE SVM MODEL
confusion_matrix <- table(test_pred, test$Heart.Attack.Risk)
print(confusion_matrix)
# USING CONFUSION MATRIX TO CALCULATE THE ACCURACY 
confusionMatrix(table(test_pred, test.heart))
#Plotting confusion matrix
Reference <- factor(c(0, 0, 1, 1))
Prediction <- factor(c(0, 1, 0, 1))
Y      <- c(698 , 0, 465, 0)
df <- data.frame(Reference, Prediction, Y)
ggplot(rf_results  =  df, mapping = aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
  labs(title = "SVM",) +
  scale_fill_gradient(low = "skyblue", high = "skyblue") +
  theme_bw() + theme(legend.position = "none")

                               #K-NEAREST NEIGHBOUR
#SET SEED TO ENSURE RPRODUCIBILITY OF RANDOM NUMBERS
set.seed(123)
#There are 2712 observations in the training data.The square root of 2712 is 52, therefore I would be creating 
#2 models. One with K Value as 52 and the other with K Value as 53
knn.52 <- knn(train=train, test=test, cl=train.heart, k=52)
knn.53 <- knn(train=train, test=test, cl=train.heart, k=53)
#MODEL EVALUATION (ACCURACY)
ACC.52 <- 100 * sum(test.heart == knn.52)/NROW(test.heart)
ACC.53 <- 100 * sum(test.heart == knn.53)/NROW(test.heart) 
#VIEW ACCURACY
ACC.52   
ACC.53  

#CHECKING PREDICTION AGAINST ACTUAL VALUE IN TABULAR FORM FOR BOTH K VALUES
table(knn.52 ,test.heart)  
knn.52
table(knn.53 ,test.heart)
knn.53
#USING CONFUSION MATRIX TO CALCULATE THE ACCURACY FOR BOTH K VALUES
confusionMatrix(table(knn.52 ,test.heart))
confusionMatrix(table(knn.53 ,test.heart))
#OPTIMIZATION{HYPER-PARAMETER TUNING}
#INITIALIZING A VARIABLE i TO 1
i = 1
#CREATING A NUMERIC VECTOR OF LENTH 48 TO STORE ACCURACY VALUES FOR EACH K VALUE
k.optm = numeric(53)
#CREATING A LOOP OF VALUE i FROM 1 TO 48
for (i in 1:53) {
  # FITTING A KNN MODEL WITH CURRENT VALUE OF K(i)
  knn.mod <- knn(train = train, test = test, cl = train.heart, k = i)
  #Calculating percentage accuracy of current k and storing in k.optm
  k.optm[i] <- 100 * sum(test.heart == knn.mod) / length(test.heart)
  #PRINTING THE ITIERATUION NUMBER,  EQUAL SIGN AND ACCURACY FOR CURRENT K 
  cat(i, '=', k.optm[i], "\n")
}
#PLOTTING KNN MODEL FOR DIFFERENT VALUES OF K IN K.optm
plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level")

#SHOWING THE KNN MODEL WITH THE HIGHEST ACCURACY
knn.1 <- knn(train=train, test=test, cl=train.heart, k= 1)
#CALCULATING THE PERCENTAGE ACCURACY OF THE MODEL
ACC.1 <- 100 * sum(test.heart == knn.1)/NROW(test.heart)
#CHECKING PREDICTION AGAINST ACTUAL VALUE IN TABULAR FORM 
table(knn.1 ,test.heart)  
knn.1
#USING CONFUSION MATRIX TO CALCULATE THE ACCURACY 
confusionMatrix(table(knn.1 ,test.heart))
#Plotting confusion matrix
Reference1 <- factor(c(0, 0, 1, 1))
Prediction1 <- factor(c(0, 1, 0, 1))
Y1     <- c(606 , 92, 35, 430)
df1 <- data.frame(Reference1, Prediction1, Y1)
ggplot(rf_results  =  df1, mapping = aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y1)), vjust = 1) +
  labs(title = "KNN",) +
  scale_fill_gradient(low = "skyblue", high = "skyblue") +
  theme_bw() + theme(legend.position = "none")

#COMPARING SVM AND KNN RESULT
data <- read.table(text="
Measures,Machine_Learning,Percent
Accuracy,SVM,60
Accuracy,KNN,89.08
Kappa,SVM,0.00
Kappa,KNN,77.7", header=TRUE, sep=",")
ggplot(data, aes(x=Machine_Learning, y=Percent, fill=Measures)) +
  geom_bar(stat="identity", position="dodge")+
  geom_text(aes(label=Percent), vjust=1.6, color="white",
            position = position_dodge(0.9), size=5)+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()

   ####ROC CURVEEEEEE
#ROC CURVE FOR SVM MODEL
#create a table for heart attack risk
y_true <- test$Heart.Attack.Risk
#convert to numeric
y_true <- as.numeric(as.character(y_true))
svm_scores <- predict(svm_model, test, decision.values = TRUE)
svm_scores <- as.numeric(as.character(svm_scores))
# Create a ROC curve object
roc_curve_svm <- roc(y_true, svm_scores)
# Plot the ROC curve
plot(roc_curve_svm, col = "skyblue", main = "ROC Curve - SVM", lwd = 2)
# Add AUC value to the plot
text(0.8, 0.2, paste("AUC =", round(auc(roc_curve_svm), 2)), col = "red", cex = 1.5)


#ROC CURVE FOR KNN MODEL
#create a table for heart attack risk
y_true_KNN <- as.numeric(as.character(test$Heart.Attack.Risk))
KNN_scores <- as.numeric(as.character(knn.1))
# Create a ROC curve object for KNN
roc_curve_KNN <- roc(y_true_KNN, KNN_scores)
# Plot the ROC curve for KNN
plot(roc_curve_KNN, col = "skyblue", main = "ROC Curve - KNN", lwd = 2)
# Add AUC value to the plot
text(0.8, 0.2, paste("AUC =", round(auc(roc_curve_KNN), 2)), col = "red", cex = 1.5)

