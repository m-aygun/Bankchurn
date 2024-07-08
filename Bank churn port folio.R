
pacman::p_load("tidyverse", "magrittr", "nycflights13", "gapminder",
               "Lahman", "maps", "lubridate", "pryr", "hms", "hexbin",
               "feather", "htmlwidgets", "broom", "pander", "modelr",
               "XML", "httr", "jsonlite", "lubridate", "microbenchmark",
               "splines", "ISLR2", "MASS", "testthat",  "caret",
               "RSQLite", "class", "babynames", "nasaweather", "pls",
               "fueleconomy", "viridis", "boot", "devtools", "tree", "leaps",
               "glmnet", "gam", "akima", "factoextra", "randomForest", "gbm", 
               "ggrepel", "GGally", "fmsb", "sjPlot", "rcompanion", "DT")
# Installer nødvendige pakker, hvis du ikke allerede har dem

# Indlæs pakkerne
library(pROC)
library(caret)
library(MASS)
library(gbm)

options(repos = c(CRAN = "https://cran.rstudio.com/"))



#bank_churn <- read.csv("Churn_Modelling.csv")
bank_churn <- read.csv("C:/Users/mette/OneDrive/Skrivebord/PB dataanalyse/Programmering og statistical learning/data/Portfolio/Churn_Modelling.csv")

bank_churn_bi <- bank_churn
bank_churn1 <- bank_churn
bank_churn_lasso <- bank_churn

#tjekker data og klasser
str(bank_churn)


#vi skaber en interaktiv tabel

datatable(bank_churn, caption = htmltools::tags$caption(
  style = 'caption-side: top; text-align: center;',
  'Table 1: ', htmltools::em('Bank data '))
)



# Beregn antallet af missing værdier i hver kolonne. No missing
bank_churn %>% purrr::map(~ sum(is.na(.)))

summary(bank_churn)

# Tæl forekomster af 0 og 1 i kolonnen 'Exited'
counts <- table(bank_churn$Exited)

# Print resultaterne
print(counts)


# There are no missing values, so we can proceed.

#the relevant variables are converted into factors.

# Type = factor and integers-----------------------------------------------------------

bank_churn_fact <- bank_churn %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.integer, as.factor)

str(bank_churn_fact)
bank_churn_fact$CreditScore <- as.integer(bank_churn_fact$CreditScore)
bank_churn_fact$Age <- as.integer(bank_churn_fact$Age)
bank_churn_fact$Tenure <- as.integer(bank_churn_fact$Tenure)
str(bank_churn_fact)

# Normalisering -----------------------------------------------------------

# Det er ikke nødvendigt at normalisere data i forbindelse med de statistiske
# modeller, som vi skal køre her. Der er forskellige typer af normalisering. 
# Vi ser her på følgende:

normalize <- function(x) {
  ((x-min(x))/(max(x)-min(x)))
}

bank_churn_fact <- bank_churn_fact %>% 
  mutate_if(is.numeric, normalize)

bank_churn_fact <- bank_churn_fact %>%
  dplyr::select(Exited, everything())

glimpse(bank_churn_fact)

#numeric_columns <- sapply(bank_churn_fact, is.numeric)

# Konverter numeriske variable fra dbl til int
#bank_churn_fact[numeric_columns] <- lapply(bank_churn_fact[numeric_columns], as.integer)

# Fravalg af customerID, Efternavn og ID ---------------------------------------------------

bank_churn_fact <- bank_churn_fact %>% 
  dplyr::select(-RowNumber, -CustomerId, -Surname)

names(bank_churn_fact)

#Ændre variablen Exited til Churn

bank_churn_fact <- bank_churn_fact %>%
  rename(Churn = Exited)

bank_churn_fact$Churn <- ifelse(bank_churn_fact$Churn == 1, "Yes", "No")
bank_churn_fact$Churn <- as.factor(bank_churn_fact$Churn)
str((bank_churn_fact))



# Install DT package
install.packages("DT")

# Load DT package
library(DT)

datatable(bank_churn_fact, 
          options = list(pageLength = 5, autoWidth = TRUE), 
          caption = 'Table 1: Telecommunication data')





#træningsdata og testdata


# Vi bruger funktionen set.seed, så vi kan reproducere vores resultater
set.seed(5)
# træningsdel og testdel:
intrain <- createDataPartition(y=bank_churn_fact$Churn,
                               p=0.70, list=FALSE)

# list=FALSE betyder at outputtet bliver en matrix og denne kan bruges 
# i koden nedenfor:

train <- bank_churn_fact[intrain,]
test <- bank_churn_fact[-intrain,]




FN_omk <- 200 

TP_omk <- 40

FP_omk <- TP_omk

TN_omk <- 0




test$no_churn <- "No"
FN_simple <- table(test$Churn, test$no_churn)[2]/(table(test$Churn, test$no_churn)[1]+
                                                    table(test$Churn, test$no_churn)[2])

omkostninger_mavefornemmelse <- FN_omk*FN_simple # per kunde


test <- dplyr::select(test, -no_churn) # We dont need no churn anymore.






bank_churn_kmeans <- bank_churn

str(bank_churn_kmeans)

bank_churn_kmeans <- bank_churn %>%
  dplyr::select(Exited, everything()) %>%
  dplyr::rename(Churn = Exited) %>%
  mutate(Churn = ifelse(Churn == 1, "Yes", "No"),
         Churn = as.factor(Churn),
         IsActiveMember = ifelse(IsActiveMember == "1", "Yes", "No"),
         HasCrCard = ifelse(HasCrCard == "1", "Yes", "No")) %>%
  dplyr::select(-RowNumber, -CustomerId, -Surname, -Churn)

str(bank_churn_kmeans)

# Specifikt standardisere de valgte kolonner
specific_columns <- c("NumOfProducts","CreditScore", "Age", "Tenure", "Balance", "EstimatedSalary")
Standard <- bank_churn_kmeans
Standard[specific_columns] <- scale(bank_churn_kmeans[specific_columns])

view(Standard)
# Indlæs caret pakken
library(caret)

# Opret et dummyVars objekt, specificer dit datasæt
# note: til ~ . betyder, at alle variabler bliver behandlet, men du kan også specificere specifikke variabler
dummies <- dummyVars(~ ., data = Standard)

# Anvend dummyVars objektet til dit datasæt for at skabe de One-Hot Encoded variabler
encoded_data <- predict(dummies, newdata = Standard)

# Konverter til en dataframe, hvis nødvendigt
encoded_data <- as.data.frame(encoded_data)

# Vis de første par rækker for at tjekke resultatet
head(encoded_data)

view(encoded_data)
str(encoded_data)

library(cluster)  # For silhouette analysis
library(factoextra)  # For visualizing clusters and elbow method

#elbow metoden for at bestemme det optimale antal clusters
set.seed(123)  # Sikrer reproducerbarhed
wss <- numeric(20)  # WSS for k fra 1 til 20

for (k in 1:20) {
  model <- kmeans(encoded_data, centers = k, nstart = 25)
  wss[k] <- model$tot.withinss
}

plot(1:20, wss, type = "b", xlab = "Antal af Clusters", ylab = "Total WSS", main = "Elbow Metode")

#det optimale antale clusters er 3, da kurven efter k=3 begynder at flade ud.


library(stats)

# Antager at dit standardiserede datasæt er gemt i et objekt kaldet Standard
set.seed(123)  # For reproducerbarhed
k_optimal <- 3  # Det antal clusters du har valgt

# Træn k-means modellen med det optimale antal clusters
kmeans_model <- kmeans(encoded_data, centers = k_optimal, nstart = 25)

# Se resultaterne
print(kmeans_model)
print(kmeans_model$centers)

# Tilføj cluster-tilhørsforhold til dit datasæt
bank_churn_fact$clusterKmeans <- as.factor(kmeans_model$cluster)
bank_churn_kmeans$clusterKmeans <- kmeans_model$cluster
bank_churn_lasso$clusterKmeans <- kmeans_model$cluster
bank_churn1$clusterKmeans <- kmeans_model$cluster
# Se de første par rækker for at bekræfte tilføjelsen af cluster-tilhørsforhold
head(bank_churn_kmeans)


#par(mfrow=c(1,1))
library(reshape2)

centers_long <- melt(kmeans_model$centers)
ggplot(centers_long, aes(x = Var2, y = value, fill = Var1)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Feature", y = "Centroid Value", fill = "Cluster")



hc.complete <- hclust(dist(encoded_data), method="complete")
hc.average <- hclust(dist(encoded_data), method="average")
hc.single <- hclust(dist(encoded_data), method="single")

# Opdel pladsen til at vise plots
par(mfrow=c(1,3))

# Plot hierarkiske klynger for forskellige linkages
plot(hc.complete, main="Complete Linkage", xlab="", sub="", cex=.9)
plot(hc.average, main="Average Linkage", xlab="", sub="", cex=.9)
plot(hc.single, main="Single Linkage", xlab="", sub="", cex=.9)


segments(0, 2, nrow(encoded_data), 2, col="red")


# Udskriv klyngerne for forskellige linkages
cutree(hc.complete, 3)
cutree(hc.average, 2)
cutree(hc.single, 2)
cutree(hc.single, 4)


#vi vælger complete linkage, k=4

#visualisering
library(reshape2)
library(ggplot2)

# Eksempeldata for klyngemodel
hc.complete <- hclust(dist(encoded_data), method="complete")
cluster_labels <- cutree(hc.complete, k = 3)

# Beregn klyngecentre
centers <- aggregate(encoded_data, by=list(cluster_labels), FUN=mean)

# Navngiv kolonner
colnames(centers)[-1] <- colnames(encoded_data)

# Lav en dataframe for klyngecentre i "long" format
centers_long <- melt(centers, id.vars="Group.1")

# Plot klyngecentre
ggplot(centers_long, aes(x = variable, y = value, fill = factor(Group.1))) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Feature", y = "Centroid Value", fill = "Cluster")

hc.complete <- hclust(dist(encoded_data), method="complete")
cluster_labels <- cutree(hc.complete, k = 4)

# Tilføj klyngemærker som en ekstra kolonne til dit datasæt
bank_churn_fact$ClusterHC <- factor(cluster_labels)
bank_churn_lasso$clusterKmean <-factor(cluster_labels)
bank_churn1$clusterHC <- factor(cluster_labels)

str(bank_churn_fact)




bank_churn_lasso <- bank_churn_lasso %>% 
  dplyr::select(-RowNumber, -CustomerId, -Surname)

names(bank_churn_lasso)

#Ændre variablen Exited til Churn

bank_churn_lasso <- bank_churn_lasso %>%
  dplyr::rename(Churn = Exited)

# Move Churn column to the first position
bank_churn_lasso <- bank_churn_lasso %>%
  dplyr::select(Churn, everything())


str(bank_churn_lasso)


x <- model.matrix(Churn ~ ., bank_churn_lasso)[, -1]
y <- bank_churn_lasso$Churn

grid <- 10^seq(10, -2, length = 100)
lasso.mod <- glmnet(x, y, alpha = 1, lambda = grid)

coef(lasso.mod)
dim(coef(lasso.mod))

names(bank_churn_lasso)

set.seed(5)
train <- sample(1:nrow(x), nrow(x)*2/3)
test <- (-train)
y.test <- y[test]


set.seed(5)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 1)
par(mfrow=c(1,1))
plot(cv.out)

bestlam <- cv.out$lambda.min
bestlam # optimale 
cv.out$lambda 
cv.out$lambda.1se

log(bestlam)

lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test, ]) 
mse_lasso <- mean((lasso.pred - y.test)^2)

mse_lasso

out <- glmnet(x, y, alpha = 1)
lasso.coef <- predict(out, type = "coefficients", s = bestlam)[1:8, ]

#print koeficienterne
lasso.coef

lasso.coef[lasso.coef != 0]



#training and test data

train <- bank_churn_fact[intrain,]
test <- bank_churn_fact[-intrain,]
view(bank_churn_fact)
names(test)

outcome <- "Churn"

#Vi så i lasso regression, hvilke variabler der ikke havde relevans, disse eksluderes

variables <- c( ".", "ClusterHC", "clusterKmeans", "HasCrCard", "NumOfProducts", "IsActiveMember")



f <- as.formula(paste(outcome, 
                      paste(variables, collapse = " - "), sep = " ~ "))

str(bank_churn_fact)
str(bank_churn1)
# Vi fitter en logistisk regressionsmodel:

fit_logit <- glm(f, data=train, family = "binomial")

# Forudsige sandsynlighederne på træningsdataene
predictions <- predict(fit_logit, type = "response")

# Opret ROC-kurven og beregn AUC
roc_curve <- roc(train$Churn, predictions)

# Vis AUC-værdien
auc_roc <- auc(roc_curve)
print(auc_roc)

# Visualiser ROC-kurven
roc_data <- data.frame(
  specificity = rev(roc_curve$specificities),
  sensitivity = rev(roc_curve$sensitivities)
)

ggplot(roc_data, aes(x = specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(linetype = "dashed", color = "red") +
  labs(
    title = paste("ROC Curve (AUC =", round(auc_roc, 2), ")"),
    x = "1 - Specificity",
    y = "Sensitivity"
  ) +
  theme_minimal()


# Foretage prædiktioner på testsættet og gemmer dem i et objekt, churn_probs:

churn_probs <- predict(fit_logit, test, type = "response")

head(churn_probs)

# Kan vi gøre det bedre end den simple model (og modellen med 50/50 split)
# Loop:

thresh <- seq(0.01, 1.0, length = 100)
omk <- rep(0, length(thresh))

for (i in 1:length(thresh)) {
  glm.pred <- rep("No", length(churn_probs))
  glm.pred[churn_probs>thresh[i]] <- "Yes"
  glm.pred <- as.factor(glm.pred)
  x <- confusionMatrix(glm.pred, test$Churn, positive = "Yes")
  total <- x$table[1] + x$table[2] + x$table[3] + x$table[4]
  TN <- x$table[1]/total
  FP <- x$table[2]/total
  FN <- x$table[3]/total
  TP <- x$table[4]/total
  omk[i] <- FN*FN_omk + TP*TP_omk + FP*FP_omk + TN*0
}



glm.pred <- rep("No", length(churn_probs))
glm.pred[churn_probs>0.5] <- "Yes"
glm.pred <- as.factor(glm.pred)
x <- confusionMatrix(glm.pred, test$Churn, positive = "Yes")
total <- x$table[1] + x$table[2] + x$table[3] + x$table[4]
TN <- x$table[1]/total
FP <- x$table[2]/total
FN <- x$table[3]/total
TP <- x$table[4]/total
omk_simple <- FN*FN_omk + TP*TP_omk + FP*FP_omk + TN*0

# adding a column with the propability of the customer churning based on the optimal threshold.

bank_churn_bi <- bank_churn_fact

bank_churn_bi$Log_Churn_Prob <- predict(fit_logit, newdata = bank_churn_fact, type = "response")



model <- c(rep("optimized", 100), "simple")
cost_thresh <- c(omk, omk_simple)
thresh_plot <- c(thresh, 0.5)



dataII <- data.frame(
  model,
  cost_thresh,
  thresh_plot
)

optimized_cost <- min(dataII$cost_thresh)
threshold_0_5_cost <- subset(dataII, thresh_plot == 0.5)$cost_thresh

ggplot(dataII, aes(x = thresh_plot, y = cost_thresh, group = model, colour = model)) +
  geom_line() +
  geom_point() +
  geom_text(data = subset(dataII, cost_thresh == optimized_cost), 
            aes(label = paste("(", round(thresh_plot, 2), ",", round(cost_thresh, 2), ")"), 
                x = thresh_plot + 0.05, y = cost_thresh), 
            vjust = -0.5, hjust = 0) +
  geom_text(data = subset(dataII, thresh_plot == 0.5), 
            aes(label = paste("(", round(thresh_plot, 2), ",", round(cost_thresh, 2), ")"), 
                x = thresh_plot + 0.05, y = cost_thresh), 
            vjust = 1.5, hjust = 0)





# Vi finder først rækken, der svarer til threshold 0.5
threshold_0_5_row <- subset(dataII, thresh_plot == 0.5)

# Vi tager den første værdi af omkostningerne ved dette threshold
threshold_0_5_cost <- threshold_0_5_row$cost_thresh[1]

# Find det index, hvor omkostningen er minimal
optimal_index <- which.min(dataII$cost_thresh)


# Gem det optimale threshold og omkostningerne ved dette threshold
optimal_threshold_log <- dataII$thresh_plot[optimal_index]
optimal_cost_log <- dataII$cost_thresh[optimal_index]


# Og endelig kan vi udskrive værdierne for thrshold 0,5
print(paste("Threshold 0.5:", 0.5))
print(paste("Omkostninger ved threshold 0.5:", threshold_0_5_cost))

# Print dem ud
print(paste("Optimalt threshold:", optimal_threshold_log))
print(paste("Omkostninger ved optimalt threshold:", optimal_cost_log))


#adding a column with a binary outcome if the customer is churning based on the logistic regression optimal threshold
bank_churn_bi <- bank_churn_bi %>%
  mutate(Log_Churn_Prediction = ifelse(Log_Churn_Prob > optimal_threshold_log, "Yes", "No"))






#own calculations
besparelse_mavefornemmelse <- omkostninger_mavefornemmelse - min(omk)

#total cost of own calculations
besparelse_mavefornemmelse*10000







# Indlæs pakkerne
library(MASS)
library(pROC)
library(ggplot2)


train <- bank_churn_fact[intrain,]
test <- bank_churn_fact[-intrain,]





lda.fit <- lda(f, data=train)
lda.pred <- data.frame(predict(lda.fit, test))

# Check the structure of the predicted object
str(lda.pred)


# Opret ROC-kurven og beregn AUC
roc_curve_lda <- roc(test$Churn, lda.pred$posterior.Yes)

# Vis AUC-værdien
auc_roc_lda <- auc(roc_curve_lda)
print(auc_roc_lda)

# Visualiser ROC-kurven
roc_data_lda <- data.frame(
  specificity = rev(roc_curve_lda$specificities),
  sensitivity = rev(roc_curve_lda$sensitivities)
)

ggplot(roc_data_lda, aes(x = specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(linetype = "dashed", color = "red") +
  labs(
    title = paste("ROC Curve for LDA Model (AUC =", round(auc_roc_lda, 2), ")"),
    x = "1 - Specificity",
    y = "Sensitivity"
  ) +
  theme_minimal()


omk_lda <- rep(0,length(thresh))
thresh <- seq(0.01, 1.0, length = 100)

results_lda <- data.frame(threshold = numeric(), cost = numeric())

# Kør for løkken for at beregne omkostningerne ved forskellige thresholds
for (i in seq_along(thresh)) {
  # Skaber forudsigelser baseret på threshold
  glm.pred <- ifelse(lda.pred$posterior.Yes > thresh[i], "Yes", "No")
  glm.pred <- factor(glm.pred, levels = c("No", "Yes"))
  
  # Beregn confusion matrix
  cm <- confusionMatrix(glm.pred, test$Churn, positive = "Yes")
  total <- sum(cm$table)
  TN <- cm$table[1] / total
  FP <- cm$table[2] / total
  FN <- cm$table[3] / total
  TP <- cm$table[4] / total
  
  # Beregn omkostninger
  cost <- FN * FN_omk + TP * TP_omk + FP * FP_omk + TN * TN_omk
  
  # Tilføj til results_lda
  results_lda <- rbind(results_lda, data.frame(threshold = thresh[i], cost = cost))
}




# Find det threshold med den laveste omkostning
optimal_threshold_lda <- results_lda$threshold[which.min(results_lda$cost)]
optimal_cost_lda <- min(results_lda$cost)


ggplot(results_lda, aes(x = threshold, y = cost)) +
  geom_line() +
  geom_point(data = subset(results_lda, threshold == optimal_threshold_lda), 
             aes(x = threshold, y = cost), color = "red", size = 3) +
  geom_text(data = subset(results_lda, threshold == optimal_threshold_lda), 
            aes(label = paste("(", round(threshold, 2), ",", round(cost, 2), ")"), 
                x = threshold + 0.05, y = cost), 
            vjust = -0.5, hjust = 0, color = "red") +
  labs(title = "Omkostninger ved forskellige thresholds for LDA",
       x = "Threshold",
       y = "Omkostning") +
  annotate("text", x = optimal_threshold_lda, y = min(results_lda$cost), 
           label = paste("Threshold:", round(optimal_threshold_lda, 2)), 
           hjust = 1, vjust = 1, size = 5, color = "red")



lda.fit <- lda(f, data=train)
lda.pred <- predict(lda.fit, test)

# Antag, at du allerede har beregnet dit optimale threshold og gemt det i variablen optimal_threshold

# Trin 2: Generer optimal_predictions
optimal_predictions <- ifelse(lda.pred$posterior[, "Yes"] > optimal_threshold_lda, "Yes", "No")

# Trin 3: Tjek længden af optimal_predictions
print(length(optimal_predictions)) # Dette skal matche antallet af observationer i testdatasættet
print(length(test$Churn))


# Beregn forudsigelser baseret på det optimale threshold
optimal_predictions <- ifelse(lda.pred$posterior[, "Yes"] > optimal_threshold_lda, "Yes", "No")
optimal_predictions <- factor(optimal_predictions, levels = c("No", "Yes"))

bank_churn_bi$LDA_Churn_Prob <- predict(lda.fit, newdata = bank_churn_fact)$posterior[, "Yes"]

# Tilføj en ny kolonne baseret på det optimale QDA threshold
bank_churn_bi <- bank_churn_bi %>%
  mutate(LDA_Churn_Prediction = ifelse(LDA_Churn_Prob > optimal_threshold_lda, "Yes", "No"))




# Beregn confusion matrix baseret på disse forudsigelser
cm <- confusionMatrix(optimal_predictions, test$Churn, positive = "Yes")
print(cm)
# Udskriv antallet af TP, FP, FN, og TN
#cat("True Positives (TP):", cm$table["Yes","Yes"], "\n")
#cat("False Positives (FP):", cm$table["No","Yes"], "\n")
#cat("False Negatives (FN):", cm$table["Yes","No"], "\n")
#cat("True Negatives (TN):", cm$table["No","No"], "\n")




# QDA ---------------------------------------------------------------------


#qda

# Load necessary library
library(caret)


view(bank_churn_fact)

bank_churn_fact <- as.data.frame(bank_churn_fact)
class(bank_churn_fact)
class(bank_churn_fact$IsActiveMember.0)
class(bank_churn_fact$IsActiveMember.1)

class(bank_churn_fact)



# Split data into training and test sets
set.seed(123)  # For reproducibility
intrain <- createDataPartition(bank_churn_fact$Churn, p = 0.7, list = FALSE)
train <- bank_churn_fact[intrain,]
test <- bank_churn_fact[-intrain,]

# Ensure Churn is a factor with the correct levels
train$Churn <- factor(train$Churn, levels = c("No", "Yes"))
test$Churn <- factor(test$Churn, levels = c("No", "Yes"))



# Fit QDA model
qda.fit <- qda(f, data=train)
qda.pred <- predict(qda.fit, test)

# Check the structure of the predicted posterior probabilities
str(qda.pred)

# Opret ROC-kurven og beregn AUC
roc_curve_qda <- roc(test$Churn, qda.pred$posterior[, "Yes"])

# Vis AUC-værdien
auc_roc_qda <- auc(roc_curve_qda)
print(paste("AUC for QDA model:", auc_roc_qda))

# Visualiser ROC-kurven
roc_data_qda <- data.frame(
  specificity = rev(roc_curve_qda$specificities),
  sensitivity = rev(roc_curve_qda$sensitivities)
)

ggplot(roc_data_qda, aes(x = specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(linetype = "dashed", color = "red") +
  labs(
    title = paste("ROC Curve for QDA Model (AUC =", round(auc_roc_qda, 2), ")"),
    x = "1 - Specificity",
    y = "Sensitivity"
  ) +
  theme_minimal()


print(length(glm.pred)) # Dette skal matche antallet af observationer i testdatasættet
print(length(test$Churn))


str(glm.pred)
head(qda.pred$posterior)


sum(is.na(qda.pred$posterior[, "Yes"]))




omk_qda <- rep(0,length(thresh))
thresh <- seq(0.01, 1.0, length = 100)

results_qda <- data.frame(threshold = numeric(), cost = numeric())

# Loop to calculate costs for different thresholds
for (i in seq_along(thresh)) {
  # Create predictions based on the threshold
  glm.pred <- ifelse(qda.pred$posterior[, "Yes"] > thresh[i], "Yes", "No")
  glm.pred <- factor(glm.pred, levels = c("No", "Yes"))
  
  # Check lengths of predictions and test data
  print(length(glm.pred)) # This should match the number of observations in the test dataset
  print(length(test$Churn))
  
  # Beregn confusion matrix
  cm <- confusionMatrix(glm.pred, test$Churn, positive = "Yes")
  total <- sum(cm$table)
  TN <- cm$table[1] / total
  FP <- cm$table[2] / total
  FN <- cm$table[3] / total
  TP <- cm$table[4] / total
  
  # Beregn omkostninger
  cost <- FN * FN_omk + TP * TP_omk + FP * FP_omk + TN * TN_omk
  
  # Tilføj til results_qda
  results_qda <- rbind(results_qda, data.frame(threshold = thresh[i], cost = cost))
}



# Find det threshold med den laveste omkostning
optimal_threshold_qda <- results_qda$threshold[which.min(results_qda$cost)]
optimal_cost_qda <- min(results_qda$cost)



ggplot(results_qda, aes(x = threshold, y = cost)) +
  geom_line() +
  geom_point(data = subset(results_qda, threshold == optimal_threshold_qda), 
             aes(x = threshold, y = cost), color = "red", size = 3) +
  geom_text(data = subset(results_qda, threshold == optimal_threshold_qda), 
            aes(label = paste("(", round(threshold, 2), ",", round(cost, 2), ")"), 
                x = threshold + 0.05, y = cost), 
            vjust = -0.5, hjust = 0, color = "red") +
  labs(title = "Omkostninger ved forskellige thresholds for QDA",
       x = "Threshold",
       y = "Omkostning") +
  annotate("text", x = optimal_threshold_qda, y = min(results_qda$cost), 
           label = paste("Threshold:", round(optimal_threshold_qda, 2)), 
           hjust = 1, vjust = 1, size = 5, color = "red")

# Print den optimale threshold og omkostning
#print(optimal_threshold_qda)
#print(optimal_cost_qda)

qda.fit <- qda(f, data=train)
qda.pred <- predict(qda.fit, test)

# Antag, at du allerede har beregnet dit optimale threshold og gemt det i variablen optimal_threshold

# Trin 2: Generer optimal_predictions
optimal_predictions <- ifelse(qda.pred$posterior[, "Yes"] > optimal_threshold_qda, "Yes", "No")

# Trin 3: Tjek længden af optimal_predictions
#print(length(optimal_predictions)) # Dette skal matche antallet af observationer i testdatasættet
print(length(test$Churn))


# Beregn forudsigelser baseret på det optimale threshold
optimal_predictions <- ifelse(qda.pred$posterior[, "Yes"] > optimal_threshold_qda, "Yes", "No")
optimal_predictions <- factor(optimal_predictions, levels = c("No", "Yes"))

# Beregn confusion matrix baseret på disse forudsigelser
cm <- confusionMatrix(optimal_predictions, test$Churn, positive = "Yes")
print(cm)
# Udskriv antallet af TP, FP, FN, og TN

#cat("True Positives (TP):", cm$table["Yes","Yes"], "\n")
#cat("False Positives (FP):", cm$table["No","Yes"], "\n")
#cat("False Negatives (FN):", cm$table["Yes","No"], "\n")
#cat("True Negatives (TN):", cm$table["No","No"], "\n")


bank_churn_bi$QDA_Churn_Prob <- predict(qda.fit, newdata = bank_churn_fact)$posterior[, "Yes"]

# Tilføj en ny kolonne baseret på det optimale QDA threshold
#bank_churn_bi<- bank_churn_bi %>%
#  mutate(QDA_Churn_Prediction = ifelse(QDA_Churn_Prob > optimal_threshold_qda, "Yes", "No"))





# GBM ---------------------------------------------------------------------


#GBM med tuning grid


bank_churn_gbm <- bank_churn1 %>%
  dplyr::select(-RowNumber, -CustomerId, -Surname) %>%
  rename(Churn = Exited) %>%
  mutate(Churn = factor(Churn, levels = c(0, 1)))


str(bank_churn_gbm)

library(gbm)

# Sikrer reproducerbarhed
set.seed(123)

# Oprette trænings- og testdatasæt
trainIndex <- createDataPartition(bank_churn_gbm$Churn, p = .7, 
                                  list = FALSE, 
                                  times = 1)

trainData <- bank_churn_gbm[trainIndex, ]
testData <- bank_churn_gbm[-trainIndex, ]

# Omdøb faktorniveauerne til "Class1" og "Class0"
trainData$Churn <- factor(trainData$Churn, levels = c(0, 1), labels = c("Class0", "Class1"))

# Opdater også testData, hvis du har det
testData$Churn <- factor(testData$Churn, levels = c(0, 1), labels = c("Class0", "Class1"))




# Definer et grid af parametre at prøve (kun n.trees og shrinkage)
tuneGrid <- expand.grid(.n.trees = c(100, 500, 1000),
                        .shrinkage = c(0.01, 0.05, 0.1),
                        .interaction.depth = 1,
                        .n.minobsinnode = 10) # Tilføjer n.minobsinnode med en værdi af 10


control <- trainControl(method = "cv", number = 5, classProbs = TRUE, summaryFunction = twoClassSummary)

gbmFit <- train(Churn ~ ., data = trainData, method = "gbm",
                trControl = control, verbose = FALSE,
                tuneGrid = tuneGrid,
                metric = "ROC",
                distribution = "bernoulli")

# Se de bedste parametre
print(gbmFit$bestTune)

# Brug model til at lave forudsigelser
predictions <- predict(gbmFit, newdata = testData, type = "prob")




# Beregn AUC for at evaluere modellens præstation
library(pROC)
auc <- roc(testData$Churn, predictions[,2])
print(auc$auc)
plot(auc)






# Omkostningsberegning kan tilføjes her baseret på dine specifikke kriterier

# Beregn forudsagte sandsynligheder for testdatasættet
predicted_probs_gbm <- predict(gbmFit, newdata = testData, type = "prob")[,"Class1"]

# Beregn forudsagte klasser for testdatasættet ved et threshold på 0,5
predicted_class_gbm <- ifelse(predicted_probs_gbm > 0.5, "Class1", "Class0")

# Sørg for at både Predicted og Actual er faktorer med de samme niveauer
predicted_factor_gbm <- factor(predicted_class_gbm, levels = c("Class0", "Class1"))
actual_factor_gbm <- factor(testData$Churn, levels = c("Class0", "Class1")) # Juster variabelnavnet efter dit datasæt

# Tjek længden af predicted_factor_gbm og actual_factor_gbm
print(length(predicted_factor_gbm)) # Dette skal matche antallet af observationer i testdatasættet
print(length(actual_factor_gbm))






# Beregn og udskriv confusion matrix
cm <- confusionMatrix(predicted_factor_gbm, actual_factor_gbm)
print(cm)



# Definer thresholds
thresh <- seq(0.01, 1, by = 0.01)

# Initialiser en dataframe til at holde omkostningerne ved hvert threshold
omkostninger_gbm <- data.frame(threshold = numeric(), cost = numeric())

# Loop over hvert threshold for at beregne omkostningerne
for(t in thresh) {
  # Generer klassificering baseret på det aktuelle threshold
  predicted_class_gbm <- ifelse(predicted_probs_gbm > t, "Class1", "Class0")
  
  # Sørg for at både Predicted og Actual er faktorer med de samme niveauer
  predicted_factor_gbm <- factor(predicted_class_gbm, levels = c("Class0", "Class1"))
  actual_factor_gbm <- factor(testData$Churn, levels = c("Class0", "Class1"))
  
  # Beregn confusion matrix
  cm_gbm <- confusionMatrix(predicted_factor_gbm, actual_factor_gbm)
  
  # Ekstraher værdier fra confusion matrix
  TN_gbm <- cm_gbm$table["Class0","Class0"]
  FP_gbm <- cm_gbm$table["Class1","Class0"]
  FN_gbm <- cm_gbm$table["Class0","Class1"]
  TP_gbm <- cm_gbm$table["Class1","Class1"]
  
  # Beregn omkostningerne
  cost_gbm <- (FN_gbm * FN_omk + TP_gbm * TP_omk + FP_gbm * FP_omk + TN_gbm * TN_omk) / sum(cm_gbm$table)
  
  # Tilføj threshold og omkostninger til dataframe
  omkostninger_gbm <- rbind(omkostninger_gbm, data.frame(threshold = t, cost = cost_gbm))
}

# Find det optimale threshold og de tilhørende omkostninger
optimal <- omkostninger_gbm[which.min(omkostninger_gbm$cost), ]
print(optimal)





optimal_threshold_gbm <- optimal$threshold
optimal_cost_gbm <- optimal$cost
#print(paste("Optimalt threshold: ", optimal_threshold_gbm))
#print(paste("Omkostninger ved optimalt threshold: ", optimal_cost_gbm))


ggplot(omkostninger_gbm, aes(x = threshold, y = cost)) +
  geom_line() +
  geom_point(data = optimal, aes(x = threshold, y = cost), color = "red", size = 4) +
  geom_text(data = optimal, 
            aes(label = paste("(", round(threshold, 2), ",", round(cost, 2), ")"), 
                x = threshold + 0.05, y = cost), 
            vjust = -0.5, hjust = 0, color = "red") +
  labs(title = "Omkostninger ved forskellige thresholds for GBM",
       x = "Threshold",
       y = "Omkostning")

# Beregn sandsynligheder for hele datasættet med din GBM-model
predicted_probs_whole_gbm <- predict(gbmFit, newdata=bank_churn_gbm, type="prob")[,2]

# Tilføjer sandsynlighederne som en kolonne i bi datasættet
bank_churn_bi$GBM_Churn_Prob <- predicted_probs_whole_gbm

# Bestem det optimale threshold fra dine tidligere resultater
optimal_threshold_gbm <- optimal$threshold  # Sørg for, at dette er opdateret baseret på GBM-resultaterne

# Generer forudsigelser baseret på det optimale threshold for hele datasættet
optimal_predictions_gbm <- ifelse(predicted_probs_whole_gbm > optimal_threshold_gbm, "Yes", "No")

# Tilføjer en kolonne, der viser om en kunde churner på baggrund af det optimale GBM threshold
bank_churn_bi <- bank_churn_bi %>%
  mutate(GBM_Churn_Prediction = optimal_predictions_gbm)



# RANDOM FORREST ----------------------------------------------------------



# We are here using random forrest. 

bank_churn_RF <- bank_churn1

bank_churn_RF <- bank_churn1 %>%
  dplyr::select(Exited, everything()) %>%
  dplyr::rename(Churn = Exited) %>%
  mutate(Churn = ifelse(Churn == 1, "Yes", "No"),
         Churn = as.factor(Churn)) %>%
  dplyr::select(-RowNumber, -CustomerId, -Surname)

bank_churn_RF <- bank_churn_RF %>%
  mutate_if(is.character, as.factor)
sum(is.na(bank_churn_RF
))


set.seed(123) # Sikrer reproducerbarhed

# Opret train/test split
trainIndex <- createDataPartition(bank_churn_RF$Churn, p = .7, list = FALSE)
trainData <- bank_churn_RF[trainIndex,]
testData <- bank_churn_RF[-trainIndex,]

# Definer de værdier, du vil teste for mtry og n.trees
mtry_values <- c(2, 4, 6, 8)  # Eksempel: Test 2, 4, 6 og 8 for mtry
n_trees_values <- c(50, 100, 250, 500, 1000)  # Eksempel: Test 50, 100, 250, 500 og 1000 for n.trees

# Initialiser variabler til at holde den bedste model og dens nøjagtighed
best_accuracy <- 0
best_model <- NULL
best_mtry <- NULL
best_ntrees <- NULL

# Løkke til at teste forskellige kombinationer af mtry og n.trees
for (mtry in mtry_values) {
  for (n_trees in n_trees_values) {
    # Byg random forest-modellen med aktuelle mtry og n.trees værdier
    rfModel <- randomForest(Churn ~ ., data = trainData, mtry = mtry, ntree = n_trees)
    
    # Forudsige testdatasættet
    predictions <- predict(rfModel, testData)
    
    # Evaluér modellens præstation
    confMat <- confusionMatrix(predictions, testData$Churn)
    
    # Beregn nøjagtighed (accuracy) og sammenlign med den bedste fundne
    accuracy <- confMat$overall['Accuracy']
    if (accuracy > best_accuracy) {
      best_accuracy <- accuracy
      best_model <- rfModel
      best_mtry <- mtry
      best_ntrees <- n_trees
    }
  }
}


print(best_model)




# Forudsige sandsynligheder for klassen "Yes"

predicted_probs <- predict(best_model, newdata = testData, type = "prob")[, "Yes"]

# Definer thresholds
thresh <- seq(0.01, 1, by = 0.01)

# Initialiser en dataframe til at holde omkostningerne ved hvert threshold
omkostninger <- data.frame(threshold = numeric(), cost = numeric())

# Loop over hvert threshold
for(t in thresh){
  # Generer forudsigelser baseret på det aktuelle threshold
  predicted_class <- ifelse(predicted_probs > t, "Yes", "No")
  
  # Sørg for at både Predicted og Actual er faktorer med de samme niveauer
  predicted_factor <- factor(predicted_class, levels = c("No", "Yes"))
  actual_factor <- factor(testData$Churn, levels = c("No", "Yes"))
  
  # Beregn confusion matrix
  cm <- table(Predicted = predicted_factor, Actual = actual_factor)
  
  # Beregn omkostninger. Brug safe indexing for at undgå "subscript out of bounds" fejl.
  TN <- ifelse(!is.na(cm["No","No"]), cm["No","No"], 0)
  FP <- ifelse(!is.na(cm["Yes","No"]), cm["Yes","No"], 0)
  FN <- ifelse(!is.na(cm["No","Yes"]), cm["No","Yes"], 0)
  TP <- ifelse(!is.na(cm["Yes","Yes"]), cm["Yes","Yes"], 0)
  total_omk <- (FN * FN_omk + TP * TP_omk + FP * FP_omk + TN * TN_omk) / sum(cm)
  
  # Tilføj threshold og omkostninger til dataframe
  omkostninger <- rbind(omkostninger, data.frame(threshold = t, cost = total_omk))
}


# Find det optimale threshold
optimal <- omkostninger[which.min(omkostninger$cost), ]
print(optimal)




# Udskriv det optimale threshold og omkostninger separat
optimal_threshold_rf <- optimal$threshold
optimal_cost_rf <- optimal$cost
#print(paste("Optimalt threshold: ", optimal_threshold_rf))
#print(paste("Omkostninger ved optimalt threshold: ", optimal_cost_rf))

# Plot omkostninger mod threshold

ggplot(omkostninger, aes(x = threshold, y = cost)) +
  geom_line() +
  geom_point(data = optimal, aes(x = threshold, y = cost), color = "red", size = 4) +
  geom_text(data = optimal, 
            aes(label = paste("(", round(threshold, 2), ",", round(cost, 2), ")"), 
                x = threshold + 0.05, y = cost), 
            vjust = -0.5, hjust = 0, color = "red") +
  labs(title = "Omkostninger ved forskellige thresholds for Random Forest",
       x = "Threshold",
       y = "Omkostning")


# Trin 1: Beregn sandsynligheder for hele datasættet bank_churn_RF
predicted_probs_whole <- predict(best_model, newdata=bank_churn_RF, type="prob")[,"Yes"]

# Trin 2: Tilføj sandsynlighederne som en ny kolonne til bank_churn_bi datasættet
bank_churn_bi$RF_Churn_Prob <- predicted_probs_whole

# Trin 3: Konverter sandsynligheder til forudsigelser baseret på det optimale threshold
# og tilføj disse forudsigelser som en anden ny kolonne
bank_churn_bi$RF_Churn_Prediction <- ifelse(predicted_probs_whole > optimal_threshold_rf, "Yes", "No")






# Installer nødvendige pakker, hvis du ikke allerede har dem
install.packages("pROC")

# Indlæs pakkerne
library(pROC)

# Antag, at dine ROC-objekter og AUC-værdier er beregnet som følger:
# Logistisk Regression
roc_curve_logit <- roc(train$Churn, predictions)
auc_roc_logit <- auc(roc_curve_logit)

# LDA
roc_curve_lda <- roc(test$Churn, lda.pred$posterior[, "Yes"])
auc_roc_lda <- auc(roc_curve_lda)

# QDA
roc_curve_qda <- roc(test$Churn, qda.pred$posterior[, "Yes"])
auc_roc_qda <- auc(roc_curve_qda)

# GBM
predicted_probs_gbm <- predict(gbmFit, newdata = testData, type = "prob")[,"Class1"]
roc_curve_gbm <- roc(testData$Churn, predicted_probs_gbm)
auc_roc_gbm <- auc(roc_curve_gbm)

# Random Forest
predicted_probs_rf <- predict(best_model, newdata = testData, type = "prob")[, "Yes"]
roc_curve_rf <- roc(testData$Churn, predicted_probs_rf)
auc_roc_rf <- auc(roc_curve_rf)

# Plot den første ROC-kurve (for eksempel Logistisk Regression)
plot(roc_curve_logit, col="red", main="Sammenligning af ROC Kurver", xlim=c(0, 1), ylim=c(0, 1))

# Tilføj yderligere ROC-kurver
plot(roc_curve_lda, add=TRUE, col="blue")
plot(roc_curve_qda, add=TRUE, col="green")
plot(roc_curve_gbm, add=TRUE, col="purple")
plot(roc_curve_rf, add=TRUE, col="orange")

# Tilføj en legende
legend("bottomright", legend=c("Logistisk Regression", "LDA", "QDA", "GBM", "Random Forest"),
       col=c("red", "blue", "green", "purple", "orange"), lty=1)

# Tilføj AUC værdier til plottet
text(0.4, 0.2, labels=paste("AUC Logistisk Regression: ", round(auc_roc_logit, 3)), col="red", cex=0.8)
text(0.4, 0.15, labels=paste("AUC LDA: ", round(auc_roc_lda, 3)), col="blue", cex=0.8)
text(0.4, 0.1, labels=paste("AUC QDA: ", round(auc_roc_qda, 3)), col="green", cex=0.8)
text(0.4, 0.05, labels=paste("AUC GBM: ", round(auc_roc_gbm, 3)), col="purple", cex=0.8)
text(0.4, 0.0, labels=paste("AUC Random Forest: ", round(auc_roc_rf, 3)), col="orange", cex=0.8)


































