### Code Summary: ########################################################################
#
#
#
### Basic packages: ######################################################################
repo <- "http://cran.us.r-project.org"

# Required packages:
if(!require(randomForest)) install.packages("randomForest", repos = repo, dependencies = TRUE)
if(!require(data.table))   install.packages("data.table",   repos = repo, dependencies = TRUE)
if(!require(tidyverse))    install.packages("tidyverse",    repos = repo, dependencies = TRUE)
if(!require(caret))        install.packages("caret",        repos = repo, dependencies = TRUE)
if(!require(dplyr))        install.packages("dplyr",        repos = repo, dependencies = TRUE)

library(randomForest)
library(data.table)
library(tidyverse)
library(caret)
library(dplyr)

rm(repo)

### Data set: ############################################################################
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/cmc/cmc.data"

# Download
setwd(getwd())
download.file(url = url, destfile = "cmc.data")
rm(url)

# Attribute Information:
#  1. Wife's age                     (numerical)
#  2. Wife's education               (categorical)      1=low, 2, 3, 4=high
#  3. Husband's education            (categorical)      1=low, 2, 3, 4=high
#  4. Number of children ever born   (numerical)
#  5. Wife's religion                (binary)           0=Non-Islam, 1=Islam
#  6. Wife's now working?            (binary)           0=Yes, 1=No
#  7. Husband's occupation           (categorical)      1, 2, 3, 4
#  8. Standard-of-living index       (categorical)      1=low, 2, 3, 4=high
#  9. Media exposure                 (binary)           0=Good, 1=Not good
#  10. Contraceptive method used     (class attribute)  1=No-use 
#                                                       2=Long-term
#                                                       3=Short-term

# Read data
cmc <- read.csv("cmc.data", sep = ",")
colnames(cmc) <- c("wife_age", "wife_ed", "husband_ed", "n_children", "wife_religion",
                   "wife_work", "husband_occ", "sol_index", "media_expo", "cm_use")

# Data partition
set.seed(2021, sample.kind = "Rounding")
test_index <- createDataPartition(cmc$cm_use, times = 1, p = .1, list = FALSE)

train <- cmc[-test_index,]
test  <- cmc[test_index,]

rm(test_index)

### Data Visualization: ##################################################################
# Correlation heatmap
cor_heatmap <- heatmap(cor(cmc))

# 
cmc %>% ggplot(aes())

### Data Classification: #################################################################
#
#
## KNN and Random Forest
# Tuning
control <- trainControl(method = "cv", number = 5)
grid <- data.frame(mtry = seq(10,500,10))

train_rf <- train(train[,-10],
                  as.factor(train[,10]),
                  method = "rf",
                  ntree = 200,
                  trControl = control,
                  tuneGrid = grid,
                  nSamp = 5000)

ggplot(train_rf)
train_rf$bestTune

fit_rf <- randomForest(train[,-10],
                       as.factor(train[,10]),
                       minNode = train_rf$bestTune$mtry)




##########################################################################################