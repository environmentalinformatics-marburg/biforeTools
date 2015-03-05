cat("\014")
################################################################################
##  
##  BiFoRe Scripts
##
##  Perform Random Forest classification for Orthoptera prevalence 
##  with lvl0300 dataset
##
##  1. 100 times Stratified sampling of plots
##  2. Split dataset into training and testing data
##  3. Perform Random Forest Classification
##  4. Extract confusion matrix & variable importance for all 100 samples and
##     average values
##  5. Model validation:
##     - Confusion Matrix
##     - Classification Errors
##     - Accuracy
##     - Kappa
##     - Sensitivity
##     - Specificity
##     - DetectionRate
##  
##  Version: 2015-02-14
##  
################################################################################
##
##  Copyright (C) 2015 Simon Schlauss (sschlauss@gmail.com)
##
##
##  This file is part of BiFoRe.
##  
##  BiFoRe is free software: you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation, either version 3 of the License, or
##  (at your option) any later version.
##  
##  BiFoRe is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##  
##  You should have received a copy of the GNU General Public License
##  along with BiFoRe.  If not, see <http://www.gnu.org/licenses/>.
##  
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("sampling", 
         "foreach", 
         "doParallel", 
         "caret", 
         "e1071", 
         "randomForest",
         "miscTools")

lapply(lib, function(...) require(..., character.only = TRUE))

## Set working directory
# setwd("/home/sschlauss/")
setwd("D:/")

## Set number of CPU cores
ncores <- detectCores()-1

## Set number of Random Forest runs
rf.runs <- 100

## Set size of training data (percentage) eg.: .75 for 75 %
train.part <- .75  # train.part > 0 && < 1

## Set Random Forest tuning parameter "mtry" and "ntree"
# Optimal mtry = sqrt(p) -> where p is number of predictor variables
mtrys <- c(1,2,3,4,5,6,7,8,9,10)

trees <- 500


## Runtime calculation
starttime <- Sys.time()

### Set filepaths ##############################################################

path.csv                  <- "Code/bifore/src/csv/"
path.testing              <- paste0(path.csv, "lvl0400_2015-01-24/")

file.in.0300              <- paste0(path.csv,"lvl0300_biodiversity_data.csv")
file.out.rf.output        <- paste0(path.testing, "lvl0400_rf_all_25test.csv")
file.out.validation       <- paste0(path.testing, "lvl0400_validation_25test.csv")
file.out.prediction       <- paste0(path.testing, "lvl0400_prediction_25test.csv")
file.out.importance       <- paste0(path.testing, "lvl0400_importance_25test.csv")
file.out.prevalence       <- paste0(path.testing, "lvl0400_prevalence.csv")
file.out.validation.final <- paste0(path.testing, "lvl0400_final_validation_25test.csv")

if (!file.exists(path.testing)) {dir.create(file.path(path.testing))}


### Import data ################################################################

data.raw <- read.csv2(file.in.0300,
                      dec = ",",
                      header = TRUE,
                      stringsAsFactors = FALSE)


### Subset data ################################################################

## Remove bands containing NA values
data.raw <- cbind(data.raw[1:8],  # basics
                  data.raw[10:13],  # basics
                  data.raw[9],  # no.of.species
                  data.raw[14:178],  # species
                  data.raw[179:188],  # greyvalues
                  data.raw[197:216])  # greyvalues

## Check greyvalues, diff and sd colums for NA values
anyNA(data.raw[179:ncol(data.raw)])


### Remove species with less than x observations in different plots ############
### Most abundant species ######################################################

## Set minimum observations in different plots
obs <- 15

data.list <- split(data.raw, data.raw$plot)
data.tmp.list <- do.call("rbind", lapply(seq(data.list), function(i) {
  matrix <- as.matrix(data.list[[i]][, 14:178])
  t <- apply(matrix, 2, sum, na.rm = TRUE)
  t[t == 0] <- NA
  t[t > 0] <- 1
  return(t)
}))

data.species.index <- which(apply(data.tmp.list, 
                                  2, 
                                  sum, 
                                  na.rm = TRUE) >= obs) + 13

data.cut <- data.raw[, c(1:13, data.species.index, 179:ncol(data.raw))]


### Subsetting data ############################################################

data.cut.basics <- data.cut[, which(colnames(data.cut) == "plot"):which(colnames(data.cut) == "coordN")]
data.cut.specno <- data.cut[length(names(data.cut.basics))+1]
data.cut.species <- data.cut[, (which(colnames(data.cut) == "coordN")+2):(which(colnames(data.cut) == "greyval_band_1")-1)]
data.cut.greyval <- data.cut[, which(colnames(data.cut) == "greyval_band_1"):which(colnames(data.cut) == "greyval_band_36")]


### calculate new no.of.prevalence (cut sophisticates old no.of.species) #######

data.cut.specno <- data.frame(apply(data.cut.species,
                                    1,
                                    function(x) sum(!is.na(x[1:ncol(data.cut.species)]))))
names(data.cut.specno) <- "no.of.prevalence"

data.cut <- cbind(data.cut.basics, 
                  data.cut.specno,
                  data.cut.species,
                  data.cut.greyval)


### Calculate prevalence #######################################################

## Read species dataframe as matrix
matrix.prevalence <- as.matrix(data.cut.species)

## Replace NA with 0
matrix.prevalence[is.na(matrix.prevalence)] <- 0

## Replace values >=1 with 1
matrix.prevalence <- ifelse(matrix.prevalence >= 1,1,0)

## Write prevalence into separate csv 
data.prevalence <- data.frame(matrix.prevalence)
df.prevalence <- cbind(data.cut.basics[1],
                       data.cut.basics[2],
                       data.prevalence)

write.table(df.prevalence,
            file = file.out.prevalence,
            quote = FALSE,
            col.names = TRUE,
            row.names = FALSE,
            sep = ";",
            dec = ",")

## Replace values "1" with "yes" and "0" with "no"
matrix.prevalence <- ifelse(matrix.prevalence >= 1,"yes","no")

## Recombine dataframes
data.cut <- cbind(data.cut.basics,
                  as.data.frame(matrix.prevalence),
                  data.cut.greyval)


### Stratified sampling ########################################################

## Function
#  df = data frame
#  class = column number of stratification variables
#  size = class size

stratified = function(df, class, size) {
  require(sampling)
  df.tmp = df[order(df[class]),]
  if (size < 1) {
    size = ceiling(table(df.tmp[class]) * size)
  } else if (size >= 1) {
    size = rep(size, times = length(table(df.tmp[class])))
  }  
  strat = strata(df.tmp, stratanames = names(df.tmp[class]), 
                 size = size, method = "srswor") #  sampling without replacement
  (dsample = getdata(df.tmp, strat))
}

## Initiate dataframe for all Random Forest runs
df.rf.output      <- data.frame()
df.rf.validation  <- data.frame()
df.rf.prediction  <- data.frame()
df.rf.importance  <- data.frame()


## Loop stratified-function 100 times
for (i in seq(1:rf.runs)) {
  cat("\n\nPERFORM RANDOM FOREST FOR STRATIFIED DATAFRAME ", i, "OF ", rf.runs,"\n")
  set.seed(i)
  
  ## Function call
  data.str <- stratified(data.cut, 1, 1)
  
  ## Reorder data frame
  data.str <- data.frame(cbind(data.str[, ncol(data.str)-3],
                               data.str[, 1:(ncol(data.str)-4)]))
  names(data.str) <- names(data.cut)
  
  
  ### Prepare Data for Random Forest ###########################################
  
  ## Get species list for Random Forest 
  lst.species <- names(data.str[(which(names(data.str) == "coordN")+1):(which(names(data.str) == "greyval_band_1")-1)])
  lst.species
  
  ## Create dataframe for Random Forest
  ## (Plot, observation date, MODIS image date, greyvalues & prevalence)
  df.randomForest <- cbind(data.str[, 1:3],
                           data.str[(which(names(data.str) == "greyval_band_1")):(which(names(data.str) == "greyval_band_36"))],
                           data.str[(which(names(data.str) == "coordN")+1):(which(names(data.str) == "greyval_band_1")-1)])
  
  ## Prepare tuning parameters for Random Forest function call
  mtrys <- data.frame(mtrys)
  names(mtrys) <- "mtry"
  
  
  ### Split dataset in training and test data ##################################
  
  set.seed(50)
  
  index <- sample(1:nrow(df.randomForest), nrow(df.randomForest)*train.part)
  
  df.rf.train <- df.randomForest[index, ]
  df.rf.test <- df.randomForest[-index, ]
  
  ## Subset training dataset
  df.rf.train.basics    <- data.frame(df.rf.train[, 1:3])
  df.rf.train.predict   <- df.rf.train[(which(names(df.rf.train) == "greyval_band_1")):(which(names(df.rf.train) == "greyval_band_36"))]
  df.rf.train.response  <- df.rf.train[(which(names(df.rf.train) == "greyval_band_36")+1):ncol(df.rf.train)]
  
  ## Subset test dataset
  df.rf.test.basics   <- data.frame(df.rf.test[, 1:3])
  df.rf.test.predict  <- df.rf.test[(which(names(df.rf.test) == "greyval_band_1")):(which(names(df.rf.test) == "greyval_band_36"))]
  df.rf.test.response <- df.rf.test[(which(names(df.rf.test) == "greyval_band_36")+1):ncol(df.rf.test)]
  
  
  ### Loop over all species (perform Random Forest) ############################
  
  ## Parallelization
  cl <- makeCluster(ncores)
  registerDoParallel(cl)
  # lst.species <- lst.species[1:3]
  df.rf.allspecies <- foreach(s = lst.species, .combine = "cbind", .packages = lib) %do% {
    
    ## Initialize dataframe
    tmp.df.validation <- data.frame()
    
    ## Get response variable as factor
    tmp.rf.train.response <- as.factor(df.rf.train.response[, names(df.rf.train.response) %in% c(s)])
    tmp.rf.train.response <- factor(tmp.rf.train.response, levels = c("yes", "no"))
    tmp.rf.test.response  <- as.factor(df.rf.test.response[, names(df.rf.test.response) %in% c(s)])
    tmp.rf.test.response  <- factor(tmp.rf.test.response, levels = c("yes", "no"))
    
    
    ### Random Forest function #################################################
    ### Classification for single species ######################################
    
    tmp.train.rf <- train(x = df.rf.train.predict,
                          y = tmp.rf.train.response,
                          method = "rf",
                          tuneGrid = mtrys,
                          metric = "Kappa", # "Kappa" or "Accuracy
                          ntree = trees)
    
    
      
      
      ### Predict & dataframe for ROC curve ####################################
      tmp.test.predict <- predict.train(tmp.train.rf, 
                                        newdata = df.rf.test.predict)
      
      ## Write raw prediction classes into a dataframe 
      tmp.predict.classes <- data.frame(tmp.test.predict)
      tmp.predict.classes <- data.frame(ifelse(tmp.predict.classes == "yes",
                                               1,
                                               0))
      
      ## Set columnnames and rownames for prediction classes dataframe
      names(tmp.predict.classes) <- s
      for (r in 1:nrow(tmp.predict.classes)) {
        rownames(tmp.predict.classes)[r] <- paste0("predict.class.plot.", 
                                                   rownames(df.rf.test.predict)[r])
      }
      
      ## Predict - get class probabilities
      tmp.test.predict.prob <- predict.train(tmp.train.rf, 
                                             newdata = df.rf.test.predict,
                                             type = "prob")
      
      tmp.predict.prob <- data.frame(tmp.test.predict.prob$yes)
      
      ## Set columnnames and rownames for prediction probabilities dataframe
      names(tmp.predict.prob) <- s
      for (p in 1:nrow(tmp.predict.prob)) {
        rownames(tmp.predict.prob)[p] <- paste0("predict.prob.plot.", 
                                                rownames(df.rf.test.predict)[p])
      }
      
      ## Write response variables of test dataset into a dataframe (ROC curve)
      tmp.observed.classes <- data.frame(tmp.rf.test.response)
      tmp.observed.classes <- data.frame(ifelse(tmp.observed.classes == "yes",
                                                1,
                                                0))

      ## Set columnnames and rownames for response classes dataframe
      names(tmp.observed.classes) <- s
      for (r in 1:nrow(tmp.observed.classes)) {
        rownames(tmp.observed.classes)[r] <- paste0("observed.class.plot.", 
                                                    rownames(df.rf.test.predict)[r])
      }
      
      ## Combine observed classes, prediction classes and 
      ## prediction probabilities in a single dataframe
      tmp.df.predict <- rbind(tmp.observed.classes,
                              tmp.predict.classes, 
                              tmp.predict.prob)
      
      
      ### Calculate Confusion Matrix from test data ############################
      tmp.confMatrix <- confusionMatrix(data = tmp.test.predict,
                                        reference = tmp.rf.test.response,
                                        dnn = c("Predicted", "Observed"),
                                        positive = "yes")
      
    
    ## Extract Confusion Matrix values
    tmp.Oyes_Pyes <- tmp.confMatrix$table[1]
    tmp.Oyes_Pno  <- tmp.confMatrix$table[2]
    tmp.Ono_Pyes  <- tmp.confMatrix$table[3]
    tmp.Ono_Pno   <- tmp.confMatrix$table[4]
    tmp.sum_Pyes  <- sum(tmp.Ono_Pyes, tmp.Oyes_Pyes)
    tmp.sum_Pno   <- sum(tmp.Ono_Pno, tmp.Oyes_Pno)
    tmp.sum_Ono   <- sum(tmp.Ono_Pno, tmp.Ono_Pyes)
    tmp.sum_Oyes  <- sum(tmp.Oyes_Pno, tmp.Oyes_Pyes)
    tmp.sum_obs   <- sum(tmp.Ono_Pno,
                         tmp.Ono_Pyes,
                         tmp.Oyes_Pno,
                         tmp.Oyes_Pyes)

    tmp.class.error.no <- tmp.Oyes_Pno/sum(tmp.Ono_Pno, 
                                           tmp.Oyes_Pno)
    tmp.class.error.yes <- tmp.Ono_Pyes/sum(tmp.Oyes_Pyes, 
                                            tmp.Ono_Pyes)

    tmp.mtry            <- tmp.train.rf$bestTune[1,1]
    tmp.Accuracy        <- tmp.confMatrix$overall[1]
    tmp.Kappa           <- tmp.confMatrix$overall[2]
    tmp.AccuracyLower   <- tmp.confMatrix$overall[3]
    tmp.AccuracyUpper   <- tmp.confMatrix$overall[4]
    tmp.AccuracyNull    <- tmp.confMatrix$overall[5]
    tmp.AccuracyPValue  <- tmp.confMatrix$overall[6]
    tmp.McnemarPValue   <- tmp.confMatrix$overall[7]
    tmp.Sensitivity     <- tmp.confMatrix$byClass[1]
    tmp.Specificity     <- tmp.confMatrix$byClass[1]
    tmp.DetectionRate   <- tmp.confMatrix$byClass[6]
    # tmp.POD <- tmp.Oyes_Pyes/tmp.sum_Pyes
    # tmp.FAR <- tmp.Ono_Pyes/tmp.sum_Pyes
    # tmp.CSI <- tmp.Oyes_Pyes/sum(tmp.Oyes_Pyes, tmp.Oyes_Pno, tmp.Ono_Pyes)
    # tmp.POFD <- tmp.Ono_Pyes/tmp.sum_Ono
    
    ## Write extracted values into a dataframe
    tmp.df.validation <- data.frame(rbind(tmp.Ono_Pno,
                                          tmp.Ono_Pyes,
                                          tmp.Oyes_Pno,
                                          tmp.Oyes_Pyes,
                                          tmp.sum_Ono,
                                          tmp.sum_Oyes,
                                          tmp.sum_Pno,
                                          tmp.sum_Pyes,
                                          tmp.sum_obs,
                                          tmp.class.error.no,
                                          tmp.class.error.yes,
                                          tmp.mtry,
                                          tmp.Accuracy,
                                          tmp.Kappa,
                                          tmp.Sensitivity,
                                          tmp.Specificity,
                                          tmp.DetectionRate))
    
    ## Set colnames (species name)
    names(tmp.df.validation) <- s
    
    
    ### Variable Importance ####################################################
    
    ## Get Variable Importance from Random Forest function call
    tmp.varimp <- varImp(tmp.train.rf, scale = TRUE)
    tmp.df.varimp <- data.frame(tmp.varimp$importance)
    
    ## Set speciesname as column name
    names(tmp.df.varimp) <- s
    
    ## Modify rownames in variable importance dataframe
    for (v in seq(1:nrow(tmp.df.varimp))) {
      tmp.rowname <- strsplit(rownames(tmp.df.varimp)[v], "greyval_")
      tmp.rowname <- paste0("varImp_", tmp.rowname[[1]][2])
      rownames(tmp.df.varimp)[v] <- tmp.rowname
    }
    
    ## Create dataframe for variable importance ranking (1 = "most important")
    tmp.df.varimp_rank <- data.frame(rank(-tmp.df.varimp[,1], na.last = TRUE))
    
    ## Create rownames for ranking dataframe
    rownames(tmp.df.varimp_rank) <- rownames(tmp.df.varimp)
    for (v in seq(1:nrow(tmp.df.varimp_rank))) {
      tmp.rowname <- strsplit(rownames(tmp.df.varimp_rank)[v], "_")
      tmp.rowname <- paste(tmp.rowname[[1]][1],
                           "rank",
                           tmp.rowname[[1]][2],
                           tmp.rowname[[1]][3],
                           sep = "_")
      rownames(tmp.df.varimp_rank)[v] <- tmp.rowname
    }
    
    ## Set speciesname as column name
    names(tmp.df.varimp_rank) <- s
    
    ## Combine variable importance and variable importance rank 
    ## in a single dataframe
    tmp.df.importance <- rbind(tmp.df.varimp, tmp.df.varimp_rank)
    
    
    
    
    tmp.df.validation.combined <- rbind(tmp.df.validation,
                                        tmp.df.predict, 
                                        tmp.df.importance)
    
    
    return(tmp.df.validation.combined)
  }
  
  
  stopCluster(cl)
  
  df.rf.allspecies$RandomForest_run <- i
  
  ## Write rownames to single column
  df.rf.allspecies$parameters <- rownames(df.rf.allspecies)
  
  
  ## Write dataframe for all species
  df.rf.allspecies2 <- cbind(df.rf.allspecies[ncol(df.rf.allspecies)-1],
                             df.rf.allspecies[ncol(df.rf.allspecies)],
                             df.rf.allspecies[1:(ncol(df.rf.allspecies)-2)]) 
  
  
  ## Append Random Forest output in a single dataframe
  df.rf.output <- rbind(df.rf.output, df.rf.allspecies2)
  
  ## Append validation parameters for each RandomForest run in a dataframe
  df.rf.validation <- rbind(df.rf.validation,
                            df.rf.allspecies2[which(df.rf.allspecies2$parameters %in% 
                                                      as.list(rownames(tmp.df.validation))), ])
  
  ## Append prediction parameters for each RandomForest run in a dataframe
  df.rf.prediction <- rbind(df.rf.prediction,
                            df.rf.allspecies2[which(df.rf.allspecies2$parameters %in% 
                                                      as.list(rownames(tmp.df.predict))), ])
  
  ## Append variable importance for each RandomForest run in a dataframe
  df.rf.importance <- rbind(df.rf.importance,
                            df.rf.allspecies2[which(df.rf.allspecies2$parameters %in% 
                                                      as.list(rownames(tmp.df.importance))), ])
  
}


### Write Random Forest output dataframes ######################################
cat("\n\nWRITE RANDOM FOREST OUTCOME DATAFRAME (ALL RF RUNS)\n")
write.table(df.rf.output,
            file = file.out.rf.output,
            quote = FALSE,
            col.names = TRUE,
            row.names = FALSE,
            sep = ";",
            dec = ",")

write.table(df.rf.validation,
            file = file.out.validation,
            quote = FALSE,
            col.names = TRUE,
            row.names = FALSE,
            sep = ";",
            dec = ",")

write.table(df.rf.prediction,
            file = file.out.prediction,
            quote = FALSE,
            col.names = TRUE,
            row.names = FALSE,
            sep = ";",
            dec = ",")

write.table(df.rf.importance,
            file = file.out.importance,
            quote = FALSE,
            col.names = TRUE,
            row.names = FALSE,
            sep = ";",
            dec = ",")


### Create final model validation statistics dataframe #########################

## Initialize dataframe for model validataion (Get species names)
df.rf.validation_final <- data.frame(names(df.rf.output[3:ncol(df.rf.output)]))
names(df.rf.validation_final) <- "species"


## Get confusion matrix sums

## Select parameters for final model validation (get sums from confusion matrix)
tmp.names <- as.character(df.rf.output[1:9, 2])

for(i in tmp.names) {
  ## Subset rows containing specific parameter (i)
  tmp.parameter <- df.rf.output[which(df.rf.output$parameters == i), ]
  
  ## Sum parameter for each species
  tmp.sums <- data.frame(colSums(tmp.parameter[, 3:ncol(tmp.parameter)]))
  
  ## Rename parameter
  i <- strsplit(i, "tmp.")
  names(tmp.sums) <- i[[1]][2]
  
  ## Bind calculated sums
  df.rf.validation_final <- cbind(df.rf.validation_final, tmp.sums)
  
}

### Calculate model validation statistics ######################################

## Classification Error "no"
df.rf.validation_final$class.error.no <- foreach(i = seq(1:nrow(df.rf.validation_final)), 
                                                 .combine = "rbind") %do% {
                                                   err.n.tmp <- (df.rf.validation_final[i, "Oyes_Pno"]) /
                                                     sum((df.rf.validation_final[i, "Ono_Pno"]),
                                                         (df.rf.validation_final[i, "Oyes_Pno"]))
                                                   
                                                   return(err.n.tmp)
                                                 }

## Classification Error "yes"
df.rf.validation_final$class.error.yes <- foreach(i = seq(1:nrow(df.rf.validation_final)), 
                                                  .combine = "rbind") %do% {
                                                    err.y.tmp <- (df.rf.validation_final[i, "Ono_Pyes"]) /
                                                      sum((df.rf.validation_final[i, "Ono_Pyes"]),
                                                          (df.rf.validation_final[i, "Oyes_Pyes"]))
                                                    
                                                    return(err.y.tmp)
                                                  }

## Observed Accuracy
df.rf.validation_final$observedAccuracy <- foreach(i = seq(1:nrow(df.rf.validation_final)), 
                                                   .combine = "rbind") %do% {
                                                     acc.obs.tmp <- ((df.rf.validation_final[i, "Oyes_Pyes"]) +
                                                                       (df.rf.validation_final[i, "Ono_Pno"])) /
                                                       (df.rf.validation_final[i, "sum_obs"])
                                                     
                                                     return(acc.obs.tmp)
                                                   }

## Expected Accuracy
df.rf.validation_final$expectedAccuracy <- foreach(i = seq(1:nrow(df.rf.validation_final)), 
                                                   .combine = "rbind") %do% {
                                                     acc.ex.tmp <- ((df.rf.validation_final[i, "sum_Oyes"] * df.rf.validation_final[i, "sum_Pyes"] / df.rf.validation_final[i, "sum_obs"]) +
                                                                      (df.rf.validation_final[i, "sum_Ono"] * df.rf.validation_final[i, "sum_Pno"] / df.rf.validation_final[i, "sum_obs"])) / 
                                                       df.rf.validation_final[i, "sum_obs"]
                                                     return(acc.ex.tmp)
                                                   }

## Kappa
df.rf.validation_final$Kappa <- foreach(i = seq(1:nrow(df.rf.validation_final)), 
                                        .combine = "rbind") %do% {
                                          kappa.tmp <- (df.rf.validation_final[i, "observedAccuracy"] - df.rf.validation_final[i, "expectedAccuracy"]) /
                                            (1 - df.rf.validation_final[i, "expectedAccuracy"])
                                          return(kappa.tmp)
                                        }


## Variable Importance (Mode)
# lst.ranks <- as.vector(rownames(df.rf.allspecies2)[(nrow(df.rf.allspecies2)-29):nrow(df.rf.allspecies2)])
# lst.ranks
# lst.species <- as.vector(rownames(df.rf.validation_final))
# 
# for (r in lst.ranks) {
#   r = lst.ranks[1]
#   tmp.rank <- df.rf.output[which(df.rf.output$parameters == r), ]
#   for ( s in 3:ncol(tmp.rank)) {
#     s=3
#     tmp.rank.vector <- as.vector(tmp.rank[, s])
#     tmp.rank.mode <- mlv(tmp.rank.vector, method = "mfv")
#     tmp.rank.mode <- tmp.rank.mode$M
#     
#   }
#     
# }


## Write final Random Forest model validation statistics dataframe
cat("\n\nWRITE FINAL RANDOM FOREST MODEL STATISTICS DATAFRAME\n")
write.table(df.rf.validation_final,
            file = file.out.validation.final,
            quote = FALSE,
            col.names = TRUE,
            row.names = FALSE,
            sep = ";",
            dec = ",")

## Runtime calulation
endtime <- Sys.time()
time <- endtime - starttime
cat("\n\nRUNTIME ", time, "\n")
