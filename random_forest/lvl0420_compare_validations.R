cat("\014")
################################################################################
##  
##  Compares validation parameter from multiple RandomForest outcomes with 
##  different setup parameters
##    eg.: Different training data set sizes
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
lib <- c("reshape", "ggplot2", "reshape2")

lapply(lib, function(...) library(..., character.only = TRUE))

## Set working directory
# setwd("/home/sschlauss/")
setwd("D:/")


### Set filepaths ##############################################################

path.csv <- "Code/bifore/src/csv/"
path.fig <- "Code/bifore/src/figures/"

file.out.comparison <- paste0(path.csv, "lvl0420_compare_validations.csv")

image.out.hist.mean.kappa <- paste0(path.fig, 
                                    "lvl0420_histogram-mean-Kappa.png")

image.out.hist.low.variance <- paste0(path.fig, 
                                      "lvl0420_histogram-lowest-variance-Kappa.png")

image.out.line.mean.kappa <- paste0(path.fig, 
                                    "lvl0420_lineplot-mean-Kappa.png")

image.out.line.variance.kappa <- paste0(path.fig, 
                                        "lvl0420_lineplot-variance-Kappa.png")

if (!file.exists(path.fig)) {dir.create(file.path(path.fig))}


### Import data ################################################################

## List files
files <- list.files(pattern = "kappa.csv",
                    recursive = TRUE,
                    full.names = FALSE,
                    include.dirs = FALSE)

files  # data_<year>-<month>-<day>_<size of training data set(%)>test-<cross validation>

## Initialize new data frame
df.Parameter <- data.frame()

## Import and rename each data frame
for(i in 1:length(files)) {
  ## Import csv
  data.raw <- read.csv2(files[i],
                        dec = ",",
                        header = TRUE,
                        stringsAsFactors = FALSE)
  
  ## Set/fetch temporary name and rowname
  tmp.name <- paste0("df", i)
  tmp.rowname <- strsplit(files[i], split = "_")
  
  ## Calculate variance for each species
  tmp.variance <- data.frame()
  for(v in 3:ncol(data.raw)) {
    tmp.variance <- rbind(tmp.variance, diff(range(data.raw[,v])))
    tmp.variance <- round(tmp.variance, digits = 4)
  }
  tmp.variance <- data.frame(t(tmp.variance))
  names(tmp.variance) <- names(data.raw[3:ncol(data.raw)])
  rownames(tmp.variance)[1] <- paste(tmp.rowname[[1]][2], 
                                     tmp.rowname[[1]][3], 
                                     "variance", sep = "_")
  
  
  ## Calculate mean parameter for each species 
  tmp.mean <- colMeans(data.raw[3:ncol(data.raw)])
  tmp.mean <- round(tmp.mean, digits = 4)
  tmp.mean <- data.frame(t(tmp.mean))
  names(tmp.mean) <- names(data.raw[3:ncol(data.raw)])
  rownames(tmp.mean)[1] <- paste(tmp.rowname[[1]][2], 
                                 tmp.rowname[[1]][3], 
                                 "mean", sep = "_")
  
  
  ## Combine mean parameter and variance in a single data frame
  df.Parameter <- rbind(df.Parameter, tmp.mean, tmp.variance)
  
  
  ## Assign new data frame name
  assign(tmp.name, data.frame(data.raw))
  
  
  ## Remove old data frame
  remove(data.raw, tmp.mean, tmp.variance)
  
}

### Prepare comparison table ###################################################

## Subset comparison table
df.mean <- df.Parameter[grep("_mean", rownames(df.Parameter)), ]
df.variance <- df.Parameter[grep("_variance", rownames(df.Parameter)), ]

## Get best RandomForest validation name in a single row  (maximal mean)
df.mean.max <- data.frame(t(apply(df.mean[1:ncol(df.mean)], 
                                  2, 
                                  which.max)))
rownames(df.mean.max)[1] <- "highest_mean"

for(y in 1:ncol(df.mean.max)) {
  df.mean.max[, y] <- rownames(df.mean)[df.mean.max[, y]]
}

## Get best RandomForest validation name in a single row  (minimum variance)
df.variance.min <- data.frame(t(apply(df.variance[1:ncol(df.variance)], 
                                      2, 
                                      which.min)))
rownames(df.variance.min)[1] <- "lowest_variance"

for(k in 1:ncol(df.variance.min)) {
  df.variance.min[, k] <- rownames(df.variance)[df.variance.min[, k]]
}


### Combine mean and variance tables ###########################################

df.comparison <- rbind(df.mean, df.variance)

summary(df.comparison)

df.comparison2 <- rbind.data.frame(df.comparison, 
                                   df.mean.max, 
                                   df.variance.min)

df.comparison2$validation <- rownames(df.comparison2)
df.comparison2 <- data.frame(df.comparison2[ncol(df.comparison2)], 
                             df.comparison2[1:(ncol(df.comparison2)-1)])


### Write csv ##################################################################

write.table(df.comparison2,
            file = file.out.comparison,
            quote = FALSE,
            col.names = TRUE,
            row.names = FALSE,
            sep = ";",
            dec = ",")


### Create histograms ##########################################################

## Prepare data for histogram
df.mean.max.2 <- data.frame(t(df.mean.max))
df.variance.min.2 <- data.frame(t(df.variance.min))


### Histogram for highest mean kappa ###########################################
plot.hist.mean.max <- ggplot(df.mean.max.2, 
                             aes(x = highest_mean)) +
  geom_histogram(binwidth = .5) +
  xlab("RandomForest run") +
  ggtitle("lvl420 compare validations - Highest mean Kappa") +
  theme(plot.title = element_text(lineheight=.8, 
                                  size = 20),
        axis.text.x = element_text(angle = 90, 
                                   hjust = 1, 
                                   vjust = .5))


png(image.out.hist.mean.kappa, 
    width = 1024 * 6, 
    height = 748 * 6, 
    units = "px", 
    res = 600)

plot.hist.mean.max

graphics.off()


### Histogram for lowest variance in kappa #####################################
plot.hist.variance.min <- ggplot(df.variance.min.2, 
                                 aes(x = lowest_variance)) +
  geom_histogram(binwidth = .5) +
  xlab("RandomForest run") +
  ggtitle("lvl420 compare validations - Lowest variance in Kappa") +
  theme(plot.title = element_text(lineheight=.8, 
                                  size = 20),
        axis.text.x = element_text(angle = 90, 
                                   hjust = 1, 
                                   vjust = .5))


png(image.out.hist.low.variance, 
    width = 1024 * 6, 
    height = 748 * 6, 
    units = "px", 
    res = 600)

plot.hist.variance.min

graphics.off()

### Lineplot for mean Kappa ####################################################

df.mean3 <- data.frame(t(df.mean))
names(df.mean3) <- rownames(df.mean)
df.mean3$species <- rownames(df.mean3)

## Melt dataframe
df.mean.melt <- melt(df.mean3, id = "species")


plot.line.mean <- ggplot(data = df.mean.melt,
                         aes(x = species, 
                             y = value, 
                             colour = variable, 
                             group = variable)) +
  geom_line() +
  xlab("species") +
  ylab("Mean Kappa") +
  ggtitle("lvl0420 compare validations - Mean Kappa") +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = .5, 
                                   vjust = 0, 
                                   size = 11),
        plot.title = element_text(lineheight = .8, 
                                  size = 20),
        legend.title=element_blank())


png(image.out.line.mean.kappa, 
    width = 1024 * 6, 
    height = 748 * 6, 
    units = "px", 
    res = 600)

plot.line.mean

graphics.off()


### Lineplot for variance in Kappa #############################################

df.variance3 <- data.frame(t(df.variance))
names(df.variance3) <- rownames(df.variance)
df.variance3$species <- rownames(df.variance3)

## Melt dataframe
df.variance.melt <- melt(df.variance3, id = "species")


plot.line.variance <- ggplot(data = df.variance.melt,
                             aes(x = species, 
                                 y = value, 
                                 colour = variable, 
                                 group = variable)) +
  geom_line() +
  xlab("species") +
  ylab("variance") +
  ggtitle("lvl0420 compare validations - Variance in Kappa") +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = .5, 
                                   vjust = 0, 
                                   size = 11),
        plot.title = element_text(lineheight = .8, 
                                  size = 20),
        legend.title = element_blank())


png(image.out.line.variance.kappa, 
    width = 1024 * 6, 
    height = 748 * 6, 
    units = "px", 
    res = 600)

plot.line.variance

graphics.off()
