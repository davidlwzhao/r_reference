library(tidyverse)
library(data.table)
rm(list = ls())

# set cwd
setwd(getwd())

# util function for directory based multi read and row-wise concatenation
mult_read <- function(folder, data_folder = "data/", pattern = "*.csv") {
  # add backslash for directory
  if (str_sub(folder, -1) != "/") {
    folder <- paste0(folder, "/")
  }
  
  # iterate through files in folder
  directory <- dir(path = paste0(data_folder, folder), pattern = pattern)
  files <- vector("list", length(directory))
  i <- 1L
  for (f in directory) {
    full_path <- paste0(data_folder, folder, f)
    files[[i]] <- fread(full_path)
    files[[i]]$SourceFile <- f
    i <- i + 1L
  }
  
  return(do.call(rbind, args = c(files, fill = TRUE)))
}

# read in dataset
DT <- mult_read("titanic")

# since dataset is from kaggle only train has survived metric
DT[, .N, .(SourceFile, Survived)]

# intial EDA (each is equivalent)
DT[, .(is.na(.SD))][, lapply(.SD, sum)]
DT[, lapply(data.table(is.na(.SD)), sum)]
DT[, lapply(.SD, function(col) sum(is.na(col)))]

# imputing age
DT[, c("LastName", "FirstNameTitle") := tstrsplit(Name, ", ", fixed=TRUE)]
DT[, c("Title") := tstrsplit(FirstNameTitle, ". ", fixed=TRUE)[[1]]]
AgeByTitle <- DT[, .(avg_age = mean(Age, na.rm = TRUE)),
                 by = .(Title)]
DT <- DT[AgeByTitle, on = .(Title)]
DT[is.na(Age), Age:=avg_age]

# general string manipulation
DT[, .(Name, str_length(Name), str_sub(Name, 1, 5))]



# visualizations
(ggplot(DT, aes(x = Survived, group = Sex)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  labs(y = "Percent", fill="Survived") +
  geom_text(aes( label = scales::percent(..prop..), 
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  facet_wrap(~Sex + Pclass) +
  scale_y_continuous(labels = scales::percent)
)

# need the group parameter to determine the denominator for proportions!
(ggplot(DT, aes(x = Survived)) +
    geom_bar(aes(y = ..prop..), stat = "count")
  
)





















