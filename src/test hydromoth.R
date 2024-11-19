install.packages("devtools")
devtools::install_github("DenaJGibbon/gibbonR")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

list.files(getwd())

library(gibbonR)
library(tidyverse)


# You need to tell R where to store the zip files on your computer.
destination.file.path.zip <-
  "zip/"

# You also need to tell R where to save the unzipped files
destination.file.path <- "data"

# This function will download the data from github

utils::download.file("https://github.com/DenaJGibbon/BorneoExampleData/archive/master.zip",
                     destfile = destination.file.path.zip)

# This function will unzip the file
utils::unzip(zipfile = destination.file.path.zip,
             exdir = destination.file.path)

# Examine the contents
list.of.sound.files <- list.files(paste(destination.file.path,
                                        "BorneoExampleData-master", "data", sep =
                                          "/"),
                                  full.names = T)
list.of.sound.files <- list.of.sound.files[c(2,3)]


loadRData <- function(fileName) {
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

list.rda.files <- list()
for(x in 1:length(list.of.sound.files)){
  list.rda.files[[x]] <-  loadRData(list.of.sound.files[[x]])
}

View(list.rda.files)

# naam geven aan elke lijst

multi.class.list <- list.rda.files[[1]]
S11_20180219_060002_1800sto3600s <- list.rda.files[[2]]


# Now we create a directory with the training .wav files

TrainingDataDirectory <- "data/BorneoMultiClass"

for(a in 1:length(multi.class.list)){
  Temp.element <- multi.class.list[[a]]
  writeWave(Temp.element[[2]],
            paste(TrainingDataDirectory,Temp.element[[1]],sep='/')
            )
}

# PART 1 Training data with labeled wav. clips

# Read in clips and calculate MFCCs

TrainingWavFilesDir <- "data/BorneoMultiClass/"

trainingdata <- gibbonR::MFCCFunction(input.dir=TrainingWavFilesDir, min.freq = 400, max.freq = 1600,win.avg='standard')

trainingdata$class <- as.factor(trainingdata$class)

# Compare Random Forest and Support Vector Machine for Supervised Classification

ml.model.svm <- e1071::svm(trainingdata[, 2:ncol(trainingdata)], trainingdata$class, kernel = "radial", 
                           cross = 25,
                           probability = TRUE)

print(paste('SVM accuracy',ml.model.svm$tot.accuracy))
#> [1] "SVM accuracy 88"


ml.model.rf <- randomForest::randomForest(x=trainingdata[, 2:ncol(trainingdata)], y = trainingdata$class)

print(ml.model.rf)

# PART 2

# Part 2a. Feature extraction

TrainingDataFolderLocation <- TrainingWavFilesDir

TrainingDataMFCC <- gibbonR::MFCCFunction(input.dir=TrainingDataFolderLocation, min.freq = 400, max.freq = 1600,win.avg="standard")

TrainingDataMFCC$class <- as.factor(TrainingDataMFCC$class)

# Part 2b. Run DetectClassify

TestFileDirectory <- "data/BorneoMultiClass"


TestFileDirectory <- "G:/.shortcut-targets-by-id/0B7wsyr_cWNeXb0g0eEdiNDZaczQ/AMFIBIEËN REPTIELEN INBO/ALIENS/Xenopus/hydromoths/R/data/BorneoMultiClass/female.gibbon_1.wav"

OutputDirectory <-  "data/DetectAndClassifyOutput"

gibbonR(input=TestFileDirectory,
        feature.df=TrainingDataMFCC,
        model.type.list=c('SVM','RF'),
        tune = TRUE,
        short.wav.duration=300,
        target.signal = c("female.gibbon"),
        min.freq = 400, max.freq = 1600,
        noise.quantile.val=0.15,
        minimum.separation =3,
        n.windows = 9, num.cep = 12,
        spectrogram.window =160,
        pattern.split = ".wav",
        min.signal.dur = 3,
        max.sound.event.dur = 25,
        maximum.separation =1,
        probability.thresh.svm = 0.15,
        probability.thresh.rf = 0.15,
        wav.output = "TRUE",
        output.dir =OutputDirectory,
        swift.time=TRUE,time.start=5,time.stop=10,
        write.table.output=FALSE,verbose=TRUE,
        random.sample='NA')

# PLOT

gibbonID(input.dir="data/BorneoMultiClass/",
         output.dir="data/BorneoMultiClass/Thumbnails/",
         win.avg='standard',add.spectrograms=TRUE,min.freq=400,max.freq=1600,class='no.clustering')

