install.packages("devtools")
devtools::install_github("https://github.com/DenaJGibbon/behaviouR")

library(behaviouR)
library(tuneR)
library(seewave)
library(ggplot2)
library(dplyr)

#Southeast Asian monkey recordings
# Now we will create folder in your RStudio Project called 'FocalRecordings'
dir.create(file.path("FocalRecordings"), showWarnings = FALSE)

# Now we will load the sound files that were in the behaviouR package
githubURL <- "https://github.com/DenaJGibbon/behaviouRdata/raw/master/data/FocalRecordings.rda"
FocalRecordings <- get(load(url(githubURL)))

# Now we will save the recordings to the new folder you created as standard
# audio .wav format files; You do not need to understand the next set of code
# in detail
for (a in 1:length(FocalRecordings)) {
  FileName <- FocalRecordings[[a]][1][[1]]
  WaveFile <- FocalRecordings[[a]][2][[1]]
  writeWave(WaveFile, paste("FocalRecordings/", FileName, sep = ""))
}

#importing and displaying an individual .wav file
GibbonWaveFile <- readWave("FocalRecordings/FemaleGibbon_1.wav")
GibbonWaveFile
#check duration 
duration(GibbonWaveFile) * GibbonWaveFile@samp.rate

#plot the amplitude using the oscillo() function
oscillo(GibbonWaveFile)
#zoom into specific areas of recording - to allow for better visualisation 
oscillo(GibbonWaveFile, from = 0.1, to = 0.2)
oscillo(GibbonWaveFile, from = 0.15, to = 0.2)

#creating a spectrogram 
SpectrogramSingle(sound.file = "FocalRecordings/FemaleGibbon_1.wav")
#zoom into lower frequencies 
SpectrogramSingle(sound.file = "FocalRecordings/FemaleGibbon_1.wav", min.freq = 500, 
                  max.freq = 2500)

#display in colour 
SpectrogramSingle(sound.file = "FocalRecordings/FemaleGibbon_1.wav", min.freq = 500, 
                  max.freq = 2500, Colors = "Colors")

#ggplot spectro 