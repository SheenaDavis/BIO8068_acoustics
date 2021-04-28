install.packages("devtools")
devtools::install_github("https://github.com/DenaJGibbon/behaviouR")

library(behaviouR)
library(tuneR)
library(seewave)
library(ggplot2)
library(dplyr)

##Southeast Asian monkey recordings
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

# It is sometimes a case of trial-and error to get the limits and spectro.colors
# at a suitable scale to see the information displayed nicely
v <- ggspectro(GibbonWaveFile, flim=c(0,2.5)) + # y-axis limits in kHz
  geom_tile(aes(fill=amplitude)) +
  scale_fill_gradient2(name="Amplitude\n(dB)\n", limits=c(-60,0),
                       na.value="transparent",
                       low="green", mid="yellow", high="red", midpoint = -30)
v

#displaying multiple spectrograms 

# We can tell R to print the spectrograms 2x2 using the code below
par(mfrow = c(2, 2))

# This is the function to create the spectrograms
SpectrogramFunction(input.dir = "FocalRecordings", min.freq = 500, max.freq = 2500,
                    Colors = "Colors")
par(mfrow = c(1,1))

#Simplifying the audio data to allow multivariate analysis
#compress your original data from over 500,000 records per .WAV file
#to a manageable number of about 100-200 mel-frequency coefficients using the
#MFCCFunction command, which will process all 12 .WAV files in your 
#FocalRecordings sub-folder:

FeatureDataframe <- MFCCFunction(input.dir = "FocalRecordings")
dim(FeatureDataframe)
View(FeatureDataframe)

#The MFCCFunction function strips away the underscore and remaining part of the filename,
#and uses the earlier part to create the contents of the Class column. 
#This provides a convenient way for you to name your .WAV files so that you can group
#them according to different species or genders.

#using NES8010.R  as source script for multivariate analysis

library(vegan) # This should already be installed from NES8010
source("nes8010.R")

# Use [, -1] to keep all rows but omit first column
acoustics_pca <- ordi_pca(FeatureDataframe[, -1], scale=TRUE)
summary(acoustics_pca)

ordi_plot(acoustics_pca, display="sites")

#adding labels using ggplot 
acoustics_sco <- ordi_scores(acoustics_pca, display="sites")
acoustics_sco <- mutate(acoustics_sco, group_code = FeatureDataframe$Class)

ggplot(acoustics_sco, aes(x=PC1, y=PC2, colour=group_code)) +
  geom_point()

#improving the plot using the NES8010 practical
#adding convex hull
ggplot(acoustics_sco, aes(x=PC1, y=PC2, fill=group_code)) +
  geom_point() +
  geom_chull(alpha=0.5) + # What do you think alpha does??
  theme_classic()

#adding labels and legend
ggplot(acoustics_sco, aes(x=PC1, y=PC2, fill=group_code)) +
  geom_point() +
  geom_chull(alpha=0.5) + 
  scale_fill_discrete(name = "Monkey type",
                      labels = c("Female Gibbon", "Great Argus", "Male Solo")) +
  theme_classic()

##Songbird analysis
install.packages("warbleR")
library(warbleR)

#search for blackbird Turdus merula recordings in the UK. We restrict our 
#length of recordings to 5 to 25 seconds, and as blackbirds’ songs and alarm calls
#are quite distinct, will search for them separately
#download = FALSE - to prevent downloads 

blackbird_songs <- query_xc(qword = 'Turdus merula cnt:"united kingdom" type:song len:5-25', download = FALSE)
blackbird_alarm <- query_xc(qword = 'Turdus merula cnt:"united kingdom" type:alarm len:5-25', download = FALSE)

#mapping using leaflet.map
map_xc(blackbird_songs, leaflet.map = TRUE)

#download audio recordings and convert from mp3 to WAV
# Create subfolders in your RStudio Project for song calls and alarm calls
dir.create(file.path("blackbird_songs"))
dir.create(file.path("blackbird_alarm"))

# Download the .MP3 files into two separate sub-folders
query_xc(X = blackbird_songs, path="blackbird_songs")
query_xc(X = blackbird_alarm, path="blackbird_alarm")

#the following r code will
#Obtain a list of all the file names in the blackbird_songs subfolder
#Create an empty R object called new_files which will contain the new filenames
#Go through a loop (the for function) to repeat a set of commands for each file
#Take the name of each file, and uses str_split to sub-divide it into 3 pieces, based on where the dash "-" symbol is (look at the original filename and there are 2 dashes).
#Concatenate the bits of the filename together using str_c, keeping the first two components together to create Turdusmerula, adding in text saying -song_ which ends with an underscore, and adding in the last component, the xxxxxx.mp3 from the file.
#Add this name to the new_files object, which gradually gets longer as you loop through each filename.
#Rename all the files.

library(stringr) # part of tidyverse

#blackbird songs
old_files <- list.files("blackbird_songs", full.names=TRUE)
new_files <- NULL
for(file in 1:length(old_files)){
  curr_file <- str_split(old_files[file], "-")
  new_name <- str_c(c(curr_file[[1]][1:2], "-song_", curr_file[[1]][3]), collapse="")
  new_files <- c(new_files, new_name)
}
file.rename(old_files, new_files)

#alarm calls
old_files <- list.files("blackbird_alarm", full.names=TRUE)
new_files <- NULL
for(file in 1:length(old_files)){
  curr_file <- str_split(old_files[file], "-")
  new_name <- str_c(c(curr_file[[1]][1:2], "-alarm_", curr_file[[1]][3]), collapse="")
  new_files <- c(new_files, new_name)
}
file.rename(old_files, new_files)

#create sub-folder -blackbird audio
dir.create(file.path("blackbird_audio"))
file.copy(from=paste0("blackbird_songs/",list.files("blackbird_songs")),
          to="blackbird_audio")
file.copy(from=paste0("blackbird_alarm/",list.files("blackbird_alarm")),
          to="blackbird_audio")

#convert to .wav
mp32wav(path="blackbird_audio", dest.path="blackbird_audio")
unwanted_mp3 <- dir(path="blackbird_audio", pattern="*.mp3")
file.remove(paste0("blackbird_audio/", unwanted_mp3))

#Visualise and analyse the song and alarm calls
#Single blackbird, oscillogram and spectrogram

blackbird_wav <- readWave("blackbird_audio/Turdusmerula-song_243908.wav")
blackbird_wav

oscillo(blackbird_wav)
oscillo(blackbird_wav, from = 0.59, to = 0.60)
SpectrogramSingle(sound.file = "blackbird_audio/Turdusmerula-song_243908.wav",
                  Colors = "Colors")

#MFCC of blackbird song and alarm calls
blackbird_mfcc <- MFCCFunction(input.dir = "blackbird_audio",
                               max.freq=7000)
dim(blackbird_mfcc)

#PCA
blackbird_pca <- ordi_pca(blackbird_mfcc[, -1], scale=TRUE)
summary(blackbird_pca)

#displaying results 
blackbird_sco <- ordi_scores(blackbird_pca, display="sites")
blackbird_sco <- mutate(blackbird_sco, group_code = blackbird_mfcc$Class)

ggplot(blackbird_sco, aes(x=PC1, y=PC2, colour=group_code)) +
  geom_point() 


ggplot(blackbird_sco, aes(x=PC1, y=PC2, color=group_code)) +
  geom_point() +
  scale_color_discrete(name = "Call type",
                      labels = c("Blackbird alarm", "Blackbird song")) +
  theme_classic()
