setwd("~/Desktop/USFSpring2018/Stochastic_Processes/project/")
load("models.RData")
library(audio)
library(magrittr)
library(dplyr)


# Helper function
# Converts frequency and duration values into a 
# wave form to be recorded.
makeSine <- function(freq, duration) {
  tempo <- 120
  sample_rate <- 44100
  
  wave <- sin(seq(0, duration / tempo * 60, 1 / sample_rate) *
                freq * 2 * pi)
  fade <- seq(0, 1, 50 / sample_rate)
  wave * c(fade, rep(1, length(wave) - 2 * length(fade)), rev(fade))
}

# Takes in a data frame created by the convertSeqToNotes()
# function and creates a long numeric vector
# to be converted into a wav file
createWave <- function(df){
  
  notes <- c(A = 0, Bb = 1, B = 2, C = 3, Db = 4,
             D = 5, Eb = 6, E = 7, F = 8, Gb = 9, G = 10,
             Ab = 11, R = -5)
  
  note <- notes[substr(df$pitch, 1, 1)]
  note <- note - grepl('b', df$pitch) + 48 * (note >=0) + 12 * (note < 3 & note > -1)
  
  freq <- 2 ^ ((note - 60) / 12) * 440
  
  wave_df <- data.frame(pitch = df$pitch,
                        duration = df$mval,
                        freq = freq)
  
  song_wave <-
    mapply(makeSine, wave_df$freq, wave_df$duration) %>%
    do.call("c", .)
  
  return(song_wave)
}

# Main function
# Creates a melodic sequence specified by the given
# argument parameters to be saved to a csv and
# wave file if requested. Model scheme 1, 2, and 3 refer
# to generating a sequence based on a full melody, separate 
# sections, or repeated + separate sections respectively.
# The logloss parameter determines the type of sequence you want
# to generate. Min and max produce the sequence with the indicated
# logloss. Random chooses any sequence (recommended). 
# Options to write a csv and record a wav file of the sequence are 
# given. 
#
# args:
#     scheme
#     n.iter
#     logloss
#     method
#     writeFile
#     write
#     recordFile
#     record
generateMelody <- function(scheme = 3, n.iter = 100, logloss = "random", method = "prob",
                           writeFile, write = TRUE, recordFile, record = TRUE){
  out.df <- data.frame(pitch = character(0),
                       mpitch = numeric(0),
                       value = character(0),
                       mval = numeric(0),
                       stringsAsFactors = FALSE)
  gen <- data.frame(pitch = character(0),
                    mpitch = numeric(0),
                    value = character(0),
                    mval = numeric(0),
                    stringsAsFactors = FALSE)
  
  if(scheme == 1){
    
    # Single model for full song
    pst.gen <- generate(pst.g, l = 96, n=n.iter, method=method)
    pst.logloss <- numeric(0)
    for(i in 1:nrow(pst.gen)){
      pst.logloss <- predict(pst.g, pst.gen[i,], output = "logloss")
    }
    if(realistic){
      gen <- convertSeqToNotes(pst.gen[which.min(pst.logloss),])
    }else{
      gen <- convertSeqToNotes(pst.gen[which.max(pst.logloss),])
    }
  }else if(scheme == 2){
    
    # 1. Create model for each section group
    # 2. Generate 50 sequences for each model
    # 3. Grab the sequence with the lowest logloss
    # 4. Append to outputted dataframe
    # section sequence : Ia, Ib, IV, Ic, V, Id
    for(i in 1:6){
      sec.pst <- subtree(sections.pst.g, group = i)
      sec.gen <- generate(sec.pst, l=16, n = n.iter, method="prob")
      sec.logloss <- numeric(0)
      for(j in 1:nrow(sec.gen)){
        sec.logloss[j] <- predict(sec.pst, sec.gen[j,], output="logloss")
      }
      gen <- switch(logloss,
                    min = rbind(gen, convertSeqToNotes(sec.gen[which.min(sec.logloss),])),
                    max = rbind(gen, convertSeqToNotes(sec.gen[which.max(sec.logloss),])),
                    random = rbind(gen, convertSeqToNotes(
                      sec.gen[sample(1:length(sec.logloss), 1),])))
    }
  }else{
    
    # section sequence : Ia, Ia, IV, Ia, V, Ia
    out.Ia <- data.frame(pitch = character(0),
                         mpitch = numeric(0), 
                         value = character(0),
                         mval = numeric(0),
                         stringsAsFactors = FALSE)
    for(i in 1:6){
      if(i %in% c(2,4,6)){
        gen <- rbind(gen, out.Ia)
        next
      }else{
        sec.pst <- subtree(sections.pst.g, group = i)
        sec.gen <- generate(sec.pst, l=16, n = n.iter, method="prob")
        sec.logloss <- numeric(0)
        
        for(j in 1:nrow(sec.gen)){
          sec.logloss[j] <- predict(sec.pst, sec.gen[j,], output="logloss")
        }
        gen <- switch(logloss,
                      min = rbind(gen, convertSeqToNotes(sec.gen[which.min(sec.logloss),])),
                      max = rbind(gen, convertSeqToNotes(sec.gen[which.max(sec.logloss),])),
                      random = rbind(gen, convertSeqToNotes(
                        sec.gen[sample(1:length(sec.logloss), 1),])))
        
        if(i == 1){
          out.Ia <- gen
        }
      }
    }
  }
  out.wave <- createWave(gen)
  
  csv_path <- "generated_melodies/csv/"
  wav_path <- "generated_melodies/wav/"
  
  if(write)
    write.csv(gen, paste(csv_path, writeFile, sep = ""), row.names = FALSE)
  if(record)
    save.wave(out.wave, paste(wav_path, recordFile, sep = ""))
}
