
# Change to the path where you saved this repo
setwd("~/Desktop/DataScienceProjects/RProjects/MarkovBlues/")
load("models.RData")


# ~~~ Helper function ~~~
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

# ~~~ Helper function ~~~
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

# ~~~~~~~~~~ MAIN FUNCTION ~~~~~~~~~~
# Creates a melodic sequence simulating a 12-bar blues 
# in csv and wav formats. 
#
#
# Model Schemes:
#   1 - PST built on the full melodies from our corpus of songs. 
#       The output is a sequence that changes over time, without any
#       underyling chord sections.
#   2 - Six PSTs built on each of six sections of the 12-bar blues. No repetition
#       occurs but each section assumes an underlying chord.
#   3 - (default) Three PSTs built on each of six sections of the 12-bar blues. Section I 
#       is repeated four times to produce I-I-IV-I-V-I. This simulates the 
#       most realistic output.
# 
# args:
#     scheme - Model scheme (described above)
#     n.iter - (default: 100) Number of random sequences first generated.
#               Depending on the logloss parameter, one sequence is chosen.
#     logloss - (default: random) Choice of how to choose a single sequence
#               from the many sequences created by the n.iter argument.
#               'min' and 'max' take sequences with loglosses respectively.
#               Logloss is analogous to the likehood of a sequence occuring,
#               however a low value is a higher likelihood and vice versa.
#     method - (default: prob) Controls how individual sequences are generated. 
#               As a PST is traversed, you can either choose the next symbol
#               with the highest probability ('max') or keep it random based
#               on their respective probabilities ('prob').
#     writeFile - CSV file string to write out to. Must end with '.csv'
#     write - logical parameter indicating whether or not to write to a csv
#     recordFile - WAV file string to write out to. Must end with '.wav'
#     record - logical parameter indicating whether or not to write to a wav
#
# returns:
#     $df - The dataframe with the generated sequence. This would be written to your
#           indicated csv file.
#     $melody - A melody in the form of a large numeric vector. This would be written
#           to your indicated wav file. You can call play(...$melody) to play
#           the object straight through R.
generateMelody <- function(scheme = 3, n.iter = 100, logloss = "random", method = "prob",
                           writeFile, write = TRUE, recordFile, record = TRUE){
  
  library(audio)
  library(magrittr)
  library(dplyr)
  
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
    # 2. Generate n.iter sequences for each model
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
  
  # Large numeric vector containing melody
  out.wave <- createWave(gen)
  
  csv_path <- "generated_melodies/csv/"
  wav_path <- "generated_melodies/wav/"
  
  if(write)
    write.csv(gen, paste(csv_path, writeFile, sep = ""), row.names = FALSE)
  if(record)
    save.wave(out.wave, paste(wav_path, recordFile, sep = ""))
  
  return(list(df = gen,
              melody = out.wave))
}

# Example calling function
out <- generateMelody(write = FALSE, record = FALSE, logloss = 'min')

# Playing melody in R
play(out$melody)
