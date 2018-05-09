library(dplyr)
library(magrittr)
songs <- list.files("songs")
for(i in seq_along(songs)){
  df <- read.csv(paste("songs/", songs[i], sep=""), stringsAsFactors = FALSE)
  new_df <- recodeSongs(df)
  print(songs[i])
  print(dim(new_df))
  write.csv(new_df, paste("recoded_songs/", songs[i], sep=""), row.names = FALSE)
}

# Takes a given song in the form of a dataframe
# and breaks the notes into 1/16 time-value states.
# This results in all twelve-bar blues songs having
# 16*12 = 192 observations as opposed to one observation
# for each note. The new note values will indicate the
# pitch as well as an _S or _L symbol indicating whether 
# the current note stays or leaves. A Bb eighth-note will 
# have observations c(Bb_S, Bb_L) having duration 1/16 + 1/16 = 1/8
# whereas a Bb dotted-quarter will have c(Bb_S, Bb_S, Bb_S, Bb_S, Bb_S, Bb_L)
# with a total duration of 1/16 * 6 = 5/8. 
#
# args:
#     df - Given dataframe created by writeSong() function
#
# returns:
#     new dataframe 192x2 wi
recodeSongs <- function(df){
  new_vals <- character(0)
  triplet_num <- 1
  for(i in 1:nrow(df)){
    value <- df$value[i]
    pitch <- df$pitch[i]
    
    
    if(value == 'e'){
      new_vals <- c(new_vals, paste(pitch,"L", sep="_"))
    }else if(value == "q"){
      new_vals <- c(new_vals, paste(pitch,"S", sep="_"),paste(pitch,"L",sep="_"))
    }else if(value == "dq"){
      new_vals <- c(new_vals, rep(paste(pitch,"S", sep="_"), 2),
                    paste(pitch,"L", sep="_"))
    }else if(value == "h"){
      new_vals <- c(new_vals, rep(paste(pitch,"S", sep="_"), 3),
                    paste(pitch,"L",sep="_"))
    }else if(value == "he"){
      new_vals <- c(new_vals, rep(paste(pitch,"S", sep="_"), 4),
                    paste(pitch,"L", sep="_"))
    }else if(value == "dh"){
      new_vals <- c(new_vals, rep(paste(pitch,"S", sep="_"), 5),
                    paste(pitch,"L", sep="_"))
    }else if(value == "ddh"){
      new_vals <- c(new_vals, rep(paste(pitch,"S", sep="_"), 6),
                    paste(pitch,"L", sep="_"))
    }else if(value == "w"){
      new_vals <- c(new_vals, rep(paste(pitch,"S", sep="_"), 7),
                    paste(pitch,"L", sep="_"))
    }else if(value == "t"){

      if(triplet_num == 1){
        pitch2 <- df$pitch[i + 1]
        new_vals <- c(new_vals, 
                      paste(pitch,"S", sep="_"),paste(pitch2, "L", sep="_"))
        triplet_num <- triplet_num + 1
        
      }else if(triplet_num == 2){
        triplet_num <- triplet_num + 1
      }else{
        triplet_num <- 1
      }
    }else{
      print("WHAT THE HECK")
      break
    }
 
  }
  new_secs <- c(rep("Ia", 16), rep("Ib", 16), rep("IV", 16), rep("Ic", 16), rep("V", 16), rep("Id",16))
  return(data.frame(state = new_vals,
                    section = new_secs,
                    stringsAsFactors = FALSE))
}

# Converts a granular version of notes broken down
# into 1/8 segments to regular pitches and values.
# 
# args:
#     genSeq - single vector output from generate() containing 1/8 segment breakdown
#
# returns:
#     dataframe(pitch, value)
convertSeqToNotes <- function(genSeq){
  
  new_seq <- unlist(genSeq) %>% as.character()

  # Replacing last character with terminating pitch
  term_pitch <- sub(pattern = "_S", replacement = "_L", x = new_seq[length(new_seq)])
  new_seq[length(new_seq)] <- term_pitch
  
  
  new_pitch <- character(0)
  new_val <- character(0)
  midi_val <- numeric(0)
  #Duration accumulator
  dur <- 0

  for(i in 1:length(new_seq)){
      
    symbol <- new_seq[i]
    if(regexpr(pattern = "L", text = symbol) == -1){ # If it's a held note
      dur <- dur + 1
    }else{# If it's an end note
      dur <- dur + 1
      new_pitch <- c(new_pitch, sub(pattern = "_L", replacement ="", x = symbol))
      new_val <- c(new_val, switch(dur,
                                   `1` = 'e',
                                   `2` = 'q',
                                   `3` = 'dq',
                                   `4` = 'h',
                                   `5` = 'he',
                                   `6` = 'dh',
                                   `7` = 'ddh',
                                   `8` = 'w'))
      midi_val <- c(midi_val, dur)
      dur <- 0
    }
  }
  
  midi_pitch <- recode(new_pitch,  C=60, Db=61, D=62, Eb=63, E=64, F=65,
         Gb=66, G=67, Ab=68, A=69, A=70, Bb=71, B=72, R=0) %>% as.numeric()

  return(data.frame(pitch = new_pitch,
                    mpitch = midi_pitch,
                    value = new_val,
                    mval = midi_val,
                    stringsAsFactors = FALSE))
}


