library(magrittr)
# Transposes a vector of pitches to the key of
# C. Accidentals will be denoted with flats 
# instead of sharp.
#
# args:
#     pitches - Vector of melodic pitches
#     key - Original key of given melodic pitches
#
# returns:
#     transposed vector with flatted accidentals
#
transposeC <- function(pitches, key){
  all_pitches <- c("C", "C#", "Db", "D", "D#",
                   "Eb", "E", "F", "F#", "Gb",
                   "G", "G#", "Ab", "A", "A#",
                   "Bb", "B")
  flat_pitches <- c("C", "Db", "D", "Eb", "E", "F",
                    "Gb", "G", "Ab", "A", "Bb", "B")
  
  # If the pitch is notated as sharp, return the flatted version
  get_flat <- function(pitch){
    if(substr(pitch, 2, 2) == "#"){
        return(all_pitches[which(all_pitches == pitch) + 1])
    }else{
      return(pitch)
    }
  }
  
  # Flatted version of pitches
  flat <- sapply(X = pitches, FUN = get_flat) %>% unname()
  
  # Flatted key (if initially indicated as sharp)
  flat_key <- get_flat(key)
  
  key_idx <- which(flat_pitches == flat_key)
  
  # Calculating the number of half steps to move to C
  note_diff <- key_idx - 1
  
  # Computing the transposed pitch vector
  transposed <- sapply(X = flat, FUN = function(p){
    if(p == "R"){
      return(p)
    }else{
      
      p_idx <- which(flat_pitches == p)
      if(p_idx <= note_diff){
        new_p <- flat_pitches[(12 + p_idx) - note_diff]
        return(new_p)
        
      }else{
        new_p <- flat_pitches[p_idx - note_diff]
        return(new_p)
      }
        
    }
  }) %>% unlist() %>% unname()
  
  return(transposed)
}

# Creates a column of sections given a vector of 
# note duration values. The total sum of durations
# must be 12 for a 12-bar-blues progression. An error
# is thrown if this sum is violated. 
#
# args:
#     durations - vector of note values for each note in a melody
#     structure - the type of chord structure to follow 
#                 (default: 12-bar-blues which groups bars in sets of 2s)
#                   
getSections <- function(durations, structure = c("Ia", "Ib", "IV", "Ic", "V", "Id")){
  if(sum(durations) != 12){
    
    return(cat("ERROR \n Duration sum: ", sum(durations), "\n",
               "Duration sum must be 12"))
  }
  sections <- character(0)
  cur_sec_idx <- 1
  cur_sec <- structure[cur_sec_idx]
  cur_sum <- 0
  
  # Threshold to classify next section
  sec_size <- 12/length(structure)
  
  for(i in 1:length(durations)){
    cur_sum <- cur_sum + durations[i]
    sections <- c(sections, cur_sec)
    
    # Can't calculate exact 0, so we check if difference
    # is smaller than a sixteenth note (which is the smallest note dur used)
    if(abs(cur_sum - sec_size) < 0.0625){
      cur_sec_idx <- cur_sec_idx + 1
      cur_sec <- structure[cur_sec_idx]
      cur_sum <- 0
    }else{
      next
    }
  }
  return(sections)
}
