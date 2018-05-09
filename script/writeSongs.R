setwd("~/Desktop/USFSpring2018/Stochastic_Processes/project/")
source("script/encodingFunctions.R")
writeSong <- function(title, key, n_pitches, n_durr){
  transpose <- transposeC(pitches = n_pitches, key = key)
  sections <- getSections(n_durr)
  
  value <- rep(NA, length(n_durr))
  value[n_durr == 1/16] <- "s"
  value[n_durr == 1/12] <- 't'
  value[n_durr == 1/8] <- 'e'
  value[n_durr == 1/4] <- 'q'
  value[n_durr == 3/8] <- 'dq'
  value[n_durr == 2/4] <- 'h'
  value[n_durr == 5/8] <- 'he'
  value[n_durr == 3/4] <- 'dh'
  value[n_durr == 7/8] <- 'ddh'
  value[n_durr == 1] <- 'w'
  
  song <- data.frame(pitch = transpose,
                     duration = n_durr,
                     value = value,
                     section = sections)

  write.csv(song, paste("songs/", title, ".csv", sep=""),
            row.names = FALSE)
}

# ~~~
# May require different structure
title <- "Creole_Love_Song"
key <- "C"
n_pitch <- c('G',
             'R', 'E', 'G', 'E', 'G', 'E', 'G', 'E',
             'G',
             'R', 'E', 'G', 'E', 'G', 'E', 'G', 'E',
             'A',
             'R', 'F', 'A', 'F', 'A', 'F', 'A', 'F',
             'A', 'A', 'G',
             'G', 'R', 'G', 'A', 'A#',
             'B', 'A', 'G', 'F',
             'C', 'A', 'G', 'A','G', 'Eb',
             'C',
             'R', 'E', 'G', 'E', 'G', 'E', 'G', 'E')
n_durr <- c(1,
            1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8,
            1,
            1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8,
            1,
            1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8,
            3/4, 1/8, 1/8,
            1/2, 1/4, 1/12, 1/12, 1/12,
            3/8, 1/8, 1/8, 3/8,
            3/8, 1/8, 1/12, 1/12, 1/12, 1/4,
            1,
            1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8)

writeSong(title = title, key = key, n_pitch = n_pitch, n_durr = n_durr)
# ~~~
title <- "Frankie_And_Johnny"
key <- "F"
n_pitch <- c('F', 'A', 'C', 'D', 'C', 'A',
             'F', 'F',
             'F', 'A', 'C', 'D', 'C', 'A',
             'F',
             'Bb', 'C', 'D', 'F', 'D', 'F',
             'D', 'D',
             'F', 'F', 'F', 'E', 'D', 'C',
             'C', 'A', 'A', 'Ab', 'G',
             'G',
             'G', 'C', 'D', 'C', 'D', 'C', 'A',
             'F',
             'F', 'R')
n_durr <- c(1/8, 1/8, 1/8, 1/4, 1/8, 1/4,
            1/4, 3/4,
            1/8, 1/8, 1/8, 1/4, 1/8, 1/4,
            1,
            1/8, 1/8, 1/8, 1/4, 1/8, 1/4,
            1/4, 3/4,
            1/4, 1/8, 1/8, 1/4, 1/8, 1/8,
            1/4, 1/4, 1/4, 1/8, 1/8,
            1,
            1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/4,
            1,
            1/4, 3/4)
writeSong(title = title, key = key, n_pitch = n_pitch, n_durr = n_durr)

# ~~~
title <- "Royal_Garden_Blues"
key <- "Bb"
n_pitch <- c('Bb', 'R', 'Bb', 'C', 'C#', 'D',
             'Bb', 'Bb', 'Bb', 'F', 'G', 'F',
             'Bb', 'R', 'Bb', 'C', 'C#', 'D',
             'Bb', 'Bb', 'Bb', 'F', 'G',
             'Bb', 'R', 'Bb', 'C', 'C#', 'D',
             'Bb', 'Bb', 'Bb', 'F', 'G',
             'Bb', 'R', 'Bb', 'C', 'C#', 'D',
             'G', 'G', 'G',
             'R', 'Bb', 'Bb', 'C', 'C#', 'D',
             'F', 'F', 'F', 'F', 'G', 'G', 
             'Bb', 'Bb', 'Bb', 'Bb', 'Bb',
             'Bb', 'R')
n_durr <- c(1/4, 1/4, 1/8, 1/8, 1/8, 1/8, 
            1/8, 1/4, 1/4, 1/8, 1/8, 1/8,
            1/4, 1/4, 1/8, 1/8, 1/8, 1/8, 
            1/8, 1/4, 1/4, 1/8, 1/4,
            1/4, 1/4, 1/8, 1/8, 1/8, 1/8, 
            1/8, 1/4, 1/4, 1/8, 1/4,
            1/4, 1/4, 1/8, 1/8, 1/8, 1/8, 
            1/8, 1/4, 5/8,
            1/4, 1/4, 1/8, 1/8, 1/8, 1/8, 
            1/8, 1/4, 1/4, 1/8, 1/8, 1/8,
            1/4, 1/4, 1/8, 1/4, 1/8,
            3/4, 1/4)
writeSong(title = title, key = key, n_pitch = n_pitch, n_durr = n_durr)

# ~~~
title <- "Dallas_Blues"
key <- "Bb"
n_pitch <- c('D', 'C#', 'D', 'Eb', 'F',
             'F',
             'F', 'F', 'F', 'G', 'Ab',
             'Ab',
             'Eb', 'Eb', 'Bb', 'G',
             'Db', 'C', 'Bb', 'G', 'Bb',
             'Bb',
             'Bb', 'R', 'C#', 'D', 'C#',
             'D', 'C', 'A', 'F',
             'Db', 'C', 'Bb', 'G', 'Bb',
             'Bb',
             'Bb', 'R', 'F', 'E', 'Eb')
n_durr <- c(3/8, 1/8, 1/4, 1/8, 1/8,
            1,
            1/4, 1/4, 1/4, 1/8, 1/8,
            1,
            1/4, 1/4, 1/8, 3/8,
            1/4, 1/4, 1/8, 1/4, 1/8,
            1,
            1/2, 1/8, 1/8, 1/8, 1/8,
            1/4, 1/4, 1/8, 3/8,
            1/4, 1/4, 1/4, 1/8, 1/8,
            1,
            1/2, 1/8, 1/8, 1/8, 1/8)
writeSong(title = title, key = key, n_pitch = n_pitch, n_durr = n_durr)
# ~~~
title <- "Dallas_Blues_Chorus"
key <- "Bb"
n_pitch <- c('D', 'F', 'D', 'F',
             'Eb', 'Eb', 'F', 'A', 'C', 'Bb',
             'Bb',
             'R', 'Bb', 'C', 'C',
             'Db', 'Db', 'C', 'Bb', 'G',
             'Db', 'C', 'Db', 'C', 'Bb', 'G', 'F',
             'F',
             'F', 'R', 'D', 'C',
             'D', 'D', 'Eb', 'D', 'C', 'A', 'F',
             'Eb', 'F', 'A', 'C', 'Bb',
             'Bb',
             'Bb', 'R')
n_durr <- c(1/4, 1/4, 3/8, 1/8,
            1/8, 1/8, 1/4, 1/4, 1/8, 1/8,
            1,
            3/4, 1/12, 1/12, 1/12, 
            1/8, 1/8, 1/4, 1/8, 3/8,
            1/8, 1/8, 1/8, 1/8, 1/4, 1/8, 1/8,
            1,
            1/2, 1/8, 1/4, 1/8,
            1/8, 1/8, 1/4, 1/8, 1/8, 1/8, 1/8,
            1/4, 1/4, 1/4, 1/8, 1/8,
            1,
            3/4, 1/4)
writeSong(title = title, key = key, n_pitch = n_pitch, n_durr = n_durr)

# ~~~
title <- "Mecca_Flat_Blues"
key <- "C"
n_pitch <- c('E', 'G', 'A', 'G', 'G#',
             'A', 'C', 'D', 'C', 'A', 'G',
             'G',
             'G',
             'A', 'C', 'D','C','C',
             'A', 'C', 'D', 'C', 'A', 'G',
             'G',
             'G', 'R', 'C',
             'B', 'D', 'E', 'F', 'G',
             'A', 'C', 'D', 'Eb', 'D',
             'C',
             'C')
n_durr <- c(1/8, 1/8, 1/8, 2/4, 1/8,
            1/8, 1/8, 1/8, 3/8, 1/8, 1/8,
            1,
            1,
            1/8, 1/8, 1/8, 2/4, 1/8,
            1/8, 1/8, 1/8, 3/8, 1/8, 1/8,
            1,
            3/4, 1/8, 1/8,
            1/8, 1/8, 1/8, 2/4, 1/8,
            1/8, 1/8, 1/8, 3/8, 1/4,
            1,
            1)
writeSong(title = title, key = key, n_pitch = n_pitch, n_durr = n_durr)

# ~~~
title <- 'Beale_Street_Blues_A'
key <- "Bb"
n_pitch <- c('R', 'Bb', 'F', 'F#',
             'G', 'F#', 'G', 'Ab', 'G', 'F', 'F',
             'F', 
             'F', 'R',
             'R', 'Bb', 'F#', 'G',
             'Bb', 'F#', 'G', 'Bb', 'G', 'Eb', 'Bb',
             'Bb',
             'Bb', 'R', 'F',
             'G#', 'A', 'G#', 'A', 'G', 'F',
             'R', 'F', 'G', 'A', 'G', 'F', 'Bb',
             'Bb', 
             'Bb', 'R')
n_durr <- c(1/4, 1/4, 1/4, 1/4,
           1/8, 1/8, 1/8, 1/8, 1/4, 1/8, 1/8,
           1,
           3/4, 1/4,
           1/4, 1/4, 1/4, 1/4,
           1/8, 1/8, 1/8, 1/8, 1/4, 1/8, 1/8,
           1,
           3/4, 1/8, 1/8,
           1/8, 1/8, 1/8, 1/8, 1/8, 3/8,
           1/8, 1/8, 1/8, 1/8, 1/4, 1/8, 1/8,
           1,
           3/4, 1/4)
writeSong(title = title, key = key, n_pitch = n_pitch, n_durr = n_durr)

# ~~~ 
title <- 'Beale_Street_Blues_C'
key <- 'Eb'
n_pitch <- c('Bb', 'Bb', 'Bb', 'Bb',
             'Bb', 'Gb', 'Gb', 'Eb', 'Gb', 'F', 'Eb',
             'Eb',
             'Eb', 'Bb', 'Eb',
             'Bb', 'Bb', 'Bb', 'Ab',
             'R', 'Bb', 'Bb', 'Ab', 'Gb', 'F', 'Eb',
             'Eb',
             'Eb', 'F#', 'G',
             'Bb', 'Bb', 'Bb', 'Ab', 'Bb',
             'R', 'Bb', 'Bb', 'Ab', 'Gb', 'F', 'Eb',
             'Eb',
             'Eb')
n_durr <- c(1/4, 1/8, 1/4, 3/8,
            1/8, 1/8, 1/8, 1/8, 1/8, 1/4, 1/8,
            1,
            2/4, 3/8, 1/8, 
            1/4, 1/8, 1/4, 3/8,
            1/8, 1/8, 1/8, 1/8, 1/4, 1/8, 1/8,
            1,
            3/4, 1/8, 1/8,
            1/4, 1/8, 1/8, 1/8, 3/8,
            1/8, 1/8, 1/8, 1/8, 1/4, 1/8, 1/8,
            1,
            1)
writeSong(title = title, key = key, n_pitch = n_pitch, n_durr = n_durr)

# ~~~
title <- 'Basin_Street_Blues'
key <- 'Bb'
n_pitch <- c('Bb', 'R', 'F',
             'Ab', 'G', 'F', 'F', 'G',
             'Bb', 'R', 'F',
             'Ab', 'F', 'G', 'F', 'Bb', 'C',
             'Eb', 'R', 'Bb',
             'Db', 'C', 'Bb',
             'D', 'F',
             'A', 'G', 'D',
             'F', 'F', 'F', 'Eb', 'F',
             'R', 'C#', 'D', 'F','D','Db','C', 'Bb',
             'Bb',
             'Bb')
n_durr <- c(3/4, 1/8, 1/8,
            1/4, 1/8, 3/8, 1/8, 1/8,
            3/4, 1/8, 1/8, 
            1/8, 1/8, 1/8, 3/8, 1/8, 1/8,
            3/4, 1/8, 1/8,
            1/4, 1/8, 5/8,
            1/8, 7/8,
            1/4, 1/8, 5/8,
            1/4, 1/8, 1/8, 1/8, 3/8,
            1/4, 1/8, 1/8, 1/12, 1/12, 1/12, 1/8, 1/8,
            1,
            1)
writeSong(title = title, key = key, n_pitch = n_pitch, n_durr = n_durr)

# ~~~ 
title <- 'C_Jam_Blues'
key <- 'C'
n_pitch <- c('G', 'G', 'R',
             'G', 'G', 'R', 'G', 'G', 'R',
             'G', 'C', 'R',
             'R',
             'G', 'G', 'R',
             'G', 'G', 'R', 'G', 'G', 'R',
             'G', 'C', 'R',
             'R',
             'G', 'G', 'R',
             'G', 'G', 'R', 'G', 'G', 'R',
             'G', 'C', 'R',
             'R')
n_durr <- c(1/8, 1/8, 3/4,
            1/8, 1/8, 1/4, 1/8, 1/8, 1/4,
            3/8, 1/8, 2/4,
            1,
            1/8, 1/8, 3/4,
            1/8, 1/8, 1/4, 1/8, 1/8, 1/4,
            3/8, 1/8, 2/4,
            1,1/8, 1/8, 3/4,
            1/8, 1/8, 1/4, 1/8, 1/8, 1/4,
            3/8, 1/8, 2/4,
            1)
writeSong(title = title, key = key, n_pitch = n_pitch, n_durr = n_durr)

# ~~~
title <- 'Apex_Blues_A'
key <- 'Eb'
n_pitch <- c('G', 'Ab', 'A', 'Bb', 'A', 'Ab', 'G',
             'G', 'Ab', 'A', 'Bb', 'A', 'Ab', 'G',
             'G', 'Ab', 'A', 'Bb', 'A', 'Ab', 'G', 'Ab', 'A', 'Bb', 'A', 'Ab',
             'G', 'Ab', 'A', 'Bb', 'A', 'Ab', 'G',
             'C', 'C#', 'D', 'Eb', 'D', 'Db', 'C',
             'C', 'C#', 'D', 'Eb', 'D', 'Db', 'C',
             'G', 'Ab', 'A', 'Bb', 'A', 'Ab', 'G', 'Ab', 'A', 'Bb', 'A', 'Ab',
             'G', 'Ab', 'A', 'Bb', 'A', 'Ab', 'G', 'C', 'G',
             'C', 'F#', 'G', 'Eb', 'C',
             'F#', 'G', 'F#', 'G', 'Bb', 'C', 'Eb',
             'Eb',
             'Eb', 'R', 'Bb')
n_durr <- c(1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 2/4,
            1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 2/4,
            1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12,
            1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 2/4,
            1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 2/4,
            1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 2/4,
            1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12,
            1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/8, 1/4, 1/8,
            1/4, 1/8, 1/8, 1/8, 3/8,
            1/8, 1/8, 1/8, 1/8, 1/4, 1/8, 1/8,
            1,
            3/4, 1/8, 1/8)
writeSong(title = title, key = key, n_pitch = n_pitch, n_durr = n_durr)

# ~~~
title <- 'Apex_Blues_B'
key <- 'Eb'
n_pitch <- c('G', 'Bb', 'F', 'Eb', 'Bb',
             'G', 'Bb', 'F', 'Eb',
             'G', 'Bb', 'F', 'Eb', 'Bb',
             'G', 'Bb', 'F', 'Eb',
             'C', 'Eb', 'Bb', 'Ab', 'Eb',
             'C', 'Eb', 'Bb', 'Ab',
             'G', 'Bb', 'F', 'Eb', 'Bb',
             'G', 'F', 'E', 'C',
             'Eb', 'Eb', 'Eb', 'C', 'Eb',
             'G', 'Ab', 'A', 'Bb', 'G', 'Bb', 'Eb',
             'Eb', 
             'Eb', 'R')
n_durr <- c(1/8, 1/8, 1/4, 1/8, 3/8,
            1/8, 1/8, 1/8, 5/8, 
            1/8, 1/8, 1/4, 1/8, 3/8,
            1/8, 1/8, 1/8, 5/8,
            1/8, 1/8, 1/4, 1/8, 3/8,
            1/8, 1/8, 1/8, 5/8,
            1/8, 1/8, 1/4, 1/8, 3/8,
            1/4, 1/8, 2/4, 1/8,
            1/4, 1/8, 1/8, 1/8, 3/8,
            1/8, 1/8, 1/8, 1/8, 1/4, 1/8, 1/8,
            1,
            3/4, 1/4)
writeSong(title = title, key = key, n_pitch = n_pitch, n_durr = n_durr)

# ~~~
title <- 'Apex_Blues_C'
key <- 'Eb'
n_pitch <- c('R', 'Eb', 'Eb', 'C', 'C',
             'G', 'G', 'Eb',
             'R', 'Eb', 'Eb', 'C', 'C',
             'G', 'G', 'Eb',
             'R', 'Eb', 'Eb', 'C', 'C',
             'Gb', 'Gb', 'Eb',
             'R', 'Eb', 'Eb', 'C', 'C',
             'G', 'G', 'C', 'G',
             'C', 'F#', 'G', 'Eb', 'C',
             'Bb', 'Ab', 'Bb', 'Ab', 'Gb', 'F', 'Eb',
             'Eb',
             'Eb', 'R')
n_durr <- c(1/8, 1/4, 1/8, 1/4, 1/4,
            1/4, 1/8, 5/8,
            1/8, 1/4, 1/8, 1/4, 1/4,
            1/4, 1/8, 5/8,
            1/8, 1/4, 1/8, 1/4, 1/4,
            1/4, 1/8, 5/8,
            1/8, 1/4, 1/8, 1/4, 1/4,
            1/4, 1/8, 2/4, 1/8,
            1/4, 1/8, 1/8, 1/8, 3/8,
            1/8, 1/8, 1/8, 1/8, 1/4, 1/8, 1/8,
            1,
            3/4, 1/4)
writeSong(title = title, key = key, n_pitch = n_pitch, n_durr = n_durr)

# ~~~
title <- 'Friendless_Blues_A'
key <- 'F'
n_pitch <- c('A', 'A', 'A', 
             'A', 'G', 
             'F',
             'G', 'A',
             'D', 'D', 'D',
             'F', 'D',
             'F', 
             'A',
             'G', 'G', 'G', 
             'A', 'G', 
             'F', 'G', 'F', 
             'F')
n_durr <- c(1/4, 2/4, 1/4,
            2/4, 2/4,
            1, 
            2/4, 2/4,
            1/4, 2/4, 1/4,
            2/4, 2/4,
            1, 
            1, 
            1/4, 2/4, 1/4,
            2/4, 2/4,
            2/4, 1/8, 3/8,
            1)
writeSong(title = title, key = key, n_pitch = n_pitch, n_durr = n_durr)

# ~~~
title <- 'Friendless_Blues_B'
key <- 'F'
n_pitch <- c('A', 'C', 'F', 'A', 'A',
             'Bb', 'C', 'Bb', 'A', 'Ab', 'G',
             'F', 'Bb', 'C', 'Bb',
             'A', 'Ab', 'G', 'F',
             'R', 'F', 'Bb', 'D', 'F', 'F',
             'F', 'G', 'F', 'D', 'F', 'F', 'F',
             'F',
             'A', 'Ab', 'G', 'F', 'A', 'F',
             'G', 'G', 'G', 'G',
             'A', 'G', 
             'F', 'G', 'F',
             'F', 'R')
n_durr <- c(1/8, 1/8, 1/8, 2/4, 1/8,
            1/8, 1/8, 1/8, 1/4, 1/8, 1/4,
            2/4, 1/8, 1/8, 1/4,
            1/8, 1/8, 1/8, 5/8,
            1/8, 1/8, 1/8, 1/8, 1/8, 3/8,
            1/8, 1/8, 1/8, 1/8, 1/4, 1/8, 1/8,
            1,
            1/8, 1/8, 1/8, 1/4, 1/4, 1/8,
            1/8, 5/8, 1/8, 1/8,
            2/4, 2/4,
            2/4, 1/8, 3/8,
            3/4, 1/4)
writeSong(title = title, key = key, n_pitch = n_pitch, n_durr = n_durr)

# ~~~
title <- 'Weary_Blues'
key <- 'F'
n_pitch <- c('C',
             'A',
             'F', 'R',
             'R', 
             'R', 'F', 'G', 'F', 'Ab', 'F', 'G',
             'R',
             'R', 'C', 'D', 'C', 'Eb', 'C', 'D',
             'R', 
             'C', 'C', 'C',
             'C', 'Bb', 'Bb', 'A', 'A', 'G',
             'F', 
             'F', 'A', 'Bb', 'B')
n_durr <- c(1,
            1,
            1/4, 3/4,
            1,
            1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/4,
            1,
            1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/4,
            1,
            1/4, 1/8, 5/8,
            1/4, 1/8, 1/8, 1/8, 1/8, 1/4,
            1,
            1/4, 1/4, 1/4, 1/4)
writeSong(title = title, key = key, n_pitch = n_pitch, n_durr = n_durr)

# ~~~
title <- 'St_Louis_Blues_A'
key <- 'G'
n_pitch <- c('B', 'D', 'B', 'G',
             'G', 'E', 'G', 'A', 'Bb', 'A', 'G',
             'G',
             'G',
             'E', 'G', 'A', 'A',
             'R', 'E', 'G', 'A', 'Bb', 'A', 'G',
             'G',
             'G',
             'F#', 'A', 'F#', 'D',
             'R', 'Bb', 'Bb', 'G', 'Bb', 'A', 'G',
             'G', 
             'G', 'R', 'Bb')
n_durr <- c(1/8, 1/8, 1/8, 5/8,
            1/8, 1/8, 1/8, 1/8, 1/4, 1/8, 1/8,
            1,
            1,
            1/8, 1/8, 1/8, 5/8,
            1/8, 1/8, 1/8, 1/8, 1/4, 1/8, 1/8,
            1,
            1,
            1/8, 1/8, 1/8, 5/8,
            1/8, 1/8, 1/8, 1/8, 1/4, 1/8, 1/8,
            1, 
            3/4, 1/8, 1/8)
writeSong(title = title, key = key, n_pitch = n_pitch, n_durr = n_durr)

# ~~~
title <- 'St_Louis_Blues_C'
key <- 'G'
n_pitch <- c('G', 'A#', 'B', 'G', 'A#', 'B',
             'G', 'A#', 'B', 'G', 'A#', 'B', 'G', 
             'G',
             'G', 'A#', 'B',
             'A', 'G', 'E', 'G', 'A#', 'B',
             'A', 'G', 'E', 'G', 'A#', 'B', 'G',
             'G',
             'G', 'A#', 'B',
             'D', 'A#', 'B', 'D', 'A#', 'B',
             'C', 'A#', 'B', 'A', 'G', 'E', 'G',
             'G', 
             'G', 'R')
n_durr <- c(1/4, 1/8, 1/8, 1/4, 1/8, 1/8, 
            1/8, 1/8, 1/8, 1/8, 1/8, 1/4, 1/8, 
            1,
            3/4, 1/8, 1/8, 
            1/4, 1/8, 1/8, 1/4, 1/8, 1/8,
            1/4, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8,
            1,
            3/4, 1/8, 1/8,
            1/4, 1/8, 1/8, 1/4, 1/8, 1/8,
            1/4, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8,
            1, 
            3/4, 1/4)
writeSong(title = title, key = key, n_pitch = n_pitch, n_durr = n_durr)

# ~~~
title <- 'Canal_Street_Blues_A'
key <- 'F'
n_pitch <- c('A', 'A', 'F', 'C', 'G#',
             'A', 'A', 'F', 'R', 'G#',
             'A', 'A', 'G#', 'A', 'A', 'G#',
             'A', 'R', 'D', 'Bb',
             'D', 'D', 'Bb', 'D', 'C#',
             'D', 'C#', 'D', 'Bb', 'C', 'B',
             'C', 'C', 'B', 'C', 'C', 'B',
             'C', 'C', 'D', 'E', 'C',
             'D', 'D','C', 'R',
             'G#', 'A', 'G', 'A', 'C', 'A', 'Ab', 'G',
             'F', 'F', 'C', 'C', 'C',
             'F', 'C', 'G#')
n_durr <- c(1/4, 1/8, 3/8, 1/8, 1/8,
            1/4, 1/8, 3/8, 1/8, 1/8,
            1/4, 1/8, 1/8, 1/4, 1/8, 1/8,
            2/4, 1/8, 1/4, 1/8,
            1/4, 1/8, 3/8, 1/8, 1/8,
            1/8, 1/8, 1/8, 3/8, 1/8, 1/8, 
            1/4, 1/8, 1/8, 1/4, 1/8, 1/8,
            2/4, 1/8, 1/8, 1/8, 1/8,
            1/4, 1/8, 3/8, 1/4,
            1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8,
            3/8, 1/8, 1/8, 1/8, 1/4,
            3/4, 1/8, 1/8)
writeSong(title = title, key = key, n_pitch = n_pitch, n_durr = n_durr)

# ~~~
title <- 'Canal_Street_Blues_B'
key <- 'F'
n_pitch <- c('A', 'F', 'C', 'A',
             'D', 
             'F', 'D', 'F', 'D',
             'C',
             'A', 'C', 'D', 'E', 'C',
             'D', 'C', 'D', 'C',
             'G#', 'A', 'G#', 'A', 'C', 'C', 'C',
             'F', 
             'F', 'C', 'G#')
n_durr <- c(1,
            1,
            1,
            1,
            1,
            1/4, 1/4, 1/4, 1/4,
            1,
            2/4, 1/8, 1/8, 1/8, 1/8,
            1/8, 1/8, 1/8, 5/8,
            1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/4,
            1,
            3/4, 1/8, 1/8)
writeSong(title = title, key = key, n_pitch = n_pitch, n_durr = n_durr)

# ~~~
title <- 'Shake_That_Thing'
key <- 'Eb'
n_pitch <- c('Db', 'C', 'Bb', 'Bb', 'C', 'Bb',
             'Db', 'C', 'Bb', 'Bb',
             'Db', 'Db', 'C', 'Bb', 'Bb', 'C', 'Bb',
             'Db', 'C', 'Bb', 'Eb', 'F', 'Eb',
             'Gb', 'F', 'Eb', 
             'Eb', 'R', 'Eb', 'F', 'Eb',
             'Gb', 'F', 'Eb',
             'Eb', 'R', 'G', 'Ab', 'A',
             'Bb', 'Bb', 'Bb',
             'R', 'F#', 'G', 'Bb', 'Bb', 'Eb',
             'Eb',
             'Eb', 'R')
n_durr <- c(1/4, 1/4, 1/8, 1/8, 1/8, 1/8, 
            1/4, 1/8, 2/4, 1/8,
            1/8, 1/8, 1/4, 1/8, 1/8, 1/8, 1/8,
            1/4, 1/8, 1/4, 1/8, 1/8, 1/8,
            1/4, 1/8, 5/8,
            2/4, 1/8, 1/8, 1/8, 1/8,
            1/4, 1/8, 5/8,
            2/4, 1/8, 1/8, 1/8, 1/8,
            1/4, 1/4, 2/4,
            1/4, 1/8, 1/8, 1/4, 1/8, 1/8,
            1,
            3/4, 1/4)
writeSong(title = title, key = key, n_pitch = n_pitch, n_durr = n_durr)

# ~~~
title <- 'Nobody_Knows'
key <- 'G'
n_pitch <- c('B', 'D', 
             'D', 'E', 'G', 'A', 'G', 'G', 'G',
             'G', 
             'G',
             'Bb', 'C',
             'C', 'E', 'G', 'A', 'G', 'G', 'G',
             'G', 
             'G',
             'B', 'D',
             'D', 'D', 'E', 'G', 'A', 'G', 'G', 'G',
             'G',
             'G')
n_durr <- c(2/4, 2/4,
            1/4, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8,
            1,
            1,
            2/4, 2/4,
            1/4, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8,
            1,
            1,
            2/4, 2/4,
            1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8,
            1,
            1)
writeSong(title = title, key = key, n_pitch = n_pitch, n_durr = n_durr)
