# install.packages("PST")
library(PST)
songs <- list.files("songs")
melodies.list <- list()
for(i in seq_along(songs)){
  melodies.list[[i]] <- read.csv(paste("songs/", songs[i], sep=""), stringsAsFactors = FALSE)$pitches
}
max_melody <- lengths(melodies.list) %>% max()

for(i in seq_along(melodies.list)){
  cur_length <- length(melodies.list[[i]])
  melodies.list[[i]] <- c(melodies.list[[i]], rep(NA, max_melody - cur_length))
}

melodies.mat <-  matrix(unlist(melodies.list), ncol = max_melody, byrow = TRUE)
mel.seq <- seqdef(melodies.mat, alphabet = c("C", "Db", "D", "Eb", "E", "F",
                                             "Gb", "G", "Ab", "A", "Bb", "B", "R"),
                  cpal = c(brewer.pal(12, 'Set3'), 'black'))

S1 <- cprob(mel.seq, L=16, prob=TRUE)
S1 <- pstree(mel.seq, L=16, nmin=2, ymin=0.001)

gen.S1 <- generate(object = S1, l = 16, n =5, method='prob')
 
# ~~~~~~~~ By section
Ia <- list()
Ib <- list()
IV <- list()
Ic <- list()
V <- list()
Id <- list()
for(i in seq_along(songs)){
  song <- read.csv(paste("songs/", songs[i], sep=""), stringsAsFactors = FALSE)
  Ia[[i]] <- song$pitch[song$section == "Ia"]
  Ib[[i]] <- song$pitch[song$section == "Ib"]
  IV[[i]] <- song$pitch[song$section == "IV"]
  Ic[[i]] <- song$pitch[song$section == "Ic"]
  V[[i]] <- song$pitch[song$section == "V"]
  Id[[i]] <- song$pitch[song$section == "Id"]
}
max_Ia <- lengths(Ia) %>% max()
max_Ib <- lengths(Ib) %>% max()
max_IV <- lengths(IV) %>% max()
max_Ic <- lengths(Ic) %>% max()
max_V <- lengths(V) %>% max()
max_Id <- lengths(Id) %>% max()
max_mel <- max(max_Ia, max_Ib, max_IV,
               max_Ic, max_V, max_Id)

imputeNA <- function(sec.list, max_mel){
  for(i in seq_along(sec.list)){
    cur_length <- length(sec.list[[i]])
    sec.list[[i]] <- c(sec.list[[i]], rep(NA, max_mel - cur_length))
  }
  return(sec.list)
}
Ia <- imputeNA(Ia, max_mel)
Ib <- imputeNA(Ib, max_mel)
IV <- imputeNA(IV, max_mel)
Ic <- imputeNA(Ic, max_mel)
V <- imputeNA(V, max_mel)
Id <- imputeNA(Id, max_mel)



Ia <- matrix(unlist(Ia), ncol = max_mel, byrow = TRUE)
Ib <- matrix(unlist(Ib), ncol = max_mel, byrow = TRUE)
IV <- matrix(unlist(IV), ncol = max_mel, byrow = TRUE)
Ic <- matrix(unlist(Ic), ncol = max_mel, byrow = TRUE)
V <- matrix(unlist(V), ncol = max_mel, byrow = TRUE)
Id <- matrix(unlist(Id), ncol = max_mel, byrow = TRUE)
mel_secs <- rbind(Ia, Ib, IV, Ic, V, Id)
section <- c(rep("Ia", length(songs)),
             rep("Ib", length(songs)),
             rep("IV", length(songs)),
             rep("Ic", length(songs)),
             rep("V", length(songs)),
             rep("Id", length(songs)))
# As a group
seq <- seqdef(mel_secs, alphabet = c("C", "Db", "D", "Eb", "E", "F",
                               "Gb", "G", "Ab", "A", "Bb", "B", "R"),
              cpal = c(brewer.pal(12, 'Set3'), 'black'))
seq[seq == "%"] <- "*"
pst <- pstree(seq, ymin=0.001, group = as.factor(section), with.missing = TRUE)

# Separately
Ia.seq <- seqdef(Ia, alphabet = c("C", "Db", "D", "Eb", "E", "F",
                                             "Gb", "G", "Ab", "A", "Bb", "B", "R"),
                  cpal = c(brewer.pal(12, 'Set3'), 'black'))
Ia.seq[Ia.seq == "%"] <- "*"

Ib.seq <- seqdef(Ib, alphabet = c("C", "Db", "D", "Eb", "E", "F",
                                  "Gb", "G", "Ab", "A", "Bb", "B", "R"),
                 cpal = c(brewer.pal(12, 'Set3'), 'black'))
Ib.seq[Ib.seq == "%"] <- "*"

IV.seq <- seqdef(IV, alphabet = c("C", "Db", "D", "Eb", "E", "F",
                                  "Gb", "G", "Ab", "A", "Bb", "B", "R"),
                 cpal = c(brewer.pal(12, 'Set3'), 'black'))
Ic.seq <- seqdef(Ic, alphabet = c("C", "Db", "D", "Eb", "E", "F",
                                  "Gb", "G", "Ab", "A", "Bb", "B", "R"),
                 cpal = c(brewer.pal(12, 'Set3'), 'black'))
V.seq <- seqdef(V, alphabet = c("C", "Db", "D", "Eb", "E", "F",
                                  "Gb", "G", "Ab", "A", "Bb", "B", "R"),
                 cpal = c(brewer.pal(12, 'Set3'), 'black'))
Id.seq <- seqdef(Id, alphabet = c("C", "Db", "D", "Eb", "E", "F",
                                  "Gb", "G", "Ab", "A", "Bb", "B", "R"),
                 cpal = c(brewer.pal(12, 'Set3'), 'black'))
Id.seq[Id.seq == "%"] <- "*"


Ia.pst <- pstree(Ia.seq, L = 4, nmin=2, ymin=0.001, with.missing = TRUE)
Ib.pst <- pstree(Ib.seq, L = 4, nmin=2, ymin=0.001, with.missing = TRUE)
Ic.pst <- pstree(Ic.seq, L = 4, nmin=2, ymin=0.001, with.missing = TRUE)
Id.pst <- pstree(Id.seq, L = 4, nmin=2, ymin=0.001, with.missing = TRUE)

distAB <- pdist(Ia.pst, Ib.pst, l=8, output="mean", symetric=TRUE)
distAC <- pdist(Ia.pst, Ic.pst, l=8, output="mean", symetric=TRUE)
distAD <- pdist(Ia.pst, Id.pst, l=8, output="mean", symetric=TRUE)
distBC <- pdist(Ib.pst, Ic.pst, l=8, output="mean", symetric=TRUE)
distBD <- pdist(Ib.pst, Id.pst, l=8, output="mean", symetric=TRUE)
distCD <- pdist(Ic.pst, Id.pst, l=8, output="mean", symetric=TRUE)

c(distAB, distAC, distAD, distBC, distBD, distCD)
distCA <- pdist(Ic.pst, Ia.pst, l=8, output="mean")
Ia.gen <- generate(object = Ia.pst, l = 16, n =5, method='prob')

predict(Ia.pst, Ia.gen, output = 'logloss')

Ib.pst <- pstree(Ib.seq, L = 0, nmin=3, ymin=0.001)

par(mfrow = c(2,1))

seq2 <- seqdef(mel_secs2, alphabet = c("C", "Db", "D", "Eb", "E", "F",
                                       "Gb", "G", "Ab", "A", "Bb", "B", "R"),
               cpal = c(brewer.pal(12, 'Set3'), 'black'))
pstree(mel_secs2)
