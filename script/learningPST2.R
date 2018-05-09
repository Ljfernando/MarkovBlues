library(PST)
library(magrittr)
setwd("~/Desktop/USFSpring2018/Stochastic_Processes/project/")

songs <- list.files("songs")

# ~~~~~~~~ By section
Ia <- list()
Ib <- list()
IV <- list()
Ic <- list()
V <- list()
Id <- list()
for(i in seq_along(songs)){
  song <- read.csv(paste("recoded_songs/", songs[i], sep=""), stringsAsFactors = FALSE)
  Ia[[i]] <- song$state[song$section == "Ia"]
  Ib[[i]] <- song$state[song$section == "Ib"]
  IV[[i]] <- song$state[song$section == "IV"]
  Ic[[i]] <- song$state[song$section == "Ic"]
  V[[i]] <- song$state[song$section == "V"]
  Id[[i]] <- song$state[song$section == "Id"]
}

Ia <- matrix(unlist(Ia), ncol = 16, byrow = TRUE)
Ib <- matrix(unlist(Ib), ncol = 16, byrow = TRUE)
IV <- matrix(unlist(IV), ncol = 16, byrow = TRUE)
Ic <- matrix(unlist(Ic), ncol = 16, byrow = TRUE)
V <- matrix(unlist(V), ncol = 16, byrow = TRUE)
Id <- matrix(unlist(Id), ncol = 16, byrow = TRUE)
mel_secs <- rbind(Ia, Ib, IV, Ic, V, Id)
section <- c(rep("Ia", length(songs)),
             rep("Ib", length(songs)),
             rep("IV", length(songs)),
             rep("Ic", length(songs)),
             rep("V", length(songs)),
             rep("Id", length(songs)))

# As a group
seq <- seqdef(mel_secs, alphabet = c("C_S", "C_L", "Db_S", "Db_L", "D_S", "D_L",
                                     "Eb_S", "Eb_L", "E_S", "E_L", "F_S", "F_L",
                                     "Gb_S", "Gb_L", "G_S", "G_L", "Ab_S", "Ab_L",
                                     "A_S", "A_L", "Bb_S", "Bb_L", "B_S", "B_L",
                                     "R_S", "R_L"),
              cpal = rep(c(brewer.pal(12, 'Set3'), 'black'), each = 2))

Ia.seq <- seqdef(Ia, alphabet = c("C_S", "C_L", "Db_S", "Db_L", "D_S", "D_L",
                                     "Eb_S", "Eb_L", "E_S", "E_L", "F_S", "F_L",
                                     "Gb_S", "Gb_L", "G_S", "G_L", "Ab_S", "Ab_L",
                                     "A_S", "A_L", "Bb_S", "Bb_L", "B_S", "B_L",
                                     "R_S", "R_L"),
              cpal = rep(c(brewer.pal(12, 'Set3'), 'black'), each = 2))
Ib.seq <- seqdef(Ib, alphabet = c("C_S", "C_L", "Db_S", "Db_L", "D_S", "D_L",
                                  "Eb_S", "Eb_L", "E_S", "E_L", "F_S", "F_L",
                                  "Gb_S", "Gb_L", "G_S", "G_L", "Ab_S", "Ab_L",
                                  "A_S", "A_L", "Bb_S", "Bb_L", "B_S", "B_L",
                                  "R_S", "R_L"),
                 cpal = rep(c(brewer.pal(12, 'Set3'), 'black'), each = 2))
Ic.seq <- seqdef(Ic, alphabet = c("C_S", "C_L", "Db_S", "Db_L", "D_S", "D_L",
                                  "Eb_S", "Eb_L", "E_S", "E_L", "F_S", "F_L",
                                  "Gb_S", "Gb_L", "G_S", "G_L", "Ab_S", "Ab_L",
                                  "A_S", "A_L", "Bb_S", "Bb_L", "B_S", "B_L",
                                  "R_S", "R_L"),
                 cpal = rep(c(brewer.pal(12, 'Set3'), 'black'), each = 2))
Id.seq <- seqdef(Id, alphabet = c("C_S", "C_L", "Db_S", "Db_L", "D_S", "D_L",
                                  "Eb_S", "Eb_L", "E_S", "E_L", "F_S", "F_L",
                                  "Gb_S", "Gb_L", "G_S", "G_L", "Ab_S", "Ab_L",
                                  "A_S", "A_L", "Bb_S", "Bb_L", "B_S", "B_L",
                                  "R_S", "R_L"),
                 cpal = rep(c(brewer.pal(12, 'Set3'), 'black'), each = 2))
IV.seq <- seqdef(IV, alphabet = c("C_S", "C_L", "Db_S", "Db_L", "D_S", "D_L",
                                  "Eb_S", "Eb_L", "E_S", "E_L", "F_S", "F_L",
                                  "Gb_S", "Gb_L", "G_S", "G_L", "Ab_S", "Ab_L",
                                  "A_S", "A_L", "Bb_S", "Bb_L", "B_S", "B_L",
                                  "R_S", "R_L"),
                 cpal = rep(c(brewer.pal(12, 'Set3'), 'black'), each = 2))
V.seq <- seqdef(V, alphabet = c("C_S", "C_L", "Db_S", "Db_L", "D_S", "D_L",
                                  "Eb_S", "Eb_L", "E_S", "E_L", "F_S", "F_L",
                                  "Gb_S", "Gb_L", "G_S", "G_L", "Ab_S", "Ab_L",
                                  "A_S", "A_L", "Bb_S", "Bb_L", "B_S", "B_L",
                                  "R_S", "R_L"),
                 cpal = rep(c(brewer.pal(12, 'Set3'), 'black'), each = 2))


Ia.pst <- pstree(Ia.seq, nmin=1, ymin=0)
Ia.gen <- generate(Ia.pst, l = 16, n = 1, method="max")

Ib.pst <- pstree(Ib.seq, nmin=1, ymin=0)
Ib.gen <- generate(Ib.pst, l = 16, n = 1, method="max")

IV.pst <- pstree(IV.seq, nmin=1, ymin=0)
IV.gen <- generate(IV.pst, l = 16, n = 1, method="max")

Ic.pst <- pstree(Ic.seq, nmin=1, ymin=0)
Ic.gen <- generate(Ic.pst, l = 16, n = 1, method="max")

V.pst <- pstree(V.seq, nmin=1, ymin=0)
V.gen <- generate(V.pst, l = 16, n = 1, method="max")

Id.pst <- pstree(Id.seq, nmin=1, ymin=0)
Id.gen <- generate(Id.pst, l = 16, n = 1, method="max")

blues <- data.frame(pitch = character(0),
                    mpitch = numeric(0),
                    value = character(0),
                    mval = numeric(0),
                    stringsAsFactors = FALSE)

blues <- rbind(blues, convertSeqToNotes(Ia.gen))
blues <- rbind(blues, convertSeqToNotes(Ib.gen))
blues <- rbind(blues, convertSeqToNotes(IV.gen))
blues <- rbind(blues, convertSeqToNotes(Ic.gen))
blues <- rbind(blues, convertSeqToNotes(V.gen))
blues <- rbind(blues, convertSeqToNotes(Id.gen))

write.csv(blues, "generated_melodies/csv/test.csv",row.names = FALSE)

blues2 <- data.frame(pitch = character(0),
                    mpitch = numeric(0),
                    value = character(0),
                    mval = numeric(0),
                    stringsAsFactors = FALSE)

blues2 <- rbind(blues2, convertSeqToNotes(Ia.gen))
blues2 <- rbind(blues2, convertSeqToNotes(Ia.gen))
blues2 <- rbind(blues2, convertSeqToNotes(IV.gen))
blues2 <- rbind(blues2, convertSeqToNotes(Ia.gen))
blues2 <- rbind(blues2, convertSeqToNotes(V.gen))
blues2 <- rbind(blues2, convertSeqToNotes(Ia.gen))
write.csv(blues2, "generated_melodies/csv/test2.csv",row.names = FALSE)


# Ia.pst.prune <- prune(Ia.pst, nmin = 16)
# plot(Ia.pst.prune)
# gen <- generate(Ia.pst.prune, l = 32, n = 10, method="max")
# 
# pst <- pstree(seq, ymin=0.001, group = as.factor(section))
# 
# gen <- generate(pst, l = 16, L = 10, n = 10, method="prob")
