rm(list=ls())
library(readxl)
library(data.table)

setwd("F:/Documents/Dropbox (Personal)/")

ct <- data.table(read_excel("cartones.xlsx", "cartones"))
cb <- data.frame(read_excel("cartones.xlsx", "Combinaciones", col_names=F))
tp <- data.table(read_excel("cartones.xlsx", "tiradas", skip=1))

colnames(cb) <- c("x", "a", "b", "c", "d", "e")
eq <- setNames(1:25, c("B1","B2","B3","B4","B5","I1","I2","I3","I4","I5","N1","N2","N3","N4","N5","G1","G2","G3","G4","G5","O1","O2","O3","O4","O5"))
for (a in 1:5) {
  cb[, a+1] <- eq[unlist(cb[ ,a+1])]
}


juego <- 7

numeros <- unlist(tp[Juego == juego, 4:ncol(tp), with=F])
numeros[is.na(numeros)] <- 0
cart <- unlist(tp[Juego == juego, c("SI", "SF")])
juegodat <- ct[SERIE >= cart[1] & SERIE <= cart[2]]

matchs <- matrix(as.numeric(as.matrix(juegodat)[, 2:ncol(juegodat)] %in% unique(numeros)), ncol=ncol(juegodat)-1)

lapply(1:nrow(cb), function(row) {
  iv <- cb[row, 2:ncol(cb)]
  acc <- rowSums(matchs[, unlist(iv[!is.na(iv)])]) == sum(!is.na(iv))
  if (sum(acc) == 0) {
    c(cb[row, 1], "-")
  } else {
    c(cb[row, 1], unlist(juegodat[acc, 1]))
  }
})

juegodat[rowSums(matchs) == 25, 1]
