library(magick)
library(qrencoder)

setwd('C:/Users/Usuario/Desktop/invitacion/bingo/')
qrI <- "qr.png"

xp <- 113 #240
yp <- 510 #640
xjump <- 300
yjump <- 300
xshift <- 0
yshift <- 1650
xqr <- 770
yqr <- 1192
start <- 1
end <- 800

comb <- expand.grid(y=seq(yp, by=yjump, length.out = 5), x=seq(xp, by=xjump, length.out = 5))

bsample <- function(x) {
  p <- sample(31:45, 4)
  p <- c(p[1:2], 0, p[3:4])
  c(sample(1:15, 5), sample(16:30, 5), p, sample(46:60, 5), sample(61:75, 5))
}
nums <- lapply(start:end, bsample)
numo <- do.call("rbind", nums)
table(as.vector(numo))
hist(as.vector(numo), breaks=-1:75)
write.csv(cbind(start:end, numo), "cartones.csv", row.names=F)


qrGen <- function(x, file) {
  png(qrI, width=150, height=150)
  par(mar=c(0,0,0,0))
  image(qrencode_raster(paste(num, collapse="+")), 
        asp=1, col=c("white", "black"), axes=FALSE, 
        xlab="", ylab="")
  dev.off()
}

all <- list()
# pdf('C:/Users/Usuario/Desktop/invitacion/todas.pdf', paper="letter")
counter <- start
while (counter <= end ) {
  tic <- image_read('bingo_carton.png')
  for (ca in c(0, 1)) { # carton 1 y 2
    num <- nums[[counter]]
    # qrGen(num, qrI)
    # qr <- image_read(qrI)
    for (pos in 1:25) { # los 24 numeros
      if (pos != 13) {
        if (num[pos] >= 10) {
          tic <- image_annotate(tic, as.character(num[pos]), size = 200, col = "black", location=paste0("+", comb[pos, 'x'] + ca*yshift, "+", comb[pos, 'y']))
        } else {
          tic <- image_annotate(tic, as.character(num[pos]), size = 200, col = "black", location=paste0("+", comb[pos, 'x'] + 70 + ca*yshift, "+", comb[pos, 'y']))
        }
      }
    }
    tic <- image_annotate(tic, sprintf("%04d", counter), size = 50, col = "red", location=paste0("+", xqr + ca*yshift, "+", yqr))
    # tic <- image_composite(tic, qr, offset=paste0("+", xqr + 1000 + ca*yshift, "+", yqr))
    all[[counter]] <- c(counter, num)
    counter <- counter + 1
  }
  image_write(tic, paste0('cartones_', sprintf("%05d", counter - 1), '.png'), format="png")
  rm(tic)
  print(counter)
}

ans <- do.call("rbind", all)
write.csv(ans, "cartones_.csv")
# hist(as.vector(ans[ , -1]), breaks=75)

### -------
# https://math.stackexchange.com/questions/281173/expected-number-of-calls-for-bingo-win
# https://math.stackexchange.com/questions/1402884/bingo-probability-of-a-tie-with-20-players
# https://www.sciencenews.org/article/probabilities-bingo
# https://math.stackexchange.com/questions/1414051/how-to-calculate-the-odds-of-a-5x5-bingo-game
# https://math.stackexchange.com/questions/1451218/how-many-bingo-card-combinations-are-there
# http://perplexus.info/show.php?pid=8320&cid=50822
# https://onlinebingositesguide.com/probability-of-winning-housie/


