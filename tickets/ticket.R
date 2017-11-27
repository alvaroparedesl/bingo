library(magick)
library(qrencoder)

setwd('C:/Users/Usuario/Desktop/invitacion/tickets/')
qrI <- "qr.png"

xp <- 50
yp <- 45
xjump <- 255
yjump <- 150
xshift <- 0
yshift <- 0
xqr <- 770
yqr <- 1192
start <- 111
end <- 200

comb <- expand.grid(y=seq(yp, by=yjump, length.out = 11), x=seq(xp, by=xjump, length.out = 5))

counter <- start
while (counter <= end ) {
  tic <- image_read('tickets.png')
  for (p in 1:nrow(comb)) {
    tic <- image_annotate(tic, paste0("F - ", sprintf("%05d", counter)), size = 50, col = "red", location=paste0("+", comb[p, 'x'], "+", comb[p, 'y']), font = 'agency-fb')
    counter <- counter + 1
  }
  image_write(tic, paste0('tickets_F_', sprintf("%05d", counter - 1), '.png'), format="png")
  rm(tic)
  print(counter)
}


