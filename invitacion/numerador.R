library(magick)
library(qrencoder)

qrI <- "C:/Users/Usuario/Desktop/invitacion/qr.png"
png(qrI, width=150, height=150)
par(mar=c(0,0,0,0))
image(qrencode_raster("https://www.facebook.com/events/289332338225403/"), 
      asp=1, col=c("white", "black"), axes=FALSE, 
      xlab="", ylab="")
dev.off()

xp <- 440
yp <- 250
xjump <- 1199
yjump <- 729
xqr <- 220
yqr <- -30

qr <- image_read(qrI)
# image_info(tic)
# image_browse(tic)

comb <- expand.grid(x=seq(xp, by=xjump, length.out = 2), y=seq(yp, by=yjump, length.out = 4))
counter <- 1

# pdf('C:/Users/Usuario/Desktop/invitacion/todas.pdf', paper="letter")
while (counter <= 160 ) {
  tic <- image_read('C:/Users/Usuario/Desktop/invitacion/Entradam.png')
  for (pos in 1:nrow(comb)) {
    print(counter)
    tic <- image_annotate(tic, sprintf("%05d", counter), size = 50, col = "red", location=paste0("+", comb[pos, 'x'], "+", comb[pos, 'y']))
    tic <- image_composite(tic, qr, offset=paste0("+", comb[pos, 'x'] + xqr, "+", comb[pos, 'y'] + yqr))
    counter <- counter + 1
  }
  image_write(tic, paste0('C:/Users/Usuario/Desktop/invitacion/E', sprintf("%05d", counter - 1), '.png'), format="png")
  # dev.off()  
}
# dev.off() 

