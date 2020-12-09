args <- commandArgs(trailingOnly = TRUE)
tps_utils_path <- args[1]
InputFolder <- args[2]
BaseFolder <- args[3]
img <- args[4]
TIF <- args[5]
PathtoTPS <- args[6]

library(raster)
library(tiff)
library(jpeg)
library(stringr)
source(tps_utils_path)

# Get image file for specimen
if (TIF=="True"){
  for (file in list.files(path=InputFolder, pattern=paste('*', img, '.tif', sep=""), full.names=FALSE,
                          recursive=TRUE, include.dirs=FALSE)){
    im1 <- readTIFF(paste(InputFolder, '/', file, sep=""))
    }
} else {
  for (file in list.files(path=InputFolder, pattern=paste('*', img, '.jpg', sep=""), full.names=FALSE,
                          recursive=TRUE, include.dirs=FALSE)){
  im1 <- readJPEG(paste(InputFolder, '/', file, sep=""))
  }
}

pixh <- dim(im1)[1]
pixw <- dim(im1)[2]

# Read in TPS file, extract xy coords of curve from specimen
species <- str_split(img,"-")[[1]][1]
pop <- str_split(img,"-")[[1]][2]
individual <- as.integer(str_split(img,"-")[[1]][3])
PopName <- paste(species, "-", pop, sep="")
TPSpath <- paste(PathtoTPS, '/', PopName, "-digitized.tps", sep="")
cc <- read.tps(TPSpath)
c1 <- cc[[individual]]$curve1.points
c1c <- rbind(c1, c1[1,])  # add start point to the end to close the polygon

# the important part - make a PNG file and plot curve to it
MaskPath <- paste(BaseFolder, '/Training/', img, "/masks/", img, ".png", sep="")
png(filename = MaskPath, width = pixw, height = pixh, bg = "black")
par(mai = c(0,0,0,0), xaxs = "i", yaxs = "i")
plot(c1c, typ="n", xlim=c(0, pixw), ylim=c(0, pixh))
polygon(c1c, col = "white")
dev.off()
