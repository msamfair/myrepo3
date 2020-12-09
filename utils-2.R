#08/13/20
# Functions to replace transfermodel_utils.py in R

source("/Users/mayasamuels-fair/Desktop/RWorkingFolder/tps-oo.R")
library(tiff)
library(jpeg)
library(raster)
library(magick)

MakeFolders <- function(PathtoImages, WorkingDirectory, TIF=TRUE) {
  if (file.exists(paste(WorkingDirectory, "/Training", sep=""))) {
  } else {
    dir.create((paste(WorkingDirectory, "/Training", sep="")))
  }
  if (TIF==TRUE) {
    for (file in list.files(path=PathtoImages, pattern='.tif', full.names=FALSE,
                            recursive=TRUE, include.dirs=FALSE)) {
      SpecimenName <- strsplit(file, "[.]")[[1]][1]
      SpecimenName <- strsplit(SpecimenName, "/")
      SpecimenName <- SpecimenName[[1]][length(SpecimenName[[1]])]
      if (is.na(SpecimenName) == FALSE) {
        dir.create((paste(WorkingDirectory, "/Training/", SpecimenName, sep="")))
        dir.create((paste(WorkingDirectory, "/Training/", SpecimenName, "/images/", sep="")))
        dir.create((paste(WorkingDirectory, "/Training/", SpecimenName, "/masks/", sep="")))
        }
      }
      } else {
        for (file in list.files(path=PathtoImages, pattern='.jpg', full.names=FALSE,
                                recursive=TRUE, include.dirs=FALSE)) {
          SpecimenName <- strsplit(file, "[.]")[[1]][1]
          SpecimenName <- strsplit(SpecimenName, "/")
          SpecimenName <- SpecimenName[[1]][length(SpecimenName[[1]])]
          if (is.na(SpecimenName) == FALSE) {
          dir.create((paste(WorkingDirectory, "/Training/", SpecimenName, sep="")))
          dir.create((paste(WorkingDirectory, "/Training/", SpecimenName, "/images/", sep="")))
          dir.create((paste(WorkingDirectory, "/Training/", SpecimenName, "/masks/", sep="")))
          }
      }
    }
  }

ToPNG <- function(PathtoImages, WorkingDirectory, TIF=TRUE) {
  if (TIF==TRUE) {
    for (file in list.files(path=PathtoImages, pattern='.tif', full.names=FALSE,
                            recursive=TRUE, include.dirs=FALSE)) {
      SpecimenName <- strsplit(file, "[.]")[[1]][1]
      SpecimenName <- strsplit(SpecimenName, "/")
      SpecimenName <- SpecimenName[[1]][length(SpecimenName[[1]])]
      ImagePath <- paste(PathtoImages, "/", file, sep="")
      imagefordims <- readTIFF(ImagePath)
      img <- image_read(ImagePath)
      pixh <- dim(imagefordims)[1]
      pixw <- dim(imagefordims)[2]
      if (pixw > pixh) {
        bigside <- pixw
      } else {
        bigside <- pixh
      }
      SavePath <- paste(WorkingDirectory, "/Training/", SpecimenName, "/images/", SpecimenName, ".png", sep="")
      addx <- (bigside - pixw)/2
      addy <- (bigside - pixh)/2
      pad <- paste(as.character(addx), "X", as.character(addy), sep="")
      img <- image_border(img, "black", pad)
      image_write(img, path=SavePath, format="png")
    }
  }
  if (TIF==FALSE) {
    for (file in list.files(path=PathtoImages, pattern='.jpg', full.names=FALSE,
                                    recursive=TRUE, include.dirs=FALSE)) {
      SpecimenName <- strsplit(file, "[.]")[[1]][1]
      SpecimenName <- strsplit(SpecimenName, "/")
      SpecimenName <- SpecimenName[[1]][length(SpecimenName[[1]])]
      ImagePath <- paste(PathtoImages, "/", file, sep="")
      imagefordims <- readJPEG(ImagePath)
      img <- image_read(ImagePath)
      pixh <- dim(imagefordims)[1]
      pixw <- dim(imagefordims)[2]
      if (pixw > pixh) {
        bigside <- pixw
      } else {
        bigside <- pixh
      }
      SavePath <- paste(WorkingDirectory, "/Training/", SpecimenName, "/images/", SpecimenName, ".png", sep="")
      addx <- (bigside - pixw)/2
      addy <- (bigside - pixh)/2
      pad <- paste(as.character(addx), "X", as.character(addy), sep="")
      img <- image_border(img, "black", pad)
      image_write(img, path=SavePath, format="png")
    }
  }
}

MakeMasks <- function(PathtoImages, WorkingDirectory, PathtoTPS, ExcelPath=NULL, TIF=TRUE) {
  library(raster)
  library(magick)
  # Get image file for specimen
  if (TIF==TRUE) {
    for (file in list.files(path=PathtoImages, pattern='.tif', full.names=FALSE,
                            recursive=TRUE, include.dirs=FALSE)) {
      ImageName <- strsplit(file, "[.]")[[1]][1]
      ImageName <- strsplit(ImageName, "/")
      ImageName <- ImageName[[1]][length(ImageName[[1]])]
      #ImagePath <- paste(PathtoImages, '/', ImageName, ".tif", sep="")
      image <- readTIFF(paste(PathtoImages, "/", file, sep = ""))
      pixh <- dim(image)[1]
      pixw <- dim(image)[2]
      
      # Read in TPS file, extract xy coords of curve from specimen
      species <- strsplit(ImageName,"-")[[1]][1]
      pop <- strsplit(ImageName,"-")[[1]][2]
      individual <- as.integer(strsplit(ImageName,"-")[[1]][3])
      PopName <- paste(species, "-", pop, sep="")
      TPSpath <- paste(PathtoTPS, '/', PopName, "-digitized.tps", sep="")
      cc <- read.tps(TPSpath)
      c1 <- cc[[individual]]$curve1.points
      c1c <- rbind(c1, c1[1,])  # add start point to the end to close the polygon
      
      # the important part - make a PNG file and plot curve to it
      MaskPath <- paste(WorkingDirectory, '/Training/', ImageName, "/masks/", ImageName, ".png", sep="")
      if (pixw > pixh) {
        bigside <- pixw
      } else {
        bigside <- pixh
      }
      png(filename = MaskPath, width = pixw, height = pixh, bg = "black")
      par(mai = c(0,0,0,0), xaxs = "i", yaxs = "i")
      plot(c1c, typ="n", xlim=c(0, pixw), ylim=c(0, pixh))
      polygon(c1c, col = "white")
      dev.off()
      img <- image_read(MaskPath)
      addx <- (bigside - pixw)/2
      addy <- (bigside - pixh)/2
      pad <- paste(as.character(addx), "X", as.character(addy), sep="")
      img <- image_border(img, "black", pad)
      image_write(img, path=MaskPath, format="png")
    }
  } else {
    for (file in list.files(path=PathtoImages, pattern='.jpg', full.names=FALSE,
                            recursive=TRUE, include.dirs=FALSE)) {
      ImageName <- strsplit(file, "[.]")[[1]][1]
      ImageName <- strsplit(ImageName, "/")
      ImageName <- ImageName[[1]][length(ImageName[[1]])]
      #ImagePath <- paste(PathtoImages, '/', ImageName, ".jpg", sep="")
      image <- readJPEG(file)
      pixh <- dim(image)[1]
      pixw <- dim(image)[2]
      
      # Read in TPS file, extract xy coords of curve from specimen
      species <- strsplit(ImageName,"-")[[1]][1]
      pop <- strsplit(ImageName,"-")[[1]][2]
      individual <- as.integer(strsplit(ImageName,"-")[[1]][3])
      PopName <- paste(species, "-", pop, sep="")
      TPSpath <- paste(PathtoTPS, '/', PopName, "-digitized.tps", sep="")
      cc <- read.tps(TPSpath)
      c1 <- cc[[individual]]$curve1.points
      c1c <- rbind(c1, c1[1,])  # add start point to the end to close the polygon
      
      # the important part - make a PNG file and plot curve to it
      MaskPath <- paste(WorkingDirectory, '/Training/', ImageName, "/masks/", ImageName, ".png", sep="")
      if (pixw > pixh) {
        bigside <- pixw
      } else {
        bigside <- pixh
      }
      png(filename = MaskPath, width = pixw, height = pixh, bg = "black")
      par(mai = c(0,0,0,0), xaxs = "i", yaxs = "i")
      plot(c1c, typ="n", xlim=c(0, pixw), ylim=c(0, pixh))
      polygon(c1c, col = "white")
      dev.off()
      img <- image_read(MaskPath)
      addx <- (bigside - pixw)/2
      addy <- (bigside - pixh)/2
      pad <- paste(as.character(addx), "X", as.character(addy), sep="")
      img <- image_border(img, "black", pad)
      image_write(img, path=MaskPath, format="png")
    }
  }
  #Optionally move folders of specimens we don't want to include (column names not accurate yet)
  if(!is.null(ExcelPath)) {
    require(filesstrings)
    require(readxl)
    ExcludeSpecimens <- read_excel(ExcelPath)
    for (row in 1:nrow(ExcludeSpecimens)) {
      if (ExcludeSpecimens$Include[row]=="N") {
        FolderName <- ExcludeSpecimens$Name[row]
        file.move(paste(WorkingDirectory, '/Training/', FolderName, sep=""), paste(WorkingDirectory, '/Excluded/', FolderName, sep=""))
      }
    }
  }
}
  
ResizeTestImages <- function(PathtoTestImages, WorkingDirectory, ImageSize, TIF=TRUE) {
  require(magick)
  if (TIF==TRUE) {
    for (file in list.files(path=PathtoTestImages, pattern='.tif', full.names=FALSE,
                            recursive=TRUE, include.dirs=FALSE)) {
      SpecimenName <- strsplit(file, "[.]")[[1]][1]
      ImagePath <- paste(PathtoTestImages, "/", file, sep="")
      imagefordims <- readTIFF(ImagePath)
      img <- image_read(ImagePath)
      pixh <- dim(imagefordims)[1]
      pixw <- dim(imagefordims)[2]
      if (pixw > pixh) {
        bigside <- pixw
      } else {
        bigside <- pixh
      }
      addx <- (bigside - pixw)/2
      addy <- (bigside - pixh)/2
      pad <- paste(as.character(addx), "X", as.character(addy), sep="")
      img <- image_border(img, "black", pad)
      dims <- paste(ImageSize, "X", ImageSize, sep="")
      img <- image_resize(img, dims)
      SavePath <- paste(WorkingDirectory, "/Test/", SpecimenName, ".png", sep="")
      if (file.exists(paste(WorkingDirectory, "/Test", sep=""))) {
        setwd((paste(WorkingDirectory, "/Test", sep="")))
        image_write(img, path=SavePath, format="png")
      } else {
        dir.create((paste(WorkingDirectory, "/Test", sep="")))
        setwd((paste(WorkingDirectory, "/Test", sep="")))
        image_write(img, path=SavePath, format="png")
      }
      }
    }
  if (TIF==FALSE) {
    for (file in list.files(path=PathtoTestImages, pattern='.jpg', full.names=FALSE,
                            recursive=TRUE, include.dirs=FALSE)) {
      SpecimenName <- strsplit(file, "[.]")[[1]][1]
      ImagePath <- paste(PathtoTestImages, "/", file, sep="")
      img <- image_read(ImagePath)
      imagefordims <- readJPEG(ImagePath)
      pixh <- dim(imagefordims)[1]
      pixw <- dim(imagefordims)[2]
      if (pixw > pixh) {
        bigside <- pixw
      } else {
        bigside <- pixh
      }
      addx <- (bigside - pixw)/2
      addy <- (bigside - pixh)/2
      pad <- paste(as.character(addx), "X", as.character(addy), sep="")
      img <- image_border(img, "black", pad)
      dims <- paste(ImageSize, "X", ImageSize, sep="")
      img <- image_resize(img, dims)
      SavePath <- paste(WorkingDirectory, "/Test/", SpecimenName, ".png", sep="")
      if (file.exists(paste(WorkingDirectory, "/Test", sep=""))) {
        image_write(img, path=SavePath, format="png")
      } else {
        dir.create((paste(WorkingDirectory, "/Test", sep="")))
        image_write(img, path=SavePath, format="png")
      }
    }
  }
}

PrepareData <- function(PathtoImages, WorkingDirectory, PathtoTPS, TIF=TRUE) {
  MakeFolders(PathtoImages, WorkingDirectory, TIF)
  ToPNG(PathtoImages, WorkingDirectory, TIF)
  MakeMasks(PathtoImages, WorkingDirectory, PathtoTPS, TIF)
}

WriteMultipletoTPS <- function(FolderofContourFiles, WorkingDirectory, PopName, Scale, TIF=TRUE) {
  PopFile <- paste(PopName, ".tps", sep="")
  library(readr)
  for (file in list.files(path=FolderofContourFiles, pattern='.txt', full.names=FALSE,
                          recursive=TRUE, include.dirs=FALSE)) {
    if (TIF==TRUE) {
    SpecimenName <- paste(strsplit(ContourFile, "_")[[1]][3], "_", strsplit(strsplit(ContourFile, "_")[[1]][4], "[.]")[[1]][1], sep="")
    SpecimenID <- strsplit(ContourFile, "-")[[1]][3]
    } else {
      SpecimenName <- paste(strsplit(ContourFile, "_")[[1]][3], "_", strsplit(strsplit(ContourFile, "_")[[1]][4], "[.]")[[1]][1], sep="")
      SpecimenID <- strsplit(strsplit(ContourFile, "-")[[1]][3], "[.]")[[1]][1]
    }
    LM <- "LM=0"
    curves <- "CURVES=1"
    Contour <- read_file(ContourFile)
    Contour <- gsub("\n\n", "\n", Contour)
    Contour <- gsub("\n ", "\n", Contour)
    ContourTable <- read.delim(ContourFile)
    points <- sprintf("POINTS=%s", nrow(ContourTable))
    if (TIF==TRUE) {
      image <- paste("IMAGE=", SpecimenName, ".tif", sep="")
    } else {
      image <- paste("IMAGE=", SpecimenName, ".jpg", sep="")
    }
    ID <- sprintf("ID=%s", SpecimenID)
    scale <- sprintf("SCALE=%s", Scale)
    write(c(LM, curves, points, image, ID, scale, Contour), PopFile, ncolumns = 1, append=TRUE)
  }
}

