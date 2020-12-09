#09/11/20
# Functions to replace transfermodel_utils.py in R

library(tiff)
library(jpeg)
library(raster)
library(magick)
library(readr)
library(filesstrings)
library(readxl)

MakeFolders <- function(PathtoImages, WorkingDirectory, TIF=TRUE) {
  #' MakeFolders: Help
  #' Makes a folder in the working directory with image and mask subfolders for every training image
  #' @param PathtoImages the full path name to where the TIF or JPG images for model training are stored, optional
  #' @param WorkingDirectory the folder path where prepped images will be stored for training, optional
  #' @param TIF TRUE if original images were TIFs, FALSE if JPGs, optional

  if (file.exists(paste(WorkingDirectory, "/Training", sep=""))) {
  } else {
    dir.create((paste(WorkingDirectory, "/Training", sep="")))
  }
  if (TIF==TRUE) {
    for (file in list.files(path=PathtoImages, pattern='.tif', full.names=FALSE,
                            recursive=TRUE, include.dirs=FALSE)) {
      cat("=== ", file, " ===\n\n")
      SpecimenName <- strsplit(file, "[.]")[[1]][1]
      SpecimenName <- strsplit(SpecimenName, "/")
      SpecimenName <- SpecimenName[[1]][length(SpecimenName[[1]])]
      if (is.null(SpecimenName) == FALSE) {
        dir.create((paste(WorkingDirectory, "/Training/", SpecimenName, sep="")))
        dir.create((paste(WorkingDirectory, "/Training/", SpecimenName, "/images/", sep="")))
        dir.create((paste(WorkingDirectory, "/Training/", SpecimenName, "/masks/", sep="")))
        }
      }
      } else {
        for (file in list.files(path=PathtoImages, pattern='.jpg', full.names=FALSE,
                                recursive=TRUE, include.dirs=FALSE)) {
          cat("=== ", file, " ===\n\n")
          SpecimenName <- strsplit(file, "[.]")[[1]][1]
          SpecimenName <- strsplit(SpecimenName, "/")
          SpecimenName <- SpecimenName[[1]][length(SpecimenName[[1]])]
          if (is.null(SpecimenName) == FALSE) {
          dir.create((paste(WorkingDirectory, "/Training/", SpecimenName, sep="")))
          dir.create((paste(WorkingDirectory, "/Training/", SpecimenName, "/images/", sep="")))
          dir.create((paste(WorkingDirectory, "/Training/", SpecimenName, "/masks/", sep="")))
          }
      }
    }
  }

ToPNG <- function(PathtoImages, WorkingDirectory, TIF=TRUE, Crop=FALSE) {
  #' ToPNG: Help
  #' Use after MakeFolders() to convert TIF or JPG images in PathtoImages to PNG images in the working directory
  #' So long as MakeFolders() has already been used, the resized PNG images will be saved to their namesake folders in the working directory
  #' @param PathtoImages the full path name to where the TIF or JPG images for model training are stored
  #' @param WorkingDirectory the folder path where prepped images will be stored for training
  #' @param TIF TRUE if original images were TIFs, FALSE if JPGs, default is TRUE
  #' @param Crop TRUE if images should be resized by cropping and FALSE if images should be resized by padding, default is FALSE
  
  if (TIF==TRUE) {
    for (file in list.files(path=PathtoImages, pattern='.tif', full.names=FALSE,
                            recursive=TRUE, include.dirs=FALSE)) {
      cat("=== ", file, " ===\n\n")
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
      if (Crop==FALSE) {
        addx <- (bigside - pixw)/2
        addy <- (bigside - pixh)/2
        pad <- paste(as.character(addx), "X", as.character(addy), sep="")
        img <- image_border(img, "black", pad)
      } else {
        crop <- paste(as.character(pixw-addx), "X", as.character(pixh-addy), sep="")
        img <- image_crop(img, crop)
      }
      image_write(img, path=SavePath, format="png")
    }
  }
  if (TIF==FALSE) {
    for (file in list.files(path=PathtoImages, pattern='.jpg', full.names=FALSE,
                                    recursive=TRUE, include.dirs=FALSE)) {
      cat("=== ", file, " ===\n\n")
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
      if (Crop==FALSE) {
        addx <- (bigside - pixw)/2
        addy <- (bigside - pixh)/2
        pad <- paste(as.character(addx), "X", as.character(addy), sep="")
        img <- image_border(img, "black", pad)
      } else {
        crop <- paste(as.character(pixw-addx), "X", as.character(pixh-addy), sep="")
        img <- image_crop(img, crop)
      }
      image_write(img, path=SavePath, format="png")
    }
  }
}

#MakeMasks() inefficiently reads a TPS file for every specimen, rather than once for each population.  A better strategy would be to read in all 
#the TPS file in the TPS path and use cat.tps() or cat.tpsList() to combine them into one big TPS object.  Then use grab.tps( , get = “IMAGE”) to 
#pull out a vector of image names which you can then use to find the index of the needed specimen to pull out its x,y coordinates
MakeMasks <- function(PathtoImages, WorkingDirectory, PathtoTPS, PathtoTPSoo, ExcelPath=NULL, TIF=TRUE, Crop=FALSE) {
  #' MakeMasks: Help
  #' Use after MakeFolders(), creates a training mask for every image and puts it in its namesake folder in the working directory
  #' Notes:
  #' If the TPS files' IDs are in the form GENUS_SPECIES-1-01, this function will replace them with integers (01, 02, etc)
  #' @param PathtoTPS the folder path to any number of TPS files
  #' @param PathtoImages the full path name to where the TIF or JPG images for model training are stored
  #' @param WorkingDirectory the folder path where prepped images will be stored for training
  #' @param ExcelPath is the path to an excel sheet with a list of which specimens to include and exclude, optional
  #' @param TIF TRUE if original images were TIFs, FALSE if JPGs, default is TRUE
  #' @param Crop TRUE if images should be resized by cropping and FALSE if images should be resized by padding, default is FALSE
  
  source(PathtoTPSoo)
  
  TPSFileList <- list.files(path=PathtoTPS, pattern='.tps', full.names=FALSE, recursive=TRUE, include.dirs=FALSE)
  AllTPS <- cat.tpsList(TPSFileList)
  
  # Get image file for specimen
  if (TIF==TRUE) {
    for (file in list.files(path=PathtoImages, pattern='.tif', full.names=FALSE,
                            recursive=TRUE, include.dirs=FALSE)) {
      cat("=== ", file, " ===\n\n")
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
      cc <- grab.tps(AllTPS, get="IMAGE")
      #TPSpath <- paste(PathtoTPS, '/', PopName, "-digitized.tps", sep="")
      #print(TPSpath)
      #cc <- read.tps(TPSpath)
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
      if (Crop==FALSE) {
        pad <- paste(as.character(addx), "X", as.character(addy), sep="")
        img <- image_border(img, "black", pad)
      } else {
        crop <- paste(as.character(pixw-addx), "X", as.character(pixh-addy), sep="")
        img <- image_crop(img, crop)
      }
      image_write(img, path=MaskPath, format="png")
    }
  } else {
    for (file in list.files(path=PathtoImages, pattern='.jpg', full.names=FALSE,
                            recursive=TRUE, include.dirs=FALSE)) {
      cat("=== ", file, " ===\n\n")
      ImageName <- strsplit(file, "[.]")[[1]][1]
      ImageName <- strsplit(ImageName, "/")
      ImageName <- ImageName[[1]][length(ImageName[[1]])]
      #ImagePath <- paste(PathtoImages, '/', ImageName, ".jpg", sep="")
      image <- readJPEG(paste(PathtoImages, '/', file, sep=""))
      pixh <- dim(image)[1]
      pixw <- dim(image)[2]
      
      # Read in TPS file, extract xy coords of curve from specimen
      species <- strsplit(ImageName,"-")[[1]][1]
      pop <- strsplit(ImageName,"-")[[1]][2]
      individual <- as.integer(strsplit(ImageName,"-")[[1]][3])
      #PopName <- paste(species, "-", pop, sep="")
      #TPSpath <- paste(PathtoTPS, '/', PopName, "-digitized.tps", sep="")
      #cc <- read.tps(TPSpath)
      cc <- grab.tps(AllTPS, get="IMAGE")
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
      if (Crop==FALSE) {
        pad <- paste(as.character(addx), "X", as.character(addy), sep="")
        img <- image_border(img, "black", pad)
      } else {
        crop <- paste(as.character(pixw-addx), "X", as.character(pixh-addy), sep="")
        img <- image_crop(img, crop)
      }
      image_write(img, path=MaskPath, format="png")
    }
  }
  #Optionally move folders of specimens we don't want to include (column names not accurate yet)
  #if(!is.null(ExcelPath)) {
    #ExcludeSpecimens <- read_excel(ExcelPath)
    #for (row in 1:nrow(ExcludeSpecimens)) {
      #if (ExcludeSpecimens$Include[row]=="N") {
        #FolderName <- ExcludeSpecimens$Name[row]
        #file.move(paste(WorkingDirectory, '/Training/', FolderName, sep=""), paste(WorkingDirectory, '/Excluded/', FolderName, sep=""))
      #}
    #}
  #}
}
  
ResizeTestImages <- function(PathtoTestImages, WorkingDirectory, ImageSize, TIF=TRUE, Crop=FALSE) {
  #' ResizeTestImages: Help
  #' This function pads or crops images to the size on which the model was trained
  #' @param PathtoTestImages the full path name to where the TIF or JPG images for model training are stored
  #' @param WorkingDirectory the folder path where prepped images will be stored for training
  #' @param ImageSize should be one integer: the width or height to which the test images should be resized (must be square)
  #' @param TIF TRUE if original images were TIFs, FALSE if JPGs, default is TRUE
  #' @param Crop TRUE if images should be resized by cropping and FALSE if images should be resized by padding, default is FALSE
  
  if (TIF==TRUE) {
    for (file in list.files(path=PathtoTestImages, pattern='.tif', full.names=FALSE,
                            recursive=TRUE, include.dirs=FALSE)) {
      cat("=== ", file, " ===\n\n")
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
      if (Crop==FALSE) {
        addx <- (bigside - pixw)/2
        addy <- (bigside - pixh)/2
        pad <- paste(as.character(addx), "X", as.character(addy), sep="")
        img <- image_border(img, "black", pad)
      } else {
        crop <- paste(as.character(pixw-addx), "X", as.character(pixh-addy), sep="")
        img <- image_crop(img, crop)
      }
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
  if (TIF==FALSE) {
    for (file in list.files(path=PathtoTestImages, pattern='.jpg', full.names=FALSE,
                            recursive=TRUE, include.dirs=FALSE)) {
      cat("=== ", file, " ===\n\n")
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
      if (Crop==FALSE) {
        addx <- (bigside - pixw)/2
        addy <- (bigside - pixh)/2
        pad <- paste(as.character(addx), "X", as.character(addy), sep="")
        img <- image_border(img, "black", pad)
      } else {
        crop <- paste(as.character(pixw-addx), "X", as.character(pixh-addy), sep="")
        img <- image_crop(img, crop)
      }
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

#I suggest setting verbose = FALSE for all calls to read.tps in your script which will suppress this printing.
PrepareData <- function(ControlFile=NULL, PathtoImages=NULL, PathtoTestImages=NULL, WorkingDirectory=NULL, PathtoTPS=NULL, PathtoTPSoo=NULL, ExcelPath=NULL, ImageSize=NULL, TIF=NULL, Crop=NULL) {
  #' PrepareData: Help
  #' This function runs MakeFolders(), TIFtoPNG(), MakeMasks(), and ResizeTestImages()
  #' Note: If there is an error, each component function can also be called individually
  #' @param ControlFile is the full path to the txt file with all arguments, optional if other parameters input straight into R
  #' @param PathtoImages the full path name to where the TIF or JPG images for model training are stored, optional if ControlFile is input
  #' @param WorkingDirectory the folder path where prepped images will be stored for training, optional if ControlFile is input
  #' @param PathtoTestImages the full path name to the folder containing the images on which the model will be tested, optional if ControlFile is input
  #' @param ImageSize should be one integer: the width or height to which the test images should be resized (must be square), optional if ControlFile is input
  #' @param ExcelPath is the path to an excel sheet with a list of which specimens to include and exclude, optional if ControlFile is input
  #' @param PathtoTPS the folder path to any number of TPS files, optional if ControlFile is input
  #' @param PathtoTPSoo is the path to the tps-oo.R file
  #' @param TIF TRUE if original images were TIFs, FALSE if JPGs, default is TRUE, optional if ControlFile is input
  #' @param Crop TRUE if images should be resized by cropping and FALSE if images should be resized by padding, default is FALSE, optional if ControlFile is input
  
  if (!is.null(ControlFile)) {
    PathtoImages <- strsplit(ArgList[4], " = ")[[1]][2]
    WorkingDirectory <- strsplit(ArgList[5], " = ")[[1]][2]
    PathtoTPS <- strsplit(ArgList[8], " = ")[[1]][2]
    TIF <- as.logical(strsplit(ArgList[6], " = ")[[1]][2])
    ImageSize <- as.integer(strsplit(ArgList[9], " = ")[[1]][2])
    PathtoTestImages <- strsplit(ArgList[10], " = ")[[1]][2]
    Crop <- as.logical(strsplit(ArgList[7], " = ")[[1]][2])
    PathtoTPSoo <- strsplit(ArgList[11], " = ")[[1]][2]
    ExcelPath <- strsplit(ArgList[12], " = ")[[1]][2]
  }
  
  cat("Making Folders...\n")
  MakeFolders(PathtoImages, WorkingDirectory, TIF)
  cat("Converting to PNG...\n")
  ToPNG(PathtoImages, WorkingDirectory, TIF, Crop)
  cat("Making Masks...\n")
  MakeMasks(PathtoImages, WorkingDirectory, PathtoTPS, PathtoTPSoo, ExcelPath, TIF, Crop)
  cat("Resizing Test Images...\n")
  ResizeTestImages(PathtoTestImages, WorkingDirectory, ImageSize, TIF, Crop)
}

WriteMultipletoTPS <- function(ControlFile=NULL, FolderofContourFiles=NULL, WorkingDirectory=NULL, PopName=NULL, Scale=NULL, TIF=TRUE) {
  #i Write Multiple to TPS: Help
  #' After the model has written contour files for each specimen in a population,
  #' use this function to combine those contour files into one
  #' @param ControlFile is the full path to the txt file with all arguments, optional if other parameters entered straight into R
  #' @param FolderofContourFiles is the full path to the files output by make_predictions.py, optional if ControlFile is input
  #' @param WorkingDirectory is the full path to where the prepared images and masks are, optional if ControlFile is input
  #' @param PopName should be in the format BUN_SHUB-1, optional if ControlFile is input
  #' @param Scale is the scale that should be written in the TPS file, optional if ControlFile is input
  #' @param TIF TRUE if original images were TIFs, FALSE if JPGs, default is TRUE, optional if ControlFile is input
  
  if (!is.null(ControlFile)) {
    FolderofContourFiles <- strsplit(ArgList[13], " = ")[[1]][2]
    WorkingDirectory <- strsplit(ArgList[5], " = ")[[1]][2]
    PopName <- strsplit(ArgList[14], " = ")[[1]][2]
    Scale <- as.integer(strsplit(ArgList[15], " = ")[[1]][2])
    TIF <- as.logical(strsplit(ArgList[6], " = ")[[1]][2])
  }
  
  PopFile <- paste(PopName, ".tps", sep="")
  for (file in list.files(path=FolderofContourFiles, pattern='.txt', full.names=FALSE,
                          recursive=TRUE, include.dirs=FALSE)) {
    cat("=== ", file, " ===\n\n")
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

ReadControl <- function(ControlFile){
	
	Args <- readLines(ControlFile)
    argList <- list()
    
    for(i in 1:length(Args)){
    	  v <- Args[i]  # get ith line
    	  # skip empty lines, or comments -- those starting with # 
    	  if(v != "" && substring(v, 1, 1) != "#" ){
    	    v <- gsub(" ", "", v)
    	  	ss <- strsplit(v, "=")[[1]]
    	  	argList[ss[1]] <- ss[[2]]
    	  }    	
    }
    
   # convert some arguments from strings to numbers/logical 
   areNumbers <- c("ImageSize", "Scale", "PATIENCE", "VAL_SPLIT", "BATCH_SIZE", "EPOCHS", "FIN_IMG_WIDTH") 
   areBoolean <- c("TIF", "Crop", "SET_SEED")
   for(jj in areNumbers)	argList[[jj]] <- as.numeric(argList[[jj]])
   for(jj in areBoolean)	argList[[jj]] <- as.logical(argList[[jj]])
   
    
   return(argList)

}
