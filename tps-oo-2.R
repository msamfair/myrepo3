##########################################################################
##
##   Functions useful for reading & processing TPS files
##	   Gene Hunt, started 17 October 2004
##
##########################################################################




####	Main Functions	####
#	--------------
#	write.tps	writes (blank) tps file from vector of images & other info
#   export.tps  exports tps class object (with its data) to a text file, readable by TPSDig
#	read.tps	reads and processes tps file
# 	plot.tps    shows LMs, outlines from tps specimen
# 	tps.to.efa  output series of tps specimens to file for EFA
# 	read.efa    read in modified output from EFA
# 	eq.curves   replace all curve points w/ equally spaced points
# 	cat.tps     catenate arrays of tps specimens
# 	curve.area1 areas of 1 curve in multi-specimen tps array
# 	grab.tps    grab an element from each config in an array of tps configs
#   sub.tps		subset a tps object
#	MD.lm       take MD info from COMMENT and apply to LM data
#	LMdist		computes linear distances btwn LM pairs
#	LMmat		makes n x p matrix of LMs (x1,y1,x2,y2...,xp,yp) from tps object

#
#	Accessory functions (called by main functions)	
#	-------------------
# plot.tps		formerly show.tps
# summary.tps	summarizes tps object
# edist 		euclidean distance btwn 2 points
# init.tps  	intialize empty specimen info (read.tps)
# plot1.tps   plots single specimen (plot.tps)
# eq.spaced	  interpolates equally-spaced coordinates
# get.area	  computes area of a closed curve
# tps.area    computes area of a curve in a tps specimen
# xy.to.cplx  convert x,y to complex notation
# cplx.to.xy  convert complex to x,y
# rotate.tps  rotate one configuration by theta
# rotate.xy2  rotate xy2 matrix by theta
# angle.hv    determine angle from vector to horiz or vertical
# reflect.tps reflect config over X or Y axis

## libraries needed
 library(circular)
 library(Momocs)
 library(magick)


####	Main Functions	####

write.tps<- function (im, sc=rep(1.0, length(im)), ids=NULL, vars=NULL, ff="kk-out.tps", nn=length(im))
# write a tps file from array of images (im), scale factors (sc) and other info
# translates ID= image name w/o extension by default
# vars must be of mode character
# NOTE: does not work for existing objects of class 'tps;' use export.tps() 
# Main use is to set up a new, blank tps file before data collection
{
 ffo<- file(ff, "w")
 for (i in 1:nn)
 {
    writeLines("LM=0", ffo)
    writeLines(paste("IMAGE=",im[i], sep=""), ffo)
    if (is.null(ids))
    	  id<- unlist(strsplit(im[i], "\\."))[1]
    else  id<- ids[i]
    writeLines(paste("ID=", id, sep=""), ffo)
    # write VARIABLES if any
    if (!is.null(vars))
    {
	if (is.null(dim(vars)))	
		vs<- vars[i]
	else	
	{
	 vs<- vars[i,1]
	 for (vv in 2:ncol(vars))
		vs<- paste(vs,vars[i,vv], sep=",")
	}
	writeLines( paste("VARIABLES=", vs, sep=""), ffo)     
    }
    writeLines(paste("SCALE=",sc[i], sep=""), ffo)
 }
 close(ffo)
}

export.tps<- function(ws, ff="kk-export.tps")
# takes an exisitng tps object and writes it to a file so that it can be opened by TpsDig
{
 if(class(ws) != "tps")	stop("ws must be of class 'tps'")
 ffo<- file(ff, "w")
 
 nn<- length(ws)
 
 for (i in 1:nn)
 {
    # initialize, handle LMs
    w<- ws[[i]]
    writeLines(paste0("LM=", w$LM), ffo)
    if(w$LM > 0)	write(t(w$LM.points), ffo, ncolumns=2)
    
    # handle IMAGE, SCALE, COMMENT, ID
    writeLines(paste0("IMAGE=", w$IMAGE), ffo)
    writeLines(paste0("SCALE=", w$SCALE), ffo)    # SCALE must be present!
    if(!is.null(w$COMMENT))	writeLines(paste0("COMMENT=", w$COMMENT), ffo)
	if(!is.null(w$ID))		writeLines(paste0("ID=", w$ID), ffo)    
	
	# handle curves and outlines
	if(!is.null(w$CURVES))	{
		nc<- w$CURVES
		writeLines(paste0("CURVES=", nc), ffo)
		for(j in 1:nc){
			xy<- t(w[[paste0("curve", j, ".points")]])
			np<- ncol(xy)
			writeLines(paste0("POINTS=", np), ffo)
			write(xy, file=ffo, ncolumns=2)
		}
	}

	if(!is.null(w$OUTLINES))	{
		no<- w$OUTLINES
		writeLines(paste0("OUTLINES=", no), ffo)
		for(j in 1:no){
			xy<- t(w[[paste0("outline", j, ".points")]])
			#print(xy)
			#print(is.data.frame(xy))
			np<- ncol(xy)
			writeLines(paste0("POINTS=", np), ffo)
			write(xy, file=ffo, ncolumns=2)
		}
	}

}
 close(ffo)	
	
}


read.tps<- function ( ff=file.choose() , verbose = TRUE)
# read in tps file
#	
{
 # open file for reading
 ffo<- file(ff, "r")
 
 ok<- TRUE
 ii<-0
 ws<-list()
 while (ok)
 {
    x<- readLines(ffo, n=1)
    if (length(x)==0)	# reached end of file
    { 
       ok<- FALSE	# end while loop
       # add last specimen to output list
       ws[[ii]] <- w

    }
    else {
    sx<- unlist(strsplit(x,"="))
    if (sx[1]=="LM" | sx[1]=="lm")	# KEY = LM
     {
		# add previous specimen to ws (only if past specimen #1)
		if (ii>0)  ws[[ii]] <- w
	
		# increment counter, initialize new specimen, change LM, reset nc
		ii<- ii+1
		w<- init.tps()
		if(verbose) cat ("--- Specimen #", ii, "---\n")
		w$LM<- as.numeric(sx[2])
		nc<-0
	
		# if LM>0, read in matrix of xy LM coordinates
		if (w$LM>0)
		  {
	  		p<- as.numeric(sx[2])
	  		xys<- scan(file=ffo, what=double(0), n=2*p)			# scan as numeric
	  		xy<- matrix(xys, p,2,byrow=TRUE)
	  		w$LM.points<- xy
		  }
     }
    
    else if (sx[1]=="CURVES" | sx[1]=="curves")  # KEY = CURVES
     {
		w$CURVES<- as.numeric(sx[2])
		flag<- "c"  # for POINTS that follow 
     }
     
    else if (sx[1]=="OUTLINES" | sx[1]=="outlines")  # KEY = OUTLINES
     {
		w$OUTLINES<- as.numeric(sx[2])
		flag<- "o"  # for POINTS that follow  
     }
    
    else if (sx[1]=="POINTS" | sx[1]=="points")  # KEY = POINTS
     {
       nc<- nc+1 
       p<- as.numeric(sx[2])
       xys<- scan(file=ffo, what=double(0), n=2*p)	   # scan as numeric
       xy<- matrix(xys, p,2,byrow=TRUE)
       if (flag=="c") 
       		cname<- paste("curve",nc,".points", sep="")
       else 
       		cname<- paste("outline",nc,".points", sep="")
       w[[cname]]<- xy
     }
     
    else if (sx[1] %in% c("SCALE", "scale"))	# KEY = SCALE
    	w$SCALE<- as.numeric(sx[2])
    
    # these keywords are recorded as is
    else if (sx[1] %in% c("ID","id","IMAGE","image","COMMENT","comment","VARIABLES","variables"))
      {
      	ch<- sx[2]
      	while (substr(ch,nchar(ch),nchar(ch)) == " ")  # strip out trailing spaces, if present
      		ch<- substr(ch,1,nchar(ch)-1)
      	w[[sx[1]]]<- ch
      }
    
    else stop(paste("Unrecognized keyword: ",sx[1]) )
    }
 }
 
 # print summary stats to screen
 close(ffo)
 class(ws)<- "tps"
 if(verbose) cat ("\n-----------------------\n")
 if(verbose) cat("File read:", ff, "\n")
 if(verbose) summary(ws)
 return (ws)
}


jpegTPS <- function(tps = file.choose(), sub = TRUE, subname = "JPG"){
# makes JPEG versions of all images in a TPS file, and writes new TPS file
# with new file names (all other data unchanged)
  
  wd <- getwd()
  if(sub){
    subd <- dir.exists(subname)
    if(!subd)   dir.create(subname)
    pth <- file.path(wd, subname)
  }

  
  x <- read.tps(tps)
  xj <- x  # for JPEG version of tps file
  flist <- grab.tps(x, get = "IMAGE")
  
  N <- length(flist)
  cat("\n\nConverting to JPEG...\n")
  for(i in 1:N){
    cat("   ", flist[[i]], "\n")
    im <- image_read(flist[[i]])
    fs <- strsplit(flist[[i]], ".", fixed=TRUE)
    newname <- paste0(fs[[1]][1], ".jpg")
    imc <- image_convert(im, format = "jpeg")
    image_write(imc, file.path(pth, newname))
    
    # alter TPS object
    xj[[i]]$IMAGE <- newname
  }
  
  im1 <- image_read(flist[[1]])
  im1c<- image_convert(im1, format = "jpeg")
  image_write(im1c, path = "kk.jpg")
  
  ts <- strsplit(tps, ".", fixed=TRUE)
  newtps <- paste0(ts[[1]][1], "-JPG.tps")
  export.tps(xj, ff = newtps)
  
  return()
}



plot.tps<- function (ws, pd=0.1, ...)
## shows LMs and CURVES from a series (or just one) tps individuals
{
  nn<- length (ws)  # number of specimens
  
  # first, go through all and get xr and yr
  xy<- NULL
  for (i in 1:nn)
    {
      xy<- rbind(xy, ws[[i]]$LM.points)
      if (!is.null(ws[[i]]$CURVES))
        {
          for (j in 1:ws[[i]]$CURVES)
            {
              cn<- paste("curve",j,".points", sep="")
              xy<- rbind(xy, ws[[i]][[cn]])
            }
         }
      if (!is.null(ws[[i]]$OUTLINES))
        {
          for (j in 1:ws[[i]]$OUTLINES)
            {
              cn<- paste("outline",j,".points", sep="")
              xy<- rbind(xy, ws[[i]][[cn]])
            }
         }

    }
   xr<- range (xy[,1], na.rm=TRUE)
   xs<- (xr[2]-xr[1])*pd
   xr<- xr + c(-xs, xs)
   yr<- range (xy[,2], na.rm=TRUE)
   ys<- (yr[2]-yr[1])*pd
   yr<- yr + c(-ys, ys)
      
   # plot specimens
   plot1.tps (ws[[1]], add=FALSE, xr, yr, ...)
   if (nn>1)
     {
       for (i in 2:nn)
       plot1.tps (ws[[i]], add=TRUE,...)
     }
}


summary.tps<- function(x, rowlabel="IMAGE")
# print summary of tps object
{
   cat ("tps object:\n")
   nspec<- length(x)
   cat (" ", nspec, "specimens\n")

   
   sumMat<- array(dim=c(nspec, 3))
   colnames(sumMat)<- c('LM', "OUTLINES", "CURVES")
   rn<- grab.tps(x, rowlabel)
   rownames(sumMat)<- rn
   for (i in 1:nspec)
   {
   	nlm<- x[[i]]$LM
   	nout<- x[[i]]$OUTLINES; 	if(is.null(nout)) nout<- 0
   	ncur<- x[[i]]$CURVES; 	if(is.null(ncur)) ncur<- 0
   	sumMat[i,]<- c(nlm, nout, ncur)
   }
   #cat ("  # landmarks, outlines and curves by specimen:\n ")
   print(sumMat)
}

tps.to.efa <- function (ws, ff=file.choose(), kind=c("curve", "outline"), nc=1, lab="TPS data for EFA")
# outputs seris of tps specimens to EFA file
#     nc is which CURVE/OUTLINE to output
#     lab is the label (1st line) of the EFA file
{
 ffo<- file(ff, "w")        # open output file
 nn<- length (ws)           # must be >1 specimens in ws
 writeLines (lab, ffo)      # write label line
 cn<- paste(kind, nc, ".points", sep="")
 for (i in 1:nn)
  {
    if (kind=="curve" & !is.null(ws[[i]]$CURVES) )
      {
        writeLines (as.character(nrow(ws[[i]][[cn]])), ffo)
        write.table(ws[[i]][[cn]]*ws[[i]]$SCALE, file=ffo, append=TRUE, quote=FALSE, row.names=FALSE, col.names=FALSE)
      }
    if (kind=="outline" & !is.null(ws[[i]]$OUTLINES) )
      {
        writeLines (as.character(nrow(ws[[i]][[cn]])), ffo)
        write.table(ws[[i]][[cn]]*ws[[i]]$SCALE, file=ffo, append=TRUE, quote=FALSE, row.names=FALSE, col.names=FALSE)
      }
  }
 close (ffo)
}


parse.efa<- function(ff=file.choose(), nharm, skip1=16, skip=15, outn="parsed.txt")
# modify efa output file to delete everything but the harmonics
{
  out<- file(outn, "w")
  kk<- readLines(ff, n=skip1)
  print (kk)
  cat ("----")
  ok<-TRUE
  #while (ok)
   {
     gd<- readLines(ff, n=nharm)
     print (gd);   cat ("----")
     #writeLines(gd, con=out)
     kk<- readLines(ff, n=skip)
     print (kk);   cat ("----")
     if (is.null(kk)) kk<- FALSE
   }
  close(out)	
}

read.efa<- function (ff=file.choose(), nharm)
# read in modified output from EFA program
# EFA file is edited to delete everything but the coefficient lines
# nharm is optional, useful if want to import only a subset of harmonics [not implemented]
{
 x<- read.table(ff, header=FALSE)
 nharm<- max(x[,1])   # number of harmonics
 nr<- nrow(x)
 ni<- nr/nharm  # number of individuals

 xc<- x[,2:5]
 om<- array(dim=c(ni, 4*nharm))
 for (i in 1:ni)
  {
     cf<-numeric()
     for (k in 1:nharm)
        cf<- append(cf, xc[(i-1)*nharm+k,])
     om[i,]<- unlist(cf, use.names=FALSE)
  }

 # make colnames
 tt<- letters[1:4]
 ii<- rep(1:nharm, times=4)
 colnames(om)<- paste(tt,sort(ii),sep="")
 
 # print summary
 cat ("\n\n---------------------------------\n")
 cat ("Processed EFA output file:\n")
 cat (" ", ff, "\n")
 cat ("  Containing: ", ni, " specimens\n")
 cat ("              ", nharm, " harmonics\n")
 cat ("---------------------------------\n")

 return (om)
}

thin.outlines<- function(ws)
# takes array of tps objects and deletes all but the last OUTLINE
# to correct glitch in TPSDIG2 that makes multiple copies of OUTLINES when editing
{
 wt<- ws
 for (i in 1:length(wt))
  {
 	numo<- ws[[i]]$OUTLINES
 	if(!is.null(numo)){ 
 	wt[[i]]$OUTLINES<- 1
 	cn<- paste("outline",numo,".points", sep="")
 	wt[[i]]$outline1.points<- ws[[i]][[cn]]
 	
 	if(numo>1){
 		for (j in 2:numo)
 		{
 			cn<- paste("outline",j,".points",sep="")
 			wt[[i]][[cn]]<- NULL
 		}}
  }}	
  return(wt)
}

convertOutline<- function(w)
# take all outlines in an object and convert to curves
# curves are appended to end of any existing curves
{
 if(is.null(w$CURVES))	nc<- 0
 else 					nc<- w$CURVES
 
 if(is.null(w$OUTLINES))	no<- 0
 else 						no<- w$OUTLINES
 
 wc<- w		
 if(no>0){
 		wc$CURVES<- no+nc	# all prev outlines and curves
 		for(i in (nc+1):(nc+no))
 			{
 				cs<- paste0("curve", i, ".points")
 				os<- paste0("outline", i-nc, ".points")
 				wc[cs]<- w[os] 
				wc[os]<- NULL
 			}
 		wc$OUTLINES<- NULL
 	}
 
  return(wc)

}

convertOutlines<- function(ws, thin=TRUE)
# goes through a TPS object, and converts its outlines to curves
# assumes no curves present
# if thin==TRUE, thin.outlines() is applied first
{
	if(thin)	wt<- thin.outlines(ws)
	else 		wt<- ws
	
	wc<- wt
	n<- length(wc)
	for(i in 1:n)
		wc[[i]]<- convertOutline(wt[[i]])
			
	return(wc)
}


chop.curves<- function(ws, kind=c("curve", "outline"), curve.number=1, visual.check=FALSE)
# takes tps specimens, and chops out curve points in between landmark pairs
# assumes only 1 curve/outline per specimen
{
 wc<- ws
 
 for (i in 1:length(wc))
  {
    if(kind=="curve" && !is.null(wc[[i]]$CURVES)){
       cn<- paste0("curve", curve.number, ".points")	
       wc[[i]][[cn]]<- chop(wc[[i]][[cn]], LM=wc[[i]]$LM.points, closed=TRUE)	}

   
    if(kind=="outline" && !is.null(wc[[i]]$OUTLINES)){
       cn<- paste0("outline", curve.number, ".points")
       wc[[i]][[cn]]<- chop(wc[[i]][[cn]], LM=wc[[i]]$LM.points, closed=TRUE)
     } 
  }
 if(visual.check){
   	for(i in 1:length(wc))
   	{
   		plot1.tps(ws[[i]], add=FALSE)
   		plot1.tps(wc[[i]], add=TRUE, c.col="gray30", o.col="gray70")
   		title(paste0("Specimen #", i, " of ", length(wc)))
   		locator(n=1)
   		
   	}
 } 
 
 return (wc)	
	
}


chop<- function(xy, LM, closed=TRUE)
# function to do the chopping btwn adjacent LMs (1,2), (3,4), etc.
# xy is matrix of xy coordinates of curve/outline, LM is marix of LMs
{
 # get info
 nlm<- nrow(LM)
 ncut<- nlm/2
 no<- nrow(xy)
 
 ## find outline points closest to each LM
 LM.match<- numeric()
 for (i in 1:nlm){
 	dd<- numeric()
 	for(j in 1:no)
 			dd[j]<- edist(LM[i,1], LM[i,2], xy[j,1], xy[j,2])
 	
 	LM.match[i]<- which.min(dd)
	}
  
  bad<- numeric()
  for(i in 1:ncut){
  	bounds<- LM.match[(2*i-1):(2*i)]	# get relevant LMs
  	bad<- c(bad, bounds[1]:bounds[2])
  } 
  
  numBad<- length(bad)
  if(numBad > no/2)	xy.ok<- (1:no %in% bad)  # kludge: invert if chopping more than half of points
  else xy.ok<- !(1:no %in% bad)
  xy.chop<- xy[xy.ok,]
  if (closed) 	xy.chop<- rbind(xy.chop, xy.chop[1,])
  return(xy.chop)	
}

eq.curves <- function (ws, p=256, kind=c("curve", "outline"), ...)
# takes tps specimens, returns same with curves replaced by equally-spaced points
{
 we<- ws
 for (i in 1:length(we))
  {
    if(kind=="curve" && !is.null(we[[i]]$CURVES)){
    for (j in 1:we[[i]]$CURVES)
     {
       cn<- paste("curve", j, ".points", sep="")
       we[[i]][[cn]]<- eq.spaced(we[[i]][[cn]], np=p, ...)
     }}
   
    if(kind=="outline" && !is.null(we[[i]]$OUTLINES)){
    for (j in 1:we[[i]]$OUTLINES)
     {
       cn<- paste("outline", j, ".points", sep="")
       we[[i]][[cn]]<- eq.spaced(we[[i]][[cn]], np=p, ...)
     }} 
  }
 return (we)
}

# cat.tps<- function (w1, w2)
# # function to combine two tps series
# {
  # ws<- w1
  # n1<- length(w1)
  # n2<- length(w2)
  
  # for (i in 1:n2)
      # ws[[n1+i]]<- w2[[i]]
  
 # return (ws)
# }

cat.tps<- function(...)
{
	ll<- list(...)
	n<- length(ll)
	cl<- ll[[1]]
	for(i in 2:n)
		cl<- c(cl, ll[[i]])	
	
	class(cl)<- "tps"
	return(cl)
}

cat.tpsList<- function(ll)
{
	n<- length(ll)
	cl<- ll[[1]]
	for(i in 2:n)
		cl<- c(cl, ll[[i]])	
	
	class(cl)<- "tps"
	return(cl)
}

curve.area1 <- function (ws, nc=1, kind=c("curve", "outline"))
## returns array of areas of ccth curve in array of tps specimens
{
 cn<- paste (kind, nc, ".points", sep="")
 aa<- sapply (ws, FUN=tps.area, nc=nc, kind=kind)
 return (aa)
}

grab.tps<- function (ws, get="ID")
# make an array out of element grabbed from tps array
{
 nn<- length(ws)
 x<- array(dim=nn)
 for (i in 1:nn)
  {
    if (is.null(ws[[i]][[get]]))
    	x[i]<- NA
    else	
    	x[i]<- ws[[i]][[get]]
  
  }
 return (x)
}

sub.tps <- function (w, ok)
# subset a (mulitple specimen) tps object (w) according to a logical 
# or index vector (ok)
{
  if(class(w) != "tps")
  	stop("Object not of class 'tps'")
  
  ws<- w[ok]
  class(ws)<- "tps"
  return(ws)	
}


MD.lm <- function (w)
# go through tps object, replace all missing LM with c(NA NA)
# missing LM's correspond to integers listed in COMMENT field
{
  w.md<- w
  ns<- length(w.md) 	# number of specimens
  for (i in 1:ns)
   {     
     if (!is.null(w.md[[i]]$COMMENT))  #  if comment isn't NULL
 	   {
   	     cmd<- as.numeric(unlist(strsplit(w.md[[i]]$COMMENT," ")))
   	     w.md[[i]]$LM.points[cmd,] <- c(NA,NA)
   	   }	
   }   	

  return(w.md)	
}

LMdist <- function (w, measure, draw=NULL)
## computes distances between LM points
## measure is two-column matrix indicating the 2 points defining
## the distance to measure
{
  ns<- length(w)	# number specimens
  nm<- nrow(measure)
  lms<- array(dim=c(nm,4))  # temp array for LMs used
  dd<- array(dim=c(ns,nm))
  row.names(dd)<- grab.tps(w)
  colnames(dd)<-  paste("d", measure[,1], measure[,2], sep=".")

  for (i in 1:ns)
   {
   	for (j in 1:nm)
      {
   	    lms[j,]<- c(w[[i]]$LM.points[measure[j,1],],w[[i]]$LM.points[measure[j,2],])
      }	
     #return (lms) 
     dd[i,]<- apply(lms,1,edist2)
     if(!is.null(w[[i]]$SCALE))  dd[i,]<- dd[i,]*w[[i]]$SCALE
   }	
  
  if (!is.null(draw))
  	{
  	 plot(sub.tps(w,draw))
  	 xy<- w[[draw]]$LM.points
  	 for (i in 1:nm)
  	  {
  	    x1<- xy[measure[i,1],1]
  	    y1<- xy[measure[i,1],2]
  	    x2<- xy[measure[i,2],1]
  	    y2<- xy[measure[i,2],2]
  	    segments(x1,y1,x2,y2, lty=2, lwd=2)
  	    xm<- mean(c(x1,x1,x1,x2))
  	    ym<- mean(c(y1,y1,y1,y2))
  	    text(xm,ym, colnames(dd)[i], font=2)
  	  }	
  	}

  return (dd) 
}


LMmat<- function (w, np=w[[1]]$LM, lab=NULL, scale=TRUE)
# converts tps object into n x 2p matrix of coordinates
# x1, y1, x2, x2, ..., xp, yp
{
  ns<- length(w)
  mm<- array(dim=c(ns, 2*np))  # output matrix
  
  for (i in 1:ns)
   {
   	 nLM<- w[[i]]$LM
   	 if (nLM != np)
   	  {
   	 	mm[i,] <- rep(NA, 2*np)  # if number of LMs doesn't match, set to NA
   	 	msg<- paste("Warning: specimen #", i, " has ", nLM," [not ", np,"] LMs.\n", sep="")
   	 	cat(msg)
   	  }
   	 else
   	  {
   	 	mm[i,] <- matrix(t(w[[i]]$LM.points), nr=1, nc=2*np)
   	 	if (scale) mm[i,] <- mm[i,]*w[[i]]$SCALE
   	  }
   }	
  
  # set row and colnames
  if (is.null(lab))	row.names(mm)<- grab.tps(w)
  else				row.names(mm)<- lab
  colnames(mm)<- paste(c("x","y"), sort(rep(1:np,times=2)), sep="")
  
  return(mm)  	
}

tps.to.dryden <- function(w, p=w[[1]]$LM)
# outputs LMs into 3d array for Dryden's 'shapes' package
{ 
  ns<- length(w)
  Y<- array(dim=c(p,2,ns))  # output array
  
  for (i in 1:ns)
   	Y[,,i]<- w[[i]]$LM.points
   	
  return (Y)	
}


################## Accessory functions  ###########################

# returns Euclidean distance between 2 points 
edist<- function(x1,y1,x2,y2) sqrt( (x2-x1)^2 + (y2-y1)^2 )
edist2<- function(cc) sqrt( (cc[3]-cc[1])^2 + (cc[4]-cc[2])^2 )

# initializes tps specimen
init.tps<- function()  list(LM=0, IMAGE=NULL, ID=NULL, COMMENT=NULL, SCALE=NULL, CURVES=NULL, OUTLINES=NULL)

# plots single specimen
plot1.tps <- function (w, add=TRUE, xr=NULL, yr=NULL, pd=0.1, c.col="tomato", o.col="seagreen", lm.col="blue", lm.lab=TRUE)
{
  # x & y ranges
  if (is.null(xr))    # x range not provided
    {
      # combine all LMs and outline points
      if (w$LM>0)
      		xy<- w$LM.points
      else
      		xy<- matrix(NA,nr=1,ncol=2) 
      		
      if (!is.null(w$CURVES))
        {
          for (i in 1:w$CURVES)
            {
              cn<- paste("curve",i,".points", sep="")
              xy<- rbind(xy, w[[cn]])
            }
        }
     if (!is.null(w$OUTLINES))
        {
          for (i in 1:w$OUTLINES)
            {
              cn<- paste("outline",i,".points", sep="")
              xy<- rbind(xy, w[[cn]])
            }
        }
      
      if (nrow(xy)==1 & sum(is.na(xy))==2)	return()  
      
      xr<- range (xy[,1], na.rm=TRUE)
      xs<- (xr[2]-xr[1])*pd
      xr<- xr + c(-xs, xs)
      yr<- range (xy[,2], na.rm=TRUE)
      ys<- (yr[2]-yr[1])*pd
      yr<- yr + c(-ys, ys)
    }

  if (!add)
    plot(0,0, xlim=xr, ylim=yr, type="n", xlab="", ylab="", bty="n", asp=1)
    
  # add LMs
  if (w$LM>0)
    {
      points (w$LM.points, pch=19, col=lm.col)
      if (lm.lab) text(w$LM.points + c(0,(yr[2]-yr[1])/40), as.character(c(1:w$LM)), col=lm.col, font=3)
    }
  # add curves
  if (!is.null(w$CURVES))
    for (i in 1:w$CURVES)
      {
         cn<- paste("curve",i,".points", sep="")
         lines (w[[cn]], lty=1, col=c.col)
         points (w[[cn]][1,1], w[[cn]][1,2], pch=20, col=c.col) # indicate first point on curve
      }
  
  # add outlines
  if (!is.null(w$OUTLINES))
    for (i in 1:w$OUTLINES)
      {
         cn<- paste("outline",i,".points", sep="")
         lines (w[[cn]], lty=1, col=o.col)
         points (w[[cn]][1,1], w[[cn]][1,2], pch=20, col=o.col) # indicate first point on curve
      }


}

eq.spaced<- function (x, y=NULL, np=256, closed=TRUE, graph=FALSE)
# takes xy coordinates of outline, and returns np equally spaced points
# currently only tested for closed=TRUE
{
 if (is.null(y))
 	xy<- x
 else 	xy<- cbind(x,y)

 if (closed)
 	xy<- rbind(xy, xy[1,])

 p<- nrow(xy)
 pd<- array(dim=p)
 pd[1]<- 0
 for (i in 2:p)
	pd[i]<- edist(xy[i-1,1], xy[i-1,2], xy[i,1], xy[i,2])
 cd<- cumsum(pd)
 TL<- cd[p]
 IL<- TL/(np)
 if (closed)
	ILvec<- seq(0, TL-IL, by=IL)

 exy<- array(dim=c(np,2))
 exy[1,]<- xy[1,]
 for (i in 2:np)
  {
	wh<- sum(cd<=ILvec[i])			# which orig point immediately previous
	ff<- (ILvec[i]-cd[wh])/pd[wh+1]		# fraction of distance along this segment
	exy[i,1]<- xy[wh,1] + (xy[wh+1,1]-xy[wh,1])*(ff)
	exy[i,2]<- xy[wh,2] + (xy[wh+1,2]-xy[wh,2])*(ff)
  }

 if (graph)
  {
	plot(xy, type="l", col="grey", asp=1)
	points (exy, col="tomato3")
	title (paste ("Original: ", p, "points\n", "Equal spaced:", np, "points" ) )
  }

 return (exy)
}

get.area<- function (x, y=NULL, sc=1.0)
# returns area of closed curve (first point automatically copied to last)
# by default, assumes SCALE=1.0
{
 # handle input
 if (is.null(y))  # separate matrix into x,y vectors if needed
    {
      y<- x[,2]
      x<- x[,1]
    }
 # append first point to the end
 nn<- length(x)
 x[nn+1]<- x[1]
 y[nn+1]<- y[1]

 # go around outline
 aa<-0
 for (i in 1:nn)
    aa<- aa+ (0.5*(x[i+1]-x[i])*(y[i+1]+y[i]))

 aa<- aa*(sc^2) # multiply by SCALE^2
 aa<- abs(aa)

 return(aa)
}

# returns area of the curve/outline nc in tps specimen w
tps.area<- function (w, nc=1, kind=c("curve", "outline"))
{
  cn<- paste(kind, nc, ".points", sep="")
  if(is.null(w[[cn]])) aa<- NA
  else {  aa<- get.area(w[[cn]], sc=w$SCALE) }
  return (aa)
}

xy2.to.cplx <- function (x,y=NULL)
# convert matrix of xy (2 cols) to complex notation
{
  if (!is.null(y)) x<- cbind(x,y)

  z<- complex(real=x[,1], imag=x[,2])
  return (z)
}

get.curveLength<- function(x, y=NULL, sc=1.0, closed=FALSE)
{
 # handle input
 if (is.null(y))  # separate matrix into x,y vectors if needed
    {
      y<- x[,2]
      x<- x[,1]
    }
 
 # if needed, append first point to the end
 if(closed){
 	x[nn+1]<- x[1]
	y[nn+1]<- y[1]
	}
  nn<- length(x)
	
  L<- 0
  for (i in 1:(nn-1))
  	L<- L+ edist(x[i], y[i], x[i+1], y[i+1])
  	
  Ls<- L*sc
  return(Ls)
}


cplx.to.xy2 <- function (z)
# convert complex to xy2 notation
{
  xy<- array(dim=c(length(z), 2))
  xy[,1]<- Re(z)
  xy[,2]<- Im(z)
  return (xy)
}

rotate.tps<- function (w, theta)
# rotates tps configuration (LM & CURVES) by theta
# w is a single specimen, theta is in radians
{
 wr<- w
 # apply rotation to LMs and each curve separately
 if (w$LM>0)
  {
    dd<- matrix(w$LM.points, w$LM,2)  # needs to be a matrix for rotation
    ddr<- rotate.xy2(dd, theta)
    wr$LM.points<- ddr
  }

 if (w$CURVES>0 | !is.null(w$CURVES)) # at least 1 curve
  {
    for (i in 1:w$CURVES)
      {
       cn<- paste("curve", i, ".points", sep="")
       dd<-  w[[cn]]
       ddr<- rotate.xy2(dd, theta)
       wr[[cn]]<- ddr
      }
  }
  
 return (wr)
}

rotate.xy2 <- function (xy, theta)
# rotates matrix xy by theta
# rotation is about the origin, not the figure's centroid
{
  z<- xy2.to.cplx(xy)
  #mu<- mean(z)
  zc<- z#-mu
  itheta<- complex(real=0,imag=theta)
  zr<- exp(itheta)*zc
  zr2<- zr#+mu
  return (cplx.to.xy2(zr2))
}


ang.hv<- function(xy,point="right")
# returns angle to rotate so that vector xy[1,]->xy[2,]
# points in direction of (1,0)
{
  cxy<- xy2.to.cplx(xy)
  t1<- Arg(cxy[1]-cxy[2])   #vector from 1st to 2nd point
  th<-pi-t1
  #if (th>pi)    th<- th-2*pi
  if (pmatch(point,"up",nomatch=FALSE))
    th<- th-(3*pi/2)
  th
}

reflect.tps<- function (w, axis="x")
# reflects over X or Y axis
{
wr<- w
 # apply reflection to LMs and each curve separately
 if (w$LM>0)
  {
    dd<- matrix(w$LM.points, w$LM,2)  # needs to be a matrix for rotation
    if (axis=="x" | axis=="X")
      ddr<- cbind(dd[,1], -dd[,2])   #y-> -y
    else if (axis=="y" | axis=="Y")
      ddr<- cbind(-dd[,1], dd[,2])    #x-> -x
    else
      stop(paste ("Axis name must be x or y", "[", axis, "]"))
    wr$LM.points<-ddr
  }

 if (w$CURVES>0 | !is.null(w$CURVES)) # at least 1 curve
  {
    for (i in 1:w$CURVES)
      {
       cn<- paste("curve", i, ".points", sep="")
       dd<-  w[[cn]]
       if (axis=="x" | axis=="X")
        ddr<- cbind(dd[,1], -dd[,2])   #y-> -y
       else if (axis=="y" | axis=="Y")
        ddr<- cbind(-dd[,1], dd[,2])    #x-> -x
       else
        stop(paste ("Axis name must be x or y", "[", axis, "]"))
       wr[[cn]]<- ddr
      }
  }

 return (wr)
}


fitEllipse<- function(xy, sc=1, draw=FALSE, ret.ellipseXY=FALSE)
# function returns major (a)  and minor (b) semi-axes of best fit ellipse to matrix of x, y coordinates
# sc is scale factor; if draw=TRUE, the points and fitted ellipse are plotted
# if ret.ellipseXY=TRUE, also returns points 
{
  N<- nrow(xy)
  w<- prcomp(xy)
  xyr<- w$x   # take PC scores to rotate
  if(draw)  plot(xyr, asp=1, cex=0.5)
  
  # compute angles for points
  theta<- array(dim=N)
  for (i in 1:N)
  	theta[i]<- coord2rad(xyr[i,1], xyr[i,2])
  
  # initial guess for a and b
  a<- (max(xyr[,1]) - min(xyr[,1]))/2  # estimate as half range
  b<- (max(xyr[,2]) - min(xyr[,2]))/2  # estimate as half range		
  
  ff<- function(x)	sqrt(sum(x^2))
  Robs<- apply(xyr, 1, FUN=ff)
  p<- c(a,b)
  names(p)<- c("a", "b")
  #print(p)
  
  mse<- function(p, Robs)
  	{
  	  a<- p[1]
  	  b<- p[2]
  	  Rpred<- (a*b)/sqrt( (b*cos(theta))^2 + (a*sin(theta))^2 )
  	  ff<- mean((Robs - Rpred)^2)
  	  return(ff)	
  	}
  
  w<- optim(par=p, fn=mse, Robs=Robs, method="Neld")
  if(w$convergence != 0)	warning("Ellipse fitting did not converge.")
  a<- w$par[1]
  b<- w$par[2]
  Rfinal<- (a*b)/sqrt( (b*cos(theta))^2 + (a*sin(theta))^2 )
  xf<- Rfinal*cos(theta)
  yf<- Rfinal*sin(theta)
  if(draw) lines(xf, yf, col='red')
  
  #A1<- abs(get.area(x=xf, y=yf))
  #A2<- abs(get.area(xyr))
  #print(abs(A1-A2)/A2)
  
  if(!ret.ellipseXY)  res<- w$par*sc  # These are half-axis lengths
  else res<- list(par=w$par*sc, xy=cbind(xf, yf))
  return(res)

}

# itEllipse2<- function(xy)  
# # use direct algorithm of Halir & Flusser
# # uses polynomial representation; not sure correctly getting a and b from this
# # visually gives worse fit than fitEllipse()
# {
 # n<- nrow(xy)
 # xyc<- scale(xy, scale=F)
 # x<- xyc[,1]
 # y<- xyc[,2]
 # D1<- cbind(x*x, x*y, y*y)
 # D2<- cbind(x, y, rep(1,n))
 # S1<- t(D1)%*%D1
 # S2<- t(D1)%*%D2
 # S3<- t(D2)%*%D2
 
 # C1<- matrix(0, nrow=3, ncol=3)
 # C1[1,3]<- 2; C1[2,2]<- -1; C1[3,1]<- 2
 # M<- solve(C1)%*%(S1 - S2%*%solve(S3)%*%t(S2))
 
 # ee<- eigen(M)
 # pos.eval<- which(ee$values>0)
 # a1<- ee$vectors[,pos.eval]  # get eigenvector corresponding to positve eigenvalue
 # a2<- -solve(S3)%*%t(S2)%*%a1
 # aa<- c(a1,a2) 
 
 # #AQ<- matrix(c(aa[1], aa[2]/2, aa[4]/2, aa[2]/2, aa[3], aa[5]/2, aa[4]/2, aa[5]/2, aa[6]), nrow=3, ncol=3, byrow=TRUE)
 # #A33<- AQ[1:2, 1:2]
 # #eeA<- eigen(A33)
 # #dr<- -det(AQ)/det(A33)
 # #a<- sqrt(eeA$values[1]/dr)
 # #b<- sqrt(eeA$values[2]/dr)
 # #pp<- c(a,b)
 # #names(pp)<- c("a", "b")
  
 # return(aa)


# }

drawE2<- function(pp, xy)
{
  x<- xy[,1]
  np<- 100
  xf<- seq(min(x), max(x), length.out=np)
  yf1<- array(dim=np)
  yf2<- array(dim=np)
  for (i in 1:np)
  {
    a<- pp[3]
    b<- pp[2]*xf[i] + pp[5]
    c<- pp[1]*xf[i]^2 + pp[4]*xf[i] + pp[6]
    sr<- sqrt(b^2 -4*a*c)
    cat(sr,'\n')
    yf1[i]<- (-b + sr)/2*a
    yf2[i]<- (-b - sr)/2*a
  	
  }
  print(xf)
  print(yf1)
  print(yf2)
  
  plot(xy, asp=1, cex=0.5)
  points(xf, yf1, pch=19, col='red', cex=0.5)
  points(xf, yf2, pch=19, col='blue', cex=0.5)
	
}

checkOutlines<- function(w, specNum)
{
  x<- w[[specNum]]
  plot1.tps(x, add=FALSE)
  no<- x$OUTLINES
  msg<- paste(no, "outlines for specimen", specNum, "; showing last in red.\n")
  cat(msg)
  olab<- paste("outline", no, ".points", sep="")
  lines(x[olab][[1]][,1], x[olab][[1]][,2], col="red")
  mtext(paste("Specimen num.", specNum))
  
  return()
}

checkAllOutlines<- function(w)
{
  ns<- length(w)
  nout<- grab.tps(w, get="OUTLINES")
  nout[is.na(nout)]<- 0
  bad<- which(nout > 1)
  nbad<- length(bad)
  cat(ns, "specimens in TPS object. ", nbad, "have more than one associated outline.\n\n")
  if(nbad==0) return(NULL)
  ok<- array(dim=nbad)
  names(ok)<- bad

    
  for(i in 1:nbad)
  {
    checkOutlines(w, bad[i])
    mc<- menu(choices=c("OK", "bad", "exit"), title="Is this specimen OK?")
    if(mc==3) break
    if(mc==1) ok[i]<- TRUE  
    if(mc==2) ok[i]<- FALSE
  }
  
  return(ok)
}


# takes TPS object, processes to thin outlines, eq.space curve/outlines, and get 
# area and fit ellipse from outline/curve (whichever is present)
# each specimen must have at one outline or one curve (or neither, but not both)
flexAreaEllipse<- function(ws, p=256)
{
	wt<- thin.outlines(ws)
	we<- eq.curves(wt, p=p, kind="curve")
	we<- eq.curves(we, p=p, kind="outline")
	
	xyList<- list()
	nspec<- length(we)
	for(i in 1:nspec)
		{
			nc<- we[[i]]$CURVES; 	if(is.null(nc)) nc<- 0
			no<- we[[i]]$OUTLINES; 	if(is.null(no)) no<- 0
			if (nc+no > 1)	stop("More than one curve/outline in specimen # ", i, "\n")
			if(nc==0 && no==0)	xyList[[i]]<- NA;
			if(nc==1)	xyList[[i]]<- we[[i]]$curve1.points
			if(no==1)	xyList[[i]]<- we[[i]]$outline1.points
		}
		
	scalef<- grab.tps(ws, get="SCALE")
	res<- matrix(nrow=nspec, ncol=3)
	colnames(res)<- c("area", "Maj", "Min")
	for(i in 1:nspec)
		{
			# get area
			if(!is.na(xyList[[i]][1]))	res[i,1]<- get.area(xyList[[i]], sc=scalef[i])
			# fit ellipse
			if(!is.na(xyList[[i]][1]))	res[i, 2:3]<- fitEllipse(xyList[[i]], sc=scalef[i])
		}	
	
	return(res)
}

smoothCurve<- function(xy, ff=rep(1/5, 5), times=1, ret.all=FALSE)
# uses moving average to smooth according to filter ff
# xy is matrix of Nx2 x,y coordinates
{
	nf<- length(ff)
	lap<- (nf-1)/2  # needed overlap at start at end to avoid NAs
	N<- nrow(xy)
	res<- list()
	
	for (i in 1:times)
	{
		#print(xy)
		bot<- xy[(N-lap+1):N,]
		top<- xy[1:lap,]
		xyp<- rbind(bot, xy, top)  	# pad curve with start/end points repeated
		xypf<- filter(xyp, ff)		# filter padded curve
		xyf<- xypf[(lap+1):(N+lap),]	# subset to original length to drop NAs
		xy<- as.matrix(xyf)
		res[[i]]<- xyf
	}
	
	if(ret.all)	return(res)
	else		return(res[[times]])
}

 # outputs into "Out" format of package Momocs
 # ws must be simple with only one curve/outline per specimen
 # add capability to ff as a fac to Out object
 tps2Out<- function(ws, kind=c('curve', 'outline'), useID=TRUE, ff=NULL)
 {
 	matL<- list()
 	N<- length(ws)
 	kind<- match.arg(kind)
 	if(kind=="curve")     {cn<- "curve1.points"	; ca<- "CURVES"}
 	else				  {cn<- "outline1.points"; ca<- "OUTLINES"}
 	
 	if(useID){
 		for(i in 1:N){
 			lab<- ws[[i]]$ID
 			if(!is.null(ws[[i]][ca]))	matL[[lab]]<- unname(ws[[i]][cn][[1]])
 			}
 	}
 	
 	else{
 	for(i in 1:N)
 			if(!is.null(ws[[i]][ca]))	matL[[i]]<- unname(ws[[i]][cn][[1]])
	}
	
	keep<- !sapply(matL, is.null)
	if(is.null(ff))		out<- Out(matL[keep])  
	else out<- Out(matL[keep], subset(ff, keep))	
	return(out)
 }

 # return angle between 3 pts: m is 3x2 matrix
 # from coo_theta3(); + is CCW, - is CW
 theta3<- function(m)
 {
    a <- c(m[1, 1] - m[2, 1], m[1, 2] - m[2, 2])
    b <- c(m[3, 1] - m[2, 1], m[3, 2] - m[2, 2])

    ang<- atan2(a[1] * b[2] - a[2] * b[1], a[1] * b[1] + 
            a[2] * b[2])

    return(ang) 	
 }
 
 getQuartAng<- function(xy, fp=1, sp=0.25, show=FALSE)
 {
 	N<- nrow(xy)
 	ctr<- apply(xy, 2, mean)
 	p1<- xy[fp,]
 	p2<- xy[floor(N*sp),]
 	m<- rbind(p1, ctr, p2)
 	
 	th<- theta3(m)
 	if(show){
 		plot(xy, asp=1, cex=0.5, bty="n")
 		lines(xy)
 		points(m[,1], m[,2], col=c("green", "tomato", "tomato"))
 		segments(p1[1], p1[2], ctr[1], ctr[2], col="tomato")
 		segments(p2[1], p2[2], ctr[1], ctr[2], col="tomato")
 	}
 	
 	return(th)
 }

 # function to check if outline is CW or CCW and force all to be the same 
 orientCoo<- function(oo, force=c("CW", "CCW"))
 {
 	ooo<- oo
 	qa<- sapply(oo$coo, FUN=getQuartAng)
 	sqa<- sign(qa)
 	tt<- table(sqa)
 	
 	if(force=="CW") 	 wh.change<- sqa == 1  else wh.change<- sqa == -1
 	
 	N<- length(oo)
 	for(i in 1:N){
 		if(wh.change[i])		ooo$coo[[i]]<- coo_rev(oo$coo[[i]])
 	}
 		
 	return(ooo)
 	
 }
 
 # ws is tps object
flexAlign<- function(ws, p=256, show=TRUE)
{
	wt<- convertOutlines(ws, thin=TRUE)  #convert to CURVES if needed
	we<- eq.curves(wt, p=p, kind="curve")
	lab<- grab.tps(we)
	
	xyList<- list()
	nspec<- length(we)
	for(i in 1:nspec)
		{
			nc<- we[[i]]$CURVES; 	if(is.null(nc)) nc<- 0
			if (nc > 1)	stop("More than one curve in specimen # ", i, "\n")
			if(nc==0)	xyList[[i]]<- NA;
			if(nc==1)	xyList[[i]]<- we[[i]]$curve1.points
		}

	thetaV<- array(dim=nspec)			
	for(i in 1:nspec)
		{
			if(is.null(nrow(xyList[[i]])))	thetaV[i]<- NA
			else{
				pc<- prcomp(xyList[[i]])
				rot<- pc$rotation
				th<- atan(rot[2,1]/rot[1,1])
				thetaV[i]<- -th						
				}
		}	
		
	wea<- we
	for(i in 1:nspec){
		if(show){
			plot1.tps(we[[i]], add=F)
			title(main=paste("Specimen ", i, lab[i]))	
		}
		
	if(!is.na(thetaV[i]))	wea[[i]]<- rotate.tps(we[[i]], thetaV[i])
	if(show)	{
		plot1.tps(wea[[i]], c.col="green")
		mtext("Click to continue", side=3)
		locator(n=1)
		}
		
	}

	return(wea)
}


 
