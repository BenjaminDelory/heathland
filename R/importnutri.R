importnutri<-function(path=NULL){
  
  #Error interceptions
  if (is.null(path)==FALSE & is.character(path)==FALSE){stop("If path is not NULL, it must be a character string")}
  
  #Locate extdata folder
  if (is.null(path)==TRUE){path<-system.file("extdata", package="heathland")}
  
  #create list
  data<-list(grazing=as.matrix(read.table(paste(path, "grazing.txt", sep="/"))), 
             mowing=as.matrix(read.table(paste(path, "mowing.txt", sep="/"))), 
             burning=as.matrix(read.table(paste(path, "burning.txt", sep="/"))), 
             choppering=as.matrix(read.table(paste(path, "choppering.txt", sep="/"))), 
             sodcutting=as.matrix(read.table(paste(path, "sodcutting.txt", sep="/"))))
  
  class(data)<-c("list", "nutrient")
  
  return(data)}