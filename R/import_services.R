import_services<-function(path=NULL){
  
  #Error interceptions
  if (is.null(path)==FALSE & is.character(path)==FALSE){stop("If path is not NULL, it must be a character string")}
  
  #Locate extdata folder
  if (is.null(path)==TRUE){path<-system.file("extdata", package="heathland")}
  
  #create list
  data<-list(waterrecharge=as.matrix(read.table(paste(path, "waterrecharge.txt", sep="/"))),
             carbon=as.matrix(read.table(paste(path, "carbon.txt", sep="/"))),
             costs=as.matrix(read.table(paste(path, "costs.txt", sep="/"))),
             waterquality=as.matrix(read.table(paste(path, "waterquality.txt", sep="/"))),
             appreciation=as.matrix(read.table(paste(path, "appreciation.txt", sep="/"))))
  
  class(data)<-c("list", "services")
  
  return(data)}