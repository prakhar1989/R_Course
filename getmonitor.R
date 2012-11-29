getmonitor = function(id, directory, summarize = F) {
    filename = paste(directory,"/", pad(id), ".csv", sep="")
    tmp = read.csv(filename, header=TRUE, comment.char="")
    if(summarize) {
      print(summary(tmp))
    }
    return(tmp)
}

pad = function(n) { 
  p = nchar(as.character(n))
  if(p ==1){
    return(paste("00", as.character(n), sep=""))
  } 
  if (p == 2){
    return(paste("0", as.character(n), sep=""))
  }
  return(as.character(n))
}