corr <- function(directory, threshold = 0) {
  corrs = vector()
  for(i in 1:332){
    tmp = getmonitor(i, directory)
    v = complete.cases(tmp)
    if(sum(v) > threshold){
      m = tmp[v, 2:3]
      corrs = append(corrs, cor(m$sulfate, m$nitrate))
    }
  }
  return(corrs)
}