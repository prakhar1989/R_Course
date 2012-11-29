complete = function(directory, id ){ #use 332
  df = data.frame(matrix(ncol=2, nrow=length(id)))
  names(df) = c("id", "nobs")
  for (i in 1:length(id)) {
    tmp = getmonitor(id[i], directory)
    df[i,] = c(id[i], sum(complete.cases(tmp)))
  }
  return(df)
}
