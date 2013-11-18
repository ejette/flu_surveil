first_r2s <-function(obj, vars, phys_look_up){
  # obj appears to be the goal timeseries
  # each provider appears to be a column in the pop matrix
  pop <- as.matrix(vars)
  results <- as.data.frame(matrix(NA,nrow(ili_unique),5))
  colnames(results) <- c('colname','phys_id','city','state','r2')
  for(i in 1:ncol(pop)){
    print(i)
    m.i<-lm(obj~pop[,i])
    r2.i<-summary(m.i)$r.squared
    id <- substr(colnames(pop)[i],7,100)
    print(id)
    city <- phys_look_up[phys_look_up$Phys_ID_Code == id,'City']
    state <- phys_look_up[phys_look_up$Phys_ID_Code == id,'St']
    results[i,] = c(colnames(pop)[i],id,city,state,r2.i)
  }#end for i
  write.csv(results,file="first_r2s.csv")
  #print(file.R2s) 
  return(results)
}#end func