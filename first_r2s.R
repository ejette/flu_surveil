first_r2s <-function(obj, vars, phys_look_up){
  # obj appears to be the goal timeseries
  # each provider appears to be a column in the pop matrix
  pop <- as.matrix(vars)
  phys_ids <- c()
  city <- c()
  state <- c()
  r2s <- c()
  for(i in 1:ncol(pop)){
        m.i<-lm(obj~pop[,i])
        r2.i<-summary(m.i)$r.squared
        r2s<-c(r2s,r2.i)
        phys_ids <- c(phys_ids, colnames(pop)[i])
        id <- substr(colnames(pop)[i],7,100)
        city <- c(city,phys_look_up[phys_look_up$Phys_ID_Code == id,'City'])
        state <- c(state,phys_look_up[phys_look_up$Phys_ID_Code == id,'St'])
      }#end for i
  write.csv(cbind(r2s,phys_ids,city,state),file="first_r2s.csv")
  print(file.R2s) 
  return(cbind(r2s,phys_ids,city,state))
}#end func