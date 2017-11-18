get.CV.stat <- function(df, nfold, var, thr){
  
  
  if(nfold==1){
    
    x_agg <- df[, list(m = mean(target), n = .N), by = var]
    x_agg <- subset(x_agg, n>=thr)
    x_agg[, n:= NULL]
    return (x_agg)
    
  }else{
    
    set.seed(123)
    kfold <- rep(1:nfold, nrow(df)/nfold +1)
    kfold <- kfold[1:nrow(df)]
    kfold <- sample(kfold, length(kfold))
    
    
    df_temp <- data.table()
    for(k in 1:5){
      
      holdout <- df[kfold == k, ]
      df2 <- df[kfold != k, ]
      x_agg <- df2[, list(m = mean(target), n = .N), by = var]
      x_agg <- subset(x_agg, n>=thr)
      
      holdout <- merge(holdout, x_agg, by = var, all.x = T)
      df_temp <- rbind(df_temp, holdout)
      
    }
    
    
    
    df_temp <- df_temp[order(ID), c("ID","m"), with = F]
    return (df_temp)
    
  }
  
}



get.CV.stat.v2 <- function(df, nfold, var1, var2, thr, func, return_count = T){
  
  
  if(nfold==1){
    
    if(is.null(func)){
      x_agg <- df[, list(m = .N), by = var1]
    }else{
      x_agg <- df[, list(m = func(get(var2)), n = .N), by = var1]
      if(!is.null(thr)){
        x_agg <- subset(x_agg, n>=thr)
      }
      if(!return_count){
        x_agg[, n:= NULL]
      }
      
    }
    
    return (x_agg)
    
  }else{
    
    set.seed(123)
    kfold <- rep(1:nfold, nrow(df)/nfold +1)
    kfold <- kfold[1:nrow(df)]
    kfold <- sample(kfold, length(kfold))
    
    
    df_temp <- data.table()
    for(k in 1:nfold){
      
      holdout <- df[kfold == k, ]
      df2 <- df[kfold != k, ]
      
      if(is.null(func)){
        x_agg <- df2[, list(m = .N), by = var1]
      }else{
        x_agg <- df2[, list(m = func(get(var2)), n = .N), by = var1]
        if(!is.null(thr)){
          x_agg <- subset(x_agg, n>=thr)
        }
        if(!return_count){
          x_agg[, n:= NULL]
        }
      }
      
      holdout <- merge(holdout, x_agg, by = var1, all.x = T)
      df_temp <- rbind(df_temp, holdout)
      
    }
    
    
    if(!return_count){
      df_temp <- df_temp[order(ID), c("ID","m"), with = F]
    }else{
      df_temp <- df_temp[order(ID), c("ID","m","n"), with = F]
    }
    
    return (df_temp)
    
  }
  
}
