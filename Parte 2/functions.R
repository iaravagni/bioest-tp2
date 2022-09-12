library(ggplot2)
library(ggpubr)
library(RVAideMemoire)
library(nortest)
library(dplyr) 
library(car)
library(qqplotr)


# ---------------- Leer csv ----------------

read_file = function(datapath,header,sep){
  
  file <- read.csv(datapath, sep=sep, header=header)
  return(file)
  
}
# -------------- tipos de datos -----------

data_type = function(df){
  
  df_type <- data.frame(Variable = colnames(df),
                        Tipo = sapply(df, class))
  
  rownames(df_type)<-NULL
  
  print(df_type)
  
}



# ---------- NAs  ------------
# Armar una funci?n que devuelva en formato dataframe
# la cantidad de NAs presentes por columna 

na_counts = function(df){
  
  df_NA <- data.frame(Variable = colnames(df),
                      Cant_NA = colSums(is.na(df)))
  
  rownames(df_NA)<-NULL
  
  print(df_NA)
}

# ------- Convertir a factor -----------
convert_to_factor = function(df,colname){
  
  df[,colname] <- as.factor(df[,colname])
  
  return(df)
  
}

# ------- Histograma --------
histogram = function(df,groupcol,colname,inpbins){
  if (groupcol == 'No agrupar'){
    
    ggplot(df, aes(df[,colname])) + 
      geom_histogram(aes(y = ..density..), bins = inpbins) + 
      xlab(colname) + 
      stat_function(fun = dnorm, args = list(mean = mean(na.omit(df[,colname])), sd = sd(na.omit(df[,colname]))))
    
  } else {
    
    levels <- unique(df[,groupcol])
    
    dat <- data.frame()
    dat_val <- rep()
    
    for (n in levels){
      df_aux <- df[df[,groupcol] == n,]
      df_aux <- df_aux[,colname]
      dat_val <- append(dat_val, rep(df_aux))
      
    }
    
    dat <- data.frame(variable = levels,
                     value= dat_val)
    
    ggplot(dat, aes(x=value, fill=variable)) +
      geom_histogram(aes(y = ..density..), alpha = 0.6, bins = inpbins) +
      xlab(colname)
      #stat_function(fun = dnorm, args = list(mean = mean(na.omit(df0[,colname])), sd = sd(na.omit(df0[,colname]))))
  }
  
}  

# ------ BOX PLOT --------
my_boxplot = function(df,colname,groupcol){
  
  if (groupcol == 'No agrupar'){
    
    ggplot(df, aes(df[,colname])) +
      geom_boxplot() + 
      coord_flip() + 
      xlab(colname)
    
  } else {
    
    levels <- unique(df[,groupcol])
    
    dat <- data.frame()
    dat_val <- rep()
    
    for (n in levels){
      df_aux <- df[df[,groupcol] == n,]
      df_aux <- df_aux[,colname]
      dat_val <- append(dat_val, rep(df_aux))
      
    }
    
    dat <- data.frame(variable = levels,
                      value= dat_val)
    
    ggplot(dat, aes(x=value, fill=variable)) +
      geom_boxplot(alpha = 0.6) +
      xlab(colname)
  }
  
}

#------- QQ PLOT --------
my_qqplot = function(df,colname,groupcol){
  
  if (groupcol == 'No agrupar'){
    
    ggplot(df, mapping = aes(sample = df[,colname])) +
      stat_qq_band() + stat_qq_line() + stat_qq_point() +
      labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
    
  } else {
    
    levels <- unique(df[,groupcol])
    
    dat <- data.frame()
    dat_val <- rep()
    
    for (n in levels){
      df_aux <- df[df[,groupcol] == n,]
      df_aux <- df_aux[,colname]
      dat_val <- append(dat_val, rep(df_aux))
      
    }
    
    dat <- data.frame(variable = levels,
                      value= dat_val)
    
    ggplot(dat, aes(sample=value, fill=variable)) +
      stat_qq_band(aplha = 0.5) + stat_qq_line() + stat_qq_point() +
      xlab(colname)
  }
  
  
}

# ----------- SHAPIRO _ WILK ---------------
shapiro_w = function(df,colname,groupcol){
 
}

# --------- Lilliefors -------------
lilliefors = function(df,colname,groupcol){
  
}

# --------- Test de Levene ---------
levene = function(df, colname, groupcol){
 
}


# ------ Test ... ---------

my_test = function(df,colname,groupcol){
}
