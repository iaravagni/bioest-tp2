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
    
    ggplot(df, aes(df[,colname], fill=df[,groupcol])) +
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
    
    ggplot(df, aes(df[,colname], fill=df[,groupcol])) +
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
    
    ggplot(df, aes(sample=df[,colname], fill=df[,groupcol])) +
      stat_qq_band(aplha = 0.5) + stat_qq_line() + stat_qq_point() +
      xlab(colname)
  }
  
  
}

# ----------- SHAPIRO _ WILK ---------------
shapiro_w = function(df,colname,groupcol){
  
  if (groupcol == 'No agrupar'){
    
    return(shapiro.test(df[,colname]))
    
  } else {
    
    data = split(df, df[,groupcol])
    dataG1 = data[[1]]
    dataG2 = data[[2]]
    
    shapiro1 = shapiro.test(as.numeric(dataG1[,colname]))
    shapiro2 = shapiro.test(as.numeric(dataG2[,colname]))

    return(list(shapiro1 = shapiro1, shapiro2 = shapiro2))
    }
    
}

# --------- Lilliefors -------------
lilliefors = function(df,colname,groupcol){
  
  if (groupcol == 'No agrupar'){
    
    return(shapiro.test(df[,colname]))
    
  } else {
    
    data = split(df, df[,groupcol])
    dataG1 = data[[1]]
    dataG2 = data[[2]]
    
    lillie1 = lillie.test(as.numeric(dataG1[,colname]))
    lillie2 = lillie.test(as.numeric(dataG2[,colname]))
    
    return(list(lillie1 = lillie1, lillie2 = lillie2))
  }
  
}


# --------- Test de Levene ---------
levene = function(df, colname, groupcol){
  
  return(leveneTest(y = df[,colname], group = df[,groupcol]))
}
    

# ------ Test t ---------

my_test = function(df,colname,groupcol){
  
  data = split(df, df[,groupcol])
  dataG1 = data[[1]]
  dataG2 = data[[2]]
  
  return(t.test(dataG1[,colname], dataG2[,colname],
                       paired = FALSE, alternative = "two.sided"))
  
}
