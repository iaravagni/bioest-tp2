library(ggplot2)
library(ggpubr)
library(qqplotr)


# ---------------- Ejercicio 1 ----------------

read_file = function(datapath,header,sep){
  
  file <- read.csv(datapath, sep=sep, header=header)
  return(file)
  
}

# -------------- Ejercicio 3 -----------

data_type = function(df){
  
  df_type <- data.frame(Variable = colnames(df),
                        Tipo = sapply(df, class))
  
  rownames(df_type)<-NULL
  
  print(df_type)
  
}


# ---------- Ejercicio 4 ------------
na_counts = function(df){

  df_NA <- data.frame(Variable = colnames(df),
                      Cant_NA = colSums(is.na(df)))
  
  rownames(df_NA)<-NULL
  
  print(df_NA)
}


# ------- Ejercicio 6 --------
# 

histogram = function(df,colname,inpbins){
  
  ggplot(df, aes(df[,colname])) + geom_histogram(aes(y = ..density..), bins = inpbins) + xlab(colname) + 
    stat_function(fun = dnorm, args = list(mean = mean(na.omit(df[,colname])), sd = sd(na.omit(df[,colname]))))

}  


# ------ Ejercicio 7 --------

my_boxplot = function(df,colname){
  
  ggplot(df, aes(df[,colname])) + geom_boxplot() + coord_flip() + xlab(colname)
 
}

#------- Ejercicio 8 --------
my_qqplot = function(df,colname){
  
  ggplot(df, mapping = aes(sample = df[,colname]))  + stat_qq_band() + stat_qq_line() + stat_qq_point() + labs(x = "Theoretical Quantiles", y = "Sample Quantiles")

  
}
