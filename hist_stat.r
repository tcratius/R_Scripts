#************
# Add and Run this at the top of file 
# This grabs the code in folder R.hist_stat.R reads it
# and makes it available in your normal code
#***********
source("R/hist_stat.R")

#*************
# Place this is a file and folder of the same name, you can
# edit source("R/hist_stat.R") if you want to change path etc.
# 
# Plots a histogram and apply a line of normality on it.
#************
hist_stat <- function(df, x, b) {
  ggplot(df, aes(df[[x]])) +
    
    geom_histogram(aes(y = stat(density)), bins = b) +
    stat_function(
      fun = dnorm, 
      args = list(mean = mean(df[[x]]), sd = sd(df[[x]])), 
      lwd = 1, 
      col = 'red') +
    theme_classic()
}    
#************
# Function call, place in main code. parameters data.frame, 
# column_index, bin size in integer.
#***********
hist_stat(df, 12, 40)
