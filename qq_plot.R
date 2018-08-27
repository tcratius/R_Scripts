source("R/qq_plot.R")
#*************
# QQ plot
#************
qq_plot <- function(df, y, title, ylabel){
ggplot(data= df, aes(sample=df[[y]])) +
  geom_qq(col='red') +
  geom_qq_line(col='blue') +
  ggtitle(title) +
  ylab(label = ylabel) +
  theme_classic()
  
}  

# Function
qq_plot(h.sun, 2, "something", "time")

h.sun <- test %>%
  filter(!sunrise == is.na(sunrise)) %>%
  slice(1:100) %>%
  select(date, sunrise)
