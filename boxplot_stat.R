#************
# Boxplot
#***********
box_plot <- function(df, x, y, title, xlabel, ylabel){
ggplot(df, aes(df[[x]], df[[y]])) +
  geom_boxplot(stat = "boxplot", fill="lightgreen") +
  ggtitle(title) +
  xlab(label = xlabel) +
  ylab(label = ylabel) +
  theme_classic()
}

#*************
# function
#************
box_plot(h.sun, 1, 2, "something", "date", "time")

