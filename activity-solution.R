################################################################
#                     ACTIVITY SOLUTION
################################################################

ggplot(books, aes(x = release.year)) +
  stat_summary(aes(y = rating, color = "Average Rating"), 
               fun = "mean", geom = "line", size = 1) +
  stat_summary(aes(y = rating, color = "Average Rating"), 
               fun = "mean", geom = "point", size = 3) +
  stat_summary(aes(y = book.length / 100, color = "Average Length (÷100)"), 
               fun = "mean", geom = "line", size = 1) +
  stat_summary(aes(y = book.length / 100, color = "Average Length (÷100)"), 
               fun = "mean", geom = "point", size = 3) +
  labs(title = "Average Rating and Length Over Time",
       x = "Release Year",
       y = "Average Rating",
       color = "Metric") +
  theme_minimal() +
  theme(legend.position = "bottom")




