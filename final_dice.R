#gotowa kostka - obci¹¿ona
roll <- function(){
  die <- 1:6
  dice <- sample(x = die, size = 2, replace = FALSE, prob = c(1/8, 1/8, 1/8, 1/8, 1/8, 3/8))
  print(dice)
  sum(dice)
}

roll()

rolls <- replicate(n = 10000, expr = roll())
library(ggplot2)

qplot(x = rolls, binwidth = 1)

