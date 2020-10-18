#Deal_function_final
deck <- read.csv("deck.csv")
DECK <- deck

deal <- function() {
  card <- deck[1, ]
  assign("deck", deck[-1, ], globalenv())
  card
}

shuffle <- function() {
  random <- sample(1:52, size = 52)
  assign("deck", DECK[random, ], globalenv())
}

shuffle()
deal()
deal()

sum(deck$value != 0)

shuffle()

sum(deck$value != 0)

deal()
deal()

sum(deck$value != 0)
