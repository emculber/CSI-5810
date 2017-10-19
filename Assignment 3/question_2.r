probability <- .6
number_of_weak_learners <- 7
x <- ceiling(number_of_weak_learners/2)

final_sum <- 0

for(i in number_of_weak_learners:x) {
  binomial_coefficient <- (factorial(number_of_weak_learners) / (factorial(i) * factorial(number_of_weak_learners-i)))
  p <- probability^i
  np <- (1-probability)^(number_of_weak_learners-i)

  final_sum <- final_sum + (binomial_coefficient * p * np)

  # print(binomial_coefficient)
  # print(p)
  # print(np)
}
print(final_sum)
