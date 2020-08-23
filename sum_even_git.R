# 1. What?
# 2. Solve?
# 3. Input?
# 4. Output? Print or return

sum_even_git.function <- function(start, end){
  sum_even <- 0
  for (i in start:end){
    if (i%%2==0){
      sum_even <- sum_even + i
    }
  }
  return(sum_even)
}
