
options(expressions=500000)
log_factorial <- function (n) {
  # Return the log of factorial(n) for any integer n > 0
  if (n <= 1)
    return (0)
  return (log(n) + log_factorial(n - 1))
}

sum_log_factorial <- function (n) {
  # Return the sum of log_factorial(i) for i in 1..n
  sum <- 0
  for(i in seq(1, n, 1)) {
    sum <- sum + log_factorial(i)
  }
  return (sum)
}

fibonacci <- function(n) {
  # Return nth Fibonacci number
  if (n <= 1)
    return (n)
  return (fibonacci(n - 1) + fibonacci(n - 2))
}

n_l<-seq(200,4000,200)
RT_log_f<- sapply(n_l, function(x) system.time(log_factorial(x))[1])
plot(n_l, RT_log_f, main="log_factorial", xlab='n', ylab='Run time of log_factorial(n)')

n_s<-seq(200,2000,100)
RT_sumlog_f<- sapply(n_s, function(x) system.time(sum_log_factorial(x))[1])
plot(n_s, RT_sumlog_f, main="sum_log_factorial", xlab='n', ylab='Run time of sum_log_factorial(n)')

n_f<-seq(1,40,1)
RT_fib<- sapply(n_f, function(x) system.time(fibonacci(x))[1])
plot(n_f, RT_fib, main="fibonacci", xlab='n', ylab='Run time of fibonacci(n)')
