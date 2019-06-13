#$ cat hw1.r
#!/usr/bin/Rscript

# GT account name: xtao41
options(expressions=500000)


#2. Implementation of log_gamma_loop(n).
log_gamma_loop = function(n) {
  f=log(1)
  for (i in 2:n-1) {
    if(n>2){
      f=f+log(i)
    }
  }
  return(f)
}

print(log_gamma_loop(5))

#3. Implementation of log_gamma_recursive(n)
log_gamma_recursive = function (n){
  if(n<=2)
    return(log(1))
  else
    return(log(n-1)+log_gamma_recursive(n-1))
}

print(log_gamma_recursive(5))

#4. Implementation of sum_log_gamma_loop(n) and sum_log_gamma_recursive(n).
sum_log_gamma_loop = function(n){
  s=0
  for(i in 1:n){
    s=s+log_gamma_loop(i)
  }
  return(s)
}

sum_log_gamma_recursive=function(n){
  s=0
  for(i in 1:n){
    s=s+log_gamma_recursive(i)
  }
  return(s)
}

sum_lgamma=function(n){
  s=0
  for(i in 1:n){
    s=s+lgamma(i)
  }
  return(s)
}
  
# Tests for sum functions. 
# print(sum_log_gamma_recursive(2000))
# print(sum_log_gamma_loop(2000))
# print(sum_lgamma(2000))
# print(system.time(sum_log_gamma_recursive(3400))) #start seeing overflow when n=3600; no overflow when n=3500, so overflow was triggered when n is between 3500-3600.
# print(system.time(sum_log_gamma_loop(6000))) # n=6000, since it is slowing down
# print(system.time(sum_lgamma(10000))) #was zero up to 10000, use n=1000000

#5. To call the three functions with increasing values of n over a reasonable range and measure execution time. 
options(expressions=500000)
n=seq(200,3400,200)
runtime_loop  <- sapply(n, function(x) system.time(sum_log_gamma_loop(x))[1])
runtime_recursive  <- sapply(n, function(x) system.time(sum_log_gamma_recursive(x))[1])
runtime_buitin  <- sapply(n, function(x) system.time(sum_lgamma(x))[1])
Df=data.frame(n,runtime_loop,runtime_recursive,runtime_buitin)
#table showing running times of three different methods for comparison
print(Df)
#plot showing running times of three different methods for comparison
matplot(n,cbind(runtime_recursive,runtime_loop,runtime_buitin), pch=19, col=c(2,4,6),ylab="Run Time") 
legend("topleft", inset=.05, legend=c("runtime_recursive", "runtime_loop","runtime_buitin"), pch=19, col=c(2,4,6))
 

#Calculating Growth Rate of recursive method
Growth_Rate_Recursive<- vector(mode="integer", length=0)
for (i in 1:length(runtime_recursive)-1){
  Growth_Rate_Recursive[i]=(runtime_recursive[i+1]-runtime_recursive[i])/200
}
Growth_Rate_Recursive[length(runtime_recursive)] <- NA
Df_R=data.frame(n,runtime_recursive,Growth_Rate_Recursive)
print(Df_R)
plot(n,Growth_Rate_Recursive)

#Calculating Growth Rate of loop method
n_l=seq(200,6000,400)
runtime_loop2  <- sapply(n_l, function(x) system.time(sum_log_gamma_loop(x))[1])
Growth_Rate_loop<- vector(mode="integer", length=0)
for (i in 1:length(runtime_loop2)-1){
  Growth_Rate_loop[i]=(runtime_loop2[i+1]-runtime_loop2[i])/400
}
Growth_Rate_loop[length(runtime_loop2)] <- NA
Df_L=data.frame(n_l,runtime_loop2,Growth_Rate_loop)
print(Df_L)
plot(n_l,Growth_Rate_loop)

#Calculating Growth Rate of built_in lgamma implementation
n_b=seq(100000,2000000,100000)
runtime_lgamma  <- sapply(n_b, function(x) system.time(sum_lgamma(x))[1])
Growth_Rate_lgamma<- vector(mode="integer", length=0)
for (i in 1:length(runtime_lgamma)-1){
  Growth_Rate_lgamma[i]=(runtime_lgamma[i+1]-runtime_lgamma[i])/100000
}
Growth_Rate_lgamma[length(runtime_lgamma)] <- NA
Df_B=data.frame(n_b,runtime_lgamma,Growth_Rate_lgamma)
print(Df_B)
plot(n_b,Growth_Rate_lgamma)
