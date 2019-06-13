
#$ cat hw2.R
#!/usr/bin/Rscript
# GT account name: xtao41

#install.packages('ggplot2')
#install.packages('GGally')
data (midwest)

#Problem #1 and #2
# aggregation of the table
# subset of midwest dataset
DF<-midwest[,c('PID', 'county', 'state', 'popadults', 'perchsd', 'percollege', 'percprof')] 
# generate new cols for number of professionals, high school diploma and colledge degree
DF$N_prof<-with(DF,popadults*percprof/100)
DF$N_hsd<-with(DF,popadults*perchsd/100)
DF$N_college<-with(DF,popadults*percollege/100)

#aggregation and add percent of prof, hsd and colledge in a give state
DF_state<-aggregate(cbind(N_prof_S=DF$N_prof,N_hsd_S=DF$N_hsd,N_college_S=DF$N_college,popadults_S=DF$popadults), by=list(state=DF$state), FUN=sum)
DF_state$percprof_S<-with(DF_state,N_prof_S*100/popadults_S)
DF_state$perchsd_S<-with(DF_state, N_hsd_S*100/popadults_S)
DF_state$percollege_S<-with(DF_state, N_college_S*100/popadults_S)

#Problem#1 Relationship between state and professional employment
library(ggplot2)
ggplot(data.frame(DF_state$state, DF_state$percprof_S), aes(DF_state$state,DF_state$percprof_S)) + geom_point()

#Plot percprof for each county grouped by state
#ggplot(DF, aes(DF$state,DF$percprof)) + geom_boxplot(stat = "boxplot") 

#Problem #2 three-way relation between perschsd_S, percollege_S and State
DF_Threeway<-DF_state[,c('perchsd_S', 'percollege_S','state')]
library(GGally)
ggpairs(DF_Threeway)

# Relation of perchsd and percollege in general regardless group by state.
library(ggplot2)
qplot(x=perchsd, y=percollege, facets=.~state, data=DF, main="Percollege vs. Perchsd by state") + stat_smooth(se=FALSE)

#Q3 BOX plot example graph
#data ( "mpg")
#ggplot(mpg, aes(reorder(manufacturer, -cty, median), cty)) + geom_boxplot() +coord_flip() 

#Q4 Random Scatterplots

# 1) Generating two random sample scatterplots
# sample 1, N=20
x<-runif(20)
y<-runif(20)
ggplot(,aes(x, y))+ geom_point()+ggtitle("sample 1, N=20")
ggsave("random1.png")
file.size("random1.png")
file.remove("random1.png")
# sample 2, N=200
x<-runif(200)
y<-runif(200)
ggplot(,aes(x, y))+ geom_point()+ggtitle("sample 2, N=200")
ggsave("random2.png")
file.size("random2.png")
file.remove("random2.png")

#2) relationship between file size/type
# define function for calcuating file size given n
filesize=function(n){
  x<-runif(n)
  y<-runif(n)
  ggplot(,aes(x, y))+ geom_point()
  ggsave("random_ps.ps")
  ggsave("random_pdf.pdf")
  ggsave("random_jpeg.jpeg")
  ggsave("random_png.png")
  ps<-file.size("random_ps.ps")
  pdf<-file.size("random_pdf.pdf")
  jpeg<-file.size("random_jpeg.jpeg")
  png<-file.size("random_png.png")
  file.remove("random_ps.ps")
  file.remove("random_pdf.pdf")
  file.remove("random_jpeg.jpeg")
  file.remove("random_png.png")
  
  return (c(ps,pdf,jpeg,png))
}

#calculate file size/type vs n
library(ggplot2)
n1=seq(10,200,10)
n=sapply(n1,function(x) x^2)
file_sizes<-t(sapply(n,function(x)filesize(x) ))
size_df<-as.data.frame(file_sizes)
colnames(size_df) <- c("ps","pdf","jpeg","png") #make the data frame
size_df$n<-with(size_df,n) # adding n to the data frame

#plotting the file sizes vs n by type of file
matplot(size_df$n,cbind(size_df$ps,size_df$pdf,size_df$jpeg,size_df$png), pch=19, col=c("red","blue","purple","green"),ylab="File Size") 
legend("topleft", inset=.05, legend=c("ps","pdf","jpeg","png"), pch=19, col=c("red","blue","purple","green"))

# adding fittings
abline(lm(size_df$ps~size_df$n), col="red") # regression line (y~x)
abline(lm(size_df$pdf~size_df$n), col="blue") # regression line (y~x)
lines(lowess(size_df$n,size_df$jpeg), col="purple") # lowess line (x,y)
lines(lowess(size_df$n,size_df$png), col="green") # lowess line (x,y)

# add a referece fitting
#y=sapply(n, function(x) 10000*(x^0.3))
#lines(lowess(size_df$n,y), col="black") # lowess line (x,y)

#5 Diamonds
library(ggplot2)
data(diamonds)
# for color
ggplot(diamonds, aes(x=color))+geom_bar()
# for carat
ggplot(diamonds, aes(x=carat))+geom_histogram(binwidth = 0.3)
# for price
ggplot(diamonds, aes(x=price))+geom_histogram(binwidth = 700)
#three way relation of price, carat and color
Threeway_diamonds<-diamonds[,c('price', 'carat','color')]
library(GGally)
ggpairs(Threeway_diamonds)
# use another plot to better illustrate price vs carat relationship
qplot(carat,
      price,
      data = diamonds,
      main = "Price vs Carat") +
  stat_smooth()