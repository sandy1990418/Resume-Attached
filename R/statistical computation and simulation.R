library(data.table)
library(stringr)
library(dplyr)
library(plyr)
library(ggplot2)
library('rhep')
library(spgs)
library(energy)
library(EnvStats)
library(independence)
library(TauStar)
setwd("D:\\課程學習\\政大課程\\研究所課程\\統計模擬\\統模作業\\作業二")

######################   Q1  ########################

###### (a) 
'''
data A;
call streaminit(123);       /* set random number seed */
do i = 1 to 100000000;
   u = rand("Uniform");     /* u ~ U(0,1) */
   output;
end;
run;

'''
random_number_from_sas <-fread("random_number_from_sas.txt",header = T)


gap_test<-function(alpha,beta,x){
  library(plyr)
  library(tidyverse)
  temp <-c()
  x<-as.data.frame(x)
  colnames(x)<-"raw_data"
  x$gap_count<-ifelse(x$raw_data>alpha&x$raw_data<beta,1,0)
  temp<-which(x$gap_count==1)
  temp<-c(temp[1],temp[-1] - temp[-length(temp)])
  temp <- temp -1 
  temp_table <-count(temp)
  temp_table$expect_value<-round(dgeom(temp_table$x,(beta-alpha))*sum(temp_table$freq))
  temp_table$x<-ifelse(temp_table$freq>=5,temp_table$x,99)
  temp_table<-aggregate(. ~ x, temp_table, sum)
  print(temp_table)
  chi_stat <-sum((temp_table$freq-temp_table$expect_value)^2/temp_table$expect_value)
  print(chi_stat)
  p_value<-1-pchisq(chi_stat,df = nrow(temp_table)-1)
  
  return(p_value)
}




permutation_test<-function(x,k){
  library(stringr)
  library(plyr)
  permutation_matrix <- c()
  x<-x[1:(length(x)%/%k*k)]
  permutation_matrix <- as.data.frame(matrix(x,ncol = k,byrow = T))
  rank_matrix<-apply(permutation_matrix,1,rank)
  table_permutation<-count(apply(rank_matrix,2,function(x) as.integer(str_c(x, collapse = ""))))
  table_permutation$expect_value<-length(x)%/%k/factorial(k)
  chi_value<-sum((table_permutation$expect_value-table_permutation$freq)^2/table_permutation$expect_value)
  p_value <- 1-pchisq(chi_value,df=factorial(k))
  return(p_value)
}




###### (b) 

up_and_down<-function(x){
  library(stringr)
  library(plyr)
  library(EnvStats)
  # split character and convert to numeric
  x<-as.numeric(c(str_split(x,"",nchar(x))[[1]]))
  # convert to dataframe
  data <-as.data.frame(t(x))
  # generate all combinations
  while (TRUE) {
    y<-c()
    # sample from x 
    y  =  sample(x,length(x))
    # if sampling result does not exist in data then append 
    if (sum(apply(data, 1,function(x) ifelse(sum(y==x)==length(x),1,0)))==0){
      data = data.frame(rbind(data,y)) 
    }
    # condition to stop
    if (nrow(data)== factorial(length(x))/prod(factorial(count(x)$freq))){
      break
    }
  }
  
  total_result<-c()
  total_sum <-0
  for (k in c(1:nrow(data))){
    
    # get k th combination from data 
    w = data[k,]
    
    # count runs 
    temp<-c()
    for(i in c(1:(length(w)-1))){
      if(w[i]<w[i+1]){
        # if smaller then give 1 else 0
        temp<-c(temp,1)
      }else{
        temp<-c(temp,0)
      }
    }
    # count total runs
    total_sum<-total_sum+(sum(temp[-length(temp)]!=temp[-1])+1)
    # record result
    total_result<-c(total_result,(sum(temp[-length(temp)]!=temp[-1])+1))
  }
  #print(abs((total_sum - (2*length(x)-1)/3)/sqrt((16*length(x) - 29)/90)))
   
  
   ## check same variance
   chi_test_value= (length(total_result)-1)*var(total_result)/((16*length(x) - 29)/90)
   p_value_smaller = pchisq(chi_test_value,df=(length(total_result)-1))  
   p_value_greater = 1- pchisq(chi_test_value,df=(length(total_result)-1))  
   two_sided_chi_p_value = 2*min(p_value_smaller ,p_value_greater)
   ## use package to check the result
   #varTest(total_result,alternative="two.sided",conf.level = 0.95,sigma.squared = ((16*length(x) - 29)/90))
   
   ## check same mean 
   t_test_value = (mean(total_result)-((2*length(x)-1)/3))/(sd(total_result)*sqrt(length(total_result)))
   two_sided_t_p_value =  2*(1-pt(t_test_value,df = length(total_result)-1))
   
  return(list(length(x),total_sum,mean(total_result),var(total_result),two_sided_chi_p_value,two_sided_t_p_value))
}


start <- Sys.time()
up_and_down("264")
end <- Sys.time()
total_time <- as.numeric (end - start, units = "mins")


k<-c()
for (i in c(1:1000)){
  print(i)
  while(TRUE){
    rand_number<-sample(100:9999,1)
    rand_number<-as.character(rand_number)
    print(rand_number)
    if (nchar(rand_number)==length(unique(c(str_split(rand_number,"",nchar(rand_number))[[1]])))){
      #return(rand_number)
      break
    }
  }
  k=rbind(k,c(up_and_down(rand_number)[[1]],up_and_down(rand_number)[[2]],
              up_and_down(rand_number)[[3]],up_and_down(rand_number)[[4]],
              up_and_down(rand_number)[[5]],up_and_down(rand_number)[[6]]))
}

k<-as.data.frame(k)
colnames(k)<-c("length_x","total_runs","mean","var","chi_test_p_value","t_test_p_value")
sum(k$chi_test_p_value>0.05)
sum(k$t_test_p_value>0.05)


######################   Q2  ########################
###### (a) 
## download_web_link = https://pi2e.ch/blog/2017/03/10/pi-digits-download/

# set threads
setDTthreads(threads = 12)
# read data 
pi_data <- fread("pi_dec_1m.txt",header = F,nrows=21000)
# convert to character
pi_million<-as.character(pi_data$V1[1])
# split and convert to numeric 
pi_million<-as.numeric(c(str_split(pi_million,"",nchar(pi_million))[[1]]))
# create frequency table 
cont_table_pi<-count(pi_million)
cont_table_pi<-as.data.frame(cont_table_pi)
cont_table_pi$x<-as.factor(cont_table_pi$x)

# plot bar chart plot
ggplot(cont_table_pi) +
  geom_bar(mapping = aes(x = x, y = freq), stat = "identity")+
  ggtitle("Distribution of number of pi")+
  ylab("Frequency")+
  geom_text(aes(x = x, y = freq, label=freq),vjust=-1)



# convert to data frame 
pi_million<-as.data.frame(pi_million)
# plot ecdf and theoretical cdf 
ggplot(aes(pi_million),data=pi_million)+
  stat_ecdf()+theme_bw()+
  stat_function(fun=punif,args=list(0,9))+
  labs(title="ECDF and theoretical CDF")



###### (b) 
# ks test is not suitable for testing discrete data. Therefore,we use chi square test.
pi_million<-as.numeric(c(str_split(pi_million,"",nchar(pi_million))[[1]]))
chisq.unif.test(x=pi_million,interval = c(0,9))
library(wPerm)
pi_million<-pi_million/10


gap_test(0.3,0.6,pi_million)



######################   Q3  ########################
###### (a) 

# create 1000 random number = summation(U)-6
uniform_1000 <-c()
for (i in c(1:1000)){
  x= sum(runif(12))-6
  uniform_1000 <-c(uniform_1000,x)
}
# plot histogram
hist(uniform_1000)

## chisq test ##
##https://stackoverflow.com/questions/40708410/r-how-to-generate-a-vector-of-probabilities-normally-distributed-to-be-used-at-c
h <- hist(uniform_1000, breaks = 10)
p <- diff(pnorm(h$breaks, mean(uniform_1000),sd(uniform_1000)))
chisq.test(h$counts, p = p, rescale.p = TRUE)

## ks test ##
ks.test(uniform_1000,"pnorm", mean=mean(uniform_1000), sd=sd(uniform_1000))


###### (b)

# Linear congruential generator
q3_b<-function(run_time){
  total_rand<-c()
  for (j in c(1:run_time)){
    x = round(runif(1,0,100000),0)
    y = round(runif(1,0,100000),0)
    z = round(runif(1,0,100000),0)
    
    sample_c<-c()
    for (i in c(1:12)){
      u = (x/30269+y/30307+z/30323)%%1
      sample_c<-c(sample_c,u)
      
      x = (171*x)%%30269
      y = (172*y)%%30307
      z = (170*z)%%30323
    }
    total_rand<-c(total_rand,sum(sample_c)-6)
  }
  return(total_rand)
}



permutation_test<-function(x,k){
  library(stringr)
  library(plyr)
  permutation_matrix <- c()
  x<-x[1:(length(x)%/%k*k)]
  permutation_matrix <- as.data.frame(matrix(x,ncol = k,byrow = T))
  rank_matrix<-apply(permutation_matrix,1,rank)
  table_permutation<-count(apply(rank_matrix,2,function(x) as.integer(str_c(x, collapse = ""))))
  table_permutation$expect_value<-length(x)%/%k/factorial(k)
  chi_value<-sum((table_permutation$expect_value-table_permutation$freq)^2/table_permutation$expect_value)
  p_value <- 1-pchisq(chi_value,df=factorial(k))
  return(p_value)
}


per_pvalue<-c()
cor_test<-c()
for (k in c(1:100)){
  print(k)
  ## create
  set_a <- q3_b(200)
  set_b <- q3_b(200)
  set_c<-c(set_a,set_b)
  
  ## Use multivariate normal property. If two data follow bivariate normal dis and  they are indepedent iff correlaiton equals 0.
  multi_data<-as.data.frame(cbind(set_a,set_b))
  ## check 
  if (mvnorm.etest(multi_data, R=1000)$p.value>=0.05){
    cor_test<-c(cor_test,cor(multi_data$set_a,multi_data$set_b))
  }
  #per_pvalue<-c(per_pvalue,permutation_test(set_c,5))
  
  per_pvalue<-c(per_pvalue,hoeffding.D.test(set_a,set_b)$p.value)
  #https://cran.r-project.org/web/packages/independence/independence.pdf
  #print(perm.ind.test(multi_data,type="raw",
                #var.names = c('set_a','set_b'), R = 100)$p.value)
}

## count number of correlation approximating 0(meaning two data set are independent)
#https://stats.stackexchange.com/questions/384743/is-it-possible-for-two-independent-variables-to-be-correlated-by-chance
b_result <-sum(abs(cor_test)<0.1)
## count number of test which is not to reject H0(meaning two data set are independent)
p_result <-sum(per_pvalue>0.05)

cat("Result of using Bivariate normal property(Two sets are independent in 100 repetitive experiments):",b_result,
    "\nResult of using Hoeffding's test of independence(Two sets are independent in 100 repetitive experiments):",p_result )


######################   Q4  ########################
###### (a) 

##  box muller function 
box_muller<-function(number){
  u1 = runif(number)
  u2 = runif(number)
  theta = 2*pi*u1
  E = -log(u2)
  R = sqrt(2*E)
  X = R*cos(theta)
  Y = R*sin(theta)
  return(list(X,Y))
}
box = box_muller(100)

box_x <-box[[1]]
box_y <-box[[2]]
##  ks test with random number creating by using box muller 
ks.test(box_x,"pnorm")
ks.test(box_y,"pnorm")


##  polar method function 
polar_method<-function(number){
  v1 = runif(number,-1,1)
  v2 = runif(number,-1,1)
  X = c()
  Y = c()
  for (j in c(1:number)){
    if ((v1[j]^2+v2[j]^2)<1){
      W =v1[j]^2+v2[j]^2
      C = sqrt(-2*log(W)/W)
      X =c(X,C*v1[j])
      Y = c(Y,C*v2[j])
    }
  }
  return(list(X,Y))
}
polar  = polar_method(1000)

polar_x <-polar[[1]][is.na(polar[[1]])==FALSE]
polar_y <-polar[[2]][is.na(polar[[2]])==FALSE]
## correlation 
cor(polar_x,polar_y, use="complete.obs")
##  ks test with random number creating by using polar method
ks.test(polar_x,"pnorm")
ks.test(polar_y,"pnorm")


##  ratio of uniform function 
ratio_of_uniform<-function(number){
  library(dplyr)
  u1 = runif(number)
  u2 = runif(number)
  v = sqrt(2/exp(1))*(2*u2-1)
  x = v/u1
  z = x^2/4
  data = as.data.frame(cbind(x,z))
  # skip step 3 
  data$value<-ifelse(data$z<=(0.259/u1+0.35)&data$z<=-log(u1),1,0)
  # retain data matching conditions
  data%>%
    filter(value==1)->data
  return(data$x)
}
##  ks test with random number creating by using ratio of uniform method
ks.test(ratio_of_uniform(100),"pnorm")


## repeatedly experiment with different method
ks_result<-c()
for (k in c(1:10000)){
  print(k)
  box = box_muller(1000)
  box_x <-box[[1]]
  box_y <-box[[2]]
  
  polar  = polar_method(1000)
  polar_x <-polar[[1]][is.na(polar[[1]])==FALSE]
  polar_y <-polar[[2]][is.na(polar[[2]])==FALSE]
  cor(polar_x,polar_y, use="complete.obs")
  
  
  ks_result<-rbind(ks_result,
                   c(ks.test(box_x,"pnorm")$p.value,ks.test(box_y,"pnorm")$p.value,
                     ks.test(polar_x,"pnorm")$p.value,ks.test(polar_y,"pnorm")$p.value,ks.test(ratio_of_uniform(1000),"pnorm")$p.value))
}


ks_result<-as.data.frame(ks_result)
colnames(ks_result)<-c("box_x","box_y","polar_x","polar_y","ratio")

## count number of doing not reject H0
apply(ks_result,2,function(x) sum(x>=0.05))




###### (b) 
sample_a<-c()
temp<-c()
temp <- as.numeric(sample(100000:999999,1))

for (i in c(1:1000)){
  print(i)
  x<-(131*temp)%%(2^32)
  sample_a<-c(sample_a,x)
  temp<-x
}
sample_a<-sample_a/(2^32)
ks.test(sample_a,"punif")
chisq.unif.test(x=sample_a)


sample_b<-c()
temp<-c()
temp <- as.numeric(sample(100000:999999,1))

for (i in c(1:1000)){
  x<-(131*temp)%%(2^32)
  sample_b<-c(sample_b,x)
  temp<-x
}
sample_b<-sample_b/(2^32)
ks.test(sample_b,"punif")
chisq.unif.test(x=sample_b)


box_muller<-function(u1,u2,number){
  theta = 2*pi*u1
  E = -log(u2)
  R = sqrt(2*E)
  X = R*cos(theta)
  Y = R*sin(theta)
  return(list(X,Y))
}


linear_test<-box_muller(sample_a,sample_b,1000)

ks.test(linear_test[[1]],"pnorm")
ks.test(linear_test[[2]],"pnorm")
summary(linear_test[[1]])
summary(linear_test[[2]])

write.csv(sample_a,"u1.csv")
write.csv(sample_b,"u2.csv")

hist(linear_test[[1]])
hist(linear_test[[2]])

write.csv(linear_test[[1]],"box_x.csv")
write.csv(linear_test[[2]],"box_y.csv")




######################   Q6  ########################

'''
利用機率積分轉換(F(x)~U(0,1))

'''
rand_num_from_poisson<-function(lambda,start_value,num_of_rand){
  k=0
  while(ppois(k,lambda = lambda)<1){
    k=k+1
    if(ppois(k,lambda = lambda)>=1){
      break
    }
  }
  unif_random_number=as.data.frame(runif(num_of_rand))  
  colnames(unif_random_number)<-"rn_unif"
  for ( i in c(1:nrow(unif_random_number))){
    step = 1
    w = 0
    if(unif_random_number$rn_unif[i]<=ppois(start_value,lambda = lambda)){
      while(TRUE){
        if(unif_random_number$rn_unif[i]>=ppois((start_value-w),lambda = lambda)){
           unif_random_number$rn_poisson[i] = start_value-w+1
           unif_random_number$step[i] = step
           break
          }
        w = w+1
        step = step+1
        }
      }else{
        while(TRUE){
          if(unif_random_number$rn_unif[i]<=ppois((start_value+w),lambda = lambda)){
            unif_random_number$rn_poisson[i] = start_value+w
            unif_random_number$step[i] = step
            break
          }
          w = w+1
          step = step+1
        }
      }
  }
  return(list("random_number"=unif_random_number$rn_poisson,
              "step"=unif_random_number$step))
}

##  start value = 0 
temp_step<-c()
temp<-c()
for(i in c(1:1000)){
  test_data= rand_num_from_poisson(10,0,100)
  test_data_rand = test_data$random_number
  temp_step = c(temp_step,mean(test_data$step))
  
  temp_table  = plyr::count(test_data_rand )
  temp_table$expect_value<-round(dpois(temp_table$x, lambda = 10)*sum(temp_table$freq))
  temp_table$x<-ifelse(temp_table$freq>=5,temp_table$x,99)
  temp_table<-aggregate(. ~ x, temp_table, sum)
  temp_table$expect_value[nrow(temp_table)-1]<-sum(temp_table$freq) - sum(temp_table$expect_value[-(nrow(temp_table)-1)])
  
  chi_stat <-sum((temp_table$freq-temp_table$expect_value)^2/temp_table$expect_value)
  p_value<-1-pchisq(chi_stat,df =  nrow(temp_table)-1)
  
  temp <-c(temp,p_value)
}
## check if  poission 
sum(temp>=0.05)
mean(temp_step)

##  start value = mean
temp_step<-c()
temp<-c()
for(i in c(1:1000)){
  test_data= rand_num_from_poisson(10,10,100)
  test_data_rand = test_data$random_number
  temp_step = c(temp_step,mean(test_data$step))
  temp_table  = plyr::count(test_data_rand )
  temp_table$expect_value<-round(dpois(temp_table$x, lambda = 10)*sum(temp_table$freq))
  temp_table$x<-ifelse(temp_table$freq>=5,temp_table$x,99)
  temp_table<-aggregate(. ~ x, temp_table, sum)
  temp_table$expect_value[nrow(temp_table)-1]<-sum(temp_table$freq) - sum(temp_table$expect_value[-(nrow(temp_table)-1)])
  
  chi_stat <-sum((temp_table$freq-temp_table$expect_value)^2/temp_table$expect_value)
  p_value<-1-pchisq(chi_stat,df = nrow(temp_table)-1)
  
  #temp <-c(temp,chisq.pois(count(test_data_rand )$x, count(test_data_rand )$freq, lambda = 5)$p_value)
  temp <-c(temp,p_value)
}
## check if  poission 
sum(temp>=0.05)
mean(temp_step)


##  start value = mode
temp_step<-c()
temp<-c()
for(i in c(1:1000)){
  test_data= rand_num_from_poisson(10,9,100)
  test_data_rand = test_data$random_number
  temp_step = c(temp_step,mean(test_data$step))
  temp_table  = plyr::count(test_data_rand )
  temp_table$expect_value<-round(dpois(temp_table$x, lambda = 10)*sum(temp_table$freq))
  temp_table$x<-ifelse(temp_table$freq>=5,temp_table$x,99)
  temp_table<-aggregate(. ~ x, temp_table, sum)
  temp_table$expect_value[nrow(temp_table)-1]<-sum(temp_table$freq) - sum(temp_table$expect_value[-(nrow(temp_table)-1)])
  
  chi_stat <-sum((temp_table$freq-temp_table$expect_value)^2/temp_table$expect_value)
  p_value<-1-pchisq(chi_stat,df = nrow(temp_table)-1)
  
  #temp <-c(temp,chisq.pois(count(test_data_rand )$x, count(test_data_rand )$freq, lambda = 5)$p_value)
  temp <-c(temp,p_value)
}


## check if  poission 
sum(temp>=0.05)
mean(temp_step)



temp <-c()
for(i in c(1:1000)){
  test_data_0 = rand_num_from_poisson(10,0,10)
  test_data_mean = rand_num_from_poisson(10,10,10)
  test_data_0 = mean(test_data_0$step)
  test_data_mean = mean(test_data_mean$step)
  temp <-rbind(temp,c( test_data_0, test_data_mean))
}

sum(temp[,1],temp[,2])

mean(temp[,1])
mean(temp[,2])
######################   Q7  ########################
###### (a) 

Q7_func<-function(num){
  total_N<-c()
  for(i in c(1:num)){
    N<-0
    sum_u<-0
    while(sum_u<=1){
      u<-runif(1)
      sum_u<-sum_u+u
      N<-N+1
    }
    total_N<-c(total_N,N)
  }
  return(total_N)
}

q7_data <-Q7_func(1000)
cat("mean:",mean(q7_data),"\nsd:",sd(q7_data))

for (j in c(1000,2000,5000,10000,100000)){
  q7_data <-Q7_func(j)
  cat("times:",j,"\nmean:",mean(q7_data),"\nsd:",sd(q7_data), "\n\n")
}







'''
rand_num_from_poisson<-function(lambda,start_value,num_of_rand){
  step = 0
  x <- seq(0,lambda*10,by=1)
  step = step +1
  
  temp_x <- seq(0,lambda*10,by=1)
  step = step +1
  p<-c()
  for (i in c(1:length(x))){
    step = step +1
    #print(p)
    temp <- seq(0,x[i])
    if (sum((lambda^temp)*exp(-lambda)/(factorial(temp)))<=1 & is.na(sum((lambda^temp)*exp(-lambda)/(factorial(temp))))!=TRUE){
      p <- c(p,sum((lambda^temp)*exp(-lambda)/(factorial(temp)))) 
    }else{
      p <- c(p,integrate(function(x) (1/sqrt(2*pi))*exp(-(1/2)*(x^2)),-Inf,((x[i]-lambda)/sqrt(lambda)))$value)
      
    }
  }
  p<-p-c(0,p[-length(p)])
  step = step +1
  rand_num<-sample(x[which(x==start_value):length(x)] ,num_of_rand,replace = T,p=p[which(x==start_value):length(x)])
  step = step +1
  return(list("random_number"=rand_num,
              "step"=step))
  
}
'''


