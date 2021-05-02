library(Matrix)
source("function_FDA.R")

##讀入資產
library(readxl)
CERN<- read_excel("ASSET.xlsx", sheet = "CERN")
IR<- read_excel("ASSET.xlsx", sheet = "IR")
OKE<- read_excel("ASSET.xlsx", sheet = "OKE")
FFIV<- read_excel("ASSET.xlsx", sheet = "FFIV")
NFLX<- read_excel("ASSET.xlsx", sheet = "NFLX")
CB<- read_excel("ASSET.xlsx", sheet = "CB")

CERN$Date <- as.Date(CERN$Date,format= "%Y%m%d")
IR$Date<-as.character(IR$Date)
IR$Date<-as.Date(IR$Date, format = "%Y%m%d")

OKE$Date <-as.character(OKE$Date)
OKE$Date <- as.Date(OKE$Date,format= "%Y%m%d")

FFIV$Date <-as.character(FFIV$Date)
FFIV$Date <- as.Date(FFIV$Date,format="%Y%m%d")

NFLX$Date <-as.character(NFLX$Date)
NFLX$Date <- as.Date(NFLX$Date,format="%Y%m%d")
CB$Date<-as.Date(CB$Date)


##calculate treasury_bond、CB returns(%)
CB$ASSET_RETURN<-c(NA, retx(as.numeric(CB$`Adj Close`)))


##combine returns
data_return<-data.frame(matrix(0, nrow(NFLX), 7))  
data_return[,1]<-NFLX$Date
data_return[,2:7]<-cbind(CERN$ASSET_RETURN,IR$ASSET_RETURN,OKE$ASSET_RETURN,FFIV$ASSET_RETURN,NFLX$ASSET_RETURN,CB$ASSET_RETURN)
colnames(data_return)<-c("Date","CERN","IR","OKE","FFIV","NFLX","CB")

##刪除缺失值(2011/01/03)
data_return<-data_return[-1,]



###另一種方法全部資產的敘述統計
##summary statistics, no annualization
summary_data_return<-rbind(apply(data_return[,2:ncol(data_return)],2,summary),
                           apply(data_return[,2:ncol(data_return)],2,var),
                           apply(data_return[,2:ncol(data_return)],2,sd),
                           apply(data_return[,2:ncol(data_return)],2,skewnessx),
                           apply(data_return[,2:ncol(data_return)],2,kurtosisx),
                           apply(data_return[,2:ncol(data_return)],2,acf1)
)

rownames(summary_data_return)[7:nrow(summary_data_return)]<-c("Var","Std.","SKewness","Kurtosis","ACF1")
summary_data_return<-t(round(summary_data_return,3))
summary_data_return

##make a data.frame object
class(summary_data_return)
summary_data_return<-data.frame(summary_data_return)


##covariance matrix
##no annualization  
covx<-cov(data_return[,2:ncol(data_return)])
covx
##corrlation matrix
corx<-cor(data_return[,2:ncol(data_return)])
corx

##calculate autocorrelation of AMZN return
##use function acf
##you can directly use "xts" object as an input
windows(width = 10, height = 8)
par(mfrow = c(3,2))  
acf(data_return$CERN,
    main = "Daily return of CERN",
    cex.lab = 1.5, cex.axis = 1.5)     ##address "na.action"
acf(data_return$IR,
    main = "Daily return of IR",
    cex.lab = 1.5, cex.axis = 1.5)     ##address "na.action"
acf(data_return$OKE,
    main = "Daily return of OKE",
    cex.lab = 1.5, cex.axis = 1.5)     ##address "na.action"
acf(data_return$FFIV,
    main = "Daily return of FFIV",
    cex.lab = 1.5, cex.axis = 1.5)     ##address "na.action"
acf(data_return$NFLX,
    main = "Daily return of NFLX",
    cex.lab = 1.5, cex.axis = 1.5)     ##address "na.action"
acf(data_return$CB,
    main = "Daily return of CB",
    cex.lab = 1.5, cex.axis = 1.5)     ##address "na.action"
###############################################################
##plot time series plot
windows(width = 10, height = 8)
par(mfrow = c(3,2))  

rangex<-range(data_return[,2])
plot( x = data_return$Date, y = data_return$CERN, type = "l",
      lwd = 2, col = 4,
      main = "CERN time series plot", 
      xlab = "Date", ylab ="daily return",
      cex.main = 1.8, cex.lab = 1.5, cex.axis = 1.5)

legend("topright", c("CERN"), 
       col = c(4), lwd = c(2), cex = 1.5)

rangex<-range(data_return[,3])
plot( x = data_return$Date, y = data_return$IR, type = "l",
      lwd = 2, col = 4,
      main = "IR time series plot", 
      xlab = "Date", ylab ="daily return",
      cex.main = 1.8, cex.lab = 1.5, cex.axis = 1.5)

legend("topright", c("IR"), 
       col = c(4), lwd = c(2), cex = 1.5)

rangex<-range(data_return[,4])
plot( x = data_return$Date, y = data_return$OKE, type = "l",
      lwd = 2, col = 4,
      main = "OKE time series plot", 
      xlab = "Date", ylab ="daily return",
      cex.main = 1.8, cex.lab = 1.5, cex.axis = 1.5)

legend("topright", c("OKE"), 
       col = c(4), lwd = c(2), cex = 1.5)

rangex<-range(data_return[,5])
plot( x = data_return$Date, y = data_return$FFIV, type = "l",
      lwd = 2, col = 4,
      main = "FFIV time series plot", 
      xlab = "Date", ylab ="daily return",
      cex.main = 1.8, cex.lab = 1.5, cex.axis = 1.5)

legend("topright", c("FFIV"), 
       col = c(4), lwd = c(2), cex = 1.5)

rangex<-range(data_return[,6])
plot( x = data_return$Date, y = data_return$NFLX, type = "l",
      lwd = 2, col = 4,
      main = "NFLX time series plot", 
      xlab = "Date", ylab ="daily return",
      cex.main = 1.8, cex.lab = 1.5, cex.axis = 1.5)

legend("topright", c("NFLX"), 
       col = c(4), lwd = c(2), cex = 1.5)

rangex<-range(data_return[,7])
plot( x = data_return$Date, y = data_return$CB, type = "l",
      lwd = 2, col = 4,
      main = "CB time series plot", 
      xlab = "Date", ylab ="daily return",
      cex.main = 1.8, cex.lab = 1.5, cex.axis = 1.5)

legend("topright", c("CB"), 
       col = c(4), lwd = c(2), cex = 1.5)

###############################################################

##calculate mean, variance and covariance of returns
mean_r<-apply(data_return[,2:7], 2, mean, na.rm = T)
var_r<-apply(data_return[,2:7], 2, var, na.rm = T)
cov_r<-cov(data_return[,2:7])

###############################################################
##buy-and-hold portfolio
rx<-rbind(0, data_return[,2:ncol(data_return)])      ##add initial period (return=0)   
rx<-1+rx                                             ##gross return
bh_cumr<-apply(rx, 2, cumprod)                       ##cumulative return of each asset
bh_cumr<-apply(bh_cumr, 1, mean)                     ##average of these cumulative returns

por_retbh<-bh_cumr[-1]/bh_cumr[-length(bh_cumr)]-1   ##portfolio return at each period

##a function for calculating BH portfolio return
por_ret_bh<-function(x){                             ##x: is a data frame or matrix of return data            
  
  x<-rbind(0, x)                                   ##add initial period (return=0)
  x<-x+1
  bh_cumr<-apply(x, 2, cumprod)
  bh_cumr<-apply(bh_cumr, 1, mean, na.rm = T)
  return(bh_cumr[-1]/bh_cumr[-length(bh_cumr)]-1)
  
}

por_retbh1<-por_ret_bh(data_return[,2:ncol(data_return)])     ##using the function
##cumulative daily portfolio return
##note that using bh_cumr
plot(x = data_return$Date, y = bh_cumr[-1],
     main = "Cumulative portfolio return",
     xlab = "Date", ylab = "Cumulative return", 
     type="l", lwd =2,
     cex.lab = 1.5, cex.axis = 1.2, cex.main = 1.8)
#################################################################
##1/N portfolio
##collect the return data
##1/N portfolio
Nx<-ncol(data_return)-1                                   ##number of assets 
wx<-rep(1/Nx, times = Nx)                            ##portfolio weights  
por_retN<-as.matrix(data_return[,2:ncol(data_return)])%*%wx    ##using weight vector
por_retN1<-apply(data_return[,2:ncol(data_return)],1,mean)     ##alternatovely using apply

##a function for calculating 1/N portfolio return
por_ret_N<-function(x){                              ##x: is a data frame or matrix of return data          
  
  return(apply(x, 1, mean, na.rm = T))
  
}

por_retN2<-por_ret_N(data_return[, 2:ncol(data_return)])       ##using the function

##compare results
all(por_retN==por_retN1)                             ##not exactly the same because of decimals
plot(por_retN1-por_retN, type="h")                   ##plot how large the difference at each time point

all(por_retN1==por_retN2)                            ##of course they should be exactly the same

##Cumulative daily portfolio return
N_cumr<-cumprod((1+por_retN2))  

plot(x = data_return$Date, y = N_cumr,
     main = "Cumulative portfolio return",
     xlab = "Date", ylab = "Cumulative return", 
     type="l", lwd =2, 
     cex.lab = 1.5, cex.axis = 1.2, cex.main = 1.8)
#################################################################
##plot the portfolio returns together(1/N、BUY AND HOLD)
windows(height = 8, width = 10)  
rangeg<-range(cbind(N_cumr, bh_cumr[-1]))

plot(x = data_return$Date, y = N_cumr, ylim = rangeg,
     main = "Cumulative portfolio return",
     xlab = "Date", ylab = "Cumulative return", 
     type="l", lwd =2, 
     cex.lab = 1.5, cex.axis = 1.2, cex.main = 1.8)
lines(x = data_return$Date, y = bh_cumr[-1], lwd = 1, col = 4, lty = 4)
abline(h = 1)
legend("topleft", legend = c("1/N", "buy-and-hold portfolio"),
       lty = c(1,4), col = c(1,4), lwd = c(2, 1))

#################################################################

##portfolio weights of the mvp (最小變異數投資組合)
mvp_wx<-function(r, mu_targ){               ##r: return data, mu_targ: required target return              
  
  n<-dim(r)[2]                              ##number of assets               
  mux<-apply(r, 2, mean, na.rm = T)         ##mean return vector      
  mux<-as.matrix(mux)
  covx<-cov(r, use = "complete.obs" )       ##note the usage of "use" vs na.rm
  covx<-as.matrix(covx)                       
  inv_covx<-solve(covx)                     ##inverse covariance matrix
  onex<-matrix(rep(1,n),n,1)                ##vector of ones
  
  ##constants A, B and C
  ABCx<-ABC_mvp(r)
  Ax<-as.numeric(ABCx$A)                         ##constant A
  Bx<-as.numeric(ABCx$B)                         ##constant B
  Cx<-as.numeric(ABCx$C)                         ##constant C
  
  
  ##Lagrange multipliers, delta and gamma
  deltax<-(Cx*mu_targ-Bx)/(Ax*Cx-Bx^2)       ##Lagrange multipler theta  
  deltax<-as.numeric(deltax)                 ##transform to scalar, very important
  gammax<-(Ax-Bx*mu_targ)/(Ax*Cx-Bx^2)       ##Lagrange multiplier gamma
  gammax<-as.numeric(gammax)
  
  ##optimal weight vector
  wx<-deltax*inv_covx%*%mux + gammax*inv_covx%*%onex
  
  return(wx)  
  
}

##mvp's standard deviation
##no risk-free asset                        ##mu_targ要求的期望報酬率
mvp_sdx<-function(r, mu_targ){              ##r: return data,rf: risk-free return
  
  n<-dim(r)[2]                              ##number of assets               
  mux<-apply(r, 2, mean, na.rm = T)
  mux<-as.matrix(mux)
  covx<-cov(r, use = "complete.obs" )        ##note the usage of "use" vs na.rm
  covx<-as.matrix(covx)  
  inv_covx<-solve(covx)                         ##inverse covariance matrix
  onex<-matrix(rep(1,n),n,1)                ##vector of ones
  
  ##constants A, B and C
  ABCx<-ABC_mvp(r)
  Ax<-as.numeric(ABCx$A)                         ##constant A
  Bx<-as.numeric(ABCx$B)                         ##constant B
  Cx<-as.numeric(ABCx$C)                         ##constant C
  
  sigma2x<-(Ax-2*Bx*mu_targ+Cx*(mu_targ^2))/(Ax*Cx-Bx^2)
  
  return(sqrt(sigma2x))
  
}


  
##obtain the weights for gmvp
consx<-ABC_mvp(data_return[,2:7])
gmvp_mr1<-consx$B/consx$C  ##期望報酬率

##利用兩種求出權重
wx<-mvp_wx(data_return[,2:7], gmvp_mr1)      

##using quadratic programming
library(quadprog)  

##mvp by using quadratic programming, need the package "quadprog"
##no risk-free assets
mvp_wx_quad<-function(r, mu_targ){      ##r: return data, mu_targ: required target return 
  
  n<-dim(r)[2]                          ##number of assets               
  mux<-apply(r, 2, mean, na.rm = T)
  mux<-as.matrix(mux)
  covx<-cov(r, use = "complete.obs" )   ##note the usage of "use" vs. na.rm
  covx<-as.matrix(covx)  
  onex<-matrix(rep(1,n),n,1)            ##vector of ones
  
  Axx<-cbind(mux, onex)                 ##Amat
  b0x<-matrix(c(mu_targ, 1), 2, 1)      ##b0
  d0x<-matrix(rep(0,n),n,1)             ##d0  
  
  solve.QP(Dmat = covx, dvec = d0x, Amat = Axx, bvec = b0x, meq = 2)##solve.QP處理二項規劃的問題  
  ##meq = 2代表有兩條equality constraints，等於0為不等式
  
}


##a simple example 
##target return (annualized): 5% 
mu_targ<- 0.001
rf<-0.0001

result<-mvp_wx_quad(data_return[,2:7], mu_targ)

wx<-as.matrix(result$solution)
sigma2x<-result$value*6                     ##should be multiplied by 2
sigma2x



##compare portfolio standard deviation
sqrt(t(wx)%*%cov(data_return[,2:7])%*%wx)
sqrt(sigma2x)


##gmvp
consx<-ABC_mvp(data_return[,2:7])
gmvp_mr<-consx$B/consx$C
result3<-mvp_wx_quad(data_return[,2:7], gmvp_mr)
gmvp_sd<-sqrt(result3$value*2)
gmvp_sd

##tangency portfollio
tan_mr<-tan_mr_sdx(data_return[,2:7], rf)$mu_tan
result4<-mvp_wx_quad(data_return[,2:7], tan_mr)
tan_sd<-sqrt(result4$value*2)
tan_sd
tan_mr_sdx(data_return[,2:7], rf)$sd_tan 
##using formula  
##GMVP and tangency portfolio
##This is a good way to set up the range of expected returns

consx<-ABC_mvp(data_return[,2:7])
gmvp_mr<-consx$B/consx$C
gmvp_mr

w_gmvp<-mvp_wx(data_return[,2:7], gmvp_mr)          ##solving mvp optimization with target return = gmvp_mrx
w_gmvp1<-gmvp_wx(data_return[,2:7])                 ##solving gmvp optimization with function

##compare w_gmvp and w_gmvp1
w_gmvp[1:6]
w_gmvp1[1:6]

##tangency portfolio's weight
w_tan<-tan_wx(data_return[,2:7], rf)

##tangency portfolio's expected return and standard deviation 
result_tan<-tan_mr_sdx(data_return[,2:7], rf)
result_tan$mu_tan               
result_tan$sd_tan




##generate optimal portfolio weight vector with the vector "por.mrx"
##verify whether the generated optimal portfolio weight vector the same as before

##Without risk-free assets
mvp_mr<-seq(-result_tan$mu_tan*1.1, result_tan$mu_tan*1.1,      ##sequence of expected mvp's returns 
            length = 2011)                                      ##max = tangency portfolio's expected return * 1.1 
##min = minus tangency portfolio's expected return * 1.1 
result1<-NULL

for(i in 1:length(mvp_mr)){
  
  mu_targ<-mvp_mr[i]
  result<-mvp_wx_quad(data_return[,2:7], mu_targ)
  result1<-rbind(result1,
                 c(mu_targ, 
                   sqrt(result$value*2)))
  
}  


########################################################################
##No-shortsale portfolio
##using quadratic programming
##no-shortsale mvp
nsmvp_wx_quad<-function(r, mu_targ){
  
  n<-dim(r)[2]                          ##number of assets               
  mux<-apply(r, 2, mean, na.rm = T)
  mux<-as.matrix(mux)
  covx<-cov(r, use = "complete.obs" )  ##note the usage of "use" vs na.rm
  covx<-as.matrix(covx)  
  onex<-matrix(rep(1, n), n,1)          ##vector of ones
  
  A<-cbind(mux, onex)                   ##Amat
  Ix<-diag(1, n)                        ##An identity matrix for no-shortsale constraints
  A<-cbind(A, Ix)
  b0<-matrix(c(mu_targ, 1, rep(0, n)), 2+n, 1)      ##b_0
  d0<-matrix(rep(0, n), n, 1)           ##d_0  
  
  return(solve.QP(Dmat = covx, dvec = d0, Amat = A, bvec = b0, meq = 2))
  
}




nsmvp_wx_quad1<-function(r, mu_targ){
  
  n<-dim(r)[2]                          ##number of assets               
  mux<-apply(r, 2, mean, na.rm = T)
  mux<-as.matrix(mux)
  covx<-cov(r, use = "complete.obs" )  ##note the usage of "use" vs na.rm
  covx<-as.matrix(covx)
  covx <- covx/ 10^6
  onex<-matrix(rep(1, n), n,1)          ##vector of ones
  
  A<-cbind(mux, onex)                   ##Amat
  Ix<-diag(1, n)                        ##An identity matrix for no-shortsale constraints
  A<-cbind(A, Ix)
  b0<-matrix(c(mu_targ, 1, rep(0, n)), 2+n, 1)      ##b_0
  d0<-matrix(rep(0, n), n, 1)           ##d_0  
  return(solve.QP(Dmat = covx, dvec = d0, Amat = A, bvec = b0, meq = 2))
  
}




mu_targ<- 0.0007                             ##note that in this case, mu_targ should be carefully set
rf<-0.0001
nx<-dim(data_return[,2:7])[2]                       ##number of assets

result<-nsmvp_wx_quad(data_return[,2:7], mu_targ)
wx<-as.matrix(result$solution)
wx
wx<-round(wx,16)                            ##round ，小數點以下8位不要 
wx
sum(wx<0)                                  ##check whether there are nonpositive weights
sum(wx==0);sum(wx>0)


##plot nsmvp portfolio weights
windows(height = 8, width = 10)

plot(wx, type = "h", xaxt = "n", lwd =2, col =2,
     xlab = "Company", ylab = "Weight",
     main = "Portfolio weights of the NSMVP", 
     cex = 1.8, cex.main = 2, cex.lab = 1.8)                    ##note how to set "xaxt"
axis(side = 1, at = c(1:6), 
     labels = colnames(data_return)[2:7], 
     las = 2)

##portfolio variance
sigma2x<-result$value*2
sigma2x

##verify constraints
t(wx)%*%apply(data_return[,2:7],2,mean)
mu_targ
t(wx)%*%matrix(rep(1,nx),nx,1)

sqrt(t(wx)%*%cov(data_return[,2:7])%*%wx)
sqrt(sigma2x)

##calculate component contribution##計算貢獻度
##wihtout the risk-free asset  
##with mu_targ = 0.08%, rf = 2.5/360
##function for calculating component contribution
por_ccx<-function(Sigmax, wx){                            ##sigmax: covariance matrix
  ##wx: portfolio weight
  wx<-as.numeric(wx)
  Sigmax<-as.matrix(Sigmax)
  varx<-t(wx)%*%Sigmax%*%wx
  varx<-as.numeric(varx)
  mcx<-Sigmax%*%wx/sqrt(varx)                             ##marginal contribution
  cx<-mcx*wx                                               ##contribution
  per_cx<-cx/sqrt(varx)*100                                ##percentage contribution
  
  list(marginal_contribution = mcx, 
       contribution = cx,
       percentage_contribution = per_cx)
  
}  




##obtain portfolio weights  
w_mvp<-mvp_wx(data_return[,2:7], mu_targ = 0.0007)               ##mvp
w_nsmvp<-nsmvp_wx_quad(data_return[,2:7], mu_targ = 0.0007)      ##no-shortsale mvp  
w_nsmvp<-round(w_nsmvp$solution,16)

##sample covariance matrix
covx<-cov(data_return[,2:7])

##result
result_mvp<-por_ccx(covx, w_mvp)
result_nsmvp<-por_ccx(covx,w_nsmvp)


##collect the results
result<-data.frame(cbind(result_mvp$percentage_contribution,
                         result_nsmvp$percentage_contribution))

rownames(result)<-names(data_return[,2:7])



##mvp
plot(result[,1], type = "h", xaxt = "n", lwd =2, col =2,
     xlab = "Comapny", ylab = "Percentage Contribution (%)",
     main = "MVP", 
     cex = 1.2, cex.main = 2, cex.lab = 1.5)                    ##note how to set "xaxt"
axis(side = 1, at = c(1:6), 
     labels = names(data_return[,2:7]), 
     las = 2, cex.axis = 0.8)


##no-shortsale mvp
plot(result[,2], type = "h", xaxt = "n", lwd =2, col =2,
     xlab = "Comapny", ylab = "Percentage Contribution (%)",
     main = "NSMVP", 
     cex = 1.2, cex.main = 2, cex.lab = 1.5)                    ##note how to set "xaxt"
axis(side = 1, at = c(1:6), 
     labels = names(data_return[,2:7]), 
     las = 2, cex.axis = 0.8)




########################################################################
##Compare performances of the portfolio strategies

##mean vector and covariance matrix estimated with rolling window, 
##window length is kx 
##portfolios: mvp and nsmvp

##使用ROLLING WINDOW建模
kx<-252*4                                   ##window length
##總長度減掉ROLLING WINDOW的長度
hx<-nrow(data_return)-kx                     ##length of out-of-sample period

##portfolio weights, starting from period t-1
wx_mat<-matrix(0, hx+1, ncol(data_return)-1)
wx1_mat<-matrix(0, hx+1, ncol(data_return)-1)
wx2_mat<-matrix(0, hx+1, ncol(data_return)-1)
wx3_mat<-matrix(0, hx+1, ncol(data_return)-1)



##portfolio  return 投資組合報酬
por_ret<-numeric(hx)
por_ret1<-numeric(hx)
por_ret2<-numeric(hx)
por_ret3<-numeric(hx)


##portfolio net return 投資組合淨收
por_netrx<-numeric(hx)
por_netrx1<-numeric(hx)
por_netrx2<-numeric(hx)
por_netrx3<-numeric(hx)

##turn over rate 
tor<-numeric(hx)
tor1<-numeric(hx)
tor2<-numeric(hx)
tor3<-numeric(hx)


##HHI
hhi<-numeric(hx)
hhi1<-numeric(hx)
hhi2<-numeric(hx)
hhi3<-numeric(hx)

##SLR (SHORT TIME LONG RATIO)
slr<-numeric(hx)
slr1<-numeric(hx)
slr2<-numeric(hx)
slr3<-numeric(hx)

##transaction cost
##可以更改成其他交易成本
epx<-0.0009                                                  ##transaction cost

##設定之要求報酬率(以下用非百分比表示)                                            
mu_targ<-0.0007


for(i in 1:hx){
  
  datax<-data_return[i:(i+kx-1), 2:ncol(data_return)]             ##data in the window (rolling window)
  ##wx可以改成其他策略
  ##ex:
  ##wx<-as.vector(gmvp_wx(datax, mu_targ = mu_targ)) 
  ##wx1<-1/49 (fixed weight)
  wx<-as.vector(mvp_wx(datax, mu_targ = mu_targ))               ##mvp
  wx1<-nsmvp_wx_quad(datax, mu_targ = mu_targ)$solution         ##nsmvp
  wx1<-round(wx1,8)
  wx2<-1/6
  
  rx<-data_return[i+kx,2:ncol(data_return)]                       ##return at period i+kx (period t+1)
  rx_lag<-datax[kx,]                                            ##return at period i+kx-1 (period t)
  

  ##individual assets' turnover over rate
  tor_ind<-wx-wx_mat[i,]*(1+rx_lag)/(1+sum(wx_mat[i,]*rx_lag))
  tor1_ind<-wx1-wx1_mat[i,]*(1+rx_lag)/(1+sum(wx1_mat[i,]*rx_lag))
  tor2_ind<-wx2-wx2_mat[i,]*(1+rx_lag)/(1+sum(wx2_mat[i,]*rx_lag))
  
  ##portfolio turn over rate
  tor[i]<-sum(abs(tor_ind))
  tor1[i]<-sum(abs(tor1_ind))
  tor2[i]<-sum(abs(tor2_ind))
  
  ##portfolio return
  por_ret[i]<-sum(wx*rx)
  por_ret1[i]<-sum(wx1*rx)
  por_ret2[i]<-sum(wx2*rx)
  
  ##portfolio net return
  por_netrx[i]<-(1+sum(wx*rx))*(1-epx*tor[i])-1
  por_netrx1[i]<-(1+sum(wx1*rx))*(1-epx*tor1[i])-1
  por_netrx2[i]<-(1+sum(wx2*rx))*(1-epx*tor2[i])-1
 
  
  ##HHI
  hhi[i]<-sum(wx^2)/(sum(abs(wx))^2)
  hhi1[i]<-sum(wx1^2)/(sum(abs(wx1))^2)
  hhi2[i]<-sum(wx2^2)/(sum(abs(wx2))^2)

  
  ##SLR
  slr[i]<-sum(abs(wx[wx<0]))/sum(abs(wx[wx>0]))
  slr1[i]<-sum(abs(wx1[wx1<0]))/sum(abs(wx1[wx1>0]))
  slr2[i]<-sum(abs(wx2[wx2<0]))/sum(abs(wx2[wx2>0]))
 
  ##store portfolio weight vector at this period
  wx_mat[i+1,]<-wx
  wx1_mat[i+1,]<-wx1
  wx2_mat[i+1,]<-wx2

  print(i) 
  
}  

##Oos  buy and hold

for(i in 1:hx){
  datax<-data_return[i:(i+kx-1), 2:ncol(data_return)] 
  rx<-data_return[i+kx,2:ncol(data_return)]   ##return at period i+kx (period t+1), scaled by 1/100
  rx_lag<-datax[kx,]   ##return at period i+kx-1 (period t).
  por_ret3[i]<-por_ret_bh(datax[,2:ncol(datax)])
  por_netrx3[i]<-por_ret_bh(datax[,2:ncol(datax)])
  
}

##portfolio  return
c(summary(por_ret),var(por_ret),sd(por_ret),skewnessx(por_ret),kurtosisx(por_ret),acf1(por_ret))
c(summary(por_ret1),var(por_ret1),sd(por_ret1),skewnessx(por_ret1),kurtosisx(por_ret1),acf1(por_ret1))
c(summary(por_ret2),var(por_ret2),sd(por_ret2),skewnessx(por_ret2),kurtosisx(por_ret2),acf1(por_ret2))
c(summary(por_ret3),var(por_ret3),sd(por_ret3),skewnessx(por_ret3),kurtosisx(por_ret3),acf1(por_ret3))


summary_oos_return<-rbind(c(summary(por_ret),var(por_ret),sd(por_ret),skewnessx(por_ret),kurtosisx(por_ret),acf1(por_ret)),
                          c(summary(por_ret1),var(por_ret1),sd(por_ret1),skewnessx(por_ret1),kurtosisx(por_ret1),acf1(por_ret1)),
                          c(summary(por_ret2),var(por_ret2),sd(por_ret2),skewnessx(por_ret2),kurtosisx(por_ret2),acf1(por_ret2)),
                          c(summary(por_ret3),var(por_ret3),sd(por_ret3),skewnessx(por_ret3),kurtosisx(por_ret3),acf1(por_ret3)))
colnames(summary_oos_return)[7:ncol(summary_oos_return)]<-c("Var","Std.","SKewness","Kurtosis","ACF1")
rownames(summary_oos_return)<-c("mvp","nsmvp.","fixed weight","buy and hold")
summary_oos_return<-round(summary_oos_return,4)

summary_oos_return

##plot the portfolio NET returns time series together(1/N、mvp、nsmvp、buy and hold )
windows(height = 8, width = 10)  


plot(x = data_return[(kx+1):nrow(data_return),]$Date, y =por_ret,ylim=c(-0.14,0.1),
     main = "portfolio  return of time series",
     xlab = "Date", ylab = "portfolio return", 
     type="l", lwd =2, cex.lab = 1.5, cex.axis = 1.2, cex.main = 1.8)
lines(x = data_return[(kx+1):nrow(data_return),]$Date, y = por_ret1, lwd = 1, col = 2, lty = 2)
lines(x = data_return[(kx+1):nrow(data_return),]$Date, y = por_ret2, lwd = 2, col = 4, lty = 3)
lines(x = data_return[(kx+1):nrow(data_return),]$Date, y = por_ret3, lwd = 2, col = 11, lty = 3)
legend("topleft", legend = c("1/N", "nsmvp","mvp","buy and hold"),
       lty = c(1,2,3,3), col = c(1,2,4,11), lwd = c(1, 1,3,2))



##portfolio net return
c(summary(por_netrx),var(por_netrx),sd(por_netrx),skewnessx(por_netrx),kurtosisx(por_netrx),acf1(por_netrx))
c(summary(por_netrx1),var(por_netrx1),sd(por_netrx1),skewnessx(por_netrx1),kurtosisx(por_netrx1),acf1(por_netrx1))
c(summary(por_netrx2),var(por_netrx2),sd(por_netrx2),skewnessx(por_netrx2),kurtosisx(por_netrx2),acf1(por_netrx2))
c(summary(por_netrx3),var(por_netrx3),sd(por_netrx3),skewnessx(por_netrx3),kurtosisx(por_netrx3),acf1(por_netrx3))


summary_oos_net_return<-rbind(c(summary(por_netrx),var(por_netrx),sd(por_netrx),skewnessx(por_netrx),kurtosisx(por_netrx),acf1(por_netrx)),
                              c(summary(por_netrx1),var(por_netrx1),sd(por_netrx1),skewnessx(por_netrx1),kurtosisx(por_netrx1),acf1(por_netrx1)),
                              c(summary(por_netrx2),var(por_netrx2),sd(por_netrx2),skewnessx(por_netrx2),kurtosisx(por_netrx2),acf1(por_netrx2)),
                              c(summary(por_netrx3),var(por_netrx3),sd(por_netrx3),skewnessx(por_netrx3),kurtosisx(por_netrx3),acf1(por_netrx3)))
colnames(summary_oos_net_return)[7:ncol(summary_oos_net_return)]<-c("Var","Std.","SKewness","Kurtosis","ACF1")
rownames(summary_oos_net_return)<-c("mvp","nsmvp.","fixed weight","buy and hold")
summary_oos_net_return<-round(summary_oos_net_return,4)

summary_oos_net_return


##plot the portfolio NET returns time series together(1/N、mvp、nsmvp、buy and hold )
windows(height = 8, width = 10)  

rangex<-range(por_netrx3)

plot(x = data_return[(kx+1):nrow(data_return),]$Date, y =por_netrx,ylim=rangex,
     main = "portfolio net  return of time series",
     xlab = "Date", ylab = "porfolio net return", 
     type="l", lwd =2, 
     cex.lab = 1.5, cex.axis = 1.2, cex.main = 1.8)
lines(x = data_return[(kx+1):nrow(data_return),]$Date, y = por_netrx1, lwd = 1, col = 2, lty = 2)
lines(x = data_return[(kx+1):nrow(data_return),]$Date, y = por_netrx2, lwd = 2, col = 4, lty = 3)
lines(x = data_return[(kx+1):nrow(data_return),]$Date, y = por_netrx3, lwd = 2, col = 11, lty = 3)
legend("topleft", legend = c("1/N", "nsmvp","mvp","buy and hold"),
       lty = c(1,2,3,3), col = c(1,2,4,11), lwd = c(1, 1,3,2))





##Annualized Sharpe ratio, annualized interest rate 1%
rfx<-0.0001
##sharp ratio
(mean(por_netrx)-rfx)/(sd(por_netrx))*sqrt(252)
(mean(por_netrx1)-rfx)/(sd(por_netrx1))*sqrt(252)
(mean(por_netrx2)-rfx)/(sd(por_netrx2))*sqrt(252)
(mean(por_netrx3)-rfx)/(sd(por_netrx3))*sqrt(252)

sharp_ratio<-rbind((mean(por_netrx)-rfx)/(sd(por_netrx))*sqrt(252),
                   (mean(por_netrx1)-rfx)/(sd(por_netrx1))*sqrt(252),
                   (mean(por_netrx2)-rfx)/(sd(por_netrx2))*sqrt(252),
                   (mean(por_netrx3)-rfx)/(sd(por_netrx3))*sqrt(252))
colnames(sharp_ratio)<-c("sharp_ratio")
rownames(sharp_ratio)<-c("mvp","nsmvp.","fixed weight","buy and hold")

##tunrover rate
c(summary(tor),sd(tor))
c(summary(tor1),sd(tor1))
c(summary(tor2),sd(tor2))
tunrover_rate<-rbind(c(summary(tor),sd(tor)),
                     c(summary(tor1),sd(tor1)),
                     c(summary(tor2),sd(tor2)))
colnames(tunrover_rate)[7]<-c("tunrover rate  Std.")
rownames(tunrover_rate)<-c("mvp","nsmvp.","fixed weight")
tunrover_rate

##HHI index
c(summary(hhi),sd(hhi1))
c(summary(hhi1),sd(hhi1))
c(summary(hhi2),sd(hhi2))

portfolio_hhi<-rbind(c(summary(hhi),sd(hhi1)),
                     c(summary(hhi1),sd(hhi1)),
                     c(summary(hhi2),sd(hhi2)))
colnames(portfolio_hhi)[7]<-c("hhi Std.")
rownames(portfolio_hhi)<-c("mvp","nsmvp.","fixed weight")
portfolio_hhi




##SLR
c(summary(slr),sd(slr))

##沒有SHORT SHALE
c(summary(slr1),sd(slr1))


##SLR FIXED WEIGHT
c(summary(slr2),sd(slr2))

##SLR  index
c(summary(slr),sd(slr))
c(summary(slr1),sd(slr1))
c(summary(slr2),sd(slr2))

portfolio_SLR<-rbind(c(summary(slr),sd(slr)),
                     c(summary(slr1),sd(slr1)),
                     c(summary(slr2),sd(slr2)))
colnames(portfolio_SLR)[7]<-c("SLR Std.")
rownames(portfolio_SLR)<-c("mvp","nsmvp.","fixed weight")
portfolio_SLR


##plot cumulative returns
windows(width = 8, height = 10)
par(mfrow = c(2,2))

##mvp
cumr_mvp<-cumprod(1+por_netrx)                                   
plot(x = data_return[(kx+1):nrow(data_return),]$Date,
     y = cumr_mvp, type = "l", lwd =1.5,
     main = "Cumulative net return (mvp)",
     xlab = "Date", ylab = "Cumulative return",
     cex.lab = 1.8, cex.axis = 1.5, cex.main = 1.8,
)

##nsmvp
cumr_nsmvp<-cumprod(1+por_netrx1)                                   
plot(x = data_return[(kx+1):nrow(data_return),]$Date,
     y = cumr_nsmvp, type = "l", lwd =1.5,
     main = "Cumulative net return (nsmvp)",
     xlab = "Date", ylab = "Cumulative return",
     cex.lab = 1.8, cex.axis = 1.5, cex.main = 1.8,
)


##fixwd weight
cumr_fixed<-cumprod(1+por_netrx2)                                   
plot(x = data_return[(kx+1):nrow(data_return),]$Date,
     y = cumr_fixed, type = "l", lwd =1.5,
     main = "Cumulative net return (fixed weight)",
     xlab = "Date", ylab = "Cumulative return",
     cex.lab = 1.8, cex.axis = 1.5, cex.main = 1.8,
)

##buy and hold
cumr_buy_and_hold<-cumprod(1+por_netrx3) 
plot(x = data_return[(kx+1):nrow(data_return),]$Date,
     y = cumr_buy_and_hold, type = "l", lwd =1.5,
     main = "Cumulative net return (buy and hold)",
     xlab = "Date", ylab = "Cumulative return",
     cex.lab = 1.8, cex.axis = 1.5, cex.main = 1.8,
)

##plot the portfolio returns together(1/N、mvp、nsmvp)
windows(height = 8, width = 10)  

rangex<-range(cumr_buy_and_hold)

plot(x = data_return[(kx+1):nrow(data_return),]$Date, y = cumr_fixed,ylim=rangex,
     main = "Cumulative portfolio return",
     xlab = "Date", ylab = "Cumulative return", 
     type="l", lwd =2, 
     cex.lab = 1.5, cex.axis = 1.2, cex.main = 1.8)
lines(x = data_return[(kx+1):nrow(data_return),]$Date, y = cumr_nsmvp, lwd = 1, col = 2, lty = 2)
lines(x = data_return[(kx+1):nrow(data_return),]$Date, y = cumr_mvp, lwd = 2, col = 4, lty = 3)
lines(x = data_return[(kx+1):nrow(data_return),]$Date, y = cumr_buy_and_hold, lwd = 2, col = 11, lty = 3)
legend("topleft", legend = c("1/N", "nsmvp","mvp","buy and hold"),
       lty = c(1,2,3,3), col = c(1,2,4,11), lwd = c(1, 1,3,2))



##plot the portfolio NET returns together(1/N、mvp、nsmvp、buy and hold )
windows(height = 8, width = 10)  

rangex<-range(por_netrx3)

plot(x = data_return[(kx+1):nrow(data_return),]$Date, y =por_netrx,ylim=rangex,
     main = "portfolio net  return",
     xlab = "Date", ylab = "Cumulative return", 
     type="l", lwd =2, 
     cex.lab = 1.5, cex.axis = 1.2, cex.main = 1.8)
lines(x = data_return[(kx+1):nrow(data_return),]$Date, y = por_netrx1, lwd = 1, col = 2, lty = 2)
lines(x = data_return[(kx+1):nrow(data_return),]$Date, y = por_netrx2, lwd = 2, col = 4, lty = 3)
lines(x = data_return[(kx+1):nrow(data_return),]$Date, y = por_netrx3, lwd = 2, col = 11, lty = 3)
legend("topleft", legend = c("1/N", "nsmvp","mvp","buy and hold"),
       lty = c(1,2,3,3), col = c(1,2,4,11), lwd = c(1, 1,3,2))

#################################################################