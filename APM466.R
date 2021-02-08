install.packages("jrvFinance")
library("jrvFinance")
install.packages("lubridate")

library(readxl)
bond <- read_excel("Desktop/APM466 A1/APM466 A1 Bond price.xlsx", sheet = 2)
View(bond)

coupon_payment <- as.numeric(bond$coupon)
maturity_date <- gsub(x= bond$'maturity date',pattern=" UTC",replacement="",fixed=T)
close_price_date <- c("2021-01-18", "2021-01-19", "2021-01-20", "2021-01-21" ,"2021-01-22","2021-01-25","2021-01-26","2021-01-27","2021-01-28","2021-01-29")
close_price_matrix = matrix(c(bond$'18',bond$'19',bond$'20',bond$'21',bond$'22',bond$'25',bond$'26',bond$'27',bond$'28',bond$'29'),nrow = 10, ncol = 11,byrow = TRUE)

# calculate YTM 

ytm_matrix = matrix('numeric', nrow = 10, ncol = 11)
for(j in c(1:11)){
  close_price = close_price_matrix[,j]
  for(i in c(1:10)){
    ytm_matrix[i,j] <- bond.yield(settle = close_price_date[i], mature = maturity_date[j], coupon = coupon_payment[j], freq = 2, close_price[i], convention = c("30/360", "ACT/ACT", "ACT/360", "30/360E"), comp.freq = 2, redemption_value = 100)
  }
}

# calculate time to maturity in year-fraction

year_frac = matrix('numeric', nrow = 10, ncol = 11)
for(i in c(1:10)){
  for (j in c(1:11)){
    year_frac[i,j] = yearFraction(close_price_date[i], maturity_date[j], freq = 2, convention = c("30/360", "ACT/ACT", "ACT/360", "30/360E"))
}}

# plot ytm curve
year<-c(0,0.5,1,year_frac[1,4]-year_frac[1,1],2,year_frac[1,6]-year_frac[1,1],3,3.5,4,4.5,5)  
plot(year,ytm_matrix[1, ], type = "o", main = 'YTM Curve', col = "black", xlab = "Year", ylab = "YTM", ylim = c(0.0003,0.0055),lwd=1.0)
colour = c("red","orange","yellow","sienna","light blue","purple","powderblue","blueviolet", "yellowgreen")
for (i in c(2:10)){ 
  lines(year, ytm_matrix[i,], type = "o", col=colour[i-1],lwd=1.0)
}


# Add a legend to the plot

legend("topleft",pch=c(15,15),legend=c("2021-01-18", "2021-01-19", "2021-01-20", "2021-01-21" ,"2021-01-22","2021-01-25","2021-01-26","2021-01-27","2021-01-28","2021-01-29"), 
                col=c(1,2,3,4,5,6,7,8,9,10), lty=1.3, cex=0.6)


# linear interpolation on ytm for "2022-09-01"and "2023-09-01"

est_maturity <- c("2022-03-01", "2022-09-01", "2023-03-01", "2023-09-01","2024-03-01")
Jan = matrix('numeric', nrow=10, ncol=5, byrow = TRUE)
for (j in c(1:10)){
  for(i in c(1:5)){
    Jan[j,i] <- yearFraction(close_price_date[j],est_maturity[i])
  }
}
est_ytm_b4 <- c()
est_ytm_b6 <- c()

for (j in c(1:10)) { 
  dy_ratio_b4 <- (as.numeric(Jan[j,2])-as.numeric(Jan[j,1]))/(as.numeric(Jan[j,3])-as.numeric(Jan[j,1]))
  est_ytm_b4[j] = (as.numeric(ytm_matrix[,5][j])-as.numeric(ytm_matrix[,3][j]))*dy_ratio_b4 + as.numeric(ytm_matrix[,3][j])

  dy_ratio_b6 <- (as.numeric(Jan[j,4])-as.numeric(Jan[j,3]))/(as.numeric(Jan[j,5])-as.numeric(Jan[j,3]))
  est_ytm_b6[j] = (as.numeric(ytm_matrix[,7][j])-as.numeric(ytm_matrix[,5][j]))*dy_ratio_b6 + as.numeric(ytm_matrix[,5][j])
}

ytm_matrix_md = ytm_matrix
ytm_matrix_md[,4] <- est_ytm_b4
ytm_matrix_md[,6] <- est_ytm_b6

maturity_date_md <- c("2021-03-01","2021-09-01","2022-03-01","2022-09-01","2023-03-01","2023-09-01","2024-03-01","2024-09-01","2025-03-01","2025-09-01","2026-03-01")

year_frac_md = year_frac
for(i in c(1:10)){
  for (j in c(4,6)){
    year_frac_md[i,j] = yearFraction(close_price_date[i], maturity_date_md[j], freq = 2, convention = c("30/360", "ACT/ACT", "ACT/360", "30/360E"))
  }
}

# plot ytm curve (with linear interpolation)
year<-c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5)
plot(year,ytm_matrix_md[1, ], type = "o", main = 'YTM Curve with interpolation', col = "black", xlab = "Year", ylab = "YTM", ylim = c(0.0003,0.0052),lwd = 0.8)
colour = c("red","orange","yellow","sienna","light blue","purple","powderblue","blueviolet", "yellowgreen")
for (i in c(2:10)){ 
  lines(year, ytm_matrix_md[i,], type = "o", col=colour[i-1], lwd = 0.8)
}

# Add a legend to the plot

legend("topleft",pch=c(15,15),legend=c("2021-01-18", "2021-01-19", "2021-01-20", "2021-01-21" ,"2021-01-22","2021-01-25","2021-01-26","2021-01-27","2021-01-28","2021-01-29"),
       col=c(1,2,3,4,5,6,7,8,9,10),lty=1.3,cex=0.6)


### Question 4(b): calculate spot rate

# calculate dirty price

dp <- matrix('numeric', nrow = 10, ncol = 11)

for (j in 1:10){
  for (i in 1:11){
    dp[j,i] = bond.TCF(close_price_date[j],maturity_date[i],coupon_payment[i],freq=2)$accrued + close_price_matrix[j,i]
  } 
}

# calculate cash flow

cf <- list()
for (i in 1:10){
  cf= bond.TCF(close_price_date[i],maturity_date[i],coupon_payment[i],freq=2,redemption_value = 100)$cf
  print(cf)
}

cf_1 = c(100.375)
cf_2 = c(0.375, 100.375)
cf_3 = c(0.25, 0.25, 100.25)
cf_4 = c(0.125, 0.125, 0.125, 100.125)
cf_5 = c(0.875, 0.875, 0.875, 0.875, 100.875)
cf_6 = c(0.75, 0.75, 0.75, 0.75, 100.75)
cf_7 = c(1.125, 1.125, 1.125, 1.125, 1.125, 1.125, 101.125)
cf_8 = c(0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 100.75)
cf_9 = c(0.625, 0.625, 0.625, 0.625, 0.625, 0.625, 0.625, 0.625, 100.625)
cf_10 = c(0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 100.25)
cf_11 = c(0.125, 0.125, 0.125, 0.125, 0.125, 0.125,0.125, 0.125, 0.125,0.125, 100.125)


# calculate spot curve

raw_spot_matrix <- matrix(nrow = 10, ncol = 11)

for (i in 1:10) {
  
  t_1 = as.numeric(year_frac[i,1])
  sf_1 = function(x) as.numeric(dp[i,1]) - cf_1[1]*(1+x/2)^(-2*t_1)
  s_1 = uniroot(sf_1,c(0,1))$root
  
  t_2 = as.numeric(year_frac[i,2])
  sf_2 = function(x) as.numeric(dp[i,2]) - cf_2[1]*(1+s_1/2)^(-2*(t_2-0.5*1))- cf_2[2]*(1+x/2)^(-2*t_2)
  s_2 = uniroot(sf_2,c(0,1))$root
  
  t_3 = as.numeric(year_frac[i,3])
  sf_3 = function(x) as.numeric(dp[i,3]) - cf_3[1]*(1+s_1/2)^(-2*(t_3-0.5*2)) - cf_3[2]*(1+s_2/2)^(-2*(t_3-0.5*1)) - cf_3[3]*(1+x/2)^(-2*t_3)
  s_3 = uniroot(sf_3,c(0,1))$root
  
  t_4 = as.numeric(year_frac[i,4])
  sf_4 = function(x) as.numeric(dp[i,4]) - cf_4[1]*(1+s_1/2)^(-2*(t_4-0.5*3)) - cf_4[2]*(1+s_2/2)^(-2*(t_4-0.5*2)) - cf_4[3]*(1+s_3/2)^(-2*(t_4-0.5*1)) - cf_4[4]*(1+x/2)^(-2*t_4)
  s_4 = uniroot(sf_4,c(0,1))$root
  
  t_5 = as.numeric(year_frac[i,5])
  sf_5 = function(x) as.numeric(dp[i,5]) - cf_5[1]*(1+s_1/2)^(-2*(t_5-0.5*4)) - cf_5[2]*(1+s_2/2)^(-2*(t_5-0.5*3)) - cf_5[3]*(1+s_3/2)^(-2*(t_5-0.5*2)) - cf_5[4]*(1+s_4/2)^(-2*(t_5-0.5*1)) - 
    cf_5[5]*(1+x/2)^(-2*t_5)
  s_5 = uniroot(sf_5,c(0,1))$root
  
  t_6 = as.numeric(year_frac[i,6])
  sf_6 = function(x) as.numeric(dp[i,6]) - cf_6[1]*(1+s_1/2)^(-2*(t_6-0.5*4)) - cf_6[2]*(1+s_2/2)^(-2*(t_6-0.5*3)) - cf_6[3]*(1+s_3/2)^(-2*(t_6-0.5*2)) - cf_6[4]*(1+s_4/2)^(-2*(t_6-0.5*1)) - 
    cf_6[5]*(1+x/2)^(-2*t_6)
  s_6 = uniroot(sf_6,c(0,1))$root
  
  t_7 = as.numeric(year_frac[i,7])
  sf_7 = function(x) as.numeric(dp[i,7]) - cf_7[1]*(1+s_1/2)^(-2*(t_7-0.5*6)) -  cf_7[2]*(1+s_2/2)^(-2*(t_7-0.5*5)) - cf_7[3]*(1+s_3/2)^(-2*(t_7-0.5*4)) - cf_7[4]*(1+s_4/2)^(-2*(t_7-0.5*3)) - 
    cf_7[5]*(1+s_5/2)^(-2*(t_7-0.5*2)) - cf_7[6]*(1+s_6/2)^(-2*(t_7-0.5*1)) -  cf_7[7]*(1+x/2)^(-2*t_7)
  s_7 = uniroot(sf_7,c(0,1))$root
  
  t_8 = as.numeric(year_frac[i,8])
  sf_8 = function(x) as.numeric(dp[i,8]) - cf_8[1]*(1+s_1/2)^(-2*(t_8-0.5*7)) - cf_8[2]*(1+s_2/2)^(-2*(t_8-0.5*6)) - cf_8[3]* (1+s_3/2)^(-2*(t_8-0.5*5)) - cf_8[4]*(1+s_4/2)^(-2*(t_8-0.5*4)) - 
    cf_8[5]*(1+s_5/2)^(-2*(t_8-0.5*3)) - cf_8[6]*(1+s_6/2)^(-2*(t_8-0.5*2)) - cf_8[7]*(1+s_7/2)^(-2*(t_8-0.5*1)) - cf_8[8]*(1+x/2)^(-2*t_8)
  s_8 = uniroot(sf_8,c(0,1))$root
  
  t_9 = as.numeric(year_frac[i,9])
  sf_9 = function(x) as.numeric(dp[i,9]) - cf_9[1]*(1+s_1/2)^(-2*(t_9-0.5*8)) - cf_9[2]*(1+s_2/2)^(-2*(t_9-0.5*7)) - cf_9[3]*(1+s_3/2)^(-2*(t_9-0.5*6)) - cf_9[4]*(1+s_4/2)^(-2*(t_9-0.5*5)) - 
    cf_9[5]*(1+s_5/2)^(-2*(t_9-0.5*4)) - cf_9[6]*(1+s_6/2)^(-2*(t_9-0.5*3)) - cf_9[7]*(1+s_7/2)^(-2*(t_9-0.5*2)) - cf_9[8]*(1+s_8/2)^(-2*(t_9-0.5*1))  - 
    cf_9[9]*(1+x/2)^(-2*t_9)
  s_9 = uniroot(sf_9,c(0,1))$root
  
  t_10 = as.numeric(year_frac[i,10])
  sf_10 = function(x) as.numeric(dp[i,10]) - cf_10[1]*(1+s_1/2)^(-2*(t_10-0.5*9)) - cf_10[2]*(1+s_2/2)^(-2*(t_10-0.5*8)) - cf_10[3]*(1+s_3/2)^(-2*(t_10-0.5*7)) - cf_10[4]*(1+s_4/2)^(-2*(t_10-0.5*6)) - 
    cf_10[5]*(1+s_5/2)^(-2*(t_10-0.5*5)) -  cf_10[6]*(1+s_6/2)^(-2*(t_10-0.5*4)) - cf_10[7]*(1+s_7/2)^(-2*(t_10-0.5*3)) - cf_10[8]*(1+s_8/2)^(-2*(t_10-0.5*2)) - 
    cf_10[9]*(1+s_9/2)^(-2*(t_10-0.5*1)) - cf_10[10]*(1+x/2)^(-2*t_10)
  s_10 = uniroot(sf_10,c(0,1))$root
  
  t_11 = as.numeric(year_frac[i,11])
  sf_11 = function(x) as.numeric(dp[i,11]) - cf_11[1]*(1+s_1/2)^(-2*(t_11-0.5*10)) - cf_11[2]*(1+s_2/2)^(-2*(t_11-0.5*9)) - cf_11[3]*(1+s_3/2)^(-2*(t_11-0.5*8)) - cf_11[4]*(1+s_4/2)^(-2*(t_11-0.5*7)) - 
    cf_11[5]*(1+s_5/2)^(-2*(t_11-0.5*6)) - cf_11[6]*(1+s_6/2)^(-2*(t_11-0.5*5)) -  cf_11[7]*(1+s_7/2)^(-2*(t_11-0.5*4)) - cf_11[8]*(1+s_8/2)^(-2*(t_11-0.5*3)) - 
    cf_11[9]*(1+s_9/2)^(-2*(t_11-0.5*2)) - cf_11[10]*(1+s_10/2)^(-2*(t_11-0.5*1)) - cf_11[11]*(1+x/2)^(-2*t_11)
  s_11 = uniroot(sf_11,c(0,1))$root
  
  s = rbind(s_1, s_2, s_3, s_4, s_5, s_6, s_7, s_8, s_9, s_10, s_11)
  raw_spot_matrix[i,] <- s 
}


# estimate spot rate by linear interpolation for bond4 and bond6, with maturity "2022-09-01" and "2023-09-01" respectively

est_spot_b4 <- c()
est_spot_b6 <- c()

for (j in 1:10){

  d_ratio_b4 <- (as.numeric(year_frac_md[j,4])-as.numeric(year_frac[j,3]))/(as.numeric(year_frac[j,5])-as.numeric(year_frac_md[j,3]))
est_spot_b4[j] = (as.numeric(raw_spot_matrix[j,5])-as.numeric(raw_spot_matrix[j,3]))*d_ratio_b4 + as.numeric(raw_spot_matrix[j,3])

  d_ratio_b6 <- (as.numeric(year_frac_md[j,6])-as.numeric(year_frac[j,5]))/(as.numeric(year_frac[j,7])-as.numeric(year_frac_md[j,5]))
est_spot_b6[j] = (as.numeric(raw_spot_matrix[j,7])-as.numeric(raw_spot_matrix[j,5]))*d_ratio_b6 + as.numeric(raw_spot_matrix[j,5])
}

spot_matrix_md = raw_spot_matrix

spot_matrix_md[,4] <- est_spot_b4
spot_matrix_md[,6] <- est_spot_b6
spot_matrix_md

# plot spot curve
year<-c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5)
plot(year,spot_matrix_md[1, ], type = "o", main = 'Spot Curve', col = "black", xlab = "Year", ylab = "Spot Rate", ylim = c(0.0007,0.0052),lwd=0.8)
colour = c("red","orange","yellow","sienna","light blue","purple","powderblue","blueviolet", "yellowgreen")
for (i in c(2:10)){ 
  lines(year, spot_matrix_md[i,], type = "o", col=colour[i-1],lwd=0.8)
}

# Add a legend to the plot

legend("topleft",pch=c(15,15),legend=c("2021-01-18", "2021-01-19", "2021-01-20", "2021-01-21" ,"2021-01-22","2021-01-25","2021-01-26","2021-01-27","2021-01-28","2021-01-29"),
       col=c(1,2,3,4,5,6,7,8,9,10),lty=1.3,cex=0.65)


## Question 4(C) :forward curve

# estimate 5 years spot rate on 2021-01-18, 2022-01-18, 2023-01-18, 2024-01-18, 2025-01-18  by linear interpolation

long_t <- c(yearFraction('2021-09-01','2022-03-01'), yearFraction('2022-09-01','2023-03-01'), yearFraction('2023-09-01','2024-03-01'),
            yearFraction('2024-09-01','2025-03-01'), yearFraction('2025-09-01','2026-03-01'))

short_t <- c(yearFraction('2021-09-01','2022-01-29'), yearFraction('2022-09-01','2023-01-29'), yearFraction('2023-09-01','2024-01-29'),
             yearFraction('2024-09-01','2025-01-29'), yearFraction('2025-09-01','2026-01-29'))

a=10
spot_md_sep_d1 <- c(spot_matrix_md[a,][2], spot_matrix_md[a,][4], spot_matrix_md[a,][6], spot_matrix_md[a,][8],spot_matrix_md[a,][10])
spot_md_mar_d1 <- c(spot_matrix_md[a,][3], spot_matrix_md[a,][5], spot_matrix_md[a,][7], spot_matrix_md[a,][9],spot_matrix_md[a,][11])

est_spot_d1 <- c()

for(i in c(1:5)){
  est_spot_d1[i] = spot_md_sep_d1[i]+ (spot_md_mar_d1[i] - spot_md_sep_d1[i]) * (short_t[i]/long_t[i])
}
est_spot_d1

est_spot_d1 <- c(0.001060246, 0.001850573, 0.002485975, 0.003586256, 0.004780742)
est_spot_d2 <- c(0.001114392, 0.001843733, 0.002469326, 0.003576229, 0.004805797)
est_spot_d3 <- c(0.001277369, 0.002013494, 0.002585780, 0.003608399, 0.004835185)
est_spot_d4 <- c(0.001334650, 0.002048354, 0.002631623, 0.003794378, 0.005034957)
est_spot_d5 <- c(0.001287526, 0.001979152, 0.002551964, 0.003703785, 0.004937044)
est_spot_d6 <- c(0.001427309, 0.002073065, 0.002605196, 0.003506598, 0.004696981)
est_spot_d7 <- c(0.001415268, 0.002056269, 0.002466645, 0.003580921, 0.004734850)
est_spot_d8 <- c(0.001223382, 0.001893326, 0.002321359, 0.003408388, 0.004538594)
est_spot_d9 <- c(0.001241752, 0.001832237, 0.002389220, 0.003546891, 0.004685744)
est_spot_d10 <- c(0.001228716, 0.001902015, 0.002409793, 0.003623796, 0.004780374)

est_spot_5years <- rbind(est_spot_d1, est_spot_d2, est_spot_d3, est_spot_d4, est_spot_d5, est_spot_d6, est_spot_d7, est_spot_d8, est_spot_d9, est_spot_d10)


# calculate 4 years forward rate 1y1y, 1y2y, 1y3y, 1y4y 

fwd_matrix = matrix(nrow = 10, ncol = 4)

for(j in c(1:10)){
  for(i in c(1:4)){
    ff = function(x) ((1+est_spot_5years[j,1]/2)^2)*((1+x/2)^(2*i)) - (1+est_spot_5years[j,i+1])^(2*(i+1))
    fwd_matrix[j,i] <- uniroot(ff,c(0,1))$root
  }
}

# plot forward curve
year <- c(2,3,4,5)
plot(year,fwd_matrix[1, ], type = "o", main = 'Forward Curve', col = "black", xlab = "Year", ylab = "Forward Rate", ylim = c(0.005,0.015),lwd=0.8)
colour = c("red","orange","yellow","sienna","light blue","purple","powderblue","blueviolet", "yellowgreen")
for (i in c(2:10)){ 
  lines(year, fwd_matrix[i,], type = "o", col=colour[i-1],lwd=0.8)
}

# Add a legend to the plot
legend("topleft",pch=c(15,15),legend=c("2021-01-18", "2021-01-19", "2021-01-20", "2021-01-21" ,"2021-01-22","2021-01-25","2021-01-26","2021-01-27","2021-01-28","2021-01-29"),
       col=c(1,2,3,4,5,6,7,8,9,10),lty=1.3,cex=0.6)


## Question 5 : cov matrix for ytm and forward rate

## (a) cov matrix for ytm

# calculate YTM for 5 years by the liner interpolation

exact_ytm_matrix = matrix(nrow = 10, ncol = 5)

for (v in c(1:10)){
  exact_ytm_matrix[v,1] = est_spot_5years[v,1]

  for (i in c(2,3,4,5)) {
    exact_ytm_matrix[v,i] <- as.numeric(ytm_matrix_md[v,i*2]) + (as.numeric(ytm_matrix_md[v,(i*2)+1]) - as.numeric(ytm_matrix_md[v,i*2])) * short_t[i] / long_t[i]
  }
}
exact_ytm_matrix

# log return of ytm
log_ytm_matrix <- matrix(nrow = 9, ncol = 5)

for (i in c(1:5)) {
  for(j in c(1:9)){
    
  log_ytm_matrix[j,i] <- log(exact_ytm_matrix[(j+1),i]/exact_ytm_matrix[j,i])
  }
}
log_ytm_matrix

# covariance matrix for ytm
ytm_cov <- cov(log_ytm_matrix,log_ytm_matrix)
print(ytm_cov)

# eigenvalues and eigenvectors for ytm cov matrix
print(eigen(ytm_cov)$values)
print(eigen(ytm_cov)$vectors)

## (b) cov matrix for forward rate

# log return of forward 

log_fwd_matrix <- matrix(nrow = 9, ncol = 4)

for (i in c(1:4)) {
  for(j in c(1:9)){
    log_fwd_matrix[j, i] <- log(fwd_matrix[(j+1),i]/fwd_matrix[j,i])
  }
}
log_fwd_matrix

# covariance matrix for forward rate
fwd_cov <- cov(log_fwd_matrix,log_fwd_matrix)
print(fwd_cov)

# eigenvalues and eigenvectors for fwd cov matrix
print(eigen(fwd_cov)$values)
print(eigen(fwd_cov)$vectors)
