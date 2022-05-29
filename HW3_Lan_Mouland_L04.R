library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)


########################################
################Question 1#############
########################################

muffin.profit.calc <- function(d,s) {
  if(d > s) {
    d <- s
  }
  p <- (d*2.50 - s*0.2)
  return(p)
}

#1A - 230

muffin.profit.calc(120,100)


#1B

#the decision variable is the supply 

#####       P = 2.50*d - 0.20*s 

#1C - 180

muffin.profit.calc(80,100)

#1D - 155 supply BUT ITS NOT CONSISTENT

## demand sim
N <- 10000
ran.dem <- round(rnorm(N,120,25))
qplot(ran.dem, type = "s", xlab = "demand level", ylab = "volume", col = "red")

###https://www.youtube.com/watch?v=4M5wHzCzUyc

stock.lvl <- seq(1,250,1)
n <- length(stock.lvl)
set.seed(10)
table1 <- data.frame(s = as.numeric(), p = as.numeric(), d = as.numeric())
table2 <- data.frame()
rm <- data.frame()

for(i in 1:1000) {
  for(j in 1:n) {
    supply <- stock.lvl[j]
    table1[j,1] <- supply 
    d.temp <- rnorm(1,120,25)
  }
  for(m in 1:n) {
    if(table1[m,1] <= d.temp) {
      table1[m,3] <- table1[m,1]
    }
    if(table1[m,1] > d.temp){
      table1[m,3] <- d.temp
    }
    table1[m,2] <- (2.50*table1[m,3] - 0.2*table1[m,1] )
  }
  for(p in 1:n){
    table2[p,i] <- table1[p,2]
  }
} 
for(i in 1:n) {
  rm[i,1] <- stock.lvl[i]
  rm[i,2] <- data.frame(r = rowMeans(table2[i,1:1000]))
}
rm[which.max(rm$r),1]


#1E - 0.92

pnorm(155,120,25)

#1F - 11.46
N <- 10000
set.seed(10)
ran.dem <- rnorm(N,120,25)
table11 <- vector()
for(i in 1:N) {
  if(ran.dem[i] > 155) {
    table11[i] <- ran.dem[i]
    table11 <- na.omit(table11)
  }
}
mean(table11)  - 155

#1G - 39
N <- 10000
set.seed(10)
ran.dem <- rnorm(N,120,25)
table12 <- vector()
for(i in 1:N) {
  if(ran.dem[i] < 155) {
    table12[i] <- ran.dem[i]
    table12 <- na.omit(table12)
  }
}
155 - mean(table12) 

#1H ##make plot
#demand uncertainty hurts

s.d. <- seq(0,30,5)
profit <- vector()
stock.lvl <- seq(1,250,1)
n <- length(stock.lvl)
set.seed(10)
table1 <- data.frame(s = as.numeric(), p = as.numeric(), d = as.numeric())
table2 <- data.frame()
rm <- data.frame()

for(a in 1:length(s.d.)) {
  for(i in 1:1000) {
    for(j in 1:n) {
      supply <- stock.lvl[j]
      table1[j,1] <- supply 
      d.temp <- rnorm(1,120,s.d.[a])
    }
    for(m in 1:n) {
      if(table1[m,1] <= d.temp) {
        table1[m,3] <- table1[m,1]
      }
      if(table1[m,1] > d.temp){
        table1[m,3] <- d.temp
      }
      table1[m,2] <- (2.50*table1[m,3] - 0.2*table1[m,1] )
    }
    for(p in 1:n){
      table2[p,i] <- table1[p,2]
    }
  } 
  for(i in 1:n) {
    rm[i,1] <- stock.lvl[i]
    rm[i,2] <- data.frame(r = rowMeans(table2[i,1:1000]))
  }
  profit[a] <- rm[which.max(rm$r),2]
}

plot(s.d.,profit, type = "b", 
     main = "Profit at different S.D.", 
     xlab = "S.D.", 
     ylab = "Mean Max Profit")


#to make the differences more clear I subtracted 260 from the answer to more 
#clearly demonstrate 

barplot((profit-260),
        main = "Profit at different S.D. minus $260",
        xlab = "S.D.",
        ylab = "Mean Max Profit",
        names.arg = c("0", "5", "10", "15", "20", "25", "30"),
        col = "darkblue")

#https://www.datamentor.io/r-programming/bar-plot/

#demand uncertainty hurtsssss

#1I - 55

#if my optimal supply is 155 and I am given 100 free muffins from my supplier 
#I woulds still want to buy 55 muffins to meet the optimal stock level

((2*120*2.50)/0.2)^(1/2)

########################################
################Question 2#############
########################################

url <- "https://s3.amazonaws.com/tripdata/202101-citibike-tripdata.csv.zip"

temp <- tempfile()
download.file(url, temp)
citibike <- read.csv(unz(temp, "202101-citibike-tripdata.csv"),
                     stringsAsFactors = FALSE)
unlink(temp)


#This is me filtering out any missing/stolen bikes

cbt <- citibike %>%
  filter(usertype == "Subscriber" | usertype == "Customer") %>% filter(tripduration <= 86400)

min15 <- 2.50/(15*60)
45*60
min30 <- 3/(30*60)
30*60


#I decided to split up Subs and Cust to more easily figure out the charge for each
#I then add the data back together in a string to then find Mean/SD.
#By initially splitting up the data it was much easier to find how much Citi was making on 
#charging customers by the second. 


table3 <- data.frame(p = as.numeric())
table4 <- data.frame(p = as.numeric())

cbt.sub <- cbt %>% filter(usertype == "Subscriber" & tripduration > 2700)
cbt.sub <- data.frame(cbt.sub[,1])
for(i in 1:nrow(cbt.sub)) {
  table4[i,1] <- (cbt.sub[i,1] - 2700)*min15
}


cbt.cus <- cbt %>% filter(usertype == "Customer" & tripduration > 1800) 
cbt.cus <- data.frame(cbt.cus[,1])
for(i in 1:nrow(cbt.cus)) {
  table3[i,1] <- (cbt.cus[i,1] - 1800)*min30 
}

cbt.cus.sub <- c(table4$p,table3$p)


#2A - 4.00

mean(cbt.cus.sub)

#2B - 14.00

sd(cbt.cus.sub)

#2C - 76000 / 75000
sum(table4$p)

sum(table3$p)


########################################
################Question 2############# part 2
########################################
table5 <- data.frame(p = as.numeric())
table6 <- data.frame(p = as.numeric())

cbt.sub1 <- cbt %>% filter(usertype == "Subscriber" & tripduration > 2700)
cbt.sub1 <- data.frame(cbt.sub1[,1])
for(i in 1:nrow(cbt.sub)) {
  table5[i,1] <- ceiling((((cbt.sub[i,1] - 2700)/60)/15))*2.50
}

cbt.cus1 <- cbt %>% filter(usertype == "Customer" & tripduration > 1800) 
cbt.cus1 <- data.frame(cbt.cus1[,1])
for(i in 1:nrow(cbt.cus)) {
  table6[i,1] <- ceiling((((cbt.cus[i,1] - 1800)/60)/30))*3
}

cbt.cus.sub1 <- c(table5$p,table6$p)



#2D - 5.80
mean(cbt.cus.sub1)

#2E - 13.90
sd(cbt.cus.sub1)

#2F
#incremental

#2G
#yes

#2H - 7.00 / 5.10
mean(table5$p)

mean(table6$p)

########################################
################Question 3#############
########################################

price.dem <- function(p){
  (1-(p/10))*50
}

p <- 0:10

p.d <- matrix(sapply(p,price.dem))
sapply(p,price.dem)
qplot(p,p.d)
mean(p.d)
sd(p.d)

#3A

#THIS IS BINOMINAL 

#3B - 40.00

set.seed(2)
p <- 1-2/10
bi2 <- rbinom(10000000,50,p)

mean(bi2)

#3C - 2.83
sd(bi2)

#3D - 30.00
p <- 1-4/10
set.seed(10)
bi4 <- rbinom(10000000,50,p)


mean(bi4)

#3E - 3.46
sd(bi4)

########################################
################Question 4#############
########################################

#4A - 0.20...... uhhh

0.20

#the expected marginal cost of stocking another muffin, is the cost associated with an increase in the supply level by S. 
#If the cost of buying one muffin is $0.20, then the cost of stocking one additional muffin is $0.20 * S. Therefore 
#the cost of stocking one more muffin is $0.20 * 1, or $0.20


#4B - 2.50 Pr (D > S*)

#Since marginal revenue is a factor of the supply level. The expected marginal revenue should be realized when 
#demand exceeds supply. The reasoning is that if supply exceeds demand, there is no loss on unsold goods 
#and the potential revenue is maximized as a result. 

#4C - 155


tt <- 1:300

d <- pnorm(tt, 120, 25)

tata <- data.frame(tt = as.numeric(), p = as.numeric(), MR = as.numeric(), diff = as.numeric())

for(i in 1:length(tt)) {
  tata[i,1] <- tt[i]
  tata[i,2] <- d[i]
  tata[i,3] <- tata[i,2] *2.50
  tata[i,4] <- 2.50 - tata[i,3]
}
tail(tata[(tata$MR <= 2.3),])

#I am looking for the point that my costs outweighs the revenue. $2.50 - $0.2 = $2.30
#The PR(D > S*)*2.50, when D = 155, is 0.92*2.50 = ~2.30
#if the supply level is 156 then the expected marginal cost would exceed the marginal revenue and would be less 
# than the profit level at 155


#4D

## in the food/beverage industry the higher the marginal cost of product 
##the higher the demand would need to be to make the optimal profit. 
###During Covid if the marginal cost of production increased businesses would need 
#### demand to increase to cover the increase of cost. 
#### however, during covid since people were going out less the service level would decrease
##### since a food/beverage company could not afford to have as many employees working at a given time 

##### E[C*S], E[PR(D > S*)]*P

########################################
################Question 5#############
########################################

#So to cut down on simulation I decided that if I have 10000 tix in a 20B draw pool
#that it is the same 1 tix in a 2000000 draw pool. 
#So i gave myself a random tix number that is used to check how many times i win. 

MyTix <- 351

#every three seconds there is a win

MinW <- 60/3
HourW <- MinW*60
DayW <- HourW*24
WeekW <- DayW*7
YearW <- DayW*365
Year10W <- YearW*10



#5A

#chance of not winning once in a week - 0.90
1-((10000/20000000000)*WeekW)
#chance of willing once in a week - 0.10
(10000/20000000000)*WeekW


#5B - 160000

0.8 * WeekW

#5C
##if i have 10000 tix - 1300
pick <- 1:2000000
table30 <- data.frame("wins" = as.numeric())
set.seed(4)
for(i in 1:YearW) {
  pick.temp <- sample(pick,1)
  if (pick.temp == MyTix) {
    table30[i,1] <- 250
  }
}
table30 <- na.omit(table30)
sum(table30[,1])

#5D

## if i have 100000 tix - 13000
pick <- 1:200000
table30 <- data.frame("wins" = as.numeric())
set.seed(7)
for(i in 1:YearW) {
  pick.temp <- sample(pick,1)
  if (pick.temp == MyTix) {
    table30[i,1] <- 250
  }
}
table30 <- na.omit(table30)
sum(table30[,1])
## if i have 1000000 tix - 130000
pick <- 1:20000
table30 <- data.frame("wins" = as.numeric())
set.seed(4)
for(i in 1:YearW) {
  pick.temp <- sample(pick,1)
  if (pick.temp == MyTix) {
    table30[i,1] <- 250
  }
}
table30 <- na.omit(table30)
sum(table30[,1])




