rm( list=ls())


data = read.csv( "Data/data_cleaned.csv" ) 
head(data)
data1 = subset( data, year == 1992 & month == 12 )
data2 = subset( data, year >= 1993 )
data3 = rbind( data1, data2 )
head(data3)
data3$julian = julian( as.Date(data3$date),
                       origin = as.Date("1992-12-31") )


data4 = read.csv( "Data/fifa_ranking-2023-07-20.csv" )
data4$julian = julian( as.Date(data4$rank_date),
                       origin = as.Date("1992-12-31") )
unique( substr( data4$rank_date, 1, 7 ) ) ### explains why we have NA

u = unique(data3$opponent_abrv)
u= u[is.na(u)== FALSE]

# i = 1 ### country index

#data3_i = subset( data3, opponent_abrv == u[i] ) ### games with a certian opponents 
#data4_i = subset( data4, country_abrv == u[i] ) ###  Opponent's fifa ranking

data_new =data.frame()



for(i in 1:length(u)){
  data3_i = subset( data3, opponent_abrv == u[i] )
  data4_i = subset( data4, country_abrv == u[i] )
  n_games = dim(data3_i)[1]
  
  for (j in 1:n_games ) { ### game index
  
    #day_diff_abs = abs( data4_i$julian - data3_i$julian[j] )
    day_diff = data4_i$julian - data3_i$julian[j]
    pos_day_diff <- which(day_diff>=0)
    index = min(pos_day_diff)
    data3_i[j,"opponent_rank"] = data4_i[index,"rank"]
  }
  data_new=rbind(data_new, data3_i)
}
data3_c = data_new
data4_c =subset(data4, country_abrv== "CHN")
num1_game <- dim(data3_c)[1]
for(k in 1:num1_game){
  #day_diff_abs = abs(data4_c$julian - data3_c$julian[k])
  day_diff_c = data4_c$julian - data3_c$julian[k]
  pos_day_diff_c <- which(day_diff_c>=0)
  index_c = min(pos_day_diff_c)
  data3_c[k,"rank"] = data4_c[index_c,"rank"]
}
data_new<- data3_c
library(Matrix)
library(lme4)

fit = glm(win~ opponent_rank +rank+ home,
             data= data_new,
             family= binomial)

fit2 = glmer(win~ I(opponent_rank/200) + I(rank/200)+ home + (1 | opponent_abrv),
          data= data_new,
          family= binomial)
summary(fit2)


rslt= round(ranef(fit2)$opponent_abrv, 8)
rownames(rslt)
o_index= order(rslt[,1], decreasing = FALSE)
rslt2 = data.frame(country= rownames(rslt)[o_index],
                   ranef=as.numeric( rslt[,1])[o_index])

rslt2

plot(GD~opponent_rank, data= data_new, pch=19 )
fit = lm(GD~opponent_rank, data= data_new)
#abline( fit, col = 2 )

x_temp= data_new$GD
y_temp= data_new$opponent_rank
lines( smooth.spline( y_temp, x_temp, df = 4 ), col = "blue" )



temp <- by(data_new$win =="1", data_new$opponent_abrv, mean )
x.temp = as.numeric(by(data_new$opponent_rank, data_new$opponent_abrv,
                       mean))
y.temp = as.numeric(temp)
n.temp = as.numeric( table(data_new$opponent_abrv) )
plot( x.temp, y.temp, pch = 19, cex = n.temp/5,
      main = "China's Observed Probability vs opponent's rank",
      xlab = "Opponent Rank",
      ylab = "Probability of winning" )
fit = glm( y.temp ~ x.temp, family = "binomial", weights = n.temp )
summary(fit)
beta0 = as.numeric( coef(fit)[1] )
beta1 = as.numeric( coef(fit)[2] )
x.value = seq( 0, 200, 0.1 )
mu = beta0 + beta1 * x.value
prob = exp(mu) / ( 1 + exp(mu) )
lines( x.value, prob, col = 2 )


plot(1, 1, col = "white", axes = FALSE,
     xlim = c(-2, 2), ylim = c(0, 76),
     xlab = "Random Effect", ylab = "")
axis(1, seq(-0.5, .5, 0.1))
abline(v = 0, col = 1, lty = 1)

rslt2
nran <- length(rslt2$ranef)
nran
for (i in 1:nran) {
  rantemp <- rslt2$ranef[i]
  x_rantemp <- c(0, rantemp)
  y_rantemp <- c(i + .5, i + .5)
  lines(x_rantemp, y_rantemp, lwd = 2)
  x_rantemp <-  ifelse(rantemp < 0, rantemp - .15, rantemp + .15)
  y_rantemp <- i + .5
  con_tempran <- rslt2$country[i]
  text(x = x_rantemp, y = y_rantemp, label = con_tempran,
       font = 2, cex = 0.01)
}

## do 2 grpahs goal difference and opponent rank with line
# second graph is pobability of winning with opponent rank logistical model
# do literature review!!! (google Scholar)
#Study logistic regression 
#do same thing for japan 


#########################################################
japan <- read.csv( "Data/data_cleaned_japan.csv" )
japan1 = subset( japan, year == 1992 & month == 12 )
japan2 = subset( japan, year >= 1993 )
japan3 = rbind( japan1, japan2 )

head(japan3)
japan3$julian = julian( as.Date(japan3$date),
                       origin = as.Date("1992-12-31") )

japan4 = read.csv( "Data/fifa_ranking-2023-07-20.csv" )
japan4$julian = julian( as.Date(japan4$rank_date),
                       origin = as.Date("1992-12-31") )
unique( substr( japan4$rank_date, 1, 7 ) )

a = unique(japan3$opponent_abrv)
a = a[is.na(a)== FALSE]

japan_new= data.frame()

for(i in 1:length(a)){
  japan3_i = subset( japan3, opponent_abrv == a[i] )
  japan4_i = subset( japan4, country_abrv == a[i] )
  n_games = dim(japan3_i)[1]
  
  for (j in 1:n_games ) { ### game index
    
    #day_diff_abs = abs( japan4_i$julian - japan3_i$julian[j] )
    day_diff_j = japan4_i$julian - japan3_i$julian[j]
    pos_day_diff_j <- which(day_diff_j>=0)
    index_j = which.min(pos_day_diff_j)
    japan3_i[j,"opponent_rank"] = japan4_i[index_j,"rank"]
  }
  japan_new=rbind(japan_new, japan3_i)
}
japan3_c = japan_new
japan4_c =subset(japan4, country_abrv== "JPN")
num1_game <- dim(japan3_c)[1]
for(k in 1:num1_game){
  #day_diff_abs = abs(japan4_c$julian - japan3_c$julian[k])
  day_diff_j2 = japan4_c$julian - japan3_c$julian[k]
  pos_day_diff_j2 <- which(day_diff_j2>=0)
  index_j2 = min(pos_day_diff_j2)
  japan3_c[k,"rank"] = japan4_c[index_j2,"rank"]
}
japan_new<- japan3_c

library(Matrix)
library(lme4)

japan_fit = glm(win~ opponent_rank +rank+ home,
          data= japan_new,
          family= binomial)
summary(fit)
japan_fit2 = glmer(win~ I(opponent_rank/200) + I(rank/200)+ home + (1 | opponent_abrv),
             data= japan_new,
             family= binomial)
summary(fit2)
japan_rslt= round(ranef(japan_fit2)$opponent_abrv, 8)
japan_index <- order(japan_rslt[,1], decreasing = FALSE)
japan_rslt2 <- data.frame(country = rownames(japan_rslt)[japan_index],
                        ranef = as.numeric(japan_rslt[,1])[japan_index])
rownames(rslt)
japan_rslt2

plot(GD~opponent_rank, data= japan_new, pch=19 )
fit = lm(GD~opponent_rank, data= japan_new)
japan_x_temp= japan_new$GD
japan_y_temp= japan_new$opponent_rank
lines( smooth.spline( japan_y_temp, japan_x_temp, df = 4 ), col = "blue" )

japan.temp <- by(japan_new$win =="1", japan_new$opponent_abrv, mean )
japan.x.temp = as.numeric(by(japan_new$opponent_rank, japan_new$opponent_abrv,
                       mean))
japan.y.temp = as.numeric(japan.temp)
japan.n.temp = as.numeric( table(japan_new$opponent_abrv) )
plot( japan.x.temp, japan.y.temp, pch = 19, cex = japan.n.temp/15,
      main = "Japan Observed Probability vs opponent's rank",
      xlab = "Opponent Rank",
      ylab = "Probability of winning" )
fit = glm( japan.y.temp ~ japan.x.temp, family = "binomial", weights = japan.n.temp )
#summary(fit)
japan.beta0 = as.numeric( coef(fit)[1] )
japan.beta1 = as.numeric( coef(fit)[2] )
japan.x.value = seq( 0, 200, 0.1 )
japan.mu = japan.beta0 + japan.beta1 * japan.x.value
japan.prob = exp(japan.mu) / ( 1 + exp(japan.mu) )
lines( japan.x.value, japan.prob, col = 2 )




plot(1, 1, col = "white", axes = FALSE,
     xlim = c(-2, 2), ylim = c(0, 76),
     xlab = "Random Effect", ylab = "")
axis(1, seq(-0.5, .5, 0.1))
abline(v = 0, col = 1, lty = 1)


nran <- length(japan_rslt2$ranef)
nran
for (i in 1:nran) {
  rantemp <- japan_rslt2$ranef[i]
  x_rantemp <- c(0, rantemp)
  y_rantemp <- c(i + .5, i + .5)
  lines(x_rantemp, y_rantemp, lwd = 2)
  x_rantemp <-  ifelse(rantemp < 0, rantemp - .15, rantemp + .15)
  y_rantemp <- i + .5
  con_tempran <- japan_rslt2$country[i]
  text(x = x_rantemp, y = y_rantemp, label = con_tempran,
       font = 2, cex = 0.01)
}

#########################################################

data_temp = data.frame()

data1 = data.frame( x = 1, y = 2 )
data_temp = rbind( data_temp, data1 )

data1 = data.frame( x = 3, y = 8 )
data_temp = rbind( data_temp, data1 )

data1 = data.frame( x = 7, y = 2 )
data_temp = rbind( data_temp, data1 )

data1 = data.frame( x = 5, y = 5 )
data_temp = rbind( data_temp, data1 )


 #########################################################
data_temp
data = read.csv( "Data/king.csv" )
head(data)
dim(data)

plot( bwt ~ gest, data = data, pch = 19 )
fit = lm( bwt ~ gest, data = data )
abline( fit, col = 2 )

x.temp = data$gest
y.temp = data$bwt
#lines( smooth.spline( x.temp, y.temp, df = 2 ), col = "blue" ) ### equivalent to line
lines( smooth.spline( x.temp, y.temp, df = 4 ), col = "blue" )
#df is degrees of freedom

head(data)

temp = by( data$low == "Y", data$gest, mean ) ### proportion of low birth weight per gest
x.temp = as.numeric( names(temp) )
y.temp = as.numeric(temp)
n.temp = as.numeric( table(data$gest) )
plot( x.temp, y.temp, pch = 19, cex = n.temp/500,
      main = "Age by Prob of low birth weight graph",
      xlab = "Gestational Age (weeks)",
      ylab = "Probability of Low Birth Weight" )
fit = glm( y.temp ~ x.temp, family = "binomial", weights = n.temp )
summary(fit)
beta0 = as.numeric( coef(fit)[1] )
beta1 = as.numeric( coef(fit)[2] )
x.value = seq( 20, 45, 0.1 )
mu = beta0 + beta1 * x.value
prob = exp(mu) / ( 1 + exp(mu) )
lines( x.value, prob, col = 2 )


# lwd is line width, 2 doubles the width
