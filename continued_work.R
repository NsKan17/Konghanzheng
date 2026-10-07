library(mosaic)
library(lme4)
library(lmerTest)

data = read.csv( "Data/data_cleaned_all.csv" ) 
data_new = data[-c(1:127), ]
data_new$date = as.Date(data_new$date,"%m/%d/%Y")

data2 = subset( data_new, home_team == "England" | away_team == "England" )

date = data2$date
year = as.numeric( substr( data2$date, 1, 4 ) )
month = as.numeric( substr( data2$date, 6, 7 ) )
day = as.numeric( substr( data2$date, 9, 10 ) )

opponent = ifelse( data2$home_team == "England", 
                   data2$away_team,
                   data2$home_team )

opponent_abrv = ifelse( data2$home_team == "England", 
                        data2$away_abrv,
                        data2$home_abrv )

rank = ifelse( data2$home_team == "England", 
               data2$home_rank,
               data2$away_rank )

opponent_rank = ifelse( data2$home_team == "England", 
                        data2$away_rank,
                        data2$home_rank )

match_type = data2$tournament
city = data2$city
country = data2$country
neutral = as.numeric(data2$neutral)
home = ifelse( data2$country == "England", 1, 0 )

GS = ifelse( data2$home_team == "England", 
             data2$home_score,
             data2$away_score )

GA = ifelse( data2$home_team == "England", 
             data2$away_score,
             data2$home_score )

GD = ifelse( data2$home_team == "England", 
             data2$home_score - data2$away_score,
             data2$away_score - data2$home_score )

win = ifelse( GD > 0, 1, 0 )

data_clean = data.frame( date, year, month, day, rank, match_type, 
                         opponent, opponent_abrv, opponent_rank,
                         city, country, neutral, home,
                         GS, GA, GD, win )

data1 = subset( data_clean, year == 1992 & month == 12 )
data2 = subset( data_clean, year >= 1993 )
data3 = rbind( data1, data2 )
data3$julian = julian( as.Date(data3$date),
                       origin = as.Date("1993-02-17") )

data4 = read.csv( "Data/fifa_ranking-2023-07-20.csv" )
data4$julian = julian( as.Date(data4$rank_date),
                       origin = as.Date("1993-02-17") )

u = unique(data3$opponent_abrv)
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
u= u[is.na(u)== FALSE]

data3_e = data_new
data4_e =subset(data4, country_abrv== "ENG")

num1_game <- dim(data3_e)[1]
for(k in 1:num1_game){
  day_diff_e = data4_e$julian - data3_e$julian[k]
  pos_day_diff_e <- which(day_diff_e>=0)
  index_e = min(pos_day_diff_e)
  data3_e[k,"rank"] = data4_e[index_e,"rank"]
}
data_new<- data3_e

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

### USA, France
### Change Match name to match name two so Sco-ENG becomes ENG-Sco and add boolean
### logic for home and away games
### use effect of +(1+Match-Name2)
### 1st poster 2nd another country with a strong random effect 3rd above

### objective 2 and some poster draft by next week
### use articles to find another country with strong random effect
