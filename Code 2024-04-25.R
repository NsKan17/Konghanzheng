rm( list = ls() )

library(mosaic)
library(lme4)
library(lmerTest)

####################
### COUNTRY CODE ###
####################

data1 = read.csv( "Data/decision.csv" )
data2 = read.csv( "Data/fifa_ranking-2023-07-20.csv" )

name1 = sort( unique( c( data1$home_team, data1$away_team ) ) )
data_temp = data2[,2:3]
data_temp = data_temp[ duplicated( data_temp[,1] ) == FALSE, ]
data_temp = data_temp[ order( data_temp[,1] ), ]
data_temp$country_name1 = NA
for (i in 1:dim(data_temp)[1] ) {
  index = which( substr( data_temp[i,1], 1, 5 ) == substr( name1, 1, 5 ) )
  if ( length(index) == 1 ) data_temp[i,3] = name1[index]  
}

write.csv( data_temp, "Data/countries.csv", row.names = FALSE )
### see countries-manual.csv for more complete data (done manually after write.csv)

#################
### READ DATA ###
#################

### add country code (abrv)

data = read.csv( "Data/decision.csv" )

##################
### CLEAN DATA ###
##################

### add country code (abrv)

data$home_abrv = NA
data$away_abrv = NA

data_name = read.csv( "Data/countries-manual.csv" )

for (i in 1:dim(data)[1] ) {

  index_home = which( data$home_team[i] == data_name$country_name1 )
  index_away = which( data$away_team[i] == data_name$country_name1 )
  
  if ( length(index_home) == 1 ) { data$home_abrv[i] = data_name$country_abrv[index_home] }
  if ( length(index_away) == 1 ) { data$away_abrv[i] = data_name$country_abrv[index_away] }

}

### add rank

data$home_rank = NA
data$away_rank = NA
data_rank = read.csv( "Data/fifa_ranking-2023-07-20.csv" )
temp_rank = paste0( substr( data_rank$rank_date, 1, 8 ), data_rank$country_abrv )
temp_home = paste0( substr( data$date, 1, 8 ), data$home_abrv )
temp_away = paste0( substr( data$date, 1, 8 ), data$away_abrv )

start = min( which( substr( data$date, 1, 4 ) == "1992" ) )

for (i in start:dim(data)[1] ) {

  index_home = which( temp_home[i] == temp_rank )
  index_away = which( temp_away[i] == temp_rank )

  if ( length(index_home) == 1 ) { data$home_rank[i] = data_rank$rank[index_home] }
  if ( length(index_away) == 1 ) { data$away_rank[i] = data_rank$rank[index_away] }

}

####################
### CLEAN DATA 2 ###
####################

data2 = subset( data, home_team == "China PR" | away_team == "China PR" )
head(data2)

date = data2$date
year = as.numeric( substr( data2$date, 1, 4 ) )
month = as.numeric( substr( data2$date, 6, 7 ) )
day = as.numeric( substr( data2$date, 9, 10 ) )

opponent = ifelse( data2$home_team == "China PR", 
                   data2$away_team,
                   data2$home_team )

opponent_abrv = ifelse( data2$home_team == "China PR", 
                        data2$away_abrv,
                        data2$home_abrv )

rank = ifelse( data2$home_team == "China PR", 
               data2$home_rank,
               data2$away_rank )

opponent_rank = ifelse( data2$home_team == "China PR", 
                        data2$away_rank,
                        data2$home_rank )

match_type = data2$tournament
city = data2$city
country = data2$country
neutral = as.numeric(data2$neutral)
home = ifelse( data2$country == "China PR", 1, 0 )

GS = ifelse( data2$home_team == "China PR", 
             data2$home_score,
             data2$away_score )

GA = ifelse( data2$home_team == "China PR", 
             data2$away_score,
             data2$home_score )

GD = ifelse( data2$home_team == "China PR", 
             data2$home_score - data2$away_score,
             data2$away_score - data2$home_score )

win = ifelse( GD > 0, 1, 0 )

data_clean = data.frame( date, year, month, day, rank, match_type, 
                         opponent, opponent_abrv, opponent_rank,
                         city, country, neutral, home,
                         GS, GA, GD, win )

write.csv( data_clean, "Data/data_cleaned.csv" )

#############################################################

n = 20
set.seed(123)
ranef_sim = runif(n, -0.5, 0.5)
country = letters[1:n]
ranef_sim= sort(ranef_sim, decreasing = FALSE)
ranef_sim
data_sim= data.frame(country, 
                     ranef_sim= sort(ranef_sim, decreasing = FALSE))

plot(1, 1, col="white", axes = FALSE,
     xlim= c(-1,1), ylim = c(0,n+1),
     xlab = "Random effect", ylab = "")

axis(1,seq(-1, 1, 0.5)) ### 1 means x axis
abline (v=0, col=1, lty=1)

i=1
temp= data_sim$ranef_sim[i]
x_temp = c(0, temp)
y_temp= c(i,i)
lines( x_temp, y_temp, lwd = 2)

x_temp = ifelse (temp < 0, temp - 0.1, temp+ 0.1)
y_temp = i
con_temp= data_sim$country[i]
text(x = x_temp, y= y_temp, label= con_temp,
     font = 2, cex = 0.8)

for(i in 2:n ){
  temp= data_sim$ranef_sim[i]
  x_temp = c(0, temp)
  y_temp= c(i,i)
  lines( x_temp, y_temp, lwd = 2)
  
  x_temp = ifelse (temp < 0, temp - 0.1, temp+ 0.1)
  y_temp = i
  con_temp= data_sim$country[i]
  text(x = x_temp, y= y_temp, label= con_temp,
       font = 2, cex = 0.8)
}