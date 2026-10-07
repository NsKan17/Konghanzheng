data1 = read.csv( "Data/decision.csv" )
data2 = read.csv( "Data/fifa_ranking-2023-07-20.csv" )
head(data1)

data1$Match = paste0( data1$home_team, "-", data1$away_team )
head(data1)

i = 1

temp1 = data1$home_team[i]
temp2 = data1$away_team[i]
temp3 = sort( c( temp1, temp2 ) )
temp4 = paste0( temp3[1], "-", temp3[2] )
temp4
