Match<-read.csv('Match.csv')
dim(Match)
Team<-read.csv('Team.csv')
dim(Team)
Team$id<- NULL
Dummy<- merge(Match,Team, by.x='home_team_api_id', by.y='team_api_id')
Dummy<- merge(Dummy,Team, by.x='away_team_api_id', by.y='team_api_id')
write.csv(Dummy,file = 'I:/UTD/Semester 3/Data Viz/Projects/soccer/Dummy.csv')
Player<-read.csv('Player.csv')
dim(Player)
PlayerStats<-read.csv('Player_Stats.csv')
dim(PlayerStats)
Player$id<-NULL
Player$player_fifa_api_id<-NULL
NewPlayer<- merge(PlayerStats,Player,by.x = 'player_api_id',by.y = 'player_api_id')
write.csv(NewPlayer,file = 'I:/UTD/Semester 3/Data Viz/Projects/soccer/NewPlayer.csv')



library("RSQLite")
SoccerCon<-dbConnect(drv = RSQLite::SQLite(),dbname="database.sqlite")
MatchSQL<- dbGetQuery(SoccerCon, 'select * from Match')
PlayerStatsSQL<-dbGetQuery(SoccerCon,'select * from Player_Stats')
PlayerNameSQL<-dbGetQuery(SoccerCon,'select * from Player')
TeamSQL<-dbGetQuery(SoccerCon,'select * from Team')

MatchSQL$date<-as.Date(MatchSQL$date)
MatchSQL$season<-as.factor(MatchSQL$season)
MatchSQL["result"]<-NA
MatchSQL$goaldiff<-MatchSQL$home_team_goal - MatchSQL$away_team_goal
MatchSQL$result<- ifelse(MatchSQL$goaldiff==0,'Draw',ifelse(MatchSQL$goaldiff<0,'Loss',ifelse(MatchSQL$goaldiff>0,'Win',NA)))
MatchSQL$points<-NA
MatchSQL$homepoints<- ifelse(MatchSQL$result=='Win',3,ifelse(MatchSQL$result=='Draw',1,0))
MatchSQL$awaypoints<- ifelse(MatchSQL$result=='Loss',3,ifelse(MatchSQL$result=='Draw',1,0))
df <- subset(MatchSQL,select = c("result","points"))
View(df)
ls()
rm(df)
rm(Dummy,Match,NewPlayer,PlayerStats,PlayerId,Team)
rm(Player)
library(Rserve)
Rserve()
RSshutdown()
library(plyr)
MatchSQL$goal<-NA
MatchSQL$card<-NA
MatchSQL$cross<-NA
MatchSQL$corner<-NA
MatchSQL$possession<-NA
MatchSQL$shoton<-NA
MatchSQL$shotoff<-NA
MatchSQL$foulcommit<-NA
tmpTable = data.frame(Team = TeamSQL$team_api_id, season = 0, Games = 0, Win = 0, Draw = 0, Loss = 0, HomeGames = 0, HomeWin = 0, HomeDraw = 0, HomeLoss = 0, AwayGames = 0, AwayWin = 0, AwayDraw = 0, AwayLoss = 0,Points = 0, HomeFor = 0, HomeAgainst = 0,AwayFor = 0, AwayAgainst = 0, For = 0, Against = 0, GoalDifference = 0)
head(tmpTable)
tmpTable$Team<- 0
Dummy1<- summary(MatchSQL$season)
Dummy1<- NA
Dummy.split<- split(MatchSQL,f=MatchSQL$season)
head(Dummy.split)
rm(Dummy,Dummy.split)

A<-read.csv('Match1.csv')
B<- read.csv('Match1.csv')
dim(Match)
setwd("I:/UTD/Semester 3/Data Viz/Projects/soccer")
B$away_player_1<-NA
B$away_player_2<-NA
B$away_player_3<-NA
B$away_player_4<-NA
B$away_player_5<-NA
B$away_player_6<-NA
B$away_player_7<-NA
B$away_player_8<-NA
B$away_player_9<-NA
B$away_player_10<-NA
B$away_player_11<-NA
B$season<- as.factor(B$season)
B$home_team_api_id<-as.numeric(B$away_team_api_id)
Man2007Home<-B
Man2007Home <- B[ which(B$home_team_api_id==10260 & B$season=='2008/2009'),]
Man2007Away<-A
A$home_player_1<-NA
A$home_player_2<-NA
A$home_player_3<-NA
A$home_player_4<-NA
A$home_player_5<-NA
A$home_player_6<-NA
A$home_player_7<-NA
A$home_player_8<-NA
A$home_player_9<-NA
A$home_player_10<-NA
A$home_player_11<-NA
Man2007Away <- A[ which(A$away_team_api_id==10260 & A$season=='2008/2009'),]

Man2007<- rbind(Man2007Home,Man2007Away)


A<-read.csv('Match1.csv')
B<- read.csv('Match1.csv')
dim(Match)
setwd("I:/UTD/Semester 3/Data Viz/Projects/soccer")
B$away_player_1<-NA
B$away_player_2<-NA
B$away_player_3<-NA
B$away_player_4<-NA
B$away_player_5<-NA
B$away_player_6<-NA
B$away_player_7<-NA
B$away_player_8<-NA
B$away_player_9<-NA
B$away_player_10<-NA
B$away_player_11<-NA
B$season<- as.factor(B$season)
B$home_team_api_id<-as.numeric(B$away_team_api_id)
Man2015Home<-B
Man2015Home <- B[ which(B$home_team_api_id==10260 & B$season=='2015/2016'),]
Man2015Away<-A
A$home_player_1<-NA
A$home_player_2<-NA
A$home_player_3<-NA
A$home_player_4<-NA
A$home_player_5<-NA
A$home_player_6<-NA
A$home_player_7<-NA
A$home_player_8<-NA
A$home_player_9<-NA
A$home_player_10<-NA
A$home_player_11<-NA
Man2015Away <- A[ which(A$away_team_api_id==10260 & A$season=='2015/2016'),]

Man2015<- rbind(Man2015Home,Man2015Away)
write.csv(Man2007,file="I:/UTD/Semester 3/Data Viz/Projects/soccer/Man2007.csv")
write.csv(Man2015,file="I:/UTD/Semester 3/Data Viz/Projects/soccer/Man2015.csv")
ls()
rm(A,B,Man2007,Match,NewMatch,Player,PlayerStats,SoccerCon)

PlayerId<- stack(Man2007, select = c(home_player_1, home_player_2, home_player_3, home_player_4, home_player_5, home_player_6, home_player_7, home_player_8, home_player_9, home_player_10, home_player_11))
PlayerIdUnique <- unique(PlayerId)
PlayerIdUnique<- na.omit(PlayerIdUnique)
nrow(PlayerIdUnique)
