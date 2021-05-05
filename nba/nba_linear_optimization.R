#NBA PLAYER RATING SYSTEM FOR DFS


library(ggplot2)  
library(hablar) 
library(janitor) 
library(lpSolve)
library(sqldf)
library(rvest) 
library(tidyverse) 
library(httr)
library(jsonlite)
library(googlesheets4)
library(tidyr)
library(sqldf)


#the-odds-api.com
odds <- read_sheet("https://docs.google.com/spreadsheets/d/1MSLI36-lYuGQFQHLPWASoOikoZoTswPzV6Y58sZ9CTM/edit#gid=0") # add googlesheet link between quotes " " here
abbrev <-read.csv('./data/nba_abbrev.csv')
odds <- as.data.frame(odds)
df <- separate(data = odds, col = event_name, into = c("team1", "team2"), sep = "\\_")
games1 <- sqldf('select abbrev.abbreviation, df.team1, df.odd_1, df.point_1  from df inner join abbrev on df.team1 = abbrev.[Team.name]')
games2 <- sqldf('select abbrev.abbreviation, df.team2, df.odd_2, df.point_1  from df inner join abbrev on df.team2 = abbrev.[Team.name]')
games <- sqldf('select * from games1 union all select * from games2')
gamesum <- sqldf('select abbreviation, team1,  max(point_1) as total
                 from games 
                 group by abbreviation, team1
                 ')
total <- gamesum %>% mutate_all(~gsub("over ","",.))

#NBA stats per 36 minutes by player
pm <- "https://www.basketball-reference.com/leagues/NBA_2021_per_minute.html" # to expand on this you most likely need more data sources
url <- pm
pobj <- read_html(url, as.data.frame=T, stringsAsFactors = TRUE)
pobj %>%
  html_nodes("table") %>%
  .[[1]] %>%
  html_table(fill=T) -> pm
pm <- clean_names(pm)
names(pm)
get_dupes(pm)
pm <- pm[!(pm$player == "Player"),]
pm <- pm[!(pm$player == ""),]
names(pm)
pm <- pm %>% convert(
  int("mp","g","x3p", "x3pa", "ft", "fta","trb","ast","stl","blk","tov","pts")
)

#rating system --> change this if model isnt working on fanduel 
pm$rating <- ((pm$mp/pm$g)) * (
                                 (pm$trb*1.2)+(pm$ast*1.5)+((pm$stl+pm$blk)*3)+
                                 pm$pts-pm$tov)

#set rating into data frame for join on fanduel data 
pg <- data.frame(Name = pm$player, Rating = pm$rating, g = pm$g, gs = pm$gs, minutesRating = pm$mp/pm$g)

#fan duel lineup data
fd <- read.csv('./data/fd_nba.csv')

#join fanduel data to rating system 
df <- sqldf('select distinct fd.Nickname, fd.Salary, fd.Team, fd.Position, min(pg.Rating+total.total) as Rating, fd.id
            from fd 
            inner join pg on fd.Nickname = pg.Name
            inner join total on fd.Team = rtrim(total.abbreviation)
            where fd.[Injury.Indicator] = ""
            and pg.g > 5 
            group by fd.Nickname, fd.Salary, fd.Team, fd.Position, fd.ID
            ')  

#START OPTIMIZATION
#change variable to appropriate data types
df$Position <- as.factor(df$Position)
df$Salary <- as.numeric(df$salary)
df$Team <- as.factor(df$team)

#prepare the constraint matrix of zeros 
A <- matrix(0, nrow = 37, ncol = nrow(df))

#set parameters
j<-1
i<-1
for (i in 1:nrow(df)){
  if(df$Position[i]=="PG")
    A[j,i]<-1
}
j<-2
i<-1
for (i in 1:nrow(df)){
  if(df$Position[i]=="SG")
    A[j,i]<-1
}
j<-3
i<-1
for (i in 1:nrow(df)){
  if(df$Position[i]=="SF")
    A[j,i]<-1
}
j<-4
i<-1
for (i in 1:nrow(df)){
  if(df$Position[i]=="PF")
    A[j,i]<-1
}
j<-5
i<-1
for (i in 1:nrow(df)){
  if(df$Position[i]=="C")
    A[j,i]<-1
}

#salary <= parameter
A[6, ] <- df$Salary 

#teams 
j<-7
i<-1
for (i in 1:nrow(df)){
  if (df$Team[i]=="ATL")
    A[j,i]<-1
}
j<-8
i<-1
for (i in 1:nrow(df)){
  if (df$Team[i]=="BOS")
    A[j,i]<-1
}
j<-9
i<-1
for (i in 1:nrow(df)){
  if (df$Team[i]=="BKN")
    A[j,i]<-1
}
j<-10
i<-1
for (i in 1:nrow(df)){
  if (df$Team[i]=="BKN")
    A[j,i]<-1
}
j<-11
i<-1
for (i in 1:nrow(df)){
  if (df$Team[i]=="CHA")
    A[j,i]<-1
}
j<-12
i<-1
for (i in 1:nrow(df)){
  if (df$Team[i]=="CHI")
    A[j,i]<-1
}
j<-13
i<-1
for (i in 1:nrow(df)){
  if (df$Team[i]=="CLE")
    A[j,i]<-1
}
j<-14
i<-1
for (i in 1:nrow(df)){
  if (df$Team[i]=="DAL")
    A[j,i]<-1
}
j<-15
i<-1
for (i in 1:nrow(df)){
  if (df$Team[i]=="DEN")
    A[j,i]<-1
}
j<-16
i<-1
for (i in 1:nrow(df)){
  if (df$Team[i]=="DET")
    A[j,i]<-1
}
j<-17
i<-1
for (i in 1:nrow(df)){
  if (df$Team[i]=="GS")
    A[j,i]<-1
}
j<-18
i<-1
for (i in 1:nrow(df)){
  if (df$Team[i]=="HOU")
    A[j,i]<-1
}
j<-19
i<-1
for (i in 1:nrow(df)){
  if (df$Team[i]=="IND")
    A[j,i]<-1
}
j<-20
i<-1
for (i in 1:nrow(df)){
  if (df$Team[i]=="LAC")
    A[j,i]<-1
}
j<-21
i<-1
for (i in 1:nrow(df)){
  if (df$Team[i]=="LAL")
    A[j,i]<-1
}
j<-22
i<-1
for (i in 1:nrow(df)){
  if (df$Team[i]=="MEM")
    A[j,i]<-1
}
j<-23
i<-1
for (i in 1:nrow(df)){
  if (df$Team[i]=="MIA")
    A[j,i]<-1
}
j<-24
i<-1
for (i in 1:nrow(df)){
  if (df$Team[i]=="MIL")
    A[j,i]<-1
}
j<-25
i<-1
for (i in 1:nrow(df)){
  if (df$Team[i]=="MIN")
    A[j,i]<-1
}
j<-26
i<-1
for (i in 1:nrow(df)){
  if (df$Team[i]=="NO")
    A[j,i]<-1
}
j<-27
i<-1
for (i in 1:nrow(df)){
  if (df$Team[i]=="NYK")
    A[j,i]<-1
}
j<-28
i<-1
for (i in 1:nrow(df)){
  if (df$Team[i]=="OKC")
    A[j,i]<-1
}
j<-29
i<-1
for (i in 1:nrow(df)){
  if (df$Team[i]=="ORL")
    A[j,i]<-1
}
j<-30
i<-1
for (i in 1:nrow(df)){
  if (df$Team[i]=="PHI")
    A[j,i]<-1
}
j<-31
i<-1
for (i in 1:nrow(df)){
  if (df$Team[i]=="PHX")
    A[j,i]<-1
}
j<-32
i<-1
for (i in 1:nrow(df)){
  if (df$Team[i]=="POR")
    A[j,i]<-1
}
j<-33
i<-1
for (i in 1:nrow(df)){
  if (df$Team[i]=="SAC")
    A[j,i]<-1
}
j<-34
i<-1
for (i in 1:nrow(df)){
  if (df$Team[i]=="SAS")
    A[j,i]<-1
}
j<-35
i<-1
for (i in 1:nrow(df)){
  if (df$Team[i]=="TOR")
    A[j,i]<-1
}
j<-36
i<-1
for (i in 1:nrow(df)){
  if (df$Team[i]=="UTA")
    A[j,i]<-1
}
j<-37
i<-1
for (i in 1:nrow(df)){
  if (df$Team[i]=="WAS")
    A[j,i]<-1
}
#prepare input for LP solver + set constraints
objective.in <- df$Rating
const.mat <- A
const.dir <- c("==","==","==","==","==","<=", "<=","<=","<=","<=","<=","<=","<=","<=","<=","<=","<=","<=","<=","<=","<=","<=","<=","<=","<=","<=","<=","<=","<=","<=","<=","<=","<=","<=","<=","<=","<=")
const.rhs <- c(2, 2, 2, 2, 1, 60000, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2) 

#generate optimal lineup with lp solve
require(lpSolve)
sol <- lp(direction = "max", objective.in, 
          const.mat, const.dir, const.rhs,
          all.bin = TRUE)

inds <- which(sol$solution == 1)
sum(df$Salary[inds])

solution <- df[inds, ]


solution
#end - use solution to set the optimum lineup for DFS NBA


