library(openxlsx)

source('functions.R')

df = data.frame(Player = as.character(), Team = as.character(), Position = as.character(), Depth = as.character(), Overall = as.character(), OverallChange = as.character(), Other = as.character(),Year = as.integer(), file = as.character())
for( year in 2014:2015) {
  
  path = paste0('./maddenData/',year,'/')
  
  temp = list.files(path = path, pattern="*.xlsx")
  weeks = grep('week',temp)
  for (i in weeks) {
    df.temp = read.xlsx(paste0(path,temp[i]))
    df.temp$file = paste0(path,temp[i])
    df.temp$Year = year
    df = rbind(df, df.temp)
  }
}
#fix some Positions
df$Traded = ifelse(grepl('Traded',df$Team),TRUE,FALSE)
df$Released = ifelse(grepl('Released',df$Team),TRUE,FALSE)
df$New = ifelse(grepl('\\(New',df$Team),TRUE,FALSE)
df$Signed = ifelse(grepl('Signed',df$Team),TRUE,FALSE)

df$Position = ifelse(grepl(' \\(',df$Position),substr(df$Position,1,regexpr(' \\(',df$Position)-2),df$Position)
df$Position = ifelse(df$Position %in% c('','D','F','H','KR','L','R','T','N','ML','W','LOL'), substr(df$Depth,1,regexpr(':',df$Depth)-1), df$Position)
df$Position = ifelse(grepl('None \\(Was ',df$Position),sub('None \\(Was ','',df$Position),df$Position)

#convert position
df = convertPositions(df)
#remove blank positions
df = df[-which(df$Position==''),]




# convert Player to Names -------------------------------------------------
df$Name = sub('\t','',df$Player)
df$Name = substring(df$Name,1,regexpr(' \\(',df$Name)-1)

#check name

checkName = addNameRM(df)
checkName$Name2 = checkName$Name
checkName$NameRM2 = checkName$NameRM
checkName$Name = paste(substring(checkName$Name,regexpr(' ',checkName$Name)+1), substr(checkName$Name,1,regexpr(' ',checkName$Name)-1), sep=' ')
checkName = addNameRM(checkName)


# Deal with Teams ---------------------------------------------------------
df$Team = ifelse(grepl(' \\(',df$Team),substr(df$Team,1,regexpr(' \\(',df$Team)-2),df$Team)
df$Team = ifelse(grepl('Released by',df$Team),'',df$Team)
df = df[-which(df$Team==''),]
df$Team = ifelse(df$Team=='Free Agen','Free Agent',df$Team)
df$Team = sub('ss','s',paste0(df$Team,'s'))
df = abbreviateTeams(df)


# Rest --------------------------------------------------------------------

df$Week = as.integer(substr(df$file,regexpr('_week_',df$file)+6,regexpr('\\.',substring(df$file,2))))
table(df$Week,df$Year)



df = addNameRM(df) 
df = abbreviateTeams(df)
df = createKey(df)
df$Overall = as.integer(df$Overall)
str(df)

#df = temp.df
temp.df=df

df = df[,c('Key','Year','Week','Team','Name','Position','Overall','file','Traded','Released','New','Signed')]

for (year in 2014:2015) {
  path = paste0('./maddenData/',year,'/')
  
  temp = list.files(path = path, pattern="*.xlsx")
  weeks = grep('week',temp)
  init = read.xlsx(paste0(path,temp[-weeks]))
  init$Name = paste(init$First.Name,init$Last.Name,sep=' ')
  init$Overall = ifelse('Overall' %in% names(init), init$Overall, init$OVR)
  init$file = paste0(path,temp[-weeks])
  init$Year = year
  init$Week = 0
  init$Depth = ''
  init$OverallChange = ''
  init$Other = ''
  init$Traded=FALSE
  init$New = FALSE
  init$Released = FALSE
  init$Signed = FALSE
  init = convertPositions(init)
  init = addNameRM(init)
  init = abbreviateTeams(init)
  init = createKey(init)
  
  df = rbind(df,init[,c('Key','Year','Week','Team','Name','Position','Overall','file','Traded','Released','New','Signed')])
  
  for (week in 1:17) {
    temp1 = df[df$Year == year & df$Week == week,]
    temp0 = df[df$Year == year & df$Week == (week-1),]
    missing = !(paste0(temp0[,'Key'],temp0[,'Team']) %in% paste0(temp1[,'Key'],temp1[,'Team']))
    exclusion = !(temp0[,'Key'] %in% 
                    temp1[temp1$Traded | temp1$Released | temp1$New | temp1$Signed ,'Key'])
    
    temp.replace = temp0[missing & exclusion,]     
    temp.replace$Week = week
    df = rbind(df,temp.replace)
    
          
    # CODE IN INJURIES?
  }
}
sum(table(df[df$Year==2014&df$Week==17&df$Team=='CHI','Position']))
df = df[order(df$Team,df$Key),]
df$Overall = as.integer(df$Overall)
mad = df
mad = addNameRM(mad)

table(mad$Week, mad$Rank)


mad2=mad[,c('Year','Week','Key','Overall')]
mad2$row = 1:length(mad2$Key)
mad2 = mad2[order(mad2$Year,mad2$Week,mad2$Key,-mad2$Overall),]
str(mad2)
for (i in length(mad2$row):2) {
  if (paste0( mad2$Year[i],mad2$Week[i], mad2$Key[i] )
      == paste0( mad2$Year[i-1],mad2$Week[i-1], mad2$Key[i-1] ) 
      & mad2$Overall[i] <= mad2$Overall[i-1] ) { mad2 = mad2[-i,] }
}
str(mad2)

mad = mad[mad2$row,]
mad$Rank = ave(-mad$Overall, mad$Team, mad$Position, mad$Week, mad$Year, FUN=function(x) rank(x, ties.method="random"))
mad$Depth = paste0(mad$Position,mad$Rank)

a.needs = data.frame(position=c('QB','WR','RB','OL','TE','DL','LB','CB','FS','SS','K'), need=c(1,3,2,5,1,4,3,3,1,1,1), stringsAsFactors = FALSE)
teams = data.frame(Opponent=as.character(unique(mad$Team)), stringsAsFactors=FALSE)
teams = data.frame(Opponent=as.character(teams[which(teams$Opponent!='Free Agents'),]), stringsAsFactors=FALSE)
teams = expand.grid(Opponent=teams$Opponent,Year=2014:2015,Week=1:17, stringsAsFactors = FALSE,  KEEP.OUT.ATTRS = FALSE)

## LOOK INTO LACK OF KICKERS
for (i in 1:length(a.needs$position)) {
  for (k in 1:length(teams$Opponent)) {
    # Check for Free Safety, replace with CB3
    if (length(which(mad$Team == teams$Opponent[k] 
                     & mad$Rank==1 
                     & mad$Position == a.needs$position[i]
                     & mad$Year == teams$Year[k]
                     & mad$Week == teams$Week[k])) == 0 ) {
      replacement=0
      if (a.needs$position[i] == 'FS') {
        replacement = which(mad$Team == teams$Opponent[k] 
                            & mad$Rank==2
                            & mad$Position == 'SS'
                            & mad$Year == teams$Year[k]
                            & mad$Week == teams$Week[k])
        
        mad$Position[replacement] = 'FS'
        mad$Rank[replacement] = 1
        print(paste(i,k,teams$Year[k],teams$Week[k],teams$Opponent[k],a.needs$position[i],replacement,mad$Position[replacement],mad$Key[replacement],sep=','))
      }
      if (a.needs$position[i] == 'K') {
        replacement = which(mad$Team == teams$Opponent[k] 
                            & mad$Rank==1
                            & mad$Position == 'P'
                            & mad$Year == teams$Year[k]
                            & mad$Week == teams$Week[k])
        mad$Position[replacement]='K'
        mad$Overall[replacement] = mad$Overall[replacement]*.7
        print(paste(i,k,teams$Year[k],teams$Week[k],teams$Opponent[k],a.needs$position[i],replacement,mad$Position[replacement],mad$Key[replacement],sep=','))
      }
    }
  }
}



for (i in 1:length(a.needs$position)) {
  for (j in 1:a.needs$need[i]) {
    nextcol = ncol(teams)+1
    for (k in 1:length(teams$Opponent)) {
      fromMAD = which(mad$Team == teams$Opponent[k] 
                      & mad$Rank==j 
                      & mad$Position == a.needs$position[i] 
                      & mad$Year == teams$Year[k]
                      & mad$Week == teams$Week[k])[[1]]
      teams[k,nextcol] = mad$Key[fromMAD]
      teams[k,nextcol+1] = mad$Overall[fromMAD]
    }
    names(teams)[c(nextcol,nextcol+1)] = c(paste(a.needs$position[i],j,'Key', sep='.'),paste(a.needs$position[i],j,'ovr', sep='.'))
  }
}

freq = ddply(mad, .(Key), summarize, freq=length(Key))
write.csv(merge(mad,freq,by='Key'),'mad2.csv')

save(mad,file='mad_backup.Rda')
save(teams,file='teams_backup.Rda')


