library(rvest)
library(stringr)
library(tidyr)
library(httr)

baseurl = 'http://www.foxsports.com/nfl/players?teamId=0&position=0&country=0&grouping=0&weightclass=0'

for (year in 2014:2015) {    
  for (page in 1:10000) { #max 50 pages?!?
    url = paste(
      baseurl,
      '&page=',
      page,
      '&season=',
      year,
      sep = ''
    )
    webpage = GET(url, add_headers('user-agent' = 'r'))
    webpage = read_html(webpage)
    sb_table <- html_nodes(webpage, 'table')
    sb <- try(html_table(sb_table, fill = TRUE)[[1]], silent=TRUE)
    
    if (class(sb) == "try-error") { # Break condition for when no more players on page
      break
    } else {
      sb$Year = year
      if (page == 1 & year == 2014) {
        df = sb
      } else {
        df = rbind(df, sb)
      }
    }
  }
}

save(df,file='scrapeAllPlayers1415.Rda')
#load('scrapeAllPlayers1415.Rda')
nameChecker = df
nameChecker$Name = substr(nameChecker$Player, 1, regexpr('\r\n',nameChecker$Player)-1)
nameChecker$Name = paste0(
  substring(nameChecker$Name,regexpr(', ',nameChecker$Name)+2)
  ,' ',substr(nameChecker$Name,1,regexpr(',',nameChecker$Name)-1))
names(nameChecker)[3] = 'Position'
nameChecker = convertPositions(nameChecker)
nameChecker = addNameRM(nameChecker)
nameChecker = abbreviateTeams(nameChecker)

nameChecker = addNameRM(nameChecker)

sum(mad$NameRM %in% nameChecker$NameRM)

found = paste0(mad$NameRM,mad$Year) %in% paste0(nameChecker$NameRM,nameChecker$Year)
sum(found)
mad$Name = ifelse(found
                  ,paste ( substring(mad$Name,regexpr(" ",mad$Name)+1), substr(mad$Name,1,regexpr(" ",mad$Name)-1),sep=' ')
                  ,mad$Name)
mad = addNameRM(mad)


write.csv(mad[found,],'foundThem.csv')


#Look at position
found = paste0(Scrape2015$NameRM,Scrape2015$Year,Scrape2015$Week ,Scrape2015$Team) %in% paste0(mad$NameRM,mad$Year,mad$Week,mad$Team)
found2 = paste0(Scrape2015$NameRM,Scrape2015$Year,Scrape2015$Week,Scrape2015$Team, Scrape2015$Position)  %in% paste0(mad$NameRM,mad$Year,mad$Week,mad$Team,mad$Position)
thesePastes = paste0(Scrape2015$NameRM[found & !found2],Scrape2015$Year[found & !found2],Scrape2015$Team[found & !found2])
changeThese = paste0(mad$NameRM,mad$Year,mad$Team) %in% thesePastes

for (i in which(changeThese)) {
  whichOne = which(paste0(Scrape2015$NameRM,Scrape2015$Year,Scrape2015$Week ,Scrape2015$Team) %in% paste0(mad$NameRM[i],mad$Year[i],mad$Week[i],mad$Team[i]))
  
  mad$Position[i] = Scrape2015$Position[whichOne]
}

sum(found)
mad$Name = ifelse(found
                  ,paste ( substring(mad$Name,regexpr(" ",mad$Name)+1), substr(mad$Name,1,regexpr(" ",mad$Name)-1),sep=' ')
                  ,mad$Name)
mad = addNameRM(mad)


write.csv(mad[found,],'foundThem.csv')




