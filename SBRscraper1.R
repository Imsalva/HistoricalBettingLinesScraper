#SBreview scraper
library(gsubfn)
library(stringr)

bothDates <- readLines(con=stdin(), 6)
startYear <- bothDates[1]
startMonth <- bothDates[2]
startDay <- bothDates[3]
closeYear <- bothDates[4]
closeMonth <- bothDates[5]
closeDay <- bothDates[6]
startDate <- as.Date(paste(startYear,"-",startMonth,"-",startDay, sep=""))
closeDate <- as.Date(paste(closeYear,"-",closeMonth,"-",closeDay,sep=""))
difference <- as.integer(closeDate-startDate+1)
for(i in 1:difference){
  currentDate <- startDate+i-1
  currDateString <- gsub("-","",currentDate)
  oddsURL <- paste("https://classic.sportsbookreview.com/betting-odds/nba-basketball/?date=",currDateString, sep="")
  SBhtml <- read_html(oddsURL)

###########################CLEANING FOR EACH DAILY URL################################################

team_names <- html_text(html_nodes(SBhtml, ".eventLine-value"))
scores <- html_text(html_nodes(SBhtml, ".total"))
FiveDimes <-  html_text(html_nodes(SBhtml, ".eventLine-book:nth-child(11) b"))
bookmaker <- html_text(html_nodes(SBhtml, ".eventLine-book:nth-child(12) b"))
BetOnline <- html_text(html_nodes(SBhtml, ".eventLine-book:nth-child(13) b"))
Bovada <- html_text(html_nodes(SBhtml, ".eventLine-book:nth-child(14) b"))
Heritage <- html_text(html_nodes(SBhtml, ".eventLine-book:nth-child(15) b"))

bookmaker_odds <- odds_cleaner1(bookmaker)
bookmaker_moneyline <- odds_cleaner2(bookmaker)

FiveDimes_odds <- odds_cleaner1(FiveDimes)
FiveDimes_moneyline <- odds_cleaner2(FiveDimes)

BetOnline_odds <- odds_cleaner1(BetOnline)
BetOnline_moneyline <- odds_cleaner2(BetOnline)

Bovada_odds <- odds_cleaner1(Bovada)
Bovada_moneyline <- odds_cleaner2(Bovada)

Heritage_odds <- odds_cleaner1(Heritage)
Heritage_moneyline <- odds_cleaner2(Heritage)

if(i == 1){
day <- as.data.frame(cbind(team_names, scores, bookmaker_odds, bookmaker_moneyline, FiveDimes_odds, 
             FiveDimes_moneyline, BetOnline_odds, BetOnline_moneyline, Bovada_odds, Bovada_moneyline,
             Heritage_odds, Heritage_moneyline, currDateString))
}else{
day <- rbind(day, as.data.frame(cbind(team_names, scores, bookmaker_odds, bookmaker_moneyline, FiveDimes_odds, 
                                FiveDimes_moneyline, BetOnline_odds, BetOnline_moneyline, Bovada_odds, Bovada_moneyline,
                                Heritage_odds, Heritage_moneyline, currDateString)))
}
}
###################HELPER FUNCTIONS##################################

odds_cleaner1 <- function(raw_odds){
  odds <- c()
  for(i in 1:length(raw_odds)){
    a <-  stri_trans_general(str_split(raw_odds[i], "\\s")[[1]][1], "latin-ascii")
    odds[i] <- improp_frac_convert(strapplyc(gsub('\"',"",a), "\\+|-|\\d+"))
  }
  return(odds)
}
odds_cleaner2 <- function(raw_odds){
  a <- c()
  for(i in 1:length(raw_odds)){
    a[i] <-  as.numeric(stri_trans_general(str_split(raw_odds[i], "\\s")[[1]][2], "latin-ascii"))
  }
  return(a)
}

improp_frac_convert <- function(lst){
  if(length(lst[[1]]) == 2){
    x = as.numeric(lst[[1]][2])
  }
  else{
    x <- as.numeric(lst[[1]][2]) + as.numeric(lst[[1]][3]) / as.numeric(lst[[1]][4])
  }
  if(lst[[1]][1] == "-"){
    return(-1*x)
  }
  else return(x)
}

