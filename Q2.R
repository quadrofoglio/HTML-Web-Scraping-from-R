library(jsonlite)
library(rlist)
library(dplyr)
library(tidyr)

days   <- c('01','02','03','04','05','06','07')
hrs    <- c(as.character(seq(from = 0, to = 23, length.out = 24)))
minute <- c('30')

for (i in hrs){
  if (i %in% c(as.character(seq(from = 0, to = 9, length.out = 10)))){
    i2 <- as.character(i)
    i2 <- paste('0',i, sep='')
    print(i)
    print(i2)
    hrs[as.integer(i)+1] <- i2
  } else if (i == '10'){
    rm(i)
    rm(i2)
    break
  }
}

d2 <- days
h2 <- hrs
m2 <- minute
links = list()
for (d in d2){
  for(h in h2){
    for(m in m2){
      if((h == '00') & (m == '00')){
        next
      } else {
        link <- paste('https://api.data.gov.sg/v1/transport/carpark-availability?date_time=2020-01-',
                      as.character(d), 'T',as.character(h),'%3A', as.character(m), '%3A00', sep='' )
        links <- list.append(links, link)
      }
    }
  }
}

df <- data.frame()

join <- function(link){
  print(link)
  data <- fromJSON(link)
  a <- as.data.frame(data$items$carpark_data, stringAsFactors = FALSE)
  b <- a[[1]]
  b <- bind_rows(b, .id = "carpark_info")
  a$carpark_info <- NULL
  a <- cbind.data.frame('carpark_info' = rownames(a),a,stringsAsFactors = FALSE)
  new_df <- left_join(b,a, by = 'carpark_info',stringAsFactors = FALSE)
  Sys.sleep(2)
  return(new_df)
}
num_crawled <- 0

for (url in links){
  new <- join(url)
  df <- rbind.data.frame(df, new, stringsAsFactors = FALSE)
  num_crawled <- num_crawled + 1
}

#Format into numeric
df$carpark_info <- as.numeric(df$carpark_info)
df$total_lots <- as.numeric(df$total_lots)
df$lots_available <- as.numeric(df$lots_available)

#Raw Crawl Data export as csv for convenience for cleaning.
setwd("D:/Documents/Uni/MODULES/Y2S2/DBA3702/Lecture Notes/Lecture 5 - Programming & Functions/Assignment 4")
wd <- getwd()
write.csv(df,'Assignment_4_Q2_raw_v1.csv', row.names = FALSE)
raw <- read.csv('Assignment_4_Q2_raw_v1.csv',stringsAsFactors = FALSE)

# Clean raw
raw <- raw %>% na.omit %>% filter(total_lots > 0)
raw <- raw %>% separate(update_datetime, into = c('Date','Time'), sep ='T')
date_copy <- raw$Date
raw$Date_ <- date_copy
raw <- raw %>% separate(Date_, into = c('Year','Month','Day'), sep ='-')
raw$Year <- as.integer(raw$Year)
raw$Month <- as.integer(raw$Month)
raw$Day   <- as.integer(raw$Day)
raw <- raw %>% filter((Year == 2020) & (Month == 1) & (Day %in% c(seq(from = 1, to = 7))))
time_copy <- raw$Time
raw$Time_ <- time_copy
raw <- raw %>% separate(Time_, into = c('Hr','Min','Sec'), sep = ':')
                      
# Q2.1 Find hourly average total number of lots available
av_lot_avai <- raw %>% group_by(Hr) %>% summarise((mean(lots_available)))
print(av_lot_avai)

# or: aggregate(x = raw$lots_available, by = list(raw$Hr),FUN = mean)

# Q.2.2 Find average hourly availability rate (lots_avai / total_lots)

# remove records where lots_available > total_lots
raw <- raw %>% filter(lots_available <= total_lots)

# compute average availability rate
raw$Average_Availabilty_Rate <- (raw$lots_available / raw$total_lots)

# Find average hourly availability rate by Lot Type
av_avai_rate <- raw %>% group_by(Hr, lot_type) %>% summarise((mean(Average_Availabilty_Rate)))
print(av_avai_rate)