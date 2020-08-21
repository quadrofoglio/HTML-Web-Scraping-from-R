library(dplyr)
library(rvest)
library(rlist)
library(tidyr)

# Initialise Empty DF
car <- data.frame('Name' = NA, 'Distributor' = NA, 'Hotline' = NA
                  , 'Manufactured In' = NA, 'Predecessor' = NA, stringsAsFactors = FALSE)
car <- na.omit(car)

# Get listings to crawl
urls <- (read_html('https://www.sgcarmart.com/new_cars/index.php') %>% html_nodes('.link') %>% html_attr('href'))[10:27] 
urls <- paste('https://www.sgcarmart.com/new_cars/', urls, sep='')

# Parallel_Import Indexes
parallel_import_index <- function(name_vect){
  temp<- vector()
  ind <- grep('Parallel', name_vect)
  return(ind)
}


#List of Names
names <- list()
for (url in urls){
  name <- read_html(url) %>% html_nodes('a.link_redbanner') %>% html_text()
  names <- list.append(names,name)
}

parallel_ind <- parallel_import_index(as.character(names))

#List of Distributor
dists <- list()
for (ind in 1:length(urls)){
  #print(ind)       #debug
  url <- urls[ind]
  if (regexpr('Parallel',names[ind]) != -1){    #if name contains parallel import
    dists <- list.append(dists,'Parallel Importers')
  } else if (regexpr('review', (read_html(url) %>% html_nodes('strong a'))[2] %>% html_text()) == -1){
      #print('elif') #debug
      dist <- (read_html(url) %>% html_nodes('strong a'))[3] %>% html_text()
      dists <- list.append(dists,dist)
  } else {
      #print('else') #debug
      dist <- (read_html(url) %>% html_nodes('strong a'))[4] %>% html_text()
      dists <- list.append(dists,dist)
  }
    }

# List of Hotline  
hotline <- list()
for (ind in 1:length(urls)){
  #print('outer')
  #print(ind)
  url <- urls[ind]
  for (i in 1:length((read_html(url) %>% html_nodes('.font_gray') %>% html_text()))){
    #print('inner')
    #print(i)
    if (regexpr('Parallel',names[ind]) != -1){ #if name contains parallel import
      #print(NA)
      hotline <- list.append(hotline,NA)
      break
    } else if (regexpr('No review posted',(read_html(url) %>% html_nodes('.font_gray'))[1] %>% html_text()) != -1)  {   #elif 1st node contains 'no review posted'
      phone <- (read_html(url) %>% html_nodes('.font_gray'))[2] %>% html_text()
      #print(phone)
      #print('no review case')
      hotline <- list.append(hotline,phone)
      break
    } else if (regexpr('No review posted',(read_html(url) %>% html_nodes('.font_gray'))[1] %>% html_text()) == -1) {   #elif  1st node don't contain 'no review posted'
      phone <- (read_html(url) %>% html_nodes('.font_gray'))[1] %>% html_text()
      #print(phone)
      #print('has review case')
      hotline <- list.append(hotline,phone)  
      break
      }
  }
}

# Made_in_Country
countries <- list()
for (ind in 1:length(urls)){
  #print('outer')
  #print(ind)
  url <- urls[ind]
  for (i in 1:length((read_html(url) %>% html_nodes('.font_gray') %>% html_text()))){
    #print('inner')
    #print(i)
    if (regexpr('Parallel',names[ind]) != -1){ #if name contains parallel import
      if (regexpr('No review posted',(read_html(url) %>% html_nodes('.font_gray'))[1] %>% html_text()) != -1){ #elif 1st node contain 'No review'
        country <- (read_html(url) %>% html_nodes('.font_gray'))[2] %>% html_text()}
      else if (regexpr('No review posted',(read_html(url) %>% html_nodes('.font_gray'))[1] %>% html_text()) == -1){
        country <- (read_html(url) %>% html_nodes('.font_gray'))[1] %>% html_text()}
      countries <- list.append(countries,country)
      break
      
    } else {   # non-parallel import
      if (regexpr('No review posted',(read_html(url) %>% html_nodes('.font_gray'))[1] %>% html_text()) != -1){ #elif 1st node contain 'No review'
        country <- (read_html(url) %>% html_nodes('.font_gray'))[3] %>% html_text()}
      else if (regexpr('No review posted',(read_html(url) %>% html_nodes('.font_gray'))[1] %>% html_text()) == -1){
        country <- (read_html(url) %>% html_nodes('.font_gray'))[2] %>% html_text()}
      countries <- list.append(countries,country)
      break
    }
  }
}


#Predecessor
predecessors <- vector()
for (ind in 1:length(urls)){
  #print('outer')
  #print(ind)
  url <- urls[ind]
  nodes <-  (read_html(url) %>% html_nodes('.font_gray') %>% html_text())
  n_node <- length(nodes)
  nodes <- sort(nodes)
  #print(nodes)
  #print(n_node)
  for (i in 1:n_node){
    #case 1: dealer case
    if (regexpr('Parallel',names[ind]) == -1){
      #case 1.1 3 nodes case
      if(n_node == 3){
        #case 1.1.1 'no review case'
        if(regexpr('No review',nodes[3]) != -1){
          temp <- NA
          print(temp)
          predecessors <- append(predecessors, temp)
          break
        }
        #case 1.1.2 pred in nodes[3]
        else {
          temp <- nodes[3]
          print(temp)
          predecessors <- append(predecessors, temp)
          break
        }
      }
      
      #case 1.2 2 nodes case
      if (n_node == 2){
        temp <- NA
        print(temp)
        predecessors <- append(predecessors, temp)
        break
      }
    }
    
    #case 2: parallel case
    else if (regexpr('Parallel',names[ind]) != -1){
      #2.1 case of 2 nodes
      if(n_node == 2){
        temp <- NA
        print(temp)
        predecessors <- append(predecessors, temp)
        break
      }
      
      #2.2 case of 3 nodes
      else if(n_node == 3){
        # 2.2.1 'No review' in node[3] case
        if (regexpr('No review',nodes[3]) != -1){
          temp <- NA
          print(temp)
          predecessors <- append(predecessors, temp)
          break
        }
        # 2.2.2 pred in nodes[1]
        else {
          temp <- nodes[1]
          print(temp)
          predecessors <- append(predecessors, temp)
          break
        }
      }
      #2.3 case of 4 nodes
      else if(n_node == 4){
        temp <- nodes[1]
        print(temp)
        predecessors <- append(predecessors, temp)
        break
      }
    }
  }
}

# clean Countries

for (i in 1:length(countries)){
  # Case 1) Dealer case: index 1 is '\r\n                                    \t
  if (regexpr('\r\n                                    \t',countries[[i]]) == 1){
    countries[[i]] <- sub('\r\n                                    \t','',countries[[i]])
    ind <- regexpr('launched', countries[[i]])
    countries[[i]] <- substr(countries[[i]],1,(ind - 3))
  }
  # Case 2) Parallel Case
  else {
  ind <- regexpr('launched', countries[[i]])
  countries[[i]] <- substr(countries[[i]],1,(ind - 3))
  }
}

# Clean Predecessors
pred <- as.list(predecessors)
for (i in 1:length(pred)){
  if (is.na(pred[[i]])){
    #print('elif')
    print(pred[[i]])
    next
  }
  else if (regexpr("\r\n                                    \t\r\n                                    \t",pred[[i]]) == 1){
    #print('if cond')
    pred[[i]] <- sub("\r\n                                    \t\r\n                                    \t",'',pred[[i]])
    ind <- regexpr('                                        \r\n\t\t\t\t\t\t\t\t\t', pred[[i]])
    pred[[i]] <- substr(pred[[i]],1,(ind - 1))
  }
  else{
    #print('else')
    print(pred[[i]])
    next
  }
}


# Put everything to car dataframe
names <- as.character(names)
dists <- as.character(dists)
hotline <- as.character(hotline)
countries <- as.character(countries)
pred <- as.character(pred)
car_dirty <- data.frame('Name' = names, 'Distributor' = dists, 'Hotline' = hotline,'Manufactured In' = countries,'Predecessor' = pred)
cars_clean <- car_dirty
cars_clean <- cars_clean %>% separate( Name, into = c("Make", "Model"), sep = ' ', extra = 'merge') %>%
               separate(Hotline, into = c("Hotline1", "Hotline2"), sep =' / ')
head(cars_clean)

# export
wd = getwd()
write.csv(cars_clean,'Assignment_4_Q1_raw.csv', row.names = FALSE)


