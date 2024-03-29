library("selectr")

library("xml2")

library("rvest")



url <- "https://nytimes.com"

webpage <- read_html(url)



# Just sample code. 

titles <- html_nodes(webpage, "div h2")

without_tags <- gsub("<.*?>", "", titles) 

# Assignment 15 Task 1: write scripts that extract "titles" and "news summary" of articles out of the scrapped data. 

# Then, print them out using "print()" statement. 

n_summary<- html_nodes(webpage,"p")
without_sum<- gsub("<.*?>", "",n_summary) 
y<-data.frame(without_sum)

x<-data.frame(without_tags[0:(length(without_sum))])


z<-data.frame(x,y)
z

# Assignment 15 Task 2: write scripts that oraganize your data as dataframe with column names, "title" and "news summary", respectively.

# Then, save this dataframe as a CSV file. Name it as "NYT_titles.csv". 

write.csv(z,"AIT580/R_assignment_15/NYT_titles.csv")





# Assignment 15 Task 3: once you save the CSV file, commit and push it back to your repository (no R scripts involved for Task 3). 
