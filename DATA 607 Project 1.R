#The raw data can be found here 
#https://raw.githubusercontent.com/vindication09/DATA607_Project1/master/RawChessData

ELOsheet <- read.csv(paste0("~/Desktop/DATA Science SPS/DATA 607/Week 3/ELOsheet.txt"))
View(ELOsheet)
head(ELOsheet, 10)

#goal:
#we want to create a structured data set that can be uploaded into MySQL that contains the following columns
#Player’s Name, Player’s State, Total Number of Points, Player’s Pre-Rating, and Average Pre Rating of Opponents.

#Before doing anything, I notice that there are headings in the first 3 rows. Lets remove them
ELOsheet2<-ELOsheet[-c(1:2),]
head(ELOsheet2)

#we need to use the stingr package in order to massage the data and extract what we need
library(stringr)

#Here we want to obtain the names of players from the score sheet 
#I notice that there is a specific structure to each row
#With the heading gone, names, scores, and opponents are found in the first row of each subsection
#States, ids, and pre rating are all on the second row of each subsection 

#I can use this to my advantge and create a subset of data that grabs the first row of each subsection
#and another that grabs the 2nd row of each subsection. 

#to grab the 1st row of each subsection, I want to skip the first row, grab the second, skip the third and 4th then repeat 
ELOsubsheet1<-ELOsheet2[seq(2, length(ELOsheet2), 3)]
head(ELOsubsheet1, 64)

#to grab the 2nd row of each subsection, I need to skip row 1, 2 grab 3, skip 4 then repeat 
#to grab the 1st row of each subsection, I want to skip the first row, grab the second, skip the third and 4th then repeat 
ELOsubsheet2<-ELOsheet2[seq(3, length(ELOsheet2), 3)]
head(ELOsubsheet2, 64)

#use subsheet1
#i noticed that names are all upper case, we can use this to our advantage 
ELOname <- unlist(str_extract_all(ELOsubsheet1, "\\| [[:upper:]- ]{4,} \\|"))
ELOname <- str_replace_all(ELOname, pattern = "(\\| )|([[:space:]]{1,}\\|)", replacement = "")
head(ELOname, 64)
df.ELOname <- data.frame(ELOname)
df.ELOname

#use subsheet 2
#the next thing we need to extract are the States the players come from 
#I notice that states are upper case but also have abbreviations. There is no pipe before states 
#This is something I can use to my advantage in order to extract them. 
ELOstate <- unlist(str_extract_all(ELOsubsheet2, "\\ [[:space:]]{1,}[[A-Z]]{2} \\|"))
ELOstate <- str_replace_all(ELOstate, pattern = "(\\|[[:space:]]{1,})|([[:space:]]{1,}\\|)", replacement = "")
head(ELOstate, 64)
df.ELOstate <- data.frame(ELOstate)
df.ELOstate

#use subsheet 1
#The next item on the list is to extract is the total number of points 
#I noticed that points are in the form n.n. The are also between pipes
#Lets take n.n skip the space before the righthand pipe 
ELOtotalpoints <- unlist(str_extract_all(ELOsubsheet1, "\\|[[:digit:].[:digit:]]{3}[[:space:]]{1,}\\|"))
ELOtotalpoints <- str_replace_all(ELOtotalpoints, pattern = "(\\|)|([[:space:]]{1,}\\|)", replacement = "")
head(ELOtotalpoints, 64)
df.ELOtotalpoints <- data.frame(as.numeric(ELOtotalpoints))
df.ELOtotalpoints

#use subsheet 2
#The next item on the list that needs to be extracted is the players pre-rating 
#the pre rating is to the right of R: and to the left of  spaces and arrow ->
#this can be used to our advantage to extract the pre rating 
ELOprerating <- unlist(str_extract_all(ELOsubsheet2, "[:] [[:alnum:] ]{2,9}\\-\\>"))
ELOprerating <- str_replace_all(ELOprerating, pattern = "(\\: )|(\\s{1,}\\-\\>)|([O-Q]\\d{1,2})|(\\-\\>)", replacement = "")
ELOprerating <- as.numeric(ELOprerating)
head(ELOprerating, 64)
df.ELOprerating<-data.frame(as.numeric(ELOprerating))
df.ELOprerating

#As of now, we have a partial data frame with four of the five columns
#computing the average opponent pre rating requires more manipulation of the original data frame 
partialcsv<-data.frame(df.ELOplayer,  df.ELOname, df.ELOstate, df.ELOtotalpoints, ELOprerating)
partialcsv

#use subsheet1
#We need to compute the average pre rating for opponents by player 
#I first need to extract the opponents into their own data frame
#We can extract digits using d and add + to keep going till it hits pipe
ELOopponent<-unlist(str_extract_all(ELOsubsheet1, "\\d+\\|" ), "\\d+")
ELOopponent<-str_replace_all(ELOopponent, pattern="\\|", replace="")
head(ELOopponent, 64)
ELOopponent<-as.integer(ELOopponent)
df.ELOopponent<-data.frame(as.integer(ELOopponent))
df.ELOopponent

#Maybe we can flatten the opponents and make as a data frame


#use subsheet 1
#the last piece we need in order to compute the average are the player numbers
#The goal is to write a loop that matches a player number to each of their opponents, 
#the loop then fetches the ratings for each opponent and divides by number of rounds 
#players are ordered 1-64. 
ELOplayer<-as.integer(str_extract(ELOsubsheet1, "\\d+"))
head(ELOplayer, 64)
df.ELOplayer<-data.frame(as.integer(ELOplayer))
df.ELOplayer

#use subsheet 1
#How do we compute the average opponent player rating? 
avg_ELOopp_rating<-length(ELOsubsheet1)
#loop
for (i in 1: length(ELOsubsheet1))
{
  avg_ELOopp_rating[i]<-mean(ELOprerating[as.numeric(unlist(ELOopponent[ELOplayer[i]]))])
}
  
head(avg_ELOopp_rating, 64)
df.avg_ELOopp_rating<-data.frame(as.numeric(avg_ELOopp_rating))
df.avg_ELOopp_rating

#Put together in a data frame 
csv<-data.frame(df.ELOplayer, df.ELOname, df.ELOstate, df.ELOtotalpoints, ELOprerating, df.avg_ELOopp_rating)
csv

#use a better naming convention 
colnames(csv)[colnames(csv)=="as.integer.ELOplayer."]<-"PlayerNumber"
colnames(csv)[colnames(csv)=="ELOname"]<-"Name"
colnames(csv)[colnames(csv)=="ELOstate"]<-"State"
colnames(csv)[colnames(csv)=="as.numeric.ELOtotalpoints."]<-"TotalPoints"
colnames(csv)[colnames(csv)=="ELOprerating"]<-"PreRating"
colnames(csv)[colnames(csv)=="as.numeric.avg_ELOopp_rating."]<-"AvgOppPreRating"
csv

#export to a CSV
write.table(mydata, "~/Desktop/", sep=",")