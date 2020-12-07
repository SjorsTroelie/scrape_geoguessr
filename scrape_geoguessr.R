#Scraping Geoguesser challenge results using Rselenium

#Note for first time use: If not already installed, the script will try to install 4 packages from CRAN (starting on line 35)

#You need to be logged in in order to view Challenge data

#Step 1; pass mail and password to mail and password variables.
#WARNING: IT IS NOT ADVISABLE TO WRITE DOWN PASSWORDS IN SAVED DOCUMENTS! 
#It is best to assign the  e-mail and password variables in the console! 

#mail <- "example@example.com"
#password <- "password123"

#Step 2: fill out the working directory (where the challenge csv file is located)

working_directory <- "C:/My_file_location"

#Step 3: fill out the name of the challenge csv
#The challenge csv should be a comma delimited file with two columns
#column 1 should be named "round" and can contain any character string
#columm 2 should be named "URL" and should contain challenge url's

challenge_csv <- "List_of_challenges.csv"

#Step 4: fill out the name of the export file; this is up to you as long as its a csv.
export_filename <- "challenge_data.csv"
 
  
#If selenium throws an error regarding the chrome browser version you have installed, change chrome_version to the version of your installed browser
chrome_version <- "87.0.4280.88"

############## You should not have to edit anything below this line ###############

#Checking if required packages are already installed, then installing packages if needed 
required_packages <- c("RSelenium", "tidyverse","rvest","stringr")

to_install <- required_packages[!required_packages %in% installed.packages()]
 
install.packages(to_install)

#Loading packages
library(RSelenium)
library(tidyverse)
library(rvest)
library(stringr)

#Set working directory
setwd(working_directory)

#Read challenge_url's
geoguessr_challenges <- read.csv2(challenge_csv, sep = ",")


#Set URL for geoguessr login page
geoguessr_signin <- "https://www.geoguessr.com/signin"

#Starting Rselenium drive

driver <- rsDriver(browser=c("chrome"), chromever=chrome_version)

remote_driver <- driver[["client"]]
remote_driver$open()

#Set implicit time-out (Preventing Rselenium from  trying to read data before the page is loaded)
remote_driver$setTimeout(type = "implicit", milliseconds = 5000)


#Navigate to login
remote_driver$navigate(geoguessr_signin)


#Login procedure; wrapped in tryCatch in case user is already logged in
    tryCatch({
      #Find username/email field
      username_field <- remote_driver$findElement(using = "name", value = "email")
      
      
      #Sent email to username field
      username_field$sendKeysToElement(list(mail))
      
      #Find password field
      password_field <- remote_driver$findElement(using = "name", value = "password")
      
      #Sent password to password field
      password_field$sendKeysToElement(list(password))
      
      #Find login button
      knop <- remote_driver$findElement(using = "class" ,value = "button--primary")
      #Click login button
      knop$clickElement()

      },
      #Sent error msg to console if login fields/buttons cannot be found
      error = function(cond){
        message("Already logged in?")
      }
    )
    

    #Helperfunctions for reading scores / distances and time
    
    read_score <- function(string){
      
      y <- str_extract(string, pattern = ".*(?= pts)") %>%
        str_replace(",","")%>%
        as.numeric()%>%
        #Set score to 0 if NA (should only when someone "Timed out" )
        replace_na(0)
      
      #return y
      y
    }
    
    #Get distance and convert it to meters
    read_distance <- function(string){
      
      #Set distance to NA if "Timed out"
      if(str_detect(string,"Timed out")){
        
        distance <- NA
      
      #If distance is measured in "km"; multiply by 1000
      }else if(str_detect(string, "km")){
      
        
        distance <- str_extract(string, pattern = ".*(?= km)")%>%
          str_replace(",","")%>%
          as.numeric()
        
        distance <- distance *1000
        
      #Distance is measured in meters already  
      }else{
         distance <- str_extract(string, pattern = ".*(?= m )")%>%
           as.numeric()
         
      }
      
      #Return distance
      distance
    }
    
    
    read_time <- function(string){
      
      #Get time from the string that includes both distance and time
      time <- str_extract(string, pattern = "(?<= - ).*")
      
      #Get minutes
      minutes <- str_extract(time, pattern = ".*(?= min)")%>%
        as.numeric()%>%
        #If there are no minutes in the string; set minutes to 0
        replace_na(0)
        
        #convert minutes to seconds
        seconds_1 <- minutes*60
      
      #Get seconds from the string
      seconds_2 <- str_extract(time, pattern = "..(?= sec)")%>%
        as.numeric()%>%
        #If there are no seconds in the string; set seconds to 0
        replace_na(0)
      
      #Add up seconds from "min" and "sec"  elements
      total_seconds <- seconds_1 + seconds_2
      
      #Return sum of seconds
      total_seconds


    }
    
    
#Create an emtpy dataframe to fill with challenge data
df <- data.frame(round = character(0),
                 placement = numeric(0),
                 name = character(0),
                 score_1 = numeric(0), 
                 score_2 = numeric(0),
                 score_3 = numeric(0),
                 score_4 = numeric(0),
                 score_5 = numeric(0),
                 score_total = numeric(0),
                 distance_1 = numeric(0),
                 distance_2 = numeric(0),
                 distance_3 = numeric(0),
                 distance_4 = numeric(0),
                 distance_5 = numeric(0),
                 distance_total = numeric(0),
                 time_1 = numeric(0),
                 time_2 = numeric(0),
                 time_3 = numeric(0),
                 time_4 = numeric(0),
                 time_5 = numeric(0),
                 time_total = numeric(0)
                 )

#Loop over all rounds in geoguessr challenge df
    for (i in 1:nrow(geoguessr_challenges)){
      
      
      round <- geoguessr_challenges$round[i]
      url <- geoguessr_challenges$URL[i]
      #Print current round & url to console
      print(paste("reading round", round,
                  "URL:", url))
      
      #Navigate driver to challenge URL
      remote_driver$navigate(url)
      
      #Read challenge data
      
      #Find highscore table
      table <- remote_driver$findElement(using = "class",value = "results-highscore--num-columns-7")    
      #Read table as character string
      table_char_str <- table$getElementText()
      #Make a vector by splitting on "\n"
      table_vector <- str_split(table_char_str, pattern = "\n") %>%
      unlist()

    #Calculate the amount of participants to determine iterations of DF loop below
      #One row of challenge data (one person) is 14 elements long
      #The headers of the table do not need to be read. These are the first 6 elements in the table_vector
    
    #So No. participants = (elements in vector - 6) / 14
    participants <- (length(table_vector)-6) /14
    
    #Set counter to 7 (first element with relevant data)
    counter <- 7
  
    #Loop over no of participants
    for (y in 1:participants){

      #Read placement
      placement <- table_vector[counter]
      #Read name
      name <- table_vector[counter+1]
      #Scores
      score_1 <- read_score(table_vector[counter+2])
      score_2 <- read_score(table_vector[counter+4])
      score_3 <- read_score(table_vector[counter+6])
      score_4 <- read_score(table_vector[counter+8])
      score_5 <- read_score(table_vector[counter+10])
      score_total <- read_score(table_vector[counter+12])
      
      #Distances
      distance_1 <- read_distance(table_vector[counter+3]) 
      distance_2 <- read_distance(table_vector[counter+5])
      distance_3 <- read_distance(table_vector[counter+7])
      distance_4 <- read_distance(table_vector[counter+9])
      distance_5 <- read_distance(table_vector[counter+11])
      distance_total <- read_distance(table_vector[counter+13])
      
      #Time
      time_1 <- read_time(table_vector[counter+3]) 
      time_2 <- read_time(table_vector[counter+5]) 
      time_3 <- read_time(table_vector[counter+7]) 
      time_4 <- read_time(table_vector[counter+9]) 
      time_5 <- read_time(table_vector[counter+11]) 
      time_total <- read_time(table_vector[counter+13]) 
      
      
      #Bind current participant data to rest of dataframe
      df<- rbind(df,c(round, placement, name,
                    score_1, score_2, score_3, score_4, score_5, score_total,
                    distance_1, distance_2, distance_3, distance_4, distance_5, distance_total,
                    time_1, time_2, time_3, time_4, time_5, time_total))
      
   
      
      #Increment counter with 14
      counter <- counter + 14
    }
    }


#define colnames
colnames(df) <- c("round", "placement","name","score_1","score_2","score_3","score_4","score_5","score_total",
                  "distance_1","distance_2","distance_3","distance_4","distance_5", "distance_total",
                  "time_1","time_2","time_3","time_4","time_5","time_total")

#Function to check if column has numeric data
is_all_numeric <- function(x) {
  !any(is.na(suppressWarnings(as.numeric(na.omit(x))))) & is.character(x)
}

#Convert relevant columns to numeric
df <- df %>% 
  mutate_if(is_all_numeric,as.numeric)

#Write csv to working directory
write.csv2(df,export_filename, row.names = F)