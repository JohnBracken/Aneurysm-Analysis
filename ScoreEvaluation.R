#This code takes a spreadsheet of intracranial aneurysm data.  There are 23
#columns describing the properties of the aneurysm and the patient (size, shape, number,
#patient gender, etc.)
#The code produces all possible combinations of 5 columns from the table of 23 columns
#and counts how many times specific sets of values come up for each combination of
#5 columns.  The idea here is to see which vairables, or combination thereof, could
#be important to predict whether or not an aneurysm could rupture.
#Aneurysm shape, location in the neurovascular anatomy, and patient gender seem
#to be important, but other factors matter as well.  


#dplyr library
library(dplyr)

#Set the working directory to the directory containing the data.
setwd("C:/Users/310084562/Documents/Aneurysm_Measurement_project/Aneurysm_Machine_Learning")

#Read in the data from the csv file.
aneurysm_data <- read.csv("Ruptured_23_columns.csv", stringsAsFactors = T, header=T)
aneurysm_data$Aneurysm_count_DC <- as.factor(aneurysm_data$Aneurysm_count_DC)


#Get the names of the columns from the data.
columns <- names(aneurysm_data[2:ncol(aneurysm_data)])

  
#Create a list of all possible combinations of 5 columns from the dataset.
column_combos <- combn(columns,5, simplify = FALSE)

#Create a blank list.  This list will store all dataframes for all combinations of tabulated 
#columns for a specific number of columns.  Each data frame for each column combo will be 
#in the list.  
combo_list <- list()


#Now start going through all the possible combinations of columns.
for(i in 1:length(column_combos)){
  
    #Create a frequency table showing the number of counts of each row for the 5 columns chosen
    #from the data.  
    results <- aggregate(aneurysm_data$Ruptured_DC, 
                by=aneurysm_data[column_combos[[i]]], length)
    


    #change the count column name.
    colnames(results)[ncol(results)] <- "Counts"


    #Add a column for percentage of rows.  Order from most number
    #of counts to smallest number of counts.
    #Filter out results that include at least 10% of the total number
    #of ruptured aneurysm counts.
    results$percentage <- results$Counts/nrow(aneurysm_data)
    results <- results[order(-results$percentage),] 
    results <- results %>% filter(percentage > 0.1)
    
    
    
    #Append the new data frame to the list of data frames of five columns.
    combo_list[[i]] <- results 
    
    
}


#Create a new list of clean data.  If a specific combination of
#columns produced no data, then remove it from the list.
clean_data <- list()

clean_data <- combo_list[sapply(combo_list, nrow)>0]


#Now we're going to filter the data to only include
#data frames that include at least three columns that
#contain at least two values each.
filtered_data <- list()
# 
#counter
j <- 1

#Now start looking at each frame in the filtered data.
#For each list element, convert each column into
#a factor variable and count the number of factor
#levels in each column.  If a data frame contains
#at least three columns with at least two values each
#Then add that data frame to the filtered data list.
for(i in 1:length(clean_data)){

#Create the number of different values present in each column
#in the current data frame.
clean_data[[i]][,1:5]<- droplevels(clean_data[[i]][,1:5])
factors <- sapply(clean_data[[i]][1:5], function(x) nlevels(x))


#If the data frame has at least three columns with at least two values each.
if((length(which(factors==1))<3) | (nrow(clean_data[[i]])==1)){

  #Add a data frame that meets the above criteria to the new filtered data list.
  filtered_data[[j]] <- clean_data[[i]]

  #Increment the counter for the filtered data.
  j <- j+1

}

}

#Write the filtered data to a .csv file.
lapply(filtered_data, function(x) write.table( data.frame(x), 
paste0(5,"_Column_Combinations_filtered",".csv"), append= T, row.names=F, sep=',' ))