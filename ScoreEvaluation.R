

#dplyr library
library(dplyr)

#Set the working directory to the directory containing the data.
setwd("C:/Users/310084562/Documents/Aneurysm_Measurement_project/Aneurysm_Machine_Learning")

#Read in the data from the csv file.
aneurysm_data <- read.csv("Ruptured_23_columns.csv", stringsAsFactors = T, header=T)
aneurysm_data$Aneurysm_count_DC <- as.factor(aneurysm_data$Aneurysm_count_DC)
#Get the names of the columns from the data.
columns <- names(aneurysm_data[2:ncol(aneurysm_data)])
#columns <- names(aneurysm_data[2:6])

#for(j in 1:(ncol(aneurysm_data) -1 )){
  
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
    results <- aggregate(aneurysm_data$Ruptured_DC, by=aneurysm_data[column_combos[[i]]], length)
    
    #phases_data <- aneurysm_data[,1:6]
    #results <- aggregate(Ruptured_DC~., data=phases_data,length)

    #Find row indices containing all rows that have no zeros.  The 1 indicates the
    #margin, which means applying the function by row.  This returns a logical
    #result for each row.  A vector value is true is a row contains no zeros
    #and false if it contains at least one zero.  Filter the results
    #using the logical vector to get rows that contain no zeros.
    #results[results== 0] <- NA
    
    #results <- na.omit(results)


    #change the count column name.
    colnames(results)[ncol(results)] <- "Counts"


    #Add a column for percentage of rows.  Order from most number
    #of counts to smallest number of counts.
    #Filter out results that include at least 10% of the total number
    #of ruptured aneurysm counts.
    results$percentage <- results$Counts/nrow(aneurysm_data)
    results <- results[order(-results$percentage),] 
    results <- results %>% filter(percentage > 0.1)
    
    #results$Phases_total <- rowSums(results[1:5])
    
    

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



#Write the list of data frames of columns to a .csv file.
#lapply(combo_list, function(x) write.table( data.frame(x), paste0(5,"_Column_Combinations",".csv"), append= T, row.names=F, sep=',' ))
#write.csv(results,"Phases_score_eval.csv", row.names=F)
#}

#Write the clean data to a file.
#lapply(clean_data, function(x) write.table( data.frame(x), paste0(5,"_Column_Combinations_clean",".csv"), append= T, row.names=F, sep=',' ))

#Write the filtered data to a file.
lapply(filtered_data, function(x) write.table( data.frame(x), paste0(5,"_Column_Combinations_filtered",".csv"), append= T, row.names=F, sep=',' ))