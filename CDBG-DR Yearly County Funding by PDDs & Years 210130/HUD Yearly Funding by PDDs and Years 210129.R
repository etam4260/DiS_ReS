#Hud Yearly County Funding by PDDs and year
#Emmet Tam & Hamed Ghaedi
#emmet_tam@yahoo.com & hghaedi@umd.edu
#January 29, 2021

#You are prompted for two files. Input CDBG-DR Excel Only_All Projects.csv
#and then PRFips.csv.
library(usmap)

All_Projects <- read.csv(file.choose(),header = TRUE)
PR_fips <- read.csv(file.choose(),header = TRUE)

#test_frame holds county funding for each year from 1999-2050.
test_frame = data.frame()

clean_data = function() {
  #Convert fips values in the imported data set to characters so they 
  #can be easily be manipulated in the future.
  All_Projects$CountyFIPSAll <- as.character(All_Projects$CountyFIPSAll)
  
  #Convert funding in the imported data set to characters so they
  #can be easily be manipulated in the future. Remove the commas in
  #funding so they can be treated as numbers.
  All_Projects$TotalFunds <- as.character(All_Projects$TotalFunds)
  All_Projects$TotalFunds <- gsub(",","",All_Projects$TotalFunds)
  return(All_Projects)
}


stack_project_fips = function() {
  #Go through every project and get their CountyFIPSAll, split them and stack ontop of each other
  #in a dataframe. Also include their action plans for each fip. Rename the first
  #column to fips.
  for (i in 1:nrow(All_Projects)){
    if ((nchar(All_Projects[i,"CountyFIPSAll"])) > 0) {
      fips <- as.data.frame(unlist(strsplit(All_Projects[i,"CountyFIPSAll"], "\\-")))
      fips$action_plan <- All_Projects[i,"GrantNo"]
      
      if (i == 1){
        test_frame <- fips
      } else {
        test_frame <- rbind(test_frame,fips)
      }
    }
  }
  names(test_frame)[1] = "fips"
  return(test_frame)
}


initialize_test_frame = function() {
  #This section calculates the minimum year and maximum year from the ProjectStartDate
  #and ProjectEndDate, respectively. These are used to determine how long projects
  #spanned for and how many more columns the data frame needs.
  minimum_year = as.integer(substring(All_Projects[1, "ProjectStartDate"], nchar(All_Projects[1, "ProjectStartDate"]) - 3, nchar(All_Projects[1, "ProjectStartDate"])))
  for(date in All_Projects$ProjectStartDate) {
    this_year = as.integer(substring(date, nchar(date) - 3, nchar(date)))
    if(this_year < minimum_year) {
      minimum_year = this_year
    }
  }
  maximum_year = minimum_year
  for(date in All_Projects$ProjectEndDate) {
    this_year = as.integer(substring(date, nchar(date) - 3, nchar(date)))
    if(this_year > maximum_year) {
      maximum_year = this_year
    }
  }
  #Create a list initialized with 0's that spans the length of the test frame.
  #Every year is going to be initialized to 0. This is going to be used to
  #initialized the whole test frame with rows of 0's spanning from 1999-2050.
  d <- c()
  for(j in 1:nrow(test_frame)) {
    d = append(d, 0.00)
  }
  
  #Using the list constructed above, create multiple new columns and column bind 
  #them to the test frame. The new columns span from the minimum year, which 
  #should be 1999 and the maximum year which should be 2050. All these columns
  #are rows of 0's.
  all_years = c()
  for(i in minimum_year:maximum_year) {
    all_years = append(all_years, toString(i))
    new_col = data.frame(x = d)
    names(new_col)[1] = toString(i)
    test_frame = cbind(test_frame, new_col)
  }
  return(test_frame)
}


aggregate_similar_fips_get_county_states = function() {
  #Aggregate function should be down here to group similar fips in the fips column
  #together by aggregating their money respectively for each year.
  test_frame = aggregate(x = test_frame[,3:ncol(test_frame)], by = list(test_frame$fips), FUN = sum)
  names(test_frame)[1] = "fips"
  
  #Create a new data frame to hold fips and their corresponding state/county
  fips_frame = data.frame(test_frame$fips)
  names(fips_frame)[1] = "fips"
  
  #Create a new column called State that will take this rows corresponding
  #fip and name it by its state using the usmaps library (fips_info)
  #Create a new column called County that will take this rows corresponding
  #fip and name it by its county using the usmaps library (fips_info)
  for (i in 1:nrow(test_frame)) {
    
    if (i%%1000 ==0) print(i/nrow(test_frame)) #Loop progress
    
    tryCatch({
      fips_frame[i,"State"] <- fips_info(as.integer(as.character(fips_frame[i,1])))[,"abbr"] 
      fips_frame[i,"County"] <- fips_info(as.integer(as.character(fips_frame[i,1])))[,"county"]
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
    
  }
  
  #reorder the data frame so counties, fips, and state column are next to each other.
  test_frame = cbind(fips_frame, test_frame[ ,2:ncol(test_frame)])
  return(test_frame)
}


group_future_years = function() {
  #group the years 2020-2050 into one column using row sum and combine the other years.
  future_row_sum = data.frame(rowSums(test_frame[, c(25:ncol(test_frame))]))
  names(future_row_sum)[1] = "2020-2050"  
  test_frame = cbind(test_frame[,c(1:24)], future_row_sum)
  return(test_frame)
}


hard_code_PR_fips = function() {
  #Read in the PRfip codes that were added and hard code the states/counties
  for(i in 979:1010) {
    index = which(PR_fips$fips == test_frame[i, "fips"])
    test_frame[i, "State"] = "PR"
    test_frame[i, "County"] = PR_fips[index, "County"]
  }
  return(test_frame)
}


#START MANIPULATING DATA

#Clean the raw data from CDBG-DR Excel Only_All Projects so R can manipulate
#it properly.
All_Projects = clean_data()

#Stack all the fips ontop of each other from every project. Initialize
#default values(0) by creating the years 1999-2050 for each 
#county(fips) that are stacked and making the values all 0.
test_frame = stack_project_fips()
test_frame = initialize_test_frame()

#Go through every project.
j = 1
for(i in 1:nrow(All_Projects)) {
  
  #Check if this project has corresponding fips.
  if((nchar(All_Projects[i,"CountyFIPSAll"])) > 0) { 
    
    #Grab the CountyFIPSAll for this project
    #and store it into a dataframe with
    #each fip in a separate row.
    fips <- as.data.frame(unlist(strsplit(All_Projects[i,"CountyFIPSAll"], "\\-")))
    
    #Get the total funding for this project.
    funding = as.numeric(All_Projects[i,"TotalFunds"])
    end = j + nrow(fips)
    
    #Some projects may have NA funding which likely means that they received
    #0 dollars.
    if(is.na(funding)) {
      funding = 0
    }
    
    #The maximum number of PDDs that a single action plan can have is 6
    #pdd 1 -> pdd 6 so go through all of them except get the years of when
    #the pdd occured : year 1 -> year 6
    #for this project.
    z = 1
    while (z < 7) {
      id = paste("year.", toString(z), sep = "")
      year = toString(All_Projects[i, c(id)])
      
      #The year can be empty because the project has less than 6 associated pdds
      #Those years are empty in the csv file.
      if(year != "NA") {
        
        #Create a list of years(when PDD occured) associated with this project
        if(z == 1) {
          all_pdd_years = c(year)
        } else {
          all_pdd_years = c(all_pdd_years, year)
        }
      } 
      z = z + 1
    }
    
    num_fips = nrow(fips)
    
    #If funding is 0 then there is 0 funding for all counties associated with
    #this project. Otherwise, split the money.
    if(funding != 0) { 
      #Split the money based off the number of fips in this project time the number
      #of associated PDDs of this project. .
      funds_for_each_fip_per_pdd = funding/(num_fips * length(all_pdd_years))
    } else {
      funds_for_each_fip_per_pdd = 0
    }
    
    #The outer while loop keeps track of how many rows that need to be edited 
    #based on the number of fips that this project has.
    while(j < end) {
      
      #Go through every year associated with the PDDs of this project and add
      #the money for this particular fip at this particular year.
      for(year in all_pdd_years) {
        test_frame[j, c(year)] = test_frame[j, c(year)] + funds_for_each_fip_per_pdd
      }
      j = j + 1
    }
  }
}

#Add similar county rows together and attach the appropriate counties 
#and state to it.
test_frame = aggregate_similar_fips_get_county_states()

#Group the years 2020-2050 for all counties.
test_frame = group_future_years()

#Find corresponding county/state for fips in Puerto Rico. Usmaps does
#not include PR.
test_frame = hard_code_PR_fips()

#Write the final data frame into a csv file, 
#for yearly county funding based off PDD.
write.csv(test_frame, file = "HUD_Yearly_County_Funding_Per_PDD_210129.csv",row.names = FALSE)