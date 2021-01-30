#Hud Yearly County Funding & Extra Funds for States
#Emmet Tam & Hamed Ghaedi
#emmet_tam@yahoo.com & hghaedi@umd.edu
#December 22, 2020

#PROGRAM PROMPTS TWO TIMES FOR FILE

#Import necessary libraries.
library(usmap)
library(quantmod)

#Load in data: choose excel only_all projects. Initialize parent data frames.
#Test frame holds spending for projects that do have associated fips numbers.
#Unknown fips frame holds money for each state for projects that don't have fips.
All_Projects <- read.csv(file.choose(),header = TRUE)
test_frame = data.frame()
unknown_fips_frame = data.frame()

#Grab the dataset from the quantmod library and get the constants.
getSymbols("CPIAUCSL", src='FRED') #Consumer Price Index for All Urban Consumers: All Items
tail(CPIAUCSL)
avg.cpi <- apply.yearly(CPIAUCSL, mean)

#Function to create a CPI coefficient chart for a base year from 1947-2020.
#This is used to adjust project funding to 2020 dollars from the project start
#date.
CPIAdj = function(year) {
  #CPI coefficients
  cf <- avg.cpi/as.numeric(avg.cpi[year]) #using 2019 as the base year
  cf <- as.data.frame(cf, row.names =TRUE)
  cf$year <- c(1947:(1947+nrow(cf)-1))
  
  return(cf)
}

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
  #Every year is going to be initialized to 0.
  d <- c()
  for(j in 1:nrow(test_frame)) {
    d = append(d, 0.00)
  }
  
  #Using the list constructed above, create multiple new columns and column bind 
  #them to the test frame. The new columns span from the minimum year, which 
  #should be 1999 and the maximum year which should be 2050.
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

#Just hard-code Puerto Rico for the fips since usmaps does not have the
#numbers.
hard_code_PR_fips = function() {
  #Read in the PRfip codes that were added and hard code the states/counties
  PR_fips <- read.csv(file.choose(),header = TRUE)
  for(i in 979:1010) {
    index = which(PR_fips$fips == test_frame[i, "fips"])
    test_frame[i, "State"] = "PR"
    test_frame[i, "County"] = PR_fips[index, "County"]
  }
  return(test_frame)
}

#START MANIPULATING DATA
All_Projects = clean_data()
test_frame = stack_project_fips()
test_frame = initialize_test_frame()


#Go through each project in All_Projects and determine the length of the projects
#and determine how the funding should be distributed across the years and fips,
#proportionally. Projects can span multiple years or not even. There are
#possibly multiple fips and they must be distributed the money evenly.
j = 1
count = 0
for(i in 1:nrow(All_Projects)) {
  #Look at projects with associated fips/counties only.
  #Determine the start date of this project
  start_date = All_Projects[i, "ProjectStartDate"]
  extract_lower_year = as.integer(substring(start_date, nchar(start_date) - 3, nchar(start_date)))
  start_date = paste(substring(start_date, 1, nchar(start_date)-4), substring(start_date, nchar(start_date) - 1, nchar(start_date)), sep = "")
    
  if((nchar(All_Projects[i,"CountyFIPSAll"])) > 0) { 
    #Determine the end date of this project
    end_date = All_Projects[i, "ProjectEndDate"]
    extract_upper_year = as.integer(substring(end_date, nchar(end_date) - 3, nchar(end_date)))
    end_date = paste(substring(end_date, 1, nchar(end_date)-4), substring(end_date, nchar(end_date) - 1, nchar(end_date)), sep = "")
    
    if(extract_lower_year != extract_upper_year) {
      #Determine how many days are in total that span these two dates.
      whole_diff_days = as.integer(difftime(as.Date(end_date, format = '%m/%d/%y'), as.Date(start_date, format='%m/%d/%y')))
    
      #Create values to help calculate the proportion of money for
      #a year that is not fully spanned. That is a project can start in
      #the middle of a year and so the money should be allocated according to
      #how much of that year is taken up.
      end_year_pointer = paste("12/31/", substring(start_date, nchar(start_date) - 1, nchar(start_date)), sep = "")
      start_year_pointer = paste("1/1/", substring(end_date, nchar(end_date) - 1, nchar(end_date)), sep = "")
    
      #Calculate how many days are in the edge years. i.e (1/23/12 -> 12/31/12)
      lower_proportion = as.integer(difftime(as.Date(end_year_pointer, format='%m/%d/%y'), as.Date(start_date, format='%m/%d/%y')))
      upper_proportion = as.integer(difftime(as.Date(end_date, format = '%m/%d/%y'), as.Date(start_year_pointer, format = '%m/%d/%y')))
    
      #Calculate the percentage that these edge years take up out of the total days
      percentage_for_first_year = lower_proportion/whole_diff_days
      percentage_for_last_year = (upper_proportion + 1)/whole_diff_days
    
      #Calculate the remaining percentage and determine how it should be distributed
      #across the "middle" years. Take the total years in between the start and end date
      #and use that to divide the remaining percentage. Now we have the percentage needed
      #for each year.
      remaining_per = 1 - percentage_for_first_year - percentage_for_last_year
      years_in_between = extract_upper_year - extract_lower_year - 1
      percentage_for_every_other_year = (remaining_per/years_in_between)
    } else {
      percentage_for_first_year = 1
    }
    
    #Grab the CountyFIPSAll from All_Projects and store it into a dataframe with
    #each fip in a separate row. Projects can have multiple fips and so they
    #are just stacked ontop of each other.
    fips <- as.data.frame(unlist(strsplit(All_Projects[i,"CountyFIPSAll"], "\\-")))
    
    #Get the total funding for this project.
    funding = as.numeric(All_Projects[i,"TotalFunds"])
    end = j + nrow(fips)
    
    
    #Some projects may have NA funding which likely means that they received
    #0 dollars.
    if(is.na(funding)) {
      funding = 0
    }
    
    #Calculate the total funding that is to be distributed towards each
    #fip/county. 
    funds_for_each_pip = funding/nrow(fips)
    if(extract_lower_year > 2020) {
      cf = CPIAdj("2020")
    } else {
      cf = CPIAdj(toString(extract_lower_year))
    }
    
    #The outer while loop keeps track of how many rows that need to be edited 
    #based on the number of fips that this project has.
    while(j < end) {

    
      #For every year that this project is done, go to the respective
      #row in test_frame and column year and change its funding. "edge" years
      #get a different amount of funding.
      for(year in extract_lower_year:extract_upper_year) {
        CPI_ind = which(cf$year == 2020)
        
        this_funds = as.numeric(funds_for_each_pip)*as.numeric(cf[CPI_ind, "CPIAUCSL"])      
        this_year = toString(year)
        
        if(year == extract_lower_year) {
          test_frame[j, c(this_year)] = as.numeric(test_frame[j, c(this_year)]) + as.numeric(percentage_for_first_year*this_funds)
        } else if(year == extract_upper_year) {
          test_frame[j, c(this_year)] = as.numeric(test_frame[j, c(this_year)]) + as.numeric(percentage_for_last_year*this_funds)
        } else {
          test_frame[j, c(this_year)] = as.numeric(test_frame[j, c(this_year)]) + as.numeric(percentage_for_every_other_year*this_funds)
        }
        format(test_frame[j, c(this_year)], scientific = FALSE)
      }
      j = j + 1
    }
  #Look at projects that do not have fips numbers associated with them.
  } else {
    
    #Grab this state and the funding for this project.
    state = All_Projects[i, "State"] 
    funding = All_Projects[i, "TotalFunds"]
    
    #Find the CPI adjustment number for this project so you can convert
    #the money value from start year of project to 2020 dollars.
    if(extract_lower_year > 2020) {
      cf = CPIAdj("2020")
    } else {
      cf = CPIAdj(toString(extract_lower_year))
    }
 
    #Get the index number to convert from start year of project to 2020 dollars
    #Convert funding to 2020 dollars.
    CPI_ind = which(cf$year == 2020)
    funding = as.numeric(funding) * as.numeric(cf[CPI_ind, "CPIAUCSL"])
    
    #Initialize the data frame for state funding once a project is
    #found that does not have associated fips.
    if(nrow(unknown_fips_frame) == 0) {
      curr_state = state
      curr_ind = 1
      count = count + 1
      
      unknown_fips_frame = data.frame(State = state, ExtraFunding = as.numeric(funding), NumExtraProj = 0)
    } else {
      
      #Once the state changes from one state to another that means we found all
      #the projects(since states are grouped together) 
      #without fips codes and can move onto another state.
      if(curr_state == state) {
        count = count + 1
        unknown_fips_frame[curr_ind, "ExtraFunding"] =+ as.numeric(funding)
      } else {
        unknown_fips_frame[curr_ind, "NumExtraProj"] = count
        count = 1
        curr_ind = curr_ind + 1
        curr_state = state
        
        unknown_fips_frame = rbind(unknown_fips_frame, data.frame(State = state, ExtraFunding = as.numeric(funding), NumExtraProj = 0))
      }
    }
  }
}
unknown_fips_frame[curr_ind, "NumExtraProj"] = count

test_frame = aggregate_similar_fips_get_county_states()
test_frame = group_future_years()
test_frame = hard_code_PR_fips()

#Since states are not perfectly grouped together within the dataset
#(A state, Maryland, might be the state for projects within the next 10
#or so entries. However, after that, Maryland might appear again as a state
#and so it will have multiple entries of Maryland). So combine these similar
#states funding to get the final funding for a state.
unknown_fips_frame = aggregate(x = unknown_fips_frame[,2:ncol(unknown_fips_frame)], by = list(unknown_fips_frame$State), FUN = sum)
names(unknown_fips_frame)[1] = "State"

#Write the master data frame that the program has worked through into a csv file.
write.csv(test_frame, file = "HUD_county_funding_w_yearly_divided_210130.csv",row.names = FALSE)
write.csv(unknown_fips_frame, file = "HUD_county_funding_for_unknown_projects_210130.csv",row.names = FALSE)