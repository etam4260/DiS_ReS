#2011, Disastr, All mony
#2011,2012, Distastr, Split vnly btwn 2011/2012

All_Projects <- read.csv(file.choose(),header = TRUE)
test_frame = data.frame()
unknown_fips_frame = data.frame()

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


All_Projects = clean_data()
test_frame = stack_project_fips()
test_frame = initialize_test_frame()
