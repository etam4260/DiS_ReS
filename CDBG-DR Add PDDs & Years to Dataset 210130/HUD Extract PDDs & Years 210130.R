data = read.csv(file.choose(),header = TRUE)
all_action_plans_pdds = data.frame()

all_action_plans_pdds = data.frame(GrantNo = data$GrantNo[1], pdd = data[1,5], year = data[1,6])



for(i in 1:nrow(data)) {
  
  if(!(data[i,]$GrantNo %in% all_action_plans_pdds$GrantNo)) {
    
    pdds = ""
    years = ""
    
    for(j in seq(5, 15, 2)) {
      if (!is.na(data[i,j]) && (data[i,j] != "")) {
        if(j == 5) {
          pdds = data[i,j]
        } else {
          pdds = paste(pdds, data[i,j], sep = " ")  
        }
      }
    }
    
    for(z in seq(6, 16, 2)) {
      if(!is.na(data[i,z]) && (data[i,z] != "")) {
        if(z == 6) {
          years = data[i,z]
        } else {
          years = paste(years, data[i,z], sep = " ") 
        }
      }
    }
    
    
    new_row = data.frame(GrantNo = data[i,]$GrantNo, pdd = pdds, year = years)
    all_action_plans_pdds = rbind(all_action_plans_pdds, new_row)

  }
}

write.csv(all_action_plans_pdds, file = "HUD_All_Action_Plans_and_PDDs.csv",row.names = FALSE)