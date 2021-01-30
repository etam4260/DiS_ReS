All_Projects <- read.csv(file.choose(),header = TRUE)


Action_Plans_to_PDD <- read.csv(file.choose(),header = TRUE)

Action_plans_to_row = data.frame()
initialized_action_plan = FALSE

#Make assumption that same action plans are grouped together.

curr = All_Projects$GrantNo[1]
for(i in 1:nrow(All_Projects)) {

  
  if(All_Projects$GrantNo[i] != curr) {
    if(!initialized_action_plan) {
      Action_plans_to_row = data.frame(action_plan = c(All_Projects$GrantNo[i-1]), index = c(i-1))  
      initialized_action_plan = TRUE
      curr = All_Projects$GrantNo[i]
    } else {
      if(!(All_Projects$GrantNo[i] %in% Action_plans_to_row$action_plan)) {
        new_row = data.frame(action_plan = c(All_Projects$GrantNo[i-1]), index = c(i-1))
        Action_plans_to_row = rbind(Action_plans_to_row, new_row)
        curr = All_Projects$GrantNo[i]
      }
    }
  }
}
new_row = data.frame(action_plan = c(All_Projects$GrantNo[i]), index = c(i))
Action_plans_to_row = rbind(Action_plans_to_row, new_row)
curr = All_Projects$GrantNo[i]

columns_to_add = data.frame()


z = 1
for(j in 1:nrow(Action_plans_to_row)) {
  plan = Action_plans_to_row$action_plan[j]
  ending_ind = Action_plans_to_row$index[j]
  pdds = Action_Plans_to_PDD[Action_Plans_to_PDD$GrantNo == plan,]$pdd 
  years = Action_Plans_to_PDD[Action_Plans_to_PDD$GrantNo == plan,]$year
    
  pdd_listed = as.list(unlist(strsplit(pdds, '[[:space:]]')))
  years_listed = as.list(unlist(strsplit(years, '[[:space:]]')))
  
  num_pdd = length(pdd_listed)
  
  while (z < ending_ind + 1) {
    for(k in 1:num_pdd) {
      pdd_num = paste("pdd",toString(k))
      year_num = paste("year", toString(k))
      if(k == 1) {
        new_row = data.frame(x = c(pdd_listed[k]), y = c(years_listed[k]))
        names(new_row)[1] = pdd_num
        names(new_row)[2] = year_num
      } else {
        new_col = data.frame(x = c(pdd_listed[k]), y = c(years_listed[k]))
        names(new_col)[1] = pdd_num
        names(new_col)[2] = year_num
        
        new_row = cbind(new_row, new_col)
      }
    }
    
    diff = 12 - (k*2)
    empty_space = rep("", diff)
    empty_space = as.data.frame(t(empty_space))
    e = k
    
    l = 1
    while(l <= diff) {
      names(empty_space)[l] = paste("pdd",toString(e + 1))
      names(empty_space)[l + 1] = paste("year", toString(e + 1))
      l = l + 2
      e = e + 1
    }
    
    new_row = cbind(new_row, empty_space)
    
    z = z + 1
    
    
    if(j == 1) {
      columns_to_add = new_row
      for(o in 1:31) {
        columns_to_add = rbind(columns_to_add, new_row)
      }
        
    } else {
      columns_to_add = rbind(columns_to_add, new_row)
    }
    
  }
  
}

All_Projects = cbind(All_Projects, columns_to_add)

write.csv(All_Projects, file = "CDBG-DR Excel Only_All Projects w PDDs 210126.csv",row.names = FALSE)