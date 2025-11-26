# Function to generate unique intervals for groups A and B
# and ensure no duplicates across groups

fnc_gen_uniq_intervals <- function(inf, sup, n, lambda = 1, used) {
  
  doRepeat = TRUE
  
  while (doRepeat) {
    
    pretender = round(rexp(n * 200, rate = lambda), 3)
    pretender = pretender[pretender > inf & pretender <= sup]
    pretender = setdiff(unique(pretender), used)
    
    if (length(pretender) >= n) return(sample(pretender, n))
    
  }
}

fnc_groups_wo_duplicates <- function(group_name, used) {
  
  times = unlist(lapply(0:4, function(i) {
    
    new = fnc_gen_uniq_intervals(i, i + 1, 20, lambda = 1, used)
    used <<- c(used, new)
    
    new
    
  }))
  
  data.frame(
    Time = times,
    Group = group_name
  )
  
}

fnc_groups_wo_duplicates_mutant <- function(group_name, used) {
  
  times = unlist(lapply(0:2, function(i) {
    
    new = fnc_gen_uniq_intervals(
      i,
      i + 1,
      round(100 / 3, 0),
      lambda = 1,
      used
    )
    
    used <<- c(used, new)
    new
    
  }))
  
  data.frame(
    Time = times,
    Group = group_name
  )
  
}

fnc_data_generator <- function() {
  
  set.seed(123)
  
  times_usedA = c()
  groupA = fnc_groups_wo_duplicates("A", times_usedA)
  
  times_usedB = c(times_usedA, groupA$Time)
  groupB = fnc_groups_wo_duplicates("B", times_usedB)
  
  data = rbind(groupA, groupB)
  
  return(data)
}

data <- fnc_data_generator()

# # Verify
# 
# ## Duplicated values
# print(ifelse(
#   any(duplicated(data$Time)),
#   "With duplicated values",
#   "Without duplicated values"
# ))
# 
# ## Same number of observations
# print(table(data$Group, cut(data$Time, breaks = seq(0, 5, 1))))
# 
# # Histograms
# par(mfrow = c(2, 2))
# hist(data$Time[data$Group == "A"], breaks = 0:5, main = "Group A")
# hist(data$Time[data$Group == "B"], breaks = 0:5, main = "Group B")
# hist(data$Time, breaks = 0:5, main = "Both groups")
# 
# # Save the data
# write.csv(data, "data.csv", row.names = FALSE, quote = FALSE)
