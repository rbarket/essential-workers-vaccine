#p_1dose <- c(0.1,0.1,0.1705,0.2043,0.1675,0.1536,0.1642,0.1069, 0.1069) #p_oneD
#age_demo_by_five <- as.matrix(readr::read_csv('../data/Population_Estimates.csv')) INPUTS SHOULD BE CHANGED WHEN YOU CALL THIS FUNCTION 

generate_contact_matrix = function(p_1dose, age_demo_by_fives){
#BEGINNING OF generate_contact_matrix
age_demo <- age_toTens(age_demo_by_five)
pop_total <- sum(age_demo)


num_per_age <- age_demo[1:9]


# Re-organize the demographics
num_essential <- num_per_age*p_1dose #num_oneD

new_age_demo <-c(num_per_age - num_essential, #num wfh
                 num_essential[2:9], #num essential (10-80+)
                 pop_total*pop_total)/pop_total 

# proportion of each grp over its age grp
prop_by_grp <- c(1-num_essential/num_per_age, num_essential[2:9]/num_per_age[2:9])

num_per_grp <- head(new_age_demo,-1)*pop_total


# LOAD IN PREM CONTACT MATRICES
mu_home <- as.matrix(read.csv("../data/mu_home.csv", sep=",", header=FALSE))
mu_work <- as.matrix(read.csv("../data/mu_work.csv", sep=",", header=FALSE))
mu_school <- as.matrix(read.csv("../data/mu_school.csv", sep=",", header=FALSE))
mu_other <- as.matrix(read.csv("../data/mu_other.csv", sep=",", header=FALSE))


# put each matrix into 10 yr age bins using BC demographics
# add 80+ compartment according to Bubar model
mu_home <- add_80bin(matrix_toTens(mu_home, age_demo_by_five))
mu_work <- add_80bin(matrix_toTens(mu_work, age_demo_by_five))
mu_school <- add_80bin(matrix_toTens(mu_school, age_demo_by_five))
mu_other <- add_80bin(matrix_toTens(mu_other, age_demo_by_five))

# Look at est. total num contacts & enforce symmetry here
mu_home_total <- 0.5*(mu_home*num_per_age + t(mu_home*num_per_age))
mu_work_total <- 0.5*(mu_work*num_per_age + t(mu_work*num_per_age))
mu_school_total <- 0.5*(mu_school*num_per_age + t(mu_school*num_per_age))
mu_other_total <- 0.5*(mu_other*num_per_age + t(mu_other*num_per_age))

# Add essential worker comparments
mu_home_essential <- matrix_addEssential(mu_home_total, prop_by_grp)/num_per_grp
mu_school_essential <- matrix_addEssential(mu_school_total, prop_by_grp)/num_per_grp
mu_other_essential <- matrix_addEssential(mu_other_total, prop_by_grp)/num_per_grp
mu_work_essential <- matrix_addEssential(mu_work_total, prop_by_grp)

# For 70-79, and 80+..move all work compartments to essential 
# also fudge
#mu_work_essential['70-79', 10:14] <- 1*(mu_work_essential['70-79', 10:14] + mu_work_essential['70-79', 3:7])
#mu_work_essential[10:14,'70-79'] <- 1*(mu_work_essential[10:14,'70-79'] + mu_work_essential[3:7,'70-79'])
#mu_work_essential['70-79', 3:7] <- 0.0
#mu_work_essential[3:7,'70-79'] <- 0.0
#mu_work_essential['80+', 10:15] <- 1*(mu_work_essential['80+', 10:15] + mu_work_essential['80+', 3:8])
#mu_work_essential[10:15,'80+'] <- 1*(mu_work_essential[10:15,'80+'] + mu_work_essential[3:8,'80+'])
#mu_work_essential['80+', 3:8] <- 0.0
#mu_work_essential[3:8,'80+'] <- 0.0

mu_work_essential <- mu_work_essential/num_per_grp
#we may want to relabel these!
saveRDS(mu_home_essential, "../generated-data/mu_home_essential.rds")
saveRDS(mu_work_essential,  "../generated-data/mu_work_essential.rds")
saveRDS(mu_school_essential, "../generated-data/mu_school_essential.rds")
saveRDS(mu_other_essential,  "../generated-data/mu_other_essential.rds")
saveRDS(new_age_demo, "../generated-data/age_demographics_essential.rds")

#end of generate_contact_matrix
}

age_toTens = function(x){
  # helper func to convert to tens
  res <- rep(0, 9)
  j <- 1
  for (i in 1:9*2){
    res[j] <- x[i]+x[i-1]
    j <- j+1
  }
  # get 80+ bin
  res[9] <- res[9] + x[19]
  
  return(res)
}


matrix_toTens <- function(C_byfives, pop_demo) {
  # Modified from Bubar et al.
  # https://github.com/kbubar/vaccine_prioritization/tree/v1 
  l <- dim(C_byfives)[1]
  C_bytens <- matrix(nrow = l/2, ncol = l/2)
  
  col_count <- 1
  for (col in seq(1,l,by = 2)){
    row_count <- 1
    for (row in seq(1,l, by = 2)){
      p1 <- pop_demo[row]
      p2 <- pop_demo[row + 1]
      C_bytens[row_count, col_count] <- ((C_byfives[row, col] + C_byfives[row, col + 1])*p1 +
                                           (C_byfives[row + 1, col] + C_byfives[row + 1, col + 1])*p2)/(p1+p2)
      row_count <- row_count + 1
    }
    col_count <- col_count + 1
  }
  colnames(C_bytens) <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79")
  rownames(C_bytens) <- colnames(C_bytens)
  
  return(C_bytens)
}

add_80bin <- function(C_bytens){
  # Modified from Bubar et al.
  # https://github.com/kbubar/vaccine_prioritization/tree/v1 
  # INPUT:
  #  C_bytens: Contact matrix with 10 year age bins, with last bin = 70-79
  #
  # OUTPUT:
  # C_new: Same C matrix with new row & col for 80+ age bin
  
  bin_70 <- dim(C_bytens)[1]
  bin_80 <- bin_70 + 1
  bin_60 <- bin_70 - 1
  
  C_new <- matrix(nrow = bin_80, ncol = bin_80)
  colnames(C_new) <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
  rownames(C_new) <- colnames(C_new)
  
  # fill rows for 80+ with same data from 70-79
  C_new[1:bin_70, 1:bin_70] <- C_bytens
  C_new[2:bin_80, bin_80] <- C_bytens[1:bin_70, bin_70]
  C_new[bin_80, 2:bin_80] <- C_bytens[bin_70, 1:bin_70]
  
  C_new[1, bin_80] <- C_new[1, bin_70]
  C_new[bin_80, 1] <- C_new[bin_70, 1]
  
  # Assumption: 80+ have similar (but less) contact rates as 70-79, with increased contact with 70-80+ (long term living facilities)
  # Implementation: Decrease contact for bins 0 - 69 for 80+  by 10% then split these contacts between 70-79 & 80+
  
  # col 80+
  to_decrease <- 0.1 * C_new[1:bin_60, bin_80]
  C_new[1:bin_60, bin_80] <- 0.9 * C_new[1:bin_60, bin_80]
  
  to_increase <- sum(to_decrease)/2
  C_new[bin_70, bin_80] <- C_new[bin_70, bin_80] + to_increase
  C_new[bin_80, bin_80] <- C_new[bin_80, bin_80] + to_increase
  
  # row 80+
  to_decrease <- 0.1 * C_new[bin_80, 1:bin_60]
  C_new[bin_80, 1:bin_60] <- 0.9 * C_new[bin_80, 1:bin_60]
  
  to_increase <- sum(to_decrease)/2
  C_new[bin_80, bin_70] <- C_new[bin_80, bin_70] + to_increase
  C_new[bin_80, bin_80] <- C_new[bin_80, bin_80] + to_increase
  
  # fudge a little bit
  C_new[bin_80, bin_80] <- 3*C_new[bin_80, bin_80]
  C_new[bin_70, bin_70] <- 2*C_new[bin_70, bin_70]
  C_new[bin_80, bin_70] <- 2*C_new[bin_80, bin_70]
  C_new[bin_70, bin_80] <- 2*C_new[bin_70, bin_80]
  return(C_new)
}

#only changed function
matrix_addEssential = function(C, p_grp){
  # C should be by tens, with 80+ (9x9) 
  # C(i,j) should be total number of contacts between age classes i & j
  # evenly distribute contacts according to demog data
  # p_grp is the prop. of the total age grp over all grps  
  C_e <- matrix(nrow=17, ncol=17,0)
  colnames(C_e) <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", 
                     "60-69", "70-79", "80+","0-9o","10-19o", "20-29o", "30-39o", "40-49o", "50-59o", "60-69o", "70-79o")
  rownames(C_e) <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", 
                     "60-69", "70-79", "80+","0-9o","10-19o", "20-29o", "30-39o", "40-49o", "50-59o", "60-69o", "70-79o")
  # Now copy & extend C
  C_e[1:9,1:9] <- C
  C_e[10:17,1:9] <- C[2:9,] 
  C_e[,10:17] <- C_e[,2:9]#i dont understand whats happening here :(
  # Now scale by p_grp to allocate contacts evenly
  C_e <- t(t(C_e)*p_grp)*p_grp
  
  return(C_e)
}
