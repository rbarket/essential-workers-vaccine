source('setup1dose.R') #NOTE: don't really need essentials...

tsize <- 16 # text size

#--- Define variables (these are mostly made up)
# Adjust these based on lit. review!!!
ve1 <- c(rep(0.75,9),rep(0.32,8)) #with one dose 10-19, 80+ (efficacy against infection)
vp1 <- c(rep(0.85,9),rep(0.38,8)) #with one dose 10-19, 80+ (efficacy against adverse outcomes)

# now construct contact matrix (any R, will be rescaled)
C <- construct_C_from_prem(home=mu_home, work=mu_work, school=mu_school, other=mu_other, u=u_var,
                          target_R0=1.0, in_school=TRUE)

#################################################################
# SEE DATA HERE:https://covid19tracker.ca/vaccinationtracker.html
# AS of May 29:  
# 80+: 93% (plateaued)
# 70-79: 90% (plateaued)
# 60-69: 83% (plateaued)
# 50-59: 73% (plateaued)
# 40-49: 70%
# 30-39: 60%
# 20-29: 47%
# 10-19: 7%
# reopening plan starts: ~ May 30 (check dates)
# second dose mass vaccination starts: ~ Jun 7 (check dates)
# approx. 50k does per day
# It seems reasonable that by the end of June, we can have
# everyone with second doses up to ~73% hesitancy in younger grps
##################################################################
# Initialize population with 2 doses (assume some essential/nonessential split)
# administer first does up until 73% coverage among all eligible age groups
# assume will take about one week per age group at rate of 50k/day
# then move on to second doses (hack: incr. ve by age group)

# Hesitancy (One dose group should have 0 hesitancy)
# Hesitancy gotten from BC-Covid 19 Surveillance dashboard 
H = c(0.0,0.21,0.11,0.16,0.15,0.14,0.11,0.09,0.07,
      0,0,0,0,0,0,0,0)*N_i # with 0-9, 10-19 one dose 

# initial cases (taken from May 29...but should be lower)
# source: http://www.bccdc.ca/Health-Info-Site/Documents/COVID_sitrep/Week_21_2021_BC_COVID-19_Situation_Report.pdf
# prevalence... hacky split between one-dose and others
I0 <- c(414, 596*0.80, 1089*0.80, 977*0.80, 643*0.80, 552*0.80, 362*0.80, 182*0.80, 100*0.80,
             596*0.20, 1089*0.20, 977*0.20, 643*0.20, 552*0.20, 362*0.20, 182*0.20, 100*0.20)
V0 <- N_i-H
# Just run for one day to get df set up...
df0 <- run_sim_basic(C, I0, percent_vax=1.0, strategy=list(2:17), num_perday=1.0, 
                        v_e=ve1, v_p=vp1,
                        u = u_var, num_days=2, H=H, 
                        one_dose=TRUE)


# Check proportion vaccinated
display_prop_vax(combine_age_groups(df0),startDate=lubridate::ymd("2021-07-01"),
                             '', stages=NULL, textsize=16) + 
    scale_y_continuous(minor_breaks = seq(0 , 1, 0.05), breaks = seq(0, 1,0.1))

# Define scenarios
# TO DO: Combine run_sim_ramp with run_sim_restart (should be very straightforward)
# then the two scenarios can change the number of ramp days
# Also to do: modify setup script to remove essential workers (but still need to generate contact matrix
# to get the ages by ten. Maybe add an option to that script?)
reopen_scen = function(ramp_days, Rfinal=3.0){
   # Run for 60 days
   alpha=0.0
   T <- 180
   n <- 0.0 # finished
   C <- construct_C_from_prem(home=mu_home, work=mu_work, school=mu_school, other=mu_other, u=u_var,
                              target_R0=Rfinal, in_school=TRUE, alpha_factor=alpha)
   
   df <- run_sim_restart(C, df_0=tail(df0, n=1), percent_vax = 1.0, strategy=list(2:17), num_perday=n,
                        v_e =ve1, v_p=vp1,
                        u = u_var, num_days=T,H=H, one_dose=TRUE) # add ramp days
   # add pars
   df$ramp_days <- ramp_days    
   return(df)}


df1 <- reopen_scen(ramp_days=1.0, R=2) # change R value depending on current covid data
df2 <- reopen_scen(ramp_days=1.0, R=3) 

d_E= 1/3

# If using df2, make sure to change R value to desired value
Es = df2[,grep("E", colnames(df2))] # this is 27 columns of Es


finaldf <- Es[1:9] %>% rename(`I1_zero`=E1, `I2_zero`=E2, `I3_zero`=E3, `I4_zero`=E4,
                              `I5_zero`=E5, `I6_zero`=E6, `I7_zero`=E7, `I8_zero`=E8, `I9_zero`=E9)

#One dose
# finaldf['I1_one'] = I_df['Ive1'] + I_df['Ixe1'] # 0-9 cant get vaccinated
finaldf['I2_one'] = Es['Eve2'] + Es['Eve2']
finaldf['I3_one'] = Es['Eve3'] + Es['Exe3']
finaldf['I4_one'] = Es['Eve4'] + Es['Exe4']
finaldf['I5_one'] = Es['Eve5'] + Es['Exe5']
finaldf['I6_one'] = Es['Eve6'] + Es['Exe6']
finaldf['I7_one'] = Es['Eve7'] + Es['Exe7']
finaldf['I8_one'] = Es['Eve8'] + Es['Exe8']
finaldf['I9_one'] = Es['Eve9'] + Es['Exe9']

#Two Dose
# finaldf['I1_two'] = Es['Ev1'] + Es['Ex1'] #0-9 cant get vaccinated
finaldf['I2_two'] = Es['Ev2'] + Es['Ex2']
finaldf['I3_two'] = Es['Ev3'] + Es['Ex3']
finaldf['I4_two'] = Es['Ev4'] + Es['Ex4']
finaldf['I5_two'] = Es['Ev5'] + Es['Ex5']
finaldf['I6_two'] = Es['Ev6'] + Es['Ex6']
finaldf['I7_two'] = Es['Ev7'] + Es['Ex7']
finaldf['I8_two'] = Es['Ev8'] + Es['Ex8']
finaldf['I9_two'] = Es['Ev9'] + Es['Ex9']

#Total cases per age group
# finaldf['I1_two'] = Es['Ev1'] + Es['Ex1'] #0-9 cant get vaccinated
finaldf['I2_two'] = Es['Ev2'] + Es['Ex2']
finaldf['I3_two'] = Es['Ev3'] + Es['Ex3']
finaldf['I4_two'] = Es['Ev4'] + Es['Ex4']
finaldf['I5_two'] = Es['Ev5'] + Es['Ex5']
finaldf['I6_two'] = Es['Ev6'] + Es['Ex6']
finaldf['I7_two'] = Es['Ev7'] + Es['Ex7']
finaldf['I8_two'] = Es['Ev8'] + Es['Ex8']
finaldf['I9_two'] = Es['Ev9'] + Es['Ex9']

#incidence
finaldf <- finaldf*d_E

#ICU rate (based on BC covid surveillance data from first week Sept.)
ICU_rates = c(7/8179, 
            rep(c(14/15450, 42/32483, 144/26625, 181/21315, 341/18054, 437/11639, 435/6012, 157/3292), times=3)
            )
ICUdf <- data.frame(mapply(`*`,finaldf,ICU_rates)) # final results

#Track R compartments instead
#totals <- rowSums(finaldf)
#prop_df <- finaldf/totals #proportions
#finaldf['total'] = totals

# Add a total column
totals <- rowSums(ICUdf)
ICUdf['total'] = totals

# Only output total column, if wanting infections for each age group, output ICUdf
write.xlsx(ICUdf['total'],"ModelR3.xlsx") # rename file based on which R value used (look at df1/2)

# compare trajectories
trajectories <- compare_sims(sim1 = df1, 
                             sim2 = df2,
                             name1 = 'first scenario', 
                             name2 = 'second scenario', 
                             startDate=ymd("2021-07-01"), 
                             textsize = tsize)

ggarrange(plotlist=trajectories, align="v")

