Model_1_bsaiwgcgeg
tem_single2015.dat
1     #nareas
3     #SRType 3=average, 2=Bholt, 1=Ricker  
1930  #start year rec estimate [styr_rec_est]  or 1930?
2013  #end year rec est [endyr_rec_est]
2     #rec likelihood type [rec_like_type]  ONLY 2 IS FUNCTIONAL

2#3     #phase F - phase for fishing mortality deviations
1     #ph avg F - phase for estim. avg fishing mortality
2     #ph rec dev - phase for estim. recruitment deviations
3     #ph fish sel - phase for estim. fishing selectivity
3     #ph fish4 sel
4     #ph srv1 sel - phase for estim. survey selectivity (was originally phase four)

0.1   #M prior - prior mean for M
0.001 #CV M prior
-1    #phase estimating M

1.2   #prior mean for recruitment devs
0.2   #prior CV for recr. devs  
-2     #phase for recr. devs

7.857 #prior mean for q srv1 coefficient
0.33  #prior CV for q coeff
3     #phase for estim. q coeff

4.693 #q_srv2prior    // Prior mean for catchability coefficient
0.242 #cvq_srv2prior  // Prior CV for catchability coefficient
3     #ph_q_srv2      // Phase for estimating catchability

4.967 #q_srv5prior    // Prior mean for catchability coefficient
0.328 #cvq_srv5prior  // Prior CV for catchability coefficient
3    #ph_q_srv5       // Phase for estimating catchability

4.967 #q_srv6prior    // Prior mean for catchability coefficient
0.328 #cvq_srv6prior  // Prior CV for catchability coefficient
3    #ph_q_srv6       // Phase for estimating catchability

4.967 #q_srv8prior    // Prior mean for catchability coefficient
0.328 #cvq_srv8prior  // Prior CV for catchability coefficient
3    #ph_q_srv8       // Phase for estimating catchability

100    #weight for catch estimation fish1
100    #weight for catch estimation fish3
#2     #weight for survey biomass estim. srv1
4     #wt_srv3              // Weight for survey N estimation
4     #wt_srv4              // Weight for survey biomass estimation
1     #wt_srv5              // Weight for 
1     #wt_srv6              // Weight for 
#0     #wt_srv8              // Weight for 
7.3     #weight for fishery age comps fish1_age 1
8     #weight for survey age compositions srv1_age 1
0     #weight for survey age comps srv2_age 1
14     # wt_fish1_size          // Weight for fishery size compositions
12     # wt_srv1_size           // Weight for survey size compostiions
0     # wt_fish2_size       // weight for ?? size comps DON'T NEED THIS ONE
10.5     # wt_srv2_size        // weight for ?? THIS WAS 1, I MADE IT 0
3.5   # wt_fish3_size       // weight f
0     # wt_fish4_size       // DON'T NEED THIS ONE
0     #wt_srv5              //
0     #wt_fish6             // WHAT IS FISH6??
0     #wt_srv6              //

0.1 #weight for estimation recruitment variations (0.1 to match DHH model)
10  # wt_sel_reg_fish3       // Weight on fishery selectivity regularity penalty
10  # wt_sel_dome_fish3      // Weight on fishery selectivity dome-shape penalty   
0.1   # weight on fishing mortality regularity  1

#-3   # ph_Rzero;              // Phase for Rzero, need to turn off for base-case
1.0  # hist_hal_prop;         // additional data for BS flag
1    # ph_ifq;
1   # ph_srv2q;
1 # yield ratio - calculated in dat file maker R
