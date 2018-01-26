Model_3_bsaiwg-cg-eg
tem_3_2015.dat
3     #nareas
3     #SRType 3=average, 2=Bholt, 1=Ricker
1930  #start year rec estimate [styr_rec_est]  
2014  #end year rec est [endyr_rec_est]
2     #rec likelihood type [rec_like_type]  ONLY 2 IS FUNCTIONAL

1     #phase F - phase for fishing mortality deviations 
2     #ph avg F - phase for estim. avg fishing mortality 1
4     #ph rec dev - phase for estim. recruitment deviations 1
1     #ph fish1 sel - phase for estim. fishing selectivity 3
1     #ph fish4 sel
3     #ph srv sel - phase for estim. survey selectivity (was originally phase four)
0.1   #M1 prior - prior mean for M
0.1   #M2 prior
0.1   #M3 prior
0.01 #CV M1 prior
0.01 #CV M2 prior
0.01 #CV M3 prior
-1    #phase estimating M

1.2   #prior mean for recruitment devs (sigr)  #was 1.2
0.2   #prior CV for recr. devs  
-2    #phase for recr. devs

7.857 #q_srv1prior A1    //prior mean for q coefficient  
0.33  #prior CV for q coeff
3 #1     #phase for estim. q coeff  

4.693 #q_srv2prior    // Prior mean for catchability coefficient
0.242 #cvq_srv2prior  // Prior CV for catchability coefficient
5 #4     #ph_q_srv2      // Phase for estimating catchability 2

4.967 #q_srv5prior A1   // Prior mean for catchability coefficient
0.328 #cvq_srv5prior  // Prior CV for catchability coefficient
3    #ph_q_srv5       // Phase for estimating catchability
4.967 #q_srv5prior A2   // Prior mean for catchability coefficient
0.328 #cvq_srv5prior  // Prior CV for catchability coefficient
3    #ph_q_srv5       // Phase for estimating catchability
4.967 #q_srv5prior A3   // Prior mean for catchability coefficient
0.328 #cvq_srv5prior  // Prior CV for catchability coefficient
3    #ph_q_srv5       // Phase for estimating catchability

4.967 #q_srv6prior    // Prior mean for catchability coefficient
0.328 #cvq_srv6prior  // Prior CV for catchability coefficient
4 #3    #ph_q_srv6       // Phase for estimating catchability

4.967 #q_srv8prior A1    // Prior mean for catchability coefficient
0.328 #cvq_srv8prior  // Prior CV for catchability coefficient
3    #ph_q_srv8       // Phase for estimating catchability
4.967 #q_srv8prior A2   // Prior mean for catchability coefficient
0.328 #cvq_srv8prior  // Prior CV for catchability coefficient
3    #ph_q_srv8       // Phase for estimating catchability
4.967 #q_srv8prior A3   // Prior mean for catchability coefficient
0.328 #cvq_srv8prior  // Prior CV for catchability coefficient
3    #ph_q_srv8       // Phase for estimating catchability

50    #weight for catch estimation wt_ssqcatch_fish1
50    #weight for catch estimation wt_ssqcatch_fish3
4 #16    #wt_srv3              // Weight for ll survey RPN (US)
4 #16     #wt_srv4              // Weight for survey biomass estimation
1 #4     #wt_srv5              // Weight for 
1 #4     #wt_srv6              // Weight for 
#0     #wt_srv8              // Weight for 
2.8 #1     #weight for fishery age comps fish1_age
8.0
6.6
0.8 #1     #weight for survey age compositions srv1_age  
3.3
2.8
0     #weight for survey age comps srv2_age
2 #2.5     # wt_fish1_size   m1       // Weight for fishery size compositions  
6.5
3 #3.25
3.2 #3.5     #wt fish1 size f1
12.9
14 #14.6
2.4     # wt_srv1_size  m1         // Weight for survey size compostiions  
8.1
8.2
10.6   # wt srv1 size f1
8.3
8
0     # wt_fish2_size       //
11.5   # wt_srv2_size m1
8.8
7.2
3.8     #srv2_size f1
7.75
5.3     
1.7   # wt_fish3_size m1      // 
2
2.05
3.85   # wt_fish3_size f1
4.75
4.75
0     # wt_fish4_size       // 
0     #wt_srv5              //
0     #wt_fish6             // 
0     #wt_srv6              //

0.1 #weight for estimation recruitment variations
10  # wt_sel_reg_fish3
10  # wt_sel_dome_fish3
0.1   # weight on fishing mortality regularity

#-3   # ph_Rzero;              // Phase for Rzero, need to turn off for base-case
1 1 1 # hist_hal_prop;         // additional data for BS flag
1    # ph_ifq;
1    # ph_srv2q;
1 #0.5256  # yield ratio area 1 - calculated in dat file maker R
1 #0.9782  # yield ratio area 2
1.0000  # yield ratio area 3
45 #num projection years

