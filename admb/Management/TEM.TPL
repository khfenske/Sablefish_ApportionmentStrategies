//==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+
//
//  Statistical, separable age-structured population model for sablefish
//  Alaska Fisheries Science Center, October 2008
//  D. Hanselman:dana.hanselman@noaa.gov
//  Input file:   tem.dat, as in tem(plate)
//  Control file: tem.ctl
//  Program file: tem.tpl
//  Output files: tem.rep, tem.std, proj.dat, newproj.dat
//  Sablefish model 
// Split sexes, seperate weight at ages, age-length keys and unsexed
// srv1= Domestic longline survey RPW
// srv2= Japanese longlin survey
// srv3= Domestic LL survey RPN
// srv4 = Japanese LL survey RPN
// srv5 = Domestic LL fishery RPW
// srv6 = Japanese LL fishery RPW
// srv7= NMFS bottom trawl survey (currently GOA only)
// fish1= U.S. Longline fishery
// fish2= Japanese LL fishery
// fish3= U.S. trawl fishery
// fish4= Japanese trawl fishery
// Changes in August 2016: 
// added SARA outputs, new SDNR summaries at end
// standard deviation for SSB
// Fixed hessian problem on tot_bio_proj(endyr+1)
// revised sable-r-report.cxx, and mhp-func.cpp to reflect new C++ protocols
// Added executive material sections and new report sable.rep with SDs
// Cleaned up a couple of messes like srv22, srv2q
//==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+
DATA_SECTION
  !!CLASS ofstream evalout("evalout.prj");
// Read data from the control file
  !! ad_comm::change_datafile_name("tem.ctl");    //Read in phases, penalties and priors from "tem.ctl"
  !! *(ad_comm::global_datafile) >>  model_name; 
  !! *(ad_comm::global_datafile) >>  data_file; 

  init_int    SrType                // 3=average, 2=Bholt, 1=Ricker
  init_int    styr_rec_est
  init_int    endyr_rec_est
  int nrecs_est;
  !! nrecs_est = endyr_rec_est-styr_rec_est+1;
  init_int    rec_like_type         //Type of likelihood specified 
  init_int    fsh1_sel_opt
  init_int    fsh2_sel_opt
  init_int    fsh3_sel_opt
  init_int    fsh4_sel_opt
  init_int    srv1_sel_opt
  init_int    srv2_sel_opt
  init_int    srv7_sel_opt
   

// Age of full selection for fishery and survey
  init_int    n_fish_sel_ages       // Age that fishery selectivity stops estimating
  init_int    n_srv1_sel_ages       // Age that survey selectivity stops estimating

// Phases that general parameter estimation begins
  init_int    ph_Fdev               // Phase for fishing mortality deviations
  init_int    ph_avg_F              // Phase for estimating average fishing mortality
  init_int    ph_recdev             // Phase for estimating recruitment deviations
  init_int    ph_fish_sel           // Phase for estimating fishing selectivity
  init_int    ph_srv1_sel           // Phase for estimating survey selectivity

// Priors
  init_number mprior                // Prior mean for natural mortality
  number log_mprior
  !! log_mprior = log(mprior);
  init_number cvmprior              // Prior CV for natural mortality
  init_int    ph_m                  //Phase for estimating natural mortality

  init_number steep_prior           // Prior mean for steepness
  init_number cv_steep_prior        // Prior CV for steepness
  init_int    ph_steepness          // Phase for steepness (not used in POP model), for estimating S-R parameters

  init_number sigrprior             // Prior mean for recruitment deviations
  init_number cvsigrprior           // Prior CV for recruitment deviations
  init_int    ph_sigr               // Phase for recruiment deviations

   init_number q_srv1prior           // Prior mean for catchability coefficient
  init_number cvq_srv1prior         // Prior CV for catchability coefficient
  init_int    ph_q_srv1             // Phase for estimating catchability
  number log_q1prior
  !! log_q1prior = log(q_srv1prior);
 init_number q_srv2prior           // Prior mean for catchability coefficient
  init_number cvq_srv2prior         // Prior CV for catchability coefficient
  init_int    ph_q_srv2             // Phase for estimating catchability
  number log_q2prior
  !! log_q2prior = log(q_srv2prior);
  init_number q_srv3prior           // Prior mean for catchability coefficient
  init_number cvq_srv3prior         // Prior CV for catchability coefficient
  init_int    ph_q_srv3             // Phase for estimating catchability
  number log_q3prior
  !! log_q3prior = log(q_srv3prior);
  init_number q_srv4prior           // Prior mean for catchability coefficient
  init_number cvq_srv4prior         // Prior CV for catchability coefficient
  init_int    ph_q_srv4             // Phase for estimating catchability
  number log_q4prior
  !! log_q4prior = log(q_srv4prior);
  init_number q_srv5prior           // Prior mean for catchability coefficient
  init_number cvq_srv5prior         // Prior CV for catchability coefficient
  init_int    ph_q_srv5             // Phase for estimating catchability
  number log_q5prior
  !! log_q5prior = log(q_srv5prior);
  init_number q_srv6prior           // Prior mean for catchability coefficient
  init_number cvq_srv6prior         // Prior CV for catchability coefficient
  init_int    ph_q_srv6             // Phase for estimating catchability
  number log_q6prior
  !! log_q6prior = log(q_srv6prior);
   init_number q_srv7prior           // Prior mean for catchability coefficient
  init_number cvq_srv7prior         // Prior CV for catchability coefficient
  init_int    ph_q_srv7             // Phase for estimating catchability
  number log_q7prior
  !! log_q7prior = log(q_srv7prior);
   init_number q_srv8prior           // Prior mean for catchability coefficient
  init_number cvq_srv8prior         // Prior CV for catchability coefficient
  init_int    ph_q_srv8             // Phase for estimating catchability
  number log_q8prior
  !! log_q8prior = log(q_srv8prior);
  init_int    yr_catchwt            // year catch-wt changes...... 
  init_number wt_ssqcatch_fish1           // Weight for catch estimation
  init_number wt_ssqcatch_fish3        // Weight for catch estimation
  init_number wt_srv1               // Weight for survey biomass estimation
   init_number wt_srv2               // Weight for survey biomass estimation
  init_number wt_srv3               // Weight for survey biomass estimation
  init_number wt_srv4               // Weight for survey biomass estimation
  init_number wt_srv5               // Weight for survey biomass estimation
  init_number wt_srv6               // Weight for survey biomass estimation
  init_number wt_srv7               // Weight for survey biomass estimation
  init_number wt_srv8               // Weight for survey biomass estimation
 init_number wt_fish1_age           // Weight for fishery age compositions
  init_number wt_srv1_age           // Weight for survey age compositions
  init_number wt_fish1_size          // Weight for fishery size compositions
  init_number wt_srv1_size          // Weight for survey size compostiions
  init_number wt_fish2_size          // Weight for fishery size compositions
  init_number wt_srv2_size          // Weight for survey size compostiions
   init_number wt_fish3_size          // Weight for fishery size compositions
  init_number wt_srv7_size          // Weight for survey size compostiions
  init_number wt_fish4_size          // Weight for fishery size compositions
  init_number wt_srv7_age          // Weight for survey size compostiions
  init_number wt_srv_extra_size          // Weight for fishery size compositions
  init_number wt_srv5_size          // Weight for survey size compostiions
  init_number wt_fish6_size          // Weight for fishery size compositions
  init_number wt_srv6_size          // Weight for survey size compostiions
 init_number wt_rec_var            // Weight for estimation recruitment variatiions penalty
  init_number wt_sel_reg_fish1       // Weight on fishery selectivity regularity penalty
   init_number wt_sel_reg_fish2       // Weight on fishery selectivity regularity penalty
  init_number wt_sel_reg_fish3       // Weight on fishery selectivity regularity penalty
  init_number wt_sel_reg_fish4       // Weight on fishery selectivity regularity penalty
  init_number wt_sel_dome_fish1     // Weight on fishery selectivity dome-shape penalty   
  init_number wt_sel_dome_fish2     // Weight on fishery selectivity dome-shape penalty   
  init_number wt_sel_dome_fish3     // Weight on fishery selectivity dome-shape penalty   
  init_number wt_sel_dome_fish4     // Weight on fishery selectivity dome-shape penalty   
  init_number wt_sel_reg_srv1       // Weight on survey selectivity regularity penalty
  init_number wt_sel_reg_srv2       // Weight on survey selectivity regularity penalty
  init_number wt_sel_dome_srv1      // Weight on survey selectivity dome-shape penalty
  init_number wt_sel_dome_srv2      // Weight on survey selectivity dome-shape penalty
  init_number wt_fmort_reg          // Weight on fishing mortality regularity
  init_number ph_Rzero;       // Phase for Rzero, need to turn off for base-case
  init_number hist_hal_prop;          // additional data for BS flag
  init_number ph_mdelta;
  init_number ph_ifq;
  init_number ph_srv2_q2;
  init_number yieldratio;
    
    !! cout<<"done with ctl"<<endl;
    !! ad_comm::change_datafile_name(data_file);    // Read data from the data file
// Start and end years, recruitment age, number of age and length bins
  init_int      styr
  init_int      endyr
  init_int      recage
  init_int      nages
  init_int      nlenbins
  init_vector   len_bin_labels(1,nlenbins)

  int styr_rec
  int styr_sp
  int endyr_sp
  int endyr_rec
  int nyrs
  !!  nyrs = endyr - styr + 1;
  !!  styr_rec = (styr - nages) + 1;     // First year of recruitment
  !! styr_sp  = styr_rec - recage ;     // First year of spawning biomass  
  !! endyr_sp = endyr   - recage - 1;   // endyr year of (main) spawning biomass
  !! endyr_rec= endyr_rec_est;  // 
  vector yy(styr,endyr);
  !! yy.fill_seqadd(styr,1) ;
  vector aa(1,nages);
  !! aa.fill_seqadd(recage,1) ;

  int ph_F50;
  !! ph_F50 = 6;

  init_number spawn_fract; // Spawning Month
  !! spawn_fract = (spawn_fract - 1) / 12;

// Natural mortality, proportion mature and weight at age
  init_vector   p_mature(1,nages)
  init_vector   wt_m(1,nages)
  init_vector   wt_f(1,nages)
  init_vector   wt_all(1,nages)
  init_vector   wt_old(1,nages)
  vector wt_mature(1,nages);                  // Weight of mature fish vector at age
  init_vector prop_m(styr,endyr);  // proportion of males in RPN
  init_vector prop_m2(styr,endyr);  // proportion of males in RPN
  !! wt_mature = elem_prod(wt_f,p_mature);
// Observed catches
  init_vector   obs_catch_fish1(styr,endyr)
  init_vector   obs_catch_fish3(styr,endyr)


// Survey biomass estimates
  init_int      nyrs_srv1               // number of years of survey biomass estimates
  init_ivector  yrs_srv1(1,nyrs_srv1)         // years survey conducted in
  init_vector   obs_srv1_biom(1,nyrs_srv1)      // mean estimate of biomass
  init_vector   obs_srv1_se(1,nyrs_srv1)        // standard error of survey biomass estimates
  init_vector   obs_srv1_lci(1,nyrs_srv1)       // lower confidence interval, for graphing not used in estimation
  init_vector   obs_srv1_uci(1,nyrs_srv1)       // upper confidence interval

  init_int      nyrs_srv2               // number of years of survey biomass estimates
  init_ivector  yrs_srv2(1,nyrs_srv2)         // years survey conducted in
  init_vector   obs_srv2_biom(1,nyrs_srv2)      // mean estimate of biomass
  init_vector   obs_srv2_se(1,nyrs_srv2)        // standard error of survey biomass estimates
  init_vector   obs_srv2_lci(1,nyrs_srv2)       // lower confidence interval, for graphing not used in estimation
  init_vector   obs_srv2_uci(1,nyrs_srv2)       // upper confidence interval
 !! cout<<"Number of srv1 rpn is:"<<nyrs_srv2<<endl;

  init_int      nyrs_srv3               // number of years of survey biomass estimates
  init_ivector  yrs_srv3(1,nyrs_srv3)         // years survey conducted in
  init_vector   obs_srv3_biom(1,nyrs_srv3)      // mean estimate of biomass
  init_vector   obs_srv3_se(1,nyrs_srv3)        // standard error of survey biomass estimates
  init_vector   obs_srv3_lci(1,nyrs_srv3)       // lower confidence interval, for graphing not used in estimation
  init_vector   obs_srv3_uci(1,nyrs_srv3)       // upper confidence interval

  init_int      nyrs_srv4               // number of years of survey biomass estimates
  init_ivector  yrs_srv4(1,nyrs_srv4)         // years survey conducted in
  init_vector   obs_srv4_biom(1,nyrs_srv4)      // mean estimate of biomass
  init_vector   obs_srv4_se(1,nyrs_srv4)        // standard error of survey biomass estimates
  init_vector   obs_srv4_lci(1,nyrs_srv4)       // lower confidence interval, for graphing not used in estimation
  init_vector   obs_srv4_uci(1,nyrs_srv4)       // upper confidence interval
  
  init_int      nyrs_srv5               // number of years of survey biomass estimates
  init_ivector  yrs_srv5(1,nyrs_srv5)         // years survey conducted in
  init_vector   obs_srv5_biom(1,nyrs_srv5)      // mean estimate of biomass
  init_vector   obs_srv5_se(1,nyrs_srv5)        // standard error of survey biomass estimates
  init_vector   obs_srv5_lci(1,nyrs_srv5)       // lower confidence interval, for graphing not used in estimation
  init_vector   obs_srv5_uci(1,nyrs_srv5)       // upper confidence interval

  init_int      nyrs_srv6               // number of years of survey biomass estimates
  init_ivector  yrs_srv6(1,nyrs_srv6)         // years survey conducted in
  init_vector   obs_srv6_biom(1,nyrs_srv6)      // mean estimate of biomass
  init_vector   obs_srv6_se(1,nyrs_srv6)        // standard error of survey biomass estimates
  init_vector   obs_srv6_lci(1,nyrs_srv6)       // lower confidence interval, for graphing not used in estimation
  init_vector   obs_srv6_uci(1,nyrs_srv6)       // upper confidence interval

  init_int      nyrs_srv7               // number of years of survey biomass estimates
  init_ivector  yrs_srv7(1,nyrs_srv7)         // years survey conducted in
  init_vector   obs_srv7_biom(1,nyrs_srv7)      // mean estimate of biomass
  init_vector   obs_srv7_se(1,nyrs_srv7)        // standard error of survey biomass estimates
  init_vector   obs_srv7_lci(1,nyrs_srv7)       // lower confidence interval, for graphing not used in estimation
  init_vector   obs_srv7_uci(1,nyrs_srv7)       // upper confidence interval
 !! cout<<"Number of trawl years:"<<nyrs_srv7<<endl;
 
  // Fishery age composition data
  init_int      nyrs_fish1_age            // number of years of fishery age compos
  init_vector  yrs_fish1_age(1,nyrs_fish1_age)    //the years of age comps
  init_vector   nsamples_fish1_age(1,nyrs_fish1_age)  // some measure of relative sample size for each age comp.
  init_matrix   oac_fish1(1,nyrs_fish1_age,1,nages)   // the actual year by year age comps
  number sdnr_fish1_age
// Survey age composition data
  init_int      nyrs_srv1_age             // number of years of survey age compositions
  init_ivector  yrs_srv1_age(1,nyrs_srv1_age)     // the years of survey age comps
  init_vector   nsamples_srv1_age(1,nyrs_srv1_age)  // some measure of relative sample size for each age comp.
  init_matrix   oac_srv1(1,nyrs_srv1_age,1,nages)   // the year by year age survey age comps
  number sdnr_srv1_age
  init_int      nyrs_srv2_age             // number of years of survey age compositions
  init_ivector  yrs_srv2_age(1,nyrs_srv2_age)     // the years of survey age comps
  init_vector   nsamples_srv2_age(1,nyrs_srv2_age)  // some measure of relative sample size for each age comp.
  init_matrix   oac_srv2(1,nyrs_srv2_age,1,nages)   // the year by year age survey age comps
  number sdnr_srv2_age
  !! cout<<"Number of ages srv2:"<<nyrs_srv2_age<<endl;
  
// Fishery size composition data
  init_int      nyrs_fish1_size             // number of years of fish1ery size comps
  init_ivector  yrs_fish1_size(1,nyrs_fish1_size)   // the years of fish1ery size comps
  init_vector   nsamples_fish1_size(1,nyrs_fish1_size)// some measure of relative sample size for each fish1ery comp
  init_matrix   osc_fish1_m(1,nyrs_fish1_size,1,nlenbins)// year by year fishery size comps
  init_matrix   osc_fish1_f(1,nyrs_fish1_size,1,nlenbins)// year by year fishery size comps
  number sdnr_fish1_size
  
  init_int      nyrs_fish2_size             // number of years of fish2ery size comps
  init_ivector  yrs_fish2_size(1,nyrs_fish2_size)   // the years of fishery size comps
  init_vector   nsamples_fish2_size(1,nyrs_fish2_size)// some measure of relative sample size for each fish2ery comp
  init_matrix   osc_fish2(1,nyrs_fish2_size,1,nlenbins)// year by year fishery size comps
   number sdnr_fish2_size
  !! cout<<"Number of fish 2 sizes:"<< nyrs_fish2_size <<endl;
  
   
  init_int      nyrs_fish3_size             // number of years of fish3ery size comps
  init_ivector  yrs_fish3_size(1,nyrs_fish3_size)   // the years of fish3ery size comps
  init_vector   nsamples_fish3_size(1,nyrs_fish3_size)// some measure of relative sample size for each fish3ery comp
  init_matrix   osc_fish3_m(1,nyrs_fish3_size,1,nlenbins)// year by year fishery size comps
  init_matrix   osc_fish3_f(1,nyrs_fish3_size,1,nlenbins)// year by year fishery size comps
  number sdnr_fish3_size
  !! cout<<"Number of fish 3 sizes:"<< nyrs_fish3_size <<endl;
  !! cout<<"Years fish 4 sizes:"<< yrs_fish3_size <<endl;

  
  init_int      nyrs_fish4_size             // number of years of fish3ery size comps
  init_ivector  yrs_fish4_size(1,nyrs_fish4_size)   // the years of fishery size comps
  init_vector   nsamples_fish4_size(1,nyrs_fish4_size)// some measure of relative sample size for each fish3ery comp
  init_matrix   osc_fish4(1,nyrs_fish4_size,1,nlenbins)// year by year fishery size comps
  number sdnr_fish4_size
 
  !! cout<<"Number of fish 4 sizes:"<< nyrs_fish4_size <<endl;
// Survey size composition data
  init_int      nyrs_srv1_size            // number of years of survey size comps
  init_ivector  yrs_srv1_size(1,nyrs_srv1_size)   // the years of survey size comps
  init_vector   nsamples_srv1_size(1,nyrs_srv1_size)//some measure of relative sample size for each survey size comp
  init_matrix   osc_srv1_m(1,nyrs_srv1_size,1,nlenbins)//year by year size comps
  init_matrix   osc_srv1_f(1,nyrs_srv1_size,1,nlenbins)//year by year size comps
  number sdnr_srv1_size
 
   !! cout<<"Number of srv1 size is:"<<nyrs_srv1_size<<endl;

    init_int      nyrs_srv2_size            // number of years of survey size comps
  init_ivector  yrs_srv2_size(1,nyrs_srv2_size)   // the years of survey size comps
  init_vector   nsamples_srv2_size(1,nyrs_srv2_size)//some measure of relative sample size for each survey size comp
  init_matrix   osc_srv2_m(1,nyrs_srv2_size,1,nlenbins)//year by year size comps
   init_matrix   osc_srv2_f(1,nyrs_srv2_size,1,nlenbins)//year by year size comps
  number sdnr_srv2_size
 
    init_int      nyrs_srv7_size            // number of years of survey size comps
  init_ivector  yrs_srv7_size(1,nyrs_srv7_size)   // the years of survey size comps
  init_vector   nsamples_srv7_size(1,nyrs_srv7_size)//some measure of relative sample size for each survey size comp
  init_matrix   osc_srv7_m(1,nyrs_srv7_size,1,nlenbins)//year by year size comps
  init_matrix   osc_srv7_f(1,nyrs_srv7_size,1,nlenbins)//year by year size comps
 number sdnr_srv7_size
 
    init_int      nyrs_srv7_age             // number of years of survey age compositions
  init_ivector  yrs_srv7_age(1,nyrs_srv7_age)     // the years of survey age comps
  init_vector   nsamples_srv7_age(1,nyrs_srv7_age)  // some measure of relative sample size for each age comp.
  init_matrix   oac_srv7(1,nyrs_srv7_age,1,nages)   // the year by year age survey age comps
 number sdnr_srv7_age
 
  !! cout<<"Number of survey age years is "<<nyrs_srv7_age<<endl;

  int phase_selcoff_fsh1 
  int phase_selcoff_fsh2 
  int phase_selcoff_fsh3 
  int phase_selcoff_fsh4 
  int phase_logist_fsh1
  int phase_logist_fsh2
  int phase_logist_fsh3
  int phase_logist_fsh4
  int phase_dlogist_fsh1
  int phase_dlogist_fsh2
  int phase_dlogist_fsh3
  int phase_dlogist_fsh4
  int phase_selcoff_srv1 
  int phase_selcoff_srv2 
  int phase_selcoff_srv7 
  int phase_logist_srv1 
  int phase_logist_srv2 
  int phase_logist_srv7 
  int phase_dlogist_srv1 
  int phase_dlogist_srv2 
  int phase_dlogist_srv7 

// Size-age transition matrix:  proportion at size given age
  init_matrix   sizeage_m(1,nages,1,nlenbins)     //size comp #1
  init_matrix   sizeage_f(1,nages,1,nlenbins)     //lets you add another size-age matrix, remove this here and in Get_Predicted section to use only one, or just have two identical
  init_matrix   sizeage_all(1,nages,1,nlenbins)     //lets you add another size-age matrix, remove this here and in Get_Predicted section to use only one, or just have two identical
  init_matrix   sizeage_m_new(1,nages,1,nlenbins)     //size comp #1
  init_matrix   sizeage_f_new(1,nages,1,nlenbins)     //lets you add another size-age matrix, remove this here and in Get_Predicted section to use only one, or just have two identical

// Ageing error transition matrix:  proportion at reader age given true age
  init_matrix   ageage(1,nages,1,nages)       // ageing error matrix
  init_number eof

     !! cout<<"The universal answer is "<<eof;

// Initialize some counting variables
  int iyr
  int i
  int j
 int ii
 int l
  vector offset(1,16);                                    // Multinomial "offset"
 LOCAL_CALCS
  
  if(rec_like_type>0) styr_rec=styr-nages+1;
   if(wt_rec_var==0)                
   {
     if (ph_sigr>0)                 
     {
       cout << "Warning, wt_rec_var is zero, so can't estimate sigr!@"<<endl;
       cout << "turning sigr off "<<endl;
       ph_sigr =-4;
       cout << "hit any key, then enter to continue"<<endl;
       char  xxx;
       cin >> xxx;
     }
   }
   switch (srv1_sel_opt)
   {
     case 1 : // Selectivity coefficients 
     {
       phase_selcoff_srv1 = ph_srv1_sel;
       phase_logist_srv1  = -1;
       phase_dlogist_srv1 = -1;
     }
     break;
     case 2 : // Single logistic
     {
       phase_selcoff_srv1 = -1; 
       phase_logist_srv1  = ph_srv1_sel;
       phase_dlogist_srv1 = -1;
      }
     break;
     case 3 : // Double logistic
     {
       phase_selcoff_srv1 = -1; 
       phase_logist_srv1  = ph_srv1_sel;
       phase_dlogist_srv1 = ph_srv1_sel;
     }
     break;
     case 4 : // Exponential logistic
     {
       phase_selcoff_srv1 = -1; 
       phase_logist_srv1  = ph_srv1_sel;
       phase_dlogist_srv1 = ph_srv1_sel;
     }
     break;
   }
// -----------
   
   switch (srv2_sel_opt)
   {
     case 1 : // Selectivity coefficients 
     {
       phase_selcoff_srv2 = ph_srv1_sel;
       phase_logist_srv2  = -1;
       phase_dlogist_srv2 = -1;
     }
     break;
     case 2 : // Single logistic
     {
       phase_selcoff_srv2 = -1; 
       phase_logist_srv2  = ph_srv1_sel;
       phase_dlogist_srv2 = -1;
      }
     break;
     case 3 : // Double logistic
     {
       phase_selcoff_srv2 = -1; 
       phase_logist_srv2  = ph_srv1_sel;
       phase_dlogist_srv2 = ph_srv1_sel;
     }
     break;
     case 4 : // Exponential logistic
     {
       phase_selcoff_srv2 = -1; 
       phase_logist_srv2  = ph_srv1_sel;
       phase_dlogist_srv2 = ph_srv1_sel;
     }
     break;
   }
// -----------
   
   switch (srv7_sel_opt)
   {
     case 1 : // Selectivity coefficients 
     {
       phase_selcoff_srv7 = ph_srv1_sel;
       phase_logist_srv7  = -1;
       phase_dlogist_srv7 = -1;
     }
     break;
     case 2 : // Single logistic
     {
       phase_selcoff_srv7 = -1; 
       phase_logist_srv7  = ph_srv1_sel;
       phase_dlogist_srv7 = -1;
      }
     break;
     case 3 : // Double logistic
     {
       phase_selcoff_srv7 = -1; 
       phase_logist_srv7  = ph_srv1_sel;
       phase_dlogist_srv7 = ph_srv1_sel;
     }
     break;
     case 4 : // Exponential logistic
     {
       phase_selcoff_srv7 = -1; 
       phase_logist_srv7  = ph_srv1_sel;
       phase_dlogist_srv7 = ph_srv1_sel;
     }
     break;
     case 5 : // Exponential logistic
     {
       phase_selcoff_srv7 = -1; 
       phase_logist_srv7  = -1;
       phase_dlogist_srv7 = -1;
     }
     break;
    if(wt_srv7==0) {
       phase_selcoff_srv7=-1;
       phase_logist_srv7=-1;
       phase_dlogist_srv7 = -1; }
     }
// -----------
   
   switch (fsh1_sel_opt)
   {
     case 1 : // Selectivity coefficients 
     {
       phase_selcoff_fsh1 = ph_fish_sel;
       phase_logist_fsh1  = -1;
       phase_dlogist_fsh1 = -1;
     }
     break;
     case 2 : // Single logistic
     {
       phase_selcoff_fsh1 = -1; 
       phase_logist_fsh1  = ph_fish_sel;
       phase_dlogist_fsh1 = -1;
      }
     break;
     case 3 : // Double logistic
     {
       phase_selcoff_fsh1 = -1; 
       phase_logist_fsh1  = ph_fish_sel;
       phase_dlogist_fsh1 = ph_fish_sel;
     }
     break;
     case 4 : // Exponential logistic
     {
       phase_selcoff_fsh1 = -1; 
       phase_logist_fsh1  = ph_fish_sel;
       phase_dlogist_fsh1 = ph_fish_sel;
     }
     break;
     case 5 : // Exponential logistic
     {
       phase_selcoff_fsh1 = -1; 
       phase_logist_fsh1  = -1;
       phase_dlogist_fsh1 = -1;
     }
     break;
   }
// -----------
   
   switch (fsh2_sel_opt)
   {
     case 1 : // Selectivity coefficients 
     {
       phase_selcoff_fsh2 = ph_fish_sel;
       phase_logist_fsh2  = -1;
       phase_dlogist_fsh2 = -1;
     }
     break;
     case 2 : // Single logistic
     {
       phase_selcoff_fsh2 = -1; 
       phase_logist_fsh2  = ph_fish_sel;
       phase_dlogist_fsh2 = -1;
      }
     break;
     case 3 : // Double logistic
     {
       phase_selcoff_fsh2 = -1; 
       phase_logist_fsh2  = ph_fish_sel;
       phase_dlogist_fsh2 = ph_fish_sel;
     }
     break;
      case 5 : // Exponential logistic
     {
       phase_selcoff_fsh2 = -1; 
       phase_logist_fsh2  = -1;
       phase_dlogist_fsh2 = -1;
     }
  }// -----------
   
   switch (fsh3_sel_opt)
   {
     case 1 : // Selectivity coefficients 
     {
       phase_selcoff_fsh3 = ph_fish_sel;
       phase_logist_fsh3  = -1;
       phase_dlogist_fsh3 = -1;
     }
     break;
     case 2 : // Single logistic
     {
       phase_selcoff_fsh3 = -1; 
       phase_logist_fsh3  = ph_fish_sel;
       phase_dlogist_fsh3 = -1;
      }
     break;
     case 3 : // Double logistic
     {
       phase_selcoff_fsh3 = -1; 
       phase_logist_fsh3  = ph_fish_sel;
       phase_dlogist_fsh3 = ph_fish_sel;
     }
     break;
     case 4 : // Exponential logistic
     {
       phase_selcoff_fsh3 = -1; 
       phase_logist_fsh3  = ph_fish_sel;
       phase_dlogist_fsh3 = ph_fish_sel;
     }
     break;
   }
//----
   switch (fsh4_sel_opt)
   {
     case 1 : // Selectivity coefficients 
     {
       phase_selcoff_fsh4 = ph_fish_sel;
       phase_logist_fsh4  = -1;
       phase_dlogist_fsh4 = -1;
     }
     break;
     case 2 : // Single logistic
     {
       phase_selcoff_fsh4 = -1; 
       phase_logist_fsh4  = ph_fish_sel;
       phase_dlogist_fsh4 = -1;
      }
     break;
     case 3 : // Double logistic
     {
       phase_selcoff_fsh4 = -1; 
       phase_logist_fsh4  = ph_fish_sel;
       phase_dlogist_fsh4 = ph_fish_sel;
     }
     break;
     case 4 : // Exponential logistic
     {
       phase_selcoff_fsh4 = -1; 
       phase_logist_fsh4  = ph_fish_sel;
       phase_dlogist_fsh4 = ph_fish_sel;
     }
    if(ph_ifq==0) {
       phase_selcoff_fsh4=-1;
       phase_logist_fsh4 = -1;
       phase_dlogist_fsh4 = -1; }
     break;
   }   // -----------
// Calculate "offset" for multinomials - survey age, fishery size, survey size
//   "Offset" value lets the multinomial likelihood equal zero when the observed and
//     predicted are equal as in Fournier (1990) "robustifies"
//   First step is to ensure that the data are expressed as proportions
  for (i=1; i<=nyrs_fish1_age; i++)
  {
   oac_fish1(i)/=sum(oac_fish1(i));
   offset(1) -= nsamples_fish1_age(i) *((oac_fish1(i) + 0.001)*log(oac_fish1(i) + 0.001)); 
  }

  for (i=1; i<=nyrs_srv1_age; i++)
  {
   oac_srv1(i)/=sum(oac_srv1(i));
   offset(2) -= nsamples_srv1_age(i)*((oac_srv1(i) + 0.001)*log(oac_srv1(i) + 0.001));
  }

  for (i=1; i<=nyrs_fish1_size; i++)
  {
   osc_fish1_f(i)/=sum(osc_fish1_f(i));
   offset(3) -= nsamples_fish1_size(i)*((osc_fish1_f(i) + 0.001)*log(osc_fish1_f(i) + 0.001));
   osc_fish1_m(i)/=sum(osc_fish1_m(i));
   offset(4) -= nsamples_fish1_size(i)*((osc_fish1_m(i) + 0.001)*log(osc_fish1_m(i) + 0.001));
  }
  for (i=1; i<=nyrs_fish2_size; i++)
  {
   osc_fish2(i)/=sum(osc_fish2(i));
   offset(5) -= nsamples_fish2_size(i)*((osc_fish2(i) + 0.001)*log(osc_fish2(i) + 0.001));
  }
  for (i=1; i<=nyrs_fish3_size; i++)
  {
   osc_fish3_f(i)/=sum(osc_fish3_f(i));
   offset(6) -= nsamples_fish3_size(i)*((osc_fish3_f(i) + 0.001)*log(osc_fish3_f(i) + 0.001));
   osc_fish3_m(i)/=sum(osc_fish3_m(i));
   offset(7) -= nsamples_fish3_size(i)*((osc_fish3_m(i) + 0.001)*log(osc_fish3_m(i) + 0.001));

     }
  for (i=1; i<=nyrs_fish4_size; i++)
  {
   osc_fish4(i)/=sum(osc_fish4(i));
   offset(8) -= nsamples_fish4_size(i)*((osc_fish4(i) + 0.001)*log(osc_fish4(i) + 0.001));
  }

  for (i=1; i<=nyrs_srv1_size; i++)
  {
   osc_srv1_f(i)/=sum(osc_srv1_f(i));
   offset(9) -= nsamples_srv1_size(i)*((osc_srv1_f(i) + 0.001)*log(osc_srv1_f(i) + 0.001));
    osc_srv1_m(i)/=sum(osc_srv1_m(i));
   offset(10) -= nsamples_srv1_size(i)*((osc_srv1_m(i) + 0.001)*log(osc_srv1_m(i) + 0.001));
  }
  for (i=1; i<=nyrs_srv2_size; i++)
  {
   osc_srv2_f(i)/=sum(osc_srv2_f(i));
   offset(11) -= nsamples_srv2_size(i)*((osc_srv2_f(i) + 0.001)*log(osc_srv2_f(i) + 0.001));
    osc_srv2_m(i)/=sum(osc_srv2_m(i));
   offset(12) -= nsamples_srv2_size(i)*((osc_srv2_m(i) + 0.001)*log(osc_srv2_m(i) + 0.001));
  }
  for (i=1; i<=nyrs_srv7_size; i++)
  {
   osc_srv7_f(i)/=sum(osc_srv7_f(i));
   offset(13) -= nsamples_srv7_size(i)*((osc_srv7_f(i) + 0.001)*log(osc_srv7_f(i) + 0.001));
    osc_srv7_m(i)/=sum(osc_srv7_m(i));
   offset(14) -= nsamples_srv7_size(i)*((osc_srv7_m(i) + 0.001)*log(osc_srv7_m(i) + 0.001));
  }
  for (i=1; i<=nyrs_srv7_age; i++)
  {
   oac_srv7(i)/=sum(oac_srv7(i));
   offset(15) -= nsamples_srv7_age(i)*((oac_srv7(i) + 0.001)*log(oac_srv7(i) + 0.001)); }
  for (i=1; i<=nyrs_srv2_age; i++)
  {
   oac_srv2(i)/=sum(oac_srv2(i));
   offset(16) -= nsamples_srv2_age(i)*((oac_srv2(i) + 0.001)*log(oac_srv2(i) + 0.001));
  }

 END_CALCS
 
INITIALIZATION_SECTION
 mdelta 0.000000000000
 log_mean_rec       2.40524770675
 sigr      sigrprior
 log_avg_F_fish1    -2.56764486068
  // log_a50_fish2      2.1 // from pushing Japanese longline fishery lengths through a combined age-length key to get a rough a_50
 logm      -2.30258509299;
log_avg_F_fish1    -2.56764486068
 log_avg_F_fish3         -4.56764486068
 log_q_srv1      2.02423694283
 log_q_srv2      1.85996498559
 log_q_srv3        2.07379999994
 log_q_srv4      2.37700000005
 log_q_srv5        1.44478842836
 log_q_srv6        2.62659387749
 log_q_srv7      0.474231085560
 log_q_srv8      1.77769087985
 log_a50_fish4_f        1.13279115361
 log_delta_fish4_f        0.723340585591
 log_a50_fish4_m        1.15627997343
 log_delta_fish4_m         0.945517826919
 log_a50_srv1_f         1.00000002481 
 log_delta_srv1_f         1.75000000000
 log_a50_srv1_m       1.00000000825
 log_delta_srv1_m        1.55000000000
 log_a50_srv2_f       1.32467214925
 log_a50_srv2_m       1.17546581443
 log_delta_srv2_m        0.565744058481
 log_a50_srv7_f       -0.235242532157
 log_a50_srv7_m       -0.924019086951
 PARAMETER_SECTION
 // Stock-recruitment
  init_bounded_number  steepness(0.2001,0.999,ph_steepness)   // Stock recruitment steepness
  init_bounded_number  log_Rzero(1,5,-1);   // Unfish equil recruitment (logged)
  vector        sam_rec(styr_rec,endyr)              // As estimated by assessment model
  vector        srm_rec(styr_rec,endyr)              // As estimated by stock-recruitment curve
  vector        Sp_Biom(styr_sp,endyr)
  sdreport_vector  ssbsd(styr,endyr);
 number                 sigrsq                 // Recruitment variance parameter
  number                alpha;                  // alpha parameter for B-H
  number                beta;                 // beta parameter for B-H
  number                Bzero;                  // Virgin spawner biomass
  number                Rzero;                  // Virgin recruitment
  number                phizero;               // SPR
  number          log_Rztemp;       // temporary logRzero

  // Key parameters
  init_number           log_q_srv1(ph_q_srv1);                   // Estimate Log survey catchability
  init_number           log_q_srv2(ph_q_srv2);                   // Estimate Log survey catchability
  init_number           log_q_srv3(-1);                   // Estimate Log survey catchability
  init_number           log_q_srv4(-1);                   // Estimate Log survey catchability
  init_number           log_q_srv5(ph_q_srv5);                   // Estimate Log survey catchability
  init_number           log_q_srv6(ph_q_srv6);                   // Estimate Log survey catchability
  init_number           log_q_srv7(ph_q_srv7);                   // Estimate Log survey catchability
  init_number           log_q_srv8(ph_q_srv8);                   // Estimate Log survey catchability
  init_number           log_q_srv9(-ph_srv2_q2);                   // Estimate Log survey catchability
  init_number           logm(ph_m);                             // Estimate log natural mortality
  init_bounded_number   log_mean_rec(-1,5,1);       // Unfish equil recruitment (logged)
  init_bounded_number   sigr(0.1,2,ph_sigr); // Recruitment sdev parameter
  init_bounded_number   mdelta(-0.05,0.05,ph_mdelta);  // Use parameter to have a male natural mortality

    // Fishery selectivity
  init_vector       log_fish1_sel_coffs_f(1,n_fish_sel_ages,phase_selcoff_fsh1); // vector of fishery selectivy log parameters up until they are constant
  init_bounded_number   log_a50_fish1_f(0.5,2.5,4);                 // age at 50% selection                                                   
  init_bounded_number   log_delta_fish1_f(-10,10,-4);                 // age between 50% selection and 95% selection....
  number          a50_fish1_f;                    // age at 50% selection                                                   
  init_number       d50_fish1_f(phase_dlogist_fsh1);
  number        delta_fish1_f;                    // age between 50% selection and 95% selection....
  init_number       gamma_fish1_f(phase_dlogist_fsh1);
  vector        log_fish1_sel_f(1,nages);                      // vector of fishery selectivy log parameters including those not estimated
  vector        fish1_sel_f(1,nages);                          // vectory of fishery selectivty parameters on arithmetic scale
  number        log_avgfish1sel_f;                               // average fishery selectivity

  init_vector       log_fish1_sel_coffs_m(1,n_fish_sel_ages,phase_selcoff_fsh1); // vector of fishery selectivy log parameters up until they are constant
  init_bounded_number   log_a50_fish1_m(1,3,4);                 // age at 50% selection                                                   
  init_bounded_number   log_delta_fish1_m(-10,10,-4);                 // age between 50% selection and 95% selection....
  number          a50_fish1_m;                    // age at 50% selection                                                   
  init_number       d50_fish1_m(phase_dlogist_fsh1);
  number        delta_fish1_m;                    // age between 50% selection and 95% selection....
  init_number       gamma_fish1_m(phase_dlogist_fsh1);
  vector        log_fish1_sel_m(1,nages);                      // vector of fishery selectivy log parameters including those not estimated
  vector        fish1_sel_m(1,nages);                          // vectory of fishery selectivty parameters on arithmetic scale
  number        log_avgfish1sel_m;                               // average fishery selectivity

  init_vector       log_fish2_sel_coffs(1,n_fish_sel_ages,phase_selcoff_fsh2); // vector of fishery selectivy log parameters up until they are constant
  init_number       log_a50_fish2(phase_logist_fsh2);                 // age at 50% selection                                                   
  init_number       log_delta_fish2(-phase_logist_fsh2);                 // age between 50% selection and 95% selection....
  number          a50_fish2;                    // age at 50% selection                                                   
  number        delta_fish2;                    // age between 50% selection and 95% selection....
  vector        log_fish2_sel(1,nages);                      // vector of fishery selectivy log parameters including those not estimated
  vector        fish2_sel(1,nages);                          // vectory of fishery selectivty parameters on arithmetic scale
  number        log_avgfish2sel;                               // average fishery selectivity

  init_vector       log_fish3_sel_coffs_f(1,n_fish_sel_ages,phase_selcoff_fsh3); // vector of fishery selectivy log parameters up until they are constant
  init_bounded_number   log_a50_fish3_f(-4,4,4);                 // age at 50% selection                                                   
  init_bounded_number   log_delta_fish3_f(-4,4,4);                 // age between 50% selection and 95% selection....
  number          a50_fish3_f;                    // age at 50% selection                                                   
  init_number       d50_fish3_f(-1);
  number        delta_fish3_f;                    // age between 50% selection and 95% selection....
  init_bounded_number log_gamma_fish3_f(-0.00001,-0.00000000001,-4);
  vector        log_fish3_sel_f(1,nages);                      // vector of fishery selectivy log parameters including those not estimated
  vector        fish3_sel_f(1,nages);                          // vectory of fishery selectivty parameters on arithmetic scale
  number        log_avgfish3sel_f;                               // average fishery selectivity
  number        gamma_fish3_f;

  init_vector       log_fish3_sel_coffs_m(1,n_fish_sel_ages,phase_selcoff_fsh3); // vector of fishery selectivy log parameters up until they are constant
  init_bounded_number   log_a50_fish3_m(-4,4,4);                 // age at 50% selection                                                   
  init_bounded_number   log_delta_fish3_m(-4,4,4);                 // age between 50% selection and 95% selection....
  number          a50_fish3_m;                    // age at 50% selection                                                   
  init_number       d50_fish3_m(-1);
  number        delta_fish3_m;                    // age between 50% selection and 95% selection....
  init_bounded_number log_gamma_fish3_m(-0.18,-0.173,-4);
  vector        log_fish3_sel_m(1,nages);                      // vector of fishery selectivy log parameters including those not estimated
  vector        fish3_sel_m(1,nages);                          // vectory of fishery selectivty parameters on arithmetic scale
  number        log_avgfish3sel_m;                               // average fishery selectivity
  number          gamma_fish3_m;

  init_vector       log_fish4_sel_coffs_f(1,n_fish_sel_ages,phase_selcoff_fsh4); // vector of fishery selectivy log parameters up until they are constant
  init_bounded_number   log_a50_fish4_f(1,3,4);                 // age at 50% selection                                                   
  init_bounded_number   log_delta_fish4_f(0.1,2,4);                 // age between 50% selection and 95% selection....
  number          a50_fish4_f;                    // age at 50% selection                                                   
  init_number       d50_fish4_f(phase_dlogist_fsh4);
  number        delta_fish4_f;                    // age between 50% selection and 95% selection....
  init_number       gamma_fish4_f(phase_dlogist_fsh4);
  vector        log_fish4_sel_f(1,nages);                      // vector of fishery selectivy log parameters including those not estimated
  vector        fish4_sel_f(1,nages);                          // vectory of fishery selectivty parameters on arithmetic scale
  number        log_avgfish4sel_f;                               // average fishery selectivity

  init_vector       log_fish4_sel_coffs_m(1,n_fish_sel_ages,phase_selcoff_fsh4); // vector of fishery selectivy log parameters up until they are constant
  init_bounded_number   log_a50_fish4_m(1,3,4);                 // age at 50% selection                                                   
  init_bounded_number   log_delta_fish4_m(0.1,40,-4);                 // age between 50% selection and 95% selection....
  number          a50_fish4_m;                    // age at 50% selection                                                   
  init_number       d50_fish4_m(phase_dlogist_fsh4);
  number        delta_fish4_m;                    // age between 50% selection and 95% selection....
  init_number       gamma_fish4_m(-4);
  vector        log_fish4_sel_m(1,nages);                      // vector of fishery selectivy log parameters including those not estimated
  vector        fish4_sel_m(1,nages);                          // vectory of fishery selectivty parameters on arithmetic scale
  number        log_avgfish4sel_m;                               // average fishery selectivity

 // Survey selectivities
 
  init_vector       log_srv1_sel_coffs_f(1,n_srv1_sel_ages,phase_selcoff_srv1);   // vector of survey selectivy log parameters up until they are constant
  init_bounded_number   log_a50_srv1_f(0.5,3,4);                 // age at 50% selection                                                   
  init_bounded_number   log_delta_srv1_f(0.5,3,-4);                 // age between 50% selection and 95% selection....
  number          a50_srv1_f;                     // age at 50% selection                                                   
  init_number       d50_srv1_f(phase_dlogist_srv1);
  number        delta_srv1_f;                     // age between 50% selection and 95% selection....
  init_number       gamma_srv1_f(phase_dlogist_srv1);
  vector        log_srv1_sel_f(1,nages);              // vector of survey selectivy log parameters including those not estimated
  vector        srv1_sel_f(1,nages);                  // vectory of survey selectivty parameters on arithmetic scale
  number        log_avgsrv1sel_f;                     // average survey selectivity

  init_vector       log_srv1_sel_coffs_m(1,n_srv1_sel_ages,phase_selcoff_srv1);   // vector of survey selectivy log parameters up until they are constant
  init_bounded_number   log_a50_srv1_m(0.5,3,4);                 // age at 50% selection                                                   
  init_bounded_number   log_delta_srv1_m(0.1,3,-4);                 // age between 50% selection and 95% selection....
  number          a50_srv1_m;                     // age at 50% selection                                                   
  init_number       d50_srv1_m(phase_dlogist_srv1);
  number        delta_srv1_m;                     // age between 50% selection and 95% selection....
  init_number       gamma_srv1_m(phase_dlogist_srv1);
  vector        log_srv1_sel_m(1,nages);              // vector of survey selectivy log parameters including those not estimated
  vector        srv1_sel_m(1,nages);                  // vectory of survey selectivty parameters on arithmetic scale
  number        log_avgsrv1sel_m;                     // average survey selectivity

  init_vector       log_srv2_sel_coffs_f(1,n_srv1_sel_ages,phase_selcoff_srv2);   // vector of survey selectivy log parameters up until they are constant
  init_bounded_number   log_a50_srv2_f(0.5,3,1);                 // age at 50% selection                                                   
  init_bounded_number   log_delta_srv2_f(-2,2,-1);                 // age between 50% selection and 95% selection....
  number          a50_srv2_f;                     // age at 50% selection                                                   
  init_number       d50_srv2_f(phase_dlogist_srv2);
  init_number       gamma_srv2_f(phase_dlogist_srv2);
  number        delta_srv2_f;                     // age between 50% selection and 95% selection....
  vector        log_srv2_sel_f(1,nages);              // vector of survey selectivy log parameters including those not estimated
  vector        srv2_sel_f(1,nages);                  // vectory of survey selectivty parameters on arithmetic scale
  number        log_avgsrv2sel_f;                     // average survey selectivity

  init_vector       log_srv2_sel_coffs_m(1,n_srv1_sel_ages,phase_selcoff_srv2);   // vector of survey selectivy log parameters up until they are constant
  init_bounded_number   log_a50_srv2_m(0.5,3.5,1);                 // age at 50% selection                                                   
  init_bounded_number   log_delta_srv2_m(-2,2,1);                 // age between 50% selection and 95% selection....
  number          a50_srv2_m;                     // age at 50% selection                                                   
  init_number       d50_srv2_m(phase_dlogist_srv2);
  init_number       gamma_srv2_m(phase_dlogist_srv2);
  number        delta_srv2_m;                     // age between 50% selection and 95% selection....
  vector        log_srv2_sel_m(1,nages);              // vector of survey selectivy log parameters including those not estimated
  vector        srv2_sel_m(1,nages);                  // vectory of survey selectivty parameters on arithmetic scale
  number        log_avgsrv2sel_m;                     // average survey selectivity

  init_vector       log_srv7_sel_coffs_f(1,n_srv1_sel_ages,phase_selcoff_srv7); // vector of fishery selectivy log parameters up until they are constant
  init_bounded_number   log_a50_srv7_f(-40,5,4);                 // age at 50% selection                                                   
  init_bounded_number   log_delta_srv7_f(-4,4,-4);                 // age between 50% selection and 95% selection....
  number          a50_srv7_f;                     // age at 50% selection                                                   
  init_number       log_d50_srv7_f(-ph_q_srv7*phase_dlogist_srv7);
  number        delta_srv7_f;                     // age between 50% selection and 95% selection....
  init_bounded_number log_gamma_srv7_f(-10,-0.0000000000000001,-4);
  vector        log_srv7_sel_f(1,nages);                       // vector of fishery selectivy log parameters including those not estimated
  vector        srv7_sel_f(1,nages);                           // vectory of fishery selectivty parameters on arithmetic scale
  number        log_avgsrv7sel_f;                              // average fishery selectivity
  number        gamma_srv7_f;
  number        d50_srv7_f;

  init_vector       log_srv7_sel_coffs_m(1,n_srv1_sel_ages,phase_selcoff_srv7); // vector of fishery selectivy log parameters up until they are constant
  init_bounded_number   log_a50_srv7_m(-40,5,4);                 // age at 50% selection                                                   
  init_bounded_number   log_delta_srv7_m(-4,4,-4);                 // age between 50% selection and 95% selection....
  number          a50_srv7_m;                     // age at 50% selection                                                   
  init_number       log_d50_srv7_m(-ph_q_srv7*phase_dlogist_srv7);
  number        delta_srv7_m;                     // age between 50% selection and 95% selection....
  init_bounded_number log_gamma_srv7_m(-10,-0.0000000000000001,-4);
  vector        log_srv7_sel_m(1,nages);                       // vector of fishery selectivy log parameters including those not estimated
  vector        srv7_sel_m(1,nages);                           // vectory of fishery selectivty parameters on arithmetic scale
  number        log_avgsrv7sel_m;                              // average fishery selectivity
  number        gamma_srv7_m;
  number        d50_srv7_m;
  // Fishing mortality
  init_number           log_avg_F_fish1(ph_avg_F);                      // Log average fishing mortality
  init_bounded_vector   log_F_devs_fish1(styr,endyr,-5,5,ph_Fdev);  // Annual fishing mortality deviations
  init_number         log_avg_F_fish3(ph_avg_F);                      // Log average fishing mortality
  init_bounded_vector   log_F_devs_fish3(styr,endyr,-5,5,ph_Fdev);  // Annual fishing mortality deviations
  vector                Fmort_fish1(styr,endyr);                        // Fishing mortality by year
  vector                Fmort_fish3(styr,endyr);                        // Fishing mortality by year
  matrix                Z_f(styr,endyr,1,nages);                    // Total mortality by year and age
  matrix                Z_m(styr,endyr,1,nages);                    // Total mortality by year and age
  matrix                F_fish1_f(styr,endyr,1,nages);                    // Fishing mortality by year and age
  matrix                F_fish1_m(styr,endyr,1,nages);                    // Fishing mortality by year and age
  matrix                F_fish3_f(styr,endyr,1,nages);                    // Fishing mortality by year and age
  matrix                F_fish3_m(styr,endyr,1,nages);                    // Fishing mortality by year and age
  matrix                S_f(styr,endyr,1,nages);                    // Survivorship by year and age
  matrix                S_m(styr,endyr,1,nages);                    // Survivorship by year and age
  matrix                S_f_mid(styr,endyr,1,nages);                    // Survivorship by year and age
  matrix                S_m_mid(styr,endyr,1,nages);                    // Survivorship by year and age

  // Create a vector of natural mortalities for proj.dat
  vector natmortv(1,nages);

// Numbers at age
  init_vector       log_rec_dev(styr-nages+2,endyr_rec_est,ph_recdev);  // Recruitment deviations from before the asssessment starts to present
  matrix                natage_m(styr,endyr,1,nages);         // Matrix of numbers at age from start year to end year
  matrix                natage_f(styr,endyr,1,nages);         // Matrix of numbers at age from start year to end year

// Catch at age
  matrix                catage_fish1_f(styr,endyr,1,nages)            // Matrix of predicted catch at age from start year to endyear
  matrix                catage_fish1_m(styr,endyr,1,nages)            // Matrix of predicted catch at age from start year to endyear
  matrix                catage_fish3_f(styr,endyr,1,nages)            // Matrix of predicted catch at age from start year to endyear
  matrix                catage_fish3_m(styr,endyr,1,nages)            // Matrix of predicted catch at age from start year to endyear
  vector                pred_catch_fish1(styr,endyr)        // Vector of predicted catches
  vector          pred_catch_fish3(styr,endyr)
  vector                pred_srv1(styr,endyr);                  // Predicted survey
  vector                pred_srv2(styr,endyr);                  // Predicted survey
  vector                pred_srv3(styr,endyr);                  // Predicted survey
  vector                pred_srv4(styr,endyr);                  // Predicted survey
  vector                pred_srv5(styr,endyr);                  // Predicted survey
  vector                pred_srv6(styr,endyr);                  // Predicted survey
  vector                pred_srv7(styr,endyr);                  // Predicted survey
  vector                pred_srv8(styr,endyr);                  // Predpreedicted survey

  matrix                eac_fish1(1,nyrs_fish1_age,1,nages)        // Expected proportion at age in fish
  matrix                eac_srv1(1,nyrs_srv1_age,1,nages)        // Expected proportion at age in survey
  matrix                eac_srv2(1,nyrs_srv2_age,1,nages)        // Expected proportion at age in survey
  matrix                esc_fish1_m(1,nyrs_fish1_size,1,nlenbins)    // Expected proportion at size in fishery
  matrix                esc_fish1_f(1,nyrs_fish1_size,1,nlenbins)    // Expected proportion at size in fishery
  matrix                esc_fish2(1,nyrs_fish2_size,1,nlenbins)    // Expected proportion at size in fishery
  matrix                esc_fish3_m(1,nyrs_fish3_size,1,nlenbins)    // Expected proportion at size in fishery
  matrix                esc_fish3_f(1,nyrs_fish3_size,1,nlenbins)    // Expected proportion at size in fishery
  matrix                esc_fish4(1,nyrs_fish4_size,1,nlenbins)    // Expected proportion at size in fishery
  matrix                esc_srv1_m(1,nyrs_srv1_size,1,nlenbins)    // Expected proportion at size in survey
  matrix                esc_srv1_f(1,nyrs_srv1_size,1,nlenbins)    // Expected proportion at size in survey
  matrix                esc_srv2_m(1,nyrs_srv2_size,1,nlenbins)    // Expected proportion at size in survey
  matrix                esc_srv2_f(1,nyrs_srv2_size,1,nlenbins)    // Expected proportion at size in survey
  matrix                esc_srv7_m(1,nyrs_srv7_size,1,nlenbins)    // Expected proportion at size in survey
  matrix                esc_srv7_f(1,nyrs_srv7_size,1,nlenbins)    // Expected proportion at size in survey
  matrix                eac_srv7(1,nyrs_srv7_age,1,nages)    // Expected proportion at size in survey

       // Some calculated variables and standard deviation estimates for some estimated parameters
  vector            tot_biom(styr,endyr);                    // Standard deviation report vector of total biomass
  sdreport_number         q_srv1;                                  // " " for Survey catchability
  sdreport_number         q_srv2;
  number         q_srv3;  //not used now that RPWs are taken out
  number         q_srv4;
  sdreport_number         q_srv5;
  sdreport_number         q_srv6;
  sdreport_number         q_srv7;
  sdreport_number         q_srv8;
  number         q_srv9;

  sdreport_vector       pred_rec(styr,endyr);         // " " for predicted recruitments
  number            avg_rec;                                 // " " for Average recruitment 
  number            spbiom_trend;                            // " " of Trend in biomass over last 2 years (B(t)/B(t-1); t=endyr)
  number            Depletion;                               // " " for Depletion
  vector            spawn_biom(styr,endyr);         // " " for spawning biomass vector
  number            natmort;                  // " " for natural mortality

 // Parameters for computing SPR rates 
  init_bounded_number   mF50(0.01,1.,ph_F50)            // Estimated F50
  init_bounded_number   mF40(0.01,1.,ph_F50)            // Estimated F40
  init_bounded_number   mF35(0.01,1.,ph_F50)            // Estimated F35
  number        F50;                      // Standard deviation report for F50
  sdreport_number       F40;                      // " " " F40
  number        F35;                      // " " " F35
  number        SB0                         // Spawning biomass at no fishing
  number        SBF50                         // " " at F50
  number        SBF40                         // " " at F40
  number        SBF35                         // " " at F35
  number        sprpen                          // Likelihood penalty to make ADMB estimate spr rates
  matrix        Nspr(1,4,1,nages)                   // Matrix of number of spawners at age at each fishing mortality level
  number        hist_hal_F; // Option of adding historical proportion of current average hook and line catch
// Likelihoods and penalty functions
  vector        surv_like(1,8);   // Likelihood values for survey biomasses, allowance for up to 3 surveys
  vector        age_like(1,16);     // Likelihood values for age and size compositions allowance for up 6 comps
  vector        sel_like(1,12);     // LIkelihood values for selectivities with alowance for up to 6 selectivities
  number        rec_like;         // Likelihood value for recruitments
  number        ssqcatch;         // Likelihood value for catch estimation
  number        F_mort_regularity;  // Penalty value for fishing mortality regularity
  number        avg_sel_penalty;    // Penalty value for selectivity regularity penalty

 // Priors
  vector        priors(1,12);       // Prior penalty values for sigr,q,natural mortality,steepness
// Define an objective function
  number        Like;             // Likelihood for data fits
  objective_function_value obj_fun;       // Total likelihood for objective function value
  vector        xdum2(styr,endyr);                // Dummy variable for use in pop-report.cxx
  vector        pred_catch(styr,endyr);
  number        fratio;
//  number        B40;
  number        ABC3;
///////////////////////////////////////////
/// Population projection Hanselsiginelli
//////////////////////////////////////////

  matrix        N_proj_f(endyr+1,endyr+15,1,nages);
  matrix        N_proj_m(endyr+1,endyr+15,1,nages);
  number        FABC_proj;
  vector        FABC_tot_proj_f(1,nages);
  vector        FABC_tot_proj_m(1,nages);
  number FOFL_proj;
  vector FOFL_tot_proj_f(1,nages);
  vector FOFL_tot_proj_m(1,nages);
  sdreport_number ABC;
  sdreport_number B40;
 number OFL;
  vector        Z_proj_f(1,nages);
  vector        Z_proj_m(1,nages);
  vector        ZOFL_proj_f(1,nages);
  vector        ZOFL_proj_m(1,nages);

  vector        S_proj_f(1,nages);
  vector        S_proj_m(1,nages);
  matrix        catage_proj_f(endyr+1,endyr+15,1,nages);
  matrix        catage_proj_m(endyr+1,endyr+15,1,nages);
  matrix        catage_proj_OFL_f(endyr+1,endyr+15,1,nages);
  matrix        catage_proj_OFL_m(endyr+1,endyr+15,1,nages);
  vector        pred_catch_proj_OFL_f(endyr+1,endyr+15);
  vector        pred_catch_proj_OFL_m(endyr+1,endyr+15);
  sdreport_vector         spawn_biom_proj(endyr+1,endyr+15);
  sdreport_vector         tot_biom_proj(endyr+1,endyr+15);
  sdreport_vector         pred_catch_proj(endyr+1,endyr+15);
  sdreport_vector         pred_catch_proj_OFL(endyr+1,endyr+15);
  number        stdev_rec;
  number        FOFL;
  number        FABC;
  number        FOFL2;
  number        FABC2; 

 
 PROCEDURE_SECTION
  l=l+1; // Initiate counter for random seeds in projection
  
  switch (SrType) {
     case 3:
     if (rec_like_type==1) log_Rztemp=(log_mean_rec);
      default:
      log_Rztemp=log_Rzero; }

     Get_Selectivity();                     // Call function to get selectivities
       Get_Mortality_Rates();                 // Call function to get fishing and natural mortality
   //  Get_Bzero();                           // OjO
       Get_Numbers_At_Age();                    // Call function to get numbers at age per year
       Get_Catch_at_Age();                      // Call function to get catch at age per year
       Get_Predicted_Values();                  // Get predicted values for catch, survbio, age and size comps
       Calc_priors();                         // Solve for priors
 //  if (last_phase())
 //  {
        
       Get_Dependent_Vars();                  // Solve for dependent variables like total bio, recruitment etc.
       ssbsd = spawn_biom;
       compute_spr_rates();                 // Compute f40 etc.
       Get_Population_Projection();
 //  }
 switch (SrType) {
     case 3:
      if (rec_like_type==1)log_Rztemp=(log_mean_rec);
      default:
      log_Rztemp=log_Rzero; }
  Evaluate_Objective_Function();          // Minimize objective function value
  
    if (mceval_phase())                     // For outputting MCMC simulations in text format 
  
    {
     evalout<<log_mean_rec<<" "<<sigr<<" "<<q_srv1<<" "<<q_srv2<<" "<<q_srv3<<" "<<q_srv4<<" "<<q_srv5<<" "<<q_srv6<<" "<<q_srv7<<" "<<q_srv8<<" "<<F40<<" "<<natmort<<" "<<obj_fun<<" "<<tot_biom<<" "<<spawn_biom<<" "<<pred_rec<<" "<<B40<<" "<<B40<<" "<<spawn_biom_proj<<" "<<pred_catch_proj<<" "<<endl;
     }

FUNCTION dvar_vector SRecruit(const dvar_vector& Stmp)
  RETURN_ARRAYS_INCREMENT();
  dvar_vector RecTmp(Stmp.indexmin(),Stmp.indexmax());
      // dvariable R_alpha;
      // dvariable R_beta;
  switch (SrType)
  {
    case 1:
      RecTmp = elem_prod((Stmp / phizero) , mfexp( alpha * ( 1. - Stmp / Bzero ))) ; //Ricker form from Dorn
      // R_alpha = exp(alpha)/phizero;
      // R_alpha  = 0.036494;
      // R_beta  = alpha/Bzero;
      // R_beta  = 0.0037207;
      // RecTmp = elem_prod( R_alpha*Stmp ,mfexp(-R_beta*Stmp)) ; // Ricker model

      break;
    case 2:
      // cout<<"BH a b "<<alpha<<" "<<beta<<endl;
      RecTmp = elem_prod(Stmp , 1. / ( alpha + beta * Stmp));        //Beverton-Holt form
      break;
    case 3:
       RecTmp = mfexp(log_mean_rec);                    //Avg recruitment
      if(rec_like_type==1) log_Rztemp=log_mean_rec;
      break;
    case 4:
      RecTmp = elem_prod(Stmp , mfexp( alpha  - Stmp * beta)) ; //Old Ricker form
      break;
  }
  RETURN_ARRAYS_DECREMENT();
  return RecTmp;

FUNCTION dvariable SRecruit(const double& Stmp)
  RETURN_ARRAYS_INCREMENT();
  dvariable RecTmp;
  switch (SrType)
  {
    case 1:
      RecTmp = (Stmp / phizero) * mfexp( alpha * ( 1. - Stmp / Bzero )) ; //Ricker form from Dorn
      break;
    case 2:
      RecTmp = Stmp / ( alpha + beta * Stmp);        //Beverton-Holt form
      break;
    case 3:
      RecTmp = mfexp(log_mean_rec);                    //Avg recruitment
      if(rec_like_type==1) log_Rztemp=log_mean_rec;
      break;
    case 4:
      RecTmp = Stmp * mfexp( alpha  - Stmp * beta) ; //old Ricker form
      break;
  }
  RETURN_ARRAYS_DECREMENT();
  return RecTmp;

FUNCTION dvariable SRecruit(CONST dvariable& Stmp)
  RETURN_ARRAYS_INCREMENT();
  dvariable RecTmp;
      // dvariable R_alpha;
      // dvariable R_beta;
  switch (SrType)
  {
    case 1:
      RecTmp = (Stmp / phizero) * mfexp( alpha * ( 1. - Stmp / Bzero )) ; //Ricker form from Dorn
      break;
    case 2:
      RecTmp = Stmp / ( alpha + beta * Stmp);        //Beverton-Holt form
      break;
    case 3:
      RecTmp = mfexp(log_mean_rec );                    //Avg recruitment
      break;
    case 4:
      RecTmp = Stmp * mfexp( alpha  - Stmp * beta) ; //old Ricker form
      break;
  }
  RETURN_ARRAYS_DECREMENT();
  return RecTmp;


FUNCTION Get_Bzero
  Bzero.initialize();
  Rzero    =  mfexp(log_Rztemp); 
  sigrsq   = sigr*sigr;

  dvariable survtmp = exp(-natmort);
  dvariable spawn_adj=pow(survtmp,spawn_fract) ;

  dvar_matrix natagetmp(styr_rec,styr,1,nages);
  natagetmp(styr_rec,1) = Rzero;
  for (j=2; j<=nages; j++)
    natagetmp(styr_rec,j) = natagetmp(styr_rec,j-1) * survtmp;

  natagetmp(styr_rec,nages) /= (1.-survtmp); 

  Bzero = wt_mature * spawn_adj *natagetmp(styr_rec) ;
  phizero = Bzero/Rzero;

  switch (SrType)
  {
    case 1:
      alpha = log(-4.*(steepness)/(steepness-1.));
      break;
    case 2:
      alpha  =  Bzero * (1. - (steepness - 0.2) / (0.8*steepness) ) / Rzero;
      beta   = (5. * steepness - 1.) / (4. * steepness * Rzero);
      break;
    case 4:
    //R = S * EXP(alpha - beta * S))
      beta  = log(5.*(steepness))/(0.8*Bzero) ;
      alpha = log((Rzero)/Bzero)+beta*Bzero;
      break;
  }

  Sp_Biom.initialize();
  Sp_Biom(styr_sp,styr_rec-1) = Bzero;
  for (i=styr_rec;i<styr;i++)
  {
    natagetmp(i,1)          = mfexp(log_rec_dev(i) + log_Rztemp);
    Sp_Biom(i) = natagetmp(i)*spawn_adj * wt_mature; 
    natagetmp(i+1)(2,nages) = ++(natagetmp(i)(1,nages-1)*mfexp(-natmort ));
    natagetmp(i+1,nages)   += natagetmp(i,nages)*mfexp(-natmort);
  }
  natagetmp(styr,1)   = mfexp(log_rec_dev(styr) + log_Rztemp);
  sam_rec(styr_rec,styr) = column(natagetmp,1);
  natage_f(styr)  = natagetmp(styr)/2; // OjO
  natage_m(styr)  = natagetmp(styr)/2; // OjO
  Sp_Biom(styr) = natagetmp(styr)*spawn_adj * wt_mature; 

 // OjO to here...
  
FUNCTION Get_Selectivity
//   Fishery selectivity
//   Selectivity does not change for ages greater than n_fish_sel_ages
  if (fsh1_sel_opt==1)
  {
    for (j=1;j<=n_fish_sel_ages;j++) {
      log_fish1_sel_f(j) = log_fish1_sel_coffs_f(j);
      log_fish1_sel_m(j) = log_fish1_sel_coffs_m(j);
      log_fish1_sel_f(j) = log_fish1_sel_f(j-1);
      log_fish1_sel_m(j) = log_fish1_sel_m(j-1); 
      log_avgfish1sel_f = log(mean(mfexp(log_fish1_sel_coffs_f)));
      log_fish1_sel_f  -= log(mean(mfexp(log_fish1_sel_f)));
      log_avgfish1sel_m = log(mean(mfexp(log_fish1_sel_coffs_m)));
      log_fish1_sel_m  -= log(mean(mfexp(log_fish1_sel_m)));
      fish1_sel_f       = mfexp(log_fish1_sel_f)/mfexp(max(log_fish1_sel_f));  // keeping maximum fish selectivity at 1
      fish1_sel_m       = mfexp(log_fish1_sel_m)/mfexp(max(log_fish1_sel_m));  // keeping maximum fish selectivity at 1
   }}
  if (fsh2_sel_opt==1) {
    for (j=1;j<=n_fish_sel_ages;j++) log_fish2_sel(j) = log_fish2_sel_coffs(j);
      log_avgfish2sel = log(mean(mfexp(log_fish2_sel_coffs)));
      log_fish2_sel  -= log(mean(mfexp(log_fish2_sel)));
      fish2_sel       = mfexp(log_fish2_sel)/mfexp(max(log_fish2_sel)); } // keeping maximum fish selectivity at 1

    if (fsh3_sel_opt==1) 
  {
    for (j=1;j<=n_fish_sel_ages;j++) {
      log_fish3_sel_f(j) = log_fish3_sel_coffs_f(j);
      log_fish3_sel_m(j) = log_fish3_sel_coffs_m(j);
      log_fish3_sel_f(j) = log_fish3_sel_f(j-1);
      log_fish3_sel_m(j) = log_fish3_sel_m(j-1); 
      log_avgfish3sel_f = log(mean(mfexp(log_fish3_sel_coffs_f)));
      log_fish3_sel_f  -= log(mean(mfexp(log_fish3_sel_f)));
      log_avgfish3sel_m = log(mean(mfexp(log_fish3_sel_coffs_m)));
      log_fish3_sel_m  -= log(mean(mfexp(log_fish3_sel_m)));
      fish3_sel_f       = mfexp(log_fish3_sel_f)/mfexp(max(log_fish3_sel_f));  // keeping maximum fish selectivity at 1
      fish3_sel_m       = mfexp(log_fish3_sel_m)/mfexp(max(log_fish3_sel_m));  // keeping maximum fish selectivity at 1
   }}
  if (fsh4_sel_opt==1) {
   for (j=1;j<=n_fish_sel_ages;j++) {
      log_fish4_sel_f(j) = log_fish4_sel_coffs_f(j);
      log_fish4_sel_m(j) = log_fish4_sel_coffs_m(j);
      log_fish4_sel_f(j) = log_fish4_sel_f(j-1);
      log_fish4_sel_m(j) = log_fish4_sel_m(j-1); }
      log_avgfish4sel_f = log(mean(mfexp(log_fish4_sel_coffs_f)));
      log_fish4_sel_f  -= log(mean(mfexp(log_fish4_sel_f)));
      log_avgfish4sel_m = log(mean(mfexp(log_fish4_sel_coffs_m)));
      log_fish4_sel_m  -= log(mean(mfexp(log_fish4_sel_m)));
      fish4_sel_f       = mfexp(log_fish4_sel_f)/mfexp(max(log_fish4_sel_f));  // keeping maximum fish selectivity at 1
      fish4_sel_m       = mfexp(log_fish4_sel_m)/mfexp(max(log_fish4_sel_m)); }  // keeping maximum fish selectivity at 1
 
  a50_fish1_f=mfexp(log_a50_fish1_f);
  a50_fish1_m=mfexp(log_a50_fish1_m);
  // a50_fish2=mfexp((log_a50_fish1_f+log_a50_fish1_m)/2); 
   a50_fish2=mfexp(log_a50_fish2); 
  a50_fish3_f=mfexp(log_a50_fish3_f);
  a50_fish3_m=mfexp(log_a50_fish3_m);
  a50_fish4_f=mfexp(log_a50_fish4_f);
  a50_fish4_m=mfexp(log_a50_fish4_m);
  delta_fish1_f=mfexp(log_delta_fish4_f); // linked to IFQ
  delta_fish1_m=mfexp(log_delta_fish4_m); // linked to IFQ
  delta_fish2=mfexp((log_delta_fish4_f+log_delta_fish4_m)/2);
  delta_fish3_f=mfexp(log_delta_fish3_f);
  delta_fish3_m=mfexp(log_delta_fish3_m);
  delta_fish4_f=mfexp(log_delta_fish4_f);
  delta_fish4_m=mfexp(log_delta_fish4_f);  /// linking shapes of IFQ selectivity between males and females because of peposterously high CV on males when estimated alone
  d50_srv7_f=mfexp(log_d50_srv7_f);
  gamma_srv7_f=mfexp(log_gamma_srv7_f);
  d50_srv7_m=mfexp(log_d50_srv7_m);
  gamma_srv7_m=mfexp(log_gamma_srv7_m);
  gamma_fish3_m=mfexp(log_gamma_fish3_m);
  gamma_fish3_f=mfexp(log_gamma_fish3_f);
  for (j=1;j<=nages;j++) {
  if(fsh1_sel_opt==2) {
    fish1_sel_f(j)=1/ (1+mfexp(-delta_fish1_f*(j-a50_fish1_f))); 
    fish1_sel_m(j)=1/ (1+mfexp(-delta_fish1_m*(j-a50_fish1_m))); }
  if(fsh1_sel_opt==3) { 
  fish1_sel_f(j) = (1/(1+mfexp(-delta_fish1_f*(double(j)-a50_fish1_f))))*(1-(1/(1+mfexp(-gamma_fish1_f*(j-d50_fish1_f)))));
  fish1_sel_m(j) = (1/(1+mfexp(-delta_fish1_m*(double(j)-a50_fish1_m))))*(1-(1/(1+mfexp(-gamma_fish1_m*(j-d50_fish1_m))))); } 
  if(fsh1_sel_opt==4) { 
  fish1_sel_f(j) = (1/(1-gamma_fish1_f))*pow(((1-gamma_fish1_f)/gamma_fish1_f),gamma_fish1_f)*(mfexp(delta_fish1_f*gamma_fish1_f*(a50_fish1_f-j))/(1+mfexp(delta_fish1_f*(a50_fish1_f-j))));
  fish1_sel_m(j) = (1/(1-gamma_fish1_m))*pow(((1-gamma_fish1_m)/gamma_fish1_m),gamma_fish1_m)*(mfexp(delta_fish1_m*gamma_fish1_m*(a50_fish1_m-j))/(1+mfexp(delta_fish1_m*(a50_fish1_m-j)))); }
  if(fsh1_sel_opt==5) {
    fish1_sel_f(j)=1/ (1+mfexp(-delta_fish1_f*(j-a50_fish1_f))); 
    fish1_sel_m(j)=1/ (1+mfexp(-delta_fish1_m*(j-a50_fish1_m))); }
  if(fsh2_sel_opt==2) fish2_sel(j)=1/ (1+mfexp(-delta_fish2*(double(j)-a50_fish2))); 
 //  if(fsh2_sel_opt==3) fish2_sel(j)=1/ (1+mfexp(-delta_fish2*(double(j)-a50_fish4_f)));
  if(fsh2_sel_opt==3)  fish2_sel(j)=(pow(j/a50_fish2,a50_fish2/(0.5*(sqrt(square(a50_fish2)+4*square(delta_fish2))-a50_fish2)))*mfexp((a50_fish2-j)/(0.5*(sqrt(square(a50_fish2)+4*square(delta_fish2))-a50_fish2))));
  if(fsh2_sel_opt==5) fish2_sel(j)=1/ (1+mfexp(-delta_fish2*(double(j)-a50_fish2))); 
  if(fsh3_sel_opt==2) {
    fish3_sel_f(j)=1/ (1+mfexp(-delta_fish3_f*(j-a50_fish3_f))); 
    fish3_sel_m(j)=1/ (1+mfexp(-delta_fish3_m*(j-a50_fish3_m))); }
  if(fsh3_sel_opt==3) { 
//  fish3_sel_f(j) = (1/(1+mfexp(-delta_fish3_f*(double(j)-a50_fish3_f))))*(1-(1/(1+mfexp(-gamma_fish3_f*(j-d50_fish3_f)))));
//  fish3_sel_m(j) = (1/(1+mfexp(-delta_fish3_m*(double(j)-a50_fish3_m))))*(1-(1/(1+mfexp(-gamma_fish3_m*(j-d50_fish3_m))))); } 
// Punt et. al 1996 gamma parameterization
    fish3_sel_f(j)=(pow(j/a50_fish3_f,a50_fish3_f/(0.5*(sqrt(square(a50_fish3_f)+4*square(delta_fish3_f))-a50_fish3_f)))*mfexp((a50_fish3_f-j)/(0.5*(sqrt(square(a50_fish3_f)+4*square(delta_fish3_f))-a50_fish3_f))));
    fish3_sel_m(j)=(pow(j/a50_fish3_m,a50_fish3_m/(0.5*(sqrt(square(a50_fish3_m)+4*square(delta_fish3_m))-a50_fish3_m)))*mfexp((a50_fish3_m-j)/(0.5*(sqrt(square(a50_fish3_m)+4*square(delta_fish3_m))-a50_fish3_m)))); }
  if(fsh3_sel_opt==4) { 
  fish3_sel_f(j) = (1/(1-gamma_fish3_f))*pow(((1-gamma_fish3_f)/gamma_fish3_f),gamma_fish3_f)*(mfexp(delta_fish3_f*gamma_fish3_f*(a50_fish3_f-j))/(1+mfexp(delta_fish3_f*(a50_fish3_f-j))));
  fish3_sel_m(j) = (1/(1-gamma_fish3_m))*pow(((1-gamma_fish3_m)/gamma_fish3_m),gamma_fish3_m)*(mfexp(delta_fish3_m*gamma_fish3_m*(a50_fish3_m-j))/(1+mfexp(delta_fish3_m*(a50_fish3_m-j)))); }
  if(fsh4_sel_opt==2) {
    fish4_sel_f(j)=1/ (1+mfexp(-delta_fish4_f*(j-a50_fish4_f)));  
    fish4_sel_m(j)=1/ (1+mfexp(-delta_fish4_m*(j-a50_fish4_m))); } 
  if(fsh4_sel_opt==3) {
    fish3_sel_f(j)=(pow(j/a50_fish3_f,a50_fish3_f/(0.5*(sqrt(square(a50_fish3_f)+4*square(delta_fish3_f))-a50_fish3_f)))*mfexp((a50_fish3_f-j)/(0.5*(sqrt(square(a50_fish3_f)+4*square(delta_fish3_f))-a50_fish3_f))));
    fish3_sel_m(j)=(pow(j/a50_fish3_m,a50_fish3_m/(0.5*(sqrt(square(a50_fish3_m)+4*square(delta_fish3_m))-a50_fish3_m)))*mfexp((a50_fish3_m-j)/(0.5*(sqrt(square(a50_fish3_m)+4*square(delta_fish3_m))-a50_fish3_m)))); }
  if(fsh4_sel_opt==4) { 
  fish4_sel_f(j) = (1/(1-gamma_fish4_f))*pow(((1-gamma_fish4_f)/gamma_fish4_f),gamma_fish4_f)*(mfexp(delta_fish4_f*gamma_fish4_f*(a50_fish4_f-j))/(1+mfexp(delta_fish4_f*(a50_fish4_f-j))));
  fish4_sel_m(j) = (1/(1-gamma_fish4_m))*pow(((1-gamma_fish4_m)/gamma_fish4_m),gamma_fish4_m)*(mfexp(delta_fish4_m*gamma_fish4_m*(a50_fish4_m-j))/(1+mfexp(delta_fish4_m*(a50_fish4_m-j)))); }
   }      
   fish2_sel=fish2_sel/max(fish2_sel);
   fish3_sel_f=fish3_sel_f/max(fish3_sel_f);
   fish3_sel_m=fish3_sel_m/max(fish3_sel_m);
 

  
    if (srv1_sel_opt==1) {
      for (j=1;j<=n_srv1_sel_ages;j++) {
        log_srv1_sel_f(j) = log_srv1_sel_coffs_f(j);
        log_srv1_sel_m(j) = log_srv1_sel_coffs_m(j); 
        log_srv1_sel_f(j) = log_srv1_sel_f(j-1);
        log_avgsrv1sel_f    = log(mean(mfexp(log_srv1_sel_coffs_f)));
        log_srv1_sel_f     -= log(mean(mfexp(log_srv1_sel_f)));
        srv1_sel_f          = mfexp(log_srv1_sel_f)/mfexp(max(log_srv1_sel_f));  //keeping max survey selectiviy at 1
        log_srv1_sel_m(j) = log_srv1_sel_m(j-1);}
        log_avgsrv1sel_m    = log(mean(mfexp(log_srv1_sel_coffs_m)));
        log_srv1_sel_m     -= log(mean(mfexp(log_srv1_sel_m)));
        srv1_sel_m          = mfexp(log_srv1_sel_m)/mfexp(max(log_srv1_sel_m));  //keeping max survey selectiviy at 1
        }
    if (srv2_sel_opt==1) {
      for (j=1;j<=n_srv1_sel_ages;j++) {
        log_srv2_sel_m(j) = log_srv2_sel_coffs_m(j);
        log_srv2_sel_f(j) = log_srv2_sel_coffs_f(j); 
        log_srv2_sel_f(j) = log_srv2_sel_f(j-1);
        log_avgsrv2sel_f    = log(mean(mfexp(log_srv2_sel_coffs_f)));
        log_srv2_sel_f     -= log(mean(mfexp(log_srv2_sel_f)));
        srv2_sel_f          = mfexp(log_srv2_sel_f)/mfexp(max(log_srv2_sel_f));  //keeping max survey selectiviy at 1
        log_srv2_sel_m(j) = log_srv2_sel_m(j-1);}
        log_avgsrv2sel_m    = log(mean(mfexp(log_srv2_sel_coffs_m)));
        log_srv2_sel_m     -= log(mean(mfexp(log_srv2_sel_m)));
      srv2_sel_m          = mfexp(log_srv2_sel_m)/mfexp(max(log_srv2_sel_m));  //keeping max survey selectiviy at 1
        }
    if (srv7_sel_opt==1) {
      for (j=1;j<=n_srv1_sel_ages;j++) {
      log_srv7_sel_f(j) = log_srv7_sel_coffs_f(j); 
        log_srv7_sel_m(j) = log_srv7_sel_coffs_m(j); 
        log_srv7_sel_f(j) = log_srv7_sel_f(j-1);
        log_avgsrv7sel_f    = log(mean(mfexp(log_srv7_sel_coffs_f)));
        log_srv7_sel_f     -= log(mean(mfexp(log_srv7_sel_f)));
        srv7_sel_f          = mfexp(log_srv7_sel_f)/mfexp(max(log_srv7_sel_f));  //keeping max survey selectiviy at 1
        log_srv7_sel_m(j) = log_srv7_sel_m(j-1); }
        log_avgsrv7sel_m    = log(mean(mfexp(log_srv7_sel_coffs_m)));
        log_srv7_sel_m     -= log(mean(mfexp(log_srv7_sel_m)));
        srv7_sel_m          = mfexp(log_srv7_sel_m)/mfexp(max(log_srv7_sel_m));  //keeping max survey selectiviy at 1
        }

   for (j=1;j<=nages;j++) {
      a50_srv1_f=mfexp(log_a50_srv1_f);
      a50_srv1_m=mfexp(log_a50_srv1_m);
      a50_srv2_f=mfexp(log_a50_srv2_f);
      a50_srv2_m=mfexp(log_a50_srv2_m);
      a50_srv7_f=mfexp(log_a50_srv7_f);
      a50_srv7_m=mfexp(log_a50_srv7_m);
    delta_srv1_f=mfexp(log_delta_srv2_m);
      delta_srv1_m=mfexp(log_delta_srv2_m);
      delta_srv2_f=mfexp(log_delta_srv2_m);
      delta_srv2_m=mfexp(log_delta_srv2_m);
      delta_srv7_f=mfexp(log_delta_srv7_f);
      delta_srv7_m=mfexp(log_delta_srv7_m);
// ----------------- Selectivity options 2 and 3
   if(srv1_sel_opt==2) { 
          srv1_sel_f(j)=1/ (1+mfexp(-delta_srv1_f*(j-a50_srv1_f)));
            srv1_sel_m(j)=1/ (1+mfexp(-delta_srv1_m*(j-a50_srv1_m))); }
   if(srv1_sel_opt==3) { 
      srv1_sel_f(j) = (1/(1+mfexp(-delta_srv1_f*(double(j)-a50_srv1_f))))*(1-(1/(1+mfexp(-gamma_srv1_f*(j-d50_srv1_f)))));
      srv1_sel_m(j) = (1/(1+mfexp(-delta_srv1_m*(double(j)-a50_srv1_m))))*(1-(1/(1+mfexp(-gamma_srv1_m*(j-d50_srv1_m)))));  }
  if(srv1_sel_opt==4) { 
  srv1_sel_f(j) = (1/(1-gamma_srv1_f))*pow(((1-gamma_srv1_f)/gamma_srv1_f),gamma_srv1_f)*(mfexp(delta_srv1_f*gamma_srv1_f*(a50_srv1_f-j))/(1+mfexp(delta_srv1_f*(a50_srv1_f-j))));
  srv1_sel_m(j) = (1/(1-gamma_srv1_m))*pow(((1-gamma_srv1_m)/gamma_srv1_m),gamma_srv1_m)*(mfexp(delta_srv1_m*gamma_srv1_m*(a50_srv1_m-j))/(1+mfexp(delta_srv1_m*(a50_srv1_m-j)))); }
   if(srv2_sel_opt==2) { 
            srv2_sel_f(j)=1/ (1+mfexp(-delta_srv2_f*(j-a50_srv2_f)));
            srv2_sel_m(j)=1/ (1+mfexp(-delta_srv2_m*(j-a50_srv2_m))); }
   if(srv2_sel_opt==3) { 
      srv2_sel_f(j) = (1/(1+mfexp(-delta_srv2_f*(double(j)-a50_srv2_f))))*(1-(1/(1+mfexp(-gamma_srv2_f*(j-d50_srv2_f)))));
      srv2_sel_m(j) = (1/(1+mfexp(-delta_srv2_m*(double(j)-a50_srv2_m))))*(1-(1/(1+mfexp(-gamma_srv2_m*(j-d50_srv2_m))))); }
  if(srv2_sel_opt==4) { 
  srv2_sel_f(j) = (1/(1-gamma_srv2_f))*pow(((1-gamma_srv2_f)/gamma_srv2_f),gamma_srv2_f)*(mfexp(delta_srv2_f*gamma_srv2_f*(a50_srv2_f-j))/(1+mfexp(delta_srv2_f*(a50_srv2_f-j))));
  srv2_sel_m(j) = (1/(1-gamma_srv2_m))*pow(((1-gamma_srv2_m)/gamma_srv2_m),gamma_srv2_m)*(mfexp(delta_srv2_m*gamma_srv2_m*(a50_srv2_m-j))/(1+mfexp(delta_srv2_m*(a50_srv2_m-j)))); }
   if(srv7_sel_opt==2) { 
            srv7_sel_f(j)=1/ (1+mfexp(-delta_srv7_f*(j-a50_srv7_f)));
            srv7_sel_m(j)=1/ (1+mfexp(-delta_srv7_m*(j-a50_srv7_m)));}
   if(srv7_sel_opt==3) { 
 //     srv7_sel_f(j) = (1/(1+mfexp(-delta_srv7_f*(double(j)-a50_srv7_f))))*(1-(1/(1+mfexp(-gamma_srv7_f*(j-d50_srv7_f)))));
 //     srv7_sel_m(j) = (1/(1+mfexp(-delta_srv7_m*(double(j)-a50_srv7_m))))*(1-(1/(1+mfexp(-gamma_srv7_m*(j-d50_srv7_m))))); }
  srv7_sel_f(j)=(1/pow(j,a50_srv7_f));
  srv7_sel_m(j)=(1/pow(j,a50_srv7_m)); }
 if(srv7_sel_opt==4) { 
  srv7_sel_f(j) = (1/(1-gamma_srv7_f))*pow(((1-gamma_srv7_f)/gamma_srv7_f),gamma_srv7_f)*(mfexp(delta_srv7_f*gamma_srv7_f*(a50_srv7_f-j))/(1+mfexp(delta_srv7_f*(a50_srv7_f-j))));
  srv7_sel_m(j) = (1/(1-gamma_srv7_m))*pow(((1-gamma_srv7_m)/gamma_srv7_m),gamma_srv7_m)*(mfexp(delta_srv7_m*gamma_srv7_m*(a50_srv7_m-j))/(1+mfexp(delta_srv7_m*(a50_srv7_m-j)))); }
  if(srv7_sel_opt==5) { 
  srv7_sel_f(j) = (1/(1-gamma_srv7_f))*pow(((1-gamma_srv7_f)/gamma_srv7_f),gamma_srv7_f)*(mfexp(delta_srv7_f*gamma_srv7_f*(a50_srv7_f-j))/(1+mfexp(delta_srv7_f*(a50_srv7_f-j))));
  srv7_sel_m(j) = (1/(1-gamma_srv7_m))*pow(((1-gamma_srv7_m)/gamma_srv7_m),gamma_srv7_m)*(mfexp(delta_srv7_m*gamma_srv7_m*(a50_srv7_m-j))/(1+mfexp(delta_srv7_m*(a50_srv7_m-j)))); }
 }
 //               srv1_sel_f=srv1_sel_f/max(srv1_sel_f);
   //              srv1_sel_m=srv1_sel_m/max(srv1_sel_m);
    //            srv2_sel_f=srv2_sel_f/max(srv2_sel_f);
    //             srv2_sel_m=srv2_sel_m/max(srv2_sel_m);
   
    /*    srv7_sel_f(j)= -(pow(j,a50_srv7_f)); // more flexible way to get convex or concave domeshaped with one parameter
     srv7_sel_m(j)= -(pow(j,a50_srv7_m)); } 

                 srv7_sel_f= 1- srv7_sel_f/min(srv7_sel_f);
                srv7_sel_m= 1- srv7_sel_m/min(srv7_sel_m);
                 srv7_sel_f /= max(srv7_sel_f);
                srv7_sel_m /= max(srv7_sel_m);  */
                    srv7_sel_f /= max(srv7_sel_f);
                srv7_sel_m /= max(srv7_sel_m);  
           
 

FUNCTION Get_Mortality_Rates
// Calculate mortality rates
  natmort        = exp(logm);                     // setting natural mortality to arithmetic scale
  Fmort_fish1         = mfexp(log_avg_F_fish1+  log_F_devs_fish1);          //setting fishing mortaltiy to arithmetic scale
//  for(iyr=styr;iyr<=styr+2;iyr++) Fmort_fish3(iyr)=0;
 Fmort_fish3         = mfexp(log_avg_F_fish3 +log_F_devs_fish3);          //trawl mortality starts in 1963 (hence +3)
 hist_hal_F = hist_hal_prop*mfexp(log_avg_F_fish1);  // optional historical fishing mortality for initial age comps
  for (iyr=styr; iyr<=1994; iyr++) {
    for (j = 1 ; j<= nages; j++) {
      F_fish1_f(iyr,j) = Fmort_fish1(iyr) * fish1_sel_f(j);             // Getting fully selected fishing mortality
       F_fish1_m(iyr,j) = Fmort_fish1(iyr) * fish1_sel_m(j);              // Getting fully selected fishing mortality
     F_fish3_f(iyr,j) = Fmort_fish3(iyr) * fish3_sel_f(j);
     F_fish3_m(iyr,j) = Fmort_fish3(iyr) * fish3_sel_m(j);}}
  for (iyr=1995; iyr<=endyr; iyr++) {
    for (j = 1 ; j<= nages; j++) {
     if(ph_ifq==1) {
        F_fish1_f(iyr,j)=Fmort_fish1(iyr)*fish4_sel_f(j);
        F_fish1_m(iyr,j)=Fmort_fish1(iyr)*fish4_sel_m(j); }
     else {
        F_fish1_f(iyr,j)=Fmort_fish1(iyr)*fish1_sel_f(j);
        F_fish1_m(iyr,j)=Fmort_fish1(iyr)*fish1_sel_m(j); } 
            F_fish3_f(iyr,j) = Fmort_fish3(iyr) * fish3_sel_f(j);
            F_fish3_m(iyr,j) = Fmort_fish3(iyr) * fish3_sel_m(j); }}

  Z_f            = F_fish1_f +F_fish3_f +natmort;                     // Fully selected total mortality
  Z_m            = F_fish1_m +F_fish3_m +natmort+mdelta;                      // Fully selected total mortality
  S_f            = mfexp(-1.0*Z_f);                   // Fully selected survival
  S_m            = mfexp(-1.0*Z_m);                   // Fully selected survival
  S_f_mid    = mfexp(-0.5*Z_f);
  S_m_mid    = mfexp(-0.5*Z_m);
  
// Next two sections are based on Baranov catch equations

FUNCTION Get_Numbers_At_Age  
// Calculate Numbers at age
// Start year

 switch (SrType)
  {
    case 3: 
  int itmp;
  
  if(rec_like_type==3) {
     natage_f(styr,1)=mfexp(log_Rztemp)*mfexp((sigr*sigr)/2)/2;
     natage_m(styr,1)=mfexp(log_Rztemp)*mfexp((sigr*sigr)/2)/2; }

 if(rec_like_type<3) {  
     natage_f(styr,1)=mfexp(log_mean_rec+log_rec_dev(styr)+sigr*sigr/2)/2;
     natage_m(styr,1)=mfexp(log_mean_rec+log_rec_dev(styr)+sigr*sigr/2)/2; 
  for (j=2;j<nages;j++)
  {
    itmp = styr+1-j;

   natage_f(styr,j) =( mfexp(log_mean_rec  - (natmort+hist_hal_F*fish1_sel_f(j)) * double(j-1)+ log_rec_dev(itmp)+sigr*sigr/2))/2; 
   natage_m(styr,j) = (mfexp(log_mean_rec  - (natmort+mdelta+hist_hal_F*fish1_sel_m(j)) * double(j-1)+ log_rec_dev(itmp)+sigr*sigr/2))/2; 
  }
    natage_f(styr,nages)      = (mfexp(log_mean_rec - (natmort+hist_hal_F*fish1_sel_f(nages-1)) * (nages-1))/ (1. - exp(-natmort+hist_hal_F*fish1_sel_f(nages-1)) ))/2;
    natage_m(styr,nages)      = (mfexp(log_mean_rec - (natmort+mdelta+hist_hal_F*fish1_sel_m(nages-1)) * (nages-1))/ (1. - exp(-(natmort+mdelta+hist_hal_F*fish1_sel_f(nages-1))) ))/2; }
   break;
   }
 // cout<<"LMR "<<log_mean_rec;
  for ( i=styr;i <= endyr_rec;i++)
  {
    natage_f(i,1)           = mfexp(log_rec_dev(i) + log_mean_rec+sigr*sigr/2 )/2;
    natage_m(i,1)           = mfexp(log_rec_dev(i) + log_mean_rec+sigr*sigr/2 )/2;
    sam_rec(i)            = natage_f(i,1)+natage_m(i,1); // OjO
    natage_f(i+1)(2,nages)  = ++elem_prod(natage_f(i)(1,nages-1),S_f(i)(1,nages-1));       // Following year
    natage_m(i+1)(2,nages)  = ++elem_prod(natage_m(i)(1,nages-1),S_m(i)(1,nages-1));       // Following year
    natage_f(i+1,nages)    += natage_f(i,nages)*S_f(i,nages);
     natage_m(i+1,nages)    += natage_m(i,nages)*S_m(i,nages);
   Sp_Biom(i)  = elem_prod(natage_f(i),pow(S_f(i),spawn_fract)) * wt_mature; 
  }
// End year
//  natage(endyr,1)         = mfexp(log_rec_dev(endyr_rec) + log_mean_rec ); 

    for(i=endyr_rec+1;i<endyr;i++){
      natage_f(i,1)=mfexp(log_mean_rec+sigr*sigr/2)/2;
      natage_m(i,1)=mfexp(log_mean_rec+sigr*sigr/2)/2;
      sam_rec(i)            = natage_f(i,1)+natage_m(i,1); // OjO
      natage_f(i+1)(2,nages)  = ++elem_prod(natage_f(i)(1,nages-1),S_f(i)(1,nages-1));       // Following year
      natage_m(i+1)(2,nages)  = ++elem_prod(natage_m(i)(1,nages-1),S_m(i)(1,nages-1));       // Following year
      natage_f(i+1,nages)    += natage_f(i,nages)*S_f(i,nages);
       natage_m(i+1,nages)    += natage_m(i,nages)*S_m(i,nages);
      Sp_Biom(i)  = elem_prod(natage_f(i),pow(S_f(i),spawn_fract)) * wt_mature; 
  
     
  }
  if(rec_like_type==1) {
     natage_f(endyr,1)=(mfexp(log_mean_rec+log_rec_dev(endyr)))/2;
     natage_m(endyr,1)=(mfexp(log_mean_rec+log_rec_dev(endyr)))/2; }
  else  {
  natage_f(endyr,1)         = mfexp(log_mean_rec )/2; 
  natage_m(endyr,1)         = mfexp(log_mean_rec )/2; }
  Sp_Biom(endyr)  = elem_prod(natage_f(endyr),pow(S_f(endyr),spawn_fract)) * wt_mature; 
  
//   natage_mid_f=elem_prod(natage_f,S_f_mid);
//  natage_mid_m=elem_prod(natage_f,S_m_mid); 
FUNCTION Get_Catch_at_Age
// Calculate catch at age
  pred_catch_fish1.initialize();
  pred_catch_fish3.initialize();
  for (iyr=styr; iyr<=endyr; iyr++) {
    catage_fish1_m(iyr) = elem_div(elem_prod(elem_prod(natage_m(iyr),F_fish1_m(iyr)),(1.-S_m(iyr))),Z_m(iyr));
    catage_fish1_f(iyr) = elem_div(elem_prod(elem_prod(natage_f(iyr),F_fish1_f(iyr)),(1.-S_f(iyr))),Z_f(iyr));
    pred_catch_fish1(iyr) = elem_div(elem_prod(elem_prod(natage_f(iyr),F_fish1_f(iyr)),(1.-S_f(iyr))),Z_f(iyr))*wt_f+elem_div(elem_prod(elem_prod(natage_m(iyr),F_fish1_m(iyr)),(1.-S_m(iyr))),Z_m(iyr))*wt_m;
    catage_fish3_m(iyr) = elem_div(elem_prod(elem_prod(natage_m(iyr),F_fish3_m(iyr)),(1.-S_m(iyr))),Z_m(iyr));
    catage_fish3_f(iyr) = elem_div(elem_prod(elem_prod(natage_f(iyr),F_fish3_f(iyr)),(1.-S_f(iyr))),Z_f(iyr));
    pred_catch_fish3(iyr) = elem_div(elem_prod(elem_prod(natage_f(iyr),F_fish3_f(iyr)),(1.-S_f(iyr))),Z_f(iyr))*wt_f+elem_div(elem_prod(elem_prod(natage_m(iyr),F_fish3_m(iyr)),(1.-S_m(iyr))),Z_m(iyr))*wt_m;
  }

FUNCTION Get_Dependent_Vars
  for (i=styr;i<=endyr;i++)
  {
    pred_rec(i) = natage_f(i,1)+natage_m(i,1);                  // Setting up results based on estimated paramters
    tot_biom(i) = wt_f * natage_f(i)+wt_m*natage_m(i);                // Total biomass results
    spawn_biom(i) = wt_mature*natage_f(i) ;   // Spawning biomass result
  }

  avg_rec        = mean(pred_rec);
  Depletion      = spawn_biom(endyr)/spawn_biom(styr);                         // Depletion
  spbiom_trend   = spawn_biom(endyr)/spawn_biom(endyr-1);
FUNCTION Get_Predicted_Values
// Calculate predicted data values

   q_srv1         = exp(log_q_srv1);                                        // Survey catchability at arithmetic scale
   q_srv2         = exp(log_q_srv2);                                        // Survey catchability at arithmetic scale
   q_srv3         = exp(log_q_srv3);                                        // Survey catchability at arithmetic scale
   q_srv4         = exp(log_q_srv4);                                         // Survey catchability at arithmetic scale
   q_srv5         = exp(log_q_srv5);                                        // Survey catchability at arithmetic scale
   q_srv6         = exp(log_q_srv6);                                        // Survey catchability at arithmetic scale
   q_srv7         = exp(log_q_srv7);                                        // Survey catchability at arithmetic scale
   q_srv8         = exp(log_q_srv8);                                        // Survey catchability at arithmetic scale
   q_srv9         = exp(log_q_srv2);                                        // Survey catchability at arithmetic scale
          
  for (i=styr;i<=endyr;i++) { 
    pred_srv1(i) = 2*(q_srv1* (1-prop_m(i))*(elem_prod(S_f_mid(i),natage_f(i))*elem_prod(srv1_sel_f,wt_f))+q_srv1*prop_m(i)* (elem_prod(S_m_mid(i),natage_m(i))*elem_prod(srv1_sel_m,wt_m)));   // Predicted Survey biomass
    pred_srv3(i) = 2*(q_srv1 * (1-prop_m(i))*(elem_prod(S_f_mid(i),natage_f(i))*srv1_sel_f)+q_srv1 *prop_m(i)* (elem_prod(S_m_mid(i),natage_m(i))*srv1_sel_m));   // Predicted Survey biomass
    pred_srv6(i) = q_srv6 * (elem_prod(S_f_mid(i),natage_f(i))*elem_prod(wt_f,fish2_sel)+ elem_prod(S_m_mid(i),natage_m(i))*elem_prod(wt_m,fish2_sel));   // Predicted Survey biomass
    pred_srv7(i) = 2*(q_srv7 *(1-prop_m2(i))*elem_prod(S_f_mid(i),natage_f(i))*elem_prod(srv7_sel_f,wt_f)+ q_srv7*prop_m2(i)*elem_prod(S_m_mid(i),natage_m(i))*elem_prod(srv7_sel_m,wt_m));   // Predicted Survey biomass
  pred_srv4(i) = 2*(q_srv2* (1-prop_m(i))*(elem_prod(S_f_mid(i),natage_f(i))*srv2_sel_f)+q_srv2 *prop_m(i)* (elem_prod(S_m_mid(i),natage_m(i))*srv2_sel_m)); }  // Predicted Survey biomass 
    for (i=1979;i<=1989;i++) {  pred_srv2(i)=2*(q_srv2* (1-prop_m(i))*(elem_prod(S_f_mid(i),natage_f(i))*elem_prod(srv2_sel_f,wt_f))+q_srv2 * prop_m(i)*(elem_prod(S_m_mid(i),natage_m(i))*elem_prod(srv2_sel_m,wt_m))); 
                  pred_srv4(i) = 2*(q_srv2* (1-prop_m(i))*(elem_prod(S_f_mid(i),natage_f(i))*srv2_sel_f)+q_srv2 *prop_m(i)* (elem_prod(S_m_mid(i),natage_m(i))*srv2_sel_m)); }  // Predicted Survey biomass 
       
    for (i=1990;i<=1994;i++)  { pred_srv2(i) = 2*(q_srv9* (1-prop_m(i))*(elem_prod(S_f_mid(i),natage_f(i))*elem_prod(srv2_sel_f,wt_f))+q_srv9 *prop_m(i)* (elem_prod(S_m_mid(i),natage_m(i))*elem_prod(srv2_sel_m,wt_m)));    // Predicted Survey biomass
                  pred_srv4(i) = 2*(q_srv9 * (1-prop_m(i))*(elem_prod(S_f_mid(i),natage_f(i))*srv2_sel_f)+q_srv9 *prop_m(i)* (elem_prod(S_m_mid(i),natage_m(i))*srv2_sel_m)); }   // Predicted Survey biomass 
       
    for (i=styr;i<=1994;i++) pred_srv5(i) = q_srv5 * (elem_prod(S_f_mid(i),natage_f(i))*elem_prod(fish1_sel_f,wt_f))+q_srv5 * (elem_prod(S_m_mid(i),natage_m(i))*elem_prod(fish1_sel_m,wt_m));    // Predicted Survey biomass
    for (i=1995;i<=endyr;i++) { 
          if(ph_ifq==1) pred_srv5(i) = q_srv8 * (elem_prod(S_f_mid(i),natage_f(i))*elem_prod(fish4_sel_f,wt_f))+q_srv8 * (elem_prod(S_m_mid(i),natage_m(i))*elem_prod(fish4_sel_m,wt_m));     // Predicted Survey biomass
          else pred_srv5(i) = q_srv8 * (elem_prod(S_f_mid(i),natage_f(i))*elem_prod(fish1_sel_f,wt_f))+q_srv8 * (elem_prod(S_m_mid(i),natage_m(i))*elem_prod(fish1_sel_m,wt_m)); }  // Predicted Survey biomass

   for (i=1;i<=nyrs_fish1_age;i++) {
    eac_fish1(i)  = ((catage_fish1_m(yrs_fish1_age(i))/sum(catage_fish1_m(yrs_fish1_age(i))))+(catage_fish1_f(yrs_fish1_age(i))/sum(catage_fish1_f(yrs_fish1_age(i)))))/2* ageage;                                                // Predicted Fishery age comps
    eac_fish1(i) /=sum(eac_fish1(i)); }
  for (i=1;i<=nyrs_srv1_age;i++) {
    eac_srv1(i)  = ((1-prop_m(yrs_srv1_age(i)))*elem_prod(srv1_sel_f,natage_f(yrs_srv1_age(i)))+prop_m(yrs_srv1_age(i))*elem_prod(srv1_sel_m,natage_m(yrs_srv1_age(i))))* ageage;                         // Predicted Survey age comps
    eac_srv1(i) /=sum(eac_srv1(i)); }
  for (i=1;i<=nyrs_srv2_age;i++) {
    eac_srv2(i)  = (elem_prod(srv2_sel_f,natage_f(yrs_srv2_age(i)))+elem_prod(srv2_sel_m,natage_m(yrs_srv2_age(i))))*ageage;                        // Predicted Survey age comps
    eac_srv2(i) /=sum(eac_srv2(i)); }
  for (i=1;i<=nyrs_srv7_age;i++) {
    eac_srv7(i)  = ((1-prop_m(yrs_srv7_age(i)))*elem_prod(srv7_sel_f,natage_f(yrs_srv7_age(i)))+prop_m(yrs_srv7_age(i))*elem_prod(srv7_sel_m,natage_m(yrs_srv7_age(i))))* ageage;                         // Predicted Survey age comps
    eac_srv7(i) /=sum(eac_srv7(i)); }

  for (i=1;i<=5;i++) {                      // Lets you use a second matrix for part of it
    esc_fish1_m(i)  = catage_fish1_m(yrs_fish1_size(i))/sum(catage_fish1_m(yrs_fish1_size(i)))* sizeage_m;    
    esc_fish1_f(i)  = catage_fish1_f(yrs_fish1_size(i))/sum(catage_fish1_f(yrs_fish1_size(i)))* sizeage_f;   } 
  for (i=6;i<=nyrs_fish1_size;i++) {                      // Lets you use a second matrix for part of it
    esc_fish1_m(i)  = catage_fish1_m(yrs_fish1_size(i))/sum(catage_fish1_m(yrs_fish1_size(i)))* sizeage_m_new;    
    esc_fish1_f(i)  = catage_fish1_f(yrs_fish1_size(i))/sum(catage_fish1_f(yrs_fish1_size(i)))* sizeage_f_new;  }  
  for (i=1;i<=1;i++) {                      // Lets you use a second matrix for part of it
    esc_fish3_m(i)  = (catage_fish3_m(yrs_fish3_size(i))/sum(catage_fish3_m(yrs_fish3_size(i))))* sizeage_m;                                              // Second Predicted Fishery size comps for 80s and 90s
    esc_fish3_f(i)  = (catage_fish3_f(yrs_fish3_size(i))/sum(catage_fish3_f(yrs_fish3_size(i))))* sizeage_f;  }                                             // Second Predicted Fishery size comps for 80s and 90s
  for (i=2;i<=nyrs_fish3_size;i++) {                      // Lets you use a second matrix for part of it
    esc_fish3_m(i)  = (catage_fish3_m(yrs_fish3_size(i))/sum(catage_fish3_m(yrs_fish3_size(i))))* sizeage_m_new;                                              // Second Predicted Fishery size comps for 80s and 90s
    esc_fish3_f(i)  = (catage_fish3_f(yrs_fish3_size(i))/sum(catage_fish3_f(yrs_fish3_size(i))))* sizeage_f_new; }                                              // Second Predicted Fishery size comps for 80s and 90s
  for (i=1;i<=nyrs_fish2_size;i++)  {                     // Lets you use a second matrix for part of it
   esc_fish2(i)  = ((1-prop_m(yrs_fish2_size(i)))*elem_prod(fish2_sel,natage_f(yrs_fish2_size(i)))+prop_m(yrs_fish2_size(i))*elem_prod(fish2_sel,natage_m(yrs_fish2_size(i))))* sizeage_all;                          // Predicted Survey age comps
   esc_fish2(i) /=sum(esc_fish2(i)); }

                                                           // Second Predicted Fishery size comps for 80s and 90s
  for ( i=1;i<=5;i++) {
    esc_srv1_m(i)  = elem_prod(srv1_sel_m,natage_m(yrs_srv1_size(i)))* sizeage_m;        // Predicted Survey size comps (not used in POP model)
    esc_srv1_m(i)  /=sum(esc_srv1_m(i));
     esc_srv1_f(i)  = elem_prod(srv1_sel_f,natage_f(yrs_srv1_size(i))) * sizeage_f;        // Predicted Survey size comps (not used in POP model)
    esc_srv1_f(i)  /=sum(esc_srv1_f(i)); }
  for ( i=6;i<=nyrs_srv1_size;i++) {
    esc_srv1_m(i)  = elem_prod(srv1_sel_m,natage_m(yrs_srv1_size(i)))* sizeage_m_new;        // Predicted Survey size comps (not used in POP model)
    esc_srv1_m(i)  /=sum(esc_srv1_m(i));
     esc_srv1_f(i)  = elem_prod(srv1_sel_f,natage_f(yrs_srv1_size(i))) * sizeage_f_new;        // Predicted Survey size comps (not used in POP model)
    esc_srv1_f(i)  /=sum(esc_srv1_f(i)); }
  for ( i=1;i<=nyrs_srv2_size;i++) {
    esc_srv2_m(i)  = elem_prod(srv2_sel_m,natage_m(yrs_srv2_size(i)))*  sizeage_m;        // Predicted Survey size comps (not used in POP model)
    esc_srv2_m(i)  /=sum(esc_srv2_m(i)); 
    esc_srv2_f(i)  = elem_prod(srv2_sel_f,natage_f(yrs_srv2_size(i))) * sizeage_f;        // Predicted Survey size comps (not used in POP model)
    esc_srv2_f(i)  /=sum(esc_srv2_f(i)); }
  for ( i=1;i<=4;i++) {
    esc_srv7_m(i)  = elem_prod(srv7_sel_m,natage_m(yrs_srv7_size(i)))*sizeage_m;        // Predicted Survey size comps (not used in POP model)
    esc_srv7_f(i)  = elem_prod(srv7_sel_f,natage_f(yrs_srv7_size(i)))* sizeage_f;       // Predicted Survey size comps (not used in POP model)
    esc_srv7_f(i)  /=sum(esc_srv7_f(i)); 
    esc_srv7_m(i)  /=sum(esc_srv7_m(i)); }
  for ( i=5;i<=nyrs_srv7_size;i++) {
    esc_srv7_m(i)  = elem_prod(srv7_sel_m,natage_m(yrs_srv7_size(i)))*sizeage_m_new;        // Predicted Survey size comps (not used in POP model)
    esc_srv7_f(i)  = elem_prod(srv7_sel_f,natage_f(yrs_srv7_size(i)))* sizeage_f_new;       // Predicted Survey size comps (not used in POP model)
    esc_srv7_f(i)  /=sum(esc_srv7_f(i)); 
    esc_srv7_m(i)  /=sum(esc_srv7_m(i)); }
  
    pred_catch=(pred_catch_fish1+pred_catch_fish3);

FUNCTION compute_spr_rates
  //Compute SPR Rates and add them to the likelihood for Females 
  fratio = Fmort_fish1(endyr)/(Fmort_fish1(endyr)+Fmort_fish3(endyr));
  // Scale F-spr rates to be on full-selected values
  F50  = mF50*max(fish4_sel_f);
  F40  = mF40*max(fish4_sel_f);
  F35  = mF35*max(fish4_sel_f);
  SB0 =0;
  SBF50=0;
  SBF40=0;
  SBF35=0;
  for (i=1;i<=4;i++)
    Nspr(i,1)=1.;
  for (j=2;j<nages;j++)
  {
    Nspr(1,j)=Nspr(1,j-1)*mfexp(-1.*natmort);
    Nspr(2,j)=Nspr(2,j-1)*mfexp(-1.*(natmort+fratio*mF50*fish4_sel_f(j-1)+(1-fratio)*mF50*fish3_sel_f(j-1)));
    Nspr(3,j)=Nspr(3,j-1)*mfexp(-1.*(natmort+fratio*mF40*fish4_sel_f(j-1)+(1-fratio)*mF40*fish3_sel_f(j-1)));
    Nspr(4,j)=Nspr(4,j-1)*mfexp(-1.*(natmort+fratio*mF35*fish4_sel_f(j-1)+(1-fratio)*mF35*fish3_sel_f(j-1)));
  }
    Nspr(1,nages)=Nspr(1,nages-1)*mfexp(-1.*natmort)/(1.-mfexp(-1.*natmort));
  Nspr(2,nages)=Nspr(2,nages-1)*mfexp(-1.* (natmort+fratio*mF50*fish4_sel_f(nages-1)+(1-fratio)*mF50*fish3_sel_f(nages-1)))/ (1.-mfexp(-1.*(natmort+fratio*mF50*fish4_sel_f(nages)+(1-fratio)*mF50*fish3_sel_f(nages))));
  Nspr(3,nages)=Nspr(3,nages-1)*mfexp(-1.* (natmort+fratio*mF40*fish4_sel_f(nages-1)+(1-fratio)*mF40*fish3_sel_f(nages-1)))/ (1.-mfexp(-1.*(natmort+fratio*mF40*fish4_sel_f(nages)+(1-fratio)*mF40*fish3_sel_f(nages))));
  Nspr(4,nages)=Nspr(4,nages-1)*mfexp(-1.* (natmort+fratio*mF35*fish4_sel_f(nages-1)+(1-fratio)*mF35*fish3_sel_f(nages-1)))/ (1.-mfexp(-1.*(natmort+fratio*mF35*fish4_sel_f(nages)+(1-fratio)*mF35*fish3_sel_f(nages))));
  for (j=1;j<=nages;j++)
  {
   // Kill them off till (spawn_fract)
    SB0    += Nspr(1,j)*wt_mature(j)*mfexp(-spawn_fract*natmort);
    SBF50  += Nspr(2,j)*wt_mature(j)*mfexp(-spawn_fract*(natmort+fratio*mF50*fish4_sel_f(j)+(1-fratio)*mF50*fish3_sel_f(j)));
    SBF40  += Nspr(3,j)*wt_mature(j)*mfexp(-spawn_fract*(natmort+fratio*mF40*fish4_sel_f(j)+(1-fratio)*mF40*fish3_sel_f(j)));
    SBF35  += Nspr(4,j)*wt_mature(j)*mfexp(-spawn_fract*(natmort+fratio*mF35*fish4_sel_f(j)+(1-fratio)*mF35*fish3_sel_f(j)));
  
   } 
  sprpen    = 100.*square(SBF50/SB0-0.5);
  sprpen   += 100.*square(SBF40/SB0-0.4);
  sprpen   += 100.*square(SBF35/SB0-0.35);
  B40= 0.5*SBF40*mean(pred_rec(1979,endyr-recage));
FUNCTION Calc_priors
// Calculate priors
    priors.initialize();
    if (active(sigr))
      priors(1)    = square(log((sigr/sigrprior)))/(2.*square(cvsigrprior));
    if (active(log_q_srv1))
      priors(2)    = square(log_q_srv1-log(q_srv1prior))/(2.*square(cvq_srv1prior));
    if (active(steepness))
      priors(3)    = square(log(steepness/steep_prior))/(2.*cv_steep_prior); // not used in POP model
    if (active(logm))
      priors(4)    = square(logm-log(mprior))/(2.*square(cvmprior));
    if (active(log_q_srv2))
      priors(5)    = square(log_q_srv2-log(q_srv2prior))/(2.*square(cvq_srv2prior));
    if (active(log_q_srv3))
      priors(6)    = square(log_q_srv3-log(q_srv3prior))/(2.*square(cvq_srv3prior));
    if (active(log_q_srv4))
      priors(7)    = square(log_q_srv4-log(q_srv4prior))/(2.*square(cvq_srv4prior));
    if (active(log_q_srv5))
      priors(8)    = square(log_q_srv5-log(q_srv5prior))/(2.*square(cvq_srv5prior));
    if (active(log_q_srv6))
      priors(9)    = square(log_q_srv6-log(q_srv6prior))/(2.*square(cvq_srv6prior));
    if (active(log_q_srv7))
      priors(10)    = square(log_q_srv7-log(q_srv7prior))/(2.*square(cvq_srv7prior));
    if (active(log_q_srv8))
      priors(11)    = square(log_q_srv8-log(q_srv8prior))/(2.*square(cvq_srv8prior));

FUNCTION Surv_Likelihood
 // Calculate likelihood for survey biomass
  surv_like.initialize();
  for (i=1; i<=nyrs_srv1; i++)  {   ii=yrs_srv1(i);
  surv_like(1) += square((log(obs_srv1_biom(i)+0.0001)-log(pred_srv1(ii)+0.0001) ))/ (2.*square(obs_srv1_se(i)/obs_srv1_biom(i))); }// log-likelihood for survey biomass
  for (i=1; i<=nyrs_srv2; i++)  {   ii=yrs_srv2(i);
  surv_like(2) += square((log(obs_srv2_biom(i)+0.0001)-log(pred_srv2(ii)+0.0001) ))/ (2.*square(obs_srv2_se(i)/obs_srv2_biom(i))); }
  for (i=1; i<=nyrs_srv3; i++)  {   ii=yrs_srv3(i);
   surv_like(3) += square((log(obs_srv3_biom(i)+0.0001)-log(pred_srv3(ii)+0.0001) ))/ (2.*square(obs_srv3_se(i)/obs_srv3_biom(i))); }
  for (i=1; i<=nyrs_srv4; i++)  {   ii=yrs_srv4(i);
  surv_like(4) += square((log(obs_srv4_biom(i)+0.0001)-log(pred_srv4(ii)+0.0001) ))/ (2.*square(obs_srv4_se(i)/obs_srv4_biom(i))); }
  for (i=1; i<=nyrs_srv5; i++)  {   ii=yrs_srv5(i);
  surv_like(5) += square((log(obs_srv5_biom(i)+0.0001)-log(pred_srv5(ii)+0.0001) ))/ (2.*square(obs_srv5_se(i)/obs_srv5_biom(i))); }
  for (i=1; i<=nyrs_srv6; i++)  {   ii=yrs_srv6(i);
  surv_like(6) += square((log(obs_srv6_biom(i)+0.0001)-log(pred_srv6(ii)+0.0001) ))/ (2.*square(obs_srv6_se(i)/obs_srv6_biom(i))); }
  for (i=1; i<=nyrs_srv7; i++)  {   ii=yrs_srv7(i);
  surv_like(7) += square((log(obs_srv7_biom(i)+0.0001)-log(pred_srv7(ii)+0.0001) ))/ (2.*square(obs_srv7_se(i)/obs_srv7_biom(i))); }
   
 
   // likelihood for survey biomass 
  surv_like(1) *= wt_srv1 ;  
  surv_like(2) *= wt_srv2 ;  
  surv_like(3) *= wt_srv3 ;  
  surv_like(4) *= wt_srv4 ;  
  surv_like(5) *= wt_srv5 ;  
  surv_like(6) *= wt_srv6 ;  
  surv_like(7) *= wt_srv7 ;  
  surv_like(8) *= wt_srv8 ;  

FUNCTION Multinomial_Likelihood
// Calculate multinomial likelihoods for survey age, fishery size, and survey size and subtract "offset"
  age_like.initialize();

  for (i=1; i <= nyrs_fish1_age; i++)
    age_like(1) -= nsamples_fish1_age(i)*((oac_fish1(i) + 0.001) * log(eac_fish1(i) + 0.001)) ;

  for (i=1; i <= nyrs_srv1_age; i++)
    age_like(2) -= nsamples_srv1_age(i)*((oac_srv1(i) + 0.001) * log(eac_srv1(i) + 0.001)) ;

  for (i=1; i <= nyrs_fish1_size; i++) age_like(3) -= nsamples_fish1_size(i)*((osc_fish1_f(i) + 0.001) * log(esc_fish1_f(i) + 0.001)) ;
  for (i=1; i <= nyrs_fish1_size; i++) age_like(4) -= nsamples_fish1_size(i)*((osc_fish1_m(i) + 0.001) * log(esc_fish1_m(i) + 0.001)) ;
  for (i=1; i <= nyrs_fish2_size; i++) age_like(5) -= nsamples_fish2_size(i)*((osc_fish2(i) + 0.001) * log(esc_fish2(i) + 0.001)) ;
  for (i=1; i <= nyrs_fish3_size; i++) age_like(6) -= nsamples_fish3_size(i)*((osc_fish3_f(i) + 0.001) * log(esc_fish3_f(i) + 0.001)) ;
  for (i=1; i <= nyrs_fish3_size; i++) age_like(7) -= nsamples_fish3_size(i)*((osc_fish3_m(i) + 0.001) * log(esc_fish3_m(i) + 0.001)) ;
  for (i=1; i <= nyrs_fish4_size; i++) age_like(8) -= nsamples_fish4_size(i)*((osc_fish4(i) + 0.001) * log(esc_fish4(i) + 0.001)) ;

  for (i=1; i <= nyrs_srv1_size; i++)  age_like(9) -= nsamples_srv1_size(i)*((osc_srv1_f(i) + 0.001) * log(esc_srv1_f(i) + 0.001)) ;
  for (i=1; i <= nyrs_srv1_size; i++)  age_like(10) -= nsamples_srv1_size(i)*((osc_srv1_m(i) + 0.001) * log(esc_srv1_m(i) + 0.001)) ;
  for (i=1; i <= nyrs_srv2_size; i++)  age_like(11) -= nsamples_srv2_size(i)*((osc_srv2_f(i) + 0.001) * log(esc_srv2_f(i) + 0.001)) ;
  for (i=1; i <= nyrs_srv2_size; i++)  age_like(12) -= nsamples_srv2_size(i)*((osc_srv2_m(i) + 0.001) * log(esc_srv2_m(i) + 0.001)) ;
  for (i=1; i <= nyrs_srv7_size; i++)  age_like(13) -= nsamples_srv7_size(i)*((osc_srv7_f(i) + 0.001) * log(esc_srv7_f(i) + 0.001)) ;
  for (i=1; i <= nyrs_srv7_size; i++)  age_like(14) -= nsamples_srv7_size(i)*((osc_srv7_m(i) + 0.001) * log(esc_srv7_m(i) + 0.001)) ;
  for (i=1; i <= nyrs_srv7_age; i++)  age_like(15) -= nsamples_srv7_age(i)*((oac_srv7(i) + 0.001) * log(eac_srv7(i) + 0.001)) ;
  for (i=1; i <= nyrs_srv2_age; i++)   age_like(16) -= nsamples_srv2_age(i)*((oac_srv2(i) + 0.001) * log(eac_srv2(i) + 0.001)) ;
  
  age_like   -= offset;                       // Subract offsets

  age_like(1) *= wt_fish1_age;    //1               // Multiple each likelihood by their weights from .ctl file
  age_like(2) *= wt_srv1_age;  //1
  age_like(3) *= wt_fish1_size;  //1
  age_like(4) *= wt_fish1_size;  //1
  age_like(5) *= wt_fish2_size; //
  age_like(6) *= wt_fish3_size;  //1
  age_like(7) *= wt_fish3_size;  //1
  age_like(8) *= wt_fish4_size; //0
  age_like(9) *= wt_srv1_size;  //1
  age_like(10) *= wt_srv1_size;  //1
  age_like(11) *= wt_srv2_size;  //1
  age_like(12) *= wt_srv2_size;  //1
  age_like(13) *= wt_srv7_size;  //1
  age_like(14) *= wt_srv7_size;  //1
  age_like(15) *= wt_srv7_age;  //1
  age_like(16) *= wt_srv1_age;  //1

FUNCTION Sel_Like
// Calculate penalty function for selectivity
  sel_like.initialize();
  if (active(log_srv1_sel_coffs_f) ) {

// Differences in selectivity between adjacent ages
  sel_like(1)   +=wt_sel_reg_fish1 * norm2(first_difference(first_difference(log_fish1_sel_f)));  // Constrains selectivities to be smooth
  sel_like(1)   +=wt_sel_reg_fish1 * norm2(first_difference(first_difference(log_fish1_sel_m)));  // Constrains selectivities to be smooth
  sel_like(2)   +=wt_sel_reg_fish2 * norm2(first_difference(first_difference(log_fish2_sel)));  // Constrains selectivities to be smooth
  sel_like(3)   +=wt_sel_reg_fish3 * norm2(first_difference(first_difference(log_fish3_sel_f)));  // Constrains selectivities to be smooth
  sel_like(4)   +=wt_sel_reg_fish4 * norm2(first_difference(first_difference(log_fish4_sel_f)));  // Constrains selectivities to be smooth
  sel_like(4)   +=wt_sel_reg_fish4 * norm2(first_difference(first_difference(log_fish4_sel_m)));  // Constrains selectivities to be smooth

  sel_like(5) +=wt_sel_reg_srv1 * norm2(first_difference(first_difference(log_srv1_sel_f)));  // Constrains selectivities to be smooth
  sel_like(6) +=wt_sel_reg_srv2 * norm2(first_difference(first_difference(log_srv2_sel_f)));  // Constrains selectivities to be smooth
  sel_like(5) +=wt_sel_reg_srv1 * norm2(first_difference(first_difference(log_srv1_sel_m)));  // Constrains selectivities to be smooth
  sel_like(6) +=wt_sel_reg_srv2 * norm2(first_difference(first_difference(log_srv2_sel_m)));  // Constrains selectivities to be smooth
  sel_like(7) +=wt_sel_reg_srv2 * norm2(first_difference(first_difference(log_srv7_sel_f)));  // Constrains selectivities to be smooth
  sel_like(8) +=wt_sel_reg_srv2 * norm2(first_difference(first_difference(log_srv7_sel_m)));  // Constrains selectivities to be smooth

// Differences in selectivity between adjacent ages when selectivity for first age is greater
//   Affects the degree of dome-shape

  for (j=1;j<nages;j++)
    if (log_fish1_sel_f(j)>log_fish1_sel_f(j+1))
      sel_like(9) += wt_sel_dome_fish1 *square(log_fish1_sel_f(j)-log_fish1_sel_f(j+1));  //Prevents dome-shapedness
    if (log_fish1_sel_m(j)>log_fish1_sel_m(j+1))
      sel_like(9) += wt_sel_dome_fish1 *square(log_fish1_sel_m(j)-log_fish1_sel_m(j+1));  //Prevents dome-shapedness
  for (j=1;j<nages;j++)
    if (log_fish2_sel(j)>log_fish2_sel(j+1))
      sel_like(10) += wt_sel_dome_fish2 *square(log_fish2_sel(j)-log_fish2_sel(j+1));  //Prevents dome-shapedness
  for (j=1;j<nages;j++)
    if (log_fish3_sel_f(j)>log_fish3_sel_f(j+1))
      sel_like(11) += wt_sel_dome_fish3 *square(log_fish3_sel_f(j)-log_fish3_sel_f(j+1));  //Prevents dome-shapedness
//  for (j=1;j<nages;j++)
//    if (log_fish4_sel(j)>log_fish4_sel(j+1))
//      sel_like(12) += wt_sel_dome_fish4 *square(log_fish4_sel(j)-log_fish4_sel(j+1));  //Prevents dome-shapedness


    for (j=1;j<nages;j++)
      if (log_srv1_sel_f(j)>log_srv1_sel_f(j+1))
        sel_like(13) +=wt_sel_dome_srv1 *square(log_srv1_sel_f(j)-log_srv1_sel_f(j+1));
      if (log_srv1_sel_m(j)>log_srv1_sel_m(j+1))
        sel_like(13) +=wt_sel_dome_srv1 *square(log_srv1_sel_m(j)-log_srv1_sel_m(j+1));
    for (j=1;j<nages;j++)
      if (log_srv2_sel_f(j)>log_srv2_sel_f(j+1))
        sel_like(14) +=wt_sel_dome_srv2 *square(log_srv2_sel_f(j)-log_srv2_sel_f(j+1));
      if (log_srv2_sel_m(j)>log_srv2_sel_m(j+1))
        sel_like(14) +=wt_sel_dome_srv2 *square(log_srv2_sel_m(j)-log_srv2_sel_m(j+1));
    for (j=1;j<nages;j++) {
      if (log_srv7_sel_f(j)>log_srv7_sel_f(j+1))
        sel_like(15) +=wt_sel_dome_srv2 *square(log_srv7_sel_f(j)-log_srv7_sel_f(j+1));
      if (log_srv7_sel_m(j)>log_srv7_sel_m(j+1))
        sel_like(15) +=wt_sel_dome_srv2 *square(log_srv7_sel_m(j)-log_srv7_sel_m(j+1)); }
//    for (j=1;j<nages;j++)
//      if (log_srv4_sel(j)>log_srv4_sel(j+1))
 //       sel_like(16) +=wt_sel_dome_srv4 *square(log_srv4_sel(j)-log_srv4_sel(j+1));
 }
FUNCTION double round(double r) 
    return double((r > 0.0) ? floor(r + 0.5) : ceil(r - 0.5)); 
FUNCTION Get_Population_Projection
//  Abundance at start of first projection year
// stdev of recvar
  int k;
  if(mceval_phase()) {
//     random_number_generator r(1000);
  stdev_rec = sqrt(norm2(value(log_rec_dev(1979,endyr-recage))-mean(value(log_rec_dev(1979,endyr-recage))))/(size_count(value(log_rec_dev(1979,endyr-recage)))-1));

   k=round(value(stdev_rec)*10000);

      N_proj_f(endyr+1,1)= mfexp(value(log(mean(value(pred_rec(1979,endyr-recage))))-square(stdev_rec)/2+stdev_rec*randn(k+l)))/2;
     N_proj_m(endyr+1,1)= mfexp(value(log(mean(value(pred_rec(1979,endyr-recage))))-square(stdev_rec)/2+stdev_rec*randn(k+l)))/2; }
  else {   N_proj_f(endyr+1,1)= mfexp(value(log(mean(pred_rec(1979,endyr-recage)))))/2;
      N_proj_m(endyr+1,1)= mfexp(value(log(mean(pred_rec(1979,endyr-recage)))))/2; }
    for (j=1; j<nages-1;j++) {
      k=k+j;
      N_proj_f(endyr+1,j+1)=natage_f(endyr,j)*value(S_f(endyr,j));
       N_proj_m(endyr+1,j+1)=natage_m(endyr,j)*value(S_m(endyr,j)); }
   N_proj_f(endyr+1,nages) = value(natage_f(endyr,nages-1))*value(S_f(endyr,nages-1))+ value(natage_f(endyr,nages))*value(S_f(endyr,nages));
   N_proj_m(endyr+1,nages) = value(natage_m(endyr,nages-1))*value(S_m(endyr,nages-1))+ value(natage_m(endyr,nages))*value(S_m(endyr,nages));

   spawn_biom_proj(endyr+1) =elem_prod(N_proj_f(endyr+1),pow(mfexp(-yieldratio*FABC_tot_proj_f-value(natmort)),spawn_fract)) * wt_mature;
    tot_biom_proj(endyr+1)=N_proj_f(endyr+1)*wt_f+N_proj_m(endyr+1)*wt_m;
  for (i=endyr+1;i<=endyr+15;i++)
  {
//  F ABC 
    if (spawn_biom_proj(i)/B40 > 1.) {
      FABC_proj = value(F40);
      FOFL_proj=value(F35); }
    else {
      FABC_proj = value(F40) * (spawn_biom_proj(i)/value(B40) - 0.05)/(1 - 0.05); 
    FOFL_proj = value(F35)*(spawn_biom_proj(i)/value(B40) - 0.05)/(1 - 0.05);  }

    for (j=1;j<=nages;j++)
    {  
      FABC_tot_proj_f(j) = fish4_sel_f(j)* FABC_proj * fratio + fish3_sel_f(j)* FABC_proj * (1-fratio);
      FABC_tot_proj_m(j) = fish4_sel_m(j)* FABC_proj * fratio + fish3_sel_m(j)* FABC_proj * (1-fratio);
      FOFL_tot_proj_f(j) = fish4_sel_f(j)* FOFL_proj * fratio + fish3_sel_f(j)* FOFL_proj * (1-fratio);
      FOFL_tot_proj_m(j) = fish4_sel_m(j)* FOFL_proj * fratio + fish3_sel_m(j)* FOFL_proj * (1-fratio);
      Z_proj_f(j)   = FABC_tot_proj_f(j)+ natmort;
      Z_proj_m(j)   = FABC_tot_proj_m(j)+ natmort;
      ZOFL_proj_f(j)   = FOFL_tot_proj_f(j)+ value(natmort);
      ZOFL_proj_m(j)   = FOFL_tot_proj_m(j)+ value(natmort);
      S_proj_f(j)   = mfexp(-1.0* Z_proj_f(j));
      S_proj_m(j)   = mfexp(-1.0* Z_proj_m(j));
    }

//  Catch 
    for (j=1;j<=nages;j++)
     { 
      catage_proj_f(i,j) = yieldratio*N_proj_f(i,j)* FABC_tot_proj_f(j)/Z_proj_f(j)*(1.-mfexp(-Z_proj_f(j)));
      catage_proj_m(i,j) = yieldratio*N_proj_m(i,j)* FABC_tot_proj_m(j)/Z_proj_m(j)*(1.-mfexp(-Z_proj_m(j)));
       catage_proj_OFL_f(i,j) = yieldratio*N_proj_f(i,j)* FOFL_tot_proj_f(j)/ZOFL_proj_f(j)*(1.-mfexp(-ZOFL_proj_f(j)));
      catage_proj_OFL_m(i,j) = yieldratio*N_proj_m(i,j)* FOFL_tot_proj_m(j)/ZOFL_proj_m(j)*(1.-mfexp(-ZOFL_proj_m(j)));
          }
    pred_catch_proj(i)     = (catage_proj_f(i)*wt_f+catage_proj_m(i)*wt_m)/yieldratio;
    pred_catch_proj_OFL(i)     =  (catage_proj_OFL_f(i)*wt_f+catage_proj_OFL_m(i)*wt_m)/yieldratio;

//  Next year's abundance
    if (i < endyr+15)
    {
 if(mceval_phase()) {
   stdev_rec = sqrt(norm2(value(log_rec_dev(1979,endyr-recage))-mean(value(log_rec_dev(1979,endyr-recage))))/(size_count(value(log_rec_dev(1979,endyr-recage)))-1));
     k=round(value(spawn_biom(endyr)*10000))+i;

  k=k+i;
      N_proj_f(i+1,1)= mfexp(value(log(mean(value(pred_rec(1979,endyr-recage))))-square(stdev_rec)/2+stdev_rec*randn(k+l)))/2;
     N_proj_m(i+1,1)= mfexp(value(log(mean(value(pred_rec(1979,endyr-recage))))-square(stdev_rec)/2+stdev_rec*randn(k+l)))/2; }
    else {  N_proj_f(i+1,1)= mfexp(value(log(mean(pred_rec(1979,endyr-recage)))))/2;
          N_proj_m(i+1,1)= mfexp(value(log(mean(pred_rec(1979,endyr-recage)))))/2; }

      for (j=1; j<nages-1;j++) {
        N_proj_f(i+1,j+1) = N_proj_f(i,j)  * mfexp(-yieldratio*FABC_tot_proj_f(j)-value(natmort));;
        N_proj_m(i+1,j+1) = N_proj_m(i,j)  * mfexp(-yieldratio*FABC_tot_proj_m(j)-value(natmort)); }
      N_proj_f(i+1,nages) = N_proj_f(i,nages-1)* mfexp(-yieldratio*FABC_tot_proj_f(nages-1)-value(natmort))+ N_proj_f(i,nages)   * mfexp(-yieldratio*FABC_tot_proj_f(nages)-value(natmort));
      N_proj_m(i+1,nages) = N_proj_m(i,nages-1)* mfexp(-yieldratio*FABC_tot_proj_m(nages-1)-value(natmort))+ N_proj_m(i,nages)   * mfexp(-yieldratio*FABC_tot_proj_m(nages)-value(natmort));

       spawn_biom_proj(i+1)        = elem_prod(N_proj_f(i+1),pow(mfexp(-yieldratio*FABC_tot_proj_f-value(natmort)),spawn_fract)) * wt_mature;  // Right way
       tot_biom_proj(i+1)=N_proj_f(i+1)*wt_f+N_proj_m(i+1)*wt_m;

        }
 }

     if (spawn_biom_proj(endyr+1)/B40 > 1.) {
      FABC = value(F40);
      FOFL = value(F35); 
      FABC2 = value(F40);
      FOFL2 = value(F35); }
    else {
      FABC = value(F40) * (spawn_biom_proj(endyr+1)/value(B40) - 0.05)/(1 - 0.05); 
    FOFL = value(F35)*(spawn_biom_proj(endyr+1)/value(B40) - 0.05)/(1 - 0.05);  
      FABC2 = value(F40) * (spawn_biom_proj(endyr+2)/value(B40) - 0.05)/(1 - 0.05); 
    FOFL2 = value(F35)*(spawn_biom_proj(endyr+2)/value(B40) - 0.05)/(1 - 0.05);  }
     OFL=pred_catch_proj_OFL(endyr+1);
     ABC=pred_catch_proj(endyr+1);

FUNCTION Evaluate_Objective_Function 
  obj_fun.initialize();
  ssqcatch.initialize();
  rec_like.initialize();
  F_mort_regularity.initialize();
  avg_sel_penalty.initialize();
  Surv_Likelihood();                                  // Likelihood function for survey biomass
  ssqcatch  +=  wt_ssqcatch_fish1 *norm2(log(obs_catch_fish1+0.01)-log(pred_catch_fish1+0.01));
  ssqcatch  +=  wt_ssqcatch_fish3 *norm2(log(obs_catch_fish3+0.8)-log(pred_catch_fish3+0.8));

  switch (SrType)
  {
    case 3:
    {
      if (rec_like_type==2)
//       rec_like      = wt_rec_var * norm2(log_rec_dev)/(2*square(sigr)) + (size_count(log_rec_dev)*log(sigr));
          rec_like      = wt_rec_var*(norm2(log_rec_dev+sigr*sigr/2.)/(2.*square(sigr)) + (size_count(log_rec_dev))*log(sigr));
      else
        rec_like = wt_rec_var*(norm2(log_rec_dev));
        break;
    }
    default: 
    {
        dvar_vector stmp(styr_rec,endyr);
        for (i=styr_rec;i<=endyr;i++)
        stmp(i) = Sp_Biom(i-recage);
        srm_rec   = SRecruit(stmp);
        dvar_vector   chi(styr_rec_est,endyr_rec_est);
        chi         = log(elem_div(sam_rec(styr_rec_est,endyr_rec_est) , srm_rec(styr_rec_est,endyr_rec_est)));
        dvariable SSQRec = norm2( chi + sigrsq/2.) ;
        rec_like    = .5*SSQRec/sigrsq + nrecs_est*log(sigr); 
        rec_like   += .5*norm2( log_rec_dev(styr_rec,styr_rec_est) )/sigrsq + (styr_rec_est-styr_rec)*log(sigr) ; 
      if (endyr>endyr_rec_est)
        rec_like += .5*norm2( log_rec_dev(endyr_rec_est,endyr  ) )/sigrsq + (endyr-endyr_rec_est)  *log(sigr) ; 
      break;
    }
  }
      F_mort_regularity  = wt_fmort_reg * norm2(log_F_devs_fish1);// Penalty function for fishing mortality deviations
      F_mort_regularity  += wt_fmort_reg * norm2(log_F_devs_fish3);// Penalty function for fishing mortality deviations
    Multinomial_Likelihood();                           // Multinomial likelihood

// Sum objective function
  obj_fun           += ssqcatch ;
  obj_fun           += sum(surv_like);
  obj_fun           += sum(age_like);
  Like = obj_fun;                     // Put here to capture the data likelihood
  obj_fun           += rec_like;
  if(active(log_F_devs_fish1))                          // Penalty function for fishing mortality deviations
    obj_fun         += F_mort_regularity;
    obj_fun           += sum(priors);               //Add priors
  if (active(mF50)&&last_phase()) 
    obj_fun         += sprpen;                    // To solve for the F40 etc.     
  if (current_phase()<3) obj_fun         += 10*(norm2(log_F_devs_fish1)+norm2(log_F_devs_fish3));         //(was-0.3) Penalty early on to scale population...                
    cout<<"monitoring SSB "<<sum(elem_prod(natage_f(endyr),wt_mature))<<endl;
GLOBALS_SECTION
 # include "admodel.h"                      // Include AD class definitions
 # include "mhp-s-funcs.cpp"                // Include S-compatible output functions (needs preceding)
  adstring model_name;
  adstring data_file;

REPORT_SECTION
// Beginning of all outputting
// Goes to routine that automatically creates input file for projection model
  if (last_phase()) {
    write_projout();
    write_newproj();
    write_sarareport();
    }
// Output file (tem.rep) which is loaded into tem.xls to display output
  
  report<<"****Executive mary Material*****"<<endl;
  report<<"     Model name"     <<endl;
  report<<model_name<<endl;
  report<<"     .dat file"     <<endl;
  report<<data_file<<endl;
  report<<"     Number parameters estimated"     <<endl;
  report<<initial_params::nvarcalc()<<endl;
  report<<"     TotalBiomass for "<<endyr+1<<endl;
  report<<tot_biom_proj(endyr+1)<<endl;
  report<<"     TotalBiomass for "<<endyr+2     <<endl;
  report<<tot_biom_proj(endyr+2)<<endl;
  report<<"     Female_Spawning Biomass for "<<endyr+1     <<endl;
  report<<spawn_biom_proj(endyr+1)<<endl;
  report<<"     Female_Spawning_Biomass for "<<endyr+2     <<endl;
  report<<spawn_biom_proj(endyr+2)<<endl;
  report<<"     B_100"     <<endl;
  report<<0.5*SB0*mean(pred_rec(1979,endyr-recage))<<endl;
  report<<"     B_40"     <<endl;
  report<<B40<<endl;
  report<<"     B_35"     <<endl;
  report<<0.5*SBF35*mean(pred_rec(1979,endyr-recage))<<endl;
  report<<"     F_40"     <<endl;
  report<<F40<<endl;
  report<<"     F_35"     <<endl;
  report<<F35<<endl;
  report<<"     F_ABC for "<<endyr+1     <<endl;
  report<<FABC<<endl;
  report<<"     F_ABC for "<<endyr+2     <<endl;
  report<<FABC2<<endl;
  report<<"     ABC for "<<endyr+1     <<endl;
  report<<pred_catch_proj(endyr+1)<<endl;
  report<<"     ABC for "<<endyr+2     <<endl;
  report<<pred_catch_proj(endyr+2)<<endl;
  report<<"     F_OFL for "<<endyr+1     <<endl;
  report<<FOFL<<endl;
  report<<"     F_OFL for "<<endyr+2     <<endl;
  report<<FOFL2<<endl;
  report<<"     OFL for "<<endyr+1     <<endl;
  report<<OFL<<endl; 
  report<<"     OFL for "<<endyr+2     <<endl;
  report<<pred_catch_proj_OFL(endyr+2)<<endl; 
  report<<"     Total likelihood"     <<endl;
  report<<obj_fun<<endl;
  report<<"     Data likelihood"     <<endl;
  report<<Like<<endl<<endl;
  
  report<<" ************   Some more parameter estimates and their SDs ************"<<endl;
 
  if(last_phase()) {
    // add standard deviation data types    
  report<<"   q_domestic   "<<endl;
  report<<q_srv1<<" "<<q_srv1.sd<<endl;
  report<<"   q_cooperative  "<<endl;
   report<<q_srv2<<" "<<q_srv2.sd<<endl;
  report<<"   q_trawl  "<<endl;
   report<<q_srv7<<" "<<q_srv7.sd<<endl;
 /*
  report<<natmort<<" "<<nattymort.sd<<endl;
  report<<"  sigr   "<<endl;  
  report<<sigr<<" "<<cigar.sd<<endl;  
  report<<"   log_mean_rec"<<endl;
  report<<log_mean_rec<<" "<<LMR.sd<<endl;
 */
  report<<"   F_40"<<endl;
  report<<F40<<" "<<F40.sd<<endl;
  report<<"    tot_biom"<<endl;
  report<<tot_biom_proj(endyr+1)<<" "<<tot_biom_proj.sd(endyr+1)<<endl;
  report<<"   spawn_biom"<<endl;
  report<<spawn_biom_proj(endyr+1)<<" "<<spawn_biom_proj.sd(endyr+1)<<endl;
  report<<"    B40"<<endl;
  report<<B40<<" "<<B40.sd<<endl;
  report<<"   ABC"<<endl;
  report<<pred_catch_proj(endyr+1)<<" "<<pred_catch_proj.sd(endyr+1)<<endl<<endl;
 
 }
  report<<model_name<<endl;
  report<<data_file<<endl;
  report<<"Num_parameters_Estimated "<<initial_params::nvarcalc()<<endl;
  report << "Year "<< yy <<endl;
  report << "Pred_Catch Fixed Geaar "<< pred_catch_fish1<<endl<<"Pred catch trawl "<<pred_catch_fish3 <<endl;
  report << "Obs_Catch Fixed Gear "<< obs_catch_fish1<<endl<<" Obs_Catch Trawl "<<obs_catch_fish3 <<endl;

  report << "Survival Female"<<aa <<endl;
  for (i=styr;i<=endyr;i++) report << i<<" "<<S_f(i) <<endl; report<<endl;
  report << "Survival Male"<<aa <<endl;
  for (i=styr;i<=endyr;i++) report << i<<" "<<S_m(i) <<endl; report<<endl;


  report << "Numbers Females "<<aa <<endl;
  for (i=styr;i<=endyr;i++) report << i<<" "<<natage_f(i) <<endl; report<<endl;
  report << "Numbers Males"<<aa <<endl;
  for (i=styr;i<=endyr;i++) report << i<<" "<<natage_m(i) <<endl; report<<endl;

  report << "Age "<<aa <<endl;
  report << "Fishery_sel1 Females"<<fish1_sel_f  <<endl;
  report << "Fishery_sel1 Males"<<fish1_sel_m  <<endl;
  report << "Fishery_sel2 "<<fish2_sel  <<endl;
  report << "Fishery_sel3 "<<fish3_sel_f  <<endl;
  report << "Fishery_sel4_f "<<fish4_sel_f  <<endl;
  report << "Fishery_sel4_m "<<fish4_sel_m  <<endl;
  report << "Survey_sel1 Female"<<srv1_sel_f  <<endl<<endl;
  report << "Survey_sel1 male"<<srv1_sel_m  <<endl<<endl;
  report << "Survey_sel2 Female"<<srv2_sel_f  <<endl<<endl;
  report << "Survey_sel2 male"<<srv2_sel_m  <<endl<<endl;
  report << "Survey_sel7 Female"<<srv7_sel_f  <<endl<<endl;
  report << "Survey_sel7 male"<<srv7_sel_m  <<endl<<endl;

  sdnr_fish1_age = 0;
  sdnr_fish1_size = 0;
  sdnr_fish3_size = 0;
  sdnr_srv1_age = 0;
  sdnr_srv2_age = 0;
  sdnr_srv1_size = 0;
  sdnr_srv2_size = 0;
  sdnr_srv7_size = 0;


  report << "Obs_P_fish_age"<<aa <<endl;
  for (i=1;i<=nyrs_fish1_age;i++) {
      sdnr_fish1_age +=sdnr(eac_fish1(i),oac_fish1(i),wt_fish1_age*double(nsamples_fish1_age(i)))/nyrs_fish1_age;
      report << yrs_fish1_age(i)<<" "<<oac_fish1(i) 
      <<" eff_N "<<(1-eac_fish1(i))*eac_fish1(i)/norm2(oac_fish1(i)-eac_fish1(i))  <<" N "<<nsamples_fish1_age(i)
      <<" SDNR "<< sdnr(eac_fish1(i),oac_fish1(i),wt_fish1_age*double(nsamples_fish1_age(i)))<<endl; report<<endl; }
  report << "Pred_P_fish1_age"<<aa <<endl;
  for (i=1;i<=nyrs_fish1_age;i++) report << yrs_fish1_age(i)<<" "<<eac_fish1(i) <<endl; report<<endl;

  report << "Obs_P_fish1_size Female"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_fish1_size;i++)  {
   sdnr_fish1_size += sdnr(esc_fish1_f(i),osc_fish1_f(i),wt_fish1_size*double(nsamples_fish1_size(i)))/nyrs_fish1_size/2;
  report << yrs_fish1_size(i)<<" "<<osc_fish1_f(i) 
      <<" eff_N "<<(1-esc_fish1_f(i))*esc_fish1_f(i)/norm2(osc_fish1_f(i)-esc_fish1_f(i))  <<" N "<<nsamples_fish1_size(i)
      <<" SDNR "<< sdnr(esc_fish1_f(i),osc_fish1_f(i),wt_fish1_size*double(nsamples_fish1_size(i)))<<endl; report<<endl; }
  report << "Pred_P_fish1_size"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_fish1_size;i++) report << yrs_fish1_size(i)<<" "<<esc_fish1_f(i) <<endl; report<<endl;

    report << "Obs_P_fish1_size Male"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_fish1_size;i++) {
    sdnr_fish1_size += sdnr(esc_fish1_m(i),osc_fish1_m(i),wt_fish1_size*double(nsamples_fish1_size(i)))/nyrs_fish1_size/2;
  report << yrs_fish1_size(i)<<" "<<osc_fish1_m(i) 
      <<" eff_N "<<(1-esc_fish1_m(i))*esc_fish1_m(i)/norm2(osc_fish1_m(i)-esc_fish1_m(i))  <<" N "<<nsamples_fish1_size(i)
      <<" SDNR "<< sdnr(esc_fish1_m(i),osc_fish1_m(i),wt_fish1_size*double(nsamples_fish1_size(i)))<<endl; report<<endl; }
  report << "Pred_P_fish1_size"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_fish1_size;i++) report << yrs_fish1_size(i)<<" "<<esc_fish1_m(i) <<endl; report<<endl;

   report << "Obs_P_fish3_size Males"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_fish3_size;i++) {
    sdnr_fish3_size+= sdnr(esc_fish3_m(i),osc_fish3_m(i),wt_fish3_size*double(nsamples_fish3_size(i)))/nyrs_fish3_size/2;
  report << yrs_fish3_size(i)<<" "<<osc_fish3_m(i) 
      <<" eff_N "<<(1-esc_fish3_m(i))*esc_fish3_m(i)/norm2(osc_fish3_m(i)-esc_fish3_m(i))  <<" N "<<nsamples_fish3_size(i)
      <<" SDNR "<< sdnr(esc_fish3_m(i),osc_fish3_m(i),wt_fish3_size*double(nsamples_fish3_size(i)))<<endl; report<<endl; }
  report << "Pred_P_fish3_size"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_fish3_size;i++) report << yrs_fish3_size(i)<<" "<<esc_fish3_m(i) <<endl; report<<endl;

  report << "Obs_P_fish3_size Female"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_fish3_size;i++)  {
     sdnr_fish3_size+= sdnr(esc_fish3_f(i),osc_fish3_f(i),wt_fish3_size*double(nsamples_fish3_size(i)))/nyrs_fish3_size/2;
  report << yrs_fish3_size(i)<<" "<<osc_fish3_f(i) 
      <<" eff_N "<<(1-esc_fish3_f(i))*esc_fish3_f(i)/norm2(osc_fish3_f(i)-esc_fish3_f(i))  <<" N "<<nsamples_fish3_size(i)
      <<" SDNR "<< sdnr(esc_fish3_f(i),osc_fish3_f(i),wt_fish3_size*double(nsamples_fish3_size(i)))<<endl; report<<endl; }
  report << "Pred_P_fish3_size"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_fish3_size;i++) report << yrs_fish3_size(i)<<" "<<esc_fish3_f(i) <<endl; report<<endl;

  report << "Obs_P_srv1_age"<<aa <<endl;
  for (i=1;i<=nyrs_srv1_age;i++) {
   sdnr_srv1_age+=sdnr(eac_srv1(i),oac_srv1(i),wt_srv1_age*double(nsamples_srv1_age(i)))/nyrs_srv1_age;
   report << yrs_srv1_age(i)<<" "<<oac_srv1(i) 
      <<" eff_N "<<(1-eac_srv1(i))*eac_srv1(i)/norm2(oac_srv1(i)-eac_srv1(i)) <<" N "<<nsamples_srv1_age(i)
      <<" SDNR "<< sdnr(eac_srv1(i),oac_srv1(i),wt_srv1_age*double(nsamples_srv1_age(i)))<<endl; report<<endl; }
  report << "Pred_P_srv1_age"<<aa <<endl;
  for (i=1;i<=nyrs_srv1_age;i++) report << yrs_srv1_age(i)<<" "<<eac_srv1(i) <<endl; report<<endl;

     report << "Obs_P_srv2_age"<<aa <<endl;
  for (i=1;i<=nyrs_srv2_age;i++) {
    sdnr_srv2_age+=sdnr(eac_srv2(i),oac_srv2(i),wt_srv1_age*double(nsamples_srv2_age(i)))/nyrs_srv2_age;
    report << yrs_srv2_age(i)<<" "<<oac_srv2(i) 
      <<" eff_N "<<(1-eac_srv2(i))*eac_srv2(i)/norm2(oac_srv2(i)-eac_srv2(i)) <<" N "<<nsamples_srv2_age(i)
      <<" SDNR "<< sdnr(eac_srv2(i),oac_srv2(i),wt_srv1_age*double(nsamples_srv2_age(i)))<<endl; report<<endl; }
  report << "Pred_P_srv2_age"<<aa <<endl;
  for (i=1;i<=nyrs_srv2_age;i++) report << yrs_srv2_age(i)<<" "<<eac_srv2(i) <<endl; report<<endl;

  report << "Obs_P_srv1_size"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_srv1_size;i++) {
     sdnr_srv1_size+=sdnr(esc_srv1_f(i),osc_srv1_f(i),wt_srv1_size*double(nsamples_srv1_size(i)))/nyrs_srv1_size/2;
    report << yrs_srv1_size(i)<<" "<<osc_srv1_f(i) 
      <<" eff_N "<<(1-esc_srv1_f(i))*esc_srv1_f(i)/norm2(osc_srv1_f(i)-esc_srv1_f(i)) <<" N "<<nsamples_srv1_size(i)
      <<" SDNR "<< sdnr(esc_srv1_f(i),osc_srv1_f(i),wt_srv1_size*double(nsamples_srv1_size(i)))<<endl; report<<endl; }
  report << "Pred_P_srv1_size"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_srv1_size;i++) report << yrs_srv1_size(i)<<" "<<esc_srv1_f(i) <<endl; report<<endl;

    report << "Obs_P_srv1_size Males"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_srv1_size;i++) {
    sdnr_srv1_size+=sdnr(esc_srv1_m(i),osc_srv1_m(i),wt_srv1_size*double(nsamples_srv1_size(i)))/nyrs_srv1_size/2;
     report << yrs_srv1_size(i)<<" "<<osc_srv1_m(i) 
      <<" eff_N "<<(1-esc_srv1_m(i))*esc_srv1_m(i)/norm2(osc_srv1_m(i)-esc_srv1_m(i)) <<" N "<<nsamples_srv1_size(i)
      <<" SDNR "<< sdnr(esc_srv1_m(i),osc_srv1_m(i),wt_srv1_size*double(nsamples_srv1_size(i)))<<endl; report<<endl; }
  report << "Pred_P_srv1_size"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_srv1_size;i++) report << yrs_srv1_size(i)<<" "<<esc_srv1_m(i) <<endl; report<<endl;

  report << "Obs_P_srv2_size"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_srv2_size;i++) {
    sdnr_srv2_size+=sdnr(esc_srv2_f(i),osc_srv2_f(i),wt_srv2_size*double(nsamples_srv2_size(i)))/nyrs_srv2_size/2;
     report << yrs_srv2_size(i)<<" "<<osc_srv2_f(i) 
      <<" eff_N "<<(1-esc_srv2_f(i))*esc_srv2_f(i)/norm2(osc_srv2_f(i)-esc_srv2_f(i)) <<" N "<<nsamples_srv2_size(i)
      <<" SDNR "<< sdnr(esc_srv2_f(i),osc_srv2_f(i),wt_srv2_size*double(nsamples_srv2_size(i)))<<endl; report<<endl; }
  report << "Pred_P_srv2_size"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_srv2_size;i++) report << yrs_srv2_size(i)<<" "<<esc_srv2_f(i) <<endl; report<<endl;

    report << "Obs_P_srv2_size Males"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_srv2_size;i++) {
        sdnr_srv2_size+=sdnr(esc_srv2_m(i),osc_srv2_m(i),wt_srv2_size*double(nsamples_srv2_size(i)))/nyrs_srv2_size/2;
 report << yrs_srv2_size(i)<<" "<<osc_srv2_m(i) 
      <<" eff_N "<<(1-esc_srv2_m(i))*esc_srv2_m(i)/norm2(osc_srv2_m(i)-esc_srv2_m(i)) <<" N "<<nsamples_srv2_size(i)
      <<" SDNR "<< sdnr(esc_srv2_m(i),osc_srv2_m(i),wt_srv2_size*double(nsamples_srv2_size(i)))<<endl; report<<endl; }
  report << "Pred_P_srv2_size Males"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_srv2_size;i++) report << yrs_srv2_size(i)<<" "<<esc_srv2_m(i) <<endl; report<<endl;
  
    report << "Obs_P_srv7_size"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_srv7_size;i++) {
    sdnr_srv7_size+= sdnr(esc_srv7_f(i),osc_srv7_f(i),wt_srv7_size*double(nsamples_srv7_size(i)))/nyrs_srv7_size/2;
     report << yrs_srv7_size(i)<<" "<<osc_srv7_f(i) 
      <<" eff_N "<<(1-esc_srv7_f(i))*esc_srv7_f(i)/norm2(osc_srv7_f(i)-esc_srv7_f(i)) <<" N "<<nsamples_srv7_size(i)
      <<" SDNR "<< sdnr(esc_srv7_f(i),osc_srv7_f(i),wt_srv7_size*double(nsamples_srv7_size(i)))<<endl; report<<endl; }
  report << "Pred_P_srv7_size"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_srv7_size;i++) report << yrs_srv7_size(i)<<" "<<esc_srv7_f(i) <<endl; report<<endl;

    report << "Obs_P_srv7_size Males"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_srv7_size;i++) {
        sdnr_srv7_size+= sdnr(esc_srv7_m(i),osc_srv7_m(i),wt_srv7_size*double(nsamples_srv7_size(i)))/nyrs_srv7_size/2;
  report << yrs_srv7_size(i)<<" "<<osc_srv7_m(i) 
      <<" eff_N "<<(1-esc_srv7_m(i))*esc_srv7_m(i)/norm2(osc_srv7_m(i)-esc_srv7_m(i)) <<" N "<<nsamples_srv7_size(i)
      <<" SDNR "<< sdnr(esc_srv7_m(i),osc_srv7_m(i),wt_srv7_size*double(nsamples_srv7_size(i)))<<endl; report<<endl; }
  report << "Pred_P_srv7_size Males"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_srv7_size;i++) report << yrs_srv7_size(i)<<" "<<esc_srv7_m(i) <<endl; report<<endl;

  report << "Survey Biomass " <<endl;
  report << "Year:     " << yrs_srv1  <<endl;
  report << "Predicted:   " << pred_srv1  <<endl;
  report << "Observed:   " << obs_srv1_biom  <<endl<< endl;

  report << "Weight "<<aa<< endl;
  report << " "<< wt_f << endl;

  report << "Fully_selected_F "<<aa <<endl;
  report << " "<< Fmort_fish1*max(fish1_sel_f)+Fmort_fish3 <<endl;

  report << "Year " << yy<< endl;
  report << "SpBiom "<< spawn_biom <<endl;
  report << "Tot_biom "<< tot_biom   <<endl;
    // report << tot_biom.sd <<endl;

  report << "Fishery Selectivity " <<endl;
  report << fish1_sel_f / max(fish1_sel_f) <<endl;
  report << "F35 F40 F50 "<<endl;
  report <<  F35 << " "<< mF40 <<" "<<  F50 <<endl <<endl <<endl <<endl <<endl <<endl;
  report << "SSB projection: "<< spawn_biom_proj<<endl<<"Catch projection: "<<pred_catch_proj<<endl;
  report <<  "B40: "<< B40<< endl;
  report << "Wts_n_Likelihoods  " << endl;
  report << wt_ssqcatch_fish1 <<" "<<ssqcatch <<" " ; report << "SSQ_Catch_Likelihood" << endl;
  report << wt_srv3     <<" "<<surv_like(3)  <<" " ; report << "Domestic_Survey_Abundance_Index_Likelihood" << endl;
  report << wt_srv4     <<" "<<surv_like(4)  <<" " ; report << "Cooperative_Survey_Abundance_Index_Likelihood" << endl;
  report << wt_fish1_age <<" "<<age_like(1)  <<" " ; report << "Fishery_Age_Composition_Likelihood" << endl;
  report << wt_srv1_age <<" "<<age_like(2)  <<" " ; report << "Survey_Age_Composition_Likelihood_DomesticLL" << endl;
  report << wt_srv1_age <<" "<<age_like(16)  <<" " ; report << "Survey_Age_Composition_Likelihood_CooperativeLL" << endl;
  report << wt_fish1_size<<" "<<age_like(3)+age_like(4)  <<" " ; report << "Fishery_Size_Composition_Likelihood_Fixed" << endl;
  report << wt_fish3_size<<" "<<age_like(6)+age_like(7)  <<" " ; report << "Fishery_Size_Composition_Likelihood_Trawl" << endl;
  report << wt_srv1_size<<" "<<age_like(9)+age_like(10)  <<" " ; report << "Survey_Size_Composition_Likelihood_Domestic LL" << endl;
  report << wt_srv2_size<<" "<<age_like(11)+age_like(12)  <<" " ; report << "Survey_Size_Composition_Likelihood_Cooperative_LL" << endl;
  report << wt_srv7_size<<" "<<age_like(13)+age_like(14)  <<" " ; report << "Survey_Size_Composition_Likelihood_GOATrawl" << endl;
  report << wt_rec_var <<" "<<rec_like     <<" " ; report << "Recruitment_Deviations_Likelihood" << endl;

  report << wt_sel_reg_fish1 <<" "<<sel_like(1)      <<" " ; report << "Fish_sel_Regularity_Penalty  "<<endl  ;
  report << wt_sel_reg_srv1 <<" "<<sel_like(2)      <<" " ; report << "Surv_sel_Regularity_Penalty  "<<endl  ;
  report << wt_sel_dome_fish1<<" "<<sel_like(3)      <<" " ; report << "Fish_Sel_Domeshapedness_Penalty "<<endl  ;
  report << wt_sel_dome_srv1<<" "<<sel_like(4)      <<" " ; report << "Surv_Sel_Domeshapedness_Penalty "<<endl  ;
  report << "0"   <<" "<<avg_sel_penalty  <<" " ; report << "Average_Selectivity_Condition_Penalty "<<endl  ;
  report << wt_fmort_reg     <<" "<<F_mort_regularity<<" " ; report << "Fishing_Mortality_Regularity_Penalty" << endl;
 // report << wt_ssqcatch     <<" "<<F_dev_penalty<<" " ; report << "Fishing_Mortality_Deviations_Penalty" << endl;
  report << " "<<priors(1)  <<" " ; report << "priors sigr"     <<endl;
  report << " "<<priors(2)  <<" " ; report << "priors q_1" <<endl;
   report << " "<<priors(5)  <<" " ; report << "priors q_2" <<endl;
  report << " "<<priors(6)  <<" " ; report << "priors q_3" <<endl;
  report << " "<<priors(7)  <<" " ; report << "priors q_4" <<endl;
  report << " "<<priors(8)  <<" " ; report << "priors q_5" <<endl;
  report << " "<<priors(9)  <<" " ; report << "priors q_6" <<endl;
  report << " "<<priors(10)  <<" " ; report << "priors q_7" <<endl;
  report << " "<<priors(10)  <<" " ; report << "priors q_8" <<endl;
  report << " "<<priors(4)  <<" " ; report << "priors M"<<endl;
  report << " "<<obj_fun    <<" " ; report << "obj_fun"         <<endl;
  report << " "<<Like       <<" " ; report << "data likelihood" <<endl;//(2*square(sigr))+ size_count(log_rec_dev)*log(sigr)<<endl;
 if(last_phase()) { 
  report <<" SDNR1 "<< wt_srv1*std_dev(elem_div((pred_srv1(yrs_srv1)-obs_srv1_biom),obs_srv1_se))<<endl;
  report <<" SDNR2 "<< wt_srv2*std_dev(elem_div((pred_srv2(yrs_srv2)-obs_srv2_biom),obs_srv2_se))<<endl;
  report <<" SDNR3 "<< wt_srv3*std_dev(elem_div((pred_srv3(yrs_srv3)-obs_srv3_biom),obs_srv3_se))<<endl;
  report <<" SDNR4 "<< wt_srv4*std_dev(elem_div((pred_srv4(yrs_srv4)-obs_srv4_biom),obs_srv4_se))<<endl;
  report <<" SDNR5 "<< wt_srv5*std_dev(elem_div((pred_srv5(yrs_srv5)-obs_srv5_biom),obs_srv5_se))<<endl;
  report <<" SDNR6 "<< wt_srv6*std_dev(elem_div((pred_srv6(yrs_srv6)-obs_srv6_biom),obs_srv6_se))<<endl;
  report <<" SDNR7 "<< wt_srv7*std_dev(elem_div((pred_srv7(yrs_srv7)-obs_srv7_biom),obs_srv7_se))<<endl;
//  report <<" SDNR8 "<< wt_srv8*std_dev(elem_div((pred_srv8(yrs_srv8)-obs_srv8_biom),obs_srv8_se))<<endl;
 

    }
  report << "SigmaR: "<<sigr<< " Nat_Mort: "<<natmort<<" Male delta M "<<mdelta<<" Spawning Per Recruit "<< " "<<SBF40<<" "<<SB0<<" Virgin SPR "<<endl;
  report << "Stock-recruitment, type: "<<SrType<<" 1=Ricker, 2=B-Holt, 3=Mean"<<endl;
  report << "Year SSB SR_Pred R_Est "<<endl;
  report << "Weighted likelihods for comps broken down"<<endl;
  report << "Fish1 Age, Survey 1 Age, Fish 1 Size Female, Fish 1 Size Male, Fish 2 Size, Fish3 Size Female, F3 Size Male, Srv1 Size F, Srv1 Size M, Srv2 Size F, Srv2 Size M, Srv7 Size F, Srv7 Size M"<<endl;
  report <<age_like<<" age _like"<<endl<<surv_like<<" surv like "<<endl<<sel_like<<" sel_like"<<endl;
  report << "Unweighted likelihods for comps broken down"<<endl;
  report << "Fish1 Age, Survey 1 Age, Fish 1 Size Female, Fish 1 Size Male, Fish 2 Size, Fish3 Size Female, F3 Size Male, Srv1 Size F, Srv1 Size M, Srv2 Size F, Srv2 Size M, Srv7 Size F, Srv7 Size M"<<endl;
  report <<age_like(1) / (0.00001+wt_fish1_age)<< " "<<(age_like(2)+0.0001) / (0.00001+wt_srv1_age)<< " "<<(age_like(3)+0.0001) / (0.00001+wt_fish1_size)<< " "<<(age_like(4)+0.0001) / (0.00001+wt_fish1_size)<< " "<<(age_like(5)+0.0001) / (0.00001+wt_fish2_size)<< " "<<(age_like(6)+0.0001) / (0.00001+wt_fish3_size)<< " "<<(age_like(7)+0.0001) / (0.00001+wt_fish3_size)<< " "<<(age_like(8)+0.0001) / (0.00001+wt_fish4_size)<< " "<<(age_like(9)+0.0001) / (0.00001+wt_srv1_size)<< " "<<(age_like(10)+0.0001) / (0.00001+wt_srv1_size)<< " "<<(age_like(11)+0.0001) / (0.00001+wt_srv2_size)<< " "<<(age_like(12)+0.0001) / (0.00001+wt_srv2_size)<< " "<<(age_like(13)+0.0001) / (0.00001+wt_srv7_size)<< " "<<(age_like(14)+0.0001) / (0.00001+wt_srv7_size)<<" "<<(age_like(15)+0.0001) / (0.00001+wt_srv7_age)<< " "<<(age_like(16)+0.0001) / (0.00001+wt_srv1_age) <<" unw_age)_like"<<endl;
  report <<surv_like(1) / (0.00001+wt_srv1)<< " "<<surv_like(2) / (0.00001+wt_srv2)<< " "<<surv_like(3) / (0.00001+wt_srv3)<< " "<<surv_like(4) / (0.00001+wt_srv4)<< " "<<surv_like(5) / (0.00001+wt_srv5)<< " "<<surv_like(6) / (0.00001+wt_srv6)<< " "<<(surv_like(7)+0.0001) / (0.00001+wt_srv7)<< " "<<(surv_like(8)+0.0001) / (0.00001+wt_srv8)<< " "<<"unw_surv_like "<<endl<<sel_like<<" sel_like"<<endl;
  report << "Survey Biomass 2" <<endl;
  report << "Year:     " << yrs_srv2  <<endl;
  report << "Predicted:   " << pred_srv2  <<endl;
  report << "Observed:   " << obs_srv2_biom  <<endl<< endl;
  report << "Survey Biomass 3" <<endl;
  report << "Year:     " << yrs_srv3  <<endl;
  report << "Predicted:   " << pred_srv3  <<endl;
  report << "Observed:   " << obs_srv3_biom  <<endl<< endl;

  report << "Survey Biomass 4" <<endl;
  report << "Year:     " << yrs_srv4  <<endl;
  report << "Predicted:   " << pred_srv4  <<endl;
  report << "Observed:   " << obs_srv4_biom  <<endl<< endl;

  report << "Survey Biomass 5" <<endl;
  report << "Year:     " << yrs_srv5  <<endl;
  report << "Predicted:   " << pred_srv5  <<endl;
  report << "Observed:   " << obs_srv5_biom  <<endl<< endl;

  report << "Survey Biomass 6" <<endl;
  report << "Year:     " << yrs_srv6  <<endl;
  report << "Predicted:   " << pred_srv6  <<endl;
  report << "Observed:   " << obs_srv6_biom  <<endl<< endl;
  report << "Survey Biomass 7" <<endl;
  report << "Year:     " << yrs_srv7  <<endl;
  report << "Predicted:   " << pred_srv7  <<endl;
  report << "Observed:   " << obs_srv7_biom  <<endl<< endl;
  report << "q's"<<endl<<q_srv1<<endl<<q_srv2<<endl<<q_srv3<<endl<<q_srv4<<endl<<q_srv5<<endl<<q_srv6<<endl<<q_srv7<<endl<<q_srv8<<endl;
  for (i=styr;i<=endyr;i++)
  report<< i <<" "<<Sp_Biom(i-recage)<<" "<<srm_rec(i)<<" "<<sam_rec(i)<<endl;
  report<< "Age/Length residuals"<<endl;
  report << "fish_age"<<aa <<endl;
  for (i=1;i<=nyrs_fish1_age;i++) report << yrs_fish1_age(i)<<" "<<oac_fish1(i)-eac_fish1(i)<<endl; 

  report << "fish1_size Female"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_fish1_size;i++) report << yrs_fish1_size(i)<<" "<<osc_fish1_f(i)-esc_fish1_f(i)<<endl; 

  report << "fish1_size Male"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_fish1_size;i++) report << yrs_fish1_size(i)<<" "<<osc_fish1_m(i)-esc_fish1_m(i)<<endl; 

  report << "fish3_size Female"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_fish3_size;i++) report << yrs_fish3_size(i)<<" "<<osc_fish3_f(i)-esc_fish3_f(i)<<endl;
   
  report << "fish3_size Males"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_fish3_size;i++) report << yrs_fish3_size(i)<<" "<<osc_fish3_m(i)-esc_fish3_m(i)<<endl;

  report << "srv1_age"<<aa <<endl;
  for (i=1;i<=nyrs_srv1_age;i++) report << yrs_srv1_age(i)<<" "<<oac_srv1(i)-eac_srv1(i)<<endl; 

  report << "Obs_P_srv1_size"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_srv1_size;i++) report << yrs_srv1_size(i)<<" "<<osc_srv1_f(i)-esc_srv1_f(i)<<endl;

  report << "srv1_size Males"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_srv1_size;i++) report << yrs_srv1_size(i)<<" "<<osc_srv1_m(i)-esc_srv1_m(i)<<endl;
  
  report << "srv2_size Females"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_srv2_size;i++) report << yrs_srv2_size(i)<<" "<<osc_srv2_f(i)-esc_srv2_f(i)<<endl;

  report << "srv2_size Males"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_srv2_size;i++) report << yrs_srv2_size(i)<<" "<<osc_srv2_m(i)-esc_srv2_m(i)<<endl;

    report << "srv7_size Females"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_srv7_size;i++) report << yrs_srv7_size(i)<<" "<<osc_srv7_f(i)-esc_srv7_f(i)<<endl;

  report << "srv7_size Males"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_srv7_size;i++) report << yrs_srv7_size(i)<<" "<<osc_srv7_m(i)-esc_srv7_m(i)<<endl;

  report << "N_proj_f "<<endl<<N_proj_f<<endl<<"N_proj_m "<<N_proj_m<<endl<<" spawn_bio next year"<<endl<<spawn_biom_proj(endyr+1)<<endl
  <<"ABC full"<<ABC3<<endl;
  
  report <<" spawn_bio projected"<<endl<<spawn_biom_proj<<endl;
  report << "Specified catch projection"<<(catage_proj_f*wt_f+catage_proj_m*wt_m)<<endl<<"ABC projection: "<<pred_catch_proj<<endl;
  // New SDNR stuff
  report<<" average SDNRs for compositional data"<<endl;
  report<<"Fishery Ages "<<sdnr_fish1_age<<endl;
  report<<"Fishery 1 sizes "<<sdnr_fish1_size<<endl;
  report<<"Fishery 3 sizes "<<sdnr_fish3_size<<endl;
  report<<"Survey 1 ages "<<sdnr_srv1_age<<endl;
  report<<"Survey 2 ages "<<sdnr_srv2_age<<endl;
  report<<"Survey 1 sizes "<<sdnr_srv1_size<<endl;
  report<<"Survey 2 sizes "<<sdnr_srv2_size<<endl;
  report<<"Survey 7 sizes "<<sdnr_srv7_size<<endl;
  write_sarareport();
    save_gradients(gradients);
  # include "sable-r-report.cxx"     
FUNCTION write_sarareport
   ofstream sarareport("SABLE_SARA.dat");

 sarareport << "SABLE        # stock  " << endl;
 sarareport << "AK       # region     (AI AK BOG BSAI EBS GOA SEO WCWYK)" << endl;
 sarareport << endyr << "       # ASSESS_YEAR - year assessment is presented to the SSC" << endl;
 sarareport << "3b         # TIER  (1a 1b 2a 2b 3a 3b 4 5 6) " << endl;
 sarareport << "none       # TIER2  if mixed (none 1a 1b 2a 2b 3a 3b 4 5 6)" << endl;
 sarareport << "full       # UPDATE (new benchmark full partial)" << endl;
  sarareport << ssbsd(endyr)-2*ssbsd.sd(endyr) << "     # Minimum B  Lower 95% confidence interval for spawning biomass in assessment year" << endl;
 sarareport << ssbsd(endyr)+2*ssbsd.sd(endyr) << "     # Maximum B  Upper 95% confidence interval for spawning biomass in assessment year" << endl;
 sarareport << 0.875*B40 << "     # BMSY  is equilibrium spawning biomass at MSY (Tiers 1-2) or 7/8 x B40% (Tier 3)" << endl;
 sarareport << "Custom AMAK       # MODEL - Required only if NMFS toolbox software used; optional otherwise " << endl;
 sarareport << "NA         # VERSION - Required only if NMFS toolbox software used; optional otherwise" << endl;
 sarareport << "2          # number of sexes  if 1 sex=ALL elseif 2 sex=(FEMALE, MALE) " << endl;
 sarareport << "2          # number of fisheries" << endl;
 sarareport << "1000000          # multiplier for recruitment, N at age, and survey number (1,1000,1000000)" << endl;
 sarareport << "2          # recruitment age used by model or size" << endl;
 sarareport << "2          # age+ or mmCW+ used for biomass estimate" << endl;
 sarareport << "\"Single age\"        # Fishing mortality type such as \"Single age\" or \"exploitation rate\"" << endl;
 sarareport << "\"Age model\"         # Fishing mortality source such as \"Model\" or \"(total catch (t))/(survey biomass (t))\"" << endl;
 sarareport << "\"Age of maximum F\"  # Fishing mortality range such as \"Age of maximum F\"" << endl; 
 sarareport << "#FISHERYDESC -list of fisheries (ALL TWL LGL POT FIX FOR DOM TWLJAN LGLMAY POTAUG ...)" << endl; 
 sarareport << "FIX TWL" << endl; 
 sarareport <<"#FISHERYYEAR - list years used in the model " << endl;
 sarareport << yy << endl; 
 sarareport<<"#AGE - list of ages used in the model"<<endl;
 sarareport << aa << endl; 
 sarareport <<"#RECRUITMENT - Number of recruits by year " << endl;
 sarareport  << pred_rec << endl;     
 sarareport <<"#SPAWNBIOMASS - Spawning biomass by year in metric tons " << endl;
 sarareport  << spawn_biom << endl;  
 sarareport <<"#TOTALBIOMASS - Total biomass by year in metric tons " << endl;
 sarareport  << tot_biom << endl;
 sarareport <<"#TOTFSHRYMORT - Fishing mortality rate by year " << endl;
 sarareport  << Fmort_fish1*max(fish1_sel_f)+Fmort_fish3 <<endl;
 sarareport <<"#TOTALCATCH - Total catch by year in metric tons " << endl;
 sarareport  << pred_catch_fish1 + pred_catch_fish3 << endl;
 sarareport <<"#MATURITY - Maturity ratio by age (females only)" << endl;  
 sarareport  << p_mature << endl; 
 sarareport <<"#SPAWNWT - Average spawning weight (in kg) by age"<< endl; 
 sarareport << wt_mature << endl;
 sarareport <<"#NATMORT - Natural mortality rate for females then males"<< endl; 
  for (i=1;  i<=nages;  i++) 
   sarareport  << natmort <<"  ";
   sarareport<< endl;   
 for (i=1;  i<=nages;  i++) 
   sarareport  << natmort <<"  ";
   sarareport<< endl;   
    sarareport << "#N_AT_AGE - Estimated numbers of female (first) then male (second) fish at age " << endl;
  for (i=styr; i<=endyr;i++)
   sarareport <<natage_f(i)<< "  ";
   sarareport<<endl;
  for (i=styr; i<=endyr;i++)
   sarareport <<natage_m(i)<< "  ";
   sarareport<<endl;
 sarareport <<"#FSHRY_WT_KG - Fishery weight at age (in kg) females (first) males (second), only one fishery"<< endl;   
   sarareport << wt_f  << endl;
   sarareport << wt_m <<endl;
     sarareport << wt_f  << endl;
   sarareport << wt_m <<endl;
   sarareport<<endl;       
 sarareport << "#SELECTIVITY - Estimated fixed gear fishery selectivity, females first, males next" << endl;
 sarareport << fish4_sel_f << endl;
 sarareport << fish4_sel_m << endl;
 sarareport << "#SELECTIVITY - Estimated trawl gear fishery selectivity, females first, males next" << endl;
 sarareport << fish3_sel_f << endl;
 sarareport << fish3_sel_m << endl;
  sarareport << "#SURVEYDESC"<<endl;
 sarareport<<"GOA_trawl_survey"<<endl;
 sarareport<<"SURVEYMULT"<<endl;
 sarareport<<"1000"<<endl;
 sarareport << "#GOA_trawl_survey - Gulf of Alaska survey biomass (Year, Obs_biomass) " << endl;
 sarareport << yrs_srv7 << endl;
 sarareport<< obs_srv7_biom << endl;
   sarareport<<"AK_LL_survey_cooperative"<<endl;
 sarareport<<"SURVEYMULT"<<endl;
 sarareport<<"1000"<<endl;
 sarareport << "#AK Longline survey - cooperative with Japanese relative population numbers " << endl;
 sarareport << yrs_srv4 << endl;
 sarareport<< obs_srv4_biom << endl;
 sarareport<<"AK_LL_survey"<<endl;
 sarareport<<"SURVEYMULT"<<endl;
 sarareport<<"1000"<<endl;
 sarareport << "#AK Longline survey - relative population numbers " << endl;
 sarareport << yrs_srv3 << endl;
 sarareport<< obs_srv3_biom << endl;
  sarareport<<"AK_LL_fishery CPUE"<<endl;
 sarareport<<"FISHCPUE"<<endl;
 sarareport<<"1000"<<endl;
 sarareport << "#AK Longline fishery CPUE - relative population weight " << endl;
 sarareport << yrs_srv5 << endl;
 sarareport<< obs_srv5_biom << endl;
 sarareport<<"#STOCKNOTES"<<endl;
 sarareport<<"\"SAFE report indicates that this stock was not subjected to overfishing in "<<endyr-1<< " and is neither overfished nor approaching a condition of being overfished in "<<endyr<<".\""<<endl;

FUNCTION double sdnr(const dvar_vector& pred,const dvector& obs,double m)
  RETURN_ARRAYS_INCREMENT();
  double sdnr;
  dvector pp = value(pred)+0.000001;
  int ntmp = -obs.indexmin()+obs.indexmax();
  sdnr = std_dev(elem_div(obs+0.000001-pp,sqrt(elem_prod(pp,(1.-pp))/m)));
  RETURN_ARRAYS_DECREMENT();
  return sdnr;

FUNCTION write_projout
 ofstream projout("projold.dat");
// Function to write out data file for projection model....
 projout <<"#_Random_number_seed"<<endl;
 projout <<"1234"<<endl;
 projout <<"#_Number_of_fisheries"<<endl;
 projout <<"1"<<endl;
 projout <<"#_Number_of_projection_years"<<endl;
 projout <<"14"<<endl;
 projout <<"#_Number_of_simulations"<<endl;
 projout <<"1000"<<endl;
 projout <<"#_Begin_year_of_projection" <<endl;
 projout <<endyr<<endl;
 projout <<"#_Number_of_ages"<<endl;
 projout <<nages<<endl;
 for (j=1;j<=nages;j++) natmortv = natmort; 
 projout <<"#_Natural_Mortality" << aa << endl;
 projout <<natmortv<<endl;
 projout <<"#_2001_TAC_or_best_estimate_of_catch_2001"<<endl;
 projout <<obs_catch_fish1(endyr)<<endl;
 projout <<"#_F_ratio(must_sum_to_one_only_one_fishery)"<<endl;
 projout <<"1"<<endl;
 projout <<"#5year_Average_F(endyr-4,endyr_as_estimated_by_ADmodel)"<<endl;
 projout << mean(Fmort_fish1(endyr-4,endyr))<<endl;
 projout <<"#_Author_F_as_fraction_F_40%"<<endl;
 projout <<"1"<<endl;
 projout <<"#_Spawn_month"<<endl;
 projout << spawn_fract*12+1<<endl;
 projout <<"#_Wt_at_age_spawners"<<aa<<endl<<wt_f<< endl;
 projout <<"#_Wt_at_age_fishery" <<aa<<endl<<wt_f<< endl;
 projout <<"#_Maturity_divided_by_2(projection_program_uses_to_get_female_spawning_biomass_if_divide_by_2"<<aa<<endl<<elem_prod(elem_div(natage_f(endyr),(natage_f(endyr)+natage_m(endyr))),p_mature)<< endl;
 projout <<"#_Selectivity_fishery_scaled_to_max_at_one"<<aa<<endl<<fish1_sel_f/max(fish1_sel_f)<< endl;
 projout <<"#_Numbers_at_age_end_year"<<aa<<endl<<natage_f(endyr)+natage_m(endyr)<< endl;
 projout <<"#_N_recruitment_years"<<endl<<endyr-1979-1<< endl;
 projout <<"#_Recruitment_start_at_1977_yearclass=1979_for_age_2_recruits"<<yy(1979,endyr-2)<<endl<<pred_rec(1979,endyr -2)<< endl;
 projout.close();

FUNCTION write_newproj
 ofstream newproj("proj.dat");
// Function to write out data file for new Ianelli 2005 projection model....
 newproj <<"#Species name here:"<<endl;
 newproj <<"GOA_SABLE"<<endl;
 newproj <<"#SSL Species?"<<endl;
 newproj <<"0"<<endl;
 newproj <<"#Constant buffer of Dorn?"<<endl;
 newproj <<"0"<<endl;
 newproj <<"#Number of fisheries?"<<endl;
 newproj <<"2"<<endl;
 newproj <<"#Number of sexes?"<<endl;
 newproj <<"2"<<endl;
 newproj <<"#5year_Average_F(endyr-4,endyr_as_estimated_by_ADmodel)"<<endl;
 newproj << mean(Fmort_fish1(endyr-4,endyr)+Fmort_fish3(endyr-4,endyr))<<endl;
 newproj <<"#_Author_F_as_fraction_F_40%"<<endl;
 newproj <<"1"<<endl;
 newproj <<"#ABC SPR" <<endl;
 newproj <<"0.4"<<endl;
 newproj <<"#MSY SPR" <<endl;
 newproj <<"0.35"<<endl;
 newproj <<"#_Spawn_month"<<endl;
 newproj << spawn_fract*12+1<<endl;
 newproj <<"#_Number_of_ages"<<endl;
 newproj <<nages<<endl;
 newproj <<"#_F_ratio(must_sum_to_one_only_one_fishery)"<<endl;
 newproj <<fratio<<" "<<1-fratio<<endl;
 for (j=1;j<=nages;j++) natmortv = natmort; 
 newproj <<"#_Natural_Mortality" << aa << endl;
 newproj <<natmortv<<endl;
 newproj <<"#_Natural_Mortality" << aa << endl;
 newproj <<natmortv<<endl;
 newproj <<"#_Maturity_divided_by_2(projection_program_uses_to_get_female_spawning_biomass_if_divide_by_2"<<aa<<endl<<p_mature<< endl;
 newproj <<"#_Wt_at_age_spawners"<<aa<<endl<<wt_f<< endl;
 newproj <<"#_Wt_at_age_fishery1 female" <<aa<<endl<<wt_f<< endl;
 newproj <<"#_Wt_at_age_fishery2 female" <<aa<<endl<<wt_f<< endl;
 newproj <<"#_Wt_at_age_spawners1 male"<<aa<<endl<<wt_m<< endl;
 newproj <<"#_Wt_at_age_fishery2 male" <<aa<<endl<<wt_m<< endl;
 newproj <<"#_Selectivity_fishery_scaled_to_max_at_one"<<aa<<endl<<fish4_sel_f/max(fish4_sel_f)<< endl<<fish3_sel_f/max(fish3_sel_f)<<endl;
 newproj <<"#_Selectivity_fishery_scaled_to_max_at_one"<<aa<<endl<<fish4_sel_m/max(fish4_sel_m)<< endl<<fish3_sel_m/max(fish3_sel_m)<<endl;
 newproj <<"#_Numbers_at_age_end_year"<<aa<<endl<<natage_f(endyr)<<endl<<natage_m(endyr)<< endl;
 newproj <<"#_N_recruitment_years"<<endl<<endyr-1979-1<< endl;
 newproj <<"#_Recruitment_start_at_1977_yearclass=1979_for_age_2_recruits"<<yy(1979,endyr-2)<<endl<<pred_rec(1979,endyr-2)<< endl;
 newproj <<"#_Spawners per recruitment (starting at 1977)"<<endl<<spawn_biom(1977,endyr-5)<< endl;
 newproj.close();

FUNCTION write_fullrep
     ofstream fullrep("sable.rep");

  fullrep<<"****Executive mary Material*****"<<endl;
  fullrep<<"     Model name"     <<endl;
  fullrep<<model_name<<endl;
  fullrep<<"     .dat file"     <<endl;
  fullrep<<data_file<<endl;
  fullrep<<"     Number parameters estimated"     <<endl;
  fullrep<<initial_params::nvarcalc()<<endl;
  fullrep<<"     TotalBiomass for "<<endyr+1<<endl;
  fullrep<<tot_biom_proj(endyr+1)<<endl;
  fullrep<<"     TotalBiomass for "<<endyr+2     <<endl;
  fullrep<<tot_biom_proj(endyr+2)<<endl;
  fullrep<<"     Female_Spawning Biomass for "<<endyr+1     <<endl;
  fullrep<<spawn_biom_proj(endyr+1)<<endl;
  fullrep<<"     Female_Spawning_Biomass for "<<endyr+2     <<endl;
  fullrep<<spawn_biom_proj(endyr+2)<<endl;
  fullrep<<"     B_100"     <<endl;
  fullrep<<SB0*mean(pred_rec(1979,endyr-recage))/2<<endl;
  fullrep<<"     B_40"     <<endl;
  fullrep<<B40<<endl;
  fullrep<<"     B_35"     <<endl;
  fullrep<<SBF35*mean(pred_rec(1979,endyr-recage))/2<<endl;
  fullrep<<"     F_40"     <<endl;
  fullrep<<F40<<endl;
  fullrep<<"     F_35"     <<endl;
  fullrep<<F35<<endl;
  fullrep<<"     F_ABC for "<<endyr+1     <<endl;
  fullrep<<FABC<<endl;
  fullrep<<"     F_ABC for "<<endyr+2     <<endl;
  fullrep<<FABC2<<endl;
  fullrep<<"     ABC for "<<endyr+1     <<endl;
  fullrep<<pred_catch_proj(endyr+1)<<endl;
  fullrep<<"     ABC for "<<endyr+2     <<endl;
  fullrep<<pred_catch_proj(endyr+2)<<endl;
  fullrep<<"     F_OFL for "<<endyr+1     <<endl;
  fullrep<<FOFL<<endl;
  fullrep<<"     F_OFL for "<<endyr+2     <<endl;
  fullrep<<FOFL2<<endl;
  fullrep<<"     OFL for "<<endyr+1     <<endl;
  fullrep<<OFL<<endl; 
  fullrep<<"     OFL for "<<endyr+2     <<endl;
  fullrep<<pred_catch_proj_OFL(endyr+2)<<endl; 
  fullrep<<"     Total likelihood"     <<endl;
  fullrep<<obj_fun<<endl;
  fullrep<<"     Data likelihood"     <<endl;
  fullrep<<Like<<endl<<endl;
  
  fullrep<<" ************   Some more parameter estimates and their SDs ************"<<endl;
 
  if(last_phase()) {
    // add standard deviation data types    
  fullrep<<"   q_domestic   "<<endl;
  fullrep<<q_srv1<<" "<<q_srv1.sd<<endl;
  fullrep<<"   q_cooperative  "<<endl;
   fullrep<<q_srv2<<" "<<q_srv2.sd<<endl;
  fullrep<<"   q_trawl  "<<endl;
   fullrep<<q_srv7<<" "<<q_srv7.sd<<endl;
 /*
  fullrep<<natmort<<" "<<nattymort.sd<<endl;
  fullrep<<"  sigr   "<<endl;  
  fullrep<<sigr<<" "<<cigar.sd<<endl;  
  fullrep<<"   log_mean_rec"<<endl;
  fullrep<<log_mean_rec<<" "<<LMR.sd<<endl;
 */
  fullrep<<"   F_40"<<endl;
  fullrep<<F40<<" "<<F40.sd<<endl;
  fullrep<<"    tot_biom"<<endl;
  fullrep<<tot_biom_proj(endyr+1)<<" "<<tot_biom_proj.sd(endyr+1)<<endl;
  fullrep<<"   spawn_biom"<<endl;
  fullrep<<spawn_biom_proj(endyr+1)<<" "<<spawn_biom_proj.sd(endyr+1)<<endl;
  fullrep<<"    B40"<<endl;
  fullrep<<B40<<" "<<B40.sd<<endl;
  fullrep<<"   ABC"<<endl;
  fullrep<<pred_catch_proj(endyr+1)<<" "<<pred_catch_proj.sd(endyr+1)<<endl<<endl;
 
 }
  fullrep<<model_name<<endl;
  fullrep<<data_file<<endl;
  fullrep<<"Num_parameters_Estimated "<<initial_params::nvarcalc()<<endl;
  fullrep << "Year "<< yy <<endl;
  fullrep << "Pred_Catch Fixed Geaar "<< pred_catch_fish1<<endl<<"Pred catch trawl "<<pred_catch_fish3 <<endl;
  fullrep << "Obs_Catch Fixed Gear "<< obs_catch_fish1<<endl<<" Obs_Catch Trawl "<<obs_catch_fish3 <<endl;

  fullrep << "Survival Female"<<aa <<endl;
  for (i=styr;i<=endyr;i++) fullrep << i<<" "<<S_f(i) <<endl; fullrep<<endl;
  fullrep << "Survival Male"<<aa <<endl;
  for (i=styr;i<=endyr;i++) fullrep << i<<" "<<S_m(i) <<endl; fullrep<<endl;


  fullrep << "Numbers Females "<<aa <<endl;
  for (i=styr;i<=endyr;i++) fullrep << i<<" "<<natage_f(i) <<endl; fullrep<<endl;
  fullrep << "Numbers Males"<<aa <<endl;
  for (i=styr;i<=endyr;i++) fullrep << i<<" "<<natage_m(i) <<endl; fullrep<<endl;

  fullrep << "Age "<<aa <<endl;
  fullrep << "Fishery_sel1 Females"<<fish1_sel_f  <<endl;
  fullrep << "Fishery_sel1 Males"<<fish1_sel_m  <<endl;
  fullrep << "Fishery_sel2 "<<fish2_sel  <<endl;
  fullrep << "Fishery_sel3 "<<fish3_sel_f  <<endl;
  fullrep << "Fishery_sel4_f "<<fish4_sel_f  <<endl;
  fullrep << "Fishery_sel4_m "<<fish4_sel_m  <<endl;
  fullrep << "Survey_sel1 Female"<<srv1_sel_f  <<endl<<endl;
  fullrep << "Survey_sel1 male"<<srv1_sel_m  <<endl<<endl;
  fullrep << "Survey_sel2 Female"<<srv2_sel_f  <<endl<<endl;
  fullrep << "Survey_sel2 male"<<srv2_sel_m  <<endl<<endl;
  fullrep << "Survey_sel7 Female"<<srv7_sel_f  <<endl<<endl;
  fullrep << "Survey_sel7 male"<<srv7_sel_m  <<endl<<endl;

  sdnr_fish1_age = 0;
  sdnr_fish1_size = 0;
  sdnr_fish3_size = 0;
  sdnr_srv1_age = 0;
  sdnr_srv2_age = 0;
  sdnr_srv1_size = 0;
  sdnr_srv2_size = 0;
  sdnr_srv7_size = 0;


  fullrep << "Obs_P_fish_age"<<aa <<endl;
  for (i=1;i<=nyrs_fish1_age;i++) {
      sdnr_fish1_age +=sdnr(eac_fish1(i),oac_fish1(i),wt_fish1_age*double(nsamples_fish1_age(i)))/nyrs_fish1_age;
      fullrep << yrs_fish1_age(i)<<" "<<oac_fish1(i) 
      <<" eff_N "<<(1-eac_fish1(i))*eac_fish1(i)/norm2(oac_fish1(i)-eac_fish1(i))  <<" N "<<nsamples_fish1_age(i)
      <<" SDNR "<< sdnr(eac_fish1(i),oac_fish1(i),wt_fish1_age*double(nsamples_fish1_age(i)))<<endl; fullrep<<endl; }
  fullrep << "Pred_P_fish1_age"<<aa <<endl;
  for (i=1;i<=nyrs_fish1_age;i++) fullrep << yrs_fish1_age(i)<<" "<<eac_fish1(i) <<endl; fullrep<<endl;

  fullrep << "Obs_P_fish1_size Female"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_fish1_size;i++)  {
   sdnr_fish1_size += sdnr(esc_fish1_f(i),osc_fish1_f(i),wt_fish1_size*double(nsamples_fish1_size(i)))/nyrs_fish1_size/2;
  fullrep << yrs_fish1_size(i)<<" "<<osc_fish1_f(i) 
      <<" eff_N "<<(1-esc_fish1_f(i))*esc_fish1_f(i)/norm2(osc_fish1_f(i)-esc_fish1_f(i))  <<" N "<<nsamples_fish1_size(i)
      <<" SDNR "<< sdnr(esc_fish1_f(i),osc_fish1_f(i),wt_fish1_size*double(nsamples_fish1_size(i)))<<endl; fullrep<<endl; }
  fullrep << "Pred_P_fish1_size"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_fish1_size;i++) fullrep << yrs_fish1_size(i)<<" "<<esc_fish1_f(i) <<endl; fullrep<<endl;

    fullrep << "Obs_P_fish1_size Male"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_fish1_size;i++) {
    sdnr_fish1_size += sdnr(esc_fish1_m(i),osc_fish1_m(i),wt_fish1_size*double(nsamples_fish1_size(i)))/nyrs_fish1_size/2;
  fullrep << yrs_fish1_size(i)<<" "<<osc_fish1_m(i) 
      <<" eff_N "<<(1-esc_fish1_m(i))*esc_fish1_m(i)/norm2(osc_fish1_m(i)-esc_fish1_m(i))  <<" N "<<nsamples_fish1_size(i)
      <<" SDNR "<< sdnr(esc_fish1_m(i),osc_fish1_m(i),wt_fish1_size*double(nsamples_fish1_size(i)))<<endl; fullrep<<endl; }
  fullrep << "Pred_P_fish1_size"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_fish1_size;i++) fullrep << yrs_fish1_size(i)<<" "<<esc_fish1_m(i) <<endl; fullrep<<endl;

   fullrep << "Obs_P_fish3_size Males"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_fish3_size;i++) {
    sdnr_fish3_size+= sdnr(esc_fish3_m(i),osc_fish3_m(i),wt_fish3_size*double(nsamples_fish3_size(i)))/nyrs_fish3_size/2;
  fullrep << yrs_fish3_size(i)<<" "<<osc_fish3_m(i) 
      <<" eff_N "<<(1-esc_fish3_m(i))*esc_fish3_m(i)/norm2(osc_fish3_m(i)-esc_fish3_m(i))  <<" N "<<nsamples_fish3_size(i)
      <<" SDNR "<< sdnr(esc_fish3_m(i),osc_fish3_m(i),wt_fish3_size*double(nsamples_fish3_size(i)))<<endl; fullrep<<endl; }
  fullrep << "Pred_P_fish3_size"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_fish3_size;i++) fullrep << yrs_fish3_size(i)<<" "<<esc_fish3_m(i) <<endl; fullrep<<endl;

  fullrep << "Obs_P_fish3_size Female"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_fish3_size;i++)  {
     sdnr_fish3_size+= sdnr(esc_fish3_f(i),osc_fish3_f(i),wt_fish3_size*double(nsamples_fish3_size(i)))/nyrs_fish3_size/2;
  fullrep << yrs_fish3_size(i)<<" "<<osc_fish3_f(i) 
      <<" eff_N "<<(1-esc_fish3_f(i))*esc_fish3_f(i)/norm2(osc_fish3_f(i)-esc_fish3_f(i))  <<" N "<<nsamples_fish3_size(i)
      <<" SDNR "<< sdnr(esc_fish3_f(i),osc_fish3_f(i),wt_fish3_size*double(nsamples_fish3_size(i)))<<endl; fullrep<<endl; }
  fullrep << "Pred_P_fish3_size"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_fish3_size;i++) fullrep << yrs_fish3_size(i)<<" "<<esc_fish3_f(i) <<endl; fullrep<<endl;

  fullrep << "Obs_P_srv1_age"<<aa <<endl;
  for (i=1;i<=nyrs_srv1_age;i++) {
   sdnr_srv1_age+=sdnr(eac_srv1(i),oac_srv1(i),wt_srv1_age*double(nsamples_srv1_age(i)))/nyrs_srv1_age;
   fullrep << yrs_srv1_age(i)<<" "<<oac_srv1(i) 
      <<" eff_N "<<(1-eac_srv1(i))*eac_srv1(i)/norm2(oac_srv1(i)-eac_srv1(i)) <<" N "<<nsamples_srv1_age(i)
      <<" SDNR "<< sdnr(eac_srv1(i),oac_srv1(i),wt_srv1_age*double(nsamples_srv1_age(i)))<<endl; fullrep<<endl; }
  fullrep << "Pred_P_srv1_age"<<aa <<endl;
  for (i=1;i<=nyrs_srv1_age;i++) fullrep << yrs_srv1_age(i)<<" "<<eac_srv1(i) <<endl; fullrep<<endl;

     fullrep << "Obs_P_srv2_age"<<aa <<endl;
  for (i=1;i<=nyrs_srv2_age;i++) {
    sdnr_srv2_age+=sdnr(eac_srv2(i),oac_srv2(i),wt_srv1_age*double(nsamples_srv2_age(i)))/nyrs_srv2_age;
    fullrep << yrs_srv2_age(i)<<" "<<oac_srv2(i) 
      <<" eff_N "<<(1-eac_srv2(i))*eac_srv2(i)/norm2(oac_srv2(i)-eac_srv2(i)) <<" N "<<nsamples_srv2_age(i)
      <<" SDNR "<< sdnr(eac_srv2(i),oac_srv2(i),wt_srv1_age*double(nsamples_srv2_age(i)))<<endl; fullrep<<endl; }
  fullrep << "Pred_P_srv2_age"<<aa <<endl;
  for (i=1;i<=nyrs_srv2_age;i++) fullrep << yrs_srv2_age(i)<<" "<<eac_srv2(i) <<endl; fullrep<<endl;

  fullrep << "Obs_P_srv1_size"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_srv1_size;i++) {
     sdnr_srv1_size+=sdnr(esc_srv1_f(i),osc_srv1_f(i),wt_srv1_size*double(nsamples_srv1_size(i)))/nyrs_srv1_size/2;
    fullrep << yrs_srv1_size(i)<<" "<<osc_srv1_f(i) 
      <<" eff_N "<<(1-esc_srv1_f(i))*esc_srv1_f(i)/norm2(osc_srv1_f(i)-esc_srv1_f(i)) <<" N "<<nsamples_srv1_size(i)
      <<" SDNR "<< sdnr(esc_srv1_f(i),osc_srv1_f(i),wt_srv1_size*double(nsamples_srv1_size(i)))<<endl; fullrep<<endl; }
  fullrep << "Pred_P_srv1_size"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_srv1_size;i++) fullrep << yrs_srv1_size(i)<<" "<<esc_srv1_f(i) <<endl; fullrep<<endl;

    fullrep << "Obs_P_srv1_size Males"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_srv1_size;i++) {
    sdnr_srv1_size+=sdnr(esc_srv1_m(i),osc_srv1_m(i),wt_srv1_size*double(nsamples_srv1_size(i)))/nyrs_srv1_size/2;
     fullrep << yrs_srv1_size(i)<<" "<<osc_srv1_m(i) 
      <<" eff_N "<<(1-esc_srv1_m(i))*esc_srv1_m(i)/norm2(osc_srv1_m(i)-esc_srv1_m(i)) <<" N "<<nsamples_srv1_size(i)
      <<" SDNR "<< sdnr(esc_srv1_m(i),osc_srv1_m(i),wt_srv1_size*double(nsamples_srv1_size(i)))<<endl; fullrep<<endl; }
  fullrep << "Pred_P_srv1_size"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_srv1_size;i++) fullrep << yrs_srv1_size(i)<<" "<<esc_srv1_m(i) <<endl; fullrep<<endl;

  fullrep << "Obs_P_srv2_size"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_srv2_size;i++) {
    sdnr_srv2_size+=sdnr(esc_srv2_f(i),osc_srv2_f(i),wt_srv2_size*double(nsamples_srv2_size(i)))/nyrs_srv2_size/2;
     fullrep << yrs_srv2_size(i)<<" "<<osc_srv2_f(i) 
      <<" eff_N "<<(1-esc_srv2_f(i))*esc_srv2_f(i)/norm2(osc_srv2_f(i)-esc_srv2_f(i)) <<" N "<<nsamples_srv2_size(i)
      <<" SDNR "<< sdnr(esc_srv2_f(i),osc_srv2_f(i),wt_srv2_size*double(nsamples_srv2_size(i)))<<endl; fullrep<<endl; }
  fullrep << "Pred_P_srv2_size"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_srv2_size;i++) fullrep << yrs_srv2_size(i)<<" "<<esc_srv2_f(i) <<endl; fullrep<<endl;

    fullrep << "Obs_P_srv2_size Males"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_srv2_size;i++) {
        sdnr_srv2_size+=sdnr(esc_srv2_m(i),osc_srv2_m(i),wt_srv2_size*double(nsamples_srv2_size(i)))/nyrs_srv2_size/2;
 fullrep << yrs_srv2_size(i)<<" "<<osc_srv2_m(i) 
      <<" eff_N "<<(1-esc_srv2_m(i))*esc_srv2_m(i)/norm2(osc_srv2_m(i)-esc_srv2_m(i)) <<" N "<<nsamples_srv2_size(i)
      <<" SDNR "<< sdnr(esc_srv2_m(i),osc_srv2_m(i),wt_srv2_size*double(nsamples_srv2_size(i)))<<endl; fullrep<<endl; }
  fullrep << "Pred_P_srv2_size Males"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_srv2_size;i++) fullrep << yrs_srv2_size(i)<<" "<<esc_srv2_m(i) <<endl; fullrep<<endl;
  
    fullrep << "Obs_P_srv7_size"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_srv7_size;i++) {
    sdnr_srv7_size+= sdnr(esc_srv7_f(i),osc_srv7_f(i),wt_srv7_size*double(nsamples_srv7_size(i)))/nyrs_srv7_size/2;
     fullrep << yrs_srv7_size(i)<<" "<<osc_srv7_f(i) 
      <<" eff_N "<<(1-esc_srv7_f(i))*esc_srv7_f(i)/norm2(osc_srv7_f(i)-esc_srv7_f(i)) <<" N "<<nsamples_srv7_size(i)
      <<" SDNR "<< sdnr(esc_srv7_f(i),osc_srv7_f(i),wt_srv7_size*double(nsamples_srv7_size(i)))<<endl; fullrep<<endl; }
  fullrep << "Pred_P_srv7_size"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_srv7_size;i++) fullrep << yrs_srv7_size(i)<<" "<<esc_srv7_f(i) <<endl; fullrep<<endl;

    fullrep << "Obs_P_srv7_size Males"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_srv7_size;i++) {
        sdnr_srv7_size+= sdnr(esc_srv7_m(i),osc_srv7_m(i),wt_srv7_size*double(nsamples_srv7_size(i)))/nyrs_srv7_size/2;
  fullrep << yrs_srv7_size(i)<<" "<<osc_srv7_m(i) 
      <<" eff_N "<<(1-esc_srv7_m(i))*esc_srv7_m(i)/norm2(osc_srv7_m(i)-esc_srv7_m(i)) <<" N "<<nsamples_srv7_size(i)
      <<" SDNR "<< sdnr(esc_srv7_m(i),osc_srv7_m(i),wt_srv7_size*double(nsamples_srv7_size(i)))<<endl; fullrep<<endl; }
  fullrep << "Pred_P_srv7_size Males"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_srv7_size;i++) fullrep << yrs_srv7_size(i)<<" "<<esc_srv7_m(i) <<endl; fullrep<<endl;

  fullrep << "Survey Biomass " <<endl;
  fullrep << "Year:     " << yrs_srv1  <<endl;
  fullrep << "Predicted:   " << pred_srv1  <<endl;
  fullrep << "Observed:   " << obs_srv1_biom  <<endl<< endl;

  fullrep << "Weight "<<aa<< endl;
  fullrep << " "<< wt_f << endl;

  fullrep << "Fully_selected_F "<<aa <<endl;
  fullrep << " "<< Fmort_fish1*max(fish1_sel_f)+Fmort_fish3 <<endl;

  fullrep << "Year " << yy<< endl;
  fullrep << "SpBiom "<< spawn_biom <<endl;
  fullrep << "Tot_biom "<< tot_biom   <<endl;
    // fullrep << tot_biom.sd <<endl;

  fullrep << "Fishery Selectivity " <<endl;
  fullrep << fish1_sel_f / max(fish1_sel_f) <<endl;
  fullrep << "F35 F40 F50 "<<endl;
  fullrep <<  F35 << " "<< mF40 <<" "<<  F50 <<endl <<endl <<endl <<endl <<endl <<endl;
  fullrep << "SSB projection: "<< spawn_biom_proj<<endl<<"Catch projection: "<<pred_catch_proj<<endl;
  fullrep <<  "B40: "<< B40<< endl;
  fullrep << "Wts_n_Likelihoods  " << endl;
  fullrep << wt_ssqcatch_fish1 <<" "<<ssqcatch <<" " ; fullrep << "SSQ_Catch_Likelihood" << endl;
  fullrep << wt_srv3     <<" "<<surv_like(3)  <<" " ; fullrep << "Domestic_Survey_Abundance_Index_Likelihood" << endl;
  fullrep << wt_srv4     <<" "<<surv_like(4)  <<" " ; fullrep << "Cooperative_Survey_Abundance_Index_Likelihood" << endl;
  fullrep << wt_fish1_age <<" "<<age_like(1)  <<" " ; fullrep << "Fishery_Age_Composition_Likelihood" << endl;
  fullrep << wt_srv1_age <<" "<<age_like(2)  <<" " ; fullrep << "Survey_Age_Composition_Likelihood_DomesticLL" << endl;
  fullrep << wt_srv1_age <<" "<<age_like(16)  <<" " ; fullrep << "Survey_Age_Composition_Likelihood_CooperativeLL" << endl;
  fullrep << wt_fish1_size<<" "<<age_like(3)+age_like(4)  <<" " ; fullrep << "Fishery_Size_Composition_Likelihood_Fixed" << endl;
  fullrep << wt_fish3_size<<" "<<age_like(6)+age_like(7)  <<" " ; fullrep << "Fishery_Size_Composition_Likelihood_Trawl" << endl;
  fullrep << wt_srv1_size<<" "<<age_like(9)+age_like(10)  <<" " ; fullrep << "Survey_Size_Composition_Likelihood_Domestic LL" << endl;
  fullrep << wt_srv2_size<<" "<<age_like(11)+age_like(12)  <<" " ; fullrep << "Survey_Size_Composition_Likelihood_Cooperative_LL" << endl;
  fullrep << wt_srv7_size<<" "<<age_like(13)+age_like(14)  <<" " ; fullrep << "Survey_Size_Composition_Likelihood_GOATrawl" << endl;
  fullrep << wt_rec_var <<" "<<rec_like     <<" " ; fullrep << "Recruitment_Deviations_Likelihood" << endl;

  fullrep << wt_sel_reg_fish1 <<" "<<sel_like(1)      <<" " ; fullrep << "Fish_sel_Regularity_Penalty  "<<endl  ;
  fullrep << wt_sel_reg_srv1 <<" "<<sel_like(2)      <<" " ; fullrep << "Surv_sel_Regularity_Penalty  "<<endl  ;
  fullrep << wt_sel_dome_fish1<<" "<<sel_like(3)      <<" " ; fullrep << "Fish_Sel_Domeshapedness_Penalty "<<endl  ;
  fullrep << wt_sel_dome_srv1<<" "<<sel_like(4)      <<" " ; fullrep << "Surv_Sel_Domeshapedness_Penalty "<<endl  ;
  fullrep << "0"   <<" "<<avg_sel_penalty  <<" " ; fullrep << "Average_Selectivity_Condition_Penalty "<<endl  ;
  fullrep << wt_fmort_reg     <<" "<<F_mort_regularity<<" " ; fullrep << "Fishing_Mortality_Regularity_Penalty" << endl;
 // fullrep << wt_ssqcatch     <<" "<<F_dev_penalty<<" " ; fullrep << "Fishing_Mortality_Deviations_Penalty" << endl;
  fullrep << " "<<priors(1)  <<" " ; fullrep << "priors sigr"     <<endl;
  fullrep << " "<<priors(2)  <<" " ; fullrep << "priors q_1" <<endl;
   fullrep << " "<<priors(5)  <<" " ; fullrep << "priors q_2" <<endl;
  fullrep << " "<<priors(6)  <<" " ; fullrep << "priors q_3" <<endl;
  fullrep << " "<<priors(7)  <<" " ; fullrep << "priors q_4" <<endl;
  fullrep << " "<<priors(8)  <<" " ; fullrep << "priors q_5" <<endl;
  fullrep << " "<<priors(9)  <<" " ; fullrep << "priors q_6" <<endl;
  fullrep << " "<<priors(10)  <<" " ; fullrep << "priors q_7" <<endl;
  fullrep << " "<<priors(10)  <<" " ; fullrep << "priors q_8" <<endl;
  fullrep << " "<<priors(4)  <<" " ; fullrep << "priors M"<<endl;
  fullrep << " "<<obj_fun    <<" " ; fullrep << "obj_fun"         <<endl;
  fullrep << " "<<Like       <<" " ; fullrep << "data likelihood" <<endl;//(2*square(sigr))+ size_count(log_rec_dev)*log(sigr)<<endl;
 if(last_phase()) { 
  fullrep <<" SDNR1 "<< wt_srv1*std_dev(elem_div((pred_srv1(yrs_srv1)-obs_srv1_biom),obs_srv1_se))<<endl;
  fullrep <<" SDNR2 "<< wt_srv2*std_dev(elem_div((pred_srv2(yrs_srv2)-obs_srv2_biom),obs_srv2_se))<<endl;
  fullrep <<" SDNR3 "<< wt_srv3*std_dev(elem_div((pred_srv3(yrs_srv3)-obs_srv3_biom),obs_srv3_se))<<endl;
  fullrep <<" SDNR4 "<< wt_srv4*std_dev(elem_div((pred_srv4(yrs_srv4)-obs_srv4_biom),obs_srv4_se))<<endl;
  fullrep <<" SDNR5 "<< wt_srv5*std_dev(elem_div((pred_srv5(yrs_srv5)-obs_srv5_biom),obs_srv5_se))<<endl;
  fullrep <<" SDNR6 "<< wt_srv6*std_dev(elem_div((pred_srv6(yrs_srv6)-obs_srv6_biom),obs_srv6_se))<<endl;
  fullrep <<" SDNR7 "<< wt_srv7*std_dev(elem_div((pred_srv7(yrs_srv7)-obs_srv7_biom),obs_srv7_se))<<endl;
//  fullrep <<" SDNR8 "<< wt_srv8*std_dev(elem_div((pred_srv8(yrs_srv8)-obs_srv8_biom),obs_srv8_se))<<endl;
 

    }
  fullrep << "SigmaR: "<<sigr<< " Nat_Mort: "<<natmort<<" Male delta M "<<mdelta<<" Spawning Per Recruit "<< " "<<SBF40<<" "<<SB0<<" Virgin SPR "<<endl;
  fullrep << "Stock-recruitment, type: "<<SrType<<" 1=Ricker, 2=B-Holt, 3=Mean"<<endl;
  fullrep << "Year SSB SR_Pred R_Est "<<endl;
  fullrep << "Weighted likelihods for comps broken down"<<endl;
  fullrep << "Fish1 Age, Survey 1 Age, Fish 1 Size Female, Fish 1 Size Male, Fish 2 Size, Fish3 Size Female, F3 Size Male, Srv1 Size F, Srv1 Size M, Srv2 Size F, Srv2 Size M, Srv7 Size F, Srv7 Size M"<<endl;
  fullrep <<age_like<<" age _like"<<endl<<surv_like<<" surv like "<<endl<<sel_like<<" sel_like"<<endl;
  fullrep << "Unweighted likelihods for comps broken down"<<endl;
  fullrep << "Fish1 Age, Survey 1 Age, Fish 1 Size Female, Fish 1 Size Male, Fish 2 Size, Fish3 Size Female, F3 Size Male, Srv1 Size F, Srv1 Size M, Srv2 Size F, Srv2 Size M, Srv7 Size F, Srv7 Size M"<<endl;
  fullrep <<age_like(1) / (0.00001+wt_fish1_age)<< " "<<(age_like(2)+0.0001) / (0.00001+wt_srv1_age)<< " "<<(age_like(3)+0.0001) / (0.00001+wt_fish1_size)<< " "<<(age_like(4)+0.0001) / (0.00001+wt_fish1_size)<< " "<<(age_like(5)+0.0001) / (0.00001+wt_fish2_size)<< " "<<(age_like(6)+0.0001) / (0.00001+wt_fish3_size)<< " "<<(age_like(7)+0.0001) / (0.00001+wt_fish3_size)<< " "<<(age_like(8)+0.0001) / (0.00001+wt_fish4_size)<< " "<<(age_like(9)+0.0001) / (0.00001+wt_srv1_size)<< " "<<(age_like(10)+0.0001) / (0.00001+wt_srv1_size)<< " "<<(age_like(11)+0.0001) / (0.00001+wt_srv2_size)<< " "<<(age_like(12)+0.0001) / (0.00001+wt_srv2_size)<< " "<<(age_like(13)+0.0001) / (0.00001+wt_srv7_size)<< " "<<(age_like(14)+0.0001) / (0.00001+wt_srv7_size)<<" "<<(age_like(15)+0.0001) / (0.00001+wt_srv7_age)<< " "<<(age_like(16)+0.0001) / (0.00001+wt_srv1_age) <<" unw_age)_like"<<endl;
  fullrep <<surv_like(1) / (0.00001+wt_srv1)<< " "<<surv_like(2) / (0.00001+wt_srv2)<< " "<<surv_like(3) / (0.00001+wt_srv3)<< " "<<surv_like(4) / (0.00001+wt_srv4)<< " "<<surv_like(5) / (0.00001+wt_srv5)<< " "<<surv_like(6) / (0.00001+wt_srv6)<< " "<<(surv_like(7)+0.0001) / (0.00001+wt_srv7)<< " "<<(surv_like(8)+0.0001) / (0.00001+wt_srv8)<< " "<<"unw_surv_like "<<endl<<sel_like<<" sel_like"<<endl;
  fullrep << "Survey Biomass 2" <<endl;
  fullrep << "Year:     " << yrs_srv2  <<endl;
  fullrep << "Predicted:   " << pred_srv2  <<endl;
  fullrep << "Observed:   " << obs_srv2_biom  <<endl<< endl;
  fullrep << "Survey Biomass 3" <<endl;
  fullrep << "Year:     " << yrs_srv3  <<endl;
  fullrep << "Predicted:   " << pred_srv3  <<endl;
  fullrep << "Observed:   " << obs_srv3_biom  <<endl<< endl;

  fullrep << "Survey Biomass 4" <<endl;
  fullrep << "Year:     " << yrs_srv4  <<endl;
  fullrep << "Predicted:   " << pred_srv4  <<endl;
  fullrep << "Observed:   " << obs_srv4_biom  <<endl<< endl;

  fullrep << "Survey Biomass 5" <<endl;
  fullrep << "Year:     " << yrs_srv5  <<endl;
  fullrep << "Predicted:   " << pred_srv5  <<endl;
  fullrep << "Observed:   " << obs_srv5_biom  <<endl<< endl;

  fullrep << "Survey Biomass 6" <<endl;
  fullrep << "Year:     " << yrs_srv6  <<endl;
  fullrep << "Predicted:   " << pred_srv6  <<endl;
  fullrep << "Observed:   " << obs_srv6_biom  <<endl<< endl;
  fullrep << "Survey Biomass 7" <<endl;
  fullrep << "Year:     " << yrs_srv7  <<endl;
  fullrep << "Predicted:   " << pred_srv7  <<endl;
  fullrep << "Observed:   " << obs_srv7_biom  <<endl<< endl;
  fullrep << "q's"<<endl<<q_srv1<<endl<<q_srv2<<endl<<q_srv3<<endl<<q_srv4<<endl<<q_srv5<<endl<<q_srv6<<endl<<q_srv7<<endl<<q_srv8<<endl;
  for (i=styr;i<=endyr;i++)
  fullrep<< i <<" "<<Sp_Biom(i-recage)<<" "<<srm_rec(i)<<" "<<sam_rec(i)<<endl;
  fullrep<< "Age/Length residuals"<<endl;
  fullrep << "fish_age"<<aa <<endl;
  for (i=1;i<=nyrs_fish1_age;i++) fullrep << yrs_fish1_age(i)<<" "<<oac_fish1(i)-eac_fish1(i)<<endl; 

  fullrep << "fish1_size Female"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_fish1_size;i++) fullrep << yrs_fish1_size(i)<<" "<<osc_fish1_f(i)-esc_fish1_f(i)<<endl; 

  fullrep << "fish1_size Male"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_fish1_size;i++) fullrep << yrs_fish1_size(i)<<" "<<osc_fish1_m(i)-esc_fish1_m(i)<<endl; 

  fullrep << "fish3_size Female"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_fish3_size;i++) fullrep << yrs_fish3_size(i)<<" "<<osc_fish3_f(i)-esc_fish3_f(i)<<endl;
   
  fullrep << "fish3_size Males"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_fish3_size;i++) fullrep << yrs_fish3_size(i)<<" "<<osc_fish3_m(i)-esc_fish3_m(i)<<endl;

  fullrep << "srv1_age"<<aa <<endl;
  for (i=1;i<=nyrs_srv1_age;i++) fullrep << yrs_srv1_age(i)<<" "<<oac_srv1(i)-eac_srv1(i)<<endl; 

  fullrep << "Obs_P_srv1_size"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_srv1_size;i++) fullrep << yrs_srv1_size(i)<<" "<<osc_srv1_f(i)-esc_srv1_f(i)<<endl;

  fullrep << "srv1_size Males"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_srv1_size;i++) fullrep << yrs_srv1_size(i)<<" "<<osc_srv1_m(i)-esc_srv1_m(i)<<endl;
  
  fullrep << "srv2_size Females"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_srv2_size;i++) fullrep << yrs_srv2_size(i)<<" "<<osc_srv2_f(i)-esc_srv2_f(i)<<endl;

  fullrep << "srv2_size Males"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_srv2_size;i++) fullrep << yrs_srv2_size(i)<<" "<<osc_srv2_m(i)-esc_srv2_m(i)<<endl;

    fullrep << "srv7_size Females"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_srv7_size;i++) fullrep << yrs_srv7_size(i)<<" "<<osc_srv7_f(i)-esc_srv7_f(i)<<endl;

  fullrep << "srv7_size Males"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_srv7_size;i++) fullrep << yrs_srv7_size(i)<<" "<<osc_srv7_m(i)-esc_srv7_m(i)<<endl;

  fullrep << "N_proj_f "<<endl<<N_proj_f<<endl<<"N_proj_m "<<N_proj_m<<endl<<" spawn_bio next year"<<endl<<spawn_biom_proj(endyr+1)<<endl
  <<"ABC full"<<ABC3<<endl;
  
  fullrep <<" spawn_bio projected"<<endl<<spawn_biom_proj<<endl;
  fullrep << "Specified catch projection"<<(catage_proj_f*wt_f+catage_proj_m*wt_m)<<endl<<"ABC projection: "<<pred_catch_proj<<endl;
  // New SDNR stuff
  fullrep<<" average SDNRs for compositional data"<<endl;
  fullrep<<"Fishery Ages "<<sdnr_fish1_age<<endl;
  fullrep<<"Fishery 1 sizes "<<sdnr_fish1_size<<endl;
  fullrep<<"Fishery 3 sizes "<<sdnr_fish3_size<<endl;
  fullrep<<"Survey 1 ages "<<sdnr_srv1_age<<endl;
  fullrep<<"Survey 2 ages "<<sdnr_srv2_age<<endl;
  fullrep<<"Survey 1 sizes "<<sdnr_srv1_size<<endl;
  fullrep<<"Survey 2 sizes "<<sdnr_srv2_size<<endl;
  fullrep<<"Survey 7 sizes "<<sdnr_srv7_size<<endl;

RUNTIME_SECTION
  convergence_criteria 1.e-4 1.e-7  
  maximum_function_evaluations 200,1000

TOP_OF_MAIN_SECTION
 gradient_structure::set_MAX_NVAR_OFFSET(1000);
   gradient_structure::set_NUM_DEPENDENT_VARIABLES(1000);
   gradient_structure::set_GRADSTACK_BUFFER_SIZE(1000000);
   gradient_structure::set_CMPDIF_BUFFER_SIZE(10000000);
  arrmblsize=390000;

FINAL_SECTION
 write_fullrep();