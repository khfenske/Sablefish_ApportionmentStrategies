//==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+
//
//  Simple 1 area Statistical, separable age-structured population model for sablefish
//  Alaska Fisheries Science Center, September2014
//  original code by D. Hanselman:dana.hanselman@noaa.gov
//  modified by KHfenske: khfenske@alaska.edu
//  Input file:   tem.dat, as in tem(plate)
//  Control file: tem.ctl
//  Program file: tem.tpl
//  Output files: tem.rep, tem.std, proj.dat, newproj.dat
//  Sablefish model 
//  separate sexes; has sex-specific weight at ages
//  srv3= Domestic longline survey RPN 1990-endyr
//  srv4= US-JP LL survey RPN 1979-1994
//  srv5= Domestic LL fishery CPUE 1990-(endyr-1) 
//  srv6= JP LL fishery RPW 1964-1981
//  fish1= U.S. Longline fishery, catch data for 1960-endyr (or are the early years data fish2?)
//  fish3= US trawl fishery, catch data for 1963-endyr
//  fish1 and srv1 age comps (1999-(endyr-1) and 1996-(endyr-1))
//  srv2/4 age comps 1981-1993, odd years (which is it - 2 or 4?)
//  fish1, fish3, srv1, srv2 length comps [srv7 read in but not used, no 2001 EG]
//  1 regions - bsaiwgcgeg

//==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+
DATA_SECTION
  !!CLASS ofstream evalout("evalout.prj");   
// Read data from the control file
  !! ad_comm::change_datafile_name("tem.ctl");    // Read in phases, penalties and priors from "tem.ctl"
  !! *(ad_comm::global_datafile) >>  model_name; 
  !! *(ad_comm::global_datafile) >>  data_file;   // identifying the data file name, using the name in row 2 of .ctl file
  !! *(ad_comm::global_datafile) >>  unchanging_data; // indentifying the name of the data file that holds size comps and other data that's not being updated in simulations
  init_int    nareas                // number of areas in the MODEL 
  init_int    SrType                // 2=mean
  init_int    styr_rec_est
  init_int    endyr_rec_est
  int nrecs_est;
  !! nrecs_est = endyr_rec_est-styr_rec_est+1;
  init_int    rec_like_type         // Type of likelihood specified 

// Phases that general parameter estimation begins
  init_int    ph_Fdev               // Phase for fishing mortality deviations
  init_int    ph_avg_F              // Phase for estimating average fishing mortality
  init_int    ph_recdev             // Phase for estimating recruitment deviations
  init_int    ph_fish_sel           // Phase for estimating fishing selectivity
  init_int    ph_fish4_sel
  init_int    ph_srv1_sel           // Phase for estimating survey selectivity

// Priors
  init_number mprior                // Prior mean for natural mortality
  init_number cvmprior              // Prior CV for natural mortality
  init_int    ph_m                  // Phase for estimating natural mortality

  init_number sigrprior             // Prior mean for recruitment deviations
  init_number cvsigrprior           // Prior CV for recruitment deviations
  init_int    ph_sigr               // Phase for recruiment deviations

  init_number q_srv1prior           // Prior mean for catchability coefficient
  init_number cvq_srv1prior         // Prior CV for catchability coefficient
  init_int    ph_q_srv1             // Phase for estimating catchability

  init_number q_srv2prior           // Prior mean for catchability coefficient
  init_number cvq_srv2prior         // Prior CV for catchability coefficient
  init_int    ph_q_srv2             // Phase for estimating catchability

  init_number q_srv5prior           // Prior mean for catchability coefficient
  init_number cvq_srv5prior         // Prior CV for catchability coefficient
  init_int    ph_q_srv5             // Phase for estimating catchability

  init_number q_srv6prior           // Prior mean for catchability coefficient
  init_number cvq_srv6prior         // Prior CV for catchability coefficient
  init_int    ph_q_srv6             // Phase for estimating catchability
  
  init_number q_srv8prior           // Prior mean for catchability coefficient
  init_number cvq_srv8prior         // Prior CV for catchability coefficient
  init_int    ph_q_srv8             // Phase for estimating catchability
 
  init_number wt_ssqcatch_fish1      // Weight for catch estimation
  init_number wt_ssqcatch_fish3      // Weight for catch estimation
  init_number wt_srv3                // Weight for survey biomass estimation
  init_number wt_srv4                // Weight for survey biomass estimation
  init_number wt_srv5                // Weight for survey biomass estimation  
  init_number wt_srv6                // Weight for survey biomass estimation
  init_number wt_fish1_age           // Weight for fishery age compositions
  init_number wt_srv1_age            // Weight for survey age compositions
  init_number wt_srv2_age            // Weight for survey age compositions
  init_number wt_fish1_size          // Weight for fishery size compositions
  init_number wt_srv1_size           // Weight for survey size compostiions
  init_number wt_fish2_size          // Weight for fishery size compositions
  init_number wt_srv2_size           // Weight for survey size compostiions
  init_number wt_fish3_size          // Weight for fishery size compositions
  init_number wt_fish4_size          // Weight for fishery size compositions
  init_number wt_srv5_size           // Weight for survey size compostiions
  init_number wt_fish6_size          // Weight for fishery size compositions
  init_number wt_srv6_size           // Weight for survey size compostiions

  init_number wt_rec_var             // Weight for estimation recruitment variations penalty
  init_number wt_sel_reg_fish3       // Weight on fishery selectivity regularity penalty
  init_number wt_sel_dome_fish3      // Weight on fishery selectivity dome-shape penalty   
  init_number wt_fmort_reg           // Weight on fishing mortality regularity
  
  init_number hist_hal_prop;         // additional data for BS flag
  init_number ph_ifq;
  init_number ph_srv2q;  
  init_number yieldratio;
  init_int projyrs;

  !! cout << yieldratio << endl;
     !! ad_comm::change_datafile_name(data_file);    // Read data from the data file

// Start and end years, recruitment age, number of age and length bins
  init_int      styr
  init_int      endyr
  init_int      recage
  init_int      nages
  init_int      nlenbins
  init_vector   len_bin_labels(1,nlenbins)  
  
  int styr_rec
  //int styr_sp
  //int endyr_sp
  int endyr_rec
  int nyrs
  !!  nyrs = endyr - styr + 1;
  !!  styr_rec = (styr - nages) + 1;       // First year of recruitment
  //!! styr_sp  = styr_rec - recage ;      // First year of spawning biomass  
  //!! endyr_sp = endyr   - recage - 1;    // endyr year of (main) spawning biomass
  !! endyr_rec= endyr_rec_est;           
  vector yy(styr,endyr);
  !! yy.fill_seqadd(styr,1);
  vector aa(1,nages);
  !! aa.fill_seqadd(recage,1);
   int ph_F40;
  !! ph_F40 = 5;

  init_number spawn_fract;                // Spawning Month
  !! spawn_fract = (spawn_fract - 1) / 12;

// Natural mortality, proportion mature and weight at age
  init_vector   p_mature(1,nages)            
  init_vector   wt_m(1,nages)
  init_vector   wt_f(1,nages)
  vector wt_mature(1,nages)                 
  !! wt_mature = elem_prod(wt_f,p_mature);
  !! cout << wt_f << endl;

// Observed fixed gear (fish1) and trawl gear (fish 3) catches
  init_int      nyrs_obscatch                // NOT USED, COULD REMOVE (but would also need to remove from data file maker)
  init_vector   obs_catch_fish1(styr,endyr)  // reading in the data for the three areas of fixed gear catches, observed catches from US fixed gear fishery for BSAIWG(1), CG(2), EG(3)
  
//fish3 -trawl catches are input but not currently used in model fitting!
  init_vector   obs_catch_fish3(styr,endyr)  //observed catches from US trawl fishery for BSAIWG, CG, EG 

// Survey biomass estimates RPN
  init_number   nyrs_srv3                      // number of years of survey biomass estimates
  init_ivector  yrs_srv3(1,nyrs_srv3)          // years survey conducted, indexed using years in BSAIWG                                                              
  init_vector   obs_srv3_biom(1,nyrs_srv3)     // USING BSAIWG FOR INDEXING! mean estimate of biomass RPW (bsaiwg, cg, eg in that order)
  init_vector   obs_srv3_se(1,nyrs_srv3)       // standard error of survey biomass estimates
  init_vector   obs_srv3_lci(1,nyrs_srv3)      // lower confidence interval, for graphing not used in estimation
  init_vector   obs_srv3_uci(1,nyrs_srv3)      // upper confidence interval

// US-JP coop LL survey RPN - srv4, 1979-1994
  init_int      nyrs_srv4                       // number of years of survey biomass estimates
  init_ivector  yrs_srv4(1,nyrs_srv4)           // years survey conducted in
  init_vector   obs_srv4_biom(1,nyrs_srv4)      // mean estimate of biomass
  init_vector   obs_srv4_se(1,nyrs_srv4)        // standard error of survey biomass estimates
  init_vector   obs_srv4_lci(1,nyrs_srv4)       // lower confidence interval, for graphing not used in estimation
  init_vector   obs_srv4_uci(1,nyrs_srv4)       // upper confidence interval
  
// Domestic LL fishery CPUE RPW - srv5, 1990-(endyr-1)
  init_int      nyrs_srv5                       // number of years of survey abundance estimates
  init_ivector  yrs_srv5(1,nyrs_srv5)           // years survey conducted in
  init_vector   obs_srv5_biom(1,nyrs_srv5)      // mean estimate of biomass
  init_vector   obs_srv5_se(1,nyrs_srv5)        // standard error of survey abundance estimates
  init_vector   obs_srv5_lci(1,nyrs_srv5)       // lower confidence interval, for graphing not used in estimation
  init_vector   obs_srv5_uci(1,nyrs_srv5)       // upper confidence interval    
  !! cout << nyrs_srv5 << endl;
// JP LL fishery RPW - srv6, 1964-1981
  init_int      nyrs_srv6                       // number of years of survey abundance estimates
  init_ivector  yrs_srv6(1,nyrs_srv6)           // years survey conducted in
  init_vector   obs_srv6_biom(1,nyrs_srv6)      // mean estimate of biomass
  init_vector   obs_srv6_se(1,nyrs_srv6)        // standard error of survey abundance estimates
  init_vector   obs_srv6_lci(1,nyrs_srv6)       // lower confidence interval, for graphing not used in estimation
  init_vector   obs_srv6_uci(1,nyrs_srv6)       // upper confidence interval    

// Domestic LL Fishery age composition data - males and females combined
  init_int      nyrs_fish1_age                        // number of Years of fishery age comps, indexed using bsaiwg
  init_ivector  yrs_fish1_age(1,nyrs_fish1_age)       // the years of age comps, indexed using BSAIWG
  init_vector   nsamples_fish1_age(1,nyrs_fish1_age)  // some measure of relative sample size for each age comp.
  init_matrix   oac_fish1(1,nyrs_fish1_age,1,nages)   // the actual year by year age comps for each area (bsaiwg=1, cg=2, eg=3)
  
// Domestic LL Survey age composition data - males and females combined
  init_int      nyrs_srv1_age                       // number of years of survey age compositions
  init_ivector  yrs_srv1_age(1,nyrs_srv1_age)       // the years of survey age comps
  init_vector   nsamples_srv1_age(1,nyrs_srv1_age)  // some measure of relative sample size for each age comp.
  init_matrix   oac_srv1(1,nyrs_srv1_age,1,nages)   // the year by year age survey age comps
 
// US-JP coop LL Survey age composition data - males and females combined
  //init_int      nyrs_srv2_age                       // number of years of survey age compositions
  //init_ivector  yrs_srv2_age(1,nyrs_srv2_age)       // the years of survey age comps
  //init_vector   nsamples_srv2_age(1,nyrs_srv2_age)  // some measure of relative sample size for each age comp.
  //init_matrix   oac_srv2(1,nyrs_srv2_age,1,nages)   // the year by year age survey age comps

  !! ad_comm::change_datafile_name(unchanging_data);    // Read in size comps, size-age, things that aren't being updated
 
// Domestic LL fishery length composition data - sexes separate 1990-(endyr-1)
// in this and in srv1 length comp below, there is an assumption that sample sizes can vary by area, but are the same between sexes for a given area.  
  init_int      nyrs_fish1_size                                // number of years of fish1ery size comps
  init_ivector  yrs_fish1_size(1,nyrs_fish1_size)              // the years of fish1ery size comps
  init_vector   nsamples_fish1_size(1,nyrs_fish1_size)         // some measure of relative sample size for each fish1ery comp
  init_matrix   osc_fish1_m(1,nyrs_fish1_size,1,nlenbins)      // year by year fishery size comps
  init_matrix   osc_fish1_f(1,nyrs_fish1_size,1,nlenbins)      // year by year fishery size comps  

// US trawl gear fishery length comps by sex
  init_int      nyrs_fish3_size                                // number of years of fish1ery size comps
  init_ivector  yrs_fish3_size(1,nyrs_fish3_size)              // the years of fish1ery size comps
  init_vector   nsamples_fish3_size(1,nyrs_fish3_size)         // some measure of relative sample size for each fish1ery comp
  init_matrix   osc_fish3_m(1,nyrs_fish3_size,1,nlenbins)      // year by year fishery size comps
  init_matrix   osc_fish3_f(1,nyrs_fish3_size,1,nlenbins)      // year by year fishery size comps  

// Domestic LL survey length frequencies 1990-(endyr)
  init_int      nyrs_srv1_size                                   // number of years of survey size comps
  init_ivector  yrs_srv1_size(1,nyrs_srv1_size)                  // the years of survey size comps
  init_vector   nsamples_srv1_size(1,nyrs_srv1_size)             // some measure of relative sample size for each survey size comp
  init_matrix   osc_srv1_m(1,nyrs_srv1_size,1,nlenbins)          // year by year size comps
  init_matrix   osc_srv1_f(1,nyrs_srv1_size,1,nlenbins)          // year by year size comps

// US-JP coop LL survey length comps
  init_int      nyrs_srv2_size                                   // number of years of survey size comps
  init_ivector  yrs_srv2_size(1,nyrs_srv2_size)                  // the years of survey size comps
  init_vector   nsamples_srv2_size(1,nyrs_srv2_size)             // some measure of relative sample size for each survey size comp
  init_matrix   osc_srv2_m(1,nyrs_srv2_size,1,nlenbins)          // year by year size comps
  init_matrix   osc_srv2_f(1,nyrs_srv2_size,1,nlenbins)          // year by year size comps
  
//int phase_logist_fsh2   //THIS IS TURNED OFF WHERE IT IS BEING USED 

// Size-age transition matrix:  proportion at size given age
  init_matrix   sizeage_m(1,nages,1,nlenbins)     //size comp #1
  init_matrix   sizeage_f(1,nages,1,nlenbins)     //lets you add another size-age matrix, remove this here and in Get_Predicted section to use only one, or just have two identical
  init_matrix   sizeage_all(1,nages,1,nlenbins)   //lets you add another size-age matrix, remove this here and in Get_Predicted section to use only one, or just have two identical
  init_matrix   sizeage_m_new(1,nages,1,nlenbins) //size comp #1
  init_matrix   sizeage_f_new(1,nages,1,nlenbins) //lets you add another size-age matrix, remove this here and in Get_Predicted section to use only one, or just have two identical
 
// Ageing error transition matrix:  proportion at reader age given true age
  init_matrix   ageage(1,nages,1,nages)				// ageing error matrix, same for both sexes (not sex-specific)
  init_number   eof

     !! cout<<"The universal answer is "<<eof;

// Initialize some counting variables
  int iyr
  int i
  int j
  int ii
  int l
  int k
  int itmp
  vector offset(1,16);   // Multinomial "offset" for each area and a dimension for each multinomial set of data 
  
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

  // Calculate "offset" for multinomials - survey age (=,2), fishery age(=,1)
  // "Offset" value lets the multinomial likelihood equal zero when the observed and
  // predicted are equal as in Fournier (1990) "robustifies"
  // First step is to ensure that the data are expressed as proportions   
  for (i=1; i<=nyrs_fish1_age; i++) {
  oac_fish1(i)/=sum(oac_fish1(i));
  offset(1) -= nsamples_fish1_age(i) *((oac_fish1(i) + 0.001)*log(oac_fish1(i) + 0.001)); }
  
  for (i=1; i<=nyrs_srv1_age; i++)  {
  oac_srv1(i)/=sum(oac_srv1(i));
  offset(2) -= nsamples_srv1_age(i)*((oac_srv1(i) + 0.001)*log(oac_srv1(i) + 0.001));  }

  //for (i=1; i<=nyrs_srv2_age; i++) {
  //oac_srv2(i)/=sum(oac_srv2(i));
  //offset(3)-= nsamples_srv2_age(i) *((oac_srv2(i) + 0.001)*log(oac_srv2(i)+0.001));}

   for (i=1; i<=nyrs_fish1_size; i++)  {
   osc_fish1_f(i)/=sum(osc_fish1_f(i));
   offset(4) -= nsamples_fish1_size(i)*((osc_fish1_f(i) + 0.001)*log(osc_fish1_f(i) + 0.001));
   osc_fish1_m(i)/=sum(osc_fish1_m(i));
   offset(5) -= nsamples_fish1_size(i)*((osc_fish1_m(i) + 0.001)*log(osc_fish1_m(i) + 0.001)); }

   for (i=1; i<=nyrs_fish3_size; i++) {
   osc_fish3_f(i)/=sum(osc_fish3_f(i));
   offset(6) -= nsamples_fish3_size(i)*((osc_fish3_f(i) + 0.001)*log(osc_fish3_f(i) + 0.001)); 
   osc_fish3_m(i)/=sum(osc_fish3_m(i));
   offset(7) -= nsamples_fish3_size(i)*((osc_fish3_m(i) + 0.001)*log(osc_fish3_m(i) + 0.001)); }

   for (i=1; i<=nyrs_srv1_size; i++)  {
   osc_srv1_f(i)/=sum(osc_srv1_f(i));
   offset(9) -= nsamples_srv1_size(i)*((osc_srv1_f(i) + 0.001)*log(osc_srv1_f(i) + 0.001));
   osc_srv1_m(i)/=sum(osc_srv1_m(i));
   offset(10) -= nsamples_srv1_size(i)*((osc_srv1_m(i) + 0.001)*log(osc_srv1_m(i) + 0.001)); }

   for (i=1; i<=nyrs_srv2_size; i++) {
   osc_srv2_f(i)/=sum(osc_srv2_f(i));
   offset(11)-=nsamples_srv2_size(i)*((osc_srv2_f(i) + 0.001)*log(osc_srv2_f(i) + 0.001));
   osc_srv2_m(i)/=sum(osc_srv2_m(i));
   offset(12)-=nsamples_srv2_size(i)*((osc_srv2_m(i) + 0.001)*log(osc_srv2_m(i) + 0.001));} 

 END_CALCS
 

INITIALIZATION_SECTION
 //log_mean_rec       2.40524770675
 sigr               sigrprior
 //log_avg_F_fish1   -2.56764486068
 logm              -2.30258509299    
 log_q_srv1         2.02423694283   
 log_q_srv2         1.85996498559
 log_q_srv5          1.44478842836
 log_q_srv6          2.62659387749
 log_q_srv8          1.77769087985
 //log_q_srv9          1.64372393525
 
 log_a50_fish1_f    0.968317625958
 //log_delta_fish1_f  5.00501076879 //the deltas for fish1 M and F are fixed to the vlues from fish4, so these aren't needed
 log_a50_fish1_m    1.99039715705   
 //log_delta_fish1_m  5.00501076879

 log_a50_fish2       2.1  // values from DHH single area model 
 log_delta_fish2     0.69874  // mean of fish4 M and F delta from s124
 
 log_a50_fish3_f    1.78651832048
 log_delta_fish3_f  1.92684143177
 //log_a50_fish3_m    -37.0500225497  //fixed at F a50
 log_delta_fish3_m  2.30157239486

 log_a50_fish4_f    1.13279115361  
 log_delta_fish4_f  0.723340585591
 log_a50_fish4_m    1.15627997343
 log_delta_fish4_m  0.945517826919

 log_a50_srv1_f     1.00000002481 
 log_delta_srv1_f   1.75000000000
 log_a50_srv1_m     1.00000002481 
 log_delta_srv1_m   1.55000000000    
 
  //log_a50_srv2_f     1.32467214925
  //log_a50_srv2_m     1.17546581443
  //log_delta_srv2_m   0.565744058481  //DHH HAS NO delta_srv2_f, why not? ONLY FITTING ONE AND USING IT FOR BOTH!
  log_a50_srv2_f     0.956710000
  //log_a50_srv2_m     1.303000000  //is fixed at F a50
  log_delta_srv2_m   0.824300000
   

 PARAMETER_SECTION
 
  // Key parameters
  init_number          	log_q_srv1(ph_q_srv1);      // Estimate Log survey catchability
  init_number           log_q_srv2(ph_q_srv2);
  init_number           log_q_srv5(ph_q_srv5);      // Estimate Log survey catchability
  init_number           log_q_srv6(ph_q_srv6);      // Estimate Log survey catchability
  init_number           log_q_srv8(ph_q_srv8);      // Estimate Log survey catchability
  init_bounded_number   logm(-4,-1,ph_m);	          // Estimate log natural mortality area 1
  init_bounded_number  	log_mean_rec(-10,5,1);       // Unfish equil recruitment (logged)
  init_bounded_number  	sigr(0.1,2,ph_sigr);        // Recruitment sdev parameter

  // Fishery selectivity (sex-specific, females first then males)  COULD TRY SPATIAL FISHERY SELECTIVITY
  init_bounded_number   log_a50_fish1_f(-5,5,ph_fish_sel);        // age at 50% selection                                                   
  init_bounded_number 	log_delta_fish1_f(-10,10,-ph_fish_sel);   // age between 50% selection and 95% selection....
  number   		          a50_fish1_f; 										              // age at 50% selection                                                   
  number 		            delta_fish1_f; 										            // age between 50% selection and 95% selection....
  vector                fish1_sel_f(1,nages);			                    // vectory of fishery selectivty parameters on arithmetic scale

  init_bounded_number   log_a50_fish1_m(-5,5,ph_fish_sel);        // age at 50% selection                                                   
  init_bounded_number   log_delta_fish1_m(-10,10,-ph_fish_sel);  // age between 50% selection and 95% selection....
  number                a50_fish1_m;                         // age at 50% selection                                                   
  number                delta_fish1_m;                       // age between 50% selection and 95% selection....
  vector                fish1_sel_m(1,nages);                // vectory of fishery selectivty parameters on arithmetic scale

  init_bounded_number   log_a50_fish2(-10,10,-1);            // age at 50% selection                                                   
  init_bounded_number   log_delta_fish2(-10,10,-1);          // age between 50% selection and 95% selection....
  number                a50_fish2;                      // age at 50% selection                                                   
  number                delta_fish2;                    // age between 50% selection and 95% selection....
  vector                fish2_sel(1,nages);             // vectory of fishery selectivty parameters on arithmetic scale

  init_bounded_number   log_a50_fish3_f(-4,4,4);        // age at 50% selection                                                   
  init_bounded_number   log_delta_fish3_f(-4,4,4);      // age between 50% selection and 95% selection....
  number                a50_fish3_f;                    // age at 50% selection                                                   
  number                delta_fish3_f;                  // age between 50% selection and 95% selection....
  vector                log_fish3_sel_f(1,nages);       // vector of fishery selectivy log parameters including those not estimated
  vector                fish3_sel_f(1,nages);           // vectory of fishery selectivty parameters on arithmetic scale
 
  init_bounded_number   log_a50_fish3_m(-40,40,-4);     // age at 50% selection                                                   
  init_bounded_number   log_delta_fish3_m(-40,40,-4);   // age between 50% selection and 95% selection....
  number                a50_fish3_m;                    // age at 50% selection                                                   
  number                delta_fish3_m;                  // age between 50% selection and 95% selection....
  vector                log_fish3_sel_m(1,nages);               // vector of fishery selectivy log parameters including those not estimated
  vector                fish3_sel_m(1,nages);                   // vectory of fishery selectivty parameters on arithmetic scale

  init_bounded_number   log_a50_fish4_f(0.1,3,ph_fish4_sel);    // age at 50% selection                                                   
  init_bounded_number   log_delta_fish4_f(-1,6,ph_fish4_sel);   // age between 50% selection and 95% selection....
  number                a50_fish4_f;                    // age at 50% selection                                                   
  number                delta_fish4_f;                  // age between 50% selection and 95% selection....
  vector                fish4_sel_f(1,nages);           // vectory of fishery selectivty parameters on arithmetic scale

  init_bounded_number   log_a50_fish4_m(0.1,3,ph_fish4_sel);       // age at 50% selection                                                   
  init_bounded_number   log_delta_fish4_m(0.1,6,ph_fish4_sel);     // age between 50% selection and 95% selection....
  number                a50_fish4_m;                    // age at 50% selection                                                   
  number                delta_fish4_m;                  // age between 50% selection and 95% selection....
  vector                fish4_sel_m(1,nages);           // vectory of fishery selectivty parameters on arithmetic scale

  // Survey selectivities (sex-specific, females first then males)
  init_bounded_number   log_a50_srv1_f(-0.5,3.5,ph_srv1_sel);     // age at 50% selection                                                   
  init_bounded_number   log_delta_srv1_f(-0.5,3.5,-ph_srv1_sel);  // khf turned off // age between 50% selection and 95% selection....
  number                a50_srv1_f;                          // age at 50% selection                                                   
  number                delta_srv1_f;                        // age between 50% selection and 95% selection....
  vector                log_srv1_sel_f(1,nages);             // NOT USED ANYWHERE vector of survey selectivy log parameters including those not estimated
  vector                srv1_sel_f(1,nages);                 // vectory of survey selectivty parameters on arithmetic scale

  init_bounded_number   log_a50_srv1_m(-0.5,3.5,ph_srv1_sel);    // age at 50% selection                                                   
  init_bounded_number   log_delta_srv1_m(-0.5,3.5,-ph_srv1_sel); // khf turned off// age between 50% selection and 95% selection....
  number                a50_srv1_m;                          // age at 50% selection                                                   
  number                delta_srv1_m;                        // age between 50% selection and 95% selection....
  vector                log_srv1_sel_m(1,nages);             // NOT USED ANYWHERE vector of survey selectivy log parameters including those not estimated
  vector                srv1_sel_m(1,nages);                 // vectory of survey selectivty parameters on arithmetic scale

  init_bounded_number   log_a50_srv2_f(-2,3,-1);             // age at 50% selection                                                   
  init_bounded_number   log_delta_srv2_f(-2,4,-1);           // age between 50% selection and 95% selection....
  number                a50_srv2_f;                          // age at 50% selection                                                   
  number                delta_srv2_f;                        // age between 50% selection and 95% selection....
  vector                log_srv2_sel_f(1,nages);             // vector of survey selectivy log parameters including those not estimated
  vector                srv2_sel_f(1,nages);                 // vectory of survey selectivty parameters on arithmetic scale

  init_bounded_number   log_a50_srv2_m(0.5,3.5,-1);          // age at 50% selection                   #DH# turned off because it seemed to be estimating poorly                                 
  init_bounded_number   log_delta_srv2_m(-2,10,-1);          // age between 50% selection and 95% selection....
  number                a50_srv2_m;                          // age at 50% selection                                                   
  number                delta_srv2_m;                        // age between 50% selection and 95% selection....
  vector                log_srv2_sel_m(1,nages);             // vector of survey selectivy log parameters including those not estimated
  vector                srv2_sel_m(1,nages);                 // vectory of survey selectivty parameters on arithmetic scale

  // Fishing mortality
  init_bounded_number   log_avg_F_fish1(-10,0,ph_avg_F);               // Log average fishing mortality
  init_bounded_vector  	log_F_devs_fish1(styr,endyr,-10,10,ph_Fdev);   // Annual, spatial fishing mortality deviations
  init_bounded_number   log_avg_F_fish3(-10,10,ph_avg_F);
  init_bounded_vector   log_F_devs_fish3(styr,endyr,-10,10,ph_Fdev);
  vector               	Fmort_fish1(styr,endyr);            // Fishing mortality by year and area
  vector                Fmort_fish3(styr,endyr);            // Fishing mortality by year and area
  matrix              	Z_f(styr,endyr,1,nages);            // Total female mortality by year and age and area
  matrix                Z_m(styr,endyr,1,nages);            // Total male mortality by year and age and area
  matrix              	F_fish1_f(styr,endyr,1,nages);      // Fishing female mortality by year and age and area
  matrix                F_fish1_m(styr,endyr,1,nages);      // Fishing male mortality by year and age and area 
  matrix                F_fish3_f(styr,endyr,1,nages);      // Fishing female mortality by year and age and area
  matrix                F_fish3_m(styr,endyr,1,nages);      // Fishing male mortality by year and age and area 
  matrix                S_f(styr,endyr,1,nages);            // Survivorship by year and age and area
  matrix                S_m(styr,endyr,1,nages);            // Survivorship by year and age and area
  matrix                S_f_mid(styr,endyr,1,nages);        // Survivorship by year and age and area
  matrix                S_m_mid(styr,endyr,1,nages);        // Survivorship by year and age and area

  // Create a vector of natural mortalities for proj.dat
  //matrix natmortv(1,nages);  NOT USED

// Numbers at age
  init_bounded_dev_vector  	log_rec_dev(styr-nages+2,endyr-1,-10,10,ph_recdev);	// Recruitment deviations from before the asssessment starts to present, by area
  matrix            natage_f(styr,endyr,1,nages);	         // Matrix of numbers at age from start year to end year by area
  matrix            natage_m(styr,endyr,1,nages);          // Matrix of numbers at age from start year to end year by area

// Catch at age
  matrix            catage_fish1_f(styr,endyr,1,nages)    // Matrix of predicted catch at age from start year to endyear by area
  matrix            catage_fish1_m(styr,endyr,1,nages)    // Matrix of predicted catch at age from start year to endyear by area
  vector            pred_catch_fish1(styr,endyr)	        // Vector of predicted catches
  matrix            catage_fish3_f(styr,endyr,1,nages)    // Matrix of predicted catch at age from start year to endyear by area
  matrix            catage_fish3_m(styr,endyr,1,nages)    // Matrix of predicted catch at age from start year to endyear by area
  vector            pred_catch_fish3(styr,endyr)          // Vector of predicted catches

  sdreport_vector   pred_srv3(1,nyrs_srv3);                // Predicted survey
  vector            pred_srv4(1,nyrs_srv4);                // Predicted survey biomass
  sdreport_vector   pred_srv5(1,nyrs_srv5);                // Predicted survey Abundance
  vector            pred_srv6(1,nyrs_srv6);                // Predicted survey Abundance
  //vector            pred_srv8(1,);                       // Predicted survey Abundance
  
  matrix            eac_fish1(1,nyrs_fish1_age,1,nages)   // Expected proportion at age in fish
  matrix            eac_srv1(1,nyrs_srv1_age,1,nages)     // Expected proportion at age in survey
  //matrix            eac_srv2(1,nyrs_srv2_age,1,nages)   // Expected proportion at age in survey
  
  matrix            esc_fish1_m(1,nyrs_fish1_size,1,nlenbins)  // Expected proportion at size in fishery
  matrix            esc_fish1_f(1,nyrs_fish1_size,1,nlenbins)  // Expected proportion at size in fishery
  matrix            esc_fish3_m(1,nyrs_fish3_size,1,nlenbins)  // Expected proportion at size in fishery
  matrix            esc_fish3_f(1,nyrs_fish3_size,1,nlenbins)  // Expected proportion at size in fishery

  matrix            esc_srv1_m(1,nyrs_srv1_size,1,nlenbins)    // Expected proportion at size in survey
  matrix            esc_srv1_f(1,nyrs_srv1_size,1,nlenbins)    // Expected proportion at size in survey
  matrix            esc_srv2_m(1,nyrs_srv2_size,1,nlenbins)    // Expected proportion at size in survey
  matrix            esc_srv2_f(1,nyrs_srv2_size,1,nlenbins)    // Expected proportion at size in survey

// Some calculated variables and standard deviation estimates for some estimated parameters
  sdreport_vector      		tot_biom(styr,endyr);     // Standard deviation report vector of total biomass
  number      		q_srv1;                           // " " for Survey catchability
  number          q_srv2;
  number          q_srv5;
  number          q_srv6;
  number          q_srv8;
  //number          q_srv9;

  sdreport_vector pred_rec(styr,endyr);		  // " " for predicted recruitments  
  number          avg_rec;                  // " " for Average recruitment 
  number          spbiom_trend;
  number          Depletion;                // depletion
  sdreport_vector      		spawn_biom(styr,endyr);   // " " for spawning biomass vector
  number      		natmort;								  // " " for natural mortality

 // Parameters for computing SPR rates 
  init_bounded_number 	mF40(0.01,1.,ph_F40)	// Estimated F40
  init_bounded_number 	mF35(0.01,1.,ph_F40)	// Estimated F35
  
  sdreport_number				F40;							// " " " F40
  number 				F35;											// " " " F35
  number 				SB0												// Spawning biomass at no fishing
  number 				SBF40											// " " at F40
  number 				SBF35											// " " at F35
  number 				sprpen										// Likelihood penalty to make ADMB estimate spr rates
  matrix				Nspr(1,3,1,nages)	        // Matrix of number of spawners at age at each fishing mortality level
  number        hist_hal_F;               // Option of adding historical proportion of current average hook and line catch

// Likelihoods and penalty functions  
  vector				surv_like(1,18);		      // Likelihood values for survey biomasses, allowance for up to 1 survey (srv1 for now)
  vector				age_like(1,16);	          // Likelihood values for age and size compositions allowance for up to 2 comps for now
  //vector        sel_like(1,12);         // NOT USED YET, ALSO NOT SPATIAL SELECTIVITY FOR NOW (OR EVER?)
  number				rec_like;			            // Likelihood value for recruitments
  number				ssqcatch;					        // Likelihood value for catch estimation
  number				F_mort_regularity;	      // Penalty value for fishing mortality regularity
  number				avg_sel_penalty;		      // Penalty value for selectivity regularity penalty

// Priors
  vector				priors(1,13);				// Prior penalty values for sigr,q,natural mortality,steepness  
// Define an objective function
  number				Like;							  // Likelihood for data fits
  objective_function_value obj_fun;	// Total likelihood for objective function value
  // vector			xdum2(styr,endyr);	// Dummy variable for use in pop-report.cxx  NOT USED ANYWHERE
  vector 				pred_catch(styr,endyr);
  number 				fratio;
  // number 		B40;
  // number     ABC3;

///////////////////////////////////////////
/// Population projection Hanselsiginelli
//////////////////////////////////////////

  matrix        N_proj_f(endyr+1,endyr+projyrs,1,nages);  // 16 years of F numbers at age
  matrix        N_proj_m(endyr+1,endyr+projyrs,1,nages);
  number        FABC_proj;  // after control rule F, need to make spatial
  vector        FABC_tot_proj_f(1,nages);  // ?
  vector        FABC_tot_proj_m(1,nages);
  number        FOFL_proj;
  vector        FOFL_tot_proj_f(1,nages);
  vector        FOFL_tot_proj_m(1,nages);
  sdreport_number        ABC;  // these may not work as SD report numbers
  sdreport_number        B40;
  number        OFL;
  vector        Z_proj_f(1,nages);
  vector        Z_proj_m(1,nages);
  vector        ZOFL_proj_f(1,nages);
  vector        ZOFL_proj_m(1,nages);

  vector        S_proj_f(1,nages);
  vector        S_proj_m(1,nages);
  matrix        catage_proj_f(endyr+1,endyr+projyrs,1,nages);
  matrix        catage_proj_m(endyr+1,endyr+projyrs,1,nages);
  matrix        catage_proj_OFL_f(endyr+1,endyr+projyrs,1,nages);
  matrix        catage_proj_OFL_m(endyr+1,endyr+projyrs,1,nages);
  vector        pred_catch_proj_OFL_f(endyr+1,endyr+projyrs);
  vector        pred_catch_proj_OFL_m(endyr+1,endyr+projyrs);
  sdreport_vector        spawn_biom_proj(endyr+1,endyr+projyrs);
  sdreport_vector        tot_biom_proj(endyr+1,endyr+projyrs);
  vector        pred_catch_proj(endyr+1,endyr+projyrs);
  vector        pred_catch_proj_OFL(endyr+1,endyr+projyrs);
  number        stdev_rec;  
  number        FOFL;
  number        FABC;
  number        FOFL2;
  number        FABC2; 

 
 PROCEDURE_SECTION
  l=l+1; // Initiate counter for random seeds in projection
	   Get_Selectivity();										// Call function to get selectivities
     Get_Mortality_Rates();								// Call function to get fishing and natural mortality
     Get_Numbers_At_Age();								// Call function to get numbers at age per year
     Get_Catch_at_Age();									// Call function to get catch at age per year
     Get_Predicted_Values();							// Get predicted values for catch, survbio, age and size comps
     Calc_priors();
     if(last_phase()) {
     Get_Dependent_Vars();								// Solve for dependent variables like total bio, recruitment etc.
     compute_spr_rates();									// Compute f40 etc.
     Get_Population_Projection();  }       
     Evaluate_Objective_Function();				// Minimize objective function value }
    if (mceval_phase())									// For outputting MCMC simulations in text format 
  
    {
     evalout<<log_mean_rec<<" "<<sigr<<" "<<q_srv1<<" "<<q_srv2<<" "<<q_srv5<<" "<<q_srv6<<" "<<q_srv8<<" "<<F40<<" "<<B40<<" "<<natmort<<" "<<obj_fun<<" "<<tot_biom<<" "<<spawn_biom<<" "<<pred_rec<<" "<<spawn_biom_proj<<" "<<pred_catch_proj<<" "<<pred_srv3<<" "<<pred_srv5<<" "<<endl; 
     }
 
FUNCTION Get_Selectivity
//   Fishery selectivity
//   Selectivity does not change for ages greater than n_fish_sel_ages
 
  a50_fish1_f=mfexp(log_a50_fish1_f);           
  a50_fish1_m=mfexp(log_a50_fish1_m);
  delta_fish1_f=mfexp(log_delta_fish4_f);       // age between 50% selection and 95% selection....
  delta_fish1_m=mfexp(log_delta_fish4_m);       // age between 50% selection and 95% selection....

  //a50_fish2=mfexp(log_a50_fish2); 
  //delta_fish2=mfexp((log_delta_fish4_f+log_delta_fish4_m)/2);
  a50_fish2=mfexp(log_a50_fish2); 
  delta_fish2=mfexp(log_delta_fish2);
  
  a50_fish3_f=mfexp(log_a50_fish3_f);
  a50_fish3_m=mfexp(log_a50_fish3_f);  //should this be _m? NO, it's fine as is
  delta_fish3_f=mfexp(log_delta_fish3_f);
  delta_fish3_m=mfexp(log_delta_fish3_m);
  
  a50_fish4_f=mfexp(log_a50_fish4_f);
  a50_fish4_m=mfexp(log_a50_fish4_m);
  delta_fish4_f=mfexp(log_delta_fish4_f); 
  delta_fish4_m=mfexp(log_delta_fish4_m);
  
  a50_srv1_f=mfexp(log_a50_srv1_f);
  a50_srv1_m=mfexp(log_a50_srv1_m);
  delta_srv1_f=mfexp(log_delta_srv1_f); 
  delta_srv1_m=mfexp(log_delta_srv1_m);  
 
  a50_srv2_f=mfexp(log_a50_srv2_f); // dhh only a50_srv2_f is turned on
  a50_srv2_m=mfexp(log_a50_srv2_f); 
  delta_srv2_f=mfexp(log_delta_srv2_m); // dhh only delta_srv2_m is turned on  
  delta_srv2_m=mfexp(log_delta_srv2_m);
 

 // selectivities:
  for (j=1;j<=nages;j++) { // currently, single-area selectivity 
  // logistic (formerly opt 2)
    fish1_sel_f(j)=1/ (1+mfexp(-delta_fish1_f*(j-a50_fish1_f))); 
    fish1_sel_m(j)=1/ (1+mfexp(-delta_fish1_m*(j-a50_fish1_m)));
  // logistic (formerly opt 2)
    fish2_sel(j)=1/ (1+mfexp(-delta_fish2*(double(j)-a50_fish2))); 
  // 2-parameter gamma (formerly opt 3)
  // Punt et. al 1996 gamma parameterization
    fish3_sel_f(j)=(pow(j/a50_fish3_f,a50_fish3_f/(0.5*(sqrt(square(a50_fish3_f)+4*square(delta_fish3_f))-a50_fish3_f)))*mfexp((a50_fish3_f-j)/(0.5*(sqrt(square(a50_fish3_f)+4*square(delta_fish3_f))-a50_fish3_f))));
    fish3_sel_m(j)=(pow(j/a50_fish3_m,a50_fish3_m/(0.5*(sqrt(square(a50_fish3_m)+4*square(delta_fish3_m))-a50_fish3_m)))*mfexp((a50_fish3_m-j)/(0.5*(sqrt(square(a50_fish3_m)+4*square(delta_fish3_m))-a50_fish3_m)))); 
  // logistic (formerly opt 2)
    fish4_sel_f(j)=1/ (1+mfexp(-delta_fish4_f*(j-a50_fish4_f)));  
    fish4_sel_m(j)=1/ (1+mfexp(-delta_fish4_m*(j-a50_fish4_m))); 
  // logistic (formerly opt 2) 
    srv1_sel_f(j)=1/ (1+mfexp(-delta_srv1_f*(j-a50_srv1_f)));
    srv1_sel_m(j)=1/ (1+mfexp(-delta_srv1_m*(j-a50_srv1_m))); 
  // logistic (formerly opt 2) 
    srv2_sel_f(j)=1/ (1+mfexp(-delta_srv2_f*(j-a50_srv2_f)));
    srv2_sel_m(j)=1/ (1+mfexp(-delta_srv2_m*(j-a50_srv2_m))); 
     }
   // fish2_sel=fish2_sel/max(fish2_sel);  DH turned this off
   // fish3_sel_f=fish3_sel_f/max(fish3_sel_f);
   // fish3_sel_m=fish3_sel_m/max(fish3_sel_m);


FUNCTION Get_Mortality_Rates
// Calculate mortality rates, not sex-specific mortality                
  natmort        = exp(logm);   // setting natural mortality to arithmetic scale

  Fmort_fish1    = mfexp(log_avg_F_fish1 + log_F_devs_fish1);		
  Fmort_fish3    = mfexp(log_avg_F_fish3 + log_F_devs_fish3);    
  hist_hal_F = hist_hal_prop*mfexp(log_avg_F_fish1);   // optional historical fishing mortality for initial age comps
  
  for (iyr=styr; iyr<=1994; iyr++) {
   for (k = 1; k<= nages; k++) {
      F_fish1_f(iyr,k) = Fmort_fish1(iyr) * fish1_sel_f(k);			// Getting fully selected FEMALE fishing mortality
			F_fish1_m(iyr,k) = Fmort_fish1(iyr) * fish1_sel_m(k);  		// Getting fully selected MALE fishing mortality
      F_fish3_m(iyr,k) = Fmort_fish3(iyr) * fish3_sel_m(k);
      F_fish3_f(iyr,k) = Fmort_fish3(iyr) * fish3_sel_f(k);       
    }}

  for (iyr=1995;iyr<=endyr;iyr++){
    for (k=1;k<=nages;k++) {
    if(ph_ifq==1) {
      F_fish1_f(iyr,k)=Fmort_fish1(iyr)*fish4_sel_f(k);
      F_fish1_m(iyr,k)=Fmort_fish1(iyr)*fish4_sel_m(k);}
    else {
      F_fish1_f(iyr,k)=Fmort_fish1(iyr)*fish1_sel_f(k);
      F_fish1_m(iyr,k)=Fmort_fish1(iyr)*fish1_sel_m(k);}
      F_fish3_f(iyr,k)=Fmort_fish3(iyr)*fish3_sel_f(k);
      F_fish3_m(iyr,k)=Fmort_fish3(iyr)*fish3_sel_m(k);}}

    Z_f        = F_fish1_f + F_fish3_f + natmort;   // Fully selected total mortality by year and age
    // in DHH model, F_fish1_m also includes mdelta - why?  I'm not doing it here for now. <seems like it's a switch for Male nat mort>
    Z_m        = F_fish1_m + F_fish3_m + natmort;   // Fully selected total mortality by year and age
   
   S_f         = mfexp(-1.0*Z_f);										// Fully selected survival
   S_f_mid     = mfexp(-0.5*Z_f);
   S_m         = mfexp(-1.0*Z_m);                   // Fully selected survival
   S_m_mid     = mfexp(-0.5*Z_m);


// Next two sections are based on Baranov catch equations
FUNCTION Get_Numbers_At_Age  
// Calculate Numbers at age
// only using rec_like_type 2, the rest of the options are not coded in 
// Starting year of the model (not the start of a given calendar year)
     natage_f(styr,1)=mfexp(log_mean_rec+log_rec_dev(styr)+sigr*sigr/2)/2; // this needs the /2 at the end to give half to males, half to females
     natage_m(styr,1)=mfexp(log_mean_rec+log_rec_dev(styr)+sigr*sigr/2)/2;
    
    for (j=2;j<nages;j++) {
     itmp = styr+1-j;       // fill in the rest of the ages, 3 to 30, for the starting year
	    natage_f(styr,j) =(mfexp(log_mean_rec - (natmort+hist_hal_F*fish1_sel_f(j)) * double(j-1)+ log_rec_dev(itmp)+sigr*sigr/2))/2; 
      natage_m(styr,j) =(mfexp(log_mean_rec - (natmort+hist_hal_F*fish1_sel_m(j)) * double(j-1)+ log_rec_dev(itmp)+sigr*sigr/2))/2; 
      }

  // fill in the n for the start year plus group
   	  natage_f(styr,nages) = (mfexp(log_mean_rec - (natmort+hist_hal_F*fish1_sel_f(nages-1)) * (nages-1))/ (1. - exp(-natmort+hist_hal_F*fish1_sel_f(nages-1)) ))/2; 
      natage_m(styr,nages) = (mfexp(log_mean_rec - (natmort+hist_hal_F*fish1_sel_m(nages-1)) * (nages-1))/ (1. - exp(-natmort+hist_hal_F*fish1_sel_m(nages-1)) ))/2; 
     
  //cout<<"LMR "<<log_mean_rec;
  for (i=styr; i<= endyr-1; i++)  // so, for 1960-endyr...
  {
    natage_f(i,1)           = mfexp(log_rec_dev(i) + log_mean_rec+sigr*sigr/2 )/2;    // REDUNDANT TO LINE 500 ABOVE?
    natage_m(i,1)           = mfexp(log_rec_dev(i) + log_mean_rec+sigr*sigr/2 )/2;
    natage_f(i+1)(2,nages)  = ++elem_prod(natage_f(i)(1,nages-1),S_f(i)(1,nages-1));  // Following year
    natage_m(i+1)(2,nages)  = ++elem_prod(natage_m(i)(1,nages-1),S_m(i)(1,nages-1));  // Following year
    natage_f(i+1,nages)    += natage_f(i,nages)*S_f(i,nages);
    natage_m(i+1,nages)    += natage_m(i,nages)*S_m(i,nages);
    spawn_biom(i)  = elem_prod(natage_f(i),pow(S_f(i),spawn_fract)) * wt_mature;      // females only for spawning biomass  
  } 

  // End year  (terminal model year)
	   for(i=endyr;i<endyr;i++){
	    natage_f(i,1)           = mfexp(log_mean_rec+sigr*sigr/2)/2;                        
      natage_m(i,1)           = mfexp(log_mean_rec+sigr*sigr/2)/2;
      natage_f(i+1)(2,nages)  = ++elem_prod(natage_f(i)(1,nages-1),S_f(i)(1,nages-1));       // Following year  
      natage_m(i+1)(2,nages)  = ++elem_prod(natage_m(i)(1,nages-1),S_m(i)(1,nages-1));       // Following year
      natage_f(i+1,nages)    += natage_f(i,nages)*S_f(i,nages);
      natage_m(i+1,nages)    += natage_m(i,nages)*S_m(i,nages);                              
      spawn_biom(i) = elem_prod(natage_f(i),pow(S_f(i),spawn_fract)) * wt_mature;          
  }

  natage_f(endyr,1)  = mfexp(log_mean_rec)/2; 
  natage_m(endyr,1)  = mfexp(log_mean_rec)/2;
  spawn_biom(endyr)  = elem_prod(natage_f(endyr),pow(S_f(endyr),spawn_fract)) * wt_mature; 

 
FUNCTION Get_Catch_at_Age
// Calculate catch at age
  pred_catch_fish1.initialize();  
  pred_catch_fish3.initialize();
    for (iyr=styr; iyr<=endyr; iyr++) {
    catage_fish1_m(iyr) = elem_div(elem_prod(elem_prod(natage_m(iyr),F_fish1_m(iyr)),(1.-S_m(iyr))),Z_m(iyr));
    catage_fish1_f(iyr) = elem_div(elem_prod(elem_prod(natage_f(iyr),F_fish1_f(iyr)),(1.-S_f(iyr))),Z_f(iyr));
    pred_catch_fish1(iyr) = elem_div(elem_prod(elem_prod(natage_m(iyr),F_fish1_m(iyr)),(1.-S_m(iyr))),Z_m(iyr))*wt_m + elem_div(elem_prod(elem_prod(natage_f(iyr),F_fish1_f(iyr)),(1.-S_f(iyr))),Z_f(iyr))*wt_f;
 
    catage_fish3_m(iyr) = elem_div(elem_prod(elem_prod(natage_m(iyr),F_fish3_m(iyr)),(1.-S_m(iyr))),Z_m(iyr));
    catage_fish3_f(iyr) = elem_div(elem_prod(elem_prod(natage_f(iyr),F_fish3_f(iyr)),(1.-S_f(iyr))),Z_f(iyr));
    pred_catch_fish3(iyr) = elem_div(elem_prod(elem_prod(natage_m(iyr),F_fish3_m(iyr)),(1.-S_m(iyr))),Z_m(iyr))*wt_m + elem_div(elem_prod(elem_prod(natage_f(iyr),F_fish3_f(iyr)),(1.-S_f(iyr))),Z_f(iyr))*wt_f;
    }

FUNCTION Get_Dependent_Vars
    pred_rec.initialize();
    tot_biom.initialize();
    spawn_biom.initialize();
   for (i=styr;i<=endyr;i++)   {
    pred_rec(i) += natage_f(i,1) + natage_m(i,1);  							  // Setting up results based on estimated paramters  
    tot_biom(i) += wt_f * natage_f(i) + natage_m(i) * wt_m;			  // Total biomass results
    spawn_biom(i) += wt_mature*natage_f(i); }  		                // Spawning biomass result
    
    avg_rec      = mean(pred_rec);
    Depletion    = spawn_biom(endyr)/spawn_biom(styr);
    spbiom_trend = spawn_biom(endyr)/spawn_biom(endyr-1);

FUNCTION Get_Predicted_Values
// Calculate predicted data values, currently seems to be estimating a single catchability, not spatial q, but will give a spatial prediction for predicted surv1 biomass
 pred_catch.initialize();
   q_srv1 = exp(log_q_srv1);      // Survey catchability at arithmetic scale     
   q_srv2 = exp(log_q_srv2);
   //q_srv4 = exp(log_q_srv4);   
   q_srv5 = exp(log_q_srv5);        // Survey catchability at arithmetic scale
   q_srv6 = exp(log_q_srv6);        // Survey catchability at arithmetic scale
   q_srv8 = exp(log_q_srv8);        // Survey catchability at arithmetic scale
   //q_srv9 = exp(log_q_srv9);        // Survey catchability at arithmetic scale

  for (i=1;i<=nyrs_srv3;i++) { 
    pred_srv3(i) = 2*(q_srv1 * (1.-0.5)*(elem_prod(S_f_mid(yrs_srv3(i)),natage_f(yrs_srv3(i)))*srv1_sel_f)+q_srv1 *0.5* (elem_prod(S_m_mid(yrs_srv3(i)),natage_m(yrs_srv3(i)))*srv1_sel_m)); }  // Predicted Survey biomass
  for (i=1;i<=nyrs_srv6;i++) {
    pred_srv6(i) = q_srv6 * (elem_prod(S_f_mid(yrs_srv6(i)),natage_f(yrs_srv6(i)))*elem_prod(wt_f,fish2_sel)+ elem_prod(S_m_mid(yrs_srv6(i)),natage_m(yrs_srv6(i)))*elem_prod(wt_m,fish2_sel));  } // Predicted Survey biomass
  for (i=1;i<=nyrs_srv4;i++) {  
    pred_srv4(i) = 2*(q_srv2* (1.-0.5)*(elem_prod(S_f_mid(yrs_srv4(i)),natage_f(yrs_srv4(i)))*srv2_sel_f)+q_srv2 *0.5* (elem_prod(S_m_mid(yrs_srv4(i)),natage_m(yrs_srv4(i)))*srv2_sel_m)); }  // Predicted Survey biomass 
     
  for (i=1;i<=5;i++) pred_srv5(i) = q_srv5 * (elem_prod(S_f_mid(yrs_srv5(i)),natage_f(yrs_srv5(i)))*elem_prod(fish1_sel_f,wt_f))+q_srv5 * (elem_prod(S_m_mid(yrs_srv5(i)),natage_m(yrs_srv5(i)))*elem_prod(fish1_sel_m,wt_m));    // Predicted Survey biomass
  for (i=6;i<=nyrs_srv5;i++) { 
          if(ph_ifq==1) pred_srv5(i) = q_srv8 * (elem_prod(S_f_mid(yrs_srv5(i)),natage_f(yrs_srv5(i)))*elem_prod(fish4_sel_f,wt_f))+q_srv8 * (elem_prod(S_m_mid(yrs_srv5(i)),natage_m(yrs_srv5(i)))*elem_prod(fish4_sel_m,wt_m));     // Predicted Survey biomass
          else pred_srv5(i) = q_srv8 * (elem_prod(S_f_mid(yrs_srv5(i)),natage_f(yrs_srv5(i)))*elem_prod(fish1_sel_f,wt_f))+q_srv8 * (elem_prod(S_m_mid(yrs_srv5(i)),natage_m(yrs_srv5(i)))*elem_prod(fish1_sel_m,wt_m)); }  // Predicted Survey biomass

  for (i=1;i<=nyrs_fish1_age;i++) {
    eac_fish1(i)  = ((catage_fish1_f(yrs_fish1_age(i))/sum(catage_fish1_f(yrs_fish1_age(i)))) + (catage_fish1_m(yrs_fish1_age(i))/sum(catage_fish1_m(yrs_fish1_age(i)))))/2* ageage;      // Predicted Fishery age comps
    eac_fish1(i) /= sum(eac_fish1(i)); }
  
  for (i=1;i<=nyrs_srv1_age;i++) {
   	eac_srv1(i)  = (1.-0.5)*elem_prod(srv1_sel_f,natage_f(yrs_srv1_age(i))) + 0.5*elem_prod(srv1_sel_m,natage_m(yrs_srv1_age(i)))* ageage;       // Predicted Survey age comps
   	eac_srv1(i) /=sum(eac_srv1(i)); }

  //for (i=1;i<=nyrs_srv2_age;i++) {
    //eac_srv2(i)  = (elem_prod(srv2_sel_f,natage_f(yrs_srv2_age(i)))+elem_prod(srv2_sel_m,natage_m(yrs_srv2_age(i))))*ageage;                        // Predicted Survey age comps
    //eac_srv2(i) /=sum(eac_srv2(i)); }

  for (i=1;i<=5;i++) {                      // Lets you use a second matrix for part of it  HUH?
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
   
  for ( i=1;i<=5;i++) {
    esc_srv1_m(i)  = elem_prod(srv1_sel_m,natage_m(yrs_srv1_size(i))) * sizeage_m;        // Predicted Survey size comps (not used in POP model)
    esc_srv1_m(i)  /=sum(esc_srv1_m(i));
    esc_srv1_f(i)  = elem_prod(srv1_sel_f,natage_f(yrs_srv1_size(i))) * sizeage_f;        // Predicted Survey size comps (not used in POP model)
    esc_srv1_f(i)  /=sum(esc_srv1_f(i)); }
  for ( i=6;i<=nyrs_srv1_size;i++) {
    esc_srv1_m(i)  = elem_prod(srv1_sel_m,natage_m(yrs_srv1_size(i))) * sizeage_m_new;        // Predicted Survey size comps (not used in POP model)
    esc_srv1_m(i)  /=sum(esc_srv1_m(i));
    esc_srv1_f(i)  = elem_prod(srv1_sel_f,natage_f(yrs_srv1_size(i))) * sizeage_f_new;        // Predicted Survey size comps (not used in POP model)
    esc_srv1_f(i)  /=sum(esc_srv1_f(i)); }
    
  for ( i=1;i<=nyrs_srv2_size;i++) {
    esc_srv2_m(i)  = elem_prod(srv2_sel_m,natage_m(yrs_srv2_size(i)))*  sizeage_m;        // Predicted Survey size comps (not used in POP model)
    esc_srv2_m(i)  /=sum(esc_srv2_m(i)); 
    esc_srv2_f(i)  = elem_prod(srv2_sel_f,natage_f(yrs_srv2_size(i))) * sizeage_f;        // Predicted Survey size comps (not used in POP model)
    esc_srv2_f(i)  /=sum(esc_srv2_f(i)); }
      
   pred_catch += (pred_catch_fish1 + pred_catch_fish3);  

FUNCTION compute_spr_rates
  //Compute SPR Rates and add them to the likelihood for Females 
  fratio = Fmort_fish1(endyr)/(Fmort_fish1(endyr)+Fmort_fish3(endyr)); 
  // Scale F-spr rates to be on full-selected values
  F40  = mF40*max(fish4_sel_f);
  F35  = mF35*max(fish4_sel_f);
  SB0 =0;
  SBF40=0;
  SBF35=0;
    for (i=1;i<=3;i++) {
    Nspr(i,1)=1.;
    }
  
   for (j=2;j<nages;j++)
    {
    Nspr(1,j)=Nspr(1,j-1)*mfexp(-1.*natmort);
    Nspr(2,j)=Nspr(2,j-1)*mfexp(-1.*(natmort+fratio*mF40*fish4_sel_f(j-1)+(1-fratio)*mF40*fish3_sel_f(j-1)));
    Nspr(3,j)=Nspr(3,j-1)*mfexp(-1.*(natmort+fratio*mF35*fish4_sel_f(j-1)+(1-fratio)*mF35*fish3_sel_f(j-1)));
    }

  Nspr(1,nages)=Nspr(1,nages-1)*mfexp(-1.*natmort)/(1.-mfexp(-1.*natmort));
  Nspr(2,nages)=Nspr(2,nages-1)*mfexp(-1.* (natmort+fratio*mF40*fish4_sel_f(nages-1)+(1-fratio)*mF40*fish3_sel_f(nages-1)))/ (1.-mfexp(-1.*(natmort+fratio*mF40*fish1_sel_f(nages)+(1-fratio)*mF40*fish3_sel_f(nages))));
  Nspr(3,nages)=Nspr(3,nages-1)*mfexp(-1.* (natmort+fratio*mF35*fish4_sel_f(nages-1)+(1-fratio)*mF40*fish3_sel_f(nages-1)))/ (1.-mfexp(-1.*(natmort+fratio*mF35*fish1_sel_f(nages)+(1-fratio)*mF40*fish3_sel_f(nages))));

   for (j=1;j<=nages;j++)
   {
   // Kill them off till (spawn_fract)
    SB0    += Nspr(1,j)*wt_mature(j)*mfexp(-spawn_fract*natmort);
    SBF40  += Nspr(2,j)*wt_mature(j)*mfexp(-spawn_fract*(natmort+fratio*mF40*fish4_sel_f(j)+(1-fratio)*mF40*fish3_sel_f(j)));
    SBF35  += Nspr(3,j)*wt_mature(j)*mfexp(-spawn_fract*(natmort+fratio*mF35*fish4_sel_f(j)+(1-fratio)*mF35*fish3_sel_f(j)));
   } 
  
  sprpen   = 100.*square(SBF40/SB0-0.4);
  sprpen   += 100.*square(SBF35/SB0-0.35);
  B40= 0.5*SBF40*mean(pred_rec(1979,endyr-recage-1));
  
FUNCTION Calc_priors
// Calculate priors
    priors.initialize(); //priors.initialize sets it all equal to zero
    if (active(sigr))
      priors(1)    = square(log((sigr/sigrprior)))/(2.*square(cvsigrprior));
    if (active(log_q_srv1))
      priors(2)    = square(log_q_srv1-log(q_srv1prior))/(2.*square(cvq_srv1prior));
    if (active(logm))
      priors(4)    = square(logm-log(mprior))/(2.*square(cvmprior));
    if (active(log_q_srv2))  
      priors(7)    = square(log_q_srv2-log(q_srv2prior))/(2.*square(cvq_srv2prior));
    if (active(log_q_srv5))
      priors(9)    = square(log_q_srv5-log(q_srv5prior))/(2.*square(cvq_srv5prior));
    if (active(log_q_srv6))
      priors(10)    = square(log_q_srv6-log(q_srv6prior))/(2.*square(cvq_srv6prior));
    if (active(log_q_srv8))
      priors(11)    = square(log_q_srv8-log(q_srv8prior))/(2.*square(cvq_srv8prior));

FUNCTION Surv_Likelihood
 // Calculate likelihood for survey biomass
  surv_like.initialize();
    for (i=1; i<=nyrs_srv3; i++)  {  
     surv_like(1) += square((log(obs_srv3_biom(i)+0.0001)-log(pred_srv3(i)+0.0001) ))/ (2.*square(obs_srv3_se(i)/obs_srv3_biom(i))); } // log-likelihood for survey biomass    // likelihood for survey biomass 
    for (i=1; i<=nyrs_srv4; i++)  {  
     surv_like(2) += square((log(obs_srv4_biom(i)+0.0001)-log(pred_srv4(i)+0.0001) ))/ (2.*square(obs_srv4_se(i)/obs_srv4_biom(i))); }
    for (i=1; i<=nyrs_srv5; i++)  {  
     surv_like(3) += square((log(obs_srv5_biom(i)+0.0001)-log(pred_srv5(i)+0.0001) ))/ (2.*square(obs_srv5_se(i)/obs_srv5_biom(i))); }
    for (i=1; i<=nyrs_srv6; i++)  {  
     surv_like(4) += square((log(obs_srv6_biom(i)+0.0001)-log(pred_srv6(i)+0.0001) ))/ (2.*square(obs_srv6_se(i)/obs_srv6_biom(i))); }
  
  surv_like(1) *= wt_srv3 ; 
  surv_like(2) *= wt_srv4 ;  
  surv_like(3) *= wt_srv5 ;
  surv_like(4) *= wt_srv6 ;  
 
FUNCTION Multinomial_Likelihood
// Calculate multinomial likelihoods for survey age, fishery size, and survey size and subtract "offset"
  age_like.initialize();       
  for (i=1; i <= nyrs_fish1_age; i++) {
    age_like(1) -= nsamples_fish1_age(i)*((oac_fish1(i) + 0.001) * log(eac_fish1(i) + 0.001)) ; }

  for (i=1; i <= nyrs_srv1_age; i++) {
    age_like(2) -= nsamples_srv1_age(i)*((oac_srv1(i) + 0.001) * log(eac_srv1(i) + 0.001)) ; }

  //for (i=1; i <= nyrs_srv2_age; i++) {
    //age_like(3) -= nsamples_srv2_age(i)*((oac_srv2(i) + 0.001) * log(eac_srv2(i) + 0.001)) ; }

  for (i=1; i <= nyrs_fish1_size; i++) age_like(4) -= nsamples_fish1_size(i)*((osc_fish1_f(i) + 0.001) * log(esc_fish1_f(i) + 0.001)) ;
  for (i=1; i <= nyrs_fish1_size; i++) age_like(5) -= nsamples_fish1_size(i)*((osc_fish1_m(i) + 0.001) * log(esc_fish1_m(i) + 0.001)) ;
  for (i=1; i <= nyrs_fish3_size; i++) age_like(6) -= nsamples_fish3_size(i)*((osc_fish3_f(i) + 0.001) * log(esc_fish3_f(i) + 0.001)) ;
  for (i=1; i <= nyrs_fish3_size; i++) age_like(7) -= nsamples_fish3_size(i)*((osc_fish3_m(i) + 0.001) * log(esc_fish3_m(i) + 0.001)) ;

  for (i=1; i <= nyrs_srv1_size; i++)  age_like(9) -= nsamples_srv1_size(i)*((osc_srv1_f(i) + 0.001) * log(esc_srv1_f(i) + 0.001)) ;
  for (i=1; i <= nyrs_srv1_size; i++)  age_like(10) -= nsamples_srv1_size(i)*((osc_srv1_m(i) + 0.001) * log(esc_srv1_m(i) + 0.001)) ;
  for (i=1; i <= nyrs_srv2_size; i++)  age_like(11) -= nsamples_srv2_size(i)*((osc_srv2_f(i) + 0.001) * log(esc_srv2_f(i) + 0.001)) ;
  for (i=1; i <= nyrs_srv2_size; i++)  age_like(12) -= nsamples_srv2_size(i)*((osc_srv2_m(i) + 0.001) * log(esc_srv2_m(i) + 0.001)) ;

    age_like(1)     -= offset(1);                      // Subract offsets
    age_like(2)     -= offset(2);                      // Subract offsets
    //age_like(3)   -= offset(3); 
    age_like(4)   -= offset(4);     
    age_like(5)   -= offset(5); 
    age_like(6)   -= offset(6); 
    age_like(7)   -= offset(7);     
    age_like(9)   -= offset(9); 
    age_like(10)  -= offset(10);
    age_like(11)  -= offset(11);
    age_like(12)  -= offset(12);

    age_like(1)  *= wt_fish1_age;    //1              // Multiple each likelihood by their weights from .ctl file
    age_like(2)  *= wt_srv1_age;     //1 
    //age_like(3)  *= wt_srv2_age;     //1 
    age_like(4)  *= wt_fish1_size;   //1
    age_like(5)  *= wt_fish1_size;   //1 
    age_like(6)  *= wt_fish3_size;   //1
    age_like(7)  *= wt_fish3_size;   //1    
    age_like(9)  *= wt_srv1_size;    //1
    age_like(10) *= wt_srv1_size;    //1  
    age_like(11) *= wt_srv2_size;    //1
    age_like(12) *= wt_srv2_size;    //1 
      


FUNCTION double round(double r) 
    return double((r > 0.0) ? floor(r + 0.5) : ceil(r - 0.5)); 
FUNCTION Get_Population_Projection
// Abundance at start of first projection year
// stdev of recvar
  int k;
  if(mceval_phase()) {
// random_number_generator r(1000);
  stdev_rec = sqrt(norm2(value(log_rec_dev(1979,endyr-recage))-mean(value(log_rec_dev(1979,endyr-recage))))/(size_count(value(log_rec_dev(1979,endyr-recage)))-1));
  
   k=round(value(stdev_rec)*10000);  // seed for random number generator

   N_proj_f(endyr+1,1)= mfexp(value(log(mean(value(pred_rec(1979,endyr-recage-1))))-square(stdev_rec)/2+stdev_rec*randn(k+l)))/2;  // generating recruitments for each year
   N_proj_m(endyr+1,1)= mfexp(value(log(mean(value(pred_rec(1979,endyr-recage-1))))-square(stdev_rec)/2+stdev_rec*randn(k+l)))/2; }
  else {   N_proj_f(endyr+1,1)= mfexp(value(log(mean(pred_rec(1979,endyr-recage-1)))))/2;
           N_proj_m(endyr+1,1)= mfexp(value(log(mean(pred_rec(1979,endyr-recage-1)))))/2; }
    for (j=1; j<nages-1;j++) {
      k=k+j;
      N_proj_f(endyr+1,j+1)=natage_f(endyr,j)*value(S_f(endyr,j));
      N_proj_m(endyr+1,j+1)=natage_m(endyr,j)*value(S_m(endyr,j)); }
      N_proj_f(endyr+1,nages) = value(natage_f(endyr,nages-1))*value(S_f(endyr,nages-1))+ value(natage_f(endyr,nages))*value(S_f(endyr,nages));
      N_proj_m(endyr+1,nages) = value(natage_m(endyr,nages-1))*value(S_m(endyr,nages-1))+ value(natage_m(endyr,nages))*value(S_m(endyr,nages));

   spawn_biom_proj(endyr+1) =elem_prod(N_proj_f(endyr+1),pow(mfexp(-yieldratio*FABC_tot_proj_f-value(natmort)),spawn_fract)) * wt_mature;
   tot_biom_proj(endyr+1)=N_proj_f(endyr+1)*wt_f+N_proj_m(endyr+1)*wt_m;

  // reinstate yieldration because they don't always catch al the quota, and should be spatial because it's bsaiwg that doesn't
  for (i=endyr+1;i<=endyr+projyrs;i++)
  {
  // F ABC control rule
    if (spawn_biom_proj(i)/B40 > 1.) {
      FABC_proj = value(F40);
      FOFL_proj=value(F35); }
    else {
      FABC_proj = value(F40) * (spawn_biom_proj(i)/value(B40) - 0.05)/(1 - 0.05); 
      FOFL_proj = value(F35)*(spawn_biom_proj(i)/value(B40) - 0.05)/(1 - 0.05);  }

    for (j=1;j<=nages;j++)
    {  
      FABC_tot_proj_f(j) = fish4_sel_f(j)* FABC_proj * fratio + fish3_sel_f(j)* FABC_proj * (1-fratio);  // think about spatail fratio
      FABC_tot_proj_m(j) = fish4_sel_m(j)* FABC_proj * fratio + fish3_sel_m(j)* FABC_proj * (1-fratio);
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
   if (i < endyr+projyrs)
   {
    if(mceval_phase()) {
    stdev_rec = sqrt(norm2(value(log_rec_dev(1979,endyr-recage-1))-mean(value(log_rec_dev(1979,endyr-recage-1))))/(size_count(value(log_rec_dev(1979,endyr-recage)))-1));
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
  Surv_Likelihood();                              		// Likelihood function for survey biomass
        
   ssqcatch  +=  wt_ssqcatch_fish1 *norm2(log(obs_catch_fish1+0.001)-log(pred_catch_fish1+0.001));
   ssqcatch  +=  wt_ssqcatch_fish3 *norm2(log(obs_catch_fish3+0.001)-log(pred_catch_fish3+0.001));   
 
   //if (rec_like_type==2)
   // rec_like  = wt_rec_var * norm2(log_rec_dev)/(2*square(sigr)) + (size_count(log_rec_dev)*log(sigr));
   rec_like     = wt_rec_var*(norm2(log_rec_dev+sigr*sigr/2.)/(2.*square(sigr)) + (size_count(log_rec_dev))*log(sigr));
   F_mort_regularity  = wt_fmort_reg * norm2(log_F_devs_fish1);  // Penalty function for fishing mortality deviations
   F_mort_regularity += wt_fmort_reg * norm2(log_F_devs_fish3);  // Penalty function for fishing mortality deviations        

         
    Multinomial_Likelihood();                     // Multinomial likelihood

// Sum objective function
  obj_fun           += ssqcatch;                  
  obj_fun           += sum(surv_like);
  obj_fun           += sum(age_like);
  Like               = obj_fun;  								  // Put here to capture the data likelihood
  obj_fun           += rec_like;
  obj_fun           += F_mort_regularity;
  obj_fun           += sum(priors);        				// Add priors
  if (active(mF40)&&last_phase()) 
    obj_fun         += sprpen;             				// To solve for the F40 etc.     
  if (current_phase()<3) obj_fun += 10*((norm2(log_F_devs_fish1))+norm2(log_F_devs_fish3)); 	//(was-0.3) Penalty early on to scale population...                


GLOBALS_SECTION
 # include "admodel.h"          						// Include AD class definitions
  adstring model_name;
  adstring data_file;
  adstring unchanging_data;

REPORT_SECTION
  if(last_phase()) {
  //report out things needed to feed into apportionment script:
  report << "$Num_parameters_Estimated" << endl; 
  report << initial_params::nvarcalc() << endl;
  report << "$SPRpen" << endl;
  report << sprpen << endl;
  report << "$obj_fun" << endl;
  report << obj_fun << endl; 
  report << "$datalikelihood" << endl;
  report << Like << endl; 

  report << "$age.likelihood_fish" << endl;
  report << age_like(1) << endl;
  report << "$age.likelihood_surv" << endl;
  report << age_like(2) << endl;
  report << "$index.likelihood_surv" << endl;
  report << surv_like(1) << endl;
  report << "$index.likelihood_fish" << endl;
  report << surv_like(4) << endl;
 
  report << "$ABC_proj" << endl;
  report << pred_catch_proj << endl;
  report << "$B40" << endl;
  report << B40 << endl;
  report << "$SBF40" << endl;
  report << SBF40 << endl;
  report << "$SBF35" << endl;
  report << SBF35 << endl;
  report << "$SB0" << endl;
  report << SB0 << endl;
  report << "$Depletion"<< endl; 
  report << Depletion << endl;

  report << "$spawn_biom" << endl;
  report << spawn_biom << endl;

  report << "$yrs_srv3" << endl;
  report << yrs_srv3 << endl;
  report << "$pred_srv3_biom" << endl;
  report << pred_srv3 << endl;
  report << "$obs_srv3_biom" << endl;
  report << obs_srv3_biom <<endl;
  report << "$yrs_srv5" << endl;
  report << yrs_srv5 << endl;
  report << "$pred_srv5_biom"<< endl;
  report << pred_srv5 <<endl;
  report << "$obs_srv5_biom" << endl;
  report << obs_srv5_biom << endl;

  report << "$predrecruitment" << endl;
  report << pred_rec << endl;
  report << "$pred_catch_fixed.biom" << endl;
  report << pred_catch_fish1 << endl;
  report << "$pred_catch_trawl.biom" << endl;
  report << pred_catch_fish3 << endl;

  report << "$fish1_sel_f" << endl; 
  report << fish1_sel_f << endl;
  report << "$fish1_sel_m" << endl;
  report << fish1_sel_m << endl;
  report << "$fish4_sel_f" << endl;
  report << fish4_sel_f << endl;
  report << "$fish4_sel_m" << endl; 
  report << fish4_sel_m  << endl;  

  report << "$fish3_sel_f" << endl;
  report << fish3_sel_f <<endl;
  report << "$fish3_sel_m" << endl; 
  report << fish3_sel_m << endl;

  report << "$srv1_sel_f" << endl; 
  report << srv1_sel_f << endl;
  report << "$srv1_sel_m" << endl;
  report << srv1_sel_m  << endl;

  report << "$q_srv1" << endl; 
  report << q_srv1 << endl;
  report << "$q_srv2" << endl; 
  report << q_srv2 << endl;
  report << "$q_srv5" << endl; 
  report << q_srv5 << endl;
  report << "$q_srv6" << endl; 
  report << q_srv6 << endl;
  report << "$q_srv8" << endl;
  report << q_srv8 << endl;

  report << "$tot_biom" << endl;
  report << tot_biom << endl;
  report << "$Fully_selected_F" << endl;
  report << Fmort_fish1*max(fish1_sel_f)+Fmort_fish3 << endl;
  }

FUNCTION write_catchatage
  ofstream catchrep("Catchatage.rep");
  if(last_phase()) {
  catchrep << "$LLcatchatage" << endl;
  catchrep << pred_catch_fish1 << endl;
  catchrep << "$TRcatchatage" << endl;
  catchrep << pred_catch_fish3 << endl;
  }

FUNCTION write_Natage
  ofstream Nrep("Natage.rep");
  if(last_phase()) {
  Nrep << "$natage_f" << endl;
  for (i=styr;i<=endyr;i++) Nrep << i << " " << natage_f(i) << endl;
  Nrep << "$natage_m" << endl;
  for (i=styr;i<=endyr;i++) Nrep << i << " " << natage_m(i) << endl;
  }


FUNCTION write_agecomps_surv
  ofstream agerep1("agecomp_surv.rep");
  if(last_phase()) {
  agerep1 << "$pred_srv1_age" << endl; 
  agerep1 << eac_srv1 << endl;
  }

FUNCTION write_agecomps_fish
  ofstream agerep2("agecomp_fish.rep");
  if(last_phase()) {
  agerep2 << "$pred_fish1_age" << endl;
  agerep2 << eac_fish1 << endl; 
  }

//FUNCTION write_fullrep
  //ofstream fullrep("sable.rep");
  //if(last_phase()) {
  //fullrep << "$Pred_catch" << endl; 
  //fullrep << pred_catch <<endl;
  //fullrep << "$avgFmort" << endl; 
  //fullrep << log_avg_F_fish1 << endl;

  //fullrep << "$F35" << endl;
  //fullrep << F35 << endl;
  //fullrep << "$F40" << endl;
  //fullrep << F40 << endl;
  //fullrep << "$SBF35" << endl;
  //fullrep << SBF35 << endl;
  //fullrep << "$F_recruitment" << endl; 
  //fullrep << trans(natage_f)(1) << endl;
  //fullrep << "$M_recruitment" << endl; 
  //fullrep << trans(natage_m)(1) << endl;
  //fullrep << "$Spec_catch_proj" << endl;
  //fullrep << (catage_proj_f*wt_f+catage_proj_m*wt_m) << endl; 
  
  //fullrep << "spawn_bio next year"<<endl<<spawn_biom_proj(endyr+1)<<endl;
  //fullrep <<" spawn_bio projected"<<endl<<spawn_biom_proj<<endl;
  //fullrep << "Specified_catch_projection"<<(catage_proj_f*wt_f+catage_proj_m*wt_m)<<endl<<"ABC projection: "<<pred_catch_proj<<endl;

  //EM structure  
  //fullrep << "single area model output" << endl;
  //fullrep << "model_start_year" << endl;
  //fullrep << $styr << endl;
  //fullrep << "model_end_year" << endl;
  //fullrep << $endyr << endl;  
  //fullrep << model_name << endl;
  //fullrep <<  data_file << endl;

  //fullrep << "years"<< endl;
  //fullrep << yy << endl;

  //Estimated values
  //fullrep << "yrs_srv4" <<endl << yrs_srv4 <<endl;
  //fullrep << "pred_srv4_biom" <<endl << pred_srv4  <<endl;
  //fullrep << "obs_srv4_biom" << endl << obs_srv4_biom  <<endl;
  
  //fullrep << "yrs_srv3" << endl << yrs_srv3  <<endl;
  //fullrep << "pred_srv3_biom" << endl << pred_srv3  <<endl;
  //fullrep << "obs_srv3_biom" << endl << obs_srv3_biom  <<endl;
  
  //fullrep << "yrs_srv5" << endl << yrs_srv5  <<endl;
  //fullrep << "pred_srv5_biom"<< endl << pred_srv5  <<endl;
  //fullrep << "obs_srv5_biom" << endl << obs_srv5_biom  <<endl;

  //fullrep << "yrs_srv6" << endl << yrs_srv6  <<endl;
  //fullrep << "pred_srv6_biom"<< endl << pred_srv6  <<endl;
  //fullrep << "obs_srv6_biom" << endl << obs_srv6_biom  <<endl;

  //fullrep << "Fully_selected_F" << endl << Fmort_fish1*max(fish1_sel_f)+Fmort_fish3 << endl;
  //fullrep << "spawn_biom"<< endl << spawn_biom <<endl;
  //fullrep << "tot_biom"<< endl << tot_biom   <<endl;

  //fullrep << "pred_catch_fish1"<< endl << pred_catch_fish1<<endl;
  //fullrep << "obs_catch_fish1"<< endl << obs_catch_fish1<<endl;
 
  //fullrep << "pred_catch_fish3"<< endl << pred_catch_fish3<<endl;
  //fullrep << "obs_catch_fish3"<< endl << obs_catch_fish3<<endl;

  //fullrep << "fish1_sel_f"<< endl << fish1_sel_f <<endl;
  //fullrep << "fish1_sel_m"<< endl << fish1_sel_m <<endl;
  //fullrep << "fish2_sel"<< endl << fish2_sel <<endl;
  //fullrep << "fish3_sel_f"<< endl << fish3_sel_f <<endl;
  //fullrep << "fish3_sel_m"<< endl << fish3_sel_m  <<endl;
  //fullrep << "fish4_sel_f"<< endl << fish4_sel_f <<endl;
  //fullrep << "fish4_sel_m"<< endl << fish4_sel_m  <<endl;

  //fullrep << "srv1_sel_f"<< endl << srv1_sel_f <<endl;
  //fullrep << "srv1_sel_m"<< endl << srv1_sel_m  <<endl;
  //fullrep << "srv2_sel_f"<< endl << srv2_sel_f  <<endl;
  //fullrep << "srv2_sel_m"<< endl << srv2_sel_m  <<endl;
  //fullrep << "SigmaR: "<<sigr<< " Nat_Mort: "<<natmort<< endl;
  //fullrep <<"Spawning Per Recruit "<< " "<<SBF40<< endl;
  //fullrep << "Virgin SPR (SB0) "<<SB0 <<endl;
   
  //fullrep<<"log_mean_rec"<< endl << log_mean_rec<< endl;
  //fullrep<<"log_rec_dev" << endl << log_rec_dev << endl;
  //fullrep<<"pred_rec" << endl << pred_rec << endl;
  
  //fullrep << "q_srv1" << q_srv1<<endl;
  //fullrep << "q_srv2" << q_srv2<<endl;
  //fullrep << "q_srv5" << q_srv5<<endl;
  //fullrep << "q_srv6" << q_srv6<<endl;
  //fullrep << "q_srv8" << q_srv8<<endl;
  
  //likelihoods
  //fullrep << "Wts_n_Likelihoods" << endl;
  //fullrep << "SSQ_catch likelihood" << endl << ssqcatch << endl;
  //fullrep << "Surv_like" << endl;
  //fullrep << "order: srv3,srv4,srv5,srv6," << surv_like << endl;
  //fullrep << "Age_like" << endl;
  //fullrep << "order: fish1a, srv1a,0, fish1s f/m, fish3s f/m, 0, srv1s f/m, srv2s f/m" << endl;
  //fullrep <<  age_like << endl;

  //fullrep << wt_rec_var <<" " << rec_like <<" " ; fullrep << "Recruitment_Deviations_Likelihood" << endl;
  //fullrep << wt_fmort_reg <<" "<<F_mort_regularity<<" " ; fullrep << "Fishing_Mortality_Regularity_Penalty" << endl;
  
  //fullrep << " "<<priors(1)  <<" " ; fullrep << "priors sigr"     <<endl;
  //fullrep << " "<<priors(2)  <<" " ; fullrep << "priors q_srv1" <<endl;
  //fullrep << " "<<priors(4)  <<" " ; fullrep << "priors M"<<endl;
  //fullrep << " "<<priors(7)  <<" " ; fullrep << "priors q_srv2"<<endl;
  //fullrep << " "<<priors(9)  <<" " ; fullrep << "priors q_srv5"<<endl;
  //fullrep << " "<<priors(10)  <<" " ; fullrep << "priors q_srv6"<<endl;
  //fullrep << " "<<priors(11)  <<" " ; fullrep << "priors q_srv8"<<endl;
  //fullrep << sprpen << " "<<"Sprpen" <<endl;
  //fullrep << " "<<obj_fun    <<" " ; fullrep << "obj_fun"         <<endl;
  //fullrep << " "<<Like       <<" " ; fullrep << "data likelihood" <<endl;//(2*square(sigr))+ size_count(log_rec_dev)*log(sigr)<<endl;

  //fullrep << "Survival_Female" << endl;
  //for (i=styr;i<=endyr;i++) fullrep << i<<" "<<S_f(i) <<endl; //fullrep <<endl ;

  //fullrep << "Survival_Male" << endl;
  //for (i=styr;i<=endyr;i++) fullrep << i<<" "<<S_m(i) <<endl; 
  //fullrep <<endl ;

  //fullrep << "Numbers_female" << endl;
  //for (i=styr;i<=endyr;i++) fullrep << i<<" "<<natage_f(i) <<endl; // fullrep<<endl;

  //fullrep << "Numbers_male" << endl;
  //for (i=styr;i<=endyr;i++) fullrep << i<<" "<<natage_m(i) <<endl; // fullrep<<endl;

  //fullrep << "ages" << endl << aa <<endl;

  //fullrep << "obs_fish1_age"<< endl << oac_fish1 << endl;
  //fullrep << "pred_fish1_age"<< endl << eac_fish1 << endl; 

  //fullrep << "obs_srv1_age"<< endl << oac_srv1 <<endl; 
  //fullrep << "pred_srv1_age"<< endl << eac_srv1 <<endl; 
  //fullrep << "obs_srv2_age"<< endl << oac_srv2 <<endl; 
  //fullrep << "pred_srv2_age"<< endl << eac_srv2 <<endl; 

  //fullrep << "obs_fish1_size_m"<<endl <<osc_fish1_m << endl;
  //fullrep << "pred_fish1_size_m"<<endl <<esc_fish1_m <<endl;
  //fullrep << "obs_fish1_size_f"<<endl <<osc_fish1_f << endl;
  //fullrep << "pred_fish1_size_f"<<endl <<esc_fish1_f <<endl;

  //fullrep << "obs_fish3_size_m"<<endl <<osc_fish3_m << endl;
  //fullrep << "pred_fish3_size_m"<<endl <<esc_fish3_m <<endl;
  //fullrep << "obs_fish3_size_f"<<endl <<osc_fish3_f << endl;
  //fullrep << "pred_fish3_size_f"<<endl <<esc_fish3_f <<endl;

  //fullrep << "obs_srv1_size_m"<< endl <<osc_srv1_m  << endl;
  //fullrep << "pred_srv1_size_m"<<endl <<esc_srv1_m <<endl;
  //fullrep << "obs_srv1_size_f"<<endl << osc_srv1_f << endl;
  //fullrep << "pred_srv1_size_f"<<endl <<esc_srv1_f <<endl;

  //fullrep << "obs_srv2_size_m"<< endl <<osc_srv2_m  << endl;
  //fullrep << "pred_srv2_size_m"<<endl <<esc_srv2_m <<endl;
  //fullrep << "obs_srv2_size_f"<<endl << osc_srv2_f << endl;
  //fullrep << "pred_srv2_size_f"<<endl <<esc_srv2_f <<endl;
  //}

FUNCTION double sdnr(const dvar_vector& pred,const dvector& obs,double m)
  RETURN_ARRAYS_INCREMENT();
  double sdnr;
  dvector pp = value(pred)+0.000001;
  int ntmp = -obs.indexmin()+obs.indexmax();
  sdnr = std_dev(elem_div(obs+0.000001-pp,sqrt(elem_prod(pp,(1.-pp))/m)));
  RETURN_ARRAYS_DECREMENT();
  return sdnr;

RUNTIME_SECTION
  convergence_criteria 1.e-4, 1.e-4, 1.e-4, 1.e-4, 1.e-4  
  maximum_function_evaluations 1000, 1000, 1000, 1000, 1000

TOP_OF_MAIN_SECTION
  gradient_structure::set_MAX_NVAR_OFFSET(1000);
  gradient_structure::set_GRADSTACK_BUFFER_SIZE(100000);
  //arrmblsize=390000;
  arrmblsize=1500000;

FINAL_SECTION
  //write_fullrep();
  write_catchatage();
  write_Natage();
  write_agecomps_fish();
  write_agecomps_surv();