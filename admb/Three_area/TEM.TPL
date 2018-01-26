//==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+
//
//  Simple 3 area Statistical, separable age-structured population model for sablefish
//  Alaska Fisheries Science Center, September2014
//  original code by D. Hanselman:dana.hanselman@noaa.gov
//  modified by KHfenske: khfenske@alaska.edu
//  Input file:   tem.dat, as in tem(plate)
//  Control file: tem.ctl
//  Program file: tem.tpl
//  Output files: tem.rep, tem.std, proj.dat, newproj.dat
//  Sablefish model 
//  separate sexes; has sex-specific weight at ages, spatial movement estimation
//  srv3= Domestic longline survey RPN 1990-endyr
//  srv4= US-JP LL survey RPN 1979-1994
//  srv5= Domestic LL fishery CPUE 1990-(endyr-1) 
//  srv6= JP LL fishery RPW 1964-1981
//  fish1= U.S. Longline fishery, catch data for 1960-endyr (or are the early years data fish2?)
//  fish3= US trawl fishery, catch data for 1963-endyr
//  fish1 and srv1 age comps (1999-(endyr-1) and 1996-(endyr-1))
//  srv2/4 age comps 1981-1993, odd years 
//  fish1, fish3, srv1, srv2 length comps
//  3 regions - bsaiwg, cg, eg
//==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+

DATA_SECTION
  !!CLASS ofstream evalout("evalout.prj");   
// Read data from the control file
  !! ad_comm::change_datafile_name("tem.ctl");    // Read in phases, penalties and priors from "tem.ctl"
  !! *(ad_comm::global_datafile) >>  model_name; 
  !! *(ad_comm::global_datafile) >>  data_file;   // identifying the data file name, using the name in row 2 of .ctl file
  init_int    nareas                // number of areas in the MODEL 
  init_int    SrType                // 3=average, others not likely functional
  init_int    styr_rec_est
  init_int    endyr_rec_est
  int nrecs_est;
  !! nrecs_est = endyr_rec_est-styr_rec_est+1;
  init_int    rec_like_type  // Type of likelihood specified 

// NOT USED Age of full selection for fishery and survey  
  //init_int    n_fish_sel_ages       // Age that fishery selectivity stops estimating
  //init_int    n_srv1_sel_ages       // Age that survey selectivity stops estimating
  
// Phases that general parameter estimation begins
  init_int    ph_Fdev               // Phase for fishing mortality deviations
  init_int    ph_avg_F              // Phase for estimating average fishing mortality
  init_int    ph_recdev             // Phase for estimating recruitment deviations
  init_int    ph_fish1_sel          // Phase for estimating fishing selectivity
  init_int    ph_fish4_sel
  init_int    ph_srv_sel            // Phase for estimating survey selectivity

// Priors
  init_vector mprior(1,nareas)      // Prior mean for natural mortality
  init_vector cvmprior(1,nareas)    // Prior CV for natural mortality
  init_int    ph_m                  // Phase for estimating natural mortality

  init_number sigrprior             // Prior mean for recruitment deviations
  init_number cvsigrprior           // Prior CV for recruitment deviations
  init_int    ph_sigr               // Phase for recruiment deviations

  init_number q_srv1prior            // Prior mean for catchability coefficient
  init_number cvq_srv1prior          // Prior CV for catchability coefficient
  init_int    ph_q_srv1              // Phase for estimating catchability
  
  init_number q_srv2prior           // Prior mean for catchability coefficient
  init_number cvq_srv2prior         // Prior CV for catchability coefficient
  init_int    ph_q_srv2             // Phase for estimating catchability

  // srv5 is dom LL fishery, pre-IFQ
  init_number q_srv5prior1           // Prior mean for catchability coefficient
  init_number cvq_srv5prior1         // Prior CV for catchability coefficient
  init_int    ph_q_srv5_1             // Phase for estimating catchability
  init_number q_srv5prior2           // Prior mean for catchability coefficient
  init_number cvq_srv5prior2         // Prior CV for catchability coefficient
  init_int    ph_q_srv5_2             // Phase for estimating catchability
  init_number q_srv5prior3           // Prior mean for catchability coefficient
  init_number cvq_srv5prior3         // Prior CV for catchability coefficient
  init_int    ph_q_srv5_3             // Phase for estimating catchability
   
  // srv6 is JP LL fishery
  init_number q_srv6prior           // Prior mean for catchability coefficient
  init_number cvq_srv6prior         // Prior CV for catchability coefficient
  init_int    ph_q_srv6             // Phase for estimating catchability

  // srv8 is dom LL fishery, post-IFQ
  init_number q_srv8prior1           // Prior mean for catchability coefficient
  init_number cvq_srv8prior1         // Prior CV for catchability coefficient
  init_int    ph_q_srv8_1            // Phase for estimating catchability
  init_number q_srv8prior2           // Prior mean for catchability coefficient
  init_number cvq_srv8prior2         // Prior CV for catchability coefficient
  init_int    ph_q_srv8_2            // Phase for estimating catchability
  init_number q_srv8prior3           // Prior mean for catchability coefficient
  init_number cvq_srv8prior3         // Prior CV for catchability coefficient
  init_int    ph_q_srv8_3            // Phase for estimating catchability
   
  init_number wt_ssqcatch_fish1      // Weight for catch estimation
  init_number wt_ssqcatch_fish3      // Weight for catch estimation
  init_number wt_srv3                // Weight for survey biomass estimation
  init_number wt_srv4                // Weight for survey biomass estimation
  init_number wt_srv5                // Weight for survey biomass estimation
  init_number wt_srv6                // Weight for survey biomass estimation
  init_vector wt_fish1_age(1,nareas)   // Weight for fishery age compositions
  //init_number wt_fish1_age2          // Weight for fishery age compositions
  //init_number wt_fish1_age3          // Weight for fishery age compositions
  init_vector wt_srv1_age(1,nareas)    // Weight for survey age compositions
  //init_number wt_srv1_age2            // Weight for survey age compositions
  //init_number wt_srv1_age3            // Weight for survey age compositions
  init_number wt_srv2_age               // Weight for survey 2 age comps
  init_vector wt_fish1_sizeM(1,nareas)  // Weight for male fishery size compositions
  //init_number wt_fish1_sizem2         // Weight for fishery size compositions
  //init_number wt_fish1_sizem3         // Weight for fishery size compositions
  init_vector wt_fish1_sizeF(1,nareas)  // Weight for female fishery size compositions
  //init_number wt_fish1_sizef2          // Weight for fishery size compositions
  //init_number wt_fish1_sizef3          // Weight for fishery size compositions
  init_vector wt_srv1_sizeM(1,nareas)    // Weight for survey size compostiions
  //init_number wt_srv1_sizem2           // Weight for survey size compostiions
  //init_number wt_srv1_sizem3           // Weight for survey size compostiions
  init_vector wt_srv1_sizeF(1,nareas)    // Weight for survey size compostiions
  //init_number wt_srv1_sizef2           // Weight for survey size compostiions
  //init_number wt_srv1_sizef3           // Weight for survey size compostiions
  init_number wt_fish2_size              // Weight for fishery size compositions
  init_vector wt_srv2_sizeM(1,nareas)          // Weight for survey size compostiions
  init_vector wt_srv2_sizeF(1,nareas)          // Weight for survey size compostiions
  init_vector wt_fish3_sizeM(1,nareas)         // Weight for fishery size compositions
  init_vector wt_fish3_sizeF(1,nareas)         // Weight for fishery size compositions
  init_number wt_fish4_size          // Weight for fishery size compositions
  init_number wt_srv5_size           // Weight for survey size compostiions
  init_number wt_fish6_size          // Weight for fishery size compositions
  init_number wt_srv6_size           // Weight for survey size compostiions

  init_number wt_rec_var             // Weight for estimation recruitment variations penalty
  init_number wt_sel_reg_fish3       // Weight on fishery selectivity regularity penalty
  init_number wt_sel_dome_fish3      // Weight on fishery selectivity dome-shape penalty   
  init_number wt_fmort_reg           // Weight on fishing mortality regularity

  //init_number ph_Rzero;              // NOT USED; Phase for Rzero, need to turn off for base-case
  init_vector hist_hal_prop(1,nareas)  // additional data for BS flag
  init_number ph_ifq
  init_number ph_srv2q                 // phase for srv9 q
  init_vector yieldratio(1,nareas);
  init_int projyrs;
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
  !!  styr_rec = (styr - nages) + 1;     // First year of recruitment
  !! endyr_rec= endyr_rec_est;           // 
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
  vector        wt_mature(1,nages)           
  !! wt_mature = elem_prod(wt_f,p_mature);

//These no longer need to be read in, so I'm adding the _NA at the end for now. Eventually
//can just delete from the .dat file and here.
  init_vector   fish3_sel_f_NA(1,nages)   // input of fish3 selectivity values, using the values for 2014 single-area assessment for Fish3

// Observed fixed gear (fish1) and trawl gear (fish 3) catches
  init_int      nyrs_obscatch                         // NOT USED, COULD REMOVE
  init_matrix   obs_catch_fish1(1,nareas,styr,endyr)  // reading in the data for the three areas of fixed gear catches, observed catches from US fixed gear fishery for BSAIWG(1), CG(2), EG(3)
// fish3 -trawl catches 
  init_matrix   obs_catch_fish3(1,nareas,styr,endyr)  //observed catches from US trawl fishery for BSAIWG, CG, EG 

// Survey biomass estimates RPN 
  init_number   nyrs_srv3                      // number of years of survey biomass estimates
  init_ivector  yrs_srv3(1,nyrs_srv3)          // years survey conducted, indexed using years in BSAIWG
  init_matrix   obs_srv3_biom(1,nareas,1,nyrs_srv3)   // USING BSAIWG FOR INDEXING! mean estimate of biomass RPW (basiwg, cg, eg in that order)
  init_matrix   obs_srv3_se(1,nareas,1,nyrs_srv3)     // standard error of survey biomass estimates
  init_matrix   obs_srv3_lci(1,nareas,1,nyrs_srv3)    // lower confidence interval, for graphing not used in estimation
  init_matrix   obs_srv3_uci(1,nareas,1,nyrs_srv3)    // upper confidence interval

// US-JP coop LL survey RPN - srv4, 1979-1994
  init_int      nyrs_srv4                                // number of years of survey abundance estimates
  init_vector   yrs_srv4(1,nyrs_srv4)                    // years survey conducted in
  init_matrix   obs_srv4_biom(1,nareas,1,nyrs_srv4)      // mean estimate of abundance
  init_matrix   obs_srv4_se(1,nareas,1,nyrs_srv4)        // standard error of survey abundance estimates
  init_matrix   obs_srv4_lci(1,nareas,1,nyrs_srv4)       // lower confidence interval, for graphing not used in estimation
  init_matrix   obs_srv4_uci(1,nareas,1,nyrs_srv4)       // upper confidence interval  

// Domestic LL Fishery CPUE RPW, srv5
  init_int      nyrs_srv5                                // number of year of fishery abundance
  init_vector   yrs_srv5(1,nyrs_srv5)
  init_matrix   obs_srv5_biom(1,nareas,1,nyrs_srv5)      // mean estimate of biomass RPW
  init_matrix   obs_srv5_se(1,nareas,1,nyrs_srv5)        // standard error of abundance estimates
  init_matrix   obs_srv5_lci(1,nareas,1,nyrs_srv5)       // lower confidence interval, for graphing not used in estimation
  init_matrix   obs_srv5_uci(1,nareas,1,nyrs_srv5)       // upper confidence interval  

// Japanese LL fishery RPW, srv6
  init_int      nyrs_srv6                                // number of year of fishery abundance
  init_vector   yrs_srv6(1,nyrs_srv6)
  init_matrix   obs_srv6_biom(1,nareas,1,nyrs_srv6)      // mean estimate of biomass RPW
  init_matrix   obs_srv6_se(1,nareas,1,nyrs_srv6)        // standard error of abundance estimates
  init_matrix   obs_srv6_lci(1,nareas,1,nyrs_srv6)       // lower confidence interval, for graphing not used in estimation
  init_matrix   obs_srv6_uci(1,nareas,1,nyrs_srv6)       // upper confidence interval 

// Domestic LL Fishery age composition data - males and females combined
  init_int      nyrs_fish1_age                                 // number of Years of fishery age comps, indexed using bsaiwg
  init_ivector  yrs_fish1_age(1,nyrs_fish1_age)                // the years of age comps, indexed using BSAIWG
  init_matrix   nsamples_fish1_age(1,nareas,1,nyrs_fish1_age)  // some measure of relative sample size for each age comp.
  init_3darray  oac_fish1(1,nareas,1,nyrs_fish1_age,1,nages)   // the actual year by year age comps for each area (bsaiwg=1, cg=2, eg=3)

// Domestic LL Survey age composition data - males and females combined
  init_int      nyrs_srv1_age                                  // number of years of survey age compositions
  init_ivector  yrs_srv1_age(1,nyrs_srv1_age)                  // the years of survey age comps
  init_matrix   nsamples_srv1_age(1,nareas,1,nyrs_srv1_age)    // some measure of relative sample size for each age comp.
  init_3darray  oac_srv1(1,nareas,1,nyrs_srv1_age,1,nages)     // the year by year age survey age comps

// US-JP coop LL survey age composition data - males and females combined  NOT USED IN SPATIAL MODEL
  //init_int      nyrs_srv2_age                                  // number of years of survey age compositions
  //init_ivector  yrs_srv2_age(1,nyrs_srv2_age)                  // the years of survey age comps
  //init_matrix   nsamples_srv2_age(1,nareas,1,nyrs_srv2_age)    // some measure of relative sample size for each age comp.
  //init_3darray  oac_srv2(1,nareas,1,nyrs_srv2_age,1,nages)     // the year by year age survey age comps

// Domestic LL fishery length composition data - sexes separate 1990-(endyr-1)
// in this and in srv1 length comp below, there is an assumption that sample sizes can vary by area
  init_int      nyrs_fish1_size                                // number of years of fishery size comps
  init_ivector  yrs_fish1_size(1,nyrs_fish1_size)              // the years of fishery size comps
  init_matrix   nsamples_fish1_msize(1,nareas,1,nyrs_fish1_size)// some measure of relative sample size for each fish1ery comp
  init_matrix   nsamples_fish1_fsize(1,nareas,1,nyrs_fish1_size)// some measure of relative sample size for each fish1ery comp
  init_3darray  osc_fish1_m(1,nareas,1,nyrs_fish1_size,1,nlenbins)      // year by year fishery size comps
  init_3darray  osc_fish1_f(1,nareas,1,nyrs_fish1_size,1,nlenbins)      // year by year fishery size comps  

// Domestic trawl fishery length composition 1990-(endyr-1)
// note that only a single matrix for each sex, sample sizes are low for WG & EG, so areas combined for now
// NOTE - THIS MAY BE WRONG! IVE PUT THE SAME SIZE COMP DATA IN THREE TIMES, ONCE FOR EACH AREA!
  init_int      nyrs_fish3_size                                // number of years of fish1ery size comps
  init_ivector  yrs_fish3_size(1,nyrs_fish3_size)              // the years of fishery size comps
  init_matrix   nsamples_fish3_size(1,nareas,1,nyrs_fish3_size)         // some measure of relative sample size for each fish1ery comp
  init_3darray  osc_fish3_m(1,nareas,1,nyrs_fish3_size,1,nlenbins)      // year by year fishery size comps
  init_3darray  osc_fish3_f(1,nareas,1,nyrs_fish3_size,1,nlenbins)      // year by year fishery size comps  

// Domestic LL survey length composition 1990-(endyr)
  init_int      nyrs_srv1_size                                   // number of years of survey size comps
  init_ivector  yrs_srv1_size(1,nyrs_srv1_size)                  // the years of survey size comps
  init_matrix   nsamples_srv1_msize(1,nareas,1,nyrs_srv1_size)   // some measure of relative sample size for each survey size comp
  init_matrix   nsamples_srv1_fsize(1,nareas,1,nyrs_srv1_size)   // some measure of relative sample size for each survey size comp
  init_3darray  osc_srv1_m(1,nareas,1,nyrs_srv1_size,1,nlenbins) // year by year size comps
  init_3darray  osc_srv1_f(1,nareas,1,nyrs_srv1_size,1,nlenbins) // year by year size comps

// US-JP coop LL survey length composition 1979-1994
  init_int      nyrs_srv2_size                                  // number of years of survey size comps
  init_ivector  yrs_srv2_size(1,nyrs_srv2_size)                 // the years of fishery size comps
  init_matrix   nsamples_srv2_msize(1,nareas,1,nyrs_srv2_size)  // some measure of relative sample size for each fish1ery comp
  init_matrix   nsamples_srv2_fsize(1,nareas,1,nyrs_srv2_size)  // some measure of relative sample size for each fish1ery comp
  init_3darray  osc_srv2_m(1,nareas,1,nyrs_srv2_size,1,nlenbins)      // year by year fishery size comps
  init_3darray  osc_srv2_f(1,nareas,1,nyrs_srv2_size,1,nlenbins)      // year by year fishery size comps  

// movement 3 groups (age or length based), 3x3 (bsaiwg-cg-eg x bsaiwg-cg-eg)
   init_3darray   phi(1,3,1,nareas,1,nareas)      // movement matrix 

// Size-age transition matrix:  proportion at size given age
  init_matrix   sizeage_m(1,nages,1,nlenbins)     // size comp #1
  init_matrix   sizeage_f(1,nages,1,nlenbins)     // lets you add another size-age matrix, remove this here and in Get_Predicted section to use only one, or just have two identical
  init_matrix   sizeage_all(1,nages,1,nlenbins)   // lets you add another size-age matrix, remove this here and in Get_Predicted section to use only one, or just have two identical
  init_matrix   sizeage_m_new(1,nages,1,nlenbins) // size comp #1
  init_matrix   sizeage_f_new(1,nages,1,nlenbins) // lets you add another size-age matrix, remove this here and in Get_Predicted section to use only one, or just have two identical

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
  matrix offset(1,nareas,1,16);  // Multinomial "offset" for each area and a dimension for each multinomial set of data 
  
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
  
   //   Calculate "offset" for multinomials 
   //   "Offset" value lets the multinomial likelihood equal zero when the observed and
   //   predicted are equal as in Fournier (1990) "robustifies"
   //   First step is to ensure that the data are expressed as proportions
   for (j=1; j<=nareas; j++) {
   for (i=1; i<=nyrs_fish1_age; i++) {
   oac_fish1(j,i)/=sum(oac_fish1(j,i));
   offset(j,1) -= nsamples_fish1_age(j,i) *((oac_fish1(j,i) + 0.001)*log(oac_fish1(j,i) + 0.001));
   }}

   for (j=1; j<=nareas; j++) {
   for (i=1; i<=nyrs_srv1_age; i++) {
   oac_srv1(j,i)/=sum(oac_srv1(j,i));
   offset(j,2) -= nsamples_srv1_age(j,i) *((oac_srv1(j,i) + 0.001)*log(oac_srv1(j,i) + 0.001));
   }}

   for (j=1; j<=nareas; j++) {
   for (i=1; i<=nyrs_fish1_size; i++) {
   osc_fish1_f(j,i)/=sum(osc_fish1_f(j,i));
   offset(j,4) -= nsamples_fish1_fsize(j,i)*((osc_fish1_f(j,i) + 0.001)*log(osc_fish1_f(j,i) + 0.001));
   osc_fish1_m(j,i)/=sum(osc_fish1_m(j,i));
   offset(j,5) -= nsamples_fish1_msize(j,i)*((osc_fish1_m(j,i) + 0.001)*log(osc_fish1_m(j,i) + 0.001));
   }}

   for (j=1; j<=nareas; j++) {
   for (i=1; i<=nyrs_fish3_size; i++) {
   osc_fish3_f(j,i)/=sum(osc_fish3_f(j,i));
   offset(j,6) -= nsamples_fish3_size(j,i)*((osc_fish3_f(j,i) + 0.001)*log(osc_fish3_f(j,i) + 0.001)); 
   osc_fish3_m(j,i)/=sum(osc_fish3_m(j,i));
   offset(j,7) -= nsamples_fish3_size(j,i)*((osc_fish3_m(j,i) + 0.001)*log(osc_fish3_m(j,i) + 0.001)); 
   }}

  for (j=1; j<=nareas; j++) {
  for (i=1; i<=nyrs_srv1_size; i++) {
   osc_srv1_f(j,i)/=sum(osc_srv1_f(j,i));
   offset(j,9) -= nsamples_srv1_fsize(j,i)*((osc_srv1_f(j,i) + 0.001)*log(osc_srv1_f(j,i) + 0.001));
   osc_srv1_m(j,i)/=sum(osc_srv1_m(j,i));
   offset(j,10) -= nsamples_srv1_msize(j,i)*((osc_srv1_m(j,i) + 0.001)*log(osc_srv1_m(j,i) + 0.001));
   }}

   for (j=1; j<=nareas; j++) {
   for (i=1; i<=nyrs_srv2_size; i++) {
   osc_srv2_f(j,i)/=sum(osc_srv2_f(j,i));
   offset(j,11)-=nsamples_srv2_fsize(j,i)*((osc_srv2_f(j,i) + 0.001)*log(osc_srv2_f(j,i) + 0.001));
   osc_srv2_m(j,i)/=sum(osc_srv2_m(j,i));
   offset(j,12)-=nsamples_srv2_msize(j,i)*((osc_srv2_m(j,i) + 0.001)*log(osc_srv2_m(j,i) + 0.001));
   }}

 END_CALCS
 
INITIALIZATION_SECTION  
 sigr               sigrprior
 logm1              -2.30258509299    
 logm2              -2.30258509299
 logm3              -2.30258509299

 log_mean_rec1      1
 //log_mean_rec2      1
 //log_mean_rec3      1

 log_q_srv1         2.02423694283

 log_q_srv2          1.85996498559
 log_q_srv5_1        1.44478842836 //pre-IFQ Q
 log_q_srv5_2        1.44478842836
 log_q_srv5_3        1.44478842836

 log_q_srv6          2.62659387749
 log_q_srv8_1        1.77769087985 //post-IFQ Q
 log_q_srv8_2        1.77769087985
 log_q_srv8_3        1.77769087985

 log_a50_fish1_f     2.0
 log_delta_fish1_f   0.88418 // fixing at the khf single model delta fish4 F s123
 log_a50_fish1_m     2.0421  // going to try estimating...//fixing it at the khf single model value
 log_delta_fish1_m   0.51394 // fixing at the khf single model delta fish4 M s123
 
 log_a50_fish2       2.1      // values from DHH single area model
 log_delta_fish2     0.7865   // values from s124 single model average M and F fish4 deltas  

 log_a50_fish3_f     1.81066964622
 log_delta_fish3_f   1.59878789681
 log_delta_fish3_m   1.0

 log_a50_fish4_f1    1.0
 log_delta_fish4_f1   0.88418 // fixing at the khf single model delta fish4 F s123
 log_a50_fish4_f2    1.0
 //log_delta_fish4_f2  1.0
 log_a50_fish4_f3    1.0
 //log_delta_fish4_f3  1.0

 log_a50_fish4_m1    1.0
 log_delta_fish4_m1  1.0 
 log_a50_fish4_m2    1.0
 //log_delta_fish4_m2  1.0
 log_a50_fish4_m3    1.0
 //log_delta_fish4_m3  1.0

 log_delta_srv1_f    1.75000000000
 log_delta_srv1_m    1.55000000000

 log_a50_srv2_f      0.956710000000
 //log_delta_srv2_f    1.00000000000 // fixed at M delta value
 //log_a50_srv2_m      1.30300000000 // fixed at F a50 value
 log_delta_srv2_m    0.824300000000

 PARAMETER_SECTION
  // Stock-recruitment (from DHH)
  // not going to add this quite yet...get other data in first
  //init_bounded_number   steepness(0.2001,0.999,ph_steepness);  //stock recruitment steepness
  //init_bounded_number   log_Rzero(1,5,-1);       // unfished equilib. recruitment (logged)
  //vector                sam_rec(styr_rec,endyr); // as estimated by assessment model
  //vector                srm_rec(styr_rec,endyr); // as estimated by stock recruitment curve
  //vector                Sp_Biom(styr_sp,endyr);  //
  //number                sigrsq;                  // recruitment variance parameter
  //number                alpha;                   // alpha param. for B-H
  //number                beta;                    // beta param. for B-H
  //number                Bzero;                   // virgin spawner biomass
  //number                Rzero;                   // virgin recruitment
  //number                phizero;                 // SPR
  //number                log_Rztemp;              // temporary logRzero
  
  // Key parameters
  init_number           log_q_srv1(ph_q_srv1);      // Estimate Log srv3 rpn catchability

  init_number           log_q_srv2(ph_q_srv2);      // estimate log srv4 rpn catchability
  init_number           log_q_srv5_1(ph_q_srv5_1);  // Estimate Log survey catchability
  init_number           log_q_srv5_2(ph_q_srv5_2);
  init_number           log_q_srv5_3(ph_q_srv5_3);

  init_number           log_q_srv6(ph_q_srv6);      // Estimate Log survey catchability
  init_number           log_q_srv8_1(ph_q_srv8_1);  // Estimate Log survey catchability
  init_number           log_q_srv8_2(ph_q_srv8_2);  // Estimate Log survey catchability
  init_number           log_q_srv8_3(ph_q_srv8_3);  // Estimate Log survey catchability
  init_bounded_number   logm1(-4,-1,ph_m);	        // Estimate log natural mortality area 1
  init_bounded_number   logm2(-4,-1,ph_m);          // Estimate log natural mortality area 2
  init_bounded_number   logm3(-4,-1,ph_m);          // Estimated log nat mort for area 3
  init_bounded_number   log_mean_rec1(-5,5,1)
  //init_bounded_number   log_mean_rec2(-10,5,1)
  //init_bounded_number   log_mean_rec3(-10,5,1)
  vector  	            log_mean_rec(1,nareas);     // Unfish equil recruitment (logged)
  vector                mean_rec(1,nareas);
  init_bounded_number  	sigr(0.1,2,ph_sigr);        // Recruitment sdev parameter

  // Fishery selectivity (sex-specific, females first then males) 
  init_bounded_number   log_a50_fish1_f(-1,5,ph_fish1_sel);     // age at 50% selection                                                   
  init_bounded_number   log_delta_fish1_f(-4,4,-ph_fish1_sel);  // age between 50% selection and 95% selection....
  number                a50_fish1_f;                            // age at 50% selection                                                   
  number                delta_fish1_f;                          // age between 50% selection and 95% selection....
  vector                fish1_sel_f(1,nages);                   // vector of fishery selectivty parameters on arithmetic scale

  init_bounded_number   log_a50_fish1_m(-0.5,3.5,ph_fish1_sel);  // age at 50% selection                                                   
  init_bounded_number   log_delta_fish1_m(-2,4,-ph_fish1_sel);   //khf turned off // age between 50% selection and 95% selection....
  number                a50_fish1_m;                         // age at 50% selection                                                   
  number                delta_fish1_m;                       // age between 50% selection and 95% selection....
  vector                fish1_sel_m(1,nages);                // vector of fishery selectivty parameters on arithmetic scale

  init_bounded_number   log_a50_fish2(-10,10,-1);       // age at 50% selection                                                   
  init_bounded_number   log_delta_fish2(-10,10,-1);     
  number                a50_fish2;                      // age at 50% selection                                                   
  number                delta_fish2;                    
  vector                fish2_sel(1,nages);             // vector of fishery selectivty parameters on arithmetic scale

  init_bounded_number   log_a50_fish3_f(-4,4,4);        // age at 50% selection                                                   
  init_bounded_number 	log_delta_fish3_f(-4,4,4);      
  number   				      a50_fish3_f; 										// age at 50% selection                                                   
  number 				        delta_fish3_f; 									
  vector 				        log_fish3_sel_f(1,nages);				// vector of fishery selectivy log parameters including those not estimated
  vector 				        fish3_sel_f(1,nages);						// vector of fishery selectivty parameters on arithmetic scale
 
  init_bounded_number   log_a50_fish3_m(-4,4,-4);       // age at 50% selection                                                   
  init_bounded_number 	log_delta_fish3_m(-4,4,4);   
  number   				      a50_fish3_m; 										// age at 50% selection                                                   
  number 				        delta_fish3_m; 									
  vector 				        log_fish3_sel_m(1,nages);				// vector of fishery selectivy log parameters including those not estimated
  vector 				        fish3_sel_m(1,nages);						// vector of fishery selectivty parameters on arithmetic scale
 
  init_bounded_number   log_a50_fish4_f1(0.1,3,ph_fish4_sel);      // KHF - THIS WAS HITTING BOUND, EXPANDED BOUNDS age at 50% selection                                                   
  init_bounded_number   log_delta_fish4_f1(0.1,4,-ph_fish4_sel);   // KHF - THIS WAS HITTING BOUND, EXPANDED BOUNDS age between 50% selection and 95% selection....
  init_bounded_number   log_a50_fish4_f2(0.7,3,ph_fish4_sel);      // age at 50% selection                                                   
  //init_bounded_number   log_delta_fish4_f2(0.1,3,ph_fish4_sel);     
  init_bounded_number   log_a50_fish4_f3(0.5,3,ph_fish4_sel);      // age at 50% selection                                                   
  //init_bounded_number   log_delta_fish4_f3(0.01,2,ph_fish4_sel);     
  vector                a50_fish4_f(1,nareas);           // age at 50% selection                                                   
  number                delta_fish4_f;         
  matrix                fish4_sel_f(1,nareas,1,nages);   // vectory of fishery selectivty parameters on arithmetic scale

  init_bounded_number   log_a50_fish4_m1(0.1,3,ph_fish4_sel);     // KHF - THIS WAS HITTING BOUND, EXPANDED BOUNDS age at 50% selection                                                   
  init_bounded_number   log_delta_fish4_m1(1,3,ph_fish4_sel);     // KHF - THIS WAS HITTING BOUND, EXPANDED BOUNDS age between 50% selection and 95% selection....
  init_bounded_number   log_a50_fish4_m2(0.1,3,ph_fish4_sel);     // age at 50% selection                                                   
  //init_bounded_number   log_delta_fish4_m2(0.1,3.5,ph_fish4_sel);     
  init_bounded_number   log_a50_fish4_m3(0.7,3,ph_fish4_sel);     // age at 50% selection                                                   
  //init_bounded_number   log_delta_fish4_m3(0.1,4,-ph_fish4_sel);     
  vector                a50_fish4_m(1,nareas);           // age at 50% selection                                                   
  number                delta_fish4_m;         
  matrix                fish4_sel_m(1,nareas,1,nages);   // vector of fishery selectivty parameters on arithmetic scale

  // Survey selectivities (sex-specific, females first then males)
  init_bounded_number   log_a50_srv1_f(-0.5,3.5,ph_srv_sel);     // age at 50% selection                                                   
  init_bounded_number 	log_delta_srv1_f(-0.5,3.5,-ph_srv_sel);  // khf turned off 
  number   	            a50_srv1_f; 									           // age at 50% selection                                                   
  number 	              delta_srv1_f; 										   
  //vector 	              log_srv1_sel_f(1,nages);			       // NOT USED ANYWHERE vector of survey selectivy log parameters including those not estimated
  vector 	              srv1_sel_f(1,nages);				         // vector of survey selectivty parameters on arithmetic scale

  init_bounded_number   log_a50_srv1_m(-0.5,3.5,ph_srv_sel); // age at 50% selection                                                   
  init_bounded_number   log_delta_srv1_m(-5,5,-ph_srv_sel);  //khf turned off
  number                a50_srv1_m;                          // age at 50% selection                                                   
  number                delta_srv1_m;                       
  //vector                log_srv1_sel_m(1,nages);             // NOT USED ANYWHERE vector of survey selectivy log parameters including those not estimated
  vector                srv1_sel_m(1,nages);                 // vector of survey selectivty parameters on arithmetic scale

  init_bounded_number   log_a50_srv2_f(-2,3,-1);             // age at 50% selection                                                   
  init_bounded_number   log_delta_srv2_f(-2,4,-1);           // age between 50% selection and 95% selection....
  number                a50_srv2_f;                          // age at 50% selection                                                   
  number                delta_srv2_f;                        // age between 50% selection and 95% selection....
  vector                log_srv2_sel_f(1,nages);             // vector of survey selectivy log parameters including those not estimated
  vector                srv2_sel_f(1,nages);                 // vector of survey selectivty parameters on arithmetic scale

  init_bounded_number   log_a50_srv2_m(0.5,3.5,-1);          //was-1 phase   // age at 50% selection                   #DH# turned off because it seemed to be estimating poorly                                 
  init_bounded_number   log_delta_srv2_m(-2,10,-1);          // age between 50% selection and 95% selection....
  number                a50_srv2_m;                          // age at 50% selection                                                   
  number                delta_srv2_m;                        // age between 50% selection and 95% selection....
  vector                log_srv2_sel_m(1,nages);             // vector of survey selectivy log parameters including those not estimated
  vector                srv2_sel_m(1,nages);                 // vectory of survey selectivty parameters on arithmetic scale

  // Fishing mortality
  init_bounded_vector  	log_avg_F_fish1(1,nareas,-10,0,ph_avg_F);             // Log average fishing mortality
  init_bounded_matrix  	log_F_devs_fish1(1,nareas,styr,endyr,-6,10,ph_Fdev);  // Annual, spatial fishing mortality deviations
  init_bounded_vector   log_avg_F_fish3(1,nareas,-10,0,ph_avg_F);
  init_bounded_matrix   log_F_devs_fish3(1,nareas,styr,endyr,-10,10,ph_Fdev);
  matrix               	Fmort_fish1(1,nareas,styr,endyr);            // Fishing mortality by year and area
  matrix                Fmort_fish3(1,nareas,styr,endyr);            // Fishing mortality by year and area
  3darray              	Z_f(1,nareas,styr,endyr,1,nages);            // Total female mortality by year and age and area
  3darray               Z_m(1,nareas,styr,endyr,1,nages);            // Total male mortality by year and age and area
  3darray              	F_fish1_f(1,nareas,styr,endyr,1,nages);      // Fishing female mortality by year and age and area
  3darray               F_fish1_m(1,nareas,styr,endyr,1,nages);      // Fishing male mortality by year and age and area 
  3darray               F_fish3_f(1,nareas,styr,endyr,1,nages);      // Fishing female mortality by year and age and area
  3darray               F_fish3_m(1,nareas,styr,endyr,1,nages);      // Fishing male mortality by year and age and area 
  3darray               S_f(1,nareas,styr,endyr,1,nages);            // Survivorship by year and age and area
  3darray               S_m(1,nareas,styr,endyr,1,nages);            // Survivorship by year and age and area
  3darray               S_f_mid(1,nareas,styr,endyr,1,nages);        // Survivorship by year and age and area
  3darray               S_m_mid(1,nareas,styr,endyr,1,nages);        // Survivorship by year and age and area

  // Create a vector of natural mortalities for proj.dat
  // matrix natmort_mtrx(1,nareas,1,nages); // is called natmortv in DHH
  // but it's only used for projout and newproj, which I don't use
  // Numbers at age
  init_bounded_dev_vector  log_rec_dev1(styr-nages+2,endyr_rec_est,-10,10,ph_recdev);  // Recruitment deviations from before the asssessment starts to present, by area
  init_bounded_dev_vector  log_rec_dev2(styr-nages+2,endyr_rec_est,-10,10,ph_recdev);
  init_bounded_dev_vector  log_rec_dev3(styr-nages+2,endyr_rec_est,-10,10,ph_recdev);
  matrix            log_rec_dev(1,nareas,styr-nages+2,endyr_rec_est);  // Combining for looping
  3darray           natage_f(1,nareas,styr,endyr,1,nages);	        // Matrix of numbers at age from start year to end year by area
  3darray           natage_m(1,nareas,styr,endyr,1,nages);          // Matrix of numbers at age from start year to end year by area
  3darray           AMnatage_f(styr,endyr,1,nareas,1,nages);        // Matrix of numbers at age from start year to end year by area
  3darray           AMnatage_m(styr,endyr,1,nareas,1,nages);        // Matrix of numbers at age from start year to end year by area
  vector            rec_a1_f(styr,endyr);                           // for easy reporting of recruitment by area and sex
  vector            rec_a1_m(styr,endyr);
  vector            rec_a2_f(styr,endyr);
  vector            rec_a2_m(styr,endyr);
  vector            rec_a3_f(styr,endyr);
  vector            rec_a3_m(styr,endyr);

// Catch at age
  3darray      catage_fish1_f(1,nareas,styr,endyr,1,nages)   // Matrix of predicted catch at age from start year to endyear by area
  3darray      catage_fish1_m(1,nareas,styr,endyr,1,nages)   // Matrix of predicted catch at age from start year to endyear by area
  3darray      catage_fish3_f(1,nareas,styr,endyr,1,nages)
  3darray      catage_fish3_m(1,nareas,styr,endyr,1,nages)
  matrix       pred_catch_fish1(1,nareas,styr,endyr)	       // Vector of predicted catches
  matrix       pred_catch_fish3(1,nareas,styr,endyr)         // Vector of predicted catches 
  
  sdreport_matrix   pred_srv3(1,nareas,1,nyrs_srv3);     // Predicted survey biomass
  matrix            pred_srv4(1,nareas,1,nyrs_srv4);     // Predicted survey Abundance
  sdreport_matrix   pred_srv5(1,nareas,1,nyrs_srv5);     // Predicted survey Abundance or Biomass
  matrix            pred_srv6(1,nareas,1,nyrs_srv6);     // Predicted survey Abundance or biomass
  //matrix          pred_srv8(1,nareas,styr,endyr);      // Predicted survey Abundance or biomass

  3darray      eac_fish1(1,nareas,1,nyrs_fish1_age,1,nages)  // Expected proportion at age in fish
  3darray      eac_srv1(1,nareas,1,nyrs_srv1_age,1,nages)    // Expected proportion at age in survey
  
  3darray      esc_fish1_m(1,nareas,1,nyrs_fish1_size,1,nlenbins)  // Expected proportion at size in fishery
  3darray      esc_fish1_f(1,nareas,1,nyrs_fish1_size,1,nlenbins)  // Expected proportion at size in fishery
  3darray      esc_fish3_m(1,nareas,1,nyrs_fish3_size,1,nlenbins)  // Expected proportion at size in fishery
  3darray      esc_fish3_f(1,nareas,1,nyrs_fish3_size,1,nlenbins)  // Expected proportion at size in fishery
  
  3darray      esc_srv1_m(1,nareas,1,nyrs_srv1_size,1,nlenbins)    // Expected proportion at size in survey
  3darray      esc_srv1_f(1,nareas,1,nyrs_srv1_size,1,nlenbins)    // Expected proportion at size in survey
  3darray      esc_srv2_m(1,nareas,1,nyrs_srv2_size,1,nlenbins)    // Expected proportion at size in survey
  3darray      esc_srv2_f(1,nareas,1,nyrs_srv2_size,1,nlenbins)    // Expected proportion at size in survey

 // Some calculated variables and standard deviation estimates for some estimated parameters
  sdreport_matrix   tot_biom(1,nareas,styr,endyr);     // Standard deviation report vector of total biomass
  sdreport_number   q_srv1;                            // " " for Survey catchability
  sdreport_number   q_srv2;
  sdreport_vector   q_srv5(1,nareas);
  sdreport_number   q_srv6;
  sdreport_vector   q_srv8(1,nareas);

  //sdreport_vector pred_rec(styr,endyr);		       // " " for predicted recruitments  
  sdreport_matrix   pred_rec(1,nareas,styr,endyr);
  number        pred_rec_all;
  vector      	avg_rec(1,nareas);                 // " " for Average recruitment 
  vector        spbiom_trend(1,nareas);
  vector        Depletion(1,nareas);               // depletion
  sdreport_matrix   spawn_biom(1,nareas,styr,endyr);   // " " for spawning biomass vector
  //matrix      Sp_Biom(1,nareas,styr,endyr);
  vector      	natmort(1,nareas);								 // " " for natural mortality

 // Parameters for computing SPR rates 
  init_bounded_vector 	mF40(1,nareas,0.01,0.9,ph_F40);		// Estimated F40  
  init_bounded_vector 	mF35(1,nareas,0.01,0.9,ph_F40);		// Estimated F35  
  sdreport_vector		F40(1,nareas);					// " " " F40
  vector 				F35(1,nareas);						  // " " " F35
  vector 				SB0(1,nareas);						  // Spawning biomass at no fishing
  vector 				SBF40(1,nareas);					  // " " at F40
  vector 				SBF35(1,nareas);					  // " " at F35
  vector 				sprpen(1,nareas);					  // Likelihood penalty to make ADMB estimate spr rates
  matrix        Nspr0(1,nareas,1,nages)     // Matrix of number of spawners at age at each fishing mortality level
  matrix        Nspr40(1,nareas,1,nages)    // Matrix of number of spawners at age at each fishing mortality level
  matrix        Nspr35(1,nareas,1,nages)    // Matrix of number of spawners at age at each fishing mortality level
  matrix        AMNspr0(1,nareas,1,nages)   // holds Nspr after they are moved
  matrix        AMNspr40(1,nareas,1,nages)  // holds Nspr after they are moved
  matrix        AMNspr35(1,nareas,1,nages)  // holds Nspr after they are moved
  vector        hist_hal_F(1,nareas);       // Option of adding historical proportion of current average hook and line catch

 // Likelihoods and penalty functions  
  matrix        surv_like(1,nareas,1,9);  // Likelihood values for survey biomasses, allowance for up to 3 surveys
  matrix				age_like(1,nareas,1,16);	// Likelihood values for age and size compositions allowance for up to 16 components
  //vector      sel_like(1,12);           // selectivity likelihood, not spatial
  vector				rec_like(1,nareas);			  // Likelihood value for recruitments
  vector				ssqcatch(1,nareas);				// Likelihood value for catch estimation
  vector				F_mort_regularity(1,nareas);	// Penalty value for fishing mortality regularity
  number				avg_sel_penalty;		      // Penalty value for selectivity regularity penalty

 // Priors
  vector				priors(1,17);				// Prior penalty values for sigr,q,natural mortality,steepness  
 // Define an objective function
  number				Like;							  // Likelihood for data fits
  objective_function_value obj_fun;	// Total likelihood for objective function value
  matrix 				pred_catch(1,nareas,styr,endyr);
  vector        fratio(1,nareas);
  sdreport_number   B40_all;        // going to try to create an average B40 across areas too
  number        B40_all_alt;

  ///////////////////////////////////////////
  // Population projection Hanselsiginelli
  //////////////////////////////////////////
   
  3darray       N_proj_f(1,nareas,endyr+1,endyr+projyrs,1,nages);  // 16 years of F numbers at age
  3darray       N_proj_m(1,nareas,endyr+1,endyr+projyrs,1,nages);
  3darray       TN_proj_f(1,nareas,endyr+1,endyr+projyrs,1,nages);  // 16 years of F numbers at age
  3darray       TN_proj_m(1,nareas,endyr+1,endyr+projyrs,1,nages);
  3darray       AM_N_proj_f(endyr+1,endyr+projyrs,1,nareas,1,nages);
  3darray       AM_N_proj_m(endyr+1,endyr+projyrs,1,nareas,1,nages);
  vector        FABC_proj(1,nareas);                                // after control rule F
  matrix        FABC_tot_proj_f(1,nareas,1,nages);  
  matrix        FABC_tot_proj_m(1,nareas,1,nages);
  vector        FOFL_proj(1,nareas);
  matrix        FOFL_tot_proj_f(1,nareas,1,nages);
  matrix        FOFL_tot_proj_m(1,nareas,1,nages);
  //sdreport_vector ABC(1,nareas);
  //sdreport_vector B40(1,nareas);               
  sdreport_vector        ABC(1,nareas);
  sdreport_vector        B40(1,nareas); 
  vector        B40_alt(1,nareas);
  vector        OFL(1,nareas);
  matrix        Z_proj_f(1,nareas,1,nages);
  matrix        Z_proj_m(1,nareas,1,nages);
  matrix        ZOFL_proj_f(1,nareas,1,nages);
  matrix        ZOFL_proj_m(1,nareas,1,nages);

  matrix        S_proj_f(1,nareas,1,nages);
  matrix        S_proj_m(1,nareas,1,nages);
  3darray       catage_proj_f(1,nareas,endyr+1,endyr+projyrs,1,nages);
  3darray       catage_proj_m(1,nareas,endyr+1,endyr+projyrs,1,nages);
  3darray       catage_proj_OFL_f(1,nareas,endyr+1,endyr+projyrs,1,nages);
  3darray       catage_proj_OFL_m(1,nareas,endyr+1,endyr+projyrs,1,nages);
  matrix        pred_catch_proj_OFL_f(1,nareas,endyr+1,endyr+projyrs);
  matrix        pred_catch_proj_OFL_m(1,nareas,endyr+1,endyr+projyrs);
  //sdreport_matrix   spawn_biom_proj(1,nareas,endyr+1,endyr+15);
  //sdreport_matrix   tot_biom_proj(1,nareas,endyr+1,endyr+15);
  sdreport_matrix     spawn_biom_proj(1,nareas,endyr+1,endyr+projyrs);
  sdreport_matrix     tot_biom_proj(1,nareas,endyr+1,endyr+projyrs);
  matrix        pred_catch_proj(1,nareas,endyr+1,endyr+projyrs);
  matrix        pred_catch_proj_OFL(1,nareas,endyr+1,endyr+projyrs);
  vector        stdev_rec(1,nareas);  
  vector        FOFL(1,nareas);
  vector        FABC(1,nareas);
  vector        FOFL2(1,nareas);
  vector        FABC2(1,nareas); 
   
 PROCEDURE_SECTION
  l=l+1; // Initiate counter for random seeds in projection
   
	   Get_Selectivity();										// Call function to get selectivities
     Get_Mortality_Rates();								// Call function to get fishing and natural mortality
     //Get_Bzero();
     Get_Numbers_At_Age();								// Call function to get numbers at age per year
     Get_Catch_at_Age();									// Call function to get catch at age per year
     Get_Predicted_Values();							// Get predicted values for catch, survbio, age and size comps
     Calc_priors();												// Solve for priors
     if(last_phase()) {
     Get_Dependent_Vars();								// Solve for dependent variables like total bio, recruitment etc.
     compute_spr_rates();									// Compute f40 etc. 
     Get_Population_Projection(); }
     Evaluate_Objective_Function();       // Minimize objective function value
     if (mceval_phase())									// For outputting MCMC simulations in text format 
    
    //evalout1
    //{
     //evalout<<log_mean_rec1<<" "<<q_srv1<<" "<<q_srv2<<" "<<q_srv5<<" "<<q_srv6<<" "<<q_srv8<<" "<<obj_fun<<" "<<F40<<" "<<B40_all<<" "<<B40<<" "<<endl;
     //}  
  
    //evalout2
    //  {
     //evalout<<spawn_biom<<" "<<endl;
     //}  
  
     //evalout3
      {
     evalout<<pred_rec<<" "<<endl;
     }  

FUNCTION Get_Selectivity
  // Fishery selectivity
  // Selectivity does not change for ages greater than n_fish_sel_ages
  // Currently only single logistic is available for all, except a 2-parameter gamma is used for fish3 (trawl)
  a50_fish1_f=mfexp(log_a50_fish1_f);           
  a50_fish1_m=mfexp(log_a50_fish1_m);
  delta_fish1_f=mfexp(log_delta_fish1_f);   // age between 50% selection and 95% selection....
  delta_fish1_m=mfexp(log_delta_fish1_m);   // age between 50% selection and 95% selection....

  a50_fish2=mfexp(log_a50_fish2); 
  delta_fish2=mfexp(log_delta_fish2);  // so if log_delta_fish2 is fixed at 0, then delta is 1...

  a50_fish3_f=mfexp(log_a50_fish3_f);
  a50_fish3_m=mfexp(log_a50_fish3_f);  // should this be _m? NO, it's fine as is
  delta_fish3_f=mfexp(log_delta_fish3_f);
  delta_fish3_m=mfexp(log_delta_fish3_m);
  
  a50_fish4_f(1)=mfexp(log_a50_fish4_f1);
  a50_fish4_f(2)=mfexp(log_a50_fish4_f2);
  a50_fish4_f(3)=mfexp(log_a50_fish4_f3);
  a50_fish4_m(1)=mfexp(log_a50_fish4_m1);
  a50_fish4_m(2)=mfexp(log_a50_fish4_m2);
  a50_fish4_m(3)=mfexp(log_a50_fish4_m3);

  delta_fish4_f=mfexp(log_delta_fish4_f1);
  //delta_fish4_f(2)=mfexp(log_delta_fish4_f2);    
  //delta_fish4_f(3)=mfexp(log_delta_fish4_f3); 
  delta_fish4_m=mfexp(log_delta_fish4_m1);
  //delta_fish4_m(2)=mfexp(log_delta_fish4_m2);
  //delta_fish4_m(3)=mfexp(log_delta_fish4_m3);

  a50_srv1_f=mfexp(log_a50_srv1_f);
  a50_srv1_m=mfexp(log_a50_srv1_m);
  delta_srv1_f=mfexp(log_delta_srv1_f); 
  delta_srv1_m=mfexp(log_delta_srv1_m);  
 
  a50_srv2_f=mfexp(log_a50_srv2_f);       // dhh only a50_srv2_f is turned on
  a50_srv2_m=mfexp(log_a50_srv2_f); 
  delta_srv2_f=mfexp(log_delta_srv2_m);   // dhh only delta_srv2_m is turned on 
  delta_srv2_m=mfexp(log_delta_srv2_m);
  
  // selectivities:
  for (j=1;j<=nages;j++) {
    fish1_sel_f(j)=1/ (1+mfexp(-delta_fish1_f*(j-a50_fish1_f))); 
    fish1_sel_m(j)=1/ (1+mfexp(-delta_fish1_m*(j-a50_fish1_m)));}

  for (i=1;i<=nareas;i++) {
   for (j=1;j<=nages;j++) {   // logistic (formerly opt 2)
  // logistic (formerly opt 2)
    fish4_sel_f(i,j)=1/ (1+mfexp(-delta_fish4_f*(j-a50_fish4_f(i))));  
    fish4_sel_m(i,j)=1/ (1+mfexp(-delta_fish4_m*(j-a50_fish4_m(i)))); }}

  for (j=1;j<=nages;j++) {   // currently, single-area selectivity 
  // logistic (formerly opt 2)  
    fish2_sel(j)=1/ (1+mfexp(-delta_fish2*(double(j)-a50_fish2))); 
  // 2-parameter gamma (formerly opt 3)
  // Punt et. al 1996 gamma parameterization
    fish3_sel_f(j)=(pow(j/a50_fish3_f,a50_fish3_f/(0.5*(sqrt(square(a50_fish3_f)+4*square(delta_fish3_f))-a50_fish3_f)))*mfexp((a50_fish3_f-j)/(0.5*(sqrt(square(a50_fish3_f)+4*square(delta_fish3_f))-a50_fish3_f))));
    fish3_sel_m(j)=(pow(j/a50_fish3_m,a50_fish3_m/(0.5*(sqrt(square(a50_fish3_m)+4*square(delta_fish3_m))-a50_fish3_m)))*mfexp((a50_fish3_m-j)/(0.5*(sqrt(square(a50_fish3_m)+4*square(delta_fish3_m))-a50_fish3_m)))); 
  // logistic (formerly opt 2) 
    srv1_sel_f(j)=1/ (1+mfexp(-delta_srv1_f*(j-a50_srv1_f)));
    srv1_sel_m(j)=1/ (1+mfexp(-delta_srv1_m*(j-a50_srv1_m))); 
  // logistic (formerly opt 2) 
    srv2_sel_f(j)=1/ (1+mfexp(-delta_srv2_f*(j-a50_srv2_f)));
    srv2_sel_m(j)=1/ (1+mfexp(-delta_srv2_m*(j-a50_srv2_m))); 
     }

FUNCTION Get_Mortality_Rates
// Calculate mortality rates, not sex-specific mortality                
  natmort(1)   = exp(logm1);   // setting natural mortality to arithmetic scale
  natmort(2)   = exp(logm2);
  natmort(3)   = exp(logm3);  

  for (j=1;j<=nareas;j++) {
  Fmort_fish1(j) = mfexp(log_avg_F_fish1(j) + log_F_devs_fish1(j));	// setting fishing mortaltiy to arithmetic scale
  Fmort_fish3(j) = mfexp(log_avg_F_fish3(j) + log_F_devs_fish3(j));
  hist_hal_F(j) = hist_hal_prop(j)*mfexp(log_avg_F_fish1(j));       // optional historical fishing mortality for initial age comps
  }

  for (j=1;j<=nareas;j++) {
   for (iyr=styr; iyr<=1994; iyr++) {
    for (k = 1; k<= nages; k++) {
      F_fish1_f(j,iyr,k) = Fmort_fish1(j,iyr) * fish1_sel_f(k);			// Getting fully selected FEMALE fishing mortality
			F_fish1_m(j,iyr,k) = Fmort_fish1(j,iyr) * fish1_sel_m(k);  		// Getting fully selected MALE fishing mortality
      F_fish3_m(j,iyr,k) = Fmort_fish3(j,iyr) * fish3_sel_m(k);
      F_fish3_f(j,iyr,k) = Fmort_fish3(j,iyr) * fish3_sel_f(k);       
   }}}

   for (j=1;j<=nareas;j++) {
    for (iyr=1995; iyr<=endyr;iyr++) {
      for (k=1;k<=nages;k++) {
        if(ph_ifq==1) {  // so if ph_ifq is 1 (which is is currently), then do line 1165 and 1166, otherwise go to 1168?
           F_fish1_f(j,iyr,k)=Fmort_fish1(j,iyr)*fish4_sel_f(j,k);  
           F_fish1_m(j,iyr,k)=Fmort_fish1(j,iyr)*fish4_sel_m(j,k); }
        else {
           F_fish1_f(j,iyr,k)=Fmort_fish1(j,iyr)*fish1_sel_f(k);
           F_fish1_m(j,iyr,k)=Fmort_fish1(j,iyr)*fish1_sel_m(k); } 
           F_fish3_f(j,iyr,k)=Fmort_fish3(j,iyr)*fish3_sel_f(k);
           F_fish3_m(j,iyr,k)=Fmort_fish3(j,iyr)*fish3_sel_m(k); }}}

  for (j=1;j<=nareas;j++) {
    Z_f(j)        = F_fish1_f(j) + F_fish3_f(j) + natmort(j);             // Fully selected total mortality by area, year and age
    // in DHH model, F_fish1_m also includes mdelta for a sex-specific M option. I'm not doing it here.
    Z_m(j)        = F_fish1_m(j) + F_fish3_m(j) + natmort(j);             // Fully selected total mortality by area, year and age
  }
   
   S_f         = mfexp(-1.0*Z_f);	// Fully selected survival
   S_f_mid     = mfexp(-0.5*Z_f);
   S_m         = mfexp(-1.0*Z_m); // Fully selected survival
   S_m_mid     = mfexp(-0.5*Z_m);

// Next two sections are based on Baranov catch equations
FUNCTION Get_Numbers_At_Age  
// Calculate Numbers at age
// only using rec_like_type 2, the rest of the options are not coded in 
// Starting year of the model (not the start of a given calendar year)
  log_rec_dev(1) = log_rec_dev1;
  log_rec_dev(2) = log_rec_dev2;
  log_rec_dev(3) = log_rec_dev3;
  log_mean_rec(1) = log_mean_rec1;
  log_mean_rec(2) = log_mean_rec1;
  log_mean_rec(3) = log_mean_rec1;

  //fill in starting year (1977)
 for(i=1;i<=nareas;i++) {  //1977 age group 1
     natage_f(i,styr,1) = mfexp(log_mean_rec(i)+log_rec_dev(i,styr)+sigr*sigr/2)/2;
     natage_m(i,styr,1) = mfexp(log_mean_rec(i)+log_rec_dev(i,styr)+sigr*sigr/2)/2; }

   for (j=2;j<nages;j++) {
     for(i=1;i<=nareas;i++) {
        itmp = styr+1-j;       // 1977 fill in the rest of the age groups, 2 to 29, for the starting year
      natage_f(i,styr,j) = (mfexp(log_mean_rec(i) - (natmort(i)+hist_hal_F(i)*fish1_sel_f(j)) * double(j-1)+ log_rec_dev(i,itmp)+sigr*sigr/2))/2; 
      natage_m(i,styr,j) = (mfexp(log_mean_rec(i) - (natmort(i)+hist_hal_F(i)*fish1_sel_m(j)) * double(j-1)+ log_rec_dev(i,itmp)+sigr*sigr/2))/2; 
      //fill in nages
      natage_f(i,styr,nages) = (mfexp(log_mean_rec(i) - (natmort(i)+hist_hal_F(i)*fish1_sel_f(nages-1)) * (nages-1))/ (1. - exp(-natmort(i)+hist_hal_F(i)*fish1_sel_f(nages-1)) ))/2; //1977 age group 30
      natage_m(i,styr,nages) = (mfexp(log_mean_rec(i) - (natmort(i)+hist_hal_F(i)*fish1_sel_m(nages-1)) * (nages-1))/ (1. - exp(-natmort(i)+hist_hal_F(i)*fish1_sel_m(nages-1)) ))/2; 
      }
      for (i=1;i<=nareas;i++) {
      AMnatage_f(styr,i,j)=natage_f(i,styr,j);
      AMnatage_m(styr,i,j)=natage_m(i,styr,j); 
     } }
   

  //fill in age 1 for rest of the years (the middle years)
  for (i=styr; i<= endyr_rec; i++) { 
    natage_f(1,i,1)           = mfexp(log_rec_dev(1,i) + log_mean_rec(1)+sigr*sigr/2 )/2;    
    natage_m(1,i,1)           = mfexp(log_rec_dev(1,i) + log_mean_rec(1)+sigr*sigr/2 )/2;
    natage_f(2,i,1)           = mfexp(log_rec_dev(2,i) + log_mean_rec(2)+sigr*sigr/2 )/2;
    natage_m(2,i,1)           = mfexp(log_rec_dev(2,i) + log_mean_rec(2)+sigr*sigr/2 )/2;
    natage_f(3,i,1)           = mfexp(log_rec_dev(3,i) + log_mean_rec(3)+sigr*sigr/2 )/2;
    natage_m(3,i,1)           = mfexp(log_rec_dev(3,i) + log_mean_rec(3)+sigr*sigr/2 )/2;
 }
  //fill in ages and years   
  for (i=styr; i<= endyr_rec; i++) {  //ages 2-6
    natage_f(1,i+1)(2,6)  = ++elem_prod(natage_f(1,i)(1,5),S_f(1,i)(1,5));   // Following year
    natage_m(1,i+1)(2,6)  = ++elem_prod(natage_m(1,i)(1,5),S_m(1,i)(1,5));   // Following year
    natage_f(2,i+1)(2,6)  = ++elem_prod(natage_f(2,i)(1,5),S_f(2,i)(1,5));   // Following year
    natage_m(2,i+1)(2,6)  = ++elem_prod(natage_m(2,i)(1,5),S_m(2,i)(1,5));   // Following year
    natage_f(3,i+1)(2,6)  = ++elem_prod(natage_f(3,i)(1,5),S_f(3,i)(1,5));   // Following year
    natage_m(3,i+1)(2,6)  = ++elem_prod(natage_m(3,i)(1,5),S_m(3,i)(1,5));   // Following year
  // ages 7-14
    natage_f(1,i+1)(7,14)  = ++elem_prod(natage_f(1,i)(6,14-1),S_f(1,i)(6,14-1));   // Following year
    natage_m(1,i+1)(7,14)  = ++elem_prod(natage_m(1,i)(6,14-1),S_m(1,i)(6,14-1));   // Following year
    natage_f(2,i+1)(7,14)  = ++elem_prod(natage_f(2,i)(6,14-1),S_f(2,i)(6,14-1));   // Following year
    natage_m(2,i+1)(7,14)  = ++elem_prod(natage_m(2,i)(6,14-1),S_m(2,i)(6,14-1));   // Following year
    natage_f(3,i+1)(7,14)  = ++elem_prod(natage_f(3,i)(6,14-1),S_f(3,i)(6,14-1));   // Following year
    natage_m(3,i+1)(7,14)  = ++elem_prod(natage_m(3,i)(6,14-1),S_m(3,i)(6,14-1));   // Following year
  // ages 15+
    natage_f(1,i+1)(15,nages)  = ++elem_prod(natage_f(1,i)(14,nages-1),S_f(1,i)(14,nages-1));   // Following year
    natage_m(1,i+1)(15,nages)  = ++elem_prod(natage_m(1,i)(14,nages-1),S_m(1,i)(14,nages-1));   // Following year
    natage_f(2,i+1)(15,nages)  = ++elem_prod(natage_f(2,i)(14,nages-1),S_f(2,i)(14,nages-1));   // Following year
    natage_m(2,i+1)(15,nages)  = ++elem_prod(natage_m(2,i)(14,nages-1),S_m(2,i)(14,nages-1));   // Following year
    natage_f(3,i+1)(15,nages)  = ++elem_prod(natage_f(3,i)(14,nages-1),S_f(3,i)(14,nages-1));   // Following year
    natage_m(3,i+1)(15,nages)  = ++elem_prod(natage_m(3,i)(14,nages-1),S_m(3,i)(14,nages-1));   // Following year
    // nages
    natage_f(1,i+1,nages)    += natage_f(1,i,nages)*S_f(1,i,nages);
    natage_m(1,i+1,nages)    += natage_m(1,i,nages)*S_m(1,i,nages);
    natage_f(2,i+1,nages)    += natage_f(2,i,nages)*S_f(2,i,nages);
    natage_m(2,i+1,nages)    += natage_m(2,i,nages)*S_m(2,i,nages);
    natage_f(3,i+1,nages)    += natage_f(3,i,nages)*S_f(3,i,nages);
    natage_m(3,i+1,nages)    += natage_m(3,i,nages)*S_m(3,i,nages);

     for(j=1;j<=6;j++){ 
     for (k=1;k<=nareas;k++) {
     AMnatage_f(i,k,j)=natage_f(k,i,j);
     AMnatage_m(i,k,j)=natage_m(k,i,j); }
     AMnatage_f(i)=trans(phi(1))*AMnatage_f(i) ;
     AMnatage_m(i)=trans(phi(1))*AMnatage_m(i) ; 
     for (k=1;k<=nareas;k++) {
     natage_f(k,i,j)=AMnatage_f(i,k,j);
     natage_m(k,i,j)=AMnatage_m(i,k,j); }}
     
     for(j=7;j<=14;j++){ 
     for (k=1;k<=nareas;k++) {
     AMnatage_f(i,k,j)=natage_f(k,i,j);
     AMnatage_m(i,k,j)=natage_m(k,i,j); }
     AMnatage_f(i)=trans(phi(2))*AMnatage_f(i) ;
     AMnatage_m(i)=trans(phi(2))*AMnatage_m(i) ; 
     for (k=1;k<=nareas;k++) {
     natage_f(k,i,j)=AMnatage_f(i,k,j);
     natage_m(k,i,j)=AMnatage_m(i,k,j); }}
     
     for(j=15;j<=nages;j++){ 
     for (k=1;k<=nareas;k++) {
     AMnatage_f(i,k,j)=natage_f(k,i,j);
     AMnatage_m(i,k,j)=natage_m(k,i,j); }
     AMnatage_f(i)=trans(phi(3))*AMnatage_f(i) ;
     AMnatage_m(i)=trans(phi(3))*AMnatage_m(i) ; 
     for (k=1;k<=nareas;k++) {
     natage_f(k,i,j)=AMnatage_f(i,k,j);
     natage_m(k,i,j)=AMnatage_m(i,k,j); }}          
 } 

 // final year
  for (i=endyr_rec+1;i<endyr;i++) {
    for (k=1;k<=nareas;k++){
    natage_f(k,i,1)=mfexp(log_mean_rec(k)+sigr*sigr/2)/2;
    natage_m(k,i,1)=mfexp(log_mean_rec(k)+sigr*sigr/2)/2;
    natage_f(k,i+1)(2,nages)  = ++elem_prod(natage_f(k,i)(1,nages-1),S_f(k,i)(1,nages-1));       // Following year
    natage_m(k,i+1)(2,nages)  = ++elem_prod(natage_m(k,i)(1,nages-1),S_m(k,i)(1,nages-1));       // Following year
    natage_f(k,i+1,nages)    += natage_f(k,i,nages)*S_f(k,i,nages);
    natage_m(k,i+1,nages)    += natage_m(k,i,nages)*S_m(k,i,nages);
  }
      for (j=1;j<=6;j++){ 
       for (k=1;k<=nareas;k++) {
          AMnatage_f(endyr,k,j)=natage_f(k,endyr,j);
          AMnatage_m(endyr,k,j)=natage_m(k,endyr,j); }
          AMnatage_f(endyr)=trans(phi(1))*AMnatage_f(endyr) ;
          AMnatage_m(endyr)=trans(phi(1))*AMnatage_m(endyr) ; 
         for (k=1;k<=nareas;k++) {
          natage_f(k,endyr)=AMnatage_f(endyr,k);
          natage_m(k,endyr)=AMnatage_m(endyr,k); } }

      for (j=7;j<=14;j++){ 
       for (k=1;k<=nareas;k++) {
          AMnatage_f(endyr,k,j)=natage_f(k,endyr,j);
          AMnatage_m(endyr,k,j)=natage_m(k,endyr,j); }
          AMnatage_f(endyr)=trans(phi(2))*AMnatage_f(endyr) ;
          AMnatage_m(endyr)=trans(phi(2))*AMnatage_m(endyr) ; 
         for (k=1;k<=nareas;k++) {
          natage_f(k,endyr)=AMnatage_f(endyr,k);
          natage_m(k,endyr)=AMnatage_m(endyr,k); } }

      for (j=15;j<=nages;j++){ 
       for (k=1;k<=nareas;k++) {
          AMnatage_f(endyr,k,j)=natage_f(k,endyr,j);
          AMnatage_m(endyr,k,j)=natage_m(k,endyr,j); }
          AMnatage_f(endyr)=trans(phi(3))*AMnatage_f(endyr) ;
          AMnatage_m(endyr)=trans(phi(3))*AMnatage_m(endyr) ; 
         for (k=1;k<=nareas;k++) {
          natage_f(k,endyr)=AMnatage_f(endyr,k);
          natage_m(k,endyr)=AMnatage_m(endyr,k); } }                    
   }

  //filling in endyr age 1 and moving endyr
  for (k=1;k<=nareas;k++) {
   natage_f(k,endyr,1)         = mfexp(log_mean_rec(k) )/2; 
   natage_m(k,endyr,1)         = mfexp(log_mean_rec(k) )/2; }

    for (j=1;j<=6;j++){ 
       for (k=1;k<=nareas;k++) {
          AMnatage_f(endyr,k,j)=natage_f(k,endyr,j);
          AMnatage_m(endyr,k,j)=natage_m(k,endyr,j); }
          AMnatage_f(endyr)=trans(phi(1))*AMnatage_f(endyr) ;
          AMnatage_m(endyr)=trans(phi(1))*AMnatage_m(endyr) ; 
         for (k=1;k<=nareas;k++) {
          natage_f(k,endyr,j)=AMnatage_f(endyr,k,j);
          natage_m(k,endyr,j)=AMnatage_m(endyr,k,j); } }
    for (j=7;j<=14;j++){ 
       for (k=1;k<=nareas;k++) {
          AMnatage_f(endyr,k,j)=natage_f(k,endyr,j);
          AMnatage_m(endyr,k,j)=natage_m(k,endyr,j); }
          AMnatage_f(endyr)=trans(phi(2))*AMnatage_f(endyr) ;
          AMnatage_m(endyr)=trans(phi(2))*AMnatage_m(endyr) ; 
         for (k=1;k<=nareas;k++) {
          natage_f(k,endyr,j)=AMnatage_f(endyr,k,j);
          natage_m(k,endyr,j)=AMnatage_m(endyr,k,j); } }
    for (j=15;j<=nages;j++){ 
       for (k=1;k<=nareas;k++) {
          AMnatage_f(endyr,k,j)=natage_f(k,endyr,j);
          AMnatage_m(endyr,k,j)=natage_m(k,endyr,j); }
          AMnatage_f(endyr)=trans(phi(3))*AMnatage_f(endyr) ;
          AMnatage_m(endyr)=trans(phi(3))*AMnatage_m(endyr) ; 
         for (k=1;k<=nareas;k++) {
          natage_f(k,endyr,j)=AMnatage_f(endyr,k,j);
          natage_m(k,endyr,j)=AMnatage_m(endyr,k,j); } }          

    for (i=endyr_rec;i<endyr;i++) {
      spawn_biom(1,i) = elem_prod(natage_f(1,i),pow(S_f(1,i),spawn_fract)) * wt_mature;          
      spawn_biom(2,i) = elem_prod(natage_f(2,i),pow(S_f(2,i),spawn_fract)) * wt_mature;          
      spawn_biom(3,i) = elem_prod(natage_f(3,i),pow(S_f(3,i),spawn_fract)) * wt_mature;          
      }
      for(j=1;j<=nareas;j++) {     
      spawn_biom(j,endyr)  = elem_prod(natage_f(j,endyr),pow(S_f(j,endyr),spawn_fract)) * wt_mature;  }
    
  // pull out recruitment numbers by sex and area for ease of reporting 
  rec_a1_f = trans(natage_f(1))(1);
  rec_a1_m = trans(natage_m(1))(1);
  rec_a2_f = trans(natage_f(2))(1);
  rec_a2_m = trans(natage_m(2))(1);
  rec_a3_f = trans(natage_f(3))(1);
  rec_a3_m = trans(natage_m(3))(1);
 

FUNCTION Get_Catch_at_Age
// Calculate catch at age
  pred_catch_fish1.initialize(); 
  pred_catch_fish3.initialize();  
  for(j=1;j<=nareas;j++) {
    for (iyr=styr; iyr<=endyr; iyr++) {
    catage_fish1_m(j,iyr) = elem_div(elem_prod(elem_prod(natage_m(j,iyr),F_fish1_m(j,iyr)),(1.-S_m(j,iyr))),Z_m(j,iyr));
    catage_fish1_f(j,iyr) = elem_div(elem_prod(elem_prod(natage_f(j,iyr),F_fish1_f(j,iyr)),(1.-S_f(j,iyr))),Z_f(j,iyr));
    pred_catch_fish1(j,iyr) = elem_div(elem_prod(elem_prod(natage_m(j,iyr),F_fish1_m(j,iyr)),(1.-S_m(j,iyr))),Z_m(j,iyr))*wt_m + elem_div(elem_prod(elem_prod(natage_f(j,iyr),F_fish1_f(j,iyr)),(1.-S_f(j,iyr))),Z_f(j,iyr))*wt_f;
 
    catage_fish3_m(j,iyr) = elem_div(elem_prod(elem_prod(natage_m(j,iyr),F_fish3_m(j,iyr)),(1.-S_m(j,iyr))),Z_m(j,iyr));
    catage_fish3_f(j,iyr) = elem_div(elem_prod(elem_prod(natage_f(j,iyr),F_fish3_f(j,iyr)),(1.-S_f(j,iyr))),Z_f(j,iyr));
    pred_catch_fish3(j,iyr) = elem_div(elem_prod(elem_prod(natage_m(j,iyr),F_fish3_m(j,iyr)),(1.-S_m(j,iyr))),Z_m(j,iyr))*wt_m + elem_div(elem_prod(elem_prod(natage_f(j,iyr),F_fish3_f(j,iyr)),(1.-S_f(j,iyr))),Z_f(j,iyr))*wt_f;
    }}

FUNCTION Get_Dependent_Vars
    pred_rec.initialize();
    tot_biom.initialize();    
    spawn_biom.initialize(); 

  for(j=1;j<=nareas;j++) {
   for (i=styr;i<=endyr;i++) {  // - or += here:
    pred_rec(j,i) = natage_f(j,i,1) + natage_m(j,i,1);  						// Setting up results based on estimated paramters  
    tot_biom(j,i) += wt_f * natage_f(j,i) + natage_m(j,i) * wt_m;		// Total biomass results
    spawn_biom(j,i) = wt_mature*natage_f(j,i);   		                // Spawning biomass result  ##DH took out your + from "+=" that was causing huge spbiom mainly because you weren't initializing above
    }}

    for(j=1;j<=nareas;j++){
    avg_rec(j)      = mean(pred_rec(j));
    Depletion(j)    = spawn_biom(j,endyr)/spawn_biom(j,styr);       
    spbiom_trend(j) = spawn_biom(j,endyr)/spawn_biom(j,endyr-1);
    }

FUNCTION Get_Predicted_Values
// Calculate predicted data values, currently seems to be estimating a single catchability, not spatial q, but will give a spatial prediction for predicted surv1 biomass
 pred_catch.initialize();
   q_srv1 = exp(log_q_srv1);        // Survey catchability at arithmetic scale
   q_srv2 = exp(log_q_srv2);
   q_srv5(1) = exp(log_q_srv5_1);   // Survey catchability at arithmetic scale
   q_srv5(2) = exp(log_q_srv5_2);
   q_srv5(3) = exp(log_q_srv5_3);   
   q_srv6 = exp(log_q_srv6);        // Survey catchability at arithmetic scale
   q_srv8(1) = exp(log_q_srv8_1);   // Survey catchability at arithmetic scale
   q_srv8(2) = exp(log_q_srv8_2);
   q_srv8(3) = exp(log_q_srv8_3);   
   //q_srv9 = exp(log_q_srv9);      // Survey catchability at arithmetic scale #dh# get rid of 

 for(j=1;j<=nareas;j++) {           
  for (i=1;i<=nyrs_srv3;i++) { 
    // prop_m was originally in these calc instead of 0.5, but prop_m was just 0.5 in the dat file.
    pred_srv3(j,i) = 2*(q_srv1 * (1.-0.5)*(elem_prod(S_f_mid(j,yrs_srv3(i)),natage_f(j,yrs_srv3(i)))*srv1_sel_f)+q_srv1 *0.5* (elem_prod(S_m_mid(j,yrs_srv3(i)),natage_m(j,yrs_srv3(i)))*srv1_sel_m));  } // Predicted Survey biomass
  
  for (i=1;i<=nyrs_srv6;i++) { 
    pred_srv6(j,i) = q_srv6 * (elem_prod(S_f_mid(j,yrs_srv6(i)),natage_f(j,yrs_srv6(i)))*elem_prod(wt_f,fish2_sel)+ elem_prod(S_m_mid(j,yrs_srv6(i)),natage_m(j,yrs_srv6(i)))*elem_prod(wt_m,fish2_sel)); }  // Predicted Survey biomass
  
  for (i=1;i<=nyrs_srv4;i++)  pred_srv4(j,i) = 2*(q_srv2* (1.-0.5)*(elem_prod(S_f_mid(j,yrs_srv4(i)),natage_f(j,yrs_srv4(i)))*srv2_sel_f)+q_srv2 *0.5*(elem_prod(S_m_mid(j,yrs_srv4(i)),natage_m(j,yrs_srv4(i)))*srv2_sel_m));   // Predicted Survey biomass <- but this is ABUNDANCE, not biomass!

  for (i=1;i<=5;i++) pred_srv5(j,i) = q_srv5(j) * (elem_prod(S_f_mid(j,yrs_srv5(i)),natage_f(j,yrs_srv5(i)))*elem_prod(fish1_sel_f,wt_f))+q_srv5(j) * (elem_prod(S_m_mid(j,yrs_srv5(i)),natage_m(j,yrs_srv5(i)))*elem_prod(fish1_sel_m,wt_m));    // Predicted Survey biomass
  for (i=6;i<=nyrs_srv5;i++) { 
      if(ph_ifq==1) pred_srv5(j,i) = q_srv8(j) * (elem_prod(S_f_mid(j,yrs_srv5(i)),natage_f(j,yrs_srv5(i)))*elem_prod(fish4_sel_f(j),wt_f))+q_srv8(j) * (elem_prod(S_m_mid(j,yrs_srv5(i)),natage_m(j,yrs_srv5(i)))*elem_prod(fish4_sel_m(j),wt_m));     // Predicted Survey biomass
      else pred_srv5(j,i) = q_srv8(j) * (elem_prod(S_f_mid(j,yrs_srv5(i)),natage_f(j,yrs_srv5(i)))*elem_prod(fish1_sel_f,wt_f))+q_srv8(j) * (elem_prod(S_m_mid(j,yrs_srv5(i)),natage_m(j,yrs_srv5(i)))*elem_prod(fish1_sel_m,wt_m)); }  // Predicted Survey biomass

  for (i=1;i<=nyrs_fish1_age;i++) { // why does dhh divide by 2?  #dh because expected age comps would sum to 2 when you add males and females together but we are only fitting one aggregate comp
    eac_fish1(j,i)  = ((catage_fish1_f(j,yrs_fish1_age(i))/sum(catage_fish1_f(j,yrs_fish1_age(i)))) + (catage_fish1_m(j,yrs_fish1_age(i))/sum(catage_fish1_m(j,yrs_fish1_age(i)))))/2* ageage;      // Predicted Fishery age comps
    eac_fish1(j,i) /= sum(eac_fish1(j,i)); }

  for (i=1;i<=nyrs_srv1_age;i++) {  // the 0.5 not needed here, came from prop_m in DHH model - here for consistency, but could be removed unless prop_m is desired and something other than all 0.5s
   	eac_srv1(j,i)  = ((1.-0.5)*elem_prod(srv1_sel_f,natage_f(j,yrs_srv1_age(i))) + 0.5*elem_prod(srv1_sel_m,natage_m(j,yrs_srv1_age(i))))* ageage;  			// Predicted Survey age comps
   	eac_srv1(j,i) /=sum(eac_srv1(j,i)); }

  //for (i=1;i<=nyrs_srv2_age;i++) {
    //eac_srv2(j,i)  = (elem_prod(srv2_sel_f,natage_f(j,yrs_srv2_age(i)))+elem_prod(srv2_sel_m,natage_m(j,yrs_srv2_age(i))))*ageage;                        // Predicted Survey age comps
    //eac_srv2(j,i) /=sum(eac_srv2(j,i)); }

  for (i=1;i<=5;i++) {                      // Lets you use a second size-age matrix for part of it  
    esc_fish1_m(j,i)  = catage_fish1_m(j,yrs_fish1_size(i))/sum(catage_fish1_m(j,yrs_fish1_size(i)))* sizeage_m;    
    esc_fish1_f(j,i)  = catage_fish1_f(j,yrs_fish1_size(i))/sum(catage_fish1_f(j,yrs_fish1_size(i)))* sizeage_f;   } 
  for (i=6;i<=nyrs_fish1_size;i++) {                      // Lets you use a second matrix for part of it
    esc_fish1_m(j,i)  = catage_fish1_m(j,yrs_fish1_size(i))/sum(catage_fish1_m(j,yrs_fish1_size(i)))* sizeage_m_new;    
    esc_fish1_f(j,i)  = catage_fish1_f(j,yrs_fish1_size(i))/sum(catage_fish1_f(j,yrs_fish1_size(i)))* sizeage_f_new;  }  

  for (i=1;i<=1;i++) {                      // Lets you use a second matrix for part of it
    esc_fish3_m(j,i)  = (catage_fish3_m(j,yrs_fish3_size(i))/sum(catage_fish3_m(j,yrs_fish3_size(i))))* sizeage_m;                                              // Second Predicted Fishery size comps for 80s and 90s
    esc_fish3_f(j,i)  = (catage_fish3_f(j,yrs_fish3_size(i))/sum(catage_fish3_f(j,yrs_fish3_size(i))))* sizeage_f;  }                                             // Second Predicted Fishery size comps for 80s and 90s
  for (i=2;i<=nyrs_fish3_size;i++) {        // Lets you use a second matrix for part of it
    esc_fish3_m(j,i)  = (catage_fish3_m(j,yrs_fish3_size(i))/sum(catage_fish3_m(j,yrs_fish3_size(i))))* sizeage_m_new;                                              // Second Predicted Fishery size comps for 80s and 90s
    esc_fish3_f(j,i)  = (catage_fish3_f(j,yrs_fish3_size(i))/sum(catage_fish3_f(j,yrs_fish3_size(i))))* sizeage_f_new; }                                              // Second Predicted Fishery size comps for 80s and 90s
   
  for ( i=1;i<=5;i++) {
    esc_srv1_m(j,i)  = elem_prod(srv1_sel_m,natage_m(j,yrs_srv1_size(i))) * sizeage_m;        // Predicted Survey size comps (not used in POP model)
    esc_srv1_m(j,i)  /=sum(esc_srv1_m(j,i));
    esc_srv1_f(j,i)  = elem_prod(srv1_sel_f,natage_f(j,yrs_srv1_size(i))) * sizeage_f;        // Predicted Survey size comps (not used in POP model)
    esc_srv1_f(j,i)  /=sum(esc_srv1_f(j,i)); }
  for ( i=6;i<=nyrs_srv1_size;i++) {
    esc_srv1_m(j,i)  = elem_prod(srv1_sel_m,natage_m(j,yrs_srv1_size(i))) * sizeage_m_new;    // Predicted Survey size comps (not used in POP model)
    esc_srv1_m(j,i)  /=sum(esc_srv1_m(j,i));
    esc_srv1_f(j,i)  = elem_prod(srv1_sel_f,natage_f(j,yrs_srv1_size(i))) * sizeage_f_new;    // Predicted Survey size comps (not used in POP model)
    esc_srv1_f(j,i)  /=sum(esc_srv1_f(j,i)); }
    
 for ( i=1;i<=nyrs_srv2_size;i++) {
    esc_srv2_m(j,i)  = elem_prod(srv2_sel_m,natage_m(j,yrs_srv2_size(i)))*  sizeage_m;        // Predicted Survey size comps (not used in POP model)
    esc_srv2_m(j,i)  /=sum(esc_srv2_m(j,i)); 
    esc_srv2_f(j,i)  = elem_prod(srv2_sel_f,natage_f(j,yrs_srv2_size(i))) * sizeage_f;        // Predicted Survey size comps (not used in POP model)
    esc_srv2_f(j,i)  /=sum(esc_srv2_f(j,i)); }
      
    pred_catch(j) += (pred_catch_fish1(j)+pred_catch_fish3(j));  
  }
  
FUNCTION compute_spr_rates
 //Compute SPR Rates 
  for(i=1;i<=nareas;i++) {  
  fratio(i) = Fmort_fish1(i,endyr)/(Fmort_fish1(i,endyr)+Fmort_fish3(i,endyr)); }  // a proportion of fixed to trawl gears
  // Scale F-spr rates to be on full-selected values 
  for(i=1;i<=nareas;i++) {
  F40(i)  = mF40(i)*max(fish4_sel_f(i));
  F35(i)  = mF35(i)*max(fish4_sel_f(i));
  SB0(i) =0;
  SBF40(i)=0;
  SBF35(i)=0; }

  //Nspr0
  pred_rec_all = mean(pred_rec(1)(1979,endyr-recage-1)+pred_rec(2)(1979,endyr-recage-1)+pred_rec(3)(1979,endyr-recage-1))/3;
      //start of the year:
      Nspr0(1,1) = (mean(pred_rec(1)(1979,endyr-recage-1))/(mean(pred_rec(1)(1979,endyr-recage-1)) +mean(pred_rec(2)(1979,endyr-recage-1)) + mean(pred_rec(3)(1979,endyr-recage-1))))*3; 
      Nspr0(2,1) = (mean(pred_rec(2)(1979,endyr-recage-1))/(mean(pred_rec(1)(1979,endyr-recage-1)) +mean(pred_rec(2)(1979,endyr-recage-1)) + mean(pred_rec(3)(1979,endyr-recage-1))))*3; 
      Nspr0(3,1) = (mean(pred_rec(3)(1979,endyr-recage-1))/(mean(pred_rec(1)(1979,endyr-recage-1)) +mean(pred_rec(2)(1979,endyr-recage-1)) + mean(pred_rec(3)(1979,endyr-recage-1))))*3; 

  //kill and then move 
   for (j=2;j<7;j++) {
   for (i=1;i<=nareas;i++){   // for ages 2 to nages-
     Nspr0(i,j)=Nspr0(i,j-1)*mfexp(-1.*natmort(i)); 
     }
      AMNspr0(1,j) = Nspr0(1,j)*phi(1,1,1) + Nspr0(2,j)*phi(1,2,1) + Nspr0(3,j)*phi(1,3,1);
      AMNspr0(2,j) = Nspr0(2,j)*phi(1,2,2) + Nspr0(1,j)*phi(1,1,2) + Nspr0(3,j)*phi(1,3,2);
      AMNspr0(3,j) = Nspr0(3,j)*phi(1,3,3) + Nspr0(1,j)*phi(1,1,3) + Nspr0(2,j)*phi(1,2,3);
     for (i=1;i<=nareas;i++) {
       Nspr0(i,j) = AMNspr0(i,j);
      }
  }

   for (j=7;j<15;j++) {
   for (i=1;i<=nareas;i++){   // for ages 2 to nages-
     Nspr0(i,j)=Nspr0(i,j-1)*mfexp(-1.*natmort(i)); 
     }
      AMNspr0(1,j) = Nspr0(1,j)*phi(2,1,1) + Nspr0(2,j)*phi(2,2,1) + Nspr0(3,j)*phi(2,3,1);
      AMNspr0(2,j) = Nspr0(2,j)*phi(2,2,2) + Nspr0(1,j)*phi(2,1,2) + Nspr0(3,j)*phi(2,3,2);
      AMNspr0(3,j) = Nspr0(3,j)*phi(2,3,3) + Nspr0(1,j)*phi(2,1,3) + Nspr0(2,j)*phi(2,2,3);
     for (i=1;i<=nareas;i++) {
       Nspr0(i,j) = AMNspr0(i,j);
      }
  }

     for (j=15;j<nages;j++) {
   for (i=1;i<=nareas;i++){   // for ages 2 to nages-
     Nspr0(i,j)=Nspr0(i,j-1)*mfexp(-1.*natmort(i)); 
     }
      AMNspr0(1,j) = Nspr0(1,j)*phi(3,1,1) + Nspr0(2,j)*phi(3,2,1) + Nspr0(3,j)*phi(3,3,1);
      AMNspr0(2,j) = Nspr0(2,j)*phi(3,2,2) + Nspr0(1,j)*phi(3,1,2) + Nspr0(3,j)*phi(3,3,2);
      AMNspr0(3,j) = Nspr0(3,j)*phi(3,3,3) + Nspr0(1,j)*phi(3,1,3) + Nspr0(2,j)*phi(3,2,3);
     for (i=1;i<=nareas;i++) {
       Nspr0(i,j) = AMNspr0(i,j);
      }
  }  
  //kill and then move 
  for (j=1; j<=nareas; j++) {  // for age nages
    Nspr0(j,nages)=Nspr0(j,nages-1)*mfexp(-1.*natmort(j))/(1.-mfexp(-1.*natmort(j)));
    }
    AMNspr0(1,nages) = Nspr0(1,nages)*phi(3,1,1) + Nspr0(2,nages)*phi(3,2,1) + Nspr0(3,nages)*phi(3,3,1);
    AMNspr0(2,nages) = Nspr0(2,nages)*phi(3,2,2) + Nspr0(1,nages)*phi(3,1,2) + Nspr0(3,nages)*phi(3,3,2);
    AMNspr0(3,nages) = Nspr0(3,nages)*phi(3,3,3) + Nspr0(1,nages)*phi(3,1,3) + Nspr0(2,nages)*phi(3,2,3);
    for (i=1;i<=nareas;i++) {
    Nspr0(i,nages) = AMNspr0(i,nages);
    }
   

  //Nspr40
      Nspr40(1,1)   = (mean(pred_rec(1)(1979,endyr-recage-1))/(mean(pred_rec(1)(1979,endyr-recage-1)) +mean(pred_rec(2)(1979,endyr-recage-1)) + mean(pred_rec(3)(1979,endyr-recage-1))))*3; 
      Nspr40(2,1)   = (mean(pred_rec(2)(1979,endyr-recage-1))/(mean(pred_rec(1)(1979,endyr-recage-1)) +mean(pred_rec(2)(1979,endyr-recage-1)) + mean(pred_rec(3)(1979,endyr-recage-1))))*3; 
      Nspr40(3,1)   = (mean(pred_rec(3)(1979,endyr-recage-1))/(mean(pred_rec(1)(1979,endyr-recage-1)) +mean(pred_rec(2)(1979,endyr-recage-1)) + mean(pred_rec(3)(1979,endyr-recage-1))))*3; 

   for (j=2;j<7;j++) {
   for (i=1;i<=nareas;i++){   // for ages 2 to nages-
     Nspr40(i,j)  =  Nspr40(i,j-1)*mfexp(-1.*(natmort(i)+fratio(i)*mF40(i)*fish4_sel_f(i,j-1)+(1-fratio(i))*mF40(i)*fish3_sel_f(j-1)));
     }
      AMNspr40(1,j) = Nspr40(1,j)*phi(1,1,1) + Nspr40(2,j)*phi(1,2,1) + Nspr40(3,j)*phi(1,3,1);
      AMNspr40(2,j) = Nspr40(2,j)*phi(1,2,2) + Nspr40(1,j)*phi(1,1,2) + Nspr40(3,j)*phi(1,3,2);
      AMNspr40(3,j) = Nspr40(3,j)*phi(1,3,3) + Nspr40(1,j)*phi(1,1,3) + Nspr40(2,j)*phi(1,2,3);
      for (i=1;i<=nareas;i++) {
       Nspr40(i,j) = AMNspr40(i,j);
       }
    }
  
   for (j=7;j<15;j++) {
   for (i=1;i<=nareas;i++){   // for ages 2 to nages-
     Nspr40(i,j)  =  Nspr40(i,j-1)*mfexp(-1.*(natmort(i)+fratio(i)*mF40(i)*fish4_sel_f(i,j-1)+(1-fratio(i))*mF40(i)*fish3_sel_f(j-1)));
     }
      AMNspr40(1,j) = Nspr40(1,j)*phi(2,1,1) + Nspr40(2,j)*phi(2,2,1) + Nspr40(3,j)*phi(2,3,1);
      AMNspr40(2,j) = Nspr40(2,j)*phi(2,2,2) + Nspr40(1,j)*phi(2,1,2) + Nspr40(3,j)*phi(2,3,2);
      AMNspr40(3,j) = Nspr40(3,j)*phi(2,3,3) + Nspr40(1,j)*phi(2,1,3) + Nspr40(2,j)*phi(2,2,3);
      for (i=1;i<=nareas;i++) {
       Nspr40(i,j) = AMNspr40(i,j);
       }
    }

   for (j=15;j<nages;j++) {
   for (i=1;i<=nareas;i++){   // for ages 2 to nages-
     Nspr40(i,j)  =  Nspr40(i,j-1)*mfexp(-1.*(natmort(i)+fratio(i)*mF40(i)*fish4_sel_f(i,j-1)+(1-fratio(i))*mF40(i)*fish3_sel_f(j-1)));
     }
      AMNspr40(1,j) = Nspr40(1,j)*phi(3,1,1) + Nspr40(2,j)*phi(3,2,1) + Nspr40(3,j)*phi(3,3,1);
      AMNspr40(2,j) = Nspr40(2,j)*phi(3,2,2) + Nspr40(1,j)*phi(3,1,2) + Nspr40(3,j)*phi(3,3,2);
      AMNspr40(3,j) = Nspr40(3,j)*phi(3,3,3) + Nspr40(1,j)*phi(3,1,3) + Nspr40(2,j)*phi(3,2,3);
      for (i=1;i<=nareas;i++) {
       Nspr40(i,j) = AMNspr40(i,j);
       }
    }

  for (j=1; j<=nareas; j++) {  // for age nages
    Nspr40(j,nages)  =  Nspr40(j,nages-1)*mfexp(-1.* (natmort(j)+fratio(j)*mF40(j)*fish4_sel_f(j,nages-1)+(1-fratio(j))*mF40(j)*fish3_sel_f(nages-1)))/ (1.-mfexp(-1.*(natmort(j)+fratio(j)*mF40(j)*fish4_sel_f(j,nages-1)+(1-fratio(j))*mF40(j)*fish3_sel_f(nages-1))));
    }
    AMNspr40(1,nages) = Nspr40(1,nages)*phi(3,1,1) + Nspr40(2,nages)*phi(3,2,1) + Nspr40(3,nages)*phi(3,3,1);
    AMNspr40(2,nages) = Nspr40(2,nages)*phi(3,2,2) + Nspr40(1,nages)*phi(3,1,2) + Nspr40(3,nages)*phi(3,3,2);
    AMNspr40(3,nages) = Nspr40(3,nages)*phi(3,3,3) + Nspr40(1,nages)*phi(3,1,3) + Nspr40(2,nages)*phi(3,2,3);
    for (i=1;i<=nareas;i++) {
    Nspr40(i,nages) = AMNspr40(i,nages);
    }


  //Nspr35
      Nspr35(1,1) = (mean(pred_rec(1)(1979,endyr-recage-1))/(mean(pred_rec(1)(1979,endyr-recage-1)) +mean(pred_rec(2)(1979,endyr-recage-1)) + mean(pred_rec(3)(1979,endyr-recage-1))))*3; 
      Nspr35(2,1) = (mean(pred_rec(2)(1979,endyr-recage-1))/(mean(pred_rec(1)(1979,endyr-recage-1)) +mean(pred_rec(2)(1979,endyr-recage-1)) + mean(pred_rec(3)(1979,endyr-recage-1))))*3; 
      Nspr35(3,1) = (mean(pred_rec(3)(1979,endyr-recage-1))/(mean(pred_rec(1)(1979,endyr-recage-1)) +mean(pred_rec(2)(1979,endyr-recage-1)) + mean(pred_rec(3)(1979,endyr-recage-1))))*3; 

   for (j=2;j<7;j++) {
   for (i=1;i<=nareas;i++){   // for ages 2 to nages-
     Nspr35(i,j)=Nspr35(i,j-1)*mfexp(-1.*(natmort(i)+fratio(i)*mF35(i)*fish4_sel_f(i,j-1)+(1-fratio(i))*mF35(i)*fish3_sel_f(j-1)));
     }
      AMNspr35(1,j) = Nspr35(1,j)*phi(1,1,1) + Nspr35(2,j)*phi(1,2,1) + Nspr35(3,j)*phi(1,3,1);
      AMNspr35(2,j) = Nspr35(2,j)*phi(1,2,2) + Nspr35(1,j)*phi(1,1,2) + Nspr35(3,j)*phi(1,3,2);
      AMNspr35(3,j) = Nspr35(3,j)*phi(1,3,3) + Nspr35(1,j)*phi(1,1,3) + Nspr35(2,j)*phi(1,2,3);
      for (i=1;i<=nareas;i++) {
       Nspr35(i,j) = AMNspr35(i,j);
       }
     }

   for (j=7;j<15;j++) {
   for (i=1;i<=nareas;i++){   // for ages 2 to nages-
     Nspr35(i,j)=Nspr35(i,j-1)*mfexp(-1.*(natmort(i)+fratio(i)*mF35(i)*fish4_sel_f(i,j-1)+(1-fratio(i))*mF35(i)*fish3_sel_f(j-1)));
     }
      AMNspr35(1,j) = Nspr35(1,j)*phi(2,1,1) + Nspr35(2,j)*phi(2,2,1) + Nspr35(3,j)*phi(2,3,1);
      AMNspr35(2,j) = Nspr35(2,j)*phi(2,2,2) + Nspr35(1,j)*phi(2,1,2) + Nspr35(3,j)*phi(2,3,2);
      AMNspr35(3,j) = Nspr35(3,j)*phi(2,3,3) + Nspr35(1,j)*phi(2,1,3) + Nspr35(2,j)*phi(2,2,3);
      for (i=1;i<=nareas;i++) {
       Nspr35(i,j) = AMNspr35(i,j);
       }
     }
 
    for (j=15;j<nages;j++) {
   for (i=1;i<=nareas;i++){   // for ages 2 to nages-
     Nspr35(i,j)=Nspr35(i,j-1)*mfexp(-1.*(natmort(i)+fratio(i)*mF35(i)*fish4_sel_f(i,j-1)+(1-fratio(i))*mF35(i)*fish3_sel_f(j-1)));
     }
      AMNspr35(1,j) = Nspr35(1,j)*phi(3,1,1) + Nspr35(2,j)*phi(3,2,1) + Nspr35(3,j)*phi(3,3,1);
      AMNspr35(2,j) = Nspr35(2,j)*phi(3,2,2) + Nspr35(1,j)*phi(3,1,2) + Nspr35(3,j)*phi(3,3,2);
      AMNspr35(3,j) = Nspr35(3,j)*phi(3,3,3) + Nspr35(1,j)*phi(3,1,3) + Nspr35(2,j)*phi(3,2,3);
      for (i=1;i<=nareas;i++) {
       Nspr35(i,j) = AMNspr35(i,j);
       }
     }
    
  for (j=1; j<=nareas; j++) {  // for age nages
    Nspr35(j,nages)=Nspr35(j,nages-1)*mfexp(-1.* (natmort(j)+fratio(j)*mF35(j)*fish4_sel_f(j,nages-1)+(1-fratio(j))*mF35(j)*fish3_sel_f(nages-1)))/ (1.-mfexp(-1.*(natmort(j)+fratio(j)*mF35(j)*fish4_sel_f(j,nages-1)+(1-fratio(j))*mF35(j)*fish3_sel_f(nages-1))));
    }
    AMNspr35(1,nages) = Nspr35(1,nages)*phi(3,1,1) + Nspr35(2,nages)*phi(3,2,1) + Nspr35(3,nages)*phi(3,3,1);
    AMNspr35(2,nages) = Nspr35(2,nages)*phi(3,2,2) + Nspr35(1,nages)*phi(3,1,2) + Nspr35(3,nages)*phi(3,3,2);
    AMNspr35(3,nages) = Nspr35(3,nages)*phi(3,3,3) + Nspr35(1,nages)*phi(3,1,3) + Nspr35(2,nages)*phi(3,2,3);
      for (i=1;i<=nareas;i++) {
    Nspr35(i,nages) = AMNspr35(i,nages);
    }


  for (i=1;i<=nareas;i++) {
   for (j=1;j<=nages;j++)   {
   // Kill them off till (spawn_fract)
    SB0(i)    += Nspr0(i,j)*wt_mature(j)*mfexp(-spawn_fract*natmort(i));
    SBF40(i)  += Nspr40(i,j)*wt_mature(j)*mfexp(-spawn_fract*(natmort(i)+fratio(i)*mF40(i)*fish4_sel_f(i,j)+(1-fratio(i))*mF40(i)*fish3_sel_f(j)));
    SBF35(i)  += Nspr35(i,j)*wt_mature(j)*mfexp(-spawn_fract*(natmort(i)+fratio(i)*mF35(i)*fish4_sel_f(i,j)+(1-fratio(i))*mF35(i)*fish3_sel_f(j)));
   }}


  for(j=1;j<=nareas;j++){
   sprpen(j)   = 100.*square(SBF40(j)/SB0(j)-0.4);
   sprpen(j)   += 100.*square(SBF35(j)/SB0(j)-0.35);}

  for (j=1;j<=nareas;j++)  {
    B40(j)= 0.5*SBF40(j)*pred_rec_all;
    B40_alt(j) = 0.5*SBF40(j)*mean(pred_rec(j)(1979,endyr-recage-1));}  // I think this is the most correct method for B40 in each area
    B40_all = B40(1) + B40(2) + B40(3);
    B40_all_alt = B40_alt(1) + B40_alt(2) + B40_alt(3);  
 
 
FUNCTION Calc_priors
// Calculate priors
    priors.initialize(); //priors.initialize sets it all equal to zero
    if (active(sigr))
       priors(1)    = square(log((sigr/sigrprior)))/(2.*square(cvsigrprior));
    if (active(log_q_srv1))
      priors(2)    = square(log_q_srv1-log(q_srv1prior))/(2.*square(cvq_srv1prior));
    //if (active(steepness))
      //priors(3)    = square(log(steepness/steep_prior))/(2.*cv_steep_prior); 
    if (active(logm1))
      priors(4)    = square(logm1-log(mprior(1)))/(2.*square(cvmprior(1)));
    if (active(logm2))
      priors(5)    = square(logm2-log(mprior(2)))/(2.*square(cvmprior(2)));
    if (active(logm3))  
      priors(6)    = square(logm3-log(mprior(3)))/(2.*square(cvmprior(3)));
    if (active(log_q_srv2))  
      priors(7)    = square(log_q_srv2-log(q_srv2prior))/(2.*square(cvq_srv2prior));
    if (active(log_q_srv5_1))
      priors(9)    = square(log_q_srv5_1-log(q_srv5prior1))/(2.*square(cvq_srv5prior1));
    if (active(log_q_srv6))
      priors(10)    = square(log_q_srv6-log(q_srv6prior))/(2.*square(cvq_srv6prior));
    if (active(log_q_srv8_1))
      priors(11)    = square(log_q_srv8_1-log(q_srv8prior1))/(2.*square(cvq_srv8prior1));
    if (active(log_q_srv8_2))
      priors(12)    = square(log_q_srv8_2-log(q_srv8prior2))/(2.*square(cvq_srv8prior2));  
    if (active(log_q_srv8_3))
      priors(13)    = square(log_q_srv8_3-log(q_srv8prior3))/(2.*square(cvq_srv8prior3)); 
    if (active(log_q_srv5_2))
      priors(14)    = square(log_q_srv5_2-log(q_srv5prior2))/(2.*square(cvq_srv5prior2));
    if (active(log_q_srv5_3))
      priors(15)    = square(log_q_srv5_3-log(q_srv5prior3))/(2.*square(cvq_srv5prior3));


FUNCTION Surv_Likelihood
 // Calculate likelihood for survey biomass
  surv_like.initialize();
   for (j=1; j<=nareas; j++) {
    for (i=1; i<=nyrs_srv3; i++)  {  
     surv_like(j,1) += square((log(obs_srv3_biom(j,i)+0.0001)-log(pred_srv3(j,i)+0.0001) ))/ (2.*square(obs_srv3_se(j,i)/obs_srv3_biom(j,i))); }
    for (i=1; i<=nyrs_srv4; i++)  {   
     surv_like(j,2) += square((log(obs_srv4_biom(j,i)+0.0001)-log(pred_srv4(j,i)+0.0001) ))/ (2.*square(obs_srv4_se(j,i)/obs_srv4_biom(j,i))); }
    for (i=1; i<=nyrs_srv5; i++)  { 
     surv_like(j,3) += square((log(obs_srv5_biom(j,i)+0.0001)-log(pred_srv5(j,i)+0.0001) ))/ (2.*square(obs_srv5_se(j,i)/obs_srv5_biom(j,i))); }
    for (i=1; i<=nyrs_srv6; i++)  {  
     surv_like(j,4) += square((log(obs_srv6_biom(j,i)+0.0001)-log(pred_srv6(j,i)+0.0001) ))/ (2.*square(obs_srv6_se(j,i)/obs_srv6_biom(j,i))); }
    }
 
   // likelihood for survey biomass 
   for(j=1;j<=nareas;j++){
    surv_like(j,1) *= wt_srv3 ;  
    surv_like(j,2) *= wt_srv4 ;  
    surv_like(j,3) *= wt_srv5 ;  
    surv_like(j,4) *= wt_srv6 ;  
   }
   
FUNCTION Multinomial_Likelihood
// Calculate multinomial likelihoods for survey age, fishery size, and survey size and subtract "offset"
  age_like.initialize();
     for(j=1;j<=nareas;j++) {         
  for (i=1; i <= nyrs_fish1_age; i++) {
    age_like(j,1) -= nsamples_fish1_age(j,i)*((oac_fish1(j,i) + 0.001) * log(eac_fish1(j,i) + 0.001)) ; }
  
   for (i=1; i <= nyrs_srv1_age; i++) {
    age_like(j,2) -= nsamples_srv1_age(j,i)*((oac_srv1(j,i) + 0.001) * log(eac_srv1(j,i) + 0.001)) ; }

  for (i=1; i <= nyrs_fish1_size; i++) age_like(j,4) -= nsamples_fish1_fsize(j,i)*((osc_fish1_f(j,i) + 0.001) * log(esc_fish1_f(j,i) + 0.001)) ;
  for (i=1; i <= nyrs_fish1_size; i++) age_like(j,5) -= nsamples_fish1_msize(j,i)*((osc_fish1_m(j,i) + 0.001) * log(esc_fish1_m(j,i) + 0.001)) ;
  for (i=1; i <= nyrs_fish3_size; i++) age_like(j,6) -= nsamples_fish3_size(j,i)*((osc_fish3_f(j,i) + 0.001) * log(esc_fish3_f(j,i) + 0.001)) ;
  for (i=1; i <= nyrs_fish3_size; i++) age_like(j,7) -= nsamples_fish3_size(j,i)*((osc_fish3_m(j,i) + 0.001) * log(esc_fish3_m(j,i) + 0.001)) ;

  for (i=1; i <= nyrs_srv1_size; i++)  age_like(j,9) -= nsamples_srv1_fsize(j,i)*((osc_srv1_f(j,i) + 0.001) * log(esc_srv1_f(j,i) + 0.001)) ;
  for (i=1; i <= nyrs_srv1_size; i++)  age_like(j,10) -= nsamples_srv1_msize(j,i)*((osc_srv1_m(j,i) + 0.001) * log(esc_srv1_m(j,i) + 0.001)) ;
  for (i=1; i <= nyrs_srv2_size; i++)  age_like(j,11) -= nsamples_srv2_fsize(j,i)*((osc_srv2_f(j,i) + 0.001) * log(esc_srv2_f(j,i) + 0.001)) ;
  for (i=1; i <= nyrs_srv2_size; i++)  age_like(j,12) -= nsamples_srv2_msize(j,i)*((osc_srv2_m(j,i) + 0.001) * log(esc_srv2_m(j,i) + 0.001)) ;

    age_like(j,1)   -= offset(j,1);                      // Subract offsets
    age_like(j,2)   -= offset(j,2);                      // Subract offsets
    age_like(j,4)   -= offset(j,4);
    age_like(j,5)   -= offset(j,5);
    age_like(j,6)   -= offset(j,6);
    age_like(j,7)   -= offset(j,7);     
    age_like(j,9)   -= offset(j,9); 
    age_like(j,10)  -= offset(j,10);
    age_like(j,11)   -= offset(j,11);
    age_like(j,12)   -= offset(j,12);

    age_like(j,1) *= wt_fish1_age(j);    //1                // Multiple each likelihood by their weights from .ctl file
    age_like(j,2) *= wt_srv1_age(j);     //1
    age_like(j,4) *= wt_fish1_sizeF(j);   //1
    age_like(j,5) *= wt_fish1_sizeM(j);   //1 
    age_like(j,6) *= wt_fish3_sizeF(j);   //1
    age_like(j,7) *= wt_fish3_sizeM(j);   //1 
    age_like(j,9) *= wt_srv1_sizeF(j);    //1
    age_like(j,10) *= wt_srv1_sizeM(j);   //1  
    age_like(j,11) *= wt_srv2_sizeF(j);   //1
    age_like(j,12) *= wt_srv2_sizeM(j);   //1 
  }
        
  //FUNCTION sel_like
  //Calculate penalty function for selectivity
  //sel_like.initialize();
   // if (active(log_srv1_sel_coffs_f) ) {  // IS THIS NEEDED?

  //sel_like(1)   +=wt_sel_reg_fish3 * norm2(first_difference(first_difference(log_fish3_sel_f)));  // Constrains selectivities to be smooth
  //for (j=1;j<nages;j++) {
  //  if (log_fish3_sel_f(j)>log_fish3_sel_f(j+1))
  //    sel_like(2) += wt_sel_dome_fish3 *square(log_fish3_sel_f(j)-log_fish3_sel_f(j+1));  //Prevents dome-shapedness
  // }
 
FUNCTION double round(double r) 
    return double((r > 0.0) ? floor(r + 0.5) : ceil(r - 0.5)); 

FUNCTION Get_Population_Projection
    // Abundance at start of first projection year
    // stdev of recvar
   int  k;  // seed 

   for (i=1;i<=nareas;i++) {  //should this line be before or after the 'if' statement?
    if(mceval_phase()) {
     // random_number_generator r(1000);
      stdev_rec(1) = sqrt(norm2(value(log_rec_dev(1)(1979,endyr-recage))-mean(value(log_rec_dev(1)(1979,endyr-recage))))/(size_count(value(log_rec_dev(1)(1979,endyr-recage)))-1));
      stdev_rec(2) = sqrt(norm2(value(log_rec_dev(2)(1979,endyr-recage))-mean(value(log_rec_dev(2)(1979,endyr-recage))))/(size_count(value(log_rec_dev(2)(1979,endyr-recage)))-1));
      stdev_rec(3) = sqrt(norm2(value(log_rec_dev(3)(1979,endyr-recage))-mean(value(log_rec_dev(3)(1979,endyr-recage))))/(size_count(value(log_rec_dev(3)(1979,endyr-recage)))-1));

      k = round(value(stdev_rec(i))*10000);  
  
     // age group 1, endyr+1
      N_proj_f(i,endyr+1,1)= mfexp(value(log(mean(value(pred_rec(i)(1979,endyr-recage-1))))-square(stdev_rec(i))/2+stdev_rec(i)*randn(k)))/2;   // generating recruitments for each year
      N_proj_m(i,endyr+1,1)= mfexp(value(log(mean(value(pred_rec(i)(1979,endyr-recage-1))))-square(stdev_rec(i))/2+stdev_rec(i)*randn(k)))/2;   // this was k+l but not sure why since there's no l defined
      } 
     else { N_proj_f(i,endyr+1,1)= mfexp(value(log(mean(pred_rec(i)(1979,endyr-recage-1)))))/2;  // fill in endyr+1 age 1
            N_proj_m(i,endyr+1,1)= mfexp(value(log(mean(pred_rec(i)(1979,endyr-recage-1)))))/2; 
            TN_proj_f(i,endyr+1,1)= mfexp(value(log(mean(pred_rec(i)(1979,endyr-recage-1)))))/2;
            TN_proj_m(i,endyr+1,1)= mfexp(value(log(mean(pred_rec(i)(1979,endyr-recage-1)))))/2; 
     }

    // fill in endyr+1 ages 2-29
    for (j=1; j<nages;j++) { 
      k = k+j;  
      N_proj_f(i,endyr+1,j+1)=natage_f(i,endyr,j)*value(S_f(i,endyr,j));  
      N_proj_m(i,endyr+1,j+1)=natage_m(i,endyr,j)*value(S_m(i,endyr,j)); } 
      N_proj_f(i,endyr+1,nages) = value(natage_f(i,endyr,nages-1))*value(S_f(i,endyr,nages-1))+ value(natage_f(i,endyr,nages))*value(S_f(i,endyr,nages));  // fill in 2015 nages
      N_proj_m(i,endyr+1,nages) = value(natage_m(i,endyr,nages-1))*value(S_m(i,endyr,nages-1))+ value(natage_m(i,endyr,nages))*value(S_m(i,endyr,nages));
         
  // move year 1
    for (j=1;j<=6;j++){
    for (l=1; l<=nareas;l++) {   
        AM_N_proj_f(endyr+1,l,j)=N_proj_f(l,endyr+1,j);
        AM_N_proj_m(endyr+1,l,j)=N_proj_m(l,endyr+1,j); }
        AM_N_proj_f(endyr+1)=trans(phi(1))*AM_N_proj_f(endyr+1) ;
        AM_N_proj_m(endyr+1)=trans(phi(1))*AM_N_proj_m(endyr+1) ; 
    for (l=1; l<=nareas;l++) {       
        N_proj_f(l,endyr+1,j)=AM_N_proj_f(endyr+1,l,j);
        N_proj_m(l,endyr+1,j)=AM_N_proj_m(endyr+1,l,j);  }}
    for (j=7;j<=14;j++){
    for (l=1; l<=nareas;l++) {   
        AM_N_proj_f(endyr+1,l,j)=N_proj_f(l,endyr+1,j);
        AM_N_proj_m(endyr+1,l,j)=N_proj_m(l,endyr+1,j); }
        AM_N_proj_f(endyr+1)=trans(phi(2))*AM_N_proj_f(endyr+1) ;
        AM_N_proj_m(endyr+1)=trans(phi(2))*AM_N_proj_m(endyr+1) ; 
    for (l=1; l<=nareas;l++) {       
        N_proj_f(l,endyr+1,j)=AM_N_proj_f(endyr+1,l,j);
        N_proj_m(l,endyr+1,j)=AM_N_proj_m(endyr+1,l,j);  }}
    for (j=15;j<=nages;j++){
    for (l=1; l<=nareas;l++) {   
        AM_N_proj_f(endyr+1,l,j)=N_proj_f(l,endyr+1,j);
        AM_N_proj_m(endyr+1,l,j)=N_proj_m(l,endyr+1,j); }
        AM_N_proj_f(endyr+1)=trans(phi(3))*AM_N_proj_f(endyr+1) ;
        AM_N_proj_m(endyr+1)=trans(phi(3))*AM_N_proj_m(endyr+1) ; 
    for (l=1; l<=nareas;l++) {       
        N_proj_f(l,endyr+1,j)=AM_N_proj_f(endyr+1,l,j);
        N_proj_m(l,endyr+1,j)=AM_N_proj_m(endyr+1,l,j);  }}

    for (l=1;l<=nareas;l++) {
       spawn_biom_proj(l,endyr+1) =elem_prod(N_proj_f(l,endyr+1),pow(mfexp(-yieldratio(l)*FABC_tot_proj_f(l)-value(natmort(l))),spawn_fract)) * wt_mature; 
        tot_biom_proj(l,endyr+1)=N_proj_f(l,endyr+1)*wt_f+N_proj_m(l,endyr+1)*wt_m;
       } 
       }

   // Now code in NPFMC control rule and start projections for 2015+
   for (i=endyr+1;i<=endyr+projyrs;i++)  {
    for (l=1;l<=nareas;l++) { 
       FABC_proj(l) = value(F40(l));
       FOFL_proj(l)=value(F35(l)); 
     
      for (j=1;j<=nages;j++)  {  
       FABC_tot_proj_f(l,j) = value(fish4_sel_f(l,j))* FABC_proj(l)* fratio(l) + value(fish3_sel_f(j))* FABC_proj(l) *(1-fratio(l));  
       FABC_tot_proj_m(l,j) = value(fish4_sel_m(l,j))* FABC_proj(l) * fratio(l) + value(fish3_sel_m(j))* FABC_proj(l)* (1-fratio(l)); 
       Z_proj_f(l,j)   = FABC_tot_proj_f(l,j)+ value(natmort(l));
       Z_proj_m(l,j)   = FABC_tot_proj_m(l,j)+ value(natmort(l));
       ZOFL_proj_f(l,j)   = FOFL_tot_proj_f(l,j)+ value(natmort(l));
       ZOFL_proj_m(l,j)   = FOFL_tot_proj_m(l,j)+ value(natmort(l));
       S_proj_f(l,j)   = mfexp(-1.0* Z_proj_f(l,j));
       S_proj_m(l,j)   = mfexp(-1.0* Z_proj_m(l,j));  
    //catch
       catage_proj_f(l,i,j) = yieldratio(l)*N_proj_f(l,i,j)* FABC_tot_proj_f(l,j)/Z_proj_f(l,j)*(1.-mfexp(-Z_proj_f(l,j)));
       catage_proj_m(l,i,j) = yieldratio(l)*N_proj_m(l,i,j)* FABC_tot_proj_m(l,j)/Z_proj_m(l,j)*(1.-mfexp(-Z_proj_m(l,j)));
       catage_proj_OFL_f(l,i,j) = yieldratio(l)*N_proj_f(l,i,j)* FOFL_tot_proj_f(l,j)/ZOFL_proj_f(l,j)*(1.-mfexp(-ZOFL_proj_f(l,j)));
       catage_proj_OFL_m(l,i,j) = yieldratio(l)*N_proj_m(l,i,j)* FOFL_tot_proj_m(l,j)/ZOFL_proj_m(l,j)*(1.-mfexp(-ZOFL_proj_m(l,j)));  
       }
       pred_catch_proj(l,i) = (catage_proj_f(l,i)*wt_f+catage_proj_m(l,i)*wt_m)/yieldratio(l);
       pred_catch_proj_OFL(l,i) = (catage_proj_OFL_f(l,i)*wt_f+catage_proj_OFL_m(l,i)*wt_m)/yieldratio(l);
       
     //  Next year's abundance
       if (i < endyr+projyrs)  {  
       if(mceval_phase()) {
        stdev_rec(l) = sqrt(norm2(value(log_rec_dev(l)(1979,endyr-recage-1))-mean(value(log_rec_dev(l)(1979,endyr-recage-1))))/(size_count(value(log_rec_dev(l)(1979,endyr-recage)))-1));
        k=round(value(stdev_rec(l))*10000)+i;  // seed for random number generator 

        k=k+i;  
        //i is endyr+1, so i+1 means we're filling in 2016 (if endyr is 2014)
        N_proj_f(l,i+1,1)= mfexp(value(log(mean(value(pred_rec(l)(1979,endyr-recage-1))))-square(stdev_rec(l))/2+stdev_rec(l)*randn(k+l)))/2;
        N_proj_m(l,i+1,1)= mfexp(value(log(mean(value(pred_rec(l)(1979,endyr-recage-1))))-square(stdev_rec(l))/2+stdev_rec(l)*randn(k+l)))/2; 
        }
       else { N_proj_f(l,i+1,1)= mfexp(value(log(mean(pred_rec(l)(1979,endyr-recage-1)))))/2;  // fill in 2016+ age 1
              N_proj_m(l,i+1,1)= mfexp(value(log(mean(pred_rec(l)(1979,endyr-recage-1)))))/2; 
              }

       for (j=1; j<nages-1;j++) {  //now filling in 2016+ ages 2+
         N_proj_f(l,i+1,j+1) = N_proj_f(l,i,j)  * mfexp(-yieldratio(l)*FABC_tot_proj_f(l,j)-value(natmort(l)));
         N_proj_m(l,i+1,j+1) = N_proj_m(l,i,j)  * mfexp(-yieldratio(l)*FABC_tot_proj_m(l,j)-value(natmort(l))); 
         }
         N_proj_f(l,i+1,nages) = N_proj_f(l,i,nages-1)* mfexp(-yieldratio(l)*FABC_tot_proj_f(l,nages-1)-value(natmort(l)))+ N_proj_f(l,i,nages) * mfexp(-yieldratio(l)*FABC_tot_proj_f(l,nages)-value(natmort(l)));
         N_proj_m(l,i+1,nages) = N_proj_m(l,i,nages-1)* mfexp(-yieldratio(l)*FABC_tot_proj_m(l,nages-1)-value(natmort(l)))+ N_proj_m(l,i,nages) * mfexp(-yieldratio(l)*FABC_tot_proj_m(l,nages)-value(natmort(l)));
        }  //closes if        
        }  //closes areas
 
       if (i < endyr+projyrs)  {     
        for (j=1;j<=6;j++){           
        for (l=1; l<=nareas;l++) {   
         AM_N_proj_f(i+1,l,j)=N_proj_f(l,i+1,j);
         AM_N_proj_m(i+1,l,j)=N_proj_m(l,i+1,j); }
         AM_N_proj_f(i+1)=trans(phi(1))*AM_N_proj_f(i+1) ;
         AM_N_proj_m(i+1)=trans(phi(1))*AM_N_proj_m(i+1) ; 
        for (l=1; l<=nareas;l++) {       
         N_proj_f(l,i+1,j)=AM_N_proj_f(i+1,l,j);
         N_proj_m(l,i+1,j)=AM_N_proj_m(i+1,l,j);  }}
        for (j=7;j<=15;j++){           
        for (l=1; l<=nareas;l++) {   
         AM_N_proj_f(i+1,l,j)=N_proj_f(l,i+1,j);
         AM_N_proj_m(i+1,l,j)=N_proj_m(l,i+1,j); }
         AM_N_proj_f(i+1)=trans(phi(2))*AM_N_proj_f(i+1) ;
         AM_N_proj_m(i+1)=trans(phi(2))*AM_N_proj_m(i+1) ; 
        for (l=1; l<=nareas;l++) {       
         N_proj_f(l,i+1,j)=AM_N_proj_f(i+1,l,j);
         N_proj_m(l,i+1,j)=AM_N_proj_m(i+1,l,j);  }}
        for (j=15;j<=nages;j++){           
        for (l=1; l<=nareas;l++) {   
         AM_N_proj_f(i+1,l,j)=N_proj_f(l,i+1,j);
         AM_N_proj_m(i+1,l,j)=N_proj_m(l,i+1,j); }
         AM_N_proj_f(i+1)=trans(phi(3))*AM_N_proj_f(i+1) ;
         AM_N_proj_m(i+1)=trans(phi(3))*AM_N_proj_m(i+1) ; 
        for (l=1; l<=nareas;l++) {       
         N_proj_f(l,i+1,j)=AM_N_proj_f(i+1,l,j);
         N_proj_m(l,i+1,j)=AM_N_proj_m(i+1,l,j);  }} 

       for (l=1; l<=nareas;l++) {
         spawn_biom_proj(l,i+1) = elem_prod(N_proj_f(l,i+1),pow(mfexp(-yieldratio(l)*FABC_tot_proj_f(l)-value(natmort(l))),spawn_fract)) * wt_mature;   
         tot_biom_proj(l,i+1)=N_proj_f(l,i+1)*wt_f+N_proj_m(l,i+1)*wt_m;}

        }  //closes if
        }  //closes year

    for (i=1;i<=nareas;i++) {
     //if (spawn_biom_proj(i,endyr+1)/value(B40(i)) > 1.) {
      FABC(i) = value(F40(i));
      FOFL(i) = value(F35(i)); 
      FABC2(i) = value(F40(i));
      FOFL2(i) = value(F35(i)); //}
              //else {
              //FABC(i) = value(F40(i)) * (spawn_biom_proj(i,endyr+1)/value(B40(i)) - 0.05)/(1 - 0.05); 
              //FOFL(i) = value(F35(i))*(spawn_biom_proj(i,endyr+1)/value(B40(i)) - 0.05)/(1 - 0.05);  
              //FABC2(i) = value(F40(i)) * (spawn_biom_proj(i,endyr+2)/value(B40(i)) - 0.05)/(1 - 0.05); 
              //FOFL2(i) = value(F35(i))*(spawn_biom_proj(i,endyr+2)/value(B40(i)) - 0.05)/(1 - 0.05);  
              //}
      OFL(i)=pred_catch_proj_OFL(i,endyr+1);
      ABC(i)=pred_catch_proj(i,endyr+1); }
  



FUNCTION Evaluate_Objective_Function 
  obj_fun.initialize();
  ssqcatch.initialize();
  rec_like.initialize();
  F_mort_regularity.initialize();
  avg_sel_penalty.initialize();
  Surv_Likelihood();                              		// Likelihood function for survey biomass
      for(j=1;j<=nareas;j++) {         
   ssqcatch(j)  +=  wt_ssqcatch_fish1 *norm2(log(obs_catch_fish1(j)+0.001)-log(pred_catch_fish1(j)+0.001));   
   ssqcatch(j)  +=  wt_ssqcatch_fish3 *norm2(log(obs_catch_fish3(j)+0.001)-log(pred_catch_fish3(j)+0.001)); }       
    /*  
   switch (SrType)
   {
    case 3:
    {
      if (rec_like_type==2)  // is this for ricker, bh, or average?  
       for(j=1;j<=nareas;j++){
       rec_like(j)      = wt_rec_var*(norm2(log_rec_dev(j)+sigr*sigr/2.)/(2.*square(sigr)) + (size_count(log_rec_dev(j)))*log(sigr));
      else
        rec_like(j) = wt_rec_var*(norm2(log_rec_dev(j)));}
      break;
    }
    default:
    { // how to make all this spatial? what is all this?  ALL RELATED TO s-r OPTIONS
      dvar_matrix stmp(1,nareas,styr_rec,endyr);
        for (i=styr_rec;i<=endyr;i++) // how is there no {} here?
        for (j=1;j<=nareas;j++)
        stmp(j,i) = Sp_Biom(j,i-recage);
        srm_rec(j)   = SRecruit(stmp(j));
        dvar_matrix   chi(1,nareas,styr_rec_est,endyr_rec_est);
        chi         = log(elem_div(sam_rec(j)(styr_rec_est,endyr_rec_est) , srm_rec(j)(styr_rec_est,endyr_rec_est)));
        dvariable SSQRec = norm2( chi + sigrsq/2.) ;
        rec_like(j)    = .5*SSQRec/sigrsq + nrecs_est*log(sigr); 
        rec_like(j)   += .5*norm2( log_rec_dev(j)(styr_rec,styr_rec_est) )/sigrsq + (styr_rec_est-styr_rec)*log(sigr) ; 
      if (endyr>endyr_rec_est)
        rec_like(j) += .5*norm2( log_rec_dev(j)(endyr_rec_est,endyr  ) )/sigrsq + (endyr-endyr_rec_est)  *log(sigr) ; 
      break;
    }
  }   
  */
   for (j=1;j<=nareas;j++) {
      rec_like(j)      = wt_rec_var*(norm2(log_rec_dev(j)+sigr*sigr/2.)/(2.*square(sigr)) + (size_count(log_rec_dev(j)))*log(sigr)); 
      F_mort_regularity(j)  = wt_fmort_reg * norm2(log_F_devs_fish1(j));  // Penalty function for fishing mortality deviations
      F_mort_regularity(j)  += wt_fmort_reg * norm2(log_F_devs_fish3(j)); // Penalty function for fishing mortality deviations        
    }
    Multinomial_Likelihood();                     // Multinomial likelihood  WHAT DOES THIS DO?

// Sum objective function
  obj_fun           += sum(ssqcatch);             
  obj_fun           += sum(surv_like);
  obj_fun           += sum(age_like);
  Like               = obj_fun;  									// Put here to capture the data likelihood
  obj_fun           += sum(rec_like);
  if(active(log_F_devs_fish1))                  // SHOULD THIS BE TURNED ON? Penalty function for fishing mortality deviations
  obj_fun           += sum(F_mort_regularity);
  obj_fun           += sum(priors);        				// Add priors
  if (active(mF40)&&last_phase()) 
    obj_fun         += sum(sprpen);             	// To solve for the F40 etc.     
  if (current_phase()<3) {
	   for (j=1;j<=nareas;j++) 
	   { obj_fun += 10*(norm2(log_F_devs_fish1(j))+norm2(log_F_devs_fish3(j))); 	}}			// (was-0.3) Penalty early on to scale population...                


GLOBALS_SECTION
 # include "admodel.h"          						// Include AD class definitions
  adstring model_name;
  adstring data_file;


REPORT_SECTION
 // Beginning of all outputting
 // Goes to routine that automatically creates input file for projection model
    if (last_phase()) {
 
  //example format: report<<"Biomass"<<endl<<bt<<endl;
  // output for R
  report << "fratio" << endl;
  report << fratio << endl;

  report << "FABC_tot_proj_f" << endl;
  report << FABC_tot_proj_f << endl;

  report << "N_proj_f" << endl;
  report << N_proj_f << endl;
  report << "AM_N_proj_f" << endl;
  report << AM_N_proj_f << endl;

  report << "# three area model output" << endl;
  report << "model start year " << styr << endl;
  report << model_name<<endl;
  report << data_file<<endl;
  report << "Num_parameters_Estimated "<<initial_params::nvarcalc()<<endl;
  report << "years"<< endl;
  report << yy << endl;

  report << "pred_catch_fish1_1"<< endl << pred_catch_fish1(1)<<endl;
  report << "obs_catch_fish1_1"<< endl << obs_catch_fish1(1)<<endl;
  report << "pred_catch_fish1_2"<< endl << pred_catch_fish1(2)<<endl;
  report << "obs_catch_fish1_2"<< endl << obs_catch_fish1(2)<<endl;
  report << "pred_catch_fish1_3"<< endl << pred_catch_fish1(3)<<endl;
  report << "obs_catch_fish1_3"<< endl << obs_catch_fish1(3)<<endl;

  report << "pred_catch_fish3_1"<< endl << pred_catch_fish3(1)<<endl;
  report << "obs_catch_fish3_1"<< endl << obs_catch_fish3(1)<<endl;
  report << "pred_catch_fish3_2"<< endl << pred_catch_fish3(2)<<endl;
  report << "obs_catch_fish3_2"<< endl << obs_catch_fish3(2)<<endl;
  report << "pred_catch_fish3_3"<< endl << pred_catch_fish3(3)<<endl;
  report << "obs_catch_fish3_3"<< endl << obs_catch_fish3(3)<<endl;

  report << "Survival_Female_1" <<endl;
  for (i=styr;i<=endyr;i++) report << i<<" "<<S_f(1,i) <<endl; report<<endl ;
  report << "Survival_Female_2"<<endl;
  for (i=styr;i<=endyr;i++) report << i<<" "<<S_f(2,i) <<endl; report<<endl ;
  report << "Survival_Female_3"<<endl;
  for (i=styr;i<=endyr;i++) report << i<<" "<<S_f(3,i) <<endl; report<<endl ;

  report << "Survival_Male_1"<<endl;
  for (i=styr;i<=endyr;i++) report << i<<" "<<S_m(1,i) <<endl; report<<endl;
  report << "Survival_Male_2"<<endl;
  for (i=styr;i<=endyr;i++) report <<i<<" "<<S_m(2,i) <<endl; report<<endl;  // Had a random 3i in here
  report << "Survival_Male_2"<<endl;
  for (i=styr;i<=endyr;i++) report << i<<" "<<S_m(3,i) <<endl; report<<endl;

  report << "Numbers_female_1"<<endl;
  for (i=styr;i<=endyr;i++) report << i<<" "<<natage_f(1,i) <<endl; report<<endl;
  report << "Numbers_female_2"<<endl;
  for (i=styr;i<=endyr;i++) report << i<<" "<<natage_f(2,i) <<endl; report<<endl;
  report << "Numbers_female_3"<<endl;
  for (i=styr;i<=endyr;i++) report << i<<" "<<natage_f(3,i) <<endl; report<<endl;

  report << "AM Numbers_female_1"<<endl;
  for (i=styr;i<=endyr;i++) report << i<<" "<<AMnatage_f(i,1) <<endl; report<<endl;
  report << "AM Numbers_female_2"<<endl;
  for (i=styr;i<=endyr;i++) report << i<<" "<<AMnatage_f(i,2) <<endl; report<<endl;
  report << "AM Numbers_female_3"<<endl;
  for (i=styr;i<=endyr;i++) report << i<<" "<<AMnatage_f(i,3) <<endl; report<<endl;

  report << "Numbers_male_1"<<endl;
  for (i=styr;i<=endyr;i++) report << i<<" "<<natage_m(1,i) <<endl; report<<endl;
  report << "Numbers_male_2"<<endl;
  for (i=styr;i<=endyr;i++) report << i<<" "<<natage_m(2,i) <<endl; report<<endl;
  report << "Numbers_male_3"<<endl;
  for (i=styr;i<=endyr;i++) report << i<<" "<<natage_m(3,i) <<endl; report<<endl;

  report << "AM Numbers_male_1"<<endl;
  for (i=styr;i<=endyr;i++) report << i<<" "<<AMnatage_m(i,1) <<endl; report<<endl;
  report << "AM Numbers_male_2"<<endl;
  for (i=styr;i<=endyr;i++) report << i<<" "<<AMnatage_m(i,2) <<endl; report<<endl;
  report << "AM Numbers_male_3"<<endl;
  for (i=styr;i<=endyr;i++) report << i<<" "<<AMnatage_m(i,3) <<endl; report<<endl; 

  report << "Nspr 0" << endl;
  report << Nspr0 <<endl; 
  report << "AM Nspr 0" << endl;
  report << AMNspr0 <<endl;

  report << "Nspr 40" << endl;
  report << Nspr40 <<endl; 
  report << "AMNspr 40" << endl;
  report << AMNspr40 <<endl;

  report << "SBF40" << endl << SBF40 << endl;
  report << "SBF35" << endl << SBF35 << endl;

  report << "ages" << endl << aa <<endl;
  report << "fish1_sel_f"<< endl << fish1_sel_f <<endl;
  report << "fish1_sel_m"<< endl << fish1_sel_m  <<endl;
  report << "fish2_sel"<< endl << fish2_sel <<endl;
  report << "fish3_sel_f"<< endl << fish3_sel_f <<endl;
  report << "fish3_sel_m"<< endl << fish3_sel_m  <<endl;
  report << "fish4_sel_f"<< endl << fish4_sel_f <<endl;
  report << "fish4_sel_m"<< endl << fish4_sel_m  <<endl;

  report << "srv1_sel_f"<< endl << srv1_sel_f  <<endl;
  report << "srv1_sel_m"<< endl << srv1_sel_m  <<endl;
  report << "srv2_sel_f"<< endl << srv2_sel_f  <<endl;
  report << "srv2_sel_m"<< endl << srv2_sel_m  <<endl;
  
  report << "obs_fish1_age_1" << endl << oac_fish1(1) << endl;  
  report << "pred_fish1_age_1"<< endl << eac_fish1(1) <<endl;
  report << "obs_fish1_age_2" << endl << oac_fish1(2) << endl;  
  report << "pred_fish1_age_2"<< endl << eac_fish1(2) <<endl;
  report << "obs_fish1_age_3" << endl << oac_fish1(3) << endl;  
  report << "pred_fish1_age_3"<< endl << eac_fish1(3) <<endl;

  report << "obs_srv1_age_1" << endl << oac_srv1(1)  << endl;
  report << "pred_srv1_age_1"<< endl << eac_srv1(1) << endl; 
  report << "obs_srv1_age_2" << endl << oac_srv1(2)  << endl;
  report << "pred_srv1_age_2"<< endl << eac_srv1(2) << endl; 
  report << "obs_srv1_age_3" << endl << oac_srv1(3)  << endl;
  report << "pred_srv1_age_3"<< endl << eac_srv1(3) << endl; 

  report << "obs_fish1_size_m_1"<< endl <<osc_fish1_m(1) << endl;
  report << "pred_fish1_size_m_1"<<endl <<esc_fish1_m(1) << endl;
  report << "obs_fish1_size_m_2"<< endl <<osc_fish1_m(2) << endl;
  report << "pred_fish1_size_m_2"<<endl <<esc_fish1_m(2) << endl;
  report << "obs_fish1_size_m_3"<< endl <<osc_fish1_m(3) << endl;
  report << "pred_fish1_size_m_3"<<endl <<esc_fish1_m(3) << endl;

  report << "obs_fish1_size_f_1"<< endl <<osc_fish1_f(1) << endl;
  report << "pred_fish1_size_f_1"<<endl <<esc_fish1_f(1) << endl;
  report << "obs_fish1_size_f_2"<< endl <<osc_fish1_f(2) << endl;
  report << "pred_fish1_size_f_2"<<endl <<esc_fish1_f(2) << endl;
  report << "obs_fish1_size_f_3"<< endl <<osc_fish1_f(3) << endl;
  report << "pred_fish1_size_f_3"<<endl <<esc_fish1_f(3) << endl;
 
  report << "obs_fish3_size_m_1"<< endl <<osc_fish3_m(1) << endl;
  report << "pred_fish3_size_m_1"<<endl <<esc_fish3_m(1) << endl;
  report << "obs_fish3_size_m_2"<< endl <<osc_fish3_m(2) << endl;
  report << "pred_fish3_size_m_2"<<endl <<esc_fish3_m(2) << endl;
  report << "obs_fish3_size_m_3"<< endl <<osc_fish3_m(3) << endl;
  report << "pred_fish3_size_m_3"<<endl <<esc_fish3_m(3) << endl;

  report << "obs_fish3_size_f_1"<< endl <<osc_fish3_f(1) << endl;
  report << "pred_fish3_size_f_1"<<endl <<esc_fish3_f(1) << endl;
  report << "obs_fish3_size_f_2"<< endl <<osc_fish3_f(2) << endl;
  report << "pred_fish3_size_f_2"<<endl <<esc_fish3_f(2) << endl;
  report << "obs_fish3_size_f_3"<< endl <<osc_fish3_f(3) << endl;
  report << "pred_fish3_size_f_3"<<endl <<esc_fish3_f(3) << endl;

  report << "obs_srv1_size_m_1"<<endl << osc_srv1_m(1) << endl;
  report << "pred_srv1_size_m_1"<<endl<< esc_srv1_m(1) << endl; 
  report << "obs_srv1_size_m_2"<<endl << osc_srv1_m(2) << endl;
  report << "pred_srv1_size_m_2"<<endl<< esc_srv1_m(2) << endl; 
  report << "obs_srv1_size_m_3"<<endl << osc_srv1_m(3) << endl;
  report << "pred_srv1_size_m_3"<<endl<< esc_srv1_m(3) << endl; 

  report << "obs_srv1_size_f_1"<<endl << osc_srv1_f(1) << endl;
  report << "pred_srv1_size_f_1"<<endl<< esc_srv1_f(1) << endl; 
  report << "obs_srv1_size_f_2"<<endl << osc_srv1_f(2) << endl;
  report << "pred_srv1_size_f_2"<<endl<< esc_srv1_f(2) << endl; 
  report << "obs_srv1_size_f_3"<<endl << osc_srv1_f(3) << endl;
  report << "pred_srv1_size_f_3"<<endl<< esc_srv1_f(3) << endl; 

  report << "obs_srv2_size_m_1"<<endl << osc_srv2_m(1) << endl;
  report << "pred_srv2_size_m_1"<<endl<< esc_srv2_m(1) << endl; 
  report << "obs_srv2_size_m_2"<<endl << osc_srv2_m(2) << endl;
  report << "pred_srv2_size_m_2"<<endl<< esc_srv2_m(2) << endl; 
  report << "obs_srv2_size_m_3"<<endl << osc_srv2_m(3) << endl;
  report << "pred_srv2_size_m_3"<<endl<< esc_srv2_m(3) << endl; 

  report << "obs_srv2_size_f_1"<<endl << osc_srv2_f(1) << endl;
  report << "pred_srv2_size_f_1"<<endl<< esc_srv2_f(1) << endl; 
  report << "obs_srv2_size_f_2"<<endl << osc_srv2_f(2) << endl;
  report << "pred_srv2_size_f_2"<<endl<< esc_srv2_f(2) << endl; 
  report << "obs_srv2_size_f_3"<<endl << osc_srv2_f(3) << endl;
  report << "pred_srv2_size_f_3"<<endl<< esc_srv2_f(3) << endl; 

  report << "yrs_srv3" << endl << yrs_srv3  <<endl;
  report << "pred_srv3_biom"<< endl << pred_srv3 <<endl;
  report << "obs_srv3_biom" <<endl << obs_srv3_biom  <<endl;

  report << "yrs_srv4" << endl << yrs_srv4  <<endl;
  report << "pred_srv4_biom"<< endl << pred_srv4  <<endl;
  report << "obs_srv4_biom" << endl << obs_srv4_biom  <<endl;

  report << "yrs_srv5" << endl << yrs_srv5  <<endl;
  report << "pred_srv5_biom"<< endl << pred_srv5  <<endl;
  report << "obs_srv5_biom" << endl << obs_srv5_biom  <<endl;

  report << "yrs_srv6" << endl << yrs_srv6  <<endl;
  report << "pred_srv6_biom"<< endl << pred_srv6  <<endl;
  report << "obs_srv6_biom" << endl << obs_srv6_biom  <<endl;

  report << "Fully_selected_F "<< endl;
  for (j=1;j<=nareas;j++) report << " "<< Fmort_fish1(j)*max(fish1_sel_f)+Fmort_fish3(j) <<endl;

  report << "spawn_biom_1"<< endl << spawn_biom(1) <<endl;
  report << "spawn_biom_2"<< endl << spawn_biom(2) <<endl;
  report << "spawn_biom_3"<< endl << spawn_biom(3) <<endl;
  
  report << "tot_biom_1"<< endl;
  report << tot_biom(1)   <<endl;
  report << "tot_biom_2"<< endl;
  report << tot_biom(2)   <<endl;
  report << "tot_biom_3"<< endl;
  report << tot_biom(3)   <<endl;    

  report << "F35_1" << endl << F35(1) << endl;
  report << "F35_2" << endl << F35(2) << endl;
  report << "F35_3" << endl << F35(3) << endl;
  report << "F40_1" << endl << F40(1) << endl;
  report << "F40_2" << endl << F40(2) << endl;
  report << "F40_3" << endl << F40(3) << endl;
  report << "B40_1" << endl << B40(1) << endl;
  report << "B40_2" << endl << B40(2) << endl;
  report << "B40_3" << endl << B40(3) << endl;
  report << "B40_all" << endl << B40_all << endl;

  report << "B40_alt_1" << endl << B40_alt(1) << endl;
  report << "B40_alt_2" << endl << B40_alt(2) << endl;
  report << "B40_alt_3" << endl << B40_alt(3) << endl;
  report << "B40_alt_all" << endl << B40_all_alt << endl;

  report << "SSB_projection"<< endl << spawn_biom_proj << endl;
  report << "Catch_projection"<< endl << pred_catch_proj<< endl;

  report<<"#Ratio of current SSB to B40 is"<<endl;
  report<<(spawn_biom(1,endyr)+spawn_biom(2,endyr)+spawn_biom(3,endyr))/B40_all<<endl;
  
  report << "Wts_n_Likelihoods" << endl;
  report << "SSQ_catch likelihood" << endl << ssqcatch << endl;
  report << "Surv_like" << endl;
  report << "order: srv3,srv4,srv5,srv6," << surv_like << endl;
  report << "Age_like" << endl;
  report << "order: fish1a, srv1a, srv2a, fish1s f/m, fish3s f/m, 0, srv1s f/m, srv2s f/m" << endl;
  report <<  age_like << endl;

  report << wt_rec_var <<" " << rec_like <<" " ; report << "Recruitment_Deviations_Likelihood" << endl;
  report << wt_fmort_reg <<" "<<F_mort_regularity<<" " ; report << "Fishing_Mortality_Regularity_Penalty" << endl;
  
  report << " "<<priors(1)  <<" " ; report << "priors sigr"     <<endl;
  report << " "<<priors(2)  <<" " ; report << "priors q_srv1" <<endl;
  report << " "<<priors(4)  <<" " ; report << "priors M area 1"<<endl;
  report << " "<<priors(5)  <<" " ; report << "priors M area 2"<<endl;
  report << " "<<priors(6)  <<" " ; report << "priors M area 3"<<endl;
  report << " "<<priors(9)  <<" " ; report << "priors q_srv5_1"<<endl;
  report << " "<<priors(10)  <<" " ; report << "priors q_srv6"<<endl;
  report << " "<<priors(11)  <<" " ; report << "priors q_srv8_1"<<endl;
  report << " "<<priors(12)  <<" " ; report << "priors q_srv8_2"<<endl;
  report << " "<<priors(13)  <<" " ; report << "priors q_srv8_3"<<endl;   
  report << " "<<priors(14)  <<" " ; report << "priors q_srv5_2"<<endl;
  report << " "<<priors(15)  <<" " ; report << "priors q_srv5_3"<<endl;     
  report<< sprpen << " "<<"Sprpen" <<endl;
  report << " "<<obj_fun    <<" " ; report << "obj_fun"         <<endl;
  report << " "<<Like       <<" " ; report << "data likelihood" <<endl;//(2*square(sigr))+ size_count(log_rec_dev)*log(sigr)<<endl;
 
  //if(last_phase()) { 
  //report <<" SDNR1 "<< wt_srv1*std_dev(elem_div((pred_srv1(yrs_srv1)-obs_srv1_biom),obs_srv1_se))<<endl;
  //report <<" SDNR2 "<< wt_srv2*std_dev(elem_div((pred_srv2(yrs_srv2)-obs_srv2_biom),obs_srv2_se))<<endl;
  //report <<" SDNR3 "<< wt_srv3*std_dev(elem_div((pred_srv3(yrs_srv3)-obs_srv3_biom),obs_srv3_se))<<endl;
  //report <<" SDNR4 "<< wt_srv4*std_dev(elem_div((pred_srv4(yrs_srv4)-obs_srv4_biom),obs_srv4_se))<<endl;
  //report <<" SDNR5 "<< wt_srv5*std_dev(elem_div((pred_srv5(yrs_srv5)-obs_srv5_biom),obs_srv5_se))<<endl;
  //report <<" SDNR6 "<< wt_srv6*std_dev(elem_div((pred_srv6(yrs_srv6)-obs_srv6_biom),obs_srv6_se))<<endl;
  //  report <<" SDNR7 "<< wt_srv7*std_dev(elem_div((pred_srv7(yrs_srv7)-obs_srv7_biom),obs_srv7_se))<<endl;
  //  report <<" SDNR8 "<< wt_srv8*std_dev(elem_div((pred_srv8(yrs_srv8)-obs_srv8_biom),obs_srv8_se))<<endl;
   // }
  report << "SigmaR: "<<sigr<< " Nat_Mort: "<<natmort<< endl;
  report <<"Spawning Per Recruit for each area"<< " "<<SBF40<< endl;
  report << "Virgin SPR (SB0) "<<SB0 <<endl;
  report<<"rec_a1_f"<<endl << rec_a1_f << endl;  // this should be outputting numbers at age 2 for each sex/area
  report<<"rec_a1_m"<<endl << rec_a1_m << endl;
  report<<"rec_a2_f"<<endl << rec_a2_f << endl;
  report<<"rec_a2_m"<<endl << rec_a2_m << endl;
  report<<"rec_a3_f"<<endl << rec_a3_f << endl;
  report<<"rec_a3_m"<<endl << rec_a3_m << endl;
   
  report<<"log_mean_rec"<< endl << log_mean_rec<< endl;
  report<<"log_rec_dev" << endl << log_rec_dev << endl;
  report<<"pred_rec" << endl << pred_rec << endl;
  
  report << "q_srv1 " << q_srv1<<endl;
  report << "q_srv2 " << q_srv2<<endl;
  report << "q_srv5 " << q_srv5<<endl;
  report << "q_srv6 " << q_srv6<<endl;
  report << "q_srv8 " << q_srv8<<endl;

   report<<"Some useful statistics if someone where looking at things for their comps"<<endl;  // will need to changes these when nareas != 2
   report<<"Ratio of current SSB to B40 is"<<endl<<(spawn_biom(1,endyr)+spawn_biom(2,endyr)+spawn_biom(3,endyr))/B40_all<<endl;
   //report<<"Ratio of current SSB to B40 is"<<endl<<(spawn_biom(1)(endyr)+spawn_biom(2)(endyr)+spawn_biom(3)(endyr))/B40_all<<endl;
  
   report<<"Total female spawning biomass is"<<endl<<(spawn_biom(1)+spawn_biom(2)+spawn_biom(3))<<endl;
   //report<<"log_rec_dev" << log_rec_dev << endl;
   report<<"Pred_catch" << pred_catch <<endl;
   //report<<"Pred_rec" << pred_rec << endl;
   report<<"avg F mort"<< log_avg_F_fish1 << endl;
   report<<"Depletion"<< Depletion<< endl;
  
   report << "spawn_bio_projected"<<endl<<spawn_biom_proj<<endl;
   report << "Spec_catch_proj1"<<endl<<(catage_proj_f(1)*wt_f+catage_proj_m(1)*wt_m)<<endl;   
   report << "Spec_catch_proj2"<<endl<<(catage_proj_f(2)*wt_f+catage_proj_m(2)*wt_m)<<endl;
   report << "Spec_catch_proj3"<<endl<<(catage_proj_f(3)*wt_f+catage_proj_m(3)*wt_m)<<endl;
   report << "ABC_proj" << endl<<pred_catch_proj <<endl;
  
   report <<" spawn_bio next year"<<endl<<spawn_biom_proj(1,endyr+1)<<endl;
   report <<" spawn_bio next year"<<endl<<spawn_biom_proj(2,endyr+1)<<endl;
   report <<" spawn_bio next year"<<endl<<spawn_biom_proj(3,endyr+1)<<endl;
   report <<" spawn_bio projected"<<endl<<spawn_biom_proj<<endl;
   report <<" ABC projection: "<<pred_catch_proj<<endl;
   report << "SB0" << endl << SB0 << endl;
  
  report << "SDNR Obs_P_fish1_age" <<endl;
  for (j=1;j<=nareas;j++) {
  for (i=1;i<=nyrs_fish1_age;i++) report << yrs_fish1_age(i)<<" "
      <<" eff_N "<<(1-eac_fish1(j,i))*eac_fish1(j,i)/norm2(oac_fish1(j,i)-eac_fish1(j,i))  <<" N "<<nsamples_fish1_age(j,i)
      <<" SDNR "<< sdnr(eac_fish1(j,i),oac_fish1(j,i),wt_fish1_age(j)*double(nsamples_fish1_age(j,i)))<<endl; report<<endl; }
  
  report << "SDNR Obs_P_fish1_size Female "<<endl;
  for (j=1;j<=nareas;j++) {
  for (i=1;i<=nyrs_fish1_size;i++) report << yrs_fish1_size(i)<<" "
      <<" eff_N "<<(1-esc_fish1_f(j,i))*esc_fish1_f(j,i)/norm2(osc_fish1_f(j,i)-esc_fish1_f(j,i))  <<" N "<<nsamples_fish1_fsize(j,i)
      <<" SDNR "<< sdnr(esc_fish1_f(j,i),osc_fish1_f(j,i),wt_fish1_sizeF(j)*double(nsamples_fish1_fsize(j,i)))<<endl; report<<endl;}

  report << "Obs_P_fish1_size Male"<<endl;
  for (j=1;j<=nareas;j++) {
  for (i=1;i<=nyrs_fish1_size;i++) report << yrs_fish1_size(i)<<" " 
      <<" eff_N "<<(1-esc_fish1_m(j,i))*esc_fish1_m(j,i)/norm2(osc_fish1_m(j,i)-esc_fish1_m(j,i))  <<" N "<<nsamples_fish1_msize(j,i)
      <<" SDNR "<< sdnr(esc_fish1_m(j,i),osc_fish1_m(j,i),wt_fish1_sizeM(j)*double(nsamples_fish1_msize(j,i)))<<endl; report<<endl;}

  report << "Obs_P_fish3_size Males" <<endl;
  for (j=1;j<=nareas;j++) {
  for (i=1;i<=nyrs_fish3_size;i++) report << yrs_fish3_size(i)<<" "
      <<" eff_N "<<(1-esc_fish3_m(j,i))*esc_fish3_m(j,i)/norm2(osc_fish3_m(j,i)-esc_fish3_m(j,i))  <<" N "<<nsamples_fish3_size(j,i)
      <<" SDNR "<< sdnr(esc_fish3_m(j,i),osc_fish3_m(j,i),wt_fish3_sizeM(j)*double(nsamples_fish3_size(j,i)))<<endl; report<<endl;}
  
  report << "Obs_P_fish3_size Female"<<endl;
  for (j=1;j<=nareas;j++) {
  for (i=1;i<=nyrs_fish3_size;i++) report << yrs_fish3_size(i)<<" " 
      <<" eff_N "<<(1-esc_fish3_f(j,i))*esc_fish3_f(j,i)/norm2(osc_fish3_f(j,i)-esc_fish3_f(j,i))  <<" N "<<nsamples_fish3_size(j,i)
      <<" SDNR "<< sdnr(esc_fish3_f(j,i),osc_fish3_f(j,i),wt_fish3_sizeF(j)*double(nsamples_fish3_size(j,i)))<<endl; report<<endl;}

  report << "Obs_P_srv1_age"<<endl;
  for (j=1;j<=nareas;j++) {
  for (i=1;i<=nyrs_srv1_age;i++) report << yrs_srv1_age(i)<<" " 
      <<" eff_N "<<(1-eac_srv1(j,i))*eac_srv1(j,i)/norm2(oac_srv1(j,i)-eac_srv1(j,i)) <<" N "<<nsamples_srv1_age(j,i)
      <<" SDNR "<< sdnr(eac_srv1(j,i),oac_srv1(j,i),wt_srv1_age(j)*double(nsamples_srv1_age(j,i)))<<endl; report<<endl;}

  report << "Obs_P_srv1_size females"<<endl;
  for (j=1;j<=nareas;j++) {
  for (i=1;i<=nyrs_srv1_size;i++) report << yrs_srv1_size(i)<<" " 
      <<" eff_N "<<(1-esc_srv1_f(j,i))*esc_srv1_f(j,i)/norm2(osc_srv1_f(j,i)-esc_srv1_f(j,i)) <<" N "<<nsamples_srv1_fsize(j,i)
      <<" SDNR "<< sdnr(esc_srv1_f(j,i),osc_srv1_f(j,i),wt_srv1_sizeF(j)*double(nsamples_srv1_fsize(j,i)))<<endl; report<<endl;}

  report << "Obs_P_srv1_size Males"<<endl;
  for (j=1;j<=nareas;j++) {
  for (i=1;i<=nyrs_srv1_size;i++) report << yrs_srv1_size(i)<<" " 
      <<" eff_N "<<(1-esc_srv1_m(j,i))*esc_srv1_m(j,i)/norm2(osc_srv1_m(j,i)-esc_srv1_m(j,i)) <<" N "<<nsamples_srv1_msize(j,i)
      <<" SDNR "<< sdnr(esc_srv1_m(j,i),osc_srv1_m(j,i),wt_srv1_sizeM(j)*double(nsamples_srv1_msize(j,i)))<<endl; report<<endl;}

  report << "Obs_P_srv2_size females"<<endl;
  for (j=1;j<=nareas;j++) {
  for (i=1;i<=nyrs_srv2_size;i++) report << yrs_srv2_size(i)<<" " 
      <<" eff_N "<<(1-esc_srv2_f(j,i))*esc_srv2_f(j,i)/norm2(osc_srv2_f(j,i)-esc_srv2_f(j,i)) <<" N "<<nsamples_srv2_fsize(j,i)
      <<" SDNR "<< sdnr(esc_srv2_f(j,i),osc_srv2_f(j,i),wt_srv2_sizeF(j)*double(nsamples_srv2_fsize(j,i)))<<endl; report<<endl;}

  report << "Obs_P_srv2_size Males"<<endl;
  for (j=1;j<=nareas;j++) {
  for (i=1;i<=nyrs_srv2_size;i++) report << yrs_srv2_size(i)<<" "
      <<" eff_N "<<(1-esc_srv2_m(j,i))*esc_srv2_m(j,i)/norm2(osc_srv2_m(j,i)-esc_srv2_m(j,i)) <<" N "<<nsamples_srv2_msize(j,i)
      <<" SDNR "<< sdnr(esc_srv2_m(j,i),osc_srv2_m(j,i),wt_srv2_sizeM(j)*double(nsamples_srv2_msize(j,i)))<<endl; report<<endl;}


  report << "meanSDNR_fish1age" << endl;  
    for (j=1;j<=nareas;j++) {
    for (i=1;i<=nyrs_fish1_age;i++) { report << sdnr(eac_fish1(j,i),oac_fish1(j,i),wt_fish1_age(j)*double(nsamples_fish1_age(j,i)))<<endl; }}

  report << "meanSDNR_Ffish1size" << endl;  
    for (j=1;j<=nareas;j++) {
    for (i=1;i<=nyrs_fish1_size;i++) { report << sdnr(esc_fish1_f(j,i),osc_fish1_f(j,i),wt_fish1_sizeF(j)*double(nsamples_fish1_fsize(j,i))) <<endl; }}

  report << "meanSDNR_Mfish1size" << endl;  
    for (j=1;j<=nareas;j++) {
    for (i=1;i<=nyrs_fish1_size;i++) { report << sdnr(esc_fish1_m(j,i),osc_fish1_m(j,i),wt_fish1_sizeM(j)*double(nsamples_fish1_msize(j,i))) <<endl; }}    

  report << "meanSDNR_Mfish3size" << endl;  
    for (j=1;j<=nareas;j++) {
    for (i=1;i<=nyrs_fish3_size;i++) { report << sdnr(esc_fish3_m(j,i),osc_fish3_m(j,i),wt_fish3_sizeM(j)*double(nsamples_fish3_size(j,i))) <<endl; }}

  report << "meanSDNR_Ffish3size" << endl;  
    for (j=1;j<=nareas;j++) {
    for (i=1;i<=nyrs_fish3_size;i++) { report << sdnr(esc_fish3_f(j,i),osc_fish3_f(j,i),wt_fish3_sizeF(j)*double(nsamples_fish3_size(j,i))) <<endl; }}  

  report << "meanSDNR_srv1age" << endl;  
    for (j=1;j<=nareas;j++) {
    for (i=1;i<=nyrs_srv1_age;i++) { report << sdnr(eac_srv1(j,i),oac_srv1(j,i),wt_srv1_age(j)*double(nsamples_srv1_age(j,i))) <<endl; }}

  report << "meanSDNR_Fsrv1size" << endl;  
    for (j=1;j<=nareas;j++) {
    for (i=1;i<=nyrs_srv1_size;i++) { report << sdnr(esc_srv1_f(j,i),osc_srv1_f(j,i),wt_srv1_sizeF(j)*double(nsamples_srv1_fsize(j,i))) <<endl; }}

  report << "meanSDNR_Msrv1size" << endl;  
    for (j=1;j<=nareas;j++) {
    for (i=1;i<=nyrs_srv1_size;i++) { report << sdnr(esc_srv1_m(j,i),osc_srv1_m(j,i),wt_srv1_sizeM(j)*double(nsamples_srv1_msize(j,i))) <<endl; }}

  report << "meanSDNR_Fsrv2size" << endl;  
    for (j=1;j<=nareas;j++) {
    for (i=1;i<=nyrs_srv2_size;i++) { report << sdnr(esc_srv2_f(j,i),osc_srv2_f(j,i),wt_srv2_sizeF(j)*double(nsamples_srv2_fsize(j,i))) <<endl; }}

  report << "meanSDNR_Msrv2size" << endl;  
    for (j=1;j<=nareas;j++) {
    for (i=1;i<=nyrs_srv2_size;i++) { report << sdnr(esc_srv2_m(j,i),osc_srv2_m(j,i),wt_srv2_sizeM(j)*double(nsamples_srv2_msize(j,i))) <<endl; }}

  report << "srv3_UCI " << endl;
  report << obs_srv3_uci << endl;
  report << "srv3_LCI " << endl;
  report << obs_srv3_lci << endl;
  report << "srv4_UCI " << endl;
  report << obs_srv4_uci << endl;
  report << "srv4_LCI " << endl;
  report << obs_srv4_lci << endl;
  report << "srv5_UCI " << endl;
  report << obs_srv5_uci << endl;
  report << "srv5_LCI " << endl;
  report << obs_srv5_lci << endl;      
  report << "srv6_UCI " << endl;
  report << obs_srv6_uci << endl;
  report << "srv6_LCI " << endl;
  report << obs_srv6_lci << endl;
   }
   

    //# include "sable-r-report.cxx"    
FUNCTION double sdnr(const dvar_vector& pred,const dvector& obs,double m)
  RETURN_ARRAYS_INCREMENT();
  double sdnr;
  dvector pp = value(pred)+0.000001;
  int ntmp = -obs.indexmin()+obs.indexmax();
  sdnr = std_dev(elem_div(obs+0.000001-pp,sqrt(elem_prod(pp,(1.-pp))/m)));
  RETURN_ARRAYS_DECREMENT();
  return sdnr;

RUNTIME_SECTION
  convergence_criteria 1.e-3, 1.e-3, 1.e-3, 1.e-3, 1.e-4
  maximum_function_evaluations 500, 500, 500, 1500, 1000

TOP_OF_MAIN_SECTION
  gradient_structure::set_MAX_NVAR_OFFSET(1000);
  //gradient_structure::set_NUM_DEPENDENT_VARIABLES(1000);
  gradient_structure::set_GRADSTACK_BUFFER_SIZE(100000);
  //gradient_structure::set_CMPDIF_BUFFER_SIZE(10000000);
  //arrmblsize=390000;
  arrmblsize=1500000;

FINAL_SECTION
