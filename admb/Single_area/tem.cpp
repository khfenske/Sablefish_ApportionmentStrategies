 # include "admodel.h"          						// Include AD class definitions
  adstring model_name;
  adstring data_file;
  adstring unchanging_data;
#include <admodel.h>
#include <contrib.h>

  extern "C"  {
    void ad_boundf(int i);
  }
#include <tem.htp>

model_data::model_data(int argc,char * argv[]) : ad_comm(argc,argv)
{
  pad_evalout = new ofstream("evalout.prj");   ;
 ad_comm::change_datafile_name("tem.ctl");    // Read in phases, penalties and priors from "tem.ctl"
 *(ad_comm::global_datafile) >>  model_name; 
 *(ad_comm::global_datafile) >>  data_file;   // identifying the data file name, using the name in row 2 of .ctl file
 *(ad_comm::global_datafile) >>  unchanging_data; // indentifying the name of the data file that holds size comps and other data that's not being updated in simulations
  nareas.allocate("nareas");
  SrType.allocate("SrType");
  styr_rec_est.allocate("styr_rec_est");
  endyr_rec_est.allocate("endyr_rec_est");
 nrecs_est = endyr_rec_est-styr_rec_est+1;
  rec_like_type.allocate("rec_like_type");
  ph_Fdev.allocate("ph_Fdev");
  ph_avg_F.allocate("ph_avg_F");
  ph_recdev.allocate("ph_recdev");
  ph_fish_sel.allocate("ph_fish_sel");
  ph_fish4_sel.allocate("ph_fish4_sel");
  ph_srv1_sel.allocate("ph_srv1_sel");
  mprior.allocate("mprior");
  cvmprior.allocate("cvmprior");
  ph_m.allocate("ph_m");
  sigrprior.allocate("sigrprior");
  cvsigrprior.allocate("cvsigrprior");
  ph_sigr.allocate("ph_sigr");
  q_srv1prior.allocate("q_srv1prior");
  cvq_srv1prior.allocate("cvq_srv1prior");
  ph_q_srv1.allocate("ph_q_srv1");
  q_srv2prior.allocate("q_srv2prior");
  cvq_srv2prior.allocate("cvq_srv2prior");
  ph_q_srv2.allocate("ph_q_srv2");
  q_srv5prior.allocate("q_srv5prior");
  cvq_srv5prior.allocate("cvq_srv5prior");
  ph_q_srv5.allocate("ph_q_srv5");
  q_srv6prior.allocate("q_srv6prior");
  cvq_srv6prior.allocate("cvq_srv6prior");
  ph_q_srv6.allocate("ph_q_srv6");
  q_srv8prior.allocate("q_srv8prior");
  cvq_srv8prior.allocate("cvq_srv8prior");
  ph_q_srv8.allocate("ph_q_srv8");
  wt_ssqcatch_fish1.allocate("wt_ssqcatch_fish1");
  wt_ssqcatch_fish3.allocate("wt_ssqcatch_fish3");
  wt_srv3.allocate("wt_srv3");
  wt_srv4.allocate("wt_srv4");
  wt_srv5.allocate("wt_srv5");
  wt_srv6.allocate("wt_srv6");
  wt_fish1_age.allocate("wt_fish1_age");
  wt_srv1_age.allocate("wt_srv1_age");
  wt_srv2_age.allocate("wt_srv2_age");
  wt_fish1_size.allocate("wt_fish1_size");
  wt_srv1_size.allocate("wt_srv1_size");
  wt_fish2_size.allocate("wt_fish2_size");
  wt_srv2_size.allocate("wt_srv2_size");
  wt_fish3_size.allocate("wt_fish3_size");
  wt_fish4_size.allocate("wt_fish4_size");
  wt_srv5_size.allocate("wt_srv5_size");
  wt_fish6_size.allocate("wt_fish6_size");
  wt_srv6_size.allocate("wt_srv6_size");
  wt_rec_var.allocate("wt_rec_var");
  wt_sel_reg_fish3.allocate("wt_sel_reg_fish3");
  wt_sel_dome_fish3.allocate("wt_sel_dome_fish3");
  wt_fmort_reg.allocate("wt_fmort_reg");
  hist_hal_prop.allocate("hist_hal_prop");
  ph_ifq.allocate("ph_ifq");
  ph_srv2q.allocate("ph_srv2q");
  yieldratio.allocate("yieldratio");
  projyrs.allocate("projyrs");
 cout << yieldratio << endl;
 ad_comm::change_datafile_name(data_file);    // Read data from the data file
  styr.allocate("styr");
  endyr.allocate("endyr");
  recage.allocate("recage");
  nages.allocate("nages");
  nlenbins.allocate("nlenbins");
  len_bin_labels.allocate(1,nlenbins,"len_bin_labels");
  nyrs = endyr - styr + 1;
  styr_rec = (styr - nages) + 1;       // First year of recruitment
 endyr_rec= endyr_rec_est;           
  yy.allocate(styr,endyr);
 yy.fill_seqadd(styr,1);
  aa.allocate(1,nages);
 aa.fill_seqadd(recage,1);
 ph_F40 = 5;
  spawn_fract.allocate("spawn_fract");
 spawn_fract = (spawn_fract - 1) / 12;
  p_mature.allocate(1,nages,"p_mature");
  wt_m.allocate(1,nages,"wt_m");
  wt_f.allocate(1,nages,"wt_f");
  wt_mature.allocate(1,nages);
 wt_mature = elem_prod(wt_f,p_mature);
 cout << wt_f << endl;
  nyrs_obscatch.allocate("nyrs_obscatch");
  obs_catch_fish1.allocate(styr,endyr,"obs_catch_fish1");
  obs_catch_fish3.allocate(styr,endyr,"obs_catch_fish3");
  nyrs_srv3.allocate("nyrs_srv3");
  yrs_srv3.allocate(1,nyrs_srv3,"yrs_srv3");
  obs_srv3_biom.allocate(1,nyrs_srv3,"obs_srv3_biom");
  obs_srv3_se.allocate(1,nyrs_srv3,"obs_srv3_se");
  obs_srv3_lci.allocate(1,nyrs_srv3,"obs_srv3_lci");
  obs_srv3_uci.allocate(1,nyrs_srv3,"obs_srv3_uci");
  nyrs_srv4.allocate("nyrs_srv4");
  yrs_srv4.allocate(1,nyrs_srv4,"yrs_srv4");
  obs_srv4_biom.allocate(1,nyrs_srv4,"obs_srv4_biom");
  obs_srv4_se.allocate(1,nyrs_srv4,"obs_srv4_se");
  obs_srv4_lci.allocate(1,nyrs_srv4,"obs_srv4_lci");
  obs_srv4_uci.allocate(1,nyrs_srv4,"obs_srv4_uci");
  nyrs_srv5.allocate("nyrs_srv5");
  yrs_srv5.allocate(1,nyrs_srv5,"yrs_srv5");
  obs_srv5_biom.allocate(1,nyrs_srv5,"obs_srv5_biom");
  obs_srv5_se.allocate(1,nyrs_srv5,"obs_srv5_se");
  obs_srv5_lci.allocate(1,nyrs_srv5,"obs_srv5_lci");
  obs_srv5_uci.allocate(1,nyrs_srv5,"obs_srv5_uci");
 cout << nyrs_srv5 << endl;
  nyrs_srv6.allocate("nyrs_srv6");
  yrs_srv6.allocate(1,nyrs_srv6,"yrs_srv6");
  obs_srv6_biom.allocate(1,nyrs_srv6,"obs_srv6_biom");
  obs_srv6_se.allocate(1,nyrs_srv6,"obs_srv6_se");
  obs_srv6_lci.allocate(1,nyrs_srv6,"obs_srv6_lci");
  obs_srv6_uci.allocate(1,nyrs_srv6,"obs_srv6_uci");
  nyrs_fish1_age.allocate("nyrs_fish1_age");
  yrs_fish1_age.allocate(1,nyrs_fish1_age,"yrs_fish1_age");
  nsamples_fish1_age.allocate(1,nyrs_fish1_age,"nsamples_fish1_age");
  oac_fish1.allocate(1,nyrs_fish1_age,1,nages,"oac_fish1");
  nyrs_srv1_age.allocate("nyrs_srv1_age");
  yrs_srv1_age.allocate(1,nyrs_srv1_age,"yrs_srv1_age");
  nsamples_srv1_age.allocate(1,nyrs_srv1_age,"nsamples_srv1_age");
  oac_srv1.allocate(1,nyrs_srv1_age,1,nages,"oac_srv1");
 ad_comm::change_datafile_name(unchanging_data);    // Read in size comps, size-age, things that aren't being updated
  nyrs_fish1_size.allocate("nyrs_fish1_size");
  yrs_fish1_size.allocate(1,nyrs_fish1_size,"yrs_fish1_size");
  nsamples_fish1_size.allocate(1,nyrs_fish1_size,"nsamples_fish1_size");
  osc_fish1_m.allocate(1,nyrs_fish1_size,1,nlenbins,"osc_fish1_m");
  osc_fish1_f.allocate(1,nyrs_fish1_size,1,nlenbins,"osc_fish1_f");
  nyrs_fish3_size.allocate("nyrs_fish3_size");
  yrs_fish3_size.allocate(1,nyrs_fish3_size,"yrs_fish3_size");
  nsamples_fish3_size.allocate(1,nyrs_fish3_size,"nsamples_fish3_size");
  osc_fish3_m.allocate(1,nyrs_fish3_size,1,nlenbins,"osc_fish3_m");
  osc_fish3_f.allocate(1,nyrs_fish3_size,1,nlenbins,"osc_fish3_f");
  nyrs_srv1_size.allocate("nyrs_srv1_size");
  yrs_srv1_size.allocate(1,nyrs_srv1_size,"yrs_srv1_size");
  nsamples_srv1_size.allocate(1,nyrs_srv1_size,"nsamples_srv1_size");
  osc_srv1_m.allocate(1,nyrs_srv1_size,1,nlenbins,"osc_srv1_m");
  osc_srv1_f.allocate(1,nyrs_srv1_size,1,nlenbins,"osc_srv1_f");
  nyrs_srv2_size.allocate("nyrs_srv2_size");
  yrs_srv2_size.allocate(1,nyrs_srv2_size,"yrs_srv2_size");
  nsamples_srv2_size.allocate(1,nyrs_srv2_size,"nsamples_srv2_size");
  osc_srv2_m.allocate(1,nyrs_srv2_size,1,nlenbins,"osc_srv2_m");
  osc_srv2_f.allocate(1,nyrs_srv2_size,1,nlenbins,"osc_srv2_f");
  sizeage_m.allocate(1,nages,1,nlenbins,"sizeage_m");
  sizeage_f.allocate(1,nages,1,nlenbins,"sizeage_f");
  sizeage_all.allocate(1,nages,1,nlenbins,"sizeage_all");
  sizeage_m_new.allocate(1,nages,1,nlenbins,"sizeage_m_new");
  sizeage_f_new.allocate(1,nages,1,nlenbins,"sizeage_f_new");
  ageage.allocate(1,nages,1,nages,"ageage");
  eof.allocate("eof");
 cout<<"The universal answer is "<<eof;
  offset.allocate(1,16);
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
}

void model_parameters::initializationfunction(void)
{
  sigr.set_initial_value(sigrprior);
  logm.set_initial_value(-2.30258509299);
  log_q_srv1.set_initial_value(2.02423694283);
  log_q_srv2.set_initial_value(1.85996498559);
  log_q_srv5.set_initial_value(1.44478842836);
  log_q_srv6.set_initial_value(2.62659387749);
  log_q_srv8.set_initial_value(1.77769087985);
  log_a50_fish1_f.set_initial_value(0.968317625958);
  log_a50_fish1_m.set_initial_value(1.99039715705);
  log_a50_fish2.set_initial_value(2.1);
  log_delta_fish2.set_initial_value(0.69874);
  log_a50_fish3_f.set_initial_value(1.78651832048);
  log_delta_fish3_f.set_initial_value(1.92684143177);
  log_delta_fish3_m.set_initial_value(2.30157239486);
  log_a50_fish4_f.set_initial_value(1.13279115361);
  log_delta_fish4_f.set_initial_value(0.723340585591);
  log_a50_fish4_m.set_initial_value(1.15627997343);
  log_delta_fish4_m.set_initial_value(0.945517826919);
  log_a50_srv1_f.set_initial_value(1.00000002481);
  log_delta_srv1_f.set_initial_value(1.75000000000);
  log_a50_srv1_m.set_initial_value(1.00000002481);
  log_delta_srv1_m.set_initial_value(1.55000000000);
  log_a50_srv2_f.set_initial_value(0.956710000);
  log_delta_srv2_m.set_initial_value(0.824300000);
}

model_parameters::model_parameters(int sz,int argc,char * argv[]) : 
 model_data(argc,argv) , function_minimizer(sz)
{
  initializationfunction();
  log_q_srv1.allocate(ph_q_srv1,"log_q_srv1");
  log_q_srv2.allocate(ph_q_srv2,"log_q_srv2");
  log_q_srv5.allocate(ph_q_srv5,"log_q_srv5");
  log_q_srv6.allocate(ph_q_srv6,"log_q_srv6");
  log_q_srv8.allocate(ph_q_srv8,"log_q_srv8");
  logm.allocate(-4,-1,ph_m,"logm");
  log_mean_rec.allocate(-10,5,1,"log_mean_rec");
  sigr.allocate(0.1,2,ph_sigr,"sigr");
  log_a50_fish1_f.allocate(-5,5,ph_fish_sel,"log_a50_fish1_f");
  log_delta_fish1_f.allocate(-10,10,-ph_fish_sel,"log_delta_fish1_f");
  a50_fish1_f.allocate("a50_fish1_f");
  #ifndef NO_AD_INITIALIZE
  a50_fish1_f.initialize();
  #endif
  delta_fish1_f.allocate("delta_fish1_f");
  #ifndef NO_AD_INITIALIZE
  delta_fish1_f.initialize();
  #endif
  fish1_sel_f.allocate(1,nages,"fish1_sel_f");
  #ifndef NO_AD_INITIALIZE
    fish1_sel_f.initialize();
  #endif
  log_a50_fish1_m.allocate(-5,5,ph_fish_sel,"log_a50_fish1_m");
  log_delta_fish1_m.allocate(-10,10,-ph_fish_sel,"log_delta_fish1_m");
  a50_fish1_m.allocate("a50_fish1_m");
  #ifndef NO_AD_INITIALIZE
  a50_fish1_m.initialize();
  #endif
  delta_fish1_m.allocate("delta_fish1_m");
  #ifndef NO_AD_INITIALIZE
  delta_fish1_m.initialize();
  #endif
  fish1_sel_m.allocate(1,nages,"fish1_sel_m");
  #ifndef NO_AD_INITIALIZE
    fish1_sel_m.initialize();
  #endif
  log_a50_fish2.allocate(-10,10,-1,"log_a50_fish2");
  log_delta_fish2.allocate(-10,10,-1,"log_delta_fish2");
  a50_fish2.allocate("a50_fish2");
  #ifndef NO_AD_INITIALIZE
  a50_fish2.initialize();
  #endif
  delta_fish2.allocate("delta_fish2");
  #ifndef NO_AD_INITIALIZE
  delta_fish2.initialize();
  #endif
  fish2_sel.allocate(1,nages,"fish2_sel");
  #ifndef NO_AD_INITIALIZE
    fish2_sel.initialize();
  #endif
  log_a50_fish3_f.allocate(-4,4,4,"log_a50_fish3_f");
  log_delta_fish3_f.allocate(-4,4,4,"log_delta_fish3_f");
  a50_fish3_f.allocate("a50_fish3_f");
  #ifndef NO_AD_INITIALIZE
  a50_fish3_f.initialize();
  #endif
  delta_fish3_f.allocate("delta_fish3_f");
  #ifndef NO_AD_INITIALIZE
  delta_fish3_f.initialize();
  #endif
  log_fish3_sel_f.allocate(1,nages,"log_fish3_sel_f");
  #ifndef NO_AD_INITIALIZE
    log_fish3_sel_f.initialize();
  #endif
  fish3_sel_f.allocate(1,nages,"fish3_sel_f");
  #ifndef NO_AD_INITIALIZE
    fish3_sel_f.initialize();
  #endif
  log_a50_fish3_m.allocate(-40,40,-4,"log_a50_fish3_m");
  log_delta_fish3_m.allocate(-40,40,-4,"log_delta_fish3_m");
  a50_fish3_m.allocate("a50_fish3_m");
  #ifndef NO_AD_INITIALIZE
  a50_fish3_m.initialize();
  #endif
  delta_fish3_m.allocate("delta_fish3_m");
  #ifndef NO_AD_INITIALIZE
  delta_fish3_m.initialize();
  #endif
  log_fish3_sel_m.allocate(1,nages,"log_fish3_sel_m");
  #ifndef NO_AD_INITIALIZE
    log_fish3_sel_m.initialize();
  #endif
  fish3_sel_m.allocate(1,nages,"fish3_sel_m");
  #ifndef NO_AD_INITIALIZE
    fish3_sel_m.initialize();
  #endif
  log_a50_fish4_f.allocate(0.1,3,ph_fish4_sel,"log_a50_fish4_f");
  log_delta_fish4_f.allocate(-1,6,ph_fish4_sel,"log_delta_fish4_f");
  a50_fish4_f.allocate("a50_fish4_f");
  #ifndef NO_AD_INITIALIZE
  a50_fish4_f.initialize();
  #endif
  delta_fish4_f.allocate("delta_fish4_f");
  #ifndef NO_AD_INITIALIZE
  delta_fish4_f.initialize();
  #endif
  fish4_sel_f.allocate(1,nages,"fish4_sel_f");
  #ifndef NO_AD_INITIALIZE
    fish4_sel_f.initialize();
  #endif
  log_a50_fish4_m.allocate(0.1,3,ph_fish4_sel,"log_a50_fish4_m");
  log_delta_fish4_m.allocate(0.1,6,ph_fish4_sel,"log_delta_fish4_m");
  a50_fish4_m.allocate("a50_fish4_m");
  #ifndef NO_AD_INITIALIZE
  a50_fish4_m.initialize();
  #endif
  delta_fish4_m.allocate("delta_fish4_m");
  #ifndef NO_AD_INITIALIZE
  delta_fish4_m.initialize();
  #endif
  fish4_sel_m.allocate(1,nages,"fish4_sel_m");
  #ifndef NO_AD_INITIALIZE
    fish4_sel_m.initialize();
  #endif
  log_a50_srv1_f.allocate(-0.5,3.5,ph_srv1_sel,"log_a50_srv1_f");
  log_delta_srv1_f.allocate(-0.5,3.5,-ph_srv1_sel,"log_delta_srv1_f");
  a50_srv1_f.allocate("a50_srv1_f");
  #ifndef NO_AD_INITIALIZE
  a50_srv1_f.initialize();
  #endif
  delta_srv1_f.allocate("delta_srv1_f");
  #ifndef NO_AD_INITIALIZE
  delta_srv1_f.initialize();
  #endif
  log_srv1_sel_f.allocate(1,nages,"log_srv1_sel_f");
  #ifndef NO_AD_INITIALIZE
    log_srv1_sel_f.initialize();
  #endif
  srv1_sel_f.allocate(1,nages,"srv1_sel_f");
  #ifndef NO_AD_INITIALIZE
    srv1_sel_f.initialize();
  #endif
  log_a50_srv1_m.allocate(-0.5,3.5,ph_srv1_sel,"log_a50_srv1_m");
  log_delta_srv1_m.allocate(-0.5,3.5,-ph_srv1_sel,"log_delta_srv1_m");
  a50_srv1_m.allocate("a50_srv1_m");
  #ifndef NO_AD_INITIALIZE
  a50_srv1_m.initialize();
  #endif
  delta_srv1_m.allocate("delta_srv1_m");
  #ifndef NO_AD_INITIALIZE
  delta_srv1_m.initialize();
  #endif
  log_srv1_sel_m.allocate(1,nages,"log_srv1_sel_m");
  #ifndef NO_AD_INITIALIZE
    log_srv1_sel_m.initialize();
  #endif
  srv1_sel_m.allocate(1,nages,"srv1_sel_m");
  #ifndef NO_AD_INITIALIZE
    srv1_sel_m.initialize();
  #endif
  log_a50_srv2_f.allocate(-2,3,-1,"log_a50_srv2_f");
  log_delta_srv2_f.allocate(-2,4,-1,"log_delta_srv2_f");
  a50_srv2_f.allocate("a50_srv2_f");
  #ifndef NO_AD_INITIALIZE
  a50_srv2_f.initialize();
  #endif
  delta_srv2_f.allocate("delta_srv2_f");
  #ifndef NO_AD_INITIALIZE
  delta_srv2_f.initialize();
  #endif
  log_srv2_sel_f.allocate(1,nages,"log_srv2_sel_f");
  #ifndef NO_AD_INITIALIZE
    log_srv2_sel_f.initialize();
  #endif
  srv2_sel_f.allocate(1,nages,"srv2_sel_f");
  #ifndef NO_AD_INITIALIZE
    srv2_sel_f.initialize();
  #endif
  log_a50_srv2_m.allocate(0.5,3.5,-1,"log_a50_srv2_m");
  log_delta_srv2_m.allocate(-2,10,-1,"log_delta_srv2_m");
  a50_srv2_m.allocate("a50_srv2_m");
  #ifndef NO_AD_INITIALIZE
  a50_srv2_m.initialize();
  #endif
  delta_srv2_m.allocate("delta_srv2_m");
  #ifndef NO_AD_INITIALIZE
  delta_srv2_m.initialize();
  #endif
  log_srv2_sel_m.allocate(1,nages,"log_srv2_sel_m");
  #ifndef NO_AD_INITIALIZE
    log_srv2_sel_m.initialize();
  #endif
  srv2_sel_m.allocate(1,nages,"srv2_sel_m");
  #ifndef NO_AD_INITIALIZE
    srv2_sel_m.initialize();
  #endif
  log_avg_F_fish1.allocate(-10,0,ph_avg_F,"log_avg_F_fish1");
  log_F_devs_fish1.allocate(styr,endyr,-10,10,ph_Fdev,"log_F_devs_fish1");
  log_avg_F_fish3.allocate(-10,10,ph_avg_F,"log_avg_F_fish3");
  log_F_devs_fish3.allocate(styr,endyr,-10,10,ph_Fdev,"log_F_devs_fish3");
  Fmort_fish1.allocate(styr,endyr,"Fmort_fish1");
  #ifndef NO_AD_INITIALIZE
    Fmort_fish1.initialize();
  #endif
  Fmort_fish3.allocate(styr,endyr,"Fmort_fish3");
  #ifndef NO_AD_INITIALIZE
    Fmort_fish3.initialize();
  #endif
  Z_f.allocate(styr,endyr,1,nages,"Z_f");
  #ifndef NO_AD_INITIALIZE
    Z_f.initialize();
  #endif
  Z_m.allocate(styr,endyr,1,nages,"Z_m");
  #ifndef NO_AD_INITIALIZE
    Z_m.initialize();
  #endif
  F_fish1_f.allocate(styr,endyr,1,nages,"F_fish1_f");
  #ifndef NO_AD_INITIALIZE
    F_fish1_f.initialize();
  #endif
  F_fish1_m.allocate(styr,endyr,1,nages,"F_fish1_m");
  #ifndef NO_AD_INITIALIZE
    F_fish1_m.initialize();
  #endif
  F_fish3_f.allocate(styr,endyr,1,nages,"F_fish3_f");
  #ifndef NO_AD_INITIALIZE
    F_fish3_f.initialize();
  #endif
  F_fish3_m.allocate(styr,endyr,1,nages,"F_fish3_m");
  #ifndef NO_AD_INITIALIZE
    F_fish3_m.initialize();
  #endif
  S_f.allocate(styr,endyr,1,nages,"S_f");
  #ifndef NO_AD_INITIALIZE
    S_f.initialize();
  #endif
  S_m.allocate(styr,endyr,1,nages,"S_m");
  #ifndef NO_AD_INITIALIZE
    S_m.initialize();
  #endif
  S_f_mid.allocate(styr,endyr,1,nages,"S_f_mid");
  #ifndef NO_AD_INITIALIZE
    S_f_mid.initialize();
  #endif
  S_m_mid.allocate(styr,endyr,1,nages,"S_m_mid");
  #ifndef NO_AD_INITIALIZE
    S_m_mid.initialize();
  #endif
  log_rec_dev.allocate(styr-nages+2,endyr_rec_est,-10,10,ph_recdev,"log_rec_dev");
  natage_f.allocate(styr,endyr,1,nages,"natage_f");
  #ifndef NO_AD_INITIALIZE
    natage_f.initialize();
  #endif
  natage_m.allocate(styr,endyr,1,nages,"natage_m");
  #ifndef NO_AD_INITIALIZE
    natage_m.initialize();
  #endif
  catage_fish1_f.allocate(styr,endyr,1,nages,"catage_fish1_f");
  #ifndef NO_AD_INITIALIZE
    catage_fish1_f.initialize();
  #endif
  catage_fish1_m.allocate(styr,endyr,1,nages,"catage_fish1_m");
  #ifndef NO_AD_INITIALIZE
    catage_fish1_m.initialize();
  #endif
  pred_catch_fish1.allocate(styr,endyr,"pred_catch_fish1");
  #ifndef NO_AD_INITIALIZE
    pred_catch_fish1.initialize();
  #endif
  catage_fish3_f.allocate(styr,endyr,1,nages,"catage_fish3_f");
  #ifndef NO_AD_INITIALIZE
    catage_fish3_f.initialize();
  #endif
  catage_fish3_m.allocate(styr,endyr,1,nages,"catage_fish3_m");
  #ifndef NO_AD_INITIALIZE
    catage_fish3_m.initialize();
  #endif
  pred_catch_fish3.allocate(styr,endyr,"pred_catch_fish3");
  #ifndef NO_AD_INITIALIZE
    pred_catch_fish3.initialize();
  #endif
  pred_srv3.allocate(1,nyrs_srv3,"pred_srv3");
  pred_srv4.allocate(1,nyrs_srv4,"pred_srv4");
  #ifndef NO_AD_INITIALIZE
    pred_srv4.initialize();
  #endif
  pred_srv5.allocate(1,nyrs_srv5,"pred_srv5");
  pred_srv6.allocate(1,nyrs_srv6,"pred_srv6");
  #ifndef NO_AD_INITIALIZE
    pred_srv6.initialize();
  #endif
  eac_fish1.allocate(1,nyrs_fish1_age,1,nages,"eac_fish1");
  #ifndef NO_AD_INITIALIZE
    eac_fish1.initialize();
  #endif
  eac_srv1.allocate(1,nyrs_srv1_age,1,nages,"eac_srv1");
  #ifndef NO_AD_INITIALIZE
    eac_srv1.initialize();
  #endif
  esc_fish1_m.allocate(1,nyrs_fish1_size,1,nlenbins,"esc_fish1_m");
  #ifndef NO_AD_INITIALIZE
    esc_fish1_m.initialize();
  #endif
  esc_fish1_f.allocate(1,nyrs_fish1_size,1,nlenbins,"esc_fish1_f");
  #ifndef NO_AD_INITIALIZE
    esc_fish1_f.initialize();
  #endif
  esc_fish3_m.allocate(1,nyrs_fish3_size,1,nlenbins,"esc_fish3_m");
  #ifndef NO_AD_INITIALIZE
    esc_fish3_m.initialize();
  #endif
  esc_fish3_f.allocate(1,nyrs_fish3_size,1,nlenbins,"esc_fish3_f");
  #ifndef NO_AD_INITIALIZE
    esc_fish3_f.initialize();
  #endif
  esc_srv1_m.allocate(1,nyrs_srv1_size,1,nlenbins,"esc_srv1_m");
  #ifndef NO_AD_INITIALIZE
    esc_srv1_m.initialize();
  #endif
  esc_srv1_f.allocate(1,nyrs_srv1_size,1,nlenbins,"esc_srv1_f");
  #ifndef NO_AD_INITIALIZE
    esc_srv1_f.initialize();
  #endif
  esc_srv2_m.allocate(1,nyrs_srv2_size,1,nlenbins,"esc_srv2_m");
  #ifndef NO_AD_INITIALIZE
    esc_srv2_m.initialize();
  #endif
  esc_srv2_f.allocate(1,nyrs_srv2_size,1,nlenbins,"esc_srv2_f");
  #ifndef NO_AD_INITIALIZE
    esc_srv2_f.initialize();
  #endif
  tot_biom.allocate(styr,endyr,"tot_biom");
  q_srv1.allocate("q_srv1");
  #ifndef NO_AD_INITIALIZE
  q_srv1.initialize();
  #endif
  q_srv2.allocate("q_srv2");
  #ifndef NO_AD_INITIALIZE
  q_srv2.initialize();
  #endif
  q_srv5.allocate("q_srv5");
  #ifndef NO_AD_INITIALIZE
  q_srv5.initialize();
  #endif
  q_srv6.allocate("q_srv6");
  #ifndef NO_AD_INITIALIZE
  q_srv6.initialize();
  #endif
  q_srv8.allocate("q_srv8");
  #ifndef NO_AD_INITIALIZE
  q_srv8.initialize();
  #endif
  pred_rec.allocate(styr,endyr,"pred_rec");
  avg_rec.allocate("avg_rec");
  #ifndef NO_AD_INITIALIZE
  avg_rec.initialize();
  #endif
  spbiom_trend.allocate("spbiom_trend");
  #ifndef NO_AD_INITIALIZE
  spbiom_trend.initialize();
  #endif
  Depletion.allocate("Depletion");
  #ifndef NO_AD_INITIALIZE
  Depletion.initialize();
  #endif
  spawn_biom.allocate(styr,endyr,"spawn_biom");
  natmort.allocate("natmort");
  #ifndef NO_AD_INITIALIZE
  natmort.initialize();
  #endif
  mF40.allocate(0.01,1.,ph_F40,"mF40");
  mF35.allocate(0.01,1.,ph_F40,"mF35");
  F40.allocate("F40");
  F35.allocate("F35");
  #ifndef NO_AD_INITIALIZE
  F35.initialize();
  #endif
  SB0.allocate("SB0");
  #ifndef NO_AD_INITIALIZE
  SB0.initialize();
  #endif
  SBF40.allocate("SBF40");
  #ifndef NO_AD_INITIALIZE
  SBF40.initialize();
  #endif
  SBF35.allocate("SBF35");
  #ifndef NO_AD_INITIALIZE
  SBF35.initialize();
  #endif
  sprpen.allocate("sprpen");
  #ifndef NO_AD_INITIALIZE
  sprpen.initialize();
  #endif
  Nspr.allocate(1,3,1,nages,"Nspr");
  #ifndef NO_AD_INITIALIZE
    Nspr.initialize();
  #endif
  hist_hal_F.allocate("hist_hal_F");
  #ifndef NO_AD_INITIALIZE
  hist_hal_F.initialize();
  #endif
  surv_like.allocate(1,18,"surv_like");
  #ifndef NO_AD_INITIALIZE
    surv_like.initialize();
  #endif
  age_like.allocate(1,16,"age_like");
  #ifndef NO_AD_INITIALIZE
    age_like.initialize();
  #endif
  rec_like.allocate("rec_like");
  #ifndef NO_AD_INITIALIZE
  rec_like.initialize();
  #endif
  ssqcatch.allocate("ssqcatch");
  #ifndef NO_AD_INITIALIZE
  ssqcatch.initialize();
  #endif
  F_mort_regularity.allocate("F_mort_regularity");
  #ifndef NO_AD_INITIALIZE
  F_mort_regularity.initialize();
  #endif
  avg_sel_penalty.allocate("avg_sel_penalty");
  #ifndef NO_AD_INITIALIZE
  avg_sel_penalty.initialize();
  #endif
  priors.allocate(1,13,"priors");
  #ifndef NO_AD_INITIALIZE
    priors.initialize();
  #endif
  Like.allocate("Like");
  #ifndef NO_AD_INITIALIZE
  Like.initialize();
  #endif
  obj_fun.allocate("obj_fun");
  prior_function_value.allocate("prior_function_value");
  likelihood_function_value.allocate("likelihood_function_value");
  pred_catch.allocate(styr,endyr,"pred_catch");
  #ifndef NO_AD_INITIALIZE
    pred_catch.initialize();
  #endif
  fratio.allocate("fratio");
  #ifndef NO_AD_INITIALIZE
  fratio.initialize();
  #endif
  N_proj_f.allocate(endyr+1,endyr+projyrs,1,nages,"N_proj_f");
  #ifndef NO_AD_INITIALIZE
    N_proj_f.initialize();
  #endif
  N_proj_m.allocate(endyr+1,endyr+projyrs,1,nages,"N_proj_m");
  #ifndef NO_AD_INITIALIZE
    N_proj_m.initialize();
  #endif
  FABC_proj.allocate("FABC_proj");
  #ifndef NO_AD_INITIALIZE
  FABC_proj.initialize();
  #endif
  FABC_tot_proj_f.allocate(1,nages,"FABC_tot_proj_f");
  #ifndef NO_AD_INITIALIZE
    FABC_tot_proj_f.initialize();
  #endif
  FABC_tot_proj_m.allocate(1,nages,"FABC_tot_proj_m");
  #ifndef NO_AD_INITIALIZE
    FABC_tot_proj_m.initialize();
  #endif
  FOFL_proj.allocate("FOFL_proj");
  #ifndef NO_AD_INITIALIZE
  FOFL_proj.initialize();
  #endif
  FOFL_tot_proj_f.allocate(1,nages,"FOFL_tot_proj_f");
  #ifndef NO_AD_INITIALIZE
    FOFL_tot_proj_f.initialize();
  #endif
  FOFL_tot_proj_m.allocate(1,nages,"FOFL_tot_proj_m");
  #ifndef NO_AD_INITIALIZE
    FOFL_tot_proj_m.initialize();
  #endif
  ABC.allocate("ABC");
  B40.allocate("B40");
  OFL.allocate("OFL");
  #ifndef NO_AD_INITIALIZE
  OFL.initialize();
  #endif
  Z_proj_f.allocate(1,nages,"Z_proj_f");
  #ifndef NO_AD_INITIALIZE
    Z_proj_f.initialize();
  #endif
  Z_proj_m.allocate(1,nages,"Z_proj_m");
  #ifndef NO_AD_INITIALIZE
    Z_proj_m.initialize();
  #endif
  ZOFL_proj_f.allocate(1,nages,"ZOFL_proj_f");
  #ifndef NO_AD_INITIALIZE
    ZOFL_proj_f.initialize();
  #endif
  ZOFL_proj_m.allocate(1,nages,"ZOFL_proj_m");
  #ifndef NO_AD_INITIALIZE
    ZOFL_proj_m.initialize();
  #endif
  S_proj_f.allocate(1,nages,"S_proj_f");
  #ifndef NO_AD_INITIALIZE
    S_proj_f.initialize();
  #endif
  S_proj_m.allocate(1,nages,"S_proj_m");
  #ifndef NO_AD_INITIALIZE
    S_proj_m.initialize();
  #endif
  catage_proj_f.allocate(endyr+1,endyr+projyrs,1,nages,"catage_proj_f");
  #ifndef NO_AD_INITIALIZE
    catage_proj_f.initialize();
  #endif
  catage_proj_m.allocate(endyr+1,endyr+projyrs,1,nages,"catage_proj_m");
  #ifndef NO_AD_INITIALIZE
    catage_proj_m.initialize();
  #endif
  catage_proj_OFL_f.allocate(endyr+1,endyr+projyrs,1,nages,"catage_proj_OFL_f");
  #ifndef NO_AD_INITIALIZE
    catage_proj_OFL_f.initialize();
  #endif
  catage_proj_OFL_m.allocate(endyr+1,endyr+projyrs,1,nages,"catage_proj_OFL_m");
  #ifndef NO_AD_INITIALIZE
    catage_proj_OFL_m.initialize();
  #endif
  pred_catch_proj_OFL_f.allocate(endyr+1,endyr+projyrs,"pred_catch_proj_OFL_f");
  #ifndef NO_AD_INITIALIZE
    pred_catch_proj_OFL_f.initialize();
  #endif
  pred_catch_proj_OFL_m.allocate(endyr+1,endyr+projyrs,"pred_catch_proj_OFL_m");
  #ifndef NO_AD_INITIALIZE
    pred_catch_proj_OFL_m.initialize();
  #endif
  spawn_biom_proj.allocate(endyr+1,endyr+projyrs,"spawn_biom_proj");
  tot_biom_proj.allocate(endyr+1,endyr+projyrs,"tot_biom_proj");
  pred_catch_proj.allocate(endyr+1,endyr+projyrs,"pred_catch_proj");
  #ifndef NO_AD_INITIALIZE
    pred_catch_proj.initialize();
  #endif
  pred_catch_proj_OFL.allocate(endyr+1,endyr+projyrs,"pred_catch_proj_OFL");
  #ifndef NO_AD_INITIALIZE
    pred_catch_proj_OFL.initialize();
  #endif
  stdev_rec.allocate("stdev_rec");
  #ifndef NO_AD_INITIALIZE
  stdev_rec.initialize();
  #endif
  FOFL.allocate("FOFL");
  #ifndef NO_AD_INITIALIZE
  FOFL.initialize();
  #endif
  FABC.allocate("FABC");
  #ifndef NO_AD_INITIALIZE
  FABC.initialize();
  #endif
  FOFL2.allocate("FOFL2");
  #ifndef NO_AD_INITIALIZE
  FOFL2.initialize();
  #endif
  FABC2.allocate("FABC2");
  #ifndef NO_AD_INITIALIZE
  FABC2.initialize();
  #endif
}

void model_parameters::userfunction(void)
{
  obj_fun =0.0;
  ofstream& evalout= *pad_evalout;
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
     Evaluate_Objective_Function();				// Minimize objective function value
    if (mceval_phase())									// For outputting MCMC simulations in text format 
    {
     evalout<<log_mean_rec<<" "<<sigr<<" "<<q_srv1<<" "<<q_srv2<<" "<<q_srv5<<" "<<q_srv6<<" "<<q_srv8<<" "<<F40<<" "<<B40<<" "<<natmort<<" "<<obj_fun<<" "<<tot_biom<<" "<<spawn_biom<<" "<<pred_rec<<" "<<spawn_biom_proj<<" "<<pred_catch_proj<<" "<<pred_srv3<<" "<<pred_srv5<<" "<<endl; 
     }
}

void model_parameters::Get_Selectivity(void)
{
  ofstream& evalout= *pad_evalout;
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
}

void model_parameters::Get_Mortality_Rates(void)
{
  ofstream& evalout= *pad_evalout;
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
}

void model_parameters::Get_Numbers_At_Age(void)
{
  ofstream& evalout= *pad_evalout;
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
  for (i=styr; i<= endyr_rec; i++)  // so, for 1960-endyr...
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
	   for(i=endyr_rec+1;i<endyr;i++){
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
}

void model_parameters::Get_Catch_at_Age(void)
{
  ofstream& evalout= *pad_evalout;
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
}

void model_parameters::Get_Dependent_Vars(void)
{
  ofstream& evalout= *pad_evalout;
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
}

void model_parameters::Get_Predicted_Values(void)
{
  ofstream& evalout= *pad_evalout;
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
}

void model_parameters::compute_spr_rates(void)
{
  ofstream& evalout= *pad_evalout;
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
}

void model_parameters::Calc_priors(void)
{
  ofstream& evalout= *pad_evalout;
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
}

void model_parameters::Surv_Likelihood(void)
{
  ofstream& evalout= *pad_evalout;
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
}

void model_parameters::Multinomial_Likelihood(void)
{
  ofstream& evalout= *pad_evalout;
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
}

double model_parameters::round(double r)
{
  ofstream& evalout= *pad_evalout;
    return double((r > 0.0) ? floor(r + 0.5) : ceil(r - 0.5)); 
}

void model_parameters::Get_Population_Projection(void)
{
  ofstream& evalout= *pad_evalout;
  int k;
  if(mceval_phase()) {
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
    for (j=1;j<=nages;j++)
     { 
      catage_proj_f(i,j) = yieldratio*N_proj_f(i,j)* FABC_tot_proj_f(j)/Z_proj_f(j)*(1.-mfexp(-Z_proj_f(j)));
      catage_proj_m(i,j) = yieldratio*N_proj_m(i,j)* FABC_tot_proj_m(j)/Z_proj_m(j)*(1.-mfexp(-Z_proj_m(j)));
      catage_proj_OFL_f(i,j) = yieldratio*N_proj_f(i,j)* FOFL_tot_proj_f(j)/ZOFL_proj_f(j)*(1.-mfexp(-ZOFL_proj_f(j)));
      catage_proj_OFL_m(i,j) = yieldratio*N_proj_m(i,j)* FOFL_tot_proj_m(j)/ZOFL_proj_m(j)*(1.-mfexp(-ZOFL_proj_m(j)));
       }
    pred_catch_proj(i)     = (catage_proj_f(i)*wt_f+catage_proj_m(i)*wt_m)/yieldratio;
    pred_catch_proj_OFL(i)     =  (catage_proj_OFL_f(i)*wt_f+catage_proj_OFL_m(i)*wt_m)/yieldratio;
    if (i < endyr+projyrs)
    {
    if(mceval_phase()) {
    stdev_rec = sqrt(norm2(value(log_rec_dev(1979,endyr-recage-1))-mean(value(log_rec_dev(1979,endyr-recage-1))))/(size_count(value(log_rec_dev(1979,endyr-recage)))-1));
     k=round(value(spawn_biom(endyr)*10000))+i;
    k=k+i;
     N_proj_f(i+1,1)= mfexp(value(log(mean(value(pred_rec(1979,endyr-recage-1))))-square(stdev_rec)/2+stdev_rec*randn(k+l)))/2;
     N_proj_m(i+1,1)= mfexp(value(log(mean(value(pred_rec(1979,endyr-recage-1))))-square(stdev_rec)/2+stdev_rec*randn(k+l)))/2; }
    else {  N_proj_f(i+1,1)= mfexp(value(log(mean(pred_rec(1979,endyr-recage-1)))))/2;
            N_proj_m(i+1,1)= mfexp(value(log(mean(pred_rec(1979,endyr-recage-1)))))/2; }
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
}

void model_parameters::Evaluate_Objective_Function(void)
{
  ofstream& evalout= *pad_evalout;
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
}

void model_parameters::report(const dvector& gradients)
{
 adstring ad_tmp=initial_params::get_reportfile_name();
  ofstream report((char*)(adprogram_name + ad_tmp));
  if (!report)
  {
    cerr << "error trying to open report file"  << adprogram_name << ".rep";
    return;
  }
  if(last_phase()) {
  //report out things needed to feed into apportionment script:
  report << "$B40" << endl;
  report << B40 << endl;
  report << "$SBF40" << endl;
  report << SBF40 << endl;
  report << "$spawn_biom" << endl;
  report << spawn_biom << endl;
  report << "$Depletion"<< endl; 
  report << Depletion << endl;
  report << "$ABC_proj" << endl;
  report << pred_catch_proj << endl;
  report << "$spawn_bio_projected" << endl; 
  report << spawn_biom_proj << endl;
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
  }
}

void model_parameters::write_fullrep(void)
{
  ofstream& evalout= *pad_evalout;
  ofstream fullrep("sable.rep");
  if(last_phase()) {
  fullrep << "$Pred_catch" << endl; 
  fullrep << pred_catch <<endl;
  fullrep << "$avgFmort" << endl; 
  fullrep << log_avg_F_fish1 << endl;
  fullrep << "$F35" << endl;
  fullrep << F35 << endl;
  fullrep << "$F40" << endl;
  fullrep << F40 << endl;
  fullrep << "$SBF35" << endl;
  fullrep << SBF35 << endl;
  fullrep << "$F_recruitment" << endl; 
  fullrep << trans(natage_f)(1) << endl;
  fullrep << "$M_recruitment" << endl; 
  fullrep << trans(natage_m)(1) << endl;
  fullrep << "$Spec_catch_proj" << endl;
  fullrep << (catage_proj_f*wt_f+catage_proj_m*wt_m) << endl; 
  fullrep << "spawn_bio next year"<<endl<<spawn_biom_proj(endyr+1)<<endl;
  fullrep <<" spawn_bio projected"<<endl<<spawn_biom_proj<<endl;
  fullrep << "Specified_catch_projection"<<(catage_proj_f*wt_f+catage_proj_m*wt_m)<<endl<<"ABC projection: "<<pred_catch_proj<<endl;
  //EM structure  
  //fullrep << "single area model output" << endl;
  //fullrep << "model_start_year" << endl;
  //fullrep << $styr << endl;
  //fullrep << "model_end_year" << endl;
  //fullrep << $endyr << endl;  
  fullrep << model_name << endl;
  fullrep <<  data_file << endl;
  fullrep << "Num_parameters_Estimated " << endl; 
  fullrep << initial_params::nvarcalc() << endl;
  fullrep << "years"<< endl;
  fullrep << yy << endl;
  //Estimated values
  fullrep << "yrs_srv4" <<endl << yrs_srv4 <<endl;
  fullrep << "pred_srv4_biom" <<endl << pred_srv4  <<endl;
  fullrep << "obs_srv4_biom" << endl << obs_srv4_biom  <<endl;
  fullrep << "yrs_srv3" << endl << yrs_srv3  <<endl;
  fullrep << "pred_srv3_biom" << endl << pred_srv3  <<endl;
  fullrep << "obs_srv3_biom" << endl << obs_srv3_biom  <<endl;
  fullrep << "yrs_srv5" << endl << yrs_srv5  <<endl;
  fullrep << "pred_srv5_biom"<< endl << pred_srv5  <<endl;
  fullrep << "obs_srv5_biom" << endl << obs_srv5_biom  <<endl;
  fullrep << "yrs_srv6" << endl << yrs_srv6  <<endl;
  fullrep << "pred_srv6_biom"<< endl << pred_srv6  <<endl;
  fullrep << "obs_srv6_biom" << endl << obs_srv6_biom  <<endl;
  fullrep << "Fully_selected_F" << endl << Fmort_fish1*max(fish1_sel_f)+Fmort_fish3 << endl;
  fullrep << "spawn_biom"<< endl << spawn_biom <<endl;
  fullrep << "tot_biom"<< endl << tot_biom   <<endl;
  fullrep << "pred_catch_fish1"<< endl << pred_catch_fish1<<endl;
  fullrep << "obs_catch_fish1"<< endl << obs_catch_fish1<<endl;
  fullrep << "pred_catch_fish3"<< endl << pred_catch_fish3<<endl;
  fullrep << "obs_catch_fish3"<< endl << obs_catch_fish3<<endl;
  fullrep << "fish1_sel_f"<< endl << fish1_sel_f <<endl;
  fullrep << "fish1_sel_m"<< endl << fish1_sel_m <<endl;
  fullrep << "fish2_sel"<< endl << fish2_sel <<endl;
  fullrep << "fish3_sel_f"<< endl << fish3_sel_f <<endl;
  fullrep << "fish3_sel_m"<< endl << fish3_sel_m  <<endl;
  fullrep << "fish4_sel_f"<< endl << fish4_sel_f <<endl;
  fullrep << "fish4_sel_m"<< endl << fish4_sel_m  <<endl;
  fullrep << "srv1_sel_f"<< endl << srv1_sel_f <<endl;
  fullrep << "srv1_sel_m"<< endl << srv1_sel_m  <<endl;
  fullrep << "srv2_sel_f"<< endl << srv2_sel_f  <<endl;
  fullrep << "srv2_sel_m"<< endl << srv2_sel_m  <<endl;
  fullrep << "SigmaR: "<<sigr<< " Nat_Mort: "<<natmort<< endl;
  fullrep <<"Spawning Per Recruit "<< " "<<SBF40<< endl;
  fullrep << "Virgin SPR (SB0) "<<SB0 <<endl;
  fullrep<<"log_mean_rec"<< endl << log_mean_rec<< endl;
  fullrep<<"log_rec_dev" << endl << log_rec_dev << endl;
  fullrep<<"pred_rec" << endl << pred_rec << endl;
  fullrep << "q_srv1" << q_srv1<<endl;
  fullrep << "q_srv2" << q_srv2<<endl;
  fullrep << "q_srv5" << q_srv5<<endl;
  fullrep << "q_srv6" << q_srv6<<endl;
  fullrep << "q_srv8" << q_srv8<<endl;
  //likelihoods
  fullrep << "Wts_n_Likelihoods" << endl;
  fullrep << "SSQ_catch likelihood" << endl << ssqcatch << endl;
  fullrep << "Surv_like" << endl;
  fullrep << "order: srv3,srv4,srv5,srv6," << surv_like << endl;
  fullrep << "Age_like" << endl;
  fullrep << "order: fish1a, srv1a,0, fish1s f/m, fish3s f/m, 0, srv1s f/m, srv2s f/m" << endl;
  fullrep <<  age_like << endl;
  fullrep << wt_rec_var <<" " << rec_like <<" " ; fullrep << "Recruitment_Deviations_Likelihood" << endl;
  fullrep << wt_fmort_reg <<" "<<F_mort_regularity<<" " ; fullrep << "Fishing_Mortality_Regularity_Penalty" << endl;
  fullrep << " "<<priors(1)  <<" " ; fullrep << "priors sigr"     <<endl;
  fullrep << " "<<priors(2)  <<" " ; fullrep << "priors q_srv1" <<endl;
  fullrep << " "<<priors(4)  <<" " ; fullrep << "priors M"<<endl;
  fullrep << " "<<priors(7)  <<" " ; fullrep << "priors q_srv2"<<endl;
  fullrep << " "<<priors(9)  <<" " ; fullrep << "priors q_srv5"<<endl;
  fullrep << " "<<priors(10)  <<" " ; fullrep << "priors q_srv6"<<endl;
  fullrep << " "<<priors(11)  <<" " ; fullrep << "priors q_srv8"<<endl;
  fullrep << sprpen << " "<<"Sprpen" <<endl;
  fullrep << " "<<obj_fun    <<" " ; fullrep << "obj_fun"         <<endl;
  fullrep << " "<<Like       <<" " ; fullrep << "data likelihood" <<endl;//(2*square(sigr))+ size_count(log_rec_dev)*log(sigr)<<endl;
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
  }
}

double model_parameters::sdnr(const dvar_vector& pred,const dvector& obs,double m)
{
  ofstream& evalout= *pad_evalout;
  RETURN_ARRAYS_INCREMENT();
  double sdnr;
  dvector pp = value(pred)+0.000001;
  int ntmp = -obs.indexmin()+obs.indexmax();
  sdnr = std_dev(elem_div(obs+0.000001-pp,sqrt(elem_prod(pp,(1.-pp))/m)));
  RETURN_ARRAYS_DECREMENT();
  return sdnr;
}

void model_parameters::set_runtime(void)
{
  dvector temp("{1.e-4, 1.e-4, 1.e-4, 1.e-4, 1.e-4  }");
  convergence_criteria.allocate(temp.indexmin(),temp.indexmax());
  convergence_criteria=temp;
  dvector temp1("{1000, 1000, 1000, 1000, 1000}");
  maximum_function_evaluations.allocate(temp1.indexmin(),temp1.indexmax());
  maximum_function_evaluations=temp1;
}

void model_parameters::final_calcs()
{
  write_fullrep();
}

void model_parameters::preliminary_calculations(void){
#if defined(USE_ADPVM)

  admaster_slave_variable_interface(*this);

#endif
}

model_data::~model_data()
{}

model_parameters::~model_parameters()
{
  delete pad_evalout;
  pad_evalout = NULL;
}

#ifdef _BORLANDC_
  extern unsigned _stklen=10000U;
#endif


#ifdef __ZTC__
  extern unsigned int _stack=10000U;
#endif

  long int arrmblsize=0;

int main(int argc,char * argv[])
{
    ad_set_new_handler();
  ad_exit=&ad_boundf;
  gradient_structure::set_MAX_NVAR_OFFSET(1000);
  gradient_structure::set_GRADSTACK_BUFFER_SIZE(100000);
  //arrmblsize=390000;
  arrmblsize=1500000;
    gradient_structure::set_NO_DERIVATIVES();
    gradient_structure::set_YES_SAVE_VARIABLES_VALUES();
    if (!arrmblsize) arrmblsize=15000000;
    model_parameters mp(arrmblsize,argc,argv);
    mp.iprint=10;
    mp.preliminary_calculations();
    mp.computations(argc,argv);
    return 0;
}

extern "C"  {
  void ad_boundf(int i)
  {
    /* so we can stop here */
    exit(i);
  }
}
