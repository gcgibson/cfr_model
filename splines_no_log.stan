data { 
  int num_data; 
  int num_basis_cases; 
  int num_basis_deaths; 

  
  vector[num_data] y_deaths;
  vector[num_data] y_cases;
  

  vector[num_data] X_cases; 
  vector[28] X_cases_new; 
  
  vector[num_data] X_deaths; 
  vector[28] X_deaths_new; 
  
  matrix[num_data,num_data] mask;
  matrix[num_data+28,num_data+28] mask_w_pred;

  matrix[num_basis_cases, num_data] B_cases; 
  matrix[num_basis_cases, 28] B_cases_predict; 
  
  matrix[num_basis_deaths, num_data] B_deaths; 
  matrix[num_basis_deaths, 28] B_deaths_predict; 

} 
 
parameters { 
  row_vector[num_basis_cases] a_cases_raw; 
  row_vector[num_basis_deaths] a_deaths_raw; 

  real cfr[num_data];
  
  real a0_cases; 
  real<lower=0> sigma_cases; 
  real<lower=0> tau_cases; 
  
  real a0_deaths; 
  real<lower=0> sigma_deaths; 
  real<lower=0> tau_deaths; 
  
  real beta;
  
} 
 
transformed parameters { 
  row_vector[num_basis_cases] a_cases; 
  vector[num_data] Y_hat_cases;
  

  row_vector[num_basis_deaths] a_deaths; 
  vector[num_data] Y_hat_deaths; 
  
  vector[num_data] cfr_logit;
 
  cfr_logit = to_vector(inv_logit(cfr));
 
 
  a_cases[1] = a_cases_raw[1];
  for (i in 2:num_basis_cases)
    a_cases[i] = a_cases[i-1] + a_cases_raw[i]*tau_cases;
  Y_hat_cases = a0_cases*to_vector(X_cases) + to_vector(a_cases*B_cases);

 

    a_deaths[1] = a_deaths_raw[1];
    for (i in 2:num_basis_deaths)
       a_deaths[i] = a_deaths[i-1] + a_deaths_raw[i]*tau_deaths;
    #Y_hat_deaths = a0_deaths*to_vector(X_deaths) + to_vector(a_deaths*B_deaths) ;
    Y_hat_deaths = cfr_logit .* (mask*Y_hat_cases) ;
  
  

} 
 
model { 
  a_cases_raw ~ normal(0, 10); 
  tau_cases ~ cauchy(0, 100); 
  sigma_cases ~ normal(0, 1); 
  
  a_deaths_raw ~ normal(0, 1); 
  tau_deaths ~ cauchy(0, 1); 
  sigma_deaths ~ normal(0, .5); 
  
  cfr[1] ~ normal(logit(.005),.2);
  for (i in 2:num_data){
    cfr[i] ~ normal(cfr[i-1],.2);
  }

   y_cases ~ normal(Y_hat_cases, sigma_cases); 
   y_deaths ~ normal(Y_hat_deaths, sigma_deaths); 


} 

generated quantities {
  

   vector[28] cfr_predict;
  
  vector[28+num_data] pred_death_total;
  vector[28+num_data] pred_cases_total;
  vector[28+num_data] pred_cfr_total;

  
  vector[28] pred_cases;
  
  cfr_predict = to_vector(rep_array(cfr_logit[num_data],28));
  
  pred_cfr_total=append_row(cfr_logit,cfr_predict);
  
  pred_cases = a0_cases*to_vector(X_cases_new) + to_vector(a_cases*B_cases_predict);
  
  pred_cases_total = append_row(Y_hat_cases,pred_cases);

  pred_death_total = pred_cfr_total .* (mask_w_pred*pred_cases_total) ;


  
  
}


