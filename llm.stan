data {
  int<lower=1> n;
  vector[n] y_cases;
  vector[n] y_deaths;
  int day_of_week[n];
  matrix[n,n] mask;
  matrix[n+27,n+27] mask_w_pred;

}
parameters {
  vector[n] mu_cases;
  vector[n-1] v_cases;
  
  vector[n] mu_deaths;
  vector[n-1] v_deaths;
  
  vector[7] day_of_week_effect;
 
  
  positive_ordered[3] sigma_cases;
  positive_ordered[3] sigma_deaths;

}
transformed parameters {
  vector[n] yhat_cases;
  vector[n] yhat_deaths;
  vector[7] day_of_week_effect_normalized;

  yhat_cases = mu_cases;
  
  yhat_deaths = .001*mask*yhat_cases*.5 + .5*mu_deaths; 
  day_of_week_effect_normalized[1:6] = day_of_week_effect[1:6];
  day_of_week_effect_normalized[7] = -sum(day_of_week_effect[1:6]);
}
model {
  v_cases[1] ~ normal(0, 1);
  for(t in 2:n-1)
    v_cases[t] ~ normal(v_cases[t-1], 1);

  mu_cases[1] ~ normal(y_cases[1], 1);
  for(t in 2:n)
    mu_cases[t] ~ normal(mu_cases[t-1] + v_cases[t-1] , 1);


   v_deaths[1] ~ normal(0, .1);
  for(t in 2:n-1)
    v_deaths[t] ~ normal(v_deaths[t-1], .1);

  mu_deaths[1] ~ normal(y_deaths[1], .1);
  for(t in 2:n)
    mu_deaths[t] ~ normal(mu_deaths[t-1] + v_deaths[t-1] , .1);

  

  y_cases ~ normal( yhat_cases, .1);
  y_deaths ~ normal( yhat_deaths, .1);



}

 generated quantities {
    vector[27] y_hat_cases_pred_vec;
    real  y_hat_cases_pred_real[27];
    
    vector[27] y_hat_deaths_pred_vec;
    real  y_hat_deaths_pred_real[27];

   vector[27] v_pred;
   vector[27] mu_pred;
   
   vector[27] v_pred_deaths;
   vector[27] mu_pred_deaths;
 
   vector[27+n] y_hat_cases_total;
    vector[27+n] y_hat_deaths_total;
    vector[27+n] y_hat_deaths_total_part_1;


   v_pred_deaths[1] = v_deaths[n-1];
   for(t in 2:27){
    v_pred_deaths[t] = normal_rng(v_pred_deaths[t-1], .00001);
   }
   mu_pred_deaths[1] = mu_deaths[n];
   for(t in 2:27){
    mu_pred_deaths[t] = normal_rng(mu_pred_deaths[t-1] + v_pred_deaths[t-1], .00001);
   }
   
   
    v_pred[1] = v_cases[n-1];
   for(t in 2:27){
    v_pred[t] = normal_rng(v_pred[t-1], .00001);
   }
   mu_pred[1] = mu_cases[n];
   for(t in 2:27){
    mu_pred[t] = normal_rng(mu_pred[t-1] + v_pred[t-1], .00001);
   }
   
   y_hat_deaths_pred_real = normal_rng(mu_pred_deaths, .0001);
   for (i in 1:27){
     y_hat_deaths_pred_vec[i] = y_hat_deaths_pred_real[i];
   }
   
   
   y_hat_cases_pred_real = normal_rng(mu_pred, .0001);
   for (i in 1:27){
     y_hat_cases_pred_vec[i] = y_hat_cases_pred_real[i];
   }
   
   y_hat_cases_total = append_row(yhat_cases, y_hat_cases_pred_vec);
   y_hat_deaths_total_part_1 = append_row(yhat_deaths, y_hat_deaths_pred_vec);

   y_hat_deaths_total = .001*mask_w_pred*y_hat_cases_total*.5 + .5*y_hat_deaths_total_part_1;


}





