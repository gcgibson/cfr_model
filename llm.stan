data {
  int<lower=1> n;
  vector[n] y_cases;
  vector[n] y_deaths;
  int day_of_week[n];
  matrix[n,n] mask;

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
  
  yhat_deaths = mu_deaths;
  day_of_week_effect_normalized[1:6] = day_of_week_effect[1:6];
  day_of_week_effect_normalized[7] = -sum(day_of_week_effect[1:6]);
}
model {
  v_cases[1] ~ normal(0, 10);
  for(t in 2:n-1)
    v_cases[t] ~ normal(v_cases[t-1], 10);

  mu_cases[1] ~ normal(y_cases[1], 10);
  for(t in 2:n)
    mu_cases[t] ~ normal(mu_cases[t-1] + v_cases[t-1], 10);

  y_cases ~ normal(yhat_cases, .0001);
  
  day_of_week_effect[1] ~normal(0,1);
  for (t in 2:7){
    day_of_week_effect[t] ~ normal(0,1);
  }

  v_deaths[1] ~ normal(0, .00001);
  for(t in 2:n-1)
    v_deaths[t] ~ normal(v_deaths[t-1], .00001);

  mu_deaths[1] ~ normal(y_deaths[1], .0001);
  for(t in 2:n)
    mu_deaths[t] ~ normal(mu_deaths[t-1] + v_deaths[t-1] , .0001);

  y_deaths ~ normal( yhat_deaths + mask*(yhat_cases)*.001 +   day_of_week_effect_normalized[day_of_week], .0001);



}

generated quantities {
   real y_hat_pred[27];
   vector[27] v_pred;
  vector[27] mu_pred;


   v_pred[1] = v_deaths[n-1];
   for(t in 2:27){
    v_pred[t] = normal_rng(v_pred[t-1], .001);
   }
   mu_pred[1] = mu_deaths[n];
   for(t in 2:27){
    mu_pred[t] = normal_rng(mu_pred[t-1] + v_pred[t-1], .001);
   }
   y_hat_pred = normal_rng(mu_pred, 1);

}





