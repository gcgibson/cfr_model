data { 
  int num_data; 
  int num_basis; 
  
  matrix[num_data,num_data] mask;
  matrix[num_data+28,num_data+28] mask_pred;
  vector[num_data] y_deaths; 
  vector[num_data] y_cases; 

  vector[num_data] X; 
  vector[28] X_new; 
  matrix[num_basis, num_data] B; 
  matrix[num_basis, 28] B_predict; 

} 
 
parameters { 
  row_vector[num_basis] a_raw; 
  real a0; 
  real<lower=0> sigma; 
  real<lower=0> tau; 
  real cfr_transform[num_data];
} 
 
transformed parameters { 
  row_vector[num_basis] a;
  vector[num_data] pred_death;
  real tmp_var;
  vector[num_data] Y_hat; 

  a[1] = a_raw[1];
  for (i in 2:num_basis){
    tmp_var = if_else(i < num_basis-3,tau,tau/100);
    a[i] = a[i-1] + a_raw[i]*tmp_var;

  }

    
  
  Y_hat = a0*to_vector(X) + to_vector(a*B);
  
  pred_death = .001*(mask*Y_hat);
} 
 
model { 
  
  cfr_transform[1] ~ normal(.01,.1);
  for (i in 2:num_data){
    cfr_transform[i] ~ normal(cfr_transform[i-1],.05);
  }
  
  a_raw ~ normal(0, 10000); 

  tau ~ normal(0, 1000); 
  sigma ~ cauchy(0, 1); 
  y_cases ~ normal(Y_hat, 1);
  y_deaths ~ normal(pred_death,1);
} 

generated quantities {
  vector[28] predicted_y_hat;
  row_vector[num_basis] transformed_a;
  vector[num_data+28] cfr_transform_new;

  vector[num_data +28] predicted_death;
  
#  transformed_a = to_row_vector(rep_vector(mean(a[(num_basis-20):num_basis]),num_basis));
  cfr_transform_new = to_vector(append_array(cfr_transform,rep_array(cfr_transform[num_data],28)));
  predicted_y_hat = a0*X_new + to_vector(a*B_predict);
  predicted_death =   .001*(mask_pred*to_vector(append_array(to_array_1d(Y_hat),to_array_1d(predicted_y_hat))));


}