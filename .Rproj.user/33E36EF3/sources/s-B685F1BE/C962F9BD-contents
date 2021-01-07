data { 
  int num_data; 
  int num_basis; 
  int bdim1;
  vector[num_data] cases; 
  vector[num_data] X; 
  vector[11] X_new; 

  matrix[num_basis, num_data] B; 
  matrix[bdim1, 11] B_new; 

} 
 
parameters { 
  row_vector[num_basis] a_raw; 
  real a0; 
  real<lower=0> sigma; 
  real<lower=0> tau; 
} 
 
transformed parameters { 
  row_vector[num_basis] a; 
  vector[num_data] Y_hat; 
  a = a_raw*tau;  
  Y_hat = a0*X + to_vector(a*B); 
} 
 
model { 
  a_raw ~ normal(0, 1); 
  tau ~ cauchy(0, 10000); 
  cases ~ normal(Y_hat, 100); 
} 

generated quantities {
   real pred_hat[11];
   pred_hat = normal_rng(a0*X_new + to_vector(a*B_new),1); 
}