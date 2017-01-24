data {
  int<lower=0> n; //
  int nst;
  int<lower=0> nsta[n]; //STATION ID
  vector[n] y; // Y Vector
  vector[n] x; // X Vector
}

parameters {
  real beta0[nst];  // intercept
  real beta1[nst];  // slope
  real<lower=min(x), upper=max(x)> tau;    // changepoint
  real<lower=0> sigmay;  // variance
  //real<lower=0> sigma[4];
  real mu1;
  real mu2;
 // real mu3;
  real mu4;
}

model {
    vector[n] yhat;

  // Priors
    mu1 ~ normal(150, 100);
    mu2 ~ normal(0, 10);
   // mu3 ~ normal(0, 100);
    //mu4 ~ normal(0, 100);
    sigmay ~ inv_gamma(0.001, 0.001);
    //sigma~uniform(0,100);
    //beta0 ~ normal(mu1,sigma[1]);
    beta0 ~ normal(mu1,150);
   // beta1 ~ normal(mu2,sigma[2]);
beta1 ~ normal(mu2,10);
    tau ~ uniform(min(x), max(x));
  //
  for(i in 1:n) {
    yhat[i] = beta0[nsta[i]] + int_step(x[i]-tau)*beta1[nsta[i]]*(x[i] - tau);
  }

  // likelihood / probability model
    y ~ normal(yhat, sigmay);
}
