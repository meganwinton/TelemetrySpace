// Declare data
data {
  int<lower=0> nind;               // number of individuals
  int<lower=0> nrec;               // number of receivers
  int<lower=0> ntime;              // number of time steps
  int<lower=0> ntrans;             // number of trials/expected number of transmissions per time step
  int<lower=0> y[nind,nrec,ntime]; // number of detections for each individual at each receiver in each time step
  real recX[nrec];                 // trap locations in east-west direction
  real recY[nrec];                 // trap locations in north-south direction
  real xlim[2];                    // area bounds east-west
  real ylim[2];                    // area boundes north-south
}

// Declare parameters
parameters {
  // fixed effects
  real<lower=-7, upper=7> alpha0;  // detection probability intercept on the logit scale - bounds are to ensure only searching reasonable parameter space
  real<lower=0> alpha1;  // coef. for decline in detection probability with distance

  // latent variables
  real<lower=xlim[1], upper=xlim[2]> sx[nind,ntime];  // E-W center of activity coordinate - bounds reflect spatial extent
  real<lower=ylim[1], upper=ylim[2]> sy[nind,ntime];  // N-S center of activity coordinate - bounds reflect spatial extent
}

// Declare derived/transformed parameters
transformed parameters  {
   real p0;         // Detection probability at a distance of 0
   real sigma;      // Standard deviation of the distance-decay function
   real d[nind,nrec,ntime];  // Array to store distances distances
   
   p0 = exp(alpha0)/(1+exp(alpha0)); // Inverse logit of alpha0 - constrains probability b/tw 0 and 1
   sigma = sqrt(1/(2*alpha1));       // Derived from coefficient specifying distance-related decay in detection prob.
  
   for (t in 1:ntime){ // For each time step
    for (i in 1:nind) { // And each individual   
      for (j in 1:nrec){ // And each receiver
      // Calculate the Euclidean distance from each COA to each receiver in each time step
      d[i,j,t] = pow(pow(sx[i,t]-recX[j],2) + pow(sy[i,t]-recY[j],2),0.5);
    }
   }
  }
}

model {
  // priors
  alpha0~cauchy(0,2.5);
  alpha1~cauchy(0,2.5);
  
  // likelihood
  for (t in 1:ntime){ // For each time step
   for (i in 1:nind){ // And each individual
    for (j in 1:nrec){ // And each receiver
    // Note observations (y) must be specified as an integer - otherwise will result in an error
    y[i,j,t] ~ binomial(ntrans, p0*exp(-alpha1*d[i,j,t]*d[i,j,t])); 
    }
   }
  }
}  //end of model



