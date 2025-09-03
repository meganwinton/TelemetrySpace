// Declare data
data {
  int<lower = 0> nind;               // number of individuals
  int<lower = 0> nrec;               // number of receivers
  int<lower = 0> ntime;              // number of time steps
  int<lower = 0> ntrans;             // number of trials/expected number of transmissions per time step
  array[nind, nrec, ntime] int<lower = 0> y; // number of detections for each individual at each receiver in each time step
  array[nrec] real recX;                 // trap locations in east-west direction
  array[nrec] real recY;                 // trap locations in north-south direction
  array[2] real xlim;                    // area bounds east-west
  array[2] real ylim;                    // area boundes north-south
}
// Declare parameters
parameters {
  // fixed effects
  array[ntime, nrec] real<lower = -7, upper = 7> alpha0; // time effect
  real<lower = 0> alpha1;  // coef. for decline in detection probability with distance

  // latent variables
  array[nind, ntime] real<lower = xlim[1], upper = xlim[2]> sx;
  // E-W center of activity coordinate - bounds reflect spatial extent
  array[nind, ntime] real<lower = ylim[1], upper = ylim[2]> sy;
  // N-S center of activity coordinate - bounds reflect spatial extent
}

// Declare derived/transformed parameters
transformed parameters  {
  // Declare them
   array[ntime, nrec] real p0; // Detection probability at a distance of 0 - time-varying
   real sigma;          // Standard deviation of the distance-decay function - assume constant
   array[nind, nrec, ntime] real d; // Array to store distances

  // Specify them
   sigma = sqrt(1 / (2 * alpha1));
   // Derived from coefficient specifying distance-related decay in detection prob.

  // COA distance
   for(t in 1:ntime){ // For each time step
    for(j in 1:nrec){ // And each receiver
      for(i in 1:nind) { // And each individual
        // Calculate the Euclidean distance from each COA to each receiver in each time step
          d[i, j, t] = sqrt(square(sx[i, t] - recX[j]) + square(sy[i, t] - recY[j]));
       // Detection probability
          p0[t, j] = inv_logit(alpha0[t, j]);
     }
   }
 }

}

// Model specification
model {
  // priors
  alpha0[ntime, nrec] ~ cauchy(0, 2.5);
  alpha1 ~ cauchy(0, 2.5);

  // likelihood
  for (t in 1:ntime){ // For each time step
   for (j in 1:nrec){ // And each receiver
     for (i in 1:nind){ // And each individual
        // Note observations (y) must be specified as an integer - otherwise will result in an error
        y[i, j, t] ~ binomial(ntrans, p0[t, j] * exp(-alpha1 * square(d[i, j, t])));
    }
   }
  }
}  //end of model


