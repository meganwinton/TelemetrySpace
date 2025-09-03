// Declare data
data {
  int<lower = 0> nind;               // number of individuals
  int<lower = 0> nrec;               // number of receivers
  int<lower = 0> ntime;              // number of time steps
  int<lower = 0> ntrans;             // number of trials/expected number of transmissions per time step
  array[nind, nrec, ntime] int<lower = 0> y; // number of detections for each individual at each receiver in each time step
  array[nrec] real recX;                // receiver locations in east-west direction
  array[nrec] real recY;              // receiver locations in north-south direction
  array[2] real xlim;                    // area bounds east-west
  array[2] real ylim;                    // area boundes north-south
}

// Declare parameters
parameters {
  // fixed effects
  real<lower = -7, upper = 7> alpha0;  // detection probability intercept on the logit scale - bounds are to ensure only searching reasonable parameter space
  real<lower = 0> alpha1;  // coef. for decline in detection probability with distance

  // latent variables
  array[nind, ntime] real<lower = xlim[1], upper = xlim[2]> sx;  // E-W center of activity coordinate - bounds reflect spatial extent
  array[nind, ntime] real<lower = ylim[1], upper = ylim[2]> sy;  // N-S center of activity coordinate - bounds reflect spatial extent
}

// Declare derived/transformed parameters
transformed parameters  {
   real p0;         // Detection probability at a distance of 0
   real sigma;      // Standard deviation of the distance-decay function
   array[nind, nrec, ntime] real d;  // Array to store distances distances

   p0 = inv_logit(alpha0); // Inverse logit of alpha0 - constrains probability b/tw 0 and 1

   sigma = sqrt(1 / (2 * alpha1)); // Derived from coefficient specifying
   // distance-related decay in detection prob. - this is 1 / 2 * sigma^2 = a1
   // solved to equal sigma - this then is used in the full model

   for (t in 1:ntime){ // For each time step
    for (i in 1:nind) { // And each individual
      for (j in 1:nrec){ // And each receiver
      // Calculate the Euclidean distance from each COA to each receiver in each time step
      d[i, j, t] = sqrt(
        square(sx[i, t] - recX[j]) +
        square(sy[i, t] - recY[j])
        );
    }
   }
  }
}

model {
  // priors
  alpha0 ~ cauchy(0, 2.5);
  alpha1 ~ cauchy(0, 2.5);

  // likelihood
  for (t in 1:ntime){ // For each time step
   for (i in 1:nind){ // And each individual
    for (j in 1:nrec){ // And each receiver
    // Note observations (y) must be specified as an integer - otherwise will result in an error
    y[i, j, t] ~ binomial(ntrans, p0 * exp(-alpha1 * square(d[i, j, t])));
    }
   }
  }
}  //end of model


