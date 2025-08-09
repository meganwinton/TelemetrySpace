// Declare data
data {
  int<lower = 0> n_ind;               // number of individuals
  int<lower = 0> n_rec;               // number of receivers
  int<lower = 0> n_time;              // number of time steps
  int<lower = 0> n_trans;             // number of trials/expected number of transmissions per time step
  array[n_ind, n_rec, n_time] int<lower = 0> det; // number of detections for each individual at each receiver in each time step
  array[n_rec] real rec_x;                // receiver locations in east-west direction
  array[n_rec] real rec_y;              // receiver locations in north-south direction
  array[2] real lim_x;                    // area bounds east-west
  array[2] real lim_y;                    // area boundes north-south
}

// Declare parameters
parameters {
  // fixed effects
  real<lower = -7, upper = 7> alpha0;  // detection probability intercept on the logit scale - bounds are to ensure only searching reasonable parameter space
  real<lower = 0> alpha1;  // coef. for decline in detection probability with distance

  // latent variables
  array[n_ind, n_time] real<lower = lim_x[1], upper = lim_x[2]> x;  // E-W center of activity coordinate - bounds reflect spatial extent
  array[n_ind, n_time] real<lower = lim_y[1], upper = lim_y[2]> y;  // N-S center of activity coordinate - bounds reflect spatial extent
}

// Declare derived/transformed parameters
transformed parameters  {
  real p0;         // Detection probability at a distance of 0
  real sigma;      // Standard deviation of the distance-decay function
  array[n_ind, n_rec, n_time] real dist;  // Array to store distances distances

  p0 = inv_logit(alpha0); // Inverse logit of alpha0 - constrains probability b/tw 0 and 1

  sigma = sqrt(1 / (2 * alpha1)); // Derived from coefficient specifying
  // distance-related decay in detection prob. - this is 1 / 2 * sigma^2 = a1
  // solved to equal sigma - this then is used in the full model

  for (t in 1:n_time){ // For each time step
    for (i in 1:n_ind) { // And each individual
      for (j in 1:n_rec){ // And each receiver
        // Calculate the Euclidean distance from each COA to each receiver in each time step
        dist[i, j, t] = sqrt(
          square(x[i, t] - rec_x[j]) +
            square(y[i, t] - rec_y[j])
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
  for (t in 1:n_time){ // For each time step
    for (i in 1:n_ind){ // And each individual
      for (j in 1:n_rec){ // And each receiver
        // Note observations (y) must be specified as an integer - otherwise will result in an error
        det[i, j, t] ~ binomial(n_trans, p0 * exp(-alpha1 * square(dist[i, j, t])));
      }
    }
  }

}  //end of model

 // generate quantities
generated quantities {
  array[n_ind, n_rec, n_time] int y_rep; // replicated data, same dimensions as observed detections

  for (t in 1:n_time) {
    for (i in 1:n_ind) {
      for (j in 1:n_rec) {
        real p = p0 * exp(-alpha1 * square(dist[i, j, t]));
        y_rep[i, j, t] = binomial_rng(n_trans, p);
      }
    }
  }
}



