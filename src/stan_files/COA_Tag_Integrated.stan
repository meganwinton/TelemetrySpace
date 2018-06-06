// Declare data
data {
  int<lower=0> nind;               // number of individuals
  int<lower=0> nrec;               // number of receivers
  int<lower=0> ntime;              // number of time steps
  int<lower=1> ntest;              // number of test tags
  int<lower=0> ntrans;             // number of trials/expected number of transmissions per time step
  int<lower=0> y[nind,nrec,ntime]; // number of detections for each individual at each receiver in each time step
  int<lower=0> test[ntest,nrec,ntime]; // number of detections from each test tag at each receiver in each time step
  real recX[nrec];                 // trap locations in east-west direction
  real recY[nrec];                 // trap locations in north-south direction
  real xlim[2];                    // area bounds east-west
  real ylim[2];                    // area boundes north-south
  real testX[ntest];               // test tag locations east-west
  real testY[ntest];               // test tag locations north-south
}

// Declare parameters
parameters {
  // fixed effects
  //real<lower=-5, upper=5> alpha0;  // detection probability intercept - max of ~1
  real<lower=-5, upper=5> alpha0[ntime,nrec]; // time effect
  real<lower=0> alpha1;  // coef. for decline in detection probability with distance

  // latent variables
  real<lower=xlim[1], upper=xlim[2]> sx[nind,ntime];  // E-W center of activity coordinate - bounds reflect spatial extent
  real<lower=ylim[1], upper=ylim[2]> sy[nind,ntime];  // N-S center of activity coordinate - bounds reflect spatial extent
}

// Declare derived/transformed parameters
transformed parameters  {
  // Declare them
   real p0[ntime,nrec]; // Detection probability at a distance of 0 - time-varying
   real sigma;          // Standard deviation of the distance-decay function - assume constant
   real d[nind,nrec,ntime]; // Array to store distances
   real td[ntest,nrec]; // Matrix of test tag distances

  // Specify them
   sigma = sqrt(1/(2*alpha1)); // Derived from coefficient specifying distance-related decay in detection prob.
  // Test tag distance
  for(s in 1:ntest){ // For each test tag
    for(j in 1:nrec){ // And each receiver
      // Calculate Euclidean distance from east test tag to each receiver
      td[s,j] = pow(pow(testX[s]-recX[j],2) + pow(testY[s]-recY[j],2),0.5); //Calc for euclidean distance
    }
  }

  // COA distance
   for(t in 1:ntime){ // For each time step
    for(j in 1:nrec){ // And each receiver
      for(i in 1:nind) { // And each individual
        // Calculate the Euclidean distance from each COA to each receiver in each time step
          d[i,j,t] = pow(pow(sx[i,t]-recX[j],2) + pow(sy[i,t]-recY[j],2),0.5);
       // Detection probability
          //p0[t,j] = exp( alpha0 + alpha2[t,j] )/( 1+exp( alpha0 + alpha2[t,j] ) );
          p0[t,j] = exp( alpha0[t,j] )/( 1+exp( alpha0[t,j] ) );
     }
   }
 }

}

// Model specification
model {
  // priors
  alpha0[ntime,nrec]~cauchy(0,2.5);
  alpha1~cauchy(0,2.5);

  // likelihood
  for (t in 1:ntime){ // For each time step
   for (j in 1:nrec){ // And each receiver
    for (s in 1:ntest){ // And each test tag
      // Data from test tag - distance for each known
      test[s,j,t] ~ binomial(ntrans, p0[t,j]*exp(-alpha1*td[s,j]*td[s,j]));
    }
     for (i in 1:nind){ // And each individual
        // Note observations (y) must be specified as an integer - otherwise will result in an error
        y[i,j,t] ~ binomial(ntrans, p0[t,j]*exp(-alpha1*d[i,j,t]*d[i,j,t]));
    }
   }
  }
}  //end of model



