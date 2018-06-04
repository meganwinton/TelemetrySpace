Estimate activity centers from acoustic telemetry data
================
Megan Winton
2018-06-04

Introduction
------------

This vignette will walk you through the analyses presented by Winton et al. (in prep), who describe the use of spatial point process models to estimate individual centers of activity from passive acoustic telemetry data. The vignette progresses from applying the simplest case, which assumes that detection probabilities/receiver detection ranges remain constant over time, to application of a test-tag integrated model, which incorporates detection data from one or more stationary test transmitters to estimate time-varying detection ranges. The models are fitted in a Bayesian framework using the Stan software (Carpenter et al. 2016); code was modified from that provided in Royle et al. (2014) for fitting spatial point process models to data from camera traps. We prefer the Bayesian approach for COA estimation due to its treatment of uncertainty, but realize the longer computational time required may be prohibitive for some applications. In the near future, we will update to include an option to fit in a maximum likelihood framework, which will reduce run times.

Data preparation
----------------

The package includes two data sets: 1) hourly detection data from a tagged black sea bass and 2) hourly detections from a stationary test transmitter over the same time period. Two files containing the receiver coordinates and the location of the test tag are also included. We have provided the data in a raw format to illustrate data preparation as well as model fitting. To access the provided data, run:

``` r
library(TelemetrySpace)
```

    ## Warning: package 'Rcpp' was built under R version 3.4.4

``` r
rlocs # Receiver locations
```

    ## # A tibble: 30 x 3
    ##    Station   east  north
    ##    <chr>    <dbl>  <dbl>
    ##  1 SB1     -1.33  1.61  
    ##  2 SB2     -0.579 1.91  
    ##  3 SB3      0.219 2.00  
    ##  4 SB4      1.00  1.81  
    ##  5 SB5     -1.69  0.896 
    ##  6 SB6     -0.879 0.950 
    ##  7 SB7     -0.231 1.20  
    ##  8 SB8      0.464 1.21  
    ##  9 SB9      1.28  1.04  
    ## 10 SB10    -1.77  0.0952
    ## # ... with 20 more rows

``` r
testloc # Test tag location
```

    ## # A tibble: 1 x 2
    ##     east north
    ##    <dbl> <dbl>
    ## 1 -0.329 0.713

``` r
head(testdat) # Hourly detection data from the test tag
```

    ## # A tibble: 6 x 5
    ##   Station Transmitter   east north  hour
    ##   <chr>         <dbl>  <dbl> <dbl> <dbl>
    ## 1 SB12          3447. -0.326 0.513    1.
    ## 2 SB13          3447.  0.125 0.575    1.
    ## 3 SB7           3447. -0.231 1.20     1.
    ## 4 SB6           3447. -0.879 0.950    1.
    ## 5 SB13          3447.  0.125 0.575    1.
    ## 6 SB6           3447. -0.879 0.950    1.

``` r
head(fishdat) # Hourly detection data from a black sea bass
```

    ## # A tibble: 6 x 5
    ##   Station Transmitter   east north  hour
    ##   <chr>         <dbl>  <dbl> <dbl> <dbl>
    ## 1 SB15          3425.  0.190 0.209    1.
    ## 2 SB14          3425. -0.261 0.159    1.
    ## 3 SB15          3425.  0.190 0.209    1.
    ## 4 SB14          3425. -0.261 0.159    1.
    ## 5 SB12          3425. -0.326 0.513    1.
    ## 6 SB14          3425. -0.261 0.159    1.

Prior to doing anything else, we need to specify our 'state space' - the spatial extent of interest. This needs to be large enough to allow for individuals that may have activity centers outside of the receiver array.

``` r
# Define state-space of point process - 'buffer' adds a fixed buffer to the outer extent of the recs
buffer=1
xlim=c(min(rlocs$east-buffer),max(rlocs$east+buffer))
ylim=c(min(rlocs$north-buffer),max(rlocs$north+buffer))
```

Next, calculate the distance between the test tag's location and each receiver. This can be done using the included 'distf' function.

``` r
# Set up a blank vector for storage
D<-NULL
# Loop over each hour
for( i in 1:nrow(testdat) ){
  D[i]=distf( testloc[,c('east','north')], testdat[i,c('east','north')] )
}

testdat$D <- D # Assign to testdat data frame

# Plot to examine variation in detection rate over time
m1 <- plyr::count(testdat, c('Station','hour','D','east','north'))
#Round distance for plotting
m1$dist=round(m1$D*1000)
#Create label by merging station name and distance
m1$label=paste(m1$dist,"m","(",m1$Station,")")

#Plot
library(ggplot2)
ggplot(data=m1,aes(x=hour,y=freq))+ 
  geom_bar(stat="identity",fill='#008080')+
  facet_wrap(~as.factor(label))+
  theme(text = element_text(size = 16)) +
  scale_y_continuous(breaks=seq(0,30,10)) +
  labs(x="Hours since deployment",y="Number of detections")
```

![](C:\Users\mwinton\AppData\Local\Temp\Rtmpwb63EK\preview-28e4753a22eb.dir\Estimate_COA_vignette_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-3-1.png)

Tally how many time intervals each receiver was operational for - This allows for individual receivers deployed for different periods and/or receivers that were lost.

``` r
# The full analysis takes several hours to run, so we'll subset out 10 hours to illustrate
fishdat <- fishdat[fishdat$hour < 11,]
testdat <- testdat[testdat$hour < 11,]

# Create a copy of the receiver locations for tallying
rs <- rlocs
# Add column for each time interval to indicate whether receiver was operational or not
rs[,c(4:( max(fishdat$hour)+3 ) )] <- 1
  rs <- plyr::rename( rs, replace = c("V4"=1, 
                                      "V5"=2, 
                                      "V6"=3, 
                                      "V7"=4, 
                                      "V8"=5, 
                                      "V9"=6, 
                                      "V10"=7, 
                                      "V11"=8, 
                                      "V12"=9, 
                                      "V13"=10) )

# Create vector of the number of sampling occasions for each receiver 
tsteps <- apply(rs[,4:ncol(rs)],1,sum)
tsteps # Will all be the same here because all operational over the entire time span
```

    ##  [1] 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10
    ## [24] 10 10 10 10 10 10 10

Since detection records only include non-zero events, we'll need to convert the number of detections to include zeros for all transmitters

``` r
## Starting with the test tag data
testdat$rec <- as.numeric(substr(testdat$Station, 3, 4))
testdat$count <- 1 # Add a column that indicates each record corresponds to 1 detection

# Aggregate the number of detections for each individual at each receiver in each time step
test.agg <- aggregate(count ~ Transmitter + rec + east + north + hour, testdat, sum) 
# Create a numeric identifier for each transmitter
test.agg$tag <- as.numeric(as.factor(test.agg$Transmitter))
# Rename hour to time for consistency when plotting below
test.agg$time <- test.agg$hour

# Specify quantities for indexing
ntest <- length( unique(test.agg$Transmitter) ) # number of individual tags (here just the one test tag)
nrec <- nrow(rlocs) # number of receivers
tsteps <- max(test.agg$hour)

# This chunk saves the total number of encounters of each individual (rows) in each trap (cols) at each sampling occasion (array elements)
testY=array( NA, dim=c( ntest, nrec, tsteps ) )
for( t in 1:max(tsteps) ){
  for ( i in 1:nrow(testY) ){
    h1 <- test.agg[test.agg$tag==i,]
      for( j in 1:nrow(rlocs) ){
    # If there are no detections at that receiver in that time period, set to 0; otherwise set to the number of detections
    testY[i,j,t]=ifelse(identical(h1[h1$hour==t & h1$rec==j,6], numeric(0)),0, h1[h1$hour==t & h1$rec==j,6])
    }
  }  
}

## Now do the same for each tagged fish
fishdat$rec <- as.numeric(substr(fishdat$Station, 3, 4))
fishdat$count <- 1 # Add a column that indicates each record corresponds to 1 detection

# Aggregate the number of detections for each individual at each receiver in each time step
fish.agg <- aggregate(count ~ Transmitter + rec + east + north + hour, fishdat, sum) 
# Create a numeric identifier for each transmitter
fish.agg$tag <- as.numeric(as.factor(fish.agg$Transmitter))
# Rename hour to time for consistency when plotting below
fish.agg$time <- fish.agg$hour

# Specify quantities for indexing
nind <- length( unique(fish.agg$Transmitter) ) # number of individual tags (here just the one test tag)

# This chunk saves the total number of encounters of each individual (rows) in each trap (cols) at each sampling occasion (array elements)
Y=array( NA, dim=c( nind, nrec, tsteps ) )
for( t in 1:max(tsteps) ){
  for ( i in 1:nrow(Y) ){
    h1 <- fish.agg[fish.agg$tag==i,]
      for( j in 1:nrow(rlocs) ){
    # If there are no detections at that receiver in that time period, set to 0; otherwise set to the number of detections
    Y[i,j,t]=ifelse(identical(h1[h1$hour==t & h1$rec==j,6], numeric(0)),0, h1[h1$hour==t & h1$rec==j,6])
    }
  }  
}
```

Fit the standard COA model
--------------------------

After all of that data processing, we're finally ready to fit our model! We'll start by fitting the model assuming that detection probabilities are the same among all receivers and remain constant over time (This will pretty much never be the case, but best to start simple!). The only other information you'll need to provide is the expected number of transmissions per tag per time interval (ntrans below).

``` r
### Format data for model fitting
fit <- COA_Standard(nind=nind, # number of individuals
                    nrec=nrec, # number of receivers
                    ntime=tsteps, # number of time steps
                    ntrans=30, # number of expected transmissions per tag per time interval
                    y=Y,   # array of detections 
                    recX=as.vector(rlocs$east),  # E-W receiver coordinates
                    recY=as.vector(rlocs$north), # N-S receiver coordinates 
                    xlim=xlim, # E-W boundary of spatial extent (receiver array + buffer)
                    ylim=ylim) # N-S boundary of spatial extent (receiver array + buffer)
```

    ##         warmup sample
    ## chain:1  8.865  9.387
    ## chain:2  8.975  9.597
    ## chain:3  8.957  9.414
    ## chain:4  9.741  9.182

The function returns a list with four objects:

``` r
summary(fit)
```

    ##               Length Class      Mode   
    ## Model           1    stanfit    S4     
    ## Summary        40    -none-     numeric
    ## Time            1    -none-     numeric
    ## COAs           70    -none-     numeric
    ## All_estimates 325    data.frame list

The first contains the Stan model object (accessible via fit$Model, which will allow you to use rstan plotting tools and diagnostic plots - see rstan documentation for details).

The second element is a table of parameter estimates and associated quantiles from the posterior distribution. The table also includes the effective sample size and the Rhat statistic (which should be ~1).

``` r
fit$Summary
```

    ##              mean      se_mean         sd       2.5%        25%        50%
    ## alpha0 -0.9408465 0.0009000313 0.12728365 -1.1848046 -1.0268292 -0.9425841
    ## p0      0.2814411 0.0001819721 0.02573474  0.2341894  0.2636993  0.2803787
    ## alpha1  6.2048976 0.0045911701 0.64928950  5.0550901  5.7525478  6.1585229
    ## sigma   0.2850200 0.0001043979 0.01476410  0.2567429  0.2751478  0.2849356
    ##               75%      97.5% n_eff     Rhat
    ## alpha0 -0.8573739 -0.6833066 20000 1.000014
    ## p0      0.2978883  0.3355237 20000 1.000020
    ## alpha1  6.6044696  7.5853048 20000 1.000064
    ## sigma   0.2948186  0.3144999 20000 1.000052

The third returns the time required to run the model (in minutes). Note that Stan will automatically detect and use multiple cores. If the computer used to run this has multiple cores, the time returned will be longer than the actual run time (because it will sum the time for each core). To return the realzied run time, divide fit$Time by the number of cores.

``` r
fit$Time
```

    ## [1] 1.2353

The fourth returns an array of COA estimates, where each matrix corresponds to one individual, the rows correspond to each time step, and the columns include the median posterior estimate of the east-west (x) and north-south (y) coordinates. The 95% credible interval (Bayesian version of a confidence interval) for each coordinate is also provided.

``` r
fit$COAs
```

    ## , , 1
    ## 
    ##       time           x         y    x.lower     x.upper    y.lower
    ##  [1,]    1 -0.04512351 0.2927282 -0.1474893  0.05721326 0.18558073
    ##  [2,]    2 -0.02325640 0.3596270 -0.1441021  0.10027187 0.22107395
    ##  [3,]    3 -0.09774278 0.3841469 -0.2554873  0.05230036 0.19716181
    ##  [4,]    4 -0.12425685 0.2829879 -0.2841948  0.02322845 0.11174389
    ##  [5,]    5 -0.16934240 0.3246739 -0.3307900 -0.01907874 0.14822304
    ##  [6,]    6 -0.12813488 0.7013537 -0.4783009  0.18796251 0.10907107
    ##  [7,]    7 -0.04975723 0.3664010 -0.1685698  0.06758352 0.23455726
    ##  [8,]    8 -0.15514994 0.4036304 -0.2823169 -0.03441384 0.26815159
    ##  [9,]    9 -0.04920945 0.3339469 -0.2185425  0.11726874 0.12826448
    ## [10,]   10 -0.10185911 0.3128733 -0.2984448  0.08072913 0.08576492
    ##         y.upper
    ##  [1,] 0.4034239
    ##  [2,] 0.4969187
    ##  [3,] 0.5654994
    ##  [4,] 0.4701498
    ##  [5,] 0.5097848
    ##  [6,] 0.8888172
    ##  [7,] 0.4975924
    ##  [8,] 0.5393795
    ##  [9,] 0.5514910
    ## [10,] 0.5704533

The last element (fit$All\_estimates) contains the estimates from each non-warm-up iteration for all 4 chains. (If you're not familiar with Bayesian lingo and this just seems like statistical jargon, all you need to know is that this is what you'll use to plot the uncertainty cloud around COA estimates and/or the distribution of parameter estimates as presented in the paper.) This includes estimates of all latent parameters for each individual in each time step, so we will need to subset out parameters for plotting.

``` r
# Extract east-west and north-south coordinates of COAs from each iteration of each chain
# Note that 'sx' and 'sy' are the variable names used to store the coordinates for each individual (rows) in each time step (columns).
# We'll store them as a list, where each element corresponds to an individual
ew <- list(NA)
ns <- list(NA)
EN <- list(NA)

for (i in 1:nind){
  ew[[i]] <- dplyr::select(fit$All_estimates, dplyr::starts_with( paste("sx[",i,",", sep='') ) )
  ns[[i]] <- dplyr::select(fit$All_estimates, dplyr::starts_with( paste("sy[",i,",", sep='') ) )

  # Merge them into one dataframe
  EN[[i]]=cbind(data.table::melt(ew[[i]]),data.table::melt(ns[[i]])[,2])
  EN[[i]]$id=as.numeric(EN[[i]]$variable) # The ID field corresponds to the time step.
  colnames(EN[[i]])=c('variable','x','y','time')
}
# You will get a warning about 'No id variables' - don't worry about it.

## Plot posterior estimates
# Select individual for plotting
post <- EN[[1]]
coa <- as.data.frame(fit$COAs[,,1])
# Plot in ggplot
plotCOAs <- ggplot(aes(x=x,y=y),data=post) +
            geom_hex(alpha=1,bins=100)  +
            geom_point(aes(x=x,y=y),data=coa,pch=25,cex=1.5,alpha=.8,fill=NA) +
            facet_wrap(~time,ncol=2) +
            scale_fill_gradientn(colours=c("white","blue"),name = "Frequency",na.value=NA) +
            geom_point(aes(x=east,y=north),data=rlocs,cex=1) +
            geom_point(aes(x=east,y=north),data=fish.agg,pch=21,cex=1.5,fill='#00BFC4') +
       # Unhash line below to include test tag location  
            #geom_point(aes(x=east,y=north),data=testloc,cex=1.5,pch=21,fill='#F8766D') +
            coord_fixed(xlim=c(-1,1),ylim=c(-0.5,1.5)) +
            scale_x_continuous(breaks = c(-1, 0, 1)) +
            #scale_y_continuous(breaks = c(-1, 0, 1)) +
            theme(legend.position="none") +
            labs(x='East-West (km)',y='North-South (km)')
```

Fit a COA model that allows for receiver- and time-specific detection probabilities
-----------------------------------------------------------------------------------

Now let's fit a model that allows for variation in detection probabilites. This model is a simple extension of the previous model - it just adds a component that specifies the distance of each test tag from each receiver, and shifts the probability of detection by comparing the number of detections logged at each receiver versus those emitted from the test tag. The only other information you'll need to provide is the expected number of transmissions per tag per time interval.

``` r
### Format data for model fitting
fit_vary <- COA_TimeVarying(nind=nind, # number of individuals
                            nrec=nrec, # number of receivers
                            ntime=tsteps, # number of time steps
                            ntrans=30, # number of expected transmissions per tag per time interval
                            y=Y,   # array of detections from tagged fish
                            recX=as.vector(rlocs$east),  # E-W receiver coordinates
                            recY=as.vector(rlocs$north), # N-S receiver coordinates 
                            xlim=xlim, # E-W boundary of spatial extent (receiver array + buffer)
                            ylim=ylim) # N-S boundary of spatial extent (receiver array + buffer)
```

    ##          warmup sample
    ## chain:1 109.169 88.186
    ## chain:2 111.263 87.761
    ## chain:3 108.349 87.942
    ## chain:4 109.240 87.808

``` r
summary(fit_vary)
```

    ##                Length Class      Mode   
    ## Model             1   stanfit    S4     
    ## Summary        6020   -none-     numeric
    ## Time              1   -none-     numeric
    ## COAs             70   -none-     numeric
    ## DetectionProbs  300   -none-     numeric
    ## All_estimates   923   data.frame list

``` r
fit_vary$Time
```

    ## [1] 13.16197

``` r
# As before, extract east-west and north-south coordinates of COAs from each iteration of each chain
# Note that 'sx' and 'sy' are the variable names used to store the coordinates for each individual (rows) in each time step (columns).
# We'll store them as a list, where each element corresponds to an individual
ew <- list(NA)
ns <- list(NA)
EN <- list(NA)

for (i in 1:nind){
  ew[[i]] <- dplyr::select(fit_vary$All_estimates, dplyr::starts_with( paste("sx[",i,",", sep='') ) )
  ns[[i]] <- dplyr::select(fit_vary$All_estimates, dplyr::starts_with( paste("sy[",i,",", sep='') ) )

  # Merge them into one dataframe
  EN[[i]]=cbind(data.table::melt(ew[[i]]),data.table::melt(ns[[i]])[,2])
  EN[[i]]$id=as.numeric(EN[[i]]$variable) # The ID field corresponds to the time step.
  colnames(EN[[i]])=c('variable','x','y','time')
}
# You will get a warning about 'No id variables' - don't worry about it.

## Plot posterior estimates
# Select individual for plotting
post <- EN[[1]]
coa <- as.data.frame(fit_vary$COAs[,,1])
# Plot in ggplot
plotTVary <- ggplot(aes(x=x,y=y),data=post) +
         geom_hex(alpha=1,bins=100)  +
         geom_point(aes(x=x,y=y),data=coa,pch=25,cex=1.5,alpha=.8,fill=NA) +
         facet_wrap(~time,ncol=2) +
         scale_fill_gradientn(colours=c("white","blue"),name = "Frequency",na.value=NA) +
         geom_point(aes(x=east,y=north),data=rlocs,cex=1) +
         geom_point(aes(x=east,y=north),data=fish.agg,pch=21,cex=1.5,fill='#00BFC4') +
    # Unhash line below to include test tag location  
         #geom_point(aes(x=east,y=north),data=testloc,cex=1.5,pch=21,fill='#F8766D') +
         coord_fixed(xlim=c(-1,1),ylim=c(-0.5,1.5)) +
         scale_x_continuous(breaks = c(-1, 0, 1)) +
         #scale_y_continuous(breaks = c(-1, 0, 1)) +
         theme(legend.position="none") +
         labs(x='East-West (km)',y='North-South (km)')
```

Fit a model that uses detections from a moored test tag to inform detection probabilites
----------------------------------------------------------------------------------------

Now we'll fit the model we're really interested in - the model that integrates test tag data to inform detection probabilites. This model is a simple extension of the previous model - it just adds a component that specifies the distance of each test tag from each receiver, and shifts the probability of detection by comparing the number of detections logged at each receiver versus those emitted from the test tag. The only other information you'll need to provide is the expected number of transmissions per tag per time interval.

``` r
### Format data for model fitting
## Specify the number of sentinal tags (this step is necessary because of issues that arise with Stan indexing if you have only 1 test tag)
nsentinal <- 1

fit_tag <- COA_TagInt(nind=nind, # number of individuals
                      nrec=nrec, # number of receivers
                      ntime=tsteps, # number of time steps
                      ntest=nsentinal, # number of test tags
                      ntrans=30, # number of expected transmissions per tag per time interval
                      y=Y,   # array of detections from tagged fish
                      test=testY, # array of detections from test tags
                      recX=as.vector(rlocs$east),  # E-W receiver coordinates
                      recY=as.vector(rlocs$north), # N-S receiver coordinates 
                      xlim=xlim, # E-W boundary of spatial extent (receiver array + buffer)
                      ylim=ylim, # N-S boundary of spatial extent (receiver array + buffer)
                      testX=array(testloc$east,dim=c(nsentinal)),
                      testY=array(testloc$north,dim=c(nsentinal)))
```

    ##         warmup sample
    ## chain:1 68.685 56.134
    ## chain:2 72.616 56.193
    ## chain:3 68.210 55.917
    ## chain:4 79.440 55.076

``` r
summary(fit_tag)
```

    ##                Length Class      Mode   
    ## Model             1   stanfit    S4     
    ## Summary        6020   -none-     numeric
    ## Time              1   -none-     numeric
    ## COAs             70   -none-     numeric
    ## DetectionProbs  300   -none-     numeric
    ## All_estimates   953   data.frame list

``` r
fit_tag$Time # See the comment about run time when your computer has multiple cores above.
```

    ## [1] 8.53785

``` r
# As before, extract east-west and north-south coordinates of COAs from each iteration of each chain
# Note that 'sx' and 'sy' are the variable names used to store the coordinates for each individual (rows) in each time step (columns).
# We'll store them as a list, where each element corresponds to an individual
ew <- list(NA)
ns <- list(NA)
EN <- list(NA)

for (i in 1:nind){
  ew[[i]] <- dplyr::select(fit_tag$All_estimates, dplyr::starts_with( paste("sx[",i,",", sep='') ) )
  ns[[i]] <- dplyr::select(fit_tag$All_estimates, dplyr::starts_with( paste("sy[",i,",", sep='') ) )

  # Merge them into one dataframe
  EN[[i]]=cbind(data.table::melt(ew[[i]]),data.table::melt(ns[[i]])[,2])
  EN[[i]]$id=as.numeric(EN[[i]]$variable) # The ID field corresponds to the time step.
  colnames(EN[[i]])=c('variable','x','y','time')
}
# You will get a warning about 'No id variables' - don't worry about it.

## Plot posterior estimates
# Select individual for plotting
post <- EN[[1]]
coa <- as.data.frame(fit_tag$COAs[,,1])
# Plot in ggplot
plotTagInt <- ggplot(aes(x=x,y=y),data=post) +
         geom_hex(alpha=1,bins=100)  +
         geom_point(aes(x=x,y=y),data=coa,pch=25,cex=1.5,alpha=.8,fill=NA) +
         facet_wrap(~time,ncol=2) +
         scale_fill_gradientn(colours=c("white","blue"),name = "Frequency",na.value=NA) +
         geom_point(aes(x=east,y=north),data=rlocs,cex=1) +
         geom_point(aes(x=east,y=north),data=fish.agg,pch=21,cex=1.5,fill='#00BFC4') +
    # Unhash line below to plot test tag location
         #geom_point(aes(x=east,y=north),data=testloc,cex=1.5,pch=21,fill='#F8766D') +
         coord_fixed(xlim=c(-1,1),ylim=c(-0.5,1.5)) +
         scale_x_continuous(breaks = c(-1, 0, 1)) +
         #scale_y_continuous(breaks = c(-1, 0, 1)) +
         theme(legend.position="none") +
         labs(x='East-West (km)',y='North-South (km)')
```

Comparison
----------

Let's plot them all up together to see how they compare.

``` r
#Plot
library(ggpubr)
ggarrange(plotCOAs, plotTVary, plotTagInt, 
          labels = c("Standard","Time-varying","Tag-integrated"),
          ncol = 3, nrow = 1,
          align = "hv", 
          #widths = c(8,8,8), heights = c(2,2,2),
          common.legend = F)
```

![](C:\Users\mwinton\AppData\Local\Temp\Rtmpwb63EK\preview-28e4753a22eb.dir\Estimate_COA_vignette_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-14-1.png)
