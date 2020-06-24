#CDF for debut sugo pulls
debut.cdf <- c(2.748, 9.367, 14.715, 20.519, 25.209, 31.929, 33.800, 35.619, 39.418, 44.862, 46.377, 51.195, 52.537, 53.841, 57.989, 59.143, 60.266, 63.837, 64.830, 68.657, 69.518, 72.257, 73.020, 73.761, 76.616, 77.259, 77.884, 78.491, 79.083, 100.000)
anni.cdf <- c(4.777,	8.921,	17.197,	24.720,	37.896,	43.688,	46.378,	48.939,	54.718,	56.881,	58.941,	62.672,	64.455,	66.153,	72.077,	73.411,	74.681,	76.982,	78.081,	100.000)

#Amount of gems saved up before initiating plan to pull every quarter 
initial.savings <- c(1500,1800,2100,2400,2700,3000,3300,3600,3900,4200)
#Set up the initial savings for the Monte Carlo
savings <- array(data = 0, dim = c(10,100000))
for(i in 1:10)
{
  for(j in 1:100000)
  {
    savings[i,j] <- initial.savings[i]
  }
}
ndebut <- array(data = 0, dim = c(10,100000))
totalpulls <- array(data = 0, dim = c(10,100000))
nmonths <- array(data = 0, dim = c(10,100000))

#Amount of gems saved every month. This amount is an estimate. You can modify this to suit your own purposes. 
#For now, 300 per month simply means you save 300 per month towards debut banners. Any excess will be treated 
#as spent on other banners or refills/box space expansions.
monthly.savings <- 300
#You gain more gems during Anni - this is just an estimate. You can modify this to change the model.
anni.extrasavings <- 600
#The number of debut Legends you plan on chasing during Anni
anni.ndebut <- 2

#Cutoff - if the simulation has run for more than this many months, it will be considered in perpetuity
#10 years
months.cutoff <- 120


#Test out each of the initial savings
for(a in 1:10)
{
  #Monte Carlo simulation of 100,000 trials
  for(b in 1:100000)
  {
    month <- 0
    #Continue until all gems are exhausted
    while((savings[a,b] > 0) & (month < months.cutoff))
    {
      #Only gain income if not the first time pulling
      if(month != 0)
      {
        #Gain monthly income
        savings[a,b] <- savings[a,b] + monthly.savings
        #Every 12 months is Anni - assume we start on Anni
        if(month %% 12 == 0)
        {
          savings[a,b] <- savings[a,b] + anni.extrasavings
        }
      }
      
      #Check Anni Pulls first
      if(month %% 12 == 0)
      {
        for(c in 1:anni.ndebut)
        {
          #Generate random number 
          pull <- runif(1, min=0, max=100)
          #Compare against CDF
          #Number of pulls needed
          npull <- 1
          while(pull > anni.cdf[npull])
          {
            npull <- npull + 1
          }
          
          #Check did you have enough gems
          gems.spent <- 30 + 50 * (npull - 1)
          if(savings[a,b] > gems.spent)
          {
            #If you had enough gems, you pull the unit
            ndebut[a,b] <- ndebut[a,b] + 1
          }
          #Regardless if you pulled the unit, you spent the gems. If this goes below zero, then your cycle ends
          savings[a,b] <- savings[a,b] - gems.spent
          
          totalpulls[a,b] <- totalpulls[a,b] + npull
        }
      }
      #Pull every quarter
      else if(month %% 3 == 0)
      {
        #Generate random number 
        pull <- runif(1, min=0, max=100)
        #Compare against CDF
        #Number of pulls needed
        npull <- 1
        while(pull > debut.cdf[npull])
        {
          npull <- npull + 1
        }
        #Check did you have enough gems
        gems.spent <- 50 * npull
        if(savings[a,b] > gems.spent)
        {
          #If you had enough gems, you pull the unit
          ndebut[a,b] <- ndebut[a,b] + 1
        }
        #Regardless if you pulled the unit, you spent the gems. If this goes below zero, then your cycle ends
        savings[a,b] <- savings[a,b] - gems.spent
        
        totalpulls[a,b] <- totalpulls[a,b] + npull
      }
      
      #move to next month
      month <- month + 1
    }
    
    nmonths[a,b] <- month
    
    #Output to keep track of number of trials
    if(b %% 10000 == 0)
    {
      print((a-1)*10 + b/10000)
    }
  }
}



#Output
for(i in 1:10)
{
  hist(ndebut[i,], main = paste(initial.savings[i], "gems saved"), freq = FALSE, ylim = c(0,0.5))
  #Probability of setting up the perpetuity
  print(paste(initial.savings[i], "gems saved for Anni"))
  print(paste(sum(ndebut[i,] == 50)/100000, "probability of setting up the perpetuity"))
}
