#######################################################
### Models to consider for Small Area Estimates for QoC

# -- model 1: iid RE department -- #
# -- model 2: iid RE department + survey RE or sampling for SEN-- #
# -- model 3: iid RE department + covariates -- #
# -- model 4: iid RE department + survey RE or sampling for SEN + covariates-- #
# -- model 5: spatial RE - no interaction -- #
# -- model 6: space-time + survey RE or sampling for SEN -- #
# -- model 7: space-time + survey RE or sampling for SEN + covariates -- #

# -- model 1: iid RE department -- #
model1 <- outcome ~ f(dist.id, model="iid", param=prior.iid) +  # department effect
  f(period.id, model=models$time[i],  param=prior.iid) +  # random walk 
  f(period.id2, model="iid",  param=prior.iid) +  # iid time
  f(period.id3, model=models$time[i], replicate=dist.id, param=prior.besag) 

# -- model 2: iid RE department + survey RE or sampling for SEN-- #
model2 <- outcome ~ f(dist.id, model="iid", param=prior.iid) +  # department effect
  f(period.id, model=models$time[i],  param=prior.iid) +  # random walk 
  f(period.id2, model="iid",  param=prior.iid) +   # iid time
  f(sampling, model="iid", param=prior.iid, extraconstr = list(A = A, e = e))  +
  f(period.id3, model=models$time[i], replicate=dist.id, param=prior.besag) 


# -- model 3: iid RE department + covariates -- #
model3 <- outcome ~ f(dist.id, model="iid", param=prior.iid) +  # department effect
  f(period.id, model=models$time[i],  param=prior.iid) +  # random walk 
  f(period.id2, model="iid",  param=prior.iid)+ 
  scale(hw_density) + scale(gdp) + scale(hdi) + scale(elevation) +
  scale(tt_hcf) + scale(access) + scale(landcover) +
  f(period.id3, model=models$time[i], replicate=dist.id, param=prior.besag) 


# -- model 4: iid RE department + survey RE or sampling for SEN + selected covariates-- #
model4 <-  outcome ~ f(dist.id, model="iid", param=prior.iid) +  # department effect
  f(period.id, model=models$time[i],  param=prior.iid) +  # random walk 
  f(period.id2, model="iid",  param=prior.iid) +   # iid time
  f(sampling, model="iid", param=prior.iid, extraconstr = list(A = A, e = e)) +
  scale(hw_density) + scale(gdp) + scale(hdi) + scale(elevation) +
  scale(tt_hcf) + scale(access) + scale(landcover) +
  f(period.id3, model=models$time[i], replicate=dist.id, param=prior.besag) 


# -- model 5: spatial RE - no interaction -- #
model5 <- outcome ~ f(dist.id, model=models$space[i], param=prior.iid, graph=mat) +
  f(dist.id2, model="iid", param=prior.iid) + # department effect
  f(period.id, model=models$time[i],  param=prior.iid) +  # random walk
  f(period.id2, model="iid",  param=prior.iid) +
  f(period.id3, model=models$time[i], replicate=dist.id, param=prior.besag)


# -- model 6: space-time + survey RE or sampling for SEN -- #
model6 <- outcome ~ f(dist.id, model=models$space[i], param=prior.iid, graph=mat) +
  f(dist.id2, model="iid", param=prior.iid) +# department effect
  f(period.id, model=models$time[i],  param=prior.iid) +  # random walk
  f(period.id2, model="iid",  param=prior.iid) +  # iid time
  f(period.id3, model=models$time[i], replicate=dist.id, param=prior.besag) +  # department X time
  f(sampling, model="iid", param=prior.iid, extraconstr = list(A = A, e = e)) 

# -- model 7: space-time + survey RE or sampling for SEN + selected covariates -- #
model7 <- outcome ~ f(dist.id, model=models$space[i], param=prior.iid, graph=mat) +  
  f(dist.id2, model="iid", param=prior.iid) + # department effect
  f(period.id, model=models$time[i],  param=prior.iid) +  # random walk
  f(period.id2, model="iid",  param=prior.iid) +  # iid time
  f(period.id3, model=models$time[i], replicate=dist.id, param=prior.besag) +  # department X time
  f(sampling, model="iid", param=prior.iid, extraconstr = list(A = A, e = e))   + 
  scale(hw_density) + scale(gdp) + scale(hdi) + scale(elevation) +
  scale(tt_hcf) + scale(access) + scale(landcover)