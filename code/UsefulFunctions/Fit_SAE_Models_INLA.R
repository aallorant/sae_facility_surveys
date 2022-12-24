##########################################
####
#### fit the INLA models
print("Fitting Model 1")
mod1 <- inla(model1, 
             family = "gaussian", 
             data =mod_dat, 
             control.predictor=list(compute=TRUE),
             control.compute=list(cpo=TRUE,dic=TRUE,waic=T,config=T),
             control.family=list(hyper=list(prec=list(initial=log(1),fixed=TRUE))),
             scale=prec)

print("Fitting Model 2")
mod2 <- inla(model2, 
             family = "gaussian", 
             data =mod_dat, 
             control.predictor=list(compute=TRUE),
             control.compute=list(cpo=TRUE,dic=TRUE,waic=T,config=T),
             control.family=list(hyper=list(prec=list(initial=log(1),fixed=TRUE))),
             scale=prec)
# 
print("Fitting Model 3")
mod3 <- inla(model3,
             family = "gaussian",
             data =mod_dat,
             control.predictor=list(compute=TRUE),
             control.compute=list(cpo=TRUE,dic=TRUE,waic=T,config=T),
             control.family=list(hyper=list(prec=list(initial=log(1),fixed=TRUE))),
             scale=prec)

print("Fitting Model 4")
mod4 <- inla(model4,
             family = "gaussian",
             data =mod_dat,
             control.predictor=list(compute=TRUE),
             control.compute=list(cpo=TRUE,dic=TRUE,waic=T,config=T),
             control.family=list(hyper=list(prec=list(initial=log(1),fixed=TRUE))),
             scale=prec)

print("Fitting Model 5")
mod5 <- inla(model5,
             family = "gaussian",
             data =mod_dat,
             control.predictor=list(compute=TRUE),
             control.compute=list(cpo=TRUE,dic=TRUE,waic=T,config=T),
             control.family=list(hyper=list(prec=list(initial=log(1),fixed=TRUE))),
             scale=prec)
print("Fitting Model 6")
mod6 <- inla(model6,
             family = "gaussian",
             data =mod_dat,
             control.predictor=list(compute=TRUE),
             control.compute=list(cpo=TRUE,dic=TRUE,waic=T,config=T),
             control.family=list(hyper=list(prec=list(initial=log(1),fixed=TRUE))),
             scale=prec)
print("Fitting Model 7")
mod7 <- inla(model7,
             family = "gaussian",
             data =mod_dat,
             control.predictor=list(compute=TRUE),
             control.compute=list(cpo=TRUE,dic=TRUE,waic=T,config=T),
             control.family=list(hyper=list(prec=list(initial=log(1),fixed=TRUE))),
             scale=prec)
