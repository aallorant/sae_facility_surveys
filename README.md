# A small area model to assess temporal trends and sub-national disparities in healthcare quality using facility surveys

This repo contains cleaned code used to generate estimates of readiness and process quality of care in health facilities, as reported in 'A small area model to assess temporal trends and sub-national disparities in healthcare quality using facility surveys'. We present an application of this code to "processed data", obtained from formatting raw SPA/SDI survey data using formatting code that can be found on the survey providers' Github: 
(i) https://github.com/DHSProgram/DHS-Analysis-Code/tree/main/EffectiveCoverage; (ii) https://github.com/worldbank/SDI-Health. The spatial analyses require additionally shapefiles; we present an application using Senegal's spatial data. Replicating the analyses to Kenya and Tanzania only requires to use the adequate shapefiles and processed survey data.

This repo contains one directory, which includes the 6 scripts used to 1) gather the simulated processed survey data; 2) fit different models to the two indicators of interest; 3) finding the best model according to goodness of fit; 4) fit the best model; 5) validate the model results using out-of-sample predictions; and 6) calculate mean error, mean absolute error, and model coverage for these out-of-sample predictions.

