The files contained in this directory allow reproducing the main results corresponding to the second case study (Greater London Area) described in the paper "A Bayesian shared-effects modeling framework to quantify the modifiable areal unit problem". They could be easily adapted to test the methodology with a different dataset.

The subdirectory Data contains the data (response variable and covariates) considered for the analysis ("data_london.rda"). This dataset was constructed from public data available in the London Datastore (https://data.london.gov.uk/).

The subdirectory Data also contains the SpatialPolygonDataFrame objects corresponding to the Districts and MSOAs considered for the case study.

The following lines provide a description of the R scripts included (the three ones that are numbered should be run, in the specified order):

- "1 Case study London shared-effects models.R": allows fitting the shared-effects model for each of the covariates considered for the analysis.
- "2 Case study London global MAUP.R": allows computing global MAUP effects. 
- "3 Case study London local MAUP.R": allows computing local MAUP effects. 
- "code_1_covariate_London.R": contains the NIMBLE code to run the shared-effects model proposed in the paper to quantify MAUP effects globally and locally.
- "delta_case_deletion_London.R": allows computing the p_j values proposed in the paper. 
- "data_build_nimble.R" and "Generate zones.R": contain some auxiliary functions.
