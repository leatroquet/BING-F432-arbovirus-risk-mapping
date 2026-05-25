# BING-F432-arbovirus-risk-mapping

This repository contains the R script and output figures produced for a risk mapping project conducted as part of the course **BING-F432 Spatial and Molecular Epidemiology**.

The analysis is based on a **simulated dataset** describing the occurrence of an emerging mosquito-borne arbovirus in Europe. The objective was to model the ecological niche of the virus and identify environmental factors associated with its potential local circulation.

## Repository content

- `arbovirus_risk_mapping.R`: R script used for data processing, ecological niche modelling and risk mapping.
- `Output_arbovirus_risk_mapping`: Pdf document containing the figures generated from the R script.
- 'Arbovirus_risk_mapping_simulated_dataset_3-2.csv' : simulated input dataset used for the analyses.

## Main analyses

The R script includes:
- mapping of simulated occurrence data
- visualization of environmental raster variables
- pseudo-absence generation
- boosted regression tree (BRT) modelling
- spatial cross-validation
- model performance evaluation
- relative variable importance
- response curves
- mean ecological suitability and uncertainty maps
