# TornadoesR

### A project by Jeremy Diaz
#### CU Earth Lab. Analytics Hub Intern. Supervisor: Maxwell Joseph

## Introduction

This repository roughly serves as the documentation to my project's workflow. It is actively updating and will likely be reordered in the future due to further data source integrations.

As of now, the project aims to predict tornado-induced property damage based on variables that you can know before an event, such as location, and, separately, variables that can "messed around with" in scenario planning. The primary data source is the NOAA-provided Storm Events Database. For each event included in the analysis, there was a land cover extraction done with a buffer radius of 2,100m (mean tornado length). The land cover values were obtained via the USGS-provided 2011 National Land Cover Database. The proportion of each land cover was then derived for each storm event buffer area. Median income per county, and income interaction effects, was available via the Federal Reserve Bank of St. Louis's GeoFRED 2014 Estimate of Median Household Income by County dataset.

Data were largely cleaned, processed, and visualized in R and then passed over to Python, where I sought to develop accurate, predictive neural networks using the early release beta Pytorch deep learning framework.

#### Extremely relevent

![](https://imgs.xkcd.com/comics/here_to_help.png)

## Best Model - June 4th

![](https://raw.githubusercontent.com/jdiaz4302/tornadoesr/master/images/best_model_june_4_2017.png)

Approximate Error Value: 405

## Best Model - June 13th

![](https://raw.githubusercontent.com/jdiaz4302/tornadoesr/master/images/best_model_june_13_2017.png)

Approximate Error Value: 281

## Best Model - June 15th

![](https://raw.githubusercontent.com/jdiaz4302/tornadoesr/master/images/best_model_june_15_2017.png)

Approximate Error Value: 302

## Best Model - June 19th

![](https://raw.githubusercontent.com/jdiaz4302/tornadoesr/master/images/best_model_june_19_2017.png)

Approximate Error Value: 265

#### Model Architecture - June 19th

![](https://raw.githubusercontent.com/jdiaz4302/tornadoesr/master/images/architecture_june_19_2017.png)

