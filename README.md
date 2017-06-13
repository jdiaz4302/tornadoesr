# TornadoesR

### A project by Jeremy Diaz
#### CU Earth Lab. Analytics Hub Intern. Supervisor: Maxwell Joseph

## Introduction

This repository roughly serves as the documentation to my project's workflow. It is actively updating and will likely be reordered in the future for future data source integration.

As of now, the project aims to predict tornado-induced property damage based on variables that you can know before an event, such as location, and, separately, variables that can "messed around with" in scenario planning. The primary data source is the NOAA-provided Storm Events Database. For each event included in the analysis, there was a land cover extraction done with buffer 2,100m (mean tornado length). The land cover values were obtained via the USGS-provided 2011 National Land Cover Database. The proportion of each land cover was then derived for each storm event buffer area.

Variables of interest were then passed over to python, where I sought to develop accurate, predictive neural networks using the early release beta Pytorch deep learning framework.

#### Extremely relevent

![](https://imgs.xkcd.com/comics/here_to_help.png)

## Best Model - June 4th

![](https://raw.githubusercontent.com/jdiaz4302/tornadoesr/master/images/best_model_june_4_2017.png)

## Best Model - June 13th

![](https://raw.githubusercontent.com/jdiaz4302/tornadoesr/master/images/best_model_june_13_2017.png)
