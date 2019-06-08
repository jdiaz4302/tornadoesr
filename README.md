# Predicting property damage from tornadoes with zero-inflated neural networks

**Authors**: Jeremy Diaz and Maxwell Joseph, Ph.D<br>
**Affiliation**: Earth Lab, Cooperative Institute of Research for Environmental Sciences, University of Colorado Boulder<br>
**Preprint**: [arXiv - stat.ML](https://arxiv.org/abs/1807.03456)<br>
**Research data**: [figshare](https://figshare.com/articles/Zipped_data/6792206) (this workflow pulls all the data, however Storm Events updates periodically) 

## Summary of the Project

This project aims to predict dollar-amount, tornado-induced property damage using the following types of variables:

* Tornado-describing
* Land cover of area struck
* Census estimates of the area struck (socioeconomic and demographic)

Multivariable linear regression, zero-inflated log-normal, and various neural networks (some mimicking a zero-inflated log-normal) are all models utilized in this project, which uses a combination of python, R, and bash/shell.

## Summary of the Repository

This repository serves as the workflow-documentation to the paper "Predicting property damage from tornadoes with zero-inflated neural networks" by Jeremy Diaz and Maxwell Joseph. R scripts, R Markdown files, and Jupyter notebooks are spread among 3 main directories:

* Revisions
* Complete Workflow
* Old Code

**Revisions** contains two subdirectories (1) Explorations and (2) Complete Workflow. **Revisions/Explorations** contains prototype code from the revision stage of our paper, while **Revisions/Complete_Workflow** contains the final form of this project, which varies drastically from **Complete_Workflow**, the first draft form of this project. Lastly, **Old Code** contains early prototype code from the first draft.

Both **Complete_Workflow** directories are organized such that the numbered files should be ran in order to fully reproduce the entire project (data gathering, data integration, preprocessing, model fitting, and evaluation).

## Interactive Dashboard

[Click here for some interactive maps produced using the best model](https://rawgit.com/jdiaz4302/tornadoesr/master/interactive_model_maps.html)
