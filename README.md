# Predicting property damage from tornadoes with deep learning

**Authors**: Jeremy Diaz and Maxwell Joseph, Ph.D<br>
**Affiliation**: Earth Lab, Cooperative Institute of Research for Environmental Sciences, University of Colorado Boulder<br>
**Preprint**: [arXiv - stat.ML](https://arxiv.org/abs/1807.03456)


## Summary

This repository serves as the workflow-documentation to the paper "Predicting property damage from tornadoes with deep learning
" by Jeremy Diaz and Maxwell Joseph. R scripts, R Markdown files, and Jupyter notebooks are spread among 2 main folders: "Complete Workflow" and "Old Code". Within "Old Code", "Python" and "R" contain code related to their respective programming languages. Both of these files contain scripts and notebooks that are no longer relevent to the project, however might prove useful to somebody somewhere some day. "Python" primarily consists of the code to produce and evaluate models, while "R" primarily consists of the code for data retrieval, integration, and processing. "Complete Workflow" serves as an **ordered, complete workflow to allow maximum reproducibility and clarity**.

This project aims to predict dollar-amount, tornado-induced property damage based on variables that you can know before a tornado event, variables that describe the tornado event, and both of those sets of variables combined. The predictive models consist of various multivariable regressions and artificial neural networks made in PyTorch, a deep learning framework.

[Click here for some interactive maps produced using the best model](https://rawgit.com/jdiaz4302/tornadoesr/master/interactive_model_maps.html)
