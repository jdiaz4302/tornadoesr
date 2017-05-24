# TornadoesR

### A project by Jeremy Diaz
#### CU Earth Lab. Analytics Hub Intern. Supervisor: Maxwell Joseph

## Introduction

This repository roughly serves as the documentation to my project's workflow. It is actively updating and will likely be reordered in the future for future data source integration.

As of now, the project aims to predict tornado-induced property damage based on variables that you can know before an event, such as location, and, separately, variables that can "messed around with" in scenario planning. The primary data source is the NOAA-provided Storm Events Database. For each event included in the analysis, there was a land cover extraction done with buffer 3,500m (mean tornado length). The land cover values were obtained via the USGS-provided 2011 National Land Cover Database. The proportion of each land cover was the derived for each storm event buffer area.