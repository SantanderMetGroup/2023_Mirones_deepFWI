# Multi-Site Fire Danger Prediction Using a Spatially Coherent Convolutional Neural Network Model
### Mirones et al., submitted to _Geophys. Res. Lett._, Jun.2024

***

![CNN-MG scheme](https://github.com/SantanderMetGroup/2023_Mirones_deepFWI/blob/devel/CNN-MSMG-scheme.png?raw=true)

Our study analyzes the ability of state-of-the-art CNN-based machine learning methods to model the multivariate spatial structure of the Fire Weather Index (FWI). Authors and corresponding ORCID can be found in the [zenodo.json](.zenodo.json) file.

**2023_Mirones_FWI_ERL.ipynb** is a Jupyter notebook based on the R languaje containing the code necessary to replicate our results. 

**environment.yml** contains the versions of the python and R libraries employed to reproduce the results of the manuscript. A conda environment with the appropriate versions can be created by typing:

```bash
mamba env create -n deep-fwi --file environment.yml
```
