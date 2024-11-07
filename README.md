# Towards Spatio-temporally Consistent Multi-Site Fire Danger Downscaling with Explainable Deep Learning
### Mirones et al., submitted to _JGR:Machine Learning and Computation_, Nov.2024

***

![ConvLSTM-MG scheme](https://github.com/SantanderMetGroup/2023_Mirones_deepFWI/blob/main/CONVLSTM-MG-scheme.png)

Our study analyzes the ability of state-of-the-art CNN and ConvLSTM-based machine learning methods to model the multivariate spatio-temporal structure of the Fire Weather Index (FWI). Authors and corresponding ORCID can be found in the [zenodo.json](.zenodo.json) file.

**2023_Mirones_FWI_ERL.ipynb** is a Jupyter notebook based on the R languaje containing the code necessary to replicate our main results. 

**environment.yml** contains the versions of the python and R libraries employed to reproduce the results of the manuscript. A conda environment with the appropriate versions can be created by typing:

```bash
mamba env create -n deep-fwi --file environment.yml
```
