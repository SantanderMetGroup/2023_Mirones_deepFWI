# Learning the spatial structure of Fire Weather Index with convolutional neural networks
This repository contains the material and guidelines to reproduce the results presented in the manuscript entitled **Learning the spatial structure of Fire Weather Index with convolutional neural networks**, submitted to the journal *Environmental Research Letters* by *O. Mirones, J. Baño-Medina, J. Bedia*. 
This paper analyzes the ability of state-of-the-art machine learning algorithms, especially convolutional neural networks, to model the spatial structure of the Fire Weather Index (FWI). Authors and corresponding ORCID can be found in the [zenodo.json](.zenodo.json) file.

**2023_Mirones_FWI_ERL.ipynb** is a Jupyter notebook based on R containing the code necessary to replicate the results. 

**environment.yml** contains the versions of the python and R libraries employed to reproduce the results of the manuscript. A conda environment with the appropriate versions can be created by typing:

```bash
mamba env create -n deep-fwi --file environment.yml
```