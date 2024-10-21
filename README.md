# HilbertBalance

Use Hilbert basis to balancing the chemical equations

## Installation

- Download the "HilbertBalance-main.zip" file and unzip it. 
- Install Normaliz (https://www.normaliz.uni-osnabrueck.de/)
- Specify the full path of normaliz.exe in Mathematica (See Examples.nb)




## Capabilities of HilbertBalance

Use Hilbert basis to to obtain the Hilbert-basis reactions of chemical equations, e.g

- ```HilbertBalance["NH4ClO4+HNO3+HCl+H2O->H5ClO6+N2O+NO+NO2+Cl2"]```
- ```HilbertBalance["HClO3->HClO4+Cl2+O2+H2O->Cl2+O2+H2O"]```
- ```HilbertBalance["ClO3+Cl+H->ClO2+Cl2+H2O",  "Charge" -> {-1, -1, 1, 0, 0, 0}]```
- More examples can be found in Examples.nb.
- See [arXiv:2410.06023](http://arxiv.org/abs/2410.06023) for a detailed description of our algorithm.

## Release Notes

- v1.0b
  - Support Mac and Linux OS
