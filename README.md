# Holle_PacificPlantInvaders_BlacklistUncertainty_2023

Blacklisting potential Pacific plant invaders using species distribution models - uncertainties through species and environmental inputs


## Workflow

The analysis is based on an initial subset of known naturalized plant populations of 122 plant species occurring on at least one of the Hawaiian Islands, derived from the compiled dataset by [Wohlwend et al. (2021)](#2). For these species, all available occurrence records were obtained from the [Global Biodiversity Information Facility](https://www.gbif.org/) (GBIF) and the [Botanical Information and Ecology Network](https://biendata.org/) (BIEN) in June 2023, which were first cleaned to remove duplicates and occurrence records with erroneous time stamps or coordinates and were fit to a 1 km resolution.

The biogeographical status of the cleaned occurrences was assigned based on the [World Checklist of Vascular Plants](#1) (WCVP), indicating whether the species record is a native or introduced occurrence. Data gaps were filled using two additional sources, the [Global Inventory of Floras and Traits](https://gift.uni-goettingen.de/home) (GIFT) and the [Global Naturalized Alien Flora](https://glonaf.org/) (GloNAF). Resulting contradictory information was dealt with using two subsequent criteria: If the conflicting sources referred to areas of different sizes, the status from the source referring to the smaller area is used. If this was not the case, the status from the WCVP was preferred as it was based on level 3 (“botanical country”) of the world geographical scheme for recording plant distributions (TDWG). Final status assignments designated species records as native, introduced, or unknown. 

### 1: Species selection
The final species selection was based on cleaned and thinned native occurrence numbers using two criteria:  A lower limit of 40 native presences per species as well as unequal numbers of native and global occurrences. The spatial thinning of the occurrences pursued to avoid spatial autocorrelation using a thinning distance of 3 km. 

### 2: Background data generation and thinning

### 3: Relation of environmental data to occurrence data

### 4: Variable selection

### 5: SDM fitting

### 6: Model validation

### 7: Model predictions

### 8. Blacklist construction

### 9: Uncertainty analysis

### References

<a id="1"></a>
Govaerts, R (2023). The World Checklist of Vascular Plants (WCVP). Royal Botanic Gardens, Kew. Checklist dataset https://doi.org/10.15468/6h8ucr accessed via GBIF.org on 2023-11-10.

<a id="2"></a>
Wohlwend, MR, Craven, D, Weigelt, P, et al. (2021). Anthropogenic and environmental drivers shape diversity of naturalized plants across the Pacific. Divers Distrib.; 27: 1120– 1133. https://doi.org/10.1111/ddi.13260
