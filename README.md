# Uncertainty in blacklisting potential Pacific plant invaders using species distribution models


## Workflow
The analysis built on the global occurrence data of the plant species listed in the [PaciFLora](https://bdj.pensoft.net/article/67318/) data set. Each occurrence point matched with a biogeographic status (native or introduced). Code for the data preparation can be found in a separate [repository](https://github.com/UP-macroecology/GlobalOccurrences).

From these species, we included an initial subset of known plant populations of 122 plant species that occur as most invasive on at least one of the Hawaiian Islands, derived from the compiled data set by [Wohlwend et al. (2021)](#2), in our study


### 1 - Initial species selection
The initial species selection was based on cleaned and thinned native occurrence numbers using two criteria to ensure the consideration of two different niches:  A lower limit of 40 native presences per species as well as a minimum difference of 40 between native and global occurrences. Additionally, species were excluded when having native assignments on the Hawaiian Islands. The spatial thinning of the occurrences pursued to avoid spatial autocorrelation using a thinning distance of 3 km. 

### 2 - Background data generation and thinning
For the selected study species, background data was derived. This was done by a random selection of points excluding the presence locations within a buffer distance of 200 km using a presence-absence ratio of 1:10. Spatial thinning was repeated for the generated background data using the same thinning distance of 3 km. As a last step for species data preparation, the sampled thinned background data was merged with the cleaned and thinned presence data. This described species processing steps were separately carried out based on native and global occurrences.

### 3 - Relation of environmental data to occurrence data
For each study species, the occurrence information was related to the environmental data. Environmental data comprised 15 climatic variables (CHELSA, version 2.1), and 14 edaphic variables (SoilGrids, version 2.0) at a resolution of 1 km. These environmental data were related in a way that each of the two distribution datasets (based on native and global occurrences) was separately related to purely climatic and combined climatic and edaphic predictor types. As output, four differently combined datasets a joined distribution and environmental data were generated that acted as input for the SDMs. Based on the occurrence numbers of the four differently combined data sets of joined distribution and environmental data per species, the final species selection was made. Once again, a lower limit of 40 native presences and a minimum difference of 40 between native and global occurrences were applied as criteria. 

### 4 - Variable selection
For each of the four SDM input datasets, the four most important and weakly correlated variables based on the Boyce index were extracted from the ranked predictor output that were then considered in the upcoming model-building process. For the models that used both predictor types, the two climatic and edaphic variables that were ranked first in the predictor output were considered.

### 5 - SDM fitting and validation
Based on each of the four SDM input datasets, four different model algorithms were applied to fit the models: GLM, GAM, RF, and BRT. For each model, the performance was assessed using the 5-fold cross validation based on five different performance measures: The area under the receiver operating characteristic curve (AUC), true skill statistic (TSS), sensitivity, specificity, and the Boyce index. Additionally, the ensemble performance of the four different models using the same niche in the input occurrence data and environmental data was assessed.

### 6 - Model predictions
For each species, predictions were made to the investigated Pacific island groups considering models fitted with the four different algorithms per SDM input data set. Moreover, an ensemble model was established by combining the continuous predictions of each algorithm using the arithmetic mean. The predicted occurrence probabilities were then converted into binary predictions, translating into predicted presences and absences. In a last step, the fraction of predicted suitable habitat in % per island group as well as Pacific-wide was quantified.

### 7 - Blacklist construction
Blacklists, reflecting the species with the highest Pacific-wide establishment potential, were constructed using three different definitions: The total fraction of predicted suitable habitat, the mean suitable habitat fraction over all island groups, and the total number of island groups with suitable habitats. This was done considering the predictions of the different predictor sets as well as the applied algorithms, resulting in 16 blacklists each based on unique information.

### 8 - Quantification of uncertainty
Using random forest regression, it was assessed whether the blacklist rankings were associated with the considered niche in the input occurrence data (native and global), the used predictor type (purely climatic and combined climatic and edaphic), and the applied algorithms (GLM, GAM, RF, BRT). The output of variable importance was considered for the uncertainty quantification of the studied sources. This was separately done for all three blacklist definitions.

### 9 - Pacific-wide unrealised colonisation potential
Additionally, it was quantified how many non-native species could establish on currently unoccupied island groups. This was done by calculating the Pacific-wide predicted unrealised colonisation potential as the False POsitive Rate for all study species, with an exclusive focus on the prediction results of ensemble models. The measure accounted for predicted presence and absence information as well as actual presence and absence information on the studied Pacific island groups. Here, the very complete occurrence data set, gathered within the study of Wohlwend et al. (2021), was used as independent validation data.

### 10 - Final Pacific-wide blacklisting
Final blacklist rankings were created for all three blacklist definitions based on the ensemble predictions.




### References

<a id="1"></a>
Govaerts, R (2023). The World Checklist of Vascular Plants (WCVP). Royal Botanic Gardens, Kew. Checklist dataset https://doi.org/10.15468/6h8ucr accessed via GBIF.org on 2023-11-10.

<a id="2"></a>
Wohlwend, MR, Craven, D, Weigelt, P, et al. (2021). Anthropogenic and environmental drivers shape diversity of naturalized plants across the Pacific. Divers Distrib.; 27: 1120– 1133. https://doi.org/10.1111/ddi.13260
