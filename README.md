# [The implementation of irrigation leads to declines in farmland birds](https://www.sciencedirect.com/science/article/pii/S0167880921004059)

### Agriculture, Ecosystems and Environment

### Xabier Cabodevilla, Alexander D. Wright, Diego Villanua, Beatriz Arroyo, and Elise F. Zipkin

### Code/Data DOI: [![DOI](https://zenodo.org/badge/457900671.svg)](https://zenodo.org/badge/latestdoi/457900671)

### Please contact the first author for questions about the code or data: Xabier Cabodevilla (xabier.cabodevilla@ehu.eus)
__________________________________________________________________________________________________________________________________________

## Abstract:  
Assessing the effects of agricultural intensification on biodiversity is critical for developing effective management plans for farmland conservation. Among other factors, the direct and indirect impacts of irrigation on wildlife have yet to be thoroughly studied despite significant increases in the surface area of irrigated farmlands since the mid-twentieth century (currently greater than 300 million hectares worldwide). Here, we evaluate the impact of irrigation on bird species occurrence patterns using a BACI (Before-After Control-Impact) design. Our study occurs in a 100 km2 area with rainfed agriculture in the Mediterranean region of northern Spain. We analysed a 13-year dataset comprised of the 47 most common bird species in the region using a multi-species hierarchical occurrence model. We examined how the implementation of irrigation in a rain-fed farmland area altered the local bird community, identifying which species were negatively or positively impacted by changes to the local ecosystem. The implementation of irrigation had an overall negative impact on the bird community, with occurrence rates of most species (55%) decreasing and only a small fraction (11%) increasing after the onset of irrigation, leading to an overall reduction in site-level species richness. Irrigation had the most detrimental impact on farmland birds (including steppe birds, which are of high conservation concern), but also had negative effects on forest, shrubland, and non-specialist bird species that occur frequently in rainfed agricultural environments. The observed negative impacts on bird occurrences are likely due to the loss of nesting and foraging habitat arising from shifts in crops and/or loss of fallow lands associated with irrigation. The fact that only a few species responded positively to the implementation of irrigation suggests that in the long-term irrigation may lead to substantial negative changes in local bird communities, with less diversity and a lack of ecologically important farmland species. Irrigation schemes should thus be implemented carefully, avoiding areas with high species richness or high densities of endangered species. In cases where irrigation cannot be avoided, promoting diverse agrosystems, avoiding monocultures, and including interspersed rainfed crops and fallow lands may help to mitigate negative effects on local bird communities and their ecosystems.


## Data

[Arable_surface.txt](https://github.com/zipkinlab/Cabodevilla_etal_2022_AgEE/blob/main/Arable_surface.txt): The percent of arable land (cereal fields) within a 100 m radius buffer at each sampling location. This variable is used to estimate the occurrence state within the Multi-species occupancy model. 

[birds_occ_data.txt](https://github.com/zipkinlab/Cabodevilla_etal_2022_AgEE/blob/main/birds_occ_data.txt): Bird observations during the study period. The document is divided into five columns: site (sampling point), year (sampling year, 1=2007, 12=2019), rep (replicate visit), species (Species ID number, the name of species can be found within Bird species names.txt document), and occ (Observation status, observed = 1, non-observed = 0). This is the data file used within the Multi-species occupancy model.

[Date.txt](https://github.com/zipkinlab/Cabodevilla_etal_2022_AgEE/blob/main/Date.txt): Sampling date data. The document is divided into five columns: site (sampling point), year (sampling year, 1=2007, 12=2019), rep (replicate visit), species (Species ID number, the name of species can be found within Bird species names.txt document), and Date (the date of sampling). This variable is used to estimate the detection probabilities within the Multi-species occupancy model.

[Hour.txt](https://github.com/zipkinlab/Cabodevilla_etal_2022_AgEE/blob/main/Hour.txt): Sampling time data. The document is divided into five columns: site (sampling point), year (sampling year, 1=2007, 12=2019), rep (replicate visit), species (Species ID number, the name of species can be found within Bird species names.txt document), and Hour (the time of sampling). This variable is used to estimate the detection probabilities within the Multi-species occupancy model.

[Irrigation.txt](https://github.com/zipkinlab/Cabodevilla_etal_2022_AgEE/blob/main/Irrigation.txt): Data on irrigation status. 1 = irrigated and 0 = non-irrigates. Rows refers to sampling locations while columns refers to years.  

[Bird species names.txt](https://github.com/zipkinlab/Cabodevilla_etal_2022_AgEE/blob/main/Bird%20species%20names.txt): English and Latin names of studied species related to species ID number. 


## Code

[Multi-species occupancy model.R](https://github.com/zipkinlab/Cabodevilla_etal_2022_AgEE/blob/main/Multi-species%20occupancy%20model.R): Code for running the Multi-species occupancy model

[Graphs.R](https://github.com/zipkinlab/Cabodevilla_etal_2022_AgEE/blob/main/Graphs.R): Code needed to recreate the figures presented in this work


