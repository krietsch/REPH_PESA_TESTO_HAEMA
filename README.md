# **Data and code from:**

### Sex differences in testosterone and haematocrit levels reflect mating system differences of two Arctic-breeding shorebird species

### **Authors**

Johannes Krietsch* $^{1,2}$[![ORCID_ID](./DATA/ILLUSTRATIONS/ORCID_ID_logo.png)](https://orcid.org/0000-0002-8080-1734) ([johannes.krietsch@nioz.nl](mailto:johannes.krietsch@nioz.nl)), 
Wolfgang Goymann* $^{3,4}$[![ORCID_ID](./DATA/ILLUSTRATIONS/ORCID_ID_logo.png)](https://orcid.org/0000-0002-7553-5910) ([wgoymann@bi.mpg.de](mailto:wgoymann@bi.mpg.de)), 
Mihai Valcu $^1$[![ORCID_ID](./DATA/ILLUSTRATIONS/ORCID_ID_logo.png)](https://orcid.org/0000-0002-6907-7802) ([mihai.valcu@bi.mpg.de](mailto:mihai.valcu@bi.mpg.de)) and 
Bart Kempenaers $^1$[![ORCID_ID](./DATA/ILLUSTRATIONS/ORCID_ID_logo.png)](https://orcid.org/0000-0002-7505-5458) ([bart.kempenaers@bi.mpg.de](mailto:bart.kempenaers@bi.mpg.de)).  

$^1$ Department of Ornithology, Max Planck Institute for Biological Intelligence, Eberhard-Gwinner-Str., 82319 Seewiesen, Germany

$^2$ Department of Coastal Systems, NIOZ Royal Netherlands Institute for Sea Research, NL-1790 AB, Den Burg, Texel, the Netherlands

$^3$ Department for Behavioural Neurobiology, Max Planck Institute for Biological Intelligence, Eberhard-Gwinner-Str., 82319 Seewiesen, Germany

$^4$ Ludwig Maximilians University Munich, Department Biology II, Großhaderner Str. 2, 82152 Martinsried, Germany


*\*contributed equally to the manuscript*  

<p>&nbsp;</p>

### **Description**

This repository contains all data and code used for data analysis and figure production in *Sex differences in testosterone and haematocrit levels reflect mating system differences of two Arctic-breeding shorebird species*.


When using the data or code please cite the associated manuscript (in review) and [Open Science Framework repository](https://doi.org/10.17605/OSF.IO/DUXFZ) (which is synchronized with this repository). Please do not hesitate to contact me if you have any questions, trouble running the code, found bugs or ideas to develop the project further. 

#### **Data**

We studied a population of pectoral sandpipers and red phalaropes breeding sympatrically in open wet tundra habitat near Utqiagvik (formerly Barrow), Alaska ([71°19’N 156°39’W](https://www.google.com/maps/place/71%C2%B019'00.0%22N+156%C2%B039'00.0%22W/@71.3152243,-156.7083365,10942m/data=!3m1!1e3!4m4!3m3!8m2!3d71.3166667!4d-156.65?entry=ttu&g_ep=EgoyMDI1MTEwNC4xIKXMDSoASAFQAw%3D%3D)) between late May and late July. We caught both species with handheld mist nests (24x1.2 m) on the first snow free patches close to roads around Utqiagvik, and – as soon as birds arrived there – on our 2.5 km$^2$ study site in 2004-2009, 2012, 2014 and 2018 for pectoral sandpipers and 2017-2019 for red phalaropes. For more details, please see the methods section of the manuscript.

#### **Code**

All statistical analyses were conducted with R, version 4.5.1 (R Core Team, 2025). For a detailed list of all packages and versions used, please see session info at the end of the [complied figures and statistics R script](https://raw.githack.com/krietsch/testosterone_analysis/master/OUTPUTS/R_COMPILED/1_figures_and_statistics.html "html").

<p>&nbsp;</p>


### **Repository Contents**

**`DATA/`**:

All data used in this analysis (**click on the black arrow** to see column definitions). Extracted from our database (see below).

<details>
  <summary> <b><code>REPH_PESA_testo_haema</code></b> – A csv table of all testosterone and haematocrit data used in this manuscript </summary>
  
  Columns are defined as:

  1. `species`: Species abbreviation (REPH = red pahalarope, PESA = pectoral sandpiper)
  2. `ID`: Metal band ID (unique identifier for each individual bird)
  3. `year_`: Year in which the data was collected
  4. `date_`: The date of capture in YYYY-MM-DD format
  5. `caught_time`: The exact datetime the bird was caught (in AKDT)
  6. `bled_time`: The datetime at which the bird was bled for sampling (in AKDT)
  7. `sex`: The sex of the bird (`M` for male, `F` for female)
  8. `tarsus`: Length of the tarsus (in mm)
  9. `wing`: Wing length (in mm)
  10. `weight`: Weight of the bird (in g)
  11. `testo`: Testosterone level measured (in pg/ml)
  12. `volume`: Blood plasma volume (in μl)
  13. `GnRH`: If GnRH was injected indicates the dose (low or high)
  14. `haema`: Heamatocrit level, as percentage of red blood cells in the blood sample

</details>


<details>
  <summary> <b><code>REPH_PESA_nests</code></b> – A csv table of all nest data used in this manuscript </summary>
  
  Columns are defined as:

  1. `species`: Species abbreviation (REPH = red pahalarope, PESA = pectoral sandpiper)
  2. `year_`: Year in which the data was collected
  3. `nest`: Unique identifier for each nest
  4. `male_id`: Male metal band id
  5. `female_id`: Female metal band id
  6. `lat`: nest location latitude (decimal degrees)
  7. `lon`: nest location longitude (decimal degrees)
  8. `initiation`: Estimated date and time the first egg was laid (AKDT)

</details>

<p>&nbsp;</p>

**`R/`**:

   - [`1_figures_and_statistics.R`](https://github.com/krietsch/testosterone_analysis/blob/master/R/1_figures_and_statistics.R). 
  The main script to reproduce all figures and statistic of this project. It contains a detailed commented workflow and 
  follows the order in the manuscript.
  ([View compiled](https://raw.githack.com/krietsch/testosterone_analysis/master/OUTPUTS/R_COMPILED/1_figures_and_statistics.html "html"))
  
  - [`0_extract_data_from_database.R`](https://github.com/krietsch/REPH_PAIRS/blob/master/R/0_extract_data_from_database.R) 
  Script used to extract the data from our database. This script can only be run with access 
  to our database and is only stored to document the process. 
  
**`OUTPUTS/`**:

  - `FIGURES/` – Contains all figures created for this manuscript
  
  - `ESM/` – Contains all supplementary tables created for this manuscript
  
  - `R_COMPILED/` – Contains a compiled html flies of `1_figures_and_statistics.R`

<p>&nbsp;</p>


### **Terms of Use**

The data and code are supplied for scientific analysis, research, teaching or conservation purposes and shall be used only in accordance with the [GNU GPLv3](https://github.com/krietsch/REPH_PATERNITY/blob/master/LICENSE) license.
For any publication making substantial use of the data or code, the authors welcome the opportunity for collaboration and to comment prior to publication.

If you use the code or data, please cite the OSF repository linked to this github project: 

Krietsch\*, J., Goymann\*, W., Valcu, M. & Kempenaers, B. (2025). Data and code from "Sex differences in testosterone and haematocrit levels reflect mating system differences of two Arctic-breeding shorebird species". Open Science Framework. https://doi.org/10.17605/OSF.IO/DUXFZ

and the corresponding article:

Krietsch\*, J., Goymann\*, W., Valcu, M. & Kempenaers, B. (2025). Sex differences in testosterone and haematocrit levels reflect mating system differences of two Arctic-breeding shorebird species. Behavioural Ecology.


*\*contributed equally to the manuscript*  


<p>&nbsp;</p>

  
### **Acknowledgments**


This work was funded by the [Max Planck Society](https://www.mpg.de/en) (to B.K.). J.K. was supported by the International Max Planck Research School for Organismal Biology. 

We thank Margherita Cragnolini, Kristina Beck, Eunbi Kwon, Pietro D'Amelio, Giulia Bambini, Peter Santema, Fenja Squirrell, Alice Pintaric, Carol Gilsenan, Anne Cillard, Kim Teltscher, Martin Bulla, Andrea Wittenzellner, Luisana Carballo, Sylvia Kuhn and Sabine Spehn for help in the field (ordered by days in the field). We also thank Richard B. Lanctot for advice and help with logistics, permits and equipment. We are grateful to the state and federal committees that reviewed and approved permits for this study, and to the Ukpeaġvik Iñupiat Corporation for logistic support and access to their lands. 

<p>&nbsp;</p>

<p align="middle">
  <a href="https://www.bi.mpg.de/en">
    <img src="./DATA/ILLUSTRATIONS/MPIO_logo.png" width="500" />
  </a>
    <img src="./DATA/ILLUSTRATIONS/IMPRS_logo.png" width="280" /> 
</p>

