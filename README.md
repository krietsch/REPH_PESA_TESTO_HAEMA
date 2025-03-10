# **Data and code from:**

### Sex differences in testosterone and haematocrit levels reflect mating system differences of two Arctic-breeding shorebird species

### **Description**

This repository contains all data and code used for data analysis and figure production in *Sex differences in testosterone and haematocrit levels reflect mating system differences of two Arctic-breeding shorebird species* by 
Johannes Krietsch* [![ORCID_ID](./DATA/ILLUSTRATIONS/ORCID_ID_logo.png)](https://orcid.org/0000-0002-8080-1734), 
Wolfgang Goymann* [![ORCID_ID](./DATA/ILLUSTRATIONS/ORCID_ID_logo.png)](https://orcid.org/0000-0002-7553-5910), 
Mihai Valcu [![ORCID_ID](./DATA/ILLUSTRATIONS/ORCID_ID_logo.png)](https://orcid.org/0000-0002-6907-7802) and 
Bart Kempenaers [![ORCID_ID](./DATA/ILLUSTRATIONS/ORCID_ID_logo.png)](https://orcid.org/0000-0002-7505-5458).  

When using the data or code please cite the associated manuscript (in review) and [Open Science Framework repository](add link) (which is synchronized with this repository). Please do not hesitate to contact me if you have any questions, trouble running the code, found bugs or ideas to develop the project further. 

*\*contributed equally to the manuscript*  

<p>&nbsp;</p>

### **Repository Contents**

**`DATA/`**:

All data used in this analysis (**click on the black arrow** to see column definitions). Extracted from our database (see below).

<details>
  <summary> <b><code>REPH_PESA_testo_haema</code></b> – A csv table of all data used in this manuscript </summary>
  
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

<p>&nbsp;</p>

**`R/`**:

   - [`1_figures_and_statistic.R`](https://github.com/krietsch/testosterone_analysis/blob/master/R/1_R_script_data_anaylsis.R). 
  The main script to reproduce all figures and statistic of this project. It contains a detailed commented workflow and 
  follows the order in the manuscript.
  ([View compiled](https://raw.githack.com/krietsch/testosterone_analysis/master/OUTPUTS/R_COMPILED/7_figures_and_statistic.html "html"))
  
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

Krietsch\*, J., Goymann\*, W., Valcu, M. & Kempenaers, B. (in prep.). Data and code from "Sex differences in testosterone and haematocrit levels reflect mating system differences of two Arctic-breeding shorebird species". Open Science Framework. https://doi.org/10.17605/OSF.IO/DUXFZ

and the corresponding article:

Krietsch\*, J., Goymann\*, W., Valcu, M. & Kempenaers, B. (in prep.). Sex differences in testosterone and haematocrit levels reflect mating system differences of two Arctic-breeding shorebird species.


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

