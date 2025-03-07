# **Data and code from:**

### Sex differences in testosterone and haematocrit levels reflect mating system differences of two Arctic-breeding shorebird species

### **Description**

This repository contains all data and code used for data analysis and figure production in *Sex differences in testosterone and haematocrit levels reflect mating system differences of two Arctic-breeding shorebird species* by 
Johannes Krietsch* [![ORCID_ID](./DATA/ILLUSTRATIONS/ORCID_ID_logo.png)](https://orcid.org/0000-0002-8080-1734), 
Wolfgang Goymann* [![ORCID_ID](./DATA/ILLUSTRATIONS/ORCID_ID_logo.png)](https://orcid.org/0000-0002-7553-5910), 
Mihai Valcu [![ORCID_ID](./DATA/ILLUSTRATIONS/ORCID_ID_logo.png)](https://orcid.org/0000-0002-6907-7802) and 
Bart Kempenaers [![ORCID_ID](./DATA/ILLUSTRATIONS/ORCID_ID_logo.png)](https://orcid.org/0000-0002-7505-5458).  

When using the data or code please cite the associated manuscript (in review) and [Open Science Framework repository](add link) (which is synchronized with this repository). Please do not hesitate to contact me (j.krietsch@orn.mpg.de) if you have any questions, trouble running the code, found bugs or ideas to develop the project further. 

*contributed equally to the manuscript

<p>&nbsp;</p>

### **Repository Contents**

**`DATA/`**:

All data used in this analysis (**click on the black arrows** to see column definitions). Extracted from our database (see below).

<details>
  <summary> <b><code>REPH_PESA_testo_haema</code></b> – A csv table of all data used in this manuscript </summary>
  
  Columns are defined as:

  1.	`year_`: year

</details>

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

Krietsch, J., Goymann, W., Valcu, M. & Kempenaers, B. (in prep.). Data and code from ‘Sex differences in testosterone and haematocrit levels reflect mating system differences of two Arctic-breeding shorebird species’. Open Science Framework. https://doi.org/ADD_HERE

and the corresponding article:

Krietsch, J., Goymann, W., Valcu, M. & Kempenaers, B. (in prep.). Sex differences in testosterone and haematocrit levels reflect mating system differences of two Arctic-breeding shorebird species.


<p>&nbsp;</p>

  
