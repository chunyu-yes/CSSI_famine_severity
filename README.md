# CSSI_famine_severity
Data and code for calculating cohort shrinkage index (CSSI) to measure famine severity in China.

## Data

### 1. China_pop.csv
The cohort size at the national level by birth year from 1900-2000.
* **Brithyear** represents the birth year of the cohort size.
* **China** represents the cohort size of the whole China.
* **Group** represents the two years, 1950 and 1970, as a marker of the time window boundary.

### 2. Province_pop.csv
The cohort size at the provincial level by birth year from 1950-1970.
* **Birthyear** represents the birth year of the cohort size.
*  **Beijing, Tianjin, ..., Xinjiang** represents the cohort size of different provinces.

### 3. prefecture_cssi.csv
The prefecture-level CSSI.
* **Province** represents the names of provinces.
* **Prefecture** represents the names of prefectures.
* **AdministrativeDivisionCode** represents the Administrative Division Code of prefectures.
* **CSSI_Pre** represents prefecture-level CSSI calculated based on pre-famine years (1950-1957).
* **CSSI_Pre.post** represents prefecture-level CSSI calculated based on pre- & post- famine years (1950-1957 & 1963-1970).

### 4. Map data
General data used to make Chinese map.

#### 4.1 gadm36_CHN_1_sf.rds
Map data for China at province level.
* **NAME_1** represents the name of the provinces.
* **geometry** represents geometry data for province mapping.

#### 4.2 gadm36_TWN_0_sf.rds
Map data for Taiwan at province level.
* **NAME_0** represents the name of the region.
* **geometry** represents geometry data for the region mapping.

#### 4.3 gadm36_CHN_2_sf.rds
Map data for China at prefecture level.
* **NAME_1** represents the name of the provinces.
* **NAME_2** represents the name of the prefectures.
* **geometry** represents geometry data for prefecture mapping.

#### 4.4 gadm36_TWN_1_sf.rds
Map data for Taiwan at prefecture level.
* **NAME_1** represents the name of the provinces.
* **NAME_2** represents the name of the prefectures.
* **geometry** represents geometry data for prefecture mapping.

## Code
* **Main analyses1.R** contains code to calculate CSSI at provincial level and regenerate Table 1.
* **Main analyses2.R** contains code to regenerate the Figure 2-4.
