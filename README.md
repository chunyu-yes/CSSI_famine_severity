# CSSI_famine_severity
Data and code for calculating cohort shrinkage index (CSSI) to measure famine severity in China.

## Data

### 1. China_pop.csv
The cohort size at the national level by birth year from 1900-2000.
* **Brithyear** represents the birth year of the cohort size.
* **China** represents the cohort size at the national level.
* **Group** represents the two years, 1950 and 1970, as a marker of the time window boundary.

### 2. Province_pop.csv
The cohort size at the provincial level by birth year from 1950-1970.
* **Birthyear** represents the birth year of the cohort size.
* **Beijing, Tianjin, ..., Xinjiang** represents the cohort size of different provinces.

### 3. prefecture_cssi.csv
The prefecture-level CSSI.
* **Province** represents the names of provinces.
* **Prefecture** represents the names of prefectures.
* **AdministrativeDivisionCode** represents the Administrative Division Code of prefectures.
* **CSSI_Pre** represents prefecture-level CSSI calculated based on pre-famine years (1950-1957).
* **CSSI_Pre.post** represents prefecture-level CSSI calculated based on pre- & post- famine years (1950-1957 & 1963-1970).

### 4. Chinamapdata.json
Map data for China at province level.
* **name** represents the name of the provinces.
* **geometry** represents geometry data for province mapping.

## Code
* **Main analyses1.R** contains code to calculate CSSI at the provincial level and regenerate Table S3.
* **Main analyses2.R** contains code to regenerate Figure 2-3.
