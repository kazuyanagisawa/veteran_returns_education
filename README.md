# Returns to Education among U.S. Veterans — ACS 2022

**Author:** Cole Kazu Yanagisawa  
**Goal:** Estimate how education affects earnings among working-age U.S. veterans using the 2022 American Community Survey.

---

## Project Structure  
``` 
├── data/        # Raw IPUMS ACS data (.csv)  
├── notebooks/   # Jupyter notebooks for EDA & regression  
├── src/         # Python scripts (data cleaning, regression)  
├── outputs/     # Generated figures and tables  
└── README.md  
```

---

## Setup  
``` bash  
# 1. Create environment  
python -m venv .venv  
source .venv/bin/activate  # Mac/Linux  

# 2. Install dependencies  
pip install -r requirements.txt  
```

---

## To Run  
1. Place your IPUMS ACS CSV extract in /data/.  
2. Open /notebooks/01_acs_cleaning.ipynb for cleaning and analysis.  
3. Figures and tables will save to /outputs/.  

---

## 📦 Requirements  
These are the core libraries you’ll need.  

``` txt  
pandas  
numpy  
matplotlib  
seaborn  
statsmodels  
jupyter  
scipy  
pyarrow  
```
