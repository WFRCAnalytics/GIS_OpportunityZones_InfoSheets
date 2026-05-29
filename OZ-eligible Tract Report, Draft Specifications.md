Landscape-oriented table-based report, each eligible tract ([web map](https://wfrc.maps.arcgis.com/apps/mapviewer/index.html?webmap=3ae7c36bdc8c49e69d91f410717b3b37)) is a row, each section below is a column with the elements formatted within the corresponding cell.

Note: we'll aim to produce the data elements below, we can then decide to exclude unneeded items during the report formatting process.

---

**Quick Reference Map image** (approximately 3" x 2")**:** Tract boundary extent x 1.5, transit stations, SAP buffer, WC centers, maybe also include regionally significant land use districts on the map??

**Tract Info:**  *County (sort by, primary), Tract ID (sort by, secondary)*, Current Population, Current HH estimate, Intersection with OZ 1.0 (acres, %), Total Acres, Developable Acres (from TDM TAZ)\*, *maybe give a descriptive name to each tract?*  

*I realize the list below is the fodder.  So I know you know this, but the deliverable needs to be easily absorbable.  I’ve bolded the four key measures in my mind.* 

**Centers and other Regionally Significant Land Uses:** (area of intersection)

* **1\) \*\*% of tract with MUorC centers**  
* Metropolitan Center: acres (% of tract)  
* Urban Center: acres (% of tract)  
* City Center: acres (% of tract)  
* Neighborhood Center: acres (% of tract)  
* Education District: acres (% of tract)  
* Employment District: acres (% of tract)  
* Industrial District: acres (% of tract)  
* Retail District: acres (% of tract)  
* Special District: acres (% of tract)  note: these are military and other non-developable lands  
* 

**Housing:** (from Jan 1 2025 Housing Unit Inventory)

* Current housing units: count by type  
* Residential development: Total acres, SFD acres, MF/SFA acres


ATO:

* **2\) \*\*We need a composite ATO score for both modes both ways**

**Auto:**

* Freeway Interchanges within\*\*\*  
* Workplace ATO: Jobs within typical auto commute  
* Workplace ATO: HHs within typical transit commute\*\*\*

**Transit:** 

* Current Rail and BRT Stations within: Count (List of Names)\*\*  
* SAP buffer areas: acres (% of tract)  
* **3\) \*\*Additional housing planned in station area plans within tract normalized by tract area** (BYRON will provide this separately based on his SAP plan tracking, he has the MAG info too)  
* Workplace [ATO](https://services1.arcgis.com/taguadKoI1XFwivx/ArcGIS/rest/services/AccessToOpportunities_gdb/FeatureServer/0): Jobs within typical transit commute\*\*\*  
* Workplace ATO: HHs within typical transit commute\*\*\*  
* Planned station notes: additional Phase 1 stations planned / needed

**Projected Growth 2027 \- 2037 (**from regional forecast feature services, see links)

* **4\) \*\*Can we convert the below into Equivalent Residential Units (normalized by tract area)?**    
* [HHs added](https://services1.arcgis.com/taguadKoI1XFwivx/ArcGIS/rest/services/Household_Projections_TAZ_RTP_2023/FeatureServer/0)\*  
* [Population added](https://services1.arcgis.com/taguadKoI1XFwivx/ArcGIS/rest/services/Household_Projections_TAZ_RTP_2023/FeatureServer/0)\*  
* [Jobs added](https://services1.arcgis.com/taguadKoI1XFwivx/ArcGIS/rest/services/Household_Projections_TAZ_RTP_2023/FeatureServer/0)\* (we'll use this dataset for typical jobs, which excludes ag, const, & mining)

**Urban Institute analysis**

* Oztoolclassification \- attribute for the likelihood of attracting investment  
* Other useful tract attributes like unemployment rate, poverty rate, etc    
* https://wfrc.maps.arcgis.com/home/item.html?id=3451d337580e4ed48d167fd76326d61e

\*where TAZ based zones don't follow tracts, use area proportioning to estimate growth in tract  
\*\* when no station or interchange is within, provide the distance to nearest from edge and from centroid  
**\*\*\*** from the TAZ scores, a weighted average by dev acres)

