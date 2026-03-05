READ ME for data preparation

R scripts:

- 1_step_of_preparation.R
- BTM_preparation.R
- RM_preparation.R
- Final_dataset_creation.R

Description:

1_step_of_preparation.R
	- 	takes original datasets from “Data/Data for 1_step_of_preparation” with written answers and converts them into numeric expressions
	-	cuts the dataset into individual scales creating 6 files containing pairwise comparisons and likert scale
	-	provides basic demography information

BTM_preparation.R
	-	takes files created in 1_step_of_preparation.R stored in “Data/snippets” and creates long BTM datasets
	-	output are 6 files containing prepared BTM long data for each scale used

RM_preparation.R
	-	takes files created in 1_step_of_preparation.R from “Data/snippets” and additional data from outsourced data collection from “Data/Outsourced data”
	-	in first part it reverses reversed variables and connects both data collections together
			-	resulting in 6 files. Data collections can be recognized by collection variable (1=in house collection, 2 = outsourced collection)
			-	plus it creates id_codebook, so we are able to connect participant study ids and their ids in dataset
	-	it creates connected datasets for 2PL estimation
	-	second part binarizes used scales and result in 6 files with binary values (-1 = disagreement, 1 = agreement)
	-	third part creates long data for and results in 6 files with binary values

Final_dataset_creation.R
	-	takes files created in BTM_preparation.R from “Data/BTM” and RM_preparation.R from “Data/RM”, containing prepared BTM long data and RM long data
	-	it creates one dataset containing all scales in both response forms under name “analysis_total.csv”
	-	next it takes analysis total and adjusts it for Global model estimating BTM and RM in one step, via adjusting reversed items and creates file “analysis_total_GM.csv”
					-	this happens through changing long data in V1:V8 from easiness to difficulty (1 to -1) for selected items and creating item_type variable
						which is a copy of id_type with changed sign for selected items (1 to -1)



DATA ANALYSIS - data

For 2PL estimation, FA, PCA - “Data/2PL/xxx_combined.csv” 
	-	reversed items already reversed, ready for 2PL estimation as well as FA and PCA
	-	beware that in aut_combined, collection 1 contains 4-point LS (1,2 = 0; 3,4 = 1), but collection 2 contains 2-point LS (1=0; 2=1)

For BTM - “Data/BTM/bt_xxx.csv” 

For RM - “Data/RM/xxx_ready.csv”

For Global model “Data/analysis_total_GM.csv”
	-	beware this file has changed signs for reversed variables in V1:V8 field and item_type column

CODE for data analysis
	-	data_analysis.R - primarily using dataset analysis_total_GM.csv




Basic terminology
-	Global model			— model estimating RM and BTM in one step
-	in house collection		— data collection performed as a part of the study by its team at Masaryk University (11-12/2024)
-	outsourced collection	— datasets acquired from various other studies completed by our colleagues 
-	Scales:
-		AUT			— autonomy part of BPNSFS questionnaire. Reversed items are coded as V2, V4, and V7 (only 7 items as we were provided only 7)
-		BMPN			— selected part of Balanced Measure of Psychological Needs questionnaire. V2 and V5 are reversed - 5 items together
-		DASS			— stress sub scale from Depression, Anxiety, Stress scales - 7 items together
-		DEM			— scale of Democracy by J. Serek - 5 items together
-		DERS			— part of Difficulties in Emotion Regulation Scale Short Form (items 7-12 and 15) - 7 items together
-		HI			— selected items of Height Inventory - 8 items altogether

