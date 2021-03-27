/*******************************************************************************
Replication of part of Professor Friedson's "Medical Scribes as an Input in 
Healthcare Production".
	-Tables 1, 2, 4 (panels a through f), and 5 (panel a)

Data provided by Professor Friedson.

SAS 9.4 used in SAS OnDemand for Academics.

Devin Bazata
*******************************************************************************/

%LET PATH = ~/FriedsonReplication;
LIBNAME FriedRep "&path./";

/*******************************************************************************
I'm going to start by importing the STATA data files provided by Professor
Friedson.
*******************************************************************************/

PROC IMPORT OUT = FriedRep.File1 DATAFILE = "&path./STATA Project File 1.dta";
RUN;

PROC CONTENTS DATA = FriedRep.File1;
RUN;

PROC IMPORT OUT = FriedRep.File2 DATAFILE = "&path./STATA Project File 2.dta";
RUN;

PROC CONTENTS DATA = FriedRep.File2;
RUN;

/*******************************************************************************
The next step is to merge these two files together. File 1 contains the data
about the scribes and the shifts they worked. File 2 contains the hospital and
doctor information, including procedures done during the shift.

These data sets are merged along several variables. These are month, day,
hospital, which shift (evening, day, night, etc), and which physician the scribe
was paired with.
*******************************************************************************/

PROC SORT DATA = FriedRep.File1;
	BY location month date starttime PhysicianID;
RUN;

PROC SORT DATA = FriedRep.File2;
	BY location month date starttime PhysicianID;
RUN;

* Merging the datasets and creating some variables;

DATA FriedRep.combined;
	MERGE FriedRep.File2 FriedRep.File1;
	BY location month date starttime PhysicianID;
	LENGTH scribe_present 3 full_date 8;
	FORMAT full_date MMDDYY.;
	IF scribe ^= . THEN scribe_present = 1;
	ELSE scribe_present = 0;
	full_date = MDY(month, date, 2015);
	* Creating the overtime variable;
	LENGTH overtime 8;
	overtime = hours - publishedhours;
RUN;

/*******************************************************************************
The next step is to replicate Table 1. Since the combined dataset contains
unused observations, these steps will be restricting to the shifts actually
used in the experiment. 

Shifts included in this table are only those from
March through November, only at 3 of the emergency rooms (and for one of those
emergency rooms only the day and evening shifts), shifts where doctors did not
trade or call out, and shifts where the physician was included in the study.

This pared-down data used to create Table 1 (and further tables) is saved
as FriedRep.restricted.

These steps generate a file called "Table1.xlsx" which contains the various 
PROC FREQ outputs that will be used to make Table 1.
*******************************************************************************/

* Creating the data set used to make Table 1;

DATA FriedRep.restricted;
	SET FriedRep.combined;
	* Restricting the data set to the proper time range;
	IF (month >= 3 AND month <= 11);
	* Restricting the data set to the proper locations, excluding the night
			shifts for the smallest ER;
	IF (location = "ER2" OR location = "ER3" OR 
			(location = "ER1" AND (starttime = 700 OR starttime = 1500)));
	* Removing shifts where the doctor called in or swapped shifts;
	IF hours = . THEN DELETE;
	* Removing shifts during the study time period, but the physician
			wasn't included in the study yet. To find the first time they
			are included I sorted FriedRep.file1 by PhysicianID, month,
			and date and checked when their first scribe shift was;
	IF (PhysicianID = 1 AND full_date < '02APR2015'd) THEN DELETE;
	IF (PhysicianID = 2 AND full_date < '23MAY2015'd) THEN DELETE;
	IF (PhysicianID = 3 AND full_date < '21MAR2015'd) THEN DELETE;
	IF (PhysicianID = 4 AND full_date < '22MAR2015'd) THEN DELETE;
	IF (PhysicianID = 5 AND full_date < '24MAR2015'd) THEN DELETE;
	IF (PhysicianID = 6 AND full_date < '08APR2015'd) THEN DELETE;
	IF (PhysicianID = 7 AND full_date < '13APR2015'd) THEN DELETE;
	IF (PhysicianID = 8 AND full_date < '19JUN2015'd) THEN DELETE;
	IF (PhysicianID = 9 AND full_date < '16MAR2015'd) THEN DELETE;
	IF (PhysicianID = 10 AND full_date < '17MAY2015'd) THEN DELETE;
	IF (PhysicianID = 11 AND full_date < '04APR2015'd) THEN DELETE;
	IF (PhysicianID = 12 AND full_date < '13MAY2015'd) THEN DELETE;
	IF (PhysicianID = 13 AND full_date < '18APR2015'd) THEN DELETE;
	IF (PhysicianID = 14 AND full_date < '16MAR2015'd) THEN DELETE;
	IF (PhysicianID = 15 AND full_date < '02APR2015'd) THEN DELETE;
	IF (PhysicianID = 16 AND full_date < '17MAR2015'd) THEN DELETE;
RUN;

* Creating variables needed for the rest of the table;

DATA FriedRep.restricted;
	SET FriedRep.restricted;
	LENGTH time_day $ 13 weekend 3;
	IF SUBSTR(shiftname, 1, 1) = "D" THEN time_day = "Day";
	ELSE IF (shiftname = "E2" OR shiftname = "E3" OR shiftname = "E4") 
			THEN time_day = "Evening";
	ELSE IF SUBSTR(shiftname, 1, 1) = "N" THEN time_day = "Overnight";
	ELSE time_day = "Other/special";
	IF WEEKDAY(full_date) = 1 OR WEEKDAY(full_date) = 7 THEN weekend = 1;
	ELSE weekend = 0;
RUN;

* Creating the rest of Table 1. Start by creating a macro to keep things clean;

%MACRO MakeTable1(var);

PROC FREQ DATA = FriedRep.restricted;
	TABLES &var. * scribe_present / NOROW NOCOL NOPERCENT;
RUN;

%MEND MakeTable1;

* Calling the macro to create the other parts of Table 1;

ODS excel FILE = "&path./Table1.xlsx";

PROC FREQ DATA = FriedRep.restricted;
	TABLES scribe_present / NOCUM NOFREQ NOPERCENT;
RUN;

%MakeTable1(time_day);
%MakeTable1(weekend);
%MakeTable1(location);
%MakeTable1(PhysicianID);

ODS excel CLOSE;

/*******************************************************************************
The next step is to create Table 2. This table is a difference in means table.
These steps will not generate Table 2, but will create the values for it.
These steps will create the statistics required and collect them in two
separate data files and then the values will be copy and pasted into a .xlsx
file. 
*******************************************************************************/

* Start by creating a macro to create the means and compare them;

%MACRO MakeTable2(var);

PROC TTEST DATA = FriedRep.restricted;
	CLASS scribe_present;
	VAR &var.;
	ODS OUTPUT ConfLimits = work.ttests_diff_&var.;
RUN;

PROC TTEST DATA = FriedRep.restricted;
	VAR &var.;
	ODS OUTPUT ConfLimits = work.ttests_&var.;
RUN;

%MEND MakeTable2;

* Next, creating the rest of the variables needed for Table 2;
* Start by using a PROC UNIVARIATE to find out the top and bottom 10%
of the distribution of RVUs to calculate the trimmed_rvus variable;

PROC UNIVARIATE DATA = FriedRep.restricted;
	var rvus;
RUN;

DATA FriedRep.restricted;
	SET FriedRep.restricted;
	LENGTH overtime_dummy 3 trimmed_rvus 8 splint_codes 8 obs_codes 8;
	IF overtime > 0 THEN overtime_dummy = 1;
	ELSE overtime_dummy = 0;
	IF rvus < 105.060 AND rvus > 39.440 THEN trimmed_rvus = rvus;
	splint_codes = cpt29105 + cpt29125 + cpt29130 + cpt29515;
	obs_codes = cpt99218 + cpt99219 + cpt99220 + cpt99224 + cpt99225
			+ cpt99226;
RUN;

* Calling the macro to create the values for Table 2;

ODS GRAPHICS OFF;
ODS EXCLUDE ALL;

%MakeTable2(overtime);
%MakeTable2(overtime_dummy);
%MakeTable2(rvus);
%MakeTable2(trimmed_rvus);
* CPT code for EKGs;
%MakeTable2(cpt93010);
%MakeTable2(splint_codes);
%MakeTable2(obs_codes);
%MakeTable2(dtd);
%MakeTable2(patients);

ODS EXCLUDE NONE;

* The previous macros created a collection of data sets with the individual
		t-tests, now we'll combine into two separate data files. Values
		from these files will then be copy and pasted into a .xlsx file;
		
DATA FriedRep.Table2Row1;
	SET work.TTESTS_OVERTIME_DUMMY work.TTESTS_OVERTIME work.TTESTS_RVUS 
			work.TTESTS_TRIMMED_RVUS work.TTESTS_CPT93010 
			work.TTESTS_SPLINT_CODES work.TTESTS_OBS_CODES work.TTESTS_DTD 
			work.TTESTS_PATIENTS;
RUN;

DATA FriedRep.Table2Rest;
	SET work.TTESTS_DIFF_OVERTIME_DUMMY work.TTESTS_DIFF_OVERTIME
			work.TTESTS_DIFF_RVUS work.TTESTS_DIFF_TRIMMED_RVUS
			work.TTESTS_DIFF_CPT93010 work.TTESTS_DIFF_SPLINT_CODES 
			work.TTESTS_DIFF_OBS_CODES work.TTESTS_DIFF_DTD
			work.TTESTS_DIFF_PATIENTS;
	* Dropping unnecessary statistics;
	IF Method = "Satterthwaite" THEN DELETE;
RUN;

/*******************************************************************************
Next I will be creating Table 4, panels a through f. The results from these
regressions will be copied over to the table manually.
*******************************************************************************/

* Creating dummy variables needed for the regressions;

DATA FriedRep.restricted;
	SET FriedRep.restricted;
	LENGTH Phys1-Phys16 3 month3-month11 3 location1-location3 3
			time_day1-time_day4 3;
	ARRAY Phys Phys1-Phys16;
	DO i = 1 to 16;
		IF PhysicianID = i THEN Phys(i) = 1;
		ELSE Phys(i) = 0;
	END;
	ARRAY mon month3-month11;
	DO i = 1 to 9;
		IF month = i THEN mon(i) = 1;
		ELSE mon(i) = 0;
	END;
	IF location = "ER1" THEN location1 = 1;
	ELSE location1 = 0;
	IF location = "ER2" THEN location2 = 1;
	ELSE location2 = 0;
	IF location = "ER3" THEN location3 = 1;
	ELSE location3 = 0;
	IF time_day = "Day" THEN time_day1 = 1;
	ELSE time_day1 = 0;
	IF time_day = "Evening" THEN time_day2 = 1;
	ELSE time_day2 = 0;
	IF time_day = "Overnight" THEN time_day3 = 1;
	ELSE time_day3 = 0;
	IF time_day = "Other/special" THEN time_day4 = 1;
	ELSE time_day4 = 0;
RUN;

* I'm also going to create categorical variables to divide physicians by
			pre-experiment outcome variables. A value of 1 is for
			bottom 50% and 2 is for upper 50%. A PROC SQL statement
			starts this off to figure out how the physicians should
			be grouped;
			
PROC SQL;
	CREATE TABLE FriedRep.panelef AS
		SELECT PhysicianID, AVG(overtime), AVG(rvus), AVG(dtd)
		FROM FriedRep.combined
		WHERE month < 3
		GROUP BY PhysicianID
		ORDER BY PhysicianID;
QUIT;

	* The table created in the last step is used to figure out
	how the physicians should be grouped;

DATA FriedRep.restricted;
	SET FriedRep.restricted;
	LENGTH over_cat 3 rvus_cat 3 dtd_cat 3;
	IF PhysicianID IN (15, 16, 3, 10 , 5, 1, 9 , 8) THEN over_cat = 1;
	ELSE over_cat = 2;
	IF PhysicianID IN (9, 8, 4, 15, 13, 3, 5, 16) THEN rvus_cat = 1;
	ELSE rvus_cat = 2;
	IF PhysicianID IN (8, 11, 10, 5, 13, 4, 15, 16) THEN dtd_cat = 1;
	ELSE dtd_cat = 2;
RUN;

* Next I'm going to create a macro that generates the parameter and
		standard error estimates for a single panel;

%MACRO MakeTable4(panel);

* Calculating the values for most of the panel, excluding the tobit
		regression and patients;

ODS GRAPHICS OFF;
ODS EXCLUDE ALL;

PROC REG DATA = work.&panel._data PLOTS = NONE;
	MODEL overtime overtime_dummy trimmed_rvus dtd = scribe_present Phys2-Phys16 
			month4-month11 location2 location3 time_day2-time_day4 
			weekend patients / WHITE;
	ODS OUTPUT ParameterEstimates = work.&panel.;
RUN;

	* The model controls for number of patients in the above regressions, but we
		can't do that for a regression where number of patients is the outcome.
		Thus, I run a separate regression here for that outcome;

PROC REG DATA = work.&panel._data PLOTS = NONE;
	MODEL patients = scribe_present Phys2-Phys16 month4-month11 location2 
			location3 time_day2-time_day4 weekend / WHITE;
	ODS OUTPUT ParameterEstimates = work.&panel._patients;
RUN;

	* Combining these estimates into a single file for ease;
	
DATA FriedRep.Table4_&panel.;
	SET work.&panel. work.&panel._patients;
	IF Variable ^= "scribe_present" THEN DELETE;
RUN;

	* Running a tobit model to calculate the conditional overtime estimate;

PROC QLIM DATA = work.&panel._data PLOTS = NONE;
	MODEL overtime = scribe_present Phys2-Phys16 month4-month11 location2 
			location3 time_day2-time_day4 weekend patients;
	ENDOGENOUS overtime ~ CENSORED (LB = 0);
	ODS OUTPUT ParameterEstimates = FriedRep.Table4_&panel._tobit;
RUN;

ODS EXCLUDE NONE;

	* To calculate the bootstrapped standard errors we're going to need to
			do a few steps. The first is to generate our sub-samples using
			PROC SURVEYSELECT. This will create 100 (as in the paper) new
			subsamples sampled from our data set. 
			
	* This step selects the same number of times as our original data set
			100 times and outputs it to a temporary file;
			
PROC SURVEYSELECT DATA = work.&panel._data NOPRINT SEED = 252
		OUT = work.boot
		METHOD = urs
		SAMPRATE = 1
		OUTHITS
		REPS = 100;
RUN;

	* The next step is to calculate the regression estimate 100 times
			so we can form a distribution. I will need to use ODS to 
			save the results;
			
ODS EXCLUDE ALL;

PROC QLIM DATA = work.boot PLOTS = NONE;
	* This statement does the QLIM step on each of the samples individually;
	BY Replicate;
	MODEL overtime = scribe_present Phys2-Phys16 month4-month11 location2 
			location3 time_day2-time_day4 weekend patients;
	ENDOGENOUS overtime ~ CENSORED (LB = 0);
	ODS OUTPUT ParameterEstimates = work.bootstats;
RUN;

ODS EXCLUDE NONE;

	* Next I'll need to drop observations from the created data set that
			don't correspond to the scribe_present variable;
			
DATA work.bootstats;
	SET work.bootstats;
	IF Parameter ^= "scribe_present" THEN DELETE;
RUN;
	
	* Now that the distribution of estimates has been created, we can use
			it to calculated the bootstrapped standard error;
			
TITLE "Table 4 &panel. tobit bootstrapped standard error";

PROC MEANS DATA = work.bootstats N StdDev;
	VAR Estimate;
RUN;

TITLE;

%MEND MakeTable4;

* Now that the macro is ready, I'm going to make data sets that correspond
		to each panel in Table 4;
		
		* This data set isn't restricted as it uses the entire sample;
		
DATA work.panel_a_data;
	SET FriedRep.restricted;
RUN;

		* This data set restricts the sample to non-overnight shifts only;

DATA work.panel_b_data;
	SET FriedRep.restricted;
	IF time_day3 = 1 THEN DELETE;
RUN;

		* This data set restricts the sample to shifts with 19 or fewer
				patients;

DATA work.panel_c_data;
	SET FriedRep.restricted;
	IF patients > 19 THEN DELETE;
RUN;

		* This data set restricts the sample to shifts with greater than
				19 patients;

DATA work.panel_d_data;
	SET FriedRep.restricted;
	IF patients <= 19 THEN DELETE;
RUN;

* Calling the macro to make panels a through d;

%MakeTable4(panel_a);
%MakeTable4(panel_b);
%MakeTable4(panel_c);
%MakeTable4(panel_d);

* Unfortunately, due to the nature of how panels e and f use more than
		one sample per panel I am forced to do these regressions without
		a macro;
		
	* Calculating the values for panel e;
	
ODS EXCLUDE ALL;

PROC REG DATA = FriedRep.restricted PLOTS = NONE;
	MODEL overtime overtime_dummy = scribe_present Phys2-Phys16 
			month4-month11 location2 location3 time_day2-time_day4 
			weekend patients / WHITE;
	WHERE over_cat = 2;
	ODS OUTPUT ParameterEstimates = work.panel_e_1;
RUN;

PROC REG DATA = FriedRep.restricted PLOTS = NONE;
	MODEL trimmed_rvus = scribe_present Phys2-Phys16 
			month4-month11 location2 location3 time_day2-time_day4 
			weekend patients / WHITE;
	WHERE rvus_cat = 2;
	ODS OUTPUT ParameterEstimates = work.panel_e_2;
RUN;

PROC REG DATA = FriedRep.restricted PLOTS = NONE;
	MODEL dtd = scribe_present Phys2-Phys16 
			month4-month11 location2 location3 time_day2-time_day4 
			weekend patients / WHITE;
	WHERE dtd_cat = 2;
	ODS OUTPUT ParameterEstimates = work.panel_e_3;
RUN;

	* The model controls for number of patients in the above regressions, but we
		can't do that for a regression where number of patients is the outcome.
		Thus, I run a separate regression here for that outcome;

PROC REG DATA = FriedRep.restricted PLOTS = NONE;
	MODEL patients = scribe_present Phys2-Phys16 month4-month11 location2 
			location3 time_day2-time_day4 weekend / WHITE;
	WHERE over_cat = 2;
	ODS OUTPUT ParameterEstimates = work.panel_e_patients;
RUN;

	* Combining the data sets for convenience;

DATA FriedRep.Table4_panel_e;
	SET work.panel_e_1 work.panel_e_2 work.panel_e_3 work.panel_e_patients;
	IF Variable ^= "scribe_present" THEN DELETE;
RUN;

	* Running a tobit model to calculate the conditional overtime estimate;

PROC QLIM DATA = FriedRep.restricted PLOTS = NONE;
	MODEL overtime = scribe_present Phys2-Phys16 month4-month11 location2 
			location3 time_day2-time_day4 weekend patients;
	ENDOGENOUS overtime ~ CENSORED (LB = 0);
	WHERE over_cat = 2;
	ODS OUTPUT ParameterEstimates = FriedRep.Table4_panel_e_tobit;
RUN;

ODS EXCLUDE NONE;

	* To calculate the bootstrapped standard errors we're going to need to
			do a few steps. The first is to generate our sub-samples using
			PROC SURVEYSELECT. This will create 100 (as in the paper) new
			subsamples of of 50 data points;
			
	* This step selects the same number of times as our original data set
			100 times and outputs it to a temporary file;
			
DATA work.panel_e_tobit_data;
	SET FriedRep.restricted;
	IF over_cat = 1 THEN DELETE;
RUN;
			
PROC SURVEYSELECT DATA = work.panel_e_tobit_data NOPRINT SEED = 252
		OUT = work.boot5
		METHOD = urs
		SAMPRATE = 1
		OUTHITS
		REPS = 100;
RUN;

	* The next step is to calculate the regression estimate 100 times
			so we can form a distribution. I will need to use ODS to 
			save the results;			
			
ODS EXCLUDE ALL;

PROC QLIM DATA = work.boot5 PLOTS = NONE;
	* This statement does the QLIM step on each of the samples individually;
	BY Replicate;
	MODEL overtime = scribe_present Phys2-Phys16 month4-month11 location2 
			location3 time_day2-time_day4 weekend patients;
	ENDOGENOUS overtime ~ CENSORED (LB = 0);
	WHERE over_cat = 2;
	ODS OUTPUT ParameterEstimates = work.bootstats5;
RUN;

ODS EXCLUDE NONE;

	* Next I'll need to drop observations from the created data set that
			don't correspond to the scribe_present variable;
			
DATA work.bootstats5;
	SET work.bootstats5;
	IF Parameter ^= "scribe_present" THEN DELETE;
RUN;
	
	* Now that the distribution of estimates has been created, we can use
			it to calculated the bootstrapped standard error;
			
TITLE "Table 4 panel e tobit bootstrapped standard error";

PROC MEANS DATA = work.bootstats5 N StdDev;
	VAR Estimate;
RUN;

TITLE;

* Calculating the values for panel f;

ODS EXCLUDE ALL;

PROC REG DATA = FriedRep.restricted PLOTS = NONE;
	MODEL overtime overtime_dummy = scribe_present Phys2-Phys16 
			month4-month11 location2 location3 time_day2-time_day4 
			weekend patients / WHITE;
	WHERE over_cat = 1;
	ODS OUTPUT ParameterEstimates = work.panel_f_1;
RUN;

PROC REG DATA = FriedRep.restricted PLOTS = NONE;
	MODEL trimmed_rvus = scribe_present Phys2-Phys16 
			month4-month11 location2 location3 time_day2-time_day4 
			weekend patients / WHITE;
	WHERE rvus_cat = 1;
	ODS OUTPUT ParameterEstimates = work.panel_f_2;
RUN;

PROC REG DATA = FriedRep.restricted PLOTS = NONE;
	MODEL dtd = scribe_present Phys2-Phys16 
			month4-month11 location2 location3 time_day2-time_day4 
			weekend patients / WHITE;
	WHERE dtd_cat = 1;
	ODS OUTPUT ParameterEstimates = work.panel_f_3;
RUN;

	* The model controls for number of patients in the above regressions, but we
		can't do that for a regression where number of patients is the outcome.
		Thus, I run a separate regression here for that outcome;

PROC REG DATA = FriedRep.restricted PLOTS = NONE;
	MODEL patients = scribe_present Phys2-Phys16 month4-month11 location2 
			location3 time_day2-time_day4 weekend / WHITE;
	WHERE over_cat = 1;
	ODS OUTPUT ParameterEstimates = work.panel_f_patients;
RUN;

	* Combining the data sets for convenience;

DATA FriedRep.Table4_panel_f;
	SET work.panel_f_1 work.panel_f_2 work.panel_f_3 work.panel_f_patients;
	IF Variable ^= "scribe_present" THEN DELETE;
RUN;

	* Running a tobit model to calculate the conditional overtime estimate;

PROC QLIM DATA = FriedRep.restricted PLOTS = NONE;
	MODEL overtime = scribe_present Phys2-Phys16 month4-month11 location2 
			location3 time_day2-time_day4 weekend patients;
	ENDOGENOUS overtime ~ CENSORED (LB = 0);
	WHERE over_cat = 1;
	ODS OUTPUT ParameterEstimates = FriedRep.Table4_panel_f_tobit;
RUN;

ODS EXCLUDE NONE;

	* To calculate the bootstrapped standard errors we're going to need to
			do a few steps. The first is to generate our sub-samples using
			PROC SURVEYSELECT. This will create 100 (as in the paper) new
			subsamples of of 50 data points;
			
	* This step selects the same number of times as our original data set
			100 times and outputs it to a temporary file;
			
DATA work.panel_f_tobit_data;
	SET FriedRep.restricted;
	IF over_cat = 2 THEN DELETE;
RUN;
			
PROC SURVEYSELECT DATA = work.panel_f_tobit_data NOPRINT SEED = 252
		OUT = work.boot6
		METHOD = urs
		SAMPRATE = 1
		OUTHITS
		REPS = 100;
RUN;

	* The next step is to calculate the regression estimate 100 times
			so we can form a distribution. I will need to use ODS to 
			save the results;			

ODS EXCLUDE ALL;

PROC QLIM DATA = work.boot6 PLOTS = NONE;
	* This statement does the QLIM step on each of the samples individually;
	BY Replicate;
	MODEL overtime = scribe_present Phys2-Phys16 month4-month11 location2 
			location3 time_day2-time_day4 weekend patients;
	ENDOGENOUS overtime ~ CENSORED (LB = 0);
	ODS OUTPUT ParameterEstimates = work.bootstats6;
	WHERE over_cat = 1;
RUN;

ODS EXCLUDE NONE;

	* Next I'll need to drop observations from the created data set that
			don't correspond to the scribe_present variable;
			
DATA work.bootstats6;
	SET work.bootstats6;
	IF Parameter ^= "scribe_present" THEN DELETE;
RUN;
	
	* Now that the distribution of estimates has been created, we can use
			it to calculated the bootstrapped standard error;
			
TITLE "Table 4 panel f tobit bootstrapped standard error";

PROC MEANS DATA = work.bootstats6 N StdDev;
	VAR Estimate;
RUN;

TITLE;

/*******************************************************************************
The last step is to recreate panel a from Table 5. Again, these results will
be entered into the table manually.
*******************************************************************************/

* The first step is to get the logged versions of the outcome variables;

PROC GENMOD DATA = FriedRep.restricted;
	MODEL cpt93010 = scribe_present Phys2-Phys16 
			month4-month11 location2 location3 time_day2-time_day4 
			weekend patients / DIST = POISSON;
RUN;

PROC GENMOD DATA = FriedRep.restricted;
	MODEL splint_codes = scribe_present Phys2-Phys16 
			month4-month11 location2 location3 time_day2-time_day4 
			weekend patients / DIST = POISSON;
RUN;

PROC GENMOD DATA = FriedRep.restricted;
	MODEL obs_codes = scribe_present Phys2-Phys16 
			month4-month11 location2 location3 time_day2-time_day4 
			weekend patients / DIST = POISSON;
RUN;