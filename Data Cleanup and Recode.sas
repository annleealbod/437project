data class.project_data; set work.data1; * Creating a new dataset from the raw 2017_2019_FemRespData.dat;

    * Keep only the variables of interest;
    KEEP CASEID RACE AGER evrmarry HIEDUC ONOWN onown18 PARMARR intact18 MOMFSTCH
         RELRAISD RELIGION ATTND14 LABORFOR TOTINCR EVERSEX GRFSTSX VRY1STAG SEX1MTHD1
         USEFSTSX MTHFSTSX1 PTSB4MAR MAR1CON1 B1PREMAR LIFPRTNR SEDBC discuss_topic1
         discuss_topic2 discuss_topic3 discuss_topic4 discuss_topic5 discuss_topic6
         discuss_topic8 topics_discussed_count premarital;
         
    /* There are 7 different options of sexual topics that the respondent's parents could have taught them.
	1. How to say no to sex
	2. Methods of birth control 
	3. Where to get birth control 
	4. Sexually transmitted diseases 
	5. How to prevent HIV/AIDS 
	6. How to use a condom 
	8. Waiting until marriage
	In this recode step I will make a variable that counts how many total topics the respondent's parents
	discussed with them, and also create a binary yes/no for each topic that discloses whether or not that 
	was a topic that the respondent's parent discussed with them. */

    /* Initialize count to 0 */
    topics_discussed_count = 0;

    /* Initialize variables for each topic to indicate if it was discussed */
    discuss_topic1 = 0;
    discuss_topic2 = 0;
    discuss_topic3 = 0;
    discuss_topic4 = 0;
    discuss_topic5 = 0;
    discuss_topic6 = 0;
    discuss_topic8 = 0;

    /* Create an array for TALKPAR variables */
    array talkpar{7} TALKPAR1-TALKPAR7;

    /* Loop through topics and update count and individual topic variables */
    do i = 1 to dim(talkpar);
        if talkpar[i] ne 95 and talkpar[i] ne 98 and talkpar[i] ne . then do;
            topics_discussed_count = topics_discussed_count + 1;
            
            if TALKPAR[i] = 1 then discuss_topic1 = 1;
            if TALKPAR[i] = 2 then discuss_topic2 = 1;
            if TALKPAR[i] = 3 then discuss_topic3 = 1;
            if TALKPAR[i] = 4 then discuss_topic4 = 1;
            if TALKPAR[i] = 5 then discuss_topic5 = 1;
            if TALKPAR[i] = 6 then discuss_topic6 = 1;
            if TALKPAR[i] = 8 then discuss_topic8 = 1;
        end;
    end;

    drop i; /* Drop the loop variable if not needed in the final dataset */
   
   * Recoding a binary variable that indicates if the respondent had premarital sex;
   	if (PTSB4MAR = 0) or (EVERSEX = 5) then premarital = 0; *if they had 0 sexual partners before marriage, or they've never had sex, then they haven't had premarital sex;
	else if ((EVERMARRY = 0) and (EVERSEX = 1)) or ((PTSB4MAR ge 1) and (PTSB4MAR le 20)) then premarital = 1; *either they haven't been married but have had sex, or they have been married and had 1 or more partners before marriage;
	else premarital = .;

	* Only respondents age 25 or younger could answer the above question, so now I'll filter the data to just that group;
	where AGER le 25;

run;

* Saving data to a CSV for R analysis;

PROC EXPORT DATA=class.project_data
    OUTFILE='/home/u63737772/437Project/project_data.csv'
    DBMS=CSV REPLACE;
RUN;


 
