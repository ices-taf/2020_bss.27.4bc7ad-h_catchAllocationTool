# C Sea bass IV VII input data file
# _SS-V3.24u
# benchmark 2017-18
#  -------------------------
#  -------------------------

1				#_N_Growth_Patterns
1				#_N_Morphs_Within_GrowthPattern(GP)
#_Cond 1 	#_Morph_between/within_stdev_ratio (no read if N_morphs=1)
#_Cond 1 	#vector_Morphdist_(-1_in_first_val_gives_normal_approx)
#
##1	#  N recruitment designs goes here if N_GP*nseas*area>1 #here 1 gp, 4 seasons, 1 area
##0	#  placeholder for recruitment interaction request
#GP seas area for each recruitment assignment
##1 1 1	# example recruitment design element for GP=1, season=1, area=1
#
#_Cond 0 # N_movement_definitions goes here if N_areas > 1
#_Cond 1.0 # first age that moves (real age at begin of season, not integer) also cond on do_migration>0
#_Cond 1 1 1 2 4 10 # example move definition for seas=1, morph=1, source=1 dest=2, age1=4, age2=10
#

#  -------------------------
1 				#_Nblock_Patterns
1 #_blocks_per_pattern
2015 2019 # begin and end years of blocks in first pattern
#

#  -------------------------
0.5				#_fracfemale #? Note sex ratio in bass increases with length.
0 				#_natM_type:_0=1Parm; 1=N_breakpoints;_2=Lorenzen;_3=agespecific;_4=agespec_withseasinterpolate
#0.150	0.150	0.150	0.152	0.163	0.209	0.247	0.256	0.257	0.257	0.257	0.257	0.257	0.257	0.257	0.257	0.257	0.257	0.257	0.257	0.257	0.257	0.257	0.257	0.257	0.257	0.257	0.257	0.257	0.257	0.257

#  -------------------------
1 	# GrowthModel: 1=vonBert with L1&L2; 2=Richards with L1&L2; 3=not implemented; 4=not implemented #note - maguire et al 2008 pg 1270, Downloaded from icesjms.oxfordjournals.org at ICES on October 17, 2011
2 	#_Growth_Age_for_L1
28 	#_Growth_Age_for_L2 (999 to use as Linf)
0 	#_SD_add_to_LAA (set to 0.1 for SS2 V1.x compatibility)
3 	#_CV_Growth_Pattern:  0 CV=f(LAA); 1 CV=F(A); 2 SD=F(LAA); 3 SD=F(A)
1 	#_maturity_option:  1=length logistic; 2=age logistic; 3=read age-maturity matrix by growth_pattern; 4=read age-fecundity; 5=read fec and wt from wtatage.ss
#_placeholder for empirical age-maturity by growth pattern

#  -------------------------
4 	#_First_Mature_Age
1 	#_fecundity option:(1)eggs=Wt*(a+b*Wt);(2)eggs=a*L^b;(3)eggs=a*Wt^b
0 	#_hermaphroditism option:  0=none; 1=age-specific fxn
1 	#_parameter_offset_approach (1=none, 2= M, G, CV_G as offset from female-GP1, 3=like SS2 V1.x)
1 	#_env/block/dev_adjust_method (1=standard; 2=logistic transform keeps in base parm bounds; 3=standard w/ no bound check)

#_growth_parms
#_LO HI INIT PRIOR PR_type SD PHASE env-var use_dev dev_minyr dev_maxyr dev_stddev Block Block_Fxn

#_growth_parms
0.01 0.5 0.24 0.24 -1 0.1 -3 0 0 0 0 0 0 0 		# NatM_p_1_GP_1 #Has a Vestor of Mortality to include the Rec fishing component.
-1 30 19.67 19.67 -1 0.5 -5 0 0 0 0 0 0 0 		# L_at_Amin_GP_1
60 100 80.26 80.26 -1 15 -4 0 0 0 0 0 0 0 		# L_at_Amax_GP_1
0.01 0.2 0.09699 0.09699 -1 0.05 -3 0 0 0 0 0 0 0 	# VonBert_K_GP_1
2 6 3.9 3.9 -1 0.8 6 0 0 0 0 0 0 0 			# CV_young_GP_1
4 10 6.9 6.9 -1 0.8 6 0 0 0 0 0 0 0 			# CV_old_GP_1

# weight-length relationship
-1 1 0.00001296 0.00001296 -1 0.05 -3 0 0 0 0 0 0 0 	# Wtlen_1
2 4 2.969 2.969 -1 0.05 -3 0 0 0 0 0 0 0 		# Wtlen_2

# proportion mature at length
30 50 40.649 40.649 -1 5 -3 0 0 0 0 0 0 0 		# Mat50%
-5 1 -0.33349 -0.33349 -1 0.03764 -3 0 0 0 0 0 0 0 	# Mat_slope

# fecundity option 1, parm values from dissertation (units of millions of eggs per kg)
-3 3 1 1 -1 0.8 -3 0 0 0 0 0 0 0 			# Eg/gm_inter
-3 3 0 0 -1 0.8 -3 0 0 0 0 0 0 0 			# Eg/gm_slope_wt

# recruitment apportionment
0 0 0 0 -1 0 -3 0 0 0 0 0 0 0 				# RecrDist_GP_1
0 0 0 0 -1 0 -3 0 0 0 0 0 0 0 				# RecrDist_Area_1
0 0 0 0 -1 0 -4 0 0 0 0 0 0 0 				# RecrDist_Seas_1 

# cohort growth deviation (fix value at 1 with negative phase; needed for blocks or annual devs)               
0 0 0 0 -1 0 -4 0 0 0 0 0 0 0 				# CohortGrowDev

#
#_Cond 0 #custom_MG-env_setup (0/1)
#_Cond -2 2 0 0 -1 99 -2 		#_placeholder when no MG-environ parameters
#
#_Cond 0 #custom_MG-block_setup (0/1)
#_Cond -2 2 0 0 -1 99 -2 		#_placeholder when no MG-block parameters
#_Cond No MG parm trends
#
#_seasonal_effects_on_biology_parms
0 0 0 0 0 0 0 0 0 0 			#_femwtlen1,femwtlen2,mat1,mat2,fec1,fec2,L1,K
#_Cond -2 2 0 0 -1 99 -2 		#_placeholder when no seasonal MG parameters
#
#-6 	#_MGparm_Dev_Phase
#

#_Spawner-Recruitment
3 	#_SR_function

#_LO HI INIT PRIOR PR_type SD PHASE
1 16 10 5 -1 1 1 			# SR_R0
0.2 0.999 0.999 0.999 -1 0.2 -1 	# SR_steep
0.1 2 0.9 0.9 -1 0.2 -5 		# SR_sigmaR
-5 5 0 0 -1 1 -3 			# SR_envlink
-5 5 0 -0.7 -1 2 -2 			# SR_R1_offset
0 0 0 0 -1 0 -99 			# SR_autocorr

0 	#_SR_env_link
0 	#_SR_env_target_0=none;1=devs;_2=R0;_3=steepness

1 	#do_recdev:  0=none; 1=devvector; 2=simple deviations
1955 	# first year of main recr_devs; early devs can preceed this era
2016 	# last year of main recr_devs; update - Youngest survey age 2gp Endyear-2;
3 	#_recdev phase
1 	# (0/1) to read 13 advanced options
0 							#_recdev_early_start (0=none; neg value makes relative to recdev_start)
-4 							#_recdev_early_phase
0 							#_forecast_recruitment phase (incl. late recr) (0 value resets to maxphase+1)
1 							#_lambda for prior_fore_recr occurring before endyr+1
1973.3 #_last_early_yr_nobias_adj_in_MPD
1982.1 #_first_yr_fullbias_adj_in_MPD
2016.9 #_last_yr_fullbias_adj_in_MPD
2017.6 #_first_recent_yr_nobias_adj_in_MPD
0.915 #_max_bias_adj_in_MPD (1.0 to mimic pre-2009 models)
0 							#_period of cycles in recruitment (N parms read below)
-5 							#min rec_dev
5 							#max rec_dev
0 							# 3 #_read_recdevs
#_end of advanced SR options
#

#Fishing Mortality info
0.2 	# F ballpark for tuning early phases
-2001 	# F ballpark year (neg value to disable)
3 	# F_Method:  1=Pope; 2=instan. F; 3=hybrid (hybrid is recommended)
2.9 	# max F or harvest rate, depends on F_Method

# no additional F input needed for Fmethod 1
# if Fmethod=2; read overall start F value; overall phase; N detailed inputs to read
#0.3 3 0 # if Fmethod=3; read N iterations for tuning for Fmethod 3
5 # N iterations for tuning F in hybrid method (recommend 3 to 7)
#

#_initial_F_parms
#_LO HI INIT PRIOR PR_type SD PHASE
0 2 0.03 0.03 -1 0.5 1 # InitF_OTB_Nets
0 2 0.03 0.03 -1 0.5 1 # InitF_Lines
0 2 0.03 0.03 -1 0.5 1 # InitF_Midwater
0 2 0.03 0.03 -1 0.5 1 # InitF_French
0 2 0.03 0.03 -1 0.5 1 # InitF_Other
0 2 0.03 0.03 -1 0.5 1 # InitF_RecFish
#

# Catchability Specification (Q_setup)
# A=do power: 0=skip, survey is prop. to abundance, 1= add par for non-linearity
# B=env. link: 0=skip, 1= add par for env. effect on Q
# C=extra SD: 0=skip, 1= add par. for additive constant to input SE (in ln space)
# D=type: <0=mirror lower abs(#) fleet, 0=no par Q is median unbiased, 1=no par Q is mean unbiased, 2=estimate par for ln(Q)
# 	   3=ln(Q) + set of devs about ln(Q) for all years. 4=ln(Q) + set of devs about Q for indexyr-1
0 0 0 0 # FISHERY1 UKTrawl_Nets
0 0 0 0	# FISHERY2 UKLines
0 0 0 0	# FISHERY3 UKMidwater
0 0 0 0 # FISHERY4 French
0 0 0 0 # FISHERY5 Other
0 0 0 0 # Fishery6 RecFish
0 0 1 0	# SURVEY AutBass
0 0 1 0 # Survey CGFS1
0 0 1 0 # FR_LPUE

#_Cond 0 #_If q has random component, then 0=read one parm for each fleet with random q; 1=read a parm for each year of index
#_Q_parms(if_any)
# LO HI INIT PRIOR PR_type SD PHASE
0     1   0.1     0.1     -1         99         3       # Q_extraSD_AutBass
0     1   0.1     0.1     -1         99         3       # Q_extraSD_CGFS1 
0     1   0.1     0.1     -1         99         3       # Q_extraSD_LPUE 

#
#_size_selex_types
24 1 0 0 	# 1 UKTrawl_Nets  #_RDM  now all fleets have size selectivity
1 0 0 0		# 2 UKLines
1 0 0 0 	# 3 UKMidwater
24 1 0 0 	# 4 French
15 0 0 4 	# 5 Other
24 0 0 0	# 6 RecFish
24 0 0 0 	# 7 AutBass
24 0 0 0 	# 8 CGFS1
15 0 0 4	# 9 FR_LPUE
#
#_age_selex_types
#_Pattern ___ Male Special
10 0 0 0 	# 1 UKTrawl_Nets
10 0 0 0	# 2 UKLines
10 0 0 0 	# 3 UKMidwater
10 0 0 0 	# 4 French
15 0 0 4 	# 5 Other
10 0 0 0	# 6 RecFish
11 0 0 0 	# 7 AutBass
10 0 0 0 	# 8 CGFS1
15 0 0 4	# 9 FR_LPUE

#_LO HI INIT PRIOR PR_type SD PHASE env-var use_dev dev_minyr dev_maxyr dev_stddev Block Block_Fxn
#UK Trawl_Nets
20 93 45 45 -1 0.05 2 0 0 0 0 0 1 2 # SizeSel_2P_1_OTB	        # PEAK     
-15 4.0 -15 -15 -1 0.05 -3 0 0 0 0 0 1 2 # SizeSel_2P_2_OTB	# TOP      
-1 9.0 3.3 3.3 -1 0.05 3 0 0 0 0 0 1 2 # SizeSel_2P_3_OTB	# ASC-WIDTH
-1 9.0 4.4 4.4 -1 0.05 3 0 0 0 0 0 0 0 # SizeSel_2P_4_OTB	# DSC-WIDTH
-999 9.0 -999 -999 -1 0.05 -2 0 0 0 0 0 0 0 # SizeSel_2P_5_OTB	# INIT     
-999 9.0 -999 -999 -1 0.05 -2 0 0 0 0 0 0 0 # SizeSel_2P_6_OTB	# FINAL    
#UK Trawl_Nets_retention                         
20 50 36 36 -1 0.05 4 0 0 0 0 0 1 2 # retention_1p_OTB	        # Inflection               
0.61 10.01 0.81	0.81 -1 0.05 4 0 0 0 0 0 1 2 # retention_2P_OTB	# Slope                    
0 1 1 1 -1 0.05 -3 0 0 0 0 0 0 0 # retention_2P_OTB	        # Asymptotic retention     
0 0 0 0	-1 0.05 -3 0 0 0 0 0 0 0 # retention_2P_OTB	        # Male offset To inflection
#UK Lines
20 91 39 30 -1 0.05 2 0 0 0 0 0 1 2 # SizeSel_5P_1_Lines
0.01 30 2 5 -1 0.05 3 0 0 0 0 0 1 2 # SizeSel_5P_2_Lines
#UK midwater
20 91 39 30 -1 0.05 2 0 0 0 0 0 0 0 # SizeSel_5P_1_MWT
0.01 30 2 5 -1 0.05 3 0 0 0 0 0 0 0 # SizeSel_5P_2_MWT
#French
20 91 57 57 -1 0.05 2 0 0 0 0 0 1 2 # SizeSel_1P_1_French       # PEAK      
-15 4.0 -15 -15 -1 0.05 -3 0 0 0 0 0 1 2 # SizeSel_1P_1_French	# TOP           
-1.0 9.0 6 6 -1 0.05 3 0 0 0 0 0 1 2 # SizeSel_1P_1_French      # ASC-WIDTH 
-1.0 9.0 9.0 9.0 -1 0.05 -3 0 0 0 0 0 0 0 # SizeSel_1P_1_French # DSC-WIDTH 
-999 9.0 -999 -999 -1 0.05 -2 0 0 0 0 0 0 0 # SizeSel_1P_1_French # INIT          
-999 9.0 9 -999 -1 0.05 -2 0 0 0 0 0 1 2 # SizeSel_1P_1_French # FINAL         
# Freanch retention
30 50 36 36 -1 0.05 4 0 0 0 0 0 1 2 # retention_1p_French		# Inflection               
0.61 10.01 0.81 0.81 -1 0.05 4 0 0 0 0 0 1 2 # retention_2P_French	# Slope                    
0 1 1 1 -1 0.05 -3 0 0 0 0 0 0 0 # retention_2P_French			# Asymptotic retention     
0 0 0 0 -1 0.05 -3 0 0 0 0 0 0 0 # retention_2P_French			# Male offset To inflection
#Rec
20 93 45 45 -1 0.05 2 0 0 0 0 0 0 0 # SizeSel_2P_1_RecFish      # PEAK     
-15 4.0 -15 -15 -1 0.05 -3 0 0 0 0 0 0 0 # SizeSel_2P_1_RecFish # TOP           
-1 9.0 5 5 -1 0.05 3 0 0 0 0 0 0 0 # SizeSel_2P_3_RecFish       # ASC-WIDTH
-1 9.0 4.4 4.4 -1 0.05 3 0 0 0 0 0 0 0 # SizeSel_2P_4_RecFish   # DSC-WIDTH
-999 9.0 -999 -999 -1 0.05 -2 0 0 0 0 0 0 0 # SizeSel_2P_5_RecFish # INIT     
-999 9.0 9 -999 -1 0.05 -2 0 0 0 0 0 0 0 # SizeSel_2P_6_RecFish # FINAL    
#Autbass
19 93 32 32 -1 0.05 2 0 0 0 0 0 0 0 # SizeSel_2P_1_AutBass      # PEAK      
-15 4.0 -15 -6.0 -1 0.05 -3 0 0 0 0 0 0 0 # SizeSel_1_AutBass   # TOP          
-1.0 9.0 3.3 3.3 -1 0.05 3 0 0 0 0 0 0 0 # SizeSel_2P_3_AutBass # ASC-WIDTH 
-1.0 9.0 4.4 4.4 -1 0.05 3 0 0 0 0 0 0 0 # SizeSel_2P_4_AutBass # DSC-WIDTH 
-999 9.0 -999 -999 -1 0.05 -2 0 0 0 0 0 0 0 # SizeSel_2P_5_RecFish # INIT      
-999 9.0 -999 -999 -1 0.05  -2 0 0 0 0 0 0 0 # SizeSel_2P_6_RecFish # FINAL     
#CGFS1
20 93 32 32 -1 0.05 2 0 0 0 0 0 0 0 # SizeSel_2P_1_CGFS1        # PEAK        
-15 4.0 -15 -15 -1 0.05 -3 0 0 0 0 0 0 0 # SizeSel_2P_2_CGFS1   # TOP               
-1.0 9.0 3.3 3.3 -1 0.05 3 0 0 0 0 0 0 0 # SizeSel_2P_3_CGFS1   # ASC-WIDTH   
-1.0 9.0 4.4 4.4 -1 0.05 3 0 0 0 0 0 0 0 # SizeSel_2P_4_CGFS1   # DSC-WIDTH   
-999 9.0 -999 -999 -1 0.05 -2 0 0 0 0 0 0 0 # SizeSel_2P_5_CGFS1 # INIT        
-999 9.0 -999 -999 -1 0.05 -2 0 0 0 0 0 0 0 # SizeSel_2P_6_CGFS1 # FINAL       

2 2 2 2 -1 99 -3 0 0 0 0 0 0 0 			# AgeSel_10P_1_Autumn 2 min age
4 4 4 4 -1 99 -3 0 0 0 0 0 0 0 			# AgeSel_10P_2_Autumn 4 max age

#_Cond 0 #_custom_sel-env_setup (0/1)
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no enviro fxns
1 #_custom_sel-blk_setup (0/1)
## Lo	Hi	Init	Prior	P_type	SD	Phase
##UK trawl selx
20 93 45 45 -1 0.05 2 # UK_Trawl_Net     	# PEAK
-15 4.0 -15 -15 -1 0.05 3 # UK_Trawl_Net	# Top
-1 9.0 3.3 3.3 -1 0.05 3 # UK_Trawl_Net		# ASC-WIDTH
20 50 36 36 -1 0.05 4 # UK_Trawl_Net    	# retention inflection
0.61 10.01 0.81	0.81 -1 0.05 4 # UK_Trawl_Net   # Slope
20 91 39 30 -1 0.05 2 # UK_lines		#
0.01 30 2 5 -1 0.05 3 # UK_lines                #
20 91 57 57 -1 0.05 2 # French                  # PEAK
-15 4.0 -15 -15 -1 0.05 3 # UK_Trawl_Net	# Top
-1.0 9.0 6 6 -1 0.05 3 # French                 # ASC=WIDTH
-999 9.0 -999 -999 -1 0.05 -2 # French          # Final     
30 50 36 36 -1 0.05 4 # French                  # retention inflection
0.61 10.01 0.81 0.81 -1 0.05 4 # French         # Slope


#_Cond No selex parm trends                           
#_Cond -4 # placeholder for selparm_Dev_Phase                          
1 #_env/block/dev_adjust_method (1=standard; 2=logistic trans to keep in base parm bounds; 3=standard w/ no bound check)
#
# Tag loss and Tag reporting parameters go next
0 # TG_custom:  0=no read; 1=read if tags exist
#_Cond -6 6 1 1 2 0.01 -4 0 0 0 0 0 0 0  #_placeholder if no parameters
#
1 #_Variance_adjustments_to_input_values
#_fleet/svy: 1 2 3 4 5 6 7 8 9
0 0 0 0 0 0 0 0 0  			#_add_to_survey_CV                        
0 0 0 0 0 0 0 0	0 			#_add_to_discard_stddev                        
0 0 0 0 0 0 0 0	0  			#_add_to_bodywt_CV                        
0.127597    0.146977    0.153673    0.117151    1	1	1	0.342860    1	#_mult_by_lencomp_N
0.229988    0.120293    0.347828    0.117815    1	1	1	1	1	#_mult_by_agecomp_N
1 1 1 1 1 1 1 1	1 			#_mult_by_size-at-age_N	

#
3 	#_maxlambdaphase
1 	#_sd_offset
#
8 	# number of changes to make to default Lambdas (default value is 1.0)
# Like_comp codes:  1=surv; 2=disc; 3=mnwt; 4=length; 5=age; 6=SizeFreq; 7=sizeage; 8=catch; 
# 9=init_equ_catch; 10=recrdev; 11=parm_prior; 12=parm_dev; 13=CrashPen; 14=Morphcomp; 15=Tag-comp; 16=Tag-negbin
#like_comp fleet/survey  phase  value  sizefreq_method
 4 1 1 0.5 1  #_RDM  reduce emphasis on age and length comp by 50%
 4 2 1 0.5 1
 4 3 1 0.5 1
 4 4 1 0.5 1
 5 1 1 0.5 1
 5 2 1 0.5 1
 5 3 1 0.5 1
 5 4 1 0.5 1
#
# lambdas (for info only; columns are phases)
#  0 0 0 0 #_CPUE/survey:_1
#  1 1 1 1 #_CPUE/survey:_2
#  1 1 1 1 #_CPUE/survey:_3
#  1 1 1 1 #_lencomp:_1
#  1 1 1 1 #_lencomp:_2
#  0 0 0 0 #_lencomp:_3
#  1 1 1 1 #_agecomp:_1
#  1 1 1 1 #_agecomp:_2
#  0 0 0 0 #_agecomp:_3
#  1 1 1 1 #_size-age:_1
#  1 1 1 1 #_size-age:_2
#  0 0 0 0 #_size-age:_3
#  1 1 1 1 #_init_equ_catch
#  1 1 1 1 #_recruitments
#  1 1 1 1 #_parameter-priors
#  1 1 1 1 #_parameter-dev-vectors
#  1 1 1 1 #_crashPenLambda
0 # (0/1) read specs for more stddev reporting
 # 1 1 -1 5 1 5 1 -1 5 # selex type, len/age, year, N selex bins, Growth pattern, N growth ages, NatAge_area(-1 for all), NatAge_yr, N Natages
 # 5 15 25 35 43 # vector with selex std bin picks (-1 in first bin to self-generate)
 # 1 2 14 26 40 # vector with growth std bin picks (-1 in first bin to self-generate)
 # 1 2 14 26 40 # vector with NatAge std bin picks (-1 in first bin to self-generate)
999
