# Test large model compiles (RxODE#117)
rxTest({
  mod <- rxode2("
##### define heart failure parameters. Allow them to increase from baseline over time #####
if (Heart_failure_link == 1) {
R_art0		  = (R_art0_initial-R_art0_initial*heart_failure_resistance_aorta_scale)/exp(sim_time/(24*5))+R_art0_initial*heart_failure_resistance_aorta_scale;
R_per0	  	  = (R_per0_initial-R_per0_initial*heart_failure_resistance_prepheral_scale)/exp(sim_time/(24*5))+R_per0_initial*heart_failure_resistance_prepheral_scale;
R_al0 = R_al0_scale*R_per0;    #0.66*R_per0
R_cap0= R_cap0_scale*R_per0;    #0.25*R_per0
R_vn0 = R_vn0_scale*R_per0;   #0.09*R_per0
R_ven0	  	= (R_ven0_initial-R_ven0_initial*heart_failure_resistance_venous_scale)/exp(sim_time/(24*5))+R_ven0_initial*heart_failure_resistance_venous_scale;
R_art_pulm	= (R_art_pulm_initial-R_art_pulm_initial*heart_failure_resistance_R_art_pulm_scale)/exp(sim_time/(24*5))+R_art_pulm_initial*heart_failure_resistance_R_art_pulm_scale;
R_ven_pulm	= (R_ven_pulm_initial-R_ven_pulm_initial*heart_failure_resistance_R_ven_pulm_scale)/exp(sim_time/(24*5))+R_ven_pulm_initial*heart_failure_resistance_R_ven_pulm_scale;

C_art	    	= (C_art_initial-C_art_initial*heart_failure_compliance_aorta_scale)/exp(sim_time/(24*5))+C_art_initial*heart_failure_compliance_aorta_scale;
C_ven0	  	= (C_ven0_initial-C_ven0_initial*heart_failure_compliance_venous_scale)/exp(sim_time/(24*5))+C_ven0_initial*heart_failure_compliance_venous_scale;
C_pulm_ven  = (C_pulm_ven_initial-C_pulm_ven_initial*heart_failure_compliance_pulm_ven_scale)/exp(sim_time/(24*5))+C_pulm_ven_initial*heart_failure_compliance_pulm_ven_scale;
C_pulm_art  = (C_pulm_art_initial-C_pulm_art_initial*heart_failure_compliance_pulm_art_scale)/exp(sim_time/(24*5))+C_pulm_art_initial*heart_failure_compliance_pulm_art_scale;

cf            = (cf_initial-cf_initial*cf_scale)/exp(sim_time/(24))+cf_initial*cf_scale;
contractility = (contractility_initial-contractility_initial*contractility_HF_scale)/exp(sim_time/(24))+contractility_initial*contractility_HF_scale;

Sodium_protein_filtration_rate_Kf = (Sodium_protein_filtration_rate_Kf_nom-Sodium_protein_filtration_rate_Kf_nom*kf_scale)/exp(sim_time/(24))+Sodium_protein_filtration_rate_Kf_nom*kf_scale;
vascular_responsiveness_scale =  (vascular_responsiveness_scale_nom-vascular_responsiveness_scale_nom*vascular_responsiveness_scale_nom_scale)/exp(sim_time/(24))+vascular_responsiveness_scale_nom*vascular_responsiveness_scale_nom_scale;
}
else {  #If no heart failure, parameters just equation baseline values
R_art0		  = R_art0_initial	;
R_per0		  = R_per0_initial	;
R_al0 = R_al0_scale*R_per0;    #0.66*R_per0
R_cap0= R_cap0_scale*R_per0;    #0.25*R_per0
R_vn0 = R_vn0_scale*R_per0;   #0.09*R_per0
R_ven0		  = R_ven0_initial	;
R_art_pulm	= R_art_pulm_initial;
R_ven_pulm	= R_ven_pulm_initial;
C_art		    = C_art_initial 	;
C_ven0	  	= C_ven0_initial	;
C_pulm_ven  = C_pulm_ven_initial;
C_pulm_art  = C_pulm_art_initial;

cf = cf_initial;
contractility = contractility_initial;
Sodium_protein_filtration_rate_Kf = Sodium_protein_filtration_rate_Kf_nom;
vascular_responsiveness_scale = vascular_responsiveness_scale_nom;
}
CO_nom = CO_nom_initial;

plasma_protein_concentration =plasma_protein_amount / (blood_volume_L * L_dL);
ISF_protein_concentration = ISF_protein_amount / (interstitial_fluid_volume * L_dL);


if(test_ISF_pressure==1){
ISF_pressure =  -0.000000002316554*interstitial_fluid_volume^6
+ 0.000000910096686*interstitial_fluid_volume^5 - 0.000141950241878*interstitial_fluid_volume^4
+ 0.011247538513374*interstitial_fluid_volume^3 - 0.471694546620885*interstitial_fluid_volume^2
+ 9.92251133652023*interstitial_fluid_volume - 81.2426416630404+5.09732;

}else{
ISF_pressure=ISF_pressure_initial;
}

########### feedback of the cardiac output perturbation#################

#If not testing cardiac reserve:
if (Heart_failure_link == 0 & cardiac_reserve == 0  ) {
tissue_autoregulation_signal = max(0.1,1+tissue_autoreg_scale*(Kp_CO*(CO_delayed - CO_nom*CO_species_scale)+Ki_CO*CO_error));
peripheral_resistance_multiplier = disease_effect_on_TPR_peripheral_resistance * B2sna_effect_on_TPR*A1sna_effect_on_TPR*tissue_autoregulation_signal;
peripheral_resistance_multiplier_adjusted = max(dilation_scale,time_TPR_scale*(1+vascular_responsiveness_scale*(peripheral_resistance_multiplier-1)));#*max(0,(-peripheral_resistance_multiplier_adjusted+dilation_scale)/dilation_scale);
heart_rate = HR_heart_rate * BB_HR_effect ;

} else if (Heart_failure_link == 0 & cardiac_reserve == 1){ #If testing cardiac reserve

tissue_autoregulation_signal = max(0.1,1+tissue_autoreg_scale*(Kp_CO*(CO_delayed - CO_nom*CO_species_scale)+Ki_CO*CO_error));
HR_autoregulation_signal = 1+tissue_autoreg_scale*(HRP_CO*(CO_delayed - CO_nom*CO_species_scale)+HRI_CO*CO_error);

heart_rate_multiplier_adjusted = time_HR_scale*(1-HR_autoregulation_signal);
Heart_rate_increasing_ratio =  min(Mag_HR_changing_ratio,(Heart_rate_increasing_ratio + heart_rate_multiplier_adjusted));#*max(0,(-heart_rate_multiplier_adjusted+Mag_HR_changing_ratio)/Mag_HR_changing_ratio);
heart_rate = (HR_heart_rate * BB_HR_effect*(1+Heart_rate_increasing_ratio));

peripheral_resistance_multiplier = disease_effect_on_TPR_peripheral_resistance * B2sna_effect_on_TPR*A1sna_effect_on_TPR*tissue_autoregulation_signal;
peripheral_resistance_multiplier_adjusted = max(dilation_scale,time_TPR_scale*(1+vascular_responsiveness_scale*(peripheral_resistance_multiplier-1)));#*max(0,(-peripheral_resistance_multiplier_adjusted+dilation_scale)/dilation_scale);

} else if (Heart_failure_link == 1 & cardiac_reserve == 0){

tissue_autoregulation_signal = max(0.1,1+tissue_autoreg_scale*(Kp_HF_CO*(CO_delayed - CO_nom*CO_species_scale)+Ki_HF_CO*CO_error));
HR_autoregulation_signal = 1+tissue_autoreg_scale*(HRP_HF_CO*(CO_delayed - CO_nom*CO_species_scale)+HRI_HF_CO*CO_error);

peripheral_resistance_multiplier = disease_effect_on_TPR_peripheral_resistance * B2sna_effect_on_TPR*A1sna_effect_on_TPR*tissue_autoregulation_signal;
peripheral_resistance_multiplier_adjusted =max(dilation_HF_Chronic_scale,(1+vascular_responsiveness_scale*(peripheral_resistance_multiplier-1)));#*max(0,(-peripheral_resistance_multiplier_adjusted+dilation_HF_scale)/dilation_HF_scale);

heart_rate_multiplier_adjusted = (1-HR_autoregulation_signal);
Heart_rate_increasing_ratio = min(Mag_HR_HF_Chronic_ratio,(Heart_rate_increasing_ratio + heart_rate_multiplier_adjusted));#*max(0,(-heart_rate_multiplier_adjusted+Mag_HR_HF_changing_ratio)/Mag_HR_HF_changing_ratio);
heart_rate = (HR_heart_rate * BB_HR_effect*(1+Heart_rate_increasing_ratio));
} else if (Heart_failure_link == 1& cardiac_reserve == 1){

tissue_autoregulation_signal = max(0.1,1+tissue_autoreg_scale*(Kp_HF_CO*(CO_delayed - CO_nom*CO_species_scale)+Ki_HF_CO*CO_error));
HR_autoregulation_signal = 1+tissue_autoreg_scale*(HRP_HF_CO*(CO_delayed - CO_nom*CO_species_scale)+HRI_HF_CO*CO_error);

peripheral_resistance_multiplier = disease_effect_on_TPR_peripheral_resistance * B2sna_effect_on_TPR*A1sna_effect_on_TPR*tissue_autoregulation_signal;
peripheral_resistance_multiplier_adjusted =max(dilation_HF_Acute_scale,(1+vascular_responsiveness_scale*(peripheral_resistance_multiplier-1)));#*max(0,(-peripheral_resistance_multiplier_adjusted+dilation_HF_scale)/dilation_HF_scale);

heart_rate_multiplier_adjusted = (1-HR_autoregulation_signal);
Heart_rate_increasing_ratio = min(Mag_HR_HF_Acute_ratio,(Heart_rate_increasing_ratio + heart_rate_multiplier_adjusted));#*max(0,(-heart_rate_multiplier_adjusted+Mag_HR_HF_changing_ratio)/Mag_HR_HF_changing_ratio);
heart_rate = (HR_heart_rate * BB_HR_effect*(1+Heart_rate_increasing_ratio));
}
########### feedback of the cardiac output perturbation #################


peripheral_resistance =TPR_scale_peripheral_resistance * R_al0*peripheral_resistance_multiplier_adjusted;

arterial_dis_resistance=peripheral_resistance;
capillary_resistance=R_cap0;
venules_resistance=R_vn0;


beat_duration = min_sec / heart_rate ;
beat_time = sim_time/beat_duration - floor(sim_time/beat_duration);
periods = floor(sim_time/beat_duration);

blood_volume = blood_volume_L/1000;
time_step = 0.01;

#Neurohormonal effects - right now they are all turned off   SNA''sympathetic nerve activity''
sna_effect_on_contractility=1;
sna_effect_on_HR = 1;
AngII_effect_on_venous_compliance=1;


# ANP_effect_on_venous_compliance=1;
SNA_effect_on_venous_compliance=1;
B2sna_effect_on_TPR = 1;
A1sna_effect_on_TPR =  1;
Ang_II_effect_on_systemic_resistance =1;
aldo_effect_on_systemic_resistances = 1;
CCB_effect_on_systemic_arterial_resistance = 1;


# ANP_effect_on_peripheral_resistance = 1;
glu_eff_1 = 0;
angII_eff_1 = 0;
aldo_eff_1 = 0;

#Beta-blockers are drugs that bind to beta-adrenoceptors
#and thereby block the binding of norepinephrine and epinephrine to these receptors.
beta_blocker_effect_on_contractility = BB_contractility_effect; # BB_contractility_effect = 1
BP_effect_on_compliance=1;




##### Volume unit conversions #####
LV_volume_mL = LV_volume * m3_mL;
arterial_volume_mL = arterial_volume * m3_mL;
arterial_dis_circulation_volume_mL 	=arterial_dis_circulation_volume *m3_mL;
capillary_circulation_volume_mL	 	=capillary_circulation_volume	 *m3_mL;
venules_circulation_volume_mL		=venules_circulation_volume		 *m3_mL;

############

RV_volume_mL = RV_volume * m3_mL;
pulmonary_arterial_volume_mL = pulmonary_arterial_volume * m3_mL;
venous_volume_mL = venous_volume * m3_mL;
#total_blood_volume_mL = LV_volume_mL + arterial_volume_mL + peripheral_volume_mL + RV_volume_mL + pulmonary_arterial_volume_mL + pulmonary_venous_volume * m3_mL + venous_volume_mL;
total_blood_volume_mL = LV_volume_mL + arterial_volume_mL + arterial_dis_circulation_volume_mL+capillary_circulation_volume_mL+venules_circulation_volume_mL + RV_volume_mL + pulmonary_arterial_volume_mL + pulmonary_venous_volume * m3_mL + venous_volume_mL;



########## Cardiac tissue composition ################
# V_w_0 = 0.00012
# Baseline_Interstitial_Fibrosis = 0.000003
#Baseline_Replacement_Fibrosis = 0.000003
#Baseline_Interstitial_Tissue = 0.000032
#Baseline_Segmental_Fibrosis = 0

baseline_total_myocyte_volume = V_w_0 - Baseline_Interstitial_Fibrosis - Baseline_Replacement_Fibrosis - Baseline_Interstitial_Tissue;  ## baseline myocyte volume determined by V_w_0
baseline_single_myocyte_volume = baseline_total_myocyte_volume/Baseline_Myocyte_Number;
baseline_myocyte_diameter = 2*sqrt(baseline_single_myocyte_volume/(Pi*Baseline_Myocyte_Length));	## baseline myocyte diameter determined by V_w_0 & Baseline_Myocyte_Length, NOT by Baseline_Myocyte_Diameter

myocyte_length = Baseline_Myocyte_Length + change_in_myocyte_length;						## change_in_myocyte_length depends on passive stress levels
myocyte_diameter = baseline_myocyte_diameter + change_in_myocyte_diameter;					## change_in_myocyte_diameter depends on active stress levels - HYPERTROPHY
single_myocyte_volume = myocyte_length * Pi * (myocyte_diameter ^ 2) / 4;					## myocyte volume calculated as a cylinder

#Baseline_Myocyte_Number = 3.3e+9
number_of_live_myocytes = Baseline_Myocyte_Number;
total_myocyte_volume = single_myocyte_volume * number_of_live_myocytes;
total_nonmyocyte_volume = Baseline_Interstitial_Fibrosis + Baseline_Interstitial_Tissue + Baseline_Replacement_Fibrosis;
LV_wall_volume = total_myocyte_volume + total_nonmyocyte_volume;


level_of_hypertrophy = LV_wall_volume / (baseline_total_myocyte_volume + total_nonmyocyte_volume);		## a measure of how much LV wall wolume has grown
outward_growth = LV_cavity_volume / LV_V0_baseline;										## a measure of how much the LV chamber has grown in volume

pct_change_in_myocyte_diameter = change_in_myocyte_diameter / baseline_myocyte_diameter * 100;
pct_change_in_myocyte_length = change_in_myocyte_length / Baseline_Myocyte_Length * 100;


#################################################################################################
#################################################################################################

######### Cardiac Mechanics #########
## Muscle fiber stress and strain are approximately homogeneously distributed, so that they may be approximated by single values.
## Microscopic constitutive laws for fiber stress and radial stress are used to model active and passive fiber stress.

LV_cavity_volume = LV_V0_baseline * (1 + myo_L_scale * change_in_myocyte_length / Baseline_Myocyte_Length) ^ 3 * (1 - myo_D_scale * change_in_myocyte_diameter / baseline_myocyte_diameter) ^ 2;
LV_fiber_stretch =((LV_volume + (LV_wall_volume/3)) / (LV_cavity_volume + (LV_wall_volume / 3)))^0.3333333;

LV_sarcomere_length = ls_0_passive_LV_sarcomere_length * LV_fiber_stretch;
LV_sarcomere_contraction_velocity = (LV_sarcomere_length - LV_sarcomere_length_delayed) / time_step;
contraction_velocity_effect_in_LV = (1 - LV_sarcomere_contraction_velocity / v0_LV_contraction_velocity_effect_in_LV) / (1 + Cv_contraction_velocity_effect_in_LV * LV_sarcomere_contraction_velocity / v0_LV_contraction_velocity_effect_in_LV);

if (LV_sarcomere_length > ls_a0) {
sarcomere_length_effect_in_LV = ((LV_sarcomere_length - ls_a0) / (ls_ar_sarcomere_length_effect_in_LV - ls_a0));
} else {
sarcomere_length_effect_in_LV = 0;
}

chamber_radius = ((LV_cavity_volume * 3 / 4 / Pi) ^ 0.3333333) * m_mm ;			## approximating the LV as a spherical shell
chamber_diameter = 2 * chamber_radius;

outer_radius = (((LV_cavity_volume + LV_wall_volume) * 3 / 4 / Pi) ^ 0.3333333) * m_mm ;
h_wall = outer_radius - chamber_radius;			## wall thickness

h_over_r = h_wall / chamber_radius;				## a measure of the LV chamber growth; it's the ratio of wall thickness to chamber radius


EDV_chamber_radius = ((LV_EDV* 3 / 4 / Pi) ^ 0.33333333) * m_mm;
EDV_chamber_diameter = 2*EDV_chamber_radius;

EDV_outer_radius = (((LV_EDV + LV_wall_volume) * 3 / 4 / Pi) ^ 0.3333333) * m_mm ;
EDV_h_wall = EDV_outer_radius - EDV_chamber_radius;			## wall thickness
EDV_h_over_r = EDV_h_wall / EDV_chamber_radius;

LV_mass = 1000000*LV_wall_volume*1.05 ;			##wall volume*[(cm->m)^3]*density
## density= 1.05 g/cm^3
#LV_mass = ( 0.8*(1.04*((chamber_diameter + h_wall + h_wall)^3 - chamber_diameter^3)) + 0.6 ) / 1000;	# LV mass in g

#### Cardiac excitation ####

RV_twitch_duration = RV_systolic_time_fraction * beat_duration;

t_d = tau_d_LV_twitch_shape;
t_r = tau_r_LV_twitch_shape;
t_twitch = t_r + t_d;

if (beat_time <= t_r) {
sin_signal=(sin(Pi*beat_time/t_twitch))^n_r_LV_twitch_shape;
}else {
sin_signal=(sin(Pi*beat_time/t_twitch))^n_r_LV_twitch_shape;
}

LV_twitch_shape = sin_signal;
if (beat_time < 0) {
LV_twitch_shape = 0;
}
if (beat_time > t_twitch) {
LV_twitch_shape=0;
}

RV_twitch_shape = (sin(Pi * beat_time / RV_twitch_duration)) ^ 2;
if (beat_time < 0) {
RV_twitch_shape=0;
}
if (beat_time > RV_twitch_duration) {
RV_twitch_shape = 0;
}

LV_active_stress = contractility_scale_LV_active_stress * contractility * sigma_ar * sarcomere_length_effect_in_LV * LV_twitch_shape * contraction_velocity_effect_in_LV * beta_blocker_effect_on_contractility *sna_effect_on_contractility;

#Hypertrophy increases ventricular stiffness
hypertrophy_effect_on_Cf = hypertrophy_Cf_slope*(level_of_hypertrophy - 1);
C_f = cf*(1+hypertrophy_effect_on_Cf);


stretch_zero_S = stretch_min_LV_passive_stress_along_fiber - stretch_scale_LV_passive_stress_along_fiber;
if (LV_fiber_stretch >= stretch_zero_S) {
LV_passive_stress_along_fiber = s_f0 * (exp(C_f * (LV_fiber_stretch - stretch_zero_S)) - 1);
} else {
LV_passive_stress_along_fiber = 0;
}


LV_radial_stretch = 1/ (LV_fiber_stretch * LV_fiber_stretch);

if (LV_radial_stretch >= 1) {
LV_passive_radial_stress = s_r0 * (exp(c_r_LV * (LV_radial_stretch - 1)) - 1);
} else {
LV_passive_radial_stress = 0;
}

LV_total_stress = LV_active_stress + LV_passive_stress_along_fiber - 2 * LV_passive_radial_stress;


if (LV_volume > LV_V0_min) {
rel_volume_LV = 1+LV_wall_volume/LV_volume;
} else {
rel_volume_LV = 1+LV_wall_volume/LV_V0_min;
}

LV_pressure = LV_total_stress * log(rel_volume_LV)/ 3;



#################################################################################################
#################################################################################################

#peripheral_pressure = P_ven0 + (peripheral_circulation_volume - V_per0) / C_per;
arterial_dis_pressure = P_al0 + (arterial_dis_circulation_volume-V_al0)/C_al;
capillary_pressure = P_cap0 + (capillary_circulation_volume-V_cap0)/C_cap;
venules_pressure = P_vn0 + (venules_circulation_volume-V_vn0)/C_vn;

# venous_compliance=C_ven0*AngII_effect_on_venous_compliance*ANP_effect_on_venous_compliance*SNA_effect_on_venous_compliance;
venous_compliance=C_ven0*AngII_effect_on_venous_compliance*SNA_effect_on_venous_compliance;
venous_pressure = P_ven0 + (venous_volume - V_ven0) / venous_compliance; ################################################################################# was P_ven0

RV_Cavity_Volume = RV_V0;
RV_wall_volume = V_w_0_RV;
RV_fiber_stretch = ((RV_volume + V_w_0_RV/3) / (RV_Cavity_Volume + RV_wall_volume/3))^(0.333);
RV_sarcomere_length = ls_a0_RV * RV_fiber_stretch;

if (RV_sarcomere_length > ls_a0_RV) {
sarcomere_length_effect_in_RV = (RV_sarcomere_length - ls_a0_RV) / (0.000002 - ls_a0_RV);
} else {
sarcomere_length_effect_in_RV = 0;
}


RV_sarcomere_contraction_velocity = (RV_sarcomere_length - RV_sarcomere_length_delayed) / time_step;
contraction_velocity_effect_in_RV = (1 - RV_sarcomere_contraction_velocity / v0_RV_contraction_velocity_effect_in_RV) / (1 + 0 * RV_sarcomere_contraction_velocity / v0_RV_contraction_velocity_effect_in_RV);
RV_active_stress_multiplier = contractility_RV*sna_effect_on_contractility;
RV_active_stress = contractility_RV * sigma_ar_RV * sarcomere_length_effect_in_RV * RV_twitch_shape * contraction_velocity_effect_in_RV*sna_effect_on_contractility;

RV_radial_stretch = 1/ (RV_fiber_stretch * RV_fiber_stretch);

if (RV_radial_stretch >= 1) {
RV_passive_radial_stress = s_r0_RV * (exp(c_r_RV * (RV_radial_stretch - 1)) - 1);
} else {
RV_passive_radial_stress = 0;
}


if (RV_fiber_stretch >= 1) {
RV_passive_stress_along_fiber = s_f0_RV * (exp(cf_RV * (RV_fiber_stretch - 1)) - 1);
} else {
RV_passive_stress_along_fiber = 0;
}

RV_total_stress = RV_active_stress + RV_passive_stress_along_fiber - 2 * RV_passive_radial_stress;

if (RV_volume > RV_V0_min) {
rel_volume = (1 + RV_wall_volume / RV_volume);
} else {
rel_volume = (1 + RV_wall_volume / RV_V0_min);
}

RV_pressure = RV_total_stress * log(rel_volume) / 3;

######### Circulatory hemodynamics #########
venous_flow = (venules_pressure - venous_pressure) / (R_ven0);

##allow blood volume link between heart and kidney to be turned on/off
if (heart_renal_link == 1) {
venous_volume_target =  blood_volume - LV_volume - arterial_volume - arterial_dis_circulation_volume-capillary_circulation_volume-venules_circulation_volume - RV_volume - pulmonary_arterial_volume - pulmonary_venous_volume;
} else {
venous_volume_target = venous_volume;
}

tricuspid_valve_flow_rate = max((venous_pressure - RV_pressure) / R_r_atrium,min_flux);

pulmonary_arterial_pressure = ( pulmonary_arterial_volume - V_pulm_art0 )/C_pulm_art + P_art0;
pulmonary_venous_pressure = P_ven0 + (pulmonary_venous_volume  - V_pulm_ven0 )/C_pulm_ven;
pulmonary_arterial_blood_flow = (pulmonary_arterial_pressure  - pulmonary_venous_pressure )/ R_ven_pulm ;

dP = RV_pressure - pulmonary_arterial_pressure;
Zn = L_pulm + time_step * R_art_pulm;
pulmonary_blood_flow = (pulmonary_blood_flow_delayed * L_pulm + dP * time_step) / Zn;

pulmonary_valve_flow_rate = max(pulmonary_blood_flow,min_flux);


#Allow regurgitation if pressure differential across mitral valve exceeds threshold (set threshold really high to eliminate any regurgitation)
if (pulmonary_venous_pressure > LV_pressure) {
mitral_valve_flow_rate = max  (  (pulmonary_venous_pressure - LV_pressure)/R_left_atrium  , min_flux ) ;
} else {
if (LV_pressure - pulmonary_venous_pressure < mitral_regurgitation_pressure_diff) {
mitral_valve_flow_rate = min_flux;
} else {
#Allow negative flow back out of the ventricle
mitral_valve_flow_rate = (pulmonary_venous_pressure - LV_pressure)/R_left_atrium;
}

}

pulmonary_valve_flow_rate = max(pulmonary_blood_flow,min_flux);


## arterial_pressure computed without taking account compliance
## arterial_compliance = compliance_scale_arterial_compliance * (C_art ) * BP_effect_on_compliance;
## arterial_pressure = (arterial_volume - V_art0) / arterial_compliance + P_art0 ;

## arterial_pressure computed taking account compliance
## Constants taken from Safar, M. E., et al. Stiffness of carotid artery wall material and blood pressure in humans: application to antihypertensive therapy and stroke prevention. Stroke 31.3 (2000): 782-790.
## We assume a linear effect as follows.
## BP_effect_on_stiffness = (arterial_pressure - 85)*Stiffness_BP_slope;
## Solving the following system

Stiffness0=1/C_art;
arterial_stiffness = Stiffness0*(1+ (MAP_delayed - nominal_map_setpoint)*Stiffness_BP_slope);
arterial_compliance = 1/arterial_stiffness;
arterial_pressure = (arterial_volume - V_art0) / arterial_compliance + P_art0 ;


#peripheral_pressure = P_ven0 + (peripheral_circulation_volume - V_per0) / C_per;
systemic_blood_flow = (arterial_pressure - arterial_dis_pressure) / arterial_dis_resistance;
arterial_dis_blood_flow = (arterial_dis_pressure-capillary_pressure)/capillary_resistance;
capillary_blood_flow = (capillary_pressure-venules_pressure)/venules_resistance;
venules_blood_flow = (venules_pressure-venous_pressure)/R_ven0;
dP_1 = LV_pressure - arterial_pressure;
Zn_1 = L_art + R_art0 * time_step;
aortic_blood_flow = (aortic_blood_flow_delayed * L_art*L_scale + dP_1 * time_step) / Zn_1;

aortic_valve_flow_rate = max(aortic_blood_flow,min_flux);



#############################################
### Relating BNP and NTP to End Diastolic Stress###

BNP = exp(BNP_factor*((LV_EDS+1736)/5.094)+3.14);
NTP = exp((log(BNP)+1.4919)/1.0694);

###Assume ANP proportional to BNP
# ANP = BNP;

## Relate ANP with other variables following Karaaslan et al. Long-term mathematical model involving renal sympathetic nerve activity, arterial pressure, and sodium excretion. Annals of biomedical engineering 33.11 (2005): 1607-1630.
if (heart_renal_link  == 1) {
# Right atrial pressure (Pra) - Frank-Starling law
Pra = 0.2787*exp(CO_delayed*0.2281); # [cardiac output]=l/min, [Pra]=mmHg
# Normalized atrial natriuretic peptide concentration
Canp = max(0,7.427 - 6.554 / ( 1 + exp( Pra - 3.762) ) + deltaCanp); # For human beings the actual value at normal conditions is about 36 ng/l
lambdaANP = -0.1*Canp+1.1199;
} else {
lambdaANP = 1;
}


#############################################
### Find Diastolic and Systolic values ###

if (beat_time >= (1 - .01/beat_duration) && beat_time < 1) {
LV_pressure_diastolic_max = LV_pressure;
LV_stress_diastolic_max = LV_passive_stress_along_fiber;
LV_volume_maximum = LV_volume;

} else {
LV_pressure_diastolic_max = LV_EDP;
LV_stress_diastolic_max = LV_EDS;
LV_volume_maximum = LV_EDV;
}

LV_EDP_old = LV_pressure_diastolic_max;
LV_EDS_old = LV_stress_diastolic_max;
LV_EDV_old = LV_volume_maximum;



## Method to find Systolic and Diastolic blood pressure
## use current and last two time steps to find local maxima and minima
if (arterial_pressure_delayed < arterial_pressure_bigger_delay) {
systemic_pressure_minimum_1 = arterial_pressure_delayed;
} else {
systemic_pressure_minimum_1 = diastolic_pressure;
}
if (arterial_pressure_delayed < arterial_pressure) {
systemic_pressure_minimum = systemic_pressure_minimum_1;
} else {
systemic_pressure_minimum = diastolic_pressure;
}

if (arterial_pressure_delayed > arterial_pressure_bigger_delay) {
systemic_pressure_maximum_1 = arterial_pressure_delayed;
} else {
systemic_pressure_maximum_1 = systolic_pressure;
}
if (arterial_pressure_delayed > arterial_pressure) {
systemic_pressure_maximum= systemic_pressure_maximum_1;
} else {
systemic_pressure_maximum= systolic_pressure;
}

systolic_pressure_old = systemic_pressure_maximum;
diastolic_pressure_old = systemic_pressure_minimum;



#############################################################
### Find Peak systolic stress in the LV ###

if (beat_time >= (t_r*0.8) && beat_time < (t_r*0.85)) {
LV_peak_stress = LV_active_stress;
} else {
LV_peak_stress = LV_active_stress_peak;
}

## fixes a problem where it was finding peaks that were very close to zero
if (LV_active_stress > 1) {
LV_active_stress_peak_old = LV_peak_stress;
} else {
LV_active_stress_peak_old = LV_active_stress_peak;
}


################################Find Capillary and Venous Pressures

###Method to compute the systolic and diastolic venous_pressure
if (venous_pressure_delayed < venous_pressure_bigger_delay) {
venous_pressure_minimum_1 = venous_pressure_delayed;
} else {
venous_pressure_minimum_1 = venous_diastolic_pressure;
}
if (venous_pressure_delayed < venous_pressure) {
venous_pressure_minimum = venous_pressure_minimum_1;
} else {
venous_pressure_minimum = venous_diastolic_pressure;
}

if (venous_pressure_delayed > venous_pressure_bigger_delay) {
venous_pressure_maximum_1 = venous_pressure_delayed;
} else {
venous_pressure_maximum_1 = venous_systolic_pressure;
}
if (venous_pressure_delayed > venous_pressure) {
venous_pressure_maximum= venous_pressure_maximum_1;
} else {
venous_pressure_maximum= venous_systolic_pressure;
}

venous_systolic_pressure_old = venous_pressure_maximum;
venous_diastolic_pressure_old = venous_pressure_minimum;

###Method to compute the systolic and diastolic peripheral_pressure

if (capillary_pressure_delayed < capillary_pressure_bigger_delay) {
capillary_pressure_minimum_1 = capillary_pressure_delayed;
} else {
capillary_pressure_minimum_1 = capillary_diastolic_pressure;
}
if (capillary_pressure_delayed < capillary_pressure) {
capillary_pressure_minimum = capillary_pressure_minimum_1;
} else {
capillary_pressure_minimum = capillary_diastolic_pressure;
}

if (capillary_pressure_delayed > capillary_pressure_bigger_delay) {
capillary_pressure_maximum_1 = capillary_pressure_delayed;
} else {
capillary_pressure_maximum_1 = capillary_systolic_pressure;
}
if (capillary_pressure_delayed > capillary_pressure) {
capillary_pressure_maximum= capillary_pressure_maximum_1;
} else {
capillary_pressure_maximum= capillary_systolic_pressure;
}

capillary_systolic_pressure_old = capillary_pressure_maximum;
capillary_diastolic_pressure_old = capillary_pressure_minimum;



#Calculate mean pressures

mean_capillary_pressure = (capillary_systolic_pressure/3+capillary_diastolic_pressure*2/3)*Pa_mmHg;



if (heart_renal_link  == 1) {
mean_arterial_pressure_MAP = (systolic_pressure/3+diastolic_pressure*2/3)*Pa_mmHg;
mean_venous_pressure = (venous_systolic_pressure+venous_diastolic_pressure*2)/3*Pa_mmHg;
} else {
mean_arterial_pressure_MAP = nominal_map_setpoint;
mean_venous_pressure = P_venous;
}




#################################################################################################
#################################################################################################

######################### Hypertrophy ########################################
## growth occurs when the peak of the active stress exceeds the threshhold
## use the same method as above to locate the peaks of the active stress

######################### Hypertrophy ########################################

if (LV_active_stress_peak > LV_active_stress_threshhold) {		## myocyte diameter grows if peak active stress exceeds the threshhold
kD_hypertrophy =  (kD_HYPERTROPHY*C_renal_CV_timescale) * max(0, ((max_myocyte_diameter_increase) - change_in_myocyte_diameter)/(max_myocyte_diameter_increase));	## progression of hypertophic growth - myocyte diameter increase
} else {																							## there is a limit on how much the diameter of the myocyte can increase (right now: 40%)
kD_hypertrophy = (kD_HYPERTROPHY*C_renal_CV_timescale);		## regression of hypertrophic growth (negative will come from ODE below)
}

if (LV_EDS > LV_passive_stress_along_fiber_threshhold) {
kL_hypertrophy =  (kL_HYPERTROPHY*C_renal_CV_timescale) * max(0, ((max_myocyte_length_increase) - change_in_myocyte_length)/(max_myocyte_length_increase));		## progression of growth - myocyte length increase
} else {              ##adding a length threshhold to limit volumetric remodeling
kL_hypertrophy = 0;		## myocyte length does not decrease once stretched
}

#################################################################################################
#################################################################################################

LVID=((6*LV_EDV)/3.14159)^(1/3);

#############################################################################


############################ Kidney Model ###############################################

#Disease effects on nephrons

#For now we limit glomeruli loss to 30% of Kf damage - somewhat arbitrary
number_of_functional_glomeruli = baseline_nephrons*(1 - 0.3*disease_effects_decreasing_Kf);

#Assume that other nephrons are lost due to tubular effects, which do not affect the glomerulus
number_of_functional_tubules = baseline_nephrons*(1-disease_effect_on_nephrons);


###AT1-bound AngII constricts the preafferent, afferent, and efferent arterioles
AT1_preaff_int = 1 - AT1_preaff_scale/2;
AT1_effect_on_preaff = AT1_preaff_int + AT1_preaff_scale/(1+exp(-(AT1_bound_AngII - nominal_equilibrium_AT1_bound_AngII)/AT1_preaff_slope));

AT1_aff_int = 1 - AT1_aff_scale/2;
AT1_effect_on_aff = AT1_aff_int + AT1_aff_scale/(1+exp(-(AT1_bound_AngII - nominal_equilibrium_AT1_bound_AngII)/AT1_aff_slope));

AT1_eff_int = 1 - AT1_eff_scale/2;
AT1_effect_on_eff = AT1_eff_int + AT1_eff_scale/(1+exp(-(AT1_bound_AngII - nominal_equilibrium_AT1_bound_AngII)/AT1_eff_slope));


# ANP_aff_int = 1 + ANP_aff_scale/2;
# ANP_effect_on_aff = ANP_aff_int - ANP_aff_scale/(1+exp(-(ANP - nom_ANP)/ANP_aff_slope));

### RSNA constricts the preafferent vasculature
rsna_preaff_int = 1 - rsna_preaff_scale/2;
rsna_effect_on_preaff = rsna_preaff_int + rsna_preaff_scale/(1+exp(-(renal_sympathetic_nerve_activity - nom_rsna)/rsna_preaff_slope));

### ANP may dilate the preafferent, afferent, and/or efferent arterioles
# ANP_preaff_int = 1 + ANP_preaff_scale/2;
# ANP_effect_on_preaff = ANP_preaff_int - ANP_preaff_scale/(1+exp(-(normalized_atrial_NP_concentration - nom_ANP)/ANP_preaff_slope));

# ANP_aff_int = 1 + ANP_aff_scale/2;
# ANP_effect_on_aff = ANP_aff_int - ANP_aff_scale/(1+exp(-(normalized_atrial_NP_concentration - nom_ANP)/ANP_aff_slope));

# ANP_eff_int = 1 + ANP_eff_scale/2;
# ANP_effect_on_eff= ANP_eff_int - ANP_eff_scale/(1+exp(-(normalized_atrial_NP_concentration - nom_ANP)/ANP_eff_slope));


######Preafferent Resistance


if(renal_blood_flow_L_min_delayed < 1 & interstitial_fluid_volume < 15){
IF_Venous_RIHP_Effect_int = 1- IF_Venous_RIHP_Effect_scale/2;
IF_Venous_RIHP_Effect =(IF_Venous_RIHP_Effect_int+IF_Venous_RIHP_Effect_scale/(1+exp(-(renal_blood_flow_L_min_delayed-nom_renal_blood_flow_L_min)/IF_Venous_RIHP_Effect_slope)));
}else{
IF_Venous_RIHP_Effect=1;
}


preaff_arteriole_signal_multiplier = AT1_effect_on_preaff*rsna_effect_on_preaff*preafferent_pressure_autoreg_signal*CCB_effect_on_preafferent_resistance;
preaff_arteriole_adjusted_signal_multiplier = (1/(1+exp(preaff_signal_nonlin_scale*(1-preaff_arteriole_signal_multiplier)))+0.5);
preafferent_arteriole_resistance = IF_Venous_RIHP_Effect*nom_preafferent_arteriole_resistance*preaff_arteriole_adjusted_signal_multiplier;

###### Afferent Arteriole Resistance
#The afferent arteriole responses the tubuloglomerular feedback (calculated later), as well as to AT1-bound AngII and ANP.
#It may respond myogenically as well. Some studies suggest the upstream portion responds myogenically while the distal portion responds to TGF. Thus, one could consider the
#myogenically responsive portion as part of the preafferent resistance.
#The dilation/constriction of the arterioles is limited, and thus the total combined effect of all regulators must saturate

# afferent_arteriole_signal_multiplier = tubulo_glomerular_feedback_effect * AT1_effect_on_aff * ANP_effect_on_aff*glomerular_pressure_autoreg_signal*CCB_effect_on_afferent_resistance;
afferent_arteriole_signal_multiplier = tubulo_glomerular_feedback_effect * AT1_effect_on_aff *glomerular_pressure_autoreg_signal*CCB_effect_on_afferent_resistance;
afferent_arteriole_adjusted_signal_multiplier = (1/(1+exp(afferent_signal_nonlin_scale*(1-afferent_arteriole_signal_multiplier)))+0.5);
afferent_arteriole_resistance = IF_Venous_RIHP_Effect*nom_afferent_arteriole_resistance*afferent_arteriole_adjusted_signal_multiplier*(1-ANP_effect_on_Arterial_Resistance);

###### Efferent Arteriole Resistance
#The efferent arteriole responses to AT1-bound AngII and ANP.
#The dilation/constriction of the arterioles is limited, and thus the total combined effect of all regulators must saturate
#efferent_arteriole_signal_multiplier = AT1_effect_on_eff * ANP_effect_on_eff *CCB_effect_on_efferent_resistance;

efferent_arteriole_signal_multiplier = AT1_effect_on_eff * CCB_effect_on_efferent_resistance;
efferent_arteriole_adjusted_signal_multiplier = 1/(1+exp(efferent_signal_nonlin_scale*(1-efferent_arteriole_signal_multiplier)))+0.5;
efferent_arteriole_resistance =  IF_Venous_RIHP_Effect*nom_efferent_arteriole_resistance*efferent_arteriole_adjusted_signal_multiplier; #

######Peritubular Resistance
#Autoregulation of peritubular resistance allows RBF to be autoregulated separately from GFR
#This is exploratory for now. By default, this effect is turned off by setting RBF_autoreg_scale to zero
RBF_autoreg_int = 1-RBF_autoreg_scale/2;
peritubular_autoreg_signal = RBF_autoreg_int + RBF_autoreg_scale/(1+exp((nom_renal_blood_flow_L_min - renal_blood_flow_L_min_delayed)/RBF_autoreg_steepness));
autoregulated_peritubular_resistance = peritubular_autoreg_signal*nom_peritubular_resistance;

###### Renal Vascular Resistance
renal_vascular_resistance = (preafferent_arteriole_resistance + (afferent_arteriole_resistance + efferent_arteriole_resistance) / number_of_functional_glomeruli + autoregulated_peritubular_resistance);

###Renal blood flow
renal_blood_flow_L_min = ((mean_arterial_pressure_MAP - P_venous) / renal_vascular_resistance);
renal_blood_flow_ml_hr = renal_blood_flow_L_min * 1000 * 60;


###Renal Vasculature Pressures
preafferent_pressure = mean_arterial_pressure_MAP - renal_blood_flow_L_min*preafferent_arteriole_resistance;
glomerular_pressure = (mean_arterial_pressure_MAP  - renal_blood_flow_L_min * (preafferent_arteriole_resistance + afferent_arteriole_resistance / number_of_functional_glomeruli));
postglomerular_pressure = (mean_arterial_pressure_MAP  - renal_blood_flow_L_min * (preafferent_arteriole_resistance + (afferent_arteriole_resistance+efferent_arteriole_resistance) / number_of_functional_glomeruli));


#Autoregulatory signals for preafferent and afferent resistances
preaff_autoreg_int = 1 - preaff_autoreg_scale/2;
preafferent_pressure_autoreg_function = preaff_autoreg_int+preaff_autoreg_scale/(1+exp((nom_preafferent_pressure - preafferent_pressure)/myogenic_steepness));
gp_autoreg_int = 1 - gp_autoreg_scale/2;
glomerular_pressure_autoreg_function = gp_autoreg_int+gp_autoreg_scale/(1+exp((nom_glomerular_pressure - glomerular_pressure)/myogenic_steepness));

######################## Glomerular Filtration ######################

# Assume glomerulosclerosis causes a decrease in Kf over time, and also a loss of the renal vasculature (afferent and efferent arterioles)
# as glomeruli become completely sclerotic

#Glomerular hypertrophy resulting in increased surface area and thus increased Kf is assumed to occur
#in response to elevated glomerular pressure. A 2 mmHg buffer is built in (i.e. glomerular pressure must be at least 2 mmHg above normal for hypertrophy to begin
#The increase in Kf saturates and cannot exceed the fractional increase set by maximal_glom_surface_area_increase
GP_effect_increasing_Kf = (maximal_glom_surface_area_increase - disease_effects_increasing_Kf) * max(glomerular_pressure/(nom_glomerular_pressure+2) - 1,0) / (T_glomerular_pressure_increases_Kf/C_renal_CV_timescale);
glomerular_hydrostatic_conductance_Kf = nom_Kf*(1+disease_effects_increasing_Kf)*(1-disease_effects_decreasing_Kf);

###Glomerular Fitlration Rate
#Calculation of P_bowmans are described later

net_filtration_pressure = glomerular_pressure - oncotic_pressure_difference - P_bowmans;

if (net_filtration_pressure <= 0) { #Prevent crashing if NFP becomes negative
SNGFR_nL_min = 0.001;
} else {
SNGFR_nL_min = glomerular_hydrostatic_conductance_Kf * (glomerular_pressure - oncotic_pressure_difference - P_bowmans);
}

GFR =  (SNGFR_nL_min / 1000 / 1000000 * number_of_functional_tubules);
GFR_ml_min = GFR * 1000;

filtration_fraction = GFR/renal_blood_flow_L_min;

serum_creatinine_concentration = serum_creatinine/blood_volume_L;
creatinine_clearance_rate = GFR_ml_min * dl_ml * serum_creatinine_concentration; #Units: mg/min

####################### Protein filtering ####################
GPdiff = max(0, glomerular_pressure - (nom_GP_seiving_damage));
GP_effect_on_Seiving = Emax_seiving * GPdiff ^ Gamma_seiving / (GPdiff ^ Gamma_seiving + Km_seiving ^ Gamma_seiving);

#Dean and Lazzara 2006 - Seiving coefficient decreases as GFR increases

nom_glomerular_albumin_sieving_coefficient = seiving_inf/(1-(1-seiving_inf)*exp(-c_albumin*SNGFR_nL_min));
glomerular_albumin_sieving_coefficient = nom_glomerular_albumin_sieving_coefficient*(1 + GP_effect_on_Seiving);

SN_albumin_filtration_rate = plasma_albumin_concentration*SNGFR_nL_min*1e-6*glomerular_albumin_sieving_coefficient; #mg/min

SN_albumin_excretion_rate = max(0, SN_albumin_filtration_rate - SN_albumin_reabsorptive_capacity)+nom_albumin_excretion_rate;
albumin_excretion_rate = SN_albumin_excretion_rate*number_of_functional_tubules;

#albumin_filtation_rate = plasma_albumin_concentration*SNGFR_nL_min*glomerular_albumin_sieving_coefficient;
#albumin_excretion_rate = max(0, albumin_filtation_rate - max_PT_albumin_reabsorption_rate);



###Oncotic pressure difference
#Landis Pappenheimer equation used to calculate oncotic pressure at entrance and exit to glomerulus
#Oncotic pressure is approximated as varying linearly along the glomerulus. Oncotic pressure in the Bowman's space is zero
#Thus the average pressure difference is the average of the entrance and exit oncotic pressure
#We do not consider filtration equilibrium
Oncotic_pressure_in = 1.629*plasma_protein_concentration+0.2935*(plasma_protein_concentration^2);
SNRBF_nl_min = 1e6*1000*renal_blood_flow_L_min/number_of_functional_glomeruli;
plasma_protein_concentration_out = (SNRBF_nl_min*plasma_protein_concentration-SN_albumin_filtration_rate)/(SNRBF_nl_min-SNGFR_nL_min);
Oncotic_pressure_out = 1.629*plasma_protein_concentration_out+0.2935*(plasma_protein_concentration_out^2);
oncotic_pressure_avg = (Oncotic_pressure_in+Oncotic_pressure_out)/2;



##################### Plasma sodium concentration and vasopressin secretion
###Plasma sodium concentration
Na_concentration = sodium_amount / blood_volume_L;
IF_Na_concentration = IF_sodium_amount/interstitial_fluid_volume;

sodium_storate_rate = Q_Na_store*((max_stored_sodium - stored_sodium)/max_stored_sodium)*(IF_Na_concentration - ref_Na_concentration);

###Control of vasopressin secretion
#A proportional-integral controller is used to ensure there is no steady state error in sodium concentration
#Relative gains of the P and I controller must be chosen carefully.
#In order to permit a steady-state error, the integral controller can be removed. But care should be given then in choosing the proportional gain
Na_water_controller = Na_controller_gain*(Kp_VP*(Na_concentration - ref_Na_concentration)+Ki_VP*Na_concentration_error);

###Vasopressin
#Vasopressin is critical in the model, because it allows water excretion to be decoupled from sodium excretion in the collecting duct
normalized_vasopressin_concentration = 1 + Na_water_controller;
vasopressin_concentration = nominal_vasopressin_conc * normalized_vasopressin_concentration;

#Effect of vasopressin on water intake
water_intake_vasopressin_int = 1-water_intake_vasopressin_scale/2;
water_intake = water_intake_species_scale*(nom_water_intake/60/24)*(water_intake_vasopressin_int + water_intake_vasopressin_scale/(1+exp((normalized_vasopressin_concentration_delayed-1)/water_intake_vasopressin_slope)));
daily_water_intake = (water_intake * 24 * 60);


##################### Tubular Flow and Reabsorption######################

#Length of tubular segments
L_pt_s1 = L_pt_s1_nom*(1+tubular_length_increase);
L_pt_s2 = L_pt_s2_nom*(1+tubular_length_increase);
L_pt_s3 = L_pt_s3_nom*(1+tubular_length_increase);
Dc_pt = Dc_pt_nom*(1+tubular_diameter_increase);
L_pt = L_pt_s1+L_pt_s2 + L_pt_s3;

SN_filtered_Na_load = (SNGFR_nL_min / 1000 / 1000000)*Na_concentration;
filtered_Na_load = SN_filtered_Na_load*number_of_functional_tubules;

######Regulatory effects on reabsorption
###Pressure natriuresis effects


pressure_natriuresis_f_int = 1- pressure_natriuresis_f_scale/2;
pressure_natriuresis_p_int = 1- pressure_natriuresis_p_scale/2;

pressure_natriuresis_signal_f = ((pressure_natriuresis_f_int+pressure_natriuresis_f_scale/(1+exp(-(renal_blood_flow_L_min_delayed-nom_renal_blood_flow_L_min)/pressure_natriuresis_f_slope))));  #+0.85 +0.3

pressure_natriuresis_signal_p =(pressure_natriuresis_p_int+pressure_natriuresis_p_scale/(1+exp(-(RIHP_delayed-RIHP0)/pressure_natriuresis_p_slope)));

pressure_natriuresis_IF_int = 1- pressure_natriuresis_IF_scale/2;
pessure_natriuresis_signal_IF = ((pressure_natriuresis_IF_int+pressure_natriuresis_IF_scale/(1+exp(-(Net_oncotic_pressure_diff-Nom_Net_oncotic_pressure_diff)/pressure_natriuresis_IF_slope))));


pressure_natriuresis_signal = pressure_natriuresis_signal_f*pressure_natriuresis_signal_p*pessure_natriuresis_signal_IF;

pressure_natriuresis_PT_int = 1 - pressure_natriuresis_PT_scale/2;
pressure_natriuresis_PT_effect = max(0.001,pressure_natriuresis_PT_int + pressure_natriuresis_PT_scale / (1 + exp(pressure_natriuresis_signal-1)));

pressure_natriuresis_LoH_int = 1 - pressure_natriuresis_LoH_scale/2;
pressure_natriuresis_LoH_effect = max(0.001,pressure_natriuresis_LoH_int + pressure_natriuresis_LoH_scale / (1 + exp((postglomerular_pressure_delayed - RIHP0) / pressure_natriuresis_LoH_slope)));

pressure_natriuresis_DCT_magnitude = max(0,pressure_natriuresis_DCT_scale );
pressure_natriuresis_DCT_int = 1 - pressure_natriuresis_DCT_magnitude/2;
pressure_natriuresis_DCT_effect = max(0.001,pressure_natriuresis_DCT_int + pressure_natriuresis_DCT_magnitude/ (1 + exp((postglomerular_pressure_delayed - RIHP0) / pressure_natriuresis_DCT_slope)));

pressure_natriuresis_CD_magnitude = max(0,pressure_natriuresis_CD_scale *(1+disease_effects_decreasing_CD_PN));
pressure_natriuresis_CD_int = 1 - pressure_natriuresis_CD_magnitude/2;
pressure_natriuresis_CD_effect = max(0.001,pressure_natriuresis_CD_int + pressure_natriuresis_CD_magnitude/ (1 + exp(pressure_natriuresis_signal-1)));

#smax(0.001,pressure_natriuresis_signal);

###AT1-bound AngII effect on PT reabsorption
AT1_PT_int = 1 - AT1_PT_scale/2;
AT1_effect_on_PT = AT1_PT_int + AT1_PT_scale/(1+exp(-(AT1_bound_AngII - nominal_equilibrium_AT1_bound_AngII)/AT1_PT_slope));
### Aldosterone effect on DCT and CD reabsorption



### RSNA effect on PT and CD sodium reabsorption
#RSNA effect on CD is turned off by default
rsna_PT_int = 1 - rsna_PT_scale/2;

#######NEED To either change rsna_delayed consistently throughout or revert back
rsna_effect_on_PT = 1;
rsna_CD_int = 1 - rsna_CD_scale/2;
rsna_effect_on_CD= rsna_CD_int + rsna_CD_scale/(1+exp((1 - renal_sympathetic_nerve_activity)/rsna_CD_slope));


###Aldosterone effect on distal and collecting duct sodium reabsorption
aldosterone_concentration = normalized_aldosterone_level* nominal_aldosterone_concentration;
Aldo_MR_normalised_effect = normalized_aldosterone_level*(1 - pct_target_inhibition_MRA);

aldo_DCT_int = 1 - aldo_DCT_scale/2;
aldo_effect_on_DCT = aldo_DCT_int + aldo_DCT_scale/(1+exp((1 - Aldo_MR_normalised_effect)/aldo_DCT_slope));

aldo_CD_int = 1 - aldo_CD_scale/2;
aldo_effect_on_CD= aldo_CD_int + aldo_CD_scale/(1+exp((1 - Aldo_MR_normalised_effect)/aldo_CD_slope));

###ANP effect on collecting duct sodium reabsorption
# anp_CD_int = 1 - anp_CD_scale/2;
# anp_effect_on_CD= anp_CD_int + anp_CD_scale/(1+exp((1 - normalized_atrial_NP_concentration)/anp_CD_slope));

#Assume insulin has effect on NHE3. Use RUGE as surrogate for insulin effect. When RUGE goes up, insulin effect goes down.
NHE3inhib = Anhe3*RUGE_delayed;
pt_multiplier = AT1_effect_on_PT * rsna_effect_on_PT *pressure_natriuresis_PT_effect*(1-NHE3inhib);

e_pt_sodreab = min(1,nominal_pt_na_reabsorption_nonSGLT * pt_multiplier);# AT1_effect_on_PT * rsna_effect_on_PT *pressure_natriuresis_PT_effect;


e_dct_sodreab = min(1,nominal_dt_na_reabsorption * aldo_effect_on_DCT*pressure_natriuresis_DCT_effect ); #HCTZ_effect_on_DT_Na_reabs

# cd_multiplier = anp_effect_on_CD *aldo_effect_on_CD*rsna_effect_on_CD*pressure_natriuresis_CD_effect;
cd_multiplier = aldo_effect_on_CD*rsna_effect_on_CD*pressure_natriuresis_CD_effect;
#Ensure that CD reabsorption smoothly approaches upper limit set by max_cd_reabs_rate
cd_scale = max_cd_reabs_rate/nominal_cd_na_reabsorption-1;
#if (cd_multiplier > 1) {
#	cd_multiplier = 1+cd_scale - cd_scale*exp((1-cd_multiplier)/.028);
#}
e_cd_sodreab = min(0.9999,nominal_cd_na_reabsorption*cd_multiplier*lambdaANP);

##################################### Proximal Tubule #########################################
###Glucose Filtration and reabsorption in PT
#Assume glucose reabsorption depends only on availability of SGLT1/2
#Assume constant amount of reabsorption per unit length through SGLT2 in convoluted PT
#Assume constant amount of reabsorption per unit length through SGLT1 in straight/recta PT

#Chosen so that UGE becomes non-zero for plasma_glucose concentration ~8.5 mmol/l
glucose_reabs_per_unit_length_s1 = nom_glucose_reabs_per_unit_length_s1*diabetic_adaptation*SGLT2_inhibition_delayed*(1+RTg_compensation);
glucose_reabs_per_unit_length_s2 = nom_glucose_reabs_per_unit_length_s2*diabetic_adaptation*SGLT2_inhibition_delayed*(1+RTg_compensation);
glucose_reabs_per_unit_length_s3 = nom_glucose_reabs_per_unit_length_s3*diabetic_adaptation*(1+RTg_compensation)*SGLT1_inhibition;

SN_filtered_glucose_load = glucose_concentration*SNGFR_nL_min / 1000 / 1000000;  #mmol/min
glucose_pt_out_s1 = max(0,SN_filtered_glucose_load-glucose_reabs_per_unit_length_s1*L_pt_s1); #mmol/min
glucose_pt_out_s2 = max(0,glucose_pt_out_s1-glucose_reabs_per_unit_length_s2*L_pt_s2); #mmol/min
glucose_pt_out_s3 = max(0,glucose_pt_out_s2-glucose_reabs_per_unit_length_s3*L_pt_s3); #mmol/min
RUGE = glucose_pt_out_s3*number_of_functional_tubules*180; #RUGE in mg/min


excess_glucose_increasing_RTg = (maximal_RTg_increase - RTg_compensation) * max(RUGE,0) / (T_glucose_RTg/C_renal_CV_timescale);

osmotic_natriuresis_effect_pt = 1-min(1,RUGE *glucose_natriuresis_effect_pt);
osmotic_natriuresis_effect_cd = 1-min(1,RUGE *glucose_natriuresis_effect_cd);
osmotic_diuresis_effect_pt = 1-min(1,RUGE *glucose_diuresis_effect_pt);
osmotic_diuresis_effect_cd = 1-min(1,RUGE *glucose_diuresis_effect_cd);


###PT Sodium filtration and reabsorption
# Sodium reabsorbed 1:1 with glucose in S1 and S2
# Sodium reabsorbed 2:1 with glucose in S3
# Assume for non-SGLT reabsorption, sodium reabsorbed at a constant RATE along the tubule
# (represents glomerulotubular balance)
SN_filtered_Na_load = (SNGFR_nL_min / 1000 / 1000000)*Na_concentration;  	#mmol/min
SGTL2_Na_reabs_mmol_s1 = SN_filtered_glucose_load-glucose_pt_out_s1;   		#mmol/min
SGTL2_Na_reabs_mmol_s2 = glucose_pt_out_s1-glucose_pt_out_s2;			#mmol/min
SGTL1_Na_reabs_mmol = 2*(glucose_pt_out_s2-glucose_pt_out_s3);			#mmol/min
total_SGLT_Na_reabs = SGTL2_Na_reabs_mmol_s1+SGTL2_Na_reabs_mmol_s2+SGTL1_Na_reabs_mmol; 	#mmol/min



Na_reabs_per_unit_length = -log(1-e_pt_sodreab)/(L_pt_s1+L_pt_s2+L_pt_s3); #non-SGLT2 reabs	#mmol/min
Na_pt_s1_reabs = min(max_s1_Na_reabs, SN_filtered_Na_load*(1-exp(-Na_reabs_per_unit_length*L_pt_s1)));
Na_pt_out_s1 = SN_filtered_Na_load - Na_pt_s1_reabs - SGTL2_Na_reabs_mmol_s1 ;

Na_pt_s2_reabs = min(max_s2_Na_reabs, Na_pt_out_s1*(1-exp(-Na_reabs_per_unit_length*L_pt_s2)));
Na_pt_out_s2 = Na_pt_out_s1 - Na_pt_s2_reabs - SGTL2_Na_reabs_mmol_s2;

Na_pt_s3_reabs = min(max_s3_Na_reabs, Na_pt_out_s2*(1-exp(-Na_reabs_per_unit_length*L_pt_s3)));
Na_pt_out_s3 = Na_pt_out_s2 - Na_pt_s3_reabs - SGTL1_Na_reabs_mmol;

PT_Na_reabs_fraction = 1-Na_pt_out_s3/SN_filtered_Na_load;

###PT Urea filtration and reabsorption
SN_filtered_urea_load = (SNGFR_nL_min / 1000 / 1000000)*plasma_urea;
urea_out_s1 = SN_filtered_urea_load - urea_permeability_PT*(SN_filtered_urea_load/(0.5*((SNGFR_nL_min / 1000 / 1000000)+water_out_s1_delayed))-plasma_urea)*water_out_s1_delayed; #For now, assuming only reabsorbed at the end
urea_out_s2 = urea_out_s1 - urea_permeability_PT*(urea_out_s1/(0.5*(water_out_s1_delayed+water_out_s2_delayed))-plasma_urea)*water_out_s2_delayed; #For now, assuming only reabsorbed at the end
urea_out_s3 = urea_out_s2 - urea_permeability_PT*(urea_out_s2/(0.5*(water_out_s2_delayed+water_out_s3_delayed))-plasma_urea)*water_out_s3_delayed; #For now, assuming only reabsorbed at the end
urea_reabsorption_fraction = 1-urea_out_s3/SN_filtered_urea_load;


###PT Water Reabsorption
osmoles_out_s1 = 2*Na_pt_out_s1 + glucose_pt_out_s1 + urea_out_s1;
water_out_s1 = (((SNGFR_nL_min / 1000 / 1000000)/(2*SN_filtered_Na_load+SN_filtered_glucose_load+ SN_filtered_urea_load)))*osmoles_out_s1;

osmoles_out_s2 = 2*Na_pt_out_s2 + glucose_pt_out_s2 + urea_out_s2;
water_out_s2 = (water_out_s1/osmoles_out_s1)*osmoles_out_s2;

osmoles_out_s3 = 2*Na_pt_out_s3 + glucose_pt_out_s3 + urea_out_s3;
water_out_s3 = (water_out_s2/osmoles_out_s2)*osmoles_out_s3;

PT_water_reabs_fraction = 1-water_out_s3/(SNGFR_nL_min / 1000 / 1000000);

###Concentrations out of PT
Na_concentration_out_s1 = Na_pt_out_s1/water_out_s1;
Na_concentration_out_s2 = Na_pt_out_s2/water_out_s2;
Na_concentration_out_s3 = Na_pt_out_s3/water_out_s3;

glucose_concentration_out_s1 = glucose_pt_out_s1/water_out_s1;
glucose_concentration_out_s2 = glucose_pt_out_s2/water_out_s2;
glucose_concentration_out_s3 = glucose_pt_out_s3/water_out_s3;

urea_concentration_out_s1 = urea_out_s1/water_out_s1;
urea_concentration_out_s2 = urea_out_s2/water_out_s2;
urea_concentration_out_s3 = urea_out_s3/water_out_s3;

osmolality_out_s1 = osmoles_out_s1/water_out_s1;
osmolality_out_s2 = osmoles_out_s2/water_out_s2;
osmolality_out_s3 = osmoles_out_s3/water_out_s3;

PT_Na_outflow = Na_pt_out_s3*number_of_functional_tubules;

#Tubular sodium reabsorption per unit SA as the driver of tubular hypertrophy
PT_Na_reab_perUnitSA = SN_filtered_Na_load*e_pt_sodreab/(3.14*Dc_pt*(L_pt_s1+L_pt_s2+L_pt_s3));

normalized_PT_reabsorption_density = PT_Na_reab_perUnitSA/PT_Na_reab_perUnitSA_0;
PT_Na_reabs_effect_increasing_tubular_length = 0;#(maximal_tubule_length_increase - tubular_length_increase) * max(normalized_PT_reabsorption_density - 1,0) / (T_PT_Na_reabs_PT_length/C_renal_CV_timescale);
PT_Na_reabs_effect_increasing_tubular_diameter = 0;#(maximal_tubule_diameter_increase - tubular_diameter_increase) * max(normalized_PT_reabsorption_density - 1,0) / (T_PT_Na_reabs_PT_diameter/C_renal_CV_timescale);


##################################### Loop of Henle #########################################

#####Descending Loop of Henle

water_in_DescLoH = water_out_s3; # L/min
Na_in_DescLoH = Na_pt_out_s3;
urea_in_DescLoH = urea_out_s3;
glucose_in_DescLoH = glucose_pt_out_s3;
osmoles_in_DescLoH = osmoles_out_s3;

Na_concentration_in_DescLoH = Na_concentration_out_s3;
Urea_concentration_in_DescLoH = urea_concentration_out_s3;
glucose_concentration_in_DescLoH = glucose_concentration_out_s3;
osmolality_in_DescLoH = osmoles_out_s3/water_out_s3;

#No solute reabsorption in descending limb
Na_out_DescLoH = Na_in_DescLoH;
urea_out_DescLoH = urea_in_DescLoH;
glucose_out_DescLoH = glucose_in_DescLoH;
osmoles_out_DescLoH = osmoles_in_DescLoH;


#For LoH, baseline osmoles reabsorbed per unit length is calculated from nominal fractional sodium reabsorption (see baseline parameters file)
#The rate of reabsorption per unit length may be flow-dependent, and may be modulated by tubular pressure-natriuresis
# If LoH_flow_dependence = 0, then no flow dependence.

deltaLoH_NaFlow = min(max_deltaLoH_reabs,LoH_flow_dependence*(Na_out_DescLoH-nom_Na_in_AscLoH));
AscLoH_Reab_Rate =(2*nominal_loh_na_reabsorption*(nom_Na_in_AscLoH+deltaLoH_NaFlow)*loop_diuretic_effect)/L_lh_des; #osmoles reabsorbed per unit length per minute. factor of 2 because osmoles = 2

effective_AscLoH_Reab_Rate =AscLoH_Reab_Rate*pressure_natriuresis_LoH_effect; #osmoles reabsorbed per unit length per minute. factor of 2 because osmoles = 2*Na


#Min function necesssary to ensure that the LoH does not reabsorb more Na than is delivered to it
osmolality_out_DescLoH = osmolality_in_DescLoH*exp(min(effective_AscLoH_Reab_Rate*L_lh_des,2*Na_in_DescLoH)/(water_in_DescLoH*osmolality_in_DescLoH));
water_out_DescLoH = water_in_DescLoH*osmolality_in_DescLoH/osmolality_out_DescLoH;

Na_concentration_out_DescLoH = Na_out_DescLoH/water_out_DescLoH;
glucose_concentration_out_DescLoH = glucose_out_DescLoH/water_out_DescLoH;
urea_concentration_out_DescLoH = urea_out_DescLoH/water_out_DescLoH;

#####Ascending Loop of Henle

Na_in_AscLoH = Na_out_DescLoH;
urea_in_AscLoH_before_secretion = urea_out_DescLoH;
glucose_in_AscLoH = glucose_out_DescLoH;
osmoles_in_AscLoH_before_secretion = osmoles_out_DescLoH;
water_in_AscLoH = water_out_DescLoH;
Na_concentration_in_AscLoH = Na_concentration_out_DescLoH;
#Urea Secretion --> Assume all urea reabsorbed and secreted only at tip of loop
urea_in_AscLoH = urea_in_AscLoH_before_secretion + reabsorbed_urea_cd_delayed;
urea_concentration_in_AscLoH = urea_in_AscLoH/water_out_DescLoH;
osmoles_in_AscLoH = osmoles_in_AscLoH_before_secretion  + reabsorbed_urea_cd_delayed;

osmolality_in_AscLoH = osmoles_in_AscLoH/water_in_AscLoH;

#Osmolality descreased due to sodium reabsorption along ascending loop
#min function necessary so that LoH doesn't reabsorb more sodium than is delivered to it
osmolality_out_AscLoH = osmolality_in_AscLoH - min(L_lh_des*effective_AscLoH_Reab_Rate, 2*Na_in_DescLoH)*(exp(min(L_lh_des*effective_AscLoH_Reab_Rate, 2*Na_in_DescLoH)/(water_in_DescLoH*osmolality_in_DescLoH))/water_in_DescLoH);
osmoles_reabsorbed_AscLoH = (osmolality_in_AscLoH - osmolality_out_AscLoH)*water_in_AscLoH;
Na_reabsorbed_AscLoH = osmoles_reabsorbed_AscLoH/2;
Na_out_AscLoH = max(0,Na_in_AscLoH - Na_reabsorbed_AscLoH);



#Water, glucose, and urea are not reabsorbed along the ascending limb
urea_out_AscLoH = urea_in_AscLoH; #urea secretion accounted for above
glucose_out_AscLoH = glucose_in_AscLoH;
water_out_AscLoH = water_in_AscLoH;

osmoles_out_AscLoH = osmolality_out_AscLoH*water_out_AscLoH;


Na_concentration_out_AscLoH = Na_out_AscLoH/water_out_AscLoH;
glucose_concentration_out_AscLoH = glucose_out_AscLoH/water_out_AscLoH;
urea_concentration_out_AscLoH = urea_out_AscLoH/water_out_AscLoH;

LoH_reabs_fraction = 1-Na_out_AscLoH/Na_in_AscLoH;

SN_macula_densa_Na_flow = Na_out_AscLoH;
MD_Na_concentration = Na_concentration_out_AscLoH;
TGF0_tubulo_glomerular_feedback = 1 - S_tubulo_glomerular_feedback/2;
tubulo_glomerular_feedback_signal = (TGF0_tubulo_glomerular_feedback + S_tubulo_glomerular_feedback / (1 + exp((MD_Na_concentration_setpoint - MD_Na_concentration)/ F_md_scale_tubulo_glomerular_feedback)));


#############################Distal Convoluted Tubule #######################


water_in_DCT = water_out_AscLoH;
Na_in_DCT = Na_out_AscLoH;
urea_in_DCT = urea_out_AscLoH;
glucose_in_DCT = glucose_out_AscLoH;
osmoles_in_DCT = osmoles_out_AscLoH;

Na_concentration_in_DCT = Na_concentration_out_AscLoH;
urea_concentration_in_DCT = urea_concentration_out_AscLoH;
glucose_concentration_in_DCT = glucose_concentration_out_AscLoH;
osmolality_in_DCT = osmolality_out_AscLoH;

#Assume only sodium reabsorbed along DCT, no water, glucose, or urea reabsorption
urea_out_DCT = urea_in_DCT;
glucose_out_DCT = glucose_in_DCT;
water_out_DCT = water_in_DCT;

urea_concentration_out_DCT = urea_out_DCT/water_out_DCT;
glucose_concentration_out_DCT = glucose_out_DCT/water_out_DCT;

#Assume sodium reabsorption at a constant fraction of delivery
R_dct = -log(1-e_dct_sodreab)/L_dct;

Na_out_DCT = Na_in_DCT*exp(-R_dct*L_dct);
Na_concentration_out_DCT = Na_out_DCT/water_out_DCT;

osmolality_out_DCT = 2*Na_concentration_out_DCT + glucose_concentration_out_DescLoH + urea_concentration_in_AscLoH;
osmoles_out_DCT = osmolality_out_DCT*water_out_DCT;

DCT_Na_reabs_fraction = 1-Na_out_DCT/Na_in_DCT;

Na_reabsorbed_DCT = Na_in_DCT - Na_out_DCT;




################################################Collecting Duct###############################

water_in_CD = water_out_DCT;
Na_in_CD = Na_out_DCT;
urea_in_CD = urea_out_DCT;
glucose_in_CD = glucose_out_DCT;

osmoles_in_CD = osmoles_out_DCT;

#Use this to turn off osmotic diuresis effect
#osmoles_in_CD = osmoles_out_DCT - glucose_in_CD;

osmolality_in_CD = osmoles_in_CD/water_in_CD;

Na_concentration_in_CD = Na_concentration_out_DCT;
urea_concentration_in_CD = urea_concentration_out_DCT;
glucose_concentration_in_CD = glucose_concentration_out_DCT;

osmotic_diuresis_effect_cd = 1-min(1,RUGE *glucose_diuresis_effect_cd);

####Assume sodium reabsorbed, then water follows
#### Then urea reabsorbed at end
#### Then additional water reabsorbed following urea reabsorption

#Assume sodium reabsorbed at fractional rate eta
e_cd_sodreab_adj = e_cd_sodreab*osmotic_natriuresis_effect_cd;
R_cd = -log(1-e_cd_sodreab_adj)/L_cd;
Na_reabsorbed_CD = min(Na_in_CD*(1-exp(-R_cd*L_cd)),CD_Na_reabs_threshold);
Na_out_CD = Na_in_CD-Na_reabsorbed_CD;

CD_Na_reabs_fraction = 1-Na_out_CD/Na_in_CD;

Na_reabsorbed_CT=Na_in_CD-Na_out_CD;

ADH_water_permeability_old = min(0.99999,max(0,nom_ADH_water_permeability*normalized_vasopressin_concentration));
ADH_water_permeability = normalized_vasopressin_concentration/(0.15+normalized_vasopressin_concentration);


osmoles_out_CD = osmoles_in_CD-2*(Na_in_CD - Na_out_CD);

osmolality_out_CD_before_osmotic_reabsorption = osmoles_out_CD/water_in_CD;
water_reabsorbed_CD = ADH_water_permeability*osmotic_diuresis_effect_cd*water_in_CD*(1-osmolality_out_CD_before_osmotic_reabsorption/osmolality_out_DescLoH);
water_out_CD = water_in_CD-water_reabsorbed_CD;
Na_concentration_out_CD = Na_out_CD/water_out_CD;
osmolality_out_CD_after_osmotic_reabsorption = osmoles_out_CD/water_out_CD;
glucose_concentration_after_urea_reabsorption = glucose_in_CD/water_out_CD;

urine_flow_rate = water_out_CD*number_of_functional_tubules;

daily_urine_flow = (urine_flow_rate * 60 * 24);

Na_excretion_via_urine = Na_out_CD*number_of_functional_tubules;
Na_balance = Na_intake_rate - Na_excretion_via_urine;
water_balance = daily_water_intake - daily_urine_flow;


total_NA_reabsorbed = (total_SGLT_Na_reabs +Na_reabsorbed_AscLoH +Na_reabsorbed_DCT +Na_reabsorbed_CT  )*number_of_functional_tubules;

Na_concentration_average_PT = (Na_concentration_out_s1 + Na_concentration_out_s2+Na_concentration_out_s3)/3;
Na_concentration_average_DescLoH = 0.5*(Na_concentration_in_DescLoH + Na_concentration_out_DescLoH);
Na_concentration_average_AscLoH = 0.5*(Na_concentration_in_AscLoH + Na_concentration_out_AscLoH );
Na_concentration_average_DCT = 0.5*(Na_concentration_in_DCT + Na_concentration_out_DCT);
Na_concentration_average_CD = 0.5*(Na_concentration_in_CD + Na_concentration_out_CD);

Na_concentration_average_tubule = 0.2*(Na_concentration_average_PT+Na_concentration_average_DescLoH+Na_concentration_average_AscLoH+Na_concentration_average_DCT+Na_concentration_average_CD);

Oncotic_pressure_tubule = Na_concentration_average_tubule*19.3*2;

Na_amount_in_renal_interstitium = total_NA_reabsorbed;
Na_concentration_renal_interstitium = Na_amount_in_renal_interstitium/(0.03831672); #Total kidney volume 202ml. Interstitium = 0.13*KV
Oncotic_pressure_renal_interstitium = Na_concentration_renal_interstitium*19.3*2;

Net_oncotic_pressure = Oncotic_pressure_tubule-Oncotic_pressure_renal_interstitium;

FENA = Na_excretion_via_urine/filtered_Na_load;

PT_fractional_glucose_reabs = (SN_filtered_glucose_load - glucose_pt_out_s3)/SN_filtered_glucose_load;
PT_fractional_Na_reabs = (SN_filtered_Na_load - Na_pt_out_s3)/SN_filtered_Na_load;
PT_fractional_urea_reabs = ( SN_filtered_urea_load - urea_out_s3)/SN_filtered_urea_load;
PT_fractional_water_reabs = ((SNGFR_nL_min / 1000 / 1000000) - water_out_s3)/(SNGFR_nL_min / 1000 / 1000000);

LoH_fractional_Na_reabs = (Na_in_DescLoH - Na_out_AscLoH)/Na_in_DescLoH;
LoH_fractional_urea_reabs = (urea_in_DescLoH-urea_out_AscLoH)/urea_in_DescLoH;
LoH_fractional_water_reabs = (water_in_DescLoH - water_out_AscLoH)/water_in_DescLoH;

DCT_fractional_Na_reabs = (Na_in_DCT - Na_out_DCT)/Na_in_DCT;

CD_fractional_Na_reabs = (Na_in_CD - Na_out_CD)/Na_in_CD;
#CD_fractional_urea_reabs = (urea_in_CD - urea_out_CD)/urea_in_CD;
CD_OM_fractional_water_reabs = (water_in_CD - water_out_CD)/water_in_CD;


#####################Renal Interstitial Hydrostatic pressure

######RIHP can be approximated from Starling's equation for the peritubular capillaries
### Flow out of the capillary = Kf_peritubular*(Peritubular pressure - RIHP - oncotic pressure difference)
### Assume that any fluid reabsorbed reenters the capillary.
### Therefore, RIHP = Peritubular Pressure - (oncotic pressure in peritubular capillary - interstitial oncotic pressure) + tubular reabsorption/KF
#Peritubular pressure is assumed to equal postglomerular pressure

#Oncotic pressure at the entrance to the peritubular circulation equals oncotic pressure at the exit of the glomerular
Oncotic_pressure_peritubular_in = Oncotic_pressure_out;
plasma_protein_concentration_peritubular_out = (SNRBF_nl_min)*plasma_protein_concentration/(SNRBF_nl_min-urine_flow_rate*1e6*1000/number_of_functional_glomeruli);
Oncotic_pressure_peritubular_out = 1.629*plasma_protein_concentration_peritubular_out+0.2935*(plasma_protein_concentration_peritubular_out^2);
oncotic_pressure_peritubular_avg = (Oncotic_pressure_peritubular_in+Oncotic_pressure_peritubular_out)/2;

Na_concentration_peritubular_cap = (sodium_amount-filtered_Na_load)/blood_volume_L;
oncotic_pressure_peritubular_cap_Na = 0;#Na_concentration_peritubular_cap*19.3*2;
oncotic_pressure_peritubular = oncotic_pressure_peritubular_avg+oncotic_pressure_peritubular_cap_Na;

tubular_reabsorption = GFR_ml_min/1000 - urine_flow_rate;

Volume = interstitial_fluid_volume;

volume_RIHP_int = 1- volume_RIHP_scale/2;
IF_effect_RIHP = (volume_RIHP_int+volume_RIHP_scale/(1+exp(-(Volume-IF_nom)/volume_RIHP_slope)));

oncotic_int = 1-IF_Interonvotic_Effect_scale/2;
IF_effect_oncotic = (oncotic_int+IF_Interonvotic_Effect_scale/(1+exp((Volume-IF_nom)/IF_Interonvotic_Effect_slope)));

#########################################################################

Renal_plasma_amount= 2.5 * RISF_nom*0.01;  # plasma amount in renal interstitium   # Plasma protein concentration = 7
RISF_plasma_protein_concentration = Renal_plasma_amount / (RISF*10);
interstitial_oncotic_pressure = 1.629*RISF_plasma_protein_concentration+0.2935*(RISF_plasma_protein_concentration^2);
RIHP = RISF/C_RISF;
capillary_filtration = nom_peritubular_cap_Kf*(RIHP - postglomerular_pressure - (interstitial_oncotic_pressure -oncotic_pressure_peritubular));

#RIHP = IF_effect_RIHP*(postglomerular_pressure - (oncotic_pressure_peritubular - interstitial_oncotic_pressure*IF_effect_oncotic) + tubular_reabsorption/nom_peritubular_cap_Kf);


################# Tubular Pressure #####################

#####See written documentation for derivation of the equations below
#flow rates expressed in m3/min, rather than L/min


mmHg_Nperm2_conv = 133.32;
Pc_pt_s1 = Pc_pt_s1_mmHg*mmHg_Nperm2_conv;
Pc_pt_s2 = Pc_pt_s2_mmHg*mmHg_Nperm2_conv;
Pc_pt_s3 = Pc_pt_s3_mmHg*mmHg_Nperm2_conv;
Pc_lh_des = Pc_lh_des_mmHg*mmHg_Nperm2_conv;
Pc_lh_asc = Pc_lh_asc_mmHg*mmHg_Nperm2_conv;
Pc_dt = Pc_dt_mmHg*mmHg_Nperm2_conv;
Pc_cd = Pc_cd_mmHg*mmHg_Nperm2_conv;
P_interstitial = (RIHP)*mmHg_Nperm2_conv;# 4.9*mmHg_Nperm2_conv;
pi=3.14;

###CD
B1 = (4*tubular_compliance+1)*128*gamma/pi;
mean_cd_water_flow = (water_in_CD-water_out_CD)/2;
B2_cd = (Pc_cd^(4*tubular_compliance))/(Dc_cd^4);
P_in_cd = (0^(4*tubular_compliance+1)+B1*B2_cd*(mean_cd_water_flow/1e3)*L_cd)^(1/(4*tubular_compliance+1));
P_in_cd_mmHg = (P_in_cd+P_interstitial)/mmHg_Nperm2_conv;



### DCT
B2_dt = (Pc_dt^(4*tubular_compliance))/(Dc_dt^4);
P_in_dt = (P_in_cd^(4*tubular_compliance+1)+B1*B2_dt*(water_in_DCT/1e3)*L_dct)^(1/(4*tubular_compliance+1));
P_in_dt_mmHg = (P_in_dt+P_interstitial)/mmHg_Nperm2_conv;



### Asc LoH
B2_lh_asc = (Pc_lh_asc^(4*tubular_compliance))/(Dc_lh^4);
P_in_lh_asc = (P_in_dt^(4*tubular_compliance+1)+B1*B2_lh_asc*(water_in_AscLoH/1e3)*L_lh_asc)^(1/(4*tubular_compliance+1));
P_in_lh_asc_mmHg = (P_in_lh_asc+P_interstitial)/mmHg_Nperm2_conv;

### Desc LoH
A_lh_des = effective_AscLoH_Reab_Rate/(water_in_DescLoH*osmolality_in_DescLoH);
B2_lh_des = (Pc_lh_des^(4*tubular_compliance))*(water_in_DescLoH/1e3)/((Dc_lh^4)*A_lh_des);
P_in_lh_des = (P_in_lh_asc^(4*tubular_compliance+1)+B1*B2_lh_des*(1-exp(-A_lh_des*L_lh_des)))^(1/(4*tubular_compliance+1));
P_in_lh_des_mmHg = (P_in_lh_des+P_interstitial)/mmHg_Nperm2_conv;

### PT

#Treat urea as if reabsorbed linearly along whole length of PT
Rurea = (SN_filtered_urea_load - urea_out_s3)/(L_pt_s1+L_pt_s2+L_pt_s3);
urea_in_s2 = SN_filtered_urea_load - Rurea*L_pt_s1;
urea_in_s3 = SN_filtered_urea_load - Rurea*(L_pt_s1+L_pt_s2);
A_na = Na_reabs_per_unit_length;

flow_integral_s3 = 2*(Na_pt_out_s2/A_na)*(1-exp(-A_na*L_pt_s3)) - (3/2)*glucose_pt_out_s2*L_pt_s3^2 + urea_in_s3*L_pt_s3 - (1/2)*Rurea*(L_pt_s3^2);
flow_integral_s2 = 2*(Na_pt_out_s1/A_na)*(1-exp(-A_na*L_pt_s2)) - (1/2)*glucose_pt_out_s1*L_pt_s2^2 + urea_in_s2*L_pt_s2 - (1/2)*Rurea*(L_pt_s2^2);
flow_integral_s1 = 2*(SN_filtered_Na_load/A_na)*(1-exp(-A_na*L_pt_s1)) - (1/2)*SN_filtered_glucose_load*L_pt_s1^2 + SN_filtered_urea_load*L_pt_s1 - (1/2)*Rurea*(L_pt_s1^2);


#S3 segment
B2_pt_s3 = (Pc_pt_s3^(4*tubular_compliance))/(Dc_pt^4);
B3_pt_s3 = (water_out_s2/1e3)/osmoles_out_s2;
P_in_pt_s3= (P_in_lh_des^(4*tubular_compliance+1)+B1*B2_pt_s3*B3_pt_s3*flow_integral_s3)^(1/(4*tubular_compliance+1));
P_in_pt_s3_mmHg = (P_in_pt_s3+P_interstitial)/mmHg_Nperm2_conv;

B2_pt_s2 = (Pc_pt_s3^(4*tubular_compliance))/(Dc_pt^4);
B3_pt_s2 = (water_out_s1/1e3)/osmoles_out_s1;
P_in_pt_s2= (P_in_pt_s3^(4*tubular_compliance+1)+B1*B2_pt_s2*B3_pt_s2*flow_integral_s2)^(1/(4*tubular_compliance+1));
P_in_pt_s2_mmHg = (P_in_pt_s2+P_interstitial)/mmHg_Nperm2_conv;

B2_pt_s1 = (Pc_pt_s1^(4*tubular_compliance))/(Dc_pt^4);
B3_pt_s1 = (SNGFR_nL_min / 1e12)/(2*SN_filtered_Na_load+SN_filtered_glucose_load+ SN_filtered_urea_load);
P_in_pt_s1= (P_in_pt_s2^(4*tubular_compliance+1)+B1*B2_pt_s1*B3_pt_s1*flow_integral_s1)^(1/(4*tubular_compliance+1));
P_in_pt_s1_mmHg = (P_in_pt_s1+P_interstitial)/mmHg_Nperm2_conv;




####################### Aldosterone and Renin Secretion

###Aldosterone is secreted in response to AT1-bound AngII and changes in potassium or sodium concentration
#Potassium concentration is treated as a constant for now
#Empirircal relationship for Karaaslan 2005


AT1_aldo_int = 1 - AT1_aldo_slope*nominal_equilibrium_AT1_bound_AngII;
AngII_effect_on_aldo = AT1_aldo_int + AT1_aldo_slope*AT1_bound_AngII;
N_als = (K_Na_ratio_effect_on_aldo * AngII_effect_on_aldo );

###Renin is secreted in response to decreases in AT1-bound AngII and decreases in MD sodium flow

###Renin is secreted in response to decreases in AT1-bound AngII, decreases in MD sodium flow, or increases in RSNA
#RSNA effect on renin secretion
rsna_renin_intercept = 1-rsna_renin_slope;
rnsa_effect_on_renin_secretion =  rsna_renin_slope * renal_sympathetic_nerve_activity + rsna_renin_intercept;


#Macula Densa Sodium flow effect on renin secretion
#This relationship is known to be non-linear, and md_renin_tau can be calibrated based on data on changes in renin as a functoin of sodium intake
md_effect_on_renin_secretion = md_renin_A*exp(-md_renin_tau*(SN_macula_densa_Na_flow_delayed*baseline_nephrons - nom_LoH_Na_outflow));


#AT1-bound AngII feedback on renin secretion
AT1_bound_AngII_effect_on_PRA = (10 ^ (AT1_PRC_slope * log10(AT1_bound_AngII / nominal_equilibrium_AT1_bound_AngII) + AT1_PRC_yint));


#Aldo effect on renin secretion
aldo_renin_intercept = 1-aldo_renin_slope;
aldo_effect_on_renin_secretion =  aldo_renin_intercept + aldo_renin_slope*Aldo_MR_normalised_effect;

#Plasma renin activity
plasma_renin_activity = concentration_to_renin_activity_conversion_plasma* plasma_renin_concentration*(1-pct_target_inhibition_DRI);

#Renin secretion
renin_secretion_rate = (log(2)/renin_half_life)*nominal_equilibrium_PRC*AT1_bound_AngII_effect_on_PRA*md_effect_on_renin_secretion*HCTZ_effect_on_renin_secretion*aldo_effect_on_renin_secretion*BB_renin_secretion_effect;

#RAAS degradation rates
renin_degradation_rate = log(2)/renin_half_life;
AngI_degradation_rate = log(2)/AngI_half_life;
AngII_degradation_rate = log(2)/AngII_half_life;
AT1_bound_AngII_degradation_rate =  log(2)/AT1_bound_AngII_half_life;
AT2_bound_AngII_degradation_rate = log(2)/AT2_bound_AngII_half_life;

#RAAS rate constants
ACE_activity = nominal_ACE_activity*(1 - pct_target_inhibition_ACEi);
chymase_activity = nominal_chymase_activity;
AT1_receptor_binding_rate = nominal_AT1_receptor_binding_rate*(1 - pct_target_inhibition_ARB);
AT2_receptor_binding_rate = nominal_AT2_receptor_binding_rate;



Blood_volume_protein_osmotic_pressure = 1.629*plasma_protein_concentration + 0.2935*plasma_protein_concentration^2;
ISF_protein_osmotic_pressure = 1.629*ISF_protein_concentration + 0.2935*ISF_protein_concentration^2;

Blood_volume_osmotic_pressure = Blood_volume_protein_osmotic_pressure+ Na_concentration*19.3*2;
ISF_osmotic_pressire = IF_Na_concentration*19.3*2 + ISF_protein_osmotic_pressure;
Protein_sodium_filtration_pressure_grad = (mean_capillary_pressure - ISF_pressure - Blood_volume_osmotic_pressure + ISF_osmotic_pressire);

Kidney_disconnect_heart = Q_water*(Na_concentration - IF_Na_concentration);
Kidney_connect_heart = -Sodium_protein_filtration_rate_Kf*(Protein_sodium_filtration_pressure_grad)*0.001;

if (heart_renal_link == 1) {
Fluid_exchanging_function=Kidney_connect_heart;
} else{
Fluid_exchanging_function=Kidney_connect_heart;
}

peripheral_volume_change=arterial_dis_circulation_volume+capillary_circulation_volume+venules_circulation_volume;
#d/dt(HR_ratio) = heart_rate_constant*heart_rate_multiplier_adjusted;
d/dt(venous_volume) = venous_flow + C_renal_CV_timescale*(venous_volume_target - venous_volume) - tricuspid_valve_flow_rate ;
d/dt(LV_volume) = mitral_valve_flow_rate - aortic_valve_flow_rate;
d/dt(arterial_volume) = (aortic_valve_flow_rate) - (systemic_blood_flow);
#d/dt(peripheral_circulation_volume)= systemic_blood_flow - venous_flow;
d/dt(RV_volume) = (tricuspid_valve_flow_rate) - (pulmonary_valve_flow_rate);
d/dt(pulmonary_arterial_volume) = pulmonary_valve_flow_rate - pulmonary_arterial_blood_flow;
#d/dt(peripheral_circulation_volume) = (systemic_blood_flow) - venous_flow;

d/dt(arterial_dis_circulation_volume)=systemic_blood_flow - capillary_blood_flow;
d/dt(capillary_circulation_volume)=arterial_dis_blood_flow-venules_blood_flow;
d/dt(venules_circulation_volume)=capillary_blood_flow-venous_flow;

d/dt(pulmonary_venous_volume) = pulmonary_arterial_blood_flow - mitral_valve_flow_rate;
d/dt(aortic_blood_flow_delayed) = C_cycle2 * (aortic_blood_flow - aortic_blood_flow_delayed);
d/dt(pulmonary_blood_flow_delayed) = C_cycle2 * (pulmonary_blood_flow - pulmonary_blood_flow_delayed);

#################################################################################################
#################################################################################################
## Hypertrophy
d/dt(change_in_myocyte_length) = kL_hypertrophy * (LV_EDS / LV_passive_stress_along_fiber_threshhold - 1);
d/dt(change_in_myocyte_diameter) = kD_hypertrophy * (LV_active_stress_peak / LV_active_stress_threshhold - 1);
d/dt(LV_active_stress_peak) = C_cycle3 *(LV_active_stress_peak_old - LV_active_stress_peak);
d/dt(sim_time)=1;
d/dt(LV_sarcomere_length_delayed) = C_cycle* (LV_sarcomere_length - LV_sarcomere_length_delayed);
d/dt(RV_sarcomere_length_delayed) = C_cycle* (RV_sarcomere_length - RV_sarcomere_length_delayed);
d/dt(LV_EDV) = C_cycle2 * (LV_EDV_old - LV_EDV);
d/dt(LV_EDP) = C_cycle2 *(LV_EDP_old - LV_EDP);
#d/dt(LV_PSP) = C_cycle2 *(LV_PSP_old - LV_PSP);
d/dt(LV_EDS) = C_cycle2 *(LV_EDS_old - LV_EDS);

d/dt(arterial_pressure_delayed) = C_cycle2 * (arterial_pressure - arterial_pressure_delayed);
d/dt(arterial_pressure_bigger_delay) = C_cycle2 * (arterial_pressure_delayed - arterial_pressure_bigger_delay);
d/dt(systolic_pressure) = C_cycle2 * (systolic_pressure_old - systolic_pressure);
d/dt(diastolic_pressure) = C_cycle2 * (diastolic_pressure_old - diastolic_pressure);

######################computing the systolic and diastolic venous pressure##########
d/dt(venous_pressure_delayed)=C_cycle2*(venous_pressure-venous_pressure_delayed);
d/dt(venous_pressure_bigger_delay)=C_cycle2*(venous_pressure_delayed-venous_pressure_bigger_delay);
d/dt(venous_systolic_pressure)=C_cycle2*(venous_systolic_pressure_old-venous_systolic_pressure);
d/dt(venous_diastolic_pressure)=C_cycle2*(venous_diastolic_pressure_old-venous_diastolic_pressure);
d/dt(mean_venous_pressure_delayed) = 1*(mean_venous_pressure - mean_venous_pressure_delayed);

######################computing the systolic and diastolic peripheral pressure##########
d/dt(capillary_pressure_delayed)=C_cycle2*(capillary_pressure-capillary_pressure_delayed);
d/dt(capillary_pressure_bigger_delay)=C_cycle2*(capillary_pressure_delayed-capillary_pressure_bigger_delay);
d/dt(capillary_systolic_pressure)=C_cycle2*(capillary_systolic_pressure_old-capillary_systolic_pressure);
d/dt(capillary_diastolic_pressure)=C_cycle2*(capillary_diastolic_pressure_old-capillary_diastolic_pressure);
d/dt(mean_capillary_pressure_delayed) = 1*(mean_capillary_pressure - mean_capillary_pressure_delayed);


d/dt(CO) = C_co*(aortic_valve_flow_rate*60/L_m3 - CO);
d/dt(CO_delayed) = C_co_delay*(CO - CO_delayed);

#RAAS Pathway
d/dt(AngI) = plasma_renin_activity - (AngI) * (chymase_activity + ACE_activity) - (AngI) * AngI_degradation_rate;
d/dt(AngII) = AngI * (chymase_activity + ACE_activity) - AngII * AngII_degradation_rate - AngII*AT1_receptor_binding_rate - AngII* (AT2_receptor_binding_rate);
d/dt(AT1_bound_AngII) = AngII * (AT1_receptor_binding_rate) - AT1_bound_AngII_degradation_rate*AT1_bound_AngII;
d/dt(AT2_bound_AngII) = AngII * (AT2_receptor_binding_rate) - AT2_bound_AngII_degradation_rate*AT2_bound_AngII;
d/dt(plasma_renin_concentration) = renin_secretion_rate - plasma_renin_concentration * renin_degradation_rate;

#Change in Interstitial fluid volume over time is determined by the different between water intake and urine outflow
d/dt(blood_volume_L) = C_renal_CV_timescale *(water_intake- urine_flow_rate+Fluid_exchanging_function);  #
d/dt(interstitial_fluid_volume) = -C_renal_CV_timescale *Fluid_exchanging_function;

#Change in total body sodium over time is determined by the different between sodium intake and excretion
d/dt(sodium_amount) = C_renal_CV_timescale * (Na_intake_rate - Na_excretion_via_urine + Q_Na*(IF_Na_concentration - Na_concentration));
d/dt(IF_sodium_amount) = C_renal_CV_timescale *(Q_Na*(Na_concentration - IF_Na_concentration) - sodium_storate_rate);

d/dt(stored_sodium) = C_renal_CV_timescale *sodium_storate_rate;

#These equations serve only to delay the input variable by one timestep. This allows the previous value of the input variable to be used in an equation that appears
#in the code before the input variable was defined
d/dt(tubulo_glomerular_feedback_effect) =  C_renal_CV_timescale *(tubulo_glomerular_feedback_signal-tubulo_glomerular_feedback_effect);
d/dt(normalized_aldosterone_level) =  C_renal_CV_timescale *C_aldo_secretion * (N_als-normalized_aldosterone_level);
d/dt(preafferent_pressure_autoreg_signal) = C_renal_CV_timescale *100*(preafferent_pressure_autoreg_function - preafferent_pressure_autoreg_signal);
d/dt(glomerular_pressure_autoreg_signal) = 0;#C_glomerular_pressure_autoreg_signal*(glomerular_pressure_autoreg_function - glomerular_pressure_autoreg_signal);

#Error signals for PI controllers of cardiac output and sodium concentration
d/dt(CO_error) = C_renal_CV_timescale*C_co_error*(CO_delayed-CO_nom);
d/dt(Na_concentration_error) = C_renal_CV_timescale *C_Na_error*(Na_concentration - ref_Na_concentration);

#This equation allows a delay between the secretion of vasopression and its effect on water intake and tubular water reabsorption
d/dt(normalized_vasopressin_concentration_delayed)= C_renal_CV_timescale *C_vasopressin_delay*(normalized_vasopressin_concentration - normalized_vasopressin_concentration_delayed);

#TGF resetting. If C_tgf_reset = 0, no TGF resetting occurs. If it is greater than zero, the setpoint will change over time and will eventually
#come to equal the ambient MD sodium flow rate.
d/dt(F0_TGF) = C_renal_CV_timescale *C_tgf_reset*(SN_macula_densa_Na_flow*baseline_nephrons - F0_TGF);

#As above, these equations allow a variable to be used in equations that appear in the code before the variable was first defined.
d/dt(P_bowmans) = C_renal_CV_timescale *100*(P_in_pt_s1_mmHg - P_bowmans);
d/dt(oncotic_pressure_difference) = 100*(oncotic_pressure_avg - oncotic_pressure_difference);
d/dt(renal_blood_flow_L_min_delayed)=C_renal_CV_timescale*C_rbf*(renal_blood_flow_L_min - renal_blood_flow_L_min_delayed);
d/dt(SN_macula_densa_Na_flow_delayed) = C_renal_CV_timescale * C_md_flow*( SN_macula_densa_Na_flow - SN_macula_densa_Na_flow_delayed);
d/dt(rsna_delayed) = C_renal_CV_timescale *C_rsna*(renal_sympathetic_nerve_activity - rsna_delayed);

###Disease effects (turned off by default)
#Glomerular hypertrophy
d/dt(disease_effects_increasing_Kf) = GP_effect_increasing_Kf;
#Loss of CD pressure natriuresis response over time
d/dt(disease_effects_decreasing_CD_PN) = CD_PN_loss_rate;

#Tubular hypertrophy
d/dt(tubular_length_increase) = PT_Na_reabs_effect_increasing_tubular_length;
d/dt(tubular_diameter_increase) = PT_Na_reabs_effect_increasing_tubular_diameter;


d/dt(water_out_s1_delayed) = C_renal_CV_timescale * C_pt_water*(water_out_s1 - water_out_s1_delayed);
d/dt(water_out_s2_delayed) = C_renal_CV_timescale * C_pt_water*(water_out_s2 - water_out_s2_delayed);
d/dt(water_out_s3_delayed) = C_renal_CV_timescale * C_pt_water*(water_out_s3 - water_out_s3_delayed);
d/dt(reabsorbed_urea_cd_delayed) = 0;#C_pt_water*(reabsorbed_urea_cd - reabsorbed_urea_cd_delayed);

#Urinary glucose excretion
d/dt(UGE) = C_renal_CV_timescale * RUGE;

#Serum Creatinine
d/dt(serum_creatinine) = C_renal_CV_timescale*(creatinine_synthesis_rate - creatinine_clearance_rate);
d/dt(cumNaExcretion) = C_renal_CV_timescale*Na_excretion_via_urine;
d/dt(cumWaterExcretion) = C_renal_CV_timescale*urine_flow_rate;
d/dt(cumCreatinineExcretion) = C_renal_CV_timescale*creatinine_clearance_rate;
d/dt(RTg_compensation) = C_renal_CV_timescale*excess_glucose_increasing_RTg;
d/dt(SGLT2_inhibition_delayed) = C_renal_CV_timescale*C_sglt2_delay*(SGLT2_inhibition - SGLT2_inhibition_delayed);
d/dt(RUGE_delayed) = C_renal_CV_timescale*C_ruge*(RUGE - RUGE_delayed);
d/dt(postglomerular_pressure_delayed) = C_renal_CV_timescale*C_postglomerular_pressure*(postglomerular_pressure - postglomerular_pressure_delayed); #necessary to prevent large oscillations
d/dt(postglomerular_pressure_error) = C_renal_CV_timescale*(postglomerular_pressure - RIHP0);
d/dt(renal_flow_rate_error) = C_renal_CV_timescale*(renal_blood_flow_L_min - nom_renal_blood_flow_L_min);
d/dt(MAP_delayed) = C_renal_CV_timescale*C_cycle2*(mean_arterial_pressure_MAP - MAP_delayed); #necessary to prevent large oscillations;
d/dt(RIHP_delayed)=C_renal_CV_timescale*C_cycle2*(RIHP - RIHP_delayed);
d/dt(Net_oncotic_pressure_diff) = C_renal_CV_timescale*C_cycle2*(Net_oncotic_pressure - Net_oncotic_pressure_diff);
d/dt(RISF) = tubular_reabsorption - capillary_filtration;
")

  test_that("large models compile", {
    expect_s3_class(mod, "rxode2")
  })
})
