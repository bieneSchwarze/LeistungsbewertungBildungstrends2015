log using "F:\IQB_Paper\Projekt_LehrerNoten\Results\log_file_26112019.txt", text replace

// IMP 1 
use "F:\IQB_Paper\Projekt_LehrerNoten\Results\impData1.dta", clear
destring trnotedeu_r, replace
gen w1 = wgt_L
gen w2 = 1
gllamm trnotedeu_r wle_ortho BEFKIwle AnstrB TR_geschlecht TR_SPF_r UNIm FHBAm mig GY Pspfges SuS_KL AntMig AntMaed_KL SysMod Lgender Llja_FDZ FB_diag1 FB_diag2 querFremdNo querFremdNA intGeschUni intGeschFHBA, i(idteach_d_FDZ1) link(oprobit) pweight(w)
estat ic 

// IMP 2 
use "F:\IQB_Paper\Projekt_LehrerNoten\Results\impData2.dta", clear
destring trnotedeu_r, replace
gen w1 = wgt_L
gen w2 = 1
gllamm trnotedeu_r wle_ortho BEFKIwle AnstrB TR_geschlecht TR_SPF_r UNIm FHBAm mig GY Pspfges SuS_KL AntMig AntMaed_KL SysMod Lgender Llja_FDZ FB_diag1 FB_diag2 querFremdNo querFremdNA intGeschUni intGeschFHBA, i(idteach_d_FDZ1) link(oprobit) pweight(w)
estat ic 

// IMP 3 
use "F:\IQB_Paper\Projekt_LehrerNoten\Results\impData3.dta", clear
destring trnotedeu_r, replace
gen w1 = wgt_L
gen w2 = 1
gllamm trnotedeu_r wle_ortho BEFKIwle AnstrB TR_geschlecht TR_SPF_r UNIm FHBAm mig GY Pspfges SuS_KL AntMig AntMaed_KL SysMod Lgender Llja_FDZ FB_diag1 FB_diag2 querFremdNo querFremdNA intGeschUni intGeschFHBA, i(idteach_d_FDZ1) link(oprobit) pweight(w)
estat ic 

// IMP 4 
use "F:\IQB_Paper\Projekt_LehrerNoten\Results\impData4.dta", clear
destring trnotedeu_r, replace
gen w1 = wgt_L
gen w2 = 1
gllamm trnotedeu_r wle_ortho BEFKIwle AnstrB TR_geschlecht TR_SPF_r UNIm FHBAm mig GY Pspfges SuS_KL AntMig AntMaed_KL SysMod Lgender Llja_FDZ FB_diag1 FB_diag2 querFremdNo querFremdNA intGeschUni intGeschFHBA, i(idteach_d_FDZ1) link(oprobit) pweight(w)
estat ic  

// IMP 5 
use "F:\IQB_Paper\Projekt_LehrerNoten\Results\impData5.dta", clear
destring trnotedeu_r, replace
gen w1 = wgt_L
gen w2 = 1
gllamm trnotedeu_r wle_ortho BEFKIwle AnstrB TR_geschlecht TR_SPF_r UNIm FHBAm mig GY Pspfges SuS_KL AntMig AntMaed_KL SysMod Lgender Llja_FDZ FB_diag1 FB_diag2 querFremdNo querFremdNA intGeschUni intGeschFHBA, i(idteach_d_FDZ1) link(oprobit) pweight(w)
estat ic 

// IMP 6 
use "F:\IQB_Paper\Projekt_LehrerNoten\Results\impData6.dta", clear
destring trnotedeu_r, replace
gen w1 = wgt_L
gen w2 = 1
gllamm trnotedeu_r wle_ortho BEFKIwle AnstrB TR_geschlecht TR_SPF_r UNIm FHBAm mig GY Pspfges SuS_KL AntMig AntMaed_KL SysMod Lgender Llja_FDZ FB_diag1 FB_diag2 querFremdNo querFremdNA intGeschUni intGeschFHBA, i(idteach_d_FDZ1) link(oprobit) pweight(w)
estat ic 

// IMP 7 
use "F:\IQB_Paper\Projekt_LehrerNoten\Results\impData7.dta", clear
destring trnotedeu_r, replace
gen w1 = wgt_L
gen w2 = 1
gllamm trnotedeu_r wle_ortho BEFKIwle AnstrB TR_geschlecht TR_SPF_r UNIm FHBAm mig GY Pspfges SuS_KL AntMig AntMaed_KL SysMod Lgender Llja_FDZ FB_diag1 FB_diag2 querFremdNo querFremdNA intGeschUni intGeschFHBA, i(idteach_d_FDZ1) link(oprobit) pweight(w)
estat ic 

// IMP 7 
use "Z:\Abteilung_3_(03)\Statistik_(04)\MA_(02)\_StGeIm\SZ\Projekt_LehrerNoten\Results\impData7.dta", clear
destring trnotedeu_r, replace
gen w1 = wgt_L
gen w2 = 1
gllamm trnotedeu_r wle_ortho BEFKIwle AnstrB TR_geschlecht TR_SPF_r terBildm mig GY Pspfges SuS_KL AntMig AntMaed_KL SysMod Lgender Llja_FDZ FB_diag1 FB_diag2 querFremdNo querFremdNA intGeschUni, i(idteach_d_FDZ1) link(oprobit) pweight(w)
estat ic 

// IMP 8 
use "F:\IQB_Paper\Projekt_LehrerNoten\Results\impData8.dta", clear
destring trnotedeu_r, replace
gen w1 = wgt_L
gen w2 = 1
gllamm trnotedeu_r wle_ortho BEFKIwle AnstrB TR_geschlecht TR_SPF_r UNIm FHBAm mig GY Pspfges SuS_KL AntMig AntMaed_KL SysMod Lgender Llja_FDZ FB_diag1 FB_diag2 querFremdNo querFremdNA intGeschUni intGeschFHBA, i(idteach_d_FDZ1) link(oprobit) pweight(w)
estat ic 

// IMP 9 
use "F:\IQB_Paper\Projekt_LehrerNoten\Results\impData9.dta", clear
destring trnotedeu_r, replace
gen w1 = wgt_L
gen w2 = 1
gllamm trnotedeu_r wle_ortho BEFKIwle AnstrB TR_geschlecht TR_SPF_r UNIm FHBAm mig GY Pspfges SuS_KL AntMig AntMaed_KL SysMod Lgender Llja_FDZ FB_diag1 FB_diag2 querFremdNo querFremdNA intGeschUni intGeschFHBA, i(idteach_d_FDZ1) link(oprobit) pweight(w)
estat ic  

// IMP 10 
use "F:\IQB_Paper\Projekt_LehrerNoten\Results\impData10.dta", clear
destring trnotedeu_r, replace
gen w1 = wgt_L
gen w2 = 1
gllamm trnotedeu_r wle_ortho BEFKIwle AnstrB TR_geschlecht TR_SPF_r UNIm FHBAm mig GY Pspfges SuS_KL AntMig AntMaed_KL SysMod Lgender Llja_FDZ FB_diag1 FB_diag2 querFremdNo querFremdNA intGeschUni intGeschFHBA, i(idteach_d_FDZ1) link(oprobit) pweight(w)
estat ic  

// IMP 11 
use "F:\IQB_Paper\Projekt_LehrerNoten\Results\impData11.dta", clear
destring trnotedeu_r, replace
gen w1 = wgt_L
gen w2 = 1
gllamm trnotedeu_r wle_ortho BEFKIwle AnstrB TR_geschlecht TR_SPF_r UNIm FHBAm mig GY Pspfges SuS_KL AntMig AntMaed_KL SysMod Lgender Llja_FDZ FB_diag1 FB_diag2 querFremdNo querFremdNA intGeschUni intGeschFHBA, i(idteach_d_FDZ1) link(oprobit) pweight(w)
estat ic 

// IMP 12 
use "F:\IQB_Paper\Projekt_LehrerNoten\Results\impData12.dta", clear
destring trnotedeu_r, replace
gen w1 = wgt_L
gen w2 = 1
gllamm trnotedeu_r wle_ortho BEFKIwle AnstrB TR_geschlecht TR_SPF_r UNIm FHBAm mig GY Pspfges SuS_KL AntMig AntMaed_KL SysMod Lgender Llja_FDZ FB_diag1 FB_diag2 querFremdNo querFremdNA intGeschUni intGeschFHBA, i(idteach_d_FDZ1) link(oprobit) pweight(w)
estat ic  

// IMP 13 
use "F:\IQB_Paper\Projekt_LehrerNoten\Results\impData13.dta", clear
destring trnotedeu_r, replace
gen w1 = wgt_L
gen w2 = 1
gllamm trnotedeu_r wle_ortho BEFKIwle AnstrB TR_geschlecht TR_SPF_r UNIm FHBAm mig GY Pspfges SuS_KL AntMig AntMaed_KL SysMod Lgender Llja_FDZ FB_diag1 FB_diag2 querFremdNo querFremdNA intGeschUni intGeschFHBA, i(idteach_d_FDZ1) link(oprobit) pweight(w)
estat ic 

// IMP 14 
use "F:\IQB_Paper\Projekt_LehrerNoten\Results\impData14.dta", clear
destring trnotedeu_r, replace
destring trnotedeu_r, replace
gen w1 = wgt_L
gen w2 = 1
gllamm trnotedeu_r wle_ortho BEFKIwle AnstrB TR_geschlecht TR_SPF_r UNIm FHBAm mig GY Pspfges SuS_KL AntMig AntMaed_KL SysMod Lgender Llja_FDZ FB_diag1 FB_diag2 querFremdNo querFremdNA intGeschUni intGeschFHBA, i(idteach_d_FDZ1) link(oprobit) pweight(w)
estat ic  

// IMP 15 
use "F:\IQB_Paper\Projekt_LehrerNoten\Results\impData15.dta", clear
destring trnotedeu_r, replace
gen w1 = wgt_L
gen w2 = 1
gllamm trnotedeu_r wle_ortho BEFKIwle AnstrB TR_geschlecht TR_SPF_r UNIm FHBAm mig GY Pspfges SuS_KL AntMig AntMaed_KL SysMod Lgender Llja_FDZ FB_diag1 FB_diag2 querFremdNo querFremdNA intGeschUni intGeschFHBA, i(idteach_d_FDZ1) link(oprobit) pweight(w)
estat ic 

// IMP 16
use "F:\IQB_Paper\Projekt_LehrerNoten\Results\impData16.dta", clear
destring trnotedeu_r, replace
gen w1 = wgt_L
gen w2 = 1
gllamm trnotedeu_r wle_ortho BEFKIwle AnstrB TR_geschlecht TR_SPF_r UNIm FHBAm mig GY Pspfges SuS_KL AntMig AntMaed_KL SysMod Lgender Llja_FDZ FB_diag1 FB_diag2 querFremdNo querFremdNA intGeschUni intGeschFHBA, i(idteach_d_FDZ1) link(oprobit) pweight(w)
estat ic 

// IMP 17
use "F:\IQB_Paper\Projekt_LehrerNoten\Results\impData17.dta", clear
destring trnotedeu_r, replace
gen w1 = wgt_L
gen w2 = 1
gllamm trnotedeu_r wle_ortho BEFKIwle AnstrB TR_geschlecht TR_SPF_r UNIm FHBAm mig GY Pspfges SuS_KL AntMig AntMaed_KL SysMod Lgender Llja_FDZ FB_diag1 FB_diag2 querFremdNo querFremdNA intGeschUni intGeschFHBA, i(idteach_d_FDZ1) link(oprobit) pweight(w)
estat ic 

// IMP 18
use "F:\IQB_Paper\Projekt_LehrerNoten\Results\impData18.dta", clear
destring trnotedeu_r, replace
gen w1 = wgt_L
gen w2 = 1
gllamm trnotedeu_r wle_ortho BEFKIwle AnstrB TR_geschlecht TR_SPF_r UNIm FHBAm mig GY Pspfges SuS_KL AntMig AntMaed_KL SysMod Lgender Llja_FDZ FB_diag1 FB_diag2 querFremdNo querFremdNA intGeschUni intGeschFHBA, i(idteach_d_FDZ1) link(oprobit) pweight(w)
estat ic 

// IMP 19
use "F:\IQB_Paper\Projekt_LehrerNoten\Results\impData19.dta", clear
destring trnotedeu_r, replace
gen w1 = wgt_L
gen w2 = 1
gllamm trnotedeu_r wle_ortho BEFKIwle AnstrB TR_geschlecht TR_SPF_r UNIm FHBAm mig GY Pspfges SuS_KL AntMig AntMaed_KL SysMod Lgender Llja_FDZ FB_diag1 FB_diag2 querFremdNo querFremdNA intGeschUni intGeschFHBA, i(idteach_d_FDZ1) link(oprobit) pweight(w)
estat ic 

// IMP 20
use "F:\IQB_Paper\Projekt_LehrerNoten\Results\impData20.dta", clear
destring trnotedeu_r, replace
gen w1 = wgt_L
gen w2 = 1
gllamm trnotedeu_r wle_ortho BEFKIwle AnstrB TR_geschlecht TR_SPF_r UNIm FHBAm mig GY Pspfges SuS_KL AntMig AntMaed_KL SysMod Lgender Llja_FDZ FB_diag1 FB_diag2 querFremdNo querFremdNA intGeschUni intGeschFHBA, i(idteach_d_FDZ1) link(oprobit) pweight(w)
estat ic 

log close
