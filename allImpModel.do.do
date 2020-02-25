// IMP 1 ok
use "E:\SZ\Projekt_LehrerNoten\Results\impData1.dta", clear
destring trnotedeu_r, replace
gen w1 = wgt_L
gen w2 = 1
gllamm trnotedeu_r wle_ortho BEFKIwle AnstrB TR_geschlecht TR_SPF_r FHBAm UNIm mig GY Pspfges SuS_KL wle_orthoM AntMig AntMaed_KL SysMod Lgender Llja_FDZ querFremdYes intGeschFHBA intGeschUni intKompFHBA intKompUni LS_gender, i(idteach_d_FDZ1) link(oprobit) pweight(w)
estat ic 

// IMP 2 
use "E:\SZ\Projekt_LehrerNoten\Results\impData2.dta", clear
destring trnotedeu_r, replace
gen w1 = wgt_L
gen w2 = 1
gllamm trnotedeu_r wle_ortho BEFKIwle AnstrB TR_geschlecht TR_SPF_r FHBAm UNIm mig GY Pspfges SuS_KL wle_orthoM AntMig AntMaed_KL SysMod Lgender Llja_FDZ querFremdYes intGeschFHBA intGeschUni intKompFHBA intKompUni LS_gender, i(idteach_d_FDZ1) link(oprobit) pweight(w)
estat ic 

// IMP 3
use "E:\SZ\Projekt_LehrerNoten\Results\impData3.dta", clear
destring trnotedeu_r, replace
gen w1 = wgt_L
gen w2 = 1
gllamm trnotedeu_r wle_ortho BEFKIwle AnstrB TR_geschlecht TR_SPF_r FHBAm UNIm mig GY Pspfges SuS_KL wle_orthoM AntMig AntMaed_KL SysMod Lgender Llja_FDZ querFremdYes intGeschFHBA intGeschUni intKompFHBA intKompUni LS_gender, i(idteach_d_FDZ1) link(oprobit) pweight(w)
estat ic 

// IMP 4 
use "E:\SZ\Projekt_LehrerNoten\Results\impData4.dta", clear
destring trnotedeu_r, replace
gen w1 = wgt_L
gen w2 = 1
gllamm trnotedeu_r wle_ortho BEFKIwle AnstrB TR_geschlecht TR_SPF_r FHBAm UNIm mig GY Pspfges SuS_KL wle_orthoM AntMig AntMaed_KL SysMod Lgender Llja_FDZ querFremdYes intGeschFHBA intGeschUni intKompFHBA intKompUni LS_gender, i(idteach_d_FDZ1) link(oprobit) pweight(w)
estat ic 

// IMP 5 
use "E:\SZ\Projekt_LehrerNoten\Results\impData5.dta", clear
destring trnotedeu_r, replace
gen w1 = wgt_L
gen w2 = 1
gllamm trnotedeu_r wle_ortho BEFKIwle AnstrB TR_geschlecht TR_SPF_r FHBAm UNIm mig GY Pspfges SuS_KL wle_orthoM AntMig AntMaed_KL SysMod Lgender Llja_FDZ querFremdYes intGeschFHBA intGeschUni intKompFHBA intKompUni LS_gender, i(idteach_d_FDZ1) link(oprobit) pweight(w)
estat ic 

// IMP 6 
use "E:\SZ\Projekt_LehrerNoten\Results\impData6.dta", clear
destring trnotedeu_r, replace
gen w1 = wgt_L
gen w2 = 1
gllamm trnotedeu_r wle_ortho BEFKIwle AnstrB TR_geschlecht TR_SPF_r FHBAm UNIm mig GY Pspfges SuS_KL wle_orthoM AntMig AntMaed_KL SysMod Lgender Llja_FDZ querFremdYes intGeschFHBA intGeschUni intKompFHBA intKompUni LS_gender, i(idteach_d_FDZ1) link(oprobit) pweight(w)
estat ic 

// IMP 7
use "E:\SZ\Projekt_LehrerNoten\Results\impData7.dta", clear
destring trnotedeu_r, replace
gen w1 = wgt_L
gen w2 = 1
gllamm trnotedeu_r wle_ortho BEFKIwle AnstrB TR_geschlecht TR_SPF_r FHBAm UNIm mig GY Pspfges SuS_KL wle_orthoM AntMig AntMaed_KL SysMod Lgender Llja_FDZ querFremdYes intGeschFHBA intGeschUni intKompFHBA intKompUni LS_gender, i(idteach_d_FDZ1) link(oprobit) pweight(w)
estat ic 

// IMP 8
use "E:\SZ\Projekt_LehrerNoten\Results\impData8.dta", clear
destring trnotedeu_r, replace
gen w1 = wgt_L
gen w2 = 1
gllamm trnotedeu_r wle_ortho BEFKIwle AnstrB TR_geschlecht TR_SPF_r FHBAm UNIm mig GY Pspfges SuS_KL wle_orthoM AntMig AntMaed_KL SysMod Lgender Llja_FDZ querFremdYes intGeschFHBA intGeschUni intKompFHBA intKompUni LS_gender, i(idteach_d_FDZ1) link(oprobit) pweight(w)
estat ic 

// IMP 9: do not converge
use "E:\SZ\Projekt_LehrerNoten\Results\impData9.dta", clear
destring trnotedeu_r, replace
gen w1 = wgt_L
gen w2 = 1
gllamm trnotedeu_r wle_ortho BEFKIwle AnstrB TR_geschlecht TR_SPF_r FHBAm UNIm mig GY Pspfges SuS_KL wle_orthoM AntMig AntMaed_KL SysMod Lgender Llja_FDZ querFremdYes intGeschFHBA intGeschUni intKompFHBA intKompUni LS_gender, i(idteach_d_FDZ1) link(oprobit) pweight(w)
estat ic 

// IMP 10
use "E:\SZ\Projekt_LehrerNoten\Results\impData10.dta", clear
destring trnotedeu_r, replace
gen w1 = wgt_L
gen w2 = 1
gllamm trnotedeu_r wle_ortho BEFKIwle AnstrB TR_geschlecht TR_SPF_r FHBAm UNIm mig GY Pspfges SuS_KL wle_orthoM AntMig AntMaed_KL SysMod Lgender Llja_FDZ querFremdYes intGeschFHBA intGeschUni intKompFHBA intKompUni LS_gender, i(idteach_d_FDZ1) link(oprobit) pweight(w)
estat ic 

// IMP 11 
use "E:\SZ\Projekt_LehrerNoten\Results\impData11.dta", clear
destring trnotedeu_r, replace
gen w1 = wgt_L
gen w2 = 1
gllamm trnotedeu_r wle_ortho BEFKIwle AnstrB TR_geschlecht TR_SPF_r FHBAm UNIm mig GY Pspfges SuS_KL wle_orthoM AntMig AntMaed_KL SysMod Lgender Llja_FDZ querFremdYes intGeschFHBA intGeschUni intKompFHBA intKompUni LS_gender, i(idteach_d_FDZ1) link(oprobit) pweight(w)
estat ic 

// IMP 12 
use "E:\SZ\Projekt_LehrerNoten\Results\impData12.dta", clear
destring trnotedeu_r, replace
gen w1 = wgt_L
gen w2 = 1
gllamm trnotedeu_r wle_ortho BEFKIwle AnstrB TR_geschlecht TR_SPF_r FHBAm UNIm mig GY Pspfges SuS_KL wle_orthoM AntMig AntMaed_KL SysMod Lgender Llja_FDZ querFremdYes intGeschFHBA intGeschUni intKompFHBA intKompUni LS_gender, i(idteach_d_FDZ1) link(oprobit) pweight(w)
estat ic 

// IMP 13 
use "E:\SZ\Projekt_LehrerNoten\Results\impData13.dta", clear
destring trnotedeu_r, replace
gen w1 = wgt_L
gen w2 = 1
gllamm trnotedeu_r wle_ortho BEFKIwle AnstrB TR_geschlecht TR_SPF_r FHBAm UNIm mig GY Pspfges SuS_KL wle_orthoM AntMig AntMaed_KL SysMod Lgender Llja_FDZ querFremdYes intGeschFHBA intGeschUni intKompFHBA intKompUni LS_gender, i(idteach_d_FDZ1) link(oprobit) pweight(w)
estat ic 

// IMP 14
use "E:\SZ\Projekt_LehrerNoten\Results\impData14.dta", clear
destring trnotedeu_r, replace
gen w1 = wgt_L
gen w2 = 1
gllamm trnotedeu_r wle_ortho BEFKIwle AnstrB TR_geschlecht TR_SPF_r FHBAm UNIm mig GY Pspfges SuS_KL wle_orthoM AntMig AntMaed_KL SysMod Lgender Llja_FDZ querFremdYes intGeschFHBA intGeschUni intKompFHBA intKompUni LS_gender, i(idteach_d_FDZ1) link(oprobit) pweight(w)
estat ic 

// IMP 15
use "E:\SZ\Projekt_LehrerNoten\Results\impData15.dta", clear
destring trnotedeu_r, replace
gen w1 = wgt_L
gen w2 = 1
gllamm trnotedeu_r wle_ortho BEFKIwle AnstrB TR_geschlecht TR_SPF_r FHBAm UNIm mig GY Pspfges SuS_KL wle_orthoM AntMig AntMaed_KL SysMod Lgender Llja_FDZ querFremdYes intGeschFHBA intGeschUni intKompFHBA intKompUni LS_gender, i(idteach_d_FDZ1) link(oprobit) pweight(w)
estat ic 

// IMP 16 
use "E:\SZ\Projekt_LehrerNoten\Results\impData16.dta", clear
destring trnotedeu_r, replace
gen w1 = wgt_L
gen w2 = 1
gllamm trnotedeu_r wle_ortho BEFKIwle AnstrB TR_geschlecht TR_SPF_r FHBAm UNIm mig GY Pspfges SuS_KL wle_orthoM AntMig AntMaed_KL SysMod Lgender Llja_FDZ querFremdYes intGeschFHBA intGeschUni intKompFHBA intKompUni LS_gender, i(idteach_d_FDZ1) link(oprobit) pweight(w)
estat ic 

// IMP 17
use "E:\SZ\Projekt_LehrerNoten\Results\impData17.dta", clear
destring trnotedeu_r, replace
gen w1 = wgt_L
gen w2 = 1
gllamm trnotedeu_r wle_ortho BEFKIwle AnstrB TR_geschlecht TR_SPF_r FHBAm UNIm mig GY Pspfges SuS_KL wle_orthoM AntMig AntMaed_KL SysMod Lgender Llja_FDZ querFremdYes intGeschFHBA intGeschUni intKompFHBA intKompUni LS_gender, i(idteach_d_FDZ1) link(oprobit) pweight(w)
estat ic 

// IMP 18 
use "E:\SZ\Projekt_LehrerNoten\Results\impData18.dta", clear
destring trnotedeu_r, replace
gen w1 = wgt_L
gen w2 = 1
gllamm trnotedeu_r wle_ortho BEFKIwle AnstrB TR_geschlecht TR_SPF_r FHBAm UNIm mig GY Pspfges SuS_KL wle_orthoM AntMig AntMaed_KL SysMod Lgender Llja_FDZ querFremdYes intGeschFHBA intGeschUni intKompFHBA intKompUni LS_gender, i(idteach_d_FDZ1) link(oprobit) pweight(w)
estat ic 

// IMP 19 
use "E:\SZ\Projekt_LehrerNoten\Results\impData19.dta", clear
destring trnotedeu_r, replace
gen w1 = wgt_L
gen w2 = 1
gllamm trnotedeu_r wle_ortho BEFKIwle AnstrB TR_geschlecht TR_SPF_r FHBAm UNIm mig GY Pspfges SuS_KL wle_orthoM AntMig AntMaed_KL SysMod Lgender Llja_FDZ querFremdYes intGeschFHBA intGeschUni intKompFHBA intKompUni LS_gender, i(idteach_d_FDZ1) link(oprobit) pweight(w)
estat ic 

// IMP 20
use "E:\SZ\Projekt_LehrerNoten\Results\impData20.dta", clear
destring trnotedeu_r, replace
gen w1 = wgt_L
gen w2 = 1
gllamm trnotedeu_r wle_ortho BEFKIwle AnstrB TR_geschlecht TR_SPF_r FHBAm UNIm mig GY Pspfges SuS_KL wle_orthoM AntMig AntMaed_KL SysMod Lgender Llja_FDZ querFremdYes intGeschFHBA intGeschUni intKompFHBA intKompUni LS_gender, i(idteach_d_FDZ1) link(oprobit) pweight(w)
estat ic 

// IMP 21
use "E:\SZ\Projekt_LehrerNoten\Results\impData21.dta", clear
destring trnotedeu_r, replace
gen w1 = wgt_L
gen w2 = 1
gllamm trnotedeu_r wle_ortho BEFKIwle AnstrB TR_geschlecht TR_SPF_r FHBAm UNIm mig GY Pspfges SuS_KL wle_orthoM AntMig AntMaed_KL SysMod Lgender Llja_FDZ querFremdYes intGeschFHBA intGeschUni intKompFHBA intKompUni LS_gender, i(idteach_d_FDZ1) link(oprobit) pweight(w)
estat ic 

// IMP 22: do not converge
use "E:\SZ\Projekt_LehrerNoten\Results\impData22.dta", clear
destring trnotedeu_r, replace
gen w1 = wgt_L
gen w2 = 1
gllamm trnotedeu_r wle_ortho BEFKIwle AnstrB TR_geschlecht TR_SPF_r FHBAm UNIm mig GY Pspfges SuS_KL wle_orthoM AntMig AntMaed_KL SysMod Lgender Llja_FDZ querFremdYes intGeschFHBA intGeschUni intKompFHBA intKompUni LS_gender, i(idteach_d_FDZ1) link(oprobit) pweight(w)
estat ic 

// IMP 23 
use "E:\SZ\Projekt_LehrerNoten\Results\impData23.dta", clear
destring trnotedeu_r, replace
gen w1 = wgt_L
gen w2 = 1
gllamm trnotedeu_r wle_ortho BEFKIwle AnstrB TR_geschlecht TR_SPF_r FHBAm UNIm mig GY Pspfges SuS_KL wle_orthoM AntMig AntMaed_KL SysMod Lgender Llja_FDZ querFremdYes intGeschFHBA intGeschUni intKompFHBA intKompUni LS_gender, i(idteach_d_FDZ1) link(oprobit) pweight(w)
estat ic 

// IMP 24
use "E:\SZ\Projekt_LehrerNoten\Results\impData24.dta", clear
destring trnotedeu_r, replace
gen w1 = wgt_L
gen w2 = 1
gllamm trnotedeu_r wle_ortho BEFKIwle AnstrB TR_geschlecht TR_SPF_r FHBAm UNIm mig GY Pspfges SuS_KL wle_orthoM AntMig AntMaed_KL SysMod Lgender Llja_FDZ querFremdYes intGeschFHBA intGeschUni intKompFHBA intKompUni LS_gender, i(idteach_d_FDZ1) link(oprobit) pweight(w)
estat ic 

// IMP 25 
use "E:\SZ\Projekt_LehrerNoten\Results\impData25.dta", clear
destring trnotedeu_r, replace
gen w1 = wgt_L
gen w2 = 1
gllamm trnotedeu_r wle_ortho BEFKIwle AnstrB TR_geschlecht TR_SPF_r FHBAm UNIm mig GY Pspfges SuS_KL wle_orthoM AntMig AntMaed_KL SysMod Lgender Llja_FDZ querFremdYes intGeschFHBA intGeschUni intKompFHBA intKompUni LS_gender, i(idteach_d_FDZ1) link(oprobit) pweight(w)
estat ic 

// IMP 26 
use "E:\SZ\Projekt_LehrerNoten\Results\impData26.dta", clear
destring trnotedeu_r, replace
gen w1 = wgt_L
gen w2 = 1
gllamm trnotedeu_r wle_ortho BEFKIwle AnstrB TR_geschlecht TR_SPF_r FHBAm UNIm mig GY Pspfges SuS_KL wle_orthoM AntMig AntMaed_KL SysMod Lgender Llja_FDZ querFremdYes intGeschFHBA intGeschUni intKompFHBA intKompUni LS_gender, i(idteach_d_FDZ1) link(oprobit) pweight(w)
estat ic 

// Start here
// IMP 27
use "E:\SZ\Projekt_LehrerNoten\Results\impData27.dta", clear
destring trnotedeu_r, replace
gen w1 = wgt_L
gen w2 = 1
gllamm trnotedeu_r wle_ortho BEFKIwle AnstrB TR_geschlecht TR_SPF_r FHBAm UNIm mig GY Pspfges SuS_KL wle_orthoM AntMig AntMaed_KL SysMod Lgender Llja_FDZ querFremdYes intGeschFHBA intGeschUni intKompFHBA intKompUni LS_gender, i(idteach_d_FDZ1) link(oprobit) pweight(w)
estat ic 

// IMP 28
use "E:\SZ\Projekt_LehrerNoten\Results\impData28.dta", clear
destring trnotedeu_r, replace
gen w1 = wgt_L
gen w2 = 1
gllamm trnotedeu_r wle_ortho BEFKIwle AnstrB TR_geschlecht TR_SPF_r FHBAm UNIm mig GY Pspfges SuS_KL wle_orthoM AntMig AntMaed_KL SysMod Lgender Llja_FDZ querFremdYes intGeschFHBA intGeschUni intKompFHBA intKompUni LS_gender, i(idteach_d_FDZ1) link(oprobit) pweight(w)
estat ic 

// IMP 29
use "E:\SZ\Projekt_LehrerNoten\Results\impData29.dta", clear
destring trnotedeu_r, replace
gen w1 = wgt_L
gen w2 = 1
gllamm trnotedeu_r wle_ortho BEFKIwle AnstrB TR_geschlecht TR_SPF_r FHBAm UNIm mig GY Pspfges SuS_KL wle_orthoM AntMig AntMaed_KL SysMod Lgender Llja_FDZ querFremdYes intGeschFHBA intGeschUni intKompFHBA intKompUni LS_gender, i(idteach_d_FDZ1) link(oprobit) pweight(w)
estat ic 

// IMP 30
use "E:\SZ\Projekt_LehrerNoten\Results\impData30.dta", clear
destring trnotedeu_r, replace
gen w1 = wgt_L
gen w2 = 1
gllamm trnotedeu_r wle_ortho BEFKIwle AnstrB TR_geschlecht TR_SPF_r FHBAm UNIm mig GY Pspfges SuS_KL wle_orthoM AntMig AntMaed_KL SysMod Lgender Llja_FDZ querFremdYes intGeschFHBA intGeschUni intKompFHBA intKompUni LS_gender, i(idteach_d_FDZ1) link(oprobit) pweight(w)
estat ic 
