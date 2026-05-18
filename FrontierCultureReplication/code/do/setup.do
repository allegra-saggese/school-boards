clear all
set maxvar 30000
set matsize 10000
set linesize 255
cap log close _all
set more off
set varabbrev on
set scheme s1mono


foreach pkg in estout semipar reghdfe ftools psacalc ivreg2 ranktest acreg boottest weakiv { 
dis "Installing `pkg'"
cap ssc install `pkg', replace
}

