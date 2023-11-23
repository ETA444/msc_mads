* This file is comprised of 6 versions of my Stata script documenting and preserving the checkpoints along the way *
* If you are trying to replicate this study use the final version and note that latex table generation code is ran seperately *


* ----- Final - Version 6.0: added Latex table generation -----

* Load the estout package
ssc install estout

* Set wd
cd "REDACTED"

* Load data: in the orignal folder this file is called ITA_DCE_data
import delimited "#DATA\clean-dce_data.csv"

* Make var names uppercase
rename *, upper(*)

* Check correlations between IVs
corr C_TEC C_RES U_TEC U_PHA U_RES R_DEV R_PRO R_POL I_NOT I_INF I_OPT R_NO R_TRA

* Create matrix to place model fit criteria results
matrix modelfit = J(5, 3, .)
matrix colnames modelfit = model AIC LL
matrix rownames modelfit = CLM LCA_1C LCA_2C LCA_2C+InVal LogReg

* Model 1 (CLM): Conditional Logit Model

* Conditional Logit Model with Coefficients
clogit CHOICE C_TEC C_RES U_TEC U_PHA U_RES R_DEV R_PRO R_POL I_NOT I_INF I_OPT R_NO R_TRA, group(ID)

* Model fit checks
estat ic

* Store the AIC, and Log likelihood values for Model 1 (note: I got them manually from estat ic output)
matrix modelfit[1, 1] = 1
matrix modelfit[1, 2] = 3313.104 
matrix modelfit[1, 3] = -1643.552 


* Conditional Logit Model with Odds Ratios instead of Coefficients
clogit CHOICE C_TEC C_RES U_TEC U_PHA U_RES R_DEV R_PRO R_POL I_NOT I_INF I_OPT R_NO R_TRA, group(ID) or


* Model 2-4 (LCAs): 1-Class LCA, 2-Class LCA & 2-Class LCA with initial estimates from 1-Class LCA

* Model 2: Fit the latent class model for 1 class
gsem (CHOICE <- C_TEC C_RES U_TEC U_PHA U_RES R_DEV R_PRO R_POL I_NOT I_INF I_OPT R_NO R_TRA, lclass(LatentClass 1))

* Calculate AIC
local AIC = 2*e(rank) - 2*e(ll)

* Store the AIC, and Log likelihood values for Model 2
matrix modelfit[2, 1] = 2
matrix modelfit[2, 2] = `AIC'
matrix modelfit[2, 3] = e(ll)


* Store the coefficient vector as a matrix to be used as initial estimates in Model 4
matrix b_init = e(b)




* Model 3: Fit the latent class model for 2 classes (without initial values)
gsem (CHOICE <- C_TEC C_RES U_TEC U_PHA U_RES R_DEV R_PRO R_POL I_NOT I_INF I_OPT R_NO R_TRA, lclass(LatentClass 2))


* Calculate AIC
local AIC = 2*e(rank) - 2*e(ll)

* Store the AIC, and Log likelihood values for Model 3
matrix modelfit[3, 1] = 3
matrix modelfit[3, 2] = `AIC'
matrix modelfit[3, 3] = e(ll)



* Model 4: Fit the latent class model for 2 classes (with initial values)
gsem (CHOICE <- C_TEC C_RES U_TEC U_PHA U_RES R_DEV R_PRO R_POL I_NOT I_INF I_OPT R_NO R_TRA, lclass(LatentClass 2)), from(b_init)


* Calculate probabilities of class membership
estat lcprob

* Calculate AIC
local AIC = 2*e(rank) - 2*e(ll)

* Store the AIC, and Log likelihood values for Model 4
matrix modelfit[4, 1] = 4
matrix modelfit[4, 2] = `AIC'
matrix modelfit[4, 3] = e(ll)



* Model 5: Logistic Regression 

* Fit Logistic Regression
logit CHOICE C_TEC C_RES U_TEC U_PHA U_RES R_DEV R_PRO R_POL I_NOT I_INF I_OPT R_NO R_TRA


* Get Odds Ratios for easier interpretation
logit, or

* Model fit
estat ic


* Store the AIC, and Log likelihood values for Model 5 (note: I got them manually from estat ic)
matrix modelfit[5, 1] = 5
matrix modelfit[5, 2] = 6299.928
matrix modelfit[5, 3] = -3135.964


* Show the model fit matrix and decide which are the best models 
matrix list modelfit





* > Run Seperately - Latex table generation < *
* In order to get my final tables as seen in my thesis I did additional edits in Overleaf *


* Model 1 (CLM): Conditional Logit Model

* Conditional Logit Model with Coefficients
clogit CHOICE C_TEC C_RES U_TEC U_PHA U_RES R_DEV R_PRO R_POL I_NOT I_INF I_OPT R_NO R_TRA, group(ID)

* Export the results to a LaTeX file: Model 1
esttab using model1.tex, replace tex


* Conditional Logit Model with Odds Ratios instead of Coefficients
clogit CHOICE C_TEC C_RES U_TEC U_PHA U_RES R_DEV R_PRO R_POL I_NOT I_INF I_OPT R_NO R_TRA, group(ID) or

* Export the results to a LaTeX file: Model 1 Odds Ratios
esttab using model1_oddsratios.tex, replace tex



* Model 2: Fit the latent class model for 1 class
gsem (CHOICE <- C_TEC C_RES U_TEC U_PHA U_RES R_DEV R_PRO R_POL I_NOT I_INF I_OPT R_NO R_TRA, lclass(LatentClass 1))

* Export the results to a LaTeX file: Model 2
esttab using model2.tex, replace tex


* Model 3: Fit the latent class model for 2 classes (without initial values)
gsem (CHOICE <- C_TEC C_RES U_TEC U_PHA U_RES R_DEV R_PRO R_POL I_NOT I_INF I_OPT R_NO R_TRA, lclass(LatentClass 2))

* Export the results to a LaTeX file: Model 3
esttab using model3.tex, replace tex


* Model 4: Fit the latent class model for 2 classes (with initial values)
gsem (CHOICE <- C_TEC C_RES U_TEC U_PHA U_RES R_DEV R_PRO R_POL I_NOT I_INF I_OPT R_NO R_TRA, lclass(LatentClass 2)), from(b_init)

* Export the results to a LaTeX file: Model 4
esttab using model4.tex, replace tex


* Model 5: Logistic Regression 

* Fit Logistic Regression
logit CHOICE C_TEC C_RES U_TEC U_PHA U_RES R_DEV R_PRO R_POL I_NOT I_INF I_OPT R_NO R_TRA

* Export the results to a LaTeX file: Model 5
esttab using model5.tex, replace tex




* ----- Version 5.0: added Conditional Logit Model and for now doing only 2-class LCA -----

* Set wd
cd "REDACTED"

* Load data
import delimited "DATA\clean-dce_data.csv"

* Make var names uppercase
rename *, upper(*)

* Check correlations between IVs
corr C_TEC C_RES U_TEC U_PHA U_RES R_DEV R_PRO R_POL I_NOT I_INF I_OPT R_NO R_TRA

* Create matrix to place model fit criteria results
matrix modelfit = J(5, 3, .)
matrix colnames modelfit = model AIC LL
matrix rownames modelfit = CLM LCA_1C LCA_2C LCA_2C+InVal LogReg

* Model 1 (CLM): Conditional Logit Model

* Conditional Logit Model with Coefficients
clogit CHOICE C_TEC C_RES U_TEC U_PHA U_RES R_DEV R_PRO R_POL I_NOT I_INF I_OPT R_NO R_TRA, group(ID)

* Model fit checks
estat ic

* Store the AIC, and Log likelihood values for Model 1 (note: I got them manually from estat ic output)
matrix modelfit[1, 1] = 1
matrix modelfit[1, 2] = 3313.104 
matrix modelfit[1, 3] = -1643.552 

* Conditional Logit Model with Odds Ratios instead of Coefficients
clogit CHOICE C_TEC C_RES U_TEC U_PHA U_RES R_DEV R_PRO R_POL I_NOT I_INF I_OPT R_NO R_TRA, group(ID) or


* Model 2-4 (LCAs): 1-Class LCA, 2-Class LCA & 2-Class LCA with initial estimates from 1-Class LCA

* Model 2: Fit the latent class model for 1 class
gsem (CHOICE <- C_TEC C_RES U_TEC U_PHA U_RES R_DEV R_PRO R_POL I_NOT I_INF I_OPT R_NO R_TRA, lclass(LatentClass 1))

* Calculate AIC
local AIC = 2*e(rank) - 2*e(ll)

* Store the AIC, and Log likelihood values for Model 2
matrix modelfit[2, 1] = 2
matrix modelfit[2, 2] = `AIC'
matrix modelfit[2, 3] = e(ll)

* Store the coefficient vector as a matrix to be used as initial estimates in Model 4
matrix b_init = e(b)


* Model 3: Fit the latent class model for 2 classes (without initial values)
gsem (CHOICE <- C_TEC C_RES U_TEC U_PHA U_RES R_DEV R_PRO R_POL I_NOT I_INF I_OPT R_NO R_TRA, lclass(LatentClass 2))

* Calculate AIC
local AIC = 2*e(rank) - 2*e(ll)

* Store the AIC, and Log likelihood values for Model 3
matrix modelfit[3, 1] = 3
matrix modelfit[3, 2] = `AIC'
matrix modelfit[3, 3] = e(ll)

* Model 4: Fit the latent class model for 2 classes (with initial values)
gsem (CHOICE <- C_TEC C_RES U_TEC U_PHA U_RES R_DEV R_PRO R_POL I_NOT I_INF I_OPT R_NO R_TRA, lclass(LatentClass 2)), from(b_init)

* Calculate probabilities of class membership
estat lcprob

* Calculate AIC
local AIC = 2*e(rank) - 2*e(ll)

* Store the AIC, and Log likelihood values for Model 4
matrix modelfit[4, 1] = 4
matrix modelfit[4, 2] = `AIC'
matrix modelfit[4, 3] = e(ll)



* Model 5: Logistic Regression 

* Fit Logistic Regression
logit CHOICE C_TEC C_RES U_TEC U_PHA U_RES R_DEV R_PRO R_POL I_NOT I_INF I_OPT R_NO R_TRA

* Get Odds Ratios for easier interpretation
logit, or

* Model fit
estat ic


* Store the AIC, and Log likelihood values for Model 5 (note: I got them manually from estat ic)
matrix modelfit[5, 1] = 5
matrix modelfit[5, 2] = 6299.928
matrix modelfit[5, 3] = -3135.964


* Show the model fit matrix and decide which are the best models 
matrix list modelfit



* ----- Version 4.0: improvement on third version - explicitly decrease tolerance + fix AIC calculation as gsem does not store AIC in e(AIC) -----

* Set wd
cd "REDACTED"

* Load data
import delimited "DATA\clean-dce_data.csv"

* Make var names uppercase
rename *, upper(*)

* Check correlations between IVs
corr C_TEC C_RES U_TEC U_PHA U_RES R_DEV R_PRO R_POL I_NOT I_INF I_OPT R_NO R_TRA

* Analysis
* Loop that tests multiple LCMs with differing number of classes and stores AIC and LL values to choose the best model)

* Set iteration limit and convergence tolerance
set maxiter 16000
set iterlog on

* Create a Stata matrix to store the results
matrix AIC_LL = J(6, 3, .)

* Fit the latent class model for 1 class
gsem (CHOICE <- C_TEC C_RES U_TEC U_PHA U_RES R_DEV R_PRO R_POL I_NOT I_INF I_OPT R_NO R_TRA, lclass(LatentClass 1))

* Calculate AIC
local AIC = 2*e(rank) - 2*e(ll)

* Store the number of classes, AIC, and Log likelihood values for 1 class
matrix AIC_LL[1, 1] = 1
matrix AIC_LL[1, 2] = `AIC'
matrix AIC_LL[1, 3] = e(ll)

* Store the coefficient vector as a matrix
matrix b_init = e(b)


* Loop through 2 to 6 classes
forvalues i = 2/6 {

    * Notify which iteration is in progress
    display "Fitting model for `i' classes now. "

    * Fit the latent class model with the stored matrix of initial values
    capture quietly gsem (CHOICE <- C_TEC C_RES U_TEC U_PHA U_RES R_DEV R_PRO R_POL I_NOT I_INF I_OPT R_NO R_TRA, lclass(LatentClass `i')), from(b_init) tolerance(1e-1)
    
    * Check if the model was successfully fitted
    if _rc == 0 {

        * Notify which iteration is in progress
        display "Successfully fit model for `i' classes!"
        * Calculate AIC
        local AIC = 2*e(rank) - 2*e(ll)

        * Store the number of classes, AIC, and Log likelihood values
        matrix AIC_LL[`i', 1] = `i'
        matrix AIC_LL[`i', 2] = `AIC'
        matrix AIC_LL[`i', 3] = e(ll)
        
        * Store the coefficient vector as a matrix for the next iteration
        matrix b_init = e(b)
    }
    else {
        display "Error in fitting model for `i' classes: " _rc
    }
}

* Show the AIC_LL matrix
matrix list AIC_LL




* ----- Version 3.0 VERSION: improvement on second version - we define the first class model outside of the loop and we fix the prev_model object to be a matrix -----

* Set wd
cd "REDACTED"

* Load data
import delimited "DATA\clean-dce_data.csv"

* Make var names uppercase
rename *, upper(*)

* Analysis
* Loop that tests multiple LCMs with differing number of classes and stores AIC and LL values to choose the best model)


* Set iteration limit and convergence tolerance
set maxiter 200
set iterlog on

* Create a Stata matrix to store the results
matrix AIC_LL = J(6, 3, .)

* Fit the latent class model for 1 class
gsem (CHOICE <- C_TEC C_RES U_TEC U_PHA U_RES R_DEV R_PRO R_POL I_NOT I_INF I_OPT R_NO R_TRA, lclass(LatentClass 1))

* Store the number of classes, AIC, and Log likelihood values for 1 class
matrix AIC_LL[1, 1] = 1
matrix AIC_LL[1, 2] = e(AIC)
matrix AIC_LL[1, 3] = e(ll)

* Store the coefficient vector as a matrix
matrix b_init = e(b)

* Loop through 2 to 6 classes
forvalues i = 2/6 {
    * Fit the latent class model with the stored matrix of initial values
    gsem (CHOICE <- C_TEC C_RES U_TEC U_PHA U_RES R_DEV R_PRO R_POL I_NOT I_INF I_OPT R_NO R_TRA, lclass(LatentClass `i')), from(b_init)
    
    * Check if the model was successfully fitted
    if _rc == 0 {
        * Store the number of classes, AIC, and Log likelihood values
        matrix AIC_LL[`i', 1] = `i'
        matrix AIC_LL[`i', 2] = e(AIC)
        matrix AIC_LL[`i', 3] = e(ll)
        
        * Store the coefficient vector as a matrix for the next iteration
        matrix b_init = e(b)
    }
    else {
        display "Error in fitting model for `i' classes: " _rc
    }
}

* Show the AIC_LL matrix
matrix list AIC_LL





* ----- Version 2.0: this loop uses starting values to hopefully improve chance of convergence -----

* Set wd
cd "REDACTED"

* Load data
import delimited "DATA\clean-dce_data.csv"

* Make var names uppercase
rename *, upper(*)

* Analysis
* Loop that tests multiple LCMs with differing number of classes and stores AIC and LL values to choose the best model)


* Set iteration limit and convergence tolerance
set maxiter 200
set iterlog on

* Create a Stata matrix to store the results
matrix AIC_LL = J(6, 3, .)

* Loop through 1 to 6 classes
forvalues i = 1/6 {
    * Fit the latent class model (capture quietly removed for now)
    if `i' == 1 {
        gsem (CHOICE <- C_TEC C_RES U_TEC U_PHA U_RES R_DEV R_PRO R_POL I_NOT I_INF I_OPT R_NO R_TRA, lclass(LatentClass `i'))
        
        * Store the parameter estimates for the current model
        estimates store prev_model
    }
    else {
        gsem (CHOICE <- C_TEC C_RES U_TEC U_PHA U_RES R_DEV R_PRO R_POL I_NOT I_INF I_OPT R_NO R_TRA, lclass(LatentClass `i')), from(prev_model)
    }
    
    * Check if the model was successfully fitted
    if _rc == 0 {
        * Store the number of classes, AIC, and Log likelihood values
        matrix AIC_LL[`i', 1] = `i'
        matrix AIC_LL[`i', 2] = e(AIC)
        matrix AIC_LL[`i', 3] = e(ll)
        
        * Store the parameter estimates for the current model
        estimates store prev_model
    }
    else {
        display "Error in fitting model for `i' classes: " _rc
    }
}

* Show the AIC_LL matrix
matrix list AIC_LL



* ----- Version 1.0: here we were attempting to fix convergence issue by increasing iteration limit, we also checked correlations -----

* Set wd
cd "REDACTED"

* Load data
import delimited "DATA\clean-dce_data.csv"

* Make var names uppercase
rename *, upper(*)

* Check correlations between IVs
corr C_TEC C_RES U_TEC U_PHA U_RES R_DEV R_PRO R_POL I_NOT I_INF I_OPT R_NO R_TRA

* Analysis
* Loop that tests multiple LCMs with differing number of classes and stores AIC and LL values to choose the best model)

* Set iteration limit 
set maxiter 200
set iterlog on

* Create a Stata matrix to store the results
matrix AIC_LL = J(6, 3, .)

* Loop through 1 to 6 classes
forvalues i = 1/4 {
    * Fit the latent class model (capture quietly  removed for now)
    gsem (CHOICE <- C_TEC C_RES U_TEC U_PHA U_RES R_DEV R_PRO R_POL I_NOT I_INF I_OPT R_NO R_TRA, lclass(LatentClass `i'))
    
    * Check if the model was successfully fitted
    if _rc == 0 {
        * Store the number of classes, AIC, and Log likelihood values
        matrix AIC_LL[`i', 1] = `i'
        matrix AIC_LL[`i', 2] = e(AIC)
        matrix AIC_LL[`i', 3] = e(ll)
    }
    else {
        display "Error in fitting model for `i' classes: " _rc
    }
}

* Show the AIC_LL matrix
matrix list AIC_LL


