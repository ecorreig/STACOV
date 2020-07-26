# STACOV study statistical analysis

## Introduction

We present here the different data management and statistical methods carried out during the analysis of the STACOV study, to assess association between statin use and COVID-19 related mortality in patients in Catalonia. The scientific article is titled "EFFECT OF STATIN THERAPY ON SARS-CoV-2 INFECTION-RELATED
MORTALITY IN HOSPITALIZED PATIENTS".

As part of our transparency policy, we publish here any and all code that we used in the analysis. The variable names, the code and it's accompanying explanations are in Catalan, but we believe that any experienced R user will be able to follow it without major issues. If nevertheless you need further clarification, please write at eudald.correig@urv.cat. If you find errors or would like some part to be improved, please file and issue and we will look into it as soon as we can.

We have not included the data in the repo yet, but, if you want access to it please write an email at luis.masana@urv.cat and please include your university affiliation and the purpose of the study you want to do.


## Structure

`format.Rmd` file contains all the data management code. It puts together the 18 excel files from 18 hospitals, cleans typos, missidentifications, wrong values, etc. imputes missings and puts everything together in a csv file.

`matching.Rmd` takes this csv file and performs a regularized logistic regression to assess the propensity to receive statins, followed by a propensity matching procedure. A second csv with the matched database is produced.

`stacov_2.Rmd` carries out the univariant and multivariant survival models needed to assess association between statin use and mortality. Two mortality methods are carried out, a Cox model applied cause specific hazard ratio function and a Fine and Gray competing evenets model.

`stacov_retirades.Rmd` does the same analysis but dividing statin users by those whose treatment was withdrawn upon hospital admissions and those whose treatment was continued throughout the whole hospital stay.

In the helpers directory you will find many of the functions developed for this study. Some of them are forks from other repositories, others are a miss-match of functions borrowed here and there and some of them are completely new (and mostly a WIP). This work needs to be refined, properly referenced and probably packaged into a couple of R libraries, but this hasn't been done yet due to time constraints.

## Disclaimer

Obviously, this code comes with no guarantee whatsoever. You will find the licence under which we are publishing it at License.md.

This code will evolve in the future, since it'll be used for other, related studies. Please see the v1 branch to find the code exactly as it was on the moment of publication of the cited article.
