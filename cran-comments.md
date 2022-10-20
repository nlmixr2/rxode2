# CRAN Comments

> Dear maintainer,

> Please see the problems shown on
>  <https://cran.r-project.org/web/checks/check_results_nlmixr2extra.html>.

> Please correct before 2022-11-03 to safely retain your package on CRAN.

> The CRAN Team

 This is caused by using an 'rxode2' version linked to the wrong version of 'rxode2parse' or an older 'rxode2' version.
 
 This version of 'rxode2' directly refers to the version that is
 needed so no errors occur for 'nlmixr2est'.  'nlmixr2est' and
 'nlmixr2extra' will then directly use those versions as well.
 
