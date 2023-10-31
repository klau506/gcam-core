
@echo off

setlocal enabledelayedexpansion

set database=%1

"C:\Program Files\R\R-4.1.0\bin\R.exe" R CMD BATCH --vanilla "--args %database%" C:\GCAM\GCAM_7.0_Claudia\gcam-core-spp\input\gcamdata\study7_analysis\create_prj_st7.R log_R_%database%.log