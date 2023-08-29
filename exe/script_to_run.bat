
@echo off

setlocal enabledelayedexpansion

set N=25  REM Set the value of N to the desired number of iterations

for /l %%i in (1,1,%N%) do (
    echo Start iteration %%i

    set "config_file=C:\GCAM\GCAM_7.0_Claudia\gcam-core\exe\config_behaviour\config_%%i.xml"
    set "temp_file=C:\GCAM\GCAM_7.0_Claudia\gcam-core\exe\configuration.xml"
    REM set "config_file=C:\Users\claudia.rodes\Documents\GitHub\gcam-core\exe\config_behaviour\config_%%i.xml"
    REM set "temp_file=C:\Users\claudia.rodes\Documents\GitHub\gcam-core\exe\configuration.xml"
    
    echo Copy !config_file! !temp_file!
    copy !config_file! !temp_file!
    
    echo Running GCAM with !config_file!
    @echo | call run-gcam.bat
)

echo GCAM runs done!

"C:\Program Files\R\R-4.1.0\bin\R.exe" R CMD BATCH C:\GCAM\GCAM_7.0_Claudia\gcam-core\input\gcamdata\study7_analysis\create_prj.R

echo Project created!

