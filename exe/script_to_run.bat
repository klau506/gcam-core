
@echo off

setlocal enabledelayedexpansion

set arg1=%1
set arg2=%2

set LOGFILE=run_%arg1%_%arg2%.log
call :LOG > %LOGFILE%
exit /B

:LOG
    echo %arg1%
    echo %arg2%
    for /l %%i in (%arg1%,1,%arg2%) do (
        echo Start iteration %%i

        set "config_file=C:\GCAM\GCAM_7.0_Claudia\gcam-core\exe\config_behaviour\config_%%i.xml"
        set "temp_file=C:\GCAM\GCAM_7.0_Claudia\gcam-core\exe\configuration_%arg1%_%arg2%.xml"
        
        echo Copy !config_file! !temp_file!
        copy !config_file! !temp_file!
        
        echo Running GCAM with !config_file!
        @echo | call run-gcam_withArgs.bat %arg1% %arg2%
        @REM @echo | call run-gcam_%arg1%_%arg2%.bat
    )

    echo GCAM runs done!

    "C:\Program Files\R\R-4.1.0\bin\R.exe" R CMD BATCH --vanilla "--args %arg1% %arg2%" C:\GCAM\GCAM_7.0_Claudia\gcam-core\input\gcamdata\study7_analysis\create_prj.R R_run_%arg1%_%arg2%.log

    echo Project created!

