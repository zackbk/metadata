@echo off
For /f "tokens=1-4 delims=/ " %%a in ('date /t') do (set mydate=%%c%%b%%a)
For /f "tokens=1-2 delims=/:" %%a in ('time /t') do (set mytime=%%a%%b)
echo %mydate%

pause 3
