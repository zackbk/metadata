@echo off
For /f "tokens=2-4 delims=/ " %%a in ('date /t') do (set mydate=%%c%%a%%b)
For /f "tokens=1-2 delims=/:" %%a in ('time /t') do (set mytime=%%a%%b)
echo %mydate% with last created dates

FOR /f "tokens=*" %%G in ('dir /ad "H:\"') DO (DIR %%G /A /T:A /S /Q /R /N /-C > temp/C_%mydate%_%%G.txt)


pause