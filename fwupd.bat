@ECHO OFF
SET DMIOUTFILE=dmiout.txt
SET DMIEXE=DMIDEC.EXE
SET DBPATH=db\mbver.csv
SET OUTCMD=OUT.BAT
IF EXIST %DMIOUTFILE% DEL %DMIOUTFILE%
%DMIEXE% -t 0,1,2 > %DMIOUTFILE%
FWUPD.EXE -d %DMIOUTFILE% -b %DBPATH% -c %OUTCMD%
IF ERRORLEVEL -1 GOTO ERROR
IF NOT EXIST %OUTCMD% GOTO ERROR
CALL %OUTCMD%
IF NOT ERRORLEVEL 0 GOTO ERROR
ECHO FINISH
GOTO CLEAR
:ERROR
ECHO ERROR
:CLEAR
IF EXIST %DMIOUTFILE% DEL %DMIOUTFILE%
IF EXIST %OUTCMD% DEL %OUTCMD%