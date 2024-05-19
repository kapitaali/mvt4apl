@ECHO OFF
REM **********************************************************************
REM ***                                                                ***
REM *** Script:  install_apl360.bat                                    ***
REM ***                                                                ***
REM *** Purpose: Install APL\360 on OS/360-MVT.                        ***
REM ***                                                                ***
REM *** Updated: 2013/03/23                                            ***
REM ***                                                                ***
REM **********************************************************************
REM *
REM * prepare
REM *
setlocal
if not exist aplinst\status goto checksrc
  echo APL\360 is already installed, use fresh mvt4apl copy to reinstall.
  set /p cont=Press any key to terminate...
  goto:eof
:checksrc
if exist source\APL360_source_code.txt goto ask4inst
  echo APL\360 source code missing, installation cannot start.
  set /p cont=Press any key to terminate...
  goto:eof
:ask4inst
echo starting APL\360 installation...
set /p cont=Continue [Y/n]?:
if /i "x%cont%" equ "xn" goto:eod
if    "x%cont%" equ "x"  goto install
if /i "x%cont%" neq "xy" goto:eod
:install
endlocal
REM *
REM * set environment
REM *
SET ARCH=32
IF DEFINED ProgramFiles(x86) SET ARCH=64
REM *
REM * remove old output and log files
REM *
if exist prt\JOB_output.txt erase /f prt\JOB_output.txt
if exist log\mvtlog.txt     erase /f log\mvtlog.txt
if exist .\aplinst.listings erase /f .\aplinst.listings
if exist .\aplinst.log      erase /f .\aplinst.log
REM *
REM * run automatic installation part 1
REM *
SET HERCULES_RC=aplinst\aplinst1.rc
.\hercules\windows\%ARCH%\hercules -d -f conf\mvt_intcons.cnf
REM *
REM * terminate on non zero return code
REM *
  if errorlevel 0 goto cont1
  echo Installation part 1 failed, Hercules RC = %errorlevel%
  setlocal
  set /p cont=Press any key to terminate...
  goto:eof
:cont1
REM *
REM * save output and log files
REM *
move prt\JOB_output.txt .\aplinst_part1.listings
move log\mvtlog.txt .\aplinst_part1.log
REM *
REM * run automatic installation part 2
REM *
SET HERCULES_RC=aplinst\aplinst2.rc
.\hercules\windows\%ARCH%\hercules -d -f conf\mvt_intcons.cnf
REM *
REM * terminate on non zero return code
REM *
  if errorlevel 0 goto cont2
  echo Installation part 2 failed, Hercules RC = %errorlevel%
  setlocal
  set /p cont=Press any key to terminate...
  goto:eof
:cont2
REM *
REM * save output and log files
REM *
move prt\JOB_output.txt .\aplinst_part2.listings
move log\mvtlog.txt .\aplinst_part2.log
REM *
REM * create APL\360 library
REM *
SET HERCULES_RC=aplinst\aplcreate.rc
.\hercules\windows\%ARCH%\hercules -d -f conf\mvt_intcons.cnf
REM *
REM * terminate on non zero return code
REM *
  if errorlevel 0 goto cont3
  echo Library creation failed, Hercules RC = %errorlevel%
  setlocal
  set /p cont=Press any key to terminate...
  goto:eof
:cont3
REM *
REM * save output and log files
REM *
copy .\aplinst_part1.listings + .\aplinst_part2.listings + prt\JOB_output.txt .\aplinst.listings
copy .\aplinst_part1.log + .\aplinst_part2.log + log\mvtlog.txt .\aplinst.log
REM *
REM * remove output and log files
REM *
erase /f prt\JOB_output.txt
erase /f prt\STC_and_TSO_output.txt
erase /f log\mvtlog.txt
erase /f .\aplinst_part1.listings
erase /f .\aplinst_part2.listings
erase /f .\aplinst_part1.log
erase /f .\aplinst_part2.log
REM *
REM * create status file
REM *
echo APL\360 installed on %Date% %Time% > aplinst\status
