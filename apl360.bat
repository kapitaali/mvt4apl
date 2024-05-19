@ECHO OFF
REM **********************************************************************
REM ***                                                                ***
REM *** Script:  apl360.bat                                            ***
REM ***                                                                ***
REM *** Purpose: Run APL\360 on OS/360-MVT.                            ***
REM ***                                                                ***
REM *** Updated: 2013/03/23                                            ***
REM ***                                                                ***
REM **********************************************************************
REM *
REM * install APL\360 if necessary
REM *
if not exist aplinst\status call .\aplinst\install_apl360
if not exist aplinst\status goto:eof
REM *
REM * set environment
REM *
SET ARCH=32
IF DEFINED ProgramFiles(x86) SET ARCH=64
REM *
REM * IPL OS/360-MVT and start APL\360
REM *
SET HERCULES_RC=scripts\automatic_windows.rc
.\hercules\windows\%ARCH%\hercules -d -f conf\mvt_intcons.cnf
