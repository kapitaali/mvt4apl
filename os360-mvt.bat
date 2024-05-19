@ECHO OFF
REM **********************************************************************
REM ***                                                                ***
REM *** Script:  os360-mvt.bat                                         ***
REM ***                                                                ***
REM *** Purpose: IPL OS/360-MVT.                                       ***
REM ***                                                                ***
REM *** Updated: 2013/03/23                                            ***
REM ***                                                                ***
REM **********************************************************************
REM *
REM * set environment
REM *
SET ARCH=32
IF DEFINED ProgramFiles(x86) SET ARCH=64
REM *
REM * IPL OS/360-MVT and start APL\360
REM *
SET HERCULES_RC=scripts\noapl.rc
.\hercules\windows\%ARCH%\hercules -d -f conf\mvt_intcons.cnf
