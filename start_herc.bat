@ECHO OFF
REM **********************************************************************
REM ***                                                                ***
REM *** Script:  start_herc.bat                                        ***
REM ***                                                                ***
REM *** Purpose: Hercules startup for OS/360-MVT with                  ***
REM ***          external console definitions.                         ***
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
REM * start Hercules
REM *
.\hercules\windows\%ARCH%\hercules -f conf\mvt_extcons.cnf
