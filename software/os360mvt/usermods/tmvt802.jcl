//SY20ZR13 JOB 1,TMVT802,CLASS=A,MSGLEVEL=(1,1)
//*
//*  PROBLEM DESCRIPTION(S):
//*    TMVT802 -
//*      Modify display console defaults to eliminate
//*      out-of-line display areas
//*
//*   COMPONENT:  360S-CI505-EBB2218
//*
//*   APARS FIXED: TMVT802
//*
//*   SPECIAL CONDITIONS:
//*     IOGEN:  An I/O generation is required for implementation
//*       of the functionality added by this usermod.
//*       GENTYPE = IO
//*
//*   COMMENTS:
//*     LAST CHANGE:  2010/07/09
//*
//*     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:
//*
//*     MACROS
//*       IEECRDCM
//*
//TMVT802  EXEC PGM=IEBUPDTE,REGION=128K,PARM=MOD
//SYSPRINT  DD SYSOUT=A
//SYSUT1    DD DISP=SHR,DSN=SYS1.MODGEN
//SYSUT2    DD DISP=SHR,DSN=SYS1.MODGEN
//SYSIN     DD DATA
./ CHANGE NAME=IEECRDCM
         AGO   .NEG                FORCE NO AREA               @TMVT802 06805000
./ ENDUP
/*
//
