//SY20ZR18 JOB 1,TMVT815,CLASS=A,MSGLEVEL=(1,1)
//*
//*  PROBLEM DESCRIPTION(S):
//*    TMVT815 -
//*      Modify IEEVRCTL to set default MSGCLASS for started
//*      tasks and mount commands to Z.
//*
//*   COMPONENT:  360S-CI505-EBB2218
//*
//*   APARS FIXED: TMVT815
//*
//*   SPECIAL CONDITIONS:
//*
//*   COMMENTS:
//*     LAST CHANGE:  2010/07/09
//*
//*     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:
//*
//*     MODULES
//*       IEEVRCTL
//*
//*     NOTE:  This usermod was previously called TMVT811.
//*
//TMVT815 EXEC PGM=IMASPZAP,REGION=128K
//SYSPRINT  DD SYSOUT=A
//SYSLIB    DD DISP=SHR,DSN=SYS1.LINKLIB
//SYSIN     DD *
 NAME IEEVRCTL IEEVRCTL
 IDRDATA TMVT815
 VER 0574 C1   MSGCLASS=A
 REP 0574 E9   MSGCLASS=Z
//
