//SY20ZR18 JOB 1,TMVT817,CLASS=A,MSGLEVEL=(1,1)
//*
//*  PROBLEM DESCRIPTION(S):
//*    TMVT817 -
//*      Modify AVR to recognize premounted volume but not
//*      attempt AVR mounts.
//*
//*   COMPONENT:  360S-CI505-EBB2218
//*
//*   APARS FIXED: TMVT817
//*
//*   SPECIAL CONDITIONS:
//*
//*   COMMENTS:
//*     LAST CHANGE:  2010/07/09
//*
//*     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:
//*
//*     MODULES
//*       IEFXV001
//*
//*     NOTE:  This usermod was previously called TMVT809.
//*
//TMVT817 EXEC PGM=IMASPZAP,REGION=128K
//SYSPRINT  DD SYSOUT=A
//SYSLIB    DD DISP=SHR,DSN=SYS1.LINKLIB
//SYSIN     DD *
NAME IEFXV001 IEFXV001
IDRDATA TMVT817
VER 057A 4780,97BC      BE    DONETEST    IF SO EXIT  18988 34620020
REP 057A 47F0,97BC      B     DONETEST    ALWAYS EXIT
//
