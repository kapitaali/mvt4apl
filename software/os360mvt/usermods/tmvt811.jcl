//SY20ZR18 JOB 1,TMVT811,CLASS=A,MSGLEVEL=(1,1)
//*
//*  PROBLEM DESCRIPTION(S):
//*    TMVT811 -
//*      Force display of "IEF868I ddd WTR WAITING FOR WORK"
//*      message (for some reason, the default is to suppress
//*      it if the control program isn't MFT).
//*
//*   COMPONENT:  360S-CI505-EBB2218
//*
//*   APARS FIXED: TMVT811
//*
//*   SPECIAL CONDITIONS:
//*
//*   COMMENTS:
//*     LAST CHANGE:  2010/07/09
//*
//*     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:
//*
//*     MODULES
//*       IEFSD084
//*
//*     NOTE:  This usermod was previously called TMVT805.
//*
//TMVT811 EXEC PGM=IMASPZAP,REGION=128K
//SYSPRINT  DD SYSOUT=A
//SYSLIB    DD DISP=SHR,DSN=SYS1.LINKLIB
//SYSIN     DD *
NAME IEFSD080 IEFSD084
VER 0004 5810,0010    L   R1,CVTPTR                  TEST FOR MFT IF NOT
VER 0008 9120,1074    TM  CVTDCB-CVTMAP(R1),CVT1SPS  BRANCH TO AVOID
VER 000C 4780,9062    BZ  SD84MESG                   MESSAGE
REP 000C 4700,9062    NOP SD84MESG                   SKIP BRANCH
IDRDATA TMVT811
//
