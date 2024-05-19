//SY20ZR13 JOB 1,TMVT803,CLASS=A,MSGLEVEL=(1,1)
//*
//*  PROBLEM DESCRIPTION(S):
//*    TMVT803 -
//*      Modify DCM generation macro to set initial values:
//*          SEG=19
//*          DEL=N
//*          CON=N
//*          RNUM=01
//*          RTME=001
//*
//*   COMPONENT:  360S-CI505-EBB2218
//*
//*   APARS FIXED: TMVT803
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
//*       IEECHCB
//*
//TMVT803  EXEC PGM=IEBUPDTE,REGION=128K,PARM=MOD
//SYSPRINT  DD SYSOUT=A
//SYSUT1    DD DISP=SHR,DSN=SYS1.MODGEN
//SYSUT2    DD DISP=SHR,DSN=SYS1.MODGEN
//SYSIN     DD DATA
./ CHANGE NAME=IEECHCB
&L       SETA   19                                             @TMVT803 00705000
&L       SETA  &N/2                                            @TMVT803 00880000
*        THIS LINE DELETED BY -------------------------------- @TMVT803 06160021
DCMDEL   DC    C'RD'               DEL VALUE                   @TMVT803 06200021
DCMCON   DC    C'N'                CON VALUE                   @TMVT803 06250021
DCMRNUM  DC    FL1'19'             ROLL NUMBER INITIAL VALUE   @TMVT803 06400021
DCMRTME  DC    H'1'                ROLL TIME VALUE             @TMVT803 06450021
DCMRNUMD DC    FL1'19'             RNUM DEFAULT                @TMVT803 06650021
DCMRTMED DC    H'1'                RTME DEFAULT                @TMVT803 06700021
DCMOPTST DC    X'70'               SCREEN CONTROL OPTIONS      @TMVT803 07400021
./ ENDUP
/*
//
