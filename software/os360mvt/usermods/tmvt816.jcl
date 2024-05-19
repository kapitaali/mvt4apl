//SY20ZR18 JOB 1,TMVT816,MSGLEVEL=(1,1),CLASS=A,MSGCLASS=A
//*
//*  PROBLEM DESCRIPTION(S):
//*    TMVT816 -
//*      This usermod updates the 3277 console write module to
//*      update the 3270 field attribute byte for each in-line
//*      message line according to the action message status of
//*      the message shown on that line before the screen is
//*      written.
//*
//*   COMPONENT:  360S-CI505-EBB2218
//*
//*   APARS FIXED: TMVT816
//*
//*   SPECIAL CONDITIONS:
//*
//*  COMMENTS:
//*     LAST CHANGE:  2010/07/09
//*
//*     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:
//*
//*     MODULES
//*       IEECVETV
//*
//*     NOTE:  This usermod was previously called TMVT810.
//*
//*     NOTE:  This usermod is an MVT adaptation of Prycroft Six P/L
//*       public domain usermod for MVS 3.8 no. 4 by Greg Price.
//*       The MVS version is available at:
//*
//*         http://www.prycroft6.com.au/vs2mods/download/zp60004.zip
//*
//TMVT816 EXEC PGM=IMASPZAP,REGION=128K,COND=(0,NE)
//SYSPRINT DD SYSOUT=A
//SYSLIB   DD DISP=SHR,DSN=SYS1.SVCLIB,
//   UNIT=3330,VOL=SER=MVTRES
//SYSIN    DD  *
NAME IGC5V07B IEECVETV
IDRDATA TMVT816
VER 000000 05B0             START    BALR  RBBASE,N0           SET UP
VER 000002 47F0,B03A                 B     ICATCH              BRANCH AROUND EYE
VER 000006 40C9C7C3F5E5F0F7          DC    C' IGC5V07B '
VER 00000E C240
VER 000010 6040C9C5C5C3E5C5 PATCHLOC DC    C'- IEECVETV'
VER 000018 E3E5
VER 00001A 40E9C1D740E9C1D7          DC    C' ZAP ZAP'
VER 000022 40E9C1D740E9C1D7          DC    C' ZAP ZAP'
VER 00002A 40E9C1D740E9C1D7          DC    C' ZAP ZAP'
VER 000032 40E9C1D740E9C1D7          DC    C' ZAP ZAP'
VER 00003A 4000                      DC    X'4000'
VER 000096 9110,A129        FULLWRT  TM    DCMIOCM1,DCMWRMSG   WRITE FULL MESSAG
VER 00009A 47E0,B0C0        PATCHRET BNO   PARTWRT              NO, TEST NEXT FU
VER 0000C2 9108,A129                 TM    DCMIOCM1,DCMWRPAR   WRITE PARTIAL MES
VER 0003B6 0006             H6       DC    H'6'
REP 000096 47F0,B00E                 B     PATCH               TO PATCH AREA
REP 000010 5850,A030        PATCH    L     R5KEEP,DCMASCRN
REP 000014 4B50,B3B4                 SH    R5KEEP,H6
REP 000018 92E4,5004        LINELOOP MVI   N4(R5KEEP),LO
REP 00001C 915C,5009                 TM    N9(R5KEEP),ASTER
REP 000020 47E0,B026                 BNO   NEXTLINE   NOT * OR @
REP 000024 92E8,5004                 MVI   N4(R5KEEP),HI
REP 000028 4A50,A10C        NEXTLINE AH    R5KEEP,DCMCORLN
REP 00002C 5950,A04C                 C     R5KEEP,DCMPFKLN
REP 000030 4740,B016                 BL    LINELOOP
REP 000034 9110,A129                 TM    DCMIOCM1,DCMWRMSG
REP 000038 47F0,B098                 B     PATCHRET
//
