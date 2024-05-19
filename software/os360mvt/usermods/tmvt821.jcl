//SY20ZR19 JOB 1,'TMVT821',CLASS=A,MSGCLASS=A
//*
//*++ USERMOD(TMVT821)     /* REWORK(20100709) */             .
//*++ VER (S218)
//*   FMID(EBB2218)
//* /*
//*   PROBLEM DESCRIPTION(S):
//*     TMVT821 -
//*       DISPLAY ACTIVE and MONITOR ACTIVE produce incorrect
//*       output for end address of a region that ends at the
//*       top of storage when main storage size is 16 MB.
//*       IEEVDRGN correctly returns X'01000000' as the region
//*       end address, but SVC 110 module IEE50110 zeroes the
//*       high-order byte before formatting the address value,
//*       so the value used to build the display is X'00000000'.
//*
//*       IEE50110 is changed to display the correct value
//*       when region end address is X'01000000'.
//*
//*   COMPONENT:  360S-CI535-EBB2218
//*
//*   APARS FIXED: TMVT821
//*
//*   SPECIAL CONDITIONS:
//*     NONE:
//*
//*   COMMENTS:
//*     LAST CHANGE:  2010/07/09
//*
//*     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:
//*
//*     MODULES
//*       IEE50110
//*  */.
//*
//TMVT821  EXEC PGM=IMASPZAP
//SYSLIB   DD DISP=SHR,DSN=SYS1.SVCLIB
//SYSPRINT DD SYSOUT=A
//SYSIN    DD *
NAME IGC50110 IEE50110
IDRDATA TMVT821
VER 012E 41202000           LA  R2,E0(E0,R2)       CLEAR HIGH ORDER BYTE  
VER 02CC 010E4780           DC  XL1'01',XL3'trash'
REP 012E 5420C2CA           N   R2,MASK16M         Clear high-order 7 bits
REP 02CC 01FFFFFF  MASK16M  DC  XL4'01FFFFFF'
/*
//
