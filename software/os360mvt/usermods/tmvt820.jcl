//SY20ZR19 JOB 1,'TMVT820',CLASS=A,MSGCLASS=A
//*
//*++ USERMOD(TMVT820)     /* REWORK(20100709) */             .
//*++ VER (S218)
//*   FMID(EBB2218)
//*   PRE  (US06016,US06421,US07226,US07619)
//* /*
//*   PROBLEM DESCRIPTION(S):
//*     TMVT820 -
//*       Various errors taking SVC dumps when main storage size
//*       is 16MB.  Dump to disk will fail with errors such as
//*       msgIEE090I DUMP 2 I/O ERROR DUMP TERMINATED.  Dump to
//*       tape will run off end of the reel.
//*
//*       The dump PCI appendage DMPCEAPP in module IEAQAB00 is
//*       changed to use arithmetic operations instead of load
//*       address to increment dump addresses.
//*
//*       Tape dump module IGC0Z05A (source name IEAQAD0Z) is
//*       changed to use arithmetic operations instead of load
//*       address to increment dump addresses.
//*
//*   COMPONENT:  360S-CI535-EBB2218
//*
//*   APARS FIXED: TMVT820
//*
//*   SPECIAL CONDITIONS:
//*     DEPENDENCY:
//*       This usermod is intended for an MVT system with TSO and
//*       SUPRVSOR OPTIONS=CCH specified in stage 1.  Offsets to be
//*       zapped and linkage editor INSERT statements may not be
//*       accurate for any other configuration.
//*
//*     DEPENDENCY:
//*       The OS/VS linkage editor HEWLF064 must be available
//*       to permit IEAQAB00 to be expanded with a patch area.
//*
//*     DEPENDENCY:
//*       For SVC dump to disk to work correctly with 16MB, usermod
//*       TMVT801 must also be installed so that SYS1.DUMP data set
//*       space requirements are calculated correctly.
//*
//*   COMMENTS:
//*     LAST CHANGE:  2010/07/09
//*
//*     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:
//*
//*     MODULES
//*       IEAQAB00
//*       IGC0Z05A (source name IEAQAD0Z)
//*  */.
//*
//*-----------------------------------------------------------------***
//*     Step 1:  Relink IEANUC01 with OS/VS linkage editor to       ***
//*              add a patch area to IEAQAB00.                      ***
//*-----------------------------------------------------------------***
//EXPAND EXEC PGM=HEWLF064,
//            PARM='NCAL,DC,SIZE=(128K,6K),SCTR,LET,LIST,XREF',
//            REGION=256K
//NUCLEUS  DD DISP=SHR,DSN=SYS1.NUCLEUS 
//*           UNIT=SYSALLDA,VOL=SER=VRESA0
//SYSLMOD  DD DSN=&&TEMP,
//            DISP=(NEW,PASS),
//            SPACE=(TRK,(50,20,2)),
//            UNIT=SYSALLDA
//SYSUT1   DD DSN=&&SYSUT1,UNIT=SYSDA,SPACE=(1024,(150,20))
//SYSPRINT DD SYSOUT=A
//SYSLIN   DD *
  INSERT IEAANIP0
  INSERT IEAQFX00
  INSERT IGFCCH
  INCLUDE NUCLEUS(IEANUC01)
  EXPAND IEAQAB00(4)              /* ADD SPACE FOR PATCH AREA */
  NAME IEANUC05(R)
/*
//*
//*-----------------------------------------------------------------***
//*     Step 2:  ZAP expanded IEANUC01 IEAQAB00 CSECT with fix.     ***
//*-----------------------------------------------------------------***
//ZAP    EXEC PGM=IMASPZAP,COND=(4,LT,EXPAND)
//SYSLIB   DD DISP=SHR,DSN=*.EXPAND.SYSLMOD,VOL=REF=*.EXPAND.SYSLMOD
//SYSPRINT DD SYSOUT=A
//SYSIN    DD *
NAME IEANUC05 IEAQAB00
IDRDATA TMVT820
VER 000708 41AA0400          LA   R10,X400(R10) .    INCREMENT BY 1K
VER 0008E4 0000       PATCH  DC   XL2'00'            Patch area
REP 000708 4AA091E0          AH   R10,H1024          Increment by 1K
REP 0008E4 0400       H1024  DC   H'1024'            Halfword 1024
/*
//*
//*-----------------------------------------------------------------***
//*     Step 3:  Execute OS linkage editor to relink modified       ***
//*              IEANUC01 to SYS1.NUCLEUS.                          ***
//*-----------------------------------------------------------------***
//LKED   EXEC PGM=IEWL,COND=(0,NE,ZAP),
//            PARM='NCAL,DC,SIZE=(128K,6K),SCTR,LET,LIST,XREF',
//            REGION=256K
//SYSPUNCH DD DISP=(OLD,DELETE),DSN=&&TEMP
//*SYSLMOD  DD DISP=SHR,DSN=SYS1.NUCLEUS,
//*            UNIT=SYSALLDA,VOL=SER=VRESA0
//SYSLMOD  DD DSN=&&NUC,
//            DISP=(NEW,PASS),
//            SPACE=(TRK,(50,20,2)),
//            UNIT=SYSALLDA
//SYSUT1   DD DSN=&&SYSUT1,UNIT=SYSDA,SPACE=(1024,(150,20))
//SYSPRINT DD SYSOUT=A
//SYSLIN   DD *
  INSERT IEAANIP0
  INSERT IEAQFX00
  INSERT IGFCCH
  INCLUDE SYSPUNCH(IEANUC05)
  NAME IEANUC05(R)
/*
//*
//*-----------------------------------------------------------------***
//*     Step 4:  ZAP tape dump module IGC0Z05A (source name         ***
//*              IEAQAD0Z).                                         ***
//*-----------------------------------------------------------------***
//ZAP     EXEC PGM=IMASPZAP,REGION=128K
//SYSPRINT DD SYSOUT=A
//SYSLIB   DD DISP=SHR,DSN=SYS1.SVCLIB
//SYSIN    DD  *
NAME IGC0Z05A IGC0Z05A
IDRDATA TMVT820
VER 01B6 41AA0800       LA    R10,X800(R10)     INCREMENT ADDRESS BY 2K
VER 0368 1000     K4    DC    H'4096' 
VER 036A 0000           DC    H'0' 
REP 01B6 4AA0C368       AH    R10,K2            Add 2K to address
REP 036A 0800     K2    DC    H'2048'
/*
//
