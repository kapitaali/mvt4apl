//VSLINK  JOB 1,'INSTALL HEWLF064',CLASS=A,MSGLEVEL=(1,1),MSGCLASS=A
//*
//*   PROBLEM DESCRIPTION(S):
//*     VSLINK -
//*       Install OS/VS Linkage Editor HEWLF064 into SYS1.LINKLIB
//*       on MVT.
//*
//*   APARS FIXED:  VSLINK
//*
//*   SPECIAL CONDITIONS:
//*     DEPENDENCY:
//*       The OS/VS IEBCOPY program (VSCOPY) is required to reload
//*       HEWLF064 from tape.
//*
//*     DEPENDENCY:
//*       The target library must be on a D/T3330.  VSCOPY will
//*       not reblock the load module during reload.
//*
//*   COMMENTS:
//*     LAST CHANGE:  2010/06/27
//*
//*     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:
//*
//*     MODULES
//*       HEWLF064
//*
//*     NOTE:
//*       The following zaps are applied to deal with
//*       differences in the ENQ/DEQ macro expansions
//*       between OS/360 and OS/VS:
//*
//*         NAME HEWLF064 HEWLFINT
//*          IDRDATA ENQFIX
//*          VER 02C0 C0,2C0800  OS/VS  ENQ LISTEND BYTE
//*          VER 02F0 C0,2C4000  OS/VS  ENQ LISTEND BYTE
//*          VER 0BC4 C0,2C4800  OS/VS  DEQ LISTEND BYTE
//*          VER 0BE4 C0,2C4000  OS/VS  DEQ LISTEND BYTE
//*          REP 02C0 FF         OS/360 ENQ LISTEND BYTE
//*          REP 02F0 FF         OS/360 ENQ LISTEND BYTE
//*          REP 0BC4 FF         OS/360 DEQ LISTEND BYTE
//*          REP 0BE4 FF         OS/360 DEQ LISTEND BYTE
//*         NAME HEWLF064 HEWLFFNL
//*          IDRDATA ENQFIX
//*          VER 0728 C0,2C4800  OS/VS  DEQ LISTEND BYTE
//*          VER 0748 C0,2C4000  OS/VS  DEQ LISTEND BYTE
//*          REP 0728 FF         OS/360 DEQ LISTEND BYTE
//*          REP 0748 FF         OS/360 DEQ LISTEND BYTE
//*
/*JOBPARM LINES=9999
//*MAIN LINES=100
//*
//VSLINK  EXEC PGM=VSCOPY,REGION=256K
//SYSPRINT DD  SYSOUT=A
//SYSUT1   DD  DISP=OLD,DSN=VSLINK.UNLOAD,
//             UNIT=TAPE,VOL=(,RETAIN,SER=VSLINK),
//             LABEL=(1,SL)
//SYSUT2   DD  DISP=SHR,DSN=SYS1.LINKLIB
//**SYSUT2   DD  DISP=SHR,DSN=SY.OS.LINKLIB
//SYSUT3   DD  UNIT=SYSDA,SPACE=(80,(60,45)),DISP=(NEW,DELETE)
//SYSIN    DD  *
  COPY INDD=SYSUT1,OUTDD=SYSUT2
  SELECT MEMBER=((HEWLF064,,R))
/*
//
