//VSASM   JOB 1,'INSTALL XF ASSEMBLER',
//            CLASS=A,MSGLEVEL=(1,1),MSGCLASS=A
//*
//*   PROBLEM DESCRIPTION(S):
//*     VSASM -
//*       Install OS/VS XF Assembler into SYS1.LINKLIB on MVT.
//*
//*    APARS FIXED:  VSASM
//*
//*    SPECIAL CONDITIONS:
//*      NONE:
//*
//*    COMMENTS:
//*      LAST CHANGE:  2010/04/03
//*
//*      THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:
//*
//*      MODULES
//*        IFOX00
//*        IFOX01
//*        IFOX02
//*        IFOX03
//*        IFOX04
//*        IFOX05
//*        IFOX06
//*        IFOX07
//*        IFOX11
//*        IFOX21
//*        IFOX31
//*        IFOX41
//*        IFOX42
//*        IFOX51
//*        IFOX61
//*        IFOX62
//*
//VSASM  EXEC PGM=IEWL,REGION=128K,PARM='LIST,MAP,NCAL'
//SYSUT1   DD UNIT=SYSDA,SPACE=(TRK,30)
//SYSLMOD  DD DISP=SHR,DSN=SYS1.LINKLIB
//SYSPRINT DD SYSOUT=A
//SYSLIN   DD DISP=OLD,DSN=XFASM.OBJ,
//            UNIT=2400-3,VOL=(,RETAIN,SER=VSASM),
//            LABEL=(1,SL),DCB=(RECFM=FB,LRECL=80,BLKSIZE=400)
//
