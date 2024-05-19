//SY20ZR19 JOB 1,'TMVT822',MSGLEVEL=(1,1),CLASS=A,MSGCLASS=A
//*
//*   PROBLEM DESCRIPTION:
//*    TMVT822 -
//*       The XF assembler cannot accept blank input source records.
//*       Much assembler code written for the High-Level Assembler
//*       which would otherwise be fully processable by publicly
//*       available assemblers cannot be processed because of
//*       changes to rules for allowable input.  One such rule is
//*       the requirement for the "SPACE" assembler instruction
//*       whenever a blank line is to be produced in the output
//*       listing, whereas the High-Level Assembler (ASMA90) can
//*       also accept blank input records.
//*
//*       This usermod updates the XF assembler (IFOX00) to
//*       allow records with blanks in the first 72 columns as
//*       valid input.  New logic adds the internal text for the
//*       "SPACE" instruction in column 10 before the input record
//*       is parsed whenever a record is found to start with 72
//*       blanks.
//*
//*       The original MVS 3.8 version of this usermod has been
//*       modified to fit the IFOX00 assembler version that runs
//*       under MVT.
//*
//*   COMPONENT:  5752-SC103-EAS1102
//*
//*   SPECIAL CONDITIONS:
//*     NONE.
//*
//*   COMMENTS:
//*     LAST CHANGE:  2010/08/26
//*
//*     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:
//*     MODULES:
//*       IF0X0F00
//*
//*     NOTE:  This usermod is an MVT adaptation of Prycroft Six P/L
//*       public domain usermod for MVS 3.8 no. 3 by Greg Price.
//*       The MVS version is available at:
//*
//*         http://www.prycroft6.com.au/vs2mods/download/zp60003.zip
//*
//TMVT811 EXEC PGM=IMASPZAP,REGION=128K
//SYSPRINT  DD SYSOUT=A
//SYSLIB    DD DISP=SHR,DSN=SYS1.LINKLIB
//SYSIN     DD *
 NAME IFOX04 IFOX0F00
 IDRDATA TMVT822
VER 0076 12B1                      LTR   R11,R1
VER 0078 47F0,C02A                 B     RETURN
VER 0100 12BB                      LTR   R11,R11
VER 0102 47F0,C02A                 B     RETURN
VER 0302 C9C6D6E7F0C6F0F0          DC    C'IFOX0F00 16.34 01/18/78'
VER 030A 40F1F64BF3F440F0
VER 0312 F161F1F861F7F8
VER 0319 40404040404040   PATCH    DS    0X
VER 0320 4040404040404040
VER 0328 4040404040404040
VER 0330 4040404040404040
VER 0338 4040404040404040
REP 0078 47F0,C316                 B     NEWCODE
REP 0102 47F0,C316                 B     NEWCODE
**REP 0319 E2D7C1C3C5   **SPACE    DC    C'SPACE'
**REP 031E 9540,B000    **NEWCODE  CLI   0(R11),C' '
REP 0319 1C190A0C0E       SPACE    DC    X'1C190A0C0E' C'SPACE' in internal code
REP 031E 952F,B000        NEWCODE  CLI   0(R11),X'2F'  C' ' in internal code
REP 0322 4770,C32E                 BNE   GOBACK
REP 0326 D546,B001,B000            CLC   1(71,R11),0(R11)
REP 032C 4770,C32E                 BNE   GOBACK
REP 0330 D204,B009,C311            MVC   9(5,R11),SPACE
REP 0336 12BB             GOBACK   LTR   R11,R11
REP 0338 47F0,C02A                 B     RETURN
/*
//

