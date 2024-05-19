//SY20ZR13 JOB 1,TMVT804,CLASS=A,MSGLEVEL=(1,1)
//*
//*  PROBLEM DESCRIPTION(S):
//*    TMVT804 -
//*      Modify IEEVIPL to attach SYSVICMD subtask to issue
//*      IPL commands from 'SYS1.PARMLIB(COMMND00)'.
//*
//*   COMPONENT:  360S-CI505-EBB2218
//*
//*   APARS FIXED: TMVT804
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
//*       SGIEE00V
//*
//*     NOTE:  SGIEE00V is modified to attach program SYSVICMD.
//*       SYSVICMD is not currently included in this usermod.
//*
//TMVT804 EXEC PGM=IEBUPDTE,REGION=128K,PARM=MOD
//SYSPRINT  DD SYSOUT=A
//SYSUT1    DD DISP=SHR,DSN=SYS1.MODGEN
//SYSUT2    DD DISP=SHR,DSN=SYS1.MODGEN
//SYSIN     DD DATA
./ CHANGE NAME=SGIEE00V,LIST=ALL   >>>> 2006/07/08
         AGO   .MVT804A            Begin statements deleted by @TMVT804 07980000
.MVT804A ANOP ,                    End statements deleted by   @TMVT804 09140100
         EJECT ,                                               @TMVT804 09140150
         COPY  MVT804A -                                       @TMVT804 09140200
./ ADD NAME=MVT804A   >>>> 2003/07/13
*****    START OF STATEMENTS INSERTED BY TMVT804        *****  @TMVT804 00010000
************************************************************** @TMVT804 00020000
*                                                            * @TMVT804 00030000
*        TMVT804:  Automatic commands processing.            * @TMVT804 00040000
*                                                            * @TMVT804 00050000
*        If AUTO=NONE was not specified on the SET           * @TMVT804 00060000
*        command, attach SYSVICMD to process COMMNDxx        * @TMVT804 00070000
*        member of SYS1.PARMLIB.                             * @TMVT804 00080000
*                                                            * @TMVT804 00090000
************************************************************** @TMVT804 00100000
         SPACE 1                                               @TMVT804 00110000
*------------------------------------------------------------* @TMVT804 00120000
*        Test for initial WTO-only call and return           * @TMVT804 00130000
*        immediately if so.                                  * @TMVT804 00140000
*------------------------------------------------------------* @TMVT804 00150000
IP700    DS    0H -                Auto commands subroutine    @TMVT804 00160000
         CLI   CB1,X'00' -         Initial WTO-only call?      @TMVT804 00170000
         BE    IP799 -             Return if so                @TMVT804 00180000
         SPACE 1                                               @TMVT804 00190000
*------------------------------------------------------------* @TMVT804 00200000
*        Select the appropriate COMMNDxx suffix              * @TMVT804 00210000
*        based on user specification of AUTO flags.          * @TMVT804 00220000
*------------------------------------------------------------* @TMVT804 00230000
         LA    R14,CB8+1 -         Point to stop flags         @TMVT804 00240000
         LA    R15,7 -             Set count of stop flags     @TMVT804 00250000
IP760    DS    0H -                Loop looking at stop flags  @TMVT804 00260000
         CLI   0(R14),C'Y' -       Did user specify "Y"?       @TMVT804 00270000
         BNE   IP763 -             Branch if so to do it       @TMVT804 00280000
         LA    R14,1(,R14) -       Else point to next stop     @TMVT804 00290000
         BCT   R15,IP760 -         Back to look at it          @TMVT804 00300000
         B     IP777 -             All "N", skip commands      @TMVT804 00310000
IP763    DS    0H -                Use this COMMNDxx suffix    @TMVT804 00320000
         LA    R1,7 -              Calculate                   @TMVT804 00330000
         SR    R1,R15 -             offset                     @TMVT804 00340000
         SLL   R1,1 -                to suffix                 @TMVT804 00350000
         LA    R1,COMMNDSF(R1) -      and point to it          @TMVT804 00360000
         SPACE 1                                               @TMVT804 00370000
*------------------------------------------------------------* @TMVT804 00380000
*        Attach SYSVICMD subtask to issue commands           * @TMVT804 00390000
*        from COMMNDxx member of SYS1.PARMLIB.               * @TMVT804 00400000
*------------------------------------------------------------* @TMVT804 00410000
         L     R2,PARMSAVE -       Get PARMLIB UCB pointer     @TMVT804 00420000
         LTR   R2,R2 -             Is PARMLIB present?         @TMVT804 00430000
         BZ    IP799 -             Skip auto commands if not   @TMVT804 00440000
         XC    IPQPAM(16),IPQPAM - Zero parm list              @TMVT804 00450000
         ST    R2,IPQPAM -         Set UCB pointer in PARMLIB  @TMVT804 00460000
         ST    R1,IPQPAM+4 -       Set suffix pointer in plist @TMVT804 00470000
         LA    R1,IPQPAM -         Point to parm list          @TMVT804 00480000
         XC    BAIPL(4),BAIPL -    Zero subtask ECB            @TMVT804 00490000
         LA    R2,BAIPL -          Point to UCB for attach     @TMVT804 00500000
*        ATTACH EP=SYSVICMD,       Attach command subtask      @TMVT804 00510000
*              ECB=(R2),           R2 points to subtask ECB    @TMVT804 00520000
*              KEY=ZERO,           Place subtask in key zero   @TMVT804 00530000
*              SM=SUP,             And supervisor state        @TMVT804 00540000
*              JSTCB=YES,          Must be job step TCB        @TMVT804 00550000
*              SVAREA=YES,         Provide subtask save area   @TMVT804 00560000
*              SZERO=NO,           Don't share subpool zero    @TMVT804 00570000
*              GIVEJPQ=NO,         Don't pass job pack queue   @TMVT804 00580000
*              MF=(E,(1))          Use remote parameter list   @TMVT804 00590000
         ATTACH EP=SYSVICMD, -     Attach command subtask      @TMVT804+00600000
               ECB=(R2), -         R2 points to subtask ECB    @TMVT804+00610000
               KEY=ZERO, -         Place subtask in key zero   @TMVT804+00620000
               SM=SUP, -           And supervisor state        @TMVT804+00630000
               JSTCB=YES, -        Must be job step TCB        @TMVT804+00640000
               SVAREA=YES, -       Provide subtask save area   @TMVT804+00650000
               SZERO=NO, -         Don't share subpool zero    @TMVT804+00660000
               GIVEJPQ=NO, -       Don't pass job pack queue   @TMVT804+00670000
               MF=(E,(1)) -        Use remote parameter list   @TMVT804 00680000
         ST    R1,CB10 -           Save subtask TCB address    @TMVT804 00690000
*        WAIT  ECB=BAIPL -         Wait for subtask to end     @TMVT804 00700000
         WAIT  ECB=BAIPL -         Wait for subtask to end     @TMVT804 00710000
         LA    R1,CB10 -           Point to subtask TCB addr   @TMVT804 00720000
*        DETACH (1) -              Detach the subtask          @TMVT804 00730000
         DETACH (1) -              Detach the subtask          @TMVT804 00740000
         L     R1,BAIPL -          Get subtask RC              @TMVT804 00750000
         SLL   R1,8 -              Shift out                   @TMVT804 00760000
         SRL   R1,8 -               possible flag bits         @TMVT804 00770000
         LTR   R1,R1 -             Did subtask end normally?   @TMVT804 00780000
         BZ    IP799 -             Branch if so                @TMVT804 00790000
         SPACE 1                                               @TMVT804 00800000
IP777    DS    0H -                COMMND processing not done  @TMVT804 00810000
         CLI   CB1,X'00' -         Initial WTO only call?      @TMVT804 00820000
         BE    IP799 -             Exit if so                  @TMVT804 00830000
*        WTO   'IEA208I COMMND FUNCTION INOPERATIVE', -        @TMVT804 00840000
*              ROUTCDE=2 -                                     @TMVT804 00850000
         WTO   'IEA208I COMMND FUNCTION INOPERATIVE', -        @TMVT804+00860000
               ROUTCDE=2 -                                     @TMVT804 00870000
         B     IP799 -             Done with commands          @TMVT804 00880000
COMMNDSF DC    C'00010203040506'   COMMNDxx suffixes           @TMVT804 00890000
*****    END OF STATEMENTS INSERTED BY TMVT804          *****  @TMVT804 00900000
./ ENDUP
/*
//
