//FIX3  JOB 1,IEFACTRT,MSGLEVEL=(1,1),
//            CLASS=A,MSGCLASS=A
//*
//* 2007/08/06 @kl initiator fix job 3:
//*                  assemble and link updated IEFACTRT
//*                  into IEFSD061
//*
//*********************************************************************
//*                                                                 ***
//*    Job:      ASM-IEFACTRT                                       ***
//*    Product:  MVT 21.8 with ASP V3.2.                            ***
//*    Purpose:  Assemble and link step/job termination             ***
//*              exit IEFACTRT.                                     ***
//*    Update:   2007/08/06                                         ***
//*                                                                 ***
//*    Changes:  2007/08/06 Changed GETMAIN to use SQA              ***
//*                         and changed link to place               ***
//*                         directly into IEFSD061.                 ***
//*                                                                 ***
//*********************************************************************
//*
//ASM    EXEC PGM=IEUASM,
//            PARM='DECK,LIST,RENT',REGION=768K
//SYSLIB   DD DISP=SHR,DSN=SYS1.MACLIB,DCB=BLKSIZE=6160
//         DD DISP=SHR,DSN=SYS1.MODGEN,UNIT=3330,VOL=SER=DLIB01
//SYSUT1   DD DSN=&&SYSUT1,UNIT=SYSDA,SPACE=(1700,(600,100))
//SYSUT2   DD DSN=&&SYSUT2,UNIT=SYSDA,SPACE=(1700,(300,50))
//SYSUT3   DD DSN=&&SYSUT3,UNIT=SYSDA,SPACE=(1700,(300,50))
//SYSPRINT DD SYSOUT=A
//SYSLIN   DD DUMMY
//SYSGO    DD DUMMY
//SYSPUNCH DD DSN=&&OBJSET,UNIT=SYSDA,SPACE=(80,(200,50)),
//            DISP=(MOD,PASS)
//SYSIN    DD *
         TITLE 'IEFACTRT    SMF step/job termination exit'
***********************************************************************
*                                                                     *
*                                                                     *
* Module name          =  IEFACTRT                                    *
*                                                                     *
*                                                                     *
* Descriptive name     =  SMF termination exit for step and job       *
*                         end processing.                             *
*                                                                     *
*                                                                     *
* Function             =  To write statistical information about      *
*                         terminating step or job to the system       *
*                         messages data set; to issue WTO at step     *
*                         end describing step completion status.      *
*                                                                     *
*                                                                     *
* Notes                =  See below.                                  *
*                                                                     *
*   Dependencies       =  SMF must be installed for this exit         *
*                         to function.                                *
*                                                                     *
*                         R12 at entry to IEFACTRT points to the      *
*                         Linkage Control Table (LCT).  If R12        *
*                         is used by the exit, the original contents  *
*                         of R12 must be restored before the system   *
*                         message output routine IEFYS is called.     *
*                                                                     *
*                         LCT fields LCTJCTAD (address of Job         *
*                         Control Table) and LCTSCTAD (address of     *
*                         Step Control Table) are assumed to be       *
*                         storage addresses and not TTRs.             *
*                                                                     *
*   Registers          =  Not documented.                             *
*                                                                     *
*   Patch space        =  None.                                       *
*                                                                     *
*                                                                     *
* Module type          =  CSECT                                       *
*                                                                     *
*   Processor          =  OS System Assembler                         *
*                                                                     *
*   Module size        =  See assembly listing.                       *
*                                                                     *
*   Attributes         =  Reentrant, task mode, enabled,              *
*                         supervisor state, key 0.                    *
*                                                                     *
*                                                                     *
* Change activity      =                                              *
*                                                                     *
*   Flag  Date        By    Description                               *
*   ----  ----------  ----  ----------------------------------------  *
*   $D01  2007/08/06  @KL   Obtain storage from SQA.                  *
*                                                                     *
*                                                                     *
***********************************************************************
         EJECT ,
******************************************************************
*                                                                *
*        Define external symbols and register equates.           *
*                                                                *
******************************************************************
         SPACE 1
IEFACTRT CSECT ,
R0       EQU    0
R1       EQU    1
R2       EQU    2
R3       EQU    3
R4       EQU    4
R5       EQU    5
R6       EQU    6
R7       EQU    7
R8       EQU    8
R9       EQU    9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
         SPACE 1
*----------------------------------------------------------------*
*        Program initialization.                                 *
*----------------------------------------------------------------*
         SAVE  (14,12),,IEFACTRT   Save caller's registers
         LR    R11,R15             Load base register
         USING IEFACTRT,R11        Addressability for program
         LR    R10,R1              Save pointer to exit parm list
         EJECT ,
*----------------------------------------------------------------*
*        Ensure that SMF is active and return to caller if not.  *
*----------------------------------------------------------------*
         L     R15,CVTPTR          Get CVT pointer
         USING CVTMAP,R15          Addressability for CVT
         L     R15,CVTSMCA         Get SMCA address
         DROP  R15                 End CVT addressability
         LA    R15,0(,R15)         Clear high-order byte
         LTR   R15,R15             Is SMF active?
         BZ    RETURN              Skip processing if no SMF
         SPACE 1
*----------------------------------------------------------------*
*        If the current job is a TSO user, return immediately    *
*        to caller.  This exit does not perform any processing   *
*        for TSO users.                                          *
*----------------------------------------------------------------*
         L     R15,0(,R10)         Get CEPA address
         USING JMR,R15             Addressability for CEPA
         TM    JMRINDC,JMRFIND     Is this a TSO user?
         BO    RETURN              Skip processing for TSO user
          CLC  KTST,JMRJOB  ** testing **  Process this job?
          BE   RETURN       ** testing **  Return if no
         DROP  R15                 End CEPA addressability
         AGO  .NOSMASH
         SPACE 1
* XXX                       ** testing **
         L     R15,16              POINT TO CVT
         L     R15,148(,R15)       POINT TO BASEA
         MVC   38(2,R15),=H'256'   SMASH MINIMUM INIT REGION
* XXX                       ** testing **
.NOSMASH ANOP  ,
         SPACE 1
*----------------------------------------------------------------*
*        Establish addressability to SMF record.                 *
*----------------------------------------------------------------*
         L     R9,36(,R10)         Point to SMF record
         USING SMFRCD,R9           Addressability for record
         EJECT ,
*----------------------------------------------------------------*
*        Test to ensure that the SMF record is one that we       *
*        process and return immediately if it's anything else.   *
*----------------------------------------------------------------*
         LA    R15,12              Get compare value
         CR    R0,R15              Is entry for type 4 record?
         BE    GETWORK             Continue if so
         LA    R15,16              Get compare value
         CR    R0,R15              Is entry for type 5 record?
         BNE   RETURN              If not, return to caller
         TITLE '    Obtain work area'
******************************************************************
*                                                                *
*        GETWORK:                                                *
*                                                                *
*        Get work area storage and initialize it to zeroes.      *
*                                                                *
*        R9  = Address of SMF record - not referenced.           *
*        R10 = Exit routine parm list - set by this section.     *
*        R12 = Address of LCT - not referenced.                  *
*        R13 = Getmained work area - obtained in this routine.   *
*                                                                *
*        Workregs: R0, R1, R14, R15.                             *
*                                                                *
******************************************************************
         SPACE 1
GETWORK  DS    0H                  Obtain and zero work storage
         LA    R0,LWORKA           Set length for getmain          @D01
         LA    R15,243             Set                             @D01
         SLL   R15,24               subpool                        @D01
         OR    R0,R15                for getmain                   @D01
         GETMAIN R,LV=(0)          Get work area storage
         LR    R2,R1               Save address of getmained area
         LR    R0,R1               Set address for MVCL
         LA    R1,LWORKA           Set length for MVCL
         XR    R15,R15             Set pad to zeroes
         MVCL  R0,R14              Zero work area
         LR    R15,R13             Save old save area address
         LR    R13,R2              Point to our save area
         USING WORKA,R13           Addressability for work area
         ST    R15,PREVR13         Save caller's R13
**       ST    R15,SAVEA+4         Chain
**       ST    R13,8(,R15)          save areas
         LM    R0,R1,20(R15)       Restore R0-R1 at entry
         EJECT ,
         USING LCT,R12             ADDRESSABILITY FOR LCT
         L     R5,LCTJCTAD         GET JCT ADDRESS FROM LCT
         USING JCT,R5              ADDRESSABILITY FOR JCT
         L     R6,LCTSCTAD         GET SCT ADDRESS FROM LCT
         USING SCT,R6              ADDRESSABILITY FOR SCT
         EJECT ,
******************************************************************
*                                                                *
*        BLDCOM:                                                 *
*                                                                *
*        Build common heading line for the statistics            *
*        boxes.  This line contains OS release and               *
*        SMF system ID.  It will be written for both             *
*        step and job termination.                               *
*                                                                *
*        R9  = Address of SMF record.                            *
*        R10 = Exit routine parameter list - not referenced.     *
*        R12 = Address of LCT - not referenced.                  *
*                                                                *
*        Workregs: R0, R15.                                      *
*                                                                *
******************************************************************
         SPACE 1
BLDCOM   DS    0H                  Build common heading
         SPACE 1
*----------------------------------------------------------------*
*        Initialize heading line.                                *
*----------------------------------------------------------------*
         MVC   HDATA,MHDATA        Copy model heading data
         SPACE 1
*----------------------------------------------------------------*
*        Move SMF system ID to line.                             *
*----------------------------------------------------------------*
         MVC   HSID,SMF4SID        Move in SMF system ID
         SPACE 1
*----------------------------------------------------------------*
*        Build OS release and level.                             *
*----------------------------------------------------------------*
         L     R15,CVTPTR          Get CVT pointer
         LA    R0,CVTRELNO-CVTFIX+L'CVTRELNO  Back up to start
         SR    R15,R0               of CVT prefix
         USING CVTFIX,R15          Addressability for CVT prefix
         MVC   HOSREL,CVTNUMB      Set OS release
         MVC   HOSMOD,CVTLEVL      Set OS level
         SPACE 1
*----------------------------------------------------------------*
*        Build OS control program name.  Assume MVT.             *
*----------------------------------------------------------------*
         L     R15,CVTPTR          Point to CVT base
         USING CVT,R15             Addressability for CVT
         MVC   HOSCP,=C'MVT'       Set OS control program
         CLI   CVTDCB,CVT4MS1      Is this MVT?
         BE    CKRTYPE             Line is done if this is MVT
         SPACE 1
*----------------------------------------------------------------*
*        Not MVT, check for M65MP.                               *
*----------------------------------------------------------------*
         CLI   CVTDCB,CVT4MS1+CVT4MPS   Is this M65MP?
         BNE   CKCPSPS             Branch if not M65MP
         MVC   HOSCP,=C'MPS'       Set control program = M65MP
         B     CKRTYPE             Continue
         SPACE 1
*----------------------------------------------------------------*
*        Not M65MP, check for MFT.                               *
*----------------------------------------------------------------*
CKCPSPS  DS    0H                  Not M65MP, check for MFT
         CLI   CVTDCB,CVT2SPS      Is this MFT?
         BNE   CKCPSSS             Branch if not MFT
         MVC   HOSCP,=C'MFT'       Set control program = MFT
         B     CKRTYPE             Continue
         SPACE 1
*----------------------------------------------------------------*
*        None of the above, assume PCP.                          *
*----------------------------------------------------------------*
CKCPSSS  DS    0H                  Not MFT, assume PCP
         MVC   HOSCP,=C'PCP'       Set control program = PCP
         DROP  R15                 End CVT addressability
         EJECT ,
******************************************************************
*                                                                *
*        CKRTYPE:                                                *
*                                                                *
*        Test whether record is type 4 (step termination)        *
*        or type 5 (job termination), and branch to the          *
*        appropriate processing logic.                           *
*                                                                *
*        R9  = Address of SMF record.                            *
*        R10 = Exit routine parameter list - not referenced.     *
*        R12 = Address of LCT - not referenced.                  *
*                                                                *
*        Workregs: R0, R15.                                      *
*                                                                *
******************************************************************
         SPACE 1
CKRTYPE  DS    0H                  Step end or job end?
         CLI   SMF5RTY,X'05'       Is entry for type 5 record?
         BE    JOBEND              Branch if job end record
         EJECT ,
*        TITLE '    Step end processing'
******************************************************************
*                                                                *
*        STEPEND:                                                *
*                                                                *
*        End-of-step processing.                                 *
*                                                                *
******************************************************************
         SPACE 1
STEPEND  DS    0H                  Begin step end processing
         EJECT ,
******************************************************************
*                                                                *
*        SINITWTO:                                               *
*                                                                *
*        Initialize "STEP:" WTO message text.                    *
*                                                                *
*        R9  = Address of SMF record - not referenced.           *
*        R10 = Exit routine parameter list - not referenced.     *
*        R12 = Address of LCT - not referenced.                  *
*                                                                *
*        Workregs:                                               *
*                                                                *
******************************************************************
         SPACE 1
SINITWTO DS    0H                  Initialize "STEP:" WTO text
**       L     R15,ABLANKS         Point to blanks
**       MVC   SWTEXT,0(R15)       Blank WTO text area
         MVI   SWTEXT,C' '         Blank variable area
         MVC   SWTEXT+1(L'SWTEXT-1),SWTEXT
         MVC   SWHDR,=CL5'STEP:'   Set header in WTO
         LA    R15,SWTOLEN         Get length of WTO
         STH   R15,SWLEN           Set in parm list
         MVC   SWRTDESC,ROUT2      Set routcde
         XC    SWMCSFLG,SWMCSFLG   Zero mcsflags field
         OI    SWMCSFLG,SWMCSA     Show rout/desc codes exist
         EJECT ,
         SPACE 1
*----------------------------------------------------------------*
*        Step start time.                                        *
*----------------------------------------------------------------*
*        ICM   R1,15,SMF4SIT       Get step start time from record
*        LA    R15,SINITTM         Point to area for output
*        BAL   R7,TIMEX            Format step start time
         SPACE 1
*----------------------------------------------------------------*
*        Step end time.                                          *
*----------------------------------------------------------------*
         TIME  BIN                 Get step end date/time
         STCM  R1,15,DATEND        Save step end date
         LR    R4,R0               Save step end time
         LR    R1,R4               Set time for convert routine
         LA    R15,XENDTM          Point to output field
         BAL   R7,TIMEX            Format step end time
         SPACE 1
*----------------------------------------------------------------*
*        Step elapsed time.                                      *
*----------------------------------------------------------------*
          MVI   PAD,C' '             XXX  **** FIX THIS ****
         ICM   R0,15,SMF4SIT       Get step start time
         LR    R1,R4               Get step end time
         MVC   DATE,SMF4STID       Get start date
         LA    R15,XELAPS          Point to output area
         BAL   R7,ELAPSED          Calculate elapsed time
         SPACE 1
         EJECT ,
******************************************************************
*                                                                *
*        Build condition code or completion code.                *
*                                                                *
*        R9  = Address of SMF record.                            *
*        R10 = Exit routine parameter list.                      *
*        R12 = Address of LCT - not referenced.                  *
*                                                                *
*        Workregs:                                               *
*                                                                *
******************************************************************
         SPACE 1
SBLDCOMP DS    0H                  Build condition/abend code
         SPACE 1
*----------------------------------------------------------------*
*        Test for step flushed.                                  *
* Some of the following may be true XXX or may not be
* INCMSTS   job flushed due to COND on JOB statement
* JCTJOBFL  job failed - jcl error, or step flushed due to
*           JOB statement COND match on earlier step
* JCTSTPFL  step flushed due to COND on EXEC statement
* SCTABCAN  step flushed because earlier step abended
* SCTONLYC  step flushed because COND=ONLY was coded and no
*           prior step abended.
*----------------------------------------------------------------*
CONCKFLU DS    0H                  Test for step flushed
         TM    JCTJSTAT,JCTJOBFL+JCTSTPFL                              +
                                   Test for job or step flushed
         BZ    CONSETCD            Branch if not
         SPACE 1
*----------------------------------------------------------------*
*        Step was flushed.                                       *
*----------------------------------------------------------------*
CONFLUSH DS    0H                  Step was flushed
         MVC   XTEPCCH,HDFLUSH     Set "FLUSHED" header
         B     PRTCOND             Go to write the line
         SPACE 1
*----------------------------------------------------------------*
*        Not step flushed.  Save condition code/completion       *
*        code in work area.                                      *
*----------------------------------------------------------------*
CONSETCD DS    0H                  Save code in work area
         MVC   DWORK(2),SMF4SCC    Move code to work area
         MVC   XTEPCCHD,HDCOND     Assume condition code
         SPACE 1
*----------------------------------------------------------------*
*        Test whether condition code or abend code.              *
*----------------------------------------------------------------*
CONCKABN DS    0H                  Test for abend
         TM    LCTBATMN,TCBFA      Did step abend?
         BZ    CONRC               No: this is a condition code
         SPACE 1
*----------------------------------------------------------------*
*        Determine abend type.                                   *
*----------------------------------------------------------------*
CONCKTYP DS    0H                  Determine type of abend
         TM    SMF4SCC,X'80'       Is it a user abend?
         BO    CONABNDU            Branch if user abend
         EJECT ,
*----------------------------------------------------------------*
*        Format system abend code.                               *
*----------------------------------------------------------------*
CONABNDS DS    0H                  Format system abend code
         MVC   XTEPCCHD,HDABNDS    Set system abend header
         MVI   DWORK+2,X'0F'       Set dummy sign for unpack
         UNPK  DWORK+3(5),DWORK(3) Unpack cc
         MVC   STEPCOND(3),DWORK+4 Move cc to print line
         TR    STEPCOND(3),HEXTRAN-240   Translate hex to character
         B     PRTCOND             Go write the line
         SPACE 1
*----------------------------------------------------------------*
*        Prepare to build user abend code.                       *
*----------------------------------------------------------------*
CONABNDU DS    0H                  Abend is a user abend
         MVC   XTEPCCHD,HDABNDU    Set user abend header
         NI    DWORK,X'7F'         Lift high-order bit of code
         SPACE 1
*----------------------------------------------------------------*
*        Common logic to build condition code or user            *
*        abend code.                                             *
*----------------------------------------------------------------*
CONRC    DS    0H                  Build rc or user cc
         LH    R15,DWORK           Get rc/user cc
         CVD   R15,DWORK           Convert it to decimal
         UNPK  STEPCOND,DWORK      Unpack into print line
         OI    STEPCOND+3,X'F0'    Force sign printable
         SPACE 1
PRTCOND  DS    0H
         EJECT ,
******************************************************************
*                                                                *
*        SBLDWTO:                                                *
*                                                                *
*        Build and write "STEP:" step-end WTO message.           *
*                                                                *
*        R9  = Address of SMF record.                            *
*        R10 = Exit routine parameter list.                      *
*        R12 = Address of LCT - not referenced.                  *
*                                                                *
*        Workregs = R0, R1, R15.                                 *
*                                                                *
******************************************************************
         SPACE 1
SBLDWTO  DS    0H                  Build and write step-end WTO
         MVC   SWJOBN,JCTJNAME     Get job name
*         LA    R15,SWTOLEN         Get length of WTO
*         STH   R15,SWLEN           Set in parm list
         LA    R1,SWTOLEN-1         Get length of WTO
*         MVC   SWRTDESC,ROUT2      Set routcde
*         XC    SWMCSFLG,SWMCSFLG   Zero mcsflags field
*         OI    SWMCSFLG,SWMCSA     Show rout/desc codes exist
         SPACE 1
*----------------------------------------------------------------*
*        Set step name and procstep name in message.             *
*----------------------------------------------------------------*
SBLDWTOX DS    0H                  Not warmstart
         CLI   SCTSCLPC,C' '       Is step from a procedure?
         BNE   FOO1                Branch if not
         MVC   SWSTEPN,SCTSNAME    Set step name in WTO
         B     FOO2                Continue
FOO1     DS    0H                  Step is from a procedure
         MVC   SWSTEPN,SCTSCLPC    Set procstep name in WTO
         MVC   SWPSTEPN,SCTSNAME   Set step name in WTO
FOO2     DS    0H                  Done with step names
         EJECT ,
*----------------------------------------------------------------*
*        Test for step abend.                                    *
*----------------------------------------------------------------*
SBLDWTO1 DS    0H                  See if step abended
**       TM    SCTABCND,SCTABEND   Did step abend?
**       BO    SBLDWTO7            Branch if abend
         SPACE 1
*----------------------------------------------------------------*
*        Check to see if step was flushed.                       *
*----------------------------------------------------------------*
SBLDWTO3 DS    0H                  Test for step flushed
         TM    JCTJSTAT,JCTJOBFL   Job being flushed?
         BO    SBLDWTO5            Step was flushed if so
         TM    SCTSSTAT,INCMSSTS   Bypassed due to condition codes?
         BO    SBLDWTO5            Show flushed if so
         TM    SCTABCND,SCTABCAN   Prior abend?
         BO    SBLDWTO5            Step was flushed if so
         TM    SCTABCND,SCTONLYC   Bypassed due to COND=ONLY?
         BZ    SBLDWTOZ            Branch if step not flushed
         SPACE 1
*----------------------------------------------------------------*
*        Step was flushed.                                       *
*----------------------------------------------------------------*
SBLDWTO5 DS    0H                  Step was flushed
         MVI   SWSTARS+0,C'*'      Initialize field to "****"
         MVC   SWSTARS+1(L'SWSTARS-1),SWSTARS
**         L     R15,ASTARS          Point to line of "****"
**         MVC   SWSTARS,0(R15)      Fill elapsed time slot
         MVC   SWFLUSH,KFLUSHED    Set "FLUSHED" keyword
         B     SBLDWTOI            Go to issue message
         SPACE 1
*----------------------------------------------------------------*
*        Test if step abended or was canceled by the             *
*        operator.  For some reason, IEFYN copies TCBFLGS1       *
*        from the terminating job step TCB to LCTBATMN, then     *
*        clears the real bits in the TCB.  We must test          *
*        bit TCBFA in LCTBATMN (originally TCBFLG1) for          *
*        step abend/operator cancel.                             *
*----------------------------------------------------------------*
SBLDWTOZ DS    0H                  Not flushed, check for abend
         TM    LCTBATMN,TCBFA      Did step abend?
         BO    SBLDWTO7            Branch if abend
         SPACE 1
*----------------------------------------------------------------*
*        Neither abend nor step flushed, so build return code.   *
*----------------------------------------------------------------*
SBLDWTO6 DS    0H                  Build return code in message
         MVC   SWRCH,KRC           Set "RC=" keyword
         MVC   SWRC,STEPCOND       Move in step return code
         B     SBLDWTO8            Go build elapsed time
         SPACE 1
*----------------------------------------------------------------*
*        Step abended.  Build completion code for                *
*        abending step.                                          *
*----------------------------------------------------------------*
SBLDWTO7 DS    0H                  Step abended
         MVC   SWABNDH,KABEND      Set "ABEND" keyword in message
         MVC   SWABND,STEPCOND-1   Move in abend code
         TM    SMF4SCC,X'80'       Is it a user abend?
         BNO   SBLDWTO8            Branch if not user abend
         LA    R1,1(,R1)           Else adjust length for WPL
         SPACE 1
*----------------------------------------------------------------*
*        Copy step elapsed time to message.                      *
*----------------------------------------------------------------*
SBLDWTO8 DS    0H                  Build elapsed time in WTO
         MVC   SWELAPH,KET         Set "ELAPSED" keyword
         CLI   XELAPS-1,C' '       Elapsed hours in 3 digits?
         BNE   SBLDWTO9            Handle specially if so
         MVC   SWELAP,XELAPS       Move in step elapsed ttime
         B     SBLDWTOA            Go translate delimiters
         SPACE 1
*----------------------------------------------------------------*
*        Handle elapsed time with 3 digits of hours.             *
*----------------------------------------------------------------*
SBLDWTO9 DS    0H                  Build 3-digit elapsed time
         MVC   SWELAP(6),XELAPS-1  Move in HHH.MM
         SPACE 1
*----------------------------------------------------------------*
*        Translate all ":" in elapsed time to "."                *
*----------------------------------------------------------------*
SBLDWTOA DS    0H                  Translate ":" to "."
         LA    R14,8               Initialize loop counter
         SPACE 1
SBLDWTOB DS    0H                  Top of translate loop
         LA    R15,SWELAP-1(R14)   Point to current character
         CLI   0(R15),COLON        Is it a ":"?
         BNE   SBLDWTOC            Continue loop if not
         MVI   0(R15),DOT          Else change it to a "."
         SPACE 1
SBLDWTOC DS    0H                  Bottom of translate loop
         BCT   R14,SBLDWTOB        Back up and check previous
         SPACE 1
*----------------------------------------------------------------*
*        Issue message via WTO.                                  *
*----------------------------------------------------------------*
SBLDWTOI DS    0H                  Issue step-end message
         LA    R14,10              Limit for blank stripping  @DBG
SBLDWTOK DS    0H                                             @DBG
         LA    R15,SWTO-1(R1)      Point to last byte of text @DBG
         CLI   0(R15),C' '         Is last byte blank?        @DBG
         BNE   SBLDWTOJ            Branch if not blank        @DBG
         BCTR  R1,0                Else decrement WTO length  @DBG
         BCT   R14,SBLDWTOK        Loop at previous byte      @DBG
SBLDWTOJ DS    0H                                             @DBG
         STH   R1,SWLEN            Set length in WPL
         LA    R1,SWTO(R1)         Point past text
         MVC   0(4,R1),ROUT2       Set routcde
         XC    SWMCSFLG,SWMCSFLG   Zero mcsflags field
         OI    SWMCSFLG,SWMCSA     Show rout/desc codes exist
 AGO .PASTDB
****  start debugging messages
         TM    JCTJSTAT,INCMSTS    Is bit on?
         BZ    DBA                 Branch if not
         DBG   'ACTRT: JCTJSTAT,INCMSTS ON'
DBA      DS    0H
         TM    JCTJSTAT,JCTABEND   Is bit on?
         BZ    DBB                 Branch if not
         DBG   'ACTRT: JCTJSTAT,JCTABEND ON'
DBB      DS    0H
         TM    JCTJSTAT,JCTJOBFL   Is bit on?
         BZ    DBC                 Branch if not
         DBG   'ACTRT: JCTJSTAT,JCTJOBFL ON'
DBC      DS    0H
         TM    JCTJSTAT,JCTSTPFL   Is bit on?
         BZ    DBD                 Branch if not
         DBG   'ACTRT: JCTJSTAT,JCTSTPFL ON'
DBD      DS    0H
         TM    SCTSSTAT,INCMSSTS   Is bit on?
         BZ    DBE                 Branch if not
         DBG   'ACTRT: SCTSSTAT,INCMSSTS ON'
DBE      DS    0H
         TM    SCTABCND,SCTABCAN   Is bit on?
         BZ    DBF                 Branch if not
         DBG   'ACTRT: SCTABCND,SCTABCAN ON'
DBF      DS    0H
         TM    SCTABCND,SCTABEND   Is bit on?
         BZ    DBG                 Branch if not
         DBG   'ACTRT: SCTABCND,SCTABEND ON'
DBG      DS    0H
         TM    SCTABCND,SCTONLYC   Is bit on?
         BZ    DBH                 Branch if not
         DBG   'ACTRT: SCTABCND,SCTONLYC ON'
DBH      DS    0H
         TM    SCTBCT,SCTJBEND     Is bit on?
         BZ    DBI                 Branch if not
         DBG   'ACTRT: SCTBCT,SCTJBEND ON'
DBI      DS    0H
         TM    LCTQENTY,LCTERRM    Is bit on?
         BZ    DBJ                 Branch if not
         DBG   'ACTRT: LCTQENTY,LCTERRM ON'
DBJ      DS    0H
         CLI   LCTPARM1+2,7        Test if LCTPARM2+2 is 7
         BNE   DBK                 Branch if not
         DBG   'ACTRT: LCTPARM1+2 IS 7 (CANCEL)'
DBK      DS    0H
         CLI   LCTPARM1+2,6        Test if LCTPARM2+2 is 6
         BNE   DBL                 Branch if not
         DBG   'ACTRT: LCTPARM1+2 IS 6 (GETMAIN FAILURE)'
DBL      DS    0H
         CLI   LCTPARM1+2,5        Test if LCTPARM2+2 is 5
         BNE   DBM                 Branch if not
         DBG   'ACTRT: LCTPARM1+2 IS 5 (JOB CANCELLED)'
DBM      DS    0H
         CLI   LCTPARM1+2,4        Test if LCTPARM2+2 is 4
         BNE   DBN                 Branch if not
         DBG   'ACTRT: LCTPARM1+2 IS 4 (ALLOCATION ERROR)'
DBN      DS    0H
         L     R15,LCTTCBAD        Get address of TCB
         USING TCB,R15             Addressability for TCB
         TM    TCBFLGS1,TCBFA      Is bit on?
         BZ    DBO                 Branch if not
         DROP  R15                 End TCB addressability
         DBG   'ACTRT: TCBFLGS1,TCBFA ON'
DBO      DS    0H
         TM    LCTBATMN,X'80'      Is bit on?
         BZ    DBP                 Branch if not
         DBG   'ACTRT: LCTBATMN,X"80" ON'
DBP      DS    0H
         AGO   .FOO17
         L     R15,LCTERROR        Get LCT error code
         CVD   R15,DWORK           Convert it to packed
         OI    DWORK+7,X'0F'       Force printable sign
         UNPK  SWSTEPN(16),DWORK   Unpack into message
.FOO17   ANOP  ,
.PASTDB  ANOP  ,
****  end debugging messages
         LA    R1,SWTO             Point to message
         WTO   MF=(E,(1))          Write step-end WTO
         EJECT ,
*----------------------------------------------------------------*
*        Write a blank line.                                     *
*----------------------------------------------------------------*
         MVI   PRTLINE,C' '        Set blank in print line
         LA    R1,PRTLINE          Point to print line
         LA    R0,1                Get length of print line
         BAL   R7,PUTLINE          Write the line
         SPACE 1
*----------------------------------------------------------------*
*        Write "OS/360 ..." line.                                *
*----------------------------------------------------------------*
         MVI   PRTLINE+0,C'*'      Initialize line to "****"
         MVC   PRTLINE+1(L'PRTLINE-1),PRTLINE
         MVC   CHDATA,HDATA        Copy heading data
         LA    R1,PRTLINE          Point to print line
         LA    R0,L'PRTLINE        Get length of print line
         BAL   R7,PUTLINE          Write the line
         SPACE 1
*----------------------------------------------------------------*
*        Write "STEP END STATISTICS" header line.                *
*----------------------------------------------------------------*
         MVI   PRTLINE,C' '        Blank output area
         MVC   PRTLINE+1(L'PRTLINE-1),PRTLINE
         MVI   PRTLINE+0,C'*'      Set initial frame character
         MVI   PRTLINE+131,C'*'    Set final frame character
         MVC   SHDRTX,MSHDRTX      Set header text
         LA    R1,PRTLINE          Point to print line
         LA    R0,L'PRTLINE        Get length of print line
         BAL   R7,PUTLINE          Write the line
         EJECT ,
*----------------------------------------------------------------*
*        Initialize step end statistics line 1.                  *
*----------------------------------------------------------------*
         MVC   STEPLN1,MSTEPLN1    Copy model line
         SPACE 1
*----------------------------------------------------------------*
*        Step number.                                            *
*----------------------------------------------------------------*
         XR    R15,R15             Zero register for IC
         L     R1,(8-1)*4(,R10)    Point to indicator/step number
         IC    R15,1(,R1)          Get step number
         CVD   R15,DWORK           Convert step number to decimal
         OI    DWORK+7,X'0F'       Force printable sign
         UNPK  STEPNO,DWORK        Unpack into print line
         SPACE 1
*----------------------------------------------------------------*
*        Hierarchy 0 region size.                                *
*----------------------------------------------------------------*
         LH    R15,SMF4RSH0        Get region size
         CVD   R15,DWORK           Make value packed
         BAL   R14,EDMKRTN         Convert to zoned
         MVC   SH0REG,EH0REG       Set H0 region requested
         SPACE 1
*----------------------------------------------------------------*
*        Step start time.                                        *
*----------------------------------------------------------------*
         ICM   R1,15,SMF4SIT       Get step start time from record
         LA    R15,SINITTM         Point to area for output
         BAL   R7,TIMEX            Format step start time
         SPACE 1
*----------------------------------------------------------------*
*        Allocation start time.                                  *
*----------------------------------------------------------------*
         ICM   R1,15,SMF4AST       Get allocation start time
         LA    R15,S1ALCST         Point to area for output
         BAL   R7,TIMEX            Format allocation start time
         SPACE 1
*----------------------------------------------------------------*
*        Write step end statistics line 1.                       *
*----------------------------------------------------------------*
         LA    R1,PRTLINE          Point to print line
         LA    R0,L'PRTLINE        Get length of print line
         BAL   R7,PUTLINE          Write the line
         EJECT ,
*----------------------------------------------------------------*
*        Initialize step end statistics line 2.                  *
*----------------------------------------------------------------*
         MVC   STEPLN2,MSTEPLN2    Copy model line
         SPACE 1
*----------------------------------------------------------------*
*        Step name.                                              *
*----------------------------------------------------------------*
*        MVC   STEPNM,SWSTEPN      Set step name
         MVC   STEPNM,SCTSNAME     Set step name
         SPACE 1
*----------------------------------------------------------------*
*        Step end time.                                          *
*----------------------------------------------------------------*
         MVC   SENDTM,XENDTM       Set step end time
         SPACE 1
*----------------------------------------------------------------*
*        Problem program start time.                             *
*----------------------------------------------------------------*
         ICM   R1,15,SMF4PPST      Get program start time
         LA    R15,S2PGMST         Point to area for output
         BAL   R7,TIMEX            Format program start time
         SPACE 1
*----------------------------------------------------------------*
*        Hierarchy 0 storage used.                               *
*----------------------------------------------------------------*
         LH    R15,SMF4H0ST        Get user storage HWM
         CVD   R15,DWORK           Make value packed
         BAL   R14,EDMKRTN         Convert to zoned
         MVC   SH0STOR,EH0STOR     Set hierarchy 0 storage used
         SPACE 1
*----------------------------------------------------------------*
*        Completion status.                                      *
*----------------------------------------------------------------*
         MVC   STEPCCHX,XTEPCCH    Set completion status
         SPACE 1
*----------------------------------------------------------------*
*        Write step end statistics line 2.                       *
*----------------------------------------------------------------*
         LA    R1,PRTLINE          Point to print line
         LA    R0,L'PRTLINE        Get length of print line
         BAL   R7,PUTLINE          Write the line
         EJECT ,
*----------------------------------------------------------------*
*        Initialize step end statistics line 3.                  *
*----------------------------------------------------------------*
         MVC   STEPLN3,MSTEPLN3    Copy model line
         SPACE 1
*----------------------------------------------------------------*
*        Procstep name.                                          *
*----------------------------------------------------------------*
*        MVC   SPSTEPN,SWPSTEPN    Step name
         MVC   SPSTEPN,SCTSCLPC    Set procstep name
         SPACE 1
*----------------------------------------------------------------*
*        Step elapsed time.                                      *
*----------------------------------------------------------------*
         MVC   SELAPS,XELAPS       Step elapsed time
         SPACE 1
*----------------------------------------------------------------*
*        Step dispatching priority.                              *
*----------------------------------------------------------------*
         XR    R15,R15             Clear a register
         IC    R15,SMF4PRTY        Get dispatching priority
         CVD   R15,DWORK           Convert to packed
         BAL   R14,EDMKRTN         Convert to zoned
         MVC   S3DPRTY,EDPRTY      Set DPRTY in output line
         SPACE 1
*----------------------------------------------------------------*
*        Hierarchy 1 region size.                                *
*----------------------------------------------------------------*
         LH    R15,SMF4RSH1        Get H1 region size
         CVD   R15,DWORK           Make value packed
         BAL   R14,EDMKRTN         Convert to zoned
         MVC   S3H1REG,EH1REG      Set H1 region size
         SPACE 1
*----------------------------------------------------------------*
*        Write step end statistics line 3.                       *
*----------------------------------------------------------------*
         LA    R1,PRTLINE          Point to print line
         LA    R0,L'PRTLINE        Get length of print line
         BAL   R7,PUTLINE          Write the line
         EJECT ,
*----------------------------------------------------------------*
*        Initialize step end statistics line 4.                  *
*----------------------------------------------------------------*
         MVC   STEPLN4,MSTEPLN4    Copy model line
         SPACE 1
*----------------------------------------------------------------*
*        Program name.                                           *
*----------------------------------------------------------------*
         MVC   S4PGMNM,SMF4PGMN     Set program name from record
         SPACE 1
*----------------------------------------------------------------*
*        Step TCB time.                                          *
*----------------------------------------------------------------*
         L     R15,20(,R10)        Point to step TCB time
         ICM   R1,7,0(R15)         Get step TCB time
         LA    R15,S4CPUTM         Point to output field
         BAL   R7,TIMEX            Format step TCB time
         SPACE 1
*----------------------------------------------------------------*
*        Protect key.                                            *
*----------------------------------------------------------------*
         XR    R15,R15             Clear a register
         IC    R15,SMF4SPK         Get protect key
         SRL   R15,4                in low-order 4 bits
         CVD   R15,DWORK           Make value packed
         BAL   R14,EDMKRTN         Convert to zoned
         MVC   S4PKEY,EPKEY        Set protect key in output line
         SPACE 1
*----------------------------------------------------------------*
*        Hierarchy 1 storage used.                               *
*----------------------------------------------------------------*
         LH    R15,SMF4H1ST        Get H1 storage HWM
         CVD   R15,DWORK           Make value packed
         BAL   R14,EDMKRTN         Convert to zoned
         MVC   S4H1STO,EH1STOR     Set hierarchy 1 storage used
         SPACE 1
*----------------------------------------------------------------*
*        Write step end statistics line 4.                       *
*----------------------------------------------------------------*
         LA    R1,PRTLINE          Point to print line
         LA    R0,L'PRTLINE        Get length of print line
         BAL   R7,PUTLINE          Write the line
         EJECT ,
******************************************************************
*                                                                *
*        SBLDEXCP:                                               *
*                                                                *
*        Build and write EXCP counts from SMF record.            *
*                                                                *
*        This logic loops through the EXCP sections in the       *
*        SMF record, formatting them into the print line.        *
*        Sections representing DD DUMMY entries or subsystem     *
*        data sets are skipped, because they contain no EXCP     *
*        counts.  Each print line can contain up to five         *
*        EXCP count entries.  When the line is full, or when     *
*        all EXCP sections in the record have been processed,    *
*        the print line containing the formatted EXCP counts     *
*        will be written.  If a header line and column           *
*        headings for the EXCP box have not yet been             *
*        written, they will be written first.  If there is       *
*        only one line of EXCP counts and it contains fewer      *
*        than five entries, the column headings will be          *
*        adjusted accordingly.                                   *
*                                                                *
*        R3  = Address of current EXCP section in SMF record.    *
*        R4  = Count of EXCP sections.                           *
*        R9  = Address of SMF record.                            *
*        R10 = Exit routine parameter list - not referenced.     *
*        R12 = Address of LCT - not referenced.                  *
*                                                                *
******************************************************************
         SPACE 1
SBLDEXCP DS    0H                  Build and write EXCP counts
         XR    R4,R4               Get length
         ICM   R4,3,SMF4LENN        of EXCP portion of record
         SH    R4,=Y(SMF4DEVC-SMF4LENN)     and adjust for length field
         BNP   SEXEXIT             Done if no EXCP counts
         SRL   R4,3                Else get number of EXCP sections
         LA    R3,SMF4LENN+(SMF4DEVC-SMF4LENN)  and point to first one
         USING SMF4DEVC,R3         Addressability for EXCP section
         LA    R15,SMF30ELN        Bump EXCP section pointer back
         SR    R3,R15               for initial increment
         SPACE 1
******************************************************************
*                                                                *
*        SEXLOOPI:                                               *
*                                                                *
*        Loop through EXCP sections of record, invoking          *
*        formatting subroutine to format EXCP counts into        *
*        print line.  Whenever print line is full, allow it      *
*        to be printed, then return here to loop some more.      *
*                                                                *
*        R3  = Address of current EXCP section in SMF record.    *
*        R4  = Count of EXCP sections.                           *
*        R5  = Count of EXCP segments in print line.             *
*        R6  = Address of current EXCP segment in print line.    *
*        R7  = Subroutine linkage.                               *
*        R9  = Start of type 4 record - not referenced.          *
*        R10 = Exit routine parameter list.                      *
*        R12 = OS Linkage Control Table - not referenced.        *
*                                                                *
******************************************************************
         SPACE 1
*----------------------------------------------------------------*
*        Prepare to loop through EXCP sections.                  *
*----------------------------------------------------------------*
SEXLOOPI DS    0H                  Set up for EXCP section loop
         MVI   PRTLINE,C' '        Blank output area
         MVC   PRTLINE+1(L'PRTLINE-1),PRTLINE
         MVI   PRTLINE+0,C'*'      Set initial frame character
         MVI   PRTLINE+131,C'*'    Set final frame character
         LA    R5,SXCOLNUM         Set maximum count on this line
         LA    R6,PRTLINE+4        Point to slot for first count
         SPACE 1
*----------------------------------------------------------------*
*        Loop through EXCP sections in SMF record.               *
*----------------------------------------------------------------*
SEXLOOP  DS    0H                  Fill one print line
         LA    R15,SMF30ELN        Get EXCP section length
SMF30ELN EQU   8
         AR    R3,R15              Adjust to next EXCP section
         LTR   R4,R4               Any more EXCP sections?
         BNP   SEXTRUNC            Branch if no more in record
         BCTR  R4,0                Decrement EXCP section count
         CLC   =XL4'00',SMF4DEVC   Dummy or jes data set?
         BE    SEXLOOP             Skip it if so
         BAL   R7,EXCP             Else format into print line
         LA    R6,L'SXSEG(,R6)     Point to next slot in line
         BCT   R5,SEXLOOP          Look at next EXCP section
         B     SEXFULL             Line is full, go print it
         EJECT ,
******************************************************************
*                                                                *
*        SEXFULL:                                                *
*                                                                *
*        EXCP print line is full.  Test to see if EXCP box       *
*        header lines have been written.                         *
*                                                                *
*        R3  = Address of current EXCP section - not             *
*              referenced.                                       *
*        R4  = Count of EXCP sections - not referenced.          *
*        R5  = Count of EXCP segments in print line - not        *
*              referenced.                                       *
*        R6  = Current EXCP segment in print line - not          *
*              referenced.                                       *
*        R7  = Subroutine linkage - not used.                    *
*        R9  = Start of type 4 record - not referenced.          *
*        R10 = Exit routine parameter list.                      *
*        R12 = OS Linkage Control Table - not referenced.        *
*                                                                *
******************************************************************
         SPACE 1
*----------------------------------------------------------------*
*        Print line is full.                                     *
*----------------------------------------------------------------*
SEXFULL  DS    0H                  EXCP print line is full
         TM    FLAG1,F1EXCPHD      Have we written headers yet?
         BO    SEXWLINE            Yes, go write line
         EJECT ,
******************************************************************
*                                                                *
*        SEXWHDR:                                                *
*                                                                *
*        Write EXCP box heading and column heading lines.        *
*        If fewer than four EXCP counts are going to be          *
*        written, blank out the column headings over columns     *
*        which will be unused.                                   *
*                                                                *
*        R3  = Address of current EXCP section in SMF record.    *
*        R4  = Count of EXCP sections.                           *
*        R5  = Count of EXCP segments in print line.             *
*        R6  = Address of current EXCP segment in print line.    *
*        R7  = Subroutine linkage.                               *
*        R9  = Start of type 4 record - not referenced.          *
*        R10 = Exit routine parameter list.                      *
*        R12 = OS Linkage Control Table.                         *
*                                                                *
******************************************************************
         SPACE 1
*----------------------------------------------------------------*
*        Set up to write EXCP header and column headings.        *
*----------------------------------------------------------------*
SEXWHDR  DS    0H                  Prepare to write EXCP headings
         SPACE 1
*----------------------------------------------------------------*
*        Write "...." line prior to EXCP box.                    *
*----------------------------------------------------------------*
         MVI   PRTLINE2,C'.'       Set "...." in output area
         MVC   PRTLINE2+1(L'PRTLINE2-1),PRTLINE2
         MVI   PRTLINE2+0,C'*'     Set initial frame character
         MVI   PRTLINE2+131,C'*'   Set final frame character
         LA    R0,L'PRTLINE2       Set length of line
         LA    R1,PRTLINE2         Point to "...." line
         BAL   R7,PUTLINE          Write "...." line
         SPACE 1
*----------------------------------------------------------------*
*        Write "EXCP STATISTICS" header line.                    *
*----------------------------------------------------------------*
         MVI   PRTLINE2,C' '       Blank output area
         MVC   PRTLINE2+1(L'PRTLINE2-1),PRTLINE2
         MVI   PRTLINE2+0,C'*'     Set initial frame character
         MVI   PRTLINE2+131,C'*'   Set final frame character
         MVC   SHDREX,MSHDREX      Set header text
         LA    R1,PRTLINE2         Point to print line
         LA    R0,L'PRTLINE2       Get length of print line
         BAL   R7,PUTLINE          Write the line
         SPACE 1
*----------------------------------------------------------------*
*        Initialize EXCP column header and determine             *
*        if we need to modify it.                                *
*----------------------------------------------------------------*
         MVC   PRTLINE2,STEPXCOL   Initialize column headings
         LTR   R5,R5               All columns used?
         BNP   SEXWHDRW            Continue if so
         SPACE 1
*----------------------------------------------------------------*
*        Find number of EXCP entries on the line.                *
*----------------------------------------------------------------*
         LA    R14,SXCOLNUM        Calculate number
         SR    R14,R5               of EXCP entries in line
         SPACE 1
*----------------------------------------------------------------*
*        Find start of header area to be blanked.                *
*----------------------------------------------------------------*
         LA    R15,SXCOLFST        Point to first column heading
         MH    R14,=Y(SXCOLLEN)    Calculate address
         AR    R15,R14              of area to blank
         SPACE 1
*----------------------------------------------------------------*
*        Calculate length of header area to blank.               *
*----------------------------------------------------------------*
         LR    R1,R5               Calculate length
         MH    R1,=Y(SXCOLLEN)      of area to be blanked
         SH    R1,=Y(2)            Adjust length
         SPACE 1
*----------------------------------------------------------------*
*        Blank column headings over empty columns.               *
*----------------------------------------------------------------*
         MVI   0(R15),C' '         Blank heading
         EX    R1,SEXBLMVC          over columns not used
         B     SEXWHDRW            Go to write EXCP headers
         SPACE 1
SEXBLMVC MVC   1(*-*,R15),0(R15)   ** Executed to blank headers **
         SPACE 1
*----------------------------------------------------------------*
*        Write EXCP column header line.                          *
*----------------------------------------------------------------*
SEXWHDRW DS    0H                  Write the headers
         LA    R0,L'PRTLINE2       Set length of line
         LA    R1,PRTLINE2         Point to column headings
         BAL   R7,PUTLINE          Write column headings
         OI    FLAG1,F1EXCPHD      Show EXCP headers were done
         EJECT
******************************************************************
*                                                                *
*        SEXWLINE:                                               *
*                                                                *
*        Headings are finished or had already been written,      *
*        so write the completed EXCP count print line.           *
*                                                                *
*        R3  = Address of current EXCP section in SMF record.    *
*        R4  = Count of EXCP sections.                           *
*        R5  = Count of EXCP segments in print line.             *
*        R6  = Address of current EXCP segment in print line.    *
*        R7  = Subroutine linkage.                               *
*        R9  = Address of SMF record - not referenced.           *
*        R10 = Exit routine parameter list - not referenced.     *
*        R12 = Address of LCT - not referenced.                  *
*                                                                *
******************************************************************
         SPACE 1
*----------------------------------------------------------------*
*        Write EXCP print line.                                  *
*----------------------------------------------------------------*
SEXWLINE DS    0H                  Write EXCP print line
         LA    R0,132              Set length of line
         LA    R1,PRTLINE          Point to print line
         BAL   R7,PUTLINE          Write EXCP counts print line
         LTR   R4,R4               Was this the last line?
         BP    SEXLOOPI            No: go do some more EXCPs
         B     SEXEXIT             Else done with EXCPs
         EJECT ,
******************************************************************
*                                                                *
*        SEXTRUNC:                                               *
*                                                                *
*        Last EXCP section in this record has been processed.    *
*        Determine if the final print line has anything in it,   *
*        and, if it does, go to write it.                        *
*                                                                *
*        R3  = Address of current EXCP section in SMF record.    *
*        R4  = Count of EXCP sections.                           *
*        R5  = Count of EXCP segments in print line.             *
*        R6  = Address of current EXCP segment in print line.    *
*        R7  = Subroutine linkage.                               *
*        R9  = Address of SMF record - not referenced.           *
*        R10 = Exit routine parameter list - not referenced.     *
*        R12 = Address of LCT - not referenced.                  *
*                                                                *
******************************************************************
         SPACE 1
*----------------------------------------------------------------*
*        Truncate print line when no more EXCP sections.         *
*----------------------------------------------------------------*
SEXTRUNC DS    0H                  Out of EXCPs
         C     R5,=A(SXCOLNUM)     Anything on final print line?
         BL    SEXFULL             Yes: then go print it
         B     SEXEXIT             Else done
         EJECT ,
******************************************************************
*                                                                *
*        SEXEXIT:                                                *
*                                                                *
*        Done with EXCP processing for this SMF record.          *
*                                                                *
*        R9  = Address of SMF record - not referenced.           *
*        R10 = Exit routine parameter list - not referenced.     *
*        R12 = Address of LCT - not referenced.                  *
*                                                                *
******************************************************************
         SPACE 1
SEXEXIT  DS    0H                  End of EXCP processing
         DROP  R3                  End EXCP section addressability
         EJECT ,
*----------------------------------------------------------------*
*        Write a line of all "****".                             *
*----------------------------------------------------------------*
         MVI   PRTLINE+0,C'*'      Initialize line to "****"
         MVC   PRTLINE+1(L'PRTLINE-1),PRTLINE
         LA    R1,PRTLINE          Point to print line
         LA    R0,L'PRTLINE        Get length of print line
         BAL   R7,PUTLINE          Write the line
         SPACE 1
*----------------------------------------------------------------*
*        Write a blank line.                                     *
*----------------------------------------------------------------*
         MVI   PRTLINE,C' '        Set blank in print line
         LA    R1,PRTLINE          Point to print line
         LA    R0,1                Get length of print line
         BAL   R7,PUTLINE          Write the line
         SPACE 1
*----------------------------------------------------------------*
*        Done with step end processing.                          *
*----------------------------------------------------------------*
         B     FREEWORK            Go to clean up and exit
         EJECT ,
******************************************************************
*                                                                *
*        JOBEND:                                                 *
*                                                                *
*        End-of-job processing.                                  *
*                                                                *
******************************************************************
         SPACE 1
JOBEND   DS    0H                  Begin end-of-job processing
**** start hasp stuff
         L     R15,CVTPTR          Get address of CVT
         USING CVT,R15             Addressability for CVT
         L     R15,CVTUSER         Get HASP $HVT address
         DROP  R15                 End CVT addressability
         LTR   R15,R15             Is HASP active?
         BZ    JBNOHASP            Skip call to HASP if not
         L     R15,28(,R15)        Else get address of XTERMSMF
         L     R1,PREVR13          Restore caller's R13
         LM    R0,R1,20(R1)        Restore R0-R1 at entry
         BALR  R14,R15             Link to XTERMSMF
JBNOHASP DS    0H                  Here to bypass call to HASP
**** end hasp stuff
         SPACE 1
*----------------------------------------------------------------*
*        Write a blank line.                                     *
*----------------------------------------------------------------*
         MVI   PRTLINE,C' '        Set blank in print line
         LA    R1,PRTLINE          Point to print line
         LA    R0,1                Get length of print line
         BAL   R7,PUTLINE          Write the line
         SPACE 1
*----------------------------------------------------------------*
*        Write "OS/360 ..." line.                                *
*----------------------------------------------------------------*
         MVI   PRTLINE+0,C'*'      Initialize line to "****"
         MVC   PRTLINE+1(L'PRTLINE-1),PRTLINE
         MVC   CHDATA,HDATA        Copy heading data
         LA    R1,PRTLINE          Point to print line
         LA    R0,L'PRTLINE        Get length of print line
         BAL   R7,PUTLINE          Write the line
         SPACE 1
*----------------------------------------------------------------*
*        Write "JOB END STATISTICS" header line.                 *
*----------------------------------------------------------------*
         MVI   PRTLINE,C' '        Blank output area
         MVC   PRTLINE+1(L'PRTLINE-1),PRTLINE
         MVI   PRTLINE+0,C'*'      Set initial frame character
         MVI   PRTLINE+131,C'*'    Set final frame character
         MVC   JHDRTX,MJHDRTX      Set header text
         LA    R1,PRTLINE          Point to print line
         LA    R0,L'PRTLINE        Get length of print line
         BAL   R7,PUTLINE          Write the line
         EJECT ,
*----------------------------------------------------------------*
*        Initialize job end statistics line 1.                   *
*----------------------------------------------------------------*
         MVC   JOBLN1,MJOBLN1      Copy model line
         SPACE 1
*----------------------------------------------------------------*
*        Job name.                                               *
*----------------------------------------------------------------*
         MVC   J1JNAME,JCTJNAME     Get job name
         SPACE 1
*----------------------------------------------------------------*
*        Reader start time.                                      *
*----------------------------------------------------------------*
         ICM   R1,15,SMF5RST       Get reader start time
         LA    R15,J1RDTM          Locate print position
         BAL   R7,TIMEX            Go convert time to HH:MM:SS:TH
         SPACE 1
*----------------------------------------------------------------*
*        Reader start date.                                      *
*----------------------------------------------------------------*
         ICM   R1,15,SMF5RSD       Get reader start date
         LA    R15,RTDOW           Find day-of-week
         BALR  R14,R15              of reader start date
         MVC   J1RDDAY,0(R15)        and set in output line
         LA    R0,J1RDDTJ          Format reader date
         LA    R15,RTJDATE          as Julian date
         BALR  R14,R15               into output line
         LA    R0,J1RDDTG          Convert reader date
         LA    R15,RTTOGREG         to Gregorian date
         BALR  R14,R15               and format into output line
         SPACE 1
*----------------------------------------------------------------*
*        Job CPU time under TCBs.                                *
*----------------------------------------------------------------*
         XR    R1,R1               Clear time register
         ICM   R1,7,SMF5JCPU       Get job TCB time
         LA    R15,J1TMTCB         Point to slot for output
         BAL   R7,TIMEX            Go convert time to HH:MM:SS:TH
         SPACE 1
*----------------------------------------------------------------*
*        Write job end statistics line 1.                        *
*----------------------------------------------------------------*
         LA    R1,PRTLINE          Point to print line
         LA    R0,L'PRTLINE        Get length of print line
         BAL   R7,PUTLINE          Write the line
         EJECT ,
*----------------------------------------------------------------*
*        Initialize job end statistics line 2.                   *
*----------------------------------------------------------------*
         MVC   JOBLN2,MJOBLN2      Copy model line
         SPACE 1
*----------------------------------------------------------------*
*        Job class.                                              *
*----------------------------------------------------------------*
         MVC   J2CLS,SMF5JICL      Get job class
         SPACE 1
*----------------------------------------------------------------*
*        Job initiation time.                                    *
*----------------------------------------------------------------*
         ICM   R1,15,SMF5JIT       Get time job was initiated
         LA    R15,J2INTM          Locate its print position
         BAL   R7,TIMEX            Go convert time to HH:MM:SS:TH
         SPACE 1
*----------------------------------------------------------------*
*        Job initiation date.                                    *
*----------------------------------------------------------------*
         ICM   R1,15,SMF5JID       Get job initiation date
         LA    R15,RTDOW           Find day-of-week
         BALR  R14,R15              of start date
         MVC   J2INDAY,0(R15)        and set in output line
         LA    R0,J2INDTJ          Format start date
         LA    R15,RTJDATE          as Julian date
         BALR  R14,R15               into output line
         LA    R0,J2INDTG          Convert start date
         LA    R15,RTTOGREG         to Gregorian date
         BALR  R14,R15               and format into output line
         SPACE 1
*----------------------------------------------------------------*
*        Job elapsed time                                        *
*----------------------------------------------------------------*
         MVC   DATE,SMF5JID        Get job start date
         MVC   DATEND,SMF5DTE      Get job end date
         ICM   R1,15,SMF5TME       Get job end time
         ICM   R0,15,SMF5JIT       Get time job was initiated
         LA    R15,J2TMELP         Point to output area
         BAL   R7,ELAPSED          Format job elapsed time
         SPACE 1
*----------------------------------------------------------------*
*        Write job end statistics line 2.                        *
*----------------------------------------------------------------*
         LA    R1,PRTLINE          Point to print line
         LA    R0,L'PRTLINE        Get length of print line
         BAL   R7,PUTLINE          Write the line
         EJECT ,
*----------------------------------------------------------------*
*        Initialize job end statistics line 3.                   *
*----------------------------------------------------------------*
         MVC   JOBLN3,MJOBLN3      Copy model line
         SPACE 1
*----------------------------------------------------------------*
*        Job end time.                                           *
*----------------------------------------------------------------*
         ICM   R0,15,SMF5TME       Get job end time
         ICM   R1,15,SMF5DTE       Get job end date
         STCM  R1,15,DATEND        Save job end date
         LR    R4,R0               Save job end time
         LR    R1,R4               Save time for convert routine
         LA    R15,J3ENTM          Point to output area
         BAL   R7,TIMEX            Go convert time to HH:MM:SS:TH
         SPACE 1
*----------------------------------------------------------------*
*        Job end date.                                           *
*----------------------------------------------------------------*
         ICM   R1,15,DATEND        Get job end date
         LA    R15,RTDOW           Find day-of-week
         BALR  R14,R15              of end date
         MVC   J3ENDAY,0(R15)        and set in output line
         LA    R0,J3ENDTJ          Format end date
         LA    R15,RTJDATE          as Julian date
         BALR  R14,R15               into output line
         LA    R0,J3ENDTG          Convert end date
         LA    R15,RTTOGREG         to Gregorian date
         BALR  R14,R15               and format into output line
         SPACE 1
*----------------------------------------------------------------*
*        See if JMRUSEID contains HASP/ASP job ID.               *
*----------------------------------------------------------------*
         L     R15,PREVR13         Restore caller's R13
         L     R15,24(,R15)        Get R1 at entry
         L     R15,0(,R15)         Get CEPA address
         USING JMR,R15             Addressability for CEPA
         CLC   =C'JOB',JMRUSEID    Does JMRUSEID contain job ID?
         BNE   JACCT               Do account number if not
         SPACE 1
*----------------------------------------------------------------*
*        Format HASP/ASP job ID from JMRUSEID.                   *
*----------------------------------------------------------------*
         MVC   J3KACT,=C'JOB ID '  Set "JOB ID" keyword in output
         MVC   J3JBID,JMRUSEID     Set job ID from JMRUSEID
         MVI   J3JBID+3,C'0'       Force leading zero
         OC    J3JBID,=C'   00000' Force blanks to zeroes
         DROP  R15                 End CEPA addressability
         B     JMISC               Continue with job fields
         SPACE 1
*----------------------------------------------------------------*
*        First accounting field from JOB card.                   *
*----------------------------------------------------------------*
JACCT    DS    0H
         MVC   J3ACCT,=CL8' '      Blank account number field
         XR    R15,R15             Zero number of account fields
         L     R1,12(,R10)         Point to num of acct fields
         ICM   R15,1,3(R1)         Get number of acct fields
         BNP   JMISC               Skip this if no fields
         L     R1,16(,R10)         Point to accounting fields
         ICM   R15,1,0(R1)         Get length of 1st field
         BNP   JMISC               Quit if no first field
         C     R15,=F'8'           1st field too big for us?
         BH    JMISC               Skip accounting field if so
         BCTR  R15,0               Decrement for executed move
         EX    R15,MOVEACCT        Move in account number
JMISC    DS    0H
         SPACE 1
*----------------------------------------------------------------*
*        Job priority.                                           *
*----------------------------------------------------------------*
         XR    R15,R15             Clear a register
         IC    R15,SMF5JPTY        Get job priority
         CVD   R15,DWORK           Convert to packed
         BAL   R14,EDMKRTN         Format as zoned
         MVC   J3PRTY,EPRTY        Set priority in output line
         SPACE 1
*----------------------------------------------------------------*
*        Write job end statistics line 3.                        *
*----------------------------------------------------------------*
         LA    R1,PRTLINE          Point to print line
         LA    R0,L'PRTLINE        Get length of print line
         BAL   R7,PUTLINE          Write the line
         EJECT ,
*----------------------------------------------------------------*
*        Write a line of all "****".                             *
*----------------------------------------------------------------*
         MVI   PRTLINE+0,C'*'      Initialize line to "****"
         MVC   PRTLINE+1(L'PRTLINE-1),PRTLINE
         LA    R1,PRTLINE          Point to print line
         LA    R0,L'PRTLINE        Get length of print line
         BAL   R7,PUTLINE          Write the line
         SPACE 1
*----------------------------------------------------------------*
*        Write a blank line.                                     *
*----------------------------------------------------------------*
         MVI   PRTLINE,C' '        Set blank in print line
         LA    R1,PRTLINE          Point to print line
         LA    R0,1                Get length of print line
         BAL   R7,PUTLINE          Write the line
         SPACE 1
*----------------------------------------------------------------*
*        Done with job end processing.                           *
*----------------------------------------------------------------*
         B     FREEWORK            Go to clean up and exit
         SPACE 1
MOVEACCT MVC   J3ACCT(*-*),1(R1)   ** Executed **
         SPACE 1
         DROP  R6                  END SCT ADDRESSABILITY
         DROP  R5                  END JCT ADDRESSABILITY
         DROP  R12                 END LCT ADDRESSABILITY
         EJECT ,
******************************************************************
*                                                                *
*        FREEWORK:                                               *
*                                                                *
*        Free dynamic work area.                                 *
*                                                                *
*        R9  = Address of SMF record - not referenced.           *
*        R10 = Exit routine parameter list - not referenced.     *
*        R12 = Address of LCT.                                   *
*        R13 = GETMAINed work area - freed in this routine.      *
*                                                                *
*        Workregs = R0, R1, R15.                                 *
*                                                                *
******************************************************************
         SPACE 1
FREEWORK DS    0H                  Free the work area
         LR    R1,R13              Set work area address for freemain
         LA    R0,LWORKA           Set work area length for freemain
         LA    R15,243             Set                             @D01
         SLL   R15,24               subpool                        @D01
         OR    R0,R15                for freemain                  @D01
         L     R13,PREVR13         Restore caller's save area address
**       L     R13,SAVEA+4         Restore caller's save area address
         DROP  R13                 End work area addressability
         FREEMAIN R,LV=(0),A=(1)   Free work area storage
         EJECT ,
*----------------------------------------------------------------*
*        Restore registers and return to system.                 *
*----------------------------------------------------------------*
RETURN   DS    0H                  Common return logic
         L     R14,12(,R13)        Restore return address
         XR    R1,R1               Allow SMF record to be written
         RETURN (2,12),RC=0        Return to caller
         EJECT ,
******************************************************************
*                                                                *
*        EXCP:                                                   *
*                                                                *
*        Subroutine to convert and place device number and       *
*        EXCP counts in print line.                              *
*                                                                *
*        R3  = Address of current EXCP section in SMF record.    *
*        R6  = Address of current EXCP segment in print line.    *
*        R7  = Subroutine return address.                        *
*        R10 = Exit routine parameter list.                      *
*                                                                *
******************************************************************
         SPACE 1
         USING WORKA,R13           Restore work area addressability
EXCP     DS    0H                  Format EXCP entry
         USING SX,R6               Addressability for EXCP line
         USING SMF4DEVC,R3         Address EXCP section of record
         MVC   DWORK(2),SMF4CUAD   Get device number
         MVI   DWORK+2,X'0F'       Set sign for unpack
         UNPK  DWORK+3(5),DWORK(3) Unpack device number
         MVC   SUNIT,DWORK+3       Move to print line
         TR    SUNIT,HEXTRAN-240   Translate hex to character
         ICM   R15,15,SMF4EXCP     Load EXCP count
         CVD   R15,DWORK           Convert EXCPs to decimal
         MVC   SCOUNT,MSKEXCP      Move edit pattern to line
         LA    R1,SCOUNT+L'SCOUNT-1  Default significance pointer
         EDMK  SCOUNT,DWORK+2      Edit EXCPs into line
         BCTR  R1,0                Decrement significance pointer
         MVI   0(R1),C' '          Set blank before first digit
         BR    R7                  Return to caller
         SPACE 1
MSKEXCP  DC    3C'-',9X'20',X'2120'  EXCP edit mask
         SPACE 1
         DROP  R3                  End EXCP record addressability
         DROP  R6                  End EXCP output addressability
         DROP  R13                 End work area addressability
         TITLE '    Julian date formatting subroutine'
***********************************************************************
*                                                                     *
*                                                                     *
* Subroutine           =  RTJDATE                                     *
*                                                                     *
*   Purpose            =  To format an MVS Julian date (year and      *
*                         day of year) as "yyyy.ddd" into a           *
*                         caller-supplied buffer.                     *
*                                                                     *
*   Linkage            =  Via BALR R14,R15                            *
*                                                                     *
*   Comments           =  None.                                       *
*                                                                     *
*   Input data         =                                              *
*       R0     = Address of 8-byte area to contain formatted date     *
*       R1     = Date in MVS TIME DEC format 0cyydddF                 *
*       R2-10  = Not applicable                                       *
*       R11    = Main program base register                           *
*       R12    = Not applicable                                       *
*       R13    = Standard 18-word OS save area                        *
*       R14    = Return address                                       *
*       R15    = Entry address                                        *
*                                                                     *
*   Registers saved    =  R0 - R15                                    *
*                                                                     *
*   Register usage     =                                              *
*       R0-2   = Not modified                                         *
*       R3     = Address of output area (from R0 at entry)            *
*       R4-13  = Not modified                                         *
*       R14    = Work register                                        *
*       R15    = Not modified                                         *
*                                                                     *
*   Registers restored =  R0 - R15                                    *
*                                                                     *
*   Output data        =                                              *
*       8-byte area pointed to by R0 at entry contains                *
*       Julian date formatted as "yyyy.ddd".                          *
*                                                                     *
*   Exit (normal)      =  Return to caller.                           *
*     Output           =  Formatted Julian date.                      *
*     Return code      =  Not applicable                              *
*                                                                     *
*                                                                     *
***********************************************************************
         EJECT ,
***********************************************************************
*                                                                     *
*        RTJDATE: Subroutine to format Julian date.                   *
*                                                                     *
***********************************************************************
         SPACE 1
RTJDATE  DS    0H                  Format Julian date
         USING WORKA,R13           Addressability for work area
         STM   R14,R12,12(R13)     Save caller's registers
         ST    R1,TIME             Set date 0cyydddF in work area
         LR    R3,R0               Save pointer to output area
         USING DJULDT,R3           Addressability for output area
         MVC   DJEDIT,EMJDATE      Initialize edit mask for date
         ED    DJEDIT,TIME+1       Edit date into output
         MVC   DJCC,=C'  '         Clear century digit positions
         IC    R14,TIME            Get the century index
         N     R14,=F'7'           Ensure a number in range 0-7
         SLL   R14,1               Get index
         LA    R14,ERCENTBL(R14)    into century table
         MVC   DJCC,0(R14)         Set century in date
         LM    R14,R12,12(R13)     Restore caller's registers
         BR    R14                 Return to caller
         DROP  R3                  End output area addressability
         DROP  R13                 End work area addressability
         SPACE 1
EMJDATE  DC    X'F021204B202020'   Edit mask for Julian date
         SPACE 1
*---------------------------------------------------------------------*
*        Table for building century from century index.               *
*---------------------------------------------------------------------*
ERCENTBL DS    0CL2                Century table
         DC    CL2'19'             +00 - century=00 - 20th century
         DC    CL2'20'             +02 - century=01 - 21st century
         DC    CL2'21'             +04 - century=02 - 22st century
         DC    CL2'22'             +06 - century=03 - 23rd century
         DC    CL2'23'             +08 - century=04 - 24th century
         DC    CL2'24'             +0a - century=05 - 25th century
         DC    CL2'25'             +0c - century=06 - 26th century
         DC    CL2'26'             +10 - century=07 - 27th century
         EJECT ,
*---------------------------------------------------------------------*
*        Mapping of area to contain formatted Julian date.            *
*---------------------------------------------------------------------*
DJULDT   DSECT ,                   Map formatted Julian date
DJYEAR   DS    0CL4                -- Four-digit year
DJCC     DS    CL2                 ---- Century
DJYY     DS    CL2                 ---- Year
         DS    C'.'                -- Separator "."
DJDAY    DS    CL3                 -- Day of year
         SPACE 1
         ORG   DJYEAR+1            Redefine year field
DJEDIT   DS    CL7                 Alias for edit
         ORG   ,                   Reset location counter
         SPACE 1
IEFACTRT CSECT ,                   Resume main CSECT
         TITLE '    Julian-to-Gregorian date conversion subroutine'
***********************************************************************
*                                                                     *
*                                                                     *
* Subroutine           =  RTTOGREG                                    *
*                                                                     *
*   Purpose            =  To convert MVS Julian date (year and day    *
*                         of year) in TIME DEC format to Gregorian    *
*                         month and day, and format the result into   *
*                         a caller-supplied buffer.                   *
*                                                                     *
*   Linkage            =  Via BALR R14,R15                            *
*                                                                     *
*   Comments           =                                              *
*                                                                     *
*       The algorithm used in this routine was adapted from           *
*       an algorithm called "Tableless Date Conversion" in            *
*       Communications of the ACM, Volume 13, Number 10,              *
*       October 1970.  The basic algorithm is:                        *
*                                                                     *
*         if (year is a leap year)                                    *
*           t = 1;                                                    *
*         else                                                        *
*           t = 0;                                                    *
*         endif                                                       *
*         if (dayofyear > (59 + t))                                   *
*           dayofmonth = dayofyear + 2 - t;                           *
*         else                                                        *
*           dayofmonth = dayofyear;                                   *
*         endif                                                       *
*         month = int(((dayofmonth + 91) * 100) / 3055);              *
*         dayofmonth = (dayofmonth + 91) - int((month * 3055) / 100); *
*         month = month - 2;                                          *
*         return(month, dayofmonth);                                  *
*                                                                     *
*       Note that the current implementation of this algorithm        *
*       doesn't include logic to handle centesimal years that         *
*       aren't leap years.  While it will work for years between      *
*       1901 and 2099 inclusive, it will break in 2100.               *
*                                                                     *
*                                                                     *
***********************************************************************
         EJECT ,
***********************************************************************
*                                                                     *
*                                                                     *
*   Input data         =                                              *
*       R0     = Address of 10-byte area for formated Gregorian date  *
*       R1     = Date in MVS TIME DEC format 0cyydddF                 *
*       R2-10  = Not applicable                                       *
*       R11    = Main program base register                           *
*       R12    = Not applicable                                       *
*       R13    = Standard 18-word OS save area                        *
*       R14    = Return address                                       *
*       R15    = Entry address                                        *
*                                                                     *
*   Registers saved    =  R0 - R15                                    *
*                                                                     *
*   Register usage     =                                              *
*       R0     = Work register                                        *
*       R1     = Work register                                        *
*       R2     = Work register                                        *
*       R3     = Pointer to area for formatted Gregorian date         *
*       R4-14  = Not modified                                         *
*       R15    = Work register                                        *
*                                                                     *
*   Registers restored =  R0 - R15                                    *
*                                                                     *
*   Output data        =                                              *
*       The 10-byte area pointed to by R0 at entry contains           *
*       the Gregorian date formatted as "yyyy/mm/dd".                 *
*                                                                     *
*   Exit (normal)      =  Return to caller via PR.                    *
*     Output           =  Formatted Gregorian date.                   *
*     Return code      =  Not applicable                              *
*                                                                     *
*                                                                     *
***********************************************************************
         EJECT ,
***********************************************************************
*                                                                     *
*        RTTOGREG:                                                    *
*                                                                     *
*        Subroutine to find Gregorian month and day from              *
*        Julian date and format the result.                           *
*                                                                     *
***********************************************************************
         SPACE 1
RTTOGREG DS    0H                  Find Gregorian month and day
         USING WORKA,R13           Addressability for work area
         STM   R14,R12,12(R13)     Save caller's registers
         LR    R3,R0               Save pointer to output area
         USING DGREGDT,R3          Addressability for output area
         LR    R2,R1               Save date 0cyydddF in R2
         SLL   R2,16               Packed days
         SRL   R2,16                0000dddF in R2
         XR    R0,R0               Isolate
         SLDL  R0,8                 century
         SRL   R1,8+16               in R0
         AH    R0,=H'19'              and adjust
         MH    R0,=H'100'              for 20th century base
         SLL   R1,4                Packed year 00000yy0 in R1
         O     R1,=F'15'           Packed year 00000yyF in R1
         XC    DWORK,DWORK         Clear CVB work area
         STCM  R1,15,DWORK+4       Set decimal year in work area
         CVB   R1,DWORK            Convert year to binary
         AR    R1,R0               Add century to year
         CVD   R1,DWORK            Convert 4-digit year to packed
         UNPK  DGYEAR,DWORK        Unpack year into output
         OI    DGYEAR+3,X'F0'       and force printable sign
         XC    DWORK,DWORK         Clear CVB work area
         STCM  R2,15,DWORK+4       Set decimal day in work area
         CVB   R0,DWORK            Convert day to binary
         LR    R2,R0               d = j = day of year in R2
         XR    R15,R15             Assume it's not a leap year
         N     R1,=F'3'            Make simple leap year test
         BNZ   RTTOGRA             t=0 if not a leap year
         LA    R15,1                otherwise t=1
RTTOGRA  LA    R1,59(,R15)         (59+t) in R1
         CR    R2,R1               Is j>(59+t)?
         BNH   RTTOGRB             Branch if not
         A     R2,=F'2'            Otherwise
         SR    R2,R15               d=j+2-t in R2
RTTOGRB  A     R2,=F'91'           d+91 in R2
         LR    R1,R2                and in R1
         M     R0,=F'100'          (d+91)*100 in R0-R1
         D     R0,=F'3055'         m=int(((d+91)*100)/3055)
         LR    R15,R1               in R1 and R15
         M     R14,=F'3055'        int((m*3055)/100)
         D     R14,=F'100'          in R15
         SR    R2,R15              d=(d+91)-int((m*3055)/100)          +
                                    in R2
         BCTR  R1,0                Adjust month
         BCTR  R1,0                 to m=m-2
         CVD   R1,DWORK            Convert month to packed
         UNPK  DGMONTH,DWORK       Unpack month into output
         OI    DGMONTH+1,X'F0'      and force printable sign
         CVD   R2,DWORK            Convert day to packed
         UNPK  DGDAY,DWORK         Unpack day into output
         OI    DGDAY+1,X'F0'        and force printable sign
         MVI   DGSEP1,C'/'         Insert separator
         MVI   DGSEP2,C'/'         Insert separator
         LM    R14,R12,12(R13)     Restore caller's registers
         BR    R14                 Return to caller
         SPACE 1
         DROP  R3                  End output area addressability
         DROP  R13                 End work area addressability
         SPACE 1
*---------------------------------------------------------------------*
*        Mapping of area to contain formatted Gregorian date.         *
*---------------------------------------------------------------------*
DGREGDT  DSECT ,                   Map formatted Gregorian date
DGYEAR   DS    CL4                 -- Four-digit year
DGSEP1   DS    C'/'                -- Separator "/"
DGMONTH  DS    CL2                 -- Month
DGSEP2   DS    C'/'                -- Separator "/"
DGDAY    DS    CL2                 -- Day of month
         SPACE 1
IEFACTRT CSECT ,                   Resume main CSECT
         TITLE '    Weekday calculation subroutine'
***********************************************************************
*                                                                     *
*                                                                     *
* Subroutine           =  RTDOW                                       *
*                                                                     *
*   Purpose            =  To find the day of the week given an        *
*                         MVS Julian date (year and day of year)      *
*                         in TIME DEC format.                         *
*                                                                     *
*   Linkage            =  Via BALR R14,R15                            *
*                                                                     *
*   Comments           =                                              *
*                                                                     *
*       The algorithm used in this routine was adapted from           *
*       "Elementary Number Theory and Its Applications" by            *
*       Kenneth H. Rosen, Second Edition, pp 152-157:                 *
*                                                                     *
*         if (year is a leap year)                                    *
*           f = j - 61 + 140;                                         *
*         else                                                        *
*           f = j - 60 + 140;                                         *
*         endif;                                                      *
*         weekday = (3 + f - (2*c) + y + int(c/4) + int(y/4)) mod 7;  *
*         return(weekday);                                            *
*                                                                     *
*       where                                                         *
*                                                                     *
*       o  J is the day of the year                                   *
*                                                                     *
*       o  C is the high-order two digits of the year                 *
*                                                                     *
*       o  Y is the low-order two digits of the year                  *
*                                                                     *
*       o  F is the offset from March 1 to the day of the year J,     *
*          adjusted by 140 to prevent the operand of the modulus      *
*          operation from becoming negative (140 is an even           *
*          multiple of the modulus that's large enough                *
*          to offset the largest anticipated negative value)          *
*                                                                     *
*       The return value is an index of the weekday, with             *
*       0 = Sunday, 1 = Monday, and so on.                            *
*                                                                     *
*       Note that the current implementation of this algorithm        *
*       doesn't include logic to handle centesimal years that         *
*       aren't leap years.  While it will work for years between      *
*       1901 and 2099 inclusive, it will break in 2100.               *
*                                                                     *
*                                                                     *
***********************************************************************
         EJECT ,
***********************************************************************
*                                                                     *
*                                                                     *
*   Input data         =                                              *
*       R0     = Not applicable                                       *
*       R1     = Date in MVS TIME DEC format 0cyydddF                 *
*       R2-10  = Not applicable                                       *
*       R11    = Main program base register                           *
*       R12    = Not applicable                                       *
*       R13    = Standard 18-word OS save area                        *
*       R14    = Return address                                       *
*       R15    = Entry address                                        *
*                                                                     *
*   Registers saved    =  R0 - R15 by stack linkage                   *
*                                                                     *
*   Register usage     =                                              *
*       R0     = Work register                                        *
*       R1     = Work register                                        *
*       R2-13  = Not modified                                         *
*       R14    = Work register                                        *
*       R15    = Work register                                        *
*                                                                     *
*   Registers restored =  R0 - R14                                    *
*                                                                     *
*   Output data        =                                              *
*       R0-14  = Same values as at entry                              *
*       R15    = Pointer to 9-byte area containing day of week.       *
*                                                                     *
*   Exit (normal)      =  Return to caller.                           *
*     Output           =  Index of day of week in R15.                *
*     Return code      =  Not applicable                              *
*                                                                     *
*                                                                     *
***********************************************************************
         EJECT ,
***********************************************************************
*                                                                     *
*        RTDOW: Subroutine to find the day of the week.               *
*                                                                     *
***********************************************************************
         SPACE 1
RTDOW    DS    0H                  Subroutine to find weekday
         USING WORKA,R13           Addressability for work area
         STM   R14,R12,12(R13)     Save caller's registers
         XR    R0,R0               Extract century index
         SLDL  R0,8                 and convert
         A     R0,=F'19'             to actual value c in R0
         SRL   R1,20               Isolate
         O     R1,=F'15'            year YY
         XC    DWORK,DWORK           and
         ST    R1,DWORK+4             convert
         CVB   R1,DWORK                to binary y in R1
         LR    R15,R1                   and in R15
         LR    R14,R0              c in R14
         SLL   R14,1               (2*c) in R14
         LCR   R14,R14             -(2*c) in R14
         SRL   R0,2                int(c/4) in R0
         A     R14,=F'3'           3-(2*c) in R14
         AR    R14,R1              3-(2*c)+y in R14
         SRL   R1,2                int(y/4) in R1
         AR    R14,R0              3-(2*c)+y+int(c/4) in R14
         AR    R14,R1              3-(2*c)+y+int(c/4)+int(y/4) in R14
         L     R1,24(,R13)         Isolate
         N     R1,=A(65535)         day of year
         ST    R1,DWORK+4            and convert
         CVB   R1,DWORK               to binary d in R1
         A     R1,=F'80'           f=j-60+140 in R1
         N     R15,=F'3'           Cheesy leap year test
         BNZ   RTDOWX              Branch if not leap year
         BCTR  R1,0                Else f=j-61+140 in R1
RTDOWX   AR    R14,R1              3+f-(2*c)+y+int(c/4)+int(y/4)       +
                                    in R14
         SRDL  R14,32              3+f-(2*c)+y+int(c/4)+int(y/4)       +
                                    in R14-R15
         D     R14,=F'7'           (3+f-(2*c)+y+int(c/4)+int(y/4))     +
                                    mod 7 in R14
         SLL   R14,4               Point R15 to
         LA    R15,DAYTBL(R14)      character weekday
         L     R14,12(,R13)        Restore
         LM    R0,R12,20(R13)       caller's registers
         BR    R14                 Return to caller
         DROP  R13                 End work area addressability
         EJECT ,
*---------------------------------------------------------------------*
*        Day of the week table.                                       *
*---------------------------------------------------------------------*
DAYTBL   DS    0F
         DC    CL16'SUNDAY'
         DC    CL16'MONDAY'
         DC    CL16'TUESDAY'
         DC    CL16'WEDNESDAY'
         DC    CL16'THURSDAY'
         DC    CL16'FRIDAY'
         DC    CL16'SATURDAY'
         TITLE '    Subroutines'
***********************************************************************
*                                                                     *
*  THIS ROUTINE CALCULATES THE ELAPSED TIME WHICH IS THE DIFFERENCE   *
*                     BETWEEN TWO BINARY TIME                         *
*                                                                     *
*        NOTE: THIS ROUTINE WILL FAIL IF THE START/END DATES          *
*              SPAN JANUARY 1.  A FIX has been under development      *
*              since 1983 but hasn't progressed far in the            *
*              vanilla MVS 3.8 version of IEFACTRT.                   *
*                                                                     *
*        R0  = (AT ENTRY) START TIME IN BINARY 1/100 SECONDS.         *
*        R1  = (AT ENTRY) END TIME IN BINARY 1/100 SECONDS.           *
*              (AT EXIT)  ELAPSED TIME IN BINARY 1/100 SECONDS.       *
*        R7  = Return address.                                        *
*                                                                     *
*        Workregs = R1.                                               *
*                                                                     *
***********************************************************************
         SPACE 2
ELAPSED  DS    0H
         USING WORKA,R13           Addressability for work area
         CP    DATE+2(2),DATEND+2(2)  Same day?
         BNL   ELAP01              Yes, go around add on
         AP    DATE+2(2),=P'1'     Add on 1 day
         A     R1,ONEDAY           And 24 hours to time
         B     ELAPSED             Try for match again
         SPACE 1
ELAP01   DS    0H
         SR    R1,R0               Get difference
         B     TIMEX               Fall into time conversion
         EJECT
***********************************************************************
*                                                                     *
*        TIMEX                                                        *
*                                                                     *
*        This routine translates binary time in 1/100 seconds         *
*        to character "HH:MM:SS.TH" format.                           *
*                                                                     *
*        R1  = (at entry) binary time in 1/100 seconds.               *
*        R7  = Return address.                                        *
*        R15 = Address of field for character output.                 *
*                                                                     *
*        Workregs = R0, R1, R14.                                      *
*                                                                     *
***********************************************************************
         SPACE 1
TIMEX    DS    0H
         MVI   2(R15),COLON        Initialize
         MVI   5(R15),COLON         separators
         MVI   8(R15),DOT            in output field
         LA    R14,100             Get divisor
         XR    R0,R0               Clear high order
         DR    R0,R14              R1 now in seconds
         CVD   R0,DWORK            Convert 100ths
         UNPK  9(2,R15),DWORK       of seconds
         OI    10(R15),F0            to character
         SPACE 1
         XR    R0,R0               Clear
         LA    R14,60              60 secs/min
         DR    R0,R14              R0 has secs, R1 has balance
         CVD   R0,DWORK            Convert
         UNPK  6(2,R15),DWORK       seconds
         OI    7(R15),F0             to character
         SPACE 1
         XR    R0,R0               Clear again
         DR    R0,R14              R0 has minutes, R1 has hours
         CVD   R0,DWORK            Convert
         UNPK  3(2,R15),DWORK       minutes
         OI    4(R15),F0             to character
         SPACE 1
         CVD   R1,DWORK            Convert hours
         OI    DWORK+7,X'0F'       Force printable sign
         LA    R1,1                Assume 2 digits of hours
         TM    DWORK+6,X'F0'       More than 99 hours?
         BZ    TIMEX01             Branch if not
         BCTR  R15,0               Else bump back output pointer
         LA    R1,2                Set machine length for move
         SPACE 1
TIMEX01  DS    0H                  Put hours in output field
         SLL   R1,4                Set length of output field
         EX    R1,SETHOURS         Unpack hours into output
         BR    R7                  Return
         SPACE 1
SETHOURS UNPK  0(*-*,R15),DWORK    ** Executed **
         DROP  R13                 End work area addressability
         EJECT ,
******************************************************************
*                                                                *
*        EDMKRTN:                                                *
*                                                                *
*        Subroutine to perform "EDIT AND MARK" operation to      *
*        format packed decimal values into zoned output with     *
*        "-" as front padding.                                   *
*                                                                *
*        Input:                                                  *
*          Doubleword field "DWORK" contains the packed          *
*          decimal number to be formatted.                       *
*                                                                *
*        Output:                                                 *
*          20-byte field "OUTWORK" contains the value            *
*          formatted in zoned decimal with leading hyphens       *
*          supplied as padding on the front.  There is one       *
*          blank between the hyphens and the first digit of      *
*          the number.  for example:                             *
*                                                                *
*                          000000000245583C                      *
*                                                                *
*          is formatted as:                                      *
*                                                                *
*                         ------------- 245583                   *
*                                                                *
*        R14 = Subroutine return address.                        *
*                                                                *
*        Workregs:  R1.                                          *
*                                                                *
******************************************************************
         SPACE 1
EDMKRTN  DS    0H                  Numeric value edit subroutine
         USING WORKA,R13           Addressability for work area
         LA    R1,EDMKWORK+L'EDMKWORK-1  Assume no leading zeroes
         MVC   EDMKWORK,EDMKMASK   Build edit mask in work area
         EDMK  EDMKWORK,DWORK      Edit value to zoned
         BCTR  R1,0                Blank "-" just before
         MVI   0(R1),C' '           the first significant digit
         BR    R14                 Return to caller
         DROP  R13                 End work area addressability
         EJECT ,
******************************************************************
*                                                                *
*        PUTLINE:                                                *
*                                                                *
*        Subroutine to write a message to the system messages    *
*        data set.                                               *
*                                                                *
*        R0  = (at entry) Length of line to be printed.          *
*        R1  = (at entry) Address of line to be printed.         *
*        R12 = OS Linkage Control Table.                         *
*        R7  = Subroutine return address.                        *
*                                                                *
*        Workregs = R0, R1, R14, R15.                            *
*                                                                *
******************************************************************
         SPACE 1
PUTLINE  DS    0H                  Call IEFYS to write line
          LR    R2,R13              Save our R13
          USING WORKA,R13           Address our workarea
**        LA    R13,SAVEAYS         Point to save area for IEFYS
          DROP  R13                 Lose our workarea
         ST    R1,36(,R12)         Set print line address in LCT
         STH   R0,42(,R12)         Place length in LCT
         L     R15,VIEFYS          Get IEFYS addr
         LTR   R15,R15             Were we linked with IEFYS?
         BZ    PUT01               Branch if not
         BALR  R14,R15             Go to IEFYS via BALR
         B     PUT02               Continue
         SPACE 1
PUT01    DS    0H                  LINK to IEFYS
         LINK  EP=IEFYS            Go to IEFYS via LINK
         SPACE 1
PUT02    DS    0H                  Reload our R13 and return
          LR    R13,R2              Restore our R13
         BR    R7                  Return to caller
         EJECT ,
*----------------------------------------------------------------*
*        Constants and literals.                                 *
*----------------------------------------------------------------*
*BLANKS  DC    A(BLANKS)           Address of blanks
*STARS   DC    A(STARS)            Address of "*" line
VIEFYS   DC    V(IEFYS)            Address of IEFYS or zero
ONEDAY   DC    A(24*60*60*100)     One day in hundredths of seconds
KTST      DC    C'SY0'      ** testing **
         SPACE 1
*----------------------------------------------------------------*
*        Substitution text for step end WTO.                     *
*----------------------------------------------------------------*
KWARMS   DC    C'WARMSTART'        "WARMSTART" text
KET      DC    C'ET='              "ELAPSED" text
KABEND   DC    C'ABEND='           "ABEND" text
KRC      DC    C'RC='              "RC=" text
KFLUSHED DC    C'FLUSHED'          "FLUSHED" text
         SPACE 1
HDCOND   DC    CL17'  CONDITION CODE '       Step hdr for rc
HDABNDS  DC    CL17'COMPLETION CODE S'       Step hdr for system cc
HDABNDU  DC    CL17'COMPLETION CODE U'       Step hdr for user cc
HDFLUSH  DC    CL21'     STEP WAS FLUSHED'   Step hdr for flushed
HEXTRAN  DC    C'0123456789ABCDEF' Hex-to-character translate tbl
MSHDRTX  DC    C'<==STEP END STATISTICS==>'
MSHDREX  DC    C'<==STEP EXCP STATISTICS==>'
MJHDRTX  DC    C'<==JOB END STATISTICS==>'
         SPACE 1
ROUT2    DC    XL4'04004000'       DESC=6,ROUTCDE=2
         SPACE 1
*----------------------------------------------------------------*
*        Edit mask for EDMKRTN subroutine.                       *
*----------------------------------------------------------------*
EDMKMASK DS    0XL20               Edit mask for EDMKRTN
         DC    CL5'-----'          Pad character and constants
         DC    13X'20'             Nonsignificant digits
         DC    XL2'2120'           Force last digit significant
         SPACE 1
*----------------------------------------------------------------*
*        Some useful equates.                                    *
*----------------------------------------------------------------*
COLON    EQU   C':'
F0       EQU   X'F0'
DOT      EQU   C'.'
         SPACE 1
*----------------------------------------------------------------*
*        Literal pool.                                           *
*----------------------------------------------------------------*
         LTORG ,                   Generate literal pool
         EJECT ,
******************************************************************
*                                                                *
*        Constants unsupported by the base registers.            *
*                                                                *
******************************************************************
         SPACE 1
*----------------------------------------------------------------*
*        A line of blanks.                                       *
*----------------------------------------------------------------*
*LANKS   DC    CL132' '            A line of blanks
         SPACE 1
*----------------------------------------------------------------*
*        "****" line for statistics box.                         *
*----------------------------------------------------------------*
*TARS    DS    0CL132              Line of asterisks
*        DC    132C'*'
         SPACE 1
******************************************************************
*                                                                *
*        Model common heading line data.                         *
*                                                                *
******************************************************************
         SPACE 1
MHDATAX  DS    0C                  Data for heading line
         DC    CL1' '
         DC    C'OS/360'
         DC    CL1' '
MHOSCP   DS    CL3                 OS control program
         DC    CL1' '
         DC    C'RELEASE'
         DC    CL1' '
MHOSREL  DS    CL2                 OS release
         DC    CL1'.'
MHOSMOD  DS    CL2                 OS modification
         DC    CL4' '
         DC    C'SYSTEM'
         DC    CL1' '
MHSID    DS    CL4                 SMF system ID
         DC    CL1' '
LMHDATA  EQU   *-MHDATAX
         ORG   MHDATAX
MHDATA   DS    CL(LMHDATA)         Alias with length
         ORG   ,
         EJECT ,
******************************************************************
*                                                                *
*        Model for step-end line 1.                              *
*                                                                *
******************************************************************
         SPACE 1
         DS    0H                  Force halfword alignment
MSTEPLN1 DS    0CL132              Model step line 1
         DC    CL1'*'
         DC    CL3' '
         DC    CL10'STEP NUM  '
MSTEPNO  DC    CL3' '              Step number
         DC    CL8' '
         DC    CL15'STEP INIT TIME '
MSINITTM DC    CL11'***********'   Step start time
         DC    CL3' '
         DC    CL15'ALLOC START    '
MS1ALCST DC    CL11' '             Allocation start time
         DC    CL3' '
MKREGION DC    CL12'MAIN REGION '
MSH0REG  DC    CL9' '              Hierarchy 0 region requested
         DC    CL1'K'
         DC    CL26' '
         DC    CL1'*'
         DC    0S(L'MSTEPLN1-(*-MSTEPLN1))
MS1LEN   EQU   *-MSTEPLN1          Symbolic length of step line 1
         EJECT ,
******************************************************************
*                                                                *
*        Model for step-end line 2.                              *
*                                                                *
******************************************************************
         SPACE 1
         DS    0H                  Force halfword alignment
MSTEPLN2 DS    0CL132
         DC    CL1'*'
         DC    CL3' '
         DC    CL10'STEP NAME '
MSTEPNM  DC    CL8'********'       Step name
         DC    CL3' '
         DC    CL15'STEP END TIME  '
MSENDTM  DC    CL11'***********'   Step end time
         DC    CL3'  '
         DC    CL15'PGM START TIME '
MS2PGMST DC    CL11' '             Problem program start time
         DC    CL3'  '
         DC    CL13'MAIN STORAGE '
MSH0STOR DC    CL8' '              Hierarchy 0 region requested
         DC    CL1'K'
         DC    CL2' '
MSTEPCCH DC    CL17' '             Completion code header
MSTEPCON DC    CL4' '              Condition code/completion code
         DC    CL3' '
         DC    CL1'*'
         DC    0S(L'MSTEPLN2-(*-MSTEPLN2))
MS2LEN   EQU   *-MSTEPLN2          Symbolic length of step line 2
         EJECT ,
******************************************************************
*                                                                *
*        Model for step-end line 3.                              *
*                                                                *
******************************************************************
         SPACE 1
         DS    0H                  Force halfword alignment
MSTEPLN3 DS    0CL132
         DC    CL1'*'
         DC    CL3' '
         DC    CL10'PSTP NAME '
MSPSTEPN DC    CL8'********'       Procstep name
         DC    CL3' '
         DC    CL15'ELAPSED TIME   '
MSELAPS  DC    CL11'***********'   Step elapsed time
         DC    CL3' '
**       DC    CL15'STEP DPRTY '
**3DPRTY DC    CL3' '              Step dispatching priority
**       DC    CL8' '
         DC    CL11'STEP DPRTY '
MS3DPRTY DC    CL15' '             Step dispatching priority
         DC    CL3' '
         DC    CL11'LCS REGION '
MS3H1REG DC    CL10' '             Hierarchy 1 region requested
         DC    CL1'K'
         DC    CL26' '
         DC    CL1'*'
         DC    0S(L'MSTEPLN3-(*-MSTEPLN3))
MS3LEN   EQU   *-MSTEPLN3          Symbolic length of step line 3
         EJECT ,
******************************************************************
*                                                                *
*        Model for step-end line 4.                              *
*                                                                *
******************************************************************
         SPACE 1
         DS    0H                  Force halfword alignment
MSTEPLN4 DS    0CL132              Model for step-end line 4
         DC    CL1'*'
         DC    CL3' '
         DC    CL10'PGM NAME  '
MS4PGMNM DC    CL8'********'       Program name
         DC    CL3' '
         DC    CL15'STEP CPU TIME  '
MS4CPUTM DC    CL11'***********'   Step CPU time under TCBs
         DC    CL3' '
**       DC    CL15'PROTECT KEY    '
**4PKEY  DC    CL2' '              Protect key
**       DC    CL9' '
         DC    CL12'PROTECT KEY '
MS4PKEY  DC    CL14' '             Protect key
         DC    CL3' '
         DC    CL13'LCS STORAGE '
MS4H1STO DC    CL8' '              Hierarchy 1 storage
         DC    CL1'K'
         DC    CL26' '
         DC    CL1'*'
         DC    0S(L'MSTEPLN4-(*-MSTEPLN4))
MS4LEN   EQU   *-MSTEPLN4          Symbolic length of step line 4
         EJECT ,
******************************************************************
*                                                                *
*        Model for step-end EXCP column header line.             *
*                                                                *
******************************************************************
         SPACE 1
STEPXCOL DS    0CL132              Step-end EXCP column headers
         DC    C'*'
MSXCOLFS EQU   *                   First EXCP column header
         DC    C'   UNIT     EXCP COUNT    '
SXCOLLEN EQU   *-MSXCOLFS          Symbolic length of each column
         DC    C'   UNIT     EXCP COUNT    '
         DC    C'   UNIT     EXCP COUNT    '
         DC    C'   UNIT     EXCP COUNT    '
         DC    C'   UNIT     EXCP COUNT    '
SXCOLNUM EQU   (*-MSXCOLFS)/SXCOLLEN   Number of EXCP columns
         DC    C'*'
         DC    0S(L'STEPXCOL-(*-STEPXCOL))
         EJECT ,
******************************************************************
*                                                                *
*        Model for job-end line 1.                               *
*                                                                *
******************************************************************
         SPACE 1
         DS    0H                  Force halfword alignment
MJOBLN1  DS    0CL132              Model for job line 1
         DC    CL4'*   '
         DC    CL8'JOB NAME'
         DC    CL2' '
MJ1JNAME DC    CL8'********'       Job name
         DC    CL3' '
         DC    CL13'JOB READ TIME'
         DC    CL1' '
MJ1RDTM  DC    CL11'***********'   Time job entered system
         DC    CL3' '
         DC    CL13'JOB READ DATE'
         DC    CL1' '
MJ1RDDTJ DC    C'********'         Job read date in Julian
         DC    CL1' '
MJ1RDDTG DC    C'**********'       Job read date in Gregorian
         DC    CL1' '
MJ1RDDAY DC    CL9'*********'      Job read date in day of week
         DC    CL3' '
         DC    CL12'JOB CPU TIME'
         DC    CL6' '
MJ1TMTCB DC    CL11'***********'   Job CPU time under TCBs
         DC    CL4'   *'
         DC    0S(L'MJOBLN1-(*-MJOBLN1))
MJ1LEN   EQU   *-MJOBLN1           Symbolic length of job line 1
         EJECT ,
******************************************************************
*                                                                *
*        Model for job-end line 2.                               *
*                                                                *
******************************************************************
         SPACE 1
         DS    0H                  Force halfword alignment
MJOBLN2  DS    0CL132              Model for job line 2
         DC    CL4'*   '
         DC    C'JOB CLASS'
         DC    CL1' '
MJ2CLS   DC    CL1' '              Job selection class
         DC    CL10' '
         DC    C'JOB INIT TIME'
         DC    CL1' '
MJ2INTM  DC    CL11'***********'   Time job was initiated
         DC    CL3' '
         DC    C'JOB INIT DATE'
         DC    CL1' '
MJ2INDTJ DC    C'********'         Job start date in Julian
         DC    CL1' '
MJ2INDTG DC    C'**********'       Job start date in Gregorian
         DC    CL1' '
MJ2INDAY DC    C'*********'        Job start date in day of week
         DC    CL3' '
         DC    C'JOB ELAPSED TIME'
         DC    CL2' '
MJ2TMELP DC    CL11'***********'   Total elapsed time of job
         DC    CL4'   *'
         DC    0S(L'MJOBLN2-(*-MJOBLN2))
MJ2LEN   EQU   *-MJOBLN2           Symbolic length of job line 2
         EJECT ,
******************************************************************
*                                                                *
*        Model for job-end line 3.                               *
*                                                                *
******************************************************************
         SPACE 1
         DS    0H                  Force halfword alignment
MJOBLN3  DS    0CL132              Model for job line 3
         DC    CL4'*   '
MJ3KACT  DC    C'ACCOUNT'          ACCOUNT/JOB ID keyword
         DC    CL3' '
MJ3ACCT  DC    C'********'         Job account number field 1
         ORG   MJ3ACCT
MJ3JBID  DC    C'********'         HASP/ASP jobid
         ORG   ,
         DC    CL3' '
         DC    C'JOB END TIME'
         DC    CL2' '
MJ3ENTM  DC    C'***********'      Time job ended
         DC    CL3' '
         DC    C'JOB END DATE'
         DC    CL2' '
MJ3ENDTJ DC    C'********'         Job end date in Julian
         DC    CL1' '
MJ3ENDTG DC    C'**********'       Job end date in Gregorian
         DC    CL1' '
MJ3ENDAY DC    C'*********'        Job end date in day of week
         DC    CL3' '
**       DC    C'JOB PRIORITY'
**       DC    CL6' '
**3PRTY  DC    CL11' '             Job priority
         DC    CL13'JOB PRIORITY '
*                   1234567890123
MJ3PRTY  DC    CL16' '             Job priority
         DC    CL4'   *'
         DC    0S(L'MJOBLN3-(*-MJOBLN3))
MJ3LEN   EQU   *-MJOBLN3           Symbolic length of job line 3
         EJECT ,
LCT      DSECT ,
*        IEFALLCT                                                       00900020
*                  *********************************                    01200020
*                  *  LINKAGE CONTROL TABLE - LCT  *                    01500020
*                  *********************************                    01800020
**********************************************************************  01810020
*     THIS MACRO WAS REWRITTEN FOR RELEASE 20.2 TO INCLUDE MANY         01850020
*     FIELD NAMES WHICH HERETOFORE APPEARED ONLY IN THE MODULES         01900020
*     WHICH USED THEM.                                                  01950020
**********************************************************************  01960020
*                                                                 20874 02000020
         DS    0F                                                       02400020
LCTQDRTY DS    CL4 -                   BITS 4-7 OF HIGH ORDER BYTE      02700021
*                                      CONTAIN LPMOD VALUE (USED WHEN   03000020
*                                      ATTACHING P/P).  THE THREE LOW   03300020
*                                      ORDER BYTES CONTAIN THE ADDRESS  03600020
*                                      OF THE JOB'S CSCB.               03900020
LCTSRTAD DS    CL4 -                   SRT ADDRESS                      04200021
LCTTCBAD DS    CL4 -                   JOB STEP TCB ADDRESS             04500021
LCTQENTY DS    CL4 -                   BIT 1 OF HIGH ORDER BYTE USED    04800021
*                                      IN CONJUNCTION WITH 'NOSEP'.     05100020
*                                      BIT 2 - DEVICE WAIT RECOVERY     05400020
*                                      BIT 3 - SPACE WAIT RECOVERY      05700020
LCTERRM  EQU   1 -                     BIT 7 - JOB TERMINATION STATUS   06000021
*                                      THE THREE LOW ORDER BYTES CON-   06300020
*                                      TAIN THE ADDRESS OF THE          06600020
*                                      LINKOR'S REGISTER SAVE AREA.     06900020
LCTJCTAD DS    CL4 -                   JCT ADDRESS                      07200021
LCTSCTAD DS    CL4 -                   SCT ADDRESS                      07500021
LCTWORKA EQU   * -                     MINSYS 3 TEMPORARY INSERT        07800021
LCTSCTDA DS    CL4 -                   CURRENT SCT DISK ADDRESS         08100021
LCTPSPAR DS    CL4 -                   ADDRESS OF RESIDENT BLOCK OF     08400021
*                                      STORAGE FOR MVT ALLOCATE.        08700020
LCTERROR DS    CL4 -                   ERROR CODE                       09000021
LCTPARM1 DS    CL4 -                   MULTI USE PARAMETER FIELD        09300021
LCTPARM2 DS    CL4 -                   MULTI USE PARAMETER FIELD        09600021
LCTPARM3 DS    CL4 -                   MULTI USE PARAMETER FIELD        09900021
LCTPARM4 DS    CL4 -                   MULTI USE PARAMETER FIELD        10200021
LCTCMCBA DS    CL4 -                   CORE ADDRESS OF CONTROL          10500021
*                                      BYTES FOR CORE MANAGEMENT        10800020
LCTSTIND EQU   * -                     STATUS INDICATOR FIELD           11100021
LCTNSPAD DS    CL1 -                   NON SETUP PADDING BYTE           11400021
LCTJFCBH DS    CL1 -                   JFCB HOUSEKEEPPING BYTE          11700021
LCTS2PEM EQU   128 -                   1ST PDQ TBL ENTRY MADE           12000021
*                                      IS INDICATOR                     12300020
LCTS2COP EQU   64 -                    CORE OBTAINED FOR PDQ            12600021
*                                      TABLE INDICATOR                  12900020
LCTS2FES EQU   32 -                    FIRST ENTRY INTO PDQ PROC        13200021
*                                      S/R FOR THE STEP INDICATR        13500020
LCTSNUMB DS    CL1 -                   CURRENT STEP NUMBER              13800021
LCTACTON DS    CL1 -                   ACTION CODE                      14100021
LCTSMBAD DS    CL4 -                   STEP TIOT POINTER IN SSS         14400021
*                                      SMB ADDRESS IN PPS AND PSS       14700020
LCTREGSV EQU   0 -                     NEW DESIGN                       15000021
LCTQMPAM EQU   144 -                   Q-MGR PARAMETER AREA             15300021
LCTCOREA EQU   128 -                   16 BYTE GETMAIN AREA             15600021
*                                                                     * 15900020
LCTBATMN DS    1F -                    USED IN GENERATING A UNIQUE    * 16200021
*                                      VOLUME SERIAL NUMBER WHEN THE  * 16500020
*                                      USER DOESN'T SPECIFY ONE ON    * 16800020
*                                      HIS DD CARD AND DOES SPECIFY   * 17100020
*                                      A PASSED DATA SET ON UNLABELED * 17400020
*                                      TAPE.                            17700021
LCTSOQMP DS    1F -                    ADDRESS OF MESSAGE CLASS QUEUE   18000021
*                                      MANAGER PARAMETER AREA           18300021
LCTRTRN DS     1F                      RETURN ADDRESS TO MASTER         18600021
*                                      SCHEDULER (FOR STOP INITIATOR)   18900021
LCTINTSW DS    0C -                    INITIATORS INTERNAL SWITCHES     19200021
LCTIHIER EQU   128 -                   RUN IN HIERARCHY ONE             19500021
LCTSD0XX EQU   32 -                    ATTACH IEFSD0XX                  19800021
LCTMINRG EQU   16 -                    JOB FLUSH - USE MINPAR           20100021
LCTSTART EQU   8 -                     TASKNAME NOT FOUND ON COMMAND    20400021
LCTSTOP  EQU   4 -                     INITIATOR INTERNAL STOP          20700021
LCTABEND EQU   2 -                     EXECUTED PROGRAM ABENDED         20750021
LCTCSCB DS     1F -                    INITIATOR'S CSCB ADDRESS         21000021
LCTTMWRK DS    4F -                    TIMER WORK AREA CONSISTING OF    21300021
*                                      4 FULL WORDS USED AS FOLLOWS-    21600021
*                                                                       21900021
*                                      1ST WD - TOTAL JOB TIME USED     22200021
*                                      2ND WD - STEP TIME               22500021
*                                      3RD WD - TIME REMAINING(STEP)    22800021
*                                      4TH WD - TIME USED (STEP)        23100021
LCTSMF   EQU  LCTTMWRK+8 -             SMF - PTR TO DEVICES USED        23400021
*                                      OR TO JMR                        23410021
*                                      INFORMATION                      23700021
LCTJOBLB DS   1F -                     ADDRESS OF JOBLIB OR             24000021
*                                      STEPLIB DCB                      24300021
LCTATLST DS    1F -                    ADDRESS OF ALLOCATE-TERMINATE    24600021
*                                      PARAMETER LISTS                  24900021
REGSAVE  DS   36F -                    ALLOCATE/TERMINATE               25200021
*                                      REGISTER SAVEAREA                25250021
QMGR1    DS   9F -                     QUEUE MANAGER PARAMETER AREA     25500021
QMGR2    DS   9F -                     ALTERNATE Q-MGR PARAMETER AREA   25800021
TRSTKINF DS   2F -                     TRACK STACKING AND QUEUE BREAK   26100021
*                                      INFORMATION                      26150021
*                                      BYTE 1 - NUMBER OF BUFFERS       26400021
*                                      BYTES 2-4 - STACK ADDRESS        26700021
*                                      BYTES 5-8 - QUEUE BREAK          27000021
*                                      INFORMATION                      27050021
ECBLIST  DS   1F                                                        27300021
LCTECBAD EQU   ECBLIST -               REMOVE ECBLIST AND REPLACE       27600021
*                                      WITH LCTECBAD                    27650021
LCTIDENT DC    2F'0' -                 HOLDER FOR IDENTIFIER            27900021
LCTPIB   EQU   LCTIDENT                                                 28200020
LCTSPIL  EQU   LCTIDENT+4                                               28500020
LCTECBLT EQU   * -                     USED IN MFT ONLY                 28800021
LCTFORCE DS    CL8 -                   POSSIBLE FORCE VALUES            29100021
LCTLIMIT DS    C -                     LIMIT VALUE                      29400021
FRCPRTY  DS    C -                     FORCE PARITY HOLDER              29700021
INITPRTY DS    C -                     INITIATOR'S PRIORTY              30000021
*          THESE FIELDS ARE NEEDED FOR L-SHAPE/INIT MERGE               30300021
         DS    0F                                                       30600020
LCTOPSW1 DS    0C -                    INITIATOR'S OPTION BYTE 1        30900021
LCTPKEYF EQU   128 -                   DON'T GET PROTECT KEY            31200021
LCTDWFF  EQU   64 -                    DON'T PROCESS DEDICATED WORK     31500021
*                                      FILES                            31550021
LCTSTMDF EQU   32 -                    DON'T PROCESS STOP/MODIFY        31800021
LCTMINPF EQU   16 -                    GET REGION SIZE SPECIFIED        32100021
LCTCANF  EQU   8 -                     ALLOW CANCEL ONLY AT ALLOCATION  32400021
LCTONEJF EQU   4 -                     PROCESS ONLY ONE JOB             32700021
LCTICMDF EQU   2 -                     DON'T PROCESS INITIATOR'S        33000021
*                                      COMMANDS                         33050021
LCTENTR  DS    F -                     ADDRESS OF INITIATOR EXIT LIST   33300021
LCTOPSW2 DS    0C -                    INITIATOR OPTIONS BYTE 2         33600021
LCTTIMEF EQU   128                                                      33900020
LCTCRF   EQU   64 -                    DON'T ALLOW CHECK/RESTART        34200021
LCTDSOF  EQU   32 -                    DON'T PROCESS DSO                34500021
LCTINTH0 EQU   16 -                    INIT IN HIERARCHY ZERO           34800021
LCTINTH1 EQU   8 -                     INIT IN HIERARCHY ONE            35100021
LCTENQU  EQU   1 -                     DON'T WAIT FOR DATA SETS         35150021
LCTCOM   DS    F -                     COMMUNICATIONS PARM AREA         35400021
*                                      POINTER                          35450021
LCTOPSW3 DS   0C -                     INITIATOR OPTION BYTE THREE      36000021
LCTTRSTK EQU   4 -                     INDICATES THAT TRACK STACKING    36002021
*                                      IS IN                            36004021
LCTJSCB  DS    F -                     ADDRESS OF JSCB                  36010021
         DS    4F -                    UNUSED IN MVT,PLACED HERE        36022021
*                                      TO MAKE LENGTH THE SAME AS MFT   36024021
IEFEND   EQU   * -                     END OF LCT                       36030021
         EJECT ,
JCT      DSECT ,
         IEFAJCTB ,
         SPACE 1
*----------------------------------------------------------------*
*        Additional bits defined in JCTJSTAT.                    *
*----------------------------------------------------------------*
JCTJOBFL EQU   X'40'               Bit 1: job flush bit
JCTSTPFL EQU   X'10'               Bit 3: step flush bit
         EJECT ,
SCT      DSECT ,
         IEFASCTB ,
         EJECT ,
         PRINT NOGEN
CVT      DSECT ,
         CVT   SYS=MVT,PREFIX=YES
         ORG   CVTRELNO
CVTNUMB  DS    CL2 -         RELEASE NUMBER
CVTLEVL  DS    CL2 -         LEVEL NUMBER OF THIS RELEASE
         EJECT ,
TCB      DSECT ,
         IKJTCB SYS=MVT
         EJECT ,
JMR      DSECT ,                                                        03000018
*********************************************************************** 04000018
*                                                                     * 05000018
*        JMR -  JOB MANAGEMENT RECORD                                 * 06000018
*                                                                     * 07000018
*********************************************************************** 08000018
JMRJOB   DS    8C        JOB NAME                                       09000018
JMRENTRY DS    F         ENTRY TIME IN 1/100'S SEC                      10000018
JMREDATE DS    F         ENTRY DATE 00YYDDDF                            11000018
JMRCPUID DS    4C        CPU - SID AND MDL FROM SMCA                    12000018
JMRUSEID DS    8C        USER ID - INITIALIZED BLANK BY R/I EACH JOB    13000018
JMRSTEP  DS    C         STEP NUMBER                                    14000018
JMRLGEND EQU   *                                                        15000018
JMRLOGSZ EQU   JMRLGEND-JMRJOB  SIZE OF JOB LOG COPIED TO DSB           16000018
*********************************************************************   17000018
*                                                                       18000018
JMRINDC  DS    C                       INDICATOR SWITCHES         20011 18400020
* BIT MEANINGS SAME AS JMROPT FIELD                               20011 18600020
*                                                                       18800020
**********************************************************************  19000020
         DS    2C                      RESERVED                   20011 19200020
JMRUCOM  DS    F         USER COMMUNICATION - INITIALIZED 0             20000018
JMRUJVP  DS    F    CORE ADDR OF IEFUJV / PTR TO SYS1.MAN RECORD        21000018
JMRSIZE  EQU   *-JMR   SIZE OF JMR IN CORE                              22000018
*                                                                       23000018
*********************************************************************   24000018
*                                                                       25000018
JMRDRSTP DS    2F   RDR STOP TIME AND DATE                              26000018
JMRJOBIN DS    F    JOB SYSIN CT                                        27000018
JMRRDR   DS    2C   RDR DEVICE CLASS AND TYPE                           28000018
JMROPT   DS    1C   OPTION SWITCHES                                     29000018
*                                                                       30000018
JMRJOBSW EQU   X'80'  JOB FUNCTIONS REQUESTED                           31000018
JMRSTPSW EQU   X'40'  STEP FUNCTIONS REQUESTED                          32000018
JMREXITS EQU   X'20'    USER EXITS REQUESTED                            33000018
JMRXONLY EQU   X'10'     EXITS ONLY SPECIFIED                           34000018
JMRFIND  EQU   X'01'                   FOREGROUND INDICATED       20011 34500020
*                                                                       35000018
         DS    C                  RESERVED                              36000018
         DS    0F                                                       37000018
JMRSYSOC DS    5C   SYSOUT CLASSES                                      38000018
*                                                                       39000018
*********************************************************************** 40000018
*                                                                       41000018
****PARM LIST PASSED TO IEFUJV IN R/I                                   42000018
*                                                                       43000018
JMRJCLCD DS    C    JCL CODE                                            44000018
         DS    2C                                                       45000018
JMRJOBP  DS    F    PTR TO JOB LOG                                      46000018
JMRJCLP  DS    F    PTR TO JCL CARD                                     47000018
JMRJCLCP DS    F    PTR TO JCL CODE                                     48000018
JMRPTRS  EQU   JMRJOBP                                                  49000018
         EJECT ,
*        TITLE '    SMF Type 4 and Type 5 Records'
******************************************************************
*                                                                *
*        SMF record mappings.                                    *
*                                                                *
******************************************************************
         SPACE 1
SMFRCD   DSECT ,
* THIS RECORD IS WRITTEN AT NORMAL OR ABNORMAL TERMINATION OF A JOB     20520019
* STEP.RECORD LENGTH IS VARIABLE.                                       20610019
*                                                                       20700019
         DS    0F        ALIGN TO FULL WORD BOUNDARY                    20790019
SMFRCD4  EQU   *         HEADER SEGMENT                                 20880019
SMF4LEN  DS    BL2'0'    RECORD LENGTH                                  20970019
SMF4SEG  DS    BL2'0'    SEGMENT DESCRIPTOR                             21060019
SMF4RSV  DC    BL1'0'    RESERVED                                       21150019
SMF4RTY  DC    BL1'0'    RECORD TYPE 4                                  21240019
SMF4TME  DC    4BL1'0'   TOD USING FORMAT FROM TIME MACRO W/BIN. INTVL. 21330019
SMF4DTE  DC    PL4'0000' DATE IN PACKED DECIMAL FORM: OOY4DDDF          21420019
SMF4SID  DC    2CL1' '   SYSTEM IDENTIFICATION                          21510019
SMF4MOD  DC    2CL1' '   MODEL NUMBER                                   21600019
SMF4JBN  DC    8CL1' '   JOB NAME                                       21690019
SMF4RST  DC    4BL1'0'   READER START TIME (IN 100THS SECONDS)          21780019
SMF4RDS  DC    PL4'0000' READER START DATE                              21870019
SMF4UIF  DC    8CL1' '   USER IDENTIFICATION FIELD                      21960019
SMF4STN  DC    BL1'0'    STEP NUMBER                                    22050019
SMF4SIT  DC    4BL1'0'   STEP INITIATION TIME (IN 100THS SECONDS)       22140019
SMF4STID DC    PL4'0000' STEP INITIATION DATE (PACKED DECIMAL FORMAT)   22230019
SMF4NCI  DC    4BL1'0'   NUMBER OF CARD IMAGES IN DD DATA OR DD  *      22320019
*                        DATA SETS                                      22410019
SMF4SCC  DC    2BL1'0'   STEP COMPLETION CODE                           22500019
SMF4PRTY DC    BL1'0'    PRIORITY AT WHICH STEP WAS DISPATCHED :        22590019
*                        ACTUAL PRIORITY=251-(15-USER PRIORITY)*16      22680019
SMF4PGMN DC    CL8' '    PROGRAM NAME                                   22770019
SMF4STMN DC    CL8' '    STEP NAME                                      22860019
SMF4RSH0 DC    2BL1'0'   REGION SIZE IN 1K BLKS REQUESTED IN HIERACHY0  22950019
SMF4RSH1 DC    2BL1'0'   REGION SIZE IN 1K BLKS REQUESTED IN HIERARCHY1 23040019
SMF4H0ST DC    4BL1'0'   HIERARCHY0 STORAGE USED                        23130019
SMF4H1ST DC    4BL1'0'   HIERARCHY1 STORAGE USED                        23220019
SMF4SPK  DC    1BL1'0'   STORAGE PROTECT KEY                     A40791 23270021
         DC    3BL1'0'   RESERVED                                A40791 23290021
SMF4AST  DC    4BL1'0'   DEVICE ALLOC START TIME                 A40791 23310021
SMF4PPST DC    4BL1'0'   PROBLEM PROGRAM START TIME              A40791 23360021
         DC    8BL1'0'   RESERVED                                A40791 23370021
SMF4LENN DC    2BL1'0'   LENGTH OF EXCP PORTION OF RECORD               23400019
*        FOR EACH DEVICE ASSIGNED TO EACH DATA SET THERE IS AN 8 BYTE   23490019
*        ENTRY HAVING THE FOLLOWING FORMAT:                             23580019
*                                                                       23670019
SMF4DEVC DC    BL1'0'    DEVICE CLASS                                   23760019
SMF4UTYP DC    BL1'0'    UNIT TYPE                                      23850019
SMF4CUAD DC    2BL1'0'   CHANNEL AND UNIT ADDRESS                       23940019
SMF4EXCP DC    4BL1'0'   COUNT OF EXCP'S ISSUED FOR THE DEVICE AND      24030019
*                        DATA SET                                       24120019
         ORG   SMF4DEVC                                                 24210019
SMF4LNTH DC    BL1'0'    TOTAL LENGTH OF NEXT THREE FIELDS              24300019
SMF4SETM DC    3BL1'0'   STEP EXECUTION TIME (IN 100THS SECONDS)        24390019
SMF4NAF  DC    BL1'0'    NUMBER OF ACCOUNTING FIELDS                    24480019
SMF4ACTF DS    0C        EXEC STATEMENT ACCT FIELDS(VARIABLE)    100000 24570021
*                                                                       24660019
*        EACH ENTRY FOR AN ACCOUNTING FIELD CONTAINS THE LENGTH OF THE  24750019
*        FIELD (1 BYTE,BINARY), FOLLOWED BY THE FIELD(EBCDIC).AN        24840019
*        OMITTED FIELD IS REPRESENTED BY A LENGTH INDICATOR OF 0.       24930019
         EJECT                                                          25200019
         ORG   SMFRCD              Reset location counter
* THIS RECORD IS WRITTEN AT  NORMAL OR ABNORMAL TERMINATION OF A JOB.   25290019
* RECORD LENGTH IS VARIABLE.                                            25380019
*                                                                       25470019
         DS    0F        ALIGN TO FULL WORD BOUNDARY                    25560019
SMFRCD5  EQU   *         HEADER SEGMENT                                 25650019
SMF5LEN  DS    BL2'0'    RECORD LENGTH                                  25740019
SMF5SEG  DS    BL2'0'    SEGMENT DESCRIPTOR                             25830019
SMF5RSV  DC    BL1'0'    RESERVED                                       25920019
SMF5RTY  DC    BL1'0'    RECORD TYPE 5                                  26010019
SMF5TME  DC    4BL1'0'   TOD USING FORMAT FROM TIME MACRO W/BIN. INTVL. 26100019
SMF5DTE  DC    PL4'0000' DATE, PACKED DEC. FORM : OOYYDDDF(F IS A SIGN) 26190019
SMF5SID  DC    2CL1' '   SYSTEM IDENTIFICATION                          26280019
SMF5MOD  DC    2CL1' '   MODEL NUMBER                            100000 26370021
SMF5JBN  DC    8CL1' '   JOB NAME                                       26460019
SMF5RST  DC    4BL1'0'   READER START TIME FOR JOB (IN 100THS/SECONDS)  26550019
SMF5RSD  DC    PL4'0000' READER START DATE,PACKED DECIMAL FORMAT        26640019
SMF5UIF  DC    8CL1' '   USER IDENTIFICATION FIELD                      26730019
SMF5NST  DC    BL1'0'    NUMBER OF STEPS IN THE JOB                     26820019
SMF5JIT  DC    4BL1'0'   JOB INITIATION TIME (IN 100THS/SECONDS)        26910019
SMF5JID  DC    PL4'0000' JOB INITIATION DATE, PACKED DECIMAL FORMAT     27000019
SMF5NCI  DC    4BL1'0'   NUMBER OF CARD-IMAGE RECORDS IN DD DATA OR DD* 27090019
*                        DATA SETS                                      27180019
SMF5JCC  DC    2BL1'0'   JOB COMPLETION CODE                            27270019
SMF5JPTY DC    BL1'0'    JOB PRIORITY                                   27360019
SMF5RSTT DC    4BL1'0'   READER STOP TIME FOR JOB (IN 100THS/SECONDS)   27450019
SMF5RSTD DC    PL4'0000' READER STOP DATE FOR JOB (IN PACKED DEC.FORM)  27540019
SMF5JBTI DC    BL1'0'    JOB TERMINATION INDICATOR                      27630019
*                        BIT 0 - RESERVED                               27720019
*                                                                       27810019
*                           1-CANCELLED AT EXIT IEFUJV                  27900019
*                           2-CANCELLED AT EXIT IEFUJI                  27990019
*                           3-CANCELLED AT EXIT IEFUSI                  28080019
*                           4-CANCELLED AT EXIT IEFACTRT                28170019
*                           5-RESERVED                           100000 28260021
*                           6-O=NORMAL COMPLETION                       28350019
*                             1=ABEND                                   28440019
*                           7-RESERVED                                  28530019
SMF5SMCI DC    5BL1'0'   SYSOUT CLASSES AND MSGCLASS INDICATOR          28620019
SMF5CKRE DC    BL1'0'    CHECKPOINT/RESTART INDICATOR                   28710019
*                        BITO-SYSTEM RESTART                            28800019
*                         1,2-RESERVED                                  28890019
*                           3-CHECKPOINT TAKEN FOR STEP                 28980019
*                           4-CHECKPOINT RESTART                        29070019
*                           5-STEP RESTART                              29160019
*                         6,7-RESERVED(MUST BE ZERO)                    29250019
*                                                                       29340019
SMF5RDCL DC    BL1'0'    READER DEVICE CLASS                            29430019
SMF5RUTY DC    BL1'0'    READER UNIT TYPE                               29520019
SMF5JICL DC    CL1' '    JOB INPUT CLASS                         100000 29610021
SMF5SPK  DC    1BL1'0'   STORAGE PROTECT KEY                     A40791 29660021
         DC    19BL1'0'  RESERVED                                A40791 29690021
SMF5TLEN DC    BL1'0'    TOTAL LENGTH OF NEXT FOUR FIELDS               29790019
SMF5PRGN DC    20CL1' '  PROGRAMMER'S NAME                              29880019
SMF5JCPU DC    3BL1'0'   JOB CPU TIME (IN 100THS/SECONDS)               29970019
SMF5ACTF DC    BL1'0'    NUMBER OF ACCOUNTING FIELDS                    30060019
SMF5JSAF DS    0C        JOB STATEMENT ACCT FIELDS(VARIABLE)     100000 30150021
*                                                                       30240019
*        EACH ENTRY FOR AN ACCOUNTING FIELD CONTAINS THE LENGTH OF THE  30330019
*        FIELD(1 BYTE,BINARY),FOLLOWED BY THE FIELD (EBCDIC).AN OMITTED 30420019
*        FIELD IS REPRESENTED BY A LENGTH INDICATOR OF 0.               30510019
         ORG   ,                   Restore highest location counter
*----------------------------------------------------------------*
*        Equates for bits defined in SMF4STI.                    *
*----------------------------------------------------------------*
         EJECT ,
******************************************************************
*                                                                *
*        Getmained work area.                                    *
*                                                                *
*        XXX IEFYS save area usage is non-standard and           *
*            needs to be documented in detail (it stores         *
*            R0-R15 into the first 16 words and may use          *
*            the next 18 words as a save area when calling       *
*            queue manager).                                     *
*                                                                *
******************************************************************
         SPACE 1
*----------------------------------------------------------------*
*        Next four lines must remain together.                   *
*----------------------------------------------------------------*
WORKA    DSECT ,                   Start of dynamic work area
SAVEA    DS    16F                 Save area for calling IEFYS
SAVEAQ   DS    18F                 Save area for queue manager
QMPA     DS    9F                  Space for a QMPA
*----------------------------------------------------------------*
*        End of lines that must remain together.                 *
*----------------------------------------------------------------*
         SPACE 1
DWORK    DS    D                   Doubleword work area
PREVR13  DS    F                   Caller's R13
XSTEPNM  DS    CL8                 Step name
XPSTEPNM DS    CL8                 Procedure step name
TIME     DS    F                   Time work area
TIMEND   DS    F                   Time work area
DATE     DS    F                   Date work area
DATEND   DS    F                   Date work area
         SPACE 1
FLAG1    DS    XL1                 Miscellaneous flags
F1EXCPHD EQU   X'80'               -- EXCP header has been done
         SPACE 1
XTEPCCH  DS    0CL21               FLUSHED header
XTEPCCHD DS    CL17                Completion code header
STEPCOND DS    CL4                 Condition code/completion code
PAD     DS    CL1     cheesy circuvention of programming error
XELAPS  DS    CL11    Step elapsed time
*SINITTM DS    CL11    Step start time
XENDTM  DS    CL11     Step end time
         EJECT ,
PRTLINE  DS    CL132               Output area
         EJECT ,
******************************************************************
*                                                                *
*        Work area for EDMKRTN subroutine.  The equates are      *
*        defined with the offsets and lengths necessary to       *
*        move edited fields to their appropriate place in        *
*        an output line.                                         *
*                                                                *
******************************************************************
         SPACE 1
EDMKWORK DS    CL20                Work area for EDMKRTN editing
         SPACE 1
         ORG   EDMKWORK+11
EH0REG   DS    CL9                 Hierarchy 0 region requested
         SPACE 1
         ORG   EDMKWORK+10
EH1REG   DS    CL10                Hierarchy 1 region requested
         SPACE 1
         ORG   EDMKWORK+12
EH0STOR  DS    CL8                 Hierarchy 0 storage used
         SPACE 1
         ORG   EDMKWORK+12
EH1STOR  DS    CL8                 Hierarchy 1 storage used
         SPACE 1
         ORG   EDMKWORK+4
EPRTY    DS    CL16                Job selection priority
         SPACE 1
         ORG   EDMKWORK+5
EDPRTY   DS    CL15                Step dispatching priority
         SPACE 1
         ORG   EDMKWORK+6
EPKEY    DS    CL14                Protect key
         SPACE 1
         ORG   ,                   Restore location counter
         EJECT ,
******************************************************************
*                                                                *
*        Area for step-end WTO.                                  *
*                                                                *
******************************************************************
* STEP: JJJJJJJJ PPPPPPPP SSSSSSSS ET=00.00.28 ABEND=U4095
         SPACE 1
SWTO     DS    0F                  Area for step-end WTO
SWLEN    DS    AL2                 Length of WTO
SWMCSFLG DS    0AL2                Mcsflags
SWMCSF1  DS    AL1                 Mcsflags byte 1
SWMCSA   EQU   X'80'               --Routing/desc codes exist
SWMCSF2  DS    AL1                 Mcsflags byte 2
SWTEXT   DS    0CL56               Text of message
SWHDR    DS    CL5'STEP:'          "STEP:"
         DS    CL1
SWJOBN   DS    CL8                 Jobname
         DS    CL1
SWSTEPN  DS    CL8                 Procstep name
         DS    CL1
SWPSTEPN DS    CL8                 Stepname
         DS    CL1
SWELAPH  DS    CL3'ET='            Elapsed time keyword
SWELAP   DS    CL8                 Step elapsed time HH.MM.SS
         ORG   SWELAPH
SWSTARS  DS    CL11                "*****" if step was flushed
         ORG   ,
         DS    CL1
SWABNDH  DS    CL6'ABEND='         "ABEND=" header
SWABND   DS    CL5                 Completion code
         ORG   SWABNDH
SWFLUSH  DS    CL7'FLUSHED'        "FLUSHED"
         ORG   SWABNDH
SWRCH    DS    CL3'RC='            "RC=" header
SWRC     DS    CL4                 Condition code
         ORG   ,
SWTOLEN  EQU   *-SWTO              Symbolic length for WTO
SWRTDESC DS    CL4                 Area for rout/desc codes
         ORG   ,
         EJECT ,
******************************************************************
*                                                                *
*        Mapping of data portion of common heading line.         *
*        This line is printed at the start of both stepend       *
*        and jobend statistics information boxes.                *
*                                                                *
******************************************************************
         SPACE 1
HDATA    DS    CL(L'MHDATA)        Reserve area
         ORG   HDATA+(MHOSCP-MHDATA)
HOSCP    DS    CL(L'MHOSCP)        OS control program
         ORG   HDATA+(MHOSREL-MHDATA)
HOSREL   DS    CL(L'MHOSREL)       OS release
         ORG   HDATA+(MHOSMOD-MHDATA)
HOSMOD   DS    CL(L'MHOSMOD)       OS modification
         ORG   HDATA+(MHSID-MHDATA)
HSID     DS    CL(L'MHSID)         SMF system id
         ORG   ,                   Reset location counter
         EJECT ,
******************************************************************
*                                                                *
*        Mapping of common header line.                          *
*                                                                *
******************************************************************
         SPACE 1
         ORG   PRTLINE+((L'PRTLINE-L'MHDATA)/2)
CHDATA   DS    CL(L'MHDATA)        Reserve area
         ORG   ,                   Reset location counter
         EJECT ,
******************************************************************
*                                                                *
*        Mapping of "STEP END STATISTICS" header line.           *
*                                                                *
******************************************************************
         ORG   PRTLINE+((L'PRTLINE-L'MSHDRTX)/2)
SHDRTX   DS    CL(L'MSHDRTX)
         ORG   ,                   Reset location counter
         EJECT ,
******************************************************************
*                                                                *
*        Mapping of step-end line 1.                             *
*                                                                *
******************************************************************
         SPACE 1
         ORG   PRTLINE
STEPLN1  DS    CL(MS1LEN)          Define space for step line 1
         ORG   STEPLN1+(MSTEPNO-MSTEPLN1)
STEPNO   DS    CL(L'MSTEPNO)       Step number
         ORG   STEPLN1+(MSINITTM-MSTEPLN1)
SINITTM  DS    CL(L'MSINITTM)      Step start time
         ORG   STEPLN1+(MS1ALCST-MSTEPLN1)
S1ALCST  DS    CL(L'MS1ALCST)      Allocation start time
         ORG   STEPLN1+(MSH0REG-MSTEPLN1)
SH0REG   DS    CL(L'MSH0REG)       Hierarchy 0 region requested
         ORG   ,                   Reset location counter
         EJECT ,
******************************************************************
*                                                                *
*        Mapping of step-end line 2.                             *
*                                                                *
******************************************************************
         SPACE 1
         ORG   PRTLINE
STEPLN2  DS    CL(MS2LEN)
         ORG   STEPLN2+(MSTEPNM-MSTEPLN2)
STEPNM   DS    CL(L'MSTEPNM)       Step name
         ORG   STEPLN2+(MSENDTM-MSTEPLN2)
SENDTM   DS    CL(L'MSENDTM)       Step end time
         ORG   STEPLN2+(MS2PGMST-MSTEPLN2)
S2PGMST  DS    CL(L'MS2PGMST)      Problem program start time
         ORG   STEPLN2+(MSH0STOR-MSTEPLN2)
SH0STOR  DS    CL(L'MSH0STOR)      Hierarchy 0 storage used
         ORG   STEPLN2+(MSTEPCCH-MSTEPLN2)
STEPCCHX DS    CL(L'MSTEPCCH+L'MSTEPCON)  FLUSHED header
         ORG   STEPLN2+(MSTEPCCH-MSTEPLN2)
STEPCCH  DS    CL(L'MSTEPCCH)      Completion code header
         ORG   STEPLN2+(MSTEPCON-MSTEPLN2)
STEPCON  DS    CL(L'MSTEPCON)      Condition code/completion code
         ORG   ,
         EJECT ,
******************************************************************
*                                                                *
*        Mapping of step-end line 3.                             *
*                                                                *
******************************************************************
         SPACE 1
         ORG   PRTLINE
STEPLN3  DS    CL(MS3LEN)
         ORG   STEPLN3+(MSPSTEPN-MSTEPLN3)
SPSTEPN  DS    CL(L'MSPSTEPN)      Procstep name
         ORG   STEPLN3+(MSELAPS-MSTEPLN3)
SELAPS   DS    CL(L'MSELAPS)       Step elapsed time
         ORG   STEPLN3+(MS3DPRTY-MSTEPLN3)
S3DPRTY  DS    CL(L'MS3DPRTY)      Step dispatching priority
         ORG   STEPLN3+(MS3H1REG-MSTEPLN3)
S3H1REG  DS    CL(L'MS3H1REG)      Hierarchy 1 region requested
         ORG   ,
         EJECT ,
******************************************************************
*                                                                *
*        Mapping of step-end line 4.                             *
*                                                                *
******************************************************************
         SPACE 1
         ORG   PRTLINE
STEPLN4  DS    CL(MS4LEN)
         ORG   STEPLN4+(MS4PGMNM-MSTEPLN4)
S4PGMNM  DS    CL(L'MS4PGMNM)      Program name
         ORG   STEPLN4+(MS4CPUTM-MSTEPLN4)
S4CPUTM  DS    CL(L'MS4CPUTM)      Step CPU time under TCBs
         ORG   STEPLN4+(MS4PKEY-MSTEPLN4)
S4PKEY   DS    CL(L'MS4PKEY)       Protect key
         ORG   STEPLN4+(MS4H1STO-MSTEPLN4)
S4H1STO  DS    CL(L'MS4H1STO)      Hierarchy 1 storage used
         ORG   ,
         EJECT ,
PRTLINE2 DS    CL132               Second print line
         EJECT ,
******************************************************************
*                                                                *
*        Mapping of "STEP EXCP STATISTICS" header line.          *
*                                                                *
******************************************************************
         SPACE 1
         ORG   PRTLINE2
         ORG   PRTLINE2+((L'PRTLINE2-L'MSHDREX)/2)
SHDREX   DS    CL(L'MSHDREX)
         ORG   ,
         EJECT ,
******************************************************************
*                                                                *
*        Mapping of step-end EXCP column headings.               *
*                                                                *
******************************************************************
         SPACE 1
         ORG   PRTLINE2
SXCOL    DS    CL132               Step-end EXCP column headers
SXCOLFST EQU   SXCOL+(MSXCOLFS-STEPXCOL)  First column header
         ORG   ,
         EJECT ,
******************************************************************
*                                                                *
*        Mapping of "JOB END STATISTICS" header line.            *
*                                                                *
******************************************************************
         ORG   PRTLINE+((L'PRTLINE-L'MJHDRTX)/2)
JHDRTX   DS    CL(L'MJHDRTX)
         ORG   ,                   Reset location counter
         EJECT ,
******************************************************************
*                                                                *
*        Mapping of job-end line 1.                              *
*                                                                *
******************************************************************
         SPACE 1
         ORG   PRTLINE
JOBLN1   DS    CL(MJ1LEN)          Define space for job line 1
         ORG   JOBLN1+(MJ1JNAME-MJOBLN1)
J1JNAME  DS    CL(L'MJ1JNAME)      Job name
         ORG   JOBLN1+(MJ1RDTM-MJOBLN1)
J1RDTM   DS    CL(L'MJ1RDTM)       Time job entered system
         ORG   JOBLN1+(MJ1RDDTJ-MJOBLN1)
J1RDDTJ  DS    CL(L'MJ1RDDTJ)      Job read date in Julian
         ORG   JOBLN1+(MJ1RDDTG-MJOBLN1)
J1RDDTG  DS    CL(L'MJ1RDDTG)      Job read date in Gregorian
         ORG   JOBLN1+(MJ1RDDAY-MJOBLN1)
J1RDDAY  DS    CL(L'MJ1RDDAY)      Job read date in day of week
         ORG   JOBLN1+(MJ1TMTCB-MJOBLN1)
J1TMTCB  DS    CL(L'MJ1TMTCB)      Job CPU time under TCBs
         ORG   ,                   Reset location counter
         EJECT ,
******************************************************************
*                                                                *
*        Mapping of job-end line 2.                              *
*                                                                *
******************************************************************
         SPACE 1
         ORG   PRTLINE
JOBLN2   DS    CL(MJ2LEN)          Define space for job line 2
         ORG   JOBLN2+(MJ2CLS-MJOBLN2)
J2CLS    DS    CL(L'MJ2CLS)        Job selection class
         ORG   JOBLN2+(MJ2INTM-MJOBLN2)
J2INTM   DS    CL(L'MJ2INTM)       Time job was initiated
         ORG   JOBLN2+(MJ2INDTJ-MJOBLN2)
J2INDTJ  DS    CL(L'MJ2INDTJ)      Job start date in Julian
         ORG   JOBLN2+(MJ2INDTG-MJOBLN2)
J2INDTG  DS    CL(L'MJ2INDTG)      Job start date in Gregorian
         ORG   JOBLN2+(MJ2INDAY-MJOBLN2)
J2INDAY  DS    CL(L'MJ2INDAY)      Job start date in day of week
         ORG   JOBLN2+(MJ2TMELP-MJOBLN2)
J2TMELP  DS    CL(L'MJ2TMELP)      Total elapsed time of job
         ORG   ,                   Reset location counter
         EJECT ,
******************************************************************
*                                                                *
*        Mapping of job-end line 3.                              *
*                                                                *
******************************************************************
         SPACE 1
         ORG   PRTLINE
JOBLN3   DS    CL(MJ3LEN)          Define space for job line 3
         ORG   JOBLN3+(MJ3KACT-MJOBLN3)
J3KACT   DS    CL(L'MJ3KACT)       ACCOUNT/JOB ID keyword
         ORG   JOBLN3+(MJ3ACCT-MJOBLN3)
J3ACCT   DS    CL(L'MJ3ACCT)       Job account number field 1
         ORG   JOBLN3+(MJ3JBID-MJOBLN3)
J3JBID   DS    CL(L'MJ3JBID)       HASP/ASP job ID
         ORG   JOBLN3+(MJ3ENTM-MJOBLN3)
J3ENTM   DS    CL(L'MJ3ENTM)       Time job ended
         ORG   JOBLN3+(MJ3ENDTJ-MJOBLN3)
J3ENDTJ  DS    CL(L'MJ3ENDTJ)      Job end date in Julian
         ORG   JOBLN3+(MJ3ENDTG-MJOBLN3)
J3ENDTG  DS    CL(L'MJ3ENDTG)      Job end date in Gregorian
         ORG   JOBLN3+(MJ3ENDAY-MJOBLN3)
J3ENDAY  DS    CL(L'MJ3ENDAY)      Job end date in day of week
         ORG   JOBLN3+(MJ3PRTY-MJOBLN3)
J3PRTY   DS    CL(L'MJ3PRTY)       Job priority
         ORG   ,                   Reset location counter
         EJECT ,
         DS    0D                  Force doubleword alignment
LWORKA   EQU   *-WORKA             Symbolic length of work area
         EJECT ,
******************************************************************
*                                                                *
*        Mapping of step-end EXCP counts line.                   *
*                                                                *
******************************************************************
         SPACE 1
SX       DSECT ,                   Mapping of EXCP count column
SXSEG    DS    0CL26               One EXCP count column
SUNIT    DS    CL4                 -- Device number
         DS    CL1                 -- Filler
SCOUNT   DS    CL14                -- Number of EXCPs to device
         DS    CL7                 -- Filler
         EJECT ,
******************************************************************
*                                                                *
*        Ensure that hex translate table doesn't run             *
*        off the end of the module.                              *
*                                                                *
******************************************************************
         SPACE 1
IEFACTRT CSECT ,                   Resume CSECT
         DS    0D                  End of module
         DS    0S((*-HEXTRAN)-256) Force error if HEXTRAN address
*                                  isn't followed by at least
*                                  256 bytes
         END   ,
//*
//LKED   EXEC PGM=HEWLF064,PARM=(XREF,LET,LIST,NCAL,REUS,REFR),
//            REGION=128K,COND=(4,LT,ASM)
//SYSLMOD  DD DSN=SYS1.LINKLIB,DISP=SHR
//SYSPUNCH DD DSN=&&OBJSET,DISP=(OLD,DELETE)
//SYSUT1   DD DSN=&&SYSUT1,UNIT=SYSDA,SPACE=(1024,(50,20))
//SYSPRINT DD SYSOUT=A
//SYSLIN   DD *
 INCLUDE SYSPUNCH
 INCLUDE SYSLMOD(IEFSD061)
 ALIAS IEFSD064
 ALIAS IEFW42SD
 ALIAS IEFSD104
 ENTRY IEFSD061
 NAME IEFSD061(R)
//

//* following link edit step to link IEFACTRT as a separate
//* load module removed 2007/08/06
//LKED   EXEC PGM=IEWL,PARM=(XREF,LET,LIST,NCAL,REUS,REFR),
//            REGION=8192K,COND=(4,LT,ASM)
//SYSLMOD  DD DSN=SYS1.LINKLIB(IEFACTRT),DISP=SHR
//SYSPUNCH DD DSN=&&OBJSET,DISP=(OLD,DELETE)
//SYSUT1   DD DSN=&&SYSUT1,UNIT=SYSDA,SPACE=(1024,(50,20))
//SYSPRINT DD SYSOUT=A
//CI505 DD DISP=SHR,VOLUME=(,RETAIN),DSNAME=SYS1.CI505
//CI535 DD DISP=SHR,VOLUME=(,RETAIN),DSNAME=SYS1.CI535
//SYSLIN   DD DSN=&&OBJSET,DISP=(OLD,DELETE)
//         DD *
 CHANGE IEFSD572(IEFQASGN)
 INCLUDE CI505(IEFQASGQ,IEFQMRAW)
 INCLUDE CI535(IEFSD111)
 INCLUDE CI505(IEFYSVMS)
/*
//
