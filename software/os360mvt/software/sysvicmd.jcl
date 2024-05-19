//SYSVICMD JOB 1,SYSVICMD,MSGLEVEL=(1,1),CLASS=A,MSGCLASS=A
//*********************************************************************
//*                                                                 ***
//*    Job:      SYSVICMD                                           ***
//*    Product:  MVT 21.8                                           ***
//*    Purpose:  Install COMMNDxx processor on new system.          ***
//*    Update:   2006/09/21                                         ***
//*                                                                 ***
//*********************************************************************
//*
/*JOBPARM LINES=9999
//*MAIN LINES=100
//ASM    EXEC PGM=IEUASM,
//            PARM='DECK,LIST',REGION=768K
//SYSLIB   DD DISP=SHR,DSN=SYS1.MACLIB,DCB=BLKSIZE=6160,
//  UNIT=3330,VOL=SER=MVTRES
//         DD DISP=SHR,DSN=SYS1.MODGEN,
//  UNIT=3330,VOL=SER=DLIB01
//SYSUT1   DD DSN=&&SYSUT1,UNIT=SYSDA,SPACE=(1700,(600,100))
//SYSUT2   DD DSN=&&SYSUT2,UNIT=SYSDA,SPACE=(1700,(300,50))
//SYSUT3   DD DSN=&&SYSUT3,UNIT=SYSDA,SPACE=(1700,(300,50))
//SYSPRINT DD SYSOUT=A,DCB=BLKSIZE=1089
//SYSLIN   DD DUMMY
//SYSGO    DD DUMMY
//SYSPUNCH DD DSN=&&OBJSET,UNIT=SYSDA,SPACE=(80,(200,50)),
//            DISP=(MOD,PASS)
//SYSIN    DD *
         TITLE 'SYSIVCMD - Master scheduler initialization COMMNDxx ini+
               tialization routine'
***********************************************************************
*                                                                     *
*                                                                     *
* Module name          =  SYSIVCMD                                    *
*                                                                     *
*                                                                     *
* Descriptive name     =  COMMNDxx PARMLIB processor.                 *
*                                                                     *
*                                                                     *
* Function             =  To read COMMNDxx member of SYS1.PARMLIB,    *
*                         extract and issue commands during           *
*                         master scheduler initialization.            *
*                                                                     *
*                                                                     *
* Notes                =  See below.                                  *
*                                                                     *
*   Dependencies       =  OS/360 system initialization process.       *
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
*   Attributes         =                                              *
*     Location         =  Linklist data set                           *
*     State            =  Supervisor                                  *
*     Key              =  Zero                                        *
*     Mode             =  Task                                        *
*     Serialization    =  None                                        *
*     Type             =  Not reentrant                               *
*                                                                     *
*                                                                     *
***********************************************************************
         EJECT ,
******************************************************************
*                                                                *
*        Begin CSECT and define register equates.                *
*                                                                *
******************************************************************
         SPACE 1
SYSVICMD CSECT ,
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
         EJECT ,
******************************************************************
*                                                                *
*        INIT:                                                   *
*                                                                *
*        Program initialization.                                 *
*                                                                *
*        R7  = Pointer to parm list (set from R1 at entry).      *
*        R12 = Program base register.                            *
*        R13 = Program save area.                                *
*                                                                *
*        Workregs:  R15.                                         *
*                                                                *
******************************************************************
         SPACE 1
**       PRINT NOGEN
         SAVE  (14,12),,           Save                                +
               SYSVICMD-$D00        caller's registers
         LR    R12,R15             Load base register
         USING SYSVICMD,R12        Addressability for program
         LR    R15,R13             Copy caller's save area address
         LA    R13,SAVEA           Point to our save area
         ST    R13,8(,R15)         Chain
         ST    R15,4(,R13)          save areas
         LR    R7,R1               Copy address of parm list
         XC    RETCODE,RETCODE     Initialize return code to zero
         XR    R8,R8               Show no read buffer yet
         EJECT ,
*----------------------------------------------------------------*
*        Get COMMNDxx suffix from parm list.                     *
*----------------------------------------------------------------*
         L     R15,4(,R7)          Point to suffix from parm list
         MVC   COMMEMSF,0(R15)     Copy suffix to member name
         SPACE 1
*----------------------------------------------------------------*
*        Initialize JFCB for SYS1.PARMLIB.                       *
*----------------------------------------------------------------*
         XC    JFCBAREA,JFCBAREA   Zero area for building JFCB
         LA    R15,JFCBAREA        Point to JFCB area
         USING INFMJFCB,R15        Addressablity for JFCB
         MVC   JFCBDSNM,KPARMLIB   Set DSN=SYS1.PARMLIB
         MVC   JFCBELNM,=CL8' '    Show no member name
         OI    JFCBTSDM,JFCNWRIT   Don't update JFCB during open
         MVI   JFCBVLCT,1          Set volume count=1
         OI    JFCBLTYP,JFCNL      Treat as unlabelled
         OI    JFCBIND2,JFCOLD     Set DISP=OLD
         MVI   JFCBNVOL,1          Show one volume
         DROP  R15                 End JFCB addressablity
         SPACE 1
*----------------------------------------------------------------*
*        Get address of master scheduler's TIOT from             *
*        our TCB.                                                *
*----------------------------------------------------------------*
         L     R15,CVTPTR          Get CVT address
         USING CVTMAP,R15          Addressability for CVT
         L     R15,CVTTCBP         Point to TCB words
         DROP  R15                 End CVT addressability
         L     R15,4(,R15)         Get current TCB address
         USING TCB,R15             Addressability for TCB
         L     R15,TCBTIO          TIOT pointer for master TCB
         DROP  R15                 End TCB addressability
         USING VTIOT,R15           Addressability for master TIOT
         SPACE 1
*----------------------------------------------------------------*
*        Save UCB address of SYS1.PROCLIB from the TIOT          *
*        entry, and replace it with the address of the           *
*        UCB of the device containing SYS1.PARMLIB.              *
*----------------------------------------------------------------*
         MVC   9(3,R7),VPUCBPT+1   Save PROCLIB UCB pointer
         MVC   VPUCBPT+1(3),1(R7)  Set PARMLIB UCB pointer in TIOT
         ST    R15,12(,R7)         Save TIOT address
         DROP  R15                 End TIOT addressability
         EJECT ,
******************************************************************
*                                                                *
*        GETPARMN:                                               *
*                                                                *
*        Get PARMLIB volser for non-data cell device.            *
*                                                                *
*        R7  = Pointer to parm list.                             *
*                                                                *
*        Workregs:  R14, R15.                                    *
*                                                                *
******************************************************************
         SPACE 1
*----------------------------------------------------------------*
*        Get the volume serial of the volume containing          *
*        SYS1.PARMLIB.  This is mildly exciting in MVT           *
*        because it might potentially be on a data cell.         *
*----------------------------------------------------------------*
GETPARMN DS    0H                  Get PARMLIB UCB
         L     R15,0(,R7)          Pick up PARMLIB UCB address
         USING UCBOB,R15           Addressability for UCB base
         CLI   UCBID,X'FF'         Is this a normal UCB?
         BNE   GETPARMD            No, it's a bizarre one
         SPACE 1
*----------------------------------------------------------------*
*        The PARMLIB volume is not mounted on a data cell.       *
*        Get the volser from the usual place.                    *
*----------------------------------------------------------------*
         LA    R14,JFCBAREA        Point to JFCB area
         USING INFMJFCB,R14        Addressablity for JFCB
         MVC   JFCBVOLS(6),SRTEVOLI   Get PARMLIB volser
         B     PARMOPEN            Continue
         DROP  R15                 End UCB addressability
         DROP  R14                 End JFCB addressablity
         EJECT ,
******************************************************************
*                                                                *
*        GETPARMD:                                               *
*                                                                *
*        Get PARMLIB volser for data cell device.                *
*                                                                *
*        R7  = Pointer to parm list - not referenced.            *
*                                                                *
*        Workregs:  R14, R15.                                    *
*                                                                *
******************************************************************
         SPACE 1
*----------------------------------------------------------------*
*        The device containing PARMLIB is a data cell.  Get      *
*        its volser before it melts.                             *
*----------------------------------------------------------------*
GETPARMD DS    0H                  Get PARMLIB volser for 2321
         LA    R14,JFCBAREA        Point to JFCB area
         USING INFMJFCB,R14        Addressablity for JFCB
         USING UCBDCELL,R15        Adjust UCB addressability
         MVC   JFCBVOLS(6),UCBDVOLI   Get data cell PARMLIB volser
         DROP  R15                 End data cell UCB addressability
         DROP  R14                 End JFCB addressablity
         EJECT ,
******************************************************************
*                                                                *
*        PARMOPEN:                                               *
*                                                                *
*        Open PARMLIB and ensure that it opened.                 *
*                                                                *
*        R7  = Pointer to parm list - not referenced.            *
*                                                                *
*        Workregs:  R0, R1, R15.                                 *
*                                                                *
******************************************************************
         SPACE 1
PARMOPEN DS    0H                  Open PARMLIB
         OPEN  (PARMLIB,(INPUT)),  Open PARMLIB                        +
               TYPE=J               using our JFCB
         LA    R1,PARMLIB          Point to PARMLIB DCB
         USING IHADCB,R1           Addressability for DCB
         TM    DCBOFLGS,DCBOFOPN   Did it open?
         BZ    OPENERR             Error if not
         MVC   BLKSIZE,DCBBLKSI    Save data set block size
         DROP  R1                  End DCB addressability
         EJECT ,
******************************************************************
*                                                                *
*        PARMFIND:                                               *
*                                                                *
*        Issue FIND to position PARMLIB to the appropriate       *
*        member.  Test FIND return code to ensure it was         *
*        successful, and branch to issue appropriate error       *
*        message if it wasn't.  Otherwise, issue message         *
*        to document that we found it, and get storage           *
*        for READ I/O buffer.                                    *
*                                                                *
*        R7  = Pointer to parm list - not referenced.            *
*        R8  = Address of READ buffer - set in thie routine.     *
*                                                                *
*        Workregs:  R0, R1, R15.                                 *
*                                                                *
******************************************************************
         SPACE 1
PARMFIND DS    0H                  Find PARMLIB member
         FIND  PARMLIB,COMMEM,D    Position to start of member
         C     R15,=F'4'           Was member found?
         BE    NOTFOUND            Error if not
         LTR   R15,R15             Other I/O error in FIND?
         BNZ   FINDERR             Go issue error message if so
         SPACE 1
*----------------------------------------------------------------*
*        The member was found in PARMLIB.  Say so.               *
*----------------------------------------------------------------*
         MVC   ME252MEM,COMMEM     Set member name in message
         WTO   MF=(E,MSGE252)      Say "MEMBER FOUND"
         SPACE 1
*----------------------------------------------------------------*
*        Get work storage as I/O area for reading the            *
*        PARMLIB member.  Save its address in R8.                *
*----------------------------------------------------------------*
         LH    R0,BLKSIZE          Get data set blksize
         GETMAIN R,LV=(0)          Get area for block
         LR    R8,R1               Point to block area
         EJECT ,
******************************************************************
*                                                                *
*        READLOOP:                                               *
*                                                                *
*        Top of read loop.  Loop reading blocks until we         *
*        encounter end of data on the member.                    *
*                                                                *
*        R7  = Pointer to parm list - not referenced.            *
*        R8  = Address of READ buffer.                           *
*                                                                *
*        Workregs:  R0, R1, R14, R15.                            *
*                                                                *
******************************************************************
         SPACE 1
READLOOP DS    0H                  Loop reading blocks
         READ  PARMDECB,SF,        Read block                          +
               PARMLIB,(R8),'S'     into our area
         CHECK PARMDECB            Wait for read to complete
         L     R15,RETCODE         Check for read error
         LTR   R15,R15             Did read encounter error?
         BNZ   IOERR               Branch if error on read
         EJECT ,
******************************************************************
*                                                                *
*        Block was successfully read.  Set up for                *
*        deblocking loop.  No deblocking should actually         *
*        be necessary (MVT requires PARMLIB to be unblocked)     *
*        but we'll be thorough.                                  *
*                                                                *
*        R3  = Pointer to current logical record for BXLE loop.  *
*        R4  = Logical record length for BXLE loop.              *
*        R5  = Pointer to end value for BXLE loop.               *
*        R7  = Pointer to parm list - not referenced.            *
*        R8  = Address of READ buffer.                           *
*                                                                *
******************************************************************
         SPACE 1
         LA    R15,PARMDECB        Point to DECB
         USING DECB,15             Addressability for DECB
         L     R15,DECIOBPT        Point to IOB  DECB+16
         USING IOBSTDRD,R15        Addressabilty for IOB
         LH    R0,IOBCSW+5         Get residual count  IOB+14
         DROP  R15                 End IOB addressability
         LH    R5,BLKSIZE          Get PARMLIB block size
         SR    R5,R0               Calculate length of block read
         LR    R3,R8               Point to first record
         LA    R4,80               Set increment for BXLE
         AR    R5,R3               Set compare value
         SR    R5,R4                for BXLE
         EJECT ,
******************************************************************
*                                                                *
*        BLKLOOP:                                                *
*                                                                *
*        Loop through logical records in block.  Call            *
*        COMRECD subroutine for each to extract and              *
*        issue command contained in record.  Note that           *
*        any record beginning with a "*" is treated as           *
*        comment and ignored.                                    *
*                                                                *
*        R3  = Pointer to current logical record for BXLE loop.  *
*        R4  = Logical record length for BXLE loop.              *
*        R5  = Pointer to end value for BXLE loop.               *
*        R7  = Pointer to parm list - not referenced.            *
*        R8  = Address of READ buffer.                           *
*        R14 = Linkage to COMRECD subroutine.                    *
*                                                                *
*        Workregs:  R14, R15.                                    *
*                                                                *
******************************************************************
         SPACE 1
BLKLOOP  DS    0H                  Loop through block
         CLI   0(R3),C'*'          Is record a comment?
         BE    BLKBOT              Skip call to subroutine if so
         LR    R1,R3               Point to record
         BAL   R14,COMRECD         Call parse subroutine
         EJECT ,
******************************************************************
*                                                                *
*        BLKLOOP:                                                *
*                                                                *
*        Bottom of deblocking loop.  Adjust record pointer       *
*        to the next logical record and loop back to             *
*        process it.  We should never actually take the          *
*        branch because MVT's unblocked PARMLIB should only      *
*        have one logical record in each block.                  *
*                                                                *
*        R3  = Pointer to current logical record for BXLE loop.  *
*        R4  = Logical record length for BXLE loop.              *
*        R5  = Pointer to end value for BXLE loop.               *
*        R7  = Pointer to parm list - not referenced.            *
*        R8  = Address of READ buffer - not referenced.          *
*                                                                *
*        Workregs:  None.                                        *
*                                                                *
******************************************************************
         SPACE 1
BLKBOT   DS    0H                  Bottom of deblocking loop
         BXLE  R3,R4,BLKLOOP       Back to do next record
         EJECT ,
******************************************************************
*                                                                *
*        BLKNEXT:                                                *
*                                                                *
*        All logical records in this block have been             *
*        processed.  Go read another block.                      *
*                                                                *
*        R3  = Pointer to current logical record for BXLE loop.  *
*        R4  = Logical record length for BXLE loop.              *
*        R5  = Pointer to end value for BXLE loop.               *
*        R7  = Pointer to parm list - not referenced.            *
*        R8  = Address of READ buffer - not referenced.          *
*                                                                *
*        Workregs:  None.                                        *
*                                                                *
******************************************************************
         SPACE 1
BLKNEXT  DS    0H                  Done with block, read next one
         B     READLOOP            Back to read next block
         EJECT ,
******************************************************************
*                                                                *
*        EOFPARM:                                                *
*                                                                *
*        End of file has been taken on the PARMLIB member.       *
*        Free work storage we obtained to read blocks into.      *
*                                                                *
*        R7  = Pointer to parm list - not referenced.            *
*        R8  = Address of READ buffer - freed in this routine.   *
*                                                                *
*        Workregs:  R0, R1, R14, R15.                            *
*                                                                *
******************************************************************
         SPACE 1
EOFPARM  DS    0H                  Here when EOF on PARMLIB
         LTR   R8,R8               Do we have a buffer to free?
         BZ    PARMCLS             Skip FREEMAIN if not
         LH    R0,BLKSIZE          Set length for FREEMAIN
         LR    R1,R8               Set area for FREEMAIN
         FREEMAIN R,LV=(0),A=(1)   Free I/O area
         EJECT ,
******************************************************************
*                                                                *
*        PARMCLS:                                                *
*                                                                *
*        Close PARMLIB.                                          *
*                                                                *
*        R7  = Pointer to parm list - not referenced.            *
*                                                                *
*        Workregs:  R0, R1, R14, R15.                            *
*                                                                *
******************************************************************
         SPACE 1
PARMCLS  DS    0H                  Here to close PARMLIB DCB
         CLOSE (PARMLIB,)          Close PARMLIB
         EJECT ,
******************************************************************
*                                                                *
*        RETURN:                                                 *
*                                                                *
*        Clean up and return to system.                          *
*                                                                *
*        R7  = Pointer to parm list.                             *
*                                                                *
*        Workregs:  R0, R1, R14, R15.                            *
*                                                                *
******************************************************************
         SPACE 1
RETURN   DS    0H                  Return to system
         SPACE 1
*----------------------------------------------------------------*
*        Unsmash UCB pointer in PROCLIB TIOT entry.              *
*----------------------------------------------------------------*
         L     R15,12(,R7)         Get address of master's TIOT
         USING VTIOT,R15           Addressability for TIOT
         MVC   VPUCBPT+1(3),9(R7)  Restore PROCLIB UCB pointer
         DROP  R15                 End TIOT addressability
         SPACE 1
*----------------------------------------------------------------*
*        Restore registers and return.                           *
*----------------------------------------------------------------*
         L     R13,4(,R13)         Restore caller's save area ptr
         L     R15,RETCODE         Set return code in R15
         RETURN (14,12),RC=(15)    Return to caller
         EJECT ,
******************************************************************
*                                                                *
*        OPENERR:                                                *
*                                                                *
*        PARMLIB open failed.  Issue "OPEN PROCESSING            *
*        FAILED" message and set return code = 8 to indicate     *
*        error, then go to exit.                                 *
*                                                                *
******************************************************************
         SPACE 1
OPENERR  DS    0H                  PARMLIB open failed
         MVC   ME591MEM,COMMEM     Set member name in message
         WTO   MF=(E,MSGE591)      Say "OPEN FAILED"
         MVC   RETCODE,=F'8'       Set error return code
         B     RETURN              Go return to IEEVIPL
         EJECT ,
******************************************************************
*                                                                *
*        NOTFOUND:                                               *
*                                                                *
*        Member not found in PARMLIB.  Issue "MEMBER NOT         *
*        FOUND" message and set return code = 8 to indicate      *
*        error, then go to close PARMLIB.                        *
*                                                                *
******************************************************************
         SPACE 1
NOTFOUND DS    0H                  Member not found
         MVC   ME538MEM,COMMEM     Set member name in message
         WTO   MF=(E,MSGE538)      Say "MEMBER NOT FOUND"
         MVC   RETCODE,=F'8'       Set error return code
         B     EOFPARM             Go close PARMLIB
         EJECT ,
******************************************************************
*                                                                *
*        FINDERR:                                                *
*                                                                *
*        I/O error occurred during FIND.  Issue "I/O ERROR"      *
*        message and set return code = 8 to indicate error,      *
*        then go to close PARMLIB.                               *
*                                                                *
******************************************************************
         SPACE 1
FINDERR  DS    0H                  Error during FIND
         MVC   ME539MEM,COMMEM     Set member name in message
         WTO   MF=(E,MSGE539)      Say "I/O ERROR"
         MVC   RETCODE,=F'8'       Set error return code
         B     PARMCLS             Go close PARMLIB
         EJECT ,
******************************************************************
*                                                                *
*        IOERR:                                                  *
*                                                                *
*        I/O error occurred during PARMLIB read.  Issue          *
*        "I/O ERROR" message and go to close PARMLIB.            *
*                                                                *
******************************************************************
         SPACE 1
IOERR    DS    0H                  I/O error on PARMLIB
         MVC   ME539MEM,COMMEM     Set member name in message
         WTO   MF=(E,MSGE539)      Say "I/O ERROR"
*        MVC   RETCODE,=F'8'       Set error return code
         B     EOFPARM             Go close PARMLIB
         EJECT ,
******************************************************************
*                                                                *
*        SYNADRTN:                                               *
*                                                                *
*        I/O error occurred on PARMLIB.  Set return              *
*        code = 8 to indicate error                              *
*                                                                *
******************************************************************
         SPACE 1
SYNADRTN DS    0H                  PARMLIB SYNAD routine
         MVC   RETCODE,=F'8'       Set error return code
         BR    R14                 Return to BPAM
         EJECT ,
******************************************************************
*                                                                *
*        Constants, work areas and literals.                     *
*                                                                *
******************************************************************
         SPACE 1
*----------------------------------------------------------------*
*        Program save area.                                      *
*----------------------------------------------------------------*
SAVEA    DS    18F                 Program save area
         SPACE 1
RETCODE  DS    F                   Program return code
BLKSIZE  DS    H                   PARMLIB block size
         SPACE 1
*----------------------------------------------------------------*
*        COMMNDxx member name.                                   *
*----------------------------------------------------------------*
COMMEM   DS    0CL8                Member name
         DC    CL6'COMMND'         -- Stem
COMMEMSF DC    CL2' '              -- Suffix
         SPACE 1
*----------------------------------------------------------------*
*        PARMLIB data set name.                                  *
*----------------------------------------------------------------*
KPARMLIB DC    CL44'SYS1.PARMLIB'  PARMLIB data set name
         SPACE 1
*----------------------------------------------------------------*
*        Flags.                                                  *
*----------------------------------------------------------------*
FLAG1    DC    XL1'00'             Flag byte
F1BADCMD EQU   X'40'               -- "BAD COMMAND" message issued
         SPACE 1
QUOTE    EQU   C''''               Compare value for single quote
         SPACE 1
*----------------------------------------------------------------*
*        MGCR (SVC 34) PARAMETER LIST.                           *
*----------------------------------------------------------------*
MGCRPL   DS    0H                  SVC 34 parameter list
MGCRLEN  DS    H                   Length of text + prefix
         DC    H'0'                Reserved
MGCRTEXT DC    CL70' '             Command text
         SPACE 1
*----------------------------------------------------------------*
*        PARMLIB DCB exit list for OPEN TYPE=J.                  *
*----------------------------------------------------------------*
JFCBXLST DS    0A                  JFCB exit list
         DC    X'87'               -- This is a JFCB exit
         DC    AL3(JFCBAREA)       -- Pointer to area for JFCB
         SPACE 1
*----------------------------------------------------------------*
*        Area building SYS1.PARMLIB JFCB.                        *
*----------------------------------------------------------------*
         DS    0F                  Force fullword alignment
JFCBAREA DS    CL176               Area for JFCB
         EJECT ,
*----------------------------------------------------------------*
*        BPAM DCB for reading PARMLIB.                           *
*----------------------------------------------------------------*
PARMLIB  DCB   DSORG=PO,RECFM=FB,LRECL=80,MACRF=(R),DDNAME=IEFPDSI,    +
               EODAD=EOFPARM,EXLST=JFCBXLST,SYNAD=SYNADRTN
         EJECT ,
******************************************************************
*                                                                *
*        Constants, work areas and literals (continued).         *
*                                                                *
******************************************************************
         SPACE 1
*----------------------------------------------------------------*
*        WPL for message IEE252I member FOUND.                   *
*----------------------------------------------------------------*
MSGE252  DS    0F                  IEE252I message
         DC    AL2(LMSGE252)       Message length
         DC    XL2'8000'           Routing/descriptor codes exist
         DC    C'IEE252I MEMBER '  Message ID
ME252MEM DC    CL8' '              Member name
         DC    C' FOUND IN'        Text
         DC    C' SYS1.PARMLIB'    Text
LMSGE252 EQU   *-MSGE252           Symbolic length of WPL
         DC    X'1000'             Descriptor codes
         DC    B'0100000000000000' Routing codes
         SPACE 1
*----------------------------------------------------------------*
*        WPL for message IEE539I I/O ERROR.                      *
*----------------------------------------------------------------*
MSGE539  DS    0F                  I/O error message
         DC    AL2(LMSGE539)       Message length
         DC    XL2'8000'           Routing/descriptor codes exist
         DC    C'IEE539I I/O ERROR DURING PROCESSING OF MEMBER '
ME539MEM DC    CL8' '              Member name
         DC    C' OF'              Text
         DC    C' SYS1.PARMLIB'    Text
LMSGE539 EQU   *-MSGE539           Symbolic length of WPL
         DC    X'1000'             Descriptor codes
         DC    B'0100000000000000' Routing codes
         SPACE 1
*----------------------------------------------------------------*
*        WPL for message IEE538I member NOT FOUND.               *
*----------------------------------------------------------------*
MSGE538  DS    0F                  IEE538I message
         DC    AL2(LMSGE538)       Message length
         DC    XL2'8000'           Routing/descriptor codes exist
         DC    C'IEE538I '         Message ID
ME538MEM DC    CL8' '              Member name
         DC    C' MEMBER NOT FOUND' Text
         DC    C' IN SYS1.PARMLIB' Text
LMSGE538 EQU   *-MSGE538           Symbolic length of WPL
         DC    X'1000'             Descriptor codes
         DC    B'0100000000000000' Routing codes
         SPACE 1
*----------------------------------------------------------------*
*        WPL for message IEE591I OPEN PROCESSING FAILED.         *
*----------------------------------------------------------------*
MSGE591  DS    0F                  IEE591I message
         DC    AL2(LMSGE591)       Message length
         DC    XL2'8000'           Routing/descriptor codes exist
         DC    C'IEE591I'          Message ID
         DC    C' OPEN PROCESSING' Text
         DC    C' FAILED FOR'      Text
         DC    C' MEMBER '         Text
ME591MEM DC    CL8' '              Member name
         DC    C' IN SYS1.PARMLIB' Text
LMSGE591 EQU   *-MSGE591           Symbolic length of WPL
         DC    X'1000'             Descriptor codes
         DC    B'0100000000000000' Routing codes
         SPACE 1
*----------------------------------------------------------------*
*        WPL for message IEA863I xxxx SPECIFICATION INVALID      *
*        IN member.                                              *
*----------------------------------------------------------------*
MSGA863  DS    0F                  Message for command invalid
         DC    AL2(LMSGA863)       Message length
         DC    XL2'8000'           Routing/descriptor codes exist
         DC    C'IEA863I <'        Message ID
MA863TXT DC    CL16' '             First 16 bytes of text
         DC    C'> SPECIFICATION INVALID IN '
MA863MEM DC    CL8' '              Member name
LMSGA863 EQU   *-MSGA863           Symbolic length of WPL
         DC    X'1000'             Descriptor codes
         DC    B'0100000000000000' Routing codes
         EJECT ,
*----------------------------------------------------------------*
*        Literal pool.                                           *
*----------------------------------------------------------------*
         LTORG ,
         EJECT ,
******************************************************************
*                                                                *
*        COMRECD:                                                *
*/*                                                              *
*/*      THE FUNCTION OF THE COMRECD RTN IS TO PROCESS THE       *
*/*      PARMLIB MEMBER SPECIFIED BY THE OPERATOR, OR THE        *
*/*      DEFAULT MEMBER, COMMND00.                               *
*/*      THE COMMAND IS SYNTAX CHECKED ONLY TO VERIFY            *
*/*      THAT IT IS ENCLOSED IN QUOTES AND THAT NO NON-          *
*/*      BLANKS ARE OUTSIDE THE QUOTES. THE COMMAND MAY          *
*/*      CONTAIN EMBEDDED QUOTES, AS LONG AS THE FINAL           *
*/*      QUOTE IS VALID.                                         *
*                                                                *
******************************************************************
         SPACE 1
*----------------------------------------------------------------*
*        Initialize for command record scan.  Verify that        *
*        the record starts with "COM='" and exit quickly if      *
*        it doesn't.                                             *
*----------------------------------------------------------------*
COMRECD  DS    0H                  Process "COM=" record
         SAVE  (14,12)             Save caller's registers
         LR    R5,R1               Save pointer to record
         CLC   =C'COM=''',0(R5)    Check for "COM='" at start
         BNE   COMMERR             Error if not present
         SPACE 1
*----------------------------------------------------------------*
*        Start at column 71 of command record and scan           *
*        backward to find first non-blank character.             *
*----------------------------------------------------------------*
         LA    R4,71-1             Initialize scan count
COMMLOOP DS    0H                  Loop scanning backward
         LA    R6,0(R4,R5)         Point to
         BCTR  R6,0                 current buffer position
         CH    R4,=H'6'            Still characters to scan?
         BL    COMMDONE            Done if not
         CLI   0(R6),C' '          Is this character a blank?
         BNE   COMMDONE            Done if not
         BCT   R4,COMMLOOP         Decrement scan count and loop back
* SHOULD NEVER FALL THROUGH HERE
         SPACE 1
*----------------------------------------------------------------*
*        Scan is done.  Check if error was encountered.  The     *
*        rightmost character in the command record (the one      *
*        that should have stopped the scan) must be a quote,     *
*        and there must be at least one byte of command text     *
*        (COM='' is not permitted).                              *
*----------------------------------------------------------------*
COMMDONE DS    0H                  Command record scan done
         CH    R4,=H'6'            Is command text empty?
         BNH   COMMERR             Error if so
         CLI   0(R6),QUOTE         Is rightmost non-blank a quote?
         BE    COMMISSU            Scan successfuly complete if so
         SPACE 1
*----------------------------------------------------------------*
*        Errors have been found in the command record.           *
*        Issue a message and skip this command.                  *
*----------------------------------------------------------------*
COMMERR  DS    0H                  Errors in command record
         MVC   MA863TXT,0(R5)      Put some command text in message
         MVC   MA863MEM,COMMEM     Set member name in IEA863I msg
         WTO   MF=(E,MSGA863)      Say "COMMAND INVALID"
         OI    FLAG1,F1BADCMD      Show an error was found
         B     COMMRET             Skip executing command
         SPACE 1
*----------------------------------------------------------------*
*        We have a command.  Calculate its length and            *
*        set length in SVC 34 parameter list.                    *
*----------------------------------------------------------------*
COMMISSU DS    0H                  Issue the command
         LR    R6,R4
         SH    R6,=H'5'
         AH    R6,=H'4'            Get command length + 4
         STH   R6,MGCRLEN          Save in MGCR parameter list
         SPACE 1
*----------------------------------------------------------------*
*        Set trailing blank after command text to keep           *
*        SVC 34 happy.                                           *
*----------------------------------------------------------------*
         LR    R6,R5                                               0437
         ALR   R6,R4                                               0437
         BCTR  R6,0
         MVI   0(R6),C' '          Set final blank for SVC 34
         SPACE 1
*----------------------------------------------------------------*
*        Copy command text to SVC 34 parameter list.             *
*----------------------------------------------------------------*
         LR    R15,R4              Calculate length
         SH    R15,=H'6'            of command text
         EX    R15,MOVECMD           and move to parm list
         SPACE 1
*----------------------------------------------------------------*
*        Issue the command.                                      *
*----------------------------------------------------------------*
         XR    R0,R0               Show internal command
         LA    R1,MGCRPL           Point to SVC 34 parm list
         MGCR  (1)                 Issue command
         SPACE 1
*----------------------------------------------------------------*
*        Return to caller of COMRECD subroutine.                 *
*----------------------------------------------------------------*
COMMRET  DS    0H                  Return from COMRECD subroutine
         RETURN (14,12)            Return to caller
         SPACE 1
MOVECMD  MVC   MGCRTEXT(*-*),5(R5) ** Executed move
         EJECT ,
******************************************************************
*                                                                *
*        OS Data Control Block (DCB).                            *
*                                                                *
******************************************************************
         SPACE 1
         DCBD  DSORG=PO            Map DCB
         EJECT ,
******************************************************************
*                                                                *
*        Data Event Control Block (DECB).                        *
*                                                                *
******************************************************************
         SPACE 1
*        IHADECB ,                 MAP DECB
DECB     DSECT ,
         DS    F                   ECB
         DS    H                   TYPE
         DS    A                   DCB ADDRESS
         DS    A                   AREA ADDRESS
DECIOBPT DS    A                   ADDRESS OF IOB
         EJECT ,
******************************************************************
*                                                                *
*        Input/Output Block (IOB).                               *
*                                                                *
******************************************************************
         SPACE 1
*        IEZIOB ,                  MAP IOB
IOB      DSECT                                                          00500000
*                                                                       00592020
*    RELEASE 20.2, 3/29/71, LEVEL=1                                     00594020
*    RELEASE 21, 3/30/71, LEVEL=1                                       00594421
*                                                                       00596020
*********************************************************************** 00600000
*                                                                     * 00700000
*                    PREFIX SECTIONS OF THE IOB                       * 00800000
*                                                                     * 00900000
*********************************************************************** 01000000
         SPACE 3                                                        01100000
IOBPREFX DS    0D                                                       01200000
         SPACE 1                                                        01300000
*                            ****************************************** 01400000
*                            *                                        * 01500000
IOBQSAMC DS    0D *          *        QSAM,BSAM,BPAM PREFIX           * 01600000
IOBBSAMC DS    0D *          *     CHAINED SCHEDULING ** 16 BYTES     * 01700000
IOBBPAMC DS    0D *          *                                        * 01800000
*                            ****************************************** 01900000
         SPACE 2                                                        02000000
IOBCFLG1 DS    B                                                        02200000
*                                                                       02300000
IOBPTST  EQU   X'08' --- NOTE OR POINT OPERATION IS IN PROCESS          02350021
IOBABAPP EQU   X'04' --- ERROR PROCESSED ONCE BY ABMORMAL-END APPENDAGE 02400000
IOBRSTCH EQU   X'02' --- RESTART CHANNEL                                02500000
IOBPCI   EQU   X'01' --- PCI INTERRUPT HAS OCCURRED                     02600000
         DS    XL1 ----- RESERVED                                       02700000
         SPACE 1                                                        02800000
IOBCINOP DS    CL1 ----- OFFSET OF LAST I/O COMMAND FOR  INPUT          02900000
*                        OPERATION(NOP CCW) FROM THE ICB ORIGIN         03000000
IOBCONOP DS    CL1 ----- OFFSET OF LAST I/O COMMAND FOR  OUTPUT         03100000
*                        OPERATION(NOP CCW) FROM THE ICB ORIGIN         03200000
IOBCECB  DS    F ------- EVENT CONTROL BLOCK                            03300000
IOBCICB  DS    A ------- ADDR.OF FIRST ICB ON QUEUE                     03400000
IOBCNOPA DS    A ------- ADDR.OF NOP COMMAND AT END OF QUEUE            03500000
         SPACE 2                                                        03600000
         ORG   IOBPREFX+8                                               03700000
         SPACE 2                                                        03800000
*                            ****************************************** 03900000
*                            *                                        * 04000000
IOBQSAMN DS    0D *          *        QSAM,BSAM,BPAM PREFIX           * 04100000
IOBBSAMN DS    0D *          *    NORMAL SCHEDULING *** 8 BYTES       * 04200000
IOBBPAMN DS    0D *          *                                        * 04300000
*                            ****************************************** 04400000
         SPACE 1                                                        04500000
*                                                                       04600000
IOBNIOBA DS    0A ------ ADDR.NEXT IOB ON CHAIN                         04700000
IOBNFLG1 DS    B                                                        04800000
*                                                                       04900000
IOBPRTOV EQU   X'80' --- 'PRTOV' HAS OCCURRED                           05000000
IOBWRITE EQU   X'40' --- 'WRITE' OPERATION IN PROCESS                   05100000
IOBREAD  EQU   X'20' --- 'READ' OPERATION IN PROCESS                    05200000
IOBUPDAT EQU   X'10' --- BLOCK IS TO BE UPDATED                         05300000
IOBBKSPC EQU   X'08' --- IOB BEING USED FOR BACKSPACE,CONTROL,NOTE/PT.  05400000
IOBSPAN  EQU   X'04' --- THIS RECORD IS A SPANNED RECORD                05500000
IOBFIRST EQU   X'01' --- THIS IS FIRST IOB ON CHAIN                     05600000
*                                                                       05700000
IOBNIOBB DS    AL3 ----- ADDR.NEXT IOB ON CHAIN                         05800000
         SPACE 1                                                        05900000
IOBNECB  DS    F ------- EVENT CONTROL BLOCK                            06000000
         SPACE 2                                                        06100000
         ORG   IOBPREFX+8                                               06200000
         SPACE 2                                                        06300000
*                            ****************************************** 06500000
*                            *                                        * 06600000
IOBBDAM  DS    0D *          *       BDAM PREFIX *** 8 BYTES          * 06700000
*                            *                                        * 06800000
*                            ****************************************** 06900000
         SPACE 1                                                        07000000
*                                                                       07100000
IOBDQADA DS    0A ------ ADDR.OF IOB WAITING TO DEQUEUE TRACKS          07200000
*                        OCCUPIED BY SPANNED RECORDS                    07300000
IOBDEQIN DS    B ------- DEQUEUE LOOP INDICATOR                         07400000
IOBDEQ   EQU   X'80' --- TASK WITH SPANNED RECORD BEING DEQUEUED        07500000
IOBDQADB DS    AL3 ----- ADDR.OF IOB WAITING TO DEQUEUE TRACKS          07600000
*                        OCCUPIED BY SPANNED RECORDS                    07700000
IOBSWAP  DS    A ------- ADDR.OF SPANNED WORK AREA                      07800000
         SPACE 2                                                        07900000
         ORG   IOBPREFX+12                                              08100000
         SPACE 2                                                        08200000
*                            ****************************************** 08400000
*                            *                                        * 08500000
IOBGAM   EQU   * *           *                                        * 08600000
IOBQISAM EQU   * *           *       GAM,QISAM PREFIX *** 4 BYTES     * 08700000
*                            ****************************************** 08800000
*                                                                       08900000
*                                                                       09000000
IOBGQECB DS    F --- EVENT CONTROL BLOCK                                09100000
         EJECT                                                          09200000
*********************************************************************** 09300000
*                                                                     * 09400000
*                    STANDARD SECTION OF THE IOB                      * 09500000
*                                                                     * 09600000
*********************************************************************** 09700000
         SPACE 3                                                        09800000
IOBSTDRD DS    0D                                                       09900000
         SPACE 1                                                        10000000
IOBFLAG1 DS    B                                                        10100000
*                                                                       10200000
IOBDATCH EQU   X'80' --- DATA CHAINING USED IN CHANNEL PROGRAM          10300000
IOBCMDCH EQU   X'40' --- COMMAND CHAINING USED IN CHANNEL PROGRAM       10400000
IOBERRTN EQU   X'20' --- ERROR ROUTINE IS IN CONTROL                    10500000
IOBRPSTN EQU   X'10' --- DEVICE IS TO BE REPOSITIONED                   10600000
IOBCYCCK EQU   X'08' --- CYCLIC REDUNDANCY CHECK NEEDED(TAPE ONLY)      10700000
IOBFCREX EQU   X'08' --- FETCH COMMAND RETRY EXIT (DIRECT ACCESS ONLY)  10750020
IOBIOERR EQU   X'04' --- I/O ERROR HAS OCCURRED                         10800000
IOBUNREL EQU   X'02' --- THIS I/O REQUEST IS UNRELATED(NON-SEQUENTIAL)  10900000
IOBRSTRT EQU   X'01' --- RESTART ADDR.IN IOB TO BE USED                 11000000
         SPACE 1                                                        11100000
IOBFLAG2 DS    B                                                        11200000
*                                                                       11300000
IOBHALT  EQU   X'80' --- HALT I/O HAS BEEN ISSUED BY SVC PURGE ROUTINE  11400000
IOBSENSE EQU   X'40' --- ISSUE SENSE COMMAND AFTER DEVICE END OCCURS    11500000
IOBPURGE EQU   X'20' --- IOB HAS BEEN PURGED *ALLOW I/O TO QUIESCE      11600000
IOBRDHA0 EQU   X'10' --- HOME ADDRESS TO BE READ * NO SEEK NEEDED       11700000
IOBALTTR EQU   X'08' --- NO TEST FOR OUT-OF-EXTENT * AN ALTERNATE TRACK 11800000
*                        IS IN USE                                      11900000
IOBSKUPD EQU   X'04' --- SEEK ADDRESS IS BEING UPDATED -CYLINDER END    12000000
*                        OR FILE MASK VIOLATION HAS OCCURRED            12100000
IOBSTATO EQU   X'02' --- DEVICE END STATUS HAS BEEN ORED WITH           12200000
*                        CHANNEL END STATUS - GRAPHICS DEVICE           12300000
IOBPNCH  EQU   X'01' --- TURNED ON BY QSAM WHEN ERROR RECOVERY IS TO    12400000
*                        BE PROVIDED FOR THE 2540 CARD PUNCH            12500000
         SPACE 1                                                        12600000
IOBSENS0 DS    B ------- FIRST SENSE BYTE                               12700000
IOBS0B0  EQU   X'80' --- BIT 0 (DEVICE DEPENDENT)                       12750020
IOBS0B1  EQU   X'40' --- BIT 1 (DEVICE DEPENDENT)                       12760020
IOBS0B2  EQU   X'20' --- BIT 2 (DEVICE DEPENDENT)                       12770020
IOBS0B3  EQU   X'10' --- BIT 3 (DEVICE DEPENDENT)                       12780020
IOBS0B4  EQU   X'08' --- BIT 4 (DEVICE DEPENDENT)                       12790020
IOBS0B5  EQU   X'04' --- BIT 5 (DEVICE DEPENDENT)                       12792020
IOBS0B6  EQU   X'02' --- BIT 6 (DEVICE DEPENDENT)                       12794020
IOBS0B7  EQU   X'01' --- BIT 7 (DEVICE DEPENDENT)                       12796020
IOBSENS1 DS    B ------- SECOND SENSE BYTE                              12800000
IOBS1B0  EQU   X'80' --- BIT 0 (DEVICE DEPENDENT)                       12850020
IOBS1B1  EQU   X'40' --- BIT 1 (DEVICE DEPENDENT)                       12860020
IOBS1B2  EQU   X'20' --- BIT 2 (DEVICE DEPENDENT)                       12870020
IOBS1B3  EQU   X'10' --- BIT 3 (DEVICE DEPENDENT)                       12880020
IOBS1B4  EQU   X'08' --- BIT 4 (DEVICE DEPENDENT)                       12890020
IOBS1B5  EQU   X'04' --- BIT 5 (DEVICE DEPENDENT)                       12892020
IOBS1B6  EQU   X'02' --- BIT 6 (DEVICE DEPENDENT)                       12894020
IOBS1B7  EQU   X'01' --- BIT 7 (DEVICE DEPENDENT)                       12896020
*                                                                       12900000
IOBECBPT DS    0A ------ ADDRESS OF ECB TO BE POSTED ON I/O COMPLETION  13000000
IOBECBCC DS    CL1 ---- COMPLETION CODE FOR THIS I/O REQUEST            13100000
IOBECBPB DS    AL3 ----- ADDRESS OF ECB TO BE POSTED ON I/O COMPLETION  13200000
*                                                                       13300000
IOBFLAG3 DS    B ------- ERROR ROUTINE FLAG BYTE                        13400000
IOBCSW   DS    CL7 ----- LOW ORDER 7 BYTES OF CSW AT CHANNEL END        13500000
*                                                                       13600000
IOBSTART DS    0A ------ ADDRESS OF CHANNEL PROGRAM                     13700000
IOBSIOCC DS    B ------- BITS 2 AND 3 = C.C. FROM SIO                   13800000
IOBSTRTB DS    AL3 ----- ADDRESS OF CHANNEL PROGRAM                     13900000
*                                                                       14000000
IOBDCBPT DS    0A ------ ADDRESS OF DATA CONTROL BLOCK FOR THIS IOB     14100000
         DS    XL1 ----- RESERVED                                       14200000
IOBDCBPB DS    AL3 ----- ADDRESS OF DATA CONTROL BLOCK FOR THIS IOB     14300000
*                                                                       14400000
IOBRESTR DS    0A ------ RESTART ADDRESS FOR ERROR RETRY                14500000
IOBREPOS DS    CL1 ----- CODE USED TO REPOSITION DEVICE                 14600000
IOBRSTRB DS    AL3 ----- RESTART ADDRESS FOR ERROR RETRY                14700000
*                                                                       14800000
IOBINCAM DS    0H ------ VALUE USED TO INCREMENT BLOCK COUNT ON TAPE    14900000
IOBBTAMF DS    B ------- FLAG BYTE FOR BTAM                             15000000
IOBPRMER EQU   X'80' --- 'SAD','ENABLE' ISSUED BY OPEN CAUSED I/O ERROR 15100000
IOBINUSE EQU   X'40' --- IOB IS IN USE                                  15200000
IOBOLTST EQU   X'01' --- LINE IS UNDER ON-LINE TEST OPERATION           15300000
         SPACE 1                                                        15400000
         DS    XL1 ----- RESERVED                                       15500000
         ORG   IOBINCAM                                                 15600000
IOBCRDCC DS    CL1 ----- OPTICAL READER: DATA CHECK ERROR COUNT         15700000
IOBCRILC DS    CL1 ----- OPTICAL READER: INCORRECT LENGTH ERROR COUNT   15800000
         SPACE 2                                                        15900000
IOBERRCT DS    H ------- NUMBER OF ERROR RETRIES                        16000000
         EJECT                                                          16100000
*********************************************************************** 16200000
*                                                                     * 16300000
*                   EXTENSION SECTIONS OF THE IOB                     * 16400000
*                                                                     * 16500000
*********************************************************************** 16600000
         SPACE 1                                                        16700000
IOBEXTEN DS    0D                                                       16800000
         SPACE 1                                                        16900000
*                            ****************************************** 17000000
*                            *                                        * 17100000
*                            *  DIRECT ACCESS EXTENSION *** 8 BYTES   * 17200000
*                            *                                        * 17300000
*                            ****************************************** 17400000
         SPACE 1                                                        17500000
IOBSEEK  DS    0CL8 ----                                                17600000
*                                                                       17700000
IOBM     DS    CL1 ----- RELATIVE EXTENT NUMBER FOR THIS REQUEST(0-15)  17800000
*                                                                       17900000
IOBBB    DS    0CL2 ---- BIN NUMBER(DATA CELL)                          18000000
IOBBB1   DS    CL1                                                      18100000
IOBBB2   DS    CL1                                                      18200000
*                                                                       18300000
IOBCC    DS    0CL2 ---- CYLINDER NUMBER                                18400000
IOBCC1   DS    CL1                                                      18500000
IOBCC2   DS    CL1                                                      18600000
*                                                                       18700000
IOBHH    DS    0CL2 ---- TRACK NUMBER                                   18800000
IOBHH1   DS    CL1                                                      18900000
IOBHH2   DS    CL1                                                      19000000
IOBR     DS    CL1 ----- RECORD NUMBER                                  19100000
         SPACE 1                                                        19200000
*                            ****************************************** 19300000
*                            *                                        * 19400000
         ORG   IOBEXTEN *    * BTAM EXTENSION ** 40 BYTES + CHNL.PGM  * 19500000
*                            *                                        * 19600000
*                            ****************************************** 19700000
         SPACE 1                                                        19800000
IOBUCBX  DS    CL1 ----- LINE NUMBER IS USED TO LOCATE THE              19900000
*                        PROPER UCB ADDRESS IN THE DEB.                 20000000
IOBWORK  DS    CL5 ----- WORK AREA USED BY ERROR ROUTINES AND           20100000
*                        ON-LINE TERMINAL ROUTINES                      20200000
IOBRCVPT DS    CL1 ----- RECEIVED ACK (ACK-0 OR ACK-1)                  20250000
IOBSNDPT DS    CL1 ----- SENT ACK (ACK-0 OR ACK-1)                      20260000
IOBERCCW DS    CL8 ----- CCW AREA USED BY BTAM ERROR ROUTINES           20300000
IOBERINF DS    CL16 ---- ERROR INFORMATION FIELD USED BY BTAM ERROR RTN 20400000
IOBCPA   DS    0D ------ AREA FOR CHANNEL PROGRAMS.LENGTH VARIES        20500000
*                        ACCORDING TO TERMINAL AND OPTIONS              20600000
         EJECT                                                          20700000
*                            ****************************************** 20800000
*                            *                                        * 20900000
         ORG   IOBSEEK+8 *   *        BISAM EXTENSION *** 12 BYTES    * 21000000
*                            *                                        * 21100000
*                            ****************************************** 21200000
         SPACE 1                                                        21300000
IOBCCWAD DS    A ------- FOR FIXED LENGTH RCRDS:PTR.TO FIRST CCW        21400000
*                        FOR VARIABLE RECORDS:PTR.TO BUFFER(DYNAMIC     21500000
*                        BUFFERING SPECIFIED)AFTER COMPLETION OF        21600000
*                        READ FOR UPDATE(READ KU)                       21700000
         SPACE 1                                                        21800000
IOBINDCT DS    B                                                        21900000
*                                                                       22000000
IOBDEQCP EQU   X'80' --- DEQUEUE CHANNEL PROGRAM FROM QUEUE             22100000
IOBUNSCH EQU   X'40' --- UNSCHEDULER QUEUE                              22200000
IOBOVPTR EQU   X'20' --- PTR.TO OVERFLOW RECORD INDICATOR:              22300000
*                        BIT = 0 MEANS 'DECBAREA' + 6 POINTS TO         22400000
*                         OVERFLOW RECORD DATA                          22500000
*                        BIT = 1 MEANS 'DCBMSWA' POINTS TO OVERFLOW     22600000
*                         RECORD KEY FOLLOWED BY DATA                   22700000
IOBKEYAD EQU   X'10' --- PTR.TO OVERFLOW RECORD KEY INDICATOR:          22800000
*                        BIT = 0 MEANS 'DECBKEY' POINTS TO OVERFLOW     22900000
*                         RECORD KEY                                    23000000
*                        BIT = 1 MEANS 'DCBMSWA' + 8 POINTS TO          23100000
*                         OVERFLOW RECORD KEY                           23200000
IOBCHNNL EQU   X'01' --- CHANNEL END STATUS INDICATOR:                  23300000
*                        BIT = 0 MEANS NORMAL CHANNEL END OCCURRED      23400000
*                        BIT = 1 MEANS ABNORMAL END OCCURRED            23500000
*                                                                       23600000
         SPACE 1                                                        23700000
IOBUNSQR DS    B ------- REASON FOR UNSCHEDULED QUEUE                   23800000
*                                                                       23900000
IOBCPBSY EQU   X'80' --- CHANNEL PROGRAM CP1  OR CP2 BUSY               24000000
IOBNTAV1 EQU   X'40' --- NO CP4,CP5 OR CP6 AVAILABLE                    24100000
IOBNTAV2 EQU   X'20' --- NO CP7 AVAILABLE                               24200000
IOBKNWR  EQU   X'10' --- WRITE KN IS IN EFFECT(UNSCHEDULED IOB IS FOR   24300000
*                        WRITE KN)                                      24400000
IOBKNRWR EQU   X'08' --- WRITE KN IS IN EFFECT(UNSCHEDULED IOB IS FOR   24500000
*                        READ OR WRITE KN)                              24600000
         SPACE 1                                                        24700000
IOBAPP   DS    CL1 ----- APPENDAGE CODE -(CONSULT SYSTEM CONTROL BLOCKS 24800000
*                        MANUAL FOR CODE DEFINITIONS)                   24900000
IOBASYN  DS    CL1 ----- ASYNCHRONOUS ROUTINE CODE -(CONSULT SYSTEM     25000000
*                        CONTROL BLOCKS MANUAL FOR CODE DEFINITIONS)    25100000
IOBFCHAD DS    0A ------ FORWARD CHAIN ADDRESS                          25200000
IOBCOUNT DS    CL1 ----- WRITE CHECK COUNTER                            25300000
IOBFCHNB DS    AL3 ----- FORWARD CHAIN ADDRESS                          25400000
IOBBCHAD DS    A ------  BACKWARD CHAIN ADDRESS                         25500000
         EJECT                                                          25600000
*                            ****************************************** 25700000
*                            *                                        * 25800000
         ORG   IOBEXTEN *    *      GAM EXTENSION *** 40 BYTES        * 25900000
*                            *                                        * 26000000
*                            ****************************************** 26100000
         SPACE 2                                                        26200000
IOBUCBXG DS    CL1 ----- UCB INDEX                                      26400000
         DS    XL3 ----- RESERVED                                       26500000
IOBNXTPT DS    0A ------ PTR.TO NEXT AVAILABLE IOB                      26600000
*                        ZERO IF LAST IOB                               26700000
IOBSTATA DS    B ------- STATUS SWITCH                                  26800000
IOBAVLFL EQU   X'80' --- IF BIT IS 0,IOB IS AVAILABLE                   26900000
*                        IF BIT IS 1,IOB IS NOT AVAILABLE               27000000
IOBNXTPB DS    AL3 ----- PTR.TO NEXT AVAILABLE IOB                      27100000
*                        ZERO IF LAST IOB                               27200000
IOBCCW   DS    CL32 ---- CHANNEL COMMAND WORDS USED TO TRANSFER DATA    27300000
         SPACE 2                                                        27400000
*                            ****************************************** 27600000
*                            *                                        * 27700000
         ORG   IOBSEEK+8 *   *     QISAM EXTENSION *** 2 BYTES        * 27800000
*                            *                                        * 27900000
*                            ****************************************** 28000000
         SPACE 1                                                        28100000
W1IEXTEN DS    0CL2 ---- APPENDAGE                                      28200000
W1OEXTEN DS    CL2 -----  CODES                                         28300000
         EJECT                                                          28400000
*                            ****************************************** 28500000
*                            *                                        * 28600000
         ORG   IOBSEEK+8 *   * BDAM EXTENSION ** 40 BYTES + CHNL.PGM  * 28700000
*                            *                                        * 28800000
*                            ****************************************** 28900000
         SPACE 1                                                        29000000
IOBDBYTR DS    H ------- NUMBER OF UNUSED BYTES ON TRACK                29100000
IOBDIOBS DS    H ------- OVERALL SIZE OF THE IOB                        29200000
IOBDPLAD DS    0A ------ ADDRESS OF NEXT IOB IN POOL OF IOB'S           29300000
IOBDAYLI DS    B ------- ALL BITS ZERO INDICATE AVAILABILITY OF IOB     29400000
*                                                                       29500000
IOBDPLB  DS    AL3 ----- ADDRESS OF NEXT IOB IN POOL OF IOB'S           29600000
         SPACE 1                                                        29700000
IOBDTYPE DS    B ------- TYPE OF REQUEST AND SPECIFIED OPTIONS          29800000
*                                                                       29900000
IOBVERFY EQU   X'80' --- VERIFY                                         30000000
IOBOVFLO EQU   X'40' --- OVERFLOW                                       30100000
IOBEXTSC EQU   X'20' --- EXTENDED SEARCH                                30200000
IOBFDBCK EQU   X'10' --- FEEDBACK                                       30300000
IOBACTAD EQU   X'08' --- ACTUAL ADDRESSING                              30400000
IOBDYNBF EQU   X'04' --- DYNAMIC BUFFERING                              30500000
IOBRDEXC EQU   X'02' --- READ EXCLUSIVE                                 30600000
IOBRELBL EQU   X'01' --- RELATIVE BLOCK ADDRESSING                      30700000
         SPACE 1                                                        30800000
IOBDTYP2 DS    B ------- SECOND BYTE OF OPTIONS AND REQUESTS            30900000
*                                                                       31000000
IOBSKEY  EQU   X'80' --- KEY ADDRESS CODED AS 'S'                       31100000
IOBSBLKL EQU   X'40' --- BLOCK LENGTH CODED AS 'S'                      31200000
IOBSUFFX EQU   X'30' --- INDICATES TYPE OF SUFFIX('R' OR 'RU')          31300000
IOBRQUST EQU   X'08' --- BIT = 1 MEANS READ; BIT = 0 MEANS WRITE;       31400000
IOBTYPE  EQU   X'04' --- BIT = 1 MEANS KEY TYPE                         31500000
*                        BIT = 0 MEANS ID TYPE                          31600000
IOBADDTY EQU   X'02' --- ADD TYPE                                       31700000
IOBRELEX EQU   X'01' --- RELEX MACRO ISSUED                             31800000
         SPACE 1                                                        31900000
IOBDSTAT DS    0CL2  --- STATUS OF THE REQUEST                          32000000
*                                                                       32100000
IOBSTAT1 DS    B                                                        32200000
*                                                                       32300000
IOBABNRM EQU   X'80' --- ABNORMAL COMPLETION                            32400000
IOBNEWVL EQU   X'40' --- ON EXTENDED SEARCH,THE NEXT EXTENT IS ON A NEW 32500000
*                        VOLUME.THE ASI ROUTINE MUST ISSUE THE EXCP     32600000
*                        MACRO;END OF EXTENT APPENDAGE CANNOT.          32700000
*                                                                       32800000
IOBPASS2 EQU   X'10' --- ON EXTENDED SEARCH,INDICATES TO RELATIVE BLOCK 32900000
*                        CONVERSION ROUTINE THAT SECOND PASS OF A       33000000
*                        TWO-PASS CONV.ROUTINE HAS COMPLETED            33100000
IOBENQUE EQU   X'08' --- RECORD ENQUEUED(EXCLUSIVE CONTROL REQUEST)     33200000
IOBBUFF  EQU   X'04' --- BUFFER ASSIGNED TO THIS IOB                    33300000
IOBADDVU EQU   X'02' --- V OR U TYPE RECORD BEING ADDED TO DATA SET     33400000
IOBSIORT EQU   X'01' --- INDICATES TO DYNAMIC BUFFERING ROUTINE THAT    33500000
*                        IT WAS ENTERED FROM,AND IS TO RETURN TO,THE    33600000
*                        START I/O APPENDAGE MODULE                     33700000
         SPACE 1                                                        33800000
IOBSTAT2 DS    CL1 ----- ERROR CODE FOR ABNORMAL COMPLETION USED AS     33900000
*                        POST CODE IN ECB                               34000000
IOBDCPND DS    A ------- ADDRESS OF LOCATION WHERE CHANNEL END PROGRAM  34100000
*                        SHOULD END                                     34200000
IOBDBYTN DS    H ------- NUMBER OF BYTES NEEDED ON A TRACK TO WRITE A   34300000
*                        NEW BLOCK                                      34400000
         DS    XL2 ----- RESERVED                                       34500000
IOBDQPTR DS    A ------- PTR.TO IOB FOR NEXT I/O OPERATION TO EXECUTE   34600000
         DS    XL8 ----- RESERVED                                       34700000
         SPACE 1                                                        34800000
IOBDNCRF DS    CL8 ----- COUNT FIELD FOR NEW BLOCK                      34900000
IOBCHNPR DS    0D ------ CHANNEL PROGRAM                                35000000
         EJECT ,
******************************************************************
*                                                                *
*        Master Schedulre Task Input/Output Table (TIOT).        *
*                                                                *
******************************************************************
         SPACE 1
MSTIOT   DSECT ,                   Start master scheduler TIOT
         IEFVTIOT ,                Map master scheduler TIOT
         EJECT ,
******************************************************************
*                                                                *
*        OS Communications Vector Table (CVT).                   *
*                                                                *
******************************************************************
         SPACE 1
CVTDSECT DSECT ,                   Start CVT dsect
         CVT   ,                   Map CVT
         EJECT ,
******************************************************************
*                                                                *
*        Job File Control Block (JFCB).                          *
*                                                                *
******************************************************************
         SPACE 1
JFCB     DSECT ,                   Start JFCB dsect
         IEFJFCBN ,                Map JFCB
         EJECT ,
******************************************************************
*                                                                *
*        Unit Control Block (UCB).                               *
*                                                                *
******************************************************************
         SPACE 1
UCBDSECT DSECT ,                   Start UCB dsect
         IEFUCBOB ,                Map UCB
         EJECT ,
******************************************************************
*                                                                *
*        General mapping of Task Input/Output Table (TIOT).      *
*                                                                *
******************************************************************
         SPACE 1
TIOT     DSECT ,                   Start TIOT dsect
         IEFTIOT1 ,                Map TIOT
         EJECT ,
******************************************************************
*                                                                *
*        OS Task Control Block (TCB).                            *
*                                                                *
******************************************************************
         SPACE 1
         IKJTCB ,                  Map TCB
         END   ,
//*
//LKED   EXEC PGM=IEWL,PARM=(XREF,LET,LIST,NCAL,MAP),
//            REGION=128K,COND=(4,LT,ASM)
//SYSLMOD  DD DSN=SYS1.LINKLIB,DISP=SHR,
//  UNIT=3330,VOL=SER=MVTRES
//SYSUT1   DD DSN=&&SYSUT1,UNIT=SYSDA,SPACE=(1024,(50,20))
//SYSPRINT DD SYSOUT=A
//SYSLIN   DD DSN=&&OBJSET,DISP=(OLD,DELETE)
//         DD *
 ENTRY SYSVICMD
 NAME  SYSVICMD(R)
//
