//SY20ZR25 JOB 1,TMVT818,MSGLEVEL=(1,1),CLASS=A,MSGCLASS=A
//*
//*  PROBLEM DESCRIPTION(S):
//*    TMVT818 -
//*      MVT TCAM does not provide a way to specify on which
//*      display line the next non-full screen TPUT is to appear.
//*
//*      This usermod adds support for the STLINENO macro as
//*      supported by MVS 3.8 TSO/VTAM.
//*
//*   COMPONENT:  360S-CI555-EBB2218
//*
//*   APARS FIXED: TMVT818
//*
//*   SPECIAL CONDITIONS:
//*     ACTION: IEHIOSUP required:
//*       The IEHIOSUP utility must be run after the link edits
//*       performed by this usermod.  An IEHIOSUP step is included
//*       after the link edits, so no separate run should be
//*       necessary.
//*
//*     DOCUMENTATION:  New STLINENO macro:
//*       STLINENO -- Set Line Number
//*
//*       Use the STLINENO macro instruction to specify the number of
//*       the screen line on an IBM 3270 display terminal on which the
//*       next non-full-screen message should appear.
//*
//*       The format of the STLINENO macro is as follows:
//*
//*        [symbol]     STLINENO      {LINE=number     }[,MODE=ON  ]
//*                                   {LINELOC=address }[,MODE=OFF ]
//*
//*       LINE=number
//*           specifies in decimal the line number on which the next
//*           non-full-screen message is to appear.  The line number
//*           must be a value from 1 to n, where n is the maximum
//*           number of lines allowed for the terminal in use.  Either
//*           the actual line number or a register (2-12, enclosed in
//*           parentheses) containing the line number in the low-order
//*           byte can be specified.
//*
//*           Note:  The MVT implementation operates in the same
//*           manner as MVS 3.8 VTAM2:  LINE=1 does not automatically
//*           clear the screen.
//*
//*       LINELOC=address
//*           specifies the address of a fullword whose low-order byte
//*           contains the number of the screen line on which the next
//*           non-full-screen message is to appear.  Either an actual
//*           address (RX-type) or a register (2-12, enclosed in
//*           parentheses) containing the address may be specified.
//*
//*       MODE=ON | OFF
//*          is accepted for compatibility but performs no function
//*          under MVT.
//*
//*       When control is returned to the user, register 15 contains
//*       one of the following return codes:
//*
//*         Return Code
//*           Dec(Hex)     Meaning
//*         ------------   -------------------------------------------
//*             0(0)       Successful.
//*             4(4)       An incorrect parameter was specified.
//*             8(8)       The terminal type is not valid.  This macro
//*                        instruction is valid only for IBM 3270
//*                        display terminals.
//*             12(C)      The line number specified was 0 or it was
//*                        greater than the maximum number of lines
//*                        allowed for the terminal in use.
//*
//*     DOCUMENTATION:  New STFSMODE macro:
//*       STFSMODE -- Set Full Screen Mode
//*
//*       The STFSMODE macro is provided for compatibility with MVS 3.8
//*       VTAM2 and performs no function in MVT except to set a return
//*       code of zero.  Thus STFSMODE may be used to determine if
//*       usermod TMVT818 is installed.  If TMVT818 is installed,
//*       STFSMODE will return a return code of zero.  If TMVT818
//*       is not installed, STFSMODE will return a return code 4
//*       (invalid parameters supplied to SVC 94).
//*
//*       The format of the STFSMODE macro is as follows:
//*
//*        [symbol] STFSMODE [ ON ] [,INITIAL=YES] [,NOEDIT=YES] [,RSHWKEY=n]
//*                          [ OFF] [,INITIAL=NO ] [,NOEDIT=NO ]
//*
//*       No operands are required.  If operands are specified, they
//*       are syntax checked by the macro and otherwise ignored.
//*
//*       When control is returned to the user, register 15 contains
//*       one of the following return codes:
//*
//*         Return Code
//*           Dec(Hex)     Meaning
//*         ------------   -------------------------------------------
//*             0(0)       Successful (usermod TMVT818 is installed).
//*             4(4)       An incorrect parameter was specified
//*                        (usermod TMVT818 is not installed).
//*
//*   COMMENTS:
//*     LAST CHANGE:  2010/07/09
//*
//*   REWORK HISTORY:
//*     2010/05/04: Original version.
//*
//*     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:
//*
//*     MODULES
//*       IGC0009D
//*       IGG09412
//*       IGG09413
//*
//*     MACROS
//*       STLINENO
//*       STFSMODE
//*
/*JOBPARM LINES=9999
//*MAIN LINES=100
//*
//*-----------------------------------------------------------------***
//*     Assemble IGC0009D (SVC 94 router).                          ***
//*-----------------------------------------------------------------***
//ASM09D  EXEC PGM=IEUASM,
//             PARM='NOLOAD,DECK,RENT',REGION=256K,COND=(0,NE)
//SYSLIB   DD  DISP=SHR,DSN=SYS1.MACLIB,DCB=BLKSIZE=12960
//         DD  DISP=SHR,DSN=SYS1.MODGEN
//SYSPRINT DD  SYSOUT=A
//SYSUT1   DD  UNIT=SYSDA,SPACE=(TRK,(90,30))
//SYSUT2   DD  UNIT=SYSDA,SPACE=(TRK,(90,30))
//SYSUT3   DD  UNIT=SYSDA,SPACE=(TRK,(90,30))
//SYSGO    DD  DUMMY
//SYSLIN   DD  DUMMY
//SYSPUNCH DD  DSN=&&OBJSET(IGC0009D),SPACE=(TRK,(20,5,5)),
//             UNIT=SYSDA,DISP=(MOD,PASS)
//SYSIN    DD *
         PRINT OFF                                                 @L01
*        %GOTO BSLAVT;                                               /* 00100022
         MACRO                                                          00200020
         TAVTD &C,&SAVT=YES                                      S22024 00400022
         SPACE                                                          00600020
.*       CONVERTED TO BILINGUAL ON 09/26/72                      S22024 00620000
*        UPDATED 01/05/73                                        S22024 00640000
.*CHANGE ACTIVITY = AS FOLLOWS:                                 SA51078 00680000
.*C006800                                                       SA51078 00680222
.*A610000,742000-745200                                         SA51078 00680322
.*D610000-616000,742000-744000                                  SA51078 00680422
.*A208700,225600-225800                                          S22027 00700022
.*C232500,366000                                                 S22027 00720022
.*C232500                                                        S22026 00760022
.*A099000,208200-209600,225500,263000,285000,301000,303000       S22026 00840022
.*A341000,357000,695000                                          S22026 00920022
.*C178000                                                       SA65392 00940052
.*C178000                                                        S21903 00960022
.*A000000,912000                                                 S22024 00960100
.*C004000,225000,225600,232500-233000,914000                     S22024 00960200
         SPACE                                                          01000020
IEDQAVTD DSECT                                                          01200020
         AIF   ('&C' EQ '').DSECT                                       01400020
         AIF    ('&C' EQ '2').D2                                        01600020
         AIF    ('&C' EQ '3').D3                                        01800020
         AIF    ('&C' EQ '4').D4                                        02000020
.DSECT   ANOP                                                           02200020
AVTSAVE1 DS    18F .                    MESSAGE CONTROL PROG SAVEAREA   02400020
.D2      ANOP                                                           02600020
AVTSAVE2 DS    18F                                                      02800020
.D3      ANOP                                                           03000020
AVTSAVE3 DS    18F                                                      03200020
.D4      ANOP                                                           03400020
AVTSAVE4 DS    18F                                                      03600020
AVTSAVEX DS    10F .                    DISABLED SAVEAREA               03800020
         ORG   *-8                                                      04000020
AVTDLQ   DS    D .                      DLQ=TERMNAME                    04200020
AVTCSTCS DS    A IEDQSTCS               ADDR OF FIRST ENTRY             04400020
*                                         CHARACTERISTICS TABLE         04600020
AVTDPARM DS    F .                      DISABLED PARAMETER LIST         04800020
*                                         USED WITH DOUBX               05000020
AVTDOUBX DS    D .                      DISABLED DOUBLEWORD SCRATCH     05200020
AVTDOUBL DS    D .                      ENABLED SCRATCH AREA            05400020
AVTCTLCH DS    D .                      OPERATOR CONTROL CHARACTERS     05600020
AVTPASWD DS    D .                      PASSWORD                        05800020
AVTTCB   DS    A .                      ADDRESS OF MESSAGE CONTROL      06000020
*                                         PROGRAM'S TCB.                06200020
*                                         SET BY OPEN                   06400020
AVTRACE  DS    A .                      TRACE TABLE ADDRESS             06600020
*                                                                       06800020
*                       DISPATCHER'S READY QUEUES                       07000020
*                                                                       07200020
AVTREADY DS    A AVTDELEM  .            ADDRESS OF FIRST OF CHAIN       07400020
*                                         OF ELEMENTS IN PRIORITY       07600020
*                                         ORDER                         07800020
AVTREADD DS    2A .                     DISABLED FIFO READY Q           08000020
*                                                                       08200020
AVTCKGET DS    A .                      CHECKPOINT WORKAREA - GETMAIN   08400020
AVTOCGET DS    A .                      OPERATOR CONTROL WORKAREA       08600020
AVTEXA2S DS    3H .                     EXECUTED INSTRUCTIONS TO        08800020
AVTEXS2A DS    3H .                       SAVE USER'S REGISTERS         09000020
AVTPARM  DS    A .                      ADDRESS OF PARAMETERS           09200020
*                                                                       09400020
AVTPKF   DS    0X .                     PROTECTION KEY FROM TCB         09600020
*                                         OF MCP                        09800020
*                        BIT DEFINITIONS                                09900022
AVTFTCHF EQU   X'08' .                  FETCH PROTECT ACTIVE SWITCH     10000020
*                                                                       10200020
AVTBASE  DS    A IEDQAVT  .             ADDRESS OF AVT                  10400020
AVTPARM3 DS    A .                      ADDITIONAL OPTIONAL PARM        10600020
AVTDISTR DS    A .                      ADDR OF DISPATCHER TRACE        10800020
AVTRNMPT DS    A IEDQTNT  .             ADDRESS OF TERMNAME TABLE       11000020
AVTRDYA  DS    A IEDQRDYA  .            USER EXIT ADDRESS IN READY      11200020
AVTBSCAN DS    A .                      LINE END BSC MSG SCAN           11400020
AVTRARTN DS    A AVTDUMBR               ADDR OF RTN TO UPDATE LINE      11600020
*                                         TRACE TABLE. IF TRACE,        11800020
*                                         OPEN LOADS THIS MODULE        12000020
AVTPOST  DS    2F .                     QPOST PARAMETER LIST            12200020
*                                         USED BY OPERATOR CONTROL      12400020
AVTSPLPT DS    A .                      START PARAMETER LIST            12600020
*                                         POINTER, SET BY INTRO         12800020
AVTCIB   DS    AL1 .                    CIB=INTEGER                     13000020
AVTNCKPR DS    X .                      CKREQS=INTEGER                  13200020
AVTNOLBF DS    H .                      LNUNITS=INTEGER                 13400020
AVTAS    DS    A .                      RELEASE                         13600020
*                                                                       13800020
*              ADDRESSES OF TCBS OF ATTACHED TASKS                      14000020
*                                                                       14200020
AVTCKTCB DS    A .                      CHECKPOINT                      14400020
AVTOCTCB DS    A .                      OPERATOR CONTROL                14600020
AVTOLTCB DS    A .                      ON LINE TEST                    14800020
AVTCWTCB DS    A .                      COMMON WRITE FE TASK            15000020
*                                                                       15200020
*                       EVENT CONTROL BLOCKS                            15400020
*                                                                       15600020
AVTCWECA DS    A .                      COMMON WRITE FE TASK ECB        15800020
AVTCKECA DS    F .                      CHECKPOINT                      16000020
AVTOLECA DS    F .                      ON LINE TEST                    16200020
AVTOPECA DS    F .                      OPERATOR CONTROL                16400020
AVTOSECB DS    F .                      EVENT CONTROL BLOCK USED BY     16600020
*                                         DISPATCHER TO CAUSE QTAM      16800020
*                                         TASK TO BE IN WAIT STATE      17000020
AVTPCBPT DS    A .                      FIRST PROCESS CONTROL BLOCK     17200020
AVTOPTPT DS    F .                      ADDRESS OF OPTION TABLE         17400020
AVTKA02  DS    A IEDQKA02               I/O GENERATOR IN ACTIVATE       17600020
AVTHRESH DS    F                        THRESHHOLD VALUES       SA65392 17800052
AVTCRSRF DS    F .                      CROSSRF=INTEGER                 18000020
AVTCOMPT DS    A .                      ADDR OF COMMUNICATIONS PARM     18200020
AVTUI    DS    A IEDQUI                 USER INTERFACE                  18400020
AVTE8    DS    A .                      SEARCH ROUT APPL. PROG.  S22025 18600022
AVTOLTST DS    0X .                     OLTEST=INTEGER                  18800020
AVTHG02  DS    A IEDQHG02               REMOVE ELE FROM TIME DELAY Q    19000020
AVTAL    DS    A IEDQAL                 SCAN AT OFFSET           S22025 19200020
AVTGD    DS    A IEDQGD  .              BUFFER ASSOCIATION              19400020
AVTA3    DS    A .                      TRANSPARENCY CCW BUILDER (BT)   19600020
AVTAX    DS    A IEDQAX                 SCAN BUFFER              S22025 19800020
AVTEA    DS    A .                      DISPATCHER                      20000020
*                                                                       20200020
AVTDISP  EQU   * .                      THESE SUBTASKS ARE ACTIVAT-     20400020
*                                         ED BY THE DISPATCHER.         20600020
AVTHA    DS    A .                      RECEIVE SCHEDULER               20800020
AVTSCOPT DS    0XL1 .                                            S22026 20820022
*                                                                S22026 20840022
*                      BIT DEFINITIONS FOR HIGH ORDER BYTE       S22026 20860022
AVTCMBUF EQU   X'10' .                  COMMON BUFFER TRANSMISSINS22027 20870022
AVTCONC  EQU   X'08' .                  CONCENTRATOR MIXED       S22026 20880022
AVTCONCO EQU   X'04' .                  CONCENTRATOR ONLY        S22026 20900022
AVTN2741 EQU   X'02' .                  NO 2741 AND NO TSO       S22026 20920022
AVTNDIAL EQU   X'01' .                  NO DIAL                  S22026 20940022
*                                                                S22026 20960022
AVTHD    DS    A .                      SEND SCHEDULER                  21000020
AVTEW    DS    A .                      GET SCHEDULER                   21200020
AVTEC    DS    A .                      PUT SCHEDULER                   21400020
AVTEZ    DS    A .                      GET FIFO                        21600020
AVTBZ    DS    A .                      LOG SCHEDULER                   21800020
AVTR1    DS    A .                      DIAL SCHEDULER                  22000020
AVTHB    DS    A .                      BUFFERED SCHEDULER              22200020
AVTE7    DS    A .                      RETRIEVE SCHEDULER              22400020
AVTQ1    DS    A .                      LOCAL SCHEDULER          S22024 22500022
AVTCSCH  DS    A .                      CONC SEND SCHEDULER      S22026 22550022
AVTTJ    DS    A .                      3705 SEND SCHEDULER      S22024 22560022
         DS    A .                      RESERVED                 S22027 22570022
AVTCMBSS DS    A .                      COMMBUF SEND SCHEDULER   S22027 22580022
*                                                                       22600020
*                                                                       22800020
*                       SPECIAL ELEMENTS                                23000020
*                                                                       23200020
         DS    A .                      RESERVED                 S22024 23250022
AVTSAVTP DS    A .                      SECONDARY AVT POINTER    S22024 23280022
AVTPLCBN DS    H .                      NUMBER OF PLCBS          S22024 23310022
AVTABEND DS    3H .                     ABEND 045, SUBCODE IN R15       23350020
AVTDMECB DS    A .                      DUMMY LINE I/O ECB              23400020
AVTA3TL  DS    A .                      IEDQA3,TRANSLATE LIST           23600020
AVTTONE  DS    A .                      ADDR OF WORLD TRADE TONE CHARS  23800020
AVTNX    DS    A .                      OPERATOR AWARENESS MSG ROUTER   24000020
AVTIOT   DS    A IGG019Q0               I/O TRACE TABLE HANDLER         24200020
AVTHI    DS    A IEDQHI                 SYSTEM DELAY QCB ADDR           24400020
AVTHK    DS    A IEDQHK                 STOPLINE QCB ADDR               24600020
AVTCKRMV DS    4F .                     REQUESTS REMOVAL OF CKPT        24800020
*                                         ELE FROM TIME DELAY QUEUE     25000020
AVTCKELE DS    2A .                     TIME DELAY OR REUS POSTS        25200020
*                                         ELE TO START CHECKPOINT       25400020
AVTSCBSZ DS    X .                      NO. BYTES IN SCB INCLUDING      25600020
*                                         SAVEAREA FOR USER'S REGS      25800020
AVTCKQAD DS    AL3 AVTCKPTB             CKPT QCB ADDRESS                26000020
AVTCKELF DS    X .                      FLAG BITS                       26200020
*                        BIT DEFINITIONS                                26300022
AVTCRDYN EQU   X'80' .                  CHECKPT REQUESTED BY READY      26400020
AVTCMCPN EQU   X'40' .                  CHECKPT REQUESTED BY            26600020
*                                         MCPCLOSE                      26800020
*        EQU   X'20' .                  UNUSED                          27000020
AVTCINCN EQU   X'10' .                  CHECKPT REQUESTED BY            27200020
*                                         INCIDENT OVERFLOW             27400020
AVTCCLCN EQU   X'08' .                  CLOSEDOWN COMPLETION BIT        27600020
AVTCPIPN EQU   X'04' .                  CHECKPT IN PROGRESS             27800020
AVTCRTLN EQU   X'02' .                  CHECKPOINT REQUESTED            28000020
AVTWARMN EQU   X'01' .                  WARM RESTART                    28300020
AVTCONTN EQU   X'FF'-AVTWARMN .         CONTINUATION RESTART            28400020
*                                                                       28500022
AVTCPRCD DS    X .                      CPRCDS=INTEGER                  28600020
AVTCKELV DS    H .                      CPINTVL=TIME INTERVAL           28800020
AVTCKTIM DS    H .                      TIME OF DAY OF INTERRUPT        29000020
         DS    X .                      INDEX TO QCB ADDRESS            29200020
AVTOPERL DS    X .                      OPEN ERROR LOCATOR              29400020
AVTOPXCL DS    2X .                     OPEN MODULE ID HAVING ERROR     29600020
AVTOPERT DS    X .                      TYPE OF OPEN ERROR              29800020
AVTCKBYT DS    X .                      CKPT TIME DELAY STATUS          30000020
*                        BIT DEFINITIONS                                30100022
AVTCKONQ EQU   X'02' .                  BIT ON, ELE ON TIME Q           30200020
*                                                                       30300022
AVTOPETR DS    0X .                     OPEN TRANSLATE BYTE             30400020
AVTHG01  DS    A IEDQHG01               TIME DELAY SUBROUTINE           30600020
AVTBSX   DS    0X 8                     OFFSET TO BINARY SEARCH ADDR    30800020
AVTCKLNK DS    A .                      LINK FIELD ON TIME Q            31000020
AVTDELEM DS    A AVTREADY-8             DUMMY LAST ELEMENT              31200020
AVTDELAD DS    A AVTDELEM  .            ADDR OF DUMMY LAST ELEMENT      31400020
*                                                                       31600020
AVTCCELE DS    2F .                     OPERATOR CONTROL POSTS THIS     31800020
*                                         ELEMENT TO REQUEST AN         32000020
*                                         INCIDENT CHECKPOINT           32200020
*                                                                       32400020
AVTCLRHI DS    2X .                     WHEN USED WITH NEXT HALFWOD     32600020
*                                         IS A MASK FOR CLEARING        32800020
*                                         LEFT TWO BYTES OF REGISTR     33000020
AVTHFF   DS    XL2 FFFF  .              HALF WORD ALL 'F'               33200020
AVTADBUF DS    A .                      BUFFER ADDRESS SET BY           33400020
*                                         MH CONTROL                    33600020
AVT2260L DS    F .                      PT TO 2260 LOCAL APPENDAGE      33800020
AVTSYSER DS    X .                      SYSTEM ERROR FLAG BYTE          34000020
*                        BIT DEFINITIONS                                34100022
AVTCMINN EQU   X'80' .                  NO. CORE Q UNITS LESS THAN      34200020
*                                         SPECIFIED BY MSMIN=PERCENT    34400020
AVTCMINF EQU   X'FF'-AVTCMINN .         NO. CORE Q UNITS MORE THAN      34600020
*                                       SPECIFIED BY MSMIN=PERCENT      34800020
AVTCMAXN EQU   X'40' .                  NO. CORE Q UNITS MORE THAN      35000020
*                                         SPECIFIED BY MSMAX=PERCENT    35200020
AVTCMAXF EQU   X'FF'-AVTCMAXN .         NO. CORE Q UNITS LESS THAN      35400020
*                                         SPECIFIED BY MSMAX=PERCENT    35600020
*                                                                       35700022
AVTMSGS  DS    AL3 IEDQMSGS             LIST OF OPTIONAL VCONS          35800020
*                                                                       36000020
*                       POINTERS TO QCBS                                36200020
*                                                                       36400020
AVTCBQCB DS    A .                      COMMBUF MASTER QCB       S22027 36600022
AVTSUPPT DS    A .                      STARTUP MESSAGE                 36800020
AVTTSOPT DS    A .                      TS INPUT                        37000020
AVTOCQPT DS    A IEDQEU                 OPEN/CLOSE APPLICATION PROG     37200020
*                                                                       37400020
*                       NON-OPTIONAL QCBS                               37600020
*                                                                       37800020
AVTDELYB DS    4A .                     TIME DELAY QCB                  37900020
AVTREFTM DS    H .                      REFERENCE TIME, CURRENT TIME    37960020
*                                         OF DAY + OR - 6 HOURS         38020020
AVTINOUT DS    H .                      DUMMY INEND/OUTEND PARM LIST    38100020
AVTIMQPS DS    2A .                     SVC 102 PARM, POST TIME QCB     38200020
*                                         TO ITSELF AT TIME INTERRUPT   38400020
AVTTIMQ  DS    A .                      TIME DELAY QUEUE                38600020
AVTBFREB DS    3F .                     AVAILABLE BUFFER                38800020
AVTBFRTB DS    3F .                     BUFFER RETURN                   39000020
AVTCKPTB DS    3F .                     CHECKPOINT                      39200020
AVTOPCOB DS    3F .                     OPERATOR CONTROL                39400020
AVTOLTQB DS    3F .                     ON LINE TEST QCB                39600020
AVTACTIB DS    3F .                     ACTIVATE                        39800020
AVTCLOSB DS    3F .                     CLOSEDOWN COMPLETION            40000020
AVTCPRMB DS    3F .                     REMOVES ELE FROM TIME DELAY Q   40200020
AVTDSIOB DS    3F .                     DISK I/O QCB                    40400020
AVTCPBCB DS    3F .                     CPB CLEANUP QCB                 40600020
AVTCOREC DS    A .                      BUFFERS POOL FOR CLOSE          40800020
AVTCADDR DS    F .                      CORE QUEUE COUNT                41000020
AVTFZERO DS    F .                      FULLWORD OF ZEROES              41200020
*                                                                       41400020
*                       FE COMMON WRITE INTERFACE AREA                  41600020
*                                                                       41800020
AVTCAREA DS    A IEDQFE                 PATCH MODULE                    42000020
AVTCWPM1 DS    F .                      FIRST PARAMETER POINTER         42200020
AVTCWEC1 DS    F .                      FIRST ECB                       42400020
AVTCWFL1 DS    X .                      FIRST FLAG BYTE                 42600020
*                                                                       42800020
*                       AVTCWFL1 BIT DEFINITIONS                        43000020
AVTCOMWN EQU   X'80' .                  COMWRTE=YES                     43200020
*                                       REMAINING BITS UNDEFINED        43400020
*                                                                       43600020
AVTCWFL2 DS    X .                      ADDITIONAL FLAG BYTES           43800020
*                                                                       44000020
*                       AVTCWFL2 BIT DEFINITIONS                        44200020
AVTCWACT EQU   X'80' .                  COMMON WRITE IS ACTIVE          44400020
*        EQU   X'7C' .                  RESERVED BY COMWRITE            44600020
AVTCWRAP EQU   X'02' .                  '0'-WRAPPING OUTPUT ALLOWED     44800020
*                                         (REUSE SAME VOLUME)           45000020
*                                       '1'-WRAP NOT ALLOWED (INVOKE    45200020
*                                         VOL SWITCHING-REQUIRES        45400020
*                                         MANUAL INTERVENTION)          45600020
AVTCWCLD EQU   X'01' .                  '1'-COMWRITE CLOSEDOWN          45800020
*                                         REQUESTED BY USER IN          46000020
*                                         PARMLIST OR TCAM CLOSEDOWN.   46200020
*                                         SET BY COMWRITE               46400020
*                                                                       46600020
AVTCWTS1 DS    X .                                                      46800020
AVTCWTS2 DS    X .                                                      47000020
AVTCWPM2 DS    F .                      SECOND PARAMETER POINTER        47200020
AVTCWEC2 DS    F .                      SECOND ECB                      47400020
AVTAFE10 DS    A .                      ENTRY POINT OF IEDQFE10         47600020
AVTAFE20 DS    A .                      ENTRY POINT OF IEDQFE20         47800020
AVTAFE30 DS    A .                      ENTRY POINT OF IEDQFE30         48000020
AVTCWINT DS    15F                      SERVICE COMMUNICATION   SA64772 48200053
*                                         AREA                  SA64772 48260053
AVTCWTOT DS    F                        TOTE COMMUNICATION AREA SA64772 48320053
*                                                                       48400020
*        GETMAIN EC,MF=L                GETMAIN PARAMETER LIST          48600020
AVTGETMN GETMAIN EC,MF=L                                                48800020
AVTHA2   DS    H 2  .                   CONSTANT                        49000020
AVTHA3   DS    H 3  .                   CONSTANT                        49200020
AVTHA4   DS    H 4  .                   CONSTANT                        49400020
AVTHA7   DS    H 7  .                   CONSTANT                        49600020
AVTHA16  DS    H 16  .                  CONSTANT                        49800020
AVTKEYLE DS    H .                      KEYLEN ON MSG QUEUES            50000020
AVTLNCNT DS    H .                      NUMBER OF LINES OPENED          50200020
*                                         SET BY OPEN                   50400020
*                                         CHECKED BY SYSTEM DELAY       50600020
AVTOPCNT DS    H .                      NUMBER OF LINES GRABBED BY      50800020
*                                         OPERATOR CONTROL.             51000020
*                                         SET BY DELAY SUBTASK          51200020
AVTOPCON DS    H .                      TERMNAME TABLE OFFSET TO        51400020
*                                         PRIMARY OPERATOR CONTROL      51600020
*                                         CONSOLE.  SET BY OPEN         51800020
*                                         FROM PRIMARY=TERMNAME IN      52000020
*                                         INTRO MACRO                   52200020
AVTAVFCT DS    H .                      SET BY OPEN TO NUMBER OF        52400020
*                                         BUFFERS IN POOL               52600020
AVTSMCNT DS    H .                      NUMBER OF LINES SERVICED BY     52800020
*                                         STARTUP MESSAGE SUBTASK       53000020
AVTINTLV DS    H .                      NUMBER OF SECONDS OF SYSTEM     53200020
*                                         DELAY                         53400020
*                                           SET BY OPERATOR CONTROL     53600020
*                                             OR BY INTRO INTVAL=INT    53800020
*                                           CHECKED BY DELAY SUBTASK    54000020
AVTDLQX  DS    H .                      OFFSET INTO TNT OF DEAD         54200020
*                                         LETTER QUEUE                  54400020
AVTDUMBR DS    H BR 11                  DUMMY LINE TRACE TABLE UPDATE   54600020
AVTBIT1  DS    X .                      FLAG BITS                       54800020
*                                                                       55000020
*                       BIT1 DEFINITIONS                                55200020
*                                                                       55400020
AVTAPLKN EQU   X'80' .                  PREVENT DISK END APPENDAGE      55600020
*                                         FROM ADDING A CPB TO DIS-     55800020
*                                         ABLED DISK END Q FOR CPB      56000020
*                                         CLEANUP (AVTDKAPQ)            56200020
AVTAPLKF EQU   X'7F' .                  PERMITS ABOVE ACTION            56400020
AVTTSON  EQU   X'40' .                  ENVIRON=TSO (OR MIXED)          56600020
AVTAQTAN EQU   X'20' .                  ENVIRON=TCAM (OR MIXED)         56800020
AVTDLAYN EQU   X'10' .                  SYSTEM DELAY IS IN EFFECT       57000020
*                                         SET BY OPERATOR CONTROL       57200020
AVTDLAYF EQU   X'EF' .                  SYSTEM DELAY NOT IN EFFECT      57400020
*                                         TURNED OFF BY DELAY SUBT      57600020
AVTREADN EQU   X'08' .                  READY MACRO HAS BEEN            57800020
*                                         EXECUTED                      58000020
*                                         SET BY READY MACRO            58200020
*                                         CHECKED BY OPEN               58400020
AVTCLOSN EQU   X'04' .                  CLOSEDOWN INDICATOR             58600020
*                                         '0'-CLOSEDOWN NOT REQUEST     58800020
*                                         '1'-CLOSEDOWN REQUESTED       59000020
AVTQUCKN EQU   X'02' .                  TYPE OF CLOSEDOWN               59200020
*                                         '0'-FLUSH                     59400020
*                                         '1'-QUICK                     59600020
AVTDISKN EQU   X'01' .                  DISK=YES                        59800020
*                                                                       60000020
AVTBIT2  DS    X .                      FLAG BITS                       60200020
*                                                                       60400020
*                       BIT2 DEFINITIONS                                60600020
*                                                                       60800020
AVTEXTSC EQU   X'80' .                  EXTENDED RESTART        SA51078 61000022
AVTREUSN EQU   X'40' .                  REUS IS RUNNING                 61800020
*                                         SET BY REUS                   62000020
*                                         CHECKED BY CPB CLEANUP        62200020
AVTREUSF EQU   X'BF' .                    TURNED OFF BY REUS            62400020
AVTCOPYN EQU   X'20' .                  COPY WANTS CONTROL              62600020
AVTTOPOL EQU   X'10' .                  TOPMSG=YES/NO                   62800020
*                                         ON,(NO) SUPPRESS TIME         63000020
*                                           OUT POLL MSGS               63200020
*                                         OFF,(YES) DISPLAY MSGS        63400020
AVTSTRTN EQU   X'08' .                  RESTART HAS BEGUN               63500020
AVTSTRTF EQU   X'F7' .                  COLDSTART (OR RESTART NOT       63600020
*                                        YET BEGUN)                     63800020
AVTOPEIN EQU   X'04' .                  INITIAL LOAD DONE INDICATOR     64000020
*              X'02'                    LINETYP=STSP                    64200020
*              X'01'                    LINETYP=BISC                    64400020
*              X'03'                    LINETYP=MINI                    64600020
*              X'00'                    LINETYP=BOTH                    64800020
AVTBIT3  DS    X .                      FLAG BITS                       65000020
*                       AVTBIT3 BIT DEFINITIONS                         65200020
AVTSTAN  EQU   X'01' .                  S=C OR S=W SPECIFIED            65400020
AVTSTACN EQU   X'02' .                  S=C COLDSTART                   65600020
AVTSTAWN EQU   X'FD' .                  S=W WARMSTART                   65800020
AVTSTAIN EQU   X'04' .                  S=I INVITAION LISTS             66000020
AVTSTAYN EQU   X'08' .                  S=Y NO CONTINUATION             66200020
AVTOLTBN EQU   X'10' .                  MAX LOAD OF OLT REACHED         66400020
AVTTSAB  EQU   X'20' .                  TSO HAS ABENDED.SET BY TIME     66600020
*                                         SHARING ABEND MODULE. RESET   66800020
*                                         BY START TIME SHARING. CKED   67000020
*                                         BY TSINPUT & TSOUTPUT         67200020
AVTRFULN EQU   X'40' .                  REUS DISK ZONE FULL             67400020
*                                         SET BY REUS                   67600020
AVTRFULF EQU   X'BF' .                  REUS DISK READY TO RECEIVE      67800020
*                                         TURNED OFF BY REUS            68000020
*                                         CHECKED BY REC. SCHEDULER     68200020
*                                           AND LINE END APPEN          68400020
AVTRECVN EQU   X'80' .                  CORE QUEUE FULL                 68600020
*                                         SET BY DESTINATION SCHEDULER  68800020
AVTRECVF EQU   X'7F' .                    TURNED OFF BY DISK I/O        69000020
*                                         CHECKED BY RECEIVE SCHEDULER  69200020
*                                           AND LINE END APPENDAGE      69400020
*                                                                       69500022
AVTCKRST DS    X .                      RESTART=INTEGER                 69600020
AVTDSKCT DS    H .                      NUMBER OF BUFFERS ON CPBS       69800020
*                                                                       70000020
*                  END OF BASIC AVT WHEN ENVIRON=TSO                    70200020
*                                                                       70400020
AVTHM02  DS    A IEDQHM02               DESTINATION ASSIGNMENT          70600020
AVTCMIN  DS    A .                      MSMIN=INTEGER                   70800020
AVTCMAX  DS    A .                      MSMAX=INTEGER                   71000020
AVTTOTNC DS    F .                      NUMBER OF RECORDS IN ENTIRE     71200020
*                                         DATA SET (MSUNITS=INTEGER)    71400020
AVTNCPBQ DS    2A .                     Q OF BUFFERS AND ERBS           71600020
*                                         WAITING TO BE PROCESSED       71800020
*                                                                       72000020
*                  END OF CORE QUEUE AVT WHEN DISK=NO                   72200020
*                                                                       72400020
AVTFL    DS    A .                      DISK EXCP DRIVER                72600020
AVTIA    DS    A .                      REUS                            72800020
AVTCOPY  DS    A .                      COPY SUBTASK QCB POINTER        73000020
AVTDKAPQ DS    2A .                     Q OF CPBS TO BE PROCESSED       73200020
*                                         BY CPB CLEANUP (DISABLED)     73400020
AVTDKENQ DS    2A .                     SAVE AS ABOVE BUT ENABLED       73600020
AVTNOBFQ DS    2A .                     Q OF CPBS WITHOUT BUFFERS.      73800020
*                                         USED BY CPB CLEANUP           74000020
AVTBIT4  DS    XL1 .                    FLAGBITS                SA51078 74200022
*                       AVTBIT4 BIT DEFINITIONS                 SA51078 74280022
AVTRUFTN EQU   X'80' .                  REUS ACTIVE AT CHECKPT  SA51078 74360022
         DS    AL3 .                    RESERVED                SA51078 74440022
         DS    AL4 .                    RESERVED                SA51078 74520022
AVTINCPQ DS    2A .                     Q OF CPBS REQUESTING I/O        74600020
*                                         TO BE DONE BY EXCP DRIVER     74800020
AVTFCPB  DS    A .                      Q OF INACTIVE CPBS, CALLED      75000020
*                                         THE CPB FREEPOOL              75200020
AVTCPBPT DS    A .                      ADDRESS OF CPB FREEPOOL TO      75400020
*                                         BE FREEMAINED BY DISK CLOSE   75600020
AVTIOBR  DS    A .                      ADDRESS OF LIST OF IOBS,        75800020
*                                         ONE FOR EACH EXTENT OF        76000020
*                                         THE REUSABLE DISK QUEUE       76200020
AVTIOBN  DS    A .                      SAME AS ABOVE, BUT FOR THE      76400020
*                                         NON-REUSABLE DISK QUEUE       76600020
AVTLODPT DS    F .                      RADDR WHEN REUS ACTIVATED       76800020
*                       REUSABLE DISK DEFINITION                        77000020
AVTADEBR DS    A .                      ADDRESS OF DEB                  77200020
AVTNOVOR DS    F .                      NUMBER OF EXTENTS               77400020
AVTRCTRR DS    F .                      NUMBER OF RECORDS/TRACK         77600020
AVTTRCYR DS    F .                      NUMBER OR TRACKS/CYLINDER       77800020
AVTTOTNR DS    F .                      NUMBER OF RECORDS IN ENTIRE     78000020
*                                         DATA SET                      78200020
AVTVOLRR DS    F .                      PRODUCT OF NUMBER OF            78400020
*                                         EXTENTS TIMES NUMBER OF       78600020
*                                         RECORDS/TRACK                 78800020
*                       NONREUSABLE DISK DEFINITION                     79000020
AVTADEBN DS    A .                      ADDRESS OF DEB                  79200020
AVTNOVON DS    F .                      NUMBER OF EXTENTS               79400020
AVTRCTRN DS    F .                      NUMBER OF RECORDS/TRACK         79600020
AVTTRCYN DS    F .                      NUMBER OF TRACKS/CYLINDER       79800020
AVTTOTNN DS    F .                      NUMBER OF RECORDS IN ENTIRE     80000020
*                                         DATA SET                      80200020
AVTVOLRN DS    F .                      PRODUCT OF NUMBER OF            80400020
*                                         EXTENTS TIMES NUMBER OF       80600020
*                                         RECORDS/TRACK                 80800020
AVTHRESN DS    F .                      THE ABSOLUTE RECORD NUMBER      81000020
*                                         WHICH IS THE THRESHOLD TO     81200020
*                                         CAUSE CLOSEDOWN DUE TO        81400020
*                                         THE FILLING OF THE            81600020
*                                         NON-REUSABLE DISK QUEUE       81800020
AVTNADDR DS    F .                      NON-REUSABLE DISK QUEUE         82000020
AVTRADDR DS    F .                      REUSABLE DISK QUEUE             82200020
AVTHRESE DS    0F .                     NON-REUSABLE THRESHOLD          82400020
*                                         CLOSEDOWN ELEMENT, 3 WORDS    82600020
         DS    F .                      ADDR OF OPERATOR CONTROL QCB    82800020
         DS    X .                      PRIORITY (VALUE IS PRIOPCTL     83000020
*                                         PLUS 4)                       83200020
         DS    AL3 .                    LINK FIELD                      83400020
         DS    X .                      X'11' - FLUSH CLOSE REQUESTED   83600020
         DS    X .                      X'0C' - LENGTH OF ELEMENT       83800020
         DS    X .                      UNUSED - (REL.LINE NO.)         84000020
AVTHRESS DS    X .                      COMPLETION CODE, USED AS STATUS 84200020
*                                         X'FF' - UNUSED ELEMENT        84400020
*                                         X'F0' - ELE HAS BEEN POSTED   84600020
*                                         X'00' OR X'04' CLOSED DOWN    84800020
AVTCPBNO DS    H .                      CPB=INTEGER                     85000020
*                                                                       85200020
*                       TCAM SYSTEM EQUATES                             85400020
*                                                                       85600020
AVTCVTPT EQU   240 .                    OFFSET INTO CVT OF ADDRESS      85800020
*                                         OF TCAM AVT POINTER           86000020
AVTUMALN EQU   12 .                     LENGTH OF CCW-TIC (UNIT         86200020
*                                         MANAGEMENT AREA)              86400020
AVTDATLN EQU   6 .                      NO. OF BYTES IN DATA FIELD      86600020
*                                         OF MESSAGE Q RECORD           86800020
AVTMINKY EQU   33 .                     MINIMUM KEYLENGTH               87000020
AVTHDRSZ EQU   30 .                     SIZE OF HEADER PREFIX           87200020
AVTTXTSZ EQU   23 .                     SIZE OF TEXT PREFIX             87400020
AVTTSOSZ EQU   9 .                      SIZE OF TSO PREFIX              87600020
AVTSVSIZ EQU   72 .                     NO. BYTES IN A SAVEAREA         87800020
AVTECD4  EQU   4 .                      RETURN CODE OF 4                88000020
AVTECD8  EQU   8 .                      RETURN CODE OF 8                88200020
AVTECD12 EQU   12 .                     RETURN CODE OF 12               88400020
AVTECD16 EQU   16 .                     RETURN CODE OF 16               88600020
AVTECD20 EQU   20 .                     RETURN CODE OF 20               88800020
AVTERACE EQU   24 .                     SIZE OF TRACE TABLE ENTRY       89000020
AVTE80   EQU   X'80' .                  SIGN BIT MASK                   89200020
AVTMODBT EQU   X'80' .                  2 TO THE 23RD BIT MASK          89400020
AVTEZERO EQU   X'00' .                  EQUATED ZERO                    89600020
AVTEBLNK EQU   C' ' .                   EQUATED BLANK CHARACTER         89800020
AVTEPER  EQU   C'.' .                   EQUATED PERIOD                  90000020
AVTEFF   EQU   X'FF' .                  ALL ONES                        90200020
* BIT DEFINITIONS OF THE HIGH ORDER BYTE OF TCAM WORD IN CVT            90400020
AVTCVT80 EQU   X'80' .                  TCAM OPEN COMPLETED             90600020
AVTCVT40 EQU   X'40' .                  2260 LOCAL SUPPORT              90800020
AVTCVT08 EQU   X'08' .                  MCP IS FETCH PROTECTED          91000020
*                       OFFSETS FOR MP65 SUPPORT                        91020020
AVTCVTMP EQU   X'C0' .                  OFFSET INTO CVT OF ADDRESS      91040020
*                                         OF MULTIPROCESSOR CVT         91060020
AVTMPTRR EQU   X'0C' .                  OFFSET INTO MP CVT OF           91080020
*                                         TASK REMOVAL ROUTINE          91100020
AVTCPUST EQU   X'2AF' .                 ABSOLUTE LOCATION LOW CORE      91120020
*                                         OF CPUSTAT BYTE               91140020
         SPACE 2                                                        91200020
         AIF   ('&SAVT' NE 'YES').SAVTEND                        S22024 91210022
IEDNSVTD DSECT                                                   S22024 91220022
         DS    A .                      RESERVED                 S22024 91230022
SAVTPLCB DS    A .                      PLCB RETURN QCB ADDRESS  S22024 91240022
SAVTPLBF EQU   X'80' .                  PLCB POOL FORMATTED      S22024 91250022
SAVTBTUT DS    A .                      BTU TRACE TABLE ADDRESS  S22024 91260022
SAVTDBQB DS    A .                      3705 DEBLOCK QCB ADDRESS S22024 91270022
SAVTODQB DS    A .                      3705 OUTPT DTA HNDLR QCB S22024 91280022
SAVTFLDH DS    A .                      3705 1ST LEVEL DATA HNDLRS22024 91290022
SAVTRQCH DS    A .                      APPLICATION REQ QUEUE    S22024 91300022
SAVTRQTG DS    H .                      BTU REQUEST TAG          S22024 91310022
         DS    H .                      RESERVED                 S22024 91320022
SAVTLCBP DS    A .                      PSEUDO-LCB POOL ADDRESS  S22024 91330022
SAVTID   DS    A .                      ID VERIFICATION ROUTINE  S22024 91340022
         SPACE 2                                                 S22024 91350022
.SAVTEND ANOP                                                    S22024 91360022
         MEND  , */                                                     91400022
*        %GOTO BSLLCB;                                               /* 00200022
         MACRO                                                          00400020
         TLCBD                                                          00800020
.*D300000,560000-564000,600000                                   S22026 00900022
.*A120000,182000,241000-243000,450000,542000,5561000             S22026 01000022
.*A562000-565000                                                 S22026 01100022
.*A243200-243800                                                 S99238 01150022
.*A241000,243000                                                SA58994 01170021
IEDQLCB  DSECT                                                          01200020
         SPACE                                                          01600020
.*       CONVERTED TO BILINGUAL ON 07/18/72                      S22024 02000022
*        UPDATED 01/05/73                                        S22024 02200022
         SPACE                                                          02400020
LCBRCB   DS    0D .                     RESOURCE CONTROL BLOCK          02800020
LCBKEY   DS    XL1 .                    ELEMENT KEY OF BUFFER           03200020
LCBQCBA  DS    AL3 .                    QCB ADDRESS                     03600020
LCBPRI   DS    XL1 .                    PRIORITY OF BUFFER              04000020
LCBLINK  DS    AL3 .                    LINK FIELD OF BUFFER            04400020
LCBRSKEY DS    XL1 .                    RECEIVE SCHEDULER KEY           04800020
LCBSTCBA DS    AL3 .                    ADDRESS OF FIRST STCB WHEN      05200020
*                                       LCB IS A QCB.                   05600020
LCBRSPRI DS    XL1 .                    RECEIVE SCHEDULER PRIORITY      06000020
LCBRNLCB DS    0AL3 .                   3705 LCB ADDRESS         S22024 06200022
LCBRSLNK DS    AL3 .                    ADDR NEXT ITEM IN CHAIN         06400020
LCBEOLTD DS    XL2 .                    END OF LIST TIME DELAY          06800020
LCBTDL   DS    XL1 .                    TIME DELAY QUEUE OFFSET TO      07200020
*                                       QCB ADDRESS. FOR LCB = X'14'    07600020
LCBTSOB  DS    XL1 .                    TSO STATUS BITS             TSO 08000020
*                        BIT DEFINITIONS                            TSO 08400020
LCBWRBRK EQU   X'80' .                  WRITE BREAK IN PROGRESS     TSO 09000020
LCBTSBUF EQU   X'40' .                  BUFFER HAS TS PREFIX        TSO 09600020
LCBSATRD EQU   X'20' .                  SIMULATED ATTN READ REQUEST TSO 10000020
LCBSOPL  EQU   X'10' .                  START OF POLLING LIST       TSO 10400020
LCBPREP  EQU   X'08' .                  PREPARE ON LINE             TSO 10800020
LCBCIRCD EQU   X'04' .                  CIRCLE D SENT TO 2741       TSO 11200020
LCBINHBN EQU   X'02' .                  USE INHIBITS FOR THIS TERM  TSO 11600020
LCB2741N EQU   X'01' .                  2741 ON THIS LINE           TSO 11800022
*                                                                       12000022
LCBRSIDT DS    0A .                     RSID TABLE ADDRESS       S22024 12200022
LCBCHAIN DS    XL1 .                    DISPOSITION STATUS BITS         12400020
*                        BIT DEFINITIONS                                12800020
*                                                                       13200020
LCBSCRNN EQU   X'80' .                  SCREEN CHANGE REQUESTED         13600020
LCBINVRQ EQU   X'80'                    INITIAL INVITE REQUEST   S22024 13800022
LCBSCRNF EQU   X'7F' .                  NO SCREEN CHANGE REQUESTED      14000020
LCBEXCP  EQU   X'40' .                  DELAY EXCP TILL ASSOCIATION     14400020
LCBERMSG EQU   X'20' .                  ERP MESSAGE WAITING             14800020
LCBNORTY EQU   X'10' .                  TEXT RETRY NOT POSSIBLE         15200020
LCBUREQN EQU   X'08' .                  UNIT REQUEST IN PROGRESS        15600020
LCBUREQF EQU   X'FF'-LCBUREQN .         UNIT REQUEST NOT IN PROGRESS    16000020
LCBBFRSZ EQU   X'04' .                  QUEUE MANAGEMENT FLAG           16400020
LCBTETEN EQU   X'02' .                  USER REQUESTED TETE A TETE      16800020
LCBTETEF EQU   X'FD' .                  TETE A TETE NOT REQUESTED       17200020
LCBABRTN EQU   X'01' .                  ABORT SEQUENCE MUST BE SENT     17600020
LCBABRTF EQU   X'FE' .                  ABORT SEQUENCE NOT REQUIRED     18000020
*                                                                       18200022
LCBINSRC DS    AL3 .                    IN-SOURCE CHAIN                 18400020
LCBNTXT  DS    XL1 .                    PRFNTXT SAVE                    18800020
LCBSCBDA DS    AL3 .                    ADDR SCB DIRECTORY              19200020
LCBLNENT EQU   LCBSCBDA+1 .             TNT OFFSET TO LINE ENTRY        19600020
LCBISZE  DS    XL1 .                    COUNT OF IDLES RESERVED         20000020
LCBFSBFR DS    0XL3 .                   FIRST BFR ASSIGNED TO THIS LCB  20800020
LCBLSBFR DS    AL3 .                                                    21200020
LCBFLAG1 DS    XL1 .                    IOS FLAGS 1                     21600020
LCBFLAG2 DS    XL1 .                    IOS FLAGS 2                     22000020
LCBSENS0 DS    XL1 .                    SENSE BYTE 0                    22400020
LCBSENS1 DS    XL1 .                    SENSE BYTE 1                    22800020
LCBECBCC DS    XL1 .                    COMPLETION CODE                 23200020
LCBECBPT DS    AL3 .                    ADDR OF ECB                     23600020
LCBFLAG3 DS    XL1 .                    IOS FLAGS 3                     24000020
*                        BIT DEFINITIONS                         S22026 24100022
LCBOBR   EQU   X'01'                    OBR RECORDING FLAG      SA58994 24130021
*                                                                       24140021
*                                                                       24160021
LCBOBRRD EQU   X'02' .                  TPER RECORD PROCESSING   S22026 24200022
*                                                                S22026 24300022
LCBIOMSG EQU   X'04'                    I/O MESSAGE FLAG        SA58994 24306021
*                                                                       24309021
*                                                                       24312021
LCBSOHC  EQU   X'08' .                  SOH % C FLAG BIT         S99238 24320022
LCBSOHR  EQU   X'20' .                  SOH % R FLAG BIT         S99228 24340022
*                                       IF R & C FLAG BITS OFF   S99238 24360022
*                                       SOH % E MSG IS ASSUMED   S99238 24380022
LCBCSW   DS    XL7 .                    LAST CSW                        24400020
LCBSIOCC DS    XL1 .                    SIO CONDITION CODE              24800020
LCBSTART DS    AL3 .                    ADDR OF CHANNEL PROGRAM         25200020
LCBDCBPT DS    A .                      ADDRESS OF DCB                  25600020
LCBRESTR DS    0A .                     ERROR MESSAGE DATA              26000020
LCBRCQCB DS    A .                      QCB TO POST RECALLED BUFFER TO  26400020
LCBLCDID DS    0H .                     RESOURCE ID OF DIAL LINE S22024 26600022
LCBINCAM DS    H .                      IOS                             26800020
LCBTTBIN DS    0H .                     TERM TO BE CONNECTED INDEX      27200020
LCBERRCT DS    H .                      IOS ERROR COUNTERS              27600020
LCBUCBX  DS    XL1 .                    UCB INDEX                       28000020
LCBRCBFR DS    0AL3 .                   POINTER TO RECALLED BUFFER      28400020
LCBRDBFR DS    0AL3 .                   START OF READ CHAN PROG  S22024 28600022
LCBLSPCI DS    AL3 .                    ADDR OF LAST SRVCD PCI          28800020
LCBRECOF DS    H .                      OFFSET INTO CURRENT BLOCK       29200020
LCBSTATE DS    0H .                     STATUS BITS                     29600020
LCBSTAT1 DS    XL1 .                    FIRST STATUS BYTE               30400020
*                        BIT DEFINITIONS                                30800020
LCBRCLLN EQU   X'80' .                  RECALL BEING PERFORMED          31200020
LCBRCLLF EQU   X'FF'-LCBRCLLN .         NO RECALL                       31600022
LCBCTLMD EQU   X'40' .                  LINE IN CONTROL MODE            32000020
LCBCVRSP EQU   LCBCTLMD .               1ST BSC OUTPUT CONVERSE BLOCK   32200020
LCBOCNI  EQU   X'20' .                  NON-IMEDIATE OPERATOR CONTROL   32400020
*                                       OPERATION IN PROGRESS           32800020
LCBCHNLN EQU   X'20' .                  SWITCH CHAN IN PROGRESS  S22024 33000022
LCBINITN EQU   X'10' .                  RECEIVING INITIATE MODE MSG     33200020
LCBINITF EQU   X'FF'-LCBINITN .         NO INITIATE MODE                33600020
LCBIPLPG EQU   X'10' .                  3705 IPL IN PROGRESS     S22024 33800022
LCBCONT  EQU   X'08' .                  CONTINUE OR RESET OPERATION     34000020
LCBFREEN EQU   X'04' .                  LINE FREE                       34400020
LCBFREEF EQU   X'FF'-LCBFREEN .         LINE NOT FREE                   34800020
LCBRECVN EQU   X'02' .                  LINE IS RECEIVING               35200022
LCBSENDN EQU   X'01' .                  ON = LINE SENDING               35600020
*                                       BOTH SEND AND RECEIVE BITS      36000020
*                                       OFF INDICATE LINE IS STOPPED    36400020
*                                                                       36800020
LCBSTAT2 DS    XL1 .                    SECOND STATUS BYTE              37200020
*                        BIT DEFINITIONS                                37600020
LCBTRACE EQU   X'80' .                  I/O TRACE ACTIVE FOR THIS LINE  38000020
LCBLOCK  EQU   X'80' .                  LINE IN LOCK MODE               38400020
LCBQUCKN EQU   X'80' .                  QUICK DEACT IN PROGRESS  S22024 38600022
LCBTRCOF EQU   X'FF'-LCBTRACE .         TRACE NOT ACTIVE                38800020
LCBMSGNN EQU   X'40' .                  MSGEN/STARTUP MESSAGE           39200020
LCBMSGNF EQU   X'FF'-LCBMSGNN .         NOT MSGEN/STARTUP MESSAGE       39600020
LCBBEOTN EQU   X'20' .                  EOT FROM BFRED TERM -NO EOM     40000020
LCBBEOTF EQU   X'DF' .                  REGULAR EOM IF EOT              40400020
LCBSNDPR EQU   X'10' .                  SEND PRIORITY SWITCH SET BY     40800020
*                                       SEND SCHEDULER                  41200020
LCBNEGRP EQU   X'08' .                  NEGATIVE RESPONSE TO POLLING    41600022
*                                       RECEIVED                        42000020
LCBACDAC EQU   X'08' .                  ACTIV/DEACT IN PROGRESS  S22024 42200022
LCBSYNC  EQU   X'04' .                  LINE IS BISYNC                  42400020
LCBATTN  EQU   X'04' .                  3705 ATTENTION RECEIVED  S22024 42600022
LCBDIAL  EQU   X'02' .                  DIAL LCB                        42800020
LCBBFRAV EQU   X'02' .                  3705 BUFFER AVAILABLE    S22024 43000022
LCBRESP  EQU   X'01' .                                                  43200020
LCBSLOWN EQU   X'01'                    3705 SLOWDOWN INDICATOR  S22024 43400022
*                                                                       43600020
LCBTSTSW DS    XL1 .                    TEST-AND-SET SWITCH             44000020
*                        BIT DEFINITIONS                                44400022
LCBCONCT EQU   X'80' .                  CONNECTION ESTABLISHED          44800020
*                                                                       45000022
LCBRECAD DS    AL3 .                    ADDRESS CURRENT MSG BLOCK       45200020
LCBERB   DS    0A .                     ERB                             45600020
LCBERBKY DS    XL1 .                    ELEMENT REQUEST BLOCK KEY       46000020
LCBERBQB DS    AL3 .                    ERB QCB                         46400020
LCBERBPY DS    XL1 .                    ERB PRIORITY                    46800020
LCBERBLK DS    AL3 .                    ADDR NEXT ITEM IN CHAIN         47200020
LCBERBST DS    XL1 .                    STATUS OF ERB                   47600020
*                                       THE X'08' BIT MUST NEVER BE ON  48000020
*                                       IN THE STATUS BYTE. IT MUST     48400020
*                                       REMAIN THE 9TH BYTE OF YHE ERB  48800020
*                        BIT DEFINITIONS                                49200020
LCBINQ   EQU   X'10' .                  ERB IS WAITING-BFRS FROM HM     49400020
LCBEOMSG EQU   X'40' .                  END OF MESSAGE READ FROM DISK   49600020
LCBRDERR EQU   X'20' .                  LOGICAL READ ERROR              50000020
LCBRDERF EQU   X'FF'-LCBRDERR .         NO READ ERROR                   50400020
LCBMSG   EQU   X'80' .                  END OF INITIATE MODE TO HM      50600020
LCBERROR EQU   X'04' .                  ERROR ON SEND SIDE              50800020
LCBPRCPG EQU   X'02' .                  AFTER INITIAL REQUEST IS        51200020
*                                       SATISFIED, ERB WILL BE POSTED   51600020
*                                       TO QCB INDICATED IN LCBRCQCB    52000020
LCBCOMPL EQU   X'02' .                  DISK REQUEST IS COMPLETE        52400020
LCBDLNKN EQU   X'01' .                  DELINK SWITCH ON-ERB NOT POSTED 52800020
*                                       IF ON THE ERB CAN BE POSTED     53200020
LCBDLNKF EQU   X'FE' .                  ERB POSTED                      53600020
*                                       PCI CAN NOT POST ERB            54000020
*                                                                       54200022
LCBERBCH DS    AL3 .                    ADDR CHAIN ASSIGNED BFRS        54400020
LCBERBCT DS    H .                      COUNT FIELDS                    54800022
LCBTTCIN DS    H .                      TERM CURR CONNECTED INDEX       55200020
LCBMSGFM DS    XL1 .                    BITS TO CONTROL BSC LINE        55600020
*                        BIT DEFINITIONS                                55610022
LCBNAK   EQU   X'80' .                  REQUEST TO SEND NAK RESPONSE    55630020
LCBACKI  EQU   X'40' .                  ACK COUNTER - SEND/RECEIVE      55660020
*   FOLLOWING TWO BITS  INDICATE WHETHER A SCAN OF LINE CONTROL         55690020
*   HAS BEEN ACCOMPLISHED AND TYPE OF LINE CONTROL RECEIVED.            55720020
LCBVSTRT EQU   X'20' .                  VALID START SEQUENCE            55750020
LCBRSTRT EQU   X'10' .                  ERROR START SEQUENCE            55780020
LCBTTD   EQU   X'08' .                  TTD RECEIVED                    55810020
LCBENQ   EQU   X'04' .                  ENQ RECEIVED                    55840020
LCBEOT   EQU   X'02' .                  EOT FIRST CHARACTER             55870020
LCBOLT   EQU   X'01' .                  TOTE REQUEST                    55900020
*                                                                       56200022
LCBLTRCA DS    0AL3 .                   3705 LINE-TRACE TBL ADDR S22024 56260022
LCBLTRCF EQU   X'01' .                  3705-LINE-TRACE-INACTIVE S22024 56320022
*                                       FLAG (IN LOW-ORDER       S22024 56380022
*                                       BYTE OF LCBLTRCA)        S22024 56440022
LCBSCBA  DS    AL3 .                    ADDRESS OF CURRENT SCB          56500022
LCBERMSK DS    XL1 .                    ERROR RECORDING MASK            56800020
LCBOBRCH DS    0AL3 .                   3705 OBR/SDR/MDR CHAIN   S22024 57000022
LCBINVPT DS    AL3 .                    ADDR CURR ENTRY IN INV LIST     57200020
LCBCOREQ DS    0CL12 .                  3705 OUTPUT CORE QUEUE   S22024 57600022
LCBTPCD  DS    0CL12 .                  TP OP CODES              S22024 57700022
LCBBFRSV DS    A .                      ADDRESS OF STOP= BUFFER  S22024 57800022
LCBWRTSV DS    CL8 .                    SAVED LAST 8 BTU BYTES   S22024 57900022
LCBSNSV  DS    XL1 .                    SAVE AREA FOR SENSE BYTE        58000020
LCBCSWSV DS    XL7 .                    SAVE AREA FOR CSW               58400020
LCBERCCW DS    3D .                     3 ERP COMMANDS                  58800020
LCBSTICS EQU   LCBERCCW+21 .            CHRACTERISTCIS WORK AREA        59200020
LCBCPA   DS    0D .                     CHANNEL PROGRAM AREA            59600020
LCBERBFS EQU   LCBERBKY-IEDQLCB .       OFFSET INTO LCB OF ERB          60400020
*                                                                S99228 60410022
*                                                                S99228 60420022
*        DSECT FOR LCB EXTENSION AT TOP OF CHANNEL PROGRAM AREA  S99228 60430022
*                                                                S99228 60440022
IEDQLCBX DSECT                          LCB EXTENSION            S99228 60450022
LCBXCON  DS    0D                       START OF CONTROL FIELDS  S99228 60460022
LCBXFLAG DS    XL1                      DEV DEP FLAGS            S99228 60470022
LCBGPCTV EQU   X'08'                    ETX RECD GENERAL POLL    S99228 60480022
LCBGPSTP EQU   X'FF'-X'08'              GENERAL POLL RESET       S99228 60490022
LCBSRCPF EQU   X'04'                    SOURCE DETERM PERFORMED  S99228 60500022
LCBSRSTP EQU   X'FF'-X'04'              SOURCE DETERM RESET      S99228 60510022
LCBERPND EQU   X'02'                    SOH%R MESSAGE PENDING    S99228 60520022
LCBERSTP EQU   X'FF'-X'02'              ERP POLL RESET           S99228 60530022
LCBXDCT  DS    XL3                      DCT STORAGE AREA         S99228 60540022
LCBDCT1  EQU   LCBXDCT                  DCT BYTE 1               S99228 60550022
LCBDCT2  EQU   LCBXDCT+1                DCT BYTE 2               S99228 60560022
LCBDCT3  EQU   LCBXDCT+2                DCT BYTE 3               S99228 60570022
LCBXRADR DS    F                        ERP POLL CHARS ADDRESS   S99228 60580022
LCBPCIRC EQU   LCBXRADR                 3270 LOCAL PCI RETRY CNTRS99228 60590022
LCBERADR EQU   LCBXRADR+1               POLL CHARS ADDR FIELD    S99228 60600022
IEDQLCB  DSECT                                                   S99228 60700022
*                                                                       60800020
*                                                                       61200020
*         *******************************************************       61600020
*     0   *   KEY    **                    QCBA                 *       62000020
*         *******************************************************       62400020
*     4   *   PRI    **                    LINK                 *       62800020
*         *******************************************************       63200020
*     8   *   RSKEY  **                        STCBA            *       63600020
*         *******************************************************       64000020
*    12   *   RSPRI  **                        RSLNK            *       64400020
*         *******************************************************       64800020
*    16   *         EOLDT          **    TDL     **    TSOB     *       65200020
*         *******************************************************       65600020
*    20   *   CHAIN  **                        INSRC            *       66000020
*         *******************************************************       66400020
*    24   *   NTXT   **                     SCBDA               *       66800020
*         *******************************************************       67200020
*    28   *   ISZE   **                  FSBFR/LSBFR            *       67600020
*         *******************************************************       68000020
*    32   *   FLAG1  **   FLAG2    **   SENS0    **   SENS1     *       68400020
*         *******************************************************       68800020
*    36   *   ECBCC  **                        ECBPT            *       69200020
*         *******************************************************       69600020
*    40   *   FLAG3  **                         CSW             *       70000020
*         *******************************************************       70400020
*    44   *                       CSW                           *       70800020
*         *******************************************************       71200020
*    48   *   SIOCC  **                        START            *       71600020
*         *******************************************************       72000020
*    52   *                      DCBPT                          *       72400020
*         *******************************************************       72800020
*    56   *                   RCQCB/RESTR                       *       73200020
*         *******************************************************       73600020
*    60   *        INCAM           **       ERRCT/TTBIN         *       74000020
*         *******************************************************       74400020
*    64   *   UCBX   **                    RCBFR/LSPCI          *       74800020
*         *******************************************************       75200020
*    68   *         RECOF          **          STATE            *       75600020
*         *******************************************************       76000020
*    72   *   TSTSW  **                 RECAD                   *       76400020
*         *******************************************************       76800020
*    76   *   ERBKY  **                 ERBQB                   *       77200020
*         *******************************************************       77600020
*    80   *   ERBPY  **                 ERBLK                   *       78000020
*         *******************************************************       78400020
*    84   *   ERBIN  **                 ERBCH                   *       78800020
*         *******************************************************       79200020
*    88   *        ERBCT           **          TTCIN            *       79600020
*         *******************************************************       80000020
*    92   *  MSGFM   **                        SCBA             *       80400020
*         *******************************************************       80800020
*    96   *   ERMSK  **                        INVPT            *       81200020
*         *******************************************************       81600020
*   100   *                      TPCD                           *       82000020
*         *******************************************************       82400020
*   104   *                      TPCD                           *       82800020
*         *******************************************************       83200020
*   108   *                      TPCD                           *       83600020
*         *******************************************************       84000020
*   112   *   SNSV   **                        CSWSV            *       84400020
*         *******************************************************       84800020
*   116   *                      CSWSV                          *       85200020
*         *******************************************************       85600020
*   120   *                      ERCCW                          *       86000020
*         *******************************************************       86400020
*   124   *                                                     *       86800020
*         *******************************************************       87200020
*   128   *                                                     *       87600020
*         *******************************************************       88000020
*   132   *                                                     *       88400020
*         *******************************************************       88800020
*   136   *                                                     *       89200020
*         *******************************************************       89600020
*   140   *                                                     *       90000020
*         *******************************************************       90400020
*                                                                       90800020
         MEND  , */                                                     91200022
*        %GOTO BSLQCB;                                               /* 00200022
         MACRO                                                          00500020
         TQCBD                                                          01000020
*        UPDATED 01/05/73                                        S22024 01500000
.*       CONVERTED TO A BILINGUAL DSECT 07/19/72                 S22024 01700022
.* CHANGE ACTIVITY = AS FOLLOWS:                                        02000122
.*D015000,825000                                                SA51078 02000222
.*C820000,905000                                                SA51078 02000322
.*A024500                                                       SA51078 02000422
.*A087000,125000                                                 S22029 02020022
.*C015000                                                        S22029 02040022
.*D230000                                                        S22025 02060022
.*C015000                                                        S22025 02120022
.*A020000,6950000                                                       02200021
.*D110000-125000                                                 S22026 02300022
.*A087000,110000-125000,337000,382000-384000,497000              S22026 02400022
.*A000000,055000,445000,936000                                   S22024 02405022
.*C935000,020000                                                 S22024 02410022
.*D265000-270000                                                 S22024 02415022
*                                                                S22024 02420022
*        THIS IS A PREFIX TO THE QCB FOR 3705 SUPPORT            S22024 02425022
*                                                                S22024 02430022
IEDNQCB  DSECT                                                   S22024 02435022
QCBSTAT1 DS    XL1 .                    FLAG BYTE                S22024 02440022
QCBPLCBN EQU   X'80' .                  ON - FIELD CONTAINS PLCB ADDR   02445022
*                                       OFF - CONTAINS TTCIN     S22024 02450122
QCBPLCBF EQU   X'FF'-QCBPLCBN .                                  S22024 02455022
QCBNIDLE EQU   X'40'                    ON - LINE IS ACTIVE      S22024 02460022
QCBCKPRN EQU   X'20'                    CHECKPOINT RECORD NUMBER        02465022
QCBCKPSI EQU   X'10'                    REPLACE SESSION INITIATION      02470022
*                                       INFORMATION RECORD NUMBER       02475022
QCBOCNI  EQU   X'08'                    ON - STOPLINE IN PROGRESSS22024 02480022
QCBFLUSH EQU   X'04'                    DEACT FLUSH IN PROCESS   S22024 02482000
QCBPLCBC EQU   X'02'                    CTERM WITH PLCB ASSIGNED S22024 02483000
*                                       TO TERMINAL'S QCB        S22024 02484000
QCBSTMM  EQU   X'01'                    ON - STOP-MID-MSG        S22024 02484300
*                                       IN PROGRESS              S22024 02484600
QCBPLCBA DS    0AL3 .                   ADDR OF PLCB             S22024 02485022
         DS    XL1 .                                             S22024 02490022
QCBTTCIN DS    H .                      TNT INDEX                S22024 02495022
*                                                                       02500020
*                                                                       03000020
*        THIS IS A DSECT OF THE MASTER QCB FOLLOWED BY THE PRIORITY     03500020
*        QCB. THERE IS A MASTER QCB FOR EVERY MESSAGE QUEUE. THERE      04000020
*        IS A PRIORITY QCB FOR EACH PRIORITY LEVEL APPLICABLE FOR       04500020
*        THIS DESTINATION QUEUE.                                        05000020
         SPACE                                                          05500020
IEDQQCB  EQU   *                                                 S22024 05700022
QCBDSFLG DS    XL1 .                    FLAGS INDICATING A QCB FOR      06000020
*                                         DISPATCHER AND WHICH DATA     06500020
*                                         SET(S) THE MESSAGES FOR       07000020
*                                         THIS DEST. ARE QUEUED ON      07500020
*                        BIT DEFINITIONS                                08000020
QCBHELD  EQU   X'01' .                  STOP SENDING                    08200020
QCBFQCB  EQU   X'02' .                  FLAG INDICATING A QCB           08500020
QCBDRQQ  EQU   X'04' .                  FLAG INDIC A DRQ         S22026 08700022
QCBALTMH EQU   X'08'  .                 MSGS GO TO ALTERNATE MH  S22029 08800022
QCBREUS  EQU   X'10' .                  FLAG FOR REUSEABLE DISK Q.      09000020
QCBNREUS EQU   X'20' .                  FLAG FOR NONREUS. DISK Q.       09500020
QCBDISK  EQU   X'30' .                  DISK QUEUES USED                10000020
QCBCORE  EQU   X'40' .                  FLAG FOR MS QUEUES.             10500020
*              X'50'                    INDICATES CORE Q'S WITH         11000022
*                                       BACKUP ON REUSABLE DISK         11500022
*              X'60'                    INDICATES CORE Q'S WITH         12000022
*                                       BACKUP ON NONREUS DISK          12500022
QCBTSQ   EQU   X'80'  .                 TIME-SHARING QUEUES      S22029 12700022
*                                                                       13000020
QCBELCHN DS    AL3 .                    ELEMENT CHAIN                   13500020
*              CONTAINS THE QCB ADDRESS TO BE POSTED TO WHEN THIS       14000020
*              QCB IS REMOVED FROM THE TIME DELAY QUEUE.                14500020
QCBPRI   DS    XL1 .                    PRIORITY                        15000020
QCBLINK  DS    AL3 .                    POINTER TO NEXT STCB IN CHAIN   15500020
QCBSTVTO DS    AL1 .                    INDEX TO THE ENTRY IN THE       16000020
*                                         SUBTASK VECTOR TABLE          16500020
QCBSTCHN DS    AL3 .                    STCB CHAIN                      17000020
QCBSTPRI DS    XL1 .                    PRIORITY OF THE STCB            17500020
QCBCLSTR DS    0AL3 .                   CLUSTER ENTRY ADDR FOR   S22024 17600022
*                                       3705 GENERAL POLL        S22024 17700022
QCBSLINK DS    AL3 .                    POINTER TO NEXT STCB IN CHAIN   18000020
QCBRQCHN DS    0AL3 .                   CLUSTER REQUEST CHAIN    S22024 18100022
*                                       FOR 3705 GENERAL POLL    S22024 18200022
QCBEOLDT DS    XL2 .                    INTERRUPT TIME                  18500020
QCBRETCT DS    0XL1 .                   TSO RETRY COUNTERS          TSO 19000020
QCBLKRLN DS    XL1 .                    LOCK RELATIVE LINE NO.          19500020
*              OFFSET TO QCB FOR TIME DELAY - FOR QCB = X'00'           20000020
QCBSTAT  DS    XL1 .                    STATUS OF THIS QCB              20500020
*                        BIT DEFINITIONS                                21000020
QCBEOM   EQU   X'80' .                  END OF MESSAGE SENT             21500020
QCBTRMHO EQU   X'40' .                  TERMINAL WAS HELD               21700022
QCBBUFRD EQU   X'20' .                  BUFFERED TERMINAL               22000020
QCBSEND  EQU   X'10' .                  SENDING TO BUFFERED TERMINAL    22500020
QCBSCHDL EQU   X'04' .                  PUT IN DELAY Q WHEN INACTIVE    23500020
QCBCLOCK EQU   X'02' .                  ON = CLOCK, OFF = INTVL         24000020
QCBTIME  EQU   X'01' .                  DELAY GREATER THAN 12 HOURS     24500020
*                                                                       25000020
QCBSCBOF DS    XL1 .                    OFFSET TO THE PROPER SCB        25500020
*                                         FOR THIS TRANSMISSION.        26000020
QCBINSRC DS    0AL3 .                   CHAIN OF SOURCE LCB'S           27500020
*                                         CURRENTLY SENDING INITIATE    28000020
*                                         MODE MSGS TO THIS DEST. Q.    28500020
QCBSATCT DS    XL1 .                    SIM ATTN OUTPUT LINE COUNT  TSO 29000020
QCBTSOF2 DS    XL1 .                    SECOND TSO FLAG BYTE        TSO 29500020
*                        BIT DEFINITIONS                            TSO 30000020
QCBINHBN EQU   X'80' .                  USE INHIBITS WITH THIS TERM TSO 30500020
QCBBUFQ  EQU   X'40' .                  TCAM BUFFER BEING HELD      TSO 31000020
QCBPOSTO EQU   X'20' .                  QCB POSTED TO ITSELF        TSO 31500020
QCBDSSMI EQU   X'10' .                  START MI CHARACTER SENT     TSO 31700022
QCBSIMRD EQU   X'08' .                  SIMATTN READ EXECUTING      TSO 32100020
QCBSATCH EQU   X'04' .                  SIMULATED ATTN BY CHARACTER TSO 32500020
QCBSATTI EQU   X'02' .                  SIMULATED ATTN BY TIME      TSO 33000020
QCBSATLC EQU   X'01' .                  SIMULATED ATTN BY LINE      TSO 33500020
*                                                                       33700022
QCBTSOF1 DS    XL1 .                    FIRST TSO FLAG BYTE         TSO 34000020
*                        BIT DEFINITIONS                            TSO 34500020
QCBWRBRK EQU   X'80' .                  ISSUE A WRITE BREAK         TSO 35000020
QCBTGET  EQU   X'40' .                  TGET REQUEST                TSO 35500020
QCBTPUT  EQU   X'20' .                  TPUT REQUEST                TSO 36000020
QCBNOBUF EQU   X'10' .                  INSUFFICIENT BUFFERS        TSO 36500020
QCBSATRD EQU   X'08' .                  SIMULATED ATTN READ REQUEST TSO 37000020
QCBPARTO EQU   X'04' .                  PARTIAL OUTPUT LINE         TSO 37500020
QCBDELAY EQU   X'02' .                  QCB IN DELAY QUEUE              38000020
QCBDISC  EQU   X'01' .                  USER TO BE LOGGED OFF       TSO 38200022
*                                                                       38300022
QCBEXTO  DS    0H .                     OFFSET TO EXT            S22026 38400022
QCBINTVL DS    XL2 .                    INTERVAL FOR POLL DELAY         38500020
QCBMSGCT DS    H .                      COUNT OF MESSAGES IN THIS       39000020
*                                         QUEUE                         39500020
QCBPREN  DS    0A .                     ADDRESS OF TERMINAL TABLE       40000020
*                                       ENTRY IF QCB FOR A PROCESS      40500020
*                                       ENTRY                           41000020
QCBPRLVL DS    XL1 .                    HIGHEST PRIORITY LEVEL MESSAGE  41500020
QCBLKRRN DS    0XL3 .                   LOCK RELATIVE RECORD NUM.       42000020
*              LINK FIELD FOR QCB WHEN ON THE TIME DELAY QUEUE          42500020
QCBCARCT DS    XL1 .                    CARRIAGE POSITION COUNT     TSO 43000020
QCBTJID  DS    H .                      TSO JOB IDENTIFICATION      TSO 43500020
QCBRELLN DS    XL1 .                    RELATIVE LINE NO. FOR THE       44000020
*                                         LINE THIS QCB REPRESENTS      44500020
QCBLGBAD DS    0AL3 .                   ADDRESS OF LGB           S22024 44700022
QCBDCBAD DS    AL3 .                    ADDRESS OF DCB                  45000020
QCBFLAG  DS    XL1 .                    QCB STATUS BITS                 45500020
*                        BIT DEFINITIONS                                46000020
QCBTSSES EQU   X'80' .                  TSO SESSION IN PROGRESS     TSO 46500020
QCBNOBRK EQU   X'40' .                  NO REVERSE BREAK FEATURE    TSO 47000020
QCBREAD  EQU   X'20' .                  READ HAS PRIORITY           TSO 47500020
QCBRSRV  EQU   X'10' .                  REUSE SERVICED BIT              48000020
QCBTERMQ EQU   X'08' .                  QUEUING BY TERMINAL             48200022
QCBSDFFO EQU   X'04' .                  CURRENTLY SENDING FEFO MSG      48500020
QCBPROC  EQU   X'02' .                  THIS QCB FOR A PROCESS ENTRY    49000020
QCBCKPT  EQU   X'01' .                  FLAG FOR CHECKPOINT             49500020
*                                                                       49700022
QCBQBACK DS    AL3 .                    QBACK MESSAGE CHAIN             50000020
         SPACE 2                                                        50500020
*     DEC                                 HEX                           51000020
*        *********************************                              51500020
*      0 * DSFLG *         ELCHN         *                              52000020
*        *********************************                              52500020
*      4 * PRI   *         LINK          *  4                           53000020
*        *********************************                              53500020
*      8 *  VTO  *         STCHN         *  8                           54000020
*        *********************************           **************     54500020
*     12 * STPRI *         SLINK         *  C        *            *     55000020
*        *********************************           * MASTER QCB *     55500020
*     16 *    EOLDT      * LKRLN * STAT  * 10        *                  56000020
*        *********************************           **************     56500020
*     20 * SCBOF *         INSRC         * 14                           57000020
*        *********************************                              57500020
*     24 *    INTVL      *     MSGCT     * 18                           58000020
*        *********************************                              58500020
*     28 * PRILVL*     PREN/LKRRN        * 1C                           59000020
*        *********************************                              59500020
*     32 * RELLN *      DCBAD/LGBAD      * 20                    S22024 60000022
*        *********************************                              60500020
*     36 * FLAG  *         QBACK         * 24                           61000020
*        *********************************                              61500020
         SPACE                                                          62000020
*                                                                       62500020
QCBMEND  EQU   * .                                                      63000020
QCBMSIZE EQU   QCBMEND-IEDQQCB .        SIZE OF MASTER QCB              63500020
*                                                                       64000020
         SPACE 2                                                        64500020
*                                                                       65000020
*        THIS IS THE DSECT OF A PRIORITY QCB.  THERE IS A PRIORITY      65500020
*        QCB FOR EACH PRIORITY LEVEL APPLICABLE FOR THIS DEST. Q        66000020
*                                                                       66500020
IEDQPQCB EQU   * .                 START OF PRI LEVEL QCB               67000020
QCBDNHDR DS    XL3 .                    DISK RECORE NUMBER TO PUT       67500020
*                                         THE NEXT HDR RECEIVED         68000020
QCBFHDLZ DS    0XL3 .                   DISK RECORD             SA52971 68500022
QCBDATFL DS    X .                     DATFLAGS FIELD OF LAST    S21101 69600021
*                                      MSG REMOVED FROM FEFO Q   S21101 69650021
QCBPFEFO DS    0XL3 .                   IF TERM OF QUEUE HELD,  SA52971 69700022
*                                         PREVIOUS TO FIRST     SA52971 69710022
*                                         HELD MESSAGE          SA52971 69720022
*                                       IF TERM NOT HELD,       SA52971 69730022
*                                         PREVIOUS TO LAST      SA52971 69740022
         DS    XL2                        MESSAGE SERVICED      SA52971 69750022
QCBFHDTZ DS    0XL3 .                   DISK RECORD             SA52971 69760022
         DS    XL1 .                    PFEFO CONTINUED         SA52971 69770022
QCBDATSQ DS    XL2 .                    SEQUENC NUMBER OF LAST   S21101 69800021
*                                       MSG REMOVED FROM FEFO Q  S21101 69850021
QCBINTFF DS    XL3 .                    DISK REC.NO. OF THE FIRST       72000020
*                                         INTERCEPTED MSG. - FEFO       72500020
*                                         ORDER                         73000020
QCBINTLF DS    0XL3 .                   LAST INTERCEPTED MSG    SA52971 73500022
QCBPREVF DS    XL3 .                    PREVIOUS TO LAST FFEFO  SA52971 74000022
QCBFFEFO DS    XL3 .                    DISK REC. NO. OF THE FIRST      74500020
*                                         (FEFO) MSG. TO BE RECVD.      75000020
*                                         CORE RECORD NO. IF THIS       75500020
*                                         IS A CORE ONLY QUEUE.         76000020
QCBLFEFO DS    XL3 .                    DISK RECORD NO. OF THE          76500020
*                                         LAST FEFO MSG. RECEIVED.      77000020
*                                         CORE REC. NO. IF THIS IS      77500020
*                                         A CORE ONLY QUEUE.            78000020
QCBCFHDR DS    XL3 .                    CORE REC. NO. OF THE FIRST      78500020
*                                         HEADER APPEARING IN THIS      79000020
*                                         QUEUE.                        79500020
QCBPRIPQ DS    XL1 .                    THE PRIORITY OF THIS PRITY      80000020
*                                         LEVEL QCB.  THIS WILL BE      80500020
*                                         X'00' IF THIS IS THE          81000020
*                                         LOWEST PRTY LEVEL.            81500020
QCBPQBCK DS    XL3 .                    PQCB BACK CHAIN         SA51078 82000022
         SPACE 2                                                        83000020
*     DEC                                 HEX                           83500020
*        *********************************                              84000020
*      0 *         DNHDR         * DATFL *  0                   SA52971 84500022
*        *********************************                              85000020
*      4 *         PFEFO         * DATSQ *  4                   SA52971 85500022
*        *********************************         ****************     86000020
*      8 *(DATSQ)*         INTFF         *  8      *            SA52971 86500022
*        *********************************         * PRIORITY QCB *     87000020
*     12 *         PREVF         * FFEFO *  C      *            SA52971 87500022
*        *********************************         ****************     88000020
*     16 *    (FFEFO)    *     LFEFO     * 10                           88500020
*        *********************************                              89000020
*     20 *(LFEFO)*         CFHDR         * 14                           89500020
*        *********************************                              90000020
*     24 * PRIPQ *         PQBCK         * 18                   SA51078 90500022
*        *********************************                              91000020
*                                                                       91500020
QCBPEND  EQU   *                                                        92000020
QCBPSIZE EQU   QCBPEND-IEDQPQCB                                         92500020
*                                                                       93000020
         MEND  , */                                                     93500022
         PRINT ON                                                  @L01
         TITLE 'IGC0009D - FIRST LOAD OF SVC 94'                        00800020
IGC0009D CSECT                                                          01600020
*0896749650-749860,750245,750260-750266,750563-750624            M0167  01602021
*0896322000-328000,346000-348000,392000,748360-748390,748960-    A45625 01605021
*0896749080,749680,750160,750220,750250-750490,750572,750582-    A45625 01610021
*0896750584,750620                                               A45625 01615021
*0896750606,750650                                               M0121  01620021
*0896750598,750606,750650                                        M0090  01650021
*0896750598,750606,750650                                        M0061  01700021
*0896750575-750670                                               S21008 01800021
*0000438300,488000,502000,746000-748000,896000-999999            M0029  02000020
*0000749830                                                      M1062  02200020
*                                                                 M4034 02300020
*                                                                M4283  02350020
*                                                                 M5113 02370020
*                                                                 M5467 02570020
*********************************************************************** 02770020
*********************************************************************** 03170020
*                                                                     * 03570020
*STATUS CHANGE LEVEL 002                                                03970021
*                                                                     * 04370020
* FUNCTION - THIS MODULE IS THE FIRST LOAD OF SVC 94. ITS FUNCTION IS * 04770020
*    TO VERIFY THE ENTRY CODE AND XCTL TO THE PROPER MODULE FOR THE   * 05170020
*    MACRO ISSUED. AN EXCEPTION IS THE TCLEARQ MACRO WHOSE FUNCTIONS  * 05570020
*    OF CLEARING EITHER THE TSO INPUT OR OUTPUT BUFFER QUEUES IS      * 05970020
*    PERFORMED IN THIS MODULE VIA BRANCH ENTRIES TO THE QTIP ATTENTION* 06370020
*    MODULE,IEDAYAA.  BEFORE XCTL TO OTHER LOADS OF SVC 94, THIS MODUL* 06770020
*    MOVES THE ENTRY CODE AND PARAMETERS IN REGISTER 0 TO REGISTER 2. * 07170020
*    SUBSEQUENT LOADS SHOULD EXAMINE REGISTER 2 FOR ENTRY PARAMETERS. * 07570020
*                                                                     * 07970020
* ENTRY POINTS -                                                      * 08370020
*         IGC0009D - CONTROL IS RECEIVED WHEN SVC 94 IS ISSUED.       * 08770020
*                                                                     * 09170020
* INPUT -                                                             * 09570020
*    REG 0 - ENTRY CODE IN HIGH ORDER BYTE,PARAMETERS IN REMAINDER    * 09970020
*    REG 1 - PARAMETERS                                               * 10370020
*    REG 3 - CVT                                                      * 10770020
*    REG 4 - TCB                                                      * 11170020
*    REG 5 - SVRB                                                     * 11570020
*                                                                     * 11970020
* OUTPUT - RETURN CODE IN REGISTER 15                                 * 12370020
*                                                                     * 12770020
* EXTERNAL REFERENCES -                                               * 13170020
*         QTIP (IKJGGQT1) WITH ENTRY CODE 21 (CLEAR INPUT QUEUE) OR   * 13570020
*         ENTRY CODE 22 (CLEAR OUTPUT QUEUE)                          * 13970020
*                                                                     * 14370020
* EXITS - NORMAL                                                      * 14770020
*         ALL EXCEPT TCLEARQ - XCTL TO NEXT LOAD                      * 15170020
*         TCLEARQ - SVC 3                                             * 15570020
*                                                                     * 15970020
* EXITS - ERROR                                                       * 16370020
*         SVC 3 WITH RETURN CODE IN REGISTER 15                       * 16770020
*                                                                     * 17170020
* TABLES AND WORK AREAS - SEE DSECTS AT END OF MODULE                 * 17570020
*                                                                     * 17970020
* ATTRIBUTES - REENTRANT,DISABLED                                     * 18370020
*                                                                     * 18770020
* CHARACTER CODE DEPENDENCY - NONE                                    * 19170020
*                                                                     * 19570020
* NOTES - NONE                                                        * 19970020
*                                                                     * 20070000
* Change activity      =                                              * 20170000
*                                                                     * 20180000
*   Flag  Date        By    Description                               * 20190000
*   ----  ----------  ----  ----------------------------------------  * 20200000
*   $L01  2010/04/15  KL    Add support for function codes 18 and 19. * 20470000
*                                                                     * 20370020
*********************************************************************** 20770020
*********************************************************************** 21170020
         SPACE 3                                                        24800020
********                                                                25600020
******** REGISTER EQUATES                                               26400020
********                                                                27200020
RZERO    EQU   0                        ENTRY CODE/WORK                 28000020
RONE     EQU   1                        PARAMETER                       28800020
RSAVE    EQU   2                        SAVE PARAMETER                  29600020
RCVT     EQU   3                        POINTS AT CVT                   30400020
RTCB     EQU   4                        POINTS AT TCB                   30600020
RLCB     EQU   4                        POINTS TO LCB                   30800020
RSVRB    EQU   5                        POINTS AT SVRB                  31200020
RTJB     EQU   5                        POINTS TO TJB                   31600020
REPLOC   EQU   6                        CONTAINS ENTRY NAME ADDR        32000020
RTSB     EQU   6                        POINTS TO TSB            A45625 32400021
RQCB     EQU   7                        POINTS TO QCB            A45625 32800021
RAVT     EQU   8                        POINTS TO AVT            A45625 33200021
RWORK    EQU   9                        WORK REGISTER                   33600020
RTSCVT   EQU   10                       POINTS TO TSCVT                 33800020
R10      EQU   10                       WORK REGISTER            A45625 33900021
RWORK1   EQU   11                       WORK REG                        34000020
RBASE    EQU   12                       BASE REGISTER                   34400020
R13      EQU   13                       WORK REGISTER            A45625 34600021
R14      EQU   14                       WORK REGISTER            A45625 34800021
R15      EQU   15                       POINTS AT PARMLIST              35200020
RCODE    EQU   15                       CONTAINS RETURN CODE            36000020
REVEN    EQU   2                        *MULTIPLICATION                 36500020
RODD     EQU   REVEN+1                  * WORK REGS                     37000020
         SPACE 3                                                        37600020
********                                                                38400020
******** EQUATES FOR LENGTH AND DISPLACEMENT                            39200021
********                                                                40000020
D0       EQU   0                        DISPLACEMENT OF 0               40200020
D1       EQU   1                        DISPLACEMENT OF 1               40400020
D2       EQU   2                        DISPLACEMENT OF 2               40600020
D4       EQU   4                        DISPLACEMENT OF 4        A45625 40660021
D5       EQU   5                        DISPLACEMENT OF 5        A45625 40720021
D6       EQU   6                        DISPLACEMENT OF 6               40800020
D14      EQU   14                       DISPLACEMENT OF 14              41000020
D32      EQU   32                       DISPLACEMENT OF 32              41200020
D96      EQU   96                       DISPLACEMENT OF 96              41400020
L2       EQU   2                        LENGTH OF 2                     41600020
L3       EQU   3                        LENGTH OF 3                     41800020
L32      EQU   32                       LENGTH OF 32                    42000020
L48      EQU   48                       LENGTH OF 48                    42200020
XSA      EQU   96                       SVRB XSA DISPLACEMENT           42400020
M24      EQU   24                       SHIFT LENGTH                    42600020
QINEC    EQU   21                       QTIP E.C. FOR CLEARING INPUT Q  43286020
QOUTEC   EQU   22                       QTIP E.C. FOR CLEARING OUTPUT Q 43292020
         SPACE 3                                                        43300020
********                                                                43400020
******** EQUATES FOR RETURN CODES                                       43500020
********                                                                43600020
BKGROUND EQU   X'00'                    JOB IS BACKGROUND               43700020
CODEBAD  EQU   X'04'                    INVALID PARAMETERS              43800020
TSOTCMDN EQU   X'08'                   TSO AND/OR TCAM IS NOT UP M0029  43840020
         SPACE 3                                                 A45625 43850021
********                                                                43860021
******** EQUATES FOR MASKS                                              43870021
********                                                                43880021
EXIT     EQU   3                        SVC EXIT EQUATE                 43890020
IDLE     EQU   X'17'                    EBCDIC IDLE CHARACTER           43920020
OFF      EQU   X'FF'                    RESET BITS                      43950020
HIPRI    EQU   X'E4'                    HIGH PRIORITY            A45625 43960021
COMPLETE EQU   X'40'                    MASK FOR ECB COMPLETION  A45625 43970021
         EJECT ,                                                   @L01 43990300
********                                                           @L01 43990500
******** Initialize module area to S(*) for possible               @L01 43990700
******** subsequent patching.                                      @L01 43990900
********                                                           @L01 43991100
         BALR  RBASE,RZERO              Set base register          @L01 43991300
         USING *,RBASE                  Addressability for program @L01 43991500
DUPS     EQU   ((1024-(*-IGC0009D))/2)  Build duplication factor   @L01 43991700
         DC    (DUPS)S(*)               Initialize program area    @L01 43991900
         ORG   IGC0009D                 Reset location counter     @L01 43992100
         SPACE 3                                                        44000020
********                                                                44800020
******** SET UP BASE REGISTER AND ADDRESSABILITY                        45600020
********                                                                46400020
         BALR  RBASE,RZERO              ESTABLISH ADDRESSABILITY        47200020
         USING *,RBASE                                                  48000020
         USING CVT,RCVT                                                 49600020
         USING TCB,RTCB                                                 49610020
         SSM   ENABLE                   ENABLE ALL INTERRUPS            49610220
         SPACE 3                                                        49610420
********                                                                49610820
******** IF ENTRY CODE IS 0 OR 15, SKIP CHECK FOR FOREGROUND JOB SINCE  49611220
******** BACKGROUND JOBS USE ENTRY CODES 0 AND 15                       49611620
********                                                                49612020
         LR    RWORK1,RZERO             GET ENTRY CODE IN WORK REG      49612420
         SRL   RWORK1,24                PUT ENT CODE IN LO-ORDER BYTE   49612820
         CH    RWORK1,ZERO              IS IT ENTRY CODE OF 0           49613220
         BE    SKIPCHK                  YES - SKIP BACKGROUND JOB CHK   49613620
         CH    RWORK1,FIFTEEN           IS IT ENTRY CODE OF 15          49614020
         BNE   MAKECHK                  NO - TEST FOR FOREGROUND JOB    49614420
         SPACE 3                                                        49614820
********                                                                49615220
******** ENTRY CODE IS EITHER 0 OR 15 - PREPARE TO XCTL IMMEDIATELY     49615620
********                                                                49616020
SKIPCHK  EQU   *                                                        49616420
         SRL   RZERO,24                 PUT ENT CODE WHERE XCTL EXPECTS 49616820
         LR    RWORK1,RONE              SAVE PARAMETER - REG 0 IS NOT   49617220
*                                       USED BY MODULES USING ENTRY     49617620
*                                       CODES 0 AND 15                  49618020
         B     CODEOK                   GO XCTL                         49618420
         SPACE 3                                                        49620020
********                                                                49630020
******** TEST WHETHER THIS IS A FOREGROUND JOB. IF IT IS BACKGROUND,    49640020
******** RETURN CONTROL TO CALLER                                       49650020
********                                                                49660020
MAKECHK  EQU   *                                                        49665020
         TM    TCBTSFLG,TCBTSTSK        IS THIS A FOREGROUND JOB        49672020
         BNO   BACKGRND                 BRANCH IF NO                    49680020
         SPACE 3                                                        49700020
********                                                                49800020
******** TEST WHETHER TSO IS UP, IF NOT, RETURN CONTROL TO CALLER       49900020
********                                                                50000020
         TM    CVTTSFLG,CVTTSRDY        IS TSO READY                    50100020
         BZ    NOTSOTCM                BRANCH IF NOT             M0029  50170020
         SSM   DISABLE                  DISABLE ALL INTERRUPS           50200020
         TM    CVTTCMFG,CVTTCRDY       IS TCAM READY             M0029  50240020
         BZ    NOTSOTCM                BRANCH IF NOT             M0029  50310020
         SPACE 3                                                        50400020
********                                                                51200020
******** TEST ENTRY AND XCTL TO PROPER MODULE                           52000020
********                                                                52800020
         SSM   ENABLE                   ENABLE ALL INTERRUPS            53000020
         LR    RWORK1,RONE              SAVE PARAMETERS                 53200020
         LR    RSAVE,RZERO              SAVE PARAMETER                  53600020
         SRL   RZERO,M24                SHIFT ENTRY CODE TO LOW ORDER   54400020
*                                       BYTE OF REGISTER                55200020
         CH    RZERO,ZERO               ENTRY CODE GREATER THAN 0       56000020
         BL    INVALID                  NO, GO TO INVALID               56800020
         CH    RZERO,ONE                ENTRY CODE ONE                  56900020
         BE    TCLEARQ                   YES,NO XCTL                    57000020
         CH    RZERO,TWO                                                57100020
         BE    INVALID                  BRANCH IF EQUAL TO INVALID      57200020
         CH    RZERO,THREE                                              57300020
         BE    INVALID                  IF EQUAL GO TO INVALID          57600020
         CH    RZERO,H18                Entry code == 18?          @L01 57804000
         BE    CODEOK                   Yes, valid entry code      @L01 57808000
         CH    RZERO,H19                Entry code == 19?          @L01 57812000
         BE    CODEOK                   Yes, valid entry code      @L01 57816000
         CH    RZERO,SIXTEEN            ENTRY CODE GREATER THAN 16      58000020
         BNH   CODEOK                   NO, GO TO CODEOK                58400020
         SPACE 3                                                        58800020
INVALID  EQU   *                                                        59200020
         LA    RCODE,CODEBAD            LOAD RETURN CODE                60000020
         SVC   EXIT                     RETURN CONTROL TO CALLER        60500020
         SPACE 3                                                        61000020
CODEOK   EQU   *                                                        61900020
         LA    RSVRB,D96(RSVRB)         SVRB SAVE AREA ADDRESS          62800020
         XC    D0(L48,RSVRB),D0(RSVRB)  ZERO NEEDED BYTES IN SAVE AREA  63700020
         MVC   D0(L32,RSVRB),XCTLCON    GET XCTL CONSTANT IN REG        64600020
         MH    RZERO,SIX                MULTIPLY TO GET PT TO TABLE     65500020
         LA    RWORK,IDTTR              ADDRESS OF TABLE IN REGISTER    66400020
         AR    RWORK,RZERO              PT TO PROPER ENTRY IN TABLE     67300020
         MVC   D6(L2,RSVRB),D0(RWORK)   MOVE IN NUMBER                  67700020
         MVC   D14(L3,RSVRB),D2(RWORK)  PUT ENTRY IN WORK AREA          68400020
         LA    RCODE,D32(RSVRB)         GET ADDR OF PARAMETER LIST      69200020
         LA    RWORK,D0(RSVRB)          ADDR OF LIST ENTRY IN REG       70000020
         XCTL  DE=(RWORK),SF=(E,(15))                                   70900020
         SPACE 3                                                        71800020
*                                       GIVE CONTROL TO APPROPRIATE     73600020
*                                       LOAD MODULE                     74400020
NOTSOTCM EQU   *                                                 M0029  74500020
         LA    RCODE,TSOTCMDN          TSO/TCAM DOWN RETURN CODE M0029  74600020
         SVC   EXIT                     RETURN CONTROL TO CALLER        74803020
BACKGRND EQU   *                                                        74806020
         LA    RCODE,BKGROUND           LOAD RETURN CODE                74809020
         SVC   EXIT                     RETURN CONTROL TO CALLER        74812020
         SPACE 5                                                        74815020
*                                                                       74818020
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 74821020
*                                                                       74824020
* T C L E A R Q - THE FOLLOWING CODE ACCOMPLISHES THE TCLEARQ           74827020
*                 FUNCTION, FOR BOTH INPUT AND OUTPUT                   74830020
*                                                                       74833020
*                 INPUT - R1 IS ZERO IF CLEARING OUTPUT Q IS DESIRED,   74836021
*                               MINUS IF CLEARING INPUT Q IS DESIRED.   74839021
*                                                                       74842020
*                 OUTPUT - QTIP (EC 21 AND 22) PERFORM QUEUE CLEARING   74845020
*                         FUNCTIONS.  IT EXPECTS--                      74848020
*                                                                       74851020
*                 R0 - ENTRY CODE                                       74854020
*                 R4 - LCB                                              74857020
*                 R5 - TJB                                              74860020
*                 R6 - TSB                                              74863020
*                 R7 - QCB                                              74866020
*                 R10- TSCVT                                            74869020
*                                                                       74872020
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 74875020
*                                                                       74878020
TCLEARQ  DS    0H                                                       74881020
         L     RTSCVT,CVTTSCVT          GET TSCVT                       74884020
         USING TSCVT,RTSCVT                                             74887020
         L     RWORK1,TCBJSCB           GET JSCB                        74890020
         USING IEZJSCB,RWORK1                                           74893020
         LR    R13,RSVRB                SAVE SVRB POINTER        A45625 74895021
         LH    RTJB,JSCBTJID             GET TJID                A45625 74897021
         MH    RTJB,TSCVTSZU            CALCULATE TJB OFFSET     A45625 74899021
         AL    RTJB,TSCVTTJB            FIND DESIRED TJB         A45625 74901021
         USING TJB,RTJB                                          A45625 74903021
         TM    TJBSTAT2,TJBHUNG         HAS USER HUNG UP                74909020
         BO    LEAVE                    YES, EXIT                       74910020
         SSM   DISABLE                  DISABLE ALL INTERRUPS           74910220
         TM    CVTTCMFG,CVTTCRDY        TEST IF TCAM IS UP              74910420
         BNO   NOTSOTCM                 NO,RETURN WITH CODE = 08        74910620
         L     RTSB,TJBTSB              GET TSB                         74911020
         USING TSB,RTSB                                                 74914020
         L     RQCB,TSBQCB-D1           LOCATE QCB                      74917020
*                                                                       74920020
* CALC LCB FOR THIS USER                                                74923020
*                                                                       74926020
         USING IEDQQCB,RQCB                                             74929020
         L     RWORK,QCBDCBAD-D1        LOCATE USERS DCB                74932020
         USING IHADCB,RWORK                                             74935020
         DROP  RTCB                                                     74938020
         L     RLCB,DCBIOBAD            IOB ADDRESS                     74941020
         LA    REVEN,LCBFLAG1-IEDQLCB   GET OFFSET OF IOB FROM LCB      74944020
         SR    RLCB,REVEN               POINT TO FIRST LCB              74947020
         SR    REVEN,REVEN              CLEAR REG                       74950020
         IC    REVEN,DCBEIOBX           GET SIZE LCB ENTRY              74953020
         SR    RODD,RODD                CLEAR REG                       74956020
         IC    RODD,QCBRELLN            REL POSITION OF LCB IN TABLE    74959020
         MR    REVEN,REVEN              CALC OF LCB                     74962020
         AR    RLCB,RODD                AT LAST, GET TO DESIRED  M0167  74962721
*                                       LCB                             74963421
         SPACE 1                                                 M0167  74964121
         LTR   RONE,RONE                TEST PARAMETER           M0167  74964821
         BP    INVALID                  INVALID IF POSITIVE      M0167  74965521
         SPACE 3                                                 M0167  74966221
*                                                                       74966921
* ENSURE TCAM NOT RUNNING ON OTHER CPU                                  74967621
*                                                                       74968321
         L     RCVT,CVTPTR              GET CVT POINTER          M0167  74969021
         L     RAVT,CVTAQAVT            GET PTR TO PTR TO AVT    M0167  74969721
         L     RAVT,0(,RAVT)            GET PTR TO AVT           M0167  74970421
         L     RWORK,CVTMPCVT           GET PTR TO MP CVT        M0167  74971121
         LTR   RWORK,RWORK              IF 0, NOT MP SYSTEM      M0167  74971821
         BZ    NOTMP                    NOT AN MP SYSTEM         M0167  74972521
         SPACE 1                                                 M0167  74973221
*                                                                       74973921
* MAKE TCAM NONDISPATCHABLE                                             74974621
*                                                                       74975321
         L     RSAVE,AVTTCB-IEDQAVTD(,RAVT) GET AVT PTR          M0167  74976021
         USING TCB,RSAVE                                         M0167  74976721
         OI    TCBNDSP1,TCBTPSP         SECONDARY NONDISP BIT    M0167  74977421
         OI    TCBFLGS5,TCBPNDSP        PRIMARY NONDISP BIT      M0167  74978121
*                                                                       74978821
* REMOVE TCAM FROM OTHER CPU IF RUNNING NOW. USE TAKE-REMOVAL           74979521
* ROUTINE 'TESTDISP'                                                    74980221
*                                                                       74980921
         L     R15,AVTMPTRR(,RWORK)     ADDR OF REMOVAL RTN      M0167  74981621
         BALR  R14,R15                  GO THERE                 M0167  74982321
         DROP  RSAVE                                             M0167  74983021
         SPACE 3                                                 M0167  74983721
NOTMP    EQU   *                                                 M0167  74984421
         LTR   RONE,RONE                TEST PARM AGAIN          M0167  74985121
         BM    INPUT                    NEGATIVE, CLEAR INPUT Q  M0167  74985821
         LA    RZERO,QOUTEC             QTIP EC FOR OUTPUT              74989020
         B     QTIP                     GO TO QTIP                      74992020
INPUT    DS    0H                                                       74995020
         NI    TSBFLG2,OFF-TSBBRKIN     RESET BREAKIN BIT               74998020
         NI    QCBFLAG,OFF-QCBREAD     TURN OFF QCBREAD                 75005020
         OI    TSBFLG1,TSBIFLSH        TURN ON INPUT FLUSH FOR   M0029  75006020
*                                       TSINPUT                  M0029  75007020
         OI    TSBFLG3,TSBSPIT         TELL TSOUTPUT NO          M0029  75008020
*                                       REPROMPTING              M0029  75009020
*                                                                       75013020
* TPOST THE TERMINAL DESTINATION QCB TO SYNCHRONIZE LINE                75016021
*                                                                       75019020
         OI    QCBTSOF1,QCBNOBUF        DON'T ALLOW INPUT               75023020
         OI    TSBSTAT,TSBLWAIT         TURN ON LWAIT BIT               75024020
         SPACE 3                                                 A45625 75024821
*                                                                       75025121
* HAS A PUT OPERATION ALREADY BEEN INITIALIZED                          75025421
*                                                                       75025721
         USING IEDQAVTD,RAVT                                     A45625 75026921
         TM    QCBTSOF1,QCBTPUT         IS THERE A REQUEST FOR   A45625 75027221
*                                       TPUT                            75027521
         BO    POSTTCAM                 YES, NOTHING TO DO       A45625 75027821
         OI    QCBTSOF1,QCBTPUT         INDICATE TPUT REQUEST    A45625 75028121
         SPACE 3                                                 A45625 75028421
*                                                                       75028721
* IS THIS QCB ON THE DELAY QUEUE                                        75029021
*                                                                       75029321
         TM    QCBTSOF1,QCBDELAY        IS DELAY Q BIT ON        A45625 75029621
         BZ    NODELAY                  NO, START A PUT          A45625 75029921
*                                       OPERATION                A45625 75030221
         SPACE 3                                                 A45625 75030521
*                                                                       75030821
* REMOVE QCB FROM DELAY QUEUE                                           75031121
*                                                                       75031421
         L     R14,AVTTSOPT             GET TSINPUT QCB ADDR     A45625 75031721
         USING IEDQTSI,R14                                       A45625 75032021
         TM    TSIDYQFG,TSIDYPOS        HAS QCB BEEN POSTED      A45625 75032321
         BO    POSTTCAM                 YES, BRANCH              A45625 75032621
         OI    TSIDYQFG,TSIDYPOS        INDICATE POST            A45625 75032921
         LA    RQCB,TSIDYQFG            GET ADDR OF TIME DELAY   A45625 75033221
*                                       QCB                             75033521
         B     POSTQCB                  GO TO POST IT            A45625 75033821
         SPACE 3                                                 A45625 75034121
*                                                                       75034421
* INITIATE A PUT OPERATION                                              75034721
*                                                                       75035021
NODELAY  EQU   *                                                 A45625 75035321
         TM    QCBTSOF2,QCBPOSTO        IS DELAY QCB ALREADY     A45625 75035621
*                                       POSTED                          75035921
         BO    ENDPOST                  YES, DO NOT POST AGAIN   A45625 75036221
         OI    QCBTSOF2,QCBPOSTO        INDICATE TSOUTPUT POSTED A45625 75036521
         MVI   QCBPRI,HIPRI             GIVE TASK HI PRIORITY    A45625 75036821
         SPACE 3                                                 A45625 75037121
*                                                                       75037421
* POST QCB TO ITSELF ON DISABLED READY QUEUE                            75037721
*                                                                       75038021
POSTQCB  EQU   *                                                 A45625 75038321
         IC    RWORK,QCBELCHN-1         SAVE FLAG BYTE           A45625 75038621
         ST    RQCB,QCBELCHN-1          PUT DELAY QCB IN CHAIN   A45625 75038921
*                                       FIELD                           75039221
         STC   RWORK,QCBELCHN-1         RESTORE FLAG BYTE        A45625 75039521
         XC    QCBLINK,QCBLINK          ZERO CHAINING FIELD      A45625 75039821
         L     RWORK,AVTREADD+D4        GET ADDR OF LAST ELEMENT A45625 75040121
*                                       ON QUEUE                        75040421
         ST    RQCB,AVTREADD+D4         QCB NOW LAST ELEM ON Q   A45625 75040721
         NC    AVTREADD+D1(L3),AVTREADD+D1 IS Q EMPTY            A45625 75041021
         BZ    PUTFIRST                 YES, MAKE QCB FIRST ON Q A45625 75041321
         MVC   D5(L3,RWORK),AVTREADD+D5 COMPLETE THE CHAIN       A45625 75041621
         B     POSTTCAM                 GO TO POST TCAM          A45625 75041921
*                                                                       75042221
PUTFIRST EQU   *                                                 A45625 75042521
         ST    RQCB,AVTREADD            QCB NOW ONLY ELEM ON Q   A45625 75042821
         SPACE 3                                                 A45625 75043121
*                                                                       75043421
* SEE IF NECESSARY TO POST TCAM                                         75043721
*                                                                       75044021
POSTTCAM EQU   *                                                 A45625 75044321
         TM    AVTOSECB,COMPLETE        IS TCAM IN WAIT STATE    A45625 75044621
         BO    ENDPOST                  YES, BRANCH              A45625 75044921
         SPACE 3                                                 A45625 75045221
*                                                                       75045521
* POST TCAM                                                             75045821
*                                                                       75046121
         LR    RSAVE,R13                SAVE SVRB ADDR AGAIN     A45625 75046421
         STM   R10,R15,D96(RSAVE)       SAVE REGS 10 THRU 15 IN  A45625 75046721
*                                       SVRB EXTENDED SAVE AREA         75047021
         SR    R10,R10                  COMPLETION CODE          A45625 75047321
         LA    RWORK1,AVTOSECB          GET ADDR OF ECB IN R11   A45625 75047621
         L     R13,AVTTCB               GET ADDR OF MCP'S TCB    A45625 75047921
         L     R15,CVT0PT01             GET ADDR OF POST ROUTINE A45625 75048221
         BALR  R14,R15                  GO TO POST ROUTINE       A45625 75048521
         LM    R10,R15,D96(RSAVE)       RESTORE REGISTERS        A45625 75048821
         SPACE 3                                                 A45625 75049121
ENDPOST  EQU   *                                                 A45625 75049421
         LA    RZERO,QINEC              QTIP EC FOR INPUT               75052020
QTIP     DS    0H                                                       75055020
         TM    TJBSTAT2,TJBHUNG         HAS USER HUNG UP                75055520
         BO    LEAVE                    YES, EXIT                       75056020
         L     R15,TSCVTQTP                                      S21008 75062621
         BALR  R14,R15                  GO TO QTIP               S21008 75062821
BACK     LR    RBASE,R14                                         S21008 75063021
         USING BACK,R14                                          S21008 75063221
*                                                                       75063421
* AFTER QTIP, MAKE TCAM DISPATCHABLE AGAIN                              75063621
*                                                                       75063821
         L     RCVT,CVTPTR(,0)          GET CVT ADDRESS          S21008 75064021
         L     RTCB,CVTAQAVT            GET PTR TO PTR TO AVT    S21008 75064221
         L     RTCB,0(,RTCB)            GET PTR TO AVT           S21008 75064421
         L     RTCB,AVTTCB-IEDQAVTD(,RTCB)  GET MCP'S TCB PT     S21008 75064621
         USING TCB,RTCB                                          S21008 75064821
         NI    TCBNDSP1,OFF-TCBTPSP     OFF SECONDARY BIT        M0121  75065021
         NC    TCBSCNDY,TCBSCNDY        ANY OTHERS ON            S21008 75065221
         BNZ   LEAVE                    YES, DON'T DISPATCH      S21008 75065421
         SPACE                                                          75065621
*                                                                       75065821
* GO TO TASK SWITCH FORCER, IEA0DS02, SO TCAM GETS NEXT SHOT            75066021
*                                                                       75066221
         NI    TCBFLGS5,OFF-TCBPNDSP    TURN OFF PRIMARY NDISPER S21008 75066421
         L     RWORK1,CVTABEND          GET PT TO SCNDY CVT      S21008 75066621
         USING SCVT,RWORK1                                       S21008 75066821
         L     RWORK1,SCVTTASW          ADDRESS OF FORCER        S21008 75067021
         LR    RTSCVT,RTCB              TCB TO PASS              S21008 75067221
         BALR  R14,RWORK1               FORCE SWITCH             S21008 75067421
         SPACE                                                          75067621
LEAVE    EQU   *                                                 S21008 75067821
         SR    R15,R15                  *                               75070020
PAU      DS    0H                                                       75073020
         SVC   EXIT                     RETURN TO CALLER                75076020
         SPACE 3                                                        75079020
********                                                                75082020
******** CONSTANTS                                                      75085020
********                                                                75088020
ZERO     DC    H'0'                     CONSTANT OF 0                   75091020
DISABLE  EQU   ZERO                     DISABLE ALL INTERRUPTS          75092020
ONE      DC    H'1'                     CONSTANT 1                      75094020
TWO      DC    H'2'                     CONSTANT OF TWO                 75097020
THREE    DC    H'3'                     CONSTANT OF THREE               75100020
SIX      DC    H'6'                     CONSTANT OF SIX                 75103020
EIGHT    DC    H'8'                     CONSTANT 8                      75106020
FIFTEEN  DC    H'15'                    CONSTANT OF 15                  75107020
SIXTEEN  DC    H'16'                    CONSTANT OF 16                  75109020
H18      DC    H'18'                    Halfword constant 18       @L01 75111400
H19      DC    H'19'                    Halfword constant 19       @L01 75111600
ENABLE   DC    X'FF'                    ENABLES ALL INTERRUPTS          75112020
         SPACE 3                                                        75115020
XCTLCON  DC    C'IGG09401'              MODULE ID                       75515020
         DC    X'00000000'              FILLER BYTES                    75915020
         DC    X'0000'                  FILLER BYTES                    76315020
         DC    X'000000'                FILLER BYTES                    76715020
         DC    X'000000'                FILLER BYTES                    77115020
         DC    X'0000'                  FILLER BYTES                    77515020
         DC    X'C378'                  FILLER BYTES                    77915020
         DC    X'000400'                FILLER BYTES                    78315020
         DC    X'0400'                  FILLER BYTES                    78715020
         DC    X'000000'                FILLER BYTES                    79115020
CODEEND  EQU   *                        End of module code         @L01 79215000
         ORG   IGC0009D+X'380'          Start of XCTL table        @L01 79515020
IDTTR    DC    C'00    '                EP NAME FOR TCABEND             79915020
         DC    C'01    '                EP NAME FOR TCLEARQ             80315020
         DC    C'02    '                UNUSED                          80715020
         DC    C'03    '                UNUSED                          81115020
         DC    C'04    '                EP  NAME FOR STBREAK            81515020
         DC    C'05    '                EP NAME FOR STCOM               81915020
         DC    C'06    '                EP NAME FOR STTIMEOU            82315020
         DC    C'07    '                EP NAME FOR STCC                82715020
         DC    C'08    '                EP NAME FOR STATTN              83115020
         DC    C'09    '                EP NAME FOR STAUTOLN            83515020
         DC    C'0A    '                EP NAME FOR STSIZE              83915020
         DC    C'0B    '                EP NAME FOR GTSIZE              84315020
         DC    C'0C    '                EP NAME FOR STAUTOCP            84715020
         DC    C'0D    '                EP NAME FOR SPAUTOPT            85115020
         DC    C'0E    '                EP NAME FOR RTAUTOPT            85515020
         DC    C'0F    '                EP NAME FOR TSABEND             85915020
         DC    C'0G    '                EP NAME FOR STCLEAR             86315020
         DC    C'11    '                17 invalid entry code      @L01 86415000
         DC    C'12    '                18 name for STFSMODE       @L01 86515000
         DC    C'13    '                19 name for STLINENO       @L01 86615000
         DC    H'0'                     END OF XCTL TABLE               86715020
         DC    XL2'00'                  Filler for alignment       @L01 86715020
         DC    C'094'                   CHAR CONSTANT                   87515020
         DC    AL1((IDTTR-IGC0009D)/8)  Pointer to XCTL table      @L01
*                                       for IEHIOSUP               @L01
XTBLSIZE EQU   *-IDTTR                  Size of XCTL table (bytes) @L01
XTBLSTRT EQU   1024-XTBLSIZE            Should be address of IDTTR @L01
         EJECT ,                                                   @L01
*----------------------------------------------------------------* @L01
*        If the following statement produces an assembly error,  * @L01
*        the XCTL table overlaps code in the module.             * @L01
*----------------------------------------------------------------* @L01
         DC    0S(IDTTR-CODEEND)                                   @L01
         SPACE 1                                                   @L01
*----------------------------------------------------------------* @L01
*        If the following statement produces an assembly error,  * @L01
*        the module is too big for the transient area.           * @L01
*----------------------------------------------------------------* @L01
         DC    0S(1024-(*-IGC0009D))                               @L01
         SPACE 1                                                   @L01
*----------------------------------------------------------------* @L01
*        If the following statement produces an assembly error,  * @L01
*        the module is shorter than 1024 bytes.                  * @L01
*----------------------------------------------------------------* @L01
         DC    0S((*-IGC0009D)-1024)                               @L01
         SPACE 1                                                   @L01
*----------------------------------------------------------------* @L01
*        If the following statement produces an assembly error,  * @L01
*        the XCTL table doesn't start on a doubleword boundary.  * @L01
*----------------------------------------------------------------* @L01
         DC    0S(((XTBLSTRT/8)*8)-XTBLSTRT)                       @L01
         SPACE 1                                                   @L01
*----------------------------------------------------------------* @L01
*        If the following statements produce an assembly error,  * @L01
*        the XCTL table doesn't start at the correct location.   * @L01
*----------------------------------------------------------------* @L01
         DC    0S(XTBLSTRT-(IDTTR-IGC0009D))                       @L01
         DC    0S((IDTTR-IGC0009D)-XTBLSTRT)                       @L01
         EJECT                                                          88115021
         TAVTD                                                          88315021
         EJECT                                                          88800020
         SPACE 3                                                 M0029  89300020
CVT      DSECT                                                          89800020
         CVT                                                            90300020
         SPACE 2                                                 M0029  90800020
         DCBD  DSORG=TX                                          M0029  91300020
         SPACE 2                                                 M0029  91800020
         IEZJSCB                                                        92300020
         SPACE 2                                                 M0029  92800020
         TLCBD                                                          93300020
         SPACE 2                                                 M0029  93800020
         TQCBD                                                          94300020
         SPACE                                                          94400021
SCVT     DSECT                                                          94500021
         SCVT                                                           94600021
         SPACE 2                                                 M0029  94800020
         IKJTCB                                                         95300020
         SPACE 2                                                 M0029  95800020
         IKJTJB                                                         96300020
         SPACE 2                                                 M0029  96800020
         IKJTSB                                                         97300020
         SPACE 2                                                 M0029  97800020
         IKJTSCVT                                                       98300020
         SPACE 3                                                 A45625 98400021
         TTSID                                                          98500021
         SPACE 2                                                 M0029  98800020
         END                                                            99300020
/*
//*
//*-----------------------------------------------------------------***
//*     Assemble IGG09412 (STFSMODE).                               ***
//*-----------------------------------------------------------------***
//ASM9412 EXEC PGM=IEUASM,
//             PARM='NOLOAD,DECK,RENT',REGION=256K,COND=(0,NE)
//SYSLIB   DD  DISP=SHR,DSN=SYS1.MACLIB,DCB=BLKSIZE=12960
//         DD  DISP=SHR,DSN=SYS1.MODGEN
//SYSPRINT DD  SYSOUT=A
//SYSUT1   DD  UNIT=SYSDA,SPACE=(TRK,(90,30))
//SYSUT2   DD  UNIT=SYSDA,SPACE=(TRK,(90,30))
//SYSUT3   DD  UNIT=SYSDA,SPACE=(TRK,(90,30))
//SYSGO    DD  DUMMY
//SYSLIN   DD  DUMMY
//SYSPUNCH DD  DSN=&&OBJSET(IGG09412),DISP=(OLD,PASS)
//SYSIN    DD *
         PRINT ON
******************************************************************
*                                                                *
* Name -                                                         *
*                                                                *
*    IGG09412                                                    *
*                                                                *
* Function -                                                     *
*                                                                *
*    This dummy routine returns a zero return code to            *
*    SVC 94 STFSMODE requests.  STFSMODE can be used to          *
*    determine if STLINENO is availabe in the current MVT        *
*    system.  If STFSMODE returns 0, STLINENO is available.      *
*                                                                *
* Entry points -                                                 *
*                                                                *
*    IGG09412 - Control is received from SVC 94 router           *
*       module IGC0009D via XCTL for a request with entry        *
*       code 18 (X'12').                                         *
*                                                                *
* Input -                                                        *
*                                                                *
*    Register  3 has CVT address                                 *
*    Register  4 has TCB address                                 *
*    Register  5 has SVRB extended save area address             *
*    Register 11 has contents of R1 from STFSMODE macro:         *
*        +------------+-----------+-----------+-------------+    *
*        |   flags    |           |           | reshow key  |    *
*        +------------+-----------+-----------+-------------+    *
*    Register  2 has contents of R0 from STFSMODE macro:         *
*        +------------+-----------+-----------+-------------+    *
*        | entry code |           |           |             |    *
*        +------------+-----------+-----------+-------------+    *
*                                                                *
* Output -                                                       *
*                                                                *
*    Register 15 has return code:                                *
*           0 - Successful                                       *
*                                                                *
* External references -                                          *
*                                                                *
*         None                                                   *
*                                                                *
******************************************************************
         EJECT ,
******************************************************************
*                                                                *
* Exits, normal -                                                *
*                                                                *
*         SVC 3 with 0 in register 15                            *
*                                                                *
* Exits, error -                                                 *
*                                                                *
*         SVC 3 with 4, 8, 12 in register 15                     *
*                                                                *
* Control blocks -                                               *
*                                                                *
*    Name      Macro     Description                     Usage   *
*    ----      --------  ------------------------------  -----   *
*      ----------------  none ------------------------           *
*                                                                *
*   Key = R-Read, W-Write, C-Create, D-Delete                    *
*                                                                *
* Attributes -                                                   *
*                                                                *
*    Reentrant, refreshable, supervisor mode                     *
*                                                                *
* Change activity -                                              *
*                                                                *
*   Flag  Date        By    Description                          *
*   ----  ----------  ----  -----------------------------------  *
*   None  2010/04/15  KL    Original source code.                *
*                                                                *
******************************************************************
         EJECT ,
IGG09412 CSECT ,
*----------------------------------------------------------------*
*        Register equates.                                       *
*----------------------------------------------------------------*
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
         SPACE 1
******************************************************************
*
*        IGG09412 service routine for STFSMODE
*
******************************************************************
         BALR  R12,0               Set base register
         USING *,R12               Addressability for program
         SPACE 1
*----------------------------------------------------------------*
*        Initialize module area to S(*) for possible             *
*        subsequent patching.                                    *
*----------------------------------------------------------------*
DUP      EQU   ((1024-(*-IGG09412))/2)  Build duplication factor
         DC    (DUP)S(*)                Initialize program area
         ORG   IGG09412                 Reset location counter
         BALR  R12,0               Set base register
         USING *,R12               Addressability for program
         SPACE 1
*----------------------------------------------------------------*
*                                                                *
*        Following will be set up by SVC interrupt handler       *
*        or SVC 94 front end:                                    *
*                                                                *
*          R3  = Address of CVT                                  *
*          R4  = Address of TCB                                  *
*          R5  = Address of RB                                   *
*          R11 = Parameter (R1 from macro invocation)            *
*          R2  = Entry code (R0 from macro invocation)           *
*                                                                *
*----------------------------------------------------------------*
         SPACE 1
*----------------------------------------------------------------*
*        Exit with return code in R15.                           *
*----------------------------------------------------------------*
STLEXIT  DS    0H                  Here to exit from subroutine
         XR    R15,R15             Set return code = 0
         SVC   3                   Return to caller
         LTORG ,                   Generate literal pool
CODEEND  EQU   *                   End of module code
         SPACE 1
*----------------------------------------------------------------*
*        XCTL table.                                             *
*----------------------------------------------------------------*
         ORG   IGG09412+1008       Position to start of table
STLXCTBL EQU   *                   Start of XCTL table
         DC    C'12    '           Table entry
         DC    H'0'                End of XCTL table
         DC    F'0'                Filler
         DC    C'094'              SVC 94 character constant
         DC    AL1((STLXCTBL-IGG09412)/8)
*                                  Pointer to XCTL table for IEHIOSUP
XTBLSIZE EQU   *-STLXCTBL          Size of XCTL table in bytes
XTBLSTRT EQU   1024-XTBLSIZE       Should be address of STLXCTBL
         EJECT ,
*----------------------------------------------------------------*
*        If the following statement produces an assembly error,  *
*        the XCTL table overlaps code in the module.             *
*----------------------------------------------------------------*
         DC    0S(STLXCTBL-CODEEND)
*
*----------------------------------------------------------------*
*        If the following statement produces an assembly error,  *
*        the module is too big for the transient area.           *
*----------------------------------------------------------------*
         DC    0S(1024-(*-IGG09412))
*
*----------------------------------------------------------------*
*        If the following statement produces an assembly error,  *
*        the module is shorter than 1024 bytes.                  *
*----------------------------------------------------------------*
         DC    0S((*-IGG09412)-1024)
*
*----------------------------------------------------------------*
*        If the following statement produces an assembly error,  *
*        the XCTL table doesn't start on a doubleword boundary.  *
*----------------------------------------------------------------*
         DC    0S(((XTBLSTRT/8)*8)-XTBLSTRT)
*
*----------------------------------------------------------------*
*        If the following statements produce an assembly error,  *
*        the XCTL table doesn't start at the correct location.   *
*----------------------------------------------------------------*
         DC    0S(XTBLSTRT-(STLXCTBL-IGG09412))
         DC    0S((STLXCTBL-IGG09412)-XTBLSTRT)
         END   ,
/*
//*
//*-----------------------------------------------------------------***
//*     Assemble IGG09413 (STLINENO).                               ***
//*-----------------------------------------------------------------***
//ASM9413 EXEC PGM=IEUASM,
//             PARM='NOLOAD,DECK,RENT',REGION=256K,COND=(0,NE)
//SYSLIB   DD  DISP=SHR,DSN=SYS1.MACLIB,DCB=BLKSIZE=12960
//         DD  DISP=SHR,DSN=SYS1.MODGEN
//SYSPRINT DD  SYSOUT=A
//SYSUT1   DD  UNIT=SYSDA,SPACE=(TRK,(90,30))
//SYSUT2   DD  UNIT=SYSDA,SPACE=(TRK,(90,30))
//SYSUT3   DD  UNIT=SYSDA,SPACE=(TRK,(90,30))
//SYSGO    DD  DUMMY
//SYSLIN   DD  DUMMY
//SYSPUNCH DD  DSN=&&OBJSET(IGG09413),DISP=(OLD,PASS)
//SYSIN    DD  *
         PRINT OFF
         MACRO                                                          00500020
         TQCBD                                                          01000020
*        UPDATED 01/05/73                                        S22024 01500000
.*       CONVERTED TO A BILINGUAL DSECT 07/19/72                 S22024 01700022
.* CHANGE ACTIVITY = AS FOLLOWS:                                        02000122
.*D015000,825000                                                SA51078 02000222
.*C820000,905000                                                SA51078 02000322
.*A024500                                                       SA51078 02000422
.*A087000,125000                                                 S22029 02020022
.*C015000                                                        S22029 02040022
.*D230000                                                        S22025 02060022
.*C015000                                                        S22025 02120022
.*A020000,6950000                                                       02200021
.*D110000-125000                                                 S22026 02300022
.*A087000,110000-125000,337000,382000-384000,497000              S22026 02400022
.*A000000,055000,445000,936000                                   S22024 02405022
.*C935000,020000                                                 S22024 02410022
.*D265000-270000                                                 S22024 02415022
*                                                                S22024 02420022
*        THIS IS A PREFIX TO THE QCB FOR 3705 SUPPORT            S22024 02425022
*                                                                S22024 02430022
IEDNQCB  DSECT                                                   S22024 02435022
QCBSTAT1 DS    XL1 .                    FLAG BYTE                S22024 02440022
QCBPLCBN EQU   X'80' .                  ON - FIELD CONTAINS PLCB ADDR   02445022
*                                       OFF - CONTAINS TTCIN     S22024 02450122
QCBPLCBF EQU   X'FF'-QCBPLCBN .                                  S22024 02455022
QCBNIDLE EQU   X'40'                    ON - LINE IS ACTIVE      S22024 02460022
QCBCKPRN EQU   X'20'                    CHECKPOINT RECORD NUMBER        02465022
QCBCKPSI EQU   X'10'                    REPLACE SESSION INITIATION      02470022
*                                       INFORMATION RECORD NUMBER       02475022
QCBOCNI  EQU   X'08'                    ON - STOPLINE IN PROGRESSS22024 02480022
QCBFLUSH EQU   X'04'                    DEACT FLUSH IN PROCESS   S22024 02482000
QCBPLCBC EQU   X'02'                    CTERM WITH PLCB ASSIGNED S22024 02483000
*                                       TO TERMINAL'S QCB        S22024 02484000
QCBSTMM  EQU   X'01'                    ON - STOP-MID-MSG        S22024 02484300
*                                       IN PROGRESS              S22024 02484600
QCBPLCBA DS    0AL3 .                   ADDR OF PLCB             S22024 02485022
         DS    XL1 .                                             S22024 02490022
QCBTTCIN DS    H .                      TNT INDEX                S22024 02495022
*                                                                       02500020
*                                                                       03000020
*        THIS IS A DSECT OF THE MASTER QCB FOLLOWED BY THE PRIORITY     03500020
*        QCB. THERE IS A MASTER QCB FOR EVERY MESSAGE QUEUE. THERE      04000020
*        IS A PRIORITY QCB FOR EACH PRIORITY LEVEL APPLICABLE FOR       04500020
*        THIS DESTINATION QUEUE.                                        05000020
         SPACE                                                          05500020
IEDQQCB  EQU   *                                                 S22024 05700022
QCBDSFLG DS    XL1 .                    FLAGS INDICATING A QCB FOR      06000020
*                                         DISPATCHER AND WHICH DATA     06500020
*                                         SET(S) THE MESSAGES FOR       07000020
*                                         THIS DEST. ARE QUEUED ON      07500020
*                        BIT DEFINITIONS                                08000020
QCBHELD  EQU   X'01' .                  STOP SENDING                    08200020
QCBFQCB  EQU   X'02' .                  FLAG INDICATING A QCB           08500020
QCBDRQQ  EQU   X'04' .                  FLAG INDIC A DRQ         S22026 08700022
QCBALTMH EQU   X'08'  .                 MSGS GO TO ALTERNATE MH  S22029 08800022
QCBREUS  EQU   X'10' .                  FLAG FOR REUSEABLE DISK Q.      09000020
QCBNREUS EQU   X'20' .                  FLAG FOR NONREUS. DISK Q.       09500020
QCBDISK  EQU   X'30' .                  DISK QUEUES USED                10000020
QCBCORE  EQU   X'40' .                  FLAG FOR MS QUEUES.             10500020
*              X'50'                    INDICATES CORE Q'S WITH         11000022
*                                       BACKUP ON REUSABLE DISK         11500022
*              X'60'                    INDICATES CORE Q'S WITH         12000022
*                                       BACKUP ON NONREUS DISK          12500022
QCBTSQ   EQU   X'80'  .                 TIME-SHARING QUEUES      S22029 12700022
*                                                                       13000020
QCBELCHN DS    AL3 .                    ELEMENT CHAIN                   13500020
*              CONTAINS THE QCB ADDRESS TO BE POSTED TO WHEN THIS       14000020
*              QCB IS REMOVED FROM THE TIME DELAY QUEUE.                14500020
QCBPRI   DS    XL1 .                    PRIORITY                        15000020
QCBLINK  DS    AL3 .                    POINTER TO NEXT STCB IN CHAIN   15500020
QCBSTVTO DS    AL1 .                    INDEX TO THE ENTRY IN THE       16000020
*                                         SUBTASK VECTOR TABLE          16500020
QCBSTCHN DS    AL3 .                    STCB CHAIN                      17000020
QCBSTPRI DS    XL1 .                    PRIORITY OF THE STCB            17500020
QCBCLSTR DS    0AL3 .                   CLUSTER ENTRY ADDR FOR   S22024 17600022
*                                       3705 GENERAL POLL        S22024 17700022
QCBSLINK DS    AL3 .                    POINTER TO NEXT STCB IN CHAIN   18000020
QCBRQCHN DS    0AL3 .                   CLUSTER REQUEST CHAIN    S22024 18100022
*                                       FOR 3705 GENERAL POLL    S22024 18200022
QCBEOLDT DS    XL2 .                    INTERRUPT TIME                  18500020
QCBRETCT DS    0XL1 .                   TSO RETRY COUNTERS          TSO 19000020
QCBLKRLN DS    XL1 .                    LOCK RELATIVE LINE NO.          19500020
*              OFFSET TO QCB FOR TIME DELAY - FOR QCB = X'00'           20000020
QCBSTAT  DS    XL1 .                    STATUS OF THIS QCB              20500020
*                        BIT DEFINITIONS                                21000020
QCBEOM   EQU   X'80' .                  END OF MESSAGE SENT             21500020
QCBTRMHO EQU   X'40' .                  TERMINAL WAS HELD               21700022
QCBBUFRD EQU   X'20' .                  BUFFERED TERMINAL               22000020
QCBSEND  EQU   X'10' .                  SENDING TO BUFFERED TERMINAL    22500020
QCBSCHDL EQU   X'04' .                  PUT IN DELAY Q WHEN INACTIVE    23500020
QCBCLOCK EQU   X'02' .                  ON = CLOCK, OFF = INTVL         24000020
QCBTIME  EQU   X'01' .                  DELAY GREATER THAN 12 HOURS     24500020
*                                                                       25000020
QCBSCBOF DS    XL1 .                    OFFSET TO THE PROPER SCB        25500020
*                                         FOR THIS TRANSMISSION.        26000020
QCBINSRC DS    0AL3 .                   CHAIN OF SOURCE LCB'S           27500020
*                                         CURRENTLY SENDING INITIATE    28000020
*                                         MODE MSGS TO THIS DEST. Q.    28500020
QCBSATCT DS    XL1 .                    SIM ATTN OUTPUT LINE COUNT  TSO 29000020
QCBTSOF2 DS    XL1 .                    SECOND TSO FLAG BYTE        TSO 29500020
*                        BIT DEFINITIONS                            TSO 30000020
QCBINHBN EQU   X'80' .                  USE INHIBITS WITH THIS TERM TSO 30500020
QCBBUFQ  EQU   X'40' .                  TCAM BUFFER BEING HELD      TSO 31000020
QCBPOSTO EQU   X'20' .                  QCB POSTED TO ITSELF        TSO 31500020
QCBDSSMI EQU   X'10' .                  START MI CHARACTER SENT     TSO 31700022
QCBSIMRD EQU   X'08' .                  SIMATTN READ EXECUTING      TSO 32100020
QCBSATCH EQU   X'04' .                  SIMULATED ATTN BY CHARACTER TSO 32500020
QCBSATTI EQU   X'02' .                  SIMULATED ATTN BY TIME      TSO 33000020
QCBSATLC EQU   X'01' .                  SIMULATED ATTN BY LINE      TSO 33500020
*                                                                       33700022
QCBTSOF1 DS    XL1 .                    FIRST TSO FLAG BYTE         TSO 34000020
*                        BIT DEFINITIONS                            TSO 34500020
QCBWRBRK EQU   X'80' .                  ISSUE A WRITE BREAK         TSO 35000020
QCBTGET  EQU   X'40' .                  TGET REQUEST                TSO 35500020
QCBTPUT  EQU   X'20' .                  TPUT REQUEST                TSO 36000020
QCBNOBUF EQU   X'10' .                  INSUFFICIENT BUFFERS        TSO 36500020
QCBSATRD EQU   X'08' .                  SIMULATED ATTN READ REQUEST TSO 37000020
QCBPARTO EQU   X'04' .                  PARTIAL OUTPUT LINE         TSO 37500020
QCBDELAY EQU   X'02' .                  QCB IN DELAY QUEUE              38000020
QCBDISC  EQU   X'01' .                  USER TO BE LOGGED OFF       TSO 38200022
*                                                                       38300022
QCBEXTO  DS    0H .                     OFFSET TO EXT            S22026 38400022
QCBINTVL DS    XL2 .                    INTERVAL FOR POLL DELAY         38500020
QCBMSGCT DS    H .                      COUNT OF MESSAGES IN THIS       39000020
*                                         QUEUE                         39500020
QCBPREN  DS    0A .                     ADDRESS OF TERMINAL TABLE       40000020
*                                       ENTRY IF QCB FOR A PROCESS      40500020
*                                       ENTRY                           41000020
QCBPRLVL DS    XL1 .                    HIGHEST PRIORITY LEVEL MESSAGE  41500020
QCBLKRRN DS    0XL3 .                   LOCK RELATIVE RECORD NUM.       42000020
*              LINK FIELD FOR QCB WHEN ON THE TIME DELAY QUEUE          42500020
QCBCARCT DS    XL1 .                    CARRIAGE POSITION COUNT     TSO 43000020
QCBTJID  DS    H .                      TSO JOB IDENTIFICATION      TSO 43500020
QCBRELLN DS    XL1 .                    RELATIVE LINE NO. FOR THE       44000020
*                                         LINE THIS QCB REPRESENTS      44500020
QCBLGBAD DS    0AL3 .                   ADDRESS OF LGB           S22024 44700022
QCBDCBAD DS    AL3 .                    ADDRESS OF DCB                  45000020
QCBFLAG  DS    XL1 .                    QCB STATUS BITS                 45500020
*                        BIT DEFINITIONS                                46000020
QCBTSSES EQU   X'80' .                  TSO SESSION IN PROGRESS     TSO 46500020
QCBNOBRK EQU   X'40' .                  NO REVERSE BREAK FEATURE    TSO 47000020
QCBREAD  EQU   X'20' .                  READ HAS PRIORITY           TSO 47500020
QCBRSRV  EQU   X'10' .                  REUSE SERVICED BIT              48000020
QCBTERMQ EQU   X'08' .                  QUEUING BY TERMINAL             48200022
QCBSDFFO EQU   X'04' .                  CURRENTLY SENDING FEFO MSG      48500020
QCBPROC  EQU   X'02' .                  THIS QCB FOR A PROCESS ENTRY    49000020
QCBCKPT  EQU   X'01' .                  FLAG FOR CHECKPOINT             49500020
*                                                                       49700022
QCBQBACK DS    AL3 .                    QBACK MESSAGE CHAIN             50000020
         SPACE 2                                                        50500020
*     DEC                                 HEX                           51000020
*        *********************************                              51500020
*      0 * DSFLG *         ELCHN         *                              52000020
*        *********************************                              52500020
*      4 * PRI   *         LINK          *  4                           53000020
*        *********************************                              53500020
*      8 *  VTO  *         STCHN         *  8                           54000020
*        *********************************           **************     54500020
*     12 * STPRI *         SLINK         *  C        *            *     55000020
*        *********************************           * MASTER QCB *     55500020
*     16 *    EOLDT      * LKRLN * STAT  * 10        *                  56000020
*        *********************************           **************     56500020
*     20 * SCBOF *         INSRC         * 14                           57000020
*        *********************************                              57500020
*     24 *    INTVL      *     MSGCT     * 18                           58000020
*        *********************************                              58500020
*     28 * PRILVL*     PREN/LKRRN        * 1C                           59000020
*        *********************************                              59500020
*     32 * RELLN *      DCBAD/LGBAD      * 20                    S22024 60000022
*        *********************************                              60500020
*     36 * FLAG  *         QBACK         * 24                           61000020
*        *********************************                              61500020
         SPACE                                                          62000020
*                                                                       62500020
QCBMEND  EQU   * .                                                      63000020
QCBMSIZE EQU   QCBMEND-IEDQQCB .        SIZE OF MASTER QCB              63500020
*                                                                       64000020
         SPACE 2                                                        64500020
*                                                                       65000020
*        THIS IS THE DSECT OF A PRIORITY QCB.  THERE IS A PRIORITY      65500020
*        QCB FOR EACH PRIORITY LEVEL APPLICABLE FOR THIS DEST. Q        66000020
*                                                                       66500020
IEDQPQCB EQU   * .                 START OF PRI LEVEL QCB               67000020
QCBDNHDR DS    XL3 .                    DISK RECORE NUMBER TO PUT       67500020
*                                         THE NEXT HDR RECEIVED         68000020
QCBFHDLZ DS    0XL3 .                   DISK RECORD             SA52971 68500022
QCBDATFL DS    X .                     DATFLAGS FIELD OF LAST    S21101 69600021
*                                      MSG REMOVED FROM FEFO Q   S21101 69650021
QCBPFEFO DS    0XL3 .                   IF TERM OF QUEUE HELD,  SA52971 69700022
*                                         PREVIOUS TO FIRST     SA52971 69710022
*                                         HELD MESSAGE          SA52971 69720022
*                                       IF TERM NOT HELD,       SA52971 69730022
*                                         PREVIOUS TO LAST      SA52971 69740022
         DS    XL2                        MESSAGE SERVICED      SA52971 69750022
QCBFHDTZ DS    0XL3 .                   DISK RECORD             SA52971 69760022
         DS    XL1 .                    PFEFO CONTINUED         SA52971 69770022
QCBDATSQ DS    XL2 .                    SEQUENC NUMBER OF LAST   S21101 69800021
*                                       MSG REMOVED FROM FEFO Q  S21101 69850021
QCBINTFF DS    XL3 .                    DISK REC.NO. OF THE FIRST       72000020
*                                         INTERCEPTED MSG. - FEFO       72500020
*                                         ORDER                         73000020
QCBINTLF DS    0XL3 .                   LAST INTERCEPTED MSG    SA52971 73500022
QCBPREVF DS    XL3 .                    PREVIOUS TO LAST FFEFO  SA52971 74000022
QCBFFEFO DS    XL3 .                    DISK REC. NO. OF THE FIRST      74500020
*                                         (FEFO) MSG. TO BE RECVD.      75000020
*                                         CORE RECORD NO. IF THIS       75500020
*                                         IS A CORE ONLY QUEUE.         76000020
QCBLFEFO DS    XL3 .                    DISK RECORD NO. OF THE          76500020
*                                         LAST FEFO MSG. RECEIVED.      77000020
*                                         CORE REC. NO. IF THIS IS      77500020
*                                         A CORE ONLY QUEUE.            78000020
QCBCFHDR DS    XL3 .                    CORE REC. NO. OF THE FIRST      78500020
*                                         HEADER APPEARING IN THIS      79000020
*                                         QUEUE.                        79500020
QCBPRIPQ DS    XL1 .                    THE PRIORITY OF THIS PRITY      80000020
*                                         LEVEL QCB.  THIS WILL BE      80500020
*                                         X'00' IF THIS IS THE          81000020
*                                         LOWEST PRTY LEVEL.            81500020
QCBPQBCK DS    XL3 .                    PQCB BACK CHAIN         SA51078 82000022
         SPACE 2                                                        83000020
*     DEC                                 HEX                           83500020
*        *********************************                              84000020
*      0 *         DNHDR         * DATFL *  0                   SA52971 84500022
*        *********************************                              85000020
*      4 *         PFEFO         * DATSQ *  4                   SA52971 85500022
*        *********************************         ****************     86000020
*      8 *(DATSQ)*         INTFF         *  8      *            SA52971 86500022
*        *********************************         * PRIORITY QCB *     87000020
*     12 *         PREVF         * FFEFO *  C      *            SA52971 87500022
*        *********************************         ****************     88000020
*     16 *    (FFEFO)    *     LFEFO     * 10                           88500020
*        *********************************                              89000020
*     20 *(LFEFO)*         CFHDR         * 14                           89500020
*        *********************************                              90000020
*     24 * PRIPQ *         PQBCK         * 18                   SA51078 90500022
*        *********************************                              91000020
*                                                                       91500020
QCBPEND  EQU   *                                                        92000020
QCBPSIZE EQU   QCBPEND-IEDQPQCB                                         92500020
*                                                                       93000020
         MEND  , */                                                     93500022
         PRINT ON
******************************************************************
*                                                                *
* Name -                                                         *
*                                                                *
*    IGG09413                                                    *
*                                                                *
* Function -                                                     *
*                                                                *
*    This routine processes SVC 94 STLINENO requests to set      *
*    the next line number for line-mode 3270 display output      *
*    to a specified value.                                       *
*                                                                *
* Entry points -                                                 *
*                                                                *
*    IGG09413 - Control is received from SVC 94 router           *
*       module IGC0009D via XCTL for a request with entry        *
*       code 19 (X'13').                                         *
*                                                                *
* Input -                                                        *
*                                                                *
*    Register  3 has CVT address                                 *
*    Register  4 has TCB address                                 *
*    Register  5 has SVRB extended save area address             *
*    Register 11 has contents of R1 from STLINENO macro:         *
*        +------------+-----------+-----------+-------------+    *
*        |            |           |           | line number |    *
*        +------------+-----------+-----------+-------------+    *
*    Register  2 has contents of R0 from STLINENO macro:         *
*        +------------+-----------+-----------+-------------+    *
*        | entry code |           |           |             |    *
*        +------------+-----------+-----------+-------------+    *
*                                                                *
* Output -                                                       *
*                                                                *
*    Register 15 has return code:                                *
*           0 - Successful                                       *
*           4 - Invalid parameter (not used by this code).       *
*           8 - STLINENO is invalid for this terminal,           *
*               only 3270 terminal types allowed                 *
*          12 - Secified line number was 0, or greater than      *
*               than the maximum number of lines allowed for     *
*               the terminal in use.                             *
*                                                                *
* External references -                                          *
*                                                                *
*         None                                                   *
*                                                                *
******************************************************************
         EJECT ,
******************************************************************
*                                                                *
* Exits, normal -                                                *
*                                                                *
*         SVC 3 with 0 in register 15                            *
*                                                                *
* Exits, error -                                                 *
*                                                                *
*         SVC 3 with 4, 8, 12 in register 15                     *
*                                                                *
* Control blocks -                                               *
*                                                                *
*    Name      Macro     Description                     Usage   *
*    ----      --------  ------------------------------  -----   *
*    CVT       CVT       OS Communications Vector Table   R      *
*    IEDQQCB   TQCBD     TCAM Queue Control Block         W      *
*    JSCB      IEZJSCB   OS Job Step Control Block        R      *
*    TCB       IKJTCB    OS Task Control Block            R      *
*    TIOCRPT   IKJTIOCP  TSO TIOC Reference Pointer Table R      *
*    TSB       IKJTSB    TSO Terminal Status Block        R      *
*    TSCVT     IKJTSCVT  TSO Communications Vector Table  R      *
*                                                                *
*   Key = R-Read, W-Write, C-Create, D-Delete                    *
*                                                                *
* Attributes -                                                   *
*                                                                *
*    Reentrant, refreshable, supervisor mode                     *
*                                                                *
* Change activity -                                              *
*                                                                *
*   Flag  Date        By    Description                          *
*   ----  ----------  ----  -----------------------------------  *
*   None  2010/04/15  KL    Original source code.                *
*                                                                *
******************************************************************
         EJECT ,
IGG09413 CSECT ,
*----------------------------------------------------------------*
*        Register equates.                                       *
*----------------------------------------------------------------*
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
         SPACE 1
******************************************************************
*
*        IGG09413 service routine for STLINENO
*
******************************************************************
         BALR  R12,0               Set base register
         USING *,R12               Addressability for program
         SPACE 1
*----------------------------------------------------------------*
*        Initialize module area to S(*) for possible             *
*        subsequent patching.                                    *
*----------------------------------------------------------------*
DUP      EQU   ((1024-(*-IGG09413))/2)  Build duplication factor
         DC    (DUP)S(*)                Initialize program area
         ORG   IGG09413                 Reset location counter
         BALR  R12,0               Set base register
         USING *,R12               Addressability for program
         SPACE 1
*----------------------------------------------------------------*
*                                                                *
*        Following will be set up by SVC interrupt handler       *
*        or SVC 94 front end:                                    *
*                                                                *
*          R3  = Address of CVT                                  *
*          R4  = Address of TCB                                  *
*          R5  = Address of RB                                   *
*          R11 = Parameter (R1 from macro invocation)            *
*          R2  = Entry code (R0 from macro invocation)           *
*                                                                *
*----------------------------------------------------------------*
         SPACE 1
         USING CVT,R3              Addressability for CVT
         USING TCB,R4              Addressability for TCB
         SPACE 1
*----------------------------------------------------------------*
*        Put current TJID from JSCB in R8.                       *
*----------------------------------------------------------------*
         L     R15,TCBJSCB         Get JSCB address from TCB
         USING IEZJSCB,R15         Addressability for JSCB
         LH    R8,JSCBTJID         Get our TJID from JSCB
         DROP  R15                 End JSCB addressability
         SPACE 1
*----------------------------------------------------------------*
*        Find the TSB for our terminal.                          *
*----------------------------------------------------------------*
         L     R15,CVTTSCVT        Get TSCVT address
         USING TSCVT,R15           Addressability for TSCVT
         L     R15,TSCVTRPT        Point to reference pointer table
         USING TIOCRPT,R15         Addressability for RPT
         L     R14,TIOCTSBS        Get address of TSB table;
*                                  high-order byte of R14 has size
*                                  of a TSB, low-order 24 bits
*                                  have TSB table address
         DROP  R15                 End RPT addressability
         LR    R15,R14             Isolate TSB size
         SRL   R15,24               in low-order byte of R15
         LA    R14,0(,R14)         Clear high-order byte
         LR    R1,R8               Copy TJID
         BCTR  R1,0                Find TSB table offset
         MR    R0,R15               as (TJID-1) * TSB_SIZE
         AR    R1,R14              Add offset to TSB table start
         USING TSB,R1              Addressability for TSB
         SPACE 1
*----------------------------------------------------------------*
*        Found TSB.  Verify terminal is a 3270 and validate      *
*        requested line number.                                  *
*----------------------------------------------------------------*
         TM    TSBSTAT,TSB3270     Is terminal a 3270?
         BZ    STLERR8             No, invalid terminal type
         CLI   TSBLNNO,X'00'       Is there a screen length?
         BE    STLERR8             No, invalid terminal type
         LR    R15,R11             Get requested line number
         LA    R15,0(,R15)          and remove high-order flag
         LTR   R15,R15             Is line number zero?
         BZ    STLERR12            Yes, invalid line number error
         XR    R0,R0               Clear register
         IC    R0,TSBLNNO          Get screen length from TSB
         CR    R15,R0              Is requested line too big?
         BH    STLERR12            Yes, invalid line number error
         SPACE 1
*----------------------------------------------------------------*
*        Requested line number valid.  Get address of            *
*        destination QCB for our terminal from TSB.              *
*----------------------------------------------------------------*
         L     R15,TSBQCB-1        Get address of QCB for terminal
         USING IEDQQCB,R15         Addressability for QCB
         DROP  R1                  End TSB addressability
         SPACE 1
*----------------------------------------------------------------*
*        Set the desired next line number minus one in the       *
*        QCB carriage count field as the previously-displayed    *
*        line.                                                   *
*----------------------------------------------------------------*
         STC   R11,QCBSATCT        >Set line number in carriage count
         DROP  R15                 End QCB addressability
         XR    R15,R15             Set zero return code
         B     STLEXIT             Go to exit
         SPACE 1
*----------------------------------------------------------------*
*        Error:  invalid parameters (not used).                  *
*----------------------------------------------------------------*
STLERR4  DS    0H                  Error, invalid parameters
         LA    R15,4               Set return code = 4
         B     STLEXIT             Go to exit
         SPACE 1
*----------------------------------------------------------------*
*        Error:  terminal type is not valid, only 3270 terminal  *
*                types allowed.                                  *
*----------------------------------------------------------------*
STLERR8  DS    0H                  Error, invalid terminal type
         LA    R15,8               Set return code = 8
         B     STLEXIT             Go to exit
         SPACE 1
*----------------------------------------------------------------*
*        Error:  specified line number was 0 or it was greater   *
*                than the maximum number of lines allowed for    *
*                the terminal in use.                            *
*----------------------------------------------------------------*
STLERR12 DS    0H                  Error, invalid line count
         LA    R15,12              Set return code = 12
         B     STLEXIT             Go to exit
         SPACE 1
*----------------------------------------------------------------*
*        Exit with return code in R15.                           *
*----------------------------------------------------------------*
STLEXIT  DS    0H                  Here to exit from subroutine
         SVC   3                   Return to caller
         LTORG ,                   Generate literal pool
CODEEND  EQU   *                   End of module code
         SPACE 1
*----------------------------------------------------------------*
*        XCTL table.                                             *
*----------------------------------------------------------------*
         ORG   IGG09413+1008       Position to start of table
STLXCTBL EQU   *                   Start of XCTL table
         DC    C'13    '           INFORMATION FOR FIRST LOAD SVC
         DC    H'0'                End of XCTL table
         DC    F'0'                Filler
         DC    C'094'              SVC 94 character constant
         DC    AL1((STLXCTBL-IGG09413)/8)
*                                  Pointer to XCTL table for IEHIOSUP
XTBLSIZE EQU   *-STLXCTBL          Size of XCTL table in bytes
XTBLSTRT EQU   1024-XTBLSIZE       Should be address of STLXCTBL
         EJECT ,
*----------------------------------------------------------------*
*        If the following statement produces an assembly error,  *
*        the XCTL table overlaps code in the module.             *
*----------------------------------------------------------------*
         DC    0S(STLXCTBL-CODEEND)
*
*----------------------------------------------------------------*
*        If the following statement produces an assembly error,  *
*        the module is too big for the transient area.           *
*----------------------------------------------------------------*
         DC    0S(1024-(*-IGG09413))
*
*----------------------------------------------------------------*
*        If the following statement produces an assembly error,  *
*        the module is shorter than 1024 bytes.                  *
*----------------------------------------------------------------*
         DC    0S((*-IGG09413)-1024)
*
*----------------------------------------------------------------*
*        If the following statement produces an assembly error,  *
*        the XCTL table doesn't start on a doubleword boundary.  *
*----------------------------------------------------------------*
         DC    0S(((XTBLSTRT/8)*8)-XTBLSTRT)
*
*----------------------------------------------------------------*
*        If the following statements produce an assembly error,  *
*        the XCTL table doesn't start at the correct location.   *
*----------------------------------------------------------------*
         DC    0S(XTBLSTRT-(STLXCTBL-IGG09413))
         DC    0S((STLXCTBL-IGG09413)-XTBLSTRT)
         EJECT ,
*----------------------------------------------------------------*
*        OS Communications Vector Table (CVT).                   *
*----------------------------------------------------------------*
CVT      DSECT ,
         CVT   ,
         SPACE 1
*----------------------------------------------------------------*
*        OS Task Control Block (TCB).
*----------------------------------------------------------------*
         IKJTCB  ,
         SPACE 1
*----------------------------------------------------------------*
*        OS Job Step Control Block (JSCB).
*----------------------------------------------------------------*
         IEZJSCB ,
         SPACE 1
*----------------------------------------------------------------*
*        TSO Communications Vector Table (TSCVT).
*----------------------------------------------------------------*
         IKJTSCVT ,
         SPACE 1
*----------------------------------------------------------------*
*        TSO TIOC Reference Pointer Table (RPT).
*----------------------------------------------------------------*
         IKJTIOCP ,
         SPACE 1
*----------------------------------------------------------------*
*        TSO Terminal Status Block (TSB).
*----------------------------------------------------------------*
         IKJTSB ,
         SPACE 1
*----------------------------------------------------------------*
*        TCAM Queue Control Block (QCB).
*----------------------------------------------------------------*
         TQCBD ,
         END   ,
//*
//*-----------------------------------------------------------------***
//*     Link all modules to SYS1.SVCLIB.                            ***
//*-----------------------------------------------------------------***
//LKED   EXEC PGM=IEWL,PARM=(XREF,LET,LIST,NCAL,RENT,REUS,REFR),
//            REGION=128K,COND=(0,NE)
//SYSLMOD  DD DISP=SHR,DSN=SYS1.SVCLIB
//SYSUT1   DD DSN=&&SYSUT1,UNIT=SYSDA,SPACE=(1024,(50,20))
//SYSPRINT DD SYSOUT=A
//SYSPUNCH DD DSN=&&OBJSET,DISP=(OLD,DELETE)
//SYSLIN   DD *
 INCLUDE SYSPUNCH(IGC0009D)
 ENTRY IGC0009D
 NAME  IGC0009D(R)
 INCLUDE SYSPUNCH(IGG09412)
 ENTRY IGG09412
 NAME  IGG09412(R)
 INCLUDE SYSPUNCH(IGG09413)
 ENTRY IGG09413
 NAME  IGG09413(R)
/*
//*
//*-----------------------------------------------------------------***
//*     Run IEHIOSUP to rebuild XCTL tables.                        ***
//*-----------------------------------------------------------------***
//IOSUP  EXEC PGM=IEHIOSUP,PARM=TSO,COND=(0,NE)
//SYSPRINT DD SYSOUT=A
//SYSUT1   DD DISP=OLD,DSN=SYS1.SVCLIB
//*
//*-----------------------------------------------------------------***
//*     Run IEBUPDTE to add macros to SYS1.MACLIB.                  ***
//*-----------------------------------------------------------------***
//UPDTE  EXEC PGM=IEBUPDTE,PARM=NEW,REGION=128K,COND=(0,NE)
//SYSPRINT DD SYSOUT=A
//SYSUT2   DD DISP=OLD,DSN=SYS1.MACLIB
//SYSIN    DD *
./ ADD NAME=STLINENO
         MACRO                                                          00050000
&NAME    STLINENO &LINE=,&LINELOC=,&MODE=OFF      PROTOTYPE             00100000
.********************************************************************** 00150000
.*                                                                    * 00200000
.* STATUS - CHANGE LEVEL 000                                          * 00250000
.*                                                                    * 00300000
.* NAME - STLINENO   (SET LINE NUMBER FOR NEXT TPUT EDIT TO 3270)     * 00350000
.*                                                                    * 00400000
.* FUNCTION - THE STLINENO MACRO IS USED TO SPECIFY ON WHICH LINE OF  * 00450000
.*            A TSO/VTAM DISPLAY SCREEN THE NEXT NON-FULL SCREEN TPUT * 00500000
.*            IS TO APPEAR.                                           * 00550000
.*                                                                    * 00600000
.*       THE MACRO PROTOTYPE IS ABOVE                                 * 00650000
.*                                                                    * 00700000
.*       THE STANDARD FORM OF THE EXPANSION IS BELOW                  * 00750000
.*                                                                    * 00800000
.*&NAME  LA    1,3                      PUT LINE NUM IN PARM REG      * 00850000
.*       LA    0,19                     LOAD ENTRY CODE               * 00900000
.*       SLL   0,24                     SHIFT TO HIGH ORDER BYTE      * 00950000
.*       SVC   94                       TERMINAL CONTROL MACRO SVC    * 01000000
.*                                                                    * 01050000
.* Note - In MVT, STLINENO operates as in MVS 3.8.  It sets the    @L01 01070200
.*        line for the next non-full screen TPUT.  STLINENO with   @L01 01070400
.*        LINE=1 does not automatically clear the screen as it     @L01 01070600
.*        does in more modern versions of MVS.                     @L01 01070800
.*                                                                 @L01 01071000
.********************************************************************** 01100000
         AIF   ('&LINE' NE '' AND '&LINELOC' NE '').ERROR1              01150000
         AIF   ('&LINE' EQ '' AND '&LINELOC' EQ '').ERROR2              01200000
         AIF   ('&MODE' NE 'OFF' AND '&MODE' NE 'ON').ERROR1            01250000
         AIF   ('&LINELOC' NE '').LNADDR                                01300000
         AIF   ('&LINE'(1,1) EQ '(').LNREG                              01350000
&NAME    LA    1,&LINE -                PUT LINE NUM IN PARM REG        01400000
         AGO   .STMODE                                                  01450000
.LNREG   ANOP                                                           01470000
&NAME    LR    1,&LINE(1) -             PUT LINE NUM IN PARM REG        01500000
         AGO   .STMODE                                                  01550000
.LNADDR  ANOP                                                           01600000
         AIF   ('&LINELOC'(1,1) EQ '(').LNADREG                         01650000
&NAME    L     1,&LINELOC -             PUT LINE NUM IN PARM REG        01700000
         AGO   .STMODE                                                  01750000
.LNADREG ANOP                                                           01800000
&NAME    L     1,0(,&LINELOC(1)) -      PUT LINE NUM IN PARM REG        01850000
.STMODE  ANOP                                                           01900000
         AIF   ('&MODE' EQ 'OFF').STMDOFF                               01950000
         LA    0,128 -                  PREPARE FLAY BYTE               02000000
         SLL   0,24 -                   SHIFT TO HIGH ORDER BYTE        02050000
         OR    1,0 -                    SET MODE=ON FLAG IN REG 1       02100000
         AGO   .ENTRYCD                                                 02110000
.STMDOFF ANOP                                                           02120000
         SR    0,0 -                    PREPARE FLAG BYTE               02130000
         OR    1,0 -                    SET MODE=OFF FLAG IN REG 1      02140000
.ENTRYCD ANOP                                                           02150000
         LA    0,19 -                   LOAD ENTRY CODE                 02200000
         SLL   0,24 -                   SHIFT TO HIGH ORDER BYTE        02250000
         SVC   94 -                     TERMINAL CONTROL MACRO SVC      02300000
         MEXIT                                                          02350000
.ERROR1  IHBERMAC 54,,,                                                 02400000
         MEXIT                                                          02450000
.ERROR2  IHBERMAC 24,,,                                                 02500000
         MEND                                                           02550000
./ ADD NAME=STFSMODE
         MACRO                                                          00050000
&NAME    STFSMODE &A,&INITIAL=NO,&RSHWKEY=64,&NOEDIT=NO,&PARTION=NO     00100000
.********************************************************************** 00150000
.*                                                                    * 00200000
.* STATUS - OS/VS2 TSO/VTAM DYNAMIC RESHOW LEVEL 2.0                  * 00250000
.*                                                                    * 00300000
.* NAME - STFSMODE  (SET FULL SCREEN MODE)                            * 00350000
.*                                                                    * 00400000
.* FUNCTION - THE STFSMODE MACRO IS USED TO SPECIFY WHETHER OR NOT    * 00450000
.*            AN APPLICATION IS PRESENTING DISPLAYS TO A TSO/VTAM     * 00500000
.*            DISPLAY TERMINAL IN FULL SCREEN MODE.                   * 00550000
.*            WHEN INITIALLY SETTING THE MODE ON, INITIAL=YES WILL    * 00560000
.*            PREVENT PAGING FOR THE FIRST FULL SCREEN TPUT ISSUED BY * 00570000
.*            THE APPLICATION.                                        * 00580000
.*            RSHWKEY IS USED TO SPECIFY WHICH PFKEY IS TO BE USED    * 00582000
.*            AS THE RESHOW KEY.  E.G., RSHWKEY=3 MEANS THAT PFKEY 3  * 00584000
.*            IS TO BE THE RESHOW KEY.  WHEN STFSMODE IS OFF,         * 00586000
.*            RSHWKEY IS NOT TO BE SPECIFIED.  WHEN STFSMODE IS ON    * 00588000
.*            AND NO RSHWKEY HAS BEEN SPECIFIED, RSHWKEY WILL DEFAULT * 00590000
.*            TO ZERO.                                                * 00592000
.*                                                                    * 00600000
.*       THE MACRO PROTOTYPE IS ABOVE                                 * 00650000
.*                                                                    * 00700000
.*       THE STANDARD FORM OF THE EXPANSION IS BELOW                  * 00750000
.*&NAME  LA    1,128+64                 FLAGS FOR MODE/INITIAL CALL   * 00800000
.*       SLL   1,24                     SHIFT TO HIGH ORDER BYTE      * 00850000
.*       SR    0,0                      CLEAR REGISTER                * 00860000
.*       LA    0,10                     LOAD RSHWKEY                  * 00870000
.*       OR    1,0                      RSHWKEY IN RIGHTMOST BYTE     * 00880000
.*       LA    0,18                     LOAD ENTRY CODE               * 00900000
.*       SLL   0,24                     SHIFT TO HIGH ORDER BYTE      * 00950000
.*       SVC   94                       TERMINAL CONTROL MACRO SVC    * 01000000
.*                                                                    * 01050000
.* Note - In MVT, STFSMODE returns a return code of zero, but      @L01 01070200
.*        otherwise doesn't do anything.                           @L01 01070400
.*                                                                 @L01 01070600
.********************************************************************** 01100000
         LCLA  &SYSFS                                                   01105000
         LCLA  &SYSINIT                                                 01110000
&SYSFS   SETA  128                                                      01115000
&SYSINIT SETA  0                                                        01120000
         AIF   ('&INITIAL' NE 'YES' AND '&INITIAL' NE 'NO').ERROR1      01125000
         AIF   (&RSHWKEY GT 0 AND &RSHWKEY LT 13).TESTFS                01125700
         AIF   (&RSHWKEY NE 64).ERROR2                                  01126100
         AGO   .NEXT                                                    01127100
.TESTFS  ANOP                                                           01127800
         AIF   ('&A' EQ 'OFF').ERROR3                                   01128500
.NEXT    ANOP                                                           01129200
         AIF   ('&INITIAL' EQ 'NO').TESTA                               01130000
&SYSINIT SETA  64                                                       01135000
.TESTA   ANOP                                                           01140000
         AIF   ('&A' EQ 'ON' OR '&A' EQ '').FSON                        01150000
         AIF   ('&A' EQ 'OFF').FSOFF                                    01200000
.ERROR   IHBERMAC 36,,&A                                                01250000
         MEXIT                                                          01300000
.ERROR1  IHBERMAC 54,,,                                                 01310000
         MEXIT                                                          01320000
.ERROR2  IHBERMAC 54,,RSHWKEY                                           01326000
         MEXIT                                                          01332000
.ERROR3  ANOP  ,                                                   @L01 01337500
.*       IHBERMAC 1020,RSHWKEY,&A                                  @L01 01338000
         MNOTE  12,'***  IHB280  RSHWKEY INVALID WITH &A.'         @L01 01338500
         MEXIT                                                          01344000
.FSON    ANOP                                                           01350000
&NAME    LA    1,&SYSFS+&SYSINIT -      FLAGS FOR MODE/INITIAL CALL     01400000
         SLL   1,24 -                   SHIFT TO HIGH ORDER BYTE        01450000
         AGO   .PFCD                                                    01500000
.FSOFF   ANOP                                                           01550000
&NAME    SR    1,1 -                    CLEAR PARAMETER REGISTER        01600000
.PFCD    ANOP                                                           01608000
         AIF   ('&A' EQ 'OFF').ENTRYCD                                  01616000
         AIF   (&RSHWKEY EQ 64).ENTRYCD                                 01620000
         SR    0,0 -                    CLEAR REGISTER                  01624000
         LA    0,&RSHWKEY -             LOAD RSHWKEY                    01632000
         OR    1,0 -                    RSHWKEY IN RIGHTMOST BYTE       01638000
.ENTRYCD ANOP                                                           01650000
         LA    0,18 -                   LOAD ENTRY CODE                 01700000
         SLL   0,24 -                   SHIFT TO HIGH ORDER BYTE        01750000
         SVC   94 -                     TERMINAL CONTROL MACRO SVC      01800000
         MEND                                                           01850000
./ ENDUP
/*
//

