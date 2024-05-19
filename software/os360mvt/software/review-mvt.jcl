//SY20ZR00 JOB 1,REVIEW,MSGCLASS=A,CLASS=A,REGION=256K
//*
//*   PROBLEM DESCRIPTION(S):
//*     REVIEW  -
//*       Refit TSO REVIEW command and REVSMF subroutine from CBT
//*       tape 249 file 296 to run under MVT.
//*
//*   SPECIAL CONDITIONS:
//*     DEPENDENCY:
//*       Assembly of REVIEW and REVSMF requires the OS/VS XF
//*       assembler IFOX00.
//*
//*     DEPENDENCY:
//*       REVIEW and REVSMF use System/370 instructions.  The
//*       hardware or hardware emulator MVT is running on must
//*       support System/370 instructions, or the version of MVT
//*       being used must include software simulation of System/370
//*       instructions.
//*
//*     DEPENDENCY:
//*       Usermod TMVT818 or equivalent must be installed to add
//*       STLINENO functionality to MVT SVC 94.  Otherwise STLINENO
//*       acts as a no-op.
//*
//*   COMMENTS:
//*     LAST CHANGE:  2010/06/22
//*
//*     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:
//*
//*     MODULES
//*       REVIEW
//*       REVSMF
//*
//*     MACROS
//*       STFSMODE
//*       STLINENO
//*
/*JOBPARM LINES=9999,LINECT=0
//*MAIN LINES=100
//*
//*-----------------------------------------------------------------***
//*     Assemble REVIEW.                                            ***
//*-----------------------------------------------------------------***
//ASM1    EXEC PGM=IFOX00,REGION=256K,
//             PARM=(DECK,NOOBJECT,NORLD,RENT,'XREF(SHORT)')
//SYSLIB   DD  DSN=SYS1.MODGEN,DISP=SHR,DCB=BLKSIZE=3360
//         DD  DSN=SYS1.MACLIB,DISP=SHR
//SYSUT1   DD  UNIT=SYSDA,SPACE=(CYL,(10,5))
//SYSUT2   DD  UNIT=SYSDA,SPACE=(CYL,(10,5))
//SYSUT3   DD  UNIT=SYSDA,SPACE=(CYL,(10,5))
//SYSPUNCH DD  UNIT=SYSDA,SPACE=(TRK,(15,5,2)),DISP=(NEW,PASS),
//         DCB=BLKSIZE=3200,DSN=&&OBJSET(REVIEW)
//SYSPRINT DD  SYSOUT=A
//SYSTERM  DD  SYSOUT=A
//SYSIN DD *
         PRINT OFF
         MACRO                                                          00050000
&NAME    STFSMODE &A,&INITIAL=NO,&RSHWKEY=64 PROTOTYPE                  00100000
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
.* Note - In MVT, STFSMODE returns a return code of zero, but      @D01 01050200
.*        otherwise doesn't do anything.                           @D01 01050400
.*                                                                 @D01 01050600
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
.ERROR3  IHBERMAC 1020,RSHWKEY,&A                                       01338000
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
.* Note - In MVT, STLINENO operates as in MVS 3.8.  It sets the    @D01 01050200
.*        line for the next non-full screen TPUT.  STLINENO with   @D01 01050400
.*        LINE=1 does not automatically clear the screen as it     @D01 01050600
.*        does in more modern versions of MVS.                     @D01 01050800
.*                                                                 @D01 01051000
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
         PRINT ON
         TITLE '  R E V I E W   '                                       00320000
*********************************************************************** 00330000
*                                                                     * 00340000
*        REVIEW - TSO COMMAND FOR EXAMINING A DATA SET                * 00350000
*                 ON A 3270 DISPLAY STATION SCREEN                    * 00360000
*                                                                     * 00370000
*********************************************************************** 00380000
*                                                                       00390000
*        WRITTEN BY. BILL GODFREY, PRC (PLANNING RESEARCH CORPORATION). 00400000
*        INSTALLATION. AIR FORCE DATA SERVICES CENTER, PENTAGON.        00410000
*        DATE WRITTEN. JANUARY 19 1981.                                 00420000
*        DATE UPDATED. MARCH 18 1982.                                   00430000
*        ATTRIBUTES. RE-ENTRANT.                                        00440000
*        LOCAL MACROS USED. DCS. (DEFINE CONSTANTS FOR SCREEN).         00450000
*        DESCRIPTION.                                                   00460000
*         THIS TSO COMMAND DISPLAYS A DATA SET ON A 3270 TERMINAL       00470000
*         IN FULLSCREEN MODE.                                           00480000
*                                                                       00490000
*         LOG OF CHANGES.                                               00500000
*         27MAR81 - ALLOW PFK DEFINITIONS TO BE CHANGED.                00510000
*         10JUL81 - IKJRLSA SOON AFTER PARSE. VOLUME KEYWORD.           00520000
*                   FIXED BUG OF IKJEFF18 BEING CALLED TWICE.           00530000
*                   MISCELLANEOUS INTERNAL RESTRUCTURING OF CODE TO     00540000
*                   TO ALLOW FUTURE CHANGES. OLD CODE HAD USED UP       00550000
*                   ALL 3 BASE REGISTERS.                               00560000
*         14JUL81 - BYPASS IKJEHDEF IF VOL SPECIFIED. ADD UNIT KW.      00570000
*                   HEX SUBCOMMAND WITHOUT OPERANDS WILL FLIP-FLOP      00580000
*                   BETWEEN HEX ON AND HEX OFF. PFK9 IS NOW HEX         00590000
*                   INSTEAD OF HEX ON. PFK12 IS NO LONGER HEX OFF.      00600000
*                   SMF SUBCOMMAND WITHOUT OPERANDS WILL FLIP FLOP.     00610000
*         20JUL81 - FINDSMF SUBCOMMAND ADDED, FOR SMF RECORD TYPE.      00620000
*         29SEP81 - ACCEPT PFK 13-24 CORRESPONDING TO 1-12.             00630000
*                   FIX BUG AT CLC FOR REPLY 'FINDSMF'.                 00640000
*         29JAN82 - SET UP IOPL BEFORE DOING GTSIZE/ERRTERM.            00650000
*         29JAN82 - ALLOW MEMBER TO BE SPECIFIED WITH DDNAME, SO USER   00660000
*                   CAN DO THINGS LIKE REVIEW SYSPROC(MEMBER) FILE      00670000
*                   (AND SHOW DDNAME+NNN IF CONCATENATED).              00680000
*                   REPLACE 'POINT' WITH 'FIND' TO SUPPORT              00690000
*                   CONCATENATED PDS'S.                                 00700000
*         18MAR82 - CALL REVSMF SUBPROGRAM TO FORMAT SMF DATA.          00710000
*                                                                       00720000
*         18JUN84 - MODIFIED BY A. BRUCE LELAND SIMILAR TO HEL COMMAND: 00730000
*                 . MOVED DCS MACRO TO FRONT FOR IFOX ASSEMBLER     ABL 00740000
*                 . ADDED HELP SUBCOMMAND                           ABL 00750000
*                 . ADDED TSO SUBCOMMAND                            ABL 00760000
*                 . CHANGED FIND TO ALWAYS REPOSITION THE SCREEN    ABL 00770000
*                 . CHANGED DEBUG TO SHOW PREVIOUS TGET RESULTS     ABL 00780000
*                 . ADDED CHECK FOR VTAM RESHOW                     ABL 00790000
*                 . ADDED VTAM FULL-SCREEN CONTROLS                 ABL 00800000
*                 . CHANGED DEFAULT DISPLAY TO ASIS                 ABL 00810000
*                 . ADDED PFK SHIFT WITH NUMERIC (AND M) ENTRY      ABL 00820000
*                 . CHANGED PFK INITIAL DEFINITIONS SLIGHTLY        ABL 00830000
*                 . ALLOW USE BY TERMINALS WITH MORE THAN 24 ROWS   ABL 00840000
*                 . CHANGED BLANK ROUTINE TO CLEAR LAST SCREEN BYTE ABL 00850000
*                 . CHANGED RIGHT ROUTINE SLIGHTLY                  ABL 00860000
*                 . MODIFIED THE HELP MEMBER (FOR SUBCOMMANDS)      ABL 00870000
*                                                                       00880000
*         11JUL84 - TSO SUBCOMMAND: IF QUOTED, BACK UP 1 CHARACTER  ABL 00890000
*                 . TSO SUBCOMMAND: TURN FULLSCREEN OFF AND ON      ABL 00900000
*                                                                       00910000
*         14MAY85 - MORE CHANGES TO SYNCHRONIZE WITH HEL COMMAND:   ABL 00920000
*                 . SUPPORT WAS ADDED FOR 3270-X TERMINALS          ABL 00930000
*                 . 3278-5 TERMINALS ALWAYS USE 24X80 DISPLAY MODE  ABL 00940000
*                 . PF 2 AND 4 DEFAULTS CHANGED TO MATCH HEL        ABL 00950000
*                                                                       00960000
*        NOTES.                                                         00970000
*         KNOWN BUGS.                                                   00980000
*          WHEN BROWSING VARIABLE SPANNED RECORDS, THE 'UP'             00990000
*          SUBCOMMAND WILL GIVE UNPREDICTABLE RESULTS.                  01000000
*          SO WILL 'DOWN' IF THE RECORDS YOU ARE GOING DOWN TO          01010000
*          HAVE ALREADY BEEN READ ONCE AND ARE THEREFORE IN THE         01020000
*          CHECKPOINT TABLE.                                            01030000
*          MORE INFORMATION MUST BE SAVED IN THE CHECKPOINT TABLE       01040000
*          IN ORDER TO FIX THIS BUG. (SMF IS VARIABLE SPANNED).         01050000
*                                                                       01060000
*********************************************************************** 01070000
         EJECT                                                          01080000
*********************************************************************** 01090000
*                                                                       01100000
*         SYNTAX -   REVIEW DSNAME                                      01110000
*          OR        REVIEW DDNAME FILE                                 01120000
*                                                                       01130000
*         IF THE SECOND OPERAND IS 'FILE', THAT MEANS THE               01140000
*         FIRST OPERAND IS NOT A DSNAME BUT A DDNAME.                   01150000
*                                                                       01160000
*         THE SCREEN WILL NORMALLY CONTAIN 20 RECORDS, ONE PER LINE,    01170000
*         80 CHARACTERS PER LINE.  TO DISPLAY RECORDS LONGER THAN 80    01180000
*         BYTES, THERE IS A 'RIGHT' SUBCOMMAND WHICH CAUSES DATA TO     01190000
*         THE RIGHT OF THE DATA ON THE SCREEN TO BE DISPLAYED.          01200000
*         THE 'LEFT' SUBCOMMAND CAN THEN BE USED TO DISPLAY DATA        01210000
*         BACK TOWARDS THE FRONT OF THE RECORD.                         01220000
*                                                                       01230000
*         SUBCOMMANDS -                                                 01240000
*         UP       - DISPLAY RECORDS ABOVE THOSE ON THE SCREEN          01250000
*         DOWN     - DISPLAY RECORDS BELOW THOSE ON THE SCREEN          01260000
*         TOP      - DISPLAY THE FIRST RECORDS                          01270000
*         TSO      - ISSUE A TSO COMMAND                            ABL 01280000
*         HELP     - DISPLAY A HELP SCREEN                          ABL 01290000
*         BOTTOM   - DISPLAY THE LAST RECORDS                           01300000
*         RIGHT    - DISPLAY DATA TO THE RIGHT                          01310000
*         LEFT     - DISPLAY DATA TO THE LEFT                           01320000
*         LIST     - DISPLAY A SPECIFIED RECORD NUMBER                  01330000
*         FIND     - DISPLAY A RECORD CONTAINING A SPECIFIED STRING     01340000
*         CAPS     - TRANSLATE LOWER CASE LETTERS TO CAPS               01350000
*         ASIS     - LEAVE LOWER CASE LETTERS IN LOWER CASE             01360000
*         HEX ON   - DISPLAY DATA IN HEXADECIMAL FORMAT                 01370000
*         HEX OFF  - TURN OFF HEXADECIMAL FORMATTING                    01380000
*         HEX      - SWITCH HEXADECIMAL FORMATTING ON/OFF.              01390000
*         PFKNN    - CHANGE DEFINITION OF PF KEY NN.                    01400000
*         SMF ON   - DISPLAY SMF RECORDS WITH TIME AND DATE FORMATTED   01410000
*         SMF OFF  - TURN OFF SMF FORMATTING                            01420000
*         SMF      - SWITCH SMF FORMATTING ON/OFF.                      01430000
*         FINDSMF  - FIND A TYPE OF SMF RECORD                          01440000
*         MEMBER   - DISPLAY A DIFFERENT MEMBER OF THE SAME PDS         01450000
*         END      - END THE COMMAND                                    01460000
*                                                                       01470000
*         WHEN 'UP' OR 'DOWN' DOES NOT SPECIFY HOW FAR UP OR DOWN,      01480000
*         THE 'RANGE' VALUE IS USED.  THIS VALUE IS ALWAYS DISPLAYED    01490000
*         NEAR THE TOP OF THE SCREEN AND CAN BE CHANGED BY MOVING       01500000
*         THE CURSOR OVER THE VALUE AND TYPING IN A NEW VALUE.          01510000
*         THE NEW VALUE WILL REMAIN IN EFFECT UNTIL THE COMMAND         01520000
*         ENDS OR UNTIL YOU CHANGE IT AGAIN.                            01530000
*                                                                       01540000
*********************************************************************** 01550000
         EJECT                                                          01560000
*********************************************************************** 01570000
*                                                                       01580000
*         PROGRAM FUNCTION KEYS                                         01590000
*                                                                       01600000
*         SOME OF THE SUBCOMMANDS DO NOT HAVE TO BE TYPED IN            01610000
*         IF THE TERMINAL IS EQUIPPED WITH PROGRAM FUNCTION KEYS.       01620000
*         FOR EXAMPLE, HITTING KEY 'PF8' IS EQUIVALENT TO               01630000
*         TYPING IN 'DOWN' AND HITTING THE 'ENTER' KEY.                 01640000
*                                                                       01650000
*         HERE ARE THE MEANINGS ASSIGNED TO EACH PF KEY.                01660000
*                                                                       01670000
*         ----------------------------------------------------          01680000
*         I                I                I                I          01690000
*         I      PF1       I      PF2       I      PF3       I          01700000
*         I                I                I                I          01710000
*         I      HELP      I                I      END       I      ABL 01720000
*         I                I                I                I          01730000
*         ----------------------------------------------------          01740000
*         I                I                I                I          01750000
*         I      PF4       I      PF5       I      PF6       I          01760000
*         I                I                I                I          01770000
*         I      END       I      FIND      I                I      ABL 01780000
*         I                I                I                I          01790000
*         ----------------------------------------------------          01800000
*         I                I                I                I          01810000
*         I      PF7       I      PF8       I      PF9       I          01820000
*         I                I                I                I          01830000
*         I      UP        I      DOWN      I      HEX       I          01840000
*         I                I                I                I          01850000
*         ----------------------------------------------------          01860000
*         I                I                I                I          01870000
*         I      PF10      I      PF11      I      PF12      I          01880000
*         I                I                I                I          01890000
*         I      LEFT      I      RIGHT     I                I          01900000
*         I                I                I                I          01910000
*         ----------------------------------------------------          01920000
*                                                                       01930000
*         THE SCREEN IS WRITTEN USING THE 'EXPANDED FULLSCREEN'         01940000
*         (EXPFLS=YES) FEATURE OF TCAM, WHICH REQUIRES THAT THE         01950000
*         FIRST ORDER IN THE BUFFER BE 'SBA,24,80' OR 'SBA,24,79'.      01960000
*         THE FIRST IS USED TO SUPPRESS LINE COUNTING, AND THE          01970000
*         LATTER IS USED TO RESET THE LINE COUNTER.                     01980000
*                                                                       01990000
*         IF AN UNQUALIFIED DATA SET NAME IS ENTERED, THE               02000000
*         CATALOG MUST BE READ TWICE, ONCE TO APPEND A                  02010000
*         TRAILING QUALIFIER IF NECESSARY, AND AGAIN DURING             02020000
*         ALLOCATION. THE USER CAN ELIMINATE THE FIRST BY USING         02030000
*         THE FULLY QUALIFIED NAME, PREFIX AND ALL, IN QUOTES,          02040000
*         OR BY ENTERING ALL BUT THE PREFIX, WITHOUT QUOTES,            02050000
*         PLUS THE KEYWORD 'Q'. THE LATTER IS MUCH SIMPLER              02060000
*         AND GIVES THE SAME PERFORMANCE IMPROVEMENT AS A               02070000
*         FULLY QUALIFIED NAME.                                         02080000
*                                                                       02090000
*         THE 'FILE' KEYWORD IS USEFUL FOR LOOKING AT TEMPORARY         02100000
*         DATA SETS.  IT TELLS THE COMMAND TO TREAT THE FIRST           02110000
*         OPERAND AS A FILENAME (DDNAME) INSTEAD OF A DSNAME.           02120000
*         WHATEVER DATA SET IS CURRENTLY ALLOCATED TO THE               02130000
*         FILENAME WILL BE DISPLAYED (AND QUICKLY, BECAUSE              02140000
*         THE COMMAND DOESNT HAVE TO ALLOCATE ANYTHING).                02150000
*                                                                       02160000
*********************************************************************** 02170000
         SPACE                                                          02180000
         MACRO                                                          02190000
&NAME    MSG   &MSG                                                     02200000
&NAME    DC    AL2(&NAME.L-2)                                           02210000
         DC    C&MSG                                                    02220000
&NAME.L  EQU   *-&NAME                                                  02230000
         MEND                                                           02240000
         MACRO                                                          02250000
&NAME    DCS                                                            02260000
.********************************************************************** 02270000
.*                                                                    * 02280000
.*        DCS  -  DEFINE CONSTANT FOR SCREEN                          * 02290000
.*                                                                    * 02300000
.*        WRITTEN BY BILL GODFREY                                     * 02310000
.*        PLANNING RESEARCH CORPORATION                               * 02320000
.*        PRC COMPUTER CENTER, MCLEAN VA 22101                        * 02330000
.*        DATE WRITTEN. JANUARY 8 1981.                               * 02340000
.*        DATE UPDATED. MARCH 18 1982. (ROW AND COL IN PARENS)        * 02350000
.*                                                                    * 02360000
.*        THIS MACRO IS USED FOR CODING A FULLSCREEN 3270 DISPLAY.    * 02370000
.*                                                                    * 02380000
.*        SAMPLE                                                      * 02390000
.*           DCS    AL1(WCC),SBA,(1,1),RTA,(7,1),X'00',IC             * 02400000
.*                                                                    * 02410000
.*        IT SIMPLIFIES THE CODING OF A SCREEN IN THE FOLLOWING WAYS. * 02420000
.*        .  BUFFER ADDRESSES ARE SPECIFIED AS ROW AND COLUMN NUM-    * 02430000
.*           BER.  THE MACRO TRANSLATES THEM INTO THE 3270 CODE.      * 02440000
.*        .  ORDERS ARE SPECIFIED BY NAME, SUCH AS 'SBA' AND 'SF',    * 02450000
.*           SO YOU DONT HAVE TO KNOW THE HEX CODES FOR THEM,         * 02460000
.*        .  COMMONLY USED ATTRIBUTE BYTES ARE SPECIFIED BY NAME      * 02470000
.*           (A SET OF RESERVED NAMES) SO YOU DONT HAVE TO KNOW       * 02480000
.*           THE HEX CODES FOR THEM.                                  * 02490000
.*        .  IT SAVES A LOT OF DOCUMENTATION WORK, AND MAKES          * 02500000
.*           THE CODE EASIER FOR OTHERS TO UNDERSTAND.                * 02510000
.*                                                                    * 02520000
.*        THE USER OF THE MACRO MUST STILL UNDERSTAND HOW A SCREEN    * 02530000
.*        IS CONSTRUCTED BEFORE USING IT. THE MACRO MERELY MAKES IT   * 02540000
.*        EASIER TO SPECIFY THE VALUES.  IT DOES VERY LITTLE ERROR    * 02550000
.*        CHECKING.  FOR INSTANCE, IT DOES NOT CHECK TO SEE IF        * 02560000
.*        YOU FOLLOW AN 'SBA' WITH A BUFFER ADDRESS.  IT IS POSSIBLE  * 02570000
.*        TO CODE A THOROUGHLY INVALID SCREEN.                        * 02580000
.*                                                                    * 02590000
.*        THE MACRO MAY HAVE ANY NUMBER OF OPERANDS, CONSISTING OF    * 02600000
.*        ANY COMBINATION OF THE FOLLOWING.                           * 02610000
.*                                                                    * 02620000
.*        .  AN ORDER.                                                * 02630000
.*           VALID ORDERS ARE: SBA, SF, RTA, IC, PT, EUA.             * 02640000
.*        .  A BUFFER ADDRESS IN PARENTHESES.                         * 02650000
.*           IF AN OPERAND IS IN PARENTHESES, IT IS ASSUMED THAT      * 02660000
.*           THE ROW AND COLUMN NUMBER ARE BETWEEN THE PARENS,        * 02670000
.*           SEPARATED BY A COMMA.  EXAMPLE: (1,1)                    * 02680000
.*           THIS FORM OF BUFFER ADDRESS IS NEW AS OF MARCH 18 1982.  * 02690000
.*        .  A ROW OR COLUMN NUMBER OF A BUFFER ADDRESS (OLD FORMAT). * 02700000
.*           IF AN OPERAND IS NUMERIC, IT IS ASSUMED TO BE            * 02710000
.*           A ROW OR COLUMN NUMBER.  IT TAKES 2 OPERANDS TO          * 02720000
.*           SPECIFY THE BUFFER ADDRESS (ROW AND COLUMN) SO NUMERIC   * 02730000
.*           OPERANDS MUST ALWAYS BE SPECIFIED IN PAIRS, THE          * 02740000
.*           FIRST BEING THE ROW AND THE SECOND BEING THE COLUMN.     * 02750000
.*           THIS FORMAT IS SUPPORTED ONLY FOR COMPATIBILITY WITH     * 02760000
.*           THE ORIGINAL VERSION OF THIS MACRO.                      * 02770000
.*        .  AN ATTRIBUTE BYTE.                                       * 02780000
.*           VALID ATTRIBUTE BYTES ARE:                               * 02790000
.*           UNPLO -  UNPROTECTED NORMAL INTENSITY                    * 02800000
.*           UNPHI  - UNPROTECTED HIGH INTENSITY                      * 02810000
.*           UNPNP  - UNPROTECTED NO-DISPLAY                          * 02820000
.*           PROLO  - PROTECTED NORMAL INTENSITY                      * 02830000
.*           PROLOS - PROTECTED NORMAL INTENSITY AUTO-SKIP            * 02840000
.*           PROHI  - PROTECTED HIGH INTENSITY                        * 02850000
.*           PROHIS - PROTECTED HIGH INTENSITY AUTO-SKIP              * 02860000
.*        .  A HEX, CHARACTER, OR ADDRESS CONSTANT.                   * 02870000
.*           FOR EXAMPLE, X'00', OR C'ENTER SIGNON'                   * 02880000
.*           THIS CAN BE USED FOR DATA WITHIN FIELDS OR FOR           * 02890000
.*           ATTRIBUTE BYTES, ORDERS, THE 'WCC', OR BUFFER            * 02900000
.*           ADDRESSES (IF YOU WANT TO FIGURE THEM OUT).              * 02910000
.*                                                                    * 02920000
.*        IF THE OPERANDS DO NOT ALL FIT ON ONE LINE, YOU CAN         * 02930000
.*        EITHER CONTINUE THE LINE IN THE STANDARD ASSEMBLER WAY      * 02940000
.*        OR CODE THE MACRO AGAIN ON THE NEXT LINE WITH THE           * 02950000
.*        REMAINING OPERANDS.  THE RESULT IS THE SAME EITHER WAY.     * 02960000
.*                                                                    * 02970000
.*        THE BUFFER ADDRESS CONVERSIONS ARE FOR A                    * 02980000
.*        SCREEN SIZE OF 24 ROWS BY 80 COLUMNS. FOR SCREENS           * 02990000
.*        OF OTHER DIMENSIONS (43 BY 80, 12 BY 40) THE MACRO          * 03000000
.*        NEEDS ONLY A FEW CHANGES.                                   * 03010000
.*                                                                    * 03020000
.*        WARNING: IF YOU CODE THE MACRO WITH A LABEL IN COLUMN 1,    * 03030000
.*        AND YOU LIKE USING LENGTH ATTRIBUTES, BEWARE THAT THE       * 03040000
.*        LENGTH ATTRIBUTE OF THE LABEL IS NOT NECESSARILY THE        * 03050000
.*        TOTAL LENGTH OF THE DATA GENERATED BY THE MACRO.            * 03060000
.*                                                                    * 03070000
.********************************************************************** 03080000
.*                                                                      03090000
         LCLA  &R,&C,&P,&Q                                              03100000
         LCLA  &AN,&AS,&AL                                              03110000
         LCLB  &B,&NUMERIC,&INTEGER                                     03120000
         LCLC  &T(64)                                                   03130000
         LCLC  &N,&ROW,&COL                                             03140000
         LCLC  &CS,&STRING                                              03150000
&T(1)    SETC  '40'                                                     03160000
&T(2)    SETC  'C1'                                                     03170000
&T(3)    SETC  'C2'                                                     03180000
&T(4)    SETC  'C3'                                                     03190000
&T(5)    SETC  'C4'                                                     03200000
&T(6)    SETC  'C5'                                                     03210000
&T(7)    SETC  'C6'                                                     03220000
&T(8)    SETC  'C7'                                                     03230000
&T(9)    SETC  'C8'                                                     03240000
&T(10)   SETC  'C9'                                                     03250000
&T(11)   SETC  '4A'                                                     03260000
&T(12)   SETC  '4B'                                                     03270000
&T(13)   SETC  '4C'                                                     03280000
&T(14)   SETC  '4D'                                                     03290000
&T(15)   SETC  '4E'                                                     03300000
&T(16)   SETC  '4F'                                                     03310000
.*                                                                      03320000
&T(17)   SETC  '50'                                                     03330000
&T(18)   SETC  'D1'                                                     03340000
&T(19)   SETC  'D2'                                                     03350000
&T(20)   SETC  'D3'                                                     03360000
&T(21)   SETC  'D4'                                                     03370000
&T(22)   SETC  'D5'                                                     03380000
&T(23)   SETC  'D6'                                                     03390000
&T(24)   SETC  'D7'                                                     03400000
&T(25)   SETC  'D8'                                                     03410000
&T(26)   SETC  'D9'                                                     03420000
&T(27)   SETC  '5A'                                                     03430000
&T(28)   SETC  '5B'                                                     03440000
&T(29)   SETC  '5C'                                                     03450000
&T(30)   SETC  '5D'                                                     03460000
&T(31)   SETC  '5E'                                                     03470000
&T(32)   SETC  '5F'                                                     03480000
.*                                                                      03490000
&T(33)   SETC  '60'                                                     03500000
&T(34)   SETC  '61'                                                     03510000
&T(35)   SETC  'E2'                                                     03520000
&T(36)   SETC  'E3'                                                     03530000
&T(37)   SETC  'E4'                                                     03540000
&T(38)   SETC  'E5'                                                     03550000
&T(39)   SETC  'E6'                                                     03560000
&T(40)   SETC  'E7'                                                     03570000
&T(41)   SETC  'E8'                                                     03580000
&T(42)   SETC  'E9'                                                     03590000
&T(43)   SETC  '6A'                                                     03600000
&T(44)   SETC  '6B'                                                     03610000
&T(45)   SETC  '6C'                                                     03620000
&T(46)   SETC  '6D'                                                     03630000
&T(47)   SETC  '6E'                                                     03640000
&T(48)   SETC  '6F'                                                     03650000
.*                                                                      03660000
&T(49)   SETC  'F0'                                                     03670000
&T(50)   SETC  'F1'                                                     03680000
&T(51)   SETC  'F2'                                                     03690000
&T(52)   SETC  'F3'                                                     03700000
&T(53)   SETC  'F4'                                                     03710000
&T(54)   SETC  'F5'                                                     03720000
&T(55)   SETC  'F6'                                                     03730000
&T(56)   SETC  'F7'                                                     03740000
&T(57)   SETC  'F8'                                                     03750000
&T(58)   SETC  'F9'                                                     03760000
&T(59)   SETC  '7A'                                                     03770000
&T(60)   SETC  '7B'                                                     03780000
&T(61)   SETC  '7C'                                                     03790000
&T(62)   SETC  '7D'                                                     03800000
&T(63)   SETC  '7E'                                                     03810000
&T(64)   SETC  '7F'                                                     03820000
.*                                                                      03830000
&N       SETC  '&NAME'                                                  03840000
&AN      SETA  N'&SYSLIST          NUMBER OF OPERANDS                   03850000
&AS      SETA  0                                                        03860000
&B       SETB  0 FALSE                                                  03870000
.EACH    AIF   (&AN EQ 0).EPILOG                                        03880000
&AS      SETA  &AS+1                                                    03890000
&CS      SETC  '&AS'                                                    03900000
&AL      SETA  K'&SYSLIST(&AS)                                          03910000
         AIF   (T'&SYSLIST(&AS) EQ 'O').NEXT                            03920000
         AIF   ('&SYSLIST(&AS)'(1,1) EQ '(').PAIR                       03930000
&NUMERIC SETB  (T'&SYSLIST(&AS) EQ 'N')                                 03940000
&INTEGER SETB  ('&SYSLIST(&AS)'(1,1) GE '0')                            03950000
         AIF   (&NUMERIC AND &INTEGER).ROWCOL                           03960000
         AIF   (NOT &B).ROWCOLX                                         03970000
         MNOTE 4,'             &CS.) MISSING COLUMN NUMBER'             03980000
&B       SETB  0 FALSE                                                  03990000
.ROWCOLX ANOP                                                           04000000
&STRING  SETC  '&SYSLIST(&AS)'                                          04010000
.*             ORDERS                                                   04020000
         AIF   ('&STRING' EQ 'SBA').SBA                                 04030000
         AIF   ('&STRING' EQ 'SF').SF                                   04040000
         AIF   ('&STRING' EQ 'RTA').RTA                                 04050000
         AIF   ('&STRING' EQ 'IC').IC                                   04060000
         AIF   ('&STRING' EQ 'PT').PT                                   04070000
         AIF   ('&STRING' EQ 'EUA').EUA                                 04080000
.*             ATTRIBUTES                                               04090000
         AIF   ('&STRING' EQ 'UNPLO').UNPLO                             04100000
         AIF   ('&STRING' EQ 'UNPHI').UNPHI                             04110000
         AIF   ('&STRING' EQ 'UNPNP').UNPNP                             04120000
         AIF   ('&STRING' EQ 'PROLO').PROLO                             04130000
         AIF   ('&STRING' EQ 'PROLOS').PROLOS                           04140000
         AIF   ('&STRING' EQ 'PROHI').PROHI                             04150000
         AIF   ('&STRING' EQ 'PROHIS').PROHIS                           04160000
.*             CONSTANTS                                                04170000
.*             IF THE OPERAND IS NONE OF THE ABOVE, IT IS               04180000
.*             PRESUMED TO BE ANY VALID 'DC' CONSTANT.                  04190000
.DC      ANOP                                                           04200000
&N       DC    &STRING                                                  04210000
         AGO   .NEXT                                                    04220000
.SBA     ANOP                                                           04230000
&N       DC    X'11'               SET BUFFER ADDRESS                   04240000
         AGO   .NEXT                                                    04250000
.SF      ANOP                                                           04260000
&N       DC    X'1D'               START FIELD                          04270000
         AGO   .NEXT                                                    04280000
.RTA     ANOP                                                           04290000
&N       DC    X'3C'               REPEAT TO ADDRESS                    04300000
         AGO   .NEXT                                                    04310000
.IC      ANOP                                                           04320000
&N       DC    X'13'               INSERT CURSOR                        04330000
         AGO   .NEXT                                                    04340000
.PT      ANOP                                                           04350000
&N       DC    X'05'               PROGRAM TAB                          04360000
         AGO   .NEXT                                                    04370000
.EUA     ANOP                                                           04380000
&N       DC    X'12'               ERASE UNPROTECTED TO ADDRESS         04390000
         AGO   .NEXT                                                    04400000
.UNPLO   ANOP                                                           04410000
&N       DC    X'40'               UNPROTECTED NORMAL INTENSITY         04420000
         AGO   .NEXT                                                    04430000
.UNPHI   ANOP                                                           04440000
&N       DC    X'C8'               UNPROTECTED HIGH INTENSITY           04450000
         AGO   .NEXT                                                    04460000
.UNPNP   ANOP                                                           04470000
&N       DC    X'4C'               UNPROTECTED NO-DISPLAY               04480000
         AGO   .NEXT                                                    04490000
.PROLO   ANOP                                                           04500000
&N       DC    X'60'               PROTECTED NORMAL INTENSITY           04510000
         AGO   .NEXT                                                    04520000
.PROLOS  ANOP                                                           04530000
&N       DC    X'F0'               PROTECTED NORMAL INTENSITY SKIP      04540000
         AGO   .NEXT                                                    04550000
.PROHI   ANOP                                                           04560000
&N       DC    X'E8'               PROTECTED HIGH INTENSITY             04570000
         AGO   .NEXT                                                    04580000
.PROHIS  ANOP                                                           04590000
&N       DC    X'F8'               PROTECTED HIGH INTENSITY SKIP        04600000
         AGO   .NEXT                                                    04610000
.********************************************************************** 04620000
.PAIR    ANOP                                                           04630000
         AIF   (N'&SYSLIST(&AS) NE 2).PERR1                             04640000
&NUMERIC SETB  (T'&SYSLIST(&AS,1) EQ 'N')                               04650000
&INTEGER SETB  ('&SYSLIST(&AS,1)'(1,1) GE '0')                          04660000
         AIF   (NOT &NUMERIC OR NOT &INTEGER).PERR2                     04670000
&R       SETA  &SYSLIST(&AS,1)                                          04680000
&NUMERIC SETB  (T'&SYSLIST(&AS,2) EQ 'N')                               04690000
&INTEGER SETB  ('&SYSLIST(&AS,2)'(1,1) GE '0')                          04700000
         AIF   (NOT &NUMERIC OR NOT &INTEGER).PERR2                     04710000
&C       SETA  &SYSLIST(&AS,2)                                          04720000
         AIF   (&R LT 1 OR &R GT 43).ROWERR   *** WAS 24                04730000
         AIF   (&C LT 1 OR &C GT 80).COLERR                             04740000
&P       SETA  (&R-1)*80+&C-1                                           04750000
&Q       SETA  &P/64               QUOTIENT                             04760000
&R       SETA  &P-&Q*64+1          REMAINDER+1                          04770000
&Q       SETA  &Q+1                QUOTIENT+1                           04780000
&N       DC    X'&T(&Q)&T(&R)'     ROW AND COLUMN                       04790000
         AGO   .NEXT                                                    04800000
.PERR1   MNOTE 4,'             &CS.) PARENS FOUND BUT NOT 2 NUMBERS'    04810000
         MEXIT                                                          04820000
.PERR2   MNOTE 4,'             &CS.) NON NUMERIC ROW/COLUMN'            04830000
         MEXIT                                                          04840000
.********************************************************************** 04850000
.ROWCOL  ANOP                                                           04860000
         AIF   (&B).COL            BRANCH IF ROW HAS BEEN CAPTURED      04870000
&R       SETA  &SYSLIST(&AS)                                            04880000
&B       SETB  1 TRUE              SET ROW-HAS-BEEN-CAPTURED            04890000
         AGO   .NEXTR                                                   04900000
.COL     ANOP                                                           04910000
&C       SETA  &SYSLIST(&AS)                                            04920000
&B       SETB  0 FALSE             RESET SWITCH                         04930000
         AIF   (&R LT 1 OR &R GT 43).ROWERR    *** WAS 24               04940000
         AIF   (&C LT 1 OR &C GT 80).COLERR                             04950000
&P       SETA  (&R-1)*80+&C-1                                           04960000
&Q       SETA  &P/64               QUOTIENT                             04970000
&R       SETA  &P-&Q*64+1          REMAINDER+1                          04980000
&Q       SETA  &Q+1                QUOTIENT+1                           04990000
&N       DC    X'&T(&Q)&T(&R)'     ROW AND COLUMN                       05000000
         AGO   .NEXT                                                    05010000
.ROWERR  MNOTE 4,'             &CS.) VALUE &R INVALID, MUST BE 1 TO 43' 05020000
         AGO   .NEXT                                                    05030000
.COLERR  MNOTE 4,'             &CS.) VALUE &C INVALID, MUST BE 1 TO 80' 05040000
.NEXT    ANOP                                                           05050000
&N       SETC  ''                  TURN OFF NAME                        05060000
.NEXTR   ANOP                                                           05070000
&AN      SETA  &AN-1                                                    05080000
         AGO   .EACH                                                    05090000
.EPILOG  ANOP                                                           05100000
         MEND                                                           05110000
         SPACE                                                          05120000
         GBLB  &MVS                                                     05130000
&MVS     SETB  0                   1 - MVS   0 - SVS,MVT           @L01 05140000
         SPACE                                                          05150000
REVIEW   START                                                          05160000
         USING *,R10,R11,R12                                            05170000
CSECT1   EQU   *                                                        05180000
         B     @PROLOG-*(,R15)                                          05190000
         DC    AL1(11),CL11'REVIEW '                                    05200000
         DC    CL16' &SYSDATE &SYSTIME '                                05210000
@SIZE    DC    0F'0',AL1(1),AL3(@DATAL)                                 05220000
@PROLOG  STM   14,12,12(13)                                             05230000
         LR    R10,R15             BASE                                 05240000
         LA    R15,1                                                    05250000
         LA    R11,4095(R15,R10)   BASE                                 05260000
         LA    R12,4095(R15,R11)   BASE                                 05270000
         LR    R2,R1                                                    05280000
         USING CPPL,R2                                                  05290000
         L     R0,@SIZE                                                 05300000
         GETMAIN R,LV=(0)                                               05310000
         LR    R9,R1                                                    05320000
         USING @DATA,R9                                                 05330000
         LR    R1,R9               Set start address for BXLE      @L01
         LA    R14,1               Set increment for BXLE          @L01
         L     R15,=A(@DATAL)      Get area length                 @L01
         BCTR  R15,0               Set ending address              @L01
         AR    R15,R1               for BXLE                       @L01
         MVI   0(R1),X'00'         Initialize byte to zeroes       @L01
         BXLE  R1,R14,*-4          Branch back to do next one      @L01
         ST    R13,4(,R9)          CHAIN SAVEAREA                  @L01 05340000
         ST    R9,8(,R13)          CHAIN SAVEAREA                  @L01 05350000
         LR    R13,R9              NEW SAVEAREA                    @L01 05360000
         SPACE 1                                                        05370000
         STM   R10,R12,BASE1                                            05380000
         L     R14,=A(REVIEW2)                                          05390000
         ST    R14,BASE2                                                05400000
         LA    R15,1                                                    05410000
         LA    R14,4095(R15,R14)   BASE                                 05420000
         ST    R14,BASE2+4                                              05430000
         LA    R14,4095(R15,R14)   BASE                                 05440000
         ST    R14,BASE2+8                                              05450000
         MVC   SIZE,@SIZE                                               05460000
         ST    R2,CPPLPTR                                               05470000
         MVC   REVCBUF(16),0(R2)   REVTEXT/REVUPT/REVPSCB/REVECT    ABL 05480000
         MVI   STATUS,0                                                 05490000
         MVI   MODE,0                                                   05500000
         MVI   SMFSW,0                                                  05510000
         XC    LINKAREA(8),LINKAREA                                     05520000
         SLR   R15,R15                                                  05530000
         STH   R15,RC              SET RC = 0                           05540000
         ST    R15,KOUNT                                                05550000
         ST    R15,TTRZ            SET Z = 0                            05560000
         XC    MSG(2),MSG                                               05570000
         XC    MYANS,MYANS         IN CASE IKJRLSA BEFORE PARSE         05580000
         XC    CALLSMFA,CALLSMFA                                        05590000
         SPACE                                                          05600000
************************************************************            05610000
*                                                          *            05620000
*        SET UP IOPL FOR PUTLINE                           *            05630000
*                                                          *            05640000
************************************************************            05650000
         SPACE                                                          05660000
         LA    R15,MYIOPL                                               05670000
         USING IOPL,R15                                                 05680000
         MVC   IOPLUPT(4),CPPLUPT                                       05690000
         MVC   IOPLECT(4),CPPLECT                                       05700000
         LA    R0,MYECB                                                 05710000
         ST    R0,IOPLECB                                               05720000
         XC    MYECB,MYECB                                              05730000
         LA    R0,MYPTPB                                                05740000
         ST    R0,IOPLIOPB                                              05750000
         DROP  R15                 IOPL                                 05760000
         SPACE                                                          05770000
         AIF   (NOT &MVS).SKIP1                                         05780000
         L     R15,16              LOAD CVT POINTER                     05790000
         TM    444(R15),X'80'      IS PUTLINE LOADED? (VS2)             05800000
         BNO   PUTLOAD             NO - BRANCH TO LOAD                  05810000
         L     R15,444(,R15)       YES - USE CVTPUTL                    05820000
         B     PUTLOADX            BRANCH AROUND LOAD                   05830000
.SKIP1   ANOP                                                           05840000
PUTLOAD  LA    R0,=CL8'IKJPUTL '                                        05850000
         LOAD  EPLOC=(0)                                                05860000
         LR    R15,R0              GET ENTRY ADDRESS                    05870000
         LA    R15,0(,R15)         CLEAR HI BYTE FOR DELETE ROUTINE     05880000
PUTLOADX ST    R15,MYPUTLEP        SAVE PUTLINE ENTRY ADDRESS           05890000
         SPACE                                                          05900000
************************************************************            05910000
*                                                          *            05920000
*        CHECK TERMINAL SCREEN SIZE                        *            05930000
*                                                          *            05940000
************************************************************            05950000
         SPACE                                                          05960000
         L     R1,REVECT                                            ABL 05970000
         MVC   PCMD(8),ECTPCMD-ECT(R1)                              ABL 05980000
         GTSIZE                                                         05990000
         CH    R0,=H'24'           24 ROWS (LINES)                      06000000
         BL    ERRTERM                                              ABL 06010000
***      CH    R1,=H'80'           80 COLUMNS                           06020000
***      BNE   ERRTERM                                                  06030000
         SPACE                                                          06040000
************************************************************            06050000
*                                                          *            06060000
*        SET UP PFK DEFAULTS                               *            06070000
*                                                          *            06080000
************************************************************            06090000
         SPACE                                                          06100000
         LA    R1,PFKDEF                                                06110000
         LA    R15,PFKTAB-4095                                          06120000
         LA    R15,4095(,R15)                                           06130000
         LA    R0,12                                                    06140000
PFKINIT  MVI   0(R15),C' '                                              06150000
         MVC   1(PFKTABL-1,R15),0(R15) PAD WITH BLANKS                  06160000
         MVC   0(PFKDEFL,R15),0(R1) INSERT DEFAULT                      06170000
         LA    R1,PFKDEFL(,R1)     POINT TO NEXT DEFAULT                06180000
         LA    R15,PFKTABL(,R15)   POINT TO NEXT PFK ENTRY              06190000
         BCT   R0,PFKINIT          11 MORE TIMES                        06200000
         MVI   0(R15),0            END OF TABLE                         06210000
         SPACE                                                          06220000
************************************************************            06230000
*                                                          *            06240000
*        SET UP PPL FOR PARSE                              *            06250000
*                                                          *            06260000
************************************************************            06270000
         SPACE                                                          06280000
         LA    R15,MYPPL                                                06290000
         USING PPL,R15                                                  06300000
         MVC   PPLUPT(4),CPPLUPT                                        06310000
         MVC   PPLECT(4),CPPLECT                                        06320000
         LA    R0,MYECB                                                 06330000
         ST    R0,PPLECB                                                06340000
         XC    MYECB,MYECB                                              06350000
         L     R0,=A(REVPCL)                                            06360000
         ST    R0,PPLPCL                                                06370000
         LA    R0,MYANS                                                 06380000
         ST    R0,PPLANS                                                06390000
         MVC   PPLCBUF(4),CPPLCBUF                                      06400000
         ST    R9,PPLUWA                                                06410000
         DROP  R15                 PPL                                  06420000
         SPACE 1                                                        06430000
************************************************************            06440000
*                                                          *            06450000
*        CALL THE PARSE SERVICE ROUTINE                    *            06460000
*                                                          *            06470000
************************************************************            06480000
         SPACE 1                                                        06490000
         LR    R1,R15              POINT TO PPL                         06500000
         AIF   (NOT &MVS).SKIP2                                         06510000
         L     R15,16              CVTPTR                               06520000
         TM    524(R15),X'80'      IF HI ORDER BIT NOT ON               06530000
         BNO   PARSELNK               THEN DO LINK, NOT CALL            06540000
         L     R15,524(,R15)       CVTPARS                              06550000
         BALR  R14,R15             CALL IKJPARS                         06560000
         B     PARSEEXT            SKIP AROUND LINK                     06570000
PARSELNK EQU   *                                                        06580000
.SKIP2   ANOP                                                           06590000
         LINK  EP=IKJPARS,SF=(E,LINKAREA)                               06600000
PARSEEXT EQU   *                                                        06610000
         SPACE 1                                                        06620000
         LTR   R15,R15                                                  06630000
         BZ    PARSEOK                                                  06640000
         LA    R1,MSG01                                                 06650000
         LA    R0,L'MSG01                                               06660000
         BAL   R14,PUTMSG1                                              06670000
         LA    R15,12                                                   06680000
         B     EXIT                                                     06690000
PARSEOK  EQU   *                                                        06700000
         SPACE                                                          06710000
         L     R3,MYANS                                                 06720000
         USING IKJPARMD,R3                                              06730000
         SPACE                                                          06740000
************************************************************            06750000
*                                                          *            06760000
*         FILL IN ALL INFO FROM PARSE                      *            06770000
*                                                          *            06780000
************************************************************            06790000
         SPACE                                                          06800000
         MVI   DEBUGSW,0                                            ABL 06810000
         MVI   HELPFLG,0                                            ABL 06820000
         XC    TGETREGS(12),TGETREGS                                ABL 06830000
         MVC   FILEKV,FILEKW                                            06840000
         MVC   QUICKV,QUICKW                                            06850000
         LA    R6,DSN                                                   06860000
         TM    6(R6),X'80'         IS DATASET NAME SPECIFIED?           06870000
         BO    OKDSN               YES - BRANCH                         06880000
         LA    R1,MSG05            NO - JUST MEMBER NAME                06890000
         LA    R0,L'MSG05                                               06900000
         BAL   R14,PUTMSG1                                              06910000
         B     EXITA12                                                  06920000
         SPACE                                                          06930000
OKDSN    EQU   *                                                        06940000
         MVC   UDSNAME(4),4(R6)    COPY LENGTH AND QUOTE FLAGS          06950000
         LA    R15,UDSNAME+4                                            06960000
         MVI   0(R15),C' '         BLANK THE DSNAME AREA                06970000
         MVC   1(43,R15),0(R15)                                         06980000
         L     R14,0(,R6)          POINT TO DSN VALUE                   06990000
         LH    R1,4(,R6)           GET LENGTH                           07000000
         STH   R1,UDSNAME                                               07010000
         BCTR  R1,0                LENGTH MINUS 1 FOR EX                07020000
         B     *+10                BRANCH AROUND EXECUTED MVC           07030000
         MVC   0(0,R15),0(R14)     (EXECUTED)                           07040000
         EX    R1,*-6              MOVE DSN TO DSNAME (AFTER PREFIX)    07050000
         SPACE                                                          07060000
         MVC   $MEMBER,=CL8' '                                          07070000
         TM    14(R6),X'80'        MEMBER SPECIFIED?                    07080000
         BZ    PARSEMX             NO - BRANCH                          07090000
         LH    R1,12(,R6)          GET LENGTH OF MEMBER                 07100000
         BCTR  R1,0                MINUS 1 FOR EX                       07110000
         L     R14,8(,R6)          GET ADDRESS OF MEMBER NAME           07120000
         B     *+10                                                     07130000
         MVC   $MEMBER(0),0(R14)   MOVE MEMBER NAME                     07140000
         EX    R1,*-6                                                   07150000
PARSEMX  EQU   *                                                        07160000
         SPACE                                                          07170000
         MVC   $PASSWRD,=CL8' '                                         07180000
         TM    22(R6),X'80'        PASSWORD SPECIFIED?                  07190000
         BZ    PARSEPX             NO - BRANCH                          07200000
         LH    R1,20(,R6)          GET LENGTH OF PSWD                   07210000
         BCTR  R1,0                MINUS 1 FOR EX                       07220000
         L     R14,16(,R6)         GET ADDRESS OF PSWD                  07230000
         B     *+10                                                     07240000
         MVC   $PASSWRD(0),0(R14)  MOVE PSWD                            07250000
         EX    R1,*-6                                                   07260000
PARSEPX  EQU   *                                                        07270000
         MVC   $VOLSER,=CL8' '     (6 BYTES)                            07280000
         MVC   $UNIT,=CL8' '                                            07290000
         LA    R6,VOL                                                   07300000
         TM    6(R6),X'80'         VOLUME SPECIFIED                     07310000
         BZ    PARSEVX             NO, LEAVE VOL AND UNIT BLANK         07320000
         L     R14,0(,R6)          POINT TO VOL VALUE                   07330000
         LH    R1,4(,R6)           GET LENGTH                           07340000
         BCTR  R1,0                LENGTH MINUS 1 FOR EX                07350000
         B     *+10                BRANCH AROUND EXECUTED MVC           07360000
         MVC   $VOLSER(0),0(R14)   (EXECUTED)                           07370000
         EX    R1,*-6              MOVE VOLUME                          07380000
         LA    R6,UNIT                                                  07390000
         TM    6(R6),X'80'         UNIT SPECIFIED                       07400000
         BZ    PARSEUX             NO, BRANCH                           07410000
         L     R14,0(,R6)          POINT TO UNITNAME VALUE              07420000
         LH    R1,4(,R6)           GET LENGTH                           07430000
         BCTR  R1,0                LENGTH MINUS 1 FOR EX                07440000
         B     *+10                BRANCH AROUND EXECUTED MVC           07450000
         MVC   $UNIT(0),0(R14)     (EXECUTED)                           07460000
         EX    R1,*-6              MOVE UNIT NAME                       07470000
PARSEUX  EQU   *                                                        07480000
PARSEVX  EQU   *                                                        07490000
         DROP  R3                  IKJPARMD                             07500000
         IKJRLSA MYANS                                                  07510000
         XC    MYANS,MYANS                                              07520000
         SPACE                                                          07530000
************************************************************            07540000
*                                                          *            07550000
*        QUALIFY THE DSNAME IF NECESSARY                   *            07560000
*                                                          *            07570000
************************************************************            07580000
         SPACE                                                          07590000
         LA    R15,$DSNAME+2                                            07600000
         MVI   0(R15),C' '         BLANK THE DSNAME AREA                07610000
         MVC   1(43,R15),0(R15)                                         07620000
         SLR   R1,R1                                                    07630000
         STH   R1,$DSNAME          ZERO DSNAME LENGTH                   07640000
         TM    UDSNAME+2,X'40'     IS DSN QUOTED?                       07650000
         BO    NOPREF              YES, SKIP PREFIXING                  07660000
         CLI   FILEKV+1,1          DSN TO BE TREATED AS DDNAME          07670000
         BE    NOPREF              YES, SKIP PREFIXING                  07680000
         AIF   (NOT &MVS).SKIPP    PREFIX WITH PREFIX                   07690000
         L     R14,CPPLUPT         POINT TO UPT                         07700000
         USING UPT,R14                                                  07710000
         IC    R1,UPTPREFL         GET LENGTH OF PREFIX                 07720000
         LTR   R1,R1               IS IT ZERO                           07730000
         BZ    NOPREF              YES, SKIP PREFIXING                  07740000
         B     *+10                                                     07750000
         MVC   0(0,R15),UPTPREFX                                        07760000
         DROP  R14                 UPT                                  07770000
.SKIPP   AIF   (&MVS).SKIPU        PREFIX WITH USERID                   07780000
         L     R14,CPPLPSCB        POINT TO PSCB                        07790000
         USING PSCB,R14                                                 07800000
         IC    R1,PSCBUSRL         GET LENGTH OF USERID                 07810000
         LTR   R1,R1               IS IT ZERO                           07820000
         BZ    NOPREF              YES, SKIP PREFIXING                  07830000
         B     *+10                                                     07840000
         MVC   0(0,R15),PSCBUSER                                        07850000
         DROP  R14                 PSCB                                 07860000
.SKIPU   ANOP                                                           07870000
         EX    R1,*-6              MOVE USERID TO DSNAME AREA           07880000
         LA    R15,0(R1,R15)       POINT PAST USERID                    07890000
         MVI   0(R15),C'.'         APPEND PERIOD                        07900000
         LA    R15,1(,R15)         POINT PAST PERIOD                    07910000
         LA    R1,1(,R1)           ADD 1 TO LENGTH                      07920000
         STH   R1,$DSNAME          STORE LENGTH OF USERID PLUS 1        07930000
NOPREF   EQU   *                                                        07940000
         LH    R1,UDSNAME          GET LENGTH                           07950000
         LR    R0,R1                                                    07960000
         AH    R0,$DSNAME          ADD LENGTH OF PREFIX OR ZERO         07970000
         STH   R0,$DSNAME          SET COMBINED LENGTH                  07980000
         LA    R14,UDSNAME+4       POINT TO DSN VALUE                   07990000
         BCTR  R1,0                LENGTH MINUS 1 FOR EX                08000000
         B     *+10                BRANCH AROUND EXECUTED MVC           08010000
         MVC   0(0,R15),0(R14)     (EXECUTED)                           08020000
         EX    R1,*-6              MOVE DSN TO DSNAME (AFTER PREFIX)    08030000
         SPACE                                                          08040000
************************************************************            08050000
*                                                          *            08060000
*        IF 'FILE' KEYWORD IS SPECIFIED,                   *            08070000
*        GET DSNAME FROM JFCB USING FILE NAME.             *            08080000
*                                                          *            08090000
************************************************************            08100000
         SPACE                                                          08110000
         CLI   FILEKV+1,1          'FILE' SPECIFIED?                    08120000
         BNE   NOFILE              NO, BRANCH                           08130000
         CLI   $DSNAME+1,8         IS LENGTH 8 OR LESS                  08140000
         BH    FILERR1             NO, BRANCH                           08150000
         DEVTYPE $DSNAME+2,DEVDATA GET DEVICE TYPE                      08160000
         LTR   R15,R15             WAS FILENAME VALID                   08170000
         BNZ   FILERR2             NO, BRANCH                           08180000
         TM    DEVDATA+2,X'20'     DIRECT ACCESS                        08190000
         BZ    FILERR3             NO, BRANCH                           08200000
         LA    R4,DYNDCBW                                               08210000
         L     R1,=A(SEQDCB)                                            08220000
         MVC   0(SEQDCBL,R4),0(R1)                                      08230000
         LA    R0,JFCB                                                  08240000
         LA    R1,DYNEXLST                                              08250000
         ST    R0,0(,R1)                                                08260000
         MVI   0(R1),X'87'                                              08270000
         ST    R1,36(,R4)          DCBEXLST                             08280000
         MVC   40(8,R4),$DSNAME+2  DCBDDNAM                             08290000
         MVC   $DDNAML(46),$DSNAME                                      08300000
         LA    R0,4                ADD 4                                08310000
         AH    R0,$DDNAML           TO LENGTH OF DDNAME                 08320000
         STH   R0,$DDNAML            FOR +NNN (CONCAT NUMBER)           08330000
         MVI   OPEND,X'80'                                              08340000
         RDJFCB ((R4)),MF=(E,OPEND)                                     08350000
         MVC   $DSNAME+2(44),JFCB                                       08360000
         LA    R1,$DSNAME+45       LAST CHAR OF DSNAME                  08370000
         LA    R0,44               INITIAL LENGTH                       08380000
FILEA    CLI   0(R1),C' '          IS THIS LAST NONBLANK                08390000
         BNE   FILEB               YES, BRANCH                          08400000
         BCTR  R1,0                BACK UP 1 CHARACTER                  08410000
         BCT   R0,FILEA            DECREMENT LENGTH AND BRANCH          08420000
FILEB    STH   R0,$DSNAME          STORE LENGTH OF DSNAME               08430000
         MVI   DSORG,X'40'         DSORG PS                             08440000
         CLI   $MEMBER,C' '        DDNAME(MEMBER)                       08450000
         BE    FILEC               NO                                   08460000
         MVI   DSORG,X'02'         YES, INDICATE PARTITIONED            08470000
         B     FILED               DONT LOOK AT JFCB                    08480000
FILEC    CLI   JFCB+44,X'40'       DDNAME ALLOCATED TO A MEMBER         08490000
         BE    *+14                NO                                   08500000
         MVC   $MEMBER,JFCB+44     YES, SAVE THE MEMBER NAME            08510000
         MVI   DSORG,X'02'         DSORG PO                             08520000
FILED    EQU   *                                                        08530000
*        MVC   $VOLSER(6),JFCB+118 GET VOLUME FROM JFCB                 08540000
         B     FILESPEC                                                 08550000
FILERR1  LA    R0,MSG14A                                                08560000
         B     FILERR                                                   08570000
FILERR2  LA    R0,MSG14B                                                08580000
         B     FILERR                                                   08590000
FILERR3  LA    R0,MSG14C                                                08600000
FILERR   MVC   MSG(L'MSG14),MSG14                                       08610000
         LA    R15,MSG+L'MSG14                                          08620000
         LA    R14,$DSNAME                                              08630000
         LH    R1,0(,R14)                                               08640000
         BCTR  R1,0                                                     08650000
         B     *+10                                                     08660000
         MVC   MSG+L'MSG14(0),2(R14)                                    08670000
         EX    R1,*-6                                                   08680000
         LA    R15,1(R1,R15)                                            08690000
         LR    R14,R0 POINT TO MSG14A, B, OR C                          08700000
         MVC   0(L'MSG14A,R15),0(R14)                                   08710000
         LA    R0,L'MSG14+L'MSG14A+1(,R1)                               08720000
         LA    R1,MSG                                                   08730000
         BAL   R14,PUTMSG1                                              08740000
         B     EXITA12                                                  08750000
NOFILE   EQU   *                                                        08760000
         TM    UDSNAME+2,X'40'     IS DSNAME QUOTED?                    08770000
         BO    DEFX                YES - SKIP DEFAULT SERVICE           08780000
         CLI   $VOLSER,C' '        IS VOLUME SPECIFIED?                 08790000
         BNE   DEFX                YES - SKIP DEFAULT SERVICE           08800000
         CLI   QUICKV+1,1          QUICK SPECIFIED                      08810000
         BE    DEFX                YES, USER ENTERED ALL BUT PREFIX     08820000
         SPACE                                                          08830000
         LA    R15,MYIOPL                                               08840000
         USING IOPL,R15                                                 08850000
         LA    R14,MYDFPB                                               08860000
         ST    R14,IOPLIOPB                                             08870000
         USING DFPB,R14                                                 08880000
         XC    0(20,R14),0(R14)                                         08890000
         LA    R0,$DSNAME                                               08900000
         ST    R0,DFPBDSN                                               08910000
         OI    DFPBCODE,X'04'      SEARCH CAT AND PROMPT IF MULTI       08920000
         MVC   DFPBPSCB,CPPLPSCB                                        08930000
*        MVI   DFPBCNTL,X'20'      PREFIX THE DSNAME                    08940000
         DROP  R14                 DFPB                                 08950000
         SPACE                                                          08960000
         LA    R1,MYIOPL                                                08970000
         SPACE                                                          08980000
         LR    R1,R15              POINT TO IOPL                        08990000
         AIF   (NOT &MVS).SKIP4                                         09000000
         L     R15,16              CVTPTR                               09010000
         TM    736(R15),X'80'      IF HI ORDER BIT NOT ON               09020000
         BNO   EHDEFLNK               THEN DO LINK, NOT CALL            09030000
         L     R15,736(,R15)       CVTEHDEF                             09040000
         BALR  R14,R15             CALL IKJEHDEF                        09050000
         B     EHDEFEXT            SKIP AROUND LINK                     09060000
EHDEFLNK EQU   *                                                        09070000
.SKIP4   ANOP                                                           09080000
         LINK  EP=IKJEHDEF,SF=(E,LINKAREA)                              09090000
EHDEFEXT EQU   *                                                        09100000
         SPACE                                                          09110000
         B     DEFCODE(R15)                                             09120000
DEFCODE  B     DEF00               SUCCESS                              09130000
         B     EXITA12              MSG ALREADY ISSUED                  09140000
         B     DEF08               INVALID NAME GT 44                   09150000
         B     EXITA12              MSG ALREADY ISUED                   09160000
         B     DEF16               NOT IN CATALOG                       09170000
         B     DEF20               NOT IN CATALOG                       09180000
         B     DEF24               IMPOSSIBLE                           09190000
         B     DEF28               COMMAND SYSTEM ERROR                 09200000
         B     DEF32               IMPOSSIBLE                           09210000
         B     DEF36               ?                                    09220000
DEF08    EQU   *                                                        09230000
DEF16    EQU   *                                                        09240000
         B     DEF24                                                    09250000
DEF20    EQU   *                                                        09260000
LOCERR   EQU   *                                                        09270000
         MVC   MSG(L'MSG02),MSG02                                       09280000
         LA    R15,MSG+L'MSG02                                          09290000
         LA    R14,$DSNAME                                              09300000
         LH    R1,0(,R14)                                               09310000
         BCTR  R1,0                                                     09320000
         B     *+10                                                     09330000
         MVC   MSG+L'MSG02(0),2(R14)                                    09340000
         EX    R1,*-6                                                   09350000
         LA    R15,1(R1,R15)                                            09360000
         MVC   0(L'MSG02A,R15),MSG02A                                   09370000
         LA    R0,L'MSG02+L'MSG02A+1(,R1)                               09380000
         LA    R1,MSG                                                   09390000
         BAL   R14,PUTMSG1                                              09400000
         B     EXITA12                                                  09410000
DEF24    EQU   *                                                        09420000
DEF28    EQU   *                                                        09430000
DEF32    EQU   *                                                        09440000
DEF36    EQU   *                                                        09450000
         LA    R1,MSG03                                                 09460000
         LA    R0,L'MSG03                                               09470000
         BAL   R14,PUTMSG1                                              09480000
         B     EXITA12                                                  09490000
         SPACE                                                          09500000
DEF00    EQU   *                                                        09510000
DEFX     EQU   *                                                        09520000
         SPACE                                                          09530000
************************************************************            09540000
*                                                          *            09550000
*        ALLOCATE THE DATASET                              *            09560000
*                                                          *            09570000
************************************************************            09580000
         SPACE                                                          09590000
         LA    R1,MYDAPL                                                09600000
         USING DAPL,R1                                                  09610000
         MVC   DAPLUPT(4),CPPLUPT                                       09620000
         MVC   DAPLECT(4),CPPLECT                                       09630000
         LA    R0,MYECB                                                 09640000
         ST    R0,DAPLECB                                               09650000
         MVC   DAPLPSCB(4),CPPLPSCB                                     09660000
         LA    R15,MYDAPB                                               09670000
         ST    R15,DAPLDAPB                                             09680000
         DROP  R1                  DAPL                                 09690000
         USING DAPB08,R15                                               09700000
         XC    0(84,R15),0(R15)                                         09710000
         MVI   DA08CD+1,X'08'                                           09720000
         LA    R0,$DSNAME                                               09730000
         ST    R0,DA08PDSN                                              09740000
         MVC   DA08DDN(8),=CL8' '                                       09750000
         MVC   DA08UNIT,$UNIT                                           09760000
         MVC   DA08SER,=CL8' '                                          09770000
         MVC   DA08SER(6),$VOLSER                                       09780000
         MVC   DA08MNM,=CL8' '     NEVER ALLOCATE WITH MEMBER           09790000
         MVC   DA08PSWD,$PASSWRD                                        09800000
         MVI   DA08DSP1,DA08SHR                                         09810000
         MVI   DA08DPS2,DA08KEEP                                        09820000
         MVI   DA08DPS3,DA08KEP                                         09830000
         LA    R1,MYDAPL                                                09840000
         SPACE                                                          09850000
         BAL   R14,CALLDAIR                                             09860000
         LTR   R15,R15                                                  09870000
         BZ    OKDAIR                                                   09880000
         BAL   R14,DAIRFAIL                                             09890000
         B     EXITA12                                                  09900000
OKDAIR   EQU   *                                                        09910000
         OI    STATUS,STALLOC      TELL CLEANUP TO FREE IT              09920000
         LA    R15,MYDAPB                                               09930000
         MVC   $DDNAME,DA08DDN                                          09940000
         MVC   DSORG,DA08DSO                                            09950000
         TM    DA08DSO,X'40'       IS DSORG SEQUENTIAL?                 09960000
         BO    OKDSORGS            YES - BRANCH                         09970000
         TM    DA08DSO,X'02'       IS DSORG PARTITIONED?                09980000
         BO    OKDSORGP            YES, BRANCH                          09990000
*                                                                       10000000
*              DSORG IS NEITHER PS NOR PO                               10010000
*              ISAM=X'80' DA=X'20' VSAM=X'00' NONE=X'00'                10020000
*                                                                       10030000
ERRDSORG LA    R1,MSG06                                                 10040000
         LA    R0,L'MSG06                                               10050000
         BAL   R14,PUTMSG1                                              10060000
         B     EXITA12                                                  10070000
OKDSORGP EQU   *                                                        10080000
         CLI   $MEMBER,C' '        WAS MEMBER SPECIFIED?                10090000
         BNE   OKDSORG             YES - BRANCH                         10100000
         LA    R1,MSG07            DSORG PO BUT MEMBER NOT SPEC         10110000
         LA    R0,L'MSG07                                               10120000
         BAL   R14,PUTMSG1                                              10130000
         B     EXITA12                                                  10140000
OKDSORGS MVC   $MEMBER,=CL8' '                                          10150000
OKDSORG  EQU   *                                                        10160000
         DROP  R15                 DAPB08                               10170000
FILESPEC EQU   *                                                        10180000
         SPACE                                                          10190000
************************************************************            10200000
*                                                          *            10210000
*         GET THE UCB ADDRESS AND VOLUME SERIAL            *            10220000
*                                                          *            10230000
************************************************************            10240000
         SPACE                                                          10250000
         L     R1,16               CVTPTR                               10260000
         L     R1,0(,R1)           TCB WORDS                            10270000
         L     R1,4(,R1)           CURRENT TCB                          10280000
         L     R1,12(,R1)          TIOT                                 10290000
         LA    R1,24(,R1)          TIOENTRY                             10300000
DDLOOP   CLI   0(R1),0             END OF TIOT                          10310000
         BE    EXITA12             YES, BRANCH (NEVER HAPPENS)          10320000
         CLC   4(8,R1),$DDNAME     DOES DDNAME MATCH                    10330000
         BE    DDFOUND                                                  10340000
         SLR   R15,R15                                                  10350000
         IC    R15,0(,R1)                                               10360000
         LA    R1,0(R15,R1)                                             10370000
         B     DDLOOP                                                   10380000
DDFOUND  L     R15,16(,R1)         TIOEFSRT-1                           10390000
         ST    R15,$UCBAD                                               10400000
         TM    18(R15),X'20'       DIRECT ACCESS DEVICE?                10410000
         BZ    *+10                NO, BYPASS VOLSER                    10420000
         MVC   $VOLSER,28(R15)     UCBVOLI                              10430000
         SPACE                                                          10440000
************************************************************            10450000
*                                                          *            10460000
*         OBTAIN THE DSCB FROM THE VTOC                    *            10470000
*                                                          *            10480000
************************************************************            10490000
         SPACE                                                          10500000
         CLI   FILEKV+1,1          'FILE' SPECIFIED?                    10510000
         BNE   OBTX                NO, BYPASS OBTAIN                    10520000
         L     R15,$UCBAD          POINT TO UCB                         10530000
         TM    18(R15),X'20'       DIRECT ACCESS DEVICE?                10540000
         BZ    OBTX                NO, BYPASS OBTAIN                    10550000
OBTDSCB  LA    R1,OBTAINW                                               10560000
         MVC   0(OBTAINL,R1),OBTAIN                                     10570000
         LA    R0,$DSNAME+2        DSN FOR OBTAIN                       10580000
         ST    R0,4(,R1)                                                10590000
         LA    R0,$VOLSER          VOLUME FOR OBTAIN                    10600000
         ST    R0,8(,R1)                                                10610000
         LA    R0,MYDSCB           ANSWER AREA FOR OBTAIN               10620000
         ST    R0,12(,R1)                                               10630000
         OBTAIN (1)                                                     10640000
         LTR   R15,R15             WAS OBTAIN SUCCESSFUL                10650000
         BZ    OKDSCB              YES, BRANCH                          10660000
         SPACE                                                          10670000
*               OBTAIN HAS FAILED. HOW CAN THAT HAPPEN WHEN             10680000
*               DYNAMIC ALLOCATION WAS SUCCESSFUL? ONE WAY IT           10690000
*               CAN HAPPEN IS IF THE DSNAME IS AN ALIAS ENTRY           10700000
*               IN A VSAM CATALOG.  IF IT IS, A 'LOCATE' WILL           10710000
*               PUT THE TRUE NAME IN THE DSNAME FIELD, SO NOW           10720000
*               WE ISSUE A LOCATE, AND TRY THE OBTAIN AGAIN.            10730000
         SPACE                                                          10740000
         TM    STATUS,STLOCAT      HAS LOCATE BEEN TRIED ALREADY?       10750000
         BZ    OBTALIAS            NO, GO TRY IT                        10760000
OBTERR   LA    R1,MSG09            UNABLE TO OBTAIN DSCB                10770000
         LA    R0,L'MSG09                                               10780000
         BAL   R14,PUTMSG1                                              10790000
         B     EXITA12                                                  10800000
OBTALIAS OI    STATUS,STLOCAT      TRIP THE SWITCH                      10810000
         LA    R1,LOCATEW                                               10820000
         MVC   0(LOCATEL,R1),LOCATE                                     10830000
         LA    R0,$DSNAME+2        DSNAME FOR LOCATE                    10840000
         ST    R0,4(,R1)                                                10850000
         LA    R0,LOCBUF           ANSWER AREA FOR LOCATE               10860000
         ST    R0,12(,R1)                                               10870000
         LOCATE (1)                                                     10880000
         LTR   15,15               WAS LOCATE SUCCESSFUL?               10890000
         BZ    OBTDSCB             YES, GO OBTAIN AGAIN                 10900000
         B     OBTERR              NO, ISSUE MESSAGE                    10910000
         SPACE                                                          10920000
OKDSCB   NI    STATUS,255-STLOCAT  TURN OFF LOCATE SWITCH               10930000
*        CLC   MYDSCB-44+X'62'(3),=X'000000' CHECK DS1LSTAR             10940000
*        BE    EMPTY               BRANCH IF EMPTY DATA SET             10950000
         MVC   DSORG,MYDSCB-44+X'52'                                    10960000
         TM    DSORG,X'40'         DSORG = PS                           10970000
         BO    OBTX                                                     10980000
         TM    DSORG,X'02'         DSORG = PO                           10990000
         BNO   ERRDSORG                                                 11000000
         CLI   $MEMBER,C' '        WAS MEMBER SPECIFIED?                11010000
         BNE   OBTX                YES - BRANCH                         11020000
         LA    R1,MSG07            DSORG PO BUT MEMBER NOT SPEC         11030000
         LA    R0,L'MSG07                                               11040000
         BAL   R14,PUTMSG1                                              11050000
         B     EXITA12                                                  11060000
OBTX     EQU   *                                                        11070000
         SPACE                                                          11080000
************************************************************            11090000
*                                                          *            11100000
*         CALL PHASE 2 FOR MAIN PROCESSING                 *            11110000
*                                                          *            11120000
************************************************************            11130000
         SPACE                                                          11140000
         L     R15,=A(REVIEW2)     GET ADDRESS OF PHASE 2               11150000
         BALR  R14,R15             GO TO PHASE 2                        11160000
         B     EXITARC             PHASE 2 HAS SET RC                   11170000
         SPACE                                                          11180000
************************************************************            11190000
*                                                          *            11200000
*         RETURN FROM PHASE 2 TO TERMINATE                 *            11210000
*                                                          *            11220000
************************************************************            11230000
         SPACE                                                          11240000
EXITA12  LA    R15,12                                                   11250000
         STH   R15,RC                                                   11260000
EXITARC  TM    STATUS,STALLOC      FREE REQUIRED?                       11270000
         BZ    NOFREE                                                   11280000
         LA    R1,MYDAPL                                                11290000
         LA    R15,MYDAPB                                               11300000
         USING DAPB18,R15                                               11310000
         XC    0(40,R15),0(R15)                                         11320000
         MVI   DA18CD+1,X'18'                                           11330000
         MVC   DA18DDN,$DDNAME                                          11340000
         MVC   DA18MNM(8),=CL8' '                                       11350000
         MVC   DA18SCLS(2),=CL8' '                                      11360000
         BAL   R14,CALLDAIR        UNALLOCATE                           11370000
         NI    STATUS,255-STALLOC  UNALLOCATED                          11380000
         DROP  R15                 DAPB18                               11390000
NOFREE   EQU   *                                                        11400000
         SPACE                                                          11410000
         IKJRLSA MYANS                                                  11420000
         SPACE                                                          11430000
         CLI   RC+1,0              IS RC ZERO?                          11440000
         BZ    STACKDX             YES, BRANCH                          11450000
         MVC   MYSTPB(STACKDL),STACKD                                   11460000
         SPACE                                                          11470000
         STACK DELETE=ALL,PARM=MYSTPB,MF=(E,MYIOPL)                     11480000
         SPACE                                                          11490000
         TCLEARQ                                                        11500000
STACKDX  EQU   *                                                        11510000
         SPACE                                                          11520000
         LH    R15,RC                                                   11530000
         B     EXIT                                                     11540000
         SPACE                                                          11550000
************************************************************            11560000
*                                                          *            11570000
*         EXIT TO CALLER                                   *            11580000
*                                                          *            11590000
************************************************************            11600000
         SPACE                                                          11610000
ERRRECFM LA    R1,MSGRECFM                                              11620000
         LA    R0,L'MSGRECFM                                            11630000
         BAL   R14,PUTMSG1                                              11640000
         B     EXITA12                                                  11650000
         SPACE                                                          11660000
ERRTERM  LA    R1,MSGTERM                                               11670000
         LA    R0,L'MSGTERM                                             11680000
         BAL   R14,PUTMSG1                                              11690000
         B     EXITA12                                                  11700000
         SPACE                                                          11710000
EXIT     LR    R1,R13                                                   11720000
         L     R0,SIZE                                                  11730000
         L     R13,4(,R13)                                              11740000
         LR    R2,R15                                                   11750000
         FREEMAIN R,A=(1),LV=(0)                                        11760000
         LR    R15,R2                                                   11770000
         LM    0,12,20(R13)                                             11780000
         L     R14,12(,R13)                                             11790000
         BR    R14                                                      11800000
         SPACE                                                          11810000
************************************************************            11820000
*                                                          *            11830000
*         CALL IKJDAIR SERVICE ROUTINE                     *            11840000
*                                                          *            11850000
************************************************************            11860000
          SPACE                                                         11870000
CALLDAIR ST    R14,DAIRREGS                                             11880000
         AIF   (NOT &MVS).SKIP6                                         11890000
         L     R15,16                                                   11900000
         TM    732(R15),X'80'     CVTDAIR                               11910000
         BNO   DAIRLINK                                                 11920000
         L     R15,732(,R15)                                            11930000
         BALR  R14,R15                                                  11940000
         B     DAIRFINI                                                 11950000
DAIRLINK EQU   *                                                        11960000
.SKIP6   ANOP                                                           11970000
         LINK  EP=IKJDAIR,SF=(E,LINKAREA)                               11980000
DAIRFINI L     R14,DAIRREGS                                             11990000
         BR    R14                                                      12000000
         SPACE                                                          12010000
************************************************************            12020000
*                                                          *            12030000
*        DYNAMIC ALLOCATION FAILURE ROUTINE                *            12040000
*                                                          *            12050000
************************************************************            12060000
         SPACE                                                          12070000
DAIRFAIL ST    R14,MYDFREGS                                             12080000
         LA    R1,MYDFPARM                                              12090000
*        USING DFDSECTD,R1         MAPPED BY IKJEFFDF DFDSECT=YES MACRO 12100000
         ST    R15,MYDFRC                                               12110000
         LA    R15,MYDFRC                                               12120000
         ST    R15,4(,R1)          DFRCP                                12130000
         LA    R15,MYDAPL                                               12140000
         ST    R15,0(,R1)          DFDAPLP                              12150000
         SLR   R15,R15                                                  12160000
         ST    R15,MYJEFF02                                             12170000
         LA    R15,MYJEFF02                                             12180000
         ST    R15,8(,R1)          DFJEFF02                             12190000
         LA    R15,1               DFDAIR                               12200000
         STH   R15,MYDFID                                               12210000
         LA    R15,MYDFID                                               12220000
         ST    R15,12(,R1)         DFIDP                                12230000
         MVC   16(4,R1),CPPLPTR    DFCPPLP                              12240000
         LINK  EP=IKJEFF18,SF=(E,LINKAREA)                              12250000
*        DROP  R1                  DFDSECTD                             12260000
         L     R15,MYDFRC                                               12270000
         L     R14,MYDFREGS                                             12280000
         BR    R14                                                      12290000
         SPACE                                                          12300000
************************************************************            12310000
*                                                          *            12320000
*         PHASE 1 INTERFACES TO PHASE 2                    *            12330000
*                                                          *            12340000
************************************************************            12350000
         SPACE                                                          12360000
PUTMSG1  ST    R14,BASE1+12                                             12370000
         L     R15,=A(PUTMSG)                                           12380000
         LM    R10,R12,BASE2       SWITCH BASE REGS                     12390000
         BALR  R14,R15                                                  12400000
         LM    R10,R12,BASE1       RESTORE BASE REGS                    12410000
         L     R14,BASE1+12                                             12420000
         BR    R14                                                      12430000
         SPACE                                                          12440000
************************************************************            12450000
*                                                          *            12460000
*         PHASE 1 CONSTANTS                                *            12470000
*                                                          *            12480000
************************************************************            12490000
         SPACE                                                          12500000
         LTORG                                                          12510000
PFKDEF   DC    0D'0'               INITIAL VALUES OF PF KEYS            12520000
         DC    X'F1',CL15'HELP '   1                                ABL 12530000
         DC    X'F2',CL15' '       2                                    12540006
         DC    X'F3',CL15'END '    3                                    12550000
         DC    X'F4',CL15'END '    4                                ABL 12560006
         DC    X'F5',CL15'FIND '   5                                    12570000
         DC    X'F6',CL15' '       6                                    12580000
         DC    X'F7',CL15'UP '     7                                    12590000
         DC    X'F8',CL15'DOWN '   8                                    12600000
         DC    X'F9',CL15'HEX '    9                                    12610000
         DC    X'7A',CL15'LEFT '   10                                   12620000
         DC    X'7B',CL15'RIGHT '  11                                   12630000
         DC    X'7C',CL15' '       12                                   12640000
PFKDEFL  EQU   16                  LENGTH OF EACH DEFAULT ENTRY         12650000
         SPACE                                                          12660000
OBTAIN   CAMLST SEARCH,2,3,4                                            12670000
OBTAINL  EQU   *-OBTAIN                                                 12680000
         SPACE                                                          12690000
LOCATE   CAMLST NAME,2,,4                                               12700000
LOCATEL  EQU   *-LOCATE                                                 12710000
         SPACE                                                          12720000
MSG01    DC    C'ERROR IN PARSE SERVICE ROUTINE'                        12730000
MSG02    DC    C'IKJ58503I DATA SET '                                   12740000
MSG02A   DC    C' NOT IN CATALOG'                                       12750000
MSG03    DC    C'ERROR IN DEFAULT SERVICE ROUTINE'                      12760000
MSG05    DC    C'IKJ58509I DATA SET NAME REQUIRED WHEN MEMBER IS SPECIF+12770000
               IED'                                                     12780000
MSG06    DC    C'ORGANIZATION OF DATA SET MUST BE PARTITIONED OR SEQUEN+12790000
               TIAL'                                                    12800000
MSG07    DC    C'DATA SET IS PARTITIONED, MEMBER NOT SPECIFIED'         12810000
MSG09    DC    C'UNABLE TO OBTAIN DSCB FOR DATA SET'                    12820000
MSG14    DC    C'FILENAME '                                             12830000
MSG14A   DC    C' INVALID, MORE THAN 8 CHARACTERS  '                    12840000
MSG14B   DC    C' IS NOT CURRENTLY ALLOCATED       '                    12850000
MSG14C   DC    C' NOT ALLOCATED TO A DASD DATA SET '                    12860000
MSGRECFM DC    C'RECORD FORMAT U IS NOT SUPPORTED'                      12870000
MSGTERM  DC    C'THIS IS NOT A 3270 TERMINAL WITH 80 CHARACTER LINES'   12880000
STACKD   STACK DELETE=ALL,MF=L                                          12890000
STACKDL  EQU   *-STACKD                                                 12900000
         SPACE                                                          12910000
*                   DC    (((*-C-1)/N)*N+N+C-*)X'00'                    12920000
*                 WHERE N IS BOUNDARY (I.E. 256, 4096, ...)             12930000
*                 AND C IS THE BEGINNING OF THE CSECT.                  12940000
         DC    (((*-CSECT1-1)/256)*256+256+CSECT1-*)X'00'               12950000
         DC    0D'0'                                                    12960000
         EJECT                                                          12970000
************************************************************            12980000
*                                                          *            12990000
*         PHASE 2 INITIALIZATION                           *            13000000
*                                                          *            13010000
************************************************************            13020000
         SPACE                                                          13030000
*                                                                       13040000
*         FROM THIS POINT ON, PROCESSING IS THE SAME                    13050000
*         FOR BOTH DSNAME AND DDNAME OPTIONS.                           13060000
*                                                                       13070000
*         $DSNAME  - CONTAINS THE FULLY QUALIFIED DSNAME,               13080000
*                    FROM THE USER OR FROM THE JFCB.                    13090000
*         $MEMBER  - CONTAINS THE MEMBER NAME, IF SPECIFIED,            13100000
*                    FROM THE USER OR FROM THE JFCB.                    13110000
*         $DDNAME  - CONTAINS THE DDNAME, FROM IKJDAIR OR               13120000
*                    FROM THE USER.                                     13130000
*         $UCBAD   - CONTAINS THE UCB ADDRESS,                          13140000
*                    FROM THE TIOT ENTRY.                               13150000
*         $VOLSER  - CONTAINS THE VOLUME, FROM THE UCB POINTED          13160000
*                    TO BY THE TIOT ENTRY.                              13170000
*                                                                       13180000
*         IF THE USER SPECIFIED A DSNAME (DID NOT SPECIFY THE FILE      13190000
*         KEYWORD) THEN THE STALLOC BIT IS SET ON SO THE DDNAME         13200000
*         WILL BE UNALLOCATED WHEN THE COMMAND IS FINISHED WITH IT.     13210000
*                                                                       13220000
REVIEW2  CSECT                                                          13230000
         USING *,R10,R11,R12                                            13240000
         LR    R10,R15             RESET BASE REGISTER                  13250000
         LA    R15,1                                                    13260000
         LA    R11,4095(R15,R10)   BASE                                 13270000
         LA    R12,4095(R15,R11)   BASE                                 13280000
         ST    R14,RET1                                                 13290000
         GTSIZE                                                         13300000
         CH    R0,=H'27'           3278-5 TERMINAL?                     13310000
         BNE   GTSIZE2             NO, BRANCH                           13320000
         OI    MODE,STSIZEX        SET A FLAG FOR LATER                 13330000
         STSIZE SIZE=80,LINE=24    SET SIZE TO 24X80                    13340000
         LA    R1,=X'27F5C11140403C4040001DC813'  WRITE                 13350000
         LA    R0,13                              LENGTH                13360000
         TPUT  (1),(0),FULLSCR                    WRITE FULL SCREEN     13370000
         GTSIZE                                                         13380000
GTSIZE2  DS    0H                  AT LEAST 24 ROWS (LINES)             13390000
         SH    R0,=H'4'            LESS HEADER INFORMATION              13400000
         STH   R0,PAGESIZE         SAVE THE PAGE SIZE                   13410000
         ST    R1,SCROLL           SET NEW RANGE                        13420000
         STH   R1,PAGEWIDT         SAVE THE LINE WIDTH                  13430000
         SPACE                                                          13440000
************************************************************            13450000
*                                                          *            13460000
*        SET UP THE DCB                                    *            13470000
*                                                          *            13480000
************************************************************            13490000
         SPACE                                                          13500000
CLEAR    STFSMODE ON,INITIAL=YES   TURN ON FULLSCREEN MODE              13510000
         MVC   DYNDCBW(SEQDCBL),SEQDCB                                  13520000
         TM    DSORG,X'02'                                              13530000
         BZ    *+10                                                     13540000
         MVC   DYNDCBW(PDSDCBL),PDSDCB                                  13550000
         LA    R4,DYNDCBW                                               13560000
         USING IHADCB,R4                                                13570000
         MVC   DCBDDNAM(8),$DDNAME                                      13580000
         SPACE                                                          13590000
         LA    R15,DYNEOD                                               13600000
         IC    R0,DCBEODAD-1                                            13610000
         ST    R15,DCBEODAD-1                                           13620000
         STC   R0,DCBEODAD-1                                            13630000
         SPACE                                                          13640000
         LA    R15,DYNSYNAD                                             13650000
         IC    R0,DCBSYNAD-1                                            13660000
         ST    R15,DCBSYNAD-1                                           13670000
         STC   R0,DCBSYNAD-1                                            13680000
         SPACE                                                          13690000
         LA    R15,DYNEXLST                                             13700000
         IC    R0,DCBEXLSA-1                                            13710000
         ST    R15,DCBEXLSA-1                                           13720000
         STC   R0,DCBEXLSA-1                                            13730000
         LA    R1,DYNDCBEX                                              13740000
         ST    R1,0(,R15)                                               13750000
         MVI   0(R15),X'05'                                             13760000
         LA    R1,DYNABEND                                              13770000
         ST    R1,4(,R15)                                               13780000
         MVI   4(R15),128+X'11'    OPEN ABEND EXIT (MVS)                13790000
         SPACE                                                          13800000
         OI    STATUS,STOPEN       TELL CLEANUP TO CLOSE DCB            13810000
         MVI   OPEND,X'80'                                              13820000
         SPACE                                                          13830000
         OPEN  ((R4),INPUT),MF=(E,OPEND)                                13840000
         SPACE                                                          13850000
         TM    DCBOFLGS,X'10'                                           13860000
         BO    OKOPEN                                                   13870000
         LA    R1,MSG08                                                 13880000
         LA    R0,L'MSG08                                               13890000
         TM    STATUS,STABEND      MEMBER NOT FOUND?                    13900000
         BO    DYNOPENM            YES, BRANCH                          13910000
         LA    R1,MSG04                                                 13920000
         LA    R0,L'MSG04                                               13930000
DYNOPENM BAL   R14,PUTMSG                                               13940000
         B     EXIT12                                                   13950000
         SPACE                                                          13960000
DYNDCBEX EQU   *                                                        13970000
*              TESTING THE RECFM FOR RECFM V OR F                       13980000
*              REQUIRES AT LEAST 2 TESTS BECAUSE BOTH THE               13990000
*              V AND F BITS ARE ON WHEN RECFM IS U.                     14000000
*              THIS ROUTINE MAKES A COPY OF THE RECFM BITS              14010000
*              AND ZEROES BOTH THE V AND F BITS IN THE COPY             14020000
*              IF THE RECFM IS U.  NOW WE CAN DETERMINE                 14030000
*              RECFM V OR F IN ONE TEST INSTEAD OF TWO.                 14040000
         MVC   TSTRECFM,DCBRECFM   COPY RECFM                           14050000
         TM    DCBRECFM,X'C0'      RECFM U                              14060000
         BNOR  R14                 NO, BRANCH                           14070000
         NI    TSTRECFM,255-X'C0'  YES, SET BOTH BITS OFF               14080000
         BR    R14                                                      14090000
         SPACE                                                          14100000
DYNABEND EQU   *                                                        14110000
         L     R8,0(,R1)           GET COMPLETION CODE                  14120000
         N     R8,=A(X'FFF0FF00')                                       14130000
         CL    R8,=A(X'01301800')  IS IT AN 013-18 ABEND                14140000
         BE    *+10                YES, BRANCH                          14150000
         MVI   4(R1),0             ABNORMALLY TERMINATE                 14160000
         BR    R14                                                      14170000
         OI    STATUS,STABEND      INDICATE MEMBER NOT FOUND            14180000
         MVI   3(R1),4             IGNORE THE ABEND                     14190000
         BR    R14                                                      14200000
         SPACE                                                          14210000
OKOPEN   EQU   *                                                        14220000
         MVC   DYNDECBW(DYNDECBL),DYNDECB                               14230000
         SPACE                                                          14240000
************************************************************            14250000
*                                                          *            14260000
*        SET UP THE BUFFER AREA                            *            14270000
*                                                          *            14280000
************************************************************            14290000
         SPACE                                                          14300000
         LH    R0,DCBBLKSI                                              14310000
         AH    R0,=H'7'            ROUND                                14320000
         N     R0,=A(X'FFFFFFF8')   UPWARDS TO MULTIPLE OF 8            14330000
         ST    R0,SAVBLKSI         SAVE FOR PARTITIONING                14340000
         SPACE                                                          14350000
         SLR   R0,R0                                                    14360000
         TM    TSTRECFM,X'48'      VS OR VBS                            14370000
         BNO   NOTSPAN             NO, BRANCH                           14380000
         LH    R0,DCBLRECL         YES, GET LRECL                       14390000
         AH    R0,=H'7'            ROUND                                14400000
         N     R0,=A(X'FFFFFFF8')   UPWARDS TO MULTIPLE OF 8            14410000
         LTR   R0,R0               WAS LRECL ZERO                       14420000
         BNZ   *+8                 NO, SKIP NEXT INSTR                  14430000
         L     R0,SAVBLKSI         YES, USE BLKSIZE ROUNDED             14440000
         A     R0,=A(2048)         ADD 2K IN CASE LRECL IS WRONG        14450000
NOTSPAN  ST    R0,SAVSPANL         SAVE FOR PARTITIONING                14460000
         SPACE                                                          14470000
         LH    R0,DCBLRECL                                              14480000
         LTR   R0,R0                                                    14490000
         BNZ   *+8                                                      14500000
         LH    R0,DCBBLKSI                                              14510000
         STH   R0,RECSIZE                                               14520000
         AH    R0,=H'7'            ROUND                                14530000
         N     R0,=A(X'FFFFFFF8')   UPWARDS TO MULTIPLE OF 8            14540000
         CH    R0,=H'1000'         IS LRECL GREATER THAN MAX            14550000
         BNH   *+8                 NO, SKIP NEXT INSTR                  14560000
         LH    R0,=H'1000'         YES, HELD RECORDS MUST BE TRUNCATED  14570000
         ST    R0,SAVLRECL         SAVE FOR HOLD AREA DIVISION          14580000
         MH    R0,PAGESIZE                                              14590000
         ST    R0,SAVHOLDL         SAVE FOR PARTITIONING                14600000
         SPACE                                                          14610000
         A     R0,SAVSPANL         COMBINE LENGTHS FOR GETMAIN          14620000
         A     R0,SAVBLKSI         COMBINE LENGTHS FOR GETMAIN          14630000
         A     R0,=A(20*200)       PLUS ROOM FOR 200 CHECKPOINTS        14640000
         LA    R7,RANGE                                                 14650000
         ST    R0,0(,R7)                                                14660000
         A     R0,=A(20*824)       OPTIONAL ROOM FOR 1024 CHECKPOINTS   14670000
*        L     R0,=A(16*1024*1024-8) 16 MEG                             14680000
         ST    R0,4(,R7)                                                14690000
         LA    R8,ANSWER                                                14700000
         MVC   GMVUW(GMVUL),GMVU                                        14710000
         GETMAIN VU,LA=(R7),A=(R8),MF=(E,GMVUW)                         14720000
         OI    STATUS,STGMVU                                            14730000
*        L     R0,ANSWER+4         LENGTH                               14740000
*        L     R1,ANSWER           ADDRESS                              14750000
*        FREEMAIN R,LV=(0),A=(1)                                        14760000
         L     R1,ANSWER                                                14770000
         LR    R0,R1                                                    14780000
         A     R0,ANSWER+4                                              14790000
         ST    R0,ENDPTR           END OF GETMAINED AREA                14800000
         ST    R1,BLOCKPTR                                              14810000
         A     R1,SAVBLKSI         POINT PAST BLOCK AREA                14820000
         ST    R1,SPANPTR                                               14830000
         A     R1,SAVSPANL         POINT PAST SPANNED RECORD AREA       14840000
         ST    R1,HOLDPTR                                               14850000
         A     R1,SAVHOLDL         POINT PAST HOLD AREA                 14860000
         ST    R1,CHKPTTOP                                              14870000
         XC    0(16,R1),0(R1)      FIRST ENTRY IN TABLE                 14880000
         XC    DEBLOCKS(12),DEBLOCKS                                    14890000
         XC    OFFSET,OFFSET                                            14900000
         LH    R0,PAGESIZE         LINES PER PAGE                       14910000
         ST    R0,SCROLL                                                14920000
         SLR   R0,R0                                                    14930000
         ST    R0,FNDNUM                                                14940000
         STH   R0,FNDOFF                                                14950000
         ST    R0,CHKPTBOT                                              14960000
         MVC   PERIODS,ASIST                                        ABL 14970000
         SPACE                                                          14980000
************************************************************            14990000
*                                                          *            15000000
*         INITIALIZE HOLD AREA DIRECTORY                   *            15010000
*                                                          *            15020000
************************************************************            15030000
         SPACE                                                          15040000
         LA    R7,HOLDDIR                                               15050000
         SLR   R0,R0                                                    15060000
         L     R15,HOLDPTR                                              15070000
         TM    TSTRECFM,X'40'      RECFM V                              15080000
         BZ    *+8                 NO                                   15090000
         LA    R15,4(,R15)         YES, POINT 4 BYTES INTO EACH RECORD  15100000
         LH    R8,PAGESIZE         LINES PER PAGE                       15110000
HOLDINIT ST    R0,DIRNUM(,R7)      PUT ZERO IN RECORD LENGTH            15120000
         ST    R0,DIRLEN(,R7)      PUT ZERO IN RECORD NUMBER            15130000
         ST    R15,DIRREC(,R7)     STORE ADDRESS OF HELD RECORD         15140000
         A     R15,SAVLRECL        POINT TO NEXT HELD RECORD            15150000
         LA    R1,DIRSIZ(,R7)      POINT TO NEXT ENTRY                  15160000
         ST    R1,DIRNXT(,R7)      STORE ITS ADDRESS IN THIS ENTRY      15170000
         LR    R14,R7              SAVE LAST ENTRY                      15180000
         LR    R7,R1               MAKE NEXT ENTRY THIS ENTRY           15190000
         BCT   R8,HOLDINIT         DO IT FOR ALL BUT LAST ENTRY         15200000
         LA    R1,HOLDDIR          ADDRESS OF FIRST ENTRY               15210000
         ST    R1,DIRNXT(,R14)     CLOSE THE CIRCLE                     15220000
         ST    R1,HOLDTOP          START WITH FIRST AS TOP              15230000
         LA    R1,PFKTAB-4095                                           15240000
         LA    R1,4095(,R1)                                             15250000
         LA    R1,SCREENW-PFKTAB(R1)                                    15260000
         ST    R1,SCREENH          POINT TO SCREEN START                15270000
         SPACE                                                          15280000
************************************************************            15290000
*                                                          *            15300000
*         POINT TO MEMBER                                  *            15310000
*                                                          *            15320000
************************************************************            15330000
         SPACE                                                          15340000
         MVC   DSNAIM(46),$DSNAME                                       15350000
         TM    DSORG,X'02'         IS THIS A PDS                        15360000
         BZ    NOBLDL              NO, SKIP MEMBER PROCESS              15370000
         MVC   BLDL(4),=AL2(1,76)                                       15380000
         MVC   BLDL+4(8),$MEMBER                                        15390000
         BLDL  (R4),BLDL                                                15400000
         LTR   R15,R15             WAS BLDL SUCCESSFUL                  15410000
         BNZ   NOMEM               NO, BRANCH                           15420000
         MVC   TTR,BLDL+12                                              15430000
         MVC   TTRK,BLDL+12                                             15440000
         FIND  (R4),TTRK,C                                              15450000
*        POINT (R4),TTR                                                 15460000
         SLR   R15,R15                                                  15470000
         IC    R15,TTR+3           GET CONCATENATION NUMBER             15480000
         LTR   R15,R15                                                  15490000
         BZ    NOCONCAT                                                 15500000
         CVD   R15,DOUBLE          APPEND                               15510000
         OI    DOUBLE+7,X'0F'       CONCAT                              15520000
         LH    R15,$DDNAML           NUMBER                             15530000
         SH    R15,=H'4'              TO DDNAME                         15540000
         LA    R15,$DDNAME(R15)         SO IT                           15550000
         UNPK  1(3,R15),DOUBLE+6(2)      BECOMES                        15560000
         MVI   0(R15),C'+'                DDNAME+NNN                    15570000
         MVC   DSNAIM(14),$DDNAML  SHOW DDNAME INSTEAD OF DSNAME        15580000
NOCONCAT EQU   *                                                        15590000
NOBLDL   EQU   *                                                        15600000
         B     NEWNAME                                                  15610000
NOMEM    MVC   MSG(MSG36L),MSG36   MEMBER NOT FOUND                     15620000
         CH    R15,=H'4'                                                15630000
         BE    *+10                                                     15640000
         MVC   MSG(MSG37L),MSG37   BLDL FAILED                          15650000
         OI    STATUS,STNOMEM                                           15660000
         MVC   $MEMBER,=CL8'* '                                         15670000
         SPACE                                                          15680000
************************************************************            15690000
*                                                          *            15700000
*         BUILD DATA SET NAME FOR TOP LINE                 *            15710000
*                                                          *            15720000
************************************************************            15730000
         SPACE                                                          15740000
NEWNAME  MVC   DSPLUS(46),DSNAIM                                        15750000
         CLI   $MEMBER,C' '                                             15760000
         BE    DSPLUSX                                                  15770000
         LA    R1,$MEMBER                                               15780000
         LA    R0,8                                                     15790000
         LA    R15,DSPLUS+2                                             15800000
         AH    R15,DSNAIM                                               15810000
         MVI   0(R15),C'('                                              15820000
         LA    R15,1(,R15)                                              15830000
DSPMEM   CLI   0(R1),C' '                                               15840000
         BE    DSPMEMX                                                  15850000
         MVC   0(1,R15),0(R1)                                           15860000
         LA    R15,1(,R15)                                              15870000
         LA    R1,1(,R1)                                                15880000
         BCT   R0,DSPMEM                                                15890000
DSPMEMX  MVI   0(R15),C')'                                              15900000
         LA    R1,DSPLUS+1                                              15910000
         SR    R15,R1                                                   15920000
         STH   R15,DSPLUS                                               15930000
DSPLUSX  EQU   *                                                        15940000
         L     R1,SCREENH                                               15950000
         MVC   0(SCREENL,R1),SCREEN                                     15960000
***      LA    R15,SCRRTA+3-SCREEN(R1)                                  15970007
***      CLI   PAGESIZE+1,23       3278-5?                              15980007
***      BNE   *+10                NO, BRANCH                           15990007
***      MVC   0(2,R15),=X'C2CA'   YES, ADD ADDRESS FOR 27X132          16000007
         LA    R15,SCRDSN-SCREEN(R1)                                    16010000
         LR    R0,R15                                                   16020000
         LH    R1,DSPLUS                                                16030000
         BCTR  R1,0                                                     16040000
         B     *+10                                                     16050000
         MVC   0(0,R15),DSPLUS+2                                        16060000
         EX    R1,*-6                                                   16070000
         LA    R15,1(R1,R15)                                            16080000
         MVI   0(R15),C' '                                              16090000
         LR    R15,R0                                                   16100000
         MVC   MSGDSN,0(R15)                                            16110000
         SPACE                                                          16120000
         BAL   R14,FILLHOLD                                             16130000
         B     FILLSCR                                                  16140000
         SPACE                                                          16150000
************************************************************            16160000
*                                                          *            16170000
*         READ ENOUGH RECORDS TO FILL HOLD AREA            *            16180000
*                                                          *            16190000
************************************************************            16200000
         SPACE                                                          16210000
FILLHOLD ST    R14,HOLDR                                                16220000
         L     R6,HOLDPTR                                               16230000
         LR    R0,R6                                                    16240000
         L     R1,SAVHOLDL                                              16250000
         SLR   R14,R14                                                  16260000
         L     R15,=A(X'40000000')                                      16270000
         MVCL  R0,R14              FILL HOLD AREA WITH BLANKS           16280000
         L     R7,HOLDTOP          POINT TO HOLD AREA DIRECTORY         16290000
         LH    R8,PAGESIZE         NUMBER OF DATA LINES PER SCREEN      16300000
HILLOOP  EQU   *                                                        16310000
         BAL   R14,GET                                                  16320000
         LA    R0,1                COMPUTE                              16330000
         A     R0,COUNT             THE                                 16340000
         ST    R0,COUNT              RECORD NUMBER                      16350000
         ST    R0,DIRNUM(,R7)      STORE THE RECORD NUMBER              16360000
         ST    R1,DIRLEN(,R7)      LENGTH IN DIRECTORY                  16370000
         LTR   R1,R1               END OF FILE                          16380000
         BM    HILLED              YES BRANCH                           16390000
         C     R1,SAVLRECL         IS RECORD LONGER THAN MAX            16400000
         BNH   *+8                 NO, SKIP NEXT INSTR                  16410000
         L     R1,SAVLRECL         YES, TRUNCATE TO MAX                 16420000
         LR    R0,R2               ADDRESS TO MOVE FROM                 16430000
         L     R14,DIRREC(,R7)     ADDRESS TO MOVE TO                   16440000
         TM    TSTRECFM,X'40'      RECFM V                              16450000
         BZ    HILLNOTV            NO                                   16460000
         SH    R14,=H'4'           YES, MOVE TO PTR MINUS 4             16470000
         L     R15,DIRLEN(,R7)                                          16480000
         SH    R15,=H'4'                                                16490000
         ST    R15,DIRLEN(,R7)                                          16500000
HILLNOTV LR    R15,R1              LENGTH FOR MOVE                      16510000
         MVCL  R14,R0                                                   16520000
         LR    R1,R7               SAVE PTR TO MOST CURRENT ENTRY       16530000
         L     R7,DIRNXT(,R7)      POINT TO NEXT DIRECTORY ENTRY        16540000
         BCT   R8,HILLOOP                                               16550000
         LR    R7,R1               GET MOST CURRENT ENTRY               16560000
HILLED   EQU   *                                                        16570000
         ST    R7,HOLDEND          SAVE LAST RECORD ENTRY               16580000
         L     R14,HOLDR                                                16590000
         BR    R14                                                      16600000
         SPACE                                                          16610000
************************************************************            16620000
*                                                          *            16630000
*         SET UP THE SCREEN HEADER PRIOR TO DISPLAY        *            16640000
*                                                          *            16650000
************************************************************            16660000
         SPACE                                                          16670000
FILLSCR  L     R1,SCREENH                                               16680003
**       LA    R15,SCRERR+2-SCREEN(,R1)                                 16690004
**       MVC   0(31,R15),SCRERR+2      RESET THE HEADER DATA            16700004
         LA    R1,SCRRGE+2-SCREEN(,R1)                                  16710000
         L     R0,SCROLL               DEFAULT SCROLL                   16720000
         CVD   R0,DOUBLE                                                16730000
         OI    DOUBLE+7,X'0F'                                           16740000
         UNPK  0(2,R1),DOUBLE+6(2)                                      16750000
         L     R1,SCREENH                                               16760000
         LA    R15,SCRCOL-SCREEN(,R1)                                   16770000
         MVC   0(8,R15),=CL8' '                                         16780000
         LH    R1,OFFSET                                                16790000
         LA    R1,1(,R1)                                                16800000
         LA    R0,79(,R1)                                               16810000
         TM    MODE,MODEX                                               16820000
         BZ    *+8                                                      16830000
         SH    R0,=H'40'                                                16840000
         CH    R0,RECSIZE                                               16850000
         BNH   *+8                                                      16860000
         LH    R0,RECSIZE                                               16870000
         CVD   R1,DOUBLE                                                16880000
         OI    DOUBLE+7,X'0F'                                           16890000
         CH    R1,=H'1000'                                              16900000
         BL    FILL3DIG                                                 16910000
         UNPK  0(4,R15),DOUBLE+5(3)                                     16920000
         CVD   R0,DOUBLE                                                16930000
         OI    DOUBLE+7,X'0F'                                           16940000
         UNPK  5(3,R15),DOUBLE+6(2)                                     16950000
         B     FILLCOLX                                                 16960000
FILL3DIG UNPK  0(3,R15),DOUBLE+6(2)                                     16970000
         CVD   R0,DOUBLE                                                16980000
         OI    DOUBLE+7,X'0F'                                           16990000
         UNPK  4(3,R15),DOUBLE+6(2)                                     17000000
FILLCOLX EQU   *                                                        17010000
         SPACE                                                          17020000
************************************************************            17030000
*                                                          *            17040000
*         SET UP THE COLUMN HEADER PRIOR TO DISPLAY        *            17050000
*                                                          *            17060000
************************************************************            17070000
         SPACE                                                          17080000
*               IT WOULD BE MORE EFFICIENT TO DO MOST OF THIS WORK      17090000
*               ONLY WHEN THE OFFSET IS INITIALIZED OR CHANGED          17100000
*               AND SAVE THE RESULTS.                                   17110000
         SPACE                                                          17120000
         L     R1,SCREENH                                               17130000
         LA    R6,SCRDATA-SCREEN(,R1)                                   17140000
         MVC   0(52,R6),BLANKS     BLANK THE FOLLOWING AREA             17150001
         CLI   PAGESIZE+1,23       3278-5?                              17160001
         BNE   *+8                 NO, BRANCH                           17170001
         LA    R6,52(,R6)          YES, ADD 52 BLANKS                   17180001
         MVC   0(132,R6),BLANKS    ADD 132 BLANKS                       17190001
         MVC   132(132,R6),BLANKS  ADD 132 BLANKS                       17200001
         LH    R1,OFFSET                                                17210000
         LTR   R1,R1               IS OFFSET ZERO                       17220000
         BNZ   *+8                 NO                                   17230000
         MVI   0(R6),C'1'          YES, SPECIAL CASE, 1                 17240000
         SR    R0,R0                                                    17250000
         LA    R14,10                                                   17260000
         DR    R0,R14                                                   17270000
         SR    R14,R0              10 MINUS REMAINDER                   17280000
         LTR   R0,R0               WAS REMAINDER 0                      17290000
         BZ    *+6                 YES, USE 0, NOT 10                   17300000
         LR    R0,R14                                                   17310000
*                                                                       17320000
         LR    R14,R6                                                   17330000
         AR    R14,R0              ADD (0 TO 9) TO LINE                 17340000
         BCTR  R14,0               START AT LINE-1 FOR OFFSET 0         17350000
*                                  LINE+8 FOR OFFSET 1                  17360000
*                                  LINE+7 FOR OFFSET 2                  17370000
*                                  LINE+6 FOR OFFSET 3                  17380000
*                                  LINE+0 FOR OFFSET 9                  17390000
         LH    R1,OFFSET                                                17400000
         AR    R1,R0               ADD (0 TO 9) TO OFFSET               17410000
         LA    R0,9                                                     17420000
COLNUML  CH    R1,RECSIZE                                               17430000
         BH    COLNUMX                                                  17440000
         CVD   R1,DOUBLE                                                17450000
         MVC   COLNUM,=X'402020202120'                                  17460000
         ED    COLNUM,DOUBLE+5                                          17470000
         CH    R1,=H'10000'        10000-32768                          17480000
         BNL   COLNUMN             YES, USE NNNNN                       17490000
*        MVC   COLNUM(5),COLNUM+1                                       17500000
*        MVI   COLNUM+5,C' '                                            17510000
*        CH    R1,=H'1000'         1000-9999                            17520000
*        BNL   COLNUMN             YES, USE NNNNX                       17530000
         MVC   COLNUM(5),COLNUM+1                                       17540000
         MVI   COLNUM+5,C' '                                            17550000
         CH    R1,=H'100'          100-999                              17560000
         BNL   COLNUMN             YES, USE XNNNX                       17570000
         MVC   COLNUM(5),COLNUM+1  USE XNNXX                            17580000
         MVI   COLNUM+5,C' '                                            17590000
COLNUMN  EQU   *                                                        17600000
         SH    R14,=H'2'                                                17610000
         CR    R14,R6                                                   17620000
         BL    *+10                                                     17630000
         MVC   0(5,R14),COLNUM+1                                        17640000
         LA    R14,12(,R14)                                             17650000
         AH    R1,=H'10'                                                17660000
         BCT   R0,COLNUML                                               17670000
COLNUMX  EQU   *                                                        17680000
*                                                                       17690000
         LH    R1,OFFSET                                                17700000
         SR    R0,R0                                                    17710000
         LA    R14,10                                                   17720000
         DR    R0,R14                                                   17730000
         LR    R1,R0                                                    17740000
         LA    R14,MARKS(R1)                                            17750000
         LH    R1,RECSIZE                                               17760000
         SH    R1,OFFSET                                                17770000
         AH    R6,PAGEWIDT                                              17780001
         CH    R1,=H'80'                                                17790000
         BL    MARKE                                                    17800000
         MVC   0(80,R6),0(R14)                                          17810001
         B     MARKX                                                    17820000
MARKE    MVI   0(R6),C' '                                               17830001
         MVC   1(79,R6),0(R6)                                           17840001
         BCTR  R1,0                                                     17850000
         B     *+10                                                     17860000
         MVC   0(00,R6),0(R14)                                          17870001
         EX    R1,*-6                                                   17880000
MARKX    EQU   *                                                        17890000
         LH    R1,OFFSET                                                17900000
         LTR   R1,R1               IS OFFSET ZERO                       17910000
         BNZ   *+8                 NO                                   17920000
         MVI   0(R6),C'+'          YES, SPECIAL CASE                    17930001
         AH    R6,PAGEWIDT                                              17940001
         SPACE                                                          17950000
************************************************************            17960000
*                                                          *            17970000
*         FILL IN THE DATA AREA OF THE SCREEN              *            17980000
*                                                          *            17990000
************************************************************            18000000
         SPACE                                                          18010000
         SR    R5,R5                                                    18020000
*        LR    R0,R6                                                    18030000
*        L     R1,=A(80*PAGE)                                           18040000
*        SLR   R14,R14                                                  18050000
*        L     R15,=A(X'40000000')                                      18060000
*        MVCL  R0,R14              FILL DATA AREA WITH BLANKS           18070000
         L     R7,HOLDTOP                                               18080000
         L     R1,SCREENH                                               18090000
         LA    R15,SCRLINE-SCREEN(,R1)                                  18100000
         L     R1,DIRNUM(,R7)      GET NUMBER OF FIRST LINE             18110000
         CVD   R1,DOUBLE                                                18120000
         OI    DOUBLE+7,X'0F'                                           18130000
         UNPK  0(5,R15),DOUBLE+5(3)                                     18140000
**       LA    R1,SCRERR+2-SCREEN(,R1)                                  18150004
**       MVC   SAVELINE(31),0(R1)  SAVE LINE AND COLUMN                 18160004
         LH    R8,PAGESIZE         NUMBER OF DATA LINES                 18170003
FILHELP  CLI   HELPFLG,C'H'        HELP REQUESTED?                      18180003
         BNE   FILBUG              NO, BRANCH                       ABL 18190000
         LA    R1,HELPMSGS         START OF HELP MESSAGES           ABL 18200000
FILHEL2  MVC   0(132,R6),BLANKS    INITIALIZE WITH BLANKS           ABL 18210001
         CLI   0(R1),X'FF'         END OF MESSAGES?                 ABL 18220000
         BE    FILHEL4             YES, BRANCH                      ABL 18230000
         MVC   0(80,R6),0(R1)      MOVE IN THE NEXT MESSAGE         ABL 18240000
         LA    R1,80(,R1)          NEXT HELP TEXT                   ABL 18250000
FILHEL4  AH    R6,PAGEWIDT         NEXT OUTPUT START                ABL 18260001
         LA    R5,01(,R5)          COUNT LINES FILLED IN            ABL 18270000
         BCT   R8,FILHEL2          DO ALL HELP MESSAGES             ABL 18280000
         B     FILLED                                               ABL 18290000
         SPACE 2                                                    ABL 18300000
FILBUG   CLI   HELPFLG,C'D'        DEBUG REQUESTED?                 ABL 18310000
         BNE   FILLOOP             NO, BRANCH                       ABL 18320000
* LINE 1                                                            ABL 18330000
         MVC   0(132,R6),BLANKS    INITIALIZE WITH BLANKS           ABL 18340001
         LA    R5,01(,R5)          COUNT LINES FILLED IN            ABL 18350000
         LA    R1,TGETREG2         POINT TO R15, R0, R1             ABL 18360000
         LA    R0,4                FIRST 4 BYTES (R15)              ABL 18370000
         LA    R15,0(,R6)                                           ABL 18380000
         BAL   R14,HEX                                              ABL 18390000
         LA    R15,1(,R15)                                          ABL 18400000
         LA    R0,4                SECOND 4 BYTES (R0)              ABL 18410000
         BAL   R14,HEX                                              ABL 18420000
         LA    R15,1(,R15)                                          ABL 18430000
         LA    R0,4                THIRD 4 BYTES (R1)               ABL 18440000
         BAL   R14,HEX                                              ABL 18450000
         SPACE 1                                                    ABL 18460000
* LINE 2                                                            ABL 18470000
         AH    R6,PAGEWIDT         NEXT OUTPUT START                ABL 18480001
         MVC   0(132,R6),BLANKS    INITIALIZE WITH BLANKS           ABL 18490001
         LA    R5,01(,R5)          COUNT LINES FILLED IN            ABL 18500000
         LA    R1,REPLY+128        REPLY AREA                       ABL 18510000
         L     R14,TGETREG2+8      REPLY LENGTH RETURNED BY TGET    ABL 18520000
         LA    R15,0(,R6)                                           ABL 18530000
         S     R14,=F'1'                                            ABL 18540000
         BM    FILLED                                               ABL 18550000
         MVC   0(5,R15),=C'DATA:'  CHARACTER HEADER                 ABL 18560000
         MVC   5(*-*,R15),0(R1)    <<EXECUTED>>                     ABL 18570000
         EX    R14,*-6             MOVE IN THE CHARACTER VERSION    ABL 18580000
         TR    0(80,R15),ASIST     TRANSLATE UNPRINTABLES           ABL 18590000
         SPACE 1                                                    ABL 18600000
* LINE 3                                                            ABL 18610000
         AH    R6,PAGEWIDT         NEXT OUTPUT START                ABL 18620002
         MVC   0(132,R6),BLANKS    INITIALIZE WITH BLANKS           ABL 18630002
         LA    R5,01(,R5)          COUNT LINES FILLED IN            ABL 18640000
         LA    R1,REPLY+128        REPLY AREA                       ABL 18650000
         L     R0,TGETREG2+8       REPLY LENGTH RETURNED BY TGET    ABL 18660000
         LA    R15,0(,R6)                                           ABL 18670000
         BAL   R14,HEX             CONVERT REPLY TO HEX             ABL 18680000
         AH    R6,PAGEWIDT         NEXT OUTPUT START                ABL 18690002
         B     FILLED                                               ABL 18700000
FILLOOP  MVI   HELPFLG,0           REFRESH OF SCREEN NOT REQUIRED   ABL 18710000
         LM    R1,R2,DIRLEN(R7)    GET LENGTH AND ADDRESS               18720000
         LTR   R1,R1                                                    18730000
         BM    FILBOT                                                   18740000
         MVC   0(132,R6),BLANKS    FILL LINE WITH BLANKS                18750002
         BZ    FILNUL              BRANCH IF R1 ZERO                    18760000
         TM    SMFSW,SMFBIT        IS SMF FORMATTING ON                 18770000
         BO    CALLSMF             YES, BRANCH                          18780000
         SH    R1,OFFSET           IS OFFSET BEYOND END OF RECORD       18790000
         BNP   FILLNEXT            YES, LEAVE LINE BLANK                18800000
         AH    R2,OFFSET                                                18810000
         TM    MODE,MODEX                                               18820000
         BO    FILLHEX                                                  18830000
         CH    R1,=H'80'                                                18840000
         BNH   *+8                                                      18850000
         LA    R1,80                                                    18860000
         BCTR  R1,0                                                     18870000
         B     *+10                                                     18880000
         MVC   0(0,R6),0(R2)                                            18890000
         EX    R1,*-6                                                   18900000
         TR    0(80,R6),PERIODS                                         18910000
         B     FILLNEXT                                                 18920000
FILLHEX  CH    R1,=H'40'                                                18930000
         BNH   *+8                                                      18940000
         LA    R1,40                                                    18950000
         LR    R0,R1                                                    18960000
         LR    R1,R2                                                    18970000
         LR    R15,R6                                                   18980000
         BAL   R14,HEX                                                  18990000
         B     FILLNEXT                                                 19000000
FILNUL   MVC   0(6,R6),=C'(NULL)'                                       19010000
         B     FILLNEXT                                                 19020000
CALLSMF  STM   R1,R2,CALLPARM                                           19030000
         ST    R6,CALLPARM+8                                            19040000
         LA    R0,CALLSAVE                                              19050000
         ST    R0,CALLPARM+12                                           19060000
         OI    CALLPARM+12,X'80'                                        19070000
         LA    R1,CALLPARM                                              19080000
         L     R15,CALLSMFA                                             19090000
         BALR  R14,R15                                                  19100000
         TR    0(80,R6),PERIODS                                         19110000
FILLNEXT LA    R5,01(,R5)          COUNT LINES FILLED IN                19120000
         AH    R6,PAGEWIDT                                              19130002
         L     R7,DIRNXT(,R7)                                           19140000
         BCT   R8,FILLOOP                                               19150000
         B     FILLED                                                   19160000
FILBOT   MVI   0(R6),C'*'                                               19170000
         MVC   1(79,R6),0(R6)                                           19180000
         MVC   32(16,R6),=C' BOTTOM OF DATA '                           19190000
         AH    R6,PAGEWIDT                                              19200002
         LA    R5,01(,R5)          COUNT LINES FILLED IN                19210000
FILLED   EQU   *                                                        19220000
         CH    R5,PAGESIZE         IS SCREEN FILLED UP                  19230000
         BNL   NOPAD                                                    19240000
         MVC   0(4,R6),SCRPAD      FILL REMAINING LINES WITH BLANKS ABL 19250000
         LA    R6,4(,R6)                                            ABL 19260000
NOPAD    MVC   0(4,R6),SCRSUF      SET CURSOR ADDRESS                   19270000
         CLI   PAGESIZE+1,23       3278-5?                              19280000
         BNE   *+10                NO, BRANCH                           19290000
         MVC   1(2,R6),=X'C2D4'    YES, ADD ADDRESS FOR 02X17           19300000
         LA    R6,4(,R6)                                                19310000
         ST    R6,SCREENF          STORE END-OF-SCREEN FOR TPUT         19320000
         SPACE                                                          19330000
PROMPT   CLI   HELPFLG,C'H'        HELP?                            ABL 19340000
         BE    PROMPT2             YES, BRANCH                      ABL 19350000
         CLI   HELPFLG,C'R'        REFRESH REQUIRED?                ABL 19360000
         BNL   FILLSCR             YES, BRANCH                      ABL 19370000
         CLI   HELPFLG,C'D'        DEBUG?                           ABL 19380000
         BNE   PROMPT2+4           NO, BRANCH                       ABL 19390000
         MVI   HELPFLG,C'S'        REFRESH REQUIRED (NEXT TIME)     ABL 19400000
         B     PROMPT2+4                                            ABL 19410000
         SPACE 1                                                    ABL 19420000
PROMPT2  MVI   HELPFLG,C'R'        REFRESH REQUIRED (NEXT TIME)     ABL 19430000
         L     R1,SCREENH                                               19440000
**       LA    R15,SCRERR+2-SCREEN(,R1)                                 19450004
**       MVC   0(31,R15),SAVELINE  RESET LINE AND COLUMN                19460004
         LA    R15,SCRDSN-SCREEN(R1)                                    19470000
         LH    R1,MSG              GET LENGTH OF MESSAGE                19480000
         LTR   R1,R1               IS THERE A MESSAGE                   19490000
         BNZ   PROMPTM             YES, BRANCH                          19500000
         MVC   0(55,R15),MSGDSN    NO, DISPLAY DSNAME                   19510000
         B     PROMPTT                                                  19520000
PROMPTMV MVC   0(0,R15),MSG+2                                           19530000
PROMPTM  CH    R1,=H'55'           MSG TOO LONG                         19540000
         BNH   *+8                 NO                                   19550000
         LA    R1,55               YES, TRUNCATE IT                     19560000
         MVI   0(R15),C'-'                                              19570000
         MVC   1(54,R15),0(R15)                                         19580000
         BCTR  R1,0                LENGTH-1 FOR EX                      19590000
         EX    R1,PROMPTMV                                              19600000
         LA    R15,1(R1,R15)                                            19610000
         MVI   0(R15),C' '                                              19620000
         XC    MSG(2),MSG          SET OFF MESSAGE FLAG                 19630000
PROMPTT  L     R1,SCREENH                                               19640000
         L     R0,SCREENF                                               19650000
         SR    R0,R1               COMPUTE SIZE OF SCREEN               19660000
         O     R1,=A(X'03000000')                                       19670000
         SVC   93                  TPUT                                 19680000
         SPACE                                                          19690000
************************************************************            19700000
*                                                          *            19710000
*         READ REPLY FROM THE TERMINAL                     *            19720000
*                                                          *            19730000
************************************************************            19740000
         SPACE                                                          19750000
         MVC   REPLY+128(128),REPLY   FOR DEBUG                     ABL 19760000
         MVC   TGETREG2(12),TGETREGS  FOR DEBUG                     ABL 19770000
         XC    REPLY(128),REPLY       TO SIMPLIFY DEBUGGING         ABL 19780000
         LA    R1,REPLY                                             ABL 19790000
         LA    R0,128                                               ABL 19800000
         O     R1,=A(X'81000000')                                       19810000
         SVC   93                  TGET                                 19820000
         STM   R15,R1,TGETREGS                                          19830000
         CH    R15,=H'12'          WAS REPLY AREA LONG ENOUGH           19840000
         BNE   TGETOK              YES, BRANCH                          19850000
         TCLEARQ INPUT                                                  19860000
TGETOK   LA    R15,REPLY                                                19870000
*               REPLY AREA CONTENTS:                                    19880000
*               OFFSET 0 LENGTH 1  -  ID OF KEY                         19890000
*               OFFSET 1 LENGTH 2  -  ADDRESS OF CURSOR                 19900000
*               OFFSET 3 LENGTH 1  -  X'11' IF ANY FIELDS MODIFIED      19910000
*               OFFSET 4 LENGTH 2  -  SCREEN ADDRESS OF FIELD           19920000
*               OFFSET 6 LENGTH V  -  DATA FROM FIELD                   19930000
*               DATA EXTENDS TO END OF BUFFER OR NEXT X'11'             19940000
         OI    REPLY,X'30'         MAKE PFK 13-24 LOOK LIKE 1-12        19950000
         CLI   REPLY,X'F3'         PFK 3                                19960000
         BE    END                                                      19970000
*        CLI   REPLY,X'7D'         ENTER                                19980000
*        BNE   PFK                                                      19990000
         L     R14,TGETREGS+8      GET LENGTH OF REPLY                  20000000
         CH    R14,=H'3'           ANYTHING BEYOND CURSOR ADDRESS       20010000
         BNH   CMDNULL             NO, BRANCH                           20020000
         CLI   3(R15),X'11'        SBA (SHOULD ALWAYS BE PRESENT)       20030000
         BNE   CMDNULL             NO                                   20040000
*                                                                       20050000
*               PROCESS FIELDS IN TGET BUFFER                           20060000
*                                                                       20070000
         SLR   R0,R0                                                    20080000
         ST    R0,CMDPTR           START WITH COMMAND-NOT-PRESENT       20090000
         BCTR  R14,0               LENGTH-1 FOR EX                      20100000
         B     *+10                                                     20110000
TRTSBA   TRT   0(0,R15),FINDSBA                                         20120000
         EX    R14,*-6                                                  20130000
         BZ    CMDNULL             NO FIELDS PRESENT                    20140000
         LR    R0,R1               ADDRESS OF SBA                       20150000
         SR    R0,R15              LENGTH OF PRECEDING DATA             20160000
         SR    R14,R0              LENGTH CODE OF REMAINING DATA        20170000
         BZ    PARSED              BRANCH IF NOTHING FOLLOWS SBA        20180000
LOOP     ST    R1,FLDPTR                                                20190000
         LA    R15,1(,R1)          POINT PAST SBA                       20200000
         BCTR  R14,0               REDUCE LENGTH ACCORDINGLY            20210000
         EX    R14,TRTSBA          LOOK FOR SECOND SBA                  20220000
         BZ    LAST                BRANCH IF NONE PRESENT               20230000
         LR    R0,R1               POINT TO NEXT SBA                    20240000
         SR    R0,R15              GET LENGTH OF THIS DATA              20250000
         ST    R0,FLDLEN                                                20260000
         STM   R14,R1,SBASAVE                                           20270000
         BAL   R2,FIELD                                                 20280000
         LM    R14,R1,SBASAVE                                           20290000
         SR    R14,R0              LENGTH CODE OF REMAINING DATA        20300000
         BNZ   LOOP                SOMETHING FOLLOWS SECOND SBA         20310000
         B     PARSED              NOTHING FOLLOWS SECOND SBA           20320000
LAST     LA    R0,1(,R14)          GET LENGTH OF THIS DATA              20330000
         ST    R0,FLDLEN                                                20340000
*        STM   R14,R1,SBASAVE                                           20350000
         BAL   R2,FIELD                                                 20360000
         B     PARSED                                                   20370000
FIELD    L     R15,FLDPTR          POINT TO SBA FOR FIELD               20380000
         L     R1,FLDLEN           GET LENGTH CODE                      20390000
         SH    R1,=H'3'            GET LENGTH CODE OF DATA              20400000
         BM    0(,R2)              NO DATA PRESENT                      20410000
         CLC   1(2,R15),FLD1BA     IS IT COMMAND?                       20420000
         BE    FLD1                                                     20430000
         CLC   1(2,R15),FLD1BB     IS IT COMMAND ON 3278-5?             20440000
         BE    FLD1                                                     20450000
         CLC   1(2,R15),FLD2BA     IS IT RANGE?                         20460000
         BE    FLD2                                                     20470000
         CLC   1(2,R15),FLD2BB     IS IT RANGE ON 3278-5?               20480000
         BE    FLD2                                                     20490000
         B     0(,R2)              USER MUST HAVE USED CLEAR KEY        20500000
         B     0(,R2)              USER MUST HAVE USED CLEAR KEY        20510000
FLD1     LA    R15,3(,R15)         POINT TO COMMAND                     20520000
         ST    R15,CMDPTR          SAVE FOR LATER                       20530000
         ST    R1,CMDLEN           SAVE LENGTH CODE FOR LATER           20540000
         BR    R2                  RETURN                               20550000
FLD2     LA    R15,3(,R15)         POINT TO RANGE                       20560000
         CH    R1,=H'1'            IS IT 2 BYTES                        20570000
         BNE   FLD2A               NO, BRANCH                           20580000
         CLC   0(2,R15),=C'  '     BLANK                                20590000
         BE    0(,R2)              YES, IGNORE                          20600000
         CLI   0(R1),C' '          LEADING BLANK                        20610000
         BNE   *+8                 NO, BRANCH                           20620000
         MVI   0(R1),C'0'          CHANGE TO LEADING ZERO               20630000
         CLI   1(R15),C' '         TRAILING BLANK                       20640000
         BNE   FLD2A               NO, BRANCH                           20650000
         BCTR  R1,0                YES, REDUCE LENGTH BY 1              20660000
FLD2A    LR    R14,R2              SAVE R2                              20670000
         EX    R1,TRTNUM           IS DATA NUMERIC                      20680000
         LR    R2,R14              RESTORE R2                           20690000
         BNZ   0(,R2)              NOT NUMERIC, BRANCH                  20700000
         EX    R1,FLD2PACK         PACK IT                              20710000
         CVB   R1,DOUBLE           CONVERT TO BINARY                    20720000
         LTR   R1,R1               IS IT ZERO                           20730000
         BZ    0(,R2)              YES, IGNORE                          20740000
         CH    R1,PAGESIZE         IT IT GREATER THAN SCREEN SIZE       20750000
         BH    0(,R2)              YES, IGNORE                          20760000
         ST    R1,SCROLL           SET NEW RANGE                        20770000
         LR    R0,R1                                                    20780000
         L     R1,SCREENH                                               20790000
         LA    R1,SCRRGE+2-SCREEN(,R1)                                  20800000
         CVD   R0,DOUBLE                                                20810000
         OI    DOUBLE+7,X'0F'                                           20820000
         UNPK  0(2,R1),DOUBLE+6(2)                                      20830000
         BR    R2                                                       20840000
TRTNUM   TRT   0(0,R15),NUMERIC    (EXECUTED)                           20850000
FLD2PACK PACK  DOUBLE(8),0(0,R15)  (EXECUTED)                           20860000
PARSED   L     R15,CMDPTR          POINT TO COMMAND                     20870000
         LTR   R15,R15             IS THERE A COMMAND                   20880000
         BZ    CMDNULL             NO, BRANCH TO CHECK PFK              20890000
         L     R14,CMDLEN          GET LENGTH CODE                      20900000
         LA    R1,CMDAREA                                               20910000
         MVI   0(R1),C' '                                               20920000
         MVC   1(62,R1),0(R1)                                           20930000
         EX    R14,CMDMOVE                                              20940000
         LA    R0,30                                                    20950000
CMDJUST  CLI   0(R1),C' '          IS COMMAND LEFT JUSTIFIED            20960000
         BNE   CMDJUSTX            YES, BRANCH                          20970000
         MVC   0(62,R1),1(R1)      SHIFT IT LEFT                        20980000
         MVI   62(R1),C' '         APPEND BLANK                         20990000
         BCT   R0,CMDJUST          REPEAT                               21000000
CMDJUSTX EQU   *                                                        21010000
         OC    0(63,R1),=CL63' '   UPPER CASE                           21020000
         B     CMDSCAN                                                  21030000
CMDMOVE  MVC   0(0,R1),0(R15)      (EXECUTED)                           21040000
CMDNULL  CLI   REPLY,X'7D'         ENTER                                21050000
         BE    PROMPT              YES, NO ACTION                       21060000
         SPACE                                                          21070000
************************************************************            21080000
*                                                          *            21090000
*         INTERPRET PROGRAM FUNCTION KEY                   *            21100000
*                                                          *            21110000
************************************************************            21120000
         SPACE                                                          21130000
*               NOTE - IF A COMMAND IS TYPED IN THEN                    21140000
*               IT OVERRIDES THE PF KEY.                                21150000
PFK      LA    R1,PFKTAB-4095                                           21160000
         LA    R1,4095(,R1)                                             21170000
PFKSRCH  CLI   0(R1),0             END OF PFK TABLE                     21180000
         BE    NOTPFK              YES, NOT IN TABLE                    21190000
         CLC   0(1,R1),REPLY       DOES PFK CODE MATCH                  21200000
         BE    PFKMOVE             YES, BRANCH                          21210000
         LA    R1,PFKTABL(,R1)     NO, POINT TO NEXT ENTRY IN TABLE     21220000
         B     PFKSRCH             KEEP SEARCHING THE TABLE             21230000
PFKMOVE  CLI   1(R1),C' '          IS PFK DEFINITION EMPTY              21240000
         BE    NOTPFK              YES, BRANCH                          21250000
         MVC   CMDAREA(63),BLANKS  BLANK THE AREA                   ABL 21260000
         MVC   CMDAREA(PFKTABL-1),1(R1)  MOVE IN THE TEXT           ABL 21270000
         LA    R1,CMDAREA                POINT TO ASSOCIATED CMD.   ABL 21280000
         SPACE                                                          21290000
************************************************************            21300000
*                                                          *            21310000
*         PARSE THE COMMAND AND OPERANDS                   *            21320000
*                                                          *            21330000
************************************************************            21340000
         SPACE                                                          21350000
CMDSCAN  EQU   *                                                        21360000
         LR    R15,R1              POINT TO COLUMN 1                    21370000
         LA    R3,39               LENGTH CODE OF STATEMENT             21380000
         LA    R6,OPDL             POINT TO OPERAND DESCRIPTOR LIST     21390000
         XC    0(OPDLL,R6),0(R6)   ZERO THE OPDL                        21400000
         SR    R1,R1               INSURE HI ORDER BYTE ZERO            21410000
         LA    R0,OPDLL/8-1        NUMBER OF ENTRIES IN O.D.L.          21420000
*                                  MINUS 1 (LAST ODE WILL REMAIN ZERO)  21430000
         B     PARLOOP                                                  21440000
TRTNONBL TRT   0(0,R15),TABNONBL   (EXECUTED)                           21450000
TRTBLANK TRT   0(0,R15),TABBLANK   (EXECUTED)                           21460000
TRTQUOTE TRT   0(0,R15),TABQUOTE   (EXECUTED)                           21470000
PARLOOP  XC    0(8,R6),0(R6)       ZERO THE OPERAND DESCRIPTOR ENTRY    21480000
         EX    R3,TRTNONBL         FIND A NONBLANK                      21490000
         BZ    PARDONE             BRANCH IF ALL BLANKS                 21500000
         LR    R14,R1              GET ADDRESS OF STRING                21510000
         SR    R14,R15             GET LENGTH OF PRECEDING BLANKS       21520000
         SR    R3,R14              GET LENGTH OF REMAINING TEXT         21530000
         LR    R15,R1              GET ADDRESS OF NONBLANK              21540000
         CLI   0(R15),QUOTE        DOES OPERAND BEGIN WITH A QUOTE      21550000
         BE    PARQUOTE            YES, BRANCH                          21560000
         EX    R3,TRTBLANK         FIND A BLANK                         21570000
         BZ    PARLAST             BRANCH IF NOT FOUND                  21580000
         LR    R14,R1              GET ADDRESS OF BLANK                 21590000
         SR    R14,R15             GET LENGTH OF FIELD                  21600000
         OI    6(R6),PRESENT       OPERAND PRESENT                      21610000
         ST    R15,0(,R6)          ADDRESS OF OPERAND                   21620000
         STH   R14,4(,R6)          LENGTH OF OPERAND                    21630000
         SR    R3,R14              GET LENGTH CODE OF REMAINING TEXT    21640000
         BZ    PARDONE             BRANCH IF ONE TRAILING BLANK         21650000
         LR    R15,R1              POINT TO BLANK                       21660000
PARNEXT  LA    R6,8(,R6)           POINT TO NEXT O.D.E.                 21670000
         BCT   R0,PARLOOP          GO PROCESS NEXT OPERAND              21680000
         B     PARDONE             MAX OPERANDS PROCESSED               21690000
PARQUOTE LA    R15,1(,R15)         POINT PAST QUOTE                     21700000
         LA    R1,1                VALUE 1                              21710000
         SR    R3,R1               REDUCE LENGTH CODE BY 1              21720000
         BM    PARDONE             IGNORE QUOTE IN LAST COLUMN          21730000
         EX    R3,TRTQUOTE         FIND THE NEXT QUOTE                  21740000
         BZ    PARLASTQ            NOT FOUND, USE ALL REM TEXT          21750000
         LR    R14,R1              GET ADDRESS OF QUOTE                 21760000
         SR    R14,R15             GET LENGTH OF FIELD                  21770000
         OI    6(R6),PRESENT+QUOTED OPERAND PRESENT AND IN QUOTES       21780000
         ST    R15,0(,R6)          ADDRESS OF OPERAND                   21790000
         STH   R14,4(,R6)          LENGTH OF OPERAND                    21800000
         SR    R3,R14              GET LENGTH CODE OF REM TEXT          21810000
         BZ    PARDONE             BRANCH IF QUOTE WAS FINAL CHAR       21820000
         LA    R15,1(,R1)          POINT TO CHAR AFTER ENDING QUOTE     21830000
         BCTR  R3,0                REDUCE LENGTH CODE BY 1              21840000
         B     PARNEXT             SET UP FOR NEXT OPERAND              21850000
PARLASTQ OI    6(R6),QUOTED        OPERAND IN QUOTES                    21860000
PARLAST  LA    R14,1(,R3)          GET LENGTH                           21870000
         OI    6(R6),PRESENT       OPERAND PRESENT                      21880000
         ST    R15,0(,R6)          ADDRESS OF OPERAND                   21890000
         STH   R14,4(,R6)          LENGTH OF OPERAND                    21900000
PARDONE  EQU   *                                                        21910000
         L     R1,OPD0             POINT TO COMMAND                     21920000
         CLC   0(2,R1),=C'M '                                       ABL 21930000
         BE    PFALSO                                               ABL 21940000
         CLI   0(R1),X'EF'                                          ABL 21950000
         BH    PFALSO                                               ABL 21960000
         CLC   0(4,R1),=C'TSO '                                     ABL 21970000
         BE    TSO                                                  ABL 21980000
         CLC   0(5,R1),=C'HELP '                                    ABL 21990000
         BE    HELP                                                 ABL 22000000
         CLC   0(2,R1),=C'H '                                       ABL 22010000
         BE    HELP                                                 ABL 22020000
         CLC   0(5,R1),=C'LEFT '                                        22030000
         BE    LEFT                                                     22040000
         CLC   0(2,R1),=C'< '                                           22050000
         BE    LEFT                                                     22060000
         CLC   0(6,R1),=C'RIGHT '                                       22070000
         BE    RIGHT                                                    22080000
         CLC   0(2,R1),=C'> '                                           22090000
         BE    RIGHT                                                    22100000
         CLC   0(3,R1),=C'UP '                                          22110000
         BE    UP                                                       22120000
         CLC   0(2,R1),=C'- '                                           22130000
         BE    UP                                                       22140000
         CLC   0(5,R1),=C'DOWN '                                        22150000
         BE    DOWN                                                     22160000
         CLC   0(2,R1),=C'+ '                                           22170000
         BE    DOWN                                                     22180000
         CLC   0(4,R1),=C'TOP '                                         22190000
         BE    TOP                                                      22200000
         CLC   0(7,R1),=C'BOTTOM '                                      22210000
         BE    BOTTOM                                                   22220000
         CLC   0(4,R1),=C'BOT '                                         22230000
         BE    BOTTOM                                                   22240000
         CLC   0(5,R1),=C'LIST '                                        22250000
         BE    LIST                                                     22260000
         CLC   0(2,R1),=C'L '                                           22270000
         BE    LIST                                                     22280000
         CLC   0(5,R1),=C'FIND '                                        22290000
         BE    FIND                                                     22300000
         CLC   0(2,R1),=C'F '                                           22310000
         BE    FIND                                                     22320000
         CLC   0(8,R1),=C'FINDSMF '                                     22330000
         BE    FINDSMF                                                  22340000
         CLC   0(3,R1),=C'FS '                                          22350000
         BE    FINDSMF                                                  22360000
         CLC   0(7,R1),=C'MEMBER '                                      22370000
         BE    MEMBERP                                                  22380000
         CLC   0(4,R1),=C'MEM '                                         22390000
         BE    MEMBERP                                                  22400000
         CLC   0(4,R1),=C'END '                                         22410000
         BE    END                                                      22420000
         CLC   0(5,R1),=C'ASIS '                                        22430000
         BE    ASIS                                                     22440000
         CLC   0(5,R1),=C'CAPS '                                        22450000
         BE    CAPS                                                     22460000
         CLC   0(4,R1),=C'HEX '                                         22470000
         BE    HEXMODE                                                  22480000
         CLC   0(3,R1),=C'PFK'                                          22490000
         BE    PFKSET                                                   22500000
         CLC   0(4,R1),=C'SMF '                                         22510000
         BE    SMFMODE                                                  22520000
         CLC   0(5,R1),=C'SNAP '                                        22530000
         BE    SNAP                                                     22540000
         CLC   0(6,R1),=C'DEBUG '                                       22550000
         BE    DEBUG                                                    22560000
         MVC   MSG(MSG20L),MSG20                                        22570000
         B     PROMPT                                                   22580000
NOTPFK   EQU   *                                                        22590000
         CLI   REPLY,X'7E'           RESHOW?                        ABL 22600000
         BE    FILLSCR               YES, BRANCH                    ABL 22610000
         MVC   MSG(MSG21L),MSG21                                    ABL 22620000
         CLI   DEBUGSW,C'D'                                         ABL 22630000
         BE    DEBUG                                                ABL 22640000
         B     PROMPT                                               ABL 22650000
         SPACE                                                          22660000
************************************************************        ABL 22670000
*                                                          *        ABL 22680000
*         INTERPRET AMOUNT AND POSSIBLE PF KEY             *        ABL 22690000
*                                                          *        ABL 22700000
************************************************************        ABL 22710000
         SPACE                                                          22720000
PFALSO   EQU   *                                                    ABL 22730000
         CLI   REPLY,X'7D'         ENTER ONLY?                      ABL 22740000
         BE    PFABAD              YES, ERROR                       ABL 22750000
         LA    R15,PFKTAB-4095                                      ABL 22760000
         LA    R15,4095(,R15)                                       ABL 22770000
PFASRCH  CLI   0(R15),0            END OF PFK TABLE                 ABL 22780000
         BE    PFABAD              YES, NOT IN TABLE                ABL 22790000
         CLC   0(1,R15),REPLY      DOES PFK CODE MATCH              ABL 22800000
         BE    PFAMOVE             YES, BRANCH                      ABL 22810000
         LA    R15,PFKTABL(,R15)   NO, POINT TO NEXT ENTRY IN TABLE ABL 22820000
         B     PFASRCH             KEEP SEARCHING THE TABLE         ABL 22830000
PFAMOVE  CLI   1(R15),C' '         IS PFK DEFINITION EMPTY          ABL 22840000
         BE    PFABAD              YES, BRANCH                      ABL 22850000
         LA    R15,1(,R15)         POINT TO ASSOCIATED COMMAND      ABL 22860000
         CLI   0(R1),C'M'          MAXIMUM?                         ABL 22870000
         BNE   PFAMOUNT            NO, BRANCH                       ABL 22880000
         CLC   0(3,R15),=C'UP '                                     ABL 22890000
         BE    TOP                                                  ABL 22900000
         CLC   0(5,R15),=C'DOWN '                                   ABL 22910000
         BE    BOTTOM                                               ABL 22920000
         CLC   0(5,R15),=C'LEFT '                                   ABL 22930000
         BE    LEFTALL                                              ABL 22940000
         CLC   0(6,R15),=C'RIGHT '                                  ABL 22950000
         BE    RIGHTALL                                             ABL 22960000
         B     PFABAD                                               ABL 22970000
PFAMOUNT MVC   OPD1(8),OPD0        MOVE THE OPERANDS UP ONE         ABL 22980000
         LR    R1,R15              POINT TO THE PFK TEXT            ABL 22990000
         ST    R1,OPD0             POINT TO THE SUBCOMMAND          ABL 23000000
         CLC   0(3,R1),=C'UP '                                      ABL 23010000
         BE    PARDONE                                              ABL 23020000
         CLC   0(5,R1),=C'DOWN '                                    ABL 23030000
         BE    PARDONE                                              ABL 23040000
         CLC   0(5,R1),=C'LEFT '                                    ABL 23050000
         BE    PARDONE                                              ABL 23060000
         CLC   0(6,R1),=C'RIGHT '                                   ABL 23070000
         BE    PARDONE                                              ABL 23080000
PFABAD   MVC   MSG(MSG20L),MSG20                                    ABL 23090000
         B     PROMPT                                               ABL 23100000
         SPACE                                                          23110000
************************************************************        ABL 23120000
*                                                          *        ABL 23130000
*         TSO                                              *        ABL 23140000
*                                                          *        ABL 23150000
************************************************************        ABL 23160000
         SPACE                                                          23170000
TSO      EQU   *                                                    ABL 23180000
         LA    R1,CMDAREA          START OF TSO PARAMETERS          ABL 23190000
         LA    R0,63               LENGTH OF PARSE STRING           ABL 23200000
         STH   R0,0(,R1)           SAVE FOR PARSE                   ABL 23210000
         LA    R15,OPD1            FIRST PARAMETER                  ABL 23220000
         TM    6(R15),PRESENT      ANY OPERANDS?                    ABL 23230000
         BZ    FILLSCR             NO, IGNORE                       ABL 23240000
         TM    6(R15),QUOTED       QUOTED?                          ABL 23250000
         BO    ERRINV              YES, INVALID                     ABL 23260000
         L     R14,0(,R15)         START OF PARAMETER               ABL 23270000
         LH    R15,4(,R15)         LENGTH OF PARAMETER              ABL 23280000
         LTR   R15,R15             IS IT NULL STRING                ABL 23290000
         BZ    ERRINV              YES, ERROR                       ABL 23300000
         CH    R15,=H'8'           IS LENGTH MORE THAN 8            ABL 23310000
         BH    ERRINV              YES, ERROR                       ABL 23320000
         BCTR  R15,0               MACHINE LENGTH                   ABL 23330000
         MVC   BLDL(4),=AL2(1,76)                                   ABL 23340000
         MVC   BLDL+4(8),BLANKS                                     ABL 23350000
         MVC   BLDL+4(*-*),0(R14)  <<EXECUTED>>                     ABL 23360000
         EX    R15,*-6             MOVE IN THE COMMAND NAME         ABL 23370000
         L     R6,REVECT           ECT ADDRESS                      ABL 23380000
         CLC   BLDL+4(2),=C'RX'    DUMPER?                          ABL 23390000
         BE    *+10                YES, BRANCH                      ABL 23400000
         MVC   ECTPCMD-ECT(8,R6),BLDL+4   COMMAND NAME IS CHANGED   ABL 23410000
         MVC   ECTSCMD-ECT(8,R6),BLANKS   NO SECONDARY COMMAND NAME ABL 23420000
         LA    R15,OPD2            START OF SECOND ODL              ABL 23430000
         L     R14,0(,R15)         START OF SECOND PARAMETER        ABL 23440000
         LA    R14,0(,R14)         DROP TOP BYTE                    ABL 23450000
         SR    R14,R1              OFFSET TO SECOND PARAMETER       ABL 23460000
         SH    R14,=H'4'           LESS TEXT LENGTH FIELDS          ABL 23470000
         NI    ECTSWS-ECT(R6),X'FF'-ECTNOPD ASSUME OPERANDS         ABL 23480000
         TM    6(R15),PRESENT      ANY OPERANDS?                    ABL 23490000
         BO    TSO010              YES, BRANCH                      ABL 23500000
         LA    R14,63              NO, USE DEFAULT                  ABL 23510000
         OI    ECTSWS-ECT(R6),ECTNOPD      ASSUME NO OPERAND        ABL 23520000
         SPACE 1                                                    ABL 23530000
TSO010   TM    6(R15),QUOTED               QUOTED?                  ABL 23540000
         BNO   *+6                         NO, BRANCH               ABL 23550000
         BCTR  R14,0                       YES, POINT TO FIRST '    ABL 23560000
         STH   R14,2(,R1)                  OFFSET TO OPERANDS       ABL 23570000
         ST    R1,REVCBUF                  START OF COMMAND TEXT    ABL 23580000
         CLC   BLDL+4(5),=C'TIME '         TIME REQUEST?            ABL 23590000
         BNE   *+10                        NO, BRANCH               ABL 23600000
         MVC   BLDL+4(8),=C'IKJEFT25'      YES, USE IKJEFT25        ABL 23610000
         SPACE 1                                                    ABL 23620000
         BLDL  0,BLDL                                               ABL 23630000
         LTR   R15,R15             SUCCESSFUL BLDL?                 ABL 23640000
         BNZ   TSOMEM              NO, BRANCH                       ABL 23650000
         SPACE 1                                                    ABL 23660000
TSO020   STLINENO LINE=1,MODE=OFF  TURN OFF FULLSCREEN              ABL 23670000
         LA    R1,REVCBUF          CPPL START                       ABL 23680000
         LA    R6,BLDL+4           START OF BLDL INFORMATION        ABL 23690000
         LINK  DE=(6),SF=(E,LINKAREA)                               ABL 23700000
         STFSMODE ON,INITIAL=YES   TURN ON FULLSCREEN               ABL 23710000
         L     R1,REVECT                                            ABL 23720000
         MVC   ECTPCMD-ECT(8,R1),PCMD  RESET COMMAND NAME           ABL 23730000
         B     FILLSCR                                              ABL 23740000
         SPACE 1                                                    ABL 23750000
TSOMEM   MVC   MSG(MSG36L),MSG36   MEMBER NOT FOUND                 ABL 23760000
         L     R1,REVECT                                            ABL 23770000
         MVC   ECTPCMD-ECT(8,R1),PCMD  RESET COMMAND NAME           ABL 23780000
         B     FILLSCR                                              ABL 23790000
         SPACE                                                          23800000
************************************************************        ABL 23810000
*                                                          *        ABL 23820000
*         HELP                                             *        ABL 23830000
*                                                          *        ABL 23840000
************************************************************        ABL 23850000
         SPACE                                                          23860000
HELP     EQU   *                                                    ABL 23870000
         LA    R15,OPD1            FIRST PARAMETER                  ABL 23880000
         TM    6(R15),PRESENT      ANY OPERANDS?                    ABL 23890000
         BO    HELP010             YES, BRANCH                      ABL 23900000
         CLI   HELPFLG,C'R'        PREVIOUS HELP SCREEN?            ABL 23910000
         BE    HELP010             YES, PERFORM HELP CALL           ABL 23920000
         MVI   HELPFLG,C'H'        NEED HELP SCREEN                 ABL 23930000
         B     FILLSCR                                              ABL 23940000
         SPACE 1                                                    ABL 23950000
HELP010  LA    R1,CMDAREA          START OF HELP PARAMETERS         ABL 23960000
         LA    R0,63               LENGTH OF PARSE STRING           ABL 23970000
         SLL   R0,16               SHIFT AS REQUIRED                ABL 23980000
         STCM  R0,B'1111',0(R1)    SAVE FOR PARSE                   ABL 23990000
         TM    6(R15),QUOTED       QUOTED?                          ABL 24000000
         BO    ERRINV              YES, INVALID                     ABL 24010000
         LA    R15,OPD1            START OF FIRST ODL               ABL 24020000
         L     R14,0(,R15)         START OF PARAMETER               ABL 24030000
         LH    R15,4(,R15)         LENGTH OF PARAMETER              ABL 24040000
         L     R6,REVECT           ECT ADDRESS                      ABL 24050000
         OI    ECTSWS-ECT(R6),ECTNOPD      ASSUME NO OPERAND        ABL 24060000
         MVC   ECTSCMD-ECT(8,R6),=C'HELP    '  SUBCOMMAND IS HELP   ABL 24070000
         LTR   R15,R15                         NULL STRING?         ABL 24080000
         BZ    HELP040                         YES, BRANCH          ABL 24090000
         CH    R15,=H'8'                       LENGTH MORE THAN 8?  ABL 24100000
         BH    ERRINV                          YES, ERROR           ABL 24110000
         BCTR  R15,0                           MACHINE LENGTH       ABL 24120000
         MVC   BLDL+4(8),BLANKS                                     ABL 24130000
         MVC   BLDL+4(*-*),0(R14)              <<EXECUTED>>         ABL 24140000
         EX    R15,*-6                         COMMAND NAME         ABL 24150000
         LA    R15,OPD1            START OF FIRST ODL               ABL 24160000
         L     R14,0(,R15)         START OF FIRST PARAMETER         ABL 24170000
         LA    R14,0(,R14)         DROP TOP BYTE                    ABL 24180000
         SR    R14,R1              OFFSET TO SECOND PARAMETER       ABL 24190000
         SH    R14,=H'4'           LESS TEXT LENGTH FIELDS          ABL 24200000
         NI    ECTSWS-ECT(R6),X'FF'-ECTNOPD ASSUME OPERANDS         ABL 24210000
         TM    6(R15),PRESENT      ANY OPERANDS?                    ABL 24220000
         BO    HELP030             YES, BRANCH                      ABL 24230000
         LA    R14,63              NO, USE DEFAULT                  ABL 24240000
         OI    ECTSWS-ECT(R6),ECTNOPD      ASSUME NO OPERAND        ABL 24250000
         SPACE 1                                                    ABL 24260000
HELP030  STH   R14,2(,R1)                  OFFSET TO OPERANDS       ABL 24270000
HELP040  ST    R1,REVCBUF                  START OF COMMAND TEXT    ABL 24280000
         LA    R1,REVCBUF          CPPL START                       ABL 24290000
         LINK  EP=HELP                                              ABL 24300000
         L     R1,REVECT                                            ABL 24310000
         MVC   ECTPCMD-ECT(8,R1),PCMD  RESET COMMAND NAME           ABL 24320000
         B     FILLSCR                                              ABL 24330000
         SPACE                                                          24340000
************************************************************            24350000
*                                                          *            24360000
*         RIGHT                                            *            24370000
*                                                          *            24380000
************************************************************            24390000
         SPACE                                                          24400000
RIGHT    EQU   *                                                        24410000
         LA    R15,OPD1            GET FIRST OPERAND ENTRY              24420000
         TM    6(R15),PRESENT      ARE THERE ANY OPERANDS               24430000
         BZ    RIGHTDEF            NO, USE DEFAULT                      24440000
         TM    6(R15),QUOTED       IS OPERAND QUOTED                    24450000
         BO    ERRINV              YES, INVALID                         24460000
         LH    R1,4(,R15)          GET LENGTH                           24470000
         LTR   R1,R1               IS IT NULL STRING                    24480000
         BZ    ERRINV              YES, ERROR                           24490000
         CH    R1,=H'7'            IS LENGTH MORE THAN 7                24500000
         BH    ERRINV              YES, ERROR                           24510000
         L     R14,0(,R15)         GET ADDRESS OF DATA                  24520000
         BCTR  R1,0                                                     24530000
         B     *+10                                                     24540000
         TRT   0(0,R14),NUMERIC                                         24550000
         EX    R1,*-6              IS IT NUMERIC                        24560000
         BNZ   ERRINV              NO, ERROR                            24570000
         B     *+10                                                     24580000
         PACK  DOUBLE(8),0(0,R14)                                       24590000
         EX    R1,*-6                                                   24600000
         CVB   R1,DOUBLE                                                24610000
         LTR   R15,R1                                                   24620000
         BZ    PROMPT                                                   24630000
         B     RIGHTR15                                                 24640000
RIGHTALL MVI   OFFSET,X'7F'                                         ABL 24650000
         MVI   OFFSET+1,X'AF'      VALUE IS X'7FFF'                 ABL 24660000
RIGHTDEF LA    R15,80                                                   24670000
         TM    MODE,MODEX                                               24680000
         BZ    *+8                                                      24690000
         LA    R15,40                                                   24700000
RIGHTR15 LH    R0,OFFSET                                                24710000
         AR    R0,R15                                                   24720000
         LH    R1,RECSIZE                                               24730000
         CH    R1,=H'1000'                                              24740000
         BNH   *+8                                                      24750000
         LH    R1,=H'1000'                                              24760000
*        SR    R1,R15              GET RECSIZE-80                   ABL 24770000
*        BNM   *+6                 IF RECSIZE IS LESS THAN 80       ABL 24780000
*        SLR   R1,R1                  THEN RECSIZE-80 IS ZERO       ABL 24790000
         CR    R0,R1               IF OFFSET+80 GT RECSIZE-80           24800000
         BNH   *+6                    THEN                              24810000
         LR    R0,R1                  USE RECSIZE-80                    24820000
         STH   R0,OFFSET                                                24830000
         B     FILLSCR                                                  24840000
         SPACE                                                          24850000
************************************************************            24860000
*                                                          *            24870000
*         LEFT                                             *            24880000
*                                                          *            24890000
************************************************************            24900000
         SPACE                                                          24910000
LEFT     EQU   *                                                        24920000
         LA    R15,OPD1            GET FIRST OPERAND ENTRY              24930000
         TM    6(R15),PRESENT      ARE THERE ANY OPERANDS               24940000
         BZ    LEFTDEF             NO, USE DEFAULT                      24950000
         TM    6(R15),QUOTED       IS OPERAND QUOTED                    24960000
         BO    ERRINV              YES, INVALID                         24970000
         LH    R1,4(,R15)          GET LENGTH                           24980000
         LTR   R1,R1               IS IT NULL STRING                    24990000
         BZ    ERRINV              YES, ERROR                           25000000
         CH    R1,=H'7'            IS LENGTH MORE THAN 7                25010000
         BH    ERRINV              YES, ERROR                           25020000
         L     R14,0(,R15)         GET ADDRESS OF DATA                  25030000
         BCTR  R1,0                                                     25040000
         B     *+10                                                     25050000
         TRT   0(0,R14),NUMERIC                                         25060000
         EX    R1,*-6              IS IT NUMERIC                        25070000
         BNZ   ERRINV              NO, ERROR                            25080000
         B     *+10                                                     25090000
         PACK  DOUBLE(8),0(0,R14)                                       25100000
         EX    R1,*-6                                                   25110000
         CVB   R1,DOUBLE                                                25120000
         LTR   R15,R1                                                   25130000
         BZ    PROMPT                                                   25140000
         B     LEFTR15                                                  25150000
LEFTDEF  LA    R15,80                                                   25160000
         TM    MODE,MODEX                                               25170000
         BZ    *+8                                                      25180000
         LA    R15,40                                                   25190000
LEFTR15  LH    R0,OFFSET                                                25200000
         SR    R0,R15              REDUCE OFFSET BY 80                  25210000
         BNM   *+6                 IF RESULT IS NEGATIVE                25220000
LEFTALL  SLR   R0,R0                  THEN MAKE IT ZERO             ABL 25230000
         STH   R0,OFFSET                                                25240000
         B     FILLSCR                                                  25250000
         SPACE                                                          25260000
************************************************************            25270000
*                                                          *            25280000
*         DOWN                                             *            25290000
*                                                          *            25300000
************************************************************            25310000
         SPACE                                                          25320000
DOWN     EQU   *                                                        25330000
         L     R0,SCROLL                                                25340000
         LA    R15,OPD1            GET FIRST OPERAND ENTRY              25350000
         TM    6(R15),PRESENT      ARE THERE ANY OPERANDS               25360000
         BZ    DOWNRGE             NO, USE RANGE                        25370000
         TM    6(R15),QUOTED       IS OPERAND QUOTED                    25380000
         BO    ERRINV              YES, INVALID                         25390000
         LH    R1,4(,R15)          GET LENGTH                           25400000
         LTR   R1,R1               IS IT NULL STRING                    25410000
         BZ    ERRINV              YES, ERROR                           25420000
         CH    R1,=H'7'            IS LENGTH MORE THAN 7                25430000
         BH    ERRINV              YES, ERROR                           25440000
         L     R14,0(,R15)         GET ADDRESS OF DATA                  25450000
         BCTR  R1,0                                                     25460000
         B     *+10                                                     25470000
         TRT   0(0,R14),NUMERIC                                         25480000
         EX    R1,*-6              IS IT NUMERIC                        25490000
         BNZ   ERRINV              NO, ERROR                            25500000
         B     *+10                                                     25510000
         PACK  DOUBLE(8),0(0,R14)                                       25520000
         EX    R1,*-6                                                   25530000
         CVB   R1,DOUBLE                                                25540000
         LTR   R0,R1                                                    25550000
         BZ    PROMPT                                                   25560000
DOWNRGE  ST    R0,DOWNAMT                                               25570000
         BAL   R14,DOWNER                                               25580000
         LTR   R15,R15             ACTION TAKEN                         25590000
         BNZ   PROMPT              NO, BRANCH                           25600000
         B     FILLSCR             YES, BRANCH                          25610000
DOWNER   EQU   *                                                        25620000
*                                                                       25630000
*              SEE IF EOF IS ON SCREEN AND WOULD BE FORCED OFF          25640000
*                                                                       25650000
         LA    R15,4                                                    25660000
         L     R7,HOLDEND          POINT TO ENTRY FOR LAST RECORD       25670000
         TM    DIRLEN(R7),X'80'    IS EOF ON SCREEN                     25680000
         BZ    DOWNNEOF            NO, DOWN IS POSSIBLE                 25690000
         L     R7,HOLDTOP                                               25700000
         L     R0,DOWNAMT          GET DOWN AMOUNT                      25710000
         CH    R0,PAGESIZE         OR SCREEN SIZE IF SMALLER            25720000
         BNH   *+8                                                      25730000
         LH    R0,PAGESIZE                                              25740000
DOWNTEST TM    DIRLEN(R7),X'80'    IS THIS END OF DATA                  25750000
         BO    0(R14)              YES, CANT GO DOWN                    25760000
         L     R7,DIRNXT(,R7)      GET ENTRY FOR NEXT RECORD            25770000
         BCT   R0,DOWNTEST                                              25780000
         L     R7,HOLDEND          POINT TO ENTRY FOR LAST RECORD       25790000
DOWNNEOF ST    R14,DOWNR                                                25800000
         L     R8,DOWNAMT          GET NUMBER OF LINES TO GO DOWN       25810000
DOWNLOOP L     R15,HOLDTOP         POINT TO TOP ENTRY                   25820000
         L     R15,DIRNXT(,R15)    GET ADDRESS OF ENTRY AFTER TOP       25830000
         ST    R15,HOLDTOP         MAKE IT NEW TOP                      25840000
         TM    DIRLEN(R7),X'80'    IS EOF ON SCREEN                     25850000
         BO    DOWNNXT             YES, BYPASS GET                      25860000
         L     R7,DIRNXT(,R7)      POINT TO NEXT ENTRY                  25870000
         BAL   R14,GET                                                  25880000
         LA    R0,1                COMPUTE                              25890000
         A     R0,COUNT             THE                                 25900000
         ST    R0,COUNT              RECORD NUMBER                      25910000
         ST    R0,DIRNUM(,R7)      STORE THE RECORD NUMBER              25920000
         ST    R1,DIRLEN(,R7)      STORE LENGTH                         25930000
         LTR   R1,R1               END OF FILE                          25940000
         BM    DOWNX               YES BRANCH                           25950000
         C     R1,SAVLRECL         IS RECORD LONGER THAN MAX            25960000
         BNH   *+8                 NO, SKIP NEXT INSTR                  25970000
         L     R1,SAVLRECL         YES, TRUNCATE TO MAX                 25980000
         LR    R0,R2               ADDRESS TO MOVE FROM                 25990000
         L     R14,DIRREC(,R7)     ADDRESS TO MOVE TO                   26000000
         TM    TSTRECFM,X'40'      RECFM V                              26010000
         BZ    DOWNNOTV            NO                                   26020000
         SH    R14,=H'4'           YES, MOVE TO PTR MINUS 4             26030000
         L     R15,DIRLEN(,R7)                                          26040000
         SH    R15,=H'4'                                                26050000
         ST    R15,DIRLEN(,R7)                                          26060000
DOWNNOTV L     R15,SAVLRECL        LENGTH OF RECEIVING FIELD            26070000
         O     R1,=A(X'40000000')  PAD WITH BLANKS                      26080000
         MVCL  R14,R0                                                   26090000
DOWNNXT  BCT   R8,DOWNLOOP                                              26100000
DOWNX    ST    R7,HOLDEND          NEW END POINTER                      26110000
         SLR   R15,R15                                                  26120000
         L     R14,DOWNR                                                26130000
         BR    R14                                                      26140000
         SPACE                                                          26150000
************************************************************            26160000
*                                                          *            26170000
*         UP                                               *            26180000
*                                                          *            26190000
************************************************************            26200000
         SPACE                                                          26210000
UP       EQU   *                                                        26220000
         L     R0,SCROLL                                                26230000
         LA    R15,OPD1            GET FIRST OPERAND ENTRY              26240000
         TM    6(R15),PRESENT      ARE THERE ANY OPERANDS               26250000
         BZ    UPRANGE             NO, USE RANGE                        26260000
         TM    6(R15),QUOTED       IS OPERAND QUOTED                    26270000
         BO    ERRINV              YES, INVALID                         26280000
         LH    R1,4(,R15)          GET LENGTH                           26290000
         LTR   R1,R1               IS IT NULL STRING                    26300000
         BZ    ERRINV              YES, ERROR                           26310000
         CH    R1,=H'7'            IS LENGTH MORE THAN 7                26320000
         BH    ERRINV              YES, ERROR                           26330000
         L     R14,0(,R15)         GET ADDRESS OF DATA                  26340000
         BCTR  R1,0                                                     26350000
         B     *+10                                                     26360000
         TRT   0(0,R14),NUMERIC                                         26370000
         EX    R1,*-6              IS IT NUMERIC                        26380000
         BNZ   ERRINV              NO, ERROR                            26390000
         B     *+10                                                     26400000
         PACK  DOUBLE(8),0(0,R14)                                       26410000
         EX    R1,*-6                                                   26420000
         CVB   R1,DOUBLE                                                26430000
         LTR   R0,R1                                                    26440000
         BZ    PROMPT                                                   26450000
UPRANGE  ST    R0,DOWNAMT                                               26460000
         L     R7,HOLDTOP                                               26470000
         CLC   DIRNUM(4,R7),=F'1'  ARE WE AT TOP ALREADY                26480000
         BNH   PROMPT              YES, BRANCH                          26490000
         TM    DIRLEN(R7),X'80'    IS FIRST LINE EOF                    26500000
         BZ    UPTOP               NO, BRANCH                           26510000
         NC    COUNT,COUNT         IS DATA SET EMPTY                    26520000
         BZ    PROMPT              YES, BRANCH                          26530000
UPTOP    EQU   *                                                        26540000
         L     R0,DIRNUM(,R7)      GET RECORD NUMBER OF TOP LINE        26550000
         S     R0,DOWNAMT          GET RECORD NUMBER TO GO BACK TO      26560000
         BP    *+8                 IF NEGATIVE                          26570000
         LA    R0,1                   THEN MAKE IT 1                    26580000
         B     LISTAT                                                   26590000
*                                                                       26600000
         L     R1,CHKPTTOP                                              26610000
         MVC   TTR,4(R1)           TTR OF FIRST BLOCK                   26620000
         MVC   TTRZ(3),TTR         TTR OF FIRST BLOCK                   26630000
         POINT (R4),TTRZ                                                26640000
         XC    COUNT,COUNT                                              26650000
         XC    DEBLOCKS(12),DEBLOCKS                                    26660000
         L     R14,SPANPTR                                              26670000
         XC    0(2,R14),0(R14)     RESET SPANNED BUFFER LENGTH          26680000
         L     R0,DIRNUM(,R7)      GET RECORD NUMBER OF TOP LINE        26690000
         S     R0,SCROLL           GET RECORD NUMBER TO GO BACK TO      26700000
         BP    *+8                 IF NEGATIVE                          26710000
         LA    R0,1                   THEN MAKE IT 1                    26720000
         AH    R0,PAGESIZE         GET RECORD NUMBER TO READ UP TO      26730000
         BCTR  R0,0                                                     26740000
         ST    R0,AIMFOR           SAVE FOR DOWNTO                      26750000
         BAL   R14,FILLHOLD        READ FIRST 20 RECORDS                26760000
         L     R7,HOLDEND          GET ENTRY FOR LAST RECORD            26770000
         TM    DIRLEN(R7),X'80'    IS EOF ON SCREEN                     26780000
         BO    FILLSCR             YES, WE ARE FINISHED                 26790000
         L     R0,AIMFOR           GET NUMBER WERE AIMING FOR           26800000
         S     R0,DIRNUM(,R7)      HAVE WE REACHED IT                   26810000
         BNP   FILLSCR             YES, WE ARE FINISHED                 26820000
         ST    R0,DOWNAMT          NO, NEED TO GO DOWN                  26830000
         BAL   R14,DOWNER                                               26840000
         B     FILLSCR                                                  26850000
         SPACE                                                          26860000
************************************************************            26870000
*                                                          *            26880000
*         TOP                                              *            26890000
*                                                          *            26900000
************************************************************            26910000
         SPACE                                                          26920000
TOP      EQU   *                                                        26930000
         L     R1,CHKPTTOP                                              26940000
         MVC   TTR,4(R1)           TTR OF FIRST BLOCK                   26950000
         MVC   TTRZ(3),TTR         TTR OF FIRST BLOCK                   26960000
         POINT (R4),TTRZ                                                26970000
         XC    COUNT,COUNT                                              26980000
         XC    DEBLOCKS(12),DEBLOCKS                                    26990000
         L     R14,SPANPTR                                              27000000
         SLR   R0,R0                                                    27010000
         STH   R0,0(,R14)          RESET SPAN BUFFER LENGTH             27020000
         ST    R0,FNDNUM           RESET LAST-FOUND NUMBER              27030000
         STH   R0,FNDOFF           RESET LAST-FOUND OFFSET              27040000
         BAL   R14,FILLHOLD                                             27050000
         B     FILLSCR                                                  27060000
         SPACE                                                          27070000
************************************************************            27080000
*                                                          *            27090000
*         BOTTOM                                           *            27100000
*                                                          *            27110000
************************************************************            27120000
         SPACE                                                          27130000
BOTTOM   EQU   *                                                        27140000
         L     R0,BOTNINES                                              27150000
         B     LISTAT                                                   27160000
BOTRET   L     R0,SCROLL                                                27170000
         BCTR  R0,0                MOVE UP LESS 1                   ABL 27180000
         B     UPRANGE                                                  27190000
         SPACE                                                          27200000
*        MVC   DOWNAMT,=F'99999999'                                     27210000
*        BAL   R14,DOWNER                                               27220000
*        B     FILLSCR                                                  27230000
         SPACE                                                          27240000
************************************************************            27250000
*                                                          *            27260000
*         LIST                                             *            27270000
*                                                          *            27280000
************************************************************            27290000
         SPACE                                                          27300000
LIST     EQU   *                                                        27310000
         LA    R15,OPD1            GET FIRST OPERAND ENTRY              27320000
         TM    6(R15),PRESENT      ARE THERE ANY OPERANDS               27330000
         BZ    ERRMISS             NO, MISSING OPERAND                  27340000
         TM    6(R15),QUOTED       IS OPERAND QUOTED                    27350000
         BO    ERRINV              YES, INVALID                         27360000
         LH    R1,4(,R15)          GET LENGTH                           27370000
         LTR   R1,R1               IS IT NULL STRING                    27380000
         BZ    ERRINV              YES, ERROR                           27390000
         CH    R1,=H'7'            IS LENGTH MORE THAN 7                27400000
         BH    ERRINV              YES, ERROR                           27410000
         L     R14,0(,R15)         GET ADDRESS OF DATA                  27420000
         BCTR  R1,0                                                     27430000
         B     *+10                                                     27440000
         TRT   0(0,R14),NUMERIC                                         27450000
         EX    R1,*-6              IS IT NUMERIC                        27460000
         BNZ   ERRINV              NO, ERROR                            27470000
         B     *+10                                                     27480000
         PACK  DOUBLE(8),0(0,R14)                                       27490000
         EX    R1,*-6                                                   27500000
         CVB   R1,DOUBLE                                                27510000
         LTR   R0,R1                                                    27520000
         BZ    TOP                                                      27530000
*                                                                       27540000
LISTAT   ST    R0,LISTNUM                                               27550000
         AH    R0,PAGESIZE         GET RECORD NUMBER TO READ UP TO      27560000
         BCTR  R0,0                                                     27570000
         ST    R0,AIMFOR           SAVE FOR DOWNTO                      27580000
*                                                                       27590000
         LA    R15,20              LENGTH OF CHKPT ENTRY                27600000
         L     R1,CHKPTBOT                                              27610000
         TM    0(R1),X'80'         IS IT EOF                            27620000
         BZ    LISTNB                                                   27630000
         C     R1,CHKPTTOP         IS DATA SET EMPTY                    27640000
         BE    PROMPT              YES                                  27650000
         SR    R1,R15              NO, BACK UP ONE ENTRY                27660000
LISTNB   L     R0,LISTNUM                                               27670000
LISTCK   C     R0,0(,R1)           DOES THIS BLOCK PRECEDE OUR RECORD   27680000
         BH    LISTPNT             YES, GO POINT TO IT                  27690000
         SR    R1,R15              NO, BACK UP ONE BLOCK ENTRY          27700000
         B     LISTCK                                                   27710000
LISTPNT  MVC   COUNT,0(R1)                                              27720000
         MVC   DEBLOCKS(12),8(R1)                                       27730000
         MVC   TTR,4(R1)           TTR OF FIRST BLOCK                   27740000
         MVC   TTRZ(3),TTR                                              27750000
         POINT (R4),TTRZ                                                27760000
         L     R14,SPANPTR                                              27770000
         SLR   R0,R0                                                    27780000
         STH   R0,0(,R14)          RESET SPAN BUFFER LENGTH             27790000
         CLC   MSG(8),MSG31        FOUND STRING MESSAGE?            ABL 27800000
         BE    LISTFIN             YES, BRANCH                      ABL 27810000
         ST    R0,FNDNUM           RESET LAST-FOUND NUMBER              27820000
         STH   R0,FNDOFF           RESET LAST-FOUND OFFSET              27830000
*                                                                       27840000
LISTFIN  BAL   R14,FILLHOLD        READ NEXT 20 RECORDS             ABL 27850000
*                                                                       27860000
LISTFINE L     R7,HOLDTOP          GET ENTRY FOR TOP OF HOLD AREA       27870000
         TM    DIRLEN(R7),X'80'    IS EOF ON TOP OF SCREEN              27880000
         BO    LISTRDY             YES, WE ARE FINISHED                 27890000
         CLC   LISTNUM,DIRNUM(R7)  IS REQUESTED NUMBER AT TOP           27900000
         BNH   FILLSCR             YES, BRANCH                          27910000
         MVC   DOWNAMT,=F'1'       DOWN 1                               27920000
         BAL   R14,DOWNER                                               27930000
         B     LISTFINE                                                 27940000
LISTRDY  CLC   LISTNUM,BOTNINES    WAS THIS A LIST 99999999             27950000
         BE    BOTRET              YES, BRANCH                          27960000
         B     FILLSCR                                                  27970000
ERRMISS  MVC   MSG(MSG35L),MSG35                                        27980000
         B     PROMPT                                                   27990000
         SPACE                                                          28000000
************************************************************            28010000
*                                                          *            28020000
*         FIND                                             *            28030000
*                                                          *            28040000
************************************************************            28050000
         SPACE                                                          28060000
FIND     EQU   *                                                        28070000
         MVI   FINDSW,0                                                 28080000
         LA    R15,OPD1            GET FIRST OPERAND ENTRY              28090000
         TM    6(R15),PRESENT      ARE THERE ANY OPERANDS               28100000
         BZ    FINDSAME            NO, USE PREVIOUS STRING              28110000
         XC    STRING,STRING       ERASE OLD STRING                     28120000
         SLR   R0,R0                                                    28130000
         ST    R0,FNDNUM           RESET LAST-FOUND NUMBER              28140000
         STH   R0,FNDOFF           RESET LAST-FOUND OFFSET              28150000
         STH   R0,FINDCOL          RESET COLUMN                         28160000
         LH    R1,4(,R15)          GET LENGTH                           28170000
         LTR   R1,R1               IS IT NULL STRING                    28180000
         BZ    FINDNULL            YES, ERROR                           28190000
         L     R14,0(,R15)         GET ADDRESS OF DATA                  28200000
         BCTR  R1,0                                                     28210000
         STH   R1,STRINGL          SAVE LENGTH CODE                     28220000
         B     *+10                                                     28230000
         MVC   STRING(0),0(R14)                                         28240000
         EX    R1,*-6                                                   28250000
         LA    R15,OPD2                                                 28260000
         TM    6(R15),PRESENT      IS THERE A SECOND OPERAND            28270000
         BZ    FINDSAME            NO, LEAVE FINDCOL NULL               28280000
         TM    6(R15),QUOTED       IS OPERAND QUOTED                    28290000
         BO    ERRINV              YES, INVALID                         28300000
         LH    R1,4(,R15)          GET LENGTH                           28310000
         LTR   R1,R1               IS IT NULL STRING                    28320000
         BZ    ERRINV              YES, ERROR                           28330000
         CH    R1,=H'3'            IS LENGTH MORE THAN 3                28340000
         BH    ERRINV              YES, ERROR                           28350000
         L     R14,0(,R15)         GET ADDRESS OF DATA                  28360000
         BCTR  R1,0                                                     28370000
         B     *+10                                                     28380000
         TRT   0(0,R14),NUMERIC                                         28390000
         EX    R1,*-6              IS IT NUMERIC                        28400000
         BNZ   ERRINV              NO, ERROR                            28410000
         B     *+10                                                     28420000
         PACK  DOUBLE(8),0(0,R14)                                       28430000
         EX    R1,*-6                                                   28440000
         CVB   R1,DOUBLE                                                28450000
         LTR   R1,R1               IS IT ZERO                           28460000
         BZ    ERRINV              YES, ERROR                           28470000
         C     R1,SAVLRECL         IS IT GREATER THAN LRECL             28480000
         BH    ERRINV              YES, ERROR                           28490000
         STH   R1,FINDCOL                                               28500000
FINDSAME EQU   *                                                        28510000
         NC    STRING,STRING       HAS A STRING BEEN ENTERED            28520000
         BZ    FINDNULL            NO, ERROR                            28530000
FINDSAM2 LH    R6,FINDCOL                                  CBT-AXC      28540000
         L     R7,HOLDTOP          GET INFO FOR FIRST HELD RECORD       28550000
         L     R0,DIRNUM(,R7)      GET RECORD NUMBER OF FIRST HELD      28560000
         L     R1,FNDNUM           GET RECORD NUMBER WHERE LAST FOUND   28570000
         LTR   R1,R1               HAS IT BEEN FOUND                    28580000
         BNP   FINDSCR             NO, START WITH HELD RECORDS          28590000
         CR    R0,R1               ARE WE PAST LAST FOUND REC           28600000
         BH    FINDSCR             YES, START WITH HELD RECORDS         28610000
         L     R14,HOLDEND                                              28620000
         L     R15,DIRNUM(,R14)    GET RECORD NUMBER OF LAST HELD       28630000
         CR    R1,R15              IS LAST FOUND REC ON SCREEN          28640000
         BH    FINDDOWN            NO, BRANCH                           28650000
FINDPREV C     R1,DIRNUM(,R7)      IS THIS RECORD WHERE LAST FOUND      28660000
         BE    FINDPCOL            YES, BRANCH                          28670000
         C     R7,HOLDEND          IS THIS LAST RECORD ON SCREEN        28680000
         BE    FINDDOWN            SHOULD NOT HAPPEN                    28690000
         L     R7,DIRNXT(,R7)      POINT TO NEXT RECORD                 28700000
         B     FINDPREV                                                 28710000
FINDPCOL L     R15,DIRREC(,R7)     POINT TO RECORD                      28720000
         LTR   R6,R6               WAS A COLUMN SPECIFIED               28730000
         BZ    FINDPOFF            NO, LOOK AT SAME RECORD              28740000
         C     R7,HOLDEND          IS THIS LAST RECORD ON SCREEN        28750000
         BE    FINDDOWN            YES, BRANCH                          28760000
         L     R7,DIRNXT(,R7)      NO, POINT TO NEXT RECORD             28770000
         B     FINDSCR             GO EXAMINE RECORD                    28780000
FINDPOFF AH    R15,FNDOFF          POINT TO LAST FOUND STRING           28790000
         LA    R15,1(,R15)         POINT PAST LAST FOUND STRING         28800000
         B     FINDSTR                                                  28810000
FINDCLC  CLC   0(0,R15),STRING     (EXECUTED)                           28820000
FINDSCR  TM    DIRLEN(R7),X'80'    ARE WE AT EOF                        28830000
         BO    FINDBOT             YES, BRANCH                          28840000
         L     R15,DIRREC(,R7)     POINT TO FIRST BYTE TO EXAMINE       28850000
         LTR   R6,R6               WAS A COLUMN SPECIFIED               28860000
         BZ    FINDSTR             NO, BRANCH                           28870000
         AR    R15,R6              YES, POINT TO COLUMN PLUS 1          28880000
         BCTR  R15,0               POINT TO COLUMN                      28890000
         LH    R14,STRINGL                                              28900000
         EX    R14,FINDCLC                                              28910000
         BE    FOUND                                                    28920000
         B     FINDNEXT                                                 28930000
FINDSTR  L     R0,DIRLEN(,R7)      GET LENGTH OF RECORD                 28940000
         C     R0,SAVLRECL         IS RECORD TRUNCATED                  28950000
         BNH   *+8                 NO, SKIP NEXT INSTR                  28960000
         L     R0,SAVLRECL         YES, USE TRUNCATED LENGTH            28970000
         L     R1,DIRREC(,R7)      GET ADDRESS OF RECORD                28980000
         AR    R0,R1               POINT PAST LAST BYTE OF RECORD       28990000
         LH    R14,STRINGL         GET LENGTH CODE OF STRING            29000000
         AR    R15,R14             POINT TO LAST BYTE TO BE COMPARED    29010000
         SR    R0,R15              GET NUMBER OF COMPARISONS            29020000
         BNP   FINDNEXT            STRING TOO LONG FOR REMAINING TEXT   29030000
         SR    R15,R14             PUT STRING ADDRESS BACK              29040000
FINDCOMP EX    R14,FINDCLC         COMPARE STRING TO DATA               29050000
         BE    FOUND                                                    29060000
         LA    R15,1(,R15)         INCREMENT DATA POINTER               29070000
         BCT   R0,FINDCOMP         GO COMPARE AGAIN                     29080000
FINDNEXT C     R7,HOLDEND          WAS THAT LAST HELD RECORD            29090000
         BE    FINDDOWN            YES, BRANCH                          29100000
         L     R7,DIRNXT(,R7)      POINT TO NEXT RECORD                 29110000
         B     FINDSCR             GO PROCESS NEXT RECORD               29120000
FINDBOT  EQU   *                                                        29130000
         MVC   MSG(MSG32L),MSG32                                        29140000
         B     FILLSCR                                                  29150000
FINDDOWN EQU   *                                                        29160000
         LA    R1,1                                                     29170000
         ST    R1,DOWNAMT                                               29180000
         BAL   R14,DOWNER                                               29190000
         L     R7,HOLDEND                                               29200000
         OI    FINDSW,1                                                 29210000
         B     FINDSCR                                                  29220000
FINDNULL MVC   MSG(MSG33L),MSG33                                        29230000
         B     PROMPT                                                   29240000
ERRINV MVC     MSG(MSG34L),MSG34                                        29250000
         B     PROMPT                                                   29260000
FOUND    MVC   MSG(MSG31L),MSG31                                        29270000
         MVC   FNDNUM,DIRNUM(R7)   SAVE RECORD NUMBER                   29280000
         S     R15,DIRREC(,R7)     GET OFFSET TO FOUND LOCATION         29290000
         STH   R15,FNDOFF          SAVE OFFSET                          29300000
         LA    R15,1(,R15)         MAKE IT COLUMN NUMBER                29310000
         CVD   R15,DOUBLE                                               29320000
         OI    DOUBLE+7,X'0F'                                           29330000
         LA    R15,MSG+26                                               29340000
         UNPK  0(5,R15),DOUBLE+5(3)                                     29350000
         L     R15,FNDNUM                                               29360000
         CVD   R15,DOUBLE                                               29370000
         OI    DOUBLE+7,X'0F'                                           29380000
         LA    R15,MSG+16                                               29390000
         UNPK  0(5,R15),DOUBLE+5(3)                                     29400000
         L     R1,DIRNUM(,R7)                                       ABL 29410000
         LA    R0,0(,R1)                                            ABL 29420000
         TM    FINDSW,1                                             ABL 29430000
         BZ    LISTAT                                               ABL 29440000
         LH    R6,PAGESIZE                                              29450000
         SH    R6,=H'2'                                                 29460000
FINDLN2  LA    R1,1                                                     29470000
         ST    R1,DOWNAMT                                               29480000
         BAL   R14,DOWNER                                               29490000
         BCT   R6,FINDLN2                                               29500000
         B     FILLSCR                                                  29510000
         SPACE                                                          29520000
************************************************************            29530000
*                                                          *            29540000
*         FINDSMF   (FIND SMF RECORD TYPE)                 *            29550000
*                                                          *            29560000
************************************************************            29570000
         SPACE                                                          29580000
FINDSMF EQU    *                                                        29590000
         MVI   FINDSW,0                                                 29600000
         LA    R15,OPD1            GET FIRST OPERAND ENTRY              29610000
         TM    6(R15),PRESENT      ARE THERE ANY OPERANDS               29620000
         BZ    FINDSAM2            NO, USE PREVIOUS STRING CBT-AXC      29630000
         XC    STRING,STRING       ERASE OLD STRING                     29640000
         SLR   R0,R0                                                    29650000
         ST    R0,FNDNUM           RESET LAST-FOUND NUMBER              29660000
         STH   R0,FNDOFF           RESET LAST-FOUND OFFSET              29670000
         STH   R0,FINDCOL          RESET COLUMN                         29680000
         TM    6(R15),QUOTED       IS OPERAND QUOTED                    29690000
         BO    ERRINV              YES, INVALID                         29700000
         LH    R1,4(,R15)          GET LENGTH                           29710000
         LTR   R1,R1               IS IT NULL STRING                    29720000
         BZ    FINDNULL            YES, ERROR                           29730000
         L     R14,0(,R15)         GET ADDRESS OF DATA                  29740000
         BCTR  R1,0                                                     29750000
         B     *+10                                                     29760000
         TRT   0(0,R14),NUMERIC                                         29770000
         EX    R1,*-6              IS IT NUMERIC                        29780000
         BNZ   ERRINV              NO, ERROR                            29790000
         B     *+10                                                     29800000
         PACK  DOUBLE(8),0(0,R14)                                       29810000
         EX    R1,*-6                                                   29820000
         CVB   R1,DOUBLE                                                29830000
         CH    R1,=H'255'          IS IT GREATER THAN 255               29840000
         BH    ERRINV              YES, ERROR                           29850000
         STC   R1,STRING           STORE BINARY VALUE 0 TO 255          29860000
         LA    R1,2                COLUMN 2                             29870000
         STH   R1,FINDCOL                                               29880000
         SR    R1,R1               LENGTH MINUS 1 IS ZERO               29890000
         STH   R1,STRINGL          SAVE LENGTH CODE                     29900000
         B     FINDSAM2                                    CBT-AXC      29910000
         SPACE                                                          29920000
************************************************************            29930000
*                                                          *            29940000
*         HEX                                              *            29950000
*                                                          *            29960000
************************************************************            29970000
         SPACE                                                          29980000
HEXMODE  EQU   *                                                        29990000
         LA    R15,OPD1            GET FIRST OPERAND ENTRY              30000000
         TM    6(R15),PRESENT      ARE THERE ANY OPERANDS               30010000
         BZ    HEXFLIP             NO, FLIP FLOP                        30020000
         L     R14,0(,R15)         POINT TO OPERAND                     30030000
         CLI   5(R15),2            IS LENGTH 2                          30040000
         BNE   HEXOFF              NO, TRY OFF                          30050000
         CLC   0(2,R14),=C'ON'     YES, IS IT 'ON'                      30060000
         BNE   ERRKW               NO, INVALID OPERAND                  30070000
         OI    MODE,MODEX                                               30080000
         B     FILLSCR                                                  30090000
HEXOFF   CLI   5(R15),3            IS LENGTH 3                          30100000
         BNE   ERRKW                                                    30110000
         CLC   0(3,R14),=C'OFF'                                         30120000
         BNE   ERRKW                                                    30130000
         NI    MODE,255-MODEX                                           30140000
         B     FILLSCR                                                  30150000
HEXFLIP  XI    MODE,MODEX          FLIP FLOP THE SWITCH                 30160000
         B     FILLSCR                                                  30170000
ERRKW    MVC   MSG(MSG34L),MSG34                                        30180000
         B     PROMPT                                                   30190000
         SPACE                                                          30200000
************************************************************            30210000
*                                                          *            30220000
*         PFK SET                                          *            30230000
*                                                          *            30240000
************************************************************            30250000
         SPACE                                                          30260000
PFKSET   EQU   *                                                        30270000
         LA    R15,3(,R1)          POINT TO NN AFTER PFK                30280000
         TRT   0(1,R15),NUMERIC    PFKN                                 30290000
         BNZ   PFKSET9             ERROR, N NOT NUMERIC                 30300000
         PACK  DOUBLE,0(1,R15)                                          30310000
         CLI   1(R15),C' '         SINGLE DIGIT                         30320000
         BE    PFKSET1             YES, BRANCH                          30330000
         TRT   1(1,R15),NUMERIC                                         30340000
         BNZ   PFKSET9             SECOND DIGIT NOT NUMERIC             30350000
         CLI   2(R15),C' '                                              30360000
         BNE   PFKSET9             MORE THAN 2 DIGITS                   30370000
         PACK  DOUBLE,0(2,R15)                                          30380000
PFKSET1  CVB   R1,DOUBLE           GET VALUE OF N OR NN                 30390000
         CH    R1,=H'1'                                                 30400000
         BL    PFKSET9             ERROR, LESS THAN 1                   30410000
         CH    R1,=H'12'                                                30420000
         BH    PFKSET9             ERROR, GREATER THAN 12               30430000
         BCTR  R1,0                CHANGE 1-12 TO 0-11                  30440000
         LA    R0,PFKTABL          LENGTH OF EACH PFK ENTRY             30450000
         MR    R0,R0               COMPUTE OFFSET INTO TABLE            30460000
         LA    R15,PFKTAB-4095                                          30470000
         LA    R15,4095(R1,R15)    POINT TO TABLE ENTRY                 30480000
         LA    R15,4095(,R15)      POINT TO TABLE ENTRY                 30490000
         LA    R1,OPD1             GET FIRST OPERAND ENTRY              30500000
         TM    6(R1),PRESENT       ARE THERE ANY OPERANDS               30510000
         BZ    PFKSETF             NO, BLANK IT                         30520000
         CLI   5(R1),0             IS IT NULL LENGTH                    30530000
         BE    PFKSETF             YES, BLANK IT                        30540000
         L     R1,0(,R1)           POINT TO OPERAND                     30550000
         CLC   0(3,R1),=C'PFK'     IS OPERAND ANOTHER PFK COMMAND       30560000
         BE    PFKSET9             YES, ERROR                           30570000
         LA    R0,CMDAREA+62       POINT TO END OF COMMAND AREA         30580000
         SR    R0,R1               GET LENGTH CODE OF COMMAND           30590000
         BM    PFKSET9                                                  30600000
         LR    R14,R0                                                   30610000
         MVI   1(R15),C' '                                              30620000
         MVC   2(PFKTABL-2,R15),1(R15)                                  30630000
         B     *+10                                                     30640000
         MVC   1(0,R15),0(R1)      EXECUTED                             30650000
         EX    R14,*-6             MOVE COMMAND INTO PFK TABLE          30660000
*        LA    R14,1(,R14)         D                                    30670000
*        CVD   R14,DOUBLE           E                                   30680000
*        OI    DOUBLE+7,X'0F'        B                                  30690000
*        UNPK  MSG+2(3),DOUBLE+6(2)   U                                 30700000
*        MVI   MSG+5,C' '              G                                30710000
*        MVC   MSG+6(10),1(R15)    D                                    30720000
*        LA    R0,14                E                                   30730000
*        STH   R0,MSG                B                                  30740000
*        B     PROMPT                 U                                 30750000
PFKSETX  MVC   MSG(MSG40L),MSG40   HAS BEEN RESET                       30760000
         B     PROMPT                                                   30770000
PFKSETF  MVI   1(R15),C' '                                              30780000
         MVC   2(PFKTABL-2,R15),1(R15)                                  30790000
         B     PFKSETX                                                  30800000
PFKSET9  MVC   MSG(MSG41L),MSG41   INVALID PFK COMMAND                  30810000
         B     PROMPT                                                   30820000
         SPACE                                                          30830000
************************************************************            30840000
*                                                          *            30850000
*         SMF                                              *            30860000
*                                                          *            30870000
************************************************************            30880000
         SPACE                                                          30890000
SMFMODE  EQU   *                                                        30900000
         LA    R15,OPD1            GET FIRST OPERAND ENTRY              30910000
         TM    6(R15),PRESENT      ARE THERE ANY OPERANDS               30920000
         BZ    SMFFLIP             NO, FLIP FLOP                        30930000
         L     R14,0(,R15)         POINT TO OPERAND                     30940000
         CLI   5(R15),2            IS LENGTH 2                          30950000
         BNE   SMFOFF              NO, TRY OFF                          30960000
         CLC   0(2,R14),=C'ON'     YES, IS IT 'ON'                      30970000
         BNE   ERRKW               NO, INVALID OPERAND                  30980000
         OI    SMFSW,SMFBIT                                             30990000
SMFLOAD  L     R15,CALLSMFA        GET ADDRESS OF REVSMF                31000000
         LTR   R15,R15             IS REVSMF LOADED                     31010000
         BNZ   FILLSCR             YES, BRANCH                          31020000
         AIF   (NOT &MVS).MA0060                                   @L01
*        L     R0,=V(REVSMF)       WILL USE THIS INSTEAD OF LOAD        31030000
*                                  AFTER REVSMF IS TESTED               31040000
         LOAD  EP=REVSMF,ERRET=SMFERR                                   31050000
         AGO   .NA0060
.MA0060  ANOP  ,                                                   @L01
         L     R0,=V(REVSMF)       Get address of REVSMF           @L01
.NA0060  ANOP  ,                                                   @L01
         ST    R0,CALLSMFA         SAVE ADDRESS OF REVSMF               31060000
         B     FILLSCR                                                  31070000
SMFERR   NI    SMFSW,255-SMFBIT    LOAD FAILED                          31080000
         B     FILLSCR                                                  31090000
SMFOFF   CLI   5(R15),3            IS LENGTH 3                          31100000
         BNE   ERRKW                                                    31110000
         CLC   0(3,R14),=C'OFF'                                         31120000
         BNE   ERRKW                                                    31130000
         NI    SMFSW,255-SMFBIT                                         31140000
         B     FILLSCR                                                  31150000
SMFFLIP  XI    SMFSW,SMFBIT                                             31160000
         TM    SMFSW,SMFBIT        DID WE FLIP IT ON                    31170000
         BO    SMFLOAD             YES - CHECK FOR REVSMF               31180000
         B     FILLSCR                                                  31190000
         SPACE                                                          31200000
************************************************************            31210000
*                                                          *            31220000
*         CAPS / ASIS                                      *            31230000
*                                                          *            31240000
************************************************************            31250000
         SPACE                                                          31260000
CAPS     MVC   PERIODS,CAPST                                            31270000
         B     FILLSCR                                                  31280000
         SPACE                                                          31290000
ASIS     MVC   PERIODS,ASIST                                            31300000
         B     FILLSCR                                                  31310000
         SPACE                                                          31320000
************************************************************            31330000
*                                                          *            31340000
*         MEMBER                                           *            31350000
*                                                          *            31360000
************************************************************            31370000
         SPACE                                                          31380000
MEMBERP  EQU   *                                                        31390000
         TM    DSORG,X'02'         IS DSORG PO                          31400000
         BZ    MEMPDS              NO, BRANCH                           31410000
         LA    R15,OPD1            GET FIRST OPERAND ENTRY              31420000
         TM    6(R15),PRESENT      ARE THERE ANY OPERANDS               31430000
         BZ    ERRMISS             NO, MISSING OPERAND                  31440000
         TM    6(R15),QUOTED       IS OPERAND QUOTED                    31450000
         BO    ERRINV              YES, INVALID                         31460000
         LH    R1,4(,R15)          GET LENGTH                           31470000
         LTR   R1,R1               IS IT NULL STRING                    31480000
         BZ    ERRMISS             YES, ERROR                           31490000
         CH    R1,=H'8'            IS LENGTH MORE THAN 8                31500000
         BH    ERRINV              YES, ERROR                           31510000
         MVC   BLDL(4),=AL2(1,76)                                       31520000
         MVC   BLDL+4(8),=CL8' '                                        31530000
         L     R14,0(,R15)         GET ADDRESS OF DATA                  31540000
         BCTR  R1,0                                                     31550000
         B     *+10                                                     31560000
         MVC   BLDL+4(0),0(R14)                                         31570000
         EX    R1,*-6                                                   31580000
         BLDL  (R4),BLDL                                                31590000
         LTR   R15,R15                                                  31600000
         BNZ   MEMERR                                                   31610000
         NI    STATUS,255-STNOMEM                                       31620000
         MVC   $MEMBER,BLDL+4                                           31630000
         L     R1,CHKPTTOP                                              31640000
         MVC   4(4,R1),BLDL+12     COPY TTR                             31650000
         ST    R15,CHKPTBOT        ZERO CHKPT TABLE                     31660000
         MVC   TTR,BLDL+12         TTR OF FIRST BLOCK                   31670000
         MVC   TTRK,BLDL+12        TTR OF FIRST BLOCK                   31680000
         FIND  (R4),TTRK,C                                              31690000
*        POINT (R4),TTR                                                 31700000
         MVC   DSNAIM(46),$DSNAME                                       31710000
         SLR   R15,R15                                                  31720000
         IC    R15,TTR+3           GET CONCATENATION NUMBER             31730000
         LTR   R15,R15                                                  31740000
         BZ    NOCONCAM                                                 31750000
         CVD   R15,DOUBLE          APPEND                               31760000
         OI    DOUBLE+7,X'0F'       CONCAT                              31770000
         LH    R15,$DDNAML           NUMBER                             31780000
         SH    R15,=H'4'              TO DDNAME                         31790000
         LA    R15,$DDNAME(R15)         SO IT                           31800000
         UNPK  1(3,R15),DOUBLE+6(2)      BECOMES                        31810000
         MVI   0(R15),C'+'                DDNAME+NNN                    31820000
         MVC   DSNAIM(46),$DDNAML  SHOW DDNAME INSTEAD OF DSNAME        31830000
NOCONCAM EQU   *                                                        31840000
         XC    COUNT,COUNT                                              31850000
         XC    DEBLOCKS(12),DEBLOCKS                                    31860000
         L     R14,SPANPTR                                              31870000
         SLR   R0,R0                                                    31880000
         STH   R0,0(,R14)          RESET SPAN BUFFER LENGTH             31890000
         ST    R0,FNDNUM           RESET LAST-FOUND NUMBER              31900000
         STH   R0,FNDOFF           RESET LAST-FOUND OFFSET              31910000
         B     NEWNAME                                                  31920000
MEMERR   CH    R15,=H'4'                                                31930000
         BNE   MEMERR2                                                  31940000
         MVC   MSG(MSG36L),MSG36   MEMBER NOT FOUND                     31950000
         B     PROMPT                                                   31960000
MEMERR2  MVC   MSG(MSG37L),MSG37   BLDL FAILED                          31970000
         B     PROMPT                                                   31980000
MEMPDS   MVC   MSG(MSG38L),MSG38   NOT PARTITIONED                      31990000
         B     PROMPT                                                   32000000
         SPACE                                                          32010000
************************************************************            32020000
*                                                          *            32030000
*         SNAP                                             *            32040000
*                                                          *            32050000
************************************************************            32060000
         SPACE                                                          32070000
SNAP     EQU   *                                                        32080000
*        LA    R7,SNAPR                                                 32090000
*        LA    R0,@DATA                                                 32100000
*        ST    R0,0(,R7)                                                32110000
*        A     R0,=A(@DATAL)                                            32120000
*        BCTR  R0,0                                                     32130000
*        ST    R0,4(,R7)                                                32140000
*        L     R0,CHKPTTOP                                              32150000
*        ST    R0,8(,R7)                                                32160000
*        L     R0,CHKPTBOT                                              32170000
*        AH    R0,=H'80'                                                32180000
*        C     R0,ENDPTR                                                32190000
*        BNH   *+8                                                      32200000
*        LH    R0,ENDPTR                                                32210000
*        SH    R0,=H'4'                                                 32220000
*        ST    R0,12(,R7)                                               32230000
*        OI    12(R7),X'80'                                             32240000
*        MVC   SNAPDW(SNAPDL),SNAPD                                     32250000
*        LA    R6,SNAPDW                                                32260000
*        OPEN  ((R6),OUTPUT),MF=(E,OPEND)                               32270000
*        TM    48(R6),X'10'                                             32280000
*        BNO   PROMPT                                                   32290000
*        MVC   SNAPLW(SNAPLL),SNAPL                                     32300000
*        SNAP  DCB=(R6),ID=7,PDATA=(REGS),LIST=(R7),MF=(E,SNAPLW)       32310000
*        CLOSE ((R6)),MF=(E,OPEND)                                      32320000
         B     PROMPT                                                   32330000
         SPACE                                                          32340000
************************************************************            32350000
*                                                          *            32360000
*         DEBUG  (SHOW TGET RESULTS ON SCREEN)             *            32370000
*                                                          *            32380000
************************************************************            32390000
         SPACE                                                          32400000
DEBUG    EQU   *                                                    ABL 32410000
         MVI   DEBUGSW,C'D'        START DEBUG DISPLAYS IF ERROR    ABL 32420000
         MVI   HELPFLG,C'D'        REFRESH OF SCREEN NEEDED         ABL 32430000
         B     FILLSCR                                              ABL 32440000
         SPACE                                                          32450000
************************************************************            32460000
*                                                          *            32470000
*         SUBROUTINE TO CONVERT DATA TO HEX                *            32480000
*                                                          *            32490000
************************************************************            32500000
         SPACE                                                          32510000
HEX      MVC   1(1,R15),0(R1)      MOVE BYTE                            32520000
         UNPK  0(3,R15),1(2,R15)   UNPACK                               32530000
         TR    0(2,R15),HEXTAB-240                                      32540000
         LA    R15,2(,R15)         INCREMENT OUTPUT PTR                 32550000
         LA    R1,1(,R1)           INCREMENT INPUT PTR                  32560000
         BCT   R0,HEX              DECREMENT LENGTH, THEN LOOP          32570000
         MVI   0(R15),C' '         BLANK THE TRAILING BYTE              32580000
         BR    R14                 RETURN TO CALLER                     32590000
         SPACE                                                          32600000
HEXTAB   DC    C'0123456789ABCDEF' TRANSLATE TABLE                      32610000
         SPACE                                                          32620000
************************************************************            32630000
*                                                          *            32640000
*         SUBROUTINE TO GET A LOGICAL RECORD               *            32650000
*                                                          *            32660000
************************************************************            32670000
         SPACE                                                          32680000
*                                                                       32690000
*               INPUT                                                   32700000
*                R4 DCB ADDRESS                                         32710000
*                CHKPTTOP  -  TOP OF CHECKPOINT TABLE                   32720000
*                CHKPTBOT  -  BOTTOM OF CHECKPOINT TABLE, ZERO 1ST TIME 32730000
*                COUNT     -  LAST LOGICAL RECORD NUMBER READ           32740000
*                BLOCKPTR  -  ADDRESS OF BUFFER                         32750000
*                DEBLOCKS  -  DEBLOCKING INFO (ZEROS FIRST TIME)        32760000
*                SPANPTR   -  ADDRESS OF AREA TO COMBINE                32770000
*                             SPANNED RECORD SEGMENTS                   32780000
*                                                                       32790000
*               OUTPUT                                                  32800000
*                 R1 CONTAINS LENGTH OF RECORD (OR -1 IF END OF FILE)   32810000
*                 R2 CONTAINS ADDRESS OF RECORD (OR 0 IF END OF FILE)   32820000
*                                                                       32830000
GET      ST    R14,READR                                                32840000
         TM    DCBRECFM,X'10'      BLOCKED                              32850000
         BZ    READI               NO, BRANCH                           32860000
*        TM    DCBRECFM,X'C0'      UNDEFINED                            32870000
*        BO    READI               YES, BRANCH                          32880000
         LM    R0,R2,DEBLOCKS      GET DEBLOCKING STATUS                32890000
         AR    R2,R1               POINT TO NEXT RECORD                 32900000
         CR    R2,R0               END OF BLOCK (OR FIRST TIME)         32910000
         BNL   READI               YES, BRANCH                          32920000
         TM    TSTRECFM,X'40'      VARIABLE LENGTH                      32930000
         BO    READVB              YES, BRANCH                          32940000
         ST    R2,DEBLOCKS+8       FIXED BLOCKED                        32950000
         B     READX                                                    32960000
READI    L     R5,CHKPTBOT         GET LAST CHECKPOINT                  32970000
         LTR   R5,R5               IS THIS FIRST READ                   32980000
         BNZ   READNF              BRANCH IF NOT FIRST                  32990000
         L     R5,CHKPTTOP                                              33000000
         B     READNEW             READING A RECORD NOT READ BEFORE     33010000
READNF   L     R1,0(,R5)           GET NUMBER OF HIGHEST RECORD READ    33020000
         LTR   R1,R1               WAS IT EOF                           33030000
         BM    READOLD             YES, BRANCH                          33040000
         C     R1,COUNT            HAVE WE READ THIS RECORD BEFORE      33050000
         BNH   READADD             NO, BRANCH                           33060000
READOLD  LA    R5,CHKPTDUM         YES, DONT CHANGE CHKPT TABLE         33070000
         B     READ                                                     33080000
READADD  LA    R5,20(,R5)          ADD AN ENTRY TO THE CHKPT TABLE      33090000
         C     R5,ENDPTR           IS TABLE FILLED UP                   33100000
         BL    *+8                 NO, SKIP NEXT INSTR                  33110000
         BAL   R14,READHALF        YES, HALVE THE TABLE                 33120000
READNEW  ST    R5,CHKPTBOT         SAVE NEW CURRENT CHECKPOINT POINTER  33130000
         MVC   0(4,R5),COUNT                                            33140000
         XC    4(4,R5),4(R5)                                            33150000
         MVC   8(12,R5),DEBLOCKS                                        33160000
READ     TM    STATUS,STNOMEM      ARE WE IN MEMBER-NOT-FOUND STATUS    33170000
         BO    DYNEOD              YES, JUST GO TO END OF FILE          33180000
         L     R2,BLOCKPTR                                              33190000
         READ  DYNDECBW,SF,(R4),(R2),'S',MF=E                           33200000
         CHECK DYNDECBW                                                 33210000
         CLI   SYNADSW,0           WAS SYNAD EXIT TAKEN?                33220000
         BNE   IOERR               YES, BRANCH                          33230000
         LH    R1,DCBBLKSI                                              33240000
         L     R14,DYNDECBW+16                                          33250000
         SH    R1,14(,R14)         SUBTRACT RESIDUAL COUNT              33260000
         TM    TSTRECFM,X'40'      VARIABLE LENGTH RECORDS              33270000
         BZ    OKREAD              NO, BRANCH                           33280000
         LH    R1,0(,R2)           YES, USE BLKSIZE IN BDW              33290000
*        SH    R1,=H'4'                                              $V 33300000
*        LA    R2,4(,R2)                                             $V 33310000
OKREAD   LA    R0,0(R1,R2)         END OF BLOCK                         33320000
         STM   R0,R2,DEBLOCKS      SAVE STATUS INFO                     33330000
         CLC   4(4,R5),=F'0'       IS THIS 2ND READ PER GET             33340000
         BNZ   NOTED               YES, BYPASS NOTE                     33350000
         NOTE  (R4)                                                     33360000
         ST    R1,4(,R5)           SAVE TTR IN TABLE                    33370000
NOTED    EQU   *                                                        33380000
         L     R1,DEBLOCKS+4       RESTORE LENGTH                       33390000
         TM    DCBRECFM,X'10'      BLOCKED?                             33400000
         BO    READB               YES, BRANCH                          33410000
         TM    TSTRECFM,X'48'      VARIABLE UNBLOCKED SPANNED           33420000
         BNO   READX               NO                                   33430000
         B     READVBS             YES, SAME AS BLOCKED                 33440000
READB    TM    DCBRECFM,X'C0'      UNDEFINED                            33450000
         BO    READX                                                    33460000
         TM    DCBRECFM,X'40'      VARIABLE LENGTH                      33470000
         BO    READVBI                                                  33480000
READFB   LH    R1,DCBLRECL         FIXED BLOCKED                        33490000
         ST    R1,DEBLOCKS+4       SAVE STATUS INFO                     33500000
         B     READX                                                    33510000
READVBI  LA    R2,4(,R2)           POINT PAST BDW                    $V 33520000
READVB   TM    DCBRECFM,X'08'      SPANNED                              33530000
         BO    READVBS                                                  33540000
READVBR  LH    R1,0(,R2)                                                33550000
*        SH    R1,=H'4'                                              $V 33560000
*        LA    R2,4(,R2)                                             $V 33570000
         STM   R1,R2,DEBLOCKS+4    SAVE STATUS INFO                     33580000
         B     READX                                                    33590000
READVBS  CLI   2(R2),0             SEGMENT                              33600000
         BE    READVBR             NO, BRANCH                           33610000
*               THIRD BYTE IS X'01' FOR FIRST SEGMENT                   33620000
*                             X'03' FOR MIDDLE SEGMENT                  33630000
*                             X'02' FOR LAST SEGMENT                    33640000
         CLI   2(R2),1             FIRST SEGMENT                        33650000
         BNE   READSEG2                                                 33660000
         L     R14,SPANPTR         ADDRESS TO MOVE TO                   33670000
         LH    R15,0(,R2)          LENGTH TO MOVE                       33680000
         LR    R1,R15              LENGTH TO MOVE                       33690000
         LR    R0,R2               ADDRESS TO MOVE FROM                 33700000
         MVCL  R14,R0              MOVE SEGMENT RDW AND DATA            33710000
         B     READSEGD            GO DEBLOCK NEXT SEGMENT              33720000
READSEG2 L     R14,SPANPTR         ADDRESS OF PRIOR SEGMENTS            33730000
         LH    R0,0(,R14)          LENGTH OF PRIOR SEGMENTS             33740000
         LH    R15,0(,R2)          LENGTH+4 OF NEW SEGMENT              33750000
         SH    R15,=H'4'           LENGTH OF NEW SEGMENT                33760000
         LR    R1,R15              LENGTH TO BE MOVED                   33770000
         AR    R15,R0              COMBINE LENGTHS                      33780000
         STH   R15,0(,R14)         STORE COMBINED LENGTHS               33790000
         LR    R15,R1              LENGTH TO ME MOVED                   33800000
         AR    R14,R0              ADDRESS TO MOVE TO                   33810000
         LA    R0,4(,R2)           ADDRESS TO MOVE FROM                 33820000
         MVCL  R14,R0              MOVE SEGMENT DATA                    33830000
         CLI   2(R2),2             LAST SEGMENT                         33840000
         BNE   READSEGD            NO, GO DEBLOCK NEXT SEGMENT          33850000
         LH    R1,0(,R2)           GET LENGTH OF THIS SEGMENT           33860000
         STM   R1,R2,DEBLOCKS+4    SAVE LENGTH AND ADDRESS              33870000
         L     R2,SPANPTR          POINT TO COMBINED RECORD             33880000
         LH    R1,0(,R2)           GET COMBINED LENGTH                  33890000
*        SH    R1,=H'4'                                              $V 33900000
*        LA    R2,4(,R2)                                             $V 33910000
         B     READX               EXIT                                 33920000
READSEGD EQU   *                   NOT NECESSARY TO CHECK RECFM X'10'   33930000
         LH    R1,0(,R2)           LENGTH                               33940000
         AR    R2,R1               POINT TO NEXT SEGMENT                33950000
         C     R2,DEBLOCKS         END OF BLOCK                         33960000
         BNL   READ                YES, BRANCH                          33970000
         B     READVBS             GO PROCESS NEW SEGMENT               33980000
READX    L     R14,READR                                                33990000
         BR    R14                                                      34000000
DYNEOD   EQU   *                                                        34010000
         L     R1,=F'-1'           EOF                                  34020000
         ST    R1,0(,R5)           SAVE EOF IN CHKPT TABLE              34030000
         SLR   R2,R2                                                    34040000
         B     READX                                                    34050000
         SPACE                                                          34060000
READHALF LA    R0,20               LENGTH OF EACH ENTRY                 34070000
         L     R15,CHKPTTOP        POINT TO FIRST ENTRY                 34080000
         AR    R15,R0              POINT TO SECOND ENTRY                34090000
         LR    R1,R15                                                   34100000
         AR    R1,R0               POINT TO THIRD ENTRY                 34110000
READHMOV MVC   0(20,R15),0(R1)     MOVE 3RD TO 2ND                      34120000
*                                       5TH TO 3RD                      34130000
*                                       7TH TO 4TH, ETC                 34140000
         AR    R15,R0              RECEIVING FIELD DOWN 1               34150000
         AR    R1,R0               SENDING FIELD DOWN 1                 34160000
         AR    R1,R0               SENDING FIELD DOWN 2                 34170000
         CR    R1,R5               ARE WE PAST THE LAST ENTRY           34180000
         BL    READHMOV            NO, BRANCH                           34190000
         LR    R5,R15              YES, NEW CURRENT POINTER             34200000
         BR    R14                 RETURN                               34210000
         SPACE                                                          34220000
************************************************************            34230000
*                                                          *            34240000
*         END OF PROGRAM                                   *            34250000
*                                                          *            34260000
************************************************************            34270000
         SPACE                                                          34280000
END      LA    R1,ERAZE                                                 34290000
         LA    R0,ERAZEL                                                34300000
         O     R1,=A(X'03000000')                                       34310000
         SVC   93                  ERASE THE SCREEN                     34320000
         TM    MODE,STSIZEX        TERMINAL SIZE RESET?                 34330000
         BNO   END2                NO, BRANCH                           34340000
         STSIZE SIZE=132,LINE=27   YES, CHANGE THE SIZE BACK            34350000
         LA    R1,=X'277EC11140403C4040001DC813'  WRITE ALTERNATE       34360000
         LA    R0,13                              LENGTH                34370000
         TPUT  (1),(0),FULLSCR                    WRITE FULL SCREEN     34380000
         XI    MODE,STSIZEX        RESET STSIZE BIT                     34390000
END2     DS    0H                                                       34400000
         STLINENO LINE=1,MODE=OFF  TURN OFF FULL SCREEN MODE        ABL 34410000
         SPACE 1                                                    ABL 34420000
         TCLEARQ INPUT             CLEAR INPUT QUEUE                ABL 34430000
         LA    R15,0                                                    34440000
         B     EXITRC                                                   34450000
         SPACE                                                          34460000
IOERR    LA    R1,SYNADMSG                                              34470000
         LA    R0,78                                                    34480000
         BAL   R14,PUTMSG                                               34490000
         B     EXIT12                                                   34500000
         SPACE                                                          34510000
EXIT12   LA    R15,12                                                   34520000
         SPACE                                                          34530000
EXITRC   CH    R15,RC                                                   34540000
         BNH   *+8                                                      34550000
         STH   R15,RC              SET HIGHEST RC                       34560000
         TM    MODE,STSIZEX        TERMINAL SIZE RESET?                 34570000
         BNO   EXITRC2             NO, BRANCH                           34580000
         STSIZE SIZE=132,LINE=27   YES, CHANGE THE SIZE BACK            34590000
         LA    R1,=X'277EC11140403C4040001DC813'  WRITE ALTERNATE       34600000
         LA    R0,13                              LENGTH                34610000
         TPUT  (1),(0),FULLSCR                    WRITE FULL SCREEN     34620000
         XI    MODE,STSIZEX        RESET STSIZE BIT                     34630000
EXITRC2  DS    0H                                                       34640000
         TM    STATUS,STGMVU                                            34650000
         BZ    NOGMVU                                                   34660000
         L     R0,ANSWER+4         LENGTH                               34670000
         L     R1,ANSWER           ADDRESS                              34680000
         FREEMAIN R,LV=(0),A=(1)                                        34690000
         NI    STATUS,255-STGMVU                                        34700000
NOGMVU   EQU   *                                                        34710000
         TM    STATUS,STOPEN                                            34720000
         BZ    NOCLOSE                                                  34730000
         TM    DCBOFLGS,X'10'      IS IT OPEN?                          34740000
         BZ    NOCLOSE             NO, BRANCH                           34750000
         MVI   CLOSED,X'80'                                             34760000
         CLOSE ((R4)),MF=(E,CLOSED)                                     34770000
         NI    STATUS,255-STOPEN   CLOSED                               34780000
         DROP  R4                  IHADCB                               34790000
NOCLOSE  EQU   *                                                        34800000
         LM    R10,R12,BASE1                                            34810000
         L     R14,RET1                                                 34820000
         BR    R14                 RETURN TO PHASE 1                    34830000
         SPACE                                                          34840000
************************************************************            34850000
*                                                          *            34860000
*        PUTMSG ROUTINE                                    *            34870000
*                                                          *            34880000
************************************************************            34890000
         SPACE                                                          34900000
PUTMSG   STM   R14,R1,PUTLINS                                           34910000
         XC    MYOLD(8),MYOLD                                           34920000
         XC    MYSEG1(4),MYSEG1                                         34930000
         MVC   MYPTPB(12),MODLPTPM                                      34940000
         LA    R14,1               NO. OF MESSAGE SEGMENTS              34950000
         ST    R14,MYOLD                                                34960000
         LA    R14,MYSEG1          POINT TO 1ST SEGMENT                 34970000
         ST    R14,MYOLD+4                                              34980000
         LR    R14,R0              LENGTH IN R0                         34990000
         LA    R14,4(,R14)         ADD 4                                35000000
         LA    R15,MYSEG1+4                                             35010000
         CLC   0(3,R1),=C'IKJ'     IS DATA PRECEEDED BY MESSAGE ID?     35020000
         BE    *+16                YES - BRANCH                         35030000
         LA    R14,1(,R14)         ADD 1 TO LENGTH                      35040000
         MVI   0(R15),C' '         INSERT LEADING BLANK                 35050000
         LA    R15,1(,R15)         BUMP POINTER                         35060000
         STH   R14,MYSEG1                                               35070000
         LR    R14,R0                                                   35080000
         BCTR  R14,0                                                    35090000
         B     *+10                                                     35100000
         MVC   0(0,R15),0(R1)      MOVE MESSAGE IN                      35110000
         EX    R14,*-6                                                  35120000
         L     R15,MYPUTLEP                                             35130000
         SPACE                                                          35140000
         PUTLINE PARM=MYPTPB,OUTPUT=(MYOLD),ENTRY=(15),MF=(E,MYIOPL)    35150000
         SPACE                                                          35160000
         LM    R14,R1,PUTLINS                                           35170000
         BR    R14                                                      35180000
         SPACE                                                          35190000
************************************************************            35200000
*                                                          *            35210000
*        SYNAD EXIT                                        *            35220000
*                                                          *            35230000
************************************************************            35240000
         SPACE                                                          35250000
*        THIS ROUTINE IS ENTERED DURING THE 'CHECK' MACRO               35260000
*        IF AN I/O ERROR OCCURS.                                        35270000
         SPACE                                                          35280000
DYNSYNAD EQU   *                                                        35290000
         SYNADAF ACSMETH=BSAM                                           35300000
         MVC   SYNADMSG(78),50(R1)                                      35310000
         MVI   SYNADSW,X'FF'                                            35320000
         SYNADRLS                                                       35330000
         BR    R14                                                      35340000
         SPACE                                                          35350000
************************************************************            35360000
*                                                          *            35370000
*        CONSTANTS                                         *            35380000
*                                                          *            35390000
************************************************************            35400000
         SPACE                                                          35410000
         LTORG                                                          35420000
         SPACE                                                          35430000
BOTNINES DC    F'99999999'                                              35440000
         SPACE                                                          35450000
MODLPTPM PUTLINE OUTPUT=(1,TERM,SINGLE,INFOR),                         X35460000
               TERMPUT=(EDIT,WAIT,NOHOLD,NOBREAK),MF=L                  35470000
         SPACE                                                          35480000
MODLPTPB PUTLINE OUTPUT=(1,TERM,SINGLE,DATA),                          X35490000
               TERMPUT=(EDIT,WAIT,NOHOLD,NOBREAK),MF=L                  35500000
         SPACE                                                          35510000
         PRINT NOGEN                                                    35520000
         SPACE                                                          35530000
SEQDCB   DCB   DDNAME=DYNAM,DSORG=PS,MACRF=(RP),                       +35540000
               EODAD=0,SYNAD=0                                          35550000
SEQDCBL  EQU   *-SEQDCB                                                 35560000
         SPACE                                                          35570000
PDSDCB   DCB   DDNAME=DYNAM,DSORG=PO,MACRF=(R),                        +35580000
               EODAD=0,SYNAD=0                                          35590000
PDSDCBL  EQU   *-PDSDCB                                                 35600000
         SPACE                                                          35610000
         PRINT GEN                                                      35620000
         SPACE                                                          35630000
DYNREAD  READ  DYNDECB,SF,0,0,'S',MF=L                                  35640000
DYNDECBL EQU   *-DYNDECB                                                35650000
         SPACE                                                          35660000
GMVU     GETMAIN VU,MF=L                                                35670000
GMVUL    EQU   *-GMVU                                                   35680000
         SPACE                                                          35690000
MSG04    DC    C'UNABLE TO OPEN DATASET'                                35700000
MSG08    DC    C'SPECIFIED MEMBER NOT FOUND IN DATASET'                 35710000
MSG20    MSG   'INVALID COMMAND'                                        35720000
MSG21    MSG   'INVALID PF KEY'                                         35730000
MSG31    MSG   'FOUND IN LINE XXXXX COL XXXXX'                          35740000
MSG32    MSG   'BOTTOM OF DATA REACHED'                                 35750000
MSG33    MSG   'STRING NOT SPECIFIED'                                   35760000
MSG34    MSG   'INVALID OPERAND'                                        35770000
MSG35    MSG   'MISSING OPERAND'                                        35780000
MSG36    MSG   'MEMBER NOT FOUND'                                       35790000
MSG37    MSG   'BLDL FAILED'                                            35800000
MSG38    MSG   'NOT A PDS'                                              35810000
MSG40    MSG   'PFK HAS BEEN RESET'                                     35820000
MSG41    MSG   'INVALID PFK COMMAND'                                    35830000
ASIST    DC    64X'4B',X'40',9X'4B'                                     35840000
         DC    X'4A4B4C4D4E4F'       CENT,PERIOD,LESS,LPAREN,PLUS,BAR   35850000
         DC    X'50',9X'4B'          AMPERSAND                          35860000
         DC    X'5A5B5C5D5E5F'       EXCL,$,DOT,RPAREN,SEMI,NOT         35870000
         DC    X'6061',8X'4B'        HYPHEN,SLASH                       35880000
         DC    X'6A6B6C6D6E6F'       WHAT,COMMA,PERCENT,UNDLN,GT,QM     35890000
         DC    9X'4B',X'79'          70-78                              35900000
         DC    X'7A7B7C7D7E7F'       COLON,POUND,AT,APOST,EQ,DBLQUOTE   35910000
         DC    X'4B'                                                    35920000
         DC    X'818283848586878889',7X'4B'                             35930000
         DC    X'919293949596979899',8X'4B'                             35940000
         DC    X'A2A3A4A5A6A7A8A9',23X'4B'                              35950000
         DC    C'ABCDEFGHI',7X'4B'                                      35960000
         DC    C'JKLMNOPQR',8X'4B'                                      35970000
         DC    C'STUVWXYZ',06X'4B'                                      35980000
         DC    C'0123456789',6X'4B'                                     35990000
CAPST    DC    64X'4B',X'40',9X'4B'                                     36000000
         DC    X'4A4B4C4D4E4F'       CENT,PERIOD,LESS,LPAREN,PLUS,BAR   36010000
         DC    X'50',9X'4B'          AMPERSAND                          36020000
         DC    X'5A5B5C5D5E5F'       EXCL,$,DOT,RPAREN,SEMI,NOT         36030000
         DC    X'6061',8X'4B'        HYPHEN,SLASH                       36040000
         DC    X'6A6B6C6D6E6F'       WHAT,COMMA,PERCENT,UNDLN,GT,QM     36050000
         DC    9X'4B',X'79'          70-78                              36060000
         DC    X'7A7B7C7D7E7F'       COLON,POUND,AT,APOST,EQ,DBLQUOTE   36070000
         DC    X'4B'                                                    36080000
         DC    C'ABCDEFGHI',7X'4B'                                      36090000
         DC    C'JKLMNOPQR',8X'4B'                                      36100000
         DC    C'STUVWXYZ',23X'4B'                                      36110000
         DC    C'ABCDEFGHI',7X'4B'                                      36120000
         DC    C'JKLMNOPQR',8X'4B'                                      36130000
         DC    C'STUVWXYZ',06X'4B'                                      36140000
         DC    C'0123456789',6X'4B'                                     36150000
SCREEN   DCS   X'40',SBA,24,80,SBA,1,1                                  36160000
         DCS   SF,PROHI                                                 36170000
SCRDSN   DC    55C'-'                                                   36180000
         DC    C' LINE '                                                36190000
SCRLINE  DCS   C'00000'                                                 36200000
         DCS   C' COL '                                                 36210000
SCRCOL   DCS   C'001 080 '                                              36220000
*CRRTA   DCS   SF,PROHIS,RTA,02,02,X'40'                                36230007
         DCS   SF,PROHIS,C'INPUT >>--->'                                36240000
         DCS   SF,UNPHI,CL56' '                                         36250008
         DCS   SF,PROHI,C'RANGE'                                        36260000
SCRRGE   DCS   SF,UNPHI,C'20'      START WITH RANGE EQUAL PAGE          36270000
         DCS   SF,PROLO                                                 36280000
SCREENL  EQU   *-SCREEN                                                 36290000
SCRDATA  EQU   *                                                        36300000
SCRPAD   DCS   RTA,1,1,C' '                                             36310000
SCRSUF   DCS   SBA,02,15,IC                                             36320000
         SPACE                                                          36330000
FLD1BA   DCS   2,15                                                     36340000
FLD2BA   DCS   2,78                                                     36350000
FLD1BB   DC    X'C2D4'   R2,C17 ON 3278-5                               36360000
FLD2BB   DC    X'C3D1'   R2,C78 ON 3278-5                               36370000
         SPACE                                                          36380000
MARKS    DC    18C'----+'                                               36390000
         SPACE                                                          36400000
ERAZE    DCS   X'40',SBA,24,79,SBA,1,1                                  36410000
         DCS   RTA,7,1,X'00',RTA,13,1,X'00'                             36420000
         DCS   RTA,19,1,X'00',RTA,1,1,X'00',IC                          36430000
ERAZEL   EQU   *-ERAZE                                                  36440000
*                                                                       36450000
*               TRANSLATE TABLES                                        36460000
*                                                                       36470000
FINDSBA  DC    17X'00',X'11',238X'00'                                   36480000
NUMERIC  DC    240X'FF',10X'00',6X'FF'                                  36490000
HEXDATA  DC    193X'FF',6X'00',41X'FF',10X'00',6X'FF'                   36500000
         SPACE                                                          36510000
TABNONBL DC    64X'FF'                                                  36520000
         DC    X'00'               BLANK                                36530000
         DC    42X'FF'                                                  36540000
         DC    X'FF'               COMMA                                36550000
         DC    148X'FF'                                                 36560000
TABBLANK DC    64X'00'                                                  36570000
         DC    X'40'               BLANK                                36580000
         DC    42X'00'                                                  36590000
         DC    X'00'               COMMA                                36600000
         DC    148X'00'                                                 36610000
TABQUOTE DC    125X'00',X'7D',130X'00'                                  36620000
         SPACE                                                          36630000
KAPS     DC    129AL1(*-KAPS)      00-80                                36640000
         DC    9AL1(*-KAPS+X'40')  81-89 BECOME C1-C9                   36650000
         DC    7AL1(*-KAPS)        8A-90                                36660000
         DC    9AL1(*-KAPS+X'40')  91-99 BECOME D1-D9                   36670000
         DC    8AL1(*-KAPS)        9A-A1                                36680000
         DC    8AL1(*-KAPS+X'40')  A2-A9 BECOME E2-E9                   36690000
         DC    86AL1(*-KAPS)       AA-FF                                36700000
         SPACE                                                          36710000
*SNAPD   DCB   DDNAME=SNAPOUT,MACRF=(W),DSORG=PS,                       36720000
*              RECFM=VBA,LRECL=125,BLKSIZE=882                          36730000
*NAPD    DC    0F'0',20X'00',A(1),X'00004000',A(1,1),X'54',AL3(0)       36740000
*        DC    CL8'SNAPOUT',X'02000020',A(1,1),AL2(0,882)               36750000
*        DC    A(0,1,1,1),AL2(0,125),A(1)                               36760000
*NAPDL   EQU   *-SNAPD                                                  36770000
*NAPL    SNAP  DCB=0,ID=7,PDATA=(REGS),LIST=0,MF=L                      36780000
*NAPLL   EQU   *-SNAPL                                                  36790000
         SPACE 2                                                    ABL 36800000
BLANKS   DC    CL132' '                                             ABL 36810000
         SPACE 2                                                    ABL 36820000
HELPMSGS DC    CL80'REVIEW - A FULL-SCREEN LIST COMMAND                X36830000
                                           '                        ABL 36840000
         DC    CL80'                                                   X36850000
                                           '                        ABL 36860000
         DC    CL80'SUBCOMMANDS:                                       X36870000
                                           '                        ABL 36880000
         DC    CL80'MEMBER NEWMEMBER     (POSITIONS TO ANOTHER MEMBER) X36890000
                                           '                        ABL 36900000
         DC    CL80'FIND STRING COL      (FINDS UPPER-CASE TEXT ONLY)  X36910000
                                           '                        ABL 36920000
         DC    CL80'CAPS                 (DISPLAYS DATA IN UPPER CASE OX36930000
               NLY)                        '                        ABL 36940000
         DC    CL80'ASIS                 (DISPLAYS DATA IN UPPER AND LOX36950000
               WER CASE)                   '                        ABL 36960000
         DC    CL80'PFK3 NEWSUBCOM       (CHANGES THE PFK DEFINITION FOX36970000
               R THE NAMED PF KEY)         '                        ABL 36980000
         DC    CL80'TSO ANYCOMMAND       (INVOKES THE TSO COMMAND)     X36990000
                                           '                        ABL 37000000
         DC    CL80'SMF                  (SWITCHES SMF PROCESSING MODE)X37010000
                                           '                        ABL 37020000
         DC    CL80'HEX                  (SWITCHES HEX MODE)           X37030000
                                           '                        ABL 37040000
         DC    CL80'TOP                  (POSITIONS TO THE TOP OF THE MX37050000
               EMBER)                      '                        ABL 37060000
         DC    CL80'BOTTOM               (POSITIONS TO THE BOTTOM OF THX37070000
               E MEMBER)                     '                      ABL 37080000
         DC    CL80'UP NUM               (POSITIONS UP THE NUMBER OF LIX37090000
               NES SPECIFIED)              '                        ABL 37100000
         DC    CL80'DOWN NUM             (POSITIONS DOWN THE NUMBER OF X37110000
               LINES SPECIFIED)            '                        ABL 37120000
         DC    CL80'                                                   X37130000
                                           '                        ABL 37140000
         DC    CL80'INITIAL PF KEYS:     1 -- HELP      2 -- (NONE)    X37150000
               3 -- END                    '                        ABL 37160000
         DC    CL80'                     4 -- END       5 -- FIND      X37170000
               6 -- (NONE)                 '                        ABL 37180000
         DC    CL80'                     7 -- UP        8 -- DOWN      X37190000
               9 -- HEX                    '                        ABL 37200000
         DC    CL80'                    10 -- LEFT     11 -- RIGHT    1X37210000
               2 -- (NONE)                 '                        ABL 37220000
         DC    X'FF'                                                ABL 37230000
         DC    0D'0'               END OF CSECT                         37240000
         SPACE                                                          37250000
************************************************************            37260000
*                                                          *            37270000
*        PARSE PARAMETERS                                  *            37280000
*                                                          *            37290000
************************************************************            37300000
         SPACE                                                          37310000
         PRINT NOGEN                                                    37320000
REVPCL   IKJPARM                                                        37330000
DSN      IKJPOSIT DSNAME,PROMPT='DATA SET NAME'                         37340000
UNITKW   IKJKEYWD                                                       37350000
         IKJNAME 'UNIT',SUBFLD=UNITSF                                   37360000
VOLKW    IKJKEYWD                                                       37370000
         IKJNAME 'VOLUME',SUBFLD=VOLSF                                  37380000
FILEKW   IKJKEYWD                                                       37390000
         IKJNAME 'FILE'                                                 37400000
QUICKW   IKJKEYWD                                                       37410000
         IKJNAME 'QUICK'                                                37420000
UNITSF   IKJSUBF                                                        37430000
UNIT     IKJIDENT 'UNIT NAME',                                         +37440000
               FIRST=ALPHANUM,OTHER=ALPHANUM,MAXLNTH=8,                +37450000
               PROMPT='UNIT NAME'                                       37460000
VOLSF    IKJSUBF                                                        37470000
VOL      IKJIDENT 'VOLUME SERIAL',                                     +37480000
               FIRST=ALPHANUM,OTHER=ALPHANUM,MAXLNTH=6,                +37490000
               PROMPT='VOLUME SERIAL'                                   37500000
         IKJENDP                                                        37510000
         PRINT GEN                                                      37520000
         SPACE                                                          37530000
************************************************************            37540000
*                                                          *            37550000
*        DSECTS                                            *            37560000
*                                                          *            37570000
************************************************************            37580000
         SPACE                                                          37590000
@DATA    DSECT                                                          37600000
         DS    18F                 REGISTER SAVEAREA                    37610000
SIZE     DS    F                   SIZE OF THIS AREA                    37620000
CPPLPTR  DS    F                                                        37630000
LINKAREA DS    2F                                                       37640000
BASE1    DS    4F                                                       37650000
BASE2    DS    3F                                                       37660000
RET1     DS    F                                                        37670000
MYPPL    DS    7F                                                       37680000
MYANS    DS    F                                                        37690000
MYECB    DS    F                   USED BY PUTLINE ROUTINE              37700000
REVCBUF  DS    F                   *** CPPL FOR CALLED ROUTINE      ABL 37710000
REVUPT   DS    F                   *** CPPL FOR CALLED ROUTINE      ABL 37720000
REVPSCB  DS    F                   *** CPPL FOR CALLED ROUTINE      ABL 37730000
REVECT   DS    F                   *** CPPL FOR CALLED ROUTINE      ABL 37740000
PCMD     DS    CL8                 PRIMARY COMMAND NAME             ABL 37750000
MYIOPL   DS    4F                  USED BY PUTLINE ROUTINE              37760000
MYPTPB   DS    3F                  USED BY PUTLINE ROUTINE              37770000
MYOLD    DS    2F                  USED BY PUTLINE ROUTINE              37780000
MYSEG1   DS    2H,CL256            USED BY PUTLINE ROUTINE              37790000
PUTLINS  DS    4F                  USED BY PUTLINE ROUTINE              37800000
MYPUTLEP DS    F                   ADDRESS OF IKJPUTL                   37810000
MYSTPB   DS    0F                  5 WORDS USED BY STACK DELETE         37820000
MYDAPL   DS    5F                                                       37830000
MYDAPB   DS    21F                                                      37840000
MYDFPB   DS    5F                                                       37850000
UDSNAME  DS    2H,CL44                                                  37860000
$DSNAME  DS    H,CL44                                                   37870000
$MEMBER  DS    CL8                                                      37880000
$PASSWRD DS    CL8                                                      37890000
$DDNAML  DS    H                   THESE                                37900000
$DDNAME  DS    CL8                  THREE                               37910000
$CONCAT  DS    CL36                  TOGETHER                           37920000
$VOLSER  DS    CL6                                                      37930000
$UCBAD   DS    F                                                        37940000
$UNIT    DS    CL8                                                      37950000
DSNAIM   DS    H,CL44                                                   37960000
DSPLUS   DS    H,CL54                                                   37970000
CALLSMFA DS    F                                                        37980000
CALLPARM DS    4F                                                       37990000
CALLSAVE DS    10D                 18 WORD SAVE AREA PLUS 1 DOUBLE WORD 38000000
LOCATEW  DS    0F                                                       38010000
OBTAINW  DS    4F                                                       38020000
LOCBUF   DS    0D                  USES NEXT 265 BYTES                  38030000
MYDSCB   DS    CL140               96 BYTES OF DSCB, 5 BYTES CCHHR      38040000
MSG      DS    CL128                                                    38050000
STATUS   DS    X                                                        38060000
STALLOC  EQU   X'80'                                                    38070000
STOPEN   EQU   X'40'                                                    38080000
STABEND  EQU   X'20'                                                    38090000
STLOCAT  EQU   X'10'                                                    38100000
STGMVU   EQU   X'08'                                                    38110000
STRECV   EQU   X'04'                                                    38120000
STEOF    EQU   X'02'                                                    38130000
STNOMEM  EQU   X'01'                                                    38140000
MODE     DS    X                                                        38150000
MODEX    EQU   X'80'                                                    38160000
STSIZEX  EQU   X'20'                                                    38170000
SMFSW    DS    X                                                        38180000
SMFBIT   EQU   X'80'                                                    38190000
DEBUGSW  DS    X                                                    ABL 38200000
HELPFLG  DS    X                                                    ABL 38210000
DSORG    DS    X                                                        38220000
TSTRECFM DS    X                                                        38230000
FINDSW   DS    X                                                        38240000
RC       DS    H                                                        38250000
FILEKV   DS    H                                                        38260000
QUICKV   DS    H                                                        38270000
MYDFPARM DS    5F  USED BY DAIRFAIL                                     38280000
MYDFREGS DS    F   USED BY DAIRFAIL                                     38290000
MYDFRC   DS    F   USED BY DAIRFAIL                                     38300000
MYJEFF02 DS    F   USED BY DAIRFAIL                                     38310000
MYDFID   DS    H   USED BY DAIRFAIL                                     38320000
PAGESIZE DS    H                                                        38330000
PAGEWIDT DS    H                                                        38340000
DOUBLE   DS    D                                                        38350000
COLNUM   DS    CL6                                                      38360000
         DS    CL2                                                      38370000
DAIRREGS DS    F                                                        38380000
OPEND    DS    0F                                                       38390000
CLOSED   DS    F                                                        38400000
DYNEXLST DS    2F                                                       38410000
KOUNT    DS    F                                                        38420000
DYNDCBW  DS    0D,XL(SEQDCBL)                                           38430000
DYNDECBW DS    0F,XL(DYNDECBL)                                          38440000
RANGE    DS    2F                                                       38450000
ANSWER   DS    2F                                                       38460000
GMVUW    DS    0F,XL(GMVUL)                                             38470000
SAVSPANL DS    F                                                        38480000
SAVLRECL DS    F                                                        38490000
SAVBLKSI DS    F                                                        38500000
SAVHOLDL DS    F                                                        38510000
BLOCKPTR DS    F                                                        38520000
SPANPTR  DS    F                                                        38530000
HOLDPTR  DS    F                                                        38540000
CHKPTTOP DS    F                                                        38550000
CHKPTBOT DS    F                                                        38560000
CHKPTDUM DS    2F                                                       38570000
ENDPTR   DS    F                                                        38580000
HOLDTOP  DS    F                                                        38590000
HOLDEND  DS    F                                                        38600000
HOLDDIR  DS    (16*66)X    *** WAS 88F  -- GOOD FOR UP TO 65 LINES      38610000
DIRNUM   EQU   0                                                        38620000
DIRLEN   EQU   4                                                        38630000
DIRREC   EQU   8                                                        38640000
DIRNXT   EQU   12                                                       38650000
DIRSIZ   EQU   16                                                       38660000
OFFSET   DS    H                                                        38670000
RECSIZE  DS    H                                                        38680000
COUNT    DS    F                                                        38690000
TTR      DS    F                                                        38700000
TTRZ     DS    F                   TTR + 0 FOR POINT                    38710000
TTRK     DS    F                   TTR + CONCAT FOR FIND                38720000
SCROLL   DS    F                                                        38730000
DOWNAMT  DS    F                                                        38740000
LISTNUM  DS    F                                                        38750000
AIMFOR   DS    F                                                        38760000
FLDPTR   DS    F                                                        38770000
FLDLEN   DS    F                                                        38780000
CMDPTR   DS    F                                                        38790000
CMDLEN   DS    F                                                        38800000
CMDAREA  DS    CL63                                                     38810000
MSGDSN   DS    CL55                                                     38820000
SBASAVE  DS    4F                                                       38830000
FINDCOL  DS    H                                                        38840000
FNDNUM   DS    F                                                        38850000
FNDOFF   DS    H                                                        38860000
STRINGL  DS    H                                                        38870000
STRING   DS    CL64                                                     38880000
OPDL     DS    0F                  OPERAND DESCRIPTOR LIST              38890000
OPD0     DS    2F                  COMMAND DESCRIPTOR                   38900000
OPD1     DS    2F                  OPERAND DESCRIPTOR 1                 38910000
OPD2     DS    2F                  OPERAND DESCRIPTOR 2                 38920000
OPD3     DS    2F                  OPERAND DESCRIPTOR 3                 38930000
OPD4     DS    2F                  OPERAND DESCRIPTOR 4                 38940000
OPD5     DS    2F                  OPERAND DESCRIPTOR 5                 38950000
OPDLL    EQU   *-OPDL              LENGTH OF LIST                       38960000
PRESENT  EQU   X'80'                                                    38970000
QUOTED   EQU   X'40'                                                    38980000
HOLDR    DS    F                                                        38990000
READR    DS    F                                                        39000000
DOWNR    DS    F                                                        39010000
DEBLOCKS DS    3F                                                       39020000
SCREENF  DS    F                                                        39030000
TGETREGS DS    3F                                                       39040000
TGETREG2 DS    3F                                                   ABL 39050000
SYNADSW  DS    F                                                        39060000
SYNADMSG DS    CL78                                                     39070000
DEVDATA  DS    2F                                                       39080000
JFCB     DS    0F,CL176                                                 39090000
POOLSIZ  DS    F                                                        39100000
POOLLEN  DS    2F                                                       39110000
         DS    0D                                                       39120000
REPLY    DS    CL256                                                    39130000
PERIODS  DS    CL256                                                    39140000
SCREENH  DS    F                                                        39150000
BLDL     DS    0H,CL80                                                  39160000
SPACES   DS    0D,300C                                                  39170003
PFKTAB   DS    12CL64,CL1                                               39180000
PFKTABL  EQU   64                  LENGTH OF EACH ENTRY                 39190000
*NAPDW   DS    0F,(SNAPDL)X                                             39200000
*NAPLW   DS    0F,(SNAPLL)X                                             39210000
*NAPR    DS    4F                                                       39220000
SCREENW  DS    0D,5400C     *** WAS 3072C                               39230000
@DATAEND EQU   *                                                        39240000
@DATAL   EQU   *-@DATA                                                  39250000
         SPACE                                                          39260000
IHADCB   DSECT                                                          39270000
         DS    32XL1                                                    39280000
DCBBFTEK DS    XL1                                                      39290000
DCBEODAD DS    AL3                                                      39300000
DCBRECFM DS    X                                                        39310000
DCBEXLSA DS    AL3                                                      39320000
DCBDDNAM DS    CL8                                                      39330000
DCBOFLGS DS    X                                                        39340000
         DS    7XL1                                                     39350000
         DS    X                                                        39360000
DCBSYNAD DS    AL3                                                      39370000
DCBBLKSI EQU   IHADCB+62,2                                              39380000
DCBLRECL EQU   IHADCB+82,2                                              39390000
         SPACE                                                          39400000
         IKJCPPL                                                        39410000
         SPACE 3                                                        39420000
         IKJPPL                                                         39430000
         SPACE                                                          39440000
         IKJDFPB                                                        39450000
         SPACE 2                                                        39460000
         IKJUPT                                                         39470000
         SPACE 2                                                        39480000
         IKJIOPL                                                        39490000
         SPACE 2                                                        39500000
         IKJDAPL                                                        39510000
         SPACE 2                                                        39520000
         IKJDAP08                                                       39530000
         SPACE 2                                                        39540000
         IKJDAP18                                                       39550000
         SPACE 2                                                        39560000
         IKJPSCB                                                        39570000
         SPACE 2                                                        39580000
         IKJECT ,                                                   ABL 39590000
         SPACE 2                                                        39600000
QUOTE    EQU   X'7D'                                                    39610000
R0       EQU   0                                                        39620000
R1       EQU   1                                                        39630000
R2       EQU   2                                                        39640000
R3       EQU   3                                                        39650000
R4       EQU   4                                                        39660000
R5       EQU   5                                                        39670000
R6       EQU   6                                                        39680000
R7       EQU   7                                                        39690000
R8       EQU   8                                                        39700000
R9       EQU   9                                                        39710000
R10      EQU   10                                                       39720000
R11      EQU   11                                                       39730000
R12      EQU   12                                                       39740000
R13      EQU   13                                                       39750000
R14      EQU   14                                                       39760000
R15      EQU   15                                                       39770000
         END                                                            39780000
/*
//*
//*-----------------------------------------------------------------***
//*     Assemble REVSMF.                                            ***
//*-----------------------------------------------------------------***
//ASM2  EXEC  PGM=IFOX00,REGION=256K,
//             PARM=(DECK,NOOBJECT,NORLD,TERM,RENT,
//             'XREF(SHORT)')
//SYSLIB   DD  DSN=SYS1.MODGEN,DISP=SHR,DCB=BLKSIZE=6160
//         DD  DSN=SYS1.MACLIB,DISP=SHR
//SYSUT1   DD  UNIT=SYSDA,SPACE=(CYL,(10,5))
//SYSUT2   DD  UNIT=SYSDA,SPACE=(CYL,(10,5))
//SYSUT3   DD  UNIT=SYSDA,SPACE=(CYL,(10,5))
//SYSPUNCH DD  DISP=(OLD,PASS),DSN=&&OBJSET(REVSMF)
//SYSPRINT DD  SYSOUT=A
//SYSTERM  DD  SYSOUT=A
//SYSIN    DD  *
         TITLE '  R E V S M F   '                                       00270000
*********************************************************************** 00280000
*                                                                     * 00290000
*        REVSMF - A SUBROUTINE OF THE REVIEW TSO COMMAND              * 00300000
*                                                                     * 00310000
*********************************************************************** 00320000
*                                                                       00330000
*        WRITTEN BY. BILL GODFREY, PRC (PLANNING RESEARCH CORPORATION). 00340000
*        INSTALLATION. AIR FORCE DATA SERVICES CENTER, PENTAGON.        00350000
*        DATE WRITTEN. MARCH 18 1982.                                   00360000
*        DATE UPDATED. MARCH 18 1982.                                   00370000
*        ATTRIBUTES. RE-ENTRANT.                                        00380000
*        LOCAL MACROS USED. NONE.                                       00390000
*        DESCRIPTION.                                                   00400000
*         THIS SUBROUTINE FORMATS AN SMF RECORD INTO A LINE OF DATA     00410000
*         FOR THE SCREEN.                                               00420000
*                                                                       00430000
         SPACE                                                          00440000
REVSMF   START                                                          00450000
         USING *,R10                                                    00460000
         B     @PROLOG-*(,R15)                                          00470000
         DC    AL1(11),CL11'REVSMF '                                    00480000
         DC    CL16' &SYSDATE &SYSTIME '                                00490000
@SIZE    DC    0F'0',AL1(1),AL3(0)                                      00500000
@PROLOG  STM   14,12,12(13)                                             00510000
         LR    R10,R15             BASE                                 00520000
         LM    R1,R4,0(R1)         LENGTH, RECORD, OUTPUT, WORK         00530000
         USING @DATA,R4                                                 00540000
         LR    R6,R3                                                    00550000
FILLSMF  MVI   0(R6),C' '                                               00560000
         MVC   1(79,R6),0(R6)                                           00570000
         SR    R0,R0                                                    00580000
         IC    R0,1(,R2)           RECORD TYPE                          00590000
         CVD   R0,DOUBLE                                                00600000
         MVC   0(4,R6),=X'40202120'                                     00610000
         ED    0(4,R6),DOUBLE+6                                         00620000
         MVC   0(3,R6),1(R6)                                            00630000
         MVI   3(R6),C' '                                               00640000
         MVC   DOUBLE(4),2(R2)     TIME                                 00650000
         L     R1,DOUBLE                                                00660000
         LA    R15,4(,R6)                                               00670000
         BAL   R14,TIMEX                                                00680000
         UNPK  14(5,R6),7(3,R2)    DATE                                 00690000
         OI    18(R6),X'F0'                                             00700000
         MVC   13(2,R6),14(R6)                                          00710000
         MVI   15(R6),C'.'                                              00720000
         MVC   20(4,R6),10(R2)                                          00730000
         CLI   1(R2),70                                                 00740000
         BL    *+12                                                     00750000
         CLI   1(R2),75                                                 00760000
         BNH   TYPTR               TYPES 70-75                          00770000
         CLI   1(R2),0                                                  00780000
         BE    TYPTR                                                    00790000
         CLI   1(R2),9                                                  00800000
         BE    TYPTR                                                    00810000
         CLI   1(R2),21                                                 00820000
         BE    TYPTR                                                    00830000
         CLI   1(R2),47                                                 00840000
         BE    TYP47                                                    00850000
         CLI   1(R2),48                                                 00860000
         BNE   NOT47                                                    00870000
TYP47    MVC   34(16,R6),24(R2)                                         00880000
         B     TYPTR                                                    00890000
NOT47    EQU   *                                                        00900000
         MVC   25(8,R6),14(R2)     JOBNAME                              00910000
         CLI   1(R2),14                                                 00920000
         BE    TYP14                                                    00930000
         CLI   1(R2),15                                                 00940000
         BNE   NOT14                                                    00950000
TYP14    MVC   34(44,R6),64(R2)                                         00960000
         B     TYPTR                                                    00970000
NOT14    EQU   *                                                        00980000
TYPTR    EQU   *                                                        00990000
         LM    14,12,12(R13)                                            01000000
         BR    R14                                                      01010000
         SPACE                                                          01020000
************************************************************            01030000
*                                                          *            01040000
*         SUBROUTINE TO CONVERT TIME TO HH.MM.SS           *            01050000
*                                                          *            01060000
************************************************************            01070000
         SPACE                                                          01080000
*         INPUT: TIME IN R1, OUTPUT ADDRESS IN R15                      01090000
         SPACE                                                          01100000
TIMEX    SLR   R0,R0                                                    01110000
         D     R0,=F'360000'                                            01120000
         CVD   R1,DOUBLE                                                01130000
         OI    DOUBLE+7,X'0F'                                           01140000
         UNPK  0(2,R15),DOUBLE+6(2)                                     01150000
         LR    R1,R0                                                    01160000
         SLR   R0,R0                                                    01170000
         D     R0,=F'6000'                                              01180000
         CVD   R1,DOUBLE                                                01190000
         OI    DOUBLE+7,X'0F'                                           01200000
         UNPK  3(2,R15),DOUBLE+6(2)                                     01210000
         LR    R1,R0                                                    01220000
         SLR   R0,R0                                                    01230000
         D     R0,=F'100'                                               01240000
         CVD   R1,DOUBLE                                                01250000
         OI    DOUBLE+7,X'0F'                                           01260000
         UNPK  6(2,R15),DOUBLE+6(2)                                     01270000
         MVI   2(R15),C'.'                                              01280000
         MVI   5(R15),C'.'                                              01290000
         BR    R14                                                      01300000
         SPACE                                                          01310000
************************************************************            01320000
*                                                          *            01330000
*        CONSTANTS                                         *            01340000
*                                                          *            01350000
************************************************************            01360000
         SPACE                                                          01370000
         LTORG                                                          01380000
         SPACE                                                          01390000
         DC    0D'0'               END OF CSECT                         01400000
         SPACE                                                          01410000
************************************************************            01420000
*                                                          *            01430000
*        DSECTS                                            *            01440000
*                                                          *            01450000
************************************************************            01460000
         SPACE                                                          01470000
@DATA    DSECT                                                          01480000
         DS    18F                 REGISTER SAVEAREA                    01490000
DOUBLE   DS    D                   SIZE OF THIS AREA                    01500000
         SPACE                                                          01510000
R0       EQU   0                                                        01520000
R1       EQU   1                                                        01530000
R2       EQU   2                                                        01540000
R3       EQU   3                                                        01550000
R4       EQU   4                                                        01560000
R5       EQU   5                                                        01570000
R6       EQU   6                                                        01580000
R7       EQU   7                                                        01590000
R8       EQU   8                                                        01600000
R9       EQU   9                                                        01610000
R10      EQU   10                                                       01620000
R11      EQU   11                                                       01630000
R12      EQU   12                                                       01640000
R13      EQU   13                                                       01650000
R14      EQU   14                                                       01660000
R15      EQU   15                                                       01670000
         END                                                            01680000
/*
//*
//*-----------------------------------------------------------------***
//*     Link REVIEW and REVSMF into REVIEW load module.             ***
//*-----------------------------------------------------------------***
//LKED    EXEC PGM=IEWL,PARM='MAP,RENT,REUS,REFR',COND=(5,LT)
//SYSPRINT DD  SYSOUT=A
//SYSLMOD  DD  DISP=SHR,DSN=SYS2.LINKLIB
//SYSPUNCH DD  DISP=(OLD,PASS),DSN=&&OBJSET
//SYSUT1   DD  UNIT=SYSDA,SPACE=(TRK,(5,5))
//SYSLIN   DD  *
 INCLUDE SYSPUNCH(REVIEW)
 INCLUDE SYSPUNCH(REVSMF)
 ENTRY REVIEW
 ALIAS REV
 NAME REVIEW(R)
//
