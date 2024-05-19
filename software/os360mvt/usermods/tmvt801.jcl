//SY20ZR13 JOB 1,TMVT801,CLASS=A,MSGLEVEL=(1,1)
//*
//*  PROBLEM DESCRIPTION(S):
//*    TMVT801 -
//*      Modify IEAANIP (NIP stage 2 sysgen macro) to:
//*      *  Increase base master scheduler region size
//*         from 12K to 18K
//*      *  Restrict SMARTNIP processing to tape and DASD
//*      *  Skip location 80 timer working check
//*      *  Fix storage initialization problem with 16MB of storage
//*      *  Fix SYS1.DUMP initialization problem with 16MB of storage
//*      *  Add IEFSD061 (step termination) and IEFSD062 (step
//*         initiation) and their aliases to default LPA list
//*      *  Force default if "RAM=" not specified at IPL
//*         to be "RAM=00,11" instead of "RAM=00" (read both
//*         IEAIGG00 and IEAIGG11 for resident routine names).
//*
//*   COMPONENT:  360S-CI505-EBB2218
//*
//*   APARS FIXED: TMVT801
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
//*       IEAANIP
//*
//TMVT801  EXEC PGM=IEBUPDTE,REGION=128K,PARM=MOD
//SYSPRINT  DD SYSOUT=A
//SYSUT1    DD DISP=SHR,DSN=SYS1.MODGEN2
//SYSUT2    DD DISP=SHR,DSN=SYS1.MODGEN2
//SYSIN     DD DATA
./ CHANGE NAME=IEAANIP
IEAMSLNT EQU   9 -                 Master scheduler region     @TMVT801 02604020
*                                  base size (9 * 2K = 18K)    @TMVT801 02605020
         CLI   UCBTYP+2(R9),X80 -  Is device tape?             @TMVT801 12490200
         BNE   IEAUCB1 -           Skip SMARTNIP if not tape   @TMVT801 12490400
         NOP   TMRNOPR -           Skip timer-not-working code @TMVT801 14154020
         LTR   9,9 -            WRAPAROUND?                    @TMVT801 14345020
         BC    8,IEASETKX -     YES, 16 MB DONE                @TMVT801 14347020
IEASETKX EQU   * -                                             @TMVT801 14360020
         LA    R4,1 -                   Add one                @TMVT801 25641020
         AR    R5,R4 -                   to find core size     @TMVT801 25644020
         BNE   IEASOFIX -               Yes, suffixes supplied @TMVT801 43358020
         MVC   IEARAMLS(4),IEARMDEF -   Else set default 00,11 @TMVT801 43359299
         B     IEASOFIX -               Go process             @TMVT801 43359499
IEARMDEF DC    C'0011' -                Default RAM suffixes   @TMVT801 43359699
*        Start of LINKLIB modules added to LPA by TMVT801      @TMVT801 84674099
         DC    CL8'IEFSD061' -     Step termination            @TMVT801 84674299
         DC    CL8'IEFSD064' -     Step termination alias      @TMVT801 84674499
         DC    CL8'IEFSD104' -     Step termination alias      @TMVT801 84674699
         DC    CL8'IEFW42SD' -     Step termination alias      @TMVT801 84674899
         DC    CL8'IEFSD062' -     Step initiation             @TMVT801 84675099
         DC    CL8'IEFV4221' -     Step initiation alias       @TMVT801 84675299
*        End of LINKLIB modules added to LPA by TMVT801        @TMVT801 84675499
./ ENDUP
/*
//
