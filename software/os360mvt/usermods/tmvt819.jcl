//SY20ZR19 JOB 1,'TMVT819',CLASS=A,MSGCLASS=A
//*
//*++ USERMOD(TMVT819)     /* REWORK(20100715) */             .
//*++ VER (S218)
//*   FMID(EBB2218)
//*   PRE  (US04999,US06401,US06958)
//* /*
//*   PROBLEM DESCRIPTION(S):
//*     TMVT819 -
//*       Unable to log on to TSO if time sharing region extends
//*       above 8 MB (X'800000').  Logon attempts fail with
//*       msgIKJ601I, msgIEA701I, msgIEA702I, abend305-1,
//*       abend30A-3, abend806-1 and other errors.  After logon
//*       failure, the TSO time sharing controller may be hung
//*       and will not shut down.
//*
//*       TSO region control task quiesce routine IKJEAR02 saves
//*       time sharing region storage allocation address and
//*       length information for a user being swapped out in the
//*       user storage map (UMSM).  Each 24-bit address and length
//*       is divided by 256 (shifted right eight bits).  The
//*       resulting 16-bit value is stored into a two-byte entry
//*       in the UMSM; for each allocation address/length pair,
//*       there is a pair of two-byte UMSM entries.
//*
//*       When a TSO user is swapped back in, the region control
//*       task restore routine IKJEAR03 rebuilds FBQEs from free
//*       space information saved in the UMSM.  Each two-byte UMSM
//*       entry is retrieved by IKJEAR03 using a load halfword
//*       (LH) instruction.  This is not a problem as long as
//*       addresses are less than X'800000'.  Once an address is
//*       X'800000' or greater, however, the two-byte value saved
//*       in the UMSM becomes X'8000' or greater, which looks to
//*       LH like a negative number.  As a result, the presumed
//*       "sign" bit is propagated leftward, so a value that
//*       should have been X'00008000' becomes X'FFFF8000'.  This
//*       value is shifted left eight bits to restore the original
//*       24 bit address, becoming X'FF800000'.  Address
//*       references using the reconstructed value produce the
//*       expected result, but arithmetic operations that assume
//*       X'00' in the high-order byte generate incorrect output.
//*       Reconstructed main storage supervision control blocks
//*       built with arithmetic using the defective values are
//*       invalid, leading to the abend305 and other main storage
//*       supervision errors.
//*
//*       When a time sharing region is started, the region
//*       controller attaches a copy of the logon scheduler and
//*       swaps out the the region as a logon image, which is used
//*       to initialize the region when a new user attempts to log
//*       on.  Invalid MSS control blocks built during restore of
//*       this logon image result in the abend305 during logon.
//*
//*       In addition to IKJEAR03, IKJEAS02 (swap mainline) and
//*       IKJEAT07 (out of core abnormal termination) also use
//*       LH to retrieve UMSM fields without zeroing the high-order
//*       byte of the resulting 24-bit value.
//*
//*       IKJEAR03 (restore), IKJEAS02 (swap mainline) and
//*       IKJEAT07 (out of core abnormal termination) are changed
//*       to ensure that spurrious sign propagation resulting from
//*       retrieving unsigned two-byte UMSM fields using LH is
//*       prevented by zeroing the high-order bits of resulting
//*       values.
//*
//*   COMPONENT:  360S-CI555-EBB2218
//*
//*   APARS FIXED: TMVT819
//*
//*   SPECIAL CONDITIONS:
//*     ACTION:  The TSO time sharing controller region must be
//*       stopped and restarted after installation of this user
//*       modification.
//*
//*     DEPENDENCY:
//*       The OS/VS linkage editor HEWLF064 must be available
//*       to permit IKJEAR03, IKJEAS02 and IKJEAT07 to be expanded
//*       with a patch area.
//*
//*   COMMENTS:
//*     LAST CHANGE:  2010/07/15
//*
//*     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:
//*
//*     MODULES
//*       IKJEAR03
//*       IKJEAS02
//*       IKJEAT07
//*  */.
//*
//*-----------------------------------------------------------------***
//*     Step 1:  Relink IKJEAT03 with OS/VS linkage editor to       ***
//*              add patch areas to IKJEAR03, IKJEAS02 and          ***
//*              IKJEAT07.                                          ***
//*-----------------------------------------------------------------***
//LKED1  EXEC PGM=HEWLF064,
//            PARM=(NCAL,LIST,XREF,LET,RENT,REFR),
//            REGION=128K
//LINKLIB  DD DISP=SHR,DSN=SYS1.LINKLIB
//SYSLMOD  DD DSN=&&TEMP,
//            DISP=(NEW,PASS),
//            SPACE=(TRK,(20,20,5)),
//            UNIT=SYSALLDA
//SYSUT1   DD DSN=&&SYSUT1,UNIT=SYSDA,SPACE=(1024,(50,20))
//SYSPRINT DD SYSOUT=A
//SYSLIN   DD *
  INCLUDE LINKLIB(IKJEAT03)
  ALIAS IKJEAT04,IKJEAT05,IKJEAT06,IKJEAT07,IKJEAT08
  ALIAS IKJEAI00,IKJEAI01,IKJEAI02,IKJEAI03,IKJEAR00
  ALIAS IKJEAR01
  EXPAND IKJEAR03(56)             /* ADD SPACE FOR PATCH AREA */
  EXPAND IKJEAS02(28)             /* ADD SPACE FOR PATCH AREA */
  EXPAND IKJEAT07(14)             /* ADD SPACE FOR PATCH AREA */
  ENTRY IKJEAT03
  NAME IKJEAT03(R)
/*
//*
//*-----------------------------------------------------------------***
//*     Step 2:  Execute OS linkage editor to relink IKJEAT03       ***
//*              to SYS1.LINKLIB after CSECTs have been expanded.   ***
//*-----------------------------------------------------------------***
//LKED2  EXEC PGM=IEWL,COND=(0,NE),
//            PARM=(NCAL,LIST,XREF,LET,RENT,REFR),
//            REGION=128K
//SYSPUNCH DD DISP=(OLD,DELETE),DSN=&&TEMP
//SYSLMOD  DD DISP=SHR,DSN=SYS1.LINKLIB
//SYSUT1   DD DSN=&&SYSUT1,UNIT=SYSDA,SPACE=(1024,(50,20))
//SYSPRINT DD SYSOUT=A
//SYSLIN   DD *
  INCLUDE SYSPUNCH(IKJEAT03)
  ALIAS IKJEAT04,IKJEAT05,IKJEAT06,IKJEAT07,IKJEAT08
  ALIAS IKJEAI00,IKJEAI01,IKJEAI02,IKJEAI03,IKJEAR00
  ALIAS IKJEAR01
  ENTRY IKJEAT03
  NAME IKJEAT03(R)
/*
//*
//*-----------------------------------------------------------------***
//*     Step 3:  ZAP expanded IKJEAT03 CSECTs with fix.             ***
//*-----------------------------------------------------------------***
//ZAP    EXEC PGM=IMASPZAP,COND=(0,NE)
//SYSLIB   DD DISP=SHR,DSN=*.LKED2.SYSLMOD,VOL=REF=*.LKED2.SYSLMOD
//SYSPRINT DD SYSOUT=A
//SYSIN    DD *
NAME IKJEAT03 IKJEAR03
IDRDATA TMVT819
VER 000000 05A0                      BALR  BASEREG,ZEROREG     Establish address
VER 000084 4810,7000        REPPED1  LH    SWORK1,UMSMBG       Compute address o
VER 00008C 4820,7002        REPPED2  LH    SWORK2,UMSMLN       Get area length f
VER 0000C2 4827,0004        REPPED3  LH    SWORK2,D4(UMSMREG)  Addr next non-swa
VER 0000DA 4810,7000        REPPED4  LH    SWORK1,UMSMBG       Get area addr fro
VER 0000E2 4820,7002        REPPED5  LH    SWORK2,UMSMLN       Get length from m
VER 00088C 0000000000000000          DC    (PATCHLEN)X'00'     Verify patch area
VER 000894 0000000000000000
VER 00089C 0000000000000000
VER 0008A4 0000000000000000
VER 0008AC 0000000000000000
VER 0008B4 0000000000000000
VER 0008BC 0000
REP 000084 4530,A88E                 BAL   R3,PATCH1           To patch area
REP 00008C 4530,A898                 BAL   R3,PATCH2           To patch area
REP 0000C2 4530,A8A2                 BAL   R3,PATCH3           To patch area
REP 0000DA 4530,A8AC                 BAL   R3,PATCH4           To patch area
REP 0000E2 4530,A8B6                 BAL   R3,PATCH5           To patch area
REP 00088C 0000FFFF         MASK16   DC    0F'0',X'0000FFFF'   Mask to turn off
REP 000890 4810,7000        PATCH1   LH    SWORK1,UMSMBG       Compute address o
REP 000894 5410,A88A                 N     SWORK1,MASK16       Lift high-order b
REP 000898 07F3                      BR    R3                  Back to mainline
REP 00089A 4820,7002        PATCH2   LH    SWORK2,UMSMLN       Get area length f
REP 00089E 5420,A88A                 N     SWORK2,MASK16       Lift high-order b
REP 0008A2 07F3                      BR    R3                  Back to mainline
REP 0008A4 4827,0004        PATCH3   LH    SWORK2,D4(UMSMREG)  Addr next non-swa
REP 0008A8 5420,A88A                 N     SWORK2,MASK16       Lift high-order b
REP 0008AC 07F3                      BR    R3                  Back to mainline
REP 0008AE 4810,7000        PATCH4   LH    SWORK1,UMSMBG       Get area addr fro
REP 0008B2 5410,A88A                 N     SWORK1,MASK16       Lift high-order b
REP 0008B6 07F3                      BR    R3                  Back to mainline
REP 0008B8 4820,7002        PATCH5   LH    SWORK2,UMSMLN       Get length from m
REP 0008BC 5420,A88A                 N     SWORK2,MASK16       Lift high-order b
REP 0008C0 07F3                      BR    R3                  Back to mainline
NAME IKJEAT03 IKJEAS02
IDRDATA TMVT819
VER 000004 05C0                      BALR  BASER,0             Load BASE1 addres
VER 0003F0 4830,2002        SSSUM01  LH    WR3,UMSMLN          Len from UMSM ent
VER 0003F4 1233                      LTR   WR3,WR3             Len=0, last entry
VER 0003F6 078B                      BCR   8,TRANR             Zero, done
VER 0003F8 8930,0008                 SLL   WR3,E8              Convert to S/360
VER 0003FE 4840,2000        REPPED2  LH    WR4,UMSMBG          Addr. from UMSM e
VER 000402 8940,0008                 SLL   WR4,E8              Convert to S/360
VER 00041C 4830,2002        SSUM10A  LH    WR3,UMSMLN          UMSM entry length
VER 000420 8930,0008                 SLL   WR3,E8              Convert to bytes
VER 000424 4840,2000        REPPED4  LH    WR4,UMSMBG          UMSM entry addres
VER 000428 8940,0008                 SLL   WR4,E8              Convert to 24-bit
VER 0012E0 0000000000000000          DC    (PATCHLEN)X'00'     Verify patch area
VER 0012E8 0000000000000000
VER 0012F0 0000000000000000
VER 0012F8 00000000
REP 0003F0 05E0                      BALR  WR14,0              Temporary address
REP 0003F2 45E0,EEEE                 BAL   WR14,PATCH3L        To patch area
REP 0003F6 1233                      LTR   WR3,WR3             Len=0, last entry
REP 0003F8 078B                      BCR   8,TRANR             Zero, done
REP 0003FA 0700                      NOPR  0                   No op
REP 0003FE 05E0                      BALR  WR14,0              Temporary address
REP 000400 45E0,EEEE                 BAL   WR14,PATCH4B        To patch area
REP 000404 0700                      NOPR  0                   No op
REP 00041C 05E0                      BALR  WR14,0              Temporary address
REP 00041E 45E0,EEC2                 BAL   WR14,PATCH3L        To patch area
REP 000422 0700                      NOPR  0                   No op
REP 000424 05E0                      BALR  WR14,0              Temporary address
REP 000426 45E0,EEC8                 BAL   WR14,PATCH4B        To patch area
REP 00042A 0700                      NOPR  0                   No op
REP 0012E0 4830,2002        PATCH3L  LH    WR3,UMSMLN          Len from UMSM ent
REP 0012E4 8930,0010                 SLL   WR3,16              Rebuild 24-bit va
REP 0012E8 8830,0008                 SRL   WR3,8                with high bits z
REP 0012EC 07FE                      BR    WR14                Back to mainline
REP 0012EE 4840,2000        PATCH4B  LH    WR4,UMSMBG          Addr from UMSM en
REP 0012F2 8940,0010                 SLL   WR4,16              Rebuild 24-bit va
REP 0012F6 8840,0008                 SRL   WR4,8                with high bits z
REP 0012FA 07FE                      BR    WR14                Back to mainline
NAME IKJEAT03 IKJEAT07
IDRDATA TMVT819
VER 000004 05A0                      BALR  BASEREG,ZEROREG     Establish address
VER 000ECA 4870,6002        REPPED1  LH    WR3,UMSMLN          Len from UMSM ent
VER 000ED4 8970,0008        REPPED2  SLL   WR3,E8              Convert to 24-bit
VER 000FDC 0000000000000000          DC    (PATCHLEN)X'00'     Verify patch area
VER 000FE4 000000000000
REP 000ECA 45E0,AFD6                 BAL   R14,PATCH1          To patch area
REP 000ED4 4700,0000                 NOP   0                   No op
REP 000FDC 4870,6002        PATCH1   LH    WR3,UMSMLN          Len from UMSM ent
REP 000FE0 8970,0010                 SLL   WR3,16              Rebuild 24-bit va
REP 000FE4 8870,0008                 SRL   WR3,8                with high bits z
REP 000FE8 07FE                      BR    R14                 Back to mainline
/*
//
