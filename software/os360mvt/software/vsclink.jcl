//TEC31660 JOB 'LINK VSCOPY',CLASS=A,MSGLEVEL=(1,1)
//STEP1   EXEC PGM=IEWL,PARM='LIST,LET,XREF',REGION=256K
//SYSPRINT DD  SYSOUT=A
//SYSUT1   DD  UNIT=SYSDA,SPACE=(TRK,(15,5))
//SYSLIN   DD  DSN=VSCOPY.OBJ,DISP=OLD,UNIT=280,VOL=SER=JAY001
//SYSLMOD  DD  DSN=&&GOSET(VSCOPY),UNIT=SYSDA,DISP=(NEW,PASS,DELETE),
//             SPACE=(TRK,(10,10,1))
//STEP2   EXEC PGM=IEBCOPY,REGION=256K
//SYSPRINT DD  SYSOUT=A
//IN       DD  DSN=*.STEP1.SYSLMOD,DISP=(OLD,PASS)
//OUT1     DD  DSN=SYS1.LINKLIB,UNIT=3330,VOL=SER=MVTRES,DISP=SHR
//OUT2     DD  DSN=SYS1.SVCLIB,UNIT=3330,VOL=SER=MVTRES,DISP=SHR
//SYSUT3   DD  UNIT=SYSDA,SPACE=(80,(60,45)),DISP=(NEW,DELETE)
//SYSIN    DD  *
  COPY INDD=IN,OUTDD=OUT1
  SELECT MEMBER=(VSCOPY)
  COPY INDD=IN,OUTDD=OUT2
  SELECT MEMBER=(IGG019C9)
/*
//