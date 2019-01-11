//DSNHICOB JOB CLASS=A,MSGCLASS=Y,NOTIFY=&SYSUID,REGION=0M
//********************************************************************
//*        DSNHICOB  - DB2 precompile, IBM COBOL compile, pre-link,  *
//*                    and link edit a DB2 SQL program.              *
//********************************************************************
//DSNHICOB PROC WSPC=500,MEM=TEMPNAME,USER=USER
//********************************************************************
//*        Precompile the IBM COBOL program                          *
//********************************************************************
//PC       EXEC PGM=DSNHPC,PARM='HOST(IBMCOB)'
//*PC       EXEC PGM=DSNHPC,
//*         PARM=('HOST(IBMCOB)',APOST,APOSTSQL,SOURCE,NOXREF,
//*         'SQL(DB2)','DEC(31)')
//DBRMLIB  DD  DISP=SHR,
//             DSN=&USER..JENKINS.DBRM(&MEM)
//STEPLIB  DD  DISP=SHR,DSN=DSNEC30.SDSNEXIT
//         DD  DISP=SHR,DSN=DSNB10.SDSNLOAD
//SYSCIN   DD  DSN=&&DSNHOUT,DISP=(MOD,PASS),UNIT=SYSDA,
//             SPACE=(800,(&WSPC,&WSPC))
//SYSIN    DD  DISP=SHR,DSN=&USER..JENKINS.COBOL(&MEM)
//SYSLIB   DD  DISP=SHR,DSN=&USER..JENKINS.COPYLIB           **DCLGEN**
//SYSPRINT DD  SYSOUT=*
//SYSTERM  DD  SYSOUT=*
//SYSUDUMP DD  SYSOUT=*
//SYSUT1   DD  SPACE=(800,(&WSPC,&WSPC),,,ROUND),UNIT=SYSDA
//SYSUT2   DD  SPACE=(800,(&WSPC,&WSPC),,,ROUND),UNIT=SYSDA
//********************************************************************
//*        Compile the IBM COBOL program if the precompile           *
//*        return code is 4 or less.                                 *
//********************************************************************
//COB      EXEC PGM=IGYCRCTL,
//             PARM=(NOSEQUENCE,QUOTE,RENT,'PGMNAME(LONGUPPER)'),
//             COND=(4,LT,PC)
//SYSMDECK DD  SYSOUT=*                                      **KENJI**
//SYSLIB   DD  DISP=SHR,DSN=&USER..JENKINS.COPYLIB         **COPYBOOK**
//SYSPRINT DD  SYSOUT=*
//SYSTERM  DD  SYSOUT=*
//SYSLIN   DD  DSN=&&LOADSET,DISP=(MOD,PASS),UNIT=SYSDA,
//             SPACE=(800,(&WSPC,&WSPC))
//SYSIN    DD  DSN=&&DSNHOUT,DISP=(OLD,DELETE)
//SYSUT1   DD  SPACE=(800,(&WSPC,&WSPC),,,ROUND),UNIT=SYSDA
//SYSUT2   DD  SPACE=(800,(&WSPC,&WSPC),,,ROUND),UNIT=SYSDA
//SYSUT3   DD  SPACE=(800,(&WSPC,&WSPC),,,ROUND),UNIT=SYSDA
//SYSUT4   DD  SPACE=(800,(&WSPC,&WSPC),,,ROUND),UNIT=SYSDA
//SYSUT5   DD  SPACE=(800,(&WSPC,&WSPC),,,ROUND),UNIT=SYSDA
//SYSUT6   DD  SPACE=(800,(&WSPC,&WSPC),,,ROUND),UNIT=SYSDA
//SYSUT7   DD  SPACE=(800,(&WSPC,&WSPC),,,ROUND),UNIT=SYSDA
//SYSUT8   DD  SPACE=(800,(&WSPC,&WSPC),,,ROUND),UNIT=SYSDA
//SYSUT9   DD  SPACE=(800,(&WSPC,&WSPC),,,ROUND),UNIT=SYSDA
//SYSUT10  DD  SPACE=(800,(&WSPC,&WSPC),,,ROUND),UNIT=SYSDA
//SYSUT11  DD  SPACE=(800,(&WSPC,&WSPC),,,ROUND),UNIT=SYSDA
//SYSUT12  DD  SPACE=(800,(&WSPC,&WSPC),,,ROUND),UNIT=SYSDA
//SYSUT13  DD  SPACE=(800,(&WSPC,&WSPC),,,ROUND),UNIT=SYSDA
//********************************************************************
//*  PRELINK STEP.                                                   *
//********************************************************************
//PLKED   EXEC PGM=EDCPRLK,COND=((4,LT,PC),(4,LT,COB))
//STEPLIB  DD  DISP=SHR,DSN=CEE.SCEERUN
//SYSMSGS  DD  DISP=SHR,
//             DSN=CEE.SCEEMSGP(EDCPMSGE)
//SYSIN    DD  DSN=&&LOADSET,DISP=(OLD,DELETE)
//SYSMOD   DD  DSN=&&PLKSET,UNIT=SYSDA,DISP=(MOD,PASS),
//             SPACE=(32000,(30,30)),
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=3200)
//SYSDEFSD DD  DUMMY
//SYSOUT   DD  SYSOUT=*
//SYSPRINT DD  SYSOUT=*
//SYSTERM  DD  SYSOUT=*
//********************************************************************
//*        Linkedit if the precompile and compile                    *
//*        return codes are 4 or less.                               *
//********************************************************************
//LKED     EXEC PGM=IEWL,PARM='MAP',
//*         PARM='LIST,XREF,MAP,RENT',
//*        PARM='LET,LIST,XREF,SIZE=(6144K,128K)',
//         COND=((4,LT,PC),(4,LT,COB),(4,LT,PLKED))
//SYSLIB   DD  DISP=SHR,DSN=CEE.SCEELKED
//         DD  DISP=SHR,DSN=DSNB10.SDSNLOAD
//*        DD  DISP=SHR,DSN=IMSVS.RESLIB
//*        DD  DISP=SHR,DSN=CICSTS.SDFHLOAD
//         DD  DISP=SHR,DSN=ISP.SISPLOAD
//         DD  DISP=SHR,DSN=GDDM.SADMMOD
//SYSLIN   DD  DSN=&&PLKSET,DISP=(OLD,DELETE)
//         DD  DDNAME=SYSIN
//SYSLMOD  DD  DSN=&USER..JENKINS.LOAD(&MEM),
//             DISP=OLD
//*            DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  SPACE=(1024,(50,50)),UNIT=SYSDA
//********************************************************************
//DSNHICOB PEND
//S01 EXEC DSNHICOB,MEM=DB2CBLEX,USER='${USER}'