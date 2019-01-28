//OCOPY JOB CLASS=A,MSGCLASS=Y,NOTIFY=&SYSUID,REGION=0M
//*********************************************************************
//* COPY USS FILES TO MVS WITH ASCII TO EBCDIC CONVERSION
//*********************************************************************
//* USE THE FOLLOWING OCOPY OPTION IF ASCII TO EBCDIC IS NEEDED:
//*
//* OCOPY INDD(INHFS) OUTDD(OUTMVS) TEXT CONVERT((BPXFX311)) TO1047
//*
//*********************************************************************
//OCOPY PROC INMEM=,OUTMEM=,TYPE=
//*
//COPYSTP  EXEC PGM=IKJEFT01
//INHFS    DD PATH='/u/lock/jenkins/EGit_Test_Proj/src/&INMEM'
//OUTMVS   DD DSN=LOCK.JENKINS.&TYPE(&OUTMEM),DISP=OLD
//SYSTSPRT DD SYSOUT=*
//SYSTSIN  DD *
OCOPY INDD(INHFS) OUTDD(OUTMVS) TEXT CONVERT(NO)
/*
//OCOPY PEND
//S01 EXEC OCOPY,INMEM='DB2CBLEX.cbl',OUTMEM='DB2CBLEX',TYPE='COBOL'
//S02 EXEC OCOPY,INMEM='DCLEMP.cpy',OUTMEM='DCLEMP',TYPE='COPYLIB'
//S03 EXEC OCOPY,INMEM='REPORT.cpy',OUTMEM='REPORT',TYPE='COPYLIB'