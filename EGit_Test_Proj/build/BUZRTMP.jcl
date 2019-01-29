//BUZRJCL  JOB CLASS=A,MSGCLASS=Y,NOTIFY=&SYSUID,REGION=0M
//*********************************************************************
//* CREATE UCD COMPONENT VERSION
//*
//* SH /opt/ibm-ucd/agent/bin/buztool.sh createzosversion
//*       "-c" "Louis_zOS_DB2"
//*       "-s" "/u/lock/jenkins/EGit_Test_Proj/build/shiplist.xml"
//*       "-v" ${BUILD_TIMESTAMP}
//*********************************************************************
//BUZTOOL  EXEC PGM=BPXBATCH,REGION=0M
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//STDPARM  DD *
SH /opt/ibm-ucd/agent/bin/buztool.sh createzosversion
       "-c" "Louis_zOS_DB2"
       "-s" "/u/lock/jenkins/EGit_Test_Proj/build/shiplist.xml"
       "-v" @BUILD_TIMESTAMP@
/*