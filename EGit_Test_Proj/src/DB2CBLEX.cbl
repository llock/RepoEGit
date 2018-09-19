      ******************************************************************00010026
      *   DO NOT REMOVE.  CHAMP LINK CONTROL STATEMENTS.                00020026
      ******************************************************************00030026
      * STARTOPT:                                                       00040026
      * DB2OEXP: YES                                                    00050026
      * DB2OISO: UR                                                     00060026
      * ENDOPT:                                                         00070026
      ******************************************************************00080026
       IDENTIFICATION DIVISION.                                         00090026
       PROGRAM-ID. DB2CBLEX.                                            00100026
      *    AUTHOR. R. WAGNER                                            00110026
      *                                                                 00120026
      *    OWNER:                                                       00130026
      *                                                                 00140026
      *    JOB NUMBER(S):                                               00150026
      *                                                                 00160026
      *REMARKS.                                                         00170026
      *                                                                 00180026
      *                                                                 00190026
      *                                                                 00200026
      *  INPUT PARMS:  NONE                                             00210026
      *                                                                 00220026
      *  OUTPUT PARMS: NONE                                             00230026
      *                                                                 00240026
      *  INPUT FILES:  NONE                                             00250026
      *                                                                 00260026
      *  OUTPUT FILES: ALL EMPLOYEE RECORDS                             00270026
      *                                                                 00280026
      *  COPY MEMBERS:                                                  00290026
      *                DCLEMP                                           00300026
      *                REPORT                                           00300026
      *    TABLES:                                                      00310026
      *            DSN8110.EMP                                          00320026
      *    SWITCHES:                                                    00330026
      *                                                                 00340026
      *                                                                 00350026
      *    EXITS:                                                       00360026
      *                                                                 00370026
      *      NORMAL:                                                    00380026
      *             WHEN A END OF TABLE FETCH RETURN CODE IS RECEIVED   00390026
      *                                                                 00400026
      *      ABNORMAL:                                                  00410026
      *                                                                 00420026
      *    RETURN CODES:                                                00430026
      *                                                                 00440026
      *    SPECIAL LOGIC:  NONE                                         00450026
      *                                                                 00460026
      ******************************************************************00500026
      ***             P R O G R A M  C H A N G E  L O G                *00510026
      ******************************************************************00520026
      *  CHANGED BY:                                  DATE:            *00530026
      *                                                                *00540026
      *  LOUIS - ADDED REPORT HEADER                  2017-08-18       *00540126
      *  LOUIS - TURNED NUM ON                        2017-08-29       *00540226
      *  LOUIS - REMOVED REDUNDANT LOG                2017-08-30       *00540326
      *  LOUIS - ADDED A COPYBOOK FOR REPORT          2017-09-01       *00540426
      *  LOUIS - MOVED DCLGEN TO A COPYBOOK           2017-09-07       *00540526
      *  LOUIS - CHANGED COPYBOOK REPORT FOR SALARY   2017-09-22       *00540632
      *  LOUIS - ADDED A REPORT TITLE                 2017-11-14       *00540733
      *  LOUIS - ADDED ORDER BY TO DB2 CURSOR         2018-02-16       *00540733
      *  LOUIS - CHANGED ORDER BY SALARY              2018-03-05       *00540733
      *  LOUIS - CHANGED ORDER BY LASTNAME            2018-03-12       *00540733
      *  LOUIS - CHANGED ORDER BY SALARY              2018-08-22       *00540733
      *  LOUIS - CHANGED ORDER BY FIRSTNME            2018-08-24       *00540733
      *  LOUIS - CHANGED ORDER BY LASTNAME            2018-08-24       *00540733
      *  LOUIS - CHANGED ORDER BY EMPNO               2018-08-27       *00540733
      *  LOUIS - CHANGED ORDER BY WORKDEPT            2018-08-27       *00540733
      *  ******* UCD-ROLLED BACK TO AUG22 SNAPSHOT *************       *00540733
      *  LOUIS - CHANGED ORDER BY FIRSTNME            2018-09-18       *00540733
      *  LOUIS - CHANGED ORDER BY LASTNAME            2018-09-19       *00540733
      *                                                                *00540826
      ******************************************************************00550026
      ***           E N D  P R O G R A M  C H A N G E  L O G           *00560026
      ******************************************************************00570026
      /                                                                 00580026
       ENVIRONMENT DIVISION.                                            00590026
       CONFIGURATION SECTION.                                           00600026
                                                                        00610026
       INPUT-OUTPUT SECTION.                                            00620026
                                                                        00630026
       FILE-CONTROL.                                                    00640026
                                                                        00650026
           SELECT REPORT-FILE         ASSIGN TO RPTO0010.               00660026
                                                                        00670026
       DATA DIVISION.                                                   00680026
                                                                        00690026
       FILE SECTION.                                                    00700026
                                                                        00710026
                                                                        00720026
      ******************************************************************00730026
      * FILE:  REPORT-FILE                        DDNAME - GPSO0010    *00740026
      *                                                                *00750026
      ******************************************************************00760026
                                                                        00770026
       FD  REPORT-FILE                                                  00780026
           LABEL RECORDS ARE STANDARD                                   00790026
           RECORDING MODE IS F                                          00800026
           BLOCK CONTAINS 0 RECORDS                                     00810026
           DATA RECORD IS REPORT-RECORD.                                00820026
                                                                        00830026
       01  REPORT-RECORD     PIC X(80).                                 00840026
                                                                        00850026
      /                                                                 00860026
       WORKING-STORAGE SECTION.                                         00870026
       01  START-OF-WORKING-STORAGE    PIC X(40)                        00880026
           VALUE 'DB2CBLEX START-OF-WORKING-STORAGE'.                   00890026
                                                                        00900026
       01  C-PROG-MOD.                                                  00910026
           05 C-THIS-PGM               PIC X(08) VALUE 'DB2CBLEX'.      00920026
      /                                                                 00930026
       COPY REPORT.                                                     00930126
      /                                                                 01290026
      ***********              ***********                              01300026
      *      DB2 COMMUNICATION AREA      *                              01310026
      ***********              ***********                              01320026
                                                                        01330026
           EXEC SQL INCLUDE SQLCA END-EXEC.                             01340026
      * DCLGEN FOR EMP TABLE                                            01340129
           EXEC SQL INCLUDE DCLEMP END-EXEC.                            01340229
                                                                        01350026
      ***********              ***********                              01360026
      *      DB2 BASIC RETURN CODES      *                              01370026
      ***********              ***********                              01380026
                                                                        01390026
       01  DB2-RETURNS.                                                 01400026
            05 DB2-OK                PIC S9(04) COMP VALUE 0.           01410026
            05 DB2-END-OF-TABLE      PIC S9(04) COMP VALUE 100.         01420026
      /                                                                 01430026
       01 NULL_AREA.                                                    01940026
          05 NULL_IND           PIC S9(4) COMP OCCURS 2 TIMES.          01950026
                                                                        01960026
      ******************************************************************01970026
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 14      *01980026
      ******************************************************************01990026
                                                                        02000026
      ****************************************************************  02010026
      *   * CURSOR CALL FOR GPS CONTRACT INTERFACES TABLES          **  02020026
      *   *                                        (PRICE ADD ONS)  **  02030026
      *   * THIS INCLUDE CONTAINS THE CURSOR CODE FOR RETRIEVING    **  02040026
      *   * PRICE ADD ON CONTRACT DATA FROM GPS INTERFACES TABLES   **  02050026
      ****************************************************************  02060026
             EXEC SQL                                                   02070026
                  DECLARE EMP_RECORD  CURSOR FOR                        02080026
                                                                        02090026
                     SELECT EMPNO,                                      02100026
                            FIRSTNME,                                   02110026
                            LASTNAME,                                   02120026
                            WORKDEPT,                                   02130026
                            SALARY                                      02140026
                     FROM DSN8110.EMP                                   02150026
                     ORDER BY LASTNAME ASC                              02160026
                                                                        02160026
                   END-EXEC.                                            02170026
                                                                        02180026
      /                                                                 02190026
                                                                        02200026
       01   W-CURR-DATE-YMD.                                            02210026
              05  W-CURR-YYYY    PIC X(4) VALUE ' '.                    02220026
              05  DASH1          PIC X(1) VALUE '-'.                    02230026
              05  W-CURR-MM      PIC X(2) VALUE ' '.                    02240026
              05  DASH2          PIC X(1) VALUE '-'.                    02250026
              05  W-CURR-DD      PIC X(2) VALUE ' '.                    02260026
       01   C-CURR-DATE-YMD  REDEFINES W-CURR-DATE-YMD  PIC X(10).      02270026
                                                                        02280026
       01   W-PART-NBR       PIC X(15) VALUE ' '.                       02290026
                                                                        02300026
       01   CONSTANTS.                                                  02310026
            05  C-ABEND-PGM      PIC X(08)  VALUE  'WAASABND'.          02320026
            05  C-ABEND-CODE     PIC S9(09) COMP SYNC VALUE +3555.      02330026
            05  C-ABEND-TYPE     PIC X(02)  VALUE 'DN'.                 02340026
                                                                        02350026
                                                                        02360026
      /                                                                 02370026
      *            **MISC WORK STORAGE**                                02380026
                                                                        02390026
       01  WS-CURR-JULIAN7             PIC S9(07) COMP-3 VALUE ZEROES.  02400026
                                                                        02410026
                                                                        02420026
       01  PROGRAMMING-DISPLAY.                                         02430026
           03 PROG-MESSAGE             PIC X(40).                       02440026
           03 FILLER                   PIC X(6) VALUE SPACES.           02450026
           03 PROG-TOTALS              PIC Z(5)9.                       02460026
           03 FILLER                   PIC X(28) VALUE SPACES.          02470026
                                                                        02480026
       01  SUBSCRIPTS.                                                  02490026
           05  W-ADDON-SUB    PIC S9(4) COMP VALUE ZERO.                02500026
                                                                        02510026
       01  WS-DOUBLE-WORD              PIC S9(8) COMP SYNC.             02520026
                                                                        02530026
       01  C-VARIABLE-NAMES.                                            02540026
           05  C-YES                   PIC X(1) VALUE 'Y'.              02550026
           05  C-NO                    PIC X(1) VALUE 'N'.              02560026
           05  C-JOB-NAME              PIC X(8) VALUE 'XXXXXXXX'.       02570026
           05  C-GOOD-RETURN           PIC S9(08) COMP SYNC VALUE +0.   02580026
                                                                        02590026
       01  ACCUMULATORS.                                                02600026
          05  A-RECORDS-READ           PIC S9(8) COMP  VALUE ZERO.      02610026
          05  A-RECORDS-WRITTEN        PIC S9(8) COMP  VALUE ZERO.      02620026
                                                                        02630026
                                                                        02640026
      /                                                                 02650026
       LINKAGE SECTION.                                                 02660026
                                                                        02670026
       PROCEDURE DIVISION.                                              02680026
      ******************************************************************02690026
      *                                                                *02700026
      *           M A I N  L O O P                                     *02710026
      *                                                                *02720026
      ******************************************************************02730026
                                                                        02740026
           OPEN OUTPUT REPORT-FILE.                                     02750026
                                                                        02760026
           INITIALIZE   REPORT-RECORD                                   02770026
                      W-REPORT-RECORD.                                  02780026
                                                                        02790001
           WRITE REPORT-RECORD  FROM  W-REPORT-TITLE.                   02800033
           WRITE REPORT-RECORD  FROM  W-REPORT-HEADER1.                 02800133
           WRITE REPORT-RECORD  FROM  W-REPORT-HEADER2.                 02810026
                                                                        02820026
           PERFORM P5000-OPEN-EMP-RECORD.                               02830026
                                                                        02840026
           IF  SQLCODE  =  DB2-OK                                       02850026
               PERFORM  P5020-FETCH-EMP-RECORD                          02860026
               IF  SQLCODE  =  DB2-OK                                   02870026
                   PERFORM P0100-PROCESS-EMP-RECORD UNTIL               02880026
                           SQLCODE  NOT =  DB2-OK                       02890026
                   PERFORM P5010-CLOSE-EMP-RECORD                       02900026
               ELSE                                                     02910026
                   NEXT SENTENCE                                        02920026
               END-IF                                                   02930026
           ELSE                                                         02940026
               NEXT SENTENCE                                            02950026
           END-IF.                                                      02960026
                                                                        02970026
           DISPLAY 'TOTAL RECORDS READ        '  A-RECORDS-READ.        02980026
           DISPLAY 'TOTAL RECORDS WRITTEN     '  A-RECORDS-WRITTEN.     02990026
                                                                        03000026
                                                                        03010026
           CLOSE REPORT-FILE.                                           03020026
                                                                        03030026
                                                                        03040026
       EXIT-PROGRAM.                                                    03050026
           GOBACK.                                                      03060026
      /                                                                 03070026
       P0100-PROCESS-EMP-RECORD.                                        03080026
                                                                        03090026
      ******************************************************************03100026
      ******************************************************************03110026
                                                                        03120026
                                                                        03130026
           IF  SQLCODE  =  DB2-OK                                       03140026
                                                                        03150026
                   PERFORM P0200-LOAD-EMP-DATA                          03160026
                   IF  SQLCODE  =  DB2-OK                               03170026
                       CONTINUE                                         03180026
                   END-IF                                               03190026
                   WRITE REPORT-RECORD  FROM  W-REPORT-RECORD           03200026
                   COMPUTE A-RECORDS-WRITTEN = A-RECORDS-WRITTEN + 1    03210026
           ELSE                                                         03220026
               IF  SQLCODE  =  DB2-END-OF-TABLE                         03230026
                   NEXT SENTENCE                                        03240026
               ELSE                                                     03250026
                   DISPLAY 'P100 PROCESS EMP REC'                       03260026
                   DISPLAY 'SQLCODE = ', SQLCODE.                       03270026
                                                                        03280026
           PERFORM P5020-FETCH-EMP-RECORD.                              03290026
      /                                                                 03300026
      ***************************************************************** 03310026
      *                                                               * 03320026
      ***************************************************************** 03330026
       P0200-LOAD-EMP-DATA.                                             03340026
                                                                        03350026
            INITIALIZE  W-REPORT-RECORD.                                03360026
                                                                        03370026
            MOVE WORKDEPT                       TO  REP-WORK-DEPT.      03380026
            MOVE EMPNO                          TO  REP-EMP-NBR.        03390026
            MOVE LASTNAME-TEXT(1:LASTNAME-LEN)  TO  REP-LAST-NAME.      03400026
            MOVE FIRSTNME-TEXT(1:FIRSTNME-LEN)  TO  REP-FIRST-NAME.     03410026
            MOVE SALARY                         TO  REP-SALARY.         03420026
      /                                                                 03430026
      ******************************************************************03440026
      *          O P E N  G P S  C O N T R A C T  C U R S O R          *03450026
      ******************************************************************03460026
       P5000-OPEN-EMP-RECORD.                                           03470026
                                                                        03480026
             EXEC SQL                                                   03490026
               OPEN EMP_RECORD                                          03500026
             END-EXEC.                                                  03510026
                                                                        03520026
             IF  SQLCODE  =  DB2-OK                                     03530026
                 NEXT SENTENCE                                          03540026
             ELSE                                                       03550026
                 DISPLAY 'ERROR IN DB2 CALL TO EMP RECORD'              03560026
                 DISPLAY 'SQLCODE =', SQLCODE                           03570026
                 DISPLAY 'P5000-OPEN-EMP-REC'.                          03580026
      /                                                                 03590026
      ******************************************************************03600026
      *        C L O S E  G P S  C O N T R A C T  C U R S O R          *03610026
      ******************************************************************03620026
       P5010-CLOSE-EMP-RECORD.                                          03630026
                                                                        03640026
             EXEC SQL                                                   03650026
               CLOSE EMP_RECORD                                         03660026
             END-EXEC.                                                  03670026
                                                                        03680026
             IF  SQLCODE  =  DB2-OK                                     03690026
                 NEXT SENTENCE                                          03700026
             ELSE                                                       03710026
                 DISPLAY ' R5010-CLOSE-EMP-REC'                         03720026
                 DISPLAY ' SQLCODE', SQLCODE.                           03730026
      /                                                                 03740026
      ******************************************************************03750026
      *          F E T C H  G P S  C O N T R A C T  D A T A            *03760026
      ******************************************************************03770026
       P5020-FETCH-EMP-RECORD.                                          03780026
                                                                        03790026
           INITIALIZE DCLEMP.                                           03800026
                                                                        03810026
           EXEC SQL                                                     03820026
              FETCH EMP_RECORD                                          03830026
                                                                        03840026
              INTO  :DCLEMP.EMPNO,                                      03850026
                    :DCLEMP.FIRSTNME,                                   03860026
                    :DCLEMP.LASTNAME,                                   03870026
                    :DCLEMP.WORKDEPT,                                   03880026
                    :DCLEMP.SALARY                                      03890026
                                                                        03900026
              INDICATOR :NULL_IND                                       03910026
                                                                        03920026
           END-EXEC.                                                    03930026
                                                                        03940026
              IF  SQLCODE  =  DB2-OK                                    03950026
                  COMPUTE  A-RECORDS-READ  =  A-RECORDS-READ  +  1      03960026
              ELSE                                                      03970026
                  IF  SQLCODE  =  DB2-END-OF-TABLE                      03980026
                      INITIALIZE DCLEMP                                 03990026
                  ELSE                                                  04000026
                      DISPLAY 'P5020 EMP REC FETCH'                     04010026
                      DISPLAY 'SQLCODE = ', SQLCODE.                    04020026
      /                                                                 04030026
                                                                        04040026
                                                                        04050026
      *                                                                 04060026