      ***             P R O G R A M  C H A N G E  L O G                **REPORT*
      *******************************************************************REPORT*
      *  CHANGED BY:                                  DATE:            **REPORT*
      *                                                                **REPORT*
      *  LOUIS - CHANGED REP-SALARY FORMAT            2017-09-22       **REPORT*
      *  LOUIS - ADDED A REPORT TITLE                 2017-11-14       **REPORT*
      *                                                                **REPORT*
      *******************************************************************REPORT*
                                                                        *REPORT*
       01  W-REPORT-RECORD.                                             *REPORT*
           05  REP-WORK-DEPT                    PIC X(3).               *REPORT*
           05  SPACER1                          PIC X.                  *REPORT*
           05  REP-EMP-NBR                      PIC X(06).              *REPORT*
           05  SPACER2                          PIC X.                  *REPORT*
           05  REP-LAST-NAME                    PIC X(15).              *REPORT*
           05  SPACER3                          PIC X.                  *REPORT*
           05  REP-FIRST-NAME                   PIC X(12).              *REPORT*
           05  SPACER4                          PIC X.                  *REPORT*
           05  REP-SALARY                       PIC ZZZZZZ9.99.         *REPORT*
           05  SPACER7                          PIC X(30).              *REPORT*
      /                                                                 *REPORT*
       01  W-REPORT-HEADER1.                                            *REPORT*
           05  HD1-WORK-DEPT           PIC X(3) VALUE 'DEP'.            *REPORT*
           05  SPACER1                          PIC X VALUE ' '.        *REPORT*
           05  HD1-EMP-NBR             PIC X(06) VALUE 'EMPNUM'.        *REPORT*
           05  SPACER2                          PIC X VALUE ' '.        *REPORT*
           05  HD1-LAST-NAME           PIC X(15) VALUE 'LASTNME'.       *REPORT*
           05  SPACER3                          PIC X VALUE ' '.        *REPORT*
           05  HD1-FIRST-NAME          PIC X(12) VALUE 'FIRSTNME'.      *REPORT*
           05  SPACER4                          PIC X VALUE ' '.        *REPORT*
           05  HD1-SALARY              PIC X(9) VALUE 'SALARY'.         *REPORT*
           05  SPACER7                          PIC X(31) VALUE ' '.    *REPORT*
      /                                                                 *REPORT*
       01  W-REPORT-HEADER2.                                            *REPORT*
           05  HD2-WORK-DEPT           PIC X(3) VALUE '---'.            *REPORT*
           05  SPACER1                          PIC X VALUE ' '.        *REPORT*
           05  HD2-EMP-NBR             PIC X(06) VALUE '------'.        *REPORT*
           05  SPACER2                          PIC X VALUE ' '.        *REPORT*
           05  HD2-LAST-NAME           PIC X(15) VALUE '-------'.       *REPORT*
           05  SPACER3                          PIC X VALUE ' '.        *REPORT*
           05  HD2-FIRST-NAME          PIC X(12) VALUE '--------'.      *REPORT*
           05  SPACER4                          PIC X VALUE ' '.        *REPORT*
           05  HD2-SALARY              PIC X(9) VALUE '------'.         *REPORT*
           05  SPACER7                          PIC X(31) VALUE ' '.    *REPORT*
      /                                                                 *REPORT*
       01  W-REPORT-TITLE.                                              *REPORT*
           05  SPACER1                 PIC X(5) VALUE '*****'.          *REPORT*
           05  REPORT-TITLE   PIC X(19) VALUE ' EMPLOYEE REPORT 4 '.    *REPORT*
           05  SPACER2                 PIC X(5) VALUE '*****'.          *REPORT*
           05  SPACER3                          PIC X(51) VALUE ' '.    *REPORT*