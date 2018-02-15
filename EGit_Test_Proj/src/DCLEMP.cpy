      *******************************************************************DCLEMP*
      * DCLGEN TABLE(DSN8110.EMP)                                      **DCLEMP*
      *        LIBRARY(LOCK.DCLGEN.DSN8110(EMP))                       **DCLEMP*
      *        LANGUAGE(COBOL)                                         **DCLEMP*
      *        QUOTE                                                   **DCLEMP*
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   **DCLEMP*
      *******************************************************************DCLEMP*
           EXEC SQL DECLARE DSN8110.EMP TABLE                           *DCLEMP*
           ( EMPNO                          CHAR(6) NOT NULL,           *DCLEMP*
             FIRSTNME                       VARCHAR(12) NOT NULL,       *DCLEMP*
             MIDINIT                        CHAR(1) NOT NULL,           *DCLEMP*
             LASTNAME                       VARCHAR(15) NOT NULL,       *DCLEMP*
             WORKDEPT                       CHAR(3),                    *DCLEMP*
             PHONENO                        CHAR(4),                    *DCLEMP*
             HIREDATE                       DATE,                       *DCLEMP*
             JOB                            CHAR(8),                    *DCLEMP*
             EDLEVEL                        SMALLINT,                   *DCLEMP*
             SEX                            CHAR(1),                    *DCLEMP*
             BIRTHDATE                      DATE,                       *DCLEMP*
             SALARY                         DECIMAL(9, 2),              *DCLEMP*
             BONUS                          DECIMAL(9, 2),              *DCLEMP*
             COMM                           DECIMAL(9, 2)               *DCLEMP*
           ) END-EXEC.                                                  *DCLEMP*
      *******************************************************************DCLEMP*
      * COBOL DECLARATION FOR TABLE DSN8110.EMP                        **DCLEMP*
      *******************************************************************DCLEMP*
       01  DCLEMP.                                                      *DCLEMP*
           10 EMPNO                PIC X(6).                            *DCLEMP*
           10 FIRSTNME.                                                 *DCLEMP*
              49 FIRSTNME-LEN      PIC S9(4) USAGE COMP.                *DCLEMP*
              49 FIRSTNME-TEXT     PIC X(12).                           *DCLEMP*
           10 MIDINIT              PIC X(1).                            *DCLEMP*
           10 LASTNAME.                                                 *DCLEMP*
              49 LASTNAME-LEN      PIC S9(4) USAGE COMP.                *DCLEMP*
              49 LASTNAME-TEXT     PIC X(15).                           *DCLEMP*
           10 WORKDEPT             PIC X(3).                            *DCLEMP*
           10 PHONENO              PIC X(4).                            *DCLEMP*
           10 HIREDATE             PIC X(10).                           *DCLEMP*
           10 JOB                  PIC X(8).                            *DCLEMP*
           10 EDLEVEL              PIC S9(4) USAGE COMP.                *DCLEMP*
           10 SEX                  PIC X(1).                            *DCLEMP*
           10 BIRTHDATE            PIC X(10).                           *DCLEMP*
           10 SALARY               PIC S9(7)V9(2) USAGE COMP-3.         *DCLEMP*
           10 BONUS                PIC S9(7)V9(2) USAGE COMP-3.         *DCLEMP*
           10 COMM                 PIC S9(7)V9(2) USAGE COMP-3.         *DCLEMP*
      *******************************************************************DCLEMP*
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 14      **DCLEMP*
      *******************************************************************DCLEMP*