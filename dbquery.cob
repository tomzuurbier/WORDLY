      ******************************************************************
      * Author: Tom Zuurbier
      * Date: November 2024
      * Purpose: To lookup candidates for the NYT Wordle puzzle.
      * Requires: wordl.o
      * Tectonics: cob2
      * Compiler: IBM Cobol for Linux 1.2.0
      * How to compile:
      * 1. cob2 -I$HOME/sqllib/include/cobol_a -L$HOME/sqllib/lib32 
      *        -ldb2 dbquery.cob -qsql -q32 -qNODYNAM -c
      * 2. cob2 wordl.o dbquery.o -q32 -I$HOME/sqllib/include/cobol_a 
      *        -L$HOME/sqllib/lib32 -ldb2
      ******************************************************************
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
      * force a subprogram into its initial state each time it is called 
      * by including the IS INITIAL clause in the PROGRAM-ID.
       PROGRAM-ID. DBQUERY IS INITIAL.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
      *FILE-CONTROL.
      *     SELECT WORDFIL ASSIGN TO ".\data\wordlist.dat"
      *     ORGANIZATION IS LINE SEQUENTIAL
      *     ACCESS IS SEQUENTIAL
      *     FILE STATUS IS FS-WORDFIL-STATUS.

      *-------------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       
       FILE SECTION.
      *-----------------------
      * FD WORDFIL.
      *COPY WORDFIL REPLACING ==(PREF)== BY ==FD-WORDFIL==.
      * COPY filestat2  REPLACING ==(PREF)== BY ==FS-WORDFIL==.

       WORKING-STORAGE SECTION.
      *-----------------------
      *-----------------------------------------------------------*
      * Import copybooks definitions
      *-----------------------------------------------------------*
      *COPY FILESTAT  REPLACING ==(PREF)== BY ==FS-WORDFIL==.
      * 01 FS-WORDFIL-STATUS PIC X(2) VALUE '00'.
       01 LS-COUNT-FIELDS.
          05 LS-WORD-ENTRY-COUNT PIC S9(4).

      *===========================================================
       01  SEARCHFIELD-COUNT   PIC S9(2).
       01  LS-FETCH-COUNTER    PIC 9(4) value zero.
       77  WS-FETCH-COUNTER    PIC x(4) value zero.
      *77  WS-FETCH-COUNTER-I  redefines WS-FETCH-COUNTER PIC 9(4).

       01  WORD-TABLE.
               03  WORD     PIC X(5) OCCURS 2315
      *                               DEPENDING ON WORD-ENTRY-COUNT
                                      ascending KEY IS WORD
                                      INDEXED BY WORD-TABLE-INDEX.


       01  WV-INFILE-STATUS            PIC X(02).
           88  INSTAT-OK                           VALUE   '00'.
           88  INSTAT-EINDE-BESTAND                Value   '10'.
      
       01   WORD-INPUT       PIC X(5).

      *Host Structure for the WORD table
       01 WORDLY-TABLE.
         05 WORD-ROW     PIC X(5).
      *-----------------------------------------------------------
      *SUPPORTING DATA STRUCTURES FOR DB2 ACCESS
      *-----------------------------------------------------------
           EXEC SQL 
            INCLUDE SQLCA 
           END-EXEC.
      * copy "sql.cbl".
      * copy "sqlca.cbl".
           EXEC SQL
            DECLARE WORDCURS CURSOR FOR 
               SELECT WORD
               FROM COBOL.WORDLY
               WHERE WORD LIKE :WORD-INPUT
           END-EXEC.
       01 PASSWD       PIC X(19) VALUE "XXXXXXXXXXXXX".
       77 errloc          pic x(80).
      * Level 77 indicates a stand-alone item which should not be subdivided further. 
      *
       01  SQLCODE-INTEGER PIC 9(3).
       01  SWITCHES.
           05  END-OF-INQUIRIES-SW     PIC X   VALUE 'N'.
                   88 END-OF-INQUIRIES         VALUE 'Y'.
           05  WORD-FOUND-SW           PIC X   VALUE 'N'.
                   88 CUSTOMER-FOUND           VALUE 'Y'.
           05  VALID-CURSOR-SW         PIC X   VALUE 'Y'.
                   88 VALID-CURSOSR            VALUE 'Y'.
           05  END-OF-WORDLIST-SW      PIC X   VALUE 'N'.
                   88 END-OF-WORDLIST          VALUE 'Y'.
      *-----------------------------------------------------------
       LINKAGE SECTION.
      *-----------------------------------------------------------
       01  LS-SEARCHFIELD              PIC X(5).
       
      *77  LS-DB-RET-SET-INDEX INDEX.
       01  LS-DB-RET-SET.
      *    05  LS-FETCHED-WORD PIC X(5) occurs 1 TO 2315 times
           05  LS-FETCHED-WORD         PIC X(5) occurs 2315 times
                INDEXED BY LS-DB-RET-SET-INDEX.
       01  LS-DB-RET-NR                PIC 9(4) VALUE zero.
      *77  LS-DB-RET-SET-INDEX INDEX.
       
       PROCEDURE DIVISION USING LS-SEARCHFIELD, LS-DB-RET-SET, 
           LS-DB-RET-NR.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
           PERFORM B010-DBINIT
      *    PERFORM B020-CONNECTDB
           PERFORM B030-QUERY-RECORD-COUNT
           PERFORM B025-CREATECURSOR
           PERFORM B035-FETCH UNTIL END-OF-WORDLIST
               WITH TEST BEFORE
      *    PERFORM B040-DISCONNECTDB
           PERFORM B999-EXIT
           GOBACK
           .

       B010-DBINIT.
      *    DISPLAY "In B010-DBINIT"
      *    DISPLAY "Enter search word and press enter: " 
      *     with no advancing
      *    ACCEPT WORD-INPUT
      *    DISPLAY "Search for: " LS-SEARCHFIELD
      *    MOVE 1 TO LS-DB-RET-NR
      *    MOVE "abcde" to LS-DB-RET-SET
           MOVE LS-SEARCHFIELD TO WORD-INPUT
           SET LS-DB-RET-SET-INDEX TO 1
           .

       B020-CONNECTDB.
      *     DISPLAY "In B020-CONNECTDB"

           EXEC SQL 
              CONNECT TO COBOLDB USER db2inst1 
              USING PASSWD
           END-EXEC.
           IF SQLCODE EQUAL TO zero
               DISPLAY "CONNECTED"
           ELSE
               DISPLAY "Cannot Connect!  (SQLCODE: " SQLCODE ")"
           END-IF
          
      *    call "checkerr" using SQLCA errloc.
           .

       B025-CREATECURSOR.
           EXEC SQL
              OPEN WORDCURS
           END-EXEC.
           IF SQLCODE NOT = 0
               MOVE 'N' TO VALID-CURSOR-SW
           .

       B030-QUERY-RECORD-COUNT.
      *    DISPLAY "In BB030-QUERY-RECORD-COUNT"
           EXEC SQL
              SELECT count(WORD) 
              INTO :WS-FETCH-COUNTER
              FROM COBOL.WORDLY
              WHERE WORD LIKE :WORD-INPUT
           END-EXEC.
      *    call "checkerr" using SQLCA errloc.
           IF SQLCODE EQUAL TO zero
      *        DISPLAY WS-FETCH-COUNTER
      *        DISPLAY WS-FETCH-COUNTER-I
               continue
           ELSE
               DISPLAY "Not Found!  (SQLCODE: " SQLCODE ")"
           END-IF
      *    MOVE WS-FETCH-COUNTER-I TO LS-DB-RET-NR
      *    MOVE WS-FETCH-COUNTER TO LS-DB-RET-NR
      *    To Convert WS-FETCH-COUNTER Alhanumeric field to Numeric.
           COMPUTE LS-DB-RET-NR = FUNCTION NUMVAL(WS-FETCH-COUNTER)
           .

       B035-FETCH.
      *    initialize WORD-ROW
           EXEC SQL
              FETCH WORDCURS
                INTO :WORD-ROW
           END-EXEC.
      *    DISPLAY "Fetch(1): " WORD-ROW
      *    CHECK FOR NO MORE RECORDS    
           IF SQLCODE NOT = 0
               SET END-OF-WORDLIST TO true
      *    SQLCODE = 100 indicates end-of-table. Anything else than 100
      *       indicates a serious error.  
            END-IF   
            MOVE function INTEGER-PART(SQLCODE) TO SQLCODE-INTEGER
            IF function INTEGER-PART(SQLCODE) NOT = 100
      *            DISPLAY "In IF SQLCODE NOT = 100"
                   MOVE WORD-ROW TO LS-FETCHED-WORD(LS-DB-RET-SET-INDEX)
      *            ADD 1 TO LS-DB-RET-NR
      *            DISPLAY "Counter value: " LS-DB-RET-NR
                   SET LS-DB-RET-SET-INDEX UP BY 1
                   MOVE 'N' TO VALID-CURSOR-SW
            else
                   continue
            END-IF
           .

       B040-DISCONNECTDB.
           DISPLAY "In B040-DISCONNECTDB"
           .
       B999-EXIT.
           EXEC SQL
              CLOSE WORDCURS
           END-EXEC.
           IF SQLCODE NOT = 0
               MOVE 'N' TO VALID-CURSOR-SW
           .
      * The EXIT PROGRAM statement specifies the end of a called 
      * program and returns control to the calling program.    
           EXIT PROGRAM.
       END PROGRAM DBQUERY.