      ******************************************************************
      * Author: Tom Zuurbier
      * Date: November 2024
      * Purpose: To lookup candidates for the NYT Wordle puzzle.
      * Requires: dbquery.o
      * Tectonics: cob2
      * Compiler: IBM Cobol for Linux 1.2.0
      * How to compile:
      * 1. cob2 -I ./copybooks wordl.cob -q32  -qNODYNAM -c
      * 2. cob2 wordl.o dbquery.o -q32 -I$HOME/sqllib/include/cobol_a 
      *        -L$HOME/sqllib/lib32 -ldb2
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. WORDL.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
     
      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
      *-----------------------
    
       WORKING-STORAGE SECTION.
      *-----------------------
      *-----------------------------------------------------------*
      * Import copybooks definitions
      *-----------------------------------------------------------*
       01 SWITCHES.
           05 WTABLE-EOF-SWITCH    PIC X   VALUE "N".
               88 WTABLE-EOF               VALUE "Y".
       01 COUNT-FIELDS.
          05 WORD-ENTRY-COUNT PIC S9(4).
       01  WS-LOOP-COUNTER PIC 9(4) value zero.
       01 DB-RET-SET.
           05  FETCHED-WORD PIC X(5) occurs 2315
                                     indexed by WS-DB-RET-SET-INDEX.
                                  
       01  DB-RET-NR                 PIC 9(4) VALUE zero.
       01  SEARCHFIELD        PIC X(5).
       01  CONTFIELD           PIC X value "y".
      *===========================================================
      *VARS for test purposes
       01  TESTFIELD           PIC X(5) value "abcde".
       01  INPUTTABLE.
           05 CHAR         PIC X OCCURS 5.
       01  COUNTER         PIC 9.
       01  INPUTDUMMY      PIC X.
      *===========================================================
       01  SEARCHFIELD-COUNT   PIC S9(2).

       
      
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
           perform UNTIL CONTFIELD NOT = "y"
               PERFORM A010-INIT
               PERFORM A020-PREPFORSEARCH
               PERFORM A030-CALLDBQUERY
      *    PERFORM A030-READTABLE
      *    PERFORM A050-SEARCHTABLE
               PERFORM A060-DISPLAY-RESULTS
           end perform 
      *     PERFORM A999-EXIT
           GOBACK
           .

       A010-INIT.
      *    DISPLAY "A010-INIT"
           SET WS-DB-RET-SET-INDEX TO 1
           .

       A020-PREPFORSEARCH.
      *    DISPLAY parameter to ensure word entered after :
      *    Input validation (5 charachter, lowercase)
           DISPLAY 'Enter search string: (use * for wildcard i.e. "yo*ng
      -    "). Enter "9" to exit program: ' 
           with NO ADVANCING
           accept SEARCHFIELD

           IF SEARCHFIELD = "9"
               STOP RUN
           end-if

           IF SEARCHFIELD IS NOT alphabetic AND NOT SEARCHFIELD(1:1) = 
      -     "*" AND NOT SEARCHFIELD(2:1) = "*" AND NOT SEARCHFIELD(3:1)
      -     = "*" AND NOT SEARCHFIELD(4:1) 
      -     = "*" AND NOT SEARCHFIELD(5:1) = "*"
               DISPLAY "Incorrect input"
           END-IF
           INSPECT SEARCHFIELD tallying SEARCHFIELD-COUNT FOR characters
           before space
           IF SEARCHFIELD-COUNT < 5
               DISPLAY "Incorrect input length"
      *        PERFORM A999-EXIT
           END-IF
           INSPECT SEARCHFIELD REPLACING ALL "*" by "_"
      *    DISPLAY SEARCHFIELD
           .

       A030-CALLDBQUERY.
      *    DISPLAY "In A030-CALLDBQUERY"
           CALL "dbquery" USING by reference SEARCHFIELD, DB-RET-SET
           DB-RET-NR
           ON exception DISPLAY "Called program not found!"
           END-CALL
      *    DISPLAY "Return DB-RET-NR: " DB-RET-NR
        .
       A060-DISPLAY-RESULTS.
      *    DISPLAY "In A060-DISPLAY-RESULT"
      *    DISPLAY "DB-RET-NR:. " DB-RET-NR
      *    DISPLAY "FETCHED-WORD(1): " FETCHED-WORD(1)
           EVALUATE true
               WHEN DB-RET-NR = 0
                   DISPLAY "No entry found"
               WHEN DB-RET-NR > 0
      *            DISPLAY "Perform times:: " DB-RET-NR
                   PERFORM DB-RET-NR TIMES
      *            perform WITH TEST after
      *            UNTIL WS-LOOP-COUNTER > DB-RET-NR
      *                DISPLAY WS-LOOP-COUNTER
                       ADD 1 TO WS-LOOP-COUNTER GIVING WS-LOOP-COUNTER
      *                DISPLAY "WS-LOOP-COUNTER: " WS-LOOP-COUNTER
                       DISPLAY FETCHED-WORD(WS-DB-RET-SET-INDEX)
                       SET WS-DB-RET-SET-INDEX UP BY 1
                   end-perform
               WHEN other 
                   DISPLAY "Error"
           END-EVALUATE
       .  

       A999-EXIT.
      *    CALL DBQUERY with parameter 5 letter word / 5 letter 
      *      querystring
      *    CALL 
      *    CLOSE   WORDFIL
           DISPLAY "In A999-EXIT"
      *    STOP run.
           .
       END PROGRAM WORDL.
