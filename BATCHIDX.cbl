       IDENTIFICATION DIVISION.
       PROGRAM-ID. BATCHIDX.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATA-FILE
              ASSIGN TO DATAFILE
              ORGANIZATION IS INDEXED
              ACCESS MODE IS RANDOM 
              RECORD KEY IS DATA-KEY
              FILE STATUS IS DATA-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  DATA-FILE.
       01  DATA-REC.
           03 DATA-KEY.
              05 DATA-ID                 PIC S9(05) COMP-3.
              05 DATA-DVZ                PIC S9(03) COMP.
           03 DATA-FNAME                 PIC X(15).
           03 DATA-LNAME                 PIC X(15).
           03 DATA-DATE                  PIC S9(07) COMP-3.
           03 DATA-BALANCE               PIC S9(15) COMP-3.
       WORKING-STORAGE SECTION.
       01  WS-DATA-REC.
           03 WS-DATA-KEY.
              05 WS-DATA-ID              PIC S9(05) COMP-3.
              05 WS-DATA-DVZ             PIC S9(03) COMP.
           03 WS-DATA-FNAME              PIC X(15).
           03 WS-DATA-LNAME              PIC X(15).
           03 WS-DATA-DATE               PIC S9(07) COMP-3.
           03 WS-DATA-BALANCE            PIC S9(15) COMP-3.
       01  WS-FLAGS.
           03 DATA-ST                    PIC 9(02).
              88 DATA-ST-SUCC            VALUE 00 97.
              88 DATA-DUP-KEY            VALUE 02.
       01  WS-REMOVE-SPACES.
           03  WS-FNAME-TEMP             PIC X(15).
           03  WS-UNSTR-PTR              PIC 9(3).
           03  WS-INPUT-SIZE             PIC 9(3).

       01  WS-CONCAT-STR PIC X(35).
       01  WS-STR-PTR PIC 999.
       LINKAGE SECTION.
       01  LS-REC.
           03  LS-OPER-TYPE                PIC X(01).
               88 LS-VALID-OPER            VALUE 'R' 'U' 'W' 'D'.
               88 LS-READ-OPER             VALUE 'R'.
               88 LS-UPDATE-OPER           VALUE 'U'.
               88 LS-WRITE-OPER            VALUE 'W'.
               88 LS-DELETE-OPER           VALUE 'D'.
           03 LS-KEY.
              05 LS-ID                 PIC X(05).
              05 LS-DVZ                PIC X(03).
           03 LS-RETURN-CODE           PIC 9(02).
           03 LS-FNAME-FROM            PIC X(15).
           03 LS-FNAME-TO              PIC X(15).
           03 LS-LNAME-FROM            PIC X(15).
           03 LS-LNAME-TO              PIC X(15).
       PROCEDURE DIVISION USING LS-REC.
       MAIN-PARA.
           PERFORM INITIALIZE-VARS-PARA.
           PERFORM OPEN-FILES-PARA.
           MOVE LS-ID  TO DATA-ID
           MOVE LS-DVZ TO DATA-DVZ
           PERFORM PROC-PARA.
           PERFORM CLOSE-FILE-PARA.
           GOBACK.

       OPEN-FILES-PARA.
           OPEN I-O DATA-FILE
           IF NOT DATA-ST-SUCC
              DISPLAY 'CANNOT OPEN DATA FILE, STATUS: ' DATA-ST
              STOP RUN
           END-IF.

       PROC-PARA.
           IF LS-READ-OPER
              PERFORM READ-DATA-PARA
           END-IF.
           IF LS-UPDATE-OPER
              PERFORM UPDATE-DATA-PARA
           END-IF.
           IF LS-WRITE-OPER
              PERFORM WRITE-DATA-PARA
           END-IF.
           IF LS-DELETE-OPER
              PERFORM DELETE-DATA-PARA
           END-IF.

       READ-DATA-PARA.
           READ DATA-FILE KEY IS DATA-KEY
              INVALID KEY
                 PERFORM INVALID-KEY-PARA
              NOT INVALID KEY
                 PERFORM NOT-INVALID-KEY-PARA
           END-READ.
       UPDATE-DATA-PARA.
           PERFORM READ-DATA-PARA.
           PERFORM GENERATE-UPDATE-DATA-PARA.
           REWRITE DATA-REC
              INVALID KEY
                 PERFORM INVALID-KEY-PARA
              NOT INVALID
                 MOVE 00 TO LS-RETURN-CODE
           END-REWRITE.
       WRITE-DATA-PARA.
           PERFORM READ-DATA-PARA.
           PERFORM GENERATE-WRITE-DATA.
           WRITE DATA-REC FROM WS-DATA-REC
              INVALID KEY
                 PERFORM INVALID-KEY-PARA
              NOT INVALID
                 PERFORM NOT-INVALID-KEY-PARA
           END-WRITE.
       DELETE-DATA-PARA.
           PERFORM READ-DATA-PARA.
           DELETE DATA-FILE RECORD
              INVALID KEY
                 PERFORM INVALID-KEY-PARA
              NOT INVALID
                 PERFORM NOT-INVALID-KEY-PARA
           END-DELETE.
      *
       GENERATE-UPDATE-DATA-PARA.
           MOVE DATA-FNAME      TO WS-DATA-FNAME
           MOVE DATA-LNAME      TO WS-DATA-LNAME
           MOVE WS-DATA-FNAME   TO LS-FNAME-FROM.
           MOVE WS-DATA-LNAME   TO LS-LNAME-FROM.

      *REPLACE THE LETTERS E WITH I, A WITH E IN LAST NAME
           INSPECT WS-DATA-LNAME REPLACING ALL 'E' BY 'I'
           INSPECT WS-DATA-LNAME REPLACING ALL 'A' BY 'E'
      *REMOVE ALL SPACES IN FIRST NAME
           COMPUTE WS-INPUT-SIZE = LENGTH OF WS-DATA-FNAME
           PERFORM REMOVE-SPACES-FROM-FNAME-PARA
                                 UNTIL WS-UNSTR-PTR > WS-INPUT-SIZE.

           MOVE WS-CONCAT-STR TO DATA-FNAME
           MOVE WS-DATA-LNAME TO DATA-LNAME
           MOVE WS-CONCAT-STR TO LS-FNAME-TO
           MOVE WS-DATA-LNAME TO LS-LNAME-TO.
      *
       GENERATE-WRITE-DATA.
           MOVE LS-ID        TO WS-DATA-ID
           MOVE LS-DVZ       TO WS-DATA-DVZ
           MOVE 'A H  MET'   TO WS-DATA-FNAME
           MOVE 'KALAYCI'    TO WS-DATA-LNAME
           MOVE 19981111     TO WS-DATA-DATE
           MOVE 10           TO WS-DATA-BALANCE.
      *
       REMOVE-SPACES-FROM-FNAME-PARA.
      *SPLIT THE CONTENT OF THE STRING WS-DATA-FNAME INTO WS-FNAME-TEMP
      *WS-UNSTR-PTR KEEP TRACK OF THE POSITION WITHIN THE WS-DATA-FNAME-
      * WILL HOLD THE POSITION WITHIN WS-DATA-FNAME WHERE -
      * THE SPLITTING STOPPED.
           UNSTRING WS-DATA-FNAME
               DELIMITED BY ALL SPACE 
               INTO WS-FNAME-TEMP
               WITH POINTER WS-UNSTR-PTR
           END-UNSTRING.
      *CONCATE THE WORDS STORED IN WS-FNAME-TEMP INTO A -
      * SINGLE STRING (WS-CONCAT-STR)
      * WS-STR-PTR IS USED TO TRACK OF THE POSITION WITHIN WS-CONCAT-STR
           STRING WS-FNAME-TEMP DELIMITED BY SPACES
               INTO WS-CONCAT-STR
               WITH POINTER WS-STR-PTR
           END-STRING.
           MOVE SPACES TO WS-FNAME-TEMP.
      *
       INITIALIZE-VARS-PARA.
           MOVE 1 TO WS-STR-PTR.
           MOVE 1 TO WS-UNSTR-PTR.
           MOVE SPACES TO WS-CONCAT-STR
           MOVE SPACES TO WS-DATA-REC.
      *
       INVALID-KEY-PARA.
           MOVE 23  TO LS-RETURN-CODE
           MOVE '-' TO LS-FNAME-FROM
           MOVE '-' TO LS-FNAME-TO
           MOVE '-' TO LS-LNAME-FROM
           MOVE '-' TO LS-LNAME-TO.
      *
       NOT-INVALID-KEY-PARA.
           MOVE 00         TO LS-RETURN-CODE
           MOVE DATA-FNAME TO LS-FNAME-FROM
           MOVE '-'        TO LS-FNAME-TO
           MOVE DATA-LNAME TO LS-LNAME-FROM
           MOVE '-'        TO LS-LNAME-TO.
      *
       CLOSE-FILE-PARA.
           CLOSE DATA-FILE.
