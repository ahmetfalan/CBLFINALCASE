       IDENTIFICATION DIVISION.
       PROGRAM-ID. BATCHPRG.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INP-FILE
              ASSIGN TO INPFILE
              FILE STATUS IS INP-ST.
           SELECT OUT-FILE
              ASSIGN TO OUTFILE
              FILE STATUS IS OUT-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  INP-FILE RECORDING MODE F.
       01  INP-REC.
           03 INP-OPER-TYPE           PIC X(01).
           03 INP-KEY.
              05 INP-ID               PIC X(05).
              05 INP-DVZ              PIC X(03).
       FD  OUT-FILE RECORDING MODE F.
       01  OUT-REC.
           03 OUT-KEY.
              05 OUT-ID               PIC 9(05).
              05 FILLER               PIC X(05) VALUE SPACE.
              05 OUT-DVZ              PIC 9(03).
           03 FILLER                  PIC X(05) VALUE SPACE.
           03 OUT-OPER-NAME           PIC X(10).
           03 FILLER                  PIC X(05) VALUE SPACE.
           03 OUT-RETURN-CODE         PIC 9(02).
           03 FILLER                  PIC X(05) VALUE SPACE.
           03 OUT-DESCRIPTION         PIC X(30).
           03 OUT-FNAME-FROM          PIC X(15).
           03 OUT-FNAME-TO            PIC X(15).
           03 OUT-LNAME-FROM          PIC X(15).
           03 OUT-LNAME-TO            PIC X(15).
       WORKING-STORAGE SECTION.
       01  WS-FLAGS.
           03 INP-ST                  PIC 9(02).
              88 INP-EOF              VALUE 10.
              88 INP-ST-SUCC          VALUE 00 97.
           03 OUT-ST                  PIC 9(02).
              88 OUT-ST-SUCC          VALUE 00 97.
       01  WS-BATCHIDX                PIC X(08) VALUE 'BATCHIDX'.
       01  WS-BATCHIDX-REC.
           03  WS-OPER-TYPE           PIC X(01).
               88 WS-OPER-TYPE-VALID  VALUE 'R' 'U' 'W' 'D'.
               88 WS-READ-OPER        VALUE 'R'.
               88 WS-UPDATE-OPER      VALUE 'U'.
               88 WS-WRITE-OPER       VALUE 'W'.
               88 WS-DELETE-OPER      VALUE 'D'.
           03 WS-KEY.
              05 WS-ID                PIC 9(05).
              05 WS-DVZ               PIC 9(03).
           03 WS-RETURN-CODE          PIC 9(02).
           03 WS-FNAME-FROM           PIC X(15).
           03 WS-FNAME-TO             PIC X(15).
           03 WS-LNAME-FROM           PIC X(15).
           03 WS-LNAME-TO             PIC X(15).
       01  WS-ERROR-MSG.
           03 WS-INVALID-OPER         PIC X(01).
           03 FILLER PIC X(25)        VALUE ': INVALID OPERATION TYPE!'.
       01  WS-HEADER-1.
           03 FILLER PIC X(24)        VALUE 'REPORT FOR BATCH PROGRAM'.
       01  WS-HEADER-2.
           03 FILLER PIC X(10)        VALUE 'ID'.
           03 FILLER PIC X(08)        VALUE 'DOVIZ'.
           03 FILLER PIC X(15)        VALUE 'OPERATION'.
           03 FILLER PIC X(07)        VALUE 'RC'.
           03 FILLER PIC X(30)        VALUE 'DESCRIPTION'.
           03 FILLER PIC X(15)        VALUE 'FIRST NAME'.
           03 FILLER PIC X(15)        VALUE 'FIRST NAME(U)'.
           03 FILLER PIC X(15)        VALUE 'LAST NAME'.
           03 FILLER PIC X(15)        VALUE 'LAST NAME(U)'.
       01  WS-HEADER-3.
           03 FILLER PIC X(130)       VALUE ALL '-'.
       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM OPEN-FILES-PARA.
           PERFORM WRITE-HEADERS-PARA.
           PERFORM READ-REC-PARA UNTIL INP-EOF.
           PERFORM CLOSE-FILES-PARA.
           PERFORM EXIT-PARA.

       OPEN-FILES-PARA.
           OPEN INPUT INP-FILE
           IF NOT INP-ST-SUCC
              DISPLAY 'CANNOT OPEN INPUT FILE, STATUS: ' INP-ST
              STOP RUN
           END-IF.
           OPEN OUTPUT OUT-FILE
           IF NOT OUT-ST-SUCC
              DISPLAY 'CANNOT OPEN OUTPUT FILE, STATUS: ' OUT-ST
              STOP RUN
           END-IF.

       READ-REC-PARA.
           READ INP-FILE
           NOT AT END
              MOVE INP-ID          TO WS-ID
              MOVE INP-DVZ         TO WS-DVZ
              MOVE INP-OPER-TYPE   TO WS-OPER-TYPE
              PERFORM CALL-WS-BATCHIDX-PARA
              PERFORM CHECK-OPER-TYPE-PARA
           END-READ.
       
       CHECK-OPER-TYPE-PARA.
           IF WS-OPER-TYPE-VALID
             EVALUATE INP-OPER-TYPE
                 WHEN 'R'
                   PERFORM OPER-READ-PARA
                 WHEN 'U'
                   PERFORM OPER-UPDATE-PARA
                 WHEN 'W'
                   PERFORM OPER-WRITE-PARA
                 WHEN 'D'
                   PERFORM OPER-DELETE-PARA
             END-EVALUATE
             WRITE OUT-REC
           ELSE
             MOVE INP-OPER-TYPE TO WS-INVALID-OPER
             WRITE OUT-REC FROM WS-ERROR-MSG
           END-IF.

       OPER-READ-PARA.
           IF WS-RETURN-CODE EQUAL 23
              MOVE 'RECORD NOT FOUND!' TO OUT-DESCRIPTION
           ELSE
              MOVE 'RECORD READ.'      TO OUT-DESCRIPTION
           END-IF.
           SET WS-READ-OPER TO TRUE
           MOVE 'READ'      TO OUT-OPER-NAME.
       OPER-UPDATE-PARA.
           IF WS-RETURN-CODE EQUAL 23
              MOVE 'RECORD NOT FOUND!' TO OUT-DESCRIPTION
           ELSE
              MOVE 'RECORD UPDATE.'    TO OUT-DESCRIPTION
           END-IF.
           SET WS-UPDATE-OPER TO TRUE
           MOVE 'UPDATE'      TO OUT-OPER-NAME.
       OPER-WRITE-PARA.
           IF WS-RETURN-CODE EQUAL 23
              MOVE 'THIS RECORD ALREADY ADDED!' TO OUT-DESCRIPTION
           ELSE
              MOVE 'RECORD WRITE.'              TO OUT-DESCRIPTION
           END-IF.
           SET WS-WRITE-OPER TO TRUE
           MOVE 'WRITE'      TO OUT-OPER-NAME.
       OPER-DELETE-PARA.
           IF WS-RETURN-CODE EQUAL 23
              MOVE 'RECORD NOT FOUND!'   TO OUT-DESCRIPTION
           ELSE
              MOVE 'RECORD DELETE.'      TO OUT-DESCRIPTION
           END-IF.
           SET WS-DELETE-OPER TO TRUE
           MOVE 'DELETE'      TO OUT-OPER-NAME.

       CALL-WS-BATCHIDX-PARA.
      *CALL WS-BATCHIDX SUB PROGRAM AND USE WS-BATCHIDX-REC PARAMATER
      * THAN GIVE THE CHANGED DATA WITHIN WS-BATCHIDX-REC
           CALL WS-BATCHIDX USING WS-BATCHIDX-REC
           MOVE SPACES          TO OUT-REC
           MOVE WS-ID           TO OUT-ID
           MOVE WS-DVZ          TO OUT-DVZ
           MOVE WS-RETURN-CODE  TO OUT-RETURN-CODE
           MOVE WS-FNAME-FROM   TO OUT-FNAME-FROM
           MOVE WS-FNAME-TO     TO OUT-FNAME-TO
           MOVE WS-LNAME-FROM   TO OUT-LNAME-FROM
           MOVE WS-LNAME-TO     TO OUT-LNAME-TO.

       WRITE-HEADERS-PARA.
           WRITE OUT-REC FROM WS-HEADER-1.
           WRITE OUT-REC FROM WS-HEADER-2.
           WRITE OUT-REC FROM WS-HEADER-3.

       CLOSE-FILES-PARA.
           CLOSE INP-FILE.
           CLOSE OUT-FILE.

       EXIT-PARA.
           STOP RUN.
