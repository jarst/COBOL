       IDENTIFICATION DIVISION.
       PROGRAM-ID. PESEL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-PESEL.
           03  WS-PESEL-MAIN.
               05 WS-DATE.
                   07  WS-YEAR     PIC 9(2).
                   07  WS-MONTH    PIC 9(2).
                       88 IS-1800  VALUES 81 THRU 92.
                       88 IS-1900  VALUES  1 THRU 12.
                       88 IS-2000  VALUES 21 THRU 32.
                   07  WS-DAY      PIC 9(2).
               05  WS-NUMBER.
                   07 WS-SEQ       PIC 9(3).
                   07 WS-GEN       PIC 9(1).
                       88 IS-WOMAN VALUES 0, 2, 4, 6, 8.
                       88 IS-MAN   VALUES 1, 3, 5, 7, 9.
           03  WS-PESEL-CHECK-DIGIT    PIC 9(1).

       77  WS-CHECK-PARTIAL  PIC 99.
       77  WS-CHECK-TOTAL    PIC 999 VALUE 0.
       77  WS-CHECK-DIGIT    PIC 9.

       01  WS-AGE       PIC 9(4).
           88 IS-CHILD  VALUES  0 THRU 11.
           88 IS-TEEN   VALUES 12 THRU 17.
           88 IS-ADDULT VALUES 18 THRU 99.
       01  WS-AGE-DISP  PIC ZZ9.

       01  WS-CUR-YEAR  PIC 9(4) VALUE 2017.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           PERFORM GET-PESEL.
           PERFORM CALC-CHECKSUM.
           PERFORM VALIDATE-CHECKSUM.
           PERFORM CALC-AGE.
           PERFORM SHOW-TYPE.
           PERFORM SHOW-AGE.

       DONE.
           STOP RUN.

       GET-PESEL.
           DISPLAY "Podaj PESEL:".
           ACCEPT WS-PESEL.

       CALC-CHECKSUM.
      * PESEL: [ABCDEFGHIJK]
      * K=(9*A + 7*B + 3*C + D + 9*E + 7*F + 3*G + H + 9*I + 7*J) MOD 10
           MOVE WS-PESEL(1:1) TO WS-CHECK-PARTIAL.
           MULTIPLY 9 BY WS-CHECK-PARTIAL.
           ADD WS-CHECK-PARTIAL TO WS-CHECK-TOTAL.

           MOVE WS-PESEL(2:1) TO WS-CHECK-PARTIAL.
           MULTIPLY 7 BY WS-CHECK-PARTIAL.
           ADD WS-CHECK-PARTIAL TO WS-CHECK-TOTAL.

           MOVE WS-PESEL(3:1) TO WS-CHECK-PARTIAL.
           MULTIPLY 3 BY WS-CHECK-PARTIAL.
           ADD WS-CHECK-PARTIAL TO WS-CHECK-TOTAL.

           MOVE WS-PESEL(4:1) TO WS-CHECK-PARTIAL.
           ADD WS-CHECK-PARTIAL TO WS-CHECK-TOTAL.

           MOVE WS-PESEL(5:1) TO WS-CHECK-PARTIAL.
           MULTIPLY 9 BY WS-CHECK-PARTIAL.
           ADD WS-CHECK-PARTIAL TO WS-CHECK-TOTAL.

           MOVE WS-PESEL(6:1) TO WS-CHECK-PARTIAL.
           MULTIPLY 7 BY WS-CHECK-PARTIAL.
           ADD WS-CHECK-PARTIAL TO WS-CHECK-TOTAL.

           MOVE WS-PESEL(7:1) TO WS-CHECK-PARTIAL.
           MULTIPLY 3 BY WS-CHECK-PARTIAL.
           ADD WS-CHECK-PARTIAL TO WS-CHECK-TOTAL.

           MOVE WS-PESEL(8:1) TO WS-CHECK-PARTIAL.
           ADD WS-CHECK-PARTIAL TO WS-CHECK-TOTAL.

           MOVE WS-PESEL(9:1) TO WS-CHECK-PARTIAL.
           MULTIPLY 9 BY WS-CHECK-PARTIAL.
           ADD WS-CHECK-PARTIAL TO WS-CHECK-TOTAL.

           MOVE WS-PESEL(10:1) TO WS-CHECK-PARTIAL.
           MULTIPLY 7 BY WS-CHECK-PARTIAL.
           ADD WS-CHECK-PARTIAL TO WS-CHECK-TOTAL.

           MOVE WS-CHECK-TOTAL(3:1) TO WS-CHECK-DIGIT.

       VALIDATE-CHECKSUM.
           IF WS-CHECK-DIGIT NOT EQUAL WS-PESEL-CHECK-DIGIT THEN
               DISPLAY "Niepoprawny PESEL"
               PERFORM DONE
           END-IF.

       CALC-AGE.
           MOVE WS-CUR-YEAR TO WS-AGE.
           IF IS-1800
               SUBTRACT 1800 FROM WS-AGE
           ELSE IF IS-1900
               SUBTRACT 1900 FROM WS-AGE
           ELSE IF IS-2000
               SUBTRACT 2000 FROM WS-AGE
           END-IF.
           SUBTRACT WS-YEAR FROM WS-AGE.

       SHOW-TYPE.
           IF IS-WOMAN THEN
               IF IS-CHILD
                   DISPLAY "Dziewczynka"
               ELSE IF IS-TEEN
                   DISPLAY "Dziewczyna"
               ELSE IF IS-ADDULT
                   DISPLAY "Kobieta"
               END-IF
           ELSE
               IF IS-CHILD
                   DISPLAY "Chlopiec"
               ELSE IF IS-TEEN
                   DISPLAY "Chlopak"
               ELSE IF IS-ADDULT
                   DISPLAY "Mezczyzna"
               END-IF
           END-IF.

       SHOW-AGE.
           MOVE WS-AGE TO WS-AGE-DISP.
           DISPLAY "Lat:" WS-AGE-DISP.

       END PROGRAM PESEL.
