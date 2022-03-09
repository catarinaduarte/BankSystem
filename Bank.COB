      ******************************************************************
      * Author:
      * Date:     30/06/2021
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TAREFA-11.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 REPETE   PIC X.
           88 VAL-REPETE VALUES "S", "C", "D".
       77 WS-TEMP  PIC X(14)     VALUE SPACES.
       77 WS-SALDO PIC S9(10)V99 VALUE 0.
       77 WS-VALOR PIC 9(10)V99  VALUE 0.
       77 ZCLEAN   PIC ZZBZZZBZZZBZZZBV,99.
       77 LINHA    PIC 9(02)     VALUE 10.


       SCREEN SECTION.
       01 CLS BLANK SCREEN.

       01 CLEAN-LINE.
           05 COL 1  VALUE " " LINE 23 ERASE EOL.
           05 COL 1  VALUE " " LINE 24 ERASE EOL.
           05 COL 1  VALUE " " LINE 26 ERASE EOL.
           05 COL 1  VALUE " " LINE 27 ERASE EOL.

       01 LAYOUT.


           05 COL 10 VALUE"----------------CONTA CORRENTE--------------"
           LINE 2.
           05 COL 10 VALUE"--------------------------------------------"
           LINE 4.
           05 COL 12 VALUE "SALDO INICIAL:"                   LINE 6
           HIGHLIGHT.
           05 COL 10 VALUE"--------------------------------------------"
           LINE 7.
           05 COL 12 VALUE "OPCAO    VALOR            SALDO"  LINE 9
           HIGHLIGHT.
           05 COL 10 VALUE"--------------------------------------------"
           LINE 10.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM UNTIL FUNCTION UPPER-CASE (REPETE) = "S"
               DISPLAY CLS
               DISPLAY LAYOUT
               MOVE 10 TO LINHA

               DISPLAY "POR FAVOR INSIRA O SALDO INICIAL DA SUA CONTA"
               AT 1409 HIGHLIGHT
               ACCEPT WS-TEMP AT 0628
               MOVE WS-TEMP TO WS-SALDO
               MOVE WS-SALDO TO ZCLEAN
               IF WS-SALDO < 0 THEN
                   DISPLAY FUNCTION CONCATENATE ("- ",
                   FUNCTION TRIM (ZCLEAN)) AT 0628 FOREGROUND-COLOR 4
                   HIGHLIGHT
               ELSE
                   DISPLAY FUNCTION TRIM (ZCLEAN) AT 0628
                   FOREGROUND-COLOR 2 HIGHLIGHT
               END-IF

               DISPLAY CLEAN-LINE
               PERFORM UNTIL REPETE = "S"
                   ADD 1 TO LINHA

                   DISPLAY "PRETENDE EFECTUAR DEBITO (D) OU CREDITO"&
                   " (C): " AT 1709 HIGHLIGHT
                   DISPLAY "PRIMA (S) PARA SAIR" AT 1909 HIGHLIGHT

                   ACCEPT WS-TEMP AT 1754
                   MOVE FUNCTION UPPER-CASE (WS-TEMP) TO REPETE

                   IF REPETE = "D" THEN
                       DISPLAY CLEAN-LINE
                       DISPLAY "D" AT COL 14 LINE LINHA
                       DISPLAY "QUAL O VALOR DO DEBITO?" AT 2309
                       HIGHLIGHT
                       ACCEPT WS-TEMP AT 2333
                       MOVE WS-TEMP TO WS-VALOR
                       SUBTRACT WS-VALOR FROM WS-SALDO
                       MOVE WS-VALOR TO ZCLEAN
                       DISPLAY FUNCTION CONCATENATE ("- ",FUNCTION
                       TRIM (ZCLEAN)) AT COL 20 LINE LINHA

                       MOVE WS-SALDO TO ZCLEAN
                       IF WS-SALDO >= 0 THEN
                       DISPLAY FUNCTION TRIM (ZCLEAN)
                           AT COL 39 LINE LINHA FOREGROUND-COLOR 2
                           HIGHLIGHT
                       ELSE
                           DISPLAY FUNCTION CONCATENATE ("- ",FUNCTION
                           TRIM (ZCLEAN)) AT COL 37 LINE LINHA
                           FOREGROUND-COLOR 4 HIGHLIGHT
                       END-IF
                   ELSE IF REPETE = "C" THEN
                       DISPLAY CLEAN-LINE
                       DISPLAY "C" AT COL 14 LINE LINHA
                       DISPLAY "QUAL O VALOR DO CREDITO?" AT 2109
                       HIGHLIGHT
                       ACCEPT WS-TEMP AT 2134
                       MOVE WS-TEMP TO WS-VALOR
                       ADD WS-VALOR TO WS-SALDO
                       MOVE WS-VALOR TO ZCLEAN
                       DISPLAY FUNCTION CONCATENATE ("+ ",
                       FUNCTION TRIM (ZCLEAN))
                       AT COL 20 LINE LINHA

                       MOVE WS-SALDO TO ZCLEAN
                       IF WS-SALDO >= 0 THEN
                           DISPLAY FUNCTION TRIM (ZCLEAN)
                           AT COL 39 LINE LINHA FOREGROUND-COLOR 2
                           HIGHLIGHT
                       ELSE
                           DISPLAY FUNCTION CONCATENATE ("- ",FUNCTION
                           TRIM (ZCLEAN)) AT COL 37 LINE LINHA
                           FOREGROUND-COLOR 4 HIGHLIGHT
                       END-IF
                   ELSE
                       DISPLAY "VALOR INSERIDO ERRADO!" HIGHLIGHT
                       FOREGROUND-COLOR 4 AT  2709
                       SUBTRACT 1 FROM LINHA
                   END-IF
               END-PERFORM

               DISPLAY CLEAN-LINE
               DISPLAY "PRIMA (S) PARA SAIR OU OUTRO PARA CONTINUAR:"
               AT 2509 HIGHLIGHT
               ACCEPT WS-TEMP AT 2554
               MOVE WS-TEMP TO REPETE
           END-PERFORM.
           STOP RUN.
       END PROGRAM TAREFA-11.
