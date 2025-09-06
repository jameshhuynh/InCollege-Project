       >>SOURCE FORMAT FREE
       *> DEMO – Account creation / password validation

       IDENTIFICATION DIVISION.
       PROGRAM-ID. CREATE-ACCOUNT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT USER-FILE ASSIGN TO "users.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  USER-FILE.
       01  USER-REC       PIC X(80).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS PIC XX VALUE SPACES.

       01  WS-INPUT-LINE      PIC X(80).
       01  WS-OUTPUT-LINE     PIC X(80).
       01  WS-USERNAME        PIC X(20).
       01  WS-PASSWORD        PIC X(12).

       01  WS-PASSWORD-FLAGS.
           05 WS-HAS-UPPER     PIC X VALUE "N".
           05 WS-HAS-DIGIT     PIC X VALUE "N".
           05 WS-HAS-SPECIAL   PIC X VALUE "N".
           05 WS-VALID-LENGTH  PIC X VALUE "N".

       01  WS-CHAR            PIC X.
       01  WS-I               PIC 99.
       01  WS-PASSWORD-LENGTH PIC 99.

       01  WS-MAX-USERS       PIC 99 VALUE 5.
       01  WS-USER-COUNT      PIC 99 VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM ENSURE-FILE
           PERFORM CREATE-ACCOUNT
           STOP RUN.

       *>--------------------------------------------------
       *> Ensure that users.dat exists (create empty if not)
       *>--------------------------------------------------
       ENSURE-FILE.
           OPEN INPUT USER-FILE
           IF WS-FILE-STATUS = "35"  *> file not found
              OPEN OUTPUT USER-FILE
              CLOSE USER-FILE
           ELSE
              CLOSE USER-FILE
           END-IF.

       *>--------------------------------------------------
       CREATE-ACCOUNT.
           IF WS-USER-COUNT >= WS-MAX-USERS
              MOVE "All permitted accounts created" TO WS-OUTPUT-LINE
              PERFORM WRITE-OUTPUT
           ELSE
              PERFORM GET-NEW-USERNAME
              PERFORM GET-NEW-PASSWORD
              IF WS-HAS-UPPER = "Y"
                 AND WS-HAS-DIGIT = "Y"
                 AND WS-HAS-SPECIAL = "Y"
                 AND WS-VALID-LENGTH = "Y"
                 ADD 1 TO WS-USER-COUNT
                 PERFORM APPEND-USER
                 MOVE "Account created successfully!" TO WS-OUTPUT-LINE
                 PERFORM WRITE-OUTPUT
              END-IF
           END-IF.

       *>--------------------------------------------------
       GET-NEW-USERNAME.
           MOVE "Enter username:" TO WS-OUTPUT-LINE
           PERFORM WRITE-OUTPUT
           PERFORM READ-INPUT
           MOVE WS-INPUT-LINE TO WS-USERNAME.

       GET-NEW-PASSWORD.
           MOVE "Enter password:" TO WS-OUTPUT-LINE
           PERFORM WRITE-OUTPUT
           PERFORM READ-INPUT
           MOVE WS-INPUT-LINE TO WS-PASSWORD
           PERFORM VALIDATE-PASSWORD.

       *>--------------------------------------------------
       VALIDATE-PASSWORD.
           MOVE "N" TO WS-HAS-UPPER WS-HAS-DIGIT WS-HAS-SPECIAL WS-VALID-LENGTH
           MOVE 0  TO WS-PASSWORD-LENGTH
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 12
               IF WS-PASSWORD(WS-I:1) NOT = SPACE
                   ADD 1 TO WS-PASSWORD-LENGTH
               END-IF
           END-PERFORM
           IF WS-PASSWORD-LENGTH >= 8 AND WS-PASSWORD-LENGTH <= 12
               MOVE "Y" TO WS-VALID-LENGTH
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-PASSWORD-LENGTH
               MOVE WS-PASSWORD(WS-I:1) TO WS-CHAR
               IF WS-CHAR >= "A" AND WS-CHAR <= "Z"
                   MOVE "Y" TO WS-HAS-UPPER
               END-IF
               IF WS-CHAR >= "0" AND WS-CHAR <= "9"
                   MOVE "Y" TO WS-HAS-DIGIT
               END-IF
               IF WS-CHAR = "!" OR WS-CHAR = "@" OR WS-CHAR = "#" OR
                  WS-CHAR = "$" OR WS-CHAR = "%" OR WS-CHAR = "^" OR
                  WS-CHAR = "&" OR WS-CHAR = "*"
                   MOVE "Y" TO WS-HAS-SPECIAL
               END-IF
           END-PERFORM
           IF WS-VALID-LENGTH = "N"
               MOVE "Password must be 8-12 chars" TO WS-OUTPUT-LINE
               PERFORM WRITE-OUTPUT
           END-IF
           IF WS-HAS-UPPER = "N"
               MOVE "Password needs uppercase" TO WS-OUTPUT-LINE
               PERFORM WRITE-OUTPUT
           END-IF
           IF WS-HAS-DIGIT = "N"
               MOVE "Password needs digit" TO WS-OUTPUT-LINE
               PERFORM WRITE-OUTPUT
           END-IF
           IF WS-HAS-SPECIAL = "N"
               MOVE "Password needs special (!,@,#,$...)" TO WS-OUTPUT-LINE
               PERFORM WRITE-OUTPUT
           END-IF.

       *>--------------------------------------------------
       APPEND-USER.
           OPEN EXTEND USER-FILE
           STRING WS-USERNAME DELIMITED BY SPACE
                  "," DELIMITED BY SIZE
                  WS-PASSWORD DELIMITED BY SPACE
                  INTO USER-REC
           END-STRING
           WRITE USER-REC
           CLOSE USER-FILE.

       *>--------------------------------------------------
       READ-INPUT.
           ACCEPT WS-INPUT-LINE.

       WRITE-OUTPUT.
           DISPLAY WS-OUTPUT-LINE.
