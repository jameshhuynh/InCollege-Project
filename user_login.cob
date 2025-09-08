       >>SOURCE FORMAT FREE
       *> IDAHO-2 – Demo for account creation / password validation with login

       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCOUNT-SYSTEM.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-INPUT-LINE      PIC X(80).
       01 WS-OUTPUT-LINE     PIC X(80).

       01 WS-USERNAME        PIC X(20).
       *> Make the raw field larger than 12 so we can detect overflow
       01 WS-PASSWORD        PIC X(20).

       01 WS-USER-COUNT      PIC 99 VALUE 0.
       01 WS-MAX-USERS       PIC 99 VALUE 5.

       01 WS-PASSWORD-FLAGS.
          05 WS-HAS-UPPER     PIC X VALUE 'N'.
          05 WS-HAS-DIGIT     PIC X VALUE 'N'.
          05 WS-HAS-SPECIAL   PIC X VALUE 'N'.
          05 WS-VALID-LENGTH  PIC X VALUE 'N'.

       01 WS-CHAR            PIC X.
       01 WS-I               PIC 99.
       01 WS-J               PIC 99.
       01 WS-PASSWORD-LENGTH PIC 99.
       01 WS-LOGIN-USERNAME  PIC X(20).
       01 WS-LOGIN-PASSWORD  PIC X(20).
       01 WS-LOGIN-SUCCESS   PIC X VALUE 'N'.
       01 WS-MENU-CHOICE     PIC X.
       01 WS-CONTINUE        PIC X VALUE 'Y'.

       01 WS-USER-TABLE.
          05 WS-USER-ENTRY OCCURS 5 TIMES.
             10 WS-USER-ID   PIC X(20).
             10 WS-USER-PASS PIC X(12).

       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM MAIN-MENU UNTIL WS-CONTINUE = 'N'
           STOP RUN.

       MAIN-MENU.
           MOVE SPACES TO WS-OUTPUT-LINE
           PERFORM WRITE-OUTPUT
           MOVE "=== ACCOUNT SYSTEM MENU ===" TO WS-OUTPUT-LINE
           PERFORM WRITE-OUTPUT
           MOVE "1. Create New Account" TO WS-OUTPUT-LINE
           PERFORM WRITE-OUTPUT
           MOVE "2. Login to Existing Account" TO WS-OUTPUT-LINE
           PERFORM WRITE-OUTPUT
           MOVE "3. Exit" TO WS-OUTPUT-LINE
           PERFORM WRITE-OUTPUT
           MOVE "Enter your choice (1-3):" TO WS-OUTPUT-LINE
           PERFORM WRITE-OUTPUT

           PERFORM READ-INPUT
           MOVE WS-INPUT-LINE(1:1) TO WS-MENU-CHOICE

           EVALUATE WS-MENU-CHOICE
               WHEN '1'
                   PERFORM CREATE-ACCOUNT
               WHEN '2'
                   PERFORM LOGIN-USER
               WHEN '3'
                   MOVE 'N' TO WS-CONTINUE
                   MOVE "Goodbye!" TO WS-OUTPUT-LINE
                   PERFORM WRITE-OUTPUT
               WHEN OTHER
                   MOVE "Invalid choice. Please enter 1, 2, or 3." TO WS-OUTPUT-LINE
                   PERFORM WRITE-OUTPUT
           END-EVALUATE.

       CREATE-ACCOUNT.
           MOVE SPACES TO WS-OUTPUT-LINE
           PERFORM WRITE-OUTPUT
           MOVE "=== CREATE NEW ACCOUNT ===" TO WS-OUTPUT-LINE
           PERFORM WRITE-OUTPUT

           IF WS-USER-COUNT >= WS-MAX-USERS
              MOVE "All permitted accounts have been created, please come back later." TO WS-OUTPUT-LINE
              PERFORM WRITE-OUTPUT
           ELSE
              PERFORM GET-NEW-USERNAME
              PERFORM CHECK-USERNAME-EXISTS
              IF WS-LOGIN-SUCCESS = 'N'
                 PERFORM GET-NEW-PASSWORD
                 IF WS-HAS-UPPER = 'Y'
                    AND WS-HAS-DIGIT = 'Y'
                    AND WS-HAS-SPECIAL = 'Y'
                    AND WS-VALID-LENGTH = 'Y'
                    ADD 1 TO WS-USER-COUNT
                    MOVE WS-USERNAME TO WS-USER-ID(WS-USER-COUNT)
                    MOVE WS-PASSWORD(1:12) TO WS-USER-PASS(WS-USER-COUNT)
                    MOVE "Account created successfully!" TO WS-OUTPUT-LINE
                    PERFORM WRITE-OUTPUT
                 END-IF
              ELSE
                 MOVE "Username already exists!" TO WS-OUTPUT-LINE
                 PERFORM WRITE-OUTPUT
              END-IF
           END-IF.

       CHECK-USERNAME-EXISTS.
           MOVE 'N' TO WS-LOGIN-SUCCESS
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > WS-USER-COUNT
               IF WS-USERNAME = WS-USER-ID(WS-J)
                   MOVE 'Y' TO WS-LOGIN-SUCCESS
                   EXIT PERFORM
               END-IF
           END-PERFORM.

       LOGIN-USER.
           MOVE SPACES TO WS-OUTPUT-LINE
           PERFORM WRITE-OUTPUT
           MOVE "=== USER LOGIN ===" TO WS-OUTPUT-LINE
           PERFORM WRITE-OUTPUT

           IF WS-USER-COUNT = 0
              MOVE "No accounts exist. Please create an account first." TO WS-OUTPUT-LINE
              PERFORM WRITE-OUTPUT
              EXIT PARAGRAPH
           END-IF

           PERFORM GET-LOGIN-CREDENTIALS
           PERFORM VALIDATE-LOGIN

           IF WS-LOGIN-SUCCESS = 'Y'
              MOVE "Login successful! Welcome back!" TO WS-OUTPUT-LINE
              PERFORM WRITE-OUTPUT
              PERFORM USER-DASHBOARD
           ELSE
              MOVE "Invalid username or password." TO WS-OUTPUT-LINE
              PERFORM WRITE-OUTPUT
           END-IF.

       GET-LOGIN-CREDENTIALS.
           MOVE "Enter username:" TO WS-OUTPUT-LINE
           PERFORM WRITE-OUTPUT
           PERFORM READ-INPUT
           MOVE WS-INPUT-LINE TO WS-LOGIN-USERNAME

           MOVE "Enter password:" TO WS-OUTPUT-LINE
           PERFORM WRITE-OUTPUT
           PERFORM READ-INPUT
           MOVE WS-INPUT-LINE TO WS-LOGIN-PASSWORD.

       VALIDATE-LOGIN.
           MOVE 'N' TO WS-LOGIN-SUCCESS
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > WS-USER-COUNT
               IF WS-LOGIN-USERNAME = WS-USER-ID(WS-J)
                   *> Compare first 12 characters of entered password with stored password
                   IF WS-LOGIN-PASSWORD(1:12) = WS-USER-PASS(WS-J)
                       MOVE 'Y' TO WS-LOGIN-SUCCESS
                       EXIT PERFORM
                   END-IF
               END-IF
           END-PERFORM.

       USER-DASHBOARD.
           MOVE SPACES TO WS-OUTPUT-LINE
           PERFORM WRITE-OUTPUT
           MOVE "=== USER DASHBOARD ===" TO WS-OUTPUT-LINE
           PERFORM WRITE-OUTPUT

           MOVE SPACES TO WS-OUTPUT-LINE
           STRING "Welcome, " FUNCTION TRIM(WS-LOGIN-USERNAME) "!"
               INTO WS-OUTPUT-LINE
           PERFORM WRITE-OUTPUT

           MOVE "You have successfully logged into the system." TO WS-OUTPUT-LINE
           PERFORM WRITE-OUTPUT

           MOVE "Press any key to return to main menu..." TO WS-OUTPUT-LINE
           PERFORM WRITE-OUTPUT
           PERFORM READ-INPUT.

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

       VALIDATE-PASSWORD.
           MOVE 'N' TO WS-HAS-UPPER WS-HAS-DIGIT WS-HAS-SPECIAL WS-VALID-LENGTH
           MOVE 0  TO WS-PASSWORD-LENGTH

           *> Count characters until space or end of buffer
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > LENGTH OF WS-PASSWORD
               IF WS-PASSWORD(WS-I:1) NOT = SPACE
                   ADD 1 TO WS-PASSWORD-LENGTH
               ELSE
                   EXIT PERFORM
               END-IF
           END-PERFORM

           *> Immediately reject if length is wrong
           IF WS-PASSWORD-LENGTH < 8 OR WS-PASSWORD-LENGTH > 12
               MOVE "Password must be 8-12 chars" TO WS-OUTPUT-LINE
               PERFORM WRITE-OUTPUT
               EXIT PARAGRAPH
           END-IF

           MOVE 'Y' TO WS-VALID-LENGTH

           *> Only run character tests when length is valid
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-PASSWORD-LENGTH
               MOVE WS-PASSWORD(WS-I:1) TO WS-CHAR
               IF WS-CHAR >= 'A' AND WS-CHAR <= 'Z'
                   MOVE 'Y' TO WS-HAS-UPPER
               END-IF
               IF WS-CHAR >= '0' AND WS-CHAR <= '9'
                   MOVE 'Y' TO WS-HAS-DIGIT
               END-IF
               IF WS-CHAR = '!' OR WS-CHAR = '@' OR WS-CHAR = '#' OR
                  WS-CHAR = '$' OR WS-CHAR = '%' OR WS-CHAR = '^' OR
                  WS-CHAR = '&' OR WS-CHAR = '*'
                   MOVE 'Y' TO WS-HAS-SPECIAL
               END-IF
           END-PERFORM

           IF WS-HAS-UPPER = 'N'
               MOVE "Password needs uppercase" TO WS-OUTPUT-LINE
               PERFORM WRITE-OUTPUT
           END-IF
           IF WS-HAS-DIGIT = 'N'
               MOVE "Password needs digit" TO WS-OUTPUT-LINE
               PERFORM WRITE-OUTPUT
           END-IF
           IF WS-HAS-SPECIAL = 'N'
               MOVE "Password needs special (!,@,#,$...)" TO WS-OUTPUT-LINE
               PERFORM WRITE-OUTPUT
           END-IF.

       READ-INPUT.
           ACCEPT WS-INPUT-LINE.

       WRITE-OUTPUT.
           DISPLAY WS-OUTPUT-LINE.
