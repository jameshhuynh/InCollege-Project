       >>SOURCE FORMAT FREE
       *> IDAHO-5: Combined Account System + Login Menu
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STUDENT-SYSTEM.
       AUTHOR. STUDENT.

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
       01 WS-USER-CHOICE      PIC X.
       01  WS-FILE-STATUS    PIC XX VALUE SPACES.
       01  WS-INPUT-LINE     PIC X(80).
       01  WS-OUTPUT-LINE    PIC X(80).

       01  WS-USERNAME       PIC X(20).
       01  WS-PASSWORD       PIC X(20).

       01  WS-USER-COUNT     PIC 99 VALUE 0.
       01  WS-MAX-USERS      PIC 99 VALUE 5.

       01  WS-PASSWORD-FLAGS.
           05 WS-HAS-UPPER    PIC X VALUE 'N'.
           05 WS-HAS-DIGIT    PIC X VALUE 'N'.
           05 WS-HAS-SPECIAL  PIC X VALUE 'N'.
           05 WS-VALID-LENGTH PIC X VALUE 'N'.

       01 WS-CHAR            PIC X.
       01 WS-I               PIC 99.
       01 WS-J               PIC 99.
       01 WS-PASSWORD-LENGTH PIC 99.

       01 WS-LOGIN-USERNAME  PIC X(20).
       01 WS-LOGIN-PASSWORD  PIC X(20).
       01 WS-LOGIN-SUCCESS   PIC X VALUE 'N'.

       01 WS-MENU-CHOICE     PIC X.
       01 WS-SKILL-CHOICE    PIC X.
       01 WS-CONTINUE        PIC X VALUE 'Y'.

       01 WS-USER-TABLE.
          05 WS-USER-ENTRY OCCURS 5 TIMES.
             10 WS-USER-ID   PIC X(20).
             10 WS-USER-PASS PIC X(12).

       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM ENSURE-FILE
           PERFORM LOAD-USERS
           PERFORM MAIN-MENU UNTIL WS-CONTINUE = 'N'
           STOP RUN.

       *>--------------------------------------------------
       ENSURE-FILE.
           OPEN INPUT USER-FILE
           IF WS-FILE-STATUS = "35"
              OPEN OUTPUT USER-FILE
              CLOSE USER-FILE
           ELSE
              CLOSE USER-FILE
           END-IF.

       *>--------------------------------------------------
       LOAD-USERS.
           MOVE 0 TO WS-USER-COUNT
           OPEN INPUT USER-FILE
           IF WS-FILE-STATUS = "00"
              PERFORM READ-USER-RECORD
              PERFORM UNTIL WS-FILE-STATUS NOT = "00" OR WS-USER-COUNT >= WS-MAX-USERS
                  PERFORM PARSE-USER-RECORD
                  PERFORM READ-USER-RECORD
              END-PERFORM
              CLOSE USER-FILE
           END-IF.

       READ-USER-RECORD.
           READ USER-FILE INTO USER-REC
           END-READ.

       PARSE-USER-RECORD.
           MOVE 1 TO WS-I
           PERFORM UNTIL WS-I > 80 OR USER-REC(WS-I:1) = ","
               ADD 1 TO WS-I
           END-PERFORM
           IF WS-I <= 80 AND USER-REC(WS-I:1) = ","
               ADD 1 TO WS-USER-COUNT
               MOVE USER-REC(1:WS-I - 1) TO WS-USER-ID(WS-USER-COUNT)
               COMPUTE WS-J = WS-I + 1
               MOVE USER-REC(WS-J:12) TO WS-USER-PASS(WS-USER-COUNT)
           END-IF.

       *>--------------------------------------------------
       MAIN-MENU.
           DISPLAY "========================================"
           DISPLAY "         WELCOME TO INCOLLEGE"
           DISPLAY "========================================"
           DISPLAY "1. Create New Account"
           DISPLAY "2. Login to Existing Account"
           DISPLAY "3. Exit"
           DISPLAY " "
           DISPLAY "Enter your choice (1-3): " WITH NO ADVANCING
           ACCEPT WS-MENU-CHOICE

           EVALUATE WS-MENU-CHOICE
               WHEN '1'
                   PERFORM CREATE-ACCOUNT
               WHEN '2'
                   PERFORM LOGIN-USER
               WHEN '3'
                   MOVE 'N' TO WS-CONTINUE
                   DISPLAY "Goodbye!"
               WHEN OTHER
                   DISPLAY "Invalid choice. Please enter 1-3."
           END-EVALUATE.

       *>--------------------------------------------------
       CREATE-ACCOUNT.
           DISPLAY "=== CREATE NEW ACCOUNT ==="

           IF WS-USER-COUNT >= WS-MAX-USERS
               DISPLAY "All permitted accounts have been created, please come back later."
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
                      PERFORM SAVE-USER-TO-FILE
                      DISPLAY "Account created successfully!"
                   END-IF
               ELSE
                   DISPLAY "Username already exists!"
               END-IF
           END-IF.

       SAVE-USER-TO-FILE.
           OPEN EXTEND USER-FILE
           IF WS-FILE-STATUS = "00"
              STRING WS-USERNAME DELIMITED BY SPACE
                     "," DELIMITED BY SIZE
                     WS-PASSWORD(1:12) DELIMITED BY SPACE
                     INTO USER-REC
              END-STRING
              WRITE USER-REC
              CLOSE USER-FILE
           END-IF.

       CHECK-USERNAME-EXISTS.
           MOVE 'N' TO WS-LOGIN-SUCCESS
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > WS-USER-COUNT
               IF WS-USERNAME = WS-USER-ID(WS-J)
                   MOVE 'Y' TO WS-LOGIN-SUCCESS
                   EXIT PERFORM
               END-IF
           END-PERFORM.

       *>--------------------------------------------------
       LOGIN-USER.
           DISPLAY "=== USER LOGIN ==="

           IF WS-USER-COUNT = 0
              DISPLAY "No accounts exist. Please create one first."
              EXIT PARAGRAPH
           END-IF

           PERFORM GET-LOGIN-CREDENTIALS
           PERFORM VALIDATE-LOGIN

           IF WS-LOGIN-SUCCESS = 'Y'
              DISPLAY "You have successfully logged in!"
              PERFORM USER-DASHBOARD
           ELSE
              DISPLAY "Incorrect username/password, please try again."
           END-IF.

       GET-LOGIN-CREDENTIALS.
           DISPLAY "Enter username: " WITH NO ADVANCING
           ACCEPT WS-LOGIN-USERNAME
           DISPLAY "Enter password: " WITH NO ADVANCING
           ACCEPT WS-LOGIN-PASSWORD.

       VALIDATE-LOGIN.
           MOVE 'N' TO WS-LOGIN-SUCCESS
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > WS-USER-COUNT
               IF WS-LOGIN-USERNAME = WS-USER-ID(WS-J)
                   IF WS-LOGIN-PASSWORD(1:12) = WS-USER-PASS(WS-J)
                       MOVE 'Y' TO WS-LOGIN-SUCCESS
                       EXIT PERFORM
                   END-IF
               END-IF
           END-PERFORM.

       *>--------------------------------------------------
       USER-DASHBOARD.
           DISPLAY " "
           DISPLAY "========================================"
           DISPLAY "           MAIN MENU"
           DISPLAY "========================================"
           DISPLAY "1. Search for a Job/Internship"
           DISPLAY "2. Find Someone You Know"
           DISPLAY "3. Learn a New Skill"
           DISPLAY "4. Exit System"
           DISPLAY " "
           DISPLAY "Please select an option (1-4): " WITH NO ADVANCING
           ACCEPT WS-MENU-CHOICE

           EVALUATE WS-MENU-CHOICE
              WHEN '1' PERFORM JOB-SEARCH-OPTION
              WHEN '2' PERFORM FIND-SOMEONE-OPTION
              WHEN '3' PERFORM LEARN-SKILL-OPTION
              WHEN '4' DISPLAY "Logging out... Goodbye!"
              WHEN OTHER DISPLAY "Invalid option."
           END-EVALUATE.

       JOB-SEARCH-OPTION.
           DISPLAY "*** UNDER CONSTRUCTION ***".
           ACCEPT WS-USER-CHOICE.

       FIND-SOMEONE-OPTION.
           DISPLAY "*** UNDER CONSTRUCTION ***".
           ACCEPT WS-USER-CHOICE.

       LEARN-SKILL-OPTION.
       DISPLAY "Select a skill to learn:".
       DISPLAY "1. Python Programming"
       DISPLAY "2. Data Analysis with Excel"
       DISPLAY "3. Digital Marketing"
       DISPLAY "4. Graphic Design"
       DISPLAY "5. Public Speaking"
       DISPLAY "6. Return to Main Menu"
       ACCEPT WS-SKILL-CHOICE

       EVALUATE WS-SKILL-CHOICE
           WHEN '1'
               DISPLAY "This skill is under construction."
           WHEN '2'
               DISPLAY "This skill is under construction."
           WHEN '3'
               DISPLAY "This skill is under construction."
           WHEN '4'
               DISPLAY "This skill is under construction."
           WHEN '5'
               DISPLAY "This skill is under construction."
           WHEN '6'
               DISPLAY "Returning to Main Menu..."
           WHEN OTHER
               DISPLAY "Invalid choice."
       END-EVALUATE.

       GET-NEW-USERNAME.
           DISPLAY "Enter username: " WITH NO ADVANCING
           ACCEPT WS-USERNAME.

       GET-NEW-PASSWORD.
           DISPLAY "Enter password: " WITH NO ADVANCING
           ACCEPT WS-PASSWORD
           PERFORM VALIDATE-PASSWORD.

       VALIDATE-PASSWORD.
           MOVE 'N' TO WS-HAS-UPPER WS-HAS-DIGIT WS-HAS-SPECIAL WS-VALID-LENGTH
           MOVE 0  TO WS-PASSWORD-LENGTH

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > LENGTH OF WS-PASSWORD
               IF WS-PASSWORD(WS-I:1) NOT = SPACE
                   ADD 1 TO WS-PASSWORD-LENGTH
               ELSE
                   EXIT PERFORM
               END-IF
           END-PERFORM

           IF WS-PASSWORD-LENGTH < 8 OR WS-PASSWORD-LENGTH > 12
               DISPLAY "Password must be 8-12 characters"
               EXIT PARAGRAPH
           END-IF

           MOVE 'Y' TO WS-VALID-LENGTH

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
               DISPLAY "Password needs uppercase"
           END-IF
           IF WS-HAS-DIGIT = 'N'
               DISPLAY "Password needs digit"
           END-IF
           IF WS-HAS-SPECIAL = 'N'
               DISPLAY "Password needs special (!,@,#,$,...)"
           END-IF.
           
