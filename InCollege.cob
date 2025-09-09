       >>SOURCE FORMAT FREE
       *> InCollege Project: Week 3 Deliverable - Complete Implementation
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INCOLLEGE.
       AUTHOR. STUDENT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT USER-FILE ASSIGN TO "users.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-USER-FILE-STATUS.

           SELECT INPUT-FILE ASSIGN TO "InCollege-Input.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-INPUT-FILE-STATUS.

           SELECT OUTPUT-FILE ASSIGN TO "InCollege-Output.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-OUTPUT-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  USER-FILE.
       01  USER-REC       PIC X(80).

       FD  INPUT-FILE.
       01  INPUT-REC      PIC X(80).

       FD  OUTPUT-FILE.
       01  OUTPUT-REC     PIC X(80).

       WORKING-STORAGE SECTION.
       01  WS-USER-FILE-STATUS    PIC XX VALUE SPACES.
       01  WS-INPUT-FILE-STATUS   PIC XX VALUE SPACES.
       01  WS-OUTPUT-FILE-STATUS  PIC XX VALUE SPACES.

       01  WS-INPUT-LINE          PIC X(80).
       01  WS-OUTPUT-LINE         PIC X(80).
       01  WS-USER-CHOICE         PIC X.

       01  WS-USERNAME            PIC X(20).
       01  WS-PASSWORD            PIC X(20).
       01  WS-LOGIN-USERNAME      PIC X(20).
       01  WS-LOGIN-PASSWORD      PIC X(20).

       01  WS-USER-COUNT          PIC 99 VALUE 0.
       01  WS-MAX-USERS           PIC 99 VALUE 5.

       01  WS-PASSWORD-FLAGS.
           05 WS-HAS-UPPER        PIC X VALUE 'N'.
           05 WS-HAS-DIGIT        PIC X VALUE 'N'.
           05 WS-HAS-SPECIAL      PIC X VALUE 'N'.
           05 WS-VALID-LENGTH     PIC X VALUE 'N'.

       01  WS-CHAR                PIC X.
       01  WS-I                   PIC 99.
       01  WS-J                   PIC 99.
       01  WS-PASSWORD-LENGTH     PIC 99.
       01  WS-LOGIN-SUCCESS       PIC X VALUE 'N'.
       01  WS-MENU-CHOICE         PIC X.
       01  WS-SKILL-CHOICE        PIC X.
       01  WS-CONTINUE            PIC X VALUE 'Y'.
       01  WS-LOGGED-IN           PIC X VALUE 'N'.
       01  WS-CURRENT-USER        PIC X(20).

       01  WS-USER-TABLE.
          05 WS-USER-ENTRY OCCURS 5 TIMES.
             10 WS-USER-ID         PIC X(20).
             10 WS-USER-PASS       PIC X(12).

       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM INITIALIZE-FILES
           PERFORM LOAD-USERS
           PERFORM MAIN-PROGRAM
           PERFORM CLOSE-FILES
           STOP RUN.

       *>--------------------------------------------------
       INITIALIZE-FILES.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE
           PERFORM ENSURE-USER-FILE.

       CLOSE-FILES.
           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE.

       ENSURE-USER-FILE.
           OPEN INPUT USER-FILE
           IF WS-USER-FILE-STATUS = "35"
              CLOSE USER-FILE
              OPEN OUTPUT USER-FILE
              CLOSE USER-FILE
           ELSE
              CLOSE USER-FILE
           END-IF.

       *>--------------------------------------------------
       LOAD-USERS.
           MOVE 0 TO WS-USER-COUNT
           OPEN INPUT USER-FILE
           IF WS-USER-FILE-STATUS = "00"
              PERFORM READ-USER-RECORD
              PERFORM UNTIL WS-USER-FILE-STATUS NOT = "00" OR WS-USER-COUNT >= WS-MAX-USERS
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
       MAIN-PROGRAM.
           PERFORM DISPLAY-AND-WRITE WITH "Welcome to InCollege!"
           PERFORM INITIAL-MENU UNTIL WS-CONTINUE = 'N'.

       INITIAL-MENU.
           PERFORM DISPLAY-AND-WRITE WITH "Log In"
           PERFORM DISPLAY-AND-WRITE WITH "Create New Account"
           PERFORM DISPLAY-AND-WRITE WITH "Enter your choice:"
           PERFORM READ-INPUT
           MOVE WS-INPUT-LINE(1:1) TO WS-USER-CHOICE

           EVALUATE WS-USER-CHOICE
               WHEN '1'
                   PERFORM LOGIN-PROCESS
               WHEN '2'
                   PERFORM CREATE-ACCOUNT-PROCESS
               WHEN OTHER
                   MOVE 'N' TO WS-CONTINUE
           END-EVALUATE.

       *>--------------------------------------------------
       CREATE-ACCOUNT-PROCESS.
           IF WS-USER-COUNT >= WS-MAX-USERS
               PERFORM DISPLAY-AND-WRITE WITH "All permitted accounts have been created, please come back later"
           ELSE
               PERFORM GET-NEW-USERNAME
               PERFORM CHECK-USERNAME-EXISTS
               IF WS-LOGIN-SUCCESS = 'N'
                   PERFORM GET-NEW-PASSWORD
                   PERFORM VALIDATE-PASSWORD
                   IF WS-HAS-UPPER = 'Y'
                      AND WS-HAS-DIGIT = 'Y'
                      AND WS-HAS-SPECIAL = 'Y'
                      AND WS-VALID-LENGTH = 'Y'
                      ADD 1 TO WS-USER-COUNT
                      MOVE WS-USERNAME TO WS-USER-ID(WS-USER-COUNT)
                      MOVE WS-PASSWORD(1:12) TO WS-USER-PASS(WS-USER-COUNT)
                      PERFORM SAVE-USER-TO-FILE
                   END-IF
               END-IF
           END-IF.

       GET-NEW-USERNAME.
           PERFORM DISPLAY-AND-WRITE WITH "Please enter your username:"
           PERFORM READ-INPUT
           MOVE WS-INPUT-LINE TO WS-USERNAME.

       GET-NEW-PASSWORD.
           PERFORM DISPLAY-AND-WRITE WITH "Please enter your password:"
           PERFORM READ-INPUT
           MOVE WS-INPUT-LINE TO WS-PASSWORD.

       CHECK-USERNAME-EXISTS.
           MOVE 'N' TO WS-LOGIN-SUCCESS
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > WS-USER-COUNT
               IF WS-USERNAME = WS-USER-ID(WS-J)
                   MOVE 'Y' TO WS-LOGIN-SUCCESS
                   EXIT PERFORM
               END-IF
           END-PERFORM.

       SAVE-USER-TO-FILE.
           OPEN EXTEND USER-FILE
           IF WS-USER-FILE-STATUS = "00"
              STRING WS-USERNAME DELIMITED BY SPACE
                     "," DELIMITED BY SIZE
                     WS-PASSWORD(1:12) DELIMITED BY SPACE
                     INTO USER-REC
              END-STRING
              WRITE USER-REC
              CLOSE USER-FILE
           END-IF.

       *>--------------------------------------------------
       LOGIN-PROCESS.
           MOVE 'N' TO WS-LOGIN-SUCCESS
           PERFORM LOGIN-ATTEMPT UNTIL WS-LOGIN-SUCCESS = 'Y'

           IF WS-LOGIN-SUCCESS = 'Y'
              PERFORM DISPLAY-AND-WRITE WITH "You have successfully logged in"
              STRING "Welcome, " DELIMITED BY SIZE
                     WS-CURRENT-USER DELIMITED BY SPACE
                     "!" DELIMITED BY SIZE
                     INTO WS-OUTPUT-LINE
              END-STRING
              PERFORM DISPLAY-AND-WRITE WITH WS-OUTPUT-LINE
              MOVE 'Y' TO WS-LOGGED-IN
              PERFORM POST-LOGIN-MENU
           END-IF.

       LOGIN-ATTEMPT.
           PERFORM DISPLAY-AND-WRITE WITH "Please enter your username:"
           PERFORM READ-INPUT
           MOVE WS-INPUT-LINE TO WS-LOGIN-USERNAME

           PERFORM DISPLAY-AND-WRITE WITH "Please enter your password:"
           PERFORM READ-INPUT
           MOVE WS-INPUT-LINE TO WS-LOGIN-PASSWORD

           PERFORM VALIDATE-LOGIN

           IF WS-LOGIN-SUCCESS = 'N'
              PERFORM DISPLAY-AND-WRITE WITH "Incorrect username/password, please try again"
           END-IF.

       VALIDATE-LOGIN.
           MOVE 'N' TO WS-LOGIN-SUCCESS
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > WS-USER-COUNT
               IF WS-LOGIN-USERNAME = WS-USER-ID(WS-J)
                   IF WS-LOGIN-PASSWORD(1:12) = WS-USER-PASS(WS-J)
                       MOVE 'Y' TO WS-LOGIN-SUCCESS
                       MOVE WS-LOGIN-USERNAME TO WS-CURRENT-USER
                       EXIT PERFORM
                   END-IF
               END-IF
           END-PERFORM.

       *>--------------------------------------------------
       POST-LOGIN-MENU.
           PERFORM POST-LOGIN-OPTIONS UNTIL WS-LOGGED-IN = 'N'.

       POST-LOGIN-OPTIONS.
           PERFORM DISPLAY-AND-WRITE WITH "Search for a job"
           PERFORM DISPLAY-AND-WRITE WITH "Find someone you know"
           PERFORM DISPLAY-AND-WRITE WITH "Learn a new skill"
           PERFORM DISPLAY-AND-WRITE WITH "Enter your choice:"
           PERFORM READ-INPUT
           MOVE WS-INPUT-LINE(1:1) TO WS-MENU-CHOICE

           EVALUATE WS-MENU-CHOICE
               WHEN '1'
                   PERFORM DISPLAY-AND-WRITE WITH "Job search/internship is under construction."
               WHEN '2'
                   PERFORM DISPLAY-AND-WRITE WITH "Find someone you know is under construction."
               WHEN '3'
                   PERFORM LEARN-SKILL-MENU
               WHEN OTHER
                   MOVE 'N' TO WS-LOGGED-IN
           END-EVALUATE.

       *>--------------------------------------------------
       LEARN-SKILL-MENU.
           PERFORM DISPLAY-AND-WRITE WITH "Learn a New Skill:"
           PERFORM DISPLAY-AND-WRITE WITH "Skill 1"
           PERFORM DISPLAY-AND-WRITE WITH "Skill 2"
           PERFORM DISPLAY-AND-WRITE WITH "Skill 3"
           PERFORM DISPLAY-AND-WRITE WITH "Skill 4"
           PERFORM DISPLAY-AND-WRITE WITH "Skill 5"
           PERFORM DISPLAY-AND-WRITE WITH "Go Back"
           PERFORM DISPLAY-AND-WRITE WITH "Enter your choice:"
           PERFORM READ-INPUT
           MOVE WS-INPUT-LINE(1:1) TO WS-SKILL-CHOICE

           EVALUATE WS-SKILL-CHOICE
               WHEN '1' THROUGH '5'
                   PERFORM DISPLAY-AND-WRITE WITH "This skill is under construction."
               WHEN '6'
                   CONTINUE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.

       *>--------------------------------------------------
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
           END-PERFORM.

       *>--------------------------------------------------
       *> UTILITY PARAGRAPHS FOR FILE I/O
       *>--------------------------------------------------
       READ-INPUT.
           READ INPUT-FILE INTO INPUT-REC
           MOVE INPUT-REC TO WS-INPUT-LINE.

       DISPLAY-AND-WRITE.
           DISPLAY WS-OUTPUT-LINE
           WRITE OUTPUT-REC FROM WS-OUTPUT-LINE.
