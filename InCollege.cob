
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STUDENT-SYSTEM.
       AUTHOR. STUDENT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT USER-FILE ASSIGN TO "users.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

           SELECT PROFILE-FILE ASSIGN TO "profiles.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-PROF-STATUS.

           SELECT OUTPUT-FILE ASSIGN TO "InCollege-Output.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.

           SELECT REQUESTS-FILE ASSIGN TO "requests.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-REQ-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  USER-FILE.
       01  USER-REC       PIC X(80).

       FD  PROFILE-FILE.
       01  PROFILE-REC    PIC X(900).

       FD  OUTPUT-FILE.
       01  OUT-REC        PIC X(300).

       FD  REQUESTS-FILE.
       01  REQ-REC        PIC X(50).

       WORKING-STORAGE SECTION.
       01  WS-USER-CHOICE     PIC X.
       01  WS-FILE-STATUS     PIC XX VALUE SPACES.
       01  WS-PROF-STATUS     PIC XX VALUE SPACES.
       01  WS-OUT-STATUS      PIC XX VALUE SPACES.
       01  WS-REQ-STATUS      PIC XX VALUE SPACES.
       01  WS-INPUT-LINE      PIC X(200).
       01  WS-OUTPUT-LINE     PIC X(300).

       01  WS-USERNAME        PIC X(20).
       01  WS-PASSWORD        PIC X(20).

       01  WS-USER-COUNT      PIC 99 VALUE 0.
       01  WS-MAX-USERS       PIC 99 VALUE 5.

       01  WS-PASSWORD-FLAGS.
           05 WS-HAS-UPPER    PIC X VALUE 'N'.
           05 WS-HAS-DIGIT    PIC X VALUE 'N'.
           05 WS-HAS-SPECIAL  PIC X VALUE 'N'.
           05 WS-VALID-LENGTH PIC X VALUE 'N'.

       01  WS-CHAR            PIC X.
       01  WS-I               PIC 99.
       01  WS-J               PIC 99.
       01  WS-PASSWORD-LENGTH PIC 99.

       01  WS-LOGIN-USERNAME  PIC X(20).
       01  WS-LOGIN-PASSWORD  PIC X(20).
       01  WS-LOGIN-SUCCESS   PIC X VALUE 'N'.

       01  WS-MENU-CHOICE     PIC X.
       01  WS-SKILL-CHOICE    PIC X.
       01  WS-CONTINUE        PIC X VALUE 'Y'.

       01  WS-USER-TABLE.
          05 WS-USER-ENTRY OCCURS 5 TIMES.
             10 WS-USER-ID   PIC X(20).
             10 WS-USER-PASS PIC X(12).

       01  WS-PROFILE.
          05 PF-USERNAME           PIC X(20).
          05 PF-FIRST-NAME         PIC X(30).
          05 PF-LAST-NAME          PIC X(30).
          05 PF-UNIVERSITY         PIC X(50).
          05 PF-MAJOR              PIC X(40).
          05 PF-GRAD-YEAR          PIC 9(4).
          05 PF-ABOUT-ME           PIC X(200).
          05 PF-EXP-COUNT          PIC 9 VALUE 0.
          05 PF-EXP OCCURS 3 TIMES.
             10 PF-EXP-TITLE       PIC X(30).
             10 PF-EXP-COMPANY     PIC X(30).
             10 PF-EXP-DATES       PIC X(20).
             10 PF-EXP-DESC        PIC X(100).
          05 PF-EDU-COUNT          PIC 9 VALUE 0.
          05 PF-EDU OCCURS 3 TIMES.
             10 PF-EDU-DEGREE      PIC X(30).
             10 PF-EDU-UNIV        PIC X(50).
             10 PF-EDU-YEARS       PIC X(20).

       01  WS-PROF-KEYLINE        PIC X(80).
       01  WS-TEMP-NUMERIC        PIC 9(4).
       01  WS-DISPLAY-MESSAGE     PIC X(300).
       01  WS-REC-USERNAME        PIC X(20).

       01 WS-SEARCH-NAME          PIC X(40).
       01 WS-SEARCH-FIRST         PIC X(20).
       01 WS-SEARCH-LAST          PIC X(20).
       01 WS-NAME-FOUND           PIC X VALUE "N".
       01 EOF-PROFILE             PIC X VALUE 'N'.

       *> Connection requests variables
       01 WS-SENDER-USERNAME      PIC X(20).
       01 WS-RECIP-USERNAME       PIC X(20).
       01 WS-REQUESTS-COUNT       PIC 99 VALUE 0.

       01 WS-REQUESTS-TABLE.
          05 WS-REQUEST-ENTRY OCCURS 100 TIMES.
             10 WS-REQ-SENDER      PIC X(20).
             10 WS-REQ-RECIP       PIC X(20).

       01 EOF-REQUESTS            PIC X VALUE 'N'.
       01 WS-REQUEST-EXISTS       PIC X VALUE 'N'.

       PROCEDURE DIVISION.

       MAIN-PARA.
           PERFORM ENSURE-FILES
           PERFORM LOAD-USERS
           PERFORM LOAD-REQUESTS
           OPEN OUTPUT OUTPUT-FILE
           PERFORM MAIN-MENU UNTIL WS-CONTINUE = 'N'
           CLOSE OUTPUT-FILE
           PERFORM SAVE-REQUESTS
           STOP RUN.

       ENSURE-FILES.
           OPEN INPUT USER-FILE
           IF WS-FILE-STATUS = "35"
              OPEN OUTPUT USER-FILE
              CLOSE USER-FILE
           ELSE
              CLOSE USER-FILE
           END-IF

           OPEN INPUT PROFILE-FILE
           IF WS-PROF-STATUS = "35"
              OPEN OUTPUT PROFILE-FILE
              CLOSE PROFILE-FILE
           ELSE
              CLOSE PROFILE-FILE
           END-IF

           OPEN INPUT REQUESTS-FILE
           IF WS-REQ-STATUS = "35"
              OPEN OUTPUT REQUESTS-FILE
              CLOSE REQUESTS-FILE
           ELSE
              CLOSE REQUESTS-FILE
           END-IF.

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

       LOAD-REQUESTS.
           MOVE 0 TO WS-REQUESTS-COUNT
           OPEN INPUT REQUESTS-FILE
           IF WS-REQ-STATUS = "00"
              PERFORM READ-REQUEST-RECORD
              PERFORM UNTIL WS-REQ-STATUS NOT = "00"
                  PERFORM PARSE-REQUEST-REC
                  PERFORM READ-REQUEST-RECORD
              END-PERFORM
              CLOSE REQUESTS-FILE
           END-IF.

       READ-USER-RECORD.
           READ USER-FILE INTO USER-REC
           AT END
               MOVE "EOF" TO WS-FILE-STATUS
           NOT AT END
               MOVE "00" TO WS-FILE-STATUS
           END-READ.

       READ-REQUEST-RECORD.
           READ REQUESTS-FILE INTO REQ-REC
           AT END
               MOVE "EOF" TO WS-REQ-STATUS
           NOT AT END
               MOVE "00" TO WS-REQ-STATUS
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

       PARSE-REQUEST-REC.
           UNSTRING REQ-REC DELIMITED BY ","
               INTO WS-REQ-SENDER(WS-REQUESTS-COUNT + 1)
                    WS-REQ-RECIP(WS-REQUESTS-COUNT + 1)
           END-UNSTRING
           ADD 1 TO WS-REQUESTS-COUNT.

       MAIN-MENU.
           MOVE "======================================" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY
           MOVE "WELCOME TO INCOLLEGE" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY
           MOVE "======================================" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY
           MOVE "1. Create New Account" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY
           MOVE "2. Login to Existing Account" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY
           MOVE "3. Exit" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY
           DISPLAY "Enter your choice (1-3): " WITH NO ADVANCING
           ACCEPT WS-MENU-CHOICE

           EVALUATE WS-MENU-CHOICE
               WHEN '1'
                   PERFORM CREATE-ACCOUNT
               WHEN '2'
                   PERFORM LOGIN-USER
               WHEN '3'
                   MOVE 'N' TO WS-CONTINUE
                   MOVE "Goodbye!" TO WS-DISPLAY-MESSAGE
                   PERFORM WRITE-OUTPUT-AND-DISPLAY
               WHEN OTHER
                   MOVE "Invalid choice. Please enter 1-3." TO WS-DISPLAY-MESSAGE
                   PERFORM WRITE-OUTPUT-AND-DISPLAY
           END-EVALUATE.

       CREATE-ACCOUNT.
           MOVE "=== CREATE NEW ACCOUNT ===" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY

           IF WS-USER-COUNT >= WS-MAX-USERS
               MOVE "All permitted accounts have been created, please come back later." TO WS-DISPLAY-MESSAGE
               PERFORM WRITE-OUTPUT-AND-DISPLAY
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
                      MOVE "Account created successfully!" TO WS-DISPLAY-MESSAGE
                      PERFORM WRITE-OUTPUT-AND-DISPLAY
                   ELSE
                      MOVE "Password does not meet requirements." TO WS-DISPLAY-MESSAGE
                      PERFORM WRITE-OUTPUT-AND-DISPLAY
                   END-IF
               ELSE
                   MOVE "Username already exists!" TO WS-DISPLAY-MESSAGE
                   PERFORM WRITE-OUTPUT-AND-DISPLAY
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

       GET-NEW-USERNAME.
           DISPLAY "Enter desired username: " WITH NO ADVANCING
           ACCEPT WS-USERNAME.

       GET-NEW-PASSWORD.
           DISPLAY "Enter password: " WITH NO ADVANCING
           ACCEPT WS-PASSWORD
           *> Basic password checks (examples)
           MOVE 'N' TO WS-HAS-UPPER
           MOVE 'N' TO WS-HAS-DIGIT
           MOVE 'N' TO WS-HAS-SPECIAL
           MOVE 'N' TO WS-VALID-LENGTH
           COMPUTE WS-PASSWORD-LENGTH = FUNCTION LENGTH(WS-PASSWORD)
           IF WS-PASSWORD-LENGTH >= 8
              MOVE 'Y' TO WS-VALID-LENGTH
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-PASSWORD-LENGTH
               MOVE WS-PASSWORD(WS-I:1) TO WS-CHAR
               IF WS-CHAR >= 'A' AND WS-CHAR <= 'Z'
                  MOVE 'Y' TO WS-HAS-UPPER
               ELSE IF WS-CHAR >= '0' AND WS-CHAR <= '9'
                  MOVE 'Y' TO WS-HAS-DIGIT
               ELSE
                  *> treat other chars as special for simplicity
                  MOVE 'Y' TO WS-HAS-SPECIAL
               END-IF
           END-PERFORM.

       LOGIN-USER.
           MOVE "=== USER LOGIN ===" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY

           IF WS-USER-COUNT = 0
              MOVE "No accounts exist. Please create one first." TO WS-DISPLAY-MESSAGE
              PERFORM WRITE-OUTPUT-AND-DISPLAY
              EXIT PARAGRAPH
           END-IF

           PERFORM GET-LOGIN-CREDENTIALS
           PERFORM VALIDATE-LOGIN

           IF WS-LOGIN-SUCCESS = 'Y'
              MOVE "You have successfully logged in!" TO WS-DISPLAY-MESSAGE
              PERFORM WRITE-OUTPUT-AND-DISPLAY
              MOVE WS-LOGIN-USERNAME TO PF-USERNAME
              PERFORM LOAD-PROFILE-FOR-USER
              PERFORM USER-DASHBOARD UNTIL WS-MENU-CHOICE = '5'
           ELSE
              MOVE "Incorrect username/password, please try again." TO WS-DISPLAY-MESSAGE
              PERFORM WRITE-OUTPUT-AND-DISPLAY
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
                       MOVE WS-LOGIN-USERNAME TO WS-REC-USERNAME
                       EXIT PERFORM
                   END-IF
               END-IF
           END-PERFORM.

       USER-DASHBOARD.
           DISPLAY " "
           MOVE "======================================" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY
           MOVE "MAIN MENU" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY
           MOVE "======================================" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY
           MOVE "1. Create/Edit My Profile" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY
           MOVE "2. View My Profile" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY
           MOVE "3. Search for User" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY
           MOVE "4. View My Pending Connection Requests" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY
           MOVE "5. Learn a New Skill" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY
           MOVE "6. Logout" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY
           DISPLAY "Please select an option (1-6): " WITH NO ADVANCING
           ACCEPT WS-MENU-CHOICE

           EVALUATE WS-MENU-CHOICE
              WHEN '1' PERFORM CREATE-EDIT-PROFILE
              WHEN '2' PERFORM DISPLAY-PROFILE
              WHEN '3' PERFORM FIND-SOMEONE-OPTION
              WHEN '4' PERFORM VIEW-PENDING-REQUESTS
              WHEN '5' PERFORM LEARN-SKILL-OPTION
              WHEN '6' MOVE "Logging out..." TO WS-DISPLAY-MESSAGE
                       PERFORM WRITE-OUTPUT-AND-DISPLAY
                       MOVE SPACES TO PF-USERNAME
              WHEN OTHER MOVE "Invalid option." TO WS-DISPLAY-MESSAGE
                         PERFORM WRITE-OUTPUT-AND-DISPLAY
           END-EVALUATE.

       CREATE-EDIT-PROFILE.
           MOVE "=== CREATE / EDIT PROFILE ===" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY
           DISPLAY "Enter first name: " WITH NO ADVANCING
           ACCEPT PF-FIRST-NAME
           DISPLAY "Enter last name: " WITH NO ADVANCING
           ACCEPT PF-LAST-NAME
           DISPLAY "Enter university: " WITH NO ADVANCING
           ACCEPT PF-UNIVERSITY
           DISPLAY "Enter major: " WITH NO ADVANCING
           ACCEPT PF-MAJOR
           DISPLAY "Enter graduation year: " WITH NO ADVANCING
           ACCEPT PF-GRAD-YEAR
           DISPLAY "Enter short about-me (one line): " WITH NO ADVANCING
           ACCEPT PF-ABOUT-ME
           MOVE "Profile saved." TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY.

       DISPLAY-PROFILE.
           MOVE "=== PROFILE ===" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY
           STRING "Username: " DELIMITED BY SIZE PF-USERNAME DELIMITED BY SIZE
                  INTO WS-OUTPUT-LINE
           END-STRING
           MOVE WS-OUTPUT-LINE TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY
           MOVE "Name: " TO WS-DISPLAY-MESSAGE
           STRING PF-FIRST-NAME DELIMITED BY SIZE " " DELIMITED BY SIZE PF-LAST-NAME DELIMITED BY SIZE
                  INTO WS-OUTPUT-LINE
           END-STRING
           MOVE WS-OUTPUT-LINE TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY
           MOVE "University: " TO WS-DISPLAY-MESSAGE
           STRING PF-UNIVERSITY DELIMITED BY SIZE INTO WS-OUTPUT-LINE END-STRING
           MOVE WS-OUTPUT-LINE TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY
           MOVE "Major: " TO WS-DISPLAY-MESSAGE
           STRING PF-MAJOR DELIMITED BY SIZE INTO WS-OUTPUT-LINE END-STRING
           MOVE WS-OUTPUT-LINE TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY
           MOVE "Graduation Year: " TO WS-DISPLAY-MESSAGE
           STRING PF-GRAD-YEAR DELIMITED BY SIZE INTO WS-OUTPUT-LINE END-STRING
           MOVE WS-OUTPUT-LINE TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY.

       LEARN-SKILL-OPTION.
           MOVE "=== LEARN A SKILL ===" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY
           MOVE "Feature not implemented yet." TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY.

       FIND-SOMEONE-OPTION.
           DISPLAY "Enter full name to search (e.g., John Doe): " WITH NO ADVANCING
           ACCEPT WS-SEARCH-NAME

           UNSTRING WS-SEARCH-NAME DELIMITED BY SPACE
               INTO WS-SEARCH-FIRST
                    WS-SEARCH-LAST
           END-UNSTRING

           MOVE 'N' TO WS-NAME-FOUND
           MOVE 'N' TO EOF-PROFILE

           OPEN INPUT PROFILE-FILE
           PERFORM UNTIL EOF-PROFILE = 'Y'
               READ PROFILE-FILE INTO PROFILE-REC
                   AT END
                       MOVE 'Y' TO EOF-PROFILE
                   NOT AT END
                       PERFORM PARSE-PROFILE-REC

                       IF PF-FIRST-NAME = WS-SEARCH-FIRST
                          AND PF-LAST-NAME = WS-SEARCH-LAST
                          MOVE 'Y' TO WS-NAME-FOUND
                          MOVE "User found!" TO WS-DISPLAY-MESSAGE
                          PERFORM WRITE-OUTPUT-AND-DISPLAY
                          PERFORM DISPLAY-PROFILE
                          PERFORM PROMPT-SEND-CONNECTION
                          MOVE 'Y' TO EOF-PROFILE
                       END-IF
               END-READ
           END-PERFORM
           CLOSE PROFILE-FILE

           IF WS-NAME-FOUND = 'N'
               MOVE "No one by that name could be found." TO WS-DISPLAY-MESSAGE
               PERFORM WRITE-OUTPUT-AND-DISPLAY
           END-IF.

       PROMPT-SEND-CONNECTION.
           MOVE "1. Send Connection Request" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY
           MOVE "2. Back to Main Menu" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY
           DISPLAY "Enter your choice: " WITH NO ADVANCING
           ACCEPT WS-MENU-CHOICE
           EVALUATE WS-MENU-CHOICE
              WHEN '1'
                   MOVE PF-USERNAME TO WS-RECIP-USERNAME
                   MOVE WS-REC-USERNAME TO WS-SENDER-USERNAME
                   PERFORM SEND-CONNECTION-REQUEST
              WHEN '2'
                   MOVE "Returning to main menu..." TO WS-DISPLAY-MESSAGE
                   PERFORM WRITE-OUTPUT-AND-DISPLAY
              WHEN OTHER
                   MOVE "Invalid choice." TO WS-DISPLAY-MESSAGE
                   PERFORM WRITE-OUTPUT-AND-DISPLAY
           END-EVALUATE.

       SEND-CONNECTION-REQUEST.
           IF WS-SENDER-USERNAME = WS-RECIP-USERNAME
              MOVE "You cannot send a connection request to yourself." TO WS-DISPLAY-MESSAGE
              PERFORM WRITE-OUTPUT-AND-DISPLAY
              EXIT PARAGRAPH
           END-IF

           PERFORM CHECK-EXISTING-CONNECTIONS

           IF WS-DISPLAY-MESSAGE NOT = SPACE
               PERFORM WRITE-OUTPUT-AND-DISPLAY
               EXIT PARAGRAPH
           END-IF

           IF WS-REQUEST-EXISTS = 'Y'
               PERFORM WRITE-OUTPUT-AND-DISPLAY
               EXIT PARAGRAPH
           END-IF

           ADD 1 TO WS-REQUESTS-COUNT
           MOVE WS-SENDER-USERNAME TO WS-REQ-SENDER(WS-REQUESTS-COUNT)
           MOVE WS-RECIP-USERNAME TO WS-REQ-RECIP(WS-REQUESTS-COUNT)

           MOVE "Connection request sent to " TO WS-DISPLAY-MESSAGE
           STRING WS-DISPLAY-MESSAGE DELIMITED BY SIZE 
                  WS-RECIP-USERNAME DELIMITED BY SIZE 
                  '.' DELIMITED BY SIZE
                  INTO WS-DISPLAY-MESSAGE
           END-STRING

           PERFORM WRITE-OUTPUT-AND-DISPLAY.

       CHECK-EXISTING-CONNECTIONS.
           MOVE SPACE TO WS-DISPLAY-MESSAGE
           MOVE 'N' TO WS-REQUEST-EXISTS

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-REQUESTS-COUNT
               IF (WS-REQ-SENDER(WS-I) = WS-RECIP-USERNAME AND WS-REQ-RECIP(WS-I) = WS-SENDER-USERNAME)
                  MOVE "This user has already sent you a connection request." TO WS-DISPLAY-MESSAGE
                  MOVE 'Y' TO WS-REQUEST-EXISTS
                  EXIT PERFORM
               ELSE
                  IF (WS-REQ-SENDER(WS-I) = WS-SENDER-USERNAME AND WS-REQ-RECIP(WS-I) = WS-RECIP-USERNAME)
                      MOVE "You have already sent a connection request to this user." TO WS-DISPLAY-MESSAGE
                      MOVE 'Y' TO WS-REQUEST-EXISTS
                      EXIT PERFORM
                  END-IF
               END-IF
           END-PERFORM.

       PARSE-PROFILE-REC.
           *> Minimal parse: assume PROFILE-REC holds fields in fixed positions
           MOVE PROFILE-REC(1:20) TO PF-USERNAME
           MOVE PROFILE-REC(21:30) TO PF-FIRST-NAME
           MOVE PROFILE-REC(51:30) TO PF-LAST-NAME
           MOVE PROFILE-REC(81:50) TO PF-UNIVERSITY
           MOVE PROFILE-REC(131:40) TO PF-MAJOR
           MOVE PROFILE-REC(171:4) TO PF-GRAD-YEAR
           MOVE PROFILE-REC(175:200) TO PF-ABOUT-ME.

       WRITE-OUTPUT-AND-DISPLAY.
           DISPLAY WS-DISPLAY-MESSAGE(1:FUNCTION LENGTH(WS-DISPLAY-MESSAGE))
           MOVE WS-DISPLAY-MESSAGE TO OUT-REC
           WRITE OUT-REC.

       SAVE-REQUESTS.
           OPEN OUTPUT REQUESTS-FILE
           IF WS-REQ-STATUS = "00"
              PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-REQUESTS-COUNT
                  STRING WS-REQ-SENDER(WS-I) DELIMITED BY SIZE "," DELIMITED BY SIZE
                         WS-REQ-RECIP(WS-I) DELIMITED BY SIZE INTO REQ-REC
                  END-STRING
                  WRITE REQ-REC
              END-PERFORM
              CLOSE REQUESTS-FILE
           END-IF.

       VIEW-PENDING-REQUESTS.
           MOVE "=== PENDING CONNECTION REQUESTS ===" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY
           
           MOVE 0 TO WS-I
           MOVE 'N' TO WS-NAME-FOUND
           
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > WS-REQUESTS-COUNT
               IF WS-REQ-RECIP(WS-J) = PF-USERNAME
                   ADD 1 TO WS-I
                   MOVE 'Y' TO WS-NAME-FOUND
                   MOVE SPACES TO WS-DISPLAY-MESSAGE
                   STRING "Request #" DELIMITED BY SIZE
                          WS-I DELIMITED BY SIZE
                          ": " DELIMITED BY SIZE
                          WS-REQ-SENDER(WS-J) DELIMITED BY SIZE
                          " wants to connect with you" DELIMITED BY SIZE
                          INTO WS-DISPLAY-MESSAGE
                   END-STRING
                   PERFORM WRITE-OUTPUT-AND-DISPLAY
               END-IF
           END-PERFORM
           
           IF WS-NAME-FOUND = 'N'
               MOVE "You have no pending connection requests." TO WS-DISPLAY-MESSAGE
               PERFORM WRITE-OUTPUT-AND-DISPLAY
           END-IF
           
           MOVE "--------------------" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY.

       END PROGRAM STUDENT-SYSTEM.
