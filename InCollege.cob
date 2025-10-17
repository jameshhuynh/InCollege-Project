       >>SOURCE FORMAT FREE
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

           SELECT CONNECTION-FILE ASSIGN TO "connections.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-CONN-STATUS.

           SELECT TEMP-CONNECTION-FILE ASSIGN TO "temp_connections.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-TEMP-CONN-STATUS.

           SELECT TEMP-PROFILE-FILE ASSIGN TO "temp_profiles.dat"
                 ORGANIZATION IS LINE SEQUENTIAL
                 FILE STATUS IS WS-TEMP-PROF-STATUS.

           SELECT JOB-FILE ASSIGN TO "jobs.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-JOB-STATUS.


       DATA DIVISION.
       FILE SECTION.
       FD  USER-FILE.
       01  USER-REC       PIC X(80).

       FD  PROFILE-FILE.
       01  PROFILE-REC    PIC X(900).

       FD  OUTPUT-FILE.
       01  OUT-REC        PIC X(100).

       FD  CONNECTION-FILE.
       01  CONN-REC       PIC X(60).

       FD TEMP-CONNECTION-FILE.
       01 TEMP-CONN-REC      PIC X(60).

       FD TEMP-PROFILE-FILE.
       01 TEMP-PROFILE-REC    PIC X(900).

       FD JOB-FILE.
       01 JOB-REC         PIC X(500).




       WORKING-STORAGE SECTION.
       01  WS-USER-CHOICE     PIC X.
       01 WS-REQUEST-INDEX       PIC 99 VALUE 0.
       01 WS-REQUEST-SENDER      PIC X(30) VALUE SPACES.
       01 EOF-CONNECTION         PIC X VALUE 'N'.
       01 EOF-CONNECTION-FILE    PIC X VALUE 'N'.

       01  WS-FILE-STATUS     PIC XX VALUE SPACES.
       01  WS-PROF-STATUS     PIC XX VALUE SPACES.
       01  WS-OUT-STATUS      PIC XX VALUE SPACES.
       01  WS-CONN-STATUS     PIC XX VALUE SPACES.
       01  WS-JOB-STATUS      PIC XX VALUE SPACES.
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
       01  WS-DISPLAY-MESSAGE     PIC X(100).
       01  WS-REC-USERNAME        PIC X(20).

       01  WS-SEARCH-NAME         PIC X(40).
       01  WS-SEARCH-FIRST        PIC X(20).
       01  WS-SEARCH-LAST         PIC X(20).
       01  WS-NAME-FOUND          PIC X VALUE "N".
       01  EOF-PROFILE            PIC X VALUE 'N'.
       01  WS-FOUND-USERNAME      PIC X(20).

       01  WS-SEARCH-PROFILE.
          05 SF-FIRST-NAME         PIC X(30).
          05 SF-LAST-NAME          PIC X(30).
          05 SF-UNIVERSITY         PIC X(50).
          05 SF-MAJOR              PIC X(40).
          05 SF-GRAD-YEAR          PIC 9(4).
          05 SF-ABOUT-ME           PIC X(200).
          05 SF-EXP-COUNT          PIC 9 VALUE 0.
          05 SF-EXP OCCURS 3 TIMES.
             10 SF-EXP-TITLE       PIC X(30).
             10 SF-EXP-COMPANY     PIC X(30).
             10 SF-EXP-DATES       PIC X(20).
             10 SF-EXP-DESC        PIC X(100).
          05 SF-EDU-COUNT          PIC 9 VALUE 0.
          05 SF-EDU OCCURS 3 TIMES.
             10 SF-EDU-DEGREE      PIC X(30).
             10 SF-EDU-UNIV        PIC X(50).
             10 SF-EDU-YEARS       PIC X(20).

       01  WS-CONN-COUNT          PIC 99 VALUE 0.
       01  WS-CONN-STATUS-FLAG    PIC X(10) VALUE SPACES.
       01  WS-REC-SENDER          PIC X(20).
       01  WS-REC-RECIPIENT       PIC X(20).

       01 WS-CONN-ALREADY-EXISTS       PIC X VALUE 'N'.
       01 WS-CONN-RECEIVED-FROM-USER   PIC X VALUE 'N'.

       01  WS-TEMP-FIRST        PIC X(30).
       01  WS-TEMP-LAST         PIC X(30).
       01 WS-OUTPUT-LINE-TEMP PIC X(50).
       01 WS-CONN-USER1    PIC X(30) VALUE SPACES.
       01 WS-CONN-USER2    PIC X(30) VALUE SPACES.
       01 WS-TEMP-CONN-STATUS PIC XX VALUE SPACES.
       01 WS-TEMP-PROF-STATUS PIC XX VALUE SPACES.

       01  WS-JOB-POSTING.
          05 JP-POSTER         PIC X(20).
          05 JP-TITLE          PIC X(50).
          05 JP-DESCRIPTION    PIC X(200).
          05 JP-EMPLOYER       PIC X(50).
          05 JP-LOCATION       PIC X(50).
          05 JP-SALARY         PIC X(30).




       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM ENSURE-FILES
           PERFORM LOAD-USERS
           OPEN OUTPUT OUTPUT-FILE
           PERFORM MAIN-MENU UNTIL WS-CONTINUE = 'N'
           CLOSE OUTPUT-FILE
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

           OPEN INPUT CONNECTION-FILE
           IF WS-CONN-STATUS = "35"
              OPEN OUTPUT CONNECTION-FILE
              CLOSE CONNECTION-FILE
           ELSE
              CLOSE CONNECTION-FILE
           END-IF

           OPEN INPUT JOB-FILE
           IF WS-JOB-STATUS = "35"
              OPEN OUTPUT JOB-FILE
              CLOSE JOB-FILE
           ELSE
              CLOSE JOB-FILE
           END-IF.

       LOAD-USERS.
           MOVE 0 TO WS-USER-COUNT
           OPEN INPUT USER-FILE
           IF WS-FILE-STATUS = "00"
              PERFORM READ-USER-RECORD
              PERFORM UNTIL WS-FILE-STATUS NOT = "00"
                     OR WS-USER-COUNT >= WS-MAX-USERS

                  PERFORM PARSE-USER-RECORD
                  PERFORM READ-USER-RECORD
              END-PERFORM
              CLOSE USER-FILE
           END-IF.

       READ-USER-RECORD.
           READ USER-FILE INTO USER-REC.

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

       MAIN-MENU.
           MOVE "==============="-
           "===============" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY
           MOVE "WELCOME TO INCOLLEGE" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY
           MOVE "==============="-
           "===============" TO WS-DISPLAY-MESSAGE
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
                   MOVE "Invalid choice. Please enter 1-3."
                         TO WS-DISPLAY-MESSAGE

                   PERFORM WRITE-OUTPUT-AND-DISPLAY
           END-EVALUATE.

       CREATE-ACCOUNT.
           MOVE "=== CREATE NEW ACCOUNT ===" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY

           IF WS-USER-COUNT >= WS-MAX-USERS
               STRING
                 "All permitted accounts have been created, please "
                 "come back later."
                   DELIMITED BY SIZE
                   INTO WS-DISPLAY-MESSAGE
               END-STRING
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
                      MOVE WS-PASSWORD(1:12) TO
                          WS-USER-PASS(WS-USER-COUNT)

                      PERFORM SAVE-USER-TO-FILE
                      MOVE "Account created successfully!"
                      TO WS-DISPLAY-MESSAGE
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

       LOGIN-USER.
           MOVE "=== USER LOGIN ===" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY

           IF WS-USER-COUNT = 0
              MOVE "No accounts exist. Please create one first."
                 TO WS-DISPLAY-MESSAGE
              PERFORM WRITE-OUTPUT-AND-DISPLAY
              EXIT PARAGRAPH
           END-IF

           PERFORM GET-LOGIN-CREDENTIALS
           PERFORM VALIDATE-LOGIN

           IF WS-LOGIN-SUCCESS = 'Y'
              MOVE "You have successfully logged in!"
                 TO WS-DISPLAY-MESSAGE
              PERFORM WRITE-OUTPUT-AND-DISPLAY
              MOVE WS-LOGIN-USERNAME TO PF-USERNAME
              PERFORM LOAD-PROFILE-FOR-USER
              PERFORM USER-DASHBOARD UNTIL WS-MENU-CHOICE = '6'
           ELSE
              MOVE "Incorrect username/password, please try again."
                 TO WS-DISPLAY-MESSAGE
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
                       EXIT PERFORM
                   END-IF
               END-IF
           END-PERFORM.

       USER-DASHBOARD.
           DISPLAY " "
           MOVE "======================================"
           TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY
           MOVE "MAIN MENU" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY
           MOVE "======================================"
           TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY
           MOVE "1. Create/Edit My Profile" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY
           MOVE "2. View My Profile" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY
           MOVE "3. Search for User" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY
           MOVE "4. Learn a New Skill" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY
           MOVE "5. View My Pending Connection Requests"
           TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY
           MOVE "6. Logout" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY
           MOVE "7. View My Network" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY
           MOVE "8. Job Search/Internship" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY
           DISPLAY "Please select an option (1-8): " WITH NO ADVANCING
           ACCEPT WS-MENU-CHOICE

           EVALUATE WS-MENU-CHOICE
              WHEN '1' PERFORM CREATE-EDIT-PROFILE
              WHEN '2' PERFORM DISPLAY-PROFILE
              WHEN '3' PERFORM FIND-SOMEONE-OPTION
              WHEN '4' PERFORM LEARN-SKILL-OPTION
              WHEN '5' PERFORM VIEW-PENDING-CONNECTIONS
              WHEN '6' MOVE "Logging out..." TO WS-DISPLAY-MESSAGE
                       PERFORM WRITE-OUTPUT-AND-DISPLAY
                       MOVE SPACES TO PF-USERNAME
                       PERFORM CLEAR-PROFILE-DATA
              WHEN '7' PERFORM VIEW-MY-NETWORK
              WHEN '8' PERFORM JOB-SEARCH-MENU
              WHEN OTHER MOVE "Invalid option." TO WS-DISPLAY-MESSAGE
                         PERFORM WRITE-OUTPUT-AND-DISPLAY
           END-EVALUATE.


       JOB-SEARCH-MENU.
           MOVE "--- Job Search/Internship Menu ---"
           TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY
           MOVE "1. Post a Job/Internship" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY
           MOVE "2. Browse Jobs/Internships" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY
           MOVE "3. Back to Main Menu" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY
           DISPLAY "Enter your choice: " WITH NO ADVANCING
           ACCEPT WS-MENU-CHOICE

           EVALUATE WS-MENU-CHOICE
               WHEN '1'
                   PERFORM POST-JOB
               WHEN '2'
                   MOVE "Browse Jobs/Internships is under construction."
                   TO WS-DISPLAY-MESSAGE
                   PERFORM WRITE-OUTPUT-AND-DISPLAY
               WHEN '3'
                   MOVE "Returning to Main Menu..." TO WS-DISPLAY-MESSAGE
                   PERFORM WRITE-OUTPUT-AND-DISPLAY
               WHEN OTHER
                   MOVE "Invalid choice." TO WS-DISPLAY-MESSAGE
                   PERFORM WRITE-OUTPUT-AND-DISPLAY
           END-EVALUATE.


       POST-JOB.
           MOVE "--- Post a New Job/Internship ---"
           TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY

           MOVE PF-USERNAME TO JP-POSTER

           DISPLAY "Enter Job Title: " WITH NO ADVANCING
           ACCEPT JP-TITLE
           IF JP-TITLE = SPACES
              MOVE "Job title is required." TO WS-DISPLAY-MESSAGE
              PERFORM WRITE-OUTPUT-AND-DISPLAY
              EXIT PARAGRAPH
           END-IF

           DISPLAY "Enter Description (max 200 chars): "
           WITH NO ADVANCING
           ACCEPT JP-DESCRIPTION
           IF JP-DESCRIPTION = SPACES
              MOVE "Description is required." TO WS-DISPLAY-MESSAGE
              PERFORM WRITE-OUTPUT-AND-DISPLAY
              EXIT PARAGRAPH
           END-IF

           DISPLAY "Enter Employer Name: " WITH NO ADVANCING
           ACCEPT JP-EMPLOYER
           IF JP-EMPLOYER = SPACES
              MOVE "Employer name is required." TO WS-DISPLAY-MESSAGE
              PERFORM WRITE-OUTPUT-AND-DISPLAY
              EXIT PARAGRAPH
           END-IF

           DISPLAY "Enter Location: " WITH NO ADVANCING
           ACCEPT JP-LOCATION
           IF JP-LOCATION = SPACES
              MOVE "Location is required." TO WS-DISPLAY-MESSAGE
              PERFORM WRITE-OUTPUT-AND-DISPLAY
              EXIT PARAGRAPH
           END-IF

           DISPLAY "Enter Salary (optional, enter 'NONE' to skip): "
           WITH NO ADVANCING
           ACCEPT JP-SALARY
           IF JP-SALARY = SPACES OR JP-SALARY = "NONE"
              MOVE "N/A" TO JP-SALARY
           END-IF

           PERFORM SAVE-JOB-TO-FILE
           MOVE "Job posted successfully!" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY.


       SAVE-JOB-TO-FILE.
           OPEN EXTEND JOB-FILE
           IF WS-JOB-STATUS = "00"
              STRING JP-POSTER DELIMITED BY SIZE
                     "," DELIMITED BY SIZE
                     JP-TITLE DELIMITED BY SIZE
                     "," DELIMITED BY SIZE
                     JP-DESCRIPTION DELIMITED BY SIZE
                     "," DELIMITED BY SIZE
                     JP-EMPLOYER DELIMITED BY SIZE
                     "," DELIMITED BY SIZE
                     JP-LOCATION DELIMITED BY SIZE
                     "," DELIMITED BY SIZE
                     JP-SALARY DELIMITED BY SIZE
                     INTO JOB-REC
              END-STRING
              WRITE JOB-REC
              CLOSE JOB-FILE
           END-IF.




       CREATE-EDIT-PROFILE.
           MOVE "--- Create/Edit Profile ---" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY

           DISPLAY "Enter First Name: " WITH NO ADVANCING
           ACCEPT PF-FIRST-NAME
           IF PF-FIRST-NAME = SPACES
              STRING "First name is required. Keeping previous value"
                     " if any."
                 DELIMITED BY SIZE
                 INTO WS-DISPLAY-MESSAGE
              END-STRING
              PERFORM WRITE-OUTPUT-AND-DISPLAY
           END-IF

           DISPLAY "Enter Last Name: " WITH NO ADVANCING
           ACCEPT PF-LAST-NAME
           IF PF-LAST-NAME = SPACES
              STRING "Last name is required. Keeping previous value"
                     " if any."
                 DELIMITED BY SIZE
                 INTO WS-DISPLAY-MESSAGE
              END-STRING
              PERFORM WRITE-OUTPUT-AND-DISPLAY
           END-IF

           DISPLAY "Enter University/College Attended: "
           WITH NO ADVANCING
           ACCEPT PF-UNIVERSITY
           IF PF-UNIVERSITY = SPACES
              STRING "University is required. Keeping previous value"
                     " if any."
                 DELIMITED BY SIZE
                 INTO WS-DISPLAY-MESSAGE
              END-STRING
              PERFORM WRITE-OUTPUT-AND-DISPLAY
           END-IF

           DISPLAY "Enter Major: " WITH NO ADVANCING
           ACCEPT PF-MAJOR
           IF PF-MAJOR = SPACES
              STRING "Major is required. Keeping previous value"
                     " if any."
                 DELIMITED BY SIZE
                 INTO WS-DISPLAY-MESSAGE
              END-STRING
              PERFORM WRITE-OUTPUT-AND-DISPLAY
           END-IF

           DISPLAY "Enter Graduation Year (YYYY): " WITH NO ADVANCING
           ACCEPT WS-INPUT-LINE
           IF FUNCTION NUMVAL(WS-INPUT-LINE(1:4)) > 1900
               AND FUNCTION NUMVAL(WS-INPUT-LINE(1:4)) < 2100
              MOVE FUNCTION NUMVAL(WS-INPUT-LINE(1:4)) TO PF-GRAD-YEAR
           ELSE
              STRING "Invalid graduation year. Keeping previous value"
                     " if any."
                 DELIMITED BY SIZE
                 INTO WS-DISPLAY-MESSAGE
              END-STRING
              PERFORM WRITE-OUTPUT-AND-DISPLAY
           END-IF

           DISPLAY "Enter About Me (optional, max 200 chars, enter"
           DISPLAY "blank line to skip): " WITH NO ADVANCING

           ACCEPT PF-ABOUT-ME

           MOVE 0 TO PF-EXP-COUNT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 3
              DISPLAY "Add Experience (optional, max 3 entries."
              DISPLAY "Enter 'DONE' to finish): " WITH NO ADVANCING
              ACCEPT WS-INPUT-LINE
              IF WS-INPUT-LINE(1:4) = "DONE"
                 EXIT PERFORM
              ELSE
                 ADD 1 TO PF-EXP-COUNT
                 MOVE WS-INPUT-LINE TO PF-EXP-TITLE(WS-I)
                 DISPLAY "Experience #" WS-I " - Company/Organization: "
                 WITH NO ADVANCING
                 ACCEPT PF-EXP-COMPANY(WS-I)
                 DISPLAY "Experience #" WS-I " - Dates (e.g., Summer"
                 DISPLAY "2024): " WITH NO ADVANCING
                 ACCEPT PF-EXP-DATES(WS-I)
                 DISPLAY "Experience #" WS-I " - Description (optional,"
                 DISPLAY "blank to skip): " WITH NO ADVANCING
                 ACCEPT PF-EXP-DESC(WS-I)
              END-IF
           END-PERFORM

           MOVE 0 TO PF-EDU-COUNT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 3
              DISPLAY "Add Education (optional, max 3 entries. Enter"
              DISPLAY "Degree here or enter 'DONE' to finish): "
              WITH NO ADVANCING
              ACCEPT WS-INPUT-LINE
              IF WS-INPUT-LINE(1:4) = "DONE"
                 EXIT PERFORM
              ELSE
                 ADD 1 TO PF-EDU-COUNT
                 MOVE WS-INPUT-LINE TO PF-EDU-DEGREE(WS-I)
                 DISPLAY "Education #" WS-I " - University/College: "
                 WITH NO ADVANCING
                 ACCEPT PF-EDU-UNIV(WS-I)
                 DISPLAY "Education #" WS-I " - Years Attended"
                 DISPLAY "(e.g., 2023-2025): " WITH NO ADVANCING
                 ACCEPT PF-EDU-YEARS(WS-I)
              END-IF
           END-PERFORM

           PERFORM SAVE-PROFILE-TO-FILE
           MOVE "Profile saved successfully!" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY.

       SAVE-PROFILE-TO-FILE.
           PERFORM DELETE-EXISTING-PROFILE
           OPEN EXTEND PROFILE-FILE
           IF WS-PROF-STATUS = "00"
               STRING PF-USERNAME DELIMITED BY SIZE ","
               DELIMITED BY SIZE
                   PF-FIRST-NAME DELIMITED BY SIZE "," DELIMITED BY SIZE
                   PF-LAST-NAME DELIMITED BY SIZE "," DELIMITED BY SIZE
                   PF-UNIVERSITY DELIMITED BY SIZE "," DELIMITED BY SIZE
                   PF-MAJOR DELIMITED BY SIZE "," DELIMITED BY SIZE
                   PF-GRAD-YEAR DELIMITED BY SIZE "," DELIMITED BY SIZE
                   PF-ABOUT-ME DELIMITED BY SIZE "," DELIMITED BY SIZE
                   PF-EXP-COUNT DELIMITED BY SIZE "," DELIMITED BY SIZE
                   PF-EXP-TITLE (1) DELIMITED BY SIZE ","
                   DELIMITED BY SIZE
                   PF-EXP-COMPANY (1) DELIMITED BY SIZE ","
                   DELIMITED BY SIZE
                   PF-EXP-DATES (1) DELIMITED BY SIZE ","
                   DELIMITED BY SIZE
                   PF-EXP-DESC (1) DELIMITED BY SIZE ","
                   DELIMITED BY SIZE
                   PF-EXP-TITLE (2) DELIMITED BY SIZE ","
                   DELIMITED BY SIZE
                   PF-EXP-COMPANY (2) DELIMITED BY SIZE ","
                   DELIMITED BY SIZE
                   PF-EXP-DATES (2) DELIMITED BY SIZE ","
                   DELIMITED BY SIZE
                   PF-EXP-DESC (2) DELIMITED BY SIZE ","
                   DELIMITED BY SIZE
                   PF-EXP-TITLE (3) DELIMITED BY SIZE ","
                   DELIMITED BY SIZE
                   PF-EXP-COMPANY (3) DELIMITED BY SIZE ","
                   DELIMITED BY SIZE
                   PF-EXP-DATES (3) DELIMITED BY SIZE ","
                   DELIMITED BY SIZE
                   PF-EXP-DESC (3) DELIMITED BY SIZE ","
                   DELIMITED BY SIZE
                   PF-EDU-COUNT DELIMITED BY SIZE ","
                   DELIMITED BY SIZE
                   PF-EDU-DEGREE (1) DELIMITED BY SIZE ","
                   DELIMITED BY SIZE
                   PF-EDU-UNIV (1) DELIMITED BY SIZE ","
                   DELIMITED BY SIZE
                   PF-EDU-YEARS (1) DELIMITED BY SIZE ","
                   DELIMITED BY SIZE
                   PF-EDU-DEGREE (2) DELIMITED BY SIZE ","
                   DELIMITED BY SIZE
                   PF-EDU-UNIV (2) DELIMITED BY SIZE ","
                   DELIMITED BY SIZE
                   PF-EDU-YEARS (2) DELIMITED BY SIZE ","
                   DELIMITED BY SIZE
                   PF-EDU-DEGREE (3) DELIMITED BY SIZE ","
                   DELIMITED BY SIZE
                   PF-EDU-UNIV (3) DELIMITED BY SIZE ","
                   DELIMITED BY SIZE
                   PF-EDU-YEARS (3) DELIMITED BY SIZE
                   INTO PROFILE-REC
               END-STRING
               WRITE PROFILE-REC
               CLOSE PROFILE-FILE
           END-IF.

       DELETE-EXISTING-PROFILE.
           OPEN INPUT PROFILE-FILE
           IF WS-PROF-STATUS = "00"
               OPEN OUTPUT TEMP-PROFILE-FILE
               PERFORM READ-PROFILE-RECORD
               PERFORM UNTIL WS-PROF-STATUS NOT = "00"
                  UNSTRING PROFILE-REC DELIMITED BY ","
                      INTO WS-REC-USERNAME
                  END-UNSTRING
                  IF WS-REC-USERNAME NOT = PF-USERNAME
                      MOVE PROFILE-REC TO TEMP-PROFILE-REC
                      WRITE TEMP-PROFILE-REC
                  END-IF
                  PERFORM READ-PROFILE-RECORD
               END-PERFORM
               CLOSE PROFILE-FILE
               CLOSE TEMP-PROFILE-FILE

               OPEN OUTPUT PROFILE-FILE
               OPEN INPUT TEMP-PROFILE-FILE
               MOVE SPACES TO PROFILE-REC
               PERFORM UNTIL WS-PROF-STATUS NOT = "00"
                   READ TEMP-PROFILE-FILE INTO TEMP-PROFILE-REC
                       AT END MOVE "10" TO WS-PROF-STATUS
                       NOT AT END
                           MOVE TEMP-PROFILE-REC TO PROFILE-REC
                           WRITE PROFILE-REC
                   END-READ
               END-PERFORM
               CLOSE PROFILE-FILE
               CLOSE TEMP-PROFILE-FILE
           ELSE
               CLOSE PROFILE-FILE
           END-IF.

       LOAD-PROFILE-FOR-USER.
           PERFORM CLEAR-PROFILE-DATA
           OPEN INPUT PROFILE-FILE
           IF WS-PROF-STATUS = "00"
              PERFORM READ-PROFILE-RECORD
              PERFORM UNTIL WS-PROF-STATUS NOT = "00"
                 *>Get just the username from the record
                 UNSTRING PROFILE-REC DELIMITED BY ","
                     INTO WS-REC-USERNAME
                 END-UNSTRING

                 *>Compare with logged-in username
                 IF WS-REC-USERNAME = PF-USERNAME
                     PERFORM PARSE-PROFILE-REC
                     EXIT PERFORM
                 END-IF

                 PERFORM READ-PROFILE-RECORD
              END-PERFORM
              CLOSE PROFILE-FILE
           END-IF.

       CLEAR-PROFILE-DATA.
           MOVE SPACES TO PF-FIRST-NAME
           MOVE SPACES TO PF-LAST-NAME
           MOVE SPACES TO PF-UNIVERSITY
           MOVE SPACES TO PF-MAJOR
           MOVE 0 TO PF-GRAD-YEAR
           MOVE SPACES TO PF-ABOUT-ME
           MOVE 0 TO PF-EXP-COUNT
           MOVE 0 TO PF-EDU-COUNT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 3
               MOVE SPACES TO PF-EXP-TITLE(WS-I)
               MOVE SPACES TO PF-EXP-COMPANY(WS-I)
               MOVE SPACES TO PF-EXP-DATES(WS-I)
               MOVE SPACES TO PF-EXP-DESC(WS-I)
               MOVE SPACES TO PF-EDU-DEGREE(WS-I)
               MOVE SPACES TO PF-EDU-UNIV(WS-I)
               MOVE SPACES TO PF-EDU-YEARS(WS-I)
           END-PERFORM.
       READ-PROFILE-RECORD.
           READ PROFILE-FILE INTO PROFILE-REC.

       PARSE-PROFILE-REC.
           UNSTRING PROFILE-REC DELIMITED BY ","
              INTO WS-REC-USERNAME
                   PF-FIRST-NAME
                   PF-LAST-NAME
                   PF-UNIVERSITY
                   PF-MAJOR
                   PF-GRAD-YEAR
                   PF-ABOUT-ME
                   PF-EXP-COUNT
                   PF-EXP-TITLE(1)
                   PF-EXP-COMPANY(1)
                   PF-EXP-DATES(1)
                   PF-EXP-DESC(1)
                   PF-EXP-TITLE(2)
                   PF-EXP-COMPANY(2)
                   PF-EXP-DATES(2)
                   PF-EXP-DESC(2)
                   PF-EXP-TITLE(3)
                   PF-EXP-COMPANY(3)
                   PF-EXP-DATES(3)
                   PF-EXP-DESC(3)
                   PF-EDU-COUNT
                   PF-EDU-DEGREE(1)
                   PF-EDU-UNIV(1)
                   PF-EDU-YEARS(1)
                   PF-EDU-DEGREE(2)
                   PF-EDU-UNIV(2)
                   PF-EDU-YEARS(2)
                   PF-EDU-DEGREE(3)
                   PF-EDU-UNIV(3)
                   PF-EDU-YEARS(3)
           END-UNSTRING.

       PARSE-SEARCH-PROFILE-REC.
           UNSTRING PROFILE-REC DELIMITED BY ","
              INTO WS-REC-USERNAME
                   SF-FIRST-NAME
                   SF-LAST-NAME
                   SF-UNIVERSITY
                   SF-MAJOR
                   SF-GRAD-YEAR
                   SF-ABOUT-ME
                   SF-EXP-COUNT
                   SF-EXP-TITLE(1)
                   SF-EXP-COMPANY(1)
                   SF-EXP-DATES(1)
                   SF-EXP-DESC(1)
                   SF-EXP-TITLE(2)
                   SF-EXP-COMPANY(2)
                   SF-EXP-DATES(2)
                   SF-EXP-DESC(2)
                   SF-EXP-TITLE(3)
                   SF-EXP-COMPANY(3)
                   SF-EXP-DATES(3)
                   SF-EXP-DESC(3)
                   SF-EDU-COUNT
                   SF-EDU-DEGREE(1)
                   SF-EDU-UNIV(1)
                   SF-EDU-YEARS(1)
                   SF-EDU-DEGREE(2)
                   SF-EDU-UNIV(2)
                   SF-EDU-YEARS(2)
                   SF-EDU-DEGREE(3)
                   SF-EDU-UNIV(3)
                   SF-EDU-YEARS(3)
           END-UNSTRING.


       DISPLAY-PROFILE.
           MOVE "--- Your Profile ---" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY

           IF PF-FIRST-NAME = SPACES AND PF-LAST-NAME = SPACES
               MOVE SPACES TO WS-DISPLAY-MESSAGE
               STRING "No profile found for user: " DELIMITED BY SIZE
                      PF-USERNAME DELIMITED BY SIZE
                      INTO WS-DISPLAY-MESSAGE
               END-STRING
               PERFORM WRITE-OUTPUT-AND-DISPLAY
           ELSE
               *>--- Name ---
               MOVE SPACES TO WS-DISPLAY-MESSAGE
               STRING "Name: " DELIMITED BY SIZE
                      FUNCTION TRIM(PF-FIRST-NAME) DELIMITED BY SIZE
                      " " DELIMITED BY SIZE
                      FUNCTION TRIM(PF-LAST-NAME) DELIMITED BY SIZE
                      INTO WS-DISPLAY-MESSAGE
               END-STRING
               PERFORM WRITE-OUTPUT-AND-DISPLAY

               *>--- University ---
               MOVE SPACES TO WS-DISPLAY-MESSAGE
               STRING "University: " DELIMITED BY SIZE
                      FUNCTION TRIM(PF-UNIVERSITY) DELIMITED BY SIZE
                      INTO WS-DISPLAY-MESSAGE
               END-STRING
               PERFORM WRITE-OUTPUT-AND-DISPLAY

               *>--- Major ---
               MOVE SPACES TO WS-DISPLAY-MESSAGE
               STRING "Major: " DELIMITED BY SIZE
                      FUNCTION TRIM(PF-MAJOR) DELIMITED BY SIZE
                      INTO WS-DISPLAY-MESSAGE
               END-STRING
               PERFORM WRITE-OUTPUT-AND-DISPLAY

               *>--- Graduation Year ---
               MOVE SPACES TO WS-DISPLAY-MESSAGE
               STRING "Graduation Year: " DELIMITED BY SIZE
                      PF-GRAD-YEAR DELIMITED BY SIZE
                      INTO WS-DISPLAY-MESSAGE
               END-STRING
               PERFORM WRITE-OUTPUT-AND-DISPLAY

               *>--- About Me ---
               IF PF-ABOUT-ME NOT = SPACES
                   MOVE SPACES TO WS-DISPLAY-MESSAGE
                   STRING "About Me: " DELIMITED BY SIZE
                          FUNCTION TRIM(PF-ABOUT-ME) DELIMITED BY SIZE
                          INTO WS-DISPLAY-MESSAGE
                   END-STRING
                   PERFORM WRITE-OUTPUT-AND-DISPLAY
               END-IF

               *>--- Experience ---
               IF PF-EXP-COUNT > 0
                   MOVE SPACES TO WS-DISPLAY-MESSAGE
                   MOVE "Experience:" TO WS-DISPLAY-MESSAGE
                   PERFORM WRITE-OUTPUT-AND-DISPLAY

                   PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I >
                       PF-EXP-COUNT
                       MOVE SPACES TO WS-DISPLAY-MESSAGE
                       STRING "Title: " DELIMITED BY SIZE
                              FUNCTION TRIM(PF-EXP-TITLE(WS-I))
                              DELIMITED BY SIZE
                              INTO WS-DISPLAY-MESSAGE
                       END-STRING
                       PERFORM WRITE-OUTPUT-AND-DISPLAY

                       MOVE SPACES TO WS-DISPLAY-MESSAGE
                       STRING "Company: " DELIMITED BY SIZE
                              FUNCTION TRIM(PF-EXP-COMPANY(WS-I))
                              DELIMITED BY SIZE
                              INTO WS-DISPLAY-MESSAGE
                       END-STRING
                       PERFORM WRITE-OUTPUT-AND-DISPLAY

                       MOVE SPACES TO WS-DISPLAY-MESSAGE
                       STRING "Dates: " DELIMITED BY SIZE
                              FUNCTION TRIM(PF-EXP-DATES(WS-I))
                              DELIMITED BY SIZE
                              INTO WS-DISPLAY-MESSAGE
                       END-STRING
                       PERFORM WRITE-OUTPUT-AND-DISPLAY

                       IF PF-EXP-DESC(WS-I) NOT = SPACES
                           MOVE SPACES TO WS-DISPLAY-MESSAGE
                           STRING "Description: " DELIMITED BY SIZE
                                  FUNCTION TRIM(PF-EXP-DESC(WS-I))
                                  DELIMITED BY SIZE
                                  INTO WS-DISPLAY-MESSAGE
                           END-STRING
                           PERFORM WRITE-OUTPUT-AND-DISPLAY
                       END-IF
                   END-PERFORM
               END-IF

               *>--- Education ---
               IF PF-EDU-COUNT > 0
                   MOVE SPACES TO WS-DISPLAY-MESSAGE
                   MOVE "Education:" TO WS-DISPLAY-MESSAGE
                   PERFORM WRITE-OUTPUT-AND-DISPLAY

                   PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I >
                       PF-EDU-COUNT
                       MOVE SPACES TO WS-DISPLAY-MESSAGE
                       STRING "Degree: " DELIMITED BY SIZE
                              FUNCTION TRIM(PF-EDU-DEGREE(WS-I))
                              DELIMITED BY SIZE
                              INTO WS-DISPLAY-MESSAGE
                       END-STRING
                       PERFORM WRITE-OUTPUT-AND-DISPLAY

                       MOVE SPACES TO WS-DISPLAY-MESSAGE
                       STRING "University: " DELIMITED BY SIZE
                              FUNCTION TRIM(PF-EDU-UNIV(WS-I))
                              DELIMITED BY SIZE
                              INTO WS-DISPLAY-MESSAGE
                       END-STRING
                       PERFORM WRITE-OUTPUT-AND-DISPLAY

                       MOVE SPACES TO WS-DISPLAY-MESSAGE
                       STRING "Years: " DELIMITED BY SIZE
                              FUNCTION TRIM(PF-EDU-YEARS(WS-I))
                              DELIMITED BY SIZE
                              INTO WS-DISPLAY-MESSAGE
                       END-STRING
                       PERFORM WRITE-OUTPUT-AND-DISPLAY
                   END-PERFORM
               END-IF
           END-IF

           MOVE "--------------------" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY.

       DISPLAY-SEARCH-PROFILE.
           MOVE "--- Your Profile ---" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY

           IF SF-FIRST-NAME = SPACES AND SF-LAST-NAME = SPACES
               MOVE SPACES TO WS-DISPLAY-MESSAGE
               STRING "No profile found for user: " DELIMITED BY SIZE
                      WS-REC-USERNAME DELIMITED BY SIZE
                      INTO WS-DISPLAY-MESSAGE
               END-STRING
               PERFORM WRITE-OUTPUT-AND-DISPLAY
           ELSE
               *>--- Name ---
               MOVE SPACES TO WS-DISPLAY-MESSAGE
               STRING "Name: " DELIMITED BY SIZE
                      FUNCTION TRIM(SF-FIRST-NAME) DELIMITED BY SIZE
                      " " DELIMITED BY SIZE
                      FUNCTION TRIM(SF-LAST-NAME) DELIMITED BY SIZE
                      INTO WS-DISPLAY-MESSAGE
               END-STRING
               PERFORM WRITE-OUTPUT-AND-DISPLAY

               *>--- University ---
               MOVE SPACES TO WS-DISPLAY-MESSAGE
               STRING "University: " DELIMITED BY SIZE
                      FUNCTION TRIM(SF-UNIVERSITY) DELIMITED BY SIZE
                      INTO WS-DISPLAY-MESSAGE
               END-STRING
               PERFORM WRITE-OUTPUT-AND-DISPLAY

               *>--- Major ---
               MOVE SPACES TO WS-DISPLAY-MESSAGE
               STRING "Major: " DELIMITED BY SIZE
                      FUNCTION TRIM(SF-MAJOR) DELIMITED BY SIZE
                      INTO WS-DISPLAY-MESSAGE
               END-STRING
               PERFORM WRITE-OUTPUT-AND-DISPLAY

               *>--- Graduation Year ---
               MOVE SPACES TO WS-DISPLAY-MESSAGE
               STRING "Graduation Year: " DELIMITED BY SIZE
                      SF-GRAD-YEAR DELIMITED BY SIZE
                      INTO WS-DISPLAY-MESSAGE
               END-STRING
               PERFORM WRITE-OUTPUT-AND-DISPLAY

               *>--- About Me ---
               IF SF-ABOUT-ME NOT = SPACES
                   MOVE SPACES TO WS-DISPLAY-MESSAGE
                   STRING "About Me: " DELIMITED BY SIZE
                          FUNCTION TRIM(SF-ABOUT-ME) DELIMITED BY SIZE
                          INTO WS-DISPLAY-MESSAGE
                   END-STRING
                   PERFORM WRITE-OUTPUT-AND-DISPLAY
               END-IF

               *>--- Experience ---
               IF SF-EXP-COUNT > 0
                   MOVE SPACES TO WS-DISPLAY-MESSAGE
                   MOVE "Experience:" TO WS-DISPLAY-MESSAGE
                   PERFORM WRITE-OUTPUT-AND-DISPLAY

                   PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I >
                       SF-EXP-COUNT
                       MOVE SPACES TO WS-DISPLAY-MESSAGE
                       STRING "Title: " DELIMITED BY SIZE
                              FUNCTION TRIM(SF-EXP-TITLE(WS-I))
                              DELIMITED BY SIZE
                              INTO WS-DISPLAY-MESSAGE
                       END-STRING
                       PERFORM WRITE-OUTPUT-AND-DISPLAY

                       MOVE SPACES TO WS-DISPLAY-MESSAGE
                       STRING "Company: " DELIMITED BY SIZE
                              FUNCTION TRIM(SF-EXP-COMPANY(WS-I))
                              DELIMITED BY SIZE
                              INTO WS-DISPLAY-MESSAGE
                       END-STRING
                       PERFORM WRITE-OUTPUT-AND-DISPLAY

                       MOVE SPACES TO WS-DISPLAY-MESSAGE
                       STRING "Dates: " DELIMITED BY SIZE
                              FUNCTION TRIM(SF-EXP-DATES(WS-I))
                              DELIMITED BY SIZE
                              INTO WS-DISPLAY-MESSAGE
                       END-STRING
                       PERFORM WRITE-OUTPUT-AND-DISPLAY

                       IF SF-EXP-DESC(WS-I) NOT = SPACES
                           MOVE SPACES TO WS-DISPLAY-MESSAGE
                           STRING "Description: " DELIMITED BY SIZE
                                  FUNCTION TRIM(SF-EXP-DESC(WS-I))
                                  DELIMITED BY SIZE
                                  INTO WS-DISPLAY-MESSAGE
                           END-STRING
                           PERFORM WRITE-OUTPUT-AND-DISPLAY
                       END-IF
                   END-PERFORM
               END-IF

               *>--- Education ---
               IF SF-EDU-COUNT > 0
                   MOVE SPACES TO WS-DISPLAY-MESSAGE
                   MOVE "Education:" TO WS-DISPLAY-MESSAGE
                   PERFORM WRITE-OUTPUT-AND-DISPLAY

                   PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I >
                       SF-EDU-COUNT
                       MOVE SPACES TO WS-DISPLAY-MESSAGE
                       STRING "Degree: " DELIMITED BY SIZE
                              FUNCTION TRIM(SF-EDU-DEGREE(WS-I))
                              DELIMITED BY SIZE
                              INTO WS-DISPLAY-MESSAGE
                       END-STRING
                       PERFORM WRITE-OUTPUT-AND-DISPLAY

                       MOVE SPACES TO WS-DISPLAY-MESSAGE
                       STRING "University: " DELIMITED BY SIZE
                              FUNCTION TRIM(SF-EDU-UNIV(WS-I))
                              DELIMITED BY SIZE
                              INTO WS-DISPLAY-MESSAGE
                       END-STRING
                       PERFORM WRITE-OUTPUT-AND-DISPLAY

                       MOVE SPACES TO WS-DISPLAY-MESSAGE
                       STRING "Years: " DELIMITED BY SIZE
                              FUNCTION TRIM(SF-EDU-YEARS(WS-I))
                              DELIMITED BY SIZE
                              INTO WS-DISPLAY-MESSAGE
                       END-STRING
                       PERFORM WRITE-OUTPUT-AND-DISPLAY
                   END-PERFORM
               END-IF
           END-IF

           MOVE "--------------------" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY.

       FIND-SOMEONE-OPTION.
           DISPLAY "Enter full name to search (e.g., John Doe): "
           WITH NO ADVANCING
           ACCEPT WS-SEARCH-NAME
           MOVE FUNCTION TRIM(WS-SEARCH-NAME) TO WS-SEARCH-NAME
           UNSTRING WS-SEARCH-NAME DELIMITED BY SPACE
               INTO WS-SEARCH-FIRST
                    WS-SEARCH-LAST
           END-UNSTRING

           MOVE 'N' TO WS-NAME-FOUND
           MOVE 'N' TO EOF-PROFILE
           MOVE SPACES TO WS-FOUND-USERNAME

           OPEN INPUT PROFILE-FILE
           PERFORM UNTIL EOF-PROFILE = 'Y'
               READ PROFILE-FILE INTO PROFILE-REC
                   AT END
                       MOVE 'Y' TO EOF-PROFILE
                   NOT AT END
                       PERFORM PARSE-SEARCH-PROFILE-REC

                       IF SF-FIRST-NAME = WS-SEARCH-FIRST
                          AND SF-LAST-NAME = WS-SEARCH-LAST
                          MOVE 'Y' TO WS-NAME-FOUND
                          MOVE WS-REC-USERNAME TO WS-FOUND-USERNAME
                          MOVE "User found!" TO WS-DISPLAY-MESSAGE
                          PERFORM WRITE-OUTPUT-AND-DISPLAY
                          PERFORM DISPLAY-SEARCH-PROFILE
                          PERFORM SEND-CONNECTION-REQUEST-OFFER
                          MOVE 'Y' TO EOF-PROFILE
                       END-IF
           END-READ
           END-PERFORM
           CLOSE PROFILE-FILE

           IF WS-NAME-FOUND = 'N'
               MOVE "No one by that name could be found." TO
               WS-DISPLAY-MESSAGE
               PERFORM WRITE-OUTPUT-AND-DISPLAY
           END-IF.

       SEND-CONNECTION-REQUEST-OFFER.
           DISPLAY "1. Send Connection Request"
           DISPLAY "2. Back to Main Menu"
           DISPLAY "Enter your choice (1-2): " WITH NO ADVANCING
           ACCEPT WS-MENU-CHOICE

           EVALUATE WS-MENU-CHOICE
               WHEN '1'
                   PERFORM SEND-CONNECTION-REQUEST
               WHEN OTHER
                   MOVE "Returning to Main Menu..." TO
                   WS-DISPLAY-MESSAGE
                   PERFORM WRITE-OUTPUT-AND-DISPLAY
           END-EVALUATE.

       SEND-CONNECTION-REQUEST.
           *> Check if sending to self
           IF PF-USERNAME = WS-FOUND-USERNAME
              MOVE "Cannot send connection request to yourself." TO
              WS-DISPLAY-MESSAGE
              PERFORM WRITE-OUTPUT-AND-DISPLAY
              EXIT PARAGRAPH
           END-IF

           *> Validate request constraints
           PERFORM CHECK-EXISTING-CONNECTIONS

           IF WS-CONN-ALREADY-EXISTS = 'Y'
               MOVE "You are already connected with this user." TO
               WS-DISPLAY-MESSAGE
               PERFORM WRITE-OUTPUT-AND-DISPLAY
           ELSE
               IF WS-CONN-RECEIVED-FROM-USER = 'Y'
                   STRING "This user has already sent you a connection"
                     " request."
                    DELIMITED BY SIZE
                    INTO WS-DISPLAY-MESSAGE
                   END-STRING
                   PERFORM WRITE-OUTPUT-AND-DISPLAY
               ELSE
                   PERFORM SAVE-CONNECTION-REQUEST

                   *> Trim and build full name
                   MOVE FUNCTION TRIM(WS-SEARCH-FIRST) TO WS-TEMP-FIRST
                   MOVE FUNCTION TRIM(WS-SEARCH-LAST)  TO WS-TEMP-LAST

                   STRING WS-TEMP-FIRST DELIMITED BY SPACE
                          " "          DELIMITED BY SPACE
                          WS-TEMP-LAST DELIMITED BY SPACE
                          INTO WS-OUTPUT-LINE-TEMP
                   END-STRING

                   *> Build final message
                   STRING "Connection request sent to "
                   DELIMITED BY SIZE
                       WS-OUTPUT-LINE-TEMP            DELIMITED BY SPACE
                          INTO WS-DISPLAY-MESSAGE
                   END-STRING

                   PERFORM WRITE-OUTPUT-AND-DISPLAY
               END-IF
           END-IF.

       CHECK-EXISTING-CONNECTIONS.
           MOVE 'N' TO WS-CONN-ALREADY-EXISTS WS-CONN-RECEIVED-FROM-USER
           MOVE 'N' TO EOF-CONNECTION
           OPEN INPUT CONNECTION-FILE
           PERFORM READ-CONNECTION
           PERFORM UNTIL EOF-CONNECTION = 'Y'
                IF FUNCTION TRIM(WS-REC-SENDER) =
                   FUNCTION TRIM(PF-USERNAME) AND
                   FUNCTION TRIM(WS-REC-RECIPIENT) =
                   FUNCTION TRIM(WS-FOUND-USERNAME)
                    IF FUNCTION TRIM(WS-CONN-STATUS-FLAG) = "CONNECTED"
                        MOVE 'Y' TO WS-CONN-ALREADY-EXISTS
                    END-IF
                    IF FUNCTION TRIM(WS-CONN-STATUS-FLAG) = "PENDING"
                        MOVE 'Y' TO WS-CONN-ALREADY-EXISTS
                    END-IF
                ELSE IF FUNCTION TRIM(WS-REC-SENDER) =
                        FUNCTION TRIM(WS-FOUND-USERNAME) AND
                        FUNCTION TRIM(WS-REC-RECIPIENT) =
                        FUNCTION TRIM(PF-USERNAME)
                    IF FUNCTION TRIM(WS-CONN-STATUS-FLAG) = "CONNECTED"
                        MOVE 'Y' TO WS-CONN-ALREADY-EXISTS
                    END-IF
                    IF FUNCTION TRIM(WS-CONN-STATUS-FLAG) = "PENDING"
                        MOVE 'Y' TO WS-CONN-RECEIVED-FROM-USER
                    END-IF
                END-IF
               PERFORM READ-CONNECTION
           END-PERFORM
           CLOSE CONNECTION-FILE.

       READ-CONNECTION.
           READ CONNECTION-FILE INTO CONN-REC
               AT END
                  MOVE 'Y' TO EOF-CONNECTION
               NOT AT END
                  UNSTRING CONN-REC DELIMITED BY ","
                     INTO WS-REC-SENDER
                          WS-REC-RECIPIENT
                          WS-CONN-STATUS-FLAG
                  END-UNSTRING
           END-READ.

       SAVE-CONNECTION-REQUEST.
           OPEN EXTEND CONNECTION-FILE
           IF WS-CONN-STATUS = "00"
              MOVE SPACES TO CONN-REC
              STRING FUNCTION TRIM(PF-USERNAME) DELIMITED BY SIZE
                     "," DELIMITED BY SIZE
                     FUNCTION TRIM(WS-FOUND-USERNAME) DELIMITED BY SIZE
                     ",PENDING" DELIMITED BY SIZE
                     INTO CONN-REC
              END-STRING
              WRITE CONN-REC
              CLOSE CONNECTION-FILE
           END-IF.


       VIEW-PENDING-CONNECTIONS.
            MOVE SPACES TO WS-DISPLAY-MESSAGE
            MOVE "--- Pending Connection Requests ---" TO
            WS-DISPLAY-MESSAGE
            PERFORM WRITE-OUTPUT-AND-DISPLAY

            MOVE 'N' TO WS-NAME-FOUND
            MOVE 0 TO WS-REQUEST-INDEX
            MOVE 'N' TO EOF-CONNECTION-FILE

            OPEN INPUT CONNECTION-FILE

            PERFORM UNTIL EOF-CONNECTION-FILE = 'Y'
                READ CONNECTION-FILE INTO CONN-REC
                    AT END
                        MOVE 'Y' TO EOF-CONNECTION-FILE
                    NOT AT END
                        UNSTRING CONN-REC DELIMITED BY ","
                            INTO WS-REC-SENDER
                                 WS-REC-RECIPIENT
                                 WS-CONN-STATUS-FLAG
                        END-UNSTRING

                        IF (FUNCTION TRIM(WS-REC-RECIPIENT) =
                            FUNCTION TRIM(PF-USERNAME))
                           AND (FUNCTION TRIM(WS-CONN-STATUS-FLAG) =
                                "PENDING")
                            ADD 1 TO WS-REQUEST-INDEX
                            MOVE 'Y' TO WS-NAME-FOUND

                            MOVE SPACES TO WS-DISPLAY-MESSAGE
                            STRING "Request from: " DELIMITED BY SIZE
                                   FUNCTION TRIM(WS-REC-SENDER)
                                   DELIMITED BY SIZE
                                INTO WS-DISPLAY-MESSAGE
                            END-STRING
                            PERFORM WRITE-OUTPUT-AND-DISPLAY

                            MOVE SPACES TO WS-DISPLAY-MESSAGE
                            MOVE "1. Accept" TO WS-DISPLAY-MESSAGE
                            PERFORM WRITE-OUTPUT-AND-DISPLAY

                            MOVE SPACES TO WS-DISPLAY-MESSAGE
                            MOVE "2. Reject" TO WS-DISPLAY-MESSAGE
                            PERFORM WRITE-OUTPUT-AND-DISPLAY

                            DISPLAY "Enter your choice for "
                                    FUNCTION TRIM(WS-REC-SENDER) ": "
                                    WITH NO ADVANCING
                            ACCEPT WS-USER-CHOICE

                            EVALUATE WS-USER-CHOICE
                                WHEN '1'
                                    MOVE WS-REC-SENDER TO
                                    WS-REQUEST-SENDER
                                    CLOSE CONNECTION-FILE
                                    PERFORM ACCEPT-CONNECTION
                                    MOVE 'Y' TO EOF-CONNECTION-FILE
                                WHEN '2'
                                    MOVE WS-REC-SENDER TO
                                    WS-REQUEST-SENDER
                                    CLOSE CONNECTION-FILE
                                    PERFORM REJECT-CONNECTION
                                    MOVE 'Y' TO EOF-CONNECTION-FILE
                                WHEN OTHER
                                    DISPLAY "Invalid choice, skipping."
                            END-EVALUATE
                        END-IF
                END-READ
            END-PERFORM

            IF EOF-CONNECTION-FILE = 'N'
                CLOSE CONNECTION-FILE
            END-IF

            IF WS-NAME-FOUND = 'N'
                MOVE SPACES TO WS-DISPLAY-MESSAGE
                STRING "You have no pending connection requests at this"
                 DELIMITED BY SIZE
                       " time." DELIMITED BY SIZE
                    INTO WS-DISPLAY-MESSAGE
                END-STRING
                PERFORM WRITE-OUTPUT-AND-DISPLAY
            END-IF

            MOVE SPACES TO WS-DISPLAY-MESSAGE
            MOVE "-------------------------------------" TO
            WS-DISPLAY-MESSAGE
            PERFORM WRITE-OUTPUT-AND-DISPLAY.

       ACCEPT-CONNECTION.
            PERFORM DELETE-PENDING-REQUEST
            MOVE FUNCTION TRIM(PF-USERNAME) TO WS-CONN-USER1
            MOVE FUNCTION TRIM(WS-REQUEST-SENDER) TO WS-CONN-USER2
            PERFORM ADD-CONNECTION

            MOVE FUNCTION TRIM(WS-REQUEST-SENDER) TO WS-CONN-USER1
            MOVE FUNCTION TRIM(PF-USERNAME) TO WS-CONN-USER2
            PERFORM ADD-CONNECTION

            MOVE SPACES TO WS-DISPLAY-MESSAGE
            STRING "✅ Connection request from " DELIMITED BY SIZE
                   FUNCTION TRIM(WS-REQUEST-SENDER) DELIMITED BY SIZE
                   " has been ACCEPTED." DELIMITED BY SIZE
                INTO WS-DISPLAY-MESSAGE
            END-STRING
            PERFORM WRITE-OUTPUT-AND-DISPLAY.

       REJECT-CONNECTION.
           PERFORM DELETE-PENDING-REQUEST
           MOVE SPACES TO WS-DISPLAY-MESSAGE
           STRING "❌ Connection request from " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-REQUEST-SENDER) DELIMITED BY SIZE
                  " has been REJECTED." DELIMITED BY SIZE
               INTO WS-DISPLAY-MESSAGE
           END-STRING
           PERFORM WRITE-OUTPUT-AND-DISPLAY.


       VIEW-MY-NETWORK.
           MOVE SPACES TO WS-DISPLAY-MESSAGE
           MOVE "--- My Network ---" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY

           MOVE 'N' TO WS-NAME-FOUND
           MOVE 'N' TO EOF-CONNECTION-FILE
           MOVE 0 TO WS-CONN-COUNT

           OPEN INPUT CONNECTION-FILE
           PERFORM UNTIL EOF-CONNECTION-FILE = 'Y'
               READ CONNECTION-FILE INTO CONN-REC
                   AT END
                       MOVE 'Y' TO EOF-CONNECTION-FILE
                   NOT AT END
                       ADD 1 TO WS-CONN-COUNT

                       *> Only process every other line (odd-numbered lines)
                       IF FUNCTION MOD(WS-CONN-COUNT, 2) = 1
                           UNSTRING CONN-REC DELIMITED BY ","
                               INTO WS-REC-SENDER
                                    WS-REC-RECIPIENT
                                    WS-CONN-STATUS-FLAG
                           END-UNSTRING

                           *> Only show connections where current user is the SENDER
                           IF FUNCTION TRIM(WS-CONN-STATUS-FLAG) = "CONNECTED"
                              AND FUNCTION TRIM(WS-REC-SENDER) =
                                  FUNCTION TRIM(PF-USERNAME)
                               MOVE FUNCTION TRIM(WS-REC-RECIPIENT)
                                 TO WS-FOUND-USERNAME
                               PERFORM DISPLAY-CONNECTION-DETAIL
                               MOVE 'Y' TO WS-NAME-FOUND
                           END-IF
                       END-IF
               END-READ
           END-PERFORM
           CLOSE CONNECTION-FILE

           IF WS-NAME-FOUND = 'N'
               MOVE SPACES TO WS-DISPLAY-MESSAGE
               MOVE "You have no connections yet." TO WS-DISPLAY-MESSAGE
               PERFORM WRITE-OUTPUT-AND-DISPLAY
           ELSE
               MOVE SPACES TO WS-DISPLAY-MESSAGE
               MOVE "-------------------------------------"
                 TO WS-DISPLAY-MESSAGE
               PERFORM WRITE-OUTPUT-AND-DISPLAY
           END-IF.


       DISPLAY-CONNECTION-DETAIL.
           MOVE 'N' TO EOF-PROFILE
           OPEN INPUT PROFILE-FILE
           PERFORM UNTIL EOF-PROFILE = 'Y'
               READ PROFILE-FILE INTO PROFILE-REC
                   AT END
                       MOVE 'Y' TO EOF-PROFILE
                   NOT AT END
                       UNSTRING PROFILE-REC DELIMITED BY ","
                           INTO WS-REC-USERNAME
                                SF-FIRST-NAME
                                SF-LAST-NAME
                                SF-UNIVERSITY
                                SF-MAJOR
                       END-UNSTRING
                       IF FUNCTION TRIM(WS-REC-USERNAME) =
                          FUNCTION TRIM(WS-FOUND-USERNAME)
                           MOVE SPACES TO WS-DISPLAY-MESSAGE
                           STRING "Name: " FUNCTION TRIM(SF-FIRST-NAME)
                                  " " FUNCTION TRIM(SF-LAST-NAME)
                                  ", " FUNCTION TRIM(SF-UNIVERSITY)
                                  ", " FUNCTION TRIM(SF-MAJOR)
                                  INTO WS-DISPLAY-MESSAGE
                           END-STRING
                           PERFORM WRITE-OUTPUT-AND-DISPLAY
                           *> Stop after one match to prevent duplicates
                           MOVE 'Y' TO EOF-PROFILE
                       END-IF
               END-READ
           END-PERFORM
           CLOSE PROFILE-FILE.


       DELETE-PENDING-REQUEST.
            OPEN INPUT CONNECTION-FILE
            OPEN OUTPUT TEMP-CONNECTION-FILE
            MOVE 'N' TO EOF-CONNECTION
            PERFORM UNTIL EOF-CONNECTION = 'Y'
                READ CONNECTION-FILE INTO CONN-REC
                    AT END MOVE 'Y' TO EOF-CONNECTION
                    NOT AT END
                        UNSTRING CONN-REC DELIMITED BY ',' INTO
                        WS-REC-SENDER WS-REC-RECIPIENT
                        WS-CONN-STATUS-FLAG
                        IF NOT (FUNCTION TRIM(WS-REC-SENDER) =
                                FUNCTION TRIM(WS-REQUEST-SENDER)
                            AND FUNCTION TRIM(WS-REC-RECIPIENT) =
                                FUNCTION TRIM(PF-USERNAME)
                            AND FUNCTION TRIM(WS-CONN-STATUS-FLAG) =
                                "PENDING")
                            MOVE CONN-REC TO TEMP-CONN-REC
                            WRITE TEMP-CONN-REC
                        END-IF
                END-READ
            END-PERFORM
            CLOSE CONNECTION-FILE
            CLOSE TEMP-CONNECTION-FILE

            OPEN OUTPUT CONNECTION-FILE
            OPEN INPUT TEMP-CONNECTION-FILE
            MOVE 'N' TO EOF-CONNECTION
            PERFORM UNTIL EOF-CONNECTION = 'Y'
                READ TEMP-CONNECTION-FILE INTO TEMP-CONN-REC
                    AT END MOVE 'Y' TO EOF-CONNECTION
                    NOT AT END
                        MOVE TEMP-CONN-REC TO CONN-REC
                        WRITE CONN-REC
                END-READ
            END-PERFORM
            CLOSE CONNECTION-FILE
            CLOSE TEMP-CONNECTION-FILE
            .

       ADD-CONNECTION.
            OPEN EXTEND CONNECTION-FILE
            MOVE SPACES TO CONN-REC
            STRING WS-CONN-USER1 DELIMITED BY SPACE
                   "," DELIMITED BY SIZE
                   WS-CONN-USER2 DELIMITED BY SPACE
                   ",CONNECTED" DELIMITED BY SIZE
                   INTO CONN-REC
            END-STRING
            WRITE CONN-REC
            CLOSE CONNECTION-FILE.


       LEARN-SKILL-OPTION.
           MOVE "Select a skill to learn:" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY
           MOVE "1. Python Programming" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY
           MOVE "2. Data Analysis with Excel" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY
           MOVE "3. Digital Marketing" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY
           MOVE "4. Graphic Design" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY
           MOVE "5. Public Speaking" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY
           MOVE "6. Return to Main Menu" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY
       ACCEPT WS-SKILL-CHOICE

       EVALUATE WS-SKILL-CHOICE
           WHEN '1'
                       MOVE "This skill is under construction." TO
                       WS-DISPLAY-MESSAGE
                       PERFORM WRITE-OUTPUT-AND-DISPLAY
           WHEN '2'
                       MOVE "This skill is under construction." TO
                       WS-DISPLAY-MESSAGE
                       PERFORM WRITE-OUTPUT-AND-DISPLAY
           WHEN '3'
                       MOVE "This skill is under construction." TO
                       WS-DISPLAY-MESSAGE
                       PERFORM WRITE-OUTPUT-AND-DISPLAY
           WHEN '4'
                       MOVE "This skill is under construction." TO
                       WS-DISPLAY-MESSAGE
                       PERFORM WRITE-OUTPUT-AND-DISPLAY
           WHEN '5'
                       MOVE "This skill is under construction." TO
                       WS-DISPLAY-MESSAGE
                       PERFORM WRITE-OUTPUT-AND-DISPLAY
           WHEN '6'
                       MOVE "Returning to Main Menu..." TO
                       WS-DISPLAY-MESSAGE
                       PERFORM WRITE-OUTPUT-AND-DISPLAY
           WHEN OTHER
                       MOVE "Invalid choice." TO WS-DISPLAY-MESSAGE
                       PERFORM WRITE-OUTPUT-AND-DISPLAY
       END-EVALUATE.

       GET-NEW-USERNAME.
           DISPLAY "Enter username: " WITH NO ADVANCING
           ACCEPT WS-USERNAME.

       GET-NEW-PASSWORD.
           DISPLAY "Enter password: " WITH NO ADVANCING
           ACCEPT WS-PASSWORD
           PERFORM VALIDATE-PASSWORD.

       VALIDATE-PASSWORD.
           MOVE 'N' TO WS-HAS-UPPER WS-HAS-DIGIT WS-HAS-SPECIAL
           WS-VALID-LENGTH
           MOVE 0  TO WS-PASSWORD-LENGTH

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > LENGTH OF
               WS-PASSWORD
               IF WS-PASSWORD(WS-I:1) NOT = SPACE
                   ADD 1 TO WS-PASSWORD-LENGTH
               ELSE
                   EXIT PERFORM
               END-IF
           END-PERFORM

           IF WS-PASSWORD-LENGTH < 8 OR WS-PASSWORD-LENGTH > 12
               MOVE "Password must be 8-12 characters" TO
               WS-DISPLAY-MESSAGE
               PERFORM WRITE-OUTPUT-AND-DISPLAY
               EXIT PARAGRAPH
           END-IF

           MOVE 'Y' TO WS-VALID-LENGTH

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I >
               WS-PASSWORD-LENGTH
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
               MOVE "Password needs uppercase" TO WS-DISPLAY-MESSAGE
               PERFORM WRITE-OUTPUT-AND-DISPLAY
           END-IF
           IF WS-HAS-DIGIT = 'N'
               MOVE "Password needs digit" TO WS-DISPLAY-MESSAGE
               PERFORM WRITE-OUTPUT-AND-DISPLAY
           END-IF
           IF WS-HAS-SPECIAL = 'N'
               MOVE "Password needs special character" TO
                WS-DISPLAY-MESSAGE
               PERFORM WRITE-OUTPUT-AND-DISPLAY
           END-IF.

       WRITE-OUTPUT-AND-DISPLAY.
           DISPLAY
           WS-DISPLAY-MESSAGE(1:FUNCTION LENGTH(FUNCTION
           TRIM(WS-DISPLAY-MESSAGE)))
           MOVE WS-DISPLAY-MESSAGE TO OUT-REC
           WRITE OUT-REC.

       END PROGRAM STUDENT-SYSTEM.
