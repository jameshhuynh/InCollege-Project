       >>SOURCE FORMAT FREE
       *> IDAHO-5: Combined Account System + Login Menu + Profile Management
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

       DATA DIVISION.
       FILE SECTION.
       FD  USER-FILE.
       01  USER-REC       PIC X(80).

       FD  PROFILE-FILE.
       01  PROFILE-REC    PIC X(900).

       FD  OUTPUT-FILE.
       01  OUT-REC        PIC X(100).

       WORKING-STORAGE SECTION.
       01  WS-USER-CHOICE     PIC X.
       01  WS-FILE-STATUS     PIC XX VALUE SPACES.
       01  WS-PROF-STATUS     PIC XX VALUE SPACES.
       01  WS-OUT-STATUS      PIC XX VALUE SPACES.
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
          05 PF-GRAD-YEAR         PIC 9(4).
          05 PF-ABOUT-ME          PIC X(200).
          05 PF-EXP-COUNT         PIC 9 VALUE 0.
          05 PF-EXP OCCURS 3 TIMES.
             10 PF-EXP-TITLE       PIC X(30).
             10 PF-EXP-COMPANY     PIC X(30).
             10 PF-EXP-DATES       PIC X(20).
             10 PF-EXP-DESC        PIC X(100).
          05 PF-EDU-COUNT         PIC 9 VALUE 0.
          05 PF-EDU OCCURS 3 TIMES.
             10 PF-EDU-DEGREE      PIC X(30).
             10 PF-EDU-UNIV        PIC X(50).
             10 PF-EDU-YEARS       PIC X(20).

       01  WS-PROF-KEYLINE        PIC X(80).
       01  WS-TEMP-NUMERIC        PIC 9(4).
       01  WS-DISPLAY-MESSAGE     PIC X(100).
       01  WS-REC-USERNAME PIC X(20).

       01 WS-SEARCH-NAME        PIC X(40).
       01 WS-SEARCH-FIRST       PIC X(20).
       01 WS-SEARCH-LAST        PIC X(20).
       01 WS-NAME-FOUND         PIC X VALUE "N".
       01 EOF-PROFILE       PIC X VALUE 'N'.




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
           MOVE "4. Learn a New Skill" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY
           MOVE "5. Logout" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY
           DISPLAY "Please select an option (1-5): " WITH NO ADVANCING
           ACCEPT WS-MENU-CHOICE

           EVALUATE WS-MENU-CHOICE
              WHEN '1' PERFORM CREATE-EDIT-PROFILE
              WHEN '2' PERFORM DISPLAY-PROFILE
              WHEN '3' PERFORM FIND-SOMEONE-OPTION
              WHEN '4' PERFORM LEARN-SKILL-OPTION
              WHEN '5' MOVE "Logging out..." TO WS-DISPLAY-MESSAGE
                       PERFORM WRITE-OUTPUT-AND-DISPLAY
                       MOVE SPACES TO PF-USERNAME
              WHEN OTHER MOVE "Invalid option." TO WS-DISPLAY-MESSAGE
                         PERFORM WRITE-OUTPUT-AND-DISPLAY
           END-EVALUATE.

       CREATE-EDIT-PROFILE.
           MOVE "--- Create/Edit Profile ---" TO WS-DISPLAY-MESSAGE
           PERFORM WRITE-OUTPUT-AND-DISPLAY

           DISPLAY "Enter First Name: " WITH NO ADVANCING
           ACCEPT PF-FIRST-NAME
           IF PF-FIRST-NAME = SPACES
              MOVE "First name is required. Keeping previous value if any." TO WS-DISPLAY-MESSAGE
              PERFORM WRITE-OUTPUT-AND-DISPLAY
           END-IF

           DISPLAY "Enter Last Name: " WITH NO ADVANCING
           ACCEPT PF-LAST-NAME
           IF PF-LAST-NAME = SPACES
              MOVE "Last name is required. Keeping previous value if any." TO WS-DISPLAY-MESSAGE
              PERFORM WRITE-OUTPUT-AND-DISPLAY
           END-IF

           DISPLAY "Enter University/College Attended: " WITH NO ADVANCING
           ACCEPT PF-UNIVERSITY
           IF PF-UNIVERSITY = SPACES
              MOVE "University is required. Keeping previous value if any." TO WS-DISPLAY-MESSAGE
              PERFORM WRITE-OUTPUT-AND-DISPLAY
           END-IF

           DISPLAY "Enter Major: " WITH NO ADVANCING
           ACCEPT PF-MAJOR
           IF PF-MAJOR = SPACES
              MOVE "Major is required. Keeping previous value if any." TO WS-DISPLAY-MESSAGE
              PERFORM WRITE-OUTPUT-AND-DISPLAY
           END-IF

           DISPLAY "Enter Graduation Year (YYYY): " WITH NO ADVANCING
           ACCEPT WS-INPUT-LINE
           IF FUNCTION NUMVAL(WS-INPUT-LINE(1:4)) > 1900 AND FUNCTION NUMVAL(WS-INPUT-LINE(1:4)) < 2100
              MOVE FUNCTION NUMVAL(WS-INPUT-LINE(1:4)) TO PF-GRAD-YEAR
           ELSE
              MOVE "Invalid graduation year. Keeping previous value if any." TO WS-DISPLAY-MESSAGE
              PERFORM WRITE-OUTPUT-AND-DISPLAY
           END-IF

           DISPLAY "Enter About Me (optional, max 200 chars, enter blank line to skip): " WITH NO ADVANCING
           ACCEPT PF-ABOUT-ME

           MOVE 0 TO PF-EXP-COUNT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 3
              DISPLAY "Add Experience (optional, max 3 entries. Enter 'DONE' to finish): " WITH NO ADVANCING
              ACCEPT WS-INPUT-LINE
              IF WS-INPUT-LINE(1:4) = "DONE"
                 EXIT PERFORM
              ELSE
                 ADD 1 TO PF-EXP-COUNT
                 MOVE WS-INPUT-LINE TO PF-EXP-TITLE(WS-I)
                 DISPLAY "Experience #" WS-I " - Company/Organization: " WITH NO ADVANCING
                 ACCEPT PF-EXP-COMPANY(WS-I)
                 DISPLAY "Experience #" WS-I " - Dates (e.g., Summer 2024): " WITH NO ADVANCING
                 ACCEPT PF-EXP-DATES(WS-I)
                 DISPLAY "Experience #" WS-I " - Description (optional, blank to skip): " WITH NO ADVANCING
                 ACCEPT PF-EXP-DESC(WS-I)
              END-IF
           END-PERFORM

           MOVE 0 TO PF-EDU-COUNT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 3
              DISPLAY "Add Education (optional, max 3 entries. Enter Degree here or enter 'DONE' to finish): " WITH NO ADVANCING
              ACCEPT WS-INPUT-LINE
              IF WS-INPUT-LINE(1:4) = "DONE"
                 EXIT PERFORM
              ELSE
                 ADD 1 TO PF-EDU-COUNT
                 MOVE WS-INPUT-LINE TO PF-EDU-DEGREE(WS-I)
                 DISPLAY "Education #" WS-I " - University/College: " WITH NO ADVANCING
                 ACCEPT PF-EDU-UNIV(WS-I)
                 DISPLAY "Education #" WS-I " - Years Attended (e.g., 2023-2025): " WITH NO ADVANCING
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
               STRING PF-USERNAME DELIMITED BY SIZE ,
                   PF-FIRST-NAME DELIMITED BY SIZE ,
                   PF-LAST-NAME DELIMITED BY SIZE ,
                   PF-UNIVERSITY DELIMITED BY SIZE ,
                   PF-MAJOR DELIMITED BY SIZE ,
                   PF-GRAD-YEAR DELIMITED BY SIZE ,
                   PF-ABOUT-ME DELIMITED BY SIZE ,
                   PF-EXP-COUNT DELIMITED BY SIZE ,
                   PF-EXP-TITLE (1) DELIMITED BY SIZE ,
                   PF-EXP-COMPANY (1) DELIMITED BY SIZE ,
                   PF-EXP-DATES (1) DELIMITED BY SIZE ,
                   PF-EXP-DESC (1) DELIMITED BY SIZE ,
                   PF-EXP-TITLE (2) DELIMITED BY SIZE ,
                   PF-EXP-COMPANY (2) DELIMITED BY SIZE ,
                   PF-EXP-DATES (2) DELIMITED BY SIZE ,
                   PF-EXP-DESC (2) DELIMITED BY SIZE ,
                   PF-EXP-TITLE (3) DELIMITED BY SIZE ,
                   PF-EXP-COMPANY (3) DELIMITED BY SIZE ,
                   PF-EXP-DATES (3) DELIMITED BY SIZE ,
                   PF-EXP-DESC (3) DELIMITED BY SIZE ,
                   PF-EDU-COUNT DELIMITED BY SIZE ,
                   PF-EDU-DEGREE (1) DELIMITED BY SIZE ,
                   PF-EDU-UNIV (1) DELIMITED BY SIZE ,
                   PF-EDU-YEARS (1) DELIMITED BY SIZE ,
                   PF-EDU-DEGREE (2) DELIMITED BY SIZE ,
                   PF-EDU-UNIV (2) DELIMITED BY SIZE ,
                   PF-EDU-YEARS (2) DELIMITED BY SIZE ,
                   PF-EDU-DEGREE (3) DELIMITED BY SIZE ,
                   PF-EDU-UNIV (3) DELIMITED BY SIZE ,
                   PF-EDU-YEARS (3) DELIMITED BY SIZE
                   INTO PROFILE-REC
               END-STRING
               WRITE PROFILE-REC
               CLOSE PROFILE-FILE
           END-IF.

       DELETE-EXISTING-PROFILE.
           OPEN INPUT PROFILE-FILE
           IF WS-PROF-STATUS = "00"
               OPEN OUTPUT PROFILE-FILE
               PERFORM READ-PROFILE-RECORD
               PERFORM UNTIL WS-PROF-STATUS NOT = "00"
                  UNSTRING PROFILE-REC DELIMITED BY ","
                      INTO WS-REC-USERNAME
                  END-UNSTRING
                  IF WS-REC-USERNAME NOT = PF-USERNAME
                      WRITE PROFILE-REC
                  END-IF
                  PERFORM READ-PROFILE-RECORD
               END-PERFORM
               CLOSE PROFILE-FILE
           END-IF.

       LOAD-PROFILE-FOR-USER.
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

                   PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > PF-EXP-COUNT
                       MOVE SPACES TO WS-DISPLAY-MESSAGE
                       STRING "Title: " DELIMITED BY SIZE
                              FUNCTION TRIM(PF-EXP-TITLE(WS-I)) DELIMITED BY SIZE
                              INTO WS-DISPLAY-MESSAGE
                       END-STRING
                       PERFORM WRITE-OUTPUT-AND-DISPLAY

                       MOVE SPACES TO WS-DISPLAY-MESSAGE
                       STRING "Company: " DELIMITED BY SIZE
                              FUNCTION TRIM(PF-EXP-COMPANY(WS-I)) DELIMITED BY SIZE
                              INTO WS-DISPLAY-MESSAGE
                       END-STRING
                       PERFORM WRITE-OUTPUT-AND-DISPLAY

                       MOVE SPACES TO WS-DISPLAY-MESSAGE
                       STRING "Dates: " DELIMITED BY SIZE
                              FUNCTION TRIM(PF-EXP-DATES(WS-I)) DELIMITED BY SIZE
                              INTO WS-DISPLAY-MESSAGE
                       END-STRING
                       PERFORM WRITE-OUTPUT-AND-DISPLAY

                       IF PF-EXP-DESC(WS-I) NOT = SPACES
                           MOVE SPACES TO WS-DISPLAY-MESSAGE
                           STRING "Description: " DELIMITED BY SIZE
                                  FUNCTION TRIM(PF-EXP-DESC(WS-I)) DELIMITED BY SIZE
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

                   PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > PF-EDU-COUNT
                       MOVE SPACES TO WS-DISPLAY-MESSAGE
                       STRING "Degree: " DELIMITED BY SIZE
                              FUNCTION TRIM(PF-EDU-DEGREE(WS-I)) DELIMITED BY SIZE
                              INTO WS-DISPLAY-MESSAGE
                       END-STRING
                       PERFORM WRITE-OUTPUT-AND-DISPLAY

                       MOVE SPACES TO WS-DISPLAY-MESSAGE
                       STRING "University: " DELIMITED BY SIZE
                              FUNCTION TRIM(PF-EDU-UNIV(WS-I)) DELIMITED BY SIZE
                              INTO WS-DISPLAY-MESSAGE
                       END-STRING
                       PERFORM WRITE-OUTPUT-AND-DISPLAY

                       MOVE SPACES TO WS-DISPLAY-MESSAGE
                       STRING "Years: " DELIMITED BY SIZE
                              FUNCTION TRIM(PF-EDU-YEARS(WS-I)) DELIMITED BY SIZE
                              INTO WS-DISPLAY-MESSAGE
                       END-STRING
                       PERFORM WRITE-OUTPUT-AND-DISPLAY
                   END-PERFORM
               END-IF
           END-IF

           MOVE "--------------------" TO WS-DISPLAY-MESSAGE
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
                          MOVE 'Y' TO EOF-PROFILE
                       END-IF
           END-READ
           END-PERFORM
           CLOSE PROFILE-FILE

           IF WS-NAME-FOUND = 'N'
               MOVE "No one by that name could be found." TO WS-DISPLAY-MESSAGE
               PERFORM WRITE-OUTPUT-AND-DISPLAY
           END-IF.


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
                   MOVE "This skill is under construction." TO WS-DISPLAY-MESSAGE
                   PERFORM WRITE-OUTPUT-AND-DISPLAY
           WHEN '2'
                   MOVE "This skill is under construction." TO WS-DISPLAY-MESSAGE
                   PERFORM WRITE-OUTPUT-AND-DISPLAY
           WHEN '3'
                   MOVE "This skill is under construction." TO WS-DISPLAY-MESSAGE
                   PERFORM WRITE-OUTPUT-AND-DISPLAY
           WHEN '4'
                   MOVE "This skill is under construction." TO WS-DISPLAY-MESSAGE
                   PERFORM WRITE-OUTPUT-AND-DISPLAY
           WHEN '5'
                   MOVE "This skill is under construction." TO WS-DISPLAY-MESSAGE
                   PERFORM WRITE-OUTPUT-AND-DISPLAY
           WHEN '6'
                   MOVE "Returning to Main Menu..." TO WS-DISPLAY-MESSAGE
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
               MOVE "Password must be 8-12 characters" TO WS-DISPLAY-MESSAGE
               PERFORM WRITE-OUTPUT-AND-DISPLAY
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
               MOVE "Password needs uppercase" TO WS-DISPLAY-MESSAGE
               PERFORM WRITE-OUTPUT-AND-DISPLAY
           END-IF
           IF WS-HAS-DIGIT = 'N'
               MOVE "Password needs digit" TO WS-DISPLAY-MESSAGE
               PERFORM WRITE-OUTPUT-AND-DISPLAY
           END-IF
           IF WS-HAS-SPECIAL = 'N'
               MOVE "Password needs special (!,@,#,$,...)" TO WS-DISPLAY-MESSAGE
               PERFORM WRITE-OUTPUT-AND-DISPLAY
           END-IF.
           
       WRITE-OUTPUT-AND-DISPLAY.
           DISPLAY WS-DISPLAY-MESSAGE(1:FUNCTION LENGTH(FUNCTION TRIM(WS-DISPLAY-MESSAGE)))
           MOVE WS-DISPLAY-MESSAGE TO OUT-REC
           WRITE OUT-REC.

       END PROGRAM STUDENT-SYSTEM.
