IDENTIFICATION DIVISION.
       PROGRAM-ID. LOGIN-SYSTEM.
       AUTHOR. STUDENT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-CREDENTIALS.
          05 WS-USERNAME        PIC X(20).
          05 WS-PASSWORD        PIC X(20).

       01 WS-VALID-CREDENTIALS.
          05 WS-VALID-USERNAME  PIC X(20) VALUE 'admin'.
          05 WS-VALID-PASSWORD  PIC X(20) VALUE 'password123'.

       01 WS-LOGIN-STATUS       PIC X VALUE 'N'.
          88 LOGIN-SUCCESSFUL   VALUE 'Y'.
          88 LOGIN-FAILED       VALUE 'N'.

       01 WS-ATTEMPT-COUNTER    PIC 9(3) VALUE 0.
       01 WS-CONTINUE-FLAG      PIC X VALUE 'Y'.
          88 CONTINUE-ATTEMPTS  VALUE 'Y'.
          88 EXIT-PROGRAM       VALUE 'N'.

       01 WS-USER-CHOICE        PIC X.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           DISPLAY "========================================".
           DISPLAY "        STUDENT LOGIN SYSTEM".
           DISPLAY "========================================".
           DISPLAY " ".

           PERFORM LOGIN-PROCESS UNTIL LOGIN-SUCCESSFUL OR EXIT-PROGRAM.

           IF LOGIN-SUCCESSFUL
              DISPLAY " "
              DISPLAY "Login successful! Welcome to the system."
              DISPLAY "You are now logged in."
           ELSE
              DISPLAY " "
              DISPLAY "Goodbye! Exiting the system."
           END-IF.

           STOP RUN.

       LOGIN-PROCESS.
           ADD 1 TO WS-ATTEMPT-COUNTER.

           DISPLAY " ".
           DISPLAY "Login Attempt #" WS-ATTEMPT-COUNTER.
           DISPLAY "--------------------------------".

           PERFORM GET-CREDENTIALS.
           PERFORM VALIDATE-LOGIN.

           IF LOGIN-FAILED
              PERFORM HANDLE-FAILED-LOGIN
           END-IF.

       GET-CREDENTIALS.
           DISPLAY "Enter Username: " WITH NO ADVANCING.
           ACCEPT WS-USERNAME.

           DISPLAY "Enter Password: " WITH NO ADVANCING.
           ACCEPT WS-PASSWORD.

       VALIDATE-LOGIN.
           IF WS-USERNAME = WS-VALID-USERNAME AND
              WS-PASSWORD = WS-VALID-PASSWORD
              SET LOGIN-SUCCESSFUL TO TRUE
           ELSE
              SET LOGIN-FAILED TO TRUE
           END-IF.

       HANDLE-FAILED-LOGIN.
           DISPLAY " ".
           DISPLAY "*** LOGIN FAILED ***".
           DISPLAY "Invalid username or password.".
           DISPLAY " ".
           DISPLAY "Total failed attempts: " WS-ATTEMPT-COUNTER.
           DISPLAY " ".
           DISPLAY "Would you like to try again? (Y/N): "
                   WITH NO ADVANCING.
           ACCEPT WS-USER-CHOICE.

           IF WS-USER-CHOICE = 'Y' OR WS-USER-CHOICE = 'y'
              SET CONTINUE-ATTEMPTS TO TRUE
           ELSE
              SET EXIT-PROGRAM TO TRUE
           END-IF.

       MAIN-MENU-PROCESS.
           DISPLAY " ".
           DISPLAY "========================================".
           DISPLAY "           MAIN MENU".
           DISPLAY "========================================".
           DISPLAY "1. Search for a Job/Internship".
           DISPLAY "2. Find Someone You Know".
           DISPLAY "3. Learn a New Skill".
           DISPLAY "4. Exit System".
           DISPLAY " ".
           DISPLAY "Please select an option (1-4): " WITH NO ADVANCING.
           ACCEPT WS-MENU-CHOICE.

           EVALUATE WS-MENU-CHOICE
              WHEN '1'
                 PERFORM JOB-SEARCH-OPTION
              WHEN '2'
                 PERFORM FIND-SOMEONE-OPTION
              WHEN '3'
                 PERFORM LEARN-SKILL-OPTION
              WHEN '4'
                 PERFORM EXIT-SYSTEM-OPTION
              WHEN OTHER
                 DISPLAY " "
                 DISPLAY "Invalid option. Please select 1-4."
           END-EVALUATE.

       JOB-SEARCH-OPTION.
           DISPLAY " ".
           DISPLAY "========================================".
           DISPLAY "        JOB SEARCH / INTERNSHIP".
           DISPLAY "========================================".
           DISPLAY " ".
           DISPLAY "*** UNDER CONSTRUCTION ***".
           DISPLAY "This feature is currently being developed.".
           DISPLAY "Please check back later!".
           DISPLAY " ".
           DISPLAY "Press Enter to return to main menu...".
           ACCEPT WS-USER-CHOICE.

       FIND-SOMEONE-OPTION.
           DISPLAY " ".
           DISPLAY "========================================".
           DISPLAY "        FIND SOMEONE YOU KNOW".
           DISPLAY "========================================".
           DISPLAY " ".
           DISPLAY "*** UNDER CONSTRUCTION ***".
           DISPLAY "This feature is currently being developed.".
           DISPLAY "Please check back later!".
           DISPLAY " ".
           DISPLAY "Press Enter to return to main menu...".
           ACCEPT WS-USER-CHOICE.

       LEARN-SKILL-OPTION.
           DISPLAY " ".
           DISPLAY "========================================".
           DISPLAY "          LEARN A NEW SKILL".
           DISPLAY "========================================".
           DISPLAY "Select a skill to learn:".
           DISPLAY " ".
           DISPLAY "1. Python Programming".
           DISPLAY "2. Data Analysis with Excel".
           DISPLAY "3. Digital Marketing".
           DISPLAY "4. Graphic Design (Photoshop)".
           DISPLAY "5. Public Speaking".
           DISPLAY "6. Return to Main Menu".
           DISPLAY " ".
           DISPLAY "Please select an option (1-6): " WITH NO ADVANCING.
           ACCEPT WS-SKILL-CHOICE.

           EVALUATE WS-SKILL-CHOICE
              WHEN '1'
                 PERFORM SKILL-UNDER-CONSTRUCTION
                    WITH "Python Programming"
              WHEN '2'
                 PERFORM SKILL-UNDER-CONSTRUCTION
                    WITH "Data Analysis with Excel"
              WHEN '3'
                 PERFORM SKILL-UNDER-CONSTRUCTION
                    WITH "Digital Marketing"
              WHEN '4'
                 PERFORM SKILL-UNDER-CONSTRUCTION
                    WITH "Graphic Design (Photoshop)"
              WHEN '5'
                 PERFORM SKILL-UNDER-CONSTRUCTION
                    WITH "Public Speaking"
              WHEN '6'
                 DISPLAY " "
                 DISPLAY "Returning to Main Menu..."
              WHEN OTHER
                 DISPLAY " "
                 DISPLAY "Invalid option. Please select 1-6."
                 DISPLAY "Press Enter to try again..."
                 ACCEPT WS-USER-CHOICE
                 PERFORM LEARN-SKILL-OPTION
           END-EVALUATE.

       SKILL-UNDER-CONSTRUCTION USING SKILL-NAME.
           DISPLAY " ".
           DISPLAY "========================================".
           DISPLAY "           " SKILL-NAME.
           DISPLAY "========================================".
           DISPLAY " ".
           DISPLAY "*** UNDER CONSTRUCTION ***".
           DISPLAY "The " SKILL-NAME " course is currently".
           DISPLAY "being developed. Please check back later!".
           DISPLAY " ".
           DISPLAY "Press Enter to return to skills menu...".
           ACCEPT WS-USER-CHOICE.
           PERFORM LEARN-SKILL-OPTION.

       EXIT-SYSTEM-OPTION.
           DISPLAY " ".
           DISPLAY "Are you sure you want to exit? (Y/N): "
                   WITH NO ADVANCING.
           ACCEPT WS-USER-CHOICE.

           IF WS-USER-CHOICE = 'Y' OR WS-USER-CHOICE = 'y'
              SET EXIT-MAIN-SYSTEM TO TRUE
              DISPLAY " "
              DISPLAY "Logging out... Goodbye!"
           ELSE
              DISPLAY " "
              DISPLAY "Returning to Main Menu..."
           END-IF.

           #./bin/Unsuccessfulogin 