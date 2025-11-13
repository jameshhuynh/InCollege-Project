---
# InCollege – Professional Networking Platform
---

## Table of Contents

* [Project Overview](#project-overview)
* [System Requirements](#system-requirements)
* [Installation & Setup](#installation--setup)
* [Running the Application](#running-the-application)
* [Application Features](#application-features)
* [File Structure](#file-structure)
* [Input File Format](#input-file-format)
* [Output File Format](#output-file-format)
* [Testing](#testing)
* [Troubleshooting](#troubleshooting)
* [Team Information](#team-information)
* [Contact & Support](#contact--support)
* [License](#license)
* [Appendix](#appendix-sample-complete-test-run)

---

## Project Overview

**InCollege** is a console-based professional networking and job search platform built in **COBOL**.

The application allows users to:

* Create accounts with secure password validation
* Build and manage professional profiles
* Search for and connect with other users
* Post and browse job/internship opportunities
* Apply for jobs and track applications
* Learn new skills through training modules

This project follows an **Agile development methodology** with weekly sprint deliverables.

---

## System Requirements

| Requirement          | Minimum                                       |
| -------------------- | --------------------------------------------- |
| **COBOL Compiler**   | GnuCOBOL 2.2 or higher                        |
| **Operating System** | Linux, macOS, or Windows (with COBOL support) |
| **Memory**           | 512 MB RAM                                    |
| **Disk Space**       | 50 MB free space                              |

---

## Installation & Setup

### 1. Clone the Repository

```bash
git clone https://github.com/jameshhuynh/InCollege-Project.git
```

### 2. Compile the Program

```bash
cobc -x -o ./bin/InCollege ./src/InCollege.cob
```

---

## Running the Application

### Interactive Mode

```bash
cd bin
./InCollege
```

### Testing Mode (with Input File)

```bash
./InCollege < ../test/input/test-case-1.txt
```

Output is shown on-screen **and** written to:

```
InCollege-Output.txt
```

---

## Application Features

### 1. Account Management

* Create and log in to accounts
* **Password Requirements**:

  * 8–12 characters
  * ≥1 uppercase, ≥1 digit, ≥1 special character (!@#$^&*)
* Maximum of **5 accounts**

### 2. Profile Management

* Add/edit:

  * Name, university, major, graduation year
  * “About Me” (≤200 chars)
  * Up to 3 work experiences and 3 education entries
* View full profile anytime

### 3. Networking

* Search users by name
* Send, accept, or reject connection requests
* View your full network

### 4. Job Board

* Post jobs/internships with title, description, employer, location, and optional salary
* Browse all postings
* Apply for jobs
* Track all your applications

### 5. Skills Learning (Modules Under Construction)

* Python Programming
* Data Analysis with Excel
* Digital Marketing
* Graphic Design
* Public Speaking

---

## File Structure

| File                     | Description                 |
| ------------------------ | --------------------------- |
| **InCollege.cob**        | Main COBOL source file      |
| **users.dat**            | User credentials            |
| **profiles.dat**         | Profile information         |
| **connections.dat**      | Network connections         |
| **jobs.dat**             | Job postings                |
| **applications.dat**     | Job applications            |
| **temp_*.dat**           | Temporary files for updates |
| **InCollege-Output.txt** | Output log file             |

---

## Input File Format

Input files simulate keyboard entries, **one input per line**.

### Example

```
2
testuser
TestPass1!
1
John
Doe
University of Florida
Computer Science
2024
Passionate about AI
DONE
8
2
1
1
0
3
6
3
```

**Tips**

1. One command per line
2. Follow prompt order exactly
3. Use valid data
4. Include comments with `#`

---

## Output File Format

`InCollege-Output.txt` contains the **exact screen output**, including:

* Menus
* Prompts
* Messages
* Reports

### Example Output

```
==============================
WELCOME TO INCOLLEGE
==============================
1. Create New Account
2. Login to Existing Account
3. Exit
=== USER LOGIN ===
You have successfully logged in!
```

---

## Testing

### Run Test

```bash
cd bin
./InCollege < ../test/input/test-browse-jobs.txt > ../test/output/test-browse-jobs-output.txt
```

### Verify Output

```bash
diff InCollege-Output.txt ../test/output/test-browse-jobs-output.txt
```

**Check that:**

* Output matches exactly
* All menus and prompts appear correctly
* Data persists across runs

---

## Troubleshooting

| Issue                                      | Possible Cause              | Solution                                                    |
| ------------------------------------------ | --------------------------- | ----------------------------------------------------------- |
| “File not found”                           | Wrong directory             | Run from `bin` or use absolute paths                        |
| “All permitted accounts have been created” | User limit reached          | Delete `users.dat` or raise `WS-MAX-USERS`                  |
| Missing connection requests                | Typo or missing profile     | Ensure both users have profiles and correct spelling        |
| Jobs not appearing                         | No postings or file missing | Confirm job creation and `jobs.dat` exists                  |
| Empty output file                          | Output not written          | Check that `OUTPUT-FILE` is opened/written/closed correctly |

---

## Team Information

* See `Roles.txt` for sprint roles
* **Framework:** Agile/Scrum
* **Sprint Duration:** 1 week
* **Tools:** GitHub, Jira, VS Code/vim

---

## Contact & Support

1. Review this README
2. Check sample input/output files
3. Contact project maintainers if issues persist

---

## License

This project was developed for **CEN 4020 – Software Engineering**
at the **University of South Florida**, taught by **Dr. James Anderson**.

---

## Appendix: Sample Complete Test Run

See:

* `test/input/complete-workflow.txt`
* `test/output/complete-workflow-expected.txt`

### Quick Start Example

```bash
# Compile
cobc -x -o ./bin/InCollege ./src/InCollege.cob

# Run
cd bin
./InCollege < ../test/input/sample-run.txt

# View Output
cat InCollege-Output.txt
```

**First Run:** Creates empty data files and allows new accounts.
**Later Runs:** Loads and persists data across sessions.

---

**Last Updated:** Week 09 Sprint
**Version:** 1.9.0

---
