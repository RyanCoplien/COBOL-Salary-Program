       IDENTIFICATION DIVISION.
       PROGRAM-ID. Program5.
	   AUTHOR. Ryan Coplien
	   DATE-WRITTEN. 5/10/2019
       ENVIRONMENT DIVISION.
	      INPUT-OUTPUT SECTION.
	          FILE-CONTROL.
			  SELECT INFILE ASSIGN to 
       'C:\Users\rjcop\Desktop\emp5.dat'
			  ORGANIZATION IS LINE SEQUENTIAL.
			  SELECT OUTFILE ASSIGN TO 
       'C:\Users\rjcop\Desktop\Salary.dat'
			   ORGANIZATION IS LINE SEQUENTIAL
			   ACCESS MODE IS SEQUENTIAL.
               SELECT SORTFILE ASSIGN TO 
        'C:\Users\rjcop\Desktop\SalarySort.dat'.
   

       CONFIGURATION SECTION.

       DATA DIVISION.
		   FILE SECTION.
		   FD INFILE.
		   01 INFILE-FILE.
			   05 EmpNumIn            PIC 9(5).
			   05 EmpNameIn           PIC X(20).
			   05 TerrNumIn           PIC 9(2).
		       05 BLANKSPACE          PIC XX VALUE " ".
			   05 SalaryIn            PIC 9(6).
			   05 BLANKSPACE2         PIC X(11) VALUE " ".
			   05 JobCodeIn           PIC 9(2).
			   05 JunkIn              PIC X(32).
		   FD OUTFILE.
		   01 OUTFILE-FILE.
		       05 JobClassOut           PIC 9(2).
			   05 JobClassNameOut       PIC X(20).
			   05 SalAvgOut             PIC 9(6)V99.
			   05 LowSalOut             PIC 9(6).
			   05 HighSalOut            PIC 9(6).

           SD SORTFILE.
           01 SORT-FILE.
               05 JobClassSort           PIC 9(2).
			   05 JobClassNameSort       PIC X(20).
			   05 SalAvgSort             PIC 9(6)V99.
			   05 LowSalSort             PIC 9(6).
			   05 HighSalSort            PIC 9(6).

       WORKING-STORAGE SECTION.
		   01 WS-ITEM.
		       05 WS-EMPNO            PIC 9(5).
			   05 WS-EMPNAME          PIC X(20).
			   05 WS-TERRITORYNO      PIC 9(2).
			   05 WS-BLANK            PIC XX VALUE " ".
			   05 WS-ANSAL            PIC 9(6).
			   05 WS-BLANK2           PIC X(11) VALUE " ".
			   05 WS-JOBCODE          PIC 9(2).
			   05 WS-JUNK             PIC X(32) VALUE SPACE.
			   05 WS-BLANK3           PIC X.
			   05 WS-BONUS            PIC 9(6).
           
		   01 WS-RUNNING.
			   05 WS-RUNSAL           PIC 9(7).
			   05 WS-RUNBONUS         PIC 9(6).
			   05 WS-RUNEMP           PIC 9(4) VALUE 0.
		   01 WS-EOFILE.
			   05 WS-EOF              PIC X.

           01 WS-CONSTANTS. 
			   05 WS-PM               PIC 9v9 VALUE 0.1.
           01 JobClassTable.
               02 JobClassDetails.
                   03 JobNumber       PIC 99 OCCURS 9 TIMES INDEXED BY I.
                   03 JobName         PIC X(20) OCCURS 9 TIMES INDEXED BY J.

       PROCEDURE DIVISION.
	   100-MAIN.
           PERFORM FillTable
		   OPEN INPUT INFILE.
		   OPEN OUTPUT OUTFILE.
			   PERFORM UNTIL WS-EOF='Y'
				   READ INFILE INTO WS-ITEM
					   AT END MOVE 'Y' TO WS-EOF
					   NOT AT END DISPLAY WS-ITEM
						   PERFORM CheckJobClass 
						   MOVE SPACES TO WS-JUNK
					   MOVE WS-ITEM TO OUTFILE-FILE
					   ADD 1 to WS-RUNEMP
					   ADD WS-ANSAL TO WS-RUNSAL
					   ADD WS-BONUS TO WS-RUNBONUS
						WRITE OUTFILE-FILE
				   END-READ
			   END-PERFORM
           sort SORTFILE on ascending JobClassSort
           using INFILE giving OUTFILE
		   CLOSE INFILE.
		   CLOSE OUTFILE.
	   STOP RUN.
           goback.

       CheckJobClass.
           SET I to 1.
           SEARCH JobNumber
               AT END DISPLAY 'BAD JOB CLASS'
               WHEN JobNumber(I) = JobCodeIn
               SET J TO I
               MOVE JobName(J) TO JobClassNameOut
           END-SEARCH
       STOP run.

       FillTable.
           MOVE 010203040506070809 TO JobNumber.
           SET J to 1
           MOVE "Manager" TO JobName(J)
           SET J UP BY 1
           MOVE "Supervisor" TO JobName(J)
           SET J UP BY 1
           MOVE "Head Cashier" TO JobName(J)
           SET J UP BY 1
           MOVE "Cashier" TO JobName(J)
           SET J UP BY 1
           MOVE "Clerk - 1" TO JobName(J)
           SET J UP BY 1
           MOVE "Maintenance" TO JobName(J)
           SET J UP BY 1
           MOVE "Clerk - 2" TO JobName(J)
           SET J UP BY 1
           MOVE "Clerk - 3" TO JobName(J)
           SET J UP BY 1
           MOVE "Accounting Clerk" TO JobName(J)

       END PROGRAM Program5.
