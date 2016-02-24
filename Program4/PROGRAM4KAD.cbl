       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PROGRAM4.
       AUTHOR.           KDAYE.

      
      *************************COMMENT SECTION************************
      * DESCRIPTION:
      * THIS PROGRAM GENERATES A STOCK ANALYSIS FOR FIREFLY LTD. THIS
      * PROGRAM LISTS THE VARIATIONS FOR EACH COSTUME GROUPED BY 
      * VENDOR. IF THERE IS NO DATA FOR A COSTUME THE LINE IS NOT
      * PRINTED. IF THERE IS AN ERROR FOR VENDOR ID OR SIZING ERRORS,
      * ERROR TEXT IS DISPLAYED IN THE CORRESPONDING COLOMN FOLLOWED
      * BY THE ERROR CODE THAT TRIGGERED THE ERROR. THIS PROGRAM ALSO
      * CALCULATES THE TOTAL COST FOR EACH COSTUME VARIENT IN STOCK,
      * ALL OF THE COSTUMES FROM A VENDOR, AND ALL OF THE COSTUMES
      * FROM EACH WAREHOUSE. A GRAND TOTAL IS DISPLAYED AT THE BOTTOM
      * OF THE REPORT.
      *
      * MODIFIED DECEMBER 06, 2015: (ADDED TO PROGRAM 3)
      * WHEN RAN THIS PROGRAM PROMPTS THE USER FOR A CHOICE OF A SINGLE
      * WAREHOUSE, OR ALL. BASED ON THIS CHOICE, THE PROGRAM WILL PRINT
      * THE WAREHOUSE INPUT RECORDS AFTER THEY ARE SORTED. IF THE ALL
      * WAREHOUSES ARE CHOSEN, THE PROGRAM SORTS EACH FILE AND THEN
      * MERGES THEM INTO A SINGLE FILE. THIS MERGED FILE IS THE ONE
      * THAT WILL BE USED FOR ALL PROCEDURES AND PROCESSING.
      ****************************************************************
      * INPUT FILES: (ONE FOR EACH WAREHOUSE - LA10, CH20, NY30)
      * 1. WAREHOUSE ID
      * 2. VENDOR ID
      * 3. COSTUME ID
      * 4. ARRAY OF 5 SETS OF COSTUME DATA CONTAINING:
      *     1. COSTUME NAME
      *     2. COSTUME SIZE
      *     3. COSTUME TYPE
      *     4. NUMBER IN STOCK
      *     5. PURCHASE PRICE
      ****************************************************************
      * REPORT OUTPUT:
      * 1. COSTUME NAME
      * 2. COSTUME ID
      * 3. COSTUME SIZE (LARGE, MEDIUM, SMALL, PLUS)-BAD ID IF OTHER
      * 4. COSTUME TYPE (ADULT, CHILD)-BAD ID IF NEITHER
      * 5. NUMBER IN STOCK
      * 6. TOTAL COST
      ****************************************************************
      * CALCULATIONS:
      *
      * TOTAL C0ST = IN STOCK * PURCHASE PRICE
      * COSTUME, VENDOR, AND GRAND COSTS ARE CALCULATED BY ADDING
      *   TOTAL COST TO EACH AFTER EACH RECORD. VENDOR COST IS
      *   RESTARTED AT ZERO ON EACH VENDOR CHANGE, AND WAREHOUSE FOR
      *   EACH WAREHOUSE CHANGE.
      ****************************************************************
	  
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *
       SELECT UNSORTED-FILE1
          ASSIGN TO "PR4F15-CH20.TXT"
          ORGANIZATION IS LINE SEQUENTIAL.
      *
       SELECT UNSORTED-FILE2
          ASSIGN TO "PR4F15-LA10.TXT"
          ORGANIZATION IS LINE SEQUENTIAL.
      *
       SELECT UNSORTED-FILE3
          ASSIGN TO "PR4F15-NY30.TXT"
          ORGANIZATION IS LINE SEQUENTIAL.
      *
       SELECT WH-FILE1
          ASSIGN TO "SORTED-CH20.txt"
          ORGANIZATION IS LINE SEQUENTIAL.
      *
       SELECT WH-FILE2
          ASSIGN TO "SORTED-LA10.txt"
          ORGANIZATION IS LINE SEQUENTIAL.
      *
       SELECT WH-FILE3
          ASSIGN TO "SORTED-NY30.txt"
          ORGANIZATION IS LINE SEQUENTIAL.
      *
       SELECT ERROR-FILE1
          ASSIGN TO "INVALID-WAREHOUSESCH20.txt"
        ORGANIZATION IS LINE SEQUENTIAL.
      *
       SELECT ERROR-FILE2
          ASSIGN TO "INVALID-WAREHOUSESLA10.txt"
        ORGANIZATION IS LINE SEQUENTIAL.
      *
       SELECT ERROR-FILE3
          ASSIGN TO "INVALID-WAREHOUSESNY30.txt"
        ORGANIZATION IS LINE SEQUENTIAL.
      *
       SELECT REPORT-FILE
          ASSIGN TO "INVEN-REPORT-KAD".
      *
       SELECT MERGED-FILE
          ASSIGN TO "merged-file.txt"
        ORGANIZATION IS LINE SEQUENTIAL.
      *
       SELECT SORT-FILE
          ASSIGN TO "SORT.tmp".

       DATA DIVISION.
       FILE SECTION.
	   
	   FD UNSORTED-FILE1
	        RECORD CONTAINS 128 CHARACTERS.
	   01 UNSORT-REC1.
          05 US1-WH-ID                PIC X(4).
          05 US1-VEN-ID               PIC X.
          05 US1-COST-ID              PIC X(3).
          05 US1-COST-DATA OCCURS 5 TIMES PIC X(24).
		  
	   FD UNSORTED-FILE2
	        RECORD CONTAINS 128 CHARACTERS.
	   01 UNSORT-REC2.
          05 US2-WH-ID                PIC X(4).
          05 US2-VEN-ID               PIC X.
          05 US2-COST-ID              PIC X(3).
          05 US1-COST-DATA OCCURS 5 TIMES PIC X(24).
		  
	   FD UNSORTED-FILE3
	        RECORD CONTAINS 128 CHARACTERS.
	   01 UNSORT-REC3.
          05 US3-WH-ID                PIC X(4).
          05 US3-VEN-ID               PIC X.
          05 US3-COST-ID              PIC X(3).
          05 US1-COST-DATA OCCURS 5 TIMES PIC X(24).
		  
	   FD WH-FILE1
	        RECORD CONTAINS 128 CHARACTERS.
	   01 SORT-REC1.
          05 SF1-WH-ID                PIC X(4).
          05 SF1-VEN-ID               PIC X.
          05 SF1-COST-ID              PIC X(3).
          05 US1-COST-DATA OCCURS 5 TIMES PIC X(24).
	   
	   FD WH-FILE2
	        RECORD CONTAINS 128 CHARACTERS.
	   01 SORT-REC2.
          05 SF2-WH-ID                PIC X(4).
          05 SF2-VEN-ID               PIC X.
          05 SF2-COST-ID              PIC X(3).
          05 US1-COST-DATA OCCURS 5 TIMES PIC X(24).
	   
	   FD WH-FILE3
	        RECORD CONTAINS 128 CHARACTERS.
	   01 SORT-REC3.
          05 SF3-WH-ID                PIC X(4).
          05 SF3-VEN-ID               PIC X.
          05 SF3-COST-ID              PIC X(3).
          05 US1-COST-DATA OCCURS 5 TIMES PIC X(24).
       
	   
	   FD MERGED-FILE.
       01 INVEN-REPORT-IN.
          05 WAREHOUSE-ID-IN      PIC X(4).
          05 VENDOR-ID-IN         PIC X.
          05 COSTUME-ID-IN        PIC X(3).
          05 COSTUME-DATA-IN      OCCURS 5 TIMES.
             10 COSTUME-NAME-IN   PIC X(13).
             10 COSTUME-SIZE-IN   PIC X.
             10 COSTUME-TYPE-IN   PIC X.
             10 IN-STOCK-IN       PIC S9(4).
             10 PURCHASE-PRICE-IN PIC S999V99.

       SD SORT-FILE.
	   01 USORT-REC.
          05 TMP-WH-ID                PIC X(4).
          05 TMP-VEN-ID               PIC X.
          05 TMP-COST-ID              PIC X(3).
          05 US1-COST-DATA OCCURS 5 TIMES PIC X(24).

       FD ERROR-FILE1
	      RECORD CONTAINS 128 CHARACTERS.
       01 ERROR-RECORD1          PIC X(128).
	   
       FD ERROR-FILE2
	      RECORD CONTAINS 128 CHARACTERS.
       01 ERROR-RECORD2          PIC X(128).
	   
       FD ERROR-FILE3
	      RECORD CONTAINS 128 CHARACTERS.
       01 ERROR-RECORD3          PIC X(128).
	   
       FD REPORT-FILE
          RECORD CONTAINS 80 CHARACTERS.

       01 REPORT-RECORD           PIC X(80).

       WORKING-STORAGE SECTION.

       01 REPORT-FIELDS.
          05 PROPER-SPACING       PIC S9V9  VALUE +1.

       01 FLAGS.
          05 EOF-FLAG             PIC X VALUE 'N'.
          05 NAME-FLAG            PIC X VALUE 'Y'.
          05 FIRST-REC            PIC X VALUE 'Y'.

       01 WS-DATE.
          05 WS-YEAR              PIC 9(4).
          05 WS-MONTH             PIC 99.
          05 WS-DAY               PIC 99.

       01 TEMP-VAR.
          05 WS-GRAND-TOT         PIC 9(9)V99.
          05 WS-VENDOR-TOT        PIC 9(7)V99.
          05 WS-WH-TOT            PIC 9(8)V99.
          05 WS-COSTUME-TOTAL     PIC 9(7)V99.
          05 WS-REC-TOT           PIC 9(6)V99.
          05 WS-WAREHOUSE-HOLD    PIC X(4).
          05 WS-VENDOR-HOLD       PIC X.
          05 WS-COST-HOLD         PIC X(3).
          05 SUB-ZERO             PIC 99      VALUE 1.
          05 WS-VENDOR-NAME       PIC X(11).
          05 TEMP                 PIC 9(6)V99.
          05 CHOICE               PIC X(4)    VALUE 'ALL'.

       01 PAGE-HEADING-1.
          05 PIC X(34) VALUE SPACES.
          05 PIC X(11) VALUE 'FIREFLY LTD'.

       01 PAGE-HEADING-2.
          05                      PIC X(10) VALUE SPACES.
          05 DL-MONTH             PIC 99.
          05                      PIC X VALUE '/'.
          05 DL-DAY               PIC 99.
          05                      PIC X VALUE '/'.
          05 DL-YEAR              PIC 9999.
          05                      PIC X(12) VALUE SPACES.
          05                      PIC X(16) VALUE 'INVENTORY REPORT'.
          05                      PIC X(21) VALUE SPACES.
          05                      PIC XXX   VALUE 'KAD'.

       01 WH-HEADER.
          05                      PIC XX.
          05                      PIC X(10) VALUE 'WAREHOUSE:'.
          05                      PIC X.
          05 WH-WAREHOUSE-ID      PIC X(4).

       01 VENDOR-HEADER.
          05                      PIC X(5) VALUE SPACES.
          05                      PIC X(7) VALUE 'VENDOR:'.
          05                      PIC X.
          05 VH-VENDOR-NAME       PIC X(11).

       01 COLUMN-HEADING-1.
          05 PIC X(8) VALUE SPACES.
          05 PIC X(7) VALUE 'COSTUME'.
          05 PIC X(7) VALUE SPACES.
          05 PIC X(4) VALUE 'COST'.
          05 PIC X(4) VALUE SPACES.
          05 PIC X(7) VALUE 'COSTUME'.
          05 PIC X(4) VALUE SPACES.
          05 PIC X(4) VALUE 'COST'.
          05 PIC X(4) VALUE SPACES.
          05 PIC XX   VALUE 'IN'.
          05 PIC X(7) VALUE SPACES.
          05 PIC X(5) VALUE 'TOTAL'.

       01 COLUMN-HEADING-2.
          05 PIC X(10) VALUE SPACES.
          05 PIC X(4)  VALUE 'NAME'.
          05 PIC X(9)  VALUE SPACES.
          05 PIC XX    VALUE 'ID'.
          05 PIC X(6)  VALUE SPACES.
          05 PIC X(4)  VALUE 'SIZE'.
          05 PIC X(6)  VALUE SPACES.
          05 PIC X(4)  VALUE 'TYPE'.
          05 PIC X(3)  VALUE SPACES.
          05 PIC X(5)  VALUE 'STOCK'.
          05 PIC X(5)  VALUE SPACES.
          05 PIC X(4)  VALUE 'COST'.

       01 GRP-COSTUME.
          05                      PIC X(44) VALUE SPACES.
          05                      PIC X(6)  VALUE 'TOTAL:'.
          05                      PIC X(3)  VALUE SPACES.
          05 GRP-COST-TOT         PIC $,$$$,$$$.99.

       01 GRP-VENDOR.
          05                      PIC X(13) VALUE SPACES.
          05                      PIC X(17) VALUE 'TOTAL FOR VENDOR:'.
          05                      PIC X     VALUE SPACES.
          05 GRP-VENDOR-NAME      PIC X(11).
          05                      PIC X(11) VALUE SPACES.
          05 GRP-VENDOR-TOT       PIC $,$$$,$$$.99.

       01 GRP-WAREHOUSE.
          05                      PIC X(10) VALUE SPACES.
          05                      PIC X(20) VALUE 
                            'TOTAL FOR WAREHOUSE:'.
          05                      PIC X     VALUE SPACES.
          05 GRP-WAREHOUSE-NAME   PIC X(4).
          05                      PIC X(17) VALUE SPACES.
          05 GRP-WH-TOT           PIC $$,$$$,$$$.99.

       01 GRP-GRAND.
          05                      PIC X(22) VALUE SPACES.
          05                      PIC X(17) VALUE
                            'GRAND TOTAL COST:'.
          05                      PIC X(12) VALUE SPACES.
          05 GRP-GRAND-TOT        PIC $$$,$$$,$$$.99.

       01 VENDOR-DATA.
          05                       PIC X(7) VALUE 'NNETON'.
          05                       PIC X(7) VALUE 'AAMEL'.
          05                       PIC X(7) VALUE 'QQUASON'.
          05                       PIC X(7) VALUE 'DDENIO'.
          05                       PIC X(7) VALUE 'WWEST'.
          05                       PIC X(7) VALUE 'VVISSON'.

       01 VENDOR-TABLE REDEFINES VENDOR-DATA.
          05 VENDOR-ITEM OCCURS 6 TIMES INDEXED BY VEN-INDEX.
             10 ST-ID            PIC X(1).
             10 ST-NAME          PIC X(6).

       01 WH-DATA.
          05                     PIC X(4) VALUE 'CH20'.
          05                     PIC X(4) VALUE 'LA10'.
          05                     PIC X(4) VALUE 'NY30'.
		  
       01 WH-TABLE REDEFINES WH-DATA.
          05 WH-ITEM OCCURS 3 TIMES INDEXED BY WH-INDEX.
             10 WHT-ID           PIC X(4).

       01 DETAIL-LINE.
          05                      PIC X(5) VALUE SPACES.
          05 DL-COST-NAME         PIC X(13).
          05                      PIC X(4) VALUE SPACES.
          05 DL-COST-ID           PIC X(3).
          05                      PIC X(4) VALUE SPACES.
          05 DL-SIZE              PIC X(8).
          05                      PIC X(3) VALUE SPACES.
          05 DL-TYPE              PIC X(5).
          05                      PIC X(3) VALUE SPACES.
          05 DL-IN-STK            PIC Z999.
          05                      PIC X(3) VALUE SPACES.
          05 DL-COST              PIC $$$,$$$.99.

      ********************PROCEDURE DIVISION**************************
       PROCEDURE DIVISION.

       000-MAIN-MODULE.
	      PERFORM 010-SORT-AND-MERGE
          PERFORM 025-HOUSEKEEPING
          PERFORM 050-READ-ROUTINE
          PERFORM 500-FINAL-ROUTINE
       .

	   010-SORT-AND-MERGE.
          DISPLAY "ENTER YOUR CHOICE OF WAREHOUSE" 
          DISPLAY "Choices are as follows: LA10, CH20, NY30, OR ALL"
          ACCEPT CHOICE
		  
          IF CHOICE = 'ALL' OR CHOICE = 'all' THEN
          SORT SORT-FILE
		     ON ASCENDING KEY   TMP-WH-ID
                                TMP-VEN-ID
                                TMP-COST-ID
             INPUT PROCEDURE IS 011-WH-CK1
             GIVING WH-FILE1
			 
          SORT SORT-FILE
             ON ASCENDING KEY   TMP-WH-ID
                                TMP-VEN-ID
                                TMP-COST-ID
             INPUT PROCEDURE IS 012-WH-CK2
             GIVING WH-FILE2
			 
          SORT SORT-FILE
             ON ASCENDING KEY   TMP-WH-ID
                                TMP-VEN-ID
                                TMP-COST-ID
             INPUT PROCEDURE IS 013-WH-CK3
             GIVING WH-FILE3
			 
          MERGE SORT-FILE
             ON ASCENDING KEY   WAREHOUSE-ID-IN
                                VENDOR-ID-IN
                                COSTUME-ID-IN
             USING WH-FILE1, WH-FILE2, WH-FILE3
             GIVING MERGED-FILE

          END-IF
		  
          IF CHOICE = 'CH20' OR CHOICE = 'ch20' THEN
          SORT SORT-FILE
             ON ASCENDING KEY   TMP-WH-ID
                                TMP-VEN-ID
                                TMP-COST-ID
             INPUT PROCEDURE IS 011-WH-CK1
             GIVING WH-FILE1
			 
          MERGE SORT-FILE
             ON ASCENDING KEY   WAREHOUSE-ID-IN
                                VENDOR-ID-IN
                                COSTUME-ID-IN
             USING WH-FILE1
             GIVING MERGED-FILE

          END-IF
          IF CHOICE = 'LA10' OR CHOICE = 'la10' THEN
          SORT SORT-FILE
             ON ASCENDING KEY   TMP-WH-ID
                                TMP-VEN-ID
                                TMP-COST-ID
             INPUT PROCEDURE IS 012-WH-CK2
             GIVING WH-FILE2
			 
          MERGE SORT-FILE
             ON ASCENDING KEY   WAREHOUSE-ID-IN
                                VENDOR-ID-IN
                                COSTUME-ID-IN
             USING WH-FILE2
             GIVING MERGED-FILE

          END-IF
          IF CHOICE = 'NY30' OR CHOICE = 'ny30' THEN
          SORT SORT-FILE
            ON ASCENDING KEY   TMP-WH-ID
                               TMP-VEN-ID
                               TMP-COST-ID
            INPUT PROCEDURE IS 013-WH-CK3
			GIVING WH-FILE3
			 
          MERGE SORT-FILE
             ON ASCENDING KEY   WAREHOUSE-ID-IN
                                VENDOR-ID-IN
                                COSTUME-ID-IN
             USING WH-FILE3
             GIVING MERGED-FILE

          END-IF
		  
          MOVE 'N' TO EOF-FLAG	 
       .
	   
       011-WH-CK1.
          OPEN INPUT UNSORTED-FILE1
               OUTPUT ERROR-FILE1
		  PERFORM UNTIL EOF-FLAG = 'Y'
			 READ UNSORTED-FILE1
                AT END MOVE 'Y' TO EOF-FLAG
                NOT AT END
                   SET WH-INDEX TO 1
                   SEARCH WH-ITEM
                      AT END PERFORM 155-ERROR-WRITE-A-LINE1
                      WHEN US1-WH-ID = WHT-ID(WH-INDEX)
                         MOVE UNSORT-REC1 TO USORT-REC
                         RELEASE USORT-REC
                   END-SEARCH
          END-PERFORM
          MOVE 'N' TO EOF-FLAG
          CLOSE ERROR-FILE1
                UNSORTED-FILE1
       .

       012-WH-CK2.
          OPEN INPUT UNSORTED-FILE2
               OUTPUT ERROR-FILE2
          PERFORM UNTIL EOF-FLAG = 'Y'
             READ UNSORTED-FILE2
                AT END MOVE 'Y' TO EOF-FLAG
                NOT AT END
                   SET WH-INDEX TO 1
                   SEARCH WH-ITEM
                      AT END PERFORM 156-ERROR-WRITE-A-LINE2
                      WHEN US2-WH-ID = WHT-ID(WH-INDEX)
                        MOVE UNSORT-REC2 TO USORT-REC
                        RELEASE USORT-REC
                   END-SEARCH
          END-PERFORM
          MOVE 'N' TO EOF-FLAG
		
		CLOSE ERROR-FILE2
		      UNSORTED-FILE2
       .

       013-WH-CK3.
          OPEN INPUT UNSORTED-FILE3
               OUTPUT ERROR-FILE3
          PERFORM UNTIL EOF-FLAG = 'Y'
             READ UNSORTED-FILE3
                AT END MOVE 'Y' TO EOF-FLAG
                NOT AT END
                   SET WH-INDEX TO 1
                   SEARCH WH-ITEM
                       AT END PERFORM 157-ERROR-WRITE-A-LINE3
                       WHEN US3-WH-ID = WHT-ID(WH-INDEX)
                          MOVE UNSORT-REC3 TO USORT-REC
                          RELEASE USORT-REC
                   END-SEARCH
          END-PERFORM
          MOVE 'N' TO EOF-FLAG
          CLOSE ERROR-FILE3
                UNSORTED-FILE3
       .	   
	   
       025-HOUSEKEEPING.
          OPEN INPUT  MERGED-FILE
               OUTPUT REPORT-FILE
          MOVE FUNCTION CURRENT-DATE TO WS-DATE
          MOVE WS-YEAR  TO DL-YEAR
          MOVE WS-MONTH TO DL-MONTH
          MOVE WS-DAY   TO DL-DAY
       .


       050-READ-ROUTINE.
          PERFORM UNTIL EOF-FLAG = 'Y'
             READ MERGED-FILE
                AT END
                   MOVE 'Y' TO EOF-FLAG
				   PERFORM 300-EOJ-ROUTINE
                NOT AT END
                   PERFORM 100-PROCESS-ROUTINE
             END-READ
          END-PERFORM
       .

       075-HEADING-ROUTINE.
          PERFORM 076-PAGE-HEADING
          PERFORM 077-WAREHOUSE-HEADING
          PERFORM 078-VENDOR-HEADING
          PERFORM 079-COL-HDR
	   .
	   
       076-PAGE-HEADING.
          MOVE 2 TO PROPER-SPACING
          MOVE PAGE-HEADING-1 TO REPORT-RECORD
          PERFORM 150-WRITE-A-LINE
          MOVE 1 TO PROPER-SPACING
          MOVE PAGE-HEADING-2 TO REPORT-RECORD
          PERFORM 150-WRITE-A-LINE
	   .

       077-WAREHOUSE-HEADING.
          MOVE 2 TO PROPER-SPACING
          MOVE WH-HEADER TO REPORT-RECORD
          PERFORM 150-WRITE-A-LINE
	   . 
		 
       078-VENDOR-HEADING.
          MOVE 2 TO PROPER-SPACING
          MOVE VENDOR-HEADER TO REPORT-RECORD
          PERFORM 150-WRITE-A-LINE
       .
	   
       079-COL-HDR.
          MOVE 3 TO PROPER-SPACING
          MOVE COLUMN-HEADING-1 TO REPORT-RECORD
          PERFORM 150-WRITE-A-LINE
          MOVE COLUMN-HEADING-2 TO REPORT-RECORD
          PERFORM 150-WRITE-A-LINE
		  MOVE 2 TO PROPER-SPACING
       .

       100-PROCESS-ROUTINE.
          SET VEN-INDEX TO 1
          SEARCH VENDOR-ITEM
             AT END
                STRING
                'INVALID' DELIMITED BY ' '
				' ' DELIMITED BY SIZE
                 VENDOR-ID-IN DELIMITED BY ' '
                 INTO WS-VENDOR-NAME
             WHEN VENDOR-ID-IN = ST-ID(VEN-INDEX)
                MOVE ST-NAME(VEN-INDEX) TO WS-VENDOR-NAME
          END-SEARCH

		  EVALUATE TRUE
             WHEN FIRST-REC = 'Y'
			   MOVE WS-VENDOR-NAME TO VH-VENDOR-NAME
               MOVE 'N' TO FIRST-REC
               MOVE WAREHOUSE-ID-IN TO WS-WAREHOUSE-HOLD
               MOVE VENDOR-ID-IN    TO WS-VENDOR-HOLD
               MOVE COSTUME-ID-IN   TO WS-COST-HOLD
               MOVE WS-WAREHOUSE-HOLD TO WH-WAREHOUSE-ID
			   PERFORM 075-HEADING-ROUTINE
			   
             WHEN WAREHOUSE-ID-IN NOT = WS-WAREHOUSE-HOLD
               PERFORM 200-WH-CONTROL-BREAK
			   PERFORM 077-WAREHOUSE-HEADING
			   PERFORM 078-VENDOR-HEADING
			   PERFORM 079-COL-HDR

             WHEN VENDOR-ID-IN NOT = WS-VENDOR-HOLD
               PERFORM 225-V-CONTROL-BREAK
			   PERFORM 078-VENDOR-HEADING
			   PERFORM 079-COL-HDR

             WHEN COSTUME-ID-IN NOT = WS-COST-HOLD
               PERFORM 250-C-CONTROL-BREAK
			   PERFORM 079-COL-HDR
			   
          END-EVALUATE
		  
          PERFORM VARYING SUB-ZERO FROM 1 BY 1 UNTIL SUB-ZERO > 5
           IF COSTUME-NAME-IN(SUB-ZERO) NOT = SPACES THEN  
			 IF NAME-FLAG = 'Y' THEN
                MOVE COSTUME-NAME-IN(SUB-ZERO)  TO DL-COST-NAME
                MOVE 'N' TO NAME-FLAG
             ELSE
                MOVE SPACES TO DL-COST-NAME
             END-IF
			 
			 MOVE COSTUME-ID-IN TO DL-COST-ID
			 
			 EVALUATE TRUE
			    WHEN COSTUME-SIZE-IN(SUB-ZERO) = 'L'
				   MOVE 'LARGE' TO DL-SIZE
                WHEN COSTUME-SIZE-IN(SUB-ZERO) = 'M'
                   MOVE 'MEDIUM' TO DL-SIZE
                WHEN COSTUME-SIZE-IN(SUB-ZERO) = 'S'
                   MOVE 'SMALL' TO DL-SIZE
                WHEN COSTUME-SIZE-IN(SUB-ZERO) = 'P'
                   MOVE 'PLUS' TO DL-SIZE
                WHEN OTHER
				   MOVE SPACES TO DL-SIZE
				   STRING
				   'BAD' DELIMITED BY ' '
				   ' ' DELIMITED BY SIZE
				   COSTUME-SIZE-IN(SUB-ZERO)
				   INTO DL-SIZE
			 END-EVALUATE
			 
			 EVALUATE TRUE
                WHEN COSTUME-TYPE-IN(SUB-ZERO) = 'A'
                   MOVE 'ADULT' TO DL-TYPE
                WHEN COSTUME-TYPE-IN(SUB-ZERO) = 'C'
                   MOVE 'CHILD' TO DL-TYPE
                WHEN OTHER
                   MOVE SPACES TO DL-TYPE
				   STRING
                   'BAD' DELIMITED BY ' '
                   ' ' DELIMITED BY SIZE
                   COSTUME-TYPE-IN(SUB-ZERO)
                   INTO DL-TYPE
		     END-EVALUATE
			 
			 IF IN-STOCK-IN(SUB-ZERO) IS NUMERIC
			    MOVE IN-STOCK-IN(SUB-ZERO) TO TEMP
		     ELSE
			    MOVE ZEROS TO TEMP
			 END-IF
			 MOVE TEMP TO DL-IN-STK
			 
			 IF PURCHASE-PRICE-IN(SUB-ZERO) IS NUMERIC
				MULTIPLY PURCHASE-PRICE-IN(SUB-ZERO) 
	              BY TEMP GIVING WS-REC-TOT
			    ADD  WS-REC-TOT TO WS-WH-TOT
			    ADD  WS-REC-TOT TO WS-VENDOR-TOT
			    ADD  WS-REC-TOT TO WS-GRAND-TOT
			    ADD  WS-REC-TOT TO WS-COSTUME-TOTAL
			    MOVE WS-REC-TOT TO DL-COST
			 ELSE
			    MOVE ZEROS TO DL-COST
			 END-IF
			 
			 MOVE DETAIL-LINE TO REPORT-RECORD
			 PERFORM 150-WRITE-A-LINE
             MOVE SPACES TO DL-COST-ID
		   END-IF
          END-PERFORM
       .

       150-WRITE-A-LINE.
          WRITE REPORT-RECORD
             AFTER ADVANCING PROPER-SPACING
          MOVE 1 TO PROPER-SPACING
       .
	   
       155-ERROR-WRITE-A-LINE1.
          MOVE UNSORT-REC1 TO ERROR-RECORD1
          WRITE ERROR-RECORD1
             AFTER ADVANCING PROPER-SPACING
          MOVE 1 TO PROPER-SPACING
	   .
	   
       156-ERROR-WRITE-A-LINE2.
          MOVE UNSORT-REC2 TO ERROR-RECORD2
          WRITE ERROR-RECORD2
             AFTER ADVANCING PROPER-SPACING
          MOVE 1 TO PROPER-SPACING
	   .
	   
       157-ERROR-WRITE-A-LINE3.
          MOVE UNSORT-REC3 TO ERROR-RECORD3
          WRITE ERROR-RECORD3
             AFTER ADVANCING PROPER-SPACING
          MOVE 1 TO PROPER-SPACING
	   .

       200-WH-CONTROL-BREAK.
          PERFORM 225-V-CONTROL-BREAK
          MOVE WS-WH-TOT TO GRP-WH-TOT
          MOVE WS-WAREHOUSE-HOLD TO GRP-WAREHOUSE-NAME
          MOVE WAREHOUSE-ID-IN TO WS-WAREHOUSE-HOLD
          MOVE WAREHOUSE-ID-IN TO WH-WAREHOUSE-ID
          
          MOVE GRP-WAREHOUSE TO REPORT-RECORD
          MOVE 2 TO PROPER-SPACING
          PERFORM 150-WRITE-A-LINE
          MOVE ZEROS TO WS-WH-TOT
		  
       .

       225-V-CONTROL-BREAK.
          PERFORM 250-C-CONTROL-BREAK
          MOVE WS-VENDOR-TOT TO GRP-VENDOR-TOT
          MOVE VH-VENDOR-NAME TO GRP-VENDOR-NAME
          MOVE ZEROS TO WS-VENDOR-TOT
          MOVE VENDOR-ID-IN TO WS-VENDOR-HOLD
          MOVE WS-VENDOR-NAME TO VH-VENDOR-NAME
          
          MOVE 3 TO PROPER-SPACING
          MOVE GRP-VENDOR TO REPORT-RECORD
          PERFORM 150-WRITE-A-LINE
          MOVE 3 TO PROPER-SPACING
       .

       250-C-CONTROL-BREAK.
          MOVE COSTUME-ID-IN TO WS-COST-HOLD
          MOVE WS-COSTUME-TOTAL TO GRP-COST-TOT
          MOVE GRP-COSTUME TO REPORT-RECORD
          MOVE 2 TO PROPER-SPACING
          PERFORM 150-WRITE-A-LINE
          
          MOVE ZEROS TO WS-COSTUME-TOTAL
          MOVE 'Y' TO NAME-FLAG
          MOVE 3 TO PROPER-SPACING
       .

       300-EOJ-ROUTINE.
          EVALUATE TRUE
             WHEN EOF-FLAG = 'Y'
                PERFORM 400-PRINT-FINAL-TOTALS
          END-EVALUATE
	   .
	   
       400-PRINT-FINAL-TOTALS.
          PERFORM 200-WH-CONTROL-BREAK
          MOVE WS-GRAND-TOT TO GRP-GRAND-TOT
          MOVE 2 TO PROPER-SPACING
          MOVE GRP-GRAND TO REPORT-RECORD
          PERFORM 150-WRITE-A-LINE
	   .
	   
       500-FINAL-ROUTINE.
          CLOSE MERGED-FILE
                REPORT-FILE
           STOP RUN
       .
		
	   