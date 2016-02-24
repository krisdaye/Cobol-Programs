       IDENTIFICATION DIVISION.
       PROGRAM-ID.	  PROGRAM1.
       AUTHOR.			  DAYE.
      *************************COMMENT SECTION************************
      * DESCRIPTION:
      *  THIS PROGRAM TAKES IN RAW DATA ABOUT HAL S AUTO
      *  PARTS WAREHOUSE FROM A FILE AND PERFORMS AN 
      *  ANALYSIS OF THE INFORMATION. USING THIS DATA,
      *  THE PROGRAM COMPUTES THE TOTAL QUANTITY IN STOCK,
      *  THE VALUE OF THIS STOCK, AND COMPUTES IF ANY ITEM
      *  HAS EXCESS RETURNS FOR THAT WEEK. IF ANY ITEM EITHER
      *  HAS NO SALES OR HAS EXCESS RETURNS, THEN THE
      *  INFORMATION ABOUT ONLY THOSE ITEMS ARE WRITTEN TO THE
      *  OUTPUT FILE.
      ****************************************************************
      * INPUT FILE:
      *  1. CATALOG NUMBER OF PRODUCT
      *  2. ITEM DESCRIPTION
      *  3. UNIT PRICE
      *  4. QUANTITY ON HAND
      *  5. QUANTITY ON ORDER
      *  6. POINT OF REORDER
      *  7. Quantity Received IN THE WEEK
      *  8. QUANTITY SOLD FOR THE WEEK
      *  9. QUANTITY RETURNED FOR THE WEEK
      ****************************************************************
      * OUTPUT FILE:
      *  1. CATALOG NUMBER
      *  2. ITEM DESCRIPTION
      *  3. PURCHASE PRICE
      *  4. QUANTITY ON HAND
      *  5. VALUE OF STOCK
      *  6. NO SALE
      *  7. EXCESS RETURN
      ****************************************************************
      * DEFINITIONS:
      *   NO SALE: QUANTITY SOLD FOR WEEK IS ZERO
      *   EXCESS RETURN: IF WEEKLY RETURNS ARE GREATER
      *                  THAN HALF OF THE WEEKLY SOLD
      ****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *
       SOURCE-COMPUTER.		IBM-PC.
       OBJECT-COMPUTER.		IBM-PC.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *
        SELECT DISK-FILE
		 ASSIGN TO "PR1FA14.TXT"
		 ORGANIZATION IS LINE SEQUENTIAL.
      *
       SELECT REPORT-FILE
	     ASSIGN TO "INVENTORY-ANALYSIS-REPORT.TXT"
		 ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
	  
       FILE SECTION.
	  
	   FD DISK-FILE
		RECORD CONTAINS 60 CHARACTERS.

       01 INV-ANALYSIS-REPORT.
          05 CATALOG-NUMBER	      PIC X(5).
          05 ITEM-DESCRIPTION	  PIC X(20).
          05 UNIT-PRICE		      PIC 999V99.
          05 FILLER			      PIC X(6).
          05 QTY-ON-HAND		  PIC 9(4).
          05 QTY-ON-ORDER		  PIC 9(4).
          05 REORDER-POINT	      PIC 9(4).
          05 QTY-RECEIVED-WK	  PIC 9(4).
          05 QTY-SOLD-WK		  PIC 9(4).
          05 QTY-RETURNED-WK	  PIC 9(4).
	  
       FD REPORT-FILE
        RECORD CONTAINS 80 CHARACTERS.

       01 REPORT-RECORD		      PIC X(80).

	   WORKING-STORAGE SECTION.

       01 REPORT-FIELDS.
          05 PROPER-SPACING	      PIC S9V9	VALUE +1.
       
       01 FLAGS-SWITCHES.
          05 EOF-FLAG		      PIC X	VALUE ' '.
       
       01 TOTAL-FIELDS.
          05 TOT-STK-VAL	      PIC 9(15).
       
       01 TEMP-VAL.
          05 WS-STK-VAL	          PIC 9(13).
          05 QTY-IN-STK	          PIC 9(5).
          05 TEST-1		          PIC 9(4).
       
       01 WS-DATE.
          05 WS-YEAR              PIC 9(4).
          05 WS-MONTH             PIC 99.
          05 WS-DAY               PIC 99.
       
       01 PAGE-HEADING-LINE.
          05 DL-MONTH             PIC 99.
          05                      PIC X VALUE '/'.
          05 DL-DAY               PIC 99.
          05                      PIC X VALUE '/'.
          05 DL-YEAR              PIC 9(4).
          05		              PIC X(5) VALUE ' '.
          05		              PIC X(3) VALUE 'KAD'.
          05		              PIC X(10) VALUE ' '.
          05		              PIC X(26) VALUE
                                    'HAL S AUTO PARTS WAREHOUSE'.
          05 		              PIC X(17) VALUE ' '.
          05		              PIC X(5) VALUE 'PAGE '.
          05 Page-No	          PIC 99.
       
       01 REPORT-TYPE-HEADER.
          05 		              PIC X(31) VALUE ' '.
          05		              PIC X(21) VALUE 
                                   'STOCK ANALYSIS REPORT'.
       
       01 COLUMN-HDR-1.
          05                      PIC X(1) VALUE ' '.
          05                      PIC X(3) VALUE 'CAT'.
          05                      PIC X(11) VALUE ' '.
          05                      PIC X(4) VALUE 'ITEM'.
          05                      PIC X(11) VALUE ' '.
          05                      PIC X(8) VALUE 'PURCH PR'.
          05                      PIC X(3) VALUE ' '.
          05                      PIC X(8) VALUE 'QUANTITY'.
          05                      PIC X(7) VALUE ' '.
          05                      PIC X(5) VALUE 'STOCK'.
          05                      PIC X(7) VALUE ' '.
          05                      PIC X(2) VALUE 'NO'.
          05                      PIC X(2) VALUE ' '.
          05                      PIC X(6) VALUE 'EXCESS'.
          05                      PIC X(2) VALUE ' '.
       
       01 COLUMN-HDR-2.
          05                      PIC X(1) VALUE ' '.
          05                      PIC X(3) VALUE 'NUM'.
          05                      PIC X(8) VALUE ' '.
          05                      PIC X(11) VALUE 'DESCRIPTION'.
          05                      PIC X(7) VALUE ' '.
          05                      PIC X(8) VALUE 'PER UNIT'.
          05                      PIC X(4) VALUE ' '.
          05                      PIC X(6) VALUE 'IN STK'.
          05                      PIC X(8) VALUE ' '.
          05                      PIC X(5) VALUE 'VALUE'.
          05                      PIC X(6) VALUE ' '.
          05                      PIC X(4) VALUE 'SALE'.
          05                      PIC X(1) VALUE ' '.
          05                      PIC X(6) VALUE 'RETURN'.
          05                      PIC X(2) VALUE ' '.
       
       
       01 DETAIL-LINE.
          05  DL-CAT-NUM          PIC X(5).
          05                      PIC X(3) VALUE ' '.
          05  DL-DESCRIP          PIC X(20).
          05                      PIC X(3) VALUE ' '.
          05  DL-PUR-PR           PIC $Z99.99.
          05                      PIC X(4) VALUE ' '.
          05  DL-QTY              PIC ZZ,Z99.
          05                      PIC X(4) VALUE ' '.
          05  DL-STK-VAL          PIC $Z,ZZZ,Z99.99.
          05                      PIC X(4) VALUE ' '.
          05  DL-NO-SALE          PIC X.
          05                      PIC X(5) VALUE ' '.
          05  DL-EX-RTN           PIC X.
          05                      PIC X(3) VALUE ' '.
       
       01 FILL-LINE               PIC X(80) VALUE ' '.
	   
	   01 TOTAL-LINE.
        05                        PIC X(21) VALUE ' '.
        05                        PIC X(27) VALUE 
					'TOTAL VALUE OF STOCK LISTED'.
        05                        PIC X(3) VALUE ' '.
        05 TL-STK-VAL	          PIC $$$,$$$,$$9.99.

	   PROCEDURE DIVISION.
	   
	   000-MAIN-MODULE.
         PERFORM 125-HOUSEKEEPING
         PERFORM 175-READ-REPORT-FILE
         PERFORM 275-PRINT-TOTAL
	   .
	   
	   125-HOUSEKEEPING.
         OPEN INPUT   DISK-FILE
			 OUTPUT  REPORT-FILE	 
         PERFORM 150-HEADING-ROUTINE	   
	   .
	   
	   150-HEADING-ROUTINE.
          MOVE FUNCTION CURRENT-DATE TO WS-DATE
          MOVE WS-YEAR TO DL-YEAR
          MOVE WS-DAY TO DL-DAY
          MOVE WS-MONTH TO DL-MONTH
          
          ADD 1 TO PAGE-NO
          
          WRITE REPORT-RECORD FROM PAGE-HEADING-LINE
            AFTER ADVANCING PROPER-SPACING
          
          MOVE 2 TO PROPER-SPACING
          WRITE REPORT-RECORD FROM REPORT-TYPE-HEADER
          	AFTER ADVANCING PROPER-SPACING
          
          MOVE 3 TO PROPER-SPACING
          WRITE REPORT-RECORD FROM COLUMN-HDR-1
          	AFTER ADVANCING PROPER-SPACING
          
          MOVE 1 TO PROPER-SPACING
          WRITE REPORT-RECORD FROM COLUMN-HDR-2
          	AFTER ADVANCING PROPER-SPACING
          WRITE REPORT-RECORD FROM FILL-LINE
	   .
	   
	   175-READ-REPORT-FILE.
		PERFORM UNTIL EOF-FLAG = 'Y'
			READ DISK-FILE
				AT END
					MOVE 'Y' TO EOF-FLAG
				NOT AT END
                    PERFORM 200-PROCESS-REPORT-RECORD
			END-READ
		END-PERFORM
	   .
	   
       200-PROCESS-REPORT-RECORD.
	   MOVE CATALOG-NUMBER TO DL-CAT-NUM
	   MOVE ITEM-DESCRIPTION TO DL-DESCRIP
	   
	   MOVE 0 TO QTY-IN-STK
	   ADD QTY-ON-HAND TO QTY-IN-STK
	   ADD QTY-RETURNED-WK TO QTY-IN-STK
	   ADD QTY-RECEIVED-WK TO QTY-IN-STK
	   SUBTRACT QTY-SOLD-WK FROM QTY-IN-STK 
	   MULTIPLY UNIT-PRICE BY QTY-IN-STK GIVING WS-STK-VAL
	   ADD WS-STK-VAL TO TOT-STK-VAL
	   
	   MOVE WS-STK-VAL TO DL-STK-VAL
	   MOVE UNIT-PRICE TO DL-PUR-PR
	   MOVE QTY-IN-STK TO DL-QTY

	   IF QTY-RETURNED-WK >= (QTY-SOLD-WK / 2) THEN
		 MOVE 'X' TO DL-EX-RTN
	   END-IF
	   IF QTY-SOLD-WK = 0 THEN
		    MOVE 'X' TO DL-NO-SALE
	   END-IF
	   IF DL-EX-RTN = 'X' OR DL-NO-SALE = 'X' THEN
	     MOVE DETAIL-LINE TO REPORT-RECORD
		 PERFORM 250-WRITE-A-LINE
	   END-IF
	   MOVE ' ' TO DL-EX-RTN, DL-NO-SALE
	   .
	   
       250-WRITE-A-LINE.
		WRITE REPORT-RECORD
			AFTER ADVANCING PROPER-SPACING
	   .
	   
       275-PRINT-TOTAL.
        MOVE TOT-STK-VAL TO TL-STK-VAL
        MOVE TOTAL-LINE TO REPORT-RECORD
        MOVE 3 TO PROPER-SPACING
        PERFORM 250-WRITE-A-LINE
	   .
	   
       300-FINAL-ROUTINE.
		CLOSE DISK-FILE
              REPORT-FILE
		STOP RUN
	   .
	   