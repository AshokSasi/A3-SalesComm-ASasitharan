       identification division.
       program-id. A3-SalesComm.
       author. Ashok Sasitharan.
       date-written. 2021-02-10.
      *Program Description:
      *This program takes in a input .dat file with salesperson info
      *and outputs them in a formatted page separated text file with
      *subtotals
       environment division.
      *
       input-output section.
       file-control.
      *Input File
           select sales-file
               assign to "../../../data/A3.dat"
               organization is line sequential.
      * Output File
           select report-file
               assign to "..\..\..\data\A3-SalesComm.out"
               organization is line sequential.
      *
       data division.
       file section.
       fd sales-file
           data record is sales-rec
           record contains 32 characters.
      *Input variables
       01 sales-rec.
         05 sr-sman-num pic 999.
         05 sr-name pic x(8).
         05 sr-sales pic 9(6).
         05 sr-min pic 9(6).
         05 sr-max pic 9(6).
         05 sr-rate pic 99v9.
      *
       fd report-file
           data record is report-line
           record contains 120 characters.
      *
       01 report-line pic x(120).
      *
       working-storage section.
      *
       01 ws-eof-flag pic x value 'n'.
      *Name Heading 
       01 ws-heading1-name-line.
         05 filler                     pic x(73) value spaces.
         05 ws-name-title              pic x(30) value 
                                       "Ashok Sasitharan, Assignment 3".
      *Title Heading
       01 ws-heading2-title.
         05 filler                     pic x(35) value spaces.
         05 ws-name-title              pic x(30) value
                                       "SALES COMMISSION REPORT".
      *
       01 ws-heading3-headings.
         05 ws-num-h3                  pic x(3) value "NO.".
         05 filler                     pic x(5) value spaces.
         05 ws-name-h3                 pic x(4) value "NAME".
         05 filler                     pic x(6) value spaces.
         05 ws-sales-h3                pic x(5) value "SALES".
         05 filler                     pic x(7) value spaces.
         05 ws-min-h3                  pic x(3) value "MIN".
         05 filler                     pic x(8) value spaces.
         05 ws-max-h3                  pic x(3) value "MAX".
         05 filler                     pic x(8) value spaces.
         05 ws-rate-h3                 pic x(4) value "RATE".
         05 filler                     pic x(7) value spaces.
         05 ws-earned-h3               pic x(6) value "EARNED".
         05 filler                     pic x(8) value spaces.
         05 ws-paid-h3                 pic x(4) value "PAID".
         05 filler                     pic x(6) value spaces.
         05 ws-no-h3                   pic x(14) value "BONUS/NO BONUS".
      *
       01 ws-heading4-underlines.
         05 ws-num-h4                  pic x(3) value "---".
         05 filler                     pic x(3) value spaces.
         05 ws-name-h4                 pic x(8) value "--------".
         05 filler                     pic x(3) value spaces.
         05 ws-sales-h4                pic x(8) value "--------".
         05 filler                     pic x(3) value spaces.
         05 ws-min-h4                  pic x(8) value "--------".
         05 filler                     pic x(3) value spaces.
         05 ws-max-h4                  pic x(8) value "--------".
         05 filler                     pic x(3) value spaces.
         05 ws-rate-h4                 pic x(8) value "--------".
         05 filler                     pic x(3) value spaces.
         05 ws-earned-h4               pic x(10) value "----------".
         05 filler                     pic x(3) value spaces.
         05 ws-paid-h4                 pic x(10) value "----------".
         05 filler                     pic x(3) value spaces.
         05 ws-bonus-h4                pic x(16)
                                       value "----------------".
      *Report detail line that prints out the main information
       01 ws-report-detail-line.
         05 ws-num-dl                  pic 9(3).
         05 filler                     pic x(3) value spaces.
         05 ws-name-dl                 pic x(8).
         05 filler                     pic x(3) value spaces.
         05 ws-sales-dl                pic ZZZ,ZZ9.
         05 filler                     pic x(4) value spaces.
         05 ws-min-dl                  pic ZZZ,ZZ9.
         05 filler                     pic x(5) value spaces.
         05 ws-max-dl                  pic ZZZ,ZZ9.
         05 filler                     pic x(4) value spaces.
         05 ws-rate-dl                 pic ZZ9.9.
         05 ws-percent-dl              pic x(1).
         05 filler                     pic x(5) value spaces.
         05 ws-earned-dl               pic Z,ZZZ,ZZ9.
         05 filler                     pic x(3) value spaces.
         05 ws-paid-dl                 pic $*,***,**9.
         05 filler                     pic x(3) value spaces.
         05 ws-bonus-dl                pic x(15).
      * Counter variables
       01 ws-counters.
         05 ws-bonus-above-max-count   pic 9(4) value 0.
         05 ws-no-bonus-less-min-count pic 9(4) value 0.
         05 ws-total-bonus-count       pic 9(4) value 0.
         05 ws-no-bonus-count          pic 9(4) value 0.
         05 ws-salespeople-count       pic 9(4) value 0.
         05 ws-paid-equal-earned-count pic 9(4) value 0.
      *Calculation placeholder variables
       01 ws-calcs.
         05 ws-earned-calc             pic 9(9) value 0.
         05 ws-earned-amount-over      pic 9(9) value 0.
         05 ws-rate-decimal            pic 99V9999.
         05 ws-paid-calc               pic 9(9) value 0.
         05 ws-bonus-output            pic x(15) .
         05 ws-total-earned            pic 9(9) value 0.
         05 ws-total-paid              pic 9(9) value 0.
         05 ws-paid-equal-calc         pic 9(3)V9999.
         05 ws-bonus-percent-calc      pic 9(3)V9999.
         05 ws-no-bonus-percent-calc   pic 9(3)V9999.
      * Constants
       77 ws-percent-sign              pic x value "%".
       77 ws-page-count                pic 99 value 0.
       77 ws-lines-per-page            pic 99 value 10.
       77 ws-line-count                pic 99 value 0.
       77 ws-one                       pic 9 value 1.
       77 ws-two                       pic 9 value 2.
       77 ws-yes                       pic x(1) value "y".
       77 ws-bonus-sales-cap           pic 9(6) value 300000.
       77 ws-bonus-percent             pic 99V9999 value 0.1525.
       77 ws-hundred                   pic 999V99 value 100.00.
       77 ws-bonus-cnst                pic x(12) value "BONUS EARNED".
       77 ws-no-bonus-cnst             pic x(15)
                                       value "NO BONUS EARNED".
      *
      *Prints out totals for earned and paid
       01 ws-total-line.
         05 filler                     pic x(50) value spaces.
         05 ws-totals-tl               pic x(6) value "Totals".
         05 filler                     pic x(5) value spaces.
         05 ws-total-earned-tl         pic $$,$$$,$$9.
         05 filler                     pic x(3) value spaces.
         05 ws-total-paid-tl           pic $$,$$$,$$9.
      *
      *Output for bonus greater than max
       01 ws-bonus-greater-than-report-line.
         05 filler                     pic x(31)
                                value "NUMBER WITH BONUS MORE THAN MAX".
         05 filler                     pic x(6) value spaces.
         05 ws-bonus-above-max-br      pic ZZZ9.
      * Output for people with no bonus and earned less than min
       01 ws-no-bonus-less-than-min-report-line.
         05 filler                     pic x(34)
                             value "NUMBER WITH NO BONUS LESS THAN MIN".
         05 filler                     pic x(3) value spaces.
         05 ws-no-bonues-less-min-br   pic ZZZ9.
      * Output for number of people with a bonus
       01 ws-num-with-bonus-report-line.
         05 filler                     pic x(32)
                               value "NUMBER OF SALESPEOPLE WITH BONUS".
         05 filler                     pic x(5) value spaces.
         05 ws-amount-bonus-rl         pic ZZZ9.
      *Output for number of people without a bonus
       01 ws-num-without-bonus-report-line.
         05 filler                     pic x(35)
                            value "NUMBER OF SALESPEOPLE WITHOUT BONUS".
         05 filler                     pic x(2) value spaces.
         05 ws-without-bonus-rl        pic ZZZ9.
      * Output for number of salespeople
       01 ws-num-salespeople-report-line.
         05 filler                     pic x(21)
                                       value "NUMBER OF SALESPEOPLE".
         05 filler                     pic x(16) value spaces.
         05 ws-num-salespeople-rl      pic ZZZ9.
      * Output for people with paid equal to their earned
       01 ws-paid-equal-earned-report-line.
         05 filler                     pic x(30)
                                 value "NUMBER  WITH PAID EQUAL EARNED".
         05 filler                     pic x(7) value spaces.
         05 ws-number-paid-equal-rl    pic ZZZ9.
      * Output for percent of people with paid equal to their earned
       01 ws-paid-equal-earned-percent-report-line.
         05 filler                     pic x(30)
                                 value "PERCENT WITH PAID EQUAL EARNED".
         05 filler                     pic x(7) value spaces.
         05 ws-percent-paid-equal-rl   pic ZZZ9.99.
         05 ws-percent-pee             pic x(1).
      * Output for percent of people with a bonus
       01 ws-with-bonus-percent-report-line.
         05 filler                     pic x(30)
                                 value "PERCENT WITH BONUS    >300,000".
         05 filler                     pic x(7) value spaces.
         05 ws-percent-bonus-rl        pic ZZZ9.99.
         05 ws-percent-pb              pic x(1).
      * Output for percent of people without a bous
       01 ws-without-bonus-percent-report-line.
         05 filler                     pic x(33)
                                value "PERCENT WITHOUT BONUS <=300,000".
         05 filler                     pic x(4) value spaces.
         05 ws-percent-no-bonus-rl     pic ZZZ9.99.
         05 ws-percent-pnb             pic x(1).
      *
       procedure division.
       000-main.
      *
           perform 10-open-file.
           perform 200-print-headings.
           perform 20-read-file.
           perform 100-process-pages
             until ws-eof-flag equals ws-yes.
           perform 510-write-totals.
           perform 520-write-footers.
           perform 600-close-files.
      *
      *Open the input and output file 
       10-open-file.
           open input sales-file.
           open output report-file.
      * Read the input file until the end of file is reached
       20-read-file.
           read sales-file
               at end
                   move "y" to ws-eof-flag.
      *Print the page heading and print outputs until end of file
       100-process-pages.
      *
          perform 210-print-page-heading.
           perform 300-process-lines
           varying ws-line-count from ws-one by ws-one
           until (ws-line-count > ws-lines-per-page
                  OR ws-eof-flag =ws-yes).
      *Print the headings
       200-print-headings.
      *
           write report-line from ws-heading1-name-line
             after advancing ws-one line.
      *Print the page headings each time a new page is created
       210-print-page-heading.
           add ws-one to ws-page-count.
           if (ws-page-count > ws-one) then
               write report-line from spaces
                 after advancing page
               write report-line from ws-heading2-title
                 after advancing ws-two lines
               write report-line from spaces
               write report-line from ws-heading3-headings
               write report-line from ws-heading4-underlines
           else
      *        write report-line from ws-page-heading
               write report-line from spaces
               write report-line from ws-heading2-title
               write report-line from spaces
               write report-line from ws-heading3-headings
               write report-line from ws-heading4-underlines
           end-if.
      * Call paragraphs to calculate totals and to print output
       300-process-lines.
      *
           perform 310-calculate-earned.
           perform 320-calculate-paid.
           perform 500-write-detail-line.
           perform 20-read-file.
      *Calculate the money earned
       310-calculate-earned.
           if (sr-sales <= ws-bonus-sales-cap) then

               divide sr-rate by ws-hundred giving ws-rate-decimal

               multiply sr-sales by ws-rate-decimal giving
                 ws-earned-calc rounded

               add ws-earned-calc to ws-total-earned
           else
               divide sr-rate by ws-hundred giving ws-rate-decimal

               multiply sr-sales by ws-rate-decimal giving
                 ws-earned-calc rounded

               subtract sr-sales from ws-bonus-sales-cap giving
                 ws-earned-amount-over rounded

               multiply ws-earned-amount-over by ws-bonus-percent
                 giving ws-earned-amount-over rounded

               add ws-earned-amount-over to ws-earned-calc giving
                 ws-earned-calc rounded

               add ws-earned-calc to ws-total-earned rounded
           end-if.
      *Calculate the amount of money paid
       320-calculate-paid.
           if (sr-sales > ws-bonus-sales-cap) then

               add ws-one to ws-total-bonus-count
               move ws-bonus-cnst to ws-bonus-output

               if (ws-earned-calc > sr-max) then
                   move sr-max to ws-paid-calc
                   add ws-one to ws-bonus-above-max-count
               
               else
                   move ws-earned-calc to ws-paid-calc
                   add ws-one to ws-paid-equal-earned-count
               end-if
           else
               move ws-no-bonus-cnst to ws-bonus-output
                 add ws-one to ws-no-bonus-count
               if (ws-earned-calc >= sr-min) then
                   
                   move ws-earned-calc to ws-paid-calc
                   add ws-one to ws-paid-equal-earned-count
              else
                add ws-one to ws-no-bonus-less-min-count
              move sr-min to ws-paid-calc
               end-if
           end-if.
           add ws-paid-calc to ws-total-paid.
      *Calculate the percent of people who have paid equal to earned
       330-percent-paid-equal.
           divide ws-paid-equal-earned-count by ws-salespeople-count
             giving ws-paid-equal-calc rounded.

           multiply ws-paid-equal-calc by ws-hundred giving
             ws-paid-equal-calc rounded.
      * calculate the percent of people with 
       330-percent-bonus.
           divide ws-total-bonus-count by ws-salespeople-count
             giving ws-bonus-percent-calc rounded.

           multiply ws-bonus-percent-calc by ws-hundred giving
             ws-bonus-percent-calc.
      * Calculate the percent of people without bonus
       340-percent-no-bonus.
           divide ws-no-bonus-count by ws-salespeople-count
             giving ws-no-bonus-percent-calc rounded.

           multiply ws-no-bonus-percent-calc by ws-hundred giving
             ws-no-bonus-percent-calc.
      *Calculate the amount of people with bonus greater than max
       400-bonus-greater-than.
           move ws-bonus-above-max-count to ws-bonus-above-max-br.
           write report-line from ws-bonus-greater-than-report-line
             after advancing ws-one line.
      *Calculate the amount of people without bonus and is less than min
       410-no-bonus-less-than-min.
           move ws-no-bonus-less-min-count to ws-no-bonues-less-min-br.
           write report-line from
             ws-no-bonus-less-than-min-report-line .
      *Print out salesperson count details    
       420-salesperson-count-print.
      *Print number of salespeople with bonus
           move ws-total-bonus-count to ws-amount-bonus-rl.
           write report-line from ws-num-with-bonus-report-line after 
           advancing ws-one line.
      *Print number of salespeople without bonus
           move ws-no-bonus-count to ws-without-bonus-rl.
           write report-line from ws-num-without-bonus-report-line.
      *Print number of salespeople
           move ws-salespeople-count to ws-num-salespeople-rl.
           write report-line from ws-num-salespeople-report-line.
      *Print the number of people paid equal to earned
       430-num-paid-equal.
           move ws-paid-equal-earned-count to ws-number-paid-equal-rl.
           write report-line from ws-paid-equal-earned-report-line 
           after advancing ws-one line.
      *Print out the percentage stats
       440-percent-stats-print.
      * Print out the percent paid equal
           perform 330-percent-paid-equal.
           move ws-paid-equal-calc to ws-percent-paid-equal-rl.
           move ws-percent-sign to ws-percent-pee.
           write report-line from 
           ws-paid-equal-earned-percent-report-line.
      * Print out the percent of people with bonus
           perform 330-percent-bonus.
           move ws-bonus-percent-calc to ws-percent-bonus-rl.
           move ws-percent-sign to ws-percent-pb.
           write report-line from
             ws-with-bonus-percent-report-line
             after advancing ws-one line.
      *Print out the percent of people without bonus
           perform 340-percent-no-bonus.
           move ws-no-bonus-percent-calc to ws-percent-no-bonus-rl.
           move ws-percent-sign to ws-percent-pnb.
           write report-line from
             ws-without-bonus-percent-report-line
             after advancing ws-one line.
      * Print out the detail line
       500-write-detail-line.
           add ws-one to ws-salespeople-count.
           move spaces to ws-report-detail-line.
           move sr-sman-num to ws-num-dl.
           move sr-name to ws-name-dl.
           move sr-sales to ws-sales-dl.
           move sr-min to ws-min-dl.
           move sr-max to ws-max-dl.
           move sr-rate to ws-rate-dl.
           move ws-percent-sign to ws-percent-dl.
           move ws-earned-calc to ws-earned-dl.
           move ws-paid-calc to ws-paid-dl.
           move ws-bonus-output to ws-bonus-dl.
           write report-line from ws-report-detail-line
             before advancing ws-two lines.
      * Print out the totals for paid and earned
       510-write-totals.
           move ws-total-earned to ws-total-earned-tl.
           move ws-total-paid to ws-total-paid-tl.
           write report-line from ws-total-line.
      * Print out the footers
       520-write-footers.
           perform 400-bonus-greater-than.
           perform 410-no-bonus-less-than-min.
           perform 420-salesperson-count-print.
           perform 430-num-paid-equal.
           perform 440-percent-stats-print.
      *Close the input and output files
       600-close-files.
           close sales-file
             report-file.
           goback.
      *
       end program A3-SalesComm.