---
layout: default
title: Comparing CSV Libraries
---
- [Review of Lisp CSV Libraries (2016)](#83da1e19-c1e2-481d-ae56-07ab83dc7ba6)
- [Need to change a special variable rather than just passing a non-default parameter.](#7d0f0cb0-6b9a-4f39-9d93-a01bbebd5790)
- [File was misread](#bb847660-6d81-4fd6-97ab-ca92f4c5c27e)
  - [Benchmark Results on 100 reads file size 155,678](#fba5f61c-b526-4a96-a162-6550e5f4cf29)
  - [Benchmark Results on 100 reads file size 1,538,142](#2a442a4f-0023-4be8-bbee-20ccc1cd8e06)
  - [Benchmark Results on 10 reads file size 3,978,966](#e3be7388-ea8e-4db2-958f-4f445ec90a5c)
  - [Benchmark Results on 1 read file size 181,132,541](#e29d99de-a7b6-496d-b9a0-fa968b772c03)
- [Function Comparison](#bc3c706c-0549-4f3f-8707-4e310c95e6c0)
- [Fare-csv](#9fa70efc-eaf1-4e82-8e79-1d0ac8b71a42)
- [Cl-csv](#a42c501d-7c50-4c5c-bdac-888e3aa6a1ca)
- [Read-csv](#7327dc8d-5266-4445-a6b0-de9ac996f5c5)
- [csv-parser](#19dff4d4-c137-452a-b076-07aa0ddbd02d)
- [cl-simple-table](#e84c7a68-8c7a-4981-a2f5-9b2a8329e7c8)
- [Profiling function](#1f8c1b19-cd01-4d62-979a-99de88f0ff83)


<a id="83da1e19-c1e2-481d-ae56-07ab83dc7ba6"></a>

# Review of Lisp CSV Libraries (2016)

reading csv benchmarks Just a quick set of benchmarks on lisp libraries reading csv files. Note that csv is an underspecified file type. See some specifications at these locations: <http://www.creativyst.com/Doc/Articles/CSV/CSV01.htm>, <http://www.rfc-editor.org/rfc/rfc4180.txt> and <http://edoceo.com/utilitas/csv-file-format>.

Libraries tested: csv-parser, cl-csv, read-csv, fare-csv and cl-simple-table. Libraries

| Library         | Author                              | Weblink                                      | License | Last Update |
|--------------- |----------------------------------- |-------------------------------------------- |------- |----------- |
| csv-parser      | Alain Picard                        | <https://github.com/sharplispers/csv-parser> | LLGPL   | 20140713    |
| cl-csv          | Russ Tyndall Nathan Bird Ryan Davis | <https://github.com/AccelerationNet/cl-csv>  | BSD     | 20150608    |
| read-csv        | Warren Wilkinson                    | <https://github.com/deadtrickster/read-csv>  | LGPL2   | 20151218    |
| fare-csv        | Francois-Rene Rideau                |                                              | BSD     | 20151218    |
| cl-simple-table | Francisco Soto                      | <https://github.com/ebobby/cl-simple-table>  | BSD     | 20130312    |

Quick Summary: cl-simple-table:read-csv returns a vector of vectors with each element in the row as a string. If the item was a string in the csv file, the item will be enclosed in escaped strings.The rest of the libraries return a list of lists, with each list member as a string.

Of the four libraries, cl-simple-table is by far the fastest. read-csv comes in second and, cl-csv coming in third. Cl-simple-table also wins for memory footprint. Cl-csv comes in second for memory footprint and read-csv coming in third. cl-csv wins for error checking. Neither cl-simple-table nor read-csv triggered error messages on the sample bad data. Fare-csv is probably the most flexible when it comes to specifying data requirements such as types of line feeds, allowing or disallowing binary data, etc.

Using any of these libraries will require that you parse the data to convert it to the data type you need. You will likely also need utility functions to rotate the list, convert to array, etc. They can all use separators other than a comma, although csv-parser is a little more complicated in that regard. Functions|

| Functionality      | Comment          | csv-parser     | cl-csv        | read-csv  | fare-csv        | cl-simple-table |
|------------------ |---------------- |-------------- |------------- |--------- |--------------- |--------------- |
| read a row         |                  | read-csv-line  | read-csv-row  | read-csv  | read-csv-line   |                 |
| read a file        |                  | do-csv-file    | read-csv      | parse-csv | read-csv-file   | read-csv        |
| read from a stream |                  |                |               |           | read-csv-stream |                 |
| map by line        |                  | map-csv-file   |               |           |                 |                 |
| write a csv line   |                  | write-csv-line | write-csv-row |           | write-csv-line  |                 |
| write a csv file   |                  |                | write-csv     |           | write-csv-lines |                 |
| Other Separators   | (Besides commas) | \*             | YES           | YES       | YES             | tab             |


<a id="7d0f0cb0-6b9a-4f39-9d93-a01bbebd5790"></a>

# Need to change a special variable rather than just passing a non-default parameter.

Error Checking sample file with missing quotes, uneven number of fields, partial quotes "Column 1 Row 1",49,"52",24.3,"24.3" "Column 1, Row 2",49,"52",24.3,"24.3",Geprge "dfs3s ,34.2,"twenty2a",,nil,

| Library                    | Error Message                                                                                                         | Condition/Restarts        |
|-------------------------- |--------------------------------------------------------------------------------------------------------------------- |------------------------- |
| csv-parser                 | Got unexpected non-blank char after end of a quoted field                                                             | Abort                     |
| cl-csv                     | We finished reading a quoted value and got more characters before a separator or EOL 14 "dfs3s ,34.2,"twenty2a",,nil, | 0 - skip reading this row |
| 1 - supply a different row |                                                                                                                       |                           |
| 2 - Abort                  |                                                                                                                       |                           |
| read-csv                   | \*                                                                                                                    |                           |
| fare-csv                   | end of field expected                                                                                                 | Abort                     |
| cl-simple-table            | \*                                                                                                                    |                           |


<a id="bb847660-6d81-4fd6-97ab-ca92f4c5c27e"></a>

# File was misread

I note that throwing a file with malformed UTF-8 characters at the libraries triggered stream-decoding errors in sbcl before the data even got to the csv libraries. Time Results Using sbcl version 1.3.3 on a linux box.

Sample function calls

```lisp
(defun csv-parser-read (file times)
  (format t "CSV-parser-read ~a ~a" file times)
  (time (dotimes (i times)
          (let ((lst nil))
            (csv-parser:do-csv-file ((filds num-filds) file)
              (add-row-to-csv-list filds lst))))))

(defun cl-csv-read (file times)
  (format t "CL-CSV-read ~a ~a" file times)
  (time (dotimes (i times)
          (with-open-file (s file)
            (cl-csv:read-csv s )))))

(defun read-csv-read (file times)
  (format t "read-csv-read ~a ~a" file times)
  (time (dotimes (i times)
          (with-open-file (s file) (read-csv:parse-csv s)))))

(defun fare-csv-read (file times)
  (format t "fare-csv-read ~a ~a" file times)
  (time (dotimes (i times)
          (with-open-file (s file) (fare-csv:read-csv-stream s)))))

(defun cl-simple-table-read (file times)
  (format t "cl-simple-table-read ~a ~a" file times)
  (time (dotimes (i times)
          (cl-simple-table:read-csv file))))

```

File Size 500 with 11 Fields 50000 reps

| Item             | CL-CSV        | CL-SIMPLE-TABLE | CSV-PARSER     | FARE-CSV       | READ-CSV      |
|---------------- |------------- |--------------- |-------------- |-------------- |------------- |
| Run Time         | 2.646667      | 1.356666        | 3.753333       | 3.74.9999      | 2.16667       |
| Processor Cycles | 8,726,461,239 | 4,470,851,802   | 12,363,650,079 | 12,358,084,257 | 7,137,992,619 |
| Bytes Consed     | 940,822,032   | 820,017,568     | 1,546,363,984  | 1,327,218,896  | 978,418,576   |

File Size 15,000 with 5 Fields 2000 reps

| Item             | CL-CSV        | CL-SIMPLE-TABLE | CSV-PARSER    | FARE-CSV      | READ-CSV      |
|---------------- |------------- |--------------- |------------- |------------- |------------- |
| Run Time         | 2.026667      | 0.69000         | 2.650000      | 1.963333      | 1.51000       |
| Processor Cycles | 6,674,924,316 | 2,263,618,308   | 8,729,576,355 | 6,469,628,430 | 4,975,928,232 |
| Bytes Consed     | 590,141,168   | 431,871,024     | 951,366,720   | 732,123,552   | 603,958,992   |

File Size 157,000 with 82 Fields 250 Reps

| Item                                       | CL-CSV         | CL-SIMPLE-TABLE | CSV-PARSER     | FARE-CSV       | READ-CSV       |
|------------------------------------------ |-------------- |--------------- |-------------- |-------------- |-------------- |
| Run Time                                   | 2.426666       | 0.76000         | 4.186667       | 5.54666        | 1.91333        |
| Processor Cycles                           | 7,992,864,432  | 2,501,030,052   | 13,788,07,848  | 18,280,535,433 | 6,301,984,953  |
| Bytes Consed                               | 577,792,848    | 593,286,848     | 1,472,604,944  | 1,163,853,088  | 823,448,208    |
| File Size 13,260,000 with 8 Fields 10 Reps |                |                 |                |                |                |
| Item                                       | CL-CSV         | CL-SIMPLE-TABLE | CSV-PARSER     | FARE-CSV       | READ-CSV       |
| Run Time                                   | 13.203332      | 5.436667        | 16.48666       | 25.109998      | 10.689998      |
| Processor Cycles                           | 43,501,278,255 | 17,902,803,282  | 54,295,431,543 | 82,735,374,393 | 35,211,651,705 |
| Bytes Consed                               | 3,288,240,864  | 2,378,835,872   | 6,667,167,456  | 6,589,610,736  | 3,280,175,600  |

Benchmark Results on 1 reads file size 181,132,541 Oops. No library managed to read the file. (Complete county file from <https://www.census.gov/econ/cbp/download/>). No error messages were thrown.

First step, just read a test csv file into an in memory list file size is 155678

```lisp
(fare-csv:read-csv-file "/home/sabra/Downloads/all-populations.csv")
(with-open-file (s "/home/sabra/Downloads/all-populations.csv") (parse-csv s))
(cl-csv:read-csv #P "/home/sabra/Downloads/all-populations.csv")
```


<a id="fba5f61c-b526-4a96-a162-6550e5f4cf29"></a>

## Benchmark Results on 100 reads file size 155,678

"Fare-csv" Evaluation took: 2.487 seconds of real time 2.490000 seconds of total run time (2.490000 user, 0.000000 system) [ Run times consist of 0.090 seconds GC time, and 2.400 seconds non-GC time. ] 100.12% CPU 8,203,932,357 processor cycles 505,833,248 bytes consed

"cl-csv" Evaluation took: 1.095 seconds of real time 1.096667 seconds of total run time (1.096667 user, 0.000000 system) [ Run times consist of 0.020 seconds GC time, and 1.077 seconds non-GC time. ] 100.18% CPU 3,611,207,784 processor cycles 255,959,152 bytes consed

"read-csv" Evaluation took: 0.884 seconds of real time 0.886667 seconds of total run time (0.886667 user, 0.000000 system) [ Run times consist of 0.033 seconds GC time, and 0.854 seconds non-GC time. ] 100.34% CPU 2,915,717,155 processor cycles 359,682,688 bytes consed


<a id="2a442a4f-0023-4be8-bbee-20ccc1cd8e06"></a>

## Benchmark Results on 100 reads file size 1,538,142

"cl-csv" Evaluation took: 12.282 seconds of real time 12.276665 seconds of total run time (12.053332 user, 0.223333 system) [ Run times consist of 0.841 seconds GC time, and 11.436 seconds non-GC time. ] 99.96% CPU 40,508,381,823 processor cycles 3,655,705,440 bytes consed

"Fare-csv" Evaluation took: 11.632 seconds of real time 11.653333 seconds of total run time (11.456666 user, 0.196667 system) [ Run times consist of 1.259 seconds GC time, and 10.395 seconds non-GC time. ] 100.18% CPU 38,364,695,982 processor cycles 3,466,825,776 bytes consed

"read-csv" Evaluation took: 7.758 seconds of real time 7.773332 seconds of total run time (7.569999 user, 0.203333 system) [ Run times consist of 0.707 seconds GC time, and 7.067 seconds non-GC time. ] 100.19% CPU 25,587,097,595 processor cycles 3,120,795,712 bytes consed


<a id="e3be7388-ea8e-4db2-958f-4f445ec90a5c"></a>

## Benchmark Results on 10 reads file size 3,978,966

"Read-csv" Evaluation took: 3.159 seconds of real time 3.163332 seconds of total run time (3.086666 user, 0.076666 system) [ Run times consist of 0.676 seconds GC time, and 2.488 seconds non-GC time. ] 100.13% CPU 10,417,108,755 processor cycles 874,065,968 bytes consed

"Fare-csv" Evaluation took: 6.201 seconds of real time 6.210000 seconds of total run time (6.040000 user, 0.170000 system) [ Run times consist of 1.245 seconds GC time, and 4.965 seconds non-GC time. ] 100.15% CPU 20,451,931,647 processor cycles 1,482,367,120 bytes consed

"Cl-csv" Evaluation took: 3.440 seconds of real time 3.446666 seconds of total run time (3.369999 user, 0.076667 system) [ Run times consist of 0.742 seconds GC time, and 2.705 seconds non-GC time. ] 100.20% CPU 11,344,296,525 processor cycles 823,028,080 bytes consed


<a id="e29d99de-a7b6-496d-b9a0-fa968b772c03"></a>

## Benchmark Results on 1 read file size 181,132,541


<a id="bc3c706c-0549-4f3f-8707-4e310c95e6c0"></a>

# Function Comparison


<a id="9fa70efc-eaf1-4e82-8e79-1d0ac8b71a42"></a>

# Fare-csv


<a id="a42c501d-7c50-4c5c-bdac-888e3aa6a1ca"></a>

# Cl-csv


<a id="7327dc8d-5266-4445-a6b0-de9ac996f5c5"></a>

# Read-csv

locked on bad-file (excess columns)


<a id="19dff4d4-c137-452a-b076-07aa0ddbd02d"></a>

# csv-parser


<a id="e84c7a68-8c7a-4981-a2f5-9b2a8329e7c8"></a>

# cl-simple-table

locked on missing file rather than throw an error message


<a id="1f8c1b19-cd01-4d62-979a-99de88f0ff83"></a>

# Profiling function

(defun simple-profile (function1 first-label function2 second-label times) "Simple profile. See sample usage in internal comments" (print first-label) (time (dotimes (i times) (funcall function1))) (print second-label) (time (dotimes (i times) (funcall function2))))

(ql:quickload :fare-csv) (ql:quickload :read-csv) (ql:quickload :cl-csv)

(defun test-fare-csv () (fare-csv:read-csv-file "/home/wol/Downloads/geography/cbp13us.txt"))

(defun test-read-csv () (with-open-file (s "/home/wol/Downloads/geography/cbp13us.txt") (read-csv:parse-csv s)))

(defun test-cl-csv () (cl-csv:read-csv #P "/home/wol/Downloads/geography/cbp13us.txt"))

Lib CSV-PARSER File Size 500 Evaluation took: 0.102 seconds of real time 0.103333 seconds of total run time (0.093333 user, 0.010000 system) 100.98% CPU 336,440,200 processor cycles 15,538,656 bytes consed

Lib CL-CSV File Size 500 Evaluation took: 0.047 seconds of real time 0.046667 seconds of total run time (0.046667 user, 0.000000 system) 100.00% CPU 155,440,827 processor cycles 9,458,192 bytes consed

Lib READ-CSV File Size 500 Evaluation took: 0.031 seconds of real time 0.033333 seconds of total run time (0.033333 user, 0.000000 system) 106.45% CPU 103,312,431 processor cycles 9,869,376 bytes consed

Lib FARE-CSV File Size 500 Evaluation took: 0.039 seconds of real time 0.036667 seconds of total run time (0.036667 user, 0.000000 system) 94.87% CPU 127,631,997 processor cycles 13,392,704 bytes consed

Lib CL-SIMPLE-TABLE File Size 500 Evaluation took: 0.022 seconds of real time 0.023333 seconds of total run time (0.020000 user, 0.003333 system) [ Run times consist of 0.007 seconds GC time, and 0.017 seconds non-GC time. ] 104.55% CPU 72,172,686 processor cycles 8,262,336 bytes consed

Lib CSV-PARSER File Size 15000 Evaluation took: 20.289 seconds of real time 20.329999 seconds of total run time (20.196665 user, 0.133334 system) [ Run times consist of 0.929 seconds GC time, and 19.401 seconds non-GC time. ] 100.20% CPU 66,915,772,500 processor cycles 7,137,128,608 bytes consed

Lib CL-CSV File Size 15000 Evaluation took: 15.960 seconds of real time 15.986665 seconds of total run time (15.886665 user, 0.100000 system) [ Run times consist of 0.615 seconds GC time, and 15.372 seconds non-GC time. ] 100.17% CPU 52,636,019,034 processor cycles 4,428,002,384 bytes consed

Lib READ-CSV File Size 15000 Evaluation took: 11.428 seconds of real time 11.453332 seconds of total run time (11.336666 user, 0.116666 system) [ Run times consist of 0.613 seconds GC time, and 10.841 seconds non-GC time. ] 100.22% CPU 37,690,585,596 processor cycles 4,531,694,496 bytes consed

Lib FARE-CSV File Size 15000 Evaluation took: 15.281 seconds of real time 15.313332 seconds of total run time (15.173332 user, 0.140000 system) [ Run times consist of 0.727 seconds GC time, and 14.587 seconds non-GC time. ] 100.21% CPU 50,397,391,740 processor cycles 5,492,886,672 bytes consed

Lib CL-SIMPLE-TABLE File Size 15000 Evaluation took: 5.349 seconds of real time 5.369999 seconds of total run time (5.273332 user, 0.096667 system) [ Run times consist of 0.429 seconds GC time, and 4.941 seconds non-GC time. ] 100.39% CPU 17,642,284,263 processor cycles 3,241,206,496 bytes consed
