// ========== BASIC CASE ==========
DECLARE grade : STRING
grade <- "A"
CASE OF grade
    "A" : OUTPUT "Excellent"
    "B" : OUTPUT "Good"
    "C" : OUTPUT "Average"
    OTHERWISE : OUTPUT "Fail"
ENDCASE

// ========== CASE WITH MULTIPLE STATEMENTS PER BRANCH ==========
DECLARE x : INTEGER
DECLARE y : INTEGER
DECLARE z : INTEGER
x <- 1
CASE OF x
    1 : OUTPUT "One"
        y <- 10
        z <- 20
    2 : OUTPUT "Two"
        y <- 30
ENDCASE

// ========== CASE WITH RANGES ==========
DECLARE score : INTEGER
score <- 75
CASE OF score
    0 TO 49 : OUTPUT "Fail"
    50 TO 69 : OUTPUT "Pass"
    70 TO 89 : OUTPUT "Merit"
    90 TO 100 : OUTPUT "Distinction"
ENDCASE

// ========== CASE WITH MIXED SINGLE VALUES AND RANGES ==========
DECLARE mark : INTEGER
mark <- 51
CASE OF mark
    0 : OUTPUT "Zero"
    1 TO 50 : OUTPUT "Low"
    51 : OUTPUT "Exactly 51"
    52 TO 100 : OUTPUT "High"
    OTHERWISE : OUTPUT "Invalid"
ENDCASE

// ========== CASE WITH EXPRESSIONS ==========
x <- 5
y <- 5
CASE OF x + y
    10 : OUTPUT "Sum is 10"
    20 : OUTPUT "Sum is 20"
    30 TO 50 : OUTPUT "Sum between 30 and 50"
ENDCASE

// ========== NESTED CASE STATEMENTS ==========
DECLARE type : INTEGER
DECLARE subtype : STRING
type <- 1
subtype <- "A"
CASE OF type
    1 : CASE OF subtype
            "A" : OUTPUT "Type 1A"
            "B" : OUTPUT "Type 1B"
        ENDCASE
    2 : OUTPUT "Type 2"
ENDCASE

// ========== CASE WITH STRINGS ==========
DECLARE day : STRING
day <- "Monday"
CASE OF day
    "Monday" : OUTPUT "Start of week"
    "Friday" : OUTPUT "End of week"
    OTHERWISE : OUTPUT "Midweek"
ENDCASE

// ========== CASE WITHOUT OTHERWISE ==========
DECLARE num : INTEGER
num <- 2
CASE OF num
    1 : OUTPUT "One"
    2 : OUTPUT "Two"
    3 : OUTPUT "Three"
ENDCASE

// ========== CASE WITH BOOLEAN EXPRESSIONS ==========
DECLARE flag : BOOLEAN
flag <- TRUE
CASE OF flag
    TRUE : OUTPUT "True case"
    FALSE : OUTPUT "False case"
ENDCASE

// ========== CASE WITH COMPLEX STATEMENTS IN BRANCHES ==========
DECLARE operation : INTEGER
operation <- 1
x <- 5
CASE OF operation
    1 : IF x > 0 THEN
            OUTPUT x
        ENDIF
    2 : FOR i <- 1 TO 10
            OUTPUT i
        NEXT i
    3 : WHILE x < 100
            x <- x + 1
        ENDWHILE
ENDCASE

// ========== MULTIPLE CONSECUTIVE CASES ==========
DECLARE value : INTEGER
value <- 5
CASE OF value
    1 : OUTPUT "First"
    2 : OUTPUT "Second"
    3 : OUTPUT "Third"
    4 : OUTPUT "Fourth"
    5 : OUTPUT "Fifth"
    6 : OUTPUT "Sixth"
    7 : OUTPUT "Seventh"
    8 : OUTPUT "Eighth"
    9 : OUTPUT "Ninth"
    10 : OUTPUT "Tenth"
ENDCASE

// ========== CASE WITH SINGLE STATEMENT ==========
x <- 1
CASE OF x
    1 : y <- 10
    2 : y <- 20
ENDCASE

// ========== CASE WITH ARRAY ACCESS ==========
DECLARE arr : ARRAY[1:10] OF INTEGER
DECLARE i : INTEGER
i <- 1
arr[1] <- 3
CASE OF arr[i]
    1 : OUTPUT "Array value is 1"
    2 TO 5 : OUTPUT "Array value between 2 and 5"
    OTHERWISE : OUTPUT "Other value"
ENDCASE

// ========== CASE WITH REAL NUMBERS ==========
DECLARE temperature : REAL
temperature <- 15.5
CASE OF temperature
    0.0 TO 10.0 : OUTPUT "Cold"
    10.1 TO 25.0 : OUTPUT "Mild"
    25.1 TO 40.0 : OUTPUT "Hot"
ENDCASE

// ========== CASE WITH DECLARATIONS IN BRANCHES ==========
DECLARE mode : INTEGER
mode <- 1
CASE OF mode
    1 : DECLARE testX : INTEGER
        testX <- 10
    2 : DECLARE testY : REAL
        testY <- 20.5
ENDCASE

// ========== MINIMAL CASE ==========
x <- 1
CASE OF x
    1 : OUTPUT "One"
ENDCASE
