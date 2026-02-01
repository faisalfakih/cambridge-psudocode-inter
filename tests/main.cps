// Complex test program for Cambridge Pseudocode Interpreter
// Tests: arrays, loops, conditionals, functions, procedures, builtins, I/O

// Test 1: Array declaration and manipulation with FOR loops
DECLARE scores : ARRAY[1:5] OF INTEGER
DECLARE names : ARRAY[1:5] OF STRING
DECLARE averageScore : REAL


// Initialize arrays using FOR loop
FOR i <- 1 TO 5
    scores[i] <- i * 10
    names[i] <- "Student"
NEXT i

// Test 2: WHILE loop with array access
DECLARE sum : INTEGER
sum <- 0
DECLARE counter : INTEGER
counter <- 1

WHILE counter <= 5
    sum <- sum + scores[counter]
    counter <- counter + 1
ENDWHILE

averageScore <- sum / 5
OUTPUT "Average score: "
OUTPUT averageScore

// Test 3: Function with string builtins and conditionals
FUNCTION gradeCalculator(score : INTEGER) RETURNS STRING
    DECLARE grade : STRING
    
    IF score >= 90 THEN
        grade <- "A"
    ELSE
        IF score >= 80 THEN
            grade <- "B"
        ELSE
            IF score >= 70 THEN
                grade <- "C"
            ELSE
                IF score >= 60 THEN
                    grade <- "D"
                ELSE
                    grade <- "F"
                ENDIF
            ENDIF
        ENDIF
    ENDIF
    
    RETURN grade
ENDFUNCTION

// Test 4: Procedure with string manipulation builtins
PROCEDURE displayStudentInfo(name : STRING, score : INTEGER)
    DECLARE formattedName : STRING
    DECLARE grade : STRING
    DECLARE nameLength : INTEGER
    
    // Test UCASE builtin
    formattedName <- UCASE(name)
    OUTPUT "Student Name (uppercase): "
    OUTPUT formattedName
    
    // Test LENGTH builtin
    nameLength <- LENGTH(name)
    OUTPUT "Name length: "
    OUTPUT nameLength
    
    // Test function call from procedure
    grade <- gradeCalculator(score)
    OUTPUT "Grade: "
    OUTPUT grade
    
    // Test RIGHT builtin
    DECLARE lastThree : STRING
    lastThree <- RIGHT(name, 3)
    OUTPUT "Last 3 characters: "
    OUTPUT lastThree
ENDPROCEDURE

// Test 5: Complex function using MID and multiple operations
FUNCTION reverseFirstThree(text : STRING) RETURNS STRING
    DECLARE result : STRING
    DECLARE first : STRING
    DECLARE second : STRING
    DECLARE third : STRING
    DECLARE len : INTEGER
    
    len <- LENGTH(text)
    
    IF len >= 3 THEN
        // Test MID builtin (1-indexed)
        first <- MID(text, 1, 1)
        second <- MID(text, 2, 1)
        third <- MID(text, 3, 1)
        result <- third & second & first
    ELSE
        result <- text
    ENDIF
    
    RETURN result
ENDFUNCTION

// Test 6: Nested loops with arrays
OUTPUT "=== Testing nested FOR loops ==="
DECLARE matrix : ARRAY[1:3] OF INTEGER

FOR row <- 1 TO 3
    matrix[row] <- 0
    FOR col <- 1 TO row
        matrix[row] <- matrix[row] + col
    NEXT col
    OUTPUT "Row sum: "
    OUTPUT matrix[row]
NEXT row

// Test 7: WHILE loop with complex condition
OUTPUT "=== Testing WHILE with conditions ==="
DECLARE testValue : INTEGER
testValue <- 100

WHILE testValue > 50 AND testValue < 101
    testValue <- testValue - 10
    OUTPUT testValue
ENDWHILE

// Test 8: Test all string builtins together
OUTPUT "=== Testing string builtins ==="
DECLARE testString : STRING
testString <- "HelloWorld"

OUTPUT "Original: "
OUTPUT testString

DECLARE lower : STRING
lower <- LCASE(testString)
OUTPUT "Lowercase: "
OUTPUT lower

DECLARE upper : STRING
upper <- UCASE(testString)
OUTPUT "Uppercase: "
OUTPUT upper

DECLARE midPart : STRING
midPart <- MID(testString, 3, 5)
OUTPUT "Middle (3,5): "
OUTPUT midPart

DECLARE rightPart : STRING
rightPart <- RIGHT(testString, 4)
OUTPUT "Right 4: "
OUTPUT rightPart

// Test 9: INT builtin with different types
OUTPUT "=== Testing INT builtin ==="
DECLARE realNum : REAL
realNum <- 42.7
DECLARE intResult : INTEGER
intResult <- INT(realNum)
OUTPUT "INT(42.7): "
OUTPUT intResult

DECLARE negReal : REAL
negReal <- -15.9
intResult <- INT(negReal)
OUTPUT "INT(-15.9): "
OUTPUT intResult

// Test 10: RAND builtin
OUTPUT "=== Testing RAND builtin ==="
DECLARE randomNum : REAL
randomNum <- RAND(100)
OUTPUT "Random (0-100): "
OUTPUT randomNum

// Test 11: Procedure calls with different parameters
OUTPUT "=== Testing procedure calls ==="
CALL displayStudentInfo("Alice", 95)
CALL displayStudentInfo("Bob", 78)
CALL displayStudentInfo("Charlie", 85)

// Test 12: Arithmetic operations in expressions
OUTPUT "=== Testing arithmetic ==="
DECLARE a : INTEGER
DECLARE b : INTEGER
DECLARE c : REAL

a <- 10
b <- 3

OUTPUT "10 + 3 = "
OUTPUT a + b

OUTPUT "10 - 3 = "
OUTPUT a - b

OUTPUT "10 * 3 = "
OUTPUT a * b

OUTPUT "10 / 3 = "
c <- a / b
OUTPUT c

OUTPUT "10 DIV 3 = "
OUTPUT a DIV b

OUTPUT "10 MOD 3 = "
OUTPUT a MOD b

OUTPUT "2 ^ 3 = "
OUTPUT 2 ^ 3

// Test 13: String concatenation
OUTPUT "=== Testing string concatenation ==="
DECLARE firstName : STRING
DECLARE lastName : STRING
DECLARE fullName : STRING

firstName <- "John"
lastName <- "Doe"
fullName <- firstName & " " & lastName
OUTPUT fullName

// Test 14: Boolean operations
OUTPUT "=== Testing boolean operations ==="
DECLARE bool1 : BOOLEAN
DECLARE bool2 : BOOLEAN

bool1 <- TRUE
bool2 <- FALSE

IF bool1 AND bool2 <> TRUE THEN
    OUTPUT "AND logic works"
ENDIF

IF bool1 OR bool2 THEN
    OUTPUT "OR logic works"
ENDIF

// Test 15: Comparison operators
OUTPUT "=== Testing comparisons ==="
IF 10 > 5 THEN
    OUTPUT "10 > 5: TRUE"
ENDIF

IF 5 < 10 THEN
    OUTPUT "5 < 10: TRUE"
ENDIF

IF 10 >= 10 THEN
    OUTPUT "10 >= 10: TRUE"
ENDIF

IF 5 <= 10 THEN
    OUTPUT "5 <= 10: TRUE"
ENDIF

IF 10 = 10 THEN
    OUTPUT "10 = 10: TRUE"
ENDIF

IF 10 <> 5 THEN
    OUTPUT "10 <> 5: TRUE"
ENDIF

// Test 16: Complex function with all features
FUNCTION complexCalculation(arr : ARRAY[1:5] OF INTEGER, multiplier : INTEGER) RETURNS INTEGER
    DECLARE total : INTEGER
    DECLARE i : INTEGER
    
    total <- 0
    i <- 1
    
    WHILE i <= 5
        total <- total + (arr[i] * multiplier)
        i <- i + 1
    ENDWHILE
    
    RETURN total
ENDFUNCTION

// This won't work yet since we can't pass arrays to functions
// Just commenting it out for now
// DECLARE result : INTEGER
// result <- complexCalculation(scores, 2)
// OUTPUT "Complex calculation result: "
// OUTPUT result

// Test 17: Nested IF-ELSE
OUTPUT "=== Testing nested conditionals ==="
DECLARE age : INTEGER
age <- 25

IF age < 13 THEN
    OUTPUT "Child"
ELSE
    IF age < 20 THEN
        OUTPUT "Teenager"
    ELSE
        IF age < 65 THEN
            OUTPUT "Adult"
        ELSE
            OUTPUT "Senior"
        ENDIF
    ENDIF
ENDIF

// Test 18: FOR loop with negative increment (if supported)
OUTPUT "=== Testing reverse counting ==="
// Note: Standard Cambridge Pseudocode FOR loops only go forward
// This tests forward counting from 5 to 1 won't work, 
// so we'll do 1 to 5 and display in reverse logic
DECLARE countdown : ARRAY[1:5] OF INTEGER
FOR idx <- 1 TO 5
    countdown[idx] <- 6 - idx
    OUTPUT countdown[idx]
NEXT idx

// Test 19: Function returning result used in expression
OUTPUT "=== Testing function in expression ==="
DECLARE grade1 : STRING
DECLARE grade2 : STRING
grade1 <- gradeCalculator(95)
grade2 <- gradeCalculator(73)
OUTPUT "Student 1 grade: "
OUTPUT grade1
OUTPUT "Student 2 grade: "
OUTPUT grade2

// Test 20: Testing type conversions
OUTPUT "=== Testing type conversions ==="
DECLARE intVal : INTEGER
DECLARE realVal : REAL

intVal <- 42
realVal <- intVal
OUTPUT "Integer to Real: "
OUTPUT realVal

realVal <- 3.14159
intVal <- INT(realVal)
OUTPUT "Real to Integer (INT): "
OUTPUT intVal

OUTPUT "=== All tests completed ==="
