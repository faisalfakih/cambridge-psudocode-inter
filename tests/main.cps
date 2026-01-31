// Password Strength Checker and Generator
// Uses all implemented features

DECLARE score : INTEGER
DECLARE password : STRING
DECLARE length : INTEGER
DECLARE hasUpper : BOOLEAN
DECLARE hasLower : BOOLEAN
DECLARE strength : STRING
DECLARE choice : STRING
DECLARE generated : STRING
DECLARE i : INTEGER
DECLARE charType : INTEGER
DECLARE temp : STRING

FUNCTION CalculateScore(pass : STRING) RETURNS INTEGER
    DECLARE localScore : INTEGER
    DECLARE len : INTEGER
    
    localScore <- 0
    len <- LENGTH(pass)
    
    // Length scoring
    IF len >= 8 THEN
        localScore <- localScore + 25
    ELSE
        localScore <- localScore + (len * 3)
    ENDIF
    
    // Check for uppercase
    IF UCASE(pass) <> pass THEN
        localScore <- localScore + 20
    ENDIF
    
    // Check for lowercase
    IF LCASE(pass) = pass THEN
        localScore <- localScore + 10
    ELSE
        localScore <- localScore + 20
    ENDIF
    
    // Bonus for very long passwords
    IF len > 12 THEN
        localScore <- localScore + 15
    ENDIF
    
    RETURN localScore
ENDFUNCTION

FUNCTION GetStrength(points : INTEGER) RETURNS STRING
    DECLARE result : STRING
    
    IF points < 30 THEN
        result <- "WEAK"
    ELSE
        IF points < 60 THEN
            result <- "MODERATE"
        ELSE
            result <- "STRONG"
        ENDIF
    ENDIF
    
    RETURN result
ENDFUNCTION

PROCEDURE GeneratePassword(len : INTEGER)
    DECLARE j : INTEGER
    DECLARE randNum : INTEGER
    DECLARE newPass : STRING
    DECLARE charset : STRING
    DECLARE randPos : INTEGER
    
    newPass <- ""
    
    FOR j <- 1 TO len
        randNum <- INT(RAND(3))
        
        IF randNum = INT(0) THEN
            charset <- "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        ELSE
            IF randNum = 1 THEN
                charset <- "abcdefghijklmnopqrstuvwxyz"
            ELSE
                charset <- "0123456789"
            ENDIF
        ENDIF
        
        // Calculate random position as INTEGER
        randPos <- INT(RAND(LENGTH(charset))) + 1
        temp <- MID(charset, randPos, 1)
        newPass <- newPass & temp
    NEXT j
    
    OUTPUT "Generated Password: " & newPass
    OUTPUT "Length: " & LENGTH(newPass)
ENDPROCEDURE

FUNCTION ContainsSubstring(text : STRING, search : STRING) RETURNS BOOLEAN
    DECLARE textLen : INTEGER
    DECLARE searchLen : INTEGER
    DECLARE pos : INTEGER
    DECLARE found : BOOLEAN
    DECLARE substr : STRING
    
    textLen <- LENGTH(text)
    searchLen <- LENGTH(search)
    found <- FALSE
    
    IF searchLen > textLen THEN
        RETURN FALSE
    ENDIF
    
    FOR pos <- 1 TO (textLen - searchLen) + 1
        substr <- MID(text, pos, searchLen)
        IF substr = search THEN
            found <- TRUE
        ENDIF
    NEXT pos
    
    RETURN found
ENDFUNCTION

// Main Program
OUTPUT "====================================="
OUTPUT "Password Strength Checker & Generator"
OUTPUT "====================================="
OUTPUT ""

DECLARE running : BOOLEAN
running <- TRUE

WHILE running = TRUE
    OUTPUT "Choose an option:"
    OUTPUT "1. Check password strength"
    OUTPUT "2. Generate random password"
    OUTPUT "3. Exit"
    INPUT choice
    
    IF choice = "1" THEN
        OUTPUT "Enter password to check:"
        INPUT password
        
        length <- LENGTH(password)
        OUTPUT ""
        OUTPUT "Password Analysis:"
        OUTPUT "==================="
        OUTPUT "Length: " & length
        
        // Check if all uppercase
        IF UCASE(password) = password THEN
            OUTPUT "Contains uppercase: YES (all)"
            hasUpper <- TRUE
        ELSE
            hasUpper <- FALSE
            OUTPUT "Contains uppercase: MIXED"
        ENDIF
        
        // Check if all lowercase
        IF LCASE(password) = password THEN
            OUTPUT "Contains lowercase: YES (all)"
            hasLower <- TRUE
        ELSE
            hasLower <- FALSE
            OUTPUT "Contains lowercase: MIXED"
        ENDIF
        
        // Extract first and last characters
        OUTPUT "First character: " & MID(password, 1, 1)
        OUTPUT "Last character: " & RIGHT(password, 1)
        
        // Calculate score
        score <- CalculateScore(password)
        strength <- GetStrength(score)
        
        OUTPUT ""
        OUTPUT "Score: " & score & "/100"
        OUTPUT "Strength: " & strength
        
        // Recommendations
        IF score < 60 THEN
            OUTPUT ""
            OUTPUT "Recommendations:"
            IF length < 8 THEN
                OUTPUT "- Use at least 8 characters"
            ENDIF
            IF hasUpper = TRUE THEN
                OUTPUT "- Add lowercase letters"
            ENDIF
            IF hasLower = TRUE THEN
                OUTPUT "- Add uppercase letters"
            ENDIF
        ENDIF
        
        // Check for common weak patterns
        IF ContainsSubstring(LCASE(password), "password") = TRUE THEN
            OUTPUT "WARNING: Contains word 'password'!"
        ENDIF
        
        IF ContainsSubstring(password, "123") = TRUE THEN
            OUTPUT "WARNING: Contains sequential numbers!"
        ENDIF
        
    ELSE
        IF choice = "2" THEN
            OUTPUT "Enter desired password length (8-20):"
            INPUT temp
            length <- INT(temp)
            
            IF length < 8 THEN
                length <- 8
                OUTPUT "Minimum length is 8. Using 8."
            ENDIF
            
            IF length > 20 THEN
                length <- 20
                OUTPUT "Maximum length is 20. Using 20."
            ENDIF
            
            OUTPUT ""
            CALL GeneratePassword(length)
            
        ELSE
            IF choice = "3" THEN
                running <- FALSE
                OUTPUT "Goodbye!"
            ELSE
                OUTPUT "Invalid choice. Please try again."
            ENDIF
        ENDIF
    ENDIF
    
    OUTPUT ""
    OUTPUT "====================================="
    OUTPUT ""
ENDWHILE
