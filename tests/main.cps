DECLARE arr : ARRAY[1 : 10] OF INTEGER
DECLARE res : STRING
FOR Idx <- 1 TO 10
    PRINT "Enter a number"
    INPUT res 
    arr[Idx] <- INT(res)
NEXT Idx

FUNCTION return_first(array: ARRAY[1:10] OF INTEGER) RETURNS INTEGER
    RETURN array[1]
ENDFUNCTION

PRINT return_first(arr)
