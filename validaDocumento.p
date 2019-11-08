DEFINE INPUT PARAMETER numDoc AS CHARACTER.
DEFINE OUTPUT PARAMETER flag AS LOGICAL.
DEFINE OUTPUT PARAMETER caracterInvalido AS CHARACTER.

DEFINE VAR cont AS INTEGER.

DO cont = 1 TO LENGTH(numDoc):
    IF SUBSTRING(numDoc,1,3) <> "BOG" AND
       SUBSTRING(numDoc,1,3) <> "MED" AND
       SUBSTRING(numDoc,1,3) <> "MAN" AND
       SUBSTRING(numDoc,1,3) <> "PAL" AND
       SUBSTRING(numDoc,cont,1) <> "0" AND
       SUBSTRING(numDoc,cont,1) <> "1" AND
       SUBSTRING(numDoc,cont,1) <> "2" AND
       SUBSTRING(numDoc,cont,1) <> "3" AND
       SUBSTRING(numDoc,cont,1) <> "4" AND
       SUBSTRING(numDoc,cont,1) <> "5" AND
       SUBSTRING(numDoc,cont,1) <> "6" AND
       SUBSTRING(numDoc,cont,1) <> "7" AND
       SUBSTRING(numDoc,cont,1) <> "8" AND
       SUBSTRING(numDoc,cont,1) <> "9" THEN DO:
        flag = TRUE.
        caracterInvalido = SUBSTRING(numDoc,cont,1).

        RETURN.
    END.
END.
