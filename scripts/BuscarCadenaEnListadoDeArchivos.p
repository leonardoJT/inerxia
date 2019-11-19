DEFINE TEMP-TABLE progs
    FIELD nombre AS CHARACTER
    FIELD propiedades AS CHARACTER.

DEFINE VAR linea AS CHARACTER FORMAT "X(120)".

INPUT FROM c:\INFO_fodun\progs.csv.
REPEAT:
    CREATE progs.
    IMPORT DELIMITER ";" progs.
END.
INPUT CLOSE.

OUTPUT TO c:\INFO_Fodun\DisableTriggers.txt.
FOR EACH progs:
    INPUT FROM VALUE(progs.nombre).
    REPEAT:
        IMPORT UNFORMATTED linea.
        IF INDEX(linea,"wmayorizacion") > 0 THEN
            DISPLAY SUBSTRING(progs.nombre,28) FORMAT "X(50)" linea FORMAT "X(120)" WITH WIDTH 200.
    END.
END.
