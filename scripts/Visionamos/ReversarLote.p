DEFINE TEMP-TABLE tt
    FIELD indice AS CHARACTER.

INPUT FROM D:\Leonardo\reversos.txt.
REPEAT :
    CREATE tt.
    IMPORT tt.
END.

FOR EACH tt NO-LOCK:
    FIND FIRST aplicarVisionamos WHERE aplicarVisionamos.indx = tt.indice NO-ERROR.
    IF AVAILABLE aplicarVisionamos THEN
        ASSIGN aplicarVisionamos.grupo_transaccional = "0400"
               estado = 1
               terminal_ubicacion = "Rev-" + TERMINAL_ubicacion.
END.
