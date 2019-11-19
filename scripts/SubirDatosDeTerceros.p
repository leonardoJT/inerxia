DEFINE TEMP-TABLE tt
    FIELD nit AS CHARACTER
    FIELD nombre AS CHARACTER
    FIELD telefono AS CHARACTER
    FIELD celular AS CHARACTER
    FIELD direccion AS CHARACTER
    FIELD email AS CHARACTER.

INPUT FROM D:\leonardo\terceros.csv.
REPEAT :
    CREATE tt.
    IMPORT DELIMITER ";" tt.
END.
INPUT CLOSE.

FOR EACH tt NO-LOCK:
    FIND FIRST clientes WHERE clientes.nit = tt.nit NO-ERROR.
    IF NOT AVAILABLE clientes THEN
        MESSAGE tt.nit
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    ELSE DO:
        clientes.tel_residencia = tt.telefono.
        clientes.celular = tt.celular.
        clientes.DIR_residencia = tt.direccion.
        clientes.email = tt.email.
    END.
END.
