DEFINE TEMP-TABLE ttfac
    FIELD agencia AS INTEGER
    FIELD cedula AS CHARACTER
    FIELD facultad AS CHARACTER
    FIELD departamento AS CHARACTER
    FIELD email AS CHARACTER
    FIELD codigo AS CHARACTER.

INPUT FROM c:\INFO_Fodun\Leonardo\FacultadesPalmira.csv.
REPEAT :
    CREATE ttfac.
    IMPORT DELIMITER ";" ttfac NO-ERROR.

    IF ERROR-STATUS:ERROR THEN
        DELETE ttFac.
    ELSE DO:
        FIND FIRST facultades WHERE facultades.agencia = ttfac.agencia
                                AND facultades.tipo = "F"
                                AND INDEX(facultades.nombre,ttfac.facultad) = 1 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE facultades THEN DO:
            MESSAGE "Facultad" ttfac.cedula ttfac.facultad
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            RETURN.
        END.
        ELSE DO:
            FIND FIRST facultades WHERE facultades.agencia = ttfac.agencia
                                    AND facultades.tipo = "D"
                                    AND INDEX(facultades.nombre,ttfac.departamento) = 1 NO-LOCK NO-ERROR.
            IF NOT AVAILABLE facultades THEN DO:
                MESSAGE "Departamento" ttfac.cedula ttfac.departamento
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.

                RETURN.
            END.
            ELSE
                ttfac.codigo = facultades.codigo.
        END.
    END.
END.

FOR EACH ttfac NO-LOCK:
    FIND FIRST clientes WHERE clientes.nit = ttfac.cedula NO-ERROR.
    IF NOT AVAILABLE clientes THEN DO:
        MESSAGE ttfac.cedula
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN.
    END.

    clientes.facultad = INTEGER(SUBSTRING(ttfac.codigo,1,2)).
    clientes.departamento = INTEGER(SUBSTRING(ttfac.codigo,3)).
    /*clientes.email = ttfac.email.*/
END.
