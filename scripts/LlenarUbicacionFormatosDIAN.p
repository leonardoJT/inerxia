DEFINE TEMP-TABLE cedulas
    FIELD nit AS CHARACTER
    FIELD departamento AS CHARACTER
    FIELD municipio AS CHARACTER.

INPUT FROM c:\INFO_fodun\cedulas.csv.
REPEAT:
    CREATE cedulas.
    IMPORT DELIMITER ";" cedulas.
END.

FOR EACH cedulas:
    FIND FIRST clientes WHERE INDEX(clientes.nit,cedulas.nit) = 1 NO-LOCK NO-ERROR.
    IF AVAILABLE clientes THEN DO:
        IF clientes.lugar_residencia <> "" AND clientes.lugar_residencia <> ?  THEN DO:
            cedulas.departamento = SUBSTRING(clientes.lugar_residencia,1,2).
            cedulas.municipio = SUBSTRING(clientes.lugar_residencia,3,3).
        END.
        ELSE DO:
            IF clientes.lugar_comercial <> "" AND clientes.lugar_comercial <> ? THEN DO:
                cedulas.departamento = SUBSTRING(clientes.lugar_comercial,1,2).
                cedulas.municipio = SUBSTRING(clientes.lugar_comercial,3,3).
            END.
            ELSE DO:
                CASE clientes.agencia:
                    WHEN 1 THEN DO:
                        cedulas.departamento = "11".
                        cedulas.municipio = "001".
                    END.

                    WHEN 2 THEN DO:
                        cedulas.departamento = "05".
                        cedulas.municipio = "001".
                    END.

                    WHEN 3 THEN DO:
                        cedulas.departamento = "17".
                        cedulas.municipio = "001".
                    END.

                    WHEN 4 THEN DO:
                        cedulas.departamento = "76".
                        cedulas.municipio = "250".
                    END.

                    OTHERWISE DO:
                        cedulas.departamento = "11".
                        cedulas.municipio = "001".
                    END.
                END CASE.
            END.
        END.
    END.
    ELSE DO:
        cedulas.departamento = "11".
        cedulas.municipio = "001".
    END.
END.

OUTPUT TO c:\INFO_fodun\formatos.csv.
FOR EACH cedulas NO-LOCK:
    EXPORT DELIMITER ";" cedulas.
END.
OUTPUT CLOSE.
