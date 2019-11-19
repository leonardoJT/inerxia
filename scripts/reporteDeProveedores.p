OUTPUT TO d:\leonardo\proveedores.csv.
EXPORT DELIMITER ";"
    "AGENCIA"
    "CÉDULA"
    "NOMBRE COMPLETO".

FOR EACH anexos WHERE ano = 2014
                  AND (SUBSTRING(cuenta,1,1) = "5" OR
                       SUBSTRING(cuenta,1,1) = "6" OR
                       SUBSTRING(cuenta,1,2) = "17")
                  AND (anexos.db[1] > 0 OR anexos.db[2] > 0 OR
                       anexos.db[3] > 0 OR anexos.db[4] > 0 OR
                       anexos.db[5] > 0 OR anexos.db[6] > 0 OR
                       anexos.db[7] > 0 OR anexos.db[8] > 0 OR
                       anexos.db[9] > 0 OR anexos.db[10] > 0 OR
                       anexos.db[11] > 0 OR anexos.db[12] > 0 OR
                       anexos.cr[1] > 0 OR anexos.cr[2] > 0 OR
                       anexos.cr[3] > 0 OR anexos.cr[4] > 0 OR
                       anexos.cr[5] > 0 OR anexos.cr[6] > 0 OR
                       anexos.cr[7] > 0 OR anexos.cr[8] > 0 OR
                       anexos.cr[9] > 0 OR anexos.cr[10] > 0 OR
                       anexos.cr[11] > 0 OR anexos.cr[12] > 0) NO-LOCK BREAK BY agencia
                                                                   BY nit:
    IF FIRST-OF(nit) THEN DO:
        FIND FIRST clientes WHERE clientes.nit = anexos.nit NO-LOCK NO-ERROR.
        IF AVAILABLE clientes AND clientes.tipo_cliente <> 5 THEN DO:
            FIND FIRST rep_ahorros WHERE rep_ahorros.fecCorte = 12/31/2014
                                     AND rep_ahorros.nit = clientes.nit
                                     AND rep_ahorros.tip_ahorro = 4
                                     AND rep_ahorros.estado = 1 NO-LOCK NO-ERROR.
            IF NOT AVAILABLE rep_ahorros THEN DO:
                EXPORT DELIMITER ";"
                    anexos.agencia
                    anexos.nit
                    clientes.nombre + " " + clientes.apellido1 + " " + clientes.apellido2.
            END.
        END.
    END.
END.
OUTPUT CLOSE.
