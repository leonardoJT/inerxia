DISABLE TRIGGERS FOR LOAD OF mov_ahorros.

DEFINE TEMP-TABLE nomina
    FIELD cuenta AS CHARACTER
    FIELD nit AS CHARACTER
    FIELD valor AS DECIMAL.

INPUT FROM d:\Leonardo\nomina.csv.
REPEAT :
    CREATE nomina.
    IMPORT delimiter ";" nomina.
END.
INPUT CLOSE.

FOR EACH nomina NO-LOCK:
    IF nomina.cuenta = "21050501" THEN DO:
        FIND FIRST ahorros WHERE ahorros.nit = nomina.nit
                             AND ahorros.cod_ahorro = 4
                             AND ahorros.estado = 1
                             AND ahorros.sdo_disponible >= nomina.valor NO-ERROR.
        IF NOT AVAILABLE ahorros THEN
            MESSAGE nomina.nit
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        ELSE DO:
            ahorros.sdo_disponible = ahorros.sdo_disponible - nomina.valor.

            FIND FIRST mov_ahorros WHERE mov_ahorros.nit = ahorros.nit
                                     AND mov_ahorros.cod_ahorro = ahorros.cod_ahorro
                                     AND mov_ahorros.fecha = TODAY
                                     AND mov_ahorros.val_efectivo = nomina.valor NO-ERROR.
            IF NOT AVAILABLE mov_ahorros THEN
                MESSAGE nomina.nit
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
            ELSE
                DELETE mov_ahorros.
        END.
    END.

    IF nomina.cuenta = "24953001" THEN DO:
        FIND FIRST ahorros WHERE ahorros.nit = nomina.nit
                             AND ahorros.cod_ahorro = 8
                             AND ahorros.estado = 1
                             AND ahorros.sdo_disponible >= nomina.valor NO-ERROR.
        IF NOT AVAILABLE ahorros THEN
            MESSAGE nomina.nit
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        ELSE DO:
            ahorros.sdo_disponible = ahorros.sdo_disponible - nomina.valor.

            FIND FIRST mov_ahorros WHERE mov_ahorros.nit = ahorros.nit
                                     AND mov_ahorros.cod_ahorro = ahorros.cod_ahorro
                                     AND mov_ahorros.fecha = TODAY
                                     AND mov_ahorros.val_efectivo = nomina.valor NO-ERROR.
            IF NOT AVAILABLE mov_ahorros THEN
                MESSAGE nomina.nit
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
            ELSE
                DELETE mov_ahorros.
        END.
    END.
END.

MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
