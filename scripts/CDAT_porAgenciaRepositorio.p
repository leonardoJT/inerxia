DEFINE VAR suma3 AS INTEGER.
DEFINE VAR suma6 AS INTEGER.

FOR EACH agencias NO-LOCK:
    suma3 = 0.
    suma6 = 0.

    FOR EACH rep_ahorros WHERE fecCorte = 03/31/2015
                           AND rep_ahorros.tip_ahorro = 3
                           AND rep_ahorros.agencia = agencias.agencia
                           AND estado = 1
                           AND sdo_disponible > 0 NO-LOCK:
        IF plazo >= 80 AND plazo <= 100 THEN
            suma3 = suma3 + 1.
        ELSE
            suma6 = suma6 + 1.
    END.

    MESSAGE agencias.agencia suma3 suma6
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.
