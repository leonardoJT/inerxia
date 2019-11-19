DEFINE TEMP-TABLE ttcrs
    FIELD Agencia AS INTEGER
    FIELD tipo AS INTEGER
    FIELD cod_credito AS INTEGER
    FIELD num_credito AS INTEGER
    FIELD pagare AS CHARACTER
    FIELD nit AS CHARACTER
    FIELD INT_corriente AS DECIMAL
    FIELD INT_difCobro AS DECIMAL
    FIELD mora_difCobro AS DECIMAL
    FIELD mora AS DECIMAL.

DEFINE VAR cont AS INTEGER.

INPUT FROM C:\INFO_FODUN\Leonardo\DevolverLiquidacion.csv.

REPEAT:
    cont = cont + 1.
    CREATE ttcrs.
    IMPORT DELIMITER ";" ttcrs NO-ERROR.

    IF ERROR-STATUS:ERROR THEN
        DELETE ttcrs.
END.

FOR EACH ttcrs NO-LOCK:
    FIND FIRST creditos WHERE creditos.nit = ttcrs.nit
                          AND creditos.agencia = ttcrs.agencia
                          AND creditos.tip_credito = ttcrs.tipo
                          AND creditos.cod_credito = ttcrs.cod_credito
                          AND creditos.num_credito = ttcrs.num_credito
                          AND creditos.pagare = ttcrs.pagare NO-ERROR.
    IF NOT AVAILABLE creditos THEN
        MESSAGE ttcrs.nit ttcrs.num_credito
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    ELSE DO:
        creditos.INT_corriente = creditos.INT_corriente - (ttcrs.INt_corriente / 2).
        creditos.INT_difCobro = creditos.INT_difCobro - (ttcrs.INT_difCobro / 2).
        creditos.INT_moraDifCob = creditos.INT_moraDifCob - (ttcrs.mora_difCobro / 2).
        creditos.INT_morCobrar = creditos.INT_morCobrar - (ttcrs.mora / 2).
    END.
END.
