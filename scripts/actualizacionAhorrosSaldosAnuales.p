DISABLE TRIGGERS FOR LOAD OF ahorros.

FOR EACH ahorros WHERE cod_ahorro = 3:
    ahorros.sdo_anuales[8] = 0.
END.

FOR EACH rep_ahorros WHERE rep_ahorros.fecCorte = 08/31/2019 NO-LOCK:
    FIND FIRST ahorros WHERE ahorros.nit = rep_ahorros.nit
                         AND ahorros.cod_ahorro = rep_ahorros.cod_ahorro
                         AND ahorros.cue_ahorro = rep_ahorros.cue_ahorro NO-ERROR.
    IF AVAILABLE ahorros THEN
        ahorros.sdo_anuales[MONTH(rep_ahorros.fecCorte)] = rep_ahorros.sdo_disponible.
END.

MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
