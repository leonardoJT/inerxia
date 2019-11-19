DEFINE VAR vFecha AS DATE INITIAL 01/31/2014.
    
FOR EACH repositorio.ahorros WHERE repositorio.ahorros.estado = 1 NO-LOCK:
    CREATE bdcentral.rep_ahorros.
    bdcentral.rep_ahorros.fecCorte = vFecha.
    BUFFER-COPY repositorio.ahorros TO bdcentral.rep_ahorros.
END.

FOR EACH repositorio.creditos WHERE repositorio.creditos.estado = 2 OR repositorio.creditos.fec_canceTotal >= ADD-INTERVAL(vFecha,-1,"months") + 1 NO-LOCK:
    CREATE bdcentral.rep_creditos.
    bdcentral.rep_creditos.fecCorte = vFecha.
    BUFFER-COPY repositorio.creditos TO bdcentral.rep_creditos.

    /* Ajustes para el reporte de CIFIN */
    IF bdcentral.rep_creditos.cuota = 0 THEN
        bdcentral.rep_creditos.cuota = INTEGER(bdcentral.rep_creditos.sdo_capital / bdcentral.rep_creditos.plazo) + bdcentral.rep_creditos.INT_corriente + bdcentral.rep_creditos.INT_difCobro + bdcentral.rep_credito.INT_morCobrar.

    IF bdcentral.rep_credito.cuota = ? THEN
        bdcentral.rep_creditos.cuota = 0.

    IF bdcentral.rep_creditos.sdo_capital = 0 THEN DO:
        bdcentral.rep_creditos.dias_atraso = 0.
        bdcentral.rep_creditos.val_atraso = 0.
        bdcentral.rep_creditos.cuo_atraso = 0.
    END.
    /* -------------------------------- */
END.

MESSAGE "Fin:" vFecha
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
