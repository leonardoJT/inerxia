FOR EACH creditos WHERE creditos.cod_credito = 123
                    AND creditos.sdo_capital + creditos.INT_corriente + creditos.INT_morCobrar + creditos.INT_difCobro + creditos.int_MoradifCob > 0
                    AND creditos.fec_pago < TODAY
                    AND creditos.dias_atraso < 85
                    AND creditos.abogado = NO
                    AND creditos.detalle_Estado <> 2:
    FIND FIRST facturacion WHERE facturacion.nit = creditos.nit
                             AND facturacion.num_credito = creditos.num_credito
                             AND facturacion.estado = 1 NO-LOCK NO-ERROR.
    IF AVAILABLE facturacion THEN
        RUN p-debitoAutomaticoRotativo_prueba.r (INPUT ROWID(creditos),
                                                 INPUT FALSE, /* No debita del Ahorro Permanente */
                                                 INPUT "Proceso automático").
END.

DISCONNECT bdcentral.
QUIT.

