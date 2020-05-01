DEFINE VAR saldoPromedio AS DECIMAL.
DEFINE VAR cont AS INTEGER.
DEFINE VAR fecha AS DATE.
DEFINE VAR fechaFin AS DATE.

FOR EACH ahorros WHERE ahorros.tip_ahorro = 4
                   AND ahorros.estado = 1:
    saldoPromedio = ahorros.sdo_disponible.

    IF ahorros.fec_apertura < 03/01/2011 THEN
        fechaFin = 03/01/2011.
    ELSE
        fechaFin = ahorros.fec_apertura.

    cont = 0.

    DO fecha = TODAY TO fechaFin:
        cont = 1.

        FOR EACH mov_ahorros WHERE mov_Ahorros.agencia = ahorros.agencia
                               AND mov_ahorros.cod_ahorro = ahorros.cod_ahorro
                               AND mov_ahorros.cue_ahorros = ahorros.cue_ahorros
                               AND mov_ahorros.fecha = fecha NO-LOCK:
            /* Calcular el saldo del día anterior a partir de los movimientos del día de hoy, revisando el tipo de operación (mov_ahorros.cod_operacion) */
            IF  THEN
        END.
    END.


END.
