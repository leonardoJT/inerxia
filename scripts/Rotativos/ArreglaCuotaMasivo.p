DISABLE TRIGGERS FOR LOAD OF creditos.
DISABLE TRIGGERS FOR LOAD OF mov_creditos.

DEFINE VAR vCuota AS DECIMAL.

OUTPUT TO d:\Leonardo\ArregloRotativos.txt.
FOR EACH facturacion WHERE fec_pago = 10/05/2014
                       AND facturacion.capital = 0
                       AND estado = 1:
    FIND FIRST creditos WHERE creditos.nit = facturacion.nit
                          AND creditos.num_credito = facturacion.num_credito
                          AND creditos.sdo_Capital > 0 NO-ERROR.
    IF AVAILABLE creditos THEN DO:
        facturacion.capital = creditos.sdo_capital / (creditos.plazo - creditos.cuo_pagadas).
        facturacion.cuota = facturacion.capital + facturacion.INT_corriente + facturacion.INT_difCobro + facturacion.INT_mora.
        facturacion.cuota = TRUNCATE((facturacion.cuota + 99) / 100,0) * 100.
        vCuota = creditos.cuota.
        creditos.cuota = facturacion.cuota - facturacion.pago_capital - facturacion.pago_intCorriente - facturacion.pago_intDifCobro - facturacion.pago_Mora.
        
        FIND FIRST mov_creditos WHERE mov_creditos.agencia = creditos.agencia
                                  AND mov_creditos.nit = creditos.nit
                                  AND mov_creditos.cod_credito = creditos.cod_credito
                                  AND mov_creditos.num_credito = creditos.num_credito
                                  AND Mov_Creditos.Cod_Operacion = 999999999
                                  AND Mov_Creditos.Fecha = 09/16/2014
                                  AND mov_creditos.cpte = 20
                                  AND SUBSTRING(mov_creditos.descrip,1,5) = "Corte" NO-ERROR.
        IF AVAILABLE mov_creditos THEN
            mov_creditos.descrip = "Corte/Asignación de Cuota --> " + STRING(creditos.cuota,"$>>>,>>>,>>9.99").

        DISPLAY creditos.agencia creditos.nit vCuota creditos.cuota.
    END.
END.
