DEFINE TEMP-TABLE ttmovs LIKE mov_creditos.

FIND FIRST creditos WHERE creditos.nit = "16752279"
                      AND creditos.num_credito = 9901795 NO-ERROR.
/*UPDATE creditos WITH 1 COL.

FOR EACH mov_creditos WHERE mov_creditos.nit = creditos.nit
                        AND mov_creditos.num_credito = creditos.num_credito
                        AND fecha = 02/28/2017 NO-LOCK:
    CREATE ttmovs.
    BUFFER-COPY mov_creditos TO ttmovs.

    DISPLAY mov_creditos WITH 1 COL.

    ttmovs.descrip = "Rev-" + ttmovs.descrip.
    ttmovs.cod_operacion = 020102002.
    ttmovs.sdo_capital = creditos.sdo_Capital.
    ttmovs.fecha = 03/02/2017.
END.

FOR EACH ttmovs NO-LOCK:
    CREATE mov_credito.
    BUFFER-COPY ttmovs TO mov_credito.
END.

*/
FOR EACH facturacion WHERE facturacion.nit = creditos.nit
                       AND facturacion.fec_pago >= 06/05/2016 BY fec_pago:
    UPDATE facturacion WITH 1 COL.
END.


