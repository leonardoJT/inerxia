FOR EACH creditos WHERE cod_credito = 17
                    AND estado = 2
                    AND fec_desembolso >= 06/24/2017
                    AND (tasa <= 13.79 OR tasa >= 13.81)
                    AND creditos.num_credito <> 20238 BY creditos.num_credito:
    FIND FIRST mov_creditos WHERE mov_creditos.nit = creditos.nit
                              AND mov_creditos.num_credito = creditos.num_credito
                              AND mov_creditos.fecha < creditos.fec_desembolso NO-LOCK NO-ERROR.
    IF NOT AVAILABLE mov_creditos THEN
        UPDATE creditos.agencia
               credito.nit
               creditos.num_credito
               creditos.fec_desembolso
               creditos.tasa
            WITH 1 COL.
END.
