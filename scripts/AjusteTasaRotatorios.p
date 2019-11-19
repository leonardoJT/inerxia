FOR EACH creditos WHERE creditos.cod_credito = 123
                    AND creditos.estado = 2:
    FOR EACH mov_creditos WHERE mov_creditos.nit = creditos.nit
                            AND mov_creditos.num_credito = creditos.num_credito
                            AND (mov_creditos.val_efectivo <> 0 OR mov_creditos.val_cheque <> 0) BY mov_creditos.fecha DESCENDING:
        FIND FIRST operacion  WHERE operacion.cod_operacion = mov_creditos.cod_operacion NO-LOCK NO-ERROR.
        IF AVAILABLE operacion THEN DO:
            IF operacion.tipo_operacion = 2 THEN DO:
                IF mov_creditos.fecha >= 05/28/2012 THEN DO:
                    /*MESSAGE creditos.nit creditos.tasa 15.6
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
                    
                    creditos.tasa = 15.6.
                END.
                ELSE DO:
                    /*MESSAGE creditos.nit creditos.tasa 14.4
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
                    
                    creditos.tasa = 14.4.
                END.

                LEAVE.
            END.
        END.
    END.
END.
