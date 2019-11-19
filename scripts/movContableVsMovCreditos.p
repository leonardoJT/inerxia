OUTPUT TO d:\MC_MC.txt.
    
FOR EACH mov_contable WHERE SUBSTRING(mov_contable.cuenta,1,2) = "14"
                        AND SUBSTRING(mov_contable.cuenta,1,4) <> "1491"
                        AND SUBSTRING(mov_contable.cuenta,1,4) <> "1498"
                        AND mov_contable.comprobante <> 20 NO-LOCK BY mov_contable.agencia
                                                                   BY mov_contable.fec_contable:
    FIND FIRST mov_creditos WHERE mov_creditos.nit = mov_contable.nit
                              AND mov_creditos.Num_Credito = INTEGER(mov_contable.enlace)
                              AND mov_creditos.val_efectivo + mov_creditos.val_cheque = mov_contable.cr + mov_contable.db
                              AND mov_creditos.fecha = mov_contable.fec_contable NO-LOCK NO-ERROR.
    IF NOT AVAILABLE mov_creditos THEN DO:
        FIND FIRST mov_creditos WHERE mov_creditos.Num_Credito = INTEGER(mov_contable.enlace)
                                  AND mov_creditos.val_efectivo + mov_creditos.val_cheque = mov_contable.cr + mov_contable.db
                                  AND mov_creditos.fecha = mov_contable.fec_contable NO-LOCK NO-ERROR.
        IF NOT AVAILABLE mov_creditos THEN DO:
            FIND FIRST mov_creditos WHERE mov_creditos.nit = mov_contable.nit
                                      AND mov_creditos.val_efectivo + mov_creditos.val_cheque = mov_contable.cr + mov_contable.db
                                      AND mov_creditos.fecha = mov_contable.fec_contable NO-LOCK NO-ERROR.
            IF NOT AVAILABLE mov_creditos THEN
                DISPLAY mov_contable.agencia
                        mov_contable.fec_contable
                        mov_contable.comprobante
                        mov_contable.num_documento
                        mov_contable.nit
                        mov_contable.enlace
                        mov_contable.db
                        mov_contable.cr
                        mov_contable.comentario FORMAT "X(30)" WITH WIDTH 200.
        END.
    END.
END.
