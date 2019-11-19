DEFINE VAR vFecha AS DATE INITIAL 07/05/2016.

DO vFecha = 07/01/2016 TO 07/31/2016:
    /*FOR EACH mov_contable WHERE mov_contable.fec_contable = vFecha
                            AND mov_contable.comprobante <> 20
                            AND mov_contable.comprobante <> 22 NO-LOCK BREAK BY mov_contable.agencia
                                                                             by mov_contable.comprobante
                                                                             BY mov_contable.num_documento:
        IF FIRST-OF (mov_contable.num_documento) THEN DO:
            FIND FIRST mov_contable_NIIF WHERE mov_contable_niif.agencia = mov_contable.agencia 
                                           AND mov_contable_niif.comprobante = mov_contable.comprobante
                                           AND mov_contable_niif.num_documento = mov_contable.num_documento
                                           AND mov_contable_NIIF.fec_contable = mov_contable.fec_contable NO-LOCK NO-ERROR.
            IF not AVAILABLE mov_contable_NIIF THEN
                DISPLAY mov_contable WITH WIDTH 300 1 COL.
        END.
    END.

    MESSAGE "SES --> NIIF" vFecha
        VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

    FOR EACH mov_contable_NIIF WHERE mov_contable_NIIF.fec_contable = vFecha NO-LOCK BREAK BY mov_contable_NIIF.agencia
                                                                                           by mov_contable_NIIF.comprobante
                                                                                           BY mov_contable_NIIF.num_documento:
        IF first-of(mov_contable_NIIF.num_documento) THEN DO:
            FIND FIRST mov_contable WHERE mov_contable.agencia = mov_contable_NIIF.agencia
                                      AND mov_contable.comprobante = mov_contable_NIIF.comprobante
                                      AND mov_contable.num_documento = mov_contable_NIIF.num_documento
                                      AND mov_contable.fec_contable = mov_contable_NIIF.fec_contable NO-LOCK NO-ERROR.
            IF not AVAILABLE mov_contable THEN
                DISPLAY mov_contable_NIIF WITH WIDTH 300 1 COL.
        END.
    END.

    MESSAGE "NIIF --> SES" vFecha
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.


