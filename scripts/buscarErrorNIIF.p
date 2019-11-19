FOR EACH agencias NO-LOCK:
    FOR EACH mov_contable WHERE mov_contable.agencia = agencias.agencia
                            AND mov_contable.fec_contable = 08/17/2017 NO-LOCK BREAK BY mov_contable.cuenta:
        IF FIRST-OF(mov_contable.cuenta) THEN DO:
            FIND FIRST cuentas WHERE cuentas.cuenta = mov_contable.cuenta NO-LOCK NO-ERROR.
            IF NOT AVAILABLE cuentas THEN DO:
                DISPLAY mov_contable WITH WIDTH 300 1 COL.
                LEAVE.
            END.
        END.
    END.
END.

MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
