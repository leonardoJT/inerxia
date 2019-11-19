DISABLE TRIGGERS FOR LOAD OF mov_contable_niif.

FOR EACH mov_contable WHERE mov_contable.agencia = 1
                        AND mov_contable.fec_contable = 05/05/2017
                        AND mov_contable.comprobante = 20
                        AND mov_contable.num_documento = 4922 NO-LOCK:
    FIND FIRST cuentas WHERE cuentas.cuenta = mov_contable.cuenta NO-LOCK NO-ERROR.
    IF AVAILABLE cuentas AND cuentas.cuentaNIIF <> "" THEN DO:
        CREATE mov_contable_niif.
        BUFFER-COPY mov_contable TO mov_contable_niif.
        mov_contable_niif.cuenta = cuentas.cuentaNIIF.
    END.
    ELSE
        DISPLAY mov_contable WITH WIDTH 300 1 COL.
END.

MESSAGE "Fin Copia"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
