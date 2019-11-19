DISABLE TRIGGERS FOR LOAD OF mov_contable_niif.

DEFINE VAR vCuentaNIIF AS CHARACTER.
DEFINE VAR tempFecha AS DATE INITIAL 03/31/2017.

FOR EACH agencias /*WHERE agencias.agencia >= 1*/ NO-LOCK BY agencias.agencia:
    FOR EACH mov_contable WHERE mov_contable.agencia = agencias.agencia
                            AND mov_contable.fec_contable >= 06/04/2017
                            AND mov_contable.fec_contable <= 06/04/2017 NO-LOCK:
        FIND FIRST cuentas WHERE cuentas.cuenta = mov_contable.cuenta NO-LOCK NO-ERROR.
        IF AVAILABLE cuentas AND cuentas.cuentaNIIF <> "" THEN DO:
            CREATE mov_contable_niif.
            BUFFER-COPY mov_contable TO mov_contable_niif.
            mov_contable_niif.cuenta = cuentas.cuentaNIIF.
        END.
        /*ELSE
            DISPLAY mov_contable WITH WIDTH 300 1 COL.*/
    END.
END.

MESSAGE "Fin Copia"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
