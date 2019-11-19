DEFINE TEMP-TABLE tt
    FIELD agencia AS INTEGER
    FIELD mes AS INTEGER
    FIELD cuenta AS CHARACTER
    FIELD cliente_id AS CHARACTER
    FIELD db AS DECIMAL
    FIELD cr AS DECIMAL
    INDEX idx agencia mes cuenta cliente_id.

FOR EACH agencias NO-LOCK:
    FOR EACH mov_contable WHERE mov_contable.agencia = agencias.agencia
                            AND mov_contable.fec_contable >= 01/01/2019
                            AND mov_contable.fec_contable <= 10/31/2019
                            AND (SUBSTRING(mov_contable.cuenta,1,1) = "4" OR
                                 SUBSTRING(mov_contable.cuenta,1,1) = "5" OR
                                 SUBSTRING(mov_contable.cuenta,1,1) = "6") NO-LOCK:
        FIND FIRST tt WHERE tt.agencia = mov_contable.agencia
                        AND tt.mes = month(mov_contable.fec_contable)
                        AND tt.cuenta = mov_contable.cuenta
                        AND tt.cliente_id = mov_contable.nit NO-ERROR.
        IF NOT AVAILABLE tt THEN DO:
            CREATE tt.
            tt.agencia = mov_contable.agencia.
            tt.mes = month(mov_contable.fec_contable).
            tt.cuenta = mov_contable.cuenta.
            tt.cliente_id = mov_contable.nit.
        END.

        tt.db = tt.db + mov_contable.db.
        tt.cr = tt.cr + mov_contable.cr.
    END.
END.

OUTPUT TO c:\Info_Fodun\Leonardo\movs456.csv.
EXPORT DELIMITER ";" "AGENCIA" "MES" "CUENTA" "CLIENTE_ID" "DB" "CR".

FOR EACH tt NO-LOCK:
    EXPORT DELIMITER ";" tt.
END.
OUTPUT CLOSE.

MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
