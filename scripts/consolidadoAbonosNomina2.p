
DEFINE TEMP-TABLE tt
    FIELD nit AS CHARACTER
    FIELD valor AS DECIMAL EXTENT 11
    INDEX idx nit.

FOR EACH mov_contable WHERE mov_contable.fec_contable >= 01/01/2019
                        AND mov_contable.comprobante = 7 USE-INDEX idx_mov1 NO-LOCK BREAK BY mov_contable.nit:
    IF FIRST-OF(mov_contable.nit) THEN DO:
        FIND FIRST tt WHERE tt.nit = mov_contable.nit NO-ERROR.
        IF NOT AVAILABLE tt THEN DO:
            CREATE tt.
            tt.nit = mov_contable.nit.
        END.
    END.

    tt.valor[MONTH(mov_contable.fec_contable)] = tt.valor[MONTH(mov_contable.fec_contable)] + mov_contable.cr - mov_contable.db.
END.

OUTPUT TO c:\INFO_Fodun\Leonardo\abonosNommina2.csv.
EXPORT DELIMITER ";"
    "CLIENTE_ID"
    "ENERO"
    "FEBRERO"
    "MARZO"
    "ABRIL"
    "MAYO"
    "JUNIO"
    "JULIO"
    "AGOSTO"
    "SEPTIEMBRE"
    "OCTUBRE"
    "NOVIEMBRE".

FOR EACH tt NO-LOCK:
    EXPORT DELIMITER ";" tt.
END.
OUTPUT CLOSE.

MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
