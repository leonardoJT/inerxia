DEFINE TEMP-TABLE tt
    FIELD nit AS CHARACTER
    FIELD valor AS DECIMAL EXTENT 11
    INDEX idx nit.

FOR EACH mov_ahorros WHERE mov_ahorros.fecha >= 01/01/2019
                       AND mov_ahorros.cpte = 7
                       AND mov_ahorros.cod_operacion = 010301001 NO-LOCK BREAK BY mov_ahorros.nit:
    IF FIRST-OF(mov_ahorros.nit) THEN DO:
        FIND FIRST tt WHERE tt.nit = mov_ahorros.nit NO-ERROR.
        IF NOT AVAILABLE tt THEN DO:
            CREATE tt.
            tt.nit = mov_ahorros.nit.
        END.
    END.

    tt.valor[MONTH(mov_ahorros.fecha)] = tt.valor[MONTH(mov_ahorros.fecha)] + mov_ahorros.val_efectivo.
END.

FOR EACH mov_creditos WHERE mov_creditos.fecha >= 01/01/2019
                        AND mov_creditos.cpte = 7 NO-LOCK BREAK BY mov_creditos.nit:
    IF FIRST-OF(mov_creditos.nit) THEN DO:
        FIND FIRST tt WHERE tt.nit = mov_creditos.nit NO-ERROR.
        IF NOT AVAILABLE tt THEN DO:
            CREATE tt.
            tt.nit = mov_creditos.nit.
        END.
    END.

    tt.valor[MONTH(mov_creditos.fecha)] = tt.valor[MONTH(mov_creditos.fecha)] + mov_creditos.val_efectivo.
END.


OUTPUT TO c:\INFO_Fodun\Leonardo\abonosNommina.csv.
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
