
DEFINE VARIABLE viCnt AS INTEGER     NO-UNDO.

OUTPUT TO "C:\tercerosBorrados.d".

DISABLE TRIGGERS FOR LOAD OF clientes.
DISABLE TRIGGERS FOR LOAD OF anexos_clientes.

FOR EACH clientes :
    IF clientes.nit BEGINS "0" THEN NEXT.
    FIND FIRST ahorros WHERE ahorros.nit EQ clientes.nit NO-LOCK NO-ERROR.
    IF NOT AVAILABLE ahorros THEN DO:
        FIND FIRST creditos WHERE creditos.nit EQ clientes.nit NO-LOCK NO-ERROR.
        IF NOT AVAILABLE creditos THEN DO:
            FIND FIRST mov_contable WHERE mov_contable.nit EQ clientes.nit NO-LOCK NO-ERROR.
            IF NOT AVAILABLE mov_contable THEN DO:
                FIND FIRST anexos_Clientes WHERE anexos_Clientes.nit EQ clientes.nit EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE anexos_Clientes THEN 
                    DELETE anexos_clientes.
                ASSIGN viCnt = viCnt + 1.
                EXPORT clientes.
                DELETE clientes.
            END.
        END.
    END.
END.

MESSAGE "Clientes Borrados: " viCnt
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

OUTPUT CLOSE.
