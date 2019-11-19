DEFINE TEMP-TABLE tcheques
    FIELD numero AS DECIMAL
    FIELD fecha AS DATE
    FIELD comprobante AS INTEGER
    FIELD usuario AS CHARACTER.

FOR EACH mov_contable WHERE agencia = 2
                        AND cuenta = "11100521"
                        AND fec_contable >= 02/01/2012 NO-LOCK BY fec_contable:
    CREATE tcheques.
    tcheques.numero = DECIMAL(doc_referencia) NO-ERROR.
    tcheques.fecha = mov_contable.fec_contable.
    tcheques.comprobante = mov_contable.comprobante.
    tcheques.usuario = mov_contable.usuario.
END.

FOR EACH tcheques WHERE tcheques.numero > 0 NO-LOCK BY tcheques.numero DESCENDING:
    DISPLAY tcheques.comprobante tcheques.numero tcheques.fecha tcheques.usuario.
END.


DISABLE TRIGGERS FOR LOAD OF mov_contable.
DEFINE VAR numero AS DECIMAL.

FOR EACH mov_contable WHERE agencia = 2
                        AND cuenta = "11100521"
                        AND fec_contable >= 02/01/2012 BY fec_contable:
    numero = DECIMAL(mov_contable.doc_referencia) NO-ERROR.

    IF numero > 85572 THEN
        mov_contable.doc_referencia = mov_contable.doc_referencia + "X".
END.

