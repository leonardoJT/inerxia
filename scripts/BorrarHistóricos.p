DISABLE TRIGGERS FOR LOAD OF mov_contable.
DISABLE TRIGGERS FOR LOAD OF mov_ahorros.
DISABLE TRIGGERS FOR LOAD OF mov_creditos.

FOR EACH mov_contable WHERE mov_contable.fec_contable <= 10/31/2011:
    DELETE mov_contable.
END.

MESSAGE "Fin mov_contable"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.


FOR EACH mov_ahorros WHERE mov_ahorros.fecha <= 10/31/2011:
    DELETE mov_ahorros.
END.

MESSAGE "Fin mov_ahorros"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.


FOR EACH mov_creditos WHERE mov_creditos.fecha <= 10/31/2011:
    DELETE mov_creditos.
END.

MESSAGE "Fin mov_creditos"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
