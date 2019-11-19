DISABLE TRIGGERS FOR LOAD OF mov_contable.

FOR EACH mov_contable WHERE fec_contable <= 01/02/2011:
    DELETE mov_contable.
END.

MESSAGE "Terminó"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
