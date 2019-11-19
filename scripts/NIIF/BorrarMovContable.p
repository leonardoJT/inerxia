DISABLE TRIGGERS FOR LOAD OF mov_contable_niif.

FOR EACH mov_contable_niif WHERE fec_contable >= 01/01/2016
                             AND fec_contable <= 01/31/2016:
    DELETE mov_contable_niif.
END.

MESSAGE "OK"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
