DISABLE TRIGGERS FOR LOAD OF mov_contable.

DEFINE VAR cont AS INTEGER.

FOR EACH mov_contable WHERE db = 0 AND cr = 0:
    DELETE mov_contable.
    cont = cont + 1.
END.

MESSAGE cont
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
