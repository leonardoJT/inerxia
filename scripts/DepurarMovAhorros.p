DISABLE TRIGGERS FOR LOAD OF mov_ahorros.

DEFINE VAR cont AS INTEGER.
    
FOR EACH mov_ahorros WHERE val_efectivo = 0
                       AND val_cheque = 0
                       AND cod_operacion <> 030303001
                       AND cod_operacion <> 999999999:
    DELETE mov_ahorros.
    cont = cont + 1.
END.

MESSAGE "Fin" cont
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
