DEFINE VAR cont AS INTEGER.

FOR EACH mov_creditos WHERE val_efectivo = 0
                        AND val_cheque = 0
                        AND cod_operacion <> 000000000
                        AND cod_operacion <> 020303001
                        AND cod_operacion <> 030303001
                        AND cod_operacion <> 999999999:
    DELETE mov_creditos.
    
    cont = cont + 1.
END.

MESSAGE "Fin" cont
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
