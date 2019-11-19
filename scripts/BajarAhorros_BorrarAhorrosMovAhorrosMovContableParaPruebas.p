/*OUTPUT TO c:\INFO_Fodun\Ahorros.txt.

FOR EACH ahorros WHERE (ahorros.tip_ahorro = 1 AND (ahorros.cod_ahorro = 9 OR ahorros.cod_ahorro = 4))
                    OR (ahorros.tip_ahorro = 2 AND ahorros.cod_ahorro = 3) NO-LOCK:
    EXPORT ahorros.
END.*/

DISABLE TRIGGERS FOR LOAD OF ahorros.
FOR EACH ahorros WHERE (ahorros.tip_ahorro = 1 AND (ahorros.cod_ahorro = 9 OR ahorros.cod_ahorro = 4))
                    OR (ahorros.tip_ahorro = 2 AND ahorros.cod_ahorro = 3):
    DELETE ahorros.
END.

DISABLE TRIGGERS FOR LOAD OF mov_ahorros.
FOR EACH mov_ahorros WHERE mov_ahorros.fecha = TODAY:
    DELETE mov_ahorros.
END.

DISABLE TRIGGERS FOR LOAD OF mov_contable.
FOR EACH mov_contable WHERE mov_contable.fec_contable = TODAY:
    DELETE mov_contable.
END.

INPUT FROM c:\INFO_Fodun\Ahorros.txt.
REPEAT:
    CREATE ahorros.
    IMPORT ahorros.
END.

MESSAGE "OK!"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
