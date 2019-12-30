OUTPUT TO C:\INFO_fodun\devolucionesCreditos_Consolidado.csv.
FOR EACH mov_ahorros WHERE mov_ahorros.agencia <> 0
                       AND mov_ahorros.cod_ahorro = 8
                       AND mov_ahorros.cue_ahorros <> ""
                       AND mov_ahorros.fecha = TODAY
                       AND mov_ahorros.num_documento = "1423" NO-LOCK:
    EXPORT DELIMITER ";" mov_ahorros.nit mov_ahorros.val_efectivo.
END.
OUTPUT CLOSE.

MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
