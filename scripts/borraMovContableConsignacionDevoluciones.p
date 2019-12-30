DISABLE TRIGGERS FOR LOAD OF mov_contable.

DEFINE TEMP-TABLE tt
    FIELD nit AS CHARACTER
    FIELD valor AS DECIMAL.

INPUT FROM c:\INFO_Fodun\devolucionesAhorros_Consolidado.csv.
REPEAT :
    CREATE tt.
    IMPORT DELIMITER ";" tt.
END.
INPUT CLOSE.

FOR EACH tt NO-LOCK:
    FIND FIRST mov_contable WHERE mov_contable.comprobante = 7
                              AND mov_contable.num_documento = 1422
                              AND mov_contable.fec_contable = TODAY
                              AND mov_contable.nit = tt.nit
                              AND mov_contable.cuenta = "24451001"
                              AND mov_contable.cr = tt.valor NO-ERROR.
    IF NOT AVAILABLE mov_contable THEN
        DISPLAY tt.
    ELSE
        DELETE mov_contable.
END.

MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
