DISABLE TRIGGERS FOR LOAD OF mov_contable.

DEFINE VAR vDB AS DECIMAL.
DEFINE VAR vCR AS DECIMAL.

DEFINE TEMP-TABLE tt
    FIELD agencia AS INTEGER
    FIELD fecha AS DATE FORMAT "99/99/9999"
    FIELD comprobante AS INTEGER
    FIELD num_documento AS INTEGER
    FIELD comentario AS CHARACTER.

INPUT FROM d:\Leonardo\reversos.csv.
REPEAT:
    CREATE tt.
    IMPORT DELIMITER ";" tt.
END.
INPUT CLOSE.

FOR EACH tt NO-LOCK:
    FIND FIRST mov_contable WHERE mov_contable.agencia = tt.agencia
                              AND mov_contable.fec_contable = tt.fecha
                              AND mov_contable.comprobante = tt.comprobante
                              AND mov_contable.num_documento = tt.num_documento
                              AND mov_contable.comentario = tt.comentario
                              AND mov_contable.cuenta = "14420505" NO-ERROR.
    IF AVAILABLE mov_contable THEN DO:
        vDB = mov_contable.db.
        vCR = mov_contable.cr.

        mov_contable.db = vCR.
        mov_contable.cr = vDB.
    END.
END.
