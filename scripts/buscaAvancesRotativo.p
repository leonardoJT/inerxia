/*FOR EACH mov_contable WHERE fec_contable = 09/09/2015
                        AND num_documento = 210
                        AND comprobante = 21 NO-LOCK:
    DISPLAY mov_contable WITH WIDTH 300 1 COL.
END.*/

DEFINE BUFFER ttmovs FOR mov_contable.

DEFINE TEMP-TABLE tt
    FIELD fec_contable AS DATE
    FIELD num_documento AS INTEGER
    FIELD nit AS CHARACTER.

FOR EACH mov_contable WHERE fec_contable >= 01/01/2014
                        /*AND fec_contable <= 12/31/2014*/
                        AND mov_contable.comprobante = 2
                        AND mov_contable.cuenta = "14420505"
                        AND mov_contable.db > 0 NO-LOCK BY mov_contable.fec_contable:
    FIND FIRST tt WHERE tt.fec_contable = mov_contable.fec_contable
                    AND tt.num_Documento = mov_contable.num_documento NO-LOCK NO-ERROR.
    IF NOT AVAILABLE tt THEN DO:
        CREATE tt.
        BUFFER-COPY mov_contable TO tt.
    END.
END.

OUTPUT TO d:\Leonardo\movsAvances.csv.
FOR EACH tt NO-LOCK:
    EXPORT DELIMITER ";" tt.
END.
OUTPUT CLOSE.

