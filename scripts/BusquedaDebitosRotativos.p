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
                        AND comprobante = 21
                        AND cuenta = "21301001" NO-LOCK BY fec_contable:
    FIND FIRST ttmovs WHERE ttmovs.fec_contable = mov_contable.fec_contable
                        AND ttmovs.num_documento = mov_contable.num_documento
                        AND ttmovs.comprobante = 21
                        AND ttmovs.nit = mov_contable.nit
                        AND ttmovs.agencia = mov_contable.agencia
                        AND ttmovs.cuenta = "14420505" NO-LOCK NO-ERROR.
    IF AVAILABLE ttmovs THEN DO:
        FIND FIRST tt WHERE tt.fec_contable = mov_contable.fec_contable
                        AND tt.num_Documento = mov_contable.num_documento NO-LOCK NO-ERROR.
        IF NOT AVAILABLE tt THEN DO:
            CREATE tt.
            BUFFER-COPY mov_contable TO tt.
        END.
    END.
END.

OUTPUT TO d:\Leonardo\movsDebito2014.csv.
FOR EACH tt NO-LOCK:
    EXPORT DELIMITER ";" tt.
END.
OUTPUT CLOSE.
