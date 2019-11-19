DEFINE BUFFER ttmov FOR mov_contable.

DEFINE TEMP-TABLE movs LIKE mov_contable.

FOR EACH mov_contable WHERE (SUBSTRING(mov_contable.cuenta,1,6) = "244515" OR
                             SUBSTRING(mov_contable.cuenta,1,6) = "244525")
                        AND mov_contable.comprobante <> 20
                        AND mov_contable.fec_contable >= 01/01/2015
                        AND mov_contable.fec_contable <= 12/31/2015 NO-LOCK BY mov_contable.fec_contable:
    FIND FIRST ttmov WHERE ttmov.agencia = mov_contable.agencia
                       AND ttmov.nit = mov_contable.nit
                       AND ttmov.fec_contable = mov_contable.fec_contable
                       AND ttmov.comprobante = mov_contable.comprobante
                       AND ttmov.num_documento = mov_contable.num_documento
                       AND (SUBSTRING(ttmov.cuenta,1,6) = "511001" OR
                         SUBSTRING(ttmov.cuenta,1,8) = "51105211" OR
                         SUBSTRING(ttmov.cuenta,1,8) = "51105212" OR
                         SUBSTRING(ttmov.cuenta,1,8) = "51105213" OR
                         SUBSTRING(ttmov.cuenta,1,8) = "51105214" OR
                         SUBSTRING(ttmov.cuenta,1,6) = "511058" OR
                         SUBSTRING(ttmov.cuenta,1,8) = "61759512" OR
                         SUBSTRING(ttmov.cuenta,1,6) = "511004") NO-LOCK NO-ERROR.
    IF NOT AVAILABLE ttmov THEN DO:
        /*DISPLAY mov_contable WITH WIDTH 300 1 COL.*/
        CREATE movs.
        BUFFER-COPY mov_contable TO movs.
    END.
END.

OUTPUT TO d:\Leonardo\Honorarios_244515.csv.
EXPORT DELIMITER ";"
    "FECHA"
    "AGENCIA"
    "COMPROBANTE"
    "NUM_DOCUMENTO"
    "CUENTA"
    "NIT"
    "DESCRIPCION"
    "DB"
    "CR".

FOR EACH movs NO-LOCK BY movs.fec_contable:
    FOR EACH mov_contable WHERE mov_contable.fec_contable = movs.fec_contable
                            AND mov_contable.agencia = movs.agencia
                            AND mov_contable.comprobante = movs.comprobante
                            AND mov_contable.num_documento = movs.num_documento
                            AND mov_contable.nit = movs.nit NO-LOCK:
        EXPORT DELIMITER ";"
            mov_contable.fec_contable
            mov_contable.agencia
            mov_contable.comprobante
            mov_contable.num_documento
            mov_contable.cuenta
            mov_contable.nit
            mov_contable.comentario
            mov_contable.db
            mov_contable.cr.
    END.

    EXPORT DELIMITER ";" "".
END.
OUTPUT CLOSE.

MESSAGE "Terminó"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
