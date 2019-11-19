OUTPUT TO d:\Leonardo\movs_75147376.csv.
EXPORT DELIMITER ";"
    "AGENCIA"
    "FECHA"
    "COMPROBANTE"
    "NUM_DOCUMENTO"
    "CUENTA"
    "COMENTARIO"
    "DB"
    "CR".

FOR EACH agencias NO-LOCK BY agencia:
    FOR EACH mov_contable WHERE mov_contable.agencia = agencias.agencia
                            AND mov_contable.fec_contable >= 01/01/2017
                            AND mov_contable.fec_contable <= 12/31/2017
                            AND nit = "75147376" NO-LOCK BY agencia
                                                         BY fec_contable:
        FIND FIRST comprobantes WHERE comprobantes.comprobante = mov_contable.comprobante NO-LOCK NO-ERROR.
        
        EXPORT DELIMITER ";"
            agencias.nombre
            STRING(mov_contable.fec_contable,"99/99/9999")
            comprobantes.nombre
            mov_contable.num_documento
            mov_contable.cuenta
            mov_contable.comentario
            mov_contable.db
            mov_contable.cr.
    END.
END.
OUTPUT CLOSE.

MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
