OUTPUT TO d:\Leonardo\movsVianey.csv.
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
    FOR EACH mov_contable WHERE mov_contable.agencia = agencia.agencia
                            AND mov_contable.fec_contable >= 07/01/2018
                            AND mov_contable.fec_contable <= 08/31/2018
                            AND usuario = "vgonzalez" NO-LOCK BY agencia
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
