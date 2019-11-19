DEFINE TEMP-TABLE tmov LIKE mov_contable.
DEFINE VAR psec AS INTEGER.

FOR EACH mov_contable WHERE agencia = 2
                        AND fec_contable = 08/30/2019
                        AND comprobante = 21
                        AND num_documento = 4448
                        AND db + cr <> 0 NO-LOCK:
    CREATE tmov.
    BUFFER-COPY mov_contable TO tmov.
END.

/*FIND FIRST comprobantes WHERE comprobantes.comprobante = 9 AND comprobantes.agencia = 1 NO-ERROR.
comprobantes.secuencia = secuencia + 1.
psec = comprobantes.secuencia.*/

pSec = 4448.

FOR EACH tmov:
    CREATE mov_contable.
    BUFFER-COPY tmov TO mov_contable.

    /*mov_contable.fec_contable = TODAY.*/
    mov_contable.usuario = "desarrollo".
    mov_contable.fec_grabacion = TODAY.
    mov_contable.comentario = "Rev-" + mov_contable.comentario.
    
    IF mov_contable.db > 0 THEN DO:
        mov_contable.cr = mov_contable.db.
        mov_contable.db = 0.
    END.
    ELSE DO:
        mov_contable.db = mov_contable.cr.
        mov_contable.cr = 0.
    END.
END.

MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
