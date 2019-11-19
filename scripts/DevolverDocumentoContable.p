DEFINE TEMP-TABLE movCont LIKE mov_contable.
DEFINE VAR deb AS DECIMAL.
DEFINE VAR cred AS DECIMAL.
DEFINE VAR pSec AS INTEGER.

FOR EACH mov_contable WHERE mov_contable.num_documento = 3488
                        AND mov_contable.agencia = 4
                        AND mov_contable.comprobante = 20
                        AND mov_contable.fec_contable = 07/31/2015 NO-LOCK:
    CREATE movCont.
    BUFFER-COPY mov_contable TO movCont.
END.

FOR EACH movCont:
    deb = movCont.db.
    cred = movCont.cr.

    movCont.db = cred.
    movCont.cr = deb.

    CREATE mov_contable.
    BUFFER-COPY movCont TO mov_contable.

    mov_contable.comentario = "Rev" + mov_contable.comentario.
    mov_contable.fec_grabacion = TODAY.
END.

MESSAGE "Ok"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
