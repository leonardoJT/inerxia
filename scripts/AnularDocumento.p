DEFINE TEMP-TABLE tmov LIKE mov_contable.
DEFINE VAR psec AS INTEGER.

FOR EACH mov_contable WHERE fec_contable = 07/03/2018
                        AND comprobante = 15
                        AND num_documento = 88
                        AND agencia = 1
                        AND db + cr <> 0 NO-LOCK:
    CREATE tmov.
    BUFFER-COPY mov_contable TO tmov.
END.

FOR EACH tmov:
    CREATE mov_contable.
    BUFFER-COPY tmov TO mov_contable.

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
