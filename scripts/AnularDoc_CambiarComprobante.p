/*DEFINE TEMP-TABLE tmov LIKE mov_contable.

FOR EACH mov_contable WHERE agencia = 1
                        AND fec_contable = 01/24/2013
                        AND comprobante = 2
                        AND num_documento = 8337 NO-LOCK:
    CREATE tmov.
    BUFFER-COPY mov_contable TO tmov.

    tmov.db = mov_contable.cr.
    tmov.cr = mov_contable.db.
END.

FOR EACH tmov:
    CREATE mov_contable.
    BUFFER-COPY tmov TO mov_contable.
END.*/

DEFINE TEMP-TABLE tmov LIKE mov_contable.
DEFINE VAR pSecuencia AS INTEGER.
DEFINE VAR cont AS INTEGER.

FIND FIRST comprobantes WHERE agencia = 1 AND comprobante = 1 NO-ERROR.
comprobantes.secuencia = comprobantes.secuencia + 1.
pSecuencia = comprobantes.secuencia.

FOR EACH mov_contable WHERE agencia = 1
                        AND fec_contable = 01/24/2013
                        AND comprobante = 2
                        AND num_documento = 8337 NO-LOCK BY ROWID(mov_contable):
    cont = cont + 1.
    
    CREATE tmov.
    BUFFER-COPY mov_contable TO tmov.

    tmov.comprobant = 1.
    tmov.num_documento = pSecuencia.

    IF cont = 4 THEN
        LEAVE.
END.

FOR EACH tmov:
    CREATE mov_contable.
    BUFFER-COPY tmov TO mov_contable.
END
