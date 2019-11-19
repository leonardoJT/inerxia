DEFINE TEMP-TABLE tmov LIKE mov_contable.

FIND FIRST mov_contable WHERE mov_contable.fec_contable = 08/14/2012
                          AND mov_contable.agencia = 1
                          AND mov_contable.num_documento = 6704
                          AND mov_contable.comprobante = 21
                          AND cuenta = "1655180101" NO-LOCK NO-ERROR.

DISPLAY mov_contable WITH WIDTH 200 1 COL.


CREATE tmov.
BUFFER-COPY mov_contable TO tmov.

CREATE mov_contable.
BUFFER-COPY tmov TO mov_contable.

mov_contable.cr = mov_contable.db.
mov_contable.db = 0.
mov_contable.comentario = "Abono Interés Corriente".
