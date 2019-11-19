DEFINE BUFFER bfrCont FOR mov_contable.

FOR EACH bfrCont WHERE bfrCont.agencia = 1 and bfrCont.fec_contable = 06/22/2016
    AND comprobante = 4 AND bfrCont.num_documento = 13
    /*AND cuenta = "1655180101"*/ NO-LOCK:
    DISPLAY bfrCont WITH WIDTH 300 1 COL.

    CREATE mov_contable.
    BUFFER-COPY bfrCont TO mov_contable.
    mov_contable.cuenta = "91250501".
    
    IF bfrCont.cr > 0 THEN
        ASSIGN mov_contable.db = bfrCont.cr
               mov_contable.cr = 0.

    IF bfrCont.db > 0 THEN
        ASSIGN mov_contable.cr = bfrCont.db
               mov_contable.db = 0.

END.
