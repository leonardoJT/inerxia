DEFINE TEMP-TABLE tmov LIKE mov_contable.

FOR EACH mov_contable WHERE mov_contable.agencia = 2
                        AND mov_contable.num_documento = 691
                        AND mov_contable.fec_contable >= 01/11/2011
                        AND mov_contable.nit = "79115345"
                        AND mov_contable.cr = 195 NO-LOCK:
    DISPLAY mov_contable EXCEPT comentario WITH 1 COL.
    
    CREATE tmov.
    BUFFER-COPY mov_contable TO tmov.
END.

FOR EACH tmov:
    DISPLAY tmov WITH WIDTH 100 1 COL.

    tmov.cr = 3200.

    CREATE mov_contable.
    BUFFER-COPY tmov TO mov_contable.
END.
