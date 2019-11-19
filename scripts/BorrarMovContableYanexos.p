DISABLE TRIGGERS FOR LOAD OF mov_contable.
DISABLE TRIGGERS FOR LOAD OF anexos.

FOR EACH mov_contable WHERE mov_contable.agencia = 3
                        AND mov_contable.fec_contable >= 02/01/2011
                        AND mov_contable.fec_contable <= 02/28/2011
                        AND mov_contable.usuario = "2305":
    DELETE mov_contable.
END.

FOR EACH anexos WHERE anexos.agencia = 3
                  AND anexos.ano = 2011
                  AND (anexos.db[2] <> 0 OR anexos.cr[2] <> 0):
    anexos.db[2] = 0.
    anexos.cr[2] = 0.
END.
