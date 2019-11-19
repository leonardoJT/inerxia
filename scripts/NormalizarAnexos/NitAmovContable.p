DISABLE TRIGGERS FOR LOAD OF mov_contable.

FOR EACH mov_contable WHERE cuenta = "25100223" AND nit = "" AND agencia = 3:
    UPDATE mov_contable WITH WIDTH 300 1 COL.
END.
