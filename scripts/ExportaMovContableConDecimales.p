OUTPUT TO d:\mov_contable.txt.

FOR EACH mov_contable WHERE TRUNCATE(db,0) <> db OR TRUNCATE(cr,0) <> cr NO-LOCK BY fec_contable
                                                                                 BY agencia:
    DISPLAY agencia fec_contable cuenta comentario FORMAT "X(30)" num_documento DECIMAL(db) FORMAT ">>>,>>>,>>>,>>9.99" DECIMAL(cr) FORMAT ">>>,>>>,>>>,>>9.99" WITH WIDTH 300.
END.
