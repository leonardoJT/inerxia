DEFI VAR Tdb LIKE Mov_Contable.Db .
DEFI VAR tcr LIKE mov_contable.cr.
OUTPUT TO c:\DESC_multi_ctaabril.txt.
FOR EACH Mov_contable WHERE fec_contab GE DATE (04,01,2008)
                        AND fec_contab LE DATE (04,30,2008) NO-LOCK
         BREAK BY cuenta:
    ASSIGN tdb = tdb + db
           tcr = tcr + cr.

    IF LAST-OF(num_docum) THEN DO:
       IF Tcpte NE 0 THEN
          PUT Agencia " " 
              cuenta " "
              tdb   " "
              tcr  skip.
       ASSIGN tdb = 0
              tcr = 0.
    END.
END.
OUTPUT CLOSE.

/*
indices tabla mov_contable
idx_mov1
    comprobante
    fec_contable
    agencia
    num_documento
*/
