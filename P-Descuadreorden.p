
DEFI VAR TCpte LIKE Mov_Contable.Db.
 OUTPUT TO c:\DESC_multi_abrilorden.txt.
FOR EACH Mov_contable WHERE fec_contab GE DATE (04,01,2008)
                        AND fec_contab LE DATE (04,30,2008) NO-LOCK
         BREAK BY Agencia BY comprob BY num_docum:
    IF integer(SUBSTRING(cuenta,1,1)) LT 8 THEN NEXT.
    ASSIGN Tcpte = Tcpte + (Mov_Contable.Db - Mov_Contable.Cr).

    IF LAST-OF(num_docum) THEN DO:
       IF Tcpte NE 0 THEN
          PUT Agencia " " 
              Fec_contab " "
              Comproban  " "
              Num_docum  " "
              tcpte skip.
       Tcpte = 0.
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
