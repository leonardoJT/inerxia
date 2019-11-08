DEFI VAR TCpte LIKE Mov_Contable.Db.

OUTPUT TO D:\Leonardo\Descuadres.txt.
    FOR EACH Mov_contable WHERE agencia = 4
                            AND fec_contab >= 06/01/2017
                            AND fec_contab <= 06/30/2017 NO-LOCK BREAK BY Agencia BY comprob BY num_docum:
        ASSIGN Tcpte = Tcpte + (Mov_Contable.Db - Mov_Contable.Cr).

        IF LAST-OF(num_docum) THEN DO:
            IF Tcpte NE 0 THEN
                PUT Agencia " "
                    Fec_contab " "
                    Comproban  " "
                    Num_docum  " "
                    tcpte SKIP.

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
