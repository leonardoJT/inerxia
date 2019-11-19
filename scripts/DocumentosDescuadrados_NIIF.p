DEFI VAR TCpte LIKE Mov_Contable.Db.

OUTPUT TO D:\Leonardo\Descuadres_NIIF.txt.
    FOR EACH Mov_contable_NIIF WHERE agencia = 1
                            AND fec_contab >= 05/01/2017
                            AND fec_contab <= 05/31/2017 NO-LOCK BREAK BY Agencia BY comprob BY num_docum:
        ASSIGN Tcpte = Tcpte + (Mov_Contable_NIIF.Db - Mov_Contable_NIIF.Cr).

        IF LAST-OF(num_docum) THEN DO:
            IF Tcpte NE 0 THEN
                PUT Agencia " "
                    Fec_contab " "
                    Comproban  " "
                    Num_docum  " "
                    mov_contable_NIIF.comentario " "
                    tcpte SKIP.

            Tcpte = 0.
        END.
    END.
OUTPUT CLOSE.

MESSAGE "Finaliza revisión de descuadres"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

/*
indices tabla mov_contable
idx_mov1
    comprobante
    fec_contable
    agencia
    num_documento
*/
