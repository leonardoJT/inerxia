DEFINE VAR TCpte LIKE Mov_Contable.Db.

OUTPUT TO D:\Leonardo\Descuadres_1.txt.
    FOR EACH Mov_contable WHERE agencia = 1
                            AND fec_contab >= 01/01/2020
                            AND fec_contab <= 01/31/2020
                            AND SUBSTRING(cuenta,1,1) <> "8"
                            AND SUBSTRING(cuenta,1,1) <> "9" NO-LOCK BREAK BY Agencia BY comprob BY num_docum:
        ASSIGN Tcpte = Tcpte + (Mov_Contable.Db - Mov_Contable.Cr).

        IF LAST-OF(num_docum) THEN DO:
            IF Tcpte NE 0 THEN
                PUT Agencia " "
                    Fec_contab " "
                    Comproban  " "
                    Num_docum  " "
                    mov_contable.comentario " "
                    tcpte SKIP.

            Tcpte = 0.
        END.
    END.
OUTPUT CLOSE.

MESSAGE "Finaliza revisi�n de descuadres"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

/*
indices tabla mov_contable
idx_mov1
    comprobante
    fec_contable
    agencia
    num_documento
*/
