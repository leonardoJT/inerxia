OUTPUT TO d:\leonardo\docs14.csv.
FOR EACH mov_contable WHERE agencia = 1
    AND SUBSTRING(cuenta,1,3) = "144"
    AND fec_contable >= 10/01/2017
    AND comentario <> "Int.Corriente x Caja"
    AND comentario <> "Int.Mor.Cobrar x Nomina"
    AND comentario <> "Int.Corriente x Nomina"
    AND comentario <> "Int.Mor.Cobrar x Caja"
    AND comentario <> "Abono para Int-Corriente"
    AND comentario <> "Abono para Capital"
    AND comentario <> "Abono para Int-X-Mora"
    AND comentario <> "Reclasif-Contingente"
    AND INDEX(comentario,"Desembolso del Credito Número:") = 0
    AND comprobante <> 20
    AND comprobante <> 22 NO-LOCK BY fec_contable:
    EXPORT DELIMITER ";"
        fec_contable
        comprobante
        num_documento
        cuenta
        nit
        comentario
        db
        cr.
END.
OUTPUT CLOSE.
