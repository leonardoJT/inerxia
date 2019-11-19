DEFINE VAR pagosCostoDeduccion AS DECIMAL.
DEFINE TEMP-TABLE docs LIKE mov_contable.
DEFINE BUFFER bfrMovContable FOR mov_contable.
DEFINE VAR ano_corte AS INTEGER INITIAL 2015.
DEFINE VAR cont AS INTEGER.
DEFINE VAR ReteFuenteRta AS DECIMAL.

FOR EACH anexos13 WHERE (SUBSTRING(anexos13.cuenta,1,6) = "271015" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "510506" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "510515" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "510521" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "510527" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "510536" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "510539" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "510548" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "510560" OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6140101206" OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6140101215" OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6140101227" OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6140101236" OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6140101239" OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6140101248" OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6140101260" OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6140102206" OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6140102215" OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6140102236" OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6140102239" OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6170401206" OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6170401215" OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6170401227" OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6170401236" OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6170401239" OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6170401248")
                    AND anexos13.ano = ano_corte NO-LOCK BREAK BY anexos13.nit
                                                               BY anexos13.cuenta:
    IF SUBSTRING(anexos13.cuenta,1,6) = "271015" THEN DO:
        pagosCostoDeduccion = pagosCostoDeduccion + anexos13.db[1] - anexos13.cr[1].

        FOR EACH mov_contable WHERE mov_contable.nit = anexos13.nit
                                AND YEAR(mov_contable.fec_contable) = ano_corte
                                AND MONTH(mov_contable.fec_contable) = 1
                                AND mov_contable.cuenta = anexos13.cuenta
                                AND mov_contable.comprobante = 9
                                AND mov_contable.cr > 0 NO-LOCK:
            pagosCostoDeduccion = pagosCostoDeduccion - mov_contable.cr.
        END.
    END.
    ELSE DO:
        pagosCostoDeduccion = pagosCostoDeduccion + anexos13.sdo_inicial.

        DO cont = 1 TO 12:
            pagosCostoDeduccion = pagosCostoDeduccion + anexos13.db[cont] - anexos13.cr[cont].
        END.
    END.

    IF LAST-OF(anexos13.cuenta) THEN DO:
        FOR EACH mov_contable WHERE mov_contable.cuenta = anexos13.cuenta
                                AND mov_contable.nit = anexos13.nit
                                AND YEAR(mov_contable.fec_contable) = ano_corte NO-LOCK BREAK:
            FIND FIRST docs WHERE docs.nit = mov_contable.nit
                              AND docs.comprobante = mov_contable.comprobante
                              AND docs.agencia = mov_contable.agencia
                              AND docs.num_documento = mov_contable.num_documento
                              AND docs.fec_contable = mov_contable.fec_contable NO-LOCK NO-ERROR.
            IF NOT AVAILABLE docs THEN DO:
                CREATE docs.
                docs.nit = mov_contable.nit.
                docs.agencia = mov_contable.agencia.
                docs.comprobante = mov_contable.comprobante.
                docs.num_documento = mov_contable.num_documento.
                docs.fec_contable = mov_contable.fec_contable.

                FOR EACH bfrMovContable WHERE bfrMovContable.agencia = docs.agencia
                                          AND bfrMovContable.comprobante = docs.comprobante
                                          AND bfrMovContable.num_documento = docs.num_documento
                                          AND bfrMovContable.nit = docs.nit
                                          AND bfrMovContable.fec_contable = docs.fec_contable
                                          AND (SUBSTRING(bfrMovContable.cuenta,1,4) = "2445")
                                          AND bfrMovContable.comentario <> "Cierre Anexos Anual" NO-LOCK:
                    ReteFuenteRta = ReteFuenteRta + bfrMovContable.cr - bfrMovContable.db.
                END.
            END.
        END.
    END.
END.

MESSAGE pagosCostoDeduccion ReteFuenteRta
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
