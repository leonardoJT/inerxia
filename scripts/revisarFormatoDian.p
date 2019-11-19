DEFINE VAR cont AS INTEGER.
DEFINE VAR totalReporte AS DECIMAL.
DEFINE VAR totalContable AS DECIMAL.
DEFINE BUFFER bfrMovContable FOR mov_contable.
DEFINE VAR topeAux AS DECIMAL.
DEFINE VAR ctaRete AS CHARACTER.
DEFINE VAR pagos AS DECIMAL.
DEFINE VAR rete AS DECIMAL.
DEFINE VAR ano_corte AS INTEGER INITIAL 2017.

DEFINE TEMP-TABLE docs
    FIELD nit AS CHARACTER
    FIELD comprobante AS INTEGER
    FIELD agencia AS INTEGER
    FIELD num_documento AS INTEGER
    FIELD fec_contable AS DATE
    INDEX Idx NIT comprobante agencia num_documento fec_contable.

OUTPUT TO d:\leonardo\revisionDIAN.csv.

FOR EACH anexos WHERE (SUBSTRING(anexos.cuenta,1,6) = "510503" OR
                       SUBSTRING(anexos.cuenta,1,6) = "510505" OR
                       SUBSTRING(anexos.cuenta,1,6) = "510509" OR
                       SUBSTRING(anexos.cuenta,1,6) = "510510" OR
                       SUBSTRING(anexos.cuenta,1,6) = "510511" OR
                       SUBSTRING(anexos.cuenta,1,6) = "510512" OR
                       SUBSTRING(anexos.cuenta,1,6) = "510516" OR
                       SUBSTRING(anexos.cuenta,1,6) = "510519" OR
                       SUBSTRING(anexos.cuenta,1,6) = "510520" OR
                       SUBSTRING(anexos.cuenta,1,10) = "6140101206" OR
                       SUBSTRING(anexos.cuenta,1,10) = "6140101215" OR
                       SUBSTRING(anexos.cuenta,1,10) = "6140101227" OR
                       SUBSTRING(anexos.cuenta,1,10) = "6140101230" OR
                       SUBSTRING(anexos.cuenta,1,10) = "6140101233" OR
                       SUBSTRING(anexos.cuenta,1,10) = "6140101236" OR
                       SUBSTRING(anexos.cuenta,1,10) = "6140101239" OR
                       SUBSTRING(anexos.cuenta,1,10) = "6140101248" OR
                       SUBSTRING(anexos.cuenta,1,10) = "6140101260")
                  /*AND anexos.nit = "19393345"*/
                  AND anexos.ano = ano_corte NO-LOCK BREAK BY anexos.nit
                                                           BY anexos.cuenta:

    pagos = pagos + anexos.sdo_inicial.

    DO cont = 1 TO 12:
        pagos = pagos + anexos.db[cont] - anexos.cr[cont].
    END.
    
    IF LAST-OF(anexos.cuenta) THEN DO:
        FOR EACH mov_contable WHERE mov_contable.cuenta = anexos.cuenta
                                AND mov_contable.nit = anexos.nit
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
                                          AND SUBSTRING(bfrMovContable.cuenta,1,4) = "2435"
                                          AND SUBSTRING(bfrMovContable.cuenta,1,6) <> "243505" NO-LOCK:
                    EXPORT DELIMITER ";"
                        mov_contable.agencia
                        mov_contable.fec_contable
                        mov_contable.comprobante
                        mov_contable.num_documento
                        mov_contable.cuenta
                        mov_contable.nit
                        mov_contable.comentario
                        mov_contable.db
                        mov_contable.cr.
                END.
            END.
        END.
    END.
END.

OUTPUT CLOSE.
