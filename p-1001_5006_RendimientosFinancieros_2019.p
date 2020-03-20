DEFINE INPUT PARAMETER pOrigen AS CHARACTER. /* C - Certificado, E - Exógena */
DEFINE INPUT PARAMETER pNumId AS CHARACTER. /* Sólo re recibe si el origen es 'C' */
DEFINE INPUT PARAMETER pTope AS DECIMAL.
DEFINE OUTPUT PARAMETER pBase AS DECIMAL.
DEFINE OUTPUT PARAMETER pRetencion AS DECIMAL.

DEFINE VAR vNumId_ini AS CHARACTER.
DEFINE VAR vNumId_fin AS CHARACTER.
DEFINE VAR ctaRete AS CHARACTER.

IF pOrigen = 'C' THEN DO:
    vNumId_ini = pNumId.
    vNumId_Fin = pNumId.
END.
ELSE DO:
    vNumId_ini = '0'.
    vNumId_fin = '999999999999999'.
END.

DEFINE TEMP-TABLE F1001
    FIELD concepto AS CHARACTER
    FIELD tipoDoc AS CHARACTER
    FIELD nit AS CHARACTER
    FIELD dv AS CHARACTER
    FIELD apellido1 AS CHARACTER
    FIELD apellido2 AS CHARACTER
    FIELD nombre1 AS CHARACTER
    FIELD nombre2 AS CHARACTER
    FIELD razonSocial AS CHARACTER
    FIELD direccion AS CHARACTER
    FIELD departamento AS CHARACTER
    FIELD municipio AS CHARACTER
    FIELD pais AS CHARACTER
    FIELD pagosCostoDeduccion AS DECIMAL
    FIELD pagosNoCostoNoDeduccion AS DECIMAL
    FIELD IvaMayorValorCostoDeducible AS DECIMAL
    FIELD IvaMayorValorCostoNoDeducible AS DECIMAL
    FIELD ReteFuenteRta AS DECIMAL
    FIELD ReteFuenteRtaAsumida AS DECIMAL
    FIELD ReteFuenteIvaPracticadaRegComun AS DECIMAL
    FIELD ReteFuenteIvaAsumidaRegSimpl AS DECIMAL
    FIELD ReteFuentePracticadaNoDomic AS DECIMAL
    FIELD ajuste AS CHARACTER.

DEFINE TEMP-TABLE otros1001 LIKE F1001.

DEFINE TEMP-TABLE docs
    FIELD nit AS CHARACTER
    FIELD comprobante AS INTEGER
    FIELD agencia AS INTEGER
    FIELD num_documento AS INTEGER
    FIELD fec_contable AS DATE
    INDEX Idx NIT comprobante agencia num_documento fec_contable.

DEFINE VAR cont AS INTEGER.
DEFINE VAR totalContable AS DECIMAL.
DEFINE VAR totalReporte AS DECIMAL.
DEFINE BUFFER bfrMovContable FOR mov_contable.
DEFINE VAR vYear AS INTEGER INITIAL 2019.


FOR EACH anexos WHERE (SUBSTRING(anexos.cuenta,1,8) = "52100505" OR
                       SUBSTRING(anexos.cuenta,1,6) = "615005" OR
                       SUBSTRING(anexos.cuenta,1,6) = "615010" OR
                       SUBSTRING(anexos.cuenta,1,8) = "61501501" OR
                       SUBSTRING(anexos.cuenta,1,6) = "615020" OR
                       SUBSTRING(anexos.cuenta,1,6) = "615035")
                  AND anexos.nit >= vNumId_ini
                  AND anexos.nit <= vNumId_fin
                  AND anexos.ano = vYear NO-LOCK BREAK BY anexos.nit
                                                       BY anexos.cuenta:
    IF FIRST-OF(anexos.nit) THEN DO:
        FIND FIRST F1001 WHERE F1001.nit = anexos.nit
                           AND F1001.concepto = "5006" NO-ERROR.
        IF NOT AVAILABLE F1001 THEN DO:
            CREATE F1001.
            F1001.nit = anexos.nit.
            F1001.concepto = "5006".

            IF pOrigen = 'E' THEN DO:
                FIND FIRST clientes WHERE clientes.nit = anexos.nit NO-LOCK NO-ERROR.
                IF AVAILABLE clientes THEN DO:
                    CASE clientes.tipo_identificacion:
                        WHEN "R.C" THEN F1001.tipoDoc = "11".
                        WHEN "T.I" THEN F1001.tipoDoc = "12".
                        WHEN "C.C" THEN F1001.tipoDoc = "13".
                        WHEN "T.E" THEN F1001.tipoDoc = "21".
                        WHEN "C.E" THEN F1001.tipoDoc = "22".
                        WHEN "NIT" THEN F1001.tipoDoc = "31".
                        WHEN "PPTE" THEN F1001.tipoDoc = "41".
                    END CASE.
    
                    IF clientes.tipo_identificacion <> "NIT" THEN DO:
                        F1001.apellido1 = clientes.apellido1.
                        F1001.apellido2 = clientes.apellido2.
    
                        IF INDEX(clientes.nombre," ") > 0 THEN DO:
                            F1001.nombre1 = SUBSTRING(clientes.nombre,1,INDEX(clientes.nombre," ") - 1).
                            F1001.nombre2 = SUBSTRING(clientes.nombre,INDEX(clientes.nombre," ") + 1).
                        END.
                        ELSE
                            F1001.nombre1 = clientes.nombre.
                    END.
                    ELSE
                        F1001.razonSocial = clientes.nombre.
    
                    IF clientes.DIR_residencia <> "" THEN
                        F1001.direccion = clientes.DIR_residencia.
                    ELSE
                        F1001.direccion = clientes.DIR_comercial.

                    IF F1001.direccion = "" THEN DO:
                        FIND FIRST agencias WHERE agencias.agencia = clientes.agencia NO-LOCK NO-ERROR.
                        IF AVAILABLE agencias THEN
                            F1001.direccion = agencias.direccion.
                    END.
    
                    IF clientes.lugar_residencia <> "" THEN DO:
                        F1001.departamento = SUBSTRING(clientes.lugar_residencia,1,2).
                        F1001.municipio = SUBSTRING(clientes.lugar_residencia,3,3).
                    END.
                    ELSE DO:
                        F1001.departamento = SUBSTRING(clientes.lugar_comercial,1,2).
                        F1001.municipio = SUBSTRING(clientes.lugar_comercial,3,3).
                    END.

                    IF F1001.departamento = "" OR F1001.departamento = ? THEN DO:
                        CASE clientes.agencia:
                            WHEN 1 THEN DO:
                                F1001.departamento = "11".
                                F1001.municipio = "001".
                            END.

                            WHEN 2 THEN DO:
                                F1001.departamento = "05".
                                F1001.municipio = "001".
                            END.

                            WHEN 3 THEN DO:
                                F1001.departamento = "17".
                                F1001.municipio = "001".
                            END.

                            WHEN 4 THEN DO:
                                F1001.departamento = "76".
                                F1001.municipio = "250".
                            END.

                            OTHERWISE DO:
                                F1001.departamento = "11".
                                F1001.municipio = "001".
                            END.
                        END CASE.
                    END.

                    F1001.pais = "169".
                END.
    
                IF clientes.tipo_identificacion = "NIT" THEN
                    RUN digitoVerificacion.r(INPUT clientes.nit,
                                             OUTPUT F1001.dv) NO-ERROR.
            END.
        END.

        EMPTY TEMP-TABLE docs.
    END.

    F1001.pagosCostoDeduccion = F1001.pagosCostoDeduccion + anexos.sdo_inicial.
        
    DO cont = 1 TO 12:
        F1001.pagosCostoDeduccion = F1001.pagosCostoDeduccion + anexos.db[cont] - anexos.cr[cont].
    END.

    IF LAST-OF(anexos.cuenta) THEN DO:
        IF SUBSTRING(anexos.cuenta,1,8) = "52100505" THEN
            ctaRete = "243525".
        ELSE
            ctaRete = "243535".

        FOR EACH mov_contable WHERE mov_contable.cuenta = anexos.cuenta
                                AND mov_contable.nit = anexos.nit
                                AND YEAR(mov_contable.fec_contable) = vYear NO-LOCK:
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
                                          AND SUBSTRING(bfrMovContable.cuenta,1,6) = ctaRete NO-LOCK:
                    F1001.ReteFuenteRta = F1001.ReteFuenteRta + bfrMovContable.cr - bfrMovContable.db.
                END.
            END.
        END.

        F1001.ReteFuenteIvaAsumidaRegSimpl = ROUND((F1001.ReteFuenteRta * 96.5) / 100,0).
    END.

    IF pOrigen = 'C' THEN DO:
        pBase = F1001.pagosCostoDeduccion.
        pRetencion = F1001.ReteFuenteRta.
    END.
END.


IF pOrigen = "E" THEN DO:
    FOR EACH F1001 WHERE F1001.concepto = "5006" NO-LOCK:
        totalReporte = totalReporte + F1001.pagosCostoDeduccion.
    END.

    FOR EACH sal_cuenta WHERE (SUBSTRING(sal_cuenta.cuenta,1,8) = "52100505" OR
                               SUBSTRING(sal_cuenta.cuenta,1,6) = "615005" OR
                               SUBSTRING(sal_cuenta.cuenta,1,6) = "615010" OR
                               SUBSTRING(sal_cuenta.cuenta,1,8) = "61501501" OR
                               SUBSTRING(sal_cuenta.cuenta,1,6) = "615020" OR
                               SUBSTRING(sal_cuenta.cuenta,1,6) = "615035")
                          AND sal_cuenta.ano = vYear NO-LOCK:
        totalContable = totalContable + sal_cuenta.sal_inicial.

        DO cont = 1 TO 12:
            totalContable = totalContable + sal_cuenta.db[cont] - sal_cuenta.cr[cont].
        END.
    END.

    IF totalContable - totalReporte <> 0 THEN DO:
        CREATE otros1001.
        otros1001.concepto = "5006".
        otros1001.nit = "222222222".
        otros1001.tipoDoc = "43".
        otros1001.pagosCostoDeduccion = totalContable - totalReporte.
    END.

    OUTPUT TO VALUE ("C:\INFO_Fodun\F1001_5006-" + STRING(vYear) + ".csv").
        FOR EACH F1001 WHERE F1001.pagosCostoDeduccion <> 0 NO-LOCK:
            IF F1001.pagosCostoDeduccion >= pTope THEN
                EXPORT DELIMITER ";" F1001.
            ELSE DO:
                FIND FIRST otros1001 WHERE otros1001.concepto = F1001.concepto NO-ERROR.
                IF NOT AVAILABLE otros1001 THEN DO:
                    CREATE otros1001.
                    otros1001.concepto = F1001.concepto.
                    otros1001.nit = "222222222".
                    otros1001.tipoDoc = "43".
                END.

                otros1001.pagosCostoDeduccion = otros1001.pagosCostoDeduccion + F1001.pagosCostoDeduccion.
                otros1001.ReteFuenteRta = otros1001.ReteFuenteRta + F1001.ReteFuenteRta.
                otros1001.ReteFuenteIvaAsumidaRegSimpl = otros1001.ReteFuenteIvaAsumidaRegSimpl + F1001.ReteFuenteIvaAsumidaRegSimpl.
            END.
        END.

        IF AVAILABLE otros1001 THEN DO:
            IF otros1001.pagosCostoDeduccion >= 0 AND otros1001.ReteFuenteRta >= 0 THEN
                EXPORT DELIMITER ";" otros1001.
        END.
    
    OUTPUT CLOSE.
END.

