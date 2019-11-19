DEFINE VAR cont AS INTEGER.
DEFINE VAR totalContable AS DECIMAL.
DEFINE VAR totalReporte AS DECIMAL.
DEFINE VAR ano_corte AS INTEGER INITIAL 2012.
DEFINE VAR suma AS DECIMAL.


DEFINE TEMP-TABLE docs
    FIELD comprobante AS INTEGER
    FIELD agencia AS INTEGER
    FIELD nit AS CHARACTER
    FIELD num_documento AS INTEGER.

DEFINE VAR nitTemporal AS CHARACTER.

DEFINE TEMP-TABLE F1008
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
    FIELD valorSaldoCreditoActivo AS DECIMAL.

EMPTY TEMP-TABLE F1008.

DEFINE TEMP-TABLE creditos12
    FIELD nit AS CHARACTER
    FIELD cod_credito AS INTEGER
    FIELD sdo_capital AS DECIMAL
    FIELD INT_corriente AS DECIMAL
    FIELD INT_morCobrar AS DECIMAL
    FIELD provision AS DECIMAL.

EMPTY TEMP-TABLE creditos12.

INPUT FROM c:\INFO_fodun\leonardo\creditosEndYear.csv.
REPEAT:
    CREATE creditos12.
    IMPORT DELIMITER ";" creditos12.
END.
INPUT CLOSE.


/*FOR EACH anexos13 WHERE (/*SUBSTRING(anexos13.cuenta,1,4) = "1625" OR*/
                         /*SUBSTRING(anexos13.cuenta,1,4) = "1635" OR*/
                         /*SUBSTRING(anexos13.cuenta,1,4) = "1650" OR*/
                         /*SUBSTRING(anexos13.cuenta,1,4) = "1660" OR*/
                         SUBSTRING(anexos13.cuenta,1,4) = "1690")
                    AND anexos13.ano = ano_corte NO-LOCK BREAK BY anexos13.nit
                                                               BY anexos13.cuenta:
    IF FIRST-OF(anexos13.nit) THEN DO:
        FIND FIRST F1008 WHERE F1008.nit = anexos13.nit
                           AND F1008.concepto = "1317" NO-ERROR.
        IF NOT AVAILABLE F1008 THEN DO:
            IF INDEX(anexos13.nit,"-") > 0 THEN
                FIND FIRST F1008 WHERE F1008.nit = SUBSTRING(anexos13.nit,1,INDEX(anexos13.nit,"-") - 1) NO-LOCK NO-ERROR.

            IF NOT AVAILABLE F1008 THEN DO:
                CREATE F1008.
                ASSIGN F1008.nit = anexos13.nit
                       F1008.concepto = "1317".
            
                FIND FIRST clientes WHERE clientes.nit = anexos13.nit NO-LOCK NO-ERROR.
                IF AVAILABLE clientes THEN DO:
                    CASE clientes.tipo_identificacion:
                        WHEN "R.C" THEN F1008.tipoDoc = "11".
                        WHEN "T.I" THEN F1008.tipoDoc = "12".
                        WHEN "C.C" THEN F1008.tipoDoc = "13".
                        WHEN "T.E" THEN F1008.tipoDoc = "21".
                        WHEN "C.E" THEN F1008.tipoDoc = "22".
                        WHEN "NIT" THEN F1008.tipoDoc = "31".
                        WHEN "PPTE" THEN F1008.tipoDoc = "41".
                    END CASE.
    
                    IF clientes.tipo_identificacion <> "NIT" THEN DO:
                        F1008.apellido1 = clientes.apellido1.
                        F1008.apellido2 = clientes.apellido2.
        
                        IF INDEX(clientes.nombre," ") > 0 THEN DO:
                            F1008.nombre1 = SUBSTRING(clientes.nombre,1,INDEX(clientes.nombre," ") - 1).
                            F1008.nombre2 = SUBSTRING(clientes.nombre,INDEX(clientes.nombre," ") + 1).
                        END.
                        ELSE
                            F1008.nombre1 = clientes.nombre.
                    END.
                    ELSE
                        F1008.razonSocial = clientes.nombre.
    
                    IF clientes.DIR_residencia <> "" THEN
                        F1008.direccion = clientes.DIR_residencia.
                    ELSE
                        F1008.direccion = clientes.DIR_comercial.
    
                    IF clientes.lugar_residencia <> "" THEN DO:
                        F1008.departamento = SUBSTRING(clientes.lugar_residencia,1,2).
                        F1008.municipio = SUBSTRING(clientes.lugar_residencia,3,3).
                    END.
                    ELSE DO:
                        F1008.departamento = SUBSTRING(clientes.lugar_comercial,1,2).
                        F1008.municipio = SUBSTRING(clientes.lugar_comercial,3,3).
                    END.
    
                    F1008.pais = "169".
                END.
    
                F1008.nit = anexos13.nit.
    
                IF INDEX(anexos13.nit,"-") > 0 THEN DO:
                    F1008.dv = SUBSTRING(anexos13.nit,INDEX(anexos13.nit,"-") + 1,1).
                    F1008.nit = SUBSTRING(anexos13.nit,1,INDEX(anexos13.nit,"-") - 1).
                END.
            END.
        END.
    END.

    F1008.valorSaldoCreditoActivo = F1008.valorSaldoCreditoActivo + anexos13.sdo_inicial.
        
    DO cont = 1 TO 12:
        F1008.valorSaldoCreditoActivo = F1008.valorSaldoCreditoActivo + anexos13.db[cont] - anexos13.cr[cont].
    END.
END.*/

FOR EACH creditos12 WHERE creditos12.cod_credito = 62 NO-LOCK BREAK BY creditos12.nit:
    suma = suma + creditos12.sdo_capital + creditos12.int_corriente + creditos12.int_morCobrar.

    IF LAST-OF(creditos12.nit) THEN DO:
        FIND FIRST F1008 WHERE F1008.nit = creditos12.nit
                           AND F1008.concepto = "1317" NO-LOCK NO-ERROR.
        IF NOT AVAILABLE F1008 THEN DO:
            CREATE F1008.
            ASSIGN F1008.nit = creditos12.nit
                   F1008.concepto = "1317".

            FIND FIRST clientes WHERE clientes.nit = creditos12.nit NO-LOCK NO-ERROR.
            IF AVAILABLE clientes THEN DO:
                CASE clientes.tipo_identificacion:
                    WHEN "R.C" THEN F1008.tipoDoc = "11".
                    WHEN "T.I" THEN F1008.tipoDoc = "12".
                    WHEN "C.C" THEN F1008.tipoDoc = "13".
                    WHEN "T.E" THEN F1008.tipoDoc = "21".
                    WHEN "C.E" THEN F1008.tipoDoc = "22".
                    WHEN "NIT" THEN F1008.tipoDoc = "31".
                    WHEN "PPTE" THEN F1008.tipoDoc = "41".
                END CASE.

                IF clientes.tipo_identificacion <> "NIT" THEN DO:
                    F1008.apellido1 = clientes.apellido1.
                    F1008.apellido2 = clientes.apellido2.

                    IF INDEX(clientes.nombre," ") > 0 THEN DO:
                        F1008.nombre1 = SUBSTRING(clientes.nombre,1,INDEX(clientes.nombre," ") - 1).
                        F1008.nombre2 = SUBSTRING(clientes.nombre,INDEX(clientes.nombre," ") + 1).
                    END.
                    ELSE
                        F1008.nombre1 = clientes.nombre.
                END.
                ELSE
                    F1008.razonSocial = clientes.nombre.

                IF clientes.DIR_residencia <> "" THEN
                    F1008.direccion = clientes.DIR_residencia.
                ELSE
                    F1008.direccion = clientes.DIR_comercial.

                IF clientes.lugar_residencia <> "" THEN DO:
                    F1008.departamento = SUBSTRING(clientes.lugar_residencia,1,2).
                    F1008.municipio = SUBSTRING(clientes.lugar_residencia,3,3).
                END.
                ELSE DO:
                    F1008.departamento = SUBSTRING(clientes.lugar_comercial,1,2).
                    F1008.municipio = SUBSTRING(clientes.lugar_comercial,3,3).
                END.

                F1008.pais = "169".
            END.

            F1008.nit = creditos12.nit.

            IF INDEX(creditos12.nit,"-") > 0 THEN DO:
                F1008.dv = SUBSTRING(creditos12.nit,INDEX(creditos12.nit,"-") + 1,1).
                F1008.nit = SUBSTRING(creditos12.nit,1,INDEX(creditos12.nit,"-") - 1).
            END.
        END.

        F1008.valorSaldoCreditoActivo = suma.

        suma = 0.
    END.
END.

FOR EACH F1008 WHERE F1008.concepto = "1317" NO-LOCK:
    totalReporte = totalReporte + F1008.valorSaldoCreditoActivo.
END.

FOR EACH sal_cuenta13 WHERE (/*SUBSTRING(sal_cuenta13.cuenta,1,4) = "1625" OR*/
                             /*SUBSTRING(sal_cuenta13.cuenta,1,4) = "1635" OR*/
                             /*SUBSTRING(sal_cuenta13.cuenta,1,4) = "1650" OR*/
                             /*SUBSTRING(sal_cuenta13.cuenta,1,4) = "1660" OR*/
                             /*SUBSTRING(sal_cuenta13.cuenta,1,4) = "1690" OR*/
                             SUBSTRING(sal_cuenta13.cuenta,1,4) = "1640")
                        AND sal_cuenta13.ano = ano_corte NO-LOCK:
    totalContable = totalContable + sal_cuenta13.sal_inicial.

    DO cont = 1 TO 12:
        totalContable = totalContable + sal_cuenta13.db[cont] - sal_cuenta13.cr[cont].
    END.
END.

MESSAGE totalReporte totalContable totalReporte - totalContable 
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
