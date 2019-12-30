FOR EACH rep_activosFijos WHERE rep_activosFijos.fecCorte = DATE(12,31,ano_corte)
                            AND YEAR(rep_activosFijos.fechaCompra) = ano_corte
                            AND rep_activosFijos.nitProveedor <> ""
                            AND rep_activosFijos.contabilizado = YES NO-LOCK BREAK BY rep_activosFijos.nitProveedor:
    IF FIRST-OF(rep_activosFijos.nitProveedor) THEN DO:
        FIND FIRST F1001 WHERE F1001.nit = rep_activosFijos.nitProveedor
                           AND F1001.concepto = "5008" NO-ERROR.
        IF NOT AVAILABLE F1001 THEN DO:
            CREATE F1001.
            ASSIGN F1001.nit = rep_activosFijos.nitProveedor
                   F1001.concepto = "5008".
    
            FIND FIRST clientes WHERE clientes.nit = rep_activosFijos.nitProveedor NO-LOCK NO-ERROR.
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
    
            F1001.nit = rep_activosFijos.nitProveedor.
    
            IF clientes.tipo_identificacion = "NIT" THEN
                RUN digitoVerificacion.r(INPUT clientes.nit,
                                         OUTPUT F1001.dv) NO-ERROR.
        END.
    END.

    F1001.pagosCostoDeduccion = F1001.pagosCostoDeduccion + rep_activosFijos.valorCompra.
    
    IF LAST-OF(rep_activosFijos.nitProveedor) THEN DO:
        FOR EACH mov_contable WHERE mov_contable.nit = rep_activosFijos.nitProveedor
                                AND YEAR(mov_contable.fec_contable) = ano_corte
                                AND SUBSTRING(mov_contable.cuenta,1,6) = "243540" NO-LOCK:
            FIND FIRST F1001 WHERE F1001.nit = rep_activosFijos.nitProveedor
                               AND F1001.concepto = "5008" NO-ERROR.
            IF NOT AVAILABLE F1001 THEN DO:
                CREATE F1001.
                ASSIGN F1001.nit = rep_activosFijos.nitProveedor
                       F1001.concepto = "5008".
            END.
                
            FIND FIRST ttmov WHERE ttmov.Id = ROWID(mov_contable) NO-LOCK NO-ERROR.
            IF NOT AVAILABLE ttmov THEN DO:
                F1001.ReteFuenteRta = F1001.ReteFuenteRta + mov_contable.cr - mov_contable.db.

                CREATE ttmov.
                ttmov.id = ROWID(mov_contable).
            END.
        END.
    END.
END.
    
FOR EACH F1001 WHERE F1001.concepto = "5008" NO-LOCK:
    totalReporte = totalReporte + F1001.pagosCostoDeduccion.
END.

OUTPUT TO C:\INFO_Fodun\F1001_5008.csv.
FOR EACH F1001 WHERE F1001.pagosCostoDeduccion <> 0 NO-LOCK:
    topeAux = DECIMAL(tope:SCREEN-VALUE IN FRAME F_Ftos).
            
    IF F1001.pagosCostoDeduccion >= topeAux THEN
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

IF AVAILABLE otros1001 THEN
    EXPORT DELIMITER ";" otros1001.

OUTPUT CLOSE.
    
END PROCEDURE.

