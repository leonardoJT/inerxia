DEFINE VAR codAhorro AS INTEGER.
DEFINE VAR pPorcentaje AS DECIMAL.
DEFINE VAR cont AS INTEGER.
DEFINE VAR baseCalculada AS DECIMAL.
DEFINE VAR ICA_Tarifa AS DECIMAL.
DEFINE BUFFER bfr_mov_contable FOR mov_contable.

DEFINE TEMP-TABLE TCer
    FIELD nit AS CHARACTER
    FIELD baseGMF AS DECIMAL
    FIELD retGMF AS DECIMAL
    FIELD baseICA AS DECIMAL
    FIELD retICA AS DECIMAL
    FIELD baseRend AS DECIMAL
    FIELD retRend AS DECIMAL
    FIELD baseHon AS DECIMAL
    FIELD retHon AS DECIMAL
    FIELD baseServ AS DECIMAL
    FIELD retServ AS DECIMAL
    FIELD baseComp AS DECIMAL
    FIELD retComp AS DECIMAL
    FIELD baseOtr AS DECIMAL
    FIELD retOtr AS DECIMAL.

DEFINE TEMP-TABLE docs
    FIELD nit AS CHARACTER
    FIELD agencia AS INTEGER
    FIELD comprobante AS INTEGER
    FIELD num_documento AS INTEGER.


FOR EACH clientes WHERE LENGTH(clientes.nit) >= 5 NO-LOCK:
    /* Gravamen a los movientos financieros - ICA */
    FOR EACH Cuentas WHERE SUBSTRING(Cuentas.Cuenta,1,4) = "2442" NO-LOCK:
        FIND FIRST TCer WHERE INDEX(clientes.nit,TCer.nit) = 1 NO-ERROR.
        IF NOT AVAILABLE TCer THEN DO:
            CREATE TCer.
            TCer.Nit = clientes.Nit.
        END.

        RUN ProcesoAnexos (OUTPUT tCer.retGMF).

        tCer.baseGMF = (tCer.RetGMF * 1000) / 4.
    END.
    /* ------------------------------------ */

    /* ICA */
    FOR EACH anexos13 WHERE anexos13.nit = clientes.nit
                        AND (SUBSTRING(anexos13.cuenta,1,4) = "5110" OR
                             SUBSTRING(anexos13.cuenta,1,8) = "61401011" OR
                             SUBSTRING(anexos13.cuenta,1,8) = "61704011")
                        AND anexos13.ano = 2014 NO-LOCK BREAK BY anexos13.nit
                                                              BY anexos13.cuenta:
        FIND FIRST cuentas WHERE cuentas.cuenta = anexos13.cuenta NO-LOCK NO-ERROR.
        
        FIND FIRST TCer WHERE INDEX(clientes.nit,TCer.nit) = 1 NO-ERROR.
        IF NOT AVAILABLE TCer THEN DO:
            CREATE TCer.
            TCer.Nit = clientes.Nit.
        END.

        TCer.BaseICA = TCer.BaseICA + anexos13.sdo_inicial.

        DO cont = 1 TO 12:
            TCer.BaseICA = TCer.BaseICA + anexos13.db[cont] - anexos13.cr[cont].
        END.
    END.

    FOR EACH mov_contable WHERE mov_contable.nit = clientes.nit
                            AND YEAR(mov_contable.fec_contable) = 2014
                            AND SUBSTRING(mov_contable.cuenta,1,4) = "2448"
                            AND mov_contable.comentario <> "Cierre Anexos Anual" NO-LOCK:
        FIND FIRST TCer WHERE INDEX(clientes.nit,TCer.nit) = 1 NO-ERROR.
        IF AVAILABLE tCer THEN
            TCer.RetICA = TCer.RetICA + mov_contable.cr - mov_contable.db.
    END.
    /* ------------------------ */
    
    
    /* Rendimientos Financieros */
    FOR EACH anexos13 WHERE anexos13.nit = clientes.nit
                        AND (SUBSTRING(anexos13.cuenta,1,6) = "617505" OR
                             SUBSTRING(anexos13.cuenta,1,6) = "617510" OR
                             SUBSTRING(anexos13.cuenta,1,6) = "617520")
                        AND anexos13.ano = 2014 NO-LOCK BREAK BY anexos13.nit
                                                              BY anexos13.cuenta:
        FIND FIRST cuentas WHERE cuentas.cuenta = anexos13.cuenta NO-LOCK NO-ERROR.
        
        FIND FIRST TCer WHERE INDEX(clientes.nit,TCer.nit) = 1 NO-ERROR.
        IF NOT AVAILABLE TCer THEN DO:
            CREATE TCer.
            TCer.Nit = clientes.Nit.
        END.

        TCer.BaseRend = TCer.BaseRend + anexos13.sdo_inicial.

        DO cont = 1 TO 12:
            TCer.BaseRend = TCer.BaseRend + anexos13.db[cont] - anexos13.cr[cont].
        END.

        IF LAST-OF(anexos13.cuenta) THEN DO:
            FOR EACH mov_contable WHERE mov_contable.cuenta = anexos13.cuenta
                                    AND mov_contable.nit = anexos13.nit
                                    AND YEAR(mov_contable.fec_contable) = 2014 NO-LOCK BREAK:
                FIND FIRST docs WHERE docs.nit = mov_contable.nit
                                  AND docs.comprobante = mov_contable.comprobante
                                  AND docs.agencia = mov_contable.agencia
                                  AND docs.num_documento = mov_contable.num_documento NO-LOCK NO-ERROR.
                IF NOT AVAILABLE docs THEN DO:
                    CREATE docs.
                    docs.nit = mov_contable.nit.
                    docs.agencia = mov_contable.agencia.
                    docs.comprobante = mov_contable.comprobante.
                    docs.num_documento = mov_contable.num_documento.

                    FOR EACH bfr_mov_contable WHERE bfr_mov_contable.agencia = docs.agencia
                                                AND bfr_mov_contable.comprobante = docs.comprobante
                                                AND bfr_mov_contable.num_documento = docs.num_documento
                                                AND bfr_mov_contable.nit = docs.nit
                                                AND YEAR(bfr_mov_contable.fec_contable) = 2014
                                                AND SUBSTRING(bfr_mov_contable.cuenta,1,6) = "244535"
                                                AND bfr_mov_contable.comentario <> "Cierre Anexos Anual" NO-LOCK:
                        TCer.RetRend = TCer.RetRend + bfr_mov_contable.cr - bfr_mov_contable.db.
                    END.
                END.
            END.
        END.
    END.
    /* ------------------------ */

    /* Honorarios */
    FOR EACH anexos13 WHERE anexos13.nit = clientes.nit
                        AND (SUBSTRING(anexos13.cuenta,1,4) = "5110" OR
                             SUBSTRING(anexos13.cuenta,1,8) = "61401011" OR
                             SUBSTRING(anexos13.cuenta,1,8) = "61704011")
                        AND anexos13.ano = 2014 NO-LOCK BREAK BY anexos13.nit
                                                              BY anexos13.cuenta:
        FIND FIRST cuentas WHERE cuentas.cuenta = anexos13.cuenta NO-LOCK NO-ERROR.
        
        FIND FIRST TCer WHERE INDEX(clientes.nit,TCer.nit) = 1 NO-ERROR.
        IF NOT AVAILABLE TCer THEN DO:
            CREATE TCer.
            TCer.Nit = clientes.Nit.
        END.

        TCer.BaseHon = TCer.BaseHon + anexos13.sdo_inicial.

        DO cont = 1 TO 12:
            TCer.BaseHon = TCer.BaseHon + anexos13.db[cont] - anexos13.cr[cont].
        END.
    END.

    FOR EACH mov_contable WHERE mov_contable.nit = clientes.nit
                            AND YEAR(mov_contable.fec_contable) = 2014
                            AND SUBSTRING(mov_contable.cuenta,1,6) = "244515"
                            AND mov_contable.comentario <> "Cierre Anexos Anual" NO-LOCK:
        FIND FIRST TCer WHERE INDEX(clientes.nit,TCer.nit) = 1 NO-ERROR.
        IF AVAILABLE tCer THEN
            TCer.RetHon = TCer.RetHon + mov_contable.cr - mov_contable.db.
    END.
    /* ------------------------ */

    /* Servicios */
    FOR EACH anexos13 WHERE anexos13.nit = clientes.nit
                        AND (SUBSTRING(anexos13.cuenta,1,4) = "5110" OR
                             SUBSTRING(anexos13.cuenta,1,8) = "61401011" OR
                             SUBSTRING(anexos13.cuenta,1,8) = "61704011")
                        AND anexos13.ano = 2014 NO-LOCK BREAK BY anexos13.nit
                                                              BY anexos13.cuenta:
        FIND FIRST cuentas WHERE cuentas.cuenta = anexos13.cuenta NO-LOCK NO-ERROR.
        
        FIND FIRST TCer WHERE INDEX(clientes.nit,TCer.nit) = 1 NO-ERROR.
        IF NOT AVAILABLE TCer THEN DO:
            CREATE TCer.
            TCer.Nit = clientes.Nit.
        END.

        TCer.BaseServ = TCer.BaseServ + anexos13.sdo_inicial.

        DO cont = 1 TO 12:
            TCer.BaseServ = TCer.BaseServ + anexos13.db[cont] - anexos13.cr[cont].
        END.
    END.

    FOR EACH mov_contable WHERE mov_contable.nit = clientes.nit
                            AND YEAR(mov_contable.fec_contable) = 2014
                            AND SUBSTRING(mov_contable.cuenta,1,6) = "244525"
                            AND mov_contable.comentario <> "Cierre Anexos Anual" NO-LOCK:
        FIND FIRST TCer WHERE INDEX(clientes.nit,TCer.nit) = 1 NO-ERROR.
        IF AVAILABLE tCer THEN
            TCer.RetServ = TCer.RetServ + mov_contable.cr - mov_contable.db.
    END.
    /* ------------------------ */

    /* Compras */
    FOR EACH anexos13 WHERE anexos13.nit = clientes.nit
                        AND (SUBSTRING(anexos13.cuenta,1,4) = "5110" OR
                             SUBSTRING(anexos13.cuenta,1,8) = "61401011" OR
                             SUBSTRING(anexos13.cuenta,1,8) = "61704011")
                        AND anexos13.ano = 2014 NO-LOCK BREAK BY anexos13.nit
                                                              BY anexos13.cuenta:
        FIND FIRST cuentas WHERE cuentas.cuenta = anexos13.cuenta NO-LOCK NO-ERROR.
        
        FIND FIRST TCer WHERE INDEX(clientes.nit,TCer.nit) = 1 NO-ERROR.
        IF NOT AVAILABLE TCer THEN DO:
            CREATE TCer.
            TCer.Nit = clientes.Nit.
        END.

        TCer.BaseComp = TCer.BaseComp + anexos13.sdo_inicial.

        DO cont = 1 TO 12:
            TCer.BaseComp = TCer.BaseComp + anexos13.db[cont] - anexos13.cr[cont].
        END.
    END.

    FOR EACH activosFijos WHERE YEAR(activosFijos.fechaCompra) = 2014
                            AND activosFijos.nitProveedor = clientes.nit NO-LOCK:
        FIND FIRST TCer WHERE INDEX(clientes.nit,TCer.nit) = 1 NO-ERROR.
        IF NOT AVAILABLE TCer THEN DO:
            CREATE TCer.
            TCer.Nit = clientes.Nit.
        END.

        TCer.BaseComp = TCer.BaseComp + activosFijos.valorCompra.
    END.

    FOR EACH mov_contable WHERE mov_contable.nit = clientes.nit
                            AND YEAR(mov_contable.fec_contable) = 2014
                            AND (SUBSTRING(mov_contable.cuenta,1,6) = "244540" OR
                                 SUBSTRING(mov_contable.cuenta,1,6) = "244565")
                            AND mov_contable.comentario <> "Cierre Anexos Anual" NO-LOCK:
        FIND FIRST TCer WHERE INDEX(clientes.nit,TCer.nit) = 1 NO-ERROR.
        IF AVAILABLE tCer THEN
            TCer.RetComp = TCer.RetComp + mov_contable.cr - mov_contable.db.
    END.
    /* ------------------------ */

    /* Otros */
    EMPTY TEMP-TABLE docs.
    FOR EACH anexos13 WHERE anexos13.nit = clientes.nit
                        AND SUBSTRING(anexos13.cuenta,1,2) = "26"
                        AND anexos13.ano = 2014 NO-LOCK BREAK BY anexos13.nit
                                                              BY anexos13.cuenta:
        FIND FIRST cuentas WHERE cuentas.cuenta = anexos13.cuenta NO-LOCK NO-ERROR.
        
        FIND FIRST TCer WHERE INDEX(clientes.nit,TCer.nit) = 1 NO-ERROR.
        IF NOT AVAILABLE TCer THEN DO:
            CREATE TCer.
            TCer.Nit = clientes.Nit.
        END.

        /*TCer.Bas = TCer.Bas + anexos13.sdo_inicial.*/

        DO cont = 1 TO 12:
            TCer.BaseOtr = TCer.BaseOtr + anexos13.db[cont] /*- anexos13.cr[cont]*/.
        END.

        IF LAST-OF(anexos13.cuenta) THEN DO:
            FOR EACH mov_contable WHERE mov_contable.cuenta = anexos13.cuenta
                                    AND mov_contable.nit = anexos13.nit
                                    AND YEAR(mov_contable.fec_contable) = 2014 NO-LOCK BREAK:
                FIND FIRST docs WHERE docs.nit = mov_contable.nit
                                  AND docs.comprobante = mov_contable.comprobante
                                  AND docs.agencia = mov_contable.agencia
                                  AND docs.num_documento = mov_contable.num_documento NO-LOCK NO-ERROR.
                IF NOT AVAILABLE docs THEN DO:
                    CREATE docs.
                    docs.nit = mov_contable.nit.
                    docs.agencia = mov_contable.agencia.
                    docs.comprobante = mov_contable.comprobante.
                    docs.num_documento = mov_contable.num_documento.
                END.
            END.

            FOR EACH docs NO-LOCK:
                FOR EACH mov_contable WHERE mov_contable.agencia = docs.agencia
                                        AND mov_contable.comprobante = docs.comprobante
                                        AND mov_contable.num_documento = docs.num_documento
                                        AND mov_contable.nit = docs.nit
                                        AND YEAR(mov_contable.fec_contable) = 2014
                                        AND SUBSTRING(mov_contable.cuenta,1,8) = "24457001"
                                        AND mov_contable.comentario <> "Cierre Anexos Anual" NO-LOCK:
                    TCer.RetOtr = TCer.RetOtr + mov_contable.cr - mov_contable.db.
                END.
            END.
        END.
    END.
    /* ------------------------ */
END.

OUTPUT TO d:\Leonardo\certs.csv.
EXPORT DELIMITER ";"
    "NIT"
    "GMF"
    "RetGMF"
    "ICA"
    "RetICA"
    "RENDIMIENTOS"
    "RetRENDIMIENTOS"
    "HONORARIOS"
    "RetHONORARIOS"
    "SERVICIOS"
    "RetSERVICIOS"
    "COMPRAS"
    "RetCOMPRAS"
    "OTROS"
    "RetOTROS".

FOR EACH TCer NO-LOCK:
    EXPORT DELIMITER ";" TCer.
END.

PROCEDURE procesoAnexos:
    DEFINE OUTPUT PARAMETER saldo AS DECIMAL.

    DEFINE VAR i AS INTEGER.

    FOR EACH Anexos13 WHERE Anexos13.Cuenta EQ Cuentas.Cuenta
                        AND Anexos13.Ano EQ 2014
                        AND Anexos13.Nit EQ clientes.Nit NO-LOCK:
        DO i = 1 TO 12 BY 1:
            IF Cuentas.Naturaleza EQ "DB" THEN
                saldo = saldo + Anexos13.DB[i] - Anexos13.CR[i].
            ELSE
                saldo = saldo - Anexos13.DB[i] + Anexos13.CR[i].
        END.
    END.
END PROCEDURE.
