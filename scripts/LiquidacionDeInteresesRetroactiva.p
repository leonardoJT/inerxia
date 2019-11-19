DEFINE TEMP-TABLE saldos
    FIELD agencia AS INTEGER
    FIELD nit AS CHARACTER
    FIELD cue_ahorros AS CHARACTER
    FIELD interes AS INTEGER EXTENT 31
    FIELD vTotal AS INTEGER
    FIELD retencion AS INTEGER.

/*DEFINE VAR tasaAnterior AS DECIMAL INITIAL 3.5947908*/
DEFINE VAR tasaAnterior AS DECIMAL INITIAL 3.66.
DEFINE VAR tasaActual1 AS DECIMAL INITIAL 3.9222864.
DEFINE VAR tasaActual2 AS DECIMAL INITIAL 4.8793464.
DEFINE VAR tasaActual3 AS DECIMAL INITIAL 5.1171912.

DEFINE VAR tasaLiq AS DECIMAL.
DEFINE VAR sdoLiq AS INTEGER.

DEFINE VAR fechaAux AS DATE.
DEFINE VAR cont AS INTEGER.
DEFINE VAR contReg AS INTEGER.
DEFINE VAR totalAg AS INTEGER.
DEFINE VAR totalRet AS INTEGER.

/* Llevo a una tabla todos los productos de ahorros a los que hay que devolverles dinero */
FOR EACH ahorros WHERE ahorros.tip_ahorro = 1 AND estado = 1 NO-LOCK:
    CREATE saldos.
    saldos.agencia = ahorros.agencia.
    saldos.nit = ahorros.nit.
    saldos.cue_ahorros = ahorros.cue_ahorros.
END.

/* Empiezo a armar los saldos con los que liquidó */
FOR EACH saldos:
    cont = 0.
    contReg = contReg + 1.

    DO fechaAux = 07/19/2016 TO 08/18/2016:
        cont = cont + 1.

        FOR EACH mov_ahorros WHERE mov_ahorros.agencia = saldos.agencia
                               AND mov_ahorros.nit = saldos.nit
                               AND mov_ahorros.cue_ahorros = saldos.cue_ahorros
                               AND mov_ahorros.cpte = 20
                               AND (mov_ahorros.descrip = "Abono Liq.interés" OR
                                    mov_ahorros.descrip = "RetFuente X Liq.Interés")
                               AND mov_ahorros.fecha = fechaAux NO-LOCK BY mov_ahorros.descrip:
            IF mov_ahorros.descrip = "Abono Liq.interés" THEN DO:
                IF mov_ahorros.sdo_disponible > mov_ahorros.val_efectivo THEN
                    sdoLiq = mov_ahorros.sdo_disponible - mov_ahorros.val_efectivo.
                ELSE
                    sdoLiq = (mov_ahorros.val_efectivo * 100) / (tasaAnterior / 360).

                IF sdoLiq <= 4999999 THEN
                    tasaLiq = tasaActual1 / 360.
                ELSE DO:
                    IF sdoLiq <= 19999999 THEN
                        tasaLiq = tasaActual2 / 360.
                    ELSE
                        tasaLiq = tasaActual3 / 360.
                END.

                saldos.interes[cont] = ((mov_ahorros.val_efectivo * tasaLiq) / (tasaAnterior / 360)) - mov_ahorros.val_efectivo.
                saldos.vtotal = saldos.vtotal + saldos.interes[cont].
            END.
            ELSE DO:
                saldos.retencion = saldos.retencion + saldos.interes[cont] / 100 * 7.
            END.
        END.
    END.

    /*DISPLAY contReg.*/

    /*IF contReg MOD 1000 = 0 THEN
        MESSAGE contReg
            VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
END.

OUTPUT TO d:\Leonardo\liqRetroactiva_.csv.
FOR EACH saldos NO-LOCK:
    EXPORT DELIMITER ";" saldos.
END.
OUTPUT CLOSE.

/*
FOR EACH agencias NO-LOCK:
    FIND FIRST comprobantes WHERE comprobantes.comprobante = 20 NO-ERROR.
    comprobantes.secuencia = comprobantes.secuencia + 1.

    totalAg = 0.
    totalRet = 0.

    FOR EACH saldos WHERE saldos.agencia = agencias.agencia NO-LOCK:
        FIND FIRST ahorros WHERE ahorros.agencia = saldos.agencia
                             AND ahorros.nit = saldos.nit
                             AND ahorros.cue_ahorros = saldos.cue_ahorros NO-ERROR.
        IF AVAILABLE ahorros THEN DO:
            totalAg = totalAg + saldos.vtotal /*- saldos.retencion*/.
            totalRet = totalRet + saldos.retencion.

            ahorros.sdo_disponible = ahorros.sdo_disponible + saldos.vTotal.

            CREATE mov_ahorros.
            ASSIGN Mov_Ahorros.Agencia = Ahorros.Agencia
                   Mov_Ahorros.Age_Destino = Ahorros.Agencia
                   Mov_Ahorros.Age_Fuente = ahorros.agencia
                   Mov_Ahorros.Cod_Ahorro = Ahorros.Cod_Ahorro
                   Mov_Ahorros.Cue_Ahorros = Ahorros.Cue_Ahorro
                   Mov_Ahorros.Fecha = TODAY
                   Mov_Ahorros.Hora = TIME
                   Mov_Ahorros.Nit = Ahorros.Nit
                   Mov_Ahorros.Num_Documento = STRING(comprobantes.comprobante)
                   Mov_Ahorros.Sdo_Disponible = ahorros.sdo_disponible
                   Mov_Ahorros.Usuario = "2305"
                   Mov_Ahorros.Val_Efectivo = saldos.vTotal
                   Mov_Ahorros.Cod_Operacion = 010101003
                   Mov_Ahorros.Cpte = 20
                   Mov_Ahorros.Descrip = "Liquidación / Resolución #29".

            ahorros.sdo_disponible = ahorros.sdo_disponible - saldos.retencion.

            IF saldos.retencion > 0 THEN DO:
                CREATE mov_ahorros.
                ASSIGN Mov_Ahorros.Agencia = Ahorros.Agencia
                       Mov_Ahorros.Age_Destino = Ahorros.Agencia
                       Mov_Ahorros.Age_Fuente = ahorros.agencia
                       Mov_Ahorros.Cod_Ahorro = Ahorros.Cod_Ahorro
                       Mov_Ahorros.Cue_Ahorros = Ahorros.Cue_Ahorro
                       Mov_Ahorros.Fecha = TODAY
                       Mov_Ahorros.Hora = TIME
                       Mov_Ahorros.Nit = Ahorros.Nit
                       Mov_Ahorros.Num_Documento = STRING(comprobantes.comprobante)
                       Mov_Ahorros.Sdo_Disponible = ahorros.sdo_disponible
                       Mov_Ahorros.Usuario = "2305"
                       Mov_Ahorros.Val_Efectivo = saldos.retencion
                       Mov_Ahorros.Cod_Operacion = 010102001
                       Mov_Ahorros.Cpte = 20
                       Mov_Ahorros.Descrip = "ReteFuente x liquidación".

                CREATE mov_contable.
                mov_contable.agencia = agencias.agencia.
                mov_contable.cuenta = "24453503".
                mov_contable.comentario = "ReteFuente x Liquidación".
                mov_contable.cr = saldos.retencion.
                mov_contable.cen_costos = 999.
                mov_contable.destino = agencias.agencia.
                mov_contable.comprobante = 20.
                mov_contable.num_documento = comprobantes.secuencia.
                mov_contable.Doc_referencia = STRING(comprobantes.secuencia).
                mov_contable.Fec_Contable = TODAY.
                mov_contable.Fec_Grabacion = TODAY.
                mov_contable.Usuario = "desarrollo".
                mov_contable.Estacion = "005".
                mov_contable.nit = ahorros.nit.
            END.
        END.
    END.

    CREATE mov_contable.
    mov_contable.agencia = agencias.agencia.
    mov_contable.cuenta = "21050501".
    mov_contable.comentario = "Liquidación / Resolución #29".
    mov_contable.cr = totalAg - totalRet.
    mov_contable.cen_costos = 999.
    mov_contable.destino = agencias.agencia.
    mov_contable.comprobante = 20.
    mov_contable.num_documento = comprobantes.secuencia.
    mov_contable.Doc_referencia = STRING(comprobantes.secuencia).
    mov_contable.Fec_Contable = TODAY.
    mov_contable.Fec_Grabacion = TODAY.
    mov_contable.Usuario = "desarrollo".
    mov_contable.Estacion = "005".

    CREATE mov_contable.
    mov_contable.agencia = agencias.agencia.
    mov_contable.cuenta = "61750503".
    mov_contable.comentario = "Liquidación / Resolución #29".
    mov_contable.db = totalAg.
    mov_contable.cen_costos = 999.
    mov_contable.destino = agencias.agencia.
    mov_contable.comprobante = 20.
    mov_contable.num_documento = comprobantes.secuencia.
    mov_contable.Doc_referencia = STRING(comprobantes.secuencia).
    mov_contable.Fec_Contable = TODAY.
    mov_contable.Fec_Grabacion = TODAY.
    mov_contable.Usuario = "desarrollo".
    mov_contable.Estacion = "005".
END.
*/

MESSAGE "Terminó" contReg
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
