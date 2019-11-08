/* Se desabilitan los triggers para las tablas a replicar. */
DISABLE TRIGGERS FOR LOAD OF BD_WEB.agencias.
DISABLE TRIGGERS FOR LOAD OF BD_WEB.Ahorros.
DISABLE TRIGGERS FOR LOAD OF BD_WEB.Clientes.
DISABLE TRIGGERS FOR LOAD OF BD_WEB.Creditos.
DISABLE TRIGGERS FOR LOAD OF BD_WEB.habiles.
DISABLE TRIGGERS FOR LOAD OF BD_WEB.Mov_Ahorros.
DISABLE TRIGGERS FOR LOAD OF BD_WEB.Mov_creditos.
DISABLE TRIGGERS FOR LOAD OF BD_WEB.Pro_Ahorros.
DISABLE TRIGGERS FOR LOAD OF BD_WEB.Pro_creditos.
DISABLE TRIGGERS FOR LOAD OF BD_WEB.ubicacion.

DEFINE TEMP-TABLE ttMovAhorros LIKE BD_WEB.mov_ahorros.

/* Borramos el movimiento anterior a 6 meses */
IF TIME <= 18599 THEN DO:
    FOR EACH BD_WEB.mov_creditos WHERE BD_WEB.mov_creditos.fecha <= ADD-INTERVAL(TODAY,-6,"months"):
        DELETE BD_WEB.mov_creditos.
    END.

    FOR EACH BD_WEB.mov_ahorros WHERE BD_WEB.mov_ahorros.fecha <= ADD-INTERVAL(TODAY,-6,"months"):
        DELETE BD_WEB.mov_ahorros.
    END.

    /* Se actualiza la tabla de Hábiles */
    FOR EACH BD_WEB.habiles:
        DELETE BD_WEB.habiles.
    END.

    FOR EACH bdcentral.habiles NO-LOCK:
        CREATE BD_WEB.habiles.
        BUFFER-COPY bdcentral.habiles TO BD_WEB.habiles.
    END.
    /* ----------------------- */
END.

/* Comienza la replicación */
FOR EACH bdcentral.Replication WHERE bdcentral.Replication.Replicado = NO BY entry-id:
    CASE bdcentral.Replication.table-name:
        WHEN "Agencias" THEN RUN ReplicarAgencias.
        WHEN "Ahorros" THEN RUN ReplicarAhorros.
        WHEN "Clientes" THEN RUN ReplicarClientes.
        WHEN "Creditos" THEN RUN ReplicarCreditos.
        WHEN "Mov_Ahorros" THEN RUN ReplicarMovAhorros.
        WHEN "Mov_creditos" THEN RUN ReplicarMovCreditos.
        WHEN "Pro_Ahorros" THEN RUN ReplicarProAhorros.
        WHEN "Pro_creditos" THEN RUN ReplicarProCreditos.
        WHEN "Ubicacion" THEN RUN ReplicarUbicacion.
    END CASE.

    DELETE bdcentral.Replication.
END.

DISCONNECT bdcentral.
DISCONNECT bd_web.
QUIT.


PROCEDURE ReplicarAgencias:
    FIND FIRST BD_WEB.agencias WHERE BD_WEB.agencias.agencia = bdcentral.Replication.key-1 NO-ERROR.
    IF NOT AVAILABLE BD_WEB.agencias THEN
        CREATE BD_WEB.agencias.

    RAW-TRANSFER bdcentral.Replication.record TO BD_WEB.agencias.
    
END PROCEDURE.


PROCEDURE ReplicarAhorros:
    FIND FIRST BD_WEB.Ahorros WHERE BD_WEB.Ahorros.agencia = bdcentral.Replication.key-1
                                AND BD_WEB.Ahorros.Cod_ahorro = bdcentral.Replication.key-3
                                AND BD_WEB.Ahorros.Cue_Ahorros = bdcentral.Replication.key-4 NO-ERROR.
    IF NOT AVAILABLE BD_WEB.Ahorros THEN
        CREATE BD_WEB.Ahorros.

    RAW-TRANSFER bdcentral.Replication.record TO BD_WEB.Ahorros.
    
    IF bd_web.ahorros.sdo_disponible = 0 AND bd_web.ahorros.estado = 2 THEN
        DELETE bd_web.ahorros.

END PROCEDURE.


PROCEDURE ReplicarClientes:
    FIND FIRST BD_WEB.Clientes WHERE BD_WEB.Clientes.Nit = bdcentral.Replication.key-2 NO-ERROR.
    IF NOT AVAILABLE BD_WEB.Clientes THEN
        CREATE BD_WEB.Clientes.

    RAW-TRANSFER bdcentral.Replication.record TO BD_WEB.Clientes NO-ERROR.
    
    IF bd_web.clientes.estado <> 1 THEN
        DELETE bd_web.clientes.

END PROCEDURE.


PROCEDURE ReplicarCreditos:
    FIND FIRST BD_WEB.Creditos WHERE BD_WEB.Creditos.agencia = bdcentral.Replication.key-1
                                 AND BD_WEB.Creditos.Nit = bdcentral.Replication.key-2
                                 AND BD_WEB.Creditos.num_credito = bdcentral.Replication.key-3
                                 AND BD_WEB.Creditos.num_solicitud = bdcentral.Replication.key-5 NO-ERROR.
    IF NOT AVAILABLE BD_WEB.Creditos THEN
        CREATE BD_WEB.Creditos.

    RAW-TRANSFER bdcentral.Replication.record TO BD_WEB.Creditos.
    
    IF bd_web.creditos.sdo_capital = 0 AND bd_web.creditos.estado <> 2 THEN
        DELETE bd_web.creditos.
    ELSE DO:
        IF bd_web.creditos.dias_atraso > 0 THEN DO:
            IF bd_web.creditos.cod_credito <> 123 THEN DO:
                bd_web.creditos.val_atraso = bd_web.creditos.INT_morCobrar + bd_web.creditos.INT_moraDifCob.

                FOR EACH bdcentral.control_pagos WHERE bdcentral.CONTROL_pagos.nit = bd_web.creditos.nit
                                                   AND bdcentral.CONTROL_pagos.num_credito = bd_web.creditos.num_credito
                                                   AND bdcentral.CONTROL_pagos.id_pdoMes < 2
                                                   AND bdcentral.CONTROL_pagos.fec_Vcto <= TODAY + 1 NO-LOCK:
                    bd_web.creditos.val_atraso = bd_web.creditos.val_atraso + bdcentral.CONTROL_pagos.cuota - bdcentral.CONTROL_pagos.cap_pagado.
                END.
            END.
            ELSE DO:
                bd_web.creditos.val_atraso = 0.

                FOR EACH bdcentral.facturacion WHERE bdcentral.facturacion.nit = bd_web.creditos.nit
                                                 AND bdcentral.facturacion.num_credito = bd_web.creditos.num_credito
                                                 AND bdcentral.facturacion.fec_pago <= TODAY + 1
                                                 AND bdcentral.facturacion.estado = 1 NO-LOCK BY bdcentral.facturacion.fec_pago DESCENDING:
                    bd_web.creditos.val_atraso = bd_web.creditos.val_atraso + (bdcentral.facturacion.cuota - bdcentral.Facturacion.pago_capital - bdcentral.Facturacion.pago_intCorriente - bdcentral.Facturacion.pago_intDifCobro - bdcentral.Facturacion.pago_mora).
                END.

                IF bd_web.creditos.val_atraso < 0 THEN
                    bd_web.creditos.val_atraso = 0.
            END.
        END.
    END.

END PROCEDURE.


PROCEDURE ReplicarMovAhorros:
    CREATE ttMovAHorros.
    RAW-TRANSFER bdcentral.Replication.record TO ttMovAhorros.

    IF LENGTH(ttMovAhorros.descrip) > 50 THEN
        ttMovAhorros.descrip = SUBSTRING(ttMovAhorros.descrip,1,50).

    /* Si no es ahorro a la vista, lo descartamos */
    IF ttMovAhorros.cod_ahorro <> 4 AND ttMovAhorros.cod_ahorro <> 8 THEN
        LEAVE.

    IF ttMovAhorros.cod_operacion = 010101003 AND ttmovahorros.descrip = "Abono Liq.Interés" THEN DO:
        FIND FIRST BD_WEB.mov_ahorros WHERE BD_WEB.mov_ahorros.nit = ttMovAhorros.nit
                                        AND BD_WEB.mov_ahorros.cue_ahorros = ttMovAhorros.cue_ahorros
                                        AND BD_WEB.mov_ahorros.cod_operacion = 010101003
                                        AND MONTH(BD_WEB.mov_ahorros.fecha) = MONTH(ttMovAhorros.fecha)
                                        AND YEAR(BD_WEB.mov_ahorros.fecha) = YEAR(ttMovAhorros.fecha)
                                        AND BD_WEB.mov_ahorros.descrip = "Abono Liq.Interés" NO-ERROR.
        IF AVAILABLE BD_WEB.mov_ahorros THEN
            ttMovAhorros.val_efectivo = ttMovAhorros.val_efectivo + BD_WEB.mov_ahorros.val_efectivo.
        ELSE
            CREATE BD_WEB.Mov_Ahorros.
    END.
    ELSE
        CREATE BD_WEB.Mov_Ahorros.

    BUFFER-COPY ttmovAhorros TO BD_WEB.mov_ahorros.

END PROCEDURE.


PROCEDURE ReplicarMovCreditos:
    CREATE BD_WEB.Mov_creditos.
    RAW-TRANSFER bdcentral.Replication.record TO BD_WEB.Mov_creditos.

    IF LENGTH(BD_web.mov_creditos.descrip) > 50 THEN
        BD_web.mov_creditos.descrip = SUBSTRING(BD_web.mov_creditos.descrip,1,50).
    
END PROCEDURE.


PROCEDURE ReplicarProAhorros:
    FIND FIRST BD_WEB.Pro_Ahorros WHERE BD_WEB.Pro_Ahorros.cod_ahorro = bdcentral.Replication.key-1
                                    AND BD_WEB.Pro_Ahorros.tip_ahorro = bdcentral.Replication.key-3 NO-ERROR.
    IF NOT AVAILABLE BD_WEB.Pro_Ahorros THEN
        CREATE BD_WEB.Pro_Ahorros.

    RAW-TRANSFER bdcentral.Replication.record TO BD_WEB.Pro_Ahorros.
    
END PROCEDURE.


PROCEDURE ReplicarProCreditos:
    FIND FIRST BD_WEB.Pro_creditos WHERE BD_WEB.Pro_creditos.cod_credito = bdcentral.Replication.key-1
                                     AND BD_WEB.Pro_creditos.tip_credito = bdcentral.Replication.key-3 NO-ERROR.
    IF NOT AVAILABLE BD_WEB.Pro_creditos THEN
        CREATE BD_WEB.Pro_creditos.

    RAW-TRANSFER bdcentral.Replication.record TO BD_WEB.Pro_creditos.
    
END PROCEDURE.


PROCEDURE ReplicarUbicacion:
    FIND FIRST BD_WEB.ubicacion WHERE BD_WEB.ubicacion.ubicacion = bdcentral.Replication.key-2 NO-ERROR.
    IF NOT AVAILABLE BD_WEB.ubicacion THEN
        CREATE BD_WEB.ubicacion.

    RAW-TRANSFER bdcentral.Replication.record TO BD_WEB.ubicacion.
    
END PROCEDURE.
