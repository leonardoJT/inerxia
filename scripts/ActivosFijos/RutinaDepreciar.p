DEFINE VAR valDepreciar AS DECIMAL.

FOR EACH agencias NO-LOCK:
    FIND FIRST comprobantes WHERE comprobantes.agencia = agencias.agencia
                              AND comprobantes.comprobante = 10 NO-ERROR.
    comprobantes.secuencia = comprobantes.secuencia + 1.

    FOR EACH activosFijos WHERE activosFijos.agencia = agencias.agencia
                            AND activosFijos.valorActual > 0
                            AND activosFijos.mesesDepreciar > 0
                            AND activosFijos.depreciable = YES:
        FIND FIRST cfg_activosFijos WHERE cfg_activosFijos.tipoActivo = activosFijos.tipoActivo NO-LOCK NO-ERROR.
        IF AVAILABLE cfg_activosFijos THEN DO:
            valDepreciar = activosFijos.valorActual / activosFijos.mesesDepreciar.

            CREATE mov_contable.
            mov_contable.agencia = activosFijos.agencia.
            mov_contable.Cen_Costos = activosFijos.cen_costo.
            mov_contable.Comentario = "Proceso Depreciación".
            mov_contable.Comprobante = comprobantes.comprobante.
            mov_contable.db = valDepreciar.
            mov_contable.Cuenta = cfg_activosFijos.gasto.
            mov_contable.Fec_Contable = TODAY.
            mov_contable.Fec_Grabacion = TODAY.
            mov_contable.Hora = TIME.
            mov_contable.Nit = activosFijos.idActivo.
            mov_contable.Num_Documento = comprobantes.secuencia.
            mov_contable.Usuario = "2305".

            CREATE mov_contable.
            mov_contable.agencia = activosFijos.agencia.
            mov_contable.Cen_Costos = activosFijos.cen_costo.
            mov_contable.Comentario = "Proceso Depreciación".
            mov_contable.Comprobante = comprobantes.comprobante.
            mov_contable.cr = valDepreciar.
            mov_contable.Cuenta = cfg_activosFijos.depreciacion.
            mov_contable.Fec_Contable = TODAY.
            mov_contable.Fec_Grabacion = TODAY.
            mov_contable.Hora = TIME.
            mov_contable.Nit = activosFijos.idActivo.
            mov_contable.Num_Documento = comprobantes.secuencia.
            mov_contable.Usuario = "2305".

            activosFijos.valorActual = activosFijos.valorActual - valDepreciar.
            activosFijos.valorDepreciado = activosFijos.valorDepreciado + valDepreciar.
            activosFijos.mesesDepreciar = activosFijos.mesesDepreciar - 1.
            activosFijos.fecUltDepreciacion = TODAY.
            activosFijos.anotacion = activosFijos.anotacion + " / " + "Última Depreciación: " + STRING(TODAY,"99/99/9999").
        END.
    END.

    FIND FIRST mov_contable WHERE mov_contable.comprobante = comprobantes.comprobante
                              AND mov_contable.agencia = agencias.agencia
                              AND mov_contable.fec_contable = TODAY
                              AND mov_contable.num_documento = comprobantes.secuencia NO-LOCK NO-ERROR.
    IF NOT AVAILABLE mov_contable THEN
        comprobantes.secuencia = comprobantes.secuencia - 1.
END.


