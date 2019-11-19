DEFINE VAR valDepreciar AS DECIMAL.
DEFINE VAR numDoc AS INTEGER.

FOR EACH agencias NO-LOCK:
    CASE agencias.agencia:
        WHEN 1 THEN numDoc = 5.
        WHEN 2 THEN numDoc = 3.
        WHEN 3 THEN numDoc = 3.
        WHEN 4 THEN numDoc = 3.
    END CASE.

    FOR EACH activosFijos WHERE activosFijos.agencia = agencias.agencia
                            AND activosFijos.valorActual > 0
                            AND activosFijos.mesesDepreciar > 0
                            AND activosFijos.valorActual <> TRUNCATE(activosFijos.valorActual,0)
                            AND activosFijos.depreciable = YES:
        FIND FIRST cfg_activosFijos WHERE cfg_activosFijos.tipoActivo = activosFijos.tipoActivo NO-LOCK NO-ERROR.
        IF AVAILABLE cfg_activosFijos THEN DO:
            valDepreciar = activosFijos.valorActual - TRUNCATE(activosFijos.valorActual,0).

            CREATE mov_contable.
            mov_contable.agencia = activosFijos.agencia.
            mov_contable.Cen_Costos = activosFijos.cen_costo.
            mov_contable.Comentario = "ProcesoDepreciación_AjusteCentavos".
            mov_contable.Comprobante = 10.
            mov_contable.cr = valDepreciar.
            mov_contable.Cuenta = cfg_activosFijos.gasto.
            mov_contable.Fec_Contable = 10/31/2013.
            mov_contable.Fec_Grabacion = TODAY.
            mov_contable.Hora = TIME.
            mov_contable.Nit = activosFijos.idActivo.
            mov_contable.Num_Documento = numDoc.
            mov_contable.Usuario = "2305".

            CREATE mov_contable.
            mov_contable.agencia = activosFijos.agencia.
            mov_contable.Cen_Costos = activosFijos.cen_costo.
            mov_contable.Comentario = "ProcesoDepreciación_AjusteCentavos".
            mov_contable.Comprobante = 10.
            mov_contable.db = valDepreciar.
            mov_contable.Cuenta = cfg_activosFijos.depreciacion.
            mov_contable.Fec_Contable = 10/31/2013.
            mov_contable.Fec_Grabacion = TODAY.
            mov_contable.Hora = TIME.
            mov_contable.Nit = activosFijos.idActivo.
            mov_contable.Num_Documento = numDoc.
            mov_contable.Usuario = "2305".

            activosFijos.valorActual = activosFijos.valorActual - valDepreciar.
            activosFijos.valorDepreciado = activosFijos.valorDepreciado + valDepreciar.
            activosFijos.fecUltDepreciacion = 10/31/2013.
            activosFijos.anotacion = activosFijos.anotacion + " / " + "AjusteCentavos: " + STRING(10/31/2013,"99/99/9999").

            IF activosFijos.valorizacion > 0 THEN DO:
                CREATE mov_contable.
                mov_contable.agencia = activosFijos.agencia.
                mov_contable.Cen_Costos = activosFijos.cen_costo.
                mov_contable.Comentario = "ProcesoValorización_AjusteCentavos".
                mov_contable.Comprobante = 10.
                mov_contable.db = valDepreciar.
                mov_contable.Cuenta = cfg_activosFijos.valorizacionDB.
                mov_contable.Fec_Contable = 10/31/2013.
                mov_contable.Fec_Grabacion = TODAY.
                mov_contable.Hora = TIME.
                mov_contable.Nit = activosFijos.idActivo.
                mov_contable.Num_Documento = numDoc.
                mov_contable.Usuario = "2305".

                CREATE mov_contable.
                mov_contable.agencia = activosFijos.agencia.
                mov_contable.Cen_Costos = activosFijos.cen_costo.
                mov_contable.Comentario = "ProcesoValorización_AjusteCentavos".
                mov_contable.Comprobante = 10.
                mov_contable.cr = valDepreciar.
                mov_contable.Cuenta = cfg_activosFijos.valorizacionCR.
                mov_contable.Fec_Contable = 10/31/2013.
                mov_contable.Fec_Grabacion = TODAY.
                mov_contable.Hora = TIME.
                mov_contable.Nit = activosFijos.idActivo.
                mov_contable.Num_Documento = numDoc.
                mov_contable.Usuario = "2305".

                activosFijos.valorizacion = activosFijos.valorizacion + valDepreciar.
            END.
        END.
    END.
END.
