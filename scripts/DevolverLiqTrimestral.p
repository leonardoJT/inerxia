DEFINE VAR valorReal AS DECIMAL.
DEFINE VAR devolver AS DECIMAL.
DEFINE VAR rfReal AS DECIMAL.
DEFINE VAR rteFteDevolver AS DECIMAL.
DEFINE VAR rfDevolver AS DECIMAL.
DEFINE VAR pSecuencia AS INTEGER.
DEFINE VAR devInteres AS DECIMAL.
DEFINE VAR TOTAL21050502 AS DECIMAL.
DEFINE VAR TOTAL61752001 AS DECIMAL.
DEFINE VAR TOTAL61750505 AS DECIMAL.

DEFINE TEMP-TABLE archivo
    FIELD agencia AS INTEGER
    FIELD producto AS INTEGER
    FIELD nit AS CHARACTER
    FIELD cuenta AS CHARACTER
    FIELD base AS DECIMAL FORMAT ">>>>>>>>>>"
    FIELD liquidado AS DECIMAL FORMAT ">>>>>>>>>>"
    FIELD reteFuente AS DECIMAL FORMAT ">>>>>>>>>>"
    FIELD neto AS DECIMAL FORMAT ">>>>>>>>>>"
    FIELD relleno AS DECIMAL
    FIELD prodDestino AS INTEGER
    FIELD ctaDestino AS CHARACTER.
    
MESSAGE "Inicia"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

INPUT FROM D:\liqTrim.csv.
REPEAT :
    CREATE archivo.
    IMPORT DELIMITER ";" archivo.
    /*DISPLAY archivo.base.*/
END.
INPUT CLOSE.

OUTPUT TO d:\DevolucionTrimestral.txt.
FOR EACH archivo WHERE archivo.nit <> "14894713"
                   AND archivo.nit <> "17057034"
                   AND archivo.nit <> "19353888"
                   AND archivo.nit <> "19404247"
                   AND archivo.nit <> "79232797"
                   /*AND archivo.nit <> "79282962"*/
                   AND archivo.nit <> "79452376"
                   AND archivo.nit <> "79504413"
                   AND archivo.nit <> "79592937" BREAK BY archivo.agencia:
    IF FIRST-OF(archivo.agencia) THEN DO:
        MESSAGE "Agencia" archivo.agencia
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        FIND FIRST comprobantes WHERE comprobantes.comprobante = 20
                                  AND comprobantes.agencia = archivo.agencia NO-ERROR.
        comprobantes.secuencia = comprobantes.secuencia + 1.
        pSecuencia = comprobantes.secuencia.
    END.

    valorReal = (archivo.base / 100) * 1.8.
    valorReal = ROUND(valorReal,0).
    devolver = archivo.liquidado - valorReal.

    IF archivo.reteFuente > 0 THEN DO:
        rfReal = (valorReal / 100) * 7.
        rfReal = ROUND(rfReal,0).
        rfDevolver = archivo.reteFuente - rfReal.
    END.
    ELSE DO:
        rfDevolver = 0.
    END.

    devolver = devolver - rfDevolver.

    devInteres = ((((devolver / 100) * 4.8) / 360) * 13).
    devInteres = ROUND(devInteres,0).
    
    devolver = devolver + devInteres.

    FIND FIRST ahorros WHERE ahorros.nit = archivo.nit
                         /*AND ahorros.cod_ahorro = archivo.prodDestino*/
                         AND ahorros.cue_ahorro = archivo.ctaDestino NO-ERROR.
    IF AVAILABLE ahorros THEN DO:
        DISPLAY archivo.nit archivo.base archivo.Liquidado archivo.retefuente archivo.neto valorReal rfReal devInteres devolver rfDevolver WITH WIDTH 250.

        ahorros.sdo_disponible = ahorros.sdo_disponible - devolver.
        ahorros.fec_ulttransaccion = TODAY.
        
        CREATE Mov_Ahorros.
        ASSIGN Mov_Ahorros.Agencia = Ahorros.Agencia
               Mov_Ahorros.Age_Destino = Ahorros.Agencia
               Mov_Ahorros.Age_Fuente = ahorros.agencia
               Mov_Ahorros.Cod_Ahorro = Ahorros.Cod_Ahorro
               Mov_Ahorros.Cue_Ahorros = Ahorros.Cue_Ahorro
               Mov_Ahorros.Fecha = TODAY
               Mov_Ahorros.Hora = TIME
               Mov_Ahorros.Nit = Ahorros.Nit
               Mov_Ahorros.Num_Documento = STRING(pSecuencia)
               Mov_Ahorros.Sdo_Disponible = Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje
               Mov_Ahorros.Usuario = "2305"
               Mov_Ahorros.Val_Efectivo = devolver
               Mov_Ahorros.Cod_Operacion = 010102001
               Mov_Ahorros.Descrip = "RevPorTasaMenor"
               Mov_Ahorros.Cpte = 20.

        IF rfDevolver > 0 THEN DO:
            CREATE mov_contable.
            ASSIGN mov_contable.cen_costos = 999
                   mov_contable.nit = ahorros.nit
                   Mov_Contable.Agencia = archivo.agencia
                   Mov_Contable.Destino = archivo.agencia
                   Mov_Contable.Comprobante = 20
                   Mov_Contable.Num_Documento = pSecuencia
                   Mov_contable.Doc_referencia = STRING(pSecuencia)
                   Mov_Contable.Fec_Contable = TODAY
                   Mov_Contable.Fec_Grabacion = TODAY
                   Mov_Contable.Cuenta = "24453501"
                   Mov_Contable.Comentario = "RevIntTrim x ErrorTasa"
                   Mov_Contable.Usuario = "2305"
                   Mov_Contable.Estacion = "005"
                   Mov_contable.db = rfDevolver.

            CREATE mov_contable.
            ASSIGN mov_contable.cen_costos = 999
                   mov_contable.nit = ahorros.nit
                   Mov_Contable.Agencia = archivo.agencia
                   Mov_Contable.Destino = archivo.agencia
                   Mov_Contable.Comprobante = 20
                   Mov_Contable.Num_Documento = pSecuencia
                   Mov_contable.Doc_referencia = STRING(pSecuencia)
                   Mov_Contable.Fec_Contable = TODAY
                   Mov_Contable.Fec_Grabacion = TODAY
                   Mov_Contable.Cuenta = "61752001"
                   Mov_Contable.Comentario = "RevIntTrim x ErrorTasa"
                   Mov_Contable.Usuario = "2305"
                   Mov_Contable.Estacion = "005"
                   Mov_contable.cr = rfDevolver.
        END.

        TOTAL21050502 = TOTAL21050502 + devolver.
        TOTAL61752001 = TOTAL61752001 + (devolver - devInteres).
        TOTAL61750505 = TOTAL61750505 + devInteres.
    END.

    IF LAST-OF(archivo.agencia) THEN DO:
        CREATE mov_contable.
        ASSIGN mov_contable.cen_costos = 999
               /*mov_contable.nit = ahorros.nit*/
               Mov_Contable.Agencia = archivo.agencia
               Mov_Contable.Destino = archivo.agencia
               Mov_Contable.Comprobante = 20
               Mov_Contable.Num_Documento = pSecuencia
               Mov_contable.Doc_referencia = STRING(pSecuencia)
               Mov_Contable.Fec_Contable = TODAY
               Mov_Contable.Fec_Grabacion = TODAY
               Mov_Contable.Cuenta = "21050502"
               Mov_Contable.Comentario = "RevIntTrim x ErrorTasa"
               Mov_Contable.Usuario = "2305"
               Mov_Contable.Estacion = "005"
               Mov_contable.db = total21050502.

        CREATE mov_contable.
        ASSIGN mov_contable.cen_costos = 999
               /*mov_contable.nit = ahorros.nit*/
               Mov_Contable.Agencia = archivo.agencia
               Mov_Contable.Destino = archivo.agencia
               Mov_Contable.Comprobante = 20
               Mov_Contable.Num_Documento = pSecuencia
               Mov_contable.Doc_referencia = STRING(pSecuencia)
               Mov_Contable.Fec_Contable = TODAY
               Mov_Contable.Fec_Grabacion = TODAY
               Mov_Contable.Cuenta = "61752001"
               Mov_Contable.Comentario = "RevIntTrim x ErrorTasa"
               Mov_Contable.Usuario = "2305"
               Mov_Contable.Estacion = "005"
               Mov_contable.cr = TOTAL61752001.

        CREATE mov_contable.
        ASSIGN mov_contable.cen_costos = 999
               /*mov_contable.nit = ahorros.nit*/
               Mov_Contable.Agencia = archivo.agencia
               Mov_Contable.Destino = archivo.agencia
               Mov_Contable.Comprobante = 20
               Mov_Contable.Num_Documento = pSecuencia
               Mov_contable.Doc_referencia = STRING(pSecuencia)
               Mov_Contable.Fec_Contable = TODAY
               Mov_Contable.Fec_Grabacion = TODAY
               Mov_Contable.Cuenta = "61750505"
               Mov_Contable.Comentario = "RevIntTrim x ErrorTasa"
               Mov_Contable.Usuario = "2305"
               Mov_Contable.Estacion = "005"
               Mov_contable.cr = TOTAL61750505.


        TOTAL21050502 = 0.
        TOTAL61752001 = 0.
        TOTAL61750505 = 0.
    END.
END.

MESSAGE "Finaliza"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
