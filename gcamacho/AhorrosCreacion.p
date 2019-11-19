
DEFINE BUFFER BA FOR Ahorros.                                          
                                          
DEFINE VARIABLE vi AS INTEGER     NO-UNDO.

OUTPUT TO "C:\info_fodun\PermanentesCreados.csv".
PUT "nit;cod_ahorro;cue_ahorro" SKIP.
FOR EACH ahorros WHERE ahorros.cod_ahorro EQ 2 AND estado EQ 1 NO-LOCK:
    FIND FIRST BA WHERE BA.nit EQ Ahorros.nit AND BA.cod_ahorro EQ 8 EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE BA THEN DO:
        FIND FIRST clientes WHERE clientes.nit EQ Ahorros.nit NO-LOCK NO-ERROR.

        FIND FIRST pro_ahorros WHERE pro_ahorros.tip_ahorro EQ 1 AND pro_ahorros.cod_ahorro EQ 8 NO-LOCK NO-ERROR.
        IF AVAILABLE pro_ahorros THEN DO:
          FIND FIRST indicadores WHERE indicadores.indicador EQ pro_ahorros.indicador NO-LOCK NO-ERROR.
          IF Pro_Ahorros.Id_Consecutivo THEN DO:
                   FIND CURRENT pro_ahorros EXCLUSIVE-LOCK NO-ERROR.
                   CREATE BA.
                   UPDATE pro_ahorros.num_consecutivo = pro_ahorros.num_consecutivo + 1
                          BA.Cue_Ahorros     = STRING(Pro_Ahorros.Num_Consecutivo)
                          BA.Detalle_Estado  = 1
                          BA.Agencia         = Ahorros.Agencia
                          BA.Tip_Ahorro      = pro_ahorros.tip_ahorro
                          BA.Cod_ahorro      = pro_ahorros.cod_ahorro
                          BA.FOR_Pago        = 2
                          BA.Monto_Apertura  = 0
                          BA.Nit             = Ahorros.Nit
                          BA.For_Liquidacion = Pro_Ahorros.FOR_Liquidacion
                          BA.Estado          = 1
                          BA.Tasa            = Indicadores.Tasa
                          BA.Usu_Creacion    = "999"
                          BA.IdNombre        = Clientes.Nombre
                          BA.IdApellido1     = Clientes.Apellido1
                          BA.Cuota           = 0
                          BA.Plazo           = 9999
                          BA.Per_Deduccion   = 1
                          BA.per_Liquidacion = 6
                          BA.Fec_Apertura    = TODAY.
                  FIND CURRENT Pro_Ahorros NO-LOCK NO-ERROR.
                  FIND CURRENT BA     NO-LOCK NO-ERROR.
                  ASSIGN vi = vi + 1.
                  EXPORT DELIMITER ";" BA.nit BA.cod_ahorro BA.cue_ahorro.
           END. /*IF Pro_Ahorros.Id_Consecutivo THEN DO TRANSACTION:*/
        END. /*IF AVAILABLE pro_ahorros */
    END.
END.

OUTPUT CLOSE.

MESSAGE "Creados: " vi
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
