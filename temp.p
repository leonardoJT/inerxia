FOR EACH Pro_Ahorros WHERE Pro_Ahorros.Tip_Ahorro <> 4 /* 4 - Aportes */
                       AND Pro_Ahorros.Estado = 1
                       AND pro_ahorros.tip_ahorro = 1 NO-LOCK:
    FOR EACH CortoLargo WHERE CortoLargo.Clase_Producto = 1
                          AND CortoLargo.Cod_Producto = Pro_Ahorros.Cod_Ahorro
                          AND CortoLargo.Plazo_Inicial >= 0 NO-LOCK BREAK BY CortoLargo.Agencia
                                                                          BY CortoLargo.Cod_Producto
                                                                          BY CortoLargo.Plazo_Inicial:
        IF FIRST-OF(CortoLargo.Cod_Producto) THEN DO:
            FIND FIRST Liqui_Int WHERE Liqui_Int.Clase_Producto = 1
                                   AND Liqui_Int.Cod_Producto = CortoLargo.Cod_Producto NO-LOCK NO-ERROR.

            DISPLAY pro_ahorros.Tip_Ahorro pro_ahorros.nom cortoLargo.cod_producto cortoLargo.cta_asoAd CortoLargo.Cta_SyA Liqui_Int.CtaCr_LiqAso Liqui_Int.CtaDb_LiqAso Liqui_Int.Cta_CauCr WITH WIDTH 300.

        END.
    END.
END.
