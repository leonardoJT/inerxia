FOR EACH Pro_Ahorros NO-LOCK WHERE Pro_Ahorros.Estado EQ 1 BY Pro_Ahorros.Cod_Ahorro:
    FOR EACH agencia NO-LOCK:
        FOR EACH CortoLargo NO-LOCK WHERE cortolargo.agencia = agencia.agencia
                                      AND CortoLargo.Clase_Producto EQ 1
                                      AND CortoLargo.Cod_Producto   EQ Pro_Ahorros.Cod_Ahorro
                                      AND CortoLargo.Plazo_Inicial  GE 0
            BREAK BY CortoLargo.Agencia BY CortoLargo.Cod_Producto BY CortoLargo.Plazo_Inicial:
            IF FIRST-OF(CortoLargo.Cod_Producto) THEN DO:
               FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ CortoLargo.Cta_AsoAd
                                    AND Cuentas.Tipo   EQ 2
                                    AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
               IF AVAIL Cuentas THEN 
                  FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ CortoLargo.Cta_SyA
                                       AND Cuentas.Tipo   EQ 2
                                       AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
               IF NOT AVAIL Cuentas THEN
                  MESSAGE "En CortoLargo.Cta_AsoAd y CortoLargo.Cta_SyA deben existir Activas en Cuentas..." SKIP
                          "Para el Pro_Ahorros.Cod_Ahorro : " Pro_Ahorros.Cod_Ahorro SKIP
                          "De la Agencia : " CortoLargo.Agencia VIEW-AS ALERT-BOX ERROR.
               IF Pro_Ahorros.Tip_Ahorro GE 4 THEN NEXT.
               FIND FIRST Liqui_Int WHERE Liqui_Int.Clase_Producto EQ 1 AND
                    Liqui_Int.Cod_Producto EQ CortoLargo.Cod_Producto NO-LOCK NO-ERROR.
               IF NOT AVAIL Liqui_Int THEN
                  MESSAGE "Falta Liqui_Int Para el Pro_Ahorros.Cod_Ahorro : "
                           Pro_Ahorros.Cod_Ahorro VIEW-AS ALERT-BOX ERROR.
            END.
        END.
    END.
END.
FOR EACH Pro_Creditos NO-LOCK WHERE Pro_Creditos.Estado EQ 1 BY Pro_Creditos.Cod_Credito:
    FOR EACH agencia NO-LOCK:
        FOR EACH CortoLargo NO-LOCK WHERE cortolargo.agencia = agencia.agencia
                                      AND CortoLargo.Clase_Producto EQ 2
                                      AND CortoLargo.Cod_Producto   EQ Pro_Creditos.Cod_Credito
                                      AND CortoLargo.Plazo_Inicial  GE 0
            BREAK BY CortoLargo.Agencia BY CortoLargo.Cod_Producto BY CortoLargo.Plazo_Inicial:
            IF FIRST-OF(CortoLargo.Cod_Producto) THEN DO:
               FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ CortoLargo.Cta_AsoAd
                                    AND Cuentas.Tipo   EQ 2
                                    AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
               IF AVAIL Cuentas THEN DO:
                  FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ CortoLargo.Cta_SyA
                                       AND Cuentas.Tipo   EQ 2
                                       AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
                  IF AVAIL Cuentas THEN DO:
                     FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ CortoLargo.Cta_CostasDB 
                                          AND Cuentas.Tipo   EQ 2
                                          AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
                     IF AVAIL Cuentas THEN DO:
                        FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ CortoLargo.Cta_HonorariosDB 
                                             AND Cuentas.Tipo   EQ 2
                                             AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
                        IF AVAIL Cuentas THEN
                           FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ CortoLargo.Cta_PolizasDB 
                                                AND Cuentas.Tipo   EQ 2
                                                AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR. 
                     END.
                  END.
               END.
               IF NOT AVAIL Cuentas THEN
                  MESSAGE "En CortoLargo.Cta_AsoAd,Cta_SyA,Cta_CostasDB,Cta_HonorariosDB,Cta_PolizasDB..." SKIP
                          "deben existir Activas en Cuentas...Para el Pro_Creditos.Cod_Credito : " Pro_Creditos.Cod_Credito SKIP
                          "De la Agencia : " CortoLargo.Agencia VIEW-AS ALERT-BOX ERROR.
                
               FIND FIRST Liqui_Int WHERE Liqui_Int.Clase_Producto EQ 2 AND
                    Liqui_Int.Cod_Producto EQ CortoLargo.Cod_Producto NO-LOCK NO-ERROR.
               IF NOT AVAIL(Liqui_Int) THEN
                  MESSAGE "Falta Liqui_Int Para el Pro_Creditos.Cod_Credito : " Pro_Creditos.Cod_Credito VIEW-AS ALERT-BOX ERROR.
               FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Liqui_Int.CtaCr_LiqAso
                                    AND Cuentas.Tipo   EQ 2
                                    AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
               IF AVAIL Cuentas THEN DO:
                  FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Liqui_Int.CtaDb_LiqAso 
                                       AND Cuentas.Tipo   EQ 2
                                       AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
                  IF AVAIL Cuentas THEN DO:
                     FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Liqui_Int.CtaInt_AntAso 
                                          AND Cuentas.Tipo   EQ 2
                                          AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
                     IF AVAIL Cuentas THEN DO:
                        FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Liqui_Int.CtaDb_MoraAso 
                                             AND Cuentas.Tipo   EQ 2
                                             AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
                        IF AVAIL Cuentas THEN DO:
                           FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Liqui_Int.CtaDb_DifCobAso 
                                                AND Cuentas.Tipo   EQ 2
                                                AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
                           IF AVAIL Cuentas THEN DO:
                              FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Liqui_Int.CtaCr_DifCobAso 
                                                   AND Cuentas.Tipo   EQ 2
                                                   AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
                              IF AVAIL(Cuentas) THEN
                                 FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Liqui_Int.CtaCr_MoraAso 
                                                      AND Cuentas.Tipo   EQ 2
                                                      AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.        
                           END.
                        END.
                     END.
                  END.
               END.
               IF NOT AVAIL(Cuentas) THEN 
                  MESSAGE "En Liqui_Int las Cuentas : CtaCr_LiqAso,CtaDb_LiqAso,CtaCr_DifCobAso" SKIP
                          "           CtaCr_MoraAso,CtaInt_AntAso,CtaDb_MoraAso,CtaDb_DifCobAso" SKIP
                          "Deben existir Activas en Plan de Cuentas..." SKIP
                          "Para el Pro_Creditos.Cod_Credito : " Pro_Creditos.Cod_Credito VIEW-AS ALERT-BOX ERROR.
            END.
        END.
    END.
END.



