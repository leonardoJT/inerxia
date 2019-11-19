/*IF Inserted.Cod_Producto EQ ? OR Inserted.Cod_Producto EQ 0 THEN DO:
   MESSAGE "El Código del producto no se puede" SKIP
           "omitir o ser nulo. Verifique."
           VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
   RETURN ERROR.
END.*/

IF Inserted.Clase_Producto EQ 1 THEN DO:
   FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ Inserted.Cod_Producto
              NO-LOCK NO-ERROR.
   IF AVAILABLE Pro_Ahorros THEN DO:
      IF Inserted.Cod_base EQ "" THEN DO:
         MESSAGE "Código Base de Retención no" SKIP
                 "puede omitirse.  Verifique."
         VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
         RETURN ERROR.
      END.
      IF Pro_Ahorros.Id_Asociado EQ 1 THEN
         IF Inserted.CtaDb_LiqAso  EQ "" OR Inserted.CtaCr_LiqAso EQ "" OR Inserted.CtaDb_Liq     EQ "" OR
            Inserted.CtaCr_Liq     EQ "" OR Inserted.CtaDb_Ret    EQ "" OR Inserted.CtaCr_Ret     EQ "" OR
            Inserted.CtaInt_AntAso EQ "" OR Inserted.CtaInt_Ant   EQ ""                            THEN DO:
            MESSAGE "Producto para todos, las Cuentas" SKIP
                    "No pueden omitirse. Verifique."
            VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
            RETURN ERROR.
         END.
      IF Pro_Ahorros.Id_Asociado EQ 2 THEN
         IF Inserted.CtaDb_LiqAso  EQ "" OR Inserted.CtaCr_LiqAso  EQ "" OR Inserted.CtaDb_Ret     EQ "" OR 
            Inserted.CtaCr_Ret     EQ "" OR Inserted.CtaInt_AntAso EQ ""                            THEN DO:
            MESSAGE "Producto para Asociado Las Cuentas" SKIP
                    "Asociado No pueden omitirse. Verifique."
            VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
            RETURN ERROR.
         END.
      IF Pro_Ahorros.Id_Asociado EQ 3 THEN
         IF Inserted.CtaDb_Liq  EQ "" OR Inserted.CtaCr_Liq  EQ "" OR Inserted.CtaDb_Ret  EQ "" OR
            Inserted.CtaCr_Ret  EQ "" OR Inserted.CtaInt_Ant EQ ""                         THEN DO:
            MESSAGE "Producto para No Asociado Las Cuentas" SKIP
                    "No Asociado No pueden omitirse. Verifique."
            VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
            RETURN ERROR.
         END.
   END.
END.            

IF Inserted.Clase_Producto EQ 2 THEN DO:
   FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ Inserted.Cod_Producto
              NO-LOCK NO-ERROR.
   IF AVAILABLE Pro_Creditos THEN DO:
      IF Pro_Creditos.Id_Asociado EQ 1 THEN
         IF Inserted.CtaDb_LiqAso    EQ "" OR Inserted.CtaCr_LiqAso    EQ "" OR Inserted.CtaDb_Liq    EQ "" OR
            Inserted.CtaCr_Liq       EQ "" OR Inserted.CtaInt_AntAso   EQ "" OR Inserted.CtaInt_Ant   EQ "" OR
            Inserted.CtaDb_MoraAso   EQ "" OR Inserted.CtaCr_MoraAso   EQ "" OR Inserted.CtaDb_Mora   EQ "" OR
            Inserted.CtaCr_Mora      EQ "" OR Inserted.CtaDb_DifCob    EQ "" OR Inserted.CtaCr_DifCob EQ "" OR
            Inserted.CtaDb_DifCobAso EQ "" OR Inserted.CtaCr_DifCobAso EQ ""                          THEN DO:
            MESSAGE "Producto para todos, las Cuentas" SKIP
                    "No pueden omitirse. Verifique."
            VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
            RETURN ERROR.
         END.
      IF Pro_Creditos.Id_Asociado EQ 2 THEN
         IF Inserted.CtaDb_LiqAso  EQ "" OR Inserted.CtaCr_LiqAso  EQ "" OR Inserted.CtaInt_AntAso   EQ "" OR
            Inserted.CtaDb_MoraAso EQ "" OR Inserted.CtaCr_MoraAso EQ "" OR Inserted.CtaDb_DifCobAso EQ "" OR
            Inserted.CtaCr_DifCobAso EQ ""                                                           THEN DO:
            MESSAGE "Producto para Asociado Las Cuentas" SKIP
                    "Asociado No pueden omitirse. Verifique."
            VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
            RETURN ERROR.
         END.
      IF Pro_Creditos.Id_Asociado EQ 3 THEN
         IF Inserted.CtaDb_Liq    EQ "" OR Inserted.CtaCr_Liq  EQ "" OR Inserted.CtaInt_Ant   EQ "" OR
            Inserted.CtaDb_Mora   EQ "" OR Inserted.CtaCr_Mora EQ "" OR Inserted.CtaDb_DifCob EQ "" OR
            Inserted.CtaCr_DifCob EQ ""                                                             THEN DO:
            MESSAGE "Producto para No Asociado Las Cuentas" SKIP
                    "No Asociado No pueden omitirse. Verifique."
            VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
            RETURN ERROR.
         END.
   END.
END.
