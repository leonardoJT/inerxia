IF Inserted.Cod_Producto EQ ? THEN DO: 
   MESSAGE "Código del Producto no" SKIP
           "puede ser nulo. Verifique."
           VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
   RETURN ERROR.
END.

IF Inserted.Plazo_Inicial GE Inserted.Plazo_Final THEN DO:
   MESSAGE "Plazo Final menor o igual" SKIP
           "que Inicial. Verifique."
           VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
   RETURN ERROR.
END.

IF Inserted.Plazo_Inicial EQ ? OR Inserted.Plazo_Final EQ ? OR
   Inserted.Plazo_Final EQ 0 THEN DO:
   MESSAGE "Plazo Final Igual a nulo o Cero/" SKIP
           "o Inicial igual a  nulo. Verifique."
           VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
   RETURN ERROR.
END.

IF Inserted.Clase_Producto EQ 1 THEN DO:
   FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ Inserted.Cod_Producto
        NO-LOCK NO-ERROR.
   IF AVAILABLE Pro_Ahorros THEN DO:
      IF Pro_Ahorros.Id_Asociado EQ 1 THEN DO:
         IF Inserted.Cta_AsoAd EQ "" OR Inserted.Cta_NoaAd EQ "" THEN DO:
            MESSAGE "Producto para todos, las Cuentas" SKIP
                    "No pueden omitirse. Verifique."
            VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
            RETURN ERROR.
         END.
      END.
      IF Pro_Ahorros.Id_Asociado EQ 2 THEN DO:
         IF Inserted.Cta_AsoAd EQ "" THEN DO:
            MESSAGE "Producto para Asociado, las Cuentas" SKIP
                    "Asociado No pueden omitirse. Verifique."
            VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
            RETURN ERROR.
         END.
      END.
      IF Pro_Ahorros.Id_Asociado EQ 3 THEN DO:
         IF Inserted.Cta_NoaAd EQ "" THEN DO:
            MESSAGE "Producto para No Asociado, las Cuentas" SKIP
                    "No Asociado No pueden omitirse. Verifique."
            VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
            RETURN ERROR.
         END.
      END.
   END.
END.
IF Inserted.Clase_Producto EQ 2 THEN DO:
   FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ Inserted.Cod_Producto
              NO-LOCK NO-ERROR.
   IF AVAILABLE Pro_Creditos THEN DO:
      IF Pro_Creditos.Id_Asociado EQ 1 THEN DO:
         IF Inserted.Cta_AsoAd EQ "" OR Inserted.Cta_NoaAd  EQ "" OR
            Inserted.Cta_AsoNa EQ "" OR Inserted.Cta_NoaNa  EQ "" THEN DO:
            MESSAGE "Producto para todos, las Cuentas de" SKIP
                    "Garantías No pueden omitirse. Verifique."
            VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
            RETURN ERROR.
         END.
      END.
      IF Pro_Creditos.Id_Asociado EQ 2 THEN DO:
         IF Inserted.Cta_AsoAd EQ "" OR Inserted.Cta_AsoNa EQ "" THEN DO:
            MESSAGE "Producto para Asociado, las Cuentas de" SKIP
                    "Garantías Asociado No pueden omitirse. Verifique."
            VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
            RETURN ERROR.
         END.
      END.      
      IF Pro_Creditos.Id_Asociado EQ 3 THEN DO:
         IF Inserted.Cta_NoaAd EQ "" OR Inserted.Cta_NoaNa EQ "" THEN DO:
            MESSAGE "Producto para No Asociado, las Cuentas de" SKIP
                    "Garantías No Asociado No pueden omitirse. Verifique."
            VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
            RETURN ERROR.
         END.
      END.      
   END.
END.
