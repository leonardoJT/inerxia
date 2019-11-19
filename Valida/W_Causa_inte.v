IF Inserted.Cod_Producto EQ ? OR Inserted.Cod_Producto EQ 0 THEN DO:
   MESSAGE "Código del Producto no" SKIP
           "puede ser nulo. Verifique."
           VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
   RETURN ERROR.
END.

IF    Inserted.CtaDebito     EQ ? OR Inserted.CtaCredito     EQ ?
   OR Inserted.CtaDebito_Cau EQ ? OR Inserted.CtaCredito_Cau EQ ? THEN DO:
   MESSAGE "Las Cuentas NO pueden" SKIP
           "ser nulas. Verifique."
           VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
  RETURN ERROR.
END.

IF Inserted.Clase_Producto EQ 1 THEN DO:
   FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Cod_Producto EQ Inserted.Cod_Producto
              NO-LOCK NO-ERROR.
   IF AVAILABLE Pro_Ahorros THEN DO:
      IF Pro_Ahorros.id_Asociado EQ 1 THEN DO:
         IF    Inserted.CtaDebito      EQ ""
            OR Inserted.CtaCredito     EQ ""
            OR Inserted.CtaDebito_Cau  EQ ""
            OR Inserted.CtaCredito_Cau EQ "" THEN DO:
            MESSAGE "Producto para todos, las Cuentas" SKIP
                    "No pueden omitirse. Verifique."
                    VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
            RETURN ERROR.
         END.
      END.
      IF Pro_Ahorros.id_Asociado EQ 2 THEN DO:
         IF    Inserted.CtaDebito      EQ ""
            OR Inserted.CtaCredito     EQ "" THEN DO:
            MESSAGE "Producto para Asociado Las Cuentas" SKIP
                    "Asociado No pueden omitirse. Verifique."
                    VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
            RETURN ERROR.
         END.
      END.
      IF Pro_Ahorros.id_Asociado EQ 3 THEN DO:
         IF    Inserted.CtaDebito_Cau      EQ ""
            OR Inserted.CtaCredito_Cau     EQ "" THEN DO:
            MESSAGE "Producto para No Asociado Las Cuentas" SKIP
                    "No Asociado No pueden omitirse. Verifique."
                    VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
            RETURN ERROR.
         END.
      END.
   END.
END.
IF Inserted.Clase_Producto EQ 2 THEN DO:
   FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Producto EQ Inserted.Cod_Producto
              NO-LOCK NO-ERROR.
   IF AVAILABLE Pro_Creditos THEN DO:
      IF Pro_Creditos.Pro_Asociado EQ 1 THEN DO:
         IF    Inserted.CtaDebito      EQ ""
            OR Inserted.CtaCredito     EQ ""
            OR Inserted.CtaDebito_Cau  EQ ""
            OR Inserted.CtaCredito_Cau EQ "" THEN DO:
            MESSAGE "Producto para todos, las Cuentas" SKIP
                    "No pueden omitirse. Verifique."
                    VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
            RETURN ERROR.
         END.
      END.
      IF Pro_Creditos.Pro_Asociado EQ 2 THEN DO:
         IF    Inserted.CtaDebito      EQ ""
            OR Inserted.CtaCredito     EQ "" THEN DO:
            MESSAGE "Producto para Asociado Las Cuentas" SKIP
                    "Asociado No pueden omitirse. Verifique."
                    VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
            RETURN ERROR.
         END.
      END.
      IF Pro_Creditos.Pro_Asociado EQ 3 THEN DO:
         IF    Inserted.CtaDebito_Cau      EQ ""
            OR Inserted.CtaCredito_Cau     EQ "" THEN DO:
            MESSAGE "Producto para No Asociado Las Cuentas" SKIP
                    "No Asociado No pueden omitirse. Verifique."
                    VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
            RETURN ERROR.
         END.
      END.
   END.
END.
