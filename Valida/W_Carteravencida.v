IF Inserted.Cod_Producto EQ 0 OR Inserted.Cod_Producto EQ ? THEN DO:
   MESSAGE "Código Producto no puede ser" SKIP(0)
           "blanco o nulo. Verifique."
           VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
   RETURN ERROR.   
END.

IF Inserted.Per_Inicial GE Inserted.Per_Final THEN DO:
   MESSAGE "Plazo Final menor o igual" SKIP(0)
           "que Inicial. Verifique."
           VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
   RETURN ERROR.
END.

/*IF Inserted.Per_Inicial EQ ? OR Inserted.Per_Final EQ ? OR
   Inserted.Per_Inicial EQ 0 OR Inserted.Per_Final EQ 0 THEN DO:
   MESSAGE "Plazo Final o Inicial igual" SKIP(0)
           "a nulo ó a cero. Verifique."
           VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
   RETURN ERROR.
 END.*/

FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ Inserted.Cod_Producto
                        NO-LOCK NO-ERROR.
IF AVAILABLE Pro_Creditos THEN DO:
   IF Inserted.Id_Interes THEN DO:
      IF Pro_Creditos.Id_Asociado EQ 1 THEN
         IF    Inserted.Cta_AsoAdDb    EQ "" OR Inserted.Cta_AsoIntAdCr EQ "" OR Inserted.Cta_AsoIntAdDb EQ ""
            OR Inserted.Cta_AsoIntNaCr EQ "" OR Inserted.Cta_AsoIntNaDb EQ "" OR Inserted.Cta_AsoNaDb    EQ ""
            OR Inserted.Cta_AsoPrvAdCr EQ "" OR Inserted.Cta_AsoPrvAdDb EQ "" OR Inserted.Cta_AsoPrvNaCr EQ ""
            OR Inserted.Cta_AsoPrvNaDb EQ "" OR Inserted.Cta_NoaAdDb    EQ "" OR Inserted.Cta_NoaIntAdCr EQ ""
            OR Inserted.Cta_NoaIntAdDb EQ "" OR Inserted.Cta_NoaIntNaCr EQ "" OR Inserted.Cta_NoaIntNaDb EQ ""
            OR Inserted.Cta_NoaNaDb    EQ "" OR Inserted.Cta_NoaPrvAdCr EQ "" OR Inserted.Cta_NoaPrvAdDb EQ ""
            OR Inserted.Cta_NoaPrvNaCr EQ "" OR Inserted.Cta_NoaPrvNaDb EQ "" THEN DO:
            MESSAGE "Producto para todos, las Cuentas" SKIP
                    "No pueden omitirse. Verifique." inserted.id_interes
                    VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
            RETURN ERROR.
         END.
      IF Pro_Creditos.id_Asociado EQ 2 THEN
         IF    Inserted.Cta_AsoAdDb    EQ "" OR Inserted.Cta_AsoIntAdCr EQ "" OR Inserted.Cta_AsoIntAdDb EQ ""
            OR Inserted.Cta_AsoIntNaCr EQ "" OR Inserted.Cta_AsoIntNaDb EQ "" OR Inserted.Cta_AsoNaDb    EQ ""
            OR Inserted.Cta_AsoPrvAdCr EQ "" OR Inserted.Cta_AsoPrvAdDb EQ "" OR Inserted.Cta_AsoPrvNaCr EQ ""
            OR Inserted.Cta_AsoPrvNaDb EQ "" THEN DO:
            MESSAGE "Producto para Asociado, las Cuentas" SKIP
                    "Asociado No pueden omitirse. Verifique." inserted.id_interes
                    VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
            RETURN ERROR.
         END.
      IF Pro_Creditos.Id_Asociado EQ 3 THEN
         IF    Inserted.Cta_NoaAdDb    EQ "" OR Inserted.Cta_NoaIntAdCr EQ "" OR Inserted.Cta_NoaIntAdDb EQ ""
            OR Inserted.Cta_NoaIntNaCr EQ "" OR Inserted.Cta_NoaIntNaDb EQ "" OR Inserted.Cta_NoaNaDb    EQ ""
            OR Inserted.Cta_NoaPrvAdCr EQ "" OR Inserted.Cta_NoaPrvAdDb EQ "" OR Inserted.Cta_NoaPrvNaCr EQ ""
            OR Inserted.Cta_NoaPrvNaDb EQ "" THEN DO:
            MESSAGE "Producto para No Asociado, las Cuentas" SKIP
                    "No Asociado No pueden omitirse. Verifique." inserted.id_interes
                    VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
            RETURN ERROR.
         END.
   END.
   ELSE DO:
      IF Pro_Creditos.Id_Asociado EQ 1 THEN
         IF     Inserted.Cta_AsoAdDb   EQ "" OR Inserted.Cta_AsoNaDb    EQ "" OR Inserted.Cta_AsoPrvAdCr EQ ""
            OR Inserted.Cta_AsoPrvAdDb EQ "" OR Inserted.Cta_AsoPrvNaCr EQ "" OR Inserted.Cta_AsoPrvNaDb EQ ""
            OR Inserted.Cta_NoaAdDb    EQ "" OR Inserted.Cta_NoaNaDb    EQ "" OR Inserted.Cta_NoaPrvAdCr EQ ""
            OR Inserted.Cta_NoaPrvAdDb EQ "" OR Inserted.Cta_NoaPrvNaCr EQ "" OR Inserted.Cta_NoaPrvNaDb EQ "" 
            THEN DO:
            MESSAGE "Producto para todos, las Cuentas" SKIP
                    "No pueden omitirse. Verifique."
                    VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
            RETURN ERROR.
         END.
      IF Pro_Creditos.Id_Asociado EQ 2 THEN
         IF    Inserted.Cta_AsoAdDb    EQ "" OR Inserted.Cta_AsoNaDb    EQ "" OR Inserted.Cta_AsoPrvAdCr EQ "" 
            OR Inserted.Cta_AsoPrvAdDb EQ "" OR Inserted.Cta_AsoPrvNaCr EQ "" OR Inserted.Cta_AsoPrvNaDb EQ "" 
            THEN DO:
            MESSAGE "Producto para Asociado, las Cuentas" SKIP
                    "Asociado No pueden omitirse. Verifique."
                    VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
            RETURN ERROR.
         END.
      IF Pro_Creditos.Id_Asociado EQ 3 THEN
         IF    Inserted.Cta_NoaAdDb    EQ "" OR Inserted.Cta_NoaNaDb    EQ "" OR Inserted.Cta_NoaPrvAdCr EQ ""
            OR Inserted.Cta_NoaPrvAdDb EQ "" OR Inserted.Cta_NoaPrvNaCr EQ "" OR Inserted.Cta_NoaPrvNaDb EQ ""
            THEN DO:
            MESSAGE "Producto para No Asociado, las Cuentas" SKIP
                    "No Asociado No pueden omitirse. Verifique."
                    VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
            RETURN ERROR.
         END.         
   END.
END.
ELSE DO:
   MESSAGE "Producto de Crédito" SKIP
           "no existe. Verifique."
           VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
   RETURN ERROR.
END.
