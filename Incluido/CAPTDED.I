/**************************************************************************
 INCLUDE : Captura los deducibles para los creditos de acuerdo al producto

**************************************************************************/
  DEFINE VAR W_CodBas LIKE Deducible.Cod_Deducible.
  DEFINE VAR W_NomBas LIKE Deducible.Nom_Deducible.
  DEFINE VAR W_WidSel AS   HANDLE.
  
  W_WidSel = LAST-EVENT:WIDGET-ENTER.
/*  IF W_WidSel:LABEL = "Btn-Cancel" THEN
    RETURN.*/
  DO WITH FRAME {&FRAME-NAME}:
    IF LAST-EVENT:LABEL = "ENTER" OR LAST-EVENT:LABEL = "TAB" THEN
     DO:
       ASSIGN W_CodBas = {1}:SCREEN-VALUE.
       FIND Deducible WHERE W_CodBas = Deducible.Cod_Deducible AND
                                       Deducible.Estado = 1 NO-LOCK NO-ERROR.
       IF AVAILABLE(Deducible) THEN
          ASSIGN W_CodBas = Deducible.Cod_Deducible
                 W_NomBas = Deducible.Nom_Deducible
                 {1}:SCREEN-VALUE = W_CodBas
                 {2}:SCREEN-VALUE = W_NomBas.
       ELSE
         DO:               
           RUN P-Brwded.r (OUTPUT W_CodBas,OUTPUT W_NomBas).
           IF TRIM(W_CodBas) = "" THEN
            DO:               
              APPLY "ENTRY" TO {1}.
              RETURN NO-APPLY.
            END.
           ELSE
             ASSIGN {1}:SCREEN-VALUE = W_CodBas
                    {2}:SCREEN-VALUE = W_NomBas.
         END.
         IF TRIM({1}:SCREEN-VALUE) = "" THEN
                 {2}:SCREEN-VALUE = "".
     END.
   else
     return.  
  END.       
