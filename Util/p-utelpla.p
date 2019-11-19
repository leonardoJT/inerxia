/* -------------------------------------------------------------------------- *
   Programa: p-utelper.p                                                      *
  ---------------------------------------------------------------------------*/
DEFINE VARIABLE W_Contador AS INTEGER LABEL "No Registros".
DISABLE TRIGGERS FOR LOAD OF Pro_Grupo.

MESSAGE "Este Proceso ELIMINA los registros de Pro_Grupo" SKIP
        "está seguro de Continuar?                      "
   VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO
   TITLE "Eliminacion R.Pro_Grupo"
   UPDATE W_Respuesta AS LOGICAL.

IF NOT W_Respuesta THEN RETURN.
SESSION:SET-WAIT-STATE("GENERAL").
ASSIGN W_Contador = 0.
DO TRANSACTION:
   FOR EACH Pro_Grupo WITH 1 DOWN SIDE-LABELS ROW 8 COLUMN 30:
       ASSIGN W_Contador = W_Contador + 1.
       PAUSE 0 BEFORE-HIDE.
       DISPLAY W_contador.
       DELETE Pro_Grupo.
   END.
END.
SESSION:SET-WAIT-STATE("").
