TRIGGER PROCEDURE FOR WRITE OF Cfg_Varios.
{INCLUIDO\variable.i "SHARED"}
DEF VAR W_Rpta       AS LOGICAL.
IF Cfg_Varios.Descripcion = ? OR Cfg_Varios.Tipo = ? THEN DO:
   RUN MostrarMensaje IN W_Manija (INPUT 330, OUTPUT W_Rpta).
   RETURN ERROR.
END.
