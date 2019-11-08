DEFINE INPUT PARAMETER WNit LIKE Clientes.Nit.
DEFINE INPUT PARAMETER FecI AS DATE.
DEFINE INPUT PARAMETER FecF AS DATE.
DEFINE OUTPUT PARAMETER WVal LIKE Ahorros.Sdo_Disponible.

{Incluido/Variable.I "SHARED"}
DEFINE VAR DiasT    AS INTEGER FORMAT "999".
DEFINE VAR Otros    AS INTEGER FORMAT ">>>,>>>,>>9.99".
DEFINE VAR Desde    AS DATE.
DEFINE VAR hasta    AS DATE.
DEFINE VAR Salario  AS DECIMAL FORMAT ">>>,>>>,>>>.99".

FIND Empleados WHERE Empleados.Nit EQ WNit NO-LOCK NO-ERROR.
IF NOT AVAILABLE Empleados THEN DO:
   MESSAGE "El empleado identificado con el nit: " WNit SKIP
           "no existe en el archivo de empleados de la" SKIP
           "cooperativa Se ha causado un error en el proceso" SKIP
           "de PRIMA LEGAL, para este empleado en especial" VIEW-AS ALERT-BOX.
   RETURN ERROR.
END.
ELSE DO:
  DiasT = W_Fecha - Empleados.Fec_Ingreso.
  IF DiasT GT 180 THEN DiasT = 180.

FOR EACH Ahorros WHERE Ahorros.Nit EQ WNit AND
                       Ahorros.Estado EQ 1 AND
                       Ahorros.Cuota GT 0  AND
                       Ahorros.FOR_Pago EQ 2 NO-LOCK:
    WVal = WVal + Ahorros.Cuota.
END.
FOR EACH Creditos WHERE Creditos.Nit    EQ WNit AND
                        Creditos.Estado EQ 2 AND
                        Creditos.FOR_Pago EQ 2 NO-LOCK:
    WVal = WVal + Creditos.Cuota.
END.

END.
