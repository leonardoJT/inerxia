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

/*IF MONTH(W_Fecha) EQ 6 THEN
   ASSIGN Desde = DATE("01/01/" + STRING(YEAR(W_Fecha))).
          Hasta = DATE("30/06/" + STRING(YEAR(W_Fecha))).

IF MONTH(W_Fecha) EQ 6 THEN
   ASSIGN Desde = DATE("01/07/" + STRING(YEAR(W_Fecha))).
          Hasta = DATE("31/12/" + STRING(YEAR(W_Fecha))).*/

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
/*suma al valor los conceptos que tienen marcado id_salario que es el que
identifica los conceptos que hace parte de salario a la hora de hacer
calculos para los proceso especiales*/
FOR EACH Cfg_Novedades WHERE Cfg_Novedades.Id_Salario NO-LOCK:
  Otros = 0.
  FOR EACH Novedades_Nomina WHERE /*contador de comisiones/aux.transporte*/
           Novedades_Nomina.Tipo   EQ 1        AND
           Novedades_Nomina.Codigo EQ Cfg_Novedades.Codigo AND
           Novedades_Nomina.Nit    EQ WNit     AND
           Novedades_Nomina.Fecha  GE FecI     AND
           Novedades_Nomina.Fecha  LE FecF     AND
           Novedades_Nomina.Estado_Liquidacion EQ 2 NO-LOCK:
      Otros = Otros + Novedades_Nomina.Valor.
  END.
  Salario = (Empleados.Salario_Mensual / 2) + Otros.
  WVal = (DiasT * Salario) / 180.
END.
END.
