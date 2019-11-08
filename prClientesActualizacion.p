/*
Proposito   : Listar a archivo txt los registros de clientes con datos actualizados a una fecha
Date        : Nov/06/07
By          : Giocam
programa llamado desde : wInfSipla.w
*/

{incluido\iprmt_rpt.i}
{incluido\igetfecha.i}
{incluido\igetSdo.i}
{incluido\Variable.i "SHARED"}


DEFINE VARIABLE qbf-count    AS INTEGER NO-UNDO.
DEFINE VARIABLE qbf-governor AS INTEGER NO-UNDO.
 
DEFINE VARIABLE qbf-govcnt AS INTEGER NO-UNDO.
DEFINE VARIABLE qbf-loop   AS INTEGER NO-UNDO.
DEFINE VARIABLE qbf-time   AS INTEGER NO-UNDO.

DEFINE VARIABLE viAge1 AS INTEGER     NO-UNDO.
DEFINE VARIABLE viAge2 AS INTEGER     NO-UNDO.
DEFINE VARIABLE viEstado AS INTEGER   NO-UNDO.
DEFINE VARIABLE vcCliente AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vcEstado AS CHARACTER   NO-UNDO.

DEFINE BUFFER Clientes FOR Clientes.

ASSIGN
  qbf-count    = 0
  qbf-governor = 0
  qbf-time     = TIME.

ASSIGN 
    viAge1 = INTEGER(pc01)
    viAge2 = INTEGER(pc02).
ASSIGN viEstado = INTEGER(pc03).

IF P_NomArchivo EQ "DEFAULT" THEN
    ASSIGN P_NomArchivo = SESSION:TEMP-DIRECTORY + "ClientesActualizados.txt".

OUTPUT TO VALUE(P_NomArchivo) PAGED PAGE-SIZE VALUE(P_NumLineas).

{incluido\RepHeader.i}

VIEW FRAME F-Encabezado.

W_Reporte   = "REPORTE   : Clientes: " + P_Titulo + " (prClientesActualizacion.p) " 
              + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").

main-loop:
FOR EACH Clientes WHERE
    (Clientes.Tipo_Vinculo EQ 1) AND
    ((Clientes.Agencia >= viAge1 AND Clientes.Agencia <= viAge2) OR (viAge1 = 0 AND viAge2 = 0)) AND
    ((Clientes.Fec_UltActualiza >= pdt01 AND Clientes.Fec_UltActualiza <= pdt02) OR (pdt01 EQ ? AND pdt02 EQ ?)) AND
    (Clientes.estado EQ viEstado OR viEstado = 0) AND
    TRUE
    NO-LOCK
  BREAK BY Clientes.Agencia
    BY Clientes.Apellido1:

    ASSIGN vcCliente = Clientes.Apellido1 + " " + Clientes.Apellido2 + " " + Clientes.Nombre
        vcEstado = IF Clientes.Estado = 1 THEN "Activo" ELSE "Inactivo".

  qbf-count  = qbf-count + 1.
    IF FIRST-OF(Clientes.Agencia) AND NOT FIRST(Clientes.Agencia) THEN PAGE.

  FORM
    Clientes.Agencia COLUMN-LABEL "Age." FORMAT "999"
    Clientes.Nit COLUMN-LABEL "Nit" FORMAT "X(12)"
    vcCliente COLUMN-LABEL "Cliente" FORMAT "X(40)"
    vcEstado COLUMN-LABEL "Estado" FORMAT "x(5)"
    Clientes.Cod_Empresa COLUMN-LABEL "Cód Empr." FORMAT "9999"
    Clientes.Tel_Residencia COLUMN-LABEL "Tel. Res." FORMAT "X(20)"
    Clientes.Tel_comercial COLUMN-LABEL "Tel.Comercial" FORMAT "X(20)"
    Clientes.Fec_UltActualiza COLUMN-LABEL "Ult. Act." FORMAT "99/99/9999"
    WITH FRAME Frep DOWN COLUMN 1 WIDTH 217
    NO-ATTR-SPACE NO-VALIDATE NO-BOX USE-TEXT STREAM-IO.

  DISPLAY
    Clientes.Agencia
    Clientes.Nit
    vcCliente
    vcEstado
    Clientes.Cod_Empresa
    Clientes.Tel_Residencia
    Clientes.Tel_comercial
    Clientes.Fec_UltActualiza
    WITH FRAME Frep.

  DOWN WITH FRAME Frep.
END.


  VIEW FRAME F-Ftr.

/* PAGE. */
OUTPUT CLOSE.
RETURN.
