/*
Proposito   : Listar a archivo txt los registros de Mov_insSipla, 
              para seguimiento de la Gestión SIPLA
Date        : Nov/02/07
By          : Giocam
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

DEFINE VARIABLE viIns1 AS INTEGER     NO-UNDO.
DEFINE VARIABLE viIns2 AS INTEGER     NO-UNDO.
DEFINE VARIABLE viAge1 AS INTEGER     NO-UNDO.
DEFINE VARIABLE viAge2 AS INTEGER     NO-UNDO.

DEFINE VARIABLE vlEstado AS LOGICAL     NO-UNDO.


DEFINE BUFFER Mov_InsSipla FOR Mov_InsSipla.

ASSIGN
  qbf-count    = 0
  qbf-governor = 0
  qbf-time     = TIME.

ASSIGN 
    viIns1 = INTEGER(pc01)
    viIns2 = INTEGER(pc02)
    viAge1 = INTEGER(pc03)
    viAge2 = INTEGER(pc04).
CASE pc07:
    WHEN "TRUE" THEN    ASSIGN vlEstado = TRUE.
    WHEN "FALSE" THEN   ASSIGN vlEstado = FALSE.
    WHEN "?" THEN       ASSIGN vlEstado = ?.
END CASE.

IF P_NomArchivo EQ "DEFAULT" THEN
    ASSIGN P_NomArchivo = SESSION:TEMP-DIRECTORY + "GestionSipla.txt".

OUTPUT TO VALUE(P_NomArchivo) PAGED PAGE-SIZE VALUE(P_NumLineas).

/* DEFINE VARIABLE W_Nom_Entidad AS CHARACTER INITIAL "JURISCOOP" NO-UNDO.        */
/* DEFINE VARIABLE W_Usuario AS CHARACTER INITIAL "911" NO-UNDO.                  */
/* DEFINE VARIABLE W_Estacion LIKE Estaciones.Estacion.                           */
/* DEFINE VARIABLE W_Nom_Agencia AS CHARACTER INITIAL "Agencia Giovanni" NO-UNDO. */

DO FOR Mov_InsSipla:

    {incluido\RepHeader.i}
    
    VIEW FRAME F-Encabezado.
    W_Reporte   = "REPORTE   : SIPLA: " + P_Titulo + " (prMov_InsSipla.p) " 
                  + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").



  main-loop:
  FOR EACH Mov_InsSipla NO-LOCK WHERE
      ((Mov_InsSipla.Fecha_Transaccion >= pdt01 AND Mov_InsSipla.Fecha_Transaccion <= pdt02) OR (pdt01 EQ ? AND pdt02 EQ ?))AND
      ((Mov_InsSipla.instancia >= viIns1 AND Mov_InsSipla.instancia <= viIns2) OR (viIns1 = 0 AND viIns2 = 0)) AND
      ((Mov_InsSipla.Agencia >= viAge1 AND Mov_InsSipla.Agencia <= viAge2) OR (viAge1 = 0 AND viAge2 = 0)) AND
      ((Mov_InsSipla.UsuGestiona >= pc05 AND Mov_InsSipla.UsuGestiona >= pc06) OR (pc05 EQ "" OR pc06 EQ "")) AND
      (Mov_InsSipla.estado EQ vlEstado OR vlEstado EQ ?) AND
        TRUE
    BREAK BY Mov_InsSipla.Agencia
      BY Mov_InsSipla.UsuReporta:

    qbf-count  = qbf-count + 1.

    FORM
      Mov_InsSipla.Instancia COLUMN-LABEL "Inst." FORMAT "999"
      Mov_InsSipla.Agencia COLUMN-LABEL "Age." FORMAT "999"
      Mov_InsSipla.Nit COLUMN-LABEL "Cliente" FORMAT "X(40)"
      Mov_InsSipla.Fecha_Transaccion COLUMN-LABEL "Fec.Trans." FORMAT "99/99/99"
      Mov_InsSipla.Estado COLUMN-LABEL "Estado" FORMAT "Gest./No Gest."
      Mov_InsSipla.Fecha_Gestion COLUMN-LABEL "Fec. Gest." FORMAT "99/99/99"
      Mov_InsSipla.UsuReporta COLUMN-LABEL "Usu.Repor." FORMAT "X(4)"
      Mov_InsSipla.UsuGestiona COLUMN-LABEL "Usu.Gest." FORMAT "X(40)"      
      Mov_InsSipla.descripcion COLUMN-LABEL "Descripción" FORMAT "X(80)"
      WITH FRAME Frep DOWN COLUMN 1 WIDTH 340
      NO-ATTR-SPACE NO-VALIDATE NO-BOX USE-TEXT STREAM-IO.

    DISPLAY
      Mov_InsSipla.Instancia
      Mov_InsSipla.Agencia
      /*Mov_InsSipla.Nit*/
        getCliente(INPUT Mov_InsSipla.Nit) @ nit
      Mov_InsSipla.Fecha_Transaccion
      Mov_InsSipla.Estado
      Mov_InsSipla.Fecha_Gestion
      Mov_InsSipla.UsuReporta
      getUsuario(INPUT Mov_InsSipla.UsuGestiona) @ Mov_InsSipla.UsuGestiona
      Mov_InsSipla.descripcion 
      WITH FRAME Frep.

    DOWN WITH FRAME Frep.
  END.

/*   PAGE. */

  VIEW FRAME F-Ftr.

END.



OUTPUT CLOSE.
RETURN.
