
/*
Proposito   : Listar a archivo txt los Usuarios de tipo 6 (SIPLA), 
              
Date        : Dic/11/07
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

DEFINE VARIABLE viEstado AS INTEGER     NO-UNDO.
DEFINE VAR vcObs AS CHARACTER FORMAT "X(10)".


ASSIGN
  qbf-count    = 0
  qbf-governor = 0
  qbf-time     = TIME.

ASSIGN 
    viIns1 = INTEGER(pc01)
    viIns2 = INTEGER(pc02)
    viAge1 = INTEGER(pc03)
    viAge2 = INTEGER(pc04)
    viEstado = INTEGER(pc07).

IF P_NomArchivo EQ "DEFAULT" THEN
    ASSIGN P_NomArchivo = SESSION:TEMP-DIRECTORY + "ListaUsuariosSIPLA.txt".

OUTPUT TO VALUE(P_NomArchivo) PAGED PAGE-SIZE VALUE(P_NumLineas).


DO FOR cfg_instancias:
    {incluido\RepHeader.i}
    
    VIEW FRAME F-Encabezado.
    W_Reporte   = "REPORTE   : " + P_Titulo + " (prSiplaListaUsuarios.p) " 
                  + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").

    main-loop:
    FOR EACH cfg_instancias WHERE tipo_instancia = 6 AND
        ((cfg_instancias.Instancia >= viIns1 AND cfg_instancias.instancia <= viIns2) OR (viIns1 = 0 AND viIns2 = 0)) AND
        ((cfg_instancias.Agencia >= viAge1 AND cfg_instancias.Agencia <= viAge2) OR (viAge1 = 0 AND viAge2 = 0)) AND
        ((cfg_instancias.usuario >= pc05 AND cfg_instancias.usuario >= pc06) OR (pc05 EQ "" OR pc06 EQ "")) AND
        (cfg_instancias.estado EQ viEstado OR viEstado EQ 0) AND
        TRUE
        NO-LOCK BY agencia:
      FIND FIRST instancias WHERE instancias.instancia       = cfg_instancias.instancia NO-LOCK NO-ERROR.
      FIND FIRST usuarios   WHERE usuarios.usuario = cfg_instancias.usuario NO-LOCK NO-ERROR.
    
      FORM
          cfg_instancias.instancia  COLUMN-LABEL "Cod.Ins"    FORMAT "999"
          Instancias.Nom_Instancia  COLUMN-LABEL "Nombre Instancia"    FORMAT "X(30)"
          cfg_instancias.agencia    COLUMN-LABEL "Age."    FORMAT "999"
          usuarios.usuario          COLUMN-LABEL "Usu."    FORMAT "999"
          usuario.nombre            COLUMN-LABEL "Nombre USuario"    FORMAT "X(40)"
          vcObs                     COLUMN-LABEL "Estado"    FORMAT "x(10)"
          WITH FRAME Frep DOWN COLUMN 1 WIDTH 120
          NO-ATTR-SPACE NO-VALIDATE NO-BOX USE-TEXT STREAM-IO.
      IF AVAILABLE(usuarios) THEN DO:
         IF cfg_instancia.estado = 1 THEN vcObs = 'Activo'.
         ELSE vcObs = 'Inactivo'.
         DISPLAY 
             cfg_instancias.instancia     
             Instancias.Nom_Instancia     
             cfg_instancias.agencia       
             usuarios.usuario             
             usuario.nombre               
             vcObs                        
             WITH FRAME Frep.
         DOWN WITH FRAME Frep.
      END.
    END.    

END.

OUTPUT CLOSE.
RETURN.
