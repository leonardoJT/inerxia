&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */


{INCLUIDO\VARIABLE.I "SHARED"}

DEFINE TEMP-TABLE NoHabiles LIKE habiles
    FIELD motivo AS CHARACTER FORMAT "X(80)".
DEF VAR W_Sarlaft AS LOG INIT FALSE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME brwHabiles

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES habiles

/* Definitions for BROWSE brwHabiles                                    */
&Scoped-define FIELDS-IN-QUERY-brwHabiles habiles.agencia habiles.nit habiles.nombre habiles.apellido1 habiles.apellido2   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brwHabiles   
&Scoped-define SELF-NAME brwHabiles
&Scoped-define QUERY-STRING-brwHabiles FOR EACH habiles NO-LOCK BY habiles.agencia                                                  BY habiles.nit
&Scoped-define OPEN-QUERY-brwHabiles OPEN QUERY {&SELF-NAME} FOR EACH habiles NO-LOCK BY habiles.agencia                                                  BY habiles.nit.
&Scoped-define TABLES-IN-QUERY-brwHabiles habiles
&Scoped-define FIRST-TABLE-IN-QUERY-brwHabiles habiles


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-brwHabiles}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnGenerar btnImprimir brwHabiles 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnGenerar 
     LABEL "Generar" 
     SIZE 15 BY 1.12.

DEFINE BUTTON btnImprimir 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/impresora2.bmp":U
     LABEL "Imprimir" 
     SIZE 10 BY 1.62 TOOLTIP "Imprimir".

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brwHabiles FOR 
      habiles SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brwHabiles
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brwHabiles C-Win _FREEFORM
  QUERY brwHabiles DISPLAY
      habiles.agencia    COLUMN-LABEL "Oficina"
 habiles.nit        COLUMN-LABEL "Cédula"
 habiles.nombre     COLUMN-LABEL "Nombre(s)"
 habiles.apellido1  COLUMN-LABEL "Primer Apellido"
 habiles.apellido2  COLUMN-LABEL "Segundo Apellido"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS DROP-TARGET SIZE 112 BY 12.92
         TITLE "Asociados Hábiles" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnGenerar AT ROW 1.54 COL 35 WIDGET-ID 4
     btnImprimir AT ROW 1.54 COL 105 WIDGET-ID 6
     brwHabiles AT ROW 3.69 COL 3 WIDGET-ID 200
     "Asociados Hábiles 2012 -->" VIEW-AS TEXT
          SIZE 31 BY 1.35 AT ROW 1.54 COL 4 WIDGET-ID 2
          FONT 0
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 114.72 BY 16 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Generación / Impresión de Asociados Hábiles"
         HEIGHT             = 16
         WIDTH              = 114.72
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 114.72
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 114.72
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB brwHabiles btnImprimir DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brwHabiles
/* Query rebuild information for BROWSE brwHabiles
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH habiles NO-LOCK BY habiles.agencia
                                                 BY habiles.nit.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brwHabiles */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Generación / Impresión de Asociados Hábiles */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Generación / Impresión de Asociados Hábiles */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnGenerar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnGenerar C-Win
ON CHOOSE OF btnGenerar IN FRAME DEFAULT-FRAME /* Generar */
DO:
    DEFINE VAR esperado AS DECIMAL.
    DEFINE VAR pagado AS DECIMAL.

    IF btnGenerar:LABEL = "Generar" THEN DO:
        MESSAGE "Este proceso es posible realizarlo únicamente hasta las" SKIP
                "12:00  p.m. del día 24 de enero. A partir de esta fecha" SKIP
                "sólo  será  posible imprimir el informa con los Asociados" SKIP
                "Hábiles"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        MESSAGE "Inicia proceso de generación de Asociados Hábiles." SKIP
                "El  listado  actual  se  borrará  y  se  procede a" SKIP
                "generar  un  nuevo listado. ¿Está seguro que desea" SKIP
                "realizar esta operación...?"
            VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE flagGenerar AS LOGICAL.

        IF flagGenerar = NO THEN
            RETURN NO-APPLY.

        /* Borramos la tabla Habiles */
        FOR EACH habiles:
            DELETE habiles.
        END.

        OPEN QUERY brwHabiles FOR EACH habiles NO-LOCK BY habiles.agencia
                                                       BY habiles.nit.

        EMPTY TEMP-TABLE NoHabiles.

        /* Se recorren todos los Aportes (Asociados) */
        FOR EACH ahorros WHERE ahorros.cod_ahorro = 2
                           AND ahorros.sdo_disponible > 0 NO-LOCK:
            FIND FIRST clientes WHERE clientes.nit = ahorros.nit NO-LOCK NO-ERROR.
            IF AVAILABLE clientes THEN DO:
                /* Buscamos en la lista de suspendidos */
                FIND FIRST listaNegra WHERE listaNegra.nit = clientes.nit NO-LOCK NO-ERROR.
                IF AVAILABLE listaNegra THEN DO:
                    CREATE NoHabiles.
                    ASSIGN NoHabiles.agencia = ahorros.agencia
                           NoHabiles.nit = clientes.nit
                           NoHabiles.nombre = clientes.nombre
                           NoHabiles.apellido1 = clientes.apellido1
                           NoHabiles.apellido2 = clientes.apellido2
                           NoHabiles.motivo = "El Asociado se encuentra suspendido".

                    NEXT.
                END.

                RUN validarUsuarioSarlaft.r (INPUT Clientes.Nit, OUTPUT W_Sarlaft) NO-ERROR.
                IF W_Sarlaft THEN
                    NEXT.

                /* Buscamos un posible crédito atrasado. En caso de encontrarlo, se descarta el Asociado */
                FIND FIRST creditos WHERE creditos.nit = ahorros.nit
                                      AND creditos.sdo_capital > 0
                                      AND creditos.dias_atraso > 0 NO-LOCK NO-ERROR.
                IF AVAILABLE creditos THEN DO:
                    CREATE NoHabiles.
                    ASSIGN NoHabiles.agencia = ahorros.agencia
                           NoHabiles.nit = clientes.nit
                           NoHabiles.nombre = clientes.nombre
                           NoHabiles.apellido1 = clientes.apellido1
                           NoHabiles.apellido2 = clientes.apellido2
                           NoHabiles.motivo = "Crédito #" + STRING(creditos.num_credito) + " con días de atraso".

                    NEXT.
                END.

                IF clientes.fec_ingreso = ? OR clientes.fec_ingreso <= 03/01/2011 THEN
                    esperado = ahorros.cuota * 10. /* 10 cuotas de marzo a diciembre */
                ELSE DO:
                    esperado = ahorros.cuota * TRUNCATE((01/24/2012 - clientes.fec_ingreso) / 30,0).
                END.
                
                pagado = 0.

                FOR EACH mov_ahorros WHERE mov_ahorros.nit = ahorros.nit
                                       AND mov_ahorros.cod_ahorro = ahorros.cod_ahorro
                                       AND mov_ahorros.cue_ahorro = ahorros.cue_ahorro
                                       AND mov_ahorros.fecha >= 03/01/2011
                                       AND mov_ahorros.fecha <= 01/24/2012 NO-LOCK:
                    FIND FIRST operacion WHERE operacion.cod_operacion = mov_ahorros.cod_operacion NO-LOCK NO-ERROR.
                    IF AVAILABLE operacion THEN DO:
                        IF operacion.tipo_operacion = 1 THEN
                            pagado = pagado + mov_ahorros.val_efectivo + mov_ahorros.val_cheque.

                        IF operacion.tipo_operacion = 2 THEN
                            pagado = pagado - mov_ahorros.val_efectivo - mov_ahorros.val_cheque.
                    END.
                END.

                IF pagado < esperado THEN DO:
                    CREATE NoHabiles.
                    ASSIGN NoHabiles.agencia = ahorros.agencia
                           NoHabiles.nit = clientes.nit
                           NoHabiles.nombre = clientes.nombre
                           NoHabiles.apellido1 = clientes.apellido1
                           NoHabiles.apellido2 = clientes.apellido2
                           NoHabiles.motivo = "No cumple con el valor esperado en los Aportes ($" + STRING(esperado,">>,>>>,>>9.99") + " Vs. $" + STRING(pagado,"->>,>>>,>>9.99") + ")".
                    
                    NEXT.
                END.

                CREATE Habiles.
                ASSIGN Habiles.agencia = ahorros.agencia
                       Habiles.nit = clientes.nit
                       Habiles.nombre = clientes.nombre
                       Habiles.apellido1 = clientes.apellido1
                       Habiles.apellido2 = clientes.apellido2.
            END.
        END.

        OUTPUT TO VALUE ("C:\INFO_Fodun\AsociadosNoHabiles" + STRING(DAY(w_fecha),"99") + STRING(MONTH(w_fecha),"99") + STRING(YEAR(w_fecha),"9999") + "_" + STRING(TIME,"99999") + ".txt").
        FOR EACH NoHabiles NO-LOCK BY noHabiles.agencia
                                   BY NoHabiles.nit:
            DISPLAY noHabiles WITH WIDTH 250.
        END.
        OUTPUT CLOSE.

        MESSAGE "Se ha generado el listado."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        MESSAGE "En el directorio C:\Info_Fodun\ se ha generado" SKIP
                "un archivo con el nombre de AsociadosNoHabiles" SKIP
                "en   el  cual  se  encuentran  todos  aquellos" SKIP
                "Asociados   que   por   algún   motivo  no  se" SKIP
                "encuentran  hábiles,  y  se  incluye una breve" SKIP
                "descripción de la razón."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        OPEN QUERY brwHabiles FOR EACH habiles NO-LOCK BY habiles.agencia
                                                       BY habiles.nit.
        
    END.
    
    APPLY "choose" TO btnImprimir.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnImprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnImprimir C-Win
ON CHOOSE OF btnImprimir IN FRAME DEFAULT-FRAME /* Imprimir */
DO:
    DEFINE VAR Listado AS CHAR FORM "X(40)".
    Listado = W_PathSpl + "ListadoDeHabiles_" + W_Usuario + ".txt".
    {Incluido\Imprimir_carta.I "listado"}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brwHabiles
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.

  FIND FIRST habiles NO-LOCK NO-ERROR.
  IF AVAILABLE habiles AND w_fecha > 01/24/2012 THEN
      btnGenerar:LABEL = "Imprimir".

  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  ENABLE btnGenerar btnImprimir brwHabiles 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir C-Win 
PROCEDURE ProcesoImprimir :
DEFINE VAR cont AS INTEGER.

{Incluido\RepEncabezado.i}

W_Reporte = "REPORTE   : INFORME DE ASOCIADOS HÁBILES - " + STRING(W_fecha) + STRING(TIME,"hh:mm am").
W_EncColumna = "OF CÉDULA        NOMBRE(S)                      PRIMER APELLIDO                SEGUNDO APELLIDO                 FACULTAD".

VIEW FRAME F-Encabezado.
VIEW FRAME f-ftr.

FOR EACH Habiles NO-LOCK BREAK BY habiles.agencia
                                BY habiles.nit:
    DISPLAY habiles.agencia
            habiles.nit
            habiles.nombre
            habiles.apellido1
            habiles.apellido2
            habiles.facultad
        WITH FRAME F_Com1 WIDTH 250 NO-BOX USE-TEXT NO-UNDERLINE NO-LABELS.

    cont = cont + 1.

    IF LAST-OF(habiles.agencia) THEN DO:
        DISPLAY SKIP(1)
                "Total Agencia " + string(habiles.agencia) + " --> " + STRING(cont,">>>,>>9") FORMAT "X(100)"
                SKIP (5)
            WITH FRAME F_Com2 WIDTH 250 NO-BOX USE-TEXT NO-UNDERLINE NO-LABELS.

        cont = 0.
    END.
END.

PAGE.

OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

