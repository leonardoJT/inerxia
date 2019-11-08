&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
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

{Incluido\variable.i "shared"}
DEFINE VARIABLE w_ok   AS LOGICAL NO-UNDO.

DEFINE TEMP-TABLE Tbitacora LIKE bitacora
                  FIELD Tnombre  AS CHARACTER FORMAT "X(80)"
    INDEX IdxFec  Fecha 
    INDEX IdxOper codigo
    INDEX Idxusu  usuario
    INDEX IdxTnom Tnombre.

DEFINE VAR woperini  LIKE varios.codigo  NO-UNDO.
DEFINE VAR woperfin  LIKE varios.codigo  NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Frm-Main
&Scoped-define BROWSE-NAME Brw-Consulta

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TBitacora

/* Definitions for BROWSE Brw-Consulta                                  */
&Scoped-define FIELDS-IN-QUERY-Brw-Consulta TBitacora.usuario TBitacora.Tnombre TBitacora.observacion Tbitacora.Cue_ahorros TBitacora.Num_credito Tbitacora.Cantidad Tbitacora.Tar_inicial Tbitacora.Tar_Final TBitacora.Usu_Inicial TBitacora.Usu_Final TBitacora.Age_Inicial TBitacora.Age_Final TBitacora.fecha TBitacora.hora   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Brw-Consulta   
&Scoped-define SELF-NAME Brw-Consulta
&Scoped-define QUERY-STRING-Brw-Consulta FOR EACH TBitacora
&Scoped-define OPEN-QUERY-Brw-Consulta OPEN QUERY {&SELF-NAME} FOR EACH TBitacora.
&Scoped-define TABLES-IN-QUERY-Brw-Consulta TBitacora
&Scoped-define FIRST-TABLE-IN-QUERY-Brw-Consulta TBitacora


/* Definitions for FRAME Frm-Main                                       */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Frm-Main ~
    ~{&OPEN-QUERY-Brw-Consulta}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Cmb-Operacion W-FecIni W-FecFin R-orden ~
Btn-Visualizar Btn-Procesar Btn_Done Brw-Consulta RECT-339 RECT-344 
&Scoped-Define DISPLAYED-OBJECTS Cmb-Operacion W-FecIni W-FecFin R-orden 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-6 Btn-Visualizar 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn-Procesar 
     LABEL "Procesar" 
     SIZE 8 BY 1.5.

DEFINE BUTTON Btn-Visualizar 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/impresora2.bmp":U
     LABEL "Button 2" 
     SIZE 8 BY 1.5.

DEFINE BUTTON Btn_Done DEFAULT 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "&Salir" 
     SIZE 8 BY 1.5 TOOLTIP "Sale de este proceso"
     BGCOLOR 8 .

DEFINE VARIABLE Cmb-Operacion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 44 BY 1 NO-UNDO.

DEFINE VARIABLE W-FecFin AS DATE FORMAT "99/99/9999":U 
     LABEL "Hasta" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81 NO-UNDO.

DEFINE VARIABLE W-FecIni AS DATE FORMAT "99/99/9999":U 
     LABEL "Desde" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81 NO-UNDO.

DEFINE VARIABLE R-orden AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Fecha", 1,
"Usuario", 2,
"Nombres", 3,
"Operacion", 4
     SIZE 42.14 BY .81
     BGCOLOR 8 FGCOLOR 1  NO-UNDO.

DEFINE RECTANGLE RECT-339
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 24 BY 2.65
     FGCOLOR 8 .

DEFINE RECTANGLE RECT-344
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 43.86 BY 1.35.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Brw-Consulta FOR 
      TBitacora SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Brw-Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Brw-Consulta C-Win _FREEFORM
  QUERY Brw-Consulta DISPLAY
      TBitacora.usuario        COLUMN-LABEL "Usuario" 
     TBitacora.Tnombre        COLUMN-LABEL "Nombre"       FORMAT "X(30)"
     TBitacora.observacion    COLUMN-LABEL "Observacion"  FORMAT "X(65)"
     Tbitacora.Cue_ahorros    COLUMN-LABEL "Cta-Ahorro"   FORMAT "X(14)"
     TBitacora.Num_credito    COLUMN-LABEL "Nro.Credito"
     Tbitacora.Cantidad       COLUMN-LABEL "Cantidad"
     Tbitacora.Tar_inicial    COLUMN-LABEL "TarjInicial"  FORMAT "X(20)"
     Tbitacora.Tar_Final      COLUMN-LABEL "TarjInicial"  FORMAT "X(20)"
     TBitacora.Usu_Inicial    COLUMN-LABEL "UsuInicial"   FORMAT "X(4)"
     TBitacora.Usu_Final      COLUMN-LABEL "UsuFinal"     FORMAT "X(4)"
     TBitacora.Age_Inicial    COLUMN-LABEL "AgeInicial"   
     TBitacora.Age_Final      COLUMN-LABEL "AgeInicial"   
     TBitacora.fecha          COLUMN-LABEL "Fecha"        FORMAT "99/99/9999"
     TBitacora.hora           COLUMN-LABEL "hora"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 104 BY 9.69
         BGCOLOR 15 FGCOLOR 0  ROW-HEIGHT-CHARS .58 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Frm-Main
     Cmb-Operacion AT ROW 2.08 COL 1 COLON-ALIGNED NO-LABEL WIDGET-ID 184
     W-FecIni AT ROW 2.27 COL 56 COLON-ALIGNED WIDGET-ID 64
     W-FecFin AT ROW 3.27 COL 56 COLON-ALIGNED WIDGET-ID 66
     R-orden AT ROW 4.04 COL 3.86 NO-LABEL WIDGET-ID 176
     Btn-Visualizar AT ROW 1.19 COL 98.29 WIDGET-ID 166
     Btn-Procesar AT ROW 2.69 COL 98.43 WIDGET-ID 86
     Btn_Done AT ROW 4.23 COL 98.43 WIDGET-ID 20
     Brw-Consulta AT ROW 6.12 COL 2 WIDGET-ID 200
     "          Selección de Operaciones" VIEW-AS TEXT
          SIZE 44 BY .81 AT ROW 1.15 COL 3 WIDGET-ID 182
          BGCOLOR 1 FGCOLOR 15 
     "Ordenado por:" VIEW-AS TEXT
          SIZE 14 BY .54 AT ROW 3.42 COL 4.14 WIDGET-ID 174
     "[dd/mm/aaaa]" VIEW-AS TEXT
          SIZE 13 BY .81 AT ROW 1.27 COL 59.29 WIDGET-ID 70
          FGCOLOR 12 
     " Fechas:" VIEW-AS TEXT
          SIZE 8 BY .81 AT ROW 1.27 COL 50.29 WIDGET-ID 68
          FGCOLOR 0 
     RECT-339 AT ROW 1.77 COL 49 WIDGET-ID 72
     RECT-344 AT ROW 3.69 COL 3.14 WIDGET-ID 172
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 106.14 BY 14.85 WIDGET-ID 100.


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
         TITLE              = "Informe Operaciones de usuario - W-Imp_OperacionesUsuario.w"
         HEIGHT             = 14.92
         WIDTH              = 106.72
         MAX-HEIGHT         = 27.38
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 27.38
         VIRTUAL-WIDTH      = 146.29
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
/* SETTINGS FOR FRAME Frm-Main
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB Brw-Consulta Btn_Done Frm-Main */
ASSIGN 
       Brw-Consulta:SEPARATOR-FGCOLOR IN FRAME Frm-Main      = 8.

/* SETTINGS FOR BUTTON Btn-Visualizar IN FRAME Frm-Main
   6                                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Brw-Consulta
/* Query rebuild information for BROWSE Brw-Consulta
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TBitacora.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE Brw-Consulta */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Informe Operaciones de usuario - W-Imp_OperacionesUsuario.w */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Informe Operaciones de usuario - W-Imp_OperacionesUsuario.w */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Procesar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Procesar C-Win
ON CHOOSE OF Btn-Procesar IN FRAME Frm-Main /* Procesar */
DO:
    FIND FIRST Cfg_TarjetaDb NO-LOCK NO-ERROR.

    ASSIGN W-FecIni
           W-FecFin
           Cmb-Operacion.
    
    EMPTY TEMP-TABLE Tbitacora.

    FOR EACH Bitacora_Tdb WHERE Bitacora.Fecha GE W-FecIni
                            AND Bitacora.Fecha LE W-FecFin
                            AND Bitacora.tipo EQ Cfg_TarjetaDb.Varios_Tipo
                            AND (Bitacora.codigo GE woperini AND
                                 Bitacora.codigo LE woperfin) NO-LOCK:
        CREATE TBitacora.
        BUFFER-COPY Bitacora_Tdb TO TBitacora.

        FIND FIRST varios WHERE varios.tipo EQ Cfg_TarjetaDb.Varios_Tipo AND varios.codigo = Bitacora_Tdb.codigo NO-LOCK NO-ERROR.
        FIND FIRST usuarios WHERE usuarios.usuario EQ Bitacora_Tdb.usuario NO-LOCK NO-ERROR.

        IF AVAILABLE(usuarios) THEN DO:
            ASSIGN TBitacora.Tnombre = usuarios.nombre
                   TBitacora.observacion = TRIM(varios.Descripcion) + " " + TRIM(TBitacora.observacion).
        END.
    END.

    APPLY "VALUE-CHANGED" TO R-Orden.
    APPLY "CHOOSE" TO Btn-Visualizar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Visualizar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Visualizar C-Win
ON CHOOSE OF Btn-Visualizar IN FRAME Frm-Main /* Button 2 */
DO:
    DEFINE VAR Listado AS CHARACTER INITIAL "".
    DEFINE VAR Tamano  AS INTEGER   INITIAL 2.
    listado = W_PathSpl + "L_Usuar.Lst".
    {Incluido\Imprimir.i "Listado" Tamano}
    APPLY "VALUE-CHANGED" TO R-Orden.
    APPLY "CHOOSE"        TO Btn-Visualizar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done C-Win
ON CHOOSE OF Btn_Done IN FRAME Frm-Main /* Salir */
DO:
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb-Operacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb-Operacion C-Win
ON VALUE-CHANGED OF Cmb-Operacion IN FRAME Frm-Main
DO:
  ASSIGN Cmb-Operacion.
  IF SUBSTRING(Cmb-operacion,1,2) = "00" THEN
     ASSIGN woperini = 0   woperfin = 99.
  ELSE
     ASSIGN woperini = INTEGER(SUBSTRING(Cmb-operacion,1,2))  
            woperfin = INTEGER(SUBSTRING(Cmb-operacion,1,2)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME R-orden
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R-orden C-Win
ON VALUE-CHANGED OF R-orden IN FRAME Frm-Main
DO:
  ASSIGN R-Orden.
  CASE R-Orden:
      WHEN 1 THEN OPEN QUERY Brw-Consulta FOR EACH Tbitacora NO-LOCK BY Tbitacora.Fecha    INDEXED-REPOSITION.
      WHEN 2 THEN OPEN QUERY Brw-Consulta FOR EACH Tbitacora NO-LOCK BY Tbitacora.usuario  INDEXED-REPOSITION.
      WHEN 3 THEN OPEN QUERY Brw-Consulta FOR EACH Tbitacora NO-LOCK BY Tbitacora.Tnombre  INDEXED-REPOSITION.
      WHEN 4 THEN OPEN QUERY Brw-Consulta FOR EACH Tbitacora NO-LOCK BY Tbitacora.codigo   INDEXED-REPOSITION.
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Brw-Consulta
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
  Cmb-Operacion:LIST-ITEMS = "".
  W_Ok = Cmb-Operacion:ADD-LAST( "00 - Todas" ).
  FIND FIRST Cfg_TarjetaDb NO-LOCK NO-ERROR.  
  FOR EACH VARIOS WHERE Varios.Tipo EQ Cfg_TarjetaDb.varios_tipo AND varios.estado = 1 NO-LOCK:
    w_ok = Cmb-Operacion:ADD-LAST(STRING(varios.codigo,"99") + " - " + TRIM(Varios.Descripcion) ).
  END.   
  Cmb-Operacion:SCREEN-VALUE = "00 - Todas".
  RUN inicializar_variables.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
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
  DISPLAY Cmb-Operacion W-FecIni W-FecFin R-orden 
      WITH FRAME Frm-Main IN WINDOW C-Win.
  ENABLE Cmb-Operacion W-FecIni W-FecFin R-orden Btn-Visualizar Btn-Procesar 
         Btn_Done Brw-Consulta RECT-339 RECT-344 
      WITH FRAME Frm-Main IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-Frm-Main}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImpLinea C-Win 
PROCEDURE ImpLinea :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 PUT  Tbitacora.usuario        " "                                  
      Tbitacora.Tnombre        FORMAT "X(30)" " "           
      Tbitacora.observacion    FORMAT "X(60)" " "           
      Tbitacora.Cantidad       FORMAT ">>>,>>9" " "         
      Tbitacora.Tar_inicial    FORMAT "9999999999999999" " "
      Tbitacora.Tar_final      FORMAT "9999999999999999" " "
      Tbitacora.Usu_Inicial    FORMAT "X(4)" " "             
      Tbitacora.Usu_Final      FORMAT "X(4)" " "            
      Tbitacora.Age_Inicial    FORMAT "999"  " "            
      Tbitacora.Age_Final      FORMAT "999"  " "            
      Tbitacora.Fecha          FORMAT "99/99/9999" " "      
      Tbitacora.Hora           FORMAT "X(8)" SKIP(0).            
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar_Variables C-Win 
PROCEDURE Inicializar_Variables :
/*------------------------------------------------------------------------------
  Autor: JOHN JAIRO MONCADA PUERTA    
  Cel  : 311 322 54 58 - Casa 253 49 71 
  Notes: diseñado bajo los parametros de Enpacto Administrativo      
------------------------------------------------------------------------------*/
FIND FIRST Cfg_Tarjeta NO-LOCK NO-ERROR.
DISABLE Btn-Visualizar.
ASSIGN Cmb-Operacion:SCREEN-VALUE IN FRAME Frm-main = "00 - Todas"  Cmb-Operacion.
ASSIGN woperini = 0   woperfin = 99.
ASSIGN W-FecIni:SCREEN-VALUE IN FRAME Frm-main = STRING(W_Fecha - DAY(w_fecha) + 1)
       W-FecFin:SCREEN-VALUE IN FRAME Frm-main = STRING(W_Fecha)
       W-FecIni  W-FecFin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir C-Win 
PROCEDURE ProcesoImprimir :
/*------------------------------------------------------------------------------
  Autor: JOHN JAIRO MONCADA PUERTA    
  Cel  : 311 322 54 58 - Casa 253 49 71 
  Notes: diseñado bajo los parametros de Enpacto Administrativo      
------------------------------------------------------------------------------*/
 {INCLUIDO\RepEncabezado.I}.  
/*  DEBUGGER:INITIATE().   */
/*  DEBUGGER:SET-BREAK().  */
 FIND FIRST cfg_tarjetaDb NO-LOCK NO-ERROR.
 DO WITH FRAME Frm-main:
     ASSIGN  W-FecIni  W-FecFin R-Orden.
 END.
 W_Reporte    = "REPORTE: Operac x Usuario - " + STRING(TODAY) + " " + STRING(TIME,"hh:mm am") + " - " +
                TRIM(Cmb-Operacion) + " - FecIni: " + STRING(W-FecIni) + " - FecFin: " + STRING(W-FecFin) .
 W_EncColumna = "Usu  Nombre                         Observacion                                                     Cant Tarjeta Inicial  Tarjeta Final    UsuI UsuF AgI AgF Fecha      Hora".
 VIEW FRAME F-Encabezado.
 VIEW FRAME F-Ftr.
 CASE R-Orden:          
     WHEN 1 THEN  FOR EACH Tbitacora NO-LOCK BY Tbitacora.Fecha         : RUN impLinea. END.
     WHEN 2 THEN  FOR EACH Tbitacora NO-LOCK BY Tbitacora.Usuario       : RUN impLinea. END.
     WHEN 3 THEN  FOR EACH Tbitacora NO-LOCK BY Tbitacora.Tnombre       : RUN impLinea. END.
     WHEN 4 THEN  FOR EACH Tbitacora NO-LOCK BY Tbitacora.codigo        : RUN impLinea. END.
 END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

