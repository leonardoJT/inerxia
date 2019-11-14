&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME W-ProCre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-ProCre 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
  DEFINE SHARED VARIABLE W_Nom_Agencia AS CHAR FORMAT "X(40)".
  DEFINE SHARED VARIABLE W_PathSpl LIKE Entidad.Dir_Spl.
  DEFINE SHARED VARIABLE W_Manija AS HANDLE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Consulta

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_Termina 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-ProCre AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-cf_deducibles AS HANDLE NO-UNDO.
DEFINE VARIABLE h_P-UPDSAV AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-cf_deducibles AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Termina 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "&Termina Consulta" 
     SIZE 8 BY 1.62
     FONT 4.

DEFINE BUTTON Btn_Ayuda 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "" 
     SIZE 4 BY 1.08 TOOLTIP "Ayuda"
     FONT 9.

DEFINE BUTTON Btn_Consulta 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "Co&nsulta" 
     SIZE 10 BY 1.62 TOOLTIP "Busqueda de información de la pantalla activa"
     FONT 4.

DEFINE BUTTON Btn_Impresion 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/impresora2.bmp":U
     LABEL "I&mpresion" 
     SIZE 10 BY 1.62 TOOLTIP "Muestra la interface de salida de información (Reportes)"
     FONT 15.

DEFINE BUTTON BUTTON-54 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/informacion.bmp":U
     LABEL "Button 54" 
     SIZE 10 BY 1.65 TOOLTIP "Muestra la pantalla de información de sesión y los mensajes del usuario activo".

DEFINE RECTANGLE RECT-267
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 12 BY 5.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BUTTON-54 AT ROW 1.54 COL 102
     Btn_Impresion AT ROW 3.15 COL 102 HELP
          "Permite Generar la Impresion de Deducibles"
     Btn_Consulta AT ROW 4.77 COL 102 HELP
          "Permite Generar la Consulta de Deducibles"
     Btn_Ayuda AT ROW 20.65 COL 105 HELP
          "Permite Obtener la ayuda de esta Pantalla"
     RECT-267 AT ROW 1.27 COL 101
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 114.14 BY 21.38
         BGCOLOR 17 .

DEFINE FRAME F-Consulta
     Btn_Termina AT ROW 12.85 COL 53 HELP
          "Termina la consulta de Deducibles"
     "Escoja el filtro por el cual desea realizar la busqueda," VIEW-AS TEXT
          SIZE 36 BY .81 AT ROW 12.85 COL 9
          FONT 4
     "digite la palabra clave a buscar, luego presione ENTER." VIEW-AS TEXT
          SIZE 38 BY .62 AT ROW 13.65 COL 9
          FONT 4
     "Nota:" VIEW-AS TEXT
          SIZE 6 BY .81 AT ROW 12.85 COL 2
          FGCOLOR 7 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 49.29 ROW 4.65
         SIZE 62.57 BY 14.5
         BGCOLOR 17 
         TITLE "Consulta de deducciones".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-ProCre ASSIGN
         HIDDEN             = YES
         TITLE              = "Configuración de Deducibles"
         HEIGHT             = 21.38
         WIDTH              = 114.29
         MAX-HEIGHT         = 21.38
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 21.38
         VIRTUAL-WIDTH      = 114.29
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT W-ProCre:LOAD-ICON("imagenes/cubos.ico":U) THEN
    MESSAGE "Unable to load icon: imagenes/cubos.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-ProCre 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-ProCre
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Consulta
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F-Consulta:HIDDEN           = TRUE
       FRAME F-Consulta:MOVABLE          = TRUE.

/* SETTINGS FOR FRAME F-Main
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-ProCre)
THEN W-ProCre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-ProCre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-ProCre W-ProCre
ON END-ERROR OF W-ProCre /* Configuración de Deducibles */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-ProCre W-ProCre
ON WINDOW-CLOSE OF W-ProCre /* Configuración de Deducibles */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME Btn_Ayuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ayuda W-ProCre
ON CHOOSE OF Btn_Ayuda IN FRAME F-Main
OR HELP OF {&WINDOW-NAME}
DO:
   SYSTEM-HELP "AYUDAS\REDECOOP" CONTEXT 14.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Consulta W-ProCre
ON CHOOSE OF Btn_Consulta IN FRAME F-Main /* Consulta */
DO:
  DISABLE ALL EXCEPT Btn_Ayuda WITH FRAME F-Main.
  VIEW FRAME F-Consulta.
  ENABLE ALL WITH FRAME F-Consulta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Impresion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Impresion W-ProCre
ON CHOOSE OF Btn_Impresion IN FRAME F-Main /* Impresion */
DO:
  DEFINE VARIABLE Listado AS CHARACTER INITIAL "".
  Listado = W_PathSpl + "L_PROCRE.LST".
  {incluido/Imprimir.I "Listado"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Consulta
&Scoped-define SELF-NAME Btn_Termina
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Termina W-ProCre
ON CHOOSE OF Btn_Termina IN FRAME F-Consulta /* Termina Consulta */
DO:
  ENABLE ALL EXCEPT Btn_Ayuda WITH FRAME F-Main.
  HIDE FRAME F-Consulta.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME BUTTON-54
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-54 W-ProCre
ON CHOOSE OF BUTTON-54 IN FRAME F-Main /* Button 54 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Consulta
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-ProCre 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-ProCre  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'b-cf_deducibles.w':U ,
             INPUT  FRAME F-Consulta:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-cf_deducibles ).
       RUN set-position IN h_b-cf_deducibles ( 1.00 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-cf_deducibles ( 11.58 , 60.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'v-cf_deducibles.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-cf_deducibles ).
       RUN set-position IN h_v-cf_deducibles ( 7.19 , 10.00 ) NO-ERROR.
       /* Size in UIB:  ( 12.12 , 81.29 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'P-UPDSAV.W':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Save,
                     AddFunction = One-Record':U ,
             OUTPUT h_P-UPDSAV ).
       RUN set-position IN h_P-UPDSAV ( 9.88 , 101.00 ) NO-ERROR.
       RUN set-size IN h_P-UPDSAV ( 10.50 , 12.57 ) NO-ERROR.

       /* Links to SmartViewer h_v-cf_deducibles. */
       RUN add-link IN adm-broker-hdl ( h_b-cf_deducibles , 'Record':U , h_v-cf_deducibles ).
       RUN add-link IN adm-broker-hdl ( h_P-UPDSAV , 'TableIO':U , h_v-cf_deducibles ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-cf_deducibles ,
             Btn_Termina:HANDLE IN FRAME F-Consulta , 'BEFORE':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-cf_deducibles ,
             Btn_Consulta:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_P-UPDSAV ,
             h_v-cf_deducibles , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-ProCre  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-ProCre  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-ProCre)
  THEN DELETE WIDGET W-ProCre.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-ProCre  _DEFAULT-ENABLE
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
  ENABLE BUTTON-54 Btn_Impresion Btn_Consulta Btn_Ayuda RECT-267 
      WITH FRAME F-Main IN WINDOW W-ProCre.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  ENABLE Btn_Termina 
      WITH FRAME F-Consulta IN WINDOW W-ProCre.
  {&OPEN-BROWSERS-IN-QUERY-F-Consulta}
  VIEW W-ProCre.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir_Excel W-ProCre 
PROCEDURE Imprimir_Excel :
{Incluido\Def_Excel.i}
 /* MANDA ENCABEZADO DE HOJA DE CALCULO*/
 DEFINE VAR W_EstadoInf AS CHARACTER FORMAT "X(8)".
 DEFINE VAR W_Estado AS CHAR FORMAT "X(10)".
 DEFINE VAR W_Tipo   AS CHAR FORMAT "X(10)".
 DEFINE VAR W_Clase  AS CHAR FORMAT "X(10)".
    
 E_NumFila = 1.
 E_NumColumn = 7.
 E_Fila   = "004" + "Cod "
          + "040" + "Nom_Deducible                           "
          + "014" + "Cuenta        "
          + "010" + "Tipo      "
          + "010" + "Clase     "
          + "021" + "Valor                "
          + "010" + "Estado    ".
    
 RUN W-GraExcel.w (INPUT E_Fila, INPUT E_NumColumn, OUTPUT E_CmpGrafic).

/* launch Excel so it is visible to the user */
chExcelApp:Visible = TRUE.

/* create a new Workbook */
chWorkbook = chExcelApp:Workbooks:Add().

/* get the active Worksheet */
chWorkSheet = chExcelApp:Sheets:Item(1).
DEFINE VAR tipo AS CHARACTER FORMAT "X(15)".
DEFINE VAR Est  AS CHARACTER FORMAT "X(8)".
  FOR EACH Deducible NO-LOCK BY Deducible.Cod_Deducible:
    IF Deducible.Cla_Deducible = 1 THEN
       W_Clase = "Porc.".
    ELSE
       W_Clase = "Valor.".
    IF Deducible.Estado = 1 THEN
       W_Estado = "Activo".
    ELSE
       W_Estado = "Inactivo".
    IF Deducible.Tip_Deducible = 1 THEN
       W_Tipo = "Fijo".
    ELSE
       W_Tipo = "Vble".
      E_Fila2     = "".
      E_Fila2     = "004" + STRING(Deducible.Cod_Deducible,"X(4)")
                  + "040" + STRING(Deducible.Nom_Deducible,"X(40)") 
                  + "014" + STRING(Deducible.Cuenta,"X(14)")
                  + "010" + STRING(W_Tipo,"X(10)")
                  + "010" + STRING(W_Clase,"X(10)")
                  + "021" + STRING(Deducible.Valor,"->>>>>,>>>,>>>,>>9.99")
                  + "010" + STRING(W_Estado,"X(10)").
      {Incluido\imprimir_Excel.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-ProCre 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir W-ProCre 
PROCEDURE ProcesoImprimir :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR Linea    AS CHAR FORMAT "X(120)" INITIAL "".
  DEFINE VAR Raya     AS CHAR INITIAL "-".
  DEFINE VAR W_Estado AS CHAR INITIAL "".
  DEFINE VAR W_Tipo   AS CHAR INITIAL "".
  DEFINE VAR W_Clase  AS CHAR INITIAL "".
    
  DEFINE FRAME F-Deducible
    Deducible.Cod_Deducible  AT  8 
    Deducible.Nom_Deducible  AT 13 FORMAT "X(30)"
    Deducible.Cuenta         AT 45 FORMAT "X(14)"
    W_Tipo                   AT 61 FORMAT "X(4)"
    W_Clase                  AT 67 FORMAT "X(5)"
    Deducible.Valor          AT 74 FORMAT ">>>,>>>,>>>,>>>"
    W_Estado                 AT 91 FORMAT "X(8)"
  WITH DOWN WIDTH 132 NO-BOX.
     
  DEFINE FRAME F-Encabezado
    HEADER
     "LISTADO DEDUCIBLES" AT 28
     "PAGINA:" AT 70 PAGE-NUMBER FORMAT ">>>9" SKIP(1)
     "Codigo"          AT  3
     "Nombre"          AT 13
     "Cuenta"          AT 45
     "Tipo"            AT 61
     "Clase"           AT 67
     "Valor Deducible" AT 74
     "Estado"          AT 91
      Linea           AT   1  FORMAT "X(96)"
  WITH DOWN WIDTH 132 USE-TEXT PAGE-TOP FRAME F-Encabezado STREAM-IO NO-BOX.
          
  DEFINE FRAME F-PiePagina
    HEADER 
     "FECHA:"   AT 2 
     TODAY     AT 10
     W_Nom_Agencia AT 28 FORMAT "X(30)"
     "HORA:"    AT 70 STRING(TIME,"HH:MM AM")
  WITH DOWN WIDTH 132 NO-BOX FRAME F-PiePagina PAGE-BOTTOM USE-TEXT STREAM-IO. 

  Linea = FILL(Raya,100).
  VIEW FRAME F-Encabezado.
  VIEW FRAME F-PiePagina.
  FOR EACH Deducible NO-LOCK BY Deducible.Cod_Deducible:
    IF Deducible.Cla_Deducible = 1 THEN
       W_Clase = "Porc.".
    ELSE
       W_Clase = "Valor.".
    IF Deducible.Estado = 1 THEN
       W_Estado = "Activo".
    ELSE
       W_Estado = "Inactivo".
    IF Deducible.Tip_Deducible = 1 THEN
       W_Tipo = "Fijo".
    ELSE
       W_Tipo = "Vble".
    DISPLAY Deducible.Cod_Deducible Deducible.Nom_Deducible Deducible.Cuenta
            W_Tipo W_Clase Deducible.Valor W_Estado 
    WITH WIDTH 132 FRAME F-Deducible USE-TEXT STREAM-IO NO-LABELS.
  END.  
  PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-ProCre  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartWindow, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-ProCre 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

