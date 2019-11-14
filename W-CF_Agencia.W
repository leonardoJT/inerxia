&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME W-Oficin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Oficin 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

{Incluido/Variable.I "SHARED"}

/*    DEFINE SHARED VAR W_Nom_Agencia    AS   CHARACTER FORMAT "X(60)".   */
/*    DEFINE SHARED VAR W_Nom_Entidad    AS   CHARACTER FORMAT "X(60)".   */
/*    DEFINE SHARED VAR W_Usuario        LIKE Usuarios.Usuario.           */
/*    DEFINE SHARED VAR W_Estacion       LIKE Estaciones.Estacion.        */
/*    DEFINE SHARED VAR W_Manija         AS   HANDLE.                     */
/*    DEFINE SHARED VAR W_PathSpl        LIKE Entidad.Dir_Spl.            */
/*    DEFINE VAR W_Rpta                  AS LOGICAL.                      */
/*    DEFINE VAR W_TamOfi                AS INTEGER INITIAL 0.            */
/*    DEFINE VAR W_OfiTra                LIKE Agencias.Agencia INITIAL 0. */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Consulta

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_Consultar 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Oficin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-cf_agencia AS HANDLE NO-UNDO.
DEFINE VARIABLE h_P-UPDSAV AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-cf_agencia AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Consultar 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/volver.bmp":U
     LABEL "&Terminar Consulta" 
     SIZE 7 BY 1.62 TOOLTIP "Salir de la consulta".

DEFINE BUTTON Btn_Ayuda 
     IMAGE-UP FILE "imagenes\interrogacion":U
     LABEL "" 
     SIZE 4 BY 1.08 TOOLTIP "Ayuda".

DEFINE BUTTON Btn_Consulta 
     IMAGE-UP FILE "imagenes\lupa":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "Co&nsultar" 
     SIZE 10 BY 1.62 TOOLTIP "Busqueda de información de la pantalla en uso"
     FONT 4.

DEFINE BUTTON Btn_Impresion 
     IMAGE-UP FILE "imagenes\impresora2":U
     IMAGE-INSENSITIVE FILE "imagenes/impresora2.bmp":U
     LABEL "I&mprimir" 
     SIZE 10 BY 1.62 TOOLTIP "Muestra interface de salida de información (Reportes)"
     FONT 15.

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "imagenes\informacion":U
     IMAGE-INSENSITIVE FILE "imagenes/informacion.bmp":U
     LABEL "Button 1" 
     SIZE 10 BY 1.62 TOOLTIP "Muestra información de sesión y mensajes del usuario activo".

DEFINE RECTANGLE RECT-280
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 12 BY 5.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Consulta
     Btn_Consultar AT ROW 15 COL 47 HELP
          "Termina la consulta de Agencias"
     "Nota:" VIEW-AS TEXT
          SIZE 5 BY .81 AT ROW 15 COL 3
          FGCOLOR 7 FONT 5
     "Escoja el filtro por el cual desea realizar la busqueda" VIEW-AS TEXT
          SIZE 36 BY .77 AT ROW 15 COL 8
     "digite la palabra clave a buscar, Luego presione ENTER." VIEW-AS TEXT
          SIZE 39 BY .5 AT ROW 15.81 COL 8
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 59.14 ROW 3.54
         SIZE 54.14 BY 16.73
         BGCOLOR 17 FONT 4
         TITLE "Consulta de Agencias".

DEFINE FRAME F-Main
     BUTTON-1 AT ROW 1.81 COL 102
     Btn_Impresion AT ROW 3.42 COL 102 HELP
          "Permite generar la impresión de Agencias"
     Btn_Consulta AT ROW 5.04 COL 102 HELP
          "Permite generar la consulta de Agencias"
     Btn_Ayuda AT ROW 20.12 COL 105 HELP
          "Permite generar la ayuda de la pantalla"
     RECT-280 AT ROW 1.54 COL 101
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.86 BY 20.96
         BGCOLOR 17 FONT 4.


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
  CREATE WINDOW W-Oficin ASSIGN
         HIDDEN             = YES
         TITLE              = "SFG - Configuración de Agencias de la Organización"
         HEIGHT             = 21.23
         WIDTH              = 114
         MAX-HEIGHT         = 21.38
         MAX-WIDTH          = 114
         VIRTUAL-HEIGHT     = 21.38
         VIRTUAL-WIDTH      = 114
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
IF NOT W-Oficin:LOAD-ICON("imagenes/cubos.ico":U) THEN
    MESSAGE "Unable to load icon: imagenes/cubos.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Oficin 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Oficin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Consulta
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME F-Consulta:HIDDEN           = TRUE
       FRAME F-Consulta:MOVABLE          = TRUE.

/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE                                                          */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Oficin)
THEN W-Oficin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Oficin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Oficin W-Oficin
ON END-ERROR OF W-Oficin /* SFG - Configuración de Agencias de la Organización */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Oficin W-Oficin
ON WINDOW-CLOSE OF W-Oficin /* SFG - Configuración de Agencias de la Organización */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ayuda W-Oficin
ON CHOOSE OF Btn_Ayuda IN FRAME F-Main
OR HELP OF {&WINDOW-NAME}       
DO:     
  SYSTEM-HELP "AYUDAS\CONFIGUR" CONTEXT 3.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Consulta W-Oficin
ON CHOOSE OF Btn_Consulta IN FRAME F-Main /* Consultar */
DO:
  DISABLE ALL EXCEPT Btn_Ayuda WITH FRAME F-Main.
  VIEW FRAME F-Consulta.
  ENABLE ALL WITH FRAME F-Consulta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Consulta
&Scoped-define SELF-NAME Btn_Consultar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Consultar W-Oficin
ON CHOOSE OF Btn_Consultar IN FRAME F-Consulta /* Terminar Consulta */
DO:
  HIDE FRAME F-Consulta.
  ENABLE ALL EXCEPT Btn_Ayuda WITH FRAME F-Main.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME Btn_Impresion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Impresion W-Oficin
ON CHOOSE OF Btn_Impresion IN FRAME F-Main /* Imprimir */
DO:
  DEFINE VAR Listado AS CHARACTER INITIAL "".
  
  listado = W_PathSpl + "L-Oficin.Lst".
  {Incluido\Imprimir.I "listado"}     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 W-Oficin
ON CHOOSE OF BUTTON-1 IN FRAME F-Main /* Button 1 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Consulta
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Oficin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Oficin  _ADM-CREATE-OBJECTS
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
             INPUT  'v-cf_agencia.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-cf_agencia ).
       RUN set-position IN h_v-cf_agencia ( 2.35 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 18.31 , 94.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'b-cf_agencia.w':U ,
             INPUT  FRAME F-Consulta:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-cf_agencia ).
       RUN set-position IN h_b-cf_agencia ( 1.27 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-cf_agencia ( 13.46 , 52.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'P-UPDSAV.W':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Save,
                     AddFunction = One-Record':U ,
             OUTPUT h_P-UPDSAV ).
       RUN set-position IN h_P-UPDSAV ( 9.88 , 101.00 ) NO-ERROR.
       RUN set-size IN h_P-UPDSAV ( 9.69 , 12.00 ) NO-ERROR.

       /* Links to SmartViewer h_v-cf_agencia. */
       RUN add-link IN adm-broker-hdl ( h_b-cf_agencia , 'Record':U , h_v-cf_agencia ).
       RUN add-link IN adm-broker-hdl ( h_P-UPDSAV , 'TableIO':U , h_v-cf_agencia ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-cf_agencia ,
             BUTTON-1:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-cf_agencia ,
             Btn_Consultar:HANDLE IN FRAME F-Consulta , 'BEFORE':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_P-UPDSAV ,
             Btn_Consulta:HANDLE IN FRAME F-Main , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Oficin  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Oficin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Oficin)
  THEN DELETE WIDGET W-Oficin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Oficin  _DEFAULT-ENABLE
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
  ENABLE RECT-280 BUTTON-1 Btn_Impresion Btn_Consulta Btn_Ayuda 
      WITH FRAME F-Main IN WINDOW W-Oficin.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  ENABLE Btn_Consultar 
      WITH FRAME F-Consulta IN WINDOW W-Oficin.
  {&OPEN-BROWSERS-IN-QUERY-F-Consulta}
  VIEW W-Oficin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir_Excel W-Oficin 
PROCEDURE Imprimir_Excel :
{Incluido\Def_Excel.i}
 /* MANDA ENCABEZADO DE HOJA DE CALCULO*/
 
 E_NumFila = 1.
 E_NumColumn = 5.
 E_Fila      = "003" + "Age"
             + "030" + "Nombre                        "
             + "010" + "Telefono  "
             + "030" + "Direccion                     "
             + "012" + "Nit_Director".
 RUN W-GraExcel.w (INPUT E_Fila, INPUT E_NumColumn, OUTPUT E_CmpGrafic).

/* launch Excel so it is visible to the user */
chExcelApp:Visible = TRUE.

/* create a new Workbook */
chWorkbook = chExcelApp:Workbooks:Add().

/* get the active Worksheet */
chWorkSheet = chExcelApp:Sheets:Item(1).

 DEFI VAR W_IdCC AS CHARACTER FORMAT "X(2)".
 FOR EACH Agencias NO-LOCK:
      E_Fila2     = "".
      E_Fila2     = "003" + STRING(Agencia.Agencia,"999")
                  + "030" + STRING(Agencia.Nombre,"X(30)")
                  + "010" + STRING(Agencia.Telefono,"X(10)")
                  + "030" + STRING(Agencia.Direccion,"X(30)")
                  + "012" + STRING(Agencia.Nit_Director,"X(12)").
      {Incluido\imprimir_Excel.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Oficin 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir W-Oficin 
PROCEDURE ProcesoImprimir :
{Incluido\RepEncabezado.i}

  W_Reporte   = "REPORTE   : AGENCIAS - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
  W_EncColumna = "AGENCIA   NOMBRE                          TELEFONO      DIRECCIÒN                     DIRECTOR".

  DEFINE FRAME F-Agencias
    Agencias.Agencia   AT  3
    Agencias.Nombre    AT 11
    Agencias.Telefono  AT 43
    Agencias.Direccion AT 57
    Agencias.Nit_Director  AT 89
  WITH DOWN WIDTH 132.
     
    W_Linea = FILL(W_Raya,132).
    VIEW FRAME F-Encabezado.
    VIEW FRAME f-ftr.
    FOR EACH Agencias NO-LOCK:
      DISPLAY Agencias.Agencia   AT  3
              Agencias.Nombre    AT 11 FORMAT "X(30)"
              Agencias.Telefono  AT 43
              Agencias.Direccion AT 57 FORMAT "X(30)"
              Agencias.Nit_Director  AT 89 FORMAT "X(30)"
    WITH WIDTH 132 FRAME F-Agencias USE-TEXT STREAM-IO NO-LABELS.
    END.  
    PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Oficin  _ADM-SEND-RECORDS
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Oficin 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
/*  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

