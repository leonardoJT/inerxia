&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

   {Incluido/Variable.I "SHARED"}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_info Btn_Consulta Btn_Imprimir Btn_Ayuda ~
RECT-277 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-cf_bancos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updsav AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-cf_bancos AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Ayuda 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "Ayuda" 
     SIZE 4 BY 1.08.

DEFINE BUTTON Btn_Consulta 
     IMAGE-UP FILE "imagenes\lupa":U
     LABEL "&Consulta" 
     SIZE 10 BY 1.69 TOOLTIP "Busqueda de información de la pantalla en uso".

DEFINE BUTTON Btn_Imprimir 
     IMAGE-UP FILE "imagenes\impresora2":U
     LABEL "Im&primir" 
     SIZE 10 BY 1.69 TOOLTIP "Muestra la interface de salida de información (Reportes)".

DEFINE BUTTON Btn_info 
     IMAGE-UP FILE "imagenes\informacion3":U
     LABEL "Button 1" 
     SIZE 10 BY 1.65 TOOLTIP "Muestra la pantalla de información de Sesión y los mensajes del usuario activo".

DEFINE RECTANGLE RECT-277
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 12 BY 5.38.

DEFINE BUTTON Btn_SalirCon 
     IMAGE-UP FILE "imagenes\volver":U
     LABEL "Salir de Consulta" 
     SIZE 7 BY 1.62 TOOLTIP "Cierra esta ventana y vuelve a la pantalla anterior".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Frame_Consulta
     Btn_SalirCon AT ROW 10.88 COL 46
     "Nota:" VIEW-AS TEXT
          SIZE 6 BY .81 AT ROW 10.96 COL 2
          FGCOLOR 7 
     "Escoja el filtro por el cual desea realizar la busqueda," VIEW-AS TEXT
          SIZE 36 BY .81 AT ROW 10.96 COL 7
          FONT 4
     "digite la palabra clave a buscar, luego presione ENTER." VIEW-AS TEXT
          SIZE 38 BY .62 AT ROW 11.77 COL 7
          FONT 4
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 58.43 ROW 3.5
         SIZE 53.72 BY 12.54
         BGCOLOR 17 FONT 5
         TITLE "Consulta de Bancos".

DEFINE FRAME F-Main
     Btn_info AT ROW 1.81 COL 102 HELP
          "Muestra pantalla de información general de la aplicación"
     Btn_Consulta AT ROW 5.04 COL 102 HELP
          "Muestra la pantalla de consulta de bancos"
     Btn_Imprimir AT ROW 3.42 COL 102 HELP
          "Muestra la pantalla de impresión del informa debancos"
     Btn_Ayuda AT ROW 20.38 COL 104
     RECT-277 AT ROW 1.54 COL 101
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 114.29 BY 20.96
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
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "SFG - Configuración de Bancos"
         HEIGHT             = 21
         WIDTH              = 114.29
         MAX-HEIGHT         = 21
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 21
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
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR FRAME Frame_Consulta
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME Frame_Consulta:HIDDEN           = TRUE
       FRAME Frame_Consulta:MOVABLE          = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* SFG - Configuración de Bancos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* SFG - Configuración de Bancos */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Ayuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ayuda W-Win
ON CHOOSE OF Btn_Ayuda IN FRAME F-Main /* Ayuda */
OR HELP OF {&WINDOW-NAME} DO:
  SYSTEM-HELP "ayudas/tesoreri" CONTEXT 20.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Consulta W-Win
ON CHOOSE OF Btn_Consulta IN FRAME F-Main /* Consulta */
DO:
  VIEW FRAME Frame_Consulta.
  FRAME F-Main:SENSITIVE         = FALSE.
  FRAME Frame_Consulta:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imprimir W-Win
ON CHOOSE OF Btn_Imprimir IN FRAME F-Main /* Imprimir */
DO:
   DEFINE VAR Listado AS CHARACTER INITIAL "".
   
   Listado = W_PathSpl + "Bancos.LST".
  {Incluido\Imprimir.I "Listado"}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_info
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_info W-Win
ON CHOOSE OF Btn_info IN FRAME F-Main /* Button 1 */
DO:
  RUN W-InfDia.r.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Frame_Consulta
&Scoped-define SELF-NAME Btn_SalirCon
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_SalirCon W-Win
ON CHOOSE OF Btn_SalirCon IN FRAME Frame_Consulta /* Salir de Consulta */
DO:
  HIDE FRAME Frame_Consulta.
  FRAME F-Main:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win  _ADM-CREATE-OBJECTS
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
             INPUT  'b-cf_bancos.w':U ,
             INPUT  FRAME Frame_Consulta:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-cf_bancos ).
       RUN set-position IN h_b-cf_bancos ( 1.27 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-cf_bancos ( 9.42 , 51.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-updsav.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Save,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updsav ).
       RUN set-position IN h_p-updsav ( 10.15 , 101.00 ) NO-ERROR.
       RUN set-size IN h_p-updsav ( 10.04 , 12.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'v-cf_bancos.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-cf_bancos ).
       RUN set-position IN h_v-cf_bancos ( 1.27 , 21.00 ) NO-ERROR.
       /* Size in UIB:  ( 20.46 , 62.00 ) */

       /* Links to SmartViewer h_v-cf_bancos. */
       RUN add-link IN adm-broker-hdl ( h_b-cf_bancos , 'Record':U , h_v-cf_bancos ).
       RUN add-link IN adm-broker-hdl ( h_p-updsav , 'TableIO':U , h_v-cf_bancos ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-cf_bancos ,
             Btn_SalirCon:HANDLE IN FRAME Frame_Consulta , 'BEFORE':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updsav ,
             Btn_Imprimir:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-cf_bancos ,
             h_p-updsav , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
  THEN DELETE WIDGET W-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win  _DEFAULT-ENABLE
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
  ENABLE Btn_info Btn_Consulta Btn_Imprimir Btn_Ayuda RECT-277 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  ENABLE Btn_SalirCon 
      WITH FRAME Frame_Consulta IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-Frame_Consulta}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir_Excel W-Win 
PROCEDURE Imprimir_Excel :
{Incluido\Def_Excel.i}
 /* MANDA ENCABEZADO DE HOJA DE CALCULO*/
 
 E_NumFila = 1.
 E_NumColumn = 2.
 E_Fila      = "006" + "Codigo"
             + "040" + "Nombre del Banco                        ".
 RUN W-GraExcel.w (INPUT E_Fila, INPUT E_NumColumn, OUTPUT E_CmpGrafic).

/* launch Excel so it is visible to the user */
chExcelApp:Visible = TRUE.

/* create a new Workbook */
chWorkbook = chExcelApp:Workbooks:Add().

/* get the active Worksheet */
chWorkSheet = chExcelApp:Sheets:Item(1).

  FOR EACH Bancos NO-LOCK:
      E_Fila2     = "".
      E_Fila2     = "006" + STRING(Bancos.Cod_Compensa,"999999")
                  + "040" + STRING(Bancos.Nombre,"X(40)").
      {Incluido\imprimir_Excel.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir W-Win 
PROCEDURE ProcesoImprimir :
{INCLUIDO\RepEncabezado.I}    
  W_Reporte    = "REPORTE   : CONSULTA DE BANCOS - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
  W_EncColumna = " BANCO           NOMBRE".
  /*" BANCO           NOMBRE                 CJ.LOC D.REM.COB D.REM.NEG  DED.REM.COB DED.REM.NEG      ".*/
  
  VIEW FRAME F-Encabezado.
  VIEW FRAME F-Ftr.
  FOR EACH Bancos NO-LOCK:
      DISPLAY  Bancos.Cod_Compensa  AT 3  NO-LABEL
               Bancos.Nombre        AT 15  NO-LABEL FORMAT "X(30)"
/*               Bancos.Dia_Canje     AT 41 NO-LABEL
               Bancos.Dia_RemCobro  AT 49 NO-LABEL
               Bancos.Dia_RemNego   AT 62 NO-LABEL
               Bancos.Cod_RemCobro  AT 72 NO-LABEL
               Bancos.Cod_RemNego   AT 84 NO-LABEL*/
      WITH FRAME F-Banco DOWN WIDTH 132 USE-TEXT STREAM-IO NO-BOX.
      DOWN WITH FRAME F-Banco.
  END.
  PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win  _ADM-SEND-RECORDS
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Win 
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

