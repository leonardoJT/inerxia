&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-PInv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-PInv 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
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
  DEFINE SHARED VAR W_Nom_Agencia    AS   CHARACTER FORMAT "X(60)".
  DEFINE SHARED VAR W_Nom_Entidad    AS   CHARACTER FORMAT "X(60)".
  DEFINE SHARED VAR W_Usuario        LIKE Usuarios.Usuario.
  DEFINE SHARED VAR W_Estacion       LIKE Estaciones.Estacion.
  DEFINE SHARED VAR W_Manija         AS   HANDLE.
  DEFINE SHARED VAR W_PathSpl        LIKE Entidad.Dir_Spl.
  DEFINE VAR W_Rpta                  AS LOGICAL.
  DEFINE VAR W_TamOfi                AS INTEGER INITIAL 0.
  DEFINE VAR W_OfiTra                LIKE Agencias.Agencia INITIAL 0.

  DEFINE VARIABLE W_NomCar             AS CHARACTER FORMAT "X(30)" INITIAL "".
  DEFINE VARIABLE W_PriUsu             AS INTEGER FORMAT 9.
  DEFINE VARIABLE W_Estado             AS CHARACTER FORMAT "X(8)" INITIAL "".
  DEFINE VARIABLE W_Rowid              AS ROWID.
  DEFINE VARIABLE W-Procedure          AS CHARACTER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-CInv

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS opc Buscar BUTTON-4 
&Scoped-Define DISPLAYED-OBJECTS opc Buscar 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-PInv AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-pro_inversiones AS HANDLE NO-UNDO.
DEFINE VARIABLE h_d-pro_inversiones AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updsav91 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-pro_inversiones AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-4 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 4" 
     SIZE 10.43 BY 1.54.

DEFINE VARIABLE Buscar AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 25 BY .69
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE opc AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Tipo", 1,
"Código", 2,
"Nombre", 3,
"Cuenta", 4
     SIZE 41 BY 1.08
     FONT 4 NO-UNDO.

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/informacion.bmp":U
     LABEL "Button 1" 
     SIZE 10.43 BY 1.54 TOOLTIP "Información".

DEFINE BUTTON BUTTON-2 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/impresora2.bmp":U
     LABEL "Button 2" 
     SIZE 10.43 BY 1.54 TOOLTIP "Imprimir".

DEFINE BUTTON BUTTON-3 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 3" 
     SIZE 5 BY 1.12 TOOLTIP "Ayuda".

DEFINE BUTTON BUTTON-5 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "Button 5" 
     SIZE 10.43 BY 1.54 TOOLTIP "Consulta".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     BUTTON-1 AT ROW 1.27 COL 101
     BUTTON-2 AT ROW 4.42 COL 101
     BUTTON-5 AT ROW 6 COL 101
     BUTTON-3 AT ROW 19.85 COL 104
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.29 BY 21.12
         BGCOLOR 17 FONT 4.

DEFINE FRAME F-CInv
     opc AT ROW 8.54 COL 5 NO-LABEL
     Buscar AT ROW 8.54 COL 49 NO-LABEL
     BUTTON-4 AT ROW 10.96 COL 59
     "digite la palabra clave a buscar, luego presione ENTER." VIEW-AS TEXT
          SIZE 38 BY .62 AT ROW 11.69 COL 13
          FONT 4
     "Nota:" VIEW-AS TEXT
          SIZE 6 BY .81 AT ROW 10.96 COL 8
          FGCOLOR 7 FONT 5
     "Escoja el filtro por el cual desea realizar la busqueda," VIEW-AS TEXT
          SIZE 36 BY .81 AT ROW 10.96 COL 13
          FONT 4
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 13 ROW 1.27
         SIZE 76 BY 12.92
         BGCOLOR 17 FONT 4
         TITLE "Consuta Inversiones".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-PInv ASSIGN
         HIDDEN             = YES
         TITLE              = "Productos Inversiones"
         HEIGHT             = 21.12
         WIDTH              = 113.29
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.14
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.14
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = 17
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-PInv 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-PInv
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME F-CInv:FRAME = FRAME fMain:HANDLE.

/* SETTINGS FOR FRAME F-CInv
                                                                        */
ASSIGN 
       FRAME F-CInv:HIDDEN           = TRUE
       FRAME F-CInv:MOVABLE          = TRUE.

/* SETTINGS FOR FILL-IN Buscar IN FRAME F-CInv
   ALIGN-L                                                              */
/* SETTINGS FOR FRAME fMain
                                                                        */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME F-CInv:MOVE-BEFORE-TAB-ITEM (BUTTON-1:HANDLE IN FRAME fMain)
/* END-ASSIGN-TABS */.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-PInv)
THEN W-PInv:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-PInv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-PInv W-PInv
ON END-ERROR OF W-PInv /* Productos Inversiones */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-PInv W-PInv
ON WINDOW-CLOSE OF W-PInv /* Productos Inversiones */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Buscar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Buscar W-PInv
ON LEAVE OF Buscar IN FRAME F-CInv
OR RETURN OF Buscar DO:
  DEFINE VARIABLE palabra AS CHARACTER NO-UNDO.
  IF SELF:SCREEN-VALUE NE "":U THEN DO:
     DEFINE VARIABLE op      AS INTEGER NO-UNDO.
     ASSIGN op = opc:INPUT-VALUE.
     CASE op:
        WHEN 1 THEN DO:
          FIND FIRST Pro_Inversiones WHERE STRING(Pro_Inversiones.Categoria) BEGINS SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
          ASSIGN palabra = "STRING(Pro_Inversiones.Categoria) BEGINS '" + SELF:SCREEN-VALUE + "'".
        END.
        WHEN 2 THEN DO:
          FIND FIRST Pro_Inversiones WHERE STRING(Pro_Inversiones.Cod_Producto) BEGINS SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
          ASSIGN palabra = "STRING(Pro_Inversiones.Cod_Producto) BEGINS '" + SELF:SCREEN-VALUE + "'".
        END.
        WHEN 3 THEN DO:
          FIND FIRST Pro_Inversiones WHERE Pro_Inversiones.Nom_Producto BEGINS SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
          ASSIGN palabra = "Pro_Inversiones.Nom_Producto BEGINS '" + SELF:SCREEN-VALUE + "'".
        END.
        WHEN 4 THEN DO:
          FIND FIRST Pro_Inversiones WHERE Pro_Inversiones.Cuenta_Inversion_DB BEGINS SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
          ASSIGN palabra = "Pro_Inversiones.Cuenta_Inversion_DB BEGINS '" + SELF:SCREEN-VALUE + "'".
        END.
     END CASE.
    END.
    IF AVAILABLE Pro_Inversiones THEN DO:
      {SET QueryWhere palabra h_d-pro_inversiones}.
      {FN OpenQuery h_d-pro_inversiones}.
     END.
   ELSE DO:
     FIND FIRST Pro_Inversiones NO-LOCK NO-ERROR.
     {FN OpenQuery h_d-pro_inversiones}.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 W-PInv
ON CHOOSE OF BUTTON-1 IN FRAME fMain /* Button 1 */
DO:
  RUN w-infdia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 W-PInv
ON CHOOSE OF BUTTON-2 IN FRAME fMain /* Button 2 */
DO:
   DEFINE VAR Listado AS CHARACTER INITIAL "".
  Listado = W_PathSpl + "Liprover.LST".
  {Incluido\Imprimir.I "Listado"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-CInv
&Scoped-define SELF-NAME BUTTON-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-4 W-PInv
ON CHOOSE OF BUTTON-4 IN FRAME F-CInv /* Button 4 */
DO:
  HIDE FRAME F-CInv.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 W-PInv
ON CHOOSE OF BUTTON-5 IN FRAME fMain /* Button 5 */
DO:
  VIEW FRAME F-CInv.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-CInv
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-PInv 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-PInv  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE currentPage  AS INTEGER NO-UNDO.

  ASSIGN currentPage = getCurrentPage().

  CASE currentPage: 

    WHEN 0 THEN DO:
       RUN constructObject (
             INPUT  'd-pro_inversiones.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamed-pro_inversionesUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_d-pro_inversiones ).
       RUN repositionObject IN h_d-pro_inversiones ( 1.54 , 3.00 ) NO-ERROR.
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'b-pro_inversiones.w':U ,
             INPUT  FRAME F-CInv:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_b-pro_inversiones ).
       RUN repositionObject IN h_b-pro_inversiones ( 1.00 , 3.00 ) NO-ERROR.
       RUN resizeObject IN h_b-pro_inversiones ( 6.19 , 70.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'v-pro_inversiones.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_v-pro_inversiones ).
       /* Position in AB:  ( 6.65 , 4.00 ) */
       /* Size in AB:  ( 15.42 , 92.72 ) */

       RUN constructObject (
             INPUT  'p-updsav91.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AddFunctionOne-RecordEdgePixels2PanelTypeSaveDeactivateTargetOnHidenoDisabledActionsHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_p-updsav91 ).
       RUN repositionObject IN h_p-updsav91 ( 7.73 , 100.00 ) NO-ERROR.
       RUN resizeObject IN h_p-updsav91 ( 11.04 , 12.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_b-pro_inversiones. */
       RUN addLink ( h_d-pro_inversiones , 'Data':U , h_b-pro_inversiones ).

       /* Links to  h_v-pro_inversiones. */
       RUN addLink ( h_d-pro_inversiones , 'Data':U , h_v-pro_inversiones ).
       RUN addLink ( h_v-pro_inversiones , 'Update':U , h_d-pro_inversiones ).
       RUN addLink ( h_p-updsav91 , 'TableIO':U , h_v-pro_inversiones ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_b-pro_inversiones ,
             opc:HANDLE IN FRAME F-CInv , 'BEFORE':U ).
       RUN adjustTabOrder ( h_p-updsav91 ,
             BUTTON-5:HANDLE IN FRAME fMain , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-PInv  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-PInv)
  THEN DELETE WIDGET W-PInv.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-PInv  _DEFAULT-ENABLE
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
  ENABLE BUTTON-1 BUTTON-2 BUTTON-5 BUTTON-3 
      WITH FRAME fMain IN WINDOW W-PInv.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  DISPLAY opc Buscar 
      WITH FRAME F-CInv IN WINDOW W-PInv.
  ENABLE opc Buscar BUTTON-4 
      WITH FRAME F-CInv IN WINDOW W-PInv.
  VIEW FRAME F-CInv IN WINDOW W-PInv.
  {&OPEN-BROWSERS-IN-QUERY-F-CInv}
  VIEW W-PInv.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject W-PInv 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir_Excel W-PInv 
PROCEDURE Imprimir_Excel :
{Incluido\Def_Excel.i}
 /* MANDA ENCABEZADO DE HOJA DE CALCULO*/
 
 E_NumFila = 1.
 E_NumColumn = 5.
 E_Fila      = "008" + "Cod Prod"
             + "040" + "Nombre"
             + "003" + "Cat"
             + "006" + "T. Val."
             + "006" + "Indice".  
 RUN W-GraExcel.w (INPUT E_Fila, INPUT E_NumColumn, OUTPUT E_CmpGrafic).

/* launch Excel so it is visible to the user */
chExcelApp:Visible = TRUE.

/* create a new Workbook */
chWorkbook = chExcelApp:Workbooks:Add().

/* get the active Worksheet */
chWorkSheet = chExcelApp:Sheets:Item(1).

  FOR EACH Bancos NO-LOCK:
      E_Fila2     = "".
      E_Fila2     = "003" + STRING(Pro_Inversiones.Cod_Producto,"X(3)")
                  + "040" + STRING(Pro_Inversiones.Nom_Producto,"X(40)")
                  + "001" + STRING(Pro_Inversiones.Categoria,"X")
                  + "006" + STRING(Pro_Inversiones.Tiempo_Valoracion,"X(6)")
                  + "004" + STRING(Pro_Inversiones.Indice,"X(4)").
{Incluido\imprimir_Excel.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject W-PInv 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.
  HIDE FRAME F-CInv.
/* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir W-PInv 
PROCEDURE ProcesoImprimir :
{INCLUIDO\RepEncabezado.I}    
  DEFINE VARIABLE ti AS INTEGER INITIAL 0.
  W_Reporte    = "REPORTE   : CONSULTA DE INVERSIONES - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
  W_EncColumna = "Producto                                     Tiempo Valoración   Indice Económico".

  VIEW FRAME F-Encabezado.
  VIEW FRAME F-Ftr.
  FOR EACH Pro_Inversiones BY Pro_Inversiones.Categoria:
      IF ti NE Pro_Inversiones.Categoria THEN DO:
        CASE Pro_Inversiones.Categoria: 
            WHEN 1 THEN
              DISPLAY 'Categoria: NEGOCIABLES.' AT 1 NO-LABEL.
            WHEN 2 THEN
              DISPLAY 'Categoria: PERMANENTES.' AT 1 NO-LABEL.
            WHEN 3 THEN
              DISPLAY 'Categoria: DISPONIBLES.' AT 1 NO-LABEL.
        END CASE.
        
        ti = Pro_Inversiones.Categoria.
      END.
      DISPLAY  Pro_Inversiones.Cod_Producto        AT 1   NO-LABEL FORMAT "999"
               Pro_Inversiones.Nom_Producto        AT 5   NO-LABEL FORMAT "X(40)"
               Pro_Inversiones.Tiempo_Valoracion   AT 50  NO-LABEL FORMAT "999999"
               Pro_Inversiones.Indice              AT 70  NO-LABEL FORMAT "9999"
      WITH FRAME F-reporte DOWN WIDTH 132 USE-TEXT STREAM-IO NO-BOX.
      DOWN WITH FRAME F-reporte.
  END.
  PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

