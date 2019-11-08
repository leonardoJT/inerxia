&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
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
{incluido\Variable.i "SHARED"}
/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE W_Of          AS INTEGER INITIAL 0.  /* Rompimiento de Oficina */
DEFINE VARIABLE W_NomCompleto AS CHARACTER FORMAT 'X(30)'. /* Nit y Nombre Completo en informes */
DEFINE VARIABLE W_NomOpera    AS CHARACTER FORMAT 'X(30)'. /* Código y Nombre de la Operacion */
DEFINE VARIABLE pagina        AS INTEGER.    /* Número de página del folder para seleccionar informe */
DEFINE VARIABLE W_RptaRdio    AS INTEGER.       /* respuesta de radio-set de consulta */    
DEFINE VARIABLE W_EstAct      AS CHARACTER FORMAT 'X(10)'. /*Contiene el estado del Activo (Activo,Retirado,Depresiado)*/    
DEFINE VARIABLE W_NomAct      AS CHARACTER FORMAT 'X(30)'. /* Código y Nombre del Activo */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME consulta
&Scoped-define BROWSE-NAME B-Activos

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Act_Fijo

/* Definitions for BROWSE B-Activos                                     */
&Scoped-define FIELDS-IN-QUERY-B-Activos Act_Fijo.Agencia Act_Fijo.Codigo ~
Act_Fijo.Nombre 
&Scoped-define ENABLED-FIELDS-IN-QUERY-B-Activos 
&Scoped-define QUERY-STRING-B-Activos FOR EACH Act_Fijo NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-B-Activos OPEN QUERY B-Activos FOR EACH Act_Fijo NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-B-Activos Act_Fijo
&Scoped-define FIRST-TABLE-IN-QUERY-B-Activos Act_Fijo


/* Definitions for FRAME consulta                                       */
&Scoped-define OPEN-BROWSERS-IN-QUERY-consulta ~
    ~{&OPEN-QUERY-B-Activos}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS B-Activos Filtro Buscar BUTTON-4 
&Scoped-Define DISPLAYED-OBJECTS Filtro Buscar 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-mov_activos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_d-act_fijos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_d-mov_actfijos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updsav91 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updsav91-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-act_fijo AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-mov_activos AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-4 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 4" 
     SIZE 10.43 BY 1.54.

DEFINE VARIABLE Buscar AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY .81
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE Filtro AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Todos", 1,
"Agencia", 2,
"Código", 3,
"Nombre", 4
     SIZE 46 BY 1.31
     BGCOLOR 17 FONT 4 NO-UNDO.

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/informacion.bmp":U
     LABEL "Button 1" 
     SIZE 9.72 BY 1.54 TOOLTIP "Información".

DEFINE BUTTON BUTTON-2 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/impresora2.bmp":U
     LABEL "Button 2" 
     SIZE 9.72 BY 1.54 TOOLTIP "Imprimir".

DEFINE BUTTON BUTTON-3 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 3" 
     SIZE 5 BY 1.12 TOOLTIP "Ayuda".

DEFINE BUTTON BUTTON-5 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "Button 5" 
     SIZE 9.72 BY 1.54 TOOLTIP "Consulta".

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 11.14 BY 5.12.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY B-Activos FOR 
      Act_Fijo SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE B-Activos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS B-Activos wWin _STRUCTURED
  QUERY B-Activos NO-LOCK DISPLAY
      Act_Fijo.Agencia FORMAT "999":U COLUMN-FONT 4 LABEL-FGCOLOR 0 LABEL-BGCOLOR 17 LABEL-FONT 4
      Act_Fijo.Codigo FORMAT "X(6)":U COLUMN-FONT 4 LABEL-FGCOLOR 0 LABEL-BGCOLOR 17 LABEL-FONT 4
      Act_Fijo.Nombre FORMAT "X(40)":U COLUMN-FONT 4 LABEL-FGCOLOR 0 LABEL-BGCOLOR 17 LABEL-FONT 4
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 86 BY 9.96
         BGCOLOR 15 FGCOLOR 0 FONT 4 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     BUTTON-1 AT ROW 1.58 COL 100.43
     BUTTON-2 AT ROW 3.19 COL 100.43
     BUTTON-5 AT ROW 4.77 COL 100.43
     BUTTON-3 AT ROW 20.15 COL 104
     RECT-1 AT ROW 1.38 COL 99.57
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 114.29 BY 21.12
         BGCOLOR 17 FGCOLOR 0 .

DEFINE FRAME consulta
     B-Activos AT ROW 1 COL 1
     Filtro AT ROW 11.08 COL 4.57 NO-LABEL
     Buscar AT ROW 11.27 COL 58.29 NO-LABEL
     BUTTON-4 AT ROW 12.5 COL 76
     "Digite en el campo en blanco según su selección" VIEW-AS TEXT
          SIZE 45 BY .81 AT ROW 12.5 COL 5
          FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 11 ROW 2.62
         SIZE 87 BY 14.54
         BGCOLOR 17 FGCOLOR 0 
         TITLE "Consulta Activos Fijos".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Design Page: 1
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Activos Fijos"
         HEIGHT             = 21.12
         WIDTH              = 114.29
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.14
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.14
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME consulta:FRAME = FRAME fMain:HANDLE.

/* SETTINGS FOR FRAME consulta
                                                                        */
/* BROWSE-TAB B-Activos 1 consulta */
ASSIGN 
       FRAME consulta:HIDDEN           = TRUE
       FRAME consulta:MOVABLE          = TRUE.

/* SETTINGS FOR FILL-IN Buscar IN FRAME consulta
   ALIGN-L                                                              */
/* SETTINGS FOR FRAME fMain
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE B-Activos
/* Query rebuild information for BROWSE B-Activos
     _TblList          = "bdcentral.Act_Fijo"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > bdcentral.Act_Fijo.Agencia
"Act_Fijo.Agencia" ? ? "integer" ? ? 4 17 0 4 no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > bdcentral.Act_Fijo.Codigo
"Act_Fijo.Codigo" ? ? "character" ? ? 4 17 0 4 no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > bdcentral.Act_Fijo.Nombre
"Act_Fijo.Nombre" ? ? "character" ? ? 4 17 0 4 no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE B-Activos */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Activos Fijos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Activos Fijos */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME B-Activos
&Scoped-define SELF-NAME B-Activos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Activos wWin
ON MOUSE-SELECT-DBLCLICK OF B-Activos IN FRAME consulta
DO:
  DEFINE VARIABLE palabra AS CHARACTER NO-UNDO.
  DEFINE VARIABLE op      AS CHARACTER NO-UNDO.
  ASSIGN op = Act_Fijo.Codigo:SCREEN-VALUE IN BROWSE B-Activos.
  FIND FIRST Act_Fijo WHERE Act_Fijo.Codigo BEGINS OP NO-LOCK NO-ERROR.
  ASSIGN palabra = "Act_Fijo.Codigo BEGINS '" + OP + "'".
  IF AVAILABLE Act_Fijo THEN DO:
      {SET QueryWhere palabra h_d-Act_Fijos}.
      {FN OpenQuery h_d-Act_Fijos}.
      HIDE FRAME Consulta.  
     END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Buscar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Buscar wWin
ON LEAVE OF Buscar IN FRAME consulta
OR RETURN OF Buscar DO:
  IF SELF:SCREEN-VALUE NE "":U THEN DO:
     ASSIGN W_RptaRdio = Filtro:INPUT-VALUE.
     CASE W_RptaRdio:
        WHEN 1 THEN
          OPEN QUERY B-Activos FOR EACH Act_Fijo WHERE Act_Fijo.Agencia >= 1 NO-LOCK.          
        WHEN 2 THEN 
          OPEN QUERY B-Activos FOR EACH Act_Fijo WHERE TRIM(SELF:SCREEN-VALUE) BEGINS STRING(Act_Fijo.Agencia,'999') NO-LOCK.    
        WHEN 3 THEN 
          OPEN QUERY B-Activos FOR EACH Act_Fijo WHERE TRIM(SELF:SCREEN-VALUE) BEGINS Act_Fijo.Codigo NO-LOCK.    
        WHEN 4 THEN 
          OPEN QUERY B-Activos FOR EACH Act_Fijo WHERE STRING(SELF:SCREEN-VALUE,'!(15)') BEGINS Act_Fijo.Nombre NO-LOCK.          
     END CASE.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 wWin
ON CHOOSE OF BUTTON-1 IN FRAME fMain /* Button 1 */
DO:
  RUN w-infdia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 wWin
ON CHOOSE OF BUTTON-2 IN FRAME fMain /* Button 2 */
DO:
   DEFINE VAR Listado AS CHARACTER INITIAL "".
   
   Listado = W_PathSpl + "LinAct.LST".
  {Incluido\Imprimir.I "Listado"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME consulta
&Scoped-define SELF-NAME BUTTON-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-4 wWin
ON CHOOSE OF BUTTON-4 IN FRAME consulta /* Button 4 */
DO:
  HIDE FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 wWin
ON CHOOSE OF BUTTON-5 IN FRAME fMain /* Button 5 */
DO:
  VIEW FRAME Consulta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME consulta
&Scoped-define SELF-NAME Filtro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Filtro wWin
ON VALUE-CHANGED OF Filtro IN FRAME consulta
DO:
 ASSIGN W_RptaRdio = Filtro:INPUT-VALUE.
 IF W_RptaRdio = 1 THEN
  OPEN QUERY B-Activos FOR EACH Act_Fijo WHERE Act_Fijo.Agencia >= 1 NO-LOCK.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
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
             INPUT  'folder.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FolderLabels':U + 'Ficha de Activo|Movimiento Activo|Detalle Movimiento' + 'FolderTabWidth0FolderFont4HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_folder ).
       RUN repositionObject IN h_folder ( 1.27 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_folder ( 20.73 , 98.00 ) NO-ERROR.

       /* Links to SmartFolder h_folder. */
       RUN addLink ( h_folder , 'Page':U , THIS-PROCEDURE ).

    END. /* Page 0 */

    WHEN 1 THEN DO:
       RUN constructObject (
             INPUT  'd-act_fijos.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamed-act_fijosUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_d-act_fijos ).
       RUN repositionObject IN h_d-act_fijos ( 1.81 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'v-act_fijo.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_v-act_fijo ).
       RUN repositionObject IN h_v-act_fijo ( 2.08 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 19.08 , 96.00 ) */

       RUN constructObject (
             INPUT  'p-updsav91.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AddFunctionOne-RecordEdgePixels2PanelTypeSaveDeactivateTargetOnHidenoDisabledActionsHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_p-updsav91 ).
       RUN repositionObject IN h_p-updsav91 ( 8.54 , 100.00 ) NO-ERROR.
       RUN resizeObject IN h_p-updsav91 ( 10.77 , 11.00 ) NO-ERROR.

       /* Links to SmartDataViewer h_v-act_fijo. */
       RUN addLink ( h_d-act_fijos , 'Data':U , h_v-act_fijo ).
       RUN addLink ( h_v-act_fijo , 'Update':U , h_d-act_fijos ).
       RUN addLink ( h_p-updsav91 , 'TableIO':U , h_v-act_fijo ).

    END. /* Page 1 */

    WHEN 2 THEN DO:
       RUN constructObject (
             INPUT  'd-mov_actfijos.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsMov_Activos.Codigo,CodigoRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamed-mov_actfijosUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_d-mov_actfijos ).
       RUN repositionObject IN h_d-mov_actfijos ( 2.08 , 50.00 ) NO-ERROR.
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'v-mov_activos.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_v-mov_activos ).
       RUN repositionObject IN h_v-mov_activos ( 2.35 , 5.00 ) NO-ERROR.
       /* Size in AB:  ( 19.19 , 89.00 ) */

       RUN constructObject (
             INPUT  'p-updsav91.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AddFunctionOne-RecordEdgePixels2PanelTypeSaveDeactivateTargetOnHidenoDisabledActionsHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_p-updsav91-2 ).
       RUN repositionObject IN h_p-updsav91-2 ( 8.54 , 100.00 ) NO-ERROR.
       RUN resizeObject IN h_p-updsav91-2 ( 10.77 , 11.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN initPages ('1') NO-ERROR.

       /* Links to SmartDataObject h_d-mov_actfijos. */
       RUN addLink ( h_d-act_fijos , 'Data':U , h_d-mov_actfijos ).

       /* Links to SmartDataViewer h_v-mov_activos. */
       RUN addLink ( h_d-mov_actfijos , 'Data':U , h_v-mov_activos ).
       RUN addLink ( h_v-mov_activos , 'Update':U , h_d-mov_actfijos ).
       RUN addLink ( h_p-updsav91-2 , 'TableIO':U , h_v-mov_activos ).

    END. /* Page 2 */

    WHEN 3 THEN DO:
       RUN constructObject (
             INPUT  'b-mov_activos.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_b-mov_activos ).
       RUN repositionObject IN h_b-mov_activos ( 2.35 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_b-mov_activos ( 19.65 , 96.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN initPages ('2') NO-ERROR.

       /* Links to SmartDataBrowser h_b-mov_activos. */
       RUN addLink ( h_d-mov_actfijos , 'Data':U , h_b-mov_activos ).

       /* Adjust the tab order of the smart objects. */
    END. /* Page 3 */

  END CASE.
  /* Select a Startup page. */
  IF currentPage eq 0
  THEN RUN selectPage IN THIS-PROCEDURE ( 1 ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
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
  ENABLE BUTTON-1 BUTTON-2 BUTTON-5 BUTTON-3 RECT-1 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  DISPLAY Filtro Buscar 
      WITH FRAME consulta IN WINDOW wWin.
  ENABLE B-Activos Filtro Buscar BUTTON-4 
      WITH FRAME consulta IN WINDOW wWin.
  VIEW FRAME consulta IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-consulta}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Excel_Fijo wWin 
PROCEDURE Excel_Fijo :
/*------------------------------------------------------------------------------
  Purpose: Envia A Excel Los Datos Necesarios Para el Informe.    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{Incluido\Def_Excel.i}
 /* MANDA ENCABEZADO DE HOJA DE CALCULO*/
 
 E_NumFila = 1.
 E_NumColumn = 7.
 E_Fila      = "007" + "Agencia"
             + "012" + "Cod Act_Fijo"
             + "040" + "Nombre                                  "
             + "010" + "Estado    "   
             + "040" + "Descripcion                             "
             + "018" + "Valor Activo      "
             + "012" + "Fecha Compra".  
 RUN W-GraExcel.w (INPUT E_Fila, INPUT E_NumColumn, OUTPUT E_CmpGrafic).

/* launch Excel so it is visible to the user */
chExcelApp:Visible = TRUE.

/* create a new Workbook */
chWorkbook = chExcelApp:Workbooks:Add().

/* get the active Worksheet */
chWorkSheet = chExcelApp:Sheets:Item(1).

  FOR EACH Act_Fijo NO-LOCK:
      CASE Act_Fijo.Estado:
          WHEN 1 THEN
            W_NomAct = 'Activo    '. 
          WHEN 2 THEN
            W_NomAct = 'Retirado  '.
          WHEN 3 THEN
            W_NomAct = 'Depresiado'.
      END CASE.
      E_Fila2     = "".
      E_Fila2     = "007" + STRING(Act_Fijo.Agencia,"999") + '    '
                  + "012" + STRING(Act_Fijo.Codigo,"999999") + '      '
                  + "040" + STRING(Act_Fijo.Nombre,"X(40)")
                  + "010" + STRING(W_NomAct,"X(10)")
                  + "040" + STRING(Act_Fijo.Descripcion,"X(40)")
                  + "018" + STRING(Act_Fijo.Val_Compra,"->>,>>>,>>>,>>>.99")
                  + "012" + STRING(Act_Fijo.Fec_Compra,"99/99/9999") + '  '.
{Incluido\imprimir_Excel.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Excel_Mvto wWin 
PROCEDURE Excel_Mvto :
/*------------------------------------------------------------------------------
  Purpose: Envia A Excel Los Datos Necesarios Para el Informe.    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{Incluido\Def_Excel.i}
 /* MANDA ENCABEZADO DE HOJA DE CALCULO*/
 
 E_NumFila = 1.
 E_NumColumn = 8.
 E_Fila      = "007" + "Agencia"
             + "006" + "Codigo"
             + "030" + "Nombre Activo Fijo            "
             + "009" + "Cod Opera"   
             + "030" + "Nombre Operacion              "
             + "030" + "Descripcion                   "
             + "018" + "Valor Transaccion "
             + "010" + "Fecha Mvto".  
 RUN W-GraExcel.w (INPUT E_Fila, INPUT E_NumColumn, OUTPUT E_CmpGrafic).

/* launch Excel so it is visible to the user */
chExcelApp:Visible = TRUE.

/* create a new Workbook */
chWorkbook = chExcelApp:Workbooks:Add().

/* get the active Worksheet */
chWorkSheet = chExcelApp:Sheets:Item(1).

  FOR EACH Mov_Activos NO-LOCK:
      FIND FIRST Act_Fijo WHERE Act_Fijo.Codigo = Mov_Activos.Codigo NO-LOCK NO-ERROR.
      IF AVAILABLE Act_Fijo THEN
        W_NomAct = TRIM(Act_Fijo.Nombre).
       ELSE
        W_NomAct = 'Activo Fijo No Existe.'.     
      FIND FIRST Operacion WHERE Operacion.Cod_Operacion = Mov_Activos.Cod_Operacion NO-LOCK NO-ERROR.
      IF AVAILABLE Operacion THEN
         W_NomOpera = TRIM(Operacion.Nom_Operacion).
       ELSE
         W_NomOpera = 'Operacion No Existe...'.      
      E_Fila2     = "".
      E_Fila2     = "007" + STRING(Mov_Activos.Agencia,"999") + '    '
                  + "006" + STRING(Mov_Activos.Codigo,'999999')
                  + "030" + STRING(W_NomAct,'X(30)') 
                  + "009" + STRING(Mov_Activos.Cod_Operacion,'999999999')
                  + "030" + STRING(W_NomOpera,'X(30)') 
                  + "030" + STRING(Mov_Activos.Descripcion,"X(30)")
                  + "018" + STRING(Mov_Activos.Valor_TX,"->>,>>>,>>>,>>>.99")
                  + "010" + STRING(Mov_Activos.Fec_Movimiento,"99/99/9999").
{Incluido\imprimir_Excel.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir_Excel wWin 
PROCEDURE Imprimir_Excel :
/*------------------------------------------------------------------------------
  Purpose: escoje el reporte según la página del folder.    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 ASSIGN Pagina = getCurrentPage().
 CASE pagina:
      WHEN 1 THEN 
        RUN Excel_Fijo. 
      WHEN 2 THEN
        RUN Excel_Mvto.
 END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.
  HIDE FRAME Consulta.
  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir wWin 
PROCEDURE ProcesoImprimir :
{INCLUIDO\RepEncabezado.I}    
  ASSIGN Pagina = getCurrentPage().
  CASE pagina:
      WHEN 1 THEN DO:
       W_Reporte    = "REPORTE   : CONSULTA DE ACTIVOS FIJOS - FICHA " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
       W_EncColumna = "Activo Fijo                            Estado     Responsable                    Descripcion                    Valor Activo Fijo  Fec.Compra".   
      END.
      WHEN 2 THEN DO:
       W_Reporte    = "REPORTE   : CONSULTA DE ACTIVOS FIJOS - MOVIMIENTO " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
       W_EncColumna = "Activo Fijo                    Operacion                      Nit                            Fec.Mvto   Valor Transaccion".
      END.
  END CASE.
  VIEW FRAME F-Encabezado.
  VIEW FRAME F-Ftr.
  CASE pagina:
      WHEN 1 THEN 
        RUN RepFicha. 
      WHEN 2 THEN
        RUN RepTran.
 END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RepFicha wWin 
PROCEDURE RepFicha :
/*------------------------------------------------------------------------------
  Purpose:  Genera el Informe de Ficha de Activo (Todos)   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  W_Of = 0.  
  FOR EACH Act_Fijo BY Act_Fijo.Agencia:
   IF Act_Fijo.Agencia NE 0 AND Act_Fijo.Codigo NE ' ' THEN DO:
    FIND FIRST Agencias WHERE Agencias.Agencia = Act_Fijo.Agencia NO-LOCK NO-ERROR.
    IF W_Of NE Act_Fijo.Agencia THEN DO:
     IF AVAILABLE Agencias THEN 
       DISPLAY 'AGENCIA: ' + STRING(Agencias.Agencia,'999') + ' ' + TRIM(Agencias.Nombre) FORMAT "X(30)" AT 10 NO-LABEL.
      ELSE
       DISPLAY 'AGENCIA: ' + STRING(Act_Fijo.Agencia,'999') ' No Existe...' FORMAT "X(30)" NO-LABEL AT 10.
     W_Of = Act_Fijo.Agencia.
    END.    
    FIND FIRST Clientes WHERE Clientes.Nit = Act_Fijo.Nit_Responsable NO-LOCK NO-ERROR.
    IF AVAILABLE Clientes THEN
       ASSIGN W_NomCompleto = TRIM(Clientes.Nit) + '-' + TRIM(Clientes.Nombre) + ' ' + TRIM(Clientes.Apellido1).
      ELSE
       IF Act_Fijo.Nit_Responsable NE ? THEN
         ASSIGN W_NomCompleto = TRIM(Act_Fijo.Nit_Responsable) + ' No Existe...'.
        ELSE
         ASSIGN W_NomCompleto = 'No Esta Asignado.'.    
    CASE Act_Fijo.Estado:
        WHEN 1 THEN
          W_EstAct = 'Activo'.
        WHEN 2 THEN
          W_EstAct = 'Retirado'.
        WHEN 3 THEN
          W_EstAct = 'Depresiado'.  
    END CASE.
    DISPLAY  Act_Fijo.Codigo         AT 1   NO-LABEL FORMAT "999999"
             Act_Fijo.Nombre         AT 8   NO-LABEL FORMAT "X(30)"
             W_EstAct                AT 40  NO-LABEL FORMAT "X(10)"
             W_NomCompleto             AT 52  NO-LABEL FORMAT "X(30)"
             Act_Fijo.Descripcion    AT 84  NO-LABEL FORMAT "X(30)"
             Act_Fijo.Val_Compra     AT 106  NO-LABEL FORMAT "->>,>>>,>>>,>>9.99"
             Act_Fijo.Fec_Compra     AT 126 NO-LABEL FORMAT "99/99/9999"
    WITH FRAME F-reporte DOWN WIDTH 138 USE-TEXT STREAM-IO NO-BOX.
    DOWN WITH FRAME F-reporte.
   END.
 
  END. /*endfor*/
 END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RepTran wWin 
PROCEDURE RepTran :
/*------------------------------------------------------------------------------
  Purpose:     genera el reporte de movimiento de los activos.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  W_Of = 0.  
  FOR EACH Mov_Activos BY Mov_Activos.Agencia:
    IF W_Of NE Mov_Activos.Agencia THEN DO:
      FIND FIRST Agencias WHERE Agencias.Agencia = Mov_Activos.Agencia NO-LOCK NO-ERROR.
      IF AVAILABLE Agencias THEN 
         DISPLAY 'AGENCIA: ' + STRING(Agencias.Agencia,'999') + ' ' + TRIM(Agencias.Nombre) FORMAT "X(30)" AT 10 NO-LABEL.
        ELSE
         DISPLAY 'AGENCIA: ' + STRING(Mov_Activos.Agencia,'999') ' No Existe...' FORMAT "X(30)" NO-LABEL.
      W_Of = Mov_Activos.Agencia.
    END. /*end if*/
    /* Operaciones */
    FIND FIRST Operacion WHERE Operacion.Cod_Operacion = Mov_Activos.Cod_Operacion NO-LOCK NO-ERROR.
      IF AVAILABLE Operacion THEN
         W_NomOpera = STRING(Operacion.Cod_Operacion,'999999999') + ' ' + TRIM(Operacion.Nom_Operacion).
       ELSE
         W_NomOpera = STRING(Mov_Activos.Cod_Operacion,'999999999') + ' No Existe...'.
    /* Nit */
    FIND FIRST Clientes WHERE Clientes.Nit = Mov_Activos.Nit NO-LOCK NO-ERROR.
    IF AVAILABLE Clientes THEN
      ASSIGN W_NomCompleto = TRIM(Clientes.Nit) + '-' + TRIM(Clientes.Nombre) + ' ' + TRIM(Clientes.Apellido1).
     ELSE
      ASSIGN W_NomCompleto = TRIM(Mov_Activos.Nit) + ' No Existe...'.
    /* Activo Fijo */
    FIND FIRST Act_Fijo WHERE Act_Fijo.Codigo = Mov_Activos.Codigo NO-LOCK NO-ERROR.
    IF AVAILABLE Act_Fijo THEN
      W_NomAct = STRING(Act_Fijo.Codigo,'999999') + ' ' + TRIM(Act_Fijo.Nombre).   
     ELSE
      W_NomAct = STRING(Mov_Activos.Codigo,'999999') + ' No Existe.'.     
    DISPLAY  W_NomAct                   AT 1   NO-LABEL FORMAT "X(30)"
             W_NomOpera                 AT 32  NO-LABEL FORMAT "X(30)"
             W_NomCompleto              AT 64  NO-LABEL FORMAT "X(30)"
             Mov_Activos.Fec_Movimiento AT 96  NO-LABEL FORMAT "99/99/9999"
             Mov_Activos.Valor_TX       AT 107 NO-LABEL FORMAT "->>,>>>,>>>,>>9.99"
      WITH FRAME F-reporte DOWN WIDTH 132 USE-TEXT STREAM-IO NO-BOX.
      DOWN WITH FRAME F-reporte.
      PAGE.
  END. /*endfor*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

