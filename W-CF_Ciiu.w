&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
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

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
/*&IF DEFINED(UIB_is_Running) = 0 &THEN
DEFINE INPUT PARAMETER dataSource AS HANDLE NO-UNDO.
&ELSE
DEF VARIABLE dataSource AS HANDLE NO-UNDO.
&ENDIF*/
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
&Scoped-define FRAME-NAME F-Consulta

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS R-Busca W_Valor BUTTON-9 RECT-2 
&Scoped-Define DISPLAYED-OBJECTS R-Busca W_Valor 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-cf_ciiu AS HANDLE NO-UNDO.
DEFINE VARIABLE h_d-cf_ciiu AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updsav91 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-cf_ciiu AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-9 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 9" 
     SIZE 8 BY 1.38.

DEFINE VARIABLE W_Valor AS INTEGER FORMAT "9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 23 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R-Busca AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Grupo", 1,
"Subgrupo", 2,
"Código", 3
     SIZE 35 BY .81
     FONT 5 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 60 BY 1.62.

DEFINE BUTTON BUTTON-10 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 10" 
     SIZE 4 BY 1.12.

DEFINE BUTTON BUTTON-11 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/impresora2.bmp":U
     LABEL "Button 11" 
     SIZE 10 BY 1.65.

DEFINE BUTTON BUTTON-12 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/informacion.bmp":U
     LABEL "Button 12" 
     SIZE 10 BY 1.65.

DEFINE BUTTON BUTTON-8 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "Consulta de Informacion" 
     SIZE 10 BY 1.65.

DEFINE RECTANGLE RECT-273
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 12 BY 5.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     BUTTON-12 AT ROW 1.81 COL 102
     BUTTON-11 AT ROW 3.42 COL 102
     BUTTON-8 AT ROW 5.04 COL 102
     BUTTON-10 AT ROW 20.65 COL 105
     RECT-273 AT ROW 1.54 COL 101
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113 BY 22
         BGCOLOR 17 .

DEFINE FRAME F-Consulta
     R-Busca AT ROW 15 COL 4 NO-LABEL
     W_Valor AT ROW 15 COL 36 COLON-ALIGNED NO-LABEL
     BUTTON-9 AT ROW 16.62 COL 55
     RECT-2 AT ROW 14.46 COL 3
     "Escoja el filtro por el cual desea realizar la busqueda," VIEW-AS TEXT
          SIZE 36 BY .81 AT ROW 16.62 COL 9
          FONT 4
     "digite la palabra clave a buscar, luego presione ENTER." VIEW-AS TEXT
          SIZE 38 BY .62 AT ROW 17.42 COL 9
          FONT 4
     "Nota:" VIEW-AS TEXT
          SIZE 5 BY .81 AT ROW 16.62 COL 3
          FGCOLOR 7 FONT 5
     " Elija el parámetro de búsqueda" VIEW-AS TEXT
          SIZE 28 BY 1.04 AT ROW 13.92 COL 5
          FGCOLOR 7 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 48 ROW 3.42
         SIZE 64 BY 18.04
         BGCOLOR 17 FONT 4
         TITLE "Consulta de Informacion".


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
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Configuración de Códigos Ciiu"
         HEIGHT             = 22
         WIDTH              = 113
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

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT wWin:LOAD-ICON("imagenes/cubos.ico":U) THEN
    MESSAGE "Unable to load icon: imagenes/cubos.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
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
ASSIGN FRAME F-Consulta:FRAME = FRAME fMain:HANDLE.

/* SETTINGS FOR FRAME F-Consulta
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F-Consulta:HIDDEN           = TRUE
       FRAME F-Consulta:MOVABLE          = TRUE.

/* SETTINGS FOR FRAME fMain
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Configuración de Códigos Ciiu */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Configuración de Códigos Ciiu */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME BUTTON-11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-11 wWin
ON CHOOSE OF BUTTON-11 IN FRAME fMain /* Button 11 */
DO:
  DEFINE VAR Listado AS CHARACTER INITIAL "".
  DEFINE VAR Tamano  AS INTEGER   INITIAL 2.
  
  listado = W_PathSpl + "L_Usuar.Lst".
  {Incluido\Imprimir.i "Listado" Tamano}
  /*{Imprimir.I "listado"}*/  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-12 wWin
ON CHOOSE OF BUTTON-12 IN FRAME fMain /* Button 12 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-8 wWin
ON CHOOSE OF BUTTON-8 IN FRAME fMain /* Consulta de Informacion */
DO:
  VIEW FRAME F-Consulta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Consulta
&Scoped-define SELF-NAME BUTTON-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-9 wWin
ON CHOOSE OF BUTTON-9 IN FRAME F-Consulta /* Button 9 */
DO:
  HIDE FRAME F-Consulta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Valor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Valor wWin
ON LEAVE OF W_Valor IN FRAME F-Consulta
DO:
  ASSIGN W_Valor.
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
             INPUT  'd-cf_ciiu.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamed-cf_ciiuUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_d-cf_ciiu ).
       RUN repositionObject IN h_d-cf_ciiu ( 18.23 , 90.00 ) NO-ERROR.
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'b-cf_ciiu.w':U ,
             INPUT  FRAME F-Consulta:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_b-cf_ciiu ).
       RUN repositionObject IN h_b-cf_ciiu ( 1.00 , 3.00 ) NO-ERROR.
       RUN resizeObject IN h_b-cf_ciiu ( 12.92 , 60.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'v-cf_ciiu.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_v-cf_ciiu ).
       RUN repositionObject IN h_v-cf_ciiu ( 10.15 , 10.00 ) NO-ERROR.
       /* Size in AB:  ( 4.85 , 80.00 ) */

       RUN constructObject (
             INPUT  'p-updsav91.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AddFunctionOne-RecordEdgePixels2PanelTypeSaveDeactivateTargetOnHidenoDisabledActionsHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_p-updsav91 ).
       RUN repositionObject IN h_p-updsav91 ( 9.35 , 101.00 ) NO-ERROR.
       RUN resizeObject IN h_p-updsav91 ( 10.77 , 12.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_b-cf_ciiu. */
       RUN addLink ( h_d-cf_ciiu , 'Data':U , h_b-cf_ciiu ).

       /* Links to SmartDataViewer h_v-cf_ciiu. */
       RUN addLink ( h_d-cf_ciiu , 'Data':U , h_v-cf_ciiu ).
       RUN addLink ( h_v-cf_ciiu , 'Update':U , h_d-cf_ciiu ).
       RUN addLink ( h_p-updsav91 , 'TableIO':U , h_v-cf_ciiu ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_b-cf_ciiu ,
             R-Busca:HANDLE IN FRAME F-Consulta , 'BEFORE':U ).
       RUN adjustTabOrder ( h_p-updsav91 ,
             BUTTON-8:HANDLE IN FRAME fMain , 'AFTER':U ).
       RUN adjustTabOrder ( h_v-cf_ciiu ,
             h_p-updsav91 , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createObjects wWin 
PROCEDURE createObjects :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.
/*  IF VALID-HANDLE(dataSource) THEN DO:
       
     RUN addLink
     (INPUT dataSource,
      INPUT 'DATA',
      INPUT h_d-CF_Ciiu).

  END.*/
  /* Code placed here will execute AFTER standard behavior.    */

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
  ENABLE BUTTON-12 BUTTON-11 BUTTON-8 BUTTON-10 RECT-273 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  DISPLAY R-Busca W_Valor 
      WITH FRAME F-Consulta IN WINDOW wWin.
  ENABLE R-Busca W_Valor BUTTON-9 RECT-2 
      WITH FRAME F-Consulta IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F-Consulta}
  VIEW wWin.
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
{Incluido\Def_Excel.i}
 /* MANDA ENCABEZADO DE HOJA DE CALCULO*/
 
 E_NumFila = 1.
 E_NumColumn = 4.
 E_Fila      = "002" + "Gr"
             + "002" + "Sb"
             + "004" + "Cod "
             + "045" + "Descripcion                                  ".
 RUN W-GraExcel.w (INPUT E_Fila, INPUT E_NumColumn, OUTPUT E_CmpGrafic).

/* launch Excel so it is visible to the user */
chExcelApp:Visible = TRUE.

/* create a new Workbook */
chWorkbook = chExcelApp:Workbooks:Add().

/* get the active Worksheet */
chWorkSheet = chExcelApp:Sheets:Item(1).

 FOR EACH Ciiu NO-LOCK:
      E_Fila2     = "".
      E_Fila2     = "002" + STRING(Ciiu.Grupo,"99")
                  + "002" + STRING(Ciiu.Subgrupo,"99")
                  + "004" + STRING(Ciiu.Codigo_Ciiu,"9999")
                  + "045" + STRING(Ciiu.Descripcion,"X(45)").
      {Incluido\imprimir_Excel.i}
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
    
  FRAME F-Consulta:HIDDEN = TRUE.
  
  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir wWin 
PROCEDURE ProcesoImprimir :
{INCLUIDO\RepEncabezado.I}    
  W_Reporte    = "REPORTE   : USUARIOS DEL SISTEMA - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
  W_EncColumna = "Grupo   SubGrupo  Codigo  Descripcion                                    Tipo".
  DEFINE VAR Des AS CHARACTER FORMAT "x(35)".
  DEFINE FRAME F-Detalle
     Ciiu.Grupo       AT 2
     Ciiu.Subgrupo    AT 12
     Ciiu.Codigo_Ciiu AT 20
     Des              AT 27
     Ciiu.Tipo        AT 75
  WITH WIDTH 132 USE-TEXT NO-BOX STREAM-IO NO-LABELS FRAME F-Detalle.
  
  VIEW FRAME F-Encabezado.
  VIEW FRAME F-Ftr.
  FOR EACH Ciiu BY Ciiu.Grupo BY Ciiu.Subgrupo BY Ciiu.Codigo_Ciiu:
     des = Ciiu.Descripcion.
     DISPLAY  Ciiu.Grupo       
              Ciiu.Subgrupo    
              Ciiu.Codigo_Ciiu 
              Des
              Ciiu.Tipo
     WITH FRAME F-Detalle WIDTH 132 NO-BOX.
  END.  
  PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

