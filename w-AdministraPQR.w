&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
{adecomm/appserv.i}
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

/* Local Variable Definitions ---                                       */

{src/adm2/widgetprto.i}

{INCLUIDO\VARIABLE.I "SHARED"}
DEFINE VAR W_Dispositivo AS CHARACTER INITIAL " " NO-UNDO.

DEFINE VAR W_Ok AS LOGICAL.

DEFINE VARIABLE vcImagen AS CHARACTER FORMAT "X(40)"  NO-UNDO.
DEFINE VARIABLE vdaFecha  AS DATE     NO-UNDO.
DEFINE VARIABLE vdaFecIni AS DATE     NO-UNDO.
DEFINE VARIABLE vdaFecFin AS DATE     NO-UNDO.
DEFINE VARIABLE viAno AS INTEGER     NO-UNDO.
DEFINE VARIABLE viMes AS INTEGER     NO-UNDO.

DEFINE VARIABLE vcAgencia    AS CHARACTER NO-UNDO.
DEFINE VARIABLE vcArea_Resp  AS CHARACTER NO-UNDO.
DEFINE VARIABLE vcProceso    AS CHARACTER NO-UNDO.
DEFINE VARIABLE vcProducto   AS CHARACTER NO-UNDO.
DEFINE VARIABLE vcUsuarioResp AS CHARACTER NO-UNDO.
DEFINE VARIABLE vcRequerimientos AS CHARACTER NO-UNDO.
DEFINE VARIABLE vcCanal      AS CHARACTER NO-UNDO.
DEFINE VARIABLE vcEstado     AS CHARACTER NO-UNDO.
DEFINE VARIABLE vcCliente    AS CHARACTER NO-UNDO.

DEFINE VARIABLE viLocal AS INTEGER   NO-UNDO.   /* Contador auxiliar */
DEFINE VARIABLE vcListado AS CHARACTER NO-UNDO. 

DEFINE TEMP-TABLE ttpqr
    FIELD tip_pqr     LIKE pqr.tip_pqr     COLUMN-LABEL "Tipo!Solicitud"
    FIELD canal       LIKE pqr.canal       COLUMN-LABEL "Canal!Recepcion"
    FIELD Cod_Proceso LIKE pqr.Cod_Proceso COLUMN-LABEL "Proceso!Misional"
    FIELD Estado      LIKE pqr.Estado      COLUMN-LABEL "Estado"
    FIELD Area_resp   LIKE pqr.Area_resp   COLUMN-LABEL "Area!Responsable"
    FIELD Cod_Req     LIKE pqr.Cod_Req     COLUMN-LABEL "Requerimiento"
    FIELD Per_Resp    LIKE pqr.Per_Resp COLUMN-LABEL "Persona!Responsable"
    FIELD Descripcion LIKE Varios.Descripcion
    FIELD Recibido    AS DECIMAL FORMAT ">>>,>>>,>>9"
    FIELD Contestados AS DECIMAL FORMAT ">>>,>>>,>>9".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Img_MesF Img_MesI RECT-282 RECT-283 Cmb_Tipo ~
Btn_Done Btn_print Cmb_Agencias Cmb_Canal BTN-Responsable Cmb_Clase ~
Cmb_Estado RS-OrdenFecha BTN-Fecini BTN-FecFin 
&Scoped-Define DISPLAYED-OBJECTS Cmb_Tipo Cmb_Agencias Cmb_Canal Cmb_Clase ~
Cmb_Estado RS-OrdenFecha W_DiaIni AnoIni W_DiaFin AnoFin EDITOR-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_dpqr AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynbrowser AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vpqr AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN-FecFin 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Fecha Fin" 
     SIZE 4 BY .54.

DEFINE BUTTON BTN-Fecini 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Fecha Inicial" 
     SIZE 3 BY .54.

DEFINE BUTTON BTN-Responsable 
     IMAGE-UP FILE "adeicon/rbuild%.ico":U
     LABEL "" 
     SIZE 6 BY 1.5.

DEFINE BUTTON Btn_Done DEFAULT 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "" 
     SIZE 6 BY 1.5 TOOLTIP "Salir"
     BGCOLOR 8 .

DEFINE BUTTON Btn_print 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/impresora2.bmp":U
     LABEL "Imprimir" 
     SIZE 6 BY 1.5.

DEFINE VARIABLE Cmb_Agencias AS CHARACTER FORMAT "X(50)":U 
     LABEL "Agencias" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     LIST-ITEMS "000 - Todas las Agencias" 
     DROP-DOWN-LIST
     SIZE 51 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_Canal AS CHARACTER FORMAT "X(40)":U 
     LABEL "Canal Recepcion" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     LIST-ITEMS "00000 - Todos los Canales" 
     DROP-DOWN-LIST
     SIZE 51 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_Clase AS CHARACTER FORMAT "X(40)":U 
     LABEL "Proceso Misional" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     LIST-ITEMS "00000 - Todos los Procesos" 
     DROP-DOWN-LIST
     SIZE 51 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_Estado AS CHARACTER FORMAT "X(40)":U 
     LABEL "Estado" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     LIST-ITEMS "00000 - Todos los Estados" 
     DROP-DOWN-LIST
     SIZE 51 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_Tipo AS CHARACTER FORMAT "X(40)":U 
     LABEL "Tipo Solicitud" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     LIST-ITEMS "00000 - Todas las Solicitudes" 
     DROP-DOWN-LIST
     SIZE 51 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE EDITOR-1 AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE 103 BY 16.15
     FONT 8 NO-UNDO.

DEFINE VARIABLE AnoFin AS INTEGER FORMAT "9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE AnoIni AS INTEGER FORMAT "9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_DiaFin AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4 BY .81 NO-UNDO.

DEFINE VARIABLE W_DiaIni AS DECIMAL FORMAT "99":U INITIAL 1 
     VIEW-AS FILL-IN 
     SIZE 4 BY .81 NO-UNDO.

DEFINE IMAGE Img_MesF
     FILENAME "adeicon/blank":U
     SIZE 11 BY .81.

DEFINE IMAGE Img_MesI
     FILENAME "adeicon/blank":U
     SIZE 11 BY .81.

DEFINE VARIABLE RS-OrdenFecha AS LOGICAL 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Ascendente", yes,
"Descendente", no
     SIZE 16 BY 1.88 NO-UNDO.

DEFINE RECTANGLE RECT-282
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 26 BY 1.88.

DEFINE RECTANGLE RECT-283
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 27 BY 1.88.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     Cmb_Tipo AT ROW 1.54 COL 22 COLON-ALIGNED WIDGET-ID 118
     Btn_Done AT ROW 2.04 COL 97.86 WIDGET-ID 152
     Btn_print AT ROW 2.08 COL 90 WIDGET-ID 70
     Cmb_Agencias AT ROW 2.62 COL 22 COLON-ALIGNED WIDGET-ID 116
     Cmb_Canal AT ROW 3.69 COL 22 COLON-ALIGNED WIDGET-ID 120
     BTN-Responsable AT ROW 4.23 COL 90 WIDGET-ID 160
     Cmb_Clase AT ROW 4.69 COL 22 COLON-ALIGNED WIDGET-ID 122
     Cmb_Estado AT ROW 5.85 COL 22 COLON-ALIGNED WIDGET-ID 122
     RS-OrdenFecha AT ROW 7.19 COL 79 NO-LABEL WIDGET-ID 154
     W_DiaIni AT ROW 8 COL 20.29 COLON-ALIGNED NO-LABEL WIDGET-ID 146
     AnoIni AT ROW 8 COL 35.29 COLON-ALIGNED NO-LABEL WIDGET-ID 126
     BTN-Fecini AT ROW 8 COL 43.29 WIDGET-ID 128
     W_DiaFin AT ROW 8 COL 48.29 NO-LABEL WIDGET-ID 144
     AnoFin AT ROW 8 COL 61.29 COLON-ALIGNED NO-LABEL WIDGET-ID 124
     BTN-FecFin AT ROW 8.04 COL 69.14 WIDGET-ID 130
     EDITOR-1 AT ROW 10.96 COL 2 NO-LABEL WIDGET-ID 162
     "Fecha Inicial" VIEW-AS TEXT
          SIZE 12.86 BY .69 AT ROW 7.19 COL 26.29 WIDGET-ID 142
          BGCOLOR 17 FGCOLOR 7 
     "Fecha Final" VIEW-AS TEXT
          SIZE 11.86 BY .62 AT ROW 7.19 COL 54.14 WIDGET-ID 140
          BGCOLOR 17 FGCOLOR 7 
     Img_MesF AT ROW 8 COL 52.29 WIDGET-ID 132
     Img_MesI AT ROW 8 COL 26.29 WIDGET-ID 134
     RECT-282 AT ROW 7.08 COL 21.29 WIDGET-ID 136
     RECT-283 AT ROW 7.08 COL 47.29 WIDGET-ID 138
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.04
         SIZE 106.86 BY 27.19
         BGCOLOR 17 FONT 5 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Administra PQR"
         HEIGHT             = 27.08
         WIDTH              = 107
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.29
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
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
/* SETTINGS FOR FRAME fMain
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN AnoFin IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN AnoIni IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       Btn_print:HIDDEN IN FRAME fMain           = TRUE.

/* SETTINGS FOR EDITOR EDITOR-1 IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       EDITOR-1:AUTO-INDENT IN FRAME fMain      = TRUE
       EDITOR-1:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN W_DiaFin IN FRAME fMain
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN W_DiaIni IN FRAME fMain
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Administra PQR */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Administra PQR */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-FecFin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-FecFin wWin
ON CHOOSE OF BTN-FecFin IN FRAME fMain /* Fecha Fin */
DO:
  RUN C-Fecha.r (OUTPUT vcImagen, OUTPUT viAno, OUTPUT viMes, OUTPUT vdaFecha).
  ASSIGN W_OK = Img_MesF:LOAD-IMAGE("imagenes\month" + STRING(viMes) + ".gif")
         W_DiaFin:SCREEN-VALUE = STRING(DAY(vdaFecha))
         AnoFin:SCREEN-VALUE = STRING(viAno)
         vdaFecFin = vdaFecha.

  RUN MuestraInfo.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-Fecini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-Fecini wWin
ON CHOOSE OF BTN-Fecini IN FRAME fMain /* Fecha Inicial */
DO:
  RUN C-Fecha.r (OUTPUT vcImagen, OUTPUT viAno, OUTPUT viMes, OUTPUT vdaFecha).
  ASSIGN W_OK = Img_MesI:LOAD-IMAGE("imagenes\month" + STRING(viMes) + ".gif")
         AnoIni:SCREEN-VALUE = STRING(viAno)
         W_DiaIni:SCREEN-VALUE = STRING(DAY(vdaFecha))
         vdaFecIni = vdaFecha.

  RUN MuestraInfo.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-Responsable
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-Responsable wWin
ON CHOOSE OF BTN-Responsable IN FRAME fMain
DO:
    IF SELF:LABEL = "Modifica" THEN DO:
          ASSIGN SELF:LABEL = "&Actualiza"
                 SELF:TOOLTIP = "Actualiza".
          RUN SelectPage(2).
      END.
      ELSE DO:
          ASSIGN SELF:LABEL = "Modifica"
              SELF:TOOLTIP = "Modifica".
          RUN SelectPage(1).
      END.
  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done wWin
ON CHOOSE OF Btn_Done IN FRAME fMain
DO:
  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
    &ELSE
      RUN exitObject.
    &ENDIF
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_print
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_print wWin
ON CHOOSE OF Btn_print IN FRAME fMain /* Imprimir */
DO:
    DEFINE VAR Listado AS CHARACTER FORM "X(35)" INITIAL "".
    ASSIGN Listado   = W_Pathspl + "ComentInst-" + W_Usuario + STRING(RANDOM(2000,10000)) + ".lst"
           /*W_SiMInst = TRUE */.

                                    
    {Incluido/Imprimir.i "Listado"}.  
                               /*
    W_SiMInst = FALSE.           */

  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Agencias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Agencias wWin
ON VALUE-CHANGED OF Cmb_Agencias IN FRAME fMain /* Agencias */
DO:
   
    RUN MuestraInfo.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Canal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Canal wWin
ON VALUE-CHANGED OF Cmb_Canal IN FRAME fMain /* Canal Recepcion */
DO:
   RUN MuestraInfo.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Clase
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Clase wWin
ON VALUE-CHANGED OF Cmb_Clase IN FRAME fMain /* Proceso Misional */
DO:
   RUN MuestraInfo.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Estado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Estado wWin
ON VALUE-CHANGED OF Cmb_Estado IN FRAME fMain /* Estado */
DO:
       
    RUN MuestraInfo.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Tipo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Tipo wWin
ON VALUE-CHANGED OF Cmb_Tipo IN FRAME fMain /* Tipo Solicitud */
DO:
     
     RUN MuestraInfo.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RS-OrdenFecha
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RS-OrdenFecha wWin
ON VALUE-CHANGED OF RS-OrdenFecha IN FRAME fMain
DO:
    ASSIGN RS-OrdenFecha.
    RUN MuestraInfo.
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
             INPUT  'dpqr.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedpqrOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dpqr ).
       RUN repositionObject IN h_dpqr ( 1.00 , 1.00 ) NO-ERROR.
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'adm2/folder.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FolderLabels':U + 'Lista|Detalle|Tipo Solicitud|Canal|Proceso|Estado|Area Responsable|Requerimiento|Usuario' + 'FolderTabWidth0FolderFont5HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_folder ).
       RUN repositionObject IN h_folder ( 9.08 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_folder ( 19.12 , 105.00 ) NO-ERROR.

       /* Links to SmartFolder h_folder. */
       RUN addLink ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_folder ,
             BTN-FecFin:HANDLE IN FRAME fMain , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN constructObject (
             INPUT  'adm2/dynbrowser.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsTip_PQR,Agencia,Canal_Servicio,Fnom_producto,Cod_Proceso,Usu_Resp,Estado,Num_PQR,Fec_Solicitud,Cod_Req,Descrip_PQR,Descrip_RespEnabledFieldsBrowseColumnTypesBrowseColumnItemsBrowseColumnItemPairsBrowseColumnInnerLinesBrowseColumnSortsBrowseColumnMaxCharsBrowseColumnAutoCompletionsBrowseColumnUniqueMatchesBrowseColumnDelimitersScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesdpqrUpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynbrowser ).
       RUN repositionObject IN h_dynbrowser ( 10.15 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_dynbrowser ( 17.50 , 103.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_dynbrowser. */
       RUN addLink ( h_dpqr , 'Data':U , h_dynbrowser ).
       RUN addLink ( h_dynbrowser , 'Update':U , h_dpqr ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dynbrowser ,
             h_folder , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN constructObject (
             INPUT  'vpqr.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable?ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vpqr ).
       RUN repositionObject IN h_vpqr ( 10.15 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 17.92 , 89.57 ) */

       /* Links to SmartDataViewer h_vpqr. */
       RUN addLink ( h_dpqr , 'Data':U , h_vpqr ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_vpqr ,
             h_folder , 'AFTER':U ).
    END. /* Page 2 */

  END CASE.
  /* Select a Startup page. */
  IF currentPage eq 0
  THEN RUN selectPage IN THIS-PROCEDURE ( 1 ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE changePage wWin 
PROCEDURE changePage :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

  ASSIGN viLocal = DYNAMIC-FUNCTION('getCurrentPage').
  
  DO WITH FRAME {&FRAME-NAME}:

    CASE viLocal :
      WHEN 3 THEN DO:   /* Tipo de Solicitud */
          EDITOR-1:SCREEN-VALUE = "".
          RUN Resumen_Tipo_Solicitud.
          EDITOR-1:HIDDEN = TRUE.
          EDITOR-1:INSERT-FILE(vcListado).
      END.
      WHEN 4 THEN DO:   /* Canal */
            EDITOR-1:SCREEN-VALUE = "".
            RUN Resumen_Canal.
            EDITOR-1:HIDDEN = TRUE.
            EDITOR-1:INSERT-FILE(vcListado).
      END.
      WHEN 5 THEN DO:   /* Proceso Misional */
          EDITOR-1:SCREEN-VALUE = "".
          RUN Resumen_Proceso.
          EDITOR-1:HIDDEN = TRUE.
          EDITOR-1:INSERT-FILE(vcListado).
      END.
        WHEN 6 THEN DO:   /* Estado */
              EDITOR-1:SCREEN-VALUE = "".
              RUN Resumen_Estado.
              EDITOR-1:HIDDEN = TRUE.
              EDITOR-1:INSERT-FILE(vcListado).
        END.
        WHEN 7 THEN DO:   /* Area Responsable */
              EDITOR-1:SCREEN-VALUE = "".
              RUN Resumen_AreaRespon.
              EDITOR-1:HIDDEN = TRUE.
              EDITOR-1:INSERT-FILE(vcListado).
        END.
        WHEN 8 THEN DO:   /* Requerimiento */
              EDITOR-1:SCREEN-VALUE = "".
              RUN Resumen_Requerimiento.
              EDITOR-1:HIDDEN = TRUE.
              EDITOR-1:INSERT-FILE(vcListado).
        END.
        WHEN 9 THEN DO:   /* Usuario Responsable */
              EDITOR-1:SCREEN-VALUE = "".
              RUN Resumen_Usuario.
              EDITOR-1:HIDDEN = TRUE.
              EDITOR-1:INSERT-FILE(vcListado).
        END.
    END CASE.
    
    EDITOR-1:HIDDEN = FALSE.

  END.
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
  DISPLAY Cmb_Tipo Cmb_Agencias Cmb_Canal Cmb_Clase Cmb_Estado RS-OrdenFecha 
          W_DiaIni AnoIni W_DiaFin AnoFin EDITOR-1 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE Img_MesF Img_MesI RECT-282 RECT-283 Cmb_Tipo Btn_Done Btn_print 
         Cmb_Agencias Cmb_Canal BTN-Responsable Cmb_Clase Cmb_Estado 
         RS-OrdenFecha BTN-Fecini BTN-FecFin 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
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
DEFINE VARIABLE viCnt AS INTEGER INITIAL 0 NO-UNDO.


    DYNAMIC-FUNCTION('setRebuildOnRepos':U IN h_dpqr,
                     INPUT NO /* LOGICAL */).  

    RUN fetchLast IN h_dpqr.
    ASSIGN viCnt = DYNAMIC-FUNCTION('getLastRowNum':U IN h_dpqr).

    MESSAGE "Se va a generar archivo Excel con " SKIP
        viCnt " registros." SKIP
        "Esto puede tardar unos segundos." SKIP
        "Desea continuar?"
        VIEW-AS ALERT-BOX INFO BUTTONS YES-NO 
        TITLE "Generar a Excel" UPDATE vlgenerar AS LOGICAL.


    DYNAMIC-FUNCTION('setRebuildOnRepos':U IN h_dpqr,
                     INPUT YES /* LOGICAL */).  

    IF vlgenerar THEN 
        RUN transferToExcel IN h_dpqr
        ( INPUT " " /* CHARACTER */, /* "Nit,Agencia " */
          INPUT YES /* LOGICAL */,
          INPUT YES /* LOGICAL */,
          INPUT viCnt /* INTEGER */).
    ELSE
        RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeDataObjects wWin 
PROCEDURE initializeDataObjects :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER plDeep AS LOGICAL NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER( INPUT plDeep).

  DO WITH FRAME fMain:
      FOR EACH Agencias WHERE Agencias.Estado EQ 1 NO-LOCK:
         W_Ok = Cmb_Agencias:ADD-LAST(STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre).
      END.

     FOR EACH Varios WHERE Varios.Tipo = 42 NO-LOCK :  /* Tipo Solicitud */
        W_Ok = Cmb_Tipo:ADD-LAST(STRING(Varios.Codigo,"99999") + " - " + Varios.Descripcion).
     END.
     
     FOR EACH Varios WHERE Varios.Tipo = 43 NO-LOCK :     /* Canal de Recepcion */
        W_Ok = Cmb_Canal:ADD-LAST(STRING(Varios.Codigo,"99999") + " - " + Varios.Descripcion).
     END.
     FOR EACH Varios WHERE Varios.Tipo = 44 NO-LOCK :     /* Proceso Misional -  Clase de Producto  */
        W_Ok = Cmb_Clase:ADD-LAST(STRING(Varios.Codigo,"99999") + " - " + Varios.Descripcion).
     END.

     DO: 
         W_Ok = Cmb_Estado:ADD-LAST(STRING(1,"99999") + " - Por Tramitar").
         W_Ok = Cmb_Estado:ADD-LAST(STRING(2,"99999") + " - Solucionado").
         W_Ok = Cmb_Estado:ADD-LAST(STRING(3,"99999") + " - Anulado").
     END.

     ASSIGN Cmb_Tipo:SCREEN-VALUE = "00000 - Todas las Solicitudes"
            Cmb_Agencias:SCREEN-VALUE = "000 - Todas las Agencias"
            Cmb_Canal:SCREEN-VALUE = "00000 - Todos los Canales"
            Cmb_Clase:SCREEN-VALUE = "00000 - Todos los Procesos"
            Cmb_Estado:SCREEN-VALUE = "00001 - Por Tramitar".

     ASSIGN W_OK = Img_MesI:LOAD-IMAGE("imagenes\month" + STRING(1) + ".gif")
            W_OK = Img_MesF:LOAD-IMAGE("imagenes\month" + STRING(MONTH(TODAY)) + ".gif")
            AnoIni:SCREEN-VALUE = STRING(YEAR(TODAY))
            AnoFin:SCREEN-VALUE = STRING(YEAR(TODAY))
            AnoFin = YEAR(TODAY)
            AnoIni = YEAR(TODAY)
            W_DiaFin:SCREEN-VALUE = STRING(DAY(TODAY))
            W_DiaIni:SCREEN-VALUE = STRING(1)
            W_DiaFin = DAY(TODAY)
            W_DiaIni = 1
            vdaFecIni = DATE(1,1,YEAR(TODAY))
            vdaFecFin = TODAY.
     ASSIGN RS-OrdenFecha = NO.

     RUN MuestraInfo.
  END.

  /* Code placed here will execute AFTER standard behavior.    */

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

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
      EDITOR-1:HIDDEN = FALSE.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE llenaimpresion wWin 
PROCEDURE llenaimpresion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO:
    FIND FIRST Agencia OF pqr NO-LOCK NO-ERROR.
    ASSIGN vcAgencia = STRING(pqr.Agencia,"999") + "-" + (IF AVAILABLE Agencia THEN Agencia.Nombre ELSE "").
    
    FIND FIRST Varios WHERE Varios.tipo = 44 AND /* Proceso Misional */
                            Varios.Codigo = pqr.Cod_Proceso NO-LOCK NO-ERROR.
    ASSIGN vcProceso = STRING(pqr.Cod_Proceso,"99999") + "-" + (IF AVAILABLE Varios THEN Varios.Descripcion ELSE "").
    
    FIND FIRST Varios WHERE Varios.tipo = 45 AND /* Area Responsable */
                          Varios.Codigo = pqr.Area_resp NO-LOCK NO-ERROR.
    ASSIGN vcArea_resp = STRING(pqr.Area_resp,"99999") + "-" + (IF AVAILABLE Varios THEN Varios.Descripcion ELSE "").
   
   FIND FIRST Varios WHERE Varios.tipo = 43 AND /* Canal */
                         Varios.Codigo = pqr.Canal NO-LOCK NO-ERROR.
   ASSIGN vcCanal = STRING(pqr.Canal,"99999") + "-" + (IF AVAILABLE Varios THEN Varios.Descripcion ELSE "").
    
    /* Producto - Captacion */
    IF pqr.Cod_Proceso = 1 THEN DO:
       FIND FIRST Pro_ahorros WHERE Pro_ahorros.Cod_Ahorro = pqr.Cod_producto  NO-LOCK NO-ERROR.
       ASSIGN vcProducto = STRING(pqr.Cod_producto,"999") + "-" + (IF AVAILABLE Pro_ahorros THEN Pro_ahorros.Nom_Producto ELSE "").
    END.
    ELSE /* Producto 2 - Colocacion */
    IF pqr.Cod_Proceso = 2 THEN DO:
       FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito = pqr.Cod_producto NO-LOCK NO-ERROR.
       ASSIGN vcProducto = STRING(pqr.Cod_producto,"999") + "-" + (IF AVAILABLE Pro_Creditos THEN Pro_Creditos.Nom_Producto ELSE "").
    END.
    
    FIND usuarios WHERE Usuarios.Usuario = pqr.Per_Resp NO-LOCK NO-ERROR.
    ASSIGN vcUsuarioResp = pqr.Per_Resp + '-' + (IF AVAILABLE usuarios THEN Usuarios.Nombre ELSE ""). 
    
    FIND FIRST varios WHERE varios.tipo   = 46 AND 
                            varios.codigo = pqr.Cod_Req NO-LOCK NO-ERROR.
    ASSIGN vcRequerimientos = STRING(pqr.Cod_Req,"99999") + "-" + (IF AVAILABLE varios THEN Varios.Descripcion ELSE "").
    
    IF pqr.Estado = 1 THEN
       ASSIGN vcEstado = STRING(1,"999") + "-" +  "Por Tramitar".
    ELSE IF pqr.Estado = 2 THEN
       ASSIGN vcEstado = STRING(2,"999") + "-" +  "Solucionado".
    ELSE IF pqr.Estado = 3 THEN
       ASSIGN vcEstado = STRING(3,"999") + "-" +  "Anulado".

    FIND cliente WHERE cliente.nit = pqr.nit NO-LOCK NO-ERROR.
    ASSIGN vcCliente = pqr.nit + "-" + clientes.Apellido1 + " " + Clientes.Apellido2 + " " + Clientes.Nombre.

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MuestraInfo wWin 
PROCEDURE MuestraInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME fMain:

  IF RS-OrdenFecha THEN DO:   /* Ascendente */ 
      DYNAMIC-FUNCTION('closeQuery':U IN h_dpqr).
      DYNAMIC-FUNCTION('setQueryWhere':U IN h_dpqr," (Tip_PQR = " + SUBSTRING(Cmb_Tipo:SCREEN-VALUE,1,5) + " OR TRUE AND " + SUBSTRING(Cmb_Tipo:SCREEN-VALUE,1,5) + " = '00000' ) " + 
                                                   " AND (Agencia = " + SUBSTRING(Cmb_Agencias:SCREEN-VALUE,1,3) + " OR TRUE AND " + SUBSTRING(Cmb_Agencias:SCREEN-VALUE,1,3) + " = '000' ) " +
                                                   " AND (Canal_Servicio = " + SUBSTRING(Cmb_Canal:SCREEN-VALUE,1,5) + " OR TRUE AND " + SUBSTRING(Cmb_Canal:SCREEN-VALUE,1,5) + " = '00000' ) " +
                                                   " AND (Clase_Producto = " + SUBSTRING(Cmb_Clase:SCREEN-VALUE,1,5) + " OR TRUE AND " + SUBSTRING(Cmb_Clase:SCREEN-VALUE,1,5) + " = '00000' ) " +
                                                   " AND (Estado = " + SUBSTRING(Cmb_Estado:SCREEN-VALUE,1,5) + " OR TRUE AND " + SUBSTRING(Cmb_Estado:SCREEN-VALUE,1,5) + " = '00000' ) "  +
                                                   " AND (Fec_Grabacion >= " + STRING(vdaFecIni) + " AND Fec_grabacion <= " + STRING(vdaFecFin) + " ) " ).
      DYNAMIC-FUNCTION('setQuerySort':U IN h_dpqr,"Fec_Grabacion").
      DYNAMIC-FUNCTION('openQuery':U IN h_dpqr).
  END.
  ELSE DO:      /* Descendete */
      DYNAMIC-FUNCTION('closeQuery':U IN h_dpqr).
      DYNAMIC-FUNCTION('setQueryWhere':U IN h_dpqr," (Tip_PQR = " + SUBSTRING(Cmb_Tipo:SCREEN-VALUE,1,5) + " OR TRUE AND " + SUBSTRING(Cmb_Tipo:SCREEN-VALUE,1,5) + " = '00000' ) " + 
                                                   " AND (Agencia = " + SUBSTRING(Cmb_Agencias:SCREEN-VALUE,1,3) + " OR TRUE AND " + SUBSTRING(Cmb_Agencias:SCREEN-VALUE,1,3) + " = '000' ) " +
                                                   " AND (Canal_Servicio = " + SUBSTRING(Cmb_Canal:SCREEN-VALUE,1,5) + " OR TRUE AND " + SUBSTRING(Cmb_Canal:SCREEN-VALUE,1,5) + " = '00000' ) " +
                                                   " AND (Clase_Producto = " + SUBSTRING(Cmb_Clase:SCREEN-VALUE,1,5) + " OR TRUE AND " + SUBSTRING(Cmb_Clase:SCREEN-VALUE,1,5) + " = '00000' ) " +
                                                   " AND (Estado = " + SUBSTRING(Cmb_Estado:SCREEN-VALUE,1,5) + " OR TRUE AND " + SUBSTRING(Cmb_Estado:SCREEN-VALUE,1,5) + " = '00000' ) "  +
                                                   " AND (Fec_Grabacion >= " + STRING(vdaFecIni) + " AND Fec_grabacion <= " + STRING(vdaFecFin) + " ) " ).
      DYNAMIC-FUNCTION('setQuerySort':U IN h_dpqr,"Fec_Grabacion DESCENDING").
      DYNAMIC-FUNCTION('openQuery':U IN h_dpqr).

  END.

  IF DYNAMIC-FUNCTION('getCurrentPage') >= 3 THEN
     RUN ChangePage.

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir wWin 
PROCEDURE ProcesoImprimir :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{Incluido\RepEncabezado.i}

/*  W_Reporte   = "REPORTE   : CONSULTA DE INFORMACION - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
  W_EncColumna = W_Nombre:SCREEN-VALUE IN FRAME F_Consulta.
  */   

VIEW FRAME F-Encabezado.
VIEW FRAME F-Ftr.

FORM PQR.Num_PQR        COLUMN-LABEL "Numero"
     PQR.Fec_Grabacion  COLUMN-LABEL "Fecha"
     vcAgencia          COLUMN-LABEL "Agencia"          FORMAT "x(30)"
     vcArea_resp        COLUMN-LABEL "Area Responsable" FORMAT "x(20)"
     vcProceso          COLUMN-LABEL "Proceso"          FORMAT "x(20)"
     vcProducto         COLUMN-LABEL "Producto"         FORMAT "x(20)"
     vcCanal            COLUMN-LABEL "Canal"            FORMAT "x(20)"
     vcCliente          COLUMN-LABEL "Cliente"          FORMAT "x(30)"
     vcUsuarioResp      COLUMN-LABEL "Usuario Responsable" FORMAT "x(30)"
     vcRequerimientos   COLUMN-LABEL "Requerimiento"    FORMAT "x(30)"
     vcEstado           COLUMN-LABEL "Estado"           FORMAT "x(20)"
     PQR.Descrip_PQR    FORMAT "x(40)"
     WITH DOWN USE-TEXT FRAME c WIDTH 400 STREAM-IO.
            
    /**/

IF RS-OrdenFecha THEN DO:   /* Ascendente */ 
    FOR EACH pqr WHERE (PQR.Tip_PQR = INT(SUBSTRING(Cmb_Tipo:SCREEN-VALUE IN FRAME fMain,1,5)) OR TRUE AND SUBSTRING(Cmb_Tipo:SCREEN-VALUE,1,5) = '00000' )  
                   AND (PQR.Agencia = INT(SUBSTRING(Cmb_Agencias:SCREEN-VALUE,1,3)) OR TRUE AND SUBSTRING(Cmb_Agencias:SCREEN-VALUE,1,3) = '000' ) 
                   AND (PQR.Canal_Servicio = INT(SUBSTRING(Cmb_Canal:SCREEN-VALUE,1,5)) OR TRUE AND SUBSTRING(Cmb_Canal:SCREEN-VALUE,1,5) = '00000' )
                   AND (PQR.Clase_Producto = INT(SUBSTRING(Cmb_Clase:SCREEN-VALUE,1,5)) OR TRUE AND SUBSTRING(Cmb_Clase:SCREEN-VALUE,1,5) = '00000' )
                   AND (PQR.Estado = INT(SUBSTRING(Cmb_Estado:SCREEN-VALUE,1,5)) OR TRUE AND SUBSTRING(Cmb_Estado:SCREEN-VALUE,1,5) = '00000' ) 
                   AND (PQR.Fec_Grabacion >= vdaFecIni AND PQR.Fec_grabacion <= vdaFecFin) NO-LOCK  BREAK BY Fec_grabacion WITH FRAME c:
       RUN llenaimpresion.
       DISPLAY PQR.Num_PQR        
               PQR.Fec_Grabacion
               vcAgencia          
               vcArea_resp        
               vcProceso          
               vcCanal
               vcCliente
               vcProducto         
               vcUsuarioResp      
               vcRequerimientos   
               vcEstado
               PQR.Descrip_PQR.
       DOWN.
       DO viLocal = 1 TO 7:
          IF LENGTH(PQR.Descrip_PQR) > 40 * viLocal THEN DO:
             DISPLAY SUBSTR(PQR.Descrip_PQR,40 * viLocal) @ PQR.Descrip_PQR.
             DOWN.
          END.
       END.
    END.
END.
ELSE DO:     /* Descendete */
    FOR EACH pqr WHERE (PQR.Tip_PQR = INT(SUBSTRING(Cmb_Tipo:SCREEN-VALUE IN FRAME fMain,1,5)) OR TRUE AND SUBSTRING(Cmb_Tipo:SCREEN-VALUE,1,5) = '00000' )  
                   AND (PQR.Agencia = INT(SUBSTRING(Cmb_Agencias:SCREEN-VALUE,1,3)) OR TRUE AND SUBSTRING(Cmb_Agencias:SCREEN-VALUE,1,3) = '000' ) 
                   AND (PQR.Canal_Servicio = INT(SUBSTRING(Cmb_Canal:SCREEN-VALUE,1,5)) OR TRUE AND SUBSTRING(Cmb_Canal:SCREEN-VALUE,1,5) = '00000' )
                   AND (PQR.Clase_Producto = INT(SUBSTRING(Cmb_Clase:SCREEN-VALUE,1,5)) OR TRUE AND SUBSTRING(Cmb_Clase:SCREEN-VALUE,1,5) = '00000' )
                   AND (PQR.Estado = INT(SUBSTRING(Cmb_Estado:SCREEN-VALUE,1,5)) OR TRUE AND SUBSTRING(Cmb_Estado:SCREEN-VALUE,1,5) = '00000' ) 
                   AND (PQR.Fec_Grabacion >= vdaFecIni AND PQR.Fec_grabacion <= vdaFecFin) NO-LOCK  BREAK BY Fec_grabacion DESCENDING WITH FRAME c:
       RUN llenaimpresion.
       DISPLAY PQR.Num_PQR        
               PQR.Fec_Grabacion
               vcAgencia          
               vcArea_resp        
               vcProceso          
               vcCanal
               vcCliente
               vcProducto         
               vcUsuarioResp      
               vcRequerimientos   
               vcEstado
               PQR.Descrip_PQR.
       DOWN.
       DO viLocal = 1 TO 7:
          IF LENGTH(PQR.Descrip_PQR) > 40 * viLocal THEN DO:
             DISPLAY SUBSTR(PQR.Descrip_PQR,40 * viLocal) @ PQR.Descrip_PQR.
             DOWN.
          END.
       END.
    END.
END.

    PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Resumen_AreaRespon wWin 
PROCEDURE Resumen_AreaRespon :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FIND FIRST agencia WHERE agencia.agencia = INT(SUBSTRING(Cmb_Agencias:SCREEN-VALUE IN FRAME fMain,1,3)) NO-LOCK NO-ERROR.
ASSIGN vcAgencia = (IF INT(SUBSTRING(Cmb_Agencias:SCREEN-VALUE,1,3)) = 0 THEN "Todas las Agencias" ELSE Agencia.nombre).

FORM ttpqr.Area_resp 
     ttpqr.Descripcion
     ttpqr.Recibido
     ttpqr.Contestado
     WITH FRAME FR-Resumen DOWN WIDTH 100 STREAM-IO
     TITLE "Total por Agencia: " + vcAgencia.

ASSIGN vcListado = W_PathSpl + "ImpAhorros-" + W_Usuario + STRING(RANDOM(2000,10000)) +  ".Lst".

FOR EACH ttpqr: DELETE ttpqr. END.

OUTPUT TO VALUE(vcListado).

FOR EACH pqr WHERE (PQR.Tip_PQR = INT(SUBSTRING(Cmb_Tipo:SCREEN-VALUE IN FRAME fMain,1,5)) OR TRUE AND SUBSTRING(Cmb_Tipo:SCREEN-VALUE,1,5) = '00000' )  
                   AND (PQR.Agencia = INT(SUBSTRING(Cmb_Agencias:SCREEN-VALUE,1,3)) OR TRUE AND SUBSTRING(Cmb_Agencias:SCREEN-VALUE,1,3) = '000' ) 
                   AND (PQR.Canal_Servicio = INT(SUBSTRING(Cmb_Canal:SCREEN-VALUE,1,5)) OR TRUE AND SUBSTRING(Cmb_Canal:SCREEN-VALUE,1,5) = '00000' )
                   AND (PQR.Clase_Producto = INT(SUBSTRING(Cmb_Clase:SCREEN-VALUE,1,5)) OR TRUE AND SUBSTRING(Cmb_Clase:SCREEN-VALUE,1,5) = '00000' )
                   AND (PQR.Estado = INT(SUBSTRING(Cmb_Estado:SCREEN-VALUE,1,5)) OR TRUE AND SUBSTRING(Cmb_Estado:SCREEN-VALUE,1,5) = '00000' ) 
                   AND (PQR.Fec_Grabacion >= vdaFecIni AND PQR.Fec_grabacion <= vdaFecFin) NO-LOCK:

    FIND FIRST ttpqr WHERE ttpqr.Area_resp = pqr.Area_resp NO-ERROR.
    IF NOT AVAILABLE ttpqr THEN DO:
        FIND FIRST Varios WHERE Varios.tipo = 45 AND /* Area Responsable */
                                Varios.Codigo = pqr.Area_resp NO-LOCK NO-ERROR.
        CREATE ttpqr.
        ASSIGN ttpqr.Area_resp = pqr.Area_resp
               ttpqr.Descripcion = Varios.Descripcion 
               ttpqr.Recibido = 0.
    END.
    ASSIGN ttpqr.Recibido = ttpqr.Recibido + 1.
    IF PQR.Fec_Fin_PQR <> ? THEN  /* Tiene fecha de Terminacion */
       ASSIGN ttpqr.Contestado = ttpqr.Contestado + 1.
END.

   
FOR EACH ttpqr WITH FRAME FR-Resumen BREAK BY ttpqr.Area_resp:
    DISPLAY ttpqr.Area_resp
            ttpqr.Descripcion
            ttpqr.Recibido
            ttpqr.Contestado.
    DOWN.
    ACCUM ttpqr.Recibido (TOTAL)
          ttpqr.Contestado (TOTAL).
END.
DO WITH FRAME FR-Resumen:
   UNDERLINE ttpqr.Recibido
             ttpqr.Contestado.
   DOWN.
   DISPLAY "Agencia: " + vcAgencia @ ttpqr.Descripcion
           (ACCUM TOTAL ttpqr.Recibido) @ ttpqr.Recibido
           (ACCUM TOTAL ttpqr.Contestado) @ ttpqr.Contestado.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Resumen_Canal wWin 
PROCEDURE Resumen_Canal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FIND FIRST agencia WHERE agencia.agencia = INT(SUBSTRING(Cmb_Agencias:SCREEN-VALUE IN FRAME fMain,1,3)) NO-LOCK NO-ERROR.
ASSIGN vcAgencia = (IF INT(SUBSTRING(Cmb_Agencias:SCREEN-VALUE,1,3)) = 0 THEN "Todas las Agencias" ELSE Agencia.nombre).

FORM ttpqr.Canal 
     ttpqr.Descripcion
     ttpqr.Recibido
     ttpqr.Contestado
     WITH FRAME FR-Resumen DOWN WIDTH 100 STREAM-IO
     TITLE "Total por Agencia: " + vcAgencia.

ASSIGN vcListado = W_PathSpl + "ImpAhorros-" + W_Usuario + STRING(RANDOM(2000,10000)) +  ".Lst".

FOR EACH ttpqr: DELETE ttpqr. END.

OUTPUT TO VALUE(vcListado).

FOR EACH pqr WHERE (PQR.Tip_PQR = INT(SUBSTRING(Cmb_Tipo:SCREEN-VALUE IN FRAME fMain,1,5)) OR TRUE AND SUBSTRING(Cmb_Tipo:SCREEN-VALUE,1,5) = '00000' )  
                   AND (PQR.Agencia = INT(SUBSTRING(Cmb_Agencias:SCREEN-VALUE,1,3)) OR TRUE AND SUBSTRING(Cmb_Agencias:SCREEN-VALUE,1,3) = '000' ) 
                   AND (PQR.Canal_Servicio = INT(SUBSTRING(Cmb_Canal:SCREEN-VALUE,1,5)) OR TRUE AND SUBSTRING(Cmb_Canal:SCREEN-VALUE,1,5) = '00000' )
                   AND (PQR.Clase_Producto = INT(SUBSTRING(Cmb_Clase:SCREEN-VALUE,1,5)) OR TRUE AND SUBSTRING(Cmb_Clase:SCREEN-VALUE,1,5) = '00000' )
                   AND (PQR.Estado = INT(SUBSTRING(Cmb_Estado:SCREEN-VALUE,1,5)) OR TRUE AND SUBSTRING(Cmb_Estado:SCREEN-VALUE,1,5) = '00000' ) 
                   AND (PQR.Fec_Grabacion >= vdaFecIni AND PQR.Fec_grabacion <= vdaFecFin) NO-LOCK:

    FIND FIRST ttpqr WHERE ttpqr.Canal = pqr.Canal NO-ERROR.
    IF NOT AVAILABLE ttpqr THEN DO:
        FIND FIRST Varios WHERE Varios.tipo = 43 AND /* Canal */
                                Varios.Codigo = pqr.Canal NO-LOCK NO-ERROR.
        CREATE ttpqr.
        ASSIGN ttpqr.Canal = pqr.Canal
               ttpqr.Descripcion = Varios.Descripcion 
               ttpqr.Recibido = 0.
    END.
    ASSIGN ttpqr.Recibido = ttpqr.Recibido + 1.
    IF PQR.Fec_Fin_PQR <> ? THEN  /* Tiene fecha de Terminacion */
       ASSIGN ttpqr.Contestado = ttpqr.Contestado + 1.
END.


   
FOR EACH ttpqr WITH FRAME FR-Resumen BREAK BY ttpqr.Canal:
    DISPLAY ttpqr.Canal
            ttpqr.Descripcion
            ttpqr.Recibido
            ttpqr.Contestado.
    DOWN.
    ACCUM ttpqr.Recibido (TOTAL)
          ttpqr.Contestado (TOTAL).
END.
DO WITH FRAME FR-Resumen:
   UNDERLINE ttpqr.Recibido
             ttpqr.Contestado.
   DOWN.
   DISPLAY "Agencia: " + vcAgencia @ ttpqr.Descripcion
           (ACCUM TOTAL ttpqr.Recibido) @ ttpqr.Recibido
           (ACCUM TOTAL ttpqr.Contestado) @ ttpqr.Contestado.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Resumen_Estado wWin 
PROCEDURE Resumen_Estado :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FIND FIRST agencia WHERE agencia.agencia = INT(SUBSTRING(Cmb_Agencias:SCREEN-VALUE IN FRAME fMain,1,3)) NO-LOCK NO-ERROR.
ASSIGN vcAgencia = (IF INT(SUBSTRING(Cmb_Agencias:SCREEN-VALUE,1,3)) = 0 THEN "Todas las Agencias" ELSE Agencia.nombre).

FORM ttpqr.Estado
     ttpqr.Descripcion
     ttpqr.Recibido
     ttpqr.Contestado
     WITH FRAME FR-Resumen DOWN WIDTH 100 STREAM-IO
     TITLE "Total por Agencia: " + vcAgencia.

ASSIGN vcListado = W_PathSpl + "ImpAhorros-" + W_Usuario + STRING(RANDOM(2000,10000)) +  ".Lst".

FOR EACH ttpqr: DELETE ttpqr. END.

OUTPUT TO VALUE(vcListado).

FOR EACH pqr WHERE (PQR.Tip_PQR = INT(SUBSTRING(Cmb_Tipo:SCREEN-VALUE IN FRAME fMain,1,5)) OR TRUE AND SUBSTRING(Cmb_Tipo:SCREEN-VALUE,1,5) = '00000' )  
                   AND (PQR.Agencia = INT(SUBSTRING(Cmb_Agencias:SCREEN-VALUE,1,3)) OR TRUE AND SUBSTRING(Cmb_Agencias:SCREEN-VALUE,1,3) = '000' ) 
                   AND (PQR.Canal_Servicio = INT(SUBSTRING(Cmb_Canal:SCREEN-VALUE,1,5)) OR TRUE AND SUBSTRING(Cmb_Canal:SCREEN-VALUE,1,5) = '00000' )
                   AND (PQR.Clase_Producto = INT(SUBSTRING(Cmb_Clase:SCREEN-VALUE,1,5)) OR TRUE AND SUBSTRING(Cmb_Clase:SCREEN-VALUE,1,5) = '00000' )
                   AND (PQR.Estado = INT(SUBSTRING(Cmb_Estado:SCREEN-VALUE,1,5)) OR TRUE AND SUBSTRING(Cmb_Estado:SCREEN-VALUE,1,5) = '00000' ) 
                   AND (PQR.Fec_Grabacion >= vdaFecIni AND PQR.Fec_grabacion <= vdaFecFin) NO-LOCK:

    FIND FIRST ttpqr WHERE ttpqr.Estado = pqr.Estado NO-ERROR.
    IF NOT AVAILABLE ttpqr THEN DO:
        IF pqr.Estado = 1 THEN
           ASSIGN vcEstado = "Por Tramitar".
        ELSE IF pqr.Estado = 2 THEN
           ASSIGN vcEstado = "Solucionado".
        ELSE IF pqr.Estado = 3 THEN
           ASSIGN vcEstado = "Anulado".
        CREATE ttpqr.
        ASSIGN ttpqr.Estado = pqr.Estado
               ttpqr.Descripcion = vcEstado
               ttpqr.Recibido = 0.
    END.
    ASSIGN ttpqr.Recibido = ttpqr.Recibido + 1.
    IF PQR.Fec_Fin_PQR <> ? THEN  /* Tiene fecha de Terminacion */
       ASSIGN ttpqr.Contestado = ttpqr.Contestado + 1.
END.

FOR EACH ttpqr WITH FRAME FR-Resumen BREAK BY ttpqr.Estado:
    DISPLAY ttpqr.Estado
            ttpqr.Descripcion
            ttpqr.Recibido
            ttpqr.Contestado.
    DOWN.
    ACCUM ttpqr.Recibido (TOTAL)
          ttpqr.Contestado (TOTAL).
END.
DO WITH FRAME FR-Resumen:
   UNDERLINE ttpqr.Recibido
             ttpqr.Contestado.
   DOWN.
   DISPLAY "Agencia: " + vcAgencia @ ttpqr.Descripcion
           (ACCUM TOTAL ttpqr.Recibido) @ ttpqr.Recibido
           (ACCUM TOTAL ttpqr.Contestado) @ ttpqr.Contestado.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Resumen_Proceso wWin 
PROCEDURE Resumen_Proceso :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FIND FIRST agencia WHERE agencia.agencia = INT(SUBSTRING(Cmb_Agencias:SCREEN-VALUE IN FRAME fMain,1,3)) NO-LOCK NO-ERROR.
ASSIGN vcAgencia = (IF INT(SUBSTRING(Cmb_Agencias:SCREEN-VALUE,1,3)) = 0 THEN "Todas las Agencias" ELSE Agencia.nombre).

FORM ttpqr.Cod_Proceso 
     ttpqr.Descripcion
     ttpqr.Recibido
     ttpqr.Contestado
     WITH FRAME FR-Resumen DOWN WIDTH 100 STREAM-IO
     TITLE "Total por Agencia: " + vcAgencia.

ASSIGN vcListado = W_PathSpl + "ImpAhorros-" + W_Usuario + STRING(RANDOM(2000,10000)) +  ".Lst".

FOR EACH ttpqr: DELETE ttpqr. END.

OUTPUT TO VALUE(vcListado).

FOR EACH pqr WHERE (PQR.Tip_PQR = INT(SUBSTRING(Cmb_Tipo:SCREEN-VALUE IN FRAME fMain,1,5)) OR TRUE AND SUBSTRING(Cmb_Tipo:SCREEN-VALUE,1,5) = '00000' )  
                   AND (PQR.Agencia = INT(SUBSTRING(Cmb_Agencias:SCREEN-VALUE,1,3)) OR TRUE AND SUBSTRING(Cmb_Agencias:SCREEN-VALUE,1,3) = '000' ) 
                   AND (PQR.Canal_Servicio = INT(SUBSTRING(Cmb_Canal:SCREEN-VALUE,1,5)) OR TRUE AND SUBSTRING(Cmb_Canal:SCREEN-VALUE,1,5) = '00000' )
                   AND (PQR.Clase_Producto = INT(SUBSTRING(Cmb_Clase:SCREEN-VALUE,1,5)) OR TRUE AND SUBSTRING(Cmb_Clase:SCREEN-VALUE,1,5) = '00000' )
                   AND (PQR.Estado = INT(SUBSTRING(Cmb_Estado:SCREEN-VALUE,1,5)) OR TRUE AND SUBSTRING(Cmb_Estado:SCREEN-VALUE,1,5) = '00000' ) 
                   AND (PQR.Fec_Grabacion >= vdaFecIni AND PQR.Fec_grabacion <= vdaFecFin) NO-LOCK:

    FIND FIRST ttpqr WHERE ttpqr.Cod_Proceso = pqr.Cod_Proceso NO-ERROR.
    IF NOT AVAILABLE ttpqr THEN DO:
        FIND FIRST Varios WHERE Varios.tipo = 44 AND /* Proceso Misional */
                                Varios.Codigo = pqr.Cod_Proceso NO-LOCK NO-ERROR.
        CREATE ttpqr.
        ASSIGN ttpqr.Cod_Proceso = pqr.Cod_Proceso
               ttpqr.Descripcion = Varios.Descripcion 
               ttpqr.Recibido = 0.
    END.
    ASSIGN ttpqr.Recibido = ttpqr.Recibido + 1.
    IF PQR.Fec_Fin_PQR <> ? THEN  /* Tiene fecha de Terminacion */
       ASSIGN ttpqr.Contestado = ttpqr.Contestado + 1.
END.


   
FOR EACH ttpqr WITH FRAME FR-Resumen BREAK BY ttpqr.Cod_Proceso:
    DISPLAY ttpqr.Cod_Proceso
            ttpqr.Descripcion
            ttpqr.Recibido
            ttpqr.Contestado.
    DOWN.
    ACCUM ttpqr.Recibido (TOTAL)
          ttpqr.Contestado (TOTAL).
END.
DO WITH FRAME FR-Resumen:
   UNDERLINE ttpqr.Recibido
             ttpqr.Contestado.
   DOWN.
   DISPLAY "Agencia: " + vcAgencia @ ttpqr.Descripcion
           (ACCUM TOTAL ttpqr.Recibido) @ ttpqr.Recibido
           (ACCUM TOTAL ttpqr.Contestado) @ ttpqr.Contestado.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Resumen_Requerimiento wWin 
PROCEDURE Resumen_Requerimiento :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FIND FIRST agencia WHERE agencia.agencia = INT(SUBSTRING(Cmb_Agencias:SCREEN-VALUE IN FRAME fMain,1,3)) NO-LOCK NO-ERROR.
ASSIGN vcAgencia = (IF INT(SUBSTRING(Cmb_Agencias:SCREEN-VALUE,1,3)) = 0 THEN "Todas las Agencias" ELSE Agencia.nombre).

FORM ttpqr.Cod_Req 
     ttpqr.Descripcion
     ttpqr.Recibido
     ttpqr.Contestado
     WITH FRAME FR-Resumen DOWN WIDTH 100 STREAM-IO
     TITLE "Total por Agencia: " + vcAgencia.

ASSIGN vcListado = W_PathSpl + "ImpAhorros-" + W_Usuario + STRING(RANDOM(2000,10000)) +  ".Lst".

FOR EACH ttpqr: DELETE ttpqr. END.

OUTPUT TO VALUE(vcListado).

FOR EACH pqr WHERE (PQR.Tip_PQR = INT(SUBSTRING(Cmb_Tipo:SCREEN-VALUE IN FRAME fMain,1,5)) OR TRUE AND SUBSTRING(Cmb_Tipo:SCREEN-VALUE,1,5) = '00000' )  
                   AND (PQR.Agencia = INT(SUBSTRING(Cmb_Agencias:SCREEN-VALUE,1,3)) OR TRUE AND SUBSTRING(Cmb_Agencias:SCREEN-VALUE,1,3) = '000' ) 
                   AND (PQR.Canal_Servicio = INT(SUBSTRING(Cmb_Canal:SCREEN-VALUE,1,5)) OR TRUE AND SUBSTRING(Cmb_Canal:SCREEN-VALUE,1,5) = '00000' )
                   AND (PQR.Clase_Producto = INT(SUBSTRING(Cmb_Clase:SCREEN-VALUE,1,5)) OR TRUE AND SUBSTRING(Cmb_Clase:SCREEN-VALUE,1,5) = '00000' )
                   AND (PQR.Estado = INT(SUBSTRING(Cmb_Estado:SCREEN-VALUE,1,5)) OR TRUE AND SUBSTRING(Cmb_Estado:SCREEN-VALUE,1,5) = '00000' ) 
                   AND (PQR.Fec_Grabacion >= vdaFecIni AND PQR.Fec_grabacion <= vdaFecFin) NO-LOCK:

    FIND FIRST ttpqr WHERE ttpqr.Cod_Req = pqr.Cod_Req NO-ERROR.
    IF NOT AVAILABLE ttpqr THEN DO:
        
        FIND FIRST varios WHERE varios.tipo   = 46 AND /* Requerimiento  */
                                varios.codigo = pqr.Cod_Req NO-LOCK NO-ERROR.
        CREATE ttpqr.
        ASSIGN ttpqr.Cod_Req = pqr.Cod_Req
               ttpqr.Descripcion = Varios.Descripcion 
               ttpqr.Recibido = 0.
    END.
    ASSIGN ttpqr.Recibido = ttpqr.Recibido + 1.
    IF PQR.Fec_Fin_PQR <> ? THEN  /* Tiene fecha de Terminacion */
       ASSIGN ttpqr.Contestado = ttpqr.Contestado + 1.
END.


   
FOR EACH ttpqr WITH FRAME FR-Resumen BREAK BY ttpqr.Cod_Req:
    DISPLAY ttpqr.Cod_Req
            ttpqr.Descripcion
            ttpqr.Recibido
            ttpqr.Contestado.
    DOWN.
    ACCUM ttpqr.Recibido (TOTAL)
          ttpqr.Contestado (TOTAL).
END.
DO WITH FRAME FR-Resumen:
   UNDERLINE ttpqr.Recibido
             ttpqr.Contestado.
   DOWN.
   DISPLAY "Agencia: " + vcAgencia @ ttpqr.Descripcion
           (ACCUM TOTAL ttpqr.Recibido) @ ttpqr.Recibido
           (ACCUM TOTAL ttpqr.Contestado) @ ttpqr.Contestado.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Resumen_Tipo_Solicitud wWin 
PROCEDURE Resumen_Tipo_Solicitud :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FIND FIRST agencia WHERE agencia.agencia = INT(SUBSTRING(Cmb_Agencias:SCREEN-VALUE IN FRAME fMain,1,3)) NO-LOCK NO-ERROR.
ASSIGN vcAgencia = (IF INT(SUBSTRING(Cmb_Agencias:SCREEN-VALUE,1,3)) = 0 THEN "Todas las Agencias" ELSE Agencia.nombre).

FORM ttpqr.tip_pqr 
     ttpqr.Descripcion
     ttpqr.Recibido
     ttpqr.Contestado
     WITH FRAME FR-Resumen DOWN WIDTH 100 STREAM-IO
    TITLE "Total por Agencia: " + vcAgencia.

ASSIGN vcListado = W_PathSpl + "ImpAhorros-" + W_Usuario + STRING(RANDOM(2000,10000)) +  ".Lst".

FOR EACH ttpqr: DELETE ttpqr. END.

OUTPUT TO VALUE(vcListado).

FOR EACH pqr WHERE (PQR.Tip_PQR = INT(SUBSTRING(Cmb_Tipo:SCREEN-VALUE IN FRAME fMain,1,5)) OR TRUE AND SUBSTRING(Cmb_Tipo:SCREEN-VALUE,1,5) = '00000' )  
                   AND (PQR.Agencia = INT(SUBSTRING(Cmb_Agencias:SCREEN-VALUE,1,3)) OR TRUE AND SUBSTRING(Cmb_Agencias:SCREEN-VALUE,1,3) = '000' ) 
                   AND (PQR.Canal_Servicio = INT(SUBSTRING(Cmb_Canal:SCREEN-VALUE,1,5)) OR TRUE AND SUBSTRING(Cmb_Canal:SCREEN-VALUE,1,5) = '00000' )
                   AND (PQR.Clase_Producto = INT(SUBSTRING(Cmb_Clase:SCREEN-VALUE,1,5)) OR TRUE AND SUBSTRING(Cmb_Clase:SCREEN-VALUE,1,5) = '00000' )
                   AND (PQR.Estado = INT(SUBSTRING(Cmb_Estado:SCREEN-VALUE,1,5)) OR TRUE AND SUBSTRING(Cmb_Estado:SCREEN-VALUE,1,5) = '00000' ) 
                   AND (PQR.Fec_Grabacion >= vdaFecIni AND PQR.Fec_grabacion <= vdaFecFin) NO-LOCK:

    FIND FIRST ttpqr WHERE ttpqr.tip_pqr = pqr.tip_pqr NO-ERROR.
    IF NOT AVAILABLE ttpqr THEN DO:
        FIND FIRST Varios WHERE Varios.tipo = 42 AND /* Tipo de Solicitud */
                                Varios.Codigo = PQR.Tip_PQR NO-LOCK NO-ERROR.
        CREATE ttpqr.
        ASSIGN ttpqr.tip_pqr = pqr.tip_pqr         
               ttpqr.Descripcion = Varios.Descripcion 
               ttpqr.Recibido = 0.
    END.
    ASSIGN ttpqr.Recibido = ttpqr.Recibido + 1.
    IF PQR.Fec_Fin_PQR <> ? THEN  /* Tiene fecha de Terminacion */
       ASSIGN ttpqr.Contestado = ttpqr.Contestado + 1.
END.

   
FOR EACH ttpqr WITH FRAME FR-Resumen BREAK BY ttpqr.tip_pqr:
    DISPLAY ttpqr.tip_pqr 
            ttpqr.Descripcion
            ttpqr.Recibido
            ttpqr.Contestado.
    DOWN.
    ACCUM ttpqr.Recibido (TOTAL)
          ttpqr.Contestado (TOTAL).
END.
DO WITH FRAME FR-Resumen:
    UNDERLINE ttpqr.Recibido
              ttpqr.Contestado.
    DOWN.
    DISPLAY "Agencia: " + vcAgencia @ ttpqr.Descripcion
            (ACCUM TOTAL ttpqr.Recibido) @ ttpqr.Recibido
            (ACCUM TOTAL ttpqr.Contestado) @ ttpqr.Contestado.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Resumen_Usuario wWin 
PROCEDURE Resumen_Usuario :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FIND FIRST agencia WHERE agencia.agencia = INT(SUBSTRING(Cmb_Agencias:SCREEN-VALUE IN FRAME fMain,1,3)) NO-LOCK NO-ERROR.
ASSIGN vcAgencia = (IF INT(SUBSTRING(Cmb_Agencias:SCREEN-VALUE,1,3)) = 0 THEN "Todas las Agencias" ELSE Agencia.nombre).

FORM ttpqr.Per_Resp 
     ttpqr.Descripcion
     ttpqr.Recibido
     ttpqr.Contestado
     WITH FRAME FR-Resumen DOWN WIDTH 100 STREAM-IO
     TITLE "Total por Agencia: " + vcAgencia.

ASSIGN vcListado = W_PathSpl + "ImpAhorros-" + W_Usuario + STRING(RANDOM(2000,10000)) +  ".Lst".

FOR EACH ttpqr: DELETE ttpqr. END.

OUTPUT TO VALUE(vcListado).

FOR EACH pqr WHERE (PQR.Tip_PQR = INT(SUBSTRING(Cmb_Tipo:SCREEN-VALUE IN FRAME fMain,1,5)) OR TRUE AND SUBSTRING(Cmb_Tipo:SCREEN-VALUE,1,5) = '00000' )  
                   AND (PQR.Agencia = INT(SUBSTRING(Cmb_Agencias:SCREEN-VALUE,1,3)) OR TRUE AND SUBSTRING(Cmb_Agencias:SCREEN-VALUE,1,3) = '000' ) 
                   AND (PQR.Canal_Servicio = INT(SUBSTRING(Cmb_Canal:SCREEN-VALUE,1,5)) OR TRUE AND SUBSTRING(Cmb_Canal:SCREEN-VALUE,1,5) = '00000' )
                   AND (PQR.Clase_Producto = INT(SUBSTRING(Cmb_Clase:SCREEN-VALUE,1,5)) OR TRUE AND SUBSTRING(Cmb_Clase:SCREEN-VALUE,1,5) = '00000' )
                   AND (PQR.Estado = INT(SUBSTRING(Cmb_Estado:SCREEN-VALUE,1,5)) OR TRUE AND SUBSTRING(Cmb_Estado:SCREEN-VALUE,1,5) = '00000' ) 
                   AND (PQR.Fec_Grabacion >= vdaFecIni AND PQR.Fec_grabacion <= vdaFecFin) NO-LOCK:

    FIND FIRST ttpqr WHERE ttpqr.Per_Resp = pqr.Per_Resp NO-ERROR.
    IF NOT AVAILABLE ttpqr THEN DO:
        FIND usuarios WHERE Usuarios.Usuario = pqr.Per_Resp NO-LOCK NO-ERROR.
        CREATE ttpqr.
        ASSIGN ttpqr.Per_Resp = pqr.Per_Resp
               ttpqr.Descripcion = (IF AVAILABLE usuarios THEN Usuarios.Nombre ELSE "")
               ttpqr.Recibido = 0.
    END.
    ASSIGN ttpqr.Recibido = ttpqr.Recibido + 1.
    IF PQR.Fec_Fin_PQR <> ? THEN  /* Tiene fecha de Terminacion */
       ASSIGN ttpqr.Contestado = ttpqr.Contestado + 1.
END.

   
FOR EACH ttpqr WITH FRAME FR-Resumen BREAK BY ttpqr.Per_Resp:
    DISPLAY ttpqr.Per_Resp
            ttpqr.Descripcion
            ttpqr.Recibido
            ttpqr.Contestado.
    DOWN.
    ACCUM ttpqr.Recibido (TOTAL)
          ttpqr.Contestado (TOTAL).
END.
DO WITH FRAME FR-Resumen:
   UNDERLINE ttpqr.Recibido
             ttpqr.Contestado.
   DOWN.
   DISPLAY "Agencia: " + vcAgencia @ ttpqr.Descripcion
           (ACCUM TOTAL ttpqr.Recibido) @ ttpqr.Recibido
           (ACCUM TOTAL ttpqr.Contestado) @ ttpqr.Contestado.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

