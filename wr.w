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

    DEFINE VARIABLE AgeIni   AS INTEGER FORMAT "999" INITIAL 0.
    DEFINE VARIABLE AgeFin   AS INTEGER FORMAT "999" INITIAL 999.

    DEFINE VAR W_Ok AS LOGICAL.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Cmb_Tipo Cmb_Agencias Cmb_Canal Cmb_Clase ~
Cmb_Estado 
&Scoped-Define DISPLAYED-OBJECTS Cmb_Tipo Cmb_Agencias Cmb_Canal Cmb_Clase ~
Cmb_Estado 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_dpqr AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dpqr-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynbrowser AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynbrowser-2 AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE Cmb_Agencias AS CHARACTER FORMAT "X(40)":U 
     LABEL "Agencias" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     LIST-ITEMS "000 - Todas las Agencias" 
     DROP-DOWN-LIST
     SIZE 58 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_Canal AS CHARACTER FORMAT "X(40)":U 
     LABEL "Canal Recepcion" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     LIST-ITEMS "00000 - Todos los Canales" 
     DROP-DOWN-LIST
     SIZE 58 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_Clase AS CHARACTER FORMAT "X(40)":U 
     LABEL "Proceso Misional" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     LIST-ITEMS "00000 - Todos los Procesos" 
     DROP-DOWN-LIST
     SIZE 58 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_Estado AS CHARACTER FORMAT "X(40)":U 
     LABEL "Estado" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     LIST-ITEMS "00000 - Todos los Estados" 
     DROP-DOWN-LIST
     SIZE 58 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_Tipo AS CHARACTER FORMAT "X(40)":U 
     LABEL "Tipo Solicitud" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     LIST-ITEMS "00000 - Todas las Solicitudes" 
     DROP-DOWN-LIST
     SIZE 58 BY 1
     BGCOLOR 15  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     Cmb_Tipo AT ROW 1.54 COL 22 COLON-ALIGNED WIDGET-ID 118
     Cmb_Agencias AT ROW 2.62 COL 22 COLON-ALIGNED WIDGET-ID 116
     Cmb_Canal AT ROW 3.69 COL 22 COLON-ALIGNED WIDGET-ID 120
     Cmb_Clase AT ROW 4.69 COL 22 COLON-ALIGNED WIDGET-ID 122
     Cmb_Estado AT ROW 4.69 COL 22 COLON-ALIGNED WIDGET-ID 124
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 99.29 BY 20.04 WIDGET-ID 100.

DEFINE FRAME FRAME-A
     Cmb_Tipo AT ROW 1 COL 11.57 COLON-ALIGNED
          LABEL "Tipo Solicitud" FORMAT "X(40)":U
          VIEW-AS COMBO-BOX INNER-LINES 15
          LIST-ITEMS "00000 - Todas las Solicitudes" 
          DROP-DOWN-LIST
          SIZE 58 BY 1
          BGCOLOR 15 
     Cmb_Agencias AT ROW 1 COL 40.29 COLON-ALIGNED
          LABEL "Agencias" FORMAT "X(40)":U
          VIEW-AS COMBO-BOX INNER-LINES 15
          LIST-ITEMS "000 - Todas las Agencias" 
          DROP-DOWN-LIST
          SIZE 58 BY 1
          BGCOLOR 15 
     Cmb_Canal AT ROW 1 COL 40.29 COLON-ALIGNED
          LABEL "Canal Recepcion" FORMAT "X(40)":U
          VIEW-AS COMBO-BOX INNER-LINES 15
          LIST-ITEMS "00000 - Todos los Canales" 
          DROP-DOWN-LIST
          SIZE 58 BY 1
          BGCOLOR 15 
     Cmb_Clase AT ROW 1 COL 40.29 COLON-ALIGNED
          LABEL "Proceso Misional" FORMAT "X(40)":U
          VIEW-AS COMBO-BOX INNER-LINES 15
          LIST-ITEMS "00000 - Todos los Procesos" 
          DROP-DOWN-LIST
          SIZE 58 BY 1
          BGCOLOR 15 
     Cmb_Estado AT ROW 5.85 COL 22 COLON-ALIGNED WIDGET-ID 124
          LABEL "Estado" FORMAT "X(40)":U
          VIEW-AS COMBO-BOX INNER-LINES 15
          LIST-ITEMS "00000 - Todos los Estados" 
          DROP-DOWN-LIST
          SIZE 58 BY 1
          BGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 99.29 BY 20.04 WIDGET-ID 200.


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
         TITLE              = "<insert SmartWindow title>"
         HEIGHT             = 20.88
         WIDTH              = 113.43
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.14
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.14
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
/* REPARENT FRAME */
ASSIGN FRAME FRAME-A:FRAME = FRAME fMain:HANDLE.

/* SETTINGS FOR FRAME fMain
                                                                        */
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* <insert SmartWindow title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* <insert SmartWindow title> */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Agencias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Agencias wWin
ON VALUE-CHANGED OF Cmb_Agencias IN FRAME FRAME-A /* Agencias */
DO:
   
    RUN MuestraInfo.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME Cmb_Agencias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Agencias wWin
ON VALUE-CHANGED OF Cmb_Agencias IN FRAME fMain /* Agencias */
DO:
   
    RUN MuestraInfo.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-A
&Scoped-define SELF-NAME Cmb_Canal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Canal wWin
ON VALUE-CHANGED OF Cmb_Canal IN FRAME FRAME-A /* Canal Recepcion */
DO:
   RUN MuestraInfo.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME Cmb_Canal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Canal wWin
ON VALUE-CHANGED OF Cmb_Canal IN FRAME fMain /* Canal Recepcion */
DO:
   RUN MuestraInfo.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-A
&Scoped-define SELF-NAME Cmb_Clase
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Clase wWin
ON VALUE-CHANGED OF Cmb_Clase IN FRAME FRAME-A /* Proceso Misional */
DO:
   RUN MuestraInfo.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME Cmb_Clase
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Clase wWin
ON VALUE-CHANGED OF Cmb_Clase IN FRAME fMain /* Proceso Misional */
DO:
   RUN MuestraInfo.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-A
&Scoped-define SELF-NAME Cmb_Estado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Estado wWin
ON VALUE-CHANGED OF Cmb_Estado IN FRAME FRAME-A /* Estado */
DO:
   RUN MuestraInfo.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Tipo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Tipo wWin
ON VALUE-CHANGED OF Cmb_Tipo IN FRAME FRAME-A /* Tipo Solicitud */
DO:
     
     RUN MuestraInfo.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME Cmb_Tipo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Tipo wWin
ON VALUE-CHANGED OF Cmb_Tipo IN FRAME fMain /* Tipo Solicitud */
DO:
     
     RUN MuestraInfo.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-A
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
             INPUT  'adm2/dynbrowser.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsAgencia,Num_PQR,Tip_PQR,Estado,Usu_Resp,Fec_Solicitud,Cod_Req,Fnom_producto,Cod_ProcesoEnabledFieldsBrowseColumnTypesBrowseColumnItemsBrowseColumnItemPairsBrowseColumnInnerLinesBrowseColumnSortsBrowseColumnMaxCharsBrowseColumnAutoCompletionsBrowseColumnUniqueMatchesBrowseColumnDelimitersScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesdpqrUpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynbrowser ).
       RUN repositionObject IN h_dynbrowser ( 8.27 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_dynbrowser ( 11.31 , 99.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dpqr.wDB-AWARE':U ,
             INPUT  FRAME FRAME-A:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedpqrOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dpqr-2 ).
       RUN repositionObject IN h_dpqr-2 ( 1.00 , 1.00 ) NO-ERROR.
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'adm2/dynbrowser.w':U ,
             INPUT  FRAME FRAME-A:HANDLE ,
             INPUT  'DisplayedFieldsAgencia,Num_PQR,Tip_PQR,Estado,Usu_Resp,Fec_Solicitud,Cod_Req,Fnom_producto,Cod_ProcesoEnabledFieldsBrowseColumnTypesBrowseColumnItemsBrowseColumnItemPairsBrowseColumnInnerLinesBrowseColumnSortsBrowseColumnMaxCharsBrowseColumnAutoCompletionsBrowseColumnUniqueMatchesBrowseColumnDelimitersScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesdpqrUpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynbrowser-2 ).
       RUN repositionObject IN h_dynbrowser-2 ( 8.27 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_dynbrowser-2 ( 11.31 , 99.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_dynbrowser. */
       RUN addLink ( h_dpqr , 'Data':U , h_dynbrowser ).
       RUN addLink ( h_dynbrowser , 'Update':U , h_dpqr ).

       /* Links to SmartDataBrowser h_dynbrowser-2. */
       RUN addLink ( h_dpqr-2 , 'Data':U , h_dynbrowser-2 ).
       RUN addLink ( h_dynbrowser-2 , 'Update':U , h_dpqr-2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dynbrowser-2 ,
             Cmb_Estado:HANDLE IN FRAME FRAME-A , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynbrowser ,
             Cmb_Estado:HANDLE IN FRAME fMain , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

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
  DISPLAY Cmb_Tipo Cmb_Agencias Cmb_Canal Cmb_Clase Cmb_Estado 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE Cmb_Tipo Cmb_Agencias Cmb_Canal Cmb_Clase Cmb_Estado 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  DISPLAY Cmb_Tipo Cmb_Agencias Cmb_Canal Cmb_Clase Cmb_Estado 
      WITH FRAME FRAME-A IN WINDOW wWin.
  ENABLE Cmb_Tipo Cmb_Agencias Cmb_Canal Cmb_Clase Cmb_Estado 
      WITH FRAME FRAME-A IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
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



     ASSIGN Cmb_Tipo:SCREEN-VALUE = "00000 - Todas las Solicitudes"
            Cmb_Agencias:SCREEN-VALUE = "000 - Todas las Agencias"
            Cmb_Canal:SCREEN-VALUE = "00000 - Todos los Canales"
            Cmb_Clase:SCREEN-VALUE = "00000 - Todos los Procesos"
         .

  END.

  /* Code placed here will execute AFTER standard behavior.    */

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
  
  DYNAMIC-FUNCTION('closeQuery':U IN h_dpqr).
  DYNAMIC-FUNCTION('setQueryWhere':U IN h_dpqr," (Tip_PQR = " + SUBST(Cmb_Tipo:SCREEN-VALUE,1,5) + " OR " + SUBST(Cmb_Tipo:SCREEN-VALUE,1,5) + " = '00000' ) " + 
                                               " AND (Agencia = " + SUBST(Cmb_Agencias:SCREEN-VALUE,1,3) + " OR " + SUBST(Cmb_Agencias:SCREEN-VALUE,1,3) + " = '000' ) " +
                                               " AND (Canal_Servicio = " + SUBST(Cmb_Canal:SCREEN-VALUE,1,5) + " OR " + SUBST(Cmb_Canal:SCREEN-VALUE,1,5) + " = '00000' ) " +
                                               " AND (Clase_Producto = " + SUBST(Cmb_Clase:SCREEN-VALUE,1,5) + " OR " + SUBST(Cmb_Clase:SCREEN-VALUE,1,5) + " = '00000' ) " +
                                               " AND (Estado = " + SUBST(Cmb_Estado:SCREEN-VALUE,1,5) + " OR " + SUBST(Cmb_Estado:SCREEN-VALUE,1,5) + " = '00000' ) " ).
  DYNAMIC-FUNCTION('openQuery':U IN h_dpqr).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

