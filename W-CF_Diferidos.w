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
{incluido\Variable.i "SHARED"}  /*   contenedor de variables Globales*/
/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-54 Btn_Impresion Btn_Consulta ~
Btn_Ayuda RECT-175 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-cf_diferidos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_d-cf_diferidos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updsav91 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-cf_diferidos AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Ayuda 
     IMAGE-UP FILE "imagenes\interrogacion":U
     LABEL "" 
     SIZE 4 BY 1.08 TOOLTIP "Ayuda".

DEFINE BUTTON Btn_Consulta 
     IMAGE-UP FILE "imagenes\lupa":U
     LABEL "Co&nsultar" 
     SIZE 11 BY 1.62 TOOLTIP "Busqueda de información de la pantalla en uso"
     FONT 4.

DEFINE BUTTON Btn_Impresion 
     IMAGE-UP FILE "imagenes\impresora2":U
     LABEL "I&mprimir" 
     SIZE 11 BY 1.62 TOOLTIP "Entra a pantalla de opciones de reporte"
     FONT 15.

DEFINE BUTTON BUTTON-54 
     IMAGE-UP FILE "imagenes\informacion":U
     LABEL "Button 54" 
     SIZE 11 BY 1.38 TOOLTIP "Muestra la pantalla de información de sesión y los mensajes del usuario activo".

DEFINE RECTANGLE RECT-175
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 13 BY 5.12.

DEFINE BUTTON Btn_SalidaConsulta 
     IMAGE-UP FILE "imagenes\volver":U
     LABEL "Terminar Consulta" 
     SIZE 7 BY 1.62 TOOLTIP "Cierra esta ventana y vuelve a la pantalla anterior".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BUTTON-54 AT ROW 2.77 COL 101
     Btn_Impresion AT ROW 4.12 COL 101 HELP
          "Permite Generar la Impresion de Programas"
     Btn_Consulta AT ROW 5.73 COL 101 HELP
          "Permite Generar la Consulta de Programas"
     Btn_Ayuda AT ROW 19.58 COL 105 HELP
          "Permite Obtener la ayuda de esta Pantalla"
     RECT-175 AT ROW 2.5 COL 100
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.86 BY 21
         BGCOLOR 17 FONT 5.

DEFINE FRAME frm_consulta
     Btn_SalidaConsulta AT ROW 11.12 COL 24.86 HELP
          "Permite Regresar a la Captura de Información"
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 66 ROW 1.81
         SIZE 32 BY 12.65
         BGCOLOR 17 
         TITLE "Cuentas de Diferidos".


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
         TITLE              = "Configuración de cuentas Para manejo de Diferidos"
         HEIGHT             = 22.08
         WIDTH              = 113.29
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
ASSIGN FRAME frm_consulta:FRAME = FRAME F-Main:HANDLE.

/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE                                                          */
/* SETTINGS FOR FRAME frm_consulta
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME frm_consulta:HIDDEN           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Configuración de cuentas Para manejo de Diferidos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Configuración de cuentas Para manejo de Diferidos */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Ayuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ayuda wWin
ON CHOOSE OF Btn_Ayuda IN FRAME F-Main
OR HELP OF {&WINDOW-NAME} DO:
  SYSTEM-HELP "AYUDAS\CONTABIL" CONTEXT 40.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Consulta wWin
ON CHOOSE OF Btn_Consulta IN FRAME F-Main /* Consultar */
DO:
  VIEW FRAME Frm_Consulta.
  ENABLE ALL WITH FRAME Frm_Consulta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Impresion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Impresion wWin
ON CHOOSE OF Btn_Impresion IN FRAME F-Main /* Imprimir */
DO:
DEFINE VAR listado AS CHARACTER INITIAL "".
listado= W_PathSpl + "Estaciones.LST".
{Incluido\Imprimir.I "Listado"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frm_consulta
&Scoped-define SELF-NAME Btn_SalidaConsulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_SalidaConsulta wWin
ON CHOOSE OF Btn_SalidaConsulta IN FRAME frm_consulta /* Terminar Consulta */
DO:
  HIDE FRAME Frm_Consulta.
  FRAME F-Main:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME BUTTON-54
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-54 wWin
ON CHOOSE OF BUTTON-54 IN FRAME F-Main /* Button 54 */
DO:
  RUN W-InfDia.r NO-ERROR.
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
             INPUT  'd-cf_diferidos.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamed-cf_diferidosUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_d-cf_diferidos ).
       RUN repositionObject IN h_d-cf_diferidos ( 1.54 , 4.00 ) NO-ERROR.
       /* Size in AB:  ( 1.08 , 4.00 ) */

       RUN constructObject (
             INPUT  'b-cf_diferidos.w':U ,
             INPUT  FRAME frm_consulta:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_b-cf_diferidos ).
       RUN repositionObject IN h_b-cf_diferidos ( 1.00 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_b-cf_diferidos ( 9.96 , 31.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'v-cf_diferidos.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_v-cf_diferidos ).
       RUN repositionObject IN h_v-cf_diferidos ( 3.15 , 7.00 ) NO-ERROR.
       /* Size in AB:  ( 16.88 , 81.00 ) */

       RUN constructObject (
             INPUT  'p-updsav91.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AddFunctionOne-RecordEdgePixels2PanelTypeSaveDeactivateTargetOnHidenoDisabledActionsHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_p-updsav91 ).
       RUN repositionObject IN h_p-updsav91 ( 9.50 , 100.00 ) NO-ERROR.
       RUN resizeObject IN h_p-updsav91 ( 9.69 , 13.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_b-cf_diferidos. */
       RUN addLink ( h_d-cf_diferidos , 'Data':U , h_b-cf_diferidos ).

       /* Links to SmartDataViewer h_v-cf_diferidos. */
       RUN addLink ( h_d-cf_diferidos , 'Data':U , h_v-cf_diferidos ).
       RUN addLink ( h_v-cf_diferidos , 'Update':U , h_d-cf_diferidos ).
       RUN addLink ( h_p-updsav91 , 'TableIO':U , h_v-cf_diferidos ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_b-cf_diferidos ,
             Btn_SalidaConsulta:HANDLE IN FRAME frm_consulta , 'BEFORE':U ).
       RUN adjustTabOrder ( h_v-cf_diferidos ,
             BUTTON-54:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjustTabOrder ( h_p-updsav91 ,
             Btn_Consulta:HANDLE IN FRAME F-Main , 'AFTER':U ).
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
  ENABLE BUTTON-54 Btn_Impresion Btn_Consulta Btn_Ayuda RECT-175 
      WITH FRAME F-Main IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  ENABLE Btn_SalidaConsulta 
      WITH FRAME frm_consulta IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-frm_consulta}
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
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 {Incluido\Def_Excel.i}
          E_NumFila = 1.
          E_NumColumn = 5.
          E_Fila      = "012" + "Cod_Producto"
                      + "012" + "PlaZo_Maximo"
                      + "015" + "Costo_Minimo"
                      + "014" + "Cta_Fuente"
                      + "016" + "Gasto_Diferido".
          RUN W-GraExcel.w (INPUT E_Fila, INPUT E_NumColumn, OUTPUT E_CmpGrafic).

          /* launch Excel so it is visible to the user */
          chExcelApp:Visible = TRUE.

          /* create a new Workbook */
          chWorkbook = chExcelApp:Workbooks:Add().

          /* get the active Worksheet */
          chWorkSheet = chExcelApp:Sheets:Item(1).

          FOR EACH Pro_Diferido NO-LOCK:
             E_Fila2     = "".
             E_Fila2     = "012" + STRING(Pro_Diferido.Cod_Producto,"999")
                         + "012" + STRING(Pro_Diferido.Dif_PlaMax,"9999")
                         + "015" + STRING(Pro_Diferido.Dif_CostoMin,">>>,>>>,>>>,>>9")
                         + "014" + STRING(Pro_Diferido.Dif_CtaFuente,"X(14)")
                         + "016" + STRING(Pro_Diferido.Dif_CtaGtoDif,"X(14)").
         
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
  HIDE FRAME frm-diferidos.
  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PorcesoImprimir wWin 
PROCEDURE PorcesoImprimir :
/*------------------------------------------------------------------------------
  Purpose:     Imprime La configuracion de las cuentas de diferidos
  Parameters:  Ninguno
  Notes:       
------------------------------------------------------------------------------*/
{INCLUIDO\RepEncabezado.I}    
  W_Reporte    = "REPORTE   : CUENTAS DE DIFERIDOS - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
  W_EncColumna = "Cod.Diferido    Plazo      Costo Min.      Cuenta. Fuente   Cuenta Gto.Difer." .
  DEFINE FRAME F-Detalle
     Pro_diferidos.Cod_producto   AT  3  FORMAT "999"
     Pro_diferidos.Dif_PlaMax     AT 19  FORMAT "9999"
     Pro_diferidos.Dif_CostoMin   AT 28  FORMAT ">,>>>,>>>,>>9.99"
     Pro_Diferidos.Dif_CtaFuente  AT 46  FORMAT "X(14)"
     Pro_Diferidos.Dif_CtaGtoDif  AT 62  FORMAT "X(14)"
  WITH WIDTH 132 USE-TEXT NO-BOX STREAM-IO NO-LABELS FRAME F-Detalle.
  
  VIEW FRAME F-Encabezado.
  VIEW FRAME F-Ftr.
  FOR EACH pro_diferidos BREAK BY cod_producto:
        DISPLAY  
              Pro_diferidos.Cod_Producto
              Pro_Diferidos.Dif_PlaMax
              Pro_diferidos.Dif_CostoMin
              Pro_Diferidos.Dif_CtaFuente
              Pro_Diferidos.Dif_CtaGtoDif      
        WITH FRAME F-Detalle WIDTH 132 NO-BOX.
  END.  
  PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

