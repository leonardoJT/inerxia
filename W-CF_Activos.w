&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME W-progra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-progra 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

  DEFINE SHARED VAR W_Nom_Entidad AS CHAR FORMAT "X(60)".
  DEFINE SHARED VAR W_Nom_agencia AS CHAR FORMAT "X(60)".
  DEFINE SHARED VAR W_PathSpl LIKE Entidad.Dir_Spl.
  DEFINE SHARED VAR W_Manija AS HANDLE.

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
&Scoped-Define ENABLED-OBJECTS BUTTON-54 Btn_Impresion Btn_Consulta ~
Btn_Ayuda 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-progra AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-cf_activos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_P-UPDSAV AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-cf_activos AS HANDLE NO-UNDO.

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

DEFINE BUTTON Btn_Terminar-2 
     IMAGE-UP FILE "imagenes\volver":U
     LABEL "Terminar Consulta" 
     SIZE 7 BY 1.62 TOOLTIP "Cierra esta ventana y vuelve a la pantalla anterior".

DEFINE BUTTON Btn_CnclImp 
     IMAGE-UP FILE "imagenes\volver":U
     LABEL "Cancelar" 
     SIZE 9 BY 1.62 TOOLTIP "Cierra esta ventana y vuelve a la pantalla anterior".

DEFINE BUTTON Btn_ContinuarImp 
     IMAGE-UP FILE "imagenes\impresora2":U
     LABEL "Continuar" 
     SIZE 9 BY 1.62 TOOLTIP "Muestra interface de salida de información (Reportes)".

DEFINE VARIABLE W_OpcImp AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Cuentas de Depreciación", 1,
"Otras cuentas", 2
     SIZE 25 BY 1.62
     FONT 5 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BUTTON-54 AT ROW 1.81 COL 101
     Btn_Impresion AT ROW 5.85 COL 101 HELP
          "Permite Generar la Impresion de Programas"
     Btn_Consulta AT ROW 7.46 COL 101 HELP
          "Permite Generar la Consulta de Programas"
     Btn_Ayuda AT ROW 20.38 COL 105 HELP
          "Permite Obtener la ayuda de esta Pantalla"
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.29 ROW 1.08
         SIZE 113.43 BY 21.15
         BGCOLOR 17 .

DEFINE FRAME Frm_consulta
     Btn_Terminar-2 AT ROW 15.27 COL 27 HELP
          "Permite Regresar a la Captura de Información"
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 78.86 ROW 3.27
         SIZE 34.29 BY 16.92
         BGCOLOR 17 FONT 4
         TITLE "Consulta Grupo de Activos".

DEFINE FRAME Frm_Imprimir
     Btn_ContinuarImp AT ROW 1.27 COL 31
     W_OpcImp AT ROW 2.08 COL 3 NO-LABEL
     Btn_CnclImp AT ROW 3.15 COL 31
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 71.29 ROW 6
         SIZE 40.86 BY 4.81
         BGCOLOR 17 FONT 4
         TITLE "Parámetros de Impresión".


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
  CREATE WINDOW W-progra ASSIGN
         HIDDEN             = YES
         TITLE              = "SFG - Configuración de Cuentas de Activos"
         HEIGHT             = 21.12
         WIDTH              = 113.72
         MAX-HEIGHT         = 21.23
         MAX-WIDTH          = 113.72
         VIRTUAL-HEIGHT     = 21.23
         VIRTUAL-WIDTH      = 113.72
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-progra 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-progra
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME                                               */
/* SETTINGS FOR FRAME Frm_consulta
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME Frm_consulta:HIDDEN           = TRUE
       FRAME Frm_consulta:MOVABLE          = TRUE.

/* SETTINGS FOR FRAME Frm_Imprimir
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME Frm_Imprimir:HIDDEN           = TRUE
       FRAME Frm_Imprimir:MOVABLE          = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-progra)
THEN W-progra:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-progra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-progra W-progra
ON END-ERROR OF W-progra /* SFG - Configuración de Cuentas de Activos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-progra W-progra
ON WINDOW-CLOSE OF W-progra /* SFG - Configuración de Cuentas de Activos */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Ayuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ayuda W-progra
ON CHOOSE OF Btn_Ayuda IN FRAME F-Main
OR HELP OF {&WINDOW-NAME} DO:
  SYSTEM-HELP "AYUDAS\CONTABIL" CONTEXT 40.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Frm_Imprimir
&Scoped-define SELF-NAME Btn_CnclImp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_CnclImp W-progra
ON CHOOSE OF Btn_CnclImp IN FRAME Frm_Imprimir /* Cancelar */
DO:
  HIDE FRAME Frm_Imprimir.
  FRAME F-Main:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME Btn_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Consulta W-progra
ON CHOOSE OF Btn_Consulta IN FRAME F-Main /* Consultar */
DO:
  FRAME F-Main:SENSITIVE = FALSE.
  VIEW FRAME Frm_Consulta.
  ENABLE ALL WITH FRAME Frm_Consulta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Frm_Imprimir
&Scoped-define SELF-NAME Btn_ContinuarImp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_ContinuarImp W-progra
ON CHOOSE OF Btn_ContinuarImp IN FRAME Frm_Imprimir /* Continuar */
DO:
  DEFINE VAR Listado AS CHARACTER INITIAL "".
  DEFINE VAR W_Tamano AS INTEGER FORMAT "99" INITIAL 2.
  
  Listado = W_PathSpl + "L-cfgact.LST".
  {Incluido\Imprimir.I "Listado" W_Tamano}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME Btn_Impresion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Impresion W-progra
ON CHOOSE OF Btn_Impresion IN FRAME F-Main /* Imprimir */
DO:
  FRAME F-Main:SENSITIVE = FALSE.
  VIEW FRAME Frm_Imprimir.
  ENABLE ALL WITH FRAME Frm_Imprimir.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Frm_consulta
&Scoped-define SELF-NAME Btn_Terminar-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Terminar-2 W-progra
ON CHOOSE OF Btn_Terminar-2 IN FRAME Frm_consulta /* Terminar Consulta */
DO:
  HIDE FRAME Frm_Consulta.
  FRAME F-Main:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME BUTTON-54
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-54 W-progra
ON CHOOSE OF BUTTON-54 IN FRAME F-Main /* Button 54 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Frm_Imprimir
&Scoped-define SELF-NAME W_OpcImp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_OpcImp W-progra
ON MOUSE-SELECT-CLICK OF W_OpcImp IN FRAME Frm_Imprimir
DO:
  APPLY "VALUE-CHANGED" TO W_OpcImp.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_OpcImp W-progra
ON VALUE-CHANGED OF W_OpcImp IN FRAME Frm_Imprimir
OR RETURN OF W_OpcImp DO:
 ASSIGN W_OpcImp.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-progra 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-progra  _ADM-CREATE-OBJECTS
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
             INPUT  'b-cf_activos.w':U ,
             INPUT  FRAME Frm_consulta:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-cf_activos ).
       RUN set-position IN h_b-cf_activos ( 1.27 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-cf_activos ( 13.92 , 32.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'v-cf_activos.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-cf_activos ).
       RUN set-position IN h_v-cf_activos ( 1.81 , 5.00 ) NO-ERROR.
       /* Size in UIB:  ( 20.00 , 94.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'P-UPDSAV.W':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Save,
                     AddFunction = One-Record':U ,
             OUTPUT h_P-UPDSAV ).
       RUN set-position IN h_P-UPDSAV ( 9.35 , 100.00 ) NO-ERROR.
       RUN set-size IN h_P-UPDSAV ( 10.50 , 13.00 ) NO-ERROR.

       /* Links to SmartViewer h_v-cf_activos. */
       RUN add-link IN adm-broker-hdl ( h_b-cf_activos , 'Record':U , h_v-cf_activos ).
       RUN add-link IN adm-broker-hdl ( h_P-UPDSAV , 'TableIO':U , h_v-cf_activos ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-cf_activos ,
             Btn_Terminar-2:HANDLE IN FRAME Frm_consulta , 'BEFORE':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-cf_activos ,
             BUTTON-54:HANDLE IN FRAME F-Main , 'BEFORE':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_P-UPDSAV ,
             Btn_Consulta:HANDLE IN FRAME F-Main , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-progra  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Ctas_Depre_Excel W-progra 
PROCEDURE Ctas_Depre_Excel :
{Incluido\Def_Excel.i}
 /* MANDA ENCABEZADO DE HOJA DE CALCULO*/
 
 E_NumFila = 1.
 E_NumColumn = 12.
 E_Fila      = "004" + "Clas"
             + "005" + "Grupo"
             + "014" + "Fuente        "
             + "014" + "Acumulada     "
             + "014" + "De Gasto      "
             + "014" + "Ajustable     "
             + "014" + "Correccion    "
             + "014" + "De Gasto      "
             + "014" + "Ajustable     "
             + "014" + "Correccion    "
             + "014" + "Ganancia x Vta"
             + "014" + "Perdida x Vta ".
 RUN W-GraExcel.w (INPUT E_Fila, INPUT E_NumColumn, OUTPUT E_CmpGrafic).

/* launch Excel so it is visible to the user */
chExcelApp:Visible = TRUE.

/* create a new Workbook */
chWorkbook = chExcelApp:Workbooks:Add().

/* get the active Worksheet */
chWorkSheet = chExcelApp:Sheets:Item(1).

    FOR EACH cfg_activosFijos NO-LOCK BY cfg_activosFijos.Grupo:
      E_Fila2     = "".
      E_Fila2     = "005" + STRING(cfg_activosFijos.Grupo,"99999")
                  + "014" + STRING(cfg_activosFijos.Cta_Fuente,"X(14)")
                  + "014" + STRING(cfg_activosFijos.CtaDepAcum,"X(14)")
                  + "014" + STRING(cfg_activosFijos.CtaDepMes,"X(14)")
                  + "014" + STRING(cfg_activosFijos.CtaGtoAjuste,"X(14)")
                  + "014" + STRING(cfg_activosFijos.CtaGanancia,"X(14)")
                  + "014" + STRING(cfg_activosFijos.CtaPerdida,"X(14)").
                  
      {Incluido\imprimir_Excel.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-progra  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-progra)
  THEN DELETE WIDGET W-progra.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-progra  _DEFAULT-ENABLE
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
  ENABLE BUTTON-54 Btn_Impresion Btn_Consulta Btn_Ayuda 
      WITH FRAME F-Main IN WINDOW W-progra.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  ENABLE Btn_Terminar-2 
      WITH FRAME Frm_consulta IN WINDOW W-progra.
  {&OPEN-BROWSERS-IN-QUERY-Frm_consulta}
  DISPLAY W_OpcImp 
      WITH FRAME Frm_Imprimir IN WINDOW W-progra.
  ENABLE Btn_ContinuarImp W_OpcImp Btn_CnclImp 
      WITH FRAME Frm_Imprimir IN WINDOW W-progra.
  {&OPEN-BROWSERS-IN-QUERY-Frm_Imprimir}
  VIEW W-progra.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir_Excel W-progra 
PROCEDURE Imprimir_Excel :
IF W_OpcImp = 1 THEN
    RUN Ctas_Depre_Excel.
 ELSE
    RUN Otras_Ctas_Excel.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-progra 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Otras_Ctas_Excel W-progra 
PROCEDURE Otras_Ctas_Excel :
{Incluido\Def_Excel.i}
 /* MANDA ENCABEZADO DE HOJA DE CALCULO*/
 
 E_NumFila = 1.
 E_NumColumn = 12.
 E_Fila      = "004" + "Clas"
             + "005" + "Grupo"
             + "014" + "Débito        "
             + "014" + "Crédito       "
             + "014" + "Débito        "
             + "014" + "Crédito       "
             + "014" + "Débito        "
             + "014" + "Crédito       "
             + "014" + "Valorización  "
             + "014" + "Fuente        "
             + "014" + "Ajuste        "
             + "014" + "Corr.Monetaria".
 RUN W-GraExcel.w (INPUT E_Fila, INPUT E_NumColumn, OUTPUT E_CmpGrafic).

/* launch Excel so it is visible to the user */
chExcelApp:Visible = TRUE.

/* create a new Workbook */
chWorkbook = chExcelApp:Workbooks:Add().

/* get the active Worksheet */
chWorkSheet = chExcelApp:Sheets:Item(1).

                  
      FOR EACH cfg_activosFijos NO-LOCK BY cfg_activosFijos.Grupo:
         E_Fila2     = "".
         E_Fila2     = "005" + STRING(cfg_activosFijos.Grupo,"99999")
                     + "014" + STRING(cfg_activosFijos.CtaDbPig,"X(14)")
                     + "014" + STRING(cfg_activosFijos.CtaCrPig,"X(14)")
                     + "014" + STRING(cfg_activosFijos.CtaDbPre,"X(14)")
                     + "014" + STRING(cfg_activosFijos.CtaCrPre,"X(14)")
                     + "014" + STRING(cfg_activosFijos.CtaValorizacion,"X(14)")
                     + "014" + STRING(cfg_activosFijos.CtaProvision,"X(14)")
                     + "014" + STRING(cfg_activosFijos.CtaGtoProv,"X(14)").
        
        {Incluido\imprimir_Excel.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir W-progra 
PROCEDURE ProcesoImprimir :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR Linea    AS CHAR FORMAT "X(160)" INITIAL "".
  DEFINE VAR Linea1   AS CHAR FORMAT "X(198)" INITIAL "".
  DEFINE VAR Raya     AS CHAR INITIAL "-".
  DEFINE VAR W_Estado AS CHAR FORMAT "X(8)" INITIAL "INACTIVO".
  DEFINE VAR W_NomVar AS CHAR FORMAT "X(25)".
     
  IF W_OpcImp = 1 THEN
    DEFINE FRAME F-Encabezado
      HEADER
        W_Nom_Entidad                AT 44
        "PAGINA:"                    AT 111 PAGE-NUMBER FORMAT ">>>9"
        "Listado de Configuración de Cuentas-Depreciación de Activos por Clase"   AT 27 SKIP(1)
        "Cuentas Depreciación"       AT 20
        "Ajustes por Inflación"      AT 65
        "Aj.Integrales Depreciación" AT 100
        "Grupo"                      AT   1
        "Fuente"                     AT   8
        "Acumulada"                  AT  23
        "de Gasto"                   AT  38
        "Ajustable"                  AT  53
        "Corrección"                 AT  68
        "de Gasto"                   AT  83
        "Ajustable"                  AT  98
        "Corrección"                 AT 113
        "Ganancia.x.Vta"             AT 129
        "Perdida.x.Vta"              AT 144
         Linea                       AT 1 SKIP(1)
    WITH DOWN WIDTH 200 USE-TEXT PAGE-TOP FRAME F-Encabezado STREAM-IO.
  ELSE 
    DEFINE FRAME F-Encabezado2
      HEADER
        W_Nom_Entidad                AT 44
        "PAGINA:"                    AT 111 PAGE-NUMBER FORMAT ">>>9"
        "Listado de Configuración de Otras Cuentas de Activos por Clase"   AT 30 SKIP(1)
        "Ctas de Pignoración"         AT 12
        "Ctas de Préstamo"           AT 42
        "Ctas de Orden Fin Dep."     AT 71
        "Ctas de Orden Ajustes."     AT 103
        "Ctas de Provisión"          AT 152
        "Grupo"                      AT   1
        "Débito"                     AT   8
        "Crédito"                    AT  23
        "Débito"                     AT  38
        "Crédito"                    AT  53
        "Débito"                     AT  68
        "Crédito"                    AT  83
        "Débito"                     AT  98
        "Crédito"                    AT 113
        "Valorización"               AT 128
        "Fuente"                     AT 143
        "Ajuste"                     AT 158
        "Corrección Mon."            AT 173
        Linea1                      AT 1 SKIP(1)
    WITH DOWN WIDTH 200 USE-TEXT PAGE-TOP FRAME F-Encabezado2 STREAM-IO.
          
  DEFINE FRAME f-ftr
    HEADER 
     "FECHA:"       AT 2 
      TODAY         AT 10 FORMAT "99/99/9999"
      W_Nom_agencia AT 28 FORMAT "X(30)"
     "HORA:"        AT 130 STRING(TIME,"HH:MM AM")
  WITH DOWN WIDTH 145 FRAME f-ftr PAGE-BOTTOM USE-TEXT STREAM-IO. 

  ASSIGN Linea  = FILL(Raya,160)
         Linea1 = FILL(Raya,200).
  IF W_OpcImp = 1 THEN
    VIEW FRAME F-Encabezado.
  ELSE
    VIEW FRAME F-Encabezado2.
  VIEW FRAME f-ftr.
  IF W_OpcImp = 1 THEN DO:
    FOR EACH cfg_activosFijos NO-LOCK BREAK BY cfg_activosFijos.Grupo:
       IF FIRST-OF (cfg_activosFijos.Grupo) THEN DO:
         DISPLAY 
           " "                  AT 1 SKIP
           "Grupo: "            AT 2
           cfg_activosFijos.Grupo     AT 9 SKIP(1)
         WITH WIDTH 145 FRAME F-Vari USE-TEXT STREAM-IO NO-BOX NO-LABELS.         
       END.
        DISPLAY
          cfg_activosFijos.Grupo            AT  1
          cfg_activosFijos.Cta_Fuente       AT  8
          cfg_activosFijos.CtaDepAcum       AT 23
          cfg_activosFijos.CtaDepMes        AT 38
          cfg_activosFijos.CtaGtoAjuste     AT 83
          cfg_activosFijos.CtaGanancia      AT 129
          cfg_activosFijos.CtaPerdida       AT 144
      WITH WIDTH 200 FRAME F-Deta USE-TEXT STREAM-IO NO-BOX NO-LABELS.
    END.
  END.
  ELSE DO:
    FOR EACH cfg_activosFijos NO-LOCK BREAK BY cfg_activosFijos.Grupo:
       IF FIRST-OF (cfg_activosFijos.Grupo) THEN DO:
         DISPLAY 
           " "                  AT 1 SKIP
           "Grupo: "            AT 2
           cfg_activosFijos.Grupo     AT 9 SKIP(1)
         WITH WIDTH 145 FRAME F-Vari2 USE-TEXT STREAM-IO NO-BOX NO-LABELS.         
       END.
       DISPLAY
          cfg_activosFijos.Grupo            AT   1
          cfg_activosFijos.CtaDbPig         AT   8
          cfg_activosFijos.CtaCrPig         AT  23
          cfg_activosFijos.CtaDbPre         AT  38
          cfg_activosFijos.CtaCrPre         AT  53
          cfg_activosFijos.CtaValorizacion  AT 128
          cfg_activosFijos.CtaProvision     AT 143
          cfg_activosFijos.CtaGtoProv       AT 173 
       WITH WIDTH 200 FRAME F-Deta2 USE-TEXT STREAM-IO NO-BOX NO-LABELS.
    END.
  END.
  PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-progra  _ADM-SEND-RECORDS
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-progra 
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

