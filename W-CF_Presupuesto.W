&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME W-progra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-progra 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

  {Incluido\VARIABLE.I "SHARED"}
  {Incluido\VARCON.I   "SHARED"}
   
   DEFINE NEW SHARED VARIABLE FCodigo      LIKE Cuentas.Cuenta.
   DEFINE VARIABLE W_naturaleza LIKE cuentas.naturaleza.
   DEFINE VARIABLE W_ctrNat     LIKE cuentas.Ctr_Naturaleza.
   DEFINE VAR W_Ventana    AS CHARACTER.
   DEFINE VAR W_Vble       AS CHAR.
   DEFINE VAR W_PrimeraVez AS LOGICAL INITIAL TRUE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Consulta
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Cuentas

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 Cuentas.Cuenta Cuentas.Nombre Cuentas.Cod_Presupuesto   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2   
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH Cuentas WHERE cuentas.cod_presupuesto EQ fcodigo AND Cuentas.cod_presupuesto NE " " NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH Cuentas WHERE cuentas.cod_presupuesto EQ fcodigo AND Cuentas.cod_presupuesto NE " " NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 Cuentas
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 Cuentas


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_Termina 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-progra AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-cf_presupuesto AS HANDLE NO-UNDO.
DEFINE VARIABLE h_P-UPDSAV AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-cf_presupuesto AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Termina 
     IMAGE-UP FILE "imagenes\volver":U
     LABEL "&Terminar Consulta" 
     SIZE 8 BY 1.62.

DEFINE BUTTON Btn_Ayuda 
     IMAGE-UP FILE "imagenes\interrogacion":U
     LABEL "" 
     SIZE 4 BY 1.08 TOOLTIP "Ayuda"
     FONT 9.

DEFINE BUTTON Btn_Consulta 
     IMAGE-UP FILE "imagenes\lupa":U
     LABEL "Co&nsulta" 
     SIZE 10 BY 1.62 TOOLTIP "Busqueda de información de la pantalla en uso"
     FONT 4.

DEFINE BUTTON Btn_Impresion 
     IMAGE-UP FILE "imagenes\impresora2":U
     LABEL "I&mpresión" 
     SIZE 10 BY 1.62 TOOLTIP "Muestra interface de salida de información (Reportes)"
     FONT 15.

DEFINE BUTTON BUTTON-56 
     IMAGE-UP FILE "imagenes\informacion":U
     LABEL "Button 56" 
     SIZE 10 BY 1.65 TOOLTIP "Muestra la pantalla de información de sesión y mensajes del usuario activo".

DEFINE BUTTON BUTTON-57 
     LABEL "Relacionar con Puc." 
     SIZE 15 BY 1.12
     FONT 4.

DEFINE BUTTON BUTTON-58 
     LABEL "Graba Relación" 
     SIZE 12 BY 1.08 TOOLTIP "Grabar Enlace"
     FONT 4.

DEFINE VARIABLE Fcuenta AS CHARACTER FORMAT "X(14)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE FNombre AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1
     BGCOLOR 15 FONT 4.

DEFINE RECTANGLE RECT-227
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 12 BY 5.38.

DEFINE RECTANGLE RECT-229
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 91 BY 1.62.

DEFINE RECTANGLE RECT-230
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 91 BY 10.23.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      Cuentas SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 W-progra _FREEFORM
  QUERY BROWSE-2 NO-LOCK DISPLAY
      Cuentas.Cuenta FORMAT "X(14)":U
      Cuentas.Nombre FORMAT "X(40)":U
      Cuentas.Cod_Presupuesto FORMAT "X(14)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 91 BY 4.5
         BGCOLOR 15 FONT 4 ROW-HEIGHT-CHARS .54 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Consulta
     Btn_Termina AT ROW 13.38 COL 48 HELP
          "Termina la consulta del plan presupuestal"
     "Nota:" VIEW-AS TEXT
          SIZE 5 BY .81 AT ROW 13.38 COL 4
          FGCOLOR 7 FONT 5
     "Escoja el filtro por el cual desea realizar la busqueda," VIEW-AS TEXT
          SIZE 36 BY .81 AT ROW 13.38 COL 9
          FGCOLOR 0 FONT 4
     "digite la palabra clave a buscar, luego presione ENTER." VIEW-AS TEXT
          SIZE 38 BY .62 AT ROW 14.19 COL 9
          FGCOLOR 0 FONT 4
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 53.43 ROW 1.46
         SIZE 56.29 BY 15.15
         BGCOLOR 17 FGCOLOR 7 FONT 4
         TITLE BGCOLOR 7 FGCOLOR 7 "Busqueda de Cuentas de Presupuestos".

DEFINE FRAME F-Main
     BUTTON-56 AT ROW 1.54 COL 100
     Btn_Impresion AT ROW 3.15 COL 100 HELP
          "Permite generar la impresión del plan presupuestal"
     Btn_Consulta AT ROW 4.77 COL 100 HELP
          "Permite generar la consulta del plan presupuestal"
     BUTTON-57 AT ROW 14.19 COL 5
     Fcuenta AT ROW 14.19 COL 19 COLON-ALIGNED NO-LABEL
     FNombre AT ROW 14.19 COL 34 COLON-ALIGNED NO-LABEL
     BUTTON-58 AT ROW 14.19 COL 81
     BROWSE-2 AT ROW 15.81 COL 4
     Btn_Ayuda AT ROW 20.38 COL 103 HELP
          "Permite generar la Ayuda de la pantalla"
     RECT-227 AT ROW 1.27 COL 99
     RECT-229 AT ROW 13.92 COL 4
     RECT-230 AT ROW 2.35 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 112.57 BY 20.69
         BGCOLOR 17 .


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
         TITLE              = "SFG - Configuraciòn de Cuentas del Presupuesto"
         HEIGHT             = 20.69
         WIDTH              = 112.57
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
/* SETTINGS FOR FRAME F-Consulta
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F-Consulta:HIDDEN           = TRUE
       FRAME F-Consulta:MOVABLE          = TRUE.

/* SETTINGS FOR FRAME F-Main
                                                                        */
/* BROWSE-TAB BROWSE-2 BUTTON-58 F-Main */
ASSIGN 
       BROWSE-2:SEPARATOR-FGCOLOR IN FRAME F-Main      = 4.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-progra)
THEN W-progra:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Cuentas WHERE cuentas.cod_presupuesto EQ fcodigo AND Cuentas.cod_presupuesto NE " " NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-progra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-progra W-progra
ON END-ERROR OF W-progra /* SFG - Configuraciòn de Cuentas del Presupuesto */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-progra W-progra
ON WINDOW-CLOSE OF W-progra /* SFG - Configuraciòn de Cuentas del Presupuesto */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ayuda W-progra
ON CHOOSE OF Btn_Ayuda IN FRAME F-Main
OR HELP OF {&WINDOW-NAME}
DO:
  SYSTEM-HELP "AYUDAS\REDECOOP" CONTEXT 21.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Consulta W-progra
ON CHOOSE OF Btn_Consulta IN FRAME F-Main /* Consulta */
DO:
/*realizo el enlace con el b-Prepto, puesto que debo disparar el open query del
  browse. la vble w_primeravez, la necesito puesto que al ejecutar el programa
  no traia ningun codigo de presupuesto */

  RUN get-link-handle IN adm-broker-hdl 
    (INPUT THIS-PROCEDURE,
     INPUT 'abrir':U,
     OUTPUT W_Ventana).

  RUN Local-Open-query IN WIDGET-HANDLE(W_Ventana)(input W_Vble).

  IF NOT W_PrimeraVez THEN DO:
    FRAME F-Main:SENSITIVE = FALSE.
    VIEW FRAME F-Consulta.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Impresion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Impresion W-progra
ON CHOOSE OF Btn_Impresion IN FRAME F-Main /* Impresión */
DO:
  DEFINE VAR Listado AS CHAR INITIAL "Cta_ppto.LST".

  Listado = W_PathSpl + Listado.
  {Incluido\Imprimir.I "Listado"}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Consulta
&Scoped-define SELF-NAME Btn_Termina
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Termina W-progra
ON CHOOSE OF Btn_Termina IN FRAME F-Consulta /* Terminar Consulta */
DO:
  HIDE FRAME F-Consulta.
  FRAME F-Main:SENSITIVE = TRUE.
  OPEN QUERY BROWSE-2  FOR EACH Cuentas WHERE cuentas.cod_presupuesto EQ Fcodigo AND cuentas.Cod_presupuesto NE " " NO-LOCK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME BUTTON-56
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-56 W-progra
ON CHOOSE OF BUTTON-56 IN FRAME F-Main /* Button 56 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-57
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-57 W-progra
ON CHOOSE OF BUTTON-57 IN FRAME F-Main /* Relacionar con Puc. */
DO:
  RUN RECCUENTA.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-58
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-58 W-progra
ON CHOOSE OF BUTTON-58 IN FRAME F-Main /* Graba Relación */
DO:
  FIND Cuentas WHERE Cuentas.Cuenta EQ FCuenta
                NO-ERROR NO-WAIT.
  IF AVAILABLE(Cuentas) THEN DO:
       ASSIGN Cuentas.Cod_presupuesto = Fcodigo
              Cuentas.Id_presupuesto = TRUE.
  OPEN QUERY BROWSE-2  FOR EACH Cuentas WHERE cuentas.cod_presupuesto EQ Fcodigo and
                                              cuentas.cod_presupuesto NE " " NO-LOCK.
  ASSIGN fnombre = " "
         fcuenta = " ".
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Consulta
&Scoped-define BROWSE-NAME BROWSE-2
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
             INPUT  'b-cf_presupuesto.w':U ,
             INPUT  FRAME F-Consulta:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-cf_presupuesto ).
       RUN set-position IN h_b-cf_presupuesto ( 1.27 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-cf_presupuesto ( 11.85 , 53.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'v-cf_presupuesto.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-cf_presupuesto ).
       RUN set-position IN h_v-cf_presupuesto ( 3.15 , 9.00 ) NO-ERROR.
       /* Size in UIB:  ( 6.73 , 80.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'P-UPDSAV.W':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Save,
                     AddFunction = One-Record':U ,
             OUTPUT h_P-UPDSAV ).
       RUN set-position IN h_P-UPDSAV ( 8.54 , 99.00 ) NO-ERROR.
       RUN set-size IN h_P-UPDSAV ( 10.23 , 12.00 ) NO-ERROR.

       /* Links to SmartBrowser h_b-cf_presupuesto. */
       RUN add-link IN adm-broker-hdl ( h_b-cf_presupuesto , 'abrir':U , THIS-PROCEDURE ).

       /* Links to SmartViewer h_v-cf_presupuesto. */
       RUN add-link IN adm-broker-hdl ( h_b-cf_presupuesto , 'Record':U , h_v-cf_presupuesto ).
       RUN add-link IN adm-broker-hdl ( h_P-UPDSAV , 'TableIO':U , h_v-cf_presupuesto ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-cf_presupuesto ,
             Btn_Termina:HANDLE IN FRAME F-Consulta , 'BEFORE':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-cf_presupuesto ,
             BUTTON-56:HANDLE IN FRAME F-Main , 'AFTER':U ).
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
  DISPLAY Fcuenta FNombre 
      WITH FRAME F-Main IN WINDOW W-progra.
  ENABLE BUTTON-56 Btn_Impresion Btn_Consulta BUTTON-57 Fcuenta FNombre 
         BUTTON-58 BROWSE-2 Btn_Ayuda RECT-227 RECT-229 RECT-230 
      WITH FRAME F-Main IN WINDOW W-progra.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  ENABLE Btn_Termina 
      WITH FRAME F-Consulta IN WINDOW W-progra.
  {&OPEN-BROWSERS-IN-QUERY-F-Consulta}
  VIEW W-progra.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir_Excel W-progra 
PROCEDURE Imprimir_Excel :
{Incluido\Def_Excel.i}
 /* MANDA ENCABEZADO DE HOJA DE CALCULO*/
 
 E_NumFila = 1.
 E_NumColumn = 3.
 E_Fila      = "014Codigo        040Nombre                                  014Enlace        ".
 RUN W-GraExcel.w (INPUT E_Fila, INPUT E_NumColumn, OUTPUT E_CmpGrafic).
 
 /* launch Excel so it is visible to the user */
chExcelApp:Visible = TRUE.

/* create a new Workbook */
chWorkbook = chExcelApp:Workbooks:Add().

/* get the active Worksheet */
chWorkSheet = chExcelApp:Sheets:Item(1).

 FOR EACH Presupuesto NO-LOCK:
      FIND FIRST Cuentas WHERE Cuentas.Cod_Presupuesto = Presupuesto.Cod_Presupuesto NO-LOCK NO-ERROR.
      IF AVAILABLE(CUENTAS) THEN
         ASSIGN FCodigo = Cuentas.Cuenta.
      ELSE
         ASSIGN FCodigo = "  -- 0 --  ".
      
      E_Fila2     = "".
      E_Fila2     = "014" + STRING(Presupuesto.Cod_Presupuesto,"X(14)")
                  + "040" + STRING(Presupuesto.Nombre,"X(40)")
                  + "014" + STRING(FCodigo,"X(14)").
      {Incluido\imprimir_Excel.i}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize W-progra 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
  APPLY "CHOOSE" TO Btn_Consulta IN FRAME F-MAIN.
  W_PrimeraVez = FALSE.
  
  /* Code placed here will execute AFTER standard behavior.    */
   OPEN QUERY BROWSE-2  FOR EACH Cuentas WHERE cuentas.cod_presupuesto EQ Fcodigo NO-LOCK.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir W-progra 
PROCEDURE ProcesoImprimir :
DEFINE VAR Linea AS CHARACTER FORMAT "X(120)" INITIAL "".
  {INCLUIDO\RepEncabezado.I}    

    W_Reporte    = "REPORTE   : CUENTAS DEL PRESUPUESTO - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
    W_EncColumna = "Presupuesto       Nombre                                  Enlace P.u.c".

  DEFINE VAR W_Total AS INTEGER.
 
  DEFINE FRAME F-Informe
    Presupuesto.Cod_Presupuesto     AT 1 
    Presupuesto.Nombre              AT 17
    Cuentas.Cod_presupuesto         AT 60
    WITH DOWN WIDTH 132.

  DEFINE FRAME F-Totales
    WITH DOWN WIDTH 132.

    VIEW FRAME F-Encabezado.
    VIEW FRAME F-Ftr.
      
    FOR EACH Presupuesto NO-LOCK:
      FIND FIRST Cuentas WHERE Cuentas.Cod_Presupuesto = Presupuesto.Cod_Presupuesto NO-LOCK NO-ERROR.
      IF AVAILABLE(CUENTAS) THEN
         ASSIGN FCodigo = Cuentas.Cuenta.
      ELSE
         ASSIGN FCodigo = "  -- 0 --  ".
           
      DISPLAY Presupuesto.Cod_presupuesto
              Presupuesto.Nombre
              Fcodigo
      WITH WIDTH 132 FRAME F-Informe USE-TEXT NO-BOX STREAM-IO NO-LABELS.
      W_Total = W_Total + 1.
    END.
    DISPLAY  
       SKIP(1)
       "Total Cuentas de Presupuesto:" AT 1
       W_Total                         AT 30
      WITH WIDTH 132 FRAME F-Totales USE-TEXT NO-BOX STREAM-IO NO-LABELS.
    PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Reccuenta W-progra 
PROCEDURE Reccuenta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE xtempo AS CHAR.
    
RUN C-Cuentas.R (OUTPUT Fcuenta, OUTPUT fnombre, OUTPUT W_Naturaleza, OUTPUT W_CtrNat, INPUT "P").
ASSIGN fnombre:SCREEN-VALUE IN FRAME F-Main = fnombre
       Fcuenta:SCREEN-VALUE IN FRAME F-Main = Fcuenta.
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

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "Cuentas"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

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

