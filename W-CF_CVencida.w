&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME W-ProCre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-ProCre 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
 {Incluido/Variable.i "SHARED"}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Carven

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BBtn_Info Btn_Impresion Btn_Consulta ~
Btn_Ayuda RECT-268 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-ProCre AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-cf_cvencida AS HANDLE NO-UNDO.
DEFINE VARIABLE h_P-UPDSAV AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-cf_cvencida AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BBtn_Info 
     IMAGE-UP FILE "imagenes\informacion":U
     LABEL "Button 49" 
     SIZE 10 BY 1.65 TOOLTIP "Muestra la pantalla de información de sesión y los mensajes del usuario activo".

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

DEFINE RECTANGLE RECT-268
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 12 BY 5.38.

DEFINE BUTTON Btn_Terminar 
     IMAGE-UP FILE "imagenes\volver":U
     LABEL "Terminar Consulta" 
     SIZE 8 BY 1.62 TOOLTIP "Cierra esta ventana y vuelve a la pantalla anterior".

DEFINE BUTTON Btn_Cancel 
     IMAGE-UP FILE "imagenes\volver":U
     LABEL "&Terminar" 
     SIZE 10 BY 1.62
     BGCOLOR 8 FONT 4.

DEFINE BUTTON BUTTON-57 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Button 57" 
     SIZE 10 BY 1.65.

DEFINE VARIABLE R_CAR_VEN AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Cartera Vencida.", 1,
"Provisión Capital", 2,
"Provisión Interes", 3
     SIZE 20 BY 2.96
     FONT 5 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Carven
     BBtn_Info AT ROW 1.54 COL 102
     Btn_Impresion AT ROW 3.15 COL 102 HELP
          "Permite generar la impresión de la Cartera Vencida"
     Btn_Consulta AT ROW 4.77 COL 102 HELP
          "Permite generar la consulta de la Cartera Vencida"
     Btn_Ayuda AT ROW 20.12 COL 105 HELP
          "Permite generar la Ayuda de la Pantalla"
     RECT-268 AT ROW 1.27 COL 101
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.57 BY 21.19
         BGCOLOR 17 .

DEFINE FRAME W-CAR_VEN
     R_CAR_VEN AT ROW 1.27 COL 2 NO-LABEL
     BUTTON-57 AT ROW 1.27 COL 26
     Btn_Cancel AT ROW 2.88 COL 26
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 76 ROW 6.38
         SIZE 37 BY 4.58
         BGCOLOR 17 FONT 5
         TITLE "Informe Cuentas".

DEFINE FRAME Frm_consulta
     Btn_Terminar AT ROW 12.31 COL 68 HELP
          "Termina la consulta de Cartera Vencida"
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 36 ROW 4.88
         SIZE 76.57 BY 13.88
         BGCOLOR 17 FGCOLOR 0 FONT 4
         TITLE FGCOLOR 0 "Consulta de Productos".


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
  CREATE WINDOW W-ProCre ASSIGN
         HIDDEN             = YES
         TITLE              = "SFG - Configuración de Cartera Vencida"
         HEIGHT             = 21.19
         WIDTH              = 113.57
         MAX-HEIGHT         = 21.19
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 21.19
         VIRTUAL-WIDTH      = 114.29
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-ProCre 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-ProCre
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME W-CAR_VEN:FRAME = FRAME F-Carven:HANDLE.

/* SETTINGS FOR FRAME F-Carven
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR FRAME Frm_consulta
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME Frm_consulta:HIDDEN           = TRUE
       FRAME Frm_consulta:MOVABLE          = TRUE.

/* SETTINGS FOR FRAME W-CAR_VEN
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME W-CAR_VEN:HIDDEN           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-ProCre)
THEN W-ProCre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-ProCre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-ProCre W-ProCre
ON END-ERROR OF W-ProCre /* SFG - Configuración de Cartera Vencida */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-ProCre W-ProCre
ON WINDOW-CLOSE OF W-ProCre /* SFG - Configuración de Cartera Vencida */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BBtn_Info
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BBtn_Info W-ProCre
ON CHOOSE OF BBtn_Info IN FRAME F-Carven /* Button 49 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Ayuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ayuda W-ProCre
ON CHOOSE OF Btn_Ayuda IN FRAME F-Carven
OR HELP OF {&WINDOW-NAME} DO:
   SYSTEM-HELP "AYUDAS\REDECOOP" CONTEXT 6.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME W-CAR_VEN
&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel W-ProCre
ON CHOOSE OF Btn_Cancel IN FRAME W-CAR_VEN /* Terminar */
DO:
  HIDE FRAME W-Car_Ven FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Carven
&Scoped-define SELF-NAME Btn_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Consulta W-ProCre
ON CHOOSE OF Btn_Consulta IN FRAME F-Carven /* Consulta */
DO:
  DISABLE ALL EXCEPT Btn_Ayuda WITH FRAME F-Carven.
  FRAME {&frame-name}:SENSITIVE = FALSE.
  VIEW FRAME Frm_Consulta.
  ENABLE ALL WITH FRAME Frm_Consulta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Impresion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Impresion W-ProCre
ON CHOOSE OF Btn_Impresion IN FRAME F-Carven /* Impresión */
DO:      
  VIEW FRAME W-Car_Ven FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Frm_consulta
&Scoped-define SELF-NAME Btn_Terminar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Terminar W-ProCre
ON CHOOSE OF Btn_Terminar IN FRAME Frm_consulta /* Terminar Consulta */
DO:
  HIDE FRAME FRM_Consulta.       
  ENABLE ALL WITH FRAME F-Carven.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME W-CAR_VEN
&Scoped-define SELF-NAME BUTTON-57
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-57 W-ProCre
ON CHOOSE OF BUTTON-57 IN FRAME W-CAR_VEN /* Button 57 */
DO:
    DEFINE VAR Listado AS CHARACTER INITIAL "".
  HIDE FRAME W-Car_Ven FRAME {&FRAME-NAME}.
  Listado = W_PathSpl + "L-CARVEN.LST".
  {Incluido/Imprimir.I "Listado"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME R_CAR_VEN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_CAR_VEN W-ProCre
ON MOUSE-SELECT-CLICK OF R_CAR_VEN IN FRAME W-CAR_VEN
DO:
  APPLY "VALUE-CHANGED" TO R_Car_Ven.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_CAR_VEN W-ProCre
ON VALUE-CHANGED OF R_CAR_VEN IN FRAME W-CAR_VEN
OR RETURN OF R_Car_ven DO:
  ASSIGN R_Car_Ven.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Carven
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-ProCre 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */

    {src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-ProCre  _ADM-CREATE-OBJECTS
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
             INPUT  'b-cf_cvencida.w':U ,
             INPUT  FRAME Frm_consulta:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-cf_cvencida ).
       RUN set-position IN h_b-cf_cvencida ( 1.27 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-cf_cvencida ( 10.50 , 74.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'P-UPDSAV.W':U ,
             INPUT  FRAME F-Carven:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Save,
                     AddFunction = One-Record':U ,
             OUTPUT h_P-UPDSAV ).
       RUN set-position IN h_P-UPDSAV ( 9.35 , 101.00 ) NO-ERROR.
       RUN set-size IN h_P-UPDSAV ( 10.23 , 12.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'v-cf_cvencida.w':U ,
             INPUT  FRAME F-Carven:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-cf_cvencida ).
       RUN set-position IN h_v-cf_cvencida ( 1.81 , 6.00 ) NO-ERROR.
       /* Size in UIB:  ( 20.00 , 88.57 ) */

       /* Links to SmartViewer h_v-cf_cvencida. */
       RUN add-link IN adm-broker-hdl ( h_b-cf_cvencida , 'Record':U , h_v-cf_cvencida ).
       RUN add-link IN adm-broker-hdl ( h_P-UPDSAV , 'TableIO':U , h_v-cf_cvencida ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-cf_cvencida ,
             Btn_Terminar:HANDLE IN FRAME Frm_consulta , 'BEFORE':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_P-UPDSAV ,
             Btn_Consulta:HANDLE IN FRAME F-Carven , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-cf_cvencida ,
             h_P-UPDSAV , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-ProCre  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CVencida_Excel W-ProCre 
PROCEDURE CVencida_Excel :
{Incluido\Def_Excel.i}
        
DEFINE VARIABLE W_NomPcto AS CHARACTER FORMAT "X(30)" INITIAL "".
DEFINE VARIABLE W_Garantia AS CHARACTER FORMAT "X(10)" INITIAL "".
DEFINE VARIABLE W_WkOfi LIKE agencias.agencia.
DEFINE VARIABLE W_NomCal AS CHARACTER FORMAT "X(25)" INITIAL "".
DEFINE VARIABLE W_NomApo AS CHARACTER FORMAT "X(20)" INITIAL "".
    
E_NumFila = 1.
E_NumColumn = 12.
 
E_Fila = "030" + "Nombre Producto               " +
         "004" + "PIni" +
         "004" + "PFin" +
         "014" + "Cta_AsoAdDb   " +
         "014" + "Cta_NoaAdDb   " +
         "014" + "Cta_AsoNaDb   " +
         "014" + "Cta_NoaNaDb   " +
         "025" + "Calificacion             " +
         "020" + "Nombre Pdto Aportes " +
         "006" + "PrAdmi" +
         "006" + "PrApor" +
         "006" + "PrDfGa".
    
RUN W-GraExcel.w (INPUT E_Fila, INPUT E_NumColumn, OUTPUT E_CmpGrafic).
    
chExcelApp:Visible = TRUE.
chWorkbook = chExcelApp:Workbooks:Add().
chWorkSheet = chExcelApp:Sheets:Item(1).
    
FOR EACH CarteraVencida NO-LOCK BREAK BY CarteraVencida.Cod_Producto:
    ASSIGN W_NomPcto = "".
        
    FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_credito EQ CarteraVencida.Cod_Producto
                              AND Pro_Creditos.Estado EQ 1 NO-LOCK NO-ERROR.
    IF AVAILABLE(Pro_Creditos) THEN
        W_NomPcto = Pro_Creditos.Nom_Producto.
    ELSE
        W_NomPcto = "Producto no Existe...".
            
    FIND FIRST Varios WHERE Varios.Tipo EQ 10
                        AND Varios.Codigo EQ CarteraVencida.Cod_Califica
                        AND Varios.Estado EQ 1 NO-LOCK NO-ERROR.
    IF AVAILABLE Varios THEN
        W_NomCal = Varios.Descripcion.
    ELSE
        W_NomCal = "Calificacion no Existe".
            
    FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ CarteraVencida.Cod_Aportes
                             AND Pro_Ahorros.Tip_Ahorro EQ 4
                             AND Pro_Ahorros.Estado EQ 1 NO-LOCK NO-ERROR.
    IF AVAILABLE Pro_Ahorros THEN
        W_NomApo = Pro_Ahorros.Nom_Producto.
    ELSE
        W_NomApo = "Aportes no Existe".
            
    E_Fila2 = "".
    
    E_Fila2 = "030" + STRING(W_NomPcto,"X(30)") +
              "004" + STRING(CarteraVencida.Per_Inicial,"9999") +
              "004" + STRING(CarteraVencida.Per_Final,"9999") +
              "014" + STRING(CarteraVencida.Cta_AsoAdDb,"X(14)") +
              "014" + STRING(CarteraVencida.Cta_NoaAdDb,"X(14)") +
              "014" + STRING(CarteraVencida.Cta_AsoNaDb,"X(14)") +
              "014" + STRING(CarteraVencida.Cta_NoaNaDb,"X(14)") +
              "025" + STRING(W_NomCal,"X(25)") +
              "020" + STRING(W_NomApo,"X(20)") +
              "006" + STRING(CarteraVencida.Porc_Admisible,"999.99") +
              "006" + STRING(CarteraVencida.Porc_DefGarantia,"999.99").
        
    {Incluido\imprimir_Excel.i}
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-ProCre  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-ProCre)
  THEN DELETE WIDGET W-ProCre.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-ProCre  _DEFAULT-ENABLE
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
  ENABLE BBtn_Info Btn_Impresion Btn_Consulta Btn_Ayuda RECT-268 
      WITH FRAME F-Carven IN WINDOW W-ProCre.
  {&OPEN-BROWSERS-IN-QUERY-F-Carven}
  ENABLE Btn_Terminar 
      WITH FRAME Frm_consulta IN WINDOW W-ProCre.
  {&OPEN-BROWSERS-IN-QUERY-Frm_consulta}
  DISPLAY R_CAR_VEN 
      WITH FRAME W-CAR_VEN IN WINDOW W-ProCre.
  ENABLE R_CAR_VEN BUTTON-57 Btn_Cancel 
      WITH FRAME W-CAR_VEN IN WINDOW W-ProCre.
  {&OPEN-BROWSERS-IN-QUERY-W-CAR_VEN}
  VIEW W-ProCre.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir_Excel W-ProCre 
PROCEDURE Imprimir_Excel :
CASE R_Car_Ven: 
    WHEN 1 THEN
         RUN CVencida_Excel.
    WHEN 2 THEN
         RUN PrCapital_Excel.
    WHEN 3 THEN
         RUN PrInteres_Excel.
END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-ProCre 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrCapital_Excel W-ProCre 
PROCEDURE PrCapital_Excel :
{Incluido\Def_Excel.i}

DEFINE VARIABLE W_NomPcto AS CHARACTER FORMAT "X(30)" INITIAL "".
DEFINE VARIABLE W_Garantia AS CHARACTER FORMAT "X(10)" INITIAL "".
DEFINE VARIABLE W_WkOfi LIKE agencias.agencia.
DEFINE VARIABLE W_NomCal AS CHARACTER FORMAT "X(25)" INITIAL "".
DEFINE VARIABLE W_NomApo AS CHARACTER FORMAT "X(20)" INITIAL "". 

E_NumFila = 1.
E_NumColumn = 11.

E_Fila = "030" + "Nombre Producto               " +
         "004" + "PrIn" +
         "004" + "PrFi" +
         "014" + "Cta_AsoPrvAdDb" +
         "014" + "Cta_AsoPrvAdCr" +
         "014" + "Cta_NoaPrvAdDb" +
         "014" + "Cta_NoaPrvAdCr" +
         "014" + "Cta_AsoPrvNaDb" +
         "014" + "Cta_AsoPrvNaCr" +
         "014" + "Cta_NoaPrvNaDb" +
         "014" + "Cta_NoaPrvNaCr".

RUN W-GraExcel.w (INPUT E_Fila, INPUT E_NumColumn, OUTPUT E_CmpGrafic).

chExcelApp:Visible = TRUE.
chWorkbook = chExcelApp:Workbooks:Add().
chWorkSheet = chExcelApp:Sheets:Item(1).

FOR EACH CarteraVencida NO-LOCK BREAK BY CarteraVencida.Cod_Producto:
    ASSIGN W_NomPcto = "".

    FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ CarteraVencida.Cod_Producto
                              AND Pro_Creditos.Estado EQ 1 NO-LOCK NO-ERROR.
    IF AVAILABLE(Pro_Creditos) THEN
        W_NomPcto = Pro_Creditos.Nom_Producto.
    ELSE
        W_NomPcto = "Producto no Existe...".

    FIND FIRST Varios WHERE Varios.Tipo EQ 10
                        AND Varios.Codigo EQ CarteraVencida.Cod_Califica
                        AND Varios.Estado EQ 1 NO-LOCK NO-ERROR.
    IF AVAILABLE Varios THEN
        W_NomCal = Varios.Descripcion.
    ELSE
        W_NomCal = "Calificacion no Existe".

    FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ CarteraVencida.Cod_Aportes
                             AND Pro_Ahorros.Tip_Ahorro EQ 4
                             AND Pro_Ahorros.Estado EQ 1 NO-LOCK NO-ERROR.
    IF AVAILABLE Pro_Ahorros THEN
        W_NomApo = Pro_Ahorros.Nom_Producto.
    ELSE
        W_NomApo = "Aportes no Existe".

    E_Fila2 = "".

    E_Fila2 = "030" + STRING(W_NomPcto,"X(30)") +
              "004" + STRING(CarteraVencida.Per_Inicial,"9999") +
              "004" + STRING(CarteraVencida.Per_Final,"9999") +
              "014" + STRING(CarteraVencida.Cta_AsoPrvAdDb,"X(14)") +
              "014" + STRING(CarteraVencida.Cta_AsoPrvAdCr,"X(14)") +
              "014" + STRING(CarteraVencida.Cta_NoaPrvAdDb,"X(14)") +
              "014" + STRING(CarteraVencida.Cta_NoaPrvAdCr,"X(14)") +
              "014" + STRING(CarteraVencida.Cta_AsoPrvNaDb,"X(14)") +
              "014" + STRING(CarteraVencida.Cta_AsoPrvNaCr,"X(14)") +
              "014" + STRING(CarteraVencida.Cta_NoaPrvNaDb,"X(14)") +
              "014" + STRING(CarteraVencida.Cta_NoaPrvNaCr,"X(14)").

    {Incluido\imprimir_Excel.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrInteres_Excel W-ProCre 
PROCEDURE PrInteres_Excel :
{Incluido\Def_Excel.i}

DEFINE VARIABLE W_NomPcto AS CHARACTER FORMAT "X(30)" INITIAL "".
DEFINE VARIABLE W_Garantia AS CHARACTER FORMAT "X(10)" INITIAL "".
DEFINE VARIABLE W_WkOfi LIKE agencias.agencia.
DEFINE VARIABLE W_NomCal AS CHARACTER FORMAT "X(25)" INITIAL "".
DEFINE VARIABLE W_NomApo AS CHARACTER FORMAT "X(20)" INITIAL "".

E_NumFila = 1.
E_NumColumn = 11.

E_Fila = "030" + "Nombre Producto               " +
         "004" + "PrIn" +
         "004" + "PrFi" +
         "014" + "Cta_AsoIntAdDb" +
         "014" + "Cta_AsoIntAdCr" +
         "014" + "Cta_NoaIntAdDb" +
         "014" + "Cta_NoaIntAdCr" +
         "014" + "Cta_AsoIntNaDb" +
         "014" + "Cta_AsoIntNaCr" +
         "014" + "Cta_NoaIntNaDb" +
         "014" + "Cta_NoaIntNaCr".

RUN W-GraExcel.w (INPUT E_Fila, INPUT E_NumColumn, OUTPUT E_CmpGrafic).

chExcelApp:Visible = TRUE.
chWorkbook = chExcelApp:Workbooks:Add().
chWorkSheet = chExcelApp:Sheets:Item(1).

FOR EACH CarteraVencida NO-LOCK BREAK BY CarteraVencida.Cod_Producto:
    ASSIGN W_NomPcto = "".

    FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ CarteraVencida.Cod_Producto
                              AND Pro_Creditos.Estado EQ 1 NO-LOCK NO-ERROR.
    IF AVAILABLE(Pro_Creditos) THEN
        W_NomPcto = Pro_Creditos.Nom_Producto.
    ELSE
        W_NomPcto = "Producto no Existe...".

    FIND FIRST Varios WHERE Varios.Tipo EQ 10
                        AND Varios.Codigo EQ CarteraVencida.Cod_Califica
                        AND Varios.Estado EQ 1 NO-LOCK NO-ERROR.
    IF AVAILABLE Varios THEN
        W_NomCal = Varios.Descripcion.
    ELSE
        W_NomCal = "Calificacion no Existe".

    FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ CarteraVencida.Cod_Aportes
                             AND Pro_Ahorros.Tip_Ahorro EQ 4
                             AND Pro_Ahorros.Estado EQ 1 NO-LOCK NO-ERROR.
    IF AVAILABLE Pro_Ahorros THEN
        W_NomApo = Pro_Ahorros.Nom_Producto.
    ELSE
        W_NomApo = "Aportes no Existe".

    E_Fila2 = "".

    E_Fila2 = "030" + STRING(W_NomPcto,"X(30)") +
              "004" + STRING(CarteraVencida.Per_Inicial,"9999") +
              "004" + STRING(CarteraVencida.Per_Final,"9999") +
              "014" + STRING(CarteraVencida.Cta_AsoIntAdDb,"X(14)") +
              "014" + STRING(CarteraVencida.Cta_AsoIntAdCr,"X(14)") +
              "014" + STRING(CarteraVencida.Cta_NoaIntAdDb,"X(14)") +
              "014" + STRING(CarteraVencida.Cta_NoaIntAdCr,"X(14)") +
              "014" + STRING(CarteraVencida.Cta_AsoIntNaDb,"X(14)") +
              "014" + STRING(CarteraVencida.Cta_AsoIntNaCr,"X(14)") +
              "014" + STRING(CarteraVencida.Cta_NoaIntNaDb,"X(14)") +
              "014" + STRING(CarteraVencida.Cta_NoaIntNaCr,"X(14)").

    {Incluido\imprimir_Excel.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir W-ProCre 
PROCEDURE ProcesoImprimir :
{INCLUIDO\RepEncabezado.I}

DEFINE VARIABLE W_NomPcto AS CHARACTER FORMAT "X(30)" INITIAL "".
DEFINE VARIABLE W_Garantia AS CHARACTER FORMAT "X(10)" INITIAL "".
DEFINE VARIABLE W_WkOfi LIKE agencias.agencia.
DEFINE VARIABLE W_NomCal AS CHARACTER FORMAT "X(25)" INITIAL "".
DEFINE VARIABLE W_NomApo AS CHARACTER FORMAT "X(20)" INITIAL "".

DEFINE FRAME F-General
    CarteraVencida.Per_Inicial       AT 1
    CarteraVencida.Per_Final         AT 7
    CarteraVencida.Cta_AsoAdDb       AT 23
    CarteraVencida.Cta_NoaAdDb       AT 41
    CarteraVencida.Cta_AsoNaDb       AT 65
    CarteraVencida.Cta_NoaNaDb       AT 82
    W_NomCal                         AT 98
    W_NomApo                         AT 124
    CarteraVencida.Porc_Admisible    AT 145
    CarteraVencida.Porc_DefGarantia  AT 159
    WITH DOWN WIDTH 200 NO-BOX.

DEFINE FRAME F-Detallado
    CarteraVencida.Per_Inicial       AT   1
    CarteraVencida.Per_Final         AT   7
    CarteraVencida.Cta_AsoPrvAdDb    AT  23
    CarteraVencida.Cta_AsoPrvAdCr    AT  39
    CarteraVencida.Cta_NoaPrvAdDb    AT  54
    CarteraVencida.Cta_NoaPrvAdCr    AT  69
    CarteraVencida.Cta_AsoPrvNaDb    AT  84
    CarteraVencida.Cta_AsoPrvNaCr    AT 100
    CarteraVencida.Cta_NoaPrvNaDb    AT 115
    CarteraVencida.Cta_NoaPrvNaCr    AT 130
    WITH DOWN WIDTH 200 NO-BOX.

DEFINE FRAME F-Detal
    CarteraVencida.Per_Inicial       AT   1
    CarteraVencida.Per_Final         AT   7
    CarteraVencida.Cta_AsoIntAdDb    AT  23
    CarteraVencida.Cta_AsoIntAdCr    AT  39
    CarteraVencida.Cta_NoaIntAdDb    AT  54
    CarteraVencida.Cta_NoaIntAdCr    AT  69
    CarteraVencida.Cta_AsoIntNaDb    AT  84
    CarteraVencida.Cta_AsoIntNaCr    AT 100
    CarteraVencida.Cta_NoaIntNaDb    AT 115
    CarteraVencida.Cta_NoaIntNaCr    AT 130
    WITH DOWN WIDTH 200 NO-BOX.

DEFINE FRAME F-Encab HEADER
    W_Nom_Entidad AT 2
    "PAGINA:"     AT 90 PAGE-NUMBER FORMAT ">>>9"
    W_Ubicacion   AT 2
    W_IdEstacion  AT 2
    W_IdReporta   AT 2
    W_Reporte     AT 2
    W_Linea       AT 1
    "Período"                           AT 1
    "Provisión Garantía Admisible"      AT 34
    "Provisión Garantía No Admisible"   AT 93
    "       Asociado                     No Asociado                     Asociado                     No Asociado"                                    AT 23 SKIP(1)
    "Inic."                             AT  1
    "Final"                             AT  7
    "Débito         Crédito        Débito         Crédito         Débito         Crédito         Débito         Crédito                             " AT 23
    "_______________________________________________________________________________________________________________________________________________" AT  1
    WITH DOWN WIDTH 320 USE-TEXT PAGE-TOP FRAME F-Encab STREAM-IO NO-BOX.

DEFINE FRAME F-Encabezado HEADER
    W_Nom_Entidad AT 2
    "PAGINA:"     AT 90 PAGE-NUMBER FORMAT ">>>9"
    W_Ubicacion   AT 2
    W_IdEstacion  AT 2
    W_IdReporta   AT 2
    W_Reporte     AT 2
    W_Linea       AT 1
    "Período"                           AT 1
    "Provisión Garantía Admisible"      AT 34
    "Provisión Garantía No Admisible"   AT 93
    "       Asociado                     No Asociado                     Asociado                     No Asociado"                                    AT 23 SKIP(1)
    "Inic."                             AT  1
    "Final"                             AT  7
    "Débito         Crédito        Débito         Crédito         Débito         Crédito         Débito         Crédito                             " AT 23
    "_______________________________________________________________________________________________________________________________________________" AT  1
    WITH DOWN WIDTH 320 USE-TEXT PAGE-TOP FRAME F-Encabezado STREAM-IO NO-BOX.

DEFINE FRAME F-Encabeza HEADER
    W_Nom_Entidad AT 2
    "PAGINA:"     AT 90 PAGE-NUMBER FORMAT ">>>9"
    W_Ubicacion   AT 2
    W_IdEstacion  AT 2
    W_IdReporta   AT 2
    W_Reporte     AT 2
    W_Linea       AT 1
    "Cuentas Garantía Admisible"     AT 25
    "Cuentas Garantía No Admisible"  AT 67
    "Inic."                          AT  1
    "Final"                          AT  7
    "Asociado          No Asociado            Asociado          No Asociado      Calificación             Pro.Aportes       %Provi.%Aportes %Defecto "                      AT 23
    "_____________________________________________________________________________________________________________________________________________________________________________"  AT  1
    WITH DOWN WIDTH 320 USE-TEXT PAGE-TOP FRAME F-Encabeza STREAM-IO NO-BOX.

FIND FIRST Usuarios WHERE Usuarios.Usuario EQ W_Usuario NO-LOCK NO-ERROR.
IF AVAILABLE(Usuarios) THEN
    W_NomUsuEnc = Usuarios.Nombre.
ELSE
    W_NomUsuEnc = "Usuario No Encontrado".

FIND FIRST Estacion WHERE Estacion.Estacion EQ W_Estacion NO-LOCK NO-ERROR.
IF AVAILABLE(Estacion) THEN
    W_NomEstEnc = Estacion.Descripcion.
ELSE
    W_NomEstEnc = "Estacion No Encontrada".

ASSIGN W_Linea = FILL(W_Raya,130)
       W_Linea2 = FILL(W_Raya2,130)
       W_Ubicacion = "UBICACION : " + TRIM(W_Nom_Agencia)
       W_IdReporta = "USUARIO   : " + W_Usuario + " - " + TRIM(W_NomUsuEnc)
       W_IdEstacion = "ESTACION  : " + STRING(INTEGER(W_Estacion)) + " - " + W_NomEstEnc
       W_PiePagina = TRIM(W_Nom_Agencia) + " / " + STRING(TODAY) + " / " + STRING(TIME,"hh:mm am").

IF R_Car_Ven = 1 THEN DO:
    W_Reporte    = "REPORTE   : CUENTAS CARTERA VENCIDA - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
    VIEW FRAME F-Encabeza.
    VIEW FRAME f-ftr.
END.
ELSE DO:
    IF R_Car_Ven = 2 THEN DO:
        W_Reporte    = "REPORTE   : CUENTAS PROVISION CAPITAL - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
        VIEW FRAME F-Encabezado.
        VIEW FRAME f-ftr.
    END.
    ELSE DO:
        W_Reporte    = "REPORTE   : CUENTAS PROVISION INTERES - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
        VIEW FRAME F-Encab.
        VIEW FRAME f-ftr.
    END.
END.

FIND FIRST agencias WHERE agencias.Estado EQ 1
                      AND agencias.Tip_Agencia EQ "C" NO-LOCK NO-ERROR.
IF AVAILABLE agencias THEN
    W_WkOfi = agencias.agencia.
ELSE
    W_WkOfi = W_agencia.

FOR EACH CarteraVencida NO-LOCK BREAK BY CarteraVencida.Cod_Producto:
    ASSIGN W_NomPcto = "".

    FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ CarteraVencida.Cod_Producto
                              AND Pro_Creditos.Estado EQ 1 NO-LOCK NO-ERROR.
    IF AVAILABLE(Pro_Creditos) THEN
        W_NomPcto = Pro_Creditos.Nom_Producto.
    ELSE
        W_NomPcto = "Producto no Existe...".

    FIND FIRST Varios WHERE Varios.Tipo EQ 10
                        AND Varios.Codigo EQ CarteraVencida.Cod_Califica
                        AND Varios.Estado EQ 1 NO-LOCK NO-ERROR.
    IF AVAILABLE Varios THEN
        W_NomCal = Varios.Descripcion.
    ELSE
        ASSIGN W_NomCal = "Calificacion no Existe".

    FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ CarteraVencida.Cod_Aportes
                             AND Pro_Ahorros.Tip_Ahorro EQ 4
                             AND Pro_Ahorros.Estado EQ 1 NO-LOCK NO-ERROR.
    IF AVAILABLE Pro_Ahorros THEN
        W_NomApo = Pro_Ahorros.Nom_Producto.
    ELSE
        W_NomApo = "Aportes no Existe".

    IF FIRST-OF(CarteraVencida.Cod_Producto) THEN DO:
        DISPLAY SKIP(1)
                "Producto: "     AT 2
                W_NomPcto        AT 13 SKIP(1)
            WITH WIDTH 200 FRAME F-NomRep PAGE-BOTTOM NO-LABELS USE-TEXT STREAM-IO NO-BOX.
    END.

    IF R_Car_Ven = 1 THEN DO:
        DISPLAY CarteraVencida.Per_Inicial
                CarteraVencida.Per_Final
                CarteraVencida.Cta_AsoAdDb
                CarteraVencida.Cta_NoaAdDb
                CarteraVencida.Cta_AsoNaDb
                CarteraVencida.Cta_NoaNaDb
                W_NomCal
                W_NomApo
                CarteraVencida.Porc_Admisible
                CarteraVencida.Porc_DefGarantia
            WITH WIDTH 200 FRAME F-General  USE-TEXT STREAM-IO NO-LABELS.
    END.
    ELSE DO:
        IF R_Car_Ven = 2 THEN DO:
            DISPLAY CarteraVencida.Per_Inicial       AT   1
                    CarteraVencida.Per_Final         AT   7
                    CarteraVencida.Cta_AsoPrvAdDb    AT  23
                    CarteraVencida.Cta_AsoPrvAdCr    AT  39
                    CarteraVencida.Cta_NoaPrvAdDb    AT  54
                    CarteraVencida.Cta_NoaPrvAdCr    AT  69
                    CarteraVencida.Cta_AsoPrvNaDb    AT  84
                    CarteraVencida.Cta_AsoPrvNaCr    AT 100
                    CarteraVencida.Cta_NoaPrvNaDb    AT 115
                    CarteraVencida.Cta_NoaPrvNaCr    AT 130
                WITH WIDTH 200 FRAME F-Detallado USE-TEXT STREAM-IO NO-LABELS.
        END.
        ELSE DO:
            DISPLAY CarteraVencida.Per_Inicial       AT   1
                    CarteraVencida.Per_Final         AT   7
                    CarteraVencida.Cta_AsoIntAdDb    AT  23
                    CarteraVencida.Cta_AsoIntAdCr    AT  39
                    CarteraVencida.Cta_NoaIntAdDb    AT  54
                    CarteraVencida.Cta_NoaIntAdCr    AT  69
                    CarteraVencida.Cta_AsoIntNaDb    AT  84
                    CarteraVencida.Cta_AsoIntNaCr    AT 100
                    CarteraVencida.Cta_NoaIntNaDb    AT 115
                    CarteraVencida.Cta_NoaIntNaCr    AT 130
                WITH WIDTH 200 FRAME F-Detal USE-TEXT STREAM-IO NO-LABELS.
        END.
    END.

    IF LAST-OF(CarteraVencida.Cod_Producto) THEN DO:
        IF R_Car_Ven = 1 THEN DO:
            DISPLAY "__________________________________________________________________________________________________________________________________________________________________________"  AT  1
                WITH WIDTH 200 FRAME F-TotPla PAGE-BOTTOM USE-TEXT STREAM-IO NO-BOX.
        END.
        ELSE DO:
            DISPLAY "________________________________________________________________________________________________________________________________________________"  AT  1
                WITH WIDTH 200 FRAME F-Tot PAGE-BOTTOM USE-TEXT STREAM-IO NO-BOX.
        END.
    END.
END.

PAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-ProCre  _ADM-SEND-RECORDS
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-ProCre 
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

