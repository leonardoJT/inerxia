&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
CREATE WIDGET-POOL.

DEFINE VAR tasa1 AS DECIMAL.

{INCLUIDO\VARIABLE.I "SHARED"}

DEFINE VAR Puntero AS ROWID.
DEFINE VAR W_TipoInforme AS CHARACTER.
DEFINE VAR W_SiMInst AS LOGICAL.
DEFINE VAR Tas_Nominal AS DECIMAL.
DEFINE VAR RowidGar_Global AS ROWID.
DEFINE VAR V_Cod AS INTEGER.
DEFINE VAR V_Nom AS CHARACTER.
DEFINE VAR PlaMinI AS DECIMAL.
DEFINE VAR PlaMaxI AS DECIMAL.
DEFINE VAR WDed AS DECIMAL.
DEFINE VAR NN AS INTEGER.
DEFINE VAR P_ForPag AS INTEGER.
DEFINE VAR P_CedNit AS CHARACTER.
DEFINE VAR vcFormatoFecha AS CHARACTER.
DEFINE VAR k AS INTEGER FORMAT "9".
DEFINE VAR CodCreditoActivo AS INTEGER.

DEFINE VARIABLE A_Age AS INTEGER.
DEFINE VARIABLE A_Pro AS INTEGER.
DEFINE VARIABLE A_NitW AS CHARACTER.
DEFINE VARIABLE A_Cue AS CHARACTER.
DEFINE VARIABLE i AS INTEGER.
DEFINE VARIABLE W_Ok AS LOGICAL.
DEFINE VARIABLE Dias AS DECIMAL.

DEFINE VAR vPeriodicidad AS INTEGER INITIAL 1.
DEFINE VAR Wk_Edad AS INTEGER.
DEFINE VAR WFactorCod AS INTEGER.

DEFINE TEMP-TABLE TmpI
    FIELD ILinea AS INTEGER FORMAT "99"
    FIELD ITexto AS CHARACTER FORMAT "X(125)".
        
DEFINE TEMP-TABLE Temp_Extras
    FIELD Nro_Cuota AS INTEGER FORMAT "9999"
    FIELD Vr_CuoExtra AS DECIMAL FORMAT "->>>>,>>>,>>9.99"
    FIELD Fec_Vcto AS DATE FORMAT "99/99/9999"
    FIELD Estado AS INTEGER.

DEFINE BUFFER BTemp_Extras FOR Temp_Extras.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME Br_Extras

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Temp_Extras

/* Definitions for BROWSE Br_Extras                                     */
&Scoped-define FIELDS-IN-QUERY-Br_Extras Temp_Extras.Nro_Cuota Temp_Extras.Vr_CuoExtra Temp_Extras.Fec_Vcto   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Extras   
&Scoped-define SELF-NAME Br_Extras
&Scoped-define QUERY-STRING-Br_Extras FOR EACH Temp_Extras                         NO-LOCK BY Temp_Extras.Nro_Cuota INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br_Extras OPEN QUERY Br_Extras FOR EACH Temp_Extras                         NO-LOCK BY Temp_Extras.Nro_Cuota INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_Extras Temp_Extras
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Extras Temp_Extras


/* Definitions for FRAME F_Extras                                       */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Extras ~
    ~{&OPEN-QUERY-Br_Extras}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-334 RECT-323 RECT-325 Fecha_Solicitud ~
Tipo_de_credito S_InfoProducto Cmb_Productos Monto_de_solicitud ~
Forma_de_interes Tasa_de_solicitud Plazo_de_solicitud Cmb_Sistemas ~
W_TasaNominal Fecha_de_desembolso Cmb_PerPago W_TasaPeriodo ~
Fecha_de_primerPago Btn_Proyectar Cuota_de_solicitud NomIndicador ~
Total_del_prestamo Btn_Ingresar W_VrADesemb W_TotExt BUTTON-2 Btn_Extras ~
W_NomPdo 
&Scoped-Define DISPLAYED-OBJECTS Fecha_Solicitud Tipo_de_credito ~
S_InfoProducto Cmb_Productos Monto_de_solicitud Forma_de_interes ~
Tasa_de_solicitud Plazo_de_solicitud Cmb_Sistemas W_TasaNominal ~
Fecha_de_desembolso Cmb_PerPago W_TasaPeriodo Fecha_de_primerPago ~
Cuota_de_solicitud NomIndicador Total_del_prestamo W_VrADesemb W_TotExt ~
W_NomPdo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-2 Btn_Ingresar 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Extras 
     LABEL "Cuotas extraordinarias" 
     SIZE 32 BY 1.

DEFINE BUTTON Btn_Ingresar 
     IMAGE-UP FILE "imagenes/borrar.bmp":U
     LABEL "Ingresar" 
     SIZE 10 BY 1.69.

DEFINE BUTTON Btn_Proyectar 
     IMAGE-UP FILE "imagenes/e_reporte.ico":U
     LABEL "Proyección de Pagos" 
     SIZE 10 BY 1.69.

DEFINE BUTTON BUTTON-2 
     IMAGE-UP FILE "imagenes/salir3.bmp":U
     LABEL "Salir" 
     SIZE 10 BY 1.69.

DEFINE VARIABLE Cmb_PerPago AS CHARACTER FORMAT "X(25)":U INITIAL "4 - Mensual" 
     LABEL "Período" 
     VIEW-AS COMBO-BOX INNER-LINES 6
     LIST-ITEMS "0 - Diario","1 - Semanal","2 - Decadal","3 - Quincenal","4 - Mensual","5 - Bimestral","6 - Trimestral","7 - Cuatrimestral","8 - Semestral","9 - Anual" 
     DROP-DOWN-LIST
     SIZE 22 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_Productos AS CHARACTER FORMAT "X(70)":U 
     LABEL "Producto" 
     VIEW-AS COMBO-BOX INNER-LINES 30
     DROP-DOWN-LIST
     SIZE 31 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_Sistemas AS CHARACTER FORMAT "X(60)":U 
     LABEL "Sistema" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 22 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cuota_de_solicitud AS DECIMAL FORMAT ">>>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     BGCOLOR 3 FGCOLOR 15 FONT 5.

DEFINE VARIABLE Fecha_de_desembolso AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha desembolso" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     FONT 4.

DEFINE VARIABLE Fecha_de_primerPago AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha primer pago" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     FONT 4.

DEFINE VARIABLE Fecha_Solicitud AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha de solicitud" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 3 FGCOLOR 15 .

DEFINE VARIABLE Monto_de_solicitud AS DECIMAL FORMAT ">>>,>>>,>>>,>>9":U INITIAL 0 
     LABEL "Monto" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     BGCOLOR 15 FONT 5.

DEFINE VARIABLE NomIndicador AS CHARACTER FORMAT "X(30)":U 
     LABEL "Indicador" 
     VIEW-AS FILL-IN 
     SIZE 39 BY 1
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Plazo_de_solicitud AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Plazo" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     BGCOLOR 15 FONT 5.

DEFINE VARIABLE Tasa_de_solicitud AS DECIMAL FORMAT ">>9.999999":U INITIAL 0 
     LABEL "Tasa" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 3 FGCOLOR 15 .

DEFINE VARIABLE Total_del_prestamo AS DECIMAL FORMAT "->>>>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Total del préstamo" 
     VIEW-AS FILL-IN 
     SIZE 16.57 BY 1
     BGCOLOR 3 FGCOLOR 15 .

DEFINE VARIABLE W_NomPdo AS CHARACTER FORMAT "X(30)":U INITIAL "Cuota" 
      VIEW-AS TEXT 
     SIZE 16 BY .62
     BGCOLOR 8  NO-UNDO.

DEFINE VARIABLE W_TasaNominal AS DECIMAL FORMAT ">>9.9999":U INITIAL 0 
     LABEL "Nominal" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_TasaPeriodo AS DECIMAL FORMAT ">>9.9999":U INITIAL 0 
     LABEL "Período" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_TotExt AS DECIMAL FORMAT "->>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Valor Extras" 
     VIEW-AS FILL-IN 
     SIZE 16.57 BY 1
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_VrADesemb AS DECIMAL FORMAT "->,>>>,>>>,>>9":U INITIAL 0 
     LABEL "Valor a desembolsar" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Forma_de_interes AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Vencido", 1,
"Anticipado", 2
     SIZE 23.29 BY .81
     FONT 5.

DEFINE VARIABLE Tipo_de_credito AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Consumo", 1,
"Comercial", 2,
"Hipotecario", 3,
"Microcrédito", 4
     SIZE 52 BY 1.08.

DEFINE RECTANGLE RECT-323
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 25 BY 5.65.

DEFINE RECTANGLE RECT-325
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 26.72 BY 2.54.

DEFINE RECTANGLE RECT-334
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 37.57 BY 12.12.

DEFINE VARIABLE S_InfoProducto AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 35 BY 2.69
     BGCOLOR 15 FONT 2 NO-UNDO.

DEFINE BUTTON Btn_AdExt 
     LABEL "&Salvar Extra" 
     SIZE 15.43 BY 1.12.

DEFINE BUTTON Btn_EliExt 
     LABEL "&Eliminar Extra" 
     SIZE 15.43 BY 1.12.

DEFINE BUTTON BUTTON-170 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 170" 
     SIZE 10.57 BY 1.62 TOOLTIP "Retorna a la ventana principal".

DEFINE VARIABLE W_PPExtra AS INTEGER FORMAT ">>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5.43 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_VrExtra AS DECIMAL FORMAT ">>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15.86 BY 1
     BGCOLOR 15  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Br_Extras FOR 
      Temp_Extras SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Br_Extras
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Extras C-Win _FREEFORM
  QUERY Br_Extras NO-LOCK DISPLAY
      Temp_Extras.Nro_Cuota FORMAT "9999":U                     COLUMN-LABEL "Pdo.Pago"
      Temp_Extras.Vr_CuoExtra FORMAT "->>>>,>>>,>>9.99":U       COLUMN-LABEL "Vr.Cuota Extra"
      Temp_Extras.Fec_Vcto FORMAT "99/99/9999":U                COLUMN-LABEL "Fecha-Vcto"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 70.72 BY 7.88
         BGCOLOR 15 FONT 5 ROW-HEIGHT-CHARS .46 FIT-LAST-COLUMN TOOLTIP "Con CLICK selecciona la Extra para ser Modificada y/o Eliminarla".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     Fecha_Solicitud AT ROW 1.5 COL 19 COLON-ALIGNED WIDGET-ID 422
     Tipo_de_credito AT ROW 3.96 COL 3 NO-LABEL WIDGET-ID 406
     S_InfoProducto AT ROW 3.96 COL 58 NO-LABEL WIDGET-ID 430
     Cmb_Productos AT ROW 5.31 COL 13 COLON-ALIGNED WIDGET-ID 398
     Monto_de_solicitud AT ROW 8.19 COL 21.57 COLON-ALIGNED WIDGET-ID 402
     Forma_de_interes AT ROW 9.08 COL 46.86 NO-LABEL WIDGET-ID 424
     Tasa_de_solicitud AT ROW 9.42 COL 86 COLON-ALIGNED WIDGET-ID 432
     Plazo_de_solicitud AT ROW 9.54 COL 21.57 COLON-ALIGNED WIDGET-ID 404
     Cmb_Sistemas AT ROW 10.54 COL 51 COLON-ALIGNED WIDGET-ID 420 NO-TAB-STOP 
     W_TasaNominal AT ROW 10.62 COL 86 COLON-ALIGNED WIDGET-ID 434
     Fecha_de_desembolso AT ROW 10.96 COL 21.57 COLON-ALIGNED WIDGET-ID 390
     Cmb_PerPago AT ROW 11.65 COL 51 COLON-ALIGNED WIDGET-ID 418 NO-TAB-STOP 
     W_TasaPeriodo AT ROW 11.85 COL 86 COLON-ALIGNED WIDGET-ID 436
     Fecha_de_primerPago AT ROW 12.42 COL 21.57 COLON-ALIGNED WIDGET-ID 392
     Btn_Proyectar AT ROW 13.54 COL 104 WIDGET-ID 416 NO-TAB-STOP 
     Cuota_de_solicitud AT ROW 13.85 COL 21.57 COLON-ALIGNED NO-LABEL WIDGET-ID 400
     NomIndicador AT ROW 14.73 COL 56 COLON-ALIGNED WIDGET-ID 428
     Total_del_prestamo AT ROW 15.19 COL 20 COLON-ALIGNED WIDGET-ID 412
     Btn_Ingresar AT ROW 15.58 COL 104 WIDGET-ID 440
     W_VrADesemb AT ROW 16.35 COL 75 COLON-ALIGNED WIDGET-ID 438
     W_TotExt AT ROW 16.54 COL 20 COLON-ALIGNED WIDGET-ID 414
     BUTTON-2 AT ROW 17.62 COL 104 WIDGET-ID 442
     Btn_Extras AT ROW 18.15 COL 5.57 WIDGET-ID 396
     W_NomPdo AT ROW 14.08 COL 5.72 NO-LABEL WIDGET-ID 444
     "Tipo de interés" VIEW-AS TEXT
          SIZE 14 BY .62 AT ROW 8.04 COL 51 WIDGET-ID 446
     "Tasa de interés" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 8 COL 82 WIDGET-ID 448
     "Tipo de crédito" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 3.04 COL 18 WIDGET-ID 394
     RECT-334 AT ROW 7.65 COL 2.86 WIDGET-ID 388
     RECT-323 AT ROW 7.65 COL 77 WIDGET-ID 10
     RECT-325 AT ROW 7.65 COL 44.86 WIDGET-ID 352
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 116.43 BY 19.54 WIDGET-ID 100.

DEFINE FRAME F_Extras
     Btn_AdExt AT ROW 2.23 COL 40
     W_PPExtra AT ROW 2.27 COL 3.72 COLON-ALIGNED NO-LABEL
     W_VrExtra AT ROW 2.27 COL 17.43 COLON-ALIGNED NO-LABEL
     BUTTON-170 AT ROW 2.31 COL 58.72
     Btn_EliExt AT ROW 3.65 COL 40.14
     Br_Extras AT ROW 5.23 COL 1.72
     "Pdo.Pago              Valor Cuota Extra" VIEW-AS TEXT
          SIZE 35 BY .62 AT ROW 1.38 COL 4.72
          BGCOLOR 18 FGCOLOR 15 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 24 ROW 4.23
         SIZE 72.86 BY 13.19
         FONT 5
         TITLE "Extras Pactadas" WIDGET-ID 200.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Simulación de solicitud"
         HEIGHT             = 19.62
         WIDTH              = 116.43
         MAX-HEIGHT         = 27.73
         MAX-WIDTH          = 194.86
         VIRTUAL-HEIGHT     = 27.73
         VIRTUAL-WIDTH      = 194.86
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME F_Extras:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON Btn_Ingresar IN FRAME DEFAULT-FRAME
   2                                                                    */
/* SETTINGS FOR FILL-IN W_NomPdo IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FRAME F_Extras
   NOT-VISIBLE                                                          */
/* BROWSE-TAB Br_Extras Btn_EliExt F_Extras */
ASSIGN 
       FRAME F_Extras:HIDDEN           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_Extras
/* Query rebuild information for BROWSE Br_Extras
     _START_FREEFORM
OPEN QUERY Br_Extras FOR EACH Temp_Extras
                        NO-LOCK BY Temp_Extras.Nro_Cuota INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE Br_Extras */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Extras
/* Query rebuild information for FRAME F_Extras
     _Query            is NOT OPENED
*/  /* FRAME F_Extras */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Simulación de solicitud */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Simulación de solicitud */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br_Extras
&Scoped-define FRAME-NAME F_Extras
&Scoped-define SELF-NAME Br_Extras
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Extras C-Win
ON MOUSE-SELECT-CLICK OF Br_Extras IN FRAME F_Extras
DO:
    IF AVAILABLE(Temp_Extras) THEN
        ASSIGN W_PPExtra = Temp_Extras.Nro_Cuota
               W_PPExtra:SCREEN-VALUE = STRING(Temp_Extras.Nro_Cuota)
               W_VrExtra = Temp_Extras.Vr_CuoExtra
               W_VrExtra:SCREEN-VALUE = STRING(Temp_Extras.Vr_CuoExtra).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_AdExt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_AdExt C-Win
ON CHOOSE OF Btn_AdExt IN FRAME F_Extras /* Salvar Extra */
DO:
    DEFINE VAR W_NroDias AS INTEGER.
    DEFINE VAR jj AS INTEG FORM "99999".
    DEFINE VAR W_FecTra AS DATE.

    CASE INT(SUBSTR(Cmb_PerPago:SCREEN-VALUE IN FRAME default-Frame,1,1)):
        WHEN 0 THEN W_NroDias = 1.
        WHEN 1 THEN W_NroDias = 7.
        WHEN 2 THEN W_NroDias = 10.
        WHEN 3 THEN W_NroDias = 15.
        WHEN 4 THEN W_NroDias = 30.
        WHEN 5 THEN W_NroDias = 60.
        WHEN 6 THEN W_NroDias = 90.
        WHEN 7 THEN W_NroDias = 120.
        WHEN 8 THEN W_NroDias = 180.
        WHEN 9 THEN W_NroDias = 360.
    END CASE.

    IF W_PPExtra <= 0 OR W_PPExtra > INT(Plazo_de_solicitud:SCREEN-VALUE) THEN DO:
        MESSAGE "El período de pago de la cuota extra debe ser mayor que 0 y" SKIP
                "menor o igual el plazo total... No se acepta la operación."
            VIEW-AS ALERT-BOX ERROR.

        RETURN.
    END.

    IF W_VrExtra <= 0 OR W_VrExtra >= DEC(Monto_de_solicitud:SCREEN-VALUE) THEN DO:
        MESSAGE "El valor de la cuota extra debe ser mayor que 0 y" SKIP
                "menor al monto total... No se acepta la operación."
            VIEW-AS ALERT-BOX ERROR.

        RETURN.
    END.

    W_TotExt = W_VrExtra.

    FOR EACH Temp_Extras:
        IF Temp_Extras.Estado = 1 AND Temp_Extras.Nro_Cuota <> W_PPExtra THEN
            W_TotExt = W_TotExt + Temp_Extras.Vr_CuoExtra.
        ELSE
            IF Temp_Extras.Estado <> 1 THEN
                DELETE Temp_Extras.
    END.

    IF W_TotExt >= DEC(Monto_de_solicitud:SCREEN-VALUE) THEN DO:
        MESSAGE "El valor total de las cuotas extras + El valor de esta Extra" SKIP
                "debe ser menor al monto total... No se acepta la operaciòn."
            VIEW-AS ALERT-BOX ERROR.

        RETURN.
    END.

    FIND FIRST Temp_Extras WHERE Extras.Nro_Cuota = W_PPExtra NO-ERROR.
    IF NOT AVAIL(Temp_Extras) THEN
        CREATE Temp_Extras.

    ASSIGN Temp_Extras.Nro_Cuota = W_PPExtra
           Temp_Extras.Vr_CuoExtra = W_VrExtra
           Temp_Extras.Estado = 1
           W_TotExt = 0
           W_FecTra = W_Fecha.

    DO JJ = 1 TO W_PPExtra:
        RUN Halla_FecVcto.R (INPUT DATE(Fecha_de_desembolso:SCREEN-VALUE),
                             INPUT W_NroDias,
                             INPUT W_FecTra,
                             OUTPUT W_FecTra).

        Temp_Extras.Fec_Vcto = W_FecTra.
    END.

    FIND CURRENT Temp_Extras NO-LOCK NO-ERROR.

    FOR EACH Temp_Extras WHERE Temp_Extras.estado = 1 NO-LOCK:
        ASSIGN W_TotExt = W_TotExt + Temp_Extras.Vr_CuoExtra
               W_TotExt:SCREEN-VALUE = STRING(W_TotExt).
    END.

    CLOSE QUERY Br_Extras.
    OPEN QUERY Br_Extras FOR EACH Temp_Extras NO-LOCK BY Temp_Extras.Nro_Cuota INDEXED-REPOSITION.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_EliExt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_EliExt C-Win
ON CHOOSE OF Btn_EliExt IN FRAME F_Extras /* Eliminar Extra */
DO:
   FIND FIRST Temp_Extras WHERE Temp_Extras.Nro_Cuota     EQ W_PPExtra NO-ERROR.
   IF AVAIL(Temp_Extras) THEN
      DELETE Temp_Extras.

   ASSIGN W_TotExt = 0.

   FOR EACH Temp_Extras NO-LOCK:
      IF Temp_Extras.Estado EQ 1 THEN
         ASSIGN W_TotExt = W_TotExt + Temp_Extras.Vr_CuoExtra
                W_TotExt:SCREEN-VALUE IN FRAME Default-Frame= STRING(W_TotExt).
   END.  

   CLOSE QUERY Br_Extras.
   OPEN  QUERY Br_Extras FOR EACH Temp_Extras NO-LOCK BY Temp_Extras.Nro_Cuota INDEXED-REPOSITION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME Btn_Extras
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Extras C-Win
ON CHOOSE OF Btn_Extras IN FRAME DEFAULT-FRAME /* Cuotas extraordinarias */
DO:

   ASSIGN FRAME F_Extras:VISIBLE     = TRUE.

   CLOSE QUERY Br_Extras.
   OPEN  QUERY Br_Extras FOR EACH Temp_Extras NO-LOCK BY Temp_Extras.Nro_Cuota INDEXED-REPOSITION.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Ingresar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ingresar C-Win
ON CHOOSE OF Btn_Ingresar IN FRAME DEFAULT-FRAME /* Ingresar */
DO:

  RUN Inicializar_Variables.  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Proyectar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Proyectar C-Win
ON CHOOSE OF Btn_Proyectar IN FRAME DEFAULT-FRAME /* Proyección de Pagos */
DO:
    
    DEF VAR W_Valida AS LOG INIT FALSE.

    RUN validar_solicitud NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN NO-APPLY.

    DO WITH FRAME Default-Frame:
        /* validaciones pantalla*/
        APPLY "leave" TO Monto_de_solicitud.
        APPLY "leave" TO Plazo_de_solicitud.

        RUN Liquidar.

        IF DEC(Monto_de_solicitud:SCREEN-VALUE) EQ 0 THEN DO:
            MESSAGE "No se puede simular la solicitud. Falta ingresar el monto"
                VIEW-AS ALERT-BOX ERROR.

            APPLY "entry" TO Monto_de_solicitud.
            RETURN.
        END.

        IF INT(Plazo_de_solicitud:SCREEN-VALUE) EQ 0 THEN DO:
            MESSAGE "No se puede simular la solicitud. Falta ingresar el plazo"
                VIEW-AS ALERT-BOX ERROR.

            APPLY "entry" TO Plazo_de_solicitud.
            RETURN.
        END.

        IF DEC(Tasa_de_solicitud:SCREEN-VALUE) EQ 0 THEN DO:
            MESSAGE "No se puede simular la solicitud. Falta la tasa, para esto debe entrar el plazo"
                VIEW-AS ALERT-BOX ERROR.
            RETURN.
        END.

        RUN Valida_Soli2.R (INPUT Pro_Creditos.Cod_Credito,
                           INPUT DEC(Monto_de_solicitud:SCREEN-VALUE),
                           INPUT INT(Plazo_de_solicitud:SCREEN-VALUE),
                           INPUT vPeriodicidad,
                           OUTPUT W_Valida) NO-ERROR.

        IF W_Valida THEN
            RETURN.
        /* fin validaciones pantalla*/

    END.

    ASSIGN W_VrADesemb = DEC(Total_del_prestamo:SCREEN-VALUE)
           W_VrADesemb:SCREEN-VALUE = STRING(W_VrADesemb).

    DEFINE VAR Listado AS CHARACTER INITIAL "".

    ASSIGN Listado = W_PathSpl + "Proyeccion" + W_Usuario + STRING(RANDOM(2000,10000)) + ".LST"
        W_TipoInforme = "Proyeccion".

    {INCLUIDO\Imprimir.I "Listado"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Extras
&Scoped-define SELF-NAME BUTTON-170
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-170 C-Win
ON CHOOSE OF BUTTON-170 IN FRAME F_Extras /* Button 170 */
DO:
    ASSIGN FRAME F_Extras:VISIBLE     = FALSE.
    RUN validar_solicitud NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN NO-APPLY.
    
    DO WITH FRAME Default-Frame:
        /* validaciones pantalla*/
        APPLY "leave" TO Monto_de_solicitud IN FRAME Default-Frame.
        APPLY "leave" TO Plazo_de_solicitud.
        RUN Liquidar.
    END.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 C-Win
ON CHOOSE OF BUTTON-2 IN FRAME DEFAULT-FRAME /* Salir */
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


&Scoped-define SELF-NAME Cmb_PerPago
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_PerPago C-Win
ON VALUE-CHANGED OF Cmb_PerPago IN FRAME DEFAULT-FRAME /* Período */
DO:
    DO WITH FRAME Default-Frame:
        Cuota_de_solicitud:SCREEN-VALUE = "0".

        IF Plazo_de_solicitud:SCREEN-VALUE NE "0" THEN DO:
            CASE INTEGER(SUBSTRING(Cmb_PerPago:SCREEN-VALUE,1,1)):
                WHEN 0 THEN
                    ASSIGN Plazo_de_solicitud:SCREEN-VALUE = STRING(ABS(Dias / 1))
                           W_NomPdo:SCREEN-VALUE = "Cuota Diaria".

                WHEN 1 THEN
                    ASSIGN Plazo_de_solicitud:SCREEN-VALUE = STRING(ABS(Dias / 7))
                           W_NomPdo:SCREEN-VALUE = "Cuota Semanal".

                WHEN 2 THEN
                    ASSIGN Plazo_de_solicitud:SCREEN-VALUE = STRING(ABS(Dias / 10))
                           W_NomPdo:SCREEN-VALUE = "Cuota Decadal".

                WHEN 3 THEN
                    ASSIGN Plazo_de_solicitud:SCREEN-VALUE = STRING(ABS(Dias / 15))
                           W_NomPdo:SCREEN-VALUE = "Cuota Quincenal".

                WHEN 4 THEN
                    ASSIGN Plazo_de_solicitud:SCREEN-VALUE = STRING(ABS(Dias / 30))
                           W_NomPdo:SCREEN-VALUE = "Cuota Mensual".

                WHEN 5 THEN
                    ASSIGN Plazo_de_solicitud:SCREEN-VALUE = STRING(ABS(Dias / 60))
                           W_NomPdo:SCREEN-VALUE = "Cuota Bimensual".

                WHEN 6 THEN
                    ASSIGN Plazo_de_solicitud:SCREEN-VALUE = STRING(ABS(Dias / 90))
                           W_NomPdo:SCREEN-VALUE = "Cuota Trimestral".

                WHEN 7 THEN
                    ASSIGN Plazo_de_solicitud:SCREEN-VALUE = STRING(ABS(Dias / 120))
                           W_NomPdo:SCREEN-VALUE = "Cuota Cuatrimestral".

                WHEN 8 THEN
                    ASSIGN Plazo_de_solicitud:SCREEN-VALUE = STRING(ABS(Dias / 180))
                           W_NomPdo:SCREEN-VALUE = "Cuota Semestral".

                WHEN 9 THEN
                    ASSIGN Plazo_de_solicitud:SCREEN-VALUE = STRING(ABS(Dias / 360))
                           W_NomPdo:SCREEN-VALUE = "Cuota Anual".
            END CASE.
        END.

        vPeriodicidad = INTEGER(SUBSTRING(Cmb_PerPago:SCREEN-VALUE,1,1)).

        APPLY "leave" TO Plazo_de_solicitud.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Productos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Productos C-Win
ON VALUE-CHANGED OF Cmb_Productos IN FRAME DEFAULT-FRAME /* Producto */
DO:

    ASSIGN Total_del_prestamo:SCREEN-VALUE = "0".

    DO WITH FRAME F_Producto:
        IF SELF:SCREEN-VALUE NE "" THEN DO:
            FIND Pro_Creditos WHERE Pro_Creditos.Tip_Credito  EQ INTEGER(Tipo_de_credito:SCREEN-VALUE)
                                AND Pro_Creditos.Cod_Credito EQ INTEGER(SUBSTRING(SELF:SCREEN-VALUE,1,3)) NO-LOCK NO-ERROR.
            IF AVAILABLE Pro_Creditos THEN DO:
                ASSIGN codCreditoActivo = pro_creditos.cod_credito.
                RUN Llenar_InfoProducto.
            END.
        END.
    END.

    Cuota_de_solicitud:SCREEN-VALUE = "0".

    IF AVAILABLE Pro_Creditos THEN DO:
        IF Pro_Creditos.Id_Tasa EQ 2 THEN
            ASSIGN Tasa_de_solicitud:BGCOL = 15
                   Tasa_de_solicitud:FGCOL = 0
                   Tasa_de_solicitud:SENSITIVE = YES.
        ELSE
            ASSIGN Tasa_de_solicitud:BGCOL = 18
                   Tasa_de_solicitud:FGCOL = 15
                   Tasa_de_solicitud:SENSITIVE = NO.
    END.

    IF DEC(Monto_de_solicitud:SCREEN-VALUE) GT 0 THEN
        APPLY "LEAVE" TO Monto_de_solicitud.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Sistemas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Sistemas C-Win
ON VALUE-CHANGED OF Cmb_Sistemas IN FRAME DEFAULT-FRAME /* Sistema */
DO:
   Cuota_de_solicitud:SCREEN-VALUE = "0".

        DEFINE VARIABLE Sistema AS INTEGER FORMAT "99999".

        Sistema = INTEGER(SUBSTRING(Cmb_sistemas:SCREEN-VALUE,1,5)).

        APPLY "leave" TO Plazo_de_solicitud.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Fecha_de_desembolso
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Fecha_de_desembolso C-Win
ON LEAVE OF Fecha_de_desembolso IN FRAME DEFAULT-FRAME /* Fecha desembolso */
DO:
    IF Fecha_de_desembolso:SCREEN-VALUE = '' OR DATE(Fecha_de_desembolso:SCREEN-VALUE) < w_fecha THEN DO:
        MESSAGE "La fecha de desembolso debe ser igual o posterior"
                "al día de hoy. Revise por favor."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        Fecha_de_desembolso:SCREEN-VALUE = ''.
        APPLY 'entry' TO Fecha_de_desembolso.
        RETURN NO-APPLY.
    END.

    RUN liquidar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Fecha_de_primerPago
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Fecha_de_primerPago C-Win
ON LEAVE OF Fecha_de_primerPago IN FRAME DEFAULT-FRAME /* Fecha primer pago */
DO:
    IF Fecha_de_primerPago:SCREEN-VALUE = '' OR DATE(Fecha_de_primerPago:SCREEN-VALUE) <= w_fecha THEN DO:
        MESSAGE "La fecha de primer pago debe ser posterior"
                "al día de hoy. Revise por favor."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        Fecha_de_primerPago:SCREEN-VALUE = ''.
        APPLY 'entry' TO Fecha_de_primerPago.
        RETURN NO-APPLY.
    END.

    RUN liquidar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Forma_de_interes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Forma_de_interes C-Win
ON VALUE-CHANGED OF Forma_de_interes IN FRAME DEFAULT-FRAME
DO:
  Cuota_de_solicitud:SCREEN-VALUE = "0".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Monto_de_solicitud
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Monto_de_solicitud C-Win
ON LEAVE OF Monto_de_solicitud IN FRAME DEFAULT-FRAME /* Monto */
DO:
    DEFINE BUFFER bfrAhorros FOR ahorros.
    DEFINE BUFFER bfrCreditos FOR creditos.
    DEFINE VAR valorMaximoPermanencia AS DECIMAL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Monto_de_solicitud C-Win
ON VALUE-CHANGED OF Monto_de_solicitud IN FRAME DEFAULT-FRAME /* Monto */
DO:
  Cuota_de_solicitud:SCREEN-VALUE = "0".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Plazo_de_solicitud
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Plazo_de_solicitud C-Win
ON LEAVE OF Plazo_de_solicitud IN FRAME DEFAULT-FRAME /* Plazo */
DO:
    DEFINE VARIABLE W_Recoge AS LOGICAL INITIAL FALSE.
    DEFINE VAR fechaTemp AS DATE. 

    ASSIGN Dias = 0
           WFactorCod = 1.

    DO WITH FRAME Default-Frame:

        IF AVAIL Pro_Creditos THEN DO:
            ASSIGN Dias = DEC(SELF:SCREEN-VALUE)
                   vPeriodicidad = INT(SUBSTR(Cmb_PerPago:SCREEN-VALUE,1,1)).

            RUN CalcularPlazo.

            IF Plazo_de_solicitud:SCREEN-VALUE NE "0" THEN
                RUN Buscar_Indicadores.

            ASSIGN Wded = 0.

            ASSIGN WDed = 0.

            ASSIGN Total_del_prestamo:SCREEN-VALUE = Monto_de_solicitud:SCREEN-VALUE
                   W_VrADesemb = DEC(Total_del_prestamo:SCREEN-VALUE)
                   W_VrADesemb:SCREEN-VALUE = STRING(W_VrADesemb).
        END.
        ELSE DO:
            MESSAGE "Se debe escoger un producto de crédito" SKIP
                    "antes de ingresar el plazo. Rectifique"
                VIEW-AS ALERT-BOX INFORMATION.

            APPLY "entry" TO Cmb_Productos.
            RETURN.
        END.

        W_Recoge = FALSE.

        IF AVAIL Solicitud AND W_recoge = TRUE THEN DO:
            RUN Hallar_Tasa_Nominal.

            RUN Hallar_Tasa_Efectiva.
            RUN Hallar_Tasa_Periodo.
        END.
    END.

    /* Se asigna la fecha de desembolso y la fecha de primer pago como propuesta */
    IF Fecha_de_desembolso:SCREEN-VALUE = '' /*OR DATE(Fecha_de_desembolso:SCREEN-VALUE) < w_fecha*/ THEN DO:
        IF INTEGER(SUBSTRING(Cmb_Sistemas:SCREEN-VALUE,1,5)) <> 2 AND
           INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE,1,3)) <> 108 AND
           INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE,1,3)) <> 113 THEN
            ASSIGN Fecha_de_desembolso:SCREEN-VALUE = Fecha_Solicitud:SCREEN-VALUE.
        ELSE
            ASSIGN Fecha_de_desembolso:SCREEN-VALUE = Fecha_Solicitud:SCREEN-VALUE.
    END.

    IF Fecha_de_primerPago:SCREEN-VALUE = '' /*OR DATE(Fecha_de_primerPago:SCREEN-VALUE) <= w_fecha*/ THEN DO:
        IF INTEGER(SUBSTRING(Cmb_Sistemas:SCREEN-VALUE,1,5)) <> 2 AND
           INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE,1,3)) <> 108 AND
           INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE,1,3)) <> 113 THEN DO:

            fechaTemp = ADD-INTERVAL(DATE(Fecha_de_desembolso:SCREEN-VALUE),1,"months").

            IF DAY(fechaTemp) <= 15 THEN
                fechaTemp = DATE(MONTH(fechaTemp),10,YEAR(fechaTemp)).
            ELSE
                fechaTemp = DATE(MONTH(ADD-INTERVAL(fechaTemp,1,"months")),10,YEAR(ADD-INTERVAL(fechaTemp,1,"months"))).

            ASSIGN Fecha_de_primerPago:SCREEN-VALUE = STRING(fechaTemp,"99/99/9999").
        END.
        ELSE
            ASSIGN Fecha_de_primerPago:SCREEN-VALUE = STRING(ADD-INTERVAL(DATE(Fecha_Solicitud:SCREEN-VALUE),INTEGER(Plazo_de_solicitud:SCREEN-VALUE),"months"),"99/99/9999").
    END.
    /* ------------------------------------------------------------------------- */
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Plazo_de_solicitud C-Win
ON VALUE-CHANGED OF Plazo_de_solicitud IN FRAME DEFAULT-FRAME /* Plazo */
DO:
  Cuota_de_solicitud:SCREEN-VALUE = "0".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Tasa_de_solicitud
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Tasa_de_solicitud C-Win
ON LEAVE OF Tasa_de_solicitud IN FRAME DEFAULT-FRAME /* Tasa */
DO:
  RUN Hallar_Tasa_Nominal.
  RUN Hallar_Tasa_Periodo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Tasa_de_solicitud C-Win
ON VALUE-CHANGED OF Tasa_de_solicitud IN FRAME DEFAULT-FRAME /* Tasa */
DO:
 DO WITH FRAME Default-Frame:
  Cuota_de_solicitud:SCREEN-VALUE = "0".
 END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Tipo_de_credito
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Tipo_de_credito C-Win
ON MOUSE-SELECT-CLICK OF Tipo_de_credito IN FRAME DEFAULT-FRAME
DO:
 ASSIGN /*Cmb_PerPago:SENSITIVE = FALSE*/
        Cmb_Productos:LIST-ITEMS                   = "".
    
 FIND FIRST Pro_Creditos WHERE Pro_Creditos.Tip_Credito EQ INTEGER(SELF:SCREEN-VALUE) AND
                             Pro_Creditos.Estado  EQ 1 NO-LOCK NO-ERROR.
 
 IF NOT AVAIL(Pro_Creditos) THEN DO:
    W_Ok = Cmb_Productos:ADD-FIRST("No existen productos en esta línea").
    Cmb_Productos:SCREEN-VALUE = "No existen productos en esta línea".
 END.
 ELSE DO:
    W_Ok = Cmb_Productos:ADD-FIRST("Elige una línea de crédito").
    Cmb_Productos:SCREEN-VALUE = "Elige una línea de crédito".
 END.

 FOR EACH Pro_Creditos WHERE Pro_Creditos.Tip_Credito EQ INTEGER(SELF:SCREEN-VALUE) AND
                             Pro_Creditos.Estado  EQ 1 NO-LOCK BY pro_creditos.cod_credito:
    W_Ok = Cmb_Productos:ADD-LAST(STRING(Pro_Creditos.Cod_Credito,"999") + " - " + Pro_Creditos.Nom_Producto).
 END.

 IF INTEGER(SELF:SCREEN-VALUE) EQ 4 THEN
    ASSIGN Cmb_PerPago:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Tipo_de_credito C-Win
ON VALUE-CHANGED OF Tipo_de_credito IN FRAME DEFAULT-FRAME
DO:
DO WITH FRAME Default-Frame:
 Cmb_Productos:LIST-ITEMS = "".
 FOR EACH Pro_Creditos WHERE Pro_Creditos.Tip_Credito EQ INTEGER(SELF:SCREEN-VALUE) AND
                             Pro_Creditos.Estado  EQ 1 NO-LOCK BY pro_creditos.cod_credito:
     
     W_Ok = Cmb_Productos:ADD-LAST(STRING(Pro_Creditos.Cod_Credito,"999") + " - " + Pro_Creditos.Nom_Producto).
     Cmb_Productos:SCREEN-VALUE = STRING(Pro_Creditos.Cod_Credito,"999") + " - " + Pro_Creditos.Nom_Producto.
 END.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Extras
&Scoped-define SELF-NAME W_PPExtra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_PPExtra C-Win
ON LEAVE OF W_PPExtra IN FRAME F_Extras
DO:
  ASSIGN W_PPExtra.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_VrExtra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_VrExtra C-Win
ON LEAVE OF W_VrExtra IN FRAME F_Extras
DO:
  ASSIGN W_VrExtra.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  RUN Inicializar_variables.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Buscar_Indicadores C-Win 
PROCEDURE Buscar_Indicadores :
DEFINE BUFFER bfrCreditos FOR creditos.


    IF Pro_Creditos.Id_Tasa EQ 1 THEN DO:
        FIND FIRST Indicadores WHERE Indicadores.Indicador EQ Pro_Creditos.Cod_Tasa
                                 AND Indicadores.Estado EQ 1 NO-LOCK NO-ERROR.
        IF NOT AVAIL(Indicadores) THEN DO:
            MESSAGE "No existe un indicador para el plazo, monto y linea"  SKIP
                    "de producto de crédito. Consulte con el Administrador" SKIP
                    "del sistema acerca de esta inconsistencia"
                VIEW-AS ALERT-BOX ERROR TITLE "Configuración del Indicador".

            APPLY "ENTRY" TO Monto_de_solicitud IN FRAME Default-Frame.

            RETURN NO-APPLY.
        END.

        IF Indicadores.FecVcto LT w_fecha THEN DO:
            MESSAGE "El indicador para este producto se encuentra Vencido" SKIP
                    "la tasa puede estar desactualizada. Consulte con" SKIP
                    "el administrador del sistema acerca de esta inconsistencia"
                VIEW-AS ALERT-BOX ERROR TITLE "Configuración del Indicador".

            APPLY "ENTRY" TO Monto_de_solicitud.

            RETURN NO-APPLY.
        END.

        NomIndicador:SCREEN-VALUE = Indicadores.Nombre.

        IF NOT Indicadores.Rangos THEN DO:
            IF Indicadores.Tasa EQ 0 THEN DO:
                MESSAGE "El indicador tiene tasa en 0. No se permite radicar la Solicitud"
                    VIEW-AS ALERT-BOX ERROR.
            END.

            Tasa_de_solicitud:SCREEN-VALUE = STRING(Indicadores.Tasa).
        END.
        ELSE
            RUN Hallar_Rangos_Indicador.
    END.
    ELSE DO:
        IF Tasa_de_solicitud:SCREEN-VALUE = "0" THEN DO:
            MESSAGE "EL producto de crédito permite que el asesor" SKIP
                    "entre la tasa para la solicitud." SKIP(1)
                    "Digite la Tasa para la solicitud en pantalla"
                VIEW-AS ALERT-BOX INFORMATION.

            ASSIGN Tasa_de_solicitud:BGCOL = 15
                   Tasa_de_solicitud:FGCOL = 0
                   Tasa_de_solicitud:SENSITIVE = YES.

            APPLY "entry" TO Tasa_de_solicitud.

            RETURN NO-APPLY.
        END.
    END.

    RUN Hallar_Tasa_Nominal.
    RUN Hallar_Tasa_Periodo.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CalcularPlazo C-Win 
PROCEDURE CalcularPlazo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  CASE INT(SUBSTR(Cmb_PerPago:SCREEN-VALUE IN FRAME Default-Frame,1,1)):
      WHEN 0 THEN ASSIGN Dias = Dias * 1
                          W_NomPdo:SCREEN-VALUE = "Cuota diaria".
      WHEN 1 THEN ASSIGN Dias = Dias * 7 
                          W_NomPdo:SCREEN-VALUE = "Cuota semanal".
      WHEN 2 THEN ASSIGN Dias   = Dias * 10
                          W_NomPdo:SCREEN-VALUE = "Cuota decadal" NO-ERROR.
      WHEN 3 THEN ASSIGN Dias   = Dias * 15
                          W_NomPdo:SCREEN-VALUE = "Cuota quincenal" NO-ERROR.
      WHEN 4 THEN ASSIGN Dias   = Dias * 30
                          W_NomPdo:SCREEN-VALUE = "Cuota mensual" NO-ERROR.
      WHEN 5 THEN ASSIGN Dias   = Dias * 60
                          W_NomPdo:SCREEN-VALUE = "Cuota bimensual" NO-ERROR.
      WHEN 6 THEN ASSIGN Dias   = Dias * 90
                          W_NomPdo:SCREEN-VALUE = "Cuota trimestral" NO-ERROR.
      WHEN 7 THEN ASSIGN Dias   = Dias * 120
                          W_NomPdo:SCREEN-VALUE = "Cuota cuatrimestral" NO-ERROR.
      WHEN 8 THEN ASSIGN Dias   = Dias * 180
                          W_NomPdo:SCREEN-VALUE = "Cuota semestral" NO-ERROR.
      WHEN 9 THEN ASSIGN Dias   = Dias * 360
                          W_NomPdo:SCREEN-VALUE = "Cuota anual" NO-ERROR.
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY Fecha_Solicitud Tipo_de_credito S_InfoProducto Cmb_Productos 
          Monto_de_solicitud Forma_de_interes Tasa_de_solicitud 
          Plazo_de_solicitud Cmb_Sistemas W_TasaNominal Fecha_de_desembolso 
          Cmb_PerPago W_TasaPeriodo Fecha_de_primerPago Cuota_de_solicitud 
          NomIndicador Total_del_prestamo W_VrADesemb W_TotExt W_NomPdo 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-334 RECT-323 RECT-325 Fecha_Solicitud Tipo_de_credito 
         S_InfoProducto Cmb_Productos Monto_de_solicitud Forma_de_interes 
         Tasa_de_solicitud Plazo_de_solicitud Cmb_Sistemas W_TasaNominal 
         Fecha_de_desembolso Cmb_PerPago W_TasaPeriodo Fecha_de_primerPago 
         Btn_Proyectar Cuota_de_solicitud NomIndicador Total_del_prestamo 
         Btn_Ingresar W_VrADesemb W_TotExt BUTTON-2 Btn_Extras W_NomPdo 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY W_PPExtra W_VrExtra 
      WITH FRAME F_Extras IN WINDOW C-Win.
  ENABLE Btn_AdExt W_PPExtra W_VrExtra BUTTON-170 Btn_EliExt Br_Extras 
      WITH FRAME F_Extras IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-F_Extras}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Hallar_Tasa_Efectiva C-Win 
PROCEDURE Hallar_Tasa_Efectiva :
DEF VAR tas_efe AS DEC.
DEF VAR Periodo AS INT FORMAT "999".
  DO WITH FRAME Default-Frame:
     periodo = 12.
        RUN NVEF IN W_ManFin (INPUT(DEC(w_tasanominal:SCREEN-VALUE IN FRAME Default-Frame) / 100),Periodo,OUTPUT tasa1).
        Tasa_de_solicitud:SCREEN-VALUE = string(tasa1 * 100).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Hallar_Tasa_Nominal C-Win 
PROCEDURE Hallar_Tasa_Nominal :
DEF VAR Periodo AS INT FORMAT "999".
DEFI VAR PlazoW AS INTEG FORM "99".
DEFI VAR W_NroDias AS INTEG FORM "9999".
DEFI VAR P_NMeses AS DEC.
DEFI VAR P_NomPer AS CHAR FORMAT "X(15)".

DO WITH FRAME Default-Frame:
    RUN HallarPeriodo IN W_ManFin (INPUT INT(SUBSTR(Cmb_PerPago:SCREEN-VALUE IN FRAME Default-Frame,1,1)),
                                   INPUT  PlazoW,
                                   OUTPUT W_NroDias,
                                   OUTPUT P_NMeses,
                                   OUTPUT Periodo,
                                   OUTPUT P_NomPer).

    IF Forma_de_interes:SCREEN-VALUE EQ "1" THEN
        RUN EFNV IN W_ManFin (INPUT(DEC(Tasa_de_solicitud:SCREEN-VALUE) / 100),
                              INPUT Periodo,
                              OUTPUT Tas_Nominal).
    ELSE
        RUN EFNA IN W_ManFin (INPUT(DEC(Tasa_de_solicitud:SCREEN-VALUE) / 100),
                              INPUT Periodo,
                              OUTPUT Tas_Nominal).

    ASSIGN Tas_Nominal = ((Tas_Nominal * Periodo) * 100)
           W_TasaNominal:SCREEN-VALUE = STRING(Tas_Nominal).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Hallar_Tasa_Periodo C-Win 
PROCEDURE Hallar_Tasa_Periodo :
DEFINE VAR Tas_Periodo AS DECIMAL.

DO WITH FRAME Default-Frame:
    RUN HallarTasPer IN W_ManFin (INPUT INTEGER(SUBSTRING(Cmb_PerPago:SCREEN-VALUE IN FRAME Default-Frame,1,1)),
                                  INPUT DECIMAL(Tasa_de_solicitud:SCREEN-VALUE),
                                  INPUT INTEGER(Forma_de_interes:SCREEN-VALUE),
                                  OUTPUT Tas_Periodo).

    /* Para los créditos Corto Plazo y Emergencia el periodo de liquidación es diario */
    IF INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE,1,3)) = 108 OR
       INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE,1,3)) = 113 OR
       INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE,1,3)) = 114 THEN DO:

        RUN HallarTasPer IN W_ManFin (INPUT 0, /* Periodo de liquidación diaria */
                                      INPUT DEC(Tasa_de_solicitud:SCREEN-VALUE),
                                      INPUT INT(Forma_de_interes:SCREEN-VALUE),
                                      OUTPUT Tas_Periodo).

        w_tasaNominal = tas_periodo * 36000.
        w_tasaNominal:SCREEN-VALUE = STRING(w_tasaNominal,">>9.999999").
    END.

    Tas_Periodo = (Tas_Periodo * 100).
    W_TasaPeriodo:SCREEN-VALUE = STRING(Tas_Periodo).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informe_Imp C-Win 
PROCEDURE Informe_Imp :
DEFINE VAR W_Dispositivo AS CHARACTER INITIAL " ".
  DEFINE VAR W_sw          AS LOGICAL. 
  DEFI   VAR lista         AS CHAR FORM "X(35)".

  Lista = W_PathSpl + "AprobNeg-".
  
  RUN P-DisPos IN W_Manija (INPUT-OUTPUT Lista,INPUT-OUTPUT W_Dispositivo).
  IF W_Dispositivo = "" THEN
     RETURN.

  RUN _SetCurs.r ("WAIT").
  OUTPUT TO VALUE(Lista) NO-ECHO PAGED PAGE-SIZE 80.

  RUN Informe.

  OUTPUT CLOSE.        
  RUN _SetCurs.r ("ARROW").
  IF W_Dispositivo = "P" THEN  
    RUN Pantalla IN W_Manija (INPUT Lista).
  ELSE                                                  
    IF W_Dispositivo = "I" THEN
       RUN adecomm/_osprint.r ( INPUT  ?, INPUT Lista,INPUT  ?,INPUT  1,INPUT  1,
                                          INPUT  99999,OUTPUT W_sw).
  /*IF W_Dispositivo <> "A" THEN
     OS-DELETE VALUE(Lista).*/

  IF W_Dispositivo = "E" THEN
     RUN Imprimir_Excel.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar_Variables C-Win 
PROCEDURE Inicializar_Variables :
DO WITH FRAME Default-Frame:
    
        ASSIGN Cmb_Productos:LIST-ITEMS IN FRAME Default-Frame = ""
           Fecha_Solicitud:SCREEN-VALUE = STRING(w_fecha)
           Forma_de_interes:SCREEN-VALUE = "1"
           Monto_de_solicitud:SCREEN-VALUE = "0"
           Plazo_de_solicitud:SCREEN-VALUE = "0"
           Cuota_de_solicitud:SCREEN-VALUE = "0"
           W_VrADesemb:SCREEN-VALUE     = "0"
           W_VrADesemb
           Cmb_Perpago:SCREEN-VALUE = "4 - Mensual"
           Tasa_de_solicitud:SCREEN-VALUE = "0"
           W_TasaNominal:SCREEN-VALUE = "0"
           W_TasaPeriodo:SCREEN-VALUE = "0"
           Fecha_de_desembolso:SCREEN-VALUE = ''
           Fecha_de_primerPago:SCREEN-VALUE = ''
           Total_del_prestamo:SCREEN-VALUE = "0"
             W_NomPdo:SCREEN-VALUE                 = "Cuota"
             W_TotExt                              = 0
             W_TotExt:SCREEN-VALUE                 = "0".
          DO i = 1 TO 10 BY 1:
             W_Ok = S_InfoProducto:DELETE(1) IN FRAME Default-Frame.
          END.

          W_Ok = Cmb_Productos:ADD-FIRST("Elige un tipo de crédito").
          Cmb_Productos:SCREEN-VALUE = STRING("Elige un tipo de crédito").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject C-Win 
PROCEDURE initializeObject :
/*inicializa las variables*/
    
    DO WITH FRAME Default-frame:
        FOR EACH Pro_Creditos 
            WHERE 
                Pro_Creditos.Tip_Credito EQ Tipo_de_credito 
            AND Pro_Creditos.Estado  EQ 1 NO-LOCK:
            W_Ok = Cmb_Productos:ADD-LAST(STRING(Pro_Creditos.Cod_Credito,"999") + " - " + Pro_Creditos.Nom_Producto) IN FRAME Default-Frame.
        END.
        FOR EACH Varios 
            WHERE 
                Varios.Tipo EQ 20:
            W_Ok = Cmb_Sistemas:ADD-LAST(STRING(Varios.Codigo,"99999") + " - " + Varios.Descripcion).
        END.
        ASSIGN Total_del_prestamo:SCREEN-VALUE = "0".
        
        ASSIGN 
            W_NomPdo:SCREEN-VALUE                 = "Cuota"
            W_TotExt                              = 0
            W_TotExt:SCREEN-VALUE                 = "0".
        Cmb_Sistemas:SCREEN-VALUE = Cmb_Sistemas:ENTRY(1).
        RUN Inicializar_Variables.
    END.                             

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Liquidar C-Win 
PROCEDURE Liquidar :
DEFINE VARIABLE PlazoW LIKE Plazo_de_solicitud.
    DEFINE VARIABLE TotPtW LIKE Monto_de_solicitud.
    DEFINE VARIABLE CuotaW LIKE Cuota_de_solicitud.
    DEFINE VARIABLE TasaW LIKE Tasa_de_solicitud.
    DEFINE VARIABLE TinteW LIKE Monto_de_solicitud.
    DEFI VAR TVrPste LIKE Monto_de_solicitud.
    DEFI VAR VrPste LIKE Monto_de_solicitud.
    DEFI VAR Periodo AS INTEG FORM "99".
    DEFI VAR W_NroDias AS INTEG FORM "9999".
    DEFI VAR P_NMeses AS DEC.
    DEFI VAR P_NomPer AS CHAR FORMAT "X(15)".
    DEFINE VAR W_Resultado AS INTEGER INITIAL 0.
    DEFINE VAR Wimp_Coop AS INTEGER INITIAL 0.
    DEFINE VAR interesPreInicio AS DECIMAL.
    DEFINE VAR tasaPeriodo AS DECIMAL.
    DEFINE VAR diasPreInicio AS INTEGER.
    DEFINE VAR fechaInicio AS DATE.
    DEFINE VAR cont AS INTEGER.
    DEFINE VAR ultimaCuota AS DECIMAL.
    DEFINE VAR nroUltimaCuota AS INTEGER.

    DO WITH FRAME Default-Frame:
        ASSIGN TotPtW = DECIMAL(Total_del_prestamo:SCREEN-VALUE IN FRAME Default-Frame)
               TasaW = DECIMAL(Tasa_de_solicitud:SCREEN-VALUE)
               PlazoW = DECIMAL(Plazo_de_solicitud:SCREEN-VALUE)
               vPeriodicidad = INTEGER(SUBSTRING(Cmb_PerPago:SCREEN-VALUE,1,1))
               TVrPste = 0
               tasaPeriodo = DECIMAL(w_tasaPeriodo:SCREEN-VALUE).
    
        IF Pro_Creditos.Id_PerGracia AND Pro_Creditos.Dia_Gracia GT 0 THEN
            TotPtW = TotPtW + (((TotPtW * Tas_Nominal / 360) * Pro_Creditos.Dia_Gracia) / 100).
    
        RUN HallarPeriodo IN W_ManFin (INPUT INT(SUBSTR(Cmb_PerPago:SCREEN-VALUE,1,1)),
                                       INPUT PlazoW,
                                       OUTPUT W_NroDias,
                                       OUTPUT P_NMeses,
                                       OUTPUT Periodo,
                                       OUTPUT P_NomPer).
    
        IF W_TotExt GT 0 THEN DO:
            FOR EACH BTemp_Extras WHERE BTemp_extras.estado = 1 NO-LOCK BY BTemp_Extras.Nro_Cuota:
                RUN HPDF IN W_ManFin (INPUT BTemp_Extras.Vr_CuoExtra,
                                      INPUT (Tas_Nominal / (Periodo * 100)),
                                      INPUT BTemp_Extras.Nro_Cuota,
                                      OUTPUT VrPste).

                TVrPste = TVrPste + VrPste.
            END.

            TotPtW = TotPtW - TVrPste.
        END.
        
        /* Esto lo hacemos con el fin de distribuir los intereses adicionales a un periodo de forma que aumentando la cuota se puedan amortizar en el transcurso del crédito */
        IF INTEGER(SUBSTRING(Cmb_Sistemas:SCREEN-VALUE,1,5)) <> 2 AND
           INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE,1,3)) <> 108 AND
           INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE,1,3)) <> 113 THEN DO:
    
            fechaInicio = ADD-INTERVAL(DATE(Fecha_de_primerPago:SCREEN-VALUE),-1,"months").
            diasPreinicio = fechaInicio - DATE(Fecha_de_desembolso:SCREEN-VALUE).
    
            ASSIGN interesPreInicio = ((DECIMAL(Total_del_prestamo:SCREEN-VALUE) / 100) * (tasaPeriodo / 30) * diasPreinicio) WHEN diasPreinicio > 0.
            
            /*totPtw = totPtw + interesPreInicio.*/
        END.
    
        IF DATE(Fecha_Solicitud:SCREEN-VALUE) >= 02/12/2018 THEN DO:
            RUN Calculo_Cuota.R (INPUT-OUTPUT TotPtw,
                                 INPUT-OUTPUT PlazoW,
                                 INPUT-OUTPUT CuotaW,
                                 INPUT-OUTPUT TInteW,
                                 INPUT-OUTPUT TasaW,
                                 INPUT 0,
                                 INPUT 0,
                                 INPUT vPeriodicidad,
                                 INPUT 3,
                                 INPUT INT(Forma_de_interes:SCREEN-VALUE),
                                 INPUT INT(SUBSTR(Cmb_Sistemas:SCREEN-VALUE,1,5))).

            IF INTEGER(SUBSTRING(Cmb_Sistemas:SCREEN-VALUE,1,5)) <> 2 AND
               INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE,1,3)) <> 108 AND
               INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE,1,3)) <> 113 THEN DO:
                RUN devuelveUltimaCuota2.r(INPUT TotPtw /*DECIMAL(Monto_de_solicitud:SCREEN-VALUE)*/ ,
                                          INPUT PlazoW /*DECIMAL(Plazo_de_solicitud:SCREEN-VALUE)*/ ,
                                          INPUT-OUTPUT cuotaW,
                                          INPUT DATE(Fecha_de_desembolso:SCREEN-VALUE),
                                          INPUT DATE(Fecha_de_primerPago:SCREEN-VALUE),
                                          INPUT DECIMAL(W_TasaPeriodo:SCREEN-VALUE) / 100,
                                          INPUT INTEGER(SUBSTRING(Cmb_PerPago:SCREEN-VALUE,1,1)),
                                          INPUT INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE,1,3)),
                                          INPUT TABLE Temp_Extras).
            END.
        
            IF CuotaW LE 0 THEN DO:
                MESSAGE "El Valor de la cuota debe ser mayor a cero. Rectifique!"
                    VIEW-AS ALERT-BOX ERROR.
    
                APPLY "ENTRY" TO Cmb_Sistemas.
                RETURN ERROR.
            END.
    
            /* Redondeo a múltiplo de 100 */
            cuotaW = ROUND((cuotaW / 100) + 1,0) * 100.
    
            ASSIGN Cuota_de_solicitud:SCREEN-VALUE = STRING(CuotaW).
        END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Llenar_InfoProducto C-Win 
PROCEDURE Llenar_InfoProducto :
DEFINE VAR gtexto AS CHARACTER FORMAT "X(50)".
DO WITH FRAME Default-Frame:

  DO i = 1 TO 10:
     W_Ok = S_InfoProducto:DELETE(1) IN FRAME Default-Frame.
  END.

  IF Pro_Creditos.Id_MontoMinimo THEN DO:
     gtexto = "Monto mínimo: "  + STRING(Pro_Creditos.Val_MontoMinimo,">>>>>>>>9").
     W_Ok = S_InfoProducto:ADD-LAST(gtexto).
  END.
  IF Pro_Creditos.Id_MontoMaximo THEN DO:
     gtexto = "Monto máximo: "  + STRING(Pro_Creditos.Val_MontoMaximo,">>>>>>>>9").
     W_Ok = S_InfoProducto:ADD-LAST(gtexto).
  END.
  IF Pro_Creditos.Id_Plazo THEN DO:
     gtexto = "Plazo mínimo: "  + STRING(Pro_Creditos.Pla_Minimo / 30,">>>>>9").
     W_Ok = S_InfoProducto:ADD-LAST(gtexto).
     gtexto = "Plazo máximo: "  + STRING(Pro_Creditos.Pla_Maximo / 30,">>>>>9").
     W_Ok = S_InfoProducto:ADD-LAST(gtexto).
  END.
END.
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Solicitud C-Win 
PROCEDURE Mostrar_Solicitud :
DEF VAR x1 AS CHA FORMAT "X(40)".
DEF VAR J AS INT FORM "9".

DO WITH FRAME Default-Frame:
    
    ASSIGN Tipo_de_credito:SCREEN-VALUE IN FRAME Default-Frame = STRING(Tipo_de_credito).

    APPLY "value-changed" TO Tipo_de_credito.

    FIND FIRST Pro_Creditos WHERE Pro_Creditos.Tip_Credito EQ Tipo_de_credito NO-LOCK NO-ERROR.
    IF AVAIL Pro_Creditos THEN
        Cmb_Productos:SCREEN-VALUE = STRING(Pro_Creditos.Cod_Credito,"999") + " - " + Pro_Creditos.Nom_Producto.
    ELSE
        MESSAGE "No se encuentra el producto de créditos de la Solicitud"
            VIEW-AS ALERT-BOX ERROR.

    APPLY "value-changed" TO Cmb_Productos.

    DISPLAY Fecha_Solicitud
            Forma_de_interes
            Monto_de_solicitud
            Cuota_de_solicitud
            Tasa_de_solicitud
            Total_del_prestamo
            Fecha_de_desembolso
            Fecha_de_primerPago.

    CASE INT(SUBSTR(Cmb_PerPago:SCREEN-VALUE,1,1)):
        WHEN 0 THEN
            ASSIGN vPeriodicidad = 0
                   W_NomPdo:SCREEN-VALUE = "Cuota diaria"
                   Cmb_PerPago:SCREEN-VALUE = "0 - Diaria"
                   Plazo_de_solicitud:SCREEN-VALUE = STRING(INTEGER(Plazo_de_solicitud) / 360)
                   Dias = Plazo_de_solicitud * 1.

            WHEN 1 THEN
                ASSIGN vPeriodicidad = 1
                       W_NomPdo:SCREEN-VALUE = "Cuota semanal"
                       Cmb_PerPago:SCREEN-VALUE = "1 - Semanal"
                       Plazo_de_solicitud:SCREEN-VALUE = STRING(INTEGER(Plazo_de_solicitud) / 7)
                       Dias = Plazo_de_solicitud * 7.

            WHEN 2 THEN
                ASSIGN vPeriodicidad = 2
                       W_NomPdo:SCREEN-VALUE = "Cuota decadal"
                       Cmb_PerPago:SCREEN-VALUE = "2 - Decadal"
                       Plazo_de_solicitud:SCREEN-VALUE = STRING(INTEGER(Plazo_de_solicitud) / 10)
                       Dias = Plazo_de_solicitud * 10.

            WHEN 3 THEN
                ASSIGN vPeriodicidad = 3
                       W_NomPdo:SCREEN-VALUE = "Cuota quincenal"
                       Cmb_PerPago:SCREEN-VALUE = "3 - Quincenal"
                       Plazo_de_solicitud:SCREEN-VALUE = STRING(INTEGER(Plazo_de_solicitud) / 15)
                       Dias = Plazo_de_solicitud * 15.

            WHEN 4 THEN
                ASSIGN vPeriodicidad = 4
                       W_NomPdo:SCREEN-VALUE = "Cuota mensual"
                       Cmb_PerPago:SCREEN-VALUE = "4 - Mensual"
                       Plazo_de_solicitud:SCREEN-VALUE = STRING(INTEGER(Plazo_de_solicitud) / 30)
                       Dias = Plazo_de_solicitud * 30.

            WHEN 5 THEN
                ASSIGN vPeriodicidad = 5
                       W_NomPdo:SCREEN-VALUE = "Cuota bimensual"
                       Cmb_PerPago:SCREEN-VALUE = "5 - Bimestral"
                       Plazo_de_solicitud:SCREEN-VALUE = STRING(INTEGER(Plazo_de_solicitud) / 60)
                       Dias = Plazo_de_solicitud * 60.

            WHEN 6 THEN
                ASSIGN vPeriodicidad = 6
                       W_NomPdo:SCREEN-VALUE = "Cuota trimestral"
                       Cmb_PerPago:SCREEN-VALUE = "6 - Trimestral"
                       Plazo_de_solicitud:SCREEN-VALUE = STRING(INTEGER(Plazo_de_solicitud) / 90)
                       Dias = Plazo_de_solicitud * 90.

            WHEN 7 THEN
                ASSIGN vPeriodicidad = 7
                       W_NomPdo:SCREEN-VALUE = "Cuota cuatrimestral"
                       Cmb_PerPago:SCREEN-VALUE = "7 - Cuatrimestral"
                       Plazo_de_solicitud:SCREEN-VALUE = STRING(INTEGER(Plazo_de_solicitud) / 120)
                       Dias = Plazo_de_solicitud * 120.

            WHEN 8 THEN
                ASSIGN vPeriodicidad = 8
                       W_NomPdo:SCREEN-VALUE = "Cuota semestral"
                       Cmb_PerPago:SCREEN-VALUE = "8 - Semestral"
                       Plazo_de_solicitud:SCREEN-VALUE = STRING(INTEGER(Plazo_de_solicitud) / 180)
                       Dias = Plazo_de_solicitud * 180.

            WHEN 9 THEN
                ASSIGN vPeriodicidad = 9
                       W_NomPdo:SCREEN-VALUE = "Cuota anual"
                       Cmb_PerPago:SCREEN-VALUE = "9 - Anual"
                       Plazo_de_solicitud:SCREEN-VALUE = STRING(INTEGER(Plazo_de_solicitud) / 360)
                       Dias = Plazo_de_solicitud * 360.
        END CASE.

        APPLY "leave" TO Plazo_de_solicitud.

        W_TotExt = 0.

        FOR EACH Temp_Extras NO-LOCK:
            IF Temp_Extras.Estado EQ 1 THEN
                ASSIGN W_TotExt = W_TotExt + Temp_Extras.Vr_CuoExtra
                       W_TotExt:SCREEN-VALUE = STRING(W_TotExt).
        END.

        IF Tipo_de_credito EQ 4 THEN
            Cmb_PerPago:SENSITIVE = TRUE.

        IF W_VrADesemb LE 0 THEN
            MESSAGE "El Valor a Desembolsar es Negativo o Cero(0), Debe Revisar los Valores a Cancelar."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir C-Win 
PROCEDURE ProcesoImprimir :
RUN Proyeccion.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proyeccion C-Win 
PROCEDURE Proyeccion :
DEFINE VAR W_NomPer AS CHARACTER FORMAT "X(15)".
DEFINE VAR W_PdoAno AS INTEGER.
DEFINE VAR plazow AS INTEGER.
DEFINE VAR W_NroDias AS INTEGER.
DEFINE VAR P_NMeses AS INTEGER.

RUN HallarPeriodo IN W_ManFin (INPUT INT(SUBSTR(Cmb_PerPago:SCREEN-VALUE IN FRAME Default-Frame,1,1)),
                               INPUT PlazoW,
                               OUTPUT W_NroDias,
                               OUTPUT P_NMeses,
                               OUTPUT W_PdoAno,
                               OUTPUT W_NomPer).

RUN Proyeccion_Credito2.R(INPUT DECIMAL(Monto_de_solicitud:SCREEN-VALUE),
                         INPUT DECIMAL(Plazo_de_solicitud:SCREEN-VALUE),
                         INPUT DECIMAL(Cuota_de_solicitud:SCREEN-VALUE),
                         INPUT DATE(Fecha_de_desembolso:SCREEN-VALUE),
                         INPUT DATE(Fecha_de_primerPago:SCREEN-VALUE),
                         INPUT DECIMAL(W_TasaPeriodo:SCREEN-VALUE) / 100,
                         INPUT INTEGER(SUBSTRING(Cmb_PerPago:SCREEN-VALUE,1,1)),
                         INPUT INTEGER(SUBSTRING(Cmb_Sistemas:SCREEN-VALUE,1,5)),
                         INPUT INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE,1,3)),
                         INPUT SUBSTRING(Cmb_Productos:SCREEN-VALUE,7,30),
                         INPUT "S",
                         INPUT SUBSTRING(Cmb_Sistemas:SCREEN-VALUE,9,30),
                         INPUT DECIMAL(Monto_de_solicitud:SCREEN-VALUE),
                         INPUT TABLE Temp_Extras).

W_TipoInforme = "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TmpL C-Win 
PROCEDURE TmpL :
DEFINE INPUT PARAMETER Linea AS INTEGER FORMAT "99".
DEFINE INPUT PARAMETER Texto AS CHARACTER FORMAT "X(125)".

CREATE TmpI.
ASSIGN TmpI.ILinea = Linea
       TmpI.ITexto = Texto.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Validar_Solicitud C-Win 
PROCEDURE Validar_Solicitud :
/* Validamos el monto/plazo mínimo y el monto máximo/plazo */
DEFINE VAR plazoMeses AS INTEGER.

CASE INTEGER(SUBSTRING(cmb_perPago:SCREEN-VALUE IN FRAME Default-Frame,1,1)):
    WHEN 0 THEN plazoMeses = DECIMAL(Plazo_de_solicitud:SCREEN-VALUE) / 30.
    WHEN 1 THEN plazoMeses = (DECIMAL(Plazo_de_solicitud:SCREEN-VALUE) * 7) / 30.
    WHEN 2 THEN plazoMeses = (DECIMAL(Plazo_de_solicitud:SCREEN-VALUE) * 10) / 30.
    WHEN 3 THEN plazoMeses = (DECIMAL(Plazo_de_solicitud:SCREEN-VALUE) * 15) / 30.
    WHEN 4 THEN plazoMeses = DECIMAL(Plazo_de_solicitud:SCREEN-VALUE).
    WHEN 5 THEN plazoMeses = DECIMAL(Plazo_de_solicitud:SCREEN-VALUE) * 2.
    WHEN 6 THEN plazoMeses = DECIMAL(Plazo_de_solicitud:SCREEN-VALUE) * 3.
    WHEN 7 THEN plazoMeses = DECIMAL(Plazo_de_solicitud:SCREEN-VALUE) * 4.
    WHEN 8 THEN plazoMeses = DECIMAL(Plazo_de_solicitud:SCREEN-VALUE) * 6.
    WHEN 9 THEN plazoMeses = DECIMAL(Plazo_de_solicitud:SCREEN-VALUE) * 12.
END CASE.


IF AVAILABLE pro_creditos THEN DO:
    IF pro_creditos.id_montoMinimo = TRUE THEN DO:
        IF DECIMAL(Monto_de_solicitud:SCREEN-VALUE) < pro_creditos.val_montoMinimo THEN DO:
            MESSAGE "El monto solicitado es inferior al monto mínimo permitido para este producto." STRING(pro_creditos.val_MontoMinimo,"$->>>,>>>,>>>,>>9.99") SKIP
                    "Revise las condiciones de la solicitud."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            
            RETURN ERROR.
        END.
    END.

    IF pro_creditos.id_montoMaximo = TRUE THEN DO:
        IF DECIMAL(Monto_de_solicitud:SCREEN-VALUE) > pro_creditos.val_montoMaximo THEN DO:
            MESSAGE "El monto solicitado es superior al monto máximo permitido para este producto." STRING(pro_creditos.val_montoMaximo,"$->>>,>>>,>>>,>>9.99") SKIP
                    "Revise las condiciones de la solicitud."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            
            RETURN ERROR.
        END.
    END.

    IF pro_creditos.id_plazo = TRUE THEN DO:
        IF plazoMeses < pro_creditos.pla_minimo THEN DO:
            MESSAGE "El plazo (en meses) solicitado es inferior al plazo mínimo permitido para este producto." STRING(pro_creditos.pla_minimo,"$->>>,>>>,>>>,>>9.99") SKIP
                    "Revise las condiciones de la solicitud."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            
            RETURN ERROR.
        END.
    END.

    IF pro_creditos.id_plazo = TRUE THEN DO:
        IF plazoMeses > pro_creditos.pla_maximo THEN DO:
            MESSAGE "EL plazo (en meses) solicitado es  superior al plazo máximo permitido para este producto." STRING(pro_creditos.pla_maximo,"$->>>,>>>,>>>,>>9.99")SKIP
                    "Revise las condiciones de la solicitud."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            
            RETURN ERROR.
        END.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

