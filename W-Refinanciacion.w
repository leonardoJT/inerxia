&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/* oakley */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{INCLUIDO\VARIABLE.I "SHARED"}

DEFINE VAR w_age AS INTEGER.

DEFINE TEMP-TABLE TT_Creditos
    FIELD nit AS CHARACTER
    FIELD Agencia AS INTEGER
    FIELD fec_desmbolso AS DATE
    FIELD cod_credito AS INTEGER
    FIELD num_credito AS INTEGER
    FIELD nombre_linea AS CHARACTER
    FIELD Cuota AS DECIMAL
    FIELD sdo_capital AS DECIMAL
    FIELD INT_corriente AS DECIMAL
    FIELD fec_pago AS DATE
    FIELD plazo AS INTEGER
    FIELD tasa AS DECIMAL
    FIELD fec_desembolso AS DATE.

DEFINE VAR w_perDed AS INTEGER.
DEFINE VAR w_InterPlazo AS DECIMAL.
DEFINE VAR Tas_Nominal AS DECIMAL.
DEFINE VAR W_TotExt AS DECIMAL.
DEFINE VAR w_razon AS INTEGER.
DEFINE VAR w_gracia AS INTEGER.
DEFINE VAR flagControlPagos AS LOGICAL.

DEFINE TEMP-TABLE tc LIKE TT_Creditos.

DEFINE VAR vTime AS INTEGER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Creditos
&Scoped-define BROWSE-NAME Br_Pdctos

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TT_Creditos

/* Definitions for BROWSE Br_Pdctos                                     */
&Scoped-define FIELDS-IN-QUERY-Br_Pdctos TT_Creditos.Agencia TT_Creditos.cod_credito TT_Creditos.nombre_linea TT_Creditos.fec_desembolso TT_Creditos.num_credito TT_Creditos.Cuota TT_Creditos.sdo_capital TT_Creditos.Int_corriente TT_Creditos.fec_pago TT_Creditos.plazo TT_creditos.tasa   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Pdctos   
&Scoped-define SELF-NAME Br_Pdctos
&Scoped-define QUERY-STRING-Br_Pdctos FOR EACH TT_Creditos NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br_Pdctos OPEN QUERY Br_Pdctos FOR EACH TT_Creditos NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_Pdctos TT_Creditos
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Pdctos TT_Creditos


/* Definitions for FRAME F_Productos                                    */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Productos ~
    ~{&OPEN-QUERY-Br_Pdctos}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-2 RECT-3 RECT-4 RECT-5 W_NitCte btnTasa ~
btnCuota btnPlazo btnFecPago btnFecPrimerPago btnPeriodicidadDePago ~
btnFormaDePago tgCongelado BUTTON-2 
&Scoped-Define DISPLAYED-OBJECTS W_NitCte W_NomCte tasaActual nuevaTasa ~
CuotaActual nuevaCuota PlazoActual NuevoPlazo fecPagoActual nuevaFecPago ~
fecPrimerPagoActual nuevaFecPrimerPago rsPeriodicidad rsFormaDePago ~
tgCongelado 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-2 Btn_Salvar 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCuota 
     LABEL "Cuota Actual / Nueva Cuota" 
     SIZE 20 BY .88.

DEFINE BUTTON btnFecPago 
     LABEL "Fecha de próximo pago" 
     SIZE 20 BY .88.

DEFINE BUTTON btnFecPrimerPago 
     LABEL "Fecha de primer pago" 
     SIZE 20 BY .88.

DEFINE BUTTON btnFormaDePago 
     LABEL "Forma de pago" 
     SIZE 20 BY .88.

DEFINE BUTTON btnImpProyeccion 
     IMAGE-UP FILE "Imagenes/impresora2.bmp":U
     LABEL "Imprimir Proyección" 
     SIZE 10 BY 1.62.

DEFINE BUTTON btnPeriodicidadDePago 
     LABEL "Periodicidad de pago" 
     SIZE 20 BY .88.

DEFINE BUTTON btnPlazo 
     LABEL "Plazo Actual / Nuevo Plazo" 
     SIZE 20 BY .88.

DEFINE BUTTON btnTasa 
     LABEL "Tasa Actual / Nueva Tasa" 
     SIZE 20 BY .88.

DEFINE BUTTON Btn_Salvar 
     IMAGE-UP FILE "Imagenes/disque.jpg":U
     LABEL "Salvar" 
     SIZE 10 BY 1.62.

DEFINE BUTTON BUTTON-2 
     LABEL "Salir" 
     SIZE 10 BY 1.62.

DEFINE VARIABLE CuotaActual AS DECIMAL FORMAT ">>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE VARIABLE fecPagoActual AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE VARIABLE fecPrimerPagoActual AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE VARIABLE nuevaCuota AS DECIMAL FORMAT ">>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE VARIABLE nuevaFecPago AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE VARIABLE nuevaFecPrimerPago AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE VARIABLE nuevaTasa AS DECIMAL FORMAT ">>9.999999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE VARIABLE NuevoPlazo AS INTEGER FORMAT ">>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE VARIABLE PlazoActual AS INTEGER FORMAT ">>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE VARIABLE tasaActual AS DECIMAL FORMAT ">>9.999999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE VARIABLE W_NitCte AS CHARACTER FORMAT "X(12)":U 
     LABEL "Ced/Nit" 
     VIEW-AS FILL-IN 
     SIZE 14.72 BY .85 TOOLTIP "Ced/Nit del Cliente, Con doble Click lo Captura."
     BGCOLOR 15 FGCOLOR 0 FONT 15 NO-UNDO.

DEFINE VARIABLE W_NomCte AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 83.14 BY .85 TOOLTIP "Nombre del Cliente"
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE rsFormaDePago AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Caja", 1,
"Nómina", 2
     SIZE 12 BY 1.08 NO-UNDO.

DEFINE VARIABLE rsPeriodicidad AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Mensual", 4,
"Trimestral", 6,
"Semestral", 8,
"Anual", 9
     SIZE 18 BY 2 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 31.14 BY 2.15.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 41 BY 2.42.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 41 BY 1.58.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 41 BY 1.

DEFINE VARIABLE tgCongelado AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .77 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Br_Pdctos FOR 
      TT_Creditos SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Br_Pdctos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Pdctos C-Win _FREEFORM
  QUERY Br_Pdctos DISPLAY
      TT_Creditos.Agencia           COLUMN-LABEL "Ag" FORMAT "99"
      TT_Creditos.cod_credito       COLUMN-LABEL "Cod" FORMAT "999"
      TT_Creditos.nombre_linea      COLUMN-LABEL "Línea de Crédito" FORMAT "X(30)"
      TT_Creditos.fec_desembolso    COLUMN-LABEL "Fec_desembolso" FORMAT "99/99/9999"
      TT_Creditos.num_credito       COLUMN-LABEL "Num_Crédito" FORMAT ">>>>>>>>>"
      TT_Creditos.Cuota             COLUMN-LABEL "Vlr_CUota" FORMAT "$>>>,>>>,>>9"
      TT_Creditos.sdo_capital       COLUMN-LABEL "Saldo Actual" FORMAT "$>>>,>>>,>>9"
      TT_Creditos.Int_corriente     COLUMN-LABEL "Int_corriente" FORMAT "$>>>,>>>,>>9"
      TT_Creditos.fec_pago          COLUMN-LABEL "Fec_pago" FORMAT "99/99/9999"
      TT_Creditos.plazo             COLUMN-LABEL "Plazo" FORMAT ">999"
      TT_creditos.tasa              COLUMN-LABEL "Tasa" FORMAT ">>9.999999%"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 104.86 BY 4.58
         FONT 15 ROW-HEIGHT-CHARS .5 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Creditos
     W_NitCte AT ROW 1.27 COL 7.29 COLON-ALIGNED WIDGET-ID 4
     W_NomCte AT ROW 1.27 COL 22.86 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     btnTasa AT ROW 8 COL 67.72 WIDGET-ID 68
     tasaActual AT ROW 8 COL 86.29 COLON-ALIGNED NO-LABEL WIDGET-ID 64
     nuevaTasa AT ROW 8 COL 96.14 COLON-ALIGNED NO-LABEL WIDGET-ID 66
     btnCuota AT ROW 9 COL 67.72 WIDGET-ID 70
     CuotaActual AT ROW 9 COL 86.29 COLON-ALIGNED NO-LABEL WIDGET-ID 72
     nuevaCuota AT ROW 9 COL 96.14 COLON-ALIGNED NO-LABEL WIDGET-ID 74
     btnPlazo AT ROW 10.04 COL 67.72 WIDGET-ID 76
     PlazoActual AT ROW 10.04 COL 86.29 COLON-ALIGNED NO-LABEL WIDGET-ID 78
     NuevoPlazo AT ROW 10.04 COL 96.14 COLON-ALIGNED NO-LABEL WIDGET-ID 80
     btnFecPago AT ROW 11.08 COL 67.72 WIDGET-ID 82
     fecPagoActual AT ROW 11.08 COL 86.29 COLON-ALIGNED NO-LABEL WIDGET-ID 86
     nuevaFecPago AT ROW 11.08 COL 96.14 COLON-ALIGNED NO-LABEL WIDGET-ID 84
     btnFecPrimerPago AT ROW 12.12 COL 67.72 WIDGET-ID 88
     fecPrimerPagoActual AT ROW 12.12 COL 86.29 COLON-ALIGNED NO-LABEL WIDGET-ID 90
     nuevaFecPrimerPago AT ROW 12.12 COL 96.14 COLON-ALIGNED NO-LABEL WIDGET-ID 92
     rsPeriodicidad AT ROW 13.35 COL 89 NO-LABEL WIDGET-ID 96
     btnPeriodicidadDePago AT ROW 13.73 COL 67.72 WIDGET-ID 94
     rsFormaDePago AT ROW 15.92 COL 89.14 NO-LABEL WIDGET-ID 106
     btnFormaDePago AT ROW 16 COL 67.72 WIDGET-ID 104
     tgCongelado AT ROW 17.54 COL 89 WIDGET-ID 112
     Btn_Salvar AT ROW 18.88 COL 76.72 WIDGET-ID 34
     btnImpProyeccion AT ROW 18.88 COL 86.57 WIDGET-ID 56
     BUTTON-2 AT ROW 18.88 COL 96.43 WIDGET-ID 38
     "Crédito congelado" VIEW-AS TEXT
          SIZE 13 BY .54 AT ROW 17.62 COL 75 WIDGET-ID 114
     RECT-2 AT ROW 18.62 COL 76 WIDGET-ID 40
     RECT-3 AT ROW 13.12 COL 67 WIDGET-ID 100
     RECT-4 AT ROW 15.69 COL 67 WIDGET-ID 102
     RECT-5 AT ROW 17.38 COL 67 WIDGET-ID 110
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 108.14 BY 20.85
         FONT 15 WIDGET-ID 100.

DEFINE FRAME F_Productos
     Br_Pdctos AT ROW 1.08 COL 1.43 WIDGET-ID 200
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 2.35
         SIZE 106 BY 5.62
         FONT 15
         TITLE "Créditos Vigentes" WIDGET-ID 200.


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
         TITLE              = "Refinanciación de Obligaciones"
         HEIGHT             = 20.08
         WIDTH              = 108
         MAX-HEIGHT         = 26.77
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 26.77
         VIRTUAL-WIDTH      = 146.29
         MAX-BUTTON         = no
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
ASSIGN FRAME F_Productos:FRAME = FRAME F_Creditos:HANDLE.

/* SETTINGS FOR FRAME F_Creditos
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON btnImpProyeccion IN FRAME F_Creditos
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON Btn_Salvar IN FRAME F_Creditos
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN CuotaActual IN FRAME F_Creditos
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fecPagoActual IN FRAME F_Creditos
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fecPrimerPagoActual IN FRAME F_Creditos
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nuevaCuota IN FRAME F_Creditos
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nuevaFecPago IN FRAME F_Creditos
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nuevaFecPrimerPago IN FRAME F_Creditos
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nuevaTasa IN FRAME F_Creditos
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN NuevoPlazo IN FRAME F_Creditos
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN PlazoActual IN FRAME F_Creditos
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET rsFormaDePago IN FRAME F_Creditos
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET rsPeriodicidad IN FRAME F_Creditos
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tasaActual IN FRAME F_Creditos
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomCte IN FRAME F_Creditos
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Productos
                                                                        */
/* BROWSE-TAB Br_Pdctos 1 F_Productos */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_Pdctos
/* Query rebuild information for BROWSE Br_Pdctos
     _START_FREEFORM
OPEN QUERY Br_Pdctos FOR EACH TT_Creditos NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE Br_Pdctos */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Refinanciación de Obligaciones */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Refinanciación de Obligaciones */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br_Pdctos
&Scoped-define FRAME-NAME F_Productos
&Scoped-define SELF-NAME Br_Pdctos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Pdctos C-Win
ON MOUSE-SELECT-DBLCLICK OF Br_Pdctos IN FRAME F_Productos
DO:
    DEFINE VAR diasPeriodo AS INTEGER.
    DEFINE VAR P_NMeses AS INTEGER.
    DEFINE VAR numPeriodos AS INTEGER.
    DEFINE VAR P_NomPer AS CHARACTER.
    DEFINE VAR vTasa AS DECIMAL.

    IF AVAILABLE TT_Creditos THEN DO:
        FIND FIRST creditos WHERE creditos.nit = tt_creditos.nit
                              AND creditos.num_credito = tt_creditos.num_credito NO-ERROR.

        RUN HallarPeriodo IN W_ManFin (INPUT Creditos.Per_Pago,
                                       INPUT Creditos.Plazo,
                                       OUTPUT diasPeriodo,
                                       OUTPUT P_NMeses,
                                       OUTPUT numPeriodos,
                                       OUTPUT P_NomPer).

        RUN NVEF IN w_manfin (INPUT creditos.tasa / 100,
                              INPUT numPeriodos,
                              OUTPUT vTasa).

        tasaActual:SCREEN-VALUE IN FRAME F_creditos = STRING(vTasa * 100).
        nuevaTasa:SCREEN-VALUE = STRING(vTasa * 100).

        cuotaActual:SCREEN-VALUE IN FRAME F_creditos = STRING(creditos.cuota).
        nuevaCuota:SCREEN-VALUE = STRING(creditos.cuota).

        plazoActual:SCREEN-VALUE IN FRAME F_creditos = STRING(creditos.plazo).
        nuevoPlazo:SCREEN-VALUE = STRING(creditos.plazo).

        fecPagoActual:SCREEN-VALUE IN FRAME F_creditos = STRING(creditos.fec_pago,"99/99/9999").
        nuevaFecPago:SCREEN-VALUE = STRING(creditos.fec_pago,"99/99/9999").

        fecPrimerPagoActual:SCREEN-VALUE IN FRAME F_creditos = STRING(creditos.fec_pagAnti,"99/99/9999").
        nuevaFecPrimerPago:SCREEN-VALUE = STRING(creditos.fec_pagAnti,"99/99/9999").

        rsPeriodicidad:SCREEN-VALUE IN FRAME F_creditos = STRING(creditos.per_pago).
        
        nuevaTasa:SENSITIVE = FALSE.
        nuevaCuota:SENSITIVE = FALSE.
        nuevoPlazo:SENSITIVE = FALSE.
        nuevaFecPago:SENSITIVE = FALSE.
        nuevaFecPrimerPago:SENSITIVE = FALSE.
        rsPeriodicidad:SENSITIVE = FALSE.
        rsFormaDePago:SENSITIVE = FALSE.
        tgCongelado:SENSITIVE = TRUE.
        
        IF creditos.cod_credito = 123 THEN DO:
            btnTasa:SENSITIVE = FALSE.
            btnCuota:SENSITIVE = FALSE.
            btnPlazo:SENSITIVE = TRUE.
            btnfecPrimerPago:SENSITIVE = FALSE.
            btnFecPrimerPago:SENSITIVE = TRUE.
            btnPeriodicidadDePago:SENSITIVE = FALSE.
            btnFormaDePago:SENSITIVE = FALSE.
        END.
        ELSE DO:
            IF creditos.cod_credito <> 108 AND
               creditos.cod_credito <> 113 AND
               creditos.cod_credito <> 114 THEN DO:
                btnTasa:SENSITIVE = TRUE.
                btnCuota:SENSITIVE = TRUE.
                btnPlazo:SENSITIVE = TRUE.
                btnFecPago:SENSITIVE = TRUE.
                btnfecPrimerPago:SENSITIVE = TRUE.
                btnPeriodicidadDePago:SENSITIVE = TRUE.
                btnFormaDePago:SENSITIVE = TRUE.
            END.
            ELSE DO:
                btnTasa:SENSITIVE = FALSE.
                btnCuota:SENSITIVE = FALSE.
                btnPlazo:SENSITIVE = FALSE.
                btnFecPago:SENSITIVE = TRUE.
                btnfecPrimerPago:SENSITIVE = FALSE.
                btnPeriodicidadDePago:SENSITIVE = FALSE.
                btnFormaDePago:SENSITIVE = TRUE.
            END.
        END.

        rsFormaDePago:SCREEN-VALUE = STRING(creditos.FOR_pago).
        
        IF creditos.detalle_Estado = 2 THEN
            tgCongelado:SCREEN-VALUE = "yes".
        ELSE
            tgCongelado:SCREEN-VALUE = "no".

        btn_salvar:SENSITIVE = TRUE.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Creditos
&Scoped-define SELF-NAME btnCuota
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCuota C-Win
ON CHOOSE OF btnCuota IN FRAME F_Creditos /* Cuota Actual / Nueva Cuota */
DO:
    ASSIGN nuevaTasa:SCREEN-VALUE = tasaActual:SCREEN-VALUE WHEN AVAILABLE creditos.
    nuevaTasa:SENSITIVE = FALSE.

    ASSIGN nuevoPlazo:SCREEN-VALUE = plazoActual:SCREEN-VALUE WHEN AVAILABLE creditos.
    nuevoPlazo:SENSITIVE = FALSE.

    ASSIGN nuevaFecPago:SCREEN-VALUE = fecPagoActual:SCREEN-VALUE WHEN AVAILABLE creditos.
    nuevaFecPago:SENSITIVE = FALSE.

    ASSIGN nuevaFecPrimerPago:SCREEN-VALUE = fecPrimerPagoActual:SCREEN-VALUE WHEN AVAILABLE creditos.
    nuevaFecPrimerPago:SENSITIVE = FALSE.

    ASSIGN rsPeriodicidad:SCREEN-VALUE = STRING(creditos.per_pago) WHEN AVAILABLE creditos.
    rsPeriodicidad:SENSITIVE = FALSE.

    ASSIGN rsFormaDePago:SCREEN-VALUE = STRING(creditos.FOR_pago) WHEN AVAILABLE creditos.
    rsFormaDePago:SENSITIVE = FALSE.

    ASSIGN nuevaCuota:SENSITIVE = TRUE WHEN AVAILABLE Creditos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFecPago
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFecPago C-Win
ON CHOOSE OF btnFecPago IN FRAME F_Creditos /* Fecha de próximo pago */
DO:
    IF creditos.cuo_pagadas = 0 AND (creditos.cod_credito <> 108 AND
                                     creditos.cod_credito <> 113 AND
                                     creditos.cod_credito <> 114) THEN DO:
        MESSAGE "No se permite esta operación ya que el crédito no registra" SKIP
                "registra cuotas pagadas. Use la opción 'Fecha de primer pago'."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN NO-APPLY.
    END.

    ASSIGN nuevaTasa:SCREEN-VALUE = tasaActual:SCREEN-VALUE WHEN AVAILABLE creditos.
    nuevaTasa:SENSITIVE = FALSE.

    ASSIGN nuevoPlazo:SCREEN-VALUE = plazoActual:SCREEN-VALUE WHEN AVAILABLE creditos.
    nuevoPlazo:SENSITIVE = FALSE.

    ASSIGN nuevaCuota:SCREEN-VALUE = cuotaActual:SCREEN-VALUE WHEN AVAILABLE creditos.
    nuevaCuota:SENSITIVE = FALSE.

    ASSIGN nuevaFecPrimerPago:SCREEN-VALUE = fecPrimerPagoActual:SCREEN-VALUE WHEN AVAILABLE creditos.
    nuevaFecPrimerPago:SENSITIVE = FALSE.

    ASSIGN rsPeriodicidad:SCREEN-VALUE = STRING(creditos.per_pago) WHEN AVAILABLE creditos.
    rsPeriodicidad:SENSITIVE = FALSE.

    ASSIGN rsFormaDePago:SCREEN-VALUE = STRING(creditos.FOR_pago) WHEN AVAILABLE creditos.
    rsFormaDePago:SENSITIVE = FALSE.

    ASSIGN nuevaFecPago:SENSITIVE = TRUE WHEN AVAILABLE Creditos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFecPrimerPago
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFecPrimerPago C-Win
ON CHOOSE OF btnFecPrimerPago IN FRAME F_Creditos /* Fecha de primer pago */
DO:
    IF creditos.cuo_pagadas > 0 THEN DO:
        MESSAGE "No se permite esta operación ya que el crédito registra" SKIP
                "cuotas pagadas"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN NO-APPLY.
    END.

    ASSIGN nuevaTasa:SCREEN-VALUE = tasaActual:SCREEN-VALUE WHEN AVAILABLE creditos.
    nuevaTasa:SENSITIVE = FALSE.

    ASSIGN nuevoPlazo:SCREEN-VALUE = plazoActual:SCREEN-VALUE WHEN AVAILABLE creditos.
    nuevoPlazo:SENSITIVE = FALSE.

    ASSIGN nuevaCuota:SCREEN-VALUE = cuotaActual:SCREEN-VALUE WHEN AVAILABLE creditos.
    nuevaCuota:SENSITIVE = FALSE.

    ASSIGN nuevaFecPago:SCREEN-VALUE = fecPagoActual:SCREEN-VALUE WHEN AVAILABLE creditos.
    nuevaFecPago:SENSITIVE = FALSE.

    ASSIGN rsPeriodicidad:SCREEN-VALUE = STRING(creditos.per_pago) WHEN AVAILABLE creditos.
    rsPeriodicidad:SENSITIVE = FALSE.

    ASSIGN rsFormaDePago:SCREEN-VALUE = STRING(creditos.FOR_pago) WHEN AVAILABLE creditos.
    rsFormaDePago:SENSITIVE = FALSE.

    ASSIGN nuevaFecPrimerPago:SENSITIVE = TRUE WHEN AVAILABLE Creditos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFormaDePago
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFormaDePago C-Win
ON CHOOSE OF btnFormaDePago IN FRAME F_Creditos /* Forma de pago */
DO:
    ASSIGN nuevaTasa:SCREEN-VALUE = tasaActual:SCREEN-VALUE WHEN AVAILABLE creditos.
    nuevaTasa:SENSITIVE = FALSE.

    ASSIGN nuevoPlazo:SCREEN-VALUE = plazoActual:SCREEN-VALUE WHEN AVAILABLE creditos.
    nuevoPlazo:SENSITIVE = FALSE.

    ASSIGN nuevaCuota:SCREEN-VALUE = cuotaActual:SCREEN-VALUE WHEN AVAILABLE creditos.
    nuevaCuota:SENSITIVE = FALSE.

    ASSIGN nuevaFecPrimerPago:SCREEN-VALUE = fecPrimerPagoActual:SCREEN-VALUE WHEN AVAILABLE creditos.
    nuevaFecPrimerPago:SENSITIVE = FALSE.

    ASSIGN rsPeriodicidad:SCREEN-VALUE = STRING(creditos.per_pago) WHEN AVAILABLE creditos.
    rsPeriodicidad:SENSITIVE = FALSE.

    ASSIGN nuevaFecPago:SCREEN-VALUE = fecPagoActual:SCREEN-VALUE WHEN AVAILABLE creditos.
    nuevaFecPago:SENSITIVE = FALSE.

    ASSIGN rsFormaDePago:SENSITIVE = TRUE WHEN AVAILABLE Creditos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnImpProyeccion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnImpProyeccion C-Win
ON CHOOSE OF btnImpProyeccion IN FRAME F_Creditos /* Imprimir Proyección */
DO:
    /*DEFINE VAR Listado AS CHAR FORM "X(40)".
    DEFINE VAR W_Rpta AS LOGICAL.

    SESSION:SET-WAIT-STATE("GENERAL").

    ASSIGN txtNuevaCuota
           txtPlazoNuevo
           txtFecPago-2.

    Listado  = W_PathSpl + W_Usuario + "_ProyCred.Lst".

    {Incluido\Imprimir.I "listado"}

    SESSION:SET-WAIT-STATE("").*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPeriodicidadDePago
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPeriodicidadDePago C-Win
ON CHOOSE OF btnPeriodicidadDePago IN FRAME F_Creditos /* Periodicidad de pago */
DO:
    ASSIGN nuevaTasa:SCREEN-VALUE = tasaActual:SCREEN-VALUE WHEN AVAILABLE creditos.
    nuevaTasa:SENSITIVE = FALSE.

    ASSIGN nuevoPlazo:SCREEN-VALUE = plazoActual:SCREEN-VALUE WHEN AVAILABLE creditos.
    nuevoPlazo:SENSITIVE = FALSE.

    ASSIGN nuevaCuota:SCREEN-VALUE = cuotaActual:SCREEN-VALUE WHEN AVAILABLE creditos.
    nuevaCuota:SENSITIVE = FALSE.

    ASSIGN nuevaFecPago:SCREEN-VALUE = fecPagoActual:SCREEN-VALUE WHEN AVAILABLE creditos.
    nuevaFecPago:SENSITIVE = FALSE.

    ASSIGN nuevaFecPrimerPago:SCREEN-VALUE = fecPrimerPagoActual:SCREEN-VALUE WHEN AVAILABLE creditos.
    nuevaFecPrimerPago:SENSITIVE = FALSE.

    ASSIGN rsFormaDePago:SCREEN-VALUE = STRING(creditos.FOR_pago) WHEN AVAILABLE creditos.
    rsFormaDePago:SENSITIVE = FALSE.

    ASSIGN rsPeriodicidad:SENSITIVE = TRUE WHEN AVAILABLE Creditos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPlazo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPlazo C-Win
ON CHOOSE OF btnPlazo IN FRAME F_Creditos /* Plazo Actual / Nuevo Plazo */
DO:
    ASSIGN nuevaTasa:SCREEN-VALUE = tasaActual:SCREEN-VALUE WHEN AVAILABLE creditos.
    nuevaTasa:SENSITIVE = FALSE.

    ASSIGN nuevoPlazo:SCREEN-VALUE = plazoActual:SCREEN-VALUE WHEN AVAILABLE creditos.
    nuevoPlazo:SENSITIVE = FALSE.

    ASSIGN nuevaFecPago:SCREEN-VALUE = fecPagoActual:SCREEN-VALUE WHEN AVAILABLE creditos.
    nuevaFecPago:SENSITIVE = FALSE.

    ASSIGN nuevaFecPrimerPago:SCREEN-VALUE = fecPrimerPagoActual:SCREEN-VALUE WHEN AVAILABLE creditos.
    nuevaFecPrimerPago:SENSITIVE = FALSE.

    ASSIGN rsPeriodicidad:SCREEN-VALUE = STRING(creditos.per_pago) WHEN AVAILABLE creditos.
    rsPeriodicidad:SENSITIVE = FALSE.

    ASSIGN rsFormaDePago:SCREEN-VALUE = STRING(creditos.FOR_pago) WHEN AVAILABLE creditos.
    rsFormaDePago:SENSITIVE = FALSE.

    ASSIGN nuevoPlazo:SENSITIVE = TRUE WHEN AVAILABLE Creditos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnTasa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTasa C-Win
ON CHOOSE OF btnTasa IN FRAME F_Creditos /* Tasa Actual / Nueva Tasa */
DO:
    ASSIGN nuevaCuota:SCREEN-VALUE = cuotaActual:SCREEN-VALUE WHEN AVAILABLE creditos.
    nuevaCuota:SENSITIVE = FALSE.

    ASSIGN nuevoPlazo:SCREEN-VALUE = plazoActual:SCREEN-VALUE WHEN AVAILABLE creditos.
    nuevoPlazo:SENSITIVE = FALSE.

    ASSIGN nuevaFecPago:SCREEN-VALUE = fecPagoActual:SCREEN-VALUE WHEN AVAILABLE creditos.
    nuevaFecPago:SENSITIVE = FALSE.

    ASSIGN nuevaFecPrimerPago:SCREEN-VALUE = fecPrimerPagoActual:SCREEN-VALUE WHEN AVAILABLE creditos.
    nuevaFecPrimerPago:SENSITIVE = FALSE.

    ASSIGN rsPeriodicidad:SCREEN-VALUE = STRING(creditos.per_pago) WHEN AVAILABLE creditos.
    rsPeriodicidad:SENSITIVE = FALSE.

    ASSIGN rsFormaDePago:SCREEN-VALUE = STRING(creditos.FOR_pago) WHEN AVAILABLE creditos.
    rsFormaDePago:SENSITIVE = FALSE.

    ASSIGN nuevaTasa:SENSITIVE = TRUE WHEN AVAILABLE Creditos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salvar C-Win
ON CHOOSE OF Btn_Salvar IN FRAME F_Creditos /* Salvar */
DO:
    DEFINE VAR vMonto AS DECIMAL.
    DEFINE VAR vCuota AS DECIMAL.
    DEFINE VAR cuotaAnterior AS DECIMAL.
    DEFINE VAR vTasa AS DECIMAL.
    DEFINE VAR vPlazo AS INTEGER.
    DEFINE VAR vInteres AS DECIMAL.
    DEFINE VAR diasPreinicio AS INTEGER.
    DEFINE VAR interesPreinicio AS DECIMAL.
    DEFINE VAR flagRearmarAmortizacion AS LOGICAL.
    DEFINE VAR novedad AS CHARACTER.

    vTime = TIME.
    
    ASSIGN tasaActual
           nuevaTasa
           cuotaActual
           nuevaCuota
           plazoActual
           nuevoPlazo
           fecPagoActual
           nuevaFecPago
           fecPrimerPagoActual
           nuevaFecPrimerPago
           rsPeriodicidad
           rsFormaDePago
           tgCongelado.

    IF tasaActual <> nuevaTasa THEN
        MESSAGE "Está seguro del cambio de tasa solicitado?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "" UPDATE flagTasa AS LOGICAL.

    IF flagTasa = TRUE THEN DO:
        RUN cambiarTasa.
        flagRearmarAmortizacion = TRUE.
        
        novedad = "CambioDeTasa".

        flagTasa = FALSE.
    END.


    IF cuotaActual <> nuevaCuota THEN
        MESSAGE "Está seguro del cambio de cuota solicitado?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "" UPDATE flagCuota AS LOGICAL.

    IF flagCuota = TRUE THEN DO:
        RUN cambiarCuota.
        flagRearmarAmortizacion = TRUE.
        
        novedad = "CambioDeCuota".

        flagCuota = FALSE.
    END.

    IF plazoActual <> nuevoPlazo THEN
        MESSAGE "Está seguro del cambio de plazo solicitado?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "" UPDATE flagPlazo AS LOGICAL.

    IF flagPlazo = TRUE THEN DO:
        RUN cambiarPlazo.
        flagRearmarAmortizacion = TRUE.
        
        novedad = "CambioDePlazo".

        flagPlazo = FALSE.
    END.

    IF fecPagoActual <> nuevaFecPago THEN
        MESSAGE "Está seguro del cambio de fecha de vencimiento solicitado?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "" UPDATE flagfecPago AS LOGICAL.

    IF flagFecPago = TRUE THEN DO:
        RUN cambiarfecPago.

        flagFecPago = FALSE.
    END.

    IF fecPrimerPagoActual <> nuevaFecPrimerPago THEN
        MESSAGE "Está seguro del cambio de la fecha de primer pago solicitado?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "" UPDATE flagfecPrimerPago AS LOGICAL.

    IF flagFecPrimerPago = TRUE THEN DO:
        RUN cambiarfecPrimerPago.
        flagRearmarAmortizacion = TRUE.

        novedad = "CambioFecPrimerPago".
        
        flagFecPrimerPago = FALSE.
    END.

    IF rsPeriodicidad <> creditos.per_pago THEN
        MESSAGE "Está seguro del cambio de periodicidad de pago solicitado?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "" UPDATE flagPeriodicidadDePago AS LOGICAL.

    IF flagPeriodicidadDePago = TRUE THEN DO:
        RUN cambiarPeriodicidadDePago.
        flagRearmarAmortizacion = TRUE.

        novedad = "CambioDePeriodicidad".

        flagPeriodicidadDePago = FALSE.
    END.

    IF rsFormaDePago <> creditos.for_pago THEN
        MESSAGE "Está seguro del cambio de forma de pago de pago solicitado?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "" UPDATE flagFormaDePago AS LOGICAL.

    IF flagFormaDePago = TRUE THEN DO:
        RUN cambiarFormaDePago.

        flagFormaDePago = FALSE.
    END.

    IF flagRearmarAmortizacion = TRUE THEN
        RUN crearControlPagosRefinancia.r (INPUT creditos.nit,
                                           INPUT creditos.num_credito,
                                           INPUT creditos.tasa,
                                           INPUT "Se modifica periodicidad de pago").

    RUN cambiarEstado.
    
    btn_salvar:SENSITIVE = FALSE.
    RELEASE creditos.
    FRAME F_Productos:VISIBLE = FALSE.

    MESSAGE "Los cambios fueron realizados de manera exitosa."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    tasaActual:SCREEN-VALUE = "".
    nuevaTasa:SCREEN-VALUE = "".
    tasaActual:SENSITIVE = FALSE.
    nuevaTasa:SENSITIVE = FALSE.
    cuotaActual:SCREEN-VALUE = "".
    nuevaCuota:SCREEN-VALUE = "".
    cuotaActual:SENSITIVE = FALSE.
    nuevaCuota:SENSITIVE = FALSE.
    plazoActual:SCREEN-VALUE = "".
    nuevoPlazo:SCREEN-VALUE = "".
    plazoActual:SENSITIVE = FALSE.
    nuevoPlazo:SENSITIVE = FALSE.
    fecPagoActual:SCREEN-VALUE = "".
    nuevaFecPago:SCREEN-VALUE = "".
    fecPagoActual:SENSITIVE = FALSE.
    nuevaFecPago:SENSITIVE = FALSE.
    fecPrimerPagoActual:SENSITIVE = FALSE.
    nuevaFecPrimerPago:SENSITIVE = FALSE.
    rsPeriodicidad:SENSITIVE = FALSE.
    rsFormaDePago:SENSITIVE = FALSE.
    tgCongelado:SCREEN-VALUE = "No".
    tgCongelado:SENSITIVE = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 C-Win
ON CHOOSE OF BUTTON-2 IN FRAME F_Creditos /* Salir */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsFormaDePago
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsFormaDePago C-Win
ON VALUE-CHANGED OF rsFormaDePago IN FRAME F_Creditos
DO:
    ASSIGN rsFormaDePago.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_NitCte
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_NitCte C-Win
ON ENTRY OF W_NitCte IN FRAME F_Creditos /* Ced/Nit */
DO:
    nuevaTasa:SENSITIVE = FALSE.  
    nuevaCuota:SENSITIVE = FALSE.
    nuevoPlazo:SENSITIVE = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_NitCte C-Win
ON LEAVE OF W_NitCte IN FRAME F_Creditos /* Ced/Nit */
DO:
    ASSIGN W_NitCte
           W_NomCte:SCREEN-VALUE = "".

    FIND FIRST Clientes WHERE Clientes.Nit = W_NitCte
                          AND Clientes.Estado = 1 NO-LOCK NO-ERROR.
    IF NOT AVAIL(Clientes) THEN DO:
        RUN C-Clientes.R (INPUT 1,
                          INPUT W_Agencia,
                          OUTPUT W_NitCte,
                          OUTPUT W_NomCte,
                          OUTPUT W_NomCte,
                          OUTPUT W_Age).

        FIND FIRST Clientes WHERE Clientes.Nit = W_NitCte
                              AND Clientes.Estado = 1 NO-LOCK NO-ERROR.
        IF AVAIL(Clientes) THEN DO:
            ASSIGN W_NitCte:SCREEN-VALUE = W_NitCte
                   W_NomCte:SCREEN-VALUE = TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2) + " " + TRIM(Clientes.Nombre).

            RUN BuscarCreditos.

            FRAME F_Productos:VISIBLE = TRUE.
        END.
        ELSE
            MESSAGE "El Nit debe existir Activo en Clientes."
                VIEW-AS ALERT-BOX.
    END.
    ELSE DO:
        ASSIGN W_NomCte:SCREEN-VALUE = TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2) + " " + TRIM(Clientes.Nombre).

        IF Clientes.Fec_fallecido NE ? THEN
            MESSAGE "Este cliente aparece como FALLECIDO fecha: " clientes.Fec_fallecido
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RUN BuscarCreditos.

        FRAME F_Productos:VISIBLE = TRUE.
    END.

    btnImpProyeccion:SENSITIVE = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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

    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.

    FRAME F_Productos:VISIBLE = FALSE.

    flagControlPagos = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuscarCreditos C-Win 
PROCEDURE BuscarCreditos :
EMPTY TEMP-TABLE TT_Creditos.

CLOSE QUERY Br_Pdctos.

FOR EACH Creditos WHERE Creditos.Nit = W_NitCte
                    AND Creditos.Estado = 2 NO-LOCK BY Creditos.Cod_Credito:
    FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito = Creditos.Cod_Credito
                              AND Pro_Creditos.Estado = 1 NO-LOCK NO-ERROR.

    CREATE TT_Creditos.
    TT_Creditos.Cod_credito = Creditos.Cod_Credito.
    TT_Creditos.num_credito = Creditos.Num_Credito.
    TT_Creditos.Agencia = Creditos.Agencia.
    TT_Creditos.fec_desembolso = Creditos.Fec_Desembolso.
    TT_Creditos.Cuota = Creditos.Cuota.
    TT_Creditos.sdo_Capital = Creditos.Honorarios +
                              Creditos.Costas +
                              Creditos.Polizas +
                              Creditos.Int_MorCobrar +
                              Creditos.Int_MoraDifCob +
                              Creditos.Int_DifCobro +
                              Creditos.Int_Corrientes +
                              Creditos.Sdo_Capital - Creditos.Int_Anticipado.

    TT_Creditos.nombre_linea = Pro_Creditos.Nom_Producto.
    TT_Creditos.fec_pago = Creditos.Fec_Pago.
    TT_Creditos.plazo = creditos.plazo.
    TT_Creditos.tasa = creditos.tasa.
    tt_creditos.nit = creditos.nit.
END.

OPEN QUERY Br_Pdctos FOR EACH TT_Creditos.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cambiarCuota C-Win 
PROCEDURE cambiarCuota :
DEFINE VAR diasPeriodo AS INTEGER.
DEFINE VAR P_NMeses AS INTEGER.
DEFINE VAR numPeriodos AS INTEGER.
DEFINE VAR P_NomPer AS CHARACTER.
DEFINE VAR vTasa AS DECIMAL.
DEFINE VAR vPlazo AS INTEGER.
DEFINE VAR vCuota AS DECIMAL.
DEFINE VAR plazoAnterior AS INTEGER.
DEFINE VAR vValorPresenteCuotaExtra AS DECIMAL.
DEFINE VAR vTotalValorPresenteCuotaExtra AS DECIMAL.

RUN HallarPeriodo IN W_ManFin (INPUT Creditos.Per_Pago,
                               INPUT Creditos.Plazo,
                               OUTPUT diasPeriodo,
                               OUTPUT P_NMeses,
                               OUTPUT numPeriodos,
                               OUTPUT P_NomPer).

FOR EACH Extras WHERE Extras.Nit = creditos.nit
                  AND Extras.Num_Solicitud = creditos.num_solicitud
                  AND extras.Fec_Vcto >= creditos.fec_pago NO-LOCK BY Extras.Nro_Cuota:
    RUN HPDF IN W_ManFin (INPUT Extras.Vr_CuoExtra,
                          INPUT (creditos.tasa / (numPeriodos * 100)),
                          INPUT Extras.Nro_Cuota - creditos.cuo_pagadas,
                          OUTPUT vValorPresenteCuotaExtra).
    vTotalValorPresenteCuotaExtra = vTotalValorPresenteCuotaExtra + vValorPresenteCuotaExtra.
END.

RUN HNCF IN W_ManFin (INPUT creditos.sdo_capital - vTotalValorPresenteCuotaExtra,
                      INPUT nuevaCuota,
                      INPUT creditos.tasa / numPeriodos / 100,
                      OUTPUT vPlazo).

MESSAGE "El nuevo plazo para el crédito es de" vPlazo "periodos." SKIP
        "Desea realizar el cambio...?"
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "" UPDATE flagYes AS LOGICAL.

IF flagYes = TRUE THEN DO:
    vCuota = creditos.cuota.
    plazoAnterior = creditos.plazo.

    creditos.fec_pagAnti = creditos.fec_pago.
    creditos.fec_desembolso = w_fecha.
    creditos.cuo_pagadas = 0.
    creditos.monto = creditos.sdo_Capital.
    creditos.cuota = nuevaCuota.
    creditos.plazo = vPlazo.

    /*RUN CrearControlPagos.r(INPUT creditos.nit,
                            INPUT creditos.num_credito).*/

    RUN crearControlPagosRefinancia.r(INPUT creditos.nit,
                                      INPUT creditos.num_credito,
                                      INPUT creditos.tasa,
                                      INPUT "Reliquida x Cambio de Cuota").

    CREATE Mov_Creditos.
    ASSIGN Mov_Creditos.Agencia = Creditos.Agencia
           Mov_Creditos.Cod_Credito = Creditos.Cod_Credito
           Mov_Creditos.Nit = Creditos.Nit
           Mov_Creditos.Num_Credito = Creditos.Num_Credito
           Mov_Creditos.Cod_Operacion = 999999999
           Mov_Creditos.Ofi_Destino = Creditos.Agencia
           Mov_Creditos.Ofi_Fuente = W_Agencia
           Mov_Creditos.Pagare = Creditos.Pagare
           Mov_Creditos.Fecha = W_Fecha
           Mov_Creditos.Hora = vTime
           Mov_Creditos.Usuario = W_Usuario
           Mov_Creditos.Int_Corriente = Creditos.Int_Corriente + creditos.INT_difCobro
           Mov_Creditos.Int_MorCobrar = Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob
           Mov_Creditos.Sdo_Capital = Creditos.Sdo_Capital
           Mov_Creditos.Cpte = 20
           Mov_Creditos.Descrip = "Cuota - $" + REPLACE(STRING(vCuota,">>>,>>>,>>9")," ","") + " --> $" + replace(STRING(creditos.cuota,"$>>>,>>>,>>9")," ","").

    CREATE Mov_Creditos.
    ASSIGN Mov_Creditos.Agencia = Creditos.Agencia
           Mov_Creditos.Cod_Credito = Creditos.Cod_Credito
           Mov_Creditos.Nit = Creditos.Nit
           Mov_Creditos.Num_Credito = Creditos.Num_Credito
           Mov_Creditos.Cod_Operacion = 999999999
           Mov_Creditos.Ofi_Destino = Creditos.Agencia
           Mov_Creditos.Ofi_Fuente = W_Agencia
           Mov_Creditos.Pagare = Creditos.Pagare
           Mov_Creditos.Fecha = W_Fecha
           Mov_Creditos.Hora = vTime
           Mov_Creditos.Usuario = W_Usuario
           Mov_Creditos.Int_Corriente = Creditos.Int_Corriente + creditos.INT_difCobro
           Mov_Creditos.Int_MorCobrar = Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob
           Mov_Creditos.Sdo_Capital = Creditos.Sdo_Capital
           Mov_Creditos.Cpte = 20
           Mov_Creditos.Descrip = "Plazo - " + STRING(plazoAnterior) + " --> " + STRING(creditos.plazo).

    CLOSE QUERY Br_Pdctos.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cambiarEstado C-Win 
PROCEDURE cambiarEstado :
IF creditos.detalle_Estado = 2 AND tgCongelado:SCREEN-VALUE IN FRAME F_Creditos = "No" THEN DO:
    MESSAGE "Se reactivará la liquidación de intereses para este" SKIP
            "crédito. Está seguro de esta operación...? (recuerde" SKIP
            "revisar la fecha de próximo pago)"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "" UPDATE flagYesReactivar AS LOGICAL.

    IF flagYesReactivar = TRUE THEN DO:
        creditos.detalle_Estado = 1.
        
        CREATE Mov_Creditos.
        ASSIGN Mov_Creditos.Agencia = Creditos.Agencia
               Mov_Creditos.Cod_Credito = Creditos.Cod_Credito
               Mov_Creditos.Nit = Creditos.Nit
               Mov_Creditos.Num_Credito = Creditos.Num_Credito
               Mov_Creditos.Cod_Operacion = 999999999
               Mov_Creditos.Ofi_Destino = Creditos.Agencia
               Mov_Creditos.Ofi_Fuente = W_Agencia
               Mov_Creditos.Pagare = Creditos.Pagare
               Mov_Creditos.Fecha = W_Fecha
               Mov_Creditos.Hora = vTime
               Mov_Creditos.Usuario = W_Usuario
               Mov_Creditos.Int_Corriente = Creditos.Int_Corriente + creditos.INT_difCobro
               Mov_Creditos.Int_MorCobrar = Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob
               Mov_Creditos.Sdo_Capital = Creditos.Sdo_Capital
               Mov_Creditos.Cpte = 20
               Mov_Creditos.Descrip = "Crédito Reactivado - Fecha próximo pago: " + STRING(nuevaFecPago,"99/99/9999").
    END.
END.
ELSE DO:
    IF creditos.detalle_Estado <> 2 AND tgCongelado:SCREEN-VALUE = "yes" THEN DO:
        MESSAGE "Se realizará el congelamiento de este crédito." SKIP
                "A partir de este momento se suspende la causación" SKIP
                "de intereses por todo concepto. Está seguro de" SKIP
                "realizar esta operación...?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "" UPDATE flagYesCongelar AS LOGICAL.

        IF flagYesCongelar = TRUE THEN DO:
            creditos.detalle_Estado = 2.

            CREATE Mov_Creditos.
            ASSIGN Mov_Creditos.Agencia = Creditos.Agencia
                   Mov_Creditos.Cod_Credito = Creditos.Cod_Credito
                   Mov_Creditos.Nit = Creditos.Nit
                   Mov_Creditos.Num_Credito = Creditos.Num_Credito
                   Mov_Creditos.Cod_Operacion = 999999999
                   Mov_Creditos.Ofi_Destino = Creditos.Agencia
                   Mov_Creditos.Ofi_Fuente = W_Agencia
                   Mov_Creditos.Pagare = Creditos.Pagare
                   Mov_Creditos.Fecha = W_Fecha
                   Mov_Creditos.Hora = vTime
                   Mov_Creditos.Usuario = W_Usuario
                   Mov_Creditos.Int_Corriente = Creditos.Int_Corriente + creditos.INT_difCobro
                   Mov_Creditos.Int_MorCobrar = Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob
                   Mov_Creditos.Sdo_Capital = Creditos.Sdo_Capital
                   Mov_Creditos.Cpte = 20
                   Mov_Creditos.Descrip = "Crédito Congelado - Fecha de próximo pago: " + STRING(nuevaFecPago,"99/99/9999").
        END.

    END.
END.


CLOSE QUERY Br_Pdctos.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cambiarFecPago C-Win 
PROCEDURE cambiarFecPago :
DEFINE VAR vFecPago AS DATE.
DEFINE VAR vInteres AS DECIMAL.
DEFINE VAR iterFecha AS DATE.

vFecPago = creditos.fec_pago.
creditos.fec_pago = nuevaFecPago.

IF creditos.cod_credito <> 123 THEN DO:
    IF creditos.cod_credito <> 108 AND
       creditos.cod_credito <> 113 AND
       creditos.cod_credito <> 114 THEN DO:
        FOR EACH control_Pagos WHERE control_Pagos.nit = creditos.nit
                                 AND CONTROL_pagos.num_credito = creditos.num_credito BY control_Pagos.fec_vcto:
            IF CONTROL_pagos.fec_vcto < creditos.fec_pago THEN DO:
                IF CONTROL_pagos.id_pdoMes <> 2 THEN
                    creditos.cuo_pagadas = creditos.cuo_pagadas + 1.

                CONTROL_pagos.Id_PdoMes = 2.
            END.
            ELSE DO:
                IF CONTROL_pagos.id_PdoMes = 2 THEN
                    creditos.cuo_pagadas = creditos.cuo_pagadas - 1.

                CONTROL_pagos.id_PdoMes = 0.
                control_Pagos.Int_pagado = 0.
                control_pagos.Morapagada = 0.
                control_pagos.Cap_pagado = 0.
            END.
        END.

        creditos.val_atraso = 0.
        creditos.cuo_atraso = 0.
        creditos.dias_atraso = 0.
        creditos.sdo_proyectado = creditos.sdo_capital.

        /* Días de atraso */
        IF creditos.fec_pago < w_fecha THEN
            creditos.dias_atraso = w_fecha - creditos.fec_pago.
        ELSE
            creditos.dias_atraso = 0.

        /* Valor del atraso - Cuotas atrasadas */
        IF creditos.dias_atraso > 0 THEN DO:
            FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                                     AND CONTROL_pagos.num_credito = creditos.num_credito
                                     AND CONTROL_pagos.id_pdoMes < 2
                                     AND CONTROL_pagos.fec_Vcto <= w_fecha NO-LOCK:
                creditos.val_atraso = creditos.val_atraso + CONTROL_pagos.pagos_capitalAcum.
                creditos.cuo_atraso = creditos.cuo_atraso + 1.
                creditos.sdo_proyectado = creditos.sdo_proyectado - CONTROL_pagos.pagos_capitalAcum.
            END.

            FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                                     AND CONTROL_pagos.num_credito = creditos.num_credito NO-LOCK BY CONTROL_pagos.fec_vcto DESC:
                IF control_Pagos.fec_vcto < w_fecha THEN
                    creditos.val_atraso = creditos.sdo_Capital.

                LEAVE.
            END.

            IF creditos.val_atraso > creditos.sdo_capital THEN
                creditos.val_atraso = creditos.sdo_capital.
        END.
    END.
    ELSE DO:
        creditos.fec_pagAnti = creditos.fec_pago.

        /* Calcular la nueva cuota */
        vInteres = creditos.INT_corriente + creditos.INT_difCobro.

        DO iterFecha = /*vFecPago*/ TODAY TO nuevaFecPago - 1:
            vInteres = vInteres + ROUND((((creditos.sdo_capital + vInteres) / 100) * (creditos.tasa / 365)),0).
        END.

        MESSAGE "La nueva cuota para el crédito es $" + STRING(creditos.monto + vInteres,">>>,>>>,>>>,>>9")
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        creditos.cuota = creditos.monto + vInteres.

        FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                                 AND CONTROL_pagos.num_credito = creditos.num_credito
                                 AND CONTROL_pagos.nro_cuota = 1:
            CONTROL_pagos.fec_vcto = creditos.fec_pago.
            CONTROL_pagos.cuota = creditos.cuota.
            CONTROL_pagos.pagos_intAcum = CONTROL_pagos.cuota - CONTROL_pagos.pagos_capitalAcum.
        END.

        FOR EACH amortizacion WHERE amortizacion.nit = creditos.nit
                                AND amortizacio.num_credito = creditos.num_credito
                                AND amortizacion.nro_cuota = 1:
            amortizacion.fec_pago = creditos.fec_pago.
            amortizacion.cuota = credito.cuota.
            amortizacion.cuota_i = amortizacion.cuota - amortizacion.cuota_k.
        END.
    END.
END.
ELSE DO:
    IF creditos.cod_credito = 123 THEN DO:
        creditos.val_atraso = 0.
        creditos.cuo_atraso = 0.

        FOR EACH facturacion WHERE facturacion.nit = creditos.nit
                               AND facturacion.num_credito = creditos.num_credito
                               AND facturacion.fec_pago < creditos.fec_pago
                               AND facturacion.estado <> 2:
            facturacion.estado = 2.
            facturacion.pago_capital = facturacion.capital.
            facturacion.pago_intCorriente = facturacion.int_corriente.
            facturacion.pago_intDifCobro = facturacion.int_difCobro.
            facturacion.pago_mora = facturacion.int_mora.

            creditos.cuo_pagadas = creditos.cuo_pagadas + 1.
        END.

        FOR EACH facturacion WHERE facturacion.nit = creditos.nit
                               AND facturacion.num_credito = creditos.num_credito
                               AND facturacion.fec_pago >= creditos.fec_pago:
            IF facturacion.estado = 2 THEN
                creditos.cuo_pagadas = creditos.cuo_pagadas - 1.

            facturacion.estado = 1.
            facturacion.pago_capital = 0.
            facturacion.pago_intCorriente = 0.
            facturacion.pago_intDifCobro = 0.
            facturacion.pago_mora = 0.

            creditos.val_atraso = creditos.val_atraso + facturacion.capital.
            creditos.cuo_atraso = creditos.cuo_atraso + 1.
        END.
    END.
    ELSE DO:
        IF creditos.detalle_estado = 2 THEN DO: /* Si el crèdito se encuentra congelado */

        END.
    END.
END.

CREATE Mov_Creditos.
ASSIGN Mov_Creditos.Agencia = Creditos.Agencia
       Mov_Creditos.Cod_Credito = Creditos.Cod_Credito
       Mov_Creditos.Nit = Creditos.Nit
       Mov_Creditos.Num_Credito = Creditos.Num_Credito
       Mov_Creditos.Cod_Operacion = 999999999
       Mov_Creditos.Ofi_Destino = Creditos.Agencia
       Mov_Creditos.Ofi_Fuente = W_Agencia
       Mov_Creditos.Pagare = Creditos.Pagare
       Mov_Creditos.Fecha = W_Fecha
       Mov_Creditos.Hora = vTime
       Mov_Creditos.Usuario = W_Usuario
       Mov_Creditos.Int_Corriente = Creditos.Int_Corriente + creditos.INT_difCobro
       Mov_Creditos.Int_MorCobrar = Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob
       Mov_Creditos.Sdo_Capital = Creditos.Sdo_Capital
       Mov_Creditos.Cpte = 20
       Mov_Creditos.Descrip = "Fecha de pago - " + STRING(vFecPago,"99/99/9999") + " --> " + STRING(creditos.fec_pago,"99/99/9999").

CLOSE QUERY Br_Pdctos.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cambiarFecPrimerPago C-Win 
PROCEDURE cambiarFecPrimerPago :
DEFINE VAR vFecPrimerPago AS DATE.

vFecPrimerPago = creditos.fec_pagAnti.
creditos.fec_pago = nuevaFecPrimerPago.
creditos.fec_pagAnti = nuevaFecPrimerPago.
creditos.fec_desembolso = w_fecha.

/* Para créditos congelados a una cuota */
IF creditos.detalle_estado = 2 THEN DO:
    FIND FIRST CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                               AND CONTROL_pagos.num_credito = creditos.num_credito
                               AND CONTROL_pagos.nro_cuota = 1 NO-ERROR.
    IF AVAILABLE CONTROL_pagos THEN
        CONTROL_pagos.fec_vcto = nuevaFecPrimerPago.
END.
ELSE
    RUN CrearControlPagos.r(INPUT creditos.nit,
                            INPUT creditos.num_credito).

CREATE Mov_Creditos.
ASSIGN Mov_Creditos.Agencia = Creditos.Agencia
       Mov_Creditos.Cod_Credito = Creditos.Cod_Credito
       Mov_Creditos.Nit = Creditos.Nit
       Mov_Creditos.Num_Credito = Creditos.Num_Credito
       Mov_Creditos.Cod_Operacion = 999999999
       Mov_Creditos.Ofi_Destino = Creditos.Agencia
       Mov_Creditos.Ofi_Fuente = W_Agencia
       Mov_Creditos.Pagare = Creditos.Pagare
       Mov_Creditos.Fecha = W_Fecha
       Mov_Creditos.Hora = vTime
       Mov_Creditos.Usuario = W_Usuario
       Mov_Creditos.Int_Corriente = Creditos.Int_Corriente + creditos.INT_difCobro
       Mov_Creditos.Int_MorCobrar = Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob
       Mov_Creditos.Sdo_Capital = Creditos.Sdo_Capital
       Mov_Creditos.Cpte = 20
       Mov_Creditos.Descrip = "Fecha Primer Pago - " + STRING(vFecPrimerPago,"99/99/9999") + " --> " + STRING(creditos.fec_pagAnti,"99/99/9999").

CLOSE QUERY Br_Pdctos.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cambiarFormaDePago C-Win 
PROCEDURE cambiarFormaDePago :
DEFINE VAR formaPagoAnterior AS CHARACTER.
DEFINE VAR formaPagoNueva AS CHARACTER.

IF creditos.FOR_pago = 1 THEN
    ASSIGN formaPagoAnterior = "Caja"
           formaPagoNueva = "Nómina".
ELSE
    ASSIGN formaPagoAnterior = "Nómina"
           formaPagoNueva = "Caja".

creditos.FOR_pago = rsFormaDePago.

CREATE Mov_Creditos.
ASSIGN Mov_Creditos.Agencia = Creditos.Agencia
       Mov_Creditos.Cod_Credito = Creditos.Cod_Credito
       Mov_Creditos.Nit = Creditos.Nit
       Mov_Creditos.Num_Credito = Creditos.Num_Credito
       Mov_Creditos.Cod_Operacion = 999999999
       Mov_Creditos.Ofi_Destino = Creditos.Agencia
       Mov_Creditos.Ofi_Fuente = W_Agencia
       Mov_Creditos.Pagare = Creditos.Pagare
       Mov_Creditos.Fecha = W_Fecha
       Mov_Creditos.Hora = vTime
       Mov_Creditos.Usuario = W_Usuario
       Mov_Creditos.Int_Corriente = Creditos.Int_Corriente + creditos.INT_difCobro
       Mov_Creditos.Int_MorCobrar = Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob
       Mov_Creditos.Sdo_Capital = Creditos.Sdo_Capital
       Mov_Creditos.Cpte = 20
       Mov_Creditos.Descrip = "Forma de Pago - " + formaPagoAnterior + " --> " + formaPagoNueva.

CLOSE QUERY Br_Pdctos.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cambiarPeriodicidadDePago C-Win 
PROCEDURE cambiarPeriodicidadDePago :
DEFINE VAR diasPeriodo AS INTEGER.
DEFINE VAR P_NMeses AS INTEGER.
DEFINE VAR numPeriodos AS INTEGER.
DEFINE VAR P_NomPer AS CHARACTER.
DEFINE VAR vTEA_original AS DECIMAL.
DEFINE VAR vTNA_nueva AS DECIMAL.
DEFINE VAR vPlazo AS INTEGER.
DEFINE VAR vCuota AS DECIMAL.
DEFINE VAR plazoAnterior AS INTEGER.
DEFINE VAR interesPreinicio AS INTEGER.
DEFINE VAR periodicidadAnterior AS CHARACTER.
DEFINE VAR nuevaPeriodicidad AS CHARACTER.
DEFINE VAR intCorriente AS INTEGER.
DEFINE VAR totalMonto AS INTEGER.
DEFINE VAR cuotaAnterior AS INTEGER.
DEFINE VAR plazoEnMeses AS INTEGER.

/* Simulamos la modificación */
vPlazo = creditos.plazo.

IF creditos.per_pago = 3 THEN
    vPlazo = creditos.plazo / 2.

IF creditos.per_pago = 4 THEN
    vPlazo = creditos.plazo * 2.

/* Hallamos la TEA a partir de la tasa original del crédito (TNA) */
RUN HallarPeriodo IN W_ManFin (INPUT creditos.per_pago,
                               INPUT vPlazo,
                               OUTPUT diasPeriodo,
                               OUTPUT P_NMeses,
                               OUTPUT numPeriodos,
                               OUTPUT P_NomPer).

RUN NVEF IN w_manFin (INPUT creditos.tasa / 100,
                      INPUT numPeriodos,
                      OUTPUT vTEA_original).

/* Ahora hallamos la nueva tasa para la nueva periodicidad */
RUN HallarPeriodo IN W_ManFin (INPUT rsPeriodicidad,
                               INPUT vPlazo,
                               OUTPUT diasPeriodo,
                               OUTPUT P_NMeses,
                               OUTPUT numPeriodos,
                               OUTPUT P_NomPer).

RUN EFNV IN w_manFin (INPUT vTEA_original,
                      INPUT numPeriodos,
                      OUTPUT vTNA_nueva).

RUN interesPreinicio (INPUT diasPeriodo,
                      INPUT (vTNA_nueva / numPeriodos) * 100,
                      OUTPUT interesPreinicio).

intCorriente = creditos.INT_corriente + creditos.INT_difCobro.
vTEA_original = vTEA_original * 100.

IF creditos.cod_credito <> 108 AND
   creditos.cod_credito <> 113 AND
   creditos.cod_credito <> 114 THEN
    totalMonto = creditos.sdo_capital + interesPreinicio.
ELSE
    totalMonto = creditos.monto.

RUN Calculo_Cuota.R (INPUT-OUTPUT totalMonto,
                     INPUT-OUTPUT vPlazo,
                     INPUT-OUTPUT vCuota,
                     INPUT-OUTPUT intCorriente,
                     INPUT-OUTPUT vTEA_original,
                     INPUT 0,
                     INPUT 0,
                     INPUT rsPeriodicidad,
                     INPUT 3,
                     INPUT 1,
                     INPUT Creditos.Sistema).

MESSAGE "La nueva cuota para el crédito es de $" + STRING(vCuota,">>>,>>>,>>9") SKIP
        "Desea realizar el cambio...?"
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "" UPDATE flagYes AS LOGICAL.

IF flagYes = TRUE THEN DO:
    plazoAnterior = creditos.plazo.
    cuotaAnterior = creditos.cuota.

    CASE creditos.per_pago:
        WHEN 4 THEN periodicidadAnterior = "Mensual".
        WHEN 6 THEN periodicidadAnterior = "Trimestral".
        WHEN 8 THEN periodicidadAnterior = "Semestral".
        WHEN 9 THEN periodicidadAnterior = "Anual".
    END CASE.

    CASE rsPeriodicidad:
        WHEN 4 THEN periodicidadAnterior = "Mensual".
        WHEN 6 THEN periodicidadAnterior = "Trimestral".
        WHEN 8 THEN periodicidadAnterior = "Semestral".
        WHEN 9 THEN periodicidadAnterior = "Anual".
    END CASE.

    creditos.per_pago = rsPeriodicidad.
    creditos.cuota = vCuota.
    creditos.fec_pagAnti = creditos.fec_pago.
    creditos.fec_desembolso = w_fecha.
    creditos.cuo_pagadas = 0.
    creditos.monto = creditos.sdo_Capital.
    creditos.plazo = vPlazo.
        
    RUN CrearControlPagos.r(INPUT creditos.nit,
                            INPUT creditos.num_credito).
    
    CREATE Mov_Creditos.
    ASSIGN Mov_Creditos.Agencia = Creditos.Agencia
           Mov_Creditos.Cod_Credito = Creditos.Cod_Credito
           Mov_Creditos.Nit = Creditos.Nit
           Mov_Creditos.Num_Credito = Creditos.Num_Credito
           Mov_Creditos.Cod_Operacion = 999999999
           Mov_Creditos.Ofi_Destino = Creditos.Agencia
           Mov_Creditos.Ofi_Fuente = W_Agencia
           Mov_Creditos.Pagare = Creditos.Pagare
           Mov_Creditos.Fecha = W_Fecha
           Mov_Creditos.Hora = vTime
           Mov_Creditos.Usuario = W_Usuario
           Mov_Creditos.Int_Corriente = Creditos.Int_Corriente + creditos.INT_difCobro
           Mov_Creditos.Int_MorCobrar = Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob
           Mov_Creditos.Sdo_Capital = Creditos.Sdo_Capital
           Mov_Creditos.Cpte = 20
           Mov_Creditos.Descrip = "Periodicidad de Pago - " + periodicidadAnterior + " --> " + nuevaPeriodicidad.
    
    CREATE Mov_Creditos.
    ASSIGN Mov_Creditos.Agencia = Creditos.Agencia
           Mov_Creditos.Cod_Credito = Creditos.Cod_Credito
           Mov_Creditos.Nit = Creditos.Nit
           Mov_Creditos.Num_Credito = Creditos.Num_Credito
           Mov_Creditos.Cod_Operacion = 999999999
           Mov_Creditos.Ofi_Destino = Creditos.Agencia
           Mov_Creditos.Ofi_Fuente = W_Agencia
           Mov_Creditos.Pagare = Creditos.Pagare
           Mov_Creditos.Fecha = W_Fecha
           Mov_Creditos.Hora = vTime
           Mov_Creditos.Usuario = W_Usuario
           Mov_Creditos.Int_Corriente = Creditos.Int_Corriente + creditos.INT_difCobro
           Mov_Creditos.Int_MorCobrar = Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob
           Mov_Creditos.Sdo_Capital = Creditos.Sdo_Capital
           Mov_Creditos.Cpte = 20
           Mov_Creditos.Descrip = "Plazo - " + STRING(plazoAnterior) + " --> " + STRING(creditos.plazo).

    CREATE Mov_Creditos.
    ASSIGN Mov_Creditos.Agencia = Creditos.Agencia
           Mov_Creditos.Cod_Credito = Creditos.Cod_Credito
           Mov_Creditos.Nit = Creditos.Nit
           Mov_Creditos.Num_Credito = Creditos.Num_Credito
           Mov_Creditos.Cod_Operacion = 999999999
           Mov_Creditos.Ofi_Destino = Creditos.Agencia
           Mov_Creditos.Ofi_Fuente = W_Agencia
           Mov_Creditos.Pagare = Creditos.Pagare
           Mov_Creditos.Fecha = W_Fecha
           Mov_Creditos.Hora = vTime
           Mov_Creditos.Usuario = W_Usuario
           Mov_Creditos.Int_Corriente = Creditos.Int_Corriente + creditos.INT_difCobro
           Mov_Creditos.Int_MorCobrar = Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob
           Mov_Creditos.Sdo_Capital = Creditos.Sdo_Capital
           Mov_Creditos.Cpte = 20
           Mov_Creditos.Descrip = "Cuota - $" + replace(STRING(cuotaAnterior,">>>,>>>,>>9")," ","") + " --> $" + replace(STRING(creditos.cuota,">>>,>>>,>>9")," ","").
    
    CLOSE QUERY Br_Pdctos.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cambiarPlazo C-Win 
PROCEDURE cambiarPlazo :
DEFINE VAR intCorriente AS DECIMAL.
DEFINE VAR vPlazo AS INTEGER.
DEFINE VAR vCuota AS DECIMAL.
DEFINE VAR cuotaAnterior AS DECIMAL.
DEFINE VAR vTasa AS DECIMAL.
DEFINE VAR diasPeriodo AS INTEGER.
DEFINE VAR P_NMeses as integer.
DEFINE VAR numPeriodos AS INTEGER.
DEFINE VAR P_NomPer AS CHARACTER.
DEFINE VAR interesPreinicio AS INTEGER.
DEFINE VAR totalMonto AS INTEGER.

/* Simulamos el cambio */
vPlazo = creditos.plazo.

IF creditos.cod_credito <> 123 THEN DO:
    RUN HallarPeriodo IN W_ManFin (INPUT Creditos.Per_Pago,
                                   INPUT Creditos.Plazo,
                                   OUTPUT diasPeriodo,
                                   OUTPUT P_NMeses,
                                   OUTPUT numPeriodos,
                                   OUTPUT P_NomPer).

    RUN NVEF IN w_manFin (INPUT creditos.tasa / 100 / numPeriodos,
                          INPUT numPeriodos,
                          OUTPUT vTasa).

    RUN interesPreinicio (INPUT diasPeriodo,
                          INPUT vTasa * 100,
                          OUTPUT interesPreinicio).

    intCorriente = creditos.INT_corriente + creditos.INT_difCobro.
    vTasa = vTasa * 100.

    totalMonto = creditos.sdo_capital + interesPreinicio.

    RUN Calculo_Cuota.R (INPUT-OUTPUT totalMonto,
                         INPUT-OUTPUT nuevoPlazo,
                         INPUT-OUTPUT vCuota,
                         INPUT-OUTPUT intCorriente,
                         INPUT-OUTPUT vTasa,
                         INPUT 0,
                         INPUT 0,
                         INPUT creditos.per_Pago,
                         INPUT 3,
                         INPUT 1,
                         INPUT Creditos.Sistema).
END.
ELSE
    vCuota = creditos.cuota.

MESSAGE "La nueva cuota para el crédito es de $" STRING(vCuota,">>>,>>>,>>9") SKIP
        "Desea realizar el cambio...?"
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "" UPDATE flagYes AS LOGICAL.

IF flagYes = TRUE THEN DO:
    vPlazo = creditos.plazo.
    cuotaAnterior = creditos.cuota.
    
    IF creditos.cod_credito <> 123 THEN DO:
        creditos.fec_pagAnti = creditos.fec_pago.
        creditos.fec_desembolso = w_fecha.
        creditos.cuo_pagadas = 0.
        creditos.monto = creditos.sdo_Capital.
    END.

    creditos.plazo = nuevoPlazo.
    creditos.cuota = vCuota.
    
    IF creditos.cod_credito <> 123 THEN DO:
        RUN CrearControlPagos.r(INPUT creditos.nit,
                                INPUT creditos.num_credito).
    END.

    CREATE Mov_Creditos.
    ASSIGN Mov_Creditos.Agencia = Creditos.Agencia
           Mov_Creditos.Cod_Credito = Creditos.Cod_Credito
           Mov_Creditos.Nit = Creditos.Nit
           Mov_Creditos.Num_Credito = Creditos.Num_Credito
           Mov_Creditos.Cod_Operacion = 999999999
           Mov_Creditos.Ofi_Destino = Creditos.Agencia
           Mov_Creditos.Ofi_Fuente = W_Agencia
           Mov_Creditos.Pagare = Creditos.Pagare
           Mov_Creditos.Fecha = W_Fecha
           Mov_Creditos.Hora = vTime
           Mov_Creditos.Usuario = W_Usuario
           Mov_Creditos.Int_Corriente = Creditos.Int_Corriente + creditos.INT_difCobro
           Mov_Creditos.Int_MorCobrar = Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob
           Mov_Creditos.Sdo_Capital = Creditos.Sdo_Capital
           Mov_Creditos.Cpte = 20
           Mov_Creditos.Descrip = "Plazo - " + STRING(vPlazo) + " --> " + STRING(creditos.plazo).

    IF creditos.cod_credito <> 123 THEN DO:
        CREATE Mov_Creditos.
        ASSIGN Mov_Creditos.Agencia = Creditos.Agencia
               Mov_Creditos.Cod_Credito = Creditos.Cod_Credito
               Mov_Creditos.Nit = Creditos.Nit
               Mov_Creditos.Num_Credito = Creditos.Num_Credito
               Mov_Creditos.Cod_Operacion = 999999999
               Mov_Creditos.Ofi_Destino = Creditos.Agencia
               Mov_Creditos.Ofi_Fuente = W_Agencia
               Mov_Creditos.Pagare = Creditos.Pagare
               Mov_Creditos.Fecha = W_Fecha
               Mov_Creditos.Hora = vTime
               Mov_Creditos.Usuario = W_Usuario
               Mov_Creditos.Int_Corriente = Creditos.Int_Corriente + creditos.INT_difCobro
               Mov_Creditos.Int_MorCobrar = Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob
               Mov_Creditos.Sdo_Capital = Creditos.Sdo_Capital
               Mov_Creditos.Cpte = 20
               Mov_Creditos.Descrip = "Cuota - $" + replace(STRING(cuotaAnterior,">>>,>>>,>>9")," ","") + " --> $" + replace(STRING(creditos.cuota,">>>,>>>,>>9")," ","").
    END.

    CLOSE QUERY Br_Pdctos.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cambiarTasa C-Win 
PROCEDURE cambiarTasa :
DEFINE VAR diasPeriodo AS INTEGER.
DEFINE VAR P_NMeses AS INTEGER.
DEFINE VAR numPeriodos AS INTEGER.
DEFINE VAR P_NomPer AS CHARACTER.
DEFINE VAR vTasa AS DECIMAL.
DEFINE VAR vPlazo AS INTEGER.
DEFINE VAR plazoAnterior AS INTEGER.
DEFINE VAR tasaAnterior AS DECIMAL.
DEFINE VAR interesPreinicio AS INTEGER.

/* Simulamos la modificación */
RUN HallarPeriodo IN W_ManFin (INPUT Creditos.Per_Pago,
                               INPUT Creditos.Plazo,
                               OUTPUT diasPeriodo,
                               OUTPUT P_NMeses,
                               OUTPUT numPeriodos,
                               OUTPUT P_NomPer).

RUN EFNV IN w_manfin (INPUT nuevaTasa / 100,
                      INPUT numPeriodos,
                      OUTPUT vTasa).

RUN interesPreinicio (INPUT diasPeriodo,
                      INPUT vTasa * 100,
                      OUTPUT interesPreinicio).

/* Recalculamos el plazo */
RUN HNCF IN W_ManFin (INPUT creditos.sdo_capital + interesPreinicio,
                      INPUT creditos.cuota,
                      INPUT vTasa,
                      OUTPUT vPlazo).

MESSAGE "El nuevo plazo para el crédito es de" vPlazo "periodos a una tasa periodica del" STRING(vTasa * 100,"zz9.99%") SKIP
        "Desea realizar el cambio...?"
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "" UPDATE flagYes AS LOGICAL.

IF flagYes = TRUE THEN DO:
    tasaAnterior = creditos.tasa.
    plazoAnterior = creditos.plazo.

    creditos.fec_pagAnti = creditos.fec_pago.
    creditos.fec_desembolso = w_fecha.
    creditos.cuo_pagadas = 0.
    creditos.monto = creditos.sdo_Capital.
    creditos.plazo = vPlazo.
    creditos.tasa = vTasa * 100 * numPeriodos.

    RUN CrearControlPagos.r(INPUT creditos.nit,
                            INPUT creditos.num_credito).

    CREATE Mov_Creditos.
    ASSIGN Mov_Creditos.Agencia = Creditos.Agencia
           Mov_Creditos.Cod_Credito = Creditos.Cod_Credito
           Mov_Creditos.Nit = Creditos.Nit
           Mov_Creditos.Num_Credito = Creditos.Num_Credito
           Mov_Creditos.Cod_Operacion = 999999999
           Mov_Creditos.Ofi_Destino = Creditos.Agencia
           Mov_Creditos.Ofi_Fuente = W_Agencia
           Mov_Creditos.Pagare = Creditos.Pagare
           Mov_Creditos.Fecha = W_Fecha
           Mov_Creditos.Hora = vTime
           Mov_Creditos.Usuario = W_Usuario
           Mov_Creditos.Int_Corriente = Creditos.Int_Corriente + creditos.INT_difCobro
           Mov_Creditos.Int_MorCobrar = Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob
           Mov_Creditos.Sdo_Capital = Creditos.Sdo_Capital
           Mov_Creditos.Cpte = 20
           Mov_Creditos.Descrip = "Tasa - " + replace(STRING(tasaAnterior / numperiodos,">>9.99")," ","") + "% --> " + replace(STRING(creditos.tasa / numPeriodos,">>9.99")," ","") + "%".

    CREATE Mov_Creditos.
    ASSIGN Mov_Creditos.Agencia = Creditos.Agencia
           Mov_Creditos.Cod_Credito = Creditos.Cod_Credito
           Mov_Creditos.Nit = Creditos.Nit
           Mov_Creditos.Num_Credito = Creditos.Num_Credito
           Mov_Creditos.Cod_Operacion = 999999999
           Mov_Creditos.Ofi_Destino = Creditos.Agencia
           Mov_Creditos.Ofi_Fuente = W_Agencia
           Mov_Creditos.Pagare = Creditos.Pagare
           Mov_Creditos.Fecha = W_Fecha
           Mov_Creditos.Hora = vTime
           Mov_Creditos.Usuario = W_Usuario
           Mov_Creditos.Int_Corriente = Creditos.Int_Corriente + creditos.INT_difCobro
           Mov_Creditos.Int_MorCobrar = Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob
           Mov_Creditos.Sdo_Capital = Creditos.Sdo_Capital
           Mov_Creditos.Cpte = 20
           Mov_Creditos.Descrip = "Plazo - " + STRING(plazoAnterior) + " --> " + STRING(creditos.plazo).

    CLOSE QUERY Br_Pdctos.
END.

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
  DISPLAY W_NitCte W_NomCte tasaActual nuevaTasa CuotaActual nuevaCuota 
          PlazoActual NuevoPlazo fecPagoActual nuevaFecPago fecPrimerPagoActual 
          nuevaFecPrimerPago rsPeriodicidad rsFormaDePago tgCongelado 
      WITH FRAME F_Creditos IN WINDOW C-Win.
  ENABLE RECT-2 RECT-3 RECT-4 RECT-5 W_NitCte btnTasa btnCuota btnPlazo 
         btnFecPago btnFecPrimerPago btnPeriodicidadDePago btnFormaDePago 
         tgCongelado BUTTON-2 
      WITH FRAME F_Creditos IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-F_Creditos}
  ENABLE Br_Pdctos 
      WITH FRAME F_Productos IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-F_Productos}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE interesPreinicio C-Win 
PROCEDURE interesPreinicio :
DEFINE INPUT PARAMETER diasPeriodo AS INTEGER.
DEFINE INPUT PARAMETER vTasa AS DECIMAL.
DEFINE OUTPUT PARAMETER interesPreinicio AS INTEGER.

DEFINE VAR diasPreinicio AS INTEGER.

diasPreinicio = (creditos.fec_pago - w_fecha) - diasPeriodo.

IF diasPreinicio > 0 THEN DO:
    interesPreinicio = (((creditos.sdo_capital / 100) * vTasa) / diasPeriodo) * diasPreinicio.
END.
ELSE
    interesPreinicio = 0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir C-Win 
PROCEDURE ProcesoImprimir :
DEFINE VAR W_NomPer AS CHAR FORM "X(15)" INITIAL "Mensual".
DEFINE VAR W_PdoAno AS INTEG FORM "99" INITIAL 12.
DEFINE VAR W_NroDias AS INT.
DEFINE VAR P_NMeses AS INT.
DEFINE VAR FecIni AS DATE.
DEFINE VAR w_fec AS DATE.
DEFINE VAR w_fec1 AS DATE.
DEFINE VAR numExtras AS INTEGER.
DEFINE VAR W_Tot_Cre AS DECIMAL.

/*FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ Creditos.Cod_Credito NO-LOCK NO-ERROR.

RUN HallarPeriodo IN W_ManFin (INPUT Creditos.Per_Pago,
                               INPUT /*creditos.plazo*/ INTEGER(txtPlazoNuevo:SCREEN-VALUE IN FRAME F_Creditos),
                               OUTPUT W_NroDias,
                               OUTPUT P_NMeses,
                               OUTPUT W_PdoAno,
                               OUTPUT W_NomPer).

/*DO WHILE MONTH(fecIni) = MONTH(TODAY):
    fecIni = fecIni - 1.
END.*/

fecIni = txtFecPago-2.

/* Se halla el número de cuotas extras para enviarlo a la rutina */
FOR EACH extras WHERE extras.agencia = creditos.agencia
                  AND extras.nit = creditos.nit
                  AND extras.cod_credito = creditos.cod_credito
                  AND extras.num_solicitud = creditos.num_solicitud NO-LOCK:
    numExtras = numExtras + 1.
END.

w_fec = /*Creditos.Fec_Desembolso*/ /*TODAY*/ fecIni.

/*Se corrige la fecha para Fodun*/
/*IF Creditos.Fec_PagAnti <> ? THEN
    w_Fec1 = Creditos.Fec_PagAnti.
ELSE
    w_Fec1 = Creditos.Fec_desembolso.*/

w_fec1 = fecIni.

IF flagControlPagos = FALSE THEN DO:
    RUN Proyeccion_Credito.R (INPUT /*creditos.monto*/ creditos.sdo_capital,
                              INPUT /*Creditos.Plazo*/ INTEGER(txtPlazoNuevo:SCREEN-VALUE),
                              INPUT /*Creditos.Cuota*/ DECIMAL(REPLACE(txtNuevaCuota:SCREEN-VALUE,"$","")),
                              INPUT numExtras,
                              INPUT w_fec,
                              INPUT w_fec1,
                              INPUT Creditos.Tasa / (W_PdoAno * 100),
                              INPUT /*(Creditos.Fec_PagAnti - creditos.Fec_Desembolso) - 1*/ w_Fec1 - TODAY - 1,
                              INPUT 0,
                              INPUT Creditos.Per_Pago,
                              INPUT 1,
                              INPUT 1,
                              INPUT Creditos.Nit,
                              INPUT W_NomCte:SCREEN-VALUE IN FRAME F_Creditos,
                              INPUT Creditos.Cod_Credito,
                              INPUT Pro_Creditos.Nom_Producto,
                              INPUT "S",
                              INPUT Creditos.Num_Solicitud,
                              INPUT "Cuota Fija",
                              INPUT W_NomPer,
                              INPUT 1,
                              INPUT /*creditos.monto*/ Creditos.sdo_capital).
END.
ELSE DO:
    FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ Creditos.Cod_Credito NO-LOCK NO-ERROR.

    RUN HallarPeriodo IN W_ManFin (INPUT Creditos.Per_Pago,
                                   INPUT creditos.plazo,
                                   OUTPUT W_NroDias,
                                   OUTPUT P_NMeses,
                                   OUTPUT W_PdoAno,
                                   OUTPUT W_NomPer).

    /* Se halla el número de cuotas extras para enviarlo a la rutina */
    FOR EACH extras WHERE extras.agencia = creditos.agencia
                      AND extras.nit = creditos.nit
                      AND extras.cod_credito = creditos.cod_credito
                      AND extras.num_solicitud = creditos.num_solicitud NO-LOCK:
        numExtras = numExtras + 1.
    END.

    FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                             AND CONTROL_pagos.num_credito = creditos.num_credito:
        DELETE CONTROL_pagos.
    END.

    creditos.fec_pagAnti = fecIni.
    creditos.fec_pago = fecIni.
    creditos.cuo_pagadas = 0.

    RUN Proyeccion_Desembolso.R (INPUT Creditos.Agencia,
                                 INPUT creditos.sdo_capital,
                                 INPUT creditos.plazo,
                                 INPUT creditos.cuota,
                                 INPUT W_TotExt,
                                 INPUT /*Creditos.Fec_Aprobacion*/ TODAY,
                                 INPUT Creditos.Fec_PagAnti,
                                 INPUT (creditos.tasa / 100) / W_PdoAno,
                                 INPUT creditos.incremento,
                                 INPUT 0,
                                 INPUT creditos.per_pago,
                                 INPUT creditos.FOR_interes,
                                 INPUT creditos.sistema,
                                 INPUT Creditos.Nit,
                                 INPUT "",
                                 INPUT Creditos.Cod_Credito,   
                                 INPUT "",
                                 INPUT "X",
                                 INPUT creditos.num_credito,
                                 INPUT "",
                                 INPUT "",
                                 INPUT 1,
                                 INPUT Creditos.Monto).

    W_Tot_Cre = Creditos.monto.

    FOR EACH CONTROL_pagos WHERE control_pagos.Nit EQ Creditos.Nit
                             and control_pagos.Cod_Credito EQ Creditos.Cod_Credito
                             and control_pagos.Num_Credito EQ Creditos.Num_Credito BY control_pagos.Nro_Cuota:
        IF control_pagos.Nro_Cuota = 1 THEN
            control_pagos.Pagos_IntAcum = ((creditos.monto * ((Creditos.Tasa / W_PdoAno) / 100)) / W_NroDias) * (Creditos.Fec_PagAnti - TODAY).
        ELSE
            control_pagos.Pagos_IntAcum = ROUND(W_Tot_Cre * ((Creditos.Tasa / 100) / W_PdoAno), 0).

        control_pagos.pagos_capitalAcum = creditos.cuota - control_pagos.Pagos_IntAcum.

        FIND FIRST extras WHERE Extras.Nit EQ Creditos.Nit
                            AND extras.num_solicitud = creditos.num_solicitud
                            AND extras.cod_credito = creditos.cod_credito
                            AND Extras.Nro_Cuota EQ control_pagos.Nro_Cuota NO-LOCK NO-ERROR.
        IF AVAILABLE extras THEN
            W_Tot_Cre = W_Tot_Cre - control_pagos.pagos_capitalAcum - Extras.Vr_CuoExtra.
        ELSE
            W_Tot_Cre = W_Tot_Cre - control_pagos.pagos_capitalAcum.

        IF CONTROL_pagos.Id_PdoMes = 2 THEN
            ASSIGN control_pagos.Cap_pagado = control_pagos.pagos_capitalAcum
                   control_pagos.Int_pagado = control_pagos.Pagos_IntAcum
                   creditos.cuo_pagadas = creditos.cuo_pagadas + 1.
    END.
END.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

