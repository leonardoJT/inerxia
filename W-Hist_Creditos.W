&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME W-Hist_Creditos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Hist_Creditos 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

ON RETURN TAB.

{Incluido/Variable.I "SHARED"}
{Incluido/VARCON.I "SHARED"}

/* oakley */

   DEFINE INPUT PARAMETER P_CedNit LIKE Creditos.Nit.
   DEFINE INPUT PARAMETER P_NumCre LIKE Creditos.Num_Credito.

   DEFI VAR DMora    AS INTEG FORM "9999"      INIT 0.
   DEFI VAR W_IntAcu LIKE Creditos.Sdo_Capital INIT 0.
   DEFI VAR W_SiProy AS LOG INIT FALSE.

   DEF TEMP-TABLE tMvmientoCrdto NO-UNDO
       FIELD tFcha AS DATE
       FIELD tOprcion AS CHAR
       FIELD tNmroTrnsccion AS CHAR
       FIELD tCrgo AS DECIMAL
       FIELD tAbno AS DECIMAL
       FIELD tTtalTrnsccion AS DECIMAL
       FIELD tSldoCptal AS DECIMAL
       FIELD tAgncia AS INTEGER
       FIELD tHra AS INTEGER
       INDEX pk tfcha DESC thra DESC.

DEF VAR lMovCrdto AS LOGICAL NO-UNDO.




    /*  ABRE SOLICITUDES DIGITALIZADAS - WILLIAM MARTINEZ RUIZ 24-11-2008  */


/*************************************** Raiz Imagenes ******************************************
*
*/
/*DEFINE VARIABLE cRaiz AS CHARACTER INITIAL "\\192.168.1.111\progress\objetos\Imagenes\SOLICITUDES\" NO-UNDO.*/
/*
*
*************************************************************************************************/

DEFINE VARIABLE cFullPathName AS CHARACTER FORMAT "x(250)" NO-UNDO.
DEFINE VARIABLE hInstance AS INTEGER   NO-UNDO.
/*
{windows.i}
{winfunc.i}

    PROCEDURE ShellExecute{&A} EXTERNAL "shell32" :
       DEFINE INPUT PARAMETER HWND AS LONG.
       DEFINE INPUT PARAMETER lpOperation AS CHARACTER.
       DEFINE INPUT PARAMETER lpFile AS CHARACTER.
       DEFINE INPUT PARAMETER lpParameters AS CHARACTER.
       DEFINE INPUT PARAMETER lpDirectory AS CHARACTER.
       DEFINE INPUT PARAMETER nShowCmd AS LONG.
       DEFINE RETURN PARAMETER hInstance AS LONG.
    END.

    DEFINE VARIABLE RutaFoto AS CHARACTER FORMAT "x(80)"   NO-UNDO.

/*************************************************************************************************/

*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Proc
&Scoped-define BROWSE-NAME Br_Cred

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Creditos

/* Definitions for BROWSE Br_Cred                                       */
&Scoped-define FIELDS-IN-QUERY-Br_Cred Creditos.Num_Credito Creditos.Pagare Creditos.Fec_Desembolso Creditos.Sdo_Capital Creditos.Cuota Creditos.For_Pago Creditos.Per_Pago   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Cred   
&Scoped-define SELF-NAME Br_Cred
&Scoped-define QUERY-STRING-Br_Cred FOR EACH Creditos WHERE Creditos.Nit EQ P_CedNit                             NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br_Cred OPEN QUERY Br_Cred FOR EACH Creditos WHERE Creditos.Nit EQ P_CedNit                             NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_Cred Creditos
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Cred Creditos


/* Definitions for FRAME F_Proc                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Proc ~
    ~{&OPEN-QUERY-Br_Cred}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-319 Br_Cred Btn_ImpProy Rs_Cons ~
btnProyectarValor FechaProyecta BUTTON-5 Btn_Done 
&Scoped-Define DISPLAYED-OBJECTS W_NitCte W_NomCte Rs_Cons FechaProyecta 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fPMvtos W-Hist_Creditos 
FUNCTION fPMvtos RETURNS LOGICAL
  (cnit AS CHAR,iNmroCrdto AS INTEGER /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Hist_Creditos AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnProyectarValor 
     LABEL "Proyectar Saldo" 
     SIZE 20 BY 1.19.

DEFINE BUTTON Btn_Done DEFAULT 
     LABEL "&Salir" 
     SIZE 10.29 BY 1.73
     BGCOLOR 8 .

DEFINE BUTTON Btn_ImpProy 
     LABEL "Proyección Pagos" 
     SIZE 34 BY 1.19.

DEFINE BUTTON BUTTON-5 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 5" 
     SIZE 9.72 BY 1.96.

DEFINE VARIABLE FechaProyecta AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 13.14 BY .85
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NitCte AS CHARACTER FORMAT "X(12)":U 
     LABEL "Ced/Nit" 
     VIEW-AS FILL-IN 
     SIZE 14.14 BY .85 TOOLTIP "Ced/Nit del Cliente"
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomCte AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY .85 TOOLTIP "Nombre del Cliente"
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Rs_Cons AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Resumido", 1,
"Detallado", 2
     SIZE 12.14 BY 2.42
     BGCOLOR 17 FGCOLOR 0  NO-UNDO.

DEFINE RECTANGLE RECT-319
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 14.72 BY 3.08.

DEFINE BUTTON btnMvmientos 
     LABEL "Movimientos" 
     SIZE 18 BY 1.12 TOOLTIP "Movimientos Créditos".

DEFINE BUTTON ReimprimirPagare 
     LABEL "Reimprimir Pagare" 
     SIZE 18 BY 1.12.

DEFINE VARIABLE dePrbbldadIncmplimiento AS DECIMAL FORMAT "9.99":U INITIAL 0 
     LABEL "Probabilidad Incumplimiento" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 TOOLTIP "Probabilidad De Incumplimiento"
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE dePrddaEsprda AS DECIMAL FORMAT "9.99":U INITIAL 0 
     LABEL "Pérdida Esperada" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 TOOLTIP "Pérdida Esperada"
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CapVdo AS DECIMAL FORMAT ">>>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Capital Vencido" 
     VIEW-AS FILL-IN 
     SIZE 15.86 BY .88
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CuoMaxMora AS INTEGER FORMAT ">,>>9":U INITIAL 0 
     LABEL "Cuotas Máx. Vencidas" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .85
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CuoUnica AS DECIMAL FORMAT ">>>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Por Cuota Única" 
     VIEW-AS FILL-IN 
     SIZE 15.86 BY .88
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_DiasMora AS INTEGER FORMAT "->,>>9":U INITIAL 0 
     LABEL "Dias de Mora" 
     VIEW-AS FILL-IN 
     SIZE 7.14 BY .85
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_DMaxMora AS INTEGER FORMAT ">,>>9":U INITIAL 0 
     LABEL "Dias Máx. en Mora" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .85
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_FecPMora AS DATE FORMAT "99/99/9999":U 
     LABEL "Fec. Primera Mora" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .85
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_FecPri AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha 1er.Crédito" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .85
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_FecProxP AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Prox.Pago" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .85
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_FecUlt AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Ult.Crédito" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .85
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_FijaCaja AS DECIMAL FORMAT ">>>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Cuotas Fijas X Caja" 
     VIEW-AS FILL-IN 
     SIZE 15.86 BY .88
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_FijaNomi AS DECIMAL FORMAT ">>>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Cuotas Fijas Nómina" 
     VIEW-AS FILL-IN 
     SIZE 15.86 BY .88
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_FUltMora AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Última Mora" 
     VIEW-AS FILL-IN 
     SIZE 15.14 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_IMora AS DECIMAL FORMAT ">>>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "       Interés  X Mora" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .85
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_IntVdo AS DECIMAL FORMAT ">>>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Interés Por pagar" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_MoraMax AS DECIMAL FORMAT ">>>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Valor Mora Máxima" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NroCastig AS INTEGER FORMAT ">,>>9":U INITIAL 0 
     LABEL "#Créditos Castigados" 
     VIEW-AS FILL-IN 
     SIZE 6.29 BY .88
     BGCOLOR 12 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NroCobJur AS INTEGER FORMAT ">,>>9":U INITIAL 0 
     LABEL "#Créditos Cobro-Jurìdico" 
     VIEW-AS FILL-IN 
     SIZE 6.29 BY .88
     BGCOLOR 12 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NroMora AS INTEGER FORMAT ">,>>9":U INITIAL 0 
     LABEL "Nro.Veces en Mora" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NroTot AS INTEGER FORMAT ">,>>9":U INITIAL 0 
     LABEL "Nro.de Créditos" 
     VIEW-AS FILL-IN 
     SIZE 6.29 BY .88
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NroVig AS INTEGER FORMAT ">,>>9":U INITIAL 0 
     LABEL "Nro.Vigentes" 
     VIEW-AS FILL-IN 
     SIZE 6.14 BY .88
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Otros AS DECIMAL FORMAT ">>>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "X Cobro Jurídico" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .85
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_SdoAct AS DECIMAL FORMAT "->>>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Saldo Deuda Actual" 
     VIEW-AS FILL-IN 
     SIZE 15.86 BY .88
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_TotMora AS DECIMAL FORMAT ">>>>,>>>,>>9.99":U INITIAL 0 
     LABEL ".....Por Valor de" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_VrTot AS DECIMAL FORMAT ">>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Vr.Tot.Prestado" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_VrVig AS DECIMAL FORMAT ">>>>,>>>,>>9.99":U INITIAL 0 
     LABEL ".....Por Valor de" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-320
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 37 BY 10.38.

DEFINE RECTANGLE RECT-321
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 37 BY 3.69.

DEFINE RECTANGLE RECT-322
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 45 BY 10.42.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Br_Cred FOR 
      Creditos SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Br_Cred
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Cred W-Hist_Creditos _FREEFORM
  QUERY Br_Cred NO-LOCK DISPLAY
      Creditos.Num_Credito FORMAT "999999999":U        COLUMN-LABEL "No.Credito"
      Creditos.Pagare FORMAT "999999999":U             COLUMN-LABEL "Pagare"
      Creditos.Fec_Desembolso FORMAT "99/99/99":U      COLUMN-LABEL "F-Desemb"
      Creditos.Sdo_Capital FORMAT "->>>>,>>>,>>9":U    COLUMN-LABEL "Sdo-Capital"
      Creditos.Cuota FORMAT ">>>>>>>,>>9":U            COLUMN-LABEL "Vr.Cuota"
      Creditos.For_Pago  FORM "9"                      COLUMN-LABEL "FP:CND"
      Creditos.Per_Pago  FORM "9"                      COLUMN-LABEL "PP:SDQM"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 59.72 BY 5
         BGCOLOR 18 FGCOLOR 15 FONT 4
         TITLE BGCOLOR 18 FGCOLOR 15 "Creditos del Cliente" ROW-HEIGHT-CHARS .54 FIT-LAST-COLUMN TOOLTIP "Con Click selecciona el Credito para el Detallado del PlanPagos".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Proc
     Br_Cred AT ROW 1.08 COL 53
     W_NitCte AT ROW 1.65 COL 7.86 COLON-ALIGNED
     W_NomCte AT ROW 1.65 COL 22 COLON-ALIGNED NO-LABEL
     Btn_ImpProy AT ROW 2.96 COL 2
     Rs_Cons AT ROW 3.15 COL 38 NO-LABEL
     btnProyectarValor AT ROW 4.65 COL 2.29 WIDGET-ID 2
     FechaProyecta AT ROW 4.81 COL 20.86 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     BUTTON-5 AT ROW 6.96 COL 100.57
     Btn_Done AT ROW 18.19 COL 100.43 HELP
          "Sale del proceso de Depreciación y Ajustes"
     RECT-319 AT ROW 2.88 COL 36.43
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 112.14 BY 20.81
         BGCOLOR 17 FGCOLOR 0 FONT 5.

DEFINE FRAME F_Resumen
     W_NroTot AT ROW 1.15 COL 14.86 COLON-ALIGNED
     W_VrTot AT ROW 1.19 COL 37.86 COLON-ALIGNED
     W_CuoUnica AT ROW 1.54 COL 77 COLON-ALIGNED
     W_NroVig AT ROW 2.15 COL 14.86 COLON-ALIGNED
     W_VrVig AT ROW 2.15 COL 37.86 COLON-ALIGNED
     W_FijaCaja AT ROW 2.62 COL 77 COLON-ALIGNED
     W_FecPri AT ROW 3.08 COL 37.86 COLON-ALIGNED
     W_FijaNomi AT ROW 3.69 COL 77 COLON-ALIGNED
     W_FecUlt AT ROW 4.04 COL 37.86 COLON-ALIGNED
     W_NroCobJur AT ROW 5.81 COL 36.86 COLON-ALIGNED
     W_FecProxP AT ROW 6.27 COL 77 COLON-ALIGNED
     W_NroCastig AT ROW 6.77 COL 36.86 COLON-ALIGNED
     W_SdoAct AT ROW 7.19 COL 77 COLON-ALIGNED
     dePrbbldadIncmplimiento AT ROW 8 COL 37 COLON-ALIGNED WIDGET-ID 6
     W_CapVdo AT ROW 8.27 COL 77 COLON-ALIGNED
     dePrddaEsprda AT ROW 9.08 COL 37 COLON-ALIGNED WIDGET-ID 8
     W_IntVdo AT ROW 9.35 COL 77 COLON-ALIGNED
     W_IMora AT ROW 10.42 COL 77 COLON-ALIGNED
     W_DiasMora AT ROW 11.5 COL 77 COLON-ALIGNED
     W_Otros AT ROW 12.31 COL 77 COLON-ALIGNED
     ReimprimirPagare AT ROW 12.85 COL 25 WIDGET-ID 10
     btnMvmientos AT ROW 14.19 COL 25 WIDGET-ID 2
     W_NroMora AT ROW 14.46 COL 77 COLON-ALIGNED
     W_FecPMora AT ROW 14.46 COL 77 COLON-ALIGNED
     W_DMaxMora AT ROW 14.46 COL 77 COLON-ALIGNED NO-TAB-STOP 
     W_MoraMax AT ROW 14.46 COL 77 COLON-ALIGNED
     W_TotMora AT ROW 14.46 COL 77 COLON-ALIGNED
     W_CuoMaxMora AT ROW 14.46 COL 77 COLON-ALIGNED NO-TAB-STOP 
     W_FUltMora AT ROW 14.5 COL 77 COLON-ALIGNED
     " Historial de Morosidad" VIEW-AS TEXT
          SIZE 20.29 BY .81 AT ROW 5 COL 14.14
          FGCOLOR 7 
     " Vr.Cuotas Vigentes" VIEW-AS TEXT
          SIZE 16.72 BY .62 AT ROW 1 COL 60.72
          BGCOLOR 17 FGCOLOR 7 
     "    Información de Créditos Vigentes" VIEW-AS TEXT
          SIZE 30.14 BY .81 AT ROW 5.08 COL 60.86
          BGCOLOR 17 FGCOLOR 7 
     RECT-320 AT ROW 5.27 COL 58.86
     RECT-321 AT ROW 1.23 COL 58.86
     RECT-322 AT ROW 5.27 COL 12
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.57 ROW 6.12
         SIZE 96.14 BY 15.69
         BGCOLOR 17 FGCOLOR 0 FONT 5
         TITLE "Resumen Historial Crediticio en la Entidad".


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
  CREATE WINDOW W-Hist_Creditos ASSIGN
         HIDDEN             = YES
         TITLE              = "Historial Crediticio, Programa W-Hist_Creditos.W"
         HEIGHT             = 20.81
         WIDTH              = 112.14
         MAX-HEIGHT         = 26.69
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 26.69
         VIRTUAL-WIDTH      = 146.29
         RESIZE             = yes
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



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Hist_Creditos
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME F_Resumen:FRAME = FRAME F_Proc:HANDLE.

/* SETTINGS FOR FRAME F_Proc
   FRAME-NAME                                                           */
/* BROWSE-TAB Br_Cred RECT-319 F_Proc */
/* SETTINGS FOR FILL-IN W_NitCte IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomCte IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Resumen
                                                                        */
/* SETTINGS FOR FILL-IN dePrbbldadIncmplimiento IN FRAME F_Resumen
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dePrddaEsprda IN FRAME F_Resumen
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_CapVdo IN FRAME F_Resumen
   NO-ENABLE                                                            */
ASSIGN 
       W_CapVdo:HIDDEN IN FRAME F_Resumen           = TRUE.

/* SETTINGS FOR FILL-IN W_CuoMaxMora IN FRAME F_Resumen
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       W_CuoMaxMora:HIDDEN IN FRAME F_Resumen           = TRUE.

/* SETTINGS FOR FILL-IN W_CuoUnica IN FRAME F_Resumen
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_DiasMora IN FRAME F_Resumen
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_DMaxMora IN FRAME F_Resumen
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       W_DMaxMora:HIDDEN IN FRAME F_Resumen           = TRUE.

/* SETTINGS FOR FILL-IN W_FecPMora IN FRAME F_Resumen
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       W_FecPMora:HIDDEN IN FRAME F_Resumen           = TRUE.

/* SETTINGS FOR FILL-IN W_FecPri IN FRAME F_Resumen
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_FecProxP IN FRAME F_Resumen
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_FecUlt IN FRAME F_Resumen
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_FijaCaja IN FRAME F_Resumen
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_FijaNomi IN FRAME F_Resumen
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_FUltMora IN FRAME F_Resumen
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       W_FUltMora:HIDDEN IN FRAME F_Resumen           = TRUE.

/* SETTINGS FOR FILL-IN W_IMora IN FRAME F_Resumen
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_IntVdo IN FRAME F_Resumen
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_MoraMax IN FRAME F_Resumen
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       W_MoraMax:HIDDEN IN FRAME F_Resumen           = TRUE.

/* SETTINGS FOR FILL-IN W_NroCastig IN FRAME F_Resumen
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NroCobJur IN FRAME F_Resumen
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NroMora IN FRAME F_Resumen
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       W_NroMora:HIDDEN IN FRAME F_Resumen           = TRUE.

/* SETTINGS FOR FILL-IN W_NroTot IN FRAME F_Resumen
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NroVig IN FRAME F_Resumen
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Otros IN FRAME F_Resumen
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_SdoAct IN FRAME F_Resumen
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_TotMora IN FRAME F_Resumen
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       W_TotMora:HIDDEN IN FRAME F_Resumen           = TRUE.

/* SETTINGS FOR FILL-IN W_VrTot IN FRAME F_Resumen
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_VrVig IN FRAME F_Resumen
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Hist_Creditos)
THEN W-Hist_Creditos:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_Cred
/* Query rebuild information for BROWSE Br_Cred
     _START_FREEFORM
OPEN QUERY Br_Cred FOR EACH Creditos WHERE Creditos.Nit EQ P_CedNit
                            NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE Br_Cred */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Proc
/* Query rebuild information for FRAME F_Proc
     _Query            is NOT OPENED
*/  /* FRAME F_Proc */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Resumen
/* Query rebuild information for FRAME F_Resumen
     _Query            is NOT OPENED
*/  /* FRAME F_Resumen */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Hist_Creditos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Hist_Creditos W-Hist_Creditos
ON END-ERROR OF W-Hist_Creditos /* Historial Crediticio, Programa W-Hist_Creditos.W */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Hist_Creditos W-Hist_Creditos
ON WINDOW-CLOSE OF W-Hist_Creditos /* Historial Crediticio, Programa W-Hist_Creditos.W */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br_Cred
&Scoped-define SELF-NAME Br_Cred
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Cred W-Hist_Creditos
ON MOUSE-SELECT-CLICK OF Br_Cred IN FRAME F_Proc /* Creditos del Cliente */
DO:
   IF AVAIL(Creditos) THEN
      P_NumCre = Creditos.Num_Credito.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Cred W-Hist_Creditos
ON MOUSE-SELECT-DBLCLICK OF Br_Cred IN FRAME F_Proc /* Creditos del Cliente */
DO:
  
  /*cFullPathName = string(cRaiz + "SOLICITUD ") + string(creditos.num_solicitud, "99999") + ".tif".

  RUN ShellExecute{&A} IN hpApi 
                   (0,
                    "open",
                    cFullPathName,
                    "",
                    "",
                    1,
                    OUTPUT hInstance).*/

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Resumen
&Scoped-define SELF-NAME btnMvmientos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMvmientos W-Hist_Creditos
ON CHOOSE OF btnMvmientos IN FRAME F_Resumen /* Movimientos */
DO:
    DEFI   VAR Listado    AS CHAR FORM "X(40)".
    DEFINE VAR W_Rpta     AS LOGICAL.
    lMovCrdto = TRUE.
    fPMvtos(P_CedNit,P_NumCre).
    SESSION:SET-WAIT-STATE("GENERAL").

    ASSIGN Listado  = W_PathSpl + W_Usuario + "_MovCred.Lst"
           W_SiProy = TRUE.

    {Incluido\Imprimir.I "listado"}

    W_SiProy = FALSE.
    SESSION:SET-WAIT-STATE("").
    lMovCrdto = FALSE.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Proc
&Scoped-define SELF-NAME btnProyectarValor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnProyectarValor W-Hist_Creditos
ON CHOOSE OF btnProyectarValor IN FRAME F_Proc /* Proyectar Saldo */
DO:
    DEFINE VAR tasaDiaria AS DECIMAL.
    DEFINE VAR tasaDiariaMora AS DECIMAL.
    DEFINE VAR interesCorriente AS DECIMAL.
    DEFINE VAR interesMora AS DECIMAL.
    DEFINE VAR dias AS INTEGER.
    DEFINE VAR fechaAux AS DATE.
    DEFINE VAR cont AS INTEGER.
    DEFINE VAR proyectado AS DECIMAL.
    DEFINE VAR interesProyectado AS DECIMAL.
    DEFINE VAR interesMoraProyectado AS DECIMAL.

    IF fechaProyecta < TODAY THEN DO:
        MESSAGE "La fecha de proyección debe ser posterior al día de hoy!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN NO-APPLY.
    END.

    IF fechaProyecta = w_fecha THEN DO:
        MESSAGE "Estado del crédito #" creditos.num_credito "al" fechaProyecta ":" SKIP
                "Saldo Capital    :" STRING(creditos.sdo_capital,"$->>>,>>>,>>9.99") SKIP
                "Interés corriente:" STRING(creditos.INT_corriente,"$->>>,>>>,>>9.99") SKIP
                "Interés de Mora  :" STRING(credito.INT_morCobrar,"$->>>,>>>,>>9.99") SKIP
                "Total deuda      :" STRING(creditos.sdo_capital + creditos.INT_corriente + credito.INT_morCobrar,"$->>>,>>>,>>9.99")
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.

    IF fechaProyecta > w_fecha THEN DO:
        RUN p-proyectarLiquidacion.r(INPUT creditos.nit,
                                     INPUT creditos.cod_credito,
                                     INPUT creditos.num_credito,
                                     INPUT creditos.tasa,
                                     INPUT creditos.sdo_capital,
                                     INPUT creditos.INT_corriente,
                                     INPUT creditos.INT_morCobrar,
                                     INPUT creditos.fec_pago,
                                     INPUT fechaProyecta,
                                     OUTPUT interesProyectado,
                                     OUTPUT interesMoraProyectado) NO-ERROR.

        MESSAGE "Estado del crédito #" creditos.num_credito "al" fechaProyecta ":" SKIP
                "Saldo Capital    :" STRING(creditos.sdo_capital,"$->>>,>>>,>>9.99") SKIP
                "Interés corriente:" STRING(interesProyectado + creditos.INT_difCobro,"$->>>,>>>,>>9.99") SKIP
                "Interés de Mora  :" STRING(interesMoraProyectado + creditos.INT_MoraDifCob,"$->>>,>>>,>>9.99") SKIP
                "Total deuda      :" STRING(Creditos.Sdo_Capital + interesProyectado + Creditos.Int_DifCobro + Creditos.Int_MoraDifCob + interesMoraProyectado + Creditos.Costas + Creditos.Polizas + Creditos.Honorarios - Creditos.Int_Anticipado,"$->>>,>>>,>>9.99")
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done W-Hist_Creditos
ON CHOOSE OF Btn_Done IN FRAME F_Proc /* Salir */
DO:     
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_ImpProy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_ImpProy W-Hist_Creditos
ON CHOOSE OF Btn_ImpProy IN FRAME F_Proc /* Proyección Pagos */
DO:
    DEFINE VAR Listado AS CHAR FORM "X(40)".
    DEFINE VAR W_Rpta AS LOGICAL.

    SESSION:SET-WAIT-STATE("GENERAL").

    ASSIGN Listado  = W_PathSpl + W_Usuario + "_ProyCred.Lst"
           W_SiProy = TRUE.

    {Incluido\Imprimir.I "listado"}

    W_SiProy = FALSE.

    SESSION:SET-WAIT-STATE("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 W-Hist_Creditos
ON CHOOSE OF BUTTON-5 IN FRAME F_Proc /* Button 5 */
DO:
  RUN W-InfDia.R.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FechaProyecta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FechaProyecta W-Hist_Creditos
ON LEAVE OF FechaProyecta IN FRAME F_Proc
DO:
    ASSIGN fechaProyecta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Resumen
&Scoped-define SELF-NAME ReimprimirPagare
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ReimprimirPagare W-Hist_Creditos
ON CHOOSE OF ReimprimirPagare IN FRAME F_Resumen /* Reimprimir Pagare */
DO:
    RUN F-PagPreCoop.r ( INPUT Creditos.Nit, DECIMAL(creditos.num_credito),  creditos.fec_paganti).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Proc
&Scoped-define SELF-NAME Rs_Cons
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Rs_Cons W-Hist_Creditos
ON MOUSE-SELECT-CLICK OF Rs_Cons IN FRAME F_Proc
DO:
  DEFI   VAR Listado    AS CHAR FORM "X(40)".
  DEFINE VAR W_Rpta     AS LOGICAL.

  ASSIGN Rs_cons.
  
  IF Rs_Cons EQ 2 THEN DO:
     SESSION:SET-WAIT-STATE("GENERAL").
     
     Listado = W_PathSpl +  W_Usuario + "_DetaCred.Lst".
                                                                                           
     {Incluido\Imprimir.I "listado"}
     
     SESSION:SET-WAIT-STATE("").
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Hist_Creditos 


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

    FIND FIRST Clientes WHERE Clientes.Nit EQ P_CedNit NO-LOCK NO-ERROR.
    IF NOT AVAIL(Clientes) THEN DO:
        MESSAGE "Cliente Inexistente...Revise por favor"
            VIEW-AS ALERT-BOX.

        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

    ASSIGN W_NitCte:SCREEN-VALUE = P_CedNit
           W_NomCte:SCREEN-VALUE = TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2) + " " + TRIM(Clientes.Nombre)
           W_NitCte
           W_NomCte
           fechaProyecta:SCREEN-VALUE = STRING(w_fecha,"99/99/9999")
           fechaProyecta = w_fecha.



    RUN Resumen.

    OPEN QUERY Br_Cred FOR EACH Creditos WHERE Creditos.Nit EQ P_CedNit
                                           AND (Creditos.Estado EQ 2 OR Creditos.Estado EQ 3 OR Creditos.Estado EQ 5) NO-LOCK INDEXED-REPOSITION.

    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Detallado W-Hist_Creditos 
PROCEDURE Detallado :
/*------------------------------------------------------------------------------
  Purpose:     
------------------------------------------------------------------------------*/
  DEFI VAR INTLiq      LIKE Creditos.Sdo_Capital INIT 0.
  DEFI VAR CapLiq      LIKE Creditos.Sdo_Capital INIT 0.
  DEFI VAR SdoPro      LIKE Creditos.Sdo_Capital INIT 0.
  DEFI VAR W_PdoPag    AS INTEG FORM "99".
  DEFI VAR W_PdoLiq    AS INTEG FORM "99" INIT 12.
  DEFI VAR W_Tasa      LIKE Creditos.Tasa.
  DEFI VAR INT_MGracia LIKE Creditos.Sdo_Capital INIT 0.
  DEFI VAR W_TasaUs     LIKE Indicadores.Tasa.

  FIND Entidad WHERE Entidad.Entidad EQ W_Entidad NO-LOCK NO-ERROR.
  FIND FIRST Indicadores WHERE Indicadores.Indicador EQ Entidad.Ind_Usura
                           AND Indicadores.Estado    EQ 1 
                           AND Indicadores.FecVcto   GE W_Fecha NO-LOCK NO-ERROR.
  IF NOT AVAILABLE(Indicadores) THEN DO:
     MESSAGE "Falta Indicador de Usura Vigente, No se permite el abono al Credito."
         VIEW-AS ALERT-BOX TITLE "Programa AboCredito.P".
     RETURN ERROR.
  END.
  
  W_TasaUs = Indicadores.Tasa.
  RUN EFNV IN W_ManFin  (INPUT W_TasaUs / 100, 12, OUTPUT W_TasaUS).
  W_TasaUS = W_TasaUS * 1200.

  
  {Incluido\RepEncabezado.I}

  ASSIGN W_Reporte    = "Reporte   : Detalle Crediticio Por C/Obligaciòn                       Fecha : " +
                         STRING(W_Fecha,"99/99/9999") + "   Hora :" + STRING(TIME,"HH:MM:SS")
         W_EncColumna = "Cliente : " + W_NitCte + " " + W_NomCte.

  VIEW FRAME F-Encabezado.
  VIEW FRAME f-ftr.
  
  FOR EACH Creditos WHERE  Creditos.Nit EQ P_CedNit AND Creditos.Num_Credito EQ P_NumCre NO-LOCK:
      CASE Creditos.Per_Pago:
           WHEN 1 THEN                                      
                ASSIGN W_PdoPag = 7.          
           WHEN 2 THEN                                      
                ASSIGN W_PdoPag = 10.         
           WHEN 3 THEN                                     
                ASSIGN W_PdoPag = 15.         
           WHEN 4 THEN                                     
                ASSIGN W_PdoPag = 30.         
       END CASE.                          
                           
                    
      FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ Creditos.Cod_Credito NO-LOCK NO-ERROR.
                    
      DISPLAY "Prod:" + STRING(Creditos.Cod_Credito,"999") + "-" + 
               STRING(Pro_Creditos.Nom_Producto,"X(20)") + "  Nro_Credito:" + 
               STRING(Creditos.Num_Credito) + "  Fec-Desemb:" +
               STRING(Creditos.Fec_Desemb) +  "  Tasa:" + STRING(Creditos.Tasa,">9.99") + 
               "  Plazo:" + STRING(Creditos.Plazo) + "  Monto:" + STRING(Creditos.Val_Desembolso,">>>>>>,>>9.99") 
                                    FORM "X(160)" SKIP
              "--------------------------------------------------------------------------------------------------------------" SKIP
              "#Pdo Fec-Plan    Vr.de Cuota     Vr-CobroJur.    Vr-Int.Mora      Vr-IntCtes.     Vlr.Capital   Sdo-Proyectado" SKIP
              "Pagas F.PròxP   Total Pagado     Pag.CobrJur.    Pag.IntMora      Pag.IntCte      Pag.Capital   Sdo-Obligaciòn" SKIP
              "--------------------------------------------------------------------------------------------------------------" SKIP
          WITH DOWN WIDTH 220 FRAME F_T1 NO-BOX NO-LABELS USE-TEXT STREAM-IO.
      
      W_IntAcu = 0.

      FOR EACH PlanPagos WHERE  PlanPagos.Nit    EQ P_CedNit
                       AND PlanPagos.Num_Credito EQ Creditos.Num_Credito
                       AND PlanPagos.Id_PdoMes   EQ 4 NO-LOCK
                       BREAK BY PlanPagos.Num_Credito BY PlanPagos.Fec_Inic :
                             /*BY PlanPagos.Fec_Vcto
                             BY ROWID(PlanPagos)      BY PlanPagos.Nro_Cuota:
                              */
          IF FIRST-OF(PlanPagos.Num_Credito) THEN
             DISPLAY SKIP (0)
                     "HISTORIAL  NO - VIGENTE" SKIP (0) WITH FRAME fftt NO-LABEL.

          IF PlanPagos.Nro_Cuota LE 1 THEN
             ASSIGN W_IntAcu = 0.

          ASSIGN W_IntAcu = W_IntAcu + PlanPagos.Int_LiqPdo
                 DMora    = PlanPagos.Fec_Vcto - PlanPagos.Fec_ProxPago.
          
          IF LAST-OF(PlanPagos.Num_Credito) THEN
             ASSIGN DMora = W_Fecha - PlanPagos.Fec_ProxPago.

          RUN Display_PP.
      END.


      W_IntAcu = 0.
      FOR EACH PlanPagos WHERE  PlanPagos.Nit    EQ P_CedNit
                       AND PlanPagos.Num_Credito EQ Creditos.Num_Credito
                       AND (PlanPagos.Id_PdoMes  EQ 1 OR PlanPagos.Id_PdoMes EQ 2) NO-LOCK
                       BREAK BY PlanPagos.Num_Credito BY PlanPagos.Id_PdoMes DESCEN BY PlanPagos.Fec_Vcto
                             BY PlanPagos.Nro_Cuota: 
          IF FIRST-OF(PlanPagos.Num_Credito) THEN
             DISPLAY SKIP(1)
                     "HISTORIAL VIGENTE" SKIP (0) WITH FRAME fftt2f NO-LABEL.

          IF PlanPagos.Nro_Cuota LE 1 THEN
             ASSIGN W_IntAcu = 0.

          ASSIGN W_IntAcu = W_IntAcu + PlanPagos.Int_LiqPdo
                 DMora    = PlanPagos.Fec_Vcto - PlanPagos.Fec_ProxPago.
          
          IF LAST-OF(PlanPagos.Num_Credito) THEN
             ASSIGN DMora = W_Fecha - PlanPagos.Fec_ProxPago.

          RUN Display_PP.
      END.
      
      DISPLAY SKIP(1)
              "Tasa del Crédito %" 
              Creditos.Tasa   FORM "-ZZ9.9999"
              "                    "
              "Tasa de Usura  %"  
              W_TasaUS FORM "-ZZ9.9999" 
              "          Saldo de Capital $"
              Creditos.Sdo_Capital FORM "->>>>,>>>,>>9.99" SKIP
              "                         Totales Debido Pagar ------------>"
               Creditos.Int_LiqAcum       FORM "->>>>,>>>,>>9.99"  
               Creditos.Capital_Acum      FORM "->>>>,>>>,>>9.99"            SKIP
              "                         Totales Pagados      ------------>"
               Creditos.Sdo_IntPag        FORM "->>>>,>>>,>>9.99"
               Creditos.Sdo_CapPag        FORM "->>>>,>>>,>>9.99"
              (Creditos.Honorarios     + Creditos.Costas         + Creditos.Polizas +
               Creditos.Int_MorCobrar  + Creditos.Int_MoraDifCob + 
               Creditos.Int_DifCobro   + Creditos.Int_Corrientes +
               Creditos.Sdo_Capital    - Creditos.Int_Anticipado)
                  FORM "->>>>,>>>,>>9.99" SKIP(1)
         WITH DOWN WIDTH 220 FRAME F_Tot1 NO-BOX NO-LABELS USE-TEXT STREAM-IO.
         
      IF Creditos.Sdo_Capital LE 0 THEN DO:
         DISPLAY SKIP(2).
         NEXT.
      END.
  
      FIND LAST PlanPagos WHERE  PlanPagos.Nit    EQ P_CedNit
                        AND PlanPagos.Num_Credito EQ Creditos.Num_Credito
                        AND PlanPagos.Id_PdoMes   EQ 1 NO-LOCK NO-ERROR.

      IF Creditos.Per_Pago EQ 1 THEN
         W_PdoLiq = 52.
      ELSE IF Creditos.Per_Pago EQ 2 THEN
         W_PdoLiq = 36.
      ELSE IF Creditos.Per_Pago EQ 3 THEN
         W_PdoLiq = 24.
      ELSE 
         W_PdoLiq = 12.
             
      ASSIGN SdoPro = Creditos.Sdo_Proyectado     /*- (PlanPagos.Cuota - Creditos.Cuota)*/
             W_Tasa = Creditos.Tasa / (W_PdoLiq * 100).

      IF Creditos.Tasa GT W_TasaUS THEN
         W_Tasa = W_TasaUS / (W_PdoLiq * 100).

      IF Pro_Creditos.Id_PerGracia AND Dia_Gracia GT 0 THEN
         ASSIGN INT_MGracia = ROUND((((Creditos.Monto * W_Tasa) / W_PdoPag) * Dia_Gracia) / Creditos.Plazo,0).


      FOR EACH PlanPagos WHERE  PlanPagos.Nit    EQ P_CedNit
                       AND PlanPagos.Num_Credito EQ Creditos.Num_Credito
                       AND PlanPagos.Id_PdoMes   LE 1 NO-LOCK
                       BREAK BY PlanPagos.Num_Credito BY PlanPagos.Nro_Cuota BY PlanPagos.Fec_Vcto:

          ASSIGN INTLiq = (SdoPro * W_Tasa) + INT_MGracia
                 CapLiq = PlanPagos.Cuota - IntLiq
                 SdoPro = SdoPro - CapLiq.
                       
          DISPLAY PlanPagos.Nro_Cuota             FORM "999"
                  PlanPagos.Fec_Vcto              FORM "99/99/99"
                  PlanPagos.Cuota                 FORM "->>>,>>>,>>9.99"
                  "               "
                  "               " 
                  INTLiq           FORM "->>>,>>>,>>9.99" 
                  CapLiq           FORM "->>>>,>>>,>>9.99"             
                  SdoPro           FORM "->>>>,>>>,>>9.99" SKIP (0)
               WITH DOWN WIDTH 220 FRAME F_Futur NO-BOX NO-LABELS USE-TEXT STREAM-IO. 

          IF SdoPro LE 0 THEN
             LEAVE.
      END.
      
      DISPLAY SKIP(2).
                  
  END.    
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Hist_Creditos  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Hist_Creditos)
  THEN DELETE WIDGET W-Hist_Creditos.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Display_PP W-Hist_Creditos 
PROCEDURE Display_PP :
/*------------------------------------------------------------------------------
  Purpose: Invocado desde Proc.Detallado
  Notes:   Agosto 18/05 GAER    
------------------------------------------------------------------------------*/
  DISPLAY PlanPagos.Nro_Cuota             FORM "999"
          PlanPagos.Fec_Vcto              FORM "99/99/99"                                                         
          PlanPagos.Cuota                 FORM "->>>,>>>,>>9.99"                                                          
          PlanPagos.Cargos_Pdo            FORM "->>>,>>>,>>9.99"                                                          
          PlanPagos.Int_MoraPdo           FORM "->>>,>>>,>>9.99"                                                          
          PlanPagos.Int_LiqPdo            FORM "->>>,>>>,>>9.99"                                                          
          PlanPagos.Capital_Pdo           FORM "->>>>,>>>,>>9.99"                                                         
          PlanPagos.Monto_Actual - PlanPagos.Capital_Acum        FORM "->>>>,>>>,>>9.99"     SKIP                         
          PlanPagos.Cuo_Pagas             FORM "999"                                                                      
          PlanPagos.Fec_ProxPago          FORM "99/99/99"                                                                 
          PlanPagos.Pagos_OtrosPdo + PlanPagos.Pagos_MoraPdo + PlanPagos.Pagos_IntPdo + PlanPagos.Pagos_Capitalpdo        
                                          FORM "->>>,>>>,>>9.99"                                                          
          PlanPagos.Pagos_OtrosPdo        FORM "->>>,>>>,>>9.99"                                                          
          PlanPagos.Pagos_MoraPdo         FORM "->>>,>>>,>>9.99"                                                          
          PlanPagos.Pagos_IntPdo          FORM "->>>,>>>,>>9.99"                                                          
          PlanPagos.Pagos_CapitalPdo      FORM "->>>>,>>>,>>9.99"                                                         
          PlanPagos.Monto_Actual - PlanPagos.Pagos_CapitalAcum    FORM "->>>>,>>>,>>9.99"     SKIP                        
          "D-Mora"                                                                                                        
           DMora                          FORM "->>>9"                                                                    
                                                                                                                          
          (PlanPagos.Cargos_Acum  - PlanPagos.Pagos_OtrosAcum)  +                                                         
          (PlanPagos.Int_MoraAcum - PlanPagos.Pagos_MoraAcum)   +                                                         
          (W_IntAcu               - PlanPagos.Pagos_IntAcum)    +                                                         
          (PlanPagos.Capital_Acum - PlanPagos.Pagos_CapitalAcum)  FORM "->>>,>>>,>>9.99"                                  
                                                                                                                          
          PlanPagos.Cargos_Acum  - PlanPagos.Pagos_OtrosAcum      FORM "->>>,>>>,>>9.99"                                  
          PlanPagos.Int_MoraAcum - PlanPagos.Pagos_MoraAcum       FORM "->>>,>>>,>>9.99"                                  
          W_IntAcu               - PlanPagos.Pagos_IntAcum        FORM "->>>,>>>,>>9.99"                                  
          PlanPagos.Capital_Acum - PlanPagos.Pagos_CapitalAcum    FORM "->>>>,>>>,>>9.99"                                 
                                                                                                                          
          (PlanPagos.Cargos_Acum  - PlanPagos.Pagos_OtrosAcum)  +                                                         
          (PlanPagos.Int_MoraAcum - PlanPagos.Pagos_MoraAcum)   +                                                         
          (PlanPagos.Int_LiqAcum  - PlanPagos.Pagos_IntAcum)    +                                                         
          (PlanPagos.Monto_Actual - PlanPagos.Pagos_CapitalAcum)  FORM "->>>>,>>>,>>9.99"                                 
           SKIP(1)                                                                                                        
       WITH DOWN WIDTH 220 FRAME F_D1 NO-BOX NO-LABELS USE-TEXT STREAM-IO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Hist_Creditos  _DEFAULT-ENABLE
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
  DISPLAY W_NitCte W_NomCte Rs_Cons FechaProyecta 
      WITH FRAME F_Proc IN WINDOW W-Hist_Creditos.
  ENABLE RECT-319 Br_Cred Btn_ImpProy Rs_Cons btnProyectarValor FechaProyecta 
         BUTTON-5 Btn_Done 
      WITH FRAME F_Proc IN WINDOW W-Hist_Creditos.
  {&OPEN-BROWSERS-IN-QUERY-F_Proc}
  DISPLAY W_NroTot W_VrTot W_CuoUnica W_NroVig W_VrVig W_FijaCaja W_FecPri 
          W_FijaNomi W_FecUlt W_NroCobJur W_FecProxP W_NroCastig W_SdoAct 
          dePrbbldadIncmplimiento W_CapVdo dePrddaEsprda W_IntVdo W_IMora 
          W_DiasMora W_Otros 
      WITH FRAME F_Resumen IN WINDOW W-Hist_Creditos.
  ENABLE RECT-320 RECT-321 RECT-322 ReimprimirPagare btnMvmientos 
      WITH FRAME F_Resumen IN WINDOW W-Hist_Creditos.
  {&OPEN-BROWSERS-IN-QUERY-F_Resumen}
  VIEW W-Hist_Creditos.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pMovCrdto W-Hist_Creditos 
PROCEDURE pMovCrdto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR cl AS CHAR NO-UNDO.
/*
   DEF TEMP-TABLE tMvmientoCrdto NO-UNDO
       FIELD tFcha AS DATE
       FIELD tOprcion AS CHAR
       FIELD tNmroTrnsccion AS CHAR
       FIELD tCrgo AS DECIMAL
       FIELD tAbno AS DECIMAL
       FIELD tTtalTrnsccion AS DECIMAL
       FIELD tSldoCptal AS DECIMAL
       FIELD tAgncia AS INTEGER
       FIELD tHra AS INTEGER
*/            
    cl = "======================================MOVIMIENTO CREDITO================================================".
    PUT UNFORMATTED cl SKIP.
    cl = "Fecha      Operación                      N.Trans        Cargos       Abonos             Sdo-Capital Of.".
    PUT UNFORMATTED cl SKIP.
    cl = "========================================================================================================".
    PUT UNFORMATTED cl SKIP.
    FOR EACH tMvmientoCrdto
        BREAK
            BY tFcha
            BY tNmroTrnsccion:
        ACCUMULATE tCrgo (TOTAL BY tNmroTrnsccion).
        ACCUMULATE tAbno (TOTAL BY tNmroTrnsccion).
        cl = "".
        OVERLAY(cl,1,10)    = STRING(tFcha,"99/99/9999").
        OVERLAY(cl,12,30)   = tOprcion.    
        OVERLAY(cl,43,8)    = STRING(tNmroTrnsccion,"99999999").
        OVERLAY(cl,52,12)   = STRING(tCrgo,"->>>,>>>,>>9").
        OVERLAY(cl,65,12)   = STRING(tAbno,"->>>,>>>,>>9").
        OVERLAY(cl,77,12)   = IF last-of(tNmroTrnsccion) THEN STRING((ACCUM TOTAL BY tNmroTrnsccion tAbno) - (ACCUM TOTAL BY tNmroTrnsccion tCrgo),"->>>,>>>,>>9") ELSE "".
        OVERLAY(cl,89,12)   = STRING(tSldoCptal,"->>>,>>>,>>9").
        OVERLAY(cl,102,3)   = STRING(tAgncia,"999").
        PUT UNFORMATTED cl SKIP.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir W-Hist_Creditos 
PROCEDURE ProcesoImprimir :
DEFI VAR W_NomPer AS CHAR FORM "X(15)" INITIAL "Mensual".
DEFI VAR W_PdoAno AS INTEG FORM "99" INITIAL 12.
DEFI VAR W_NroDias AS INT.
DEFI VAR P_NMeses AS INT.
DEFI VAR FecIni AS DATE.
DEFI VAR w_fec AS DATE.
DEFI VAR w_fec1 AS DATE.
DEFINE VAR numExtras AS INTEGER.
DEFINE VAR vMonto AS DECIMAL.

IF lMovCrdto THEN DO:
    RUN pMovCrdto.
    RETURN.
END.

IF W_SiProy THEN DO:
    FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ Creditos.Cod_Credito NO-LOCK NO-ERROR.

    RUN HallarPeriodo IN W_ManFin (INPUT Creditos.Per_Pago,
                                   INPUT Creditos.Plazo,
                                   OUTPUT W_NroDias,
                                   OUTPUT P_NMeses,
                                   OUTPUT W_PdoAno,
                                   OUTPUT W_NomPer).

    FecIni = Creditos.Fec_Desembolso.

    IF Creditos.Fec_PagAnti NE ? AND Creditos.Fec_PagAnti GT Creditos.Fec_Desembolso THEN
        FecIni = Creditos.Fec_PagAnti.

    /* Se halla el número de cuotas extras para enviarlo a la rutina */
    FOR EACH extras WHERE extras.agencia = creditos.agencia
                      AND extras.nit = creditos.nit
                      AND extras.cod_credito = creditos.cod_credito
                      AND extras.num_solicitud = creditos.num_solicitud NO-LOCK:
        numExtras = numExtras + 1.
    END.


    w_fec = Creditos.Fec_Desembolso.

    FIND FIRST mov_creditos WHERE mov_creditos.nit = creditos.nit
                              AND mov_creditos.num_credito = creditos.num_credito
                              AND mov_creditos.fecha >= creditos.fec_desembolso
                              AND mov_creditos.cod_operacion = 999999999 NO-LOCK NO-ERROR.
    IF AVAILABLE mov_creditos THEN
        w_fec = creditos.fec_pagAnti - W_NroDias.
                              
    /*Se corrige la fecha para Fodun*/
    IF Creditos.Fec_PagAnti <> ? THEN
        w_Fec1 = Creditos.Fec_PagAnti.
    ELSE
        w_Fec1 = Creditos.Fec_desembolso.

/*
    IF Creditos.For_Pago = 2 THEN
        w_Fec1 = DATE(MONTH(w_Fec1), 1, YEAR(w_Fec1)) - 1.
*/
    
    IF creditos.cod_credito = 123 THEN DO:
        FOR EACH mov_creditos WHERE mov_creditos.nit = creditos.nit
                                AND mov_creditos.num_credito = creditos.num_credito
                                AND mov_creditos.fecha <= ADD-INTERVAL(creditos.fec_pagAnti,-1,"months") + 11 NO-LOCK BY mov_creditos.fecha DESCENDING:
            vMonto = mov_creditos.Sdo_Capital.
            LEAVE.
        END.
    END.
    ELSE
        vMonto = creditos.monto.

    RUN Proyeccion_Credito.R (INPUT vMonto,
                              INPUT Creditos.Plazo,
                              INPUT Creditos.Cuota,
                              INPUT creditos.fec_desembolso,
                              INPUT creditos.fec_pagAnti,
                              INPUT Creditos.Tasa / (W_PdoAno * 100),
                              INPUT Creditos.Per_Pago,
                              INPUT creditos.sistema,
                              INPUT Creditos.Nit,
                              INPUT W_NomCte:SCREEN-VALUE IN FRAME F_Proc,
                              INPUT Creditos.Cod_Credito,
                              INPUT Pro_Creditos.Nom_Producto,
                              INPUT "S",
                              INPUT Creditos.Num_Solicitud,
                              INPUT "Cuota Fija",
                              INPUT creditos.monto).
    W_SiProy = FALSE.
    
    RETURN.
END.

RUN Detallado.
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Resumen W-Hist_Creditos 
PROCEDURE Resumen :
/*------------------------------------------------------------------------------
  Purpose:     Campos del Frame con el Resumen de Créditos.
------------------------------------------------------------------------------*/
    DEFI VAR MoraMax LIKE Creditos.Int_MorCobrar EXTENT 4 INIT 0.

  ASSIGN W_FecPri   = ?
         W_FecUlt   = ?
         W_FecProxP = ?
         W_FUltMora = ?
         W_FecPMora = ?.
         
  FOR EACH Creditos WHERE  Creditos.Nit    EQ P_CedNit
                      AND (Creditos.Estado EQ 2 OR
                           Creditos.Estado EQ 3 OR
                           Creditos.Estado EQ 5) NO-LOCK
                    BREAK BY Creditos.Fec_Desembolso:
      IF FIRST-OF(Creditos.Fec_Desembolso) AND W_FecPri = ? THEN
         ASSIGN W_FecPri = Creditos.Fec_Desembolso.
                              
      ASSIGN W_NroTot = W_NroTot + 1
             W_VrTot  = W_VrTot  + Creditos.Val_Desembolso.
             
      IF Creditos.Estado EQ 5 THEN
         W_NroCastig = W_NroCastig + 1. 

      IF Creditos.Sdo_Capital GT 0 THEN DO:
         IF W_FecProxP = ? THEN
            W_FecProxP = Creditos.Fec_Pago.
            
         ASSIGN W_NroVig = W_NroVig + 1
                W_VrVig  = W_VrVig  + Creditos.Val_Desembolso
                W_SdoAct = W_SdoAct + (Creditos.Honorarios     + Creditos.Costas       + Creditos.Polizas +
                                       Creditos.Int_MorCobrar  + Creditos.Int_DifCobro + Creditos.Int_Corrientes +
                                       Creditos.Sdo_Capital    + Creditos.Int_MoraDifCob -
                                       Creditos.Int_Anticipado)
                W_IMora  = W_IMora  + (Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob)
                W_Otros  = W_Otros  + (Creditos.Honorarios + Creditos.Costas + Creditos.Polizas).

         IF W_FecProxP GT Creditos.Fec_Pago THEN
            W_FecProxP = Creditos.Fec_Pago.

         IF Creditos.Val_atraso GT 0 THEN
            ASSIGN W_CapVdo = W_CapVdo + val_atraso.
         /*IF Creditos.Capital_Acum GT Creditos.Sdo_CapPag THEN
            ASSIGN W_CapVdo = W_CapVdo + (Creditos.Capital_Acum - Creditos.Sdo_CapPag)
                   W_SdoVdo = W_SdoVdo + (Creditos.Capital_Acum - Creditos.Sdo_CapPag).*/
          
          ASSIGN W_IntVdo =  W_IntVdo +  INT_corriente + INT_difcobro.
         /*IF Creditos.Int_LiqAcum GT Creditos.Sdo_IntPag THEN
            ASSIGN W_IntVdo = W_IntVdo + (Creditos.Int_LiqAcum - Creditos.Sdo_IntPag)
                   W_SdoVdo = W_SdoVdo + (Creditos.Int_LiqAcum - Creditos.Sdo_IntPag).*/
                
        /* IF W_Fecha GT Creditos.Fec_Pago THEN
            ASSIGN W_DiasMora = (W_Fecha - Creditos.Fec_Pago) WHEN W_DiasMora LT (W_Fecha - Creditos.Fec_Pago).*/
          ASSIGN W_DiasMora =  W_DiasMora + dias_atraso.

         IF Creditos.Sistema EQ 2 THEN
            W_CuoUnica = W_CuoUnica + Creditos.Cuota.
         ELSE DO:
            IF Creditos.For_Pago EQ 2 THEN
               W_FijaNomi = W_FijaNomi + Creditos.Cuota.
            ELSE
               W_FijaCaja = W_FijaCaja + Creditos.Cuota.                           
         END.          
      END.

      IF LAST-OF(Creditos.Fec_Desemb) THEN
         ASSIGN W_FecUlt = Creditos.Fec_Desemb.

  END.

  FOR EACH PlanPagos WHERE  PlanPagos.Nit       EQ P_CedNit
                       AND (PlanPagos.Id_PdoMes EQ 1 OR PlanPagos.Id_PdoMes EQ 2) NO-LOCK
                       BREAK BY PlanPagos.Num_Credito BY PlanPagos.Nro_Cuota:
      IF PlanPagos.Capital_Acum    GT PlanPagos.Pagos_CapitalAcum 
      OR PlanPagos.Int_LiqAcum     GT PlanPagos.Pagos_IntAcum  
      OR PlanPagos.Int_MoraAcum    GT PlanPagos.Pagos_MoraAcum
      OR PlanPagos.Cargos_Acum     GT PlanPagos.Pagos_OtrosAcum THEN DO:
         IF W_FecPMora = ? THEN
            W_FecPMora = PlanPagos.Fec_Vcto.
      
         IF W_FUltMora = ? THEN
            W_FUltMora = PlanPagos.Fec_Vcto.
      
         ASSIGN W_NroMora = W_NroMora + 1.
         
         IF PlanPagos.Nro_Cuota GT PlanPagos.Cuo_Pagas THEN
            ASSIGN W_CuoMaxMora = (PlanPagos.Nro_Cuota - PlanPagos.Cuo_Pagas) WHEN 
                                   W_CuoMaxMora LT (PlanPagos.Nro_Cuota - PlanPagos.Cuo_Pagas).
         
         IF PlanPagos.Fec_Vcto GT PlanPagos.Fec_ProxPago THEN DO:
            ASSIGN W_DMaxMora = (PlanPagos.Fec_Vcto - PlanPagos.Fec_ProxPago) WHEN
                                 W_DMaxMora LT (PlanPagos.Fec_Vcto - PlanPagos.Fec_ProxPago).
      
            ASSIGN W_FecPMora = PlanPagos.Fec_ProxPago WHEN
                                W_FecPMora GT PlanPagos.Fec_ProxPago. 
                                
            ASSIGN W_FUltMora = PlanPagos.Fec_ProxPago WHEN
                                W_FUltMora LT PlanPagos.Fec_ProxPago.
         END.
         
         IF PlanPagos.Capital_Acum GT PlanPagos.Pagos_CapitalAcum THEN
            ASSIGN MoraMax[1] = (PlanPagos.Capital_Acum - PlanPagos.Pagos_CapitalAcum) WHEN
                                 MoraMax[1] LT (PlanPagos.Capital_Acum - PlanPagos.Pagos_CapitalAcum).
         
         IF PlanPagos.Int_LiqAcum GT PlanPagos.Pagos_IntAcum THEN
            ASSIGN MoraMax[2] = (PlanPagos.Int_LiqAcum - PlanPagos.Pagos_IntAcum) WHEN
                                 MoraMax[2] LT (PlanPagos.Int_LiqAcum - PlanPagos.Pagos_IntAcum).
                                 
         IF PlanPagos.Int_MoraAcum GT PlanPagos.Pagos_MoraAcum THEN
            ASSIGN MoraMax[3] = (PlanPagos.Int_MoraAcum - PlanPagos.Pagos_MoraAcum) WHEN
                                 MoraMax[3] LT (PlanPagos.Int_MoraAcum - PlanPagos.Pagos_MoraAcum).
                                 
         IF PlanPagos.Cargos_Acum GT PlanPagos.Pagos_OtrosAcum THEN
            ASSIGN MoraMax[4] = (PlanPagos.Cargos_Acum - PlanPagos.Pagos_OtrosAcum) WHEN
                                 MoraMax[4] LT (PlanPagos.Cargos_Acum - PlanPagos.Pagos_OtrosAcum).
      END.
      
      IF LAST-OF(PlanPagos.Num_Credito) THEN DO:
         ASSIGN W_MoraMax = (MoraMax[1] + MoraMax[2] + MoraMax[3] + MoraMax[4]) WHEN
                             W_MoraMax LT (MoraMax[1] + MoraMax[2] + MoraMax[3] + MoraMax[4]).
         
         ASSIGN W_TotMora = W_TotMora + (MoraMax[1] + MoraMax[2] + MoraMax[3] + MoraMax[4])
                MoraMax   = 0.
                
         IF PlanPagos.Cargos_Acum NE 0 OR PlanPagos.Pagos_OtrosAcum NE 0 THEN
            W_NroCobJur = W_NroCobJur + 1.
      END.
  END.
  
  ASSIGN W_FecPri:SCREEN-VALUE IN FRAME F_Resumen = STRING(W_FecPri)
         W_FecUlt:SCREEN-VALUE                    = STRING(W_FecUlt).

  DISPLAY W_NroTot   W_VrTot      W_FecProxP  W_CapVdo
          W_FijaCaja W_CuoUnica   W_FijaNomi W_IntVdo  W_NroCastig W_NroCobJur
          W_NroVig   W_VrVig      W_SdoAct   W_IMora   W_Otros    
          W_DiasMora
     WITH FRAME F_Resumen.
                  
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fPMvtos W-Hist_Creditos 
FUNCTION fPMvtos RETURNS LOGICAL
  (cnit AS CHAR,iNmroCrdto AS INTEGER /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    DEF VAR deVlor AS DECIMAL NO-UNDO.
    EMPTY TEMP-TABLE tMvmientoCrdto.
    FOR EACH Mov_Creditos NO-LOCK
        WHERE 
            Mov_Creditos.Num_Credito EQ iNmroCrdto 
        AND Mov_Creditos.Nit         EQ CNit,
        FIRST operacion NO-LOCK
            WHERE
                operacion.cod_operacion = Mov_Creditos.Cod_Operacion
        BREAK 
            BY Mov_Creditos.Fecha DESC
            BY Mov_Creditos.Hora  DESC
            BY Mov_Creditos.Cpte 
            BY Mov_Creditos.Num_Documento:
        devlor = Mov_Creditos.Val_Efectivo + Mov_Creditos.Val_Cheque.

        CREATE tMvmientoCrdto.
        ASSIGN tMvmientoCrdto.tFcha             = Mov_Creditos.Fecha
               tMvmientoCrdto.tOprcion          = Operacion.Nom_Operacion
               tMvmientoCrdto.tNmroTrnsccion    = Mov_Creditos.Num_Documento
               tMvmientoCrdto.tCrgo             = IF operacion.tipo_operacion = 2 THEN devlor ELSE 0
               tMvmientoCrdto.tAbno             = IF NOT operacion.tipo_operacion = 2 THEN devlor ELSE 0
               tMvmientoCrdto.tTtalTrnsccion    = tMvmientoCrdto.tAbno - tMvmientoCrdto.tCrgo
               tMvmientoCrdto.tSldoCptal        = Mov_Creditos.Sdo_Capital
               tMvmientoCrdto.tAgncia           = mov_creditos.agencia
               tMvmientoCrdto.tHra              = mov_creditos.hora.

    END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

