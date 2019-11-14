&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  Programa       : W-grapptomanual.w
  DESCRIPCION   : Captura de presupuesto de cuentas.

------------------------------------------------------------------------*/
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
{Incluido\VARIABLE.I "SHARED"}
/* Local Variable Definitions ---                                       */

 DEFINE VAR W_Adicion   AS   LOGICAL INITIAL FALSE.
 DEFINE VAR Cont        AS   INTEGER.
 DEFINE VAR w_Ind       AS   INTEGER   INITIAL 0.                                      
 DEFINE VAR W_WidSel    AS   HANDLE.
 DEFINE VAR W_COfi      AS   LOGICAL.
 DEFINE VAR W_ManejaOf  AS   LOGICAL.
 DEFINE VAR W_Ccos      AS   LOGICAL.
 DEFINE VAR W_BorCmb    AS   LOGICAL.
 DEFINE VAR w_Aux       AS   DECIMAL   INITIAL 0.
 DEFINE VAR Vector      AS   DECIMAL   FORMAT "->>>,>>>,>>>,>>9" EXTENT 12.
 DEFINE VAR W_TotAux    AS   DECIMAL   FORMAT "->>>,>>>,>>>,>>9".
 DEFINE VAR W_OfStr     AS   CHARACTER FORMAT "X(35)".
 DEFINE VAR w_Campo     AS   WIDGET-HANDLE. 

 DEFINE VAR W_ValMes    LIKE Sal_Presupuesto.Sdo_Final.
 DEFINE VAR W_OfiAux    LIKE agencias.agencia.
 DEFINE VAR W_CtaAux    LIKE Sal_Presupuesto.Cod_Presupuesto.
 DEFINE VAR W_CcoAux    LIKE Cen_Costos.Cen_Costos.
 DEFINE VAR W_AnoAux    LIKE Sal_Presupuesto.Ano.

 DEFINE TEMP-TABLE Tmp
   FIELD W_Mes          LIKE Sal_Presupuesto.Mes       LABEL "Mes"
   FIELD W_Valor        AS   DECIMAL FORMAT "->>>,>>>,>>>,>>9"
                             LABEL "    Valor   " INITIAL 0.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Imprimir
&Scoped-define BROWSE-NAME B_Consulta

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Sal_Presupuesto Tmp

/* Definitions for BROWSE B_Consulta                                    */
&Scoped-define FIELDS-IN-QUERY-B_Consulta Sal_Presupuesto.agencia Sal_Presupuesto.Cen_Costos Sal_Presupuesto.Cod_Presupuesto Sal_Presupuesto.Ano   
&Scoped-define ENABLED-FIELDS-IN-QUERY-B_Consulta   
&Scoped-define SELF-NAME B_Consulta
&Scoped-define QUERY-STRING-B_Consulta FOR EACH Sal_Presupuesto WHERE Sal_Presupuesto.Mes = 01 NO-LOCK
&Scoped-define OPEN-QUERY-B_Consulta OPEN QUERY B_Consulta FOR EACH Sal_Presupuesto WHERE Sal_Presupuesto.Mes = 01 NO-LOCK.
&Scoped-define TABLES-IN-QUERY-B_Consulta Sal_Presupuesto
&Scoped-define FIRST-TABLE-IN-QUERY-B_Consulta Sal_Presupuesto


/* Definitions for BROWSE Temp                                          */
&Scoped-define FIELDS-IN-QUERY-Temp Tmp.W_Mes Tmp.W_Valor   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Temp ALL EXCEPT Tmp.W_Mes   
&Scoped-define ENABLED-TABLES-IN-QUERY-Temp Tmp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Temp Tmp
&Scoped-define SELF-NAME Temp
&Scoped-define QUERY-STRING-Temp FOR EACH Tmp
&Scoped-define OPEN-QUERY-Temp OPEN QUERY Temp FOR EACH Tmp.
&Scoped-define TABLES-IN-QUERY-Temp Tmp
&Scoped-define FIRST-TABLE-IN-QUERY-Temp Tmp


/* Definitions for FRAME F_Consulta                                     */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Consulta ~
    ~{&OPEN-QUERY-B_Consulta}

/* Definitions for FRAME F_Ppto                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Ppto ~
    ~{&OPEN-QUERY-Temp}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS R-Detalle FILL-Ano BUTTON-69 BUTTON-70 ~
RECT-129 RECT-130 RECT-131 
&Scoped-Define DISPLAYED-OBJECTS R-Detalle FILL-Ano FILL-agencia ~
FILL-Cencos 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 Temp W_OfiPre W_Cuenta W_Cen_Costos W_Ano 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-69 
     LABEL "Generar" 
     SIZE 15 BY .85.

DEFINE BUTTON BUTTON-70 
     LABEL "Cancelar" 
     SIZE 15 BY .85.

DEFINE VARIABLE FILL-agencia AS INTEGER FORMAT "999":U INITIAL 0 
     LABEL "agencia" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .73 NO-UNDO.

DEFINE VARIABLE FILL-Ano AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Año" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .73 NO-UNDO.

DEFINE VARIABLE FILL-Cencos AS INTEGER FORMAT "999":U INITIAL 0 
     LABEL "Cen. Costos" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .73 NO-UNDO.

DEFINE VARIABLE R-Detalle AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "General", 1,
"Detallado", 2
     SIZE 31 BY 1.08 NO-UNDO.

DEFINE RECTANGLE RECT-129
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 50.72 BY 1.62.

DEFINE RECTANGLE RECT-130
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 50.72 BY 2.15.

DEFINE RECTANGLE RECT-131
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 50.72 BY 1.88.

DEFINE BUTTON Btn_CanCon 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "&Terminar Consulta" 
     SIZE 14 BY 1.35.

DEFINE BUTTON Btn_Todas 
     LABEL "T&odas" 
     SIZE 14 BY 1.35.

DEFINE VARIABLE W_FillConsulta AS CHARACTER FORMAT "X(25)":U 
     LABEL "Valor" 
     VIEW-AS FILL-IN 
     SIZE 22.86 BY .81 NO-UNDO.

DEFINE VARIABLE R_Consulta AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "agencia", 1,
"Centro Costos", 2,
"Cuenta", 3,
"Año", 4
     SIZE 50 BY .54
     BGCOLOR 17 FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-31
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 49 BY 1.88.

DEFINE BUTTON Btn_Ayuda 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "Ayuda" 
     SIZE 4 BY 1.08 TOOLTIP "Ayuda".

DEFINE BUTTON Btn_Cancelar 
     LABEL "&Cancelar" 
     SIZE 11 BY 1.88.

DEFINE BUTTON Btn_Consultar 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "C&onsultar" 
     SIZE 11 BY 1.62.

DEFINE BUTTON Btn_Imprimir 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/impresora2.bmp":U
     LABEL "&Imprimir" 
     SIZE 11 BY 1.62.

DEFINE BUTTON Btn_Ingresar 
     LABEL "I&ngresar" 
     SIZE 11 BY 1.88.

DEFINE BUTTON Btn_Modificar 
     LABEL "&Modificar" 
     SIZE 11 BY 1.88.

DEFINE BUTTON Btn_Salir DEFAULT 
     LABEL "&Salir" 
     SIZE 11 BY 1.88
     BGCOLOR 8 .

DEFINE BUTTON Btn_Salvar 
     LABEL "&Salvar" 
     SIZE 11 BY 1.62.

DEFINE BUTTON BUTTON-71 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/informacion.bmp":U
     LABEL "Button 71" 
     SIZE 9.29 BY 1.62.

DEFINE VARIABLE W_Ano AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Año" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Cen_Costos AS INTEGER FORMAT "999":U INITIAL 0 
     LABEL "Centro de Costos" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Cuenta AS CHARACTER FORMAT "X(14)":U 
     LABEL "Cuenta" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomCCo AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 35 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomCta AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 35 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomOfi AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 35 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_OfiPre AS INTEGER FORMAT "999":U INITIAL 0 
     LABEL "agencia" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .81 NO-UNDO.

DEFINE VARIABLE W_Total AS DECIMAL FORMAT "->>>,>>>,>>>,>>9":U INITIAL 0 
     LABEL "Total" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-128
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 64 BY 1.35.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 64 BY 6.46.

DEFINE RECTANGLE RECT-29
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 13 BY 10.77.

DEFINE VARIABLE r_tipo AS LOGICAL INITIAL no 
     LABEL "Asigna Valores del Presupuesto a cada mes Automáticamente?" 
     VIEW-AS TOGGLE-BOX
     SIZE 46 BY .77 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY B_Consulta FOR 
      Sal_Presupuesto SCROLLING.

DEFINE QUERY Temp FOR 
      Tmp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE B_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS B_Consulta C-Win _FREEFORM
  QUERY B_Consulta NO-LOCK DISPLAY
      Sal_Presupuesto.agencia
      Sal_Presupuesto.Cen_Costos      
      Sal_Presupuesto.Cod_Presupuesto  
      Sal_Presupuesto.Ano  width 6
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 51 BY 4.85
         FONT 4.

DEFINE BROWSE Temp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Temp C-Win _FREEFORM
  QUERY Temp NO-LOCK DISPLAY
      Tmp.W_Mes
      Tmp.W_Valor 
      ENABLE ALL EXCEPT Tmp.W_Mes
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN NO-ROW-MARKERS SEPARATORS SIZE 19.57 BY 8.62
         FONT 4.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Ppto
     BUTTON-71 AT ROW 1.81 COL 102
     Btn_Imprimir AT ROW 5.04 COL 101 HELP
          "Permite Generar la Impresión de Cuentas de Presupuesto"
     Temp AT ROW 5.31 COL 70
     W_OfiPre AT ROW 5.85 COL 24 COLON-ALIGNED
     W_NomOfi AT ROW 5.85 COL 30 COLON-ALIGNED NO-LABEL
     Btn_Consultar AT ROW 6.65 COL 101 HELP
          "Permite Generar la consulta de Cuentas de Presupuesto"
     W_Cuenta AT ROW 7.46 COL 13 COLON-ALIGNED
     W_NomCta AT ROW 7.46 COL 30 COLON-ALIGNED NO-LABEL
     W_Cen_Costos AT ROW 9.08 COL 25 COLON-ALIGNED
     W_NomCCo AT ROW 9.08 COL 30 COLON-ALIGNED NO-LABEL
     Btn_Salvar AT ROW 9.35 COL 101 HELP
          "Permite Salvar la Información de Cuentas Presupuestales"
     W_Ano AT ROW 10.42 COL 24 COLON-ALIGNED
     Btn_Ingresar AT ROW 10.96 COL 101 HELP
          "Permite el Ingreso de Información de Cuentas Presupuestales"
     r_tipo AT ROW 12.31 COL 10
     Btn_Modificar AT ROW 12.85 COL 101 HELP
          "Permite la Modificación de Cuentas Presupuestales"
     W_Total AT ROW 13.92 COL 71 COLON-ALIGNED HELP
          "Ingrese el total a Presupuestar"
     Btn_Cancelar AT ROW 14.73 COL 101 HELP
          "Permite Cancelar la Información de Cuentas Presupuestales"
     Btn_Salir AT ROW 16.62 COL 101 HELP
          "Permite Salir de Cuentas Presupuestales"
     Btn_Ayuda AT ROW 20.12 COL 105 HELP
          "Permite Obtener la Ayuda de esta Pantalla"
     RECT-128 AT ROW 12.04 COL 4
     RECT-28 AT ROW 5.31 COL 4
     RECT-29 AT ROW 9.08 COL 100
     "Datos Cuentas Presupuestables" VIEW-AS TEXT
          SIZE 86 BY .54 AT ROW 4.23 COL 4
          BGCOLOR 18 FGCOLOR 15 FONT 6
     "." VIEW-AS TEXT
          SIZE 86 BY .54 AT ROW 17.15 COL 5
          BGCOLOR 18 FGCOLOR 7 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.43 ROW 1.08
         SIZE 113.86 BY 21.23
         BGCOLOR 17 FONT 4.

DEFINE FRAME F-Imprimir
     R-Detalle AT ROW 1.54 COL 15 NO-LABEL
     FILL-Ano AT ROW 3.42 COL 8 COLON-ALIGNED
     FILL-agencia AT ROW 3.42 COL 25 COLON-ALIGNED
     FILL-Cencos AT ROW 3.42 COL 45 COLON-ALIGNED
     BUTTON-69 AT ROW 5.58 COL 11
     BUTTON-70 AT ROW 5.58 COL 35
     RECT-129 AT ROW 1.27 COL 3
     RECT-130 AT ROW 2.88 COL 3
     RECT-131 AT ROW 5.04 COL 3
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 14.57 ROW 12.12
         SIZE 54.86 BY 6.96
         BGCOLOR 17 FONT 4
         TITLE "Imprimir Informe".

DEFINE FRAME F_Consulta
     B_Consulta AT ROW 1.54 COL 3
     R_Consulta AT ROW 7.19 COL 3 HELP
          "Seleccione la Opcion a consultar" NO-LABEL
     W_FillConsulta AT ROW 8 COL 17 COLON-ALIGNED
     Btn_Todas AT ROW 10.15 COL 9
     Btn_CanCon AT ROW 10.15 COL 33 HELP
          "Permite Regresar a la Captura de Información"
     RECT-31 AT ROW 9.88 COL 4
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 17.29 ROW 3.77
         SIZE 55.72 BY 12.23
         BGCOLOR 17 FONT 4
         TITLE "Consulta".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Captura detallada de presupuesto"
         HEIGHT             = 21.31
         WIDTH              = 113.43
         MAX-HEIGHT         = 21.38
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 21.38
         VIRTUAL-WIDTH      = 114.29
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
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Imprimir
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F-Imprimir:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-agencia IN FRAME F-Imprimir
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-Cencos IN FRAME F-Imprimir
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Consulta
   NOT-VISIBLE                                                          */
/* BROWSE-TAB B_Consulta 1 F_Consulta */
ASSIGN 
       FRAME F_Consulta:HIDDEN           = TRUE
       FRAME F_Consulta:MOVABLE          = TRUE.

/* SETTINGS FOR FRAME F_Ppto
                                                                        */
/* BROWSE-TAB Temp Btn_Imprimir F_Ppto */
/* SETTINGS FOR BUTTON Btn_Cancelar IN FRAME F_Ppto
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX r_tipo IN FRAME F_Ppto
   NO-ENABLE                                                            */
/* SETTINGS FOR BROWSE Temp IN FRAME F_Ppto
   NO-ENABLE 1                                                          */
ASSIGN 
       Temp:HIDDEN  IN FRAME F_Ppto                = TRUE.

/* SETTINGS FOR FILL-IN W_Ano IN FRAME F_Ppto
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN W_Cen_Costos IN FRAME F_Ppto
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN W_Cuenta IN FRAME F_Ppto
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN W_NomCCo IN FRAME F_Ppto
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomCta IN FRAME F_Ppto
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomOfi IN FRAME F_Ppto
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_OfiPre IN FRAME F_Ppto
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN W_Total IN FRAME F_Ppto
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE B_Consulta
/* Query rebuild information for BROWSE B_Consulta
     _START_FREEFORM
OPEN QUERY B_Consulta FOR EACH Sal_Presupuesto WHERE Sal_Presupuesto.Mes = 01 NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK"
     _Query            is OPENED
*/  /* BROWSE B_Consulta */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Temp
/* Query rebuild information for BROWSE Temp
     _START_FREEFORM
OPEN QUERY Temp FOR EACH Tmp.
     _END_FREEFORM
     _Options          = "NO-LOCK"
     _Query            is OPENED
*/  /* BROWSE Temp */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Captura detallada de presupuesto */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Captura detallada de presupuesto */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ppto
&Scoped-define SELF-NAME Btn_Ayuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ayuda C-Win
ON CHOOSE OF Btn_Ayuda IN FRAME F_Ppto /* Ayuda */
OR HELP OF {&WINDOW-NAME}
DO:
  SYSTEM-HELP "AYUDAS\CONTABIL" CONTEXT 7.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancelar C-Win
ON CHOOSE OF Btn_Cancelar IN FRAME F_Ppto /* Cancelar */
DO:
  W_Adicion = FALSE.
  RUN Cancelar.
  DO WITH FRAME F_Ppto:
    ENABLE Btn_Modificar Btn_Ingresar Btn_Consultar.
    DISABLE {&list-1} R_Tipo Btn_Cancelar.
    W_OfiPre:SENSITIVE IN FRAME F_Ppto = FALSE.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME Btn_CanCon
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_CanCon C-Win
ON CHOOSE OF Btn_CanCon IN FRAME F_Consulta /* Terminar Consulta */
DO:
  HIDE FRAME F_Consulta.
  FRAME F_Ppto:SENSITIVE = TRUE.
  APPLY "ENTRY" TO Tmp.W_Valor IN BROWSE Temp.
  DISABLE Temp WITH FRAME F_Ppto.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ppto
&Scoped-define SELF-NAME Btn_Consultar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Consultar C-Win
ON CHOOSE OF Btn_Consultar IN FRAME F_Ppto /* Consultar */
DO:
  DEFINE VAR W_Metodo  AS LOGICAL.
  OPEN QUERY B_Consulta FOR EACH Sal_Presupuesto WHERE Sal_Presupuesto.Mes = 01 NO-LOCK.
  FRAME F_ppto:SENSITIVE = FALSE.
  VIEW FRAME F_Consulta.
  APPLY "ENTRY":U TO B_Consulta IN FRAME F_Consulta.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imprimir C-Win
ON CHOOSE OF Btn_Imprimir IN FRAME F_Ppto /* Imprimir */
DO:
  FRAME F_ppto:SENSITIVE = FALSE.
  VIEW FRAME F-Imprimir.
  ASSIGN    FILL-Ano:SCREEN-VALUE IN FRAME F-Imprimir     = "0"
            FILL-Cencos:SCREEN-VALUE IN FRAME F-Imprimir  = "0" 
            FILL-agencia:SCREEN-VALUE IN FRAME F-Imprimir = "0"
            FILL-Cencos:SENSITIVE IN FRAME F-Imprimir     = FALSE
            FILL-agencia:SENSITIVE IN FRAME F-Imprimir    = FALSE
            R-Detalle:SCREEN-VALUE IN FRAME F-Imprimir    = "1"
            R-Detalle.
  APPLY "ENTRY" TO FILL-Ano IN FRAME F-Imprimir.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Ingresar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ingresar C-Win
ON CHOOSE OF Btn_Ingresar IN FRAME F_Ppto /* Ingresar */
DO:
  DO WITH FRAME F_Ppto:
    W_Adicion = TRUE.
    DISABLE Btn_Modificar Btn_Ingresar Btn_Consultar.
    IF W_ManejaOf = TRUE THEN
      W_OfiPre:SENSITIVE IN FRAME F_Ppto = TRUE.
    ENABLE {&list-1} R_Tipo Btn_Cancelar.
    W_TotAux = W_Total.
    RUN Guardar-VarAux.
    ASSIGN W_Total = 0 W_Cen_Costos = 0 W_Cuenta = "" W_Ano = 0.
    ASSIGN W_Total:SCREEN-VALUE      = "0"
           W_OfiPre:SCREEN-VALUE     = STRING(W_agencia)
           W_Cen_Costos:SCREEN-VALUE = "0"
           W_Cuenta:SCREEN-VALUE     = "0"
           W_Ano:SCREEN-VALUE        = "0"
           W_NomOfi:SCREEN-VALUE     = W_Nom_agencia
           W_NomCco:SCREEN-VALUE     = "" 
           W_NomCta:SCREEN-VALUE     = "".
    RUN Inicializar-Browse.
    VIEW Temp IN FRAME F_ppto.
    APPLY "ENTRY" TO W_OfiPre.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Modificar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Modificar C-Win
ON CHOOSE OF Btn_Modificar IN FRAME F_Ppto /* Modificar */
DO:
  DO WITH FRAME F_Ppto:
    DISABLE Btn_Ingresar Btn_Modificar R_Tipo.
    W_TotAux = W_Total.
    ENABLE {&LIST-1} btn_Cancelar.
    RUN Guardar-VarAux.
    APPLY "ENTRY" TO W_OfiPre.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir C-Win
ON CHOOSE OF Btn_Salir IN FRAME F_Ppto /* Salir */
DO:
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salvar C-Win
ON CHOOSE OF Btn_Salvar IN FRAME F_Ppto /* Salvar */
DO:
  DO WITH FRAME F_Ppto:
    W_Adicion = FALSE.
    IF Presupuesto.Id_CenCostos THEN DO:
      FIND Cen_Costos WHERE Cen_Costos.agencia    EQ W_OfiPre
                        AND Cen_Costos.Cen_Costos EQ W_Cen_Costos
                        AND Cen_Costos.Estado     EQ 1
                    NO-LOCK NO-ERROR.
      IF NOT AVAILABLE(Cen_Costos) THEN DO:
        RUN MostrarMensaje IN W_Manija (INPUT 70, OUTPUT W_Eleccion).
        APPLY "ENTRY" TO W_cen_Costos.
        RETURN NO-APPLY.
      END.
    END.
    FIND Presupuesto WHERE Presupuesto.Estado          EQ 1 
                       AND Presupuesto.Cod_Presupuesto EQ W_Cuenta 
                   NO-LOCK NO-ERROR.
    IF NOT AVAILABLE(Presupuesto) THEN DO:
      RUN MostrarMensaje IN W_Manija (INPUT 66, OUTPUT W_Eleccion).
      APPLY "ENTRY" TO W_cuenta.
      RETURN NO-APPLY.
    END.

    ENABLE Btn_Modificar Btn_Ingresar Btn_Consultar.
    DISABLE R_Tipo Btn_Cancelar.
    ASSIGN W_Cuenta W_OfiPre W_Ano W_Cen_Costos.
    IF W_OfiPre:SCREEN-VALUE = "0" OR W_Cen_Costos:SCREEN-VALUE = "0" OR
       W_Cuenta:SCREEN-VALUE = "0" OR W_Ano:SCREEN-VALUE = "0" THEN
      DO:
       RUN MostrarMensaje IN W_Manija (INPUT 316, OUTPUT W_Eleccion).
       APPLY "ENTRY" TO W_OfiPre.
       RETURN NO-APPLY.
      END.
    ELSE
      RUN Salvar.
    W_OfiPre:SENSITIVE IN FRAME F_Ppto = FALSE.
    DISABLE {&List-1}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME Btn_Todas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Todas C-Win
ON CHOOSE OF Btn_Todas IN FRAME F_Consulta /* Todas */
DO:
  OPEN QUERY B_Consulta FOR EACH Sal_Presupuesto WHERE Sal_Presupuesto.Mes = 01 NO-LOCK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Imprimir
&Scoped-define SELF-NAME BUTTON-69
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-69 C-Win
ON CHOOSE OF BUTTON-69 IN FRAME F-Imprimir /* Generar */
DO:
  DEFINE VAR Listado AS CHARACTER INITIAL "".
  ASSIGN FILL-Ano.
  IF FILL-Ano = 0 THEN
   DO:
    MESSAGE "El Año debe ser Diferente de Cero"
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    APPLY "ENTRY" TO FILL-Ano.
    RETURN NO-APPLY.
   END.
  ELSE
   DO:              
    HIDE FRAME F-Imprimir.
    Listado = W_PathSpl + "L_Detppto.LST".
    {Incluido\Imprimir.I "Listado"} 
    FRAME F_ppto:SENSITIVE = TRUE.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-70
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-70 C-Win
ON CHOOSE OF BUTTON-70 IN FRAME F-Imprimir /* Cancelar */
DO:
  HIDE FRAME F-Imprimir.
  FRAME F_ppto:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ppto
&Scoped-define SELF-NAME BUTTON-71
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-71 C-Win
ON CHOOSE OF BUTTON-71 IN FRAME F_Ppto /* Button 71 */
DO:
  RUN W-infDia.r.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME B_Consulta
&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME B_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_Consulta C-Win
ON ENTRY OF B_Consulta IN FRAME F_Consulta
DO:
/*  APPLY "VALUE-CHANGED" TO B_CONSULTA.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_Consulta C-Win
ON VALUE-CHANGED OF B_Consulta IN FRAME F_Consulta
DO:
   ASSIGN W_Cuenta     = Sal_Presupuesto.Cod_Presupuesto
          W_Ano        = Sal_Presupuesto.Ano
          W_Cen_Costos = Sal_Presupuesto.Cen_Costos
          W_OfiPre     = Sal_Presupuesto.agencia.
   IF W_Cen_costos = 0 THEN
     W_NomCco = "".
   ELSE DO:
     FIND Cen_Costos WHERE Cen_Costos.agencia    EQ W_OfiPre
                       AND Cen_Costos.Cen_Costos EQ W_Cen_Costos 
                   NO-LOCK NO-ERROR.
     IF AVAILABLE(Cen_Costos) THEN
          W_NomCco = Cen_Costos.Nombre.
   END.
   FIND Presupuesto WHERE Presupuesto.Cod_Presupuesto EQ Sal_Presupuesto.Cod_Presupuesto
                  NO-LOCK NO-ERROR.
   IF AVAILABLE(Presupuesto) THEN
      W_NomCta = Presupuesto.Nombre.
   DISPLAY W_Cuenta W_Ano W_Cen_costos W_OfiPre W_Nomcta W_NomCco WITH FRAME F_Ppto.
   RUN Mostrar-Browse.
   DISABLE Temp WITH FRAME F_Ppto.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Imprimir
&Scoped-define SELF-NAME FILL-Ano
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-Ano C-Win
ON LEAVE OF FILL-Ano IN FRAME F-Imprimir /* Año */
DO:
  ASSIGN FILL-Ano.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME R-Detalle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R-Detalle C-Win
ON VALUE-CHANGED OF R-Detalle IN FRAME F-Imprimir
DO:
 DO WITH FRAME F-Imprimir:
  ASSIGN R-Detalle.
  CASE   R-Detalle:
   WHEN 1 THEN
    DO:
     ASSIGN FILL-Ano:SCREEN-VALUE      = "0"
            FILL-Cencos:SCREEN-VALUE   = "0" 
            FILL-agencia:SCREEN-VALUE  = "0"
            FILL-Cencos:SENSITIVE      = FALSE
            FILL-agencia:SENSITIVE     = FALSE.
     APPLY "ENTRY" TO FILL-Ano.
    END.
   WHEN 2 THEN
    DO:
     ASSIGN FILL-Ano:SCREEN-VALUE      = "0"
            FILL-Cencos:SCREEN-VALUE   = "0" 
            FILL-agencia:SCREEN-VALUE  = "0"
            FILL-Cencos:SENSITIVE      = TRUE
            FILL-agencia:SENSITIVE     = TRUE.
     APPLY "ENTRY" TO FILL-Ano.
    END.
  END CASE.
 END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME R_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_Consulta C-Win
ON MOUSE-SELECT-CLICK OF R_Consulta IN FRAME F_Consulta
DO:
  APPLY "VALUE-CHANGED" TO R_Consulta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_Consulta C-Win
ON VALUE-CHANGED OF R_Consulta IN FRAME F_Consulta
OR RETURN OF R_Consulta DO:
  ASSIGN R_consulta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ppto
&Scoped-define SELF-NAME r_tipo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL r_tipo C-Win
ON LEAVE OF r_tipo IN FRAME F_Ppto /* Asigna Valores del Presupuesto a cada mes Automáticamente? */
DO:
  APPLY "VALUE-CHANGED" TO R_Tipo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL r_tipo C-Win
ON MOUSE-SELECT-CLICK OF r_tipo IN FRAME F_Ppto /* Asigna Valores del Presupuesto a cada mes Automáticamente? */
DO:
  APPLY "VALUE-CHANGED" TO R_Tipo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL r_tipo C-Win
ON VALUE-CHANGED OF r_tipo IN FRAME F_Ppto /* Asigna Valores del Presupuesto a cada mes Automáticamente? */
DO:
  DISABLE W_Total WITH FRAME F_Ppto.
  ASSIGN R_Tipo.
  IF R_Tipo THEN DO:
    HIDE Temp IN FRAME F_ppto.
    ENABLE W_Total WITH FRAME F_Ppto.
    APPLY "ENTRY" TO W_Total IN FRAME F_Ppto.
  END.
  ELSE
    VIEW Temp IN FRAME F_ppto.

/*  CASE R_Tipo:
     WHEN 1 THEN
      DO:
         VIEW Temp IN FRAME F_Ppto.
         ENABLE Temp WITH FRAME F_Ppto.
         W_Total = 0.
         RUN Inicializar-Browse.
         APPLY "ENTRY" TO Tmp.W_Valor IN BROWSE Temp.
         RETURN NO-APPLY.  
      END.
     WHEN 2 THEN
      DO:
         HIDE Temp IN FRAME F_ppto.
         ENABLE W_Total WITH FRAME F_Ppto.
         APPLY "ENTRY" TO W_Total IN FRAME F_Ppto.
      END.
     WHEN 3 THEN
      DO:
         VIEW Temp IN FRAME F_Ppto.
         APPLY "ENTRY" TO W_Valor IN BROWSE Temp.
         RETURN NO-APPLY.
      END.
  END CASE.*/
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Ano
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Ano C-Win
ON LEAVE OF W_Ano IN FRAME F_Ppto /* Año */
DO:
  W_WidSel = LAST-EVENT:Widget-Enter.
  IF W_WidSel:NAME <> "Btn_Cancelar" THEN
   DO:
    ASSIGN W_Ano.
    IF W_Ano > YEAR(TODAY) THEN
     DO:
       RUN MostrarMensaje IN W_Manija (INPUT 97, OUTPUT W_Eleccion).
       RETURN NO-APPLY.
     END.
    ELSE
      IF W_Ano = 0 THEN
        RETURN NO-APPLY.
      ELSE DO:
        IF W_Adicion THEN DO:
          FIND FIRST Sal_Presupuesto WHERE Sal_Presupuesto.agencia         EQ W_OfiPre
                                       AND Sal_Presupuesto.Cen_Costos      EQ W_Cen_Costos 
                                       AND Sal_Presupuesto.Cod_Presupuesto EQ W_Cuenta 
                                       AND Sal_Presupuesto.Ano             EQ W_Ano 
                                   NO-LOCK NO-ERROR.
          IF AVAILABLE(Sal_Presupuesto) THEN DO:
            RUN MostrarMensaje IN W_Manija (INPUT 342, OUTPUT W_Eleccion).
            RETURN NO-APPLY.
          END.
        END.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Cen_Costos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Cen_Costos C-Win
ON LEAVE OF W_Cen_Costos IN FRAME F_Ppto /* Centro de Costos */
DO:
  W_WidSel = LAST-EVENT:Widget-Enter.
  IF W_WidSel:NAME <> "Btn_Cancelar" THEN DO:
    ASSIGN W_Cen_Costos.
    FIND Cen_Costos WHERE Cen_Costos.agencia    EQ W_OfiPre
                      AND Cen_Costos.Cen_Costos EQ W_Cen_Costos
                      AND Cen_Costos.Estado     EQ 1
                  NO-LOCK NO-ERROR.

    IF AVAILABLE(Cen_Costos) THEN
      DISPLAY Cen_Costos.Nombre @ W_NomCco WITH FRAME F_Ppto.
    ELSE DO:
       RUN C-Cencosto.r (OUTPUT W_Cen_Costos, OUTPUT W_NomCco).
       DISPLAY W_Cen_Costos WITH FRAME F_Ppto.
       DISPLAY W_NomCco     WITH FRAME F_Ppto.
       IF W_Cen_costos = 0 THEN DO:
         RUN MostrarMensaje IN W_Manija (INPUT 70, OUTPUT W_Eleccion).
         RETURN NO-APPLY.
       END.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Cuenta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Cuenta C-Win
ON LEAVE OF W_Cuenta IN FRAME F_Ppto /* Cuenta */
DO:
  W_WidSel = LAST-EVENT:Widget-Enter.
  IF W_WidSel:NAME <> "Btn_Cancelar" THEN DO:
     ASSIGN W_Cuenta.
     FIND Presupuesto WHERE Presupuesto.Estado          EQ 1 
                        AND Presupuesto.Cod_Presupuesto EQ W_Cuenta
                    NO-LOCK NO-ERROR.
     IF AVAILABLE(Presupuesto) THEN
       DISPLAY Presupuesto.Nombre @ W_NomCta WITH FRAME F_Ppto.
     ELSE DO:
        RUN P-BrwPpt.r (OUTPUT W_Cuenta, OUTPUT W_NomCta).
        DISPLAY W_Cuenta @ W_Cuenta WITH FRAME F_Ppto.
        DISPLAY W_NomCta @ W_NomCta WITH FRAME F_Ppto.
        IF W_Cuenta = " " THEN DO:
          RUN MostrarMensaje IN W_Manija (INPUT 66, OUTPUT W_Eleccion).
          RETURN NO-APPLY.
        END.
        FIND Presupuesto WHERE Presupuesto.Estado          EQ 1 
                           AND Presupuesto.Cod_Presupuesto EQ W_Cuenta
                       NO-LOCK NO-ERROR.
     END.

     IF  W_Adicion = TRUE THEN
       ASSIGN W_NomCco:SCREEN-VALUE = "".
     IF Presupuesto.Id_CenCostos THEN
       ASSIGN W_Cen_costos:SENSITIVE = TRUE
              W_Cen_Costos = 0.
     ELSE
       ASSIGN W_Cen_Costos:SENSITIVE = FALSE
              W_Cen_Costos = 0.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME W_FillConsulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_FillConsulta C-Win
ON LEAVE OF W_FillConsulta IN FRAME F_Consulta /* Valor */
DO:
  ASSIGN FRAME F_Consulta W_FillConsulta.
  CASE R_Consulta:SCREEN-VALUE IN FRAME F_Consulta:
     WHEN "1" THEN
         OPEN QUERY B_Consulta FOR EACH Sal_Presupuesto
              WHERE Sal_Presupuesto.agencia = INTEGER(W_FillConsulta) AND
                    Sal_Presupuesto.Mes = 01 NO-LOCK
              BY Sal_presupuesto.Cod_Presupuesto
              BY Sal_presupuesto.agencia
              BY Sal_presupuesto.Cen_Costos
              BY Sal_presupuesto.Ano.
     WHEN "2" THEN
         OPEN QUERY B_Consulta FOR EACH Sal_presupuesto
              WHERE Sal_Presupuesto.Cen_Costos = INTEGER(W_FillConsulta) AND
              Sal_Presupuesto.Mes = 01 NO-LOCK
              BY Sal_presupuesto.Cod_Presupuesto
              BY Sal_presupuesto.agencia
              BY Sal_presupuesto.Cen_Costos
              BY Sal_presupuesto.Ano.
     WHEN "3" THEN
         OPEN QUERY B_Consulta FOR EACH Sal_Presupuesto
              WHERE Sal_Presupuesto.Cod_Presupuesto BEGINS W_FillConsulta AND
                    Sal_Presupuesto.Mes = 01 NO-LOCK  
              BY Sal_presupuesto.Cod_Presupuesto
              BY Sal_presupuesto.agencia
              BY Sal_presupuesto.Cen_Costos
              BY Sal_presupuesto.Ano.
     WHEN "4" THEN
         OPEN QUERY B_Consulta FOR EACH Sal_Presupuesto
              WHERE Sal_Presupuesto.Ano = INTEGER(W_FillConsulta) AND
                    Sal_Presupuesto.Mes = 01 NO-LOCK
              BY Sal_presupuesto.Cod_Presupuesto
              BY Sal_presupuesto.agencia
              BY Sal_presupuesto.Cen_Costos
              BY Sal_presupuesto.Ano.
    END CASE.
    REPOSITION B_Consulta TO ROWID ROWID(Sal_Presupuesto) NO-ERROR.
    IF B_Consulta:NUM-SELECTED-ROWS  LE 0 THEN 
     OPEN QUERY B_Consulta FOR EACH Sal_Presupuesto WHERE Sal_Presupuesto.Mes = 01 NO-LOCK. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ppto
&Scoped-define SELF-NAME W_OfiPre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_OfiPre C-Win
ON LEAVE OF W_OfiPre IN FRAME F_Ppto /* agencia */
DO:
  W_WidSel = LAST-EVENT:Widget-Enter.
  IF W_WidSel:NAME <> "Btn_Cancelar" THEN
   DO:
     ASSIGN W_OfiPre.
     FIND agencias WHERE agencias.agencia = W_OfiPre NO-LOCK NO-ERROR.
     IF AVAILABLE(agencias) THEN
        W_NomOfi = agencias.Nombre.
     ELSE
      DO:
        RUN P-BrwOfi.r (OUTPUT W_OfiPre, OUTPUT W_NomOfi).
        IF W_OfiPre = 0 THEN
          DO:
            RUN MostrarMensaje IN W_Manija (INPUT 18, OUTPUT W_Eleccion).
            APPLY "ENTRY" TO W_OfiPre.
            RETURN NO-APPLY.
          END.
      END.
     DISPLAY W_NomOfi W_OfiPre WITH FRAME F_Ppto.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Total
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Total C-Win
ON LEAVE OF W_Total IN FRAME F_Ppto /* Total */
DO:
  ASSIGN W_Total.
  RUN Inicializar-Browse.
  RUN Valores-Distribuidos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Imprimir
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


ON LEAVE OF Tmp.W_Valor IN BROWSE Temp DO:
   W_Total = DECIMAL(W_Total:SCREEN-VALUE IN FRAME F_Ppto).
   W_Aux = Tmp.W_Valor.
   ASSIGN INPUT BROWSE Temp W_Valor.
   W_Total = W_Total - W_Aux + Tmp.W_Valor.
   DISPLAY W_Total WITH FRAME F_Ppto.
END.

MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN Inicializar-Variables.
  RUN enable_UI.
  W_ManejaOf = FALSE.

/* si se desa controlar agencia por super usuario quitar los comentarios */
/*MDC  FIND USUARIOS WHERE Usuarios.agencia   EQ W_agencia
                  AND Usuarios.Id_OpeOfi EQ TRUE
                  AND Usuarios.Usuario   EQ W_Usuario 
                  AND Usuarios.Prioridad GT 2
              NO-LOCK NO-ERROR.
  IF AVAILABLE(Usuarios) THEN
    ASSIGN W_OfiPre:SENSITIVE = TRUE
           W_OfiPre:BGCOLOR   = ?
           W_OfiPre:FGCOLOR   = ?
           W_ManejaOf         = TRUE.
  ELSE 
    ASSIGN W_OfiPre           = W_agencia.
           W_OfiPre:SENSITIVE = FALSE.
 MDC*/
  
  DISABLE Temp WITH FRAME F_Ppto.
  
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cancelar C-Win 
PROCEDURE Cancelar :
/*------------------------------------------------------------------------------
  Objetivo:     muestra los valores anteriores del browse, no toma modificaciones
------------------------------------------------------------------------------*/
  DO WITH FRAME F_Ppto:
    ASSIGN W_Cuenta:SCREEN-VALUE     = W_CtaAux
           W_OfiPre:SCREEN-VALUE     = STRING(W_OfiAux)
           W_Cen_Costos:SCREEN-VALUE = STRING(W_CcoAux)
           W_Ano:SCREEN-VALUE        = STRING(W_AnoAux).
    FIND agencias WHERE agencias.agencia = W_Ofiaux NO-LOCK NO-ERROR.
    IF AVAILABLE(agencias) THEN
      W_NomOfi:SCREEN-VALUE = agencias.Nombre.
    FIND Presupuesto WHERE
         Presupuesto.Cod_Presupuesto = W_CtaAux NO-LOCK NO-ERROR.
    IF AVAILABLE(Presupuesto) THEN
       W_NomCta:SCREEN-VALUE = Presupuesto.Nombre.
    FIND FIRST Cen_Costos WHERE 
         Cen_Costos.agencia = W_OfiAux AND
         Cen_Costos.Cen_Costos = W_CcoAux NO-LOCK NO-ERROR.
    IF AVAILABLE(Cen_Costos) THEN
      W_NomCco:SCREEN-VALUE = Cen_Costos.Nombre.
  END.
  GET FIRST Temp.
  DO Cont = 1 TO 12 BY 1:
     ASSIGN Tmp.W_Valor = Vector[Cont].
     GET NEXT Temp.
  END.
  OPEN QUERY Temp FOR EACH Tmp.
  W_Total = W_TotAux.
  DISPLAY W_Total WITH FRAME F_Ppto.
  VIEW Temp IN FRAME F_Ppto.
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
  DISPLAY W_OfiPre W_NomOfi W_Cuenta W_NomCta W_Cen_Costos W_NomCCo W_Ano r_tipo 
          W_Total 
      WITH FRAME F_Ppto IN WINDOW C-Win.
  ENABLE BUTTON-71 Btn_Imprimir Btn_Consultar Btn_Salvar Btn_Ingresar 
         Btn_Modificar Btn_Salir Btn_Ayuda RECT-128 RECT-28 RECT-29 
      WITH FRAME F_Ppto IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-F_Ppto}
  DISPLAY R_Consulta W_FillConsulta 
      WITH FRAME F_Consulta IN WINDOW C-Win.
  ENABLE B_Consulta R_Consulta W_FillConsulta Btn_Todas Btn_CanCon RECT-31 
      WITH FRAME F_Consulta IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-F_Consulta}
  DISPLAY R-Detalle FILL-Ano FILL-agencia FILL-Cencos 
      WITH FRAME F-Imprimir IN WINDOW C-Win.
  ENABLE R-Detalle FILL-Ano BUTTON-69 BUTTON-70 RECT-129 RECT-130 RECT-131 
      WITH FRAME F-Imprimir IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Imprimir}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Guardar-VarAux C-Win 
PROCEDURE Guardar-VarAux :
/*------------------------------------------------------------------------------
  Objetivo:     Guarda los valores anteriores en pantalla antes de hacerse una
                modificacion o de agregar una cuenta.
------------------------------------------------------------------------------*/
   ASSIGN W_CtaAux = W_Cuenta:SCREEN-VALUE IN FRAME F_PPTO
          W_OfiAux = INTEGER(W_OfiPre:SCREEN-VALUE IN FRAME F_PPTO)
          W_CcoAux = INTEGER(W_Cen_Costos:SCREEN-VALUE IN FRAME F_PPTO)
          W_AnoAux = INTEGER(W_Ano:SCREEN-VALUE IN FRAME F_PPTO).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar-Browse C-Win 
PROCEDURE Inicializar-Browse :
/*------------------------------------------------------------------------------
 Objetivo: Prepara el browse para ser mostrado
------------------------------------------------------------------------------*/  
  FOR EACH Tmp:
      ASSIGN Tmp.W_Valor = 0.
  END.
  OPEN QUERY Temp FOR EACH Tmp.
  ENABLE Temp WITH FRAME F_Ppto.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar-Variables C-Win 
PROCEDURE Inicializar-Variables :
/*------------------------------------------------------------------------------
  Objetivo: Inicializa las variables de entrada
------------------------------------------------------------------------------*/
  DO Cont = 1 TO 12 BY 1:
     CREATE Tmp.
     ASSIGN Tmp.W_Mes   = Cont.
  END.

  FIND agencias WHERE agencias.agencia = W_agencia NO-LOCK NO-ERROR.
  IF AVAILABLE(agencias) THEN
    DO:
      W_NomOfi = agencias.Nombre.
      RUN Variables-Pantalla.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar-Browse C-Win 
PROCEDURE Mostrar-Browse :
/*------------------------------------------------------------------------------
  Objetivo:     Muestra el browse en pantalla
------------------------------------------------------------------------------*/
  W_Total = 0.
  OPEN QUERY Temp FOR EACH Tmp.
  ENABLE Temp WITH FRAME F_Ppto.
  GET FIRST Temp.
  FOR EACH Sal_Presupuesto WHERE
     Sal_Presupuesto.agencia       = W_OfiPre     AND
     Sal_Presupuesto.Cen_Costos    = W_Cen_Costos AND
     Sal_Presupuesto.Cod_Presupuesto = W_Cuenta     AND
     Sal_Presupuesto.Ano           = W_Ano:
     ASSIGN Tmp.W_Valor   = Sal_Presupuesto.Sdo_Final
            Vector[W_Mes] = Sal_Presupuesto.Sdo_Final
            W_Total = W_Total + Sal_Presupuesto.Sdo_Final.
     IF W_Mes = 1 THEN
        Tmp.W_Valor:SCREEN-VALUE IN BROWSE Temp = STRING(Sal_Presupuesto.Sdo_Final).
     GET NEXT Temp.
  END.
  OPEN QUERY Temp FOR EACH Tmp.

  W_Total:SCREEN-VALUE IN FRAME F_Ppto = STRING(W_Total).

  VIEW Temp IN FRAME F_Ppto.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Procesoimprimir C-Win 
PROCEDURE Procesoimprimir :
/*------------------------------------------------------------------------------
  Objetivo:     Imprimir el informe de presupuesto tomando las caracteristcas
                de la consulta
------------------------------------------------------------------------------*/
  DEFINE VAR W_Cuenta    AS CHARACTER  FORMAT "X(14)".
  DEFINE VAR I           AS INTEGER    FORMAT "99".
  
  DEFINE VAR W_Tmp       AS DECIMAL    FORMAT "->>>,>>>,>>>,>>>" EXTENT 13
         INITIAL[0,0,0,0,0,0,0,0,0,0,0,0,0].
         
  DEFINE VAR W_Mes       AS CHARACTER  FORMAT "X(10)" EXTENT 12
          INITIAL ["Enero","Frebrero","Marzo","Abril","Mayo","Junio","Julio",
                   "Agosto", "Septiembre","Octubre","Noviembre","Diciembre"].
  DEFINE VAR W_Ofi       AS CHARACTER  FORMAT "X(1)" EXTENT 200.
  

  DEFINE FRAME F-Encabezado
    HEADER
      W_Nom_agencia                                         AT 49
      "PAGINA:"                                             AT 160 PAGE-NUMBER FORMAT ">>>9"
      "CONSULTA SALDOS DE PRESUPUESTO CONSOLIDADO ANUAL"    AT 44 SKIP(1)
      "___________________________________________________________________________________________________________________________________________________________________________________________________________________________"   AT 1 SKIP(1)
      "Cuenta                   Enero          Febrero          Marzo           Abril           Mayo           Junio          Julio           Agosto       Septiembre          Octubre      Noviembre      Diciembre  Total Cuenta"   AT 1
      "___________________________________________________________________________________________________________________________________________________________________________________________________________________________"   AT 1
    WITH DOWN WIDTH 250 USE-TEXT PAGE-TOP FRAME F-Encabezado STREAM-IO.

  DEFINE FRAME F-Reportes0
    WITH WIDTH 250 NO-LABELS USE-TEXT STREAM-IO NO-BOX.
  DEFINE FRAME F-Reportes1
    WITH WIDTH 250 NO-LABELS USE-TEXT STREAM-IO NO-BOX.
  DEFINE FRAME F-Reportes2
    WITH WIDTH 250 NO-LABELS USE-TEXT STREAM-IO NO-BOX.
  DEFINE FRAME F-Reportes3
    WITH WIDTH 250 NO-LABELS USE-TEXT STREAM-IO NO-BOX.
  DEFINE FRAME F-Reportes4
    WITH WIDTH 250 NO-LABELS USE-TEXT STREAM-IO NO-BOX.
  DEFINE FRAME F-Reportes5
    WITH WIDTH 250 NO-LABELS USE-TEXT STREAM-IO NO-BOX.

  DEFINE FRAME F-Total
    WITH WIDTH 250 NO-LABELS.

  DEFINE FRAME F-PiePagina
    HEADER 
      "FECHA:"                  AT 2 
      TODAY                     AT 10 FORMAT "99/99/9999"
      W_Nom_agencia             AT 30 
      "HORA:"                   AT 80 STRING(TIME,"HH:MM:SS")
      WITH WIDTH 132 FRAME F-Piepagina PAGE-BOTTOM USE-TEXT STREAM-IO. 
  
  VIEW FRAME F-Encabezado.
  VIEW FRAME F-Piepagina.
  ASSIGN W_Cuenta = "".
  ASSIGN FRAME F-Imprimir  FILL-Ano 
                           FILL-Cencos 
                           FILL-agencia.
  IF R-Detalle = 1 THEN
   DO:
     FOR EACH Sal_Presupuesto WHERE Sal_presupuesto.Ano = FILL-Ano BY Sal_presupuesto.Ano BY Sal_presupuesto.Cod_Presupuesto: 
         IF W_Cuenta = TRIM(Sal_presupuesto.Cod_Presupuesto) THEN
            ASSIGN W_Tmp[Sal_presupuesto.Mes] = W_Tmp[Sal_presupuesto.Mes] + Sal_presupuesto.Sdo_final
                   W_Tmp[13]                  = W_Tmp[13] + Sal_presupuesto.Sdo_final.
         ELSE
            IF W_Cuenta = "" THEN
               ASSIGN W_Cuenta = TRIM(Sal_presupuesto.Cod_Presupuesto)
                      W_Tmp[Sal_presupuesto.Mes] = W_Tmp[Sal_presupuesto.Mes] + Sal_presupuesto.Sdo_final
                      W_Tmp[13]                  = W_Tmp[13] + Sal_presupuesto.Sdo_final.
            ELSE
             DO:
              DISPLAY W_Cuenta     AT 1   NO-LABEL
                      W_Tmp[1]     AT 15  NO-LABEL
                      W_Tmp[2]     AT 31  NO-LABEL
                      W_Tmp[3]     AT 47  NO-LABEL
                      W_Tmp[4]     AT 63  NO-LABEL
                      W_Tmp[5]     AT 79  NO-LABEL
                      W_Tmp[6]     AT 95  NO-LABEL
                      W_Tmp[7]     AT 111 NO-LABEL
                      W_Tmp[8]     AT 127 NO-LABEL
                      W_Tmp[9]     AT 143 NO-LABEL
                      W_Tmp[10]    AT 159 NO-LABEL
                      W_Tmp[11]    AT 175 NO-LABEL
                      W_Tmp[12]    AT 191 NO-LABEL
                      W_Tmp[13]    AT 207 NO-LABEL
                      WITH FRAME F-Reportes0 WIDTH 250.
              REPEAT I = 1 TO 13 BY 1:
                W_Tmp[I] = 0.
              END. /* REPEAT*/
              ASSIGN W_Cuenta = TRIM(Sal_presupuesto.Cod_Presupuesto)
                     W_Tmp[Sal_presupuesto.Mes] = W_Tmp[Sal_presupuesto.Mes] + Sal_presupuesto.Sdo_final
                     W_Tmp[13]                  = W_Tmp[13] + Sal_presupuesto.Sdo_final.
             END.
     END.
     DISPLAY W_Cuenta     AT 1   NO-LABEL
             W_Tmp[1]     AT 15  NO-LABEL
             W_Tmp[2]     AT 31  NO-LABEL
             W_Tmp[3]     AT 47  NO-LABEL
             W_Tmp[4]     AT 63  NO-LABEL
             W_Tmp[5]     AT 79  NO-LABEL
             W_Tmp[6]     AT 95  NO-LABEL
             W_Tmp[7]     AT 111 NO-LABEL
             W_Tmp[8]     AT 127 NO-LABEL
             W_Tmp[9]     AT 143 NO-LABEL
             W_Tmp[10]    AT 159 NO-LABEL
             W_Tmp[11]    AT 175 NO-LABEL
             W_Tmp[12]    AT 191 NO-LABEL
             W_Tmp[13]    AT 207 NO-LABEL
             WITH FRAME F-Reportes1 WIDTH 250.
   END.
  ELSE
   DO:
    /* Consolidado por agencia*/
    IF FILL-agencia > 0 AND FILL-Cencos = 0 THEN
     DO:
      FOR EACH Sal_Presupuesto WHERE Sal_presupuesto.Ano     = FILL-Ano AND
                                     Sal_presupuesto.agencia = FILL-agencia
                                     BY Sal_presupuesto.Ano
                                     BY Sal_presupuesto.agencia:
         IF W_Cuenta = TRIM(Sal_presupuesto.Cod_Presupuesto) THEN
            ASSIGN W_Tmp[Sal_presupuesto.Mes] = W_Tmp[Sal_presupuesto.Mes] + Sal_presupuesto.Sdo_final
                   W_Tmp[13]                  = W_Tmp[13] + Sal_presupuesto.Sdo_final.
         ELSE
            IF W_Cuenta = "" THEN
               ASSIGN W_Cuenta = TRIM(Sal_presupuesto.Cod_Presupuesto)
                      W_Tmp[Sal_presupuesto.Mes] = W_Tmp[Sal_presupuesto.Mes] + Sal_presupuesto.Sdo_final
                      W_Tmp[13]                  = W_Tmp[13] + Sal_presupuesto.Sdo_final.
            ELSE
             DO:
              DISPLAY W_Cuenta     AT 1   NO-LABEL
                      W_Tmp[1]     AT 15  NO-LABEL
                      W_Tmp[2]     AT 31  NO-LABEL
                      W_Tmp[3]     AT 47  NO-LABEL
                      W_Tmp[4]     AT 63  NO-LABEL
                      W_Tmp[5]     AT 79  NO-LABEL
                      W_Tmp[6]     AT 95  NO-LABEL
                      W_Tmp[7]     AT 111 NO-LABEL
                      W_Tmp[8]     AT 127 NO-LABEL
                      W_Tmp[9]     AT 143 NO-LABEL
                      W_Tmp[10]    AT 159 NO-LABEL
                      W_Tmp[11]    AT 175 NO-LABEL
                      W_Tmp[12]    AT 191 NO-LABEL
                      W_Tmp[13]    AT 207 NO-LABEL
                      WITH FRAME F-Reportes2 WIDTH 250.
              REPEAT I = 1 TO 13 BY 1:
                W_Tmp[I] = 0.
              END. /* REPEAT*/
              ASSIGN W_Cuenta = TRIM(Sal_presupuesto.Cod_Presupuesto)
                     W_Tmp[Sal_presupuesto.Mes] = W_Tmp[Sal_presupuesto.Mes] + Sal_presupuesto.Sdo_final
                     W_Tmp[13]                  = W_Tmp[13] + Sal_presupuesto.Sdo_final.
             END.
      END.
      DISPLAY W_Cuenta     AT 1   NO-LABEL
              W_Tmp[1]     AT 15  NO-LABEL
              W_Tmp[2]     AT 31  NO-LABEL
              W_Tmp[3]     AT 47  NO-LABEL
              W_Tmp[4]     AT 63  NO-LABEL
              W_Tmp[5]     AT 79  NO-LABEL
              W_Tmp[6]     AT 95  NO-LABEL
              W_Tmp[7]     AT 111 NO-LABEL
              W_Tmp[8]     AT 127 NO-LABEL
              W_Tmp[9]     AT 143 NO-LABEL
              W_Tmp[10]    AT 159 NO-LABEL
              W_Tmp[11]    AT 175 NO-LABEL
              W_Tmp[12]    AT 191 NO-LABEL
              W_Tmp[13]    AT 207 NO-LABEL
              WITH FRAME F-Reportes3 WIDTH 250.
     END. 
     
   /* Consolidado por Centro de Costos */
   IF FILL-agencia > 0 AND FILL-Cencos > 0 THEN
     DO:
      FOR EACH Sal_Presupuesto WHERE Sal_presupuesto.Ano        = FILL-Ano     AND
                                     Sal_presupuesto.agencia    = FILL-agencia AND
                                     Sal_presupuesto.Cen_Costos = FILL-Cencos:
         IF W_Cuenta = TRIM(Sal_presupuesto.Cod_Presupuesto) THEN
            ASSIGN W_Tmp[Sal_presupuesto.Mes] = W_Tmp[Sal_presupuesto.Mes] + Sal_presupuesto.Sdo_final
                   W_Tmp[13]                  = W_Tmp[13] + Sal_presupuesto.Sdo_final.
         ELSE
            IF W_Cuenta = "" THEN
               ASSIGN W_Cuenta = TRIM(Sal_presupuesto.Cod_Presupuesto)
                      W_Tmp[Sal_presupuesto.Mes] = W_Tmp[Sal_presupuesto.Mes] + Sal_presupuesto.Sdo_final
                      W_Tmp[13]                  = W_Tmp[13] + Sal_presupuesto.Sdo_final.
            ELSE
             DO:
              DISPLAY W_Cuenta     AT 1   NO-LABEL
                      W_Tmp[1]     AT 15  NO-LABEL
                      W_Tmp[2]     AT 31  NO-LABEL
                      W_Tmp[3]     AT 47  NO-LABEL
                      W_Tmp[4]     AT 63  NO-LABEL
                      W_Tmp[5]     AT 79  NO-LABEL
                      W_Tmp[6]     AT 95  NO-LABEL
                      W_Tmp[7]     AT 111 NO-LABEL
                      W_Tmp[8]     AT 127 NO-LABEL
                      W_Tmp[9]     AT 143 NO-LABEL
                      W_Tmp[10]    AT 159 NO-LABEL
                      W_Tmp[11]    AT 175 NO-LABEL
                      W_Tmp[12]    AT 191 NO-LABEL
                      W_Tmp[13]    AT 207 NO-LABEL
                      WITH FRAME F-Reportes4 WIDTH 250.
              REPEAT I = 1 TO 13 BY 1:
                W_Tmp[I] = 0.
              END. /* REPEAT*/
              ASSIGN W_Cuenta = TRIM(Sal_presupuesto.Cod_Presupuesto)
                     W_Tmp[Sal_presupuesto.Mes] = W_Tmp[Sal_presupuesto.Mes] + Sal_presupuesto.Sdo_final
                     W_Tmp[13]                  = W_Tmp[13] + Sal_presupuesto.Sdo_final.
             END.
      END.
      DISPLAY W_Cuenta     AT 1   NO-LABEL
              W_Tmp[1]     AT 15  NO-LABEL
              W_Tmp[2]     AT 31  NO-LABEL
              W_Tmp[3]     AT 47  NO-LABEL
              W_Tmp[4]     AT 63  NO-LABEL
              W_Tmp[5]     AT 79  NO-LABEL
              W_Tmp[6]     AT 95  NO-LABEL
              W_Tmp[7]     AT 111 NO-LABEL
              W_Tmp[8]     AT 127 NO-LABEL
              W_Tmp[9]     AT 143 NO-LABEL
              W_Tmp[10]    AT 159 NO-LABEL
              W_Tmp[11]    AT 175 NO-LABEL
              W_Tmp[12]    AT 191 NO-LABEL
              W_Tmp[13]    AT 207 NO-LABEL
              WITH FRAME F-Reportes5 WIDTH 250.
     END. 
   END.
  PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Salvar C-Win 
PROCEDURE Salvar :
/*------------------------------------------------------------------------------
  Objetivo:  Toma los datos de pantalla y los graba en la tabla Sal_presupuesto
------------------------------------------------------------------------------*/  
  GET FIRST Temp.
  DO Cont = 1 TO 12 BY 1:
     FIND Sal_Presupuesto WHERE Sal_Presupuesto.agencia         EQ W_OfiPre
                            AND Sal_Presupuesto.Cen_Costos      EQ W_Cen_Costos 
                            AND Sal_Presupuesto.Cod_Presupuesto EQ W_Cuenta 
                            AND Sal_Presupuesto.Ano             EQ W_Ano
                            AND Sal_Presupuesto.Mes             EQ Cont 
                     SHARE-LOCK NO-ERROR.
     IF AVAILABLE(Sal_Presupuesto) THEN
        ASSIGN Sal_Presupuesto.Sdo_Final  = Tmp.W_Valor.
     ELSE DO:
       CREATE Sal_Presupuesto.
       ASSIGN Sal_Presupuesto.agencia         = W_OfiPre
              Sal_Presupuesto.Cen_Costos      = W_Cen_Costos
              Sal_Presupuesto.Cod_Presupuesto = W_Cuenta
              Sal_Presupuesto.Ano             = W_Ano
              Sal_Presupuesto.Mes             = Cont
              Sal_Presupuesto.Sdo_Final       = Tmp.W_Valor.
     END.
     GET NEXT Temp.
  END.
  GET FIRST Temp.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Valores-Distribuidos C-Win 
PROCEDURE Valores-Distribuidos :
/*------------------------------------------------------------------------------
  Objetivo:     Distribuye en los 12 meses el valor entrado en w_total
------------------------------------------------------------------------------*/
  W_ValMes = W_Total / 12.
  FOR EACH Tmp:
    ASSIGN Tmp.W_Valor = W_ValMes.
  END.
  OPEN QUERY Temp FOR EACH Tmp.
  APPLY "ENTRY" TO Tmp.W_Valor IN BROWSE Temp.
  RETURN ERROR.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Variables-Pantalla C-Win 
PROCEDURE Variables-Pantalla :
/*------------------------------------------------------------------------------
  Objetivo:     Muestra el registro de la tabla en las vbles de pantalaa
------------------------------------------------------------------------------*/
   FIND FIRST Sal_Presupuesto WHERE
        Sal_Presupuesto.agencia = W_agencia NO-LOCK NO-ERROR.
   IF AVAILABLE(Sal_Presupuesto) THEN DO:
     ASSIGN W_Cuenta     = Sal_Presupuesto.Cod_Presupuesto
            W_Ano        = Sal_Presupuesto.Ano
            W_Cen_Costos = Sal_Presupuesto.Cen_Costos
            W_OfiPre     = Sal_Presupuesto.agencia.
     FIND Presupuesto WHERE
          Presupuesto.Cod_Presupuesto = Sal_Presupuesto.Cod_Presupuesto NO-LOCK NO-ERROR.
     IF AVAILABLE(Presupuesto) THEN
        W_NomCta = Presupuesto.Nombre.
     FIND FIRST Cen_Costos WHERE 
          Cen_Costos.agencia = Sal_Presupuesto.agencia AND
          Cen_Costos.Cen_Costos = Sal_Presupuesto.Cen_Costos NO-LOCK NO-ERROR.
     IF AVAILABLE(Cen_Costos) THEN
       W_NomCco = Cen_Costos.Nombre.
   END.            
   RUN Mostrar-Browse.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

