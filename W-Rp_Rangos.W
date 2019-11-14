&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-ProCre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-ProCre 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local VARIABLE Definitions ---                                       */
  {INCLUIDO/Variable.i "SHARED"}
  
  DEFINE VAR tpv AS DECIMAL FORMAT ">>9,9999" INITIAL 0.
  DEFINE VAR ttpv AS DECIMAL FORMAT ">>9,9999".
  DEFINE VARIABLE W_CbStr       AS   CHARACTER.
  DEFINE VARIABLE W_SuperUsu    AS   LOGICAL.
  DEFINE VARIABLE W_OfStr       AS   CHARACTER FORMAT "X(20)".
  DEFINE VARIABLE W_PctoStr     AS   CHARACTER FORMAT "X(20)".
  DEFINE VARIABLE W_Ofitbajo    LIKE agencias.agencia.
  DEFINE VARIABLE W_OfiIni      LIKE agencias.agencia.
  DEFINE VARIABLE W_OfiFin      LIKE agencias.agencia.
  DEFINE VARIABLE W_agenciaP    AS   CHARACTER FORMAT "X(20)".  
  DEFINE VARIABLE W_WkOfi       LIKE agencias.agencia.
  DEFINE VARIABLE W_Rpta        AS   LOGICAL INITIAL FALSE.
  DEFINE VARIABLE W_Metodo      AS   LOGICAL.
  DEFINE VARIABLE W_ProIni      LIKE PRO_creditos.cod_credito.
  DEFINE VARIABLE W_ProFin      LIKE PRO_creditos.cod_credito.
  DEFINE VARIABLE W_SaldoAho    LIKE Ahorros.Sdo_Disponible INITIAL 0.
  DEFINE VARIABLE W_Opcinforme  AS   CHARACTER.     
  DEFINE VARIABLE ProcName      AS   CHARACTER INITIAL "L-INFRAN.LST".
  DEFINE VARIABLE W_Linea       AS   CHARACTER FORMAT "X(95)" INITIAL "".
  DEFINE VARIABLE W_Linea1      AS   CHARACTER FORMAT "X(95)" INITIAL "".  
  DEFINE VARIABLE Raya          AS   CHARACTER FORMAT "X" INITIAL "-".  
  DEFINE VARIABLE W_Eleccion1   AS   LOGICAL.
  DEFINE VARIABLE W_NomInf      AS   CHARACTER FORMAT "x(10)".
  DEFINE VARIABLE W_DisAhoIni   LIKE Ahorros.Sdo_Disponible INITIAL 0.
  DEFINE VARIABLE W_DisAhoFin   LIKE Ahorros.Sdo_Disponible INITIAL 0.
  DEFINE VARIABLE W_CapCreIni   LIKE Creditos.Monto INITIAL 0.
  DEFINE VARIABLE W_CapCreFin   LIKE Creditos.Monto INITIAL 0.
  DEFINE VARIABLE W_CapitalCre  LIKE Creditos.Monto INITIAL 0.
  DEFINE VARIABLE W_Cont        AS   INTEGER INITIAL 0.
  DEFINE VARIABLE W_TotGen      AS   DECIMAL FORMAT "->>>,>>>,>>>,>>9" INITIAL 0.
  DEFINE VARIABLE W_TotCont     AS   DECIMAL FORMAT ">>>,>>>,>>9"      INITIAL 0.
  DEFINE VARIABLE W_WkIni       AS   DECIMAL FORMAT "->>>,>>>,>>>,>>9" INITIAL 0.
  DEFINE VARIABLE W_WkFin       AS   DECIMAL FORMAT "->>>,>>>,>>>,>>9" INITIAL 0.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS W_CmbOfi R_opcion w_cmbpcto BUTTON-1 FVI ~
FINC Btn_Consulta Btn_Imprimir W_Rango1i w_Rango1f W_Rango2i W_Rango2f ~
W_Rango3i W_Rango3f W_Rango4i W_Rango4f W_Rango5i W_Rango5f W_Rango6i ~
W_Rango6f w_Rango7i W_Rango7f W_Rango8i W_Rango8f Btn_Cancelar W_Rango9i ~
w_Rango9f w_Rango10i W_Rango10f Btn_Ayuda RECT-23 RECT-24 RECT-5 
&Scoped-Define DISPLAYED-OBJECTS W_CmbOfi R_opcion w_cmbpcto FVI FINC ~
W_Rango1i w_Rango1f Valor1 ocur1 TP1 W_Rango2i W_Rango2f valor2 ocur2 tp2 ~
W_Rango3i W_Rango3f valor3 ocur3 tp3 W_Rango4i W_Rango4f valor4 ocur4 tp4 ~
W_Rango5i W_Rango5f valor5 ocur5 tp5 W_Rango6i W_Rango6f valor6 ocur6 tp6 ~
w_Rango7i W_Rango7f valor7 ocur7 tp7 W_Rango8i W_Rango8f valor8 ocur8 tp8 ~
W_Rango9i w_Rango9f valor9 ocur9 tp9 w_Rango10i W_Rango10f valor10 ocur10 ~
tp10 W_Totvalor W_TotOcur TTTpv 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-ProCre AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Ayuda 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "" 
     SIZE 4 BY 1.08.

DEFINE BUTTON Btn_Cancelar 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "S&alir" 
     SIZE 10 BY 1.62
     FONT 15.

DEFINE BUTTON Btn_Consulta 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     LABEL "&Nueva Consulta" 
     SIZE 10 BY 1.62.

DEFINE BUTTON Btn_Imprimir 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "I&mprimir" 
     SIZE 10 BY 1.62.

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 1" 
     SIZE 10 BY 1.65.

DEFINE VARIABLE W_CmbOfi AS CHARACTER FORMAT "X(20)":U 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 28 BY .81 TOOLTIP "agencias Activas"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE w_cmbpcto AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 28 BY .81 TOOLTIP "Productos de Captación o Créditos Disponibles"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FINC AS DECIMAL FORMAT ">>>,>>>,>>9":U INITIAL 1000000 
     LABEL "Incremento" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE FVI AS DECIMAL FORMAT ">>>,>>>,>>9":U INITIAL 1000000 
     LABEL "Valor Inicial" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE ocur1 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE ocur10 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE ocur2 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE ocur3 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE ocur4 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE ocur5 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE ocur6 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE ocur7 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE ocur8 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE ocur9 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TP1 AS DECIMAL FORMAT ">>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE tp10 AS DECIMAL FORMAT ">>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE tp2 AS DECIMAL FORMAT ">>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE tp3 AS DECIMAL FORMAT ">>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE tp4 AS DECIMAL FORMAT ">>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE tp5 AS DECIMAL FORMAT ">>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE tp6 AS DECIMAL FORMAT ">>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE tp7 AS DECIMAL FORMAT ">>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE tp8 AS DECIMAL FORMAT ">>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE tp9 AS DECIMAL FORMAT ">>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TTTpv AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Valor1 AS DECIMAL FORMAT "->>>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE valor10 AS DECIMAL FORMAT "->>>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE valor2 AS DECIMAL FORMAT "->>>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE valor3 AS DECIMAL FORMAT "->>>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE valor4 AS DECIMAL FORMAT "->>>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE valor5 AS DECIMAL FORMAT "->>>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE valor6 AS DECIMAL FORMAT "->>>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE valor7 AS DECIMAL FORMAT "->>>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE valor8 AS DECIMAL FORMAT "->>>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE valor9 AS DECIMAL FORMAT "->>>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Rango10f AS DECIMAL FORMAT "->>>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE w_Rango10i AS DECIMAL FORMAT "->>>,>>>,>>>,>>9":U INITIAL 0 
     LABEL "10.)" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE w_Rango1f AS DECIMAL FORMAT "->>>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Rango1i AS DECIMAL FORMAT "->>>,>>>,>>>,>>9":U INITIAL 0 
     LABEL "1.)" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Rango2f AS DECIMAL FORMAT "->>>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Rango2i AS DECIMAL FORMAT "->>>,>>>,>>>,>>9":U INITIAL 0 
     LABEL "2.)" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Rango3f AS DECIMAL FORMAT "->>>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Rango3i AS DECIMAL FORMAT "->>>,>>>,>>>,>>9":U INITIAL 0 
     LABEL "3.)" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Rango4f AS DECIMAL FORMAT "->>>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Rango4i AS DECIMAL FORMAT "->>>,>>>,>>>,>>9":U INITIAL 0 
     LABEL "4.)" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Rango5f AS DECIMAL FORMAT "->>>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Rango5i AS DECIMAL FORMAT "->>>,>>>,>>>,>>9":U INITIAL 0 
     LABEL "5.)" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Rango6f AS DECIMAL FORMAT "->>>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Rango6i AS DECIMAL FORMAT "->>>,>>>,>>>,>>9":U INITIAL 0 
     LABEL "6.)" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Rango7f AS DECIMAL FORMAT "->>>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE w_Rango7i AS DECIMAL FORMAT "->>>,>>>,>>>,>>9":U INITIAL 0 
     LABEL "7.)" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Rango8f AS DECIMAL FORMAT "->>>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Rango8i AS DECIMAL FORMAT "->>>,>>>,>>>,>>9":U INITIAL 0 
     LABEL "8.)" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE w_Rango9f AS DECIMAL FORMAT "->>>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Rango9i AS DECIMAL FORMAT "->>>,>>>,>>>,>>9":U INITIAL 0 
     LABEL "9.)" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_TotOcur AS DECIMAL FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Totvalor AS DECIMAL FORMAT "->>>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R_opcion AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Captación", 1,
"Créditos", 2
     SIZE 25 BY .81
     BGCOLOR 17 FONT 5 NO-UNDO.

DEFINE RECTANGLE RECT-23
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 109 BY 1.62.

DEFINE RECTANGLE RECT-24
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 52 BY 1.88.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 69 BY 1.35.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     W_CmbOfi AT ROW 3.69 COL 11 COLON-ALIGNED
     R_opcion AT ROW 3.69 COL 56 HELP
          "Seleccione el tipo de producto a consultar" NO-LABEL
     w_cmbpcto AT ROW 3.69 COL 81 HELP
          "Seleccione el producto que desea consultar" NO-LABEL
     BUTTON-1 AT ROW 5.31 COL 100
     FVI AT ROW 6.38 COL 40 COLON-ALIGNED
     FINC AT ROW 6.38 COL 65 COLON-ALIGNED
     Btn_Consulta AT ROW 6.92 COL 100
     Btn_Imprimir AT ROW 8.54 COL 100 HELP
          "Permite Generar la Impresión de Saldos"
     W_Rango1i AT ROW 9.35 COL 21.57 COLON-ALIGNED HELP
          "Ingrese el Valor inicial para el Rango de valores"
     w_Rango1f AT ROW 9.35 COL 39 COLON-ALIGNED HELP
          "Ingrese el Valor final para el Rango de valores" NO-LABEL
     Valor1 AT ROW 9.35 COL 56 COLON-ALIGNED NO-LABEL
     ocur1 AT ROW 9.35 COL 70 COLON-ALIGNED NO-LABEL
     TP1 AT ROW 9.35 COL 80 COLON-ALIGNED NO-LABEL
     W_Rango2i AT ROW 10.15 COL 21.57 COLON-ALIGNED HELP
          "Ingrese el Valor inicial para el Rango de valores"
     W_Rango2f AT ROW 10.15 COL 39 COLON-ALIGNED HELP
          "Ingrese el Valor final para el Rango de valores" NO-LABEL
     valor2 AT ROW 10.15 COL 56 COLON-ALIGNED NO-LABEL
     ocur2 AT ROW 10.15 COL 70 COLON-ALIGNED NO-LABEL
     tp2 AT ROW 10.15 COL 80 COLON-ALIGNED NO-LABEL
     W_Rango3i AT ROW 10.96 COL 21.57 COLON-ALIGNED HELP
          "Ingrese el Valor inicial para el Rango de valores"
     W_Rango3f AT ROW 10.96 COL 39 COLON-ALIGNED HELP
          "Ingrese el Valor final para el Rango de valores" NO-LABEL
     valor3 AT ROW 10.96 COL 56 COLON-ALIGNED NO-LABEL
     ocur3 AT ROW 10.96 COL 70 COLON-ALIGNED NO-LABEL
     tp3 AT ROW 10.96 COL 80 COLON-ALIGNED NO-LABEL
     W_Rango4i AT ROW 11.77 COL 21.57 COLON-ALIGNED HELP
          "Ingrese el Valor inicial para el Rango de valores"
     W_Rango4f AT ROW 11.77 COL 39 COLON-ALIGNED HELP
          "Ingrese el Valor final para el Rango de valores" NO-LABEL
     valor4 AT ROW 11.77 COL 56 COLON-ALIGNED NO-LABEL
     ocur4 AT ROW 11.77 COL 70 COLON-ALIGNED NO-LABEL
     tp4 AT ROW 11.77 COL 80 COLON-ALIGNED NO-LABEL
     W_Rango5i AT ROW 12.58 COL 21.57 COLON-ALIGNED HELP
          "Ingrese el Valor inicial para el Rango de valores"
     W_Rango5f AT ROW 12.58 COL 39 COLON-ALIGNED HELP
          "Ingrese el Valor final para el Rango de valores" NO-LABEL
     valor5 AT ROW 12.58 COL 56 COLON-ALIGNED NO-LABEL
     ocur5 AT ROW 12.58 COL 70 COLON-ALIGNED NO-LABEL
     tp5 AT ROW 12.58 COL 80 COLON-ALIGNED NO-LABEL
     W_Rango6i AT ROW 13.38 COL 21.57 COLON-ALIGNED HELP
          "Ingrese el Valor inicial para el Rango de valores"
     W_Rango6f AT ROW 13.38 COL 39 COLON-ALIGNED HELP
          "Ingrese el Valor final para el Rango de valores" NO-LABEL
     valor6 AT ROW 13.38 COL 56 COLON-ALIGNED NO-LABEL
     ocur6 AT ROW 13.38 COL 70 COLON-ALIGNED NO-LABEL
     tp6 AT ROW 13.38 COL 80 COLON-ALIGNED NO-LABEL
     w_Rango7i AT ROW 14.19 COL 21.57 COLON-ALIGNED HELP
          "Ingrese el Valor inicial para el Rango de valores"
     W_Rango7f AT ROW 14.19 COL 39 COLON-ALIGNED HELP
          "Ingrese el Valor final para el Rango de valores" NO-LABEL
     valor7 AT ROW 14.19 COL 56 COLON-ALIGNED NO-LABEL
     ocur7 AT ROW 14.19 COL 70 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.14 BY 21.12
         BGCOLOR 17 FONT 4.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     tp7 AT ROW 14.19 COL 80 COLON-ALIGNED NO-LABEL
     W_Rango8i AT ROW 15 COL 21.57 COLON-ALIGNED HELP
          "Ingrese el Valor inicial para el Rango de valores"
     W_Rango8f AT ROW 15 COL 39 COLON-ALIGNED HELP
          "Ingrese el Valor final para el Rango de valores" NO-LABEL
     valor8 AT ROW 15 COL 56 COLON-ALIGNED NO-LABEL
     ocur8 AT ROW 15 COL 70 COLON-ALIGNED NO-LABEL
     tp8 AT ROW 15 COL 80 COLON-ALIGNED NO-LABEL
     Btn_Cancelar AT ROW 15 COL 100 HELP
          "Permite Regresar a la Opción anterior"
     W_Rango9i AT ROW 15.81 COL 21.57 COLON-ALIGNED HELP
          "Ingrese el Valor inicial para el Rango de valores"
     w_Rango9f AT ROW 15.81 COL 39 COLON-ALIGNED HELP
          "Ingrese el Valor final para el Rango de valores" NO-LABEL
     valor9 AT ROW 15.81 COL 56 COLON-ALIGNED NO-LABEL
     ocur9 AT ROW 15.81 COL 70 COLON-ALIGNED NO-LABEL
     tp9 AT ROW 15.81 COL 80 COLON-ALIGNED NO-LABEL
     w_Rango10i AT ROW 16.62 COL 21.57 COLON-ALIGNED HELP
          "Ingrese el Valor inicial para el Rango de valores"
     W_Rango10f AT ROW 16.62 COL 39 COLON-ALIGNED HELP
          "Ingrese el Valor final para el Rango de valores" NO-LABEL
     valor10 AT ROW 16.62 COL 56 COLON-ALIGNED NO-LABEL
     ocur10 AT ROW 16.62 COL 70 COLON-ALIGNED NO-LABEL
     tp10 AT ROW 16.62 COL 80 COLON-ALIGNED NO-LABEL
     Btn_Ayuda AT ROW 16.88 COL 103
     W_Totvalor AT ROW 17.69 COL 55 COLON-ALIGNED NO-LABEL
     W_TotOcur AT ROW 17.69 COL 70 COLON-ALIGNED NO-LABEL
     TTTpv AT ROW 17.69 COL 80 COLON-ALIGNED NO-LABEL
     RECT-23 AT ROW 3.15 COL 2
     RECT-24 AT ROW 5.85 COL 31
     RECT-5 AT ROW 17.42 COL 23
     "Parametros de Consulta" VIEW-AS TEXT
          SIZE 22 BY .54 AT ROW 2.88 COL 3
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "Producto:" VIEW-AS TEXT
          SIZE 9 BY .81 AT ROW 3.69 COL 47
          FONT 5
     "Valor Inicial" VIEW-AS TEXT
          SIZE 12 BY .54 AT ROW 8.54 COL 23
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "Valor final" VIEW-AS TEXT
          SIZE 11 BY .54 AT ROW 8.54 COL 41
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "Valor por Rango" VIEW-AS TEXT
          SIZE 12 BY .5 AT ROW 8.54 COL 58
          BGCOLOR 17 FGCOLOR 7 
     "Ocurrencias" VIEW-AS TEXT
          SIZE 9 BY .5 AT ROW 8.54 COL 71
          BGCOLOR 17 FGCOLOR 7 
     "Totales de Consulta:" VIEW-AS TEXT
          SIZE 14 BY .54 AT ROW 17.96 COL 41
          BGCOLOR 17 FGCOLOR 7 
     "Tasa Ponderada" VIEW-AS TEXT
          SIZE 12 BY .5 AT ROW 8.54 COL 81
          FGCOLOR 7 
     "  Valores Predeterminados" VIEW-AS TEXT
          SIZE 25 BY 1.08 AT ROW 5.31 COL 32
          FGCOLOR 7 FONT 5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.14 BY 21.12
         BGCOLOR 17 FONT 4.


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
         TITLE              = "Manejo de Rangos"
         HEIGHT             = 21.12
         WIDTH              = 113.14
         MAX-HEIGHT         = 21.12
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 21.12
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-ProCre 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-ProCre
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE                                                          */
/* SETTINGS FOR FILL-IN ocur1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ocur10 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ocur2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ocur3 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ocur4 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ocur5 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ocur6 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ocur7 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ocur8 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ocur9 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN TP1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tp10 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tp2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tp3 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tp4 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tp5 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tp6 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tp7 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tp8 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tp9 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN TTTpv IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Valor1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN valor10 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN valor2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN valor3 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN valor4 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN valor5 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN valor6 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN valor7 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN valor8 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN valor9 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX w_cmbpcto IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN W_TotOcur IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Totvalor IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-ProCre)
THEN W-ProCre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-ProCre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-ProCre W-ProCre
ON END-ERROR OF W-ProCre /* Manejo de Rangos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-ProCre W-ProCre
ON WINDOW-CLOSE OF W-ProCre /* Manejo de Rangos */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Ayuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ayuda W-ProCre
ON CHOOSE OF Btn_Ayuda IN FRAME F-Main
OR HELP OF {&WINDOW-NAME} DO:
   SYSTEM-HELP "\AYUDAS\AHORRO.HLP" CONTEXT 163.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancelar W-ProCre
ON CHOOSE OF Btn_Cancelar IN FRAME F-Main /* Salir */
DO:
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Consulta W-ProCre
ON CHOOSE OF Btn_Consulta IN FRAME F-Main /* Nueva Consulta */
DO:
  RUN Inicia_Campos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imprimir W-ProCre
ON CHOOSE OF Btn_Imprimir IN FRAME F-Main /* Imprimir */
DO:
  DEFINE VARIABLE Listado AS CHARACTER INITIAL "".
  ASSIGN Listado = W_PathSpl + "L-infran.Lst".
  {incluido/Imprimir.I "listado"}     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 W-ProCre
ON CHOOSE OF BUTTON-1 IN FRAME F-Main /* Button 1 */
DO:
  RUN W-infdia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FINC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FINC W-ProCre
ON LEAVE OF FINC IN FRAME F-Main /* Incremento */
DO:
  ASSIGN FRAME f-main FINC.
  RUN Inicia_Campos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FVI
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FVI W-ProCre
ON LEAVE OF FVI IN FRAME F-Main /* Valor Inicial */
DO:
  ASSIGN FRAME f-main FVI.
  RUN Inicia_Campos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME R_opcion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_opcion W-ProCre
ON MOUSE-SELECT-CLICK OF R_opcion IN FRAME F-Main
DO:
  APPLY "VALUE-CHANGED" TO R_Opcion.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_opcion W-ProCre
ON VALUE-CHANGED OF R_opcion IN FRAME F-Main
OR RETURN OF R_Opcion OR TAB OF R_Opcion DO:
  DO WITH FRAME {&FRAME-NAME}:  
     ASSIGN R_Opcion.
     IF R_Opcion EQ 1 THEN DO:
        ASSIGN W_PctoStr = "000 CONSOLIDADO"
               W_CmbPcto:LIST-ITEMS = ""
               W_Metodo = W_CmbPcto:ADD-LAST(W_PctoStr).    

        FOR EACH Pro_Ahorros WHERE Pro_Ahorros.Estado  EQ 1 
                             NO-LOCK
                             BREAK BY Pro_ahorros.cod_ahorro:
            IF FIRST-OF(Pro_ahorros.cod_ahorro) THEN DO:
               W_CmbPcto:ADD-LAST(STRING(Pro_ahorros.cod_ahorro,"999") + " " + Pro_Ahorros.Nom_Producto).
            END.
        END.
     END.
     ELSE DO:
       ASSIGN W_PctoStr = "000 CONSOLIDADO"
              W_CmbPcto:LIST-ITEMS = ""
              W_Metodo = W_CmbPcto:ADD-LAST(W_PctoStr).    

       FOR EACH Pro_Creditos WHERE Pro_Creditos.Estado EQ 1 NO-LOCK
                             BREAK BY Pro_creditos.cod_credito:
           IF FIRST-OF(Pro_creditos.cod_credito) THEN DO:
              W_CmbPcto:ADD-LAST(STRING(Pro_creditos.cod_credito,"999") + " " + Pro_Creditos.Nom_Producto).
           END.
       END.
     END.
     ASSIGN W_CmbPcto:SCREEN-VALUE = W_CmbPcto:ENTRY(1).  
  END.   
  RUN Inicia_Campos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CmbOfi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbOfi W-ProCre
ON VALUE-CHANGED OF W_CmbOfi IN FRAME F-Main /* Agencia */
DO:
  ASSIGN W_OfiTbajo = INTEGER(SUBSTRING(W_CmbOfi:SCREEN-VALUE IN FRAME {&FRAME-NAME},1,3)) NO-ERROR.
         W_agenciaP = STRING(W_OfiTbajo).

  IF W_OfiTbajo EQ 0 THEN DO:
     FIND FIRST agencias WHERE agencias.Estado NE 2 
                         NO-LOCK NO-ERROR.
     IF AVAILABLE agencias THEN 
        ASSIGN W_OfiIni = agencias.agencia.

     FIND LAST agencias WHERE agencias.Estado  NE 2 
                        NO-LOCK NO-ERROR.
     IF AVAILABLE agencias THEN 
        ASSIGN W_OfiFin = agencias.agencia.
  END.
  ELSE ASSIGN W_OfiIni = W_OfiTbajo
              W_OfiFin = W_OfiTbajo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME w_cmbpcto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w_cmbpcto W-ProCre
ON VALUE-CHANGED OF w_cmbpcto IN FRAME F-Main
DO:
  ASSIGN W_ProIni = INTEGER(SUBSTRING(W_CmbPcto:SCREEN-VALUE IN FRAME {&FRAME-NAME},1,3)).
  IF W_ProIni GT 0 THEN
     ASSIGN W_ProFin = W_ProIni.
  ELSE DO:   
    IF R_Opcion EQ 1 THEN DO:
       IF W_OfiIni EQ 0 THEN DO:
          FIND agencias WHERE agencias.Estado      EQ 1
                          AND agencias.Tip_Agencia EQ "C"
                        NO-LOCK NO-ERROR.
          IF AVAILABLE agencias THEN DO:
             ASSIGN W_WkOfi = agencias.agencia.
             FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Estado  EQ 1 
                                    NO-LOCK NO-ERROR.
             IF AVAILABLE Pro_Ahorros THEN 
                ASSIGN W_ProIni = Pro_ahorros.cod_ahorro.       
 
             FIND LAST Pro_Ahorros WHERE Pro_Ahorros.Estado  EQ 1 
                                   NO-LOCK NO-ERROR.
             IF AVAILABLE Pro_Ahorros THEN 
                ASSIGN W_ProFin = Pro_ahorros.cod_ahorro.
          END.       
       END.
       FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Estado  EQ 1 
                              NO-LOCK NO-ERROR.
       IF AVAILABLE Pro_Ahorros THEN 
          ASSIGN W_ProIni = Pro_ahorros.cod_ahorro.       
 
       FIND LAST Pro_Ahorros WHERE Pro_Ahorros.Estado  EQ 1 
                             NO-LOCK NO-ERROR.
       IF AVAILABLE Pro_Ahorros THEN 
          ASSIGN W_ProFin = Pro_ahorros.cod_ahorro.
    END.
    ELSE DO:
      IF W_OfiIni EQ 0 THEN DO:
         FIND agencias WHERE agencias.Estado      EQ 1
                         AND agencias.Tip_Agencia EQ "C"
                       NO-LOCK NO-ERROR.
         IF AVAILABLE agencias THEN DO:
            FIND FIRST Pro_Creditos WHERE Pro_Creditos.Estado  EQ 1 
                                   NO-LOCK NO-ERROR.
            IF AVAILABLE Pro_Creditos THEN 
               ASSIGN W_ProIni = Pro_creditos.cod_credito.       

            FIND LAST Pro_Creditos WHERE Pro_Creditos.Estado  EQ 1 
                                  NO-LOCK NO-ERROR.
            IF AVAILABLE Pro_Ahorros THEN 
               ASSIGN W_ProFin = Pro_creditos.cod_credito.
         END.       
      END.
      FIND FIRST Pro_Creditos WHERE Pro_Creditos.Estado  EQ 1 
                              NO-LOCK NO-ERROR.
      IF AVAILABLE Pro_Creditos THEN 
         ASSIGN W_ProIni = Pro_creditos.cod_credito.

      FIND LAST Pro_Creditos WHERE Pro_Creditos.Estado  EQ 1 
                             NO-LOCK NO-ERROR.
      IF AVAILABLE Pro_Creditos THEN 
         ASSIGN W_ProFin = Pro_creditos.cod_credito.
    END.   
  END.  
  RUN Inicia_Campos.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Rango10f
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Rango10f W-ProCre
ON LEAVE OF W_Rango10f IN FRAME F-Main
DO:
  ASSIGN W_Rango10f
         W_WkFin = W_Rango10f.
         
  IF R_Opcion EQ 1 THEN
     ASSIGN W_CapCreFin = 0
            W_DisAhoFin = W_Rango10f.
  ELSE ASSIGN W_DisAhoFin = 0
              W_CapCreFin = W_Rango10f.   
         
  RUN Valida_Rango9.
  IF W_Rpta THEN DO:
     APPLY "ENTRY" TO W_Rango10I.     
     RETURN NO-APPLY.
  END.   
  RUN Calculo.
  IF R_Opcion EQ 1 THEN
     ASSIGN Valor10:SCREEN-VALUE IN FRAME {&frame-name}   = STRING(W_SaldoAho).
  ELSE ASSIGN Valor10:SCREEN-VALUE IN FRAME {&frame-name} = STRING(W_CapitalCre).   
  
  ASSIGN Ocur10:SCREEN-VALUE      IN FRAME {&frame-name} = STRING(W_Cont)
         tp10:SCREEN-VALUE       IN FRAME {&frame-name} = STRING(tpv / DECIMAL(Valor10:SCREEN-VALUE)).
  RUN Totales.
  ASSIGN W_TotValor:SCREEN-VALUE IN FRAME {&frame-name} = STRING(W_TotGen)
         TTTpv:SCREEN-VALUE = STRING(TTpv / W_TotGen)
         W_TotOcur:SCREEN-VALUE  IN FRAME {&frame-name} = STRING(W_TotCont).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME w_Rango10i
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w_Rango10i W-ProCre
ON LEAVE OF w_Rango10i IN FRAME F-Main /* 10.) */
DO:
  ASSIGN W_Rango10i
         W_WkIni = W_Rango10i.

  IF R_Opcion EQ 1 THEN
     ASSIGN W_CapCreIni = 0
            W_DisAhoIni = W_Rango10i.
  ELSE ASSIGN W_DisAhoIni = 0
              W_CapCreIni = W_Rango10i.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME w_Rango1f
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w_Rango1f W-ProCre
ON LEAVE OF w_Rango1f IN FRAME F-Main
DO:
  ASSIGN W_Rango1f.

  IF R_Opcion EQ 1 THEN
     ASSIGN W_CapCreFin = 0
            W_DisAhoFin = W_Rango1f.
  ELSE ASSIGN W_DisAhoFin = 0
              W_CapCreFin = W_Rango1f.   
         
  IF W_Rango1I GT W_Rango1f THEN DO:
    RUN MostrarMensaje IN W_Manija (INPUT 068,OUTPUT W_eleccion).
    APPLY "ENTRY" TO W_Rango1I.     
    RETURN NO-APPLY.
  END.
  RUN Calculo.
  IF R_Opcion EQ 1 THEN
     ASSIGN Valor1:SCREEN-VALUE IN FRAME {&frame-name}   = STRING(W_SaldoAho).
  ELSE ASSIGN Valor1:SCREEN-VALUE IN FRAME {&frame-name} = STRING(W_CapitalCre).   
  
  ASSIGN Ocur1:SCREEN-VALUE      IN FRAME {&frame-name} = STRING(W_Cont)
         tp1:SCREEN-VALUE = STRING(tpv / DECIMAL(Valor1:SCREEN-VALUE)).
         
         
  RUN Totales.
  ASSIGN W_TotValor:SCREEN-VALUE IN FRAME {&frame-name} = STRING(W_TotGen)
         TTTpv:SCREEN-VALUE = STRING(TTpv / W_TotGen)
         W_TotOcur:SCREEN-VALUE  IN FRAME {&frame-name} = STRING(W_TotCont).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Rango1i
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Rango1i W-ProCre
ON LEAVE OF W_Rango1i IN FRAME F-Main /* 1.) */
DO:
  ASSIGN W_Rango1i Ttpv = 0.
  IF R_Opcion EQ 1 THEN
     ASSIGN W_CapCreIni = 0
            W_DisAhoIni = W_Rango1i.
  ELSE ASSIGN W_DisAhoIni = 0
              W_CapCreIni = W_Rango1i.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Rango2f
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Rango2f W-ProCre
ON LEAVE OF W_Rango2f IN FRAME F-Main
DO:
  ASSIGN W_Rango2f
         W_WkFin = W_Rango2f.
         
  IF R_Opcion EQ 1 THEN
     ASSIGN W_CapCreFin = 0
            W_DisAhoFin = W_Rango2f.
  ELSE ASSIGN W_DisAhoFin = 0
              W_CapCreFin = W_Rango2f.   
         
  RUN Valida_Rango1.
  IF W_Rpta THEN DO:
     APPLY "ENTRY" TO W_Rango2I.     
     RETURN NO-APPLY.
  END.   
  RUN Calculo.
  IF R_Opcion EQ 1 THEN
     ASSIGN Valor2:SCREEN-VALUE IN FRAME {&frame-name}   = STRING(W_SaldoAho).
  ELSE ASSIGN Valor2:SCREEN-VALUE IN FRAME {&frame-name} = STRING(W_CapitalCre).   
  
  ASSIGN Ocur2:SCREEN-VALUE      IN FRAME {&frame-name} = STRING(W_Cont)
         tp2:SCREEN-VALUE       IN FRAME {&frame-name} = STRING(tpv / DECIMAL(Valor2:SCREEN-VALUE)).
  RUN Totales.
  ASSIGN W_TotValor:SCREEN-VALUE IN FRAME {&frame-name} = STRING(W_TotGen)
         TTTpv:SCREEN-VALUE = STRING(TTpv / W_TotGen)
         W_TotOcur:SCREEN-VALUE  IN FRAME {&frame-name} = STRING(W_TotCont).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Rango2i
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Rango2i W-ProCre
ON LEAVE OF W_Rango2i IN FRAME F-Main /* 2.) */
DO:
  ASSIGN W_Rango2i
         W_WkIni = W_Rango2i.
         
  IF R_Opcion EQ 1 THEN
     ASSIGN W_CapCreIni = 0
            W_DisAhoIni = W_Rango2i.
  ELSE ASSIGN W_DisAhoIni = 0
              W_CapCreIni = W_Rango2i.       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Rango3f
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Rango3f W-ProCre
ON LEAVE OF W_Rango3f IN FRAME F-Main
DO:
  ASSIGN W_Rango3f
         W_WkFin = W_Rango3f.
         
  IF R_Opcion EQ 1 THEN
     ASSIGN W_CapCreFin = 0
            W_DisAhoFin = W_Rango3f.
  ELSE ASSIGN W_DisAhoFin = 0
              W_CapCreFin = W_Rango3f.   
         
  RUN Valida_Rango2.
  IF W_Rpta THEN DO:
    APPLY "ENTRY" TO W_Rango3I.     
    RETURN NO-APPLY.
  END.  
  RUN Calculo.
  IF R_Opcion EQ 1 THEN
     ASSIGN Valor3:SCREEN-VALUE IN FRAME {&frame-name}   = STRING(W_SaldoAho).
  ELSE ASSIGN Valor3:SCREEN-VALUE IN FRAME {&frame-name} = STRING(W_CapitalCre).   
  
  ASSIGN Ocur3:SCREEN-VALUE      IN FRAME {&frame-name} = STRING(W_Cont)
         tp3:SCREEN-VALUE       IN FRAME {&frame-name} = STRING(tpv / DECIMAL(Valor3:SCREEN-VALUE)).
  RUN Totales.
  ASSIGN W_TotValor:SCREEN-VALUE IN FRAME {&frame-name} = STRING(W_TotGen)
         TTTpv:SCREEN-VALUE = STRING(TTpv / W_TotGen)
         W_TotOcur:SCREEN-VALUE  IN FRAME {&frame-name} = STRING(W_TotCont).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Rango3i
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Rango3i W-ProCre
ON LEAVE OF W_Rango3i IN FRAME F-Main /* 3.) */
DO:
  ASSIGN W_Rango3i
         W_WkIni = W_Rango3i.
         
  IF R_Opcion EQ 1 THEN
     ASSIGN W_CapCreIni = 0
            W_DisAhoIni = W_Rango3i.
  ELSE ASSIGN W_DisAhoIni = 0
              W_CapCreIni = W_Rango3i.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Rango4f
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Rango4f W-ProCre
ON LEAVE OF W_Rango4f IN FRAME F-Main
DO:
  ASSIGN W_Rango4f
         W_WkFin = W_Rango4f.
  IF R_Opcion EQ 1 THEN
     ASSIGN W_CapCreFin = 0
            W_DisAhoFin = W_Rango4f.
  ELSE ASSIGN W_DisAhoFin = 0
              W_CapCreFin = W_Rango4f.   
         
  RUN Valida_Rango3.
  IF W_Rpta THEN DO:
     APPLY "ENTRY" TO W_Rango4I.     
     RETURN NO-APPLY.
  END.   
  RUN Calculo.
  IF R_Opcion EQ 1 THEN
     ASSIGN Valor4:SCREEN-VALUE IN FRAME {&frame-name}   = STRING(W_SaldoAho).
  ELSE ASSIGN Valor4:SCREEN-VALUE IN FRAME {&frame-name} = STRING(W_CapitalCre).   
  
  ASSIGN Ocur4:SCREEN-VALUE      IN FRAME {&frame-name} = STRING(W_Cont)
         tp4:SCREEN-VALUE       IN FRAME {&frame-name} = STRING(tpv / DECIMAL(Valor4:SCREEN-VALUE)).
  RUN Totales.
  ASSIGN W_TotValor:SCREEN-VALUE IN FRAME {&frame-name} = STRING(W_TotGen)
         TTTpv:SCREEN-VALUE = STRING(TTpv / W_TotGen)
         W_TotOcur:SCREEN-VALUE  IN FRAME {&frame-name} = STRING(W_TotCont).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Rango4i
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Rango4i W-ProCre
ON LEAVE OF W_Rango4i IN FRAME F-Main /* 4.) */
DO:
  ASSIGN W_Rango4i
         W_WkIni = W_Rango4i.
  
  IF R_Opcion EQ 1 THEN
     ASSIGN W_CapCreIni = 0
            W_DisAhoIni = W_Rango4i.
  ELSE ASSIGN W_DisAhoIni = 0
              W_CapCreIni = W_Rango4i.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Rango5f
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Rango5f W-ProCre
ON LEAVE OF W_Rango5f IN FRAME F-Main
DO:
  ASSIGN W_Rango5f
         W_WkFin = W_Rango5f.
         
  IF R_Opcion EQ 1 THEN
     ASSIGN W_CapCreFin = 0
            W_DisAhoFin = W_Rango5f.
  ELSE ASSIGN W_DisAhoFin = 0
              W_CapCreFin = W_Rango5f.   
         
  RUN Valida_Rango4.
  IF W_Rpta THEN DO:
     APPLY "ENTRY" TO W_Rango5I.     
     RETURN NO-APPLY.
  END.   
  RUN Calculo.
  IF R_Opcion EQ 1 THEN
     ASSIGN Valor5:SCREEN-VALUE IN FRAME {&frame-name}   = STRING(W_SaldoAho).
  ELSE ASSIGN Valor5:SCREEN-VALUE IN FRAME {&frame-name} = STRING(W_CapitalCre).   
  
  ASSIGN Ocur5:SCREEN-VALUE      IN FRAME {&frame-name} = STRING(W_Cont)
         tp5:SCREEN-VALUE       IN FRAME {&frame-name} = STRING(tpv / DECIMAL(Valor5:SCREEN-VALUE)).
  RUN Totales.
  ASSIGN W_TotValor:SCREEN-VALUE IN FRAME {&frame-name} = STRING(W_TotGen)
         TTTpv:SCREEN-VALUE = STRING(TTpv / W_TotGen)
         W_TotOcur:SCREEN-VALUE  IN FRAME {&frame-name} = STRING(W_TotCont).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Rango5i
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Rango5i W-ProCre
ON LEAVE OF W_Rango5i IN FRAME F-Main /* 5.) */
DO:
  ASSIGN W_Rango5i
         W_WkIni = W_Rango5i.

  IF R_Opcion EQ 1 THEN
     ASSIGN W_CapCreIni = 0
            W_DisAhoIni = W_Rango5i.
  ELSE ASSIGN W_DisAhoIni = 0
              W_CapCreIni = W_Rango5i.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Rango6f
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Rango6f W-ProCre
ON LEAVE OF W_Rango6f IN FRAME F-Main
DO:
  ASSIGN W_Rango6f
         W_WkFin = W_Rango6f.  
         
  IF R_Opcion EQ 1 THEN
     ASSIGN W_CapCreFin = 0
            W_DisAhoFin = W_Rango6f.
  ELSE ASSIGN W_DisAhoFin = 0
              W_CapCreFin = W_Rango6f.   
         
  RUN Valida_Rango5.
  IF W_Rpta THEN DO:
     APPLY "ENTRY" TO W_Rango6I.     
     RETURN NO-APPLY.
  END.   
  RUN Calculo.
  IF R_Opcion EQ 1 THEN
     ASSIGN Valor6:SCREEN-VALUE IN FRAME {&frame-name}   = STRING(W_SaldoAho).
  ELSE ASSIGN Valor6:SCREEN-VALUE IN FRAME {&frame-name} = STRING(W_CapitalCre).   
  
  ASSIGN Ocur6:SCREEN-VALUE      IN FRAME {&frame-name} = STRING(W_Cont)
         tp6:SCREEN-VALUE       IN FRAME {&frame-name} = STRING(tpv / DECIMAL(Valor6:SCREEN-VALUE)).
  RUN Totales.
  ASSIGN W_TotValor:SCREEN-VALUE IN FRAME {&frame-name} = STRING(W_TotGen)
         TTTpv:SCREEN-VALUE = STRING(TTpv / W_TotGen)
         W_TotOcur:SCREEN-VALUE  IN FRAME {&frame-name} = STRING(W_TotCont).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Rango6i
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Rango6i W-ProCre
ON LEAVE OF W_Rango6i IN FRAME F-Main /* 6.) */
DO:
  ASSIGN W_Rango6i
         W_WkIni = W_Rango6i.

  IF R_Opcion EQ 1 THEN
     ASSIGN W_CapCreIni = 0
            W_DisAhoIni = W_Rango6i.
  ELSE ASSIGN W_DisAhoIni = 0
              W_CapCreIni = W_Rango6i.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Rango7f
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Rango7f W-ProCre
ON LEAVE OF W_Rango7f IN FRAME F-Main
DO:
  ASSIGN W_Rango7f
         W_WkFin = W_Rango7f.
         
  IF R_Opcion EQ 1 THEN
     ASSIGN W_CapCreFin = 0
            W_DisAhoFin = W_Rango7f.
  ELSE ASSIGN W_DisAhoFin = 0
              W_CapCreFin = W_Rango7f.   
         
  RUN Valida_Rango6.
  IF W_Rpta THEN DO:
     APPLY "ENTRY" TO W_Rango7I.     
     RETURN NO-APPLY.
  END.   
  RUN Calculo.
  IF R_Opcion EQ 1 THEN
     ASSIGN Valor7:SCREEN-VALUE IN FRAME {&frame-name}   = STRING(W_SaldoAho).
  ELSE ASSIGN Valor7:SCREEN-VALUE IN FRAME {&frame-name} = STRING(W_CapitalCre).   
  
  ASSIGN Ocur7:SCREEN-VALUE      IN FRAME {&frame-name} = STRING(W_Cont)
         tp7:SCREEN-VALUE       IN FRAME {&frame-name} = STRING(tpv / DECIMAL(Valor7:SCREEN-VALUE)).
  RUN Totales.
  ASSIGN W_TotValor:SCREEN-VALUE IN FRAME {&frame-name} = STRING(W_TotGen)
         TTTpv:SCREEN-VALUE = STRING(TTpv / W_TotGen)
         W_TotOcur:SCREEN-VALUE  IN FRAME {&frame-name} = STRING(W_TotCont).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME w_Rango7i
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w_Rango7i W-ProCre
ON LEAVE OF w_Rango7i IN FRAME F-Main /* 7.) */
DO:
  ASSIGN W_Rango7i
         W_WkIni = W_Rango7i.

  IF R_Opcion EQ 1 THEN
     ASSIGN W_CapCreIni = 0
            W_DisAhoIni = W_Rango7i.
  ELSE ASSIGN W_DisAhoIni = 0
              W_CapCreIni = W_Rango7i.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Rango8f
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Rango8f W-ProCre
ON LEAVE OF W_Rango8f IN FRAME F-Main
DO:
  ASSIGN W_Rango8f
         W_WkFin = W_Rango8f.
         
  IF R_Opcion EQ 1 THEN
     ASSIGN W_CapCreFin = 0
            W_DisAhoFin = W_Rango8f.
  ELSE ASSIGN W_DisAhoFin = 0
              W_CapCreFin = W_Rango8f.   
         
  RUN Valida_Rango7.
  IF W_Rpta THEN DO:
     APPLY "ENTRY" TO W_Rango8I.     
     RETURN NO-APPLY.
  END.   
  RUN Calculo.
  IF R_Opcion EQ 1 THEN
     ASSIGN Valor8:SCREEN-VALUE IN FRAME {&frame-name}   = STRING(W_SaldoAho).
  ELSE ASSIGN Valor8:SCREEN-VALUE IN FRAME {&frame-name} = STRING(W_CapitalCre).   
  
  ASSIGN Ocur8:SCREEN-VALUE      IN FRAME {&frame-name} = STRING(W_Cont)
         tp8:SCREEN-VALUE       IN FRAME {&frame-name} = STRING(tpv / DECIMAL(Valor8:SCREEN-VALUE)).
  RUN Totales.
  ASSIGN W_TotValor:SCREEN-VALUE IN FRAME {&frame-name} = STRING(W_TotGen)
         TTTpv:SCREEN-VALUE = STRING(TTpv / W_TotGen)
         W_TotOcur:SCREEN-VALUE  IN FRAME {&frame-name} = STRING(W_TotCont).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Rango8i
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Rango8i W-ProCre
ON LEAVE OF W_Rango8i IN FRAME F-Main /* 8.) */
DO:
  ASSIGN W_Rango8i
         W_WkIni = W_Rango8i.

  IF R_Opcion EQ 1 THEN
     ASSIGN W_CapCreIni = 0
            W_DisAhoIni = W_Rango8i.
  ELSE ASSIGN W_DisAhoIni = 0
              W_CapCreIni = W_Rango8i.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME w_Rango9f
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w_Rango9f W-ProCre
ON LEAVE OF w_Rango9f IN FRAME F-Main
DO:
  ASSIGN W_Rango9f
         W_Wkfin = W_Rango9f.
         
  IF R_Opcion EQ 1 THEN
     ASSIGN W_CapCreFin = 0
            W_DisAhoFin = W_Rango9f.
  ELSE ASSIGN W_DisAhoFin = 0
              W_CapCreFin = W_Rango9f.   
         
  RUN Valida_Rango8.
  IF W_Rpta THEN DO:
     APPLY "ENTRY" TO W_Rango9I.     
     RETURN NO-APPLY.
  END.   
  RUN Calculo.
  IF R_Opcion EQ 1 THEN
     ASSIGN Valor9:SCREEN-VALUE IN FRAME {&frame-name}   = STRING(W_SaldoAho).
  ELSE ASSIGN Valor9:SCREEN-VALUE IN FRAME {&frame-name} = STRING(W_CapitalCre).   
  
  ASSIGN Ocur9:SCREEN-VALUE      IN FRAME {&frame-name} = STRING(W_Cont)
         tp9:SCREEN-VALUE       IN FRAME {&frame-name} = STRING(tpv / DECIMAL(Valor9:SCREEN-VALUE)).
  RUN Totales.
  ASSIGN W_TotValor:SCREEN-VALUE IN FRAME {&frame-name} = STRING(W_TotGen)
         TTTpv:SCREEN-VALUE = STRING(TTpv / W_TotGen)
         W_TotOcur:SCREEN-VALUE  IN FRAME {&frame-name} = STRING(W_TotCont).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Rango9i
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Rango9i W-ProCre
ON LEAVE OF W_Rango9i IN FRAME F-Main /* 9.) */
DO:
  ASSIGN W_Rango9i
         W_WkIni = W_Rango9i.

  IF R_Opcion EQ 1 THEN
     ASSIGN W_CapCreIni = 0
            W_DisAhoIni = W_Rango9i.
  ELSE ASSIGN W_DisAhoIni = 0
              W_CapCreIni = W_Rango9i.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Asignar_Campos W-ProCre 
PROCEDURE Asignar_Campos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN Valor1  = DECIMAL(Valor1:SCREEN-VALUE) 
           Ocur1   = DECIMAL(Ocur1:SCREEN-VALUE)
            Valor2  = DECIMAL(Valor2:SCREEN-VALUE)
           Ocur2   = DECIMAL(Ocur2:SCREEN-VALUE)
           Valor3  = DECIMAL(Valor3:SCREEN-VALUE)
           Ocur3   = DECIMAL(Ocur3:SCREEN-VALUE)
           Valor4  = DECIMAL(Valor4:SCREEN-VALUE)
           Ocur4   = DECIMAL(Ocur4:SCREEN-VALUE)
           Valor5  = DECIMAL(Valor5:SCREEN-VALUE)
           Ocur5   = DECIMAL(Ocur5:SCREEN-VALUE)
           Valor6  = DECIMAL(Valor6:SCREEN-VALUE)
           Ocur6   = DECIMAL(Ocur6:SCREEN-VALUE)
           Valor7  = DECIMAL(Valor7:SCREEN-VALUE)
           Ocur7   = DECIMAL(Ocur7:SCREEN-VALUE)
           Valor8  = DECIMAL(Valor8:SCREEN-VALUE)
           Ocur8   = DECIMAL(Ocur8:SCREEN-VALUE)
           Valor9  = DECIMAL(Valor9:SCREEN-VALUE)
           Ocur9   = DECIMAL(Ocur9:SCREEN-VALUE)
           Valor10 = DECIMAL(Valor10:SCREEN-VALUE)
           Ocur10  = DECIMAL(Ocur10:SCREEN-VALUE).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Calculo W-ProCre 
PROCEDURE Calculo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN W_SaldoAho   = 0
         W_CapitalCre = 0
         W_Cont       = 0
         tpv = 0.
  IF R_Opcion EQ 1 THEN DO:
     FOR EACH Ahorros FIELDS(Sdo_Disponible Tasa)
                      WHERE Ahorros.agencia        GE W_OfiIni
                        AND Ahorros.agencia        LE W_OfiFin 
                        AND Ahorros.Estado         NE 2 
                        AND ahorros.cod_ahorro   GE W_ProIni 
                        AND ahorros.cod_ahorro   LE W_ProFin 
                        AND Ahorros.Sdo_Disponible GE W_DisAhoIni
                        AND Ahorros.Sdo_Disponible LE W_DisAhoFin
                      NO-LOCK:
         ASSIGN W_SaldoAho = W_SaldoAho + Ahorros.Sdo_Disponible
                W_Cont     = W_Cont     + 1
                tpv        = Ahorros.Sdo_Disponible * Ahorros.Tasa
                ttpv       = ttpv + tpv.
     END.
  END.  
  ELSE DO:
    FOR EACH Creditos FIELDS(Sdo_Capital tasa)
                      WHERE Creditos.agencia      GE W_OfiIni
                        AND Creditos.agencia      LE W_OfiFin
                        AND Creditos.Estado       NE 2 
                        AND creditos.cod_credito GE W_ProIni 
                        AND creditos.cod_credito LE W_ProFin
                        AND Creditos.Sdo_Capital  GE W_CapCreIni
                        AND Creditos.Sdo_Capital  LE W_CapCreFin                         
                      NO-LOCK:
        ASSIGN W_CapitalCre = W_CapitalCre + Creditos.Sdo_Capital
               W_Cont       = W_Cont     + 1
                tpv        = Creditos.Sdo_Capital * Creditos.Tasa
                ttpv       = ttpv + tpv.
    END.           
  END.
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
  DISPLAY W_CmbOfi R_opcion w_cmbpcto FVI FINC W_Rango1i w_Rango1f Valor1 ocur1 
          TP1 W_Rango2i W_Rango2f valor2 ocur2 tp2 W_Rango3i W_Rango3f valor3 
          ocur3 tp3 W_Rango4i W_Rango4f valor4 ocur4 tp4 W_Rango5i W_Rango5f 
          valor5 ocur5 tp5 W_Rango6i W_Rango6f valor6 ocur6 tp6 w_Rango7i 
          W_Rango7f valor7 ocur7 tp7 W_Rango8i W_Rango8f valor8 ocur8 tp8 
          W_Rango9i w_Rango9f valor9 ocur9 tp9 w_Rango10i W_Rango10f valor10 
          ocur10 tp10 W_Totvalor W_TotOcur TTTpv 
      WITH FRAME F-Main IN WINDOW W-ProCre.
  ENABLE W_CmbOfi R_opcion w_cmbpcto BUTTON-1 FVI FINC Btn_Consulta 
         Btn_Imprimir W_Rango1i w_Rango1f W_Rango2i W_Rango2f W_Rango3i 
         W_Rango3f W_Rango4i W_Rango4f W_Rango5i W_Rango5f W_Rango6i W_Rango6f 
         w_Rango7i W_Rango7f W_Rango8i W_Rango8f Btn_Cancelar W_Rango9i 
         w_Rango9f w_Rango10i W_Rango10f Btn_Ayuda RECT-23 RECT-24 RECT-5 
      WITH FRAME F-Main IN WINDOW W-ProCre.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-ProCre.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir_excel W-ProCre 
PROCEDURE Imprimir_excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  /* 
 * Este ejemplo estracta datos desde una base de datos progress
 * y grafica en excell utilizando la automatización ole.
 */

DEFINE VARIABLE chExcelApplication      AS COM-HANDLE.
DEFINE VARIABLE chWorkbook              AS COM-HANDLE.
DEFINE VARIABLE chWorksheet             AS COM-HANDLE.
DEFINE VARIABLE chChart                 AS COM-HANDLE.
DEFINE VARIABLE chWorksheetRange        AS COM-HANDLE.
DEFINE VARIABLE iCount                  AS INTEGER.
DEFINE VARIABLE iIndex                  AS INTEGER.
DEFINE VARIABLE iTotalNumberOfOrders    AS INTEGER.
DEFINE VARIABLE iMonth                  AS INTEGER.
DEFINE VARIABLE dAnnualQuota            AS DECIMAL.
DEFINE VARIABLE dTotalSalesAmount       AS DECIMAL.
DEFINE VARIABLE iColumn                 AS INTEGER INITIAL 1.
DEFINE VARIABLE cColumn                 AS CHARACTER.
DEFINE VARIABLE cRange                  AS CHARACTER.

 
/* create a new Excel Application object */
CREATE "Excel.Application" chExcelApplication.

/* launch Excel so it is visible to the user */
chExcelApplication:Visible = TRUE.

/* create a new Workbook */
chWorkbook = chExcelApplication:Workbooks:Add().

/* get the active Worksheet */
chWorkSheet = chExcelApplication:Sheets:Item(1).

/* set the column names for the Worksheet */
chWorkSheet:Columns("A"):ColumnWidth = 18.
chWorkSheet:Columns("B"):ColumnWidth = 18.
chWorkSheet:Columns("C"):ColumnWidth = 12.
chWorkSheet:Columns("D"):ColumnWidth = 12.


chWorkSheet:Range("A1:D1"):Font:Bold = TRUE.
chWorkSheet:Range("A1"):Value = "DE".
chWorkSheet:Range("B1"):Value = "HASTA".
chWorkSheet:Range("C1"):Value = "Valores".
chWorkSheet:Range("D1"):Value = "Ocurrencias".

/* Llena hoja de calculo*/

chWorkSheet:Range("A2"):Value = W_Rango1I.
chWorkSheet:Range("B2"):Value = W_Rango1F.
chWorkSheet:Range("C2"):Value =  Valor1:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
chWorkSheet:Range("D2"):Value = Ocur1:SCREEN-VALUE.

chWorkSheet:Range("A3"):Value = W_Rango2I.
chWorkSheet:Range("B3"):Value = W_Rango2F.
chWorkSheet:Range("C3"):Value = Valor2:SCREEN-VALUE.
chWorkSheet:Range("D3"):Value = Ocur2:SCREEN-VALUE.

chWorkSheet:Range("A4"):Value = W_Rango3I.
chWorkSheet:Range("B4"):Value = W_Rango3F.
chWorkSheet:Range("C4"):Value = Valor3:SCREEN-VALUE.
chWorkSheet:Range("D4"):Value = Ocur3:SCREEN-VALUE.

chWorkSheet:Range("A5"):Value = W_Rango4I.
chWorkSheet:Range("B5"):Value = W_Rango4F.
chWorkSheet:Range("C5"):Value = Valor4:SCREEN-VALUE.
chWorkSheet:Range("D5"):Value = Ocur4:SCREEN-VALUE.

chWorkSheet:Range("A6"):Value = W_Rango5I.
chWorkSheet:Range("B6"):Value = W_Rango5F.
chWorkSheet:Range("C6"):Value = Valor5:SCREEN-VALUE.
chWorkSheet:Range("D6"):Value = Ocur5:SCREEN-VALUE.

chWorkSheet:Range("A7"):Value = W_Rango6I.
chWorkSheet:Range("B7"):Value = W_Rango6F.
chWorkSheet:Range("C7"):Value = Valor6:SCREEN-VALUE.
chWorkSheet:Range("D7"):Value = Ocur6:SCREEN-VALUE.

chWorkSheet:Range("A8"):Value = W_Rango7I.
chWorkSheet:Range("B8"):Value = W_Rango7F.
chWorkSheet:Range("C8"):Value = Valor7:SCREEN-VALUE.
chWorkSheet:Range("D8"):Value = Ocur7:SCREEN-VALUE.

chWorkSheet:Range("A9"):Value = W_Rango8I.
chWorkSheet:Range("B9"):Value = W_Rango8F.
chWorkSheet:Range("C9"):Value = Valor8:SCREEN-VALUE.
chWorkSheet:Range("D9"):Value = Ocur8:SCREEN-VALUE.

chWorkSheet:Range("A10"):Value = W_Rango9I.
chWorkSheet:Range("B10"):Value = W_Rango9F.
chWorkSheet:Range("C10"):Value = Valor9:SCREEN-VALUE.
chWorkSheet:Range("D10"):Value = Ocur9:SCREEN-VALUE.

chWorkSheet:Range("A11"):Value = W_Rango10I.
chWorkSheet:Range("B11"):Value = W_Rango10F.
chWorkSheet:Range("C11"):Value = Valor10:SCREEN-VALUE.
chWorkSheet:Range("D11"):Value = Ocur10:SCREEN-VALUE.


chWorkSheet:Range("B2:D11"):Select().
/*chExcelApplication:Selection:Style = "Currency".*/

/* create embedded chart using the data in the Worksheet */
chWorksheetRange = chWorksheet:Range("B1:C11").
chWorksheet:ChartObjects:Add(10,150,425,300):Activate.
chExcelApplication:ActiveChart:ChartWizard(chWorksheetRange, 3, 1, 2, 1, 1, TRUE,
    "RANGO DE SALDOS   ", "VALORES     ", "OCURRENCIAS ").

/* create chart using the data in the Worksheet */
chChart=chExcelApplication:Charts:Add().
chChart:Name = "Rango de saldos ".
chChart:Type = 11.

/* release com-handles */
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet.
RELEASE OBJECT chChart.
RELEASE OBJECT chWorksheetRange. 


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicia_Campos W-ProCre 
PROCEDURE Inicia_Campos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN Valor1:SCREEN-VALUE     = "0"
           Ocur1:SCREEN-VALUE      = "0"
           Valor2:SCREEN-VALUE     = "0"   
           Ocur2:SCREEN-VALUE      = "0"                 
           Valor3:SCREEN-VALUE     = "0"     
           Ocur3:SCREEN-VALUE      = "0"      
           Valor4:SCREEN-VALUE     = "0"     
           Ocur4:SCREEN-VALUE      = "0"
           Valor5:SCREEN-VALUE     = "0"    
           Ocur5:SCREEN-VALUE      = "0"      
           Valor6:SCREEN-VALUE     = "0"     
           Ocur6:SCREEN-VALUE      = "0"      
           Valor7:SCREEN-VALUE     = "0"     
           Ocur7:SCREEN-VALUE      = "0"      
           Valor8:SCREEN-VALUE     = "0"     
           Ocur8:SCREEN-VALUE      = "0"      
           Valor9:SCREEN-VALUE     = "0"     
           Ocur9:SCREEN-VALUE      = "0"      
           Valor10:SCREEN-VALUE    = "0"    
           Ocur10:SCREEN-VALUE     = "0"
           W_Rango1i:SCREEN-VALUE = STRING(FVI)
           W_Rango1f:SCREEN-VALUE  = STRING(FVI + FINC)     
           W_Rango2i:SCREEN-VALUE  = STRING(DECIMAL(W_Rango1f:SCREEN-VALUE) + 1)     
           W_Rango2f:SCREEN-VALUE  = STRING(DECIMAL(W_Rango2i:SCREEN-VALUE) + FInc - 1)     
           W_Rango3i:SCREEN-VALUE  = STRING(DECIMAL(W_Rango2f:SCREEN-VALUE) + 1)       
           W_Rango3f:SCREEN-VALUE  = STRING(DECIMAL(W_Rango3i:SCREEN-VALUE) + FInc - 1)    
           W_Rango4i:SCREEN-VALUE  = STRING(DECIMAL(W_Rango3f:SCREEN-VALUE) + 1)        
           W_Rango4f:SCREEN-VALUE  = STRING(DECIMAL(W_Rango4i:SCREEN-VALUE) + FInc - 1)     
           W_Rango5i:SCREEN-VALUE  = STRING(DECIMAL(W_Rango4f:SCREEN-VALUE) + 1)        
           W_Rango5f:SCREEN-VALUE  = STRING(DECIMAL(W_Rango5i:SCREEN-VALUE) + FInc - 1)     
           W_Rango6i:SCREEN-VALUE  = STRING(DECIMAL(W_Rango5f:SCREEN-VALUE) + 1)        
           W_Rango6f:SCREEN-VALUE  = STRING(DECIMAL(W_Rango6i:SCREEN-VALUE) + FInc - 1)     
           W_Rango7i:SCREEN-VALUE  = STRING(DECIMAL(W_Rango6f:SCREEN-VALUE) + 1)        
           W_Rango7f:SCREEN-VALUE  = STRING(DECIMAL(W_Rango7i:SCREEN-VALUE) + FInc - 1)     
           W_Rango8i:SCREEN-VALUE  = STRING(DECIMAL(W_Rango7f:SCREEN-VALUE) + 1)        
           W_Rango8f:SCREEN-VALUE  = STRING(DECIMAL(W_Rango8i:SCREEN-VALUE) + FInc - 1)     
           W_Rango9i:SCREEN-VALUE  = STRING(DECIMAL(W_Rango8f:SCREEN-VALUE) + 1)       
           W_Rango9f:SCREEN-VALUE  = STRING(DECIMAL(W_Rango9i:SCREEN-VALUE) + FInc - 1)    
           W_Rango10i:SCREEN-VALUE = STRING(DECIMAL(W_Rango9f:SCREEN-VALUE) + 1)        
           W_Rango10f:SCREEN-VALUE = STRING(DECIMAL(W_Rango10i:SCREEN-VALUE) + FInc - 1)
           W_TotValor:SCREEN-VALUE = "0" 
           W_TotOcur:SCREEN-VALUE  = "0"           
           W_TotGen                = 0
           W_TotCont               = 0.
   
ASSIGN FRAME F-Main W_Rango1i W_Rango1f W_Rango2i W_Rango2f
                    W_Rango3i W_Rango3f W_Rango4i W_Rango4f 
                    W_Rango5i W_Rango5f W_Rango6i W_Rango6f
                    W_Rango7i W_Rango7f W_Rango8i W_Rango8f
                    W_Rango9i W_Rango9f W_Rango10i W_Rango10f.
      APPLY "leave" TO W_Rango1i.
      APPLY "leave" TO W_Rango1f.
      APPLY "leave" TO W_Rango2i.
      APPLY "leave" TO W_Rango2f.
      APPLY "leave" TO W_Rango3i.
      APPLY "leave" TO W_Rango3f.
      APPLY "leave" TO W_Rango4i.
      APPLY "leave" TO W_Rango4f.
      APPLY "leave" TO W_Rango5i.
      APPLY "leave" TO W_Rango5f.
      APPLY "leave" TO W_Rango6i.
      APPLY "leave" TO W_Rango6f.
      APPLY "leave" TO W_Rango7i.
      APPLY "leave" TO W_Rango7f.
      APPLY "leave" TO W_Rango8i.
      APPLY "leave" TO W_Rango8f.
      APPLY "leave" TO W_Rango9i.
      APPLY "leave" TO W_Rango9f.
      APPLY "leave" TO W_Rango10i.
      APPLY "leave" TO W_Rango10f.
      
  END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize W-ProCre 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
  DO WITH FRAME f-main:     
     ASSIGN W_OfStr = "000 CONSOLIDADO"
            W_Metodo = W_CmbOfi:ADD-LAST(W_OfStr).    
     
     FOR EACH agencias WHERE agencias.Estado NE 2 NO-LOCK:
         ASSIGN W_OfStr = STRING(agencias.agencia,"999") + 
                          "-" + STRING(agencias.Nombre,"X(16)")
                W_Metodo = W_CmbOfi:ADD-LAST(W_OfStr).    
         IF agencias.agencia EQ W_agencia THEN
            ASSIGN W_agenciaP = W_OfStr
                   W_CmbOfi:SCREEN-VALUE = W_OfStr.
     END.              
     ASSIGN W_OfiTbajo = INTEGER(SUBSTRING(W_CmbOfi:SCREEN-VALUE,1,3))
            W_OfiIni   = W_OfiTbajo
            W_OfiFin   = W_OfiTbajo.
            
     APPLY "VALUE-CHANGED" TO W_CmbPcto.
     APPLY "VALUE-CHANGED" TO R_Opcion.
     APPLY "ENTRY" TO W_CmbOfi.
  END.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir W-ProCre 
PROCEDURE ProcesoImprimir :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE W_OfiInf AS CHARACTER FORMAT "X(40)".
  DEFINE VARIABLE W_ProInf AS CHARACTER FORMAT "X(40)".
  
  ASSIGN W_OfiInf = W_CmbOfi:SCREEN-VALUE IN FRAME {&frame-name}
         W_ProInf = W_CmbPcto:SCREEN-VALUE IN FRAME {&frame-name}.
  
  IF R_Opcion = 1 THEN
     ASSIGN W_NomInf   = "CAPTACION". 
  ELSE ASSIGN W_NomInf = "CREDITOS".

  DEFINE FRAME f-encabezado
    HEADER
      SKIP(2)       
      "PAGINA:"                                       AT  1 PAGE-NUMBER FORMAT ">9" 
      W_Nom_Entidad                                   AT 15
      "FECHA:"                                        AT 77 
      TODAY                                           AT 85 FORMAT "99/99/9999" SKIP(1)
      "Informe de Rangos - "                          AT 25
      W_NomInf                                        AT 46 SKIP(1)
      "agencia:"                                      AT  1
      W_OfiInf                                        AT 11 
      "PRODUCTO:"                                     AT  1
      W_ProInf                                        AT 11
      W_Linea1                                        AT  1 SKIP(1)
      "R A N G O S"                                   AT 15
      "Inicial"                                       AT 10
      "Final"                                         AT 25
      "VALORES"                                       AT 45
      "OCURRENCIAS"                                   AT 65
      W_Linea                                         AT  1 SKIP      
   WITH WIDTH 200 USE-UNDERLINE USE-TEXT STREAM-IO PAGE-TOP FRAME f-encabezado.
           
  ASSIGN W_Linea  = FILL(raya,95)
         W_Linea1 = FILL(raya,95).

  DEFINE FRAME f-reportes
      WITH WIDTH 200 NO-LABELS.       
  DEFINE FRAME f-PiePagina
    HEADER 
      W_Nom_agencia  AT 23 FONT 9
      "HORA:"        AT 85 STRING(TIME,"HH:MM AM")
      WITH WIDTH 200 FRAME f-piepagina PAGE-BOTTOM USE-TEXT STREAM-IO. 

  VIEW FRAME f-encabezado.
  VIEW FRAME f-piepagina.
  RUN Asignar_Campos.
  DISPLAY 
     W_Rango1I       AT  1
     W_Rango1F       AT 18
     Valor1          AT 40
     Ocur1           AT 60 SKIP(1)
     W_Rango2I       AT  1
     W_Rango2F       AT 18
     Valor2          AT 40
     Ocur2           AT 60 SKIP 
     W_Rango3I       AT  1
     W_Rango3F       AT 18
     Valor3          AT 40
     Ocur3           AT 60 SKIP
     W_Rango4I       AT  1
     W_Rango4F       AT 18
     Valor4          AT 40
     Ocur4           AT 60 SKIP
     W_Rango5I       AT  1
     W_Rango5F       AT 18
     Valor5          AT 40
     Ocur5           AT 60 SKIP
     W_Rango6I       AT  1
     W_Rango6F       AT 18
     Valor6          AT 40
     Ocur6           AT 60 SKIP(1)
     W_Rango7I       AT  1
     W_Rango7F       AT 18
     Valor7          AT 40
     Ocur7           AT 60 SKIP
     W_Rango8I       AT  1
     W_Rango8F       AT 18
     Valor8          AT 40
     Ocur8           AT 60 SKIP
     W_Rango9I       AT  1
     W_Rango9F       AT 18
     Valor9          AT 40
     Ocur9           AT 60 SKIP
     W_Rango10I      AT  1
     W_Rango10F      AT 18
     Valor10         AT 40
     Ocur10          AT 60 SKIP 
     W_Linea         AT  1   
     W_TotGen        AT 40
     W_TotCont       AT 60    
  WITH WIDTH 200 FRAME f-Reportes NO-LABELS USE-TEXT STREAM-IO NO-BOX.
  PAGE.
  OUTPUT CLOSE.   
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Totales W-ProCre 
PROCEDURE Totales :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN W_TotGen   = (INTEGER(Valor1:SCREEN-VALUE) + INTEGER(Valor2:SCREEN-VALUE) +       
                         INTEGER(Valor3:SCREEN-VALUE) + INTEGER(Valor4:SCREEN-VALUE) +
                         INTEGER(Valor5:SCREEN-VALUE) + INTEGER(Valor6:SCREEN-VALUE) +
                         INTEGER(Valor7:SCREEN-VALUE) + INTEGER(Valor8:SCREEN-VALUE) +
                         INTEGER(Valor9:SCREEN-VALUE) + INTEGER(Valor10:SCREEN-VALUE))       
            W_TotCont  = (INTEGER(Ocur1:SCREEN-VALUE) + INTEGER(Ocur2:SCREEN-VALUE) +
                         INTEGER(Ocur3:SCREEN-VALUE) + INTEGER(Ocur4:SCREEN-VALUE) +
                         INTEGER(Ocur5:SCREEN-VALUE) + INTEGER(Ocur6:SCREEN-VALUE) +
                         INTEGER(Ocur7:SCREEN-VALUE) + INTEGER(Ocur8:SCREEN-VALUE) +         
                         INTEGER(Ocur9:SCREEN-VALUE) + INTEGER(Ocur10:SCREEN-VALUE)).      
  END.                       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Valida_Rango1 W-ProCre 
PROCEDURE Valida_Rango1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
     IF ( W_WkIni GE W_WkFin )
     OR ( W_WkIni GE W_Rango1i AND W_WkIni LE W_Rango1f )
     OR ( W_WkIni LE W_Rango1i AND W_WkFin LE W_Rango1f )
     OR ( W_WkIni LE W_Rango1i AND W_WkFin GE W_Rango1f )
     OR ( ( W_WkIni GE W_Rango1i AND W_WkIni LE W_Rango1f )
          AND W_WkFin GE W_Rango1f ) THEN DO:
              RUN MostrarMensaje IN W_Manija (INPUT 305,OUTPUT W_eleccion).
              ASSIGN W_Rpta = TRUE.
     END.  
     ELSE ASSIGN W_Rpta = FALSE.   
  END.         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Valida_Rango2 W-ProCre 
PROCEDURE Valida_Rango2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
     IF ( W_WkIni GE W_WkFin )
     OR ( W_WkIni GE W_Rango2i AND W_WkIni LE W_Rango2f )
     OR ( W_WkIni LE W_Rango2i AND W_WkFin LE W_Rango2f )
     OR ( W_WkIni LE W_Rango2i AND W_WkFin GE W_Rango2f )
     OR ( ( W_WkIni GE W_Rango2i AND W_WkIni LE W_Rango2f )
          AND W_WkFin GE W_Rango2f ) THEN DO:
              RUN MostrarMensaje IN W_Manija (INPUT 305,OUTPUT W_eleccion).
              ASSIGN W_Rpta = TRUE.
     END.  
     ELSE ASSIGN W_Rpta = FALSE.   
  END.         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Valida_Rango3 W-ProCre 
PROCEDURE Valida_Rango3 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
     IF ( W_WkIni GE W_WkFin )
     OR ( W_WkIni GE W_Rango3i AND W_WkIni LE W_Rango3f )
     OR ( W_WkIni LE W_Rango3i AND W_WkFin LE W_Rango3f )
     OR ( W_WkIni LE W_Rango3i AND W_WkFin GE W_Rango3f )
     OR ( ( W_WkIni GE W_Rango3i AND W_WkIni LE W_Rango3f )
          AND W_WkFin GE W_Rango3f ) THEN DO:
              RUN MostrarMensaje IN W_Manija (INPUT 305,OUTPUT W_eleccion).
              ASSIGN W_Rpta = TRUE.
     END.  
     ELSE ASSIGN W_Rpta = FALSE.   
  END.         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Valida_Rango4 W-ProCre 
PROCEDURE Valida_Rango4 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
     IF ( W_WkIni GE W_WkFin )
     OR ( W_WkIni GE W_Rango4i AND W_WkIni LE W_Rango4f )
     OR ( W_WkIni LE W_Rango4i AND W_WkFin LE W_Rango4f )
     OR ( W_WkIni LE W_Rango4i AND W_WkFin GE W_Rango4f )
     OR ( ( W_WkIni GE W_Rango4i AND W_WkIni LE W_Rango4f )
          AND W_WkFin GE W_Rango4f ) THEN DO:
              RUN MostrarMensaje IN W_Manija (INPUT 305,OUTPUT W_eleccion).
              ASSIGN W_Rpta = TRUE.
     END.  
     ELSE ASSIGN W_Rpta = FALSE.   
  END.         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Valida_Rango5 W-ProCre 
PROCEDURE Valida_Rango5 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
     IF ( W_WkIni GE W_WkFin )
     OR ( W_WkIni GE W_Rango5i AND W_WkIni LE W_Rango5f )
     OR ( W_WkIni LE W_Rango5i AND W_WkFin LE W_Rango5f )
     OR ( W_WkIni LE W_Rango5i AND W_WkFin GE W_Rango5f )
     OR ( ( W_WkIni GE W_Rango5i AND W_WkIni LE W_Rango5f )
          AND W_WkFin GE W_Rango5f ) THEN DO:
              RUN MostrarMensaje IN W_Manija (INPUT 305,OUTPUT W_eleccion).
              ASSIGN W_Rpta = TRUE.
     END.  
     ELSE ASSIGN W_Rpta = FALSE.   
  END.         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Valida_Rango6 W-ProCre 
PROCEDURE Valida_Rango6 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
     IF ( W_WkIni GE W_WkFin )
     OR ( W_WkIni GE W_Rango6i AND W_WkIni LE W_Rango6f )
     OR ( W_WkIni LE W_Rango6i AND W_WkFin LE W_Rango6f )
     OR ( W_WkIni LE W_Rango6i AND W_WkFin GE W_Rango6f )
     OR ( ( W_WkIni GE W_Rango6i AND W_WkIni LE W_Rango6f )
          AND W_WkFin GE W_Rango6f ) THEN DO:
              RUN MostrarMensaje IN W_Manija (INPUT 305,OUTPUT W_eleccion).
              ASSIGN W_Rpta = TRUE.
     END.  
     ELSE ASSIGN W_Rpta = FALSE.   
  END.         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Valida_Rango7 W-ProCre 
PROCEDURE Valida_Rango7 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
     IF ( W_WkIni GE W_WkFin )
     OR ( W_WkIni GE W_Rango7i AND W_WkIni LE W_Rango7f )
     OR ( W_WkIni LE W_Rango7i AND W_WkFin LE W_Rango7f )
     OR ( W_WkIni LE W_Rango7i AND W_WkFin GE W_Rango7f )
     OR ( ( W_WkIni GE W_Rango7i AND W_WkIni LE W_Rango7f )
          AND W_WkFin GE W_Rango7f ) THEN DO:
              RUN MostrarMensaje IN W_Manija (INPUT 305,OUTPUT W_eleccion).
              ASSIGN W_Rpta = TRUE.
     END.  
     ELSE ASSIGN W_Rpta = FALSE.   
  END.         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Valida_Rango8 W-ProCre 
PROCEDURE Valida_Rango8 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
     IF ( W_WkIni GE W_WkFin )
     OR ( W_WkIni GE W_Rango8i AND W_WkIni LE W_Rango8f )
     OR ( W_WkIni LE W_Rango8i AND W_WkFin LE W_Rango8f )
     OR ( W_WkIni LE W_Rango8i AND W_WkFin GE W_Rango8f )
     OR ( ( W_WkIni GE W_Rango8i AND W_WkIni LE W_Rango8f )
          AND W_WkFin GE W_Rango8f ) THEN DO:
              RUN MostrarMensaje IN W_Manija (INPUT 305,OUTPUT W_eleccion).
              ASSIGN W_Rpta = TRUE.
     END.  
     ELSE ASSIGN W_Rpta = FALSE.   
  END.         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Valida_Rango9 W-ProCre 
PROCEDURE Valida_Rango9 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
     IF ( W_WkIni GE W_WkFin )
     OR ( W_WkIni GE W_Rango9i AND W_WkIni LE W_Rango9f )
     OR ( W_WkIni LE W_Rango9i AND W_WkFin LE W_Rango9f )
     OR ( W_WkIni LE W_Rango9i AND W_WkFin GE W_Rango9f )
     OR ( ( W_WkIni GE W_Rango9i AND W_WkIni LE W_Rango9f )
          AND W_WkFin GE W_Rango9f ) THEN DO:
              RUN MostrarMensaje IN W_Manija (INPUT 305,OUTPUT W_eleccion).
              ASSIGN W_Rpta = TRUE.
     END.  
     ELSE ASSIGN W_Rpta = FALSE.   
  END.         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

