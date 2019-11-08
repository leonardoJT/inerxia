&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
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
 {Incluido/Variable.I "SHARED"}
  DEFINE VARIABLE W_Ok    AS LOGICAL.
  DEFINE VARIABLE W_Nuevo AS LOGICAL.
  DEFINE VARIABLE Puntero AS ROWID.
  
  DEFINE VAR CodWork LIKE Pro_Ahorros.Cod_Ahorro.    
  DEFINE VAR W_AgeDest  LIKE Agencias.Agencia.
  DEFINE VAR W_Ind LIKE Indicadores.Indicador.
  DEFINE VAR W_Cero   LIKE Indicadores.Valor.
  DEFINE TEMP-TABLE RegPCre           LIKE Pro_Creditos.

  DEFINE TEMP-TABLE Tmp
    FIELD TCodDed LIKE Deducible.Cod_Deducible
    FIELD TNomDed LIKE Deducible.Nom_Deducible
    FIELD TValDed LIKE bdCentral.Deducible.Valor.
  
  DEFINE VAR i AS INTEGER.

  /*para busqueda de indicadores*/
  DEFINE VARIABLE P_CodInd  LIKE Indicadores.Indicador.
  DEFINE VARIABLE P_NomInd  LIKE Indicadores.Nombre.
  DEFINE VARIABLE P_ValTas  LIKE Indicadores.Tasa.
  DEFINE VARIABLE P_Valor   LIKE Indicadores.Valor.
  
  /*para busqueda de deducibles*/
  DEFINE VARIABLE W_Deducible     LIKE Deducible.Cod_Deducible INITIAL "".
  DEFINE VARIABLE W_NomDeducible  LIKE Deducible.Nom_Deducible INITIAL "".
  /*para consultar un producto*/
  DEFINE VARIABLE P_Cod     LIKE Pro_Creditos.Cod_Credito.
  DEFINE VARIABLE p_Nombre  LIKE Pro_Creditos.Nom_Producto.
  DEFINE VARIABLE P_AgePro  LIKE Creditos.Agencia.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Contabilidad
&Scoped-define BROWSE-NAME Brw_Deducibles

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Tmp

/* Definitions for BROWSE Brw_Deducibles                                */
&Scoped-define FIELDS-IN-QUERY-Brw_Deducibles Tmp.TCodDed Tmp.TNomDed Tmp.TValDed   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Brw_Deducibles Tmp.TcodDed   
&Scoped-define ENABLED-TABLES-IN-QUERY-Brw_Deducibles Tmp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Brw_Deducibles Tmp
&Scoped-define SELF-NAME Brw_Deducibles
&Scoped-define QUERY-STRING-Brw_Deducibles FOR EACH Tmp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Brw_Deducibles OPEN QUERY {&SELF-NAME} FOR EACH Tmp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Brw_Deducibles Tmp
&Scoped-define FIRST-TABLE-IN-QUERY-Brw_Deducibles Tmp


/* Definitions for FRAME F_Deducibles                                   */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Deducibles ~
    ~{&OPEN-QUERY-Brw_Deducibles}

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-2 Pro_Creditos.Prioridad 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Borrar 
     LABEL "Borrar" 
     SIZE 12 BY 1.62.

DEFINE BUTTON Btn_Cancelar 
     LABEL "Cancelar" 
     SIZE 12 BY 1.62.

DEFINE BUTTON Btn_contabilizacion 
     LABEL "Contabilización" 
     SIZE 15 BY 1.12.

DEFINE BUTTON Btn_Deducibles 
     LABEL "Deducibles" 
     SIZE 15 BY 1.12.

DEFINE BUTTON Btn_Deshacer 
     LABEL "Deshacer" 
     SIZE 12 BY 1.62.

DEFINE BUTTON Btn_Generales 
     LABEL "Generales" 
     SIZE 15 BY 1.12
     FONT 5.

DEFINE BUTTON Btn_Ingresar 
     LABEL "Ingresar" 
     SIZE 12 BY 1.62.

DEFINE BUTTON Btn_Intereses 
     LABEL "Intereses" 
     SIZE 15 BY 1.12.

DEFINE BUTTON Btn_Rangos 
     LABEL "Rangos" 
     SIZE 12 BY 1.62.

DEFINE BUTTON Btn_Salir 
     LABEL "Salir" 
     SIZE 12 BY 1.62.

DEFINE BUTTON Btn_Salvar 
     LABEL "Salvar" 
     SIZE 12 BY 1.62.

DEFINE BUTTON Btn_Varios 
     LABEL "Varios" 
     SIZE 15 BY 1.12
     FONT 5.

DEFINE BUTTON BUTTON-120 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 120" 
     SIZE 12 BY 1.62.

DEFINE BUTTON BUTTON-121 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Button 121" 
     SIZE 12 BY 1.62.

DEFINE BUTTON BUTTON-122 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     LABEL "Button 122" 
     SIZE 12 BY 1.62.

DEFINE BUTTON BUTTON-130 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 130" 
     SIZE 4 BY 1.12.

DEFINE BUTTON BUTTON-132 
     LABEL "Restricciones" 
     SIZE 15 BY 1.12.

DEFINE RECTANGLE RECT-237
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 89 BY 1.35
     BGCOLOR 18 .

DEFINE RECTANGLE RECT-280
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 30 BY 2.15.

DEFINE RECTANGLE RECT-281
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 14 BY 5.38.

DEFINE RECTANGLE RECT-282
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 14 BY 11.85.

DEFINE VARIABLE Cmb_ForPagare AS CHARACTER FORMAT "X(40)":U 
     LABEL "Pagare" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 39 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_ForRetencion AS CHARACTER FORMAT "X(40)":U 
     LABEL "Retención" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 39 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_ProAhorros AS CHARACTER FORMAT "X(40)":U INITIAL "000 - No Asignado" 
     LABEL "Productos de Ahorros" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "000 - No Asignado" 
     DROP-DOWN-LIST
     SIZE 31 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-229
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 54 BY 2.96.

DEFINE RECTANGLE RECT-230
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 54 BY 1.88.

DEFINE RECTANGLE RECT-231
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46 BY 1.62.

DEFINE VARIABLE nomIndicadorMax AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 37 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Nom_IndicadorMora AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 36 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Nom_IndicadorTasa AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 36 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE ValorTasaMax AS DECIMAL FORMAT "->>,>>9.999999":U INITIAL 0 
     LABEL "Valor Tasa Maxima" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Valor_Tasa AS DECIMAL FORMAT ">>9.9999999":U INITIAL 0 
     LABEL "Tasa" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Valor_TasaMora AS DECIMAL FORMAT ">>9.9999999":U INITIAL 0 
     LABEL "Tasa" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-235
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 78 BY 3.23.

DEFINE RECTANGLE RECT-236
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 78 BY 4.04.

DEFINE RECTANGLE RECT-334
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 78 BY 3.5.

DEFINE BUTTON Btn_Rangos1 
     LABEL "Salvar" 
     SIZE 12 BY 1.62
     FONT 5.

DEFINE BUTTON Btn_Rangos2 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     LABEL "Btn_Rangos2" 
     SIZE 12 BY 1.62.

DEFINE VARIABLE WAnos AS INTEGER FORMAT "Z9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 3.57 BY .81
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE WTasa AS DECIMAL FORMAT "Z.9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4 BY .81
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE WVeces AS INTEGER FORMAT "Z9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4 BY .81
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE RECTANGLE RECT-285
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 34.57 BY 2.69.

DEFINE RECTANGLE RECT-286
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 14 BY 3.77.

DEFINE RECTANGLE RECT-287
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 18 BY 1.35.

DEFINE RECTANGLE RECT-283
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 42 BY 1.62.

DEFINE VARIABLE Cmb_ForChequeo AS CHARACTER FORMAT "X(20)":U 
     LABEL "Digito de Chequeo" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 31 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_ForTalonario AS CHARACTER FORMAT "X(25)":U 
     LABEL "Formato" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 46 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_ProMultiplicador AS CHARACTER FORMAT "X(25)":U 
     LABEL "Ahorro" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "000 - No Asignado" 
     DROP-DOWN-LIST
     SIZE 47 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-232
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 84 BY 1.88.

DEFINE RECTANGLE RECT-233
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 33 BY 1.88.

DEFINE RECTANGLE RECT-234
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 84 BY 1.62.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Brw_Deducibles FOR 
      Tmp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Brw_Deducibles
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Brw_Deducibles wWin _FREEFORM
  QUERY Brw_Deducibles NO-LOCK DISPLAY
      Tmp.TCodDed  LABEL "Codigo"
  Tmp.TNomDed  LABEL "Nombre"
  Tmp.TValDed  LABEL "Valor"
  ENABLE Tmp.TcodDed
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 81 BY 8.62
         BGCOLOR 15 FONT 5 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Creditos
     Pro_Creditos.Fec_Matricula AT ROW 1.81 COL 78 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .81
          BGCOLOR 18 FGCOLOR 15 
     BUTTON-120 AT ROW 1.81 COL 99
     Pro_Creditos.Cod_Credito AT ROW 2.08 COL 13 COLON-ALIGNED
          LABEL "Código"
          VIEW-AS FILL-IN 
          SIZE 4.43 BY .81
          BGCOLOR 15 
     Pro_Creditos.Nom_Producto AT ROW 2.08 COL 18 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 42 BY .81
          BGCOLOR 15 
     Pro_Creditos.Fec_Retiro AT ROW 2.62 COL 78 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Pro_Creditos.Estado AT ROW 2.88 COL 38 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Activo", 1,
"Inactivo", 2
          SIZE 24 BY .81
     BUTTON-121 AT ROW 3.42 COL 99
     Pro_Creditos.Tip_Credito AT ROW 3.96 COL 5 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Consumo", 1,
"Comercial", 2,
"Hipotecario", 3,
"Microcredito", 4,
"Bienes y Serv.", 5,
"Empleados", 6,
"Convenios", 7
          SIZE 87 BY .81
          BGCOLOR 18 FGCOLOR 15 
     BUTTON-122 AT ROW 5.04 COL 99
     Btn_Generales AT ROW 5.58 COL 4
     Btn_Varios AT ROW 5.58 COL 19
     Btn_Intereses AT ROW 5.58 COL 34
     Btn_Deducibles AT ROW 5.58 COL 49
     BUTTON-132 AT ROW 5.58 COL 64
     Btn_contabilizacion AT ROW 5.58 COL 79
     Btn_Rangos AT ROW 9.08 COL 99.14 WIDGET-ID 2
     Btn_Salvar AT ROW 10.69 COL 99
     Btn_Deshacer AT ROW 12.31 COL 99
     Btn_Ingresar AT ROW 13.92 COL 99
     Btn_Borrar AT ROW 15.54 COL 99
     Btn_Cancelar AT ROW 17.15 COL 99
     Btn_Salir AT ROW 18.77 COL 99
     BUTTON-130 AT ROW 20.65 COL 103
     RECT-237 AT ROW 3.69 COL 4
     RECT-280 AT ROW 1.54 COL 63
     RECT-281 AT ROW 1.54 COL 98
     RECT-282 AT ROW 8.81 COL 98
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.14 BY 21.46
         BGCOLOR 17 FONT 5.

DEFINE FRAME F_Restricciones
     Pro_Creditos.Id_Montomaximo AT ROW 1.81 COL 6
          VIEW-AS TOGGLE-BOX
          SIZE 24 BY .81
     Pro_Creditos.Val_Montomaximo AT ROW 1.81 COL 28 COLON-ALIGNED NO-LABEL FORMAT ">,>>>,>>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 16.43 BY .81
          BGCOLOR 15 
     Pro_Creditos.Id_Montominimo AT ROW 3.42 COL 6
          VIEW-AS TOGGLE-BOX
          SIZE 23 BY .81
     Pro_Creditos.Val_Montominimo AT ROW 3.42 COL 28 COLON-ALIGNED NO-LABEL FORMAT ">,>>>,>>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 16.43 BY .81
          BGCOLOR 15 
     Pro_Creditos.Id_Plazo AT ROW 4.92 COL 6
          VIEW-AS TOGGLE-BOX
          SIZE 18 BY .81
     Pro_Creditos.Pla_Minimo AT ROW 5.85 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5.43 BY .81
          BGCOLOR 15 
     Pro_Creditos.Pla_Maximo AT ROW 5.85 COL 38.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5.43 BY .81
          BGCOLOR 15 
     RECT-283 AT ROW 5.31 COL 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 4 ROW 6.92
         SIZE 90 BY 15.08
         BGCOLOR 17 FGCOLOR 0 FONT 5
         TITLE "Restricciones".

DEFINE FRAME F_Varios
     Pro_Creditos.Id_Linea AT ROW 1.27 COL 5
          LABEL "Maneja Línea"
          VIEW-AS TOGGLE-BOX
          SIZE 18 BY .81
     Cmb_ForChequeo AT ROW 1.27 COL 52 COLON-ALIGNED
     Pro_Creditos.Id_Debito AT ROW 2.35 COL 5
          LABEL "Maneja Débito Automático"
          VIEW-AS TOGGLE-BOX
          SIZE 26 BY .81
     Pro_Creditos.Id_CreUnico AT ROW 3.42 COL 5
          VIEW-AS TOGGLE-BOX
          SIZE 15 BY .77
     Pro_Creditos.Id_PagParcial AT ROW 4.5 COL 5
          LABEL "Respeta Liquidación Por Proyección"
          VIEW-AS TOGGLE-BOX
          SIZE 35 BY .81
     Pro_Creditos.Id_Extracto AT ROW 5.58 COL 5
          LABEL "Es Transitorio"
          VIEW-AS TOGGLE-BOX
          SIZE 18 BY .81 TOOLTIP "Marque solo si el Producto es Transitorio"
     Pro_Creditos.Id_Sorteos AT ROW 6.65 COL 5
          LABEL "X Libranza(Nómina)"
          VIEW-AS TOGGLE-BOX
          SIZE 20 BY .81 TOOLTIP "Marque solo si el pago es X Libranza(Nómina)"
     Pro_Creditos.Id_PerGracia AT ROW 8 COL 5
          LABEL "Manejo de Período de Gracia"
          VIEW-AS TOGGLE-BOX
          SIZE 29 BY .54
     Pro_Creditos.Dia_Gracia AT ROW 8.88 COL 18.43 COLON-ALIGNED
          LABEL "Dias de Gracia"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 15 
     Cmb_ForTalonario AT ROW 11.12 COL 38 COLON-ALIGNED
     Pro_Creditos.Id_Talonario AT ROW 11.19 COL 7 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Ninguno", 1,
"Libreta", 2
          SIZE 22 BY .81
          BGCOLOR 17 
     Pro_Creditos.Id_CreProAhorro AT ROW 12.85 COL 5
          LABEL "Base Aprobación"
          VIEW-AS TOGGLE-BOX
          SIZE 18 BY .54
     Pro_Creditos.Multiplicador AT ROW 13.65 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 15 
     Cmb_ProMultiplicador AT ROW 13.65 COL 37 COLON-ALIGNED
     "  Maneja Algun Tipo de Talonario" VIEW-AS TEXT
          SIZE 29 BY 1.08 AT ROW 10.15 COL 6
          BGCOLOR 17 FGCOLOR 7 FONT 5
     RECT-232 AT ROW 13.12 COL 4
     RECT-233 AT ROW 8.27 COL 4
     RECT-234 AT ROW 10.69 COL 4
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 4 ROW 6.92
         SIZE 90 BY 15.08
         BGCOLOR 17 FONT 5
         TITLE "Información Varia".

DEFINE FRAME F_Rangos
     WAnos AT ROW 1.54 COL 7.57 COLON-ALIGNED NO-LABEL WIDGET-ID 24
     Btn_Rangos2 AT ROW 1.96 COL 38 WIDGET-ID 30
     WTasa AT ROW 3.23 COL 9.86 COLON-ALIGNED NO-LABEL WIDGET-ID 26
     Btn_Rangos1 AT ROW 3.73 COL 38 WIDGET-ID 20
     WVeces AT ROW 4.35 COL 13.29 COLON-ALIGNED NO-LABEL WIDGET-ID 28
     "Hasta" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 1.62 COL 3.29 WIDGET-ID 8
          FONT 5
     "Años" VIEW-AS TEXT
          SIZE 5 BY .62 AT ROW 1.62 COL 14 WIDGET-ID 10
          FONT 5
     "Descontar" VIEW-AS TEXT
          SIZE 9.14 BY .62 AT ROW 3.31 COL 2.72 WIDGET-ID 12
          FONT 5
     "Puntos a la Tasa Plena" VIEW-AS TEXT
          SIZE 19.72 BY .62 AT ROW 3.27 COL 16 WIDGET-ID 14
          FONT 5
     "Prestar Hasta" VIEW-AS TEXT
          SIZE 12.29 BY .62 AT ROW 4.5 COL 2.72 WIDGET-ID 16
          FONT 5
     "Veces los Aportes" VIEW-AS TEXT
          SIZE 16.29 BY .62 AT ROW 4.5 COL 19.72 WIDGET-ID 18
          FONT 5
     RECT-285 AT ROW 2.81 COL 1.72 WIDGET-ID 22
     RECT-286 AT ROW 1.77 COL 37 WIDGET-ID 32
     RECT-287 AT ROW 1.27 COL 2 WIDGET-ID 34
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 43 ROW 6.92
         SIZE 51 BY 5.65
         BGCOLOR 17 
         TITLE "Rangos Comunes" WIDGET-ID 100.

DEFINE FRAME F_Intereses
     Pro_Creditos.Id_Tasa AT ROW 2.62 COL 11 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Por Producto", 1,
"Por Cuenta", 2
          SIZE 38 BY .81
          FONT 5
     Pro_Creditos.Cod_Tasa AT ROW 3.96 COL 31 COLON-ALIGNED
          LABEL "Indicador de Intereses"
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 15 
     Nom_IndicadorTasa AT ROW 3.96 COL 44 COLON-ALIGNED NO-LABEL
     Valor_Tasa AT ROW 5.04 COL 31 COLON-ALIGNED
     Pro_Creditos.Cod_TasaMora AT ROW 7.73 COL 31 COLON-ALIGNED
          LABEL "Indicador Interés de Mora"
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 15 
     Nom_IndicadorMora AT ROW 7.73 COL 44 COLON-ALIGNED NO-LABEL
     Valor_TasaMora AT ROW 8.81 COL 31 COLON-ALIGNED
     Pro_Creditos.Cod_TasaMax AT ROW 11.69 COL 31.14 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 11.86 BY .81
          BGCOLOR 15 
     nomIndicadorMax AT ROW 11.69 COL 43.14 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     ValorTasaMax AT ROW 12.85 COL 31 COLON-ALIGNED WIDGET-ID 10
     "Tasa de Interés Máxima efectivo Anual" VIEW-AS TEXT
          SIZE 34 BY 1.08 AT ROW 10.5 COL 9 WIDGET-ID 4
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "Tasa de Interés Efectiva Anual" VIEW-AS TEXT
          SIZE 28 BY 1.08 AT ROW 1.54 COL 11
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "Tasa de Interés Mora efectivo Anual" VIEW-AS TEXT
          SIZE 32 BY 1.08 AT ROW 6.38 COL 10
          BGCOLOR 17 FGCOLOR 7 FONT 5
     RECT-235 AT ROW 6.92 COL 7
     RECT-236 AT ROW 2.08 COL 7
     RECT-334 AT ROW 10.96 COL 7 WIDGET-ID 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 4 ROW 6.92
         SIZE 90 BY 15.08
         BGCOLOR 17 FONT 5
         TITLE "Configuración de Intereses".

DEFINE FRAME F_Contabilidad
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 4 ROW 6.92
         SIZE 90 BY 15.08
         BGCOLOR 17 FONT 5
         TITLE "Cuentas Contables".

DEFINE FRAME F_Deducibles
     Brw_Deducibles AT ROW 4.23 COL 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 4 ROW 6.92
         SIZE 90 BY 15.08
         BGCOLOR 17 FGCOLOR 0 FONT 5
         TITLE "Configuración de Deducibles del Producto".

DEFINE FRAME F_Generales
     Pro_Creditos.Prioridad AT ROW 1.19 COL 79 COLON-ALIGNED FORMAT "9999"
          VIEW-AS FILL-IN 
          SIZE 5 BY .81
          BGCOLOR 15 FGCOLOR 0 
     Pro_Creditos.Per_GarPer AT ROW 2 COL 79 COLON-ALIGNED
          LABEL "Días Dif.Cobro Otras Garantías"
          VIEW-AS FILL-IN 
          SIZE 5 BY .81
          BGCOLOR 15 
     Pro_Creditos.Id_Asociado AT ROW 2.35 COL 4 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Todos", 1,
"Asociados", 2,
"Empleados", 3
          SIZE 43 BY .81
     Pro_Creditos.Per_GarRea AT ROW 2.85 COL 79 COLON-ALIGNED
          LABEL "Días Dif.Cobro Garantía Admisible"
          VIEW-AS FILL-IN 
          SIZE 5 BY .81
          BGCOLOR 15 
     Pro_Creditos.Id_NumAlterno AT ROW 3.69 COL 6
          LABEL "Requiere Número Alterno"
          VIEW-AS TOGGLE-BOX
          SIZE 26 BY .77
     Pro_Creditos.Id_Garantia AT ROW 3.69 COL 79 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 5 BY .81
          BGCOLOR 15 
     Pro_Creditos.Id_AsociaCta AT ROW 4.5 COL 35
          VIEW-AS TOGGLE-BOX
          SIZE 17 BY .81
     Pro_Creditos.Id_Consecutivo AT ROW 4.77 COL 6
          LABEL "Consecutivo"
          VIEW-AS TOGGLE-BOX
          SIZE 14 BY .81
     Cmb_ProAhorros AT ROW 5.31 COL 53 COLON-ALIGNED
     Pro_Creditos.Consecutivo AT ROW 5.85 COL 4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 23 BY .81
          BGCOLOR 15 
     Pro_Creditos.Id_Formato AT ROW 6.92 COL 35
          VIEW-AS TOGGLE-BOX
          SIZE 17 BY .81
     Pro_Creditos.Id_AprobAgencia AT ROW 8 COL 6 HELP
          "Id_AprobAgencia: Si el producto se apruebas en la misma agencia"
          LABEL "Aprobar en la Agencia"
          VIEW-AS TOGGLE-BOX
          SIZE 22.86 BY .77 TOOLTIP "Marque solo si se aprueba en la misma agencia"
     Cmb_ForPagare AT ROW 8 COL 45 COLON-ALIGNED
     Cmb_ForRetencion AT ROW 9.08 COL 45 COLON-ALIGNED
     Pro_Creditos.Descripcion AT ROW 10.69 COL 4 NO-LABEL
          VIEW-AS EDITOR MAX-CHARS 240 SCROLLBAR-VERTICAL
          SIZE 82 BY 3.77
          BGCOLOR 15 
     "Características del Producto" VIEW-AS TEXT
          SIZE 26 BY .81 AT ROW 9.88 COL 4
          BGCOLOR 17 FGCOLOR 7 FONT 5
     " Producto Dedicado a..." VIEW-AS TEXT
          SIZE 20 BY 1.04 AT ROW 1.27 COL 3
          BGCOLOR 17 FGCOLOR 7 FONT 5
     RECT-229 AT ROW 7.19 COL 33
     RECT-230 AT ROW 4.77 COL 33
     RECT-231 AT ROW 1.81 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 4 ROW 6.92
         SIZE 90 BY 15.08
         BGCOLOR 17 FONT 5
         TITLE "Información General del Producto".


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
         TITLE              = "SFG - Configuración de Productos de Crédito"
         HEIGHT             = 21.46
         WIDTH              = 113.14
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
ASSIGN FRAME F_Contabilidad:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_Deducibles:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_Generales:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_Intereses:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_Rangos:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_Restricciones:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_Varios:FRAME = FRAME F_Creditos:HANDLE.

/* SETTINGS FOR FRAME F_Contabilidad
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME F_Contabilidad:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_Creditos
                                                                        */
/* SETTINGS FOR FILL-IN Pro_Creditos.Cod_Credito IN FRAME F_Creditos
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR RADIO-SET Pro_Creditos.Estado IN FRAME F_Creditos
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Pro_Creditos.Fec_Matricula IN FRAME F_Creditos
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Pro_Creditos.Fec_Retiro IN FRAME F_Creditos
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Deducibles
   NOT-VISIBLE                                                          */
/* BROWSE-TAB Brw_Deducibles 1 F_Deducibles */
ASSIGN 
       FRAME F_Deducibles:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_Generales
                                                                        */
/* SETTINGS FOR TOGGLE-BOX Pro_Creditos.Id_AprobAgencia IN FRAME F_Generales
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR TOGGLE-BOX Pro_Creditos.Id_Consecutivo IN FRAME F_Generales
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Pro_Creditos.Id_NumAlterno IN FRAME F_Generales
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Pro_Creditos.Per_GarPer IN FRAME F_Generales
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Pro_Creditos.Per_GarRea IN FRAME F_Generales
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Pro_Creditos.Prioridad IN FRAME F_Generales
   2 EXP-FORMAT                                                         */
/* SETTINGS FOR FRAME F_Intereses
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Intereses:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN Pro_Creditos.Cod_Tasa IN FRAME F_Intereses
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Pro_Creditos.Cod_TasaMora IN FRAME F_Intereses
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN nomIndicadorMax IN FRAME F_Intereses
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Nom_IndicadorMora IN FRAME F_Intereses
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Nom_IndicadorTasa IN FRAME F_Intereses
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ValorTasaMax IN FRAME F_Intereses
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Valor_Tasa IN FRAME F_Intereses
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Valor_TasaMora IN FRAME F_Intereses
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Rangos
                                                                        */
/* SETTINGS FOR FRAME F_Restricciones
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Restricciones:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN Pro_Creditos.Val_Montomaximo IN FRAME F_Restricciones
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN Pro_Creditos.Val_Montominimo IN FRAME F_Restricciones
   EXP-FORMAT                                                           */
/* SETTINGS FOR FRAME F_Varios
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Varios:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN Pro_Creditos.Dia_Gracia IN FRAME F_Varios
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Pro_Creditos.Id_CreProAhorro IN FRAME F_Varios
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Pro_Creditos.Id_Debito IN FRAME F_Varios
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Pro_Creditos.Id_Extracto IN FRAME F_Varios
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Pro_Creditos.Id_Linea IN FRAME F_Varios
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Pro_Creditos.Id_PagParcial IN FRAME F_Varios
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Pro_Creditos.Id_PerGracia IN FRAME F_Varios
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Pro_Creditos.Id_Sorteos IN FRAME F_Varios
   EXP-LABEL                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Brw_Deducibles
/* Query rebuild information for BROWSE Brw_Deducibles
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Tmp NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE Brw_Deducibles */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Creditos
/* Query rebuild information for FRAME F_Creditos
     _Query            is NOT OPENED
*/  /* FRAME F_Creditos */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* SFG - Configuración de Productos de Crédito */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* SFG - Configuración de Productos de Crédito */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Creditos
&Scoped-define SELF-NAME Btn_Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancelar wWin
ON CHOOSE OF Btn_Cancelar IN FRAME F_Creditos /* Cancelar */
DO:
  W_Nuevo = NO.
  DISABLE Pro_creditos.cod_credito WITH FRAME F_Creditos.
  FIND Pro_Creditos WHERE ROWID(Pro_Creditos) EQ Puntero NO-ERROR.
  RUN Mostrar_Producto.  

  Button-122:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_contabilizacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_contabilizacion wWin
ON CHOOSE OF Btn_contabilizacion IN FRAME F_Creditos /* Contabilización */
DO:
  HIDE FRAME F_Generales FRAME F_Varios FRAME F_Intereses FRAME F_Deducibles FRAME F_Contabilidad.
  VIEW FRAME F_Contabilidad.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Deducibles
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Deducibles wWin
ON CHOOSE OF Btn_Deducibles IN FRAME F_Creditos /* Deducibles */
DO:
  DEFINE VAR j AS INTEGER.
  HIDE FRAME F_Generales FRAME F_Varios FRAME F_Intereses FRAME F_Deducibles FRAME F_Contabilidad.
  VIEW FRAME F_Deducibles.

  MESSAGE "Recuerde que si esta Ingresando un Nuevo Producto, Debe Salvar primero," SKIP
           "para poder ingresarle Deducibles..."
      VIEW-AS ALERT-BOX TITLE "INFORMATIVO".
 /* FOR EACH Tmp: i = i + 1. END.
  j = 10 - i.
  MESSAGE j i.
  DO i = 1 TO J BY 1:
     CREATE Tmp.
  END.
  OPEN QUERY Brw_Deducibles FOR EACH Tmp NO-LOCK INDEXED-REPOSITION.*/
  APPLY "entry" TO Tmp.TCodDed IN BROWSE Brw_Deducibles.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Deshacer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Deshacer wWin
ON CHOOSE OF Btn_Deshacer IN FRAME F_Creditos /* Deshacer */
DO:
  W_Nuevo = NO.
  DISABLE Pro_creditos.cod_credito WITH FRAME F_Creditos.
  FIND Pro_Creditos WHERE ROWID(Pro_Creditos) EQ Puntero NO-ERROR.
  RUN Mostrar_Producto.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Generales
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Generales wWin
ON CHOOSE OF Btn_Generales IN FRAME F_Creditos /* Generales */
DO:
  HIDE FRAME F_Generales FRAME F_Varios FRAME F_Intereses FRAME F_Deducibles FRAME F_Contabilidad.
  VIEW FRAME F_Generales.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Ingresar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ingresar wWin
ON CHOOSE OF Btn_Ingresar IN FRAME F_Creditos /* Ingresar */
DO:
  DEFINE VARIABLE W_Ultimo AS INTEGER.
  ENABLE Pro_creditos.cod_credito WITH FRAME F_Creditos.

  ASSIGN Puntero = ROWID(Pro_Creditos)
         W_Nuevo = YES
         Button-122:SENSITIVE = FALSE.

  RELEASE Pro_creditos.

  RUN Inicializar_Variables.
  HIDE FRAME F_Generales FRAME F_Varios FRAME F_Intereses FRAME F_Deducibles FRAME F_Contabilidad.
  VIEW FRAME F_Generales.
  APPLY "entry" TO Pro_creditos.cod_credito IN FRAME F_Creditos.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Intereses
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Intereses wWin
ON CHOOSE OF Btn_Intereses IN FRAME F_Creditos /* Intereses */
DO:
  HIDE FRAME F_Generales FRAME F_Varios FRAME F_Intereses FRAME F_Deducibles FRAME F_Contabilidad.
  VIEW FRAME F_Intereses.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Rangos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Rangos wWin
ON CHOOSE OF Btn_Rangos IN FRAME F_Creditos /* Rangos */
DO:
  VIEW FRAME F_Rangos.
  FRAME F_Rangos:MOVE-TO-TOP().
  FIND FIRST Varios WHERE Varios.Tipo = 39 NO-LOCK NO-ERROR.
  IF AVAIL Varios THEN
     ASSIGN WAnos:SCREEN-VALUE  = STRING(Varios.Codigo)
            WTasa:SCREEN-VALUE  = STRING(Varios.Val_Inicial)
            WVeces:SCREEN-VALUE = STRING(Varios.Val_Final).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Rangos
&Scoped-define SELF-NAME Btn_Rangos1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Rangos1 wWin
ON CHOOSE OF Btn_Rangos1 IN FRAME F_Rangos /* Salvar */
DO:
  FIND Varios WHERE Varios.Tipo = 39 AND Varios.Codigo =
       INT(WAnos:SCREEN-VALUE) EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL varios THEN DO:
     CREATE Varios.
     ASSIGN Varios.Tipo = 39 Varios.Codigo = INT(WAnos:SCREEN-VALUE).
  END.
  ASSIGN Varios.Val_Inicial = DEC(WTasa:SCREEN-VALUE)
         Varios.Val_Final   = DEC(WVeces:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Rangos2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Rangos2 wWin
ON CHOOSE OF Btn_Rangos2 IN FRAME F_Rangos /* Btn_Rangos2 */
DO:
  DEF VAR V_Cod AS INT NO-UNDO.
  DEF VAR V_Nom AS CHA NO-UNDO.
  RUN C-Varios.r (39,OUTPUT V_Cod,OUTPUT V_Nom).
  FIND Varios WHERE Varios.Tipo = 39 AND Varios.Codigo = V_Cod NO-LOCK NO-ERROR.
  IF AVAIL Varios THEN
     ASSIGN WAnos:SCREEN-VALUE  = STRING(Varios.Codigo)
            WTasa:SCREEN-VALUE  = STRING(Varios.Val_Inicial)
            WVeces:SCREEN-VALUE = STRING(Varios.Val_Final).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Creditos
&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir wWin
ON CHOOSE OF Btn_Salir IN FRAME F_Creditos /* Salir */
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


&Scoped-define SELF-NAME Btn_Salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salvar wWin
ON CHOOSE OF Btn_Salvar IN FRAME F_Creditos /* Salvar */
DO:
  IF W_Nuevo THEN 
     CREATE Pro_Creditos.
  ELSE FIND CURRENT Pro_creditos NO-ERROR.

  W_Nuevo = NO.
  RUN Grabar_Producto.
  DISABLE Pro_creditos.cod_credito  WITH FRAME F_Creditos.  

  Button-122:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Varios
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Varios wWin
ON CHOOSE OF Btn_Varios IN FRAME F_Creditos /* Varios */
DO:
  HIDE FRAME F_Generales FRAME F_Varios FRAME F_Intereses FRAME F_Deducibles FRAME F_Contabilidad.
  VIEW FRAME F_Varios.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-120
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-120 wWin
ON CHOOSE OF BUTTON-120 IN FRAME F_Creditos /* Button 120 */
DO:
  RUN V-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-121
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-121 wWin
ON CHOOSE OF BUTTON-121 IN FRAME F_Creditos /* Button 121 */
DO:
  DEFINE VAR Listado AS CHAR INITIAL "L_ProCre.LST".
  
  Listado = W_PathSpl + Listado.
  {INCLUIDO/Imprimir.I "Listado"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-122
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-122 wWin
ON CHOOSE OF BUTTON-122 IN FRAME F_Creditos /* Button 122 */
DO:
  ASSIGN WWin:SENSITIVE = FALSE.
    
  RUN C-ProCreditos.r (INPUT W_Agencia, OUTPUT P_Cod, OUTPUT p_Nombre, OUTPUT P_AgePro ).
  FIND Pro_Creditos WHERE Pro_creditos.cod_credito EQ P_Cod NO-ERROR.
  IF AVAILABLE Pro_Creditos THEN 
     RUN Mostrar_Producto.
    
  ASSIGN WWin:SENSITIVE = TRUE.
  WWin:MOVE-TO-TOP().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-132
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-132 wWin
ON CHOOSE OF BUTTON-132 IN FRAME F_Creditos /* Restricciones */
DO:
  HIDE FRAME F_Generales FRAME F_Varios FRAME F_Intereses FRAME F_Deducibles FRAME F_Contabilidad FRAME F_Restricciones.
  VIEW FRAME F_Restricciones.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Varios
&Scoped-define SELF-NAME Cmb_ForChequeo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_ForChequeo wWin
ON VALUE-CHANGED OF Cmb_ForChequeo IN FRAME F_Varios /* Digito de Chequeo */
DO:
/*  IF W_SGiro AND W_DChequeo:SCREEN-VALUE NE " " THEN DO:
     MESSAGE "Es un Producto para Sobregiro" VIEW-AS ALERT-BOX.
     ASSIGN W_DChequeo:SCREEN-VALUE = " ".
  END.
  ASSIGN W_DChequeo.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Generales
&Scoped-define SELF-NAME Cmb_ForPagare
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_ForPagare wWin
ON VALUE-CHANGED OF Cmb_ForPagare IN FRAME F_Generales /* Pagare */
DO:
  /*ASSIGN W_CodFor = INTEGER(SUBSTRING(SELF:SCREEN-VALUE IN FRAME {&FRAME-NAME},1,2)).*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_ForRetencion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_ForRetencion wWin
ON VALUE-CHANGED OF Cmb_ForRetencion IN FRAME F_Generales /* Retención */
DO:
  /*ASSIGN W_CodRet = INTEGER(SUBSTRING(SELF:SCREEN-VALUE IN FRAME {&FRAME-NAME},1,2)).*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Varios
&Scoped-define SELF-NAME Cmb_ForTalonario
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_ForTalonario wWin
ON VALUE-CHANGED OF Cmb_ForTalonario IN FRAME F_Varios /* Formato */
DO:
  /*ASSIGN W_CodForTalo = INTEGER(SUBSTRING(SELF:SCREEN-VALUE IN FRAME {&FRAME-NAME},1,2)).*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Generales
&Scoped-define SELF-NAME Cmb_ProAhorros
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_ProAhorros wWin
ON VALUE-CHANGED OF Cmb_ProAhorros IN FRAME F_Generales /* Productos de Ahorros */
DO:
  /*  IF INTEGER(SUBSTRING((W_CProAho:SCREEN-VALUE),1,3)) NE 0 THEN
       W_ProAho = INTEGER(SUBSTRING((W_CProAho:SCREEN-VALUE),1,3)).  
    ELSE
       W_ProAho = 0.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Varios
&Scoped-define SELF-NAME Cmb_ProMultiplicador
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_ProMultiplicador wWin
ON VALUE-CHANGED OF Cmb_ProMultiplicador IN FRAME F_Varios /* Ahorro */
DO:
  /*ASSIGN W_CPromul.
  IF SUBSTRING(W_CProMul,1,3) LE "000" THEN DO:
     ASSIGN Pro_Creditos.Multiplicador:SCREEN-VALUE   = "000"
            Pro_Creditos.Id_CreProAhorro:SCREEN-VALUE = "no"
            W_ProAhoM                                 = 0
            W_Multip                                  = 0.
     HIDE Pro_creditos.Multiplicador W_CProMul.
     APPLY "ENTRY" TO Pro_Creditos.Id_CreProAhorro.
     RETURN NO-APPLY.  
  END.*/
  /*ASSIGN W_ProAhoM = SELF:LOOKUP(SELF:SCREEN-VALUE) IN FRAME {&FRAME-NAME}
         W_ProAhoM = INTEGER(SUBSTRING(SELF:ENTRY(W_ProAhoM),1,3)).*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Intereses
&Scoped-define SELF-NAME Pro_Creditos.Cod_Tasa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Creditos.Cod_Tasa wWin
ON LEAVE OF Pro_Creditos.Cod_Tasa IN FRAME F_Intereses /* Indicador de Intereses */
DO:
DO WITH FRAME F_Intereses:
   FIND Indicadores WHERE Indicadores.Indicador EQ INTEGER(SELF:SCREEN-VALUE) NO-LOCK NO-ERROR.
   IF AVAILABLE Indicadores THEN DO:
      ASSIGN Nom_IndicadorTasa:SCREEN-VALUE = Indicadores.Nombre.
             Valor_Tasa:SCREEN-VALUE        = STRING(Indicadores.Tasa).
   END.
   ELSE DO:
      RUN C-Indicadores.r (OUTPUT P_CodInd, OUTPUT P_NomInd, OUTPUT P_ValTas, OUTPUT P_Valor).
      ASSIGN Pro_Creditos.Cod_Tasa:SCREEN-VALUE   = STRING(P_CodInd).
             Nom_IndicadorTasa:SCREEN-VALUE       = P_NomInd.
             Valor_Tasa:SCREEN-VALUE              = STRING(P_ValTas).
   END.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Creditos.Cod_Tasa wWin
ON MOUSE-SELECT-DBLCLICK OF Pro_Creditos.Cod_Tasa IN FRAME F_Intereses /* Indicador de Intereses */
DO:
   RUN C-Indicadores.r (OUTPUT P_CodInd, OUTPUT P_NomInd, OUTPUT P_ValTas, OUTPUT P_Valor).
   ASSIGN Pro_Creditos.Cod_Tasa:SCREEN-VALUE   = STRING(P_CodInd)
          Nom_IndicadorTasa:SCREEN-VALUE       = P_NomInd
          Valor_Tasa:SCREEN-VALUE              = STRING(P_ValTas).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Pro_Creditos.Cod_TasaMax
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Creditos.Cod_TasaMax wWin
ON LEAVE OF Pro_Creditos.Cod_TasaMax IN FRAME F_Intereses /* Código de Tasa Máxima */
DO:
DO WITH FRAME F_Intereses:
   FIND Indicadores WHERE Indicadores.Indicador EQ INTEGER(SELF:SCREEN-VALUE) NO-LOCK NO-ERROR.
   IF AVAILABLE Indicadores THEN DO:
      ASSIGN NomIndicadorMax:SCREEN-VALUE = Indicadores.Nombre.
             ValorTasaMax:SCREEN-VALUE        = STRING(Indicadores.Tasa).
   END.
   ELSE DO:
      RUN C-Indicadores.r (OUTPUT P_CodInd, OUTPUT P_NomInd, OUTPUT P_ValTas, OUTPUT P_Valor).
      ASSIGN Pro_Creditos.Cod_TasaMax:SCREEN-VALUE   = STRING(P_CodInd).
             NomIndicadorMax:SCREEN-VALUE       = P_NomInd.
             ValorTasaMax:SCREEN-VALUE          = STRING(P_ValTas).
   END.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Pro_Creditos.Cod_TasaMora
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Creditos.Cod_TasaMora wWin
ON LEAVE OF Pro_Creditos.Cod_TasaMora IN FRAME F_Intereses /* Indicador Interés de Mora */
OR RETURN OF Pro_Creditos.Id_Tasa DO:
DO WITH FRAME F_Intereses:
   FIND Indicadores WHERE Indicadores.Indicador EQ INTEGER(SELF:SCREEN-VALUE) NO-LOCK NO-ERROR.
   IF AVAILABLE Indicadores THEN
      ASSIGN Nom_IndicadorMora:SCREEN-VALUE = Indicadores.Nombre
             Valor_TasaMora:SCREEN-VALUE    = STRING(Indicadores.Tasa).
   ELSE DO:
      RUN C-Indicadores.r (OUTPUT P_CodInd, OUTPUT P_NomInd, OUTPUT P_ValTas, OUTPUT P_Valor).
      ASSIGN Pro_Creditos.Cod_TasaMora:SCREEN-VALUE   = STRING(P_CodInd)
             Nom_IndicadorMora:SCREEN-VALUE           = P_NomInd
             Valor_TasaMora:SCREEN-VALUE              = STRING(P_ValTas).
   END.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Creditos.Cod_TasaMora wWin
ON MOUSE-SELECT-DBLCLICK OF Pro_Creditos.Cod_TasaMora IN FRAME F_Intereses /* Indicador Interés de Mora */
DO:
   RUN C-Indicadores.r (OUTPUT P_CodInd, OUTPUT P_NomInd, OUTPUT P_ValTas, OUTPUT P_Valor).
   ASSIGN Pro_Creditos.Cod_TasaMora:SCREEN-VALUE   = STRING(P_CodInd)
          Nom_IndicadorMora:SCREEN-VALUE           = P_NomInd
          Valor_TasaMora:SCREEN-VALUE              = STRING(P_ValTas).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Generales
&Scoped-define SELF-NAME Pro_Creditos.Consecutivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Creditos.Consecutivo wWin
ON LEAVE OF Pro_Creditos.Consecutivo IN FRAME F_Generales /* Valor Inicial del Consecutivo */
DO:
/*  IF W_SGiro AND W_ClaProC EQ 1 THEN DO:
     MESSAGE "Es un Producto para Sobregiro" VIEW-AS ALERT-BOX.
     ASSIGN Pro_Creditos.Id_consecutivo:SCREEN-VALUE IN FRAME {&Frame-Name} = "no".
     APPLY "VALUE-CHANGED" TO Pro_Creditos.Id_consecutivo.
     RETURN NO-APPLY.
  END.

  IF INTEGER(Pro_Creditos.Consecutivo:SCREEN-VALUE IN FRAME {&Frame-Name}) LT Pro_Creditos.Consecutivo 
  OR INTEGER(Pro_Creditos.Consecutivo:SCREEN-VALUE IN FRAME {&Frame-Name}) LE 0 THEN DO:
     W_Mani = LAST-EVENT:WIDGET-ENTER.
     IF W_Mani:LABEL = "Maneja Consecutivo" THEN 
        RETURN.
     ELSE DO:  
        RUN MostrarMensaje IN W_Manija (INPUT 182, OUTPUT W_Rpta).
        DISPLAY Pro_Creditos.Consecutivo WITH FRAME {&Frame-Name}.
        APPLY "ENTRY" TO Pro_Creditos.Consecutivo.
        /*RETURN NO-APPLY.*/
     END.
  END.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Pro_Creditos.Id_AsociaCta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Creditos.Id_AsociaCta wWin
ON VALUE-CHANGED OF Pro_Creditos.Id_AsociaCta IN FRAME F_Generales /* Asocia Cuenta */
DO:
DO WITH FRAME F_Generales:
  IF Pro_Creditos.Id_AsociaCta:SCREEN-VALUE EQ "yes" THEN DO:
     ENABLE Cmb_ProAhorros.
     APPLY "entry" TO Cmb_ProAhorros.
     RETURN NO-APPLY.
  END.
  ELSE DO: 
    DISABLE Cmb_ProAhorros.
    Cmb_ProAhorros:SCREEN-VALUE = Cmb_ProAhorros:ENTRY(1).
  END.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Pro_Creditos.Id_Consecutivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Creditos.Id_Consecutivo wWin
ON VALUE-CHANGED OF Pro_Creditos.Id_Consecutivo IN FRAME F_Generales /* Consecutivo */
DO:
DO WITH FRAME F_Generales:
  IF Pro_Creditos.Id_Consecutivo:SCREEN-VALUE EQ "yes" THEN DO:
     ENABLE Pro_Creditos.Consecutivo.
     APPLY "entry" TO Pro_Creditos.Consecutivo.
     RETURN NO-APPLY.
  END.
  ELSE DISABLE Pro_Creditos.Consecutivo.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Varios
&Scoped-define SELF-NAME Pro_Creditos.Id_CreProAhorro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Creditos.Id_CreProAhorro wWin
ON VALUE-CHANGED OF Pro_Creditos.Id_CreProAhorro IN FRAME F_Varios /* Base Aprobación */
DO:
DO WITH FRAME F_Varios:
  IF SELF:SCREEN-VALUE EQ "No" THEN DO: 
     DISABLE Cmb_ProMultiplicador Pro_Creditos.Multiplicador.
     ASSIGN Cmb_ProMultiplicador:SCREEN-VALUE = Cmb_ProMultiplicador:ENTRY(1)
            Pro_Creditos.Multiplicador:SCREEN-VALUE = "0".
  END.
  ELSE DO:
    ENABLE Cmb_ProMultiplicador Pro_Creditos.Multiplicador.
    APPLY "entry" TO Pro_Creditos.Multiplicador.
  END.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Pro_Creditos.Id_Debito
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Creditos.Id_Debito wWin
ON VALUE-CHANGED OF Pro_Creditos.Id_Debito IN FRAME F_Varios /* Maneja Débito Automático */
DO:
  /*IF W_SGiro AND Pro_Creditos.Id_Debito:SCREEN-VALUE NE "no" THEN DO:
     MESSAGE "Es un Producto para Sobregiro" VIEW-AS ALERT-BOX.
     ASSIGN Pro_Creditos.Id_Debito:SCREEN-VALUE = "no".
  END.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Pro_Creditos.Id_Extracto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Creditos.Id_Extracto wWin
ON VALUE-CHANGED OF Pro_Creditos.Id_Extracto IN FRAME F_Varios /* Es Transitorio */
DO:
  /*IF W_SGiro AND Pro_Creditos.Id_Extracto:SCREEN-VALUE NE "no" THEN DO:
     MESSAGE "Es un Producto para Sobregiro" VIEW-AS ALERT-BOX.
     ASSIGN Pro_Creditos.Id_Extracto:SCREEN-VALUE = "no".
  END.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Generales
&Scoped-define SELF-NAME Pro_Creditos.Id_Formato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Creditos.Id_Formato wWin
ON VALUE-CHANGED OF Pro_Creditos.Id_Formato IN FRAME F_Generales /* Maneja Formato */
DO:
DO WITH FRAME F_Generales:
  IF Pro_Creditos.Id_Formato:SCREEN-VALUE EQ "yes" THEN DO:
     ENABLE Cmb_ForPagare Cmb_ForRetencion.
     APPLY "entry" TO Cmb_ForPagare.
     RETURN NO-APPLY.
  END.
  ELSE DO: 
    DISABLE Cmb_ForPagare Cmb_ForRetencion.
    ASSIGN Cmb_ForPagare:SCREEN-VALUE    = Cmb_ForPagare:ENTRY(1)
           Cmb_ForRetencion:SCREEN-VALUE = Cmb_ForRetencion:ENTRY(1).
  END.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Varios
&Scoped-define SELF-NAME Pro_Creditos.Id_Linea
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Creditos.Id_Linea wWin
ON VALUE-CHANGED OF Pro_Creditos.Id_Linea IN FRAME F_Varios /* Maneja Línea */
DO:
  /*IF W_SGiro AND Pro_Creditos.Id_Linea:SCREEN-VALUE NE "no" THEN DO:
     MESSAGE "Es un Producto para Sobregiro" VIEW-AS ALERT-BOX.
     ASSIGN Pro_Creditos.Id_Linea:SCREEN-VALUE = "no".
  END.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Restricciones
&Scoped-define SELF-NAME Pro_Creditos.Id_Montomaximo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Creditos.Id_Montomaximo wWin
ON VALUE-CHANGED OF Pro_Creditos.Id_Montomaximo IN FRAME F_Restricciones /* Maneja Monto Maximo */
DO:
DO WITH FRAME F_Restricciones:
  IF SELF:SCREEN-VALUE EQ "yes" THEN DO:
     ENABLE Pro_Creditos.Val_MontoMaximo.
     APPLY "entry" TO Pro_Creditos.Val_MontoMaximo.
     RETURN NO-APPLY.
  END.
  ELSE DO:
     DISABLE Pro_Creditos.Val_MontoMaximo.
     ASSIGN Pro_Creditos.Val_MontoMaximo:SCREEN-VALUE = "0".
     APPLY "entry" TO Pro_Creditos.Id_MontoMinimo.
     RETURN NO-APPLY.
  END.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Pro_Creditos.Id_Montominimo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Creditos.Id_Montominimo wWin
ON VALUE-CHANGED OF Pro_Creditos.Id_Montominimo IN FRAME F_Restricciones /* Maneja Monto Minimo */
DO:
DO WITH FRAME F_Restricciones:
  IF SELF:SCREEN-VALUE EQ "yes" THEN DO:
     ENABLE Pro_Creditos.Val_MontoMinimo.
     APPLY "entry" TO Pro_Creditos.Val_MontoMinimo.
     RETURN NO-APPLY.
  END.
  ELSE DO:
     DISABLE Pro_Creditos.Val_MontoMinimo.
     ASSIGN Pro_Creditos.Val_MontoMinimo:SCREEN-VALUE = "0".
     APPLY "entry" TO Pro_Creditos.Id_Plazo.
     RETURN NO-APPLY.
  END.
END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Generales
&Scoped-define SELF-NAME Pro_Creditos.Id_NumAlterno
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Creditos.Id_NumAlterno wWin
ON VALUE-CHANGED OF Pro_Creditos.Id_NumAlterno IN FRAME F_Generales /* Requiere Número Alterno */
DO:
/* DO WITH FRAME {&FRAME-NAME}:
  IF W_SGiro AND W_ClaProC EQ 1 AND Pro_Creditos.Id_NumAlterno:SCREEN-VALUE EQ "si" 
  THEN DO:
         MESSAGE "Es un Producto para Sobregiro" VIEW-AS ALERT-BOX.
         ASSIGN Pro_Creditos.Id_NumAlterno:SCREEN-VALUE = "no".
       END.
 END.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Varios
&Scoped-define SELF-NAME Pro_Creditos.Id_PagParcial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Creditos.Id_PagParcial wWin
ON VALUE-CHANGED OF Pro_Creditos.Id_PagParcial IN FRAME F_Varios /* Respeta Liquidación Por Proyección */
DO:
  /*IF W_SGiro THEN DO:
     IF Pro_Creditos.Id_PagParcial:SCREEN-VALUE NE "yes" THEN
        MESSAGE "Es un Producto para Sobregiro" VIEW-AS ALERT-BOX.
     ASSIGN Pro_Creditos.Id_PagParcial:SCREEN-VALUE = "NO".
  END.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Pro_Creditos.Id_PerGracia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Creditos.Id_PerGracia wWin
ON VALUE-CHANGED OF Pro_Creditos.Id_PerGracia IN FRAME F_Varios /* Manejo de Período de Gracia */
DO:
DO WITH FRAME F_Varios:
   IF SELF:SCREEN-VALUE EQ "yes" THEN DO:
      ENABLE Pro_Creditos.Dia_Gracia.
      APPLY "entry" TO Pro_Creditos.Dia_Gracia.
   END.
   ELSE DO:
     DISABLE Pro_Creditos.Dia_Gracia.
     Pro_Creditos.Dia_Gracia:SCREEN-VALUE = "0".
   END.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Restricciones
&Scoped-define SELF-NAME Pro_Creditos.Id_Plazo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Creditos.Id_Plazo wWin
ON VALUE-CHANGED OF Pro_Creditos.Id_Plazo IN FRAME F_Restricciones /* Maneja Plazo */
DO:
DO WITH FRAME F_Restricciones:
  IF SELF:SCREEN-VALUE EQ "yes" THEN DO:
     ENABLE Pro_Creditos.Pla_Maximo Pro_Creditos.Pla_Minimo.
     APPLY "entry" TO Pro_Creditos.Pla_Minimo.
     RETURN NO-APPLY.
  END.
  ELSE DO:
     DISABLE Pro_Creditos.Pla_Maximo Pro_Creditos.Pla_Minimo.
     ASSIGN Pro_Creditos.Pla_Maximo:SCREEN-VALUE = "0"
            Pro_Creditos.Pla_Minimo:SCREEN-VALUE = "0".
  END.
END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Varios
&Scoped-define SELF-NAME Pro_Creditos.Id_Sorteos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Creditos.Id_Sorteos wWin
ON VALUE-CHANGED OF Pro_Creditos.Id_Sorteos IN FRAME F_Varios /* X Libranza(Nómina) */
DO:
  /*IF W_SGiro AND Pro_Creditos.Id_Sorteos:SCREEN-VALUE NE "no" THEN DO:
     MESSAGE "Es un Producto para Sobregiro" VIEW-AS ALERT-BOX.
     ASSIGN Pro_Creditos.Id_Sorteos:SCREEN-VALUE = "no".
  END.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Pro_Creditos.Id_Talonario
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Creditos.Id_Talonario wWin
ON VALUE-CHANGED OF Pro_Creditos.Id_Talonario IN FRAME F_Varios /* Maneja Talonario */
DO:
DO WITH FRAME F_Varios:
  IF SELF:SCREEN-VALUE EQ "1" THEN DO: 
     DISABLE Cmb_ForTalonario.
     Cmb_ForTalonario:SCREEN-VALUE = Cmb_ForTalonario:ENTRY(1).
  END.
  ELSE DO:
    ENABLE Cmb_ForTalonario.
    APPLY "entry" TO Cmb_ForTalonario.
  END.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Intereses
&Scoped-define SELF-NAME Pro_Creditos.Id_Tasa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Creditos.Id_Tasa wWin
ON VALUE-CHANGED OF Pro_Creditos.Id_Tasa IN FRAME F_Intereses /* Manejo de Tasa */
OR RETURN OF Pro_Creditos.Id_Tasa DO:
DO WITH FRAME F_Intereses:
   IF SELF:SCREEN-VALUE EQ "1" THEN DO:
      ENABLE Pro_Creditos.Cod_Tasa.
      APPLY "entry" TO Pro_Creditos.Cod_Tasa.
   END.
   ELSE DO:
     DISABLE Pro_Creditos.Cod_Tasa.
     ASSIGN  Pro_Creditos.Cod_Tasa:SCREEN-VALUE = "0"
             Nom_IndicadorTasa:SCREEN-VALUE     = ""
             Valor_Tasa:SCREEN-VALUE            = "0".
     APPLY "entry" TO Pro_Creditos.Cod_TasaMora.
   END.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Varios
&Scoped-define SELF-NAME Pro_Creditos.Multiplicador
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Creditos.Multiplicador wWin
ON LEAVE OF Pro_Creditos.Multiplicador IN FRAME F_Varios /* Multiplicador */
DO:
  /*DO WITH FRAME {&Frame-Name}:  
     IF  Pro_Creditos.Multiplicador:SCREEN-VALUE   LE "000" 
     AND Pro_Creditos.Id_CreProAhorro:SCREEN-VALUE EQ "yes" THEN DO:
         ASSIGN Pro_Creditos.Multiplicador:SCREEN-VALUE   = "000"
                Pro_Creditos.Id_CreProAhorro:SCREEN-VALUE = "no"
                W_ProAhoM                                 = 0
                W_Multip                                  = 0.
         HIDE Pro_creditos.Multiplicador W_CProMul.
         APPLY "ENTRY" TO Pro_Creditos.Id_CreProAhorro.
         RETURN NO-APPLY.
     END.
     ASSIGN W_Multip = INTEGER(Pro_Creditos.Multiplicador:SCREEN-VALUE).
     APPLY "ENTRY" TO W_CPromul.
     RETURN NO-APPLY.
  END.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Restricciones
&Scoped-define SELF-NAME Pro_Creditos.Val_Montomaximo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Creditos.Val_Montomaximo wWin
ON LEAVE OF Pro_Creditos.Val_Montomaximo IN FRAME F_Restricciones /* Valor Monto Maximo */
DO:
    SELF:SCREEN-VALUE = STRING(ROUND(DECIMAL(SELF:SCREEN-VALUE),0)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Pro_Creditos.Val_Montominimo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Creditos.Val_Montominimo wWin
ON LEAVE OF Pro_Creditos.Val_Montominimo IN FRAME F_Restricciones /* Valor Monto Mínimo */
DO:
    SELF:SCREEN-VALUE = STRING(ROUND(DECIMAL(SELF:SCREEN-VALUE),0)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Rangos
&Scoped-define SELF-NAME WAnos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL WAnos wWin
ON LEAVE OF WAnos IN FRAME F_Rangos
DO:
  FIND Varios WHERE Varios.Tipo = 39 AND
       Varios.Codigo = int(WAnos:SCREEN-VALUE) NO-LOCK NO-ERROR.
  IF AVAIL Varios THEN ASSIGN WTasa:SCREEN-VALUE  = STRING(Varios.Val_Inicial)
                              WVeces:SCREEN-VALUE = STRING(Varios.Val_Final).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Contabilidad
&Scoped-define BROWSE-NAME Brw_Deducibles
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */
DO WITH FRAME F_Deducibles:
ON 'mouse-select-dblclick':U OF Tmp.TCodDed IN BROWSE Brw_Deducibles
DO:
 RUN Buscar_Deducible.
 OPEN QUERY Brw_Deducibles FOR EACH Tmp NO-LOCK INDEXED-REPOSITION.
END.

ON 'leave':U OF Tmp.TCodDed IN BROWSE Brw_Deducibles
  DO:
    IF Tmp.TCodDed NE "" OR Tmp.TCodDed NE "?" THEN DO:
       FIND Deducible WHERE Deducible.Cod_Deducible EQ Tmp.TCodDed NO-LOCK NO-ERROR.
       IF AVAILABLE Deducible THEN DO:
          ASSIGN Tmp.TNomDed = Deducible.Nom_Deducible
                 Tmp.TValDed = Deducible.Valor.
          OPEN QUERY Brw_Deducibles FOR EACH Tmp NO-LOCK INDEXED-REPOSITION.
       END.
       ELSE DO:
         MESSAGE "No existe el Deducible, si quiere asignar un deducible" SKIP
                 "haga doble click sobre el campo" VIEW-AS ALERT-BOX.
       END.
    END.
    ELSE /*IF AVAIL(Tmp) THEN*/ DO:
      MESSAGE "si avail"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
       FIND CURRENT Tmp NO-ERROR.
       DELETE Tmp.
       CLOSE QUERY Brw_Deducibles.
       OPEN QUERY Brw_Deducibles FOR EACH Tmp NO-LOCK INDEXED-REPOSITION.
    END.
  END.
END.

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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Bor_RegPCre wWin 
PROCEDURE Bor_RegPCre :
FOR EACH RegPCre: 
      DELETE RegPCre. 
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Buscar_Deducible wWin 
PROCEDURE Buscar_Deducible :
RUN C-Deducibles.r(OUTPUT W_Deducible,OUTPUT W_NomDeducible).
   FIND Deducible WHERE Deducible.Cod_Deducible EQ W_Deducible NO-LOCK NO-ERROR.
   IF AVAILABLE Deducible THEN
      ASSIGN Tmp.TCodDed = W_Deducible
             Tmp.TNomDed = W_NomDeducible
             Tmp.TValDed = Deducible.Valor.
   ELSE DO:
      MESSAGE "No existe el deducible...Será Eliminado."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      FIND CURRENT Tmp NO-ERROR.
      DELETE Tmp.
      CLOSE QUERY Brw_Deducibles.
      OPEN QUERY Brw_Deducibles FOR EACH Tmp NO-LOCK INDEXED-REPOSITION.
  END.
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
  IF AVAILABLE Pro_Creditos THEN 
    DISPLAY Pro_Creditos.Fec_Matricula Pro_Creditos.Cod_Credito 
          Pro_Creditos.Nom_Producto Pro_Creditos.Fec_Retiro Pro_Creditos.Estado 
          Pro_Creditos.Tip_Credito 
      WITH FRAME F_Creditos IN WINDOW wWin.
  ENABLE RECT-237 RECT-280 RECT-281 RECT-282 BUTTON-120 
         Pro_Creditos.Nom_Producto BUTTON-121 Pro_Creditos.Tip_Credito 
         BUTTON-122 Btn_Generales Btn_Varios Btn_Intereses Btn_Deducibles 
         BUTTON-132 Btn_contabilizacion Btn_Rangos Btn_Salvar Btn_Deshacer 
         Btn_Ingresar Btn_Borrar Btn_Cancelar Btn_Salir BUTTON-130 
      WITH FRAME F_Creditos IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Creditos}
  {&OPEN-BROWSERS-IN-QUERY-F_Contabilidad}
  ENABLE Brw_Deducibles 
      WITH FRAME F_Deducibles IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Deducibles}
  DISPLAY Cmb_ProAhorros Cmb_ForPagare Cmb_ForRetencion 
      WITH FRAME F_Generales IN WINDOW wWin.
  IF AVAILABLE Pro_Creditos THEN 
    DISPLAY Pro_Creditos.Prioridad Pro_Creditos.Per_GarPer 
          Pro_Creditos.Id_Asociado Pro_Creditos.Per_GarRea 
          Pro_Creditos.Id_NumAlterno Pro_Creditos.Id_Garantia 
          Pro_Creditos.Id_AsociaCta Pro_Creditos.Id_Consecutivo 
          Pro_Creditos.Consecutivo Pro_Creditos.Id_Formato 
          Pro_Creditos.Id_AprobAgencia Pro_Creditos.Descripcion 
      WITH FRAME F_Generales IN WINDOW wWin.
  ENABLE RECT-229 RECT-230 RECT-231 Pro_Creditos.Prioridad 
         Pro_Creditos.Per_GarPer Pro_Creditos.Id_Asociado 
         Pro_Creditos.Per_GarRea Pro_Creditos.Id_NumAlterno 
         Pro_Creditos.Id_Garantia Pro_Creditos.Id_AsociaCta 
         Pro_Creditos.Id_Consecutivo Cmb_ProAhorros Pro_Creditos.Consecutivo 
         Pro_Creditos.Id_Formato Pro_Creditos.Id_AprobAgencia Cmb_ForPagare 
         Cmb_ForRetencion Pro_Creditos.Descripcion 
      WITH FRAME F_Generales IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Generales}
  DISPLAY Nom_IndicadorTasa Valor_Tasa Nom_IndicadorMora Valor_TasaMora 
          nomIndicadorMax ValorTasaMax 
      WITH FRAME F_Intereses IN WINDOW wWin.
  IF AVAILABLE Pro_Creditos THEN 
    DISPLAY Pro_Creditos.Id_Tasa Pro_Creditos.Cod_Tasa Pro_Creditos.Cod_TasaMora 
          Pro_Creditos.Cod_TasaMax 
      WITH FRAME F_Intereses IN WINDOW wWin.
  ENABLE RECT-235 RECT-236 RECT-334 Pro_Creditos.Id_Tasa Pro_Creditos.Cod_Tasa 
         Pro_Creditos.Cod_TasaMora Pro_Creditos.Cod_TasaMax 
      WITH FRAME F_Intereses IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Intereses}
  IF AVAILABLE Pro_Creditos THEN 
    DISPLAY Pro_Creditos.Id_Montomaximo Pro_Creditos.Val_Montomaximo 
          Pro_Creditos.Id_Montominimo Pro_Creditos.Val_Montominimo 
          Pro_Creditos.Id_Plazo Pro_Creditos.Pla_Minimo Pro_Creditos.Pla_Maximo 
      WITH FRAME F_Restricciones IN WINDOW wWin.
  ENABLE RECT-283 Pro_Creditos.Id_Montomaximo Pro_Creditos.Val_Montomaximo 
         Pro_Creditos.Id_Montominimo Pro_Creditos.Val_Montominimo 
         Pro_Creditos.Id_Plazo Pro_Creditos.Pla_Minimo Pro_Creditos.Pla_Maximo 
      WITH FRAME F_Restricciones IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Restricciones}
  DISPLAY Cmb_ForChequeo Cmb_ForTalonario Cmb_ProMultiplicador 
      WITH FRAME F_Varios IN WINDOW wWin.
  IF AVAILABLE Pro_Creditos THEN 
    DISPLAY Pro_Creditos.Id_Linea Pro_Creditos.Id_Debito Pro_Creditos.Id_CreUnico 
          Pro_Creditos.Id_PagParcial Pro_Creditos.Id_Extracto 
          Pro_Creditos.Id_Sorteos Pro_Creditos.Id_PerGracia 
          Pro_Creditos.Dia_Gracia Pro_Creditos.Id_Talonario 
          Pro_Creditos.Id_CreProAhorro Pro_Creditos.Multiplicador 
      WITH FRAME F_Varios IN WINDOW wWin.
  ENABLE RECT-232 RECT-233 RECT-234 Pro_Creditos.Id_Linea Cmb_ForChequeo 
         Pro_Creditos.Id_Debito Pro_Creditos.Id_CreUnico 
         Pro_Creditos.Id_PagParcial Pro_Creditos.Id_Extracto 
         Pro_Creditos.Id_Sorteos Pro_Creditos.Id_PerGracia 
         Pro_Creditos.Dia_Gracia Cmb_ForTalonario Pro_Creditos.Id_Talonario 
         Pro_Creditos.Id_CreProAhorro Pro_Creditos.Multiplicador 
         Cmb_ProMultiplicador 
      WITH FRAME F_Varios IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Varios}
  DISPLAY WAnos WTasa WVeces 
      WITH FRAME F_Rangos IN WINDOW wWin.
  ENABLE RECT-285 RECT-286 RECT-287 WAnos Btn_Rangos2 WTasa Btn_Rangos1 WVeces 
      WITH FRAME F_Rangos IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Rangos}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Grabar_Producto wWin 
PROCEDURE Grabar_Producto :
DO WITH FRAME F_Creditos:
  ASSIGN FRAME F_Creditos
         Pro_creditos.cod_credito
         Pro_creditos.Tip_Credito
         Pro_Creditos.Nom_Producto
         Pro_Creditos.Fec_Matricula
         Pro_Creditos.Fec_Retiro   
         Pro_Creditos.Estado.       
END.                                 
DO WITH FRAME F_Generales:
  ASSIGN Pro_Creditos.Id_Asociado
         Pro_Creditos.Id_Garantia
         Pro_Creditos.Prioridad
         Pro_Creditos.Per_GarPer
         Pro_Creditos.Per_GarRea
         Pro_Creditos.Id_NumAlterno
         Pro_Creditos.Id_Consecutivo
         Pro_Creditos.Consecutivo
         Pro_Creditos.Id_AsociaCta
         Pro_Creditos.Id_Formato
         Pro_Creditos.Descripcion
         Pro_Creditos.Id_AprobAgencia.

   ASSIGN  Pro_Creditos.Cod_ForPagare  = INTEGER(SUBSTRING(Cmb_ForPAgare:SCREEN-VALUE,1,2))
           Pro_Creditos.Cod_ForRetencion  = INTEGER(SUBSTRING(Cmb_ForRetencion:SCREEN-VALUE,1,2))
           Pro_Creditos.Cod_ProAhorro  = INTEGER(SUBSTRING(Cmb_ProAhorros:SCREEN-VALUE,1,3)).
END.
DO WITH FRAME F_Varios:
  ASSIGN FRAME F_Varios
         Pro_Creditos.Id_Linea
         Pro_Creditos.Id_Debito
         Pro_Creditos.Id_CreUnico
         Pro_Creditos.Id_PagParcial
         Pro_Creditos.Id_Extracto
         Pro_Creditos.Id_Sorteos
         Pro_Creditos.Id_PerGracia
         Pro_Creditos.Id_Talonario
         Pro_Creditos.Id_CreProAhorro
         Pro_Creditos.Dia_Gracia
         Pro_Creditos.Multiplicador.
         
    ASSIGN Pro_Creditos.Cod_ForChequeo   = INTEGER(SUBSTRING(Cmb_ForChequeo:SCREEN-VALUE,1,2))
           Pro_Creditos.Cod_ForTalonario = INTEGER(SUBSTRING(Cmb_ForTalonario:SCREEN-VALUE,1,2))
           Pro_Creditos.Lin_Multiplicador = INTEGER(SUBSTRING(Cmb_ProMultiplicador:SCREEN-VALUE,1,3)).
END.
DO WITH FRAME F_Intereses:
   ASSIGN FRAME F_Intereses Pro_Creditos.Id_Tasa Pro_Creditos.Cod_Tasa Pro_Creditos.Cod_TasaMora Pro_Creditos.Cod_TasaMax.
END.

DO WITH FRAME F_Deducibles:
   i = 1.
   FOR EACH Tmp: 
      Pro_Creditos.Deducible[i] = Tmp.TCodDed.
      i = i + 1.
   END.
END.
 
DO WITH FRAME F_Restricciones:
 ASSIGN FRAME F_Restricciones 
    Pro_Creditos.Id_MontoMaximo Pro_Creditos.Val_MontoMaximo
    Pro_Creditos.Id_MontoMinimo Pro_Creditos.Val_MontoMinimo
    Pro_Creditos.Id_Plazo Pro_Creditos.Pla_Maximo Pro_Creditos.Pla_Minimo.
END.

FIND CURRENT Pro_Creditos NO-LOCK NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir_Excel wWin 
PROCEDURE Imprimir_Excel :
{Incluido\Def_Excel.i}
 /* MANDA ENCABEZADO DE HOJA DE CALCULO*/
 DEFINE VAR W_EstadoInf AS CHARACTER FORMAT "X(8)". 
 E_NumFila = 1.
 E_NumColumn = 5.
 E_Fila      = "003" + "Age"
             + "003" + "Pro"
             + "040" + "Nom_Producto                            "
             + "015" + "Tipo           "
             + "008" + "Estado  ".
    
 RUN W-GraExcel.w (INPUT E_Fila, INPUT E_NumColumn, OUTPUT E_CmpGrafic).

/* launch Excel so it is visible to the user */
chExcelApp:Visible = TRUE.

/* create a new Workbook */
chWorkbook = chExcelApp:Workbooks:Add().

/* get the active Worksheet */
chWorkSheet = chExcelApp:Sheets:Item(1).
DEFINE VAR tipo AS CHARACTER FORMAT "X(15)".
DEFINE VAR Est  AS CHARACTER FORMAT "X(8)".
    FOR EACH Pro_Creditos NO-LOCK BY Pro_creditos.cod_credito:
        CASE Pro_Creditos.Tip_Credito:
            WHEN 1 THEN
                    Tipo = "Consumo".
            WHEN 2 THEN
                    Tipo = "Comercial".
            WHEN 3 THEN
                    Tipo = "Hipotecario".
            WHEN 3 THEN
                    Tipo = "Microcredito".
        END CASE. 
        CASE Pro_Creditos.Estado:
            WHEN 1 THEN
                    Est = "Activo".
            WHEN 2 THEN
                    Est = "Inactivo".
        END CASE.
      E_Fila2     = "".
      E_Fila2     = "005" + STRING(Pro_creditos.cod_credito,"999") 
                  + "005" + STRING(Pro_Creditos.Nom_Producto,"X(40)")
                  + "015" + STRING(Tipo,"X(15)")
                  + "008" + STRING(Est,"X(8)"). 

      {Incluido\imprimir_Excel.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar_Variables wWin 
PROCEDURE Inicializar_Variables :
DO WITH FRAME F_Creditos:
  ASSIGN Pro_Creditos.Nom_Producto:SCREEN-VALUE = ""
         Pro_Creditos.Fec_Matricula:SCREEN-VALUE = STRING(TODAY)
         Pro_Creditos.Fec_Retiro:SCREEN-VALUE    = ""
         Pro_Creditos.Estado:SCREEN-VALUE        = "1".
  FIND LAST Pro_Creditos WHERE Pro_creditos.cod_credito GT 0 NO-LOCK NO-ERROR.
  IF AVAILABLE Pro_Creditos THEN
     Pro_creditos.cod_credito:SCREEN-VALUE = STRING(Pro_creditos.cod_credito + 1).
  
END.
DO WITH FRAME F_Generales:
  ASSIGN Pro_Creditos.Id_Asociado:SCREEN-VALUE    = "1"
         Pro_Creditos.Prioridad:SCREEN-VALUE      = "0"
         Pro_Creditos.Per_GarPer:SCREEN-VALUE     = "0"
         Pro_Creditos.Per_GarRea:SCREEN-VALUE     = "0"
         Pro_Creditos.Id_NumAlterno:SCREEN-VALUE  = "NO"
         Pro_Creditos.Id_Consecutivo:SCREEN-VALUE = "NO"
         Pro_Creditos.Consecutivo:SCREEN-VALUE    = "0"
         Pro_Creditos.Id_AsociaCta:SCREEN-VALUE   = "NO"
         Pro_Creditos.Id_Formato:SCREEN-VALUE     = "NO"
         Pro_Creditos.Id_AprobAgencia:SCREEN-VALUE = "NO"
         Pro_Creditos.Descripcion:SCREEN-VALUE    = ""
         Cmb_ForPagare:SCREEN-VALUE               = Cmb_ForPagare:ENTRY(1)
         Cmb_Forretencion:SCREEN-VALUE            = Cmb_Forretencion:ENTRY(1)
         Cmb_ProAhorros:SCREEN-VALUE              = Cmb_ProAhorros:ENTRY(1).
         DISABLE Pro_Creditos.Consecutivo Cmb_ForPagare Cmb_ForRetencion Cmb_ProAhorros.
END.
DO WITH FRAME F_Varios:
   ASSIGN Pro_Creditos.Id_Linea:SCREEN-VALUE        = "NO"
          Pro_Creditos.Id_Debito:SCREEN-VALUE       = "NO"
          Pro_Creditos.Id_CreUnico:SCREEN-VALUE     = "NO"
          Pro_Creditos.Id_PagParcial:SCREEN-VALUE   = "NO"
          Pro_Creditos.Id_Extracto:SCREEN-VALUE     = "NO"
          Pro_Creditos.Id_Sorteos:SCREEN-VALUE      = "NO"
          Pro_Creditos.Id_PerGracia:SCREEN-VALUE    = "NO"
          Pro_Creditos.Id_Talonario:SCREEN-VALUE    = "1"
          Pro_Creditos.Id_CreProAhorro:SCREEN-VALUE = "NO"
          Pro_Creditos.Dia_Gracia:SCREEN-VALUE      = "0"
          Cmb_ForTalonario:SCREEN-VALUE             = Cmb_ForTalonario:ENTRY(1)
          Cmb_ProMultiplicador:SCREEN-VALUE         = Cmb_ProMultiplicador:ENTRY(1)
          Cmb_ForChequeo:SCREEN-VALUE             = Cmb_ForChequeo:ENTRY(1)
          Pro_Creditos.Multiplicador:SCREEN-VALUE   = "0".
    DISABLE Pro_Creditos.Dia_Gracia Cmb_ForTalonario Multiplicador Cmb_ProMultiplicador Cmb_ForTalonario.
END.
DO WITH FRAME F_Intereses:
   ASSIGN Pro_Creditos.Id_Tasa:SCREEN-VALUE     = "2"
          Pro_Creditos.Cod_Tasa:SCREEN-VALUE    = "0"
          Valor_Tasa:SCREEN-VALUE               = "0"
          Nom_IndicadorTasa:SCREEN-VALUE        = ""
          Nom_IndicadorMora:SCREEN-VALUE        = ""
          Valor_TasaMora:SCREEN-VALUE           = "0"
         Pro_Creditos.Cod_TasaMora:SCREEN-VALUE = "".
   DISABLE Pro_Creditos.Cod_Tasa Valor_Tasa.
END.

DO WITH FRAME F_Deducibles:
   FOR EACH Tmp: DELETE Tmp. END.
   OPEN QUERY Brw_Deducibles FOR EACH Tmp NO-LOCK INDEXED-REPOSITION.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
DO WITH FRAME F_Creditos:
    VIEW FRAME F_Generales.
    RUN SUPER.

    FIND FIRST Pro_Creditos  NO-ERROR.
    IF AVAILABLE Pro_Creditos THEN DO:
        Puntero = ROWID(Pro_Creditos).
    FOR EACH Pro_Ahorros  NO-LOCK:
      W_Ok = Cmb_ProAhorros:ADD-LAST(STRING(Pro_ahorros.cod_ahorro,"999") + " - " + Pro_Ahorros.Nom_Producto) IN FRAME F_Generales.
      W_Ok = Cmb_ProMultiplicador:ADD-LAST(STRING(Pro_ahorros.cod_ahorro,"999") + " - " + Pro_Ahorros.Nom_Producto) IN FRAME F_Varios.
    END.
    FOR EACH Formatos  NO-LOCK:
      W_Ok = Cmb_ForPagare:ADD-LAST(STRING(Formatos.Cod_Formato,"99") + " - " + Formatos.Nom_Formato) IN FRAME F_Generales.
      W_Ok = Cmb_ForRetencion:ADD-LAST(STRING(Formatos.Cod_Formato,"99") + " - " + Formatos.Nom_Formato) IN FRAME F_Generales.
      W_Ok = Cmb_ForChequeo:ADD-LAST(STRING(Formatos.Cod_Formato,"99") + " - " + Formatos.Nom_Formato) IN FRAME F_Varios.
      W_Ok = Cmb_ForTalonario:ADD-LAST(STRING(Formatos.Cod_Formato,"99") + " - " + Formatos.Nom_Formato) IN FRAME F_Varios.
    END.
  END.
  /*FOR EACH Agencias WHERE Agencias.Estado EQ 1 NO-LOCK BREAK BY Agencias.Agencia:
    W_Ok = Cmb_Agencias:ADD-LAST(STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre).
  END.*/  
  IF AVAILABLE Pro_Creditos  THEN 
      RUN Mostrar_Producto.
  
  IF NOT AVAILABLE Pro_Creditos THEN DO:
     RUN Inicializar.
  END.
  HIDE FRAME F_Generales FRAME F_Varios FRAME F_Intereses FRAME F_Deducibles FRAME F_Contabilidad.
  VIEW FRAME F_Generales.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Producto wWin 
PROCEDURE Mostrar_Producto :
DO WITH FRAME F_Creditos:
  ASSIGN Pro_creditos.cod_credito:SCREEN-VALUE     = STRING(Pro_creditos.cod_credito)
         Pro_creditos.Tip_credito:SCREEN-VALUE     = STRING(Pro_creditos.Tip_credito)
         Pro_Creditos.Nom_Producto:SCREEN-VALUE    = Pro_Creditos.Nom_Producto
         Pro_Creditos.Fec_Matricula:SCREEN-VALUE   = STRING(Pro_Creditos.Fec_Matricula)
         Pro_Creditos.Fec_Retiro:SCREEN-VALUE      = STRING(Pro_Creditos.Fec_Retiro)
         Pro_Creditos.Estado:SCREEN-VALUE          = STRING(Pro_Creditos.Estado).
END.
DO WITH FRAME F_Generales:
  ASSIGN Pro_Creditos.Id_Asociado:SCREEN-VALUE     = STRING(Pro_Creditos.Id_Asociado)
         Pro_Creditos.Id_Garantia:SCREEN-VALUE     = STRING(Pro_Creditos.Id_Garantia)
         Pro_Creditos.Prioridad:SCREEN-VALUE       = STRING(Pro_Creditos.Prioridad)
         Pro_Creditos.Per_GarPer:SCREEN-VALUE      = STRING(Pro_Creditos.Per_GarPer)
         Pro_Creditos.Per_GarRea:SCREEN-VALUE      = STRING(Pro_Creditos.Per_GarRea)
         Pro_Creditos.Id_NumAlterno:SCREEN-VALUE   = STRING(Pro_Creditos.Id_NumAlterno)
         Pro_Creditos.Id_Consecutivo:SCREEN-VALUE  = STRING(Pro_Creditos.Id_Consecutivo)
         Pro_Creditos.Consecutivo:SCREEN-VALUE     = STRING(Pro_Creditos.Consecutivo)
         Pro_Creditos.Id_AsociaCta:SCREEN-VALUE    = STRING(Pro_Creditos.Id_AsociaCta)
         Pro_Creditos.Id_Formato:SCREEN-VALUE      = STRING(Pro_Creditos.Id_Formato)
         Pro_Creditos.Descripcion:SCREEN-VALUE     = Pro_Creditos.Descripcion
         Pro_Creditos.Id_AprobAgencia:SCREEN-VALUE = STRING(Pro_Creditos.Id_AprobAgencia).
         
         IF Pro_Creditos.Id_Consecutivo THEN ENABLE Pro_Creditos.Consecutivo. ELSE DISABLE Pro_Creditos.Consecutivo.
         IF Pro_Creditos.Id_Formato THEN DO:
            ENABLE Cmb_ForPagare Cmb_ForRetencion.
            FIND FIRST Formatos WHERE Formatos.Cod_Formato EQ Pro_Creditos.Cod_ForPagare NO-LOCK NO-ERROR.
            IF AVAILABLE(Formatos) THEN 
               Cmb_ForPagare:SCREEN-VALUE = STRING(Formatos.Cod_Formato,"99") + " - " + Formatos.Nom_Formato.
            FIND FIRST Formatos WHERE Formatos.Cod_Formato EQ Pro_Creditos.Cod_ForRetencion NO-LOCK NO-ERROR.
            IF AVAILABLE(Formatos) THEN 
                Cmb_ForRetencion :SCREEN-VALUE = STRING(Formatos.Cod_Formato,"99") + " - " + Formatos.Nom_Formato.
         END.
         ELSE DISABLE Cmb_ForPagare Cmb_ForRetencion.
         IF Pro_Creditos.Id_AsociaCta THEN DO:
            ENABLE Cmb_ProAhorros.
            FIND Pro_Ahorros WHERE Pro_ahorros.cod_ahorro EQ Pro_Creditos.Cod_ProAhorro NO-LOCK NO-ERROR.
            IF AVAILABLE Pro_Ahorros THEN
               Cmb_ProAhorros:SCREEN-VALUE = STRING(Pro_ahorros.cod_ahorro,"999") + " - " + Pro_Ahorros.Nom_Producto.
            ELSE Cmb_ProAhorros:SCREEN-VALUE = Cmb_ProAhorros:ENTRY(1).
         END.
         ELSE DISABLE Cmb_ProAhorros.
END.
DO WITH FRAME F_Varios:
   ASSIGN Pro_Creditos.Id_Linea:SCREEN-VALUE        = STRING(Pro_Creditos.Id_Linea)
          Pro_Creditos.Id_Debito:SCREEN-VALUE       = STRING(Pro_Creditos.Id_Debito)
          Pro_Creditos.Id_CreUnico:SCREEN-VALUE     = STRING(Pro_Creditos.Id_CreUnico)
          Pro_Creditos.Id_PagParcial:SCREEN-VALUE   = STRING(Pro_Creditos.Id_PagParcial)
          Pro_Creditos.Id_Extracto:SCREEN-VALUE     = STRING(Pro_Creditos.Id_Extracto)
          Pro_Creditos.Id_Sorteos:SCREEN-VALUE      = STRING(Pro_Creditos.Id_Sorteos)
          Pro_Creditos.Id_PerGracia:SCREEN-VALUE    = STRING(Pro_Creditos.Id_PerGracia)
          Pro_Creditos.Id_Talonario:SCREEN-VALUE    = STRING(Pro_Creditos.Id_Talonario)
          Pro_Creditos.Id_CreProAhorro:SCREEN-VALUE = STRING(Pro_Creditos.Id_CreProAhorro)
          Pro_Creditos.Dia_Gracia:SCREEN-VALUE      = STRING(Pro_Creditos.Dia_Gracia)
          Pro_Creditos.Multiplicador:SCREEN-VALUE   = STRING(Pro_Creditos.Multiplicador).
    IF Pro_Creditos.Id_PerGracia THEN ENABLE Pro_Creditos.Dia_Gracia.
    ELSE DISABLE Pro_Creditos.Dia_Gracia.
    FIND Formatos WHERE Formatos.Cod_Formato EQ Pro_Creditos.Cod_ForChequeo NO-LOCK NO-ERROR.
    IF AVAILABLE Formatos THEN Cmb_ForChequeo:SCREEN-VALUE = STRING(Formatos.Cod_Formato,"99") + " - " + Formatos.Nom_Formato.
    ELSE Cmb_ForChequeo:SCREEN-VALUE = Cmb_ForChequeo:ENTRY(1).
    IF Pro_Creditos.Id_Talonario EQ 1 THEN DO:
       DISABLE Cmb_ForTalonario.
       Cmb_ForTalonario:SCREEN-VALUE = Cmb_ForTalonario:ENTRY(1).
    END.
    ELSE DO:
       FIND Formatos WHERE Formatos.Cod_Formato EQ Pro_Creditos.Cod_ForTalonario NO-LOCK NO-ERROR.
       IF AVAILABLE Formatos THEN
          Cmb_ForTalonario:SCREEN-VALUE = STRING(Formatos.Cod_Formato,"99") + " - " + Formatos.Nom_Formato.
       ELSE
          Cmb_ForTalonario:SCREEN-VALUE = Cmb_ForTalonario:ENTRY(1).
    END.
    IF Pro_Creditos.Id_CreProAhorro THEN DO:
       ENABLE Cmb_ProMultiplicador.
       FIND Pro_Ahorros WHERE Pro_ahorros.cod_ahorro EQ Pro_Creditos.Lin_Multiplicador NO-LOCK NO-ERROR.
       IF AVAILABLE Pro_Ahorros THEN
          Cmb_ProMultiplicador:SCREEN-VALUE = STRING(Pro_ahorros.cod_ahorro,"999") + " - " + Pro_Ahorros.Nom_Producto.
       ELSE Cmb_ProMultiplicador:SCREEN-VALUE = Cmb_ProMultiplicador:ENTRY(1).
    END.
    ELSE DISABLE Multiplicador Cmb_ProMultiplicador.
END.
DO WITH FRAME F_Intereses:
   ASSIGN Pro_Creditos.Id_Tasa:SCREEN-VALUE      = STRING(Pro_Creditos.Id_Tasa)
          Pro_Creditos.Cod_TasaMora:SCREEN-VALUE = STRING(Pro_Creditos.Cod_TasaMora:SCREEN-VALUE)
          Pro_Creditos.Cod_TasaMax:SCREEN-VALUE = STRING(Pro_Creditos.Cod_TasaMax:SCREEN-VALUE).

   IF Pro_Creditos.Id_Tasa EQ 2 THEN DO:
      DISABLE Pro_Creditos.Cod_Tasa Valor_Tasa.
      ASSIGN  Pro_Creditos.Cod_Tasa:SCREEN-VALUE = "0"
              Valor_Tasa:SCREEN-VALUE            = "0"
              Nom_IndicadorTasa:SCREEN-VALUE     = "".
   END.
   ELSE DO:
      ENABLE Pro_Creditos.Cod_Tasa.
      FIND Indicadores WHERE Indicadores.Indicador EQ INTEGER(Pro_Creditos.Cod_Tasa) NO-LOCK NO-ERROR.
      IF AVAILABLE Indicadores THEN DO:
         ASSIGN Pro_Creditos.Cod_Tasa:SCREEN-VALUE  = STRING(Pro_Creditos.Cod_Tasa)
                Nom_IndicadorTasa:SCREEN-VALUE      = Indicadores.Nombre
                Valor_Tasa:SCREEN-VALUE             = STRING(Indicadores.Tasa).
      END.
   END.

   FIND Indicadores WHERE Indicadores.Indicador EQ Pro_Creditos.Cod_TasaMora
                      AND Indicadores.Estado EQ 1 NO-LOCK NO-ERROR.
   IF AVAILABLE(Indicadores) THEN
      ASSIGN Nom_IndicadorMora:SCREEN-VALUE = Indicadores.Nombre
             Valor_TasaMora:SCREEN-VALUE   = STRING(Indicadores.Tasa)
             Pro_Creditos.Cod_TasaMora:SCREEN-VALUE = STRING(Pro_Creditos.Cod_TasaMora).
   ELSE
      ASSIGN Nom_IndicadorMora:SCREEN-VALUE = ""
             Valor_TasaMora:SCREEN-VALUE   = "0"
             Pro_Creditos.Cod_TasaMora:SCREEN-VALUE = "".

   FIND Indicadores WHERE Indicadores.Indicador EQ Pro_Creditos.Cod_TasaMax
                      AND Indicadores.Estado EQ 1 NO-LOCK NO-ERROR.
   IF AVAILABLE(Indicadores) THEN
      ASSIGN NomIndicadorMax:SCREEN-VALUE = Indicadores.Nombre
             ValorTasaMax:SCREEN-VALUE   = STRING(Indicadores.Tasa)
             Pro_Creditos.Cod_TasaMax:SCREEN-VALUE = STRING(Pro_Creditos.Cod_TasaMax).
   ELSE
      ASSIGN NomIndicadorMax:SCREEN-VALUE = ""
             ValorTasaMax:SCREEN-VALUE   = "0"
             Pro_Creditos.Cod_TasaMax:SCREEN-VALUE = "".


END.

DO WITH FRAME F_Deducibles:
   DEFINE VAR j AS INTEGER.
   FOR EACH Tmp: DELETE Tmp. END.
   DO i = 1 TO 10:
      FIND Deducible WHERE Deducible.Cod_Deducible EQ Pro_Creditos.Deducible[i] NO-LOCK NO-ERROR.
      IF AVAILABLE Deducible AND Pro_Creditos.Deducible[i] NE "" THEN DO:
         CREATE Tmp.
         ASSIGN Tmp.TCodDed = Deducible.Cod_Deducible
                Tmp.TNomDed = Deducible.Nom_Deducible
                Tmp.TValDed = Deducible.Valor.
      END.
   END.
   i = 0.
   FOR EACH Tmp: i = i + 1. END.
     j = 10 - i.
   DO i = 1 TO J BY 1:
      CREATE Tmp.
   END.
   OPEN QUERY Brw_Deducibles FOR EACH Tmp NO-LOCK INDEXED-REPOSITION.
END.
DO WITH FRAME F_Restricciones:
   ASSIGN Pro_Creditos.Id_MontoMaximo:SCREEN-VALUE  = STRING(Pro_Creditos.Id_MontoMaximo)
          Pro_Creditos.Val_MontoMaximo:SCREEN-VALUE = STRING(Pro_Creditos.Val_MontoMaximo)
          Pro_Creditos.Id_MontoMinimo:SCREEN-VALUE  = STRING(Pro_Creditos.Id_MontoMinimo)
          Pro_Creditos.Val_MontoMinimo:SCREEN-VALUE = STRING(Pro_Creditos.Val_MontoMinimo)
          Pro_Creditos.Id_Plazo:SCREEN-VALUE        = STRING(Pro_Creditos.Id_Plazo)
          Pro_Creditos.Pla_Minimo:SCREEN-VALUE      = STRING(Pro_Creditos.Pla_Minimo)
          Pro_Creditos.Pla_Maximo:SCREEN-VALUE      = STRING(Pro_Creditos.Pla_Maximo).
   IF NOT Pro_Creditos.Id_MontoMaximo THEN DISABLE Pro_Creditos.Val_MontoMaximo.
   ELSE ENABLE Pro_Creditos.Val_MontoMaximo.
   IF NOT Pro_Creditos.Id_MontoMinimo THEN DISABLE Pro_Creditos.Val_MontoMinimo.
   ELSE ENABLE Pro_Creditos.Val_MontoMinimo.
   IF NOT Pro_Creditos.Id_Plazo THEN DISABLE Pro_Creditos.Pla_Maximo Pro_Creditos.Pla_Minimo.
   ELSE ENABLE Pro_Creditos.Pla_Maximo Pro_Creditos.Pla_Minimo.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir wWin 
PROCEDURE ProcesoImprimir :
{Incluido\RepEncabezado.i}

  DEFINE VARIABLE i_TipPdt AS CHARACTER FORMAT "X(15)". 
  DEFINE VARIABLE i_Estado AS CHARACTER FORMAT "X(15)". 
  DEFINE VARIABLE i_Aplica AS CHARACTER FORMAT "X(20)". 
  DEFINE VARIABLE i_debito AS CHARACTER FORMAT "X(10)". 
  DEFINE VARIABLE i_Libret AS CHARACTER FORMAT "X(10)".
  DEFINE VARIABLE i_IdTasa AS CHARACTER FORMAT "X(10)".
  DEFINE VARIABLE i_TipInt AS CHARACTER FORMAT "X(10)".
  DEFINE VARIABLE i_BasCal AS CHARACTER FORMAT "X(10)".
  DEFINE VARIABLE i_Anuali AS CHARACTER FORMAT "X(10)".

  W_Reporte   = "REPORTE   : PRODUCTOS DE CREDITOS - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
  /*W_EncColumna = " - PRODUCTO: " + STRING(Pro_creditos.cod_credito:SCREEN-VALUE) + " - " 
                 + Pro_Creditos.Nom_Producto:SCREEN-VALUE.*/

  VIEW FRAME F-Encabezado.
  VIEW FRAME F-Ftr.

DO WITH FRAME F_Creditos:
    IF Pro_Creditos.Estado:SCREEN-VALUE EQ "1" THEN i_Estado = "ACTIVO".
    ELSE I_Estado = "INACTIVO".
    CASE Pro_Creditos.Tip_Credito:SCREEN-VALUE:
        WHEN "1" THEN i_TipPdt = "CONSUMO".
        WHEN "2" THEN i_TipPdt = "COMERCIAL".
        WHEN "3" THEN i_TipPdt = "HIPOTECARIO".
        WHEN "4" THEN i_TipPdt = "MICROCREDITO".
    END CASE.

    DISPLAY "INFORMACION ENCABEZADO"                            AT 1 SKIP(1)
            "Agencia             : "                            AT 1 
             "Tipo de Producto    : "                            AT 60
             i_TipPdt                                           AT 85
            "Fecha de Creación   : "                            AT 1
             Pro_Creditos.Fec_Matricula                          AT 25
            "Estado del Producto : "                            AT 60
             i_Estado                                           AT 85
   WITH FRAME a WIDTH 132 NO-BOX USE-TEXT NO-LABELS.
END.

DO WITH FRAME F_Generales:
   CASE Pro_Creditos.Id_Asociado:SCREEN-VALUE:
       WHEN "1" THEN i_Aplica = "TODOS LOS CLIENTES".
       WHEN "2" THEN i_Aplica = "ASOCIADOS".
       WHEN "3" THEN i_Aplica = "NO ASOCIADOS".
   END CASE.
   DISPLAY SKIP(1)
           "INFORMACION GENERAL"                               AT 1 SKIP(1)
           "Producto Creado Para: "                            AT 1 
            i_Aplica                                           AT 25
  WITH FRAME b WIDTH 132 NO-BOX USE-TEXT NO-LABELS.
END.

DO WITH FRAME F_Intereses:
   IF Pro_Creditos.Id_Tasa:SCREEN-VALUE EQ "1" THEN i_IdTasa = "PRODUCTO".
   ELSE I_IdTasa = "CUENTA".
   DISPLAY SKIP(1)
           "INFORMACION INTEGERESES"                           AT 1 SKIP(1)
           "Tasa Manejada por   : "                            AT 1 
           i_IdTasa                                            AT 25
           "Valor de la Tasa    : "                            AT 60
           Valor_Tasa:SCREEN-VALUE                             AT 85
           "Tasa de Mora        : "                            AT 1
           Valor_TasaMora:SCREEN-VALUE                         AT 25
   WITH FRAME c WIDTH 132 NO-BOX USE-TEXT NO-LABELS.
END.
PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

