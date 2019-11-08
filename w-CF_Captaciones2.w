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
{INCLUIDO\VARIABLE.I "SHARED"}
DEFINE VAR W_Ok AS LOGICAL.
DEFINE VAR W_Nuevo AS LOGICAL INITIAL NO.
DEFINE VAR W_Puntero AS ROWID.

DEFINE VARIABLE W_CodOpe LIKE Operacion.Cod_Operacion.
DEFINE VARIABLE W_NomOpe LIKE Operacion.Nom_Operacion.
    
DEFINE VAR CodWork LIKE pro_ahorros.cod_ahorro.    
DEFINE VAR W_AgeDest  LIKE Agencias.Agencia.
DEFINE VAR W_Ind LIKE Indicadores.Indicador.
DEFINE VAR W_Cero   LIKE Indicadores.Valor.
DEFINE TEMP-TABLE RegPAho           LIKE Pro_AHORROS.
    
DEFINE VAR P_Agencia  LIKE Agencias.Agencia.        
DEFINE VAR P_Cod      LIKE pro_ahorros.cod_ahorro.
DEFINE VAR p_Nombre   LIKE Pro_Ahorros.Nom_Producto.
DEFINE VAR P_AgePro   LIKE Agencias.Agencia.

DEFINE VAR WCod  LIKE Formatos.Cod_Formato.
DEFINE VAR WNom  LIKE Formatos.Nom_Formato.
DEFINE VAR WAge  LIKE Formatos.Agencia.

    DEFINE VARIABLE W_Codbase      AS CHARACTER FORMAT "X(4)".
    DEFINE VARIABLE W_Pcuenta AS CHARACTER.
    DEFINE VARIABLE W_Pnombre AS CHARACTER.
    DEFINE VARIABLE W_Naturaleza AS CHARACTER.
    DEFINE VARIABLE W_CtrNat AS LOGICAL.
    DEFINE VARIABLE W_Consulta     AS LOGICAL INITIAL FALSE.
    DEFINE VARIABLE W_ConsCta AS CHARACTER.
    DEFINE VARIABLE W_DispCta AS CHARACTER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_General

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Pro_Ahorros

/* Definitions for FRAME F_ProAhorros                                   */
&Scoped-define SELF-NAME F_ProAhorros
&Scoped-define QUERY-STRING-F_ProAhorros FOR EACH Pro_Ahorros NO-LOCK
&Scoped-define OPEN-QUERY-F_ProAhorros OPEN QUERY {&SELF-NAME} FOR EACH Pro_Ahorros NO-LOCK.
&Scoped-define TABLES-IN-QUERY-F_ProAhorros Pro_Ahorros
&Scoped-define FIRST-TABLE-IN-QUERY-F_ProAhorros Pro_Ahorros


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Pro_Ahorros.Id_Talonario ~
Pro_Ahorros.Nro_CheqACobrar Pro_Ahorros.Val_CadaCheque ~
Pro_Ahorros.Id_Extracto Pro_Ahorros.Id_Asociado Pro_Ahorros.Id_Debito ~
Pro_Ahorros.Id_Linea Pro_Ahorros.Id_NumAlterno Pro_Ahorros.Id_Revaloriza 
&Scoped-define ENABLED-TABLES Pro_Ahorros
&Scoped-define FIRST-ENABLED-TABLE Pro_Ahorros
&Scoped-Define ENABLED-OBJECTS RECT-279 RECT-38 RECT-39 
&Scoped-Define DISPLAYED-FIELDS Pro_Ahorros.Id_Talonario ~
Pro_Ahorros.Id_CobroTal Pro_Ahorros.Val_Talonario ~
Pro_Ahorros.Nro_CheqACobrar Pro_Ahorros.Val_CadaCheque ~
Pro_Ahorros.Id_Extracto Pro_Ahorros.Id_Asociado Pro_Ahorros.Id_Debito ~
Pro_Ahorros.Id_Consecutivo Pro_Ahorros.Num_Consecutivo Pro_Ahorros.Id_Linea ~
Pro_Ahorros.Titulo_Valor Pro_Ahorros.Id_Retparcial ~
Pro_Ahorros.Id_Vencimiento Pro_Ahorros.Id_NumAlterno ~
Pro_Ahorros.Tip_Vencimiento Pro_Ahorros.Id_Revaloriza ~
Pro_Ahorros.Id_RenVencimiento Pro_Ahorros.Cta_Revaloriz ~
Pro_Ahorros.Prioridad 
&Scoped-define DISPLAYED-TABLES Pro_Ahorros
&Scoped-define FIRST-DISPLAYED-TABLE Pro_Ahorros
&Scoped-Define DISPLAYED-OBJECTS W_CobroLibreta W_CobroCheq NomCtaRev 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 Pro_Ahorros.Id_Consecutivo ~
Pro_Ahorros.Num_Consecutivo 
&Scoped-define List-2 Pro_Ahorros.Cod_ahorro Pro_Ahorros.Id_Sobregiro ~
Pro_Ahorros.Tip_Salminimo Pro_Ahorros.Val_SdoMinimo Pro_Ahorros.Prioridad 
&Scoped-define List-3 Pro_Ahorros.Porce_Embargo Pro_Ahorros.Num_SMSeguro ~
Pro_Ahorros.Val_Talonario Pro_Ahorros.Tie_Inactividad ~
Pro_Ahorros.Val_MinRetiro Pro_Ahorros.Mon_MinLiqidacion ~
Pro_Ahorros.Tip_Salminimo Pro_Ahorros.Num_Consecutivo ~
Pro_Ahorros.Val_SdoMinimo Pro_Ahorros.Dia_Gracia ~
Pro_Ahorros.Val_Minconsignacion Pro_Ahorros.Val_Cuota ~
Pro_Ahorros.Tip_Vencimiento Pro_Ahorros.Val_MaxConsignacion ~
Pro_Ahorros.Val_MonAper Pro_Ahorros.Val_MaxRetEfectivo 
&Scoped-define List-4 W_Nin 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_CobroCheq 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     IMAGE-INSENSITIVE FILE "imagenes/arwdwn.gif":U
     LABEL "Btn_cobroCheq" 
     SIZE 3 BY .54.

DEFINE BUTTON Btn_CobroTal 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     IMAGE-INSENSITIVE FILE "imagenes/arwdwn.gif":U
     LABEL "Button 120" 
     SIZE 3 BY .54.

DEFINE VARIABLE NomCtaRev AS CHARACTER FORMAT "X(30)":U 
      VIEW-AS TEXT 
     SIZE 23 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CobroCheq AS CHARACTER FORMAT "X(30)":U 
     LABEL "Oper.Contable" 
      VIEW-AS TEXT 
     SIZE 39 BY .73
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CobroLibreta AS CHARACTER FORMAT "X(30)":U 
     LABEL "Oper.Contable" 
      VIEW-AS TEXT 
     SIZE 39.14 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-279
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 43 BY 2.96.

DEFINE RECTANGLE RECT-38
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 43 BY 4.04.

DEFINE RECTANGLE RECT-39
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 85.86 BY 4.77.

DEFINE VARIABLE Cmb_Creditos AS CHARACTER FORMAT "X(35)":U 
     LABEL "Producto Asociado" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 29 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Nin AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 52 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Val AS DECIMAL FORMAT ">>9.9999999":U INITIAL 0 
     LABEL "Tasa" 
     VIEW-AS FILL-IN 
     SIZE 20 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-212
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 36 BY 2.15.

DEFINE RECTANGLE RECT-213
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 36 BY 2.15.

DEFINE RECTANGLE RECT-214
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 36 BY 1.88.

DEFINE RECTANGLE RECT-215
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 48 BY 3.23.

DEFINE RECTANGLE RECT-277
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 85 BY 3.77.

DEFINE VARIABLE W_CmbPeriodo AS CHARACTER FORMAT "X(15)":U INITIAL "Diario" 
     LABEL "Periodo de Liquidación" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "01 - Diario","02 - Mensual","03 - Trimestral","04 - Semestral","05 - Anual","06 - Al Vencimiento" 
     DROP-DOWN-LIST
     SIZE 24 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-220
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 62 BY 1.35.

DEFINE RECTANGLE RECT-221
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 62 BY 1.35.

DEFINE RECTANGLE RECT-278
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 62 BY 7.81.

DEFINE RECTANGLE RECT-98
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 15 BY 4.04.

DEFINE RECTANGLE RECT-99
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 15 BY 2.96.

DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Salir" 
     SIZE 11 BY 1.62
     BGCOLOR 8 .

DEFINE BUTTON Btn_Cancelar 
     LABEL "Cancelar" 
     SIZE 11 BY 1.62.

DEFINE BUTTON Btn_Consulta 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "&Consulta" 
     SIZE 11 BY 1.62 TOOLTIP "Busqueda de información de la pantalla en uso".

DEFINE BUTTON Btn_Generales 
     LABEL "Generales" 
     SIZE 18 BY 1.08
     FONT 4.

DEFINE BUTTON Btn_Impresion 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/impresora2.bmp":U
     LABEL "Im&primir" 
     SIZE 11 BY 1.62 TOOLTIP "Muestra la interface de salida de información (Reportes)".

DEFINE BUTTON Btn_Interes 
     LABEL "Intereses" 
     SIZE 18 BY 1.08
     FONT 4.

DEFINE BUTTON Btn_Liquidacion 
     LABEL "Liquidaciones" 
     SIZE 18 BY 1.08
     FONT 4.

DEFINE BUTTON Btn_Restriccion 
     LABEL "Restricciones" 
     SIZE 18 BY 1.08
     FONT 4.

DEFINE BUTTON Btn_Salvar 
     LABEL "Salvar" 
     SIZE 11 BY 1.62.

DEFINE BUTTON Btn_Varios 
     LABEL "Varios" 
     SIZE 18 BY 1.08
     FONT 4.

DEFINE BUTTON BUTTON-11 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 11" 
     SIZE 5 BY 1.08.

DEFINE BUTTON BUTTON-4 
     LABEL "Ingresar" 
     SIZE 11 BY 1.62.

DEFINE BUTTON BUTTON-49 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 49" 
     SIZE 11 BY 1.65 TOOLTIP "Muestra la pantalla de información de sesión y los mensajes del usuario activo".

DEFINE BUTTON BUTTON-6 
     LABEL "Deshacer" 
     SIZE 11 BY 1.62.

DEFINE BUTTON BUTTON-8 
     LABEL "Borrar" 
     SIZE 11 BY 1.62.

DEFINE RECTANGLE RECT-211
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 16 BY 4.31.

DEFINE RECTANGLE RECT-237
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 16 BY 4.31.

DEFINE RECTANGLE RECT-276
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 13 BY 5.38.

DEFINE RECTANGLE RECT-216
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 41 BY 2.42.

DEFINE RECTANGLE RECT-217
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 41 BY 2.96.

DEFINE RECTANGLE RECT-218
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 41 BY 1.62.

DEFINE RECTANGLE RECT-219
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 41 BY 2.69.

DEFINE RECTANGLE RECT-61
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 42 BY 5.38.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 42 BY 1.88.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 42 BY 2.69.

DEFINE BUTTON BUTTON-118 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Button 118" 
     SIZE 3 BY .54.

DEFINE BUTTON BUTTON-119 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Button 119" 
     SIZE 3 BY .54.

DEFINE VARIABLE NomCGto AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 27.29 BY .77
     BGCOLOR 18  NO-UNDO.

DEFINE VARIABLE NomCxP AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 26.86 BY .77
     BGCOLOR 18  NO-UNDO.

DEFINE VARIABLE W_CodFormato AS CHARACTER FORMAT "X(50)":U 
     LABEL "Formato" 
     VIEW-AS FILL-IN 
     SIZE 37.14 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_ProDigito AS CHARACTER FORMAT "X(50)":U 
     LABEL "Digito Chequeo" 
     VIEW-AS FILL-IN 
     SIZE 37.29 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-222
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 34.57 BY 2.96.

DEFINE RECTANGLE RECT-223
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 34.72 BY 2.69.

DEFINE RECTANGLE RECT-224
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 34.57 BY 2.15.

DEFINE RECTANGLE RECT-225
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 18.57 BY 8.35.

DEFINE RECTANGLE RECT-280
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28.43 BY 13.46.

DEFINE RECTANGLE RECT-281
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 25.43 BY 2.27.

DEFINE RECTANGLE RECT-282
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 25.43 BY 1.35.

DEFINE RECTANGLE RECT-283
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 25.43 BY 1.35.

DEFINE RECTANGLE RECT-284
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 25.43 BY 1.35.

DEFINE RECTANGLE RECT-285
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 55 BY 3.77.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY F_ProAhorros FOR 
      Pro_Ahorros SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_ProAhorros
     Pro_Ahorros.Tip_Ahorro AT ROW 1.27 COL 63 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "A la Vista", 1,
"Contractual", 2,
"A Término", 3,
"Aportes", 4,
"Convenios", 5
          SIZE 14 BY 3.81
          FONT 5
     Pro_Ahorros.Estado AT ROW 1.46 COL 79.29 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Activo", 1,
"Inactivo", 2
          SIZE 14 BY 1.69
     BUTTON-49 AT ROW 1.54 COL 100
     Pro_Ahorros.Fec_Matricula AT ROW 1.65 COL 19.14 COLON-ALIGNED
          LABEL "Fecha Matrícula"
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Pro_Ahorros.Fec_Retiro AT ROW 1.65 COL 45.86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Btn_Impresion AT ROW 3.15 COL 100 HELP
          "Permite generar la Impresión de los productos de Ahorros"
     Pro_Ahorros.Cod_ahorro AT ROW 3.23 COL 8.57 COLON-ALIGNED
          LABEL "Producto"
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
          BGCOLOR 15 FGCOLOR 0 
     Pro_Ahorros.Nom_Producto AT ROW 3.23 COL 13.57 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 46 BY 1
          BGCOLOR 15 
     Pro_Ahorros.Id_Persona AT ROW 3.35 COL 79.43 NO-LABEL WIDGET-ID 2
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Adulto", 1,
"Infantil", 2
          SIZE 14 BY 1.69
     Btn_Consulta AT ROW 4.77 COL 100 HELP
          "Permite generar la consulta de los productos de Ahorros"
     Btn_Generales AT ROW 5.31 COL 5 HELP
          "Matrícula información general de producto"
     Btn_Varios AT ROW 5.31 COL 23 HELP
          "Matrícula información variada del producto"
     Btn_Interes AT ROW 5.31 COL 41 HELP
          "Matrícula información de tasas de interés"
     Btn_Liquidacion AT ROW 5.31 COL 59 HELP
          "Matrícula información de liquidaciones"
     Btn_Restriccion AT ROW 5.31 COL 77 HELP
          "Matrícula información de restricciones"
     Btn_Salvar AT ROW 9.88 COL 100
     BUTTON-6 AT ROW 11.5 COL 100
     BUTTON-4 AT ROW 13.12 COL 100
     BUTTON-8 AT ROW 14.73 COL 100
     Btn_Cancelar AT ROW 16.35 COL 100
     BtnDone AT ROW 17.96 COL 100
     BUTTON-11 AT ROW 20.38 COL 103
     RECT-211 AT ROW 1 COL 62
     RECT-237 AT ROW 1 COL 78.57
     RECT-276 AT ROW 1.27 COL 99
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.43 BY 21.12
         BGCOLOR 17 FONT 5.

DEFINE FRAME F_Restricciones
     Pro_Ahorros.Id_Plazo AT ROW 1.27 COL 4
          LABEL "Maneja Plazos en días"
          VIEW-AS TOGGLE-BOX
          SIZE 24 BY .54
     Pro_Ahorros.Id_BloInactividad AT ROW 1.27 COL 48
          LABEL "Maneja Bloqueo por Inactividad"
          VIEW-AS TOGGLE-BOX
          SIZE 31 BY .54
     Pro_Ahorros.Pla_Minimo AT ROW 1.81 COL 28 COLON-ALIGNED
          LABEL "Mínimo"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 15 
     Pro_Ahorros.Tie_Inactividad AT ROW 2.08 COL 74 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 15 
     Pro_Ahorros.Pla_Maximo AT ROW 2.88 COL 28 COLON-ALIGNED
          LABEL "Máximo"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 15 
     Pro_Ahorros.Id_Salminimo AT ROW 4.23 COL 4
          LABEL "Maneja Saldo Mínimo"
          VIEW-AS TOGGLE-BOX
          SIZE 22 BY 1.08
     Pro_Ahorros.Id_Montominimo AT ROW 4.23 COL 48
          LABEL "Maneja Monto Mínimo"
          VIEW-AS TOGGLE-BOX
          SIZE 22 BY 1.08
          BGCOLOR 17 
     Pro_Ahorros.Val_MinRetiro AT ROW 5.31 COL 74 COLON-ALIGNED
          LABEL "Valor Mínimo de Retiro"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 15 
     Pro_Ahorros.Tip_Salminimo AT ROW 5.58 COL 5 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Por Producto", yes,
"Por Cuenta", no
          SIZE 30 BY .54
          BGCOLOR 17 
     Pro_Ahorros.Val_SdoMinimo AT ROW 6.38 COL 28 COLON-ALIGNED
          LABEL "Vlr Saldo Mínimo"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 15 
     Pro_Ahorros.Val_Minconsignacion AT ROW 6.92 COL 74 COLON-ALIGNED
          LABEL "Valor Mínimo de Consignación"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 15 
     Pro_Ahorros.Id_Cuota AT ROW 8.27 COL 4
          LABEL "Maneja Cuota"
          VIEW-AS TOGGLE-BOX
          SIZE 15 BY .54
     Pro_Ahorros.Val_MinRetcheque AT ROW 8.54 COL 74 COLON-ALIGNED
          LABEL "Valor Mínimo Retiro en cheque"
          VIEW-AS FILL-IN 
          SIZE 11.86 BY .81
          BGCOLOR 15 
     Pro_Ahorros.Val_Cuota AT ROW 9.08 COL 28 COLON-ALIGNED
          LABEL "Valor Mínimo de Cuota"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 15 
     Pro_Ahorros.Id_Montomaximo AT ROW 10.42 COL 48
          LABEL "Maneja Monto Máximo"
          VIEW-AS TOGGLE-BOX
          SIZE 23 BY 1.08
     Pro_Ahorros.Id_MonApertura AT ROW 10.54 COL 4
          LABEL "Maneja Monto Apertura"
          VIEW-AS TOGGLE-BOX
          SIZE 23 BY .81
     Pro_Ahorros.Val_MaxConsignacion AT ROW 11.5 COL 74 COLON-ALIGNED
          LABEL "Valor Máximo de Consignación"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 15 
     Pro_Ahorros.Val_MonAper AT ROW 12.04 COL 28 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 15 
     Pro_Ahorros.Val_MaxRetEfectivo AT ROW 12.58 COL 74 COLON-ALIGNED
          LABEL "Valor Máximo Retiro Efectivo"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 15 
     Pro_Ahorros.Id_AfeSdoMinimo AT ROW 14.19 COL 48
          LABEL "Afecta Saldo Mínimo"
          VIEW-AS TOGGLE-BOX
          SIZE 21 BY .81
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 5 ROW 6.65
         SIZE 90 BY 15.08
         BGCOLOR 17 FONT 5.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F_Restricciones
     RECT-216 AT ROW 1.54 COL 3
     RECT-217 AT ROW 4.77 COL 3
     RECT-218 AT ROW 8.54 COL 3
     RECT-219 AT ROW 10.96 COL 3
     RECT-61 AT ROW 4.77 COL 47
     RECT-7 AT ROW 1.54 COL 47
     RECT-9 AT ROW 10.96 COL 47
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 5 ROW 6.65
         SIZE 90 BY 15.08
         BGCOLOR 17 FONT 5
         TITLE "Restricciones".

DEFINE FRAME F_Liquidaciones
     Pro_Ahorros.Bas_Calculo AT ROW 2.62 COL 72 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Mínimo", 1,
"Promedio", 2,
"Efectivo", 3
          SIZE 11 BY 3.5
          BGCOLOR 17 
     Pro_Ahorros.Id_perliquidacion AT ROW 3.69 COL 12 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Producto", 1,
"Cuenta", 2
          SIZE 28 BY .81
          BGCOLOR 17 FGCOLOR 7 FONT 5
     W_CmbPeriodo AT ROW 5.31 COL 38 COLON-ALIGNED HELP
          "Seleccione el Tipo de Anualidad para el producto"
     Pro_Ahorros.Abo_Cuenta AT ROW 6.38 COL 38 COLON-ALIGNED
          LABEL "Abono Cuenta"
          VIEW-AS FILL-IN 
          SIZE 24 BY .81
          BGCOLOR 15 
     Pro_Ahorros.Per_Pago AT ROW 7.46 COL 38 COLON-ALIGNED
          LABEL "Periodo de Pago"
          VIEW-AS FILL-IN 
          SIZE 24 BY .81
          BGCOLOR 15 
     Pro_Ahorros.Tip_Anualidad AT ROW 7.73 COL 71 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Fija", yes,
"Variable", no
          SIZE 12 BY 2.15
          BGCOLOR 17 
     Pro_Ahorros.Id_SdoMinLiquidacion AT ROW 10.69 COL 7
          LABEL "Saldo Mínimo Liquidación"
          VIEW-AS TOGGLE-BOX
          SIZE 25 BY .81
     Pro_Ahorros.Mon_MinLiqidacion AT ROW 10.69 COL 40 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 24 BY .81
          BGCOLOR 15 
     Pro_Ahorros.Id_PerGracia AT ROW 12.31 COL 7
          LABEL "Período de Gracia"
          VIEW-AS TOGGLE-BOX
          SIZE 21 BY .81
     Pro_Ahorros.Dia_Gracia AT ROW 12.31 COL 40 COLON-ALIGNED
          LABEL "Días"
          VIEW-AS FILL-IN 
          SIZE 24 BY .81
          BGCOLOR 15 
     "La liquidación de intereses se hará por..." VIEW-AS TEXT
          SIZE 35 BY .81 AT ROW 2.08 COL 7
          FGCOLOR 7 FONT 5
     " Anualidad" VIEW-AS TEXT
          SIZE 11 BY 1.04 AT ROW 6.65 COL 71
          BGCOLOR 17 FGCOLOR 7 FONT 5
     " Base Interés" VIEW-AS TEXT
          SIZE 11 BY 1.08 AT ROW 1.81 COL 71
          BGCOLOR 17 FGCOLOR 7 FONT 5
     RECT-220 AT ROW 10.42 COL 5
     RECT-221 AT ROW 12.04 COL 5
     RECT-278 AT ROW 2.35 COL 5
     RECT-98 AT ROW 2.35 COL 70
     RECT-99 AT ROW 7.19 COL 70
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 5 ROW 6.65
         SIZE 90 BY 15.08
         BGCOLOR 17 FONT 5
         TITLE "Liquidaciones".

DEFINE FRAME F_Intereses
     Pro_Ahorros.Id_Tasa AT ROW 2.08 COL 17 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Producto", 1,
"Cuenta", 2
          SIZE 31 BY 1.35
          BGCOLOR 17 
     Pro_Ahorros.Indicador AT ROW 3.42 COL 15 COLON-ALIGNED
          LABEL "Indicador"
          VIEW-AS FILL-IN 
          SIZE 20 BY .81
          BGCOLOR 15 
     W_Nin AT ROW 3.42 COL 35 COLON-ALIGNED NO-LABEL
     W_Val AT ROW 4.5 COL 15 COLON-ALIGNED
     Pro_Ahorros.Id_Sobregiro AT ROW 6.12 COL 44
          LABEL "Maneja Sobregiro"
          VIEW-AS TOGGLE-BOX
          SIZE 20 BY .81
          BGCOLOR 17 
     Pro_Ahorros.Tip_Interes AT ROW 6.92 COL 6 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Simple", 1,
"Compuesto", 2
          SIZE 34 BY 1.35
          BGCOLOR 17 
     Cmb_Creditos AT ROW 7.19 COL 58 COLON-ALIGNED
     Pro_Ahorros.ProCre_Asociado AT ROW 8.27 COL 50 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.43 BY 1
          BGCOLOR 17 FGCOLOR 17 
     Pro_Ahorros.Id_CanCalIntereses AT ROW 8.27 COL 62
          LABEL "Deja de liquidar intereses"
          VIEW-AS TOGGLE-BOX
          SIZE 25 BY 1.08
     Pro_Ahorros.Id_ForLiquidacion AT ROW 9.88 COL 6 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Producto", 1,
"Cuenta", 2
          SIZE 34 BY 1.35
          BGCOLOR 17 
     Pro_Ahorros.For_Liquidacion AT ROW 13.12 COL 6 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Anticipado", 1,
"Vencido", 2
          SIZE 34 BY 1.08
          BGCOLOR 17 
     " Maneja Intereses Por" VIEW-AS TEXT
          SIZE 19 BY 1.08 AT ROW 1.27 COL 6
          BGCOLOR 17 FGCOLOR 7 FONT 5
     " Liquidación de Interés" VIEW-AS TEXT
          SIZE 19 BY 1.08 AT ROW 8.81 COL 6
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "Forma Liquidación" VIEW-AS TEXT
          SIZE 17 BY 1.08 AT ROW 12.04 COL 6
          BGCOLOR 17 FGCOLOR 7 FONT 5
     " Cálculo del Interés" VIEW-AS TEXT
          SIZE 16 BY 1.08 AT ROW 5.85 COL 6
          BGCOLOR 17 FGCOLOR 7 FONT 5
     RECT-212 AT ROW 6.38 COL 5
     RECT-213 AT ROW 9.35 COL 5
     RECT-214 AT ROW 12.58 COL 5
     RECT-215 AT ROW 6.38 COL 42
     RECT-277 AT ROW 1.81 COL 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 5 ROW 6.65
         SIZE 90 BY 15.08
         BGCOLOR 17 FONT 5
         TITLE "Intereses".

DEFINE FRAME F_General
     Pro_Ahorros.Id_Talonario AT ROW 1.54 COL 36.29 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Libreta", 1,
"Chequera", 2,
"Ninguna", 3
          SIZE 47.14 BY .81
          BGCOLOR 17 
     Pro_Ahorros.Id_CobroTal AT ROW 1.92 COL 3.86
          LABEL "Maneja Cobro Talonario"
          VIEW-AS TOGGLE-BOX
          SIZE 25 BY .77
     Pro_Ahorros.Val_Talonario AT ROW 2.77 COL 13.72 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.57 BY .81
          BGCOLOR 15 
     Btn_CobroTal AT ROW 3.04 COL 83.57
     Pro_Ahorros.Nro_CheqACobrar AT ROW 3.96 COL 78.14 COLON-ALIGNED
          LABEL "Nro de Cupones por Talonario" FORMAT "ZZ"
          VIEW-AS FILL-IN 
          SIZE 3.14 BY .88 TOOLTIP "Nro_Cheques a Cobrar : 0 No hay cobro; Desde que Nro se cobra..."
          BGCOLOR 15 
     Pro_Ahorros.Val_CadaCheque AT ROW 4.92 COL 13.72 COLON-ALIGNED
          LABEL "Valor Unidad" FORMAT ">>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 13.86 BY .77 TOOLTIP "Valor de cada Cheque"
          BGCOLOR 15 
     Btn_CobroCheq AT ROW 5.12 COL 83.57
     Pro_Ahorros.Id_Extracto AT ROW 6.65 COL 5
          LABEL "Maneja Extracto"
          VIEW-AS TOGGLE-BOX
          SIZE 19 BY .5
     Pro_Ahorros.Id_Asociado AT ROW 6.92 COL 46 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Todos", 1,
"Asociados", 2,
"No Asociados", 3
          SIZE 39 BY .54
     Pro_Ahorros.Id_Debito AT ROW 8 COL 5
          LABEL "Admite Débito Automático"
          VIEW-AS TOGGLE-BOX
          SIZE 27 BY .54
     Pro_Ahorros.Id_Consecutivo AT ROW 8 COL 47
          LABEL "Maneja Consecutivo"
          VIEW-AS TOGGLE-BOX
          SIZE 21 BY .77
     Pro_Ahorros.Num_Consecutivo AT ROW 8 COL 67 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     Pro_Ahorros.Id_Linea AT ROW 9.08 COL 5
          LABEL "Trabaja en Línea"
          VIEW-AS TOGGLE-BOX
          SIZE 22 BY .54
     Pro_Ahorros.Titulo_Valor AT ROW 9.08 COL 47
          LABEL "Es un Titulo-valor"
          VIEW-AS TOGGLE-BOX
          SIZE 18 BY .77
     Pro_Ahorros.Id_Retparcial AT ROW 10.15 COL 5
          LABEL "Maneja Retiros Parciales"
          VIEW-AS TOGGLE-BOX
          SIZE 27 BY .81
     Pro_Ahorros.Id_Vencimiento AT ROW 10.69 COL 47
          LABEL "Maneja Vencimientos"
          VIEW-AS TOGGLE-BOX
          SIZE 22 BY .81
     Pro_Ahorros.Id_NumAlterno AT ROW 11.5 COL 5
          LABEL "Utiliza Número Alterno"
          VIEW-AS TOGGLE-BOX
          SIZE 23 BY .5
     Pro_Ahorros.Tip_Vencimiento AT ROW 11.88 COL 50 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Plazo", 1,
"Nro.Cuotas", 2,
"Monto", 3
          SIZE 32 BY .81
          BGCOLOR 17 
     Pro_Ahorros.Id_Revaloriza AT ROW 12.58 COL 5
          LABEL "Id. Saldo Revalorización?"
          VIEW-AS TOGGLE-BOX
          SIZE 28 BY .77
     Pro_Ahorros.Id_RenVencimiento AT ROW 12.96 COL 50
          LABEL "Maneja Renovación Vencimiento"
          VIEW-AS TOGGLE-BOX
          SIZE 33 BY .5
     Pro_Ahorros.Cta_Revaloriz AT ROW 13.65 COL 7 COLON-ALIGNED
          LABEL "Cuenta"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 15 FONT 4
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 5 ROW 6.65
         SIZE 90 BY 15.08
         BGCOLOR 17 FONT 5.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F_General
     Pro_Ahorros.Prioridad AT ROW 14.19 COL 81 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_CobroLibreta AT ROW 2.85 COL 42.43 COLON-ALIGNED
     W_CobroCheq AT ROW 5.04 COL 42.43 COLON-ALIGNED
     NomCtaRev AT ROW 13.65 COL 19 COLON-ALIGNED NO-LABEL
     " La cuenta es manejada mediante..." VIEW-AS TEXT
          SIZE 31 BY .81 AT ROW 1.08 COL 5
          BGCOLOR 17 FGCOLOR 7 FONT 5
     " Este producto aplica para" VIEW-AS TEXT
          SIZE 23 BY .85 AT ROW 6.04 COL 47
          BGCOLOR 17 FGCOLOR 7 FONT 5
     RECT-279 AT ROW 11.08 COL 45
     RECT-38 AT ROW 6.38 COL 45
     RECT-39 AT ROW 1.27 COL 2.14
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 5 ROW 6.65
         SIZE 90 BY 15.08
         BGCOLOR 17 FONT 5
         TITLE "Información General".

DEFINE FRAME F_Varios
     Pro_Ahorros.Id_Pignoracion AT ROW 1.54 COL 5
          LABEL "Maneja Pignoración"
          VIEW-AS TOGGLE-BOX
          SIZE 20 BY 1.04
     Pro_Ahorros.Val_Pignoracion AT ROW 2.58 COL 23 COLON-ALIGNED
          LABEL "Valor"
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 15 
     Pro_Ahorros.Porce_Pignoracion AT ROW 3.65 COL 23 COLON-ALIGNED
          LABEL "Porcentaje"
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 15 
     Pro_Ahorros.Id_Embargo AT ROW 5.04 COL 6
          LABEL "Maneja Embargo"
          VIEW-AS TOGGLE-BOX
          SIZE 18 BY .77
     Pro_Ahorros.Val_Embargo AT ROW 5.81 COL 23.14 COLON-ALIGNED
          LABEL "Valor"
          VIEW-AS FILL-IN 
          SIZE 12.86 BY .81
          BGCOLOR 15 
     Pro_Ahorros.Porce_Embargo AT ROW 6.88 COL 23.14 COLON-ALIGNED
          LABEL "Porcentaje"
          VIEW-AS FILL-IN 
          SIZE 12.86 BY .81
          BGCOLOR 15 
     Pro_Ahorros.Id_Seguro AT ROW 8 COL 5
          LABEL "El Producto tiene seguro?"
          VIEW-AS TOGGLE-BOX
          SIZE 25 BY .77
     Pro_Ahorros.Num_SMSeguro AT ROW 9.04 COL 24.14 COLON-ALIGNED
          LABEL "Salarios mínimos Aseg."
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 15 
     BUTTON-118 AT ROW 11 COL 55.57
     W_CodFormato AT ROW 11 COL 16.29 COLON-ALIGNED
     BUTTON-119 AT ROW 12.08 COL 55.57
     W_ProDigito AT ROW 12.08 COL 16.29 COLON-ALIGNED
     Pro_Ahorros.Dia_Canje AT ROW 13.27 COL 16.57 COLON-ALIGNED
          LABEL "Días Canje" FORMAT ">9"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 15 
     Pro_Ahorros.Id_VerSdoTaq AT ROW 13.23 COL 31.86
          LABEL "Muestra Saldo por Taquilla?"
          VIEW-AS TOGGLE-BOX
          SIZE 22 BY .81
          FONT 4
     Pro_Ahorros.Id_Sorteos AT ROW 1.54 COL 41.43
          LABEL "Maneja Sorteos"
          VIEW-AS TOGGLE-BOX
          SIZE 15.86 BY 1.08
     Pro_Ahorros.Periodicidad AT ROW 2.62 COL 44 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Diaria", 1,
"Semanal", 2,
"Dececadal", 3,
"Quincenal", 4,
"Mensual", 5,
"Bimestral", 6,
"Trimestral", 7,
"Semestral", 8,
"Anual", 9
          SIZE 12.72 BY 7.27
          BGCOLOR 17 
     Pro_Ahorros.Id_GMF AT ROW 2.19 COL 63.86 HELP
          "Id_GMF : 0 a cargo de la entidad, 1 a cargo del Cliente" NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "A Cargo de la Entidad", 0,
"A Cargo del Cliente", 1,
"Todo Exento", 2
          SIZE 21.86 BY 2 TOOLTIP "Marque si el GMF es cargo de la entidad o a cargo del Cliente"
     Pro_Ahorros.VrTope_ExentoEE AT ROW 5.35 COL 63.86 COLON-ALIGNED NO-LABEL FORMAT ">>>>,>>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 16 BY .81 TOOLTIP "Si es a cargo del Cliente y tiene Tope, matricule el valor"
          BGCOLOR 15 FGCOLOR 0 
     Pro_Ahorros.Vr_Subsidio_GMF AT ROW 7.38 COL 63.86 COLON-ALIGNED NO-LABEL WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
          BGCOLOR 15 
     Pro_Ahorros.VrTope_ExentoPension AT ROW 9.27 COL 64.14 COLON-ALIGNED NO-LABEL WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
          BGCOLOR 15 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 5 ROW 6.65
         SIZE 90 BY 15.08
         BGCOLOR 17 FONT 5.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F_Varios
     Pro_Ahorros.Cta_XPagarGMF AT ROW 11 COL 63.72 COLON-ALIGNED NO-LABEL FORMAT "X(14)"
          VIEW-AS FILL-IN 
          SIZE 16 BY .81 TOOLTIP "Cta X Pagar a la DIAN por G.M.F."
          BGCOLOR 15 
     NomCxP AT ROW 11.85 COL 58.86 COLON-ALIGNED NO-LABEL
     Pro_Ahorros.Cta_GtoGMF AT ROW 13.27 COL 63.72 COLON-ALIGNED NO-LABEL FORMAT "X(14)"
          VIEW-AS FILL-IN 
          SIZE 16 BY .81 TOOLTIP "Cta contable del Gasto por GMF"
          BGCOLOR 15 
     NomCGto AT ROW 14.08 COL 58.57 COLON-ALIGNED NO-LABEL
     "Cuenta de Gasto G. M. F." VIEW-AS TEXT
          SIZE 22 BY .58 AT ROW 12.73 COL 63.43
     "Vr Subsidiado a Pensiones" VIEW-AS TEXT
          SIZE 24.29 BY .62 AT ROW 8.58 COL 62.14 WIDGET-ID 12
          BGCOLOR 18 
     "Vr Subsidiado x el Gobierno" VIEW-AS TEXT
          SIZE 24.29 BY .62 AT ROW 4.62 COL 62.14
          BGCOLOR 18 
     "G.    M.    F. :" VIEW-AS TEXT
          SIZE 10.57 BY .62 AT ROW 1.27 COL 63
          BGCOLOR 18 FGCOLOR 0 
     "Vr Subsidiado x la Entidad" VIEW-AS TEXT
          SIZE 24.29 BY .62 AT ROW 6.62 COL 62.14 WIDGET-ID 4
          BGCOLOR 18 
     "Cuenta X Pagar a la DIAN" VIEW-AS TEXT
          SIZE 23.14 BY .54 AT ROW 10.42 COL 63
     RECT-222 AT ROW 2.08 COL 4
     RECT-223 AT ROW 5.31 COL 4
     RECT-224 AT ROW 8.27 COL 4
     RECT-225 AT ROW 2.08 COL 40
     RECT-280 AT ROW 1.54 COL 60
     RECT-281 AT ROW 2.08 COL 61.43
     RECT-282 AT ROW 4.96 COL 61.43 WIDGET-ID 6
     RECT-283 AT ROW 6.96 COL 61.43 WIDGET-ID 8
     RECT-284 AT ROW 8.85 COL 61.57 WIDGET-ID 14
     RECT-285 AT ROW 10.69 COL 3.72 WIDGET-ID 16
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 5 ROW 6.65
         SIZE 90 BY 15.08
         BGCOLOR 17 FONT 5
         TITLE "Otra Información y Gravamen Movimientos Financieros".


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
         TITLE              = "SFG - Configuración de los Productos de Ahorro"
         HEIGHT             = 21.12
         WIDTH              = 113.43
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
ASSIGN FRAME F_General:FRAME = FRAME F_ProAhorros:HANDLE
       FRAME F_Intereses:FRAME = FRAME F_ProAhorros:HANDLE
       FRAME F_Liquidaciones:FRAME = FRAME F_ProAhorros:HANDLE
       FRAME F_Restricciones:FRAME = FRAME F_ProAhorros:HANDLE
       FRAME F_Varios:FRAME = FRAME F_ProAhorros:HANDLE.

/* SETTINGS FOR FRAME F_General
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON Btn_CobroCheq IN FRAME F_General
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON Btn_CobroTal IN FRAME F_General
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Pro_Ahorros.Cta_Revaloriz IN FRAME F_General
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR TOGGLE-BOX Pro_Ahorros.Id_CobroTal IN FRAME F_General
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR TOGGLE-BOX Pro_Ahorros.Id_Consecutivo IN FRAME F_General
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX Pro_Ahorros.Id_Debito IN FRAME F_General
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Pro_Ahorros.Id_Extracto IN FRAME F_General
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Pro_Ahorros.Id_Linea IN FRAME F_General
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Pro_Ahorros.Id_NumAlterno IN FRAME F_General
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Pro_Ahorros.Id_RenVencimiento IN FRAME F_General
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR TOGGLE-BOX Pro_Ahorros.Id_Retparcial IN FRAME F_General
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR TOGGLE-BOX Pro_Ahorros.Id_Revaloriza IN FRAME F_General
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Pro_Ahorros.Id_Vencimiento IN FRAME F_General
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN NomCtaRev IN FRAME F_General
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Pro_Ahorros.Nro_CheqACobrar IN FRAME F_General
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Pro_Ahorros.Num_Consecutivo IN FRAME F_General
   NO-ENABLE 1 3                                                        */
/* SETTINGS FOR FILL-IN Pro_Ahorros.Prioridad IN FRAME F_General
   NO-ENABLE 2                                                          */
/* SETTINGS FOR RADIO-SET Pro_Ahorros.Tip_Vencimiento IN FRAME F_General
   NO-ENABLE 3                                                          */
/* SETTINGS FOR TOGGLE-BOX Pro_Ahorros.Titulo_Valor IN FRAME F_General
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Pro_Ahorros.Val_CadaCheque IN FRAME F_General
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Pro_Ahorros.Val_Talonario IN FRAME F_General
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN W_CobroCheq IN FRAME F_General
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_CobroLibreta IN FRAME F_General
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Intereses
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Intereses:HIDDEN           = TRUE.

/* SETTINGS FOR TOGGLE-BOX Pro_Ahorros.Id_CanCalIntereses IN FRAME F_Intereses
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Pro_Ahorros.Id_Sobregiro IN FRAME F_Intereses
   2 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN Pro_Ahorros.Indicador IN FRAME F_Intereses
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Pro_Ahorros.ProCre_Asociado IN FRAME F_Intereses
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Nin IN FRAME F_Intereses
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN W_Val IN FRAME F_Intereses
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Liquidaciones
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Liquidaciones:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN Pro_Ahorros.Abo_Cuenta IN FRAME F_Liquidaciones
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Pro_Ahorros.Dia_Gracia IN FRAME F_Liquidaciones
   3 EXP-LABEL                                                          */
/* SETTINGS FOR TOGGLE-BOX Pro_Ahorros.Id_PerGracia IN FRAME F_Liquidaciones
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Pro_Ahorros.Id_SdoMinLiquidacion IN FRAME F_Liquidaciones
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Pro_Ahorros.Mon_MinLiqidacion IN FRAME F_Liquidaciones
   3                                                                    */
/* SETTINGS FOR FILL-IN Pro_Ahorros.Per_Pago IN FRAME F_Liquidaciones
   EXP-LABEL                                                            */
/* SETTINGS FOR FRAME F_ProAhorros
                                                                        */
/* SETTINGS FOR BUTTON BUTTON-8 IN FRAME F_ProAhorros
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Pro_Ahorros.Cod_ahorro IN FRAME F_ProAhorros
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN Pro_Ahorros.Fec_Matricula IN FRAME F_ProAhorros
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Pro_Ahorros.Fec_Retiro IN FRAME F_ProAhorros
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Restricciones
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Restricciones:HIDDEN           = TRUE.

/* SETTINGS FOR TOGGLE-BOX Pro_Ahorros.Id_AfeSdoMinimo IN FRAME F_Restricciones
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Pro_Ahorros.Id_BloInactividad IN FRAME F_Restricciones
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Pro_Ahorros.Id_Cuota IN FRAME F_Restricciones
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Pro_Ahorros.Id_MonApertura IN FRAME F_Restricciones
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Pro_Ahorros.Id_Montomaximo IN FRAME F_Restricciones
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Pro_Ahorros.Id_Montominimo IN FRAME F_Restricciones
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Pro_Ahorros.Id_Plazo IN FRAME F_Restricciones
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR TOGGLE-BOX Pro_Ahorros.Id_Salminimo IN FRAME F_Restricciones
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Pro_Ahorros.Pla_Maximo IN FRAME F_Restricciones
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Pro_Ahorros.Pla_Minimo IN FRAME F_Restricciones
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Pro_Ahorros.Tie_Inactividad IN FRAME F_Restricciones
   3                                                                    */
/* SETTINGS FOR RADIO-SET Pro_Ahorros.Tip_Salminimo IN FRAME F_Restricciones
   NO-ENABLE 2 3                                                        */
/* SETTINGS FOR FILL-IN Pro_Ahorros.Val_Cuota IN FRAME F_Restricciones
   3 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN Pro_Ahorros.Val_MaxConsignacion IN FRAME F_Restricciones
   3 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN Pro_Ahorros.Val_MaxRetEfectivo IN FRAME F_Restricciones
   3 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN Pro_Ahorros.Val_Minconsignacion IN FRAME F_Restricciones
   3 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN Pro_Ahorros.Val_MinRetcheque IN FRAME F_Restricciones
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Pro_Ahorros.Val_MinRetiro IN FRAME F_Restricciones
   3 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN Pro_Ahorros.Val_MonAper IN FRAME F_Restricciones
   3                                                                    */
/* SETTINGS FOR FILL-IN Pro_Ahorros.Val_SdoMinimo IN FRAME F_Restricciones
   NO-ENABLE 2 3 EXP-LABEL                                              */
/* SETTINGS FOR FRAME F_Varios
   NOT-VISIBLE Custom                                                   */
ASSIGN 
       FRAME F_Varios:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN Pro_Ahorros.Cta_GtoGMF IN FRAME F_Varios
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Pro_Ahorros.Cta_XPagarGMF IN FRAME F_Varios
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Pro_Ahorros.Dia_Canje IN FRAME F_Varios
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR TOGGLE-BOX Pro_Ahorros.Id_Embargo IN FRAME F_Varios
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR RADIO-SET Pro_Ahorros.Id_GMF IN FRAME F_Varios
   EXP-HELP                                                             */
/* SETTINGS FOR TOGGLE-BOX Pro_Ahorros.Id_Pignoracion IN FRAME F_Varios
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR TOGGLE-BOX Pro_Ahorros.Id_Seguro IN FRAME F_Varios
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Pro_Ahorros.Id_Sorteos IN FRAME F_Varios
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Pro_Ahorros.Id_VerSdoTaq IN FRAME F_Varios
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN NomCGto IN FRAME F_Varios
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN NomCxP IN FRAME F_Varios
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Pro_Ahorros.Num_SMSeguro IN FRAME F_Varios
   3 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN Pro_Ahorros.Porce_Embargo IN FRAME F_Varios
   NO-ENABLE 3 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN Pro_Ahorros.Porce_Pignoracion IN FRAME F_Varios
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Pro_Ahorros.Val_Embargo IN FRAME F_Varios
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Pro_Ahorros.Val_Pignoracion IN FRAME F_Varios
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Pro_Ahorros.VrTope_ExentoEE IN FRAME F_Varios
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN W_CodFormato IN FRAME F_Varios
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_ProDigito IN FRAME F_Varios
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_General
/* Query rebuild information for FRAME F_General
     _Query            is NOT OPENED
*/  /* FRAME F_General */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Intereses
/* Query rebuild information for FRAME F_Intereses
     _Query            is NOT OPENED
*/  /* FRAME F_Intereses */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Liquidaciones
/* Query rebuild information for FRAME F_Liquidaciones
     _Query            is NOT OPENED
*/  /* FRAME F_Liquidaciones */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_ProAhorros
/* Query rebuild information for FRAME F_ProAhorros
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Pro_Ahorros NO-LOCK.
     _END_FREEFORM
     _Query            is OPENED
*/  /* FRAME F_ProAhorros */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Restricciones
/* Query rebuild information for FRAME F_Restricciones
     _Query            is NOT OPENED
*/  /* FRAME F_Restricciones */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Varios
/* Query rebuild information for FRAME F_Varios
     _Query            is NOT OPENED
*/  /* FRAME F_Varios */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* SFG - Configuración de los Productos de Ahorro */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* SFG - Configuración de los Productos de Ahorro */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F_ProAhorros
&Scoped-define FRAME-NAME F_Liquidaciones
&Scoped-define SELF-NAME Pro_Ahorros.Abo_Cuenta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Ahorros.Abo_Cuenta wWin
ON LEAVE OF Pro_Ahorros.Abo_Cuenta IN FRAME F_Liquidaciones /* Abono Cuenta */
DO:
 DO WITH FRAME F_Liquidaciones:
 END.
   IF (INTEGER(Pro_Ahorros.Abo_Cuenta:SCREEN-VALUE) MODULO 30) <> 0 AND 
     INTEGER(Pro_Ahorros.Abo_Cuenta:SCREEN-VALUE) <> 1 THEN
   DO:
     MESSAGE "Valor Invalido" VIEW-AS ALERT-BOX.
     RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Pro_Ahorros.Bas_Calculo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Ahorros.Bas_Calculo wWin
ON VALUE-CHANGED OF Pro_Ahorros.Bas_Calculo IN FRAME F_Liquidaciones /* Valor Base Calculo */
DO:
  IF pro_ahorros.tip_ahorro:SCREEN-VALUE IN FRAME F_ProAhorros EQ "3" AND Pro_Ahorros.Bas_Calculo:SCREEN-VALUE IN FRAME F_Liquidaciones NE "3" THEN
     ASSIGN Pro_Ahorros.Bas_Calculo:SCREEN-VALUE = "3".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_ProAhorros
&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone wWin
ON CHOOSE OF BtnDone IN FRAME F_ProAhorros /* Salir */
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


&Scoped-define SELF-NAME Btn_Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancelar wWin
ON CHOOSE OF Btn_Cancelar IN FRAME F_ProAhorros /* Cancelar */
DO:
  Btn_Consulta:SENSITIVE = TRUE.
  DISABLE Pro_ahorros.tip_ahorro WITH FRAME F_ProAhorros.
  W_Nuevo = NO.
  FIND Pro_Ahorros WHERE ROWID(Pro_Ahorros) EQ W_Puntero NO-ERROR.
  RUN Mostrar_Producto.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_General
&Scoped-define SELF-NAME Btn_CobroCheq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_CobroCheq wWin
ON CHOOSE OF Btn_CobroCheq IN FRAME F_General /* Btn_cobroCheq */
DO:
  RUN C-Transacciones.r (INPUT 4, OUTPUT W_CodOpe, OUTPUT W_NomOpe).
  
  IF W_CodOpe NE 0 THEN 
     W_CobroCheq:SCREEN-VALUE IN FRAME F_General = STRING(W_CodOpe,"999999999") + " - " + W_NomOpe.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_CobroTal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_CobroTal wWin
ON CHOOSE OF Btn_CobroTal IN FRAME F_General /* Button 120 */
DO:
  RUN C-Transacciones.r (INPUT 4, OUTPUT W_CodOpe, OUTPUT W_NomOpe).
  
  IF W_CodOpe NE 0 THEN 
     W_CobroLibreta:SCREEN-VALUE IN FRAME F_General = STRING(W_CodOpe,"999999999") + " - " + W_NomOpe.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_ProAhorros
&Scoped-define SELF-NAME Btn_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Consulta wWin
ON CHOOSE OF Btn_Consulta IN FRAME F_ProAhorros /* Consulta */
DO:
  RUN C-ProAhorros.r (INPUT W_Agencia, OUTPUT P_Cod, OUTPUT p_Nombre, OUTPUT P_AgePro ).
  FIND Pro_Ahorros WHERE pro_ahorros.cod_ahorro EQ P_Cod NO-LOCK NO-ERROR.
  IF AVAILABLE Pro_Ahorros THEN DO:
     W_Puntero = ROWID(Pro_Ahorros).
     RUN Mostrar_Producto.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Generales
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Generales wWin
ON CHOOSE OF Btn_Generales IN FRAME F_ProAhorros /* Generales */
DO:
  HIDE FRAME F_Varios FRAME F_Intereses FRAME F_Restricciones FRAME F_Liquidaciones.
  VIEW FRAME F_General.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Impresion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Impresion wWin
ON CHOOSE OF Btn_Impresion IN FRAME F_ProAhorros /* Imprimir */
DO:
  DEFINE VAR Listado AS CHAR INITIAL "L_ProAho.LST".
  
  Listado = W_PathSpl + Listado.
  {INCLUIDO/Imprimir.I "Listado"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Interes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Interes wWin
ON CHOOSE OF Btn_Interes IN FRAME F_ProAhorros /* Intereses */
DO:
    HIDE FRAME F_Geneneral FRAME F_Varios FRAME F_Restricciones FRAME F_Intereses.
    VIEW FRAME F_Intereses.
    ASSIGN W_Val:SENSITIVE = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Liquidacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Liquidacion wWin
ON CHOOSE OF Btn_Liquidacion IN FRAME F_ProAhorros /* Liquidaciones */
DO:
    HIDE FRAME F_Generales FRAME F_Varios FRAME F_Intereses FRAME F_Restricciones.
    VIEW FRAME F_Liquidaciones.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Restriccion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Restriccion wWin
ON CHOOSE OF Btn_Restriccion IN FRAME F_ProAhorros /* Restricciones */
DO:
HIDE FRAME F_General FRAME F_Varios FRAME F_Intereses FRAME F_Liquidaciones.
    VIEW FRAME F_Restricciones.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salvar wWin
ON CHOOSE OF Btn_Salvar IN FRAME F_ProAhorros /* Salvar */
DO:
  DO WITH FRAME F_Varios: END.
  
  IF Pro_Ahorros.Cta_XPagarGMF:SCREEN-VALUE EQ ? OR Pro_Ahorros.Cta_XPagarGMF:SCREEN-VALUE LE "0"
  OR Pro_Ahorros.Cta_GtoGMF:SCREEN-VALUE    EQ ? OR Pro_Ahorros.Cta_GtoGMF:SCREEN-VALUE    LE "0" THEN DO:
     MESSAGE "Faltan las Ctas-Contables para G.M.F...Operaciòn rechazada." VIEW-AS ALERT-BOX ERROR. 
     RETURN NO-APPLY.
  END.
  
  DO WITH FRAME F_General: END.
  
  IF  Pro_Ahorros.Nro_CheqACobrar:SCREEN-VALUE GT "0"
  AND Pro_Ahorros.Val_CadaCheque:SCREEN-VALUE  LE "0" THEN DO:
      MESSAGE "Falta el valor a Cobrar por Unidad de cheque...Operaciòn rechazada." VIEW-AS ALERT-BOX ERROR. 
      RETURN NO-APPLY.  
  END.
  
  IF  INTEGER(SUBSTRING(W_CobroCheq:SCREEN-VALUE,1,9)) LE 0
  AND Pro_Ahorros.Nro_CheqACobrar:SCREEN-VALUE GT "0" THEN DO:
      MESSAGE "Falta la Operac.para Cobro por Unidad de cheque...Operaciòn rechazada." VIEW-AS ALERT-BOX ERROR. 
      RETURN NO-APPLY.  
  END.
          
  IF W_Nuevo THEN DO:
     CREATE Pro_Ahorros.
     ASSIGN pro_ahorros.cod_ahorro  = INTEGER(pro_ahorros.cod_ahorro:SCREEN-VALUE IN FRAME F_ProAhorros)
            pro_ahorros.tip_ahorro  = INTEGER(pro_ahorros.tip_ahorro:SCREEN-VALUE)
            Pro_Ahorros.Prioridad   = INTEGER(Pro_Ahorros.Prioridad:SCREEN-VALUE IN FRAME F_General).
  END.

  FIND CURRENT Pro_Ahorros NO-ERROR.
  RUN Grabar_Producto.
  FIND CURRENT Pro_Ahorros NO-LOCK NO-ERROR.
  
  DISABLE pro_ahorros.cod_ahorro WITH FRAME F_ProAhorros.
  
  Btn_Consulta:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Varios
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Varios wWin
ON CHOOSE OF Btn_Varios IN FRAME F_ProAhorros /* Varios */
DO:
   HIDE FRAME F_Generales FRAME F_Intereses FRAME F_Restricciones FRAME F_Liquidaciones.
   VIEW FRAME F_Varios.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Varios
&Scoped-define SELF-NAME BUTTON-118
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-118 wWin
ON CHOOSE OF BUTTON-118 IN FRAME F_Varios /* Button 118 */
DO:
  RUN C-Formatos.r (INPUT "AC", OUTPUT WCod, OUTPUT WNom, OUTPUT WAge).
  IF WCod EQ 0 THEN W_CodFormato:SCREEN-VALUE IN FRAME F_Varios = "00 - No Asignado".
  ELSE  W_CodFormato:SCREEN-VALUE IN FRAME F_Varios = STRING(WCod,"99") + " - " + WNom.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-119
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-119 wWin
ON CHOOSE OF BUTTON-119 IN FRAME F_Varios /* Button 119 */
DO:
  RUN C-Formatos.r (INPUT "AC", OUTPUT WCod, OUTPUT WNom, OUTPUT WAge).
  IF WCod EQ 0 THEN W_CodFormato:SCREEN-VALUE IN FRAME F_Varios = "00 - No Asignado".
  ELSE  W_ProDigito:SCREEN-VALUE IN FRAME F_Varios = STRING(WCod,"99") + " - " + WNom.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_ProAhorros
&Scoped-define SELF-NAME BUTTON-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-4 wWin
ON CHOOSE OF BUTTON-4 IN FRAME F_ProAhorros /* Ingresar */
DO:
  ASSIGN W_Puntero = ROWID(Pro_Ahorros)
         W_Nuevo   = YES
         Btn_Consulta:SENSITIVE = FALSE.
         
  ENABLE pro_ahorros.tip_ahorro WITH FRAME F_ProAhorros.
  RUN Inicializar_Campos.
  ENABLE pro_ahorros.cod_ahorro WITH FRAME F_ProAhorros.
  APPLY 'entry' TO pro_ahorros.tip_ahorro IN FRAME F_ProAhorros.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-49
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-49 wWin
ON CHOOSE OF BUTTON-49 IN FRAME F_ProAhorros /* Button 49 */
DO:
  RUN W-InfDia NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-6 wWin
ON CHOOSE OF BUTTON-6 IN FRAME F_ProAhorros /* Deshacer */
DO:
  DISABLE pro_ahorros.tip_ahorro WITH FRAME F_ProAhorros.
  W_Nuevo = NO.
  FIND Pro_Ahorros WHERE ROWID(Pro_Ahorros) EQ W_Puntero NO-ERROR.
  RUN Mostrar_Producto.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Intereses
&Scoped-define SELF-NAME Cmb_Creditos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Creditos wWin
ON VALUE-CHANGED OF Cmb_Creditos IN FRAME F_Intereses /* Producto Asociado */
DO:
  Pro_Ahorros.ProCre_Asociado:SCREEN-VALUE IN FRAME F_Intereses = SUBSTRING(Cmb_Creditos:SCREEN-VALUE IN FRAME {&FRAME-NAME},1,3) NO-ERROR.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_ProAhorros
&Scoped-define SELF-NAME Pro_Ahorros.Cod_ahorro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Ahorros.Cod_ahorro wWin
ON LEAVE OF Pro_Ahorros.Cod_ahorro IN FRAME F_ProAhorros /* Producto */
DO:
DO WITH FRAME F_ProAhorros:
    FIND Pro_Ahorros WHERE 
         pro_ahorros.cod_ahorro EQ INTEGER(pro_ahorros.cod_ahorro:SCREEN-VALUE) NO-ERROR.
    IF AVAILABLE Pro_Ahorros THEN DO:
       MESSAGE "El este producto ya existe para esta Agencia" VIEW-AS ALERT-BOX.
       RUN Mostrar_Producto.
    END.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Varios
&Scoped-define SELF-NAME Pro_Ahorros.Cta_GtoGMF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Ahorros.Cta_GtoGMF wWin
ON LEAVE OF Pro_Ahorros.Cta_GtoGMF IN FRAME F_Varios /* Cuenta */
DO:
  FIND Cuentas WHERE Cuentas.Cuenta     EQ SELF:SCREEN-VALUE
                     AND Cuentas.Tipo   EQ 2 
                     AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR NO-WAIT.
  IF NOT AVAILABLE(Cuentas) THEN
     APPLY "MOUSE-SELECT-DBLCLICK" TO SELF.   
  ELSE ASSIGN NomCGto:SCREEN-VALUE = Cuentas.Nombre. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Ahorros.Cta_GtoGMF wWin
ON MOUSE-SELECT-DBLCLICK OF Pro_Ahorros.Cta_GtoGMF IN FRAME F_Varios /* Cuenta */
DO:
   RUN C-Cuentas.r (OUTPUT W_Pcuenta, OUTPUT W_Pnombre, OUTPUT W_Naturaleza, 
                    OUTPUT W_CtrNat, INPUT "M").
   IF W_PCuenta EQ ? THEN DO:
      FIND Cuentas WHERE Cuentas.Cuenta EQ SELF:SCREEN-VALUE
                     AND Cuentas.Tipo   EQ 2 
                     AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR NO-WAIT.
      IF NOT AVAILABLE(Cuentas) THEN
         ASSIGN SELF:SCREEN-VALUE    = ""
                NomCGto:SCREEN-VALUE = "".           
   END.
   ELSE
      ASSIGN SELF:SCREEN-VALUE    = W_Pcuenta
             NomCGto:SCREEN-VALUE = W_Pnombre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_General
&Scoped-define SELF-NAME Pro_Ahorros.Cta_Revaloriz
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Ahorros.Cta_Revaloriz wWin
ON LEAVE OF Pro_Ahorros.Cta_Revaloriz IN FRAME F_General /* Cuenta */
DO:
      FIND Cuentas WHERE Cuentas.Cuenta EQ SELF:SCREEN-VALUE
                     AND Cuentas.Tipo   EQ 2 
                     AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR NO-WAIT.
      IF NOT AVAILABLE(Cuentas) THEN
         APPLY "MOUSE-SELECT-DBLCLICK" TO SELF.  
      ELSE ASSIGN NomCtaRev:SCREEN-VALUE = Cuentas.nombre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Ahorros.Cta_Revaloriz wWin
ON MOUSE-SELECT-DBLCLICK OF Pro_Ahorros.Cta_Revaloriz IN FRAME F_General /* Cuenta */
DO:
   RUN C-Cuentas.r (OUTPUT W_Pcuenta, OUTPUT W_Pnombre, OUTPUT W_Naturaleza, 
                  OUTPUT W_CtrNat, INPUT "M").
   IF W_PCuenta EQ ? THEN DO:
      FIND Cuentas WHERE Cuentas.Cuenta EQ SELF:SCREEN-VALUE
                     AND Cuentas.Tipo   EQ 2 
                     AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR NO-WAIT.
      IF NOT AVAILABLE(Cuentas) THEN
         ASSIGN SELF:SCREEN-VALUE        = ""
                NomCtaRev:SCREEN-VALUE = "".           
   END.
   ELSE
      ASSIGN SELF:SCREEN-VALUE      = W_Pcuenta
             NomCtaRev:SCREEN-VALUE = W_Pnombre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Varios
&Scoped-define SELF-NAME Pro_Ahorros.Cta_XPagarGMF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Ahorros.Cta_XPagarGMF wWin
ON LEAVE OF Pro_Ahorros.Cta_XPagarGMF IN FRAME F_Varios /* Cuenta */
DO:
  FIND Cuentas WHERE Cuentas.Cuenta EQ SELF:SCREEN-VALUE
                     AND Cuentas.Tipo   EQ 2 
                     AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR NO-WAIT.
  IF NOT AVAILABLE(Cuentas) THEN
     APPLY "MOUSE-SELECT-DBLCLICK" TO SELF.   
  ELSE ASSIGN NomCxP:SCREEN-VALUE = Cuentas.Nombre.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Ahorros.Cta_XPagarGMF wWin
ON MOUSE-SELECT-DBLCLICK OF Pro_Ahorros.Cta_XPagarGMF IN FRAME F_Varios /* Cuenta */
DO:
   RUN C-Cuentas.r (OUTPUT W_Pcuenta, OUTPUT W_Pnombre, OUTPUT W_Naturaleza, 
                    OUTPUT W_CtrNat, INPUT "M").
   IF W_PCuenta EQ ? THEN DO:
      FIND Cuentas WHERE Cuentas.Cuenta EQ SELF:SCREEN-VALUE
                     AND Cuentas.Tipo   EQ 2 
                     AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR NO-WAIT.
      IF NOT AVAILABLE(Cuentas) THEN
         ASSIGN SELF:SCREEN-VALUE   = ""
                NomCxP:SCREEN-VALUE = "".           
   END.
   ELSE
      ASSIGN SELF:SCREEN-VALUE   = W_Pcuenta
             NomCxP:SCREEN-VALUE = W_Pnombre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_ProAhorros
&Scoped-define SELF-NAME Pro_Ahorros.Estado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Ahorros.Estado wWin
ON VALUE-CHANGED OF Pro_Ahorros.Estado IN FRAME F_ProAhorros /* Estado */
DO:
  IF Pro_Ahorros.Estado:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "2" THEN DO:
     FIND FIRST Ahorros WHERE Ahorros.Cod_ahorro EQ pro_ahorros.cod_ahorro 
                         NO-LOCK NO-ERROR.
     IF AVAILABLE(Ahorros) THEN DO:
        ASSIGN Pro_Ahorros.Estado:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "1".
        RUN MostrarMensaje IN W_Manija (INPUT 101,OUTPUT W_Eleccion).
     END.
     ELSE
        IF Pro_Ahorros.Fec_Retiro:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ ? THEN        
           Pro_Ahorros.Fec_Retiro:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Restricciones
&Scoped-define SELF-NAME Pro_Ahorros.Id_BloInactividad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Ahorros.Id_BloInactividad wWin
ON VALUE-CHANGED OF Pro_Ahorros.Id_BloInactividad IN FRAME F_Restricciones /* Maneja Bloqueo por Inactividad */
DO:
DO WITH FRAME F_Restricciones:
    IF Pro_Ahorros.Id_BloInactividad:SCREEN-VALUE = "No" THEN
      DO:
        DISABLE Pro_Ahorros.Tie_Inactividad.
        ASSIGN  Pro_Ahorros.Tie_Inactividad:SCREEN-VALUE = "".
      END.
    ELSE
      DO:
        ENABLE Pro_Ahorros.Tie_Inactividad.
        APPLY "ENTRY" TO Pro_Ahorros.Tie_Inactividad.
      END.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_General
&Scoped-define SELF-NAME Pro_Ahorros.Id_CobroTal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Ahorros.Id_CobroTal wWin
ON VALUE-CHANGED OF Pro_Ahorros.Id_CobroTal IN FRAME F_General /* Maneja Cobro Talonario */
DO:
DO WITH FRAME F_General:
    IF SELF:SCREEN-VALUE EQ "yes" THEN DO: 
      ENABLE Pro_Ahorros.Val_Talonario Btn_CobroTal.
      APPLY 'entry' TO Pro_Ahorros.Val_Talonario.
    END.
    ELSE DO: 
      DISABLE Pro_Ahorros.Val_Talonario Btn_CobroTal.
      ASSIGN Pro_Ahorros.Val_Talonario:SCREEN-VALUE = "0"
             W_CobroLibreta:SCREEN-VALUE = "".
    END.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Pro_Ahorros.Id_Consecutivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Ahorros.Id_Consecutivo wWin
ON VALUE-CHANGED OF Pro_Ahorros.Id_Consecutivo IN FRAME F_General /* Maneja Consecutivo */
DO:
DO WITH FRAME F_General:
    IF Pro_Ahorros.Id_Consecutivo:SCREEN-VALUE = "No" THEN
      DO:
        DISABLE Pro_Ahorros.Num_Consecutivo.
        ASSIGN Num_Consecutivo:SCREEN-VALUE = "".
        DISABLE Num_Consecutivo.
      END.
    ELSE
      DO:
        ENABLE Num_Consecutivo.
        APPLY "ENTRY" TO Pro_Ahorros.Num_Consecutivo.
      END.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Restricciones
&Scoped-define SELF-NAME Pro_Ahorros.Id_Cuota
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Ahorros.Id_Cuota wWin
ON VALUE-CHANGED OF Pro_Ahorros.Id_Cuota IN FRAME F_Restricciones /* Maneja Cuota */
DO:
 DO WITH FRAME F_Restricciones:
     IF  Pro_Ahorros.Id_Cuota:SCREEN-VALUE = "No" THEN DO:
         IF pro_ahorros.tip_ahorro:SCREEN-VALUE IN FRAME F_ProAhorros NE "2" THEN DO:
            DISABLE Pro_Ahorros.Val_Cuota.
            ASSIGN  Val_Cuota:SCREEN-VALUE = "".
         END.
         ELSE DO:
             MESSAGE "Este producto es un contractual y debe llevar cuota"
                 VIEW-AS ALERT-BOX WARNING.
             Pro_Ahorros.Id_Cuota:SCREEN-VALUE = "Yes".
         END.
     END.
     ELSE DO:
/*         Pro_Ahorros.Id_Cuota:SCREEN-VALUE = "Yes".*/
         ENABLE Pro_Ahorros.Val_Cuota.
         APPLY "ENTRY" TO Pro_Ahorros.Val_Cuota.
     END.
 END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_General
&Scoped-define SELF-NAME Pro_Ahorros.Id_Debito
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Ahorros.Id_Debito wWin
ON VALUE-CHANGED OF Pro_Ahorros.Id_Debito IN FRAME F_General /* Admite Débito Automático */
DO:
  IF pro_ahorros.tip_ahorro:SCREEN-VALUE IN FRAME F_ProAhorros NE "1" THEN
   DO:
     APPLY "TAB" TO Pro_Ahorros.Id_Debito IN FRAME F_General.
     RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Varios
&Scoped-define SELF-NAME Pro_Ahorros.Id_Embargo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Ahorros.Id_Embargo wWin
ON VALUE-CHANGED OF Pro_Ahorros.Id_Embargo IN FRAME F_Varios /* Maneja Embargo */
DO:
  DO WITH FRAME F_Varios:
     IF Pro_Ahorros.Id_Embargo:SCREEN-VALUE EQ "NO" THEN DO: 
        ASSIGN Pro_Ahorros.Porce_Embargo:SCREEN-VALUE = "0"
               Pro_Ahorros.Val_Embargo:SCREEN-VALUE   = "0".
        DISABLE Pro_Ahorros.Val_Embargo Pro_Ahorros.Porce_Embargo.
     END.
     ELSE DO:
        ENABLE Pro_Ahorros.Val_Embargo Pro_Ahorros.Porce_Embargo.
        APPLY "ENTRY" TO Pro_Ahorros.Val_Embargo.
        RETURN NO-APPLY.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Pro_Ahorros.Id_GMF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Ahorros.Id_GMF wWin
ON MOUSE-SELECT-CLICK OF Pro_Ahorros.Id_GMF IN FRAME F_Varios /* Id_GMF */
DO:
  ASSIGN Pro_Ahorros.VrTope_ExentoEE:SENSITIVE = FALSE
         Pro_Ahorros.Vr_Subsidio_GMF:SENSITIVE = FALSE.
  IF Pro_Ahorros.Id_GMF:SCREEN-VALUE EQ "1" THEN DO:
     ASSIGN Pro_Ahorros.VrTope_ExentoEE:SENSITIVE = TRUE
            Pro_Ahorros.Vr_Subsidio_GMF:SENSITIVE = TRUE.
     APPLY "ENTRY" TO VrTope_ExentoEE.
  END.
  ELSE ASSIGN Pro_Ahorros.VrTope_ExentoEE:SCREEN-VALUE = "0.00"
              Pro_Ahorros.Vr_Subsidio_GMF:SCREEN-VALUE = "0.00".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Restricciones
&Scoped-define SELF-NAME Pro_Ahorros.Id_MonApertura
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Ahorros.Id_MonApertura wWin
ON VALUE-CHANGED OF Pro_Ahorros.Id_MonApertura IN FRAME F_Restricciones /* Maneja Monto Apertura */
DO:
DO WITH FRAME F_Restricciones:
    IF  Pro_Ahorros.Id_MonApertura:SCREEN-VALUE = "No" 
    AND pro_ahorros.tip_ahorro:SCREEN-VALUE IN FRAME F_ProAhorros NE "1" 
    AND pro_ahorros.tip_ahorro:SCREEN-VALUE IN FRAME F_ProAhorros NE "3" THEN DO:
        DISABLE Pro_Ahorros.Val_MonAper.
        ASSIGN  Pro_Ahorros.Val_MonAper:SCREEN-VALUE = "".
    END.
    ELSE DO:
        Pro_Ahorros.Id_MonApertura:SCREEN-VALUE = "Yes".
        ENABLE Pro_Ahorros.Val_MonAper.
        APPLY "ENTRY" TO Pro_Ahorros.Val_MonAper.
    END.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Pro_Ahorros.Id_Montomaximo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Ahorros.Id_Montomaximo wWin
ON VALUE-CHANGED OF Pro_Ahorros.Id_Montomaximo IN FRAME F_Restricciones /* Maneja Monto Máximo */
DO:
DO WITH FRAME F_Restricciones:
    IF Pro_Ahorros.Id_RetParcial:SCREEN-VALUE IN FRAME F_General EQ "NO" THEN DO:
       DISABLE Pro_Ahorros.Id_MontoMinimo
               Pro_Ahorros.Id_MontoMaximo.
       ASSIGN  Pro_Ahorros.Id_MontoMinimo:SCREEN-VALUE     = "No"
               Pro_Ahorros.Id_MontoMaximo:SCREEN-VALUE     = "No"                
               Pro_Ahorros.Val_MinRetiro:SCREEN-VALUE      = ""
               Pro_Ahorros.Val_MaxRetEfectivo:SCREEN-VALUE = ""
               Pro_Ahorros.Val_MinRetCheque:SCREEN-VALUE   = "".
       APPLY "ENTRY" TO Pro_Ahorros.Val_MinConsignacion.
       RETURN NO-APPLY.
    END.
    IF Pro_Ahorros.Id_MontoMaximo:SCREEN-VALUE = "No" THEN
      DO:
        DISABLE Pro_Ahorros.Val_MaxRetEfectivo Pro_Ahorros.Val_MaxConsignacion.
        ASSIGN  Pro_Ahorros.Val_MaxRetEfectivo:SCREEN-VALUE = ""
                Pro_Ahorros.Val_MaxConsignacion:SCREEN-VALUE = "".
      END.
    ELSE
      DO:
        ENABLE Pro_Ahorros.Val_MaxRetEfectivo Pro_Ahorros.Val_MaxConsignacion. 
        APPLY "ENTRY" TO Pro_Ahorros.Val_MaxConsignacion.
      END.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Pro_Ahorros.Id_Montominimo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Ahorros.Id_Montominimo wWin
ON VALUE-CHANGED OF Pro_Ahorros.Id_Montominimo IN FRAME F_Restricciones /* Maneja Monto Mínimo */
DO:
 DO WITH FRAME F_Restricciones:
     IF Pro_Ahorros.Id_RetParcial:SCREEN-VALUE IN FRAME F_General EQ "NO" THEN DO:
        MESSAGE "El parametro de Manejo de Retiros parciales" SKIP
                "Se encuentra inactivo. No se permite configurar" SKIP
                "los topes minimos ni maximos de retiro!." VIEW-AS ALERT-BOX INFORMATION.
        DISABLE Pro_Ahorros.Val_MinRetiro Pro_Ahorros.Val_MaxRetEfectivo Pro_Ahorros.Val_MinRetCheque.
        ASSIGN  Pro_Ahorros.Val_MinRetiro:SCREEN-VALUE      = ""
                Pro_Ahorros.Val_MaxRetEfectivo:SCREEN-VALUE = ""
                Pro_Ahorros.Val_MinRetCheque:SCREEN-VALUE   = "".
        APPLY "ENTRY" TO Pro_Ahorros.Val_MinConsignacion.
        RETURN NO-APPLY.
     END.
     ELSE DO:
        ENABLE Pro_Ahorros.Val_MinRetiro Pro_Ahorros.Val_MaxRetEfectivo Pro_Ahorros.Val_MinRetCheque.
     END.
     IF Pro_Ahorros.Id_MontoMinimo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "No" THEN
       DO:
         DISABLE Pro_Ahorros.Val_MinRetiro Pro_Ahorros.Val_MinRetCheque Pro_Ahorros.Val_MinConsignacion WITH FRAME {&FRAME-NAME}.
         ASSIGN  Val_MinRetiro:SCREEN-VALUE = ""
                 Val_MinConsignacion:SCREEN-VALUE = ""
                 Val_MinRetCheque:SCREEN-VALUE = "".
       END.
     ELSE
       DO:
         ENABLE Pro_Ahorros.Val_MinRetiro Pro_Ahorros.Val_MinConsignacion Pro_Ahorros.Val_MinRetCheque WITH FRAME {&FRAME-NAME}.
         APPLY "ENTRY" TO Pro_Ahorros.Val_MinRetiro.
       END.
 END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Liquidaciones
&Scoped-define SELF-NAME Pro_Ahorros.Id_PerGracia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Ahorros.Id_PerGracia wWin
ON VALUE-CHANGED OF Pro_Ahorros.Id_PerGracia IN FRAME F_Liquidaciones /* Período de Gracia */
DO:
DO WITH FRAME F_Liquidaciones:
    IF Pro_Ahorros.Id_Pergracia:SCREEN-VALUE = "No" THEN
      DO:
        DISABLE Pro_Ahorros.Dia_Gracia.
        ASSIGN  Pro_Ahorros.Dia_Gracia:SCREEN-VALUE = "".
      END.
    ELSE
      DO:
        ENABLE Pro_Ahorros.Dia_Gracia.
        APPLY "ENTRY" TO Pro_Ahorros.Dia_Gracia.
      END.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Pro_Ahorros.Id_perliquidacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Ahorros.Id_perliquidacion wWin
ON VALUE-CHANGED OF Pro_Ahorros.Id_perliquidacion IN FRAME F_Liquidaciones /* Liquidación por */
DO:
  IF INTEGER(Pro_Ahorros.Id_PerLiquidacion:SCREEN-VALUE IN FRAME {&FRAME-NAME}) = 1 THEN
     ENABLE W_CmbPeriodo Pro_Ahorros.Abo_Cuenta Pro_Ahorros.Per_Pago WITH FRAME {&FRAME-NAME}.
  ELSE DO:
      DISABLE W_CmbPeriodo Pro_Ahorros.Abo_Cuenta Pro_Ahorros.Per_Pago WITH FRAME {&FRAME-NAME}.
      ASSIGN Pro_ahorros.Abo_Cuenta:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0"
             Pro_ahorros.Per_Pago:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Varios
&Scoped-define SELF-NAME Pro_Ahorros.Id_Pignoracion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Ahorros.Id_Pignoracion wWin
ON VALUE-CHANGED OF Pro_Ahorros.Id_Pignoracion IN FRAME F_Varios /* Maneja Pignoración */
DO:
  DO WITH FRAME F_Varios:
    IF Pro_Ahorros.Id_Pignoracion:SCREEN-VALUE = STRING(FALSE) THEN DO:
       ASSIGN Pro_Ahorros.Val_Pignoracion:SCREEN-VALUE   = "0"
              Pro_Ahorros.Porce_Pignoracion:SCREEN-VALUE = "0".    
       DISABLE Pro_Ahorros.Val_Pignoracion Pro_Ahorros.Porce_Pignoracion.
    END.
    ELSE DO:
       ENABLE Pro_Ahorros.Val_Pignoracion Pro_Ahorros.Porce_Pignoracion.
       APPLY "ENTRY" TO Pro_Ahorros.Val_Pignoracion.
       RETURN NO-APPLY.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Restricciones
&Scoped-define SELF-NAME Pro_Ahorros.Id_Plazo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Ahorros.Id_Plazo wWin
ON VALUE-CHANGED OF Pro_Ahorros.Id_Plazo IN FRAME F_Restricciones /* Maneja Plazos en días */
OR LEAVE OF Pro_Ahorros.Id_Plazo DO:
      
  DO WITH FRAME Restricciones:
  IF Pro_Ahorros.Id_Plazo:SCREEN-VALUE = "No" THEN
    DO:
      DISABLE Pro_Ahorros.Pla_Minimo Pla_Maximo.
      ASSIGN  Pro_Ahorros.Pla_Minimo:SCREEN-VALUE = ""
              Pro_Ahorros.Pla_Maximo:SCREEN-VALUE = "".
    END.
  ELSE
    DO:
      ENABLE Pro_Ahorros.Pla_Minimo Pro_Ahorros.Pla_Maximo.
      APPLY "ENTRY" TO Pro_Ahorros.Pla_Minimo.
      RETURN NO-APPLY.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_General
&Scoped-define SELF-NAME Pro_Ahorros.Id_Retparcial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Ahorros.Id_Retparcial wWin
ON VALUE-CHANGED OF Pro_Ahorros.Id_Retparcial IN FRAME F_General /* Maneja Retiros Parciales */
DO:
  DEFINE VAR choice AS LOGICAL.
  IF SELF:SCREEN-VALUE EQ "no" THEN DO:
     MESSAGE "La configuracion de los valores minimos y maximos de retiro" SKIP
             "sera anulada y desactivada!" SKIP(1)
             "Desea que se efectue la inactivacion de los retiros parciales?"
             VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO UPDATE choice.
     IF choice THEN DO:
       DISABLE Pro_Ahorros.Val_MinRetiro Pro_Ahorros.Val_MaxRetEfectivo Pro_Ahorros.Val_MinRetCheque WITH FRAME F_Restricciones.
       ASSIGN Pro_Ahorros.Val_MinRetiro:SCREEN-VALUE IN FRAME F_Restricciones = "0"
              Pro_Ahorros.Val_MaxRetEfectivo:SCREEN-VALUE IN FRAME F_Restricciones = "0"
              Pro_Ahorros.Val_MinRetCheque:SCREEN-VALUE IN FRAME F_Restricciones = "0".
     END.
     ELSE
       SELF:SCREEN-VALUE = "yes".
  END.
  ELSE DO:
   ENABLE Pro_Ahorros.Val_MinRetiro Pro_Ahorros.Val_MaxRetEfectivo Pro_Ahorros.Val_MinRetCheque WITH FRAME F_Restricciones.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Pro_Ahorros.Id_Revaloriza
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Ahorros.Id_Revaloriza wWin
ON VALUE-CHANGED OF Pro_Ahorros.Id_Revaloriza IN FRAME F_General /* Id. Saldo Revalorización? */
DO:
  IF SELF:SCREEN-VALUE EQ "yes" THEN DO:
     ENABLE Pro_Ahorros.Cta_Revaloriz WITH FRAME F_General.
     APPLY "entry" TO Pro_Ahorros.Cta_Revaloriz IN FRAME F_General.
     RETURN NO-APPLY.
  END.
  ELSE DO:
     ASSIGN Pro_Ahorros.Cta_Revaloriz:SCREEN-VALUE = ""
            NomCtaRev:SCREEN-VALUE = "".
     DISABLE Pro_Ahorros.Cta_Revaloriz WITH FRAME F_General.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Restricciones
&Scoped-define SELF-NAME Pro_Ahorros.Id_Salminimo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Ahorros.Id_Salminimo wWin
ON VALUE-CHANGED OF Pro_Ahorros.Id_Salminimo IN FRAME F_Restricciones /* Maneja Saldo Mínimo */
DO:
  DO WITH FRAME F_Restricciones:
     /*Pro_Ahorros.Val_SdoMinimo:SENSITIVE = TRUE.*/
     IF Pro_Ahorros.Id_RetParcial:SCREEN-VALUE IN FRAME F_General EQ "NO" OR 
        pro_ahorros.tip_ahorro:SCREEN-VALUE IN FRAME F_ProAhorros EQ "3"  THEN DO:
        ASSIGN Pro_Ahorros.Id_Salminimo:SCREEN-VALUE = "No"
               Pro_Ahorros.Val_SdoMinimo:SENSITIVE = FALSE.
        RETURN NO-APPLY.
     END.
     
     IF Pro_Ahorros.Id_SalMinimo:SCREEN-VALUE EQ "No" THEN
        ASSIGN Pro_Ahorros.Tip_SalMinimo:SENSITIVE      = FALSE 
               Pro_Ahorros.Val_SdoMinimo:SENSITIVE      = FALSE
               Pro_Ahorros.Id_AfeSdoMinimo:SENSITIVE    = FALSE
               Pro_Ahorros.Val_SdoMinimo:SCREEN-VALUE   = ""
               Pro_Ahorros.Id_AfeSdoMinimo:SCREEN-VALUE = "No".
     ELSE 
        ASSIGN Pro_Ahorros.Tip_SalMinimo:SENSITIVE   = TRUE 
               Pro_Ahorros.Val_SdoMinimo:SENSITIVE   = TRUE
               Pro_Ahorros.Id_AfeSdoMinimo:SENSITIVE = TRUE.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Liquidaciones
&Scoped-define SELF-NAME Pro_Ahorros.Id_SdoMinLiquidacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Ahorros.Id_SdoMinLiquidacion wWin
ON VALUE-CHANGED OF Pro_Ahorros.Id_SdoMinLiquidacion IN FRAME F_Liquidaciones /* Saldo Mínimo Liquidación */
DO:
DO WITH FRAME F_Liquidaciones:
    IF Pro_Ahorros.Id_SdoMinLiquidacion:SCREEN-VALUE = "No" THEN
      DO:
        DISABLE Pro_Ahorros.Mon_MinLiqidacion.
        ASSIGN  Pro_Ahorros.Mon_MinLiqidacion:SCREEN-VALUE = "".
      END.
    ELSE
      DO:
        ENABLE Pro_Ahorros.Mon_MinLiqidacion.
        APPLY "ENTRY" TO Pro_Ahorros.Mon_MinLiqidacion.
      END.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Varios
&Scoped-define SELF-NAME Pro_Ahorros.Id_Seguro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Ahorros.Id_Seguro wWin
ON VALUE-CHANGED OF Pro_Ahorros.Id_Seguro IN FRAME F_Varios /* El Producto tiene seguro? */
DO:
DO WITH FRAME F_Varios:
    IF Pro_Ahorros.Id_Seguro:SCREEN-VALUE = "No" THEN DO:
        DISABLE Pro_Ahorros.Num_SMSeguro.
        ASSIGN Pro_Ahorros.Num_SMSeguro:SCREEN-VALUE = "0".
    END.
    ELSE DO:
       ENABLE Pro_Ahorros.Num_SMSeguro.
       APPLY "ENTRY" TO Pro_Ahorros.Num_SMSeguro.
    END.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Intereses
&Scoped-define SELF-NAME Pro_Ahorros.Id_Sobregiro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Ahorros.Id_Sobregiro wWin
ON VALUE-CHANGED OF Pro_Ahorros.Id_Sobregiro IN FRAME F_Intereses /* Maneja Sobregiro */
OR LEAVE OF Pro_Ahorros.Id_Sobregiro DO:
   DO WITH FRAME Intereses:
      IF Pro_Ahorros.Id_Sobregiro:SCREEN-VALUE EQ "YES" THEN
         ASSIGN Cmb_Creditos:SENSITIVE = TRUE.
      ELSE DO:
         ASSIGN Cmb_Creditos:SENSITIVE    = FALSE.
     END.
     APPLY "VALUE-CHANGED":U TO Cmb_Creditos.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Varios
&Scoped-define SELF-NAME Pro_Ahorros.Id_Sorteos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Ahorros.Id_Sorteos wWin
ON VALUE-CHANGED OF Pro_Ahorros.Id_Sorteos IN FRAME F_Varios /* Maneja Sorteos */
DO:
DO WITH FRAME F_Varios:
    IF Pro_Ahorros.Id_Sorteos:SCREEN-VALUE = "No" THEN
      DO:
        DISABLE Pro_Ahorros.Periodicidad.
      END.
    ELSE
      DO:
         ENABLE Pro_Ahorros.Periodicidad.
         APPLY "ENTRY" TO Pro_Ahorros.Periodicidad.
      END.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_General
&Scoped-define SELF-NAME Pro_Ahorros.Id_Talonario
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Ahorros.Id_Talonario wWin
ON VALUE-CHANGED OF Pro_Ahorros.Id_Talonario IN FRAME F_General /* Talonario */
DO:
  DO WITH FRAME F_General:
     IF pro_ahorros.tip_ahorro:SCREEN-VALUE IN FRAME F_ProAhorros EQ "1" THEN DO:
        IF SELF:SCREEN-VALUE NE "3" THEN DO:
           ENABLE Pro_Ahorros.Id_CobroTal.
           APPLY 'entry' TO Pro_Ahorros.Id_CobroTal.
        END.
     END.
     ELSE DO:
        IF SELF:SCREEN-VALUE NE "3" THEN DO:
           ASSIGN SELF:SCREEN-VALUE = "3"
                  Pro_Ahorros.Id_CobroTal:SCREEN-VALUE = "NO"
                  Pro_Ahorros.Val_Talonario:SCREEN-VALUE = "".
           DISABLE Pro_Ahorros.Id_CobroTal Pro_Ahorros.Val_Talonario.
        END.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Intereses
&Scoped-define SELF-NAME Pro_Ahorros.Id_Tasa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Ahorros.Id_Tasa wWin
ON VALUE-CHANGED OF Pro_Ahorros.Id_Tasa IN FRAME F_Intereses /* Manejo de Tasa de Interes */
DO:
  DO WITH FRAME F_Intereses:
     ASSIGN Pro_Ahorros.Indicador:SCREEN-VALUE = "0"
            W_Nin:SCREEN-VALUE                = ""
            W_Val:SCREEN-VALUE                = "0"
            W_Nin                             = ""   
            W_Val                             = 0.
  
     IF Pro_Ahorros.Id_Tasa:SCREEN-VALUE EQ "1" THEN DO:
        Pro_Ahorros.Indicador:SENSITIVE = TRUE.
        APPLY "ENTRY":U TO Pro_Ahorros.Indicador.
        RETURN NO-APPLY.
     END.
     ELSE 
        Pro_Ahorros.Indicador:SENSITIVE = FALSE.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_General
&Scoped-define SELF-NAME Pro_Ahorros.Id_Vencimiento
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Ahorros.Id_Vencimiento wWin
ON VALUE-CHANGED OF Pro_Ahorros.Id_Vencimiento IN FRAME F_General /* Maneja Vencimientos */
DO:
DO WITH FRAME F_General:
  IF Pro_Ahorros.Id_Vencimiento:SCREEN-VALUE = "No" THEN
  DO:
    DISABLE Pro_Ahorros.Id_RenVencimiento Pro_Ahorros.Tip_Vencimiento.
    ASSIGN Pro_Ahorros.Tip_Vencimiento:SCREEN-VALUE = "1"
           Pro_Ahorros.Id_RenVencimiento:SCREEN-VALUE = "No".
  END.
  ELSE
    DO:
      ENABLE Pro_Ahorros.Id_RenVencimiento Pro_Ahorros.Tip_Vencimiento.
      APPLY "ENTRY" TO Pro_Ahorros.Tip_Vencimiento.
    END.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Intereses
&Scoped-define SELF-NAME Pro_Ahorros.Indicador
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Ahorros.Indicador wWin
ON LEAVE OF Pro_Ahorros.Indicador IN FRAME F_Intereses /* Indicador */
DO:
  DO WITH FRAME Intereses:
     ASSIGN W_Nin              = ""
            W_Val              = 0    
            W_Nin:SCREEN-VALUE = ""
            W_Val:SCREEN-VALUE = "0".
     IF AVAILABLE(Pro_Ahorros) THEN DO:
        FIND FIRST Indicadores WHERE Indicadores.Indicador EQ INTEGER(Pro_Ahorros.Indicador:SCREEN-VALUE) 
                               AND Indicadores.FecVcto     GE TODAY 
                               AND Indicadores.Estado      EQ 1 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE(Indicadores) THEN 
           FIND FIRST Indicadores WHERE Indicadores.Indicador EQ INTEGER(Pro_Ahorros.Indicador:SCREEN-VALUE) 
                                  AND Indicadores.Estado      EQ 1 NO-LOCK NO-ERROR.
        
        IF NOT AVAILABLE(Indicadores) THEN DO:   
           RUN C-indicadores.r (OUTPUT W_Ind, OUTPUT W_Nin, OUTPUT W_Val, OUTPUT W_Cero).
           
           ASSIGN Pro_Ahorros.Indicador:SCREEN-VALUE = STRING(W_Ind)
                  W_Nin:SCREEN-VALUE                 = W_Nin
                  W_Val:SCREEN-VALUE                 = STRING(W_Val).
                  
           IF INTEGER(Pro_Ahorros.Indicador:SCREEN-VALUE) EQ 0   AND
                      Pro_Ahorros.Id_Tasa:SCREEN-VALUE   EQ "1" THEN DO:
              APPLY "ENTRY":U TO Pro_Ahorros.Indicador.
              RETURN NO-APPLY.                                                       
           END.
        END.
        ELSE
           ASSIGN Pro_Ahorros.Indicador:SCREEN-VALUE = STRING(Indicadores.Indicador)
                  W_Nin:SCREEN-VALUE                = Indicadores.Nombre
                  W_Val:SCREEN-VALUE                = STRING(Indicadores.Tasa).
     END.
  END.
  
  ASSIGN W_Nin
         W_Val.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_General
&Scoped-define SELF-NAME Pro_Ahorros.Nro_CheqACobrar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Ahorros.Nro_CheqACobrar wWin
ON LEAVE OF Pro_Ahorros.Nro_CheqACobrar IN FRAME F_General /* Nro de Cupones por Talonario */
DO:
  IF Pro_Ahorros.Nro_CheqACobrar:SCREEN-VALUE LE "0"  THEN 
     Pro_Ahorros.Val_CadaCheque:SCREEN-VALUE  = "0.00".
  ELSE APPLY "ENTRY" TO Pro_Ahorros.Val_CadaCheque.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Liquidaciones
&Scoped-define SELF-NAME Pro_Ahorros.Per_Pago
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Ahorros.Per_Pago wWin
ON LEAVE OF Pro_Ahorros.Per_Pago IN FRAME F_Liquidaciones /* Periodo de Pago */
DO:
DO WITH FRAME F_Liquidaciones:
    IF (INTEGER(Pro_Ahorros.Per_Pago:SCREEN-VALUE) MODULO 30) <> 0 AND 
      INTEGER(Pro_Ahorros.Per_Pago:SCREEN-VALUE) <> 1 THEN
    DO:
      MESSAGE "Valor Invalido" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
    END.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Restricciones
&Scoped-define SELF-NAME Pro_Ahorros.Pla_Minimo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Ahorros.Pla_Minimo wWin
ON LEAVE OF Pro_Ahorros.Pla_Minimo IN FRAME F_Restricciones /* Mínimo */
OR VALUE-CHANGED OF Pro_Ahorros.Pla_Minimo DO:
   IF Pro_Ahorros.Id_Plazo:SCREEN-VALUE EQ "yes" THEN DO:
      APPLY "ENTRY" TO Pro_Ahorros.Pla_Maximo.
      RETURN NO-APPLY.  
   END.
   ELSE DISABLE Pro_Ahorros.Pla_Minimo Pro_Ahorros.Pla_Maximo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_ProAhorros
&Scoped-define SELF-NAME Pro_Ahorros.Tip_Ahorro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Ahorros.Tip_Ahorro wWin
ON MOUSE-SELECT-CLICK OF Pro_Ahorros.Tip_Ahorro IN FRAME F_ProAhorros /* Tipo Producto */
DO:
  APPLY "VALUE-CHANGED" TO pro_ahorros.tip_ahorro.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Liquidaciones
&Scoped-define SELF-NAME Pro_Ahorros.Tip_Anualidad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Ahorros.Tip_Anualidad wWin
ON VALUE-CHANGED OF Pro_Ahorros.Tip_Anualidad IN FRAME F_Liquidaciones /* Tipo Anualidad */
DO:
  IF pro_ahorros.tip_ahorro:SCREEN-VALUE IN FRAME F_ProAhorros EQ "1" AND 
     Pro_Ahorros.Tip_Anualidad:SCREEN-VALUE IN FRAME F_Liquidaciones NE "no" THEN
     ASSIGN Pro_Ahorros.Tip_Anualidad:SCREEN-VALUE IN FRAME F_Liquidaciones = "no".  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Restricciones
&Scoped-define SELF-NAME Pro_Ahorros.Tip_Salminimo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Ahorros.Tip_Salminimo wWin
ON VALUE-CHANGED OF Pro_Ahorros.Tip_Salminimo IN FRAME F_Restricciones /* Tipo Saldo Minimo */
DO:
  DO WITH FRAME F_Restricciones:
     IF Pro_Ahorros.Tip_SalMinimo:SCREEN-VALUE EQ "No" OR 
        Pro_Ahorros.Id_Salminimo:SCREEN-VALUE  EQ "No" THEN
        ASSIGN Pro_Ahorros.Val_SdoMinimo:SCREEN-VALUE = "0"
               Pro_Ahorros.Val_SdoMinimo:SENSITIVE    = FALSE.
     ELSE
        Pro_Ahorros.Val_SdoMinimo:SENSITIVE = TRUE.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_General
&Scoped-define SELF-NAME Pro_Ahorros.Tip_Vencimiento
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Ahorros.Tip_Vencimiento wWin
ON VALUE-CHANGED OF Pro_Ahorros.Tip_Vencimiento IN FRAME F_General /* Tipo Vencimiento */
DO:
  IF pro_ahorros.tip_ahorro:SCREEN-VALUE in FRAME F_ProAhorros EQ "3" THEN 
     ASSIGN Pro_Ahorros.Tip_Vencimiento:SCREEN-VALUE IN FRAME F_General = "1".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Restricciones
&Scoped-define SELF-NAME Pro_Ahorros.Val_SdoMinimo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Ahorros.Val_SdoMinimo wWin
ON LEAVE OF Pro_Ahorros.Val_SdoMinimo IN FRAME F_Restricciones /* Vlr Saldo Mínimo */
DO:
DO WITH FRAME F_Restricciones:
    IF  Pro_Ahorros.Tip_SalMinimo:SCREEN-VALUE EQ "No" THEN DO:
        ASSIGN Pro_Ahorros.Val_SdoMinimo:SCREEN-VALUE = "0".
        DISABLE Pro_Ahorros.Val_SdoMinimo.
        RETURN.
    END.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_General
&Scoped-define SELF-NAME Pro_Ahorros.Val_Talonario
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Ahorros.Val_Talonario wWin
ON LEAVE OF Pro_Ahorros.Val_Talonario IN FRAME F_General /* Valor Unidad */
DO:
DO WITH FRAME F_General:
    IF DECIMAL(SELF:SCREEN-VALUE) EQ 0 THEN DO:
       MESSAGE "El valor debe ser mayor a cero" VIEW-AS ALERT-BOX.
       DISABLE Pro_Ahorros.Val_Talonario.
       Pro_Ahorros.Id_CobroTal:SCREEN-VALUE = "NO".
       APPLY 'entry' TO Pro_Ahorros.Id_CobroTal.
       RETURN NO-APPLY.
    END.
END.
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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Bor_RegpAho wWin 
PROCEDURE Bor_RegpAho :
FOR EACH RegPAho: 
      DELETE RegPAho. 
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

  {&OPEN-QUERY-F_ProAhorros}
  GET FIRST F_ProAhorros.
  IF AVAILABLE Pro_Ahorros THEN 
    DISPLAY Pro_Ahorros.Tip_Ahorro Pro_Ahorros.Estado Pro_Ahorros.Fec_Matricula 
          Pro_Ahorros.Fec_Retiro Pro_Ahorros.Cod_ahorro Pro_Ahorros.Nom_Producto 
          Pro_Ahorros.Id_Persona 
      WITH FRAME F_ProAhorros IN WINDOW wWin.
  ENABLE RECT-211 RECT-237 RECT-276 Pro_Ahorros.Tip_Ahorro Pro_Ahorros.Estado 
         BUTTON-49 Btn_Impresion Pro_Ahorros.Nom_Producto 
         Pro_Ahorros.Id_Persona Btn_Consulta Btn_Generales Btn_Varios 
         Btn_Interes Btn_Liquidacion Btn_Restriccion Btn_Salvar BUTTON-6 
         BUTTON-4 Btn_Cancelar BtnDone BUTTON-11 
      WITH FRAME F_ProAhorros IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_ProAhorros}
  DISPLAY W_CobroLibreta W_CobroCheq NomCtaRev 
      WITH FRAME F_General IN WINDOW wWin.
  IF AVAILABLE Pro_Ahorros THEN 
    DISPLAY Pro_Ahorros.Id_Talonario Pro_Ahorros.Id_CobroTal 
          Pro_Ahorros.Val_Talonario Pro_Ahorros.Nro_CheqACobrar 
          Pro_Ahorros.Val_CadaCheque Pro_Ahorros.Id_Extracto 
          Pro_Ahorros.Id_Asociado Pro_Ahorros.Id_Debito 
          Pro_Ahorros.Id_Consecutivo Pro_Ahorros.Num_Consecutivo 
          Pro_Ahorros.Id_Linea Pro_Ahorros.Titulo_Valor 
          Pro_Ahorros.Id_Retparcial Pro_Ahorros.Id_Vencimiento 
          Pro_Ahorros.Id_NumAlterno Pro_Ahorros.Tip_Vencimiento 
          Pro_Ahorros.Id_Revaloriza Pro_Ahorros.Id_RenVencimiento 
          Pro_Ahorros.Cta_Revaloriz Pro_Ahorros.Prioridad 
      WITH FRAME F_General IN WINDOW wWin.
  ENABLE RECT-279 RECT-38 RECT-39 Pro_Ahorros.Id_Talonario 
         Pro_Ahorros.Nro_CheqACobrar Pro_Ahorros.Val_CadaCheque 
         Pro_Ahorros.Id_Extracto Pro_Ahorros.Id_Asociado Pro_Ahorros.Id_Debito 
         Pro_Ahorros.Id_Linea Pro_Ahorros.Id_NumAlterno 
         Pro_Ahorros.Id_Revaloriza 
      WITH FRAME F_General IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_General}
  DISPLAY W_Nin W_Val Cmb_Creditos 
      WITH FRAME F_Intereses IN WINDOW wWin.
  IF AVAILABLE Pro_Ahorros THEN 
    DISPLAY Pro_Ahorros.Id_Tasa Pro_Ahorros.Indicador Pro_Ahorros.Id_Sobregiro 
          Pro_Ahorros.Tip_Interes Pro_Ahorros.ProCre_Asociado 
          Pro_Ahorros.Id_CanCalIntereses Pro_Ahorros.Id_ForLiquidacion 
          Pro_Ahorros.For_Liquidacion 
      WITH FRAME F_Intereses IN WINDOW wWin.
  ENABLE RECT-212 RECT-213 RECT-214 RECT-215 RECT-277 Pro_Ahorros.Id_Tasa 
         Pro_Ahorros.Id_Sobregiro Pro_Ahorros.Tip_Interes Cmb_Creditos 
         Pro_Ahorros.Id_CanCalIntereses Pro_Ahorros.Id_ForLiquidacion 
         Pro_Ahorros.For_Liquidacion 
      WITH FRAME F_Intereses IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Intereses}
  DISPLAY W_CmbPeriodo 
      WITH FRAME F_Liquidaciones IN WINDOW wWin.
  IF AVAILABLE Pro_Ahorros THEN 
    DISPLAY Pro_Ahorros.Bas_Calculo Pro_Ahorros.Id_perliquidacion 
          Pro_Ahorros.Abo_Cuenta Pro_Ahorros.Per_Pago Pro_Ahorros.Tip_Anualidad 
          Pro_Ahorros.Id_SdoMinLiquidacion Pro_Ahorros.Mon_MinLiqidacion 
          Pro_Ahorros.Id_PerGracia Pro_Ahorros.Dia_Gracia 
      WITH FRAME F_Liquidaciones IN WINDOW wWin.
  ENABLE RECT-220 RECT-221 RECT-278 RECT-98 RECT-99 Pro_Ahorros.Bas_Calculo 
         Pro_Ahorros.Id_perliquidacion W_CmbPeriodo Pro_Ahorros.Abo_Cuenta 
         Pro_Ahorros.Per_Pago Pro_Ahorros.Tip_Anualidad 
         Pro_Ahorros.Id_SdoMinLiquidacion Pro_Ahorros.Mon_MinLiqidacion 
         Pro_Ahorros.Id_PerGracia Pro_Ahorros.Dia_Gracia 
      WITH FRAME F_Liquidaciones IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Liquidaciones}
  IF AVAILABLE Pro_Ahorros THEN 
    DISPLAY Pro_Ahorros.Id_Plazo Pro_Ahorros.Id_BloInactividad 
          Pro_Ahorros.Pla_Minimo Pro_Ahorros.Tie_Inactividad 
          Pro_Ahorros.Pla_Maximo Pro_Ahorros.Id_Salminimo 
          Pro_Ahorros.Id_Montominimo Pro_Ahorros.Val_MinRetiro 
          Pro_Ahorros.Tip_Salminimo Pro_Ahorros.Val_SdoMinimo 
          Pro_Ahorros.Val_Minconsignacion Pro_Ahorros.Id_Cuota 
          Pro_Ahorros.Val_MinRetcheque Pro_Ahorros.Val_Cuota 
          Pro_Ahorros.Id_Montomaximo Pro_Ahorros.Id_MonApertura 
          Pro_Ahorros.Val_MaxConsignacion Pro_Ahorros.Val_MonAper 
          Pro_Ahorros.Val_MaxRetEfectivo Pro_Ahorros.Id_AfeSdoMinimo 
      WITH FRAME F_Restricciones IN WINDOW wWin.
  ENABLE RECT-216 RECT-217 RECT-218 RECT-219 RECT-61 RECT-7 RECT-9 
         Pro_Ahorros.Id_BloInactividad Pro_Ahorros.Tie_Inactividad 
         Pro_Ahorros.Id_Salminimo Pro_Ahorros.Id_Montominimo 
         Pro_Ahorros.Val_MinRetiro Pro_Ahorros.Val_Minconsignacion 
         Pro_Ahorros.Id_Cuota Pro_Ahorros.Val_MinRetcheque 
         Pro_Ahorros.Val_Cuota Pro_Ahorros.Id_Montomaximo 
         Pro_Ahorros.Id_MonApertura Pro_Ahorros.Val_MaxConsignacion 
         Pro_Ahorros.Val_MonAper Pro_Ahorros.Val_MaxRetEfectivo 
         Pro_Ahorros.Id_AfeSdoMinimo 
      WITH FRAME F_Restricciones IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Restricciones}
  DISPLAY W_CodFormato W_ProDigito NomCxP NomCGto 
      WITH FRAME F_Varios IN WINDOW wWin.
  IF AVAILABLE Pro_Ahorros THEN 
    DISPLAY Pro_Ahorros.Id_Pignoracion Pro_Ahorros.Val_Pignoracion 
          Pro_Ahorros.Porce_Pignoracion Pro_Ahorros.Id_Embargo 
          Pro_Ahorros.Val_Embargo Pro_Ahorros.Porce_Embargo 
          Pro_Ahorros.Id_Seguro Pro_Ahorros.Num_SMSeguro Pro_Ahorros.Dia_Canje 
          Pro_Ahorros.Id_VerSdoTaq Pro_Ahorros.Id_Sorteos 
          Pro_Ahorros.Periodicidad Pro_Ahorros.Id_GMF 
          Pro_Ahorros.VrTope_ExentoEE Pro_Ahorros.Vr_Subsidio_GMF 
          Pro_Ahorros.VrTope_ExentoPension Pro_Ahorros.Cta_XPagarGMF 
          Pro_Ahorros.Cta_GtoGMF 
      WITH FRAME F_Varios IN WINDOW wWin.
  ENABLE Pro_Ahorros.Id_Seguro Pro_Ahorros.Num_SMSeguro BUTTON-118 BUTTON-119 
         Pro_Ahorros.Id_VerSdoTaq Pro_Ahorros.Id_Sorteos 
         Pro_Ahorros.Periodicidad Pro_Ahorros.Id_GMF 
         Pro_Ahorros.Vr_Subsidio_GMF Pro_Ahorros.VrTope_ExentoPension 
         Pro_Ahorros.Cta_XPagarGMF Pro_Ahorros.Cta_GtoGMF RECT-222 RECT-223 
         RECT-224 RECT-225 RECT-280 RECT-281 RECT-282 RECT-283 RECT-284 
         RECT-285 
      WITH FRAME F_Varios IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Varios}
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
DO WITH FRAME F_ProAhorros:
   ASSIGN Pro_Ahorros.cod_Ahorro    = INTEGER(Pro_Ahorros.cod_Ahorro:SCREEN-VALUE) 
          Pro_Ahorros.Nom_Producto  = Pro_Ahorros.Nom_producto:SCREEN-VALUE 
          Pro_Ahorros.Fec_Matricula = DATE(Pro_Ahorros.Fec_Matricula:SCREEN-VALUE)
          Pro_Ahorros.Fec_Retiro    = DATE(Pro_Ahorros.Fec_Retiro:SCREEN-VALUE)   
          Pro_Ahorros.Estado        = INTEGER(Pro_Ahorros.Estado:SCREEN-VALUE)       
          Pro_Ahorros.Id_Persona    = INTEGER(Pro_Ahorros.Id_Persona:SCREEN-VALUE) 
          Pro_Ahorros.tip_Ahorro    = INTEGER(Pro_Ahorros.tip_Ahorro:SCREEN-VALUE).
END.
DO WITH FRAME F_General:
   ASSIGN Pro_Ahorros.Id_Asociado       = INTEGER(Pro_Ahorros.Id_Asociado:SCREEN-VALUE)      
          Pro_Ahorros.Id_Consecutivo    = LOGICAL(Pro_Ahorros.Id_Consecutivo:SCREEN-VALUE)
          Pro_Ahorros.Titulo_Valor      = LOGICAL(Pro_Ahorros.Titulo_Valor:SCREEN-VALUE)     
          Pro_Ahorros.Num_Consecutivo   = DECIMAL(Pro_Ahorros.Num_Consecutivo:SCREEN-VALUE)
          Pro_Ahorros.Id_Talonario      = INTEGER(Pro_Ahorros.Id_Talonario:SCREEN-VALUE)
          Pro_Ahorros.Id_CobroTal       = LOGICAL(Pro_Ahorros.Id_CobroTal:SCREEN-VALUE)
          Pro_Ahorros.Val_Talonario     = DECIMAL(Pro_Ahorros.Val_Talonario:SCREEN-VALUE)
          Pro_Ahorros.Id_Extracto       = LOGICAL(Pro_Ahorros.Id_Extracto:SCREEN-VALUE)
          Pro_Ahorros.Id_Debito         = LOGICAL(Pro_Ahorros.Id_Debito:SCREEN-VALUE)
          Pro_Ahorros.Id_Linea          = LOGICAL(Pro_Ahorros.Id_Linea:SCREEN-VALUE)
          Pro_Ahorros.Id_RetParcial     = LOGICAL(Pro_Ahorros.Id_RetParcial:SCREEN-VALUE)
          Pro_Ahorros.Id_NumAlterno     = LOGICAL(Pro_Ahorros.Id_NumAlterno:SCREEN-VALUE)
          Pro_Ahorros.Id_Vencimiento    = LOGICAL(Pro_Ahorros.Id_Vencimiento:SCREEN-VALUE)
          Pro_Ahorros.Tip_Vencimiento   = INTEGER(Pro_Ahorros.Tip_Vencimiento:SCREEN-VALUE)
          Pro_Ahorros.Id_RenVencimiento = LOGICAL(Pro_Ahorros.Id_RenVencimiento:SCREEN-VALUE)
          Pro_Ahorros.Prioridad         = INTEGER(Pro_Ahorros.Prioridad:SCREEN-VALUE)
          Pro_Ahorros.Id_Revaloriz      = LOGICAL(Pro_Ahorros.Id_Revaloriz:SCREEN-VALUE)
          Pro_Ahorros.Cta_Revaloriz     = STRING(Pro_Ahorros.Cta_Revaloriz:SCREEN-VALUE)
          Pro_Ahorros.Nro_CheqACobrar   = INTEG(Pro_Ahorros.Nro_CheqACobrar:SCREEN-VALUE) 
          Pro_Ahorros.Val_CadaCheque    = DEC(Pro_Ahorros.Val_CadaCheque:SCREEN-VALUE)
          Pro_Ahorros.Cod_CobroCheq     = INTEGER(SUBSTRING(W_CobroCheq:SCREEN-VALUE,1,9)).
   IF Pro_Ahorros.Id_CobroTal:SCREEN-VALUE EQ "yes" THEN
      Pro_Ahorros.Cod_CobroLibreta = INTEGER(SUBSTRING(W_CobroLibreta:SCREEN-VALUE,1,9)).
END.
DO WITH FRAME F_Varios:
   ASSIGN Pro_Ahorros.Id_Pignoracion       = LOGICAL(Pro_Ahorros.Id_Pignoracion:SCREEN-VALUE)
          Pro_Ahorros.Val_Pignoracion      = DECIMAL(Pro_Ahorros.Val_Pignoracion:SCREEN-VALUE)
          Pro_Ahorros.Porce_Pignoracion    = DECIMAL(Pro_Ahorros.Porce_Pignoracion:SCREEN-VALUE)
          Pro_Ahorros.Id_Embargo           = LOGICAL(Pro_Ahorros.Id_Embargo:SCREEN-VALUE)
          Pro_Ahorros.Val_Embargo          = DECIMAL(Pro_Ahorros.Val_Embargo:SCREEN-VALUE)
          Pro_Ahorros.Porce_Embargo        = DECIMAL(Pro_Ahorros.Porce_Embargo:SCREEN-VALUE)
          Pro_Ahorros.Id_Seguro            = LOGICAL(Pro_Ahorros.Id_Seguro:SCREEN-VALUE)
          Pro_Ahorros.Num_SMSeguro         = DECIMAL(Pro_Ahorros.Num_SMSeguro:SCREEN-VALUE)
          Pro_Ahorros.Dia_Canje            = DECIMAL(Pro_Ahorros.Dia_Canje:SCREEN-VALUE)
          Pro_Ahorros.Id_Sorteos           = LOGICAL(Pro_Ahorros.Id_Sorteos:SCREEN-VALUE)
          Pro_Ahorros.Periodicidad         = INTEGER(Pro_Ahorros.Periodicidad:SCREEN-VALUE)
          Pro_Ahorros.Id_VerSdoTaq         = LOGICAL(Pro_Ahorros.Id_VerSdoTaq:SCREEN-VALUE)
          Pro_Ahorros.Cod_Formato          = INTEGER(SUBSTRING(W_CodFormato:SCREEN-VALUE,1,2))
          Pro_Ahorros.Pro_Digito           = INTEGER(SUBSTRING(W_ProDigito:SCREEN-VALUE,1,2))
          Pro_Ahorros.Id_GMF               = INTEG(Pro_Ahorros.Id_GMF:SCREEN-VALUE)
          Pro_Ahorros.VrTope_ExentoEE      = DEC(Pro_Ahorros.VrTope_ExentoEE:SCREEN-VALUE)
          Pro_Ahorros.VrTope_ExentoPension = DEC(Pro_Ahorros.VrTope_ExentoPension:SCREEN-VALUE)
          Pro_Ahorros.Vr_Subsidio_GMF      = DEC(Pro_Ahorros.Vr_Subsidio_GMF:SCREEN-VALUE)
          Pro_Ahorros.Cta_XPagarGMF        = Pro_Ahorros.Cta_XPagarGMF:SCREEN-VALUE     
          Pro_Ahorros.Cta_GtoGMF           = Pro_Ahorros.Cta_GtoGMF:SCREEN-VALUE.       
END.
DO WITH FRAME F_Intereses:
   ASSIGN Pro_Ahorros.Id_Tasa              = INTEGER(Pro_Ahorros.Id_Tasa:SCREEN-VALUE)
          Pro_Ahorros.Indicador            = INTEGER(Pro_Ahorros.Indicador:SCREEN-VALUE)
          Pro_Ahorros.Tip_Interes          = INTEGER(Pro_Ahorros.Tip_Interes:SCREEN-VALUE)
          Pro_Ahorros.Id_ForLiquidacion    = INTEGER(Pro_Ahorros.Id_ForLiquidacion:SCREEN-VALUE)
          Pro_Ahorros.FOR_Liquidacion      = INTEGER(Pro_Ahorros.FOR_Liquidacion:SCREEN-VALUE)
          Pro_Ahorros.Id_Sobregiro         = LOGICAL(Pro_Ahorros.Id_Sobregiro:SCREEN-VALUE)
          Pro_Ahorros.Id_CanCalIntereses   = LOGICAL(Pro_Ahorros.Id_CanCalIntereses:SCREEN-VALUE)
          Pro_Ahorros.ProCre_Asociado      = INTEGER(Pro_Ahorros.ProCre_Asociado:SCREEN-VALUE).
END.
DO WITH FRAME F_Liquidaciones:
   ASSIGN Pro_Ahorros.Id_PerLiquidacion    = INTEGER(Pro_Ahorros.Id_PerLiquidacion:SCREEN-VALUE)
          Pro_Ahorros.Per_Liquidacion      = INTEGER(SUBSTRING(W_CmbPeriodo:SCREEN-VALUE,1,2))
          Pro_Ahorros.Abo_Cuenta           = DECIMAL(Pro_Ahorros.Abo_Cuenta:SCREEN-VALUE)
          Pro_Ahorros.Per_Pago             = DECIMAL(Pro_Ahorros.Per_Pago:SCREEN-VALUE)
          Pro_Ahorros.Id_SdoMinLiquidacion = LOGICAL(Pro_Ahorros.Id_SdoMinLiquidacion:SCREEN-VALUE)
          Pro_Ahorros.Mon_MinLiqidacion    = DECIMAL(Pro_Ahorros.Mon_MinLiqidacion:SCREEN-VALUE)
          Pro_Ahorros.Id_PerGracia         = LOGICAL(Pro_Ahorros.Id_PerGracia:SCREEN-VALUE)
          Pro_Ahorros.Dia_Gracia           = DECIMAL(Pro_Ahorros.Dia_Gracia:SCREEN-VALUE)
          Pro_Ahorros.Bas_Calculo          = INTEGER(Pro_Ahorros.Bas_Calculo:SCREEN-VALUE)
          Pro_Ahorros.Tip_Anualidad        = LOGICAL(Pro_Ahorros.Tip_Anualidad:SCREEN-VALUE).
END.
DO WITH FRAME F_Restricciones:
   ASSIGN Pro_Ahorros.Id_Plazo             = LOGICAL(Pro_Ahorros.Id_Plazo:SCREEN-VALUE)
          Pro_Ahorros.Pla_Minimo           = DECIMAL(Pro_Ahorros.Pla_Minimo:SCREEN-VALUE)
          Pro_Ahorros.Pla_Maximo           = DECIMAL(Pro_Ahorros.Pla_Maximo:SCREEN-VALUE)
          Pro_Ahorros.Id_SalMinimo         = LOGICAL(Pro_Ahorros.Id_SalMinimo:SCREEN-VALUE)
          Pro_Ahorros.Tip_SalMinimo        = LOGICAL(Pro_Ahorros.Tip_SalMinimo:SCREEN-VALUE)
          Pro_Ahorros.Val_SdoMinimo        = DECIMAL(Pro_Ahorros.Val_SdoMinimo:SCREEN-VALUE)
          Pro_Ahorros.Id_Cuota             = LOGICAL(Pro_Ahorros.Id_Cuota:SCREEN-VALUE)
          Pro_Ahorros.Val_Cuota            = DECIMAL(Pro_Ahorros.Val_Cuota:SCREEN-VALUE)
          Pro_Ahorros.Id_MonApertura       = LOGICAL(Pro_Ahorros.Id_MonApertura:SCREEN-VALUE)
          Pro_Ahorros.Val_MonAper          = DECIMAL(Pro_Ahorros.Val_MonAper:SCREEN-VALUE)
          Pro_Ahorros.Id_BloInactividad    = LOGICAL(Pro_Ahorros.Id_BloInactividad:SCREEN-VALUE)
          Pro_Ahorros.Tie_Inactividad      = DECIMAL(Pro_Ahorros.Tie_Inactividad:SCREEN-VALUE)
          Pro_Ahorros.Id_MontoMinimo       = LOGICAL(Pro_Ahorros.Id_MontoMinimo:SCREEN-VALUE)
          Pro_Ahorros.Val_MinRetiro        = DECIMAL(Pro_Ahorros.Val_MinRetiro:SCREEN-VALUE)
          Pro_Ahorros.Val_MinConsignacion  = DECIMAL(Pro_Ahorros.Val_MinConsignacion:SCREEN-VALUE)
          Pro_Ahorros.Val_MinRetCheque     = DECIMAL(Pro_Ahorros.Val_MinRetCheque:SCREEN-VALUE)
          Pro_Ahorros.Id_MontoMaximo       = LOGICAL(Pro_Ahorros.Id_MontoMaximo:SCREEN-VALUE)
          Pro_Ahorros.Val_MaxConsignacion  = DECIMAL(Pro_Ahorros.Val_MaxConsignacion:SCREEN-VALUE)
          Pro_Ahorros.Val_MaxRetEfectivo   = DECIMAL(Pro_Ahorros.Val_MaxRetEfectivo:SCREEN-VALUE)
          Pro_Ahorros.Id_AfeSdoMinimo      = LOGICAL(Pro_Ahorros.Id_AfeSdoMinimo:SCREEN-VALUE).    
END.
RUN Habilitar_Deshabilitar.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Habilitar_Deshabilitar wWin 
PROCEDURE Habilitar_Deshabilitar :
DO WITH FRAME F_General:
   DISABLE pro_ahorros.cod_ahorro WITH FRAME F_ProAhorros.
   IF Pro_Ahorros.Id_Consecutivo:SCREEN-VALUE EQ "YES" THEN ENABLE Pro_Ahorros.Num_Consecutivo.
   ELSE DISABLE Pro_Ahorros.Num_Consecutivo.

   IF Pro_Ahorros.Id_Vencimiento:SCREEN-VALUE EQ "YES" THEN ENABLE Pro_Ahorros.Tip_Vencimiento Pro_Ahorros.Id_RenVencimiento.
   ELSE DISABLE Pro_Ahorros.Tip_Vencimiento Pro_Ahorros.Id_RenVencimiento.

   IF Pro_Ahorros.Id_Talonario:SCREEN-VALUE EQ "3" THEN DISABLE Pro_Ahorros.Id_CobroTal Pro_Ahorros.Val_Talonario.
   ELSE ENABLE Pro_Ahorros.Id_CobroTal Pro_Ahorros.Val_Talonario.
END.

DO WITH FRAME F_Varios:
   IF Pro_Ahorros.Id_Pignoracion:SCREEN-VALUE EQ "YES" THEN ENABLE Pro_Ahorros.Val_Pignoracion Pro_Ahorros.Porce_Pignoracion.
   ELSE DISABLE Pro_Ahorros.Val_Pignoracion Pro_Ahorros.Porce_Pignoracion.

   IF Pro_Ahorros.Id_Embargo:SCREEN-VALUE EQ "YES" THEN ENABLE Pro_Ahorros.Val_Embargo Pro_Ahorros.Porce_Embargo.
   ELSE DISABLE Pro_Ahorros.Val_Embargo Pro_Ahorros.Porce_Embargo.

   IF Pro_Ahorros.Id_Seguro:SCREEN-VALUE EQ "YES" THEN ENABLE Pro_Ahorros.Num_SMSeguro.
   ELSE DISABLE Pro_Ahorros.Num_SMSeguro.

   IF Pro_Ahorros.Id_Sorteos:SCREEN-VALUE EQ "YES" THEN ENABLE Pro_Ahorros.Periodicidad.
   ELSE DISABLE Pro_Ahorros.Periodicidad.

   DISABLE W_CodFormato W_ProDigito.
END.

DO WITH FRAME F_Intereses:
   IF Pro_Ahorros.Id_Tasa:SCREEN-VALUE EQ "1" THEN ENABLE Pro_Ahorros.Indicador.
   ELSE DISABLE Pro_Ahorros.Indicador.
   IF Pro_Ahorros.Id_Sobregiro:SCREEN-VALUE EQ "yes" THEN ENABLE Pro_Ahorros.Id_CanCalIntereses Cmb_Creditos.
   ELSE DISABLE Pro_Ahorros.Id_CanCalIntereses Cmb_Creditos.

   DISABLE W_Nin.
END.
    
DO WITH FRAME F_Liquidaciones:
   ENABLE ALL WITH FRAME F_Liquidaciones.
   IF Pro_Ahorros.Id_PerLiquidacion:SCREEN-VALUE EQ "1" THEN ENABLE W_CmbPeriodo Pro_Ahorros.Abo_Cuenta Pro_Ahorros.Per_Pago.
   ELSE DISABLE W_CmbPeriodo Pro_Ahorros.Abo_Cuenta Pro_Ahorros.Per_Pago.

   IF Pro_Ahorros.Id_SdoMinLiquidacion:SCREEN-VALUE EQ "YES" THEN ENABLE Pro_Ahorros.Mon_MinLiqidacion.
   ELSE ENABLE Pro_Ahorros.Mon_MinLiqidacion.

   IF Pro_Ahorros.Id_PerGracia:SCREEN-VALUE EQ "yes" THEN ENABLE Pro_Ahorros.Dia_Gracia.
   ELSE DISABLE Pro_Ahorros.Dia_Gracia.
END.
    
DO WITH FRAME F_Restricciones:
   ENABLE ALL WITH FRAME F_Restricciones.
   IF Pro_Ahorros.Id_Plazo:SCREEN-VALUE EQ "YES" THEN ENABLE Pro_Ahorros.Pla_Minimo Pro_Ahorros.Pla_Maximo.
   ELSE DISABLE Pro_Ahorros.Pla_Minimo Pro_Ahorros.Pla_Maximo.

   IF Pro_Ahorros.Id_SalMinimo:SCREEN-VALUE EQ "YES" THEN ENABLE Pro_Ahorros.Tip_SalMinimo Pro_Ahorros.Val_SdoMinimo.
   ELSE ENABLE Pro_Ahorros.Tip_SalMinimo Pro_Ahorros.Val_SdoMinimo.

   IF Pro_Ahorros.Id_Cuota:SCREEN-VALUE EQ "YES" THEN ENABLE Pro_Ahorros.Val_Cuota.
   ELSE DISABLE Pro_Ahorros.Val_Cuota.

   IF Pro_Ahorros.Id_MonApertura:SCREEN-VALUE EQ "YES" THEN ENABLE Pro_Ahorros.Val_MonAper.
   ELSE DISABLE Pro_Ahorros.Val_MonAper.

   IF Pro_Ahorros.Id_BloInactividad:SCREEN-VALUE EQ "YES" THEN ENABLE Pro_Ahorros.Tie_Inactividad.
   ELSE DISABLE Pro_Ahorros.Tie_Inactividad. 

   IF Pro_Ahorros.Id_MontoMinimo:SCREEN-VALUE EQ "YES" THEN ENABLE Pro_Ahorros.Val_MinRetiro Pro_Ahorros.Val_MinConsignacion Pro_Ahorros.Val_MinRetCheque.
   ELSE DISABLE Pro_Ahorros.Val_MinRetiro Pro_Ahorros.Val_MinConsignacion Pro_Ahorros.Val_MinRetCheque.

   IF Pro_Ahorros.Id_MontoMaximo:SCREEN-VALUE EQ "YES" THEN ENABLE Pro_Ahorros.Val_MaxConsignacion Pro_Ahorros.Val_MaxRetEfectivo.
   ELSE DISABLE Pro_Ahorros.Val_MaxConsignacion Pro_Ahorros.Val_MaxRetEfectivo.
   
   IF Pro_Ahorros.Id_RetParcial:SCREEN-VALUE IN FRAME F_General EQ "no" THEN 
      DISABLE Pro_Ahorros.Val_MinRetiro Pro_Ahorros.Val_MaxRetEfectivo Pro_Ahorros.Val_MinRetCheque WITH FRAME F_Restricciones.
   ELSE
      ENABLE Pro_Ahorros.Val_MinRetiro Pro_Ahorros.Val_MaxRetEfectivo Pro_Ahorros.Val_MinRetCheque WITH FRAME F_Restricciones.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir_Excel wWin 
PROCEDURE Imprimir_Excel :
{Incluido\Def_Excel.i}
 /* MANDA ENCABEZADO DE HOJA DE CALCULO*/
 DEFINE VAR W_EstadoInf AS CHARACTER FORMAT "X(8)". 
 E_NumFila = 1.
 E_NumColumn = 6.
 E_Fila      = "003" + "Age"
             + "003" + "Pro"
             + "040" + "Nom_Producto                            "
             + "015" + "Tipo           "
             + "021" + "SdoMinimo            "
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
    FOR EACH Pro_Ahorros NO-LOCK  BY pro_ahorros.cod_ahorro:
        CASE pro_ahorros.tip_ahorro:
            WHEN 1 THEN
                    Tipo = "A la Vista".
            WHEN 2 THEN
                    Tipo = "Contractual".
            WHEN 3 THEN
                    Tipo = "A Termino".
        END CASE. 
        CASE Pro_Ahorros.Estado:
            WHEN 1 THEN
                    Est = "Activo".
            WHEN 2 THEN
                    Est = "Inactivo".
        END CASE.
      E_Fila2     = "".
      E_Fila2     = "003" + STRING(Pro_Ahorros.Cod_ahorro,"999")
                  + "005" + STRING(pro_ahorros.cod_ahorro,"999") 
                  + "005" + STRING(Pro_Ahorros.Nom_Producto,"X(40)")
                  + "015" + STRING(Tipo,"X(15)")
                  + "021" + STRING(Pro_Ahorros.Val_SdoMinimo,"->>>>>,>>>,>>>,>>9.99")
                  + "008" + STRING(Est,"X(8)"). 

      {Incluido\imprimir_Excel.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar_Campos wWin 
PROCEDURE Inicializar_Campos :
DO WITH FRAME F_ProAhorros:
    ASSIGN Pro_ahorros.Nom_producto:SCREEN-VALUE      = ""
          Pro_Ahorros.Fec_Matricula:SCREEN-VALUE     = STRING(TODAY)
          Pro_Ahorros.Fec_Retiro:SCREEN-VALUE        = ""
          Pro_Ahorros.Estado:SCREEN-VALUE            = "1"
          pro_ahorros.tip_ahorro:SCREEN-VALUE      = "1".
   FIND LAST Pro_Ahorros NO-LOCK NO-ERROR.
   IF AVAILABLE Pro_Ahorros THEN pro_ahorros.cod_ahorro:SCREEN-VALUE = STRING(pro_ahorros.cod_ahorro + 1).
END.
DO WITH FRAME F_General:
   ASSIGN Pro_Ahorros.Id_Asociado:SCREEN-VALUE       = "1"
          Pro_Ahorros.Id_Consecutivo:SCREEN-VALUE    = "NO"
          Pro_Ahorros.Titulo_Valor:SCREEN-VALUE      = "NO"
          Pro_Ahorros.Num_Consecutivo:SCREEN-VALUE   = ""
          Pro_Ahorros.Id_Talonario:SCREEN-VALUE      = "3"
          Pro_Ahorros.Id_CobroTal:SCREEN-VALUE       = "NO"
          Pro_Ahorros.Val_Talonario:SCREEN-VALUE     = ""
          Pro_Ahorros.Id_Extracto:SCREEN-VALUE       = "NO"
          Pro_Ahorros.Id_Debito:SCREEN-VALUE         = "NO"
          Pro_Ahorros.Id_Linea:SCREEN-VALUE          = "NO"
          Pro_Ahorros.Id_RetParcial:SCREEN-VALUE     = "NO"
          Pro_Ahorros.Id_NumAlterno:SCREEN-VALUE     = "NO"
          Pro_Ahorros.Id_Vencimiento:SCREEN-VALUE    = "NO"
          Pro_Ahorros.Tip_Vencimiento:SCREEN-VALUE   = "1"
          Pro_Ahorros.Id_RenVencimiento:SCREEN-VALUE = "NO"
          Pro_Ahorros.Prioridad:SCREEN-VALUE         = ""
          Pro_Ahorros.Id_Revaloriz:SCREEN-VALUE      = "NO"
          Pro_Ahorros.Cta_Revaloriz:SCREEN-VALUE     = ""
          Pro_Ahorros.Nro_CheqACobrar:SCREEN-VALUE   = "0"
          Pro_Ahorros.Val_CadaCheque:SCREEN-VALUE    = "0.00"
          W_CobroLibreta:SCREEN-VALUE                = " "
          W_CobroCheq:SCREEN-VALUE                   = " ".
           
   DISABLE Pro_Ahorros.Cta_Revaloriz.
END.

DO WITH FRAME F_Varios:
   ASSIGN Pro_Ahorros.Id_Pignoracion:SCREEN-VALUE    = "NO"
          Pro_Ahorros.Val_Pignoracion:SCREEN-VALUE   = ""
          Pro_Ahorros.Porce_Pignoracion:SCREEN-VALUE = ""
          Pro_Ahorros.Id_Embargo:SCREEN-VALUE        = "NO"
          Pro_Ahorros.Val_Embargo:SCREEN-VALUE       = ""
          Pro_Ahorros.Porce_Embargo:SCREEN-VALUE     = ""
          Pro_Ahorros.Id_Seguro:SCREEN-VALUE         = "NO"
          Pro_Ahorros.Num_SMSeguro:SCREEN-VALUE      = ""
          Pro_Ahorros.Dia_Canje:SCREEN-VALUE         = ""
          Pro_Ahorros.Id_Sorteos:SCREEN-VALUE        = "NO"
          Pro_Ahorros.Periodicidad:SCREEN-VALUE      = "1"
          Pro_Ahorros.Id_VerSdoTaq:SCREEN-VALUE      = "NO"
          Pro_Ahorros.Id_GMF:SCREEN-VALUE            = "0"
          Pro_Ahorros.VrTope_ExentoEE:SCREEN-VALUE   = "0.00"
          Pro_Ahorros.Cta_XPagarGMF:SCREEN-VALUE     = ""
          Pro_Ahorros.Cta_GtoGMF:SCREEN-VALUE        = ""
          Pro_Ahorros.VrTope_ExentoEE:SENSITIVE      = FALSE
          NomCGto:SCREEN-VALUE                       = ""
          NomCxP:SCREEN-VALUE                        = "".
          
   W_CodFormato:SCREEN-VALUE = "".
   W_ProDigito:SCREEN-VALUE = "".
END.

DO WITH FRAME F_Intereses:
   ASSIGN Pro_Ahorros.Id_Tasa:SCREEN-VALUE            = "1"
          Pro_Ahorros.Indicador:SCREEN-VALUE          = ""
          Pro_Ahorros.Tip_Interes:SCREEN-VALUE        = "1"
          Pro_Ahorros.Id_ForLiquidacion:SCREEN-VALUE  = "1"
          Pro_Ahorros.FOR_Liquidacion:SCREEN-VALUE    = "2"
          Pro_Ahorros.Id_Sobregiro:SCREEN-VALUE       = "NO"
          Pro_Ahorros.Id_CanCalIntereses:SCREEN-VALUE = "NO"
          W_Nin:SCREEN-VALUE                          = ""
          W_Val:SCREEN-VALUE                          = "".
END.
    
DO WITH FRAME F_Liquidaciones:
   ASSIGN Pro_Ahorros.Id_PerLiquidacion:SCREEN-VALUE       = "1"
          W_CmbPeriodo:SCREEN-VALUE                        = W_CmbPeriodo:ENTRY(1)
          Pro_Ahorros.Abo_Cuenta:SCREEN-VALUE              = ""
          Pro_Ahorros.Per_Pago:SCREEN-VALUE                = ""
          Pro_Ahorros.Id_SdoMinLiquidacion:SCREEN-VALUE    = "NO"
          Pro_Ahorros.Mon_MinLiqidacion:SCREEN-VALUE       = ""
          Pro_Ahorros.Id_PerGracia:SCREEN-VALUE            = "NO"
          Pro_Ahorros.Dia_Gracia:SCREEN-VALUE              = ""
          Pro_Ahorros.Bas_Calculo:SCREEN-VALUE             = "1"
          Pro_Ahorros.Tip_Anualidad:SCREEN-VALUE           = "yes".
END.
    
DO WITH FRAME F_Restricciones:
   ASSIGN Pro_Ahorros.Id_Plazo:SCREEN-VALUE                = "NO"
          Pro_Ahorros.Pla_Minimo:SCREEN-VALUE              = ""
          Pro_Ahorros.Pla_Maximo:SCREEN-VALUE              = ""
          Pro_Ahorros.Id_SalMinimo:SCREEN-VALUE            = "NO"
          Pro_Ahorros.Tip_SalMinimo:SCREEN-VALUE           = "yes"
          Pro_Ahorros.Val_SdoMinimo:SCREEN-VALUE           = ""
          Pro_Ahorros.Id_Cuota:SCREEN-VALUE                = "NO"
          Pro_Ahorros.Val_Cuota:SCREEN-VALUE               = ""
          Pro_Ahorros.Id_MonApertura:SCREEN-VALUE          = "NO"
          Pro_Ahorros.Val_MonAper:SCREEN-VALUE             = ""
          Pro_Ahorros.Id_BloInactividad:SCREEN-VALUE       = "NO"
          Pro_Ahorros.Tie_Inactividad:SCREEN-VALUE         = "0"
          Pro_Ahorros.Id_MontoMinimo:SCREEN-VALUE          = "NO"
          Pro_Ahorros.Val_MinRetiro:SCREEN-VALUE           = ""
          Pro_Ahorros.Val_MinConsignacion:SCREEN-VALUE     = ""
          Pro_Ahorros.Val_MinRetCheque:SCREEN-VALUE        = ""
          Pro_Ahorros.Id_MontoMaximo:SCREEN-VALUE          = "NO"
          Pro_Ahorros.Val_MaxConsignacion:SCREEN-VALUE     = ""
          Pro_Ahorros.Val_MaxRetEfectivo:SCREEN-VALUE      = ""
          Pro_Ahorros.Id_AfeSdoMinimo:SCREEN-VALUE         = "NO".
END.
RUN Habilitar_Deshabilitar.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
RUN SUPER.
  FIND FIRST Pro_Ahorros NO-LOCK NO-ERROR.
  IF AVAILABLE(Pro_Ahorros) THEN RUN Mostrar_Producto.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Producto wWin 
PROCEDURE Mostrar_Producto :
DO WITH FRAME F_ProAhorros:
   ASSIGN Pro_Ahorros.cod_Ahorro:SCREEN-VALUE    = STRING(Pro_Ahorros.cod_Ahorro)
          Pro_Ahorros.Nom_producto:SCREEN-VALUE  = Pro_Ahorros.Nom_Producto
          Pro_Ahorros.Fec_Matricula:SCREEN-VALUE = STRING(Pro_Ahorros.Fec_Matricula)
          Pro_Ahorros.Fec_Retiro:SCREEN-VALUE    = STRING(Pro_Ahorros.Fec_Retiro)
          Pro_Ahorros.Estado:SCREEN-VALUE        = STRING(Pro_Ahorros.Estado)
          Pro_Ahorros.Id_Persona:SCREEN-VALUE    = STRING(Pro_Ahorros.Id_Persona)
          Pro_Ahorros.tip_Ahorro:SCREEN-VALUE    = STRING(Pro_Ahorros.tip_Ahorro).
END.
DO WITH FRAME F_General:
   ENABLE ALL WITH FRAME F_General.
   ASSIGN Pro_Ahorros.Id_Asociado:SCREEN-VALUE       = STRING(Pro_Ahorros.Id_Asociado)
          Pro_Ahorros.Id_Consecutivo:SCREEN-VALUE    = STRING(Pro_Ahorros.Id_Consecutivo)
          Pro_Ahorros.Titulo_Valor:SCREEN-VALUE      = STRING(Pro_Ahorros.Titulo_Valor)
          Pro_Ahorros.Num_Consecutivo:SCREEN-VALUE   = STRING(Pro_Ahorros.Num_Consecutivo)
          Pro_Ahorros.Id_Talonario:SCREEN-VALUE      = STRING(Pro_Ahorros.Id_Talonario)
          Pro_Ahorros.Id_CobroTal:SCREEN-VALUE       = STRING(Pro_Ahorros.Id_CobroTal)
          Pro_Ahorros.Val_Talonario:SCREEN-VALUE     = STRING(Pro_Ahorros.Val_Talonario)
          Pro_Ahorros.Id_Extracto:SCREEN-VALUE       = STRING(Pro_Ahorros.Id_Extracto)
          Pro_Ahorros.Id_Debito:SCREEN-VALUE         = STRING(Pro_Ahorros.Id_Debito)
          Pro_Ahorros.Id_Linea:SCREEN-VALUE          = STRING(Pro_Ahorros.Id_Linea)
          Pro_Ahorros.Id_RetParcial:SCREEN-VALUE     = STRING(Pro_Ahorros.Id_RetParcial)
          Pro_Ahorros.Id_NumAlterno:SCREEN-VALUE     = STRING(Pro_Ahorros.Id_NumAlterno)
          Pro_Ahorros.Id_Vencimiento:SCREEN-VALUE    = STRING(Pro_Ahorros.Id_Vencimiento)
          Pro_Ahorros.Tip_Vencimiento:SCREEN-VALUE   = STRING(Pro_Ahorros.Tip_Vencimiento)
          Pro_Ahorros.Id_RenVencimiento:SCREEN-VALUE = STRING(Pro_Ahorros.Id_RenVencimiento)
          Pro_Ahorros.Prioridad:SCREEN-VALUE         = STRING(Pro_Ahorros.Prioridad)
          Pro_Ahorros.Id_Revaloriz:SCREEN-VALUE      = STRING(Pro_Ahorros.Id_Revaloriz)
          Pro_Ahorros.Cta_Revaloriz:SCREEN-VALUE     = STRING(Pro_Ahorros.Cta_Revaloriz)
          Pro_Ahorros.Nro_CheqACobrar:SCREEN-VALUE   = STRING(Pro_Ahorros.Nro_CheqACobrar) 
          Pro_Ahorros.Val_CadaCheque:SCREEN-VALUE    = STRING(Pro_Ahorros.Val_CadaCheque)
          W_CobroCheq:SCREEN-VALUE                   = STRING(Pro_Ahorros.Cod_CobroCheq).
   IF Pro_Ahorros.Id_Revaloriz EQ NO THEN DO:
      DISABLE Pro_Ahorros.Cta_Revaloriz WITH FRAME F_General.
      ASSIGN NomCtaRev:SCREEN-VALUE = "".
   END.
   ELSE DO:
      ENABLE Pro_Ahorros.Cta_Revaloriz WITH FRAME F_General.
      FIND Cuentas WHERE Cuentas.Cuenta EQ Pro_Ahorros.Cta_Revaloriz NO-LOCK NO-ERROR.
      IF AVAIL Cuentas THEN NomCtaRev:SCREEN-VALUE = Cuentas.Nombre.
   END.
   IF Pro_Ahorros.Id_Talonario NE 3 AND Pro_Ahorros.Id_CobroTal THEN DO:
      ENABLE Btn_CobroTal.
      IF Pro_Ahorros.Cod_CobroLibreta EQ 0 THEN DO:
         MESSAGE "Falta por configurar la Operacion para" SKIP
                 "el cobro del talonario o libreta." VIEW-AS ALERT-BOX INFORMATION.
      END.
      ELSE DO:
         FIND Operacion WHERE Operacion.Tipo_Producto EQ 4 AND Operacion.Cod_Operacion EQ Pro_Ahorros.Cod_CobroLibreta NO-LOCK NO-ERROR.
         IF AVAIL Operacion
            THEN W_CobroLibreta:SCREEN-VALUE = STRING(Operacion.Cod_Operacion,"999999999") + " - " + Operacion.Nom_Operacion.
            ELSE W_CobroLibreta:SCREEN-VALUE = "La operación configurada: " + STRING(Pro_Ahorros.Cod_CobroLibreta,"999999999")
                                                                            + " en la tabla de Operaciones".
      END.
   END.
   ELSE DO:
     DISABLE Btn_CobroTal.
     W_CobroLibreta:SCREEN-VALUE = "".
   END.
END.
DO WITH FRAME F_Varios:
   ENABLE ALL WITH FRAME F_Varios.
   ASSIGN Pro_Ahorros.Id_Pignoracion:SCREEN-VALUE       = STRING(Pro_Ahorros.Id_Pignoracion)
          Pro_Ahorros.Val_Pignoracion:SCREEN-VALUE      = STRING(Pro_Ahorros.Val_Pignoracion)
          Pro_Ahorros.Porce_Pignoracion:SCREEN-VALUE    = STRING(Pro_Ahorros.Porce_Pignoracion)
          Pro_Ahorros.Id_Embargo:SCREEN-VALUE           = STRING(Pro_Ahorros.Id_Embargo)
          Pro_Ahorros.Val_Embargo:SCREEN-VALUE          = STRING(Pro_Ahorros.Val_Embargo)
          Pro_Ahorros.Porce_Embargo:SCREEN-VALUE        = STRING(Pro_Ahorros.Porce_Embargo)
          Pro_Ahorros.Id_Seguro:SCREEN-VALUE            = STRING(Pro_Ahorros.Id_Seguro)
          Pro_Ahorros.Num_SMSeguro:SCREEN-VALUE         = STRING(Pro_Ahorros.Num_SMSeguro)
          Pro_Ahorros.Dia_Canje:SCREEN-VALUE            = STRING(Pro_Ahorros.Dia_Canje)
          Pro_Ahorros.Id_Sorteos:SCREEN-VALUE           = STRING(Pro_Ahorros.Id_Sorteos)
          Pro_Ahorros.Periodicidad:SCREEN-VALUE         = STRING(Pro_Ahorros.Periodicidad)
          Pro_Ahorros.Id_VerSdoTaq:SCREEN-VALUE         = STRING(Pro_Ahorros.Id_VerSdoTaq)
          Pro_Ahorros.Id_GMF:SCREEN-VALUE               = STRING(Pro_Ahorros.Id_GMF)
          Pro_Ahorros.VrTope_ExentoEE:SCREEN-VALUE      = STRING(Pro_Ahorros.VrTope_ExentoEE)
          Pro_Ahorros.VrTope_ExentoPension:SCREEN-VALUE = STRING(Pro_Ahorros.VrTope_ExentoPension)
          Pro_Ahorros.Vr_Subsidio_GMF:SCREEN-VALUE      = STRING(Pro_Ahorros.Vr_Subsidio_GMF)
          Pro_Ahorros.Cta_XPagarGMF:SCREEN-VALUE        = Pro_Ahorros.Cta_XPagarGMF      
          Pro_Ahorros.Cta_GtoGMF:SCREEN-VALUE           = Pro_Ahorros.Cta_GtoGMF
          Pro_Ahorros.VrTope_ExentoEE:SENSITIVE         = FALSE
          Pro_Ahorros.VrTope_ExentoPension:SENSITIVE    = FALSE
          NomCGto:SENSITIVE                             = FALSE
          NomCxP:SENSITIVE                              = FALSE.
   IF Pro_Ahorros.Id_GMF EQ 1 THEN ASSIGN Pro_Ahorros.VrTope_ExentoEE:SENSITIVE      = TRUE
                                          Pro_Ahorros.VrTope_ExentoPension:SENSITIVE = TRUE.
   APPLY "Leave" TO Pro_Ahorros.Cta_XPagarGMF.   
   APPLY "Leave" TO Pro_Ahorros.Cta_GtoGMF. 
   FIND Formatos WHERE Formatos.Agencia     EQ W_Agencia AND
                       Formatos.Cod_Formato EQ Pro_Ahorros.Cod_Formato AND
                       Formatos.Id_Formato  EQ "AC" and
                       Formatos.Estado      EQ 1 NO-LOCK NO-ERROR.
   IF AVAIL Formatos THEN 
      W_CodFormato:SCREEN-VALUE = STRING(Formatos.Cod_Formato,"99") + " - " + Formatos.Nom_Formato.
   ELSE W_CodFormato:SCREEN-VALUE = "00 - No Asignado".
   FIND Formatos WHERE Formatos.Agencia     EQ W_Agencia AND
                       Formatos.Cod_Formato EQ Pro_Ahorros.Pro_Digito AND
                       Formatos.Id_Formato  EQ "AC" and
                       Formatos.Estado      EQ 1 NO-LOCK NO-ERROR.
   IF AVAIL Formatos
      THEN W_ProDigito:SCREEN-VALUE = STRING(Formatos.Cod_Formato,"99") + " - " + Formatos.Nom_Formato.
      ELSE W_ProDigito:SCREEN-VALUE = "00 - No Asignado".
END.
DO WITH FRAME F_Intereses:
   ENABLE ALL WITH FRAME F_Intereses.
   ASSIGN Pro_Ahorros.Id_Tasa:SCREEN-VALUE            = STRING(Pro_Ahorros.Id_Tasa)        
          Pro_Ahorros.Indicador:SCREEN-VALUE          = STRING(Pro_Ahorros.Indicador)
          Pro_Ahorros.Tip_Interes:SCREEN-VALUE        = STRING(Pro_Ahorros.Tip_Interes)
          Pro_Ahorros.Id_ForLiquidacion:SCREEN-VALUE  = STRING(Pro_Ahorros.Id_ForLiquidacion)
          Pro_Ahorros.FOR_Liquidacion:SCREEN-VALUE    = STRING(Pro_Ahorros.FOR_Liquidacion)
          Pro_Ahorros.Id_Sobregiro:SCREEN-VALUE       = STRING(Pro_Ahorros.Id_Sobregiro)
          Pro_Ahorros.Id_CanCalIntereses:SCREEN-VALUE = STRING(Pro_Ahorros.Id_CanCalIntereses)
          Pro_Ahorros.ProCre_Asociado:SCREEN-VALUE    = STRING(Pro_Ahorros.ProCre_Asociado).
   FIND FIRST Indicadores WHERE Indicadores.Indicador EQ Pro_Ahorros.Indicador
                            AND Indicadores.FecVcto   GE TODAY
                            AND Indicadores.Estado    EQ 1 NO-LOCK NO-ERROR.
   IF NOT AVAIL(Indicadores) THEN
      FIND FIRST Indicadores WHERE Indicadores.Indicador EQ Pro_Ahorros.Indicador
                               AND Indicadores.Estado    EQ 1 NO-LOCK NO-ERROR.
   IF AVAIL Indicadores THEN
      ASSIGN W_Nin:SCREEN-VALUE = Indicadores.Nombre
             W_Val:SCREEN-VALUE = STRING(Indicadores.Tasa)
             W_Nin W_Val.
   FIND Pro_Creditos WHERE pro_Creditos.cod_Credito EQ Pro_Ahorros.ProCre_Asociado
                       AND Pro_Creditos.Estado EQ 1 NO-LOCK NO-ERROR.
   IF AVAIL Pro_Creditos THEN Cmb_Creditos:SCREEN-VALUE = STRING(pro_Creditos.cod_Credito) + " - " + Pro_Creditos.Nom_Producto.
END.
DO WITH FRAME F_Liquidaciones:
   ENABLE ALL WITH FRAME F_Liquidaciones.
   ASSIGN Pro_Ahorros.Id_PerLiquidacion:SCREEN-VALUE    =  STRING(Pro_Ahorros.Id_PerLiquidacion)
          W_CmbPeriodo:SCREEN-VALUE                     = W_CmbPeriodo:ENTRY(Pro_Ahorros.Per_Liquidacion)
          Pro_Ahorros.Abo_Cuenta:SCREEN-VALUE           = STRING(Pro_Ahorros.Abo_Cuenta)
          Pro_Ahorros.Per_Pago:SCREEN-VALUE             = STRING(Pro_Ahorros.Per_Pago)
          Pro_Ahorros.Id_SdoMinLiquidacion:SCREEN-VALUE = STRING(Pro_Ahorros.Id_SdoMinLiquidacion)
          Pro_Ahorros.Mon_MinLiqidacion:SCREEN-VALUE    = STRING(Pro_Ahorros.Mon_MinLiqidacion)
          Pro_Ahorros.Id_PerGracia:SCREEN-VALUE         = STRING(Pro_Ahorros.Id_PerGracia)
          Pro_Ahorros.Dia_Gracia:SCREEN-VALUE           = STRING(Pro_Ahorros.Dia_Gracia)
          Pro_Ahorros.Bas_Calculo:SCREEN-VALUE          = STRING(Pro_Ahorros.Bas_Calculo)
          Pro_Ahorros.Tip_Anualidad:SCREEN-VALUE        = STRING(Pro_Ahorros.Tip_Anualidad).
END.
DO WITH FRAME F_Restricciones:
   ENABLE ALL WITH FRAME F_Restricciones.
   ASSIGN Pro_Ahorros.Id_Plazo:SCREEN-VALUE             = STRING(Pro_Ahorros.Id_Plazo)
          Pro_Ahorros.Pla_Minimo:SCREEN-VALUE           = STRING(Pro_Ahorros.Pla_Minimo)
          Pro_Ahorros.Pla_Maximo:SCREEN-VALUE           = STRING(Pro_Ahorros.Pla_Maximo)
          Pro_Ahorros.Id_SalMinimo:SCREEN-VALUE         = STRING(Pro_Ahorros.Id_SalMinimo)
          Pro_Ahorros.Tip_SalMinimo:SCREEN-VALUE        = STRING(Pro_Ahorros.Tip_SalMinimo)
          Pro_Ahorros.Val_SdoMinimo:SCREEN-VALUE        = STRING(Pro_Ahorros.Val_SdoMinimo)
          Pro_Ahorros.Id_Cuota:SCREEN-VALUE             = STRING(Pro_Ahorros.Id_Cuota)
          Pro_Ahorros.Val_Cuota:SCREEN-VALUE            = STRING(Pro_Ahorros.Val_Cuota)
          Pro_Ahorros.Id_MonApertura:SCREEN-VALUE       = STRING(Pro_Ahorros.Id_MonApertura)
          Pro_Ahorros.Val_MonAper:SCREEN-VALUE          = STRING(Pro_Ahorros.Val_MonAper)
          Pro_Ahorros.Id_BloInactividad:SCREEN-VALUE    = STRING(Pro_Ahorros.Id_BloInactividad)
          Pro_Ahorros.Tie_Inactividad:SCREEN-VALUE      = STRING(Pro_Ahorros.Tie_Inactividad)
          Pro_Ahorros.Id_MontoMinimo:SCREEN-VALUE       = STRING(Pro_Ahorros.Id_MontoMinimo)
          Pro_Ahorros.Val_MinRetiro:SCREEN-VALUE        = STRING(Pro_Ahorros.Val_MinRetiro)
          Pro_Ahorros.Val_MinConsignacion:SCREEN-VALUE  = STRING(Pro_Ahorros.Val_MinConsignacion)
          Pro_Ahorros.Val_MinRetCheque:SCREEN-VALUE     = STRING(Pro_Ahorros.Val_MinRetCheque)
          Pro_Ahorros.Id_MontoMaximo:SCREEN-VALUE       = STRING(Pro_Ahorros.Id_MontoMaximo)
          Pro_Ahorros.Val_MaxConsignacion:SCREEN-VALUE  = STRING(Pro_Ahorros.Val_MaxConsignacion)
          Pro_Ahorros.Val_MaxRetEfectivo:SCREEN-VALUE   = STRING(Pro_Ahorros.Val_MaxRetEfectivo)
          Pro_Ahorros.Id_AfeSdoMinimo:SCREEN-VALUE      = STRING(Pro_Ahorros.Id_AfeSdoMinimo).
   IF Pro_Ahorros.Id_RetParcial:SCREEN-VALUE EQ "no"
      THEN DISABLE Pro_Ahorros.Val_MinRetiro Pro_Ahorros.Val_MaxRetEfectivo Pro_Ahorros.Val_MinRetCheque.
      ELSE ENABLE  Pro_Ahorros.Val_MinRetiro Pro_Ahorros.Val_MaxRetEfectivo Pro_Ahorros.Val_MinRetCheque.
END.
RUN Habilitar_Deshabilitar.
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

  W_Reporte   = "REPORTE   : PRODUCTOS DE AHORRO - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
  W_EncColumna = " - PRODUCTO: " + STRING(pro_ahorros.cod_ahorro:SCREEN-VALUE  IN FRAME F_ProAhorros
) + " - " 
                 + Pro_Ahorros.Nom_Producto:SCREEN-VALUE.

  VIEW FRAME F-Encabezado.
  VIEW FRAME F-Ftr.

DO WITH FRAME F_ProAhorros:
    IF Pro_Ahorros.Estado:SCREEN-VALUE EQ "1" THEN i_Estado = "ACTIVO".
    ELSE I_Estado = "INACTIVO".
    CASE pro_ahorros.tip_ahorro:SCREEN-VALUE:
        WHEN "1" THEN i_TipPdt = "A LA VISTA".
        WHEN "2" THEN i_TipPdt = "CONTRACTUAL".
        WHEN "3" THEN i_TipPdt = "A TERMINO".
        WHEN "4" THEN i_TipPdt = "APORTES".
    END CASE.

    DISPLAY "INFORMACION ENCABEZADO"                            AT 1 SKIP(1)
            "Agencia             : "                            AT 1 
             "Tipo de Producto    : "                            AT 60
             i_TipPdt                                           AT 85
            "Fecha de Creación   : "                            AT 1
             Pro_Ahorros.Fec_Matricula                          AT 25
            "Estado del Producto : "                            AT 60
             i_Estado                                           AT 85
   WITH FRAME a WIDTH 132 NO-BOX USE-TEXT NO-LABELS.
END.

DO WITH FRAME F_General:
   CASE Pro_Ahorros.Id_Asociado:SCREEN-VALUE:
       WHEN "1" THEN i_Aplica = "TODOS LOS CLIENTES".
       WHEN "2" THEN i_Aplica = "ASOCIADOS".
       WHEN "3" THEN i_Aplica = "NO ASOCIADOS".
   END CASE.
   CASE Pro_Ahorros.Id_Talonario:SCREEN-VALUE:
       WHEN "1" THEN i_Libret = "LIBRETA".
       WHEN "2" THEN i_Libret = "CHEQUERA".
       WHEN "3" THEN i_Libret = "NO MANEJA".
   END CASE.
   DISPLAY SKIP(1)
           "INFORMACION GENERAL"                               AT 1 SKIP(1)
           "Producto Creado Para: "                            AT 1 
            i_Aplica                                           AT 25
           "Permite Db.Automatic: "                            AT 60
            Pro_Ahorros.Id_Debito:SCREEN-VALUE                 AT 85
           "Tipo de Talonario   : "                            AT 1
            I_Libret                                           AT 25
           "Valor Talonario     : "                            AT 60
            Pro_Ahorros.Val_Talonario:SCREEN-VALUE             AT 85
  WITH FRAME b WIDTH 132 NO-BOX USE-TEXT NO-LABELS.
END.

DO WITH FRAME F_Intereses:
   IF Pro_Ahorros.Id_Tasa:SCREEN-VALUE EQ "1" THEN i_IdTasa = "PRODUCTO".
   ELSE I_IdTasa = "CUENTA".
   IF Pro_Ahorros.Tip_Interes:SCREEN-VALUE EQ "1" THEN i_TipInt = "SIMPLE".
   ELSE I_TipInt = "COMPUESTO".

   DISPLAY SKIP(1)
           "INFORMACION INTEGERESES"                           AT 1 SKIP(1)
           "Tasa Manejada por   : "                            AT 1 
           i_IdTasa                                            AT 25
           "Valor de la Tasa    : "                            AT 60
           W_Val:SCREEN-VALUE                                  AT 85
           "Tipo de Interes     : "                            AT 1
           i_TipInt                                            AT 25
   WITH FRAME c WIDTH 132 NO-BOX USE-TEXT NO-LABELS.
END.
DO WITH FRAME F_Liquidaciones:
   CASE Pro_Ahorros.Bas_Calculo:SCREEN-VALUE:
       WHEN "1" THEN i_BasCal = "MINIMO".
       WHEN "2" THEN i_BasCal = "PROMEDIO".
       WHEN "3" THEN i_BasCal = "EFECTIVO".
   END CASE.
   IF Pro_Ahorros.Tip_Anualidad:SCREEN-VALUE EQ "1" THEN i_Anuali = "FIJA".
   ELSE I_Anuali = "VARIABLE".
   
   DISPLAY skip(1) "INFORMACION LIQUIDACIONES" AT 1 SKIP(1)
           "Base Calculo Interes: "                           AT 1 
           i_BasCal                                           AT 25
           "Anualidad           : "                           AT 60
           i_Anuali                                           AT 85
   WITH FRAME aa WIDTH 132 NO-BOX USE-TEXT NO-LABELS.           
   IF Pro_Ahorros.Id_SdoMinLiquidacion:SCREEN-VALUE EQ "yes" THEN
      DISPLAY 
           "Sdo.Min.Liquidacion : "                           AT 1
           Pro_Ahorros.Mon_MinLiqidacion                      AT 25
      WITH FRAME d WIDTH 132 NO-BOX USE-TEXT NO-LABELS.
   IF Pro_Ahorros.Id_PerLiquidacion:SCREEN-VALUE EQ "1" THEN
      DISPLAY 
           "Periodo Liquidacion : "                            AT 1
           W_CmbPeriodo:SCREEN-VALUE FORMAT "X(20)"            AT 25
      WITH FRAME e WIDTH 132 NO-BOX USE-TEXT NO-LABELS.
END.

DO WITH FRAME F_Restricciones:
   DISPLAY "RESTRICCIONES" AT 1 WITH FRAME bb.

   IF DECIMAL(Pro_Ahorros.Pla_Minimo:SCREEN-VALUE) NE 0 THEN
       DISPLAY "Plazo Mínimo en días: "                            AT 1
            Pro_Ahorros.Pla_Minimo:SCREEN-VALUE FORMAT "X(20)"  AT 25
       WITH FRAME f WIDTH 132 NO-BOX USE-TEXT NO-LABELS.
   IF DECIMAL(Pro_Ahorros.Pla_Maximo:SCREEN-VALUE) NE 0 THEN
       DISPLAY "Plazo Máximo en días: "                            AT 1
            Pro_Ahorros.Pla_Maximo:SCREEN-VALUE FORMAT "X(20)"  AT 25
       WITH FRAME g WIDTH 132 NO-BOX USE-TEXT NO-LABELS.
   IF DECIMAL(Pro_Ahorros.Val_SdoMinimo:SCREEN-VALUE) NE 0 THEN
       DISPLAY "Saldo Mínimo        : "                            AT 1
            Pro_Ahorros.Val_SdoMinimo:SCREEN-VALUE FORMAT "X(20)"  AT 25
       WITH FRAME h WIDTH 132 NO-BOX USE-TEXT NO-LABELS.
   IF DECIMAL(Pro_Ahorros.Val_Cuota:SCREEN-VALUE) NE 0 THEN
       DISPLAY "Cuota Mínima        : "                            AT 1
            Pro_Ahorros.Val_Cuota:SCREEN-VALUE FORMAT "X(20)"  AT 25
       WITH FRAME i WIDTH 132 NO-BOX USE-TEXT NO-LABELS.
   IF DECIMAL(Pro_Ahorros.Val_MonAper:SCREEN-VALUE) NE 0 THEN
       DISPLAY "Monto de Apertura   : "                            AT 1
            Pro_Ahorros.Val_MonAper:SCREEN-VALUE FORMAT "X(20)"  AT 25
       WITH FRAME j WIDTH 132 NO-BOX USE-TEXT NO-LABELS.
   IF DECIMAL(Pro_Ahorros.Val_MinRetiro:SCREEN-VALUE) NE 0 THEN
       DISPLAY "Mínimo de Retiro    : "                            AT 1
            Pro_Ahorros.Val_MinRetiro:SCREEN-VALUE FORMAT "X(20)"  AT 25
       WITH FRAME k WIDTH 132 NO-BOX USE-TEXT NO-LABELS.
   IF DECIMAL(Pro_Ahorros.Val_MinConsignacion:SCREEN-VALUE) NE 0 THEN
       DISPLAY "Mínimo Consignación : "                            AT 1
            Pro_Ahorros.Val_MinConsignacion:SCREEN-VALUE FORMAT "X(20)"  AT 25
       WITH FRAME l WIDTH 132 NO-BOX USE-TEXT NO-LABELS.
   IF DECIMAL(Pro_Ahorros.Val_MinRetCheque:SCREEN-VALUE) NE 0 THEN
       DISPLAY "Mín.Retiro Cheque   : "                            AT 1
            Pro_Ahorros.Val_MinRetCheque:SCREEN-VALUE FORMAT "X(20)"  AT 25
       WITH FRAME m WIDTH 132 NO-BOX USE-TEXT NO-LABELS.
   IF DECIMAL(Pro_Ahorros.Val_MaxConsignacion:SCREEN-VALUE) NE 0 THEN
       DISPLAY "Máximo Consignación : "                            AT 1
            Pro_Ahorros.Val_MaxConsignacion:SCREEN-VALUE FORMAT "X(20)"  AT 25
       WITH FRAME n WIDTH 132 NO-BOX USE-TEXT NO-LABELS.
   IF DECIMAL(Pro_Ahorros.Val_MaxRetEfectivo:SCREEN-VALUE) NE 0 THEN
       DISPLAY "Máximo Ret.Efectivo : "                            AT 1
            Pro_Ahorros.Val_MaxRetEfectivo:SCREEN-VALUE FORMAT "X(20)"  AT 25
       WITH FRAME o WIDTH 132 NO-BOX USE-TEXT NO-LABELS.
END.

     
    PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TodosProAge wWin 
PROCEDURE TodosProAge :
/*-Purpose:  Genera desde Of.Central todos los Productos para las demás Agencias.-*/
  /*DEFINE BUFFER Tmp_ProAho  FOR Pro_Ahorros.
  DEFINE BUFFER Tmp2_ProAho FOR Pro_Ahorros.
  ASSIGN W_Con   = 0
         W_Puntero = ROWID(Pro_Ahorros).
  DISPLAY W_Con WITH FRAME F_Replica.
  RUN Bor_RegPAho.

  FOR EACH Tmp2_ProAho WHERE Tmp2_ProAho.Agencia EQ W_Agencia
                         AND Tmp2_ProAho.Estado  EQ 1 NO-LOCK:
      CREATE RegPAho.
      BUFFER-COPY Tmp2_ProAho TO RegPAho.      
      ASSIGN RegPAho.Fec_Matricula   = TODAY 
             RegPAho.Fec_retiro      = ?
             RegPAho.Pro_Digito      = 0
             RegPAho.Cod_Formato     = 0
             RegPAho.Id_Consecutivo  = FALSE
             RegPAho.Num_Consecutivo = 0.

      FOR EACH Agencias FIELDS(Agencia.Agencia)
                        WHERE Agencias.Agencia NE W_Agencia
                          AND Agencias.Estado  NE 3 NO-LOCK:
          ASSIGN RegPAho.Agencia = Agencias.Agencia.
          FIND Tmp_ProAho WHERE Tmp_ProAho.Agencia      EQ RegPAho.Agencia
                            AND Tmp_ProAho.Tip_Producto EQ RegPAho.Tip_Producto
                            AND Tmp_ProAho.Cod_Producto EQ RegPAho.Cod_Producto NO-LOCK NO-ERROR.
          IF NOT AVAILABLE(Tmp_ProAho) THEN DO:
             CREATE Tmp_ProAho.
             BUFFER-COPY RegPAho TO Tmp_ProAho.
             ASSIGN W_Con = W_Con + 1.
             DISPLAY W_Con WITH FRAME F_Replica.
          END.
      END.
      DELETE RegPAho. 
  END.
  FIND Pro_Ahorros WHERE ROWID(Pro_Ahorros) EQ W_Puntero NO-LOCK NO-ERROR.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TodosProUnaAge wWin 
PROCEDURE TodosProUnaAge :
/*-Purpose:  Genera desde Of.Central todos los Productos para la Agencia Seleccionada.-*/
  /*DEFINE BUFFER Tmp_ProAho  FOR Pro_Ahorros.
  ASSIGN W_Con   = 0
         W_Puntero = ROWID(Pro_Ahorros).
  DISPLAY W_Con WITH FRAME F_Replica.
  RUN Bor_RegpAho.

  FOR EACH Tmp_ProAho WHERE Tmp_ProAho.Agencia EQ W_Agencia
                       AND  Tmp_ProAho.Estado  EQ 1 NO-LOCK:
      CREATE RegPAho.
      BUFFER-COPY Tmp_ProAho TO RegPAho.      
      ASSIGN RegPAho.Agencia         = W_AgeDest
             RegPAho.Fec_Matricula   = TODAY 
             RegPAho.Fec_retiro      = ?
             RegPAho.Pro_Digito      = 0
             RegPAho.Cod_Formato     = 0
             RegPAho.Id_Consecutivo  = FALSE
             RegPAho.Num_Consecutivo = 0.              
      FIND Pro_Ahorros WHERE Pro_Ahorros.Agencia      EQ W_AgeDest
                         AND pro_ahorros.tip_ahorro EQ RegPAho.Tip_Producto
                         AND pro_ahorros.cod_ahorro EQ RegPAho.Cod_Producto NO-LOCK NO-ERROR.
      IF NOT AVAILABLE(Pro_Ahorros) THEN DO:
         CREATE Pro_Ahorros.
         BUFFER-COPY RegPAho TO Pro_Ahorros.           
         W_Con = W_Con + 1.
         DISPLAY W_Con WITH FRAME F_Replica.
      END.  
      DELETE RegPAho.
  END.
  FIND Pro_Ahorros WHERE ROWID(Pro_Ahorros) EQ W_Puntero NO-LOCK NO-ERROR.  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UnProTodasAge wWin 
PROCEDURE UnProTodasAge :
/*-Purpose:  Genera desde Of.Central El Producto Seleccionado para las demás Agencias.-*/
  /*ASSIGN W_Con   = 0
         W_Puntero = ROWID(Pro_Ahorros).
  DISPLAY W_Con WITH FRAME F_Replica.
  RUN Bor_RegPAho.  
  FIND Pro_Ahorros WHERE Pro_Ahorros.Agencia      EQ W_Agencia
                     AND pro_ahorros.cod_ahorro EQ CodWork
                     AND Pro_Ahorros.Estado       EQ 1
                     NO-LOCK NO-ERROR.
  IF AVAILABLE (Pro_Ahorros) THEN DO:
     BUFFER-COPY Pro_Ahorros TO RegPAho.
     FOR EACH Agencias FIELDS (Agencia) WHERE Agencias.Agencia NE W_Agencia
                                          AND Agencias.Estado  NE 3 NO-LOCK:
         FIND Pro_Ahorros WHERE Pro_Ahorros.Agencia      EQ Agencias.Agencia
                            AND pro_ahorros.tip_ahorro EQ RegPAho.Tip_Producto
                            AND pro_ahorros.cod_ahorro EQ CodWork NO-LOCK NO-ERROR.
         IF NOT AVAILABLE(Pro_Ahorros) THEN DO:
            ASSIGN RegPAho.Agencia         = Agencias.Agencia
                   RegPAho.Fec_Matricula   = TODAY 
                   RegPAho.Fec_retiro      = ?
                   RegPAho.Pro_Digito      = 0
                   RegPAho.Cod_Formato     = 0
                   RegPAho.Id_Consecutivo  = FALSE
                   RegPAho.Num_Consecutivo = 0.            
            CREATE Pro_Ahorros.
            BUFFER-COPY RegPAho TO Pro_Ahorros.           
            W_Con = W_Con + 1.
            DISPLAY W_Con WITH FRAME F_Replica.         
         END.
     END.
     DELETE RegPAho.
  END.
  FIND Pro_Ahorros WHERE ROWID(Pro_Ahorros) EQ W_Puntero NO-LOCK NO-ERROR.*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UnProUnaAge wWin 
PROCEDURE UnProUnaAge :
/*ASSIGN W_Con   = 0
         W_Puntero = ROWID(Pro_Ahorros).
  DISPLAY W_Con WITH FRAME F_Replica.
  
  CodWork = INTEGER(pro_ahorros.cod_ahorro:SCREEN-VALUE IN FRAME F_ProAhorros).
  RUN Bor_RegPAho.
  FIND Pro_Ahorros WHERE Pro_Ahorros.Agencia      EQ W_Agencia
                     AND pro_ahorros.cod_ahorro EQ CodWork
                     AND Pro_Ahorros.Estado       EQ 1
                     NO-LOCK NO-ERROR.
  IF AVAILABLE (Pro_Ahorros) THEN DO:
     BUFFER-COPY Pro_Ahorros TO RegPAho.
     FIND Pro_Ahorros WHERE Pro_Ahorros.Agencia      EQ W_AgeDest
                        AND pro_ahorros.tip_ahorro EQ RegPAho.Tip_Producto
                        AND pro_ahorros.cod_ahorro EQ CodWork NO-LOCK NO-ERROR.
     IF NOT AVAILABLE(Pro_Ahorros) THEN DO:
        ASSIGN RegPAho.Agencia         = W_AgeDest
               RegPAho.Fec_Matricula   = TODAY 
               RegPAho.Fec_retiro      = ?
               RegPAho.Pro_Digito      = 0
               RegPAho.Cod_Formato     = 0
               RegPAho.Id_Consecutivo  = FALSE
               RegPAho.Num_Consecutivo = 0.           
        CREATE Pro_Ahorros.
        BUFFER-COPY RegPAho TO Pro_Ahorros.           
        DELETE RegPAho.
        W_Con = W_Con + 1.
        DISPLAY W_Con WITH FRAME F_Replica.     
     END.
  END.
  FIND Pro_Ahorros WHERE ROWID(Pro_Ahorros) EQ W_Puntero NO-LOCK NO-ERROR.  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UnTipoTodasAge wWin 
PROCEDURE UnTipoTodasAge :
/*-Purpose:  Genera desde Of.Central todos los Productos del Tipo seleccionado (Rs_SelecProd) para las demás Agencias.-*/
/*  DEFINE INPUT PARAMETER W_tipoP AS INTEGER.  
  DEFINE BUFFER Tmp_ProAho      FOR Pro_Ahorros.

  ASSIGN W_Con   = 0
         W_Puntero = ROWID(Pro_Ahorros).
  DISPLAY W_Con WITH FRAME F_Replica.
  RUN Bor_RegPAho.
  FOR EACH Tmp_ProAho WHERE Tmp_ProAho.Agencia      EQ W_Agencia
                        AND Tmp_ProAho.Tip_Producto EQ W_tipoP
                        AND Tmp_ProAho.Estado       EQ 1 NO-LOCK:
      CREATE RegPAho.
      BUFFER-COPY Tmp_ProAho TO RegPAho.      
      FOR EACH Agencias FIELDS (Agencia) WHERE Agencias.Agencia NE W_Agencia
                                           AND Agencias.Estado  NE 3 NO-LOCK:
          FIND Pro_Ahorros WHERE Pro_Ahorros.Agencia      EQ Agencias.Agencia
                             AND pro_ahorros.cod_ahorro EQ RegPAho.Cod_Producto
                             AND pro_ahorros.tip_ahorro EQ W_tipoP NO-LOCK NO-ERROR.
          IF NOT AVAILABLE(Pro_Ahorros) THEN DO:
             ASSIGN RegPAho.Agencia         = Agencias.Agencia
                    RegPAho.Fec_Matricula   = TODAY 
                    RegPAho.Fec_retiro      = ?
                    RegPAho.Pro_Digito      = 0
                    RegPAho.Cod_Formato     = 0
                    RegPAho.Id_Consecutivo  = FALSE
                    RegPAho.Num_Consecutivo = 0.              
             CREATE Pro_Ahorros.
             BUFFER-COPY RegPAho TO Pro_Ahorros.           
             W_Con = W_Con + 1.
             DISPLAY W_Con WITH FRAME F_Replica.
          END.
      END.
  DELETE RegPAho.
  END.
  FIND Pro_Ahorros WHERE ROWID(Pro_Ahorros) EQ W_Puntero NO-LOCK NO-ERROR.  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UnTipoUnaAge wWin 
PROCEDURE UnTipoUnaAge :
/*-Purpose:  Genera desde Of.Central todos los Productos del Tipo seleccionado
            (Rs_SelecProd) para la Agencia Seleccionada.-*/
  /*DEFINE INPUT PARAMETER W_tipoP AS INTEGER.
  DEFINE BUFFER Tmp_ProAho       FOR Pro_Ahorros.
    
  ASSIGN W_Con   = 0
         W_Puntero = ROWID(Pro_Ahorros).
  DISPLAY W_Con WITH FRAME F_Replica.
  RUN Bor_RegPAho.  
  FOR EACH Tmp_ProAho WHERE Tmp_ProAho.Agencia      EQ W_Agencia
                        AND Tmp_ProAho.Tip_Producto EQ W_TipoP
                        AND Tmp_ProAho.Estado       EQ 1 NO-LOCK:
      CREATE RegPAho.
      BUFFER-COPY Tmp_ProAho TO RegPAho.      
      ASSIGN RegPAho.Agencia         = W_AgeDest
             RegPAho.Fec_Matricula   = TODAY 
             RegPAho.Fec_retiro      = ?
             RegPAho.Pro_Digito      = 0
             RegPAho.Cod_Formato     = 0
             RegPAho.Id_Consecutivo  = FALSE
             RegPAho.Num_Consecutivo = 0.              
      FIND Pro_Ahorros WHERE Pro_Ahorros.Agencia      EQ W_AgeDest
                         AND pro_ahorros.cod_ahorro EQ RegPAho.Cod_Producto
                         AND pro_ahorros.tip_ahorro EQ W_TipoP NO-LOCK NO-ERROR.
      IF NOT AVAILABLE(Pro_Ahorros) THEN DO:
         CREATE Pro_Ahorros.
         BUFFER-COPY RegPAho TO Pro_Ahorros.           
         W_Con = W_Con + 1.
         DISPLAY W_Con WITH FRAME F_Replica.
      END.  
  DELETE RegPAho.
  END.
  FIND Pro_Ahorros WHERE ROWID(Pro_Ahorros) EQ W_Puntero NO-LOCK NO-ERROR.*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Validaciones wWin 
PROCEDURE Validaciones :
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

