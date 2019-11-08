&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME Wwin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Wwin 
/*------------------------------------------------------------------------
  File: W-ProRec_Refinancia.w
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
/*----------------------------------------------------------------------*/

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
{INCLUIDO\VARIABLE.I "SHARED"}
DEFINE INPUT PARAMETER P_NomIns AS CHARACTER FORMAT "X(50)".
DEFINE INPUT PARAMETER P_NitCli LIKE Clientes.Nit.
DEFINE INPUT PARAMETER P_CodCre LIKE Creditos.Cod_Credito.
DEFINE INPUT PARAMETER P_TipCre LIKE Creditos.Tip_Credito.
DEFINE INPUT PARAMETER P_NumCre LIKE Creditos.Num_Credito.
DEFINE INPUT PARAMETER P_CodOpe LIKE Operacion.Cod_Operacion.
DEFINE INPUT PARAMETER P_Progra AS INTEGER FORMAT "9".

  DEFI VAR Pagas   LIKE Creditos.Cuo_Pagadas.
  DEFI VAR FecPP   LIKE Creditos.Fec_Pago.   
  
  DEFI   VAR W_PdoPag     AS INTEG FORM "99".
  DEFI   VAR W_SiPdo      AS LOG   INIT FALSE.
  DEFI   VAR W_FecControl AS DATE.
  DEFI   VAR W_QnaDec     AS INTEG FORM "9" INIT 0.
  DEFI   VAR W_RowIdPP    AS ROWID.
  
/*para buscar un cliente*/
  DEFINE VARIABLE P_Nit       LIKE Clientes.Nit.
  DEFINE VARIABLE p_Nombre    LIKE Clientes.Nombre.
  DEFINE VARIABLE P_Apellido  LIKE Clientes.Apellido1.
  DEFINE VARIABLE P_AgeCli    LIKE Clientes.Agencia.
  DEFINE VARIABLE W_Nuevo     AS LOGICAL.
  DEFI   VAR      W_CuoExtra  LIKE Creditos.Sdo_Capital INIT 0.
  DEFINE VAR      W_Contenido AS CHARACTER FORMAT "X(400)".
  DEFI   VAR      W_NvoPdo    AS INTEG FORM "999" INIT 12.

  DEFI TEMP-TABLE TPlanP  LIKE PlanPagos.
  DEFI TEMP-TABLE CPlanP  LIKE PlanPagos.
  DEFI TEMP-TABLE TExtras LIKE Extras.
  
/*guarda los usuarios disponibles para la siguiente instancia*/
  DEFINE TEMP-TABLE TProIns
    FIELD TP_Agencia LIKE Agencias.Agencia
    FIELD TP_Orden LIKE Instancias.Orden_Instancia
    FIELD TP_Instancia LIKE Instancias.Instancia
    FIELD TP_NomInstan AS CHARACTER FORMAT "X(30)"
    FIELD TP_Usuario   LIKE Usuarios.Usuario
    FIELD TP_NomUsuar  AS CHARACTER FORMAT "X(30)"
    FIELD TP_Cantidad  AS INTEGER FORMAT "999".
    
/*Contiene los usuarios por instancia*/  
  DEFINE TEMP-TABLE TUXI
    FIELD Agencia  LIKE Usuarios.Agencia
    FIELD Usuario  LIKE Usuarios.Usuario
    FIELD Nombre   LIKE Usuarios.Nombre
    FIELD Cantidad AS INTEGER FORMAT "999"
    FIELD Proceso  AS LOGICAL.
    
/*para guardar las instancias de una solicitud*/
  DEFINE TEMP-TABLE TCerradas
    FIELD Instancia      LIKE Mov_Instancias.Instancia
    FIELD INom_Instancia AS CHARACTER FORMAT "X(20)"
    FIELD Fec_Ingreso    LIKE Mov_Instancias.Fec_Ingreso
    FIELD Fec_Retiro     LIKE Mov_Instancias.Fec_Retiro
    FIELD Hora_Ingreso   LIKE Mov_Instancias.Hora_Ingreso
    FIELD Hora_Retiro    LIKE Mov_Instancias.Hora_Retiro
    FIELD Estado         LIKE Mov_Instancias.Estado
    FIELD Num_Solicitud   LIKE Mov_Instancias.Num_Solicitud
    FIELD Usuario        LIKE Mov_Instancias.Usuario
    FIELD INom_Usuario   AS CHARACTER FORMAT "X(30)"
    FIELD Descripcion    LIKE Mov_Instancias.Descripcion.
  
  
  DEFINE VARIABLE i AS INTEGER.
  DEFINE VARIABLE W_Ok AS LOGICAL.
  DEFINE VARIABLE W_TipoProducto LIKE Pro_Creditos.Tip_Credito.
  DEFINE VARIABLE Dias      AS DECIMAL.
  DEFI   VAR      W_PdoAno  AS INTEG FORM "99".
  
  DEFINE TEMP-TABLE TIns    LIKE Cfg_Instancias.
  
  DEFINE TEMP-TABLE TScoring
      FIELD CodS LIKE Scoring.Codigo
      FIELD TabS LIKE Pro_Scoring.Tabla
      FIELD VarS LIKE Scoring.VARIABLE
      FIELD VVaS LIKE Scoring.Valor_Variable
      FIELD PunS LIKE Scoring.Puntaje
      FIELD FecS LIKE Scoring.Fec_Scoring.
      
 DEFINE TEMP-TABLE Consulta
      FIELD Num_Credito   LIKE Creditos.Num_Credito
      FIELD Num_Solicitud LIKE Creditos.Num_Solicitud
      FIELD AgeCredito    LIKE Agencias.Agencia
      FIELD Nit           LIKE Clientes.Nit
      FIELD Estado        LIKE Creditos.Estado
      FIELD Nombre        AS CHARACTER FORMAT "X(40)"
      FIELD Fec_Ingreso   LIKE Mov_Instancias.Fec_Ingreso
      FIELD Hor_Ingreso   AS CHARACTER FORMAT "X(15)"
      FIELD Monto         LIKE Solicitud.Monto
      FIELD Vigencia      AS INTEGER FORMAT "9999".

DEFINE VAR vTime AS INTEGER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Cre
&Scoped-define BROWSE-NAME Br_Extras

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TExtras Clientes

/* Definitions for BROWSE Br_Extras                                     */
&Scoped-define FIELDS-IN-QUERY-Br_Extras TExtras.Agencia TExtras.Cod_Credito TExtras.Nit TExtras.Num_Solicitud TExtras.Nro_Cuota TExtras.Vr_CuoExtra TExtras.Fec_Vcto   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Extras   
&Scoped-define SELF-NAME Br_Extras
&Scoped-define QUERY-STRING-Br_Extras FOR EACH TExtras WHERE TExtras.Nit           EQ Creditos.Nit                                        AND  TExtras.Num_Solicitud EQ Creditos.Num_Credito                         NO-LOCK BY TExtras.Nro_Cuota INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br_Extras OPEN QUERY Br_Extras FOR EACH TExtras WHERE TExtras.Nit           EQ Creditos.Nit                                        AND  TExtras.Num_Solicitud EQ Creditos.Num_Credito                         NO-LOCK BY TExtras.Nro_Cuota INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_Extras TExtras
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Extras TExtras


/* Definitions for FRAME F_Cre                                          */
&Scoped-define FIELDS-IN-QUERY-F_Cre Clientes.Calificacion ~
Clientes.Reestructurado 
&Scoped-define ENABLED-FIELDS-IN-QUERY-F_Cre Clientes.Calificacion 
&Scoped-define ENABLED-TABLES-IN-QUERY-F_Cre Clientes
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-F_Cre Clientes
&Scoped-define QUERY-STRING-F_Cre FOR EACH Clientes SHARE-LOCK
&Scoped-define OPEN-QUERY-F_Cre OPEN QUERY F_Cre FOR EACH Clientes SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-F_Cre Clientes
&Scoped-define FIRST-TABLE-IN-QUERY-F_Cre Clientes


/* Definitions for FRAME F_Extras                                       */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Extras ~
    ~{&OPEN-QUERY-Br_Extras}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Clientes.Calificacion 
&Scoped-define ENABLED-TABLES Clientes
&Scoped-define FIRST-ENABLED-TABLE Clientes
&Scoped-Define ENABLED-OBJECTS RECT-296 RECT-320 RECT-323 RECT-326 RECT-328 ~
RECT-329 RECT-330 RECT-331 RECT-332 Rs_Op W_FecIni Btn_Acepta_Terminos ~
Btn_Salir 
&Scoped-Define DISPLAYED-FIELDS Creditos.Fec_Desembolso Creditos.Fec_Pago ~
Creditos.Sdo_Capital Creditos.Fec_UltPago Creditos.Plazo Creditos.Cuota ~
Creditos.Tasa Creditos.Int_DifCobro Creditos.Int_Corrientes ~
Creditos.Categoria Creditos.Int_Anticipado Creditos.Reestructurado ~
Creditos.Cod_Califica Creditos.Sdo_Proyectado Clientes.Calificacion ~
Creditos.Cuo_Pagadas Clientes.Reestructurado Creditos.Val_Atraso ~
Creditos.Dias_Atraso Creditos.Cuo_Atraso Creditos.Provision ~
Creditos.Fec_Reestructurado 
&Scoped-define DISPLAYED-TABLES Creditos Clientes
&Scoped-define FIRST-DISPLAYED-TABLE Creditos
&Scoped-define SECOND-DISPLAYED-TABLE Clientes
&Scoped-Define DISPLAYED-OBJECTS Rs_Op W_PerPag W_TotExt W_CuoFaltan ~
W_NvoPlazo W_SdoOtros W_NvaCuota W_IntMora W_NvaTasa W_CuoFalNva W_Reest ~
W_FecIni 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR Wwin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Acepta_Terminos 
     LABEL "Aceptar Nuevos Términos" 
     SIZE 18.57 BY 1.23.

DEFINE BUTTON Btn_Salir 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 176" 
     SIZE 10.57 BY 2.04.

DEFINE VARIABLE W_CuoFalNva AS DECIMAL FORMAT "9999":U INITIAL 0 
     LABEL "Nuevas Cuotas X Pagar" 
     VIEW-AS FILL-IN 
     SIZE 5.43 BY .81
     BGCOLOR 12 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CuoFaltan AS DECIMAL FORMAT "9999":U INITIAL 0 
     LABEL "Cuotas X Pagar" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_FecIni AS DATE FORMAT "99/99/9999":U 
     LABEL "Nueva Fec-Inicio" 
     VIEW-AS FILL-IN 
     SIZE 10.57 BY .85
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_IntMora AS DECIMAL FORMAT "->>>>>,>>>,>>>,>>9.99" INITIAL 0 
     LABEL "Interès Moratorio" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .81
     BGCOLOR 18 FGCOLOR 15 .

DEFINE VARIABLE W_NvaCuota AS DECIMAL FORMAT ">>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Nueva Cuota" 
     VIEW-AS FILL-IN 
     SIZE 11.86 BY .92
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NvaTasa AS DECIMAL FORMAT ">>9.9999999":U INITIAL 0 
     LABEL "Nueva Tasa" 
     VIEW-AS FILL-IN 
     SIZE 11.57 BY .81 TOOLTIP "Nueva Tasa para el Crédito"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NvoPlazo AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Nuevo Plazo" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .85
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_PerPag AS CHARACTER FORMAT "X(15)":U 
     LABEL "Pdo de Pago" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .81
     BGCOLOR 12 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_SdoOtros AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Sdo.Cobro Jurídico" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_TotExt AS DECIMAL FORMAT "->>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Valor Extras" 
     VIEW-AS FILL-IN 
     SIZE 12.72 BY .81
     BGCOLOR 5 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Rs_Op AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "ReCalcular Plazo", 1,
"ReCalcular Cuota", 2,
"ReFinanciar -ReInicio-", 3,
"Clàusula Aceleratoria", 4,
"Modificar Condiciones", 5
     SIZE 18.57 BY 4.73
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Reest AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Si ReEstructurar", 1,
"No ReEstructurar", 2
     SIZE 14.57 BY 1.08 NO-UNDO.

DEFINE RECTANGLE RECT-296
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 26.72 BY 13.42.

DEFINE RECTANGLE RECT-320
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 43.14 BY 4.77.

DEFINE RECTANGLE RECT-323
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 18.86 BY 1.58.

DEFINE RECTANGLE RECT-326
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 20.86 BY .96.

DEFINE RECTANGLE RECT-328
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 16.57 BY .81.

DEFINE RECTANGLE RECT-329
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 17.14 BY 3.81.

DEFINE RECTANGLE RECT-330
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 21.72 BY .62.

DEFINE RECTANGLE RECT-331
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 85.14 BY 2.15.

DEFINE RECTANGLE RECT-332
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 42.43 BY 1.31.

DEFINE BUTTON Btn_AdExt 
     LABEL "&Salvar Extra" 
     SIZE 15.43 BY 1.12.

DEFINE BUTTON Btn_EliExt 
     LABEL "&Eliminar Extra" 
     SIZE 15.43 BY 1.12.

DEFINE BUTTON BUTTON-170 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 170" 
     SIZE 10.57 BY 1.62 TOOLTIP "Retorna a la Ventana Principal".

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
      TExtras SCROLLING.

DEFINE QUERY F_Cre FOR 
      Clientes SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Br_Extras
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Extras Wwin _FREEFORM
  QUERY Br_Extras NO-LOCK DISPLAY
      TExtras.Agencia FORMAT "999":U                        COLUMN-LABEL "Ag."
      TExtras.Cod_Credito FORMAT "999":U                    COLUMN-LABEL "Pdcto"
      TExtras.Nit FORMAT "X(12)":U                          COLUMN-LABEL "Ced./Nit"
      TExtras.Num_Solicitud FORMAT "99999999":U             COLUMN-LABEL "No.Solicitud"
      TExtras.Nro_Cuota FORMAT "9999":U                     COLUMN-LABEL "Pdo.Pago"
      TExtras.Vr_CuoExtra FORMAT "->>>>,>>>,>>9.99":U       COLUMN-LABEL "Vr.Cuota Extra"
      TExtras.Fec_Vcto FORMAT "99/99/9999":U                COLUMN-LABEL "Fecha-Vcto"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 70.72 BY 7.88
         BGCOLOR 15 FONT 5 ROW-HEIGHT-CHARS .62 FIT-LAST-COLUMN TOOLTIP "Con CLICK selecciona la Extra para ser Modificada y/o Eliminarla".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Extras
     Btn_AdExt AT ROW 2.23 COL 40
     W_PPExtra AT ROW 2.27 COL 3.72 COLON-ALIGNED NO-LABEL
     W_VrExtra AT ROW 2.27 COL 17.43 COLON-ALIGNED NO-LABEL
     Btn_EliExt AT ROW 3.65 COL 40.14
     BUTTON-170 AT ROW 4.73 COL 58.86
     Br_Extras AT ROW 6.46 COL 1.72
     "Pdo.Pago         Valor Cuota Extra" VIEW-AS TEXT
          SIZE 35 BY .62 AT ROW 1.38 COL 4.72
          BGCOLOR 18 FGCOLOR 15 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE 
         AT COL 2.14 ROW 1
         SIZE 72.57 BY 14.62
         BGCOLOR 17 
         TITLE "Extras Futuras Pactadas".

DEFINE FRAME F_Cre
     Rs_Op AT ROW 1.88 COL 28.29 NO-LABEL
     W_PerPag AT ROW 2.04 COL 81.72 COLON-ALIGNED
     Creditos.Fec_Desembolso AT ROW 2.08 COL 14 COLON-ALIGNED
          LABEL "Fecha Desembolso"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_TotExt AT ROW 2.27 COL 56.14 COLON-ALIGNED
     Creditos.Fec_Pago AT ROW 2.88 COL 14 COLON-ALIGNED
          LABEL "Fec.Próximo Pago"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_CuoFaltan AT ROW 2.92 COL 81.72 COLON-ALIGNED
     Creditos.Sdo_Capital AT ROW 3.27 COL 58 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 12 FGCOLOR 15 
     Creditos.Fec_UltPago AT ROW 3.69 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_NvoPlazo AT ROW 3.85 COL 81.72 COLON-ALIGNED
     Creditos.Plazo AT ROW 4.15 COL 58 COLON-ALIGNED
          LABEL "Plazo Actual"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_SdoOtros AT ROW 4.54 COL 14 COLON-ALIGNED
     W_NvaCuota AT ROW 4.88 COL 81.72 COLON-ALIGNED
     Creditos.Cuota AT ROW 5.08 COL 58 COLON-ALIGNED
          LABEL "Cuota Actual"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_IntMora AT ROW 5.31 COL 14 COLON-ALIGNED
     Creditos.Tasa AT ROW 5.96 COL 58 COLON-ALIGNED
          LABEL "T.Nominal Anual"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_NvaTasa AT ROW 5.96 COL 81.86 COLON-ALIGNED
     Creditos.Int_DifCobro AT ROW 6.12 COL 14 COLON-ALIGNED
          LABEL "Interés Contingente"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Int_Corrientes AT ROW 6.96 COL 14 COLON-ALIGNED
          LABEL "Interés Corriente"
          VIEW-AS FILL-IN 
          SIZE 11.14 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_CuoFalNva AT ROW 7.12 COL 87.86 COLON-ALIGNED
     Creditos.Categoria AT ROW 7.73 COL 65 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 3 BY .81
     Creditos.Int_Anticipado AT ROW 7.77 COL 14 COLON-ALIGNED
          LABEL "Interés-Anticipado"
          VIEW-AS FILL-IN 
          SIZE 11.14 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Reestructurado AT ROW 7.85 COL 29.14 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "No-Aplica", 0,
"Reestructurado", 1,
"No-Reestructurado", 2
          SIZE 15.86 BY 3.12
          BGCOLOR 15 
     W_Reest AT ROW 8.38 COL 78.57 NO-LABEL
     Creditos.Cod_Califica AT ROW 8.54 COL 66 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 2 BY .81
     Creditos.Sdo_Proyectado AT ROW 8.65 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 12 FGCOLOR 15 
     Clientes.Calificacion AT ROW 9.35 COL 66 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 1.86 BY .81
     Creditos.Cuo_Pagadas AT ROW 9.46 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Clientes.Reestructurado AT ROW 10.15 COL 65 COLON-ALIGNED NO-LABEL WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 2.86 BY .81
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 98.43 BY 16.54
         BGCOLOR 17 FONT 4.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F_Cre
     W_FecIni AT ROW 10.15 COL 82.43 COLON-ALIGNED
     Creditos.Val_Atraso AT ROW 10.27 COL 14 COLON-ALIGNED
          LABEL "Capital_Vencido" FORMAT "->,>>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 12 FGCOLOR 15 
     Creditos.Dias_Atraso AT ROW 11.08 COL 14 COLON-ALIGNED
          LABEL "Dias_Vencidos" FORMAT "-99999"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 12 FGCOLOR 15 
     Btn_Acepta_Terminos AT ROW 11.46 COL 76.72
     Creditos.Cuo_Atraso AT ROW 11.88 COL 14 COLON-ALIGNED
          LABEL "Cuotas_Vencidas"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 12 FGCOLOR 15 
     Creditos.Provision AT ROW 12.69 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Fec_Reestructurado AT ROW 13.46 COL 14 COLON-ALIGNED
          LABEL "Fec-Reestructurado"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Btn_Salir AT ROW 14.54 COL 87.72
     "Nro. Restruct." VIEW-AS TEXT
          SIZE 9 BY .5 AT ROW 10.42 COL 57 WIDGET-ID 10
     "Nuevas Condiciones" VIEW-AS TEXT
          SIZE 18.43 BY .5 AT ROW 7.08 COL 38.72
          BGCOLOR 12 FONT 5
     "Acción a Procesar" VIEW-AS TEXT
          SIZE 16.14 BY .58 AT ROW 1.23 COL 29.14
          FGCOLOR 7 FONT 5
     "Información del Crédito" VIEW-AS TEXT
          SIZE 19.57 BY .77 AT ROW 1.19 COL 4.43
          FGCOLOR 7 FONT 5
     RECT-296 AT ROW 1 COL 1.29
     RECT-320 AT ROW 7 COL 28.14
     RECT-323 AT ROW 8.12 COL 76.43
     RECT-326 AT ROW 1.04 COL 3.72
     RECT-328 AT ROW 1.12 COL 28.86
     RECT-329 AT ROW 7.69 COL 28.86
     RECT-330 AT ROW 7.04 COL 37
     RECT-331 AT ROW 14.5 COL 1.29
     RECT-332 AT ROW 12.96 COL 28.72
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 98.43 BY 16.54
         BGCOLOR 17 FONT 4
         TITLE "".


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
  CREATE WINDOW Wwin ASSIGN
         HIDDEN             = YES
         TITLE              = "Refinanciación Créditos, Programa W-ProRec_Refinancia.W"
         COLUMN             = 9.29
         ROW                = 3.88
         HEIGHT             = 16.54
         WIDTH              = 98.43
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.14
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.14
         CONTROL-BOX        = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         ALWAYS-ON-TOP      = yes
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Wwin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW Wwin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F_Cre
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN Creditos.Categoria IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Cod_Califica IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Cuota IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Cuo_Atraso IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Cuo_Pagadas IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Dias_Atraso IN FRAME F_Cre
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN Creditos.Fec_Desembolso IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Fec_Pago IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Fec_Reestructurado IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Fec_UltPago IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Int_Anticipado IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Int_Corrientes IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Int_DifCobro IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Plazo IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Provision IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Clientes.Reestructurado IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET Creditos.Reestructurado IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Sdo_Capital IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Sdo_Proyectado IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Tasa IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Val_Atraso IN FRAME F_Cre
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN W_CuoFalNva IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_CuoFaltan IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_IntMora IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NvaCuota IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NvaTasa IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NvoPlazo IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_PerPag IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET W_Reest IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_SdoOtros IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_TotExt IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Extras
   NOT-VISIBLE                                                          */
/* BROWSE-TAB Br_Extras BUTTON-170 F_Extras */
ASSIGN 
       FRAME F_Extras:HIDDEN           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(Wwin)
THEN Wwin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_Extras
/* Query rebuild information for BROWSE Br_Extras
     _START_FREEFORM
OPEN QUERY Br_Extras FOR EACH TExtras WHERE TExtras.Nit           EQ Creditos.Nit
                                       AND  TExtras.Num_Solicitud EQ Creditos.Num_Credito
                        NO-LOCK BY TExtras.Nro_Cuota INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE Br_Extras */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Cre
/* Query rebuild information for FRAME F_Cre
     _TblList          = "bdcentral.Clientes"
     _Query            is NOT OPENED
*/  /* FRAME F_Cre */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Extras
/* Query rebuild information for FRAME F_Extras
     _Query            is NOT OPENED
*/  /* FRAME F_Extras */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Wwin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Wwin Wwin
ON END-ERROR OF Wwin /* Refinanciación Créditos, Programa W-ProRec_Refinancia.W */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Wwin Wwin
ON WINDOW-CLOSE OF Wwin /* Refinanciación Créditos, Programa W-ProRec_Refinancia.W */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br_Extras
&Scoped-define FRAME-NAME F_Extras
&Scoped-define SELF-NAME Br_Extras
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Extras Wwin
ON MOUSE-SELECT-CLICK OF Br_Extras IN FRAME F_Extras
DO:
   IF AVAIL(TExtras) THEN
      ASSIGN W_PPExtra              = TExtras.Nro_Cuota 
             W_PPExtra:SCREEN-VALUE = STRING(TExtras.Nro_Cuota)
             W_VrExtra              = TExtras.Vr_CuoExtra   
             W_VrExtra:SCREEN-VALUE = STRING(TExtras.Vr_CuoExtra).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Cre
&Scoped-define SELF-NAME Btn_Acepta_Terminos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Acepta_Terminos Wwin
ON CHOOSE OF Btn_Acepta_Terminos IN FRAME F_Cre /* Aceptar Nuevos Términos */
DO:
  DEFI VAR NomOp    AS CHAR FORM "X(20)".
  DEFI VAR NomReest AS CHAR FORM "X(20)" INIT "No - ReEstructurado".
  DEFI VAR NomReIn  AS CHAR FORM "X(15)" INIT "No - ReIniciar".
  DEFI VAR W_CuoYaPag LIKE Creditos.Cuo_Pagadas.
  DEFI VAR W_RowIdCr  AS ROWID.
  DEFI VAR W_RowIdPP  AS ROWID.
  DEFI VAR W_TasaUs   LIKE Indicadores.Tasa.

  vTime = TIME.

  
  IF Rs_Op EQ 1 THEN
     ASSIGN NomOp = "Rebajar Plazo".
  ELSE IF Rs_Op EQ 2 THEN
     ASSIGN NomOp = "Rebajar Cuota".
  ELSE IF Rs_Op EQ 3 THEN
     ASSIGN NomOp = "ReFinanciar -ReInicio-".
  ELSE IF Rs_Op EQ 4 THEN
     ASSIGN NomOp = "Clàusula Aceleratoria".
  ELSE 
     ASSIGN NomOp = "Modifición Condiciones".

    IF W_Reest EQ 1 OR Creditos.ReEstructurado:SCREEN-VALUE EQ "1" THEN
     NomReest = "SI ReEstructurado".
  
  MESSAGE "El Crèdito lo Liquidò con la siguiente OPCIÓN: " NomOp SKIP
          "                                                          " NomReIn SKIP
          "Plazo del Crèdito : " Creditos.Plazo    "            Plazo Nuevo : " W_NvoPlazo   SKIP
          "Cuota del Crèdito : " Creditos.Cuota    "            Cuota Nueva : " W_NvaCuota   SKIP
          "Cuotas X Pagar    : " W_CuoFaltan     "    Nuevas Cuotas X Pagar : "  W_CuoFalNva SKIP
          "Tasa del Crèdito  : " Creditos.Tasa    "              Tasa Nueva : " W_NvaTasa    SKIP
          "Crèdito quedarà marcado como : " NomReest      SKIP
          "                    Està Segura(o) de Salvar la  Modificaciòn...?"
       VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirmar Refinanciaciòn"
       UPDATE W_RptaConf AS LOGICAL.
  
  IF NOT W_RptaConf THEN
     RETURN.
       
  FIND CURRENT Creditos NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
     MESSAGE "El Crèdito no puede ser actualizado, Està en Bloqueo..." SKIP
             "              Intente nuevamente."  VIEW-AS ALERT-BOX.
     APPLY "choose" TO Btn_Salir.
  END.
  
              
     IF INTEG(Creditos.Reestructurado:SCREEN-VALUE) EQ 1 THEN DO:
        ASSIGN Creditos.Fec_Reestructurado = W_Fecha
               Clientes.calificacion = integer(Clientes.califica:SCREEN-VALUE)
               clientes.reestructurado = integer(clientes.reestructurado) + 1.
     END.
  
  RUN Imprimir.  
            
  APPLY "choose" TO Btn_Salir.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Extras
&Scoped-define SELF-NAME Btn_AdExt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_AdExt Wwin
ON CHOOSE OF Btn_AdExt IN FRAME F_Extras /* Salvar Extra */
DO: 
  DEFI VAR W_NroDias AS INTEG FORM "99".
  DEFI VAR JJ        AS INTEG FORM "99999".
  DEFI VAR W_FecTra  AS DATE.
  DEFI VAR W_FecIni  AS DATE.

  W_NroDias = 30.
  IF Creditos.Per_Pago EQ 1 THEN
     W_NroDias = 7.
  ELSE IF Creditos.Per_Pago EQ 2 THEN
     W_NroDias = 10.
  IF Creditos.Per_Pago EQ 3 THEN
     W_NroDias = 15.

  IF W_PPExtra LE 0 OR W_PPExtra GT Creditos.Plazo THEN DO:
     MESSAGE "El Perìodo de Pago de la Cuota Extra debe ser Mayor que Cero y," SKIP
             "Menor o Igual al Plazo Total...No se Acepta la Operaciòn."
         VIEW-AS ALERT-BOX ERROR.
     RETURN.
  END.

  IF W_VrExtra LE 0 OR W_VrExtra GE Creditos.Sdo_Capital THEN DO:
     MESSAGE "El Valor de la Cuota Extra debe ser Mayor que Cero y," SKIP
             "Menor al Saldo de Capital...No se Acepta la Operaciòn."
         VIEW-AS ALERT-BOX ERROR.
     RETURN.
  END.

  W_TotExt = W_VrExtra.
  FOR EACH TExtras WHERE TExtras.Nit           EQ Creditos.Nit
                     AND TExtras.Num_Solicitud EQ Creditos.Num_Credito:
      IF TExtras.Estado EQ 1 AND TExtras.Nro_Cuota NE W_PPExtra THEN
         W_TotExt = W_TotExt + TExtras.Vr_CuoExtra.
      ELSE IF TExtras.Estado NE 1 THEN
         DELETE TExtras.
  END.

  IF W_TotExt GE Creditos.Sdo_Capital THEN DO:
     MESSAGE "El Valor Total de las Cuotas Extras + El valor de Esta Extra" SKIP
             "Debe ser Menor al Sdo-Capital...No se Acepta la Operaciòn."
         VIEW-AS ALERT-BOX ERROR.
     RETURN.
  END.

  FIND FIRST TExtras WHERE TExtras.Nit           EQ Creditos.Nit
                      AND  TExtras.Num_Solicitud EQ Creditos.Num_Credito
                      AND  TExtras.Nro_Cuota     EQ W_PPExtra NO-ERROR.
  IF NOT AVAIL(TExtras) THEN
     CREATE TExtras.

  ASSIGN W_FecIni = Creditos.Fec_Desem.

  IF Creditos.Fec_PagAnti NE ? AND Creditos.Fec_PagAnti GT Creditos.Fec_Desem THEN
     W_FecIni = Creditos.Fec_PagAnti.

  ASSIGN TExtras.Agencia       = Creditos.Agencia
         TExtras.Cod_Credito   = Creditos.Cod_Credito
         TExtras.Nit           = Creditos.Nit
         TExtras.Num_Solicitud = Creditos.Num_Credito
         TExtras.Nro_Cuota     = W_PPExtra
         TExtras.Vr_CuoExtra   = W_VrExtra
         TExtras.Estado        = 1
         W_TotExt              = 0
         W_FecTra              = W_FecIni.

  DO JJ = 1 TO W_PPExtra:
     RUN Halla_FecVcto.R (INPUT  W_FecIni,W_NroDias,W_FecTra,   
                          OUTPUT W_FecTra).
     TExtras.Fec_Vcto = W_FecTra.
  END.       

  FOR EACH Extras WHERE TExtras.Nit           EQ Creditos.Nit
                    AND TExtras.Num_Solicitud EQ Creditos.Num_Credito NO-LOCK:
      IF TExtras.Estado EQ 1 THEN DO:
         ASSIGN W_TotExt = W_TotExt + TExtras.Vr_CuoExtra
                W_TotExt:SCREEN-VALUE IN FRAME F_Cre = STRING(W_TotExt).
      
         FIND LAST PlanPagos WHERE PlanPagos.Agencia EQ Creditos.Agencia    
                        AND PlanPagos.Nit          EQ Creditos.Nit        
                        AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito
                        AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito
                        AND PlanPagos.Id_PdoMes    LE 2
                        AND PlanPagos.Nro_Cuota    EQ TExtras.Nro_Cuota NO-ERROR.
         ASSIGN PlanPagos.Cuota = Creditos.Cuota + TExtras.Vr_CuoExtra WHEN AVAIL(PlanPagos).
      END.
      ELSE DO:
         FIND LAST PlanPagos WHERE PlanPagos.Agencia EQ Creditos.Agencia    
                        AND PlanPagos.Nit          EQ Creditos.Nit        
                        AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito
                        AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito
                        AND PlanPagos.Id_PdoMes    LE 2
                        AND PlanPagos.Nro_Cuota    EQ TExtras.Nro_Cuota NO-ERROR.
         ASSIGN PlanPagos.Cuota = Creditos.Cuota WHEN AVAIL(PlanPagos).
      END.
  END.  

  CLOSE QUERY Br_Extras.
  OPEN  QUERY Br_Extras FOR EACH TExtras WHERE TExtras.Nit           EQ Creditos.Nit
                                         AND   TExtras.Num_Solicitud EQ Creditos.Num_Credito NO-LOCK 
                     BY TExtras.Nro_Cuota INDEXED-REPOSITION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_EliExt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_EliExt Wwin
ON CHOOSE OF Btn_EliExt IN FRAME F_Extras /* Eliminar Extra */
DO:
   FIND FIRST TExtras WHERE TExtras.Nit           EQ Creditos.Nit
                      AND   TExtras.Num_Solicitud EQ Creditos.Num_Credito
                      AND   TExtras.Nro_Cuota     EQ W_PPExtra NO-ERROR.
   IF AVAIL(TExtras) THEN DO:
      DELETE TExtras.
      FIND LAST PlanPagos WHERE PlanPagos.Agencia EQ Creditos.Agencia    
                        AND PlanPagos.Nit          EQ Creditos.Nit        
                        AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito
                        AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito
                        AND PlanPagos.Id_PdoMes    LE 2
                        AND PlanPagos.Nro_Cuota    EQ W_PPExtra NO-ERROR.
      ASSIGN PlanPagos.Cuota = Creditos.Cuota WHEN AVAIL(PlanPagos).
   END.

   ASSIGN W_TotExt = 0.

   FOR EACH TExtras WHERE TExtras.Nit           EQ Creditos.Nit
                    AND   TExtras.Num_Solicitud EQ Creditos.Num_Credito NO-LOCK:
      IF TExtras.Estado EQ 1 THEN
         ASSIGN W_TotExt = W_TotExt + TExtras.Vr_CuoExtra
                W_TotExt:SCREEN-VALUE IN FRAME F_Cre = STRING(W_TotExt).
   END.  

   CLOSE QUERY Br_Extras.
   OPEN  QUERY Br_Extras FOR EACH TExtras WHERE TExtras.Nit           EQ Creditos.Nit
                                          AND   TExtras.Num_Solicitud EQ Creditos.Num_Credito
               NO-LOCK BY TExtras.Nro_Cuota INDEXED-REPOSITION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Cre
&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir Wwin
ON CHOOSE OF Btn_Salir IN FRAME F_Cre /* Button 176 */
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


&Scoped-define FRAME-NAME F_Extras
&Scoped-define SELF-NAME BUTTON-170
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-170 Wwin
ON CHOOSE OF BUTTON-170 IN FRAME F_Extras /* Button 170 */
DO:

  FIND CURRENT planpagos NO-LOCK NO-ERROR.
  ASSIGN FRAME F_Extras:VISIBLE = FALSE
         FRAME F_Cre:SENSITIVE  = TRUE.

 /* APPLY "Choose" TO Btn_Salvar IN FRAME F_Creditos.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Cre
&Scoped-define SELF-NAME Rs_Op
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Rs_Op Wwin
ON MOUSE-SELECT-CLICK OF Rs_Op IN FRAME F_Cre
DO:

  

  

  
         
 
            W_Reest:SENSITIVE    = TRUE.
  
            Creditos.Reest:SENSITIVE    = TRUE.
          
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Rs_Op Wwin
ON VALUE-CHANGED OF Rs_Op IN FRAME F_Cre
DO:
  APPLY "Mouse-Select-Click" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_FecIni
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_FecIni Wwin
ON LEAVE OF W_FecIni IN FRAME F_Cre /* Nueva Fec-Inicio */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_NvoPlazo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_NvoPlazo Wwin
ON LEAVE OF W_NvoPlazo IN FRAME F_Cre /* Nuevo Plazo */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Extras
&Scoped-define SELF-NAME W_PPExtra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_PPExtra Wwin
ON LEAVE OF W_PPExtra IN FRAME F_Extras
DO:
  ASSIGN W_PPExtra.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Cre
&Scoped-define SELF-NAME W_Reest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Reest Wwin
ON MOUSE-SELECT-CLICK OF W_Reest IN FRAME F_Cre
DO:
  ASSIGN W_Reest.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Extras
&Scoped-define SELF-NAME W_VrExtra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_VrExtra Wwin
ON LEAVE OF W_VrExtra IN FRAME F_Extras
DO:
  ASSIGN W_VrExtra.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Cre
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Wwin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects Wwin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cambio_Plan Wwin 
PROCEDURE Cambio_Plan :
/*------------------------------------------------------------------------------
  Purpose: Modifica el planPagos por cambio de Pdo.de pago    
  Notes:   Nov.9/05 GAER.
------------------------------------------------------------------------------*/
                                                

END PROCE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Wwin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(Wwin)
  THEN DELETE WIDGET Wwin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Wwin  _DEFAULT-ENABLE
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
  DISPLAY Rs_Op W_PerPag W_TotExt W_CuoFaltan W_NvoPlazo W_SdoOtros W_NvaCuota 
          W_IntMora W_NvaTasa W_CuoFalNva W_Reest W_FecIni 
      WITH FRAME F_Cre IN WINDOW Wwin.
  IF AVAILABLE Clientes THEN 
    DISPLAY Clientes.Calificacion Clientes.Reestructurado 
      WITH FRAME F_Cre IN WINDOW Wwin.
  IF AVAILABLE Creditos THEN 
    DISPLAY Creditos.Fec_Desembolso Creditos.Fec_Pago Creditos.Sdo_Capital 
          Creditos.Fec_UltPago Creditos.Plazo Creditos.Cuota Creditos.Tasa 
          Creditos.Int_DifCobro Creditos.Int_Corrientes Creditos.Categoria 
          Creditos.Int_Anticipado Creditos.Reestructurado Creditos.Cod_Califica 
          Creditos.Sdo_Proyectado Creditos.Cuo_Pagadas Creditos.Val_Atraso 
          Creditos.Dias_Atraso Creditos.Cuo_Atraso Creditos.Provision 
          Creditos.Fec_Reestructurado 
      WITH FRAME F_Cre IN WINDOW Wwin.
  ENABLE RECT-296 RECT-320 RECT-323 RECT-326 RECT-328 RECT-329 RECT-330 
         RECT-331 RECT-332 Rs_Op Clientes.Calificacion W_FecIni 
         Btn_Acepta_Terminos Btn_Salir 
      WITH FRAME F_Cre IN WINDOW Wwin.
  {&OPEN-BROWSERS-IN-QUERY-F_Cre}
  DISPLAY W_PPExtra W_VrExtra 
      WITH FRAME F_Extras IN WINDOW Wwin.
  ENABLE Btn_AdExt W_PPExtra W_VrExtra Btn_EliExt BUTTON-170 Br_Extras 
      WITH FRAME F_Extras IN WINDOW Wwin.
  {&OPEN-BROWSERS-IN-QUERY-F_Extras}
  VIEW Wwin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject Wwin 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir Wwin 
PROCEDURE Imprimir :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR Listado  AS CHARACTER INITIAL "".
  Listado = W_PathSpl + W_Usuario + "Refinan.LST".
  {INCLUIDO\Imprimir.I "Listado"}    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject Wwin 
PROCEDURE initializeObject :
RUN SUPER.
  FIND Clientes WHERE Clientes.Nit EQ P_NitCli NO-LOCK NO-ERROR.

  ASSIGN Wwin:TITLE = "Modificaciòn-Refinanciación de Crèditos, Prog.W-ProRec_Refinancia.W.".
         FRAME F_Cre:TITLE = "Ced./Nit Asociado : " + P_NitCli + "   " +
                              TRIM(Clientes.Nombre) + " " + TRIM(Clientes.Apellido1) + " " +
                              TRIM(Clientes.Apellido2).

  IF P_Progra EQ 9 THEN 
     ASSIGN Wwin:TITLE = "Simulación Refinanciación, Prog.W-ProRec_Refinancia.W."
            Btn_Acepta_Terminos:VISIBLE IN FRAME F_Cre = FALSE.
            
           

  /*IF W_Agencia NE 10 THEN DO:
     Rs_Op:DISABLE("ReFinanciar -ReInicio-").
     Rs_Op:DISABLE("Clàusula Aceleratoria").
     Rs_Op:DISABLE("Modificar Condiciones").
     ASSIGN Btn_ModifVcto:VISIBLE = FALSE
            W_CapVdo:SENSITIVE    = FALSE
            W_IntVdo:SENSITIVE    = FALSE
            W_FecPP:SENSITIVE     = FALSE
            W_CuoPaga:SENSITIVE   = FALSE.
  END.*/

  FIND Creditos WHERE
       Creditos.Tip_Credito EQ P_TipCre AND
       Creditos.Cod_Credito EQ P_CodCre AND
       Creditos.Num_Credito EQ P_NumCre AND
       Creditos.Nit         EQ P_NitCli NO-LOCK NO-ERROR.
  IF AVAILABLE Creditos THEN DO:
     FRAME F_Cre:TITLE = FRAME F_Cre:TITLE + "  -   Nro_Credito : " + STRING(P_NumCre). 
     /* IF Creditos.Val_Atraso GT 0 THEN DO:
        MESSAGE "No se puede refinanciar un Crédito" SKIP
                "Que se encuentre atrasado." VIEW-AS ALERT-BOX INFORMATION.
        APPLY "choose" TO Btn_Salir IN FRAME F_Cre.
     END.*/
     
     FIND Pro_Creditos WHERE Pro_Creditos.Tip_Credito EQ Creditos.Tip_Credito AND
                             Pro_Creditos.Cod_Credito EQ Creditos.Cod_Credito NO-LOCK NO-ERROR.
     IF NOT AVAILABLE(Pro_Creditos) THEN DO:
        MESSAGE "El Producto de Créditos no se encuentra Disponible" SKIP
               "Hable con el Administrador!" VIEW-AS ALERT-BOX ERROR.
        APPLY "choose" TO Btn_Salir IN FRAME F_Cre.
     END.       
     
     RUN Mostrar_Credito.
  END.
  ELSE APPLY "CHHOSE" TO Btn_Salir.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Liquidar Wwin 
PROCEDURE Liquidar :
/*Halla Cuota o plazo */
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mes_Cont Wwin 
PROCEDURE Mes_Cont :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF MONTH(W_FecControl) EQ 4 OR MONTH(W_FecControl) EQ 6 OR MONTH(W_FecControl) EQ 9 
  OR MONTH(W_FecControl) EQ 11 THEN                                                            
     ASSIGN W_FecControl       = W_FecControl + 1                                              
            PlanPagos.Fec_Vcto = W_FecControl.                                                 
  ELSE IF MONTH(W_FecControl) EQ 3 THEN                                                        
     ASSIGN W_FecControl       = W_FecControl - 2                                              
            PlanPagos.Fec_Vcto = W_FecControl.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Modif_PlanPag Wwin 
PROCEDURE Modif_PlanPag :
/*------------------------------------------------------------------------------
  Purpose:       
  Notes:   Oct.11/06 GAER    
------------------------------------------------------------------------------*/
  DEFI VAR PdoSigue  AS INTEG FORM "9999".
  DEFI VAR NN        AS INTEG FORM "9999".
  DEFI VAR FecVcto   AS DATE.
  DEFI VAR FecCont   AS DATE.
  DEFI VAR FecIni    AS DATE.
  DEFI VAR W_NroDias AS INTEG FORM "9999".

  IF Creditos.Sistema EQ 2 THEN DO:
     ASSIGN W_NroDias = 30.
     IF      Creditos.Per_Pago EQ 1 THEN
            W_NroDias = 7.
     ELSE IF Creditos.Per_Pago EQ 2 THEN
            W_NroDias = 10.
     ELSE IF Creditos.Per_Pago EQ 3 THEN
            W_NroDias = 15.

     ASSIGN FecCont         = Creditos.Fec_Desemb
            FecIni          = Creditos.Fec_Desemb
            PlanPagos.Cuota = W_NvaCuota.

     IF Creditos.Fec_PagAnti NE ? AND Creditos.Fec_PagAnti GT Creditos.Fec_Desemb THEN
        ASSIGN FecCont = Creditos.Fec_PagAnti
               FecIni  = Creditos.Fec_PagAnti.
               
     RUN Halla_FecVcto.R (INPUT  FecIni,(W_NroDias * W_NvoPlazo),FecCont,   
                          OUTPUT FecCont).
     ASSIGN PlanPagos.Fec_Vcto     = FecCont
            PlanPagos.Fec_ProxPag  = FecCont
            Creditos.Fec_Pago      = FecCont
            Creditos.Sdo_Proyec    = Creditos.Monto            
            Creditos.Capital_Acum  = 0
            Creditos.Cuo_Pagadas   = 0
            PlanPagos.Cuo_Pagas    = 0
            PlanPagos.Capital_Acum = 0
            Creditos.Val_Atraso    = 0
            Creditos.Dias_Atraso   = 0
            Creditos.Cuo_Atraso    = 0.

     IF FecCont LE W_Fecha THEN
        ASSIGN Creditos.Sdo_Proyec    = 0
               Creditos.Capital_Acum  = Creditos.Monto
               PlanPagos.Capital_Acum = Creditos.Monto
               Creditos.Val_Atraso    = Creditos.Sdo_Capital
               Creditos.Dias_Atraso   = W_Fecha - FecCont + 1
               Creditos.Cuo_Atraso    = 1.

     FOR EACH PlanPagos WHERE PlanPagos.Agencia    EQ Creditos.Agencia    
                        AND PlanPagos.Nit          EQ Creditos.Nit        
                        AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito
                        AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito
                        AND PlanPagos.Id_PdoMes    EQ 0:   /*Solo Futuros sobran en Cuota Unica*/                        
         DELETE PlanPagos.
     END.

     RETURN.
  END.

  FIND LAST PlanPagos WHERE PlanPagos.Agencia      EQ Creditos.Agencia    
                        AND PlanPagos.Nit          EQ Creditos.Nit        
                        AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito
                        AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito
                        AND PlanPagos.Id_PdoMes    EQ 0   /*Solo Futuros*/
                        AND PlanPagos.Nro_Cuota    GT 0 NO-ERROR.
  IF AVAILABLE(PlanPagos) THEN DO:
     IF PlanPagos.Nro_Cuota EQ W_NvoPlazo THEN    /*Igual Pdos en el PlanPagos a creditos*/
        FOR EACH PlanPagos WHERE PlanPagos.Agencia EQ Creditos.Agencia    
                        AND PlanPagos.Nit          EQ Creditos.Nit        
                        AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito
                        AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito
                        AND PlanPagos.Id_PdoMes    EQ 0   /*Solo Futuros*/
                        AND PlanPagos.Nro_Cuota    GT 0:
            ASSIGN PlanPagos.Cuota = PlanPagos.Cuota - Creditos.Cuota + W_NvaCuota
                   PlanPagos.Tasa  = W_NvaTasa.

            IF PlanPagos.Cuota LT W_NvaCuota THEN
               PlanPagos.Cuota = W_NvaCuota.
     END. 
     ELSE IF PlanPagos.Nro_Cuota GT W_NvoPlazo THEN   /*Sobran Pdos en el PlanPagos*/
        FOR EACH PlanPagos WHERE PlanPagos.Agencia EQ Creditos.Agencia    
                        AND PlanPagos.Nit          EQ Creditos.Nit        
                        AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito
                        AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito
                        AND PlanPagos.Id_PdoMes    EQ 0   /*Solo Futuros*/
                        AND PlanPagos.Nro_Cuota    GT 0:
            IF PlanPagos.Nro_Cuota GT W_NvoPlazo THEN
               DELETE PlanPagos.
            ELSE DO:
               ASSIGN PlanPagos.Cuota = PlanPagos.Cuota - Creditos.Cuota + W_NvaCuota
                      PlanPagos.Plazo = W_NvoPlazo
                      PlanPagos.Tasa  = W_NvaTasa.

               IF PlanPagos.Cuota LT W_NvaCuota THEN
                  PlanPagos.Cuota = W_NvaCuota.
            END.
     END. 
     ELSE DO:  /*Faltan Pdos en el PlanPagos*/
        FOR EACH PlanPagos WHERE PlanPagos.Agencia EQ Creditos.Agencia    
                        AND PlanPagos.Nit          EQ Creditos.Nit        
                        AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito
                        AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito
                        AND PlanPagos.Id_PdoMes    EQ 0   /*Solo Futuros*/
                        AND PlanPagos.Nro_Cuota    GT 0:
            ASSIGN PlanPagos.Cuota = PlanPagos.Cuota - Creditos.Cuota + W_NvaCuota
                   PlanPagos.Plazo = W_NvoPlazo
                   PlanPagos.Tasa  = W_NvaTasa
                   PdoSigue        = PlanPagos.Nro_Cuota
                   FecVcto         = PlanPagos.Fec_Vcto
                   FecCont         = PlanPagos.Fec_Vcto.                   

            IF PlanPagos.Cuota LT W_NvaCuota THEN
               PlanPagos.Cuota = W_NvaCuota.

            IF NOT AVAIL(CPlanP) THEN
               CREATE CPlanP.

            BUFFER-COPY PlanPagos TO CPlanP.
        END.

        ASSIGN W_NroDias = 30.
        IF      Creditos.Per_Pago EQ 1 THEN
                W_NroDias = 7.
        ELSE IF Creditos.Per_Pago EQ 2 THEN
                W_NroDias = 10.
        ELSE IF Creditos.Per_Pago EQ 3 THEN
                W_NroDias = 15.

        DO NN = PdoSigue + 1 TO W_NvoPlazo:
           CREATE PlanPagos.
           BUFFER-COPY CPlanP TO PlanPagos.

           ASSIGN PlanPagos.Nro_Cuota = NN
                  PlanPagos.Fec_Ini   = FecCont
                  PlanPagos.Cuota     = W_NvaCuota.

           RUN Halla_FecVcto.R (INPUT  FecVcto,W_NroDias,FecCont,   
                                OUTPUT FecCont).
           PlanPagos.Fec_Vcto = FecCont.                  
        END.

        FOR EACH CPlanP: DELETE CPlanP. END.
     END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Credito Wwin 
PROCEDURE Mostrar_Credito :
DO WITH FRAME F_Cre:
   ASSIGN Creditos.Fec_Desembolso:SCREEN-VALUE = STRING(Creditos.Fec_Desembolso)
          Creditos.Fec_Pago:SCREEN-VALUE = STRING(Creditos.Fec_Pago)
          Creditos.Fec_UltPago:SCREEN-VALUE = STRING(Creditos.Fec_UltPago)
          Creditos.INT_DifCobro:SCREEN-VALUE = STRING(Creditos.INT_DifCobro)
          Creditos.Sdo_Proyectado:SCREEN-VALUE = STRING(Creditos.Sdo_Proyectado)
          Creditos.Cuo_Pagadas:SCREEN-VALUE = STRING(Creditos.Cuo_Pagadas)
          Creditos.Val_Atraso:SCREEN-VALUE = STRING(Creditos.Val_Atraso)
          Creditos.Dias_Atraso:SCREEN-VALUE = STRING(Creditos.Dias_Atraso)
          Creditos.Cuo_Atraso:SCREEN-VALUE = STRING(Creditos.Cuo_Atraso)
          Creditos.Provision:SCREEN-VALUE = STRING(Creditos.Provision)
          Creditos.Fec_Reestructurado:SCREEN-VALUE = STRING(Creditos.Fec_Reestructurado,"99/99/99")
          Creditos.Plazo:SCREEN-VALUE = STRING(Creditos.Plazo)
          Creditos.Tasa:SCREEN-VALUE = STRING(Creditos.Tasa)
          Creditos.Sdo_Capital:SCREEN-VALUE = STRING(Creditos.Sdo_Capital)
          Creditos.Cuota:SCREEN-VALUE = STRING(Creditos.Cuota)
          Creditos.Reestructurado:SCREEN-VALUE = STRING(Creditos.Reestructurado)
          Creditos.INT_Corrientes:SCREEN-VALUE = STRING(Creditos.INT_Corrientes)
          Creditos.INT_Anticipado:SCREEN-VALUE = STRING(Creditos.INT_Anticipado).

   ASSIGN W_NvaCuota:SCREEN-VALUE = STRING(Creditos.Cuota)
          W_NvoPlazo:SCREEN-VALUE = STRING(Creditos.Plazo)
          W_INTMora:SCREEN-VALUE   = STRING(Creditos.INT_MorCobrar + Creditos.INT_MoraDifCob)
          W_SdoOtros:SCREEN-VALUE  = STRING(Creditos.Costas + Creditos.Honorarios + Creditos.Polizas)
          W_CuoFaltan:SCREEN-VALUE = STRING(Creditos.Plazo - Creditos.Cuo_Pagadas)
          W_CuoFalNva:SCREEN-VALUE = STRING(Creditos.Plazo - Creditos.Cuo_Pagadas)
          W_NvaTasa:SCREEN-VALUE   = STRING(Creditos.Tasa)
          W_FecIni:SCREEN-VALUE       = STRING(Creditos.Fec_PagAnti)
          W_FecIni
          W_NvaCuota
          W_NvoPlazo.

   IF Creditos.Sdo_Proyectado LE 0 THEN
      Creditos.Sdo_Proyectado:SCREEN-VALUE = "0".

   IF Creditos.Val_Atraso GT Creditos.Sdo_Capital THEN
      Creditos.Val_Atraso:SCREEN-VALUE = STRING(Creditos.Sdo_Capital).

  FOR EACH TExtras: DELETE TExtras. END.
   ASSIGN W_TotExt = 0
          W_TotExt:SCREEN-VALUE = "0".

   FOR EACH PlanPagos WHERE PlanPagos.Agencia      EQ Creditos.Agencia    
                        AND PlanPagos.Nit          EQ Creditos.Nit        
                        AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito
                        AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito
                        AND PlanPagos.Id_PdoMes    EQ 0 NO-LOCK:
       IF PlanPagos.Cuota GT Creditos.Cuota THEN DO:
          CREATE TExtras.
          ASSIGN TExtras.Agencia       = Creditos.Agencia     
                 TExtras.Cod_Credito   = Creditos.Cod_Credito 
                 TExtras.Nit           = Creditos.Nit         
                 TExtras.Num_Solicitud = Creditos.Num_Credito 
                 TExtras.Nro_Cuota     = PlanPagos.Nro_Cuota            
                 TExtras.Vr_CuoExtra   = PlanPagos.Cuota - Creditos.Cuota  
                 TExtras.Fec_Vcto      = PlanPagos.Fec_Vcto
                 TExtras.Estado        = 1                    
                 W_TotExt              = W_TotExt + TExtras.Vr_CuoExtra                    
                 W_TotExt:SCREEN-VALUE = STRING(W_TotExt).             
       END.
   END.

   OPEN  QUERY Br_Extras FOR EACH TExtras WHERE TExtras.Nit           EQ Creditos.Nit
                                          AND   TExtras.Num_Solicitud EQ Creditos.Num_Credito
                        NO-LOCK BY TExtras.Nro_Cuota INDEXED-REPOSITION.
          
   FIND LAST PlanPagos WHERE PlanPagos.Agencia     EQ Creditos.Agencia    
                        AND PlanPagos.Nit          EQ Creditos.Nit        
                        AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito
                        AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito
                        AND PlanPagos.Id_PdoMes    EQ 1 NO-LOCK NO-ERROR.
   IF NOT AVAILABLE(PlanPagos) THEN DO:
      MESSAGE "La Tabla PlanPagos para el Pdo-Actual No Existe..." SKIP
              "                                     Refinanciaciòn cancelada..."
              VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
   END.  
   
   CASE Creditos.Per_Pago:
     WHEN 1 THEN 
          ASSIGN W_PerPag:SCREEN-VALUE  = "Semanas"
                 W_PdoPag               = 7
                 W_PdoAno               = 52.
     WHEN 2 THEN 
          ASSIGN W_PerPag:SCREEN-VALUE  = "Dècadas"
                 W_PdoPag               = 10
                 W_PdoAno               = 36.
     WHEN 3 THEN 
          ASSIGN W_PerPag:SCREEN-VALUE  = "Quincenas"
                 W_PdoPag               = 15
                 W_PdoAno               = 24.
     WHEN 4 THEN 
          ASSIGN W_PerPag:SCREEN-VALUE  = "Meses"
                 W_PdoPag               = 30
                 W_PdoAno               = 12.
   END CASE.     
   
   ASSIGN W_CuoExtra = PlanPagos.Cuota - Creditos.Cuota
          Pagas      = Creditos.Cuo_Pagadas
          FecPP      = Creditos.Fec_Pago.
   
  /* RUN Liquidar.*/
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mov_Credito Wwin 
PROCEDURE Mov_Credito :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    CREATE Mov_Creditos.
    ASSIGN Mov_Creditos.Agencia       = Creditos.Agencia
           Mov_Creditos.Cod_Credito   = Creditos.Cod_Credito
           Mov_Creditos.Nit           = Creditos.Nit
           Mov_Creditos.Num_Credito   = Creditos.Num_Credito
           Mov_Creditos.Ofi_Destino   = Creditos.agencia
           Mov_Creditos.Ofi_Fuente    = W_Agencia
           Mov_Creditos.Pagare        = Creditos.Pagare
           Mov_Creditos.Fecha         = W_Fecha
           Mov_Creditos.Hora          = vTime
           Mov_Creditos.Num_Documento = STRING(W_Fecha,"99999999")           
           Mov_Creditos.Usuario       = W_Usuario
           Mov_Creditos.Int_Corriente = Creditos.Int_Corriente
           Mov_Creditos.Int_MorCobrar = Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob
           Mov_Creditos.Sdo_Capital   = Creditos.Sdo_Capital
           Mov_Creditos.Val_Efectivo  = 0
           Mov_Creditos.Cpte          = 4
           Mov_Creditos.Cod_Operacion = 030303001
           Mov_Creditos.Descrip       = "X Refin,Cuota y Plazo :" + STRING(Creditos.Cuota,">>>>>>>,>>9") + " " +
                                        STRING(Creditos.Plazo,">>>9").
                                          
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir Wwin 
PROCEDURE ProcesoImprimir :
{Incluido\RepEncabezado.i}
    DEFINE VAR W_Cliente AS CHARACTER FORMAT "X(60)".
    DEFINE VAR T_Plazo   AS CHARACTER FORMAT "X(30)".
    DEFINE VAR T_Dedu    AS CHARACTER FORMAT "X(30)".
    FIND Clientes WHERE Clientes.Nit EQ Creditos.Nit NO-LOCK NO-ERROR.
    ASSIGN W_Cliente = Creditos.Nit + " - " + Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
 
    W_EncColumna = "Cliente Solicitante         :   " + W_Cliente.
    W_Reporte   = "REESTRUCTURACION   : CREDITO - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
    
 VIEW FRAME F-Encabezado.
 VIEW FRAME F-Ftr.

 DO WITH FRAME F_Cre:
   CASE Creditos.Per_Pago:
     WHEN 1 THEN T_Plazo = Creditos.Plazo:SCREEN-VALUE  + " Semanas".
     WHEN 2 THEN T_Plazo = Creditos.Plazo:SCREEN-VALUE  + " Dècadas".
     WHEN 3 THEN T_Plazo = Creditos.Plazo:SCREEN-VALUE  + " Quincenas".
     WHEN 4 THEN T_Plazo = Creditos.Plazo:SCREEN-VALUE  + " Meses".
   END CASE.
   
   DISPLAY 
     "=============================================DATOS GENERALES DEL CREDITO=================================================" AT 1
     "Agencia de Radicación       : " AT 1
     Creditos.Agencia
     "Número del Crédito          : " AT 65
     Creditos.Num_Credito             AT 98
     "Número de Solicitud         : " AT 1
     Creditos.Num_Solicitud           AT 33
     "Fecha de Aprobación         : " AT 65
     Creditos.Fec_Aprobacion          AT 98
     "Producto de Crédito         : " AT 1
     Pro_Creditos.Nom_Producto        AT 33 
     "Tipo de Producto            : " AT 65
     Pro_Creditos.Tip_Credito         AT 98 
     "Instancia Actual            : " AT 1
     P_NomIns                         AT 33 
     "Usuario Actualmente Procesa : " AT 65
     W_Usuario                        AT 98   
     "Forma de Pago de la Cuota   : " AT 1
     Creditos.FOR_Pago                AT 33 
     "=============================================DETALLE DE VALORES DEL CREDITO==============================================" AT 1
     "Monto Inicial del Credito   : " AT 1
     Creditos.Val_Desemb              AT 33 
     "Tasa Anterior               : " AT 65
     Creditos.Tasa:SCREEN-VALUE IN FRAME F_Cre AT 98 SKIP(1)
     WITH FRAME F_Sol WIDTH 150 NO-BOX USE-TEXT NO-LABELS STREAM-IO.
   
   DISPLAY "CAMBIOS POR MODIFICACION" AT 1
     "Saldo Capital               : " AT 1
     Creditos.Sdo_Capital             AT 33  FORMAT ">>>,>>>,>>9"     
     "Tasa Nominal Anual          : " AT 65
     Creditos.Tasa                    AT 98
     "Plazo Anterior              : " AT 1
     T_Plazo                          AT 33
     "Nuevo Plazo                 : " AT 65
     W_NvoPlazo                       AT 98
     "Cuota Anterior              : " AT 1
     Creditos.Cuota:SCREEN-VALUE      AT 33
     "Nueva Cuota                 : " AT 65
     W_NvaCuota                       AT 98
    
   WITH FRAME F_Sol2 WIDTH 150 NO-BOX USE-TEXT NO-LABELS STREAM-IO.
   
 END.
 PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proyeccion Wwin 
PROCEDURE Proyeccion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*RUN Proyeccion_Credito.R (INPUT DECIMAL(Solicitud.TOTAL_Prestamo:SCREEN-VALUE IN FRAME F_Solicitud),
                INPUT DECIMAL(Solicitud.Plazo:SCREEN-VALUE IN FRAME F_Solicitud),
                INPUT DECIMAL(Solicitud.Cuota:SCREEN-VALUE IN FRAME F_Solicitud),
                INPUT 0, /*total extras que ya no se utiliza*/
                INPUT TODAY, /*no se que fecha se pone o se pide*/
                INPUT DECIMAL(W_TasaPeriodo:SCREEN-VALUE IN FRAME F_Solicitud) / 100,
                INPUT DECIMAL(Solicitud.Incremento:SCREEN-VALUE IN FRAME F_Solicitud),
                INPUT 0, /*gracia*/
                INPUT INTEGER(SUBSTRING(Cmb_PerPago:SCREEN-VALUE IN FRAME F_Solicitud,1,1)),
                INPUT INTEGER(Solicitud.FOR_Interes:SCREEN-VALUE IN FRAME F_Solicitud),
                INPUT INTEGER(SUBSTRING(Cmb_Sistemas:SCREEN-VALUE IN FRAME F_Solicitud,1,5)),
                INPUT Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud,
                INPUT NomNit:SCREEN-VALUE IN FRAME F_Solicitud,
                INPUT INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Producto,1,3)),   
                INPUT SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Producto,7,30),
                INPUT "S",
                INPUT DECIMAL(Solicitud.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud),
                INPUT SUBSTRING(Cmb_Sistemas:SCREEN-VALUE IN FRAME F_Solicitud,9,30),
                INPUT SUBSTRING(Cmb_PerPago:SCREEN-VALUE IN FRAME F_Solicitud,5,15),
                INPUT 1,Solicitud.Monto).                          */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Refinanc Wwin 
PROCEDURE Refinanc :
/*------------------------------------------------------------------------------
  Purpose:  Actualiza nuevas condiciones en Crèditos y en PlanPagos.
            (Opciòn Rebaja Plazo y/o Cuota).
------------------------------------------------------------------------------*/
  ASSIGN PlanPagos.Cuota = PlanPagos.Cuota - Creditos.Cuota + W_NvaCuota /*La que transcurre*/
         PlanPagos.Plazo = W_NvoPlazo
         Creditos.Cuota  = W_NvaCuota
         Creditos.Plazo  = W_NvoPlazo.
         
  IF Rs_Op EQ 1 THEN DO:  /*Rebajò el Plazo*/
     FOR EACH PlanPagos WHERE PlanPagos.Agencia    EQ Creditos.Agencia    
                        AND PlanPagos.Nit          EQ Creditos.Nit        
                        AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito
                        AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito
                        AND PlanPagos.Id_PdoMes    EQ 0
                        AND PlanPagos.Nro_Cuota    GT W_NvoPlazo:   /*Futuros que Sobran*/
         DELETE PlanPagos.
     END.
  
     FOR EACH PlanPagos WHERE PlanPagos.Agencia    EQ Creditos.Agencia    
                        AND PlanPagos.Nit          EQ Creditos.Nit        
                        AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito
                        AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito
                        AND PlanPagos.Id_PdoMes    EQ 0:           /*Futuros que Faltan*/      
         ASSIGN PlanPagos.Plazo = W_NvoPlazo.                                                       
     END.  
  END.
  ELSE DO:    /*Rebajò Cuota, Igual Plazo*/
     FOR EACH PlanPagos WHERE PlanPagos.Agencia    EQ Creditos.Agencia    
                        AND PlanPagos.Nit          EQ Creditos.Nit        
                        AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito
                        AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito
                        AND PlanPagos.Id_PdoMes    EQ 0:           /*Futuros que Faltan*/      
         ASSIGN PlanPagos.Cuota = W_NvaCuota.                                                       
     END.  
  END.
         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReInicio Wwin 
PROCEDURE ReInicio :
/*------------------------------------------------------------------------------
  Purpose:  El crèdito se reinicia con las nuevas condiciones, y en PlanPagos.
 ------------------------------------------------------------------------------*/
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TotalMoroso Wwin 
PROCEDURE TotalMoroso :
/*------------------------------------------------------------------------------
  Purpose:  Aplica Clàusula aceleratoria de pago (Total Sdo_Capital) en mora,
            y en PlanPagos. 
------------------------------------------------------------------------------*/
  ASSIGN Creditos.Sdo_proyectado = 0
         Creditos.Capital_Acum   = Creditos.Monto
         Creditos.Cuo_Atraso     = Creditos.Plazo - Creditos.Cuo_Pagadas 
         Creditos.Dias_Atraso    = (W_Fecha + 1)  - Creditos.Fec_Pago
         Creditos.Val_Atraso     = Creditos.Sdo_capital.
         
  ASSIGN PlanPagos.Capital_Pdo  = PlanPagos.Capital_Pdo + Creditos.Sdo_capital
         PlanPagos.Capital_Acum = Creditos.Monto.
         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

