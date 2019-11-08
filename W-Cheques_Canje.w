&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME

/* Connected Databases 
          bdcentral        PROGRESS
*/

&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{Incluido/Variable.I "SHARED"}
DEFINE VAR Cbte AS INTEGER.
DEFINE VAR Docto AS INTEGER.
DEFINE VAR CtaDb AS CHARACTER.
DEFINE VAR CtaCr AS CHARACTER.

   /* oakley */

   DEFINE VAR P_Per      AS   INTEGER.
   DEFINE VAR P_Valor    AS   INTEGER.
   DEFINE VAR P_Gracia   AS   INTEGER.
   DEFINE VAR T_Estado   LIKE Che_Transito.Estado.
   DEFINE VAR T_OfiIni   LIKE Che_Transito.Agencia.
   DEFINE VAR T_OfiFin   LIKE Che_Transito.Agencia.
   DEFINE VAR T_BcoIni   LIKE Che_Transito.Cod_Compensa.
   DEFINE VAR T_BcoFin   LIKE Che_Transito.Cod_Compensa.
   DEFINE VAR T_PtoIni   LIKE Che_Transito.Tip_Producto.
   DEFINE VAR T_PtoFin   LIKE Che_Transito.Tip_Producto.
   DEFINE VAR T_FecIni   LIKE Che_Transito.Fec_Canje.
   DEFINE VAR T_FecFin   LIKE Che_Transito.Fec_Canje.
   DEFINE VAR T_CtaIni   LIKE Che_Transito.Num_Cuenta.
   DEFINE VAR T_CtaFin   LIKE Che_Transito.Num_Cuenta.
   DEFINE VAR W_CtaBco   LIKE Cuentas.Cuenta.
   DEFINE VAR W_NomBco   LIKE Cuentas.Nombre.
   DEFINE VAR W_Canje    LIKE Bancos.Dia_Canje INITIAL 99.
   DEFINE VAR W_Int      AS   DECIMAL DECIMALS 0.
   DEFINE VAR W_ValCanje AS   DECIMAL DECIMALS 0.
   DEFINE VAR W_Error    AS   LOGICAL.
   DEFINE VAR W_Habil    AS   INTEGER.
   DEFINE VAR W_Pl       AS   INTEGER.
   DEFINE VAR W_RPTA     AS   LOGICAL.
   DEFINE VAR T_Valor    LIKE Creditos.Sdo_Capital INITIAL 0.
   DEFINE VAR T_Difer    LIKE Creditos.Sdo_Capital INITIAL 0.
   DEFINE VAR T_Inter    LIKE Creditos.Sdo_Capital INITIAL 0.
   DEFI VAR P_Nit LIKE Clientes.Nit. 
   DEFI VAR W_SiContab AS LOG INIT FALSE.
   DEFI VAR W_Inicio   AS LOG INIT FALSE.
   
   DEFINE VAR LiqIntDB   LIKE Cuentas.Cuenta. 
   DEFINE VAR LiqIntCR   LIKE Cuentas.Cuenta.
   DEFINE VAR CtaIntAnt  LIKE Cuentas.Cuenta.
   DEFINE VAR CtaLiqMor  LIKE Cuentas.Cuenta.
   DEFINE VAR CtaLiqDco  LIKE Cuentas.Cuenta.
   DEFINE VAR CtaAuxLiq  LIKE Cuentas.Cuenta.
   DEFINE VAR Int_Mor    LIKE Ahorros.Sdo_Disponible.
   DEFINE VAR Int_Dco    LIKE Ahorros.Sdo_Disponible.
   DEFINE VAR Int_Cor    LIKE Ahorros.Sdo_Disponible.
   DEFINE VAR Int_Ant    LIKE Ahorros.Sdo_Disponible.
   DEFINE VAR Capital    LIKE Ahorros.Sdo_Disponible.
   DEFINE VAR W_Devuel   LIKE Ahorros.Sdo_Disponible.
   
   DEFINE VAR P_Cuenta LIKE Cuentas.Cuenta.
   DEFINE VAR P_Nombre LIKE Cuentas.Nombre.
   DEFINE VAR P_NatTra LIKE Cuentas.Naturaleza.
   DEFINE VAR P_CtrNat LIKE Cuentas.Ctr_Naturaleza.
   DEFINE VAR P_TipCta AS   CHARACTER FORMAT "X".
   
/* Cuentas Para la Contabilización */
   
   DEFINE VAR AuxCta     LIKE Cuentas.Cuenta.
   DEFINE VAR CtaSyA     LIKE Cuentas.Cuenta.
   DEFINE VAR CtaCble    LIKE Cuentas.Cuenta.
   DEFINE VAR W_LiqIntDb LIKE Cuentas.Cuenta.
   DEFINE VAR W_LiqIntCr LIKE Cuentas.Cuenta.
   DEFINE VAR W_LiqMor   LIKE Cuentas.Cuenta.
   DEFINE VAR W_DifCdb   LIKE Cuentas.Cuenta.
   DEFINE VAR W_DifCcr   LIKE Cuentas.Cuenta.
   DEFINE VAR Cta_Caja   LIKE Cuentas.Cuenta.
   DEFINE VAR Cta_Banco  LIKE Cuentas.Cuenta.
   
   DEFINE VAR T_Banco  LIKE Che_Transito.Cod_Compensa.
   DEFINE VAR T_Cheque LIKE Che_Transito.Cheque.
   DEFINE VAR I        AS   INTEGER.
   DEFINE VAR J        AS   INTEGER.
   
   DEFINE TEMP-TABLE Aux_Taquilla LIKE Taquilla.
   
   DEFINE TEMP-TABLE Tmp_ConTaq
     FIELD T_Agencia   LIKE Agencias.Agencia
     FIELD T_Operacion LIKE Taquilla.Cod_Operacion
     FIELD T_Cuenta    LIKE Cuentas.Cuenta
     FIELD T_Valor     LIKE Mov_Contable.Db
     FIELD T_Nat       LIKE Taquilla.Naturaleza
   INDEX Idx_ConTaq IS PRIMARY T_Operacion T_Cuenta ASCENDING.
   
   DEFINE BUFFER Tmp_Ahorros    FOR Ahorros.
   DEFINE BUFFER Tmp_ProAhorros FOR Pro_Ahorros.
   DEFINE VAR P_CodComp LIKE Bancos.Cod_Compensa.
   DEFINE VAR P_NomComp  LIKE Bancos.Nombre.

DEFINE VAR vTime AS INTEGER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME Brw_Consulta

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Che_Transito

/* Definitions for BROWSE Brw_Consulta                                  */
&Scoped-define FIELDS-IN-QUERY-Brw_Consulta Che_Transito.Agencia ~
Che_Transito.Cod_Compensa Che_Transito.Cheque Che_Transito.Num_Cuenta ~
Che_Transito.Tip_Producto Che_Transito.Cod_Producto Che_Transito.Fec_Canje ~
Che_Transito.Fec_Confirmacion Che_Transito.Valor Che_Transito.Estado 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Brw_Consulta 
&Scoped-define QUERY-STRING-Brw_Consulta FOR EACH Che_Transito NO-LOCK
&Scoped-define OPEN-QUERY-Brw_Consulta OPEN QUERY Brw_Consulta FOR EACH Che_Transito NO-LOCK.
&Scoped-define TABLES-IN-QUERY-Brw_Consulta Che_Transito
&Scoped-define FIRST-TABLE-IN-QUERY-Brw_Consulta Che_Transito


/* Definitions for BROWSE Brw_Proceso                                   */
&Scoped-define FIELDS-IN-QUERY-Brw_Proceso Che_Transito.Agencia Che_Transito.Ofi_Destino Che_Transito.Cod_Compensa Che_Transito.Cheque Che_Transito.Num_Cuenta Che_Transito.Tip_Producto Che_Transito.Cod_Producto Che_Transito.Fec_Canje Che_Transito.Fec_Confirmacion Che_Transito.Valor Che_Transito.Estado   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Brw_Proceso   
&Scoped-define SELF-NAME Brw_Proceso
&Scoped-define OPEN-QUERY-Brw_Proceso /*OPEN QUERY {&SELF-NAME} FOR EACH Che_Transito NO-LOCK.*/.
&Scoped-define TABLES-IN-QUERY-Brw_Proceso Che_Transito
&Scoped-define FIRST-TABLE-IN-QUERY-Brw_Proceso Che_Transito


/* Definitions for FRAME F-Main                                         */

/* Definitions for FRAME F_Ingresar                                     */
&Scoped-define SELF-NAME F_Ingresar
&Scoped-define OPEN-QUERY-F_Ingresar /*OPEN QUERY {&SELF-NAME} FOR EACH Che_Transito SHARE-LOCK.*/.
&Scoped-define TABLES-IN-QUERY-F_Ingresar Che_Transito
&Scoped-define FIRST-TABLE-IN-QUERY-F_Ingresar Che_Transito


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Brw_Proceso Brw_Consulta Btn_Ingresar ~
BUTTON-117 Com_Agencia Com_Consulta F-CtaCble Rad_Estado Btn_Liberar ~
Btn_Devolver Btn_Salir Btn_Imprimir Btn_Ayuda RECT-116 RECT-117 RECT-281 
&Scoped-Define DISPLAYED-OBJECTS F_Mensage Com_Agencia Com_Consulta ~
F-CtaCble F_RegProcesados Rad_Estado Rad_Producto F_BcoIni F_BcoFin ~
F_CueIni F_CtaFin F_FecIni F_FecFin NomCta F-CtaSyADev NomCta2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Ayuda 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "Ayuda" 
     SIZE 4 BY 1.

DEFINE BUTTON Btn_Devolver 
     LABEL "Devolver" 
     SIZE 12 BY 1.62.

DEFINE BUTTON Btn_Imprimir 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "&Imprimir" 
     SIZE 12 BY 1.62.

DEFINE BUTTON Btn_Ingresar 
     LABEL "Ingresar" 
     SIZE 12 BY 1.62.

DEFINE BUTTON Btn_Liberar 
     LABEL "Liberar" 
     SIZE 12 BY 1.62.

DEFINE BUTTON Btn_Salir DEFAULT 
     LABEL "&Salir" 
     SIZE 12 BY 1.62
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-117 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 117" 
     SIZE 12 BY 1.62.

DEFINE VARIABLE Com_Agencia AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 31 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Com_Consulta AS CHARACTER FORMAT "X(15)":U INITIAL "Todos" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Todos","Por Producto","Por Cuenta","Por Fecha","Por Banco" 
     DROP-DOWN-LIST
     SIZE 21 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-CtaCble AS CHARACTER FORMAT "X(14)":U 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-CtaSyADev AS CHARACTER FORMAT "X(14)":U 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F_BcoFin AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Bco.Final" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F_BcoIni AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Bco.Inicial" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F_CtaFin AS CHARACTER FORMAT "X(14)":U 
     LABEL "Cta.Final" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F_CueIni AS CHARACTER FORMAT "X(14)":U 
     LABEL "Cta.Inicial" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F_FecFin AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Final" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F_FecIni AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Inicial" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F_Mensage AS CHARACTER FORMAT "X(80)":U 
     VIEW-AS FILL-IN 
     SIZE 94 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F_RegProcesados AS INTEGER FORMAT ">>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 19 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomCta AS CHARACTER FORMAT "X(40)":U INITIAL "0" 
     VIEW-AS FILL-IN 
     SIZE 36.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomCta2 AS CHARACTER FORMAT "X(40)":U INITIAL "0" 
     VIEW-AS FILL-IN 
     SIZE 36.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Rad_Estado AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "En Canje", 1,
"Liberados", 2,
"Devueltos", 3,
"Girados", 4
     SIZE 15 BY 4.04 NO-UNDO.

DEFINE VARIABLE Rad_Producto AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Ctas Contables", "0",
"Ahorros", "1",
"Créditos", "2",
"P.Especiales", "4"
     SIZE 53 BY .58 NO-UNDO.

DEFINE RECTANGLE RECT-116
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 53.43 BY 4.85.

DEFINE RECTANGLE RECT-117
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 18 BY 4.85.

DEFINE RECTANGLE RECT-281
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 95 BY 2.15.

DEFINE BUTTON Btn_Cancelar 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Cancelar" 
     SIZE 12 BY 1.88.

DEFINE BUTTON Btn_Ejecutar 
     LABEL "Procesar" 
     SIZE 12 BY 1.88.

DEFINE VARIABLE Rad_Seleccion AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Cheques del Día ", 1,
"Cheques Seleccionados", 2
     SIZE 26 BY 2.15 NO-UNDO.

DEFINE BUTTON Btn_Can 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Cancelar" 
     SIZE 12 BY 1.88.

DEFINE BUTTON Btn_Imp 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Imprimir" 
     SIZE 12 BY 1.88.

DEFINE VARIABLE Fill_FecFin AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Fill_FecIni AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Rad_Imprimir AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Todos", 0,
"Cheques En Canje", 1,
"Cheques Liberados ", 2,
"Cheques Devueltos", 3,
"Cheques Girados", 4
     SIZE 22 BY 3.77 NO-UNDO.

DEFINE BUTTON Btn_Salvar 
     LABEL "Salvar" 
     SIZE 11 BY 1.62.

DEFINE BUTTON BUTTON-120 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 120" 
     SIZE 11 BY 1.62.

DEFINE VARIABLE Cmb_AgenciasCanje AS CHARACTER FORMAT "X(256)":U 
     LABEL "Agencias" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 41 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-282
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 20 BY 3.23.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Brw_Consulta FOR 
      Che_Transito SCROLLING.

DEFINE QUERY Brw_Proceso FOR 
      Che_Transito SCROLLING.

DEFINE QUERY F_Ingresar FOR 
      Che_Transito SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Brw_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Brw_Consulta W-Win _STRUCTURED
  QUERY Brw_Consulta DISPLAY
      Che_Transito.Agencia COLUMN-LABEL "Age" FORMAT "999":U WIDTH 4.43
      Che_Transito.Cod_Compensa COLUMN-LABEL "Bco" FORMAT "99":U
      Che_Transito.Cheque COLUMN-LABEL "Nro.Cheque" FORMAT "X(10)":U
            WIDTH 10.43
      Che_Transito.Num_Cuenta COLUMN-LABEL "Nro. Cuenta" FORMAT "X(14)":U
            WIDTH 10.43
      Che_Transito.Tip_Producto COLUMN-LABEL "Pcto" FORMAT "X(1)":U
            WIDTH 4.86
      Che_Transito.Cod_Producto COLUMN-LABEL "Cod.Pto" FORMAT "999":U
            WIDTH 8.43
      Che_Transito.Fec_Canje COLUMN-LABEL "Fec.Canje" FORMAT "99/99/9999":U
      Che_Transito.Fec_Confirmacion COLUMN-LABEL "Fec.Confirma" FORMAT "99/99/9999":U
            WIDTH 11.86
      Che_Transito.Valor COLUMN-LABEL "Valor" FORMAT "->>>>>,>>>,>>>,>>9.99":U
      Che_Transito.Estado COLUMN-LABEL "E" FORMAT "9":U WIDTH 3
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 95 BY 9.15
         BGCOLOR 15 FONT 5
         TITLE BGCOLOR 15 "Consulta de los Cheques".

DEFINE BROWSE Brw_Proceso
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Brw_Proceso W-Win _FREEFORM
  QUERY Brw_Proceso DISPLAY
      Che_Transito.Agencia COLUMN-LABEL "Age" FORMAT "999":U WIDTH 4.43
      Che_Transito.Ofi_Destino COLUMN-LABEL "Orig"
      Che_Transito.Cod_Compensa COLUMN-LABEL "Bco" FORMAT "99":U
      Che_Transito.Cheque COLUMN-LABEL "Nro.Cheque" FORMAT "X(10)":U WIDTH 10.43
      Che_Transito.Num_Cuenta COLUMN-LABEL "Nro.  Cuenta" FORMAT "X(14)":U WIDTH 11.57
      Che_Transito.Tip_Producto COLUMN-LABEL "Pcto" FORMAT "X(1)":U WIDTH 4.29
      Che_Transito.Cod_Producto COLUMN-LABEL "Cod.Pto" FORMAT "999":U WIDTH 7.43
      Che_Transito.Fec_Canje COLUMN-LABEL "Fec.Canje" FORMAT "99/99/9999":U
      Che_Transito.Fec_Confirmacion COLUMN-LABEL "Fec.Confirma" FORMAT "99/99/9999":U WIDTH 11.72
      Che_Transito.Valor COLUMN-LABEL "Valor" FORMAT "->>>>>,>>>,>>>,>>9.99":U
      Che_Transito.Estado COLUMN-LABEL "E" FORMAT "9":U WIDTH 3
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 97 BY 9.42
         BGCOLOR 15 FONT 5
         TITLE BGCOLOR 15 "Selección de los Cheques".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Frame_Imprimir
     Btn_Imp AT ROW 1.27 COL 26
     Rad_Imprimir AT ROW 1.54 COL 3 NO-LABEL
     Btn_Can AT ROW 3.15 COL 26
     Fill_FecIni AT ROW 6.38 COL 1 COLON-ALIGNED NO-LABEL
     Fill_FecFin AT ROW 6.38 COL 21 COLON-ALIGNED NO-LABEL
     "Fecha Inicial" VIEW-AS TEXT
          SIZE 12 BY .77 AT ROW 5.58 COL 3
          BGCOLOR 17 FGCOLOR 7 
     "Fecha Final" VIEW-AS TEXT
          SIZE 11 BY .77 AT ROW 5.58 COL 23
          BGCOLOR 17 FGCOLOR 7 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 51 ROW 5.73
         SIZE 38.57 BY 7.58
         BGCOLOR 17 FONT 5
         TITLE "Imprimir Cheques".

DEFINE FRAME F-Main
     Brw_Proceso AT ROW 10.42 COL 3
     Brw_Consulta AT ROW 10.69 COL 3
     Btn_Ingresar AT ROW 13.12 COL 102
     BUTTON-117 AT ROW 1.81 COL 101
     F_Mensage AT ROW 21.19 COL 2 COLON-ALIGNED NO-LABEL
     Com_Agencia AT ROW 2.88 COL 3 COLON-ALIGNED NO-LABEL
     Com_Consulta AT ROW 2.88 COL 35 COLON-ALIGNED NO-LABEL
     F-CtaCble AT ROW 2.88 COL 57 COLON-ALIGNED NO-LABEL
     F_RegProcesados AT ROW 2.88 COL 75 COLON-ALIGNED NO-LABEL
     Rad_Estado AT ROW 5.31 COL 4 NO-LABEL
     Rad_Producto AT ROW 5.42 COL 21 NO-LABEL
     F_BcoIni AT ROW 6.38 COL 32 COLON-ALIGNED
     F_BcoFin AT ROW 6.31 COL 57.14 COLON-ALIGNED
     F_CueIni AT ROW 7.46 COL 32 COLON-ALIGNED
     F_CtaFin AT ROW 7.38 COL 57.14 COLON-ALIGNED
     F_FecIni AT ROW 8.54 COL 32 COLON-ALIGNED
     F_FecFin AT ROW 8.46 COL 57.14 COLON-ALIGNED
     Btn_Liberar AT ROW 14.73 COL 102
     Btn_Devolver AT ROW 16.35 COL 102
     Btn_Salir AT ROW 17.96 COL 102
     Btn_Imprimir AT ROW 3.69 COL 101
     Btn_Ayuda AT ROW 19.85 COL 106
     NomCta AT ROW 3.96 COL 57 COLON-ALIGNED NO-LABEL
     F-CtaSyADev AT ROW 5.77 COL 73.57 COLON-ALIGNED NO-LABEL
     NomCta2 AT ROW 6.62 COL 73.72 COLON-ALIGNED NO-LABEL
     "Selección de Consulta" VIEW-AS TEXT
          SIZE 20 BY .81 AT ROW 4.46 COL 20.14
          BGCOLOR 17 FGCOLOR 7 
     "Mensajes de Proceso" VIEW-AS TEXT
          SIZE 20 BY 1.04 AT ROW 20.12 COL 4
          BGCOLOR 17 FGCOLOR 7 
     "Cta-Banco(Dev.)" VIEW-AS TEXT
          SIZE 17 BY .81 AT ROW 2.08 COL 59
          BGCOLOR 17 FGCOLOR 7 
     "Cta-S.y A (Dev.)" VIEW-AS TEXT
          SIZE 15.14 BY .81 AT ROW 5 COL 75.57
          BGCOLOR 17 FGCOLOR 7 
     "Agencia" VIEW-AS TEXT
          SIZE 8 BY .81 AT ROW 2.08 COL 5
          BGCOLOR 17 FGCOLOR 7 
     "Nivel de Consulta" VIEW-AS TEXT
          SIZE 20 BY .81 AT ROW 2.08 COL 37
          BGCOLOR 17 FGCOLOR 7 
     "Registros Procesados" VIEW-AS TEXT
          SIZE 20 BY .81 AT ROW 2.08 COL 77
          BGCOLOR 17 FGCOLOR 7 
     "Estado Cheques" VIEW-AS TEXT
          SIZE 14.86 BY .81 AT ROW 4.23 COL 3
          BGCOLOR 17 FGCOLOR 7 
     RECT-116 AT ROW 5.04 COL 21
     RECT-117 AT ROW 5.04 COL 3
     RECT-281 AT ROW 1.81 COL 3
    WITH 1 DOWN NO-BOX OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 114 BY 21.38
         BGCOLOR 17 FONT 5
         DEFAULT-BUTTON Btn_Salir.

DEFINE FRAME F_Ingresar
     Cmb_AgenciasCanje AT ROW 1.27 COL 16 COLON-ALIGNED
     Che_Transito.Cheque AT ROW 2.35 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 23 BY .81
          BGCOLOR 15 
     Che_Transito.Valor AT ROW 3.42 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 23 BY .81
          BGCOLOR 15 
     Che_Transito.Tip_Remesa AT ROW 3.69 COL 46 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Local", 1,
"Al Cobro", 2,
"Negociada", 3
          SIZE 14 BY 2.42
     Che_Transito.Fec_Canje AT ROW 4.5 COL 28 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .92
          BGCOLOR 15 
     Che_Transito.Cod_Compensa AT ROW 5.58 COL 28 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .81
          BGCOLOR 15 
     Btn_Salvar AT ROW 6.12 COL 40
     BUTTON-120 AT ROW 6.12 COL 52
     "Tipo de Remesa" VIEW-AS TEXT
          SIZE 16 BY .88 AT ROW 2.62 COL 44
          FGCOLOR 7 
     RECT-282 AT ROW 3.15 COL 43
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 32 ROW 11.77
         SIZE 64 BY 7.54
         BGCOLOR 17 FONT 5
         TITLE "Ingreso de Cheques en Canje".

DEFINE FRAME Frame_Ejecutar
     Btn_Ejecutar AT ROW 1.27 COL 34
     Rad_Seleccion AT ROW 2.35 COL 6 NO-LABEL
     Btn_Cancelar AT ROW 3.69 COL 34
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 24.72 ROW 11.08
         SIZE 47 BY 5.65
         BGCOLOR 17 FONT 5
         TITLE "Liberación de Cheques".


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
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Cheques en Canje"
         HEIGHT             = 21.38
         WIDTH              = 114.29
         MAX-HEIGHT         = 36.54
         MAX-WIDTH          = 182.86
         VIRTUAL-HEIGHT     = 36.54
         VIRTUAL-WIDTH      = 182.86
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME F_Ingresar:FRAME = FRAME F-Main:HANDLE.

/* SETTINGS FOR FRAME F-Main
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB Brw_Proceso 1 F-Main */
/* BROWSE-TAB Brw_Consulta Brw_Proceso F-Main */
ASSIGN 
       Brw_Consulta:HIDDEN  IN FRAME F-Main                = TRUE.

ASSIGN 
       Brw_Proceso:HIDDEN  IN FRAME F-Main                = TRUE.

/* SETTINGS FOR FILL-IN F-CtaSyADev IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F_BcoFin IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F_BcoIni IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F_CtaFin IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F_CueIni IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F_FecFin IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F_FecIni IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F_Mensage IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F_RegProcesados IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN NomCta IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN NomCta2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET Rad_Producto IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME Frame_Ejecutar
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME Frame_Ejecutar:HIDDEN           = TRUE
       FRAME Frame_Ejecutar:MOVABLE          = TRUE.

/* SETTINGS FOR FRAME Frame_Imprimir
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME Frame_Imprimir:HIDDEN           = TRUE
       FRAME Frame_Imprimir:MOVABLE          = TRUE.

/* SETTINGS FOR FILL-IN Fill_FecFin IN FRAME Frame_Imprimir
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Fill_FecIni IN FRAME Frame_Imprimir
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Ingresar
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Ingresar:HIDDEN           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Brw_Consulta
/* Query rebuild information for BROWSE Brw_Consulta
     _TblList          = "bdCentral.Che_Transito"
     _FldNameList[1]   > bdCentral.Che_Transito.Agencia
"Che_Transito.Agencia" "Age" ? "integer" ? ? ? ? ? ? no ? no no "4.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > bdCentral.Che_Transito.Cod_Compensa
"Che_Transito.Cod_Compensa" "Bco" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > bdCentral.Che_Transito.Cheque
"Che_Transito.Cheque" "Nro.Cheque" ? "character" ? ? ? ? ? ? no ? no no "10.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > bdCentral.Che_Transito.Num_Cuenta
"Che_Transito.Num_Cuenta" "Nro. Cuenta" ? "character" ? ? ? ? ? ? no ? no no "10.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > bdCentral.Che_Transito.Tip_Producto
"Che_Transito.Tip_Producto" "Pcto" ? "character" ? ? ? ? ? ? no ? no no "4.86" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > bdCentral.Che_Transito.Cod_Producto
"Che_Transito.Cod_Producto" "Cod.Pto" ? "integer" ? ? ? ? ? ? no ? no no "8.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > bdCentral.Che_Transito.Fec_Canje
"Che_Transito.Fec_Canje" "Fec.Canje" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > bdCentral.Che_Transito.Fec_Confirmacion
"Che_Transito.Fec_Confirmacion" "Fec.Confirma" ? "date" ? ? ? ? ? ? no ? no no "11.86" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > bdCentral.Che_Transito.Valor
"Che_Transito.Valor" "Valor" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > bdCentral.Che_Transito.Estado
"Che_Transito.Estado" "E" ? "integer" ? ? ? ? ? ? no ? no no "3" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE Brw_Consulta */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Brw_Proceso
/* Query rebuild information for BROWSE Brw_Proceso
     _START_FREEFORM
/*OPEN QUERY {&SELF-NAME} FOR EACH Che_Transito NO-LOCK.*/
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE Brw_Proceso */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Ingresar
/* Query rebuild information for FRAME F_Ingresar
     _START_FREEFORM
/*OPEN QUERY {&SELF-NAME} FOR EACH Che_Transito SHARE-LOCK.*/
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* FRAME F_Ingresar */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Cheques en Canje */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Cheques en Canje */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F_Ingresar
&Scoped-define BROWSE-NAME Brw_Proceso
&Scoped-define SELF-NAME Brw_Proceso
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Brw_Proceso W-Win
ON MOUSE-SELECT-CLICK OF Brw_Proceso IN FRAME F-Main /* Selección de los Cheques */
DO:
  /*ASSIGN T_Banco  = INTEGER(Che_Transito.Cod_Compensa:SCREEN-VALUE IN BROWSE Brw_Proceso)
 *          T_Cheque = Che_Transito.Cheque:SCREEN-VALUE IN BROWSE Brw_Proceso
 *          I = 1  J = 1.
 *   IF Brw_Proceso:IS-ROW-SELECTED(Brw_Proceso:FOCUSED-ROW) EQ TRUE THEN DO:
 *      GET FIRST Brw_Proceso NO-LOCK.
 *      DO WHILE AVAILABLE(Che_Transito):
 *         IF Che_Transito.Cod_Compensa EQ T_Banco  AND 
 *            Che_Transito.Cheque       EQ T_Cheque THEN DO:
 *            Brw_Proceso:SELECT-ROW(I) NO-ERROR.
 *         END.
 *         ASSIGN I = I + 1.
 *         GET NEXT Brw_Proceso NO-LOCK.
 *      END.
 *   END.
 *   ELSE DO:
 *      /*Brw_Proceso:DESELECT-ROWS().*/
 *      GET FIRST Brw_Proceso NO-LOCK.
 *      DO WHILE AVAILABLE(Che_Transito):
 *         IF Che_Transito.Cod_Compensa EQ T_Banco  AND 
 *            Che_Transito.Cheque       EQ T_Cheque THEN DO:
 *            Brw_Proceso:DESELECT-SELECTED-ROW(J) NO-ERROR.
 *         END.
 *         ASSIGN J = J + 1.
 *         GET NEXT Brw_Proceso NO-LOCK.
 *      END.
 *   END.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Ayuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ayuda W-Win
ON CHOOSE OF Btn_Ayuda IN FRAME F-Main /* Ayuda */
OR HELP OF {&WINDOW-NAME}
DO:
   SYSTEM-HELP "ayudas/tesoreri" CONTEXT 22.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Frame_Imprimir
&Scoped-define SELF-NAME Btn_Can
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Can W-Win
ON CHOOSE OF Btn_Can IN FRAME Frame_Imprimir /* Cancelar */
DO:
  FRAME F-Main:SENSITIVE         = TRUE.
  FRAME Frame_Imprimir:HIDDEN    = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Frame_Ejecutar
&Scoped-define SELF-NAME Btn_Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancelar W-Win
ON CHOOSE OF Btn_Cancelar IN FRAME Frame_Ejecutar /* Cancelar */
DO:
  FRAME Frame_Ejecutar:HIDDEN = TRUE.
  FRAME F-Main:SENSITIVE      = TRUE.
  ASSIGN F_Mensage:SCREEN-VALUE IN FRAME F-Main = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME Btn_Devolver
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Devolver W-Win
ON CHOOSE OF Btn_Devolver IN FRAME F-Main /* Devolver */
DO:
  DEFINE VARIABLE W_Contador AS INTEGER INITIAL 0.
  DEFINE VAR      I          AS INTEGER.

  W_SiContab = FALSE.

  IF (F-CtaCble   LE " " AND Che_Transito.Tip_Producto NE "2")
  OR (F-CtaSyADev LE " " AND Che_Transito.Tip_Producto NE "2") THEN DO:
     MESSAGE "Debe Seleccionar Cuenta Contable para Contabilizar el Banco de la Devolución." SKIP
             "Y La Cta - S.y A. debe Existir."
         VIEW-AS ALERT-BOX ERROR.
     RETURN NO-APPLY.
  END.

  IF NOT AVAIL(Che_Transito) THEN DO:
     MESSAGE "Debe Seleccionar El cheque para la Devolución." SKIP             
         VIEW-AS ALERT-BOX ERROR.
     RETURN NO-APPLY.
  END.

  RUN MostrarMensaje IN W_Manija (INPUT 112,OUTPUT W_Error).
  IF W_Error EQ FALSE THEN
     RETURN NO-APPLY.

  DO WITH FRAME F-MAIN:
   DO TRANSACTION ON ERROR  UNDO, RETURN NO-APPLY
                  ON ENDKEY UNDO, RETURN NO-APPLY
                  ON STOP   UNDO, RETURN NO-APPLY:
      ASSIGN F_Mensage:SCREEN-VALUE = "Procesando de Devolución de Cheques...".
      SESSION:SET-WAIT-STATE("GENERAL").
      IF Brw_Proceso:NUM-SELECTED-ROWS NE 0 THEN DO:
         FOR EACH Aux_Taquilla:
             DELETE Aux_Taquilla.
         END.
         DO I = 1 TO Brw_Proceso:NUM-SELECTED-ROWS BY 1:
            ASSIGN W_Contador = W_Contador + 1
                   F_RegProcesados:SCREEN-VALUE = STRING(W_Contador).
            Brw_Proceso:FETCH-SELECTED-ROW(I).
            GET CURRENT Brw_Proceso EXCLUSIVE-LOCK.
            IF Che_Transito.Tip_Producto EQ "0" THEN DO:
               RUN Dev_Cuentas NO-ERROR.
               IF ERROR-STATUS:ERROR THEN DO:
                  ASSIGN W_Contador = 0.
                  UNDO, RETURN NO-APPLY.
               END.
            END.
            IF Che_Transito.Tip_Producto EQ "1" THEN DO:
               RUN Dev_Ahorros NO-ERROR.
               IF ERROR-STATUS:ERROR THEN DO:
                  ASSIGN W_Contador = 0.
                  UNDO, RETURN NO-APPLY.
               END.
            END.
            IF Che_Transito.Tip_Producto EQ "2" THEN DO:
               ASSIGN Che_Transito.Estado = 3.
               MESSAGE "Solo Marcado como Devuelto (Creditos), la devolución debe ser por traslados."
                   VIEW-AS ALERT-BOX INFO BUTTONS OK.

              /* RUN Dev_Creditos NO-ERROR.
               IF ERROR-STATUS:ERROR THEN DO:
                  ASSIGN W_Contador = 0.
                  UNDO, RETURN NO-APPLY.
               END.*/
            END.
            IF Che_Transito.Tip_Producto EQ "4" THEN DO:
               RUN Dev_Especiales NO-ERROR.
               IF ERROR-STATUS:ERROR THEN DO:
                  ASSIGN W_Contador = 0.
                  UNDO, RETURN NO-APPLY.
               END.
            END.
         END.
         IF Brw_Proceso:NUM-SELECTED-ROWS GT 0 THEN
            Brw_Proceso:DELETE-SELECTED-ROWS().
         FOR EACH Aux_Taquilla:
             BUFFER-COPY Aux_Taquilla TO Taquilla.
             RELEASE Taquilla.
         END.
         IF W_Contador GT 0 THEN 
            RUN MostrarMensaje IN W_Manija (INPUT 118, OUTPUT W_Eleccion).
      END.
      ELSE DO:
         MESSAGE "No Existe Ningun Registro Seleccionado" SKIP
                 "en el Browse Para el Proceso."
         VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
         TITLE "Error En El Proceso".
         UNDO, RETURN NO-APPLY.
      END.
      ASSIGN F_RegProcesados:SCREEN-VALUE = ""
             F_Mensage:SCREEN-VALUE       = "".
      SESSION:SET-WAIT-STATE("").
   END.

   RELEASE Che_Transito.

   DEFI VAR Listado AS CHAR FORM "X(40)".                                                                                                                                                                                   
   ASSIGN Listado = W_PathSpl + "DEVOL-" + STRING(Comprobantes.Secuencia)  + ".Lst"
          W_SiContab = TRUE.
                                                                                                            
  {Incluido\Imprimir.I "listado"}

   W_SiContab = FALSE.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Frame_Ejecutar
&Scoped-define SELF-NAME Btn_Ejecutar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ejecutar W-Win
ON CHOOSE OF Btn_Ejecutar IN FRAME Frame_Ejecutar /* Procesar */
DO:
    vTime = TIME.

    ASSIGN FRAME Frame_Ejecutar Rad_Seleccion.

    IF Rad_Seleccion EQ 1 THEN
        RUN Cheques_Dia.
    ELSE
        RUN Cheques_Seleccionados.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Frame_Imprimir
&Scoped-define SELF-NAME Btn_Imp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imp W-Win
ON CHOOSE OF Btn_Imp IN FRAME Frame_Imprimir /* Imprimir */
DO:
  DEFINE VAR Listado AS CHARACTER INITIAL "".
   
  Listado = W_PathSpl + "PrTaqui.LST".
  {Incluido\Imprimir.I "Listado"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME Btn_Imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imprimir W-Win
ON CHOOSE OF Btn_Imprimir IN FRAME F-Main /* Imprimir */
DO:
  FRAME F-Main:SENSITIVE         = FALSE.
  FRAME Frame_Imprimir:SENSITIVE = TRUE.
  FRAME Frame_Imprimir:HIDDEN    = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Ingresar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ingresar W-Win
ON CHOOSE OF Btn_Ingresar IN FRAME F-Main /* Ingresar */
DO:
  DO WITH FRAME F_Ingresar:
     ASSIGN Che_Transito.Cheque:SCREEN-VALUE        = ""
            Che_Transito.Valor:SCREEN-VALUE         = "0"
            Che_Transito.Cod_Compensa:SCREEN-VALUE  = "0"
            Che_Transito.Tip_Remesa:SCREEN-VALUE    = "1"
            Che_Transito.Fec_Canje:SCREEN-VALUE     = STRING(TODAY).
     FIND Agencias WHERE Agencias.Agencia EQ W_Agencia NO-LOCK NO-ERROR.
     IF AVAILABLE Agencias THEN
        Cmb_AgenciasCanje:SCREEN-VALUE IN FRAME F_Ingresar = STRING(Agencias.Agencia,"999") + " " + STRING(Agencias.Nombre,"X(30)").
  END.
  VIEW FRAME F_Ingresar.
  APPLY "entry" TO Cmb_AgenciasCanje IN FRAME F_Ingresar.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Liberar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Liberar W-Win
ON CHOOSE OF Btn_Liberar IN FRAME F-Main /* Liberar */
DO:
/*  IF W_Agencia NE 10 THEN DO:
     MESSAGE "Solo Libera la Direcc.Administrativa..."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     RETURN.
  END.  */

  ASSIGN F_Mensage:SCREEN-VALUE IN FRAME F-Main = "Proceso de Liberación de Los Cheques...".
  FRAME F-Main:SENSITIVE         = FALSE.
  FRAME Frame_Ejecutar:SENSITIVE = TRUE.
  FRAME Frame_Ejecutar:HIDDEN    = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir W-Win
ON CHOOSE OF Btn_Salir IN FRAME F-Main /* Salir */
DO:
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ingresar
&Scoped-define SELF-NAME Btn_Salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salvar W-Win
ON CHOOSE OF Btn_Salvar IN FRAME F_Ingresar /* Salvar */
DO:
DO WITH FRAME F_Ingresar:
  /*verificacion de informacion*/
  IF DECIMAL(Che_Transito.Valor:SCREEN-VALUE) EQ 0 THEN DO:
     MESSAGE "No se permite grabar cheques con valor cero. Rectifique!" VIEW-AS ALERT-BOX ERROR.
     APPLY "entry" TO Che_Transito.Valor.
     RETURN NO-APPLY.
  END.
  IF DECIMAL(Che_Transito.Cod_Compensa:SCREEN-VALUE) EQ 0 THEN DO:
     MESSAGE "No se permite grabar cheques con Codigo de" SKIP
             "Compensación en Cero. Rectifique!" VIEW-AS ALERT-BOX ERROR.
     APPLY "entry" TO Che_Transito.Cod_Compensa.
     RETURN NO-APPLY.
  END.
  FIND Bancos WHERE Bancos.Cod_Compensa EQ INTEGER(Che_Transito.Cod_Compensa:SCREEN-VALUE) NO-LOCK NO-ERROR.
  IF NOT AVAILABLE Bancos THEN DO:
     MESSAGE "El Codigo de Compensación No existe. Rectifique!" VIEW-AS ALERT-BOX ERROR.
     APPLY "leave" TO Che_Transito.Cod_Compensa.
     RETURN NO-APPLY.
  END.
     
  IF Che_Transito.Cheque:SCREEN-VALUE EQ "?" OR Che_Transito.Cheque:SCREEN-VALUE EQ "" THEN DO:
     MESSAGE "No se permite grabar cheques con Codigo de" SKIP
             "Cheque en blancos. Rectifique!" VIEW-AS ALERT-BOX ERROR.
     APPLY "entry" TO Che_Transito.Cheque.
     RETURN NO-APPLY.
  END.
  FIND Che_Transito WHERE
       Che_Transito.Agencia EQ INTEGER(SUBSTRING(Cmb_AgenciasCanje:SCREEN-VALUE,1,3)) AND
       Che_Transito.Cheque  EQ Che_Transito.Cheque:SCREEN-VALUE AND 
       Che_Transito.Cod_Compensa EQ INTEGER(Che_Transito.Cod_Compensa:SCREEN-VALUE) NO-ERROR.
  IF AVAILABLE Che_Transito THEN DO:
     MESSAGE "Este cheque ya existe para esta Agencia y" SKIP
             "Este codigo de Compensacion. Rectifique!" VIEW-AS ALERT-BOX ERROR.
     APPLY "entry" TO Che_Transito.Cheque.
     RETURN NO-APPLY.
  END.
  CREATE Che_Transito.
  ASSIGN Che_Transito.Agencia = INTEGER(SUBSTRING(Cmb_AgenciasCanje:SCREEN-VALUE,1,3))
         Che_Transito.Cheque  = Che_Transito.Cheque:SCREEN-VALUE
         Che_Transito.Valor   = DECIMAL(Che_Transito.Valor:SCREEN-VALUE)
         Che_Transito.Tip_Remesa = INTEGER(Che_Transito.Tip_Remesa:SCREEN-VALUE)
         Che_Transito.Cod_Compensa = INTEGER(Che_Transito.Cod_Compensa:SCREEN-VALUE)
         Che_Transito.Fec_Canje = DATE(Che_Transito.Fec_Canje:SCREEN-VALUE)
         Che_Transito.Estado = 1
         Che_Transito.Ofi_Destino = Che_Transito.Agencia.
   RUN Local-Initialize.
   HIDE FRAME F_Ingresar.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME BUTTON-117
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-117 W-Win
ON CHOOSE OF BUTTON-117 IN FRAME F-Main /* Button 117 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ingresar
&Scoped-define SELF-NAME BUTTON-120
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-120 W-Win
ON CHOOSE OF BUTTON-120 IN FRAME F_Ingresar /* Button 120 */
DO:
  HIDE FRAME F_Ingresar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Che_Transito.Cod_Compensa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Che_Transito.Cod_Compensa W-Win
ON LEAVE OF Che_Transito.Cod_Compensa IN FRAME F_Ingresar /* Código de Compensación */
DO:
  FIND Bancos WHERE Bancos.Cod_Compensa EQ INTEGER(SELF:SCREEN-VALUE) NO-LOCK NO-ERROR.
  IF NOT AVAILABLE Bancos THEN DO:
     RUN C-Bancos.r (OUTPUT P_CodComp, OUTPUT P_NomComp).
     Che_Transito.Cod_Compensa:SCREEN-VALUE IN FRAME F_Ingresar = STRING(P_CodComp).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME Com_Agencia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Com_Agencia W-Win
ON VALUE-CHANGED OF Com_Agencia IN FRAME F-Main
DO:
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN Com_Agencia
            T_OfiIni = INTEGER(SUBSTRING(Com_Agencia,1,3)).
     IF T_OfiIni EQ 0 THEN DO:
        ASSIGN T_OfiFin = 999.
     END.
     ELSE DO:
        ASSIGN T_OfiFin = T_OfiIni.
     END.

     APPLY "VALUE-CHANGED" TO Rad_Estado.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Com_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Com_Consulta W-Win
ON VALUE-CHANGED OF Com_Consulta IN FRAME F-Main
DO:
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN Com_Consulta
            F_BcoFin:SCREEN-VALUE = ""
            F_BcoIni:SCREEN-VALUE = ""
            F_CtaFin:SCREEN-VALUE = ""
            F_CueIni:SCREEN-VALUE = ""
            F_FecFin:SCREEN-VALUE = ""
            F_FecIni:SCREEN-VALUE = "".
     CASE Com_Consulta:
      WHEN "Todos" THEN DO:
         DISABLE F_BcoFin F_BcoIni F_CtaFin F_CueIni F_FecFin F_FecIni Rad_Producto.
         APPLY "VALUE-CHANGED" TO Rad_Estado.
         RETURN NO-APPLY.
      END.
      WHEN "Por Producto" THEN DO:
         DISABLE F_BcoFin F_BcoIni F_CtaFin F_CueIni F_FecFin F_FecIni.
         ENABLE  Rad_Producto.
         APPLY "VALUE-CHANGED" TO Rad_Estado.
         APPLY "ENTRY"         TO Rad_Producto.
         RETURN NO-APPLY.
      END.
      WHEN "Por Cuenta" THEN DO:
         DISABLE F_BcoFin F_BcoIni F_FecFin F_FecIni Rad_Producto.
         ENABLE  F_CtaFin F_CueIni.
         APPLY "ENTRY" TO F_CueIni.
         RETURN NO-APPLY.
      END.
      WHEN "Por Fecha" THEN DO:
         DISABLE F_BcoFin F_BcoIni F_CtaFin F_CueIni Rad_Producto.
         ENABLE  F_FecFin F_FecIni.
         APPLY "ENTRY" TO F_FecIni.
         RETURN NO-APPLY.
      END.
      WHEN "Por Banco" THEN DO:
         DISABLE F_CtaFin F_CueIni F_FecFin F_FecIni Rad_Producto.
         ENABLE  F_BcoFin F_BcoIni.
         APPLY "ENTRY" TO F_BcoIni.
         RETURN NO-APPLY.
      END.
     END CASE.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F-CtaCble
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-CtaCble W-Win
ON ENTRY OF F-CtaCble IN FRAME F-Main
DO:
  IF NOT W_Inicio THEN
     W-Win:MOVE-TO-TOP().
  
  W_Inicio = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-CtaCble W-Win
ON LEAVE OF F-CtaCble IN FRAME F-Main
DO:
  ASSIGN F-CtaCble.
  FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ F-CtaCble
                     AND   Cuentas.Tipo   EQ 2
                     AND   Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
  IF NOT AVAILABLE(Cuentas) THEN
     ASSIGN F-CtaCble = ""
            F-CtaCble:SCREEN-VALUE = ""
            NomCta:SCREEN-VALUE = "". 
  ELSE NomCta:SCREEN-VALUE = Cuentas.Nombre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-CtaCble W-Win
ON MOUSE-SELECT-DBLCLICK OF F-CtaCble IN FRAME F-Main
DO:
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN P_TipCta = "M".
     RUN C-Cuentas.r(OUTPUT P_Cuenta,OUTPUT P_Nombre,OUTPUT P_NatTra,
                    OUTPUT P_CtrNat,INPUT P_TipCta).
     ASSIGN F-CtaCble:SCREEN-VALUE = P_Cuenta
            F-CtaCble
            NomCta:SCREEN-VALUE = P_Nombre.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-CtaCble W-Win
ON TAB OF F-CtaCble IN FRAME F-Main
OR RETURN OF F-CtaCble DO:
   DO WITH FRAME {&FRAME-NAME}:
      IF F-CtaCble:SCREEN-VALUE NE "" THEN DO:
         FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ F-CtaCble:SCREEN-VALUE 
                            AND   Cuentas.Tipo   EQ 2
                            AND   Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
         IF NOT AVAILABLE(Cuentas) THEN DO:
            ASSIGN P_TipCta = "M".
            RUN C-Cuentas.r(OUTPUT P_Cuenta,OUTPUT P_Nombre,OUTPUT P_NatTra,
                          OUTPUT P_CtrNat,INPUT "M").
            ASSIGN F-CtaCble:SCREEN-VALUE = P_Cuenta
                   F-CtaCble
                   NomCta:SCREEN-VALUE    = P_Nombre.
         END.
      END.
      ELSE DO:
          RUN C-Cuentas.r(OUTPUT P_Cuenta,OUTPUT P_Nombre,OUTPUT P_NatTra,
                          OUTPUT P_CtrNat,INPUT "M").
          ASSIGN F-CtaCble:SCREEN-VALUE = P_Cuenta
                 F-CtaCble
                 NomCta:SCREEN-VALUE    = P_Nombre.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F_BcoFin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F_BcoFin W-Win
ON ENTRY OF F_BcoFin IN FRAME F-Main /* Bco.Final */
DO:
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN F_BcoIni.
     IF F_BcoIni EQ ? THEN DO:
        APPLY "ENTRY" TO F_BcoIni.
        RETURN NO-APPLY.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F_BcoFin W-Win
ON LEAVE OF F_BcoFin IN FRAME F-Main /* Bco.Final */
DO:
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN F_BcoFin F_BcoIni.
     IF F_BcoIni GT F_BcoFin THEN DO:
        MESSAGE "El Banco Inicial Debe Ser Menor" SKIP 
                "o Igual al Banco Final."
        VIEW-AS ALERT-BOX QUESTION BUTTONS OK
        TITLE "Error en el Proceso".
        APPLY "VALUE-CHANGED" TO Rad_Estado.
        APPLY "ENTRY" TO F_BcoIni.
        RETURN NO-APPLY.
     END.
     ELSE DO:
        APPLY "VALUE-CHANGED" TO Rad_Estado.
        APPLY "ENTRY"         TO F_BcoIni.
        RETURN NO-APPLY.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F_BcoIni
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F_BcoIni W-Win
ON TAB OF F_BcoIni IN FRAME F-Main /* Bco.Inicial */
OR RETURN OF F_BcoIni DO:
   APPLY "ENTRY" TO F_BcoFin.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F_CtaFin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F_CtaFin W-Win
ON LEAVE OF F_CtaFin IN FRAME F-Main /* Cta.Final */
DO:
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN F_CtaFin F_CueIni.
     IF F_CueIni GT F_CtaFin THEN DO:
        MESSAGE "La Cuenta Inicial Debe Ser Menor" SKIP 
                "o Igual a la Cuenta Final."
        VIEW-AS ALERT-BOX QUESTION BUTTONS OK
        TITLE "Error en el Proceso".
        APPLY "VALUE-CHANGED" TO Rad_Estado.
        APPLY "ENTRY" TO F_CueIni.
        RETURN NO-APPLY.
     END.
     ELSE DO:
        APPLY "VALUE-CHANGED" TO Rad_Estado.
        APPLY "ENTRY"         TO F_CueIni.
        RETURN NO-APPLY.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F_CueIni
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F_CueIni W-Win
ON TAB OF F_CueIni IN FRAME F-Main /* Cta.Inicial */
OR RETURN OF F_CueIni DO:
   APPLY "ENTRY" TO F_CtaFin.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F_FecFin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F_FecFin W-Win
ON ENTRY OF F_FecFin IN FRAME F-Main /* Fecha Final */
DO:
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN F_FecIni.
     IF F_FecIni EQ ? THEN DO:
        APPLY "ENTRY" TO F_FecIni.
        RETURN NO-APPLY.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F_FecFin W-Win
ON LEAVE OF F_FecFin IN FRAME F-Main /* Fecha Final */
DO:
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN F_FecFin F_FecIni.
     IF F_FecIni GT F_FecFin THEN DO:
        MESSAGE "La Fecha Inicial Debe Ser Menor" SKIP 
                "o Igual a la Fecha Final."
        VIEW-AS ALERT-BOX QUESTION BUTTONS OK
        TITLE "Error en el Proceso".
        APPLY "VALUE-CHANGED" TO Rad_Estado.
        APPLY "ENTRY" TO F_FecIni.
        RETURN NO-APPLY.
     END.
     ELSE DO:
        APPLY "VALUE-CHANGED" TO Rad_Estado.
        APPLY "ENTRY"         TO F_FecIni.
        RETURN NO-APPLY.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F_FecIni
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F_FecIni W-Win
ON TAB OF F_FecIni IN FRAME F-Main /* Fecha Inicial */
OR RETURN OF F_FecIni DO:
   APPLY "ENTRY" TO F_FecFin.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Rad_Estado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Rad_Estado W-Win
ON VALUE-CHANGED OF Rad_Estado IN FRAME F-Main
DO:
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN Com_Consulta Rad_Estado Rad_Producto F_BcoFin  
            F_BcoIni F_CtaFin F_CueIni F_FecFin F_FecIni.
     IF Com_Consulta EQ "Por Producto" THEN DO:
        ASSIGN T_BcoIni = 0
               T_BcoFin = 99
               T_PtoIni = Rad_Producto
               T_PtoFin = Rad_Producto
               T_FecIni = 01/01/1980
               T_FecFin = TODAY
               T_CtaIni = ""
               T_CtaFin = "99999999999999".
     END.
     ELSE
     IF Com_Consulta EQ "Por Cuenta" THEN DO:
        ASSIGN T_BcoIni = 0
               T_BcoFin = 99
               T_PtoIni = "0"
               T_PtoFin = "4"
               T_FecIni = 01/01/1980
               T_FecFin = TODAY
               T_CtaIni = F_CueIni
               T_CtaFin = F_CtaFin.
     END.
     ELSE
     IF Com_Consulta EQ "Por Fecha" THEN DO:
        ASSIGN T_BcoIni = 0
               T_BcoFin = 99
               T_PtoIni = "0"
               T_PtoFin = "4"
               T_FecIni = F_FecIni
               T_FecFin = F_FecFin
               T_CtaIni = ""
               T_CtaFin = "99999999999999".
     END.
     ELSE
     IF Com_Consulta EQ "Por Banco" THEN DO:
        ASSIGN T_BcoIni = F_BcoIni 
               T_BcoFin = F_BcoFin
               T_PtoIni = "0"
               T_PtoFin = "4"
               T_FecIni = 01/01/1980
               T_FecFin = TODAY
               T_CtaIni = ""
               T_CtaFin = "99999999999999".
     END. 
     IF Com_Consulta EQ "Todos" THEN DO:
        CASE Rad_Estado:
         WHEN 1 THEN DO:
            ENABLE Btn_Devolver Btn_Liberar.
            ASSIGN Brw_Consulta:HIDDEN      = TRUE
                   Brw_Proceso:HIDDEN       = FALSE.
            OPEN QUERY Brw_Proceso FOR EACH Che_Transito WHERE Che_Transito.Agencia GE T_OfiIni
                                                         AND   Che_Transito.Agencia LE T_OfiFin 
                                                         AND   Che_Transito.Estado  EQ 1 /*NO-LOCK*/.
            IF NOT W_Inicio THEN DO:
              W-Win:MOVE-TO-TOP().
              /*MESSAGE "si por vchang consulta"
              VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
              W_Inicio = FALSE.
            END.

         END.
         WHEN 2 THEN DO:
            DISABLE Btn_Devolver Btn_Liberar.
            ASSIGN Brw_Consulta:HIDDEN      = FALSE
                   Brw_Proceso:HIDDEN       = TRUE.
            OPEN QUERY Brw_Consulta FOR EACH Che_Transito WHERE Che_Transito.Agencia GE T_OfiIni
                                                          AND   Che_Transito.Agencia LE T_OfiFin 
                                                          AND   Che_Transito.Estado  EQ 2 NO-LOCK.
         END.
         WHEN 3 THEN DO:
            DISABLE Btn_Devolver Btn_Liberar.
            ASSIGN Brw_Consulta:HIDDEN      = FALSE
                   Brw_Proceso:HIDDEN       = TRUE.
            OPEN QUERY Brw_Consulta FOR EACH Che_Transito WHERE Che_Transito.Agencia GE T_OfiIni
                                                          AND   Che_Transito.Agencia LE T_OfiFin 
                                                          AND   Che_Transito.Estado  EQ 3 NO-LOCK.
         END.
         WHEN 4 THEN DO:
            DISABLE Btn_Devolver Btn_Liberar.
            ASSIGN Brw_Consulta:HIDDEN      = FALSE
                   Brw_Proceso:HIDDEN       = TRUE.
            OPEN QUERY Brw_Consulta FOR EACH Che_Transito WHERE Che_Transito.Agencia GE T_OfiIni
                                                          AND   Che_Transito.Agencia LE T_OfiFin 
                                                          AND   Che_Transito.Estado  EQ 4 NO-LOCK.
         END.
         WHEN 5 THEN DO:
            DISABLE Btn_Devolver Btn_Liberar.
            ASSIGN Brw_Consulta:HIDDEN      = FALSE
                   Brw_Proceso:HIDDEN       = TRUE.
            OPEN QUERY Brw_Consulta FOR EACH Che_Transito WHERE Che_Transito.Agencia GE T_OfiIni
                                                          AND   Che_Transito.Agencia LE T_OfiFin 
                                                          AND   Che_Transito.Estado  EQ 4 
                                                          AND   che_transito.conciliado = NO NO-LOCK.
         END.
        END CASE.
     END.
     ELSE DO:
        CASE Rad_Estado:
         WHEN 1 THEN DO:
            ENABLE Btn_Devolver Btn_Liberar.
            ASSIGN Brw_Consulta:HIDDEN      = TRUE
                   Brw_Proceso:HIDDEN       = FALSE.
            RUN Abrir_Consulta(INPUT 1,INPUT T_OfiIni,INPUT T_OfiFin,INPUT T_BcoIni,INPUT T_BcoFin,INPUT T_PtoIni,
                               INPUT T_PtoFin,INPUT T_FecIni,INPUT T_FecFin,INPUT T_CtaIni,INPUT T_CtaFin).
         END.
         WHEN 2 THEN DO:
            DISABLE Btn_Devolver Btn_Liberar.
            ASSIGN Brw_Consulta:HIDDEN      = FALSE
                   Brw_Proceso:HIDDEN       = TRUE.
            RUN Abrir_Consulta(INPUT 2,INPUT T_OfiIni,INPUT T_OfiFin,INPUT T_BcoIni,INPUT T_BcoFin,INPUT T_PtoIni,
                               INPUT T_PtoFin,INPUT T_FecIni,INPUT T_FecFin,INPUT T_CtaIni,INPUT T_CtaFin).
         END.
         WHEN 3 THEN DO:
            DISABLE Btn_Devolver Btn_Liberar.
            ASSIGN Brw_Consulta:HIDDEN      = FALSE
                   Brw_Proceso:HIDDEN       = TRUE.
            RUN Abrir_Consulta(INPUT 3,INPUT T_OfiIni,INPUT T_OfiFin,INPUT T_BcoIni,INPUT T_BcoFin,INPUT T_PtoIni,
                               INPUT T_PtoFin,INPUT T_FecIni,INPUT T_FecFin,INPUT T_CtaIni,INPUT T_CtaFin).
         END.
         WHEN 4 THEN DO:
            DISABLE Btn_Devolver Btn_Liberar.
            ASSIGN Brw_Consulta:HIDDEN      = FALSE
                   Brw_Proceso:HIDDEN       = TRUE.
            RUN Abrir_Consulta(INPUT 4,INPUT T_OfiIni,INPUT T_OfiFin,INPUT T_BcoIni,INPUT T_BcoFin,INPUT T_PtoIni,
                               INPUT T_PtoFin,INPUT T_FecIni,INPUT T_FecFin,INPUT T_CtaIni,INPUT T_CtaFin).
         END.
        END CASE.
     END.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Frame_Imprimir
&Scoped-define SELF-NAME Rad_Imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Rad_Imprimir W-Win
ON VALUE-CHANGED OF Rad_Imprimir IN FRAME Frame_Imprimir
DO:
  DO WITH FRAME Frame_Imprimir:
     ASSIGN Rad_Imprimir.
     CASE Rad_Imprimir:
       WHEN 0 THEN DO:
         ASSIGN Fill_FecFin:SCREEN-VALUE = ""
                Fill_FecIni:SCREEN-VALUE = ""
                Fill_FecFin:SENSITIVE    = FALSE
                Fill_FecIni:SENSITIVE    = FALSE.
       END.
       OTHERWISE   DO:
         ASSIGN Fill_FecFin:SCREEN-VALUE = STRING(TODAY)
                Fill_FecIni:SCREEN-VALUE = STRING(TODAY)
                Fill_FecFin:SENSITIVE    = TRUE
                Fill_FecIni:SENSITIVE    = TRUE.
         APPLY "ENTRY" TO Fill_FecIni.
         RETURN NO-APPLY.
       END.
     END CASE.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME Rad_Producto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Rad_Producto W-Win
ON VALUE-CHANGED OF Rad_Producto IN FRAME F-Main
DO:
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN Rad_Producto.
     APPLY "VALUE-CHANGED" TO Rad_Estado.

     IF NOT W_Inicio THEN DO:
        W-Win:MOVE-TO-TOP().
        MESSAGE "si por Rad-pdcto"
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
        W_Inicio = FALSE.
     END.

     RETURN NO-APPLY.

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Brw_Consulta
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Abrir_Consulta W-Win 
PROCEDURE Abrir_Consulta :
/*------------------------------------------------------------------------------
  OBSERVACIONES : Permite Abrir la Consulta.
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER Estado LIKE Che_Transito.Estado.
DEFINE INPUT PARAMETER OfiIni LIKE Che_Transito.Agencia.
DEFINE INPUT PARAMETER OfiFin LIKE Che_Transito.Agencia.
DEFINE INPUT PARAMETER BcoIni LIKE Che_Transito.Cod_Compensa.
DEFINE INPUT PARAMETER BcoFin LIKE Che_Transito.Cod_Compensa.
DEFINE INPUT PARAMETER PtoIni LIKE Che_Transito.Tip_Producto.
DEFINE INPUT PARAMETER PtoFin LIKE Che_Transito.Tip_Producto.
DEFINE INPUT PARAMETER FecIni LIKE Che_Transito.Fec_Canje.
DEFINE INPUT PARAMETER FecFin LIKE Che_Transito.Fec_Canje.
DEFINE INPUT PARAMETER CtaIni LIKE Che_Transito.Num_Cuenta.
DEFINE INPUT PARAMETER CtaFin LIKE Che_Transito.Num_Cuenta.

DO WITH FRAME {&FRAME-NAME}:
    IF Estado EQ 1 THEN DO:
        ASSIGN Brw_Consulta:HIDDEN = TRUE
               Brw_Proceso:HIDDEN = FALSE.

        OPEN QUERY Brw_Proceso FOR EACH Che_Transito WHERE Che_Transito.Agencia GE OfiIni
                                                       AND Che_Transito.Agencia LE OfiFin
                                                       AND Che_Transito.Estado EQ Estado
                                                       AND Che_Transito.Cod_Compensa GE BcoIni
                                                       AND Che_Transito.Cod_Compensa LE BcoFin
                                                       AND Che_Transito.Tip_Producto GE PtoIni
                                                       AND Che_Transito.Tip_Producto LE PtoFin
                                                       AND Che_Transito.Fec_Canje GE FecIni
                                                       AND Che_Transito.Fec_Canje LE FecFin
                                                       AND Che_Transito.Num_Cuenta GE CtaIni
                                                       AND Che_Transito.Num_Cuenta LE CtaFin.
    END.
    ELSE DO:
        ASSIGN Brw_Consulta:HIDDEN = FALSE
               Brw_Proceso:HIDDEN = TRUE.
        
        OPEN QUERY Brw_Consulta FOR EACH Che_Transito WHERE Che_Transito.Agencia GE T_OfiIni
                                                        AND Che_Transito.Agencia LE T_OfiFin
                                                        AND Che_Transito.Estado EQ Estado
                                                        AND Che_Transito.Cod_Compensa GE BcoIni
                                                        AND Che_Transito.Cod_Compensa LE BcoFin
                                                        AND Che_Transito.Tip_Producto GE PtoIni
                                                        AND Che_Transito.Tip_Producto LE PtoFin
                                                        AND Che_Transito.Fec_Canje GE FecIni
                                                        AND Che_Transito.Fec_Canje LE FecFin
                                                        AND Che_Transito.Num_Cuenta GE CtaIni
                                                        AND Che_Transito.Num_Cuenta LE CtaFin NO-LOCK.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Buscar_Ctas_Liq W-Win 
PROCEDURE Buscar_Ctas_Liq :
/*-----------------------------------------------------------------------------------
  OBSERVACIONES : Definir las cuentas contables para el proceso y Validar que no
                  esten en Blanco.       
-------------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER T_Tpto   AS   INTEGER.
  DEFINE INPUT PARAMETER T_CodPto LIKE Ahorros.Cod_ahorro.
  
  ASSIGN LiqIntDB = "" LiqIntCR = "" CtaIntAnt = "" CtaLiqMor = "" CtaLiqDco = "".
  FIND Clientes WHERE Clientes.Nit EQ Ahorros.Nit NO-LOCK NO-ERROR.
  IF AVAILABLE(Clientes) THEN DO:
     FIND FIRST Liqui_Int WHERE Liqui_Int.Clase_Producto EQ T_Tpto
                          AND   Liqui_Int.Cod_Producto   EQ T_CodPto NO-LOCK NO-ERROR.
     IF AVAILABLE(Liqui_Int) THEN DO:
        IF Clientes.Tipo_Vinculo EQ 1 THEN DO:
           ASSIGN LiqIntDB  = Liqui_Int.CtaDb_LiqAso
                  LiqIntCR  = Liqui_Int.CtaCr_LiqAso
                  CtaIntAnt = Liqui_Int.CtaInt_AntAso
                  CtaLiqMor = Liqui_Int.CtaCr_MoraAso
                  CtaLiqDco = Liqui_Int.CtaCr_DifCobAso
                  CtaSyA    = Liqui_Int.Cta_SucyAge.
        END.
        ELSE DO:
           ASSIGN LiqIntDB  = Liqui_Int.CtaDb_Liq
                  LiqIntCR  = Liqui_Int.CtaCr_Liq
                  CtaIntAnt = Liqui_Int.CtaInt_Ant
                  CtaLiqMor = Liqui_Int.CtaCr_Mora
                  CtaLiqDco = Liqui_Int.CtaCr_DifCob
                  CtaSyA    = Liqui_Int.Cta_SucyAge.
        END.
        IF LiqIntDB  EQ "" THEN RETURN ERROR.
        IF LiqIntCR  EQ "" THEN RETURN ERROR.
        IF CtaIntAnt EQ "" THEN RETURN ERROR.
        IF CtaLiqMor EQ "" THEN RETURN ERROR.
        IF CtaLiqDco EQ "" THEN RETURN ERROR.
     END.
     ELSE RETURN ERROR.
  END.
  ELSE RETURN ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Buscar_Cuenta W-Win 
PROCEDURE Buscar_Cuenta :
/*------------------------------------------------------------------------------
  OBSERVACIONES : Cuentas de los Productos.       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT  PARAMETER B_Pto    LIKE Pro_Ahorros.Tip_ahorro.
   DEFINE INPUT  PARAMETER B_CodPto LIKE Ahorros.Cod_Ahorro.
   DEFINE INPUT  PARAMETER B_Plazo  LIKE Ahorros.Plazo.
   DEFINE INPUT  PARAMETER B_Nit    LIKE Clientes.Nit.
   DEFINE OUTPUT PARAMETER B_Cuenta LIKE Cuentas.Cuenta.
   
   ASSIGN B_Cuenta = "".
   FIND Clientes WHERE Clientes.Nit EQ B_Nit NO-LOCK NO-ERROR.
   IF AVAILABLE(Clientes) THEN DO:
      IF B_Pto EQ 1 THEN DO:
         FIND CortoLargo WHERE CortoLargo.Clase_Producto EQ B_Pto 
                         AND   CortoLargo.Cod_Producto   EQ B_CodPto
                         AND   CortoLargo.Plazo_Inicial  LE B_Plazo
                         AND   CortoLargo.Plazo_Final    GE B_Plazo
                         NO-LOCK NO-ERROR.
         IF AVAILABLE(CortoLargo) THEN DO:
            IF Clientes.Tipo_Vinculo EQ 1 THEN DO:
               ASSIGN B_Cuenta = CortoLargo.Cta_AsoAd.
            END.
            ELSE DO:
               ASSIGN B_Cuenta = CortoLargo.Cta_NoaAd.
            END.
         END.
         ELSE DO:
            RETURN ERROR.
         END.
      END.
      ELSE DO:
         RUN Garantias(INPUT Creditos.Agencia,INPUT Creditos.Cod_Credito,INPUT Creditos.Pagare,OUTPUT W_Error) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN DO:
            MESSAGE "NO HAY GARANTIA".
            RETURN ERROR.
         END.
         IF W_Error THEN DO:
            FIND CortoLargo WHERE CortoLargo.Clase_Producto EQ B_Pto 
                            AND   CortoLargo.Cod_Producto   EQ B_CodPto
                            AND   CortoLargo.Plazo_Inicial  LE B_Plazo
                            AND   CortoLargo.Plazo_Final    GE B_Plazo NO-LOCK NO-ERROR.
            IF AVAILABLE(CortoLargo) THEN DO:
               IF Clientes.Tipo_Vinculo EQ 1 THEN DO:
                  ASSIGN B_Cuenta = CortoLargo.Cta_AsoAd.
               END.
               ELSE DO:
                  ASSIGN B_Cuenta = CortoLargo.Cta_NoaAd.
               END.
            END.
            ELSE 
               RETURN ERROR.
         END.
         ELSE DO:
            FIND CortoLargo WHERE CortoLargo.Clase_Producto EQ B_Pto 
                            AND   CortoLargo.Cod_Producto   EQ B_CodPto
                            AND   CortoLargo.Plazo_Inicial  LE B_Plazo
                            AND   CortoLargo.Plazo_Final    GE B_Plazo NO-LOCK NO-ERROR.
            IF AVAILABLE(CortoLargo) THEN DO:
               IF Clientes.Tipo_Vinculo EQ 1 THEN DO:
                  ASSIGN B_Cuenta = CortoLargo.Cta_AsoNa.
               END.
               ELSE DO:
                  ASSIGN B_Cuenta = CortoLargo.Cta_NoaNa.
               END.
            END.
            ELSE 
               RETURN ERROR.
         END.
      END.
      IF B_Cuenta EQ "" THEN RETURN ERROR.
   END.
   ELSE DO:
      RETURN ERROR.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Canje W-Win 
PROCEDURE Canje :
/* OBSERVACIONES : Definir y contabilizar el valor del canje */

DEFINE OUTPUT PARAMETER T_ValCan AS DECIMAL DECIMALS 0.

DEFINE VAR T_CtaDed LIKE Cuentas.Cuenta.
DEFINE VAR T_CtaPto LIKE Cuentas.Cuenta.

MESSAGE "pro canje"
    VIEW-AS ALERT-BOX QUESTION BUTTONS OK.

ASSIGN T_ValCan = 0.

IF Che_Transito.Tip_Remesa EQ 2 THEN DO:
    FIND FIRST Bancos WHERE Bancos.Cod_Compensa EQ Che_Transito.Cod_Compensa NO-LOCK NO-ERROR.
    IF AVAILABLE(Bancos) THEN DO:
        FIND FIRST Deducible WHERE Deducible.Cod_Deducible EQ Bancos.Cod_RemCobro
                               AND Deducible.Estado EQ 1 NO-LOCK NO-ERROR.
        IF AVAILABLE(Deducible) THEN DO:
            IF Cla_Deducible EQ 1 THEN /* Porcentaje */
                ASSIGN T_ValCan = Che_Transito.Valor * Deducible.Valor / 100.
            ELSE    /* Valor */
                ASSIGN T_ValCan = Deducible.Valor.
            
            ASSIGN T_CtaDed = Deducible.Cuenta.
        END.
        ELSE DO:
            MESSAGE "No Hay Deducible Configurado" SKIP
                    "Para el Cobro de la Remesa."
                VIEW-AS ALERT-BOX QUESTION BUTTONS OK TITLE "Error en Deducibles".
            RETURN ERROR.
        END.
    END.
    ELSE DO:
        MESSAGE "No Hay Banco Configurado."
            VIEW-AS ALERT-BOX QUESTION BUTTONS OK TITLE "Error en Bancos".
        RETURN ERROR.
    END.
END.
ELSE
    IF Che_Transito.Tip_Remesa EQ 3 THEN DO:
        FIND FIRST Bancos WHERE Bancos.Cod_Compensa EQ Che_Transito.Cod_Compensa NO-LOCK NO-ERROR.
        IF AVAILABLE(Bancos) THEN DO:
            FIND FIRST Deducible WHERE Deducible.Cod_Deducible EQ Bancos.Cod_RemNego
                                   AND Deducible.Estado EQ 1 NO-LOCK NO-ERROR.
            IF AVAILABLE(Deducible) THEN DO:
                IF Cla_Deducible EQ 1 THEN  /* Porcentaje */
                    ASSIGN T_ValCan = Che_Transito.Valor * Deducible.Valor / 100.
                ELSE    /* Valor */
                    ASSIGN T_ValCan = Deducible.Valor.

                ASSIGN T_CtaDed = Deducible.Cuenta.
            END.
            ELSE DO:
                MESSAGE "No Hay Deducible Configurado" SKIP
                        "Para el Cobro de la Remesa."
                    VIEW-AS ALERT-BOX QUESTION BUTTONS OK TITLE "Error en Deducibles".
                RETURN ERROR.
            END.
        END.
        ELSE DO:
            MESSAGE "No Hay Banco Configurado."
                VIEW-AS ALERT-BOX QUESTION BUTTONS OK TITLE "Error en Bancos".
            RETURN ERROR.
        END.
    END.

IF T_ValCan NE 0 THEN DO:
    RUN Buscar_Cuenta(INPUT 1,
                      INPUT Ahorros.Cod_Ahorro,
                      INPUT Ahorros.Plazo,
                      INPUT Ahorros.Nit,
                      OUTPUT T_CtaPto) NO-ERROR.
    IF ERROR-STATUS:ERROR OR T_CtaPto EQ "" THEN DO:
        RUN MostrarMensaje IN W_Manija (INPUT 66,
                                        OUTPUT W_Error).
        RETURN ERROR.
    END.

    FIND Operacion WHERE Operacion.Cod_Operacion EQ 010102012 NO-LOCK NO-ERROR.
    IF AVAILABLE(Operacion) AND Operacion.Comprobante NE 0 THEN
        ASSIGN Cbte = Operacion.Comprobante.
    ELSE DO:
        RUN MostrarMensaje IN W_Manija (INPUT 143,
                                        OUTPUT W_Error).
        RETURN ERROR.
    END.

    RUN Cbte_Agencia IN W_Manija (INPUT Cbte,
                                  INPUT Che_Transito.Agencia,
                                  OUTPUT Docto,
                                  OUTPUT W_Error).
    IF W_Error THEN DO:
        RUN MostrarMensaje IN W_Manija (INPUT 144,
                                        OUTPUT W_Eleccion).
        RETURN ERROR.
    END.

    RUN Gra_MovAhorros(INPUT 010102012,
                       INPUT Ahorros.Cod_Ahorro,
                       INPUT Ahorros.Cue_Ahorros,
                       INPUT Che_Transito.Cheque,
                       INPUT Che_Transito.Agencia,
                       INPUT W_Agencia,
                       INPUT Che_Transito.Agencia,
                       INPUT W_Usuario,
                       INPUT 0,
                       INPUT T_ValCan) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN ERROR.

    RUN Gra_MovContable(INPUT Che_Transito.Agencia,
                        INPUT Cbte,
                        INPUT T_CtaPto,
                        INPUT W_Fecha,
                        INPUT "Deducción por Canje",
                        INPUT W_Usuario,
                        INPUT T_ValCan,
                        INPUT 0,
                        INPUT 999,
                        INPUT Docto,
                        INPUT TODAY,
                        INPUT TIME) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN ERROR.

    RUN Gra_MovContable(INPUT Che_Transito.Agencia,
                        INPUT Cbte,
                        INPUT T_CtaDed,
                        INPUT W_Fecha,
                        INPUT "Deducción por Canje",
                        INPUT W_Usuario,
                        INPUT 0,
                        INPUT T_ValCan,
                        INPUT 999,
                        INPUT Docto,
                        INPUT TODAY,
                        INPUT TIME) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN ERROR.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cheques_Dia W-Win 
PROCEDURE Cheques_Dia :
/* OBSERVACIONES : Permite Liberar los Cheques Que Cumplen Con los dias de Canje dado el proceso diario */

DEFINE VARIABLE W_Contador AS INTEGER INITIAL 0.

DO TRANSACTION ON ERROR UNDO, RETURN ERROR
               ON ENDKEY UNDO, RETURN ERROR
               ON STOP UNDO, RETURN ERROR:
    ASSIGN F_Mensage:SCREEN-VALUE IN FRAME F-Main = "Procesando Liberación Cheques Diarios...".

    SESSION:SET-WAIT-STATE("GENERAL").

    FOR EACH Che_Transito WHERE Che_Transito.Agencia GE T_OfiIni 
                            AND Che_Transito.Agencia LE T_OfiFin
                            AND Che_Transito.Estado EQ 1 EXCLUSIVE-LOCK:
        RUN Dia_Habil(INPUT Che_Transito.Agencia,
                      INPUT Che_Transito.Fec_Canje,
                      OUTPUT W_Habil).

        FIND Bancos WHERE Bancos.Cod_Compensa EQ Che_Transito.Cod_Compensa NO-LOCK NO-ERROR.
        IF AVAILABLE(Bancos) THEN
            ASSIGN W_Canje = Bancos.Dia_Canje.

        IF W_Habil GE W_Canje THEN DO:
            ASSIGN W_Contador = W_Contador + 1
                   W_Canje = 99.

            IF Che_Transito.Tip_Producto EQ "1" THEN DO:
                RUN Che_Ahorros NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                    UNDO, RETURN ERROR.
            END.
            ELSE
                ASSIGN Che_Transito.Estado = 2
                       Che_Transito.Fec_Confirmacion = TODAY.
        END.
    END.

    ASSIGN F_RegProcesados:SCREEN-VALUE IN FRAME F-MAIN = STRING(W_Contador).

    SESSION:SET-WAIT-STATE("").

    IF W_Contador EQ 0 THEN
        MESSAGE "No Hay Registros Disponibles Para el Proceso."
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK TITLE "Error En El Proceso".
    ELSE
        RUN MostrarMensaje IN W_Manija (INPUT 118, OUTPUT W_Eleccion).

    ASSIGN F_Mensage:SCREEN-VALUE IN FRAME F-Main = ""
           F_RegProcesados:SCREEN-VALUE IN FRAME F-MAIN = "0".

    RELEASE Che_Transito.

    APPLY "VALUE-CHANGED" TO Rad_Estado.

    RETURN NO-APPLY.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cheques_Seleccionados W-Win 
PROCEDURE Cheques_Seleccionados :
/* OBSERVACIONES : Permite Confirmar los Cheques seleccionados sin validar el canje */

DEFINE VARIABLE I AS INTEGER.
DEFINE VARIABLE W_Contador AS INTEGER INITIAL 0.

DO WITH FRAME F-MAIN:
    DO TRANSACTION ON ERROR UNDO, RETURN ERROR
                   ON ENDKEY UNDO, RETURN ERROR
                   ON STOP UNDO, RETURN ERROR:
        ASSIGN F_Mensage:SCREEN-VALUE = "Procesando Liberación Cheques Seleccionados...".

        SESSION:SET-WAIT-STATE("GENERAL").

        IF Brw_Proceso:NUM-SELECTED-ROWS GT 0 THEN DO:
            DO I = 1 TO Brw_Proceso:NUM-SELECTED-ROWS BY 1:
                W_RPTA = Brw_Proceso:FETCH-SELECTED-ROW(I).

                GET CURRENT Brw_Proceso EXCLUSIVE-LOCK NO-WAIT.

                ASSIGN W_Contador = W_Contador + 1.

                IF Che_Transito.Tip_Producto EQ "1" THEN DO:
                    RUN Che_Ahorros NO-ERROR.
                    IF ERROR-STATUS:ERROR THEN
                        UNDO, RETURN ERROR.
                END.
                ELSE
                    ASSIGN Che_Transito.Estado = 2
                           Che_Transito.Fec_Confirmacion = TODAY.
            END.
        END.
        ELSE DO:
            MESSAGE "No Existe Ningun Registro Seleccionado" SKIP
                    "en el Browse Para el Proceso."
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK TITLE "Error En El Proceso".
            RETURN ERROR.
        END.

        ASSIGN F_RegProcesados:SCREEN-VALUE = STRING(W_Contador)
               F_Mensage:SCREEN-VALUE = "".

        IF Brw_Proceso:NUM-SELECTED-ROWS GT 0 THEN
            W_Rpta = Brw_Proceso:DELETE-SELECTED-ROWS().

        IF W_Contador GT 0 THEN
            RUN MostrarMensaje IN W_Manija (INPUT 118,
                                            OUTPUT W_Eleccion).

        SESSION:SET-WAIT-STATE("").
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Che_Ahorros W-Win 
PROCEDURE Che_Ahorros :
/* OBSERVACIONES : Confirmar cheques de Ahorros que cumplen con los dias de canje */

ASSIGN W_Int = 0
       W_ValCanje = 0.

FOR FIRST Pro_Ahorros WHERE pro_ahorros.Cod_ahorro EQ Che_Transito.Cod_Producto
                        AND Pro_Ahorros.Estado EQ 1 EXCLUSIVE-LOCK,
    FIRST Ahorros WHERE Ahorros.Agencia EQ Che_Transito.Agencia
                    AND Ahorros.Cod_ahorro EQ pro_ahorros.Cod_ahorro
                    AND Ahorros.Cue_Ahorros EQ Che_Transito.Num_Cuenta EXCLUSIVE-LOCK:
    IF Che_Transito.Tip_Remesa NE 1 THEN DO:
        RUN Canje(OUTPUT W_ValCanje) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            RETURN ERROR.
    END.

    IF Ahorros.Detalle_Estado EQ 1 THEN DO:
        ASSIGN Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible + Che_Transito.Valor - W_ValCanje
               Ahorros.Sdo_Inicial = Che_Transito.Valor
               Ahorros.Sdo_Canje = Ahorros.Sdo_Canje - Che_Transito.Valor
               Ahorros.Fec_Ulttransaccion = TODAY.

        IF Pro_Ahorros.Tip_Ahorro EQ 3 THEN DO:
            IF Ahorros.Sdo_Disponible EQ Ahorros.Monto_Apertura THEN
                ASSIGN Ahorros.Detalle_Estado = 2.
        END.
        ELSE
            ASSIGN Ahorros.Detalle_Estado = 2.

        IF Che_Transito.Int_Generado GT 0 THEN DO:
            RUN Ret_Interes(INPUT Ahorros.Per_Liquidacion,
                            INPUT Ahorros.Nit,
                            INPUT 1,
                            INPUT Ahorros.Cod_ahorro,
                            INPUT Che_Transito.Int_Generado,
                            OUTPUT W_Int) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                MESSAGE "Error en el Cálculo de Retención de Intereses.".
                RETURN ERROR.
            END.

            IF Pro_Ahorros.Tip_Ahorro EQ 1 THEN
                ASSIGN Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible + W_Int.
            ELSE DO:
                IF Ahorros.Des_Intereses EQ 1 THEN DO:
                    RUN Des_Int_Ctas_Ahorro NO-ERROR.
                    IF ERROR-STATUS:ERROR THEN
                        RETURN ERROR.
                END.

                IF Ahorros.Des_Intereses EQ 2 THEN DO:
                    RUN Des_Int_Ctas_Credito NO-ERROR.
                    IF ERROR-STATUS:ERROR THEN
                        RETURN ERROR.
                END.

                IF Ahorros.Des_Intereses EQ 3 THEN
                    ASSIGN Ahorros.Int_Pagar = Ahorros.Int_Pagar + W_Int.
            END.

            ASSIGN Ahorros.Sal_Intpagados = Ahorros.Sal_Intpagados + W_Int.
        END.
    END.
    ELSE DO:
        IF Pro_Ahorros.ID_Sobregiro EQ TRUE AND Ahorros.Sdo_Disponible LT 0 THEN DO:
            RUN Sobregiro NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                RETURN ERROR.
        END.
        ELSE DO:
            ASSIGN Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible + Che_Transito.Valor - W_ValCanje
                   Ahorros.Sdo_Canje = Ahorros.Sdo_Canje - Che_Transito.Valor
                   Ahorros.Fec_Ulttransaccion = TODAY.

            IF Che_Transito.Int_Generado GT 0 THEN DO:
                RUN Ret_Interes(INPUT Ahorros.Per_Liquidacion,
                                INPUT Ahorros.Nit,
                                INPUT 1,
                                INPUT Ahorros.Cod_Ahorro,
                                INPUT Che_Transito.Int_Generado,
                                OUTPUT W_Int) NO-ERROR.
                IF ERROR-STATUS:ERROR THEN DO:
                    MESSAGE "Error en el Cálculo de Retención de Intereses.".
                    RETURN ERROR.
                END.

                IF Pro_Ahorros.Tip_ahorro EQ 1 THEN
                    ASSIGN Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible + W_Int.
                ELSE DO:
                    IF Ahorros.Des_Intereses EQ 1 THEN DO:
                        RUN Des_Int_Ctas_Ahorro NO-ERROR.
                        IF ERROR-STATUS:ERROR THEN
                            RETURN ERROR.
                    END.

                    IF Ahorros.Des_Intereses EQ 2 THEN DO:
                        RUN Des_Int_Ctas_Credito NO-ERROR.
                        IF ERROR-STATUS:ERROR THEN
                            RETURN ERROR.
                    END.

                    IF Ahorros.Des_Intereses EQ 3 THEN
                        ASSIGN Ahorros.Int_Pagar = Ahorros.Int_Pagar + W_Int.
                END.

                ASSIGN Ahorros.Sal_Intpagados = Ahorros.Sal_Intpagados + W_Int.
            END.
        END.

        IF Ahorros.Detalle_Estado GT 3 THEN
            ASSIGN Ahorros.Detalle_Estado = 2.
    END.

    IF Ahorros.Per_Liquidacion GT 0 AND Ahorros.Fec_UltLiquidacion GT Che_Transito.Fec_Canje THEN
        ASSIGN Ahorros.Sdo_UltLiquidacion = Ahorros.Sdo_UltLiquidacion + Che_Transito.Valor.
    
    ASSIGN Che_Transito.Estado = 2
           Che_Transito.Fec_Confirmacion = TODAY.
END.

RELEASE Pro_Ahorros.
RELEASE Ahorros.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cta_Interes W-Win 
PROCEDURE Cta_Interes :
/*------------------------------------------------------------------------------
  OBSERVACIONES : Permite consultar las cuentas de interes.       
------------------------------------------------------------------------------*/
   /*DEFINE INPUT  PARAMETER I_Pto    LIKE Pro_Ahorros.Tip_Producto.
 *    DEFINE INPUT  PARAMETER I_CodPto LIKE ahorros.cod_ahorro.
 *    DEFINE INPUT  PARAMETER I_Nit    LIKE Clientes.Nit.
 *    DEFINE OUTPUT PARAMETER I_CuenDb LIKE Cuentas.Cuenta.
 *    DEFINE OUTPUT PARAMETER I_CuenCr LIKE Cuentas.Cuenta.
 *    
 *    ASSIGN I_CuenDb = "" I_CuenCr = "".
 *    FIND Clientes WHERE Clientes.Nit EQ I_Nit NO-LOCK NO-ERROR.
 *    IF AVAILABLE(Clientes) THEN DO:
 *       FIND Liqui_Int WHERE Liqui_Int.Clase_Producto EQ I_Pto
 *                      AND   Liqui_Int.Cod_Producto   EQ I_CodPto NO-LOCK NO-ERROR.
 *       IF AVAILABLE(Liqui_Int) THEN DO:
 *          IF Clientes.Asociado THEN DO:
 *             ASSIGN I_CuenDb = Liqui_Int.CtaDb_LiqAso
 *                    I_CuenCr = Liqui_Int.CtaCr_LiqAso.
 *          END.
 *          ELSE DO:
 *             ASSIGN I_CuenDb = Liqui_Int.CtaDb_Liq
 *                    I_CuenCr = Liqui_Int.CtaCr_Liq.
 *          END.
 *          IF I_CuenDb EQ "" THEN RETURN ERROR.
 *          IF I_CuenCr EQ "" THEN RETURN ERROR.
 *       END.
 *       ELSE
 *          RETURN ERROR.
 *    END.
 *    ELSE
 *       RETURN ERROR.*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cta_Sobregiro W-Win 
PROCEDURE Cta_Sobregiro :
/*------------------------------------------------------------------------------
  OBSERVACIONES : Devolver las cuentas de liquidacion de intereses 
                  para el sobregiro.       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER I_Pto      LIKE Pro_Ahorros.Tip_Ahorro.
   DEFINE INPUT  PARAMETER I_CodPto   LIKE Ahorros.Cod_ahorro.
   DEFINE INPUT  PARAMETER I_Nit      LIKE Clientes.Nit.
   DEFINE OUTPUT PARAMETER C_LiqIntDb LIKE Cuentas.Cuenta.
   DEFINE OUTPUT PARAMETER C_LiqIntCr LIKE Cuentas.Cuenta.
   DEFINE OUTPUT PARAMETER C_LiqMor   LIKE Cuentas.Cuenta.
   DEFINE OUTPUT PARAMETER C_DifCdb   LIKE Cuentas.Cuenta.
   DEFINE OUTPUT PARAMETER C_DifCcr   LIKE Cuentas.Cuenta.
   
   ASSIGN C_LiqIntDb = "" C_LiqIntCr = "" C_LiqMor   = ""
          C_DifCdb   = "" C_DifCcr   = "".
   FIND Clientes WHERE Clientes.Nit EQ I_Nit NO-LOCK NO-ERROR.
   IF AVAILABLE(Clientes) THEN DO:
      FIND Liqui_Int WHERE Liqui_Int.Clase_Producto EQ I_Pto
                     AND   Liqui_Int.Cod_Producto   EQ I_CodPto NO-LOCK NO-ERROR.
      IF AVAILABLE(Liqui_Int) THEN DO:
         IF Clientes.Tipo_Vinculo EQ 1 THEN DO:
            ASSIGN C_LiqIntDb = Liqui_Int.CtaDb_LiqAso
                   C_LiqIntCr = Liqui_Int.CtaCr_LiqAso              
                   C_LiqMor   = Liqui_Int.CtaDb_MoraAso
                   C_DifCdb   = Liqui_Int.CtaDb_DifCobAso
                   C_DifCcr   = Liqui_Int.CtaCr_DifCobAso.
         END.
         ELSE DO:
            ASSIGN C_LiqIntDb = Liqui_Int.CtaDb_Liq
                   C_LiqIntCr = Liqui_Int.CtaCr_Liq           
                   C_LiqMor   = Liqui_Int.CtaDb_Mora
                   C_DifCdb   = Liqui_Int.CtaDb_DifCob
                   C_DifCcr   = Liqui_Int.CtaCr_DifCob.
         END.
         IF C_LiqIntDb EQ "" THEN RETURN ERROR.
         IF C_LiqIntCr EQ "" THEN RETURN ERROR.       
         IF C_LiqMor   EQ "" THEN RETURN ERROR.
         IF C_DifCdb   EQ "" THEN RETURN ERROR.
         IF C_DifCcr   EQ "" THEN RETURN ERROR.
      END.
      ELSE DO:
         RETURN ERROR.
      END.
   END.
   ELSE DO:
      RETURN ERROR.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Des_Int_Cre_Sya W-Win 
PROCEDURE Des_Int_Cre_Sya :
/*------------------------------------------------------------------------------
  Observaciones : Destino de los Intereses del Ahorros a los Créditos.       
------------------------------------------------------------------------------*/
  /* Contabiliza el Abono al Capital */
  IF Capital GT 0 THEN DO:
     RUN Gra_MovCreditos(INPUT 020101004, INPUT Creditos.Cod_Credito, INPUT Creditos.Pagare, INPUT Docto,
                         INPUT Creditos.Agencia,INPUT W_Agencia, INPUT Creditos.Agencia, INPUT W_Usuario, INPUT 0, 
                         INPUT Capital).
     RUN Gra_MovContable(INPUT Ahorros.Agencia, INPUT Cbte,INPUT CtaAuxLiq, INPUT TODAY, 
                         INPUT "Destino Liq. de Intereses",INPUT W_Usuario,INPUT Capital, INPUT 0, 
                         INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
     RUN Gra_MovContable(INPUT Ahorros.Agencia, INPUT Cbte,INPUT CtaSyA, INPUT TODAY, 
                         INPUT "Destino Liq. de Intereses",INPUT W_Usuario,INPUT 0, INPUT Capital, 
                         INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
     RUN Gra_MovContable(INPUT Creditos.Agencia, INPUT Cbte,INPUT CtaSyA, INPUT TODAY, 
                         INPUT "Destino Liq. de Intereses",INPUT W_Usuario,INPUT Capital, INPUT 0, 
                         INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).                    
     RUN Gra_MovContable(INPUT Creditos.Agencia, INPUT Cbte,INPUT CtaCble, INPUT TODAY, 
                         INPUT "Destino Liq. de Intereses",INPUT W_Usuario,INPUT 0, INPUT Capital, 
                         INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
  END.
  /* Contabilizar el Abono a Intereses Corrirntes. */
  IF Int_Cor GT 0 THEN DO:
     RUN Gra_MovCreditos(INPUT 020101005, INPUT Creditos.Cod_Credito, INPUT Creditos.Pagare, INPUT Docto,
                         INPUT Creditos.Agencia,INPUT W_Agencia, INPUT Creditos.Agencia, INPUT W_Usuario, INPUT 0, 
                         INPUT Int_Cor).
     RUN Gra_MovContable(INPUT Ahorros.Agencia, INPUT Cbte,INPUT CtaAuxLiq, INPUT TODAY, 
                         INPUT "Destino Liq. de Intereses",INPUT W_Usuario,INPUT Int_Cor, INPUT 0, 
                         INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
     RUN Gra_MovContable(INPUT Ahorros.Agencia, INPUT Cbte,INPUT CtaSyA, INPUT TODAY, 
                         INPUT "Destino Liq. de Intereses",INPUT W_Usuario,INPUT 0, INPUT Int_Cor, 
                         INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
     RUN Gra_MovContable(INPUT Creditos.Agencia, INPUT Cbte,INPUT CtaSyA, INPUT TODAY, 
                         INPUT "Destino Liq. de Intereses",INPUT W_Usuario,INPUT Int_Cor, INPUT 0, 
                         INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).                    
     RUN Gra_MovContable(INPUT Creditos.Agencia, INPUT Cbte,INPUT LiqIntCR, INPUT TODAY, 
                         INPUT "Destino Liq. de Intereses",INPUT W_Usuario,INPUT 0, INPUT Int_Cor, 
                         INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
  END.
  /* Contabilizar Los Intereses de Mora */
  IF Int_Mor GT 0 THEN DO:
     RUN Gra_MovCreditos(INPUT 020101006, INPUT Creditos.Cod_credito, INPUT Creditos.Pagare, INPUT Docto,
                         INPUT Creditos.Agencia,INPUT W_Agencia, INPUT Creditos.Agencia, INPUT W_Usuario, INPUT 0, 
                         INPUT Int_Mor).
     RUN Gra_MovContable(INPUT Ahorros.Agencia, INPUT Cbte,INPUT CtaAuxLiq, INPUT TODAY, 
                         INPUT "Destino Liq. de Intereses",INPUT W_Usuario,INPUT Int_Mor, INPUT 0, 
                         INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
     RUN Gra_MovContable(INPUT Ahorros.Agencia, INPUT Cbte,INPUT CtaSyA, INPUT TODAY, 
                         INPUT "Destino Liq. de Intereses",INPUT W_Usuario,INPUT 0, INPUT Int_Mor, 
                         INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
     RUN Gra_MovContable(INPUT Creditos.Agencia, INPUT Cbte,INPUT CtaSyA, INPUT TODAY, 
                         INPUT "Destino Liq. de Intereses",INPUT W_Usuario,INPUT Int_Mor, INPUT 0,
                         INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).                    
     RUN Gra_MovContable(INPUT Creditos.Agencia, INPUT Cbte,INPUT CtaLiqMor, INPUT TODAY, 
                         INPUT "Destino Liq. de Intereses",INPUT W_Usuario,INPUT 0, INPUT Int_Mor, 
                         INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
  END.
  /* Contabilizar los Intereses Anticipados. */
  IF Int_Ant GT 0 THEN DO:
     RUN Gra_MovCreditos(INPUT 020101007, INPUT credito.cod_credito, INPUT Creditos.Pagare, INPUT Docto,
                         INPUT Creditos.Agencia,INPUT W_Agencia, INPUT Creditos.Agencia, INPUT W_Usuario, INPUT 0, 
                         INPUT Int_Ant).
     RUN Gra_MovContable(INPUT Ahorros.Agencia, INPUT Cbte,INPUT CtaAuxLiq, INPUT TODAY, 
                         INPUT "Destino Liq. de Intereses",INPUT W_Usuario,INPUT Int_Ant, INPUT 0,
                         INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
     RUN Gra_MovContable(INPUT Ahorros.Agencia, INPUT Cbte,INPUT CtaSyA, INPUT TODAY, 
                         INPUT "Destino Liq. de Intereses",INPUT W_Usuario,INPUT 0, INPUT Int_Ant, 
                         INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
     RUN Gra_MovContable(INPUT Creditos.Agencia, INPUT Cbte,INPUT CtaSyA, INPUT TODAY, 
                         INPUT "Destino Liq. de Intereses",INPUT W_Usuario,INPUT Int_Ant, INPUT 0,
                         INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).                    
     RUN Gra_MovContable(INPUT Creditos.Agencia, INPUT Cbte,INPUT CtaIntAnt, INPUT TODAY, 
                         INPUT "Destino Liq. de Intereses",INPUT W_Usuario,INPUT 0, INPUT Int_Ant, 
                         INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
  END.
  /* Contabilizar el Difícil Cobro. */
  IF Int_Dco GT 0 THEN DO:
     RUN Gra_MovCreditos(INPUT 020101008, INPUT credito.cod_credito, INPUT Creditos.Pagare, INPUT Docto,
                         INPUT Creditos.Agencia,INPUT W_Agencia, INPUT Creditos.Agencia, INPUT W_Usuario, INPUT 0, 
                         INPUT Int_Dco).
     RUN Gra_MovContable(INPUT Ahorros.Agencia, INPUT Cbte,INPUT CtaAuxLiq, INPUT TODAY, 
                         INPUT "Destino Liq. de Intereses",INPUT W_Usuario,INPUT Int_Dco, INPUT 0,
                         INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
     RUN Gra_MovContable(INPUT Ahorros.Agencia, INPUT Cbte,INPUT CtaSyA, INPUT TODAY, 
                         INPUT "Destino Liq. de Intereses",INPUT W_Usuario,INPUT 0, INPUT Int_Dco, 
                         INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
     RUN Gra_MovContable(INPUT Creditos.Agencia, INPUT Cbte,INPUT CtaSyA, INPUT TODAY, 
                         INPUT "Destino Liq. de Intereses",INPUT W_Usuario,INPUT Int_Dco, INPUT 0,
                         INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).                    
     RUN Gra_MovContable(INPUT Creditos.Agencia, INPUT Cbte,INPUT CtaLiqDco, INPUT TODAY, 
                         INPUT "Destino Liq. de Intereses",INPUT W_Usuario,INPUT 0, INPUT Int_Dco, 
                         INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Des_Int_Ctas_Ahorro W-Win 
PROCEDURE Des_Int_Ctas_Ahorro :
/*------------------------------------------------------------------------------
  Observaciones : Desembolsar Intereses del Cheque en Cuentas de ahorro.       
------------------------------------------------------------------------------*/
  DEFINE VAR P_Per    AS INTEGER.
  DEFINE VAR P_Valor  AS INTEGER.
  DEFINE VAR P_Gracia AS INTEGER.
  DEFINE VAR SW       AS LOGICAL INITIAL FALSE.
  
  FIND Varios WHERE Varios.Tipo   EQ 8 
              AND   Varios.Codigo EQ 14 NO-LOCK NO-ERROR.
  IF AVAILABLE(Varios) THEN
     ASSIGN Cbte = Varios.Comprobante.
  ELSE DO:
    RUN MostrarMensaje (INPUT 143,OUTPUT W_Rpta).
    RETURN ERROR.
  END.
  RUN Cbte_Agencia IN W_Manija (INPUT  Cbte,INPUT  Ahorros.Agencia,
                                OUTPUT Docto, OUTPUT W_Rpta).
  IF W_Rpta THEN DO:
     RUN MostrarMensaje IN W_Manija (INPUT 144, OUTPUT W_Eleccion).
     RETURN ERROR.
  END.
  FOR EACH Tmp_ProAhorros WHERE Tmp_ProAhorros.Cod_ahorro EQ Ahorros.Pro_Destino
                          AND   Tmp_ProAhorros.Estado       EQ 1 EXCLUSIVE-LOCK,
      EACH Tmp_Ahorros WHERE Tmp_Ahorros.Cod_ahorro EQ Tmp_ProAhorros.Cod_Ahorro
                       AND   Tmp_Ahorros.Cue_Ahorros  EQ Ahorros.Cue_Destino EXCLUSIVE-LOCK:
      ASSIGN SW = TRUE.
      /* Devolver la Contabilización que Hace el proceso de Liquidación */
      RUN Buscar_Ctas_Liq(INPUT 1,INPUT ahorros.cod_ahorro) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
         RUN MostrarMensaje IN W_Manija (INPUT 66,OUTPUT W_Rpta).
         RETURN ERROR.
      END.
      RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT LiqIntCR, INPUT TODAY, 
                          INPUT "Devolución Liq. Int.",INPUT W_Usuario,INPUT Che_Transito.Int_Generado, INPUT 0,
                          INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
      RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT LiqIntDB, INPUT TODAY, 
                          INPUT "Devolución Liq. Int.",INPUT W_Usuario,INPUT 0, INPUT Che_Transito.Int_Generado, 
                          INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
      RUN Buscar_Ctas_Liq(INPUT 1,INPUT Tmp_ahorros.cod_ahorro) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
         RUN MostrarMensaje IN W_Manija (INPUT 66,OUTPUT W_Rpta).
         RETURN ERROR.
      END.
      RUN Buscar_Cuenta(INPUT 1,INPUT Tmp_ahorros.cod_ahorro,INPUT Tmp_Ahorros.Plazo,INPUT Tmp_Ahorros.Nit,OUTPUT CtaCble) NO-ERROR.
      IF ERROR-STATUS:ERROR OR CtaCble EQ "" THEN DO:
         RUN MostrarMensaje IN W_Manija (INPUT 66,OUTPUT W_Rpta).
         RETURN ERROR.
      END.
      IF Ahorros.Agencia EQ Ahorros.Agencia_Destino THEN DO:
         IF Tmp_Ahorros.Sdo_Disponible GT 0 THEN DO:
            ASSIGN Tmp_Ahorros.Sdo_Disponible = Tmp_Ahorros.Sdo_Disponible + W_Int.
            RUN Gra_MovAhorros(INPUT 010101003,INPUT Tmp_ahorros.cod_ahorro, INPUT Tmp_Ahorros.Cue_Ahorros, INPUT Docto,
                               INPUT Ahorros.Agencia_Destino,INPUT W_Agencia, INPUT Ahorros.Agencia_Destino, INPUT W_Usuario,INPUT 0,
                               INPUT W_Int).
            RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT LiqIntDB, INPUT TODAY, 
                                INPUT "Destino Liq. de Intereses",INPUT W_Usuario,INPUT W_Int, INPUT 0, 
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT CtaCble, INPUT TODAY, 
                                INPUT "Destino Liq. de Intereses",INPUT W_Usuario,INPUT 0, INPUT W_Int, 
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
         END.
         ELSE DO:
            RUN Des_Sobregiro NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
               RETURN ERROR.
         END.
      END.
      ELSE DO:
         IF Tmp_Ahorros.Sdo_Disponible GT 0 THEN DO:
            ASSIGN Tmp_Ahorros.Sdo_Disponible = Tmp_Ahorros.Sdo_Disponible + W_Int.
            RUN Gra_MovAhorros(INPUT 010101003,INPUT Tmp_ahorros.cod_ahorro, INPUT Tmp_Ahorros.Cue_Ahorros, INPUT Docto,
                               INPUT Ahorros.Agencia_Destino,INPUT W_Agencia, INPUT Ahorros.Agencia_Destino, INPUT W_Usuario,INPUT 0,
                               INPUT W_Int).
            RUN Gra_MovContable(INPUT Ahorros.Agencia, INPUT Cbte,INPUT LiqIntDB, INPUT TODAY, 
                                INPUT "Destino Liq. de Intereses",INPUT W_Usuario,INPUT W_Int, INPUT 0,
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            RUN Gra_MovContable(INPUT Ahorros.Agencia, INPUT Cbte,INPUT CtaSyA, INPUT TODAY, 
                                INPUT "Destino Liq. de Intereses",INPUT W_Usuario,INPUT 0, INPUT W_Int, 
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            RUN Gra_MovContable(INPUT Ahorros.Agencia_Destino, INPUT Cbte,INPUT CtaSyA, INPUT TODAY, 
                                INPUT "Devolución Liq. Int.",INPUT W_Usuario,INPUT W_Int, INPUT 0,
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            RUN Gra_MovContable(INPUT Ahorros.Agencia_Destino, INPUT Cbte,INPUT CtaCble, INPUT TODAY, 
                                INPUT "Devolución Liq. Int.",INPUT W_Usuario,INPUT 0, INPUT W_Int, 
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
         END.
         ELSE DO:
            RUN Des_SobregiroSYA NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
               RETURN ERROR.
         END.
      END.
  END.
  IF SW EQ FALSE THEN DO:
  /* Devolver la Contabilización que Hace el proceso de Liquidación */
     RUN Buscar_Ctas_Liq(INPUT 1,INPUT ahorros.cod_ahorro) NO-ERROR.
     IF ERROR-STATUS:ERROR THEN DO:
        RUN MostrarMensaje IN W_Manija (INPUT 66,OUTPUT W_Rpta).
        RETURN ERROR.
     END.
     RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT LiqIntCR, INPUT TODAY, 
                         INPUT "Devolución Liq. Int.",INPUT W_Usuario,INPUT Che_Transito.Int_Generado, INPUT 0,
                         INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
     RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT LiqIntDB, INPUT TODAY, 
                         INPUT "Devolución Liq. Int.",INPUT W_Usuario,INPUT 0, INPUT Che_Transito.Int_Generado, 
                         INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
     RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT LiqIntDB, INPUT TODAY, 
                         INPUT "Destino Liq. de Intereses SYA",INPUT W_Usuario,INPUT W_Int, INPUT 0,
                         INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
     RUN Gra_MovContable(INPUT Ahorros.Agencia_Destino, INPUT Cbte,INPUT CtaSyA, INPUT TODAY, 
                         INPUT "Destino Liq. de Intereses SYA",INPUT W_Usuario,INPUT 0, INPUT W_Int, 
                         INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
  END.
  RELEASE Tmp_ProAhorros.
  RELEASE Tmp_Ahorros.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Des_Int_Ctas_Credito W-Win 
PROCEDURE Des_Int_Ctas_Credito :
/*------------------------------------------------------------------------------
  Observaciones : Distribuir los Intereses en el Credito.       
  ------------------------------------------------------------------------------*/ 
  DEFINE VAR SW           AS   LOGICAL.
  DEFINE VAR W_Difern     LIKE Ahorros.Sdo_Disponible.
  
  ASSIGN Int_Mor = 0 Int_Dco = 0 Int_Cor  = 0
         Int_Ant = 0 Capital = 0 W_Devuel = 0.
  
  FIND Varios WHERE Varios.Tipo   EQ 8 
              AND   Varios.Codigo EQ 14 NO-LOCK NO-ERROR.
  IF AVAILABLE(Varios) THEN
     ASSIGN Cbte = Varios.Comprobante.
  ELSE DO:
    RUN MostrarMensaje (INPUT 143,OUTPUT W_Rpta).
    RETURN ERROR.
  END.
  RUN Cbte_Agencia IN W_Manija (INPUT  Cbte,INPUT  Ahorros.Agencia,
                                OUTPUT Docto, OUTPUT W_Rpta).
  IF W_Rpta THEN DO:
     RUN MostrarMensaje IN W_Manija (INPUT 144, OUTPUT W_Eleccion).
     RETURN ERROR.
  END.
  FOR FIRST Pro_Creditos WHERE Pro_credito.cod_credito EQ Ahorros.Pro_Destino
                         AND   Pro_Creditos.Estado       EQ 1 EXCLUSIVE-LOCK,
      FIRST Creditos WHERE credito.cod_credito EQ Pro_credito.cod_credito
                     AND   Creditos.Pagare       EQ Ahorros.Cue_Destino EXCLUSIVE-LOCK:
      ASSIGN SW = TRUE W_Devuel = 0.
      RUN P-RutDis.R (INPUT TRUE,INPUT Creditos.Agencia,INPUT Creditos.Pagare,INPUT credito.cod_credito,
                      INPUT Pro_Creditos.Id_PagParcial,INPUT W_Int,OUTPUT Int_Mor,OUTPUT Int_Dco,
                      OUTPUT Int_Cor,OUTPUT Int_Ant,OUTPUT Capital,OUTPUT W_Devuel).
      IF W_Devuel GT 0 THEN DO:
         ASSIGN Ahorros.Int_Pagar = Ahorros.Int_Pagar + W_Devuel
                W_Difern = Che_Transito.Int_Generado - W_Devuel.
      END.
      ELSE DO:
         ASSIGN W_Difern = Che_Transito.Int_Generado.
      END.
      /* Devolver la Contabilización que Hace el proceso de Liquidación */
      RUN Buscar_Ctas_Liq(INPUT 1,INPUT ahorros.cod_ahorro) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
         RUN MostrarMensaje IN W_Manija (INPUT 66,OUTPUT W_Rpta).
         RETURN ERROR.
      END.
      RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT LiqIntCR, INPUT TODAY, 
                          INPUT "Devolución Liq. Int.",INPUT W_Usuario,INPUT W_Difern, INPUT 0,
                          INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
      RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT LiqIntDB, INPUT TODAY, 
                          INPUT "Devolución Liq. Int.",INPUT W_Usuario,INPUT 0, INPUT W_Difern, 
                          INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
      ASSIGN CtaAuxLiq = LiqIntDB.
      RUN Buscar_Cuenta(INPUT 2,INPUT credito.cod_credito,INPUT Creditos.Plazo,INPUT Creditos.Nit,OUTPUT CtaCble) NO-ERROR.
      IF ERROR-STATUS:ERROR OR CtaCble EQ "" THEN DO:
         RUN MostrarMensaje IN W_Manija (INPUT 66,OUTPUT W_Rpta).
         RETURN ERROR.
      END.
      RUN Buscar_Ctas_Liq(INPUT 2,INPUT credito.cod_credito) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
         RUN MostrarMensaje IN W_Manija (INPUT 66,OUTPUT W_Rpta).
         RETURN ERROR.
      END.
      IF Ahorros.Agencia EQ Creditos.Agencia THEN DO:
         /* Contabiliza el Abono al Capital */
         IF Capital GT 0 THEN DO:
            RUN Gra_MovCreditos(INPUT 020101004, INPUT credito.cod_credito, INPUT Creditos.Pagare, INPUT Docto,
                                INPUT Creditos.Agencia,INPUT W_Agencia, INPUT Creditos.Agencia, INPUT W_Usuario, INPUT 0, 
                                INPUT Capital).
            RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT CtaAuxLiq, INPUT TODAY, 
                                INPUT "Destino Liq. de Intereses",INPUT W_Usuario,INPUT Capital, INPUT 0,
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT CtaCble, INPUT TODAY, 
                                INPUT "Destino Liq. de Intereses",INPUT W_Usuario,INPUT 0, INPUT Capital, 
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
         END.
         /* Contabilizar el Abono a Intereses Corrirntes. */
         IF Int_Cor GT 0 THEN DO:
            RUN Gra_MovCreditos(INPUT 020101005, INPUT credito.cod_credito, INPUT Creditos.Pagare, INPUT Docto,
                                INPUT Creditos.Agencia,INPUT W_Agencia, INPUT Creditos.Agencia, INPUT W_Usuario, INPUT 0, 
                                INPUT Int_Cor).
            RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT CtaAuxLiq, INPUT TODAY, 
                                INPUT "Destino Liq. de Intereses",INPUT W_Usuario,INPUT Int_Cor, INPUT 0,
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT LiqIntCR, INPUT TODAY, 
                                INPUT "Destino Liq. de Intereses",INPUT W_Usuario,INPUT 0, INPUT Int_Cor, 
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
         END.
         /* Contabilizar Los Intereses de Mora */
         IF Int_Mor GT 0 THEN DO:
            RUN Gra_MovCreditos(INPUT 020101006, INPUT credito.cod_credito, INPUT Creditos.Pagare, INPUT Docto,
                                INPUT Creditos.Agencia,INPUT W_Agencia, INPUT Creditos.Agencia, INPUT W_Usuario, INPUT 0, 
                                INPUT Int_Mor).
            RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT CtaAuxLiq, INPUT TODAY, 
                                INPUT "Destino Liq. de Intereses",INPUT W_Usuario,INPUT Int_Mor, INPUT 0,
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT CtaLiqMor, INPUT TODAY, 
                                INPUT "Destino Liq. de Intereses",INPUT W_Usuario,INPUT 0, INPUT Int_Mor, 
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
         END.
         /* Contabilizar los Intereses Anticipados. */
         IF Int_Ant GT 0 THEN DO:
            RUN Gra_MovCreditos(INPUT 020101007, INPUT credito.cod_credito, INPUT Creditos.Pagare, INPUT Docto,
                                INPUT Creditos.Agencia,INPUT W_Agencia, INPUT Creditos.Agencia, INPUT W_Usuario, INPUT 0, 
                                INPUT Int_Ant).
            RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT CtaAuxLiq, INPUT TODAY, 
                                INPUT "Destino Liq. de Intereses",INPUT W_Usuario,INPUT Int_Ant, INPUT 0,
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT CtaIntAnt, INPUT TODAY, 
                                INPUT "Destino Liq. de Intereses",INPUT W_Usuario,INPUT 0, INPUT Int_Ant, 
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
         END.
         /* Contabilizar el Difícil Cobro. */
         IF Int_Dco GT 0 THEN DO:
            RUN Gra_MovCreditos(INPUT 020101008, INPUT credito.cod_credito, INPUT Creditos.Pagare, INPUT Docto,
                                INPUT Creditos.Agencia,INPUT W_Agencia, INPUT Creditos.Agencia, INPUT W_Usuario, INPUT 0, 
                                INPUT Int_Dco).
            RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT CtaAuxLiq, INPUT TODAY, 
                                INPUT "Destino Liq. de Intereses",INPUT W_Usuario,INPUT Int_Dco, INPUT 0,
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT CtaLiqDco, INPUT TODAY, 
                                INPUT "Destino Liq. de Intereses",INPUT W_Usuario,INPUT 0, INPUT Int_Dco, 
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
         END.
      END.
      ELSE DO:
         RUN Des_Int_Cre_Sya.
      END.
  END.
  IF SW EQ FALSE THEN DO:
  /* Devolver la Contabilización que Hace el proceso de Liquidación */
      RUN Buscar_Ctas_Liq(INPUT 1,INPUT ahorros.cod_ahorro) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
         RUN MostrarMensaje IN W_Manija (INPUT 66,OUTPUT W_Rpta).
         RETURN ERROR.
      END.
      RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT LiqIntCR, INPUT TODAY, 
                          INPUT "Devolución Liq. Int.",INPUT W_Usuario,INPUT Che_Transito.Int_Generado, INPUT 0,
                          INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
      RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT LiqIntDB, INPUT TODAY, 
                          INPUT "Devolución Liq. Int.",INPUT W_Usuario,INPUT 0, INPUT Che_Transito.Int_Generado, 
                          INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
      RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT LiqIntDB, INPUT TODAY, 
                          INPUT "Destino Liq. de Intereses SYA",INPUT W_Usuario,INPUT W_Int, INPUT 0,
                          INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
      RUN Gra_MovContable(INPUT Ahorros.Agencia_Destino, INPUT Cbte,INPUT CtaSyA, INPUT TODAY, 
                          INPUT "Destino Liq. de Intereses SYA",INPUT W_Usuario,INPUT 0, INPUT W_Int, 
                          INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
  END.
  RELEASE Pro_Creditos.
  RELEASE Creditos.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Des_Sobregiro W-Win 
PROCEDURE Des_Sobregiro :
/*------------------------------------------------------------------------------
  OBSERVACIONES : Consigna Sobregiro en Efectivo.       
------------------------------------------------------------------------------*/
   DEFINE VAR T_Valor LIKE Creditos.Sdo_Capital INITIAL 0.
   DEFINE VAR T_Difer LIKE Creditos.Sdo_Capital INITIAL 0.
   DEFINE VAR T_Inter LIKE Creditos.Sdo_Capital INITIAL 0.
   
   ASSIGN T_Valor = W_Int.
   FIND Creditos WHERE Creditos.Agencia      EQ Tmp_Ahorros.Agencia
                 AND   credito.cod_credito EQ Tmp_ProAhorros.ProCre_Asociado
                 AND   Creditos.Pagare       EQ Tmp_Ahorros.Cue_Ahorros EXCLUSIVE-LOCK NO-ERROR.
   IF AVAILABLE(Creditos) THEN DO:
      RUN Cta_Sobregiro(INPUT 2,INPUT credito.cod_credito,INPUT Creditos.Nit,
                        OUTPUT W_LiqIntDb,OUTPUT W_LiqIntCr,OUTPUT W_LiqMor,
                        OUTPUT W_DifCdb,OUTPUT W_DifCcr).
      IF ERROR-STATUS:ERROR THEN DO:
         RUN MostrarMensaje IN W_Manija (INPUT 66,OUTPUT W_Error).
         RETURN ERROR.
      END.
      IF Creditos.Int_MorCobrar GT 0 AND T_Valor NE 0 THEN DO:
         IF T_Valor GT Creditos.Int_MorCobrar THEN DO:
            RUN Gra_MovAhorros(INPUT 010101006, INPUT Tmp_ahorros.cod_ahorro, INPUT Tmp_Ahorros.Cue_Ahorros, INPUT Che_Transito.Cheque,
                               INPUT Che_Transito.Agencia,INPUT W_Agencia, INPUT Che_Transito.Agencia, INPUT W_Usuario, 
                               INPUT 0,INPUT Creditos.Int_MorCobrar).
            RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT CtaCble, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT Creditos.Int_MorCobrar, INPUT 0,
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT W_LiqMor, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT 0, INPUT Creditos.Int_MorCobrar, 
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            ASSIGN T_Valor = T_Valor - Creditos.Int_MorCobrar
                   T_Inter = T_Inter + Creditos.Int_MorCobrar
                   Creditos.Int_MorCobrar = 0.
         END.
         ELSE DO:
            RUN Gra_MovAhorros(INPUT 010101006, INPUT Tmp_ahorros.cod_ahorro, INPUT Tmp_Ahorros.Cue_Ahorros, INPUT Che_Transito.Cheque,
                               INPUT Che_Transito.Agencia,INPUT W_Agencia, INPUT Che_Transito.Agencia, INPUT W_Usuario, 
                               INPUT 0,INPUT T_Valor).
            RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT CtaCble, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT T_Valor, INPUT 0,
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT W_LiqMor, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT 0, INPUT T_Valor, 
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            ASSIGN Creditos.Int_MorCobrar = Creditos.Int_MorCobrar - T_Valor
                   T_Inter = T_Inter + T_Valor
                   T_Valor = 0.
         END.
      END.
      IF Creditos.Int_difCobro GT 0 AND T_Valor NE 0 THEN DO:
         FIND Operacion WHERE Operacion.Cod_Operacion EQ 010101008 NO-LOCK NO-ERROR.
         IF AVAILABLE(Operacion) AND Operacion.Comprobante NE 0 THEN DO:
            ASSIGN Cbte = Operacion.Comprobante.
         END.
         ELSE DO:
            RUN MostrarMensaje IN W_Manija (INPUT 143,OUTPUT W_Error).
            RETURN ERROR.
         END.
         RUN Cbte_Agencia IN W_Manija (INPUT  Cbte,  INPUT  Ahorros.Agencia,
                                       OUTPUT Docto, OUTPUT W_Error).
         IF W_Error THEN DO:
            RUN MostrarMensaje IN W_Manija (INPUT 144, OUTPUT W_Eleccion).
            RETURN ERROR.
         END.
         IF T_Valor GT Creditos.Int_difCobro THEN DO:
            RUN Gra_MovAhorros(INPUT 010101008, INPUT Tmp_ahorros.cod_ahorro, INPUT Tmp_Ahorros.Cue_Ahorros, INPUT Che_Transito.Cheque,
                               INPUT Che_Transito.Agencia,INPUT W_Agencia, INPUT Che_Transito.Agencia, INPUT W_Usuario, 
                               INPUT 0,INPUT Creditos.Int_difCobro).
            RUN Gra_MovContable(INPUT Creditos.Agencia, INPUT Cbte,INPUT W_DifCdb,  INPUT W_Fecha, 
                                INPUT "Cerrar Cuenta Dificil Cobro",INPUT W_Usuario, 
                                INPUT 0, INPUT Creditos.Int_difCobro, 
                                INPUT 999,       INPUT Docto,    
                                INPUT TODAY, INPUT TIME).
            RUN Gra_MovContable(INPUT Creditos.Agencia, INPUT Cbte,INPUT W_DifCcr,  INPUT W_Fecha, 
                                INPUT "Cerrar Cuenta Dificil Cobro",INPUT W_Usuario,   
                                INPUT Creditos.Int_difCobro, INPUT 0,    INPUT 999,         INPUT Docto,    
                                INPUT TODAY, INPUT TIME).
            RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT CtaCble, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT Creditos.Int_difCobro, INPUT 0,
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT W_LiqIntCr, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT 0, INPUT Creditos.Int_difCobro, 
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            ASSIGN T_Valor = T_Valor - Creditos.Int_difCobro
                   T_Inter = T_Inter + Creditos.Int_difCobro
                   Creditos.Int_difCobro = 0.
         END.
         ELSE DO:
            RUN Gra_MovAhorros(INPUT 010101008, INPUT Tmp_ahorros.cod_ahorro, INPUT Tmp_Ahorros.Cue_Ahorros, INPUT Che_Transito.Cheque,
                               INPUT Che_Transito.Agencia,INPUT W_Agencia, INPUT Che_Transito.Agencia, INPUT W_Usuario, 
                               INPUT 0,INPUT T_Valor).
            RUN Gra_MovContable(INPUT Creditos.Agencia, INPUT Cbte,INPUT W_DifCdb,  INPUT W_Fecha, 
                                INPUT "Cerrar Cuenta Dificil Cobro",INPUT W_Usuario,   
                                INPUT 0, INPUT T_Valor,    INPUT 999,       INPUT Docto,    
                                INPUT TODAY, INPUT TIME).
            RUN Gra_MovContable(INPUT Creditos.Agencia, INPUT Cbte,INPUT W_DifCcr,  INPUT W_Fecha, 
                                INPUT "Cerrar Cuenta Dificil Cobro",INPUT W_Usuario,   
                                INPUT T_Valor, INPUT 0,   INPUT 999,         INPUT Docto,    
                                INPUT TODAY, INPUT TIME).
            RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT CtaCble, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT T_Valor, INPUT 0,
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT W_LiqIntCr, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT 0, INPUT T_Valor, 
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            ASSIGN Creditos.Int_difCobro = Creditos.Int_difCobro - T_Valor
                   T_Inter = T_Inter + T_Valor
                   T_Valor = 0.
         END.
      END.
      IF Creditos.Int_MorCobrar GT 0 AND T_Valor NE 0 THEN DO:
         IF T_Valor GT Creditos.Int_MorCobrar THEN DO:
            RUN Gra_MovAhorros(INPUT 010101007, INPUT Tmp_ahorros.cod_ahorro, INPUT Tmp_Ahorros.Cue_Ahorros, INPUT Che_Transito.Cheque,
                               INPUT Che_Transito.Agencia,INPUT W_Agencia, INPUT Che_Transito.Agencia, INPUT W_Usuario, 
                               INPUT 0,INPUT Creditos.Int_MorCobrar).
            RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT CtaCble, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT Creditos.Int_MorCobrar, INPUT 0,
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT W_LiqIntDb, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT 0, INPUT Creditos.Int_MorCobrar, 
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            ASSIGN T_Valor = T_Valor - Creditos.Int_MorCobrar
                   T_Inter = T_Inter + Creditos.Int_MorCobrar
                   Creditos.Int_MorCobrar = 0.
         END.
         ELSE DO:
            RUN Gra_MovAhorros(INPUT 010101007, INPUT Tmp_ahorros.cod_ahorro, INPUT Tmp_Ahorros.Cue_Ahorros, INPUT Che_Transito.Cheque,
                               INPUT Che_Transito.Agencia,INPUT W_Agencia, INPUT Che_Transito.Agencia, INPUT W_Usuario, 
                               INPUT 0,INPUT T_Valor).
            RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT CtaCble, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT T_Valor, INPUT 0,
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT W_LiqIntDb, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT 0, INPUT T_Valor, 
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            ASSIGN Creditos.Int_MorCobrar = Creditos.Int_morCobrar - T_Valor
                   T_Inter = T_Inter + T_Valor
                   T_Valor = 0.
         END.
      END.
      IF Creditos.Int_Corrientes GT 0 AND T_Valor NE 0 THEN DO:
         IF T_Valor GT Creditos.Int_Corrientes THEN DO:
            RUN Gra_MovAhorros(INPUT 010101005, INPUT Tmp_ahorros.cod_ahorro, INPUT Tmp_Ahorros.Cue_Ahorros, INPUT Che_Transito.Cheque,
                               INPUT Che_Transito.Agencia,INPUT W_Agencia, INPUT Che_Transito.Agencia, INPUT W_Usuario, 
                               INPUT 0,INPUT Creditos.Int_Corrientes).
            RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT CtaCble, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT Creditos.Int_Corrientes, INPUT 0,
                                INPUT 999,INPUT Docto,
                                INPUT TODAY,INPUT TIME).
            RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT W_LiqIntDb, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT 0, INPUT Creditos.Int_Corrientes, 
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            ASSIGN T_Valor = T_Valor - Creditos.Int_Corrientes
                   T_Inter = T_Inter + Creditos.Int_Corrientes
                   Creditos.Int_Corrientes = 0.
         END.
         ELSE DO:
            RUN Gra_MovAhorros(INPUT 010101005, INPUT Tmp_ahorros.cod_ahorro, INPUT Tmp_Ahorros.Cue_Ahorros, INPUT Che_Transito.Cheque,
                               INPUT Che_Transito.Agencia,INPUT W_Agencia, INPUT Che_Transito.Agencia, INPUT W_Usuario, 
                               INPUT 0,INPUT T_Valor).
            RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT CtaCble, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT T_Valor, INPUT 0,
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT W_LiqIntDb, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT 0, INPUT T_Valor, 
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            ASSIGN Creditos.Int_Corrientes = Creditos.Int_Corrientes - T_Valor
                   T_Inter = T_Inter + T_Valor
                   T_Valor = 0.
         END.
      END.
      IF Creditos.Sdo_Capital GT 0 AND T_Valor NE 0 THEN DO:
         IF T_Valor GT Creditos.Sdo_Capital THEN DO:
            ASSIGN T_Difer = T_Valor - Creditos.Sdo_Capital.
            RUN Buscar_Cuenta(INPUT 2,INPUT credito.cod_credito,INPUT Creditos.Plazo,INPUT Ahorros.Nit,OUTPUT AuxCta) NO-ERROR.
            IF ERROR-STATUS:ERROR OR AuxCta EQ "" THEN DO:
               RUN MostrarMensaje IN W_Manija (INPUT 66,OUTPUT W_Error).
               RETURN ERROR.
            END.
            RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT CtaCble, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT Creditos.Sdo_Capital, INPUT 0,
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT AuxCta, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT 0, INPUT Creditos.Sdo_Capital, 
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            ASSIGN Creditos.Sdo_Capital = 0.
         END.
         ELSE DO:
            RUN Buscar_Cuenta(INPUT 2,INPUT credito.cod_credito,INPUT Creditos.Plazo,INPUT Ahorros.Nit,OUTPUT AuxCta) NO-ERROR.
            IF ERROR-STATUS:ERROR OR AuxCta EQ "" THEN DO:
               RUN MostrarMensaje IN W_Manija (INPUT 66,OUTPUT W_Error).
               RETURN ERROR.
            END.
            RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT CtaCble, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT T_Valor, INPUT 0,
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT AuxCta, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT 0, INPUT T_Valor, 
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            ASSIGN Creditos.Sdo_Capital = Creditos.Sdo_Capital - T_Valor.
         END.
      END.
      ASSIGN Tmp_Ahorros.Sdo_Canje          = Tmp_Ahorros.Sdo_Canje - Che_Transito.Valor
             Tmp_Ahorros.Sdo_Disponible     = Tmp_Ahorros.Sdo_Disponible + Che_Transito.Valor + W_Int - T_Inter - W_ValCanje
             Tmp_Ahorros.Fec_Ulttransaccion = TODAY
             Tmp_Ahorros.Sal_Intpagados     = Tmp_Ahorros.Sal_Intpagados + W_Int.
   END.
   ELSE DO:
      MESSAGE "No Hay Producto de Crédito Asociado a la Cuenta."
      VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
      TITLE "Error en Taquilla.".
      RETURN ERROR.
   END.
   RELEASE Creditos.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Des_Sobregirosya W-Win 
PROCEDURE Des_Sobregirosya :
/*------------------------------------------------------------------------------
  OBSERVACIONES : Consigna Sobregiro en Efectivo.       
------------------------------------------------------------------------------*/
   
   ASSIGN T_Valor = W_Int.
   FIND Creditos WHERE Creditos.Agencia      EQ Tmp_Ahorros.Agencia
                 AND   credito.cod_credito EQ Tmp_ProAhorros.ProCre_Asociado
                 AND   Creditos.Pagare       EQ Tmp_Ahorros.Cue_Ahorros EXCLUSIVE-LOCK NO-ERROR.
   IF AVAILABLE(Creditos) THEN DO:
      RUN Cta_Sobregiro(INPUT 2,INPUT credito.cod_credito,INPUT Creditos.Nit,
                        OUTPUT W_LiqIntDb,OUTPUT W_LiqIntCr,OUTPUT W_LiqMor,
                        OUTPUT W_DifCdb,OUTPUT W_DifCcr).
      IF ERROR-STATUS:ERROR THEN DO:
         RUN MostrarMensaje IN W_Manija (INPUT 66,OUTPUT W_Error).
         RETURN ERROR.
      END.
      IF Creditos.Int_MorCobrar GT 0 AND T_Valor NE 0 THEN DO:
         IF T_Valor GT Creditos.Int_MorCobrar THEN DO:
            RUN Gra_MovAhorros(INPUT 010101006, INPUT Tmp_ahorros.cod_ahorro, INPUT Tmp_Ahorros.Cue_Ahorros, INPUT Che_Transito.Cheque,
                               INPUT Che_Transito.Agencia,INPUT W_Agencia, INPUT Che_Transito.Agencia, INPUT W_Usuario, 
                               INPUT 0,INPUT Creditos.Int_MorCobrar).
            RUN Gra_MovContable(INPUT Tmp_Ahorros.Agencia, INPUT Cbte,INPUT CtaCble, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT Creditos.Int_MorCobrar, INPUT 0,
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            RUN Gra_MovContable(INPUT Tmp_Ahorros.Agencia, INPUT Cbte,INPUT CtaSyA, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT 0, INPUT Creditos.Int_MorCobrar, 
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT CtaSyA, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT Creditos.Int_MorCobrar, INPUT 0,
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT W_LiqMor, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT 0, INPUT Creditos.Int_MorCobrar, 
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            ASSIGN T_Valor = T_Valor - Creditos.Int_MorCobrar
                   T_Inter = T_Inter + Creditos.Int_MorCobrar
                   Creditos.Int_MorCobrar = 0.
         END.
         ELSE DO:
            RUN Gra_MovAhorros(INPUT 010101006, INPUT Tmp_ahorros.cod_ahorro, INPUT Tmp_Ahorros.Cue_Ahorros, INPUT Che_Transito.Cheque,
                               INPUT Che_Transito.Agencia,INPUT W_Agencia, INPUT Che_Transito.Agencia, INPUT W_Usuario, 
                               INPUT 0,INPUT T_Valor).
            RUN Gra_MovContable(INPUT Tmp_Ahorros.Agencia, INPUT Cbte,INPUT CtaCble, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT T_Valor, INPUT 0,
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            RUN Gra_MovContable(INPUT Tmp_Ahorros.Agencia, INPUT Cbte,INPUT CtaSyA, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT 0, INPUT T_Valor, 
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT CtaSyA, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT T_Valor, INPUT 0,
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT W_LiqMor, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT 0, INPUT T_Valor, 
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            ASSIGN Creditos.Int_MorCobrar = Creditos.Int_MorCobrar - T_Valor
                   T_Inter = T_Inter + T_Valor
                   T_Valor = 0.
         END.
      END.
      IF Creditos.Int_difCobro GT 0 AND T_Valor NE 0 THEN DO:
         FIND Operacion WHERE Operacion.Cod_Operacion EQ 010101008 NO-LOCK NO-ERROR.
         IF AVAILABLE(Operacion) AND Operacion.Comprobante NE 0 THEN DO:
            ASSIGN Cbte = Operacion.Comprobante.
         END.
         ELSE DO:
            RUN MostrarMensaje IN W_Manija (INPUT 143,OUTPUT W_Error).
            RETURN ERROR.
         END.
         RUN Cbte_Agencia IN W_Manija (INPUT  Cbte,  INPUT  Ahorros.Agencia,
                                       OUTPUT Docto, OUTPUT W_Error).
         IF W_Error THEN DO:
            RUN MostrarMensaje IN W_Manija (INPUT 144, OUTPUT W_Eleccion).
            RETURN ERROR.
         END.
         IF T_Valor GT Creditos.Int_difCobro THEN DO:
            RUN Gra_MovAhorros(INPUT 010101008, INPUT Tmp_ahorros.cod_ahorro, INPUT Tmp_Ahorros.Cue_Ahorros, INPUT Che_Transito.Cheque,
                               INPUT Che_Transito.Agencia,INPUT W_Agencia, INPUT Che_Transito.Agencia, INPUT W_Usuario, 
                               INPUT 0,INPUT Creditos.Int_difCobro).
            RUN Gra_MovContable(INPUT Creditos.Agencia, INPUT Cbte,INPUT W_DifCdb,  INPUT W_Fecha, 
                                INPUT "Cerrar Cuenta Dificil Cobro",INPUT W_Usuario,   
                                INPUT 0, INPUT Creditos.Int_difCobro,    INPUT 999,       INPUT Docto,    
                                INPUT TODAY, INPUT TIME).
            RUN Gra_MovContable(INPUT Creditos.Agencia, INPUT Cbte,INPUT W_DifCcr,  INPUT W_Fecha, 
                                INPUT "Cerrar Cuenta Dificil Cobro",INPUT W_Usuario,   
                                INPUT Creditos.Int_difCobro, INPUT 0,   INPUT 999,         INPUT Docto,    
                                INPUT TODAY, INPUT TIME).
            RUN Gra_MovContable(INPUT Tmp_Ahorros.Agencia, INPUT Cbte,INPUT CtaCble, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT Creditos.Int_difCobro, INPUT 0,
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            RUN Gra_MovContable(INPUT Tmp_Ahorros.Agencia, INPUT Cbte,INPUT CtaSyA, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT 0, INPUT Creditos.Int_difCobro, 
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT CtaSyA, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT Creditos.Int_difCobro, INPUT 0,
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT W_LiqIntCr, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT 0, INPUT Creditos.Int_difCobro, 
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            ASSIGN T_Valor = T_Valor - Creditos.Int_difCobro
                   T_Inter = T_Inter + Creditos.Int_difCobro
                   Creditos.Int_difCobro = 0.
         END.
         ELSE DO:
            RUN Gra_MovAhorros(INPUT 010101008, INPUT Tmp_ahorros.cod_ahorro, INPUT Tmp_Ahorros.Cue_Ahorros, INPUT Che_Transito.Cheque,
                               INPUT Che_Transito.Agencia,INPUT W_Agencia, INPUT Che_Transito.Agencia, INPUT W_Usuario, 
                               INPUT 0,INPUT T_Valor).
            RUN Gra_MovContable(INPUT Creditos.Agencia, INPUT Cbte,INPUT W_DifCdb,  INPUT W_Fecha, 
                                INPUT "Cerrar Cuenta Dificil Cobro",INPUT W_Usuario,   
                                INPUT 0, INPUT T_Valor,    INPUT 999,       INPUT Docto,    
                                INPUT TODAY, INPUT TIME).
            RUN Gra_MovContable(INPUT Creditos.Agencia, INPUT Cbte,INPUT W_DifCcr,  INPUT W_Fecha, 
                                INPUT "Cerrar Cuenta Dificil Cobro",INPUT W_Usuario,   
                                INPUT T_Valor, INPUT 0,   INPUT 999,         INPUT Docto,    
                                INPUT TODAY, INPUT TIME).
            RUN Gra_MovContable(INPUT Tmp_Ahorros.Agencia, INPUT Cbte,INPUT CtaCble, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT T_Valor, INPUT 0,
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            RUN Gra_MovContable(INPUT Tmp_Ahorros.Agencia, INPUT Cbte,INPUT CtaSyA, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT 0, INPUT T_Valor, 
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT CtaSyA, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT T_Valor, INPUT 0,
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT W_LiqIntCr, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT 0, INPUT T_Valor, 
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            ASSIGN Creditos.Int_difCobro = Creditos.Int_difCobro - T_Valor
                   T_Inter = T_Inter + T_Valor
                   T_Valor = 0.
         END.
      END.
      IF Creditos.Int_MOrCobrar GT 0 AND T_Valor NE 0 THEN DO:
         IF T_Valor GT Creditos.Int_MorCobrar THEN DO:
            RUN Gra_MovAhorros(INPUT 010101007, INPUT Tmp_ahorros.cod_ahorro, INPUT Tmp_Ahorros.Cue_Ahorros, INPUT Che_Transito.Cheque,
                               INPUT Che_Transito.Agencia,INPUT W_Agencia, INPUT Che_Transito.Agencia, INPUT W_Usuario, 
                               INPUT 0,INPUT Creditos.Int_MorCobrar).
            RUN Gra_MovContable(INPUT Tmp_Ahorros.Agencia, INPUT Cbte,INPUT CtaCble, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT Creditos.Int_MorCobrar, INPUT 0,
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            RUN Gra_MovContable(INPUT Tmp_Ahorros.Agencia, INPUT Cbte,INPUT CtaSyA, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT 0, INPUT Creditos.Int_MorCobrar, 
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT CtaSyA, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT Creditos.Int_MorCobrar, INPUT 0,INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT W_LiqIntDb, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT 0, INPUT Creditos.Int_MorCobrar, 
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            ASSIGN T_Valor = T_Valor - Creditos.Int_MorCobrar
                   T_Inter = T_Inter + Creditos.Int_MorCobrar
                   Creditos.Int_MorCobrar = 0.
         END.
         ELSE DO:
            RUN Gra_MovAhorros(INPUT 010101007, INPUT Tmp_ahorros.cod_ahorro, INPUT Tmp_Ahorros.Cue_Ahorros, INPUT Che_Transito.Cheque,
                               INPUT Che_Transito.Agencia,INPUT W_Agencia, INPUT Che_Transito.Agencia, INPUT W_Usuario, 
                               INPUT 0,INPUT T_Valor).
            RUN Gra_MovContable(INPUT Tmp_Ahorros.Agencia, INPUT Cbte,INPUT CtaCble, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT T_Valor, INPUT 0,
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            RUN Gra_MovContable(INPUT Tmp_Ahorros.Agencia, INPUT Cbte,INPUT CtaSyA, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT 0, input T_Valor, 
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT CtaSyA, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT T_Valor, INPUT 0,
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT W_LiqIntDb, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT 0, INPUT T_Valor, 
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            ASSIGN Creditos.Int_MorCobrar = Creditos.Int_MorCobrar - T_Valor
                   T_Inter = T_Inter + T_Valor
                   T_Valor = 0.
         END.
      END.
      IF Creditos.Int_Corrientes GT 0 AND T_Valor NE 0 THEN DO:
         IF T_Valor GT Creditos.Int_Corrientes THEN DO:
            RUN Gra_MovAhorros(INPUT 010101005, INPUT Tmp_ahorros.cod_ahorro, INPUT Tmp_Ahorros.Cue_Ahorros, INPUT Che_Transito.Cheque,
                               INPUT Che_Transito.Agencia,INPUT W_Agencia, INPUT Che_Transito.Agencia, INPUT W_Usuario, 
                               INPUT 0,INPUT Creditos.Int_Corrientes).
            RUN Gra_MovContable(INPUT Tmp_Ahorros.Agencia, INPUT Cbte,INPUT CtaCble, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT Creditos.Int_Corrientes, INPUT 0,
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            RUN Gra_MovContable(INPUT Tmp_Ahorros.Agencia, INPUT Cbte,INPUT CtaSyA, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT 0, INPUT Creditos.Int_Corrientes, 
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT CtaSyA, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT Creditos.Int_Corrientes, INPUT 0,
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT W_LiqIntDb, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT 0, INPUT Creditos.Int_Corrientes, 
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            ASSIGN T_Valor = T_Valor - Creditos.Int_Corrientes
                   T_Inter = T_Inter + Creditos.Int_Corrientes
                   Creditos.Int_Corrientes = 0.
         END.
         ELSE DO:
            RUN Gra_MovAhorros(INPUT 010101005, INPUT Tmp_ahorros.cod_ahorro, INPUT Tmp_Ahorros.Cue_Ahorros, INPUT Che_Transito.Cheque,
                               INPUT Che_Transito.Agencia,INPUT W_Agencia, INPUT Che_Transito.Agencia, INPUT W_Usuario, 
                               INPUT 0,INPUT T_Valor).
            RUN Gra_MovContable(INPUT Tmp_Ahorros.Agencia, INPUT Cbte,INPUT CtaCble, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT T_Valor, INPUT 0,
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            RUN Gra_MovContable(INPUT Tmp_Ahorros.Agencia, INPUT Cbte,INPUT CtaSyA, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT 0, INPUT T_Valor, 
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT CtaSyA, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT T_Valor, INPUT 0,
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT W_LiqIntDb, INPUT TODAY, 
                                INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT 0, INPUT T_Valor, 
                                INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            ASSIGN Creditos.Int_Corrientes = Creditos.Int_Corrientes - T_Valor
                   T_Inter = T_Inter + T_Valor
                   T_Valor = 0.
         END.
      END.
      IF Creditos.Sdo_Capital GT 0 AND T_Valor NE 0 THEN DO:
         RUN Des_Sobregirosya1.
      END.
      ASSIGN Tmp_Ahorros.Sdo_Canje          = Tmp_Ahorros.Sdo_Canje - Che_Transito.Valor
             Tmp_Ahorros.Sdo_Disponible     = Tmp_Ahorros.Sdo_Disponible + Che_Transito.Valor + W_Int - T_Inter - W_ValCanje
             Tmp_Ahorros.Fec_Ulttransaccion = TODAY
             Tmp_Ahorros.Sal_Intpagados     = Tmp_Ahorros.Sal_Intpagados + W_Int.
   END.
   ELSE DO:
      MESSAGE "No Hay Producto de Crédito Asociado a la Cuenta."
      VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
      TITLE "Error en Taquilla.".
      RETURN ERROR.
   END.
   RELEASE Creditos.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Des_Sobregirosya1 W-Win 
PROCEDURE Des_Sobregirosya1 :
/*------------------------------------------------------------------------------
  Obssrvaciones :       
------------------------------------------------------------------------------*/
   IF T_Valor GT Creditos.Sdo_Capital THEN DO:
      ASSIGN T_Difer = T_Valor - Creditos.Sdo_Capital.
      RUN Buscar_Cuenta(INPUT 2,INPUT credito.cod_credito,INPUT Creditos.Plazo,INPUT Ahorros.Nit,OUTPUT AuxCta) NO-ERROR.
      IF ERROR-STATUS:ERROR OR AuxCta EQ "" THEN DO:
         RUN MostrarMensaje IN W_Manija (INPUT 66,OUTPUT W_Error).
         RETURN ERROR.
      END.
      RUN Gra_MovContable(INPUT Tmp_Ahorros.Agencia, INPUT Cbte,INPUT CtaCble, INPUT TODAY, 
                          INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT Creditos.Sdo_Capital, INPUT 0,
                          INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
       RUN Gra_MovContable(INPUT Tmp_Ahorros.Agencia, INPUT Cbte,INPUT CtaSyA, INPUT TODAY, 
                          INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT 0, INPUT Creditos.Sdo_Capital, 
                          INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
      RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT CtaSyA, INPUT TODAY, 
                          INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT Creditos.Sdo_Capital, INPUT 0,
                          INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
      RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT AuxCta, INPUT TODAY, 
                          INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT 0, INPUT Creditos.Sdo_Capital, 
                          INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
      ASSIGN Creditos.Sdo_Capital = 0.
   END.
   ELSE DO:
      RUN Buscar_Cuenta(INPUT 2,INPUT credito.cod_credito,INPUT Creditos.Plazo,INPUT Ahorros.Nit,OUTPUT AuxCta) NO-ERROR.
      IF ERROR-STATUS:ERROR OR AuxCta EQ "" THEN DO:
         RUN MostrarMensaje IN W_Manija (INPUT 66,OUTPUT W_Error).
         RETURN ERROR.
      END.
      RUN Gra_MovContable(INPUT Tmp_Ahorros.Agencia, INPUT Cbte,INPUT CtaCble, INPUT TODAY, 
                          INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT T_Valor, INPUT 0,
                          INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
      RUN Gra_MovContable(INPUT Tmp_Ahorros.Agencia, INPUT Cbte,INPUT CtaSyA, INPUT TODAY, 
                          INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT 0, INPUT T_Valor, 
                          INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
      RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT CtaSyA, INPUT TODAY, 
                          INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT T_Valor, INPUT 0,
                          INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
      RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT AuxCta, INPUT TODAY, 
                          INPUT "Destino Liq. Int.",INPUT W_Usuario,INPUT 0, INPUT T_Valor, 
                          INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
      ASSIGN Creditos.Sdo_Capital = Creditos.Sdo_Capital - T_Valor.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Dev_Ahorros W-Win 
PROCEDURE Dev_Ahorros :
/*------------------------------------------------------------------------------
  Observaciones : Devolver los Cheques en Productos de Ahorros.       
------------------------------------------------------------------------------*/
  DEFINE VAR Nro_Dto  LIKE Taquilla.Num_Documento.
  DEFINE VAR CtaliqDB LIKE Cuentas.Cuenta.
  DEFINE VAR CtaliqCR LIKE Cuentas.Cuenta.
  DEFI   VAR W_NitAso LIKE Clientes.Nit.

  FIND FIRST CortoLargo WHERE CortoLargo.Agencia        EQ Che_Transito.Agencia
                          AND CortoLargo.Cod_Producto   EQ Che_Transito.Cod_Producto
                          AND CortoLargo.Clase_Producto EQ 1 NO-LOCK NO-ERROR.
  IF NOT AVAIL(CortoLargo) THEN DO:
     MESSAGE "Falta Cortolargo para el Pdcto de ahorros."
         VIEW-AS ALERT-BOX ERROR.
     RETURN ERROR.
  END.

  FIND FIRST Operacion WHERE Operacion.Cod_Operacion EQ 010102002
                         AND Operacion.Estado        EQ 1 NO-LOCK NO-ERROR.
  IF NOT AVAIL(Operacion) THEN DO:
     MESSAGE "Falta Operación (Config.Transacc) : 010102002"
         VIEW-AS ALERT-BOX ERROR.
     RETURN ERROR.
  END.
  
  FIND FIRST Comprobantes WHERE Comprobantes.Agencia     EQ Che_Transito.Agencia
                            AND Comprobantes.Comprobante EQ Operacion.Comprobante
                            AND Comprobantes.Estado      EQ 1 NO-ERROR.
  IF NOT AVAIL(Comprobantes) THEN DO:
     MESSAGE "Falta Comprobante : " Operacion.Comprobante " En la agencia : " Che_Transito.Agencia
         VIEW-AS ALERT-BOX ERROR.
     RETURN ERROR.
  END.

  ASSIGN Comprobantes.Secuencia = Comprobantes.Secuencia + 1
         Nro_Dto                = STRING(Comprobantes.Secuencia)
         Cbte                   = Comprobantes.Comprobante.
         
  FIND CURRENT Comprobantes NO-LOCK NO-ERROR.
  
  FOR EACH Taquilla WHERE Taquilla.Cod_Producto  EQ Che_Transito.Cod_Producto
                    AND   Taquilla.Nro_cuenta    EQ Che_Transito.Num_Cuenta
                    AND   Taquilla.Cod_Compensa  EQ Che_Transito.Cod_Compensa
                    AND   Taquilla.Num_Retcheque EQ Che_Transito.Cheque
                    AND   Taquilla.Val_Cheque    EQ Che_Transito.Valor
                    AND   Taquilla.Tip_Producto  EQ 1 EXCLUSIVE-LOCK:
      /*ASSIGN Nro_Dto = Taquilla.Num_Documento.*/
      P_Nit = Taquilla.Nit.
      RUN Gra_AuxTaquilla(INPUT Taquilla.Autorizo,     INPUT Taquilla.Cod_Compensa, INPUT 010102002 , 
                          INPUT Taquilla.Cod_Producto, INPUT F-CtaCble,   INPUT CortoLargo.Cta_AsoAd,      
                          INPUT Taquilla.Naturaleza,   INPUT Taquilla.Nit,          INPUT Taquilla.Nro_cuenta,
                          INPUT Nro_Dto ,INPUT Taquilla.Num_Retcheque,INPUT Taquilla.Agencia,     
                          INPUT Taquilla.Age_Destino,  INPUT Taquilla.Age_Fuente,   INPUT Taquilla.Tip_Producto,  
                          INPUT W_Usuario,             INPUT Taquilla.Val_Cheque,   INPUT Taquilla.Val_Efectivo).
  END.

  FOR EACH Pro_Ahorros WHERE  Pro_ahorros.Cod_ahorro EQ Che_Transito.Cod_Producto 
                        AND   Pro_Ahorros.Estado     EQ 1 NO-LOCK,
      FIRST Ahorros     WHERE Ahorros.Agencia        EQ Che_Transito.Agencia
                        AND   Ahorros.cod_ahorro     EQ Pro_ahorros.Cod_ahorro
                        AND   Ahorros.Cue_Ahorros    EQ Che_Transito.Num_Cuenta EXCLUSIVE-LOCK:
      W_NitAso = Ahorros.Nit.

      RUN Gra_MovAhorros(INPUT 010102002, INPUT Ahorros.cod_ahorro, INPUT Ahorros.Cue_Ahorros, INPUT Nro_Dto,
                         INPUT Che_Transito.Agencia,INPUT W_Agencia,INPUT Che_Transito.Agencia,INPUT W_Usuario, 
                         INPUT Che_Transito.Valor,INPUT 0).
      IF Ahorros.Detalle_Estado   EQ 1 THEN DO:
         ASSIGN Ahorros.Sdo_Canje   = Ahorros.Sdo_Canje   - Che_Transito.Valor
                Ahorros.Sdo_Inicial = Ahorros.Sdo_Inicial - Che_Transito.Valor
                Ahorros.Fec_Ulttransaccion = W_Fecha.
      END.
      ELSE 
         ASSIGN Ahorros.Sdo_Canje = Ahorros.Sdo_Canje - Che_Transito.Valor
                Ahorros.Fec_Ulttransaccion = W_Fecha.                          
      
      ASSIGN Mov_Ahorros.Sdo_Dispon = Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje.

      RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT CortoLargo.Cta_AsoAd, INPUT W_Fecha, 
                          INPUT "Devolución-Cheque.",INPUT W_Usuario,INPUT Che_Transito.Valor, INPUT 0,
                          INPUT 999,INPUT Nro_Dto ,INPUT TODAY,INPUT TIME).
      Mov_Contable.Nit = Ahorros.Nit.
      
      RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT F-CtaCble, INPUT W_Fecha, 
                          INPUT "Devolución-Cheque",INPUT W_Usuario,INPUT 0, INPUT Che_Transito.Valor, 
                          INPUT 999,INPUT Nro_Dto ,INPUT TODAY,INPUT TIME).
      Mov_Contable.Nit = Ahorros.Nit.
      
      IF Che_Transito.Agencia NE Che_Transito.Ofi_Destino THEN DO:
         ASSIGN Mov_Contable.Cuenta = F-CtaSyADev
                Mov_Contable.Nit    = STRING(Che_Transito.Ofi_Destino,"999").
                
         RUN Gra_MovContable(INPUT Che_Transito.Ofi_Destino, INPUT Cbte,INPUT F-CtaSyADev, INPUT W_Fecha, 
                             INPUT "Devolución-Cheque.",INPUT W_Usuario,INPUT Che_Transito.Valor, INPUT 0,
                             INPUT 999,INPUT Nro_Dto ,INPUT TODAY,INPUT TIME).
         ASSIGN Mov_Contable.Nit = STRING(Che_Transito.Agencia,"999").
                
         RUN Gra_MovContable(INPUT Che_Transito.Ofi_Destino, INPUT Cbte,INPUT F-CtaCble, INPUT W_Fecha, 
                             INPUT "Devolución-Cheque.",INPUT W_Usuario,INPUT 0, INPUT Che_Transito.Valor,
                             INPUT 999,INPUT Nro_Dto ,INPUT TODAY,INPUT TIME).
      END.

      IF Che_Transito.Int_Generado GT 0 THEN DO:
         FIND Clientes WHERE Clientes.Nit EQ Ahorros.Nit NO-LOCK NO-ERROR.
         IF AVAILABLE(Clientes) THEN DO:
            FIND Liqui_Int WHERE Liqui_Int.Clase_Producto EQ 1
                           AND   Liqui_Int.Cod_Producto   EQ ahorros.cod_ahorro NO-LOCK NO-ERROR.
            IF AVAILABLE(Liqui_Int) THEN DO:
               IF Ahorros.For_Liquidacion EQ 1 THEN DO:
                  IF Clientes.Tipo_Vinculo EQ 1 THEN
                     ASSIGN CtaliqDB = Liqui_Int.CtaInt_AntAso
                            CtaliqCR = Liqui_Int.CtaCr_LiqAso. 
                  ELSE
                     ASSIGN CtaliqDB = Liqui_Int.CtaInt_Ant
                            CtaliqCR = Liqui_Int.CtaCr_Liq.
               END.
               ELSE DO:
                  IF Clientes.Tipo_vinculo EQ 1 THEN
                     ASSIGN CtaliqDB = Liqui_Int.CtaDb_LiqAso
                            CtaliqCR = Liqui_Int.CtaCr_LiqAso. 
                  ELSE
                     ASSIGN CtaliqDB = Liqui_Int.CtaDb_Liq
                            CtaliqCR = Liqui_Int.CtaCr_Liq.
               END.
               IF CtaliqDB EQ "" THEN DO: RUN MostrarMensaje IN W_Manija (INPUT 66, OUTPUT W_Eleccion). RETURN ERROR. END.
               IF CtaliqCR EQ "" THEN DO: RUN MostrarMensaje IN W_Manija (INPUT 66, OUTPUT W_Eleccion). RETURN ERROR. END.
               FIND Operacion WHERE Operacion.Cod_Operacion EQ 010302003 NO-LOCK NO-ERROR.
               IF AVAILABLE(Operacion) THEN DO:
                  ASSIGN Cbte = Operacion.Comprobante.
               END.
               RUN Cbte_Agencia IN W_Manija (INPUT  Cbte,  INPUT  Che_Transito.Agencia,
                                             OUTPUT Docto, OUTPUT W_Error).
               IF W_Error THEN DO:
                  RUN MostrarMensaje IN W_Manija (INPUT 144, OUTPUT W_Eleccion).
                  RETURN ERROR.
               END.  
               RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT CtaliqCR, INPUT W_Fecha, 
                                   INPUT "Devolución Liq. Int.",INPUT W_Usuario,INPUT Che_Transito.Int_Generado, INPUT 0,
                                   INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
               RUN Gra_MovContable(INPUT Che_Transito.Agencia, INPUT Cbte,INPUT CtaliqDB, INPUT W_Fecha, 
                                   INPUT "Devolución Liq. Int.",INPUT W_Usuario,INPUT 0, INPUT Che_Transito.Int_Generado, 
                                   INPUT 999,INPUT Docto,INPUT TODAY,INPUT TIME).
            END.
         END.
      END.

      ASSIGN Che_Transito.Estado = 3.
  END.

  FIND FIRST Ahorros WHERE Ahorros.Nit            EQ W_NitAso
                     AND   Ahorros.Cod_ahorro     EQ 5
                     AND   Ahorros.Sdo_Dispon     GT 0 NO-LOCK NO-ERROR.
  IF NOT AVAIL(Ahorros) THEN DO:
     FIND FIRST Clientes WHERE Clientes.Nit EQ W_NitAso NO-ERROR.
     ASSIGN Clientes.Tipo_Vinculo = 2 WHEN AVAIL(Clientes).
  END.
  FIND CURRENT Clientes NO-LOCK NO-ERROR.

  RELEASE Taquilla.
  RELEASE Ahorros.
  RELEASE Pro_Ahorros.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Dev_Creditos W-Win 
PROCEDURE Dev_Creditos :
/*------------------------------------------------------------------------------
  OBSERVACIONES : Hacer la Devolución del Crédito si no hay otros pagos.       
------------------------------------------------------------------------------*/
  DEFINE VAR W_CtaDev LIKE Taquilla.Num_Documento.
  
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN F-CtaCble.
     IF F-CtaCble EQ "" THEN DO:
        MESSAGE "La Cuenta Contable Para la Devolución del" SKIP
                "Crédito No se ha Definido."
        VIEW-AS ALERT-BOX QUESTION BUTTONS OK
        TITLE "Error en Cheques en Transito".
        RETURN ERROR.
     END.
     ELSE DO:
        FIND Liqui_Int WHERE Liqui_Int.Clase_Producto EQ 2
                       AND   Liqui_Int.Cod_Producto   EQ Che_Transito.Cod_Producto NO-LOCK NO-ERROR.
        IF AVAILABLE(Liqui_Int) AND Liqui_Int.CtaDb_Ret NE "" THEN DO:
           ASSIGN W_CtaDev = Liqui_Int.CtaDb_Ret.
        END.
        ELSE DO:
           MESSAGE "La Cuenta Contable Para la Devolución del Crédito" SKIP
                   "No Esta Definida en la Tabla de Liquidaciones."
           VIEW-AS ALERT-BOX QUESTION BUTTONS OK
           TITLE "Error en Cheques en Transito".
           RETURN ERROR.
        END.
        FIND Operacion WHERE Operacion.Cod_Operacion EQ 020102001 NO-LOCK NO-ERROR.
        IF AVAILABLE(Operacion) AND Operacion.Comprobante NE 0 THEN DO:
           ASSIGN Cbte = Operacion.Comprobante.
        END.
        RUN Cbte_Agencia IN W_Manija (INPUT  Cbte,  INPUT  Che_Transito.Agencia,
                                      OUTPUT Docto, OUTPUT W_Error).
        IF W_Error THEN DO:
           RUN MostrarMensaje IN W_Manija (INPUT 144, OUTPUT W_Eleccion).
           RETURN ERROR.
        END.
        FIND Creditos WHERE Creditos.Agencia      EQ Che_Transito.Agencia  
                      AND   credito.cod_credito EQ Che_Transito.Cod_Producto
                      AND   Creditos.Pagare       EQ Che_Transito.Num_Cuenta NO-LOCK NO-ERROR.
        IF AVAILABLE(Creditos) THEN DO:
           RUN Gra_AuxTaquilla(INPUT W_Usuario,             INPUT Che_Transito.Cod_Compensa, INPUT 020302001, 
                               INPUT credito.cod_credito, INPUT W_CtaDev,                  INPUT F-CtaCble,      
                               INPUT "DB",                  INPUT Creditos.Nit,              INPUT Creditos.Pagare,
                               INPUT Docto,                 INPUT Che_Transito.Cheque,       INPUT Creditos.Agencia,     
                               INPUT Creditos.Agencia,      INPUT Creditos.Agencia,          INPUT 2,  
                               INPUT W_Usuario,             INPUT Che_Transito.Valor,        INPUT 0).
           ASSIGN Che_Transito.Estado = 3.
        END.
        ELSE DO:
           MESSAGE "El Crédito Para la Devolución No Existe. Verifique..."
           VIEW-AS ALERT-BOX QUESTION BUTTONS OK
           TITLE "Error en Cheques en Transito".
           RETURN ERROR.
        END.
     END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Dev_Cuentas W-Win 
PROCEDURE Dev_Cuentas :
/*------------------------------------------------------------------------------
  OBSERVACIONES : Devolver las operaciones de cheques con los pagos.        
  ------------------------------------------------------------------------------*/
   
   FIND Taquilla WHERE Taquilla.Agencia       EQ Che_Transito.Agencia
                 AND   Taquilla.Cod_Compensa  EQ Che_Transito.Cod_Compensa
                 AND   Taquilla.Num_Retcheque EQ Che_Transito.Cheque
                 AND   Taquilla.Val_Cheque    EQ Che_Transito.Valor
                 AND   Taquilla.Tip_Producto  EQ 0 EXCLUSIVE-LOCK NO-ERROR.
   IF AVAILABLE(Taquilla) THEN DO:
      RUN Gra_AuxTaquilla(INPUT Taquilla.Autorizo,     INPUT Taquilla.Cod_Compensa, INPUT Taquilla.Cod_Operacion , 
                          INPUT Taquilla.Cod_Producto, INPUT Taquilla.Cta_Contra,   INPUT Taquilla.Cuenta,      
                          INPUT Taquilla.Naturaleza,   INPUT Taquilla.Nit,          INPUT Taquilla.Nro_cuenta,
                          INPUT Taquilla.Num_Documento,INPUT Taquilla.Num_Retcheque,INPUT Taquilla.Agencia,     
                          INPUT Taquilla.Age_Destino,  INPUT Taquilla.Age_Fuente,   INPUT Taquilla.Tip_Producto,  
                          INPUT W_Usuario,             INPUT Taquilla.Val_Cheque,   INPUT Taquilla.Val_Efectivo).
      ASSIGN Che_Transito.Estado = 3.
   END.
   ELSE DO:
      MESSAGE "No Encuentra Registro de Taquilla Para el Cheque :" Che_Transito.Cheque
      VIEW-AS ALERT-BOX QUESTION BUTTONS OK
      TITLE "Error en Devolución de Cheques".
      RETURN ERROR.
   END.
   RELEASE Taquilla.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Dev_Especiales W-Win 
PROCEDURE Dev_Especiales :
/*------------------------------------------------------------------------------
  Observaciones : Devolucion de Productos especiales.       
------------------------------------------------------------------------------*/
  DEFINE VAR E_Nit      LIKE Especiales.Nit.
  DEFINE VAR T_Cuota    LIKE Especiales.Cuota.
  DEFINE VAR T_DtoRfcia LIKE Taquilla.Num_Documento.
  DEFINE VAR T_Secuen   LIKE Especiales.Secuencia.
  
  FOR EACH Taquilla WHERE Taquilla.Cod_Producto  EQ Che_Transito.Cod_Producto
                    AND   Taquilla.Cod_Compensa  EQ Che_Transito.Cod_Compensa
                    AND   Taquilla.Num_Retcheque EQ Che_Transito.Cheque
                    AND   Taquilla.Val_Cheque    EQ Che_Transito.Valor
                    AND   Taquilla.Tip_Producto  EQ 4 NO-LOCK:
      ASSIGN E_Nit      = Taquilla.Nit
             T_DtoRfcia = Taquilla.Num_Documento.
      RUN Gra_AuxTaquilla(INPUT Taquilla.Autorizo,     INPUT Taquilla.Cod_Compensa, INPUT Taquilla.Cod_Operacion , 
                          INPUT Taquilla.Cod_Producto, INPUT Taquilla.Cta_Contra,   INPUT Taquilla.Cuenta,      
                          INPUT Taquilla.Naturaleza,   INPUT Taquilla.Nit,          INPUT Taquilla.Nro_cuenta,
                          INPUT Taquilla.Num_Documento,INPUT Taquilla.Num_Retcheque,INPUT Taquilla.Agencia,     
                          INPUT Taquilla.Age_Destino,  INPUT Taquilla.Age_Fuente,   INPUT Taquilla.Tip_Producto,  
                          INPUT W_Usuario,             INPUT Taquilla.Val_Cheque,   INPUT Taquilla.Val_Efectivo).
  END.
  
  FOR FIRST Pro_Especiales WHERE Pro_Especiales.Agencia      EQ Che_Transito.Agencia
                           AND   Pro_Especiales.Cod_Producto EQ Che_Transito.Cod_Producto
                           AND   Pro_Especiales.Estado       EQ 1 EXCLUSIVE-LOCK,
      FIRST Especiales     WHERE Especiales.Agencia      EQ Pro_Especiales.Agencia
                           AND   Especiales.Cod_Producto EQ Pro_Especiales.Cod_Producto
                           AND   Especiales.Nit          EQ E_Nit EXCLUSIVE-LOCK:
      IF Pro_Especiales.Id_CtrSaldo EQ TRUE THEN
         ASSIGN Especiales.Sdo_Pendiente = Especiales.Sdo_Pendiente + Che_Transito.Valor.
      ASSIGN Especiales.Vlr_AcumPagos    = Especiales.Vlr_AcumPagos - Che_Transito.Valor
             T_Cuota = ROUND(Especiales.Vlr_AcumPagos / (Especiales.Cuota * Especiales.Cantidad),0)
             Especiales.Cuo_Pagadas      = T_Cuota
             Che_Transito.Estado         = 3
             T_Secuen                    = Especiales.Secuencia.
  END.
  RUN Gra_MovEspeciales(INPUT 040102006,           INPUT Che_Transito.Cod_Producto,INPUT Che_Transito.Agencia, INPUT W_Agencia,
                        INPUT Che_Transito.Agencia,INPUT E_Nit,                    INPUT T_DtoRfcia,           INPUT T_Secuen,
                        INPUT Che_Transito.Valor,  INPUT 0).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Dia_Habil W-Win 
PROCEDURE Dia_Habil :
/* OBSERVACIONES : Permite Identificar los dias Habiles desde el canje hasta hoy */

DEFINE INPUT PARAMETER C_Agencia LIKE Agencias.Agencia.
DEFINE INPUT PARAMETER C_Fecha AS DATE.
DEFINE OUTPUT PARAMETER C_Habil AS INTEGER INITIAL 0.

DEFINE VAR W_Ano AS INTEGER FORMAT "9999".
DEFINE VAR W_Mes AS INTEGER FORMAT "99".
DEFINE VAR W_Dia AS INTEGER FORMAT "99".

REPEAT:
    ASSIGN C_Fecha = C_Fecha + 1
           W_Ano = YEAR(C_Fecha)
           W_Mes = MONTH(C_Fecha)
           W_Dia = DAY(C_Fecha).

    FIND Calendario WHERE Calendario.Agencia EQ C_Agencia
                      AND Calendario.Ano EQ W_Ano
                      AND Calendario.Mes EQ W_Mes
                      AND Calendario.Dia EQ W_Dia NO-LOCK NO-ERROR.
    IF AVAILABLE(Calendario) THEN DO:
        IF Calendario.Habil THEN
            ASSIGN C_Habil = C_Habil + 1.
    END.

    IF C_Fecha GE TODAY THEN
        LEAVE.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
  THEN DELETE WIDGET W-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win  _DEFAULT-ENABLE
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
  DISPLAY F_Mensage Com_Agencia Com_Consulta F-CtaCble F_RegProcesados 
          Rad_Estado Rad_Producto F_BcoIni F_BcoFin F_CueIni F_CtaFin F_FecIni 
          F_FecFin NomCta F-CtaSyADev NomCta2 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE Brw_Proceso Brw_Consulta Btn_Ingresar BUTTON-117 Com_Agencia 
         Com_Consulta F-CtaCble Rad_Estado Btn_Liberar Btn_Devolver Btn_Salir 
         Btn_Imprimir Btn_Ayuda RECT-116 RECT-117 RECT-281 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  DISPLAY Rad_Imprimir Fill_FecIni Fill_FecFin 
      WITH FRAME Frame_Imprimir IN WINDOW W-Win.
  ENABLE Btn_Imp Rad_Imprimir Btn_Can 
      WITH FRAME Frame_Imprimir IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-Frame_Imprimir}
  DISPLAY Rad_Seleccion 
      WITH FRAME Frame_Ejecutar IN WINDOW W-Win.
  ENABLE Btn_Ejecutar Rad_Seleccion Btn_Cancelar 
      WITH FRAME Frame_Ejecutar IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-Frame_Ejecutar}
  DISPLAY Cmb_AgenciasCanje 
      WITH FRAME F_Ingresar IN WINDOW W-Win.
  IF AVAILABLE Che_Transito THEN 
    DISPLAY Che_Transito.Cheque Che_Transito.Valor Che_Transito.Tip_Remesa 
          Che_Transito.Fec_Canje Che_Transito.Cod_Compensa 
      WITH FRAME F_Ingresar IN WINDOW W-Win.
  ENABLE RECT-282 Cmb_AgenciasCanje Che_Transito.Cheque Che_Transito.Valor 
         Che_Transito.Tip_Remesa Che_Transito.Fec_Canje 
         Che_Transito.Cod_Compensa Btn_Salvar BUTTON-120 
      WITH FRAME F_Ingresar IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F_Ingresar}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Garantias W-Win 
PROCEDURE Garantias :
/*------------------------------------------------------------------------------
  OBSERVACIONES :       
 ------------------------------------------------------------------------------*/
   
   DEFINE INPUT  PARAMETER G_Agencia LIKE Garantias.Agencia.
   DEFINE INPUT  PARAMETER G_PTO     LIKE Garantias.Cod_Credito.
   DEFINE INPUT  PARAMETER G_Pagare  LIKE Garantias.Pagare.
   DEFINE OUTPUT PARAMETER G_Estado  AS   LOGICAL.
   
   DEFINE VAR W_Ad AS INTEGER INITIAL 0.
   DEFINE VAR W_Na AS INTEGER INITIAL 0.
   
/*   FOR EACH Garantias WHERE Garantias.Agencia      EQ G_Agencia
                      AND   Garantias.Cod_Credito EQ G_PTO
                      AND   Garantias.Pagare       EQ G_Pagare 
                      AND   Garantias.Estado       EQ 1 NO-LOCK :
      IF Garantia.Clase_Garantia EQ "ADMI" THEN
         ASSIGN W_Ad = W_Ad + 1.
      ELSE 
         ASSIGN W_Na = W_Na + 1.
   END.*/
   IF W_Ad GT 0 THEN DO:
      ASSIGN G_Estado = TRUE.
      RETURN.
   END.
   ELSE DO:
      IF W_Na GT 0 THEN DO:
         ASSIGN G_Estado = FALSE.
         RETURN.
      END.
      ELSE DO:
         RETURN ERROR.
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Gra_AuxTaquilla W-Win 
PROCEDURE Gra_AuxTaquilla :
/*------------------------------------------------------------------------------
  OBSERVACIONES: Permite Almacenar el Registro en Taquilla.       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER T_Autorizo  LIKE Taquilla.Autorizo.
   DEFINE INPUT PARAMETER T_Banco     LIKE Taquilla.Cod_Compensa.
   DEFINE INPUT PARAMETER T_CodOper   LIKE Taquilla.Cod_Operacion.
   DEFINE INPUT PARAMETER T_CodPto    LIKE Taquilla.Cod_Producto.
   DEFINE INPUT PARAMETER T_Cuenta    LIKE Taquilla.Cuenta.
   DEFINE INPUT PARAMETER T_CtraCta   LIKE Taquilla.Cuenta.
   DEFINE INPUT PARAMETER T_Nat       LIKE Taquilla.Naturaleza.
   DEFINE INPUT PARAMETER T_Nit       LIKE Taquilla.Nit.
   DEFINE INPUT PARAMETER T_Nrocuenta LIKE Taquilla.Nro_cuenta.
   DEFINE INPUT PARAMETER T_NumDto    LIKE Taquilla.Num_Documento.
   DEFINE INPUT PARAMETER T_NumRetche LIKE Taquilla.Num_Retcheque.
   DEFINE INPUT PARAMETER T_Agencia   LIKE Taquilla.Agencia.
   DEFINE INPUT PARAMETER T_OfiDes    LIKE Taquilla.Age_Destino.
   DEFINE INPUT PARAMETER T_OfiFue    LIKE Taquilla.Age_Fuente.
   DEFINE INPUT PARAMETER T_TipPto    LIKE Taquilla.Tip_Producto.
   DEFINE INPUT PARAMETER T_Usuario   LIKE Taquilla.Usuario.
   DEFINE INPUT PARAMETER T_ValChe    LIKE Taquilla.Val_Cheque.
   DEFINE INPUT PARAMETER T_ValEfec   LIKE Taquilla.Val_Efectivo.
   
   CREATE Aux_Taquilla.
   ASSIGN Aux_Taquilla.Autorizo         = T_Autorizo
          Aux_Taquilla.Cod_Compensa     = T_Banco
          Aux_Taquilla.Cod_Operacion    = T_CodOper
          Aux_Taquilla.Cod_Producto     = T_CodPto
          Aux_Taquilla.Contabilizar     = TRUE
          Aux_Taquilla.Cuenta           = T_Cuenta
          Aux_Taquilla.Cta_Contra       = T_CtraCta
          Aux_Taquilla.Duracion         = 0
          Aux_Taquilla.Est_Linea        = 0
          Aux_Taquilla.Fec_Transaccion  = TODAY
          Aux_Taquilla.Hora_Transaccion = TIME
          Aux_Taquilla.Naturaleza       = T_Nat
          Aux_Taquilla.Nit              = T_Nit
          Aux_Taquilla.Nro_cuenta       = T_Nrocuenta
          Aux_Taquilla.Num_Documento    = T_NumDto
          Aux_Taquilla.Num_Retcheque    = T_NumRetche
          Aux_Taquilla.Agencia          = T_Agencia
          Aux_Taquilla.Age_Destino      = T_OfiDes
          Aux_Taquilla.Age_Fuente       = T_OfiFue
          Aux_Taquilla.Tip_Producto     = T_TipPto
          Aux_Taquilla.Usuario          = T_Usuario
          Aux_Taquilla.Val_Cheque       = T_ValChe
          Aux_Taquilla.Val_Efectivo     = T_ValEfec.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Gra_MovAhorros W-Win 
PROCEDURE Gra_MovAhorros :
/*------------------------------------------------------------------------------
  OBSERVACIONES : Permite grabar en movimientos los registro de devolución.       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER M_CodOper LIKE Mov_Ahorros.Cod_Operacion.
  DEFINE INPUT PARAMETER M_CodPto  LIKE Mov_ahorros.cod_ahorro.
  DEFINE INPUT PARAMETER M_Cuenta  LIKE Mov_Ahorros.Cue_Ahorros.
  DEFINE INPUT PARAMETER M_Dto     LIKE Mov_Ahorros.Num_Documento.
  DEFINE INPUT PARAMETER M_Agencia LIKE Mov_Ahorros.Agencia.
  DEFINE INPUT PARAMETER M_OfiFte  LIKE Mov_Ahorros.Age_Fuente.
  DEFINE INPUT PARAMETER M_OfiDest LIKE Mov_Ahorros.Age_Destino.
  DEFINE INPUT PARAMETER M_Usuario LIKE Mov_Ahorros.Usuario.
  DEFINE INPUT PARAMETER M_VlrChe  LIKE Mov_Ahorros.Val_Cheque.
  DEFINE INPUT PARAMETER M_VlrEfe  LIKE Mov_Ahorros.Val_Efectivo.
  
  CREATE Mov_Ahorros.
  ASSIGN Mov_Ahorros.Cod_Operacion = M_CodOper
         Mov_ahorros.Cod_ahorro    = M_CodPto
         Mov_Ahorros.Cue_Ahorros   = M_Cuenta
         Mov_Ahorros.Fecha         = W_Fecha
         Mov_Ahorros.Hora          = TIME
         Mov_Ahorros.Num_Documento = M_Dto
         Mov_Ahorros.Cpte          = Cbte
         Mov_Ahorros.Agencia       = M_Agencia
         Mov_Ahorros.Age_Fuente    = M_OfiFte
         Mov_Ahorros.Age_Destino   = M_OfiDest
         Mov_Ahorros.Usuario       = M_Usuario
         Mov_Ahorros.Nit           = Ahorros.Nit
         Mov_Ahorros.Descrip       = "Devoluciòn Cheque"
         Mov_Ahorros.Val_Cheque    = M_VlrChe
         Mov_Ahorros.Val_Efectivo  = M_VlrEfe NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
     RETURN ERROR. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Gra_MovContable W-Win 
PROCEDURE Gra_MovContable :
/*------------------------------------------------------------------------------
  Objetivo: Hacer el asiento contable.
------------------------------------------------------------------------------*/
  
  DEFINE INPUT PARAMETER P_Agencia LIKE Agencias.Agencia.
  DEFINE INPUT PARAMETER P_Cbte    LIKE Mov_Contable.Comprobante.
  DEFINE INPUT PARAMETER P_Cuenta  LIKE Cuentas.Cuenta.
  DEFINE INPUT PARAMETER P_Fecha1  LIKE Mov_Contable.Fec_Contable.
  DEFINE INPUT PARAMETER P_Coment  LIKE Mov_Contable.Comentario.
  DEFINE INPUT PARAMETER P_Usuario LIKE Usuarios.Usuario.
  DEFINE INPUT PARAMETER P_ValorDB LIKE Mov_Contable.DB.
  DEFINE INPUT PARAMETER P_ValorCR LIKE Mov_Contable.DB.
  DEFINE INPUT PARAMETER P_CC      LIKE Mov_Contable.Cen_Costos.
  DEFINE INPUT PARAMETER P_Docto   LIKE Mov_Contable.Num_Documento.
  DEFINE INPUT PARAMETER P_Fecha2  LIKE Mov_Contable.Fec_Grabacion.
  DEFINE INPUT PARAMETER P_Hora    LIKE Mov_Contable.Hora.
  
  CREATE Mov_Contable.
  ASSIGN Mov_Contable.Agencia       = P_Agencia
         Mov_Contable.Destino       = W_Agencia
         Mov_Contable.Comprobante   = P_Cbte
         Mov_Contable.Cuenta        = P_Cuenta
         Mov_Contable.Fec_Contable  = W_Fecha
         Mov_Contable.Nit           = P_Nit
         Mov_Contable.Comentario    = P_Coment
         Mov_Contable.Usuario       = P_Usuario
         Mov_Contable.DB            = P_ValorDB
         Mov_Contable.CR            = P_ValorCR
         Mov_Contable.Cen_Costos    = P_CC
         Mov_Contable.Num_Documento = P_Docto
         Mov_Contable.Doc_Refer     = Che_Transito.Cheque
         Mov_Contable.Fec_Grabacion = TODAY
         Mov_Contable.Hora          = TIME NO-ERROR.

  IF ERROR-STATUS:ERROR THEN
     RETURN ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Gra_MovCreditos W-Win 
PROCEDURE Gra_MovCreditos :
/*------------------------------------------------------------------------------
  Observaciones : Permite Gravar el Detalle de la Operación en Movimientos.       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER C_CodOper LIKE Mov_Creditos.Cod_Operacion.
  DEFINE INPUT PARAMETER C_CodPto  LIKE Mov_credito.cod_credito.
  DEFINE INPUT PARAMETER C_Pagare  LIKE Mov_Creditos.Pagare.
  DEFINE INPUT PARAMETER C_Dto     LIKE Mov_Creditos.Num_Documento.
  DEFINE INPUT PARAMETER C_Agencia LIKE Mov_Creditos.Agencia.
  DEFINE INPUT PARAMETER C_OfiFte  LIKE Mov_Creditos.Ofi_Fuente.
  DEFINE INPUT PARAMETER C_OfiDest LIKE Mov_Creditos.Ofi_Destino.
  DEFINE INPUT PARAMETER C_Usuario LIKE Mov_Creditos.Usuario.
  DEFINE INPUT PARAMETER C_VlrChe  LIKE Mov_Creditos.Val_Cheque.
  DEFINE INPUT PARAMETER C_VlrEfe  LIKE Mov_Creditos.Val_Efectivo.
  
  CREATE Mov_Creditos.
  ASSIGN Mov_Creditos.Cod_Operacion  = C_CodOper
         Mov_credito.cod_credito   = C_CodPto
         Mov_Creditos.Fecha          = TODAY
         Mov_Creditos.Hora           = vTime
         Mov_Creditos.Num_Documento  = C_Dto
         Mov_Creditos.Agencia        = C_Agencia
         Mov_Creditos.Ofi_Fuente     = C_OfiFte
         Mov_Creditos.Ofi_Destino    = C_OfiDest
         Mov_Creditos.Pagare         = C_Pagare
         Mov_Creditos.Usuario        = C_Usuario
         Mov_Creditos.Val_Cheque     = C_VlrChe
         Mov_Creditos.Val_Efectivo   = C_VlrEfe.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Gra_MovEspeciales W-Win 
PROCEDURE Gra_MovEspeciales :
/*------------------------------------------------------------------------------
  Observación : 
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER E_Opera  LIKE Mov_Especiales.Cod_Operacion.
    DEFINE INPUT PARAMETER E_CodPto LIKE Mov_Especiales.Cod_Producto.
    DEFINE INPUT PARAMETER E_Ofi    LIKE Mov_Especiales.Agencia.
    DEFINE INPUT PARAMETER E_OfiFte LIKE Mov_Especiales.Ofi_Fuente.
    DEFINE INPUT PARAMETER E_OfiDno LIKE Mov_Especiales.Ofi_Destino.
    DEFINE INPUT PARAMETER E_Nit    LIKE Mov_Especiales.Nit.
    DEFINE INPUT PARAMETER E_Ndto   LIKE Mov_Especiales.Num_Documento.
    DEFINE INPUT PARAMETER E_Scia   LIKE Mov_Especiales.Secuencia.
    DEFINE INPUT PARAMETER E_Vche   LIKE Mov_Especiales.Vlr_Cheque.
    DEFINE INPUT PARAMETER E_Vefe   LIKE Mov_Especiales.Vlr_Efectivo.
    
    CREATE Mov_Especiales.
    ASSIGN Mov_Especiales.Cod_Operacion = E_Opera
           Mov_Especiales.Cod_Producto  = E_CodPto
           Mov_Especiales.Agencia       = E_Ofi
           Mov_Especiales.Ofi_Fuente    = E_OfiFte
           Mov_Especiales.Ofi_Destino   = E_OfiDno
           Mov_Especiales.Nit           = E_Nit
           Mov_Especiales.Num_Documento = E_Ndto
           Mov_Especiales.Secuencia     = E_Scia
           Mov_Especiales.Vlr_Cheque    = E_Vche
           Mov_Especiales.Vlr_Efectivo  = E_Vefe
           Mov_Especiales.Usuario       = W_Usuario
           Mov_Especiales.Fecha         = TODAY
           Mov_Especiales.Hora          = TIME.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Gra_Taquilla W-Win 
PROCEDURE Gra_Taquilla :
/*------------------------------------------------------------------------------
  OBSERVACIONES: Permite Almacenar el Registro en Taquilla.       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER T_Autorizo  LIKE Taquilla.Autorizo.
   DEFINE INPUT PARAMETER T_Banco     LIKE Taquilla.Cod_Compensa.
   DEFINE INPUT PARAMETER T_CodOper   LIKE Taquilla.Cod_Operacion.
   DEFINE INPUT PARAMETER T_CodPto    LIKE Taquilla.Cod_Producto.
   DEFINE INPUT PARAMETER T_Cuenta    LIKE Taquilla.Cuenta.
   DEFINE INPUT PARAMETER T_CtraCta   LIKE Taquilla.Cuenta.
   DEFINE INPUT PARAMETER T_Nat       LIKE Taquilla.Naturaleza.
   DEFINE INPUT PARAMETER T_Nit       LIKE Taquilla.Nit.
   DEFINE INPUT PARAMETER T_Nrocuenta LIKE Taquilla.Nro_cuenta.
   DEFINE INPUT PARAMETER T_NumDto    LIKE Taquilla.Num_Documento.
   DEFINE INPUT PARAMETER T_NumRetche LIKE Taquilla.Num_Retcheque.
   DEFINE INPUT PARAMETER T_Agencia   LIKE Taquilla.Agencia.
   DEFINE INPUT PARAMETER T_OfiDes    LIKE Taquilla.Age_Destino.
   DEFINE INPUT PARAMETER T_OfiFue    LIKE Taquilla.Age_Fuente.
   DEFINE INPUT PARAMETER T_TipPto    LIKE Taquilla.Tip_Producto.
   DEFINE INPUT PARAMETER T_Usuario   LIKE Taquilla.Usuario.
   DEFINE INPUT PARAMETER T_ValChe    LIKE Taquilla.Val_Cheque.
   DEFINE INPUT PARAMETER T_ValEfec   LIKE Taquilla.Val_Efectivo.
   
   CREATE Taquilla.
   ASSIGN Taquilla.Autorizo         = T_Autorizo
          Taquilla.Cod_Compensa     = T_Banco
          Taquilla.Cod_Operacion    = T_CodOper
          Taquilla.Cod_Producto     = T_CodPto
          Taquilla.Contabilizar     = FALSE
          Taquilla.Cuenta           = T_Cuenta
          Taquilla.Cta_Contra       = T_CtraCta
          Taquilla.Duracion         = 0
          Taquilla.Est_Linea        = 0
          Taquilla.Fec_Transaccion  = TODAY
          Taquilla.Hora_Transaccion = TIME
          Taquilla.Naturaleza       = T_Nat
          Taquilla.Nit              = T_Nit
          Taquilla.Nro_cuenta       = T_Nrocuenta
          Taquilla.Num_Documento    = T_NumDto
          Taquilla.Num_Retcheque    = T_NumRetche
          Taquilla.Agencia          = T_Agencia
          Taquilla.Age_Destino      = T_OfiDes
          Taquilla.Age_Fuente       = T_OfiFue
          Taquilla.Tip_Producto     = T_TipPto
          Taquilla.Usuario          = T_Usuario
          Taquilla.Val_Cheque       = T_ValChe
          Taquilla.Val_Efectivo     = T_ValEfec.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImprimeCpte W-Win 
PROCEDURE ImprimeCpte :
/*------------------------------------------------------------------------------
  Purpose:     
 ------------------------------------------------------------------------------*/
 DEFI VAR TotD   LIKE Mov_Contable.Db INIT 0.
 DEFI VAR TotC   LIKE Mov_Contable.Db INIT 0.
 DEFI VAR TTotD  LIKE Mov_Contable.Db INIT 0.
 DEFI VAR TTotC  LIKE Mov_Contable.Db INIT 0.

 {Incluido\RepEncabezado.I}

 ASSIGN W_Reporte = "Cpte Resumen : Contabilización Devolución-Cheque      Fecha del Informe: " +
                     STRING(W_Fecha,"99/99/9999") + "      Hora : " + STRING(TIME,"HH:MM:SS")
        W_EncColumna = "Comprobante: " + STRING(Comprobantes.Comprobante,"99") + "-" + 
                       STRING(Comprobantes.Secuencia,"99999999") + "-" + Comprobantes.Nombre.

 VIEW FRAME F-Encabezado.
 VIEW FRAME f-ftr.

 FOR EACH Mov_Contable WHERE Mov_Contable.Comprobante   EQ Comprobantes.Comprobante
                         AND Mov_Contable.Num_Documento EQ Comprobantes.Secuencia
                         AND Mov_Contable.Fec_Contable  EQ W_Fecha 
                         /*AND Mov_Contable.Agencia       EQ W_Agencia*/ NO-LOCK
                             BREAK BY Mov_Contable.Agencia BY Mov_Contable.Cuenta
                                   BY Mov_Contable.Nit:
     ASSIGN TotD  = TotD  + Mov_Contable.Db
            TTotD = TTotD + Mov_Contable.Db
            TotC  = TotC  + Mov_Contable.Cr
            TTotC = TTotC + Mov_Contable.Cr.

     IF LAST-OF(Mov_Contable.Nit) THEN DO:
        FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Mov_Contable.Cuenta
                             AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
        DISPLAY Mov_Contable.Agencia   LABEL "Ag."
                Mov_Contable.Cuenta    LABEL "Cta-Contable"
                Cuentas.Nombre         LABEL "Descripciòn de la Cuenta" WHEN AVAIL(Cuentas)
                Mov_Contable.Nit       LABEL "Ced/Nit"
                Mov_Contable.Doc_Refer LABEL "Doc-Refer"
                TotD                   LABEL "TOTAL DEBITOS"  FORM "->>>>>>,>>>,>>9.99"
                TotC                   LABEL "TOTAL CREDITOS" FORM "->>>>>>,>>>,>>9.99"
            WITH DOWN WIDTH 140 FRAME F21 USE-TEXT NO-LABELS STREAM-IO NO-BOX.

        ASSIGN TotD  = 0
               TotC  = 0.
     END.

 END.

 DISPLAY SKIP(1)
         "                     TOTAL FINAL------------>                                       ------------------ ------------------"
         SKIP
         "                                                                                   "
         TTotD      FORM "->>>>>>,>>>,>>9.99"
         TTotC      FORM "->>>>>>,>>>,>>9.99"
            WITH DOWN WIDTH 180 FRAME FT21T USE-TEXT NO-LABELS STREAM-IO NO-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir_Excel W-Win 
PROCEDURE Imprimir_Excel :
IF Rad_Imprimir  EQ 0 THEN RUN Imp_ExcelTodos.
ELSE RUN Imp_ExcelVarios.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imp_ExcelTodos W-Win 
PROCEDURE Imp_ExcelTodos :
{Incluido\Def_Excel.i}
    DEFINE VAR EstIni  LIKE Che_Transito.Estado.
    DEFINE VAR EstFin  LIKE Che_Transito.Estado.
    DEFINE VAR W_NomPcto   AS CHARACTER FORMAT "X(30)"  INITIAL "".
    DEFINE VAR W_Tipo      AS CHARACTER FORMAT "X(10)"  INITIAL "".
    DEFINE VAR W_Ret       AS DECIMAL   FORMAT "99.99%" INITIAL 0.
 /* MANDA ENCABEZADO DE HOJA DE CALCULO*/
 
 E_NumFila = 1.
 E_NumColumn = 8.
 E_Fila      =      "003" + "Age"
                  + "002" + "CC"
                  + "010" + "Cheque    "
                  + "010" + "Fec.Canje "
                  + "010" + "Fec.Confir"
                  + "001" + "E"
                  + "021" + "Int.Generado         "
                  + "021" + "Valor                ".
 RUN W-GraExcel.w (INPUT E_Fila, INPUT E_NumColumn, OUTPUT E_CmpGrafic).

/* launch Excel so it is visible to the user */
chExcelApp:Visible = TRUE.

/* create a new Workbook */
chWorkbook = chExcelApp:Workbooks:Add().

/* get the active Worksheet */
chWorkSheet = chExcelApp:Sheets:Item(1).

     ASSIGN EstIni = 1
            EstFin = 4.
     FOR EACH Che_Transito WHERE Che_Transito.Estado GE EstIni
                           AND   Che_Transito.Estado LE EstFin NO-LOCK:
      E_Fila2     = "".
      E_Fila2     = "003" + STRING(Che_Transito.Agencia,"999")
                  + "002" + STRING(Che_Transito.Cod_Compensa,"99")
                  + "010" + STRING(Che_Transito.Cheque,"X(10)")
                  + "010" + STRING(Che_Transito.Fec_Canje,"99/99/9999")
                  + "010" + STRING(Che_Transito.Fec_Confirmacion,"99/99/9999")
                  + "001" + STRING(Che_Transito.Estado,"9")
                  + "021" + STRING(Che_Transito.Int_Generado,"->>>>>,>>>,>>>,>>9.99")
                  + "021" + STRING(Che_Transito.Valor,"->>>>>,>>>,>>>,>>9.99").
                  
      {Incluido\imprimir_Excel.i}
 END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imp_ExcelVarios W-Win 
PROCEDURE Imp_ExcelVarios :
{Incluido\Def_Excel.i}
    DEFINE VAR EstIni  LIKE Che_Transito.Estado.
    DEFINE VAR EstFin  LIKE Che_Transito.Estado.
    DEFINE VAR W_NomPcto   AS CHARACTER FORMAT "X(30)"  INITIAL "".
    DEFINE VAR W_Tipo      AS CHARACTER FORMAT "X(10)"  INITIAL "".
    DEFINE VAR W_Ret       AS DECIMAL   FORMAT "99.99%" INITIAL 0.
 /* MANDA ENCABEZADO DE HOJA DE CALCULO*/
 
 E_NumFila = 1.
 E_NumColumn = 8.
 E_Fila      =      "003" + "Age"
                  + "002" + "CC"
                  + "010" + "Cheque    "
                  + "010" + "Fec.Canje "
                  + "010" + "Fec.Confir"
                  + "001" + "E"
                  + "021" + "Int.Generado         "
                  + "021" + "Valor                ".
 RUN W-GraExcel.w (INPUT E_Fila, INPUT E_NumColumn, OUTPUT E_CmpGrafic).

/* launch Excel so it is visible to the user */
chExcelApp:Visible = TRUE.

/* create a new Workbook */
chWorkbook = chExcelApp:Workbooks:Add().

/* get the active Worksheet */
chWorkSheet = chExcelApp:Sheets:Item(1).

     ASSIGN EstIni = Rad_Imprimir
            EstFin = Rad_Imprimir.
     FOR EACH Che_Transito WHERE Che_Transito.Fec_Canje GE Fill_FecIni 
                           AND   Che_Transito.Fec_Canje LE Fill_FecFin
                           AND   Che_Transito.Estado    GE EstIni
                           AND   Che_Transito.Estado    LE EstFin NO-LOCK:
      E_Fila2     = "".
      E_Fila2     = "003" + STRING(Che_Transito.Agencia,"999")
                  + "002" + STRING(Che_Transito.Cod_Compensa,"99")
                  + "010" + STRING(Che_Transito.Cheque,"X(10)")
                  + "010" + STRING(Che_Transito.Fec_Canje,"99/99/9999")
                  + "010" + STRING(Che_Transito.Fec_Confirmacion,"99/99/9999")
                  + "001" + STRING(Che_Transito.Estado,"9")
                  + "021" + STRING(Che_Transito.Int_Generado,"->>>>>,>>>,>>>,>>9.99")
                  + "021" + STRING(Che_Transito.Valor,"->>>>>,>>>,>>>,>>9.99").
                  
      {Incluido\imprimir_Excel.i}
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize W-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  OBSERVACIONES :       
------------------------------------------------------------------------------*/
  DEFINE VAR I_Agencia LIKE Agencias.Agencia.
  DEFINE VAR I_NomOfi  LIKE Agencias.Nombre.
  
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN I_NomOfi = "" T_OfiIni = 0 T_OfiFin = 0.
     RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ).
     
     ASSIGN Com_Agencia:LIST-ITEMS = "".
     RUN SuperUsuario IN W_Manija (INPUT W_Agencia, W_Usuario, OUTPUT W_Error).
     IF W_Error THEN
        ASSIGN Com_Agencia:SENSITIVE  = TRUE.
     ELSE
        ASSIGN Com_Agencia:SENSITIVE  = FALSE.
     Com_Agencia:ADD-LAST("000 Consolidado").
     FOR EACH Agencias WHERE Agencias.Estado NE 3 NO-LOCK:
         IF Agencias.Estado EQ 1 THEN DO:
            Com_Agencia:ADD-LAST(STRING(Agencias.Agencia,"999") + " " + STRING(Agencias.Nombre,"X(30)")).
            Cmb_AgenciasCanje:ADD-LAST(STRING(Agencias.Agencia,"999") + " " + STRING(Agencias.Nombre,"X(30)")) IN FRAME F_Ingresar.
            IF Agencias.Agencia EQ W_Agencia THEN DO:
               ASSIGN I_Agencia = Agencias.Agencia
                      I_NomOfi  = Agencias.Nombre
                      Cmb_AgenciasCanje:SCREEN-VALUE IN FRAME F_Ingresar = STRING(Agencias.Agencia,"999") + " " + STRING(Agencias.Nombre,"X(30)").
            END.
         END.
         ELSE DO:
            IF Agencias.Agencia EQ W_Agencia AND W_Error = FALSE THEN DO:
               MESSAGE "La Agencia de Origen del Usuario Esta en Cierre." SKIP
                       "Este No Tiene Autorización Para Otra Agencia."
               VIEW-AS ALERT-BOX QUESTION BUTTONS OK
               TITLE "Error en Cheques en Transito".
               RETURN ERROR.
            END.
         END.
     END.
     IF I_NomOfi EQ "" AND W_Error THEN DO:
        ASSIGN Com_Agencia:SCREEN-VALUE = Com_Agencia:ENTRY(1)
               Brw_Consulta:HIDDEN      = TRUE
               Brw_Proceso:HIDDEN       = FALSE
               T_OfiIni                 = 0
               T_OfiFin                 = 999.
     END.
     ELSE DO:
        ASSIGN Com_Agencia:SCREEN-VALUE = STRING(I_Agencia,"999") + " " + STRING(I_NomOfi,"X(30)")
               Brw_Consulta:HIDDEN      = TRUE
               Brw_Proceso:HIDDEN       = FALSE
               T_OfiIni                 = W_Agencia
               T_OfiFin                 = W_Agencia.
     END.
     OPEN QUERY Brw_Proceso FOR EACH Che_Transito WHERE Che_Transito.Agencia GE T_OfiIni
                                                  AND   Che_Transito.Agencia LE T_OfiFin
                                                  AND   Che_Transito.Estado  EQ 1 /*NO-LOCK*/.

     FIND FIRST CortoLargo WHERE CortoLargo.Agencia  EQ W_Agencia  /*Busca Oficina Fuente*/
                  AND   CortoLargo.Clase_Producto EQ 1 
                  AND   CortoLargo.Cod_Producto   EQ 1
                  AND   CortoLargo.Plazo_Inicial  GE 0 NO-LOCK NO-ERROR.
     IF AVAILABLE(CortoLargo) THEN DO:
        FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ CortoLargo.Cta_SYA
                           AND   Cuentas.Tipo   EQ 2
                           AND   Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
        IF AVAIL(Cuentas) THEN
           ASSIGN NomCta2:SCREEN-VALUE     = Cuentas.Nombre
                  F-CtaSyADev              = CortoLargo.Cta_SYA
                  F-CtaSyADev:SCREEN-VALUE = CortoLargo.Cta_SYA.
     END.
  END.

  IF NOT W_Inicio THEN DO:
        APPLY "Entry" TO F-CtaCble.
        RETURN NO-APPLY.
 END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Periodo W-Win 
PROCEDURE Periodo :
/*------------------------------------------------------------------------------
  Observaciones :       
------------------------------------------------------------------------------*/
  
  DEFINE INPUT  PARAMETER P_Dato  AS INTEGER.
  DEFINE OUTPUT PARAMETER P_Valor AS INTEGER.
  
  CASE P_Dato:
    WHEN 4 THEN DO:
       ASSIGN P_Valor = 30.
    END.
    WHEN 5 THEN DO:
       ASSIGN P_Valor = 60.
    END.
    WHEN 6 THEN DO:
       ASSIGN P_Valor = 90.
    END.
    WHEN 8 THEN DO:
       ASSIGN P_Valor = 180.
    END.
    WHEN 9 THEN DO:
       ASSIGN P_Valor = 360.
    END.
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir W-Win 
PROCEDURE ProcesoImprimir :
/*------------------------------------------------------------------------------
  OBSERVACIONES : Impresión de los Cheques.       
------------------------------------------------------------------------------*/
  IF W_SiContab THEN DO:
     RUN ImprimeCpte.
     W_SiContab = FALSE.
     RETURN.
  END.

{INCLUIDO\RepEncabezado.I}    
    W_Reporte    = "REPORTE   : CHEQUES EN TRANSITO - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
    W_EncColumna = "".
    
  
  DEFINE FRAME F-LineaEncabezado
    HEADER
      " Agencia  Banco    Cheque    Fec.Canje   Fec.Confirma  Estado        Int.Generado                Valor     " AT 1  
      "___________________________________________________________________________________________________________" AT 1
  WITH DOWN WIDTH 132 USE-TEXT PAGE-TOP FRAME F-LineaEncabezado STREAM-IO NO-BOX.
  
  
  VIEW FRAME F-Encabezado.
  VIEW FRAME F-LineaEncabezado.
  VIEW FRAME F-Ftr.

  DEFINE VAR EstIni  LIKE Che_Transito.Estado.
  DEFINE VAR EstFin  LIKE Che_Transito.Estado.
  ASSIGN FRAME Frame_Imprimir Rad_Imprimir Fill_FecIni Fill_FecFin.
  IF Rad_Imprimir EQ 0 THEN DO:
     ASSIGN EstIni = 1
            EstFin = 4.
     FOR EACH Che_Transito WHERE Che_Transito.Estado GE EstIni
                           AND   Che_Transito.Estado LE EstFin NO-LOCK:
         DISPLAY Che_Transito.Agencia          AT 4  NO-LABEL
                 Che_Transito.Cod_Compensa     AT 12 NO-LABEL
                 Che_Transito.Cheque           AT 18 NO-LABEL
                 Che_Transito.Fec_Canje        AT 30 NO-LABEL
                 Che_Transito.Fec_Confirmacion AT 43 NO-LABEL
                 Che_Transito.Estado           AT 59 NO-LABEL
                 Che_Transito.Int_Generado     AT 65 NO-LABEL
                 Che_Transito.Valor            AT 87 NO-LABEL
         WITH FRAME F-Cheques DOWN WIDTH 132 USE-TEXT STREAM-IO NO-BOX.
         DOWN WITH FRAME F-Cheques.
     END.
  END.
  ELSE DO:
     ASSIGN EstIni = Rad_Imprimir
            EstFin = Rad_Imprimir.
     FOR EACH Che_Transito WHERE Che_Transito.Fec_Canje GE Fill_FecIni 
                           AND   Che_Transito.Fec_Canje LE Fill_FecFin
                           AND   Che_Transito.Estado    GE EstIni
                           AND   Che_Transito.Estado    LE EstFin NO-LOCK:
         DISPLAY Che_Transito.Agencia          AT 4  NO-LABEL
                 Che_Transito.Cod_Compensa     AT 12 NO-LABEL
                 Che_Transito.Cheque           AT 18 NO-LABEL
                 Che_Transito.Fec_Canje        AT 30 NO-LABEL
                 Che_Transito.Fec_Confirmacion AT 43 NO-LABEL
                 Che_Transito.Estado           AT 59 NO-LABEL
                 Che_Transito.Int_Generado     AT 65 NO-LABEL
                 Che_Transito.Valor            AT 87 NO-LABEL
         WITH FRAME F-Cheques1 DOWN WIDTH 132 USE-TEXT STREAM-IO NO-BOX.
         DOWN WITH FRAME F-Cheques1.
     END.
  END.
  PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PtllaTop W-Win 
PROCEDURE PtllaTop :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  W-Win:MOVE-TO-TOP().
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Ret_Interes W-Win 
PROCEDURE Ret_Interes :
/* OBSERVACIONES : Permite verifivar si es sujeto a retención en la fuente */

DEFINE INPUT PARAMETER R_Periodo AS INTEGER.
DEFINE INPUT PARAMETER R_Nit LIKE Ahorros.Nit.
DEFINE INPUT PARAMETER R_Pto LIKE Liqui_Int.Clase_Producto.
DEFINE INPUT PARAMETER R_CodPto LIKE Liqui_Int.Cod_Producto.
DEFINE INPUT PARAMETER R_Valor AS DECIMAL.
DEFINE OUTPUT PARAMETER R_ValCon AS DECIMAL.

DEFINE VAR CtaliqDB LIKE Cuentas.Cuenta INITIAL "".
DEFINE VAR CtaliqCR LIKE Cuentas.Cuenta INITIAL "".
DEFINE VAR CtaRDb LIKE Liqui_Int.CtaDb_Ret INITIAL "".
DEFINE VAR CtaRCr LIKE Liqui_Int.CtaCr_Ret INITIAL "".
DEFINE VAR Por_Ret LIKE Base_Ret.Porcentaje INITIAL 0.
DEFINE VAR Base LIKE Liqui_Int.Base INITIAL 0.
DEFINE VAR CodBas LIKE Liqui_Int.Cod_base INITIAL 0.
DEFINE VAR W_Valor AS DECIMAL INITIAL 0.
DEFINE VAR R_DiaPer AS INTEGER INITIAL 0.
DEFINE VAR W_ValRet AS DECIMAL INITIAL 0.
     
FIND Clientes WHERE Clientes.Nit EQ R_Nit NO-LOCK NO-ERROR.
IF AVAILABLE(Clientes) THEN DO:
    IF Clientes.Id_Retencion THEN DO:
        FIND FIRST Liqui_Int WHERE Liqui_Int.Clase_Producto EQ R_Pto
                               AND Liqui_Int.Cod_Producto EQ R_CodPto NO-LOCK NO-ERROR.
        IF AVAILABLE(Liqui_Int) THEN DO:
            ASSIGN CtaRDb = Liqui_Int.CtaDb_Ret
                   CtaRCr = Liqui_Int.CtaCr_Ret
                   Base = Liqui_Int.Base
                   CodBas = Liqui_Int.Cod_base.
            
            IF Ahorros.For_Liquidacion EQ 1 THEN DO:
                IF Clientes.Tipo_Vinculo EQ 1 THEN
                    ASSIGN CtaliqDB = Liqui_Int.CtaInt_AntAso
                           CtaliqCR = Liqui_Int.CtaCr_LiqAso.
                ELSE
                    ASSIGN CtaliqDB = Liqui_Int.CtaInt_Ant
                           CtaliqCR = Liqui_Int.CtaCr_Liq.
            END.
            ELSE DO:
                IF Clientes.Tipo_Vinculo EQ 1 THEN
                    ASSIGN CtaliqDB = Liqui_Int.CtaDb_LiqAso
                           CtaliqCR = Liqui_Int.CtaCr_LiqAso.
                ELSE
                    ASSIGN CtaliqDB = Liqui_Int.CtaDb_Liq
                           CtaliqCR = Liqui_Int.CtaCr_Liq.
            END.

            IF CtaRDb EQ "" THEN
                RETURN ERROR.

            IF CtaRCr EQ "" THEN
                RETURN ERROR.

            IF CtaliqDB EQ "" THEN
                RETURN ERROR.

            IF CtaliqCR EQ "" THEN
                RETURN ERROR.
        END.
        ELSE
            RETURN ERROR.

        RUN Ret_Pdo.

        IF Ahorros.Per_Liquidacion EQ 0 THEN DO:
            ASSIGN R_DiaPer = TODAY - Che_Transito.Fec_Canje
                   W_Valor = R_Valor / R_DiaPer.
        END.
        ELSE
            ASSIGN W_Valor  = R_Valor / W_Pl.
        
        IF W_Valor GE Base THEN DO:
            FIND FIRST Base_Ret WHERE Base_Ret.Cod_base EQ CodBas
                                  AND Base_Ret.Estado EQ 1 NO-LOCK NO-ERROR.
            IF AVAILABLE(Base_Ret) THEN DO:
                ASSIGN Por_Ret = Base_Ret.Porcentaje
                       W_ValRet = R_Valor * Por_Ret / 100
                       R_ValCon = R_Valor - W_ValRet.

                FIND Operacion WHERE Operacion.Cod_Operacion EQ 010302001 NO-LOCK NO-ERROR.
                IF AVAILABLE(Operacion) THEN
                    ASSIGN Cbte = Operacion.Comprobante.
            
                RUN Cbte_Agencia IN W_Manija (INPUT Cbte,
                                              INPUT Che_Transito.Agencia,
                                              OUTPUT Docto,
                                              OUTPUT W_Error).
                IF W_Error THEN DO:
                    RUN MostrarMensaje IN W_Manija (INPUT 144,
                                                    OUTPUT W_Eleccion).
                    RETURN ERROR.
                END.

                RUN Gra_MovContable(INPUT Che_Transito.Agencia,
                                    INPUT Cbte,
                                    INPUT CtaliqCR,
                                    INPUT W_Fecha,
                                    INPUT "Retención en la Fuente Int.",
                                    INPUT W_Usuario,
                                    INPUT W_ValRet,
                                    INPUT 0,
                                    INPUT 999,
                                    INPUT Docto,
                                    INPUT TODAY,
                                    INPUT TIME).

                RUN Gra_MovContable(INPUT Che_Transito.Agencia,
                                    INPUT Cbte,
                                    INPUT CtaliqDB,
                                    INPUT W_Fecha,
                                    INPUT "Retención en la Fuente Int.",
                                    INPUT W_Usuario,
                                    INPUT 0,
                                    INPUT W_ValRet,
                                    INPUT 999,
                                    INPUT Docto,
                                    INPUT TODAY,
                                    INPUT TIME).

                RUN Gra_MovContable(INPUT Che_Transito.Agencia,
                                    INPUT Cbte,
                                    INPUT CtaRDb,
                                    INPUT W_Fecha,
                                    INPUT "Retención en la Fuente Int.",
                                    INPUT W_Usuario,
                                    INPUT W_ValRet,
                                    INPUT 0,
                                    INPUT 999,
                                    INPUT Docto,
                                    INPUT TODAY,
                                    INPUT TIME).

                RUN Gra_MovContable(INPUT Che_Transito.Agencia,
                                    INPUT Cbte,
                                    INPUT CtaRCr,
                                    INPUT W_Fecha,
                                    INPUT "Retención en la Fuente Int.",
                                    INPUT W_Usuario,
                                    INPUT 0,
                                    INPUT W_ValRet,
                                    INPUT 999,
                                    INPUT Docto,
                                    INPUT TODAY,
                                    INPUT TIME).
            END.
            ELSE
                RETURN ERROR.
        END.
        ELSE
            ASSIGN R_ValCon = R_Valor.
    END.
    ELSE
        ASSIGN R_ValCon = R_Valor.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Ret_Pdo W-Win 
PROCEDURE Ret_Pdo :
/*------------------------------------------------------------------------------
   Objetivo: Busca el numero de meses o dias segun el periodo
   ------------------------------------------------------------------------------*/
   
   CASE Ahorros.Per_Liquidacion:
     WHEN 0 THEN  ASSIGN W_Pl = 1.              /*Diario*/
     WHEN 4 THEN  ASSIGN W_Pl = 30.             /*Mensual*/
     WHEN 5 THEN  ASSIGN W_Pl = 60.             /*Bimestre*/
     WHEN 6 THEN  ASSIGN W_Pl = 90.             /*Trimestre*/
     WHEN 8 THEN  ASSIGN W_Pl = 180.            /*Semestre*/
     WHEN 9 THEN  ASSIGN W_Pl = 360.            /*Anual*/
     WHEN 10 THEN ASSIGN W_Pl = Ahorros.Plazo.  /*Al Vencimiento */
   END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "Che_Transito"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Sobregiro W-Win 
PROCEDURE Sobregiro :
/*------------------------------------------------------------------------------
  OBSERVACIONES : Consigna Sobregiro en Efectivo.       
------------------------------------------------------------------------------*/
   DEFINE VAR T_Valor LIKE Creditos.Sdo_Capital INITIAL 0.
   DEFINE VAR T_Difer LIKE Creditos.Sdo_Capital INITIAL 0.
   DEFINE VAR T_Inter LIKE Creditos.Sdo_Capital INITIAL 0.
   
   RUN Buscar_Cuenta(INPUT 1,INPUT ahorros.cod_ahorro,INPUT Ahorros.Plazo,INPUT Ahorros.Nit,OUTPUT CtaCble) NO-ERROR.
   IF ERROR-STATUS:ERROR OR CtaCble EQ "" THEN DO:
      RUN MostrarMensaje IN W_Manija (INPUT 66,OUTPUT W_Error).
      RETURN ERROR.
   END.
   
   IF Che_Transito.Int_Generado GT 0 THEN DO:
      RUN Ret_Interes(INPUT Ahorros.Per_Liquidacion,INPUT Ahorros.Nit,INPUT 1,
                      INPUT ahorros.cod_ahorro,INPUT Che_Transito.Int_Generado,OUTPUT W_Int) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
         MESSAGE "Error en el Cálculo de Retención de Intereses.".
         RETURN ERROR.
      END.
   END.
   ASSIGN T_Valor = Che_Transito.Valor + W_Int - W_ValCanje.
   FIND Creditos WHERE Creditos.Agencia      EQ Ahorros.Agencia
                 AND   credito.cod_credito EQ Pro_Ahorros.ProCre_Asociado
                 AND   Creditos.Pagare       EQ Ahorros.Cue_Ahorros EXCLUSIVE-LOCK NO-ERROR.
   IF AVAILABLE(Creditos) THEN DO:
      RUN Cta_Sobregiro(INPUT 2,INPUT credito.cod_credito,INPUT Creditos.Nit,
                        OUTPUT W_LiqIntDb,OUTPUT W_LiqIntCr,OUTPUT W_LiqMor,
                        OUTPUT W_DifCdb,OUTPUT W_DifCcr).
      IF ERROR-STATUS:ERROR THEN DO:
         RUN MostrarMensaje IN W_Manija (INPUT 66,OUTPUT W_Error).
         RETURN ERROR.
      END.
      IF Creditos.Int_MorCobrar GT 0 AND T_Valor NE 0 THEN DO:
         IF T_Valor GT Creditos.Int_MorCobrar THEN DO:
            RUN Gra_MovAhorros(INPUT 010101006, INPUT ahorros.cod_ahorro, INPUT Ahorros.Cue_Ahorros, INPUT Che_Transito.Cheque,
                               INPUT Che_Transito.Agencia,INPUT W_Agencia, INPUT Che_Transito.Agencia, INPUT W_Usuario, 
                               INPUT Creditos.Int_MorCobrar,INPUT 0).
            RUN Gra_Taquilla(INPUT W_Usuario,           INPUT Che_Transito.Cod_Compensa,INPUT 010101015, 
                             INPUT ahorros.cod_ahorro,INPUT W_LiqMor,                 INPUT CtaCble,
                             INPUT "CR",                INPUT Ahorros.Nit,              INPUT Ahorros.Cue_Ahorros, 
                             INPUT Che_Transito.Cheque, INPUT Che_Transito.Cheque,      INPUT Ahorros.Agencia,           
                             INPUT Che_Transito.Agencia,INPUT Che_Transito.Ofi_Destino, INPUT "1",                 
                             INPUT W_Usuario,           INPUT Creditos.Int_MorCobrar,   INPUT 0).
            ASSIGN T_Valor = T_Valor - Creditos.Int_MorCobrar
                   T_Inter = T_Inter + Creditos.Int_MorCobrar
                   Creditos.Int_MorCobrar = 0.
         END.
         ELSE DO:
            RUN Gra_MovAhorros(INPUT 010101006, INPUT ahorros.cod_ahorro, INPUT Ahorros.Cue_Ahorros, INPUT Che_Transito.Cheque,
                               INPUT Che_Transito.Agencia,INPUT W_Agencia, INPUT Che_Transito.Agencia, INPUT W_Usuario, 
                               INPUT T_Valor,INPUT 0).
            RUN Gra_Taquilla(INPUT W_Usuario,           INPUT Che_Transito.Cod_Compensa,INPUT 010101015, 
                             INPUT ahorros.cod_ahorro,INPUT W_LiqMor,                 INPUT CtaCble,
                             INPUT "CR",                INPUT Ahorros.Nit,              INPUT Ahorros.Cue_Ahorros, 
                             INPUT Che_Transito.Cheque, INPUT Che_Transito.Cheque,      INPUT Ahorros.Agencia,           
                             INPUT Che_Transito.Agencia,INPUT Che_Transito.Ofi_Destino, INPUT "1",                 
                             INPUT W_Usuario,           INPUT T_Valor,                  INPUT 0).
            ASSIGN Creditos.Int_MorCobrar = Creditos.Int_MorCobrar - T_Valor
                   T_Inter = T_Inter + T_Valor
                   T_Valor = 0.
         END.
      END.
      IF Creditos.Int_difCobro GT 0 AND T_Valor NE 0 THEN DO:
         FIND Operacion WHERE Operacion.Cod_Operacion EQ 010101008 NO-LOCK NO-ERROR.
         IF AVAILABLE(Operacion) AND Operacion.Comprobante NE 0 THEN DO:
            ASSIGN Cbte = Operacion.Comprobante.
         END.
         ELSE DO:
            RUN MostrarMensaje IN W_Manija (INPUT 143,OUTPUT W_Error).
            RETURN ERROR.
         END.
         RUN Cbte_Agencia IN W_Manija (INPUT  Cbte,  INPUT  Ahorros.Agencia,
                                       OUTPUT Docto, OUTPUT W_Error).
         IF W_Error THEN DO:
            RUN MostrarMensaje IN W_Manija (INPUT 144, OUTPUT W_Eleccion).
            RETURN ERROR.
         END.
         IF T_Valor GT Creditos.Int_difCobro THEN DO:
            RUN Gra_MovAhorros(INPUT 010101008, INPUT ahorros.cod_ahorro, INPUT Ahorros.Cue_Ahorros, INPUT Che_Transito.Cheque,
                               INPUT Che_Transito.Agencia,INPUT W_Agencia, INPUT Che_Transito.Agencia, INPUT W_Usuario, 
                               INPUT Creditos.Int_difCobro,INPUT 0).
            RUN Gra_MovContable(INPUT Creditos.Agencia, INPUT Cbte,INPUT W_DifCdb,  INPUT W_Fecha, 
                                INPUT "Cerrar Cuenta Dificil Cobro",INPUT W_Usuario,   
                                INPUT 0, INPUT Creditos.Int_difCobro,    INPUT 999,       INPUT Docto,    
                                INPUT TODAY, INPUT TIME).
            RUN Gra_MovContable(INPUT Creditos.Agencia, INPUT Cbte,INPUT W_DifCcr,  INPUT W_Fecha, 
                                INPUT "Cerrar Cuenta Dificil Cobro",INPUT W_Usuario,   
                                INPUT Creditos.Int_difCobro, INPUT 0,   INPUT 999,         INPUT Docto,    
                                INPUT TODAY, INPUT TIME).
            RUN Gra_Taquilla(INPUT W_Usuario,           INPUT Che_Transito.Cod_Compensa,INPUT 010101017, 
                             INPUT ahorros.cod_ahorro,INPUT W_LiqIntCr,               INPUT CtaCble,
                             INPUT "CR",                INPUT Ahorros.Nit,              INPUT Ahorros.Cue_Ahorros, 
                             INPUT Che_Transito.Cheque, INPUT Che_Transito.Cheque,      INPUT Ahorros.Agencia,           
                             INPUT Che_Transito.Agencia,INPUT Che_Transito.Ofi_Destino, INPUT "1",                 
                             INPUT W_Usuario,           INPUT Creditos.Int_difCobro,    INPUT 0).
            ASSIGN T_Valor = T_Valor - Creditos.Int_difCobro
                   T_Inter = T_Inter + Creditos.Int_difCobro
                   Creditos.Int_difCobro = 0.
         END.
         ELSE DO:
            RUN Gra_MovAhorros(INPUT 010101008, INPUT ahorros.cod_ahorro, INPUT Ahorros.Cue_Ahorros, INPUT Che_Transito.Cheque,
                               INPUT Che_Transito.Agencia,INPUT W_Agencia, INPUT Che_Transito.Agencia, INPUT W_Usuario, 
                               INPUT Creditos.Int_difCobro,INPUT 0).
            RUN Gra_MovContable(INPUT Creditos.Agencia, INPUT Cbte,INPUT W_DifCdb,  INPUT W_Fecha, 
                                INPUT "Cerrar Cuenta Dificil Cobro",INPUT W_Usuario,   
                                INPUT 0, INPUT T_Valor,    INPUT 999,       INPUT Docto,    
                                INPUT TODAY, INPUT TIME).
            RUN Gra_MovContable(INPUT Creditos.Agencia, INPUT Cbte,INPUT W_DifCcr,  INPUT W_Fecha, 
                                INPUT "Cerrar Cuenta Dificil Cobro",INPUT W_Usuario,   
                                INPUT T_Valor, INPUT 0,   INPUT 999,         INPUT Docto,    
                                INPUT TODAY, INPUT TIME).
            RUN Gra_Taquilla(INPUT W_Usuario,           INPUT Che_Transito.Cod_Compensa,INPUT 010101017, 
                             INPUT ahorros.cod_ahorro,INPUT W_LiqIntCr,               INPUT CtaCble,
                             INPUT "CR",                INPUT Ahorros.Nit,              INPUT Ahorros.Cue_Ahorros, 
                             INPUT Che_Transito.Cheque, INPUT Che_Transito.Cheque,      INPUT Ahorros.Agencia,           
                             INPUT Che_Transito.Agencia,INPUT Che_Transito.Ofi_Destino, INPUT "1",                 
                             INPUT W_Usuario,           INPUT T_Valor,    INPUT 0).
            ASSIGN Creditos.Int_difCobro = Creditos.Int_difCobro - T_Valor
                   T_Inter = T_Inter + T_Valor
                   T_Valor = 0.
         END.
      END.
      IF Creditos.Int_MorCobrar GT 0 AND T_Valor NE 0 THEN DO:
         IF T_Valor GT Creditos.Int_MorCobrar THEN DO:
            RUN Gra_MovAhorros(INPUT 010101007, INPUT ahorros.cod_ahorro, INPUT Ahorros.Cue_Ahorros, INPUT Che_Transito.Cheque,
                               INPUT Che_Transito.Agencia,INPUT W_Agencia, INPUT Che_Transito.Agencia, INPUT W_Usuario, 
                               INPUT Creditos.Int_MorCobrar,INPUT 0).
            RUN Gra_Taquilla(INPUT W_Usuario,           INPUT Che_Transito.Cod_Compensa,INPUT 010101016, 
                             INPUT ahorros.cod_ahorro,INPUT W_LiqIntDb,               INPUT CtaCble,
                             INPUT "CR",                INPUT Ahorros.Nit,              INPUT Ahorros.Cue_Ahorros, 
                             INPUT Che_Transito.Cheque, INPUT Che_Transito.Cheque,      INPUT Ahorros.Agencia,           
                             INPUT Che_Transito.Agencia,INPUT Che_Transito.Ofi_Destino, INPUT "1",                 
                             INPUT W_Usuario,           INPUT Creditos.Int_MorCobrar,     INPUT 0).
            ASSIGN T_Valor = T_Valor - Creditos.Int_MorCobrar
                   T_Inter = T_Inter + Creditos.Int_MorCobrar
                   Creditos.Int_MorCobrar = 0.
         END.
         ELSE DO:
            RUN Gra_MovAhorros(INPUT 010101007, INPUT ahorros.cod_ahorro, INPUT Ahorros.Cue_Ahorros, INPUT Che_Transito.Cheque,
                               INPUT Che_Transito.Agencia,INPUT W_Agencia, INPUT Che_Transito.Agencia, INPUT W_Usuario, 
                               INPUT T_Valor,INPUT 0).
            RUN Gra_Taquilla(INPUT W_Usuario,           INPUT Che_Transito.Cod_Compensa,INPUT 010101016, 
                             INPUT ahorros.cod_ahorro,INPUT W_LiqIntDb,               INPUT CtaCble,
                             INPUT "CR",                INPUT Ahorros.Nit,              INPUT Ahorros.Cue_Ahorros, 
                             INPUT Che_Transito.Cheque, INPUT Che_Transito.Cheque,      INPUT Ahorros.Agencia,           
                             INPUT Che_Transito.Agencia,INPUT Che_Transito.Ofi_Destino, INPUT "1",                 
                             INPUT W_Usuario,           INPUT T_Valor,                  INPUT 0).
            ASSIGN Creditos.Int_MorCobrar = Creditos.Int_MorCobrar - T_Valor
                   T_Inter = T_Inter + T_Valor
                   T_Valor = 0.
         END.
      END.
      IF Creditos.Int_Corrientes GT 0 AND T_Valor NE 0 THEN DO:
         IF T_Valor GT Creditos.Int_Corrientes THEN DO:
            RUN Gra_MovAhorros(INPUT 010101005, INPUT ahorros.cod_ahorro, INPUT Ahorros.Cue_Ahorros, INPUT Che_Transito.Cheque,
                               INPUT Che_Transito.Agencia,INPUT W_Agencia, INPUT Che_Transito.Agencia, INPUT W_Usuario, 
                               INPUT Creditos.Int_Corrientes,INPUT 0).
            RUN Gra_Taquilla(INPUT W_Usuario,           INPUT Che_Transito.Cod_Compensa,INPUT 010101014, 
                             INPUT ahorros.cod_ahorro,INPUT W_LiqIntDb,               INPUT CtaCble,
                             INPUT "CR",                INPUT Ahorros.Nit,              INPUT Ahorros.Cue_Ahorros, 
                             INPUT Che_Transito.Cheque, INPUT Che_Transito.Cheque,      INPUT Ahorros.Agencia,           
                             INPUT Che_Transito.Agencia,INPUT Che_Transito.Ofi_Destino, INPUT "1",                 
                             INPUT W_Usuario,           INPUT Creditos.Int_Corrientes,  INPUT 0).
            ASSIGN T_Valor = T_Valor - Creditos.Int_Corrientes
                   T_Inter = T_Inter + Creditos.Int_Corrientes
                   Creditos.Int_Corrientes = 0.
         END.
         ELSE DO:
            RUN Gra_MovAhorros(INPUT 010101005, INPUT ahorros.cod_ahorro, INPUT Ahorros.Cue_Ahorros, INPUT Che_Transito.Cheque,
                               INPUT Che_Transito.Agencia,INPUT W_Agencia, INPUT Che_Transito.Agencia, INPUT W_Usuario, 
                               INPUT T_Valor,INPUT 0).
            RUN Gra_Taquilla(INPUT W_Usuario,           INPUT Che_Transito.Cod_Compensa,INPUT 010101014, 
                             INPUT ahorros.cod_ahorro,INPUT W_LiqIntDb,               INPUT CtaCble,
                             INPUT "CR",                INPUT Ahorros.Nit,              INPUT Ahorros.Cue_Ahorros, 
                             INPUT Che_Transito.Cheque, INPUT Che_Transito.Cheque,      INPUT Ahorros.Agencia,           
                             INPUT Che_Transito.Agencia,INPUT Che_Transito.Ofi_Destino, INPUT "1",                 
                             INPUT W_Usuario,           INPUT T_Valor,                  INPUT 0).
            ASSIGN Creditos.Int_Corrientes = Creditos.Int_Corrientes - T_Valor
                   T_Inter = T_Inter + T_Valor
                   T_Valor = 0.
         END.
      END.
      IF Creditos.Sdo_Capital GT 0 AND T_Valor NE 0 THEN DO:
         IF T_Valor GT Creditos.Sdo_Capital THEN DO:
            ASSIGN T_Difer = T_Valor - Creditos.Sdo_Capital.
            RUN Buscar_Cuenta(INPUT 2,INPUT credito.cod_credito,INPUT Creditos.Plazo,INPUT Ahorros.Nit,OUTPUT AuxCta) NO-ERROR.
            IF ERROR-STATUS:ERROR OR AuxCta EQ "" THEN DO:
               RUN MostrarMensaje IN W_Manija (INPUT 66,OUTPUT W_Error).
               RETURN ERROR.
            END.
            RUN Gra_Taquilla(INPUT W_Usuario,           INPUT Che_Transito.Cod_Compensa,INPUT 010101004, 
                             INPUT ahorros.cod_ahorro,INPUT AuxCta,                   INPUT CtaCble,
                             INPUT "CR",                INPUT Ahorros.Nit,              INPUT Ahorros.Cue_Ahorros, 
                             INPUT Che_Transito.Cheque, INPUT Che_Transito.Cheque,      INPUT Ahorros.Agencia,           
                             INPUT Che_Transito.Agencia,INPUT Che_Transito.Ofi_Destino, INPUT "1",                 
                             INPUT W_Usuario,           INPUT Creditos.Sdo_Capital,     INPUT 0).
            ASSIGN Creditos.Sdo_Capital = 0.
         END.
         ELSE DO:
            RUN Buscar_Cuenta(INPUT 2,INPUT credito.cod_credito,INPUT Creditos.Plazo,INPUT Ahorros.Nit,OUTPUT AuxCta) NO-ERROR.
            IF ERROR-STATUS:ERROR OR AuxCta EQ "" THEN DO:
               RUN MostrarMensaje IN W_Manija (INPUT 66,OUTPUT W_Error).
               RETURN ERROR.
            END.
            RUN Gra_Taquilla(INPUT W_Usuario,           INPUT Che_Transito.Cod_Compensa,INPUT 010101004, 
                             INPUT ahorros.cod_ahorro,INPUT AuxCta,                   INPUT CtaCble,
                             INPUT "CR",                INPUT Ahorros.Nit,              INPUT Ahorros.Cue_Ahorros, 
                             INPUT Che_Transito.Cheque, INPUT Che_Transito.Cheque,      INPUT Ahorros.Agencia,           
                             INPUT Che_Transito.Agencia,INPUT Che_Transito.Ofi_Destino, INPUT "1",                 
                             INPUT W_Usuario,           INPUT T_Valor,                  INPUT 0).
            ASSIGN Creditos.Sdo_Capital = Creditos.Sdo_Capital - T_Valor.
         END.
      END.
      ASSIGN Ahorros.Sdo_Canje          = Ahorros.Sdo_Canje - Che_Transito.Valor
             Ahorros.Sdo_Disponible     = Ahorros.Sdo_Disponible + Che_Transito.Valor + W_Int - T_Inter - W_ValCanje
             Ahorros.Fec_Ulttransaccion = TODAY
             Ahorros.Sal_Intpagados     = Ahorros.Sal_Intpagados + W_Int.
   END.
   ELSE DO:
      MESSAGE "No Hay Producto de Crédito Asociado a la Cuenta."
      VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
      TITLE "Error en Taquilla.".
      RETURN ERROR.
   END.
   RELEASE Creditos.
   RELEASE Taquilla.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Win 
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

