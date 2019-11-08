&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME

&Scoped-define WINDOW-NAME W-InverS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-InverS 

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
ON RETURN TAB.

{Incluido\Variable.I "SHARED"}

DEFINE VAR W_RowId AS ROWID.
DEFINE VAR W_CheProfor AS CHARACTER.
DEFINE VAR W_SiCheq AS LOGICAL.
DEFINE VAR W_RetFte AS DECIMAL.
DEFINE VAR W_Plazo AS INTEGER.
DEFINE VAR W_Tasa AS DECIMAL.
DEFINE VAR W_NitI AS CHARACTER.
DEFINE VAR W_NitF AS CHARACTER.
DEFINE VAR W_NitEmi AS CHARACTER.
DEFINE VAR W_NomEmi AS CHARACTER.
DEFINE VAR W_Nat AS CHARACTER.
DEFINE VAR W_Ctr AS LOGICAL.
DEFINE VAR W_Age AS INTEGER.
DEFINE VAR W_Ope AS CHARACTER FORMAT "X(1)".
DEFINE VAR W_RetGMF AS DECIMAL.
DEFINE VAR W_Canc AS LOGICAL.
DEFINE VAR W_IntFut AS DECIMAL.
DEFINE VAR W_Tot AS DECIMAL EXTENT 9.

   /* oakley */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Canc
&Scoped-define BROWSE-NAME Br_Inv

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Inversion_Sdos

/* Definitions for BROWSE Br_Inv                                        */
&Scoped-define FIELDS-IN-QUERY-Br_Inv Inversion_Sdos.Agencia Inversion_Sdos.Categ Inversion_Sdos.Cod_Produc Inversion_Sdos.Nit_Emi Inversion_Sdos.Nombre Inversion_Sdos.Nro_Titulo Inversion_Sdos.Secuencia Inversion_Sdos.Fec_Apertura Inversion_Sdos.VrInvers_Inic Inversion_Sdos.Sdo_Actual Inversion_Sdos.Estado Inversion_Sdos.Tasa_NomiAnual Inversion_Sdos.Plazo_Dias Inversion_Sdos.Fec_Vcto Inversion_Sdos.Interes_Causad Inversion_Sdos.Interes_Recibi Inversion_Sdos.Dias_Cau Inversion_Sdos.Int_Pactado Inversion_Sdos.Vr_Provision Inversion_Sdos.Descrip   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Inv   
&Scoped-define SELF-NAME Br_Inv
&Scoped-define OPEN-QUERY-Br_Inv /*OPEN QUERY Br_Inv FOR EACH Inversion_Sdos NO-LOCK            BY Inversion_Sdos.Agencia    BY Inversion_Sdos.Nit            BY Inversion_Sdos.Nro_Titulo BY Inversion_Sdos.Secuen.*/.
&Scoped-define TABLES-IN-QUERY-Br_Inv Inversion_Sdos
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Inv Inversion_Sdos


/* Definitions for FRAME F_Cons                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Cons ~
    ~{&OPEN-QUERY-Br_Inv}

/* Definitions for FRAME F_Invers                                       */
&Scoped-define FIELDS-IN-QUERY-F_Invers Inversion_Sdos.Nro_Unidades ~
Inversion_Sdos.Tasa_NomiAMcdo Inversion_Sdos.Valor_Unidad ~
Inversion_Sdos.Agencia Inversion_Sdos.Nit_Emisor Inversion_Sdos.Nro_Titulo ~
Inversion_Sdos.Secuencia Inversion_Sdos.Nombre_Emisor ~
Inversion_Sdos.Fec_Apertura Inversion_Sdos.Calif_Emisor ~
Inversion_Sdos.Calif_Invers Inversion_Sdos.Cod_Producto ~
Inversion_Sdos.TipTasa_FV Inversion_Sdos.Indicador ~
Inversion_Sdos.Puntos_Adic Inversion_Sdos.Tasa_NomiAnual ~
Inversion_Sdos.LiqInt_Dias Inversion_Sdos.Plazo_Dias ~
Inversion_Sdos.VrInvers_Inic Inversion_Sdos.Categoria ~
Inversion_Sdos.Fec_Prorroga Inversion_Sdos.Cuenta_Contab ~
Inversion_Sdos.Descrip Inversion_Sdos.Fec_Vcto Inversion_Sdos.Sdo_Actual ~
Inversion_Sdos.Interes_Recibido Inversion_Sdos.Interes_Causado 
&Scoped-define ENABLED-FIELDS-IN-QUERY-F_Invers Inversion_Sdos.Nro_Unidades ~
Inversion_Sdos.Nit_Emisor Inversion_Sdos.Nro_Titulo ~
Inversion_Sdos.Secuencia Inversion_Sdos.Fec_Apertura ~
Inversion_Sdos.Calif_Invers Inversion_Sdos.Cod_Producto ~
Inversion_Sdos.Tasa_NomiAnual Inversion_Sdos.LiqInt_Dias ~
Inversion_Sdos.Plazo_Dias Inversion_Sdos.VrInvers_Inic ~
Inversion_Sdos.Categoria Inversion_Sdos.Fec_Prorroga Inversion_Sdos.Descrip ~
Inversion_Sdos.Fec_Vcto Inversion_Sdos.Interes_Recibido ~
Inversion_Sdos.Interes_Causado 
&Scoped-define ENABLED-TABLES-IN-QUERY-F_Invers Inversion_Sdos
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-F_Invers Inversion_Sdos
&Scoped-define QUERY-STRING-F_Invers FOR EACH Inversion_Sdos SHARE-LOCK
&Scoped-define OPEN-QUERY-F_Invers OPEN QUERY F_Invers FOR EACH Inversion_Sdos SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-F_Invers Inversion_Sdos
&Scoped-define FIRST-TABLE-IN-QUERY-F_Invers Inversion_Sdos


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS W_IntReal Btn_ContCanc Btn_Reg 
&Scoped-Define DISPLAYED-OBJECTS W_CtaPpal DescCta W_Debe W_Haber ~
W_CtaPpal-2 DescCta-2 W_Debe-2 W_Haber-2 W_CtaPpal-3 DescCta-3 W_Debe-3 ~
W_Haber-3 W_CtaPpal-4 DescCta-4 W_Debe-4 W_Haber-4 W_CtaPpal-5 DescCta-5 ~
W_Debe-5 W_Haber-5 W_CtaPpal-6 DescCta-6 W_Debe-6 W_Haber-6 W_CtaPpal-7 ~
DescCta-7 W_Debe-7 W_Haber-7 W_CtaPpal-8 DescCta-8 W_Debe-8 W_Haber-8 ~
W_IntReal 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-InverS AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_ContCanc 
     LABEL "&Contabilizar" 
     SIZE 10.43 BY .92 TOOLTIP "Si las partidas y cuentas son correctas puede Contabilizar".

DEFINE BUTTON Btn_Reg 
     LABEL "&Regresar" 
     SIZE 10.57 BY .92 TOOLTIP "Permite regresar a la ventana principal, no se contabiliza".

DEFINE VARIABLE DescCta AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 26.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE DescCta-2 AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 26.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE DescCta-3 AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 26.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE DescCta-4 AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 26.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE DescCta-5 AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 26.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE DescCta-6 AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 26.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE DescCta-7 AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 26.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE DescCta-8 AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 26.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CtaPpal AS CHARACTER FORMAT "X(14)":U 
     VIEW-AS FILL-IN 
     SIZE 12.14 BY .81 TOOLTIP "Cta-Contable de la Cancelación"
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CtaPpal-2 AS CHARACTER FORMAT "X(14)":U 
     VIEW-AS FILL-IN 
     SIZE 12.14 BY .81 TOOLTIP "Cta-Contable de la Cancelación"
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CtaPpal-3 AS CHARACTER FORMAT "X(14)":U 
     VIEW-AS FILL-IN 
     SIZE 12.14 BY .81 TOOLTIP "Cta-Contable de la Cancelación"
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CtaPpal-4 AS CHARACTER FORMAT "X(14)":U 
     VIEW-AS FILL-IN 
     SIZE 12.14 BY .81 TOOLTIP "Cta-Contable de la Cancelación"
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CtaPpal-5 AS CHARACTER FORMAT "X(14)":U 
     VIEW-AS FILL-IN 
     SIZE 12.14 BY .81 TOOLTIP "Cta-Contable de la Cancelación"
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CtaPpal-6 AS CHARACTER FORMAT "X(14)":U 
     VIEW-AS FILL-IN 
     SIZE 12.14 BY .81 TOOLTIP "Cta-Contable de la Cancelación"
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CtaPpal-7 AS CHARACTER FORMAT "X(14)":U 
     VIEW-AS FILL-IN 
     SIZE 12.14 BY .81 TOOLTIP "Cta-Contable de la Cancelación"
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CtaPpal-8 AS CHARACTER FORMAT "X(14)":U 
     VIEW-AS FILL-IN 
     SIZE 12.14 BY .81 TOOLTIP "Cta-Contable de la Cancelación"
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Debe AS DECIMAL FORMAT ">>>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     BGCOLOR 15 FGCOLOR 3  NO-UNDO.

DEFINE VARIABLE W_Debe-2 AS DECIMAL FORMAT ">>>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     BGCOLOR 15 FGCOLOR 3  NO-UNDO.

DEFINE VARIABLE W_Debe-3 AS DECIMAL FORMAT ">>>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     BGCOLOR 15 FGCOLOR 3  NO-UNDO.

DEFINE VARIABLE W_Debe-4 AS DECIMAL FORMAT ">>>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     BGCOLOR 15 FGCOLOR 3  NO-UNDO.

DEFINE VARIABLE W_Debe-5 AS DECIMAL FORMAT ">>>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     BGCOLOR 15 FGCOLOR 3  NO-UNDO.

DEFINE VARIABLE W_Debe-6 AS DECIMAL FORMAT ">>>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     BGCOLOR 15 FGCOLOR 3  NO-UNDO.

DEFINE VARIABLE W_Debe-7 AS DECIMAL FORMAT ">>>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     BGCOLOR 15 FGCOLOR 3  NO-UNDO.

DEFINE VARIABLE W_Debe-8 AS DECIMAL FORMAT ">>>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     BGCOLOR 15 FGCOLOR 3  NO-UNDO.

DEFINE VARIABLE W_Haber AS DECIMAL FORMAT ">>>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     BGCOLOR 15 FGCOLOR 3  NO-UNDO.

DEFINE VARIABLE W_Haber-2 AS DECIMAL FORMAT ">>>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     BGCOLOR 15 FGCOLOR 3  NO-UNDO.

DEFINE VARIABLE W_Haber-3 AS DECIMAL FORMAT ">>>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     BGCOLOR 15 FGCOLOR 3  NO-UNDO.

DEFINE VARIABLE W_Haber-4 AS DECIMAL FORMAT ">>>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     BGCOLOR 15 FGCOLOR 3  NO-UNDO.

DEFINE VARIABLE W_Haber-5 AS DECIMAL FORMAT ">>>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     BGCOLOR 15 FGCOLOR 3  NO-UNDO.

DEFINE VARIABLE W_Haber-6 AS DECIMAL FORMAT ">>>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     BGCOLOR 15 FGCOLOR 3  NO-UNDO.

DEFINE VARIABLE W_Haber-7 AS DECIMAL FORMAT ">>>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     BGCOLOR 15 FGCOLOR 3  NO-UNDO.

DEFINE VARIABLE W_Haber-8 AS DECIMAL FORMAT ">>>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     BGCOLOR 15 FGCOLOR 3  NO-UNDO.

DEFINE VARIABLE W_IntReal AS DECIMAL FORMAT ">>>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Interés Real Liquidado" 
     VIEW-AS FILL-IN 
     SIZE 12.29 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE BUTTON Btn_Salir-2 
     IMAGE-UP FILE "Imagenes/volver.bmp":U
     LABEL "&Regresar" 
     SIZE 10 BY 1.62.

DEFINE VARIABLE W_NitEmiC AS CHARACTER FORMAT "X(12)":U 
     LABEL "Por Nit Emisor" 
     VIEW-AS FILL-IN 
     SIZE 13.72 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_NroTitC AS CHARACTER FORMAT "X(14)":U 
     LABEL "Por Nro.de Tìtulo" 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE Tg_Todos AS LOGICAL INITIAL no 
     LABEL "Todas las Inversiones" 
     VIEW-AS TOGGLE-BOX
     SIZE 17.86 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE BUTTON Btn_FinImp 
     IMAGE-UP FILE "Imagenes/volver.bmp":U
     LABEL "&Regresar" 
     SIZE 9.57 BY 1.69 TOOLTIP "Regresar a la ventana principal de Inversiones".

DEFINE BUTTON Btn_Imp-2 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "&Imprimir" 
     SIZE 9.57 BY 1.73 TOOLTIP "Informe por vencimientos y Extracto por Inversión.".

DEFINE VARIABLE W_FecFin AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha  Final" 
     VIEW-AS FILL-IN 
     SIZE 11.43 BY .81 TOOLTIP "Fecha Final para el Filtro"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_FecIni AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Inicial" 
     VIEW-AS FILL-IN 
     SIZE 11.29 BY .81 TOOLTIP "Fecha Inicial para el Filtro"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_NitEnt AS CHARACTER FORMAT "X(12)":U INITIAL "0" 
     LABEL "Por Nit Entidad" 
     VIEW-AS FILL-IN 
     SIZE 13.29 BY .81 TOOLTIP "Teclee Nit Entidad Emisora, o blanco-cero para todos"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE Rs_Inf AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Extracto Inversión seleccionada", 1,
"Informe Inversiones por Fecha-Vencimientos", 2,
"Informe inversiones por Fecha-Aperturaa", 3,
"Informe inversiones Canceladas", 4,
"Participaciones por Entidad", 5,
"Resumen por Productos", 6
     SIZE 30.57 BY 4.12 TOOLTIP "Marque el informe solicitado"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE BUTTON Btn_Apert 
     LABEL "&Apertura" 
     SIZE 10 BY 1.62 TOOLTIP "Presione para Apertura de Inversiones".

DEFINE BUTTON Btn_ayuda 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "" 
     SIZE 5.14 BY 1.12.

DEFINE BUTTON Btn_Canc 
     LABEL "&Cancelar" 
     SIZE 10 BY 1.62.

DEFINE BUTTON Btn_Consul 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     LABEL "&Consultar" 
     SIZE 10 BY 1.62.

DEFINE BUTTON Btn_Imp 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "&Imprimir" 
     SIZE 10 BY 1.62 TOOLTIP "Informe por vencimientos y Extracto por Inversión.".

DEFINE BUTTON Btn_Salir 
     LABEL "&Salir" 
     SIZE 10 BY 1.62.

DEFINE BUTTON Btn_Salva 
     LABEL "&Salvar" 
     SIZE 10 BY 1.62 TOOLTIP "Actualiza Modificaciones y Contabiliza Aperturas y Cancelaciones".

DEFINE BUTTON BUTTON-5 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 5" 
     SIZE 10 BY 1.62.

DEFINE VARIABLE Cmb_pdctos AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 29.29 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_OpeC AS CHARACTER FORMAT "X(40)":U 
     LABEL "Operaciòn" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEMS "","Apertura de Inversiòn","Cancelaciòn Total","Incremento de la Inversiòn","Modificaciòn","Prorroga de la Inversiòn","Rendimientos Parciales","Saldo-Inicial","Valor Parcial de Cancelaciòn" 
     DROP-DOWN-LIST
     SIZE 22.29 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_CancParc AS DECIMAL FORMAT ">>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Cancelaciòn Parcial" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .73 TOOLTIP "Vr.Cancelaciòn parcial"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_ContraPart AS CHARACTER FORMAT "X(14)":U 
     LABEL "Cta-Contrapartida" 
     VIEW-AS FILL-IN 
     SIZE 12.72 BY .73 TOOLTIP "Cta-Contable Contrapartida de la Apertura y/o Cancelación"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_CtaGMF AS CHARACTER FORMAT "X(14)":U 
     LABEL "Cuenta G.M.F." 
     VIEW-AS FILL-IN 
     SIZE 12.72 BY .73 TOOLTIP "Cuenta G.M.F."
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CtaRend AS CHARACTER FORMAT "X(14)":U 
     LABEL "Cta-Rendimientos" 
     VIEW-AS FILL-IN 
     SIZE 12.72 BY .73 TOOLTIP "Cta-Contable de Rendimientos"
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Debe1 AS DECIMAL FORMAT ">>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.14 BY .73
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Debe2 AS DECIMAL FORMAT ">>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.14 BY .73
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Debe3 AS DECIMAL FORMAT ">>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.14 BY .73
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Debe4 AS DECIMAL FORMAT ">>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.14 BY .73
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_DesCrip AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 41.72 BY .81 TOOLTIP "Descripción de la Transacción o de la modificación"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_Haber1 AS DECIMAL FORMAT ">>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.14 BY .73
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Haber2 AS DECIMAL FORMAT ">>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.14 BY .73
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Haber3 AS DECIMAL FORMAT ">>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.14 BY .73
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Haber4 AS DECIMAL FORMAT ">>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.14 BY .73
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_IntFuturo AS DECIMAL FORMAT ">>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Interès por Liquidar" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .77 TOOLTIP "Interès por Liquidar"
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_IntPag AS DECIMAL FORMAT ">>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Interès Recibido parcial" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .73 TOOLTIP "Interés Recibido parcial"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_NomCta AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 27.57 BY .73
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomCta2 AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 27.57 BY .73
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomCta3 AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 27.57 BY .73
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomCta4 AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 27.57 BY .73
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomInd AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 22.86 BY .65
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NroCheq AS CHARACTER FORMAT "X(8)":U 
     LABEL "Nro.de Cheque" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .77 TOOLTIP "Nro.de Cheque"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_UnidXV AS DECIMAL FORMAT "->>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Unidades por Valor Actual" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .69 TOOLTIP "Unidades por Valor Actual"
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 94.29 BY 20.31.

DEFINE RECTANGLE RECT-306
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 45.72 BY 9.65.

DEFINE RECTANGLE RECT-307
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 12.43 BY 5.46.

DEFINE RECTANGLE RECT-308
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 12.29 BY 7.27.

DEFINE RECTANGLE RECT-311
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 35.86 BY 8.04.

DEFINE RECTANGLE RECT-312
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 85.43 BY 6.04.

DEFINE RECTANGLE RECT-313
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 12.72 BY .96.

DEFINE VARIABLE Tg_GMF AS LOGICAL INITIAL no 
     LABEL "Impto G.M.F" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY .73 TOOLTIP "Marque Solo si la Cancelaciòn Total/Parcial lo Lleva"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Br_Inv FOR 
      Inversion_Sdos SCROLLING.

DEFINE QUERY F_Invers FOR 
      Inversion_Sdos SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Br_Inv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Inv W-InverS _FREEFORM
  QUERY Br_Inv DISPLAY
      Inversion_Sdos.Agencia     COLUMN-LABEL "Ag."
   Inversion_Sdos.Categ          COLUMN-LABEL "C"
   Inversion_Sdos.Cod_Produc     COLUMN-LABEL "Pcto"
   Inversion_Sdos.Nit_Emi
   Inversion_Sdos.Nombre         FORM "X(30)"
   Inversion_Sdos.Nro_Titulo
   Inversion_Sdos.Secuencia      COLUMN-LABEL "Sc"
   Inversion_Sdos.Fec_Apertura
   Inversion_Sdos.VrInvers_Inic  COLUMN-LABEL "Valor Apertura"   FORM "->>>>,>>>,>>9.99"
   Inversion_Sdos.Sdo_Actual     COLUMN-LABEL "Vr.Actual Invers" FORM "->>>>,>>>,>>9.99"
   Inversion_Sdos.Estado         COLUMN-LABEL "E"
   Inversion_Sdos.Tasa_NomiAnual COLUMN-LABEL "Tasa NAnual"
   Inversion_Sdos.Plazo_Dias     COLUMN-LABEL "PlazoD"
   Inversion_Sdos.Fec_Vcto       COLUMN-LABEL "Fec-Vecto."
   Inversion_Sdos.Interes_Causad COLUMN-LABEL "Interés Liquidado"
   Inversion_Sdos.Interes_Recibi COLUMN-LABEL "Interés Pagado"
   Inversion_Sdos.Dias_Cau       COLUMN-LABEL "DiasC"
   Inversion_Sdos.Int_Pactado    COLUMN-LABEL "Int_Pactado a Hoy"
   Inversion_Sdos.Vr_Provision   COLUMN-LABEL "Vr-Provisión"
   Inversion_Sdos.Descrip
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 91.72 BY 8.85
         BGCOLOR 15 FGCOLOR 7 FONT 4 ROW-HEIGHT-CHARS .42 TOOLTIP "Con DobleClick Captura la Inversión seleccionada SOLO si está vigente".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Cons
     Br_Inv AT ROW 1.23 COL 1.43
     Btn_Salir-2 AT ROW 10.65 COL 80.86
     W_NitEmiC AT ROW 10.85 COL 11 COLON-ALIGNED
     W_NroTitC AT ROW 10.88 COL 40.14 COLON-ALIGNED
     Tg_Todos AT ROW 10.92 COL 59.29
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2.43 ROW 8.5
         SIZE 92.86 BY 12.42
         BGCOLOR 17 FGCOLOR 0 FONT 4
         TITLE "Consulta Inversiones,  Con Dbl-Click Captura la Selecciòn SOLO Vigentes".

DEFINE FRAME F_Canc
     W_CtaPpal AT ROW 1.77 COL 2 NO-LABEL
     DescCta AT ROW 1.77 COL 12.57 COLON-ALIGNED NO-LABEL
     W_Debe AT ROW 1.77 COL 39.57 COLON-ALIGNED NO-LABEL
     W_Haber AT ROW 1.77 COL 53 COLON-ALIGNED NO-LABEL
     W_CtaPpal-2 AT ROW 2.69 COL 2 NO-LABEL
     DescCta-2 AT ROW 2.69 COL 12.57 COLON-ALIGNED NO-LABEL
     W_Debe-2 AT ROW 2.69 COL 39.57 COLON-ALIGNED NO-LABEL
     W_Haber-2 AT ROW 2.69 COL 53 COLON-ALIGNED NO-LABEL
     W_CtaPpal-3 AT ROW 3.62 COL 2 NO-LABEL
     DescCta-3 AT ROW 3.62 COL 12.57 COLON-ALIGNED NO-LABEL
     W_Debe-3 AT ROW 3.62 COL 39.57 COLON-ALIGNED NO-LABEL
     W_Haber-3 AT ROW 3.62 COL 53 COLON-ALIGNED NO-LABEL
     W_CtaPpal-4 AT ROW 4.5 COL 2 NO-LABEL
     DescCta-4 AT ROW 4.5 COL 12.57 COLON-ALIGNED NO-LABEL
     W_Debe-4 AT ROW 4.5 COL 39.57 COLON-ALIGNED NO-LABEL
     W_Haber-4 AT ROW 4.5 COL 53 COLON-ALIGNED NO-LABEL
     W_CtaPpal-5 AT ROW 5.46 COL 1.86 NO-LABEL
     DescCta-5 AT ROW 5.46 COL 12.43 COLON-ALIGNED NO-LABEL
     W_Debe-5 AT ROW 5.46 COL 39.43 COLON-ALIGNED NO-LABEL
     W_Haber-5 AT ROW 5.46 COL 52.86 COLON-ALIGNED NO-LABEL
     W_CtaPpal-6 AT ROW 6.38 COL 1.86 NO-LABEL
     DescCta-6 AT ROW 6.38 COL 12.43 COLON-ALIGNED NO-LABEL
     W_Debe-6 AT ROW 6.38 COL 39.43 COLON-ALIGNED NO-LABEL
     W_Haber-6 AT ROW 6.38 COL 52.86 COLON-ALIGNED NO-LABEL
     W_CtaPpal-7 AT ROW 7.31 COL 1.86 NO-LABEL
     DescCta-7 AT ROW 7.31 COL 12.43 COLON-ALIGNED NO-LABEL
     W_Debe-7 AT ROW 7.31 COL 39.43 COLON-ALIGNED NO-LABEL
     W_Haber-7 AT ROW 7.31 COL 52.86 COLON-ALIGNED NO-LABEL
     W_CtaPpal-8 AT ROW 8.19 COL 1.86 NO-LABEL
     DescCta-8 AT ROW 8.19 COL 12.43 COLON-ALIGNED NO-LABEL
     W_Debe-8 AT ROW 8.19 COL 39.43 COLON-ALIGNED NO-LABEL
     W_Haber-8 AT ROW 8.19 COL 52.86 COLON-ALIGNED NO-LABEL
     W_IntReal AT ROW 9.54 COL 15.86 COLON-ALIGNED
     Btn_ContCanc AT ROW 9.58 COL 42.29
     Btn_Reg AT ROW 9.58 COL 55.72
     "                  HABER" VIEW-AS TEXT
          SIZE 15.57 BY .5 AT ROW 1.15 COL 51.29
          BGCOLOR 1 FGCOLOR 15 
     "Cta-Contable            Descripción-Cta                                     DEBE" VIEW-AS TEXT
          SIZE 53.29 BY .5 AT ROW 1.15 COL 1.86
          BGCOLOR 1 FGCOLOR 15 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 5.86 ROW 8.69
         SIZE 66.57 BY 10.5
         BGCOLOR 17 FGCOLOR 0 FONT 4
         TITLE "Contabilización de la Cancelación".

DEFINE FRAME F_Invers
     Inversion_Sdos.Nro_Unidades AT ROW 11.19 COL 37.43 COLON-ALIGNED
          LABEL "Nro.  de  Unidades" FORMAT "->>>,>>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 9.57 BY .73
          BGCOLOR 15 FGCOLOR 0 
     Inversion_Sdos.Tasa_NomiAMcdo AT ROW 12.08 COL 9.14 COLON-ALIGNED NO-LABEL FORMAT ">>9.9999999"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81 TOOLTIP "Tasa_Nominal Anual de Mercado"
          BGCOLOR 18 FGCOLOR 15 
     Tg_GMF AT ROW 20 COL 77.72
     Inversion_Sdos.Valor_Unidad AT ROW 12 COL 37.43 COLON-ALIGNED
          LABEL "Valor cada Unidad" FORMAT "->>>,>>>,>>9.9999"
          VIEW-AS FILL-IN 
          SIZE 9.72 BY .69
          BGCOLOR 15 FGCOLOR 0 
     W_OpeC AT ROW 1.65 COL 39.86 COLON-ALIGNED
     Btn_ayuda AT ROW 18.77 COL 102
     Btn_Canc AT ROW 14.5 COL 99.72
     Btn_Salir AT ROW 16.15 COL 99.72
     BUTTON-5 AT ROW 2.31 COL 99.72
     Inversion_Sdos.Agencia AT ROW 3.35 COL 6 NO-LABEL FORMAT "999"
          VIEW-AS FILL-IN 
          SIZE 4.14 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Inversion_Sdos.Nit_Emisor AT ROW 3.35 COL 8.29 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13.43 BY .81
          BGCOLOR 15 FGCOLOR 0 
     Inversion_Sdos.Nro_Titulo AT ROW 3.35 COL 22.29 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .81
          BGCOLOR 15 FGCOLOR 0 
     Inversion_Sdos.Secuencia AT ROW 3.38 COL 35.72 COLON-ALIGNED HELP
          "Secuencia: Para identificar números de títulos iguales" NO-LABEL FORMAT "99"
          VIEW-AS FILL-IN 
          SIZE 3.57 BY .81
     Inversion_Sdos.Nombre_Emisor AT ROW 3.38 COL 40 COLON-ALIGNED NO-LABEL FORMAT "X(40)"
          VIEW-AS FILL-IN 
          SIZE 39 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Inversion_Sdos.Fec_Apertura AT ROW 3.38 COL 79.14 COLON-ALIGNED NO-LABEL FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 9.86 BY .81
          BGCOLOR 15 FGCOLOR 0 
     Inversion_Sdos.Calif_Emisor AT ROW 4.31 COL 22.29 COLON-ALIGNED
          LABEL "Calificación del Emisor" FORMAT "X(3)"
          VIEW-AS FILL-IN 
          SIZE 4.86 BY .73 TOOLTIP "Calificación del Emisor"
          BGCOLOR 15 FGCOLOR 0 
     Inversion_Sdos.Calif_Invers AT ROW 4.38 COL 43.72 COLON-ALIGNED
          LABEL "Calific.de la Inversión" FORMAT "X(2)"
          VIEW-AS FILL-IN 
          SIZE 5.43 BY .65 TOOLTIP "Calificación de la Inversión"
          BGCOLOR 15 FGCOLOR 0 
     Inversion_Sdos.Cod_Producto AT ROW 6.73 COL 13.86 COLON-ALIGNED
          LABEL "Cod-Pdcto" FORMAT "999"
          VIEW-AS FILL-IN 
          SIZE 3.86 BY .77
          BGCOLOR 18 FGCOLOR 15 
     Cmb_pdctos AT ROW 6.73 COL 17.86 COLON-ALIGNED NO-LABEL
     Inversion_Sdos.TipTasa_FV AT ROW 8.77 COL 8.14 HELP
          "TipoTasa: 0 Fija, 1 Variable" NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Tasa Fija", 0,
"Variable", 1
          SIZE 17.86 BY .65 TOOLTIP "Tasa Fija = 0, Variable = 1."
          BGCOLOR 15 FGCOLOR 0 
     Inversion_Sdos.Indicador AT ROW 8.73 COL 41.57 COLON-ALIGNED
          LABEL "Código Indicador" FORMAT "99999"
          VIEW-AS FILL-IN 
          SIZE 5.43 BY .69 TOOLTIP "Código de Indicador para Tasa Variable"
          BGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 112.14 BY 20.73
         BGCOLOR 17 FGCOLOR 0 FONT 4.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F_Invers
     Inversion_Sdos.Puntos_Adic AT ROW 10.35 COL 11.29 COLON-ALIGNED NO-LABEL FORMAT "->9.999"
          VIEW-AS FILL-IN 
          SIZE 7 BY .62
          BGCOLOR 15 FGCOLOR 0 
     Inversion_Sdos.Tasa_NomiAnual AT ROW 10.23 COL 37.43 COLON-ALIGNED
          LABEL "Tasa Nomin.Anual" FORMAT ">>9.9999999"
          VIEW-AS FILL-IN 
          SIZE 9.57 BY .69
          BGCOLOR 15 FGCOLOR 0 
     Inversion_Sdos.LiqInt_Dias AT ROW 13.81 COL 11.29 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 7.14 BY .69 TOOLTIP "Liquidaciòn de Interès (Dias)"
          BGCOLOR 15 FGCOLOR 0 
     Inversion_Sdos.Plazo_Dias AT ROW 12.96 COL 42 COLON-ALIGNED
          LABEL "Plazo"
          VIEW-AS FILL-IN 
          SIZE 5 BY .69 TOOLTIP "Plazo en Dias"
          BGCOLOR 15 FGCOLOR 0 
     Inversion_Sdos.VrInvers_Inic AT ROW 5.65 COL 74 COLON-ALIGNED
          LABEL "Valor Inicial Inversión" FORMAT "->>>>,>>>,>>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 14 BY .77
          BGCOLOR 15 FGCOLOR 0 
     W_ContraPart AT ROW 16.77 COL 17.86 COLON-ALIGNED
     W_NroCheq AT ROW 19.92 COL 17.72 COLON-ALIGNED
     W_DesCrip AT ROW 19.92 COL 33.14 NO-LABEL
     Inversion_Sdos.Categoria AT ROW 5.69 COL 8.29 HELP
          "(1) Negociables, (2) Permanentes, (3) Disponibles" NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Negociables", 1,
"Permanentes", 2,
"Disponibles", 3
          SIZE 40.72 BY .62
          BGCOLOR 15 FGCOLOR 0 
     Inversion_Sdos.Fec_Prorroga AT ROW 4.31 COL 79.14 COLON-ALIGNED
          LABEL "Fecha de Prorroga" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 10 BY .77 TOOLTIP "Solo si el Titulo es prorrogado"
          BGCOLOR 15 FGCOLOR 0 
     Inversion_Sdos.Cuenta_Contab AT ROW 15.85 COL 7.28
          LABEL "Cta. de  Inversiòn"
          VIEW-AS FILL-IN 
          SIZE 12.72 BY .77
          BGCOLOR 18 FGCOLOR 15 
     Inversion_Sdos.Descrip AT ROW 7.62 COL 6 COLON-ALIGNED NO-LABEL FORMAT "X(25)"
          VIEW-AS FILL-IN 
          SIZE 41 BY .73
          BGCOLOR 15 FGCOLOR 0 
     Inversion_Sdos.Fec_Vcto AT ROW 13.73 COL 37.57 COLON-ALIGNED
          LABEL "Fecha Vencimiento"
          VIEW-AS FILL-IN 
          SIZE 9.43 BY .73
          BGCOLOR 15 FGCOLOR 0 
     Inversion_Sdos.Sdo_Actual AT ROW 7.46 COL 74 COLON-ALIGNED
          LABEL "Saldo Actual" FORMAT "->>>>,>>>,>>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 14 BY .73
          BGCOLOR 18 FGCOLOR 15 
     W_CancParc AT ROW 6.58 COL 74 COLON-ALIGNED
     Inversion_Sdos.Interes_Recibido AT ROW 9.31 COL 74 COLON-ALIGNED
          LABEL "Interès Pago(Acum.)" FORMAT "->>>>,>>>,>>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 14 BY .73 TOOLTIP "Solo si está Ingresando Saldos Teclee el Acumulado Recibido"
          BGCOLOR 18 FGCOLOR 15 
     Inversion_Sdos.Interes_Causado AT ROW 10.15 COL 74 COLON-ALIGNED
          LABEL "Rendimientos por Cobrar" FORMAT "->>>>,>>>,>>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 14 BY .77 TOOLTIP "Solo si está Ingresando Saldos Teclee el Acumulado"
          BGCOLOR 18 FGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 112.14 BY 20.73
         BGCOLOR 17 FGCOLOR 0 FONT 4.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F_Invers
     W_NomCta AT ROW 16.77 COL 33.14 NO-LABEL
     W_IntPag AT ROW 8.38 COL 74 COLON-ALIGNED
     Btn_Apert AT ROW 12.88 COL 99.72
     Btn_Salva AT ROW 11.27 COL 99.72
     Btn_Consul AT ROW 5.54 COL 99.72
     Btn_Imp AT ROW 3.92 COL 99.72
     W_NomInd AT ROW 9.42 COL 26.14 NO-LABEL
     W_NomCta2 AT ROW 15.88 COL 33.14 NO-LABEL
     W_Debe1 AT ROW 15.88 COL 60 COLON-ALIGNED NO-LABEL
     W_Haber1 AT ROW 15.88 COL 75 COLON-ALIGNED NO-LABEL
     W_Debe2 AT ROW 16.77 COL 60.14 COLON-ALIGNED NO-LABEL
     W_Haber2 AT ROW 16.77 COL 75.14 COLON-ALIGNED NO-LABEL
     W_CtaRend AT ROW 17.62 COL 17.86 COLON-ALIGNED
     W_NomCta3 AT ROW 17.62 COL 33.14 NO-LABEL
     W_Debe3 AT ROW 17.62 COL 60.14 COLON-ALIGNED NO-LABEL
     W_Haber3 AT ROW 17.62 COL 75.14 COLON-ALIGNED NO-LABEL
     W_CtaGMF AT ROW 18.46 COL 17.86 COLON-ALIGNED
     W_NomCta4 AT ROW 18.46 COL 33.14 NO-LABEL
     W_Debe4 AT ROW 18.46 COL 60.14 COLON-ALIGNED NO-LABEL
     W_Haber4 AT ROW 18.46 COL 75.14 COLON-ALIGNED NO-LABEL
     W_IntFuturo AT ROW 11.08 COL 73.86 COLON-ALIGNED
     W_UnidXV AT ROW 12.04 COL 73.86 COLON-ALIGNED
     "  D E B I T O S             C R E D I T O S" VIEW-AS TEXT
          SIZE 27.72 BY .5 AT ROW 15.31 COL 62.29
          FGCOLOR 7 
     "Contabilizaciòn de la Operaciòn" VIEW-AS TEXT
          SIZE 21.72 BY .5 AT ROW 14.62 COL 55.57
          BGCOLOR 1 FGCOLOR 7 
     "Tasa Nom.A. Mercado" VIEW-AS TEXT
          SIZE 16.29 BY .65 AT ROW 11.5 COL 8.14
          FGCOLOR 7 
     "Liquidac. Interès (Dias)" VIEW-AS TEXT
          SIZE 16.43 BY .5 AT ROW 13.35 COL 8.43
          FGCOLOR 7 
     "Descrip.Novedad" VIEW-AS TEXT
          SIZE 12.14 BY .54 AT ROW 19.42 COL 33.43
          FGCOLOR 7 
     "    Nombre Emisor                                                   Fec-Apertura" VIEW-AS TEXT
          SIZE 43.72 BY .5 AT ROW 2.81 COL 47.43
          FGCOLOR 7 
     "Agen.    Nit Emisor                Nro.del Título        Sec." VIEW-AS TEXT
          SIZE 41.86 BY .5 AT ROW 2.81 COL 5.72
          FGCOLOR 7 
     "Puntos Adicionales T.V" VIEW-AS TEXT
          SIZE 16.29 BY .54 AT ROW 9.85 COL 8.14
          FGCOLOR 7 
     RECT-3 AT ROW 1.19 COL 1.72
     RECT-306 AT ROW 5.31 COL 5.86
     RECT-307 AT ROW 1.96 COL 98.29
     RECT-308 AT ROW 10.88 COL 98.43
     RECT-311 AT ROW 5.31 COL 55.57
     RECT-312 AT ROW 15.12 COL 5.86
     RECT-313 AT ROW 19.88 COL 77.43
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 112.14 BY 20.73
         BGCOLOR 17 FGCOLOR 0 FONT 4.

DEFINE FRAME F_Imp
     Rs_Inf AT ROW 1.23 COL 4.14 NO-LABEL
     W_NitEnt AT ROW 5.85 COL 19.14 COLON-ALIGNED
     W_FecIni AT ROW 7.27 COL 11.86 COLON-ALIGNED
     W_FecFin AT ROW 8.35 COL 11.72 COLON-ALIGNED
     Btn_Imp-2 AT ROW 9.42 COL 4.29
     Btn_FinImp AT ROW 9.42 COL 25
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         PAGE-BOTTOM SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 74.86 ROW 9.69
         SIZE 37 BY 11.38
         BGCOLOR 17 FGCOLOR 0 FONT 4
         TITLE "Informes de Inversiones".


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
  CREATE WINDOW W-InverS ASSIGN
         HIDDEN             = YES
         TITLE              = "Inversiones - Apertura y Cancelaciòn, Programa W-Inversiones.W"
         HEIGHT             = 20.96
         WIDTH              = 112.43
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
/* SETTINGS FOR WINDOW W-InverS
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F_Canc
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME F_Canc:HIDDEN           = TRUE
       FRAME F_Canc:MOVABLE          = TRUE.

/* SETTINGS FOR FILL-IN DescCta IN FRAME F_Canc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN DescCta-2 IN FRAME F_Canc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN DescCta-3 IN FRAME F_Canc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN DescCta-4 IN FRAME F_Canc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN DescCta-5 IN FRAME F_Canc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN DescCta-6 IN FRAME F_Canc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN DescCta-7 IN FRAME F_Canc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN DescCta-8 IN FRAME F_Canc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_CtaPpal IN FRAME F_Canc
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN W_CtaPpal-2 IN FRAME F_Canc
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN W_CtaPpal-3 IN FRAME F_Canc
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN W_CtaPpal-4 IN FRAME F_Canc
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN W_CtaPpal-5 IN FRAME F_Canc
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN W_CtaPpal-6 IN FRAME F_Canc
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN W_CtaPpal-7 IN FRAME F_Canc
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN W_CtaPpal-8 IN FRAME F_Canc
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN W_Debe IN FRAME F_Canc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Debe-2 IN FRAME F_Canc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Debe-3 IN FRAME F_Canc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Debe-4 IN FRAME F_Canc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Debe-5 IN FRAME F_Canc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Debe-6 IN FRAME F_Canc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Debe-7 IN FRAME F_Canc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Debe-8 IN FRAME F_Canc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Haber IN FRAME F_Canc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Haber-2 IN FRAME F_Canc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Haber-3 IN FRAME F_Canc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Haber-4 IN FRAME F_Canc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Haber-5 IN FRAME F_Canc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Haber-6 IN FRAME F_Canc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Haber-7 IN FRAME F_Canc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Haber-8 IN FRAME F_Canc
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Cons
                                                                        */
/* BROWSE-TAB Br_Inv 1 F_Cons */
ASSIGN 
       FRAME F_Cons:MOVABLE          = TRUE.

/* SETTINGS FOR FRAME F_Imp
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Imp:HIDDEN           = TRUE
       FRAME F_Imp:MOVABLE          = TRUE.

/* SETTINGS FOR FRAME F_Invers
   Custom                                                               */
/* SETTINGS FOR FILL-IN Inversion_Sdos.Agencia IN FRAME F_Invers
   NO-ENABLE ALIGN-L EXP-LABEL EXP-FORMAT                               */
/* SETTINGS FOR BUTTON Btn_Apert IN FRAME F_Invers
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON Btn_Canc IN FRAME F_Invers
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Inversion_Sdos.Calif_Emisor IN FRAME F_Invers
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN Inversion_Sdos.Calif_Invers IN FRAME F_Invers
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR RADIO-SET Inversion_Sdos.Categoria IN FRAME F_Invers
   EXP-HELP                                                             */
/* SETTINGS FOR FILL-IN Inversion_Sdos.Cod_Producto IN FRAME F_Invers
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Inversion_Sdos.Cuenta_Contab IN FRAME F_Invers
   NO-ENABLE ALIGN-L EXP-LABEL                                          */
/* SETTINGS FOR FILL-IN Inversion_Sdos.Descrip IN FRAME F_Invers
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Inversion_Sdos.Fec_Apertura IN FRAME F_Invers
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Inversion_Sdos.Fec_Prorroga IN FRAME F_Invers
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Inversion_Sdos.Fec_Vcto IN FRAME F_Invers
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Inversion_Sdos.Indicador IN FRAME F_Invers
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN Inversion_Sdos.Interes_Causado IN FRAME F_Invers
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Inversion_Sdos.Interes_Recibido IN FRAME F_Invers
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Inversion_Sdos.LiqInt_Dias IN FRAME F_Invers
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Inversion_Sdos.Nombre_Emisor IN FRAME F_Invers
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN Inversion_Sdos.Nro_Unidades IN FRAME F_Invers
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Inversion_Sdos.Plazo_Dias IN FRAME F_Invers
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Inversion_Sdos.Puntos_Adic IN FRAME F_Invers
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN Inversion_Sdos.Sdo_Actual IN FRAME F_Invers
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN Inversion_Sdos.Secuencia IN FRAME F_Invers
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN Inversion_Sdos.Tasa_NomiAMcdo IN FRAME F_Invers
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN Inversion_Sdos.Tasa_NomiAnual IN FRAME F_Invers
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR TOGGLE-BOX Tg_GMF IN FRAME F_Invers
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET Inversion_Sdos.TipTasa_FV IN FRAME F_Invers
   NO-ENABLE EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN Inversion_Sdos.Valor_Unidad IN FRAME F_Invers
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN Inversion_Sdos.VrInvers_Inic IN FRAME F_Invers
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN W_CtaGMF IN FRAME F_Invers
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_CtaRend IN FRAME F_Invers
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Debe1 IN FRAME F_Invers
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Debe2 IN FRAME F_Invers
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Debe3 IN FRAME F_Invers
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Debe4 IN FRAME F_Invers
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_DesCrip IN FRAME F_Invers
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN W_Haber1 IN FRAME F_Invers
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Haber2 IN FRAME F_Invers
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Haber3 IN FRAME F_Invers
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Haber4 IN FRAME F_Invers
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_IntFuturo IN FRAME F_Invers
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomCta IN FRAME F_Invers
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN W_NomCta2 IN FRAME F_Invers
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN W_NomCta3 IN FRAME F_Invers
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN W_NomCta4 IN FRAME F_Invers
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN W_NomInd IN FRAME F_Invers
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN W_UnidXV IN FRAME F_Invers
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-InverS)
THEN W-InverS:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_Inv
/* Query rebuild information for BROWSE Br_Inv
     _START_FREEFORM
/*OPEN QUERY Br_Inv FOR EACH Inversion_Sdos NO-LOCK
           BY Inversion_Sdos.Agencia    BY Inversion_Sdos.Nit
           BY Inversion_Sdos.Nro_Titulo BY Inversion_Sdos.Secuen.*/
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE Br_Inv */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Canc
/* Query rebuild information for FRAME F_Canc
     _Query            is NOT OPENED
*/  /* FRAME F_Canc */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Cons
/* Query rebuild information for FRAME F_Cons
     _Query            is NOT OPENED
*/  /* FRAME F_Cons */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Imp
/* Query rebuild information for FRAME F_Imp
     _Query            is NOT OPENED
*/  /* FRAME F_Imp */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Invers
/* Query rebuild information for FRAME F_Invers
     _TblList          = "bdcentral.Inversion_Sdos"
     _Query            is NOT OPENED
*/  /* FRAME F_Invers */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-InverS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-InverS W-InverS
ON END-ERROR OF W-InverS /* Inversiones - Apertura y Cancelaciòn, Programa W-Inversiones.W */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-InverS W-InverS
ON WINDOW-CLOSE OF W-InverS /* Inversiones - Apertura y Cancelaciòn, Programa W-Inversiones.W */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  
  ON RETURN RETURN.
  
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br_Inv
&Scoped-define FRAME-NAME F_Cons
&Scoped-define SELF-NAME Br_Inv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Inv W-InverS
ON MOUSE-SELECT-DBLCLICK OF Br_Inv IN FRAME F_Cons
DO:
    ASSIGN Inversion_Sdo.Fec_Prorroga:SENSITIVE IN FRAME F_Invers = FALSE
           Inversion_Sdo.Fec_Prorroga:SCREEN-VALUE = ""
           W_IntPag:SENSITIVE                      = FALSE
           W_IntPag:SCREEN-VALUE                   = "0.00"
           W_CancParc:SENSITIVE                    = FALSE
           W_CancParc:SCREEN-VALUE                 = "0.00"
           W_CancParc                              = 0
           W_IntPag                                = 0
           W_UnidXV:SCREEN-VALUE                   = "0.00".

    IF AVAIL(Inversion_Sdos) AND Inversion_Sdos.Estado_Inver EQ 0 THEN DO:
       ASSIGN Inversion_Sdos.Agencia:SENSITIVE     = FALSE
              Inversion_Sdos.Nit_Emisor:SENSITIVE  = FALSE
              Inversion_Sdos.Nro_Titulo:SENSITIVE  = FALSE
              Inversion_Sdos.Secuencia:SENSITIVE   = FALSE
              Inversion_Sdo.Fec_Prorroga:SENSITIVE = FALSE
              Inversion_Sdos.TipTasa_FV:SENSITIVE  = FALSE  
              Inversion_Sdos.Indicador:SENSITIVE   = FALSE
              Inversion_Sdos.Puntos_Adic:SENSITIVE = FALSE.             

       DISPLAY 
           Inversion_Sdos.Agencia       Inversion_Sdos.Nit_Emisor 
           Inversion_Sdos.Nro_Titulo    Inversion_Sdos.Secuencia 
           Inversion_Sdos.Nombre_Emisor Inversion_Sdos.Fec_Apertura 
           Inversion_Sdos.Cuenta_Contab Inversion_Sdos.VrInvers_Inic 
           Inversion_Sdos.Descrip       Inversion_Sdos.Fec_Vcto 
           Inversion_Sdos.Tasa_NomiAnua Inversion_Sdos.Plazo_Dias 
           Inversion_Sdos.Interes_Causado
           Inversion_Sdos.Calif_Invers  Inversion_Sdos.Calif_Emisor 
           Inversion_Sdo.Fec_Prorroga   Inversion_Sdos.Interes_Recibido
           Inversion_Sdos.Cod_Producto  Inversion_Sdos.Categoria
           Inversion_Sdos.Sdo_Actual    Inversion_Sdos.LiqInt_Dias
           Inversion_Sdos.TipTasa_FV    Inversion_Sdos.Indicador 
           Inversion_Sdos.Puntos_Adic   Inversion_Sdos.Nro_Unidades
           Inversion_Sdos.Valor_Unidad  Inversion_Sdos.Tasa_NomiAMcdo 
                WITH FRAME F_Invers IN WINDOW W-InverS.
       
       FIND FIRST Pro_Inversiones WHERE Pro_Inversiones.Cod_Producto EQ Inversion_Sdos.Cod_Producto
                                    AND Pro_Inversiones.Categoria    EQ Inversion_Sdos.Categoria
                  NO-LOCK NO-ERROR.

       IF AVAIL(Pro_Inversiones) THEN DO:
          FIND Cuentas WHERE Cuentas.Cuenta EQ Pro_Inversiones.Cuenta_Inversion_DB
                         AND Cuentas.Tipo   EQ 2
                         AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
          IF AVAIL(Cuentas) THEN 
             ASSIGN W_NomCta2:SCREEN-VALUE = Cuentas.Nombre.
            
          FIND Cuentas WHERE Cuentas.Cuenta EQ Pro_Inversiones.CtaRendimiento_CR
                         AND Cuentas.Tipo   EQ 2
                         AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
          IF AVAIL(Cuentas) THEN 
             ASSIGN W_NomCta3:SCREEN-VALUE = Cuentas.Nombre
                    W_CtaRend:SCREEN-VALUE = Pro_Inversiones.CtaRendimiento_CR.

          APPLY "MOUSE-SELECT-CLICK" TO Inversion_Sdos.Categoria.

          FIND FIRST Pro_Inversiones WHERE Pro_Inversiones.Cod_Producto EQ Inversion_Sdos.Cod_Producto
                                       AND Pro_Inversiones.Categoria    EQ Inversion_Sdos.Categoria
                     NO-LOCK NO-ERROR.

          IF Pro_Inversiones.Id_TasaUnid EQ 1 THEN
             ASSIGN W_UnidXV:SCREEN-VALUE = STRING(Inversion_Sdos.Nro_Unid * Inversion_Sdos.Valor_Unid)
                    W_UnidXV:LABEL        = "Unidades por Valor Actual".
          ELSE
             ASSIGN W_UnidXV:SCREEN-VALUE = STRING(Inversion_Sdos.INT_Pactado)
                    W_UnidXV:LABEL        = "Interès Pactado hasta Hoy".


          IF Inversion_Sdos.TipTasa_FV EQ 1 THEN
             APPLY "MOUSE-SELECT-CLICK" TO Inversion_Sdos.TipTasa_FV.
       END.
       ELSE MESSAGE "El Producto debe existir en tabla Pro_Inversiones, Revise por favor."
                    VIEW-AS ALERT-BOX.

       ASSIGN W_IntFut = (((Inversion_Sdos.Sdo_Actual * Inversion_Sdos.Tasa_NomiAnual) /
                              365) * Inversion_Sdo.Plazo) / 100
              W_IntFuturo:SCREEN-VALUE             = STRING (W_IntFut - Inversion_Sdos.Interes_Causado)
              Inversion_Sdos.Puntos_Adic:SENSITIVE = FALSE
              FRAME F_Cons:VISIBLE                 = FALSE
              FRAME F_Invers:SENSITIVE             = TRUE.
    END.
    /*ELSE */

    APPLY "VALUE-CHANGED" TO W_OpeC.

    ASSIGN Inversion_Sdos.Puntos_Adic:SENSITIVE = FALSE.           
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Invers
&Scoped-define SELF-NAME Btn_Apert
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Apert W-InverS
ON CHOOSE OF Btn_Apert IN FRAME F_Invers /* Apertura */
DO:
/*  RELEASE Inversion_Sdos.
    
  ASSIGN Br_Inv:VISIBLE                                = FALSE
         W_Ope                                         = "A"
         W_OpeC:SCREEN-VALUE                           = "Apertura"
         Inversion_Sdos.Agencia:SCREEN-VALUE           = STRING(W_Agencia)
         Inversion_Sdos.Nit_Emisor:SCREEN-VALUE        = " "
         Inversion_Sdos.Nro_Titulo:SCREEN-VALUE        = " "
         Inversion_Sdos.Secuencia:SCREEN-VALUE         = "00"
         Inversion_Sdos.Nombre_Emisor:SCREEN-VALUE     = " "
         Inversion_Sdos.Fec_Apertura:SCREEN-VALUE      = STRING(TODAY)
         Inversion_Sdos.Cuenta_Contab:SCREEN-VALUE     = " "
         Inversion_Sdos.VrInvers_Inic:SCREEN-VALUE     = "0.00"
         Inversion_Sdos.Sdo_Actual:SCREEN-VALUE        = "0.00"
         Inversion_Sdos.Descrip:SCREEN-VALUE           = " "
         Inversion_Sdos.Fec_Vcto:SCREEN-VALUE          = " "
         Inversion_Sdos.Tasa_NomiAnual:SCREEN-VALUE    = "0.00"
         Inversion_Sdos.Plazo_Dias:SCREEN-VALUE        = "0"
         Inversion_Sdos.Interes_Causado:SCREEN-VALUE   = "0.00"
         Inversion_Sdos.Interes_Recibido:SCREEN-VALUE  = "0.00".
         
  ASSIGN Inversion_Sdos.Nit_Emisor:SENSITIVE = TRUE
         Inversion_Sdos.Nro_Titulo:SENSITIVE = TRUE
         Inversion_Sdos.Secuencia:SENSITIVE  = TRUE
         W_NitEmiC:VISIBLE                   = FALSE
         Tg_Todos:VISIBLE                    = FALSE
         W_NroTitC:VISIBLE                   = FALSE
         W_NomCta:SCREEN-VALUE               = ""
         W_Contrapart:SCREEN-VALUE           = ""
         W_Contrapart                        = ""
         W_NroCheq                           = ""
         W_NroCheq:SCREEN-VALUE              = ""
         Inversion_Sdo.Interes_Causado:SENSITIVE = FALSE
         W_Descrip:SCREEN-VALUE                  = ""
         W_Descrip                               = ""
         Inversion_Sdo.Fec_Prorroga:SENSITIVE    = FALSE
         Inversion_Sdo.Fec_Prorroga:SCREEN-VALUE = ""
         W_IntPag:SENSITIVE                      = FALSE
         W_IntPag:SCREEN-VALUE                   = "0.00"
         W_IntPag                                = 0
         W_CancParc:SENSITIVE                    = FALSE
         W_CancParc:SCREEN-VALUE                 = "0.00"
         W_CancParc                              = 0.
        
  APPLY "ENTRY" TO Inversion_Sdos.Nit_Emisor.
  */
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Consul
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Consul W-InverS
ON CHOOSE OF Btn_Consul IN FRAME F_Invers /* Consultar */
DO:
  ASSIGN FRAME F_Invers:SENSITIVE  = FALSE
         W_NomCta:SCREEN-VALUE     = ""
         W_Contrapart:SCREEN-VALUE = ""
         W_Contrapart
         W_NroCheq                 = ""
         W_NroCheq:SCREEN-VALUE    = ""
         W_SiCheq                  = FALSE
         Inversion_Sdo.Interes_Causado:SENSITIVE = FALSE
         W_Descrip:SCREEN-VALUE                  = ""
         W_Descrip                               = ""
         W_Ope                                   = " "
         W_OpeC:SCREEN-VALUE                     = " "
         FRAME F_Cons:VISIBLE                    = TRUE.
  
  CLOSE QUERY Br_Inv. 

  APPLY "Entry" TO W_NitEmiC.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Canc
&Scoped-define SELF-NAME Btn_ContCanc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_ContCanc W-InverS
ON CHOOSE OF Btn_ContCanc IN FRAME F_Canc /* Contabilizar */
DO:
  ASSIGN FRAME F_Invers:SENSITIVE = TRUE
         FRAME F_Canc:VISIBLE     = FALSE.
         
  RUN ProcSalva.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Imp
&Scoped-define SELF-NAME Btn_FinImp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_FinImp W-InverS
ON CHOOSE OF Btn_FinImp IN FRAME F_Imp /* Regresar */
DO:
   ASSIGN FRAME F_Invers:SENSITIVE = TRUE
          FRAME F_Imp:VISIBLE      = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Invers
&Scoped-define SELF-NAME Btn_Imp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imp W-InverS
ON CHOOSE OF Btn_Imp IN FRAME F_Invers /* Imprimir */
DO:
  ASSIGN FRAME F_Invers:SENSITIVE = FALSE
         FRAME F_Imp:VISIBLE      = TRUE
         W_FecIni:SCREEN-VALUE    = STRING(TODAY)
         W_FecIni
         W_FecFin:SCREEN-VALUE    = STRING(TODAY)
         W_FecFin
         Rs_Inf.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Imp
&Scoped-define SELF-NAME Btn_Imp-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imp-2 W-InverS
ON CHOOSE OF Btn_Imp-2 IN FRAME F_Imp /* Imprimir */
DO:
  DEFI VAR Listado AS CHAR FORM "X(40)".
  
  listado = "C:\InfInver.Lst".
  
  {Incluido\Imprimir.I "listado"}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Canc
&Scoped-define SELF-NAME Btn_Reg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Reg W-InverS
ON CHOOSE OF Btn_Reg IN FRAME F_Canc /* Regresar */
DO:
  ASSIGN FRAME F_Invers:SENSITIVE = TRUE
         FRAME F_Canc:VISIBLE     = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Invers
&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir W-InverS
ON CHOOSE OF Btn_Salir IN FRAME F_Invers /* Salir */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  
  ON RETURN RETURN.
  
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Cons
&Scoped-define SELF-NAME Btn_Salir-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir-2 W-InverS
ON CHOOSE OF Btn_Salir-2 IN FRAME F_Cons /* Regresar */
DO:
  ASSIGN FRAME F_Cons:VISIBLE     = FALSE
         FRAME F_Invers:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Invers
&Scoped-define SELF-NAME Btn_Salva
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salva W-InverS
ON CHOOSE OF Btn_Salva IN FRAME F_Invers /* Salvar */
DO: 
  DEFI VAR W_NomOpe AS CHAR FORM "X(30)" INIT "APERTURA de Inversión".

  ASSIGN W_RetGMF = 0
         W_RetFte = 0.
  
  FIND FIRST Inversion_Sdos WHERE Inversion_Sdos.Agencia    EQ INTEG(Inversion_Sdos.Agencia:SCREEN-VALUE)
                              AND Inversion_Sdos.Nit_Emi    EQ Inversion_Sdos.Nit_Emi:SCREEN-VALUE
                              AND Inversion_Sdos.Nro_Titulo EQ Inversion_Sdos.Nro_Titulo:SCREEN-VALUE
                              AND Inversion_Sdos.Secuencia  EQ INTEG(Inversion_Sdos.Secuencia:SCREEN-VALUE)
                                  NO-ERROR.

  IF AVAIL(Inversion_Sdos) AND Inversion_Sdos.Estado_Inver NE 0 THEN DO:
     MESSAGE "La Inversión ya fue cancelada, no se permite modificarla..." VIEW-AS ALERT-BOX.
     RETURN.
  END.

  FIND FIRST Pro_Inversiones WHERE Pro_Inversiones.Cod_Producto EQ INTEG(Inversion_Sdos.Cod_Producto:SCREEN-VALUE)
                               AND Pro_Inversiones.Categoria    EQ INTEG(Inversion_Sdos.Categoria:SCREEN-VALUE)
                                   NO-LOCK NO-ERROR.
  IF NOT AVAIL(Pro_Inversiones) THEN DO:
     MESSAGE "El Producto de La Inversión no-existe...Revise por favor"  VIEW-AS ALERT-BOX.
     RETURN.
  END.

  FIND FIRST Bancos WHERE Bancos.Nit     EQ Inversion_Sdos.Nit_Emi:SCREEN-VALUE NO-LOCK NO-ERROR.
  FIND FIRST Clientes WHERE Clientes.Nit EQ Inversion_Sdos.Nit_Emi:SCREEN-VALUE NO-LOCK NO-ERROR.
  
  IF NOT AVAIL(Clientes) 
  OR NOT AVAIL(Bancos)
  OR INTEG(Inversion_Sdos.Agencia:SCREEN-VALUE)     NE W_Agencia
  OR Inversion_Sdos.Nit_Emisor:SCREEN-VALUE         LE "0"
  OR Inversion_Sdos.Nro_Titulo:SCREEN-VALUE         LE "0"
  OR Inversion_Sdos.Nombre_Emisor:SCREEN-VALUE      LE " "
  OR Inversion_Sdos.Fec_Apertura:SCREEN-VALUE       EQ ?
  OR Inversion_Sdos.Fec_Apertura:SCREEN-VALUE       EQ "  /  /    "
  OR Inversion_Sdos.Fec_Apertura:SCREEN-VALUE       EQ " "
  OR Inversion_Sdos.Cuenta_Contab:SCREEN-VALUE      LE "0"
  OR DEC(Inversion_Sdos.VrInvers_Inic:SCREEN-VALUE) LE 0
  OR Inversion_Sdos.Descrip:SCREEN-VALUE            LE " "
  OR DATE(Inversion_Sdos.Fec_Vcto:SCREEN-VALUE)     LT DATE(Inversion_Sdos.Fec_Apertura:SCREEN-VALUE)
  OR DATE(Inversion_Sdos.Fec_Vcto:SCREEN-VALUE)     EQ ?
  OR Inversion_Sdos.Plazo_Dias:SCREEN-VALUE         LE "0"
  OR Inversion_Sdos.LiqInt_Dias:SCREEN-VALUE        LE "0"
  OR W_Descrip                                      LE " "    THEN DO:
     MESSAGE "El Nit del Emisor debe existir en Clientes y en Tabla Bancos," SKIP
             "O Falta la Descripción de la Tx, "           SKIP
             "O Faltan Campos indispensables para Salvar."  VIEW-AS ALERT-BOX.
     RETURN.
  END.

  IF  INTEG(Inversion_Sdos.TipTasa_FV:SCREEN-VALUE) EQ 1
  AND INTEG(Inversion_Sdos.Indicador:SCREEN-VALUE)  LE 0 THEN DO:
      MESSAGE "Por Favor Ingrese Cod-Indicador para Tasa Variable..." VIEW-AS ALERT-BOX.
  
      RETURN.
  END.
  
  RUN ValidarCtas NO-ERROR.
  IF ERROR-STATUS:ERROR THEN 
     RETURN.
     
  ASSIGN W_ContraPart
         W_SiCheq     = FALSE.
         
  IF  W_Ope NE "A" AND W_Ope NE "C" AND W_Ope NE "I" AND W_Ope NE "M" 
  AND W_Ope NE "P" AND W_Ope NE "R" AND W_Ope NE "S" AND W_Ope NE "V" THEN DO:
      MESSAGE "Ingrese una Operación correcta por favor..." VIEW-AS ALERT-BOX.
  
      RETURN.
  END.

  IF W_Ope EQ "I" AND Inversion_Sdos.Categoria:SCREEN-VALUE NE "3" THEN DO:
     IF TODAY NE Inversion_Sdos.Fec_Apertura THEN DO:
        MESSAGE "La operaciòn de Incremento solo es Vàlida para Inversiones-Disponibles," SKIP
                "O solo Si la efectua en la misma fecha de Apertura."
                "                           No se permite la operaciòn." VIEW-AS ALERT-BOX.
        RETURN.
     END.
  END.

  IF W_Ope EQ "R" AND Pro_Inversiones.Id_TasaUnid EQ 1 THEN DO:
     MESSAGE "Pdcto por Unidades...la operaciòn Rendimientos Recibidos no es vàlida...?" SKIP 
             "Ingrese una correcta por favor..." VIEW-AS ALERT-BOX.
     RETURN.
  END.

  IF  Pro_Inversiones.Id_TasaUnid EQ 0
  AND Inversion_Sdos.Tasa_NomiAnual:SCREEN-VALUE LE "0.00" THEN DO:
      MESSAGE "Falta la Tasa_NomiAnual...Ingrese una correcta por favor..." VIEW-AS ALERT-BOX.
      RETURN.
  END.

  IF W_Ope EQ "A" OR W_Ope EQ "C" OR W_Ope EQ "I" OR W_Ope EQ "R" OR W_Ope EQ "V" THEN DO:
     FIND Cuentas WHERE Cuentas.Cuenta EQ W_ContraPart
                    AND Cuentas.Tipo   EQ 2
                    AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
     IF NOT AVAIL(Cuentas) THEN DO:
        MESSAGE "La cuenta contable Contrapartida para Apertura, Cancelación O Interés es obligatoria," SKIP
                "de movimiento y Activa en el PUC...Ingrese una correcta por favor." VIEW-AS ALERT-BOX.
        RETURN.
     END.
     
     IF (W_Ope = "A" OR W_Ope EQ "I")
     AND Cuentas.Cod_FlujoEfec EQ "D"
     AND Cuentas.Car_Efectivo  EQ 3 THEN DO:
         FIND FIRST Formatos WHERE Formatos.Agencia     EQ W_Agencia
                               AND Formatos.Cod_Formato EQ Cuentas.Cod_Formato NO-LOCK NO-ERROR.
         IF  AVAILABLE(Formatos)
         AND W_NroCheq GT " " THEN DO:
             MESSAGE "El Número del cheque a Girar es: " W_NroCheq SKIP
                     "Está Segura(o) de ejecutar la Apertura-Incremento con Imp-Cheque ?...Teclee YES" SKIP
                     "                           La Apertura-Incremento sin Cheque     ?...Teclee NO"  SKIP
                     "                           Para Cancelar              ?...Tecle CANCEL" 
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL TITLE "Confirmar No.Cheque"
                     UPDATE W_RptaCh AS LOGICAL.
             IF W_RptaCh THEN
                ASSIGN W_CheProfor  = Formatos.Nom_Proceso
                       W_SiCheq     = TRUE.
             ELSE IF NOT W_RptaCh THEN.
             ELSE RETURN.
         END.
         ELSE DO: 
             MESSAGE "La Cuenta es de Banco si desea Imprimir cheque a Girar," SKIP
                     "          Esta debe tener formato para cheque y el Nro.Cheque válido."  SKIP
                     "Está Segura(o) de ejecutar la Apertura-Incremento con Imp-Cheque ?...Teclee YES" SKIP
                     "                           La Apertura-Incremento sin Cheque     ?...Teclee NO"  SKIP
                                     VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirmar Imp.Cheque"
                     UPDATE W_RptaCh1 AS LOGICAL.
             IF W_RptaCh1 THEN DO:
                APPLY "ENTRY" TO W_NroCheq.
                RETURN.
             END.
         END.
     END.   
  END.
   
  IF  NOT AVAIL(Inversion_Sdos) 
  AND W_Ope NE "A" 
  AND W_Ope NE "S" THEN DO:
      MESSAGE "La operación debe ser A-pertura o S-aldo Inicial....Ingrese una correcta por favor." VIEW-AS ALERT-BOX.
  
     RETURN.
  END.           
  
  IF  AVAIL(Inversion_Sdos) 
  AND W_Ope NE "C"
  AND W_Ope NE "I" 
  AND W_Ope NE "M"
  AND W_Ope NE "P"
  AND W_Ope NE "R"
  AND W_Ope NE "V" THEN DO:
      MESSAGE "La operación debe ser C-ancelación o M-Modificación o P-Prorroga o R-Intereses...." SKIP
              "             Ingrese una correcta por favor." VIEW-AS ALERT-BOX. 
      RETURN.
  END.
  
  IF  AVAIL(Inversion_Sdos) 
  AND W_Ope EQ "R" 
  AND W_IntPag LE 0 THEN DO:
      MESSAGE "La operación R-Rendimientos exige Intereses Recibidos..." SKIP
              "             Revise por favor." VIEW-AS ALERT-BOX. 
      RETURN.
  END.
  
  IF  AVAIL(Inversion_Sdos) 
  AND W_Ope EQ "V" AND W_CancParc LE 0 THEN DO:
      MESSAGE "La operación Vlr.Cancelaciòn-Parcial exige Vlr-Cancelaciòn Parcial..." SKIP
              "             Revise por favor." VIEW-AS ALERT-BOX. 
      RETURN.
  END.

  IF  AVAIL(Inversion_Sdos) 
  AND W_Ope EQ "I" AND W_CancParc LE 0 THEN DO:
      MESSAGE "La operación Incremento de la Inversiòn exige Valor Incremento ..." SKIP
              "             Revise por favor." VIEW-AS ALERT-BOX. 
      RETURN.
  END.

  IF Pro_Inversiones.Id_TasaUnid EQ 0 THEN 
     RUN HallaTasaMcdo.
  ELSE IF W_Ope NE "C" THEN 
     RUN HallaNroUnid.

  IF  Pro_Inversiones.Id_TasaUnid EQ 1 
  AND Inversion_Sdos.Nro_Unidades:SCREEN-VALUE LE "0.00" THEN DO:
      MESSAGE "Por Favor Ingrese Nro_Unidades para el producto..." VIEW-AS ALERT-BOX.
  
      RETURN.      
  END.

  IF      W_Ope EQ "C" THEN
     W_NomOpe = "CANCELAR la Inversión".
  ELSE IF W_Ope EQ "I" THEN 
     W_NomOpe = "Incremento de Inversiòn".
  ELSE IF W_Ope EQ "R" THEN 
     W_NomOpe = "Intereses Pagados".
  ELSE IF W_Ope EQ "M" THEN
     W_NomOpe = "MODIFICAR Condiciones".
  ELSE IF W_Ope EQ "S" THEN
     W_NomOpe = "INGRESAR Saldo". 
  ELSE IF W_Ope EQ "V" THEN
     W_NomOpe = "Cancelaciòn Parcial".
  ELSE IF W_Ope EQ "P" THEN DO:
     W_NomOpe = "Prorroga".
     
     MESSAGE "La Fecha de Prorroga es : " Inversion_Sdo.Fec_Prorroga:SCREEN-VALUE 
          VIEW-AS ALERT-BOX.
  END.

  IF W_Ope NE "C" THEN DO:
     RUN CtasContab.
     
     MESSAGE "Está Segura(o) de ejecutar la operación de : " W_NomOpe " ?..." 
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirmar Operación"
                  UPDATE W_RptaInv AS LOGICAL.
     IF NOT W_RptaInv THEN RETURN. 
  END.

  IF  (W_Ope = "A" OR W_Ope EQ "I")
  AND NOT W_SiCheq
  AND Cuentas.Cod_FlujoEfec EQ "D"
  AND Cuentas.Car_Efectivo  EQ 3 THEN DO:
      MESSAGE "Está Segura(o) de ejecutar la operación de Apertura-Incremento SIN Imp-Cheque ?..." 
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirmar Sin Cheque"
                  UPDATE W_RptaCh2 AS LOGICAL.
      IF NOT W_RptaCh2 THEN RETURN. 
  END.
  
  IF W_Ope EQ "P" THEN
     ASSIGN W_Plazo = Inversion_Sdo.Plazo_Dias   
            W_Tasa  = Inversion_Sdo.Tasa_NomiAnual.
  
  IF W_Ope EQ "C" THEN DO:
     ASSIGN FRAME F_Invers:SENSITIVE = FALSE
            FRAME F_Canc:VISIBLE     = TRUE
            W_Canc                   = FALSE.
          
     RUN CtasCanc.
  END.
  ELSE RUN ProcSalva.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 W-InverS
ON CHOOSE OF BUTTON-5 IN FRAME F_Invers /* Button 5 */
DO:
  RUN W-InfDia.R.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Inversion_Sdos.Calif_Emisor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Inversion_Sdos.Calif_Emisor W-InverS
ON LEAVE OF Inversion_Sdos.Calif_Emisor IN FRAME F_Invers /* Calificación del Emisor */
DO:
  Inversion_Sdos.Calif_Emisor:SCREEN-VALUE = CAPS(Inversion_Sdos.Calif_Emisor:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Inversion_Sdos.Calif_Invers
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Inversion_Sdos.Calif_Invers W-InverS
ON LEAVE OF Inversion_Sdos.Calif_Invers IN FRAME F_Invers /* Calific.de la Inversión */
DO:
  Inversion_Sdos.Calif_Invers:SCREEN-VALUE = CAPS(Inversion_Sdos.Calif_Invers:SCREEN-VALUE).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Inversion_Sdos.Categoria
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Inversion_Sdos.Categoria W-InverS
ON MOUSE-SELECT-CLICK OF Inversion_Sdos.Categoria IN FRAME F_Invers /* Categoria */
DO:
  Cmb_Pdctos:LIST-ITEMS = "".

  FOR EACH Pro_Inversiones 
           WHERE Pro_Inversiones.Categoria EQ INTEG(Inversion_Sdos.Categoria:SCREEN-VALUE)
                 NO-LOCK:
      Cmb_Pdctos:ADD-LAST(STRING(Pro_Inversiones.Cod_Produc,"999") + 
                        "-" + STRING(Pro_Inversiones.Nom_Prod,"X(35)")).
      IF Pro_Inversiones.Cod_Produc EQ INTEG(Inversion_Sdos.Cod_Produc:SCREEN-VALUE) THEN DO:
         ASSIGN Cmb_Pdctos:SCREEN-VALUE = STRING(Pro_Inversiones.Cod_Produc,"999") + 
                                                 "-" + STRING(Pro_Inversiones.Nom_Prod,"X(35)").
         IF Pro_Inversiones.Id_TasaUnid EQ 0 THEN
            ASSIGN Inversion_Sdos.Tasa_NomiAnual:VISIBLE = TRUE.
         ELSE 
            ASSIGN Inversion_Sdos.Tasa_NomiAnual:VISIBLE = FALSE.
      END.
  END.              
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_pdctos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_pdctos W-InverS
ON LEAVE OF Cmb_pdctos IN FRAME F_Invers
DO:
  APPLY "ENTRY" TO Inversion_Sdos.VrInvers_Inic.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_pdctos W-InverS
ON VALUE-CHANGED OF Cmb_pdctos IN FRAME F_Invers
DO:
  ASSIGN Inversion_Sdos.Cod_Producto:SCREEN-VALUE = SUBSTRING(Cmb_Pdctos:SCREEN-VALUE,1,3).

  IF W_Ope EQ "A" OR W_Ope = "S" THEN
     ASSIGN Inversion_Sdos.Descrip:SCREEN-VALUE = SUBSTRING(Cmb_Pdctos:SCREEN-VALUE,5,35).

  FIND FIRST Pro_Inversiones WHERE Pro_Inversiones.Cod_Producto EQ INTEG(Inversion_Sdos.Cod_Producto:SCREEN-VALUE)
                                   NO-LOCK NO-ERROR.
  IF AVAIL(Pro_Inversiones) THEN DO:
     ASSIGN Inversion_Sdos.Cuenta_Contab:SCREEN-VALUE = Pro_Inversiones.Cuenta_Inversion_DB
            Inversion_Sdos.Categoria:SCREEN-VALUE     = STRING(Pro_Inversiones.Categoria)
            Inversion_Sdos.Indicador:SCREEN-VALUE     = STRING(Pro_Inversiones.Indice).

     FIND FIRST Indicadores WHERE Indicadores.Indicador EQ INTEG(Inversion_Sdos.Indicador:SCREEN-VALUE)
                                 AND Indicadores.Estado EQ 1 NO-LOCK NO-ERROR.
     IF AVAIL(Indicadores) THEN
        ASSIGN W_NomInd:SCREEN-VALUE = Indicadores.Nombre.
               
     IF Pro_Inversiones.Id_TasaUnid EQ 0 THEN
        ASSIGN Inversion_Sdos.Tasa_NomiAnual:VISIBLE = TRUE
               Inversion_Sdos.Nro_Unidades:VISIBLE   = FALSE
               Inversion_Sdos.Valor_Unidad:VISIBLE   = FALSE
               Inversion_Sdos.TipTasa_FV:SENSITIVE   = TRUE.
     ELSE DO:
        ASSIGN Inversion_Sdos.Nro_Unidades:VISIBLE    = TRUE
               Inversion_Sdos.Valor_Unidad:VISIBLE    = TRUE
               Inversion_Sdos.Nro_Unidades:SENSITIVE  = TRUE
               Inversion_Sdos.Valor_Unidad:SENSITIVE  = TRUE
               Inversion_Sdos.Tasa_NomiAnual:VISIBLE  = FALSE
               Inversion_Sdos.TipTasa_FV:SENSITIVE    = FALSE
               Inversion_Sdos.TipTasa_FV:SCREEN-VALUE = "0".
        
        IF AVAIL(Indicadores) THEN
           ASSIGN Inversion_Sdos.Valor_Unidad:SCREEN-VALUE = STRING(Indicadores.Valor).
     END.

     FIND Cuentas WHERE Cuentas.Cuenta     EQ Pro_Inversiones.Cuenta_Inversion_DB   
                        AND Cuentas.Tipo   EQ 2                                      
                        AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.                    
     IF AVAIL(Cuentas) THEN                                                          
        ASSIGN W_NomCta2:SCREEN-VALUE = Cuentas.Nombre.                              
                                                                                     
     FIND Cuentas WHERE Cuentas.Cuenta EQ Pro_Inversiones.CtaRendimiento_CR          
                    AND Cuentas.Tipo   EQ 2                                          
                    AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.                        
     IF AVAIL(Cuentas) THEN                                                          
        ASSIGN W_NomCta3:SCREEN-VALUE = Cuentas.Nombre                               
               W_CtaRend:SCREEN-VALUE = Pro_Inversiones.CtaRendimiento_CR.                             
                                                                                     
     APPLY "MOUSE-SELECT-CLICK" TO Inversion_Sdos.TipTasa_FV.                        
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Inversion_Sdos.Cuenta_Contab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Inversion_Sdos.Cuenta_Contab W-InverS
ON LEAVE OF Inversion_Sdos.Cuenta_Contab IN FRAME F_Invers /* Cta. de  Inversiòn */
DO:
  FIND Cuentas WHERE Cuentas.Cuenta EQ Inversion_Sdos.Cuenta_Contab:SCREEN-VALUE
                 AND Cuentas.Tipo   EQ 2
                 AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
  IF NOT AVAIL(Cuentas) THEN 
     MESSAGE "La cuenta contable de la inversión es obligatoria," SKIP
             "de movimiento y Activa en el PUC...Ingrese una correcta por favor." VIEW-AS ALERT-BOX.              
                  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Inversion_Sdos.Fec_Prorroga
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Inversion_Sdos.Fec_Prorroga W-InverS
ON LEAVE OF Inversion_Sdos.Fec_Prorroga IN FRAME F_Invers /* Fecha de Prorroga */
DO:
  ASSIGN Inversion_Sdos.Fec_Vcto:SCREEN-VALUE = STRING(DATE(Inversion_Sdos.Fec_Prorroga:SCREEN-VALUE) + 
                                                INTEG(Inversion_Sdos.Plazo:SCREEN-VALUE)).

  APPLY "ENTRY" TO Inversion_Sdo.LiqInt_Dias.

  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Inversion_Sdos.Fec_Vcto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Inversion_Sdos.Fec_Vcto W-InverS
ON LEAVE OF Inversion_Sdos.Fec_Vcto IN FRAME F_Invers /* Fecha Vencimiento */
DO:
  IF DATE(Inversion_Sdos.Fec_Apertura:SCREEN-VALUE) GT DATE(Inversion_Sdos.Fec_Vcto:SCREEN-VALUE) THEN DO:
     ASSIGN Inversion_Sdos.Fec_Vcto:SCREEN-VALUE = Inversion_Sdos.Fec_Apertura:SCREEN-VALUE.  
     
     MESSAGE "Rectifique la Fec-Vencimiento..." view-as alert-box.
     
     APPLY "ENTRY" TO Inversion_Sdos.Fec_Vcto.
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Inversion_Sdos.Indicador
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Inversion_Sdos.Indicador W-InverS
ON LEAVE OF Inversion_Sdos.Indicador IN FRAME F_Invers /* Código Indicador */
DO:
  DEFI VAR W_Tasa LIKE Indicadores.Tasa.

  ASSIGN W_NomInd:SCREEN-VALUE                      = ""
         Inversion_Sdos.Tasa_NomiAnual:SCREEN-VALUE = "0.00".

  FIND FIRST Indicadores WHERE Indicadores.Indicador EQ INTEG(Inversion_Sdos.Indicador:SCREEN-VALUE)
                           AND Indicadores.Estado    EQ 1 NO-LOCK NO-ERROR.
  IF AVAIL(Indicadores) THEN DO:
     ASSIGN W_NomInd:SCREEN-VALUE = Indicadores.Nombre.
            W_Tasa                = (Indicadores.Tasa + DEC(Inversion_Sdos.Puntos_Adic:SCREEN-VALUE)).

     RUN TasaPerNom IN W_ManFin (INPUT 365,W_Tasa,1,OUTPUT W_Tasa).

     Inversion_Sdos.Tasa_NomiAnual:SCREEN-VALUE = STRING(W_Tasa * 36500).
  END.
  ELSE IF Inversion_sdos.TipTasa_FV:SCREEN-VALUE EQ "1" THEN DO:
     MESSAGE "Para Tasa variable es Indispensable el Cod-Indicador y que exista en la configuraciòn."
             VIEW-AS ALERT-BOX.

     Inversion_Sdos.Indicador:SCREEN-VALUE = "0".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Inversion_Sdos.Nit_Emisor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Inversion_Sdos.Nit_Emisor W-InverS
ON LEAVE OF Inversion_Sdos.Nit_Emisor IN FRAME F_Invers /* Nit_Emisor */
DO:
  FIND FIRST Clientes WHERE Clientes.Nit EQ Inversion_Sdos.Nit_Emi:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF NOT AVAIL(Clientes) THEN DO:
     ASSIGN FRAME F_Invers:SENSITIVE = FALSE.

     RUN C-Clientes.R (INPUT  1,W_Agencia,
                       OUTPUT W_NitEmi, OUTPUT W_NomEmi, OUTPUT W_NomEmi, OUTPUT W_Age).

     ASSIGN FRAME F_Invers:SENSITIVE = TRUE.

     FIND FIRST Clientes WHERE Clientes.Nit EQ W_NitEmi NO-LOCK NO-ERROR.
     IF AVAIL(Clientes) THEN
        ASSIGN Inversion_Sdos.Nit_Emi:SCREEN-VALUE = W_NitEmi.
     ELSE
        MESSAGE "El Nit del Emisor debe existir en Clientes" VIEW-AS ALERT-BOX.         
  END.                                                                         

  FIND FIRST Bancos WHERE Bancos.Nit EQ Inversion_Sdos.Nit_Emi:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF AVAIL(Bancos) THEN 
     ASSIGN Inversion_Sdos.Nombre_Emi:SCREEN-VALUE = Bancos.Nombre
            Inversion_Sdos.Calif_Emi:SCREEN-VALUE  = Bancos.Calif_Emi.     
  ELSE
     MESSAGE "El Nit del Emisor debe existir en Clientes y en Tabla Bancos." VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Inversion_Sdos.Nro_Titulo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Inversion_Sdos.Nro_Titulo W-InverS
ON LEAVE OF Inversion_Sdos.Nro_Titulo IN FRAME F_Invers /* Nro_Titulo */
DO:
  FIND LAST Inversion_Sdos WHERE Inversion_Sdos.Nit_Emisor        EQ Inversion_Sdos.Nit_Emisor:SCREEN-VALUE
                             AND Inversion_Sdos.Nro_Titulo EQ Inversion_Sdos.Nro_Titulo:SCREEN-VALUE
                             AND Inversion_Sdos.Secuencia  GE 0 NO-LOCK NO-ERROR.
  IF AVAIL(Inversion_Sdos) THEN
     Inversion_Sdos.Secuencia:SCREEN-VALUE = STRING(Inversion_Sdos.Secuencia + 1).  

  APPLY "LEAVE" TO Inversion_Sdos.Secuencia.

  APPLY "ENTRY" TO Cmb_pdctos.

  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Inversion_Sdos.Plazo_Dias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Inversion_Sdos.Plazo_Dias W-InverS
ON LEAVE OF Inversion_Sdos.Plazo_Dias IN FRAME F_Invers /* Plazo */
DO:
  IF W_Ope = "A" THEN
     ASSIGN Inversion_Sdos.Fec_Vcto:SCREEN-VALUE = STRING(TODAY + 
                                                   INTEG(Inversion_Sdos.Plazo:SCREEN-VALUE)).
  ELSE IF W_Ope = "S" THEN
     ASSIGN Inversion_Sdos.Fec_Vcto:SCREEN-VALUE = STRING(DATE(Inversion_Sdos.Fec_Apert:SCREEN-VALUE) + 
                                                   INTEG(Inversion_Sdos.Plazo:SCREEN-VALUE)).
  ELSE IF W_Ope = "P"  THEN
     ASSIGN Inversion_Sdos.Fec_Vcto:SCREEN-VALUE = STRING(DATE(Inversion_Sdos.Fec_Prorroga:SCREEN-VALUE) + 
                                                   INTEG(Inversion_Sdos.Plazo:SCREEN-VALUE)).

  IF Inversion_Sdos.Plazo:SCREEN-VALUE LT Inversion_Sdos.LiqInt_Dias:SCREEN-VALUE THEN
     Inversion_Sdos.LiqInt_Dias:SCREEN-VALUE = Inversion_Sdos.Plazo:SCREEN-VALUE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Inversion_Sdos.Puntos_Adic
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Inversion_Sdos.Puntos_Adic W-InverS
ON LEAVE OF Inversion_Sdos.Puntos_Adic IN FRAME F_Invers /* Puntos_Adic T.Vble */
DO:
  APPLY "LEAVE" TO Inversion_Sdos.Indicador.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Imp
&Scoped-define SELF-NAME Rs_Inf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Rs_Inf W-InverS
ON VALUE-CHANGED OF Rs_Inf IN FRAME F_Imp
DO:
  ASSIGN Rs_Inf.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Invers
&Scoped-define SELF-NAME Tg_GMF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Tg_GMF W-InverS
ON VALUE-CHANGED OF Tg_GMF IN FRAME F_Invers /* Impto G.M.F */
DO:
  ASSIGN Tg_GMF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Cons
&Scoped-define SELF-NAME Tg_Todos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Tg_Todos W-InverS
ON VALUE-CHANGED OF Tg_Todos IN FRAME F_Cons /* Todas las Inversiones */
DO:
  CLOSE QUERY Br_Inv.
  
  OPEN QUERY Br_Inv FOR EACH Inversion_Sdos NO-LOCK
                             BY Inversion_Sdos.Agencia    BY Inversion_Sdos.Nit_Emisor
                             BY Inversion_Sdos.Nro_Titulo BY Inversion_Sdos.Secuen.

  ASSIGN Tg_Todos:SCREEN-VALUE = "NO".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Invers
&Scoped-define SELF-NAME Inversion_Sdos.TipTasa_FV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Inversion_Sdos.TipTasa_FV W-InverS
ON MOUSE-SELECT-CLICK OF Inversion_Sdos.TipTasa_FV IN FRAME F_Invers /* TipoTasa_FV */
DO:
  FIND FIRST Pro_Inversiones WHERE Pro_Inversiones.Cod_Producto EQ INTEG(Inversion_Sdos.Cod_Producto:SCREEN-VALUE)
                               AND Pro_Inversiones.Categoria    EQ INTEG(Inversion_Sdos.Categoria:SCREEN-VALUE)
                 NO-LOCK NO-ERROR.
    
  ASSIGN Inversion_Sdos.Indicador:SENSITIVE   = FALSE
         Inversion_Sdos.Puntos_Adic:SENSITIVE = FALSE
         Inversion_Sdos.Tasa_NomiAn:SENSITIVE = FALSE.

  IF INTEG(Inversion_Sdos.TipTasa:SCREEN-VALUE) EQ 0 THEN DO:
     ASSIGN Inversion_Sdos.Tasa_NomiAn:SENSITIVE    = TRUE
            Inversion_Sdos.Puntos_Adic:SCREEN-VALUE = "0.00".

     APPLY "LEAVE" TO Inversion_Sdos.Indicador.
  END.
  ELSE DO:
     ASSIGN Inversion_Sdos.Indicador:SCREEN-VALUE = STRING(Pro_Inversiones.Indice)
            Inversion_Sdos.Puntos_Adic:SENSITIVE  = TRUE
            Inversion_Sdos.Tasa_NomiAn:SENSITIVE  = FALSE.
     APPLY "LEAVE" TO Inversion_Sdos.Indicador.

     APPLY "ENTRY" TO Inversion_Sdos.Puntos_Adic.

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Inversion_Sdos.Valor_Unidad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Inversion_Sdos.Valor_Unidad W-InverS
ON LEAVE OF Inversion_Sdos.Valor_Unidad IN FRAME F_Invers /* Valor cada Unidad */
DO:
  RUN HallaNroUnid.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Inversion_Sdos.VrInvers_Inic
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Inversion_Sdos.VrInvers_Inic W-InverS
ON LEAVE OF Inversion_Sdos.VrInvers_Inic IN FRAME F_Invers /* Valor Inicial Inversión */
DO:
   DEFI VAR W_RowidI AS ROWID.
  
   ASSIGN W_RowidI = ROWID(Inversion_Sdos).
          
   RUN ParticYTope.

   FIND Inversion_Sdos WHERE ROWID(Inversion_Sdos) EQ W_RowidI NO-LOCK NO-ERROR.

   IF Pro_Inversiones.Id_TasaUnid EQ 0 THEN 
      RUN HallaTasaMcdo.
   ELSE RUN HallaNroUnid.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CancParc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CancParc W-InverS
ON LEAVE OF W_CancParc IN FRAME F_Invers /* Cancelaciòn Parcial */
DO:
   DEFI VAR W_RowidI AS ROWID.

   ASSIGN W_CancParc.

   IF W_Ope EQ "I" THEN DO:
      ASSIGN W_RowidI = ROWID(Inversion_Sdos).
          
      RUN ParticYTope.

      FIND Inversion_Sdos WHERE ROWID(Inversion_Sdos) EQ W_RowidI NO-LOCK NO-ERROR.
   END.

   IF W_Ope EQ "V" AND W_CancParc GT Inversion_Sdos.Sdo_Actual THEN
      ASSIGN W_CancParc = Inversion_Sdos.Sdo_Actual
             W_CancParc:SCREEN-VALUE = STRING(Inversion_Sdos.Sdo_Actual).

   IF Pro_Inversiones.Id_TasaUnid EQ 0 THEN 
      RUN HallaTasaMcdo.
   ELSE RUN HallaNroUnid.

   APPLY "ENTRY" TO W_ContraPart.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_ContraPart
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_ContraPart W-InverS
ON LEAVE OF W_ContraPart IN FRAME F_Invers /* Cta-Contrapartida */
DO:
  ASSIGN W_ContraPart
         W_SiCheq     = FALSE.

  IF W_Ope EQ "S" OR W_Ope EQ "M" OR W_Ope EQ " " THEN RETURN.
  
  FIND Cuentas WHERE Cuentas.Cuenta EQ W_ContraPart
                 AND Cuentas.Tipo   EQ 2
                 AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
  IF AVAIL(Cuentas) THEN 
     ASSIGN W_NomCta:SCREEN-VALUE = Cuentas.Nombre
            W_NomCta.
  ELSE DO:
     ASSIGN FRAME F_Invers:SENSITIVE = FALSE.

     RUN C-Cuentas.R (OUTPUT W_ContraPart,OUTPUT W_NomCta, OUTPUT W_Nat, OUTPUT W_Ctr, 
                      INPUT  2).

     ASSIGN FRAME F_Invers:SENSITIVE = TRUE.

     FIND Cuentas WHERE Cuentas.Cuenta EQ W_ContraPart
                    AND Cuentas.Tipo   EQ 2
                    AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
     IF AVAIL(Cuentas) THEN 
        ASSIGN W_NomCta:SCREEN-VALUE     = Cuentas.Nombre
               W_ContraPart:SCREEN-VALUE = W_ContraPart
               W_NomCta.
     ELSE 
        MESSAGE "La cuenta contable Contrapartida para Apertura, Interés o Cancelación es obligatoria," SKIP
                "de movimiento y Activa en el PUC...Ingrese una correcta por favor." VIEW-AS ALERT-BOX.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Canc
&Scoped-define SELF-NAME W_Debe-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Debe-3 W-InverS
ON LEAVE OF W_Debe-3 IN FRAME F_Canc
DO:
  W_RetFte = DEC(W_Debe-3:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Invers
&Scoped-define SELF-NAME W_Debe1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Debe1 W-InverS
ON LEAVE OF W_Debe1 IN FRAME F_Invers
DO:
   DEFI VAR W_RowidI AS ROWID.

   ASSIGN W_CancParc.

   IF W_Ope EQ "I" THEN DO:
      ASSIGN W_RowidI = ROWID(Inversion_Sdos).
          
      RUN ParticYTope.

      FIND Inversion_Sdos WHERE ROWID(Inversion_Sdos) EQ W_RowidI NO-LOCK NO-ERROR.
   END.

   IF W_Ope EQ "C" AND W_CancParc GT Inversion_Sdos.Sdo_Actual THEN
      ASSIGN W_CancParc = Inversion_Sdos.Sdo_Actual
             W_CancParc:SCREEN-VALUE = STRING(Inversion_Sdos.Sdo_Actual).

   APPLY "ENTRY" TO W_ContraPart.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Debe2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Debe2 W-InverS
ON LEAVE OF W_Debe2 IN FRAME F_Invers
DO:
   DEFI VAR W_RowidI AS ROWID.

   ASSIGN W_CancParc.

   IF W_Ope EQ "I" THEN DO:
      ASSIGN W_RowidI = ROWID(Inversion_Sdos).
          
      RUN ParticYTope.

      FIND Inversion_Sdos WHERE ROWID(Inversion_Sdos) EQ W_RowidI NO-LOCK NO-ERROR.
   END.

   IF W_Ope EQ "C" AND W_CancParc GT Inversion_Sdos.Sdo_Actual THEN
      ASSIGN W_CancParc = Inversion_Sdos.Sdo_Actual
             W_CancParc:SCREEN-VALUE = STRING(Inversion_Sdos.Sdo_Actual).

   APPLY "ENTRY" TO W_ContraPart.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Debe3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Debe3 W-InverS
ON LEAVE OF W_Debe3 IN FRAME F_Invers
DO:
   DEFI VAR W_RowidI AS ROWID.

   ASSIGN W_CancParc.

   IF W_Ope EQ "I" THEN DO:
      ASSIGN W_RowidI = ROWID(Inversion_Sdos).
          
      RUN ParticYTope.

      FIND Inversion_Sdos WHERE ROWID(Inversion_Sdos) EQ W_RowidI NO-LOCK NO-ERROR.
   END.

   IF W_Ope EQ "C" AND W_CancParc GT Inversion_Sdos.Sdo_Actual THEN
      ASSIGN W_CancParc = Inversion_Sdos.Sdo_Actual
             W_CancParc:SCREEN-VALUE = STRING(Inversion_Sdos.Sdo_Actual).

   APPLY "ENTRY" TO W_ContraPart.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Debe4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Debe4 W-InverS
ON LEAVE OF W_Debe4 IN FRAME F_Invers
DO:
   DEFI VAR W_RowidI AS ROWID.

   ASSIGN W_CancParc.

   IF W_Ope EQ "I" THEN DO:
      ASSIGN W_RowidI = ROWID(Inversion_Sdos).
          
      RUN ParticYTope.

      FIND Inversion_Sdos WHERE ROWID(Inversion_Sdos) EQ W_RowidI NO-LOCK NO-ERROR.
   END.

   IF W_Ope EQ "C" AND W_CancParc GT Inversion_Sdos.Sdo_Actual THEN
      ASSIGN W_CancParc = Inversion_Sdos.Sdo_Actual
             W_CancParc:SCREEN-VALUE = STRING(Inversion_Sdos.Sdo_Actual).

   APPLY "ENTRY" TO W_ContraPart.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_DesCrip
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_DesCrip W-InverS
ON LEAVE OF W_DesCrip IN FRAME F_Invers
DO:
  ASSIGN W_Descrip.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Imp
&Scoped-define SELF-NAME W_FecFin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_FecFin W-InverS
ON LEAVE OF W_FecFin IN FRAME F_Imp /* Fecha  Final */
DO:
  ASSIGN W_FecFin.
  
  IF W_FecIni GT W_FecFin THEN
     ASSIGN W_FecIni = W_FecFin
            W_FecIni:SCREEN-VALUE = STRING(W_FecFin).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_FecIni
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_FecIni W-InverS
ON LEAVE OF W_FecIni IN FRAME F_Imp /* Fecha Inicial */
DO:
  ASSIGN W_FecIni.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Invers
&Scoped-define SELF-NAME W_Haber1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Haber1 W-InverS
ON LEAVE OF W_Haber1 IN FRAME F_Invers
DO:
   DEFI VAR W_RowidI AS ROWID.

   ASSIGN W_CancParc.

   IF W_Ope EQ "I" THEN DO:
      ASSIGN W_RowidI = ROWID(Inversion_Sdos).
          
      RUN ParticYTope.

      FIND Inversion_Sdos WHERE ROWID(Inversion_Sdos) EQ W_RowidI NO-LOCK NO-ERROR.
   END.

   IF W_Ope EQ "C" AND W_CancParc GT Inversion_Sdos.Sdo_Actual THEN
      ASSIGN W_CancParc = Inversion_Sdos.Sdo_Actual
             W_CancParc:SCREEN-VALUE = STRING(Inversion_Sdos.Sdo_Actual).

   APPLY "ENTRY" TO W_ContraPart.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Haber2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Haber2 W-InverS
ON LEAVE OF W_Haber2 IN FRAME F_Invers
DO:
   DEFI VAR W_RowidI AS ROWID.

   ASSIGN W_CancParc.

   IF W_Ope EQ "I" THEN DO:
      ASSIGN W_RowidI = ROWID(Inversion_Sdos).
          
      RUN ParticYTope.

      FIND Inversion_Sdos WHERE ROWID(Inversion_Sdos) EQ W_RowidI NO-LOCK NO-ERROR.
   END.

   IF W_Ope EQ "C" AND W_CancParc GT Inversion_Sdos.Sdo_Actual THEN
      ASSIGN W_CancParc = Inversion_Sdos.Sdo_Actual
             W_CancParc:SCREEN-VALUE = STRING(Inversion_Sdos.Sdo_Actual).

   APPLY "ENTRY" TO W_ContraPart.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Haber3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Haber3 W-InverS
ON LEAVE OF W_Haber3 IN FRAME F_Invers
DO:
   DEFI VAR W_RowidI AS ROWID.

   ASSIGN W_CancParc.

   IF W_Ope EQ "I" THEN DO:
      ASSIGN W_RowidI = ROWID(Inversion_Sdos).
          
      RUN ParticYTope.

      FIND Inversion_Sdos WHERE ROWID(Inversion_Sdos) EQ W_RowidI NO-LOCK NO-ERROR.
   END.

   IF W_Ope EQ "C" AND W_CancParc GT Inversion_Sdos.Sdo_Actual THEN
      ASSIGN W_CancParc = Inversion_Sdos.Sdo_Actual
             W_CancParc:SCREEN-VALUE = STRING(Inversion_Sdos.Sdo_Actual).

   APPLY "ENTRY" TO W_ContraPart.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Haber4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Haber4 W-InverS
ON LEAVE OF W_Haber4 IN FRAME F_Invers
DO:
   DEFI VAR W_RowidI AS ROWID.

   ASSIGN W_CancParc.

   IF W_Ope EQ "I" THEN DO:
      ASSIGN W_RowidI = ROWID(Inversion_Sdos).
          
      RUN ParticYTope.

      FIND Inversion_Sdos WHERE ROWID(Inversion_Sdos) EQ W_RowidI NO-LOCK NO-ERROR.
   END.

   IF W_Ope EQ "C" AND W_CancParc GT Inversion_Sdos.Sdo_Actual THEN
      ASSIGN W_CancParc = Inversion_Sdos.Sdo_Actual
             W_CancParc:SCREEN-VALUE = STRING(Inversion_Sdos.Sdo_Actual).

   APPLY "ENTRY" TO W_ContraPart.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_IntFuturo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_IntFuturo W-InverS
ON LEAVE OF W_IntFuturo IN FRAME F_Invers /* Interès por Liquidar */
DO:
   ASSIGN W_IntPag.
   
   APPLY "ENTRY" TO W_ContraPart.

   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_IntPag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_IntPag W-InverS
ON LEAVE OF W_IntPag IN FRAME F_Invers /* Interès Recibido parcial */
DO:
   ASSIGN W_IntPag.
   
   APPLY "ENTRY" TO W_ContraPart.

   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Canc
&Scoped-define SELF-NAME W_IntReal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_IntReal W-InverS
ON LEAVE OF W_IntReal IN FRAME F_Canc /* Interés Real Liquidado */
DO:
   ASSIGN W_Canc   = TRUE
          W_IntReal.

   RUN CtasCanc.            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Cons
&Scoped-define SELF-NAME W_NitEmiC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_NitEmiC W-InverS
ON LEAVE OF W_NitEmiC IN FRAME F_Cons /* Por Nit Emisor */
DO:
  ASSIGN W_NitEmiC.

  CLOSE QUERY Br_Inv.
  
  OPEN QUERY Br_Inv FOR EACH Inversion_Sdos WHERE Inversion_Sdos.Nit_Emisor EQ W_NitEmiC NO-LOCK
                             BY Inversion_Sdos.Agencia    BY Inversion_Sdos.Nit_Emisor
                             BY Inversion_Sdos.Nro_Titulo BY Inversion_Sdos.Secuen.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Imp
&Scoped-define SELF-NAME W_NitEnt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_NitEnt W-InverS
ON LEAVE OF W_NitEnt IN FRAME F_Imp /* Por Nit Entidad */
DO:
  ASSIGN W_NitEnt.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Invers
&Scoped-define SELF-NAME W_NroCheq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_NroCheq W-InverS
ON LEAVE OF W_NroCheq IN FRAME F_Invers /* Nro.de Cheque */
DO:
  ASSIGN W_NroCheq.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Cons
&Scoped-define SELF-NAME W_NroTitC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_NroTitC W-InverS
ON LEAVE OF W_NroTitC IN FRAME F_Cons /* Por Nro.de Tìtulo */
DO:
  ASSIGN W_NroTitC.

  CLOSE QUERY Br_Inv.
  
  OPEN QUERY Br_Inv FOR EACH Inversion_Sdos WHERE Inversion_Sdos.Nro_Titulo EQ W_NroTitC NO-LOCK
                             BY Inversion_Sdos.Agencia    BY Inversion_Sdos.Nit_Emisor
                             BY Inversion_Sdos.Nro_Titulo BY Inversion_Sdos.Secuen.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Invers
&Scoped-define SELF-NAME W_OpeC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_OpeC W-InverS
ON VALUE-CHANGED OF W_OpeC IN FRAME F_Invers /* Operaciòn */
DO:
  ASSIGN W_Ope                                   = SUBSTRING(W_OpeC:SCREEN-VALUE,1,1)
         Inversion_Sdo.Fec_Prorroga:SENSITIVE    = FALSE
         W_IntPag:SENSITIVE                      = FALSE
         W_CancParc:SENSITIVE                    = FALSE
         Inversion_Sdo.Interes_Causado:SENSITIVE = FALSE.

  RUN HabilVtna.

  IF W_Ope EQ "A" OR W_Ope EQ "S" THEN
     RUN Apertura.
  ELSE IF NOT AVAIL(Inversion_Sdo) THEN DO:
     APPLY "CHOOSE" TO Btn_Consul.
     
     APPLY "ENTRY" TO W_NitEmiC IN FRAME F_Cons.

     RETURN NO-APPLY.
  END.
  ELSE
     FIND FIRST Pro_Inversiones WHERE Pro_Inversiones.Cod_Producto EQ Inversion_Sdos.Cod_Producto
                                  AND Pro_Inversiones.Categoria    EQ Inversion_Sdos.Categoria
                   NO-LOCK NO-ERROR.

  IF  NOT AVAIL(Inversion_Sdo) 
  AND W_Ope NE "A" AND W_Ope NE "S" THEN 
      ASSIGN W_Ope              = "A"
             W_OpeC:SCREEN-VALUE = "Apertura de Inversiòn".
  
  IF W_Ope EQ "C" THEN DO:
     APPLY "ENTRY" TO W_ContraPart.
     RETURN NO-APPLY.
  END.
  
  IF AVAIL(Inversion_Sdo) AND W_Ope EQ "P" THEN DO:
     ASSIGN W_Plazo = Inversion_Sdo.Plazo_Dias   
            W_Tasa  = Inversion_Sdo.Tasa_NomiAnual 
            Inversion_Sdo.Fec_Prorroga:SCREEN-VALUE = STRING(TODAY)
            Inversion_Sdo.Fec_Prorroga:SENSITIVE    = TRUE
            Inversion_Sdo.Fec_Vcto:SENSITIVE        = TRUE
            Inversion_Sdo.LiqInt_Dias:SENSITIVE     = TRUE
            Inversion_Sdo.Plazo:SENSITIVE           = TRUE
            Inversion_Sdo.Tasa_NomiAnual:SENSITIVE  = TRUE.
            
     APPLY "ENTRY" TO Inversion_Sdo.Fec_Prorroga.
     RETURN NO-APPLY.
  END.

  IF W_Ope EQ "R" THEN DO:
     W_IntPag:SENSITIVE = TRUE.
     
     APPLY "ENTRY" TO W_IntPag.
     RETURN NO-APPLY.
  END.
  ELSE IF W_Ope EQ "I" OR W_Ope EQ "V" THEN DO:
     ASSIGN W_CancParc:SENSITIVE = TRUE
            W_CancParc:LABEL     = "Cancelaciòn Parcial".

     IF AVAIL(Pro_Inversiones) AND Pro_Inversiones.Id_TasaUnid EQ 1 THEN 
        ASSIGN Inversion_Sdos.Nro_Unidades:SENSITIVE = TRUE
               Inversion_Sdos.Valor_Unidad:SENSITIVE = TRUE.
     IF W_Ope EQ "I" THEN
        ASSIGN W_CancParc:SENSITIVE = TRUE
               W_CancParc:LABEL     = "Valor Incremento".

     APPLY "ENTRY" TO W_CancParc.
     RETURN NO-APPLY.
  END.
  ELSE IF W_Ope EQ "S" OR W_Ope EQ "M" THEN
     ASSIGN Inversion_Sdos.Interes_Causado:SENSITIVE  = TRUE         
            Inversion_Sdos.Interes_Recibido:SENSITIVE = TRUE
            Inversion_Sdos.TipTasa_FV:SENSITIVE       = TRUE.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Canc
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-InverS 


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
  
  RUN ProcMainBlock.
  
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Apertura W-InverS 
PROCEDURE Apertura :
/*------------------------------------------------------------------------------
     Invocado desde triggers W-opeC -Combo de operaciones-
  Purpose: Habilita Ventana para Apertura o Saldo inicial.
------------------------------------------------------------------------------*/
  RELEASE Inversion_Sdos.
    
  ASSIGN Inversion_Sdos.Agencia:SCREEN-VALUE IN FRAME F_Invers = STRING(W_Agencia)
         Inversion_Sdos.Nit_Emisor:SCREEN-VALUE        = " "
         Inversion_Sdos.Nro_Titulo:SCREEN-VALUE        = " "
         Inversion_Sdos.Secuencia:SCREEN-VALUE         = "00"
         Inversion_Sdos.Nombre_Emisor:SCREEN-VALUE     = " "
         Inversion_Sdos.Fec_Apertura:SCREEN-VALUE      = STRING(TODAY)
         Inversion_Sdos.Cuenta_Contab:SCREEN-VALUE     = " "
         Inversion_Sdos.VrInvers_Inic:SCREEN-VALUE     = "0.00"
         Inversion_Sdos.Sdo_Actual:SCREEN-VALUE        = "0.00"
         Inversion_Sdos.Descrip:SCREEN-VALUE           = " "
         Inversion_Sdos.Fec_Vcto:SCREEN-VALUE          = " "
         Inversion_Sdos.Tasa_NomiAnual:SCREEN-VALUE    = "0.00"
         Inversion_Sdos.Plazo_Dias:SCREEN-VALUE        = "0"
         Inversion_Sdos.Interes_Causado:SCREEN-VALUE   = "0.00"
         Inversion_Sdos.Interes_Recibido:SCREEN-VALUE  = "0.00"
         W_UnidXV:SCREEN-VALUE                         = "0.00".
         
  ASSIGN Inversion_Sdos.Nit_Emisor:SENSITIVE = TRUE
         Inversion_Sdos.Nro_Titulo:SENSITIVE = TRUE
         Inversion_Sdos.Secuencia:SENSITIVE  = TRUE
         W_NomCta:SCREEN-VALUE               = ""
         W_Contrapart:SCREEN-VALUE           = ""
         W_Contrapart                        = ""
         W_SiCheq                            = FALSE
         W_NroCheq                           = ""
         W_NroCheq:SCREEN-VALUE              = ""
         Inversion_Sdo.Interes_Causado:SENSITIVE = FALSE
         Inversion_Sdos.LiqInt_Dias:SCREEN-VALUE = "0" 
         W_Descrip:SCREEN-VALUE                  = ""
         W_Descrip                               = ""
         Inversion_Sdo.Fec_Prorroga:SENSITIVE    = FALSE
         Inversion_Sdo.Fec_Prorroga:SCREEN-VALUE = ""
         W_IntPag:SENSITIVE                      = FALSE
         W_IntPag:SCREEN-VALUE                   = "0.00"
         W_IntPag                                = 0
         W_CancParc:SENSITIVE                    = FALSE
         W_CancParc:SCREEN-VALUE                 = "0.00"
         W_CancParc                              = 0.

  ASSIGN Inversion_Sdos.VrInvers_Inic:SENSITIVE    = TRUE
         Inversion_Sdos.Categoria:SENSITIVE        = TRUE  
         Cmb_Pdctos:SENSITIVE                      = TRUE           
         Inversion_Sdos.Fec_Apertura:SENSITIVE     = TRUE         
         Inversion_Sdos.Fec_Vcto:SENSITIVE         = TRUE         
         Inversion_Sdos.Plazo_Dias:SENSITIVE       = TRUE
         Inversion_Sdos.LiqInt_Dias:SENSITIVE      = TRUE
         Inversion_Sdos.Calif_Invers:SCREEN-VALUE  = "A"
         Inversion_Sdos.TipTasa:SCREEN-VALUE       = "0"
         Inversion_Sdos.TipTasa:SENSITIVE          = TRUE
         Inversion_Sdos.Indicador:SENSITIVE        = FALSE
         Inversion_Sdos.Puntos_Adic:SENSITIVE      = FALSE
         Inversion_Sdos.Tasa_NomiAn:SENSITIVE      = TRUE
         Inversion_Sdos.Puntos_Adic:SCREEN-VALUE   = "0.00"
         Inversion_Sdos.Indicador:SCREEN-VALUE     = "0"
         Inversion_Sdos.Nro_Unidades:SCREEN-VALUE  = "0.00"
         Inversion_Sdos.Valor_Unidad:SCREEN-VALUE  = "0.00"
         Inversion_Sdos.Tasa_NomiAMcd:SCREEN-VALUE = "0.00".   

  ASSIGN W_Debe1:SCREEN-VALUE   = "0.00"
         W_Debe2:SCREEN-VALUE   = "0.00"
         W_Debe3:SCREEN-VALUE   = "0.00"
         W_Debe4:SCREEN-VALUE   = "0.00"
         W_Haber1:SCREEN-VALUE  = "0.00"
         W_Haber2:SCREEN-VALUE  = "0.00"
         W_Haber3:SCREEN-VALUE  = "0.00"
         W_Haber4:SCREEN-VALUE  = "0.00"
         W_IntFuturo:SCREEN-VALUE = "0.00".
        
  APPLY "ENTRY" TO Inversion_Sdos.Nit_Emisor.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ContabTx W-InverS 
PROCEDURE ContabTx :
/*------------------------------------------------------------------------------
      Invocado desde Btn_Salva.
  Purpose:  Contabiliza las aperturas, Pago-Intereses y las cancelaciones.
 ------------------------------------------------------------------------------*/
  CREATE Mov_Contable.
  ASSIGN Mov_Contable.Agencia         = Inversion_Sdos.Agencia 
         Mov_Contable.Destino         = W_Agencia
         Mov_Contable.Comprobante     = Comprobantes.Comprobante
         Mov_Contable.Num_Documento   = Comprobantes.Secuencia
         Mov_Contable.Fec_Contable    = TODAY
         Mov_Contable.Fec_Grabacion   = TODAY
         Mov_Contable.Comentario      = W_Descrip
         Mov_Contable.Usuario         = W_Usuario
         Mov_Contable.Hora            = TIME
         Mov_Contable.Cen_Costos      = W_CenCosGral
         Mov_Contable.Nit             = Inversion_Sdos.Nit_Emi
         Mov_Contable.Doc_Refer       = Inversion_Sdos.Nro_Titulo
         Mov_Contable.Cuenta          = Pro_Inversiones.Cuenta_Inversion_DB.

  IF W_Ope = "A" THEN
      Mov_Contable.Db = Inversion_Sdos.VrInvers_Inic.
  ELSE DO:
      IF W_Ope = "C" THEN
          Mov_Contable.Cr = Inversion_Sdos.Sdo_Actual.
      ELSE DO:
          IF W_Ope = "I" THEN
              Mov_Contable.Db = W_CancParc.
          ELSE DO:
              IF W_Ope = "V" THEN
                  Mov_Contable.Cr = W_CancParc.
              ELSE DO:
                  IF W_Ope = "R" THEN DO:
                      Mov_Contable.Cr = W_IntPag.
                      Mov_Contable.Cuenta = Pro_Inversiones.CtaRendimiento_DB.
                  END.
              END.
          END.
      END.
  END.

  CREATE Mov_Contable.
  ASSIGN Mov_Contable.Agencia         = Inversion_Sdos.Agencia 
         Mov_Contable.Destino         = W_Agencia
         Mov_Contable.Comprobante     = Comprobantes.Comprobante
         Mov_Contable.Num_Documento   = Comprobantes.Secuencia
         Mov_Contable.Fec_Contable    = TODAY
         Mov_Contable.Fec_Grabacion   = TODAY
         Mov_Contable.Comentario      = W_Descrip
         Mov_Contable.Usuario         = W_Usuario
         Mov_Contable.Hora            = TIME
         Mov_Contable.Cen_Costos      = W_CenCosGral
         Mov_Contable.Doc_Refer       = W_NroCheq
         Mov_Contable.Cuenta          = W_Contrapart.

  IF W_Ope = "A" THEN DO:
      Mov_Contable.Cr = Inversion_Sdos.VrInvers_Inic.
  END.
  ELSE
      IF W_Ope = "C" THEN DO:
          Mov_Contable.Db = DEC(W_Debe-2:SCREEN-VALUE IN FRAME F_Canc).
      END.
      ELSE
          IF W_Ope = "I" THEN DO:
              Mov_Contable.Cr = W_CancParc.
          END.
          ELSE
              IF W_Ope = "V" THEN DO:
                  Mov_Contable.Db = W_CancParc - W_RetGMF.
              END.
              ELSE
                  IF W_Ope = "R" THEN DO:
                      Mov_Contable.Db = W_IntPag.
                  END.

  FIND Cuentas WHERE Cuentas.Cuenta EQ W_Contrapart NO-LOCK NO-ERROR.
  
  IF  Cuentas.Car_Efectivo EQ 2
  AND Cuentas.Id_Caja      
  AND Cuentas.Id_Nit THEN DO:
      FIND FIRST Usuarios WHERE Usuarios.Usuario EQ W_Usuario NO-LOCK NO-ERROR.
      
      Mov_Contable.Nit = Inversion_Sdos.Nit_Emi.   /*Usuarios.Cedula.*/
  END.
  ELSE
      IF Cuentas.Id_Nit THEN DO:
          Mov_Contable.Nit = Inversion_Sdos.Nit_Emi.
      END.


  IF W_NroCheq LE " " THEN DO:
      Mov_Contable.Doc_Refer = Inversion_Sdos.Nro_Titulo.
  END.

  IF W_RetGMF GT 0 THEN DO:        /*Ret.GMF*/ 
     FIND FIRST Deducible WHERE Deducible.Cod_Ded EQ Entidad.Deduc_GMF 
                               AND Deducible.Estado  EQ 1 NO-LOCK NO-ERROR.
     CREATE Mov_Contable.                                        
     ASSIGN Mov_Contable.Agencia     = Inversion_Sdos.Agencia                    
            Mov_Contable.Destino         = W_Agencia                             
            Mov_Contable.Comprobante     = Comprobantes.Comprobante              
            Mov_Contable.Num_Documento   = Comprobantes.Secuencia                
            Mov_Contable.Fec_Contable    = TODAY                                 
            Mov_Contable.Fec_Grabacion   = TODAY                                 
            Mov_Contable.Comentario      = W_Descrip                             
            Mov_Contable.Usuario         = W_Usuario                             
            Mov_Contable.Hora            = TIME                                  
            Mov_Contable.Cen_Costos      = W_CenCosGral                          
            Mov_Contable.Nit             = Inversion_Sdos.Nit_Emi                
            Mov_Contable.Doc_Refer       = Inversion_Sdos.Nro_Titulo             
            Mov_Contable.Cuenta          = Deducible.Cuenta          
            Mov_Contable.Db              = W_RetGMF.                             
  END.
           
  IF  (W_Ope = "A" OR W_Ope = "I")
  AND W_SiCheq THEN
      RUN ImpCheque.
  ELSE IF W_Ope = "C" THEN DO:
      IF W_RetFte GT 0 THEN DO:
         CREATE Mov_Contable.                   /*Ret.Fuente*/
         ASSIGN Mov_Contable.Agencia     = Inversion_Sdos.Agencia 
                Mov_Contable.Destino         = W_Agencia                    
                Mov_Contable.Comprobante     = Comprobantes.Comprobante     
                Mov_Contable.Num_Documento   = Comprobantes.Secuencia       
                Mov_Contable.Fec_Contable    = TODAY                        
                Mov_Contable.Fec_Grabacion   = TODAY                        
                Mov_Contable.Comentario      = W_Descrip                    
                Mov_Contable.Usuario         = W_Usuario                    
                Mov_Contable.Hora            = TIME                         
                Mov_Contable.Cen_Costos      = W_CenCosGral                 
                Mov_Contable.Nit             = Inversion_Sdos.Nit_Emi       
                Mov_Contable.Doc_Refer       = Inversion_Sdos.Nro_Titulo    
                Mov_Contable.Cuenta          = Pro_Inversiones.CtaRetFte_DB 
                Mov_Contable.Db              = W_RetFte.                    
      END.

      /* oakley */
         
      CREATE Mov_Contable.                   /*Ingr.x Intereses*/
      ASSIGN Mov_Contable.Agencia     = Inversion_Sdos.Agencia 
         Mov_Contable.Destino         = W_Agencia
         Mov_Contable.Comprobante     = Comprobantes.Comprobante
         Mov_Contable.Num_Documento   = Comprobantes.Secuencia
         Mov_Contable.Fec_Contable    = TODAY
         Mov_Contable.Fec_Grabacion   = TODAY
         Mov_Contable.Comentario      = W_Descrip
         Mov_Contable.Usuario         = W_Usuario
         Mov_Contable.Hora            = TIME
         Mov_Contable.Cen_Costos      = W_CenCosGral
         Mov_Contable.Nit             = Inversion_Sdos.Nit_Emi
         Mov_Contable.Doc_Refer       = Inversion_Sdos.Nro_Titulo
         Mov_Contable.Cuenta          = Pro_Inversiones.CtaRendimiento_Cr
         Mov_Contable.Cr              = W_IntReal - Inversion_Sdos.Interes_Causado.

      IF (W_IntReal - Inversion_Sdos.Interes_Causado) LT 0 AND Pro_Inversiones.Id_TasaUnid EQ 0 THEN DO:
          ASSIGN Mov_Contable.Db = Inversion_Sdos.Interes_Causado - W_IntReal
                 Mov_Contable.Cr = 0.
      END.
      ELSE
          IF Pro_Inversiones.Id_TasaUnid EQ 1 THEN DO:
              ASSIGN Mov_Contable.Db = 0
                     Mov_Contable.Cr = 0.

              IF W_IntReal NE (Inversion_Sdos.Sdo_Actual + Inversion_Sdos.Interes_Causado) THEN DO:
                  IF W_IntReal GT (Inversion_Sdos.Sdo_Actual + Inversion_Sdos.Interes_Causado) THEN DO:
                      Mov_Contable.Cr = W_IntReal - (Inversion_Sdos.Sdo_Actual + Inversion_Sdos.Interes_Causado).
                  END.
                  ELSE DO:
                      Mov_Contable.Db = (Inversion_Sdos.Sdo_Actual + Inversion_Sdos.Interes_Causado) - W_IntReal.
                  END.
              END.
          END.

      IF Inversion_Sdos.Interes_Causado NE 0 THEN DO:
         CREATE Mov_Contable.                   /*El pago de la CxC Intereses*/
         ASSIGN Mov_Contable.Agencia     = Inversion_Sdos.Agencia              
            Mov_Contable.Destino         = W_Agencia                           
            Mov_Contable.Comprobante     = Comprobantes.Comprobante            
            Mov_Contable.Num_Documento   = Comprobantes.Secuencia              
            Mov_Contable.Fec_Contable    = TODAY                               
            Mov_Contable.Fec_Grabacion   = TODAY                               
            Mov_Contable.Comentario      = W_Descrip                           
            Mov_Contable.Usuario         = W_Usuario                           
            Mov_Contable.Hora            = TIME                                
            Mov_Contable.Cen_Costos      = W_CenCosGral                        
            Mov_Contable.Nit             = Inversion_Sdos.Nit_Emi              
            Mov_Contable.Doc_Refer       = Inversion_Sdos.Nro_Titulo           
            Mov_Contable.Cuenta          = Pro_Inversiones.CtaRendimiento_Db   
            Mov_Contable.Cr              = Inversion_Sdos.Interes_Causado.  

         IF Inversion_Sdos.Interes_Causado LT 0 THEN DO:
             ASSIGN Mov_Contable.Db     = Inversion_Sdos.Interes_Causado * -1
                    Mov_Contable.Cuenta = Pro_Inversiones.CtaRendimiento_Cr.
         END.
      END.
      
  END.

  RUN ImpCpte. 
      
END PROCEDURE.
         
/*     IF Inversion_Sdos.Vr_Valoracion GT 0 THEN DO:                    /* Reversa Ctas-Orden x Valoración*/
        CREATE Mov_Contable.                            /*Créditos*/
        ASSIGN Mov_Contable.Agencia         = Inversion_Sdos.Agencia 
               Mov_Contable.Destino         = W_Agencia
               Mov_Contable.Comprobante     = Comprobantes.Comprobante
               Mov_Contable.Num_Documento   = Comprobantes.Secuencia
               Mov_Contable.Fec_Contable    = TODAY
               Mov_Contable.Fec_Grabacion   = TODAY
               Mov_Contable.Comentario      = "Canc.Ctas-Orden valoración"
               Mov_Contable.Usuario         = W_Usuario
               Mov_Contable.Hora            = TIME
               Mov_Contable.Cen_Costos      = W_CenCosGral
               Mov_Contable.Nit             = Inversion_Sdos.Nit_Emi
               Mov_Contable.Doc_Refer       = Inversion_Sdos.Nro_Titulo
               Mov_Contable.Cuenta          = Inversion_Sdos.CfgCuentas_Deb[3]        
            /*   Mov_Contable.Natur           = "CR"*/
               Mov_Contable.Cr              = Inversion_Sdos.Vr_Valoracion.

        CREATE Mov_Contable.                            /*Débitos*/
        ASSIGN Mov_Contable.Agencia         = Inversion_Sdos.Agencia 
               Mov_Contable.Destino         = W_Agencia
               Mov_Contable.Comprobante     = Comprobantes.Comprobante
               Mov_Contable.Num_Documento   = Comprobantes.Secuencia
               Mov_Contable.Fec_Contable    = TODAY
               Mov_Contable.Fec_Grabacion   = TODAY
               Mov_Contable.Comentario      = "Canc.Ctas-Orden valoración"
               Mov_Contable.Usuario         = W_Usuario
               Mov_Contable.Hora            = TIME
               Mov_Contable.Cen_Costos      = W_CenCosGral
               Mov_Contable.Nit             = Inversion_Sdos.Nit_Emi
               Mov_Contable.Doc_Refer       = Inversion_Sdos.Nro_Titulo
               Mov_Contable.Cuenta          = Inversion_Sdos.CfgCuentas_Cre[3]        
              /* Mov_Contable.Natur           = "DB"*/
               Mov_Contable.Db              = Inversion_Sdos.Vr_Valoracion.          
     END.    
*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CtasCanc W-InverS 
PROCEDURE CtasCanc :
/*------------------------------------------------------------------------------
     Invocado desde Btn_Salva.
  Purpose:    Arma Cuentas contables para la cancelación.
------------------------------------------------------------------------------*/
  FIND FIRST Pro_Inversiones WHERE Pro_Inversiones.Cod_Producto EQ INTEG(Inversion_Sdos.Cod_Producto:SCREEN-VALUE IN FRAME F_Invers)
                               AND Pro_Inversiones.Categoria    EQ INTEG(Inversion_Sdos.Categoria:SCREEN-VALUE)
                                   NO-LOCK NO-ERROR.
  IF NOT AVAIL(Pro_Inversiones) THEN DO:
     MESSAGE "El Producto de La Inversión no-existe...Revise por favor"  VIEW-AS ALERT-BOX.
     RETURN.
  END.

  ASSIGN W_Debe-2:SCREEN-VALUE IN FRAME F_Canc = "0.00"
         W_Debe-3:SCREEN-VALUE                 = "0.00"
         W_Haber-4:SCREEN-VALUE                = "0.00"
         W_Debe-4:SCREEN-VALUE                 = "0.00"
         W_Haber-5:SCREEN-VALUE                = "0.00"
         W_Debe-6:SCREEN-VALUE                 = "0.00".

  IF Pro_Inversiones.Id_TasaUnid EQ 0 THEN DO:
     ASSIGN W_IntReal:LABEL = "Interés Real Liquidado".

     IF NOT W_Canc THEN
        W_IntReal = (((Inversion_Sdos.Sdo_Actual * Inversion_Sdos.Tasa_NomiAnual) /
                    365) * Inversion_Sdos.Plazo_Dias) / 100.
     ASSIGN W_RetFte  = (W_IntReal * Pro_Inversion.Porc_RetFte) / 100.
  END.
  ELSE DO:
     ASSIGN W_IntReal:LABEL = "Total Venta".

     IF NOT W_Canc THEN
        W_IntReal = Inversion_Sdos.Sdo_Actual + Inversion_Sdos.Interes_Causado.

     IF Inversion_Sdos.Interes_Causado GT 0 THEN
        ASSIGN W_RetFte  = (Inversion_Sdos.Interes_Causado * Pro_Inversion.Porc_RetFte) / 100.
  END.

  FIND Cuentas WHERE Cuentas.Cuenta EQ Pro_Inversiones.Cuenta_Inversion_DB
                 AND Cuentas.Tipo   EQ 2
                 AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.

  ASSIGN W_CtaPPal:SCREEN-VALUE   = Pro_Inversiones.Cuenta_Inversion_DB
         W_IntReal:SCREEN-VALUE   = STRING(W_IntReal)
         DescCta:SCREEN-VALUE     = Cuentas.Nombre
         W_Haber:SCREEN-VALUE     = STRING(Inversion_Sdos.Sdo_Actual)
         W_CtaPPal-2:SCREEN-VALUE = W_ContraPart
         W_Debe-2:SCREEN-VALUE    = STRING(Inversion_Sdos.Sdo_Actual + W_IntReal - W_RetFte)
         DescCta-2:SCREEN-VALUE   = W_NomCta
         W_Debe-3:SCREEN-VALUE    = STRING(W_RetFte)
         W_CtaPPal-3:SCREEN-VALUE = Pro_Inversiones.CtaRetFte_DB
         W_CtaPPal-4:SCREEN-VALUE = Pro_Inversiones.CtaRendimiento_Cr
         W_CtaPPal-5:SCREEN-VALUE = Pro_Inversiones.CtaRendimiento_DB
         W_Haber-5:SCREEN-VALUE   = STRING(Inversion_Sdos.Interes_Causado).
        
  IF Tg_GMF AND Pro_Inversiones.Id_TasaUnid EQ 0 THEN DO:
     FIND FIRST Entidad   WHERE Entidad.Estado    EQ 1 NO-LOCK NO-ERROR.
     FIND FIRST Deducible WHERE Deducible.Cod_Ded EQ Entidad.Deduc_GMF 
                            AND Deducible.Estado  EQ 1 NO-LOCK NO-ERROR.
     IF AVAIL(Deducible) THEN DO:
        FIND Cuentas WHERE Cuentas.Cuenta  EQ Deducible.Cuenta
                       AND Cuentas.Tipo    EQ 2
                       AND Cuentas.Estado  EQ 1 NO-LOCK NO-ERROR.
        ASSIGN W_CtaPPal-6:SCREEN-VALUE = Deducible.Cuenta
               DescCta-6:SCREEN-VALUE   = Cuentas.Nombre
               W_RetGMF                 = (Inversion_Sdos.Sdo_Actual + W_IntReal - W_RetFte)
                                           * Deducible.Valor
               W_Debe-2:SCREEN-VALUE = STRING(Inversion_Sdos.Sdo_Actual + W_IntReal - W_RetFte - W_RetGMF)
               W_Debe-6:SCREEN-VALUE = STRING(W_RetGMF).             
     END.
  END.
         
  IF Pro_Inversiones.Id_TasaUnid EQ 0 THEN DO:
     IF W_IntReal GE Inversion_Sdos.Interes_Causado THEN                                    
        ASSIGN W_Haber-4:SCREEN-VALUE  = STRING(W_IntReal - Inversion_Sdos.Interes_Causado).
     ELSE                                                                                   
        ASSIGN W_Debe-4:SCREEN-VALUE   = STRING(Inversion_Sdos.Interes_Causado - W_IntReal) 
               W_Haber-5:SCREEN-VALUE  = "0.00".                                            
  END.
  ELSE DO:
     W_Debe-2:SCREEN-VALUE = STRING(W_IntReal - W_RetFte).

     IF W_IntReal NE (Inversion_Sdos.Sdo_Actual + Inversion_Sdos.Interes_Causado) THEN DO:
        IF W_IntReal GT (Inversion_Sdos.Sdo_Actual + Inversion_Sdos.Interes_Causado) THEN                                    
           ASSIGN W_Haber-4:SCREEN-VALUE  = STRING(W_IntReal - (Inversion_Sdos.Sdo_Actual + Inversion_Sdos.Interes_Causado))
                  W_Debe-4:SCREEN-VALUE   = "0.00".
        ELSE                                                                                   
           ASSIGN W_Debe-4:SCREEN-VALUE   = STRING((Inversion_Sdos.Sdo_Actual + Inversion_Sdos.Interes_Causado) - W_IntReal) 
                  W_Haber-4:SCREEN-VALUE  = "0.00"
                  W_Haber-5:SCREEN-VALUE  = "0.00".                                             
     END.
     ELSE ASSIGN W_Haber-4:SCREEN-VALUE = "0.00"
                 W_Debe-4:SCREEN-VALUE  = "0.00"
                 W_Haber-5:SCREEN-VALUE = "0.00".    
  END.

  FIND Cuentas WHERE Cuentas.Cuenta EQ Pro_Inversiones.CtaRetFte_DB
                AND Cuentas.Tipo    EQ 2
                AND Cuentas.Estado  EQ 1 NO-LOCK NO-ERROR.
  ASSIGN DescCta-3:SCREEN-VALUE     = Cuentas.Nombre.

  FIND Cuentas WHERE Cuentas.Cuenta EQ Pro_Inversiones.CtaRendimiento_Cr
                AND Cuentas.Tipo    EQ 2
                AND Cuentas.Estado  EQ 1 NO-LOCK NO-ERROR.
  ASSIGN DescCta-4:SCREEN-VALUE     = Cuentas.Nombre.

  FIND Cuentas WHERE Cuentas.Cuenta EQ Pro_Inversiones.CtaRendimiento_DB
                AND Cuentas.Tipo    EQ 2
                AND Cuentas.Estado  EQ 1 NO-LOCK NO-ERROR.
  ASSIGN DescCta-5:SCREEN-VALUE     = Cuentas.Nombre.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CtasContab W-InverS 
PROCEDURE CtasContab :
/*------------------------------------------------------------------------------
     Invocado desde Btn-Salva.
  Purpose:  visualiza Ctas para contabilizar antes de salvar.
------------------------------------------------------------------------------*/
  ASSIGN W_Debe1:SCREEN-VALUE IN FRAME F_Invers  = "0.00"
         W_Debe2:SCREEN-VALUE   = "0.00"
         W_Debe3:SCREEN-VALUE   = "0.00"
         W_Debe4:SCREEN-VALUE   = "0.00"
         W_Haber1:SCREEN-VALUE  = "0.00"
         W_Haber2:SCREEN-VALUE  = "0.00"
         W_Haber3:SCREEN-VALUE  = "0.00"
         W_Haber4:SCREEN-VALUE  = "0.00"
         W_RetGMF               = 0.

  FIND FIRST Pro_Inversiones WHERE Pro_Inversiones.Cod_Producto EQ INTEG(Inversion_Sdos.Cod_Producto:SCREEN-VALUE)
                               AND Pro_Inversiones.Categoria    EQ INTEG(Inversion_Sdos.Categoria:SCREEN-VALUE)
                                   NO-LOCK NO-ERROR.
  IF NOT AVAIL(Pro_Inversiones) THEN DO:
     MESSAGE "El Producto de La Inversión no-existe...Revise por favor"  VIEW-AS ALERT-BOX.
     RETURN.
  END.

  IF W_Ope EQ "A" OR W_Ope EQ "I" THEN DO:
     ASSIGN W_Debe1:SCREEN-VALUE  = STRING(Inversion_Sdos.VrInvers_Inic:SCREEN-VALUE)
            W_Haber2:SCREEN-VALUE = STRING(Inversion_Sdos.VrInvers_Inic:SCREEN-VALUE).

     IF W_Ope EQ "I" THEN
        ASSIGN W_Debe1:SCREEN-VALUE  = STRING(W_CancParc)
               W_Haber2:SCREEN-VALUE = STRING(W_CancParc).
  END.
  ELSE IF W_Ope EQ "V" THEN DO:
     IF Tg_GMF THEN DO:
        FIND FIRST Entidad   WHERE Entidad.Estado    EQ 1 NO-LOCK NO-ERROR.
        FIND FIRST Deducible WHERE Deducible.Cod_Ded EQ Entidad.Deduc_GMF 
                               AND Deducible.Estado  EQ 1 NO-LOCK NO-ERROR.
        IF AVAIL(Deducible) THEN DO:
           FIND Cuentas WHERE Cuentas.Cuenta EQ Deducible.Cuenta
                         AND Cuentas.Tipo    EQ 2
                         AND Cuentas.Estado  EQ 1 NO-LOCK NO-ERROR.
           ASSIGN W_NomCta4:SCREEN-VALUE = Cuentas.Nombre 
                  W_RetGMF               = (W_CancParc * Deducible.Valor)
                  W_CtaGMF:SCREEN-VALUE  = Deducible.Cuenta.
        END.
     END.

     ASSIGN W_Haber1:SCREEN-VALUE = STRING(W_CancParc)
            W_Debe2:SCREEN-VALUE  = STRING(W_CancParc - W_RetGMF)
            W_Debe4:SCREEN-VALUE  = STRING(W_RetGMF).
  END. 
  ELSE IF W_Ope EQ "R" THEN DO:
     FIND Cuentas WHERE Cuentas.Cuenta EQ Pro_Inversiones.CtaRendimiento_Db
                    AND Cuentas.Tipo   EQ 2
                    AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
     IF AVAIL(Cuentas) THEN 
        ASSIGN W_NomCta4:SCREEN-VALUE = Cuentas.Nombre
               W_CtaRend:SCREEN-VALUE = Pro_Inversiones.CtaRendimiento_Db.

     ASSIGN W_Haber4:SCREEN-VALUE = STRING(W_IntPag)
            W_Debe2:SCREEN-VALUE  = STRING(W_IntPag).
  END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-InverS  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-InverS)
  THEN DELETE WIDGET W-InverS.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-InverS  _DEFAULT-ENABLE
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
  DISPLAY Tg_GMF W_OpeC Cmb_pdctos W_ContraPart W_NroCheq W_DesCrip W_CancParc 
          W_NomCta W_IntPag W_NomInd W_NomCta2 W_Debe1 W_Haber1 W_Debe2 W_Haber2 
          W_CtaRend W_NomCta3 W_Debe3 W_Haber3 W_CtaGMF W_NomCta4 W_Debe4 
          W_Haber4 W_IntFuturo W_UnidXV 
      WITH FRAME F_Invers IN WINDOW W-InverS.
  IF AVAILABLE Inversion_Sdos THEN 
    DISPLAY Inversion_Sdos.Nro_Unidades Inversion_Sdos.Tasa_NomiAMcdo 
          Inversion_Sdos.Valor_Unidad Inversion_Sdos.Agencia 
          Inversion_Sdos.Nit_Emisor Inversion_Sdos.Nro_Titulo 
          Inversion_Sdos.Secuencia Inversion_Sdos.Nombre_Emisor 
          Inversion_Sdos.Fec_Apertura Inversion_Sdos.Calif_Emisor 
          Inversion_Sdos.Calif_Invers Inversion_Sdos.Cod_Producto 
          Inversion_Sdos.TipTasa_FV Inversion_Sdos.Indicador 
          Inversion_Sdos.Puntos_Adic Inversion_Sdos.Tasa_NomiAnual 
          Inversion_Sdos.LiqInt_Dias Inversion_Sdos.Plazo_Dias 
          Inversion_Sdos.VrInvers_Inic Inversion_Sdos.Categoria 
          Inversion_Sdos.Fec_Prorroga Inversion_Sdos.Cuenta_Contab 
          Inversion_Sdos.Descrip Inversion_Sdos.Fec_Vcto 
          Inversion_Sdos.Sdo_Actual Inversion_Sdos.Interes_Recibido 
          Inversion_Sdos.Interes_Causado 
      WITH FRAME F_Invers IN WINDOW W-InverS.
  ENABLE Inversion_Sdos.Nro_Unidades W_OpeC Btn_ayuda Btn_Salir BUTTON-5 
         Inversion_Sdos.Nit_Emisor Inversion_Sdos.Nro_Titulo 
         Inversion_Sdos.Secuencia Inversion_Sdos.Fec_Apertura 
         Inversion_Sdos.Calif_Invers Inversion_Sdos.Cod_Producto Cmb_pdctos 
         Inversion_Sdos.Tasa_NomiAnual Inversion_Sdos.LiqInt_Dias 
         Inversion_Sdos.Plazo_Dias Inversion_Sdos.VrInvers_Inic W_ContraPart 
         W_NroCheq W_DesCrip Inversion_Sdos.Categoria 
         Inversion_Sdos.Fec_Prorroga Inversion_Sdos.Descrip 
         Inversion_Sdos.Fec_Vcto W_CancParc Inversion_Sdos.Interes_Recibido 
         Inversion_Sdos.Interes_Causado W_IntPag Btn_Salva Btn_Consul Btn_Imp 
         RECT-3 RECT-306 RECT-307 RECT-308 RECT-311 RECT-312 RECT-313 
      WITH FRAME F_Invers IN WINDOW W-InverS.
  {&OPEN-BROWSERS-IN-QUERY-F_Invers}
  DISPLAY W_NitEmiC W_NroTitC Tg_Todos 
      WITH FRAME F_Cons IN WINDOW W-InverS.
  ENABLE Br_Inv Btn_Salir-2 W_NitEmiC W_NroTitC Tg_Todos 
      WITH FRAME F_Cons IN WINDOW W-InverS.
  {&OPEN-BROWSERS-IN-QUERY-F_Cons}
  DISPLAY W_CtaPpal DescCta W_Debe W_Haber W_CtaPpal-2 DescCta-2 W_Debe-2 
          W_Haber-2 W_CtaPpal-3 DescCta-3 W_Debe-3 W_Haber-3 W_CtaPpal-4 
          DescCta-4 W_Debe-4 W_Haber-4 W_CtaPpal-5 DescCta-5 W_Debe-5 W_Haber-5 
          W_CtaPpal-6 DescCta-6 W_Debe-6 W_Haber-6 W_CtaPpal-7 DescCta-7 
          W_Debe-7 W_Haber-7 W_CtaPpal-8 DescCta-8 W_Debe-8 W_Haber-8 W_IntReal 
      WITH FRAME F_Canc IN WINDOW W-InverS.
  ENABLE W_IntReal Btn_ContCanc Btn_Reg 
      WITH FRAME F_Canc IN WINDOW W-InverS.
  {&OPEN-BROWSERS-IN-QUERY-F_Canc}
  DISPLAY Rs_Inf W_NitEnt W_FecIni W_FecFin 
      WITH FRAME F_Imp IN WINDOW W-InverS.
  ENABLE Rs_Inf W_NitEnt W_FecIni W_FecFin Btn_Imp-2 Btn_FinImp 
      WITH FRAME F_Imp IN WINDOW W-InverS.
  {&OPEN-BROWSERS-IN-QUERY-F_Imp}
  VIEW W-InverS.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HabilVtna W-InverS 
PROCEDURE HabilVtna :
/*------------------------------------------------------------------------------
  Purpose:  Habilita campos ventana ppal.   
------------------------------------------------------------------------------*/
      ASSIGN Inversion_Sdos.VrInvers_Inic:SENSITIVE IN FRAME F_Invers = FALSE
             Inversion_Sdos.Agencia:SENSITIVE          = FALSE                
             Inversion_Sdos.Nit_Emi:SENSITIVE          = FALSE
             Inversion_Sdos.Nro_Titulo:SENSITIVE       = FALSE
             Inversion_Sdos.Secuencia:SENSITIVE        = FALSE
             Inversion_Sdos.Categoria:SENSITIVE        = FALSE
             Cmb_Pdctos:SENSITIVE                      = FALSE 
             Inversion_Sdos.Interes_Causado:SENSITIVE  = FALSE
             Inversion_Sdos.Interes_Recibido:SENSITIVE = FALSE
             Inversion_Sdos.Fec_Apertura:SENSITIVE     = FALSE
             Inversion_Sdos.Fec_Vcto:SENSITIVE         = FALSE
             Inversion_Sdos.Tasa_NomiAnual:SENSITIVE   = FALSE
             Inversion_Sdos.Plazo_Dias:SENSITIVE       = FALSE
             Inversion_Sdo.Fec_Prorroga:SENSITIVE      = FALSE
             Inversion_Sdos.LiqInt_Dias:SENSITIVE      = FALSE
             Tg_GMF:SENSITIVE                          = FALSE
             Tg_GMF:SCREEN-VALUE                       = "No"
             Tg_GMF                                    = FALSE
             Inversion_Sdos.Nro_Unidades:SENSITIVE     = FALSE
             Inversion_Sdos.Valor_Unidad:SENSITIVE     = FALSE.           

      IF W_Ope EQ "S" OR W_Ope EQ "M" THEN
         ASSIGN Inversion_Sdos.VrInvers_Inic:SENSITIVE    = TRUE
                Inversion_Sdos.Categoria:SENSITIVE        = TRUE
                Cmb_Pdctos:SENSITIVE                      = TRUE    
                Inversion_Sdos.Interes_Causado:SENSITIVE  = TRUE
                Inversion_Sdos.Interes_Recibido:SENSITIVE = TRUE
                Inversion_Sdos.Fec_Apertura:SENSITIVE     = TRUE
                Inversion_Sdos.Fec_Vcto:SENSITIVE         = TRUE
                Inversion_Sdos.Tasa_NomiAnual:SENSITIVE   = TRUE
                Inversion_Sdos.Plazo_Dias:SENSITIVE       = TRUE
                Inversion_Sdo.Fec_Prorroga:SENSITIVE      = TRUE
                Inversion_Sdos.LiqInt_Dias:SENSITIVE      = TRUE.

      IF W_Ope EQ "C" OR W_Ope EQ "V" THEN
         Tg_GMF:SENSITIVE = TRUE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HallaNroUnid W-InverS 
PROCEDURE HallaNroUnid :
/*------------------------------------------------------------------------------
  Purpose:     
  ------------------------------------------------------------------------------*/
  DEFI VAR W_Monto LIKE Tasas_Mercado.Monto_Final INIT 0.
  DEFI VAR W_Tasa  LIKE Tasas_Mercado.Tasa        INIT 0.
  DEFI VAR W_Nro   LIKE Inversion_Sdos.Nro_Unid   INIT 0.
  
  ASSIGN W_Monto = DEC(Inversion_Sdos.VrInvers_Inic:SCREEN-VALUE IN FRAME F_Invers).

  IF W_Ope EQ "I" OR W_Ope EQ "V" THEN
     W_Monto = DEC(W_CancParc:SCREEN-VALUE).
  ELSE IF W_Ope NE "A" AND W_Ope NE "S" THEN
     W_Monto = Inversion_Sdos.Sdo_Actual.
                                                 
  W_Nro = W_Monto / DEC(Inversion_Sdos.Valor_Unidad:SCREEN-VALUE).                                         
                                                                                           
  IF DEC(Inversion_Sdos.Nro_Unid:SCREEN-VALUE) NE W_Nro THEN DO:                           
     IF DEC(Inversion_Sdos.Nro_Unid:SCREEN-VALUE) LE 0.00 THEN                             
        ASSIGN Inversion_Sdos.Nro_Unid:SCREEN-VALUE = STRING(W_Nro).                       
     ELSE DO:                                                                              
        MESSAGE "El Nùmero de Unidades calculado es Diferente al Capturado..." SKIP        
                "                          Desea Actualizarlo..."              SKIP        
               VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirmar # Unidades"      
               UPDATE W_RptaUni AS LOGICAL.                                                
        IF W_RptaUni THEN                                                                  
           ASSIGN Inversion_Sdos.Nro_Unid:SCREEN-VALUE = STRING(W_Nro).                    
     END.                                                                                  
  END.                                                                                     

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HallaTasaMcdo W-InverS 
PROCEDURE HallaTasaMcdo :
/*------------------------------------------------------------------------------
  Purpose:     
------------------------------------------------------------------------------*/
  DEFI VAR W_Monto LIKE Tasas_Mercado.Monto_Final INIT 0.
  DEFI VAR W_Tasa  LIKE Tasas_Mercado.Tasa        INIT 0.
  DEFI VAR W_Nro   LIKE Inversion_Sdos.Nro_Unid   INIT 0.
  
  ASSIGN W_Monto = DEC(Inversion_Sdos.VrInvers_Inic:SCREEN-VALUE IN FRAME F_Invers)
         Inversion_Sdos.Tasa_NomiAMcdo:SCREEN-VALUE = "0.00".

  IF W_Ope EQ "I" THEN
     W_Monto = Inversion_Sdos.Sdo_Actual + DEC(W_CancParc:SCREEN-VALUE).
  ELSE IF W_Ope EQ "V" THEN
     W_Monto = Inversion_Sdos.Sdo_Actual - DEC(W_CancParc:SCREEN-VALUE).
  ELSE IF W_Ope NE "A" AND W_Ope NE "S" THEN
     W_Monto = Inversion_Sdos.Sdo_Actual.

  FIND FIRST Tasas_Mercado WHERE Tasas_Mercado.Nit_Entidad  EQ Bancos.Nit 
                             AND Tasas_Mercado.Pdcto        EQ 3
                             AND Tasas_Mercado.Tipo         EQ Pro_Inversiones.Categoria
                             AND Tasas_Mercado.Cod_Producto EQ Pro_Inversiones.Cod_Produc
                             AND Tasas_Mercado.Pla_Final    GE INTEG(Inversion_Sdos.Plazo:SCREEN-VALUE)    
                             AND Tasas_Mercado.Monto_Final  GE W_Monto                
                             AND Tasas_Mercado.Estado       EQ 1   NO-LOCK NO-ERROR.
      
  IF AVAIL(Tasas_Mercado) THEN DO:
     RUN TasaPerNom IN W_ManFin (INPUT 365,Tasas_Mercado.Tasa,1,OUTPUT W_Tasa).

     ASSIGN W_NomInd:SCREEN-VALUE                      = Tasas_Mercado.Nom_TasaUnid.
            Inversion_Sdos.Tasa_NomiAMcdo:SCREEN-VALUE = STRING(W_Tasa * 36500).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImpCheque W-InverS 
PROCEDURE ImpCheque :
/*------------------------------------------------------------------------------
     Invocado desde Proc.Contabilizar.
  Purpose:     Imprime cheque para aperturas.
  ------------------------------------------------------------------------------*/
   DEFINE VAR W_Cadena AS CHARACTER FORMAT "X(150)".
   DEFINE VAR W_Monto1 AS CHARACTER FORMAT "X(70)".
   DEFINE VAR W_Monto2 AS CHARACTER FORMAT "X(70)".
   DEFINE VAR W_Monto3 AS CHARACTER FORMAT "X(70)".
   DEFINE VAR W_Rpta   AS LOGICAL.

   RUN MontoEsc.r (INPUT Inversion_Sdos.VrInvers_Inic, INPUT 0, OUTPUT W_Cadena).
   RUN PartirValor IN W_Manija (INPUT W_Cadena,INPUT 70,OUTPUT W_Monto1,OUTPUT W_Monto2,OUTPUT W_Monto3).
   
   MESSAGE "Inserte cheque a imprimir." VIEW-AS ALERT-BOX. 
        
   RUN VALUE(W_CheProfor) 
            (W_Monto1, W_Monto2, Inversion_Sdos.Nombre_Emisor, W_Ciudad, Inversion_Sdos.VrInvers_Inic, 
             " ").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImpCpte W-InverS 
PROCEDURE ImpCpte :
/*------------------------------------------------------------------------------
  Purpose:     Impresión cpte contable.
  ------------------------------------------------------------------------------*/
   DEFINE VAR W_Rpta   AS LOGICAL.
   
   FIND Formatos WHERE Formatos.Agencia     EQ W_Agencia
                   AND Formatos.Cod_Formato EQ Comprobantes.Cod_formato 
               NO-LOCK NO-ERROR.

   IF AVAILABLE(Formatos) THEN
      RUN VALUE(Formatos.Nom_Proceso) (INPUT Comprobantes.comprobante,
                                       INPUT Comprobantes.Secuencia, INPUT Comprobantes.Secuencia,
                                       INPUT W_Agencia).
   ELSE
     RUN MostrarMensaje IN W_Manija (INPUT 345, OUTPUT W_Rpta).
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImpExtracto W-InverS 
PROCEDURE ImpExtracto :
/*------------------------------------------------------------------------------
  Purpose:   Informe extracto con todos los movtos de la inversión.
  ------------------------------------------------------------------------------*/
    DEFI VAR Fec1 AS CHAR FORM "X(10)" INIT "  /  /    ".
    DEFI VAR Fec2 AS CHAR FORM "X(10)" INIT "  /  /    ".
    
    IF NOT AVAIL(Inversion_Sdo) THEN DO:
       MESSAGE "Debe Seleccionar en el BROWSER de Consulta la Inversión...?" 
           VIEW-AS ALERT-BOX.
           
       RETURN.    
    END.
    
    IF Inversion_Sdo.Fec_Prorroga NE ? THEN
       Fec1 = STRING(Inversion_Sdo.Fec_Prorroga).
       
    IF Inversion_Sdo.Fec_Canc NE ? THEN
       Fec2 = STRING(Inversion_Sdo.Fec_Canc).

    {Incluido\RepEncabezado.I}

    ASSIGN W_Reporte = "Reporte   : EXTRACTO " + Inversion_Sdos.Descrip + " hasta : " +
           STRING(TODAY,"99/99/9999") + "     Hora :" + STRING(TIME,"HH:MM:SS").

    VIEW FRAME F-Encabezado.
    VIEW FRAME f-ftr.
       
    DISPLAY SKIP(0)
            "Nit Emisor : " +
            STRING(Inversion_Sdo.Nit_Emi,"X(12)") +
            " " +
            TRIM(Inversion_Sdo.Nombre_Emi,"X(25)") +
            "    Calific.Emisor-Inversión : " +
            STRING(Inversion_Sdos.Calif_Emisor,"X(3)") +
            " - " +
            STRING(Inversion_Sdos.Calif_Invers,"X(2)")      FORMAT "X(120)" SKIP
            "Nro.del Título : " +
            STRING(Inversion_Sdo.Nro_Titulo,"X(12)") + "-" + STRING(Inversion_Sdos.Secuencia) +
            "  Fecha-Apertura :   " +
            STRING(Inversion_Sdo.Fec_Apertura,"99/99/9999") +
            "                    Fecha-Prorroga : " +
            STRING(Fec1,"X(10)")                            FORMAT "X(120)" SKIP
            "Tasa Nominal-Anual: " +
            STRING(Inversion_Sdos.Tasa_NomiAnual,">>9.99") +
            "       Fecha-Vencimiento :" +
            STRING(Inversion_Sdo.Fec_Vcto,"99/99/9999") +
            "  Plazo Dias: " +
            STRING(Inversion_Sdos.Plazo_Dias,"9999") +
            "  Fec-Cancelación: " +
            STRING(Fec2,"X(10)")                            FORMAT "X(120)" SKIP
            "-----------------------------------------------------------------------------------------------------------"
       WITH DOWN WIDTH 130 FRAME F1 USE-TEXT NO-LABELS STREAM-IO NO-BOX.     
                 
    FOR EACH Mov_Inversion WHERE Mov_Inversion.Nit_Emi    EQ Inversion_Sdo.Nit_Emi
                             AND Mov_Inversion.Nro_Titulo EQ Inversion_Sdo.Nro_Titulo 
                             AND Mov_Inversion.Secuencia  EQ Inversion_Sdo.Secuencia NO-LOCK
             BY Mov_Inversion.Fecha BY Mov_Inversion.Hora:
        
        DISPLAY Mov_Inversion.Fecha        LABEL "Fecha" FORM "99/99/99"
                Mov_Inversion.Descrip      LABEL "Descrip.de la Novedad" FORMAT "X(25)"
                Mov_Inversion.Oper         LABEL "Op"
                Mov_Inversion.Comprobante  LABEL "Cte"
                Mov_Inversion.Num_Document LABEL "No.Docum" 
                Mov_Inversion.Tasa_NomiAMcdo LABEL "Tasa Mcdo"
                Mov_Inversion.Tasa_NomiAnual LABEL "Tasa Invers"
                Mov_Inversion.Vr_Consig      LABEL "Valor Consignado" FORMAT "->>>>>,>>>,>>9.99"
                Mov_Inversion.Vr_Retiro      LABEL "Valor Retiro"     FORMAT ">>>>>,>>>,>>9.99"
                Mov_Inversion.Sdo_Inversio   LABEL "Saldo Actual"     FORMAT ">>>>>,>>>,>>9.99"
                                             WHEN  Mov_Inversion.Sdo_Inversio NE 0
                Mov_Inversion.Nro_Unidad     LABEL "Nro.Unid."        FORMAT ">>>>>9.99"
                Mov_Inversion.Valor_Unidad   LABEL "Vr.Unidad"        FORMAT ">>>>>9.99"
                Mov_Inversion.Usuario        LABEL "Usu."
                SKIP(0)
           WITH DOWN WIDTH 220 FRAME F2 USE-TEXT NO-LABELS STREAM-IO NO-BOX.  
           
        IF Mov_Inversion.Oper EQ "P" THEN
           DISPLAY "            Tasa Anterior : " +
                   STRING(Mov_Inversion.Tasa_NomiAnual,">>9.99") +
                   "            Plazo Anterior : " +
                   STRING(Mov_Inversion.Plazo_Dias,"9999") FORM "X(130)"
                   SKIP(1)
               WITH DOWN WIDTH 130 FRAME F3 USE-TEXT NO-LABELS STREAM-IO NO-BOX. 
    END. 

    IF Inversion_Sdos.Nro_Unidad GT 0 THEN
       DISPLAY SKIP(1)
               "                                   Nùmero y Valor Unidades a Hoy ---->              "
               Inversion_Sdos.Nro_Unid   FORMAT ">>>>>9.99"
               " X "
               Inversion_Sdos.Valor_Unid FORMAT ">>>>>9.99"
               (Inversion_Sdos.Nro_Unid * Inversion_Sdos.Valor_Unid) FORMAT ">>>>>,>>>,>>9.99"
          WITH DOWN WIDTH 150 FRAME F4 USE-TEXT NO-LABELS STREAM-IO NO-BOX.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InfApert W-InverS 
PROCEDURE InfApert :
/*------------------------------------------------------------------------------
  Purpose:     Informe por Fec-Aperturas.
  ------------------------------------------------------------------------------*/
  ASSIGN W_Tot = 0.

  {Incluido\RepEncabezado.I}

  ASSIGN W_Reporte = "Reporte   : Aperturas de Inversiones     Fecha del Informe: " +
                     STRING(TODAY,"99/99/9999") + "     Hora :" + STRING(TIME,"HH:MM:SS")

         W_EncColumna = "                   Fechas Desde : " + STRING(W_FecIni,"99/99/9999") +
                        " Hasta : " +
                          STRING(W_FecFin,"99/99/9999").

  VIEW FRAME F-Encabezado.
  VIEW FRAME f-ftr.
  
  DISPLAY SKIP(0)
            "Nit Emisor   Nombre del Emisor         Nro.Título     SC F-Apertura. Fecha-Vcto. T.Nom-Año Plazo Vr.Inversión Inic  Saldo Inversión Interés Liquidado        Nro-Unid    Interès Futuro" SKIP
            "------------ ------------------------- -------------- -- ----------- ----------- --------- ----- ----------------- ---------------- ----------------- --------------- -----------------"
                   WITH DOWN WIDTH 260 FRAME F1 USE-TEXT NO-LABELS STREAM-IO NO-BOX. 
  
  FOR EACH Inversion_Sdos WHERE Inversion_Sdos.Fec_Apert GE W_FecIni
                           AND Inversion_Sdos.Fec_Apert  LE W_FecFin
                           AND Inversion_Sdo.Nit_Emi     GE W_NitI
                           AND Inversion_Sdo.Nit_Emi     LE W_NitF NO-LOCK
             BREAK BY Inversion_Sdo.Nit_Emi BY Inversion_Sdos.Secuencia
                   BY Inversion_Sdos.Fec_Apert:

      ASSIGN W_IntReal = (((Inversion_Sdos.Sdo_Actual * Inversion_Sdos.Tasa_NomiAnual) /
                              365) * Inversion_Sdo.Plazo) / 100
             W_Tot [1] = W_Tot [1] + Inversion_Sdos.Sdo_Actual
             W_Tot [2] = W_Tot [2] + Inversion_Sdos.Sdo_Actual
             W_Tot [3] = W_Tot [3] + Inversion_Sdos.Interes_Causado
             W_Tot [4] = W_Tot [4] + Inversion_Sdos.Interes_Causado
             W_Tot [5] = W_Tot [5] + (W_IntReal - Inversion_Sdos.Interes_Causado)
             W_Tot [6] = W_Tot [6] + (W_IntReal - Inversion_Sdos.Interes_Causado).

      DISPLAY Inversion_Sdo.Nit_Emi                    
              TRIM(Inversion_Sdo.Nombre_Emi)             FORM "X(25)"
              Inversion_Sdo.Nro_Titulo                 
              Inversion_Sdo.Secuen                     
              Inversion_Sdo.Fec_Apertura               
              Inversion_Sdo.Fec_Vcto                   
              Inversion_Sdos.Tasa_NomiAnual              FORM ">>9.99"
              "      "
              Inversion_Sdos.Plazo                     
              Inversion_Sdos.VrInvers_Inic               FORMAT ">>>>>,>>>,>>9.99"
              Inversion_Sdos.Sdo_Actual                  FORMAT ">>>>>,>>>,>>9.99"
              Inversion_Sdos.Interes_Causado             FORMAT "->>>>>,>>>,>>9.99"
              Inversion_Sdos.Nro_Unidades              
              W_IntReal - Inversion_Sdos.Interes_Causado FORMAT "->>>>>,>>>,>>9.99"
              SKIP (0)      
        WITH DOWN WIDTH 220 FRAME F2 USE-TEXT NO-LABELS STREAM-IO NO-BOX. 

      IF LAST-OF(Inversion_Sdo.Nit_Emi) THEN DO:
         RUN TotXEnt.

         ASSIGN W_Tot [1] = 0
                W_Tot [3] = 0
                W_Tot [5] = 0.                
      END.
  END.

  DISPLAY SKIP(2)
          "                                                       TOTAL GENERAL----->                                       "
          W_Tot [2]          FORMAT "->>>>>>,>>>,>>9.99"  
          W_Tot [4]          FORMAT "->>>>>,>>>,>>9.99"  
          "              " 
          W_Tot [6]          FORMAT "->>>>>>,>>>,>>9.99"  
          SKIP(1)
      WITH DOWN WIDTH 250 FRAME Ftot333 USE-TEXT NO-LABELS STREAM-IO NO-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InfCanc W-InverS 
PROCEDURE InfCanc :
/*------------------------------------------------------------------------------
  Purpose:     Informe por Fec-Cancelación.
  ------------------------------------------------------------------------------*/
  {Incluido\RepEncabezado.I}
  
  ASSIGN W_Reporte = "Reporte   : Inversiones Canceladas     Fecha del Informe: " +
                     STRING(TODAY,"99/99/9999") + "     Hora :" + STRING(TIME,"HH:MM:SS")
         
         W_EncColumna = "                   Fechas Desde : " + STRING(W_FecIni,"99/99/9999") +
                        " Hasta : " + STRING(W_FecFin,"99/99/9999").

  VIEW FRAME F-Encabezado.
  VIEW FRAME f-ftr.
  
  FOR EACH Inversion_Sdo WHERE Inversion_Sdo.Estado  NE 0
                           AND Inversion_Sdo.Nit_Emi GE W_NitI
                           AND Inversion_Sdo.Nit_Emi LE W_NitF NO-LOCK
             BY Inversion_Sdo.Fec_Canc BY Inversion_Sdo.Nit_Emi
             BY Inversion_Sdo.Secuencia:
      DISPLAY Inversion_Sdo.Nit_Emi                    LABEL "Nit Emisor"
              TRIM(Inversion_Sdo.Nombre_Emi)           LABEL "Nombre del Emisor"  FORM "X(25)"
              Inversion_Sdo.Nro_Titulo                 LABEL "Nro.Título"
              Inversion_Sdo.Secuen                     LABEL "SC" 
              Inversion_Sdo.Fec_Apertura               LABEL "F-Apertura."
              Inversion_Sdo.Fec_Vcto                   LABEL "Fecha-Vcto."
              Inversion_Sdos.Tasa_NomiAnual            LABEL "T.Nom-Año"       FORM ">>9.99"
              Inversion_Sdos.Plazo                     LABEL "Plazo"
              Inversion_Sdos.VrInvers_Inic             LABEL "Vr.Inversión Inic" FORMAT ">>>>>,>>>,>>9.99"
              Inversion_Sdos.Interes_Causado           LABEL "Interés Causado" FORMAT "->>>>>,>>>,>>9.99"
              Inversion_Sdos.Fec_Canc                  LABEL "Fec-Cancel"
              SKIP (0)      
        WITH DOWN WIDTH 180 FRAME F2 USE-TEXT NO-LABELS STREAM-IO NO-BOX. 
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InfPartic W-InverS 
PROCEDURE InfPartic :
/*------------------------------------------------------------------------------
  Purpose:   Inf.Participaciones X Entidades.  
  ------------------------------------------------------------------------------*/
  DEFI VAR W_TotT LIKE Inversion_Sdos.Interes_Causado INIT 0.
  DEFI VAR W_Tot  LIKE Inversion_Sdos.Interes_Causado INIT 0.

  {Incluido\RepEncabezado.I}
  
  ASSIGN W_Reporte = "Reporte   : Participaciones de Inversiones     Fecha del Informe: " +
            STRING(TODAY,"99/99/9999") + "     Hora :" + STRING(TIME,"HH:MM:SS").

  VIEW FRAME F-Encabezado.
  VIEW FRAME f-ftr.

  FOR EACH Inversion_Sdos WHERE Inversion_Sdos.Estado EQ 0 NO-LOCK:
      ASSIGN W_TotT = W_TotT + Inversion_Sdos.Sdo_Actual.
  END.

  FOR EACH Inversion_Sdos WHERE Inversion_Sdos.Estado EQ 0 NO-LOCK
                          BREAK BY Inversion_Sdos.Nit_Emisor:
      ASSIGN W_Tot = W_Tot + Inversion_Sdos.Sdo_Actual.

      IF LAST-OF(Inversion_Sdos.Nit_Emisor) THEN DO:
         FIND FIRST Bancos WHERE Bancos.Nit EQ Inversion_Sdos.Nit_Emisor NO-LOCK NO-ERROR.

         DISPLAY Inversion_Sdos.Nit_Emisor    LABEL "Nit-Emisor"
                 Inversion_Sdos.Nombre_Emisor LABEL "Nombre de la Entidad"
                 W_Tot                        LABEL "Tot.Inversiones"
                 (W_Tot / W_TotT) * 100       LABEL "%Particip" FORMAT "->>>9.999"
                 Bancos.Porc_Partic           LABEL "%Max.Ent." FORMAT "->>>9.999"
              WITH DOWN WIDTH 140 FRAME F2 USE-TEXT NO-LABELS STREAM-IO NO-BOX.  

         W_Tot = 0.
      END.
  END.

  DISPLAY SKIP(1)
          "                                                          ----------------" SKIP
          "                          Total de Inversiones ----> "  
          W_TotT
    WITH DOWN WIDTH 140 FRAME F3 USE-TEXT NO-LABELS STREAM-IO NO-BOX.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InfVctos W-InverS 
PROCEDURE InfVctos :
/*------------------------------------------------------------------------------
  Purpose:     Informe por vencimientos.
  ------------------------------------------------------------------------------*/
  DEFI VAR W_IntReal LIKE Inversion_Sdos.Interes_Causado INIT 0.

  {Incluido\RepEncabezado.I}
  
  ASSIGN W_Reporte = "Reporte   : Vencimientos de Inversiones     Fecha del Informe: " +
                     STRING(TODAY,"99/99/9999") + "     Hora :" + STRING(TIME,"HH:MM:SS")
         
         W_EncColumna = "                   Fechas Desde : " + STRING(W_FecIni,"99/99/9999") +
                        " Hasta : " + STRING(W_FecFin,"99/99/9999")

         W_Tot = 0.

  VIEW FRAME F-Encabezado.
  VIEW FRAME f-ftr.

  DISPLAY SKIP(0)
            "Nit Emisor   Nombre del Emisor         Nro.Título     SC F-Apertura. Fecha-Vcto. T.Nom-Año Plazo Vr.Inversión Inic  Saldo Inversión Interés Liquidado        Nro-Unid    Interès Futuro" SKIP
            "------------ ------------------------- -------------- -- ----------- ----------- --------- ----- ----------------- ---------------- ----------------- --------------- -----------------"
       WITH DOWN WIDTH 260 FRAME F1 USE-TEXT NO-LABELS STREAM-IO NO-BOX. 
  
  FOR EACH Inversion_Sdo WHERE Inversion_Sdo.Estado   EQ 0
                           AND Inversion_Sdo.Fec_Vcto GE W_FecIni
                           AND Inversion_Sdo.Fec_Vcto LE W_FecFin
                           AND Inversion_Sdo.Nit_Emi  GE W_NitI
                           AND Inversion_Sdo.Nit_Emi  LE W_NitF NO-LOCK
           BREAK BY Inversion_Sdo.Nit_Emi
                 BY Inversion_Sdos.Secuencia BY Inversion_Sdo.Fec_Vcto:
      
      ASSIGN W_IntReal = (((Inversion_Sdos.Sdo_Actual * Inversion_Sdos.Tasa_NomiAnual) /
                              365) * Inversion_Sdo.Plazo) / 100
             W_Tot [1] = W_Tot [1] + Inversion_Sdos.Sdo_Actual
             W_Tot [2] = W_Tot [2] + Inversion_Sdos.Sdo_Actual
             W_Tot [3] = W_Tot [3] + Inversion_Sdos.Interes_Causado
             W_Tot [4] = W_Tot [4] + Inversion_Sdos.Interes_Causado
             W_Tot [5] = W_Tot [5] + (W_IntReal - Inversion_Sdos.Interes_Causado)
             W_Tot [6] = W_Tot [6] + (W_IntReal - Inversion_Sdos.Interes_Causado).

      DISPLAY Inversion_Sdo.Nit_Emi                    
              TRIM(Inversion_Sdo.Nombre_Emi)           FORM "X(25)"
              Inversion_Sdo.Nro_Titulo                 
              Inversion_Sdo.Secuen                     
              Inversion_Sdo.Fec_Apertura               
              Inversion_Sdo.Fec_Vcto                   
              Inversion_Sdos.Tasa_NomiAnual            FORM ">>9.99"
              "      "
              Inversion_Sdos.Plazo                     
              Inversion_Sdos.VrInvers_Inic             FORMAT ">>>>>,>>>,>>9.99"
              Inversion_Sdos.Sdo_Actual                FORMAT ">>>>>,>>>,>>9.99"
              Inversion_Sdos.Interes_Causado           FORMAT "->>>>>,>>>,>>9.99"
              Inversion_Sdos.Nro_Unidades              
              W_IntReal - Inversion_Sdos.Interes_Causa FORMAT "->>>>>,>>>,>>9.99"
              SKIP (0)      
        WITH DOWN WIDTH 220 FRAME F2 USE-TEXT NO-LABELS STREAM-IO NO-BOX.

      IF LAST-OF(Inversion_Sdo.Nit_Emi) THEN DO:
         RUN TotXEnt.

         ASSIGN W_Tot [1] = 0
                W_Tot [3] = 0
                W_Tot [5] = 0.                
      END.      
  END.

  DISPLAY SKIP(2)
          "                                                      TOTAL GENERAL----->                                       "
          W_Tot [2]          FORMAT "->>>>>>,>>>,>>9.99"  
          W_Tot [4]          FORMAT "->>>>>,>>>,>>9.99"  
          "              " 
          W_Tot [6]          FORMAT "->>>>>>,>>>,>>9.99"  
          SKIP(1)
      WITH DOWN WIDTH 250 FRAME Ftot333 USE-TEXT NO-LABELS STREAM-IO NO-BOX.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InfXPdctos W-InverS 
PROCEDURE InfXPdctos :
/*------------------------------------------------------------------------------
  Purpose:   Resumen X Tipo y clase de Inversiones.  
 ------------------------------------------------------------------------------*/
  DEFI VAR NomPro AS CHAR FORM "X(12)".

  {Incluido\RepEncabezado.I}
  
  ASSIGN W_Reporte = "Reporte   : Resumen X Pdctos de Inversiones     Fecha del Informe: " +
                      STRING(TODAY,"99/99/9999")  + "     Hora :" + STRING(TIME,"HH:MM:SS")
         
         W_EncColumna = "Pdcto   Nombre del Producto         Saldo Inversiones    Intereses Liquidados      Intereses Futuros"
      
         W_Tot = 0.

  VIEW FRAME F-Encabezado.
  VIEW FRAME f-ftr.

  FOR EACH Inversion_Sdo WHERE Inversion_Sdo.Estado EQ 0  NO-LOCK
           BREAK BY Inversion_Sdo.Categoria BY Inversion_Sdos.Cod_Produc:
      
      IF FIRST-OF(Inversion_Sdo.Categoria) THEN DO:
         IF Inversion_Sdo.Categoria EQ 1 THEN
            NomPro = "Negociables".
         ELSE IF Inversion_Sdo.Categoria EQ 2 THEN
            NomPro = "Permanentes".
         ELSE IF Inversion_Sdo.Categoria EQ 3 THEN
            NomPro = "Disponibles".

         DISPLAY "Tipo de Inversiòn : "
                 NomPro  
                 SKIP(1)
             WITH DOWN WIDTH 100 FRAME FT11 USE-TEXT NO-LABELS STREAM-IO NO-BOX.
      END.

      ASSIGN W_IntReal = (((Inversion_Sdos.Sdo_Actual * Inversion_Sdos.Tasa_NomiAnual) /
                              365) * Inversion_Sdo.Plazo) / 100
             W_Tot [1] = W_Tot [1] + Inversion_Sdos.Sdo_Actual
             W_Tot [2] = W_Tot [2] + Inversion_Sdos.Sdo_Actual
             W_Tot [7] = W_Tot [7] + Inversion_Sdos.Sdo_Actual
             W_Tot [3] = W_Tot [3] + Inversion_Sdos.Interes_Causado
             W_Tot [4] = W_Tot [4] + Inversion_Sdos.Interes_Causado
             W_Tot [8] = W_Tot [8] + Inversion_Sdos.Interes_Causado
             W_Tot [5] = W_Tot [5] + (W_IntReal - Inversion_Sdos.Interes_Causado)
             W_Tot [6] = W_Tot [6] + (W_IntReal - Inversion_Sdos.Interes_Causado)
             W_Tot [9] = W_Tot [9] + (W_IntReal - Inversion_Sdos.Interes_Causado). 


      IF LAST-OF(Inversion_Sdo.Cod_Prod) THEN DO:
         FIND FIRST Pro_Inversiones WHERE Pro_Inversiones.Cod_prod EQ Inversion_Sdos.Cod_Produc
                                      /*AND Pro_Inversiones.Estado   EQ 1*/ NO-LOCK NO-ERROR.
         DISPLAY Inversion_Sdo.Cod_Prod
                 Pro_Inversiones.Nom_Prod      FORM "X(31)" WHEN AVAIL(Pro_Inversiones)
                 W_Tot [1]                     FORMAT "->>>>>>,>>>,>>9.99"
                 "  "
                 W_Tot [3]                     FORMAT "->>>>>>,>>>,>>9.99"
                 "    "
                 W_Tot [5]                     FORMAT "->>>>>>,>>>,>>9.99" SKIP(0)
             WITH DOWN WIDTH 130 FRAME FTot11 USE-TEXT NO-LABELS STREAM-IO NO-BOX.

         ASSIGN W_Tot [1] = 0
                W_Tot [3] = 0
                W_Tot [5] = 0.  
      END.

      IF LAST-OF(Inversion_Sdos.Categoria) THEN DO:
         DISPLAY SKIP (1)
                 "Total Tipo "
                 NomPro                        
                 "          "
                 W_Tot [2]                     FORMAT "->>>>>>,>>>,>>9.99"
                 "  "
                 W_Tot [4]                     FORMAT "->>>>>>,>>>,>>9.99"
                 "    "
                 W_Tot [6]                     FORMAT "->>>>>>,>>>,>>9.99" SKIP(2)
             WITH DOWN WIDTH 130 FRAME FTotTip USE-TEXT NO-LABELS STREAM-IO NO-BOX.

         ASSIGN W_Tot [2] = 0
                W_Tot [4] = 0
                W_Tot [6] = 0.  
      END.
  END.

  DISPLAY SKIP (2)
                 "Total General de Inversiones ----->"
                 W_Tot [7]                     FORMAT "->>>>>>,>>>,>>9.99"
                 "  "
                 W_Tot [8]                     FORMAT "->>>>>>,>>>,>>9.99"
                 "    "
                 W_Tot [9]                     FORMAT "->>>>>>,>>>,>>9.99"
             WITH DOWN WIDTH 130 FRAME FTotGral USE-TEXT NO-LABELS STREAM-IO NO-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-Initialize W-InverS 
PROCEDURE Local-Initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'Initialize':U ) .
  
  
  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Movtos W-InverS 
PROCEDURE Movtos :
/*------------------------------------------------------------------------------
    Invocado desde Btn_Salva.
  Purpose: Graba Movimiento en la Tabla Mov_Inversion.  
 ------------------------------------------------------------------------------*/
    IF W_RetFte GT 0 THEN DO:
       CREATE Mov_Inversion.                                     
       ASSIGN Mov_Inversion.Agencia = Inversion_Sdos.Agencia        
          Mov_Inversion.Ofi_Origen = W_Agencia                      
          Mov_Inversion.Nit        = Inversion_Sdos.Nit_Emisor      
          Mov_Inversion.Nro_Titulo = Inversion_Sdos.Nro_Titulo      
          Mov_Inversion.Secuencia  = Inversion_Sdos.Secuencia       
          Mov_Inversion.Categoria  = Inversion_Sdos.Categoria       
          Mov_Inversion.Cod_Produc = Inversion_Sdos.Cod_Producto    
          Mov_Inversion.Fecha      = TODAY                          
          Mov_Inversion.Hora       = TIME                           
          Mov_Inversion.Usuario    = W_Usuario                      
          Mov_Inversion.Descrip    = "Retención en la Fuente"       
          Mov_Inversion.Oper       = W_Ope                          
          Mov_Inversion.Comprobant = Comprobantes.Comprobante       
          Mov_Inversion.Num_Docume = Comprobantes.Secuencia         
          Mov_Inversion.Vr_Consig  = W_RetFte.                      
    END.

    IF W_RetGMF GT 0 THEN DO:
       CREATE Mov_Inversion.                                     
       ASSIGN Mov_Inversion.Agencia = Inversion_Sdos.Agencia        
          Mov_Inversion.Ofi_Origen = W_Agencia                      
          Mov_Inversion.Nit        = Inversion_Sdos.Nit_Emisor      
          Mov_Inversion.Nro_Titulo = Inversion_Sdos.Nro_Titulo      
          Mov_Inversion.Secuencia  = Inversion_Sdos.Secuencia       
          Mov_Inversion.Categoria  = Inversion_Sdos.Categoria       
          Mov_Inversion.Cod_Produc = Inversion_Sdos.Cod_Producto    
          Mov_Inversion.Fecha      = TODAY                          
          Mov_Inversion.Hora       = TIME                           
          Mov_Inversion.Usuario    = W_Usuario                      
          Mov_Inversion.Descrip    = "Retención GMF"       
          Mov_Inversion.Oper       = W_Ope                          
          Mov_Inversion.Comprobant = Comprobantes.Comprobante       
          Mov_Inversion.Num_Docume = Comprobantes.Secuencia         
          Mov_Inversion.Vr_Consig  = W_RetGMF.                      
    END.
 
    CREATE Mov_Inversion.
    ASSIGN Mov_Inversion.Agencia   = Inversion_Sdo.Agencia
          Mov_Inversion.Ofi_Origen = W_Agencia
          Mov_Inversion.Nit        = Inversion_Sdos.Nit_Emisor
          Mov_Inversion.Nro_Titulo = Inversion_Sdos.Nro_Titulo
          Mov_Inversion.Secuencia  = Inversion_Sdos.Secuencia
          Mov_Inversion.Categoria  = Inversion_Sdos.Categoria
          Mov_Inversion.Cod_Produc = Inversion_Sdos.Cod_Producto
          Mov_Inversion.Fecha      = TODAY
          Mov_Inversion.Hora       = TIME
          Mov_Inversion.Usuario    = W_Usuario
          Mov_Inversion.Descrip    = W_Descrip
          Mov_Inversion.Oper       = W_Ope
          Mov_Inversion.Comprobant = Comprobantes.Comprobante
          Mov_Inversion.Num_Docume = Comprobantes.Secuencia.

    IF Pro_Inversiones.Id_TasaUnid EQ 1 THEN
       ASSIGN Mov_Inversion.Nro_Unid   = DEC(Inversion_Sdos.Nro_Unid:SCREEN-VALUE IN FRAME F_Invers)
              Mov_Inversion.Valor_Unid = DEC(Inversion_Sdos.Valor_Unid:SCREEN-VALUE).
    ELSE ASSIGN Mov_Inversion.Tasa_NomiAMcdo = Inversion_Sdos.Tasa_NomiAMcdo
                Mov_Inversion.Tasa_NomiAnual = Inversion_Sdos.Tasa_NomiAnual.
          
    IF W_Ope EQ "R" THEN
       ASSIGN Mov_Inversion.Vr_Consig = W_IntPag.
    ELSE IF W_Ope EQ "P" THEN
       ASSIGN Mov_Inversion.Plazo_Dias     = W_Plazo
              Mov_Inversion.Tasa_NomiAnual = W_Tasa.
    ELSE IF W_Ope EQ "I" THEN
       ASSIGN Mov_Inversion.Vr_Consig     = W_CancParc
              Mov_Inversion.Sdo_Inversion = Inversion_Sdos.Sdo_Actual + W_CancParc.
    ELSE IF W_Ope EQ "V" THEN
       ASSIGN Mov_Inversion.Vr_Retiro     = W_CancParc
              Mov_Inversion.Sdo_Inversion = Inversion_Sdos.Sdo_Actual - W_CancParc.

    IF W_Ope EQ "A" OR W_Ope EQ "S" THEN
       ASSIGN Mov_Inversion.Vr_Consig     = Inversion_Sdos.VrInvers_Inic
              Mov_Inversion.Sdo_Inversion = Inversion_Sdos.VrInvers_Inic. 
    ELSE IF W_Ope EQ "C" THEN DO:
       ASSIGN Mov_Inversion.Vr_Retiro     = Inversion_Sdos.Sdo_Actual
              Mov_Inversion.Sdo_Inversion = 0.

       IF W_IntReal GT 0 THEN DO:
          CREATE Mov_Inversion.
          ASSIGN Mov_Inversion.Agencia = Inversion_Sdo.Agencia
                 Mov_Inversion.Ofi_Origen = W_Agencia                    
                 Mov_Inversion.Nit        = Inversion_Sdos.Nit_Emisor    
                 Mov_Inversion.Nro_Titulo = Inversion_Sdos.Nro_Titulo    
                 Mov_Inversion.Secuencia  = Inversion_Sdos.Secuencia     
                 Mov_Inversion.Categoria  = Inversion_Sdos.Categoria     
                 Mov_Inversion.Cod_Produc = Inversion_Sdos.Cod_Producto  
                 Mov_Inversion.Fecha      = TODAY                        
                 Mov_Inversion.Hora       = TIME                         
                 Mov_Inversion.Usuario    = W_Usuario                    
                 Mov_Inversion.Descrip    = "Tot.Intereses Pagados"      
                 Mov_Inversion.Oper       = W_Ope                        
                 Mov_Inversion.Comprobant = Comprobantes.Comprobante     
                 Mov_Inversion.Num_Docume = Comprobantes.Secuencia       
                 Mov_Inversion.Vr_Consig  = W_IntReal. 

          IF Pro_Inversiones.Id_TasaUnid EQ 1 THEN 
             ASSIGN Mov_Inversion.Vr_Consig = W_IntReal - (Inversion_Sdos.Sdo_Actual + Inversion_Sdos.Interes_Causado)
                    Mov_Inversion.Descrip   = "Tot.Ganancia/Pèrdida".
       END.
    END.
 
 END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ParticYTope W-InverS 
PROCEDURE ParticYTope :
/*------------------------------------------------------------------------------
  Purpose:     Valida el % de participaciòn desde Bancos y el tope desde entidad.
  ------------------------------------------------------------------------------*/
  DEFI VAR TotInv   LIKE Inversion_Sdos.VrInvers_Inic INIT 0.
  DEFI VAR TTotInv  LIKE Inversion_Sdos.VrInvers_Inic INIT 0.

  IF W_Ope EQ "A" OR W_Ope EQ "S" THEN
     ASSIGN TotInv  = DEC(Inversion_Sdos.VrInvers_Inic:SCREEN-VALUE IN FRAME F_Invers)
            TTotInv = TotInv.
  ELSE ASSIGN TotInv  = W_CancParc
              TTotInv = TotInv.

  FOR EACH Inversion_Sdos WHERE Inversion_Sdos.Estado EQ 0 NO-LOCK:
      IF Inversion_Sdos.Nit_Emi EQ Inversion_Sdos.Nit_Emi:SCREEN-VALUE THEN
         ASSIGN TotInv  = TotInv + Inversion_Sdos.Sdo_Actual.

      ASSIGN TTotInv = TTotInv + Inversion_Sdos.Sdo_Actual.
  END.

  FIND FIRST Entidad  WHERE Entidad.Estado        EQ 1 NO-LOCK NO-ERROR.
  FIND FIRST Bancos   WHERE Bancos.Nit            EQ Inversion_Sdos.Nit_Emi:SCREEN-VALUE NO-LOCK NO-ERROR.

  IF TTotInv GE Entidad.ValMax_Inversiones THEN
     MESSAGE "El Total de las Inversiones actuales + Vr.Esta Apertura-Incremento," SKIP
             "                   Llegan al Tope Permitido....Revise por favor."
             VIEW-AS ALERT-BOX TITLE "INFORMATIVO TOPE DE INVERSIONES".

  IF (TotInv / TTotInv) * 100 GE Bancos.Porc_Partic THEN
     MESSAGE "En La Entidad Emisora las Inversiones Actuales + Vr.Esta Apertura-Incremento," SKIP
             "       Llegan al Tope de participaciòn Permitido....Revise por favor."
             VIEW-AS ALERT-BOX TITLE "INFORMATIVO TOPE DE PARTICIPACION".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir W-InverS 
PROCEDURE ProcesoImprimir :
/*------------------------------------------------------------------------------
  Purpose:     
-----------------------------------------------------------------------------*/
  ASSIGN W_NitI = W_NitEnt
         W_NitF = W_NitEnt.

  IF W_NitEnt LE "0" THEN
     ASSIGN W_NitI = "0"
            W_NitF = "99999999999".

  IF Rs_Inf EQ 1 THEN
     RUN ImpExtracto.
  ELSE IF Rs_Inf EQ 2 THEN
     RUN InfVctos.
  ELSE IF Rs_Inf EQ 3 THEN
     RUN InfApert.
  ELSE IF Rs_Inf EQ 4 THEN
     RUN InfCanc.
  ELSE IF Rs_Inf EQ 5 THEN
     RUN InfPartic.
  ELSE 
     RUN InfXPdctos.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcMainBlock W-InverS 
PROCEDURE ProcMainBlock :
/*------------------------------------------------------------------------------
     Invocado desde Main-Block
  Purpose: Inicio del programa.  
------------------------------------------------------------------------------*/
  ASSIGN Inversion_Sdo.Fec_Prorroga:SENSITIVE IN FRAME F_Invers  = FALSE
         Inversion_Sdo.Cod_Producto:SENSITIVE                    = FALSE
         Inversion_Sdo.Cuenta_Contab:SENSITIVE                   = FALSE
         Inversion_Sdos.Interes_Recibido:SENSITIVE               = FALSE
         Inversion_Sdos.Interes_Causado:SENSITIVE                = FALSE
         W_IntPag:SENSITIVE                                      = FALSE
         W_CancParc:SENSITIVE                                    = FALSE.
          

  FOR EACH Pro_Inversiones /*WHERE Pro_Inversiones.Estado EQ 0*/ NO-LOCK:
      Cmb_Pdctos:ADD-LAST(STRING(Pro_Inversiones.Cod_Produc,"999") + 
                        "-" + STRING(Pro_Inversiones.Nom_Prod,"X(35)")).      
  END.

  FRAME F_Cons:VISIBLE = FALSE.

  RELEASE Inversion_Sdo.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcSalva W-InverS 
PROCEDURE ProcSalva :
/*------------------------------------------------------------------------------
  Purpose: Transacciones contables.
 ------------------------------------------------------------------------------*/
  DEFI VAR W_Cpte LIKE Comprobantes.Comprobante.
  
DO TRANSACTION: /*Inicio Tx*/

  ASSIGN W_Cpte = Pro_Inversiones.Cpte_Tras.
  
  IF W_Ope EQ "A" OR W_Ope EQ "I" THEN
     W_Cpte = Pro_Inversiones.Cpte_Egr.                /*Egresos*/
  ELSE IF W_Ope EQ "C" OR W_Ope EQ "R" OR W_Ope EQ "V" THEN
     W_Cpte = Pro_Inversiones.Cpte_Ingr.               /*Ingresos*/
   
  FIND Comprobantes WHERE Comprobantes.Agencia EQ W_Agencia
                      AND Comprobantes.Comprob EQ W_Cpte
                      AND Comprobantes.Estado  EQ 1 NO-ERROR.
  IF NOT AVAIL(Comprobantes) THEN DO:
     MESSAGE "El Comprobante-Fuente Contable : " W_Cpte " debe existir activo en esta oficina." SKIP
             "                          No se acepta la Operación." VIEW-AS ALERT-BOX.
     RETURN ERROR.
  END.
      
  ASSIGN Comprobantes.Secuencia = Comprobantes.Secuencia + 1.
                       
  FIND CURRENT Comprobantes NO-LOCK NO-ERROR.

  DO WITH FRAME F_Invers: END.
      
      IF NOT AVAIL(Inversion_Sdos) THEN
         CREATE Inversion_Sdos.
              
      ASSIGN Inversion_Sdos.Agencia                Inversion_Sdos.Nit_Emi    
             Inversion_Sdos.Nro_Titulo             Inversion_Sdos.Secuencia 
             Inversion_Sdos.Categoria              Inversion_Sdos.Cod_Producto 
             Inversion_Sdos.Interes_Causado        Inversion_Sdos.Interes_Recibido
             Inversion_Sdos.Nombre_Emisor          Inversion_Sdos.Fec_Apertura 
             Inversion_Sdos.Cuenta_Contab          Inversion_Sdos.VrInvers_Inic 
             Inversion_Sdos.Descrip                Inversion_Sdos.Fec_Vcto 
             Inversion_Sdos.Tasa_NomiAnual         Inversion_Sdos.Plazo_Dias 
             Inversion_Sdos.Calif_Invers           Inversion_Sdos.Calif_Emisor 
             Inversion_Sdo.Fec_Prorroga            Inversion_Sdos.LiqInt_Dias
             Inversion_Sdos.TipTasa_FV             Inversion_Sdos.Indicador 
             Inversion_Sdos.Puntos_Adic            
             Inversion_Sdos.Valor_Unidad           Inversion_Sdos.Tasa_NomiAMcdo 
             W_RowId = ROWID(Inversion_Sdos).

      IF Pro_Inversiones.Id_TasaUnid EQ 0 THEN
         ASSIGN Inversion_Sdos.Nro_Unidades = 0
                Inversion_Sdos.Valor_Unidad = 0.
      ELSE DO:
         ASSIGN Inversion_Sdos.Tasa_NomiAnual = 0.

         IF W_Ope EQ "I" THEN
            Inversion_Sdos.Nro_Unidades = Inversion_Sdos.Nro_Unidades + 
                                          DEC(Inversion_Sdos.Nro_Unidades:SCREEN-VALUE).
         ELSE IF W_Ope EQ "V" THEN
            Inversion_Sdos.Nro_Unidades = Inversion_Sdos.Nro_Unidades - 
                                          DEC(Inversion_Sdos.Nro_Unidades:SCREEN-VALUE).
         ELSE Inversion_Sdos.Nro_Unidades = DEC(Inversion_Sdos.Nro_Unidades:SCREEN-VALUE).
      END.

      IF W_Ope EQ "S" THEN
         ASSIGN Inversion_Sdos.Sdo_Actual = Inversion_Sdos.VrInvers_Inic.

      RUN Movtos.
    
      IF W_Ope EQ "A" OR W_Ope EQ "C" OR W_Ope EQ "I" OR W_Ope EQ "R" OR W_Ope EQ "V" THEN DO:  
         RUN ContabTx.
          
         IF W_Ope EQ "C" THEN
            ASSIGN Inversion_Sdos.Estado_Inver = 1
                   Inversion_Sdos.Fec_Canc     = TODAY
                   Inversion_Sdos.Sdo_Actual   = 0.
         ELSE IF W_Ope EQ "R" THEN
            ASSIGN Inversion_Sdos.Interes_Recibido = Inversion_Sdos.Interes_Recibido + W_IntPag
                   Inversion_Sdos.Interes_Causado  = Inversion_Sdos.Interes_Causado  - W_IntPag.
         ELSE IF W_Ope EQ "A" THEN
            ASSIGN Inversion_Sdos.Sdo_Actual = Inversion_Sdos.VrInvers_Inic.
         ELSE IF W_Ope EQ "I" THEN DO:
            ASSIGN Inversion_Sdos.Sdo_Actual = Inversion_Sdos.Sdo_Actual + W_CancParc.

            IF TODAY EQ Inversion_Sdos.Fec_Apertura THEN
               ASSIGN Inversion_Sdos.VrInvers_Inic = Inversion_Sdos.VrInvers_Inic + W_CancParc.
         END.
         ELSE IF W_Ope EQ "V" THEN
            ASSIGN Inversion_Sdos.Sdo_Actual = Inversion_Sdos.Sdo_Actual - W_CancParc.

         ASSIGN Inversion_Sdos.Sdo_Actual:SCREEN-VALUE = STRING(Inversion_Sdos.Sdo_Actual).

      END.
      
      FIND CURRENT Inversion_Sdos NO-LOCK NO-ERROR.

      ASSIGN W_OpeC:SCREEN-VALUE = " "
             W_Ope               = " ".

      VIEW FRAME F_Cons.

      CLOSE QUERY Br_Inv.

      OPEN QUERY Br_Inv FOR EACH Inversion_Sdos WHERE ROWID(Inversion_Sdos) EQ W_RowId NO-LOCK.        

      RUN HabilVtna.    

      ASSIGN W_Debe1:SCREEN-VALUE IN FRAME F_Invers  = "0.00"
             W_Debe2:SCREEN-VALUE   = "0.00" 
             W_Debe3:SCREEN-VALUE   = "0.00" 
             W_Debe4:SCREEN-VALUE   = "0.00" 
             W_Haber1:SCREEN-VALUE  = "0.00" 
             W_Haber2:SCREEN-VALUE  = "0.00" 
             W_Haber3:SCREEN-VALUE  = "0.00" 
             W_Haber4:SCREEN-VALUE  = "0.00" 
             W_IntFut               = (((Inversion_Sdos.Sdo_Actual * Inversion_Sdos.Tasa_NomiAnual) /
                                      365) * Inversion_Sdo.Plazo) / 100
             W_IntFuturo:SCREEN-VALUE  = STRING (W_IntFut - Inversion_Sdos.Interes_Causado).

      
 END.  /*Fin Tx*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TotXEnt W-InverS 
PROCEDURE TotXEnt :
/*------------------------------------------------------------------------------
  Purpose:     
------------------------------------------------------------------------------*/
  DISPLAY "                                                      TOTAL ENTIDAD----->                                       "
          W_Tot [1]          FORMAT "->>>>>>,>>>,>>9.99"  
          W_Tot [3]          FORMAT "->>>>>,>>>,>>9.99"  
          "              " 
          W_Tot [5]          FORMAT "->>>>>>,>>>,>>9.99"  
          SKIP(1)
      WITH DOWN WIDTH 250 FRAME Ftot1 USE-TEXT NO-LABELS STREAM-IO NO-BOX.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValidarCtas W-InverS 
PROCEDURE ValidarCtas :
/*------------------------------------------------------------------------------
     Invocado desde Triggers Salvar.
  Purpose: Valida las configuraciones contables.
 ------------------------------------------------------------------------------*/
  FIND Cuentas WHERE Cuentas.Cuenta EQ Pro_Inversiones.Cuenta_Inversion_DB
                 AND Cuentas.Tipo   EQ 2
                 AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
  IF NOT AVAIL(Cuentas) THEN DO:
     MESSAGE "La cuenta contable-Dèbito de la inversión es obligatoria," SKIP
             "de movimiento y Activa en el PUC...Ingrese una correcta por favor." VIEW-AS ALERT-BOX.
     RETURN ERROR.
  END.
  
  FIND Cuentas WHERE Cuentas.Cuenta EQ Pro_Inversiones.CtaRendimiento_DB
                 AND Cuentas.Tipo   EQ 2
                 AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
  IF NOT AVAIL(Cuentas) THEN DO:
     MESSAGE "La cuenta contable-Débito para Int-Valorado es obligatoria," SKIP
             "de movimiento y Activa en el PUC...Ingrese una correcta por favor." VIEW-AS ALERT-BOX.
     RETURN ERROR.
  END.

  FIND Cuentas WHERE Cuentas.Cuenta EQ Pro_Inversiones.CtaRendimiento_CR
                 AND Cuentas.Tipo   EQ 2
                 AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
  IF NOT AVAIL(Cuentas) THEN DO:
     MESSAGE "La cuenta contable-Haber para Int-Valorado es obligatoria," SKIP
             "de movimiento y Activa en el PUC...Ingrese una correcta por favor." VIEW-AS ALERT-BOX.
     RETURN ERROR.
  END.

  FIND Cuentas WHERE Cuentas.Cuenta EQ Pro_Inversiones.CtaRetFte_DB
                 AND Cuentas.Tipo   EQ 2
                 AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
  IF NOT AVAIL(Cuentas) THEN DO:
     MESSAGE "La cuenta contable-Débito para Ret-Fuente es obligatoria," SKIP
             "de movimiento y Activa en el PUC...Ingrese una correcta por favor." VIEW-AS ALERT-BOX.
     RETURN ERROR.
  END.

/*  FIND Cuentas WHERE Cuentas.Cuenta EQ Pro_Inversiones.CtaValoracion_DB
                 AND Cuentas.Tipo   EQ 2
                 AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
  IF NOT AVAIL(Cuentas) THEN DO:
     MESSAGE "La cuenta contable-Débito para Valoración es obligatoria," SKIP
             "de movimiento y Activa en el PUC...Ingrese una correcta por favor." VIEW-AS ALERT-BOX.
     RETURN ERROR.
  END.

  FIND Cuentas WHERE Cuentas.Cuenta EQ Pro_Inversiones.CtaValoracion_CR
                 AND Cuentas.Tipo   EQ 2
                 AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
  IF NOT AVAIL(Cuentas) THEN DO:
     MESSAGE "La cuenta contable-Haber para Valoración es obligatoria," SKIP
             "de movimiento y Activa en el PUC...Ingrese una correcta por favor." VIEW-AS ALERT-BOX.
     RETURN ERROR.
  END.
*/

  FIND Cuentas WHERE Cuentas.Cuenta EQ Pro_Inversiones.CtaProvision_DB
                 AND Cuentas.Tipo   EQ 2
                 AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
  IF NOT AVAIL(Cuentas) THEN DO:
     MESSAGE "La cuenta contable-Débito para Provisión es obligatoria," SKIP
             "de movimiento y Activa en el PUC...Ingrese una correcta por favor." VIEW-AS ALERT-BOX.
     RETURN ERROR.
  END.

  FIND Cuentas WHERE Cuentas.Cuenta EQ Pro_Inversiones.CtaProvision_Cr
                 AND Cuentas.Tipo   EQ 2
                 AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
  IF NOT AVAIL(Cuentas) THEN DO:
     MESSAGE "La cuenta contable-Haber para Provisión es obligatoria," SKIP
             "de movimiento y Activa en el PUC...Ingrese una correcta por favor." VIEW-AS ALERT-BOX.
     RETURN ERROR.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

