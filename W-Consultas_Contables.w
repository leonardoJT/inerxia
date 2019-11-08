&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
{Incluido\VARIABLE.I "SHARED"}
/* VARIABLE PARA FECHA */
DEF VAR location AS INTEGER FORMAT "99" NO-UNDO.
DEF VAR lweekday AS INTEGER NO-UNDO.
DEF VAR lstday AS INTEGER NO-UNDO.
DEF VAR d AS INTEGER NO-UNDO.
DEF VAR tnum AS INTEGER format "z9" NO-UNDO.
DEF VAR tchar AS CHAR FORMAT "X(9)" NO-UNDO.
DEF VAR lastday AS INTEGER NO-UNDO.
DEF VAR lastdate AS DATE NO-UNDO.
DEF VAR lentdate AS DATE LABEL " Date" INIT TODAY NO-UNDO.
DEF VAR lmonth AS INTEGER NO-UNDO.
DEF VAR lm LIKE lmonth no-undo.
DEF VAR lyear AS INTEGER FORMAT "9999" NO-UNDO.
def var lyr like lyear no-undo.
DEF var cur as WIDGET-HANDLE.
DEF VAR mnthname AS CHAR FORMAT "stdate(10)" EXTENT 12 INIT
["Enero",
 "Febrero",
 "Marzo",
 "Abril",
 "Mayo",
 "Junio",
 "Julio",
 "Agosto",
 "Septeimbre",
 "Octubre",
 "Noviembre",
 "Diciembre"] NO-UNDO.
def var lbut as logical NO-UNDO.
DEF VAR newdate AS DATE NO-UNDO.
def var totbut as int no-undo.
def var bhandle as widget-handle extent 42 no-undo.
def var lname as char no-undo.
def var i as integer no-undo.
def var lday as integer no-undo.
def var ok as logical no-undo.
DEF VAR barray AS HANDLE EXTENT 42 NO-UNDO.
DEF VAR pmod AS LOGICAL NO-UNDO.
/*FIN VARIABLES PARA FECHA*/

DEFINE VARIABLE W_Ok AS LOGICAL.
    DEFINE VARIABLE W_Ofi        LIKE agencias.agencia.
    DEFINE VARIABLE W_Metodo       AS LOGICAL.
    DEFINE VARIABLE W_Val          AS LOGICAL.
    DEFINE VARIABLE W_Interes    LIKE CarteraVencida.Id_Interes.
    DEFINE VARIABLE W_Asociado     AS INTEGER.
    DEFINE VARIABLE W_ConsCta    LIKE Cuentas.Cuenta.
    DEFINE VARIABLE W_DispCta    LIKE Cuentas.Cuenta.
    DEFINE VARIABLE W_Pcuenta    LIKE Cuentas.Cuenta.
    DEFINE VARIABLE W_Pnombre    LIKE Cuentas.Nombre.
    DEFINE VARIABLE W_Naturaleza LIKE Cuentas.Naturaleza.
    DEFINE VARIABLE W_CtrNat     LIKE Cuentas.Ctr_Naturaleza.
    DEFINE VARIABLE crowid         AS ROWID.
    DEFINE VARIABLE W_Rpta         AS LOGICAL.

/*variables para la funcion hallarsaldo*/
    DEFINE VARIABLE P_OfiIni LIKE Agencias.Agencia.
    DEFINE VARIABLE P_OfiFin LIKE Agencias.Agencia.
    DEFINE VARIABLE P_CenIni LIKE Cen_Costos.Cen_Costos.
    DEFINE VARIABLE P_CenFin LIKE Cen_Costos.Cen_Costos.
    DEFINE VARIABLE P_Ano    LIKE Sal_Cuenta.Ano.
    DEFINE VARIABLE P_Mes    AS INTEGER   FORMAT "99".
    DEFINE VARIABLE P_SdoDeb LIKE Sal_Cuenta.Sal_Inicial   INITIAL 0.
    DEFINE VARIABLE P_SdoCre LIKE Sal_Cuenta.Sal_Inicial INITIAL 0.
    DEFINE VARIABLE P_SdoAct LIKE Sal_Cuenta.Sal_Inicial  INITIAL 0.

/*Temporales para los browsers*/
    DEFINE TEMP-TABLE TSaldos
        FIELD TS_Mes    AS INTEGER FORMAT "99" 
        FIELD TS_NMes   AS CHARACTER FORMAT "X(23)"
        FIELD TS_Inicial   AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" LABEL "Saldo Inicial"
        FIELD TS_Debito    AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" LABEL "Debito"
        FIELD TS_Credito   AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" LABEL "Credito"
        FIELD TS_Final     AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" LABEL "Saldo Final"
    INDEX I_Mes IS UNIQUE PRIMARY Ts_Mes DESCENDING.

    DEFINE TEMP-TABLE TAnexos
        FIELD TA_Reg       AS INTEGER   FORMAT "9999"
        FIELD TA_Nit       AS CHARACTER FORMAT "X(14)" LABEL "Nit"
        FIELD TA_Agencia   AS INTEGER   FORMAT "999"   LABEL "Age"
        FIELD TA_CenCos    AS INTEGER   FORMAT "999"   LABEL "CCos"
        FIELD TA_Inicial   AS DECIMAL   FORMAT "->>>,>>>,>>>,>>9.99" LABEL "Saldo Inicial"
        FIELD TA_Debito    AS DECIMAL   FORMAT "->>>,>>>,>>>,>>9.99" LABEL "Debito"
        FIELD TA_Credito   AS DECIMAL   FORMAT "->>>,>>>,>>>,>>9.99" LABEL "Credito"
        FIELD TA_Final     AS DECIMAL   FORMAT "->>>,>>>,>>>,>>9.99" LABEL "Saldo Final".

    DEFINE TEMP-TABLE TDetalle
        FIELD TD_Doc       AS CHARACTER FORMAT "X(10)" LABEL "Doc.Referencia"
        /*FIELD TD_Nit       AS CHARACTER FORMAT "X(14)" LABEL "Nit"*/
        FIELD TD_Agencia   AS INTEGER   FORMAT "999"   LABEL "Age"
        FIELD TD_CenCos    AS INTEGER   FORMAT "999"   LABEL "CCos"
        FIELD TD_FecCon    AS DATE                     LABEL "Fec.Grabacion"
        FIELD TD_FecUlt    AS DATE                     LABEL "Fec.Actualizacion"
        FIELD TD_Debito    AS DECIMAL   FORMAT "->>>,>>>,>>9.99"
        FIELD TD_Credito   AS DECIMAL   FORMAT "->>>,>>>,>>9.99"
        FIELD TD_Plazo     AS DECIMAL                  LABEL "Plazo"
        FIELD TD_Amort     AS DECIMAL   FORMAT "->>>,>>>,>>9.99" LABEL "Amortizacion"
        FIELD TD_SdoFin    AS DECIMAL   FORMAT "->>>,>>>,>>9.99" LABEL "Saldo Final".

    
    DEFINE TEMP-TABLE TMov
        FIELD TM_Age       AS INTEGER   FORMAT "999"                LABEL "Age"
        FIELD TM_CCos      AS INTEGER   FORMAT "999"                LABEL "CCo"
        FIELD TM_Cbte      AS INTEGER   FORMAT "99"                 LABEL "Cbte"
        FIELD TM_Doc       AS INTEGER   FORMAT "9999999"            LABEL "Docto"
        FIELD TM_FecGra    AS DATE      FORMAT "99/99/9999"         LABEL "FecCont"
        FIELD TM_NumDoc    AS CHARACTER FORMAT "X(10)"              LABEL "DcRef"
        FIELD TM_Coment    AS CHARACTER FORMAT "X(18)"              LABEL "Comentario"
        FIELD TM_Debito    AS DECIMAL   FORMAT "->>,>>>,>>>,>>9.99" LABEL "Debito"
        FIELD TM_Credito   AS DECIMAL   FORMAT "->>,>>>,>>>,>>9.99" LABEL "Credito"
        FIELD TM_Usuario   AS CHARACTER FORMAT "X(4)"               LABEL "Usu".

    DEFINE TEMP-TABLE TPartidas
        FIELD TP_Cuenta    AS CHARACTER FORMAT "X(14)"              LABEL "Cuenta"
        FIELD TP_CCos      AS DECIMAL   FORMAT "999"                LABEL "CCo"
        FIELD TP_DocRef    AS CHARACTER FORMAT "X(10)"            LABEL "DocRef"
        FIELD TP_Nit       AS CHARACTER FORMAT "X(14)"              LABEL "Nit"
        FIELD TP_Coment    AS CHARACTER FORMAT "X(40)"              LABEL "Comentario"
        FIELD TP_Debito    AS DECIMAL   FORMAT "->>,>>>,>>>,>>9.99" LABEL "Debito"
        FIELD TP_Credito   AS DECIMAL   FORMAT "->>,>>>,>>>,>>9.99" LABEL "Credito".
        

     DEFINE TEMP-TABLE TExcel
        FIELD TE_Ide      AS CHARACTER FORMAT "X(14)"
        FIELD TE_Age      AS INTEGER   FORMAT "999"
        FIELD TE_CC       AS INTEGER   FORMAT "999"
        FIELD TE_Doc      AS CHARACTER FORMAT "X(10)"
        FIELD TE_Fecha    AS DATE FORMAT "99/99/99"
        FIELD TE_Inicial  AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99"
        FIELD TE_Debito   AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99"
        FIELD TE_Credito  AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99"
        FIELD TE_Final    AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99".

    /*Variables del Browse de Saldos para mostrar en informe*/

    DEFINE VAR IMes      AS INTEGER   FORMAT "99".
    DEFINE VAR INMes     AS CHARACTER FORMAT "X(15)".
    DEFINE VAR IInicial  AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99".
    DEFINE VAR IDebito   AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99".
    DEFINE VAR ICredito  AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99".
    DEFINE VAR IFinal    AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99".
    DEFINE VAR NReg      AS INTEGER.
    DEFINE VARIABLE IDSdoFin    AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99".

DEFINE VAR saldoInicial AS DECIMAL.
DEFINE VAR cenCostos AS INTEGER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME BR_Anexos

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TAnexos TMov TPartidas TSaldos

/* Definitions for BROWSE BR_Anexos                                     */
&Scoped-define FIELDS-IN-QUERY-BR_Anexos TAnexos.TA_Nit TAnexos.TA_Agencia TAnexos.TA_CenCos TAnexos.TA_Inicial TAnexos.TA_Debito TAnexos.TA_Credito TAnexos.TA_Final   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BR_Anexos   
&Scoped-define SELF-NAME BR_Anexos
&Scoped-define QUERY-STRING-BR_Anexos FOR EACH TAnexos NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BR_Anexos OPEN QUERY {&SELF-NAME} FOR EACH TAnexos NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BR_Anexos TAnexos
&Scoped-define FIRST-TABLE-IN-QUERY-BR_Anexos TAnexos


/* Definitions for BROWSE Br_Mov                                        */
&Scoped-define FIELDS-IN-QUERY-Br_Mov TMov.TM_Age TMov.TM_CCos TMov.TM_Cbte TMov.TM_Doc TMov.TM_FecGra TMov.TM_NumDoc TMov.TM_Coment TMov.TM_Debito TMov.TM_Credito TMov.TM_Usuario   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Mov   
&Scoped-define SELF-NAME Br_Mov
&Scoped-define QUERY-STRING-Br_Mov FOR EACH TMov NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br_Mov OPEN QUERY {&SELF-NAME} FOR EACH TMov NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_Mov TMov
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Mov TMov


/* Definitions for BROWSE Br_Partidas                                   */
&Scoped-define FIELDS-IN-QUERY-Br_Partidas TP_Cuenta TP_Coment TP_CCos TP_DocRef TP_Nit TP_Debito TP_Credito   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Partidas   
&Scoped-define SELF-NAME Br_Partidas
&Scoped-define QUERY-STRING-Br_Partidas FOR EACH TPartidas NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br_Partidas OPEN QUERY {&SELF-NAME} FOR EACH TPartidas NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_Partidas TPartidas
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Partidas TPartidas


/* Definitions for BROWSE B_Saldos                                      */
&Scoped-define FIELDS-IN-QUERY-B_Saldos TSaldos.TS_Mes TSaldos.TS_NMes TSaldos.TS_Inicial TSaldos.TS_Debito TSaldos.TS_Credito TSaldos.TS_Final   
&Scoped-define ENABLED-FIELDS-IN-QUERY-B_Saldos   
&Scoped-define SELF-NAME B_Saldos
&Scoped-define QUERY-STRING-B_Saldos FOR EACH TSaldos NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-B_Saldos OPEN QUERY {&SELF-NAME} FOR EACH TSaldos NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-B_Saldos TSaldos
&Scoped-define FIRST-TABLE-IN-QUERY-B_Saldos TSaldos


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-Br_Mov}~
    ~{&OPEN-QUERY-B_Saldos}

/* Definitions for FRAME F_Partidas                                     */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS B_Saldos BR_Anexos Br_Mov BuscarNit ~
B_Filtros W_Cuenta BUTTON-8 BUTTON-9 BUTTON-14 BUTTON-11 RECT-220 
&Scoped-Define DISPLAYED-OBJECTS BuscarNit Titulo_Movimiento W_Cuenta ~
W_NomCuenta 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-Br_Mov 
       MENU-ITEM m_Ver_Partidas_Contables LABEL "Ver Partidas Contables".


/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-11 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 11" 
     SIZE 4 BY 1.12.

DEFINE BUTTON BUTTON-121 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "Button 121" 
     SIZE 8 BY 1.62.

DEFINE BUTTON BUTTON-14 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 14" 
     SIZE 8 BY 1.62.

DEFINE BUTTON BUTTON-8 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 8" 
     SIZE 8 BY 1.62.

DEFINE BUTTON BUTTON-9 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Button 9" 
     SIZE 8 BY 1.62.

DEFINE BUTTON B_Filtros 
     LABEL "Filtros" 
     SIZE 15 BY .81.

DEFINE VARIABLE BuscarNit AS CHARACTER FORMAT "X(14)":U 
     LABEL "Buscar Nit" 
     VIEW-AS FILL-IN 
     SIZE 16.57 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Titulo_Movimiento AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 59 BY .81 NO-UNDO.

DEFINE VARIABLE W_Cuenta AS CHARACTER FORMAT "X(14)":U 
     LABEL "Cuenta" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomCuenta AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 70 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-220
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.86 BY 5.38.

DEFINE BUTTON b-nextmonth 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Btn 3":L 
     SIZE 2.57 BY .62.

DEFINE BUTTON b-nextyear 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Btn 3":L 
     SIZE 2.57 BY .62.

DEFINE BUTTON b-prevbmonth 
     IMAGE-UP FILE "imagenes/arwup.gif":U
     LABEL "Btn 3":L 
     SIZE 2.57 BY .62.

DEFINE BUTTON b-prevyear 
     IMAGE-UP FILE "imagenes/arwup.gif":U
     LABEL "Btn 3":L 
     SIZE 2.57 BY .62.

DEFINE BUTTON BUTTON-4 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 4" 
     SIZE 8 BY 1.62.

DEFINE VARIABLE Cmb_Agencia1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 28 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_Agencia2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Hasta Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 28 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_CenCostos1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Cen.Costos" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 28 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_CenCostos2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Hasta Cen.Costos" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 28 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE styear AS CHARACTER FORMAT "X(5)":U 
     VIEW-AS FILL-IN 
     SIZE 4.86 BY .81
     BGCOLOR 15 FGCOLOR 7 FONT 5 NO-UNDO.

DEFINE IMAGE monthimage
     FILENAME "imagenes/month1.gif":U
     SIZE 11 BY .92.

DEFINE RECTANGLE RECT-150
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 26 BY 2.15.

DEFINE RECTANGLE RECT-152
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 48 BY 7.27.

DEFINE VARIABLE T_IdCenCostos AS LOGICAL INITIAL no 
     LABEL "Consulta entre Rangos de Centros de Costo?" 
     VIEW-AS TOGGLE-BOX
     SIZE 42 BY 1.04 NO-UNDO.

DEFINE VARIABLE T_RangoAgencia AS LOGICAL INITIAL no 
     LABEL "Consulta entre Rango de Agencias?" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY 1.08 NO-UNDO.

DEFINE BUTTON BUTTON-15 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 15" 
     SIZE 9 BY 1.62.

DEFINE VARIABLE SPartidas AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 94 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 7 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BR_Anexos FOR 
      TAnexos SCROLLING.

DEFINE QUERY Br_Mov FOR 
      TMov SCROLLING.

DEFINE QUERY Br_Partidas FOR 
      TPartidas SCROLLING.

DEFINE QUERY B_Saldos FOR 
      TSaldos SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BR_Anexos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BR_Anexos wWin _FREEFORM
  QUERY BR_Anexos NO-LOCK DISPLAY
      TAnexos.TA_Nit 
      TAnexos.TA_Agencia 
      TAnexos.TA_CenCos
      TAnexos.TA_Inicial
      TAnexos.TA_Debito 
      TAnexos.TA_Credito
      TAnexos.TA_Final
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 98 BY 6
         BGCOLOR 15 FONT 5 ROW-HEIGHT-CHARS .5 FIT-LAST-COLUMN.

DEFINE BROWSE Br_Mov
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Mov wWin _FREEFORM
  QUERY Br_Mov NO-LOCK DISPLAY
      TMov.TM_Age
      TMov.TM_CCos
      TMov.TM_Cbte
      TMov.TM_Doc
      TMov.TM_FecGra
      TMov.TM_NumDoc
      TMov.TM_Coment
      TMov.TM_Debito
      TMov.TM_Credito
      TMov.TM_Usuario
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 98 BY 5.62
         BGCOLOR 15 FONT 5 FIT-LAST-COLUMN.

DEFINE BROWSE Br_Partidas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Partidas wWin _FREEFORM
  QUERY Br_Partidas NO-LOCK DISPLAY
      TP_Cuenta
      TP_Coment
      TP_CCos    
      TP_DocRef     
      TP_Nit     
      TP_Debito  
      TP_Credito
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 94 BY 7
         BGCOLOR 15 FONT 4 FIT-LAST-COLUMN.

DEFINE BROWSE B_Saldos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS B_Saldos wWin _FREEFORM
  QUERY B_Saldos NO-LOCK DISPLAY
      TSaldos.TS_Mes  FORMAT "99" LABEL "Mes"
      TSaldos.TS_NMes FORMAT "X(23)" LABEL "Nombre"
      TSaldos.TS_Inicial FORMAT "->>,>>>,>>>,>>>,>>9.99" LABEL "Saldo Inicial"
      TSaldos.TS_Debito  FORMAT "->>,>>>,>>>,>>>,>>9.99" LABEL "Debito"
      TSaldos.TS_Credito FORMAT "->>,>>>,>>>,>>>,>>9.99" LABEL "Credito"
      TSaldos.TS_Final   FORMAT "->>,>>>,>>>,>>>,>>9.99" LABEL "Saldo Final"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 108 BY 3.31
         BGCOLOR 15 FONT 5 ROW-HEIGHT-CHARS .65 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     B_Saldos AT ROW 3.15 COL 3
     BR_Anexos AT ROW 8 COL 3
     Br_Mov AT ROW 15.85 COL 3
     BUTTON-121 AT ROW 10.42 COL 103
     BuscarNit AT ROW 7 COL 82 COLON-ALIGNED
     Titulo_Movimiento AT ROW 15.04 COL 1 COLON-ALIGNED NO-LABEL
     B_Filtros AT ROW 1.31 COL 3
     W_Cuenta AT ROW 1.27 COL 24 COLON-ALIGNED
     W_NomCuenta AT ROW 1.27 COL 39 COLON-ALIGNED NO-LABEL
     BUTTON-8 AT ROW 7.19 COL 103
     BUTTON-9 AT ROW 8.81 COL 103
     BUTTON-14 AT ROW 18.23 COL 103
     BUTTON-11 AT ROW 20.65 COL 105
     "Resultado de la Consulta (Saldos de Cuenta)" VIEW-AS TEXT
          SIZE 40 BY .62 AT ROW 2.31 COL 3
          BGCOLOR 17 FGCOLOR 0 
     "Anexos de la cuenta por Mes contable" VIEW-AS TEXT
          SIZE 33 BY .62 AT ROW 7.19 COL 3
     RECT-220 AT ROW 6.92 COL 102
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 111.86 BY 21.08
         BGCOLOR 17 FONT 5.

DEFINE FRAME F_Partidas
     SPartidas AT ROW 1.54 COL 3 NO-LABEL
     Br_Partidas AT ROW 2.62 COL 3
     BUTTON-15 AT ROW 9.88 COL 88
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 8 ROW 5.85
         SIZE 98 BY 11.58
         BGCOLOR 17 
         TITLE "Partidas del Comprobante".

DEFINE FRAME F_Filtros
     b-prevbmonth AT ROW 2.35 COL 16
     b-prevyear AT ROW 2.35 COL 24.14
     styear AT ROW 2.5 COL 17 COLON-ALIGNED NO-LABEL
     b-nextmonth AT ROW 2.88 COL 16
     b-nextyear AT ROW 2.88 COL 24.14
     T_RangoAgencia AT ROW 4.5 COL 5
     Cmb_Agencia1 AT ROW 5.58 COL 18 COLON-ALIGNED
     Cmb_Agencia2 AT ROW 6.65 COL 18 COLON-ALIGNED
     T_IdCenCostos AT ROW 7.73 COL 5
     Cmb_CenCostos1 AT ROW 8.81 COL 18 COLON-ALIGNED
     Cmb_CenCostos2 AT ROW 9.88 COL 18 COLON-ALIGNED
     BUTTON-4 AT ROW 11.5 COL 41
     " Fecha de la Consulta" VIEW-AS TEXT
          SIZE 19 BY .62 AT ROW 1.27 COL 5
          FGCOLOR 7 
     monthimage AT ROW 2.54 COL 5
     RECT-150 AT ROW 1.54 COL 2
     RECT-152 AT ROW 3.96 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 40 ROW 1
         SIZE 51 BY 13.19
         BGCOLOR 17 FONT 5
         TITLE "Filtros de la Consulta".


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
         TITLE              = "Consultas Contables"
         HEIGHT             = 21.08
         WIDTH              = 111.86
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

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT wWin:LOAD-ICON("adeicon/debug%.ico":U) THEN
    MESSAGE "Unable to load icon: adeicon/debug%.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
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
ASSIGN FRAME F_Filtros:FRAME = FRAME F-Main:HANDLE
       FRAME F_Partidas:FRAME = FRAME F-Main:HANDLE.

/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Custom                                        */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME F_Filtros:MOVE-AFTER-TAB-ITEM (W_NomCuenta:HANDLE IN FRAME F-Main)
       XXTABVALXX = FRAME F_Filtros:MOVE-BEFORE-TAB-ITEM (BUTTON-8:HANDLE IN FRAME F-Main)
/* END-ASSIGN-TABS */.

/* BROWSE-TAB B_Saldos 1 F-Main */
/* BROWSE-TAB BR_Anexos B_Saldos F-Main */
/* BROWSE-TAB Br_Mov BR_Anexos F-Main */
ASSIGN 
       Br_Mov:POPUP-MENU IN FRAME F-Main             = MENU POPUP-MENU-Br_Mov:HANDLE.

/* SETTINGS FOR BUTTON BUTTON-121 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Titulo_Movimiento IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomCuenta IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Filtros
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Filtros:HIDDEN           = TRUE.

/* SETTINGS FOR COMBO-BOX Cmb_Agencia2 IN FRAME F_Filtros
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       Cmb_Agencia2:HIDDEN IN FRAME F_Filtros           = TRUE.

/* SETTINGS FOR COMBO-BOX Cmb_CenCostos2 IN FRAME F_Filtros
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       Cmb_CenCostos2:HIDDEN IN FRAME F_Filtros           = TRUE.

/* SETTINGS FOR FRAME F_Partidas
   NOT-VISIBLE                                                          */
/* BROWSE-TAB Br_Partidas SPartidas F_Partidas */
ASSIGN 
       FRAME F_Partidas:HIDDEN           = TRUE
       FRAME F_Partidas:MOVABLE          = TRUE.

ASSIGN 
       SPartidas:READ-ONLY IN FRAME F_Partidas        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BR_Anexos
/* Query rebuild information for BROWSE BR_Anexos
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TAnexos NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is NOT OPENED
*/  /* BROWSE BR_Anexos */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_Mov
/* Query rebuild information for BROWSE Br_Mov
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TMov NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE Br_Mov */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_Partidas
/* Query rebuild information for BROWSE Br_Partidas
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TPartidas NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is NOT OPENED
*/  /* BROWSE Br_Partidas */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE B_Saldos
/* Query rebuild information for BROWSE B_Saldos
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TSaldos NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE B_Saldos */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Consultas Contables */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Consultas Contables */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Filtros
&Scoped-define SELF-NAME b-nextmonth
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-nextmonth wWin
ON CHOOSE OF b-nextmonth IN FRAME F_Filtros /* Btn 3 */
DO:

    IF  lmonth > 1 THEN lmonth = lmonth - 1.
  ELSE ASSIGN lmonth = 12
              lyear = lyear - 1.
  RUN setday. 
  pmod = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b-nextyear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-nextyear wWin
ON CHOOSE OF b-nextyear IN FRAME F_Filtros /* Btn 3 */
DO:
       lyear = lyear - 1.
     FIND FIRST Calendario WHERE Calendario.Ano EQ LYear NO-LOCK NO-ERROR.
     IF NOT AVAILABLE(Calendario) THEN
     DO:
         MESSAGE "El calendario para el año " STRING(Lyear) " no ha sido creado" SKIP
                 "utilice el boton de 'Generación de Calendario' para crear un nuevo" SKIP
                 "Calendario!" VIEW-AS ALERT-BOX INFORMATION.
         Lyear = Lyear + 1.
         RETURN NO-APPLY.
     END.
     RUN setday. 
     pmod = TRUE.
  
    /* DYNAMIC-FUNCTION('setDataModified':U,INPUT pMod).   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b-prevbmonth
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-prevbmonth wWin
ON CHOOSE OF b-prevbmonth IN FRAME F_Filtros /* Btn 3 */
DO:
    IF lmonth < 12 THEN
     lmonth = lmonth + 1.
  ELSE ASSIGN lmonth = 1
              lyear = lyear + 1.
  RUN setday. 
  pmod = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b-prevyear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-prevyear wWin
ON CHOOSE OF b-prevyear IN FRAME F_Filtros /* Btn 3 */
DO:
     lyear = lyear + 1.
     FIND FIRST Calendario WHERE Calendario.Ano EQ LYear NO-LOCK NO-ERROR.
     IF NOT AVAILABLE(Calendario) THEN
     DO:
         MESSAGE "El calendario para el año " STRING(Lyear) " no ha sido creado" SKIP
                 "utilice el boton de 'Generación de Calendario' para crear un nuevo" SKIP
                 "Calendario!" VIEW-AS ALERT-BOX INFORMATION.
         Lyear = Lyear - 1.
         RETURN NO-APPLY.
     END.
     RUN setday. 

     pmod = TRUE.

     /*DYNAMIC-FUNCTION('setDataModified':U,
     INPUT pMod).   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BR_Anexos
&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME BR_Anexos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BR_Anexos wWin
ON MOUSE-SELECT-CLICK OF BR_Anexos IN FRAME F-Main
DO:
    cenCostos = TAnexos.TA_CenCos.

    /*IF Cuentas.Id_Detalle THEN DO:
        RUN Repos_Detalle.
        RUN Mov_Detalle.
    END.
    ELSE*/ RUN Mov_Anexos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BuscarNit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BuscarNit wWin
ON ENTRY OF BuscarNit IN FRAME F-Main /* Buscar Nit */
DO:
  ON RETURN TAB.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BuscarNit wWin
ON LEAVE OF BuscarNit IN FRAME F-Main /* Buscar Nit */
DO:
  ASSIGN FRAME F-Main BuscarNit.
  DEFINE VAR T AS INTEGER.
  FIND FIRST TAnexos WHERE TAnexos.TA_Nit EQ BuscarNit NO-ERROR.
  IF AVAILABLE(TAnexos) THEN REPOSITION Br_Anexos TO ROW TAnexos.TA_Reg.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-14
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-14 wWin
ON CHOOSE OF BUTTON-14 IN FRAME F-Main /* Button 14 */
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


&Scoped-define FRAME-NAME F_Partidas
&Scoped-define SELF-NAME BUTTON-15
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-15 wWin
ON CHOOSE OF BUTTON-15 IN FRAME F_Partidas /* Button 15 */
DO:
  HIDE FRAME F_Partidas.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Filtros
&Scoped-define SELF-NAME BUTTON-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-4 wWin
ON CHOOSE OF BUTTON-4 IN FRAME F_Filtros /* Button 4 */
DO:
  HIDE FRAME F_Filtros.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME BUTTON-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-8 wWin
ON CHOOSE OF BUTTON-8 IN FRAME F-Main /* Button 8 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-9 wWin
ON CHOOSE OF BUTTON-9 IN FRAME F-Main /* Button 9 */
DO:
  DEFINE VAR Listado AS CHARACTER INITIAL "".
  listado = W_PathSpl + "L_Grupo.Lst".
  {Incluido\Imprimir.I "listado"}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B_Filtros
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_Filtros wWin
ON CHOOSE OF B_Filtros IN FRAME F-Main /* Filtros */
DO:
  VIEW FRAME F_Filtros.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME B_Saldos
&Scoped-define SELF-NAME B_Saldos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_Saldos wWin
ON MOUSE-SELECT-CLICK OF B_Saldos IN FRAME F-Main
DO:
    p_Mes = TSaldos.TS_Mes.

    IF Cuentas.Id_Nit THEN DO:
        RUN Repos_Anexos.

        /*IF Cuentas.Id_Detalle THEN DO:
            RUN Repos_Detalle.
            RUN Mov_Detalle.
        END.
        ELSE*/
            RUN Mov_Anexos.
    END.
    ELSE
        RUN Mov_Cuenta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_Saldos wWin
ON VALUE-CHANGED OF B_Saldos IN FRAME F-Main
DO:
  ASSIGN IMes     = TSaldos.TS_Mes
         INmes    = TSaldos.TS_NMes
         IInicial = TSaldos.TS_Inicial
         IDebito  = TSaldos.TS_Debito
         ICredito = TSaldos.TS_Credito
         IFinal   = TSaldos.TS_Final.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Ver_Partidas_Contables
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Ver_Partidas_Contables wWin
ON CHOOSE OF MENU-ITEM m_Ver_Partidas_Contables /* Ver Partidas Contables */
DO:
    
  DEFINE VAR Texto AS CHARACTER FORMAT "X(80)" INITIAL "".
  FOR EACH TPartidas: DELETE TPartidas. END.
  FOR EACH Mov_Contable WHERE Mov_Contable.Agencia       EQ TMov.TM_Age     AND
                              Mov_Contable.Comprobante   EQ TMov.TM_Cbte    AND
                              Mov_Contable.Num_Documento EQ TMov.TM_Doc     AND
                              Mov_Contable.Fec_Grabacion EQ TMov.TM_FecGra  NO-LOCK:
    Texto = 
        "Agencia : " + STRING(TMov.TM_Age) + " - " +
        "Comprobante  : " + STRING(TMov.TM_Cbte) + " - Num.Doc: " + STRING(TMov.TM_Doc) + " - " +
        "Fec.Grabacion: " + STRING(TMov.TM_FecGra) + " - " +
        "Usuario : " + Mov_Contable.Usuario.
    SPartidas:SCREEN-VALUE IN FRAME F_Partidas = Texto. 
    CREATE TPartidas.
    ASSIGN TPartidas.TP_Cuenta     = Mov_Contable.Cuenta
           TPartidas.TP_CCos       = Mov_Contable.Cen_Costos
           TPartidas.TP_DocRef     = Mov_Contable.Doc_Referencia
           TPartidas.TP_Coment     = Mov_Contable.Comentario
           TPartidas.TP_Nit        = Mov_Contable.Nit
           TPartidas.TP_Debito     = Mov_Contable.DB
           TPartidas.TP_Credito    = Mov_Contable.CR.
 END.
 OPEN QUERY Br_Partidas FOR EACH TPartidas NO-LOCK INDEXED-REPOSITION.
 VIEW FRAME F_Partidas.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Filtros
&Scoped-define SELF-NAME T_RangoAgencia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T_RangoAgencia wWin
ON VALUE-CHANGED OF T_RangoAgencia IN FRAME F_Filtros /* Consulta entre Rango de Agencias? */
DO:
DO WITH FRAME F_Filtros:
  ASSIGN T_RangoAgencia.
  IF T_RangoAgencia THEN
     ENABLE Cmb_Agencia2 WITH FRAME F_Filtros.
  ELSE
     Cmb_Agencia2:HIDDEN = YES.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME W_Cuenta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Cuenta wWin
ON LEAVE OF W_Cuenta IN FRAME F-Main /* Cuenta */
DO:
    ASSIGN FRAME F-Main W_Cuenta.

    IF W_Cuenta EQ "" THEN
        RUN MoDbClkCta.
    ELSE DO:
        FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ W_Cuenta:SCREEN-VALUE NO-LOCK NO-ERROR.
        IF AVAILABLE(Cuentas) THEN
            W_NomCuenta:SCREEN-VALUE IN FRAME F-Main = Cuentas.Nombre + " | Naturaleza: " + Cuentas.Naturaleza.
    END.

    EMPTY TEMP-TABLE TSaldos.

    RUN Saldos_de_Cuentas.

    OPEN QUERY B_Saldos FOR EACH TSaldos NO-LOCK INDEXED-REPOSITION.
    REPOSITION B_Saldos TO ROW 1.

    IF Cuentas.Id_Nit THEN DO:
        Titulo_Movimiento:SCREEN-VALUE IN FRAME F-Main = "Movimiento por Anexos".

        IF Cuentas.Id_Detalle THEN
            Titulo_Movimiento:SCREEN-VALUE IN FRAME F-Main = "Movimiento por Detalle".

        RUN Repos_Anexos.
    END.
    ELSE DO:
        Titulo_Movimiento:SCREEN-VALUE IN FRAME F-Main = "Movimiento por Cuenta".

        RUN Mov_Cuenta.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BR_Anexos
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Armar_TablaExcel wWin 
PROCEDURE Armar_TablaExcel :
FOR EACH TSaldos WHERE TSaldos.TS_Mes EQ INTEGER(TSaldos.TS_Mes:SCREEN-VALUE IN BROWSE B_Saldos):
       CREATE TExcel.
       ASSIGN TExcel.TE_Ide            = W_Cuenta
              TExcel.TE_Inicial        = TSaldos.TS_Inicial
              TExcel.TE_Debito         = TSaldos.TS_Debito
              TExcel.TE_Credito        = TSaldos.TS_Credito
              TExcel.TE_Final          = TSaldos.TS_Final.
    END.
    FOR EACH TAnexos:
       CREATE TExcel.
       ASSIGN TExcel.TE_Age            = TA_Agencia
              TExcel.TE_Ide            = TA_Nit    
              TExcel.TE_CC             = TA_CenCos 
              TExcel.TE_Inicial        = TA_Inicial
              TEXcel.TE_Debito         = TA_Debito 
              TExcel.TE_Credito        = TA_Credito
              TEXcel.TE_Final          = TA_Final.  
       IF Cuentas.Id_Detalle THEN DO:
           FOR EACH Detalle WHERE Detalle.Agencia  EQ  TAnexos.TA_Agencia AND 
                    Detalle.Cen_Costos             EQ  TAnexos.TA_CenCos  AND
                    Detalle.Nit                    EQ  TAnexos.TA_Nit     AND
                    Detalle.Cuenta                 EQ  W_Cuenta:
               IF Cuentas.Naturaleza EQ "DB" THEN
                  IDSdoFin         = Detalle.Db - Detalle.Cr.
               ELSE
                  IDSdoFin         = Detalle.Cr - Detalle.Db.
               CREATE TExcel.
               ASSIGN TExcel.TE_Age      = Detalle.Agencia              
                      TExcel.TE_CC       = Detalle.Cen_Costos           
                      TExcel.TE_Doc      = Detalle.Doc_referencia       
                      TExcel.TE_Fecha    = Detalle.Fec_ultActualizacion 
                      TExcel.TE_Inicial  = Detalle.Plazo                
                      TExcel.TE_Debito   = Detalle.Valor_Inicial        
                      TExcel.TE_Final    = IDSdoFin.
           END.
       END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DispDate wWin 
PROCEDURE DispDate :
DO WITH FRAME F_Filtros:
   OK = monthimage:LOAD-IMAGE("imagenes\month" + STRING(lmonth) + ".gif") IN FRAME F_Filtros.
   newdate = DATE(lmonth,lday,lyear).
   ASSIGN styear =  string(INTEGER(YEAR(newdate)),"9999"). /*lyear*/
   DISPLAY  styear WITH FRAME F_Filtros. 
 END.
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
  DISPLAY BuscarNit Titulo_Movimiento W_Cuenta W_NomCuenta 
      WITH FRAME F-Main IN WINDOW wWin.
  ENABLE B_Saldos BR_Anexos Br_Mov BuscarNit B_Filtros W_Cuenta BUTTON-8 
         BUTTON-9 BUTTON-14 BUTTON-11 RECT-220 
      WITH FRAME F-Main IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  DISPLAY styear T_RangoAgencia Cmb_Agencia1 T_IdCenCostos Cmb_CenCostos1 
      WITH FRAME F_Filtros IN WINDOW wWin.
  ENABLE monthimage RECT-150 RECT-152 b-prevbmonth b-prevyear styear 
         b-nextmonth b-nextyear T_RangoAgencia Cmb_Agencia1 T_IdCenCostos 
         Cmb_CenCostos1 BUTTON-4 
      WITH FRAME F_Filtros IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Filtros}
  DISPLAY SPartidas 
      WITH FRAME F_Partidas IN WINDOW wWin.
  ENABLE SPartidas Br_Partidas BUTTON-15 
      WITH FRAME F_Partidas IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Partidas}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HallarDetalle wWin 
PROCEDURE HallarDetalle :
FOR EACH Detalle WHERE Detalle.Agencia EQ TAnexos.TA_Agencia
                   AND Detalle.Cen_Costos EQ  TAnexos.TA_CenCos
                   AND Detalle.Nit EQ TAnexos.TA_Nit
                   AND Detalle.Cuenta EQ W_Cuenta:
    CREATE TDetalle.
    ASSIGN TDetalle.TD_Agencia = Detalle.Agencia
           TDetalle.TD_CenCos = Detalle.Cen_Costos
           TDetalle.TD_Doc = Detalle.Doc_referencia
           TDetalle.TD_FecCon = Detalle.Fec_Contable
           TDetalle.TD_FecUlt = Detalle.Fec_ultActualizacion
           TDetalle.TD_Plazo = Detalle.Plazo
           TDetalle.TD_Amort = Detalle.Valor_amortizacion.

    IF Cuentas.Naturaleza EQ "DB" THEN
        TDetalle.TD_SdoFin = Detalle.Db - Detalle.Cr.
    ELSE
        TDetalle.TD_SdoFin = Detalle.Cr - Detalle.Db.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HallarSaldo wWin 
PROCEDURE HallarSaldo :
DEFINE VARIABLE i AS INTEGER.

FOR EACH Sal_Cuenta FIELDS(Sal_Cuenta.Cuenta Sal_Cuenta.Sal_Inicial Sal_Cuenta.Db Sal_Cuenta.Cr)
                    WHERE Sal_Cuenta.Agencia GE P_OfiIni
                      AND Sal_Cuenta.Agencia LE P_OfiFin
                      AND Sal_Cuenta.Cen_Costos GE P_CenIni
                      AND Sal_Cuenta.Cen_Costos LE P_CenFin
                      AND Sal_Cuenta.Cuenta EQ W_Cuenta
                      AND Sal_Cuenta.Ano EQ P_Ano NO-LOCK BREAK BY Sal_Cuenta.Cuenta:
    IF FIRST-OF(Sal_Cuenta.Cuenta) THEN
        P_SdoAct = 0.

    P_SdoAct = /*P_SdoAct +*/ Sal_Cuenta.Sal_Inicial.
    
    DO i = 1 TO P_Mes BY 1:
        IF FIRST-OF(Sal_Cuenta.Cuenta) THEN
            CREATE TSaldos.       /**/
        ELSE
            FIND FIRST TSaldos WHERE TSaldos.TS_Mes EQ i NO-ERROR.     /**/

        ASSIGN TSaldos.TS_Mes = I
               TSaldos.TS_NMes = MnthName[i]
               TSaldos.TS_Debito = TSaldos.TS_Debito + Db[i]
               TSaldos.TS_Credito = TSaldos.TS_Credito + Cr[i].

/*        IF Cuentas.Naturaleza EQ "DB" THEN
            ASSIGN P_SdoAct = P_SdoAct + DB[i] - Cr[i]
                   TSaldos.TS_Final = TSaldos.TS_Final + P_SdoAct /**/
                   TSaldos.TS_Inicial = P_SdoAct - DB[i] + Cr[i].
        ELSE
            ASSIGN P_SdoAct = P_SdoAct - DB[i] + Cr[i]
                   TSaldos.TS_Final = TSaldos.TS_Final + P_SdoAct /**/
                   TSaldos.TS_Inicial = P_SdoAct + DB[i] - Cr[i].
*/
        IF Cuentas.Naturaleza EQ "DB" THEN
            ASSIGN TSaldos.TS_Inicial = TSaldos.TS_inicial + P_SdoAct
                   P_SdoAct = P_SdoAct + DB[i] - Cr[i]
                   TSaldos.TS_Final = TSaldos.TS_Final + P_SdoAct.
        ELSE
            ASSIGN TSaldos.TS_Inicial = TSaldos.TS_Inicial + P_SdoAct
                   P_SdoAct = P_SdoAct - DB[i] + Cr[i]
                   TSaldos.TS_Final = TSaldos.TS_Final + P_SdoAct.
    END.

    /*ASSIGN P_SdoCre = P_SdoCre + Cr[P_Mes]
           P_SdoDeb = P_SdoDeb + Db[P_Mes].*/
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HallarSdoAnexos wWin 
PROCEDURE HallarSdoAnexos :
NReg = 1.
  FOR EACH Anexos WHERE 
           Anexos.Agencia    GE P_OfiIni AND
           Anexos.Agencia    LE P_OfiFin AND
           Anexos.Cen_Costos GE P_CenIni AND
           Anexos.Cen_Costos LE P_CenFin AND
           Anexos.Cuenta      = W_Cuenta AND
           Anexos.Ano         = P_Ano    NO-LOCK:
      IF AVAILABLE(Cuentas) THEN DO:
        P_SdoAct = Anexos.Sdo_Inicial.
        DO i = 1 TO P_Mes BY 1:
           IF Cuentas.Naturaleza EQ "DB" THEN
              ASSIGN P_SdoAct           = P_SdoAct + DB[i] - Cr[i].
           ELSE
              ASSIGN P_SdoAct           = P_SdoAct - DB[i] + Cr[i].
        END.
        CREATE TAnexos.
        ASSIGN TAnexos.TA_Reg      = NReg
               TAnexos.TA_Nit      = Anexos.Nit
               TAnexos.TA_Agencia  = Anexos.Agencia
               TAnexos.TA_CenCos   = Anexos.Cen_Costos
               TAnexos.TA_Debito   = Db[TSaldos.TS_Mes]
               TAnexos.TA_Credito  = Cr[TSaldos.TS_Mes].
        IF Cuentas.Naturaleza EQ "DB" THEN
           ASSIGN TAnexos.TA_Final   = P_SdoAct
                  TAnexos.TA_Inicial = P_SdoAct - DB[TSaldos.TS_Mes] + Cr[TSaldos.TS_Mes].
        ELSE
           ASSIGN TAnexos.TA_Final   = P_SdoAct
                  TAnexos.TA_Inicial = P_SdoAct + DB[TSaldos.TS_Mes] - Cr[TSaldos.TS_Mes].
        NReg = NReg + 1.
      END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir_Excel wWin 
PROCEDURE Imprimir_Excel :
{Incluido\Def_Excel.i}
 /* MANDA ENCABEZADO DE HOJA DE CALCULO*/
 RUN Armar_TablaExcel.
 
 E_NumFila = 1.
 E_NumColumn = 8.
 E_Fila      =      "014" + "Cuenta        "
                  + "003" + "Age"
                  + "003" + "CCo"
                  + "010" + "Doc.Refer "
                  /*+ "008" + "Fecha   "*/
                  + "019" + "Saldo Inicial      "
                  + "019" + "Debito             "
                  + "019" + "Credito            "
                  + "019" + "Saldo Final        ".
 RUN W-GraExcel.w (INPUT E_Fila, INPUT E_NumColumn, OUTPUT E_CmpGrafic).

/* launch Excel so it is visible to the user */
chExcelApp:Visible = TRUE.

/* create a new Workbook */
chWorkbook = chExcelApp:Workbooks:Add().

/* get the active Worksheet */
chWorkSheet = chExcelApp:Sheets:Item(1).

    FOR EACH TExcel NO-LOCK:
      E_Fila2     = "".
      E_Fila2     = "014" + STRING(TExcel.TE_ide,"X(14)")
                  + "003" + STRING(TExcel.TE_Age,"999")
                  + "003" + STRING(TExcel.TE_CC,"999")
                  + "010" + STRING(TExcel.TE_Doc,"X(10)")
                  /*+ "008" + STRING(TExcel.TE_Fecha,"99/99/99")*/
                  + "019" + STRING(TExcel.TE_Inicial,"->>,>>>,>>>,>>>,>>>")
                  + "019" + STRING(TExcel.TE_Debito,"->>,>>>,>>>,>>>,>>>")
                  + "019" + STRING(TExcel.TE_Credito,"->>,>>>,>>>,>>>,>>>")
                  + "019" + STRING(TExcel.TE_Final,"->>,>>>,>>>,>>>,>>>").

      {Incluido\imprimir_Excel.i}


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
ASSIGN lmonth = MONTH(W_Fecha)
         lyear = YEAR(W_Fecha)
         lday = DAY(W_Fecha).
  RUN SetDay.
  
  RUN SUPER.
  FOR EACH Agencias BREAK BY Agencias.Agencia:
      IF FIRST-OF(Agencias.Agencia) THEN DO:
          FIND FIRST Entidad WHERE Entidad.Entidad EQ Agencias.Entidad NO-LOCK NO-ERROR.
          IF AVAILABLE(Entidad) THEN DO:
             IF NOT Entidad.Id_CenCosto THEN
                DISABLE T_IdCenCostos Cmb_CenCostos1 WITH FRAME F_Filtros.
          END.
      END.
      W_Ok = Cmb_Agencia1:ADD-LAST(STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre) IN FRAME F_Filtros.
      W_Ok = Cmb_Agencia2:ADD-LAST(STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre) IN FRAME F_Filtros.
      IF Agencias.Agencia EQ W_Agencia THEN
          Cmb_Agencia1:SCREEN-VALUE IN FRAME F_Filtros = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.
  END.
  IF Entidad.Id_CenCosto THEN DO:
     FOR EACH Cen_Costos WHERE Cen_Costos.Agencia EQ INTEGER(SUBSTRING(Cmb_Agencia1,1,3)):
         W_Ok = Cmb_CenCostos1:ADD-LAST(STRING(Cen_Costos.Cen_Costos,"999") + " - " + Cen_Costos.Nombre).
         W_Ok = Cmb_CenCostos2:ADD-LAST(STRING(Cen_Costos.Cen_Costos,"999") + " - " + Cen_Costos.Nombre).
     END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ModbClkCta wWin 
PROCEDURE ModbClkCta :
DO WITH FRAME {&FRAME-NAME}:
     RUN C-Cuentas.r (OUTPUT W_Pcuenta, OUTPUT W_Pnombre, OUTPUT W_Naturaleza, OUTPUT W_CtrNat, INPUT "T").
     FIND Cuentas WHERE Cuentas.Cuenta EQ W_PCuenta
                    AND Cuentas.Tipo   EQ 2 
                    AND Cuentas.Estado EQ 1
                    NO-LOCK NO-ERROR NO-WAIT.
     IF AVAILABLE(Cuentas) THEN
         ASSIGN W_Cuenta:SCREEN-VALUE     = W_Pcuenta
                W_NomCuenta:SCREEN-VALUE  = W_Pnombre
                W_Cuenta                  = W_PCuenta.
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mov_Anexos wWin 
PROCEDURE Mov_Anexos :
EMPTY TEMP-TABLE tDetalle.
EMPTY TEMP-TABLE tMov.

FOR EACH Mov_Contable WHERE Mov_Contable.Agencia GE P_OfiIni
                        AND Mov_Contable.Agencia LE P_OfiFin
                        AND Mov_Contable.Cen_Costos GE cenCostos /*P_CenIni*/
                        AND Mov_Contable.Cen_Costos LE cenCostos /*P_CenFin*/
                        AND Mov_Contable.Cuenta EQ W_Cuenta
                        AND Mov_Contable.Nit EQ TAnexos.TA_Nit
                        AND YEAR(Mov_Contable.Fec_contable) EQ P_Ano
                        AND MONTH(Mov_Contable.Fec_contable) EQ P_Mes:

    CREATE TMov.
    ASSIGN TMov.TM_Age = Mov_Contable.Agencia
           TMov.TM_CCos = Mov_Contable.Cen_Costos
           TMov.TM_Cbte = Mov_Contable.Comprobante
           TMov.TM_Doc = Mov_Contable.Num_Documento
           TMov.TM_FecGra = Mov_Contable.Fec_Contable
           TMov.TM_NumDoc = Mov_Contable.Doc_Referencia
           TMov.TM_Coment = Mov_Contable.Comentario
           TMov.TM_Debito = Mov_Contable.DB
           TMov.TM_Credito = Mov_Contable.CR
           TMov.TM_Usuario = Mov_Contable.Usuario.
END.

OPEN QUERY Br_Mov FOR EACH TMov NO-LOCK INDEXED-REPOSITION.
REPOSITION Br_Mov TO ROW 1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mov_Cuenta wWin 
PROCEDURE Mov_Cuenta :
FOR EACH TAnexos: DELETE TAnexos. END.
 OPEN QUERY Br_Anexos FOR EACH TAnexos NO-LOCK INDEXED-REPOSITION.

 FOR EACH TDetalle: DELETE TDetalle. END.
 FOR EACH TMov: DELETE TMov. END.
 FOR EACH Mov_Contable WHERE Mov_Contable.Agencia    GE P_OfiIni    AND
                            Mov_Contable.Agencia    LE P_OfiFin    AND
                            Mov_Contable.Cen_Costos GE P_CenIni    AND
                            Mov_Contable.Cen_Costos LE P_CenFin    AND
                            Mov_Contable.Cuenta     EQ W_Cuenta    AND
                       YEAR(Mov_Contable.Fec_Grabacion) EQ P_Ano   AND
                      MONTH(Mov_Contable.Fec_Grabacion) EQ P_Mes:
    CREATE TMov.
    ASSIGN TMov.TM_Age        = Mov_Contable.Agencia
           TMov.TM_CCos       = Mov_Contable.Cen_Costos
           TMov.TM_Cbte       = Mov_Contable.Comprobante
           TMov.TM_Doc        = Mov_Contable.Num_Documento
           TMov.TM_FecGra     = Mov_Contable.Fec_Contable
           TMov.TM_NumDoc     = Mov_Contable.Doc_Referencia
           TMov.TM_Coment     = Mov_Contable.Comentario
           TMov.TM_Debito     = Mov_Contable.DB
           TMov.TM_Credito    = Mov_Contable.CR
           TMov.TM_Usuario    = Mov_Contable.Usuario.

 END.
 OPEN QUERY Br_Mov FOR EACH TMov NO-LOCK INDEXED-REPOSITION.
 REPOSITION Br_Mov TO ROW 1. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mov_Detalle wWin 
PROCEDURE Mov_Detalle :
EMPTY TEMP-TABLE tmov.

MESSAGE P_OfiIni P_OfiFin P_CenIni P_CenFin W_Cuenta TAnexos.TA_Nit
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

FOR EACH Mov_Contable WHERE Mov_Contable.Agencia GE P_OfiIni
                        AND Mov_Contable.Agencia LE P_OfiFin
                        AND Mov_Contable.Cen_Costos GE P_CenIni
                        AND Mov_Contable.Cen_Costos LE P_CenFin
                        AND Mov_Contable.Cuenta EQ W_Cuenta
                        AND Mov_Contable.Nit EQ TAnexos.TA_Nit
                        AND Mov_Contable.Doc_Referencia EQ TDetalle.TD_Doc NO-LOCK:
    CREATE TMov.
    ASSIGN TMov.TM_Age = Mov_Contable.Agencia
           TMov.TM_CCos = Mov_Contable.Cen_Costos
           TMov.TM_Cbte = Mov_Contable.Comprobante
           TMov.TM_Doc = Mov_Contable.Num_Documento
           TMov.TM_FecGra = Mov_Contable.Fec_Contable
           TMov.TM_NumDoc = Mov_Contable.Doc_Referencia
           TMov.TM_Coment = Mov_Contable.Comentario
           TMov.TM_Debito = Mov_Contable.DB
           TMov.TM_Credito = Mov_Contable.CR
           TMov.TM_Usuario = Mov_Contable.Usuario.
END.

OPEN QUERY Br_Mov FOR EACH TMov NO-LOCK INDEXED-REPOSITION.
REPOSITION Br_Mov TO ROW 1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir wWin 
PROCEDURE ProcesoImprimir :
{INCLUIDO\RepEncabezado.I}    
 ASSIGN W_Reporte    = "REPORTE   : CONSULTAS CONTABLES - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am")
        W_EncColumna = W_Cuenta + " - " + W_NomCuenta:SCREEN-VALUE IN FRAME F-Main.
  
 DEFINE VARIABLE WI_Mes      AS CHARACTER FORMAT "X(15)".
 DEFINE VARIABLE IDNomCli    AS CHARACTER FORMAT "X(50)".  
 

 IF B_Saldos:NUM-ITERATIONS IN FRAME F-Main GT 0 THEN DO:
    WI_Mes = STRING(IMes,"99") + " - " + INMes.
    DEFINE FRAME FI_Saldos 
    WITH WIDTH 132 FRAME FI_Saldos USE-TEXT STREAM-IO NO-LABELS.
 END.
     
 IF Br_Anexos:NUM-ITERATIONS IN FRAME F-Main GT 0 THEN DO:
    DEFINE FRAME FI_Anexos WITH WIDTH 132 FRAME FI_Anexos USE-TEXT STREAM-IO NO-LABELS.
 END.
    
    VIEW FRAME F-Encabezado.
    VIEW FRAME f-ftr.
    FOR EACH TSaldos WHERE TSaldos.TS_Mes EQ INTEGER(TSaldos.TS_Mes:SCREEN-VALUE IN BROWSE B_Saldos):
       DISPLAY  "Filtros Utilizados en la Consulta" AT 1
                "-------------------------------------------------------------------------------------------------------------" AT 1
                "Agencia Inicial         : "        AT 1
                P_OfiIni                            AT 27
                "Agencia Final           : "        AT 35
                P_OfiFin                            AT 70
                "Centro de Costos Inicial: "        AT 1
                P_CenIni                            AT 27
                "Centro de Costos Final  : "        AT 35
                P_CenFin                            AT 70
                "Fecha                   : "        AT 1
                WI_Mes                              AT 27 
                "-------------------------------------------------------------------------------------------------------------" AT 1 SKIP(1)
                "                               Saldo Inicial               Debito              Credito          Saldo Final" AT 1
                "(Saldos de Cuenta)"     AT 1
                TSaldos.TS_Inicial       AT 26
                TSaldos.TS_Debito        AT 47
                TSaldos.TS_Credito       AT 68
                TSaldos.TS_Final         AT 90
         WITH WIDTH 132 FRAME FI_Saldos USE-TEXT STREAM-IO NO-LABELS.
    END.
    IF Br_Anexos:NUM-ITERATIONS IN FRAME F-Main GT 0 THEN 
       DISPLAY "(Anexos de la cuenta)" AT 1
               "Age  Nit            C.C" AT 1 WITH WIDTH 80.
    FOR EACH TAnexos:
       DISPLAY TA_Agencia               AT 1
               TA_Nit                   AT 5
               TA_CenCos                AT 21
               TA_Inicial               AT 26
               TA_Debito                AT 47
               TA_Credito               AT 68
               TA_Final                 AT 90
       WITH WIDTH 132 FRAME FI_Anexos USE-TEXT STREAM-IO NO-LABELS NO-BOX.
       IF Cuentas.Id_Detalle THEN DO:
           FIND FIRST Clientes WHERE Clientes.Nit EQ TAnexos.TA_Nit NO-LOCK NO-ERROR.
           IF AVAILABLE(Clientes) THEN DO:
               IdNomCli = Clientes.Nit + " - " + TRIM(Clientes.Nombre) + " " + TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2).
               DISPLAY "Detalle Anexo:" AT 7
                        IDNomCli        AT 23
                       "        Age         C.C  Doct.Refer  Ult.Tran  Plazo           V.Inicial              Sdo.Final" AT 1 WITH WIDTH 100 NO-LABELS.
                              /*1         2         3         4         5         6         7         8         9
                     123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
                                */
           END.
           FOR EACH Detalle WHERE Detalle.Agencia  EQ  TAnexos.TA_Agencia AND 
                    Detalle.Cen_Costos             EQ  TAnexos.TA_CenCos  AND
                    Detalle.Nit                    EQ  TAnexos.TA_Nit     AND
                    Detalle.Cuenta                 EQ  W_Cuenta:
               IF Cuentas.Naturaleza EQ "DB" THEN
                  IDSdoFin         = Detalle.Db - Detalle.Cr.
               ELSE
                  IDSdoFin         = Detalle.Cr - Detalle.Db.
               DISPLAY Detalle.Agencia              AT 9
                       Detalle.Cen_Costos           AT 21
                       Detalle.Doc_referencia       AT 26
                       Detalle.Fec_ultActualizacion AT 38
                       Detalle.Plazo                AT 50
                       Detalle.Valor_Inicial        AT 57
                       IDSdoFin                     AT 81
               WITH WIDTH 132 FRAME FI_Detalle USE-TEXT STREAM-IO NO-LABELS NO-BOX.
           END.
           DISPLAY SKIP(1) WITH FRAME F_Espacio.
       END.

    END.
    
    
    PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Repos_Anexos wWin 
PROCEDURE Repos_Anexos :
FOR EACH TAnexos: DELETE TAnexos. END.
   RUN HallarSdoAnexos.
   OPEN QUERY BR_Anexos FOR EACH TAnexos NO-LOCK INDEXED-REPOSITION.
   REPOSITION BR_Anexos TO ROW 1.
   RUN Mov_Anexos.
   IF Cuentas.Id_Detalle THEN
      RUN Repos_Detalle.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Repos_Detalle wWin 
PROCEDURE Repos_Detalle :
EMPTY TEMP-TABLE tDetalle.

RUN HallarDetalle.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Saldos_de_Cuentas wWin 
PROCEDURE Saldos_de_Cuentas :
ASSIGN FRAME F_Filtros T_RangoAgencia.

ASSIGN P_OfiIni = INTEGER(SUBSTRING(Cmb_Agencia1:SCREEN-VALUE IN FRAME F_Filtros,1,3))
       P_OfiFin = P_OfiIni
       P_Ano = Lyear
       P_Mes = LMonth.

IF T_RangoAgencia THEN
    P_OfiFin = INTEGER(SUBSTRING(Cmb_Agencia2:SCREEN-VALUE IN FRAME F_Filtros,1,3)).

IF Entidad.Id_CenCosto AND T_IdCenCostos:SCREEN-VALUE IN FRAME F_Filtros EQ "yes" THEN DO:
    ASSIGN P_CenIni = INTEGER(SUBSTRING(Cmb_CenCostos1:SCREEN-VALUE IN FRAME F_Filtros,1,3))
           P_CenFin = P_CenIni.

    IF T_IdCenCostos THEN
        P_CenFin = INTEGER(SUBSTRING(Cmb_CenCostos2:SCREEN-VALUE IN FRAME F_Filtros,1,3)).
END.
ELSE
    ASSIGN P_CenIni = 0
           P_CenFin = 999.

RUN HallarSaldo.

ASSIGN IMes = TSaldos.TS_Mes
       INmes = TSaldos.TS_NMes
       IInicial = TSaldos.TS_Inicial
       IDebito = TSaldos.TS_Debito
       ICredito = TSaldos.TS_Credito
       IFinal = TSaldos.TS_Final.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetDay wWin 
PROCEDURE SetDay :
lweekday = WEEKDAY (DATE(lmonth,1,lyear)).
 lm = lmonth + 1.
 lyr = lyear.
 IF lm > 12 THEN DO:
    lm = 1.
    lyr =  lyr + 1.
 END.
 ASSIGN lastdate = (date(lm,1,lyr)) - 1
        lastday = day(lastdate)
        lstday = lweekday.
 IF lday >= 28 THEN DO:
    DO WHILE lastday < lday:
       lday = lday - 1.
    END.
 END.  /*lday >= 28*/  
 RUN dispdate.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

