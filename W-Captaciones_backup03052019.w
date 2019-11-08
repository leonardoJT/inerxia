&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
CREATE WIDGET-POOL.

/* ***************************  DEFnitions  ************************** */
{Incluido\VARIABLE.i "shared"}

DEFINE VAR W_NomEstado AS CHARACTER FORMAT "X(10)".
DEFINE VAR choice AS LOGICAL.
DEFINE VAR ROWID_mov AS ROWID.
DEFINE VAR Wk_Cambio1 AS CHARACTER.
DEFINE VAR Wk_Cambio2 AS CHARACTER.
DEFINE VAR Wk_Cambio3 AS CHARACTER.
DEFINE VAR Wk_DatAnt AS CHARACTER FORMAT "X(20)".
DEFINE VAR Wk_PerPagEmp AS INTEGER.
DEFINE VAR OK_Cancelar AS LOGICAL.
DEFINE VAR wconcep AS CHARACTER FORMAT "X(20)".
DEFINE VAR Wk_IdMonApe AS LOGICAL.
DEFINE VAR Wk_MontoApertura AS DECIMAL.
DEFINE VAR Wk_Cuota AS DECIMAL.

   /* oakley */

   DEF VAR T_Dd LIKE Operacion.Cod_Deducible.
   
   DEF VAR W_Ok AS LOG.
   DEF VAR TPdt LIKE Pro_Ahorros.Tip_Ahorro.
   DEF VAR NuevaCta AS LOG INIT NO.
   DEF VAR Puntero  AS ROWID.
   DEF VAR PunteroP AS ROWID.
   DEF VAR W_Inf       AS CHA FORMAT "X(10)".
   DEF VAR P_Nit       LIKE Clientes.Nit.
   DEF VAR p_Nombre    LIKE Clientes.Nombre.
   DEF VAR P_Apellido  LIKE Clientes.Apellido1.
   DEF VAR P_AgeCli    LIKE Clientes.Agencia.
   DEF VAR X           AS INT.
   DEF VAR W_DigChe    AS INT FORMAT ">>9".
   DEF VAR W_CueAho    LIKE Ahorros.Cue_Ahorros.
   DEF VAR W_tasa      LIKE Indicadores.Tasa.
   DEF VAR W_Puntos    LIKE Ran_Intereses.Puntos.
   DEF VAR W_Error     AS LOG.
   DEF VAR W_Autorizo  LIKE Usuarios.Usuario.
   DEF VAR MaximoPuntos AS DEC FORMAT ">>,>>9.9999999".
   /*vaiables para debito de libreta*/
   DEF VAR W_Total     LIKE Ahorros.Sdo_Disponible.
   DEF VAR W_Operacion LIKE Operacion.Cod_Operacion.
   DEF VAR W_Cbte      LIKE Comprobantes.Secuencia.
   DEF VAR W_CtaOpe    LIKE Cuentas.Cuenta.
   DEF VAR W_CtaCor    LIKE Cuentas.Cuenta.
   DEF VAR W_CodDed    LIKE Operacion.Cod_Deducible.
   DEF VAR W_Comision  LIKE Operacion.Comision.
   DEF VAR W_DedNom    AS CHA FORMAT "X(20)".
   DEF VAR W_DedCta    LIKE Cuentas.Cuenta.
   DEF VAR W_DedCla    LIKE Deducible.Cla_Deducible.
   DEF VAR W_DedVal    LIKE Deducible.Valor.
   DEF VAR W_DedValImp LIKE Deducible.Valor_Impuesto.
   DEF VAR W_DedCtaImp LIKE Cuentas.Cuenta.
   DEF VAR W_ComNom    AS CHA FORMAT "X(20)".
   DEF VAR W_ComCta    LIKE Cuentas.Cuenta.
   DEF VAR W_ComCla    LIKE Deducible.Cla_Deducible.
   DEF VAR W_ComVal    LIKE Deducible.Valor.
   DEF VAR W_ComValImp LIKE Deducible.Valor_Impuesto.
   DEF VAR W_ComCtaImp LIKE Cuentas.Cuenta.
   /*fin vbls para debito libreta*/

   DEF VAR WEdad AS INT NO-UNDO FORMAT "ZZ".
   DEF BUFFER Rela_Clientes FOR Clientes.
 
  /*para buscar cuentas destino y cuentas debito automatico*/
  DEF VAR W_Age  LIKE Ahorros.Agencia.
  DEF VAR W_Pro  LIKE Ahorros.Cod_Ahorro.
  DEF VAR W_Nit  LIKE Ahorros.Nit.
  DEF VAR W_Cue  LIKE Ahorros.Cue_Ahorros.

  /*validacion de numeros inicial y final de las libretas*/
  DEF VAR W_NIni LIKE Lib_Chequera.Num_Inicial.
  DEF VAR W_NFin LIKE Lib_Chequera.Num_Inicial.

    /*manejo de las relaciones*/
 DEF TEMP-TABLE T_Relaciones
     FIELD R_Relacion   LIKE Varios.Descripcion
     FIELD R_AgeObjeto  LIKE Clientes.Agencia
     FIELD R_NitObjeto  LIKE Clientes.Nit
     FIELD R_NomObjeto  AS CHA FORMAT "X(35)"
     FIELD R_TelObjeto  AS CHA FORMAT "X(30)"
     FIELD R_NomDescri  AS CHA FORMAT "X(15)"
     FIELD R_VrAutoriz  LIKE Relaciones.Val_Autoriz INIT 0
     FIELD R_ClasePdt   AS INT FORMAT "9"
     FIELD R_CodPdt     AS INT FORMAT "999"
     FIELD R_CuePdt     AS CHA FORMAT "X(14)"
     FIELD Est          AS INTEG FORM "9".
/*vlbles plant*/
    DEF VAR W_Per              AS DEC FORMAT "999.9999999".
    DEF VAR W_Pla              AS INT FORMAT "99999".  
    DEF VAR W_DiaPer           AS INT FORMAT "9999".
    DEF VAR W_TasPer           AS DEC FORMAT ">>9.99".
    DEF VAR W_TasaWork         AS DEC FORMAT ">>9.99".  
    DEF VAR W_PerLiqui         AS INT FORMAT "99".
    DEF VAR W_Rpta             AS LOG.
    DEF VAR W-Procedure        AS CHA.
    DEF VAR P_TipoAsociado     AS LOG.
    DEF VAR W_NomCliWork       AS CHA FORMAT "X(40)".
    DEF VAR W_Fec              AS DATE FORMAT "99/99/9999".  
    DEF VAR W_Mes              AS INT.
    DEF VAR I                  AS INT.  
    DEF VAR W_TipExtra         AS CHA FORMAT "X".

    DEF VAR W_PunNegociables LIKE Ran_Intereses.Pun_Negociables.
    DEF VAR W_Nit1           LIKE Ahorros.Nit.
    DEF VAR W_TipProdWork    LIKE Pro_Ahorros.Tip_Ahorro.
    DEF VAR W_CodProdWork    LIKE Pro_Ahorros.Cod_Ahorro.
    DEF VAR W_NomProdWork    LIKE Pro_Ahorros.Nom_Producto.
    DEF VAR W_BasRet         LIKE Pro_Creditos.Val_Montomaximo.
    DEF VAR W_CodBase        LIKE Liqui_Int.Cod_base. 
    DEF VAR W_Valor          LIKE Ahorros.Monto_Apertura.
    DEF VAR W_Cap            LIKE Ahorros.Sdo_Disponible INIT 0.
    DEF VAR W_Int            LIKE Ahorros.Sdo_Disponible INIT 0.
    DEF VAR W_IntTot         LIKE Ahorros.Sdo_Disponible INIT 0.
    DEF VAR W_Tot            LIKE Ahorros.Sdo_Disponible INIT 0.
    DEF VAR W_TotGen         LIKE Ahorros.Sdo_Disponible INIT 0.
    DEF VAR W_IntGen         LIKE Ahorros.Sdo_Disponible INIT 0.
    DEF VAR W_IntRet         LIKE Ahorros.Sdo_Disponible INIT 0.
    DEF VAR W_Ret            LIKE Ahorros.Sdo_Disponible INIT 0.
    DEF VAR W_porce          LIKE Base_Ret.Porcentaje.
    DEF VAR W_CancTarDB      AS LOG INIT FALSE.
    DEF TEMP-TABLE TmpAho
           FIELD T_Fecha   LIKE Ahorros.Fec_Apertura
           FIELD T_Valor   LIKE Ahorros.Cuota
           FIELD T_Extra     AS CHA FORMAT "X"
           FIELD T_Capital LIKE Ahorros.Monto_Apertura
           FIELD T_Interes   AS DEC FORMAT ">>>,>>>,>>9.99"
           FIELD T_IntTot    AS DEC FORMAT ">>>,>>>,>>9.99"
           FIELD T_Reten     AS DEC FORMAT ">,>>>,>>9.99".
    DEF TEMP-TABLE TAhorros NO-UNDO LIKE Ahorros.
    DEF VAR vlNewRel  AS LOG NO-UNDO. /* Nueva relacion? */
    DEF VAR vdeValIni AS DEC NO-UNDO.
    DEF VAR vlTpAct   AS LOG NO-UNDO. /* identificar si cliente es independiente */
    DEF BUFFER BAhorros FOR Ahorros.
    DEF VAR chWordApplication AS COM-HANDLE NO-UNDO.
    DEF VAR W_TasNom LIKE Ahorros.Tasa.
    DEF VAR Fecha_GMF AS DATE FORMAT "99/99/9999" NO-UNDO EXTENT 2.
    DEF VAR SwError   AS logi NO-UNDO.
/*Variables Para Imprimir en Excel*/
  DEF VAR Dato         AS CHA NO-UNDO.
  DEF VAR ValCol       AS CHA NO-UNDO.
  DEF VAR SwExiste     AS CHA NO-UNDO.
  DEF VAR InputFile    AS CHA NO-UNDO.
  DEF VAR PrinterName  AS CHA NO-UNDO.
  DEF VAR TasaEfectiva AS DEC NO-UNDO.
  DEF VAR chExcelApp   AS COM-HANDLE NO-UNDO.
  DEF VAR hWorkBooks   AS COM-HANDLE NO-UNDO.
  DEF VAR chWorksheet  AS COM-HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Ahorros
&Scoped-define BROWSE-NAME BR_Libretas

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Lib_Chequera T_Relaciones Ahorros

/* Definitions for BROWSE BR_Libretas                                   */
&Scoped-define FIELDS-IN-QUERY-BR_Libretas Lib_Chequera.Tip_Talonario Lib_Chequera.Num_Inicial Lib_Chequera.Num_Final Lib_Chequera.Pagada W_NomEstado   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BR_Libretas   
&Scoped-define SELF-NAME BR_Libretas
&Scoped-define QUERY-STRING-BR_Libretas FOR EACH Lib_Chequera WHERE    Lib_Chequera.Cod_Producto EQ Ahorros.Cod_Ahorro AND    Lib_Chequera.Cue_Ahorros EQ Ahorros.Cue_Ahorros    NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BR_Libretas OPEN QUERY {&SELF-NAME} FOR EACH Lib_Chequera WHERE    Lib_Chequera.Cod_Producto EQ Ahorros.Cod_Ahorro AND    Lib_Chequera.Cue_Ahorros EQ Ahorros.Cue_Ahorros    NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BR_Libretas Lib_Chequera
&Scoped-define FIRST-TABLE-IN-QUERY-BR_Libretas Lib_Chequera


/* Definitions for BROWSE Br_Relaciones                                 */
&Scoped-define FIELDS-IN-QUERY-Br_Relaciones T_Relaciones.R_Relacion T_Relaciones.R_AgeObjeto T_Relaciones.R_NitObjeto T_Relaciones.R_NomObjeto T_Relaciones.R_NomDescri T_Relaciones.R_VrAutoriz T_Relaciones.R_TelObjeto T_Relaciones.Est   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Relaciones   
&Scoped-define SELF-NAME Br_Relaciones
&Scoped-define QUERY-STRING-Br_Relaciones FOR EACH T_Relaciones NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br_Relaciones OPEN QUERY {&SELF-NAME} FOR EACH T_Relaciones NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_Relaciones T_Relaciones
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Relaciones T_Relaciones


/* Definitions for FRAME F_Ahorros                                      */
&Scoped-define SELF-NAME F_Ahorros
&Scoped-define QUERY-STRING-F_Ahorros FOR EACH Ahorros WHERE Ahorros.Agencia EQ 0 NO-LOCK
&Scoped-define OPEN-QUERY-F_Ahorros OPEN QUERY {&SELF-NAME} FOR EACH Ahorros WHERE Ahorros.Agencia EQ 0 NO-LOCK.
&Scoped-define TABLES-IN-QUERY-F_Ahorros Ahorros
&Scoped-define FIRST-TABLE-IN-QUERY-F_Ahorros Ahorros


/* Definitions for FRAME F_Contractual                                  */
&Scoped-define ENABLED-FIELDS-IN-QUERY-F_Contractual Ahorros.porce_Aporte 
&Scoped-define ENABLED-TABLES-IN-QUERY-F_Contractual Ahorros
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-F_Contractual Ahorros
&Scoped-define QUERY-STRING-F_Contractual FOR EACH Ahorros SHARE-LOCK
&Scoped-define OPEN-QUERY-F_Contractual OPEN QUERY F_Contractual FOR EACH Ahorros SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-F_Contractual Ahorros
&Scoped-define FIRST-TABLE-IN-QUERY-F_Contractual Ahorros


/* Definitions for FRAME F_Libretas                                     */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Libretas ~
    ~{&OPEN-QUERY-BR_Libretas}

/* Definitions for FRAME F_Relaciones                                   */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Relaciones ~
    ~{&OPEN-QUERY-Br_Relaciones}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Ahorros.Exento_3xm ~
Ahorros.Id_Subsidio_Entidad Ahorros.Ind_Tipo_Subsidio ~
Ahorros.Id_Tesoro_Nacional Ahorros.Id_Mesada_Pensional Ahorros.Sdo_MinCta ~
Ahorros.Nit_Tutor Ahorros.Estado 
&Scoped-define ENABLED-TABLES Ahorros
&Scoped-define FIRST-ENABLED-TABLE Ahorros
&Scoped-Define ENABLED-OBJECTS T-ExeCobro tgl-TarjetaDB Tg_Tasa BUTTON-1 ~
BUTTON-2 Btn_Consulta Cmb_PerLiquidacion Btn_Ingresar BtnDone BUTTON-95 ~
Btn_Libretas BUTTON-92 Imp_Cdat wcomenTdb Imp_UtraMas BUTTON-123 RECT-274 ~
RECT-275 RECT-281 RECT-284 RECT-315 RECT-317 RECT-318 RECT-319 RECT-334 ~
RECT-335 
&Scoped-Define DISPLAYED-FIELDS Ahorros.Nit Ahorros.Exento_3xm ~
Ahorros.Fec_Activacion[1] Ahorros.Fec_DesActivacion[1] ~
Ahorros.Id_Subsidio_Entidad Ahorros.Ind_Tipo_Subsidio ~
Ahorros.Fec_Activacion[2] Ahorros.Fec_DesActivacion[2] ~
Ahorros.Id_Tesoro_Nacional Ahorros.Fec_Activacion[3] ~
Ahorros.Fec_DesActivacion[3] Ahorros.Id_Mesada_Pensional ~
Ahorros.Fec_Activacion[4] Ahorros.Fec_DesActivacion[4] Ahorros.Cue_Ahorros ~
Ahorros.Num_Formato Ahorros.Sdo_MinCta Ahorros.Id_Tutor Ahorros.Nit_Tutor ~
Ahorros.TarjetaDB Ahorros.For_Liquidacion Ahorros.Tasa Ahorros.Estado 
&Scoped-define DISPLAYED-TABLES Ahorros
&Scoped-define FIRST-DISPLAYED-TABLE Ahorros
&Scoped-Define DISPLAYED-OBJECTS Cmb_TipoProductos Cmb_Productos ~
W_NomTitular T-ExeCobro Nombre_Tutor tgl-TarjetaDB Tg_Tasa W_Digito ~
Cmb_PerLiquidacion Tasa_Efectiva Tasa_Nominal Interes_PorPagar Cmb_Estados ~
wcomenTdb 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 Cmb_TipoProductos Cmb_Productos Interes_PorPagar 
&Scoped-define List-4 Btn_IngLib BTN_CanLib BTN_SalLib BTN_MosLib ~
BUTTON-121 
&Scoped-define List-6 BUTTON-1 BUTTON-2 Btn_Consulta Btn_Salvar ~
Btn_Deshacer Btn_Ingresar Btn_Cancelar BtnDone BUTTON-95 Btn_Autorizados ~
Btn_Libretas BUTTON-92 BUTTON-123 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Salir" 
     SIZE 10 BY 1.62
     BGCOLOR 8 FONT 4.

DEFINE BUTTON Btn_Autorizados 
     LABEL "Beneficiarios/Autorizados" 
     SIZE 20 BY 1.38.

DEFINE BUTTON Btn_Borrar 
     LABEL "&Borrar" 
     SIZE 10 BY 1.65
     FONT 4.

DEFINE BUTTON Btn_Cancelar 
     LABEL "&Cancelar" 
     SIZE 10 BY 1.65
     FONT 4.

DEFINE BUTTON Btn_Consulta 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "Button 3" 
     SIZE 10 BY 1.62.

DEFINE BUTTON Btn_Deshacer 
     LABEL "&Deshacer" 
     SIZE 10 BY 1.65
     FONT 4.

DEFINE BUTTON Btn_Ingresar 
     LABEL "&Nuevos" 
     SIZE 10 BY 1.62
     FONT 4.

DEFINE BUTTON Btn_Libretas 
     LABEL "Libretas/Chequeras" 
     SIZE 18 BY 1.38.

DEFINE BUTTON Btn_Salvar 
     LABEL "&Grabar" 
     SIZE 10 BY 1.62
     FONT 4.

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/informacion.bmp":U
     LABEL "Button 1" 
     SIZE 10 BY 1.62.

DEFINE BUTTON BUTTON-123 
     IMAGE-UP FILE "imagenes/combo.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/arwdwn.gif":U
     LABEL "Button 123" 
     SIZE 3 BY .81 TOOLTIP "Imprime Copia del Formato GMF AV-303".

DEFINE BUTTON BUTTON-2 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/impresora2.bmp":U
     LABEL "Button 2" 
     SIZE 10 BY 1.62.

DEFINE BUTTON BUTTON-92 
     LABEL "Fechas y Saldos" 
     SIZE 18 BY 1.38.

DEFINE BUTTON BUTTON-95 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 95" 
     SIZE 4 BY 1.12.

DEFINE BUTTON Imp_Cdat 
     LABEL "Impresion CDAT" 
     SIZE 15 BY 1.38.

DEFINE BUTTON Imp_UtraMas 
     LABEL "Impresion Titulo" 
     SIZE 15 BY 1.38.

DEFINE VARIABLE Cmb_Estados AS CHARACTER FORMAT "X(256)":U 
     LABEL "Estado" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 26.72 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_PerLiquidacion AS CHARACTER FORMAT "X(25)":U INITIAL "01 - Diario" 
     LABEL "Periodo de Liquidacion" 
     VIEW-AS COMBO-BOX INNER-LINES 6
     LIST-ITEMS "01 - Diario","02 - Mensual","03 - Trimestral","04 - Semestral","05 - Anual","06 - Al Vencimiento" 
     DROP-DOWN-LIST
     SIZE 22 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_Productos AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 68 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE Cmb_TipoProductos AS CHARACTER FORMAT "X(256)":U INITIAL "A la Vista" 
     LABEL "Productos" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "1 - A La Vista","2 - A Programado","3 - A Termino","4 - Aportes","5 - Convenios" 
     DROP-DOWN-LIST
     SIZE 21.29 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE Interes_PorPagar AS DECIMAL FORMAT ">>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Interés x Pagar" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Nombre_Tutor AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 71.14 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Tasa_Efectiva AS DECIMAL FORMAT ">>9.9999999":U INITIAL 0 
     LABEL "Efectiva Anual" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Tasa_Nominal AS DECIMAL FORMAT ">>9.9999999":U INITIAL 0 
     LABEL "Tasa del Pdo." 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE wcomenTdb AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 40.14 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Digito AS CHARACTER FORMAT "X(1)":U 
     VIEW-AS FILL-IN 
     SIZE 3 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomTitular AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 68 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-274
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 12 BY 5.38.

DEFINE RECTANGLE RECT-275
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 99.29 BY 2.15.

DEFINE RECTANGLE RECT-281
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 48 BY 2.15.

DEFINE RECTANGLE RECT-284
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 48 BY 7.

DEFINE RECTANGLE RECT-315
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 21.43 BY 1.04.

DEFINE RECTANGLE RECT-317
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 71 BY 5.27.

DEFINE RECTANGLE RECT-318
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 27 BY 5.27.

DEFINE RECTANGLE RECT-319
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 99 BY 2.19.

DEFINE RECTANGLE RECT-334
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 10.57 BY 1.81.

DEFINE RECTANGLE RECT-335
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 67.14 BY 1.69.

DEFINE VARIABLE T-ExeCobro AS LOGICAL INITIAL no 
     LABEL "Cuenta Nómina" 
     VIEW-AS TOGGLE-BOX
     SIZE 13 BY .77 NO-UNDO.

DEFINE VARIABLE tgl-TarjetaDB AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 2 BY .81 NO-UNDO.

DEFINE VARIABLE Tg_Tasa AS LOGICAL INITIAL no 
     LABEL "Modif.Tasa CDAT" 
     VIEW-AS TOGGLE-BOX
     SIZE 19.43 BY .58 NO-UNDO.

DEFINE BUTTON Bt_DebitoAP 
     LABEL "Buscar C/ta Debito" 
     SIZE 18 BY 1.12.

DEFINE VARIABLE AP_AgenciaDebito AS CHARACTER FORMAT "X(50)":U 
     LABEL "Agencia" 
     VIEW-AS FILL-IN 
     SIZE 37.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE AP_CuentaDebito AS CHARACTER FORMAT "X(14)":U 
     LABEL "Cuenta" 
     VIEW-AS FILL-IN 
     SIZE 37.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE AP_FechaDebito AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Débito" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE AP_NombreDebito AS CHARACTER FORMAT "X(50)":U 
     LABEL "Nombre" 
     VIEW-AS FILL-IN 
     SIZE 38 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE AP_ProductoDebito AS CHARACTER FORMAT "X(50)":U 
     LABEL "Producto" 
     VIEW-AS FILL-IN 
     SIZE 37.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE AV_Cuota AS DECIMAL FORMAT ">>,>>>,>>9.99":U INITIAL 0 
     LABEL "Cuota" 
     VIEW-AS FILL-IN 
     SIZE 11.14 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE AV_Monto AS DECIMAL FORMAT "->>>,>>>,>>9.99" INITIAL 0 
     LABEL "Monto Apertura" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE AV_Puntos AS DECIMAL FORMAT "->9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY .81
     BGCOLOR 17  NO-UNDO.

DEFINE VARIABLE AV_VerTasa AS CHARACTER FORMAT "X(40)":U INITIAL "Digite el Monto para Ver la Tasa" 
     VIEW-AS FILL-IN 
     SIZE 32 BY .81
     FGCOLOR 7  NO-UNDO.

DEFINE VARIABLE AP_FormaPago AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Caja", 1,
"Nómina", 2,
"Débito Automático", 3
     SIZE 41 BY .81 NO-UNDO.

DEFINE VARIABLE AP_PeriodoPago AS INTEGER INITIAL 4 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Semanal", 1,
"Decadal", 2,
"Quincenal", 3,
"Mensual", 4
     SIZE 47 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-280
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 49 BY 5.65.

DEFINE RECTANGLE RECT-283
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 49 BY 1.73.

DEFINE BUTTON BT_DesAT 
     LABEL "Buscar Cuenta / Pagare" 
     SIZE 24 BY 1.08.

DEFINE VARIABLE AT_AgenciaDestino AS CHARACTER FORMAT "X(30)":U 
     LABEL "Agencia" 
     VIEW-AS FILL-IN 
     SIZE 29 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE AT_CuentaDestino AS CHARACTER FORMAT "X(14)":U 
     LABEL "Cuenta Destino" 
     VIEW-AS FILL-IN 
     SIZE 29.14 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE AT_Monto AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Monto Inicial" 
     VIEW-AS FILL-IN 
     SIZE 16.14 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE AT_NombreDestino AS CHARACTER FORMAT "X(50)":U 
     LABEL "Nombre" 
     VIEW-AS FILL-IN 
     SIZE 29 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE AT_Plazo AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Plazo en Dìas" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE AT_ProductoDestino AS CHARACTER FORMAT "X(80)":U 
     LABEL "Producto" 
     VIEW-AS FILL-IN 
     SIZE 29 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE AT_Puntos AS DECIMAL FORMAT ">9.99":U INITIAL 0 
     LABEL "Puntos" 
     VIEW-AS FILL-IN 
     SIZE 6.86 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE AT_VerTasa AS CHARACTER FORMAT "X(50)":U INITIAL "Debe Digitar el Monto y el Plazo para ver la Tasa" 
     VIEW-AS FILL-IN 
     SIZE 43 BY .81
     FGCOLOR 7  NO-UNDO.

DEFINE VARIABLE F-FecVenc AS DATE FORMAT "99/99/9999":U 
     LABEL "Fec. Vencim." 
     VIEW-AS FILL-IN 
     SIZE 16 BY .81 TOOLTIP "Digite Fecha Vencimiento"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE AT_Destino_Intereses AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Ahorros", 1,
"Crédito", 2,
"Cuentas X Pagar", 3
     SIZE 45 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-277
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 48 BY 5.88.

DEFINE BUTTON Bt_Debito 
     LABEL "Buscar Cuenta Debito" 
     SIZE 18 BY .81.

DEFINE VARIABLE CT_AgenciaDebito AS CHARACTER FORMAT "X(50)":U 
     LABEL "Agencia" 
     VIEW-AS FILL-IN 
     SIZE 37.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE CT_CuentaDebito AS CHARACTER FORMAT "X(14)":U 
     LABEL "Cuenta" 
     VIEW-AS FILL-IN 
     SIZE 37.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE CT_Cuota AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Cuota" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE CT_FechaDebito AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Débito" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE CT_Monto AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Monto de Apertura" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE CT_NombreDebito AS CHARACTER FORMAT "X(50)":U 
     LABEL "Nombre" 
     VIEW-AS FILL-IN 
     SIZE 38 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE CT_Plazo AS DECIMAL FORMAT "9999":U INITIAL 0 
     LABEL "Plazo en Días" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE CT_ProductoDebito AS CHARACTER FORMAT "X(50)":U 
     LABEL "Producto" 
     VIEW-AS FILL-IN 
     SIZE 37.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE CT_Puntos AS DECIMAL FORMAT "->9.99":U INITIAL 0 
     LABEL "Puntos" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-AporOblig AS DECIMAL FORMAT ">>>,>>9.99":U INITIAL 0 
     LABEL "Total Aporte" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-PorApoSue AS DECIMAL FORMAT ">9.99":U INITIAL 0 
     LABEL "% Aporte respecto al sueldo" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-Salario AS DECIMAL FORMAT ">>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Salario" 
     VIEW-AS FILL-IN 
     SIZE 13.72 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE CT_FormaPago AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Caja", 1,
"Conv. Libranza", 2,
"Débito Automático", 3
     SIZE 41 BY .69 NO-UNDO.

DEFINE VARIABLE CT_PeriodoPago AS INTEGER INITIAL 4 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Semanal", 1,
"Decadal", 2,
"Quincenal", 3,
"Mensual", 4
     SIZE 47 BY .65 NO-UNDO.

DEFINE RECTANGLE RECT-278
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 49 BY 5.35.

DEFINE BUTTON BTN_CanLib 
     LABEL "Cancelar" 
     SIZE 15 BY 1.12.

DEFINE BUTTON Btn_IngLib 
     LABEL "Ingresar" 
     SIZE 15 BY 1.08.

DEFINE BUTTON BTN_MosLib 
     LABEL "Mostrar Todos" 
     SIZE 15 BY 1.08.

DEFINE BUTTON BTN_SalLib 
     LABEL "Salvar" 
     SIZE 15 BY 1.08.

DEFINE BUTTON BUTTON-121 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 121" 
     SIZE 9 BY 1.62.

DEFINE VARIABLE W_AccionLibreta AS CHARACTER FORMAT "X(50)":U INITIAL "Talonario Activo al Momento" 
      VIEW-AS TEXT 
     SIZE 34 BY .62
     BGCOLOR 18 FGCOLOR 15 FONT 5 NO-UNDO.

DEFINE RECTANGLE RECT-285
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 1.62.

DEFINE BUTTON Btn_Activas 
     LABEL "Inactivar" 
     SIZE 15 BY .81.

DEFINE BUTTON Btn_CanRel 
     LABEL "Cancelar" 
     SIZE 15 BY .81.

DEFINE BUTTON Btn_CreRel 
     LABEL "Crear" 
     SIZE 15 BY .81.

DEFINE BUTTON Btn_Outbene 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Btn_outbene" 
     SIZE 6 BY 1.35.

DEFINE BUTTON Btn_SalRel 
     LABEL "Salvar" 
     SIZE 12.86 BY .81.

DEFINE VARIABLE Cmb_TRF AS CHARACTER FORMAT "X(256)":U 
     LABEL "TRF" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 41 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE R_Relacion2 AS CHARACTER 
     LABEL "Relación O Parentesco" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 30
     LIST-ITEMS "Abuelo(a)","Bisabuelo(a)","Bisnieto(a)","Conyuge","Compañero(a)","Cuñado(a)","Hermano(a)","Hermano(a) del Abuelo(a)","Hijo(a)","Hijo(a) Adoptivo(a)","Hijastro(a)","Hijo(a) del Conyuge","Hijo(a) del Compañero(a)","Madre","Madre Adoptante","Madrastra","Nieto(a)","Nuera","Padre","Padre Adoptante","Padrastro","Primo(a)","Primo(a) Hermano(a)","Sobrino(a)","Suegro(a)","Tatarabuelo(a)","Tataranieto(a)","Tio(a)","Yerno" 
     DROP-DOWN MAX-CHARS 20
     SIZE 20 BY 1 TOOLTIP "Parentesco O Relación"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Lb_Nit AS CHARACTER FORMAT "X(15)":U INITIAL "Nit Relacion:" 
     VIEW-AS FILL-IN 
     SIZE 9.86 BY .81
     BGCOLOR 17 FGCOLOR 0 FONT 4 NO-UNDO.

DEFINE VARIABLE Nit_Relacion AS CHARACTER FORMAT "X(14)":U 
     VIEW-AS FILL-IN 
     SIZE 13.72 BY .81
     BGCOLOR 17 FGCOLOR 0 FONT 5 NO-UNDO.

DEFINE VARIABLE W-DescTRF AS CHARACTER FORMAT "X(80)":U 
     LABEL "Notas TRF" 
     VIEW-AS FILL-IN 
     SIZE 75 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_NomNitRelacion AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 40.86 BY .81
     BGCOLOR 17 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_VrAutoriz AS DECIMAL FORMAT ">>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.86 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R-reclamo AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Reclama Todos Los Titulares", 1,
"Reclama Cualesquiera de los Titulares", 2
     SIZE 54 BY .54 NO-UNDO.

DEFINE VARIABLE RActivas AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Activas", 1,
"Inactivas", 2
     SIZE 19.57 BY .81 NO-UNDO.

DEFINE VARIABLE R_Eleccion AS INTEGER INITIAL 7 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Beneficiario", 4,
"Autorizado", 7
     SIZE 27 BY .92
     FONT 5 NO-UNDO.

DEFINE BUTTON BUTTON-122 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 122" 
     SIZE 5 BY 1.35.

DEFINE VARIABLE W_NomUsuario AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 43 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-303
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 56 BY 1.35.

DEFINE RECTANGLE RECT-304
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 56 BY 1.35.

DEFINE RECTANGLE RECT-305
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 56 BY 2.42.

DEFINE RECTANGLE RECT-306
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 56 BY 1.35.

DEFINE RECTANGLE RECT-307
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 1.38.

DEFINE RECTANGLE RECT-308
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 37 BY 6.5.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BR_Libretas FOR 
      Lib_Chequera SCROLLING.

DEFINE QUERY Br_Relaciones FOR 
      T_Relaciones SCROLLING.

DEFINE QUERY F_Ahorros FOR 
      Ahorros SCROLLING.

DEFINE QUERY F_Contractual FOR 
      Ahorros SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BR_Libretas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BR_Libretas wWin _FREEFORM
  QUERY BR_Libretas NO-LOCK DISPLAY
      Lib_Chequera.Tip_Talonario FORMAT "9":U
      Lib_Chequera.Num_Inicial FORMAT "99999999":U
      Lib_Chequera.Num_Final FORMAT "99999999":U
      Lib_Chequera.Pagada FORMAT "yes/no":U
      W_NomEstado FORMAT "X(10)":U COLUMN-LABEL "Estado"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 86 BY 4.58
         BGCOLOR 15 FONT 4 FIT-LAST-COLUMN.

DEFINE BROWSE Br_Relaciones
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Relaciones wWin _FREEFORM
  QUERY Br_Relaciones NO-LOCK DISPLAY
      T_Relaciones.R_Relacion  FORMAT "X(30)" LABEL "Relacion"
      T_Relaciones.R_AgeObjeto FORMAT "999"   LABEL "Age"
      T_Relaciones.R_NitObjeto FORMAT "X(14)" LABEL "Nit"
      T_Relaciones.R_NomObjeto FORMAT "X(35)" LABEL "Nombre"
      T_Relaciones.R_NomDescri FORMAT "X(15)" LABEL "Descripción"
      T_Relaciones.R_VrAutoriz LABEL "Vr-Autorizado"
      T_Relaciones.R_TelObjeto FORMAT "X(14)" LABEL "Tel.Residencia"
      T_Relaciones.Est         LABEL "Est"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 96.14 BY 3
         BGCOLOR 15 FONT 4 ROW-HEIGHT-CHARS .65 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Relaciones
     R_Eleccion AT ROW 1 COL 3.43 NO-LABEL
     Cmb_TRF AT ROW 1.12 COL 33 COLON-ALIGNED
     RActivas AT ROW 1.19 COL 78.14 NO-LABEL
     W-DescTRF AT ROW 2.04 COL 19.14 COLON-ALIGNED
     Btn_CreRel AT ROW 3.04 COL 12.72
     Btn_CanRel AT ROW 3.04 COL 30.14
     Btn_Activas AT ROW 3.04 COL 47.14
     Btn_SalRel AT ROW 3.04 COL 64.14
     Lb_Nit AT ROW 4 COL 1.29 COLON-ALIGNED NO-LABEL
     Nit_Relacion AT ROW 4 COL 11.29 COLON-ALIGNED NO-LABEL
     W_NomNitRelacion AT ROW 4 COL 25.14 COLON-ALIGNED NO-LABEL
     W_VrAutoriz AT ROW 4.04 COL 78.86 COLON-ALIGNED NO-LABEL
     R-reclamo AT ROW 5.12 COL 3 NO-LABEL
     Br_Relaciones AT ROW 5.96 COL 3
     Btn_Outbene AT ROW 9.23 COL 93.43
     R_Relacion2 AT ROW 9.35 COL 18 COLON-ALIGNED WIDGET-ID 294
     "Monto Autorizado" VIEW-AS TEXT
          SIZE 12.86 BY .5 AT ROW 3.35 COL 81.72
          FGCOLOR 0 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 11.77
         SIZE 99.14 BY 10.42
         BGCOLOR 17 FGCOLOR 0 FONT 4
         TITLE "Beneficiarios y Autorizados".

DEFINE FRAME F_Libretas
     Lib_Chequera.Num_Inicial AT ROW 2.08 COL 41 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY .81
          BGCOLOR 18 FGCOLOR 15 FONT 4
     Lib_Chequera.Num_Final AT ROW 2.08 COL 60 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Lib_Chequera.Fec_Entrega AT ROW 2.08 COL 84 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Lib_Chequera.Tip_Talonario AT ROW 2.19 COL 5 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Libreta", 1,
"Chequera", 2
          SIZE 22 BY .81
     Btn_IngLib AT ROW 3.42 COL 3
     BTN_CanLib AT ROW 3.42 COL 20
     BTN_SalLib AT ROW 3.42 COL 37
     BTN_MosLib AT ROW 3.42 COL 54
     BR_Libretas AT ROW 4.77 COL 3
     BUTTON-121 AT ROW 7.69 COL 90
     W_AccionLibreta AT ROW 1.27 COL 3 COLON-ALIGNED NO-LABEL
     RECT-285 AT ROW 1.54 COL 3
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 11.12
         SIZE 98.86 BY 9.62
         BGCOLOR 16 FONT 4
         TITLE "Libretas y Chequeras".

DEFINE FRAME F_Secundaria
     Ahorros.Fec_Apertura AT ROW 1.54 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Ahorros.Num_DepDia AT ROW 1.54 COL 54 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 3.57 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Ahorros.Val_DepDia AT ROW 1.54 COL 80 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Ahorros.Fec_Cancelacion AT ROW 2.38 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Ahorros.Val_DepMes AT ROW 2.88 COL 80 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Ahorros.Num_DepMes AT ROW 2.92 COL 54 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 3.57 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Ahorros.Fec_ProLiquidacion AT ROW 3.23 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Ahorros.Fec_UltLiquidacion AT ROW 4.08 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Ahorros.Val_RetDia AT ROW 4.23 COL 80 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Ahorros.Num_RetDia AT ROW 4.31 COL 54 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 3.57 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Ahorros.Fec_Ulttransaccion AT ROW 4.92 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Ahorros.Val_RetDiaCheq AT ROW 5.38 COL 79.86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.86 BY .85
          BGCOLOR 18 FGCOLOR 15 
     Ahorros.Num_RetDiaCheq AT ROW 5.42 COL 54 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 3.86 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Ahorros.Fec_Vencimiento AT ROW 5.77 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Ahorros.Fec_Prorroga AT ROW 6.65 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 18 
     Ahorros.Num_RetMes AT ROW 6.69 COL 54 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 3.57 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Ahorros.Val_RetMes AT ROW 6.69 COL 79.72 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.72 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Ahorros.Sdo_Canje AT ROW 8.15 COL 15.14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Ahorros.Sdo_UltLiquidacion AT ROW 8.15 COL 49.14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Ahorros.Sdo_Disponible AT ROW 8.15 COL 79.14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Ahorros.Usu_Creacion AT ROW 9.35 COL 40 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.29 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_NomUsuario AT ROW 9.35 COL 44 COLON-ALIGNED NO-LABEL
     BUTTON-122 AT ROW 9.35 COL 91.14
     RECT-303 AT ROW 1.27 COL 41
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2.29 ROW 10.23
         SIZE 98.57 BY 10.5
         BGCOLOR 17 FONT 4.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F_Secundaria
     RECT-304 AT ROW 2.62 COL 41
     RECT-305 AT ROW 3.96 COL 41
     RECT-306 AT ROW 6.42 COL 41
     RECT-307 AT ROW 7.81 COL 3
     RECT-308 AT ROW 1.27 COL 3
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2.29 ROW 10.23
         SIZE 98.57 BY 10.5
         BGCOLOR 17 FONT 4
         TITLE "Información Adicional".

DEFINE FRAME F_Ahorros
     Cmb_TipoProductos AT ROW 1.42 COL 7.72 COLON-ALIGNED
     Cmb_Productos AT ROW 1.42 COL 30 COLON-ALIGNED NO-LABEL
     Ahorros.Nit AT ROW 2.38 COL 7.72 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 21.29 BY .81
          BGCOLOR 15 FONT 5
     W_NomTitular AT ROW 2.38 COL 30 COLON-ALIGNED NO-LABEL
     Ahorros.Exento_3xm AT ROW 4.31 COL 24.43 COLON-ALIGNED WIDGET-ID 28
          LABEL "Cuenta Subsidiada x el Gobierno"
          VIEW-AS FILL-IN 
          SIZE 3.57 BY .81
          BGCOLOR 15 
     Ahorros.Fec_Activacion[1] AT ROW 4.38 COL 39.57 COLON-ALIGNED NO-LABEL WIDGET-ID 46
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 15 
     Ahorros.Fec_DesActivacion[1] AT ROW 4.38 COL 55.43 COLON-ALIGNED NO-LABEL WIDGET-ID 52
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 15 
     Ahorros.Id_Subsidio_Entidad AT ROW 5.73 COL 24.43 COLON-ALIGNED WIDGET-ID 30
          LABEL "Cuenta Subsidiada x la Entidad?"
          VIEW-AS FILL-IN 
          SIZE 3.57 BY .81
          BGCOLOR 15 
     Ahorros.Ind_Tipo_Subsidio AT ROW 5.54 COL 31.29 NO-LABEL WIDGET-ID 70
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Limitado", 1,
"Ilimitado", 2
          SIZE 9.86 BY 1.31
     Ahorros.Fec_Activacion[2] AT ROW 5.77 COL 39.57 COLON-ALIGNED NO-LABEL WIDGET-ID 48
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 15 
     Ahorros.Fec_DesActivacion[2] AT ROW 5.77 COL 55.43 COLON-ALIGNED NO-LABEL WIDGET-ID 54
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 15 
     Ahorros.Id_Tesoro_Nacional AT ROW 7.19 COL 24.43 COLON-ALIGNED NO-LABEL WIDGET-ID 32
          VIEW-AS FILL-IN 
          SIZE 3.57 BY .81
          BGCOLOR 15 
     Ahorros.Fec_Activacion[3] AT ROW 7.19 COL 39.57 COLON-ALIGNED NO-LABEL WIDGET-ID 50
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 15 
     Ahorros.Fec_DesActivacion[3] AT ROW 7.19 COL 55.57 COLON-ALIGNED NO-LABEL WIDGET-ID 56
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 15 
     Ahorros.Id_Mesada_Pensional AT ROW 8.12 COL 24.43 COLON-ALIGNED NO-LABEL WIDGET-ID 80
          VIEW-AS FILL-IN 
          SIZE 3.57 BY .81
          BGCOLOR 15 
     Ahorros.Fec_Activacion[4] AT ROW 8.12 COL 39.57 COLON-ALIGNED NO-LABEL WIDGET-ID 84
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 15 
     Ahorros.Fec_DesActivacion[4] AT ROW 8.12 COL 55.57 COLON-ALIGNED NO-LABEL WIDGET-ID 86
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 15 
     Ahorros.Cue_Ahorros AT ROW 4.08 COL 81.72 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14.14 BY .81
          BGCOLOR 15 
     Ahorros.Num_Formato AT ROW 5.5 COL 82 COLON-ALIGNED HELP
          ""
          LABEL "Nro Formato" FORMAT "X(14)"
          VIEW-AS FILL-IN 
          SIZE 16.57 BY .81
          BGCOLOR 15 
     Ahorros.Sdo_MinCta AT ROW 6.85 COL 82 COLON-ALIGNED HELP
          ""
          LABEL "Vr Sobregiro" FORMAT "->>>>>,>>>,>>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 16.57 BY .81
          BGCOLOR 15 FGCOLOR 0 
     T-ExeCobro AT ROW 7.92 COL 75 WIDGET-ID 8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 117.43 BY 22.54
         BGCOLOR 17 FONT 4.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F_Ahorros
     Ahorros.Id_Tutor AT ROW 9.5 COL 8.43 COLON-ALIGNED WIDGET-ID 24
          LABEL "R. Legal"
          VIEW-AS COMBO-BOX 
          LIST-ITEMS "Padre","Madre","Otro" 
          DROP-DOWN-LIST
          SIZE 17.57 BY 1
          BGCOLOR 15 
     Ahorros.Nit_Tutor AT ROW 10.42 COL 8.43 COLON-ALIGNED WIDGET-ID 16
          LABEL "Nit.Legal"
          VIEW-AS FILL-IN 
          SIZE 17.57 BY .88
          BGCOLOR 15 
     Nombre_Tutor AT ROW 10.38 COL 27 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     tgl-TarjetaDB AT ROW 9.5 COL 30.57 WIDGET-ID 2
     Ahorros.TarjetaDB AT ROW 9.5 COL 39 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 18 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Tg_Tasa AT ROW 17.5 COL 78.57
     W_Digito AT ROW 4.08 COL 95.72 COLON-ALIGNED NO-LABEL
     BUTTON-1 AT ROW 1.54 COL 103
     BUTTON-2 AT ROW 3.15 COL 103
     Btn_Consulta AT ROW 4.73 COL 103
     Cmb_PerLiquidacion AT ROW 12.92 COL 75 COLON-ALIGNED
     Ahorros.For_Liquidacion AT ROW 13.69 COL 78 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Anticipado", 1,
"Vencido", 2
          SIZE 20.29 BY .81
     Tasa_Efectiva AT ROW 14.58 COL 84 COLON-ALIGNED
     Btn_Salvar AT ROW 9.65 COL 103.29
     Tasa_Nominal AT ROW 15.46 COL 84 COLON-ALIGNED
     Btn_Deshacer AT ROW 11.23 COL 103.29
     Ahorros.Tasa AT ROW 16.35 COL 84 COLON-ALIGNED
          LABEL "Tasa Nominal Anual"
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Interes_PorPagar AT ROW 18.35 COL 83.86 COLON-ALIGNED
     Btn_Ingresar AT ROW 12.85 COL 103.29
     Btn_Cancelar AT ROW 14.46 COL 103.29
     Btn_Borrar AT ROW 16.08 COL 103.29
     BtnDone AT ROW 17.69 COL 103.29
     Ahorros.Estado AT ROW 19.88 COL 54.72 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Activa", 1,
"Inactiva", 2
          SIZE 9 BY 1.5
     Cmb_Estados AT ROW 20.31 COL 70.29 COLON-ALIGNED
     BUTTON-95 AT ROW 19.58 COL 106
     Btn_Autorizados AT ROW 21.88 COL 54.57
     Btn_Libretas AT ROW 21.88 COL 75.43
     BUTTON-92 AT ROW 21.88 COL 94.43
     Imp_Cdat AT ROW 17.77 COL 54
     wcomenTdb AT ROW 9.5 COL 58 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     Imp_UtraMas AT ROW 16.12 COL 54 WIDGET-ID 66
     BUTTON-123 AT ROW 4.38 COL 53.86 WIDGET-ID 88
     "Fecha de Activacion" VIEW-AS TEXT
          SIZE 14.29 BY .81 AT ROW 3.5 COL 39.86 WIDGET-ID 42
          BGCOLOR 18 FGCOLOR 15 
     "Fecha de DesActivacion" VIEW-AS TEXT
          SIZE 17 BY .81 AT ROW 3.46 COL 55.72 WIDGET-ID 44
          BGCOLOR 18 FGCOLOR 15 
     "o de Entidades Territoriales?" VIEW-AS TEXT
          SIZE 19.72 BY .5 AT ROW 7.65 COL 6.29 WIDGET-ID 38
     "Cuenta del Tesoro Nacional" VIEW-AS TEXT
          SIZE 19 BY .5 AT ROW 7 COL 6.43 WIDGET-ID 40
     "   Liquidación de Intereses" VIEW-AS TEXT
          SIZE 48 BY .81 AT ROW 11.81 COL 53
          BGCOLOR 18 FGCOLOR 15 
     "Titular:" VIEW-AS TEXT
          SIZE 5 BY .77 AT ROW 2.38 COL 4.72 WIDGET-ID 12
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 117.43 BY 22.54
         BGCOLOR 17 FONT 4
         DEFAULT-BUTTON BtnDone.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F_Ahorros
     "G.M.F." VIEW-AS TEXT
          SIZE 5 BY .81 AT ROW 3.5 COL 12.86 WIDGET-ID 36
          BGCOLOR 18 FGCOLOR 15 
     "  Tipo" VIEW-AS TEXT
          SIZE 5 BY .81 AT ROW 4.77 COL 34 WIDGET-ID 78
          BGCOLOR 18 FGCOLOR 15 
     "Maneja Mesadas Pensionales?" VIEW-AS TEXT
          SIZE 21 BY .69 AT ROW 8.23 COL 4.43 WIDGET-ID 82
     RECT-274 AT ROW 1.27 COL 102
     RECT-275 AT ROW 1.27 COL 1.72
     RECT-281 AT ROW 19.54 COL 53
     RECT-284 AT ROW 12.62 COL 53
     RECT-315 AT ROW 17.23 COL 77.43
     RECT-317 AT ROW 3.81 COL 2.29 WIDGET-ID 34
     RECT-318 AT ROW 3.81 COL 74.14 WIDGET-ID 62
     RECT-319 AT ROW 9.27 COL 2.14 WIDGET-ID 64
     RECT-334 AT ROW 19.73 COL 54 WIDGET-ID 68
     RECT-335 AT ROW 5.27 COL 2.86 WIDGET-ID 76
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 117.43 BY 22.54
         BGCOLOR 17 FONT 4
         DEFAULT-BUTTON BtnDone.

DEFINE FRAME F_Atermino
     AT_Monto AT ROW 1.15 COL 11.86 COLON-ALIGNED
     AT_Puntos AT ROW 1.27 COL 41 COLON-ALIGNED
     F-FecVenc AT ROW 2.08 COL 12 COLON-ALIGNED WIDGET-ID 2
     AT_Plazo AT ROW 3.15 COL 12 COLON-ALIGNED
     AT_VerTasa AT ROW 4.23 COL 5 COLON-ALIGNED NO-LABEL
     AT_Destino_Intereses AT ROW 5.77 COL 4 NO-LABEL
     BT_DesAT AT ROW 6.65 COL 4
     AT_AgenciaDestino AT ROW 7.77 COL 15 COLON-ALIGNED
     AT_ProductoDestino AT ROW 8.62 COL 15 COLON-ALIGNED
     AT_CuentaDestino AT ROW 9.46 COL 15 COLON-ALIGNED
     AT_NombreDestino AT ROW 10.31 COL 15 COLON-ALIGNED
     " Destino de los Intereses" VIEW-AS TEXT
          SIZE 22 BY .81 AT ROW 5.04 COL 5
          FGCOLOR 7 
     RECT-277 AT ROW 5.46 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 11.92
         SIZE 51 BY 11.23
         BGCOLOR 17 FONT 5
         TITLE "A Termino".

DEFINE FRAME F_Alavista
     AV_Monto AT ROW 1.15 COL 14.14 COLON-ALIGNED HELP
          "Valor de Apertura de la Cuenta"
     AV_Cuota AT ROW 1.15 COL 36.43 COLON-ALIGNED
     AV_Puntos AT ROW 2.15 COL 7 COLON-ALIGNED NO-LABEL
     AV_VerTasa AT ROW 2.15 COL 14 COLON-ALIGNED NO-LABEL
     AP_PeriodoPago AT ROW 3.81 COL 3 NO-LABEL
     AP_FormaPago AT ROW 5.69 COL 6 NO-LABEL
     AP_AgenciaDebito AT ROW 6.46 COL 9 COLON-ALIGNED
     AP_ProductoDebito AT ROW 7.31 COL 9 COLON-ALIGNED
     AP_CuentaDebito AT ROW 8.15 COL 9 COLON-ALIGNED
     AP_NombreDebito AT ROW 9 COL 9 COLON-ALIGNED
     Bt_DebitoAP AT ROW 9.88 COL 4
     AP_FechaDebito AT ROW 10.04 COL 33 COLON-ALIGNED
     " Forma de Pago de la Cuota" VIEW-AS TEXT
          SIZE 25 BY .62 AT ROW 5.08 COL 3
          FGCOLOR 7 
     " Periodo de Pago de la Cuota" VIEW-AS TEXT
          SIZE 27 BY .81 AT ROW 3 COL 3
          FGCOLOR 7 
     RECT-280 AT ROW 5.46 COL 2
     RECT-283 AT ROW 3.27 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 11.92
         SIZE 51 BY 11.23
         BGCOLOR 17 FONT 5
         TITLE "Ahorros a la Vista y Aportes".

DEFINE FRAME F_Contractual
     F-AporOblig AT ROW 1.04 COL 14 COLON-ALIGNED WIDGET-ID 4
     F-Salario AT ROW 1.04 COL 35 COLON-ALIGNED WIDGET-ID 12
     CT_Monto AT ROW 1.88 COL 14 COLON-ALIGNED
     CT_Puntos AT ROW 1.88 COL 35 COLON-ALIGNED
     CT_Plazo AT ROW 2.69 COL 14 COLON-ALIGNED
     CT_Cuota AT ROW 2.69 COL 35 COLON-ALIGNED
     Ahorros.porce_Aporte AT ROW 3.58 COL 44.14 COLON-ALIGNED HELP
          "% Máximo para pasar de aporte Social. Máximo debe ser 80%" WIDGET-ID 2
          LABEL "% Cuota Ahorro Permanen" FORMAT ">9.99"
          VIEW-AS FILL-IN 
          SIZE 5 BY .81
          BGCOLOR 15 FONT 4
     F-PorApoSue AT ROW 3.62 COL 19.43 COLON-ALIGNED WIDGET-ID 14
     CT_PeriodoPago AT ROW 5.04 COL 3 NO-LABEL
     CT_FormaPago AT ROW 6.38 COL 6 NO-LABEL
     CT_AgenciaDebito AT ROW 7.08 COL 9.14 COLON-ALIGNED
     CT_ProductoDebito AT ROW 7.92 COL 9.14 COLON-ALIGNED
     CT_CuentaDebito AT ROW 8.77 COL 9.14 COLON-ALIGNED
     CT_NombreDebito AT ROW 9.62 COL 9 COLON-ALIGNED
     CT_FechaDebito AT ROW 10.46 COL 13 COLON-ALIGNED
     Bt_Debito AT ROW 10.46 COL 29.57
     " Forma de Pago de la Cuota" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 5.77 COL 3
          FGCOLOR 7 
     " Periodo de Pago de la Cuota" VIEW-AS TEXT
          SIZE 27 BY .54 AT ROW 4.54 COL 4
          FGCOLOR 7 
     RECT-278 AT ROW 6.04 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 11.73
         SIZE 51 BY 11.42
         BGCOLOR 17 FONT 4
         TITLE "Programado".


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
         TITLE              = "Matricula de Captaciones, Programa W-Captaciones.W"
         HEIGHT             = 22.54
         WIDTH              = 117.43
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
ASSIGN FRAME F_Alavista:FRAME = FRAME F_Ahorros:HANDLE
       FRAME F_Atermino:FRAME = FRAME F_Ahorros:HANDLE
       FRAME F_Contractual:FRAME = FRAME F_Ahorros:HANDLE.

/* SETTINGS FOR FRAME F_Ahorros
   NOT-VISIBLE FRAME-NAME Custom                                        */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME F_Atermino:MOVE-AFTER-TAB-ITEM (Ahorros.Fec_Activacion[2]:HANDLE IN FRAME F_Ahorros)
       XXTABVALXX = FRAME F_Atermino:MOVE-BEFORE-TAB-ITEM (Ahorros.Fec_DesActivacion[2]:HANDLE IN FRAME F_Ahorros)
       XXTABVALXX = FRAME F_Alavista:MOVE-AFTER-TAB-ITEM (Btn_Consulta:HANDLE IN FRAME F_Ahorros)
       XXTABVALXX = FRAME F_Contractual:MOVE-BEFORE-TAB-ITEM (Cmb_PerLiquidacion:HANDLE IN FRAME F_Ahorros)
       XXTABVALXX = FRAME F_Alavista:MOVE-BEFORE-TAB-ITEM (FRAME F_Contractual:HANDLE)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR BUTTON BtnDone IN FRAME F_Ahorros
   6                                                                    */
/* SETTINGS FOR BUTTON Btn_Autorizados IN FRAME F_Ahorros
   NO-ENABLE 6                                                          */
/* SETTINGS FOR BUTTON Btn_Borrar IN FRAME F_Ahorros
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON Btn_Cancelar IN FRAME F_Ahorros
   NO-ENABLE 6                                                          */
/* SETTINGS FOR BUTTON Btn_Consulta IN FRAME F_Ahorros
   6                                                                    */
/* SETTINGS FOR BUTTON Btn_Deshacer IN FRAME F_Ahorros
   NO-ENABLE 6                                                          */
/* SETTINGS FOR BUTTON Btn_Ingresar IN FRAME F_Ahorros
   6                                                                    */
/* SETTINGS FOR BUTTON Btn_Libretas IN FRAME F_Ahorros
   6                                                                    */
/* SETTINGS FOR BUTTON Btn_Salvar IN FRAME F_Ahorros
   NO-ENABLE 6                                                          */
/* SETTINGS FOR BUTTON BUTTON-1 IN FRAME F_Ahorros
   6                                                                    */
/* SETTINGS FOR BUTTON BUTTON-123 IN FRAME F_Ahorros
   6                                                                    */
/* SETTINGS FOR BUTTON BUTTON-2 IN FRAME F_Ahorros
   6                                                                    */
/* SETTINGS FOR BUTTON BUTTON-92 IN FRAME F_Ahorros
   6                                                                    */
/* SETTINGS FOR BUTTON BUTTON-95 IN FRAME F_Ahorros
   6                                                                    */
/* SETTINGS FOR COMBO-BOX Cmb_Estados IN FRAME F_Ahorros
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX Cmb_Productos IN FRAME F_Ahorros
   NO-ENABLE 1                                                          */
/* SETTINGS FOR COMBO-BOX Cmb_TipoProductos IN FRAME F_Ahorros
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN Ahorros.Cue_Ahorros IN FRAME F_Ahorros
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Ahorros.Exento_3xm IN FRAME F_Ahorros
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Ahorros.Fec_Activacion[1] IN FRAME F_Ahorros
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Ahorros.Fec_Activacion[2] IN FRAME F_Ahorros
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Ahorros.Fec_Activacion[3] IN FRAME F_Ahorros
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Ahorros.Fec_Activacion[4] IN FRAME F_Ahorros
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Ahorros.Fec_DesActivacion[1] IN FRAME F_Ahorros
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Ahorros.Fec_DesActivacion[2] IN FRAME F_Ahorros
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Ahorros.Fec_DesActivacion[3] IN FRAME F_Ahorros
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Ahorros.Fec_DesActivacion[4] IN FRAME F_Ahorros
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET Ahorros.For_Liquidacion IN FRAME F_Ahorros
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Ahorros.Id_Subsidio_Entidad IN FRAME F_Ahorros
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Ahorros.Id_Tesoro_Nacional IN FRAME F_Ahorros
   EXP-LABEL                                                            */
/* SETTINGS FOR COMBO-BOX Ahorros.Id_Tutor IN FRAME F_Ahorros
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Interes_PorPagar IN FRAME F_Ahorros
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN Ahorros.Nit IN FRAME F_Ahorros
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Ahorros.Nit_Tutor IN FRAME F_Ahorros
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Nombre_Tutor IN FRAME F_Ahorros
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Ahorros.Num_Formato IN FRAME F_Ahorros
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN Ahorros.Sdo_MinCta IN FRAME F_Ahorros
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN Ahorros.TarjetaDB IN FRAME F_Ahorros
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Ahorros.Tasa IN FRAME F_Ahorros
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Tasa_Efectiva IN FRAME F_Ahorros
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Tasa_Nominal IN FRAME F_Ahorros
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Digito IN FRAME F_Ahorros
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomTitular IN FRAME F_Ahorros
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Alavista
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Alavista:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN AP_AgenciaDebito IN FRAME F_Alavista
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN AP_CuentaDebito IN FRAME F_Alavista
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN AP_NombreDebito IN FRAME F_Alavista
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET AP_PeriodoPago IN FRAME F_Alavista
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN AP_ProductoDebito IN FRAME F_Alavista
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN AV_Monto IN FRAME F_Alavista
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN AV_Puntos IN FRAME F_Alavista
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN AV_VerTasa IN FRAME F_Alavista
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR BUTTON Bt_DebitoAP IN FRAME F_Alavista
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Atermino
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Atermino:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN AT_AgenciaDestino IN FRAME F_Atermino
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN AT_CuentaDestino IN FRAME F_Atermino
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN AT_NombreDestino IN FRAME F_Atermino
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN AT_Plazo IN FRAME F_Atermino
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN AT_ProductoDestino IN FRAME F_Atermino
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN AT_Puntos IN FRAME F_Atermino
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN AT_VerTasa IN FRAME F_Atermino
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FRAME F_Contractual
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Contractual:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON Bt_Debito IN FRAME F_Contractual
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN CT_AgenciaDebito IN FRAME F_Contractual
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN CT_CuentaDebito IN FRAME F_Contractual
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN CT_NombreDebito IN FRAME F_Contractual
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN CT_ProductoDebito IN FRAME F_Contractual
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN CT_Puntos IN FRAME F_Contractual
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-AporOblig IN FRAME F_Contractual
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-PorApoSue IN FRAME F_Contractual
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-Salario IN FRAME F_Contractual
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Ahorros.porce_Aporte IN FRAME F_Contractual
   NO-DISPLAY EXP-LABEL EXP-FORMAT EXP-HELP                             */
/* SETTINGS FOR FRAME F_Libretas
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BR_Libretas BTN_MosLib F_Libretas */
ASSIGN 
       FRAME F_Libretas:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON BTN_CanLib IN FRAME F_Libretas
   4                                                                    */
/* SETTINGS FOR BUTTON Btn_IngLib IN FRAME F_Libretas
   4                                                                    */
/* SETTINGS FOR BUTTON BTN_MosLib IN FRAME F_Libretas
   4                                                                    */
/* SETTINGS FOR BUTTON BTN_SalLib IN FRAME F_Libretas
   4                                                                    */
/* SETTINGS FOR BUTTON BUTTON-121 IN FRAME F_Libretas
   4                                                                    */
/* SETTINGS FOR FILL-IN Lib_Chequera.Fec_Entrega IN FRAME F_Libretas
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Lib_Chequera.Num_Final IN FRAME F_Libretas
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Lib_Chequera.Num_Inicial IN FRAME F_Libretas
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_AccionLibreta IN FRAME F_Libretas
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Relaciones
   NOT-VISIBLE                                                          */
/* BROWSE-TAB Br_Relaciones R-reclamo F_Relaciones */
ASSIGN 
       FRAME F_Relaciones:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON Btn_CanRel IN FRAME F_Relaciones
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX Cmb_TRF IN FRAME F_Relaciones
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Lb_Nit IN FRAME F_Relaciones
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN Nit_Relacion IN FRAME F_Relaciones
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR RADIO-SET R_Eleccion IN FRAME F_Relaciones
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W-DescTRF IN FRAME F_Relaciones
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomNitRelacion IN FRAME F_Relaciones
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Secundaria
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Secundaria:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN Ahorros.Fec_Apertura IN FRAME F_Secundaria
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Ahorros.Fec_Cancelacion IN FRAME F_Secundaria
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Ahorros.Fec_ProLiquidacion IN FRAME F_Secundaria
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Ahorros.Fec_Prorroga IN FRAME F_Secundaria
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Ahorros.Fec_UltLiquidacion IN FRAME F_Secundaria
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Ahorros.Fec_Ulttransaccion IN FRAME F_Secundaria
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Ahorros.Fec_Vencimiento IN FRAME F_Secundaria
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Ahorros.Num_DepDia IN FRAME F_Secundaria
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Ahorros.Num_DepMes IN FRAME F_Secundaria
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Ahorros.Num_RetDia IN FRAME F_Secundaria
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Ahorros.Num_RetDiaCheq IN FRAME F_Secundaria
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Ahorros.Num_RetMes IN FRAME F_Secundaria
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Ahorros.Sdo_Canje IN FRAME F_Secundaria
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Ahorros.Sdo_Disponible IN FRAME F_Secundaria
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Ahorros.Sdo_UltLiquidacion IN FRAME F_Secundaria
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Ahorros.Usu_Creacion IN FRAME F_Secundaria
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Ahorros.Val_DepDia IN FRAME F_Secundaria
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Ahorros.Val_DepMes IN FRAME F_Secundaria
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Ahorros.Val_RetDia IN FRAME F_Secundaria
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Ahorros.Val_RetDiaCheq IN FRAME F_Secundaria
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Ahorros.Val_RetMes IN FRAME F_Secundaria
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomUsuario IN FRAME F_Secundaria
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BR_Libretas
/* Query rebuild information for BROWSE BR_Libretas
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Lib_Chequera WHERE
   Lib_Chequera.Cod_Producto EQ Ahorros.Cod_Ahorro AND
   Lib_Chequera.Cue_Ahorros EQ Ahorros.Cue_Ahorros
   NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE BR_Libretas */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_Relaciones
/* Query rebuild information for BROWSE Br_Relaciones
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH T_Relaciones NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE Br_Relaciones */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Ahorros
/* Query rebuild information for FRAME F_Ahorros
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Ahorros WHERE Ahorros.Agencia EQ 0 NO-LOCK.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* FRAME F_Ahorros */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Alavista
/* Query rebuild information for FRAME F_Alavista
     _Query            is NOT OPENED
*/  /* FRAME F_Alavista */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Atermino
/* Query rebuild information for FRAME F_Atermino
     _Query            is NOT OPENED
*/  /* FRAME F_Atermino */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Contractual
/* Query rebuild information for FRAME F_Contractual
     _TblList          = "bdcentral.Ahorros"
     _Query            is NOT OPENED
*/  /* FRAME F_Contractual */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Libretas
/* Query rebuild information for FRAME F_Libretas
     _Query            is NOT OPENED
*/  /* FRAME F_Libretas */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Relaciones
/* Query rebuild information for FRAME F_Relaciones
     _Query            is NOT OPENED
*/  /* FRAME F_Relaciones */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Secundaria
/* Query rebuild information for FRAME F_Secundaria
     _Query            is NOT OPENED
*/  /* FRAME F_Secundaria */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Matricula de Captaciones, Programa W-Captaciones.W */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Matricula de Captaciones, Programa W-Captaciones.W */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F_Ahorros
&Scoped-define FRAME-NAME F_Alavista
&Scoped-define SELF-NAME AP_FechaDebito
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL AP_FechaDebito wWin
ON LEAVE OF AP_FechaDebito IN FRAME F_Alavista /* Fecha Débito */
DO:
  IF DATE(SELF:SCREEN-VALUE) LT W_Fecha THEN DO:
     MESSAGE "La fecha a realizar el débito automático debe ser" SKIP
             "mayor o igual a la fecha actual. digite de nuevo " SKIP
             "la fecha a debitar!" VIEW-AS ALERT-BOX INFORMATION.
     SELF:SCREEN-VALUE = STRING(W_Fecha).
     APPLY "entry" TO AP_FechaDebito IN FRAME F_Alavista.
     RETURN NO-APPLY.
  END.
  ENABLE Btn_Salvar WITH FRAME F_Ahorros.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME AP_FormaPago
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL AP_FormaPago wWin
ON LEAVE OF AP_FormaPago IN FRAME F_Alavista
DO:
  IF AVAILABLE Ahorros AND INTEGER(SELF:SCREEN-VALUE) NE Ahorros.For_Pago THEN DO:
     CASE SELF:SCREEN-VALUE:
       WHEN "1" THEN Wk_DatAnt = "CAJA".
       WHEN "2" THEN Wk_DatAnt = "NÓMINA".
       WHEN "3" THEN Wk_DatAnt = "DEB.AUTOMÁTICO".
     END CASE.
     Wk_Cambio2 = "Nva.For.Pag.Cuo: " + Wk_DatAnt.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL AP_FormaPago wWin
ON VALUE-CHANGED OF AP_FormaPago IN FRAME F_Alavista
DO:
  ENABLE Btn_Salvar WITH FRAME F_Ahorros.
  ASSIGN Wk_Cambio2   = ""
         Wk_PerPagEmp = 0.
  
  IF AP_FormaPago:SCREEN-VALUE IN FRAME F_AlaVista EQ "3" THEN
     ENABLE BT_DebitoAP AP_FechaDebito WITH FRAME F_AlaVista.
  ELSE DO:
     DISABLE BT_DebitoAP AP_FechaDebito WITH FRAME F_AlaVista.
     ASSIGN AP_AgenciaDebito:SCREEN-VALUE     = ""
            AP_ProductoDebito:SCREEN-VALUE    = ""
            AP_CuentaDebito:SCREEN-VALUE      = ""
            AP_NombreDebito:SCREEN-VALUE      = ""
            AP_FechaDebito:SCREEN-VALUE       = "".
  END.
  
  IF AVAILABLE Clientes AND Clientes.Cod_Empresa NE 0 THEN DO:
     FIND Empresas WHERE Empresas.Cod_Empresa EQ Clientes.Cod_Empresa NO-LOCK NO-ERROR.
     IF AVAILABLE Empresas THEN
        Wk_PerPagEmp = Empresas.FOR_Pago.
  END.                    
                        
  IF Wk_PerPagEmp NE 0 THEN 
     AP_PeriodoPago:SCREEN-VALUE = STRING(Wk_PerPagemp).
        
  IF SELF:SCREEN-VALUE NE "2" THEN
     AP_PeriodoPago:SCREEN-VALUE = "4".
     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME AP_PeriodoPago
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL AP_PeriodoPago wWin
ON LEAVE OF AP_PeriodoPago IN FRAME F_Alavista
DO:
  IF AVAILABLE Ahorros AND Ahorros.Per_Deduccion NE INTEGER(SELF:SCREEN-VALUE) THEN DO:
     CASE SELF:SCREEN-VALUE:
       WHEN "1" THEN Wk_DatAnt = "SEMANAL".
       WHEN "2" THEN Wk_DatAnt = "DECADAL".
       WHEN "3" THEN Wk_DatAnt = "QUINCENAL".
       WHEN "4" THEN Wk_DatAnt = "MENSUAL".
     END CASE.
     Wk_Cambio2 = " - Nvo.Per.Deducción: " + Wk_DatAnt.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL AP_PeriodoPago wWin
ON VALUE-CHANGED OF AP_PeriodoPago IN FRAME F_Alavista
DO:
  Wk_Cambio2 = "".
    
  ENABLE Btn_Salvar WITH FRAME F_Ahorros.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Atermino
&Scoped-define SELF-NAME AT_Destino_Intereses
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL AT_Destino_Intereses wWin
ON LEAVE OF AT_Destino_Intereses IN FRAME F_Atermino
DO:
  IF AVAILABLE Ahorros AND INTEGER(SELF:SCREEN-VALUE) NE Ahorros.Des_Intereses THEN DO:
     CASE SELF:SCREEN-VALUE:
       WHEN "1" THEN Wk_DatAnt = "Ahorro".
       WHEN "2" THEN Wk_DatAnt = "CRÉDITO".
       WHEN "3" THEN Wk_DatAnt = "CUENTA X PAGAR".
     END CASE.
     Wk_Cambio1 = "Nvo.Destino Interes: " + Wk_DatAnt.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL AT_Destino_Intereses wWin
ON VALUE-CHANGED OF AT_Destino_Intereses IN FRAME F_Atermino
DO:
  Wk_Cambio1 = "".
  ENABLE Btn_Salvar WITH FRAME F_Ahorros.
  DO WITH FRAME F_Atermino:
    CASE SELF:SCREEN-VALUE IN FRAME F_Atermino:
       WHEN "1" THEN DO:
           ASSIGN AT_CuentaDestino:LABEL = "Cuenta Destino".
           ENABLE Bt_DesAT.
       END.
        WHEN "2" THEN DO:
           ASSIGN AT_CuentaDestino:LABEL = "Pagare".
           ENABLE Bt_DesAT.
        END.
        WHEN "3" THEN DO:
            DISABLE Bt_DesAT.
        END.
    END CASE.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME AT_Monto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL AT_Monto wWin
ON LEAVE OF AT_Monto IN FRAME F_Atermino /* Monto Inicial */
DO:
    DO WITH FRAME F_Atermino:
        IF AVAILABLE Pro_Ahorros THEN DO:
            IF Pro_Ahorros.Id_MonAper EQ YES AND Pro_Ahorros.Val_MonAper GT DEC(SELF:SCREEN-VALUE) THEN DO:
                MESSAGE "El valor minimo para el monto de apertura" SKIP
                        "es de: $" Pro_Ahorros.Val_MonAper
                    VIEW-AS ALERT-BOX INFORMATION.

                SELF:SCREEN-VALUE = STRING(Pro_Ahorros.Val_MonAPer).
            END.
        END.

        APPLY "leave" TO AT_Plazo.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL AT_Monto wWin
ON VALUE-CHANGED OF AT_Monto IN FRAME F_Atermino /* Monto Inicial */
DO:
  ENABLE Btn_Salvar WITH FRAME F_Ahorros.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME AT_Plazo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL AT_Plazo wWin
ON LEAVE OF AT_Plazo IN FRAME F_Atermino /* Plazo en Dìas */
DO:
    DO WITH FRAME F_ATermino:
        DEFI VAR W_ValC LIKE Ahorros.Cuota.

        ASSIGN AT_Plazo
               W_ValC = AT_Plazo / 30.

        IF DEC(AT_Monto:SCREEN-VALUE) = 0 THEN DO:
            MESSAGE "NO a digitado monto!"
                VIEW-AS ALERT-BOX ERROR.

            APPLY "ENTRY" to AT_monto.
        END.

        IF W_ValC NE ROUND(W_ValC,0) AND
           INTEGER(SUBSTRING(Cmb_TipoProductos:SCREEN-VALUE IN FRAME F_Ahorros,1,1)) EQ 3 AND
           INTEGER(SUBSTRING(SELF:SCREEN-VALUE,1,2)) GT 1 AND
           INTEGER(SUBSTRING(SELF:SCREEN-VALUE,1,2)) LT 6 THEN
            ASSIGN Cmb_PerLiquidacion:SCREEN-VALUE = "06 - Al Vencimiento"
                   Cmb_PerLiquidacion:SENSITIVE = FALSE.
        ELSE
            Cmb_PerLiquidacion:SENSITIVE = TRUE.

        IF Pro_Ahorros.Id_Tasa EQ 1 THEN DO:
            IF DEC(AT_Monto:SCREEN-VALUE) EQ 0 OR DEC(AT_Plazo:SCREEN-VALUE) EQ 0 THEN DO:
                MESSAGE "El Monto y el Plazo deben ser mayores a cero. Rectifique-1" SKIP
                        "Monto:" DEC(AT_Monto:SCREEN-VALUE) SKIP
                        "Plazo:" DEC(AT_Plazo:SCREEN-VALUE)
                    VIEW-AS ALERT-BOX ERROR.

                RETURN NO-APPLY.
            END. 

            RUN Hallar_Tasa (INPUT Pro_Ahorros.Indicador,
                             INPUT DEC(AT_Monto:SCREEN-VALUE),
                             INPUT DEC(AT_Plazo:SCREEN-VALUE),
                             OUTPUT W_Tasa,
                             OUTPUT W_Puntos).

            ASSIGN Tasa_Efectiva:SCREEN-VALUE IN FRAME F_Ahorros = STRING(w_tasa).

            IF W_Puntos NE 0 THEN DO:
                AT_Puntos:SCREEN-VALUE = STRING(W_Puntos).

                ENABLE AT_Puntos.

                AT_Puntos:BGCOLOR = 15.
                AT_Puntos:SCREEN-VALUE = STRING(W_Puntos,"->9.99").
            END.
            ELSE DO:
                DISABLE AT_Puntos.
                
                AT_Puntos:SCREEN-VALUE = "".
            END.
        END.

        IF INTEGER(SUBSTRING(Cmb_TipoProductos:SCREEN-VALUE IN FRAME F_Ahorros,1,1)) EQ 3 THEN
            APPLY "ENTRY" TO Cmb_PerLiquidacion.
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL AT_Plazo wWin
ON VALUE-CHANGED OF AT_Plazo IN FRAME F_Atermino /* Plazo en Dìas */
DO:
  ENABLE Btn_Salvar WITH FRAME F_Ahorros.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME AT_Puntos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL AT_Puntos wWin
ON LEAVE OF AT_Puntos IN FRAME F_Atermino /* Puntos */
DO:
  /*DEFINE VAR PuntosRestar AS DEC FORMAT ">>,>>>9.9999999".
  IF DEC(SELF:SCREEN-VALUE) GT MaximoPuntos THEN DO:
     MESSAGE "El máximo de puntos negociables permitidos" SKIP
             "para este producto es de: " STRING(MaximoPuntos).
     AT_Puntos:SCREEN-VALUE = STRING(MaximoPuntos).
     APPLY "entry" TO AT_Puntos.
     RETURN NO-APPLY.
  END.
  
     ASSIGN Tasa_Efectiva:SCREEN-VALUE IN FRAME F_Ahorros = 
                          STRING(DEC(Tasa_Efectiva:SCREEN-VALUE IN FRAME F_Ahorros) 
                        - W_Puntos + DEC(SELF:SCREEN-VALUE)).
     
 W_Puntos = DEC(SELF:SCREEN-VALUE). 
 RUN Convertir_Tasa_Periodica (INPUT DEC(AT_Monto:SCREEN-VALUE), INPUT DEC(Tasa_Efectiva:SCREEN-VALUE)).    */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Alavista
&Scoped-define SELF-NAME AV_Cuota
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL AV_Cuota wWin
ON LEAVE OF AV_Cuota IN FRAME F_Alavista /* Cuota */
DO:
    /*
    IF AVAILABLE Ahorros AND DEC(SELF:SCREEN-VALUE) NE Ahorros.Cuota THEN
     Wk_Cambio1 = "Cambio Cuota a: $" + SELF:SCREEN-VALUE.
     */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL AV_Cuota wWin
ON VALUE-CHANGED OF AV_Cuota IN FRAME F_Alavista /* Cuota */
DO:
  Wk_Cambio1 = "".
  ENABLE Btn_Salvar WITH FRAME F_Ahorros.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME AV_Monto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL AV_Monto wWin
ON LEAVE OF AV_Monto IN FRAME F_Alavista /* Monto Apertura */
DO:
DO WITH FRAME F_AlaVista:
  FIND Pro_Ahorros WHERE
       Pro_Ahorros.Cod_Ahorro EQ INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Ahorros,1,3)) AND 
       Pro_Ahorros.Tip_Ahorro EQ INTEGER(SUBSTRING(Cmb_TipoProductos:SCREEN-VALUE IN FRAME F_Ahorros,1,1)) NO-LOCK NO-ERROR.
  IF AVAILABLE Pro_Ahorros THEN DO:
     IF Pro_Ahorros.Id_MonAper EQ YES AND
        Pro_Ahorros.Val_MonAper GT DEC(SELF:SCREEN-VALUE) THEN 
     /*   MESSAGE "El valor minimo para el monto de apertura" SKIP
                "es de: $ " Pro_Ahorros.Val_MonAper VIEW-AS ALERT-BOX INFORMATION.*/
        SELF:SCREEN-VALUE = STRING(Pro_Ahorros.Val_MonAPer).
 

  RUN Hallar_Tasa (INPUT Pro_Ahorros.Indicador, INPUT DEC(AV_Monto:SCREEN-VALUE),
                   INPUT 99999, OUTPUT W_Tasa, OUTPUT W_Puntos).
  IF W_Puntos NE 0 THEN DO:
     ENABLE AV_Puntos.
     AV_Puntos:BGCOLOR = 15.
  END.

  ASSIGN Tasa_Efectiva:SCREEN-VALUE IN FRAME F_Ahorros = STRING(W_Tasa)
         Tasa_Efectiva:LABEL                           = "Tasa Nominal Anual"
         Ahorros.Tasa:SCREEN-VALUE  = STRING(W_Tasa).
       

    IF (SUBSTRING(Cmb_TipoProductos:SCREEN-VALUE IN FRAME F_Ahorros,1,1)) EQ '4'  OR 
         (SUBSTRING(Cmb_TipoProductos:SCREEN-VALUE IN FRAME F_Ahorros,1,1)) EQ '2' THEN DISABLE  AV_Cuota.
    
  END. 
        ELSE MESSAGE "No Encontro Producto".
  
END.
    ENABLE Btn_Salvar WITH FRAME F_Ahorros.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BR_Libretas
&Scoped-define FRAME-NAME F_Libretas
&Scoped-define SELF-NAME BR_Libretas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BR_Libretas wWin
ON ROW-DISPLAY OF BR_Libretas IN FRAME F_Libretas
DO:
  IF Lib_Chequera.Estado EQ 1 THEN
     W_NomEstado = "Activa".
  ELSE
     W_NomEstado = "Inactiva".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br_Relaciones
&Scoped-define FRAME-NAME F_Relaciones
&Scoped-define SELF-NAME Br_Relaciones
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Relaciones wWin
ON MOUSE-SELECT-CLICK OF Br_Relaciones IN FRAME F_Relaciones
DO:
    /*ASSIGN Nit_Relacion:SCREEN-VALUE = T_Relaciones.R_NitObjeto.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ahorros
&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone wWin
ON CHOOSE OF BtnDone IN FRAME F_Ahorros /* Salir */
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


&Scoped-define FRAME-NAME F_Relaciones
&Scoped-define SELF-NAME Btn_Activas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Activas wWin
ON CHOOSE OF Btn_Activas IN FRAME F_Relaciones /* Inactivar */
DO:
  DO WITH FRAME F_Relaciones:
  ASSIGN vlNewRel = FALSE.
  IF Br_Relaciones:NUM-SELECTED-ROWS EQ 0 THEN DO:
     MESSAGE "Debe posicionarse en la relación a inactivar" SKIP
             "mediante el mouse. Rectifique!!!" VIEW-AS ALERT-BOX INFORMATION.
  END.
  ELSE
  DO:
    FIND FIRST Relaciones WHERE Relaciones.Nit      EQ Ahorros.Nit:SCREEN-VALUE IN FRAME F_Ahorros AND
                          Relaciones.Cod_Relacion   EQ INTEGER(R_Eleccion:SCREEN-VALUE) AND
                          Relaciones.Nit_Relacion   EQ T_Relaciones.R_NitObjeto         AND
                          Relaciones.Cuenta         EQ Cue_Ahorros:SCREEN-VALUE IN FRAME F_Ahorros AND
                          Relaciones.Cod_Producto   EQ T_Relaciones.R_CodPdt                       AND
                          Relaciones.Clase_Producto EQ 1  NO-ERROR.
    IF AVAILABLE(Relaciones) THEN DO:
       IF RActivas:SCREEN-VALUE EQ "1" THEN
          ASSIGN Relaciones.Estado = 2
                 Relaciones.Fec_Inactividad = W_Fecha.
       ELSE
          ASSIGN Relaciones.Estado = 1
                 Relaciones.Fec_Inactividad = ?.
    END.
    ELSE 
       MESSAGE "No se hallo la relacion...?"
           VIEW-AS ALERT-BOX ERROR.

    APPLY "Mouse-Select-Click" TO R_Eleccion.
    RETURN NO-APPLY.
  END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ahorros
&Scoped-define SELF-NAME Btn_Autorizados
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Autorizados wWin
ON CHOOSE OF Btn_Autorizados IN FRAME F_Ahorros /* Beneficiarios/Autorizados */
DO:            
     IF Ahorros.Nit:SCREEN-VALUE IN FRAME F_Ahorros EQ "" THEN DO:
     MESSAGE "Para poder crear Benificiarios y/o Autorizados" SKIP
             "debe escogerse una cuenta de Ahorros" VIEW-AS ALERT-BOX ERROR.
     RETURN.
  END.
      FIND Ahorros WHERE 
       Ahorros.Tip_Ahorro = INT(SUBSTR(Cmb_TipoProductos:SCREEN-VALUE IN FRAME  F_Ahorros,1,1)) AND
       Ahorros.Cod_Ahorro = INT(SUBSTR(Cmb_Productos:SCREEN-VALUE IN FRAME F_Ahorros,1,3))
       AND  int(Ahorros.Cue_Ahorros) = INT (Ahorros.Cue_Ahorros:SCREEN-VALUE IN FRAME F_Ahorros)
       AND Ahorros.Cue_Ahorros:SCREEN-VALUE IN FRAME F_Ahorros NE ?  NO-LOCK NO-ERROR.
  IF AVAIL (Ahorros) THEN DO:   
    IF AVAIL Ahorros AND (Ahorros.tip_Ahorro EQ 2 OR Ahorros.tip_Ahorro EQ 3)
     THEN r-reclamo:SENSITIVE IN FRAME f_relaciones = TRUE.
     ELSE r-reclamo:SENSITIVE IN FRAME f_relaciones = FALSE.
  
  HIDE FRAME F_Libretas FRAME F_Secundaria.
  
    IF AVAIL Ahorros AND Ahorros.Nit:SCREEN-VALUE IN FRAME F_Ahorros NE "" THEN DO:
     ASSIGN FRAME F_Ahorros:SENSITIVE = FALSE FRAME F_Relaciones:VISIBLE = TRUE.
     IF Ahorros.conjuncion = YES THEN R-Reclamo:SCREEN-VALUE = "1".
                                 ELSE R-Reclamo:SCREEN-VALUE = "2".
     IF Ahorros.tip_Ahorro = 1 THEN DO:                                 
        ASSIGN Cmb_TRF:SENSITIVE = TRUE W-DescTRF:SENSITIVE = TRUE.
        RUN prc_TRF.
     END.
     ELSE ASSIGN Cmb_TRF:SENSITIVE = FALSE w-DescTRF:SENSITIVE = FALSE.


  END.
  END.
  ELSE DO:
     MESSAGE "Para poder crear Benificiarios y/o Autorizados" SKIP
             "debe primero grabar la cuenta"
         INT(SUBSTR(Cmb_TipoProductos:SCREEN-VALUE IN FRAME  F_Ahorros,1,1))
          INT(SUBSTR(Cmb_Productos:SCREEN-VALUE IN FRAME F_Ahorros,1,3)) Ahorros.Cue_Ahorros:SCREEN-VALUE IN FRAME F_Ahorros
          VIEW-AS ALERT-BOX INFORMATION.
     RETURN.
  END.

  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancelar wWin
ON CHOOSE OF Btn_Cancelar IN FRAME F_Ahorros /* Cancelar */
DO:
  NuevaCta = NO.
  ENABLE Btn_Consulta Btn_Ingresar Btn_Borrar WITH FRAME F_Ahorros.
  DISABLE Btn_Salvar Btn_Cancelar Btn_Deshacer Btn_Borrar WITH FRAME F_Ahorros.  
     
  /*substring(Cmb_TipoProductos:SCREEN-VALUE IN FRAME f_Ahorros,1,1) = "1".*/
  Cmb_TipoProductos:SCREEN-VALUE =Cmb_TipoProductos:ENTRY(1).
     ASSIGN Tpdt = 1 OK_Cancelar = YES.
     RUN Llenar_CmbProductos.
     OK_Cancelar = NO.
  
     FIND Pro_Ahorros WHERE Pro_Ahorros.Tip_Ahorro EQ 1
                        AND Pro_Ahorros.Cod_Ahorro EQ 1 NO-LOCK NO-ERROR.
     IF AVAIL Pro_Ahorros THEN DO:
        W_Ok = Cmb_Productos:ADD-LAST(STRING(Pro_Ahorros.Cod_Ahorro,"999") + " - " + Pro_Ahorros.Nom_Producto).
        Cmb_Productos:SCREEN-VALUE = STRING(Pro_Ahorros.Cod_Ahorro,"999") + " - " + Pro_Ahorros.Nom_Producto.
        ASSIGN Wk_MontoApertura = 0 Wk_IdMonApe = NO Wk_Cuota = 0.
        IF Pro_Ahorros.Id_MonApertura THEN
           ASSIGN Wk_IdMonApe      = Pro_Ahorros.Id_MonApertura
                  Wk_MontoApertura = Pro_Ahorros.Val_MonAper.
        IF Pro_Ahorros.Id_Cuota THEN Wk_Cuota = Pro_Ahorros.Val_Cuota.
        PunteroP = ROWID(Pro_Ahorros).
     END.
  
  DISABLE Ahorros.Nit Ahorros.Cue_Ahorros Cmb_Productos Cmb_TipoProductos Num_Formato WITH FRAME F_Ahorros.
  RUN mostrar_registro.
  RUN inicializar_variables.
   /* ASSIGN ahorros.nit:SCREEN-VALUE = ""
          ahorros.cue_ahorros:SCREEN-VALUE = ""  . */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Libretas
&Scoped-define SELF-NAME BTN_CanLib
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_CanLib wWin
ON CHOOSE OF BTN_CanLib IN FRAME F_Libretas /* Cancelar */
DO:
  DO WITH FRAME F_Libretas:
   FIND FIRST Lib_Chequera WHERE Lib_Chequera.Agencia   EQ Ahorros.Agencia AND
              Lib_Chequera.Cod_Producto   EQ Ahorros.Cod_Ahorro AND 
              Lib_Chequera.Cue_Ahorros    EQ Ahorros.Cue_Ahorros  AND
              Lib_Chequera.Estado         EQ 1 NO-ERROR.
   IF AVAILABLE Lib_Chequera THEN
      ASSIGN Lib_Chequera.Tip_Talonario:SCREEN-VALUE = STRING(Lib_Chequera.Tip_Talonario)
             Lib_Chequera.Num_Inicial:SCREEN-VALUE   = STRING(Lib_Chequera.Num_Inicial)
             Lib_Chequera.Num_Final:SCREEN-VALUE     = STRING(Lib_Chequera.Num_Final)
             Lib_Chequera.Fec_Entrega:SCREEN-VALUE   = STRING(Lib_Chequera.Fec_Entrega)
             W_AccionLibreta:SCREEN-VALUE            = "Talonario Activo al Momento".
   ELSE
      W_AccionLibreta:SCREEN-VALUE = "No Hay talonarios Activos".
    ASSIGN Lib_Chequera.Num_Inicial:SCREEN-VALUE = "0"
           Lib_Chequera.Num_Inicial:BGCOLOR        = 18
           Lib_Chequera.Num_Inicial:FGCOLOR        = 15
           Lib_Chequera.Num_Final:BGCOLOR          = 18
           Lib_Chequera.Num_Final:SCREEN-VALUE     = "0"
           Lib_Chequera.Num_Final:FGCOLOR          = 15.
    DISABLE Lib_Chequera.Num_Inicial Lib_Chequera.Num_Final Lib_Chequera.Tip_Talonario Lib_Chequera.Fec_Entrega.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Relaciones
&Scoped-define SELF-NAME Btn_CanRel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_CanRel wWin
ON CHOOSE OF Btn_CanRel IN FRAME F_Relaciones /* Cancelar */
DO:
  DO WITH FRAME F_Relaciones:
      ASSIGN vlNewRel = FALSE.
      HIDE Nit_Relacion Lb_Nit R_Relacion2.
      ASSIGN Nit_Relacion:SCREEN-VALUE = ""
             Nit_Relacion:FGCOLOR      = 17
             Nit_Relacion:BGCOLOR      = 17
             R_Relacion2:FGCOLOR      = 17
             R_Relacion2:BGCOLOR      = 17
             W_NomNitRelacion:BGCOLOR  = 17
             W_NomNitRelacion:SCREEN-VALUE = ""
             W_VrAutoriz:VISIBLE           = FALSE.
      ENABLE Btn_CreRel.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ahorros
&Scoped-define SELF-NAME Btn_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Consulta wWin
ON CHOOSE OF Btn_Consulta IN FRAME F_Ahorros /* Button 3 */
DO:
    ASSIGN FRAME F_Relaciones:VISIBLE = FALSE
           FRAME F_Secundaria:VISIBLE = FALSE
           FRAME F_Contractual:VISIBLE = FALSE
           Btn_Libretas:VISIBLE = FALSE
           WWin:SENSITIVE = FALSE
           tgl-TarjetaDb:VISIBLE = FALSE
           Ahorros.TarjetaDB:VISIBLE = FALSE
           wcomenTdb:VISIBLE = FALSE
           Ahorros.Id_Tutor:VISIBLE = NO
           Ahorros.Nit_Tutor:VISIBLE = NO
           Nombre_Tutor:VISIBLE = NO
           AT_Destino_Intereses:SENSITIVE IN FRAME F_ATermino = TRUE.

    DO WITH FRAME F_Relaciones:
        FOR EACH T_Relaciones:
            DELETE T_Relaciones.
        END.

        CLOSE QUERY BR_Relaciones.
    END.

    RUN C-Ahorros.w (INPUT "",
                     OUTPUT W_Age,
                     OUTPUT W_Pro,
                     OUTPUT W_Nit,
                     OUTPUT W_Cue).

    ASSIGN NuevaCta = NO
           Wk_Cambio1 = ""
           Wk_Cambio2 = ""
           Wk_Cambio3 = ""
           Wk_DatAnt = ""
           WWin:SENSITIVE = TRUE.

    WWin:MOVE-TO-TOP().

    FIND FIRST Ahorros WHERE Ahorros.Agencia EQ W_Age
                         AND Ahorros.Cod_Ahorro EQ W_Pro
                         AND Ahorros.Cue_Ahorros EQ W_Cue
                         AND Ahorros.Nit EQ W_Nit NO-LOCK NO-ERROR.
    IF AVAIL Ahorros THEN DO:
        puntero = ROWID(Ahorros).

        FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ W_Pro NO-LOCK NO-ERROR.
        IF AVAIL Pro_Ahorros THEN DO:
            punterop = ROWID(Pro_Ahorros).

            Cmb_TipoProductos:SCREEN-VALUE = Cmb_TipoProductos:ENTRY(Pro_Ahorros.Tip_Ahorro).
            W_Ok = Cmb_Productos:ADD-LAST(STRING(Pro_Ahorros.Cod_Ahorro,"999") + " - " + Pro_Ahorros.Nom_Producto).
            Cmb_Productos:SCREEN-VALUE IN FRAME f_ahorros = STRING(Pro_Ahorros.Cod_Ahorro,"999") + " - " + Pro_Ahorros.Nom_Producto.

            RUN Habilitar_Campos.
            RUN mostrar_registro.

            IF AVAIL Pro_ahorros AND Pro_Ahorros.Id_Persona EQ 2 THEN DO:
                APPLY "value-changed" TO ahorros.ID_tutor.
                DISABLE ahorros.id_tutor.
            END.

            ENABLE Btn_Cancelar
                   Btn_Deshacer
                   Btn_Borrar
                   Btn_autorizados
                WITH FRAME F_Ahorros.

            IF Pro_Ahorros.Id_Cuota THEN
                ASSIGN AV_Cuota:SENSITIVE IN FRAME F_AlaVista = TRUE.
        END.

        IF Pro_Ahorros.tip_Ahorro EQ 3 THEN
            IF Ahorros.Detalle_estado NE 1 THEN
                ASSIGN Cmb_PerLiquidacion:SENSITIVE = FALSE
                       /*AT_Destino_Intereses:SENSITIVE IN FRAME F_ATermino = FALSE*/.

    ct_formaPago:SCREEN-VALUE IN FRAME f_contractual = STRING(ahorros.FOR_pago). 


      IF  Ahorros.tip_ahorro = 1 AND ( Ahorros.cod_Ahorro = 1 OR Ahorros.cod_Ahorro = 3 ) THEN DO:
    IF Ahorros.TarjetaDB NE " " AND Ahorros.Fec_CancTdb = ? THEN 
          ASSIGN Btn_Libretas:VISIBLE           = FALSE
                 tgl-TarjetaDb:SCREEN-VALUE     = "Yes".
                      
        IF Ahorros.ajuste NE 0 THEN 
          ASSIGN T-ExeCobro:SCREEN-VALUE         = "Yes".
        ELSE
          ASSIGN T-ExeCobro:SCREEN-VALUE         = "No".         
          
        wcomenTdb:SCREEN-VALUE = "".
        IF Ahorros.Fec_CancTdb <> ? THEN  
            ASSIGN wcomenTdb:SCREEN-VALUE         = "Cancelada".
      END.

      ENABLE Btn_salvar.
     ASSIGN FRAME F_Libretas:VISIBLE = FALSE.
     END.
  ELSE   APPLY "CHOOSE" TO Btn_Cancelar.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Relaciones
&Scoped-define SELF-NAME Btn_CreRel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_CreRel wWin
ON CHOOSE OF Btn_CreRel IN FRAME F_Relaciones /* Crear */
DO:
  DO WITH FRAME F_Relaciones:
     ASSIGN vlNewRel = TRUE.
     ASSIGN Nit_Relacion:HIDDEN = NO
            Nit_Relacion:BGCOLOR = 15
            Nit_Relacion:FGCOLOR = 0
            R_Relacion2:BGCOLOR = 15
            R_Relacion2:FGCOLOR = 0
            RActivas:SCREEN-VALUE = "1"
            W_NomNitRelacion:BGCOLOR = 18
            W_VrAutoriz:SCREEN-VALUE = "0".
            W_VrAutoriz:VISIBLE      = FALSE.
            
     ENABLE  Nit_Relacion R_Relacion2 Btn_SalRel Btn_CanRel.
     DISABLE Btn_CreRel.
     
     DISPLAY Lb_Nit  WITH FRAME F_Relaciones.
     
     IF INTEGER(R_Eleccion:SCREEN-VALUE) EQ 7 THEN
        ASSIGN W_VrAutoriz:VISIBLE   = TRUE
               W_VrAutoriz:SENSITIVE = TRUE.
     
     APPLY 'entry' TO Nit_Relacion.
     RETURN NO-APPLY.
  END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ahorros
&Scoped-define SELF-NAME Btn_Deshacer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Deshacer wWin
ON CHOOSE OF Btn_Deshacer IN FRAME F_Ahorros /* Deshacer */
DO:
  NuevaCta = NO.
  ENABLE Btn_Ingresar Btn_Borrar WITH FRAME F_Ahorros.
  DISABLE Btn_Salvar Btn_Cancelar Btn_Deshacer WITH FRAME F_Ahorros.  
    /*RUN Inicializar_Variables. */
  
  FIND Ahorros WHERE ROWID(Ahorros) EQ Puntero.
  DISABLE Ahorros.Nit Cmb_Productos Cmb_TipoProductos Num_Formato WITH FRAME F_Ahorros.
  RUN Mostrar_Registro.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Libretas
&Scoped-define SELF-NAME Btn_IngLib
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_IngLib wWin
ON CHOOSE OF Btn_IngLib IN FRAME F_Libretas /* Ingresar */
DO:
 DO WITH FRAME F_Libretas:
   ASSIGN W_CtaOpe = ""
          W_Operacion = 0 
          W_CtaCor = ""
          W_CodDed = ""
          W_Comision = ""
          W_DedCta = ""
          W_DedCla = 0
          W_DedVal = 0
          W_DedValImp = 0
          W_DedCtaImp = ""
          W_ComCta = ""
          W_ComCla = 0
          W_ComVal = 0
          W_ComValImp = 0
          W_ComCtaImp = ""
          W_ComNom = ""
          W_DedNom = "".

   FIND Agencias WHERE Agencias.Agencia EQ Ahorros.Agencia NO-LOCK NO-ERROR.
   IF W_Agencia NE Ahorros.Agencia THEN DO:
     MESSAGE "La compra de una Libreta o Chequera debe" SKIP
             "Realizarse en la Agencia donde se encuentra" SKIP
             "matriculada la cuenta. en este caso en la" SKIP
             "Agencia: " STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre SKIP(1)
             "La matricula de la libreta se cancela"
             VIEW-AS ALERT-BOX INFORMATION.
     APPLY "choose" TO Btn_CanLib.
     RETURN NO-APPLY.
   END.
 
    FIND Pro_Ahorros WHERE Pro_Ahorros.Tip_Ahorro EQ INTEGER(SUBSTRING(Cmb_TipoProductos:SCREEN-VALUE IN FRAME F_Ahorros,1,1)) AND
                           Pro_Ahorros.Cod_Ahorro EQ INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Ahorros,1,3)) AND
                           Pro_Ahorros.Estado       EQ 1 NO-LOCK NO-ERROR.
    IF AVAILABLE(Pro_Ahorros) THEN DO:
       MESSAGE "Crear un Nuevo Talonario Inactivará el Talonario que" SKIP
             "esta cuenta tenga activo." SKIP(1)
             "Desea seguir con la creación de un nuevo Talonario." VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO TITLE "Creación de un Nuevo Talonario"
              UPDATE choice AS LOGICAL.
       IF choice THEN DO:
          ENABLE Lib_Chequera.Tip_Talonario 
                 Lib_Chequera.Num_Inicial. 
                 /*Lib_Chequera.Num_Final.*/
          ASSIGN Num_Inicial:BGCOLOR = 15
                 Num_Inicial:FGCOLOR = 0
                 Lib_Chequera.Fec_Entrega:SCREEN-VALUE = STRING(W_Fecha).
                 /*Num_Final:BGCOLOR   = 15
                 Num_Final:FGCOLOR   = 0.*/
          APPLY "ENTRY" TO Lib_Chequera.Num_Inicial. /*validar si se cobra el talonario en salvar*/
       END.
    END.
 END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ahorros
&Scoped-define SELF-NAME Btn_Ingresar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ingresar wWin
ON CHOOSE OF Btn_Ingresar IN FRAME F_Ahorros /* Nuevos */
DO:
  ASSIGN Wk_Cambio1 = "" Wk_Cambio2 = "" Wk_Cambio3 = "" Wk_DatAnt = ""
  Ahorros.Id_tutor:SCREEN-VALUE IN FRAME F_Ahorros = ""
  nombre_Tutor:SCREEN-VALUE IN FRAME F_Ahorros = "".
    DO WITH FRAME F_Relaciones:
  FOR EACH T_Relaciones: DELETE T_Relaciones. END.

  CLOSE QUERY BR_Relaciones.
    END.
  RUN Inicializar_Variables.
  IF AVAIL Ahorros THEN Puntero = ROWID(Ahorros).
  RELEASE Ahorros.  
   NuevaCta = YES. 
  DISABLE Btn_Consulta BUTTON-92 Btn_Ingresar Btn_Borrar   WITH FRAME F_Ahorros.
  ENABLE Btn_Salvar Btn_Cancelar Btn_Deshacer Btn_Autorizados WITH FRAME F_Ahorros.
  ENABLE Ahorros.Nit Cmb_TipoProductos Cmb_Productos WITH FRAME F_Ahorros.
  Cmb_TipoProductos:SCREEN-VALUE = Cmb_TipoProductos:ENTRY(1).
  RUN Llenar_CmbProductos.
  Cmb_Productos:SCREEN-VALUE = Cmb_Productos:ENTRY(1).
  FIND FIRST Pro_Ahorros WHERE
       Pro_Ahorros.Tip_Ahorro = 1 AND Pro_Ahorros.Estado = 1 NO-LOCK NO-ERROR.
  Tpdt = 1. 
  ASSIGN Cmb_Perliquidacion:SENSITIVE = FALSE
         Ahorros.Exento_3xM:SCREEN-VALUE          = "No"
         Wk_Cambio1     = "" Ahorros.Id_Tesoro_Nacional:SCREEN-VALUE  = "No"
         Wk_Cambio2     = "" Ahorros.Id_Subsidio_Entidad:SCREEN-VALUE = "No"
         Wk_Cambio3     = "" Ahorros.Id_Mesada_Pensional:SCREEN-VALUE = "No"
         Wk_DatAnt      = "" Ahorros.Ind_Tipo_Subsidio:SCREEN-VALUE   = "1"
         WWin:SENSITIVE = TRUE.
  WWin:MOVE-TO-TOP().

  ASSIGN Ahorros.Cue_Ahorros:SCREEN-VALUE = "" Ahorros.Tarjetadb:SCREEN-VALUE = "".
     
  IF AVAIL Pro_Ahorros THEN Cmb_PerLiquidacion:SCREEN-VALUE = Cmb_PerLiquidacion:ENTRY(INT(Pro_Ahorros.Per_Liquidacion)).
  AT_Destino_Intereses:SENSITIVE IN FRAME F_ATermino = TRUE.
  APPLY 'entry' TO Cmb_TipoProductos.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Libretas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Libretas wWin
ON CHOOSE OF Btn_Libretas IN FRAME F_Ahorros /* Libretas/Chequeras */
DO:
DO WITH FRAME F_Libretas:
  IF Ahorros.Nit:SCREEN-VALUE IN FRAME F_Ahorros EQ "" THEN DO:
     MESSAGE "No se ha escogido la cuenta a la cual" SKIP
             "se le desea consultar las libretas."
             VIEW-AS ALERT-BOX INFORMATION.
     APPLY "entry" TO Btn_Consulta IN FRAME F_Ahorros.
     RETURN NO-APPLY.
  END.
  
  IF Pro_Ahorros.Id_Talonario EQ 3 THEN DO:
     MESSAGE "Este producto no maneja Talonarios ni Libretas"
             VIEW-AS ALERT-BOX INFORMATION.
     RETURN.        
  END.

  IF NOT AVAIL(Ahorros) THEN DO:
     MESSAGE "Seleccione una cuenta de Ahorros..." VIEW-AS ALERT-BOX.
     RETURN.
  END.
  
IF Pro_Ahorros.Id_Talonario NE 3 THEN DO:
  /*IF NOT NuevaCta THEN DO:*/
    Puntero = ROWID(Ahorros).
    HIDE FRAME F_Secundaria FRAME F_Relaciones.
    
    OPEN QUERY Br_Libretas FOR EACH Lib_Chequera WHERE 
                   Lib_Chequera.Cod_Producto EQ Ahorros.Cod_Ahorro AND
                   Lib_Chequera.Cue_Ahorros EQ Ahorros.Cue_Ahorros NO-LOCK INDEXED-REPOSITION.
                   
    IF AVAILABLE Lib_Chequera THEN 
       ASSIGN Lib_Chequera.Num_Inicial:SCREEN-VALUE = STRING(Lib_Chequera.Num_Inicial)
              Lib_Chequera.Num_Final:SCREEN-VALUE   = STRING(Lib_Chequera.Num_Final)
              Lib_Chequera.Fec_Entrega:SCREEN-VALUE = STRING(Lib_Chequera.Fec_Entrega)
              Lib_Chequera.Tip_Talonario:SENSITIVE  = FALSE
              Lib_Chequera.Num_Inicial:SENSITIVE    = FALSE. 
   ELSE
       ASSIGN Lib_Chequera.Num_Inicial:SCREEN-VALUE = "0"
              Lib_Chequera.Num_Final:SCREEN-VALUE   = "0"
              Lib_Chequera.Fec_Entrega:SCREEN-VALUE = "?".
    
    VIEW FRAME F_Libretas.
    /*RUN Deshabilitar_Fondo.*/
    ASSIGN FRAME F_Ahorros:SENSITIVE = FALSE.
  END.
ELSE
    MESSAGE "No se pueden matricular Libretas o Chequeras" SKIP
             VIEW-AS ALERT-BOX WARNING.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Libretas
&Scoped-define SELF-NAME BTN_MosLib
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_MosLib wWin
ON CHOOSE OF BTN_MosLib IN FRAME F_Libretas /* Mostrar Todos */
DO:
  OPEN QUERY BR_Libretas FOR EACH Lib_Chequera WHERE
     Lib_Chequera.Agencia       EQ Ahorros.Agencia AND
     Lib_Chequera.Cod_Producto  EQ INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Ahorros,1,3)) AND
     Lib_Chequera.Cue_Ahorros   EQ Ahorros.Cue_Ahorros:SCREEN-VALUE NO-LOCK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Relaciones
&Scoped-define SELF-NAME Btn_Outbene
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Outbene wWin
ON CHOOSE OF Btn_Outbene IN FRAME F_Relaciones /* Btn_outbene */
DO:
  HIDE FRAME F_Relaciones.
       /* Br_Relaciones:SCREEN-VALUE EQ " ". */
    W-Desctrf :SCREEN-VALUE EQ  " ".
  ASSIGN FRAME F_Ahorros:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Libretas
&Scoped-define SELF-NAME BTN_SalLib
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SalLib wWin
ON CHOOSE OF BTN_SalLib IN FRAME F_Libretas /* Salvar */
DO:
  DEFINE VARIABLE W_EstadoLib AS CHARACTER FORMAT "X(10)".
  DEFI   VAR      W_NoCobro   AS LOG INIT FALSE.
  DEFI   VAR      W_SiErr     AS LOG.

  DO WITH FRAME F_Libretas:
  /*  IF INTEGER(Lib_Chequera.Num_Final:SCREEN-VALUE) - INTEGER(Lib_Chequera.Num_Inicial:SCREEN-VALUE) NE 49 THEN DO:
         MESSAGE "No se acepta la creación del talonario........." SKIP
                 "El Nro de desprendibles es diferente al Control(#50)..." SKIP VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO Btn_IngLib.
         RETURN NO-APPLY.
      END.*/
  
      FIND FIRST Lib_Chequera WHERE Lib_Chequera.Agencia   EQ Ahorros.Agencia      AND
                               Lib_Chequera.Cod_Producto   EQ Ahorros.Cod_Ahorro AND 
                               Lib_Chequera.Cue_Ahorros    EQ Ahorros.Cue_Ahorros  AND
                               Lib_Chequera.Tip_Talonario  EQ INTEGER(Tip_Talonario:SCREEN-VALUE) AND
                               Lib_Chequera.Num_Inicial    EQ INTEGER(Num_Inicial:SCREEN-VALUE) AND
                               Lib_Chequera.Num_Final      EQ INTEGER(Num_Final:SCREEN-VALUE) 
                               NO-ERROR.
      IF AVAILABLE Lib_Chequera THEN DO:
         IF Lib_Chequera.Estado EQ 1 THEN
            W_EstadoLib = "Activo".
         ELSE
            W_EstadoLib = "Inactivo".
         MESSAGE "El Talonario que se pretende crear ya existe matriculado" SKIP
                 "para esta cuenta. y se encuentra: " W_EstadoLib SKIP(1)
                 "Se cancela la operación de ingreso!" VIEW-AS ALERT-BOX ERROR.
         APPLY 'choose' TO Btn_CanLib.
      END.
      ELSE DO:
         FIND FIRST Lib_Chequera WHERE Lib_Chequera.Agencia        EQ Ahorros.Agencia     AND
                                       Lib_Chequera.Cod_Producto   EQ Ahorros.Cod_Ahorro  AND 
                                       Lib_Chequera.Cue_Ahorros    EQ Ahorros.Cue_Ahorros NO-LOCK NO-ERROR.
         IF NOT AVAIL(Lib_Chequera) THEN
            W_NoCobro = TRUE.

         IF  Pro_Ahorros.Id_CobroTal /*AND INTEGER(SUBSTRING(Cmb_Estados:SCREEN-VALUE IN FRAME F_Ahorros,1,2)) NE 1*/ 
         AND Pro_Ahorros.Val_Talonario GT 0
         AND NOT W_NoCobro THEN DO:
             MESSAGE "El valor de la libreta o Talonario: $" STRING(Pro_Ahorros.Val_Talonario,">>>,>>>,>>9") SKIP
                     "                       Se Cobra SI o NO al Asociado...?" SKIP VIEW-AS ALERT-BOX 
                    QUESTION BUTTONS YES-NO TITLE "Confirmar Cobro Libreta"
                    UPDATE RptaCobro AS LOGICAL.
             IF NOT RptaCobro THEN DO:
                RUN P-ValiDarTrans IN W_Manija (OUTPUT W_SiErr,OUTPUT W_Autorizo).
                IF NOT W_SiErr THEN DO:
                   APPLY "choose" TO btn_CanLib.
                   RETURN NO-APPLY.
                END. 

                Choice = FALSE.                
             END.
             ELSE DO: 
                MESSAGE "Se debitara de la cuenta de Ahorros el valor de" SKIP
                    "la libreta o talonario: $" STRING(Pro_Ahorros.Val_Talonario,">>>,>>>,>>9") SKIP
                    "Autoriza el débito a la cuenta?" VIEW-AS ALERT-BOX INFORMATION
                    BUTTONS YES-NO UPDATE choice.
                IF choice THEN DO:
                   RUN Debitar_Talonario NO-ERROR.
                   IF ERROR-STATUS:ERROR THEN DO:
                      APPLY "choose" TO btn_CanLib.
                      RETURN NO-APPLY.
                   END.
                END.
                ELSE DO:
                   MESSAGE "Se cancela la creación del talonario" SKIP
                      "el pago de este no fue autorizado por" SKIP
                      "el usuario." VIEW-AS ALERT-BOX INFORMATION.
                   APPLY "entry" TO Btn_IngLib.
                   RETURN NO-APPLY.
                END.            
             END.
         END.
         
         /* Se inactiva la libreta anterior */
         FIND FIRST Lib_Chequera WHERE Lib_Chequera.Agencia  EQ Ahorros.Agencia
                                   AND Lib_Chequera.Cod_Producto EQ Ahorros.Cod_Ahorro
                                   AND Lib_Chequera.Cue_Ahorros EQ Ahorros.Cue_Ahorros
                                   AND Lib_Chequera.Estado EQ 1 NO-ERROR.
         IF AVAILABLE Lib_Chequera THEN
             ASSIGN Lib_Chequera.Estado = 2.

         CREATE Lib_Chequera.
         ASSIGN Lib_Chequera.Agencia      = Ahorros.Agencia
                Lib_Chequera.Cod_Producto = Ahorros.Cod_Ahorro
                Lib_Chequera.Cue_Ahorros  = Ahorros.Cue_Ahorros
                Lib_Chequera.Estado       = 1
                Lib_Chequera.Fec_Entrega  = W_Fecha
                Lib_Chequera.Num_Final    = INTEGER(Lib_Chequera.Num_Final:SCREEN-VALUE)
                Lib_Chequera.Num_Inicial  = INTEGER(Lib_Chequera.Num_Inicial:SCREEN-VALUE)
                Lib_Chequera.Tip_Talonario = INTEGER(Lib_Chequera.Tip_Talonario:SCREEN-VALUE)
                Lib_Chequera.Ult_Consec    = Lib_Chequera.Num_Inicial - 1.
         IF Choice THEN 
            Lib_Chequera.Pagada = YES.
         ELSE
            RUN Graba_HojaVida.  /* Feb.7/06 GAER.*/
      END.
      
      ASSIGN  Lib_Chequera.Num_Inicial:SCREEN-VALUE = "0"
              Lib_Chequera.Num_Final:SCREEN-VALUE   = "0"
              Lib_Chequera.Tip_Talonario:SCREEN-VALUE = "1".
              
      DISABLE Lib_Chequera.Num_Inicial Lib_Chequera.Num_Final Lib_Chequera.Tip_Talonario
              WITH FRAME F_Libretas.
              
      FIND CURRENT Lib_Chequera NO-LOCK NO-ERROR.
              
      APPLY 'choose' TO BTN_MosLib.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Relaciones
&Scoped-define SELF-NAME Btn_SalRel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_SalRel wWin
ON CHOOSE OF Btn_SalRel IN FRAME F_Relaciones /* Salvar */
DO:
/*     DEBUGGER:INITIATE().  */
/*     DEBUGGER:SET-BREAK().         */

    DO WITH FRAME F_Relaciones:
     FIND Varios WHERE Varios.Tipo EQ 3 AND Varios.Codigo EQ INT(R_Eleccion:SCREEN-VALUE) NO-LOCK NO-ERROR.
  /* JJMP - Controles TRF */
     FIND CURRENT Ahorros NO-ERROR.
     ASSIGN Ahorros.trf = INT(SUBSTR(Cmb_TRF:SCREEN-VALUE,1,2))
            Ahorros.TRF_notas = CAPS(TRIM(W-DescTRF:SCREEN-VALUE)).
     FIND CURRENT Ahorros NO-LOCK NO-ERROR.         
     IF Nit_Relacion:SCREEN-VALUE LE " " THEN RETURN.
     IF vlNewRel THEN DO: /* registro es nuevo */
        CREATE Relaciones.
        UPDATE Relaciones.Nit             = Ahorros.Nit:SCREEN-VALUE IN FRAME F_Ahorros
               Relaciones.Cod_Relacion    = INT(R_Eleccion:SCREEN-VALUE)
               Relaciones.Nit_Relacion    = Nit_Relacion:SCREEN-VALUE
               Relaciones.Val_Autorizado  = W_VrAutoriz
               Relaciones.Usuario         = W_Usuario
               Relaciones.Fec_Ingreso     = W_Fecha
               Relaciones.Descripcion     = R_Relacion2:SCREEN-VALUE
               Relaciones.Estado          = 1
               Relaciones.Clase_Producto  = 1
               Relaciones.Cod_Producto    = INT(SUBSTR(Cmb_Productos:SCREEN-VALUE IN FRAME F_Ahorros,1,3))
               Relaciones.Cuenta          = Cue_Ahorros:SCREEN-VALUE IN FRAME F_Ahorros.
        FIND Clientes WHERE Clientes.Nit EQ Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
        IF AVAIL(Clientes) THEN DO:
           CREATE T_Relaciones.
           UPDATE T_Relaciones.R_Relacion =  Varios.Descripcion
                  T_Relaciones.R_AgeObjeto = W_Agencia
                  T_Relaciones.R_NitObjeto = Relaciones.Nit_Relacion
                  T_Relaciones.R_VrAutoriz = Relaciones.Val_Autorizado
                  T_Relaciones.R_NomDescri = Relaciones.Descripcion
                  T_Relaciones.R_NomObjeto = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2
                  T_Relaciones.R_TelObjeto = Clientes.Tel_Residencia
                  T_Relaciones.R_ClasePdt  = 1
                  T_Relaciones.R_CodPdt    = INT(SUBSTR(Cmb_Productos:SCREEN-VALUE IN FRAME F_Ahorros,1,3))
                  T_Relaciones.R_CuePdt    = Cue_Ahorros:SCREEN-VALUE IN FRAME F_Ahorros.
        END.
     END.
     ELSE DO:
        FIND FIRST relaciones WHERE relaciones.nit EQ Ahorros.Nit:SCREEN-VALUE IN FRAME F_Ahorros
               AND Relaciones.Nit_Relacion    = Nit_Relacion:SCREEN-VALUE EXCLUSIVE-LOCK NO-ERROR.
        IF AVAIL relaciones THEN 
           UPDATE Relaciones.Nit             = Ahorros.Nit:SCREEN-VALUE IN FRAME F_Ahorros
                  Relaciones.Cod_Relacion    = INT(R_Eleccion:SCREEN-VALUE)
                  Relaciones.Nit_Relacion    = Nit_Relacion:SCREEN-VALUE
                  Relaciones.Val_Autorizado  = W_VrAutoriz
                  Relaciones.Usuario         = W_Usuario
                  Relaciones.Fec_Ingreso     = W_Fecha
                  Relaciones.Descripcion     = R_Relacion2:SCREEN-VALUE
                  Relaciones.Estado          = 1
                  Relaciones.Clase_Producto  = 1
                  Relaciones.Cod_Producto    = INT(SUBSTR(Cmb_Productos:SCREEN-VALUE IN FRAME F_Ahorros,1,3))
                  Relaciones.Cuenta          = Cue_Ahorros:SCREEN-VALUE IN FRAME F_Ahorros.
        APPLY "VALUE-CHANGED":U TO R_Eleccion.
     END.
     DISABLE Nit_Relacion /*Btn_SalRel*/ Btn_CanRel R_Relacion2. /* W_VrAutoriz.*/
     HIDE Nit_Relacion Lb_Nit  R_Relacion2.
     ASSIGN Nit_Relacion:SCREEN-VALUE = ""
            Nit_Relacion:FGCOLOR      = 17
            Nit_Relacion:BGCOLOR      = 17
            R_Relacion2:FGCOLOR          = 17
            R_Relacion2:BGCOLOR          = 17
            W_NomNitRelacion:BGCOLOR  = 17
            W_NomNitRelacion:SCREEN-VALUE = "".
   /*10/01/2005  Se graba en el titulo si se reclama individual o conjuntamente*/
   /*RELEASE Ahorros.*/
     FIND CURRENT Ahorros NO-ERROR.
     IF AVAIL(Ahorros) THEN DO:
        IF INT(R-reclamo:SCREEN-VALUE) EQ 1 THEN Ahorros.conjuncion = YES. 
                                                ELSE Ahorros.conjuncion = NO.
     END.
     OPEN QUERY BR_Relaciones FOR EACH T_Relaciones NO-LOCK INDEXED-REPOSITION.
     ASSIGN Btn_CreRel:SENSITIVE = TRUE. /* W_VrAutoriz:VISIBLE = FALSE.*/
     ASSIGN vlNewRel = FALSE.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ahorros
&Scoped-define SELF-NAME Btn_Salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salvar wWin
ON CHOOSE OF Btn_Salvar IN FRAME F_Ahorros /* Grabar */
DO:
    IF NuevaCta AND INTEGER(SUBSTRING(Cmb_TipoProductos:SCREEN-VALUE,1,1)) = 4 AND INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Ahorros,1,3)) = 2 THEN DO:
        /* Si se trata de una cuenta de Aportes revisamos que no exista ya una cuenta de este tipo activa para el mismo asociado. En caso que así sea, se rechaza la operación */

        FIND FIRST Ahorros WHERE Ahorros.Tip_Ahorro = 4
                             AND ahorros.cod_ahorro = 2
                             AND Ahorros.Nit = Ahorros.Nit:SCREEN-VALUE
                             AND Ahorros.Estado = 1 NO-LOCK NO-ERROR.
        IF AVAILABLE ahorros THEN DO:
            MESSAGE "Ya existe una cuenta de Aportes activa para este asociado."
                    "No se permite la operación."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            APPLY 'entry' TO Ahorros.Nit IN FRAME F_Ahorros.
            RETURN NO-APPLY.
        END.
    END.

    IF NuevaCta AND INTEGER(SUBSTRING(Cmb_TipoProductos:SCREEN-VALUE,1,1)) = 2 AND INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Ahorros,1,3)) = 3 THEN DO:
        /* Si se trata de una cuenta de Ahorro Permanente revisamos que no exista ya una cuenta de este tipo activa para el mismo asociado. En caso que así sea, se rechaza
        la operación */

        FIND FIRST Ahorros WHERE Ahorros.Tip_Ahorro = 2
                             AND ahorros.cod_ahorro = 3
                             AND Ahorros.Nit = Ahorros.Nit:SCREEN-VALUE
                             AND Ahorros.Estado = 1 NO-LOCK NO-ERROR.
        IF AVAILABLE ahorros THEN DO:
            MESSAGE "Ya existe una cuenta de Ahorro Permanente activa para este asociado."
                    "No se permite la operación."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            APPLY 'entry' TO Ahorros.Nit IN FRAME F_Ahorros.
            RETURN NO-APPLY.
        END.
    END.

    IF NuevaCta AND INT(SUBSTR(Cmb_TipoProductos:SCREEN-VALUE,1,1)) EQ 4 THEN DO:
        FIND FIRST Ahorros WHERE Ahorros.Tip_Ahorro EQ 4
                             AND Ahorros.Nit EQ Ahorros.Nit:SCREEN-VALUE
                             AND Ahorros.Estado EQ 1 NO-LOCK NO-ERROR.
        
        Ahorros.Exento_3xM:SCREEN-VALUE = "No".
    END.

    IF Ahorros.Nit:SCREEN-VALUE EQ "" THEN DO:
        MESSAGE "No se puede matricular una cuenta de Ahorros sin Nit"
            VIEW-AS ALERT-BOX ERROR.

        APPLY 'entry' TO Ahorros.Nit IN FRAME F_Ahorros.
        RETURN NO-APPLY.
    END.

    IF Ahorros.Cue_Ahorros:SCREEN-VALUE EQ "" AND Ahorros.Cue_Ahorros:SENSITIVE EQ YES THEN DO:
        MESSAGE "No se ha Ingresado Nro de la Cuenta de Ahorros"
            VIEW-AS ALERT-BOX ERROR.

        APPLY 'entry' TO Ahorros.Cue_Ahorros IN FRAME F_Ahorros.
        RETURN NO-APPLY.
    END.

    IF DEC(Ahorros.Tasa:SCREEN-VALUE) LE 0 AND SUBSTR(Cmb_TipoProductos:SCREEN-VALUE,1,1) NE "4" AND SUBSTR(Cmb_TipoProductos:SCREEN-VALUE,1,1) NE "1" THEN DO:
        IF Ahorros.Cod_Ahorro LT 24 THEN DO:
            MESSAGE "No se puede matricular una cuenta de Ahorros" SKIP
                    "si no se tiene configurada la Tasa. Rectifique!"
                VIEW-AS ALERT-BOX ERROR.

            RETURN NO-APPLY.
        END.
    END.

    IF DEC(Tasa_Efectiva:SCREEN-VALUE) LE 0 AND SUBSTR(Cmb_TipoProductos:SCREEN-VALUE,1,1) NE "4" AND SUBSTR(Cmb_TipoProductos:SCREEN-VALUE,1,1) NE "1" THEN DO:
        IF Ahorros.Cod_Ahorro LT 24 THEN DO:
            MESSAGE "No se puede matricular una cuenta de Ahorros" SKIP
                    "si no se tiene configurada la Tasa. Rectifique!"
                VIEW-AS ALERT-BOX ERROR.

            RETURN NO-APPLY.
        END.
    END.

    WEdad = (TODAY - Clientes.Fec_Nacimiento).

    IF WEdad < 6570 THEN DO:
        IF Ahorros.Id_Tutor:SCREEN-VALUE EQ " " THEN DO:
            MESSAGE "El Titular de la Cuenta es Menor de Edad y no se ha Seleccionado Representante Legal" skip(1)
                    "Fecha de Nacimiento: " Clientes.Fec_Nacimiento skip(1) "Edad: " WEdad
                VIEW-AS ALERT-BOX.

            RETURN NO-APPLY.
        END.

        IF Ahorros.Id_Tutor:SCREEN-VALUE = "Padre" OR Ahorros.Id_Tutor:SCREEN-VALUE = "Madre" THEN DO:
            FIND FIRST Relaciones WHERE Relaciones.Nit = Clientes.Nit
                                    AND Relaciones.Cod_Relacion = 3
                                    AND Relaciones.descripcion = Ahorros.Id_Tutor:SCREEN-VALUE
                                    AND Relaciones.Estado = 1 NO-LOCK NO-ERROR.
            IF NOT AVAIL Relaciones THEN DO:
                MESSAGE "No se Encontro la Relacion PADRE/MADRE Para el Titular de la Cuenta"
                    VIEW-AS ALERT-BOX.

                RETURN NO-APPLY.
            END.

            FIND FIRST Rela_Clientes WHERE Rela_Clientes.Nit = Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
            IF NOT AVAIL Rela_Clientes THEN DO:
                MESSAGE "No se ha Encontrado PADRE/MADRE del Titular en la Tabla CLIENTES"
                    VIEW-AS ALERT-BOX.

                RETURN NO-APPLY.
            END.
        END.
        ELSE DO:
            FIND FIRST Relaciones WHERE Relaciones.Nit = Clientes.Nit
                                    AND Relaciones.Nit_Relacion = Ahorros.Nit_Tutor:SCREEN-VALUE
                                    AND Relaciones.Cod_Relacion = 3
                                    AND Relaciones.Estado = 1 NO-LOCK NO-ERROR.
            IF NOT AVAIL Relaciones THEN DO:
                MESSAGE "No se Encontro la Relacion Representante Legal Para el Titular de la Cuenta"
                    VIEW-AS ALERT-BOX.

                RETURN NO-APPLY.
            END.

            FIND FIRST Rela_Clientes WHERE Rela_Clientes.Nit = Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
            IF NOT AVAIL Rela_Clientes THEN DO:
                MESSAGE "No se ha Encontrado Representante Legal del Titular en la Tabla CLIENTES"
                    VIEW-AS ALERT-BOX.

                RETURN NO-APPLY.
            END.
        END.
    END.

    SwError = FALSE.

    run verificaciones.

    IF SwError THEN DO:
        FIND FIRST TAhorros NO-ERROR.
        IF AVAIL TAhorros THEN
            ASSIGN Ahorros.Exento_3xm:SCREEN-VALUE = STRING(TAhorros.Exento_3xm)
                   Ahorros.Id_Subsidio_Entidad:SCREEN-VALUE = STRING(TAhorros.Id_Subsidio_Entidad)
                   Ahorros.Id_Mesada_Pensional:SCREEN-VALUE = STRING(TAhorros.Id_Mesada_Pensional)
                   Ahorros.Ind_Tipo_Subsidio:SCREEN-VALUE = STRING(TAhorros.Ind_Tipo_Subsidio)
                   Ahorros.Id_Tesoro_Nacional:SCREEN-VALUE = STRING(TAhorros.Id_Tesoro_Nacional)
                   Ahorros.Fec_Activacion[1]:SCREEN-VALUE = STRING(TAhorros.Fec_Activacion[1])
                   Ahorros.Fec_DesActivacion[1]:SCREEN-VALUE = STRING(TAhorros.Fec_DesActivacion[1])
                   Ahorros.Fec_Activacion[2]:SCREEN-VALUE = STRING(TAhorros.Fec_Activacion[2])
                   Ahorros.Fec_DesActivacion[2]:SCREEN-VALUE = STRING(TAhorros.Fec_DesActivacion[2])
                   Ahorros.Fec_Activacion[3]:SCREEN-VALUE = STRING(TAhorros.Fec_Activacion[3])
                   Ahorros.Fec_DesActivacion[3]:SCREEN-VALUE = STRING(TAhorros.Fec_DesActivacion[3])
                   Ahorros.Fec_Activacion[4]:SCREEN-VALUE = STRING(TAhorros.Fec_Activacion[4])
                   Ahorros.Fec_DesActivacion[4]:SCREEN-VALUE = STRING(TAhorros.Fec_DesActivacion[4]).
        ELSE
            ASSIGN Ahorros.Exento_3xm:SCREEN-VALUE = "No"
                   Ahorros.Id_Subsidio_Entidad:SCREEN-VALUE = "No"
                   Ahorros.Id_Mesada_Pensional:SCREEN-VALUE = "No"
                   Ahorros.Ind_Tipo_Subsidio:SCREEN-VALUE = "1"
                   Ahorros.Id_Tesoro_Nacional:SCREEN-VALUE = "No"
                   Ahorros.Fec_Activacion[1]:SCREEN-VALUE = ?
                   Ahorros.Fec_DesActivacion[1]:SCREEN-VALUE = ?
                   Ahorros.Fec_Activacion[2]:SCREEN-VALUE = ?
                   Ahorros.Fec_DesActivacion[2]:SCREEN-VALUE = ?
                   Ahorros.Fec_Activacion[3]:SCREEN-VALUE = ?
                   Ahorros.Fec_DesActivacion[3]:SCREEN-VALUE = ?
                   Ahorros.Fec_Activacion[4]:SCREEN-VALUE = ?
                   Ahorros.Fec_DesActivacion[4]:SCREEN-VALUE = ?.

        ENABLE Ahorros.Exento_3xm
               Ahorros.Id_Subsidio_Entidad
               Ahorros.Ind_Tipo_Subsidio
               Ahorros.Id_Tesoro_Nacional
               Ahorros.Id_Mesada_Pensional
            WITH FRAME F_Ahorros.

        RETURN NO-APPLY.
    END.

    DISABLE Cmb_TipoProductos
            Cmb_Productos
            Ahorros.Nit
            Ahorros.Cue_Ahorros
            Ahorros.Sdo_MinCta
            Ahorros.Num_Formato.

    DO WITH FRAME F_AlaVista:
        IF SUBSTR(Cmb_TipoProductos:SCREEN-VALUE,1,1) EQ "1" OR SUBSTR(Cmb_TipoProductos:SCREEN-VALUE,1,1) EQ "4" THEN DO:
            IF DEC(AV_Monto:SCREEN-VALUE) EQ 0 AND Wk_MontoApertura GT 0 AND NOT AVAIL(Ahorros) THEN DO:
                MESSAGE "No se puede matricular una cuenta de Ahorros" SKIP
                        "Si no se ha digitado el Monto Inicial." SKIP(1)
                        "El monto mínimo de apertura debe ser: $" STRING(Wk_MontoApertura,">>,>>>,>>9")
                    VIEW-AS ALERT-BOX ERROR.

                APPLY "entry" TO AV_Monto IN FRAME F_Alavista.

                RETURN NO-APPLY.
            END.
            ELSE
                IF NOT AVAIL(Ahorros) THEN DO:
                    IF DEC(AV_Monto:SCREEN-VALUE) LT Wk_MontoApertura THEN DO:
                        MESSAGE "El monto de apertura digitado es menor al monto" SKIP
                                "mínimo de apertura estipulado para este producto" SKIP
                                "el cual debe ser mínimo de: $" STRING(Wk_MontoApertura,">>,>>>,>>9")
                            VIEW-AS ALERT-BOX ERROR.

                        APPLY "entry" TO AV_Monto IN FRAME F_Alavista.
                        RETURN NO-APPLY.
                    END.
                END.

            IF AV_Cuota:SCREEN-VALUE EQ ? OR AV_Cuota:SCREEN-VALUE EQ "?" THEN
                AV_Cuota:SCREEN-VALUE = "0".

            IF DEC(AV_Cuota:SCREEN-VALUE) LE 0 AND INTEG(AP_FormaPago:SCREEN-VALUE) GT 1 AND (SUBSTR(Cmb_TipoProductos:SCREEN-VALUE,1,1) EQ "4" OR
                                                                                              SUBSTR(Cmb_TipoProductos:SCREEN-VALUE,1,1) EQ "2") THEN DO:
                MESSAGE "No se puede matricular esta cuenta de Ahorros" SKIP
                        "Si no se ha digitado el valor de la cuota, con forma pago Nòmina o Deb/Aut." SKIP(1)
                        "El valor mínimo de cuota es: $" STRING(Wk_Cuota,">>,>>>,>>9")
                    VIEW-AS ALERT-BOX ERROR.

                APPLY "entry" TO AV_Cuota IN FRAME F_Alavista.
                RETURN NO-APPLY.
            END.

            IF DEC(AV_Cuota:SCREEN-VALUE) EQ 0 AND Wk_Cuota GT 0 AND (SUBSTR(Cmb_TipoProductos:SCREEN-VALUE,1,1) EQ "4" OR SUBSTR(Cmb_TipoProductos:SCREEN-VALUE,1,1) EQ "2") THEN DO:
                MESSAGE "No se puede matricular esta cuenta de Ahorros" SKIP
                        "Si no se ha digitado el valor de la cuota." SKIP(1)
                        "El valor mínimo de cuota es: $" STRING(Wk_Cuota,">>,>>>,>>9")
                    VIEW-AS ALERT-BOX ERROR.

                APPLY "entry" TO AV_Cuota IN FRAME F_Alavista.
                RETURN NO-APPLY.
            END.
            ELSE DO:
                IF DEC(AV_Cuota:SCREEN-VALUE) LT Wk_Cuota AND (SUBSTR(Cmb_TipoProductos:SCREEN-VALUE,1,1) EQ "4" OR SUBSTR(Cmb_TipoProductos:SCREEN-VALUE,1,1) EQ "2") THEN DO:
                    MESSAGE "El valor mínimo de la cuota digitada es menor al" SKIP
                            "valor mínimo de cuota estipulado para este producto" SKIP
                            "el cual es: $" STRING(Wk_Cuota,">>,>>>,>>9")
                        VIEW-AS ALERT-BOX ERROR.

                    APPLY "entry" TO AV_Cuota IN FRAME F_Alavista.
                    RETURN NO-APPLY.
                END.
            END.

            IF AP_FormaPago:SCREEN-VALUE EQ "3" AND (AP_AgenciaDebito:SCREEN-VALUE  EQ "" OR AP_ProductoDebito:SCREEN-VALUE EQ "" OR AP_CuentaDebito:SCREEN-VALUE EQ "") THEN DO:
                MESSAGE "No se puede matricular esta cuenta de Ahorros" SKIP
                        "por que no se ha especificado la cuenta de Ahorros." SKIP
                        "de donde se hará el débito automático de la cuota" SKIP(1)
                        "A continuación podrá escoger la cuenta débito!"
                    VIEW-AS ALERT-BOX ERROR.

                APPLY "choose" TO BT_DebitoAP IN FRAME F_Alavista.
                RETURN NO-APPLY.
            END.

            IF AP_FormaPago:SCREEN-VALUE EQ "3" AND DATE(AP_FechaDebito:SCREEN-VALUE) EQ ? THEN DO:
                MESSAGE "Se debe entrar la fecha en la cual se hará" SKIP
                        "el débito automático. Rectifique la fecha!"
                    VIEW-AS ALERT-BOX ERROR.

                APPLY "entry" TO AP_FechaDebito IN FRAME F_Alavista.
                RETURN NO-APPLY.
            END.
        END.
    END.

    DO WITH FRAME F_Atermino:
        IF SUBSTR(Cmb_TipoProductos:SCREEN-VALUE,1,1) EQ "3" THEN DO:
            IF Cmb_PerLiquidacion:SCREEN-VALUE = "01 - Diario" THEN DO:
                MESSAGE "Producto A-Termino no se permite liquidacion Diaria."
                    VIEW-AS ALERT-BOX ERROR.

                RETURN NO-APPLY.
            END.

            IF DEC(AT_Monto:SCREEN-VALUE) EQ 0 THEN DO:
                MESSAGE "No se puede matricular una cuenta de Ahorros" SKIP
                        "si no se ha digitado el monto de Apertura. Rectifique!"
                    VIEW-AS ALERT-BOX ERROR.

                APPLY 'entry' TO AT_Monto.
                RETURN NO-APPLY.
            END.

            IF DEC(AT_Plazo:SCREEN-VALUE) EQ 0 THEN DO:
                MESSAGE "No se puede matricular una cuenta de Ahorros" SKIP
                        "si no se ha digitado el plazo. Rectifique!"
                    VIEW-AS ALERT-BOX ERROR.

                APPLY 'entry' TO AT_Plazo.
                RETURN NO-APPLY.
            END.

            IF AT_Destino_Intereses:SCREEN-VALUE NE "3" AND (AT_AgenciaDestino:SCREEN-VALUE EQ "" OR AT_ProductoDestino:SCREEN-VALUE EQ "" OR AT_CuentaDestino:SCREEN-VALUE EQ "") THEN DO:
                MESSAGE "No se puede matricular una cuenta de Ahorros" SKIP
                        "Si no se ha especificado la cuenta de Ahorros." SKIP
                        "o el crédito al cual se abonaran los intereses" SKIP
                        "Rectifique!"
                    VIEW-AS ALERT-BOX ERROR.

                APPLY 'entry' TO Bt_DesAT.
                RETURN NO-APPLY.
            END.
        END.
    END.

    DO WITH FRAME F_Contractual:
        IF SUBSTR(Cmb_TipoProductos:SCREEN-VALUE,1,1) EQ "2" THEN DO:
            IF DEC(CT_Monto:SCREEN-VALUE) EQ 0 THEN DO:
                MESSAGE "No se puede matricular una cuenta de Ahorros" SKIP
                        "si no se ha digitado el Monto de Apertura. Rectifique!"
                    VIEW-AS ALERT-BOX ERROR.

                APPLY 'entry' TO CT_Monto.
                RETURN NO-APPLY.
            END.

            IF DEC(CT_Cuota:SCREEN-VALUE) EQ 0 THEN DO:
                MESSAGE "No se puede matricular una cuenta de Ahorros" SKIP
                        "si no se ha digitado la Cuota. Rectifique!"
                    VIEW-AS ALERT-BOX ERROR.

                APPLY 'entry' TO CT_Cuota.
                RETURN NO-APPLY.
            END.

            IF INT(CT_Plazo:SCREEN-VALUE) EQ 0 THEN DO:
                MESSAGE "No se puede matricular una cuenta de Ahorros" SKIP
                        "si no se ha digitado el Plazo. Rectifique!"
                    VIEW-AS ALERT-BOX ERROR.

                CT_Plazo:SCREEN-VALUE ='30'.
                
                APPLY 'entry' TO CT_Plazo.
                RETURN NO-APPLY.
            END.

            IF CT_FormaPago:SCREEN-VALUE EQ "3" AND (CT_AgenciaDebito:SCREEN-VALUE  EQ "" OR
                                                     CT_ProductoDebito:SCREEN-VALUE EQ "" OR
                                                     CT_CuentaDebito:SCREEN-VALUE EQ "" OR
                                                     CT_FechaDebito:SCREEN-VALUE EQ "") THEN DO:
                MESSAGE "No se puede matricular una cuenta de Ahorros" SKIP
                        "No se ha escogido la cuenta de debito automatico. Rectifique!"
                    VIEW-AS ALERT-BOX ERROR.

                APPLY 'entry' TO BT_Debito.
                RETURN NO-APPLY.
            END.

            /*fecha debito*/
            IF CT_FormaPago:SCREEN-VALUE EQ "3" AND DATE(CT_FechaDebito:SCREEN-VALUE) EQ ? THEN DO:
                MESSAGE "Se debe entrar la fecha en la cual se hará" SKIP
                        "el débito automático. Rectifique la fecha!"
                    VIEW-AS ALERT-BOX ERROR.

                APPLY "entry" TO CT_FechaDebito IN FRAME F_Contractual.
                RETURN NO-APPLY.
            END.
        END.
    END.

    RUN Grabar_Datos.

    CASE SUBSTR(Cmb_TipoProductos:SCREEN-VALUE IN FRAME F_Ahorros,1,2):
        WHEN "2" THEN RUN Grabar_Contractual.
        WHEN "3" THEN RUN Grabar_Atermino.
        OTHERWISE RUN Grabar_AlaVista.
    END CASE.

    IF Wk_Cambio1 NE "" THEN RUN Grabar_Cambio (Wk_Cambio1,"ACT CTA").
    IF Wk_Cambio2 NE "" THEN RUN Grabar_Cambio (Wk_Cambio2,"ACT CTA").
    IF Wk_Cambio3 NE "" THEN RUN Grabar_Cambio (Wk_Cambio3,"ACT CTA").

    ASSIGN Wk_Cambio1 = ""
           Wk_Cambio2 = ""
           Wk_Cambio3 = ""
           Wk_DatAnt = ""
           NuevaCta = NO.

    IF Ahorros.Sdo_Dispon GT 0 OR Ahorros.Sdo_Canje GT 0 OR Ahorros.INT_Pagar  GT 0 OR Ahorros.INT_Causado GT 0 THEN
        ASSIGN Ahorros.Estado = 1
               Ahorros.Estado:SCREEN-VALUE = "1".

    ENABLE Btn_Consulta
           Btn_Ingresar
           Btn_Autorizados
           Btn_Libretas
        WITH FRAME F_Ahorros.

    DISABLE Btn_Deshacer
            Btn_Salvar
        WITH FRAME F_Ahorros.

    FIND CURRENT Ahorros NO-LOCK NO-ERROR.

    RUN Validar_Cambios. /* Valida si se Cambiaron Datos del GMF */

    ASSIGN Ahorros.Tasa:SENSITIVE = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Contractual
&Scoped-define SELF-NAME Bt_Debito
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Bt_Debito wWin
ON CHOOSE OF Bt_Debito IN FRAME F_Contractual /* Buscar Cuenta Debito */
DO:
    ENABLE Btn_Salvar WITH FRAME F_Ahorros.
    RUN C-Ahorros(INPUT Ahorros.Nit:SCREEN-VALUE IN FRAME F_Ahorros,
                    OUTPUT W_Age, OUTPUT W_Pro, OUTPUT W_Nit, OUTPUT W_Cue).

    IF W_Age NE 0 AND W_Pro NE 0 AND W_Nit NE "" AND W_Cue NE "" THEN DO:
          FIND Agencias WHERE Agencias.Agencia EQ W_Age AND Agencias.Estado EQ 1 NO-LOCK NO-ERROR.
          IF AVAILABLE(Agencias) THEN
             CT_AgenciaDebito:SCREEN-VALUE IN FRAME F_Contractual = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.
          PunteroP = ROWID(Pro_Ahorros).
          FIND Pro_Ahorros WHERE 
               Pro_Ahorros.Cod_Ahorro   EQ W_Pro AND
               Pro_Ahorros.Estado       EQ 1 NO-LOCK NO-ERROR.
          IF AVAILABLE(Pro_Ahorros) THEN DO:
             IF NOT Pro_Ahorros.Id_Debito THEN DO:
                MESSAGE "El producto al cual pertenece la cuenta escogida" SKIP
                        "no permite el débito automático. Escoja una cuenta" SKIP
                        "de un producto diferente a este" VIEW-AS ALERT-BOX INFORMATION.
                FIND Pro_Ahorros WHERE ROWID(Pro_Ahorros) EQ punteroP NO-LOCK NO-ERROR.
                APPLY "choose" TO Bt_Debito IN FRAME F_Contractual.
                RETURN NO-APPLY.
             END.
             CT_ProductoDebito:SCREEN-VALUE IN FRAME F_Contractual = STRING(Pro_Ahorros.Cod_Ahorro,"999") + " - " + Pro_Ahorros.Nom_Producto.
          END.
          ELSE
             CT_ProductoDebito:SCREEN-VALUE IN FRAME F_Contractual = "No encontro pproducto de Ahorros".
          FIND Pro_Ahorros WHERE ROWID(Pro_Ahorros) EQ punteroP NO-LOCK NO-ERROR.
          FIND Clientes WHERE Clientes.Nit EQ W_Nit AND Clientes.Estado EQ 1 NO-LOCK NO-ERROR.
          IF AVAILABLE(Clientes) THEN
             CT_NombreDebito:SCREEN-VALUE IN FRAME F_Contractual = CAPS(Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2).
          CT_CuentaDebito:SCREEN-VALUE IN FRAME F_Contractual = W_Cue.
      END.
      ELSE DO:
         MESSAGE "Inconsistencia en la Busqueda de la Cuenta Débito" VIEW-AS ALERT-BOX.       
         DISABLE Bt_Debito CT_FechaDebito WITH FRAME F_Contractual.
      END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Alavista
&Scoped-define SELF-NAME Bt_DebitoAP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Bt_DebitoAP wWin
ON CHOOSE OF Bt_DebitoAP IN FRAME F_Alavista /* Buscar C/ta Debito */
DO:
    ENABLE Btn_Salvar WITH FRAME F_Ahorros.
    RUN C-Ahorros(INPUT Ahorros.Nit:SCREEN-VALUE IN FRAME F_Ahorros,
                    OUTPUT W_Age, OUTPUT W_Pro, OUTPUT W_Nit, OUTPUT W_Cue).
    IF W_Age NE 0 AND W_Pro NE 0 AND W_Nit NE "" AND W_Cue NE "" THEN DO:
          FIND Agencias WHERE Agencias.Agencia EQ W_Age AND Agencias.Estado EQ 1 NO-LOCK NO-ERROR.
          IF AVAILABLE(Agencias) THEN
             AP_AgenciaDebito:SCREEN-VALUE IN FRAME F_AlaVista = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.

          PunteroP = ROWID(Pro_Ahorros).

          FIND Pro_Ahorros WHERE 
               Pro_Ahorros.Cod_Ahorro   EQ W_Pro AND
               Pro_Ahorros.Estado       EQ 1 NO-LOCK NO-ERROR.
          IF AVAILABLE(Pro_Ahorros) THEN DO:
             IF Pro_Ahorros.Id_Debito THEN
                AP_ProductoDebito:SCREEN-VALUE IN FRAME F_AlaVista = STRING(Pro_Ahorros.Cod_Ahorro,"999") + " - " + Pro_Ahorros.Nom_Producto.
             ELSE DO:
                MESSAGE "El producto al cual pertenece la cuenta escogida" SKIP
                        "no permite el débito automático. Escoja una cuenta" SKIP
                        "de un producto diferente a este" VIEW-AS ALERT-BOX INFORMATION.
                AP_FormaPago:SCREEN-VALUE = "1".
                DISABLE Bt_DebitoAP AP_FechaDebito WITH FRAME F_AlaVista.
             END.
          
          END.
          ELSE
             AP_ProductoDebito:SCREEN-VALUE IN FRAME F_AlaVista = "No encontro pproducto de Ahorros".

          FIND Pro_Ahorros WHERE ROWID(Pro_Ahorros) EQ punteroP NO-LOCK NO-ERROR.
          FIND Clientes WHERE Clientes.Nit EQ W_Nit AND Clientes.Estado EQ 1 NO-LOCK NO-ERROR.
          IF AVAILABLE(Clientes) THEN
             AP_NombreDebito:SCREEN-VALUE IN FRAME F_AlaVista = CAPS(Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2).
          AP_CuentaDebito:SCREEN-VALUE IN FRAME F_AlaVista = W_Cue.
      END.
      ELSE DO:
         MESSAGE "Inconsistencia en la Busqueda de la Cuenta Débito" VIEW-AS ALERT-BOX.
         AP_FormaPago:SCREEN-VALUE = "1".
         DISABLE Bt_DebitoAP AP_FechaDebito WITH FRAME F_AlaVista.
      END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Atermino
&Scoped-define SELF-NAME BT_DesAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BT_DesAT wWin
ON CHOOSE OF BT_DesAT IN FRAME F_Atermino /* Buscar Cuenta / Pagare */
DO:
  CASE AT_Destino_Intereses:SCREEN-VALUE IN FRAME F_Atermino:
      WHEN "1" THEN RUN AT_Destino_Ahorros.
      WHEN "2" THEN RUN AT_Destino_Creditos.
      WHEN "3" THEN ASSIGN AT_AgenciaDestino:SCREEN-VALUE IN FRAME F_Atermino = STRING(w_agencia)
                           AT_ProductoDestino:SCREEN-VALUE                    = ""
                           AT_CuentaDestino:SCREEN-VALUE                      = ""
                           AT_NombreDestino:SCREEN-VALUE                      = "".
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ahorros
&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 wWin
ON CHOOSE OF BUTTON-1 IN FRAME F_Ahorros /* Button 1 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Libretas
&Scoped-define SELF-NAME BUTTON-121
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-121 wWin
ON CHOOSE OF BUTTON-121 IN FRAME F_Libretas /* Button 121 */
DO:
  /*RUN Habilitar_Fondo.*/
  HIDE FRAME F_Libretas.
  ASSIGN FRAME F_Ahorros:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Secundaria
&Scoped-define SELF-NAME BUTTON-122
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-122 wWin
ON CHOOSE OF BUTTON-122 IN FRAME F_Secundaria /* Button 122 */
DO:
  HIDE FRAME F_Secundaria.
  FRAME F_Ahorros:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ahorros
&Scoped-define SELF-NAME BUTTON-123
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-123 wWin
ON CHOOSE OF BUTTON-123 IN FRAME F_Ahorros /* Button 123 */
DO:
         
        InputFile = "Formatos\AV - 303.xls".
      
      IF SwExiste EQ ?  THEN  DO:
                  MESSAGE InputFile "no Encontrado" VIEW-AS ALERT-BOX.
                 RETURN NO-APPLY.
      END.
      ELSE DO:
          ASSIGN Fecha_GMF[1] = Ahorros.Fec_Activacion[1]
                   Fecha_GMF[2] = Ahorros.Fec_DesActivacion[1].

        IF  AVAIL Ahorros AND  ahorros.fec_activacion[1] = ? THEN DO:                     
        MESSAGE "favor grabar o Marcar la cuenta"  VIEW-AS ALERT-BOX INFO BUTTON OK.
        RETURN NO-APPLY.
         END.
        ELSE DO:
            IF  AVAIL Ahorros AND  ahorros.fec_desactivacion[1] = ? AND
                 ahorros.exento_3xm:SCREEN-VALUE IN FRAME f_ahorros = "No" THEN DO:
                 MESSAGE "favor grabar o Desmarcar la cuenta"  VIEW-AS ALERT-BOX INFO BUTTON OK.
                  RETURN NO-APPLY.
            END.
            ELSE DO:
                RUN Abrir_Excel.
            RUN Imprime_Formato_GMF.
            END.                    
         END.
      END.
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 wWin
ON CHOOSE OF BUTTON-2 IN FRAME F_Ahorros /* Button 2 */
DO:
     IF NOT AVAIL Ahorros THEN
        MESSAGE "El Registro No Existe. Salvelo Para Poder Imprimirlo" VIEW-AS ALERT-BOX.
     IF AVAIL Ahorros AND Ahorros.tip_Ahorro = 1 THEN DO:
        InputFile = "Formatos\AV - 300.xls".
        RUN Abrir_Excel.
        RUN Imprime_Formato_Apertura.
       /* RUN Cerrar_Excel. */
     END.
/*   DEF VAR W_Row_Id AS ROWID.                                                                           */
/*   DEF VAR Listado  AS CHA INIT "".                                                                     */
/*   DEF VAR W_TasEfe AS DEC INIT 0.0.                                                                    */
/*   DEF VAR W_ArcSal AS CHA FORMAT "X(30)".                                                              */
/* /*ASSIGN W_ArcSal = W_Path + TRIM(Ahorros.cue_Ahorro) + ".txt" .*/                                     */
/*   ASSIGN W_Row_Id = ROWID(Ahorros) W_TasEfe = DEC(Tasa_Efectiva:SCREEN-VALUE).                         */
/*   IF Ahorros.Cod_Ahorro EQ 4 THEN DO:                                                                  */
/*      ASSIGN W_ArcSal = "\\172.28.1.201\d\tempo\CDAT" + TRIM(Ahorros.cue_Ahorro) + ".txt" .             */
/*      IF SEARCH(W_ArcSal) EQ ? AND Ahorros.Cod_Ahorro EQ 4 AND Ahorros.Estado EQ 1                      */
/*                               AND Ahorros.Monto_Apertura LE Ahorros.Sdo_Disponible                     */
/*                               AND Ahorros.Fec_Prorroga = ? AND Fec_apertura GT DATE(5,1,2005) THEN DO: */
/*         MESSAGE "Imprimir Titulo Valor   (Si) o  (No)" VIEW-AS ALERT-BOX QUESTION                      */
/*                  BUTTONS YES-NO TITLE 'Eleccion de Impresiòn' UPDATE Imp_Cdat AS LOG.                  */
/*         IF Imp_Cdat THEN DO:                                                                           */
/*            RUN formatos.r (INPUT "CDAT", W_Row_Id, W_ArcSal, W_TasEfe, "", 0, 0 ).                     */
/*            APPLY "ENTRY" TO Cmb_TipoProductos IN FRAME F_Ahorros.                                      */
/*         END.                                                                                           */
/*      END.                                                                                              */
/*   END.                                                                                                 */
/*   IF Ahorros.Cod_Ahorro EQ 12 THEN DO:                                                                 */
/*      ASSIGN W_ArcSal = "\\172.28.1.201\d\tempo\TAC" + TRIM(Ahorros.cue_Ahorro) + ".txt" .              */
/*      IF SEARCH(W_ArcSal) EQ ? AND Ahorros.Cod_Ahorro EQ 12 AND Ahorros.Estado EQ 1                     */
/*                               AND Ahorros.Monto_Apertura LE Ahorros.Sdo_Disponible THEN DO:            */
/*         MESSAGE "Imprimir Titulo Valor   (Si) o  (No)" VIEW-AS ALERT-BOX QUESTION                      */
/*                  BUTTONS YES-NO TITLE 'Eleccion de Impresiòn' UPDATE Imp_Semi AS LOG.                  */
/*         IF Imp_Semi THEN DO:                                                                           */
/*            RUN formatos.r (INPUT "TAC", W_Row_Id, W_ArcSal, W_TasEfe, "", 0, 0 ).                      */
/*            APPLY "ENTRY" TO Cmb_TipoProductos IN FRAME F_Ahorros.                                      */
/*         END.                                                                                           */
/*      END.                                                                                              */
/*   END.                                                                                                 */
/*   W_Inf = "Cuenta".                                                                                    */
/*   Listado = W_Pathspl + "Lst_Cuenta.lst".                                                              */
/*  {incluido/imprimir.i "Listado"}.                                                                      */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-92
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-92 wWin
ON CHOOSE OF BUTTON-92 IN FRAME F_Ahorros /* Fechas y Saldos */
DO:
  HIDE FRAME F_Libretas FRAME F_Relaciones.

  IF AVAILABLE Ahorros AND Ahorros.Nit:SCREEN-VALUE IN FRAME F_Ahorros NE "" THEN 
     ASSIGN FRAME F_Ahorros:SENSITIVE    = FALSE
            FRAME F_Secundaria:VISIBLE   = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_PerLiquidacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_PerLiquidacion wWin
ON ENTRY OF Cmb_PerLiquidacion IN FRAME F_Ahorros /* Periodo de Liquidacion */
DO:
   APPLY "value-changed" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_PerLiquidacion wWin
ON LEAVE OF Cmb_PerLiquidacion IN FRAME F_Ahorros /* Periodo de Liquidacion */
DO:
  APPLY "value-changed" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_PerLiquidacion wWin
ON VALUE-CHANGED OF Cmb_PerLiquidacion IN FRAME F_Ahorros /* Periodo de Liquidacion */
DO:
  DEFI VAR W_ValC LIKE Ahorros.Cuota.
  
  W_ValC = AT_Plazo / 30. 
  
  IF  W_ValC NE ROUND(W_ValC,0) 
  AND INTEGER(SUBSTRING(Cmb_TipoProductos:SCREEN-VALUE IN FRAME F_Ahorros,1,1)) EQ 3
  AND INTEGER(SUBSTRING(SELF:SCREEN-VALUE,1,2)) GT 1 
  AND INTEGER(SUBSTRING(SELF:SCREEN-VALUE,1,2)) LT 6   THEN
      ASSIGN Cmb_PerLiquidacion:SCREEN-VALUE = "06 - Al Vencimiento".
  ELSE IF  INTEGER(SUBSTRING(Cmb_TipoProductos:SCREEN-VALUE IN FRAME F_Ahorros,1,1)) EQ 3
       AND INTEGER(SUBSTRING(SELF:SCREEN-VALUE,1,2)) GT 1 
       AND INTEGER(SUBSTRING(SELF:SCREEN-VALUE,1,2)) LT 6   
       AND W_ValC / INTEGER(SUBSTRING(SELF:SCREEN-VALUE,1,2)) LT 0 THEN
           ASSIGN Cmb_PerLiquidacion:SCREEN-VALUE = "06 - Al Vencimiento".
  ELSE IF  INTEGER(SUBSTRING(Cmb_TipoProductos:SCREEN-VALUE IN FRAME F_Ahorros,1,1)) EQ 3
       AND INTEGER(SUBSTRING(SELF:SCREEN-VALUE,1,2)) GT 1 
       AND INTEGER(SUBSTRING(SELF:SCREEN-VALUE,1,2)) LT 6   THEN DO:      
           IF AT_Plazo LE 60 AND INTEGER(SUBSTRING(SELF:SCREEN-VALUE,1,2)) NE 2 THEN
              ASSIGN Cmb_PerLiquidacion:SCREEN-VALUE = "06 - Al Vencimiento".
           ELSE IF AT_Plazo LE 90 AND INTEGER(SUBSTRING(SELF:SCREEN-VALUE,1,2)) NE 2
                                  AND INTEGER(SUBSTRING(SELF:SCREEN-VALUE,1,2)) NE 3 THEN
              ASSIGN Cmb_PerLiquidacion:SCREEN-VALUE = "06 - Al Vencimiento".
           ELSE IF AT_Plazo LE 180 AND INTEGER(SUBSTRING(SELF:SCREEN-VALUE,1,2)) NE 2
                                   AND INTEGER(SUBSTRING(SELF:SCREEN-VALUE,1,2)) NE 3
                                   AND INTEGER(SUBSTRING(SELF:SCREEN-VALUE,1,2)) NE 4 THEN DO:
               ASSIGN Cmb_PerLiquidacion:SCREEN-VALUE = "06 - Al Vencimiento".
           END.
           ELSE IF AT_Plazo EQ 270 AND INTEGER(SUBSTRING(SELF:SCREEN-VALUE,1,2)) NE 2
                                   AND INTEGER(SUBSTRING(SELF:SCREEN-VALUE,1,2)) NE 3 THEN 
              ASSIGN Cmb_PerLiquidacion:SCREEN-VALUE = "06 - Al Vencimiento".
           ELSE IF AT_Plazo EQ 360 AND INTEGER(SUBSTRING(SELF:SCREEN-VALUE,1,2)) NE 2
                                   AND INTEGER(SUBSTRING(SELF:SCREEN-VALUE,1,2)) NE 3
                                   AND INTEGER(SUBSTRING(SELF:SCREEN-VALUE,1,2)) NE 4 
                                   AND INTEGER(SUBSTRING(SELF:SCREEN-VALUE,1,2)) NE 5 THEN 
              ASSIGN Cmb_PerLiquidacion:SCREEN-VALUE = "06 - Al Vencimiento".
           
           IF (   AT_Plazo EQ 120 OR AT_Plazo EQ 150 OR AT_Plazo EQ 210 OR AT_Plazo EQ 240 
               OR AT_Plazo EQ 300 OR AT_Plazo EQ 330)
               AND INTEGER(SUBSTRING(SELF:SCREEN-VALUE,1,2)) NE 2 THEN
              ASSIGN Cmb_PerLiquidacion:SCREEN-VALUE = "06 - Al Vencimiento".
  
  END.       

 /* APPLY "Leave" TO AT_Plazo      IN FRAME F_ATermino. */
  APPLY "leave" TO Tasa_Efectiva IN FRAME F_Ahorros.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Productos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Productos wWin
ON LEAVE OF Cmb_Productos IN FRAME F_Ahorros
DO:
    FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Tip_Ahorro = INT(SUBSTR(Cmb_TipoProductos:SCREEN-VALUE,1,1))
                             AND Pro_Ahorros.Cod_Ahorro = INT(SUBSTR(Cmb_Productos:SCREEN-VALUE IN FRAME F_Ahorros,1,3)) NO-LOCK NO-ERROR.
    IF AVAIL Pro_Ahorros THEN DO:
        ASSIGN Wk_MontoApertura = 0
               Wk_IdMonApe = NO
               Wk_Cuota = 0.

        IF Pro_Ahorros.Id_MonApertura THEN
            ASSIGN Wk_MontoApertura = Pro_Ahorros.Val_MonAper
                   Wk_IdMonApe = YES.

        IF Pro_Ahorros.Id_Cuota THEN
            Wk_Cuota = Pro_Ahorros.Val_Cuota.

        RUN Mostrar_Registro.

        Cmb_PerLiquidacion:SCREEN-VALUE = Cmb_PerLiquidacion:ENTRY(INTEGER(Pro_Ahorros.Per_Liquidacion)).

        IF Tpdt EQ 2 THEN DO:
            ASSIGN Cmb_Perliquidacion:SCREEN-VALUE = "06 - Al Vencimiento"
                   Imp_Utramas:SENSITIVE = TRUE
                   FRAME F_Contractual:VISIBLE = TRUE.
        END.
        ELSE
            IF Tpdt EQ 1 THEN DO:
                AV_MONTO:SCREEN-VALUE IN FRAME F_ALAVISTA = "0".

                APPLY 'leave' TO AV_MONTO IN FRAME F_ALAVISTA.
            END.
    END.
    ELSE DO:
        MESSAGE "Producto Seleccionado No Encontado"
            VIEW-AS ALERT-BOX.

        RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Productos wWin
ON VALUE-CHANGED OF Cmb_Productos IN FRAME F_Ahorros
DO:
    APPLY "leave" TO Cmb_Productos.

    IF SUBSTRING(cmb_tipoProductos:SCREEN-VALUE,1,1) = "3" THEN DO:
        IF INTEGER(SUBSTRING(cmb_productos:SCREEN-VALUE,1,3)) = 5 THEN DO:
            F-FecVenc:SCREEN-VALUE IN FRAME F_ATermino = STRING(ADD-INTERVAL(w_fecha,3,"months")).
            /*AT_Plazo:SCREEN-VALUE = "90".*/
        END.
        
        IF INTEGER(SUBSTRING(cmb_productos:SCREEN-VALUE,1,3)) = 6 THEN DO:
            F-FecVenc:SCREEN-VALUE = STRING(ADD-INTERVAL(w_fecha,6,"months")).
            /*AT_Plazo:SCREEN-VALUE = "180".*/
        END.

        ASSIGN F-FecVenc.
        AT_Plazo:SCREEN-VALUE = STRING(INTERVAL(F-FecVenc,W_fecha,"days")).
        
        AT_Plazo:SENSITIVE = FALSE.
        F-FecVenc:SENSITIVE = FALSE.
        VIEW FRAME F_Atermino.
    END.
    ELSE DO:
        AT_Plazo:SENSITIVE = TRUE.
        F-FecVenc:SENSITIVE = TRUE.
        AT_Plazo:SCREEN-VALUE = STRING(0).
        F-FecVenc:SCREEN-VALUE = "".
    END.

    ASSIGN F-FecVenc.
    ASSIGN AT_plazo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_TipoProductos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_TipoProductos wWin
ON VALUE-CHANGED OF Cmb_TipoProductos IN FRAME F_Ahorros /* Productos */
DO:
 
     RUN Llenar_CmbProductos.
     Tpdt = INT(SUBSTR(Cmb_TipoProductos:SCREEN-VALUE,1,1)).
/*
     FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ INT(SUBSTR(Cmb_Productos:SCREEN-VALUE,1,3)) NO-LOCK NO-ERROR.
     IF AVAIL Pro_Ahorros THEN DO: 
        ASSIGN Cmb_Productos:SCREEN-VALUE = STRING(Pro_Ahorros.Cod_Ahorro,"999") + " - " + Pro_Ahorros.Nom_Producto.              
     /* IF Pro_Ahorro.Id_PerLiquidacion EQ 1 OR Pro_Ahorro.Id_PerLiquidacion EQ 3 THEN */
      
     END.
      Cmb_PerLiquidacion:SCREEN-VALUE = Cmb_PerLiquidacion:ENTRY(INT(Pro_Ahorros.Per_Liquidacion)).
    IF  NuevaCta THEN Cmb_Productos:SCREEN-VALUE = Cmb_Productos:ENTRY(2).*/
                               

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Relaciones
&Scoped-define SELF-NAME Cmb_TRF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_TRF wWin
ON VALUE-CHANGED OF Cmb_TRF IN FRAME F_Relaciones /* TRF */
DO:
  DO WITH FRAME F_Relaciones:
  FOR EACH T_Relaciones: DELETE T_Relaciones. END.
  CLOSE QUERY BR_Relaciones.
  
  IF  R_Eleccion EQ 4 THEN DO:
       ASSIGN W_VrAutoriz:SCREEN-VALUE = "0"
            W_VrAutoriz
            W_VrAutoriz:VISIBLE      = FALSE.
       FIND Varios WHERE Varios.Tipo EQ 3 AND Varios.Codigo EQ 4 NO-LOCK NO-ERROR.
  END.
  ELSE  DO: 
      W_VrAutoriz:VISIBLE = TRUE.           
  
  FIND Varios WHERE Varios.Tipo EQ 3 AND Varios.Codigo EQ 7 NO-LOCK NO-ERROR.
  END.
    FOR EACH Relaciones WHERE 
           Relaciones.Nit EQ Ahorros.Nit:SCREEN-VALUE IN FRAME F_Ahorros AND 
           Relaciones.Cod_Relacion  EQ  R_Eleccion AND 
           Relaciones.Clase_Producto EQ 1 AND
           Relaciones.Cod_Producto   EQ INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE,1,3)) AND
           Relaciones.Cuenta         EQ Cue_Ahorros:SCREEN-VALUE IN FRAME F_Ahorros AND
           Relaciones.Estado EQ INTEGER(RActivas:SCREEN-VALUE) NO-LOCK:

          FIND Clientes WHERE Clientes.Nit EQ Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
          IF AVAILABLE(Clientes) THEN DO:
             CREATE T_Relaciones.
             UPDATE T_Relaciones.R_Relacion  = Varios.Descripcion
                    T_Relaciones.R_AgeObjeto = Clientes.Agencia
                    T_Relaciones.R_NitObjeto = Relaciones.Nit_Relacion
                    T_Relaciones.R_VrAutoriz = Relaciones.Val_Autoriz
                    T_Relaciones.R_NomObjeto = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2
                    T_Relaciones.R_NomDescri = Relaciones.Descripcion
                    T_Relaciones.R_TelObjeto = Clientes.Tel_Residencia
                    T_Relaciones.R_CodPdt    = Relaciones.Cod_Producto
                    T_Relaciones.Est         = Relaciones.Estado.
          END.
  END.     
  OPEN QUERY BR_Relaciones FOR EACH T_Relaciones NO-LOCK INDEXED-REPOSITION.  
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Contractual
&Scoped-define SELF-NAME CT_Cuota
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CT_Cuota wWin
ON LEAVE OF CT_Cuota IN FRAME F_Contractual /* Cuota */
DO:
  IF AVAILABLE Ahorros AND DEC(SELF:SCREEN-VALUE) NE Ahorros.Cuota THEN
     Wk_Cambio1 = " - Cambio Cuota: $" + SELF:SCREEN-VALUE.
  IF Pro_Ahorros.Id_Cuota AND Pro_Ahorros.Val_Cuota GT DEC(SELF:SCREEN-VALUE) THEN DO:
     MESSAGE "La Cuota minima para este producto" SKIP
             "es de $: " STRING(Pro_Ahorros.Val_Cuota,">>>,>>>,>>9")
              VIEW-AS ALERT-BOX INFORMATION.
     SELF:SCREEN-VALUE = STRING(Pro_Ahorros.Val_Cuota).
     APPLY "entry" TO Ct_Cuota IN FRAME F_Contractual.
     RETURN NO-APPLY.
  END.
  
  IF AVAILABLE Ahorros AND Ahorros.Detalle_Estado EQ 1 THEN
     ENABLE Btn_Salvar WITH FRAME F_Ahorros.

  
  /*ASSIGN Ct_Monto:SCREEN-VALUE = SELF:SCREEN-VALUE.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CT_Cuota wWin
ON VALUE-CHANGED OF CT_Cuota IN FRAME F_Contractual /* Cuota */
DO:
  Wk_Cambio1 = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CT_FechaDebito
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CT_FechaDebito wWin
ON LEAVE OF CT_FechaDebito IN FRAME F_Contractual /* Fecha Débito */
DO:
  IF DATE(SELF:SCREEN-VALUE) LT W_Fecha THEN DO:
     MESSAGE "La fecha a realizar el débito automático debe ser" SKIP
             "mayor o igual a la fecha actual. digite de nuevo " SKIP
             "la fecha a debitar!" VIEW-AS ALERT-BOX INFORMATION.
     SELF:SCREEN-VALUE = STRING(W_Fecha).
     APPLY "entry" TO CT_FechaDebito IN FRAME F_Contractual.
     RETURN NO-APPLY.
  END.
  ENABLE Btn_Salvar WITH FRAME F_Ahorros.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CT_FormaPago
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CT_FormaPago wWin
ON LEAVE OF CT_FormaPago IN FRAME F_Contractual
DO:
  IF AVAILABLE Ahorros AND Ahorros.For_Pago NE DEC(SELF:SCREEN-VALUE) THEN DO:
     CASE SELF:SCREEN-VALUE:
       WHEN "1" THEN Wk_DatAnt = "CAJA".
       WHEN "2" THEN Wk_DatAnt = "NÓMINA".
       WHEN "3" THEN Wk_DatAnt = "DEB.AUTOMÁTICO".
     END CASE.
     Wk_Cambio3 = " - Nvo.Per.Deducción: " + Wk_DatAnt.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CT_FormaPago wWin
ON VALUE-CHANGED OF CT_FormaPago IN FRAME F_Contractual
DO:
  ASSIGN Wk_Cambio3   = ""
         Wk_PerPagEmp = 0.
            
  ENABLE Btn_Salvar WITH FRAME F_Ahorros.
  IF CT_FormaPago:SCREEN-VALUE IN FRAME F_Contractual EQ "3" THEN DO:
      ENABLE BT_Debito CT_FechaDebito WITH FRAME F_Contractual.
      ENABLE Imp_Cdat WITH FRAME f_ahorros.
  END.
  ELSE DO:
     DISABLE BT_Debito CT_FechaDebito WITH FRAME F_Contractual.
     ASSIGN CT_AgenciaDebito:SCREEN-VALUE     = ""
            CT_ProductoDebito:SCREEN-VALUE    = ""
            CT_CuentaDebito:SCREEN-VALUE      = ""
            CT_NombreDebito:SCREEN-VALUE      = ""
            CT_FechaDebito:SCREEN-VALUE       = "".
     DISABLE Imp_Cdat WITH FRAME f_ahorros.
  END.
  
  IF AVAILABLE Clientes AND Clientes.Cod_Empresa NE 0 THEN DO:
     FIND Empresas WHERE Empresas.Cod_Empresa EQ Clientes.Cod_Empresa NO-LOCK NO-ERROR.
     IF AVAILABLE Empresas THEN
        Wk_PerPagEmp = Empresas.FOR_Pago.
  END.                      
        
  IF Wk_PerPagEmp NE 0 THEN 
     CT_PeriodoPago:SCREEN-VALUE = STRING(Wk_PerPagemp).
  IF SELF:SCREEN-VALUE NE "2" THEN
     CT_PeriodoPago:SCREEN-VALUE = "4".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CT_Monto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CT_Monto wWin
ON LEAVE OF CT_Monto IN FRAME F_Contractual /* Monto de Apertura */
DO:
DO WITH FRAME F_Contractual:
  IF AVAILABLE Pro_Ahorros THEN DO:
     IF Pro_Ahorros.Id_MonAper EQ YES AND
        Pro_Ahorros.Val_MonAper GT DEC(SELF:SCREEN-VALUE) THEN DO:
           MESSAGE "El valor minimo para el monto de apertura" SKIP
              "es de: $ " Pro_Ahorros.Val_MonAper VIEW-AS ALERT-BOX INFORMATION.
           SELF:SCREEN-VALUE = STRING(Pro_Ahorros.Val_MonAPer).
     END.
     ASSIGN Ct_Cuota:SCREEN-VALUE = SELF:SCREEN-VALUE.
  END.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CT_PeriodoPago
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CT_PeriodoPago wWin
ON LEAVE OF CT_PeriodoPago IN FRAME F_Contractual
DO:
  IF AVAILABLE Ahorros AND Ahorros.Per_Deduccion NE INTEGER(SELF:SCREEN-VALUE) THEN DO:
     CASE SELF:SCREEN-VALUE:
       WHEN "1" THEN Wk_DatAnt = "SEMANAL".
       WHEN "2" THEN Wk_DatAnt = "DECADAL".
       WHEN "3" THEN Wk_DatAnt = "QUINCENAL".
       WHEN "4" THEN Wk_DatAnt = "MENSUAL".
     END CASE.
     Wk_Cambio2 = " - Nvo.Per.Deducción: " + Wk_DatAnt.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CT_PeriodoPago wWin
ON VALUE-CHANGED OF CT_PeriodoPago IN FRAME F_Contractual
DO:
  Wk_Cambio2 = "".
  ENABLE Btn_Salvar WITH FRAME F_Ahorros.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CT_Plazo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CT_Plazo wWin
ON LEAVE OF CT_Plazo IN FRAME F_Contractual /* Plazo en Días */
DO:
 DO WITH FRAME F_Contractual:
  /* IF Pro_Ahorros.Tip_Ahorro EQ 2 AND INTEG(CT_Plazo:SCREEN-VALUE) GT 730 THEN DO:
       MESSAGE "El Plazo-Contractual superior a 730 Dias... Rectifique!" VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO CT_Monto.            
       RETURN NO-APPLY.    
  END. */

 DEFINE VAR plazoU AS DEC INITIAL 0.

    ASSIGN plazoU = DEC (ct_plazo:SCREEN-VALUE).  
      IF Tpdt EQ 2  AND plazoU < 180  AND plazoU > 360  THEN DO:
    MESSAGE "El plazo debe ser minimo 6 meses y multiplo de 30"  string(plazoU) VIEW-AS ALERT-BOX INFO BUTTON OK.
    RETURN NO-APPLY.
    END.
    ELSE
        IF  Tpdt EQ 2  AND   plazoU MODULO 30 <> 0 THEN DO:
            MESSAGE "El plazo debe ser  multiplo de 30" VIEW-AS ALERT-BOX INFO BUTTON OK.
            RETURN NO-APPLY.
        END.

  IF Pro_Ahorros.Id_Tasa EQ 1 THEN DO:
     IF DEC(CT_Monto:SCREEN-VALUE) EQ 0 OR DEC(CT_Plazo:SCREEN-VALUE) EQ 0 THEN DO:
        MESSAGE "El Monto y el Plazo deben ser mayores a cero. Rectifique-2" VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO CT_Monto.
        RETURN NO-APPLY.
     END.

     RUN Hallar_Tasa (INPUT Pro_Ahorros.Indicador, INPUT DEC(CT_Monto:SCREEN-VALUE),
                      INPUT DEC(CT_Plazo:SCREEN-VALUE), OUTPUT W_Tasa, OUTPUT W_Puntos).
     IF W_Puntos NE 0 THEN DO:
        CT_Puntos:SCREEN-VALUE = STRING(W_Puntos).
        ENABLE CT_Puntos.
        CT_Puntos:BGCOLOR = 15.
        CT_Puntos:SCREEN-VALUE = STRING(W_Puntos,"->9.99").
     END.
     ELSE DO:
        DISABLE CT_Puntos.
        CT_Puntos:SCREEN-VALUE = "".
     END.
             
     ASSIGN Ahorros.Tasa:SCREEN-VALUE IN FRAME F_Ahorros = STRING(W_Tasa)
            Tasa_Efectiva:LABEL                          = "Tasa Nominal Anual"
            Tasa_Efectiva:SCREEN-VALUE  = STRING(W_Tasa).
  END.
 END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ahorros
&Scoped-define SELF-NAME Ahorros.Cue_Ahorros
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Ahorros.Cue_Ahorros wWin
ON LEAVE OF Ahorros.Cue_Ahorros IN FRAME F_Ahorros /* Cuenta */
DO:
    FIND Ahorros WHERE Ahorros.Agencia      EQ W_Agencia 
                   AND Ahorros.Cod_Ahorro EQ INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Ahorros,1,3))
                   AND Ahorros.Cue_Ahorros  EQ Ahorros.Cue_Ahorros:SCREEN-VALUE
                 NO-LOCK NO-ERROR.
    IF AVAILABLE Ahorros THEN DO:
       RUN MostrarMensaje IN W_Manija(INPUT 322,OUTPUT W_Rpta).
       APPLY "ENTRY":U TO Ahorros.Cue_Ahorros.
       RETURN NO-APPLY.
    END.
    RUN Digito_Chequeo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Ahorros.Estado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Ahorros.Estado wWin
ON VALUE-CHANGED OF Ahorros.Estado IN FRAME F_Ahorros /* Estado */
DO:
    btn_salvar:SENSITIVE = TRUE.

    IF ahorros.estado:SCREEN-VALUE = "1" THEN
        cmb_estados:SCREEN-VALUE = cmb_Estados:ENTRY(2).
    ELSE
        cmb_estados:SCREEN-VALUE = cmb_estados:ENTRY(9).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Ahorros.Exento_3xm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Ahorros.Exento_3xm wWin
ON VALUE-CHANGED OF Ahorros.Exento_3xm IN FRAME F_Ahorros /* Cuenta Subsidiada x el Gobierno */
DO:
  
  IF Ahorros.Exento_3xm:SCREEN-VALUE = "Si" THEN
     IF Ahorros.Fec_desActivacion[1]:SCREEN-VALUE = STRING(TODAY + 1) THEN DO:
         MESSAGE "No se puede Activar el mismo dia de desactivacion!" VIEW-AS 
             ALERT-BOX INFORMATION.
         RETURN NO-APPLY.
     END.
     ELSE
     Ahorros.Fec_Activacion   [1]:SCREEN-VALUE = STRING(TODAY + 1).
  
  IF Ahorros.Exento_3xm:SCREEN-VALUE = "No" THEN
     Ahorros.Fec_DesActivacion[1]:SCREEN-VALUE = STRING(TODAY + 1).  
  /* DISABLE Ahorros.Exento_3xm WITH FRAME F_Ahorros. */
    ENABLE Btn_salvar WITH FRAME f_ahorros.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Atermino
&Scoped-define SELF-NAME F-FecVenc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-FecVenc wWin
ON LEAVE OF F-FecVenc IN FRAME F_Atermino /* Fec. Vencim. */
DO:
  
/*     DEFINE VARIABLE fecFin AS DATE  INITIAL 05/05/2011 NO-UNDO. */
/*     DEFINE VARIABLE viDias AS INTEGER     NO-UNDO.              */

    ASSIGN AT_Plazo:SCREEN-VALUE IN FRAME F_Atermino = STRING(INTERVAL(DATE(SELF:SCREEN-VALUE),W_fecha,"days")).

/*     MESSAGE "hoy " TODAY        SKIP             */
/*         "fec. Final " FecFin    SKIP             */
/*         "NumDias "   viDias SKIP(2)              */
/*         "new " ADD-INTERVAL(TODAY,viDias,"days") */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK.       */


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ahorros
&Scoped-define SELF-NAME Ahorros.Id_Mesada_Pensional
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Ahorros.Id_Mesada_Pensional wWin
ON LEAVE OF Ahorros.Id_Mesada_Pensional IN FRAME F_Ahorros /* Mesada_Pensional */
DO:
  IF Ahorros.Id_Mesada_Pensional:SCREEN-VALUE = "Si" THEN
     Ahorros.Fec_Activacion   [4]:SCREEN-VALUE = STRING(TODAY + 1).
  IF Ahorros.Id_Mesada_Pensional:SCREEN-VALUE = "No" THEN
     Ahorros.Fec_DesActivacion[4]:SCREEN-VALUE = STRING(TODAY + 1).
  DISABLE Ahorros.Id_Mesada_Pensional WITH FRAME F_Ahorros.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Ahorros.Id_Subsidio_Entidad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Ahorros.Id_Subsidio_Entidad wWin
ON VALUE-CHANGED OF Ahorros.Id_Subsidio_Entidad IN FRAME F_Ahorros /* Cuenta Subsidiada x la Entidad? */
DO:
  IF Ahorros.Id_Subsidio_Entidad:SCREEN-VALUE  = "Si" THEN
     Ahorros.Fec_Activacion   [2]:SCREEN-VALUE = STRING(TODAY + 1).
  IF Ahorros.Id_Subsidio_Entidad:SCREEN-VALUE  = "No" THEN
     Ahorros.Fec_DesActivacion[2]:SCREEN-VALUE = STRING(TODAY + 1).
  DISABLE Ahorros.Id_Subsidio_Entidad WITH FRAME F_Ahorros.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Ahorros.Id_Tesoro_Nacional
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Ahorros.Id_Tesoro_Nacional wWin
ON VALUE-CHANGED OF Ahorros.Id_Tesoro_Nacional IN FRAME F_Ahorros /* Tesoro Nacional */
DO:
  IF Ahorros.Id_Tesoro_Nacional:SCREEN-VALUE   = "Si" THEN
     Ahorros.Fec_Activacion   [3]:SCREEN-VALUE = STRING(TODAY + 1).
  IF Ahorros.Id_Tesoro_Nacional:SCREEN-VALUE   = "No" THEN
     Ahorros.Fec_DesActivacion[3]:SCREEN-VALUE = STRING(TODAY + 1).
  DISABLE Ahorros.Id_Tesoro_Nacional WITH FRAME F_Ahorros.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Ahorros.Id_Tutor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Ahorros.Id_Tutor wWin
ON VALUE-CHANGED OF Ahorros.Id_Tutor IN FRAME F_Ahorros /* R. Legal */
DO:
 /* ENABLE Ahorros.Nit_Tutor WITH FRAME F_Ahorros.*/
  /*IF Ahorros.Id_Tutor:SCREEN-VALUE = "Padre" OR Ahorros.Id_Tutor:SCREEN-VALUE = "Madre" THEN DO:*/
     DISABLE Ahorros.Nit_Tutor WITH FRAME F_Ahorros.
     FIND FIRST Relaciones WHERE Relaciones.Nit = string(Ahorros.Nit:SCREEN-VALUE IN FRAME f_ahorros) AND Relaciones.cod_Relacion = 3 AND
           Relaciones.Estado = 1 NO-LOCK NO-ERROR.
     IF NOT AVAIL Relaciones THEN DO:
        MESSAGE "No se Encontro la Relacion PADRE/MADRE Para el Titular de la Cuenta" VIEW-AS ALERT-BOX.
        ENABLE Btn_Cancelar.
        RETURN NO-APPLY.
     END.
     FIND rela_Clientes WHERE rela_Clientes.Nit = Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
     IF NOT AVAIL(rela_Clientes) THEN DO:
        MESSAGE "No se ha Encontrado PADRE/MADRE del Titular en la Tabla CLIENTES" VIEW-AS ALERT-BOX.
        Nombre_Tutor:SCREEN-VALUE = " ".
        ENABLE Btn_Cancelar.
        RETURN NO-APPLY.
     END.
     ASSIGN Ahorros.Nit_Tutor:SCREEN-VALUE = Relaciones.Nit_Relacion
            Nombre_Tutor:SCREEN-VALUE = Rela_Clientes.nombre + " " + Rela_Clientes.Apellido1 + " " + Rela_Clientes.Apellido2.
      
          ASSIGN Ahorros.Id_Tutor:SCREEN-VALUE = Relaciones.descripcion.
 /*     
  END.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Imp_Cdat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Imp_Cdat wWin
ON CHOOSE OF Imp_Cdat IN FRAME F_Ahorros /* Impresion CDAT */
DO:
    DEFINE VAR hPdf AS COM-HANDLE NO-UNDO.
    DEFINE VAR l-file AS CHARACTER.

    /*FIND CURRENT ahorros NO-LOCK.*/

    IF ahorros.estado = 1 AND (ahorros.fec_vencimiento <> ? OR ahorros.fec_vencimiento >= w_fecha) THEN DO:
        RUN Rp_CDAT.r(INPUT ahorros.nit,
                      INPUT ahorros.cue_ahorros).

        l-file = "Reportes\CDAT\" + ahorros.nit + "_" + ahorros.cue_ahorros + ".pdf".
    
        ASSIGN FRAME F_Ahorros:SENSITIVE = FALSE.
    
        RUN visorPDF(INPUT l-file) NO-ERROR.

        ASSIGN FRAME F_Ahorros:SENSITIVE = TRUE.
    END.
    ELSE
        MESSAGE "El CDAT no se encuentra activo." SKIP
                "No se permite su impresión."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

    /*FIND CURRENT ahorros.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Imp_UtraMas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Imp_UtraMas wWin
ON CHOOSE OF Imp_UtraMas IN FRAME F_Ahorros /* Impresion Titulo */
DO: 
  IF NOT AVAIL Ahorros THEN
     MESSAGE "Registro No Existe. Salvelo Para Poder Imprimirlo" VIEW-AS ALERT-BOX.
  IF AVAIL Ahorros AND (Ahorros.Cod_Ahorro = 17 OR Ahorros.Cod_Ahorro = 18)  AND  ahorros.Cod_ahorro NE 38 THEN DO:
          IF  ahorros.fec_Vencimiento = ? THEN DO:
        MESSAGE "Primero debe consignar el asociado antes de imprimir el formulario"
            VIEW-AS ALERT-BOX INFO BUTTON OK.
        RETURN NO-APPLY.
    END.
    ELSE DO:
     InputFile = "Formatos\AP - 300.xls".
     RUN Abrir_Excel.
     RUN Imprime_Formato_UtraMas.
    /* RUN Cerrar_Excel. */
    END.
  END.
ELSE
    IF AVAIL Ahorros AND (Ahorros.tip_Ahorro = 2 OR Ahorros.Cod_Ahorro = 38) THEN DO:
          IF  ahorros.fec_Vencimiento = ? THEN DO:
        MESSAGE "Primero debe consignar el asociado antes de imprimir el formulario"
            VIEW-AS ALERT-BOX INFO BUTTON OK.
        RETURN NO-APPLY.
    END.
    ELSE DO:    
      InputFile = "Formatos\AP - 302.xls".
     RUN Abrir_Excel.
     RUN Imprime_Utravivienda.
    /*  END.
      ELSE DO:
              MESSAGE "Primero debes grabar el cambio de forma de pago"
            VIEW-AS ALERT-BOX INFO BUTTON OK.
        RETURN NO-APPLY.
      END.*/

        
     END.
    /* */
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Ahorros.Nit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Ahorros.Nit wWin
ON LEAVE OF Ahorros.Nit IN FRAME F_Ahorros /* Nit */
DO:
    FIND FIRST Clientes WHERE Clientes.Nit = Ahorros.Nit:SCREEN-VALUE IN FRAME F_Ahorros NO-LOCK NO-ERROR.
    IF AVAILABLE(Clientes) THEN
        W_NomTitular:SCREEN-VALUE = CAPS(Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2).
    ELSE DO:
        RUN C-Clientes.r(INPUT 2,
                         INPUT W_Agencia,
                         OUTPUT P_Nit,
                         OUTPUT P_Nombre,
                         OUTPUT P_Apellido,
                         OUTPUT P_AgeCli).

        ASSIGN Ahorros.Nit:SCREEN-VALUE = P_Nit
               W_Nomtitular:SCREEN-VALUE = CAPS(TRIM(P_Nombre) + " " + P_Apellido).

        FIND FIRST Clientes WHERE Clientes.Nit = P_Nit NO-LOCK NO-ERROR.
    END.

    FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Tip_Ahorro = INTEGER(SUBSTRING(Cmb_TipoProductos:SCREEN-VALUE,1,1))
                             AND Pro_Ahorros.Cod_Ahorro = INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Ahorros,1,3)) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Pro_Ahorros THEN DO:
        IF SUBSTRING(Cmb_TipoProductos:SCREEN-VALUE IN FRAME F_Ahorros,1,1) = ? THEN DO:
            APPLY "CHOOSE" TO Btn_Cancelar.
            RETURN NO-APPLY.
        END.

        MESSAGE "El Producto seleccionado no ha sido" SKIP
                "encontado en el Sistema."
            VIEW-AS ALERT-BOX.

        Cmb_Productos:SCREEN-VALUE = Cmb_Productos:ENTRY(1).
        APPLY "entry" TO Cmb_Productos.
        RETURN NO-APPLY.
    END.

    IF INTEGER(SUBSTRING(Cmb_TipoProductos:SCREEN-VALUE,1,1)) = 4 THEN DO:
        FIND FIRST BAhorros WHERE BAhorros.nit = Ahorros.nit:SCREEN-VALUE
                              AND BAhorros.tip_Ahorro = 4
                              AND BAhorros.estado = 1 NO-LOCK NO-ERROR.
        IF AVAILABLE BAhorros THEN DO:
            MESSAGE "El Asociado ya posee activo un producto de Aportes." SKIP
                    "No se permite la operación."
                VIEW-AS ALERT-BOX.

            Cmb_Productos:SCREEN-VALUE = Cmb_Productos:ENTRY(1).
            RUN inicializar_variables.
            btn_salvar:SENSITIVE = FALSE.
            APPLY "CHOOSE" TO Btn_Cancelar.
            RETURN NO-APPLY.
        END.
    END.

    IF INTEGER(SUBSTRING(Cmb_TipoProductos:SCREEN-VALUE,1,1)) = 2 AND INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE,1,3)) = 3 THEN DO:
        FIND FIRST BAhorros WHERE BAhorros.nit = Ahorros.nit:SCREEN-VALUE
                              AND BAhorros.tip_Ahorro = 2
                              AND BAhorros.cod_ahorro = 3
                              AND BAhorros.estado = 1 NO-LOCK NO-ERROR.
        IF AVAILABLE BAhorros THEN DO:
            MESSAGE "El Asociado ya posee activo un producto de Ahorro Permanente." SKIP
                    "No se permite la operación."
                VIEW-AS ALERT-BOX.

            Cmb_Productos:SCREEN-VALUE = Cmb_Productos:ENTRY(1).
            RUN inicializar_variables.
            btn_salvar:SENSITIVE = FALSE.
            APPLY "CHOOSE" TO Btn_Cancelar.
            RETURN NO-APPLY.
        END.
    END.

    ASSIGN vlTpAct = IF Clientes.Tipo_Actividad BEGINS "Independiente" THEN TRUE ELSE FALSE.

    FIND FIRST ListaNegra WHERE ListaNegra.Nit = Clientes.Nit AND listaNegra.estado = 1 NO-LOCK NO-ERROR.
    IF AVAILABLE ListaNegra THEN DO:
        MESSAGE "Este Número de Identificación ha sido encontrado" SKIP
                "en las listas de Control de la cooperativa." SKIP
                "con el nombre : " ListaNegra.Nombre " " ListaNegra.Apellido1 " " ListaNegra.Apellido2 SKIP(1)
                "Se cancela la operación de afiliación!" SKIP
                "Reporte esta situación al departamento" SKIP
                "de control interno, Por estar EXCLUIDO" VIEW-AS ALERT-BOX WARNING TITLE "Activo en Lista Negra".

        APPLY 'CHOOSE' TO Btn_Cancelar.
        RETURN NO-APPLY.
    END.

    IF Clientes.Estado = 2 AND Clientes.Fec_Retiro <> ? THEN DO:
        MESSAGE "No se pueden crear cuentas de Ahorro para Asociados retirados"
            VIEW-AS ALERT-BOX WARNING.

        APPLY "CHOOSE" TO Btn_Cancelar.
        RETURN NO-APPLY.
    END.

    WEdad = (TODAY - Clientes.Fec_Nacimiento).

    IF Pro_Ahorros.Id_Persona = 1 AND WEdad < 6570 THEN DO:
        MESSAGE "Producto disponible solo para mayores de edad." SKIP(1)
                "Fecha de Nacimiento:" Clientes.Fec_Nacimiento SKIP(1)
                "Edad:" WEdad
            VIEW-AS ALERT-BOX.

        ASSIGN Ahorros.Nit:SCREEN-VALUE = ""
               W_NomTitular:SCREEN-VALUE = "".

        Cmb_Productos:SCREEN-VALUE = Cmb_Productos:ENTRY(1).
        APPLY "entry" TO Cmb_Productos.
        RETURN NO-APPLY.
    END.

    IF AVAILABLE Pro_ahorros AND Pro_Ahorros.Id_Persona = 2 THEN DO:
        APPLY "value-changed" TO ahorros.ID_tutor.
        DISABLE ahorros.id_tutor.
    END.

    IF Pro_Ahorros.Id_Persona = 2 AND WEdad >= 6570 THEN DO:
        MESSAGE "Producto disponible solo para menores de edad." SKIP(1)
                "Fecha de Nacimiento:" Clientes.Fec_Nacimiento SKIP(1)
                "Edad:" WEdad
            VIEW-AS ALERT-BOX.

        ASSIGN Ahorros.Nit:SCREEN-VALUE = ""
               W_NomTitular:SCREEN-VALUE = "".

        Cmb_Productos:SCREEN-VALUE = Cmb_Productos:ENTRY(1).
        APPLY "entry" TO Cmb_Productos.
        RETURN NO-APPLY.
    END.

    IF INTEGER(SUBSTRING(Cmb_TipoProductos:SCREEN-VALUE,1,1)) = 4 THEN DO:
        R_Eleccion:SCREEN-VALUE IN FRAME F_Relaciones = "4".

        APPLY 'value-changed' TO R_Eleccion IN FRAME F_Relaciones.

        IF NuevaCta THEN RUN Apertura.
    END.

    RELEASE Ahorros. 

    DISABLE ahorros.num_formato
            ahorros.sdo_mincta
       WITH FRAME f_ahorros.

    IF INTEGER(SUBSTRING(Cmb_TipoProductos:SCREEN-VALUE,1,1)) = 3 THEN DO:
        ASSIGN Tasa_Efectiva:LABEL = "Efectiva Anual:".
       APPLY "entry" TO AT_MONTO IN FRAME F_ATERMINO.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Relaciones
&Scoped-define SELF-NAME Nit_Relacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Nit_Relacion wWin
ON LEAVE OF Nit_Relacion IN FRAME F_Relaciones
DO:  


    DO WITH FRAME F_Relaciones:
   IF SELF:SCREEN-VALUE EQ Ahorros.Nit THEN DO:
      MESSAGE "No puede ser Autorizado o Beneficiario" SKIP
              "ya que es el titular de la cuenta. Rectifique!" VIEW-AS ALERT-BOX INFORMATION.
      APPLY "choose" TO Btn_CanRel.
   END.
   
   FIND Clientes WHERE Clientes.Nit EQ Nit_Relacion:SCREEN-VALUE NO-LOCK NO-ERROR.
   IF NOT AVAILABLE(Clientes) THEN DO:
      RUN C-Clientes.R(INPUT 1, INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_AgeCli).
      ASSIGN W_NomNitRelacion:SCREEN-VALUE = P_Nombre + " " + P_Apellido
             Nit_Relacion:SCREEN-VALUE = P_Nit.
   END.
   ELSE
      ASSIGN W_NomNitRelacion:SCREEN-VALUE = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.   
      
   FIND Relaciones WHERE 
        Relaciones.Nit EQ Ahorros.Nit:SCREEN-VALUE IN FRAME F_Ahorros AND
        Relaciones.Cod_Relacion EQ INTEGER(R_Eleccion:SCREEN-VALUE) AND
        Relaciones.Nit_Relacion EQ Nit_Relacion:SCREEN-VALUE AND 
        Relaciones.Cod_Producto EQ Ahorros.Cod_Ahorro AND 
        Relaciones.Cuenta EQ Ahorros.Cue_Ahorros NO-LOCK NO-ERROR.
   IF AVAILABLE(Relaciones) THEN DO:
      MESSAGE "Esta Relacion ya existe!" SKIP
              "Se Cancela la Operacion de Creado" VIEW-AS ALERT-BOX WARNING.
      DISABLE Nit_Relacion Btn_SalRel R_Relacion2.
      HIDE Nit_Relacion Lb_Nit .
      ASSIGN Nit_Relacion:SCREEN-VALUE = ""
             Nit_Relacion:FGCOLOR      = 17
             Nit_Relacion:BGCOLOR      = 17
             W_NomNitRelacion:BGCOLOR  = 17
             R_Relacion2:BGCOL            = 17
             W_NomNitRelacion:SCREEN-VALUE = "".
   END.
   ELSE
       APPLY 'entry' TO Btn_SalRel.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ahorros
&Scoped-define SELF-NAME Ahorros.Nit_Tutor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Ahorros.Nit_Tutor wWin
ON LEAVE OF Ahorros.Nit_Tutor IN FRAME F_Ahorros /* Nit.Legal */
DO:
  FIND FIRST Relaciones WHERE Relaciones.Nit = Clientes.Nit AND Relaciones.Nit_Relacion =
       Ahorros.Nit_Tutor:SCREEN-VALUE AND Relaciones.cod_Relacion = 3 AND Relaciones.Estado = 1 NO-LOCK NO-ERROR.
  IF NOT AVAIL Relaciones THEN DO:
     MESSAGE "No se Encontro la Relacion TUTOR Para el Titular de la Cuenta" VIEW-AS ALERT-BOX.
     DISABLE Ahorros.Nit_Tutor WITH FRAME F_Ahorros.
     RETURN NO-APPLY.
  END.
  FIND Rela_Clientes WHERE Rela_Clientes.Nit = Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
  IF NOT AVAIL Rela_Clientes THEN DO:
     MESSAGE "No se ha Encontrado TUTOR del Titular en la Tabla CLIENTES" VIEW-AS ALERT-BOX.
     DISABLE Ahorros.Nit_Tutor WITH FRAME F_Ahorros.
     RETURN NO-APPLY.
  END.
  Nombre_Tutor:SCREEN-VALUE = Rela_Clientes.nombre + " " + Rela_Clientes.Apellido1 + " " + Rela_Clientes.Apellido2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Libretas
&Scoped-define SELF-NAME Lib_Chequera.Num_Inicial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Lib_Chequera.Num_Inicial wWin
ON LEAVE OF Lib_Chequera.Num_Inicial IN FRAME F_Libretas /* Numero Inicial */
DO:
  Lib_Chequera.Num_Final:SCREEN-VALUE = STRING(INTEG(Lib_Chequera.Num_Inicial:SCREEN-VALUE) + Pro_Ahorros.Nro_CheqACobrar).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Contractual
&Scoped-define SELF-NAME Ahorros.porce_Aporte
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Ahorros.porce_Aporte wWin
ON ENTRY OF Ahorros.porce_Aporte IN FRAME F_Contractual /* % Cuota Ahorro Permanen */
DO:
    ASSIGN vdeValIni = DEC(SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Ahorros.porce_Aporte wWin
ON LEAVE OF Ahorros.porce_Aporte IN FRAME F_Contractual /* % Cuota Ahorro Permanen */
DO:
    DEFINE VARIABLE vdeApoObli AS DEC     NO-UNDO. /* Porcentaje para aporte obligatorio */
    DEFINE VARIABLE viCuota AS INTEGER     NO-UNDO.

    IF vdeValIni NE DEC(SELF:SCREEN-VALUE) THEN DO:
        ENABLE Btn_Salvar WITH FRAME F_Ahorros.
        FIND FIRST BAhorros WHERE BAhorros.nit EQ Ahorros.nit:SCREEN-VALUE IN FRAME F_Ahorros AND
                BAhorros.tip_Ahorro EQ 4 AND BAhorros.cod_Ahorro EQ 5 NO-LOCK NO-ERROR.
        IF AVAILABLE BAhorros THEN DO:
            ASSIGN vdeApoObli = DEC(F-PorApoSue:SCREEN-VALUE) - DEC(SELF:SCREEN-VALUE).
            CASE vlTpAct: /* Validacion de aporte obligatorio, para garantizar el minimo de aporte*/
                WHEN TRUE THEN DO: /* si asociado es independiente */
                    IF DEC(F-PorApoSue:SCREEN-VALUE) < 2 THEN DO:
                        MESSAGE "% de Aporte respecto al sueldo es menor" SKIP
                            "al 2%. No se puede crear Producto." SKIP
                            "Se debe pasar la novedad." SKIP
                            "Asociado es Independiente."
                            VIEW-AS ALERT-BOX INFO BUTTONS OK.
                        APPLY "CHOOSE":U TO Btn_Cancelar IN FRAME F_Ahorros.
                        RETURN NO-APPLY.
                    END. /* IF DEC(F-PorApoSue:SCREEN-VALUE) < 1 */
                    ELSE DO:
                        IF vdeApoObli < 2 THEN DO:
                            MESSAGE "Porcentaje cuota para aporte obligatorio" SKIP 
                                "no puede ser menor al 2%" SKIP
                                "El máximo a usar Con este asocido es de " STRING(ABS(2 - DEC(F-PorApoSue:SCREEN-VALUE)),">9.99%")
                                VIEW-AS ALERT-BOX INFO BUTTONS OK.
                            APPLY "ENTRY":U TO SELF.
                            RETURN NO-APPLY.
                        END. /* IF vdeApoObli < 1 */
                        ELSE DO:
                            ASSIGN viCuota = (DEC(SELF:SCREEN-VALUE) / DEC(F-PorApoSue:SCREEN-VALUE)) * INTEGER(F-AporOblig:SCREEN-VALUE)
                                CT_Cuota:SCREEN-VALUE = STRING(viCuota)
                                CT_Plazo:SCREEN-VALUE = "9999"
                                CT_Monto:SCREEN-VALUE = STRING(viCuota).
                            APPLY "LEAVE":U to CT_Plazo.
                        END.
                    END. /* ELSE DO: */            
                END. /* WHEN TRUE THEN DO: */
                WHEN FALSE THEN DO: /* si asociado es empleado */
                    IF DEC(F-PorApoSue:SCREEN-VALUE) < 1 THEN DO:
                        MESSAGE "% de Aporte respecto al sueldo es menor" SKIP
                            "al 1%. No se puede crear Producto." SKIP
                            "Se debe pasar la novedad."
                            VIEW-AS ALERT-BOX INFO BUTTONS OK.
                        APPLY "CHOOSE":U TO Btn_Cancelar IN FRAME F_Ahorros.
                        RETURN NO-APPLY.
                    END. /* IF DEC(F-PorApoSue:SCREEN-VALUE) < 1 */
                    ELSE DO:
                        IF vdeApoObli < 1 THEN DO:
                            MESSAGE "Porcentaje cuota para aporte obligatorio" SKIP 
                                "no puede ser menor al 1%" SKIP
                                "El máximo a usar Con este asocido es de " STRING(ABS(1 - DEC(F-PorApoSue:SCREEN-VALUE)),">9.99%")
                                VIEW-AS ALERT-BOX INFO BUTTONS OK.
                            APPLY "ENTRY":U TO SELF.
                            RETURN NO-APPLY.
                        END. /* IF vdeApoObli < 1 */
                        ELSE DO:
                            ASSIGN viCuota = (DEC(SELF:SCREEN-VALUE) / DEC(F-PorApoSue:SCREEN-VALUE)) * INTEGER(F-AporOblig:SCREEN-VALUE)
                                CT_Cuota:SCREEN-VALUE = STRING(viCuota)
                                CT_Plazo:SCREEN-VALUE = "9999"
                                CT_Monto:SCREEN-VALUE = STRING(viCuota).
                            APPLY "LEAVE":U to CT_Plazo.
                        END.
                    END. /* ELSE DO: */            
                END. /* WHEN FALSE THEN DO: */
            END CASE.
        END. /* IF AVAILABLE BAhorros */
        ELSE DO:
            MESSAGE "Cliente no tiene Aporte Obligatorio." SKIP
                "No se creará Ahorro Permanente."
                VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "Sin Aporte Obligatorio".
            APPLY "CHOOSE":U TO Btn_Cancelar.
            RETURN NO-APPLY.
        END.
    END. /* IF vdeValIni NE DEC(SELF:SCREEN-VALUE) */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Relaciones
&Scoped-define SELF-NAME RActivas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RActivas wWin
ON VALUE-CHANGED OF RActivas IN FRAME F_Relaciones
DO:
  DO WITH FRAME F_Relaciones:
  IF RActivas:SCREEN-VALUE EQ "1" THEN
    Btn_Activas:LABEL = "Inactivar".
  ELSE
    Btn_Activas:LABEL = "Activar".
  END.
  APPLY 'value-changed' TO R_Eleccion.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME R_Eleccion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_Eleccion wWin
ON MOUSE-SELECT-CLICK OF R_Eleccion IN FRAME F_Relaciones
DO:
 /* APPLY "Value-Changed" TO SELF. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_Eleccion wWin
ON VALUE-CHANGED OF R_Eleccion IN FRAME F_Relaciones
DO:
    /*
DO WITH FRAME F_Relaciones:
  FOR EACH T_Relaciones: DELETE T_Relaciones. END.
  CLOSE QUERY BR_Relaciones.
  
  IF INTEGER(SELF:SCREEN-VALUE) EQ 4 THEN
     ASSIGN W_VrAutoriz:SCREEN-VALUE = "0"
            W_VrAutoriz
            W_VrAutoriz:VISIBLE      = FALSE.
  ELSE W_VrAutoriz:VISIBLE = TRUE.           
  
  FIND Varios WHERE Varios.Tipo EQ 3 AND Varios.Codigo EQ INTEGER(SELF:SCREEN-VALUE) NO-LOCK NO-ERROR.
  
  FOR EACH Relaciones WHERE 
           Relaciones.Nit EQ Ahorros.Nit:SCREEN-VALUE IN FRAME F_Ahorros AND 
           Relaciones.Cod_Relacion  EQ INTEGER(SELF:SCREEN-VALUE) AND 
           Relaciones.Clase_Producto EQ 1 AND
           Relaciones.Cod_Producto   EQ INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE,1,3)) AND
           Relaciones.Cuenta         EQ Cue_Ahorros:SCREEN-VALUE IN FRAME F_Ahorros AND
           Relaciones.Estado EQ INTEGER(RActivas:SCREEN-VALUE) NO-LOCK:

          FIND Clientes WHERE Clientes.Nit EQ Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
          IF AVAILABLE(Clientes) THEN DO:
             CREATE T_Relaciones.
             UPDATE T_Relaciones.R_Relacion  = Varios.Descripcion
                    T_Relaciones.R_AgeObjeto = Clientes.Agencia
                    T_Relaciones.R_NitObjeto = Relaciones.Nit_Relacion
                    T_Relaciones.R_VrAutoriz = Relaciones.Val_Autoriz
                    T_Relaciones.R_NomObjeto = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2
                    T_Relaciones.R_NomDescri = Relaciones.Descripcion
                    T_Relaciones.R_TelObjeto = Clientes.Tel_Residencia
                    T_Relaciones.R_CodPdt    = Relaciones.Cod_Producto
                    T_Relaciones.Est         = Relaciones.Estado.
          END.
  END.
  
  OPEN QUERY BR_Relaciones FOR EACH T_Relaciones NO-LOCK INDEXED-REPOSITION.  
END.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ahorros
&Scoped-define SELF-NAME T-ExeCobro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-ExeCobro wWin
ON MOUSE-SELECT-CLICK OF T-ExeCobro IN FRAME F_Ahorros /* Cuenta Nómina */
DO:
  FIND Pro_Ahorros WHERE 
       Pro_Ahorros.Tip_Ahorro EQ INTEGER(SUBSTRING(Cmb_TipoProductos:SCREEN-VALUE,1,1)) AND
       Pro_Ahorros.Cod_Ahorro EQ INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Ahorros,1,3))
                              NO-LOCK NO-ERROR.
  IF NOT AVAILABLE(Pro_Ahorros) THEN DO:
     ASSIGN T-Execobro:SCREEN-VALUE = "No".

     MESSAGE "Debe seleccionar 1o.el pdcto de Ahorros." VIEW-AS ALERT-BOX.

     RETURN.
  END.
  ELSE DO:
     ASSIGN T-ExeCobro.
     DEFI VAR W_SiErr AS LOG.
     RUN P-ValiDarTrans IN W_Manija (OUTPUT W_SiErr,OUTPUT W_Autorizo).
     IF NOT W_SiErr THEN 
       IF T-exeCobro THEN
         ASSIGN T-Execobro:SCREEN-VALUE  = "No"  T-Execobro.
       ELSE
         ASSIGN T-Execobro:SCREEN-VALUE   = "Yes" T-Execobro. 
     ELSE
       ASSIGN  T-Execobro.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Ahorros.TarjetaDB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Ahorros.TarjetaDB wWin
ON LEAVE OF Ahorros.TarjetaDB IN FRAME F_Ahorros /* TarjetaDB */
DO:
  WEdad = (TODAY - Clientes.Fec_Nacimiento).
  IF WEdad < 6570 THEN DO:
     MESSAGE "Tarjeta Debito Disponible Solo Para Mayores de Edad" SKIP(1)
             "Fecha de Nacimiento: " Clientes.Fec_Nacimiento SKIP(1)
             "Edad: " WEdad VIEW-AS ALERT-BOX.
      Ahorros.TarjetaDB:SCREEN-VALUE = " ".

     APPLY 'choose' TO btn_Cancelar.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Tasa_Efectiva
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Tasa_Efectiva wWin
ON LEAVE OF Tasa_Efectiva IN FRAME F_Ahorros /* Efectiva Anual */
DO:
   CASE INTEGER(SUBSTRING(Cmb_TipoProductos:SCREEN-VALUE IN FRAME F_Ahorros,1,1)):
   WHEN 2 THEN DO:
     DO WITH FRAME F_Contractual:
       RUN Convertir_Tasa_Periodica (INPUT DEC(Ct_Monto), INPUT DEC(SELF:SCREEN-VALUE)).
     END.
   END.
   WHEN 3 THEN DO:
     DO WITH FRAME F_ATermino:
       RUN Convertir_Tasa_Periodica (INPUT DEC(at_Monto:SCREEN-VALUE IN FRAME f_atermino),
                                      INPUT DEC(SELF:SCREEN-VALUE)).
     END.
   END.
   OTHERWISE
     DO WITH FRAME F_AlaVista:
       RUN Convertir_Tasa_Periodica (INPUT DEC(Av_Monto), INPUT DEC(SELF:SCREEN-VALUE)).
     END.
 END CASE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tgl-TarjetaDB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tgl-TarjetaDB wWin
ON MOUSE-SELECT-CLICK OF tgl-TarjetaDB IN FRAME F_Ahorros
DO:
    ENABLE Btn_Salvar.
  FIND Ahorros WHERE 
       Ahorros.Tip_Ahorro EQ INTEGER(SUBSTRING(Cmb_TipoProductos:SCREEN-VALUE,1,1)) AND
       Ahorros.Cod_Ahorro EQ INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Ahorros,1,3)) AND
        ahorros.cue_ahorros  EQ STRING(ahorros.cue_ahorros:SCREEN-VALUE IN FRAME f_ahorros)
                              NO-LOCK NO-ERROR.
  IF NOT AVAILABLE(Ahorros) THEN DO:
     ASSIGN Tgl-TarjetaDB:SCREEN-VALUE = "No".

     MESSAGE "Debe grabar la cuenta primero!" VIEW-AS ALERT-BOX.

     RETURN NO-APPLY.
  END.
  ELSE
      FIND FIRST lib_chequera WHERE lib_chequera.agencia = ahorros.agencia AND  
          lib_chequera.cod_producto = ahorros.cod_ahorro AND lib_chequera.estado = 1 AND
          lib_chequera.cue_ahorros = ahorros.cue_ahorros  NO-LOCK NO-ERROR.
          IF AVAILABLE(lib_chequera) THEN DO:
            ASSIGN Tgl-TarjetaDB:SCREEN-VALUE = "No".
             MESSAGE "Primero debe cancelar las libretas que tiene activas" VIEW-AS ALERT-BOX INFO BUTTON OK.
             RETURN NO-APPLY.
          END.
          ELSE DO:
    ASSIGN Tgl-TarjetaDB.
    ENABLE wcomenTdb.
    ASSIGN wcomenTdb:VISIBLE = TRUE.
    IF Tgl-TarjetaDB THEN DO:
       ASSIGN Btn_Libretas:VISIBLE = FALSE.
      IF Ahorros.TarjetaDB:SCREEN-VALUE = "" THEN  
         Ahorros.TarjetaDB:SCREEN-VALUE = "TARJ.DB.SOLICIT.".    
    END.
    ELSE DO:
    ASSIGN  btn_libretas:VISIBLE = TRUE.
      ENABLE btn_libretas.
      DISABLE wcomenTdb.
      DEFI VAR W_SiErr   AS LOG.
      IF Ahorros.TarjetaDB:SCREEN-VALUE = "TARJ.DB.SOLICIT." THEN
        Ahorros.TarjetaDB:SCREEN-VALUE = "".
      IF NOT Tgl-TarjetaDB AND Ahorros.tarjetadb NE  "" THEN DO:
        RUN P-ValiDarTrans IN W_Manija (OUTPUT W_SiErr,OUTPUT W_Autorizo).
        IF NOT W_SiErr THEN DO:
              ENABLE wcomenTdb.
          ASSIGN Btn_Libretas:VISIBLE = FALSE
                 tgl-TarjetaDb:SCREEN-VALUE  = "Yes"
                 Ahorros.TarjetaDB:SCREEN-VALUE = Ahorros.TarjetaDB.
          RETURN.
        END.  
        ELSE DO:
          W_CancTarDB = TRUE.    
          Btn_Libretas:VISIBLE = TRUE.
           ENABLE btn_libretas.
        END.
      END.
    END.

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Tg_Tasa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Tg_Tasa wWin
ON MOUSE-SELECT-CLICK OF Tg_Tasa IN FRAME F_Ahorros /* Modif.Tasa CDAT */
DO:
  DEFI VAR W_SiErr AS LOG.

  ASSIGN Tg_Tasa.

  FIND FIRST Usuarios WHERE Usuarios.Usuario EQ W_Usuario NO-LOCK NO-ERROR.

  IF  AVAIL(Ahorros) AND Ahorros.Tip_Ahorro EQ 3 AND Tg_Tasa AND (Usuarios.Prioridad = 6 ) THEN DO:
     
   /*  RUN P-ValiDarTrans IN W_Manija (OUTPUT W_SiErr,OUTPUT W_Autorizo).
     IF NOT W_SiErr THEN DO:
        ASSIGN Tg_Tasa                = FALSE
               Tg_Tasa:SCREEN-VALUE   = "No"
               Ahorros.Tasa:SENSITIVE = FALSE.
        RETURN.
      END. */
     
     
     ASSIGN Tg_Tasa                = FALSE
            Tg_Tasa:SCREEN-VALUE   = "No"
            Ahorros.Tasa:SENSITIVE = TRUE
            Btn_Salvar:SENSITIVE   = TRUE.     
  END.
  ELSE
     ASSIGN Tg_Tasa                = FALSE
            Tg_Tasa:SCREEN-VALUE   = "No"
            Ahorros.Tasa:SENSITIVE = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Tg_Tasa wWin
ON VALUE-CHANGED OF Tg_Tasa IN FRAME F_Ahorros /* Modif.Tasa CDAT */
DO:
  APPLY "mouse-select-click" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Libretas
&Scoped-define SELF-NAME Lib_Chequera.Tip_Talonario
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Lib_Chequera.Tip_Talonario wWin
ON MOUSE-SELECT-CLICK OF Lib_Chequera.Tip_Talonario IN FRAME F_Libretas /* Tipo de Talonario */
DO:
  APPLY "ENTRY" TO Lib_Chequera.Num_Inicial.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Relaciones
&Scoped-define SELF-NAME W_VrAutoriz
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_VrAutoriz wWin
ON LEAVE OF W_VrAutoriz IN FRAME F_Relaciones
DO:
  ASSIGN W_VrAutoriz.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ahorros
&Scoped-define BROWSE-NAME BR_Libretas
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Abrir_Excel wWin 
PROCEDURE Abrir_Excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    SwExiste = SEARCH(InputFile).
   /* MESSAGE "ola" SKIP SwExiste SKIP inputFile VIEW-AS ALERT-BOX INFORMATION.*/
    IF SwExiste EQ ? THEN DO:
       MESSAGE InputFile "no Encontrado" VIEW-AS ALERT-BOX.
       RETURN.
    END.

    FIND Usuarios WHERE Usuarios.Usuario = W_Usuario       NO-LOCK NO-ERROR.
    FIND Clientes WHERE Clientes.Nit     = Ahorros.Nit     NO-LOCK NO-ERROR.
    FIND Agencias WHERE Agencias.Agencia = Ahorros.Agencia NO-LOCK NO-ERROR.
    CREATE "Excel.Application" chExcelApp.


    hWorkBooks = chExcelApp:WorkBooks:OPEN(SwExiste,,TRUE,). 
    IF  hWorkBooks THEN DO:
        chExcelApp:Visible = TRUE.
    chWorkSheet = chExcelApp:Sheets:Item(1).
    END.
    ELSE
        SwExiste = ?.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Apertura wWin 
PROCEDURE Apertura PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  CASE Clientes.Tipo_Identificacion:
       WHEN "C.C" OR WHEN "C.E" THEN                                                                            
          ASSIGN Av_Monto = ROUND((W_SMLV / 2) * 0.10,0)
                 Av_Monto = ROUND(Av_Monto + 25,0)
                 Av_Monto:SCREEN-VALUE IN FRAME F_AlaVista = STRING(Av_Monto)
                 Av_Cuota = INTEGER((W_SMLV / 2) - ((W_SMLV / 2) * 0.10)) / (12 - MONTH(W_fecha)) 
                 Av_Cuota:SCREEN-VALUE = STRING(Av_Cuota).
                 
       WHEN "NIT" THEN                                                                                                
          ASSIGN Av_Monto = INTEGER((W_SMLV / 2) * 0.10)
                 Av_Monto = ROUND(Av_Monto + 25,0)
                 Av_Monto:SCREEN-VALUE = STRING(Av_Monto)                 
                 Av_Cuota = INTEGER((W_SMLV / 2) - ((W_SMLV / 2) * 0.10)) / (12 - MONTH(W_fecha))
                 Av_Cuota:SCREEN-VALUE = STRING(Av_Cuota).
                 
       WHEN "T.I" OR WHEN "R.C" THEN                                                                                  
         ASSIGN Av_Monto = INTEGER((W_SMLV * 0.10) * 0.10) 
                Av_Monto:SCREEN-VALUE = STRING(Av_Monto)
                Av_Cuota = 3150   
                Av_Cuota:SCREEN-VALUE = STRING(Av_Cuota).
                
  END CASE.
 
  ASSIGN Av_Cuota              = ROUND(Av_Cuota,0)
         Av_Cuota:SCREEN-VALUE = STRING(Av_Cuota).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AT_Destino_Ahorros wWin 
PROCEDURE AT_Destino_Ahorros :
RUN C-Ahorros(INPUT Ahorros.Nit:SCREEN-VALUE IN FRAME F_Ahorros,
                OUTPUT W_Age, OUTPUT W_Pro, OUTPUT W_Nit, OUTPUT W_Cue).
  IF W_Age NE 0 AND W_Pro NE 0 AND W_Nit NE "" AND W_Cue NE "" THEN DO:
      FIND Agencias WHERE Agencias.Agencia EQ W_Age AND Agencias.Estado EQ 1 NO-LOCK NO-ERROR.
      IF AVAILABLE(Agencias) THEN
         AT_AgenciaDestino:SCREEN-VALUE IN FRAME F_Atermino = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.
      FIND Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ W_Pro AND
                             Pro_Ahorros.Estado       EQ 1 NO-LOCK NO-ERROR.
      IF AVAILABLE(Pro_Ahorros) THEN
         AT_ProductoDestino:SCREEN-VALUE IN FRAME F_Atermino = STRING(Pro_Ahorros.Cod_Ahorro,"999") + " - " + Pro_Ahorros.Nom_Producto.
      FIND Clientes WHERE Clientes.Nit EQ W_Nit AND Clientes.Estado EQ 1 NO-LOCK NO-ERROR.
      IF AVAILABLE(Clientes) THEN
         AT_NombreDestino:SCREEN-VALUE IN FRAME F_Atermino = CAPS(Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2).
      AT_CuentaDestino:SCREEN-VALUE IN FRAME F_Atermino = W_Cue.
  END.
  ELSE DO:
     MESSAGE "Inconsistencia en la Busqueda de la cuenta destino" VIEW-AS ALERT-BOX.
     ASSIGN AT_Destino_Intereses:SCREEN-VALUE IN FRAME F_Atermino = "3".
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AT_Destino_Creditos wWin 
PROCEDURE AT_Destino_Creditos :
RUN C-Creditos(INPUT Ahorros.Nit:SCREEN-VALUE IN FRAME F_Ahorros,
                OUTPUT W_Age, OUTPUT W_Pro, OUTPUT W_Nit, OUTPUT W_Cue).
  IF W_Age NE 0 AND W_Pro NE 0 AND W_Nit NE "" AND W_Cue NE "" THEN DO:
      FIND Agencias WHERE Agencias.Agencia EQ W_Age AND Agencias.Estado EQ 1 NO-LOCK NO-ERROR.
      IF AVAILABLE(Agencias) THEN
         AT_AgenciaDestino:SCREEN-VALUE IN FRAME F_Atermino = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.
      FIND Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ W_Pro AND
                              Pro_Creditos.Estado       EQ 1 NO-LOCK NO-ERROR.
      IF AVAILABLE(Pro_creditos) THEN
         AT_ProductoDestino:SCREEN-VALUE IN FRAME F_Atermino = STRING(Pro_Creditos.Cod_Credito,"999") + " - " + Pro_Creditos.Nom_Producto.
      FIND Clientes WHERE Clientes.Nit EQ W_Nit AND Clientes.Estado EQ 1 NO-LOCK NO-ERROR.
      IF AVAILABLE(Clientes) THEN
         AT_NombreDestino:SCREEN-VALUE IN FRAME F_Atermino = CAPS(Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2).
      AT_CuentaDestino:SCREEN-VALUE IN FRAME F_Atermino = W_Cue.
  END.
  ELSE DO:
     MESSAGE "Inconsistencia en la Busqueda del Credito Destino" VIEW-AS ALERT-BOX.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cerrar_Excel wWin 
PROCEDURE Cerrar_Excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    SYSTEM-DIALOG PRINTER-SETUP. 
    PrinterName = SESSION:PRINTER-NAME.
    hWorkBooks:PrintOut(1,2,1,FALSE,PrinterName,).
    chExcelApp:displayalerts = FALSE.
    chExcelApp:Application:WorkBooks:CLOSE() NO-ERROR.
    chExcelApp:Application:QUIT NO-ERROR.
    RELEASE OBJECT hWorkBooks.
    RELEASE OBJECT chExcelApp.      
    RELEASE OBJECT chWorksheet.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE condicionales wWin 
PROCEDURE condicionales :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Contabilizar_Talonario wWin 
PROCEDURE Contabilizar_Talonario :
DEFI VAR P_ImpAplic LIKE Pro_Ahorros.Val_Talonario INIT 0.

  FIND FIRST Operacion WHERE STRING(Operacion.Cod_Operacion,"999999999") BEGINS "010302" AND
                                    Operacion.Estado                     EQ     1        AND 
                                    Operacion.Id_SYA                     EQ     NO       NO-LOCK NO-ERROR.  
  IF AVAIL(Operacion) AND Operacion.Cod_Deducible GT " " THEN DO:
     RUN RutGMF.R (INPUT TRUE,W_Agencia,Ahorros.Agencia,1,Ahorros.Cod_Ahorro,Ahorros.Nit,  
                   INPUT Ahorros.Cue_Ahorro,Operacion.Cod_Operacion,W_Total,
                   Comprobantes.Comprobante,                   
                   INPUT STRING(W_Cbte),"Dèb-Talonario",0,3,OUTPUT P_ImpAplic) NO-ERROR.                                                    
     IF ERROR-STATUS:ERROR THEN DO:                                                                
        MESSAGE "El Prog: RutGMF.P...Retorno ERROR(Salvando) no se permite la operaciòn..."               
              VIEW-AS ALERT-BOX ERROR.                                                                                 
        RETURN ERROR.                                                                                               
     END.                                                                                                           
  END.  
  CREATE Mov_Contable.
  ASSIGN Mov_Contable.Agencia        = Ahorros.Agencia
         Mov_Contable.Comprobante    = Comprobantes.Comprobante
         Mov_Contable.Cuenta         = W_CtaOpe
         Mov_Contable.Fec_Contable   = W_Fecha
         Mov_Contable.Comentario     = "Debito por Compra de Talonario"
         Mov_Contable.Usuario        = W_Usuario
         Mov_contable.Nit            = Ahorros.Nit
         Mov_Contable.Cen_Costos     = 999
         Mov_Contable.Destino        = W_Agencia
         Mov_Contable.Num_Documento  = INTEGER(W_Cbte)
         Mov_Contable.Doc_Referencia = STRING(Ahorros.Cod_Ahorro)
         /*Mov_contable.Enlace         = STRING(Taquilla.Nro_Transaccion)*/
         Mov_Contable.Fec_Grabacion  = TODAY
         Mov_Contable.Hora           = TIME
         Mov_Contable.Estacion       = W_Estacion
         Mov_Contable.CR             = Pro_Ahorros.Val_Talonario - ROUND(W_DedVal,0) NO-ERROR.

  /*Deducible*/
  IF W_DedVal NE 0 THEN DO:
    CREATE Mov_Contable.
    ASSIGN Mov_Contable.Agencia        = Ahorros.Agencia
           Mov_Contable.Comprobante    = Comprobantes.Comprobante
           Mov_Contable.Cuenta         = W_DedCta
           Mov_Contable.Fec_Contable   = W_Fecha
           Mov_Contable.Comentario     = "Deduccion: " + W_DedNom
           Mov_Contable.Usuario        = W_Usuario
           Mov_contable.Nit            = Ahorros.Nit
           Mov_Contable.Cen_Costos     = 999
           Mov_Contable.Destino        = W_Agencia
           Mov_Contable.Num_Documento  = INTEGER(W_Cbte)
           Mov_Contable.Doc_Referencia = STRING(Ahorros.Cod_Ahorro)
           /*Mov_contable.Enlace         = STRING(Taquilla.Nro_Transaccion)*/
           Mov_Contable.Fec_Grabacion  = TODAY
           Mov_Contable.Hora           = TIME
           Mov_Contable.Estacion       = W_Estacion
           Mov_Contable.CR             = ROUND(W_DedVal,0) NO-ERROR.
  END.
  /*impuesto al Deducible*/
  IF W_DedValImp NE 0 THEN DO:
    CREATE Mov_Contable.
    ASSIGN Mov_Contable.Agencia        = Ahorros.Agencia
           Mov_Contable.Comprobante    = Comprobantes.Comprobante
           Mov_Contable.Cuenta         = W_DedCtaImp
           Mov_Contable.Fec_Contable   = W_Fecha
           Mov_Contable.Comentario     = "Impuesto: " + STRING(W_DedValImp) 
           Mov_Contable.Usuario        = W_Usuario
           Mov_contable.Nit            = Ahorros.Nit
           Mov_Contable.Cen_Costos     = 999
           Mov_Contable.Destino        = W_Agencia
           Mov_Contable.Num_Documento  = INTEGER(W_Cbte)
           Mov_Contable.Doc_Referencia = STRING(Ahorros.Cod_Ahorro)
           /*Mov_contable.Enlace         = STRING(Taquilla.Nro_Transaccion)*/
           Mov_Contable.Fec_Grabacion  = TODAY
           Mov_Contable.Hora           = TIME
           Mov_Contable.Estacion       = W_Estacion
           Mov_Contable.CR             = ROUND(Pro_Ahorros.Val_Talonario * W_DedValImp,0) NO-ERROR.
  END.
  
  /*Comision*/
  IF W_ComVal NE 0 THEN DO:
    CREATE Mov_Contable.
    ASSIGN Mov_Contable.Agencia        = Ahorros.Agencia
           Mov_Contable.Comprobante    = Comprobantes.Comprobante
           Mov_Contable.Cuenta         = W_ComCta
           Mov_Contable.Fec_Contable   = W_Fecha
           Mov_Contable.Comentario     = W_ComNom
           Mov_Contable.Usuario        = W_Usuario
           Mov_contable.Nit            = Ahorros.Nit
           Mov_Contable.Cen_Costos     = 999
           Mov_Contable.Destino        = W_Agencia
           Mov_Contable.Num_Documento  = INTEGER(W_Cbte)
           Mov_Contable.Doc_Referencia = STRING(Ahorros.Cod_Ahorro)
           /*Mov_contable.Enlace         = STRING(Taquilla.Nro_Transaccion)*/
           Mov_Contable.Fec_Grabacion  = TODAY
           Mov_Contable.Hora           = TIME
           Mov_Contable.Estacion       = W_Estacion
           Mov_Contable.CR             = ROUND(W_ComVal,0) NO-ERROR.
  END.
  /*impuesto a la comision*/
  IF W_ComValImp NE 0 THEN DO:
    CREATE Mov_Contable.
    ASSIGN Mov_Contable.Agencia        = Ahorros.Agencia
           Mov_Contable.Comprobante    = Comprobantes.Comprobante
           Mov_Contable.Cuenta         = W_ComCtaImp
           Mov_Contable.Fec_Contable   = W_Fecha
           Mov_Contable.Comentario     = "Impuesto: " + STRING(W_ComValImp) 
           Mov_Contable.Usuario        = W_Usuario
           Mov_contable.Nit            = Ahorros.Nit
           Mov_Contable.Cen_Costos     = 999
           Mov_Contable.Destino        = W_Agencia
           Mov_Contable.Num_Documento  = INTEGER(W_Cbte)
           Mov_Contable.Doc_Referencia = STRING(Ahorros.Cod_Ahorro)
           /*Mov_contable.Enlace         = STRING(Taquilla.Nro_Transaccion)*/
           Mov_Contable.Fec_Grabacion  = TODAY
           Mov_Contable.Hora           = TIME
           Mov_Contable.Estacion       = W_Estacion
           Mov_Contable.CR             = ROUND(Pro_Ahorros.Val_Talonario * W_ComValImp,0) NO-ERROR.
  END.
  /*copntrapartida*/
  CREATE Mov_Contable.
  ASSIGN Mov_Contable.Agencia        = Ahorros.Agencia
         Mov_Contable.Comprobante    = Comprobantes.Comprobante
         Mov_Contable.Cuenta         = W_CtaCor
         Mov_Contable.Fec_Contable   = W_Fecha
         Mov_Contable.Comentario     = "DB X compra Libreta" 
         Mov_Contable.Usuario        = W_Usuario
         Mov_contable.Nit            = Ahorros.Nit
         Mov_Contable.Cen_Costos     = 999
         Mov_Contable.Destino        = W_Agencia
         Mov_Contable.Num_Documento  = INTEGER(W_Cbte)
         Mov_Contable.Doc_Referencia = STRING(Ahorros.Cod_Ahorro)
         /*Mov_contable.Enlace         = STRING(Taquilla.Nro_Transaccion)*/
         Mov_Contable.Fec_Grabacion  = TODAY
         Mov_Contable.Hora           = TIME
         Mov_Contable.Estacion       = W_Estacion
         Mov_Contable.DB             = ROUND(W_Total,0)   NO-ERROR.
  ASSIGN ROWID_mov = ROWID(mov_contable).

 /*Saca del sdo disponible el valor debitado*/
 FIND CURRENT Ahorros EXCLUSIVE-LOCK.
 ASSIGN Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible - W_Total
        Ahorros.Sdo_Minimo     = Ahorros.Sdo_Minimo     - W_Total.
 FIND CURRENT Ahorros NO-LOCK NO-ERROR.

 /*registro en mov_Ahorros*/
  CREATE Mov_Ahorros.
  ASSIGN Mov_Ahorros.Cod_Operacion = Operacion.Cod_Operacion
         Mov_Ahorros.cod_Ahorro    = Ahorros.Cod_Ahorro
         Mov_Ahorros.Cue_Ahorros   = Ahorros.Cue_Ahorros
         Mov_Ahorros.nit           = Ahorros.Nit
         Mov_Ahorros.Fecha         = TODAY
         Mov_Ahorros.Hora          = TIME
         Mov_Ahorros.Cpte          = Comprobantes.Comprobante
         Mov_Ahorros.Num_Documento = STRING(W_Cbte)
         Mov_Ahorros.Agencia       = W_Agencia
         Mov_Ahorros.Age_Fuente    = W_Agencia
         Mov_Ahorros.Age_Destino   = W_Agencia
         Mov_Ahorros.Usuario       = W_Usuario
         Mov_Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje
         Mov_Ahorros.Descrip        = "DB X compra Libreta"
         Mov_Ahorros.Val_Efectivo   = W_Total.
 
 /*impresion de la nota*/
  RUN formatos.r (INPUT "NOTA2MUL",ROWID_mov, 0, 0, "Contabilizacion - Debito del Talonario", 0, 0).
  APPLY "ENTRY" TO Cmb_TipoProductos IN FRAME F_Ahorros.

  /*
  FIND Formatos WHERE Formatos.Agencia     EQ W_Agencia
                  AND Formatos.Cod_Formato EQ Comprobantes.Cod_formato 
                  NO-LOCK NO-ERROR.
  IF AVAILABLE(Formatos) THEN DO:
     RUN VALUE(Formatos.Nom_Proceso) (INPUT Comprobantes.Comprobante,
                                      INPUT W_Cbte, INPUT W_Cbte,
                                      INPUT W_Agencia) NO-ERROR.
     IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "Error al llamar la rutina de impresion" SKIP
                "consulte con el administrador!" VIEW-AS ALERT-BOX.
        RETURN ERROR.
     END.
  END.
  ELSE DO:
     MESSAGE "No se encuentra el formato de impresión" SKIP
             "consulte con el administrador!" VIEW-AS ALERT-BOX.
     RETURN ERROR.
  END. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE contratoAhorroPerm wWin 
PROCEDURE contratoAhorroPerm :
/*------------------------------------------------------------------------------
  Purpose:     Imprimir en Word el Contrato de Ahorro Permanente
  Parameters:  <none>
  Notes: Creado POr GiovanniCamacho Abril 21/08      
------------------------------------------------------------------------------*/
    DEFINE VARIABLE vcTxA AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE vcTxB AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE vcTxOFA AS CHARACTER   NO-UNDO. /* Declaración de fondos parte A */
    DEFINE VARIABLE vcTxOF1 AS CHARACTER   NO-UNDO. /* Declaración de fondos parte 1 */
    DEFINE VARIABLE vcTxOF2 AS CHARACTER   NO-UNDO. /* Declaración de fondos parte 2 */
    DEFINE VARIABLE vcTxOF3 AS CHARACTER   NO-UNDO. /* Declaración de fondos parte 3 */
    DEFINE VARIABLE vcTxOF4 AS CHARACTER   NO-UNDO. /* Declaración de fondos parte 4 */
    DEFINE VARIABLE vcTxOF5 AS CHARACTER   NO-UNDO. /* Declaración de fondos parte 5 */
    DEFINE VARIABLE vcTxFL1 AS CHARACTER   NO-UNDO. /* Titulos firmas*/
    DEFINE VARIABLE vcTxFL2 AS CHARACTER   NO-UNDO. /* Linea firma:____ */
    DEFINE VARIABLE vcTxFL3 AS CHARACTER   NO-UNDO. /* Linea Nombre:___ */
    DEFINE VARIABLE vcTxFL4 AS CHARACTER   NO-UNDO. /* Linea Ident:____ */
    DEFINE VARIABLE vcMes AS CHARACTER EXTENT 12 INITIAL ["Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"].
    
    ASSIGN vcTxA = "Entre la Cooperativa del Sistema Nacional de Justicia 'JURISCOOP', " +  
        "quién en adelante se denominará LA COOPERATIVA y %nombreapellido%, " + 
        "identificado como aparece al pie de su firma, en calidad de DEPOSITANTE, " + 
        "quién en adelante se denominará EL AHORRADOR, se ha celebrado el presente contrato de " + 
        "Ahorro Permanente que se regirá por las siguientes cláusulas: PRIMERA: EL AHORRADOR se compromete a " + 
        "ahorrar mensualmente hasta que pierda la calidad de asociado de conformidad con la Ley, la suma mensual " + 
        "equivalente a sus aportes sociales actuales menos el 50% de sus aportes obligatorios a partir del día__________ del mes de____________________________ " + 
        "del año_____________. PARÁGRAFO PRIMERO. De conformidad con lo previsto en el artículo 59 del Estatuto, EL AHORRADOR  " +
        "podrá solicitar que de la suma que está obligado a aportar mensualmente se destine la mitad a su cuenta de Ahorro  " +
        "permanente. PARÁGRAFO SEGUNDO.  Cuando EL AHORRADOR supere el monto máximo de aportes obligatorios establecidos  " +
        "en el estatuto, el valor equivalente a la totalidad de su aporte mensual será destinado a la cuenta de Ahorro permanente " +
        "salvo que el ahorrador manifieste lo contrario. " + 
        "SEGUNDA: Los depósitos periódicos podrán efectuarse en cualquiera de las oficinas de Juriscoop, en efectivo o con  " +
        "cheque. En este último caso, el depósito se considerará efectuado cuando LA COOPERATIVA haya recibido la conformidad  " +
        "del respectivo banco librado. Los depósitos en cheques de otras plazas serán enviados al cobro y causarán a cargo  " +
        "del AHORRADOR la comisión vigente que esté establecida para estas transacciones, pudiéndose debitar su valor del  " +
        "depósito correspondiente. De la misma forma, podrá utilizarse el sistema de descuento por nómina, para cuyo efecto  " +
        "EL AHORRADOR firmará la autorización correspondiente en el presente documento. TERCERA: LA COOPERATIVA, liquidará trimestralmente, " + 
        "sobre los depósitos efectuados, un interés a una tasa que no podrá ser inferior al promedio ponderado de la DTF del respectivo " + 
        "periodo. La base de liquidación será el promedio del saldo del Ahorro correspondiente al mismo trimestre. " + 
        "El abono de los intereses se efectuará anual, y proporcionalmente en caso de pérdida de la calidad de asociado, " + 
        "depositados en su cuenta de Ahorros a la vista. CUARTA: LA COOPERATIVA se compromete a hacer la devolución de los depósitos " + 
        "efectuados en su cuenta individual de Ahorro Permanente una vez el asociado pierda tal calidad, de conformidad  " +
        "con la ley.  Los intereses serán pagaderos según se establece en la cláusula anterior. QUINTA: El incumplimiento  " +
        "por parte del AHORRADOR en el depósito de las sumas contempladas en el presente documento, hará que cese la  " +
        "obligación de LA COOPERATIVA de reconocer y pagar intereses, a partir del momento en que ésta se produzca y  " +
        "generará la inhabilidad del asociado de conformidad con el artículo 59 del estatuto. SEXTA. EL AHORRADOR sólo  " +
        "podrá retirar los depósitos una vez pierda la calidad de asociado. SÉPTIMA: En caso de muerte del AHORRADOR,  " +
        "Juriscoop pagará el valor del depósito y sus intereses a sus causahabientes, de acuerdo con las disposiciones legales  " +
        "vigentes a la fecha de su fallecimiento.  Según lo previsto en tales normas, podrá entregar la cantidad que éstas  " +
        "dispongan directamente a sus herederos, sin necesidad del juicio de sucesión, previa comprobación de dicha  " +
        "calidad mediante las pruebas conducentes. OCTAVA: LA COOPERATIVA podrá compensar, con cargo a los depósitos de  " +
        "Ahorro constituidos por el AHORRADOR, cualquier tipo de obligación que éste posea con LA COOPERATIVA,  " +
        "siempre y cuando se cumplan los requisitos exigidos en el artículo 1715 del Código Civil. NOVENA: EL  " +
        "AHORRADOR declara que el origen de los fondos que depositará en desarrollo del presente contrato  " +
        "proviene de actividades lícitas. DÉCIMA: El presente contrato sólo se entenderá perfeccionado una vez  " +
        "Juriscoop reciba el valor del depósito correspondiente al primer mes, de conformidad con la cláusula  " +
        "segunda del presente documento. En señal de asentamiento se firma a los %dia% días del mes de %mes%  " +
        "de %ano%.".

     ASSIGN vcTxB = "%nombreapellido%, identificado como aparece al pie de mi firma, en calidad de DEPOSITANTE, de " +
         "conformidad con lo previsto en el parágrafo del artículo 59 del estatuto, autorizo para que de la suma que " +
         "estoy obligado a aportar mensualmente se destine la mitad a la cuenta de Ahorro permanente que por virtud del " +
         "contrato que he celebrado con Juriscoop, me ha sido abierta.".

/* se quita declaracion de origen de fondos */
/*      ASSIGN vcTxOFA = "Obrando en nombre propio, de manera voluntaria y dando certeza que todo lo aquí consignado es cierto, " +                    */
/*          "realizo la siguiente declaración de origen de fondos a Juriscoop, con el propósito que se pueda dar cumplimiento a lo señalado " +        */
/*          "al respecto en la circular externa 007 de 2003 expedida por la Superintendencia de Economía Solidaria, el Estatuto Orgánico " +           */
/*          "del Sistema Financiero (Decreto 663 de 1993).  Ley 190 de 1995 (Estatuto Anticorrupción) y demás normas legales concordantes, " +         */
/*          "para el manejo de los dineros depositados en Juriscoop.".                                                                                 */
/*                                                                                                                                                     */
/*      ASSIGN vcTxOF1 = "1. Los recursos que entregue, provienen de las siguientes fuentes (Detalle de la ocupación, oficio, profesión, negocio o " + */
/*          "actividad).".                                                                                                                             */
/*      ASSIGN vcTxOF2 = "2. Declaro que los recursos que entregue, no provienen de ninguna actividad ilícita, contempladas en el código penal " +     */
/*          "colombiano o cualquier norma que lo modifique o adicione.".                                                                               */
/*      ASSIGN vcTxOF3 = "3. No admitiré que terceros efectúen depósitos a mi cuenta, con fondos provenientes de actividades ilícitas, " +             */
/*          "contempladas en código penal colombiano o cualquier norma que lo modifique o adicione, ni efectuaré transacciones " +                     */
/*          "destinadas a tales actividades a favor de personas relacionadas con las mismas.".                                                         */
/*      ASSIGN vcTxOF4 = "4. Autorizo a saldar las cuentas y depósitos que tenga en esta institución, en el caso de infracción de cualquiera " +       */
/*          "de los numerales contenidos en este documento, eximiendo a la Cooperativa de toda responsabilidad que se derive por información " +       */
/*          "errónea, falsa, inexacta, que yo hubiere proporcionado en este documento, o la violación del mismo.".                                     */
/*      ASSIGN vcTxOF5 = "Declaro que mis activos provienen de ACTIVIDADES LICITAS conforme a los declarado anteriormente.".                           */

     


     ASSIGN 
         vcTxFL1 = "     ASOCIADO AHORRADOR                                                                RESPONSABLE JURISCOOP"
         vcTxFL2 = "Firma:  ______________________                                                       Firma:  ______________________"
         vcTxFL3 = "Nombre: ______________________                                                    Nombre: ______________________"
         vcTxFL4 = "c.c:    _________________________      Huella índice Derecho          c.c.:   _________________________".


    ASSIGN vcTxA = REPLACE(vcTxA,"%nombreapellido%",TRIM(W_NomTitular:SCREEN-VALUE IN FRAME F_Ahorros)).
    ASSIGN vcTxA = REPLACE(vcTxA,"%cuota%",TRIM(CT_Cuota:SCREEN-VALUE IN FRAME F_Contractual)).
    ASSIGN vcTxA = REPLACE(vcTxA,"%dia%",TRIM(STRING(DAY(W_fecha)))).
    ASSIGN vcTxA = REPLACE(vcTxA,"%mes%",TRIM(STRING(vcMes[MONTH(W_fecha)]))).
    ASSIGN vcTxA = REPLACE(vcTxA,"%ano%",TRIM(STRING(YEAR(W_fecha)))).
    ASSIGN vcTxB = REPLACE(vcTxB,"%nombreapellido%",TRIM(W_NomTitular:SCREEN-VALUE IN FRAME F_Ahorros)).

    SESSION:SET-WAIT("general").
    CREATE "Word.application" chWordApplication.
    chWordApplication:VISIBLE=FALSE.
    chWordApplication:Documents:ADD().

    chWordApplication:selection:TypeParagraph.
    chWordApplication:selection:TypeParagraph.
    chWordApplication:selection:TypeParagraph.
    chWordApplication:Selection:FONT:bold = 9999998.
    chWordApplication:Selection:font:SIZE = 12.
    chWordApplication:Selection:ParagraphFormat:Alignment = 1.
    chWordApplication:Selection:typetext("Ahorro PERMANENTE").
    chWordApplication:selection:TypeParagraph.
    chWordApplication:selection:TypeParagraph.
    chWordApplication:selection:TypeParagraph.

    chWordApplication:Selection:FONT:bold = 9999998.
    chWordApplication:Selection:font:SIZE = 8.5.
    chWordApplication:Selection:ParagraphFormat:Alignment = 3.
    chWordApplication:Selection:typetext(vcTxA).
    chWordApplication:selection:TypeParagraph.
    chWordApplication:selection:TypeParagraph.
    chWordApplication:selection:TypeParagraph.
/*     chWordApplication:selection:TypeParagraph. */

    chWordApplication:Selection:ParagraphFormat:Alignment = 3.
    chWordApplication:Selection:typetext(vcTxFL1). /*Titulos Firmas*/
    chWordApplication:selection:TypeParagraph.
    chWordApplication:selection:TypeParagraph.
    chWordApplication:selection:TypeParagraph.
    chWordApplication:Selection:typetext(vcTxFL2). /* Firma */
    chWordApplication:selection:TypeParagraph.
    chWordApplication:Selection:typetext(vcTxFL3). /* Nombre */
    chWordApplication:selection:TypeParagraph.
    chWordApplication:Selection:typetext(vcTxFL4). /* c.c. */
    chWordApplication:selection:TypeParagraph.
    chWordApplication:selection:TypeParagraph.
    chWordApplication:selection:TypeParagraph.
/*     chWordApplication:selection:TypeParagraph. */

    chWordApplication:Selection:ParagraphFormat:Alignment = 1.
    chWordApplication:Selection:typetext("===========================================================").
    chWordApplication:selection:TypeParagraph.
    chWordApplication:selection:TypeParagraph.
    
    chWordApplication:Selection:ParagraphFormat:Alignment = 3.
    chWordApplication:Selection:typetext(vcTxB).
    chWordApplication:selection:TypeParagraph.
    chWordApplication:selection:TypeParagraph.
    chWordApplication:selection:TypeParagraph.

    chWordApplication:Selection:typetext(vcTxFL1). /*Titulos Firmas*/
    chWordApplication:selection:TypeParagraph.
    chWordApplication:selection:TypeParagraph.
    chWordApplication:selection:TypeParagraph.
    chWordApplication:Selection:typetext(vcTxFL2). /* Firma */
    chWordApplication:selection:TypeParagraph.
    chWordApplication:Selection:typetext(vcTxFL3). /* Nombre */
    chWordApplication:selection:TypeParagraph.
    chWordApplication:Selection:typetext(vcTxFL4). /* c.c. */
    chWordApplication:selection:TypeParagraph.


/* se quita declaracion de origen de fondos */
/*         /* Declaracion de fondos */            */
/*     chWordApplication:selection:TypeParagraph. */
/*     chWordApplication:selection:TypeParagraph. */
/*     chWordApplication:selection:TypeParagraph. */
/*     chWordApplication:selection:TypeParagraph. */
/*     chWordApplication:selection:TypeParagraph. */
/*     chWordApplication:selection:TypeParagraph. */
/*     chWordApplication:selection:TypeParagraph. */
/*                                                */
/*     chWordApplication:Selection:FONT:bold = 9999998.                                                                            */
/*     chWordApplication:Selection:font:SIZE = 11.                                                                                 */
/*     chWordApplication:Selection:ParagraphFormat:Alignment = 1.                                                                  */
/*     chWordApplication:Selection:typetext("DECLARACION DE ORIGEN DE FONDOS").                                                    */
/*     chWordApplication:selection:TypeParagraph.                                                                                  */
/*     chWordApplication:selection:TypeParagraph.                                                                                  */
/*                                                                                                                                 */
/*     chWordApplication:Selection:FONT:bold = 9999998.                                                                            */
/*     chWordApplication:Selection:font:SIZE = 10.                                                                                 */
/*     chWordApplication:Selection:ParagraphFormat:Alignment = 3.                                                                  */
/*     chWordApplication:Selection:typetext(vcTxOFA).                                                                              */
/*     chWordApplication:selection:TypeParagraph.                                                                                  */
/*     chWordApplication:Selection:typetext(vcTxOF1).                                                                              */
/*     chWordApplication:selection:TypeParagraph.                                                                                  */
/*     chWordApplication:Selection:typetext("_________________________________________________________________________________."). */
/*     chWordApplication:selection:TypeParagraph.                                                                                  */
/*     chWordApplication:Selection:typetext(vcTxOF2).                                                                              */
/*     chWordApplication:selection:TypeParagraph.                                                                                  */
/*     chWordApplication:Selection:typetext(vcTxOF3).                                                                              */
/*     chWordApplication:selection:TypeParagraph.                                                                                  */
/*     chWordApplication:Selection:typetext(vcTxOF4).                                                                              */
/*     chWordApplication:selection:TypeParagraph.                                                                                  */
/*     chWordApplication:Selection:typetext(vcTxOF5).                                                                              */
/*                                                                                                                                 */
/*                                                                                                                                 */
/*     ASSIGN vcTxFL2 = "Firma:  ______________________"                                                                           */
/*         vcTxFL3 = "Nombre: ______________________"                                                                              */
/*         vcTxFL4 = "c.c:    ______________________".                                                                             */
/*                                                                                                                                 */
/*     chWordApplication:selection:TypeParagraph.                                                                                  */
/*     chWordApplication:selection:TypeParagraph.                                                                                  */
/*     chWordApplication:selection:TypeParagraph.                                                                                  */
/*     chWordApplication:Selection:typetext(vcTxFL2). /* Firma */                                                                  */
/*     chWordApplication:selection:TypeParagraph.                                                                                  */
/*     chWordApplication:Selection:typetext(vcTxFL3). /* Nombre */                                                                 */
/*     chWordApplication:selection:TypeParagraph.                                                                                  */
/*     chWordApplication:Selection:typetext(vcTxFL4). /* c.c. */                                                                   */



    chWordApplication:VISIBLE=TRUE.
/*     chWordApplication:Quit(). */
    RELEASE OBJECT chWordApplication.
    SESSION:SET-WAIT("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Convertir_Tasa_Periodica wWin 
PROCEDURE Convertir_Tasa_Periodica :
DEFINE INPUT PARAMETER W_Monto LIKE Ahorros.monto.
DEFINE INPUT PARAMETER W_Tasa  LIKE Indicadores.Tasa.

CASE INTEGER(SUBSTRING(Cmb_Perliquidacion:SCREEN-VALUE IN FRAME F_Ahorros,1,2)):
     WHEN 1 THEN
         ASSIGN W_Per      = 365
                W_Perliqui = 0
                W_DiaPer   = 1.
     WHEN 2 THEN
         ASSIGN W_Per      = 12
                W_Perliqui = 4
                W_DiaPer   = 30.
     WHEN 3 THEN
         ASSIGN W_Per      = 4
                W_Perliqui = 6
                W_DiaPer   = 90.
     WHEN 4 THEN
         ASSIGN W_Per      = 2
                W_Perliqui = 8
                W_DiaPer   = 180.
     WHEN 5 THEN
         ASSIGN W_Per      = 1
                W_Perliqui = 9
                W_DiaPer   = 360.
     WHEN 6 THEN DO:
         IF At_Plazo GT 360 THEN
            RUN MostrarMensaje IN W_Manija (INPUT 325,OUTPUT W_Rpta).
         ELSE
            ASSIGN W_Per      = (360 / At_Plazo)
                   W_Perliqui = 10
                   W_DiaPer   = At_Plazo.
     END.
  END CASE.
  
  IF INTEGER(Ahorros.For_Liquidacion:SCREEN-VALUE IN FRAME F_Ahorros) EQ 2 THEN 
    W_tasper = (EXP( (w_tasa / 100)  + 1,1 / W_per)) - 1.  
   ELSE 
     IF INTEGER(Ahorros.For_Liquidacion:SCREEN-VALUE) EQ 1 THEN 
        W_TasPer = 1 - (1 / EXP((W_Tasa / 100 )+ 1,1 / W_per)). 
   
  ASSIGN Tasa_Nominal:SCREEN-VALUE     = STRING((W_TasPer * 100))
         Ahorros.Tasa:SCREEN-VALUE     = STRING((W_TasPer * W_Per) * 100)
         Interes_PorPagar:SCREEN-VALUE = STRING((W_Tasa * W_Monto) / 100).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Crearnit_Enpacto wWin 
PROCEDURE Crearnit_Enpacto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VAR zdatos   AS CHARACTER FORMAT "X(120)".
    DEFINE VAR ztels   AS CHARACTER FORMAT "X(30)".
    DEFINE VAR zsdomin AS DEC INITIAL 0.
    DEFINE VAR zconteo AS INTEGER INITIAL 0.
    ztels   = TRIM(clientes.Tel_residencia) + "." + TRIM(clientes.Tel_comercial).
    FIND Pro_Ahorros WHERE
         Pro_Ahorros.Cod_Ahorro EQ INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Ahorros,1,3)) AND 
         Pro_Ahorros.Tip_Ahorro EQ INTEGER(SUBSTRING(Cmb_TipoProductos:SCREEN-VALUE IN FRAME F_Ahorros,1,1)) NO-LOCK NO-ERROR.
    IF AVAILABLE(Pro_Ahorros) THEN zsdomin = pro_Ahorros.val_sdominimo.
    IF ztels = "." THEN  ztels = "3487300".
    zdatos = TRIM(clientes.nombre) + "." + TRIM(Clientes.apellido1) + "." + TRIM(clientes.apellido2) + "," +
            TRIM(STRING(Ahorros.sdo_disponible - zsdomin)) + "," +
            TRIM(STRING(Ahorros.sdo_disponible + Ahorros.sdo_canje)) + "," +
            TRIM(ztels) + "," + TRIM(clientes.nit) + "," + TRIM(Ahorros.cue_Ahorros) + "," +
            STRING(TIME).
    DO zconteo = 1 TO LENGTH(TRIM(zdatos)):
       IF SUBSTRING(TRIM(zdatos),zconteo,1) = ' ' OR SUBSTRING(TRIM(zdatos),zconteo,1) = '-' THEN
          zdatos = SUBSTRING(zdatos,1,zconteo - 1) + '.' + SUBSTRING(zdatos,zconteo + 1,LENGTH(TRIM(zdatos)) - zconteo ).
    END.

    DEFINE VAR comando  AS CHARACTER FORMAT "X(150)".
    DEFINE VAR lineabat AS CHARACTER FORMAT "X(150)".
    /* comando = "\\172.28.1.201\d\tempo\CreaNit_" + TRIM(W_Usuario) + ".bat" . */
    comando =  W_Pathspl + "CreaNit_" + TRIM(W_Usuario) + ".bat".
    OUTPUT TO VALUE(comando).
    /* lineabat = "\\172.28.1.201\d\tempo\crearnit.exe '" + TRIM(zdatos) + "'".  */
    lineabat = W_Pathspl + "\crearnit.exe '" + TRIM(zdatos) + "'".
    PUT lineabat SKIP(0).
    PUT "EXIT"   SKIP(0).
    OUTPUT CLOSE.
    /* Hago el llamado del ejecutable que permitira por fin la impresion del 
     documento */
    OS-COMMAND VALUE(comando) NO-CONSOLE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Debitar_Talonario wWin 
PROCEDURE Debitar_Talonario :
FIND Operacion WHERE Operacion.Cod_Operacion EQ Pro_Ahorros.Cod_CobroLibreta NO-LOCK NO-ERROR.
IF NOT AVAILABLE Operacion THEN DO:
   MESSAGE "No hay operacion disponible para debito de libreta".
   RETURN ERROR.
END.
   IF AVAILABLE Operacion THEN DO:
      IF Operacion.Cuenta EQ "" THEN DO:
         MESSAGE "No se encuentra configurada la cuenta en la operación" SKIP
                 "se cancela la operacion de creación de libreta" VIEW-AS ALERT-BOX ERROR.
         RETURN ERROR.
      END.
      ELSE DO:
         IF Operacion.Cuenta EQ "" THEN DO:
            MESSAGE "No se encuentra configurada la cuenta en la transacción" SKIP
                    "informe al adminsitrador para que rectifique"
                    VIEW-AS ALERT-BOX ERROR.
            RETURN ERROR.
         END.
         ASSIGN W_CtaOpe    = Operacion.Cuenta
                W_CodDed    = Operacion.Cod_Deducible
                W_Comision  = Operacion.Comision
                W_Operacion = Operacion.Cod_Operacion.
         IF W_CodDed NE "" THEN DO:
           FIND Deducible WHERE Deducible.Cod_Deducible = W_CodDed NO-LOCK NO-ERROR.
           IF NOT AVAILABLE Deducible THEN DO:
              MESSAGE "No existe el deducible configurado en la Transaccion" SKIP
                      "Informe al Administrador para que rectifique la Transacción" SKIP
                      "para el cobro del talonario!"
                      VIEW-AS ALERT-BOX ERROR.
              RETURN ERROR. 
           END.
           ELSE DO: 
             ASSIGN W_DedCta = Deducible.Cuenta
                    W_DedCla = Deducible.Cla_Deducible
                    W_DedVal = Deducible.Valor
                    W_DedValImp = Deducible.Valor_Impuesto
                    W_DedCtaImp = Deducible.Cuenta_Impuesto
                    W_DedNom    = Deducible.Nom_Deducible.
             IF W_DedCla EQ 1 THEN DO:
                 W_DedVal = Pro_Ahorros.Val_Talonario - (Pro_Ahorros.Val_Talonario / (1 + W_DedVal)). /*W_ComVal.*/
             END.
           END.
         END.
         IF W_Comision NE "" THEN DO:
           FIND Deducible WHERE Deducible.Cod_Deducible = W_Comision NO-LOCK NO-ERROR.
           IF NOT AVAILABLE Deducible THEN DO:
              MESSAGE "No existe un deducible con Codigo igual a la Comisión configurada" SKIP
                      "en la Transaccion" SKIP(1)
                      "Informe al Administrador para que rectifique la Transacción" SKIP
                      "para el cobro del talonario!"
                      VIEW-AS ALERT-BOX ERROR.
              RETURN ERROR. 
           END.
           ELSE DO:
             ASSIGN W_ComCta = Deducible.Cuenta
                    W_ComCla = Deducible.Cla_Deducible
                    W_ComVal = Deducible.Valor
                    W_ComValImp = Deducible.Valor_Impuesto
                    W_ComCtaImp = Deducible.Cuenta_Impuesto
                    W_ComNom    = Deducible.Nom_Deducible.
             IF W_ComCla EQ 1 THEN
                W_ComVal = Pro_Ahorro.Val_Talonario * W_ComVal.
           END.
         END.
         FIND FIRST CortoLargo WHERE CortoLargo.Agencia  EQ Ahorros.Agencia /*Busca Oficina destino*/
                         AND   CortoLargo.Clase_Producto EQ 1 
                         AND   CortoLargo.Cod_Producto   EQ INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Ahorros,1,3))
                         NO-LOCK NO-ERROR.
         IF AVAILABLE(CortoLargo) THEN DO:
            IF Clientes.Tipo_Vinculo EQ 1 THEN DO:
              W_CtaCor = CortoLargo.Cta_AsoAd.
              IF W_CtaCor EQ "" THEN DO:
                 MESSAGE "Cuenta de Asociados para el producto en Corto y largo" SKIP
                         "no esta configurada. Rectifique con el administrador!"
                         VIEW-AS ALERT-BOX ERROR.
                 RETURN ERROR.
              END.
            END.
            ELSE DO:
              W_CtaCor = CortoLargo.Cta_NoaAd.
              IF W_CtaCor EQ "" THEN DO:
                 MESSAGE "Cuenta de No Asociados para el producto en Corto y largo" SKIP
                         "no esta configurada. Rectifique con el administrador!"
                         VIEW-AS ALERT-BOX ERROR.
                 RETURN ERROR.
              END.
            END.

            IF CortoLargo.Comprobante EQ 0 THEN DO:
               MESSAGE "No se encuentra configurado el comprobante de traslado" SKIP
                       "en corto y largo para la realización de la transacción" SKIP
                       "Informe al Administrador para que Rectifique la configuración" SKIP
                       "del producto en Corto y Largo!"
                       VIEW-AS ALERT-BOX ERROR.
               RETURN ERROR.
            END.

            /*W_Total = Pro_Ahorros.Val_Talonario + ROUND(W_DedVal,0) + ROUND(Pro_Ahorros.Val_Talonario * W_DedValImp,0) + 
                ROUND(W_ComVal,0) + ROUND(Pro_Ahorros.Val_Talonario * W_ComValImp,0). */                                     
             W_Total = Pro_Ahorros.Val_Talonario + ROUND(Pro_Ahorros.Val_Talonario * W_DedValImp,0) + 
                       ROUND(W_ComVal,0) + ROUND(Pro_Ahorros.Val_Talonario * W_ComValImp,0).                                      


            IF (Ahorros.Sdo_Disponible - Pro_Ahorros.Val_SdoMinimo) LT W_Total THEN DO:
               MESSAGE "La cuenta no tiene efectivo suficiente para el debito" SKIP
                       "de la libreta o chequera." SKIP
                       "Se cancela la Creación del talonario"
                       VIEW-AS ALERT-BOX.
               RETURN ERROR.
            END.

            FIND Comprobantes WHERE Comprobantes.Agencia EQ W_Agencia AND
                                    Comprobantes.Comprobante EQ CortoLargo.Comprobante AND
                                    Comprobantes.Estado EQ 1 AND 
                                    Comprobantes.Id_Efectivo EQ NO NO-ERROR.
            IF AVAILABLE Comprobantes THEN DO:
               ASSIGN Comprobantes.Secuencia = Comprobantes.Secuencia + 1
                      W_Cbte = Comprobantes.Secuencia.
               RUN Contabilizar_Talonario NO-ERROR.
               IF ERROR-STATUS:ERROR THEN DO:
                  MESSAGE "La contabilizaciòn del Dèbito del talonario presentò error..." SKIP
                          "Rectifique por favor." VIEW-AS ALERT-BOX ERROR.
                  FIND CURRENT Comprobantes NO-LOCK NO-ERROR.
                          
                  RETURN ERROR.
               END.
               
               FIND CURRENT Comprobantes NO-LOCK NO-ERROR.
            END.
            ELSE DO:
               MESSAGE "No exite el comprobante: " STRING(CortoLargo.Comprobante,"99") SKIP
                       "que se encuentra configurado en corto y largo" SKIP
                       "Informe al Administrador para que rectifique" SKIP
                       "la configuración del comprobante en la Agencia"
                       VIEW-AS ALERT-BOX ERROR.
               RETURN ERROR.
                       
            END.
         END.
         ELSE DO:
           MESSAGE "No ha encontrado la configuración de corto y largo" SKIP
                   "para el producto de Ahorros actual. Rectifique" SKIP
                   "esta configuracion." VIEW-AS ALERT-BOX ERROR.
           RETURN ERROR.
         END.
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Deshabilitar_Fondo wWin 
PROCEDURE Deshabilitar_Fondo :
DISABLE ALL WITH FRAME F_Ahorros.
IF FRAME F_Libretas:HIDDEN EQ NO THEN DO:
   ENABLE {&List-4} WITH FRAME F_Libretas.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Digito_Chequeo wWin 
PROCEDURE Digito_Chequeo :
W_DigChe = 0.
   IF Pro_Ahorros.Pro_Digito GT 0 THEN DO:
      FIND Formatos WHERE Formatos.Agencia     EQ W_Agencia
                      AND Formatos.Cod_Formato EQ Pro_Ahorros.Pro_Digito 
                      AND Formatos.Id_Formato  EQ "AC"
                    NO-LOCK NO-ERROR.
      IF AVAILABLE(Formatos) THEN DO:
         IF Formatos.Nom_Proceso EQ "" THEN DO:
            RUN MostrarMensaje IN W_Manija (INPUT 384, OUTPUT W_Rpta).
            RETURN.
         END.
         ELSE DO:
            W_CueAho = Ahorros.Cue_Ahorros:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
            RUN VALUE(Formatos.Nom_Proceso) (INPUT-OUTPUT W_CueAho, INPUT-OUTPUT W_DigChe).
            Ahorros.Cue_Ahorros:SCREEN-VALUE = W_CueAho.
         END.
      END.
      ELSE
         RUN MostrarMensaje IN W_Manija (INPUT 300,OUTPUT W_Rpta). 
   END.
   W_Digito:SCREEN-VALUE = STRING(W_DigChe).
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
  DISPLAY Cmb_TipoProductos Cmb_Productos W_NomTitular T-ExeCobro Nombre_Tutor 
          tgl-TarjetaDB Tg_Tasa W_Digito Cmb_PerLiquidacion Tasa_Efectiva 
          Tasa_Nominal Interes_PorPagar Cmb_Estados wcomenTdb 
      WITH FRAME F_Ahorros IN WINDOW wWin.
  IF AVAILABLE Ahorros THEN 
    DISPLAY Ahorros.Nit Ahorros.Exento_3xm Ahorros.Fec_Activacion[1] 
          Ahorros.Fec_DesActivacion[1] Ahorros.Id_Subsidio_Entidad 
          Ahorros.Ind_Tipo_Subsidio Ahorros.Fec_Activacion[2] 
          Ahorros.Fec_DesActivacion[2] Ahorros.Id_Tesoro_Nacional 
          Ahorros.Fec_Activacion[3] Ahorros.Fec_DesActivacion[3] 
          Ahorros.Id_Mesada_Pensional Ahorros.Fec_Activacion[4] 
          Ahorros.Fec_DesActivacion[4] Ahorros.Cue_Ahorros Ahorros.Num_Formato 
          Ahorros.Sdo_MinCta Ahorros.Id_Tutor Ahorros.Nit_Tutor 
          Ahorros.TarjetaDB Ahorros.For_Liquidacion Ahorros.Tasa Ahorros.Estado 
      WITH FRAME F_Ahorros IN WINDOW wWin.
  ENABLE Ahorros.Exento_3xm Ahorros.Id_Subsidio_Entidad 
         Ahorros.Ind_Tipo_Subsidio Ahorros.Id_Tesoro_Nacional 
         Ahorros.Id_Mesada_Pensional Ahorros.Sdo_MinCta T-ExeCobro 
         Ahorros.Nit_Tutor tgl-TarjetaDB Tg_Tasa BUTTON-1 BUTTON-2 Btn_Consulta 
         Cmb_PerLiquidacion Btn_Ingresar BtnDone Ahorros.Estado BUTTON-95 
         Btn_Libretas BUTTON-92 Imp_Cdat wcomenTdb Imp_UtraMas BUTTON-123 
         RECT-274 RECT-275 RECT-281 RECT-284 RECT-315 RECT-317 RECT-318 
         RECT-319 RECT-334 RECT-335 
      WITH FRAME F_Ahorros IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Ahorros}
  DISPLAY W_NomUsuario 
      WITH FRAME F_Secundaria IN WINDOW wWin.
  IF AVAILABLE Ahorros THEN 
    DISPLAY Ahorros.Fec_Apertura Ahorros.Num_DepDia Ahorros.Val_DepDia 
          Ahorros.Fec_Cancelacion Ahorros.Val_DepMes Ahorros.Num_DepMes 
          Ahorros.Fec_ProLiquidacion Ahorros.Fec_UltLiquidacion 
          Ahorros.Val_RetDia Ahorros.Num_RetDia Ahorros.Fec_Ulttransaccion 
          Ahorros.Val_RetDiaCheq Ahorros.Num_RetDiaCheq Ahorros.Fec_Vencimiento 
          Ahorros.Fec_Prorroga Ahorros.Num_RetMes Ahorros.Val_RetMes 
          Ahorros.Sdo_Canje Ahorros.Sdo_UltLiquidacion Ahorros.Sdo_Disponible 
          Ahorros.Usu_Creacion 
      WITH FRAME F_Secundaria IN WINDOW wWin.
  ENABLE RECT-303 RECT-304 RECT-305 RECT-306 RECT-307 RECT-308 BUTTON-122 
      WITH FRAME F_Secundaria IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Secundaria}
  DISPLAY W_AccionLibreta 
      WITH FRAME F_Libretas IN WINDOW wWin.
  IF AVAILABLE Lib_Chequera THEN 
    DISPLAY Lib_Chequera.Num_Inicial Lib_Chequera.Num_Final 
          Lib_Chequera.Fec_Entrega Lib_Chequera.Tip_Talonario 
      WITH FRAME F_Libretas IN WINDOW wWin.
  ENABLE RECT-285 Lib_Chequera.Tip_Talonario Btn_IngLib BTN_CanLib BTN_SalLib 
         BTN_MosLib BR_Libretas BUTTON-121 
      WITH FRAME F_Libretas IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Libretas}
  DISPLAY F-AporOblig F-Salario CT_Monto CT_Puntos CT_Plazo CT_Cuota F-PorApoSue 
          CT_PeriodoPago CT_FormaPago CT_AgenciaDebito CT_ProductoDebito 
          CT_CuentaDebito CT_NombreDebito CT_FechaDebito 
      WITH FRAME F_Contractual IN WINDOW wWin.
  ENABLE RECT-278 CT_Monto CT_Plazo CT_Cuota Ahorros.porce_Aporte 
         CT_PeriodoPago CT_FormaPago CT_FechaDebito 
      WITH FRAME F_Contractual IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Contractual}
  DISPLAY R_Eleccion Cmb_TRF RActivas W-DescTRF W_NomNitRelacion W_VrAutoriz 
          R-reclamo R_Relacion2 
      WITH FRAME F_Relaciones IN WINDOW wWin.
  ENABLE RActivas Btn_CreRel Btn_Activas Btn_SalRel W_VrAutoriz R-reclamo 
         Br_Relaciones Btn_Outbene R_Relacion2 
      WITH FRAME F_Relaciones IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Relaciones}
  DISPLAY AV_Monto AV_Cuota AP_PeriodoPago AP_FormaPago AP_AgenciaDebito 
          AP_ProductoDebito AP_CuentaDebito AP_NombreDebito AP_FechaDebito 
      WITH FRAME F_Alavista IN WINDOW wWin.
  ENABLE RECT-280 RECT-283 AV_Cuota AP_FormaPago AP_FechaDebito 
      WITH FRAME F_Alavista IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Alavista}
  DISPLAY AT_Monto AT_Puntos F-FecVenc AT_Plazo AT_Destino_Intereses 
          AT_AgenciaDestino AT_ProductoDestino AT_CuentaDestino AT_NombreDestino 
      WITH FRAME F_Atermino IN WINDOW wWin.
  ENABLE RECT-277 AT_Monto F-FecVenc AT_Destino_Intereses BT_DesAT 
      WITH FRAME F_Atermino IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Atermino}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FProxLiqAV wWin 
PROCEDURE FProxLiqAV :
/*------------------------------------------------------------------------------
  Purpose:  Halla Fec-ProxLiq para aperturas Ah-Vista. (mensuales y trimestrales) 
            y contractuales (mensuales).  
------------------------------------------------------------------------------*/
 IF SUBSTRING(Cmb_Perliquidacion:SCREEN-VALUE IN FRAME F_Ahorros,1,2) EQ "02" THEN DO:
    FIND FIRST Calendario WHERE Calendario.Agencia EQ W_Agencia
                                AND Calendario.Ano EQ YEAR(W_Fecha)
                                AND Calendario.Mes EQ MONTH(W_Fecha)          
                                AND Calendario.Dia GT 0                               
                                AND Calendario.Cierre NO-LOCK NO-ERROR.               
    IF AVAIL(Calendario) THEN
       Ahorros.Fec_ProLiquidacion = DATE(MONTH(W_Fecha),Calendario.Dia,YEAR(W_Fecha)).       
 END.
 ELSE DO:
    FIND FIRST Calendario WHERE Calendario.Agencia EQ W_Agencia
                            AND DATE(Calendario.Mes,Calendario.Dia,Calendario.Ano) GE W_Fecha
                            AND Calendario.Trimestre_Cierre NO-LOCK NO-ERROR.               
    IF AVAIL(Calendario) THEN
       Ahorros.Fec_ProLiquidacion = DATE(Calendario.Mes,Calendario.Dia,Calendario.Ano).
 END.
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getAhorros wWin 
PROCEDURE getAhorros PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iplGerNew AS LOGICAL     NO-UNDO. /* Registro es nuevo? */

    FIND FIRST BAhorros WHERE BAhorros.nit EQ Ahorros.nit:SCREEN-VALUE IN FRAME F_Ahorros AND 
        BAhorros.tip_Ahorro EQ 4 AND 
        (BAhorros.cod_Ahorro EQ 8 OR BAhorros.cod_Ahorro EQ 9) AND BAhorros.estado EQ 1 NO-LOCK NO-ERROR.

    IF AVAILABLE BAhorros THEN DO: /*Tiene Aporte Obligatorio*/
        ASSIGN 
            F-AporOblig:SCREEN-VALUE IN FRAME F_Contractual = STRING(BAhorros.cuota)
            F-Salario:SCREEN-VALUE IN FRAME F_Contractual = STRING(Clientes.Salario)
            F-PorApoSue:SCREEN-VALUE  IN FRAME F_Contractual = STRING((BAhorros.cuota / Clientes.Salario) * 100)
            CT_FormaPago:SCREEN-VALUE IN FRAME F_Contractual = STRING(BAhorros.For_Pago).
            DISABLE  Ahorros.Sdo_MinCta.
        IF iplGerNew THEN DO:
/*             ASSIGN Ahorros.porce_Aporte:SCREEN-VALUE IN FRAME F_Contractual = STRING(ABS(1 - INTEGER(F-PorApoSue:SCREEN-VALUE))). */
            APPLY "ENTRY":U TO Ahorros.porce_Aporte.
            RETURN NO-APPLY.
        END. /*IF AVAILABLE BAhorros*/
        ELSE
/*             ASSIGN */
/*                 Ahorros.porce_Aporte:SCREEN-VALUE IN FRAME F_Contractual = STRING(BAhorros.porce_Aporte). */
    END.
    ELSE DO:                
        IF iplGerNew THEN DO:
        /*    MESSAGE "Cliente no tiene producto 'Aporte Obligatorio'" SKIP
            "No se puede crear este producto a no asociados"
            VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "No Asociado".
            APPLY "CHOOSE":U TO btn_Cancelar.
            RETURN NO-APPLY.*/
        END.
    END. /*ELSE DO:*/


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Grabar_AlaVista wWin 
PROCEDURE Grabar_AlaVista :
DO WITH FRAME F_AlaVista:
  DEFINE VAR wcom_alavista AS CHAR FORMAT "X(200)" INITIAL "".
  IF Ahorros.Fec_ProxDeb NE  DATE(AP_FechaDebito:SCREEN-VALUE) THEN DO:
      CREATE Hoja_Vida.        
      IF Ahorros.Fec_ProxDeb = ? THEN
         wcom_alavista = "Ingreso Fecha ProxDebAut " + STRING(Ahorros.Agencia,"999") + "-" 
                                          + STRING(Ahorros.Cod_Ahorro,"999") + "-"
                                          + Ahorros.Cue_Ahorros + "  ProxDebAut: " + AP_FechaDebito:SCREEN-VALUE.
      ELSE
         wcom_alavista = "Cambio  Fecha ProxDebAut " + STRING(Ahorros.Agencia,"999") + "-" 
                                          + STRING(Ahorros.Cod_Ahorro,"999") + "-"
                                          + Ahorros.Cue_Ahorros + "  Ant.Fec: " + string(Ahorros.Fec_ProxDeb,'99/99/9999') 
                                          + "  NuevaFec: " + AP_FechaDebito:SCREEN-VALUE. 
      ASSIGN Hoja_Vida.Codigo           = 15
             Hoja_Vida.DoctoRefer       = DEC(Ahorros.Cue_Ahorros)
             Hoja_Vida.Fec_Grabacion    = W_Fecha
             Hoja_Vida.Hora_Grabacion   = TIME
             Hoja_Vida.Nit              = Ahorros.Nit
             Hoja_Vida.Tipo             = 21
             Hoja_Vida.Usuario          = W_Usuario.                         
             Hoja_Vida.Observacion      = wcom_alavista. 
  END.
  ASSIGN Ahorros.Monto_Apertura     = DEC(AV_Monto:SCREEN-VALUE)
         Ahorros.Cuota              = DEC(AV_Cuota:SCREEN-VALUE)
         Ahorros.Pun_TasVariable    = DEC(AV_Puntos:SCREEN-VALUE)
         Ahorros.Per_Deduccion      = INTEGER(AP_PeriodoPago:SCREEN-VALUE)
         Ahorros.FOR_Pago           = INTEGER(AP_FormaPago:SCREEN-VALUE)
         Ahorros.Age_DebAutomatico  = INTEGER(SUBSTRING(AP_AgenciaDebito:SCREEN-VALUE,1,3))
         Ahorros.Cod_DebAutomatico  = INTEGER(SUBSTRING(AP_ProductoDebito:SCREEN-VALUE,1,3))
         Ahorros.Cue_DebAutomatico  = AP_CuentaDebito:SCREEN-VALUE
         Ahorros.Fec_ProxDeb        = DATE(AP_FechaDebito:SCREEN-VALUE).
         
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Grabar_ATermino wWin 
PROCEDURE Grabar_ATermino :
DO WITH FRAME F_ATermino:
  ASSIGN Ahorros.Monto_Apertura   = DEC(AT_Monto:SCREEN-VALUE)
         Ahorros.Plazo            = DEC(AT_Plazo:SCREEN-VALUE)
         Ahorros.Pun_TasVariable  = DEC(AT_Puntos:SCREEN-VALUE)
         Ahorros.Des_Intereses    = INTEGER(AT_Destino_Intereses:SCREEN-VALUE)
         Ahorros.Per_Deduccion    = 4
         Ahorros.FOR_Pago         = 1.

  IF Ahorros.Des_Intereses NE 3 THEN DO:
     ASSIGN Ahorros.Agencia_Destino = INTEGER(SUBSTRING(AT_AgenciaDestino:SCREEN-VALUE,1,3))
            Ahorros.Pro_Destino     = INTEGER(SUBSTRING(AT_ProductoDestino:SCREEN-VALUE,1,3))
            Ahorros.Cue_Destino     = AT_CuentaDestino:SCREEN-VALUE.
  END.
  DISABLE AT_Puntos AT_Plazo AT_Monto.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Grabar_Cambio wWin 
PROCEDURE Grabar_Cambio :
DEF INPUT PARAMETER W_Cambio  AS CHA.
DEF INPUT PARAMETER T_Proceso AS CHA.
  CREATE Mov_Ahorros.
  ASSIGN Mov_Ahorros.Cod_Operacion = 999999999
         Mov_Ahorros.cod_Ahorro    = Ahorros.Cod_Ahorro
         Mov_Ahorros.Cue_Ahorros   = Ahorros.Cue_Ahorros
         Mov_Ahorros.nit           = Ahorros.Nit
         Mov_Ahorros.Fecha         = W_Fecha
         Mov_Ahorros.Hora          = TIME
         Mov_Ahorros.Num_Documento = T_Proceso
         Mov_Ahorros.Agencia       = W_Agencia
         Mov_Ahorros.Age_Fuente    = W_Agencia
         Mov_Ahorros.Age_Destino   = W_Agencia
         Mov_Ahorros.Usuario       = W_Usuario
         Mov_Ahorros.Val_Cheque    = 0
         Mov_Ahorros.Val_Efectivo  = 0
         Mov_Ahorros.Descrip       = W_Cambio.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Grabar_Contractual wWin 
PROCEDURE Grabar_Contractual :
DO WITH FRAME F_Contractual:
  ASSIGN Ahorros.Monto_Apertura     = DEC(CT_Monto:SCREEN-VALUE)
         Ahorros.Plazo              = DEC(CT_Plazo:SCREEN-VALUE)
         Ahorros.Cuota              = DEC(CT_Cuota:SCREEN-VALUE)
         Ahorros.Pun_TasVariable    = DEC(CT_Puntos:SCREEN-VALUE)
         Ahorros.Per_Deduccion      = INTEGER(CT_PeriodoPago:SCREEN-VALUE)
         Ahorros.FOR_Pago           = INTEGER(CT_FormaPago:SCREEN-VALUE)
         Ahorros.Age_DebAutomatico  = INTEGER(SUBSTRING(CT_AgenciaDebito:SCREEN-VALUE,1,3))
         Ahorros.Cod_DebAutomatico  = INTEGER(SUBSTRING(CT_ProductoDebito:SCREEN-VALUE,1,3))
         Ahorros.Cue_DebAutomatico  = CT_CuentaDebito:SCREEN-VALUE
         Ahorros.Fec_ProxDeb        = DATE(CT_FechaDebito:SCREEN-VALUE)
         Ahorros.porce_aporte       = DEC(Ahorros.porce_aporte:SCREEN-VALUE).
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Grabar_Datos wWin 
PROCEDURE Grabar_Datos :
DEF VAR tippro AS INT INITIAL 0.

FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Tip_Ahorro EQ INT(SUBSTR(Cmb_TipoProductos:SCREEN-VALUE IN FRAME F_Ahorros,1,1))
                         AND Pro_Ahorros.Cod_Ahorro EQ INT(SUBSTR(Cmb_Productos:SCREEN-VALUE IN FRAME F_Ahorros,1,3)) NO-LOCK NO-ERROR.

tippro = INT(SUBSTR(Cmb_TipoProductos:SCREEN-VALUE IN FRAME F_Ahorros,1,1)).

IF NuevaCta THEN DO:
    CREATE Ahorros.
    
    IF AVAIL Pro_Ahorros THEN DO:
        FIND CURRENT Pro_Ahorros NO-ERROR.

        IF Pro_Ahorros.Id_Consecutivo THEN
            ASSIGN Pro_Ahorros.Num_Consecutivo = Pro_Ahorros.Num_Consecutivo + 1
                   Ahorros.Cue_Ahorros = STRING(Pro_Ahorros.Num_Consecutivo)
                   Ahorros.Cue_Ahorros:SCREEN-VALUE = STRING(Pro_Ahorros.Num_Consecutivo).
        ELSE
            Ahorros.Cue_Ahorros = Ahorros.Cue_Ahorros:SCREEN-VALUE.

        FIND CURRENT Pro_Ahorros NO-LOCK NO-ERROR.
    END.

    ASSIGN Ahorros.Detalle_Estado = 1
           Ahorros.Agencia = W_Agencia
           Ahorros.Tip_Ahorro = tippro
           Ahorros.Cod_Ahorro = INT(SUBSTR(Cmb_Productos:SCREEN-VALUE,1,3))
           Ahorros.Nit = Ahorros.Nit:SCREEN-VALUE
           Ahorros.Tasa = DEC(Ahorros.Tasa:SCREEN-VALUE)
           Ahorros.trf = 0
           Ahorros.TRF_notas = "".

    Cmb_Estados:SCREEN-VALUE = Cmb_Estados:ENTRY(1).

    IF SUBSTR(Cmb_TipoProductos:SCREEN-VALUE,1,1) LE "2" THEN DO:
        Ahorros.Fec_ProLiquidacion = W_Fecha.

        IF SUBSTR(Cmb_Perliquidacion:SCREEN-VALUE,1,2) EQ "02" OR SUBSTR(Cmb_Perliquidacion:SCREEN-VALUE,1,2) EQ "03" THEN
            RUN FProxLiqAV.
    END.
END.
ELSE DO:
    FIND CURRENT Ahorros NO-ERROR.

    IF ahorros.estado:SCREEN-VALUE = "1" THEN DO:
        ahorros.estado = 1.
        ahorros.detalle_Estado = INTEGER(SUBSTRING(cmb_Estados:SCREEN-VALUE,1,2)).
        ahorros.fec_Cancelacion = ?.
    END.
    ELSE DO:
        IF ahorros.sdo_disponible = 0 AND ahorros.sdo_Canje = 0 AND ahorros.int_pagar = 0 AND ahorros.int_causado = 0 THEN DO:
            ahorros.estado = 2.
            ahorros.detalle_Estado = INTEGER(SUBSTRING(cmb_Estados:SCREEN-VALUE,1,2)).

            IF ahorros.fec_Cancelacion = ? THEN
                ahorros.fec_Cancelacion = w_fecha.
        END.
        ELSE DO:
            MESSAGE "El producto aun posee saldos pendientes por cancelar." SKIP
                    "No se permite su inactivación."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.
    END.
END.

FIND CURRENT Ahorros NO-ERROR.
DO WITH FRAME F_Relaciones.
    IF NOT NuevaCta THEN
        ASSIGN Ahorros.trf = INT(SUBSTR(Cmb_TRF:SCREEN-VALUE,1,2))
               Ahorros.TRF_notas = CAPS(TRIM(W-DescTRF:SCREEN-VALUE)).
END.

DO WITH FRAME F_Ahorros.
    ASSIGN t-execobro.
    ASSIGN Ahorros.Num_Formato       = Ahorros.Num_Formato:SCREEN-VALUE
            Ahorros.Sdo_MinCta        = DEC(Ahorros.Sdo_MinCta:SCREEN-VALUE)
            Ahorros.Per_Liquidacion   = INT(SUBSTR(Cmb_PerLiquidacion:SCREEN-VALUE,1,2))
            Ahorros.For_Liquidacion   = INT(Ahorros.FOR_Liquidacion:SCREEN-VALUE)
            Ahorros.Estado            = INT(Ahorros.Estado:SCREEN-VALUE)
            Ahorros.Ind_Tipo_Subsidio = INT(Ahorros.Ind_Tipo_Subsidio:SCREEN-VALUE)
            Ahorros.Detalle_Estado    = INT(SUBSTR(Cmb_Estados:SCREEN-VALUE,1,2))
            Ahorros.Usu_Creacion      = W_Usuario           
            Ahorros.IdNombre          = Clientes.Nombre     
            Ahorros.IdApellido1       = Clientes.Apellido1  
            Ahorros.Id_Tutor
            Ahorros.Nit_Tutor
            Ahorros.Tasa.
         
IF tippro = 1 OR tippro =5 THEN DO:
 ASSIGN     Ahorros.Exento_3xM          Ahorros.Fec_Activacion[1] Ahorros.Fec_DesActivacion[1]
            Ahorros.Id_Tesoro_Nacional  Ahorros.Fec_Activacion[2] Ahorros.Fec_DesActivacion[2]
            Ahorros.Id_Subsidio_Entidad Ahorros.Fec_Activacion[3] Ahorros.Fec_DesActivacion[3]
            Ahorros.Id_Mesada_Pensional Ahorros.Fec_Activacion[4] Ahorros.Fec_DesActivacion[4]. 

     IF (t-execobro AND Ahorros.ajuste NE 1) OR (NOT t-execobro AND Ahorros.ajuste NE 0) THEN DO:
         IF t-execobro THEN ASSIGN Ahorros.ajuste = 1 wconcep = 'Activa   NO Cobro Nomina - Tarjeta Débito'.
                       ELSE ASSIGN Ahorros.ajuste = 0 wconcep = 'Inactiva NO Cobro Nomina - Tarjeta Débito'.
         CREATE Hoja_Vida.                                  
         ASSIGN Hoja_Vida.Asunto_Cumplido  = TRUE           
                Hoja_Vida.Codigo           = 7              
                Hoja_Vida.Fec_Grabacion    = W_Fecha        
                Hoja_Vida.Hora_Grabacion   = TIME           
                Hoja_Vida.Nit              = Ahorros.Nit    
                Hoja_Vida.Observacion      = wconcep        
                Hoja_Vida.Tipo             = 21             
                Hoja_Vida.Usuario          = W_Usuario. 
      END.
      END.
      IF Tgl-TarjetaDB THEN 
      IF Ahorros.TarjetaDB:SCREEN-VALUE EQ "TARJ.DB.SOLICIT." THEN DO:
          MESSAGE "Està   seguro  de  matricular esta Cuenta  para  uso de  Tarjeta Dèbito, " SKIP
                  "Si tiene Libreta de Ahorro, esta  quedarà  inmediatamente  deshabilitada." SKIP(1)
                  "Desea seguir con la matricula de Tarjeta Dèbito."
                   VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO TITLE "Creación de TARJETA DEBITO"
          UPDATE choice AS LOG.
        IF choice THEN DO:
           ASSIGN Ahorros.fec_creaTdb    = w_fecha   
                  Ahorros.Fec_CancTdb    = ?
                  Ahorros.Fec_BloqueoTdb = ?
                  Ahorros.TarjetaDB      = "TARJ.DB.SOLICIT."
                  Btn_Libretas:VISIBLE   = FALSE
                  Tgl-TarjetaDB:VISIBLE  = FALSE.
           FOR EACH Lib_Chequera WHERE Lib_Chequera.Agencia      = Ahorros.Agencia
                                   AND Lib_Chequera.cue_Ahorro   = Ahorros.cue_Ahorro
                                   AND Lib_Chequera.Cod_producto = Ahorros.Cod_Ahorro
                                   AND Lib_Chequera.estado       = 1:
               ASSIGN Lib_Chequera.estado  = 2.
           END.
           CREATE Hoja_Vida.               
           ASSIGN Hoja_Vida.Asunto_Cumplido  = TRUE
                  Hoja_Vida.Codigo           = 7
                  Hoja_Vida.Fec_Grabacion    = W_Fecha
                  Hoja_Vida.Hora_Grabacion   = TIME
                  Hoja_Vida.Nit              = Ahorros.Nit
                  Hoja_Vida.Observacion      = "Tarjeta Dèbito"
                  Hoja_Vida.Tipo             = 21
                  Hoja_Vida.Usuario          = W_Usuario.
        /* RUN CrearNit_Enpacto. se hace por enlinea */ 
        END.
        ELSE
          ASSIGN Tgl-TarjetaDB:SCREEN-VALUE = "No"  Ahorros.TarjetaDB:SCREEN-VALUE = "".
      END.
      IF W_CancTarDB THEN DO:
         W_CancTarDB = FALSE. 
         IF tgl-TarjetaDb:SCREEN-VALUE  = "No" AND Ahorros.tarjetadb NE "" THEN DO:
            MESSAGE "La tarjeta dèbito  quedará    inmediatamente  deshabilitada, " SKIP
                    "para las operaciones internas.  Recuerde retener  la tarjeta " SKIP
                    "e informar inmediatamente al administrador de Tarjeta Débito " SKIP
                    "con el fin de eliminarla para las transacciones externas.     "SKIP(2) 
                    "..Està   seguro  de  CANCELAR la Tarjeta Dèbito  ?          "
                     VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO TITLE "Creación de TARJETA DEBITO"
            UPDATE choice2 AS LOG.
            IF choice2 THEN DO:
               ASSIGN Ahorros.Fec_CancTdb        = w_fecha
                      Ahorros.Fec_BloqueoTdb     = w_fecha
                      wcomenTdb:SCREEN-VALUE     = "Cancelada"
                      Btn_Libretas:VISIBLE       = TRUE
                      tgl-TarjetaDb:VISIBLE      = TRUE
                      tgl-tarjetaDB:SENSITIVE    = YES
                      tgl-TarjetaDb:SCREEN-VALUE = "No".                  
               CREATE Hoja_Vida.               
               ASSIGN Hoja_Vida.Asunto_Cumplido  = TRUE
                      Hoja_Vida.Codigo           = 9
                      Hoja_Vida.Fec_Grabacion    = W_Fecha
                      Hoja_Vida.Hora_Grabacion   = TIME
                      Hoja_Vida.Nit              = Ahorros.Nit
                      Hoja_Vida.Observacion      = "Cancela Tarjeta Dèbito Nro " + TRIM(Ahorros.TarjetaDB)
                      Hoja_Vida.Tipo             = 21
                      Hoja_Vida.Usuario          = W_Usuario
                      Ahorros.TarjetaDB = "".
            END.
            ELSE 
            ASSIGN Btn_Libretas:VISIBLE = FALSE tgl-TarjetaDb:SCREEN-VALUE = "Yes".
         END.
      END.
  END.
      MESSAGE "INFORMACION GRABADA!" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Graba_HojaVida wWin 
PROCEDURE Graba_HojaVida :
/*------------------------------------------------------------------------------
  Purpose:   Graba en hoja-Vida libreta no cobrada, Usuario y el que autoriza.  
  Notes:     Feb.7/06 GAER.    
------------------------------------------------------------------------------*/
   CREATE Hoja_Vida.               
   ASSIGN Hoja_Vida.Asunto_Cumplido  = TRUE
          Hoja_Vida.Codigo           = 1
          Hoja_Vida.DoctoRefer       = Lib_Chequera.Num_Inicial 
          Hoja_Vida.Fec_Grabacion    = W_Fecha
          Hoja_Vida.Hora_Grabacion   = TIME
          Hoja_Vida.Nit              = Ahorros.Nit
          Hoja_Vida.Observacion      = STRING(Ahorros.Agencia,"999")    + "-" +
                                       STRING(Ahorros.Cod_Ahorro,"999") + "-"
                                       + Ahorros.Cue_Ahorros
          Hoja_Vida.Tipo             = 31
          Hoja_Vida.Usuario          = W_Usuario
          Hoja_Vida.Instancia        = INTEGER(W_Autorizo).                         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Habilitar_Campos wWin 
PROCEDURE Habilitar_Campos :
FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Tip_Ahorro EQ INT(SUBSTR(Cmb_TipoProductos:SCREEN-VALUE IN FRAME F_Ahorros,1,1))
                         AND Pro_Ahorros.Cod_Ahorro EQ INT(SUBSTR(Cmb_Productos:SCREEN-VALUE IN FRAME F_Ahorros,1,3)) NO-LOCK NO-ERROR.
IF AVAIL Pro_Ahorros AND Pro_Ahorros.Tip_Ahorro NE 1 AND Pro_Ahorros.Id_Persona NE 2 THEN
    ASSIGN tgl-TarjetaDb:VISIBLE = FALSE
           Ahorros.TarjetaDB:VISIBLE = FALSE
           wcomenTdb:VISIBLE = FALSE
           t-ExeCobro:VISIBLE = FALSE
           t-ExeCobro:SENSITIVE = NO.
ELSE
    IF AVAIL pro_Ahorros AND Pro_Ahorros.Tip_Ahorro = 1 THEN DO:
        Btn_Libretas:VISIBLE = TRUE.
        FRAME F_Libretas:VISIBLE = false.
        
        ENABLE Btn_Libretas
               ahorros.Id_Subsidio_Entidad
               Ahorros.Exento_3xm
               Ahorros.Id_tesoro_Nacional
               Ahorros.Id_Mesada_pensional
               Ahorros.Ind_tipo_Subsidio.

        IF (Pro_Ahorros.Cod_Ahorro = 1 OR Pro_Ahorros.Cod_Ahorro = 3) THEN DO:
            ASSIGN tgl-TarjetaDb:VISIBLE = TRUE
                   Ahorros.TarjetaDB:VISIBLE = TRUE
                   wcomenTdb:VISIBLE = TRUE
                   tgl-tarjetaDB:SENSITIVE = YES
                   t-ExeCobro:VISIBLE = TRUE
                   t-ExeCobro:SENSITIVE = YES.
                   
            ENABLE wcomenTdb.

            IF NOT nuevacta THEN
                Ahorros.tarjetaDB:SCREEN-VALUE IN FRAME f_Ahorros = Ahorros.TarjetaDB.
        END.
        ELSE DO:
            ASSIGN tgl-TarjetaDb:VISIBLE = FALSE
                   tgl-tarjetaDB:SENSITIVE = NO
                   tgl-TarjetaDb:SCREEN-VALUE = "No"
                   Ahorros.TarjetaDB:VISIBLE = FALSE
                   t-execobro:VISIBLE = FALSE
                   t-execobro:SENSITIVE = NO
                   wcomenTdb:VISIBLE = FALSE
                   t-execobro:SCREEN-VALUE = "No".
        END.
    END.
    ELSE DO:
        ASSIGN tgl-TarjetaDb:VISIBLE = FALSE
               tgl-tarjetaDB:SENSITIVE = NO
               tgl-TarjetaDb:SCREEN-VALUE = "No"
               Ahorros.TarjetaDB:VISIBLE = FALSE
               t-execobro:VISIBLE = FALSE
               t-execobro:SENSITIVE = NO
               wcomenTdb:VISIBLE = FALSE
               t-execobro:SCREEN-VALUE = "No".
    END.

IF AVAIL Pro_Ahorros AND NuevaCta THEN DO:
    DISABLE Ahorros.Sdo_Mincta.
    DISABLE ahorros.estado.

    ENABLE Ahorros.Nit WITH FRAME F_Ahorros.

    IF Pro_Ahorros.Id_Talonario NE 1 AND Pro_Ahorros.Id_Talonario NE 2 THEN DO:
        DISABLE Btn_Libretas WITH FRAME F_Ahorros.
        DISABLE Btn_libretas WITH FRAME F_Libretas.

        W_AccionLibreta:SCREEN-VALUE IN FRAME F_Libretas = "El Producto No maneja Talonario".
    END.
    ELSE DO:
        ENABLE Btn_Libretas WITH FRAME F_Libretas.
        ENABLE Btn_Libretas WITH FRAME F_Ahorros.

        W_AccionLibreta:SCREEN-VALUE IN FRAME F_Libretas = "Talonario Activo al Momento".
    END.

    IF Pro_Ahorros.Id_Salminimo THEN DO:
        Ahorros.Sdo_MinCta:SCREEN-VALUE = STRING(Pro_Ahorros.Val_SdoMinimo).

        IF NOT Pro_Ahorros.Tip_Salminimo THEN
            Ahorros.Sdo_MinCta:SENSITIVE = TRUE.
    END.

    IF Pro_Ahorros.Id_Consecutivo THEN
        ASSIGN Ahorros.Cue_Ahorros:SCREEN-VALUE IN FRAME F_Ahorros = STRING(Pro_Ahorros.Num_Consecutivo + 1)
               Ahorros.Cue_Ahorros:SENSITIVE IN FRAME F_Ahorros = FALSE.
    ELSE
        ASSIGN Ahorros.num_formato:SCREEN-VALUE IN FRAME F_Ahorros = ""
               Ahorros.Cue_Ahorros:SENSITIVE IN FRAME F_Ahorros = TRUE.

    IF Pro_Ahorros.Id_NumAlterno THEN
        ASSIGN Ahorros.num_formato:SENSITIVE = TRUE
               Ahorros.num_formato:SCREEN-VALUE = Ahorros.Cue_Ahorros:SCREEN-VALUE.
    ELSE
        ASSIGN Ahorros.num_formato:SENSITIVE = FALSE
               Ahorros.num_formato:SCREEN-VALUE = "".

    ASSIGN Cmb_Productos:SCREEN-VALUE = STRING(Pro_Ahorros.Cod_Ahorro,"999") + " - " + Pro_Ahorros.Nom_Producto
           Cmb_TipoProductos:SCREEN-VALUE = Cmb_TipoProductos:ENTRY(INT(Pro_Ahorros.Tip_Ahorro)).
END.

IF Pro_Ahorros.tip_Ahorro EQ 3  OR Pro_Ahorros.tip_Ahorro EQ 2 THEN DO:
    IF Pro_Ahorros.tip_Ahorro EQ 3 THEN DO:
        ASSIGN Imp_Cdat:SENSITIVE IN FRAME F_Ahorros = TRUE
               Imp_Cdat:LABEL IN FRAME F_Ahorros = "Impres. CDAT".

        Cmb_PerLiquidacion:SENSITIVE IN FRAME F_Ahorros = TRUE.
    END.       
    ELSE DO:
        ASSIGN FRAME F_Contractual:VISIBLE = TRUE
               Imp_Cdat:LABEL IN FRAME F_Ahorros = "Debito Automatico"
               imp_utramas:SENSITIVE IN FRAME f_ahorros = TRUE.

        ASSIGN Ahorros.porce_Aporte:VISIBLE IN FRAME F_Contractual = FALSE
               F-PorApoSue:VISIBLE IN FRAME F_Contractual = FALSE
               F-AporOblig:VISIBLE IN FRAME F_Contractual = FALSE
               F-Salario:VISIBLE IN FRAME F_Contractual = FALSE
               Ahorros.porce_Aporte:SENSITIVE IN FRAME F_Contractual = FALSE
               CT_PeriodoPago:SENSITIVE IN FRAME F_Contractual = TRUE
               CT_Monto:SENSITIVE IN FRAME F_Contractual = TRUE
               CT_Puntos:SENSITIVE IN FRAME F_Contractual = TRUE
               CT_Plazo:SENSITIVE IN FRAME F_Contractual = TRUE
               CT_Cuota:SENSITIVE IN FRAME F_Contractual = TRUE.

        ENABLE Imp_Cdat WITH FRAME f_ahorros.
        ENABLE imp_Utramas.

        IF Pro_Ahorros.tip_Ahorro EQ 2 AND Pro_Ahorros.cod_Ahorro EQ 38 THEN
            ASSIGN Imp_utramas:SENSITIVE IN FRAME F_Ahorros = TRUE
                   Imp_utramas:LABEL IN FRAME F_Ahorros = "I.UtraVivienda".
    END.
END.
ELSE DO:
    IF pro_ahorros.tip_Ahorro NE 3 AND pro_ahorros.tip_Ahorro NE 2 THEN
        ASSIGN Imp_Cdat:SENSITIVE IN FRAME F_Ahorros = FALSE
               FRAME F_Contractual:VISIBLE = FALSE.
    ELSE
        IF Pro_Ahorros.tip_Ahorro EQ 4 THEN
            AV_Cuota:SENSITIVE IN FRAME F_Alavista = TRUE.
END.

IF AVAIL Pro_ahorros AND Pro_Ahorros.Id_Persona EQ 2 THEN
    ASSIGN tgl-TarjetaDb:VISIBLE = FALSE
           Ahorros.TarjetaDB:VISIBLE = FALSE
           wcomenTdb:VISIBLE = FALSE
           t-ExeCobro:VISIBLE = FALSE
           t-ExeCobro:SENSITIVE = NO
           Ahorros.Id_Tutor:VISIBLE = YES
           Ahorros.Nit_Tutor:VISIBLE = YES
           Nombre_Tutor:VISIBLE = YES.
ELSE
    ASSIGN Ahorros.Id_Tutor:VISIBLE = NO
           Ahorros.Nit_Tutor:VISIBLE = NO
           Nombre_Tutor:VISIBLE = NO.

Tpdt = Pro_Ahorros.Tip_Ahorro.

DO WITH FRAME f_ahorros:
    IF Tpdt EQ 3 THEN
        Cmb_Perliquidacion:SENSITIVE = TRUE.

    IF (Tpdt EQ 1 OR Tpdt EQ 5) THEN DO:
        ASSIGN Ahorros.Exento_3xm:VISIBLE = TRUE
               Ahorros.Id_Subsidio_Entidad:VISIBLE = TRUE
               Ahorros.Ind_Tipo_Subsidio:VISIBLE = TRUE
               Ahorros.Id_Tesoro_Nacional:VISIBLE = TRUE
               Ahorros.Id_Mesada_Pensional:VISIBLE = TRUE
               Ahorros.Fec_Activacion[1]:VISIBLE = TRUE
               Ahorros.Fec_Desactivacion[1]:VISIBLE = TRUE
               Ahorros.Fec_Activacion[2]:VISIBLE = TRUE
               Ahorros.Fec_Desactivacion[2]:VISIBLE = TRUE
               Ahorros.Fec_Activacion[3]:VISIBLE = TRUE
               Ahorros.Fec_Desactivacion[3]:VISIBLE = TRUE
               Ahorros.Fec_Activacion[4]:VISIBLE = TRUE
               Ahorros.Fec_Desactivacion[4]:VISIBLE = TRUE
               BUTTON-123:VISIBLE = TRUE.
    END.
    ELSE
        ASSIGN Ahorros.Exento_3xm:VISIBLE = FALSE
               Ahorros.Id_Subsidio_Entidad:VISIBLE = false
               Ahorros.Ind_Tipo_Subsidio:VISIBLE = FALSE
               Ahorros.Id_Tesoro_Nacional:VISIBLE = FALSE
               Ahorros.Id_Mesada_Pensional:VISIBLE = FALSE
               Ahorros.Fec_Activacion[1]:VISIBLE = FALSE
               Ahorros.Fec_Desactivacion[1]:VISIBLE = FALSE
               Ahorros.Fec_Activacion[2]:VISIBLE = FALSE
               Ahorros.Fec_Desactivacion[2]:VISIBLE = FALSE
               Ahorros.Fec_Activacion[3]:VISIBLE = FALSE
               Ahorros.Fec_Desactivacion[3]:VISIBLE = FALSE
               Ahorros.Fec_Activacion[4]:VISIBLE = FALSE
               Ahorros.Fec_Desactivacion[4]:VISIBLE = FALSE
               BUTTON-123:VISIBLE = FALSE.
END.

FIND FIRST usuarios WHERE usuarios.usuario = w_usuario NO-LOCK NO-ERROR.
IF AVAILABLE usuarios THEN DO:
    IF usuarios.prioridad = 6 THEN
        ahorros.estado:SENSITIVE = TRUE.
    ELSE
        ahorros.estado:SENSITIVE = FALSE.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Habilitar_Fondo wWin 
PROCEDURE Habilitar_Fondo :
ENABLE {&List-6} WITH FRAME F_Ahorros.
RUN Mostrar_General.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Hallar_Tasa wWin 
PROCEDURE Hallar_Tasa :
DEFINE INPUT PARAMETER  W_Indicador LIKE Indicadores.Indicador.
    DEFINE INPUT PARAMETER  W_Monto     LIKE Ahorros.Monto.
    DEFINE INPUT PARAMETER  W_Plazo     LIKE Ahorros.Plazo.
    DEFINE OUTPUT PARAMETER W_TasaW     LIKE Indicadores.Tasa.
    DEFINE OUTPUT PARAMETER W_PuntosW   LIKE Ran_Intereses.Puntos.     
IF AVAILABLE Clientes THEN DO:
 /*   MESSAGE  w_indicador "--" W_tasa "--" W_puntos "paso 3" w_fecha VIEW-AS ALERT-BOX INFO BUTTON OK.*/
    FIND Indicadores WHERE
       Indicadores.Indicador EQ W_Indicador AND
       Indicadores.FecVcto   GE W_Fecha      AND
       Indicadores.Estado    EQ 1           NO-LOCK NO-ERROR.
  IF AVAILABLE Indicadores THEN DO:
   /*    MESSAGE  "Encontrado en indicadores" VIEW-AS ALERT-BOX INFO BUTTON OK. */
      ASSIGN W_TasaW = Indicadores.Tasa.          
     IF NOT Indicadores.Rangos THEN DO:
       ASSIGN W_TasaW = Indicadores.Tasa.
        /*RUN Convertir_Tasa_Periodica (INPUT W_Monto, INPUT W_TasaW).*/
     END.
     ELSE DO:
        IF INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Ahorros,1,3)) EQ 2 AND                  
           INTEGER(SUBSTRING(Cmb_TipoProductos:SCREEN-VALUE IN FRAME F_Ahorros,1,1)) EQ 1 THEN
           FIND FIRST Ran_Intereses 
             WHERE /*Ran_Intereses.Agencia    EQ W_Agencia                  AND*/
                   Ran_Intereses.Indicador  EQ W_Indicador                AND
        /*           W_Monto                  GE Ran_Intereses.Val_Inicial  AND
                   W_Monto                  LE Ran_Intereses.Val_Final    AND
                   W_plazo                  GE Ran_Intereses.Pla_Inicial  AND
                   W_Plazo                  LE Ran_Intereses.Pla_Final    AND*/
                   Ran_Interes.Estado       EQ 1 NO-LOCK NO-ERROR.
        ELSE
            FIND FIRST Ran_Intereses 
              WHERE Ran_Intereses.Indicador  EQ W_Indicador                AND
                    W_Monto                  GE Ran_Intereses.Val_Inicial  AND
                    W_Monto                  LE Ran_Intereses.Val_Final    AND
                    W_plazo                  GE Ran_Intereses.Pla_Inicial  AND
                    W_Plazo                  LE Ran_Intereses.Pla_Final    AND
                    Ran_Interes.Estado       EQ 1 NO-LOCK NO-ERROR.

        IF AVAILABLE Ran_Intereses THEN DO:
           ASSIGN W_TasaW   = Indicadores.Tasa + Ran_Intereses.Puntos_Asoc. 
           W_PuntosW    = Ran_Intereses.Puntos_Asoc.
           /* MESSAGE  "Encontrado en RAn" w_tasaw VIEW-AS ALERT-BOX INFO BUTTON OK.*/
          /* ASSIGN MaximoPuntos = Ran_Intereses.Pun_Negociables 
                  W_PuntosW    = Ran_Intereses.Pun_Negociables.  */
                       
/*           IF Pro_Ahorros.Tip_Producto NE 1 OR Pro_Ahorros.Tip_Producto NE 4 THEN
              Ahorros.Pun_TasVariable:SENSITIVE = TRUE.
           IF Ran_Intereses.Pun_Negociable EQ 0 THEN 
              Ahorros.Pun_TasVariable:SENSITIVE = FALSE.          */
           /*RUN Convertir_Tasa_Periodica (INPUT W_Monto, INPUT W_TasaW + W_PuntosW).*/
        END.
        ELSE DO:
           RUN MostrarMensaje IN W_Manija (INPUT 257, OUTPUT W_Rpta).
           ASSIGN W_TasaW   = 0
                  W_PuntosW = 0.
        END.
     END.
     
  END.
  ELSE DO:
        MESSAGE "NO SE ENCONTRO TASA O INDICADOR" VIEW-AS ALERT-BOX INFO BUTTON OK.
        RETURN NO-APPLY.
  END. 
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE imprime_debitoA wWin 
PROCEDURE imprime_debitoA :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR age1 AS INT.
 DEF VAR pro1 AS INT.
 DEF VAR cue1 AS CHAR.
 DEF VAR  punteroc AS ROWID.
 DEF VAR  punteroA AS ROWID. 
      ASSIGN punteroc = ROWID(clientes)      
             punteroA = ROWID(ahorros)
             age1 = ahorros.age_debAutomatico
             pro1 = ahorros.cod_Debautomatico
             cue1 = ahorros.cue_Debautomatico.                                                                        
    ASSIGN ValCol = "D5" Dato = Agencias.Nombre.
    RUN Llenar_Celda.
      ASSIGN ValCol = "P23" Dato   = "" + Clientes.Apellido1 + " " + Clientes.Apellido2 + " " + Clientes.nombre.
     RUN Llenar_Celda.
     ASSIGN ValCol = "B32" Dato = Usuario.Nombre.
     RUN Llenar_Celda.
     ASSIGN valcol = "U21" dato = STRING(ahorros.agencia) + "-" + STRING(ahorros.cod_ahorro) + "-" + STRING(ahorros.cue_ahorro). 
     RUN llenar_celda.

    ASSIGN ValCol = "B19" Dato = STRING (ahorros.age_debAutomatico) + "-" +
                    string(ahorros.cod_DebAutomatico) + "-" +  STRING(ahorros.cue_DebAutomatico).       
    RUN Llenar_Celda.


    FOR EACH  ahorros WHERE  Ahorros.agencia = age1 AND Ahorros.cod_ahorro = pro1
     AND ahorros.cue_ahorro =cue1:
        FIND FIRST clientes WHERE clientes.nit = ahorros.nit  NO-LOCK NO-ERROR.
      IF AVAILABLE(clientes) THEN DO:     
          ASSIGN ValCol = "B15" Dato   = "" + Clientes.Apellido1 + " " + Clientes.Apellido2 + " " + Clientes.nombre.
          RUN Llenar_Celda.
            ASSIGN ValCol = "AG15" Dato = ahorros.nit.
            RUN Llenar_Celda.                   
            FIND FIRST Ubicacion WHERE Ubicacion.Tipo = "C" AND Ubicacion.Ubicacion 
            BEGINS SUBSTR(Clientes.Lugar_Expedicion,1,5) NO-LOCK NO-ERROR.
            IF AVAIL Ubicacion THEN 
                            ASSIGN ValCol = "E17" Dato = Ubicacion.Nombre.
             ELSE ASSIGN ValCol = "E17" Dato = "no encontrado".
             RUN Llenar_Celda.
      END.
      ELSE  DO:
                ASSIGN ValCol = "B15"  Dato = "Cedula NO encontrada".
           RUN Llenar_Celda.
      END.
        END.
       FOR EACH Empresas WHERE Empresas.Cod_Empresa = Clientes.Cod_Empresa NO-LOCK:
           FIND FIRST clientes WHERE clientes.nit = Empresas.nit NO-LOCK NO-ERROR.
           IF AVAIL(clientes) THEN 
                     ASSIGN ValCol = "k25"  Dato = clientes.nombre.
           ELSE
         ASSIGN ValCol = "k25"  Dato = "Empresa NO encontrada".
       RUN Llenar_Celda.
       END. 
      
         FIND clientes WHERE ROWID(clientes) EQ Punteroc NO-LOCK NO-ERROR.
         FIND Ahorros WHERE ROWID(ahorros) EQ PunteroA NO-LOCK NO-ERROR.
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprime_Formato_Apertura wWin 
PROCEDURE Imprime_Formato_Apertura :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN ValCol = "J4"  Dato = Agencias.Nombre.
    RUN Llenar_Celda.
    ASSIGN ValCol = "V2"  Dato = Clientes.Nit.
    RUN Llenar_Celda.
    ASSIGN ValCol = "S4" Dato = Clientes.Tipo_Identificacion.
    RUN Llenar_Celda.
    ASSIGN ValCol = "A8"  Dato = Clientes.Apellido1 + " " + Clientes.Apellido2 + " " + Clientes.nombre.
    RUN Llenar_Celda.
    ASSIGN ValCol = "P8"  Dato = STRING(Ahorros.Fec_Apertura,"99/99/9999").
    RUN Llenar_Celda.
    ASSIGN ValCol = "W8 " Dato = STRING(Ahorros.agencia) + " - " + STRING(Ahorros.Cod_Ahorro) + " - " + Ahorros.Cue_Ahorro.
    RUN Llenar_Celda.
    FIND FIRST Relaciones WHERE Relaciones.Nit             = Ahorros.Nit
                            AND Relaciones.Cod_Relacion    = 7
                            AND Relaciones.Clase_Producto  = 1
                            AND Relaciones.Cod_Producto    = Ahorros.Cod_Ahorro
                            AND Relaciones.Estado          = 1
                            AND Relaciones.Cuenta          = Ahorros.Cue_Ahorros NO-LOCK NO-ERROR.
    IF AVAIL Relaciones THEN DO:
       FIND Clientes WHERE Clientes.Nit = Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
       ASSIGN ValCol = "A12" Dato = Clientes.Apellido1 + " " + Clientes.Apellido2 + " " + Clientes.nombre.
       RUN Llenar_Celda.
       ASSIGN ValCol = "S12" Dato = Clientes.Tipo_Identificacion.
       RUN Llenar_Celda.
       ASSIGN ValCol = "U12" Dato = Clientes.Nit.
       RUN Llenar_Celda.
       FIND NEXT Relaciones WHERE Relaciones.Nit             = Ahorros.Nit
                              AND Relaciones.Cod_Relacion    = 7
                              AND Relaciones.Clase_Producto  = 1
                              AND Relaciones.Cod_Producto    = Ahorros.Cod_Ahorro
                              AND Relaciones.Estado          = 1
                              AND Relaciones.Cuenta          = Ahorros.Cue_Ahorros NO-LOCK NO-ERROR.
       IF AVAIL Relaciones THEN DO:
          FIND Clientes WHERE Clientes.Nit = Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
          ASSIGN ValCol = "A13" Dato = Clientes.Apellido1 + " " + Clientes.Apellido2 + " " + Clientes.nombre.
          RUN Llenar_Celda.
          ASSIGN ValCol = "S13" Dato = Clientes.Tipo_Identificacion.
          RUN Llenar_Celda.
          ASSIGN ValCol = "U13" Dato = Clientes.Nit.
          RUN Llenar_Celda.
       END.
    END.
    ASSIGN ValCol = "B18"  Dato = Ahorros.Trf_Notas.
    RUN Llenar_Celda.
    ASSIGN ValCol = "Y29"  Dato = Usuarios.Nombre.
    RUN Llenar_Celda.
    FIND Clientes WHERE Clientes.nit = Ahorros.Nit NO-LOCK NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprime_Formato_CDAT wWin 
PROCEDURE Imprime_Formato_CDAT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR W_MonEs AS CHA NO-UNDO.
    ASSIGN ValCol = "A3"   Dato = Agencias.Nombre.
    RUN Llenar_Celda.
    ASSIGN ValCol = "K3"   Dato = STRING(Ahorros.Fec_Apertura,"99/99/9999").
    RUN Llenar_Celda.
    ASSIGN ValCol = "U3"   Dato = STRING(Ahorros.Fec_Vencimiento,"99/99/9999").
    RUN Llenar_Celda.
    ASSIGN ValCol = "AE3"  Dato = string(ahorros.agencia) + "-" +  string(ahorros.cod_ahorro)
                            + "-" + Ahorros.Cue_Ahorro.
    RUN Llenar_Celda.
    ASSIGN ValCol = "A7"   Dato = Clientes.Apellido1 + " " + Clientes.Apellido2 + " " + Clientes.nombre.
    RUN Llenar_Celda.
    ASSIGN ValCol = "U7"   Dato = Clientes.Nit.
    RUN Llenar_Celda.
    ASSIGN ValCol = "B11"  Dato = STRING(Ahorros.Monto_Apertura,"->>>,>>>,>>>.99").
    RUN Llenar_Celda.
    RUN MontoEsc.r (Ahorros.Monto_Apertura,INPUT 0,OUTPUT W_MonEs).
    ASSIGN ValCol = "J11"  Dato = "SON " + CAPS(W_MonEs).
    RUN Llenar_Celda.
    ASSIGN ValCol = "A14"  Dato = STRING(Ahorros.Plazo).
    RUN Llenar_Celda.
    ASSIGN ValCol = "G14"  Dato = SUBSTRING(cmb_Perliquidacion:SCREEN-VALUE IN FRAME F_Ahorros,6,12).
    RUN Llenar_Celda.
    ASSIGN ValCol = "M14"  Dato = STRING(Ahorros.Tasa:SCREEN-VALUE IN FRAME F_Ahorros,"99999").
    RUN Llenar_Celda.
    ASSIGN ValCol = "S14"  Dato = STRING(Tasa_Efectiva:SCREEN-VALUE IN FRAME F_Ahorros,"99999").
    RUN Llenar_Celda.
    ASSIGN ValCol = "Y14"  Dato = "Vencido".
    IF Ahorros.For_Liquidacion  = 1 THEN Dato = "Anticipado".
    RUN Llenar_Celda.
    ASSIGN ValCol = "AE14" Dato =  string(ahorros.agencia_destino) + "-" + string(ahorros.pro_destino) + "-" + Ahorros.Cue_Destino.
    RUN Llenar_Celda.
    ASSIGN ValCol = "AJ26" Dato = Usuarios.Nombre.
    RUN Llenar_Celda.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprime_Formato_GMF wWin 
PROCEDURE Imprime_Formato_GMF :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME  f_ahorros.
    ASSIGN ValCol = "D5" Dato = Agencias.Nombre.
    RUN Llenar_Celda.
    ASSIGN ValCol = "C12"  Dato = Clientes.Apellido1 + " " + Clientes.Apellido2 + " " + Clientes.nombre.
    RUN Llenar_Celda.
    ASSIGN ValCol = "C13" Dato = Clientes.Nit.
    RUN Llenar_Celda.
    ASSIGN ValCol = "R14" Dato = STRING(Pro_Ahorros.VrTope_ExentoEE,"->>>,>>>,>>>.99").
    RUN Llenar_Celda.
    ASSIGN ValCol = "X15" Dato = string(ahorros.agencia) + "-" + string(ahorros.cod_ahorro) + 
                                "-" + Ahorros.Cue_Ahorro.
    RUN Llenar_Celda.
    ASSIGN ValCol = "P16" Dato = Agencias.Nombre.
    RUN Llenar_Celda.
    ASSIGN ValCol = "Q21" Dato = STRING(Fecha_GMF[1],"99/99/9999").
    RUN Llenar_Celda.
    ASSIGN ValCol = "Q22" Dato = STRING(Fecha_GMF[2],"99/99/9999").
    RUN Llenar_Celda.
    ASSIGN ValCol = "AB49" Dato = Usuarios.Nombre.
    RUN Llenar_Celda.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprime_Formato_UtraMas wWin 
PROCEDURE Imprime_Formato_UtraMas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR  DATOF AS CHAR.
    DEF VAR tasae AS DECIMAL.

    
    ASSIGN ValCol = "AE11"  Dato = string(DAY(Ahorros.Fec_Vencimiento)) + "/" + STRING (month(Ahorros.Fec_Vencimiento)) + 
                                       "/" + STRING( YEAR(Ahorros.Fec_Vencimiento)).
    RUN Llenar_Celda.
    ASSIGN ValCol = "X11"  Dato =  string(DAY(Ahorros.Fec_apertura)) + "/" + STRING (month(Ahorros.Fec_apertura)) + 
                                       "/" + STRING( YEAR(Ahorros.Fec_apertura)).
    RUN Llenar_Celda.
    ASSIGN ValCol = "X69"  .
    RUN Llenar_Celda.


    ASSIGN ValCol = "k7"   Dato = Clientes.Apellido1 + " " + Clientes.Apellido2 + " " + Clientes.nombre.
    RUN Llenar_Celda.
    ASSIGN ValCol = "AD7"  Dato = Clientes.Nit.
    RUN Llenar_Celda.
    ASSIGN ValCol = "A7"  Dato = Agencias.Nombre.
    RUN Llenar_Celda.
    ASSIGN ValCol = "G7"  Dato = Ahorros.Cue_Ahorro.
    RUN Llenar_Celda.
    ASSIGN ValCol = "A11"  Dato = STRING(Ahorros.Cuota,"->>>,>>>,>>>.99").
    RUN Llenar_Celda.
    ASSIGN ValCol = "F11"  Dato = STRING(ROUND(Ahorros.plazo / 30,0) ,"->>>,>>>,>>>.99").
    RUN Llenar_Celda.
    ASSIGN ValCol = "J11"  Dato = STRING(Ahorros.cuota *  ROUND((ahorros.plazo / 30),0) ,"->>>,>>>,>>>.99").
    RUN Llenar_Celda. 
    ASSIGN ValCol = "R11"  Dato = STRING(Ahorros.Tasa:SCREEN-VALUE IN FRAME F_Ahorros).
    RUN Llenar_Celda.

    RUN NVEF IN W_ManFin (INPUT ((DECIMAL(ahorros.Tasa:SCREEN-VALUE) / 100) / 12),12, OUTPUT Tasae).
          ASSIGN ValCol ="U11" dato = SUBSTRING(STRING((tasae * 100),"99.999"),1,5).
          RUN Llenar_Celda.
/*
    ASSIGN ValCol = "U11" Dato = STRING(Tasa_Efectiva:SCREEN-VALUE IN FRAME F_Ahorros).
    RUN Llenar_Celda.
    */

    ASSIGN ValCol = "AE69"  Dato = STRING(Ahorros.Fec_Vencimiento).
    RUN Llenar_Celda.


    ASSIGN ValCol = "AH25" Dato = Usuarios.Nombre.
    RUN Llenar_Celda.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE imprime_Utravivienda wWin 
PROCEDURE imprime_Utravivienda :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN ValCol = "A25"   Dato = Agencias.Nombre.
    RUN Llenar_Celda.
    ASSIGN ValCol = "Q25"   Dato = STRING(Ahorros.Fec_Apertura,"99/99/9999").
    RUN Llenar_Celda.
    ASSIGN ValCol = "H25"  Dato = STRING(ahorros.agencia) + string(ahorros.cod_ahorro) + Ahorros.Cue_Ahorro.
    RUN Llenar_Celda.
    ASSIGN ValCol = "B9"   Dato = Clientes.Apellido1 + " " + Clientes.Apellido2 + " " + Clientes.nombre.
    RUN Llenar_Celda.
    ASSIGN ValCol = "Y9"   Dato = Clientes.Nit.
    RUN Llenar_Celda.
    ASSIGN ValCol = "AE25"  Dato = STRING(Ahorros.Monto_Apertura,"->>>,>>>,>>>.99").
    RUN Llenar_Celda.
        
    ASSIGN ValCol = "A11"  Dato = STRING(clientes.dir_residencia).
    RUN Llenar_Celda.
    ASSIGN ValCol = "J11"  Dato = STRING(clientes.tel_residencia).
    RUN Llenar_Celda.

    FIND ubicacion WHERE  ubicacion.tipo = "D" AND substring(ubicacion.ubicacion,1,2) = substring(clientes.lugar_residencia,1,2) NO-LOCK NO-ERROR.
    IF AVAILABLE(ubicacion) THEN
          ASSIGN ValCol = "AD11"  Dato = ubicacion.nombre.
      ELSE
          ASSIGN ValCol = "AD11"  Dato = "no encontrado".
    RUN Llenar_Celda.

        FIND ubicacion WHERE  ubicacion.tipo = "C" AND substring(ubicacion.ubicacion,1,5) = substring(clientes.lugar_residencia,1,5) NO-LOCK NO-ERROR.
    IF AVAILABLE(ubicacion) THEN
          ASSIGN ValCol = "N11"  Dato = ubicacion.nombre.
      ELSE
          ASSIGN ValCol = "N11"  Dato = "no encontrado".
    RUN Llenar_Celda.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir_Cuenta wWin 
PROCEDURE Imprimir_Cuenta :
{Incluido\RepEncabezado.i}
  
  DEFINE VAR W_NomTipoCliente AS CHARACTER FORMAT "X(20)".
  DEFINE VAR W_NomEstado      AS CHARACTER FORMAT "X(10)".
  DEFINE VAR W_NomTipoContrat AS CHARACTER FORMAT "X(14)".
  DEFINE VAR W_NomVinculo     AS CHARACTER FORMAT "X(20)".
  DEFINE VAR W_NomSexo        AS CHARACTER FORMAT "X(6)".
  DEFINE VAR W_NomRet         AS CHARACTER FORMAT "X(2)".
  DEFINE VAR W_NomGran        AS CHARACTER FORMAT "X(2)".
  
  FIND Clientes WHERE Clientes.Nit EQ Ahorros.Nit:SCREEN-VALUE IN FRAME F_Ahorros NO-LOCK NO-ERROR.

  W_Reporte   = "REPORTE   : INFORMACION DE CUENTA - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
  W_EncColumna = "CLIENTE: " + Ahorros.Nit:SCREEN-VALUE IN FRAME F_Ahorros + " - " + 
                  Clientes.Nombre + " " +
                  Clientes.Apellido1 + " " +
                  Clientes.Apellido2.
     
    VIEW FRAME F-Encabezado.
    VIEW FRAME F-Ftr.

    DO WITH FRAME F_Ahorros:
       DEFINE VAR W_EstadoA AS CHARACTER FORMAT "X(10)".
       DEFINE VAR W_ForLiqA AS CHARACTER FORMAT "X(10)".
       IF Ahorros.Estado:SCREEN-VALUE EQ "1" THEN W_EstadoA = "Activa".
       ELSE W_EstadoA = "Inactiva".
       IF Ahorros.FOR_Liquidacion:SCREEN-VALUE EQ "1" THEN W_ForLiqA = "Anticipado".
       ELSE W_ForliqA = "Vencido".
       DISPLAY "INFORMACIÓN GENERAL--------------------------------------------------------------------------------" AT 1 SKIP(1)
                "Tipo de Producto     : "            AT 1
                Cmb_TipoProductos:SCREEN-VALUE       AT 25 FORMAT "X(20)"
                "Producto             : "            AT 50
                Cmb_Productos:SCREEN-VALUE           AT 75 FORMAT "X(40)"
                "Id. del Titular      : "            AT 1
                Ahorros.Nit:SCREEN-VALUE             AT 25 FORMAT "X(14)"
                "Nombre del Titular   : "            AT 50
                W_NomTitular:SCREEN-VALUE            AT 75 FORMAT "X(40)"
                "Número de la Cuenta  : "            AT 1
                Ahorros.Cue_Ahorros:SCREEN-VALUE     AT 25 
                "Número del Formato   : "            AT 50
                Ahorros.Num_Formato:SCREEN-VALUE     AT 75
                "Tasa Nomina Anual    : "            AT 1
                Tasa_Nominal:SCREEN-VALUE            AT 25
                "Tasa del Periodo     : "            AT 50
                Ahorros.Tasa:SCREEN-VALUE            AT 75
                "Interes del Periodo  : "            AT 1
                Interes_PorPagar:SCREEN-VALUE        AT 25
                "Periodo Liquidación  : "            AT 50
                Cmb_PerLiquidacion:SCREEN-VALUE      AT 75 FORMAT "X(20)"
                "Estado               : "            AT 1
                W_EstadoA                            AT 25
                "Forma de Liquidación : "            AT 50
                W_ForLiqA                            AT 75
                "Detalle del Estado   : "            AT 1
                Cmb_Estados:SCREEN-VALUE             AT 25 FORMAT "X(20)"  SKIP(1) 
           WITH FRAME AH WIDTH 132 NO-BOX USE-TEXT NO-LABELS STREAM-IO.
    END.
    CASE SUBSTRING(Cmb_TipoProductos:SCREEN-VALUE,1,1):
        WHEN "2" THEN DO:
          DEFINE VARIABLE W_PerPago AS CHARACTER FORMAT "X(20)".
          DEFINE VARIABLE W_ForPago AS CHARACTER FORMAT "X(20)".
          DEFINE VARIABLE W_CuePago AS CHARACTER FORMAT "X(50)".
          DO WITH FRAME F_Contractual:
              CASE CT_PeriodoPago:SCREEN-VALUE:
                  WHEN "1" THEN W_PerPago = "Semanal".
                  WHEN "2" THEN W_PerPago = "Decadal".
                  WHEN "3" THEN W_PerPago = "Quincenal".
                  WHEN "4" THEN W_PerPago = "Mensual".
              END CASE.
              CASE CT_FormaPago:SCREEN-VALUE:
                  WHEN "1" THEN W_ForPago = "Caja".
                  WHEN "2" THEN W_ForPago = "Nomina".
                  WHEN "3" THEN ASSIGN W_ForPago = "Debito Automático"
                                       W_CuePago = CT_AgenciaDebito + " - " + CT_ProductoDebito + " - Cuenta: " + CT_CuentaDebito.
              END CASE.
              DISPLAY "ESPECÍFICA DEL PRODUCTO----------------------------------------------------------------------------" AT 1 SKIP(1)
                      "Monto de Apertura    : "            AT 1
                      CT_Monto:SCREEN-VALUE                AT 25
                      "Cuota                : "            AT 50
                      CT_Cuota:SCREEN-VALUE                AT 75 FORMAT "X(40)"
                      "Tasa Efectiva Anual  : "            AT 1
                      Tasa_Efectiva:SCREEN-VALUE IN FRAME F_Ahorros AT 25
                      "Plazo en Días        : "            AT 50
                      CT_Plazo:SCREEN-VALUE                AT 75
                      "Periodicidad Cuota   : "            AT 1
                      W_PerPago                            AT 25
                      "Forma Pago Cuota     : "            AT 1
                      W_ForPago                            AT 25
                      "Cuenta Debito        : "            AT 1
                      W_CuePago                            AT 25  SKIP(1)
              WITH FRAME CT WIDTH 132 NO-BOX USE-TEXT NO-LABELS STREAM-IO.
          END.
        END.
        WHEN "3" THEN DO:
          DO WITH FRAME F_ATermino:
              DEFINE VARIABLE W_Destino AS CHARACTER FORMAT "X(20)".
              DEFINE VARIABLE W_CueDest AS CHARACTER FORMAT "X(50)".
              CASE AT_Destino_Intereses:SCREEN-VALUE:
                  WHEN "1" THEN ASSIGN W_Destino = "Ahorros"
                                       W_CueDest = AT_AgenciaDestino + " - " + AT_ProductoDestino + " - Cuenta: " + AT_CuentaDestino.
                  WHEN "2" THEN ASSIGN W_Destino = "Creditos"
                                       W_CueDest = AT_AgenciaDestino + " - " + AT_ProductoDestino + " - Cuenta: " + AT_CuentaDestino.
                  WHEN "3" THEN ASSIGN W_Destino = "Cta X Pagar".
              END CASE.
              DISPLAY "ESPECÍFICA DEL PRODUCTO----------------------------------------------------------------------------" AT 1 SKIP(1)
                      "Monto de Apertura    : "            AT 1
                      AT_Monto:SCREEN-VALUE                AT 25
                      "Tasa Efectiva Anual  : "            AT 50
                      Tasa_Efectiva:SCREEN-VALUE IN FRAME F_Ahorros                AT 75
                      "Destino Intereses     : "           AT 1
                      W_Destino                            AT 25
                      "Cuenta Destino        : "           AT 1
                      W_CueDest                            AT 25 SKIP(1)
              WITH FRAME ATe WIDTH 132 NO-BOX USE-TEXT NO-LABELS STREAM-IO.
          END.
        END.
        OTHERWISE DO:
          DO WITH FRAME F_AlaVista:
              DISPLAY "ESPECÍFICA DEL PRODUCTO----------------------------------------------------------------------------" AT 1 SKIP(1)
                      "Monto de Apertura    : "            AT 1
                      AV_Monto:SCREEN-VALUE                AT 25 FORMAT "X(20)" 
                      "Cuota                : "            AT 50
                      AV_Cuota:SCREEN-VALUE                AT 75 FORMAT "X(40)"
                      "Tasa Efectiva Anual  : "            AT 1
                      Tasa_Efectiva:SCREEN-VALUE IN FRAME F_Ahorros                AT 25 FORMAT "X(20)"   SKIP(1)
              WITH FRAME AV WIDTH 132 NO-BOX USE-TEXT NO-LABELS STREAM-IO.
          END.
        END.
    END CASE.
    DO WITH FRAME F_Secundaria:
        DISPLAY "INFORMACIÓN SECUNDARIA-----------------------------------------------------------------------------" AT 1 SKIP(1)
                "Fecha de Apertura    : "               AT 1
                Ahorros.Fec_Apertura:SCREEN-VALUE       AT 25 FORMAT "X(20)"
                "Fecha de Cancelación : "               AT 50
                Ahorros.Fec_Cancelacion:SCREEN-VALUE    AT 75 FORMAT "X(20)" 
                "Proxima Liquidación  : "               AT 1
                Ahorros.Fec_ProLiquidacion:SCREEN-VALUE AT 25 FORMAT "X(20)" 
                "Ultima Liquidacion   : "               AT 50
                Ahorros.Fec_UltLiquidacion:SCREEN-VALUE AT 75 FORMAT "X(20)" 
                "Ultima Transacción   : "               AT 1
                Ahorros.Fec_UltTransaccion:SCREEN-VALUE AT 25 FORMAT "X(20)" 
                "Fecha de Vencimiento : "               AT 50
                Ahorros.Fec_Vencimiento:SCREEN-VALUE    AT 75 FORMAT "X(20)" 
                "Saldo Disponible     : "               AT 1
                Ahorros.Sdo_Disponible:SCREEN-VALUE     AT 25 FORMAT "X(20)" 
                "Saldo Ult.Liquidacion: "               AT 50
                Ahorros.Sdo_UltLiquidacion:SCREEN-VALUE AT 75 FORMAT "X(20)" 
                "Saldo en Canje       : "               AT 1 
                Ahorros.Sdo_Canje:SCREEN-VALUE          AT 25 FORMAT "X(20)" 
                "Usuario              : "               AT 1
                Ahorros.Usu_Creacion:SCREEN-VALUE       AT 25
                W_NomUsuario:SCREEN-VALUE               AT 32 FORMAT "X(50)" 
        WITH FRAME SC WIDTH 132 NO-BOX USE-TEXT NO-LABELS STREAM-IO.
    END.

    PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir_Relaciones wWin 
PROCEDURE Imprimir_Relaciones :
{Incluido\RepEncabezado.i}
  
  FIND Clientes WHERE Clientes.Nit EQ Ahorros.Nit:SCREEN-VALUE IN FRAME F_Ahorros NO-LOCK NO-ERROR.

  W_Reporte   = "REPORTE   : RELACIONES :" + STRING(R_Eleccion) + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
  W_EncColumna = "CLIENTE: " + Ahorros.Nit:SCREEN-VALUE  + " - " + 
                  Clientes.Nombre + " " +
                  Clientes.Apellido1 + " " +
                  Clientes.Apellido2.
     
    W_Linea = FILL(W_Raya,132).
    VIEW FRAME F-Encabezado.
    VIEW FRAME F-Ftr.
    DISPLAY "NOMBRE RELACION        AGE NIT            NOMBRE                             DESCRIPCION     TELEFONOS" SKIP(1) WITH WIDTH 132.
    FOR EACH T_Relaciones NO-LOCK:
      DISPLAY T_Relaciones.R_Relacion  FORMAT "X(22)"
              T_Relaciones.R_AgeObjeto FORMAT "999"  
              T_Relaciones.R_NitObjeto FORMAT "X(14)"
              T_Relaciones.R_NomObjeto FORMAT "X(35)"
              T_Relaciones.R_NomDescri FORMAT "X(15)"
              T_Relaciones.R_TelObjeto FORMAT "X(30)"
    WITH WIDTH 132 FRAME F-Relaciones NO-BOX USE-TEXT STREAM-IO NO-LABELS.
    END.  
    PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informacion_Secundaria wWin 
PROCEDURE Informacion_Secundaria :
DO WITH FRAME F_Secundaria:
  IF AVAILABLE Ahorros THEN DO:
    DISPLAY Ahorros.Fec_Apertura Ahorros.Fec_Cancelacion Ahorros.Fec_ProLiquidacion Ahorros.Fec_Prorroga
            Ahorros.Fec_UltLiquidacion Ahorros.Fec_UltTransaccion Ahorros.Fec_Vencimiento
            Ahorros.Sdo_Disponible Ahorros.Sdo_UltLiquidacion Ahorros.Sdo_Canje
            Ahorros.Usu_Creacion Ahorros.Num_DepDia Ahorros.Num_DepMes Ahorros.Num_RetDia
            Ahorros.Num_RetMes Ahorros.Val_RetDia Ahorros.Val_RetMes Ahorros.Val_DepDia
            Ahorros.Val_DepMes Ahorros.Val_RetDiaCheq Ahorros.Num_RetDiaCheq WITH FRAME F_Secundaria.
    FIND Usuarios WHERE Usuarios.Usuario EQ Ahorros.Usu_Creacion NO-LOCK NO-ERROR.
    IF AVAILABLE Usuarios THEN DO:
       W_NomUsuario:SCREEN-VALUE = Usuarios.Nombre.
    END.
  END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar_Variables wWin 
PROCEDURE Inicializar_Variables :
DO WITH FRAME F_Ahorros:
     ASSIGN Ahorros.Nit:SCREEN-VALUE                 = " "  Ahorros.Cue_Ahorros:SCREEN-VALUE         = " "
            Tasa_Efectiva:SCREEN-VALUE               = "0"  Ahorros.Num_formato:SCREEN-VALUE         = " "
            Ahorros.Sdo_MinCta:SCREEN-VALUE          = "0"  Tasa_Nominal:SCREEN-VALUE                = "0"
            Ahorros.Tasa:SCREEN-VALUE                = "0"  Interes_PorPagar:SCREEN-VALUE            = "0"
            Ahorros.Estado:SCREEN-VALUE              = "1"  Ahorros.Ind_Tipo_Subsidio:SCREEN-VALUE   = "1"
            Ahorros.FOR_Liquidacion:SCREEN-VALUE     = "2"  W_Nomtitular:SCREEN-VALUE                = " "
            Ahorros.Exento_3xM:SCREEN-VALUE          = "No" Ahorros.Id_Tesoro_Nacional:SCREEN-VALUE  = "No"
            Ahorros.Id_Subsidio_Entidad:SCREEN-VALUE = "No" Ahorros.Id_Mesada_Pensional:SCREEN-VALUE = "No"
            Tgl-TarjetaDB:SCREEN-VALUE = "No"               ahorros.fec_activacion[1]:SCREEN-VALUE = ""
           ahorros.fec_activacion[2]:SCREEN-VALUE = ""     ahorros.fec_desactivacion[1]:SCREEN-VALUE = ""
         ahorros.fec_desactivacion[2]:SCREEN-VALUE = ""    ahorros.fec_activacion[3]:SCREEN-VALUE = ""
         ahorros.fec_desactivacion[3]:SCREEN-VALUE = ""    ahorros.fec_activacion[4]:SCREEN-VALUE = ""
         ahorros.fec_desactivacion[4]:SCREEN-VALUE = "".

          /*falta combo descripcion y perliquidacion*/
  END.
  DO WITH FRAME F_AlaVista:
     ASSIGN AV_Monto:SCREEN-VALUE          = "0" AV_Cuota:SCREEN-VALUE           = "0"
            AP_FormaPago:SCREEN-VALUE      = "1" AP_AgenciaDebito:SCREEN-VALUE   = " "
            AP_ProductoDebito:SCREEN-VALUE = " " AP_CuentaDebito:SCREEN-VALUE    = " "
            AP_NombreDebito:SCREEN-VALUE   = " " AP_FechaDebito:SCREEN-VALUE     = "?"
            AP_PeriodoPago:SCREEN-VALUE    = "4".
     DISABLE BT_DebitoAP AP_FechaDebito WITH FRAME F_AlaVista.
  END.
  DO WITH FRAME F_Contractual:
     ASSIGN CT_Monto:SCREEN-VALUE          = "0" CT_Cuota:SCREEN-VALUE           = "0"
            CT_Puntos:SCREEN-VALUE         = "0" CT_Plazo:SCREEN-VALUE           = "0"
            CT_PeriodoPago:SCREEN-VALUE    = "4" CT_FormaPago:SCREEN-VALUE       = "1"
            CT_AgenciaDebito:SCREEN-VALUE  = " " CT_ProductoDebito:SCREEN-VALUE  = " "
            CT_CuentaDebito:SCREEN-VALUE   = " " CT_NombreDebito:SCREEN-VALUE    = " "
            CT_FechaDebito:SCREEN-VALUE    = "?" porce_aporte:SCREEN-VALUE       = "0"
            F-AporOblig:SCREEN-VALUE       = "0" F-Salario:SCREEN-VALUE          = "0"
            F-PorApoSue:SCREEN-VALUE       = "0" vlTpAct                         = FALSE.
  END.
  DO WITH FRAME F_Atermino:
     ASSIGN AT_Monto:SCREEN-VALUE          = "0" AT_Puntos:SCREEN-VALUE          = "0"
            AT_Plazo:SCREEN-VALUE          = "0" AT_AgenciaDestino:SCREEN-VALUE  = " "
            AT_CuentaDestino:SCREEN-VALUE  = " " AT_ProductoDestino:SCREEN-VALUE = " "
            AT_NombreDestino:SCREEN-VALUE  = " ".
  END.
  DO WITH FRAME F_Libretas:
     ASSIGN Lib_Chequera.Num_Inicial:SCREEN-VALUE = "00000"
            Lib_Chequera.Num_Final:SCREEN-VALUE   = "00000"
            Lib_Chequera.Fec_Entrega:SCREEN-VALUE = "?".
  END.
  DO WITH FRAME F_Secundaria:
     ASSIGN Ahorros.Num_DepDia:SCREEN-VALUE     = "0" Ahorros.Num_DepMes:SCREEN-VALUE     = "0"
            Ahorros.Num_RetDia:SCREEN-VALUE     = "0" Ahorros.Num_RetMes:SCREEN-VALUE     = "0"
            Ahorros.Val_RetDia:SCREEN-VALUE     = "0" Ahorros.Val_RetMes:SCREEN-VALUE     = "0"
            Ahorros.Val_DepDia:SCREEN-VALUE     = "0" Ahorros.Val_DepMes:SCREEN-VALUE     = "0"
            Ahorros.Val_RetDiaCheq:SCREEN-VALUE = "0" Ahorros.Num_RetDiaCheq:SCREEN-VALUE = "0". 
  END.
   DO WITH FRAME F_Relaciones:
       ASSIGN W-DescTRF:SCREEN-VALUE  = " "           Cmb_TRF:SCREEN-VALUE  = ""
              R_Relacion2:SCREEN-VALUE = "".
   END.
    DISABLE Btn_Salvar.
    Ahorros.Nit:SCREEN-VALUE = " ".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializa_Libretas wWin 
PROCEDURE Inicializa_Libretas :
DO WITH FRAME F_Libretas:

     ASSIGN Lib_Chequera.Num_Inicial:SCREEN-VALUE = "00000"
            Lib_Chequera.Num_Final:SCREEN-VALUE   = "00000"
            Lib_Chequera.Fec_Entrega:SCREEN-VALUE = STRING (W_Fecha).
            
    APPLY 'choose' TO Btn_CanLib.
    
    OPEN QUERY BR_Libretas FOR EACH Lib_Chequera WHERE
     Lib_Chequera.Agencia       EQ Ahorros.Agencia AND
     Lib_Chequera.Cod_Producto  EQ INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Ahorros,1,3)) AND
     Lib_Chequera.Cue_Ahorros   EQ Ahorros.Cue_Ahorros:SCREEN-VALUE IN FRAME F_Ahorros NO-LOCK.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
RUN SUPER.
  DO WITH FRAME F_Ahorros:
      FIND FIRST Ahorros WHERE Ahorros.Agencia EQ 0 NO-LOCK NO-ERROR.
      IF AVAILABLE(Ahorros) THEN DO:
         FIND FIRST Pro_Ahorros WHERE 
                    Pro_Ahorros.Cod_Ahorro EQ Ahorros.Cod_Ahorro NO-LOCK NO-ERROR.
         IF AVAILABLE Pro_Ahorros THEN 
            ASSIGN TPdt = Pro_Ahorros.Tip_Ahorro.
      END.
      FOR EACH Varios WHERE Varios.Tipo EQ 21 NO-LOCK:
        W_Ok = Cmb_Estados:ADD-LAST(STRING(Varios.Codigo,"99") + " - " + Varios.Descripcion).
      END.
      W_Ok = Cmb_Estados:ADD-LAST("99 - Cta.No Creada").
      Cmb_Estados:SCREEN-VALUE = "99 - Cta.No Creada".
      FOR EACH Pro_Ahorros WHERE Pro_Ahorros.Tip_Ahorro EQ Tpdt NO-LOCK:
        IF pro_Ahorros.cod_Ahorro NE 10 THEN DO:  /* personalizado crearcoop */
           W_Ok = Cmb_Productos:ADD-LAST(STRING(Pro_Ahorros.Cod_Ahorro,"999") + " - " + Pro_Ahorros.Nom_Producto).
           IF AVAILABLE(Ahorros) AND Ahorros.Agencia EQ W_Agencia AND Ahorros.Cod_Ahorro EQ Pro_Ahorros.Cod_Ahorro THEN
           DO:
             PunteroP = ROWID(Pro_Ahorros).
             ASSIGN Cmb_Productos:SCREEN-VALUE = STRING(Pro_Ahorros.Cod_Ahorro,"999") + " - " + Pro_Ahorros.Nom_Producto
                    Cmb_TipoProductos:SCREEN-VALUE = Cmb_TipoProductos:ENTRY(INTEGER(Pro_Ahorros.Tip_Ahorro)).
             RUN Mostrar_Registro.
            END.
           ELSE RUN Inicializar_Variables.
        END.
      END.
      ASSIGN Imp_Cdat:SENSITIVE IN FRAME F_Ahorros = NO.
      VIEW FRAME F_Contractual.
  END.
  DO WITH FRAME F_Contractual:
      /*GCamacho Abril 12/08 Ahorro Permanente*/
      ASSIGN 
          Ahorros.porce_Aporte:VISIBLE IN FRAME F_Contractual   = NO
          F-PorApoSue:VISIBLE IN FRAME F_Contractual   = NO
          F-AporOblig:VISIBLE IN FRAME F_Contractual   = NO
          F-Salario:VISIBLE IN FRAME F_Contractual   = NO
          Ahorros.porce_Aporte:SENSITIVE IN FRAME F_Contractual = NO.
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Llenar_Celda wWin 
PROCEDURE Llenar_Celda :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    chWorkSheet:Range(ValCol):Value = Dato.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Llenar_CmbProductos wWin 
PROCEDURE Llenar_CmbProductos :
DO WITH FRAME F_Ahorros:
   Cmb_Productos:LIST-ITEMS = "".         
   FOR EACH Pro_Ahorros NO-LOCK WHERE
            Pro_Ahorros.Tip_Ahorro = INT(SUBSTR(Cmb_TipoProductos:SCREEN-VALUE,1,1))
        AND Pro_Ahorros.Estado = 1 BREAK BY Pro_Ahorros.Tip_Ahorro:
        W_Ok = Cmb_Productos:ADD-LAST(STRING(Pro_Ahorros.Cod_Ahorro,"999") + " - " + Pro_Ahorros.Nom_Producto).
        IF AVAIL(Ahorros) AND Pro_Ahorros.Cod_Ahorro EQ Ahorros.Cod_Ahorro THEN DO:            
           Cmb_Productos:SCREEN-VALUE = STRING(Pro_Ahorros.Cod_Ahorro,"999") + " - " + Pro_Ahorros.Nom_Producto.           
           ASSIGN Wk_MontoApertura = 0
                  Wk_IdMonApe      = NO
                  Wk_Cuota         = 0.
           IF Pro_Ahorros.Id_MonApertura THEN
              ASSIGN Wk_IdMonApe = Pro_Ahorros.Id_MonApertura
                     Wk_MontoApertura = Pro_Ahorros.Val_MonAper.
           IF Pro_Ahorros.Id_Cuota THEN Wk_Cuota = Pro_Ahorros.Val_Cuota.
           PunteroP = ROWID(Pro_Ahorros).
        END.
        ELSE
        IF NOT AVAIL(Ahorros) AND FIRST-OF(Pro_Ahorros.Tip_Ahorro) AND NOT Ok_Cancelar THEN DO:
           Cmb_Productos:SCREEN-VALUE = STRING(Pro_Ahorros.Cod_Ahorro,"999") + " - " + Pro_Ahorros.Nom_Producto.
           ASSIGN Wk_MontoApertura = 0
                  Wk_IdMonApe      = NO
                  Wk_Cuota         = 0.
           IF Pro_Ahorros.Id_MonApertura THEN
              ASSIGN Wk_IdMonApe = Pro_Ahorros.Id_MonApertura
                     Wk_MontoApertura = Pro_Ahorros.Val_MonAper.
           IF Pro_Ahorros.Id_Cuota THEN Wk_Cuota = Pro_Ahorros.Val_Cuota.
           PunteroP = ROWID(Pro_Ahorros).
        END.
   END.   
  W_Ok = Cmb_Productos:ADD-FIRST("000" + " - " + "Seleccione el Producto").
  Cmb_Productos:SCREEN-VALUE = ("000" + " - " + "Seleccione el Producto").
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_AlaVista wWin 
PROCEDURE Mostrar_AlaVista :
/* Muestra los campos de la cuenta de A la Vista segun producto de Ahorros */
  DO WITH FRAME F_AlaVista:
      DISABLE ALL EXCEPT AV_VerTasa AP_FormaPago WITH FRAME F_AlaVista.
      ASSIGN AV_Monto:SCREEN-VALUE = STRING(Ahorros.Monto_Apertura)
             AV_Cuota:SCREEN-VALUE = STRING(Ahorros.Cuota)
             AP_FormaPago:SCREEN-VALUE   = STRING(Ahorros.For_Pago)
             AP_PeriodoPago:SCREEN-VALUE = STRING(Ahorros.Per_Deduc)
             AP_FechaDebito:SCREEN-VALUE = STRING(Ahorros.Fec_ProxDeb).
      IF bdcentral.Ahorros.Detalle_Estado = 1 THEN DO:
         ENABLE AV_Monto WITH FRAME F_AlaVista.
      END.
      IF Ahorros.FOR_Pago EQ 3 THEN DO:
          FIND Agencias WHERE Agencias.Agencia EQ Ahorros.Age_DebAutomatico AND Agencias.Estado EQ 1 NO-LOCK NO-ERROR.
          IF AVAILABLE(Agencias) THEN
             AP_AgenciaDebito:SCREEN-VALUE = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.
          PunteroP = ROWID(Pro_Ahorros).
          FIND Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ Ahorros.Cod_DebAutomatico AND
                                 Pro_Ahorros.Estado       EQ 1 NO-LOCK NO-ERROR.
          IF AVAILABLE(Pro_Ahorros) THEN
             AP_ProductoDebito:SCREEN-VALUE = STRING(Pro_Ahorros.Cod_Ahorro,"999") + " - " + Pro_Ahorros.Nom_Producto.
          ASSIGN W_Age = Ahorros.Age_DebAutomatico
                 W_Pro = Ahorros.Cod_DebAutomatico
                 W_Cue = Ahorros.Cue_DebAutomatico.
          Puntero = ROWID(Ahorros).
          FIND Ahorros WHERE Ahorros.Agencia     EQ W_Age AND
                             Ahorros.Cod_Ahorro  EQ W_Pro AND
                             Ahorros.Cue_Ahorros EQ W_Cue NO-LOCK NO-ERROR.
          IF AVAILABLE Ahorros THEN DO:
              FIND Clientes WHERE Clientes.Nit EQ Ahorros.Nit AND Clientes.Estado EQ 1 NO-LOCK NO-ERROR.
              IF AVAILABLE(Clientes) THEN
                 AP_NombreDebito:SCREEN-VALUE = CAPS(Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2).
              AP_CuentaDebito:SCREEN-VALUE = W_Cue.
          END.

          FIND Pro_Ahorros WHERE ROWID(Pro_Ahorros) EQ PunteroP NO-LOCK NO-ERROR.
          FIND Ahorros WHERE ROWID(Ahorros) EQ Puntero NO-ERROR.
      END.
      ELSE DO:
         ASSIGN AP_AgenciaDebito:SCREEN-VALUE  = ""
                AP_ProductoDebito:SCREEN-VALUE = ""
                AP_CuentaDebito:SCREEN-VALUE   = ""
                AP_NombreDebito:SCREEN-VALUE   = ""
                AP_FechaDebito:SCREEN-VALUE    = "".
      END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Atermino wWin 
PROCEDURE Mostrar_Atermino :
DO WITH FRAME F_Atermino:
    DISABLE ALL EXCEPT AT_Monto WITH FRAME F_Atermino.

    IF AVAILABLE Ahorros THEN
        Puntero = ROWID(Ahorros).

    DISABLE ALL WITH FRAME F_Contractual.

    ASSIGN AT_Puntos:SCREEN-VALUE = ""
           AT_Monto:SCREEN-VALUE = STRING(Ahorros.Monto_Apertura)
           AT_Plazo:SCREEN-VALUE = STRING(Ahorros.Plazo)
           AT_Destino_Intereses:SCREEN-VALUE = STRING(Ahorros.Des_intereses)
           Tasa_Efectiva:SCREEN-VALUE IN FRAME F_Ahorros = STRING(ahorros.tasa)
           ahorros.tasa:SCREEN-VALUE IN FRAME f_ahorros = STRING(ahorros.tasa)
           ahorros.tasa:LABEL  = "Tasa Efectiva".

    IF Ahorros.Detalle_Estado = 1 THEN
        ENABLE AT_Monto WITH FRAME F_Atermino.
    
    IF Ahorros.Des_intereses EQ 1 THEN DO:
        FIND FIRST Agencias WHERE Agencias.Agencia EQ Ahorros.Agencia_Destino
                              AND Agencias.Estado EQ 1 NO-LOCK NO-ERROR.
        IF AVAILABLE(Agencias) THEN
            AT_AgenciaDestino:SCREEN-VALUE IN FRAME F_Atermino = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.

        FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ Ahorros.Pro_Destino
                                 AND Pro_Ahorros.Estado EQ 1 NO-LOCK NO-ERROR.
        IF AVAILABLE(Pro_Ahorros) THEN
            AT_ProductoDestino:SCREEN-VALUE IN FRAME F_Atermino = STRING(Pro_Ahorros.Cod_Ahorro,"999") + " - " + Pro_Ahorros.Nom_Producto.

        ASSIGN W_Age = Ahorros.Agencia_Destino
               W_Pro = Ahorros.Pro_Destino
               W_Cue = Ahorros.Cue_Destino.

        Puntero = ROWID(Ahorros).

        FIND FIRST Ahorros WHERE Ahorros.Agencia EQ W_Age
                             AND Ahorros.Cod_Ahorro EQ W_Pro
                             AND Ahorros.Cue_Ahorros EQ W_Cue NO-LOCK NO-ERROR.
        IF AVAILABLE Ahorros THEN DO:
            FIND FIRST Clientes WHERE Clientes.Nit EQ Ahorros.Nit
                                  AND Clientes.Estado EQ 1 NO-LOCK NO-ERROR.
            IF AVAILABLE(Clientes) THEN
                AT_NombreDestino:SCREEN-VALUE IN FRAME F_Atermino = CAPS(Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2).

            AT_CuentaDestino:SCREEN-VALUE IN FRAME F_Atermino = W_Cue.
        END.

        FIND FIRST Ahorros WHERE ROWID(Ahorros) EQ Puntero NO-ERROR.
    END.

    IF Ahorros.Des_Intereses EQ 2 THEN DO:
        FIND FIRST Agencias WHERE Agencias.Agencia EQ Ahorros.Agencia_Destino
                              AND Agencias.Estado EQ 1 NO-LOCK NO-ERROR.
        IF AVAILABLE(Agencias) THEN
            AT_AgenciaDestino:SCREEN-VALUE IN FRAME F_Atermino = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.

        FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ Ahorros.Pro_Destino
                                  AND Pro_Creditos.Estado EQ 1 NO-LOCK NO-ERROR.
        IF AVAILABLE(Pro_Creditos) THEN
            AT_ProductoDestino:SCREEN-VALUE IN FRAME F_Atermino = STRING(Pro_Creditos.Cod_Credito,"999") + " - " + Pro_Creditos.Nom_Producto.

        ASSIGN W_Age = Ahorros.Agencia_Destino
               W_Pro = Ahorros.Pro_Destino
               W_Cue = Ahorros.Cue_Destino.

        FIND FIRST Creditos WHERE Creditos.Agencia EQ W_Age
                              AND Creditos.Cod_Credito EQ W_Pro
                              AND Creditos.Pagare EQ W_Cue NO-LOCK NO-ERROR.
        IF AVAILABLE Creditos THEN DO:
            FIND FIRST Clientes WHERE Clientes.Nit EQ Creditos.Nit
                                  AND Clientes.Estado EQ 1 NO-LOCK NO-ERROR.
            IF AVAILABLE(Clientes) THEN
                AT_NombreDestino:SCREEN-VALUE IN FRAME F_Atermino = CAPS(Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2).

            AT_CuentaDestino:SCREEN-VALUE IN FRAME F_Atermino = W_Cue.
        END.
    END.

    IF Ahorros.Des_Intereses EQ 3 THEN
        ASSIGN AT_AgenciaDestino:SCREEN-VALUE = ""
               AT_ProductoDestino:SCREEN-VALUE = ""
               AT_CuentaDestino:SCREEN-VALUE = ""
               AT_NombreDestino:SCREEN-VALUE = "".

    IF Ahorros.Detalle_Estado EQ 01 THEN
        ENABLE AT_Monto AT_Plazo.
    ELSE
        DISABLE /*AT_Monto*/ AT_Plazo.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Contractual wWin 
PROCEDURE Mostrar_Contractual :
DO WITH FRAME F_Contractual:
      IF AVAILABLE Ahorros THEN Puntero = ROWID(Ahorros).
      DISABLE ALL WITH FRAME F_Contractual.
      ASSIGN CT_Monto:SCREEN-VALUE              = STRING(Ahorros.Monto_Apertura)
             CT_Cuota:SCREEN-VALUE              = STRING(Ahorros.Cuota)
             Ahorros.porce_aporte:SCREEN-VALUE  = STRING(Ahorros.porce_aporte)
             CT_PeriodoPago:SCREEN-VALUE        = STRING(Ahorros.Per_Deduccion)
             CT_FormaPago:SCREEN-VALUE          = STRING(Ahorros.For_Pago)
             CT_Plazo:SCREEN-VALUE              = STRING(Ahorros.Plazo)
             CT_FechaDebito:SCREEN-VALUE        = STRING(Ahorros.Fec_ProxDeb)  

            Tasa_Efectiva:SCREEN-VALUE IN FRAME F_Ahorros = STRING(ahorros.tasa)           
          ahorros.tasa:SCREEN-VALUE IN FRAME f_ahorros = STRING(ahorros.tasa)
           Tasa_Efectiva:LABEL                           = "Tasa Nominal Anual".
         CT_Cuota:SENSITIVE = TRUE.
      
         IF Ahorros.Detalle_estado NE 1 THEN DO:
         ASSIGN CT_Cuota:SENSITIVE = FALSE.
         ENABLE CT_Monto 
                porce_aporte
             WITH FRAME F_Contractual.
      END.
             
      RUN getAhorros(NuevaCta).

      IF Ahorros.FOR_Pago EQ 3 THEN DO:
          FIND Agencias WHERE Agencias.Agencia EQ Ahorros.Age_DebAutomatico AND Agencias.Estado EQ 1 NO-LOCK NO-ERROR.
          IF AVAILABLE(Agencias) THEN
             CT_AgenciaDebito:SCREEN-VALUE = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.
          PunteroP = ROWID(Pro_Ahorros).
          FIND Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ Ahorros.Cod_DebAutomatico AND
                                 Pro_Ahorros.Estado     EQ 1 NO-LOCK NO-ERROR.
          IF AVAILABLE(Pro_Ahorros) THEN
             CT_ProductoDebito:SCREEN-VALUE = STRING(Pro_Ahorros.Cod_Ahorro,"999") + " - " + Pro_Ahorros.Nom_Producto.
          ASSIGN W_Age = Ahorros.Age_DebAutomatico
                 W_Pro = Ahorros.Cod_DebAutomatico
                 W_Cue = Ahorros.Cue_DebAutomatico.
          Puntero = ROWID(Ahorros).
          FIND Ahorros WHERE Ahorros.Agencia     EQ W_Age AND
                             Ahorros.Cod_Ahorro  EQ W_Pro AND
                             Ahorros.Cue_Ahorros EQ W_Cue NO-LOCK NO-ERROR.
          IF AVAILABLE Ahorros THEN DO:
              FIND Clientes WHERE Clientes.Nit EQ Ahorros.Nit AND Clientes.Estado EQ 1 NO-LOCK NO-ERROR.
              IF AVAILABLE(Clientes) THEN
                 CT_NombreDebito:SCREEN-VALUE = CAPS(Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2).
              CT_CuentaDebito:SCREEN-VALUE = W_Cue.
          END.
          FIND Pro_Ahorros WHERE ROWID(Pro_Ahorros) EQ PunteroP NO-LOCK NO-ERROR.
          FIND Ahorros WHERE ROWID(Ahorros) EQ Puntero NO-LOCK NO-ERROR.
      END.
      ELSE DO:
         ASSIGN Ct_AgenciaDebito:SCREEN-VALUE  = ""
                Ct_ProductoDebito:SCREEN-VALUE = ""
                Ct_CuentaDebito:SCREEN-VALUE   = ""
                Ct_NombreDebito:SCREEN-VALUE   = ""
                Ct_FechaDebito:SCREEN-VALUE    = "".
      END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_General wWin 
PROCEDURE Mostrar_General :
FIND Pro_Ahorros WHERE 
     Pro_Ahorros.Tip_Ahorro EQ INT(SUBSTR(Cmb_TipoProductos:SCREEN-VALUE IN FRAME F_Ahorros,1,1)) AND 
     Pro_Ahorros.Cod_Ahorro EQ INT(SUBSTR(Cmb_Productos:SCREEN-VALUE IN FRAME F_Ahorros,1,3)) NO-LOCK NO-ERROR.
ASSIGN Ahorros.Fec_Activacion[1]:SCREEN-VALUE = " " Ahorros.Fec_DesActivacion[1]:SCREEN-VALUE = " "
       Ahorros.Fec_Activacion[2]:SCREEN-VALUE = " " Ahorros.Fec_DesActivacion[2]:SCREEN-VALUE = " "
       Ahorros.Fec_Activacion[3]:SCREEN-VALUE = " " Ahorros.Fec_DesActivacion[3]:SCREEN-VALUE = " "
       Ahorros.Fec_Activacion[4]:SCREEN-VALUE = " " Ahorros.Fec_DesActivacion[4]:SCREEN-VALUE = " ".
FOR EACH TAhorros: DELETE TAhorros. END.
DO WITH FRAME F_Ahorros:
   IF AVAIL Ahorros THEN DO:
      CREATE TAhorros.
      BUFFER-COPY Ahorros TO TAhorros.
      ASSIGN Ahorros.Cue_Ahorros:SCREEN-VALUE          =        Ahorros.Cue_Ahorros
             Ahorros.Id_Tutor:SCREEN-VALUE             =        Ahorros.Id_Tutor
             Ahorros.Nit_Tutor:SCREEN-VALUE            =        Ahorros.Nit_Tutor
             Ahorros.Num_Formato:SCREEN-VALUE          =        Ahorros.Num_Formato
             Ahorros.Nit:SCREEN-VALUE                  =        Ahorros.Nit
             Ahorros.Sdo_MinCta:SCREEN-VALUE           = STRING(Ahorros.Sdo_MinCta)
             Ahorros.Estado:SCREEN-VALUE               = STRING(Ahorros.Estado)
             Ahorros.FOR_Liquidacion:SCREEN-VALUE      = STRING(Ahorros.FOR_Liquidacion)
             Ahorros.Tasa:SCREEN-VALUE                 = STRING(Ahorros.Tasa)
             Ahorros.Exento_3xM:SCREEN-VALUE           = STRING(Ahorros.Exento_3xM)
             Ahorros.Id_Tesoro_Nacional:SCREEN-VALUE   = STRING(Ahorros.Id_Tesoro_Nacional)
             Ahorros.Id_Subsidio_Entidad:SCREEN-VALUE  = STRING(Ahorros.Id_Subsidio_Entidad)
             Ahorros.Id_Mesada_Pensional:SCREEN-VALUE  = STRING(Ahorros.Id_Mesada_Pensional)
             Ahorros.Ind_Tipo_Subsidio:SCREEN-VALUE    = STRING(Ahorros.Ind_Tipo_Subsidio)
             Ahorros.Fec_Activacion   [1]:SCREEN-VALUE = STRING(Ahorros.Fec_Activacion   [1])
             Ahorros.Fec_Activacion   [2]:SCREEN-VALUE = STRING(Ahorros.Fec_Activacion   [2])
             Ahorros.Fec_Activacion   [3]:SCREEN-VALUE = STRING(Ahorros.Fec_Activacion   [3])
             Ahorros.Fec_Activacion   [4]:SCREEN-VALUE = STRING(Ahorros.Fec_Activacion   [4])
             Ahorros.Fec_DesActivacion[1]:SCREEN-VALUE = STRING(Ahorros.Fec_DesActivacion[1])
             Ahorros.Fec_DesActivacion[2]:SCREEN-VALUE = STRING(Ahorros.Fec_DesActivacion[2])
             Ahorros.Fec_DesActivacion[3]:SCREEN-VALUE = STRING(Ahorros.Fec_DesActivacion[3])
             Ahorros.Fec_DesActivacion[4]:SCREEN-VALUE = STRING(Ahorros.Fec_DesActivacion[4]).
      FIND Rela_Clientes WHERE Rela_Clientes.Nit = Ahorros.Nit_Tutor NO-LOCK NO-ERROR.
      IF AVAIL Rela_Clientes THEN
         Nombre_Tutor:SCREEN-VALUE = Rela_Clientes.Nombre + " " + Rela_Clientes.Apellido1 + " " + Rela_Clientes.Apellido2.
      Cmb_Estados:SCREEN-VALUE = Cmb_Estados:ENTRY(Ahorros.Detalle_Estado).
      RUN Mostrar_Perliquidacion.
      Interes_PorPagar:SCREEN-VALUE = STRING(DEC(Ahorros.Tasa:SCREEN-VALUE) * Ahorros.Monto_Apertura / 100).
      IF Pro_Ahorros.Id_SalMinimo EQ YES AND Pro_Ahorros.Tip_SalMinimo EQ NO
         THEN ENABLE Ahorros.Sdo_MinCta WITH FRAME F_Ahorros.
         ELSE DISABLE Ahorros.Sdo_MinCta WITH FRAME F_Ahorros.
      IF Pro_Ahorro.Id_PerLiquidacion EQ 1 THEN DO:
         Cmb_PerLiquidacion:SCREEN-VALUE       = Cmb_PerLiquidacion:ENTRY(INT(Pro_Ahorros.Per_Liquidacion)).
         DISABLE Cmb_PerLiquidacion WITH FRAME F_Ahorros.
      END.
      ELSE DO:
         Cmb_PerLiquidacion:SCREEN-VALUE       = Cmb_PerLiquidacion:ENTRY(INT(Ahorros.Per_Liquidacion)).
         ENABLE Cmb_PerLiquidacion WITH FRAME F_Ahorros.
      END.
      IF Pro_Ahorros.Id_Forliquidacion EQ 1 THEN DO:
         ASSIGN Ahorros.For_Liquidacion:SCREEN-VALUE = STRING(Pro_Ahorros.For_Liquidacion).
         DISABLE Ahorros.FOR_Liquidacion.
      END.
      ELSE DO:
         ASSIGN Ahorros.For_Liquidacion:SCREEN-VALUE = STRING(Ahorros.For_Liquidacion).
         ENABLE Ahorros.FOR_Liquidacion.
      END.
      IF Pro_Ahorros.Id_Talonario EQ 1 OR Pro_Ahorros.Id_Talonario EQ 2
         THEN RUN Mostrar_Libretas.
      IF NOT NuevaCta THEN DO:
         FIND Clientes WHERE Clientes.Nit EQ Ahorros.Nit:SCREEN-VALUE NO-LOCK NO-ERROR.
         IF AVAIL Clientes THEN DO:
            W_Nomtitular:SCREEN-VALUE = CAPS(Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2).
            Wk_PerPagEmp = 0.
            IF Clientes.Cod_Empresa NE 0 THEN DO:
               FIND Empresas WHERE Empresas.Cod_Empresa EQ Clientes.Cod_Empresa NO-LOCK NO-ERROR.
               IF AVAIL Empresas THEN Wk_PerPagEmp = Empresas.FOR_Pago.
            END.
         END.
      END.
      ELSE DO:
         ASSIGN  W_Nomtitular:SCREEN-VALUE = "" Ahorros.Nit:SCREEN-VALUE = "".
         IF Pro_Ahorro.Id_PerLiquidacion EQ 2 THEN
            Cmb_PerLiquidacion:SCREEN-VALUE = Cmb_PerLiquidacion:ENTRY(1).
      END.
   END.
END. 
RUN Informacion_Secundaria.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Libretas wWin 
PROCEDURE Mostrar_Libretas :
DO WITH FRAME F_Libretas:
 FIND FIRST Lib_Chequera WHERE Lib_Chequera.Agencia        EQ Ahorros.Agencia      AND
                               Lib_Chequera.Cod_Producto   EQ Ahorros.Cod_Ahorro AND 
                               Lib_Chequera.Cue_Ahorros    EQ Ahorros.Cue_Ahorros  AND
                               Lib_Chequera.Estado         EQ 1 NO-ERROR.
 IF AVAILABLE Lib_Chequera THEN DO:
    ASSIGN Lib_Chequera.Tip_Talonario:SCREEN-VALUE = STRING(Lib_Chequera.Tip_Talonario)
           Lib_Chequera.Num_Inicial:SCREEN-VALUE   = STRING(Lib_Chequera.Num_Inicial)
           Lib_Chequera.Num_Final:SCREEN-VALUE     = STRING(Lib_Chequera.Num_Final)
           Lib_Chequera.Fec_Entrega:SCREEN-VALUE   = STRING(Lib_Chequera.Fec_Entrega)
           W_AccionLibreta:SCREEN-VALUE            = "Talonario Activo al Momento".
 END.
 ELSE W_AccionLibreta:SCREEN-VALUE IN FRAME F_Libretas = "No hay Libretas Matriculadas".
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_PerLiquidacion wWin 
PROCEDURE Mostrar_PerLiquidacion :
IF Ahorros.Per_Liquidacion EQ 0 THEN 
        ASSIGN W_DiaPer = 1     
               W_Per    = 365.
     ELSE
        IF Ahorros.Per_Liquidacion EQ 4 THEN  
           ASSIGN W_DiaPer = 30
                  W_Per    = 12.
        ELSE
           IF Ahorros.Per_Liquidacion EQ 5 THEN
              ASSIGN W_DiaPer = 60
                     W_Per    = 6.
           ELSE
              IF Ahorros.Per_Liquidacion EQ 6 THEN
                 ASSIGN W_DiaPer = 90
                        W_Per    = 4.
              ELSE
                 IF Ahorros.Per_Liquidacion EQ 8 THEN
                    ASSIGN W_DiaPer = 180 
                           W_Per    = 2.
                 ELSE
                    IF Ahorros.Per_Liquidacion EQ 9 THEN
                       ASSIGN W_DiaPer = 1
                              W_Per    = 360.
                    ELSE
                       IF Ahorros.Per_Liquidacion EQ 10 THEN 
                          ASSIGN W_DiaPer = Ahorros.Plazo
                                 W_Per    = (360 / Ahorros.Plazo).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Registro wWin 
PROCEDURE Mostrar_Registro :
IF NuevaCta THEN
    ASSIGN W_Nomtitular:SCREEN-VALUE IN FRAME F_Ahorros = ""
           Ahorros.Sdo_MinCta:SCREEN-VALUE IN FRAME F_Ahorros = ""
           Ahorros.Id_tutor:SCREEN-VALUE IN FRAME F_Ahorros = ""
           nombre_Tutor:SCREEN-VALUE IN FRAME F_Ahorros = "".
ELSE
    RUN Mostrar_General.

Ct_monto:SCREEN-VALUE IN FRAME f_Contractual = STRING(Pro_Ahorros.Val_MonAPer).

CASE Tpdt:
    WHEN 2 THEN DO:
        HIDE FRAME F_ALaVista
             FRAME F_Atermino.

        VIEW FRAME F_Contractual.

        DO WITH FRAME F_Contractual:
            IF AVAIL Ahorros AND AVAIL Pro_Ahorros AND NOT NuevaCta THEN
                RUN Mostrar_Contractual.

            IF AVAIL Pro_Ahorros AND Pro_Ahorros.tip_Ahorro EQ 2 AND (Pro_Ahorros.cod_Ahorro EQ 17 OR Pro_Ahorros.cod_Ahorro EQ 18 OR Pro_Ahorros.cod_Ahorro EQ 38) THEN DO:
                ASSIGN F-AporOblig:VISIBLE = TRUE
                       F-Salario:VISIBLE = TRUE
                       F-PorApoSue:VISIBLE = TRUE
                       Ahorros.porce_Aporte:VISIBLE = TRUE
                       Ahorros.porce_Aporte:SENSITIVE = TRUE.
            END.

            ENABLE CT_Cuota
                   CT_FormaPago.

            IF CT_FormaPago:SCREEN-VALUE EQ "3" THEN
                ENABLE BT_Debito
                       CT_FechaDebito.

            IF NuevaCta THEN DO:
                ENABLE CT_Monto
                       CT_Cuota
                       CT_Plazo
                       porce_aporte
                       CT_FormaPago.

                IF AVAIL Pro_Ahorros AND Pro_Ahorros.Id_Tasa EQ 1 THEN DO:
                    DISABLE Tasa_Efectiva WITH FRAME F_Ahorros.

                    ASSIGN Tasa_Efectiva:BGCOL = 18
                           Tasa_Efectiva:FGCOL = 15.
                END.
                ELSE DO:
                    ENABLE Tasa_Efectiva WITH FRAME F_Ahorros.

                    ASSIGN Tasa_Efectiva:BGCOL = 15
                           Tasa_Efectiva:FGCOL = 0.
                END.
            END.
        END.
    END.

    WHEN 3 THEN DO:
        HIDE FRAME F_Contractual
             FRAME F_ALaVista.

        VIEW FRAME F_Atermino.

        DO WITH FRAME F_Atermino:
            IF AVAIL Ahorros AND AVAIL Pro_Ahorros AND NOT NuevaCta THEN
                RUN Mostrar_Atermino.

            HIDE AT_VerTasa.
            DISABLE Tasa_Efectiva WITH FRAME F_Ahorros.
            ENABLE AT_Destino_Intereses.

            IF INTEGER(AT_Destino_Intereses:SCREEN-VALUE) LT 3 THEN
                ENABLE  Bt_DesAT.
            ELSE
                DISABLE Bt_DesAT.

            IF NuevaCta THEN DO:
                ENABLE AT_Monto.

                /* oakley */

               IF AVAIL Pro_Ahorros AND Pro_Ahorros.Id_Tasa EQ 1 THEN DO:
                  ASSIGN Tasa_Efectiva:BGCOL = 18
                         Tasa_Efectiva:FGCOL = 15.
                  DISP AT_VerTasa.
               END.
               ELSE DO:
                  HIDE AT_VerTasa.
                  ENABLE  Tasa_Efectiva WITH FRAME F_Ahorros.
                  ASSIGN Tasa_Efectiva:BGCOL = 15
                         Tasa_Efectiva:FGCOL = 7.
               END.
            END.
        END.
      END.
      OTHERWISE DO:
        DO WITH FRAME F_AlaVista:            
           HIDE FRAME F_Contractual FRAME F_Atermino.
           VIEW FRAME F_AlaVista.
           IF AVAIL Ahorros AND AVAIL Pro_Ahorros AND NOT NuevaCta THEN RUN Mostrar_AlaVista.
           HIDE AV_VerTasa IN FRAME F_AlaVista.
           DISABLE Tasa_Efectiva WITH FRAME F_Ahorros.
           IF tpdt = 1 THEN                           
                DISABLE AV_Cuota  WITH FRAME F_AlaVista.
             ELSE 
                 ENABLE AV_Cuota AP_FormaPago   WITH FRAME F_AlaVista.


           IF AP_FormaPago:SCREEN-VALUE EQ "3" THEN ENABLE  Bt_DebitoAP AP_FechaDebito.
                                               ELSE DISABLE Bt_DebitoAP AP_FechaDebito.
           IF NuevaCta THEN DO: 
              ENABLE AV_Monto AP_FormaPago WITH FRAME F_AlaVista.
/*            IF Pro_Ahorros.Tip_Ahorro EQ 4 THEN
                 ENABLE AP_FormaPago WITH FRAME F_AlaVista.
              ELSE
                 DISABLE AP_FormaPago WITH FRAME F_AlaVista.*/
              IF AVAIL Pro_Ahorros AND Pro_Ahorros.Id_Tasa EQ 1 THEN DO:
                 DISP AV_VerTasa WITH FRAME F_AlaVista.
                 ASSIGN Tasa_Efectiva:BGCOL = 18
                        Tasa_Efectiva:FGCOL = 15.
              END.
              ELSE DO:
                  HIDE AV_VerTasa IN FRAME F_AlaVista.
                  ENABLE Tasa_Efectiva WITH FRAME F_Ahorros.
                  ASSIGN Tasa_Efectiva:BGCOL = 15
                         Tasa_Efectiva:FGCOL = 10.
              END.
           END.
        END.
      END.
  END CASE.
/*IF Tpdt NE 1 THEN DISABLE Exento_3xm WITH FRAME F_Ahorros.
               ELSE ENABLE Exento_3xm  WITH FRAME F_Ahorros.
  RUN Mostrar_Tasas.
END.*/
     /* MESSAGE "" nuevacta tpdt VIEW-AS ALERT-BOX INFO BUTTON OK. */

IF NuevaCta THEN RUN Habilitar_Campos.
            ELSE DISABLE Cmb_PerLiquidacion WITH FRAME F_Ahorros.

    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Tasas wWin 
PROCEDURE Mostrar_Tasas :
DO WITH FRAME F_Ahorros:
 IF AVAILABLE Ahorros THEN DO:
   Ahorros.Tasa:SCREEN-VALUE = STRING(Ahorros.Tasa).
   
   CASE INTEGER(SUBSTRING(Cmb_PerLiquidacion:SCREEN-VALUE,1,2)):
     WHEN 1 THEN
       ASSIGN W_TasNom = Ahorros.Tasa / 365
              W_Per = 365.
     WHEN 2 THEN
       ASSIGN W_TasNom = Ahorros.Tasa / 12
              W_Per = 12.
     WHEN 3 THEN
       ASSIGN W_TasNom = Ahorros.Tasa / 4
              W_Per    = 4.
     WHEN 4 THEN
       ASSIGN W_TasNom = Ahorros.Tasa / 2
              W_per = 2.
     WHEN 5 THEN
       ASSIGN W_TasNom = Ahorros.Tasa 
              W_Per = 1.
     WHEN 6 THEN
       ASSIGN W_TasNom = Ahorros.Tasa / 365
              W_Per = 365.
   END CASE.
   
   Tasa_Nominal:SCREEN-VALUE = STRING(W_TasNom).
   
   IF INTEGER(Ahorros.For_Liquidacion:SCREEN-VALUE) EQ 1 THEN
      RUN NAEF IN W_ManFin (W_TasNom / 100,INPUT W_Per, OUTPUT W_Tasper).
   ELSE 
      RUN NVEF IN W_ManFin (W_TasNom / 100,INPUT W_Per ,OUTPUT W_Tasper).
      
   ASSIGN Tasa_Efectiva:LABEL                           = "Tasa Efectiva Anual"
          Tasa_Efectiva:SCREEN-VALUE = STRING(W_TasPer * 100) NO-ERROR.
   
   IF ERROR-STATUS:ERROR THEN 
      Tasa_Efectiva:SCREEN-VALUE = "0".
   
END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prc_TRF wWin 
PROCEDURE prc_TRF :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* Control de Tarjeta de Registro de Firmas TRF */


  DO WITH FRAME F_Relaciones:
    IF TRIM(Ahorros.TRF_Notas) NE " " THEN
      w-DescTRF:SCREEN-VALUE = Ahorros.TRF_Notas.
    ELSE
      w-DescTRF:SCREEN-VALUE = " ".
    Cmb_TRF:LIST-ITEMS = "".                             
    FIND FIRST clientes WHERE clientes.nit EQ Ahorros.nit:SCREEN-VALUE IN FRAME F_Ahorros NO-LOCK NO-ERROR.    
    IF AVAILABLE(clientes) THEN DO:                                            
        IF Clientes.tipo_cliente = 1 THEN DO: /* Natural Mayor de Edad */ 
      
                MESSAGE Ahorros.Nit:SCREEN-VALUE IN FRAME F_Ahorros
            SUBSTR(Cmb_TipoProductos:SCREEN-VALUE IN FRAME  F_Ahorros,1,1)
            SUBSTR(Cmb_Productos:SCREEN-VALUE IN FRAME F_Ahorros,1,3)
            Ahorros.Cue_Ahorros:SCREEN-VALUE IN FRAME F_Ahorros  VIEW-AS ALERT-BOX INFO.                    
       
       FOR EACH varios WHERE varios.tipo = 27 NO-LOCK:             
       W_Ok = Cmb_TRF:ADD-LAST(STRING(Varios.Codigo,"99") + " - " + TRIM(Varios.Descripcion)).
      END.
     FIND varios WHERE varios.tipo = 27 AND varios.codigo = Ahorros.trf NO-LOCK NO-ERROR.
       IF AVAILABLE(VARIOS) THEN
      ASSIGN Cmb_TRF:SCREEN-VALUE = STRING(Varios.Codigo,"99") + " - " + TRIM(Varios.Descripcion). 
      ELSE        
         MESSAGE "No se encontro en varios" VIEW-AS ALERT-BOX INFO.
    END.
    ELSE
      IF Clientes.tipo_cliente = 2 THEN DO: /* Natural Menor de Edad */ 
        FOR EACH varios WHERE varios.tipo = 28 NO-LOCK:
          W_Ok = Cmb_TRF:ADD-LAST(STRING(Varios.Codigo,"99") + " - " + TRIM(Varios.Descripcion)).
        END.
        FIND     varios WHERE varios.tipo = 28 AND varios.codigo = Ahorros.trf NO-LOCK NO-ERROR.
        ASSIGN Cmb_TRF:SCREEN-VALUE = STRING(Varios.Codigo,"99") + " - " + TRIM(Varios.Descripcion).
     END.
      ELSE   /* Juridicas */
      DO:
        FOR EACH varios WHERE varios.tipo = 29 NO-LOCK:
          W_Ok = Cmb_TRF:ADD-LAST(STRING(Varios.Codigo,"99") + " - " + TRIM(Varios.Descripcion)).
        END.
        FIND     varios WHERE varios.tipo = 29 AND varios.codigo = Ahorros.trf NO-LOCK NO-ERROR.
        ASSIGN Cmb_TRF:SCREEN-VALUE = STRING(Varios.Codigo,"99") + " - " + TRIM(Varios.Descripcion).
        SUBSTRING (Cmb_TRF:SCREEN-VALUE,1,2) ="02".
        APPLY "value-changed" TO cmb_trf IN FRAME f_relaciones.
      END.

    END.
        ELSE DO:
        MESSAGE "NO SE ENCONTRO REGISTRO" VIEW-AS ALERT-BOX INFORMATION.        
            RETURN NO-APPLY.
        END.                        
  END.
    
  FOR EACH Relaciones WHERE 
           Relaciones.Nit EQ Ahorros.Nit:SCREEN-VALUE IN FRAME F_Ahorros AND 
           Relaciones.Cod_Relacion  EQ  R_Eleccion AND 
           Relaciones.Clase_Producto EQ 1 AND        
           Relaciones.Cuenta         EQ Cue_Ahorros:SCREEN-VALUE IN FRAME F_Ahorros AND
           Relaciones.Estado EQ 1 NO-LOCK:

          FIND Clientes WHERE Clientes.Nit EQ Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
          IF AVAILABLE(Clientes) THEN DO:
             CREATE T_Relaciones.
             UPDATE T_Relaciones.R_Relacion  = Varios.Descripcion
                    T_Relaciones.R_AgeObjeto = Clientes.Agencia
                    T_Relaciones.R_NitObjeto = Relaciones.Nit_Relacion
                    T_Relaciones.R_VrAutoriz = Relaciones.Val_Autoriz
                    T_Relaciones.R_NomObjeto = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2
                    T_Relaciones.R_NomDescri = Relaciones.Descripcion
                    T_Relaciones.R_TelObjeto = Clientes.Tel_Residencia
                    T_Relaciones.R_CodPdt    = Relaciones.Cod_Producto
                    T_Relaciones.Est         = Relaciones.Estado.
          END.
           SUBSTRING (Cmb_TRF:SCREEN-VALUE IN FRAME F_Relaciones,1,3) = string(Ahorros.TRF).
            OPEN QUERY BR_Relaciones FOR EACH T_Relaciones NO-LOCK INDEXED-REPOSITION.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir wWin 
PROCEDURE ProcesoImprimir :
IF W_Inf EQ "Relaciones" THEN RUN Imprimir_Relaciones.
IF W_Inf EQ "Cuenta"    THEN RUN Imprimir_Cuenta.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Validar_Cambios wWin 
PROCEDURE Validar_Cambios :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FIND FIRST TAhorros NO-ERROR.
  InputFile = "Formatos\AV - 303.xls".
  IF AVAIL TAhorros THEN DO:
     /* Activa GMF Cuenta Existente */
     IF TAhorros.Exento_3xm = FALSE AND Ahorros.Exento_3xm = TRUE THEN DO:
        Wk_Cambio1 = "Cambia Exencion Legal de "
                   +  STRING(TAhorros.Exento_3xm,"Si/No") + " a " +
                      STRING(Ahorros.Exento_3xm, "Si/No") + " de ".
        IF TAhorros.Fec_Activacion[1] <> ? THEN Wk_Cambio1 = Wk_Cambio1 + STRING(TAhorros.Fec_Activacion[1],"99/99/9999").
                                           ELSE Wk_Cambio1 = Wk_Cambio1 + "SIN FECHA ".
        Wk_Cambio1 = Wk_Cambio1 + " a ".
        IF Ahorros.Fec_Activacion [1] <> ? THEN Wk_Cambio1 = Wk_Cambio1 + STRING(Ahorros.Fec_Activacion [1],"99/99/9999").
                                           ELSE Wk_Cambio1 = Wk_Cambio1 + "SIN FECHA ".
        ASSIGN Fecha_GMF[1] = Ahorros.Fec_Activacion[1] Fecha_GMF[2] = ?.
        RUN Grabar_Cambio (Wk_Cambio1,"ACT GMF").
        RUN Abrir_Excel.
        RUN Imprime_Formato_GMF.
      /*  RUN Cerrar_Excel. */
     END.
     /* Desactiva GMF Cuenta Existente */
     IF TAhorros.Exento_3xm = TRUE AND Ahorros.Exento_3xm = FALSE THEN DO:
        Wk_Cambio1 = "Cambia Exencion Legal de "
                   +  STRING(TAhorros.Exento_3xm,"Si/No") + " a "
                   +  STRING(Ahorros.Exento_3xm, "Si/No") + " de ".
        IF TAhorros.Fec_DesActivacion[1] <> ? THEN Wk_Cambio1 = Wk_Cambio1 + STRING(TAhorros.Fec_DesActivacion[1],"99/99/9999").
                                              ELSE Wk_Cambio1 = Wk_Cambio1 + "SIN FECHA ".
        Wk_Cambio1 = Wk_Cambio1 + " a ".
        IF Ahorros.Fec_DesActivacion [1] <> ? THEN Wk_Cambio1 = Wk_Cambio1 + STRING(Ahorros.Fec_DesActivacion [1],"99/99/9999").
                                              ELSE Wk_Cambio1 = Wk_Cambio1 + "SIN FECHA ".
        ASSIGN Fecha_GMF[2] = Ahorros.Fec_DesActivacion[1] Fecha_GMF[1] = ?.
        RUN Grabar_Cambio (Wk_Cambio1,"ACT GMF").
        RUN Abrir_Excel.
        RUN Imprime_Formato_GMF.
       /* RUN Cerrar_Excel. */
     END.
     /* Activa Subsidio Entidad Cuenta Existente */
     IF TAhorros.Id_Subsidio_Entidad = FALSE AND Ahorros.Id_Subsidio_Entidad = TRUE THEN DO:
        Wk_Cambio1 = "Cambia Subsidio_Entidad de "
                   +  STRING(TAhorros.Id_Subsidio_Entidad,"Si/No") + " a "
                   +  STRING(Ahorros.Id_Subsidio_Entidad, "Si/No") + " de ".
        IF TAhorros.Fec_Activacion[2] <> ? THEN Wk_Cambio1 = Wk_Cambio1 + STRING(TAhorros.Fec_Activacion[2],"99/99/9999").
                                           ELSE Wk_Cambio1 = Wk_Cambio1 + "SIN FECHA ".
        Wk_Cambio1 = Wk_Cambio1 + " a ".
        IF Ahorros.Fec_Activacion [2] <> ? THEN Wk_Cambio1 = Wk_Cambio1 + STRING(Ahorros.Fec_Activacion [2],"99/99/9999").
                                           ELSE Wk_Cambio1 = Wk_Cambio1 + "SIN FECHA ".
        RUN Grabar_Cambio (Wk_Cambio1,"ACT GMF").
     END.
     /* Desactiva Subsidio Entidad Cuenta Existente */
     IF TAhorros.Id_Subsidio_Entidad = TRUE AND Ahorros.Id_Subsidio_Entidad = FALSE THEN DO:
        Wk_Cambio1 = "Cambia Subsidio_Entidad de "
                   +  STRING(TAhorros.Id_Subsidio_Entidad,"Si/No") + " a "
                   +  STRING(Ahorros.Id_Subsidio_Entidad, "Si/No") + " de ".
        IF TAhorros.Fec_DesActivacion[2] <> ? THEN Wk_Cambio1 = Wk_Cambio1 + STRING(TAhorros.Fec_DesActivacion[2],"99/99/9999").
                                              ELSE Wk_Cambio1 = Wk_Cambio1 + "SIN FECHA ".
        Wk_Cambio1 = Wk_Cambio1 + " a ".
        IF Ahorros.Fec_DesActivacion [2] <> ? THEN Wk_Cambio1 = Wk_Cambio1 + STRING(Ahorros.Fec_DesActivacion [2],"99/99/9999").
                                              ELSE Wk_Cambio1 = Wk_Cambio1 + "SIN FECHA ".
        RUN Grabar_Cambio (Wk_Cambio1,"ACT GMF").
     END.
     /* Activa Tesoro Nacional Cuenta Existente */
     IF TAhorros.Id_Tesoro_Nacional = FALSE AND Ahorros.Id_Tesoro_Nacional = TRUE THEN DO:
        Wk_Cambio1 = "Cambia Tesoro_Nacional de "
                   +  STRING(TAhorros.Id_Tesoro_Nacional,"Si/No") + " a "
                   +  STRING(Ahorros.Id_Tesoro_Nacional, "Si/No") + " de ".
        IF TAhorros.Fec_Activacion[3] <> ? THEN Wk_Cambio1 = Wk_Cambio1 + STRING(TAhorros.Fec_Activacion[3],"99/99/9999").
                                           ELSE Wk_Cambio1 = Wk_Cambio1 + "SIN FECHA ".
        Wk_Cambio1 = Wk_Cambio1 + " a ".
        IF Ahorros.Fec_Activacion [3] <> ? THEN Wk_Cambio1 = Wk_Cambio1 + STRING(Ahorros.Fec_Activacion [3],"99/99/9999").
                                           ELSE Wk_Cambio1 = Wk_Cambio1 + "SIN FECHA ".
        RUN Grabar_Cambio (Wk_Cambio1,"ACT GMF").
     END.
     /* Desactiva Tesoro Nacional Cuenta Existente */
     IF TAhorros.Id_Tesoro_Nacional = TRUE AND Ahorros.Id_Tesoro_Nacional = FALSE THEN DO:
        Wk_Cambio1 = "Cambia Tesoro_Nacional de "
                   +  STRING(TAhorros.Id_Tesoro_Nacional,"Si/No") + " a "
                   +  STRING(Ahorros.Id_Tesoro_Nacional, "Si/No") + " de ".
        IF TAhorros.Fec_DesActivacion[3] <> ? THEN Wk_Cambio1 = Wk_Cambio1 + STRING(TAhorros.Fec_DesActivacion[3],"99/99/9999").
                                              ELSE Wk_Cambio1 = Wk_Cambio1 + "SIN FECHA ".
        Wk_Cambio1 = Wk_Cambio1 + " a ".
        IF Ahorros.Fec_DesActivacion [3] <> ? THEN Wk_Cambio1 = Wk_Cambio1 + STRING(Ahorros.Fec_DesActivacion [3],"99/99/9999").
                                              ELSE Wk_Cambio1 = Wk_Cambio1 + "SIN FECHA ".
        RUN Grabar_Cambio (Wk_Cambio1,"ACT GMF").
     END.
     /* Activa Mesada Pensional Cuenta Existente */
     IF TAhorros.Id_Mesada_Pensional = FALSE AND Ahorros.Id_Mesada_Pensional = TRUE THEN DO:
        Wk_Cambio1 = "Cambia Mesada_Pensional de "
                   +  STRING(TAhorros.Id_Mesada_Pensional,"Si/No") + " a "
                   +  STRING(Ahorros.Id_Mesada_Pensional, "Si/No") + " de ".
        IF TAhorros.Fec_Activacion[4] <> ? THEN Wk_Cambio1 = Wk_Cambio1 + STRING(TAhorros.Fec_Activacion[4],"99/99/9999").
                                           ELSE Wk_Cambio1 = Wk_Cambio1 + "SIN FECHA ".
        Wk_Cambio1 = Wk_Cambio1 + " a ".
        IF Ahorros.Fec_Activacion [4] <> ? THEN Wk_Cambio1 = Wk_Cambio1 + STRING(Ahorros.Fec_Activacion [4],"99/99/9999").
                                           ELSE Wk_Cambio1 = Wk_Cambio1 + "SIN FECHA ".
        RUN Grabar_Cambio (Wk_Cambio1,"ACT GMF").
     END.
     /* Desactiva Mesada Pensional Cuenta Existente */
     IF TAhorros.Id_Mesada_Pensional = TRUE AND Ahorros.Id_Mesada_Pensional = FALSE THEN DO:
        Wk_Cambio1 = "Cambia Mesada_Pensional de "
                   +  STRING(TAhorros.Id_Mesada_Pensional,"Si/No") + " a "
                   +  STRING(Ahorros.Id_Mesada_Pensional, "Si/No") + " de ".
        IF TAhorros.Fec_DesActivacion[4] <> ? THEN Wk_Cambio1 = Wk_Cambio1 + STRING(TAhorros.Fec_DesActivacion[4],"99/99/9999").
                                              ELSE Wk_Cambio1 = Wk_Cambio1 + "SIN FECHA ".
        Wk_Cambio1 = Wk_Cambio1 + " a ".
        IF Ahorros.Fec_DesActivacion [4] <> ? THEN Wk_Cambio1 = Wk_Cambio1 + STRING(Ahorros.Fec_DesActivacion [4],"99/99/9999").
                                              ELSE Wk_Cambio1 = Wk_Cambio1 + "SIN FECHA ".
        RUN Grabar_Cambio (Wk_Cambio1,"ACT GMF").
     END.
  END.
  ELSE DO:
     /* Activa GMF Cuenta Nueva */
     IF Ahorros.Exento_3xm = TRUE THEN DO:
        Wk_Cambio1 = "Activa la Exencion Legal a "
                   +  STRING(Ahorros.Exento_3xm,"Si/No")
          + " en " +  STRING(Ahorros.Fec_Activacion  [1],
                      IF Ahorros.Fec_Activacion      [1] <> ? THEN "99/99/9999" ELSE "SIN FECHA ").
        ASSIGN Fecha_GMF [1] = Ahorros.Fec_Activacion[1] Fecha_GMF[2] = ?.
        RUN Grabar_Cambio (Wk_Cambio1,"ACT GMF").
        RUN Abrir_Excel.
        RUN Imprime_Formato_GMF.
       /* RUN Cerrar_Excel. */
     END.
     /* Desactiva GMF Cuenta Nueva */
     IF Ahorros.Exento_3xm = FALSE AND Ahorros.Fec_DesActivacion[1] <> ? THEN DO:
        Wk_Cambio1 = "Activa la Exencion Legal a "
                   +  STRING(Ahorros.Exento_3xm,"Si/No")
          + " en " +  STRING(Ahorros.Fec_DesActivacion [1],
                      IF Ahorros.Fec_DesActivacion     [1] <> ? THEN "99/99/9999" ELSE "SIN FECHA ").
        ASSIGN Fecha_GMF[2] = Ahorros.Fec_DesActivacion[1] Fecha_GMF[1] = ?.
        RUN Grabar_Cambio (Wk_Cambio1,"ACT GMF").
        RUN Abrir_Excel.
        RUN Imprime_Formato_GMF.
      /*  RUN Cerrar_Excel. */
     END.
     /* Activa Subsidio Entidad Cuenta Nueva */
     IF Ahorros.Id_Subsidio_Entidad = TRUE THEN DO:
        Wk_Cambio1 = "Activa Subsidio_Entidad a "
                   +  STRING(Ahorros.Id_Subsidio_Entidad, "Si/No")
          + " en " +  STRING(Ahorros.Fec_Activacion   [2],
                      IF Ahorros.Fec_Activacion       [2] <> ? THEN "99/99/9999" ELSE "SIN FECHA ").
        RUN Grabar_Cambio (Wk_Cambio1,"ACT GMF").
     END.
     /* Desactiva Subsidio Entidad Cuenta Nueva */
     IF Ahorros.Id_Subsidio_Entidad = FALSE AND Ahorros.Fec_DesActivacion[2] <> ? THEN DO:
        Wk_Cambio1 = "Activa Subsidio_Entidad a "
                   +  STRING(Ahorros.Id_Subsidio_Entidad,"Si/No")
          + " en " +  STRING(Ahorros.Fec_DesActivacion[2],
                      IF Ahorros.Fec_DesActivacion    [2] <> ? THEN "99/99/9999" ELSE "SIN FECHA ").
        RUN Grabar_Cambio (Wk_Cambio1,"ACT GMF").
     END.
     /* Activa Tesoro Nacional Cuenta Nueva */
     IF Ahorros.Id_Tesoro_Nacional = TRUE THEN DO:
        Wk_Cambio1 = "Activa Tesoro_Nacional a "
                   +  STRING(Ahorros.Id_Tesoro_Nacional,"Si/No")
          + " en " +  STRING(Ahorros.Fec_Activacion   [3],
                      IF Ahorros.Fec_Activacion       [3] <> ? THEN "99/99/9999" ELSE "SIN FECHA ").
        RUN Grabar_Cambio (Wk_Cambio1,"ACT GMF").
     END.
     /* Desactiva Tesoro Nacional Cuenta Nueva */
     IF Ahorros.Id_Tesoro_Nacional = FALSE AND Ahorros.Fec_DesActivacion[3] <> ? THEN DO:
        Wk_Cambio1 = "Activa Tesoro_Nacional a "
                   +  STRING(Ahorros.Id_Tesoro_Nacional,"Si/No")
          + " en " +  STRING(Ahorros.Fec_DesActivacion[3],
                      IF Ahorros.Fec_DesActivacion    [3] <> ? THEN "99/99/9999" ELSE "SIN FECHA ").
        RUN Grabar_Cambio (Wk_Cambio1,"ACT GMF").
     END.
     /* Activa Mesada_Pensional Cuenta Nueva */
     IF Ahorros.Id_Mesada_Pensional = TRUE THEN DO:
        Wk_Cambio1 = "Activa Mesada_Pensional a "
                   +  STRING(Ahorros.Id_Mesada_Pensional,"Si/No")
          + " en " +  STRING(Ahorros.Fec_Activacion   [4],
                      IF Ahorros.Fec_Activacion       [4] <> ? THEN "99/99/9999" ELSE "SIN FECHA ").
        RUN Grabar_Cambio (Wk_Cambio1,"ACT GMF").
     END.
     /* Desactiva Mesada_Pensional Cuenta Nueva */
     IF Ahorros.Id_Mesada_Pensional = FALSE AND Ahorros.Fec_DesActivacion[4] <> ? THEN DO:
        Wk_Cambio1 = "Activa Mesada_Pensional a "
                   +  STRING(Ahorros.Id_Mesada_Pensional,"Si/No")
          + " en " +  STRING(Ahorros.Fec_DesActivacion[4],
                      IF Ahorros.Fec_DesActivacion    [4] <> ? THEN "99/99/9999" ELSE "SIN FECHA ").
        RUN Grabar_Cambio (Wk_Cambio1,"ACT GMF").
     END.
  END.
  FOR EACH TAhorros: DELETE TAhorros. END.
  CREATE TAhorros.
  BUFFER-COPY Ahorros TO TAhorros.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Validar_Operacion wWin 
PROCEDURE Validar_Operacion :
DEFINE INPUT  PARAMETER T_OfiVal  LIKE Agencias.Agencia.
  DEFINE INPUT  PARAMETER T_GrpVal  LIKE Grupos.Grupo.
  DEFINE INPUT  PARAMETER T_UsuVal  LIKE Usuarios.Usuario.
  DEFINE INPUT  PARAMETER T_OpeVal  LIKE Operacion.Cod_Operacion.
  DEFINE OUTPUT PARAMETER T_Validar AS LOGICAL.
  DEFINE OUTPUT PARAMETER T_NomOpe  LIKE Operacion.Nom_Operacion.
  
  DEFINE VAR T_Clave AS LOGICAL.
  
  ASSIGN T_Validar = FALSE T_Clave = FALSE.
  FIND Operacion WHERE Operacion.Cod_Operacion EQ T_OpeVal NO-LOCK NO-ERROR.
  IF AVAILABLE(Operacion) THEN DO:
     ASSIGN T_NomOpe = Operacion.Nom_Operacion
            T_Clave  = Operacion.Id_Clave.
  END.
  ELSE ASSIGN T_NomOpe = "".
  
  IF T_Clave THEN DO:
     MESSAGE "La Operación "  T_NomOpe   SKIP
             "Requiere Clave de SuperUsuario."
     VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
     TITLE "Validación En Taquilla".
     RUN P-ValiDarTrans IN W_Manija (OUTPUT W_Error,OUTPUT W_Autorizo).
     IF W_Error EQ FALSE THEN DO:
        ASSIGN T_Validar = TRUE.
     END.
  END.
  
  FIND FIRST Res_Operacion WHERE Res_Operacion.Restriccion   EQ 3
                           AND   Res_Operacion.Usuario       EQ T_UsuVal
                           AND   Res_Operacion.Cod_Operacion EQ T_OpeVal NO-LOCK NO-ERROR.
  IF AVAILABLE(Res_Operacion) THEN DO:
     ASSIGN T_Validar = TRUE.
  END.
  FIND FIRST Res_Operacion WHERE Res_Operacion.Restriccion   EQ 2
                           AND   Res_Operacion.Grupo         EQ T_GrpVal
                           AND   Res_Operacion.Cod_Operacion EQ T_OpeVal NO-LOCK NO-ERROR.
  IF AVAILABLE(Res_Operacion) THEN DO:
     ASSIGN T_Validar = TRUE.
  END.
  FIND FIRST Res_Operacion WHERE Res_Operacion.Restriccion   EQ 1
                           AND   Res_Operacion.Agencia       EQ T_OfiVal
                           AND   Res_Operacion.Cod_Operacion EQ T_OpeVal NO-LOCK NO-ERROR.
  IF AVAILABLE(Res_Operacion) THEN DO:
     ASSIGN T_Validar = TRUE.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Verificaciones wWin 
PROCEDURE Verificaciones :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME F_Ahorros:
/*
        IF pro_ahorros.id_persona = 2 OR  clientes.tipo_identificacion = "Nit" THEN DO:
         FIND FIRST relaciones WHERE relaciones.clase_producto = 1 AND relaciones.estado = 1 AND
             relaciones.cod_producto = INT(SUBSTR(Cmb_Productos:SCREEN-VALUE,1,3))  AND 
             relaciones.cuenta = ahorros.cue_Ahorros NO-LOCK NO-ERROR.
            IF NOT AVAILABLE(relaciones) THEN DO:
              MESSAGE "No se encontraron autorizados!" VIEW-AS ALERT-BOX.
              SwError = TRUE.
            END.
        END.
*/
        IF (Tpdt EQ 1 OR Tpdt EQ 5)  THEN DO:
     IF Ahorros.Id_Tesoro_Nacional:SCREEN-VALUE  = "Si" AND (Ahorros.Exento_3xM:SCREEN-VALUE = "Si" OR
        Ahorros.Id_Subsidio_Entidad:SCREEN-VALUE = "Si" OR Ahorros.Id_Mesada_Pensional:SCREEN-VALUE = "Si") THEN DO:
        MESSAGE "Cuenta del Tesoro Nacional No Puede Ser Subsidiada" VIEW-AS ALERT-BOX.
        SwError = TRUE.
     END.
     IF Ahorros.Id_Mesada_Pensional:SCREEN-VALUE = "Si" AND (Ahorros.Exento_3xM:SCREEN-VALUE = "Si" OR
        Ahorros.Id_Subsidio_Entidad:SCREEN-VALUE = "Si" OR Ahorros.Id_Tesoro_Nacional:SCREEN-VALUE = "Si") THEN DO:
        MESSAGE "Cuenta de Mesadas Pensionales No Puede Ser Subsidiada" VIEW-AS ALERT-BOX.
        SwError = TRUE.
     END.
     IF Ahorros.Id_Tesoro_Nacional:SCREEN-VALUE  = "Si" OR Ahorros.Exento_3xM:SCREEN-VALUE = "Si" OR
        Ahorros.Id_Subsidio_Entidad:SCREEN-VALUE = "Si" OR Ahorros.Id_Mesada_Pensional:SCREEN-VALUE = "Si" THEN DO:
        FIND FIRST Relaciones WHERE Relaciones.Nit             = Ahorros.Nit:SCREEN-VALUE IN FRAME F_Ahorros
                                AND Relaciones.Cod_Relacion    = 7
                                AND Relaciones.Clase_Producto  = 1
                                AND Relaciones.Cod_Producto    = INT(SUBSTR(Cmb_Productos:SCREEN-VALUE IN FRAME F_Ahorros,1,3))
                                AND Relaciones.Estado          = 1
                                AND Relaciones.Cuenta          = Ahorros.Cue_Ahorros:SCREEN-VALUE IN FRAME F_Ahorros NO-LOCK NO-ERROR.
       /* IF NOT AVAIL Ahorros OR AVAIL Relaciones OR (AVAIL Ahorros AND Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje <= 0) THEN DO:
           MESSAGE "Cuenta Sin Saldo o Con Mas de un Titular" SKIP
                   "no Puede Tener Exenciones del GMF" VIEW-AS ALERT-BOX.
           SwError = TRUE.
        END. */
        IF NOT SwError THEN DO:
           IF Ahorros.Exento_3xM:SCREEN-VALUE = "Si" THEN DO:
              FIND FIRST BAhorros WHERE BAhorros.Nit = Ahorros.Nit:SCREEN-VALUE IN FRAME F_Ahorros AND BAhorros.Exento_3xM = TRUE
                                  AND BAhorros.Cue_Ahorros <> Ahorros.Cue_Ahorros:SCREEN-VALUE IN FRAME F_Ahorros NO-LOCK NO-ERROR.
              IF AVAIL BAhorros THEN DO:
                 MESSAGE "Cliente Ya Posee Otra Cuenta con Exencion al GMF: " BAhorros.Cue_Ahorros VIEW-AS ALERT-BOX.
                 SwError = TRUE.
              END.
           END.
           IF Ahorros.Id_Subsidio_Entidad:SCREEN-VALUE = "Si" THEN DO:
              FIND FIRST BAhorros WHERE BAhorros.Nit = Ahorros.Nit:SCREEN-VALUE IN FRAME F_Ahorros AND BAhorros.Id_Subsidio_Entidad
                             = TRUE AND BAhorros.Cue_Ahorros <> Ahorros.Cue_Ahorros:SCREEN-VALUE IN FRAME F_Ahorros NO-LOCK NO-ERROR.
              IF AVAIL BAhorros THEN DO:
                 IF INT(SUBSTR(Cmb_TipoProductos:SCREEN-VALUE,1,1)) NE 3 THEN DO:   /* excepto cdat */
                    IF INT(SUBSTR(Cmb_TipoProductos:SCREEN-VALUE,1,1)) NE 2 THEN DO:   /* excepto contrac */
                       MESSAGE "Cliente Ya Posee Otra Cuenta con Subsidio al GMF: " BAhorros.Cue_Ahorros VIEW-AS ALERT-BOX.
                       SwError = TRUE.
                    END.
                 END.
              END.
           END.
           IF Ahorros.Id_Tesoro_Nacional:SCREEN-VALUE = "Si" THEN DO:
              FIND FIRST BAhorros WHERE BAhorros.Nit = Ahorros.Nit:SCREEN-VALUE IN FRAME F_Ahorros AND BAhorros.Id_Tesoro_Nacional
                             = TRUE AND BAhorros.Cue_Ahorros <> Ahorros.Cue_Ahorros:SCREEN-VALUE IN FRAME F_Ahorros NO-LOCK NO-ERROR.
              IF AVAIL BAhorros THEN DO:
                 MESSAGE "Cliente Ya Posee Otra Cuenta Marcada como 'del Tesoro Nacional': " BAhorros.Cue_Ahorros VIEW-AS ALERT-BOX.
                 SwError = TRUE.
              END.
           END.
           IF Ahorros.Id_Mesada_Pensional:SCREEN-VALUE = "Si" THEN DO:
              FIND FIRST BAhorros WHERE BAhorros.Nit = Ahorros.Nit:SCREEN-VALUE IN FRAME F_Ahorros AND BAhorros.Id_Mesada_Pensional
                             = TRUE AND BAhorros.Cue_Ahorros <> Ahorros.Cue_Ahorros:SCREEN-VALUE IN FRAME F_Ahorros NO-LOCK NO-ERROR.
              IF AVAIL BAhorros THEN DO:
                 MESSAGE "Cliente Ya Posee Otra Cuenta Marcada como de Mesada Pensional: " BAhorros.Cue_Ahorros VIEW-AS ALERT-BOX.
                 SwError = TRUE.
              END.
           END.
        END.
     END.
   END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

