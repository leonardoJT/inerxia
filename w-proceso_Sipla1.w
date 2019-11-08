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
  DEFINE VAR w_consecutivo AS INTEGER FORMAT "9999999999" INITIAL 0.
  DEFINE VAR w_vlrtotal    AS DECIMAL INITIAL 0.
  DEFINE VAR Listado       AS CHARACTER INITIAL "".
  DEFINE VAR WListado      AS CHARACTER INITIAL "".
  DEFINE VAR W_Dispositivo AS CHARACTER INITIAL " ".
  DEFINE VAR W_sw          AS LOGICAL. 
  DEFINE VAR W_Ok AS LOGICAL.
  
  DEFINE VAR i AS INTEGER.
  DEFINE VAR Orden_InsPan LIKE Instancias.Orden.
  DEFINE VAR Arc       AS CHARACTER FORMAT "X(25)".
  DEFINE VAR W_VigIns  AS INTEGER FORMAT "9999".
  DEFINE VAR W_Ultima  LIKE Instancias.Instancia.
  DEFINE VAR W_Primera LIKE Instancias.Instancia.
  DEFINE VAR W_Negadas LIKE Instancias.Instancia.
  DEFINE VAR Reg       AS INTEGER.
  DEFINE VAR Puntero AS ROWID.

  DEFINE VAR XLin AS CHARACTER FORMAT "X(100)".
  DEFINE VAR XTmp AS CHARACTER FORMAT "X(25)".
  DEFINE VAR XTmp2 AS CHARACTER FORMAT "X(25)".
  

  DEFINE VAR W_MaxDia  AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
  DEFINE VAR W_MaxMes  AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".

/*para buscar un cliente*/
  DEFINE VARIABLE P_Nit      LIKE Clientes.Nit.
  DEFINE VARIABLE p_Nombre   LIKE Clientes.Nombre.
  DEFINE VARIABLE P_Apellido LIKE Clientes.Apellido1.
  DEFINE VARIABLE P_AgeCli   LIKE Clientes.Agencia.
  DEFINE VARIABLE W_Nuevo    AS LOGICAL.
  DEFINE TEMP-TABLE TCerradas LIKE Mov_InsSipla.

  DEFINE TEMP-TABLE TUT
      FIELD Usuario LIKE Usuarios.Usuario
      FIELD Nombre  AS CHARACTER FORMAT "X(30)".

  DEFINE TEMP-TABLE TReportadosM
      FIELD Nit LIKE Clientes.Nit.

  DEFINE TEMP-TABLE TConsulta
      FIELD Agencia   LIKE Agencias.Agencia
      FIELD Instancia LIKE Instancias.Instancia
      FIELD Nit       LIKE Clientes.Nit
      FIELD Nombre    AS CHARACTER FORMAT "X(40)"
      FIELD Id_NUD    AS LOGICAL
      FIELD Id_NUM    AS LOGICAL
      FIELD SCons     AS DECIMAL FORMAT "->>>,>>>,>>9"
      FIELD SAbon     AS DECIMAL FORMAT "->>>,>>>,>>9"
      FIELD SReti     AS DECIMAL FORMAT "->>>,>>>,>>9"
      FIELD IdNor     LIKE Mov_InsSipla.Id_Sospechosa
      FIELD IdSos     LIKE Mov_InsSipla.Id_Sospechosa
      FIELD IdUIAF    LIKE Mov_InsSipla.Id_Sospechosa
      FIELD IdROSS    LIKE Mov_InsSipla.Id_Sospechosa
      FIELD FecRep    AS DATE
      FIELD HorRep    LIKE Mov_InsSipla.Hora_Gestion
      FIELD Vigencia  AS INTEGER FORMAT "99999"
      FIELD PCod      LIKE Mov_InsSipla.CodAutoriza
      FIELD IAct      LIKE Mov_InsSipla.Instancia
      FIELD IAnt      LIKE Mov_InsSipla.Instancia_Anterior
      FIELD TipReg    LIKE Mov_InsSipla.Tipo_Registro.

  DEFINE TEMP-TABLE TTransacciones
      FIELD AgeTran   LIKE Agencias.Agencia
      FIELD CodTran   LIKE Ahorros.Cod_Ahorro
      FIELD CueTran   LIKE Ahorros.Cue_Ahorros
      FIELD ConTran   LIKE Ahorros.Sdo_Disponible
      FIELD RetTran   LIKE Ahorros.Sdo_Disponible
      FIELD FecTran   AS DATE
      FIELD BddTran   AS LOGICAL
      FIELD BdmTran   AS LOGICAL
      FIELD EfeChe    AS CHARACTER FORMAT "X".

  DEFINE TEMP-TABLE TTransaccionesC
      FIELD AgeCre   LIKE Agencias.Agencia
      FIELD CodCre   LIKE Creditos.Cod_Credito
      FIELD PagCre   LIKE Creditos.Pagare
      FIELD DepCre   LIKE Creditos.Sdo_Capital
      FIELD FecCre   AS DATE
      FIELD BddCre   AS LOGICAL
      FIELD BdmCre   AS LOGICAL
      FIELD EfeChe    AS CHARACTER FORMAT "X".

  DEFINE TEMP-TABLE TInfoCli
      FIELD Reg AS INTEGER
      FIELD Tip AS INTEGER
      FIELD Lin AS CHARACTER FORMAT "X(100)".

  DEFINE TEMP-TABLE TInfoAho
      FIELD AReg AS INTEGER
      FIELD ATip AS INTEGER
      FIELD ALin AS CHARACTER FORMAT "X(100)".

  DEFINE TEMP-TABLE TInfoCre
      FIELD CReg AS INTEGER
      FIELD CTip AS INTEGER
      FIELD CLin AS CHARACTER FORMAT "X(100)".

  DEFINE TEMP-TABLE TInfoHVd
      FIELD HReg AS INTEGER
      FIELD HTip AS INTEGER
      FIELD HLin AS CHARACTER FORMAT "X(100)".

  DEFINE FRAME FEncHV
      HEADER
      "______________________________________________________________________________"
    WITH DOWN WIDTH 220 USE-TEXT PAGE-TOP FRAME FEncHV STREAM-IO.

    DEFINE FRAME FFTRHV
      HEADER 
        "____________________________________________________________________________" AT 2
      WITH DOWN WIDTH 132 FRAME FFTRHV PAGE-BOTTOM USE-TEXT STREAM-IO.


DEFINE TEMP-TABLE RegistrosUIAF
    FIELD consecutivo AS INTEGER
    FIELD registro   AS CHARACTER FORMAT "X(89)".

DEFINE VAR SSTTTCCC AS CHARACTER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FCerradas
&Scoped-define BROWSE-NAME BCerradas

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TCerradas TConsulta TTransaccionesC ~
TTransacciones TUT Mov_InsSipla

/* Definitions for BROWSE BCerradas                                     */
&Scoped-define FIELDS-IN-QUERY-BCerradas TCerradas.Agencia TCerradas.Instancia TCerradas.UsuGestiona TCerradas.Id_NUM TCerradas.Id_NUD TCerradas.Id_Sospechosa TCerradas.Id_RepUIAF TCerradas.Id_Exonerada TCerradas.Fecha_Transaccion TCerradas.Fecha_Gestion   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BCerradas   
&Scoped-define SELF-NAME BCerradas
&Scoped-define QUERY-STRING-BCerradas FOR EACH TCerradas
&Scoped-define OPEN-QUERY-BCerradas OPEN QUERY BCerradas FOR EACH TCerradas.
&Scoped-define TABLES-IN-QUERY-BCerradas TCerradas
&Scoped-define FIRST-TABLE-IN-QUERY-BCerradas TCerradas


/* Definitions for BROWSE BConsulta                                     */
&Scoped-define FIELDS-IN-QUERY-BConsulta TConsulta.Agencia TConsulta.Nit TConsulta.Nombre TConsulta.Id_NUD TConsulta.Id_NUM TConsulta.SCons TConsulta.SReti TConsulta.SAbon TConsulta.FecRep STRING(TConsulta.HorRep,"HH:MM:SS am") TConsulta.Vigencia TConsulta.TipReg   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BConsulta   
&Scoped-define SELF-NAME BConsulta
&Scoped-define QUERY-STRING-BConsulta FOR EACH TConsulta NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BConsulta OPEN QUERY {&SELF-NAME} FOR EACH TConsulta NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BConsulta TConsulta
&Scoped-define FIRST-TABLE-IN-QUERY-BConsulta TConsulta


/* Definitions for BROWSE BCreditos                                     */
&Scoped-define FIELDS-IN-QUERY-BCreditos TTransaccionesC.AgeCre TTransaccionesC.PagCre TTransaccionesC.FecCre TTransaccionesC.EfeChe TTransaccionesC.DepCre   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BCreditos   
&Scoped-define SELF-NAME BCreditos
&Scoped-define QUERY-STRING-BCreditos FOR EACH TTransaccionesC
&Scoped-define OPEN-QUERY-BCreditos OPEN QUERY {&SELF-NAME} FOR EACH TTransaccionesC.
&Scoped-define TABLES-IN-QUERY-BCreditos TTransaccionesC
&Scoped-define FIRST-TABLE-IN-QUERY-BCreditos TTransaccionesC


/* Definitions for BROWSE BTransacciones                                */
&Scoped-define FIELDS-IN-QUERY-BTransacciones TTransacciones.AgeTran TTransacciones.CodTran TTransacciones.CueTran TTransacciones.FecTran TTransacciones.EfeChe TTransacciones.ConTran TTransacciones.RetTran   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BTransacciones   
&Scoped-define SELF-NAME BTransacciones
&Scoped-define QUERY-STRING-BTransacciones FOR EACH TTransacciones NO-LOCK BY TTransacciones.FecTran INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BTransacciones OPEN QUERY {&SELF-NAME} FOR EACH TTransacciones NO-LOCK BY TTransacciones.FecTran INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BTransacciones TTransacciones
&Scoped-define FIRST-TABLE-IN-QUERY-BTransacciones TTransacciones


/* Definitions for BROWSE BTraspaso                                     */
&Scoped-define FIELDS-IN-QUERY-BTraspaso Tut.Usuario Tut.Nombre   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BTraspaso   
&Scoped-define SELF-NAME BTraspaso
&Scoped-define QUERY-STRING-BTraspaso FOR EACH TUT
&Scoped-define OPEN-QUERY-BTraspaso OPEN QUERY {&SELF-NAME} FOR EACH TUT.
&Scoped-define TABLES-IN-QUERY-BTraspaso TUT
&Scoped-define FIRST-TABLE-IN-QUERY-BTraspaso TUT


/* Definitions for FRAME FCerradas                                      */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FCerradas ~
    ~{&OPEN-QUERY-BCerradas}

/* Definitions for FRAME FConsulta                                      */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FConsulta ~
    ~{&OPEN-QUERY-BConsulta}

/* Definitions for FRAME FDetCon                                        */
&Scoped-define FIELDS-IN-QUERY-FDetCon Mov_InsSipla.Agencia ~
Mov_InsSipla.Fecha_Transaccion Mov_InsSipla.Nit Mov_InsSipla.Fecha_Gestion ~
Mov_InsSipla.Id_NUM Mov_InsSipla.Id_Exonerada Mov_InsSipla.Id_Sospechosa ~
Mov_InsSipla.Id_RepUIAF Mov_InsSipla.Id_RepROSS Mov_InsSipla.Id_NUD ~
Mov_InsSipla.Fec_RepUIAF Mov_InsSipla.Fec_RepROSS Mov_InsSipla.Descripcion ~
Mov_InsSipla.Tipo_Registro Mov_InsSipla.Valor_RegManual ~
Mov_InsSipla.UsuReporta Mov_InsSipla.UsuGestiona 
&Scoped-define ENABLED-FIELDS-IN-QUERY-FDetCon Mov_InsSipla.Id_Exonerada ~
Mov_InsSipla.Id_Sospechosa Mov_InsSipla.Id_RepUIAF Mov_InsSipla.Id_RepROSS ~
Mov_InsSipla.Descripcion 
&Scoped-define ENABLED-TABLES-IN-QUERY-FDetCon Mov_InsSipla
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-FDetCon Mov_InsSipla
&Scoped-define QUERY-STRING-FDetCon FOR EACH Mov_InsSipla SHARE-LOCK
&Scoped-define OPEN-QUERY-FDetCon OPEN QUERY FDetCon FOR EACH Mov_InsSipla SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-FDetCon Mov_InsSipla
&Scoped-define FIRST-TABLE-IN-QUERY-FDetCon Mov_InsSipla


/* Definitions for FRAME FManual                                        */
&Scoped-define QUERY-STRING-FManual FOR EACH Mov_InsSipla SHARE-LOCK
&Scoped-define OPEN-QUERY-FManual OPEN QUERY FManual FOR EACH Mov_InsSipla SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-FManual Mov_InsSipla
&Scoped-define FIRST-TABLE-IN-QUERY-FManual Mov_InsSipla


/* Definitions for FRAME FTransacciones                                 */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FTransacciones ~
    ~{&OPEN-QUERY-BCreditos}~
    ~{&OPEN-QUERY-BTransacciones}

/* Definitions for FRAME FTraspaso                                      */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FTraspaso ~
    ~{&OPEN-QUERY-BTraspaso}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BCerradas ECerradas BtnOcu_Cerradas 
&Scoped-Define DISPLAYED-OBJECTS ECerradas 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 Btn_Trans Btn_Cliente Btn_Ahorros Btn_Creditos ~
Btn_HojaVida 
&Scoped-define List-2 NitM Id_NUMM Id_NUDM Id_ExoneradaM Id_SospechosaM ~
Id_RepFiscaliaM Id_RepROSSM ValManual DescripcionM 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnOcu_Cerradas 
     LABEL "Ocultar" 
     SIZE 15 BY 1.12.

DEFINE VARIABLE ECerradas AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 95 BY 5.38
     BGCOLOR 15 FONT 2 NO-UNDO.

DEFINE BUTTON Btn_RegManual 
     LABEL "Adicionar Registro Manual" 
     SIZE 31 BY 1.12.

DEFINE BUTTON Btn_Traspasar 
     LABEL "Pasar todos los registros a otro usuario" 
     SIZE 31 BY 1.12.

DEFINE BUTTON BUTTON-205 
     IMAGE-UP FILE "imagenes/delimtxt.bmp":U
     LABEL "Button 205" 
     SIZE 8 BY 2.15.

DEFINE VARIABLE ROrganizar AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Agencia", 1,
"Nit", 2,
"Nombre", 3,
"Fecha", 4,
"Vigencia", 5
     SIZE 49 BY .81
     FONT 5 NO-UNDO.

DEFINE RECTANGLE RECT-304
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 52.14 BY 1.35.

DEFINE BUTTON Btn_Ahorros 
     LABEL "Ahorros" 
     SIZE 18 BY 1.12.

DEFINE BUTTON Btn_BorManual 
     LABEL "Borrar Registro Manual" 
     SIZE 23 BY 1.08.

DEFINE BUTTON Btn_Cerradas 
     LABEL "Ver Instancias Cerradas" 
     SIZE 22 BY 1.12.

DEFINE BUTTON Btn_Cliente 
     LABEL "Cliente" 
     SIZE 18 BY 1.12.

DEFINE BUTTON Btn_Creditos 
     LABEL "Creditos" 
     SIZE 18 BY 1.12.

DEFINE BUTTON Btn_Devolver 
     LABEL "Devolver Una Instancia" 
     SIZE 22 BY 1.12.

DEFINE BUTTON Btn_HojaVida 
     LABEL "Hoja de Vida" 
     SIZE 18 BY 1.12.

DEFINE BUTTON Btn_OcuDetCon 
     LABEL "Consultar otro" 
     SIZE 22 BY 1.12.

DEFINE BUTTON Btn_Pasar 
     LABEL "Pasar a Siguiente Instancia" 
     SIZE 22 BY 1.12.

DEFINE BUTTON Btn_Salvar 
     LABEL "Salvar Modificaciones" 
     SIZE 22 BY 1.12.

DEFINE BUTTON Btn_Trans 
     LABEL "Transacciones" 
     SIZE 18 BY 1.12.

DEFINE VARIABLE AboCreditosChe AS DECIMAL FORMAT ">>>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE AboCreditosEfe AS DECIMAL FORMAT ">>>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE ConsCheque AS DECIMAL FORMAT ">>>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE ConsEfectivo AS DECIMAL FORMAT ">>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE RetCheque AS DECIMAL FORMAT ">>>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE RetEfectivo AS DECIMAL FORMAT ">>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WDevuelto AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE WHorGestion AS CHARACTER FORMAT "X(12)":U 
     VIEW-AS FILL-IN 
     SIZE 10.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WHorTransaccion AS CHARACTER FORMAT "X(12)":U 
     VIEW-AS FILL-IN 
     SIZE 10.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WNomAgencia AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WNomCliente AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WNomUsuGestiona AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 25.29 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WNomUsuReporta AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 25.29 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-302
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 69 BY 1.04.

DEFINE RECTANGLE RECT-324
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 53 BY 2.96.

DEFINE BUTTON Btn_outfiltros 
     LABEL "Ocultar" 
     SIZE 15 BY 1.12.

DEFINE VARIABLE RFiltros AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Todos los Registros", 1,
"Los del Mes Actual", 2,
"Los del dia anterior", 3,
"Los del dia actual", 4,
"Los de meses anteriores", 5,
"Marcados Sospechosos", 6,
"Marcados Rep.UIAF", 7,
"Marcados Rep.ROSS", 8,
"Marcados Normal", 9,
"Registros Automaticos", 10,
"Registros Manuales", 11
     SIZE 26 BY 9.69 NO-UNDO.

DEFINE BUTTON BUTTON-200 
     LABEL "Imprimir" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-201 
     LABEL "Ocultar" 
     SIZE 15 BY 1.12.

DEFINE VARIABLE FecCorte AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha de Corte" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE RInformes AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Informe del Registro Actual", 1,
"Informe de los Registros de la Instancia", 2,
"Reporte Transacciones en Efectivo", 3,
"Reporte Clientes Exonerados", 4,
"Reporte de Productos", 5,
"Reporte ROSS", 6
     SIZE 31 BY 6.19 NO-UNDO.

DEFINE VARIABLE TIncluir AS LOGICAL INITIAL no 
     LABEL "Incluir operaciones de meses anteriores sin reportar?" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .77 NO-UNDO.

DEFINE BUTTON Btn_Alavista 
     LABEL "A la Vista" 
     SIZE 22 BY 1.12.

DEFINE BUTTON Btn_Aportes 
     LABEL "Aportes" 
     SIZE 22 BY 1.12.

DEFINE BUTTON Btn_Atermino 
     LABEL "A Termino" 
     SIZE 22 BY 1.12.

DEFINE BUTTON Btn_Contractual 
     LABEL "Contractual" 
     SIZE 22 BY 1.12.

DEFINE BUTTON Btn_OcultarAhorros 
     LABEL "Ocultar" 
     SIZE 16 BY 1.12.

DEFINE VARIABLE SInfoAhorros AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 91 BY 8.08
     BGCOLOR 15 FONT 2 NO-UNDO.

DEFINE BUTTON Btn_Controles 
     LABEL "Controles" 
     SIZE 15 BY .81.

DEFINE BUTTON Btn_Fechas 
     LABEL "Fechas" 
     SIZE 15 BY .81.

DEFINE BUTTON Btn_Relaciones 
     LABEL "Relaciones" 
     SIZE 15 BY .81.

DEFINE BUTTON Btn_Segmentacion 
     LABEL "Segmentacion" 
     SIZE 15 BY .81.

DEFINE BUTTON BUTTON-186 
     LABEL "Ubicacion" 
     SIZE 15 BY .81.

DEFINE BUTTON BUTTON-187 
     LABEL "Economica" 
     SIZE 15 BY .81.

DEFINE BUTTON BUTTON-192 
     LABEL "Ocultar" 
     SIZE 16 BY 1.12.

DEFINE VARIABLE SInfoCliente AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE 
     SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL 
     SIZE 92 BY 8.35
     BGCOLOR 15 FONT 2 NO-UNDO.

DEFINE BUTTON Btn_Comercial 
     LABEL "Comercial" 
     SIZE 22 BY 1.12.

DEFINE BUTTON Btn_Consumo 
     LABEL "Consumo" 
     SIZE 22 BY 1.12.

DEFINE BUTTON Btn_Hipotecario 
     LABEL "Hipotecario" 
     SIZE 22 BY 1.12.

DEFINE BUTTON Btn_Microcredito 
     LABEL "Microcredito" 
     SIZE 22 BY 1.12.

DEFINE BUTTON Btn_OcuCre 
     LABEL "Ocultar" 
     SIZE 16 BY 1.12.

DEFINE VARIABLE SInfoCreditos AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 91 BY 8.08
     BGCOLOR 15 FONT 2 NO-UNDO.

DEFINE BUTTON Btn_OcuHV 
     LABEL "Ocultar" 
     SIZE 16 BY 1.12.

DEFINE VARIABLE SInfoHojaVida AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL LARGE
     SIZE 94 BY 9.15
     BGCOLOR 15 FONT 2 NO-UNDO.

DEFINE BUTTON Btn_OutManual 
     LABEL "Ocultar" 
     SIZE 15 BY 1.12.

DEFINE BUTTON Btn_SalvaManual 
     LABEL "Salvar" 
     SIZE 15 BY 1.12.

DEFINE VARIABLE DescripcionM AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL LARGE
     SIZE 92 BY 2.42
     BGCOLOR 15 FONT 2.

DEFINE VARIABLE Fecha_TransaccionM AS DATE FORMAT "99/99/9999" 
     LABEL "Fecha Transaccion" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NitM AS CHARACTER FORMAT "X(14)" 
     LABEL "Nit" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE ValManual AS DECIMAL FORMAT ">>>,>>>,>>>,>>9":U INITIAL 0 
     LABEL "Valor de la operacion por el cual se graba este registro manual" 
     VIEW-AS FILL-IN 
     SIZE 17.86 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WNomClienteM AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 69 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-323
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 63 BY 1.35.

DEFINE VARIABLE Id_ExoneradaM AS LOGICAL INITIAL no 
     LABEL "Normal" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY .77
     FONT 5.

DEFINE VARIABLE Id_NUDM AS LOGICAL INITIAL no 
     LABEL "No Usual Dia" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .65
     FONT 5.

DEFINE VARIABLE Id_NUMM AS LOGICAL INITIAL no 
     LABEL "No Usual Mes" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .65
     FONT 5 NO-UNDO.

DEFINE VARIABLE Id_RepFiscaliaM AS LOGICAL INITIAL no 
     LABEL "Reportar a UIAF" 
     VIEW-AS TOGGLE-BOX
     SIZE 17.29 BY .77
     FONT 5.

DEFINE VARIABLE Id_RepROSSM AS LOGICAL INITIAL no 
     LABEL "Reportar ROSS" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .77
     FONT 5.

DEFINE VARIABLE Id_SospechosaM AS LOGICAL INITIAL no 
     LABEL "Sospechosa" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY .77
     FONT 5.

DEFINE BUTTON BUTTON-203 
     LABEL "Imprimir" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-204 
     LABEL "Ocultar" 
     SIZE 15 BY 1.12.

DEFINE VARIABLE E1 AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE 94 BY 15.35
     BGCOLOR 15 FONT 2 NO-UNDO.

DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Salir" 
     SIZE 13 BY 1.92
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-195 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 195" 
     SIZE 13 BY 1.92.

DEFINE BUTTON BUTTON-196 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Button 196" 
     SIZE 13 BY 1.92.

DEFINE BUTTON BUTTON-197 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     LABEL "Button 197" 
     SIZE 13 BY 2.19.

DEFINE VARIABLE Cmb_Instancias AS CHARACTER FORMAT "X(256)":U 
     LABEL "Instancias" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 35 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomUsuario AS CHARACTER FORMAT "X(50)":U 
     LABEL "Usuario" 
     VIEW-AS FILL-IN 
     SIZE 40 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE BUTTON BtnOcultar 
     LABEL "Ocultar" 
     SIZE 15 BY 1.12.

DEFINE VARIABLE W_AboCreChe AS DECIMAL FORMAT ">>>,>>>,>>>,>>9":U INITIAL 0 
     LABEL "Abonos Credito Cheque" 
     VIEW-AS FILL-IN 
     SIZE 12.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_AboCreEfe AS DECIMAL FORMAT ">>>,>>>,>>>,>>9":U INITIAL 0 
     LABEL "Abonos Credito Efectivo" 
     VIEW-AS FILL-IN 
     SIZE 12.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_ValConChe AS DECIMAL FORMAT ">>>,>>>,>>>,>>9":U INITIAL 0 
     LABEL "Cheque" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_ValConEfe AS DECIMAL FORMAT ">>>,>>>,>>>,>>9":U INITIAL 0 
     LABEL "Efectivo" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_ValRetChe AS DECIMAL FORMAT ">>>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_ValRetEfe AS DECIMAL FORMAT ">>>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE BUTTON Btn_outtraspaso 
     LABEL "Ocultar" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-199 
     LABEL "Traspasar" 
     SIZE 15 BY 1.12.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BCerradas FOR 
      TCerradas SCROLLING.

DEFINE QUERY BConsulta FOR 
      TConsulta SCROLLING.

DEFINE QUERY BCreditos FOR 
      TTransaccionesC SCROLLING.

DEFINE QUERY BTransacciones FOR 
      TTransacciones SCROLLING.

DEFINE QUERY BTraspaso FOR 
      TUT SCROLLING.

DEFINE QUERY FDetCon FOR 
      Mov_InsSipla SCROLLING.

DEFINE QUERY FManual FOR 
      Mov_InsSipla SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BCerradas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BCerradas wWin _FREEFORM
  QUERY BCerradas DISPLAY
      TCerradas.Agencia      LABEL "Age"
 TCerradas.Instancia         LABEL "Instancia"
 TCerradas.UsuGestiona       LABEL "Gestion"
 TCerradas.Id_NUM            LABEL "NUM"
 TCerradas.Id_NUD            LABEL "NUD"
 TCerradas.Id_Sospechosa     LABEL "Sosp"
 TCerradas.Id_RepUIAF        LABEL "RepF"
 TCerradas.Id_Exonerada      LABEL "Exo"
 TCerradas.Fecha_Transaccion
 TCerradas.Fecha_Gestion
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 95 BY 3.5
         BGCOLOR 15 FONT 4 ROW-HEIGHT-CHARS .5 FIT-LAST-COLUMN.

DEFINE BROWSE BConsulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BConsulta wWin _FREEFORM
  QUERY BConsulta DISPLAY
      TConsulta.Agencia COLUMN-LABEL "Age"
 TConsulta.Nit
 TConsulta.Nombre  FORMAT "X(31)"
 TConsulta.Id_NUD   COLUMN-LABEL "NUD"
 TConsulta.Id_NUM   COLUMN-LABEL "NUM"
 TConsulta.SCons  COLUMN-LABEL "Consignac"
 TConsulta.SReti COLUMN-LABEL "Retiros"
 TConsulta.SAbon COLUMN-LABEL "AboCreditos"
 TConsulta.FecRep LABEL "Reportada"
 STRING(TConsulta.HorRep,"HH:MM:SS am") LABEL "H.Reporte"
 TConsulta.Vigencia COLUMN-LABEL "Vig" FORMAT "999"
 TConsulta.TipReg COLUMN-LABEL "TR" FORMAT "X"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 96 BY 14.54
         BGCOLOR 15 FONT 4 ROW-HEIGHT-CHARS .51 FIT-LAST-COLUMN.

DEFINE BROWSE BCreditos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BCreditos wWin _FREEFORM
  QUERY BCreditos DISPLAY
      TTransaccionesC.AgeCre
 TTransaccionesC.PagCre
 TTransaccionesC.FecCre
 TTransaccionesC.EfeChe LABEL "E/C"
 TTransaccionesC.DepCre LABEL "Depositos" FORMAT ">>>,>>>,>>9"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 38 BY 7.27
         BGCOLOR 15 FONT 4 FIT-LAST-COLUMN.

DEFINE BROWSE BTransacciones
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BTransacciones wWin _FREEFORM
  QUERY BTransacciones DISPLAY
      TTransacciones.AgeTran COLUMN-LABEL "Age"
 TTransacciones.CodTran COLUMN-LABEL "Pdt"
 TTransacciones.CueTran COLUMN-LABEL "Cuenta"
 TTransacciones.FecTran COLUMN-LABEL "Fecha"
 TTransacciones.EfeChe COLUMN-LABEL "E/C"
 TTransacciones.ConTran COLUMN-LABEL "Depositos" FORMAT ">,>>>,>>>,>>9"
 TTransacciones.RetTran COLUMN-LABEL "Retiros" FORMAT ">,>>>,>>>,>>9"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 51 BY 8.35
         BGCOLOR 15 FONT 4 ROW-HEIGHT-CHARS .55 FIT-LAST-COLUMN.

DEFINE BROWSE BTraspaso
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BTraspaso wWin _FREEFORM
  QUERY BTraspaso DISPLAY
      Tut.Usuario
 Tut.Nombre
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 43 BY 6.73
         BGCOLOR 15 FONT 4 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FSipla
     Cmb_Instancias AT ROW 1.27 COL 11 COLON-ALIGNED
     NomUsuario AT ROW 1.27 COL 55 COLON-ALIGNED
     BUTTON-195 AT ROW 1.54 COL 101
     BUTTON-196 AT ROW 3.42 COL 101
     BUTTON-197 AT ROW 5.31 COL 101
     BtnDone AT ROW 19.85 COL 101
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.57 BY 21.73
         BGCOLOR 17 FONT 5
         DEFAULT-BUTTON BtnDone.

DEFINE FRAME FDetCon
     Mov_InsSipla.Agencia AT ROW 1.46 COL 7 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 18 FGCOLOR 15 
     WNomAgencia AT ROW 1.46 COL 19.29 COLON-ALIGNED NO-LABEL
     Mov_InsSipla.Fecha_Transaccion AT ROW 1.46 COL 66.57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 18 FGCOLOR 15 
     WHorTransaccion AT ROW 1.46 COL 82 COLON-ALIGNED NO-LABEL
     Mov_InsSipla.Nit AT ROW 2.35 COL 7 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 18 FGCOLOR 15 
     WNomCliente AT ROW 2.35 COL 19.29 COLON-ALIGNED NO-LABEL
     Mov_InsSipla.Fecha_Gestion AT ROW 2.35 COL 66.57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 18 FGCOLOR 15 
     WHorGestion AT ROW 2.35 COL 82 COLON-ALIGNED NO-LABEL
     Mov_InsSipla.Id_NUM AT ROW 3.27 COL 9
          VIEW-AS TOGGLE-BOX
          SIZE 16 BY .65
          FONT 5
     Mov_InsSipla.Id_Exonerada AT ROW 3.65 COL 26.86
          LABEL "Normal"
          VIEW-AS TOGGLE-BOX
          SIZE 10 BY .77
          FONT 5
     Mov_InsSipla.Id_Sospechosa AT ROW 3.65 COL 40.29
          LABEL "Sospechosa"
          VIEW-AS TOGGLE-BOX
          SIZE 15.43 BY .77
          FONT 5
     Mov_InsSipla.Id_RepUIAF AT ROW 3.65 COL 56.72
          LABEL "Reportar UIAF"
          VIEW-AS TOGGLE-BOX
          SIZE 15.29 BY .77
          FONT 5
     Mov_InsSipla.Id_RepROSS AT ROW 3.65 COL 75
          LABEL "Reportar ROSS"
          VIEW-AS TOGGLE-BOX
          SIZE 18 BY .77
          FONT 5
     Mov_InsSipla.Id_NUD AT ROW 4 COL 9
          VIEW-AS TOGGLE-BOX
          SIZE 15 BY .65
          FONT 5
     Mov_InsSipla.Fec_RepUIAF AT ROW 4.77 COL 57 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 17 FGCOLOR 0 
     Mov_InsSipla.Fec_RepROSS AT ROW 4.77 COL 75 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 17 FGCOLOR 0 
     WDevuelto AT ROW 6.12 COL 51 COLON-ALIGNED NO-LABEL
     Btn_Trans AT ROW 7.08 COL 4
     Btn_Cliente AT ROW 7.08 COL 22
     Btn_Ahorros AT ROW 7.08 COL 40
     Btn_Creditos AT ROW 7.08 COL 58
     Btn_HojaVida AT ROW 7.08 COL 76
     Mov_InsSipla.Descripcion AT ROW 9.08 COL 3 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL LARGE
          SIZE 92 BY 3.5
          BGCOLOR 15 FONT 2
     Mov_InsSipla.Tipo_Registro AT ROW 13.12 COL 73 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Automatico", "A":U,
"Manual", "M":U
          SIZE 20 BY .81
     ConsEfectivo AT ROW 13.92 COL 10 COLON-ALIGNED NO-LABEL
     RetEfectivo AT ROW 13.92 COL 24 COLON-ALIGNED NO-LABEL
     AboCreditosEfe AT ROW 13.92 COL 38 COLON-ALIGNED NO-LABEL
     Mov_InsSipla.Valor_RegManual AT ROW 14.19 COL 76 COLON-ALIGNED
          LABEL "Valor Operacion Manual"
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 18 FGCOLOR 15 
     ConsCheque AT ROW 14.73 COL 10 COLON-ALIGNED NO-LABEL
     RetCheque AT ROW 14.73 COL 24 COLON-ALIGNED NO-LABEL
     AboCreditosChe AT ROW 14.73 COL 38 COLON-ALIGNED NO-LABEL
     Mov_InsSipla.UsuReporta AT ROW 15.54 COL 63 COLON-ALIGNED
          LABEL "Reporta"
          VIEW-AS FILL-IN 
          SIZE 4 BY .81
          BGCOLOR 18 FGCOLOR 15 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 2.88
         SIZE 98 BY 19.12
         BGCOLOR 17 FONT 4.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME FDetCon
     WNomUsuReporta AT ROW 15.54 COL 67.43 COLON-ALIGNED NO-LABEL
     Btn_Devolver AT ROW 16.35 COL 4
     Btn_Cerradas AT ROW 16.35 COL 26
     Mov_InsSipla.UsuGestiona AT ROW 16.5 COL 63 COLON-ALIGNED
          LABEL "Gestiona"
          VIEW-AS FILL-IN 
          SIZE 4 BY .81
          BGCOLOR 18 FGCOLOR 15 
     WNomUsuGestiona AT ROW 16.5 COL 67.43 COLON-ALIGNED NO-LABEL
     Btn_Pasar AT ROW 17.69 COL 4
     Btn_Salvar AT ROW 17.69 COL 26
     Btn_BorManual AT ROW 17.69 COL 49
     Btn_OcuDetCon AT ROW 17.69 COL 73
     "  Efectivo" VIEW-AS TEXT
          SIZE 8 BY .5 AT ROW 13.92 COL 4
          FGCOLOR 0 
     "  CONSIGNACIONES" VIEW-AS TEXT
          SIZE 15 BY .81 AT ROW 13.12 COL 10.57
          BGCOLOR 17 FGCOLOR 0 
     "  RETIROS" VIEW-AS TEXT
          SIZE 8.43 BY .81 AT ROW 13.12 COL 28
          BGCOLOR 17 FGCOLOR 0 
     "  Concepto emitido para la transaccion" VIEW-AS TEXT
          SIZE 92 BY .81 AT ROW 8.27 COL 3
          BGCOLOR 18 FGCOLOR 15 FONT 1
     "ABONO A CREDITOS" VIEW-AS TEXT
          SIZE 16.14 BY .5 AT ROW 13.27 COL 39
     "El registro es de tipo:" VIEW-AS TEXT
          SIZE 14 BY .5 AT ROW 13.23 COL 58.29
          FONT 4
     "  Informacion para el analisis de la transaccion" VIEW-AS TEXT
          SIZE 49 BY .81 AT ROW 6.12 COL 4
          BGCOLOR 18 FGCOLOR 15 FONT 1
     "Cheque" VIEW-AS TEXT
          SIZE 6 BY .5 AT ROW 14.73 COL 5
     RECT-302 AT ROW 3.46 COL 26
     RECT-324 AT ROW 12.85 COL 3
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 2.88
         SIZE 98 BY 19.12
         BGCOLOR 17 FONT 4
         TITLE "Detalle de las Transacciones que componen la instancia".

DEFINE FRAME FConsulta
     BConsulta AT ROW 1.54 COL 2
     Btn_RegManual AT ROW 16.35 COL 65
     BUTTON-205 AT ROW 16.62 COL 2
     ROrganizar AT ROW 17.46 COL 12.14 NO-LABEL
     Btn_Traspasar AT ROW 17.69 COL 65
     RECT-304 AT ROW 17.15 COL 11
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 2.88
         SIZE 98 BY 19.12
         BGCOLOR 17 FONT 4
         TITLE "Clientes Reportados en la Instancia".

DEFINE FRAME FReporte
     E1 AT ROW 1.54 COL 3 NO-LABEL
     BUTTON-203 AT ROW 17.69 COL 65
     BUTTON-204 AT ROW 17.69 COL 82
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 2.88
         SIZE 98 BY 19.12
         BGCOLOR 17 FONT 4
         TITLE "Reporte Generado".

DEFINE FRAME FInfoAhorros
     Btn_Alavista AT ROW 1.27 COL 3
     Btn_Contractual AT ROW 1.27 COL 26
     Btn_Atermino AT ROW 1.27 COL 49
     Btn_Aportes AT ROW 1.27 COL 72
     SInfoAhorros AT ROW 2.62 COL 3 NO-LABEL
     Btn_OcultarAhorros AT ROW 10.96 COL 78
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 9.62
         SIZE 96 BY 12.38
         BGCOLOR 17 FONT 4
         TITLE "Informacion de Ahorros".

DEFINE FRAME FCerradas
     BCerradas AT ROW 1.27 COL 3
     ECerradas AT ROW 5.31 COL 3 NO-LABEL
     BtnOcu_Cerradas AT ROW 10.96 COL 82
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 9.62
         SIZE 98 BY 12.38
         BGCOLOR 17 FONT 4
         TITLE "Instancias Cerradas".

DEFINE FRAME FManual
     Fecha_TransaccionM AT ROW 3.96 COL 72 COLON-ALIGNED
     NitM AT ROW 1.27 COL 8 COLON-ALIGNED
     WNomClienteM AT ROW 1.27 COL 21 COLON-ALIGNED NO-LABEL
     Id_NUMM AT ROW 2.35 COL 10
     Id_NUDM AT ROW 3.15 COL 10
     Id_ExoneradaM AT ROW 2.62 COL 30
     Id_SospechosaM AT ROW 2.62 COL 41
     Id_RepFiscaliaM AT ROW 2.62 COL 56
     Id_RepROSSM AT ROW 2.62 COL 74
     ValManual AT ROW 5.04 COL 72 COLON-ALIGNED
     DescripcionM AT ROW 6.12 COL 4 NO-LABEL
     Btn_SalvaManual AT ROW 8.81 COL 4
     Btn_OutManual AT ROW 8.81 COL 81
     RECT-323 AT ROW 2.35 COL 29
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 11.77
         SIZE 98 BY 10.23
         BGCOLOR 17 FONT 4
         TITLE "Creacion de un Registro Manual".

DEFINE FRAME FInfoCreditos
     Btn_Consumo AT ROW 1.27 COL 3
     Btn_Comercial AT ROW 1.27 COL 26
     Btn_Hipotecario AT ROW 1.27 COL 49
     Btn_Microcredito AT ROW 1.27 COL 72
     SInfoCreditos AT ROW 2.62 COL 4 NO-LABEL
     Btn_OcuCre AT ROW 10.96 COL 79
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 9.62
         SIZE 96 BY 12.38
         BGCOLOR 17 FONT 4
         TITLE "Informacion de Creditos".

DEFINE FRAME FFiltros
     RFiltros AT ROW 1.54 COL 5 NO-LABEL
     Btn_outfiltros AT ROW 11.77 COL 9
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 6.12
         SIZE 32 BY 13.19
         BGCOLOR 17 FONT 5
         TITLE "Filtro de Regsitros Mostrados".

DEFINE FRAME FInfoCliente
     Btn_Segmentacion AT ROW 1.27 COL 4
     BUTTON-186 AT ROW 1.27 COL 19
     BUTTON-187 AT ROW 1.27 COL 34
     Btn_Relaciones AT ROW 1.27 COL 49
     Btn_Fechas AT ROW 1.27 COL 64
     Btn_Controles AT ROW 1.27 COL 79
     SInfoCliente AT ROW 2.35 COL 3 NO-LABEL
     BUTTON-192 AT ROW 10.96 COL 79
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 9.62
         SIZE 96 BY 12.38
         BGCOLOR 17 FONT 4
         TITLE "Informacion del Cliente".

DEFINE FRAME FTransacciones
     BTransacciones AT ROW 2.08 COL 4
     BCreditos AT ROW 2.08 COL 56
     W_AboCreEfe AT ROW 9.38 COL 77 COLON-ALIGNED
     W_AboCreChe AT ROW 10.23 COL 77 COLON-ALIGNED
     W_ValConEfe AT ROW 10.69 COL 29 COLON-ALIGNED
     W_ValRetEfe AT ROW 10.69 COL 40 COLON-ALIGNED NO-LABEL
     BtnOcultar AT ROW 11.23 COL 79
     W_ValConChe AT ROW 11.5 COL 29 COLON-ALIGNED
     W_ValRetChe AT ROW 11.5 COL 40 COLON-ALIGNED NO-LABEL
     "  Depositos y Retiros a Cuentas de Ahorro" VIEW-AS TEXT
          SIZE 51 BY .81 AT ROW 1.27 COL 4
          BGCOLOR 18 FGCOLOR 15 FONT 1
     "  Sobrepasa Maximo Dia" VIEW-AS TEXT
          SIZE 18 BY .81 AT ROW 10.69 COL 4
          BGCOLOR 2 FGCOLOR 15 
     "  Abonos a Creditos" VIEW-AS TEXT
          SIZE 38 BY .81 AT ROW 1.27 COL 56
          BGCOLOR 18 FGCOLOR 15 FONT 1
     "  Sobrepasa Maximo Mes" VIEW-AS TEXT
          SIZE 18 BY .81 AT ROW 11.5 COL 4
          BGCOLOR 12 FGCOLOR 15 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 9.62
         SIZE 96 BY 12.38
         BGCOLOR 17 FONT 4
         TITLE "TRANSACCIONES HASTA LA FECHA DE REPORTE".

DEFINE FRAME FInfoHojaVida
     SInfoHojaVida AT ROW 1.54 COL 2 NO-LABEL
     Btn_OcuHV AT ROW 10.96 COL 80
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 9.35
         SIZE 96 BY 12.38
         BGCOLOR 17 FONT 4
         TITLE "Hoja de Vida".

DEFINE FRAME FTraspaso
     BTraspaso AT ROW 1.27 COL 2
     BUTTON-199 AT ROW 8.54 COL 13
     Btn_outtraspaso AT ROW 8.54 COL 30
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 54 ROW 9.35
         SIZE 46 BY 9.96
         BGCOLOR 17 FONT 4
         TITLE "Traspaso de Registros".

DEFINE FRAME FImprimir
     RInformes AT ROW 1.27 COL 3 NO-LABEL
     TIncluir AT ROW 7.81 COL 6
     FecCorte AT ROW 8.62 COL 31 COLON-ALIGNED
     BUTTON-200 AT ROW 9.96 COL 5
     BUTTON-201 AT ROW 9.96 COL 31
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 53 ROW 6.12
         SIZE 47 BY 11.31
         BGCOLOR 17 FONT 4
         TITLE "Impresion de Informes".


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
         TITLE              = "Gestion de Instancias de Lavado de Activos"
         HEIGHT             = 21.73
         WIDTH              = 113.57
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.14
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.14
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
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
ASSIGN FRAME FCerradas:FRAME = FRAME FSipla:HANDLE
       FRAME FConsulta:FRAME = FRAME FSipla:HANDLE
       FRAME FDetCon:FRAME = FRAME FSipla:HANDLE
       FRAME FFiltros:FRAME = FRAME FSipla:HANDLE
       FRAME FImprimir:FRAME = FRAME FSipla:HANDLE
       FRAME FInfoAhorros:FRAME = FRAME FSipla:HANDLE
       FRAME FInfoCliente:FRAME = FRAME FSipla:HANDLE
       FRAME FInfoCreditos:FRAME = FRAME FSipla:HANDLE
       FRAME FInfoHojaVida:FRAME = FRAME FSipla:HANDLE
       FRAME FManual:FRAME = FRAME FSipla:HANDLE
       FRAME FReporte:FRAME = FRAME FSipla:HANDLE
       FRAME FTransacciones:FRAME = FRAME FSipla:HANDLE
       FRAME FTraspaso:FRAME = FRAME FSipla:HANDLE.

/* SETTINGS FOR FRAME FCerradas
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB BCerradas 1 FCerradas */
ASSIGN 
       FRAME FCerradas:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FConsulta
                                                                        */
/* BROWSE-TAB BConsulta RECT-304 FConsulta */
/* SETTINGS FOR BUTTON Btn_RegManual IN FRAME FConsulta
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FDetCon
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FDetCon:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN AboCreditosChe IN FRAME FDetCon
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN AboCreditosEfe IN FRAME FDetCon
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Mov_InsSipla.Agencia IN FRAME FDetCon
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON Btn_Ahorros IN FRAME FDetCon
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Cliente IN FRAME FDetCon
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Creditos IN FRAME FDetCon
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_HojaVida IN FRAME FDetCon
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Trans IN FRAME FDetCon
   1                                                                    */
/* SETTINGS FOR FILL-IN ConsCheque IN FRAME FDetCon
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ConsEfectivo IN FRAME FDetCon
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Mov_InsSipla.Fecha_Gestion IN FRAME FDetCon
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Mov_InsSipla.Fecha_Transaccion IN FRAME FDetCon
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Mov_InsSipla.Fec_RepROSS IN FRAME FDetCon
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Mov_InsSipla.Fec_RepUIAF IN FRAME FDetCon
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR TOGGLE-BOX Mov_InsSipla.Id_Exonerada IN FRAME FDetCon
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Mov_InsSipla.Id_NUD IN FRAME FDetCon
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX Mov_InsSipla.Id_NUM IN FRAME FDetCon
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX Mov_InsSipla.Id_RepROSS IN FRAME FDetCon
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Mov_InsSipla.Id_RepUIAF IN FRAME FDetCon
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Mov_InsSipla.Id_Sospechosa IN FRAME FDetCon
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Mov_InsSipla.Nit IN FRAME FDetCon
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RetCheque IN FRAME FDetCon
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RetEfectivo IN FRAME FDetCon
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET Mov_InsSipla.Tipo_Registro IN FRAME FDetCon
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Mov_InsSipla.UsuGestiona IN FRAME FDetCon
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Mov_InsSipla.UsuReporta IN FRAME FDetCon
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Mov_InsSipla.Valor_RegManual IN FRAME FDetCon
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN WDevuelto IN FRAME FDetCon
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN WHorGestion IN FRAME FDetCon
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN WHorTransaccion IN FRAME FDetCon
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN WNomAgencia IN FRAME FDetCon
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN WNomCliente IN FRAME FDetCon
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN WNomUsuGestiona IN FRAME FDetCon
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN WNomUsuReporta IN FRAME FDetCon
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FFiltros
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FFiltros:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FImprimir
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FImprimir:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FInfoAhorros
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FInfoAhorros:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FInfoCliente
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FInfoCliente:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FInfoCreditos
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FInfoCreditos:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FInfoHojaVida
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FInfoHojaVida:HIDDEN           = TRUE.

ASSIGN 
       SInfoHojaVida:READ-ONLY IN FRAME FInfoHojaVida        = TRUE.

/* SETTINGS FOR FRAME FManual
   NOT-VISIBLE Custom                                                   */
ASSIGN 
       FRAME FManual:HIDDEN           = TRUE.

/* SETTINGS FOR EDITOR DescripcionM IN FRAME FManual
   NO-ENABLE 2                                                          */
/* SETTINGS FOR TOGGLE-BOX Id_ExoneradaM IN FRAME FManual
   NO-ENABLE 2                                                          */
/* SETTINGS FOR TOGGLE-BOX Id_NUDM IN FRAME FManual
   NO-ENABLE 2                                                          */
/* SETTINGS FOR TOGGLE-BOX Id_NUMM IN FRAME FManual
   NO-ENABLE 2                                                          */
/* SETTINGS FOR TOGGLE-BOX Id_RepFiscaliaM IN FRAME FManual
   NO-ENABLE 2                                                          */
/* SETTINGS FOR TOGGLE-BOX Id_RepROSSM IN FRAME FManual
   NO-ENABLE 2                                                          */
/* SETTINGS FOR TOGGLE-BOX Id_SospechosaM IN FRAME FManual
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN NitM IN FRAME FManual
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN ValManual IN FRAME FManual
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN WNomClienteM IN FRAME FManual
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FReporte
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FReporte:HIDDEN           = TRUE.

ASSIGN 
       E1:AUTO-RESIZE IN FRAME FReporte      = TRUE.

/* SETTINGS FOR FRAME FSipla
                                                                        */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME FReporte:MOVE-AFTER-TAB-ITEM (BUTTON-195:HANDLE IN FRAME FSipla)
       XXTABVALXX = FRAME FConsulta:MOVE-BEFORE-TAB-ITEM (BUTTON-196:HANDLE IN FRAME FSipla)
       XXTABVALXX = FRAME FFiltros:MOVE-AFTER-TAB-ITEM (BUTTON-197:HANDLE IN FRAME FSipla)
       XXTABVALXX = FRAME FManual:MOVE-BEFORE-TAB-ITEM (BtnDone:HANDLE IN FRAME FSipla)
       XXTABVALXX = FRAME FInfoAhorros:MOVE-BEFORE-TAB-ITEM (FRAME FManual:HANDLE)
       XXTABVALXX = FRAME FInfoCreditos:MOVE-BEFORE-TAB-ITEM (FRAME FInfoAhorros:HANDLE)
       XXTABVALXX = FRAME FInfoCliente:MOVE-BEFORE-TAB-ITEM (FRAME FInfoCreditos:HANDLE)
       XXTABVALXX = FRAME FTransacciones:MOVE-BEFORE-TAB-ITEM (FRAME FInfoCliente:HANDLE)
       XXTABVALXX = FRAME FCerradas:MOVE-BEFORE-TAB-ITEM (FRAME FTransacciones:HANDLE)
       XXTABVALXX = FRAME FTraspaso:MOVE-BEFORE-TAB-ITEM (FRAME FCerradas:HANDLE)
       XXTABVALXX = FRAME FInfoHojaVida:MOVE-BEFORE-TAB-ITEM (FRAME FTraspaso:HANDLE)
       XXTABVALXX = FRAME FImprimir:MOVE-BEFORE-TAB-ITEM (FRAME FInfoHojaVida:HANDLE)
       XXTABVALXX = FRAME FFiltros:MOVE-BEFORE-TAB-ITEM (FRAME FImprimir:HANDLE)
       XXTABVALXX = FRAME FDetCon:MOVE-BEFORE-TAB-ITEM (FRAME FConsulta:HANDLE)
       XXTABVALXX = FRAME FReporte:MOVE-BEFORE-TAB-ITEM (FRAME FDetCon:HANDLE)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR FILL-IN NomUsuario IN FRAME FSipla
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FTransacciones
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BTransacciones TEXT-2 FTransacciones */
/* BROWSE-TAB BCreditos BTransacciones FTransacciones */
ASSIGN 
       FRAME FTransacciones:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN W_AboCreChe IN FRAME FTransacciones
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_AboCreEfe IN FRAME FTransacciones
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_ValConChe IN FRAME FTransacciones
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_ValConEfe IN FRAME FTransacciones
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_ValRetChe IN FRAME FTransacciones
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_ValRetEfe IN FRAME FTransacciones
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FTraspaso
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BTraspaso 1 FTraspaso */
ASSIGN 
       FRAME FTraspaso:HIDDEN           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BCerradas
/* Query rebuild information for BROWSE BCerradas
     _START_FREEFORM
OPEN QUERY BCerradas FOR EACH TCerradas.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BCerradas */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BConsulta
/* Query rebuild information for BROWSE BConsulta
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TConsulta NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BConsulta */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BCreditos
/* Query rebuild information for BROWSE BCreditos
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TTransaccionesC.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BCreditos */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BTransacciones
/* Query rebuild information for BROWSE BTransacciones
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TTransacciones NO-LOCK BY TTransacciones.FecTran INDEXED-REPOSITION.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BTransacciones */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BTraspaso
/* Query rebuild information for BROWSE BTraspaso
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TUT.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BTraspaso */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FDetCon
/* Query rebuild information for FRAME FDetCon
     _TblList          = "bdcentral.Mov_InsSipla"
     _Query            is OPENED
*/  /* FRAME FDetCon */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FManual
/* Query rebuild information for FRAME FManual
     _TblList          = "bdcentral.Mov_InsSipla"
     _Query            is OPENED
*/  /* FRAME FManual */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Gestion de Instancias de Lavado de Activos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Gestion de Instancias de Lavado de Activos */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BCerradas
&Scoped-define SELF-NAME BCerradas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BCerradas wWin
ON MOUSE-SELECT-CLICK OF BCerradas IN FRAME FCerradas
DO:
  ECerradas:SCREEN-VALUE IN FRAME FCerradas = TCerradas.Descripcion.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BConsulta
&Scoped-define FRAME-NAME FConsulta
&Scoped-define SELF-NAME BConsulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BConsulta wWin
ON MOUSE-SELECT-DBLCLICK OF BConsulta IN FRAME FConsulta
DO:
  DISABLE Cmb_Instancias WITH FRAME FSipla.
  RUN Transacciones.
  FOR EACH TCerradas: DELETE TCerradas. END.
  FOR EACH Mov_InsSipla WHERE
           /*Mov_InsSipla.Agencia     EQ TConsulta.Agencia AND*/
           Mov_InsSipla.Nit         EQ TConsulta.Nit     AND
           Mov_InsSipla.CodAutoriza EQ TConsulta.PCod AND
           Mov_InsSipla.Estado      EQ YES NO-LOCK:
      CREATE TCerradas.
      BUFFER-COPY Mov_InsSipla TO TCerradas.
  END.
  FIND Mov_InsSipla WHERE
       Mov_InsSipla.Agencia     EQ TConsulta.Agencia AND
       Mov_InsSipla.Nit         EQ TConsulta.Nit     AND
       Mov_InsSipla.Instancia   EQ TConsulta.IAct AND
       Mov_InsSipla.CodAutoriza EQ TConsulta.PCod
       NO-LOCK NO-ERROR.
  IF AVAILABLE Mov_InsSipla THEN DO:
     DISPLAY Mov_InsSipla.Agencia Mov_InsSipla.Descripcion Mov_InsSipla.Fecha_Gestion
             Mov_InsSipla.Fecha_Transaccion Mov_InsSipla.Id_Exonerada Mov_InsSipla.Id_NUD
             Mov_InsSipla.Id_NUM Mov_InsSipla.Id_RepUIAF Mov_InsSipla.Id_Sospechosa
             Mov_InsSipla.Nit Mov_InsSipla.UsuGestiona Mov_InsSipla.UsuReporta
             Mov_InsSipla.Fec_RepUIAF Mov_InsSipla.Fec_RepROSS
             ConsEfectivo ConsCheque RetEfectivo RetCheque 
             AboCreditosEfe AboCreditosChe Mov_InsSipla.Valor_RegManual
             Mov_InsSipla.Tipo_Registro WITH FRAME FDetCon.
     ASSIGN WDevuelto:BGCOL = 18
            WDevuelto:FGCOL = 18
            WDevuelto:SCREEN-VALUE IN FRAME FDetCon = "".
     IF Mov_InsSipla.Instancia_Anterior NE 0 AND Mov_InsSipla.Instancia_Anterior GT TConsulta.IAct THEN DO:
        ASSIGN WDevuelto:BGCOL = 12
               WDevuelto:FGCOL = 15
               WDevuelto:SCREEN-VALUE IN FRAME FDetCon = "Devuelto por Contraloria. Revisar Cerradas".
     END.
     ASSIGN WHorTransaccion:SCREEN-VALUE = STRING(Mov_InsSipla.Hora_Transaccion,"HH:MM:SS am")
            WHorGestion:SCREEN-VALUE     = STRING(Mov_InsSipla.Hora_Gestion,"HH:MM:SS am").
     FIND Usuarios WHERE Usuarios.Usuario EQ Mov_InsSipla.UsuGestiona NO-LOCK NO-ERROR.
     IF AVAILABLE Usuarios THEN ASSIGN WNomUsuGestiona:SCREEN-VALUE = Usuarios.Nombre.
     FIND Usuarios WHERE Usuarios.Usuario EQ Mov_InsSipla.UsuReporta NO-LOCK NO-ERROR.
     IF AVAILABLE Usuarios THEN ASSIGN WNomUsuReporta:SCREEN-VALUE = Usuarios.Nombre.
     FIND Agencias WHERE Agencias.Agencia EQ Mov_InsSipla.Agencia NO-LOCK NO-ERROR.
     IF AVAILABLE Agencias THEN WNomAgencia:SCREEN-VALUE = Agencias.Nombre.
     WNomCliente:SCREEN-VALUE = TConsulta.Nombre.
  END.
  ELSE MESSAGE "oe" VIEW-AS ALERT-BOX.
  W_Ok = RInformes:DISABLE("Informe de los Registros de la Instancia") IN FRAME FImprimir.
  W_Ok = RInformes:ENABLE("Informe del Registro Actual") IN FRAME FImprimir.
  IF TConsulta.TipReg EQ "M" THEN
     ENABLE Btn_BorManual WITH FRAME FDetCon.
  ELSE
     DISABLE Btn_BorManual WITH FRAME FDetCon.
  VIEW FRAME FDetCon.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BConsulta wWin
ON ROW-DISPLAY OF BConsulta IN FRAME FConsulta
DO:
  IF TConsulta.Vigencia GT 2 THEN DO:
     ASSIGN TConsulta.Agencia:BGCOL IN BROWSE BConsulta = 12
            TConsulta.Nit:BGCOL      = 12
            TConsulta.Nombre:BGCOL   = 12
            TConsulta.Id_NUD:BGCOL   = 12
            TConsulta.Id_NUM:BGCOL   = 12
            TConsulta.SCons:BGCOL    = 12
            TConsulta.SReti:BGCOL    = 12
            TConsulta.SAbon:BGCOL    = 12
            TConsulta.FecRep:BGCOL   = 12
            /*TConsulta.HorRep:BGCOL = 12*/
            TConsulta.Vigencia:BGCOL = 12
            TConsulta.TipReg:BGCOL   = 12
            TConsulta.Agencia:FGCOL  = 15
            TConsulta.Nit:FGCOL      = 15
            TConsulta.Nombre:FGCOL   = 15
            TConsulta.Id_NUD:FGCOL   = 15
            TConsulta.Id_NUM:FGCOL   = 15
            TConsulta.SCons:FGCOL    = 15
            TConsulta.SReti:FGCOL    = 15
            TConsulta.SAbon:FGCOL    = 15
            TConsulta.FecRep:FGCOL   = 15
            /*TConsulta.HorRep:FGCOL = 12*/
            TConsulta.Vigencia:FGCOL = 15
            TConsulta.TipReg:FGCOL   = 15.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FSipla
&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone wWin
ON CHOOSE OF BtnDone IN FRAME FSipla /* Salir */
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


&Scoped-define FRAME-NAME FTransacciones
&Scoped-define SELF-NAME BtnOcultar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnOcultar wWin
ON CHOOSE OF BtnOcultar IN FRAME FTransacciones /* Ocultar */
DO:
  ENABLE {&List-1} WITH FRAME FDetCon.
  HIDE FRAME FTransacciones.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FCerradas
&Scoped-define SELF-NAME BtnOcu_Cerradas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnOcu_Cerradas wWin
ON CHOOSE OF BtnOcu_Cerradas IN FRAME FCerradas /* Ocultar */
DO:
  ENABLE {&List-1} WITH FRAME FDetCon.
  HIDE FRAME FCerradas.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FDetCon
&Scoped-define SELF-NAME Btn_Ahorros
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ahorros wWin
ON CHOOSE OF Btn_Ahorros IN FRAME FDetCon /* Ahorros */
DO:
DO WITH FRAME FInfoAhorros:
  FOR EACH TInfoAho: DELETE TInfoAho. END.
  DO i = 1 TO 100 BY 1:
      W_Ok = SInfoAhorros:DELETE(1).
  END.
  RUN Llenar_InfoAhorros.
  /*HIDE FRAME FInfoAhorros FRAME FInfoCreditos FRAME FInfoHojaVida FRAME FTransacciones FRAME FInfoCliente.*/
  DISABLE {&List-1} WITH FRAME FDetCon.
  VIEW FRAME FInfoAhorros.
  APPLY "choose" TO Btn_Alavista IN FRAME FInfoAhorros.
  DISPLAY SInfoAhorros.
END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FInfoAhorros
&Scoped-define SELF-NAME Btn_Alavista
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Alavista wWin
ON CHOOSE OF Btn_Alavista IN FRAME FInfoAhorros /* A la Vista */
DO:
  FRAME FInfoAhorros:TITLE = "Informacion Ahorros: A LA VISTA".
  DO i = 1 TO 100 BY 1:
     W_Ok = SInfoAhorros:DELETE(1).
  END.

  FOR EACH TInfoAho WHERE TInfoAho.ATip EQ 1 BREAK BY TInfoAho.AReg:
      W_Ok = SInfoAhorros:ADD-LAST(TInfoAho.ALin).
  END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Aportes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Aportes wWin
ON CHOOSE OF Btn_Aportes IN FRAME FInfoAhorros /* Aportes */
DO:
    FRAME FInfoAhorros:TITLE = "Informacion Ahorros: APORTES".
    DO i = 1 TO 100 BY 1:
       W_Ok = SInfoAhorros:DELETE(1).
    END.

    FOR EACH TInfoAho WHERE TInfoAho.ATip EQ 4 BREAK BY TInfoAho.AReg:
        W_Ok = SInfoAhorros:ADD-LAST(TInfoAho.ALin).
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Atermino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Atermino wWin
ON CHOOSE OF Btn_Atermino IN FRAME FInfoAhorros /* A Termino */
DO:
    FRAME FInfoAhorros:TITLE = "Informacion Ahorros: CONTRACTUAL".
    DO i = 1 TO 100 BY 1:
       W_Ok = SInfoAhorros:DELETE(1).
    END.

    FOR EACH TInfoAho WHERE TInfoAho.ATip EQ 3 BREAK BY TInfoAho.AReg:
        W_Ok = SInfoAhorros:ADD-LAST(TInfoAho.ALin).
    END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FDetCon
&Scoped-define SELF-NAME Btn_Cerradas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cerradas wWin
ON CHOOSE OF Btn_Cerradas IN FRAME FDetCon /* Ver Instancias Cerradas */
DO:
  FIND FIRST TCerradas NO-ERROR.
  IF NOT AVAILABLE TCerradas THEN
     MESSAGE "No existen instancias cerradas para esta transaccion" VIEW-AS ALERT-BOX.
  ELSE DO:
      DISABLE {&List-1} WITH FRAME FDetCon.
      ECerradas:SCREEN-VALUE IN FRAME FCerradas = "".
      OPEN QUERY BCerradas FOR EACH TCerradas.
      APPLY "mouse-select-click" TO BROWSE BCerradas.
      VIEW FRAME Fcerradas.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cliente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cliente wWin
ON CHOOSE OF Btn_Cliente IN FRAME FDetCon /* Cliente */
DO:
DO WITH FRAME FInfoCliente:
  FOR EACH TInfoCli: DELETE TInfoCli. END.
  DO i = 1 TO 100 BY 1:
      W_Ok = SInfocliente:DELETE(1).
  END.
  RUN Llenar_InfoCliente.
  /*HIDE FRAME FInfoAhorros FRAME FInfoCreditos FRAME FInfoHojaVida FRAME FTransacciones FRAME FInfoCliente.*/
  DISABLE {&List-1} WITH FRAME FDetCon.
  VIEW FRAME FInfoCliente.
  APPLY "choose" TO Btn_Segmentacion IN FRAME FInfoCliente.
  DISPLAY SInfoCliente.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FInfoCreditos
&Scoped-define SELF-NAME Btn_Comercial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Comercial wWin
ON CHOOSE OF Btn_Comercial IN FRAME FInfoCreditos /* Comercial */
DO:
  FRAME FInfoCreditos:TITLE = "Informacion Creditos: COMERCIAL".
  DO i = 1 TO 100 BY 1:
     W_Ok = SInfoCreditos:DELETE(1).
  END.

  FOR EACH TInfoCre WHERE TInfoCre.CTip EQ 2 BREAK BY TInfoCre.CReg:
      W_Ok = SInfoCreditos:ADD-LAST(TInfoCre.CLin).
  END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Consumo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Consumo wWin
ON CHOOSE OF Btn_Consumo IN FRAME FInfoCreditos /* Consumo */
DO:
  FRAME FInfoCreditos:TITLE = "Informacion Creditos: CONSUMO".
  DO i = 1 TO 100 BY 1:
     W_Ok = SInfoCreditos:DELETE(1).
  END.

  FOR EACH TInfoCre WHERE TInfoCre.CTip EQ 1 BREAK BY TInfoCre.CReg:
      W_Ok = SInfoCreditos:ADD-LAST(TInfoCre.CLin).
  END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FInfoAhorros
&Scoped-define SELF-NAME Btn_Contractual
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Contractual wWin
ON CHOOSE OF Btn_Contractual IN FRAME FInfoAhorros /* Contractual */
DO:
    FRAME FInfoAhorros:TITLE = "Informacion Ahorros: CONTRACTUAL".
    DO i = 1 TO 100 BY 1:
       W_Ok = SInfoAhorros:DELETE(1).
    END.

    FOR EACH TInfoAho WHERE TInfoAho.ATip EQ 2 BREAK BY TInfoAho.AReg:
        W_Ok = SInfoAhorros:ADD-LAST(TInfoAho.ALin).
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FInfoCliente
&Scoped-define SELF-NAME Btn_Controles
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Controles wWin
ON CHOOSE OF Btn_Controles IN FRAME FInfoCliente /* Controles */
DO:
  FRAME FInfoCliente:TITLE = "Informacion del Cliente: CONTROLES".
  DO i = 1 TO 100 BY 1:
     W_Ok = SInfocliente:DELETE(1).
  END.

  FOR EACH TInfoCli WHERE TInfoCli.Tip EQ 6 BREAK BY TInfoCli.Reg:
      W_Ok = SInfoCliente:ADD-LAST(TInfoCli.Lin).
  END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FDetCon
&Scoped-define SELF-NAME Btn_Creditos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Creditos wWin
ON CHOOSE OF Btn_Creditos IN FRAME FDetCon /* Creditos */
DO:
DO WITH FRAME FInfoCreditos:
  FOR EACH TInfoCre: DELETE TInfoCre. END.
  DO i = 1 TO 100 BY 1:
      W_Ok = SInfoCreditos:DELETE(1).
  END.
  RUN Llenar_InfoCreditos.
  /*HIDE FRAME FInfoAhorros FRAME FInfoCreditos FRAME FInfoHojaVida FRAME FTransacciones FRAME FInfoCliente.*/
  DISABLE {&List-1} WITH FRAME FDetCon.
  VIEW FRAME FInfoCreditos.
  APPLY "choose" TO Btn_Consumo IN FRAME FInfoCreditos.
  DISPLAY SInfoCreditos.
END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Devolver
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Devolver wWin
ON CHOOSE OF Btn_Devolver IN FRAME FDetCon /* Devolver Una Instancia */
DO:
  IF orden_inspan EQ 1 THEN
     MESSAGE "Esta es la primera instancia." SKIP
             "La instancia no puede devolverse mas!" VIEW-AS ALERT-BOX INFORMATION.
  ELSE DO:
     APPLY "choose" TO Btn_Salvar IN FRAME FDetCon.
     IF LENGTH(Mov_InsSipla.Descripcion) LT 10 THEN DO:
        MESSAGE "No se puede devolver una instancia sin antes" SKIP
                "haber dado un concepto que oriente al" SKIP
                "usuario de la instancia anterior para que" SKIP
                "investigue o pida algun documento al cliente"
            VIEW-AS ALERT-BOX WARNING.
        APPLY "entry" TO Mov_InsSipla.Descripcion.
        RETURN NO-APPLY.
     END.
     FIND CURRENT Mov_InsSipla NO-ERROR.
     IF AVAILABLE Mov_InsSipla THEN DO: /*inactiva la actual*/
        ASSIGN Mov_InsSipla.Estado        = YES
               Mov_InsSipla.Fecha_Gestion = W_Fecha
               Mov_InsSipla.Hora_Gestion  = TIME.
     END.
     FIND Mov_InsSipla WHERE
          Mov_InsSipla.Instancia   EQ TConsulta.IAnt AND
          Mov_InsSipla.Nit         EQ TConsulta.Nit AND
          Mov_InsSipla.CodAutoriza EQ TConsulta.PCod AND 
          Mov_InsSipla.Estado      EQ YES NO-ERROR.
     IF AVAILABLE Mov_InsSipla THEN DO:
        IF orden_inspan EQ 2 THEN
           FIND FIRST Cfg_Instancias WHERE
                   Cfg_Instancias.Agencia        EQ Mov_InsSipla.Agencia  AND
                   Cfg_Instancias.Tipo_Instancia EQ 6          AND
                   Cfg_Instancias.Instancia      EQ IAnt       AND
                   Cfg_Instancias.Estado         EQ 1   NO-LOCK NO-ERROR.
        IF orden_inspan EQ 3 THEN
           FIND FIRST Cfg_Instancias WHERE
                   Cfg_Instancias.Tipo_Instancia EQ 6          AND
                   Cfg_Instancias.Instancia      EQ IAnt       AND
                   Cfg_Instancias.Estado         EQ 1   NO-LOCK NO-ERROR.
        IF AVAILABLE Cfg_Instancias THEN DO:
           IF Cfg_Instancias.Usuario NE Mov_InsSipla.UsuGestiona THEN DO: /*si el que gestiona la instancia no es
              el mimo que la gestiono anteriormente, se asigna la instancia al vigente y se pone una
              nota en la descripcion de quien anteriormente la gestiono*/
              MESSAGE "El Usuario que inicialmente gestiono la instancia" SKIP
                      "a la cual se devolvera, ya no esta activo para gestionar" SKIP
                      "estas instancias. El nuevo usuario que gestionara la" SKIP
                      "informacion es el: " Cfg_Instancias.Usuario
              VIEW-AS ALERT-BOX INFORMATION.
              ASSIGN Mov_InsSipla.Descripcion = Mov_InsSipla.Descripcion + " (UAG:" + Mov_InsSipla.UsuGestiona + 
                  " hasta " + STRING(W_Fecha) + ") "
                     Mov_InsSipla.UsuGestion = Cfg_Instancias.Usuario.
           END.
           ASSIGN Mov_InsSipla.Estado = NO
                  Mov_InsSipla.Fecha_Gestion = ?
                  Mov_InsSipla.Hora_Gestion  = 0.
           IF orden_inspan EQ 2 THEN
              Mov_InsSipla.Instancia_Anterior = TConsulta.IAct.
        END.
     END.
     ELSE DO:
         MESSAGE "No se encontro el movimiento de instancia anterior"
             VIEW-AS ALERT-BOX ERROR.
     END.
     APPLY "choose" TO Btn_OcuDetCon IN FRAME FDetCon.
     APPLY "value-changed" TO Cmb_Instancias IN FRAME FSipla.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FInfoCliente
&Scoped-define SELF-NAME Btn_Fechas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Fechas wWin
ON CHOOSE OF Btn_Fechas IN FRAME FInfoCliente /* Fechas */
DO:
  FRAME FInfoCliente:TITLE = "Informacion del Cliente: FECHAS".
  DO i = 1 TO 100 BY 1:
     W_Ok = SInfocliente:DELETE(1).
  END.

  FOR EACH TInfoCli WHERE TInfoCli.Tip EQ 5 BREAK BY TInfoCli.Reg:
      W_Ok = SInfoCliente:ADD-LAST(TInfoCli.Lin).
  END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FInfoCreditos
&Scoped-define SELF-NAME Btn_Hipotecario
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Hipotecario wWin
ON CHOOSE OF Btn_Hipotecario IN FRAME FInfoCreditos /* Hipotecario */
DO:
    FRAME FInfoCreditos:TITLE = "Informacion Creditos: HIPOTECARIO".
    DO i = 1 TO 100 BY 1:
       W_Ok = SInfoCreditos:DELETE(1).
    END.

    FOR EACH TInfoCre WHERE TInfoCre.CTip EQ 3 BREAK BY TInfoCre.CReg:
        W_Ok = SInfoCreditos:ADD-LAST(TInfoCre.CLin).
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FDetCon
&Scoped-define SELF-NAME Btn_HojaVida
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_HojaVida wWin
ON CHOOSE OF Btn_HojaVida IN FRAME FDetCon /* Hoja de Vida */
DO:
DO WITH FRAME FInfoHojaVida:
/*  DO i = 1 TO 500 BY 1:
      W_Ok = SInfoHojaVida:DELETE(1).
  END.*/
  SInfoHojaVida        = "".
  RUN Llenar_InfoHojaVida.
  /*MESSAGE arc.*/
  W_ok = SInfoHojaVida:READ-FILE(Arc).
  /*HIDE FRAME FInfoAhorros FRAME FInfoCreditos FRAME FInfoHojaVida FRAME FTransacciones FRAME FInfoCliente.*/
  DISABLE {&List-1} WITH FRAME FDetCon.
  VIEW FRAME FInfoHojaVida.
END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FInfoCreditos
&Scoped-define SELF-NAME Btn_Microcredito
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Microcredito wWin
ON CHOOSE OF Btn_Microcredito IN FRAME FInfoCreditos /* Microcredito */
DO:
    FRAME FInfoCreditos:TITLE = "Informacion Creditos: MICROCREDITO".
    DO i = 1 TO 100 BY 1:
       W_Ok = SInfoCreditos:DELETE(1).
    END.

    FOR EACH TInfoCre WHERE TInfoCre.CTip EQ 4 BREAK BY TInfoCre.CReg:
        W_Ok = SInfoCreditos:ADD-LAST(TInfoCre.CLin).
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OcuCre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OcuCre wWin
ON CHOOSE OF Btn_OcuCre IN FRAME FInfoCreditos /* Ocultar */
DO:
  ENABLE {&List-1} WITH FRAME FDetCon.
  HIDE FRAME FInfoCreditos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FDetCon
&Scoped-define SELF-NAME Btn_OcuDetCon
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OcuDetCon wWin
ON CHOOSE OF Btn_OcuDetCon IN FRAME FDetCon /* Consultar otro */
DO:
  W_Ok = RInformes:ENABLE("Informe de los Registros de la Instancia") IN FRAME FImprimir.
  W_Ok = RInformes:DISABLE("Informe del Registro Actual") IN FRAME FImprimir.
  ENABLE Cmb_Instancias WITH FRAME FSipla.
  HIDE FRAME FDetCon.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FInfoHojaVida
&Scoped-define SELF-NAME Btn_OcuHV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OcuHV wWin
ON CHOOSE OF Btn_OcuHV IN FRAME FInfoHojaVida /* Ocultar */
DO:
  ENABLE {&List-1} WITH FRAME FDetCon.
  HIDE FRAME FInfoHojaVida.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FInfoAhorros
&Scoped-define SELF-NAME Btn_OcultarAhorros
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OcultarAhorros wWin
ON CHOOSE OF Btn_OcultarAhorros IN FRAME FInfoAhorros /* Ocultar */
DO:
  ENABLE {&List-1} WITH FRAME FDetCon.
  HIDE FRAME FInfoAhorros.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FFiltros
&Scoped-define SELF-NAME Btn_outfiltros
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_outfiltros wWin
ON CHOOSE OF Btn_outfiltros IN FRAME FFiltros /* Ocultar */
DO:
  HIDE FRAME FFiltros.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FManual
&Scoped-define SELF-NAME Btn_OutManual
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OutManual wWin
ON CHOOSE OF Btn_OutManual IN FRAME FManual /* Ocultar */
DO:
  APPLY "value-changed" TO Cmb_Instancias IN FRAME FSipla.
  HIDE FRAME FManual.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FTraspaso
&Scoped-define SELF-NAME Btn_outtraspaso
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_outtraspaso wWin
ON CHOOSE OF Btn_outtraspaso IN FRAME FTraspaso /* Ocultar */
DO:
  HIDE FRAME FTraspaso.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FDetCon
&Scoped-define SELF-NAME Btn_Pasar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Pasar wWin
ON CHOOSE OF Btn_Pasar IN FRAME FDetCon /* Pasar a Siguiente Instancia */
DO:
  DEFINE VAR HoraT AS INTEGER FORMAT "999999".
  DEFINE VAR AgeOri LIKE Agencias.Agencia.
  DEFINE VAR IdTras AS LOGICAL.
  DEFINE VAR UsuCaj LIKE Usuarios.Usuario.
  FIND CURRENT Mov_InsSipla NO-ERROR.
  IF Mov_InsSipla.Descripcion:SCREEN-VALUE IN FRAME FDetCon EQ "" THEN DO:
     MESSAGE "Para poder pasar a la siguiente instancia debe darse" SKIP
             "un concepto acerca de la transaccion!" VIEW-AS ALERT-BOX.
     APPLY "entry" TO Mov_InsSipla.Descripcion IN FRAME FDetCon.
     RETURN NO-APPLY.
  END. /*IF Mov_InsSipla.Descripcion:SCREEN-VALUE IN FRAME FDetCon EQ "" */
  ELSE DO:
     FIND Instancias WHERE Instancias.Tipo_Instancia EQ 6 AND
                           Instancias.Orden_Instancia EQ Orden_InsPan + 1 AND
                           Instancias.Estado EQ 1 NO-ERROR.
     IF AVAILABLE Instancias THEN DO:
         APPLY "choose" TO Btn_Salvar IN FRAME FDetCon.
         FIND CURRENT Mov_InsSipla.
         FIND FIRST Cfg_Instancias WHERE
              /*Cfg_Instancias.Agencia        EQ W_Agencia AND*/
              Cfg_Instancias.Tipo_Instancia EQ 6 AND
              Cfg_Instancias.Instancia      EQ Instancias.Instancia AND
              Cfg_Instancias.Estado         EQ 1 NO-LOCK NO-ERROR.
         IF NOT AVAILABLE Cfg_Instancias THEN DO:
            MESSAGE "Ha ocurrido un error al tratar de buscar" SKIP
                    "el usuario que tiene a su cargo la instancia:" SKIP
                    Instancias.Instancia Instancias.Nom_Instancia VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
         END.
         ASSIGN Mov_InsSipla.Fecha_Gestion = W_Fecha
                Mov_InsSipla.Hora_Gestion  = TIME
                Mov_InsSipla.Estado        = YES
                Mov_InsSipla.UsuGestiona   = W_Usuario
                /*AgeOri                     = Mov_InsSipla.AgeOrigen*/
                HoraT                      = Mov_InsSipla.Hora_Transaccion
                IdTras                     = Mov_InsSipla.Id_Traslado
                UsuCaj                     = Mov_InsSipla.UsuCajero.
         FIND Mov_InsSipla WHERE
              Mov_InsSipla.Instancia   EQ Instancias.Instancia AND
              Mov_InsSipla.Nit         EQ TConsulta.Nit AND
              Mov_InsSipla.CodAutoriza EQ TConsulta.PCod NO-ERROR.
         IF NOT AVAILABLE Mov_InsSipla THEN DO:
            CREATE Mov_InsSipla.
            ASSIGN Mov_InsSipla.Agencia            = INTEGER(Mov_InsSipla.Agencia:SCREEN-VALUE)
                   Mov_InsSipla.Instancia          = Instancias.Instancia
                   Mov_InsSipla.Instancia_Anterior = INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME FSipla,1,5))
                   Mov_InsSipla.Nit                = Mov_InsSipla.Nit:SCREEN-VALUE IN FRAME FDetCon
                   /*Mov_InsSipla.AgeOrigen          = AgeOri*/
                   Mov_InsSipla.CodAutoriza        = TConsulta.PCod
                   Mov_InsSipla.Descripcion        = ""
                   Mov_InsSipla.Estado             = NO
                   Mov_InsSipla.Fecha_Gestion      = ?
                   Mov_InsSipla.Fecha_Transaccion  = DATE(Mov_InsSipla.Fecha_Transaccion:SCREEN-VALUE)
                   Mov_InsSipla.Hora_Gestion       = 0
                   Mov_InsSipla.Hora_Transaccion   = HoraT
                   Mov_InsSipla.Id_Exonerada       = LOGICAL(Mov_InsSipla.Id_Exonera:SCREEN-VALUE)
                   Mov_InsSipla.Id_NUD             = LOGICAL(Mov_InsSipla.Id_NUD:SCREEN-VALUE)
                   Mov_InsSipla.Id_NUM             = LOGICAL(Mov_InsSipla.Id_NUM:SCREEN-VALUE)
                   Mov_InsSipla.Id_RepUIAF         = LOGICAL(Mov_InsSipla.Id_RepUIAF:SCREEN-VALUE)
                   Mov_InsSipla.Id_Sospechosa      = LOGICAL(Mov_InsSipla.Id_Sospechosa:SCREEN-VALUE)
                   Mov_InsSipla.Id_Traslado        = IdTras
                   Mov_InsSipla.UsuCajero          = UsuCaj
                   Mov_InsSipla.UsuGestiona        = Cfg_Instancias.Usuario
                   Mov_InsSipla.UsuReporta         = Mov_InsSipla.UsuReporta:SCREEN-VALUE
                   Mov_InsSipla.Tipo_Registro      = Mov_InsSipla.Tipo_Registro:SCREEN-VALUE
                   Mov_InsSipla.Valor_RegManual    = DECIMAL(Mov_InsSipla.Valor_RegManual:SCREEN-VALUE).
                   
         END. /*IF NOT AVAILABLE Mov_InsSipla */
         ELSE
             ASSIGN Mov_InsSipla.Estado = NO
                    Mov_InsSipla.Fecha_Gestion = ?
                    Mov_InsSipla.Hora_Gestion  = 0
                    Mov_InsSipla.UsuGestiona   = Cfg_Instancias.Usuario.

         APPLY "choose" TO Btn_OcuDetCon IN FRAME FDetCon.
         APPLY "value-changed" TO Cmb_Instancias IN FRAME FSipla.
     END. /*IF AVAILABLE Instancias*/
     ELSE MESSAGE "no encontro la instancia que seguia".
  END. /*ELSE DO:*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FConsulta
&Scoped-define SELF-NAME Btn_RegManual
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_RegManual wWin
ON CHOOSE OF Btn_RegManual IN FRAME FConsulta /* Adicionar Registro Manual */
DO:
DO WITH FRAME FManual:
  ASSIGN NitM:SCREEN-VALUE            = ""
         WNomClienteM:SCREEN-VALUE    = ""
         Id_NumM:SCREEN-VALUE         = "no"
         Id_NudM:SCREEN-VALUE         = "no"
         Id_ExoneradaM:SCREEN-VALUE   = "no"
         Id_SospechosaM:SCREEN-VALUE  = "no"
         Id_RepFiscaliaM:SCREEN-VALUE = "no"
         Id_RepROSSM:SCREEN-VALUE     = "no"
         DescripcionM:SCREEN-VALUE    = ""
         ValManual:SCREEN-VALUE       = "0".
  ENABLE {&List-2}.
  VIEW FRAME FManual.
  APPLY "entry" TO NitM.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FInfoCliente
&Scoped-define SELF-NAME Btn_Relaciones
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Relaciones wWin
ON CHOOSE OF Btn_Relaciones IN FRAME FInfoCliente /* Relaciones */
DO:
  FRAME FInfoCliente:TITLE = "Informacion del Cliente: RELACIONES".
  DO i = 1 TO 100 BY 1:
     W_Ok = SInfocliente:DELETE(1).
  END.

  FOR EACH TInfoCli WHERE TInfoCli.Tip EQ 4 BREAK BY TInfoCli.Reg:
      W_Ok = SInfoCliente:ADD-LAST(TInfoCli.Lin).
  END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FManual
&Scoped-define SELF-NAME Btn_SalvaManual
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_SalvaManual wWin
ON CHOOSE OF Btn_SalvaManual IN FRAME FManual /* Salvar */
DO:
DO WITH FRAME FManual:
  IF LENGTH(DescripcionM:SCREEN-VALUE) LT 10 THEN DO:
     MESSAGE "Para poder ingresar un registro manual debe ingresarse" SKIP
             "una descripcion de la operacion realizada de no menos" SKIP
             "de 10 caracteres" SKIP(1)
             "No se permite la creacion de la operacion manual"
             VIEW-AS ALERT-BOX ERROR.
     APPLY "choose" TO Btn_OutManual.
  END.
  IF DECIMAL(ValManual:SCREEN-VALUE) EQ 0 THEN DO:
     MESSAGE "Para poder ingresar un registro manual debe ingresarse" SKIP
             "un valor de la operacion que se esta realizando" SKIP(1)
             "No se permite la creacion de la operacion manual"
             VIEW-AS ALERT-BOX ERROR.
     APPLY "choose" TO Btn_OutManual.
  END.
  FIND Mov_InsSipla WHERE
       Mov_InsSipla.Agencia            EQ W_Agencia AND
       Mov_InsSipla.Instancia          EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME FSipla,1,5)) AND
       Mov_InsSipla.Nit                EQ NitM:SCREEN-VALUE AND
       Mov_InsSipla.Fecha_Transaccion  EQ DATE(Fecha_TransaccionM:SCREEN-VALUE) AND 
       Mov_InsSipla.Tipo_Registro      EQ "M" NO-ERROR.
  IF AVAILABLE Mov_InsSipla THEN DO: /*Si tiene una instancia manual abierta. adiciona el valor y la descripcion*/
     IF NOT Mov_InsSipla.Estado THEN DO:
         MESSAGE "Ya existe un registro manual para este nit" SKIP
                 "en esta instancia, en esta fecha"           SKIP(1)
                 "no se creara un nuevo registro si no que"   SKIP
                 "se adicionara la descripcion y el valor"    SKIP
                 "a los del registro existente"
                 VIEW-AS ALERT-BOX INFORMATION.
         ASSIGN Mov_InsSipla.Valor_RegManual    = Mov_InsSipla.Valor_RegManual + DECIMAL(ValManual:SCREEN-VALUE)
                Mov_InsSipla.Descripcion        = Mov_InsSipla.Descripcion + " . " + DescripcionM:SCREEN-VALUE.
     END.
     ELSE DO:
         MESSAGE "Un registro manual para este nit, ha sido ya procesado" SKIP
                 "para la siguiente instancia. Solo se puede ingresar un" SKIP
                 "registro manual por agencia, nit, instancia." SKIP(1)
                 "No se permite la operacion de creacion manual"
                 VIEW-AS ALERT-BOX ERROR.
     END.
  END.
  ELSE DO:
      CREATE Mov_InsSipla.
      ASSIGN Mov_InsSipla.Agencia            = W_Agencia
             Mov_InsSipla.CodAutoriza        = NEXT-VALUE(Sec_Autorizacion)
             Mov_InsSipla.Descripcion        = DescripcionM:SCREEN-VALUE
             Mov_InsSipla.Estado             = NO
             Mov_InsSipla.Fecha_Transaccion  = DATE(Fecha_TransaccionM:SCREEN-VALUE)
             Mov_InsSipla.Hora_Transaccion   = TIME
             Mov_InsSipla.Id_Exonerada       = LOGICAL(Id_ExoneradaM:SCREEN-VALUE)
             Mov_InsSipla.Id_NUD             = LOGICAL(Id_NuDM:SCREEN-VALUE)
             Mov_InsSipla.Id_NUM             = LOGICAL(Id_NumM:SCREEN-VALUE)
             Mov_InsSipla.Id_RepUIAF         = LOGICAL(Id_RepFiscaliaM:SCREEN-VALUE)
             Mov_InsSipla.Id_RepROSS         = LOGICAL(Id_RepRossM:SCREEN-VALUE)
             Mov_InsSipla.Id_Sospechosa      = LOGICAL(Id_SospechosaM:SCREEN-VALUE)
             Mov_InsSipla.Instancia          = INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME FSipla,1,5))
             Mov_InsSipla.Nit                = NitM:SCREEN-VALUE
             Mov_InsSipla.Tipo_Registro      = "M"
             Mov_InsSipla.UsuReporta         = W_Usuario
             Mov_InsSipla.UsuCajero          = W_Usuario
             Mov_InsSipla.UsuGestiona        = W_Usuario
             Mov_InsSipla.Valor_RegManual    = DECIMAL(ValManual:SCREEN-VALUE).
  END.
END.
APPLY "choose" TO Btn_OutManual.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FDetCon
&Scoped-define SELF-NAME Btn_Salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salvar wWin
ON CHOOSE OF Btn_Salvar IN FRAME FDetCon /* Salvar Modificaciones */
DO:
  FIND CURRENT Mov_InsSipla NO-ERROR.
  DO WITH FRAME FDetCon:
     ASSIGN Mov_InsSipla.Descripcion
            Mov_InsSipla.Id_Exonerada
            Mov_InsSipla.Id_Sospechosa
            Mov_InsSipla.Id_RepUIAF
            Mov_InsSipla.Id_RepROSS.
     ASSIGN TConsulta.IdNor = Mov_InsSipla.Id_Exonerada
            TConsulta.IdSos = Mov_InsSipla.Id_Sospechosa
            TConsulta.IdUIAF = Mov_InsSipla.Id_RepUIAF
            TConsulta.IdROSS = Mov_InsSipla.Id_RepROSS.

  END.
  FIND CURRENT Mov_InsSipla NO-LOCK NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FInfoCliente
&Scoped-define SELF-NAME Btn_Segmentacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Segmentacion wWin
ON CHOOSE OF Btn_Segmentacion IN FRAME FInfoCliente /* Segmentacion */
DO:
  FRAME FInfoCliente:TITLE = "Informacion del Cliente: SEGMENTACION".
  DO i = 1 TO 100 BY 1:
     W_Ok = SInfocliente:DELETE(1).
  END.

  FOR EACH TInfoCli WHERE TInfoCli.Tip EQ 1 BREAK BY TInfoCli.Reg:
      W_Ok = SInfoCliente:ADD-LAST(TInfoCli.Lin).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FDetCon
&Scoped-define SELF-NAME Btn_Trans
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Trans wWin
ON CHOOSE OF Btn_Trans IN FRAME FDetCon /* Transacciones */
DO:
  DISABLE {&List-1} WITH FRAME FDetCon.
  VIEW FRAME FTransacciones.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FConsulta
&Scoped-define SELF-NAME Btn_Traspasar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Traspasar wWin
ON CHOOSE OF Btn_Traspasar IN FRAME FConsulta /* Pasar todos los registros a otro usuario */
DO:
  OPEN QUERY BTraspaso FOR EACH TUT.
  VIEW FRAME FTraspaso.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BTransacciones
&Scoped-define FRAME-NAME FTransacciones
&Scoped-define SELF-NAME BTransacciones
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTransacciones wWin
ON ROW-DISPLAY OF BTransacciones IN FRAME FTransacciones
DO:
  IF TTransacciones.BddTran THEN
     ASSIGN TTransacciones.AgeTran:BGCOL IN BROWSE BTransacciones = 2
            TTransacciones.CodTran:BGCOL IN BROWSE BTransacciones = 2
            TTransacciones.CueTran:BGCOL IN BROWSE BTransacciones = 2
            TTransacciones.ConTran:BGCOL IN BROWSE BTransacciones = 2
            TTransacciones.RetTran:BGCOL IN BROWSE BTransacciones = 2
            TTransacciones.FecTran:BGCOL IN BROWSE BTransacciones = 2
            TTransacciones.EfeChe:BGCOL IN BROWSE BTransacciones = 2
            TTransacciones.AgeTran:FGCOL IN BROWSE BTransacciones = 15
            TTransacciones.CodTran:FGCOL IN BROWSE BTransacciones = 15
            TTransacciones.CueTran:FGCOL IN BROWSE BTransacciones = 15
            TTransacciones.ConTran:FGCOL IN BROWSE BTransacciones = 15
            TTransacciones.RetTran:FGCOL IN BROWSE BTransacciones = 15
            TTransacciones.FecTran:FGCOL IN BROWSE BTransacciones = 15
            TTransacciones.EfeChe:FGCOL IN BROWSE BTransacciones = 15.
  IF TTransacciones.BdmTran THEN
     ASSIGN TTransacciones.AgeTran:BGCOL IN BROWSE BTransacciones = 12
            TTransacciones.CodTran:BGCOL IN BROWSE BTransacciones = 12
            TTransacciones.CueTran:BGCOL IN BROWSE BTransacciones = 12
            TTransacciones.ConTran:BGCOL IN BROWSE BTransacciones = 12
            TTransacciones.RetTran:BGCOL IN BROWSE BTransacciones = 12
            TTransacciones.FecTran:BGCOL IN BROWSE BTransacciones = 12
            TTransacciones.EfeChe:BGCOL IN BROWSE BTransacciones = 12
            TTransacciones.AgeTran:FGCOL IN BROWSE BTransacciones = 15
            TTransacciones.CodTran:FGCOL IN BROWSE BTransacciones = 15
            TTransacciones.CueTran:FGCOL IN BROWSE BTransacciones = 15
            TTransacciones.ConTran:FGCOL IN BROWSE BTransacciones = 15
            TTransacciones.RetTran:FGCOL IN BROWSE BTransacciones = 15
            TTransacciones.FecTran:FGCOL IN BROWSE BTransacciones = 15
            TTransacciones.EfeChe:FGCOL IN BROWSE BTransacciones = 15.
  IF NOT TTransacciones.BddTran AND NOT Ttransacciones.BdmTran THEN
     ASSIGN TTransacciones.AgeTran:BGCOL IN BROWSE BTransacciones = 15
            TTransacciones.CodTran:BGCOL IN BROWSE BTransacciones = 15
            TTransacciones.CueTran:BGCOL IN BROWSE BTransacciones = 15
            TTransacciones.ConTran:BGCOL IN BROWSE BTransacciones = 15
            TTransacciones.RetTran:BGCOL IN BROWSE BTransacciones = 15
            TTransacciones.FecTran:BGCOL IN BROWSE BTransacciones = 15
            TTransacciones.EfeChe:BGCOL IN BROWSE BTransacciones = 15
            TTransacciones.AgeTran:FGCOL IN BROWSE BTransacciones = 0
            TTransacciones.CodTran:FGCOL IN BROWSE BTransacciones = 0
            TTransacciones.CueTran:FGCOL IN BROWSE BTransacciones = 0
            TTransacciones.ConTran:FGCOL IN BROWSE BTransacciones = 0
            TTransacciones.RetTran:FGCOL IN BROWSE BTransacciones = 0
            TTransacciones.FecTran:FGCOL IN BROWSE BTransacciones = 0
            TTransacciones.EfeChe:FGCOL IN BROWSE BTransacciones = 0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FInfoCliente
&Scoped-define SELF-NAME BUTTON-186
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-186 wWin
ON CHOOSE OF BUTTON-186 IN FRAME FInfoCliente /* Ubicacion */
DO:
  FRAME FInfoCliente:TITLE = "Informacion del Cliente: UBICACION".
  DO i = 1 TO 100 BY 1:
     W_Ok = SInfocliente:DELETE(1).
  END.

  FOR EACH TInfoCli WHERE TInfoCli.Tip EQ 2 BREAK BY TInfoCli.Reg:
      W_Ok = SInfoCliente:ADD-LAST(TInfoCli.Lin).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-187
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-187 wWin
ON CHOOSE OF BUTTON-187 IN FRAME FInfoCliente /* Economica */
DO:
  FRAME FInfoCliente:TITLE = "Informacion del Cliente: ECONOMICA".
  DO i = 1 TO 100 BY 1:
     W_Ok = SInfocliente:DELETE(1).
  END.

  FOR EACH TInfoCli WHERE TInfoCli.Tip EQ 3 BREAK BY TInfoCli.Reg:
      W_Ok = SInfoCliente:ADD-LAST(TInfoCli.Lin).
  END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-192
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-192 wWin
ON CHOOSE OF BUTTON-192 IN FRAME FInfoCliente /* Ocultar */
DO:
  ENABLE {&List-1} WITH FRAME FDetCon.
  HIDE FRAME FInfoCliente.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FSipla
&Scoped-define SELF-NAME BUTTON-195
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-195 wWin
ON CHOOSE OF BUTTON-195 IN FRAME FSipla /* Button 195 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-196
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-196 wWin
ON CHOOSE OF BUTTON-196 IN FRAME FSipla /* Button 196 */
DO:
  APPLY "value-changed" TO RInformes IN FRAME FImprimir.
  VIEW FRAME FImprimir.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-197
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-197 wWin
ON CHOOSE OF BUTTON-197 IN FRAME FSipla /* Button 197 */
DO:
   HIDE FRAME FDetCon.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FTraspaso
&Scoped-define SELF-NAME BUTTON-199
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-199 wWin
ON CHOOSE OF BUTTON-199 IN FRAME FTraspaso /* Traspasar */
DO:
  FIND Cfg_Instancias WHERE
       Cfg_Instancias.Tipo_Instancia EQ 6 AND
       Cfg_Instancias.Instancia EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME FSipla,1,5)) AND
       Cfg_Instancias.Usuario   EQ Tut.Usuario AND
       Cfg_Instancias.Estado    EQ 2 NO-ERROR.
  IF AVAILABLE Cfg_Instancias THEN Cfg_Instancias.Estado = 1.
  FIND Cfg_Instancias WHERE
       Cfg_Instancias.Tipo_Instancia EQ 6 AND
       Cfg_Instancias.Instancia EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME FSipla,1,5)) AND
       Cfg_Instancias.Usuario   EQ W_Usuario NO-ERROR.
  IF AVAILABLE Cfg_Instancias THEN Cfg_Instancias.Estado = 2.
  FOR EACH TConsulta:
      FIND Mov_InsSipla WHERE
           Mov_InsSipla.Instancia   EQ TConsulta.Instancia AND
           Mov_InsSipla.Nit         EQ TConsulta.Nit AND
           Mov_InsSipla.CodAutoriza EQ TConsulta.PCod AND
           Mov_InsSipla.UsuGestiona EQ W_Usuario NO-ERROR.
      IF AVAILABLE Mov_InsSipla THEN 
         Mov_InsSipla.UsuGestiona = Tut.Usuario.
  END.
  APPLY "choose" TO Btn_outtraspaso IN FRAME FTraspaso.
  APPLY "value-changed" TO Cmb_Instancias IN FRAME FSipla.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FImprimir
&Scoped-define SELF-NAME BUTTON-200
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-200 wWin
ON CHOOSE OF BUTTON-200 IN FRAME FImprimir /* Imprimir */
DO:
  ASSIGN FRAME FImprimir RInformes TIncluir FecCorte.
  CASE RInformes:
      WHEN 1 THEN RUN Inf_Individual.
      WHEN 2 THEN RUN Inf_Consulta.
      WHEN 3 THEN RUN Inf_UIAF.
      WHEN 4 THEN RUN Inf_Exonerados.
      WHEN 5 THEN RUN Inf_Productos.
      WHEN 6 THEN RUN Inf_ROSS.
  END CASE.
  W_ok = e1:READ-FILE(Listado) IN FRAME FReporte.
  VIEW FRAME FReporte.
  HIDE FRAME FImprimir.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-201
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-201 wWin
ON CHOOSE OF BUTTON-201 IN FRAME FImprimir /* Ocultar */
DO:
  HIDE FRAME FImprimir.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FReporte
&Scoped-define SELF-NAME BUTTON-204
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-204 wWin
ON CHOOSE OF BUTTON-204 IN FRAME FReporte /* Ocultar */
DO:
  HIDE FRAME FReporte.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FConsulta
&Scoped-define SELF-NAME BUTTON-205
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-205 wWin
ON CHOOSE OF BUTTON-205 IN FRAME FConsulta /* Button 205 */
DO:
  VIEW FRAME FFiltros.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FSipla
&Scoped-define SELF-NAME Cmb_Instancias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Instancias wWin
ON VALUE-CHANGED OF Cmb_Instancias IN FRAME FSipla /* Instancias */
DO:
  FIND FIRST Instancias WHERE Instancias.Instancia EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME FSipla,1,5)) NO-LOCK NO-ERROR.
  IF AVAILABLE Instancias THEN DO:
     ASSIGN Orden_InsPan = Instancias.Orden
            W_VigIns     = Instancias.TMI.
  END.
  RUN Registros_X_Instancia NO-ERROR.
  IF AVAILABLE TConsulta THEN DO:
      IF TConsulta.Instancia NE W_Ultima THEN DO:
         W_Ok = RInformes:DISABLE("Reporte UIAF") IN FRAME FImprimir.
         DISABLE FecCorte TIncluir WITH FRAME FImprimir.
      END.
      ELSE DO:
         W_Ok = RInformes:ENABLE("Reporte UIAF") IN FRAME FImprimir.
         ENABLE FecCorte TIncluir WITH FRAME FImprimir.
      END.
  END.
  IF Orden_InsPan EQ 1 THEN
     ENABLE Btn_RegManual WITH FRAME FConsulta.
  ELSE
     DISABLE Btn_RegManual WITH FRAME FConsulta.
  FOR EACH Tut: DELETE Tut. END.
  W_Ok = RInformes:ENABLE("Informe de los Registros de la Instancia") IN FRAME FImprimir.
  W_Ok = RInformes:DISABLE("Informe del Registro Actual") IN FRAME FImprimir.

  RUN UsuariosTraspaso.
  VIEW FRAME FConsulta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FDetCon
&Scoped-define SELF-NAME Mov_InsSipla.Id_Exonerada
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Mov_InsSipla.Id_Exonerada wWin
ON VALUE-CHANGED OF Mov_InsSipla.Id_Exonerada IN FRAME FDetCon /* Normal */
DO:
  IF SELF:SCREEN-VALUE EQ "yes" THEN
     ASSIGN Mov_InsSipla.Id_Sospechosa:SCREEN-VALUE IN FRAME FDetCon = "no"
            Mov_InsSipla.Id_RepUIAF:SCREEN-VALUE IN FRAME FDetCon = "no"
            Mov_InsSipla.Id_RepROSS:SCREEN-VALUE IN FRAME FDetCon = "no".
  ELSE
     ASSIGN Mov_InsSipla.Id_Sospechosa:SCREEN-VALUE IN FRAME FDetCon = "yes"
            Mov_InsSipla.Id_RepUIAF:SCREEN-VALUE IN FRAME FDetCon = "no"
            Mov_InsSipla.Id_RepROSS:SCREEN-VALUE IN FRAME FDetCon = "no".
/*  IF TConsulta.Instancia NE W_Primera THEN 
     IF SELF:SCREEN-VALUE EQ "yes" THEN
        Btn_Pasar:LABEL = "Normalizar".
     ELSE
        Btn_Pasar:LABEL = "Pasar a Siguiente Instancia".*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FManual
&Scoped-define SELF-NAME Id_ExoneradaM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Id_ExoneradaM wWin
ON VALUE-CHANGED OF Id_ExoneradaM IN FRAME FManual /* Normal */
DO:
DO WITH FRAME FManual:
  IF SELF:SCREEN-VALUE EQ "yes" THEN
     ASSIGN Id_SospechosaM:SCREEN-VALUE  = "no"
            Id_RepFiscaliaM:SCREEN-VALUE = "no".
  ELSE
     ASSIGN Id_SospechosaM:SCREEN-VALUE  = "yes"
            Id_RepFiscaliaM:SCREEN-VALUE = "no".
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Id_RepFiscaliaM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Id_RepFiscaliaM wWin
ON VALUE-CHANGED OF Id_RepFiscaliaM IN FRAME FManual /* Reportar a UIAF */
DO:
DO WITH FRAME FManual:
  IF SELF:SCREEN-VALUE EQ "yes" THEN
     ASSIGN Id_ExoneradaM:SCREEN-VALUE = "no".
  ELSE DO:
     IF Id_SospechosaM:SCREEN-VALUE EQ "no" THEN
        ASSIGN Id_ExoneradaM:SCREEN-VALUE = "yes".
  END.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FDetCon
&Scoped-define SELF-NAME Mov_InsSipla.Id_RepROSS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Mov_InsSipla.Id_RepROSS wWin
ON VALUE-CHANGED OF Mov_InsSipla.Id_RepROSS IN FRAME FDetCon /* Reportar ROSS */
DO:
DO WITH FRAME FDetCon:
  IF SELF:SCREEN-VALUE EQ "yes" THEN
     ASSIGN Mov_InsSipla.Id_Exonerada:SCREEN-VALUE = "no"
            Mov_InsSipla.Id_Sospechosa:SCREEN-VALUE = "yes"
            Btn_Pasar:LABEL = "Pasara a Siguiente Instancia".
  IF TConsulta.Instancia NE W_Primera THEN 
     IF Mov_InsSipla.Id_Exonerada:SCREEN-VALUE EQ "yes" THEN
        ASSIGN Btn_Pasar:LABEL = "Normalizar".
     ELSE
        ASSIGN Btn_Pasar:LABEL = "Pasar a Siguiente Instancia".  
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FManual
&Scoped-define SELF-NAME Id_RepROSSM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Id_RepROSSM wWin
ON VALUE-CHANGED OF Id_RepROSSM IN FRAME FManual /* Reportar ROSS */
DO:
DO WITH FRAME FDetCon:
  IF SELF:SCREEN-VALUE EQ "yes" THEN
     ASSIGN Mov_InsSipla.Id_Exonerada:SCREEN-VALUE = "no"
            Mov_InsSipla.Id_Sospechosa:SCREEN-VALUE = "yes"
            Btn_Pasar:LABEL = "Pasara a Siguiente Instancia".
  IF TConsulta.Instancia NE W_Primera THEN 
     IF Mov_InsSipla.Id_Exonerada:SCREEN-VALUE EQ "yes" THEN
        ASSIGN Btn_Pasar:LABEL = "Normalizar".
     ELSE
        ASSIGN Btn_Pasar:LABEL = "Pasar a Siguiente Instancia".  
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FDetCon
&Scoped-define SELF-NAME Mov_InsSipla.Id_RepUIAF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Mov_InsSipla.Id_RepUIAF wWin
ON VALUE-CHANGED OF Mov_InsSipla.Id_RepUIAF IN FRAME FDetCon /* Reportar UIAF */
DO:
  IF SELF:SCREEN-VALUE EQ "yes" THEN
     ASSIGN Btn_Pasar:LABEL = "Pasara a Siguiente Instancia"
            Mov_InsSipla.Id_Exonerada:SCREEN-VALUE IN FRAME FDetCon = "no".
  ELSE DO:
     IF Mov_InsSipla.Id_Sospechosa:SCREEN-VALUE EQ "no" THEN
        ASSIGN Mov_InsSipla.Id_Exonerada:SCREEN-VALUE IN FRAME FDetCon = "yes".
  END.
  IF TConsulta.Instancia NE W_Primera THEN 
     IF Mov_InsSipla.Id_Exonerada:SCREEN-VALUE IN FRAME FDetCon EQ "yes" THEN
        ASSIGN Btn_Pasar:LABEL IN FRAME FDetCon = "Normalizar".
     ELSE
        ASSIGN Btn_Pasar:LABEL IN FRAME FDetCon = "Pasar a Siguiente Instancia".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Mov_InsSipla.Id_Sospechosa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Mov_InsSipla.Id_Sospechosa wWin
ON VALUE-CHANGED OF Mov_InsSipla.Id_Sospechosa IN FRAME FDetCon /* Sospechosa */
DO:
  IF SELF:SCREEN-VALUE EQ "yes" THEN
     ASSIGN Btn_Pasar:LABEL = "Pasara a Siguiente Instancia"
            Mov_InsSipla.Id_Exonerada:SCREEN-VALUE IN FRAME FDetCon = "no".
  ELSE DO:
     IF Mov_InsSipla.Id_RepUIAF:SCREEN-VALUE EQ "no" AND 
        Mov_InsSipla.Id_RepROSS:SCREEN-VALUE EQ "no" THEN
        ASSIGN Mov_InsSipla.Id_Exonerada:SCREEN-VALUE IN FRAME FDetCon = "yes".
  END.
  /*IF TConsulta.Instancia NE W_Primera THEN 
     IF Mov_InsSipla.Id_Exonerada:SCREEN-VALUE IN FRAME FDetCon EQ "yes" THEN
        ASSIGN Btn_Pasar:LABEL IN FRAME FDetCon = "Normalizar".
     ELSE
        ASSIGN Btn_Pasar:LABEL IN FRAME FDetCon = "Pasar a Siguiente Instancia".*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FManual
&Scoped-define SELF-NAME Id_SospechosaM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Id_SospechosaM wWin
ON VALUE-CHANGED OF Id_SospechosaM IN FRAME FManual /* Sospechosa */
DO:
DO WITH FRAME FManual:
  IF SELF:SCREEN-VALUE EQ "yes" THEN
     ASSIGN Id_ExoneradaM:SCREEN-VALUE = "no".
  ELSE DO:
     IF Id_RepFiscaliaM:SCREEN-VALUE EQ "no" THEN
        ASSIGN Id_ExoneradaM:SCREEN-VALUE = "yes".
  END.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME NitM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL NitM wWin
ON LEAVE OF NitM IN FRAME FManual /* Nit */
DO:
  IF SELF:SCREEN-VALUE EQ "" THEN DO:
     MESSAGE "No se puede crear el registro si no se ingresa" SKIP
             "un numero de identificacion valido" VIEW-AS ALERT-BOX ERROR.
     APPLY "choose" TO Btn_OutManual.
  END.
  ELSE DO:
     FIND Clientes WHERE Clientes.Nit EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
     IF AVAILABLE Clientes THEN DO:
        ASSIGN WNomClienteM:SCREEN-VALUE = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
        APPLY "entry" TO Id_NUMM.
     END.
     ELSE DO:
        RUN C-Clientes.R(INPUT 1,INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_AgeCli).
        ASSIGN WNomClienteM:SCREEN-VALUE = P_Nombre + " " + P_Apellido
               SELF:SCREEN-VALUE   = P_Nit.
        FIND Clientes WHERE Clientes.Nit EQ P_Nit NO-LOCK NO-ERROR.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FFiltros
&Scoped-define SELF-NAME RFiltros
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RFiltros wWin
ON VALUE-CHANGED OF RFiltros IN FRAME FFiltros
DO:
  DEFINE VAR w-fechainf  AS DATE.
  CASE RFiltros:SCREEN-VALUE IN FRAME FFiltros:
      WHEN "1" THEN DO:
          CASE ROrganizar:SCREEN-VALUE IN FRAME FConsulta:
            WHEN "1" THEN OPEN QUERY BConsulta FOR EACH TConsulta NO-LOCK BY TConsulta.Agencia INDEXED-REPOSITION.
            WHEN "2" THEN OPEN QUERY BConsulta FOR EACH TConsulta NO-LOCK BY TConsulta.Nit INDEXED-REPOSITION.
            WHEN "3" THEN OPEN QUERY BConsulta FOR EACH TConsulta NO-LOCK BY TConsulta.Nombre INDEXED-REPOSITION.
            WHEN "4" THEN OPEN QUERY BConsulta FOR EACH TConsulta NO-LOCK BY TConsulta.FecRep INDEXED-REPOSITION.
            WHEN "5" THEN OPEN QUERY BConsulta FOR EACH TConsulta NO-LOCK BY TConsulta.Vigencia DESC INDEXED-REPOSITION.
          END CASE.
      END.
      WHEN "2" THEN DO:
          CASE ROrganizar:SCREEN-VALUE IN FRAME FConsulta:
            WHEN "1" THEN OPEN QUERY BConsulta 
                               FOR EACH TConsulta WHERE MONTH(TConsulta.FecRep) EQ MONTH(W_Fecha)
                                        NO-LOCK BY TConsulta.Agencia INDEXED-REPOSITION.
            WHEN "2" THEN OPEN QUERY BConsulta
                               FOR EACH TConsulta WHERE MONTH(TConsulta.FecRep) EQ MONTH(W_Fecha)
                                        NO-LOCK BY TConsulta.Nit INDEXED-REPOSITION.
            WHEN "3" THEN OPEN QUERY BConsulta 
                               FOR EACH TConsulta WHERE MONTH(TConsulta.FecRep) EQ MONTH(W_Fecha)
                                        NO-LOCK BY TConsulta.Nombre INDEXED-REPOSITION.
            WHEN "4" THEN OPEN QUERY BConsulta 
                               FOR EACH TConsulta WHERE MONTH(TConsulta.FecRep) EQ MONTH(W_Fecha)
                                        NO-LOCK BY TConsulta.FecRep INDEXED-REPOSITION.
            WHEN "5" THEN OPEN QUERY BConsulta 
                               FOR EACH TConsulta WHERE MONTH(TConsulta.FecRep) EQ MONTH(W_Fecha)
                                        NO-LOCK BY TConsulta.Vigencia DESC INDEXED-REPOSITION.
          END CASE.
      END.
      WHEN "3" THEN DO:
          CASE ROrganizar:SCREEN-VALUE IN FRAME FConsulta:
            WHEN "1" THEN OPEN QUERY BConsulta 
                               FOR EACH TConsulta WHERE DAY(TConsulta.FecRep) EQ DAY(W_Fecha) - 1
                                        NO-LOCK BY TConsulta.Agencia INDEXED-REPOSITION.
            WHEN "2" THEN OPEN QUERY BConsulta
                               FOR EACH TConsulta WHERE DAY(TConsulta.FecRep) EQ DAY(W_Fecha) - 1
                                        NO-LOCK BY TConsulta.Nit INDEXED-REPOSITION.
            WHEN "3" THEN OPEN QUERY BConsulta 
                               FOR EACH TConsulta WHERE DAY(TConsulta.FecRep) EQ DAY(W_Fecha) - 1
                                        NO-LOCK BY TConsulta.Nombre INDEXED-REPOSITION.
            WHEN "4" THEN OPEN QUERY BConsulta 
                               FOR EACH TConsulta WHERE DAY(TConsulta.FecRep) EQ DAY(W_Fecha) - 1
                                        NO-LOCK BY TConsulta.FecRep INDEXED-REPOSITION.
            WHEN "5" THEN OPEN QUERY BConsulta 
                               FOR EACH TConsulta WHERE DAY(TConsulta.FecRep) EQ DAY(W_Fecha) - 1
                                        NO-LOCK BY TConsulta.Vigencia DESC INDEXED-REPOSITION.
          END CASE.
      END.
      WHEN "4" THEN DO:
          CASE ROrganizar:SCREEN-VALUE IN FRAME FConsulta:
            WHEN "1" THEN OPEN QUERY BConsulta 
                               FOR EACH TConsulta WHERE DAY(TConsulta.FecRep) EQ DAY(W_Fecha) AND
                                                      MONTH(TConsulta.FecRep) EQ MONTH(W_Fecha)
                                        NO-LOCK BY TConsulta.Agencia INDEXED-REPOSITION.
            WHEN "2" THEN OPEN QUERY BConsulta
                               FOR EACH TConsulta WHERE DAY(TConsulta.FecRep) EQ DAY(W_Fecha) AND
                                                      MONTH(TConsulta.FecRep) EQ MONTH(W_Fecha)
                                        NO-LOCK BY TConsulta.Nit INDEXED-REPOSITION.
            WHEN "3" THEN OPEN QUERY BConsulta 
                               FOR EACH TConsulta WHERE DAY(TConsulta.FecRep) EQ DAY(W_Fecha) AND
                                                      MONTH(TConsulta.FecRep) EQ MONTH(W_Fecha)
                                        NO-LOCK BY TConsulta.Nombre INDEXED-REPOSITION.
            WHEN "4" THEN OPEN QUERY BConsulta 
                               FOR EACH TConsulta WHERE DAY(TConsulta.FecRep) EQ DAY(W_Fecha) AND
                                                      MONTH(TConsulta.FecRep) EQ MONTH(W_Fecha)
                                        NO-LOCK BY TConsulta.FecRep INDEXED-REPOSITION.
            WHEN "5" THEN OPEN QUERY BConsulta 
                               FOR EACH TConsulta WHERE DAY(TConsulta.FecRep) EQ DAY(W_Fecha) AND
                                                      MONTH(TConsulta.FecRep) EQ MONTH(W_Fecha)
                                        NO-LOCK BY TConsulta.Vigencia DESC INDEXED-REPOSITION.
          END CASE.
      END.
      WHEN "5" THEN DO:
          w-fechainf =  w_fecha - (DAY(w_fecha) + 1).
          CASE ROrganizar:SCREEN-VALUE IN FRAME FConsulta:
            WHEN "1" THEN OPEN QUERY BConsulta 
                               FOR EACH TConsulta WHERE TConsulta.FecRep LT w-fechainf
                                        NO-LOCK BY TConsulta.Agencia INDEXED-REPOSITION.
            WHEN "2" THEN OPEN QUERY BConsulta
                               FOR EACH TConsulta WHERE TConsulta.FecRep LT w-fechainf
                                        NO-LOCK BY TConsulta.Nit INDEXED-REPOSITION.
            WHEN "3" THEN OPEN QUERY BConsulta 
                               FOR EACH TConsulta WHERE TConsulta.FecRep LT w-fechainf
                                        NO-LOCK BY TConsulta.Nombre INDEXED-REPOSITION.
            WHEN "4" THEN OPEN QUERY BConsulta 
                               FOR EACH TConsulta WHERE TConsulta.FecRep LT w-fechainf
                                        NO-LOCK BY TConsulta.FecRep INDEXED-REPOSITION.
            WHEN "5" THEN OPEN QUERY BConsulta 
                               FOR EACH TConsulta WHERE TConsulta.FecRep LT w-fechainf
                                        NO-LOCK BY TConsulta.Vigencia DESC INDEXED-REPOSITION.
          END CASE.
      END.
      WHEN "6" THEN DO:
          CASE ROrganizar:SCREEN-VALUE IN FRAME FConsulta:
            WHEN "1" THEN OPEN QUERY BConsulta 
                               FOR EACH TConsulta WHERE TConsulta.IdSos
                                        NO-LOCK BY TConsulta.Agencia INDEXED-REPOSITION.
            WHEN "2" THEN OPEN QUERY BConsulta
                               FOR EACH TConsulta WHERE TConsulta.IdSos
                                        NO-LOCK BY TConsulta.Nit INDEXED-REPOSITION.
            WHEN "3" THEN OPEN QUERY BConsulta 
                               FOR EACH TConsulta WHERE TConsulta.IdSos
                                        NO-LOCK BY TConsulta.Nombre INDEXED-REPOSITION.
            WHEN "4" THEN OPEN QUERY BConsulta 
                               FOR EACH TConsulta WHERE TConsulta.IdSos
                                        NO-LOCK BY TConsulta.FecRep INDEXED-REPOSITION.
            WHEN "5" THEN OPEN QUERY BConsulta 
                               FOR EACH TConsulta WHERE TConsulta.IdSos
                                        NO-LOCK BY TConsulta.Vigencia DESC INDEXED-REPOSITION.
          END CASE.
      END.
      WHEN "7" THEN DO:
          CASE ROrganizar:SCREEN-VALUE IN FRAME FConsulta:
            WHEN "1" THEN OPEN QUERY BConsulta 
                               FOR EACH TConsulta WHERE TConsulta.IdUIAF
                                        NO-LOCK BY TConsulta.Agencia INDEXED-REPOSITION.
            WHEN "2" THEN OPEN QUERY BConsulta
                               FOR EACH TConsulta WHERE TConsulta.IdUIAF
                                        NO-LOCK BY TConsulta.Nit INDEXED-REPOSITION.
            WHEN "3" THEN OPEN QUERY BConsulta 
                               FOR EACH TConsulta WHERE TConsulta.IdUIAF
                                        NO-LOCK BY TConsulta.Nombre INDEXED-REPOSITION.
            WHEN "4" THEN OPEN QUERY BConsulta 
                               FOR EACH TConsulta WHERE TConsulta.IdUIAF
                                        NO-LOCK BY TConsulta.FecRep INDEXED-REPOSITION.
            WHEN "5" THEN OPEN QUERY BConsulta 
                               FOR EACH TConsulta WHERE TConsulta.IdUIAF
                                        NO-LOCK BY TConsulta.Vigencia DESC INDEXED-REPOSITION.
          END CASE.
      END.
      WHEN "8" THEN DO:
          CASE ROrganizar:SCREEN-VALUE IN FRAME FConsulta:
            WHEN "1" THEN OPEN QUERY BConsulta 
                               FOR EACH TConsulta WHERE TConsulta.IdROSS
                                        NO-LOCK BY TConsulta.Agencia INDEXED-REPOSITION.
            WHEN "2" THEN OPEN QUERY BConsulta
                               FOR EACH TConsulta WHERE TConsulta.IdROSS
                                        NO-LOCK BY TConsulta.Nit INDEXED-REPOSITION.
            WHEN "3" THEN OPEN QUERY BConsulta 
                               FOR EACH TConsulta WHERE TConsulta.IdROSS
                                        NO-LOCK BY TConsulta.Nombre INDEXED-REPOSITION.
            WHEN "4" THEN OPEN QUERY BConsulta 
                               FOR EACH TConsulta WHERE TConsulta.IdROSS
                                        NO-LOCK BY TConsulta.FecRep INDEXED-REPOSITION.
            WHEN "5" THEN OPEN QUERY BConsulta 
                               FOR EACH TConsulta WHERE TConsulta.IdROSS
                                        NO-LOCK BY TConsulta.Vigencia DESC INDEXED-REPOSITION.
          END CASE.
      END.
      WHEN "9" THEN DO:
          CASE ROrganizar:SCREEN-VALUE IN FRAME FConsulta:
            WHEN "1" THEN OPEN QUERY BConsulta 
                               FOR EACH TConsulta WHERE TConsulta.IdNor
                                        NO-LOCK BY TConsulta.Agencia INDEXED-REPOSITION.
            WHEN "2" THEN OPEN QUERY BConsulta
                               FOR EACH TConsulta WHERE TConsulta.IdNor
                                        NO-LOCK BY TConsulta.Nit INDEXED-REPOSITION.
            WHEN "3" THEN OPEN QUERY BConsulta 
                               FOR EACH TConsulta WHERE TConsulta.IdNor
                                        NO-LOCK BY TConsulta.Nombre INDEXED-REPOSITION.
            WHEN "4" THEN OPEN QUERY BConsulta 
                               FOR EACH TConsulta WHERE TConsulta.IdNor
                                        NO-LOCK BY TConsulta.FecRep INDEXED-REPOSITION.
            WHEN "5" THEN OPEN QUERY BConsulta 
                               FOR EACH TConsulta WHERE TConsulta.IdNor
                                        NO-LOCK BY TConsulta.Vigencia DESC INDEXED-REPOSITION.
          END CASE.
      END.
      WHEN "10" THEN DO:
          CASE ROrganizar:SCREEN-VALUE IN FRAME FConsulta:
            WHEN "1" THEN OPEN QUERY BConsulta 
                               FOR EACH TConsulta WHERE TConsulta.TipReg EQ "A"
                                        NO-LOCK BY TConsulta.Agencia INDEXED-REPOSITION.
            WHEN "2" THEN OPEN QUERY BConsulta
                               FOR EACH TConsulta WHERE TConsulta.TipReg EQ "A"
                                        NO-LOCK BY TConsulta.Nit INDEXED-REPOSITION.
            WHEN "3" THEN OPEN QUERY BConsulta 
                               FOR EACH TConsulta WHERE TConsulta.TipReg EQ "A"
                                        NO-LOCK BY TConsulta.Nombre INDEXED-REPOSITION.
            WHEN "4" THEN OPEN QUERY BConsulta 
                               FOR EACH TConsulta WHERE TConsulta.TipReg EQ "A"
                                        NO-LOCK BY TConsulta.FecRep INDEXED-REPOSITION.
            WHEN "5" THEN OPEN QUERY BConsulta 
                               FOR EACH TConsulta WHERE TConsulta.TipReg EQ "A"
                                        NO-LOCK BY TConsulta.Vigencia DESC INDEXED-REPOSITION.
          END CASE.
      END.
      WHEN "11" THEN DO:
          CASE ROrganizar:SCREEN-VALUE IN FRAME FConsulta:
            WHEN "1" THEN OPEN QUERY BConsulta 
                               FOR EACH TConsulta WHERE TConsulta.TipReg EQ "M"
                                        NO-LOCK BY TConsulta.Agencia INDEXED-REPOSITION.
            WHEN "2" THEN OPEN QUERY BConsulta
                               FOR EACH TConsulta WHERE TConsulta.TipReg EQ "M"
                                        NO-LOCK BY TConsulta.Nit INDEXED-REPOSITION.
            WHEN "3" THEN OPEN QUERY BConsulta 
                               FOR EACH TConsulta WHERE TConsulta.TipReg EQ "M"
                                        NO-LOCK BY TConsulta.Nombre INDEXED-REPOSITION.
            WHEN "4" THEN OPEN QUERY BConsulta 
                               FOR EACH TConsulta WHERE TConsulta.TipReg EQ "M"
                                        NO-LOCK BY TConsulta.FecRep INDEXED-REPOSITION.
            WHEN "5" THEN OPEN QUERY BConsulta 
                               FOR EACH TConsulta WHERE TConsulta.TipReg EQ "M"
                                        NO-LOCK BY TConsulta.Vigencia DESC INDEXED-REPOSITION.
          END CASE.
      END.

  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FImprimir
&Scoped-define SELF-NAME RInformes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RInformes wWin
ON VALUE-CHANGED OF RInformes IN FRAME FImprimir
DO:
  ENABLE TIncluir FecCorte WITH FRAME FImprimir.
  IF SELF:SCREEN-VALUE NE "3" AND
     SELF:SCREEN-VALUE NE "4" AND
     SELF:SCREEN-VALUE NE "5" AND
     SELF:SCREEN-VALUE NE "6" THEN
     DISABLE TIncluir FecCorte WITH FRAME FImprimir.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FConsulta
&Scoped-define SELF-NAME ROrganizar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ROrganizar wWin
ON VALUE-CHANGED OF ROrganizar IN FRAME FConsulta
DO:
  CASE ROrganizar:SCREEN-VALUE IN FRAME FConsulta:
    WHEN "1" THEN OPEN QUERY BConsulta FOR EACH TConsulta NO-LOCK BY TConsulta.Agencia INDEXED-REPOSITION.
    WHEN "2" THEN OPEN QUERY BConsulta FOR EACH TConsulta NO-LOCK BY TConsulta.Nit INDEXED-REPOSITION.
    WHEN "3" THEN OPEN QUERY BConsulta FOR EACH TConsulta NO-LOCK BY TConsulta.Nombre INDEXED-REPOSITION.
    WHEN "4" THEN OPEN QUERY BConsulta FOR EACH TConsulta NO-LOCK BY TConsulta.FecRep INDEXED-REPOSITION.
    WHEN "5" THEN OPEN QUERY BConsulta FOR EACH TConsulta NO-LOCK BY TConsulta.Vigencia DESC INDEXED-REPOSITION.
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FCerradas
&Scoped-define BROWSE-NAME BCerradas
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

FIND FIRST cfg_instancias WHERE cfg_instancias.instancia = 880
                            AND cfg_instancias.usuario = w_usuario
                            AND cfg_instancias.estado = 1 NO-LOCK NO-ERROR.
IF NOT AVAILABLE cfg_instancias THEN DO:
    RInformes:DISABLE("Reporte Transacciones en Efectivo") IN FRAME FImprimir.
    RInformes:DISABLE("Reporte Clientes Exonerados") IN FRAME FImprimir.
    RInformes:DISABLE("Reporte de Productos") IN FRAME FImprimir.
END.

FIND FIRST entidad WHERE entidad.entidad = w_entidad NO-LOCK NO-ERROR.
IF AVAILABLE entidad THEN
    SSTTTCCC = STRING(entidad.cod_sector,"99") +
               STRING(entidad.tipo_entidad,"999") +
               STRING(entidad.cod_entidadSarlaft,"999").

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
  DISPLAY Cmb_Instancias NomUsuario 
      WITH FRAME FSipla IN WINDOW wWin.
  ENABLE Cmb_Instancias BUTTON-195 BUTTON-196 BUTTON-197 BtnDone 
      WITH FRAME FSipla IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FSipla}
  DISPLAY ROrganizar 
      WITH FRAME FConsulta IN WINDOW wWin.
  ENABLE RECT-304 BConsulta BUTTON-205 ROrganizar Btn_Traspasar 
      WITH FRAME FConsulta IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FConsulta}

  {&OPEN-QUERY-FDetCon}
  GET FIRST FDetCon.
  DISPLAY WNomAgencia WHorTransaccion WNomCliente WHorGestion WDevuelto 
          ConsEfectivo RetEfectivo AboCreditosEfe ConsCheque RetCheque 
          AboCreditosChe WNomUsuReporta WNomUsuGestiona 
      WITH FRAME FDetCon IN WINDOW wWin.
  IF AVAILABLE Mov_InsSipla THEN 
    DISPLAY Mov_InsSipla.Agencia Mov_InsSipla.Fecha_Transaccion Mov_InsSipla.Nit 
          Mov_InsSipla.Fecha_Gestion Mov_InsSipla.Id_NUM 
          Mov_InsSipla.Id_Exonerada Mov_InsSipla.Id_Sospechosa 
          Mov_InsSipla.Id_RepUIAF Mov_InsSipla.Id_RepROSS Mov_InsSipla.Id_NUD 
          Mov_InsSipla.Fec_RepUIAF Mov_InsSipla.Fec_RepROSS 
          Mov_InsSipla.Descripcion Mov_InsSipla.Tipo_Registro 
          Mov_InsSipla.Valor_RegManual Mov_InsSipla.UsuReporta 
          Mov_InsSipla.UsuGestiona 
      WITH FRAME FDetCon IN WINDOW wWin.
  ENABLE RECT-302 RECT-324 Mov_InsSipla.Id_Exonerada Mov_InsSipla.Id_Sospechosa 
         Mov_InsSipla.Id_RepUIAF Mov_InsSipla.Id_RepROSS Btn_Trans Btn_Cliente 
         Btn_Ahorros Btn_Creditos Btn_HojaVida Mov_InsSipla.Descripcion 
         Btn_Devolver Btn_Cerradas Btn_Pasar Btn_Salvar Btn_BorManual 
         Btn_OcuDetCon 
      WITH FRAME FDetCon IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FDetCon}
  DISPLAY E1 
      WITH FRAME FReporte IN WINDOW wWin.
  ENABLE E1 BUTTON-203 BUTTON-204 
      WITH FRAME FReporte IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FReporte}
  DISPLAY RFiltros 
      WITH FRAME FFiltros IN WINDOW wWin.
  ENABLE RFiltros Btn_outfiltros 
      WITH FRAME FFiltros IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FFiltros}
  DISPLAY RInformes TIncluir FecCorte 
      WITH FRAME FImprimir IN WINDOW wWin.
  ENABLE RInformes TIncluir FecCorte BUTTON-200 BUTTON-201 
      WITH FRAME FImprimir IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FImprimir}
  DISPLAY SInfoHojaVida 
      WITH FRAME FInfoHojaVida IN WINDOW wWin.
  ENABLE SInfoHojaVida Btn_OcuHV 
      WITH FRAME FInfoHojaVida IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FInfoHojaVida}
  ENABLE BTraspaso BUTTON-199 Btn_outtraspaso 
      WITH FRAME FTraspaso IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FTraspaso}
  DISPLAY ECerradas 
      WITH FRAME FCerradas IN WINDOW wWin.
  ENABLE BCerradas ECerradas BtnOcu_Cerradas 
      WITH FRAME FCerradas IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FCerradas}
  DISPLAY SInfoCliente 
      WITH FRAME FInfoCliente IN WINDOW wWin.
  ENABLE Btn_Segmentacion BUTTON-186 BUTTON-187 Btn_Relaciones Btn_Fechas 
         Btn_Controles SInfoCliente BUTTON-192 
      WITH FRAME FInfoCliente IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FInfoCliente}
  DISPLAY SInfoAhorros 
      WITH FRAME FInfoAhorros IN WINDOW wWin.
  ENABLE Btn_Alavista Btn_Contractual Btn_Atermino Btn_Aportes SInfoAhorros 
         Btn_OcultarAhorros 
      WITH FRAME FInfoAhorros IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FInfoAhorros}
  DISPLAY W_AboCreEfe W_AboCreChe W_ValConEfe W_ValRetEfe W_ValConChe 
          W_ValRetChe 
      WITH FRAME FTransacciones IN WINDOW wWin.
  ENABLE BTransacciones BCreditos BtnOcultar 
      WITH FRAME FTransacciones IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FTransacciones}
  DISPLAY SInfoCreditos 
      WITH FRAME FInfoCreditos IN WINDOW wWin.
  ENABLE Btn_Consumo Btn_Comercial Btn_Hipotecario Btn_Microcredito 
         SInfoCreditos Btn_OcuCre 
      WITH FRAME FInfoCreditos IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FInfoCreditos}

  {&OPEN-QUERY-FManual}
  GET FIRST FManual.
  DISPLAY Fecha_TransaccionM NitM WNomClienteM Id_NUMM Id_NUDM Id_ExoneradaM 
          Id_SospechosaM Id_RepFiscaliaM Id_RepROSSM ValManual DescripcionM 
      WITH FRAME FManual IN WINDOW wWin.
  ENABLE Fecha_TransaccionM Btn_SalvaManual Btn_OutManual RECT-323 
      WITH FRAME FManual IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FManual}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GraInfAho wWin 
PROCEDURE GraInfAho :
DEFINE INPUT PARAMETER XLin AS CHARACTER FORMAT "X(100)".
DEFINE INPUT PARAMETER XTip AS INTEGER.

Reg = Reg + 1.

CREATE TInfoAho.
ASSIGN TInfoAho.AReg = Reg
       TInfoAho.ATip = XTip
       TInfoAho.ALin = XLin.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GraInfCli wWin 
PROCEDURE GraInfCli :
DEFINE INPUT PARAMETER XLin AS CHARACTER FORMAT "X(100)".
DEFINE INPUT PARAMETER XTip AS INTEGER.

Reg = Reg + 1.

CREATE TInfoCli.
ASSIGN TInfoCli.Reg = Reg
       TInfoCli.Tip = XTip
       TInfocli.Lin = XLin.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GraInfCre wWin 
PROCEDURE GraInfCre :
DEFINE INPUT PARAMETER XLin AS CHARACTER FORMAT "X(100)".
DEFINE INPUT PARAMETER XTip AS INTEGER.

Reg = Reg + 1.

CREATE TInfoCre.
ASSIGN TInfoCre.CReg = Reg
       TInfoCre.CTip = XTip
       TInfoCre.CLin = XLin.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GraInfHV wWin 
PROCEDURE GraInfHV :
DEFINE INPUT PARAMETER XLin AS CHARACTER FORMAT "X(100)".
DEFINE INPUT PARAMETER XTip AS INTEGER.

Reg = Reg + 1.

CREATE TInfoHVd.
ASSIGN TInfoHVd.HReg = Reg
       TInfoHVd.HTip = XTip
       TInfoHVd.HLin = XLin.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inf_Consulta wWin 
PROCEDURE Inf_Consulta :
Listado = W_PathSpl + "Empresas.LST".
OS-DELETE VALUE(Listado).

DEFINE VAR WEnc1 AS CHARACTER FORMAT "X(75)".
DEFINE VAR WEnc2 AS CHARACTER FORMAT "X(2)".
DEFINE VAR WNEmp AS CHARACTER FORMAT "X(30)".
DEFINE VAR NomEmp AS CHARACTER FORMAT "X(50)".
DEFINE VAR TotMon AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
DEFINE VAR TotCuo AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
DEFINE VAR TTotMon AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
DEFINE VAR TTotCuo AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
DEFINE VAR EmpIni LIKE Empresas.Cod_Empresa.
DEFINE VAR EmpFin LIKE Empresas.Cod_Empresa.
DEFINE VAR TotReg AS DECIMAL.
DEFINE VAR XLinea1 AS CHARACTER FORMAT "X(100)".
DEFINE VAR XLinea2 AS CHARACTER FORMAT "X(100)".
DEFINE VAR XLinea3 AS CHARACTER FORMAT "X(100)".
DEFINE VAR XLinea4 AS CHARACTER FORMAT "X(100)".
DEFINE VAR XLinea5 AS CHARACTER FORMAT "X(100)".

ASSIGN FRAME FDetCon WNomAgencia WNomCliente WNomUsuReporta WNomUsuGestiona
             ConsEfectivo ConsCheque RetEfectivo RetCheque AboCreditosEfe AboCreditosChe.

OUTPUT TO value(Listado) NO-ECHO PAGED PAGE-SIZE 65.
{Incluido\RepEncabezado.i}
    XLinea1 = Cmb_Instancias:SCREEN-VALUE IN FRAME FSipla.
    W_Reporte = "REPORTE   : REPORTE DE REGISTRO INDIVIDUAL"
                + " - Fecha: " + STRING(W_Fecha) + " - " + STRING(TIME,"hh:mm am").
    W_EncColumna = "AGE NIT          NOMBRE                              NUD NUM   CONSIGNADO      RETIROS     CREDITOS FEC.REPO VIGEN".

    VIEW FRAME F-Encabezado.

    DISPLAY " Instancia: " XLinea1
        WITH FRAME FDetIns WIDTH 132 NO-LABELS USE-TEXT NO-BOX STREAM-IO.

    FOR EACH TConsulta:
        DISPLAY TConsulta.Agencia
                TConsulta.Nit
                TConsulta.Nombre FORMAT "X(35)"
                TConsulta.Id_NUD
                TConsulta.Id_NUM
                TConsulta.SCons
                TConsulta.SReti
                TConsulta.SAbon
                TConsulta.FecRep
                TConsulta.Vigencia
            WITH FRAME FMov WIDTH 132 NO-LABELS USE-TEXT NO-BOX STREAM-IO.
    END.

    VIEW FRAME F-Ftr.

    PAGE.
OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inf_Exonerados wWin 
PROCEDURE Inf_Exonerados :
/* Reporte a la UIAF de los Clientes Exonerados */

/*' VALUE(Listado).*/
DEFINE VAR fechaIni AS DATE.
DEFINE VAR fechaFin AS DATE.
DEFINE VAR zcadena AS CHARACTER FORMAT "X(89)".
DEFINE VAR reg_encabezado AS CHARACTER.
DEFINE VAR reg_fin AS CHARACTER.
DEFINE VAR varRegistro AS CHARACTER.
DEFINE VAR tipoId AS CHARACTER.
DEFINE VAR numId AS CHARACTER.
DEFINE VAR i AS INTEGER INITIAL 0.
DEFINE VAR vApellido1 AS CHARACTER FORMAT "X(40)" INITIAL "                                        ".
DEFINE VAR vApellido2 AS CHARACTER FORMAT "X(40)" INITIAL "                                        ".
DEFINE VAR NombreTitular AS CHARACTER FORMAT "X(40)" INITIAL "                                        ".
DEFINE VAR OtroNombreTitular AS CHARACTER FORMAT "X(40)" INITIAL "                                        ".
DEFINE VAR razonSocial AS CHARACTER FORMAT "X(60)" INITIAL "                                                            ".
DEFINE VAR consecutivo AS INTEGER INITIAL 0.
/* ----- */

FOR EACH registrosUIAF:
    DELETE registrosUIAF.
END.

IF fecCorte = ? THEN
    fecCorte = TODAY.

ASSIGN FRAME FImprimir TIncluir FecCorte.
ASSIGN fechafin = FecCorte
       fechaIni = FecCorte - DAY(FecCorte) + 1.

ASSIGN consecutivo = 0
       w_vlrTotal = 0.

/* Creacion de registros al plano de los Clientes que se encuentran exonerados */
FOR EACH clientes WHERE clientes.id_ExoneradoSipla = YES
                    AND clientes.estado = 1 NO-LOCK:
    /* Se limpian las variables */
    vApellido1 = "".
    vApellido2 = "".
    NombreTitular = "".
    OtroNombreTitular = "".
    razonSocial = "".
    varRegistro = "".
    tipoId = "".
    numId = "".

    
    /* Se carga el tipo de identificación */
    CASE clientes.Tipo_Identificacion:
        WHEN "C.C" THEN tipoId = "13".
        WHEN "C.E" THEN tipoId = "21".
        WHEN "NIT" THEN tipoId = "31".
        WHEN "T.I" THEN tipoId = "12".
        WHEN "NUI" THEN tipoId = "00".
    END CASE.

    /* Se elimina el caracter "-" del campo Nit */
    IF clientes.tipo_ident EQ "NIT" THEN DO:
        REPEAT i = 1 TO 15:
            IF SUBSTRING(clientes.nit,i,1) NE "-" THEN
                numId = TRIM(numId) + SUBSTRING(clientes.nit,i,1).
            ELSE
                i = 20.
        END.
    END.
    ELSE
        ASSIGN numId = TRIM(clientes.nit).
    
    /* Se cargan los apellidos, nombre y/o razón social del titular */
    IF clientes.tipo_identificacion <> "NIT" THEN DO:
        vApellido1 = clientes.Apellido1.
        vApellido2 = clientes.Apellido2.
        NombreTitular = SUBSTRING(clientes.nombre,1,INDEX(clientes.nombre," ")).
        OtroNombreTitular = SUBSTRING(clientes.nombre,INDEX(clientes.nombre," ") + 1).
    END.
    ELSE DO:
        ASSIGN razonSocial = clientes.nombre + " " + clientes.apellido1 + clientes.apellido2
               vApellido1 = ""
               vApellido2 = ""
               NombreTitular = ""
               OtroNombreTitular = "".
    END.

    consecutivo = consecutivo + 1.
    varRegistro = STRING(consecutivo,"9999999999")                   +   /* Consecutivo */
                  STRING(YEAR(clientes.FecIni_NoSipla),"9999") + "-"   +
                  STRING(MONTH(clientes.FecIni_NoSipla),"99") + "-"    +
                  STRING(DAY(clientes.FecIni_NoSipla),"99")            +   /* Fecha de inicio de exoneración */
                  STRING(tipoId,"X(2)")                                +   /* Tipo de documento de identificación del Cliente */
                  STRING(numId,"X(20)")                                +   /* Nümero de identificación */
                  STRING(vApellido1,"X(40)")                            +   /* Primer apellido */
                  STRING(vApellido2,"X(40)")                            +   /* Segundo Apellido */
                  STRING(NombreTitular,"X(40)")                        +   /* Primer nombre del titular de la cuenta */
                  STRING(OtroNombreTitular,"X(40)")                    +   /* Otros nombres del titular de la cuenta */
                  STRING(razonSocial,"X(60)").                             /* Razón social de la empresa titular de la cuenta */

    CREATE registrosUIAF.
    ASSIGN registrosUIAF.consecutivo = consecutivo
           registrosUIAF.registro = varRegistro.
END.

listado = W_pathspl + SSTTTCCC + STRING(MONTH(fecCorte),"99") + STRING(YEAR(fecCorte),"9999") + ".txt".
OS-DELETE VALUE(listado).
OUTPUT TO VALUE(listado).

/* Encabezado del archivo */
  reg_encabezado = "0000000000" +   /* Consecutivo */
                   SSTTTCCC +   /* Código de la entidad */
                   STRING(YEAR(fechaIni),"9999") + "-" +
                   STRING(MONTH(fechaIni),"99") + "-" +
                   STRING(DAY(fechaIni),"99")               +    /* Fecha de corte inicial */
                   STRING(YEAR(fechaFin),"9999") + "-" +
                   STRING(MONTH(fechaFin),"99") + "-" +
                   STRING(DAY(fechaFin),"99")               +    /* Fecha de corte final */
                   STRING(consecutivo,"9999999999")       +     /* Consecutivo */
                   "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX".
  PUT UNFORMATTED
      reg_encabezado SKIP(0).
  /* ----- */

  /* Registros detalles */
  FOR EACH RegistrosUIAF:
     PUT UNFORMATTED registro SKIP(0).
  END.
  /* ----- */

  /* Fin del archivo */
  reg_fin = "0000000000" +  /* Consecutivo */
            SSTTTCCC +  /* Código de la entidad */
            STRING(consecutivo,"9999999999") +     /* Consecutivo */
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX".

  PUT UNFORMATTED
    reg_fin SKIP(0).

  OUTPUT CLOSE.
  MESSAGE "Se ha generado el Informe de Clientes Exonerados en la siguiente carpeta: " SKIP
          TRIM(listado)
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inf_Individual wWin 
PROCEDURE Inf_Individual :
Listado = /*W_PathSpl +*/ "c:\info\Empresas.LST".  
OS-DELETE VALUE(Listado).
DEFINE VAR WEnc1 AS CHARACTER FORMAT "X(75)".
DEFINE VAR WEnc2 AS CHARACTER FORMAT "X(2)".
DEFINE VAR WNEmp AS CHARACTER FORMAT "X(30)".
DEFINE VAR NomEmp AS CHARACTER FORMAT "X(50)".
DEFINE VAR TotMon AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
DEFINE VAR TotCuo AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
DEFINE VAR TTotMon AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
DEFINE VAR TTotCuo AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
DEFINE VAR EmpIni LIKE Empresas.Cod_Empresa.
DEFINE VAR EmpFin LIKE Empresas.Cod_Empresa.
DEFINE VAR TotReg AS DECIMAL.
DEFINE VAR XLinea1 AS CHARACTER FORMAT "X(100)".
DEFINE VAR XLinea2 AS CHARACTER FORMAT "X(100)".
DEFINE VAR XLinea3 AS CHARACTER FORMAT "X(100)".
DEFINE VAR XLinea4 AS CHARACTER FORMAT "X(100)".
DEFINE VAR XLinea5 AS CHARACTER FORMAT "X(100)".

ASSIGN FRAME FDetCon WNomAgencia WNomCliente WNomUsuReporta WNomUsuGestiona
             ConsEfectivo ConsCheque RetEfectivo RetCheque AboCreditosEfe AboCreditosChe.
OUTPUT TO value(Listado) NO-ECHO PAGED PAGE-SIZE 65.
{Incluido\RepEncabezado.i}
  W_Reporte   = "REPORTE   : REPORTE DE REGISTRO INDIVIDUAL"
              + " - Fecha: " + STRING(W_Fecha) + " - " + STRING(TIME,"hh:mm am").
  W_EncColumna = " Instancia: "  + Cmb_Instancias:SCREEN-VALUE IN FRAME FSipla.

  VIEW FRAME F-Encabezado.
  ASSIGN XLinea1 = SUBSTRING(Mov_InsSipla.Descripcion,1,70)
         XLinea2 = SUBSTRING(Mov_InsSipla.Descripcion,71,70)
         XLinea3 = SUBSTRING(Mov_InsSipla.Descripcion,141,70)
         XLinea4 = SUBSTRING(Mov_InsSipla.Descripcion,211,70)
         XLinea5 = SUBSTRING(Mov_InsSipla.Descripcion,281,70).
  DISPLAY " Agencia de Origen       :   " Mov_InsSipla.Agencia WNomAgencia SKIP
          " No.Identificacion       :   " Mov_InsSipla.Nit                 SKIP
          " Nombre del Cliente      :   " WNomCliente                      SKIP
          " Fecha de Transaccion    :   " Mov_InsSipla.Fecha_Transaccion   SKIP
          " Fecha de Gestion        :   " Mov_InsSipla.Fecha_Gestion       SKIP
          " Id. No Usual Dia        :   " Mov_InsSipla.Id_Nud              SKIP
          " Id. No Usual Mes        :   " Mov_InsSipla.Id_Num              SKIP
          " Catalogado Normal       :   " Mov_InsSipla.Id_Exonerada        SKIP
          "            Sospechoso   :   " Mov_InsSipla.Id_Sospechosa       SKIP
          "            Rep.Fiscalia :   " Mov_InsSipla.Id_RepUIAF          SKIP
          " Descripcion Instancia   :   "                                  SKIP(1)
          XLinea1 SKIP
          XLinea2 SKIP
          XLinea3 SKIP
          XLinea4 SKIP
          XLinea5 SKIP(1)
          " Cons. Efectivo :   " ConsEfectivo    
          " Cons. Cheque   :   " ConsCheque      SKIP
          " Ret.  Efectivo :   " RetEfectivo     
          " Ret.  Cheque   :   " RetCheque       SKIP
          " Abo.Cre.Efect  :   " AboCreditosEfe  
          " Abo.Cre.Cheque :   " AboCreditosChe  SKIP
          "----------------------------------------------------------------------------------"
  WITH FRAME FMov WIDTH 132 NO-LABELS USE-TEXT NO-BOX STREAM-IO.
  VIEW FRAME F-Ftr.
  PAGE.
OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inf_Productos wWin 
PROCEDURE Inf_Productos :
/* Reporte a la UIAF de los Productos de inversión */

/*OS-DELETE VALUE(Listado).*/
DEFINE VAR fechaIni AS DATE.
DEFINE VAR fechaFin AS DATE.
DEFINE VAR numProducto AS CHARACTER.
DEFINE VAR tipoProducto AS CHARACTER.
DEFINE VAR codDepartamento AS CHARACTER.
DEFINE VAR numId AS CHARACTER.
DEFINE VAR numId2 AS CHARACTER.
DEFINE VAR reg_encabezado AS CHARACTER.
DEFINE VAR reg_fin AS CHARACTER.
DEFINE VAR vRegistro AS CHARACTER.
DEFINE VAR tipoId AS CHARACTER.
DEFINE VAR tipoId2 AS CHARACTER.
DEFINE VAR i AS INTEGER INITIAL 0.
DEFINE VAR vApellido1 AS CHARACTER FORMAT "X(40)" INITIAL "                                        ".
DEFINE VAR vApellido2 AS CHARACTER FORMAT "X(40)" INITIAL "                                        ".
DEFINE VAR NombreTitular AS CHARACTER FORMAT "X(40)" INITIAL "                                        ".
DEFINE VAR OtroNombreTitular AS CHARACTER FORMAT "X(40)" INITIAL "                                        ".
DEFINE VAR razonSocial AS CHARACTER FORMAT "X(60)" INITIAL "                                                            ".
DEFINE VAR vApellido1_2 AS CHARACTER FORMAT "X(40)" INITIAL "                                        ".
DEFINE VAR vApellido2_2 AS CHARACTER FORMAT "X(40)" INITIAL "                                        ".
DEFINE VAR NombreTitular2 AS CHARACTER FORMAT "X(40)" INITIAL "                                        ".
DEFINE VAR OtroNombreTitular2 AS CHARACTER FORMAT "X(40)" INITIAL "                                        ".
DEFINE VAR razonSocial2 AS CHARACTER FORMAT "X(60)" INITIAL "                                                            ".

/* ----- */

FOR EACH registrosUIAF:
    DELETE registrosUIAF.
END.

ASSIGN FRAME FImprimir TIncluir FecCorte.
ASSIGN fechafin = FecCorte
       fechaIni = FecCorte - DAY(FecCorte) + 1.

ASSIGN w_consecutivo = 0.
       
/* Creacion de registros en la tabla temporal con todos los productos */
FOR EACH pro_ahorros WHERE pro_ahorros.subtipo >= 3 NO-LOCK:
    FOR EACH ahorros WHERE ahorros.cod_ahorro = pro_ahorros.cod_ahorro NO-LOCK:
        IF ahorros.detalle_estado <> 9 OR (ahorros.detalle = 9 AND
                                           (ahorros.sdo_disponible + ahorros.sdo_canje) > 0) THEN DO:
            /* Se limpian variables */
            razonSocial = "".
            razonSocial2 = "".
            tipoId = "".
            tipoId2 = "".
            numId = "".
            numId2 = "".
            vApellido1 = "".
            vApellido2 = "".
            vApellido1_2 = "".
            vApellido2_2 = "".
            NombreTitular = "".
            OtroNombreTitular = "".
            NombreTitular2 = "".
            OtroNombreTitular2 = "".
            

            /* Se carga el tipo de producto */
            CASE ahorros.tip_ahorro:
                WHEN 1 THEN tipoProducto = "02".
                WHEN 2 THEN tipoProducto = "02".
                WHEN 3 THEN DO:
                    IF ahorros.cod_ahorro = 14 OR
                       ahorros.cod_ahorro = 223 OR
                       ahorros.cod_ahorro = 228 THEN tipoProducto = "19".
                    ELSE DO:
                        IF ahorros.cod_ahorro = 224 OR
                           ahorros.cod_ahorro = 225 OR
                           ahorros.cod_ahorro = 226 OR 
                           ahorros.cod_ahorro = 229 OR
                           ahorros.cod_ahorro = 231 THEN tipoProducto = "66".
                        ELSE
                            tipoProducto = "09".    /* Tipo de producto "Otro" */
                    END.
                END.
            END CASE.

            /* Se carga el código del Departamento en el cual se dio apertura al producto */
            FIND FIRST agencias WHERE agencias.agencia = ahorros.agencia NO-LOCK NO-ERROR.
            IF AVAILABLE agencias THEN
                codDepartamento = SUBSTRING(agencias.ciudad,1,5).

            /* Se carga el tipo de identificación */
            FIND FIRST clientes WHERE clientes.nit = ahorros.nit NO-LOCK NO-ERROR.
            IF AVAILABLE clientes THEN DO:
                CASE clientes.Tipo_Identificacion:
                    WHEN "C.C" THEN tipoId = "13".
                    WHEN "C.E" THEN tipoId = "21".
                    WHEN "NIT" THEN tipoId = "31".
                    WHEN "T.I" THEN tipoId = "12".
                    WHEN "NUI" THEN tipoId = "00".
                END CASE.

                /* Se elimina el caracter "-" del campo Nit y se carga el número de identificación */
                IF clientes.tipo_ident EQ "NIT" THEN DO:
                    REPEAT i = 1 TO 15:
                        IF SUBSTRING(clientes.nit,i,1) NE "-" THEN
                            numId = TRIM(numId) + SUBSTRING(clientes.nit,i,1).
                        ELSE
                            i = 20.
                    END.
                END.
                ELSE
                    ASSIGN numId = TRIM(clientes.nit).

                /* Se cargan los apellidos, nombre y/o razón social del titular */
                IF clientes.tipo_identificacion <> "NIT" THEN DO:
                    vApellido1 = clientes.Apellido1.
                    vApellido2 = clientes.Apellido2.
                    NombreTitular = SUBSTRING(clientes.nombre,1,INDEX(clientes.nombre," ")).
                    OtroNombreTitular = SUBSTRING(clientes.nombre,INDEX(clientes.nombre," ") + 1).
                END.
                ELSE
                    ASSIGN  razonSocial = clientes.nombre /*+ " " + clientes.apellido1 + clientes.apellido2*/
                            vApellido1 = ""
                            vApellido2 = ""
                            NombreTitular = ""
                            OtroNombreTitular = "".
            END.
    
            /* Se busca un posible segundo segundo titular para el producto */
            FIND FIRST relaciones WHERE relaciones.nit = ahorros.nit
                                    AND relaciones.cod_relacion = 13    /* Código de relación para Titularidad */
                                    AND relaciones.clase_producto = 1   /* Clase para los productos de ahorro */
                                    AND relaciones.cod_producto = ahorros.cod_ahorro
                                    AND relaciones.cuenta = ahorros.cue_ahorros NO-LOCK NO-ERROR.
            IF AVAILABLE relaciones THEN DO:
                /* Se carga el tipo de identificación para el segundo titular */
                FIND FIRST clientes WHERE clientes.nit = relaciones.nit_Relacion NO-LOCK NO-ERROR.
                IF AVAILABLE clientes THEN DO:
                    CASE clientes.Tipo_Identificacion:
                        WHEN "C.C" THEN tipoId2 = "13".
                        WHEN "C.E" THEN tipoId2 = "21".
                        WHEN "NIT" THEN tipoId2 = "31".
                        WHEN "T.I" THEN tipoId2 = "12".
                        WHEN "NUI" THEN tipoId2 = "00".
                    END CASE.
                
                    /* Se elimina el caracter "-" del campo Nit y se carga el número de identificación para el segundo titular */
                    IF clientes.tipo_ident EQ "NIT" THEN DO:
                        REPEAT i = 1 TO 15:
                            IF SUBSTRING(clientes.nit,i,1) NE "-" THEN
                                numId2 = TRIM(numId2) + SUBSTRING(clientes.nit,i,1).
                            ELSE
                                i = 20.
                        END.
                    END.
                    ELSE
                        ASSIGN numId2 = TRIM(clientes.nit).

                    /* Se cargan los apellidos, nombre y/o razón social del segundo titular */
                    IF clientes.tipo_identificacion <> "NIT" THEN DO:
                        vApellido1_2 = clientes.Apellido1.
                        vApellido2_2 = clientes.Apellido2.
                        NombreTitular2 = SUBSTRING(clientes.nombre,1,INDEX(clientes.nombre," ")).
                        OtroNombreTitular2 = SUBSTRING(clientes.nombre,INDEX(clientes.nombre," ") + 1).
                    END.
                    ELSE
                        ASSIGN razonSocial = clientes.nombre /*+ " " + clientes.apellido1 + clientes.apellido2*/
                               vApellido1 = ""           
                               vApellido2 = ""           
                               NombreTitular = ""        
                               OtroNombreTitular = "".   
                END.
            END.

    
            /* Se configura el registro detalle */
            w_consecutivo = w_consecutivo + 1.
            vRegistro = STRING(w_consecutivo,"9999999999")                   +   /* Consecutivo */
                        STRING(ahorros.cue_ahorros,"X(20)")                  +   /* Número del producto */
                        STRING(YEAR(ahorros.fec_apertura),"9999") + "-"      +
                        STRING(MONTH(ahorros.fec_apertura),"99") + "-"       +
                        STRING(DAY(ahorros.fec_apertura),"99")               +   /* Fecha de inicio de exoneración */
                        STRING(tipoProducto,"X(2)")                          +   /* Tipo de producto */
                        STRING(codDepartamento,"X(5)")                       +   /* Código del Departamento */
                        STRING(tipoId,"X(2)")                                +   /* Tipo de documento de identificación del primer titular */
                        STRING(numId,"X(20)")                                +   /* Nümero de identificación del primer titular */
                        STRING(vApellido1,"X(40)")                           +   /* Primer apellido del primer titular */
                        STRING(vApellido2,"X(40)")                           +   /* Segundo Apellido del segundo titular */
                        STRING(NombreTitular,"X(40)")                        +   /* Primer nombre del primer titular de la cuenta */
                        STRING(OtroNombreTitular,"X(40)")                    +   /* Otros nombres del primer titular de la cuenta */
                        STRING(razonSocial,"X(60)")                          +   /* Razón social de la empresa primer titular de la cuenta */
                        STRING(tipoId2,"X(2)")                               +   /* Tipo de documento de identificación del primer titular */
                        STRING(numId2,"X(20)")                               +   /* Nümero de identificación del primer titular */
                        STRING(vApellido1_2,"X(40)")                         +   /* Primer apellido del primer titular */
                        STRING(vApellido2_2,"X(40)")                         +   /* Segundo Apellido del segundo titular */
                        STRING(NombreTitular2,"X(40)")                       +   /* Primer nombre del primer titular de la cuenta */
                        STRING(OtroNombreTitular2,"X(40)")                   +   /* Otros nombres del primer titular de la cuenta */
                        STRING(razonSocial2,"X(60)")                         +   /* Razón social de la empresa primer titular de la cuenta */
                        STRING(ahorros.estado,"99") + "--"                   +   /* Estado */
                        STRING(ahorros.Detalle_Estado,"99") + "--"           +   /* Detalle Estado */
                        STRING(ahorros.agencia,"99") + "--"                  +   /* Agencia */
                        STRING(ahorros.Sdo_Disponible).

            CREATE registrosUIAF.
            ASSIGN registrosUIAF.consecutivo = w_consecutivo
                   registrosUIAF.registro = vRegistro.
        END.
    END.
END.

listado = W_pathspl + SSTTTCCC + STRING(MONTH(fecCorte),"99") + STRING(YEAR(fecCorte),"9999") + ".txt".
OS-DELETE VALUE(listado).
OUTPUT TO VALUE(listado).

/* Encabezado del archivo */
  reg_encabezado = "0000000000" +   /* Consecutivo */
                   SSTTTCCC +   /* Código de la entidad */
                   STRING(YEAR(fechaIni),"9999") + "-" +
                   STRING(MONTH(fechaIni),"99") + "-" +
                   STRING(DAY(fechaIni),"99")               +    /* Fecha de corte inicial */
                   STRING(YEAR(fechaFin),"9999") + "-" +
                   STRING(MONTH(fechaFin),"99") + "-" +
                   STRING(DAY(fechaFin),"99")               +    /* Fecha de corte final */
                   STRING(w_consecutivo,"9999999999")       +    /* Consecutivo */
                   "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX".
  PUT UNFORMATTED
      reg_encabezado SKIP(0).
  /* ----- */

  /* Registros detalles */
  FOR EACH RegistrosUIAF BY registrosUIAF.consecutivo:
     PUT UNFORMATTED RegistrosUIAF.registro SKIP(0).
  END.
  /* ----- */

  /* Fin del archivo */
  reg_fin = "0000000000"    +   /* Consecutivo */
            SSTTTCCC        +   /* Código de la entidad */
            STRING(w_consecutivo,"9999999999") +     /* Consecutivo */
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX".

  PUT UNFORMATTED
    reg_fin SKIP(0).

  OUTPUT CLOSE.
  MESSAGE "Se ha generado el Reporte de Productos en la siguiente carpeta: " SKIP
          TRIM(listado)
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inf_UIAF wWin 
PROCEDURE Inf_UIAF :
/*OS-DELETE VALUE(Listado).*/

DEFINE VAR WEnc2 AS CHARACTER FORMAT "X(2)".
DEFINE VAR WNEmp AS CHARACTER FORMAT "X(30)".
DEFINE VAR FInicial AS DATE.
DEFINE VAR FFinal   AS DATE.
DEFINE VAR TConAho LIKE Ahorros.Sdo_Disponible.
DEFINE VAR TRetAho LIKE Ahorros.Sdo_Disponible.
DEFINE VAR TAboCre LIKE Ahorros.Sdo_Disponible.
DEFINE VAR zcadena AS CHARACTER FORMAT "X(89)".
DEFINE VAR cedTConAho AS CHARACTER.
DEFINE VAR cedTRetAho AS CHARACTER.
DEFINE VAR cedTAboCre AS CHARACTER.
DEFINE VAR nomTConAho AS CHARACTER.
DEFINE VAR nomTRetAho AS CHARACTER.
DEFINE VAR nomTAboCre AS CHARACTER.
DEFINE VAR reg_encabezado AS CHARACTER.
DEFINE VAR reg_fin AS CHARACTER.

FOR EACH RegistrosUIAF:
    DELETE RegistrosUIAF.
END.

MESSAGE "¿El reporte generado será el reporte definitivo?" SKIP
        "Si es así, haga clic en el botón 'Si' (los registros" SKIP
        "quedarán marcados y no se volverán a reportar)"
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "" UPDATE ReporteDefinitivo AS LOGICAL.

ASSIGN FRAME FImprimir TIncluir FecCorte.
ASSIGN FFinal = FecCorte
       FInicial = FecCorte - DAY(FecCorte) + 1.

IF TIncluir THEN
   FInicial = DATE("01/01/" + STRING(YEAR(W_Fecha))).

DO TRANSACTION:
    /* OUTPUT TO value(Listado) NO-ECHO PAGED PAGE-SIZE 65.
    {Incluido\RepEncabezado.i}
    W_Reporte   = "REPORTE   : REPORTE UIAF "  
    + " - Fecha: " + STRING(W_Fecha) + " - " + STRING(TIME,"hh:mm am").
    VIEW FRAME F-Encabezado.                                                       */
    ASSIGN w_consecutivo = 0
           w_vlrTotal = 0.

    /*creacion de registros al plano de los que sobrepasaron el valor mensual (reg automaticos)*/
    FOR EACH TConsulta WHERE TConsulta.Id_NUM
                         AND TConsulta.TipReg = "A"
                         AND TConsulta.FecRep >= FInicial
                         AND TConsulta.FecRep <= FFinal
                         AND TConsulta.Instancia = 880 BREAK BY TConsulta.Nit:    /* Se reportan únicamente las que están en la última instancia */
        IF FIRST-OF(TConsulta.Nit) THEN
            FIND Clientes WHERE Clientes.Nit EQ TConsulta.Nit NO-LOCK NO-ERROR.

        FIND Mov_InsSipla WHERE Mov_InsSipla.Agencia EQ TConsulta.Agencia
                            AND Mov_InsSipla.Instancia EQ TConsulta.Instancia
                            AND Mov_InsSipla.Nit EQ TConsulta.Nit
                            AND Mov_InsSipla.Fecha_Transaccion EQ TConsulta.FecRep
                            AND Mov_InsSipla.Tipo_Registro EQ "A" NO-ERROR.
        IF AVAILABLE Mov_InsSipla THEN
            IF Mov_InsSipla.Fec_RepUIAF NE ? THEN
                NEXT.

        RUN TransaccionesReporte(INPUT TConsulta.Nit,
                                 INPUT FInicial,
                                 INPUT FFinal,
                                 OUTPUT TConAho,
                                 OUTPUT TRetAho,
                                 OUTPUT TAboCre,
                                 OUTPUT cedTConAho,
                                 OUTPUT cedTRetAho,
                                 OUTPUT cedTAboCre,
                                 OUTPUT nomTConAho,
                                 OUTPUT nomTRetAho,
                                 OUTPUT nomTAboCre).

        IF TConAho GT Entidad.MaxOp_Efectivo_Mes THEN
            RUN Registro_UIAF(INPUT TConsulta.Nit,
                              INPUT TConAho,
                              INPUT Clientes.Agencia,
                              INPUT "CA",
                              INPUT FFinal,
                              INPUT cedTConAho,
                              INPUT nomTConAho).

        IF TRetAho GT Entidad.MaxOp_Efectivo_Mes THEN
            RUN Registro_UIAF(INPUT TConsulta.Nit,
                              INPUT TRetAho,
                              INPUT Clientes.Agencia,
                              INPUT "RA",
                              INPUT FFinal,
                              INPUT cedTRetAho,
                              INPUT nomTRetAho).

        IF TAboCre GT Entidad.MaxOp_Efectivo_Mes THEN
            RUN Registro_UIAF (INPUT TConsulta.Nit,
                               INPUT TAboCre,
                               INPUT Clientes.Agencia,
                               INPUT "AC",
                               INPUT FFinal,
                               INPUT cedTAboCre,
                               INPUT nomTAboCre).

        IF reporteDefinitivo = YES THEN
            ASSIGN Mov_InsSipla.Fec_RepUIAF = FecCorte
                   Mov_InsSipla.Id_RepUIAF = YES.

        CREATE TReportadosM.
        ASSIGN TReportadosM.Nit = TConsulta.Nit.
    END.

    /*creacion de registros al plano de los que sobrepasaron el valor diario (reg automaticos)*/
    FOR EACH TConsulta WHERE TConsulta.Id_NUD
                         AND NOT TConsulta.Id_NUM
                         AND TConsulta.IdUIAF
                         AND TConsulta.TipReg EQ "A"
                         AND TConsulta.FecRep GE FInicial
                         AND TConsulta.FecRep LE FFinal BREAK BY TConsulta.Nit:
        ASSIGN FInicial = TConsulta.FecRep
               FFinal = TConsulta.FecRep.

        FIND TReportadosM WHERE TReportadosM.Nit EQ TConsulta.Nit NO-LOCK NO-ERROR.
        IF AVAILABLE TReportadosM THEN
            NEXT.

        FIND Mov_InsSipla WHERE Mov_InsSipla.Agencia EQ TConsulta.Agencia
                            AND Mov_InsSipla.Instancia EQ TConsulta.Instancia
                            AND Mov_InsSipla.Nit EQ TConsulta.Nit
                            AND Mov_InsSipla.Fecha_Transaccion EQ TConsulta.FecRep
                            AND Mov_InsSipla.Tipo_Registro EQ "A" NO-ERROR.
        IF AVAILABLE Mov_InsSipla THEN
            IF Mov_InsSipla.Fec_RepUIAF NE ? THEN
                NEXT.

        IF FIRST-OF(TConsulta.Nit) THEN
            FIND Clientes WHERE Clientes.Nit EQ TConsulta.Nit NO-LOCK NO-ERROR.
            
        RUN TransaccionesReporte(INPUT TConsulta.Nit,
                                 INPUT FInicial,
                                 INPUT FFinal,
                                 OUTPUT TConAho,
                                 OUTPUT TRetAho,
                                 OUTPUT TAboCre,
                                 OUTPUT cedTConAho,
                                 OUTPUT cedTRetAho,
                                 OUTPUT cedTAboCre,
                                 OUTPUT nomTConAho,
                                 OUTPUT nomTRetAho,
                                 OUTPUT nomTAboCre).

        IF TConAho GT Entidad.MaxOp_Efectivo_Dia THEN
            RUN Registro_UIAF(INPUT TConsulta.Nit,
                              INPUT TConAho,
                              INPUT Clientes.Agencia,
                              INPUT "CA",
                              INPUT FInicial,
                              INPUT cedTConAho,
                              INPUT nomTConAho).

        IF TRetAho GT Entidad.MaxOp_Efectivo_Dia THEN
            RUN Registro_UIAF(INPUT TConsulta.Nit,
                              INPUT TRetAho,
                              INPUT Clientes.Agencia,
                              INPUT "RA",
                              INPUT FInicial,
                              INPUT cedTRetAho,
                              INPUT nomTRetAho).

        IF TAboCre GT Entidad.MaxOp_Efectivo_Dia THEN
            RUN Registro_UIAF(INPUT TConsulta.Nit,
                              INPUT TAboCre,
                              INPUT Clientes.Agencia,
                              INPUT "AC",
                              INPUT FInicial,
                              INPUT cedTAboCre,
                              INPUT nomTAboCre).

        IF reporteDefinitivo = YES THEN
            ASSIGN Mov_InsSipla.Fec_RepUIAF = FecCorte
                   Mov_InsSipla.Id_RepUIAF = YES.
    END.

    FOR EACH TReportadosM:
        DELETE TReportadosM.
    END.

    FOR EACH TConsulta WHERE TConsulta.Id_NUD
                         AND NOT TConsulta.Id_NUM
                         AND TConsulta.IdUIAF
                         AND TConsulta.TipReg EQ "M"
                         AND TConsulta.FecRep GE FInicial
                         AND TConsulta.FecRep LE FFinal BREAK BY TConsulta.Nit:
        ASSIGN FInicial = TConsulta.FecRep
               FFinal = TConsulta.FecRep.

        FIND Mov_InsSipla WHERE Mov_InsSipla.Agencia EQ TConsulta.Agencia
                            AND Mov_InsSipla.Instancia EQ TConsulta.Instancia
                            AND Mov_InsSipla.Nit EQ TConsulta.Nit
                            AND Mov_InsSipla.Fecha_Transaccion EQ TConsulta.FecRep
                            AND Mov_InsSipla.Tipo_Registro EQ "M" NO-ERROR.
        IF AVAILABLE Mov_InsSipla THEN
            IF Mov_InsSipla.Fec_RepUIAF NE ? THEN
                NEXT.

        IF FIRST-OF(TConsulta.Nit) THEN
            FIND Clientes WHERE Clientes.Nit EQ TConsulta.Nit NO-LOCK NO-ERROR.

        RUN Registro_UIAF(INPUT TConsulta.Nit,
                          Mov_InsSipla.Valor_RegManual,
                          Clientes.Agencia,
                          "CA",
                          Mov_InsSipla.Fecha_Transaccion).

        IF reporteDefinitivo THEN
            ASSIGN Mov_InsSipla.Fec_RepUIAF = FecCorte
                   Mov_InsSipla.Id_RepUIAF = YES.
    END.

    listado = W_pathspl + "FINJUR" + STRING(MONTH(Feccorte),"99") + SUBSTRING(STRING(YEAR(feccorte),"9999"),3,2) + ".txt".

    OS-DELETE VALUE(listado).
    
    OUTPUT TO VALUE(listado).
        /* Encabezado del archivo */
        reg_encabezado = "0000000000" +   /* Consecutivo */
                         SSTTTCCC +   /* Código de la entidad */
                         STRING(YEAR(fecCorte),"9999") + "-"  +
                         STRING(MONTH(fecCorte),"99") + "-"   +
                         STRING(DAY(fecCorte),"99")           +    /* Fecha de corte */
                         STRING(w_consecutivo,"9999999999")   +    /* Consecutivo */
                         "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX".

        PUT UNFORMATTED
            reg_encabezado SKIP(0).
        /* ----- */

        /* Registros detalles */
        FOR EACH RegistrosUIAF BY consecutivo:
            PUT UNFORMATTED RegistrosUIAF.registro SKIP(0).
        END.
        /* ----- */

        /* Fin del archivo */
        reg_fin = "0000000000" +  /* Consecutivo */
                  SSTTTCCC +  /* Código de la entidad */
                  STRING(w_consecutivo,"9999999999") +     /* Consecutivo */
                  "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX".

        PUT UNFORMATTED
            reg_fin SKIP(0).
    OUTPUT CLOSE.

    MESSAGE "Se ha generado UIAF en la siguiente carpeta " SKIP
            TRIM(listado)
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    /*VIEW FRAME F-Ftr.
    PAGE.
    OUTPUT CLOSE.*/
END. /*fin transaccion*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
DEFINE VAR Xok AS LOGICAL INITIAL YES.
FIND Entidad WHERE Entidad.Entidad EQ W_Entidad NO-LOCK NO-ERROR.
  FIND FIRST Cfg_Instancias WHERE
       Cfg_Instancias.Agencia        EQ W_Agencia AND
       Cfg_Instancias.Tipo_Instancia EQ 6         AND
       Cfg_Instancias.Usuario        EQ W_Usuario AND
       Cfg_Instancias.Estado         EQ 1 NO-LOCK NO-ERROR.
  IF NOT AVAILABLE Cfg_Instancia THEN DO:
     MESSAGE "Su usuario no tiene la instancia necesaria" SKIP
             "que le permite trabajar en este programa" VIEW-AS ALERT-BOX INFORMATION.
     XOk = NO.
  END.

ASSIGN W_MaxDia = Entidad.MaxOp_Efectivo_Dia
       W_MaxMes = Entidad.MaxOp_Efectivo_Mes.
FOR EACH Instancias WHERE Instancias.Tipo_Instancia EQ 6 AND
                          Instancias.Estado         EQ 1
                          NO-LOCK BREAK BY Instancias.Orden:
     IF Instancias.Ultima     THEN W_Ultima = Instancias.Instancia.
     IF Instancias.Primera    THEN W_Primera = Instancias.Instancia.
     IF Instancias.Id_Negadas THEN W_Negadas = Instancias.Instancia.
     FIND FIRST Cfg_Instancias WHERE
          Cfg_Instancias.Agencia        EQ W_Agencia            AND
          Cfg_Instancias.Tipo_Instancia EQ 6                    AND
          Cfg_Instancias.Instancia      EQ Instancias.Instancia AND
          Cfg_Instancias.Usuario        EQ W_Usuario            AND
          Cfg_Instancias.Estado         EQ 1 NO-LOCK NO-ERROR.
     IF AVAILABLE Cfg_Instancias THEN
        W_Ok = Cmb_Instancias:ADD-LAST(STRING(Instancias.Instancia,"99999") + " - " + Instancias.Nom_Instancia) IN FRAME FSipla.
  END.
  FIND Usuarios WHERE Usuarios.Usuario EQ W_Usuario NO-LOCK NO-ERROR.
  ASSIGN NomUsuario = W_Usuario + " - " + Usuarios.Nombre.

  RUN SUPER.
  HIDE FRAME FCerradas.
  Cmb_Instancias:SCREEN-VALUE = Cmb_Instancias:ENTRY(1).
  APPLY "entry" TO Cmb_Instancias IN FRAME FSipla.
  APPLY "value-changed" TO Cmb_Instancias IN FRAME FSipla.
  IF NOT Xok THEN APPLY "choose" TO BtnDone IN FRAME FSipla. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Llenar_InfoAhorros wWin 
PROCEDURE Llenar_InfoAhorros :
Reg = 0.
DO WITH FRAME FInfoAhorros:
   RUN LL_InfAhorros(INPUT 1).
   RUN LL_InfAhorros(INPUT 2).
   RUN LL_InfAhorros(INPUT 3).
   RUN LL_InfAhorros(INPUT 4).
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Llenar_InfoCliente wWin 
PROCEDURE Llenar_InfoCliente :
Reg = 0.
FIND Clientes WHERE Clientes.Nit EQ TConsulta.Nit NO-LOCK NO-ERROR.
DO WITH FRAME FInfoCliente:
    IF AVAILABLE Clientes THEN DO:
        RUN LL_InfoSegmentacion.
        RUN LL_InfoUbicacion.
        RUN LL_InfoEconomica.
        RUN LL_InfoRelaciones.
        RUN LL_InfoFechas.
        RUN LL_InfoControl.
    END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Llenar_InfoCreditos wWin 
PROCEDURE Llenar_InfoCreditos :
DO WITH FRAME FInfoAhorros:
   RUN LL_InfCreditos(INPUT 1).
   RUN LL_InfCreditos(INPUT 2).
   RUN LL_InfCreditos(INPUT 3).
   RUN LL_InfCreditos(INPUT 4).
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LLenar_InfoHojaVida wWin 
PROCEDURE LLenar_InfoHojaVida :
DEFINE VAR Tmp AS CHARACTER FORMAT "X(50)".
DEFINE VAR Xo1 AS CHARACTER FORMAT "X(90)".
DEFINE VAR Xo2 AS CHARACTER FORMAT "X(90)".
DEFINE VAR Xo3 AS CHARACTER FORMAT "X(90)".
DEFINE VAR Xo4 AS CHARACTER FORMAT "X(90)".
DEFINE VAR Xo5 AS CHARACTER FORMAT "X(90)".
Arc = "c:\INFred\HV" + ".txt".

OUTPUT TO VALUE(Arc).
VIEW FRAME FEncHV.
FOR EACH Hoja_Vida WHERE 
         Hoja_Vida.Nit             EQ TConsulta.Nit AND
         Hoja_Vida.Asunto_Cumplido EQ NO NO-LOCK:
    ASSIGN Tmp = "Inconsistencia en el Encabezado".
    FIND Varios WHERE
         Varios.Tipo     EQ 18 AND
         Varios.Codigo   EQ Hoja_Vida.Codigo NO-LOCK NO-ERROR.
    IF AVAILABLE Varios THEN Tmp = Varios.Descripcion.
    ASSIGN xo1 = SUBSTRING(Hoja_Vida.Observacion,1,90)
           xo2 = SUBSTRING(Hoja_Vida.Observacion,91,90)
           xo3 = SUBSTRING(Hoja_Vida.Observacion,181,90)
           xo4 = SUBSTRING(Hoja_Vida.Observacion,271,90)
           xo5 = SUBSTRING(Hoja_Vida.Observacion,361,90).
    DISPLAY "Encabezado      :" Tmp SKIP
            "Fecha           :" Hoja_Vida.Fec_Grabacion STRING(Hoja_Vida.Hora_Grabacion,"HH:MM:SS") SKIP
            "Doc.Referencia  :" Hoja_Vida.DoctoRefer SKIP
            xo1 SKIP
            xo2 SKIP
            xo3 SKIP
            xo4 SKIP
            xo5 SKIP(2)
    WITH FRAME F_MHV WIDTH 100 NO-LABELS USE-TEXT NO-BOX STREAM-IO.
END.
VIEW FRAME FFTRHV.
PAGE.
OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LL_InfAhorros wWin 
PROCEDURE LL_InfAhorros :
DEFINE INPUT PARAMETER XTipo AS INTEGER.
FOR EACH Ahorros WHERE 
         Ahorros.Tip_Ahorro EQ XTipo AND
         Ahorros.Nit EQ TConsulta.Nit AND
         Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje GT 0 NO-LOCK:
    FIND Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ Ahorros.Cod_Ahorro NO-LOCK NO-ERROR.
    XTmp2 = STRING(Ahorros.Cod_Ahorro,"999") + " - ".
    IF AVAILABLE Pro_Ahorros THEN XTmp2 = Pro_Ahorros.Nom_Producto.
    FIND Agencias WHERE Agencias.Agencia EQ Ahorros.Agencia NO-LOCK NO-ERROR.
    XTmp = STRING(Ahorros.Agencia,"999") + " - ".
    IF AVAILABLE Agencias THEN XTmp = XTmp + Agencias.Nombre.

    XLin = "Agencia de Origen: " + STRING(XTmp,"X(22)") +
    "  Producto Ahorro  : " + STRING(XTmp2,"X(25)").
    RUN GraInfAho(INPUT XLin, INPUT XTipo).

    XLin = "Tasa             : " + STRING(Ahorros.Tasa,">>>9.9999") + "             " +
    "  Plazo            : " + STRING(Ahorros.Plazo,"999").
    RUN GraInfAho(INPUT XLin, INPUT XTipo).

    XLin = "Cuota            : " + STRING(Ahorros.Cuota,">>>>>>>>>>>9") + "          " +
    "  Fecha Apertura   : " + STRING(Ahorros.Fec_Apertura,"99/99/9999").
    RUN GraInfAho(INPUT XLin, INPUT XTipo).

    XLin = "Saldo Disponible : " + STRING(Ahorros.Sdo_Disponible,">>>>>>>>>>>9") + "          " +
    "  Saldo en Canje   : " + STRING(Ahorros.Sdo_Canje,">>>>>>>>>>>9").
    RUN GraInfAho(INPUT XLin, INPUT XTipo).
    RUN GraInfAho(INPUT "", INPUT XTipo).
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LL_InfCreditos wWin 
PROCEDURE LL_InfCreditos :
DEFINE INPUT PARAMETER XTipo AS INTEGER.
FOR EACH Creditos WHERE 
         Creditos.Tip_Credito EQ XTipo AND
         Creditos.Nit EQ TConsulta.Nit AND
         Creditos.Sdo_Capital + Creditos.Val_Atraso GT 0 NO-LOCK:
    FIND Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ Creditos.Cod_Credito NO-LOCK NO-ERROR.
    XTmp2 = STRING(Creditos.Cod_Credito,"999") + " - ".
    IF AVAILABLE Pro_Creditos THEN XTmp2 = XTmp2 + Pro_Creditos.Nom_Producto.
    FIND Agencias WHERE Agencias.Agencia EQ Creditos.Agencia NO-LOCK NO-ERROR.
    XTmp = STRING(Creditos.Agencia,"999") + " - ".
    IF AVAILABLE Agencias THEN XTmp = XTmp + Agencias.Nombre.

    XLin = "Agencia de Origen: " + STRING(XTmp,"X(22)") +
    "  Producto Credito : " + STRING(XTmp2,"X(25)").
    RUN GraInfCre(INPUT XLin, INPUT XTipo).

    XLin = "Tasa             : " + STRING(Creditos.Tasa,">>>9.9999") + "             " +
    "  Plazo            : " + STRING(Creditos.Plazo,"999").
    RUN GraInfCre(INPUT XLin, INPUT XTipo).

    XLin = "Cuota            : " + STRING(Creditos.Cuota,">>>>>>>>>>>9") + "          " +
    "  Fecha Desembolso : " + STRING(Creditos.Fec_Desembolso,"99/99/9999").
    RUN GraInfCre(INPUT XLin, INPUT XTipo).

    XLin = "Saldo Capital    : " + STRING(Creditos.Sdo_Capital,">>>>>>>>>>>9") + "          " +
    "  Valor Atraso     : " + STRING(Creditos.Val_Atraso,">>>>>>>>>>>9").
    RUN GraInfCre(INPUT XLin, INPUT XTipo).
    RUN GraInfCre(INPUT "", INPUT XTipo).
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LL_InfoControl wWin 
PROCEDURE LL_InfoControl :
XLin = "".
   XTmp = "No".
   IF Clientes.Sancionado THEN XTmp = "Si".
   XLin = "Sancionado       : " + STRING(XTmp,"X(2)") + "                       ".
   IF Clientes.Dias_Sancion NE ? THEN
      XLin = XLin + "  Dias Sancion     : " + STRING(Clientes.Dias_Sancion,"999").
   RUN GraInfCli(INPUT XLin, INPUT 6).

   CASE Clientes.Reestructurado:
       WHEN 1 THEN XTmp = "No Aplica".
       WHEN 2 THEN XTmp = "Reestructurado".
       WHEN 3 THEN XTmp = "No Reestructurado".
   END CASE.
   XLin = "".
   XLin = "Reestructurado   : " + STRING(XTmp,"X(25)").
   XTmp = "No".
   IF Clientes.Id_PuedeCodeudar THEN XTmp = "Si".
      XLin = XLin + "  Puede Codeudar   : " + STRING(XTmp,"X(5)").
   RUN GraInfCli(INPUT XLin, INPUT 6).

   CASE Clientes.Id_Privilegiado:
       WHEN 0 THEN XTmp = "No".
       WHEN 1 THEN XTmp = "Si".
   END CASE.
   XLin = "Privilegiado     : " + STRING(XTmp,"X(25)").
   XLin = XLin + "  Cod.Segmento     : " + STRING(Clientes.Cod_Segmento,"99").
   RUN GraInfCli(INPUT XLin, INPUT 6).

   XLin = "".
   XTmp = "No".
   IF Clientes.Reportado_Super THEN XTmp = "Si".
   XLin = "Reportado Super  : " + STRING(XTmp,"X(25)").
   XTmp = "No".
   IF Clientes.Aut_CentralRiesgo THEN XTmp = "Si".
   XLin = XLin + "  Aut.Cent.Riesgo  : " + STRING(XTmp,"X(5)").
   RUN GraInfCli(INPUT XLin, INPUT 6).

   XLin = "".
   XTmp = "No".
   IF Clientes.Id_Preexistentes THEN XTmp = "Si".
   XLin = "Enf.Preexistentes: " + STRING(XTmp,"X(25)").
   XTmp = "No".
   IF Clientes.Reportado_Fiscalia THEN XTmp = "Si".
   XLin = XLin + "  Repo.Fiscalia    : " + STRING(XTmp,"X(5)").
   RUN GraInfCli(INPUT XLin, INPUT 6).

   XLin = "".
   XTmp = "No".
   IF Clientes.Reportado_Procredito THEN XTmp = "Si".
   XLin = "Rep.Procredito   : " + STRING(XTmp,"X(25)").
   XLin = XLin + "  Calificacion     : " + STRING(Clientes.Calificacion,"99999").
   RUN GraInfCli(INPUT XLin, INPUT 6).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LL_InfoEconomica wWin 
PROCEDURE LL_InfoEconomica :
ASSIGN XTmp2 = "".
    XLin = "Ingresos Mensuales " + STRING(XTmp2,"X(25)")  + 
           "  Egresos Mensuales    " + STRING(XTmp2,"X(25)").
    RUN GraInfCli(INPUT XLin, INPUT 3).

    XLin = "  Salario        : " + STRING(Clientes.Salario,">>>>>>>>9") + "                " +
           "    Familiares     : " + STRING(Clientes.Gto_Familiar,">>>>>>>>9").
    RUN GraInfCli(INPUT XLin, INPUT 3).

    XLin = "  Arriendos      : " + STRING(Clientes.Ing_arriendos,">>>>>>>>9") + "                " +
           "    Arrendamiento  : " + STRING(Clientes.Gto_Arriendo,">>>>>>>>9").
    RUN GraInfCli(INPUT XLin, INPUT 3).

    XLin = "  Financieros    : " + STRING(Clientes.Ing_financieros,">>>>>>>>9") + "                " +
           "    Financieros    : " + STRING(Clientes.GtoFinanc_Indir,">>>>>>>>9").
    RUN GraInfCli(INPUT XLin, INPUT 3).

    XLin = "  Honorarios     : " + STRING(Clientes.Ing_Honorarios,">>>>>>>>9").
    RUN GraInfCli(INPUT XLin, INPUT 3).

    XLin = "  Otros Ingresos : " + STRING(Clientes.Ing_Otros,">>>>>>>>9").
    RUN GraInfCli(INPUT XLin, INPUT 3).

    XLin = "  Total          : " + STRING(Clientes.Salario
                                        + Clientes.Ing_Arriendos
                                        + Clientes.Ing_Financieros
                                        + Clientes.Ing_Honorarios
                                        + Clientes.Ing_Otros,">>>>>>>>9") + "                " +
           "                     " + STRING(Clientes.Gto_Familiar
                                          + Clientes.Gto_Arriendo
                                          + Clientes.GtoFinanc_Indir,">>>>>>>>9").
    RUN GraInfCli(INPUT XLin, INPUT 3).
    RUN GraInfCli(INPUT "", INPUT 3).

    XLin = "Activos            " + STRING(XTmp2,"X(25)")  + 
           "  Pasivos              " + STRING(XTmp2,"X(25)").
    RUN GraInfCli(INPUT XLin, INPUT 3).

    XLin = "  Propiedades    :  " + STRING(Clientes.Act_casa,">>>>>>>>>>>9") + "              " +
           " Sdo.Obligacion : " + STRING(Clientes.Sdo_Obligaciones,">>>>>>>>9").
    RUN GraInfCli(INPUT XLin, INPUT 3).

    XLin = "  Vehiculos      :     " + STRING(Clientes.Act_vehiculo,">>>>>>>>9").
    RUN GraInfCli(INPUT XLin, INPUT 3).
    XLin = "  Inversiones    :     " + STRING(Clientes.Act_inversion,">>>>>>>>9").
    RUN GraInfCli(INPUT XLin, INPUT 3).

    XLin = "  Total          : " + STRING(Clientes.Act_Casa
                                        + Clientes.Act_Vehiculo
                                        + Clientes.Act_Inversion,">>>>>>>>>>>>9") + "           " +
           "                     " + STRING(Clientes.Sdo_Obligaciones,">>>>>>>>9").
    RUN GraInfCli(INPUT XLin, INPUT 3).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LL_InfoFechas wWin 
PROCEDURE LL_InfoFechas :
XLin = "".
     IF Clientes.Fec_Ingreso NE ? THEN
        XLin = "Ing.Cooperativa  : " + STRING(Clientes.Fec_Ingreso,"99/99/9999") + "               ".
     IF Clientes.Fec_Retiro NE ? THEN
        XLin = XLin + "  Ret.Cooperativa  : " + STRING(Clientes.Fec_Retiro,"99/99/9999").
     RUN GraInfCli(INPUT XLin, INPUT 5).
     XLin = "".
     IF Clientes.Fec_UltActualiza NE ? THEN
        XLin = "Ult.Actualizacion: " + STRING(Clientes.Fec_UltActualiza,"99/99/9999") + "               ".
     IF Clientes.Fec_IngEmpresa NE ? THEN
        XLin = XLin + "  Ingreso Empresa  : " + STRING(Clientes.Fec_IngEmpresa,"99/99/9999").
     RUN GraInfCli(INPUT XLin, INPUT 5).

     XLin = "".
     IF Clientes.Fec_Calificacion NE ? THEN
        XLin = "Fec.Calificacion : " + STRING(Clientes.Fec_Calificacion,"99/99/9999") + "               ".
     IF Clientes.Fec_IniSancion NE ? THEN
        XLin = XLin + "  Inicio Sancion   : " + STRING(Clientes.Fec_IniSancion,"99/99/9999").
     RUN GraInfCli(INPUT XLin, INPUT 5).

     XLin = "".
     IF Clientes.Fec_Expedicion NE ? THEN
        XLin = "Exp.Doc.Identific: " + STRING(Clientes.Fec_Expedicion,"99/99/9999") + "               ".
     IF Clientes.Fec_Nacimiento NE ? THEN
        XLin = XLin + "  Fec.Nacimiento   : " + STRING(Clientes.Fec_Nacimiento,"99/99/9999").
     RUN GraInfCli(INPUT XLin, INPUT 5).

     XLin = "".
     IF Clientes.Fec_Fallecido NE ? THEN
        XLin = "Fec.Fallecido    : " + STRING(Clientes.Fec_Fallecido,"99/99/9999").
     RUN GraInfCli(INPUT XLin, INPUT 5).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LL_InfoRelaciones wWin 
PROCEDURE LL_InfoRelaciones :
DEFINE VAR WNit LIKE Clientes.Nit.
WNit = Clientes.Nit.
FOR EACH Relaciones WHERE 
         Relaciones.Nit EQ WNit NO-LOCK BREAK BY Relaciones.Nit:
    IF FIRST-OF(Relaciones.Nit) THEN Puntero = ROWID(Clientes).
    FIND Varios WHERE Varios.Tipo EQ 3 AND Varios.Codigo EQ Relaciones.Cod_Relacion NO-LOCK NO-ERROR.
    IF AVAILABLE Varios THEN DO:
       FIND Clientes WHERE Clientes.Nit EQ Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
       IF AVAILABLE Clientes THEN DO:
           XTmp = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2. 
           XLin = "Relacion         : " + STRING(Varios.Descripcion,"X(25)") +
                  "Fecha Relacion     : " + STRING(Relaciones.Fec_Ingreso,"99/99/9999").
           RUN GraInfCli(INPUT XLin, INPUT 4).
           XLin = "Nit              : " + STRING(Clientes.Nit,"X(25)") +
                  "Nombre             : " + STRING(XTmp,"X(50)").
           RUN GraInfCli(INPUT XLin, INPUT 4).
           XLin = "Telefono         : " + STRING(Clientes.Tel_Residencia,"X(25)") +
                  "Direccion          : " + STRING(Dir_Residencia,"X(50)").
           RUN GraInfCli(INPUT XLin, INPUT 4).
           RUN GraInfCli(INPUT "", INPUT 4).
       END.
    END.
END.
FIND Clientes WHERE ROWID(Clientes) EQ Puntero NO-LOCK.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LL_InfoSegmentacion wWin 
PROCEDURE LL_InfoSegmentacion :
/*llena segmentacion*/
        FIND Agencias WHERE Agencias.Agencia EQ Clientes.Agencia NO-LOCK NO-ERROR.
        CASE Clientes.Tipo_Vinculo:
            WHEN 1 THEN XTmp = "Asociado".
            WHEN 2 THEN XTmp = "Cliente No Asociado".
            WHEN 3 THEN XTmp = "Tercero".
            WHEN 4 THEN XTmp = "Proveedor".
        END CASE.
        XLin = "Agencia de Origen: " + STRING(Agencias.Nombre,"X(25)") +
        "  Tipo de Vinculo  : " + STRING(XTmp,"X(25)").
        RUN GraInfCli(INPUT XLin, INPUT 1).

        XLin = "Estado Civil     : " + STRING(Clientes.Est_Civil,"X(25)") +
        "  Personas a Cargo : " + STRING(Clientes.Per_Acargo,"999").
        RUN GraInfCli(INPUT XLin, INPUT 1).

        XLin = "Numero de Hijos  : " + STRING(Clientes.Num_Hijos,"999").
        RUN GraInfCli(INPUT XLin, INPUT 1).
        RUN GraInfCli(INPUT "", INPUT 1).

        XLin = "Retencion Fte    : " + STRING(Clientes.Id_Retencion) + "                      " +
        "  GranContribuyente: " + STRING(Clientes.Gran_Contribuyente).
        RUN GraInfCli(INPUT XLin, INPUT 1).
        
        XTmp = "".
        FIND Varios WHERE Varios.Tipo EQ 1 AND Varios.Codigo EQ Clientes.Cod_Profesion NO-LOCK NO-ERROR.
        IF AVAILABLE Varios THEN XTmp = Varios.Descripcion.
        XLin = "Nivel Educativo  : " + STRING(Clientes.Niv_Educativo,"X(25)") +
        "  Profesion        : " + STRING(XTmp,"X(25)").
        RUN GraInfCli(INPUT XLin, INPUT 1).

        ASSIGN XTmp= "" XTmp2 = "".
        FIND Empresas WHERE Empresas.Cod_Empresa EQ Clientes.Cod_Empresa NO-LOCK NO-ERROR.
        IF AVAILABLE Empresas THEN XTmp = Empresas.Alias_Empresa.
        FIND Varios WHERE Varios.Tipo EQ 2 AND Varios.Codigo EQ Clientes.Cod_Cargo NO-LOCK NO-ERROR.
        IF AVAILABLE Varios THEN XTmp2 = Varios.Descripcion.
        XLin = "Empresa          : " + STRING(XTmp,"X(25)") +
        "  Cargo            : " + STRING(XTmp2,"X(25)").
        RUN GraInfCli(INPUT XLin, INPUT 1).

        CASE Clientes.Tip_Contrato:
            WHEN 0 THEN XTmp = "Ninguno".
            WHEN 1 THEN XTmp = "Indefinido".
            WHEN 2 THEN XTmp = "Fijo".
            WHEN 3 THEN XTmp = "Labor Presetada".
            WHEN 4 THEN XTmp = "Prestacion de Servicios".
        END CASE.
        XLin = "Carnet           : " + STRING(Clientes.Carnet,"X(25)") +
        "  Tipo de Contrato : " + STRING(XTmp,"X(25)").
        RUN GraInfCli(INPUT XLin, INPUT 1).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LL_InfoUbicacion wWin 
PROCEDURE LL_InfoUbicacion :
/*llena ubicacion*/
        XLin = "Dir.Residencia   : " + STRING(Clientes.DIR_Residencia,"X(25)") +
               "  Tel.Residencia   : " + STRING(Clientes.Tel_Residencia,"X(25)").
        RUN GraInfCli(INPUT XLin, INPUT 2).
        XLin = "Tel.Residencia   : " + STRING(Clientes.Tel_Residencia,"X(25)")  + 
               "  email            : " + STRING(Clientes.email,"X(40)").
        RUN GraInfCli(INPUT XLin, INPUT 2).

        XTmp2 = "".
        FIND Ubicacion WHERE 
             Ubicacion.Ubicacion BEGINS SUBSTRING(Clientes.Lugar_Residencia,1,5) AND
             Ubicacion.Tipo EQ "C" NO-LOCK NO-ERROR.
        IF AVAILABLE Ubicacion THEN DO:
           XTmp2 = Ubicacion.Nombre.
           FIND Ubicacion WHERE Ubicacion.Ubicacion EQ Clientes.Lugar_Residencia AND
                Ubicacion.Tipo EQ "B" NO-LOCK NO-ERROR.
           IF AVAILABLE Ubicacion THEN DO: 
              XTmp2 = XTmp2 + "  " + Ubicacion.Nombre.
           END.
        END.

        XLin = "Lugar Residencia : " + STRING(XTmp2,"X(25)") +
               "  Codigo de Zona   : " + STRING(Clientes.Cod_Zona,"9999").
        RUN GraInfCli(INPUT XLin, INPUT 2).
        RUN GraInfCli(INPUT "", INPUT 2).

        XLin = "Dir.Comercial    : " + STRING(Clientes.DIR_Comercial,"X(25)") +
               "  Tel.Comercial    : " + STRING(Clientes.Tel_Comercial,"X(25)").
        RUN GraInfCli(INPUT XLin, INPUT 2).

        XTmp2 = "".
        FIND Ubicacion WHERE 
             Ubicacion.Ubicacion BEGINS SUBSTRING(Clientes.Lugar_Comercial,1,5) AND
             Ubicacion.Tipo EQ "C" NO-LOCK NO-ERROR.
        IF AVAILABLE Ubicacion THEN DO:
           XTmp2 = Ubicacion.Nombre.
           FIND Ubicacion WHERE Ubicacion.Ubicacion EQ Clientes.Lugar_Comercial AND
                Ubicacion.Tipo EQ "B" NO-LOCK NO-ERROR.
           IF AVAILABLE Ubicacion THEN DO: 
              XTmp2 = XTmp2 + "  " + Ubicacion.Nombre.
           END.
        END.
        XLin = "Lugar Comercial  : " + STRING(XTmp2,"X(25)").
        RUN GraInfCli(INPUT XLin, INPUT 2).


        ASSIGN XTmp = "" XTmp2 = "".
        IF Clientes.DIR_Correspondencia THEN XTmp = "Oficina".
        ELSE XTmp = "Residencia".

        FIND Ubicacion WHERE 
             Ubicacion.Ubicacion BEGINS SUBSTRING(Clientes.Lugar_Nacimiento,1,5) AND
             Ubicacion.Tipo EQ "C" NO-LOCK NO-ERROR.
        IF AVAILABLE Ubicacion THEN DO:
           XTmp2 = Ubicacion.Nombre.
           FIND Ubicacion WHERE Ubicacion.Ubicacion EQ Clientes.Lugar_Nacimiento AND
                Ubicacion.Tipo EQ "B" NO-LOCK NO-ERROR.
           IF AVAILABLE Ubicacion THEN 
           DO: 
              XTmp2 = XTmp2 + "  " + Ubicacion.Nombre.
           END.
        END.
        XLin = "Lugar Nacimiento : " + STRING(XTmp2,"X(25)")  + 
               "  Envio Informacion  : " + STRING(XTmp,"X(25)").
        RUN GraInfCli(INPUT XLin, INPUT 2).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RegistrosConsulta wWin 
PROCEDURE RegistrosConsulta :
CREATE TConsulta.
ASSIGN TConsulta.Agencia   = Mov_InsSipla.Agencia
       TConsulta.Nit       = Mov_InsSipla.Nit
       TConsulta.FecRep    = Mov_InsSipla.Fecha_Transaccion
       TConsulta.HorRep    = Mov_InsSipla.Hora_Transaccion
       TConsulta.Id_NUD    = Mov_InsSipla.Id_NUD
       TConsulta.Id_NUM    = Mov_InsSipla.Id_NUM
       TConsulta.PCod      = Mov_InsSipla.CodAutoriza
       TConsulta.IAnt      = Mov_InsSipla.Instancia_Anterior
       TConsulta.IAct      = Mov_InsSipla.Instancia
       TConsulta.Instancia = Mov_InsSipla.Instancia
       TConsulta.IdNor     = Mov_InsSipla.Id_Exonerada
       TConsulta.IdSos     = Mov_InsSipla.Id_Sospechosa
       TConsulta.IdUIAF    = Mov_InsSipla.Id_RepUIAF
       TConsulta.IdROSS    = Mov_InsSipla.Id_RepROSS
       TConsulta.TipReg    = Mov_InsSipla.Tipo_Registro.
IF  W_Fecha - Mov_InsSipla.Fecha_Transaccion GT 0 THEN
    TConsulta.Vigencia = W_Fecha - Mov_InsSipla.Fecha_Transaccion.
FIND Clientes WHERE Clientes.Nit EQ Mov_InsSipla.Nit NO-LOCK NO-ERROR.
IF AVAILABLE Clientes THEN
   ASSIGN TConsulta.Nombre      = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
FOR EACH Mov_Ahorros WHERE
         /*Mov_Ahorros.Agencia         EQ Clientes.Agencia AND*/
         Mov_Ahorros.Nit             EQ TConsulta.Nit    AND
         Mov_Ahorros.Fecha           EQ TConsulta.FecRep AND
         SUBSTRING(STRING(Mov_Ahorros.Cod_Operacion,"999999999"),1,2) EQ "01" NO-LOCK:
    IF Mov_Ahorros.Descrip EQ "Abono Liq.Interés" /* agregado por JJMP - 16 ene-2006 */ THEN
       NEXT.
    FIND Operacion WHERE Operacion.Cod_Operacion EQ Mov_Ahorros.Cod_Operacion NO-LOCK NO-ERROR.
    IF AVAILABLE Operacion THEN DO:
       IF Operacion.Tipo_Operacion EQ 1 THEN
          TConsulta.SCons = TConsulta.SCons + Mov_Ahorros.Val_Efectivo + Mov_Ahorros.Val_Cheque.
       IF Operacion.Tipo_Operacion EQ 2 THEN
          TConsulta.SReti = TConsulta.SReti + Mov_Ahorros.Val_Efectivo + Mov_Ahorros.Val_Cheque.
    END.
END.
FOR EACH Mov_Creditos WHERE
         Mov_Creditos.Nit             EQ TConsulta.Nit AND
         Mov_Creditos.Fecha           EQ TConsulta.FecRep AND 
         SUBSTRING(STRING(Mov_Creditos.Cod_Operacion,"999999999"),1,2) EQ "02" NO-LOCK:
    FIND Operacion WHERE Operacion.Cod_Operacion EQ Mov_Creditos.Cod_Operacion NO-LOCK NO-ERROR.
    IF AVAILABLE Operacion THEN DO:
       IF Operacion.Tipo_Operacion EQ 1 THEN
          TConsulta.SAbon = TConsulta.SAbon + Mov_Creditos.Val_Efectivo + Mov_Creditos.Val_Cheque.
    END.
END.
IF Mov_InsSipla.Tipo_Registro EQ "M" THEN TConsulta.SCons = Mov_InsSipla.Valor_RegManual.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Registros_X_Instancia wWin 
PROCEDURE Registros_X_Instancia :
FOR EACH TConsulta: DELETE TConsulta. END.
  FOR EACH TTransacciones: DELETE TTransacciones. END.
/*  IF INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME FSipla,1,5)) EQ W_Primera THEN DO:*/
     FOR EACH Mov_InsSipla WHERE
              Mov_InsSipla.Instancia   EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME FSipla,1,5)) AND
              Mov_InsSipla.UsuGestiona EQ W_Usuario AND
              Mov_InsSipla.Estado      EQ NO NO-LOCK:
         RUN RegistrosConsulta.
     END.
/*  END.   /*para que las dos ultimas instancias puedan ser manejadas por los dos de control interno*/
  ELSE DO:
     FOR EACH Mov_InsSipla WHERE
              Mov_InsSipla.Instancia   EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME FSipla,1,5)) AND
              Mov_InsSipla.Estado      EQ NO NO-LOCK:
         RUN RegistrosConsulta.
     END.
  END.*/
  CASE ROrganizar:SCREEN-VALUE IN FRAME FConsulta:
    WHEN "1" THEN OPEN QUERY BConsulta FOR EACH TConsulta NO-LOCK BY TConsulta.Agencia  INDEXED-REPOSITION.
    WHEN "2" THEN OPEN QUERY BConsulta FOR EACH TConsulta NO-LOCK BY TConsulta.Nit      INDEXED-REPOSITION.
    WHEN "3" THEN OPEN QUERY BConsulta FOR EACH TConsulta NO-LOCK BY TConsulta.Nombre   INDEXED-REPOSITION.
    WHEN "4" THEN OPEN QUERY BConsulta FOR EACH TConsulta NO-LOCK BY TConsulta.FecRep   INDEXED-REPOSITION.
    WHEN "5" THEN OPEN QUERY BConsulta FOR EACH TConsulta NO-LOCK BY TConsulta.Vigencia DESC INDEXED-REPOSITION.
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Registro_UIAF wWin 
PROCEDURE Registro_UIAF :
/* Arma el registro Detalle para el informe de Transacciones en Efectivo de la UIAF  */

DEFINE INPUT PARAMETER wnit       LIKE ahorros.nit.  
DEFINE INPUT PARAMETER wvalor     LIKE ahorros.sdo_disponible.
DEFINE INPUT PARAMETER wagencia   LIKE ahorros.agencia. 
DEFINE INPUT PARAMETER wtipo      AS CHARACTER FORMAT 'X(2)'. 
DEFINE INPUT PARAMETER wfecha     AS DATE.
DEFINE INPUT PARAMETER NumIdTercero AS CHARACTER FORMAT "X(20)".
DEFINE INPUT PARAMETER nomTercero AS CHARACTER FORMAT "X(40)".

DEFINE VAR wRegistro2  AS CHARACTER FORMAT "X(89)".
DEFINE VAR wtipopdcto  AS CHARACTER FORMAT "X(2)".
DEFINE VAR wtipotra    AS CHARACTER FORMAT "X(1)".
DEFINE VAR wcta        AS CHARACTER FORMAT "X(20)".
DEFINE VAR wtipoide    AS CHARACTER FORMAT "X(2)".
DEFINE VAR znit_new    AS CHARACTER FORMAT "X(15)".
DEFINE VAR blancos     AS CHARACTER FORMAT "X(15)" INITIAL "               ".
DEFINE VAR i           AS INTEGER INITIAL 0.
DEFINE VAR vApellido1 AS CHARACTER FORMAT "X(40)" INITIAL "                                        ".
DEFINE VAR vApellido2 AS CHARACTER FORMAT "X(40)" INITIAL "                                        ".
DEFINE VAR NombreTitular AS CHARACTER FORMAT "X(40)" INITIAL "                                        ".
DEFINE VAR OtroNombreTitular AS CHARACTER FORMAT "X(40)" INITIAL "                                        ".
DEFINE VAR razonSocial AS CHARACTER FORMAT "X(60)" INITIAL "                                                            ".
DEFINE VAR tipoIdTercero AS CHARACTER FORMAT "X(2)".
DEFINE VAR activEcon AS CHARACTER.
DEFINE VAR IngTotales AS DECIMAL.

/* Definicion del registro tipo 1 - Encabezado */

/* Definicion del registro tipo 2 - Detalle */
FIND FIRST agencias WHERE agencias.agencia EQ wagencia NO-LOCK NO-ERROR.
CASE wtipo:
    /* Retiro Ahorro  */
    WHEN "RA" THEN 
        ASSIGN wtipopdcto = "02"
               wtipotra = "2".
    /* Consignacion Ahorro */
    WHEN "CA" THEN 
        ASSIGN wtipopdcto = "02"
               wtipotra = "1".
    /* Abono Credito */
    WHEN "AC" THEN 
        ASSIGN wtipopdcto = "03"
               wtipotra = "1".
END CASE.

/* Se carga el número de cuenta de ahorros */
IF wtipo = "RA" OR wtipo = "CA" THEN DO:
    FIND FIRST ahorros WHERE ahorros.nit EQ wnit AND
                             ahorros.estado = 1 NO-LOCK NO-ERROR.
    IF AVAILABLE(ahorros) THEN
        wcta = STRING(cue_ahorros,"X(20)").
    ELSE DO:
        FIND FIRST ahorros WHERE ahorros.nit EQ wnit
                             AND MONTH(ahorros.fec_cancelacion) = MONTH(wFecha)
                             AND YEAR(ahorros.fec_cancelacion) = YEAR(wFecha) NO-LOCK NO-ERROR.
        IF AVAILABLE ahorros THEN
            wcta = STRING(cue_ahorros,"X(20)").
    END.
END.
ELSE DO:
    /* Se carga el número de cuenta para el crédito */
    FIND FIRST creditos WHERE creditos.nit = wnit NO-LOCK NO-ERROR.
    IF AVAILABLE(creditos) THEN
        wcta = STRING(num_credito,"99999999999999999999").
END.

FIND FIRST clientes WHERE clientes.nit     EQ wnit     NO-LOCK NO-ERROR.
FIND FIRST agencias WHERE agencias.agencia EQ wagencia NO-LOCK NO-ERROR.

/* Se carga el tipo de identificación */
CASE Tipo_Identificacion:
    WHEN "C.C" THEN wtipoide = "13".
    WHEN "C.E" THEN wtipoide = "21".
    WHEN "NIT" THEN wtipoide = "31".
    WHEN "T.I" THEN wtipoide = "12".
    WHEN "NUI" THEN wtipoide = "00".
END CASE.

/* Se elimina el caracter "-" del campo Nit */
IF clientes.tipo_ident EQ "NIT" THEN DO:
    REPEAT i = 1 TO 15:
        IF SUBSTRING(wnit,i,1) NE "-" THEN DO:
            znit_new = TRIM(znit_new) + SUBSTRING(wnit,i,1).
        END.
        ELSE
            i = 20.
    END.
END.
ELSE                                                                                                                                   
   ASSIGN znit_new = TRIM(wnit). 

/*znit_new = SUBSTRING(blancos, 1, 15 - LENGTH(TRIM(znit_new) )) + TRIM(znit_new).*/

/* Se cargan los apellidos, nombre y/o razón social del titular */
IF tipo_identificacion <> "NIT" THEN DO:
    vApellido1 = clientes.Apellido1.
    vApellido2 = clientes.Apellido2.
    NombreTitular = SUBSTRING(clientes.nombre,1,INDEX(clientes.nombre," ")).
    OtroNombreTitular = SUBSTRING(clientes.nombre,INDEX(clientes.nombre," ") + 1).
END.
ELSE DO:
    ASSIGN razonSocial = clientes.nombre + " " + clientes.apellido1 + clientes.apellido2
           vApellido1 = ""
           vApellido2 = ""
           NombreTitular = ""
           OtroNombreTitular = "".
END.

/* Se cargan los datos del tercero que realiza la transacción individual */
/*IF wValor >= Entidad.MaxOp_Efectivo_Mes THEN DO:*/
IF NumIdTercero <> "" THEN
    tipoIdTercero = "13". /* Siempre es cédula */

/* Se carga la actividad económica del Titular de la cuenta */
FIND FIRST CIIU WHERE CIIU.codigo_CIIU = clientes.codigo_CIIU NO-LOCK NO-ERROR.
IF AVAILABLE CIIU THEN
    activEcon = CIIU.descripcion.

/* Se calcula el total de los ingresos mensuales para el titular de la cuenta */
IngTotales = clientes.salario + clientes.ing_arriendos + clientes.ing_honorarios + clientes.ing_financieros + clientes.ing_otros.

/* Se arma el registro */
w_consecutivo = w_consecutivo + 1.
wregistro2 = STRING(w_consecutivo,"9999999999") +   /* Consecutivo */
             STRING(YEAR(wfecha),"9999") + "-"  +
             STRING(MONTH(wfecha),"99") + "-" + STRING(DAY(wfecha),"99") + /* Fecha */
             STRING(wValor,"99999999999999999999") + /* Valor */
             "1" + /* Tipo de Moneda */
             STRING(agencias.agencia,"999999999999999") + /* Código de la oficina */
             wtipopdcto + /* Tipo de producto */
             wtipotra + /* Tipo de transacción */
             wcta + /* Número de cuenta */
             STRING(wtipoide,"X(2)") + /* Tipo de identificación */
             STRING(znit_new,"X(20)") + /* Nümero de identificación */
             STRING(vApellido1,"X(40)") + /* Primer apellido */
             STRING(vApellido2,"X(40)") + /* Segundo Apellido */
             STRING(NombreTitular,"X(40)") + /* Primer nombre del titular de la cuenta */
             STRING(OtroNombreTitular,"X(40)") + /* Otros nombres del titular de la cuenta */
             STRING(razonSocial,"X(60)") + /* Razón social de la empresa titular de la cuenta */
             SUBSTRING(agencias.ciudad,1,5) + /* Código del Departamento */
             tipoIdTercero +    /* Tipo de identificacion del tercero que realiza la transacción */
             STRING(NumIdTercero,"X(20)") + /* Número de identificación del tercero que realiza la transacción */
             "                                        " +   /* Primer apellido de quien realiza la transacción */
             "                                        " +   /* Segundo apellido de quien realiza la transacción */
             "                                        " +   /* Primer nombre de quien realiza la transacción */
             STRING(nomTercero,"X(40)") +   /* Nombre completo de quien realiza la transacción */
             STRING(activEcon,"X(20)") +   /* Actividad económica del titular */
             STRING(ingTotales,">>>>>>>>>>>>>>>>9.99"). /* Total de ingresos mensual del titular de la cuenta */

w_vlrtotal = w_vlrtotal + wvalor.

CREATE RegistrosUIAF.
ASSIGN RegistrosUIAF.consecutivo = w_consecutivo
       RegistrosUIAF.registro    = wregistro2.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Transacciones wWin 
PROCEDURE Transacciones :
ASSIGN W_ValConEfe = 0 W_ValRetEfe = 0 W_ValConChe = 0 W_ValRetChe = 0 W_AboCreEfe = 0 W_AboCreChe = 0.
  FOR EACH TTransacciones: DELETE TTransacciones. END.
  FOR EACH TTransaccionesC: DELETE TTransaccionesC. END.
  FOR EACH Mov_Ahorros WHERE
           Mov_Ahorros.Nit   EQ TConsulta.Nit AND
           Mov_Ahorros.Fecha GE TConsulta.FecRep - DAY(TConsulta.FecRep) + 1 AND
           Mov_Ahorros.Fecha LE TConsulta.FecRep NO-LOCK:
      IF Mov_Ahorros.Descrip EQ "Abono Liq.Interés" /* agregado por JJMP - 12 ene-2006 */ THEN
          NEXT.
      CREATE TTransacciones.
      ASSIGN TTransacciones.AgeTran = Mov_Ahorros.Age_Fuente
             TTransacciones.CodTran = Mov_Ahorros.Cod_Ahorro
             TTransacciones.CueTran = Mov_Ahorros.Cue_Ahorros
             TTransacciones.FecTran = Mov_Ahorros.Fecha.
      FIND Operacion WHERE Operacion.Cod_Operacion EQ Mov_Ahorros.Cod_Operacion NO-LOCK NO-ERROR.
      IF AVAILABLE Operacion THEN DO:
         IF Operacion.Tipo_Operacion EQ 1 THEN DO:
             ASSIGN TTransacciones.ConTran = Mov_Ahorros.Val_Efectivo + Mov_Ahorros.Val_Cheque.
             IF Operacion.Ctrl_EfeChe EQ 1 THEN
                ASSIGN W_ValConEfe = W_ValConEfe + Mov_Ahorros.Val_Efectivo
                       TTransacciones.EfeChe = "E".
             IF Operacion.Ctrl_EfeChe EQ 2 THEN DO:
                 IF Mov_Ahorros.Val_Cheque GT 0 THEN
                    ASSIGN W_ValConChe = W_ValConChe + Mov_Ahorros.Val_Cheque.
                 ELSE
                    ASSIGN W_ValConChe = W_ValConChe + Mov_Ahorros.Val_Efectivo.                        
                 TTransacciones.EfeChe = "C".
             END.
         END.
         IF Operacion.Tipo_Operacion EQ 2 THEN DO:
             ASSIGN TTransacciones.RetTran = Mov_Ahorros.Val_Efectivo + Mov_Ahorros.Val_Cheque
                    W_ValRetEfe = W_ValRetEfe + Mov_Ahorros.Val_Efectivo
                    W_ValRetChe = W_ValRetChe + Mov_Ahorros.Val_Cheque.
             IF Operacion.Ctrl_EfeChe EQ 1 THEN ASSIGN TTransacciones.EfeChe = "E".
             IF Operacion.Ctrl_EfeChe EQ 2 THEN ASSIGN TTransacciones.EfeChe = "C".
         END.
      END.
      IF W_ValConEfe + W_ValConChe GT W_MaxDia THEN TTransacciones.BddTran = YES.
      IF W_ValConEfe + W_ValConChe GT W_MaxMes THEN TTransacciones.BdmTran = YES.
      IF W_ValRetEfe + W_ValRetChe GT W_MaxDia THEN TTransacciones.BddTran = YES.
      IF W_ValRetEfe + W_ValRetChe GT W_MaxMes THEN TTransacciones.BdmTran = YES.
      DISPLAY W_ValConEfe W_ValConChe W_ValRetEfe W_ValRetChe WITH FRAME FTransacciones.
  END.
  ASSIGN ConsEfectivo = W_ValConEfe
         ConsCheque   = W_ValConChe
         RetEfectivo  = W_ValRetEfe
         RetCheque    = W_ValRetChe.
  DISPLAY ConsEfectivo ConsCheque RetEfectivo RetCheque WITH FRAME FDetCon.
  OPEN QUERY BTransacciones FOR EACH TTransacciones NO-LOCK BY TTransacciones.FecTran INDEXED-REPOSITION.
  FOR EACH Mov_Creditos WHERE
           Mov_Creditos.Nit   EQ TConsulta.Nit AND
           Mov_Creditos.Fecha GE TConsulta.FecRep - DAY(TConsulta.FecRep) + 1 AND
           Mov_Creditos.Fecha LE TConsulta.FecRep  NO-LOCK:
      FIND Operacion WHERE Operacion.Cod_Operacion EQ Mov_Creditos.Cod_Operacion AND
           Operacion.Tipo_Operacion EQ 1 NO-LOCK NO-ERROR.
      IF AVAILABLE Operacion THEN DO:
          CREATE TTransaccionesC.
          ASSIGN TTransaccionesC.AgeCre = Mov_Creditos.Agencia
                 TTransaccionesC.CodCre = Mov_Creditos.Cod_Credito
                 TTransaccionesC.PagCre = Mov_Creditos.Pagare
                 TTransaccionesC.FecCre = Mov_Creditos.Fecha
                 TTransaccionesC.DepCre = Mov_Creditos.Val_Efectivo + Mov_Creditos.Val_Cheque.
          IF Operacion.Ctrl_EfeChe EQ 1 THEN
             ASSIGN W_AboCreEfe = W_AboCreEfe + Mov_Creditos.Val_Efectivo
                    TTransaccionesC.EfeChe = "E".
          IF Operacion.Ctrl_EfeChe EQ 2 THEN
             ASSIGN W_AboCreChe = W_AboCreChe + Mov_Creditos.Val_Cheque
                    TTransaccionesC.EfeChe = "C".
      END.
  END.
  ASSIGN AboCreditosEfe = W_AboCreEfe
         AboCreditosChe = W_AboCreChe.
  DISPLAY W_AboCreEfe W_AboCreChe WITH FRAME FTransacciones.
  OPEN QUERY BCreditos FOR EACH TTransaccionesC NO-LOCK BY TTransaccionesC.FecCre INDEXED-REPOSITION.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TransaccionesReporte wWin 
PROCEDURE TransaccionesReporte :
DEFINE INPUT  PARAMETER pNit LIKE Clientes.Nit.
DEFINE INPUT  PARAMETER FIni AS DATE.
DEFINE INPUT  PARAMETER FFin AS DATE.
DEFINE OUTPUT PARAMETER TCA LIKE Ahorros.Sdo_Disponible.
DEFINE OUTPUT PARAMETER TRA LIKE Ahorros.Sdo_Disponible.
DEFINE OUTPUT PARAMETER TAC LIKE Ahorros.Sdo_Disponible.
DEFINE OUTPUT PARAMETER cedTCA LIKE mov_ahorros.cedula_transac.
DEFINE OUTPUT PARAMETER cedTRA LIKE mov_ahorros.cedula_transac.
DEFINE OUTPUT PARAMETER cedTAC LIKE mov_ahorros.cedula_transac.
DEFINE OUTPUT PARAMETER nomTCA LIKE mov_ahorros.cedula_transac.
DEFINE OUTPUT PARAMETER nomTRA LIKE mov_ahorros.cedula_transac.
DEFINE OUTPUT PARAMETER nomTAC LIKE mov_ahorros.cedula_transac.

FOR EACH Mov_Ahorros WHERE Mov_Ahorros.Nit EQ pNit
                       AND Mov_Ahorros.Fecha GE FIni
                       AND Mov_Ahorros.Fecha LE FFin NO-LOCK BREAK BY fecha:
    FIND Operacion WHERE Operacion.Cod_Operacion EQ Mov_Ahorros.Cod_Operacion NO-LOCK NO-ERROR.
    IF AVAILABLE Operacion THEN DO:
        IF Operacion.Tipo_Operacion EQ 1 THEN DO:
            IF Operacion.Ctrl_EfeChe EQ 1 THEN DO:
                ASSIGN TCA = TCA + Mov_Ahorros.Val_Efectivo.
                IF TCA > Entidad.MaxOp_Efectivo_Dia THEN DO:
                    cedTCA = mov_ahorros.cedula_transac.
                    nomTCA = mov_ahorros.nomApell_trans.
                END.
            END.
        END.
        IF Operacion.Tipo_Operacion EQ 2 THEN DO:
            ASSIGN TRA = TRA + Mov_Ahorros.Val_Efectivo.
            IF TRA > Entidad.MaxOp_Efectivo_Dia THEN DO:
                cedTRA = mov_ahorros.cedula_transac.
                nomTRA = mov_ahorros.nomApell_trans.
            END.
        END.
    END.
END.

FOR EACH Mov_Creditos WHERE Mov_Creditos.Nit EQ pNit
                        AND Mov_Creditos.Fecha GE FIni
                        AND Mov_Creditos.Fecha LE FFin NO-LOCK:
    FIND Operacion WHERE Operacion.Cod_Operacion EQ Mov_Creditos.Cod_Operacion AND
                         Operacion.Tipo_Operacion EQ 1 NO-LOCK NO-ERROR.
    IF AVAILABLE Operacion THEN DO:
        IF Operacion.Ctrl_EfeChe EQ 1 THEN DO:
            ASSIGN TAC = TAC + Mov_Creditos.Val_Efectivo.
        END.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UsuariosTraspaso wWin 
PROCEDURE UsuariosTraspaso :
FOR EACH Cfg_Instancias WHERE
     Cfg_Instancias.Agencia        EQ W_Agencia           AND
     Cfg_Instancias.Tipo_Instancia EQ 6                   AND
     Cfg_Instancias.Instancia      EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME FSipla,1,6)) AND
     Cfg_Instancias.Estado         EQ 2 NO-LOCK:
   IF AVAILABLE Cfg_Instancias THEN DO:
      CREATE Tut.
      ASSIGN Tut.Usuario = Cfg_Instancias.Usuario.
      FIND Usuarios WHERE Usuarios.Usuario EQ Cfg_Instancias.Usuario NO-LOCK NO-ERROR.
      IF AVAILABLE Usuarios THEN Tut.Nombre = Usuarios.Nombre.
   END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

