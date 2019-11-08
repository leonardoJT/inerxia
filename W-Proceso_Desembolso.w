&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
{INCLUIDO\VARIABLE.I "SHARED"}

DEFINE VAR wvlres AS DECIMAL EXTENT 9.
DEFINE VAR wnombres AS CHARACTER FORMAT "X(20)" EXTENT 9.
DEFINE VAR p_linea AS INTEGER.
DEFINE VAR TasaOk AS DECIMAL.
DEFINE VAR W_Error AS LOGICAL.
DEFINE VAR W_Autorizo AS CHARACTER.
DEFINE VAR choice AS LOGICAL.
DEFINE VAR W_NvaAdm AS LOGICAL.
DEFINE VAR W_NvaHV AS LOGICAL.
DEFINE VAR Puntero AS ROWID.
DEFINE VAR PuntGar AS ROWID.
DEFINE VAR Longitud AS DECIMAL.
DEFINE VAR W_Ultima AS INTEGER.
DEFINE VAR W_Primera AS INTEGER.
DEFINE VAR Id_Agregar AS CHARACTER FORMAT "X(2)".
DEFINE VAR W_Tippdt AS INTEGER.
DEFINE VAR W_Antiguedad AS INTEGER NO-UNDO.
DEFINE VAR W_Tasa AS DECIMAL NO-UNDO.
DEFINE VAR W_TipoInforme AS CHARACTER FORMAT "X(10)".
DEFINE VAR W_VigIns AS INTEGER.
DEFINE VAR W_NumCbt AS INTEGER.
DEFINE VAR W_RowidGar AS ROWID.
DEFINE VAR W_NoContab AS LOGICAL.
DEFINE VAR W_IdGar AS CHARACTER.
DEFINE VAR W_DiasAnt AS INTEGER FORMAT "99".
DEFINE VAR W_Anular AS LOGICAL.
DEFINE VAR WDed AS DECIMAL.
DEFINE VAR W_DesAho AS CHARACTER.
DEFINE VAR W_CtaSyA AS CHARACTER.
DEFINE VAR W_Iva AS DECIMAL.
DEFINE VAR W_TotExt AS DECIMAL.
DEFINE VAR WK_Edad AS INTEGER FORMAT "99999".
DEFINE VAR W_VrImpto AS DECIMAL.
DEFINE VAR Val_GMFTotDed AS DECIMAL.
DEFINE VAR Val_GMFEfec AS DECIMAL.
DEFINE VAR Val_Deduc AS DECIMAL.

DEFINE TEMP-TABLE TmpI
    FIELD nit AS CHARACTER
    FIELD ILinea AS INTEGER FORMAT "99"
    FIELD ITexto AS CHARACTER FORMAT "X(125)". 

DEFINE TEMP-TABLE CCfg_RegCredito LIKE Cfg_RegCredito.

DEFINE VAR W_CtaCorCre AS CHARACTER.
DEFINE VAR W_CtaCorAho AS CHARACTER.
DEFINE VAR W_CtaBanco AS CHARACTER.
DEFINE VAR W_Des AS LOGICAL.
DEFINE VAR W_MontoCre AS DECIMAL.
DEFINE VAR W_Cbte AS INTEGER.
DEFINE VAR W_CancCap AS DECIMAL.
DEFINE VAR W_CtaSyA_aporte AS CHARACTER.
DEFINE VAR w_ageaportes AS INTEGER.
DEFINE VAR P_Nit AS CHARACTER.
DEFINE VAR p_Nombre AS CHARACTER.
DEFINE VAR P_Apellido AS CHARACTER.
DEFINE VAR P_AgeCli AS INTEGER.
DEFINE VAR W_Nuevo AS LOGICAL.

DEFINE TEMP-TABLE TPartidas
    FIELD Tage AS INTEGER
    FIELD TCta AS CHARACTER
    FIELD TCed AS CHARACTER
    FIELD TDoc AS CHARACTER
    FIELD TDsc AS CHARACTER
    FIELD TTip AS INTEGER FORMAT 9 /*1-Ahorro, 2-Credito*/
    FIELD TOpe AS INTEGER
    FIELD TDeb AS DECIMAL
    FIELD TCre AS DECIMAL.

DEFINE TEMP-TABLE TGarCon
    FIELD TIdGar AS CHARACTER.

DEFINE TEMP-TABLE TProIns
    FIELD TP_Agencia AS INTEGER
    FIELD TP_Orden AS INTEGER
    FIELD TP_Instancia AS INTEGER
    FIELD TP_NomInstan AS CHARACTER FORMAT "X(30)"
    FIELD TP_Usuario AS CHARACTER
    FIELD TP_NomUsuar AS CHARACTER FORMAT "X(30)"
    FIELD TP_Cantidad AS INTEGER FORMAT "999".

DEFINE TEMP-TABLE TCode
    FIELD TC_AgeCode AS INTEGER
    FIELD TC_NitCode AS CHARACTER
    FIELD TC_NitDeud AS CHARACTER
    FIELD TC_NumSoli AS INTEGER
    FIELD TC_NomCode  AS CHARACTER FORMAT "X(60)"
    FIELD TC_Aprob AS LOGICAL
    FIELD TC_TelCdRs AS CHARACTER
    FIELD TC_TelCdCo AS CHARACTER
    FIELD TC_EmlCode AS CHARACTER
    FIELD TC_EstRela AS INTEGER INITIAL 1
    FIELD TC_FecCrea AS DATE INITIAL TODAY
    FIELD TC_FecReti AS DATE INITIAL TODAY.

DEFINE TEMP-TABLE TUXI
    FIELD Agencia AS INTEGER
    FIELD Usuario AS CHARACTER
    FIELD Nombre AS CHARACTER
    FIELD Cantidad AS INTEGER FORMAT "999"
    FIELD proceso AS LOGICAL.

DEFINE TEMP-TABLE TCerradas
    FIELD Instancia AS INTEGER
    FIELD INom_Instancia AS CHARACTER FORMAT "X(20)"
    FIELD Fec_Ingreso AS DATE
    FIELD Fec_Retiro AS DATE
    FIELD Hora_Ingreso AS INTEGER
    FIELD Hora_Retiro AS INTEGER
    FIELD Estado AS LOGICAL
    FIELD Num_Solicitud AS INTEGER
    FIELD Usuario AS CHARACTER
    FIELD INom_Usuario AS CHARACTER FORMAT "X(30)"
    FIELD Descripcion AS CHARACTER.

DEFINE VARIABLE A_Nit AS CHARACTER.
DEFINE VARIABLE A_Age AS INTEGER.
DEFINE VARIABLE A_Pro AS INTEGER.
DEFINE VARIABLE A_NitW AS CHARACTER.
DEFINE VARIABLE A_Cue AS CHARACTER.
DEFINE VARIABLE i AS INTEGER.

  /* oakley */

  DEFINE VARIABLE W_Ok AS LOGICAL.
  DEFINE VARIABLE W_TipoProducto LIKE Pro_Creditos.Tip_Credito.
  DEFINE VARIABLE Dias AS DECIMAL.
  DEFINE TEMP-TABLE TIns LIKE Cfg_Instancias.
  
  DEFINE TEMP-TABLE TScoring
      FIELD CodS LIKE Scoring.Codigo
      FIELD TabS LIKE Pro_Scoring.Tabla
      FIELD VarS LIKE Scoring.VARIABLE
      FIELD VVaS LIKE Scoring.Valor_Variable
      FIELD PunS LIKE Scoring.Puntaje
      FIELD FecS LIKE Scoring.Fec_Scoring.
      
  DEFINE TEMP-TABLE TDeducc LIKE Deducible.
  
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

   DEFI   VAR Tot_Deduc    LIKE Solicitud.Monto INIT 0.
   DEFINE VAR W_Consulta   AS   LOGICAL.
   DEFINE VAR W_TotCuoEsp  LIKE Pro_Especiales.Ran_FinCuota.
   DEFINE VAR W_NomTer     LIKE  Terceros.Nombre.
   DEFINE VAR W_Tercero    LIKE  Tercero.Nit.
   DEFINE VAR W_NvaAse     AS LOGICAL INITIAL FALSE.
   DEFINE VAR W_Rpta1      AS LOGICAL INITIAL FALSE.
   DEFINE VAR W_NomLin     AS    CHARACTER FORMAT "X(15)".
   DEFINE VAR W_P          AS    INTEGER.
   DEFINE VAR W_ConDed     AS    INTEGER INITIAL 1.
   DEFINE VAR W_Ind        AS    INTEGER INITIAL 0.  
   DEFINE VAR W_WidSel     AS    WIDGET-HANDLE.
   DEFINE VAR W_Cerrado    AS    INTEGER INITIAL 0.
   DEFINE VAR W_VlrAux     AS    DECIMAL FORMAT ">>>,>>>,>>>,>>9" INITIAL 0.
   DEFINE VAR W_PerDed     AS    INTEGER INITIAL 1.
   DEFINE VAR W_Codfor     LIKE  Formatos.Cod_Formato INITIAL 0.
   DEFINE VAR W_Interplazo AS    DECIMAL FORMAT "->>>>>>>>>>>9" INITIAL 0.   
   DEFINE VAR W_Destino    LIKE  Solicitud.Destino.
   DEFINE VAR W_TasEfe     AS    DECIMAL FORMAT ">>9.999999".
   DEFINE VAR W_TasaCont   AS    DECIMAL FORMAT ">>9.999999".
   DEFINE VAR W_Liquidar   AS    LOGICAL INITIAL FALSE.
   DEFINE VAR W_Existe     AS    LOGICAL INITIAL FALSE.
   DEFINE VAR W_NroPer     AS    INTEGER INITIAL 1.
   DEFINE VAR W_MonMul     AS    DECIMAL FORMAT ">>>,>>>,>>>,>>9" INITIAL 0.
   DEFINE VAR W_Razon      AS    DECIMAL FORMAT "->>>>>>>>>>>9.999" INITIAL 0.
   DEFINE VAR P_Band       AS    LOGICAL INITIAL FALSE.
   DEFINE VAR W_TasNom     AS    DECIMAL FORMAT "->>>>>>>>>>>9.999" INITIAL 0.
   DEFINE VAR W_ErrIndica  AS    LOGICAL.
   DEFINE VAR W_TotPorDed  AS    DECIMAL FORMAT ">>9.9999" INITIAL 0.
   DEFINE VAR Suma         AS    DECIMAL FORMAT ">>>,>>>,>>>,>>9" INITIAL 0.
   DEFINE VAR W_Suma       AS    DECIMAL FORMAT ">>>,>>>,>>>,>>9" INITIAL 0.
   DEFINE VAR W_PlaPer     AS    INTEGER INITIAL 0.   
   DEFINE VAR W_PlazoDias  AS    INTEGER INITIAL 0.   
   DEFINE VAR W_DesAse     LIKE  Varios.Codigo.
   DEFINE VAR W_MonMin     LIKE  Pro_Creditos.Val_Montominimo.
   DEFINE VAR W_MonMax     LIKE  Pro_Creditos.Val_Montomaximo.
   DEFINE VAR W_PlaMin     LIKE  Pro_Creditos.Pla_Minimo.
   DEFINE VAR W_PlaMax     LIKE  Pro_Creditos.Pla_Maximo.    
   DEFINE VAR W_Sistema    LIKE  Solicitud.Sistema INITIAL 0.      
   DEFINE VAR W_Gracia     LIKE  Solicitud.Per_Gracia.
   DEFINE VAR W_TipInt     LIKE  Solicitud.For_Interes INITIAL 1.
   DEFINE VAR W_Incremento LIKE  Solicitud.Incremento.
   DEFINE VAR W_ClaTas     LIKE  Pro_Creditos.Tip-Tasa.
   DEFINE VAR W_PNegocia   LIKE  Ran_Interes.Pun_Negociables.
   DEFINE VAR W_TasDif     LIKE  Pro_Creditos.Id_TasDiferencial.
   DEFINE VAR W_TotExtras  LIKE  Pro_Creditos.Val_Montomaximo.
   DEFINE VAR W_DiaPer     AS    INTEGER   FORMAT "9999".
   DEFINE VAR W_PerLiqui   AS    INTEGER   FORMAT "99".
   DEFINE VAR W_Per        AS    DECIMAL   FORMAT "999.9999999".
   DEFINE VAR W_LinAho     LIKE Solicitud.Lin_Ahorro.
   DEFINE VAR W_CueAho     LIKE Solicitud.Cue_desembolso.
   DEFINE VAR W_CreMixto   LIKE Solicitud.Monto.
   DEFINE VAR WFactorCod   AS INTEGER INITIAL 0.

DEFINE TEMP-TABLE tmp-tarjetadb LIKE tarjetadebito.

/*  
    Creado: Giocam Nov 26/2007
    para texto de impresion formato condiciones
*/

DEFINE NEW SHARED TEMP-TABLE TTimpresion
    FIELD   reg01       AS CHARACTER
    FIELD   reg02       AS CHARACTER
    FIELD   reg03       AS CHARACTER
    FIELD   reg04       AS CHARACTER
    FIELD   reg05       AS CHARACTER
    FIELD   reg06       AS CHARACTER
    FIELD   reg07       AS CHARACTER
    FIELD   reg08       AS CHARACTER
    FIELD   reg09       AS CHARACTER
    FIELD   reg10       AS CHARACTER
    FIELD   reg11       AS CHARACTER
    FIELD   reg12       AS CHARACTER
    FIELD   reg13       AS CHARACTER
    FIELD   condiciones AS CHARACTER.

DEFINE VARIABLE vcCondiciones AS CHARACTER   NO-UNDO.
DEF BUFFER bSlctud FOR solicitud.
DEFINE BUFFER TEmpresas FOR Empresas.
DEF BUFFER tClientes FOR clientes.

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

/* para los formatos de Excel */
  DEF VAR W_Meses AS CHA NO-UNDO EXTENT 12 INIT ["Enero","Febrero","Marzo","Abril",
  "Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre"].
   
   DEFINE  VAR ptipo AS INTEGER INITIAL 1.
   DEFINE VAR  ndias  AS INTEGER INITIAL 0.
   DEFINE VAR fefin AS DATE FORMAT "99/99/9999".
   DEFINE VAR  tcuota AS INTEGER INITIAL 0.
   DEFINE VAR finicio AS DATE FORMAT "99/99/9999".
   DEFINE VAR primerc AS DATE  FORMAT "99/99/9999".
   DEF VAR W_FecTra    AS DATE FORMAT "99/99/9999".
   DEFINE VAR mesan AS INT INITIAL 0.
   DEFINE VAR  indi AS INT.
   DEFINE  VAR periodo AS INT INITIAL 0.
   DEFINE VAR cta AS INT INITIAL 0.

DEFINE VAR vTime AS INTEGER.
DEFINE VAR vPagoCreditosTotal AS DECIMAL. /* Almacena el total cancelado de créditos con el desembolso */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Creditos
&Scoped-define BROWSE-NAME Bancos

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Cuentas Operacion Garantias TCerradas TCode ~
Hoja_Vida Consulta TDeducc TScoring Tuxi TPartidas Creditos

/* Definitions for BROWSE Bancos                                        */
&Scoped-define FIELDS-IN-QUERY-Bancos Operacion.Cod_Compensa Cuentas.Cuenta ~
Cuentas.Nombre 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Bancos 
&Scoped-define QUERY-STRING-Bancos FOR EACH Cuentas ~
      WHERE Cuentas.Cod_FlujoEfec = "D" ~
 AND Cuentas.Car_Efectivo = 3 ~
 AND Cuentas.Estado = 1 NO-LOCK, ~
      EACH Operacion OF Cuentas ~
      WHERE Operacion.Clase_Operacion = 1 ~
 AND Operacion.Ctrl_EfeChe = 2 ~
 AND Operacion.Tipo_Operacion = 2 ~
 and Operacion.Cod_Compensa <> ? NO-LOCK ~
    BY Operacion.Cod_Compensa
&Scoped-define OPEN-QUERY-Bancos OPEN QUERY Bancos FOR EACH Cuentas ~
      WHERE Cuentas.Cod_FlujoEfec = "D" ~
 AND Cuentas.Car_Efectivo = 3 ~
 AND Cuentas.Estado = 1 NO-LOCK, ~
      EACH Operacion OF Cuentas ~
      WHERE Operacion.Clase_Operacion = 1 ~
 AND Operacion.Ctrl_EfeChe = 2 ~
 AND Operacion.Tipo_Operacion = 2 ~
 and Operacion.Cod_Compensa <> ? NO-LOCK ~
    BY Operacion.Cod_Compensa.
&Scoped-define TABLES-IN-QUERY-Bancos Cuentas Operacion
&Scoped-define FIRST-TABLE-IN-QUERY-Bancos Cuentas
&Scoped-define SECOND-TABLE-IN-QUERY-Bancos Operacion


/* Definitions for BROWSE BR_Admisible                                  */
&Scoped-define FIELDS-IN-QUERY-BR_Admisible Garantias.Tipo_Garantia Garantias.Identificacion_Bien Garantias.Nom_Bien   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BR_Admisible   
&Scoped-define SELF-NAME BR_Admisible
&Scoped-define OPEN-QUERY-BR_Admisible /*OPEN QUERY {&SELF-NAME} FOR EACH Garantias INDEXED-REPOSITION.*/.
&Scoped-define TABLES-IN-QUERY-BR_Admisible Garantias
&Scoped-define FIRST-TABLE-IN-QUERY-BR_Admisible Garantias


/* Definitions for BROWSE Br_Cerradas                                   */
&Scoped-define FIELDS-IN-QUERY-Br_Cerradas TCerradas.Instancia TCerradas.INom_Instancia TCerradas.Usuario TCerradas.INom_Usuario TCerradas.Fec_Retiro TCerradas.Descripcion   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Cerradas   
&Scoped-define SELF-NAME Br_Cerradas
&Scoped-define QUERY-STRING-Br_Cerradas FOR EACH TCerradas NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br_Cerradas OPEN QUERY {&SELF-NAME} FOR EACH TCerradas NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_Cerradas TCerradas
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Cerradas TCerradas


/* Definitions for BROWSE Br_Codeudores                                 */
&Scoped-define FIELDS-IN-QUERY-Br_Codeudores TC_AgeCode TC_NitCode TC_NomCode TC_Aprob TC_TelCdRs TC_TelCdCo TC_emlCode   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Codeudores   
&Scoped-define SELF-NAME Br_Codeudores
&Scoped-define QUERY-STRING-Br_Codeudores FOR EACH TCode NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br_Codeudores OPEN QUERY {&SELF-NAME} FOR EACH TCode NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_Codeudores TCode
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Codeudores TCode


/* Definitions for BROWSE Br_ConHV                                      */
&Scoped-define FIELDS-IN-QUERY-Br_ConHV Hoja_Vida.Fec_Grabacion Hoja_Vida.Observacion   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_ConHV   
&Scoped-define SELF-NAME Br_ConHV
&Scoped-define QUERY-STRING-Br_ConHV FOR EACH Hoja_Vida WHERE        Hoja_Vida.Tipo      EQ 9 AND Hoja_Vida.Codigo EQ 1 AND        Hoja_Vida.Instancia EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos, ~
      1, ~
      5)) AND        Hoja_Vida.DoctoRef  EQ INTEGER(Creditos.Num_Credito:SCREEN-VALUE IN FRAME F_Solicitud) AND        Hoja_Vida.Nit       EQ Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud AND        Hoja_Vida.Asunto_Cumplido EQ NO AND        Hoja_Vida.Usuario   EQ W_Usuario   INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br_ConHV OPEN QUERY {&SELF-NAME} FOR EACH Hoja_Vida WHERE        Hoja_Vida.Tipo      EQ 9 AND Hoja_Vida.Codigo EQ 1 AND        Hoja_Vida.Instancia EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos, ~
      1, ~
      5)) AND        Hoja_Vida.DoctoRef  EQ INTEGER(Creditos.Num_Credito:SCREEN-VALUE IN FRAME F_Solicitud) AND        Hoja_Vida.Nit       EQ Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud AND        Hoja_Vida.Asunto_Cumplido EQ NO AND        Hoja_Vida.Usuario   EQ W_Usuario   INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_ConHV Hoja_Vida
&Scoped-define FIRST-TABLE-IN-QUERY-Br_ConHV Hoja_Vida


/* Definitions for BROWSE Br_Consulta                                   */
&Scoped-define FIELDS-IN-QUERY-Br_Consulta Consulta.Num_Credito Consulta.Num_Solicitud Consulta.AgeCredito Consulta.Nit Consulta.Nombre Consulta.Fec_Ingreso Consulta.Hor_Ingreso Consulta.Monto Consulta.Vigencia   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Consulta   
&Scoped-define SELF-NAME Br_Consulta
&Scoped-define QUERY-STRING-Br_Consulta FOR EACH Consulta NO-LOCK BY Consulta.Num_Credito INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br_Consulta OPEN QUERY {&SELF-NAME} FOR EACH Consulta NO-LOCK BY Consulta.Num_Credito INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_Consulta Consulta
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Consulta Consulta


/* Definitions for BROWSE Br_Deducibles                                 */
&Scoped-define FIELDS-IN-QUERY-Br_Deducibles TDeducc.Nom_Deducible TDeducc.Valor TDeducc.Valor_Impuesto   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Deducibles   
&Scoped-define SELF-NAME Br_Deducibles
&Scoped-define QUERY-STRING-Br_Deducibles FOR EACH TDeducc NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br_Deducibles OPEN QUERY {&SELF-NAME} FOR EACH TDeducc NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_Deducibles TDeducc
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Deducibles TDeducc


/* Definitions for BROWSE BR_Scoring                                    */
&Scoped-define FIELDS-IN-QUERY-BR_Scoring VarS VVaS PunS   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BR_Scoring   
&Scoped-define SELF-NAME BR_Scoring
&Scoped-define QUERY-STRING-BR_Scoring FOR EACH TScoring NO-LOCK BY TScoring.VarS INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BR_Scoring OPEN QUERY {&SELF-NAME} FOR EACH TScoring NO-LOCK BY TScoring.VarS INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BR_Scoring TScoring
&Scoped-define FIRST-TABLE-IN-QUERY-BR_Scoring TScoring


/* Definitions for BROWSE Br_Usuarios                                   */
&Scoped-define FIELDS-IN-QUERY-Br_Usuarios Tuxi.Agencia Tuxi.Usuario Tuxi.Nombre Tuxi.Cantidad   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Usuarios   
&Scoped-define SELF-NAME Br_Usuarios
&Scoped-define QUERY-STRING-Br_Usuarios FOR EACH Tuxi NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br_Usuarios OPEN QUERY {&SELF-NAME} FOR EACH Tuxi NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_Usuarios Tuxi
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Usuarios Tuxi


/* Definitions for BROWSE B_Partidas                                    */
&Scoped-define FIELDS-IN-QUERY-B_Partidas TPartidas.TCta TPArtidas.TDeb TPartidas.TCre   
&Scoped-define ENABLED-FIELDS-IN-QUERY-B_Partidas   
&Scoped-define SELF-NAME B_Partidas
&Scoped-define QUERY-STRING-B_Partidas FOR EACH TPartidas NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-B_Partidas OPEN QUERY {&SELF-NAME} FOR EACH TPartidas NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-B_Partidas TPartidas
&Scoped-define FIRST-TABLE-IN-QUERY-B_Partidas TPartidas


/* Definitions for FRAME F_Bancos                                       */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Bancos ~
    ~{&OPEN-QUERY-Bancos}

/* Definitions for FRAME F_Cerradas                                     */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Cerradas ~
    ~{&OPEN-QUERY-Br_Cerradas}

/* Definitions for FRAME F_Codeudores                                   */

/* Definitions for FRAME F_ConAdmisible                                 */

/* Definitions for FRAME F_Condicionada                                 */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Condicionada ~
    ~{&OPEN-QUERY-Br_Usuarios}

/* Definitions for FRAME F_ConHV                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_ConHV ~
    ~{&OPEN-QUERY-Br_ConHV}

/* Definitions for FRAME F_Consulta                                     */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Consulta ~
    ~{&OPEN-QUERY-Br_Consulta}

/* Definitions for FRAME F_Deducibles                                   */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Deducibles ~
    ~{&OPEN-QUERY-Br_Deducibles}

/* Definitions for FRAME F_Partidas                                     */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Partidas ~
    ~{&OPEN-QUERY-B_Partidas}

/* Definitions for FRAME F_Scoring                                      */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Scoring ~
    ~{&OPEN-QUERY-BR_Scoring}

/* Definitions for FRAME F_Solicitud                                    */
&Scoped-define FIELDS-IN-QUERY-F_Solicitud Creditos.Num_Credito ~
Creditos.Num_Solicitud Creditos.Fec_Aprobacion Creditos.Nit ~
Creditos.For_Interes Creditos.Monto Creditos.Plazo Creditos.Cuota ~
Creditos.Deducible Creditos.Id_Adicionales Creditos.Incremento ~
Creditos.Sdo_Capital Creditos.Tasa 
&Scoped-define QUERY-STRING-F_Solicitud FOR EACH Creditos SHARE-LOCK
&Scoped-define OPEN-QUERY-F_Solicitud OPEN QUERY F_Solicitud FOR EACH Creditos SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-F_Solicitud Creditos
&Scoped-define FIRST-TABLE-IN-QUERY-F_Solicitud Creditos


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-2 RECT-3 Cmb_Instancias BUTTON-1 ~
Btn_ProInstancia T_Refresh Btn_Imprimir Btn_Consulta BUTTON-2 BUTTON-4 
&Scoped-Define DISPLAYED-OBJECTS Cmb_Instancias T_Refresh NomUsuario 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-6 Garantias.Estado Nom_Aseguradora Nom_UsuGarantia ~
Btn_SalAdm 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fEdad wWin 
FUNCTION fEdad RETURNS CHARACTER
  (daFchaNcmiento AS DATE /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fFrmto wWin 
FUNCTION fFrmto RETURNS CHARACTER
  (c AS CHAR,l AS INTEGER /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fNmbreEstcion wWin 
FUNCTION fNmbreEstcion RETURNS CHARACTER
  (c AS CHAR /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fNmbreUsuario wWin 
FUNCTION fNmbreUsuario RETURNS CHARACTER
  (c AS CHAR /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_CanAdm 
     LABEL "Cancelar" 
     SIZE 15 BY 1.12.

DEFINE BUTTON Btn_ConAdm 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     LABEL "Button 160" 
     SIZE 8 BY 1.62.

DEFINE BUTTON Btn_InaAdm 
     LABEL "Inactivar" 
     SIZE 15 BY 1.12.

DEFINE BUTTON Btn_IngAdm 
     LABEL "Ingresar" 
     SIZE 15 BY 1.12.

DEFINE BUTTON Btn_SalAdm 
     LABEL "Salvar" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-161 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 161" 
     SIZE 8 BY 1.62.

DEFINE VARIABLE Nom_Aseguradora AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Nom_UsuGarantia AS CHARACTER FORMAT "X(35)":U 
     VIEW-AS FILL-IN 
     SIZE 31.29 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CredAval AS DECIMAL FORMAT ">>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.29 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_DispGaran AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.29 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-290
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 33 BY 5.88.

DEFINE RECTANGLE RECT-291
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 27.86 BY 2.85.

DEFINE RECTANGLE RECT-292
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 31 BY 2.85.

DEFINE RECTANGLE RECT-296
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 20.57 BY 1.04.

DEFINE RECTANGLE RECT-301
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 27.86 BY 3.08.

DEFINE BUTTON BUTTON-153 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 153" 
     SIZE 9 BY 1.88.

DEFINE VARIABLE E_Agregar AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 55 BY 5.38
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE BUTTON Btn_ImpChe 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 162" 
     SIZE 10.29 BY 1.46.

DEFINE VARIABLE Nom_Beneficiario AS CHARACTER FORMAT "X(50)":U 
     LABEL "Nombre del Beneficiario" 
     VIEW-AS FILL-IN 
     SIZE 33 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Num_Cheque AS CHARACTER FORMAT "X(10)":U 
     LABEL "Número de Cheque" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE BUTTON Btn_OutCerradas 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 143" 
     SIZE 8 BY 1.62.

DEFINE BUTTON BUTTON-154 
     LABEL "Ver solo Instancia y Descripción" 
     SIZE 25 BY 1.12.

DEFINE BUTTON Btn_Activas 
     LABEL "Inactivar" 
     SIZE 15 BY 1.12.

DEFINE BUTTON Btn_CanCod 
     LABEL "Cancelar" 
     SIZE 15 BY 1.12.

DEFINE BUTTON Btn_CreCod 
     LABEL "Crear" 
     SIZE 15 BY 1.12.

DEFINE BUTTON Btn_SalCod 
     LABEL "Salvar" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-155 
     LABEL "i" 
     SIZE 3.72 BY .81
     FONT 0.

DEFINE VARIABLE W_NitCodeudor AS CHARACTER FORMAT "X(14)":U 
     LABEL "Nit Codeudor" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomCodeudor AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 40.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE RActivas AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Activos", 1,
"Inactivos", 2
     SIZE 25 BY 1.08
     FONT 5 NO-UNDO.

DEFINE RECTANGLE RECT-297
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 17.43 BY 1.42.

DEFINE BUTTON Btn_OutConAdm 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 163" 
     SIZE 9 BY 1.62.

DEFINE VARIABLE R_ConAdm AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Activas", 1,
"Inactivas", 2
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE Cmb_InsCon AS CHARACTER FORMAT "X(256)":U 
     LABEL "Instancias" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 38 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE E_Condicion AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 48 BY 8.35
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE BUTTON Btn_OutConHV 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 153" 
     SIZE 9 BY 1.62.

DEFINE BUTTON Btn_OutConsulta 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 133" 
     SIZE 8 BY 1.62.

DEFINE VARIABLE Buscar AS CHARACTER FORMAT "X(30)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 19 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE VG_Alta AS CHARACTER FORMAT "X(40)":U 
      VIEW-AS TEXT 
     SIZE 29 BY .54
     BGCOLOR 12 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE VG_Media AS CHARACTER FORMAT "X(50)":U 
      VIEW-AS TEXT 
     SIZE 28.29 BY .62
     BGCOLOR 14 FONT 4 NO-UNDO.

DEFINE VARIABLE VG_Normal AS CHARACTER FORMAT "X(40)":U 
      VIEW-AS TEXT 
     SIZE 29 BY .65
     BGCOLOR 10 FGCOLOR 0 FONT 4 NO-UNDO.

DEFINE VARIABLE R_Organizar AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Solicitud", 1,
"Agencia", 2,
"Nit", 3,
"Nombre", 4,
"Fecha", 5
     SIZE 69 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-223
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 1.35.

DEFINE RECTANGLE RECT-287
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 30 BY 1.08
     BGCOLOR 14 .

DEFINE RECTANGLE RECT-288
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 31 BY 1.08
     BGCOLOR 10 .

DEFINE RECTANGLE RECT-289
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 31 BY 1.08
     BGCOLOR 12 .

DEFINE BUTTON Btn_Consulta 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "Button 10" 
     SIZE 10 BY 1.62.

DEFINE BUTTON Btn_Imprimir 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/impresora2.bmp":U
     LABEL "Button 8" 
     SIZE 10 BY 1.62.

DEFINE BUTTON Btn_ProInstancia 
     LABEL "Procesar Instancia" 
     SIZE 44 BY 1.12.

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/informacion.bmp":U
     LABEL "Button 1" 
     SIZE 10 BY 1.62.

DEFINE BUTTON BUTTON-2 
     LABEL "Salir" 
     SIZE 10 BY 1.62.

DEFINE BUTTON BUTTON-4 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 4" 
     SIZE 5 BY 1.12.

DEFINE VARIABLE Cmb_Instancias AS CHARACTER FORMAT "X(256)":U 
     LABEL "Instancias" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 44 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomUsuario AS CHARACTER FORMAT "X(40)":U 
      VIEW-AS TEXT 
     SIZE 44 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 12 BY 5.38.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 12 BY 2.42.

DEFINE VARIABLE T_Refresh AS LOGICAL INITIAL no 
     LABEL "Refrescar Automaticamente" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .77 NO-UNDO.

DEFINE BUTTON BUTTON-101 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 101" 
     SIZE 9 BY 1.88.

DEFINE BUTTON BUTTON-207 
     LABEL "Imprimir Deducibles" 
     SIZE 19 BY 1.88
     FONT 4.

DEFINE BUTTON Btn_OutUltima-2 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Btn_outultima 2" 
     SIZE 11 BY 1.46.

DEFINE BUTTON Btn_SalvaFormal 
     LABEL "Salvar" 
     SIZE 11 BY 1.46.

DEFINE VARIABLE NomCed AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 37.14 BY .77
     BGCOLOR 1 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomCta AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 36.57 BY .85
     BGCOLOR 1 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CtaTerceros AS CHARACTER FORMAT "X(14)":U 
     LABEL "Cta-Contable (Terceros)" 
     VIEW-AS FILL-IN 
     SIZE 15.14 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_FecIni AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Inicio" 
     VIEW-AS FILL-IN 
     SIZE 10.57 BY .81 TOOLTIP "Fecha de Pago, Solo si es Futura para Días de Interés-Anticipado"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_MenDes AS CHARACTER FORMAT "X(80)":U 
      VIEW-AS TEXT 
     SIZE 89 BY .62
     BGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_NetoAD AS DECIMAL FORMAT ">,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY .85
     BGCOLOR 5 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NitTercero AS CHARACTER FORMAT "X(12)":U 
     LABEL "Ced.Nit del Tercero" 
     VIEW-AS FILL-IN 
     SIZE 14.29 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE Rs_Desemb AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Todo a la Cta-Ahorros", 1,
"Orden a Terceros", 2,
"Desembolsa Cajero", 3
     SIZE 60.29 BY .65 NO-UNDO.

DEFINE RECTANGLE RECT-227
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 53.14 BY 1.65
     BGCOLOR 0 .

DEFINE RECTANGLE RECT-306
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.14 BY 1.27.

DEFINE RECTANGLE RECT-309
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 62 BY 1.04.

DEFINE BUTTON Btn_OutGarantias 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 136" 
     SIZE 7 BY 1.62.

DEFINE VARIABLE R_TipoGarantia AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Garantía Personal", 1,
"Garantía Admisible", 2,
"Garantía No Admisible", 3
     SIZE 77.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-226
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 95 BY 1.35
     BGCOLOR 18 .

DEFINE BUTTON Btn_NvoHv 
     LABEL "Ingresar" 
     SIZE 11 BY 1.35.

DEFINE BUTTON Btn_SalvaHV 
     LABEL "Salvar" 
     SIZE 11 BY 1.35.

DEFINE BUTTON BUTTON-149 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 149" 
     SIZE 11 BY 1.65.

DEFINE BUTTON BUTTON-150 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     LABEL "Button 150" 
     SIZE 11 BY 1.62.

DEFINE BUTTON BUTTON-152 
     LABEL "Cancelar" 
     SIZE 11 BY 1.38.

DEFINE VARIABLE Rs_SiNo AS LOGICAL INITIAL yes 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Si", yes,
"No", no
     SIZE 15 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-315
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 18.57 BY 1.35.

DEFINE BUTTON BUTTON-108 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 108" 
     SIZE 7 BY 1.35.

DEFINE BUTTON BUTTON-156 
     LABEL "Ver Información Detallada" 
     SIZE 30 BY 1.12.

DEFINE VARIABLE S_InfoCliente AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 43 BY 12.38
     BGCOLOR 15 FONT 2 NO-UNDO.

DEFINE BUTTON Btn_OutScoring 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 121" 
     SIZE 7 BY 1.62.

DEFINE VARIABLE S_InfoProducto AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 41 BY 7.81
     BGCOLOR 15 FONT 2 NO-UNDO.

DEFINE BUTTON Btn_AgregarTXT 
     LABEL "Agregar Texto" 
     SIZE 15 BY 1.62.

DEFINE BUTTON Btn_GraInstancia 
     LABEL "Grabar" 
     SIZE 15 BY 1.65.

DEFINE BUTTON Btn_insVolver 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 135" 
     SIZE 15 BY 1.62.

DEFINE BUTTON BUTTON-142 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "Button 142" 
     SIZE 10 BY 1.88.

DEFINE VARIABLE Vigencia AS CHARACTER FORMAT "X(15)":U 
     LABEL "Tiempo vigente de la instancia" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WHora_Ingreso AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 10.57 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE Whora_Retiro AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 10.57 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE W_Instancia AS CHARACTER FORMAT "X(45)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_UsuarioInstancia AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE BUTTON BUTTON-163 
     LABEL "Button 163" 
     SIZE 10 BY 1.62.

DEFINE BUTTON BUTTON-99 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 99" 
     SIZE 7 BY 1.62.

DEFINE VARIABLE Total_Puntaje AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Total Puntaje" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 5 NO-UNDO.

DEFINE BUTTON BtnCndciones 
     LABEL "Condiciones" 
     SIZE 11.57 BY 1.12 TOOLTIP "Condiciones De Otorgamiento".

DEFINE BUTTON Btn_Historia 
     LABEL "Historial" 
     SIZE 11.57 BY 1.12 TOOLTIP "Historial De Créditos".

DEFINE BUTTON Btn_HojaVida 
     LABEL "Pendientes" 
     SIZE 11.57 BY 1.12.

DEFINE BUTTON Btn_ImpPagare 
     LABEL "Lib / Pagaré" 
     SIZE 11 BY 1.12.

DEFINE BUTTON Btn_Proyectar 
     LABEL "Proyectar" 
     SIZE 11.57 BY 1.12.

DEFINE BUTTON Btn_Scoring 
     LABEL "Scoring" 
     SIZE 11.57 BY 1.12.

DEFINE BUTTON BUTTON-102 
     LABEL "Deducibles" 
     SIZE 11.57 BY 1.12.

DEFINE BUTTON BUTTON-120 
     LABEL "Producto" 
     SIZE 11.57 BY 1.12.

DEFINE BUTTON BUTTON-134 
     LABEL "Garantías" 
     SIZE 11.57 BY 1.12.

DEFINE BUTTON BUTTON-19 
     LABEL "i" 
     SIZE 3 BY .81
     FONT 0.

DEFINE BUTTON BUTTON-208 
     LABEL "Inf_Previa" 
     SIZE 11 BY 1.12.

DEFINE BUTTON BUTTON-209 
     LABEL "Libranza" 
     SIZE 11 BY 1.12.

DEFINE BUTTON BUTTON-210 
     LABEL "Todo" 
     SIZE 10 BY 1.12.

DEFINE BUTTON BUTTON-211 
     LABEL "Parcial" 
     SIZE 10 BY 1.12.

DEFINE BUTTON BUTTON-212 
     LABEL "Blanco" 
     SIZE 10 BY 1.12.

DEFINE BUTTON BUTTON-231 
     LABEL "Comp_Créd" 
     SIZE 12.43 BY 1.12.

DEFINE BUTTON BUTTON-232 
     LABEL "Carta_Instr" 
     SIZE 12.29 BY 1.12.

DEFINE BUTTON BUTTON-233 
     LABEL "Deb_Autom" 
     SIZE 10.86 BY 1.12.

DEFINE VARIABLE Cmb_Agencias AS CHARACTER FORMAT "X(256)":U 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 26.57 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_PerPago AS CHARACTER FORMAT "X(256)":U INITIAL "4" 
     LABEL "Período" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0 - Diario","1 - Semanal","2 - Decadal","3 - Quincenal","4 - Mensual","5 - Bimestral","6 - Trimestral","7 - Cuatrimestral","8 - Semestral","9 - Anual" 
     DROP-DOWN-LIST
     SIZE 46 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_Sistemas AS CHARACTER FORMAT "X(60)":U 
     LABEL "Sistema" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 46 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Ded_Ahorros AS DECIMAL FORMAT ">>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Deduccion ajuste creasuerte" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomIndicador AS CHARACTER FORMAT "X(30)":U 
     LABEL "Indicador" 
     VIEW-AS FILL-IN 
     SIZE 33 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomNit AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 57 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Nom_Producto AS CHARACTER FORMAT "X(70)":U 
     LABEL "Producto" 
     VIEW-AS FILL-IN 
     SIZE 32.72 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE Texto1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 57 BY .69
     BGCOLOR 17 FGCOLOR 12 FONT 14 NO-UNDO.

DEFINE VARIABLE Texto2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 57 BY .69
     BGCOLOR 17 FGCOLOR 12 FONT 14 NO-UNDO.

DEFINE VARIABLE W-fecEntLib AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Primer Pago" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Desembolso AS CHARACTER FORMAT "X(45)":U 
     LABEL "Desembolso" 
     VIEW-AS FILL-IN 
     SIZE 33 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_FaltApor AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Faltante" 
     VIEW-AS FILL-IN 
     SIZE 13.43 BY .77
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_ForPago AS CHARACTER FORMAT "X(45)":U 
     LABEL "Forma de Pago" 
     VIEW-AS FILL-IN 
     SIZE 33 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_PromedDD AS DECIMAL FORMAT "->>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15.43 BY .85 TOOLTIP "Promedio Ult.Trimestre Ahorros a la vista"
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_ReqPtmo AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Para este Crédito" 
     VIEW-AS FILL-IN 
     SIZE 13.43 BY .77
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_SdoApor AS DECIMAL FORMAT "->>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Saldo Actual" 
     VIEW-AS FILL-IN 
     SIZE 13.29 BY .77
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_TasaNominal AS DECIMAL FORMAT ">>9.999999":U INITIAL 0 
     LABEL "Nominal" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_TasaPeriodo AS DECIMAL FORMAT ">>9.9999999":U INITIAL 0 
     LABEL "Período" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Tipo_Credito AS CHARACTER FORMAT "X(20)":U 
     LABEL "Tipo" 
      VIEW-AS TEXT 
     SIZE 21 BY .81
     BGCOLOR 5 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_VrADesemb AS DECIMAL FORMAT "->,>>>,>>>,>>9":U INITIAL 0 
     LABEL "Vr.A Desembolsar Hoy" 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY .81
     BGCOLOR 5 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_VrCredACanc AS DECIMAL FORMAT ">,>>>,>>>,>>9":U INITIAL 0 
     LABEL "Deudas a Cancelar con esta Solicitud" 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY .81
     BGCOLOR 5 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-151
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 57 BY 2.46.

DEFINE RECTANGLE RECT-303
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 47.57 BY 3.35.

DEFINE RECTANGLE RECT-317
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 39.14 BY 2.42.

DEFINE RECTANGLE RECT-318
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 33 BY 2.42.

DEFINE RECTANGLE RECT-319
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 25 BY 2.42.

DEFINE BUTTON Btn_OutUltima 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 153" 
     SIZE 12.29 BY 1.85 TOOLTIP "Retorna a la Ventana principal SIN Efectuar Operación alguna".

DEFINE BUTTON Btn_SalvaUltima 
     LABEL "&S A L V A R" 
     SIZE 13.43 BY 1.42 TOOLTIP "Procesa el Desembolso".

DEFINE VARIABLE NomAsoc AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 54.72 BY .69
     BGCOLOR 5 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_IntAntic AS DECIMAL FORMAT ">>>>,>>>,>>9":U INITIAL 0 
     LABEL "Interés Anticipado" 
     VIEW-AS FILL-IN 
     SIZE 11.86 BY .81
     BGCOLOR 0 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NetoD AS DECIMAL FORMAT ">>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .81
     BGCOLOR 0 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NetoDesemb AS DECIMAL FORMAT ">>>>,>>>,>>9":U INITIAL 0 
     LABEL "Neto a Desembolsar" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .81
     BGCOLOR 0 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_VrAlAhorro AS DECIMAL FORMAT ">>>>,>>>,>>9":U INITIAL 0 
     LABEL "Valor a la Cta-Ahorros" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_VrCheque AS DECIMAL FORMAT ">>>>,>>>,>>9":U INITIAL 0 
     LABEL "En Cheque" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_VrEfectivo AS DECIMAL FORMAT ">>>>,>>>,>>9":U INITIAL 0 
     LABEL "En Efectivo" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE RECTANGLE RECT-224
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 56.72 BY 1.46
     BGCOLOR 0 .

DEFINE RECTANGLE RECT-225
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 93.57 BY .85
     BGCOLOR 0 .

DEFINE RECTANGLE RECT-228
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 25.57 BY .85
     BGCOLOR 0 .

DEFINE RECTANGLE RECT-229
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 21.72 BY .85
     BGCOLOR 0 .

DEFINE RECTANGLE RECT-304
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 21.72 BY 6.65.

DEFINE RECTANGLE RECT-305
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 69.72 BY .81.

DEFINE RECTANGLE RECT-308
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 34.29 BY 6.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Bancos FOR 
      Cuentas, 
      Operacion SCROLLING.

DEFINE QUERY BR_Admisible FOR 
      Garantias SCROLLING.

DEFINE QUERY Br_Cerradas FOR 
      TCerradas SCROLLING.

DEFINE QUERY Br_Codeudores FOR 
      TCode SCROLLING.

DEFINE QUERY Br_ConHV FOR 
      Hoja_Vida SCROLLING.

DEFINE QUERY Br_Consulta FOR 
      Consulta SCROLLING.

DEFINE QUERY Br_Deducibles FOR 
      TDeducc SCROLLING.

DEFINE QUERY BR_Scoring FOR 
      TScoring SCROLLING.

DEFINE QUERY Br_Usuarios FOR 
      Tuxi SCROLLING.

DEFINE QUERY B_Partidas FOR 
      TPartidas SCROLLING.

DEFINE QUERY F_Solicitud FOR 
      Creditos SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Bancos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Bancos wWin _STRUCTURED
  QUERY Bancos NO-LOCK DISPLAY
      Operacion.Cod_Compensa COLUMN-LABEL "Banco" FORMAT "99":U
            WIDTH 5.43
      Cuentas.Cuenta FORMAT "X(14)":U WIDTH 14
      Cuentas.Nombre FORMAT "X(35)":U WIDTH 30
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 54 BY 5.38
         BGCOLOR 15 FONT 5.

DEFINE BROWSE BR_Admisible
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BR_Admisible wWin _FREEFORM
  QUERY BR_Admisible NO-LOCK DISPLAY
      Garantias.Tipo_Garantia      COLUMN-LABEL "Tipo"
      Garantias.Identificacion_Bien FORMAT "X(12)":U
      Garantias.Nom_Bien FORMAT "X(60)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 92 BY 6.73
         BGCOLOR 15 FONT 5 FIT-LAST-COLUMN.

DEFINE BROWSE Br_Cerradas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Cerradas wWin _FREEFORM
  QUERY Br_Cerradas NO-LOCK DISPLAY
      TCerradas.Instancia FORMAT "999":U COLUMN-LABEL "Ins"
      TCerradas.INom_Instancia COLUMN-LABEL "Nombre Instancia" FORMAT "x(30)"
      TCerradas.Usuario FORMAT "X(12)":U COLUMN-LABEL "Usuario"
      TCerradas.INom_Usuario COLUMN-LABEL "Nombre Usuario"
      TCerradas.Fec_Retiro FORMAT "99/99/9999":U   COLUMN-LABEL "Fec.Retiro"
      TCerradas.Descripcion COLUMN-LABEL "Descripcion" FORMAT "X(200)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 94 BY 10.5
         BGCOLOR 15 FONT 4 ROW-HEIGHT-CHARS .65 FIT-LAST-COLUMN.

DEFINE BROWSE Br_Codeudores
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Codeudores wWin _FREEFORM
  QUERY Br_Codeudores NO-LOCK DISPLAY
      TC_AgeCode COLUMN-LABEL "Age"
 TC_NitCode COLUMN-LABEL "Nit" FORMAT "X(14)"
 TC_NomCode COLUMN-LABEL "Nombre" FORMAT "X(45)"
 TC_Aprob   COLUMN-LABEL "Aprob"
 TC_TelCdRs COLUMN-LABEL "Tel.Residencia" FORMAT "X(14)"
 TC_TelCdCo COLUMN-LABEL "Tel.Comercial" FORMAT "X(14)"
 TC_emlCode COLUMN-LABEL "Correo Electronico"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 91 BY 5.65
         BGCOLOR 15 FONT 4 FIT-LAST-COLUMN.

DEFINE BROWSE Br_ConHV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_ConHV wWin _FREEFORM
  QUERY Br_ConHV NO-LOCK DISPLAY
      Hoja_Vida.Fec_Grabacion FORMAT "99/99/9999":U
      Hoja_Vida.Observacion FORMAT "X(400)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 95 BY 6.73
         BGCOLOR 15 FONT 4 FIT-LAST-COLUMN.

DEFINE BROWSE Br_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Consulta wWin _FREEFORM
  QUERY Br_Consulta NO-LOCK DISPLAY
      Consulta.Num_Credito FORMAT "999999999":U COLUMN-LABEL "Num.Credito"
    Consulta.Num_Solicitud FORMAT "999999999":U COLUMN-LABEL "Num.Solicitud"
    Consulta.AgeCredito COLUMN-LABEL "Age"
    Consulta.Nit FORMAT "X(12)":U
    Consulta.Nombre FORMAT "X(25)"
    Consulta.Fec_Ingreso FORMAT "99/99/9999":U COLUMN-LABEL "Fecha"
    Consulta.Hor_Ingreso COLUMN-LABEL "Hora"
    Consulta.Monto COLUMN-LABEL "Monto"
    Consulta.Vigencia COLUMN-LABEL "Vig"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 92 BY 13.19
         BGCOLOR 15 FONT 4 FIT-LAST-COLUMN.

DEFINE BROWSE Br_Deducibles
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Deducibles wWin _FREEFORM
  QUERY Br_Deducibles NO-LOCK DISPLAY
      TDeducc.Nom_Deducible FORMAT "X(20)":U COLUMN-LABEL "Nombre Deducible"
  TDeducc.Valor FORMAT ">>>,>>>,>>9.999":U COLUMN-LABEL "Valor"
  TDeducc.Valor_Impuesto FORMAT ">>>,>>>,>>9.999":U COLUMN-LABEL "Impuesto"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 53 BY 7.81
         BGCOLOR 15 FONT 2 ROW-HEIGHT-CHARS .54 FIT-LAST-COLUMN.

DEFINE BROWSE BR_Scoring
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BR_Scoring wWin _FREEFORM
  QUERY BR_Scoring NO-LOCK DISPLAY
      VarS COLUMN-LABEL "Variable"
   VVaS COLUMN-LABEL "Valor" 
   PunS COLUMN-LABEL "Puntaje"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 42 BY 11.31
         BGCOLOR 15 FONT 2 ROW-HEIGHT-CHARS .46 FIT-LAST-COLUMN.

DEFINE BROWSE Br_Usuarios
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Usuarios wWin _FREEFORM
  QUERY Br_Usuarios NO-LOCK DISPLAY
      Tuxi.Agencia FORMAT "999":U COLUMN-LABEL "Agencia"
  Tuxi.Usuario FORMAT "X(4)":U COLUMN-LABEL "Usuario"
  Tuxi.Nombre FORMAT "X(35)":U COLUMN-LABEL "Nombre" 
  Tuxi.Cantidad FORMAT "999":U COLUMN-LABEL "Cantidad"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 46 BY 8.08
         BGCOLOR 15 FONT 4 ROW-HEIGHT-CHARS .5 FIT-LAST-COLUMN.

DEFINE BROWSE B_Partidas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS B_Partidas wWin _FREEFORM
  QUERY B_Partidas NO-LOCK DISPLAY
      TPartidas.TCta
  TPArtidas.TDeb 
  TPartidas.TCre
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 89 BY 9.69
         BGCOLOR 15 FGCOLOR 0 FONT 5 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_ImpCpte
     Rs_SiNo AT ROW 3.35 COL 14 NO-LABEL
     "Desea Imprimir el documento...?" VIEW-AS TEXT
          SIZE 29.86 BY 1.08 AT ROW 2 COL 6.14
          BGCOLOR 17 FGCOLOR 7 
     RECT-315 AT ROW 3.08 COL 12.43
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 31.86 ROW 10.38
         SIZE 41.43 BY 5.81
         BGCOLOR 17 
         TITLE "Frame Impresión".

DEFINE FRAME F_Formalizar
     Btn_SalvaFormal AT ROW 1.27 COL 85
     Btn_OutUltima-2 AT ROW 2.77 COL 85
     W_NetoAD AT ROW 3.77 COL 1.72 COLON-ALIGNED NO-LABEL
     W_FecIni AT ROW 3.85 COL 27.43 COLON-ALIGNED
     W_CtaTerceros AT ROW 4.12 COL 65.43 COLON-ALIGNED
     Creditos.Pagare AT ROW 4.88 COL 27.29 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.14 BY .88
          BGCOLOR 15 FGCOLOR 0 
     NomCta AT ROW 4.96 COL 44 COLON-ALIGNED NO-LABEL
     W_NitTercero AT ROW 5.96 COL 43.86 COLON-ALIGNED
     NomCed AT ROW 6 COL 58.14 COLON-ALIGNED NO-LABEL
     Creditos.Estado AT ROW 7.77 COL 4.57 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Aprobado No Desembolsado", 1,
"Formalizado", 2,
"No-Disponible", 3,
"Retirado sin Formalizar", 4,
" ", 5
          SIZE 87.43 BY .69
     Rs_Desemb AT ROW 9.12 COL 35.72 NO-LABEL
     W_MenDes AT ROW 10.27 COL 3 COLON-ALIGNED NO-LABEL
     "Marque Solo Si Desea Desembolsar:" VIEW-AS TEXT
          SIZE 32.29 BY .81 AT ROW 9.08 COL 3
          BGCOLOR 0 FGCOLOR 15 
     "Neto Desembolso" VIEW-AS TEXT
          SIZE 15.72 BY .81 AT ROW 2.96 COL 2
          BGCOLOR 0 FGCOLOR 15 
     "Se ha llegado a la Instancia Formalización del Desembolso," VIEW-AS TEXT
          SIZE 51.29 BY .81 AT ROW 1.23 COL 24.14
          BGCOLOR 0 FGCOLOR 15 
     "Cambie el estado del Crédito según el estudio realizado." VIEW-AS TEXT
          SIZE 49 BY .62 AT ROW 2.04 COL 24.14
          BGCOLOR 0 FGCOLOR 15 
     RECT-227 AT ROW 1.12 COL 23
     RECT-306 AT ROW 7.46 COL 3
     RECT-309 AT ROW 8.92 COL 35
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3.77
         SIZE 97.57 BY 11.31
         BGCOLOR 18 FGCOLOR 15 FONT 5
         TITLE "Formalización Desembolso".

DEFINE FRAME FRAME-A
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 1.27
         SIZE 5 BY 1.35
         TITLE "".

DEFINE FRAME F_Creditos
     Cmb_Instancias AT ROW 1.27 COL 10 COLON-ALIGNED
     BUTTON-1 AT ROW 1.54 COL 103
     Btn_ProInstancia AT ROW 2.35 COL 12
     T_Refresh AT ROW 2.35 COL 60
     Btn_Imprimir AT ROW 3.15 COL 103
     Btn_Consulta AT ROW 4.77 COL 103
     BUTTON-2 AT ROW 17.69 COL 103
     BUTTON-4 AT ROW 19.58 COL 106
     NomUsuario AT ROW 1.27 COL 55 COLON-ALIGNED NO-LABEL
     RECT-2 AT ROW 1.27 COL 102
     RECT-3 AT ROW 17.15 COL 102
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.14 BY 21.31
         BGCOLOR 17 FONT 5.

DEFINE FRAME F_Consulta
     Br_Consulta AT ROW 2.08 COL 4
     R_Organizar AT ROW 15.54 COL 25 NO-LABEL
     Btn_OutConsulta AT ROW 16.73 COL 88
     Buscar AT ROW 17.15 COL 19 COLON-ALIGNED
     VG_Normal AT ROW 1.23 COL 3 COLON-ALIGNED NO-LABEL
     VG_Media AT ROW 1.27 COL 33.72 COLON-ALIGNED NO-LABEL
     VG_Alta AT ROW 1.27 COL 64 COLON-ALIGNED NO-LABEL
     "Las solicitudes en rojo se encuentran condicionadas" VIEW-AS TEXT
          SIZE 45 BY .81 AT ROW 17.15 COL 42
          FGCOLOR 7 
     "Organizada por..." VIEW-AS TEXT
          SIZE 16 BY .81 AT ROW 15.54 COL 7
          FGCOLOR 7 
     RECT-223 AT ROW 15.27 COL 4
     RECT-287 AT ROW 1 COL 35
     RECT-288 AT ROW 1 COL 4
     RECT-289 AT ROW 1 COL 65
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3.69
         SIZE 98 BY 18.31
         BGCOLOR 17 FONT 5
         TITLE "Solicitudes Disponibles".

DEFINE FRAME F_Scoring
     BR_Scoring AT ROW 1.27 COL 2
     BUTTON-99 AT ROW 12.85 COL 36
     Total_Puntaje AT ROW 13.38 COL 13 COLON-ALIGNED
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 57 ROW 4.5
         SIZE 44 BY 14.81
         BGCOLOR 17 FONT 5
         TITLE "Datos del Scoring de Créditos".

DEFINE FRAME F_Partidas
     B_Partidas AT ROW 1.54 COL 3
     BUTTON-163 AT ROW 12.04 COL 47
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 5 ROW 7.46
         SIZE 93 BY 14
         TITLE "Partidas Contables".

DEFINE FRAME F_Condicionada
     Cmb_InsCon AT ROW 1.27 COL 11 COLON-ALIGNED
     Br_Usuarios AT ROW 2.35 COL 52
     E_Condicion AT ROW 3.15 COL 3 NO-LABEL
     "  Usuarios Disponibles para recibir la Solicitud" VIEW-AS TEXT
          SIZE 46 BY .81 AT ROW 1.27 COL 52
          BGCOLOR 18 FGCOLOR 15 
     "Condición" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 2.35 COL 3
     "Los Usuarios en Rojo han tenido la Solicitud asignada" VIEW-AS TEXT
          SIZE 46 BY .62 AT ROW 10.69 COL 52
          FGCOLOR 15 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 10.42
         SIZE 98 BY 11.58
         BGCOLOR 18 FGCOLOR 15 FONT 5
         TITLE "Instancia a la cual se devuelve la solicitud a condicionar".

DEFINE FRAME F_HojaVida
     Hoja_Vida.Fec_Grabacion AT ROW 1.27 COL 70 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .81
          BGCOLOR 18 FGCOLOR 15 
     BUTTON-150 AT ROW 1.27 COL 86
     Hoja_Vida.Asunto_Cumplido AT ROW 1.54 COL 4
          VIEW-AS TOGGLE-BOX
          SIZE 18 BY .81
     Hoja_Vida.Observacion AT ROW 2.62 COL 4 NO-LABEL
          VIEW-AS EDITOR LARGE
          SIZE 80 BY 7.08
          BGCOLOR 15 
     Btn_SalvaHV AT ROW 3.15 COL 86
     Btn_NvoHv AT ROW 4.5 COL 86
     BUTTON-152 AT ROW 5.85 COL 86
     BUTTON-149 AT ROW 8 COL 86
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 12.04
         SIZE 98 BY 9.96
         BGCOLOR 17 FONT 5
         TITLE "Hoja de Vida".

DEFINE FRAME F_Cerradas
     Br_Cerradas AT ROW 1.54 COL 3
     Btn_OutCerradas AT ROW 12.31 COL 89
     BUTTON-154 AT ROW 12.58 COL 62
     "La instancia activa se encuentra en letra color rojo" VIEW-AS TEXT
          SIZE 43 BY .81 AT ROW 12.58 COL 3
          FGCOLOR 7 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 7.88
         SIZE 98 BY 14.12
         BGCOLOR 17 FGCOLOR 0 FONT 4
         TITLE "Consulta de Instancias Procesadas y Actuales".

DEFINE FRAME F_ConHV
     Br_ConHV AT ROW 1.27 COL 3
     Btn_OutConHV AT ROW 8.27 COL 89
     "   Los Mensajes en Rojo estan pendientes por cumplirse" VIEW-AS TEXT
          SIZE 85 BY 1.08 AT ROW 8.54 COL 3
          BGCOLOR 0 FGCOLOR 15 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 12.04
         SIZE 98 BY 9.96
         BGCOLOR 17 FONT 4
         TITLE "Asuntos Pendientes".

DEFINE FRAME F_Deducibles
     Br_Deducibles AT ROW 1.27 COL 2
     BUTTON-207 AT ROW 9.35 COL 25
     BUTTON-101 AT ROW 9.35 COL 45
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 4 ROW 10.42
         SIZE 55 BY 11.31
         BGCOLOR 17 
         TITLE "Deducibles del Producto".

DEFINE FRAME F_InfoCliente
     S_InfoCliente AT ROW 3.15 COL 3 NO-LABEL
     BUTTON-156 AT ROW 13.92 COL 2
     BUTTON-108 AT ROW 13.92 COL 38
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 6.65
         SIZE 46 BY 15.35
         BGCOLOR 17 FONT 5
         TITLE "Información Financiera del Cliente".

DEFINE FRAME F_InfoProducto
     S_InfoProducto AT ROW 1.27 COL 3 NO-LABEL
     Btn_OutScoring AT ROW 9.19 COL 37
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 53 ROW 7.73
         SIZE 45 BY 10.77
         BGCOLOR 17 FGCOLOR 0 FONT 5
         TITLE "Información del Producto".

DEFINE FRAME F_Garantias
     R_TipoGarantia AT ROW 1.23 COL 6.14 NO-LABEL
     Btn_OutGarantias AT ROW 12.31 COL 90
     RECT-226 AT ROW 1 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 6.65
         SIZE 98 BY 15.35
         BGCOLOR 17 FONT 5
         TITLE "Garantías".

DEFINE FRAME F_Codeudores
     Relaciones.Aprobada AT ROW 1.27 COL 78.72 HELP
          "Aprobada/Negada" NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Aceptado", yes,
"Rechazado", no
          SIZE 14.14 BY 1.12 TOOLTIP "Marque si el Codeudor es Aceptado Ò No."
     W_NitCodeudor AT ROW 1.54 COL 14 COLON-ALIGNED
     W_NomCodeudor AT ROW 1.54 COL 27 COLON-ALIGNED NO-LABEL
     BUTTON-155 AT ROW 1.54 COL 70.14
     Btn_CreCod AT ROW 2.62 COL 3
     Btn_CanCod AT ROW 2.62 COL 19
     Btn_Activas AT ROW 2.62 COL 35
     Btn_SalCod AT ROW 2.62 COL 51
     RActivas AT ROW 2.62 COL 69 NO-LABEL
     Br_Codeudores AT ROW 3.96 COL 3
     RECT-297 AT ROW 1.12 COL 76.43
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 2.08
         SIZE 95 BY 9.69
         BGCOLOR 17 FONT 5
         TITLE "Personal".

DEFINE FRAME F_ConAdmisible
     BR_Admisible AT ROW 1.27 COL 2
     Btn_OutConAdm AT ROW 8.27 COL 85
     R_ConAdm AT ROW 8.58 COL 3.57 NO-LABEL
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.08
         SIZE 95 BY 9.96
         BGCOLOR 17 FONT 5
         TITLE "Consulta de Garantías Admisibles".

DEFINE FRAME F_Admisible
     Garantias.Descripcion_Bien2 AT ROW 4.5 COL 2 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 46 BY 1.35
          BGCOLOR 15 
     Garantias.Tipo_Garantia AT ROW 1.12 COL 2.14 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Propiedad", 1,
"Aportes", 6,
"Prenda", 2,
"Inversión", 3,
"Cdat-Contrac NoAd", 4,
"Otras NoAd", 5
          SIZE 92.86 BY .81
     Garantias.Contabilizada AT ROW 2.08 COL 2
          LABEL "Contabilizada?"
          VIEW-AS TOGGLE-BOX
          SIZE 13 BY .69
     Garantias.Estado AT ROW 3 COL 3 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Activa", 1,
"Inactiva", 2
          SIZE 18 BY .54
     Garantias.Identificacion_Bien AT ROW 1.92 COL 32 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.43 BY .81
          BGCOLOR 15 
     Garantias.Nom_Bien AT ROW 1.92 COL 47 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 46 BY .81
          BGCOLOR 15 
     Garantias.Val_Bien AT ROW 2.77 COL 32 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.43 BY .81
          BGCOLOR 15 
     Garantias.Fec_Creacion AT ROW 2.88 COL 58.29 COLON-ALIGNED
          LABEL "Fecha Creación"
          VIEW-AS FILL-IN 
          SIZE 11.14 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Garantias.Fec_Retiro AT ROW 2.88 COL 80.86 COLON-ALIGNED
          LABEL "Fecha Retiro"
          VIEW-AS FILL-IN 
          SIZE 11.86 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Garantias.Descripcion_Bien AT ROW 4.5 COL 49 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 46 BY 1.35
          BGCOLOR 15 FONT 4
     Garantias.Nro_Seguro AT ROW 6.5 COL 18.86 COLON-ALIGNED
          LABEL "Número de Seguro"
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 15 
     Garantias.Nit_Aseguradora AT ROW 7.38 COL 18.86 COLON-ALIGNED
          LABEL "Nit Aseguradora"
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 15 
     Nom_Aseguradora AT ROW 8.27 COL 2 COLON-ALIGNED NO-LABEL
     Garantias.Fec_IniSeguro AT ROW 9.19 COL 18.86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 15 
     Garantias.Fec_FinSeguro AT ROW 10.12 COL 18.86 COLON-ALIGNED
          LABEL "Fecha Vencimiento"
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 15 
     Garantias.Val_Asegurado AT ROW 11 COL 18.86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 15 
     Garantias.Nom_Impuesto AT ROW 6.27 COL 46.86 COLON-ALIGNED
          LABEL "Nombre"
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 15 FONT 4
     Garantias.Fec_VctoImpuesto AT ROW 7.12 COL 46.86 COLON-ALIGNED
          LABEL "Fec.Vencimiento"
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 15 
     Garantias.Val_Impuesto AT ROW 7.96 COL 46.86 COLON-ALIGNED
          LABEL "Valor"
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 15 
     Garantias.Fec_ProxAvaluo AT ROW 6.31 COL 78.86 COLON-ALIGNED
          LABEL "Fec.Próx.Avaluo"
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 15 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 2.08
         SIZE 96 BY 13.19
         BGCOLOR 17 FONT 4.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F_Admisible
     Garantias.Fec_UltAvaluo AT ROW 7.19 COL 78.86 COLON-ALIGNED
          LABEL "Fec.Últ.Avaluo"
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 15 
     Garantias.Val_UltAvaluo AT ROW 8 COL 78.86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 15 
     Nom_UsuGarantia AT ROW 10.88 COL 61.72 COLON-ALIGNED NO-LABEL
     Btn_ConAdm AT ROW 11.73 COL 79
     BUTTON-161 AT ROW 11.73 COL 87
     Btn_SalAdm AT ROW 12.15 COL 4.14
     Btn_CanAdm AT ROW 12.15 COL 20.14
     Btn_IngAdm AT ROW 12.15 COL 36.14
     Btn_InaAdm AT ROW 12.15 COL 52.14
     Garantias.Aprobada AT ROW 9.27 COL 75.14 HELP
          "Aprobada/Negada" NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Aprobada", yes,
"Negada", no
          SIZE 18.86 BY .69 TOOLTIP "Marque si la Garantìa fue Ò no aprobada"
     W_CredAval AT ROW 9.65 COL 45.86 COLON-ALIGNED NO-LABEL
     W_DispGaran AT ROW 10.96 COL 45.86 COLON-ALIGNED NO-LABEL
     "Valor Crèditos Avalados + Vr.Solicitado" VIEW-AS TEXT
          SIZE 26.29 BY .5 AT ROW 9.12 COL 35.86
          FGCOLOR 7 
     "Valor Disponible de esta Garantia" VIEW-AS TEXT
          SIZE 25.86 BY .5 AT ROW 10.46 COL 36
          FGCOLOR 7 
     "  Seguro" VIEW-AS TEXT
          SIZE 8 BY .58 AT ROW 5.92 COL 3
          FGCOLOR 7 
     "Impuesto" VIEW-AS TEXT
          SIZE 7.72 BY .54 AT ROW 5.85 COL 36.29
          FGCOLOR 7 
     "  Avalùo" VIEW-AS TEXT
          SIZE 8 BY .58 AT ROW 5.92 COL 64.72
          FGCOLOR 7 FONT 4
     "Concepto del Abogado" VIEW-AS TEXT
          SIZE 16 BY .5 AT ROW 3.88 COL 49.43
          FGCOLOR 7 
     "Información de la Garantía" VIEW-AS TEXT
          SIZE 19 BY .5 AT ROW 3.88 COL 2.43
          FGCOLOR 7 
     "La garantìa es :" VIEW-AS TEXT
          SIZE 10.43 BY .62 AT ROW 9.38 COL 64
     "Usuario que Ingresò" VIEW-AS TEXT
          SIZE 16.57 BY .5 AT ROW 10.38 COL 64
          FGCOLOR 7 
     RECT-290 AT ROW 6.12 COL 2
     RECT-291 AT ROW 6.12 COL 35.14
     RECT-292 AT ROW 6.12 COL 63.72
     RECT-296 AT ROW 9.12 COL 74.29
     RECT-301 AT ROW 8.85 COL 35.14
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 2.08
         SIZE 96 BY 13.19
         BGCOLOR 17 FONT 4
         TITLE "Garantías Admisibles".

DEFINE FRAME F_Agregar
     E_Agregar AT ROW 1.27 COL 2 NO-LABEL
     BUTTON-153 AT ROW 6.92 COL 48
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 23 ROW 11.5
         SIZE 57 BY 8.88
         BGCOLOR 17 
         TITLE "Texto a ser Agregado".

DEFINE FRAME F_Ultima
     Creditos.Desembolso AT ROW 1.85 COL 3.86 HELP
          "Forma del Desembolso del Crédito" NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Solo Efectivo", 1,
"Solo Cheque", 2,
"Transferencia", 9,
"Cta Ahorro", 3,
"Efectivo y Cheque", 5,
"Efectivo y Cta-Ahorros", 6,
"Cheque y Cta-Ahorros", 7,
"No Aplica", 8
          SIZE 18.86 BY 5.77 TOOLTIP "Seleccione la distribución del desembolso"
          FONT 4
     NomAsoc AT ROW 2.69 COL 24.29 COLON-ALIGNED NO-LABEL
     Btn_SalvaUltima AT ROW 3 COL 82.72
     W_FecIni AT ROW 3.69 COL 41 COLON-ALIGNED
          LABEL "Fecha Primer Pago" FORMAT "99/99/9999":U
          VIEW-AS FILL-IN 
          SIZE 11.86 BY .81 TOOLTIP "Fecha de Pago, Solo si es Futura para Días de Interés-Anticipado"
          BGCOLOR 15 FGCOLOR 0 
     Creditos.Estado AT ROW 4.81 COL 26.72 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "NAp.", 1,
"Desembolsar", 2,
"N.Ap", 3,
"Retirar Sin Desembolsar", 4,
"N.A", 5
          SIZE 69 BY .54
     W_CtaTerceros AT ROW 5.54 COL 79 COLON-ALIGNED
          LABEL "Cta-Contable (Terceros)" FORMAT "X(14)":U
          VIEW-AS FILL-IN 
          SIZE 15.14 BY .81
          BGCOLOR 15 FGCOLOR 0 
     NomCta AT ROW 6.38 COL 57.57 COLON-ALIGNED NO-LABEL FORMAT "X(30)":U
          VIEW-AS FILL-IN 
          SIZE 36.57 BY .88
          BGCOLOR 1 FGCOLOR 15 
     Creditos.Pagare AT ROW 6.46 COL 36.86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.14 BY .81
          BGCOLOR 0 FGCOLOR 15 
     Creditos.Monto AT ROW 8.65 COL 79.14 COLON-ALIGNED
          LABEL "Monto del Credito" FORMAT ">>,>>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 14.29 BY .81
          BGCOLOR 5 FGCOLOR 15 
     W_IntAntic AT ROW 10.69 COL 80.14 COLON-ALIGNED
     W_VrEfectivo AT ROW 11.73 COL 80.14 COLON-ALIGNED
     W_VrCheque AT ROW 12.65 COL 80.14 COLON-ALIGNED
     W_VrAlAhorro AT ROW 13.58 COL 80 COLON-ALIGNED
     W_NetoDesemb AT ROW 14.73 COL 80 COLON-ALIGNED
     Btn_OutUltima AT ROW 16.46 COL 83.43
     W_NetoD AT ROW 17.04 COL 62.86 COLON-ALIGNED NO-LABEL
     W_MenDes AT ROW 7.77 COL 2.86 COLON-ALIGNED NO-LABEL FORMAT "X(80)":U
           VIEW-AS TEXT 
          SIZE 89 BY .69
          BGCOLOR 0 
     "Opción Desembolso" VIEW-AS TEXT
          SIZE 18 BY .58 AT ROW 1.15 COL 4.86
          BGCOLOR 0 FGCOLOR 15 
     "Se ha llegado a la última instancia en el proceso de Desembolso," VIEW-AS TEXT
          SIZE 56 BY .58 AT ROW 1.23 COL 26.72
          BGCOLOR 0 FGCOLOR 15 
     "Distribución del Desembolso" VIEW-AS TEXT
          SIZE 25.14 BY .62 AT ROW 9.69 COL 65.43
          BGCOLOR 0 FGCOLOR 15 
     "Click en Opción Desembolso, Según la necesidad del asociado" VIEW-AS TEXT
          SIZE 55.57 BY .62 AT ROW 1.81 COL 26.72
          BGCOLOR 0 FGCOLOR 15 
     RECT-224 AT ROW 1.12 COL 26.14
     RECT-225 AT ROW 7.65 COL 2.43
     RECT-228 AT ROW 9.58 COL 65.14
     RECT-229 AT ROW 1.04 COL 2.72
     RECT-304 AT ROW 1 COL 2.57
     RECT-305 AT ROW 4.65 COL 26.29
     RECT-308 AT ROW 9.85 COL 61.29
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS TOP-ONLY NO-UNDERLINE THREE-D 
         AT COL 4 ROW 3.69
         SIZE 98 BY 18.31
         BGCOLOR 18 FGCOLOR 15 FONT 5
         TITLE "Estado de la Solicitud".

DEFINE FRAME F_Bancos
     Bancos AT ROW 1.27 COL 2
     Nom_Beneficiario AT ROW 6.92 COL 3.57
     Num_Cheque AT ROW 8 COL 6.43
     Btn_ImpChe AT ROW 8.04 COL 42.72
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2.29 ROW 8.73
         SIZE 56 BY 9.65
         BGCOLOR 17 FONT 4
         TITLE "Cuentas de Bancos".

DEFINE FRAME F_Instancias
     BUTTON-142 AT ROW 1.27 COL 3
     Mov_Instancias.Fec_Ingreso AT ROW 1.27 COL 73 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .81
          BGCOLOR 18 FGCOLOR 15 
     WHora_Ingreso AT ROW 1.27 COL 85 COLON-ALIGNED NO-LABEL
     W_Instancia AT ROW 1.42 COL 12 COLON-ALIGNED NO-LABEL
     W_UsuarioInstancia AT ROW 2.35 COL 12 COLON-ALIGNED NO-LABEL
     Mov_Instancias.Fec_Retiro AT ROW 2.35 COL 73 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Whora_Retiro AT ROW 2.35 COL 85 COLON-ALIGNED NO-LABEL
     Vigencia AT ROW 3.42 COL 48 COLON-ALIGNED
     Mov_Instancias.Estado AT ROW 3.54 COL 3
          VIEW-AS TOGGLE-BOX
          SIZE 17 BY .77
     Mov_Instancias.Descripcion AT ROW 4.5 COL 3 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 78 BY 9.42
          BGCOLOR 15 
     Btn_GraInstancia AT ROW 4.5 COL 82
     Btn_AgregarTXT AT ROW 6.38 COL 82
     Btn_insVolver AT ROW 12.31 COL 82
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 7.88
         SIZE 98 BY 14.12
         BGCOLOR 17 FONT 5
         TITLE "Procesar Instancias".

DEFINE FRAME F_Solicitud
     Creditos.Num_Credito AT ROW 1.19 COL 43.14 COLON-ALIGNED
          LABEL "Crèdito"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Num_Solicitud AT ROW 1.19 COL 62.86 COLON-ALIGNED
          LABEL "Solicitud"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Fec_Aprobacion AT ROW 1.19 COL 85.43 COLON-ALIGNED
          LABEL "Aprobación"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Cmb_Agencias AT ROW 1.23 COL 9 COLON-ALIGNED
     Creditos.Nit AT ROW 2.31 COL 9 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 26.29 BY .81
          BGCOLOR 15 
     NomNit AT ROW 2.31 COL 35.57 COLON-ALIGNED NO-LABEL
     BUTTON-19 AT ROW 2.31 COL 94.72
     Nom_Producto AT ROW 3.35 COL 35.57 COLON-ALIGNED
     Creditos.For_Interes AT ROW 4.15 COL 10 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Vencido", 1,
"Anticipado", 2
          SIZE 22 BY .62
     Texto1 AT ROW 4.31 COL 36 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     Creditos.Monto AT ROW 4.85 COL 19.57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
          BGCOLOR 15 
     Texto2 AT ROW 5 COL 36 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     Creditos.Plazo AT ROW 5.69 COL 19.57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
          BGCOLOR 15 
     Cmb_Sistemas AT ROW 5.77 COL 46.29 COLON-ALIGNED
     Creditos.Cuota AT ROW 6.54 COL 19.57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
          BGCOLOR 15 
     Cmb_PerPago AT ROW 6.73 COL 46.29 COLON-ALIGNED
     Creditos.Deducible AT ROW 7.38 COL 19.57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
          BGCOLOR 15 
     Creditos.Id_Adicionales AT ROW 8.04 COL 59.43 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Financiados", 1,
"Descontados", 2,
"Caja", 3
          SIZE 37 BY .54
     Creditos.Incremento AT ROW 8.23 COL 19.57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY .73
          BGCOLOR 15 
     Creditos.Sdo_Capital AT ROW 9 COL 19.57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY .77
          BGCOLOR 15 
     W_TasaNominal AT ROW 9.12 COL 65.57 COLON-ALIGNED
     W_TasaPeriodo AT ROW 9.12 COL 84.14 COLON-ALIGNED
     Creditos.Tasa AT ROW 9.15 COL 46.57 COLON-ALIGNED
          LABEL "Efectiva"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 18 FGCOLOR 15 
     NomIndicador AT ROW 10.38 COL 62.43 COLON-ALIGNED
     W_SdoApor AT ROW 11.04 COL 16.14 COLON-ALIGNED
     W_PromedDD AT ROW 11.04 COL 30.57 COLON-ALIGNED NO-LABEL
     W_ForPago AT ROW 11.23 COL 62.29 COLON-ALIGNED
     W_ReqPtmo AT ROW 11.92 COL 16 COLON-ALIGNED
     W_Desembolso AT ROW 12.08 COL 62.29 COLON-ALIGNED
     W_FaltApor AT ROW 12.77 COL 16 COLON-ALIGNED
     Ded_Ahorros AT ROW 12.92 COL 77.14 COLON-ALIGNED
     W_VrCredACanc AT ROW 13.77 COL 82.72 COLON-ALIGNED
     W-fecEntLib AT ROW 14.12 COL 34.86 COLON-ALIGNED WIDGET-ID 2
     Btn_Historia AT ROW 14.19 COL 3
     Btn_ImpPagare AT ROW 14.19 COL 3
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3.69
         SIZE 99 BY 18.31
         BGCOLOR 17 FONT 5.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F_Solicitud
     Btn_HojaVida AT ROW 14.19 COL 3
     Btn_Scoring AT ROW 14.19 COL 3
     Btn_Proyectar AT ROW 14.19 COL 3
     BtnCndciones AT ROW 14.19 COL 3 WIDGET-ID 4
     BUTTON-120 AT ROW 14.19 COL 3
     BUTTON-134 AT ROW 14.19 COL 3
     BUTTON-102 AT ROW 14.19 COL 3
     W_VrADesemb AT ROW 14.62 COL 82.72 COLON-ALIGNED
     BUTTON-210 AT ROW 16.58 COL 27.72 WIDGET-ID 24
     BUTTON-211 AT ROW 16.58 COL 38.57 WIDGET-ID 28
     BUTTON-212 AT ROW 16.58 COL 49.43 WIDGET-ID 30
     BUTTON-208 AT ROW 16.62 COL 3.14 WIDGET-ID 20
     BUTTON-209 AT ROW 16.62 COL 15.29 WIDGET-ID 22
     BUTTON-233 AT ROW 16.62 COL 61.14 WIDGET-ID 42
     BUTTON-231 AT ROW 16.62 COL 72.57 HELP
          "COMPROBANTE CREDITO" WIDGET-ID 38
     BUTTON-232 AT ROW 16.62 COL 86 HELP
          "CARTA DE INSTRUCCIONES" WIDGET-ID 40
     W_Tipo_Credito AT ROW 3.38 COL 74.29 COLON-ALIGNED
     "  Imprimir Pagaré" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 15.5 COL 36.72 WIDGET-ID 36
     "Pago de Deducible" VIEW-AS TEXT
          SIZE 17.86 BY .58 AT ROW 7.96 COL 41.14
          FGCOLOR 7 
     "Interes" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 4.12 COL 3.72 WIDGET-ID 8
     "Prom.Ult-Trim.(A la Vista)" VIEW-AS TEXT
          SIZE 21.57 BY .5 AT ROW 10.35 COL 27.43
     " Tasas" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 8.69 COL 41
          BGCOLOR 18 FGCOLOR 7 
     "Información de Aportes" VIEW-AS TEXT
          SIZE 20.43 BY .62 AT ROW 10.19 COL 3.57
          FGCOLOR 7 
     RECT-151 AT ROW 7.73 COL 40.14
     RECT-303 AT ROW 10.5 COL 1.43
     RECT-317 AT ROW 15.81 COL 60 WIDGET-ID 26
     RECT-318 AT ROW 15.81 COL 27 WIDGET-ID 32
     RECT-319 AT ROW 15.81 COL 2 WIDGET-ID 34
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3.69
         SIZE 99 BY 18.31
         BGCOLOR 17 FONT 5
         TITLE "Información del Credito".


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
         TITLE              = "Proceso de Desembolso"
         HEIGHT             = 21.5
         WIDTH              = 113.43
         MAX-HEIGHT         = 36.62
         MAX-WIDTH          = 182.86
         VIRTUAL-HEIGHT     = 36.62
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
ASSIGN FRAME FRAME-A:FRAME = FRAME F_Formalizar:HANDLE
       FRAME F_Admisible:FRAME = FRAME F_Garantias:HANDLE
       FRAME F_Agregar:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_Bancos:FRAME = FRAME F_Ultima:HANDLE
       FRAME F_Cerradas:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_Codeudores:FRAME = FRAME F_Garantias:HANDLE
       FRAME F_ConAdmisible:FRAME = FRAME F_Garantias:HANDLE
       FRAME F_Condicionada:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_ConHV:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_Consulta:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_Deducibles:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_Garantias:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_HojaVida:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_InfoCliente:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_InfoProducto:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_Instancias:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_Partidas:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_Scoring:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_Solicitud:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_Ultima:FRAME = FRAME F_Creditos:HANDLE.

/* SETTINGS FOR FRAME FRAME-A
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FRAME-A:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_Admisible
   NOT-VISIBLE Custom                                                   */
ASSIGN 
       FRAME F_Admisible:HIDDEN           = TRUE.

/* SETTINGS FOR RADIO-SET Garantias.Aprobada IN FRAME F_Admisible
   NO-ENABLE EXP-HELP                                                   */
/* SETTINGS FOR BUTTON Btn_IngAdm IN FRAME F_Admisible
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON Btn_SalAdm IN FRAME F_Admisible
   6                                                                    */
/* SETTINGS FOR TOGGLE-BOX Garantias.Contabilizada IN FRAME F_Admisible
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR EDITOR Garantias.Descripcion_Bien IN FRAME F_Admisible
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET Garantias.Estado IN FRAME F_Admisible
   NO-ENABLE 6                                                          */
/* SETTINGS FOR FILL-IN Garantias.Fec_Creacion IN FRAME F_Admisible
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Garantias.Fec_FinSeguro IN FRAME F_Admisible
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Garantias.Fec_ProxAvaluo IN FRAME F_Admisible
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Garantias.Fec_Retiro IN FRAME F_Admisible
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Garantias.Fec_UltAvaluo IN FRAME F_Admisible
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Garantias.Fec_VctoImpuesto IN FRAME F_Admisible
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Garantias.Nit_Aseguradora IN FRAME F_Admisible
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Nom_Aseguradora IN FRAME F_Admisible
   NO-ENABLE 6                                                          */
/* SETTINGS FOR FILL-IN Garantias.Nom_Impuesto IN FRAME F_Admisible
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Nom_UsuGarantia IN FRAME F_Admisible
   NO-ENABLE 6                                                          */
/* SETTINGS FOR FILL-IN Garantias.Nro_Seguro IN FRAME F_Admisible
   EXP-LABEL                                                            */
/* SETTINGS FOR RADIO-SET Garantias.Tipo_Garantia IN FRAME F_Admisible
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Garantias.Val_Impuesto IN FRAME F_Admisible
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN W_CredAval IN FRAME F_Admisible
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_DispGaran IN FRAME F_Admisible
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Agregar
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Agregar:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_Bancos
                                                                        */
/* BROWSE-TAB Bancos 1 F_Bancos */
/* SETTINGS FOR BUTTON Btn_ImpChe IN FRAME F_Bancos
   NO-ENABLE                                                            */
ASSIGN 
       Btn_ImpChe:HIDDEN IN FRAME F_Bancos           = TRUE.

/* SETTINGS FOR FILL-IN Nom_Beneficiario IN FRAME F_Bancos
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN Num_Cheque IN FRAME F_Bancos
   ALIGN-L                                                              */
/* SETTINGS FOR FRAME F_Cerradas
   NOT-VISIBLE                                                          */
/* BROWSE-TAB Br_Cerradas TEXT-28 F_Cerradas */
ASSIGN 
       FRAME F_Cerradas:HIDDEN           = TRUE.

ASSIGN 
       Br_Cerradas:ALLOW-COLUMN-SEARCHING IN FRAME F_Cerradas = TRUE.

/* SETTINGS FOR FRAME F_Codeudores
                                                                        */
/* BROWSE-TAB Br_Codeudores RActivas F_Codeudores */
/* SETTINGS FOR RADIO-SET Relaciones.Aprobada IN FRAME F_Codeudores
   NO-ENABLE EXP-HELP                                                   */
/* SETTINGS FOR BUTTON Btn_Activas IN FRAME F_Codeudores
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON Btn_CanCod IN FRAME F_Codeudores
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON Btn_CreCod IN FRAME F_Codeudores
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON Btn_SalCod IN FRAME F_Codeudores
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NitCodeudor IN FRAME F_Codeudores
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomCodeudor IN FRAME F_Codeudores
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_ConAdmisible
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BR_Admisible 1 F_ConAdmisible */
ASSIGN 
       FRAME F_ConAdmisible:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_Condicionada
   NOT-VISIBLE                                                          */
/* BROWSE-TAB Br_Usuarios Cmb_InsCon F_Condicionada */
ASSIGN 
       FRAME F_Condicionada:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_ConHV
   NOT-VISIBLE                                                          */
/* BROWSE-TAB Br_ConHV TEXT-27 F_ConHV */
ASSIGN 
       FRAME F_ConHV:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_Consulta
                                                                        */
/* BROWSE-TAB Br_Consulta RECT-289 F_Consulta */
/* SETTINGS FOR FILL-IN VG_Alta IN FRAME F_Consulta
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN VG_Media IN FRAME F_Consulta
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN VG_Normal IN FRAME F_Consulta
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Creditos
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN NomUsuario IN FRAME F_Creditos
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Deducibles
   NOT-VISIBLE                                                          */
/* BROWSE-TAB Br_Deducibles 1 F_Deducibles */
ASSIGN 
       FRAME F_Deducibles:HIDDEN           = TRUE
       FRAME F_Deducibles:MOVABLE          = TRUE.

/* SETTINGS FOR FRAME F_Formalizar
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Formalizar:HIDDEN           = TRUE
       FRAME F_Formalizar:MOVABLE          = TRUE.

/* SETTINGS FOR FILL-IN NomCed IN FRAME F_Formalizar
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN NomCta IN FRAME F_Formalizar
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Pagare IN FRAME F_Formalizar
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_CtaTerceros IN FRAME F_Formalizar
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_FecIni IN FRAME F_Formalizar
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_MenDes IN FRAME F_Formalizar
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NetoAD IN FRAME F_Formalizar
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NitTercero IN FRAME F_Formalizar
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Garantias
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Garantias:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_HojaVida
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_HojaVida:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_ImpCpte
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_ImpCpte:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_InfoCliente
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_InfoCliente:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_InfoProducto
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_InfoProducto:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_Instancias
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Instancias:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN Mov_Instancias.Fec_Ingreso IN FRAME F_Instancias
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Mov_Instancias.Fec_Retiro IN FRAME F_Instancias
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Vigencia IN FRAME F_Instancias
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN WHora_Ingreso IN FRAME F_Instancias
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Whora_Retiro IN FRAME F_Instancias
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Instancia IN FRAME F_Instancias
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_UsuarioInstancia IN FRAME F_Instancias
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Partidas
   NOT-VISIBLE                                                          */
/* BROWSE-TAB B_Partidas 1 F_Partidas */
ASSIGN 
       FRAME F_Partidas:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_Scoring
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BR_Scoring 1 F_Scoring */
ASSIGN 
       FRAME F_Scoring:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN Total_Puntaje IN FRAME F_Scoring
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Solicitud
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Solicitud:HIDDEN           = TRUE
       FRAME F_Solicitud:PRIVATE-DATA     = 
                "02 Información Cónyuge o Compañero (a) permanente".

/* SETTINGS FOR BUTTON BtnCndciones IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON Btn_Historia IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON Btn_HojaVida IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON Btn_ImpPagare IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON Btn_Proyectar IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON Btn_Scoring IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-102 IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-120 IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-134 IN FRAME F_Solicitud
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-231:PRIVATE-DATA IN FRAME F_Solicitud     = 
                "Comp".

/* SETTINGS FOR COMBO-BOX Cmb_Agencias IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX Cmb_PerPago IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX Cmb_Sistemas IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Cuota IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Deducible IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Ded_Ahorros IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Fec_Aprobacion IN FRAME F_Solicitud
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR RADIO-SET Creditos.For_Interes IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET Creditos.Id_Adicionales IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Incremento IN FRAME F_Solicitud
   NO-ENABLE                                                            */
ASSIGN 
       Creditos.Incremento:HIDDEN IN FRAME F_Solicitud           = TRUE.

/* SETTINGS FOR FILL-IN Creditos.Monto IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Nit IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN NomIndicador IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN NomNit IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Nom_Producto IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Num_Credito IN FRAME F_Solicitud
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Num_Solicitud IN FRAME F_Solicitud
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Plazo IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Sdo_Capital IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Tasa IN FRAME F_Solicitud
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Texto1 IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Texto2 IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Desembolso IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_FaltApor IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_ForPago IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_PromedDD IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_ReqPtmo IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_SdoApor IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_TasaNominal IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_TasaPeriodo IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Tipo_Credito IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_VrADesemb IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_VrCredACanc IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Ultima
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Ultima:HIDDEN           = TRUE.

/* SETTINGS FOR RADIO-SET Creditos.Desembolso IN FRAME F_Ultima
   EXP-HELP                                                             */
/* SETTINGS FOR RADIO-SET Creditos.Estado IN FRAME F_Ultima
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Monto IN FRAME F_Ultima
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN NomAsoc IN FRAME F_Ultima
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN NomCta IN FRAME F_Ultima
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Pagare IN FRAME F_Ultima
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_CtaTerceros IN FRAME F_Ultima
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_IntAntic IN FRAME F_Ultima
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_MenDes IN FRAME F_Ultima
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NetoD IN FRAME F_Ultima
   NO-ENABLE                                                            */
ASSIGN 
       W_NetoD:HIDDEN IN FRAME F_Ultima           = TRUE.

/* SETTINGS FOR FILL-IN W_NetoDesemb IN FRAME F_Ultima
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_VrAlAhorro IN FRAME F_Ultima
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_VrEfectivo IN FRAME F_Ultima
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Bancos
/* Query rebuild information for BROWSE Bancos
     _TblList          = "bdCentral.Cuentas,bdCentral.Operacion OF bdCentral.Cuentas"
     _Options          = "NO-LOCK"
     _OrdList          = "bdCentral.Operacion.Cod_Compensa|yes"
     _Where[1]         = "Cuentas.Cod_FlujoEfec = ""D""
 AND Cuentas.Car_Efectivo = 3
 AND Cuentas.Estado = 1"
     _Where[2]         = "Operacion.Clase_Operacion = 1
 AND Operacion.Ctrl_EfeChe = 2
 AND Operacion.Tipo_Operacion = 2
 and Operacion.Cod_Compensa <> ?"
     _FldNameList[1]   > bdCentral.Operacion.Cod_Compensa
"Operacion.Cod_Compensa" "Banco" ? "integer" ? ? ? ? ? ? no ? no no "5.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > bdCentral.Cuentas.Cuenta
"Cuentas.Cuenta" ? ? "character" ? ? ? ? ? ? no ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > bdCentral.Cuentas.Nombre
"Cuentas.Nombre" ? "X(35)" "character" ? ? ? ? ? ? no ? no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE Bancos */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BR_Admisible
/* Query rebuild information for BROWSE BR_Admisible
     _START_FREEFORM
/*OPEN QUERY {&SELF-NAME} FOR EACH Garantias INDEXED-REPOSITION.*/
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is NOT OPENED
*/  /* BROWSE BR_Admisible */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_Cerradas
/* Query rebuild information for BROWSE Br_Cerradas
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TCerradas NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE Br_Cerradas */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_Codeudores
/* Query rebuild information for BROWSE Br_Codeudores
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TCode NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is NOT OPENED
*/  /* BROWSE Br_Codeudores */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_ConHV
/* Query rebuild information for BROWSE Br_ConHV
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Hoja_Vida WHERE
       Hoja_Vida.Tipo      EQ 9 AND Hoja_Vida.Codigo EQ 1 AND
       Hoja_Vida.Instancia EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) AND
       Hoja_Vida.DoctoRef  EQ INTEGER(Creditos.Num_Credito:SCREEN-VALUE IN FRAME F_Solicitud) AND
       Hoja_Vida.Nit       EQ Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud AND
       Hoja_Vida.Asunto_Cumplido EQ NO AND
       Hoja_Vida.Usuario   EQ W_Usuario
  INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE Br_ConHV */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_Consulta
/* Query rebuild information for BROWSE Br_Consulta
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Consulta NO-LOCK BY Consulta.Num_Credito INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE Br_Consulta */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_Deducibles
/* Query rebuild information for BROWSE Br_Deducibles
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TDeducc NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE Br_Deducibles */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BR_Scoring
/* Query rebuild information for BROWSE BR_Scoring
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TScoring NO-LOCK BY TScoring.VarS INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE BR_Scoring */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_Usuarios
/* Query rebuild information for BROWSE Br_Usuarios
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Tuxi NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE Br_Usuarios */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE B_Partidas
/* Query rebuild information for BROWSE B_Partidas
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TPartidas NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE B_Partidas */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-A
/* Query rebuild information for FRAME FRAME-A
     _Query            is NOT OPENED
*/  /* FRAME FRAME-A */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Admisible
/* Query rebuild information for FRAME F_Admisible
     _Query            is NOT OPENED
*/  /* FRAME F_Admisible */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Codeudores
/* Query rebuild information for FRAME F_Codeudores
     _Query            is NOT OPENED
*/  /* FRAME F_Codeudores */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Formalizar
/* Query rebuild information for FRAME F_Formalizar
     _Query            is NOT OPENED
*/  /* FRAME F_Formalizar */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_HojaVida
/* Query rebuild information for FRAME F_HojaVida
     _Query            is NOT OPENED
*/  /* FRAME F_HojaVida */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_ImpCpte
/* Query rebuild information for FRAME F_ImpCpte
     _Query            is NOT OPENED
*/  /* FRAME F_ImpCpte */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Instancias
/* Query rebuild information for FRAME F_Instancias
     _Query            is NOT OPENED
*/  /* FRAME F_Instancias */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Solicitud
/* Query rebuild information for FRAME F_Solicitud
     _TblList          = "bdCentral.Creditos"
     _Query            is OPENED
*/  /* FRAME F_Solicitud */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Ultima
/* Query rebuild information for FRAME F_Ultima
     _Query            is NOT OPENED
*/  /* FRAME F_Ultima */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME F_Creditos:HANDLE
       ROW             = 2.35
       COLUMN          = 7
       HEIGHT          = 1.35
       WIDTH           = 4
       HIDDEN          = yes
       SENSITIVE       = yes.

PROCEDURE adm-create-controls:
      CtrlFrame:NAME = "CtrlFrame":U .
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */
      CtrlFrame:MOVE-AFTER(BUTTON-1:HANDLE IN FRAME F_Creditos).

END PROCEDURE.

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Proceso de Desembolso */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Proceso de Desembolso */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_HojaVida
&Scoped-define SELF-NAME Hoja_Vida.Asunto_Cumplido
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Hoja_Vida.Asunto_Cumplido wWin
ON VALUE-CHANGED OF Hoja_Vida.Asunto_Cumplido IN FRAME F_HojaVida /* Cumplido */
DO:
  ENABLE Btn_SalvaHV WITH FRAME F_HojaVida.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Bancos
&Scoped-define FRAME-NAME F_Bancos
&Scoped-define SELF-NAME Bancos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Bancos wWin
ON MOUSE-SELECT-CLICK OF Bancos IN FRAME F_Bancos
DO:
    DEFINE VAR numCheque AS DECIMAL.

    IF creditos.desembolso <> 9 THEN DO:
        IF Bancos:NUM-SELECTED-ROWS NE 0 AND Cuentas.Cuenta NE "?" AND Cuentas.Cuenta GT " " THEN DO:
            ASSIGN W_CtaBanco = Cuentas.Cuenta.

            FOR EACH mov_contable WHERE mov_contable.agencia = w_agencia
                                    AND mov_contable.cuenta = cuentas.cuenta
                                    AND mov_contable.fec_contable <= w_fecha NO-LOCK BY mov_contable.fec_contable DESCENDING
                                                                                     BY mov_contable.num_documento DESCENDING:
                numCheque = INTEGER(mov_contable.doc_referencia) NO-ERROR.

                IF numCheque > 0 THEN DO:
                    Num_Cheque:SCREEN-VALUE IN FRAME F_Bancos = STRING(DECIMAL(mov_contable.doc_referencia) + 1).
                    LEAVE.
                END.

                numCheque = 0.
                num_cheque:SENSITIVE = TRUE.
            END.

            num_cheque:SENSITIVE = TRUE.
        END.
    END.
    ELSE DO:
        Num_Cheque:SCREEN-VALUE IN FRAME F_Bancos = "TRANSF".
        num_cheque:SENSITIVE = FALSE.
    END.

    ASSIGN num_cheque.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Bancos wWin
ON MOUSE-SELECT-DBLCLICK OF Bancos IN FRAME F_Bancos
DO:
    DEFINE VAR numCheque AS DECIMAL INITIAL 0.
    DEFINE VAR aux AS INTEGER.

    ASSIGN W_CtaBanco = Cuentas.Cuenta.

    IF creditos.desembolso <> 9 THEN DO:
        FOR EACH mov_contable WHERE mov_contable.agencia = w_agencia
                                AND mov_contable.cuenta = cuentas.cuenta
                                AND mov_contable.fec_contable <= w_fecha NO-LOCK BY mov_contable.fec_contable DESCENDING
                                                                                 BY mov_contable.num_documento DESCENDING:
            aux = INTEGER(mov_contable.doc_referencia) NO-ERROR.

            IF aux > 0 THEN DO:
                numCheque = aux.
                LEAVE.
            END.
        END.

        Num_Cheque:SCREEN-VALUE IN FRAME F_Bancos = STRING(numCheque + 1).
        num_cheque:SENSITIVE = TRUE.
    END.
    ELSE DO:
        Num_Cheque:SCREEN-VALUE IN FRAME F_Bancos = "TRANSF".
        num_cheque:SENSITIVE = FALSE.
    END.

    ASSIGN num_cheque.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BR_Admisible
&Scoped-define FRAME-NAME F_ConAdmisible
&Scoped-define SELF-NAME BR_Admisible
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BR_Admisible wWin
ON MOUSE-SELECT-DBLCLICK OF BR_Admisible IN FRAME F_ConAdmisible
DO:
  APPLY "choose" TO Btn_OutConAdm.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br_Cerradas
&Scoped-define FRAME-NAME F_Cerradas
&Scoped-define SELF-NAME Br_Cerradas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Cerradas wWin
ON MOUSE-SELECT-DBLCLICK OF Br_Cerradas IN FRAME F_Cerradas
DO:
  APPLY "choose" TO Btn_OutCerradas IN FRAME F_Cerradas.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Cerradas wWin
ON ROW-DISPLAY OF Br_Cerradas IN FRAME F_Cerradas
DO:
  IF TCerradas.Instancia EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) THEN DO:
     Tcerradas.Instancia:FGCOL IN BROWSE BR_Cerradas = 12.
     TCerradas.INom_Instancia:FGCOL IN BROWSE BR_Cerradas = 12.
     TCerradas.Usuario:FGCOL IN BROWSE BR_Cerradas = 12.
     TCerradas.INom_Usuario:FGCOL IN BROWSE BR_Cerradas = 12.
     TCerradas.Fec_Retiro:FGCOL IN BROWSE BR_Cerradas = 12.
     TCerradas.Descripcion:FGCOL IN BROWSE BR_Cerradas = 12.
  END.
  ELSE DO:
     Tcerradas.Instancia:FGCOL IN BROWSE BR_Cerradas = 0.
     TCerradas.INom_Instancia:FGCOL IN BROWSE BR_Cerradas = 0.
     TCerradas.Usuario:FGCOL IN BROWSE BR_Cerradas = 0.
     TCerradas.INom_Usuario:FGCOL IN BROWSE BR_Cerradas = 0.
     TCerradas.Fec_Retiro:FGCOL IN BROWSE BR_Cerradas = 0.
     TCerradas.Descripcion:FGCOL IN BROWSE BR_Cerradas = 0.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br_Codeudores
&Scoped-define FRAME-NAME F_Codeudores
&Scoped-define SELF-NAME Br_Codeudores
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Codeudores wWin
ON MOUSE-SELECT-CLICK OF Br_Codeudores IN FRAME F_Codeudores
DO:
  IF Br_Codeudores:NUM-SELECTED-ROWS IN FRAME F_Codeudores NE 0 THEN 
       ASSIGN W_NitCodeudor:SCREEN-VALUE     = TCode.TC_NitCode
            W_NomCodeudor:SCREEN-VALUE       = TCode.TC_NomCode
            Relaciones.Aprobada:SCREEN-VALUE = STRING(TCode.TC_Aprob).
           /* Btn_SalCod:SENSITIVE             = TRUE.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br_ConHV
&Scoped-define FRAME-NAME F_ConHV
&Scoped-define SELF-NAME Br_ConHV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_ConHV wWin
ON MOUSE-SELECT-DBLCLICK OF Br_ConHV IN FRAME F_ConHV
DO:
  APPLY "choose" TO Btn_OutConHV.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_ConHV wWin
ON ROW-DISPLAY OF Br_ConHV IN FRAME F_ConHV
DO:
   IF NOT Hoja_Vida.Asunto_Cumplido THEN DO:
     Hoja_Vida.Fec_Grabacion:FGCOL IN BROWSE Br_ConHV = 12.
     Hoja_Vida.Observacion:FGCOL IN BROWSE Br_ConHV = 12. 
   END.
   ELSE DO:
     Hoja_Vida.Fec_Grabacion:FGCOL IN BROWSE Br_ConHV = 0.
     Hoja_Vida.Observacion:FGCOL IN BROWSE Br_ConHV = 0. 
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br_Consulta
&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME Br_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Consulta wWin
ON MOUSE-SELECT-DBLCLICK OF Br_Consulta IN FRAME F_Consulta
DO:
  APPLY "choose" TO Btn_OutConsulta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Consulta wWin
ON ROW-DISPLAY OF Br_Consulta IN FRAME F_Consulta
DO:
  IF Consulta.Vigencia LE (W_VigIns / 2) THEN DO:
     ASSIGN Consulta.Num_Credito:BGCOL IN BROWSE Br_Consulta = 10
            Consulta.Num_Solicitud:BGCOL IN BROWSE Br_Consulta = 10
            Consulta.AgeCredito:BGCOL IN BROWSE Br_Consulta = 10
            Consulta.Nit:BGCOL IN BROWSE Br_Consulta = 10
            Consulta.Nombre:BGCOL IN BROWSE Br_Consulta = 10
            Consulta.Fec_Ingreso:BGCOL IN BROWSE Br_Consulta = 10
            Consulta.Hor_Ingreso:BGCOL IN BROWSE Br_Consulta = 10
            Consulta.Monto:BGCOL IN BROWSE Br_Consulta = 10
            Consulta.Vigencia:BGCOL IN BROWSE Br_Consulta = 10.
/*     ASSIGN Consulta.Num_Credito:FGCOL IN BROWSE Br_Consulta = 15
            Consulta.Num_Solicitud:FGCOL IN BROWSE Br_Consulta = 15
            Consulta.AgeCredito:FGCOL IN BROWSE Br_Consulta = 15 
            Consulta.Nit:FGCOL IN BROWSE Br_Consulta = 15
            Consulta.Nombre:FGCOL IN BROWSE Br_Consulta = 15
            Consulta.Fec_Ingreso:FGCOL IN BROWSE Br_Consulta = 15
            Consulta.Hor_Ingreso:FGCOL IN BROWSE Br_Consulta = 15
            Consulta.Monto:FGCOL IN BROWSE Br_Consulta = 15
            Consulta.Vigencia:FGCOL IN BROWSE Br_Consulta = 15.*/
  END.
  IF Consulta.Vigencia GT (W_VigIns / 2) AND Consulta.Vigencia LE W_VigIns THEN DO:
     ASSIGN Consulta.Num_Credito:BGCOL IN BROWSE Br_Consulta = 14
            Consulta.Num_Solicitud:BGCOL IN BROWSE Br_Consulta = 14
            Consulta.AgeCredito:BGCOL IN BROWSE Br_Consulta = 14 
            Consulta.Nit:BGCOL IN BROWSE Br_Consulta = 14
            Consulta.Nombre:BGCOL IN BROWSE Br_Consulta = 14
            Consulta.Fec_Ingreso:BGCOL IN BROWSE Br_Consulta = 14
            Consulta.Hor_Ingreso:BGCOL IN BROWSE Br_Consulta = 14
            Consulta.Monto:BGCOL IN BROWSE Br_Consulta = 14
            Consulta.Vigencia:BGCOL IN BROWSE Br_Consulta = 14.
  END.
  IF Consulta.Vigencia GT W_VigIns THEN DO:
     ASSIGN Consulta.Num_Credito:BGCOL IN BROWSE Br_Consulta = 12
            Consulta.Num_Solicitud:BGCOL IN BROWSE Br_Consulta = 12
            Consulta.AgeCredito:BGCOL IN BROWSE Br_Consulta = 12 
            Consulta.Nit:BGCOL IN BROWSE Br_Consulta = 12
            Consulta.Nombre:BGCOL IN BROWSE Br_Consulta = 12
            Consulta.Fec_Ingreso:BGCOL IN BROWSE Br_Consulta = 12
            Consulta.Hor_Ingreso:BGCOL IN BROWSE Br_Consulta = 12
            Consulta.Monto:BGCOL IN BROWSE Br_Consulta = 12
            Consulta.Vigencia:BGCOL IN BROWSE Br_Consulta = 12.
     ASSIGN Consulta.Num_Credito:FGCOL IN BROWSE Br_Consulta = 15
            Consulta.Num_Solicitud:FGCOL IN BROWSE Br_Consulta = 15
            Consulta.AgeCredito:FGCOL IN BROWSE Br_Consulta = 15 
            Consulta.Nit:FGCOL IN BROWSE Br_Consulta = 15
            Consulta.Nombre:FGCOL IN BROWSE Br_Consulta = 15
            Consulta.Fec_Ingreso:FGCOL IN BROWSE Br_Consulta = 15
            Consulta.Hor_Ingreso:FGCOL IN BROWSE Br_Consulta = 15
            Consulta.Monto:FGCOL IN BROWSE Br_Consulta = 15
            Consulta.Vigencia:FGCOL IN BROWSE Br_Consulta = 15.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br_Usuarios
&Scoped-define FRAME-NAME F_Condicionada
&Scoped-define SELF-NAME Br_Usuarios
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Usuarios wWin
ON ROW-DISPLAY OF Br_Usuarios IN FRAME F_Condicionada
DO:
  IF Tuxi.Proceso THEN DO:
     Tuxi.Agencia:FGCOL IN BROWSE Br_Usuarios = 12.
     Tuxi.Usuario:FGCOL IN BROWSE Br_Usuarios = 12.
     Tuxi.Nombre:FGCOL IN BROWSE Br_Usuarios = 12.
  END.
  ELSE DO:
     Tuxi.Agencia:FGCOL IN BROWSE Br_Usuarios = 0.
     Tuxi.Usuario:FGCOL IN BROWSE Br_Usuarios = 0.
     Tuxi.Nombre:FGCOL IN BROWSE Br_Usuarios = 0.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Solicitud
&Scoped-define SELF-NAME BtnCndciones
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnCndciones wWin
ON CHOOSE OF BtnCndciones IN FRAME F_Solicitud /* Condiciones */
DO:
    DEF VAR o_des LIKE w_des NO-UNDO.
    o_des = W_Des.
    w_des = TRUE.
    RUN Calcular_Deducible (decimal(Creditos.Monto:SCREEN-VALUE)).
    IF ERROR-STATUS:ERROR 
    THEN DO:
        MESSAGE "ERROR: Calculando Deducibles"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.

    DEFINE VAR Listado     AS CHARACTER INITIAL "" NO-UNDO.
    Listado = W_PathSpl + W_Usuario + "Cndciones.LST".
    W_TipoInforme = "CNDCIONES".
    {INCLUIDO\Imprimir.I "Listado"}
    w_des = o_des.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Codeudores
&Scoped-define SELF-NAME Btn_Activas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Activas wWin
ON CHOOSE OF Btn_Activas IN FRAME F_Codeudores /* Inactivar */
DO:
    /* harold por precaucion
    RETURN.
DO WITH FRAME F_Codeudores:
  IF Br_Codeudores:NUM-SELECTED-ROWS EQ 0 THEN DO:
     MESSAGE "Debe posicionarse en la relación a inactivar" SKIP
             "mediante el mouse. Rectifique!!!" VIEW-AS ALERT-BOX INFORMATION.
  END.
  ELSE DO:
    FIND Relaciones WHERE Relaciones.Nit            EQ Creditos.Nit  AND
                          Relaciones.Cod_Relacion   EQ 11 AND
                          Relaciones.Clase_Producto EQ 2  AND
                          Relaciones.Cod_Producto   EQ Creditos.Cod_Credito AND
                          Relaciones.Cuenta         EQ TRIM(STRING(Creditos.Num_Credito)) AND
                          Relaciones.Nit_Relacion   EQ TCode.TC_NitCode NO-ERROR.
    IF AVAILABLE(Relaciones) THEN DO:
       IF RActivas:SCREEN-VALUE EQ "1" THEN
          ASSIGN Relaciones.Estado = 2
                 Relaciones.Fec_Inactividad = W_Fecha
                 Relaciones.Aprobada        = FALSE
                 TCode.TC_Aprob             = FALSE
                 TCode.TC_EstRela           = 2
                 TCode.TC_FecReti           = W_Fecha.
       ELSE
          ASSIGN Relaciones.Estado          = 1
                 Relaciones.Fec_Inactividad = ?
                 TCode.TC_EstRela           = 1
                 TCode.TC_FecReti           = ?.

       OPEN QUERY Br_Codeudores 
          FOR EACH TCode WHERE TCode.TC_EstRel EQ INTEGER(RActivas:SCREEN-VALUE) NO-LOCK INDEXED-REPOSITION.
       IF Br_Codeudores:NUM-SELECTED-ROWS IN FRAME F_Codeudores NE 0 THEN
         ASSIGN W_NitCodeudor:SCREEN-VALUE = TCode.TC_NitCode
                W_NomCodeudor:SCREEN-VALUE = TCode.TC_NomCode.
       ELSE
         ASSIGN W_NitCodeudor:SCREEN-VALUE = ""
                W_NomCodeudor:SCREEN-VALUE = "".

    END.
  END.
END.
*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Instancias
&Scoped-define SELF-NAME Btn_AgregarTXT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_AgregarTXT wWin
ON CHOOSE OF Btn_AgregarTXT IN FRAME F_Instancias /* Agregar Texto */
DO:
  Id_Agregar = "IN".
  E_Agregar:SCREEN-VALUE IN FRAME F_Agregar = "".
  ENABLE ALL WITH FRAME F_Agregar.
  VIEW FRAME F_Agregar.
  APPLY "entry" TO E_Agregar IN FRAME F_Agregar.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Admisible
&Scoped-define SELF-NAME Btn_CanAdm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_CanAdm wWin
ON CHOOSE OF Btn_CanAdm IN FRAME F_Admisible /* Cancelar */
DO:
  RUN Mostrar_Admisible.
  DISABLE Btn_SalAdm WITH FRAME F_Admisible.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Codeudores
&Scoped-define SELF-NAME Btn_CanCod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_CanCod wWin
ON CHOOSE OF Btn_CanCod IN FRAME F_Codeudores /* Cancelar */
DO:
  DISABLE W_NitCodeudor Btn_SalCod Btn_CanCod WITH FRAME F_Codeudores.
  ENABLE Btn_CreCod WITH FRAME F_Codeudores.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Admisible
&Scoped-define SELF-NAME Btn_ConAdm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_ConAdm wWin
ON CHOOSE OF Btn_ConAdm IN FRAME F_Admisible /* Button 160 */
DO:
  ASSIGN R_ConAdm:SCREEN-VALUE IN FRAME F_ConAdmisible = "1"
         R_ConAdm.
  
  RUN Consul_Gtias.

  ASSIGN FRAME F_Admisible:SENSITIVE = FALSE.
  VIEW FRAME F_ConAdmisible.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Creditos
&Scoped-define SELF-NAME Btn_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Consulta wWin
ON CHOOSE OF Btn_Consulta IN FRAME F_Creditos /* Button 10 */
DO:
  APPLY "value-changed" TO Cmb_Instancias IN FRAME F_Creditos.
  ENABLE ALL WITH FRAME F_Consulta.
  FRAME F_Consulta:HIDDEN = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Codeudores
&Scoped-define SELF-NAME Btn_CreCod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_CreCod wWin
ON CHOOSE OF Btn_CreCod IN FRAME F_Codeudores /* Crear */
DO:
    /* harold por precuacion
   RETURN. 

DO WITH FRAME F_Codeudores:
   W_NvoCD = YES.

   RELEASE Relaciones.

   ENABLE Btn_SalCod Btn_CanCod W_NitCodeudor.

   DISABLE Btn_CreCod.
   ASSIGN W_NitCodeudor:SCREEN-VALUE = ""
          W_NomCodeudor:SCREEN-VALUE = ""
          Relaciones.Aprobada:SCREEN-VALUE = "No".
   APPLY "entry" TO W_NitCodeudor.

   RETURN NO-APPLY.
END.    */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Instancias
&Scoped-define SELF-NAME Btn_GraInstancia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_GraInstancia wWin
ON CHOOSE OF Btn_GraInstancia IN FRAME F_Instancias /* Grabar */
DO:
    IF Mov_Instancias.Descripcion:SCREEN-VALUE IN FRAME F_Instancias = "" THEN DO:
        MESSAGE "No se puede grabar si no se ha entrado" SKIP
                "el concepto de la instancia. Entre el Concepto!"
            VIEW-AS ALERT-BOX ERROR.

        APPLY "entry" TO Mov_Instancias.Descripcion IN FRAME F_Instancias.
        RETURN NO-APPLY.
    END.

    FIND FIRST Mov_Instancias WHERE Mov_Instancias.Instancia = INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5))
                                AND Mov_Instancias.Nit = Creditos.Nit
                                AND INTEGER(Mov_Instancias.Cuenta) = Creditos.Num_Credito
                                AND Mov_Instancias.Num_Solicitud = Creditos.Num_Solicitud
                                AND Mov_Instancias.Estado = NO NO-ERROR.
    IF AVAILABLE Mov_Instancias THEN DO:
        ASSIGN FRAME F_Instancias
            Mov_Instancias.Descripcion.

        FIND TCerradas WHERE TCerradas.Instancia = INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5))
                         AND TCerradas.Num_Solicitud = INTEGER(Creditos.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud) NO-ERROR.
        IF AVAILABLE TCerradas THEN
            TCerradas.Descripcion = Mov_Instancias.Descripcion:SCREEN-VALUE IN FRAME F_Instancias.

        ASSIGN Mov_Instancias.Fec_Retiro = ?
               Mov_Instancias.Hora_Retiro = 0
               Creditos.Tasa = DECIMAL(W_TasaNominal:SCREEN-VALUE)
               Creditos.Cuota = DECIMAL(Creditos.Cuota:SCREEN-VALUE).

        IF Mov_Instancias.Estado:SCREEN-VALUE IN FRAME F_Instancias = "YES" THEN DO:
            FIND FIRST Hoja_Vida WHERE Hoja_Vida.Tipo = 9
                                   AND Hoja_Vida.Codigo = 1
                                   AND Hoja_Vida.Instancia = INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5))
                                   AND Hoja_Vida.DoctoRefer = INTEGER(Creditos.Num_Credito:SCREEN-VALUE IN FRAME F_Solicitud)
                                   AND Hoja_Vida.Nit = Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud
                                   AND Hoja_Vida.Asunto_Cumplido = NO
                                   AND Hoja_Vida.Usuario = W_Usuario NO-LOCK NO-ERROR.
            IF AVAILABLE Hoja_Vida THEN DO:
                Mov_Instancias.Estado = NO.

                MESSAGE "El crédito aun tiene Asuntos pendientes por resolver" SKIP
                        "en la hoja de vida. No se permite pasar a la siguiente" SKIP
                        "instancia si estos asuntos no se han cumplido." SKIP(1)
                        "Desea ver los asuntos por cumplir del crédito?"
                    VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE choice.

                HIDE FRAME F_Instancias.
                VIEW FRAME F_Solicitud.
                ENABLE ALL WITH FRAME F_Creditos.

                DISABLE NomUsuario WITH FRAME F_Creditos.

                IF choice THEN DO:
                    APPLY "choose" TO Btn_HojaVida IN FRAME F_Solicitud.
                    APPLY "entry" TO Hoja_Vida.Observacion IN FRAME F_HojaVida.
                    RETURN NO-APPLY.
                END.
                ELSE DO:
                    APPLY "entry" TO Btn_HojaVida IN FRAME F_Solicitud.
                    RETURN NO-APPLY.
                END.
            END.
            ELSE DO:
                HIDE FRAME F_Instancias.

                IF W_Ultima <> INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) THEN DO:
                    IF w_primera = INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) THEN DO:
                        ASSIGN FRAME F_Creditos:SENSITIVE = FALSE
                               FRAME F_Formalizar:VISIBLE = TRUE
                               Creditos.Estado:SCREEN-VALUE IN FRAME F_Formalizar = "5"
                               Creditos.Pagare:SCREEN-VALUE  = ""
                               Creditos.Pagare:SENSITIVE = FALSE
                               w_fecini:SENSITIVE = FALSE
                               w_fecini:SCREEN-VALUE IN FRAME f_formalizar = STRING(solicitud.fec_primerPago)
                               W_NetoDesemb = W_VrADesemb
                               W_NetoAD = W_VrADesemb
                               W_NetoAD:SCREEN-VALUE = STRING(W_VrADesemb).

                        /*IF Pro_Creditos.Id_PerGracia AND Pro_Creditos.Dia_Gracia > 0 THEN
                            W_FecIni:SCREEN-VALUE  = STRING(W_Fecha + Pro_Creditos.Dia_Gracia).*/

                        APPLY "Entry" TO Creditos.Estado.
                        RETURN NO-APPLY.
                    END.
                    ELSE DO:
                        IF Mov_Instancias.Descripcion:SCREEN-VALUE IN FRAME F_Instancias = "" THEN DO:
                            MESSAGE "No se puede grabar si no se ha entrado" SKIP
                                    "el concepto de la instancia. Entre el Concepto!"
                                VIEW-AS ALERT-BOX ERROR.

                            APPLY "entry" TO Mov_Instancias.Descripcion IN FRAME F_Instancias.
                            RETURN NO-APPLY.
                        END.

                        MESSAGE "                  Seguro de Procesar la Instancia...?"
                            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "CONFIRMAR SALVAR" UPDATE W_SiNos AS LOGICAL.

                        IF NOT W_SiNoS THEN
                            RETURN.

                        RUN Asignar_ProxInstancia.
                        IF ERROR-STATUS:ERROR THEN DO:
                            UNDO.
                        END.

                        APPLY "choose" TO Btn_Consulta IN FRAME F_Creditos.

                        FOR EACH Mov_Instancias WHERE Mov_Instancias.Instancia = INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5))
                                                  AND Mov_Instancias.Nit = STRING(Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud)
                                                  AND Mov_Instancias.Num_Solicitud = INTEGER(Creditos.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud)
                                                  AND Mov_Instancias.Usuario = W_Usuario:
                            ASSIGN Mov_Instancias.Fec_Retiro = W_Fecha
                                   Mov_Instancias.Hora_Retiro = TIME
                                   Mov_Instancias.Estado = YES
                                   Mov_Instancias.Agencia = INTEGER(SUBSTRING(Cmb_Agencias:SCREEN-VALUE IN FRAME F_Solicitud,1,3)).
                        END.

                        FIND CURRENT Mov_Instancias NO-LOCK NO-ERROR.

                        APPLY "value-changed" TO Cmb_Instancias IN FRAME F_Creditos.
                        RELEASE Creditos.
                    END.
                END.
                ELSE DO:
                    ASSIGN Creditos.Estado:SCREEN-VALUE IN FRAME F_Ultima = "2"
                           Creditos.Pagare:SCREEN-VALUE = ""
                           Creditos.Pagare:SENSITIVE = NO
                           W_NetoDesemb = W_VrADesemb
                           W_NetoDesemb:SCREEN-VALUE = STRING(W_NetoDesemb)
                           W_NetoD = W_VrADesemb
                           W_NetoD:SCREEN-VALUE = STRING(W_NetoDesemb)
                           /*W_FecIni:SCREEN-VALUE = STRING(finicio)*/
                           w_fecini:SCREEN-VALUE IN FRAME f_formalizar = STRING(solicitud.fec_primerPago)
                           W_CtaBanco = "".

                    ENABLE ALL EXCEPT Creditos.Pagare Creditos.Estado w_FecIni WITH FRAME F_Ultima.

                    ASSIGN FRAME F_Bancos:SENSITIVE = FALSE
                        Nom_Beneficiario:SCREEN-VALUE = ""
                        Nom_Beneficiario = ""
                        Num_Cheque:SCREEN-VALUE = ""
                        Num_Cheque = ""
                        FRAME F_Ultima:VISIBLE = TRUE
                        W_CtaTerceros:SENSITIVE = FALSE
                        NomCta:SENSITIVE = FALSE
                        W_VrEfectivo = 0
                        W_VrEfectivo:SCREEN-VALUE = "0"
                        W_VrEfectivo:SENSITIVE = FALSE
                        Creditos.Estado:SENSITIVE = FALSE.

                    ASSIGN W_VrCheque = 0
                           W_VrCheque:SCREEN-VALUE = "0"
                           W_VrCheque:SENSITIVE = FALSE
                           W_VrAlAhorro = 0
                           W_VrAlAhorro:SCREEN-VALUE = "0"
                           W_VrAlAhorro:SENSITIVE = FALSE
                           W_NetoDesemb:SENSITIVE = FALSE
                           W_NetoD:VISIBLE = FALSE
                           Creditos.Desembolso:SCREEN-VALUE = "8"
                           Creditos.Monto:SENSITIVE = FALSE
                           W_IntAntic:SENSITIVE = FALSE
                           W_IntAntic = 0
                           W_IntAntic:SCREEN-VALUE = "0"
                           Creditos.Monto:SCREEN-VALUE = STRING(Creditos.Monto)
                           NomAsoc:SCREEN-VALUE = NomNit:SCREEN-VALUE IN FRAME F_Solicitud.

                    FIND FIRST PlanPagos WHERE PlanPagos.Agencia = Creditos.Agencia
                                           AND PlanPagos.Nit = Creditos.Nit
                                           AND PlanPagos.Cod_Credito = Creditos.Cod_Credito
                                           AND PlanPagos.Num_Credito = Creditos.Num_Credito
                                           AND PlanPagos.Id_PdoMes = 1 NO-LOCK NO-ERROR.

                    Creditos.Desembolso:DISABLE("Efectivo y Cheque").
                    Creditos.Desembolso:DISABLE("Efectivo y Cta-Ahorros").
                    Creditos.Desembolso:DISABLE("Cheque y Cta-Ahorros").

                    APPLY "Entry" TO Btn_SalvaUltima IN FRAME F_Ultima.
                    RETURN NO-APPLY.
                END.
            END.
        END.

        FIND CURRENT Mov_Instancias NO-LOCK NO-ERROR.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Solicitud
&Scoped-define SELF-NAME Btn_Historia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Historia wWin
ON CHOOSE OF Btn_Historia IN FRAME F_Solicitud /* Historial */
DO:
  ASSIGN WWin:SENSITIVE = FALSE.

  RUN W-Hist_Creditos.r (INPUT Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud,9999999).
  
  ASSIGN WWin:SENSITIVE = TRUE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_HojaVida
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_HojaVida wWin
ON CHOOSE OF Btn_HojaVida IN FRAME F_Solicitud /* Pendientes */
DO:
  DISABLE ALL WITH FRAME F_Creditos.
  FIND FIRST Hoja_Vida WHERE 
       Hoja_Vida.Tipo EQ 9 AND Hoja_Vida.Codigo EQ 1 AND 
       Hoja_Vida.Instancia  EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) AND
       Hoja_Vida.DoctoRefer EQ INTEGER(Creditos.Num_Credito:SCREEN-VALUE IN FRAME F_Solicitud) AND
       Hoja_Vida.Nit        EQ Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud AND
       Hoja_Vida.Asunto_Cumplido EQ NO AND
       Hoja_Vida.Usuario   EQ W_Usuario NO-ERROR.
DO WITH FRAME F_HojaVida:
  ENABLE ALL WITH FRAME F_HojaVida.
  DISABLE Hoja_Vida.Fec_Grabacion Btn_SalvaHV WITH FRAME F_HojaVida.
  IF AVAILABLE Hoja_Vida THEN DO:
     ASSIGN Hoja_Vida.Asunto_Cumplido:SCREEN-VALUE = STRING(Hoja_Vida.Asunto_Cumplido)
            Hoja_Vida.Observacion:SCREEN-VALUE     = Hoja_Vida.Observacion
            Hoja_Vida.Fec_Grabacion:SCREEN-VALUE   = STRING(Hoja_Vida.Fec_Grabacion).
     ENABLE Hoja_Vida.Asunto_Cumplido Hoja_Vida.Observacion.
  END.
  ELSE DO:
     ASSIGN Hoja_Vida.Asunto_Cumplido:SCREEN-VALUE = "no"
            Hoja_Vida.Observacion:SCREEN-VALUE     = ""
            Hoja_Vida.Fec_Grabacion:SCREEN-VALUE   = "".
     DISABLE Hoja_Vida.Asunto_Cumplido Hoja_Vida.Observacion.
  END.
END.
VIEW FRAME F_HojaVida.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Bancos
&Scoped-define SELF-NAME Btn_ImpChe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_ImpChe wWin
ON CHOOSE OF Btn_ImpChe IN FRAME F_Bancos /* Button 162 */
DO:
  /*ASSIGN FRAME F_Bancos Num_Cheque Nom_Beneficiario.
  IF Bancos:NUM-SELECTED-ROWS NE 0 AND Cuentas.Cuenta NE "?" THEN DO:
     ASSIGN W_CtaBanco = Cuentas.Cuenta.
     APPLY "choose" TO Btn_SalvaUltima IN FRAME F_Ultima.
  END.
  ELSE DO:
     MESSAGE "Aunque el desembolso se debe hacer por cheque" SKIP
             "no se ha escogido un banco o digitado el número" SKIP
             "de cheque o digitado el nombre del beneficiario" SKIP(1)
             "Se cancela la operación de desembolso" VIEW-AS ALERT-BOX ERROR.
     ASSIGN Creditos.Pagare:SCREEN-VALUE IN FRAME F_Ultima = ""
            Creditos.Pagare:SENSITIVE IN FRAME F_Ultima = NO
            Creditos.Estado:SCREEN-VALUE IN FRAME F_Ultima = "1".
  END.
  HIDE FRAME F_Bancos.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Solicitud
&Scoped-define SELF-NAME Btn_ImpPagare
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_ImpPagare wWin
ON CHOOSE OF Btn_ImpPagare IN FRAME F_Solicitud /* Lib / Pagaré */
DO:
  /*Impresion del pagare*/ 
  IF NOT AVAIL(Creditos) THEN
     RETURN.

  IF W-FecEntLib  = ? OR STRING(W-FecEntLib) LE "" THEN DO:
     MESSAGE "Debe de colocar la fecha de desembolso "
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     APPLY "leave" TO W-FecEntLib.
     RETURN NO-APPLY.
  END.

/*  IF creditos.FOR_pago NE 2 THEN DO: 
    MESSAGE "Sólo es válido para pagos por Nómina - Libranzas"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN. /* solo se trabaja libranza  */
  END. */
  ASSIGN W-FecEntLib.
  ASSIGN W_FecIni  = W-FecEntLib
         W_FecIni:SCREEN-VALUE IN FRAME f_ultima = W-FecEntLib:SCREEN-VALUE IN FRAME f_solicitud.
  IF creditos.FOR_pago EQ 2 THEN DO:
      MESSAGE "...... Desea imprimir LIBRANZA ?" SKIP
              VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE choice.
      IF choice THEN
         RUN f-libranza.r (INPUT Creditos.Nit:SCREEN-VALUE, DECIMAL(Creditos.Num_Credito:SCREEN-VALUE), 1, W-FecEntLib) NO-ERROR.
  END.

  MESSAGE "...... Desea imprimir PAGARE ?" SKIP
          VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE choice.
  IF choice THEN
    RUN F-PagPreCoop.r ( INPUT Creditos.Nit:SCREEN-VALUE, DECIMAL(Creditos.Num_Credito:SCREEN-VALUE),  W-FecEntLib ).
 

 
  /*   para habilitar una vez ok 
  FIND Pro_Creditos WHERE 
       Pro_Creditos.Cod_Credito   EQ Creditos.Cod_Credito AND
       Pro_Creditos.Tip_Credito   EQ Creditos.Tip_Credito AND 
       Pro_Creditos.Id_Formato    EQ YES AND
       Pro_Creditos.Cod_ForPagare NE 0 NO-LOCK NO-ERROR.
  IF AVAILABLE Pro_Credito THEN DO:
     FIND Formatos WHERE Formatos.Agencia     EQ Creditos.Agencia
                     AND Formatos.Cod_Formato EQ Pro_Creditos.Cod_ForPagare  NO-LOCK NO-ERROR.
     IF AVAILABLE(Formatos) THEN DO:
        IF Formatos.Nom_Proceso EQ "" THEN DO:
           MESSAGE "EL formato encontrado no tiene asignado un programa" SKIP
                   "ejecutable. verifique en la tabla de formatos, el" SKIP
                   "nombre del proceso para la Agencia: " Creditos.Agencia SKIP
                   "El formato: " Formatos.Cod_Formato SKIP(1)
                   "Al momento este campo no tiene ningún nombre de ejecutable digitado" VIEW-AS ALERT-BOX ERROR.
           RETURN.
        END.

        RUN VALUE(Formatos.Nom_Proceso) (INPUT Creditos.Nit, INPUT Creditos.Num_Credito) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
           MESSAGE "Error al llamar la rutina de impresion de pagare" SKIP
                  "consulte con el administrador!" VIEW-AS ALERT-BOX.
           RETURN.
        END.
     END.
     ELSE DO:
        MESSAGE "No se encuentra el formato de Impresión de pagarè" SKIP
                "consulte con el administrador!" VIEW-AS ALERT-BOX.
        RETURN.
     END.
  END. */


    /*impresion del pagare, quedò en el Btn_ImpPagare pero del programa w-desembolso2.w */
  /*MESSAGE "Imprimir Pagare / Libranza  (Si) o  (No)" 
       VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE 'Eleccion de Impresiòn' UPDATE Imp_Pag AS LOGICAL.
    /* Para la Libranza se imprime tanto el documento adjunto como el Pagare 14 marzo/05 6 pm */
  IF Imp_pag THEN DO: 
    IF Creditos.For_Pago EQ 2 THEN 
       RUN f-libranzaj.r (INPUT "LIBRANZA", Creditos.Nit , Creditos.num_credito) NO-ERROR.

    RUN f-pagarej.r (INPUT "PAGARE", Creditos.Nit , Creditos.num_credito) NO-ERROR.
  END.*/
  APPLY "ENTRY" TO Cmb_Instancias IN FRAME F_Creditos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Creditos
&Scoped-define SELF-NAME Btn_Imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imprimir wWin
ON CHOOSE OF Btn_Imprimir IN FRAME F_Creditos /* Button 8 */
DO:
/*   DEFINE VAR Listado     AS CHARACTER INITIAL "". */
/*   Listado = W_PathSpl + W_Usuario + "Proyec.LST". */
/*   {INCLUIDO\Imprimir.I "Listado"}                 */

    RUN Informacion_Credito.

    RUN wimprime.w ("prProcesoDesembolso.p", 
                    "Proceso Desembolso",
                   creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud, 
                   "",
                   "",
                           "",
                   "",
                           "",
                   "",
                           "",
                   "",
                           "",
                   "",
                           "",
                           "",
                           "",
                           "",
                           "",
                           "",
                           "",
                           "",
                           "",
                   ?,
                           ?,
                   ?,
                           ?,
                   ?,
                           ?,
                   ?,
                           ?,
                           no,
                           no,
                           no,
                           no).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Admisible
&Scoped-define SELF-NAME Btn_InaAdm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_InaAdm wWin
ON CHOOSE OF Btn_InaAdm IN FRAME F_Admisible /* Inactivar */
DO:
DO WITH FRAME F_Admisible:
 FIND CURRENT Garantias EXCLUSIVE-LOCK.
 IF SELF:LABEL EQ "Inactivar" THEN DO:
   ASSIGN Garantias.Estado:SCREEN-VALUE = "2"
          Garantias.Fec_Retiro:SCREEN-VALUE = STRING(W_Fecha).
   MESSAGE "Confirma la Inactivación de la Garantía?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
         UPDATE choice.
   IF choice THEN
     ASSIGN Garantias.Estado = 2
            Garantias.Fec_Retiro = W_Fecha.
   ELSE
     ASSIGN Garantias.Estado:SCREEN-VALUE = "1"
            Garantias.Fec_Retiro:SCREEN-VALUE = "?".
 END.
 ELSE DO:
   ASSIGN Garantias.Estado:SCREEN-VALUE = "1"
          Garantias.Fec_Retiro:SCREEN-VALUE = "?".
   MESSAGE "Confirma la Activación de la Garantía?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
         UPDATE choice.
   IF choice THEN
     ASSIGN Garantias.Estado = 1
            Garantias.Fec_Retiro = ?.
   ELSE
     ASSIGN Garantias.Estado:SCREEN-VALUE = "2"
            Garantias.Fec_Retiro:SCREEN-VALUE = STRING(Garantias.Fec_Retiro).
 END.
  
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_IngAdm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_IngAdm wWin
ON CHOOSE OF Btn_IngAdm IN FRAME F_Admisible /* Ingresar */
DO:
DO WITH FRAME F_Admisible:
  DISABLE Btn_IngAdm WITH FRAME F_Amisible.
  ENABLE {&List-6} Btn_SalAdm WITH FRAME F_Admisible.
  DISABLE Nom_Aseguradora Nom_UsuGarantia WITH FRAME F_Admisible.
  RUN Inicializar_Admisible.
  
  ASSIGN W_NvaAdm                  = YES
         W_CredAval:SCREEN-VALUE   = "0"
         W_DispGaran:SCREEN-VALUE  = "0"
         W_CredAval                = 0
         W_DispGaran               = 0.

  APPLY "entry" TO Garantias.Identificacion_Bien.
  RETURN NO-APPLY.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Instancias
&Scoped-define SELF-NAME Btn_insVolver
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_insVolver wWin
ON CHOOSE OF Btn_insVolver IN FRAME F_Instancias /* Button 135 */
DO:
  ENABLE ALL WITH FRAME F_Creditos.
  DISABLE NomUsuario WITH FRAME F_Creditos.
  HIDE FRAME F_Instancias.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_HojaVida
&Scoped-define SELF-NAME Btn_NvoHv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_NvoHv wWin
ON CHOOSE OF Btn_NvoHv IN FRAME F_HojaVida /* Ingresar */
DO:
  ENABLE Btn_SalvaHV Hoja_Vida.Asunto_Cumplido Hoja_Vida.Observacion WITH FRAME F_HojaVida.
  DISABLE Btn_NvoHV WITH FRAME F_HojaVida.
  ASSIGN Hoja_Vida.Asunto_Cumplido:SCREEN-VALUE = "no"
         Hoja_Vida.Observacion:SCREEN-VALUE     = ""
         Hoja_Vida.Fec_Grabacion:SCREEN-VALUE   = STRING(TODAY).
  HIDE FRAME F_ConHV.
  W_NvaHV = YES.
  Hoja_Vida.Observacion:READ-ONLY = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Cerradas
&Scoped-define SELF-NAME Btn_OutCerradas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OutCerradas wWin
ON CHOOSE OF Btn_OutCerradas IN FRAME F_Cerradas /* Button 143 */
DO:
  IF AVAILABLE TCerradas THEN DO:
     FIND Mov_Instancias WHERE 
          Mov_Instancias.Instancia EQ TCerradas.Instancia AND
          Mov_Instancias.Num_Solicitud EQ TCerradas.Num_Solicitud AND
          Mov_Instancias.Usuario EQ TCerradas.Usuario NO-LOCK NO-ERROR.
     IF AVAILABLE Mov_Instancias THEN DO:
       ASSIGN Mov_Instancias.Estado:SCREEN-VALUE IN FRAME F_Instancias = STRING(Mov_Instancias.Estado)
              Mov_Instancias.Fec_Ingreso:SCREEN-VALUE = STRING(Mov_Instancias.Fec_Ingreso)
              WHora_Ingreso:SCREEN-VALUE = STRING(Mov_Instancia.Hora_Ingreso,"HH:MM:SS AM")
              Mov_Instancia.Descripcion:SCREEN-VALUE = Mov_Instancia.Descripcion
              Mov_Instancias.Fec_Retiro:SCREEN-VALUE = ""
              W_instancia:SCREEN-VALUE = Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos
              W_UsuarioInstancia:SCREEN-VALUE = NomUsuario:SCREEN-VALUE IN FRAME F_Creditos.
       Vigencia:SCREEN-VALUE = STRING(TODAY - TCerradas.Fec_Ingreso) + " Dias".
       IF Mov_Instancias.Estado EQ YES THEN 
         DISABLE Mov_Instancias.Estado Mov_Instancias.Descripcion Btn_GraInstancia WITH FRAME F_Instancias.
       ELSE
         ENABLE Mov_Instancias.Estado Mov_Instancias.Descripcion Btn_GraInstancia WITH FRAME F_Instancias.
     END.
          
  END.
  HIDE FRAME F_Cerradas.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_ConAdmisible
&Scoped-define SELF-NAME Btn_OutConAdm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OutConAdm wWin
ON CHOOSE OF Btn_OutConAdm IN FRAME F_ConAdmisible /* Button 163 */
DO:
  HIDE FRAME F_ConAdmisible.
  ASSIGN FRAME F_Admisible:SENSITIVE = TRUE.

  IF Br_Admisible:NUM-SELECTED-ROWS GT 0 THEN DO:
     RUN Mostrar_Admisible.

     ASSIGN R_TipoGarantia:SCREEN-VALUE IN FRAME F_Garantias = "3"
            R_TipoGarantia
            W_NvaAdm                                         = FALSE.

     IF Garantias.Tipo_Garantia LE 3 THEN
        ASSIGN R_TipoGarantia:SCREEN-VALUE IN FRAME F_Garantias = "2"
               R_TipoGarantia.

     APPLY "Value-Changed" TO R_TipoGarantia.

     IF Garantias.Estado EQ 1 THEN
        Btn_InaAdm:LABEL IN FRAME F_Admisible = "Inactivar".
     ELSE
        Btn_InaAdm:LABEL IN FRAME F_Admisible = "Activar".
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_ConHV
&Scoped-define SELF-NAME Btn_OutConHV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OutConHV wWin
ON CHOOSE OF Btn_OutConHV IN FRAME F_ConHV /* Button 153 */
DO:
DO WITH FRAME F_HojaVida:
  IF AVAILABLE Hoja_Vida THEN DO:
     ASSIGN Hoja_Vida.Asunto_Cumplido:SCREEN-VALUE = STRING(Hoja_Vida.Asunto_Cumplido)
            Hoja_Vida.Observacion:SCREEN-VALUE     = Hoja_Vida.Observacion
            Hoja_Vida.Fec_Grabacion:SCREEN-VALUE   = STRING(Hoja_Vida.Fec_Grabacion).
  END.
  HIDE FRAME F_ConHV.
  IF Hoja_Vida.Asunto_Cumplido THEN
     DISABLE Hoja_Vida.Observacion WITH FRAME F_HojaVida.
  ELSE
     ENABLE Hoja_Vida.Observacion WITH FRAME F_HojaVida.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME Btn_OutConsulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OutConsulta wWin
ON CHOOSE OF Btn_OutConsulta IN FRAME F_Consulta /* Button 133 */
DO:
    IF AVAILABLE Consulta THEN DO:
        RELEASE Solicitud.

        FIND FIRST Creditos WHERE Creditos.Num_Credito = Consulta.Num_Credito
                              AND Creditos.Nit = Consulta.Nit.
        IF LOCKED Creditos THEN DO:
            MESSAGE "El Crédito esta siendo accesada por otro" SKIP
                    "asesor. Por este motivo no podra ser modificada"
                VIEW-AS ALERT-BOX INFORMATION.

            APPLY "choose" TO Btn_Consulta IN FRAME F_Creditos.
        END.
        ELSE DO:
            IF AVAILABLE Creditos THEN DO:
                FIND FIRST Solicitud WHERE Solicitud.Agencia = Creditos.Agencia
                                       AND Solicitud.Cod_Credito = Creditos.Cod_Credito
                                       AND Solicitud.Num_Solicitud = Creditos.Num_Solicitud
                                       AND Solicitud.Estado = 2
                                       AND Solicitud.Nit = Creditos.Nit NO-ERROR.
                IF NOT AVAILABLE(Solicitud) THEN DO:
                    MESSAGE "No se halló la solicitud aprobada correspondiente :"  Creditos.Num_Solicitud SKIP
                            "No se acepta la operacion"
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.

                    RETURN.
                END.

                RUN Mostrar_Credito NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                    RETURN.

                ENABLE Btn_Consulta
                       /*Btn_ProInstancia*/
                       Button-2
                    WITH FRAME F_Creditos.

                RUN Act_NumCre_Adm.
            END.
            ELSE DO:
                MESSAGE "Credito no disponible"
                    VIEW-AS ALERT-BOX ERROR.

                RETURN.
            END.
        END.

        HIDE FRAME F_Consulta.
        VIEW FRAME F_Solicitud.

        RUN Buscar_Indicadores.
    END.
    ELSE DO:
        IF INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) = 790 THEN DO:
            MESSAGE "No hay creditos seleccionada para trabajar o" SKIP
                    "el usuario no tiene solicitudes pendientes " SKIP
                    "por procesar en esta instancia."
                VIEW-AS ALERT-BOX WARNING.

            APPLY "choose" TO Btn_Consulta.
        END.
        ELSE DO:
            MESSAGE "No hay solicitud seleccionada para trabajar o" SKIP
                    "el usuario no tiene solicitudes pendientes " SKIP
                    "por procesar en esta instancia."
                VIEW-AS ALERT-BOX WARNING.
        END.
    END.

    IF W_Ultima = INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) THEN DO:
        DISABLE ALL WITH FRAME F_Creditos.
        ENABLE Btn_Consulta
               Btn_ProInstancia
               Button-2
            WITH FRAME F_Creditos.
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Garantias
&Scoped-define SELF-NAME Btn_OutGarantias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OutGarantias wWin
ON CHOOSE OF Btn_OutGarantias IN FRAME F_Garantias /* Button 136 */
DO:
  ENABLE ALL WITH FRAME F_Creditos.
  DISABLE NomUsuario WITH FRAME F_Creditos.
/*  IF INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) NE W_Primera THEN
     DISABLE Btn_Ingresar WITH FRAME F_Creditos.*/
  FIND Clientes WHERE ROWID(Clientes) EQ puntero NO-LOCK NO-ERROR.
  IF NOT AVAILABLE Clientes THEN MESSAGE "jouy".
  HIDE FRAME F_Garantias.
  FRAME F_Consulta:HIDDEN = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_InfoProducto
&Scoped-define SELF-NAME Btn_OutScoring
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OutScoring wWin
ON CHOOSE OF Btn_OutScoring IN FRAME F_InfoProducto /* Button 121 */
DO:
  ENABLE ALL WITH FRAME F_Creditos.
  DISABLE NomUsuario WITH FRAME F_Creditos.
  HIDE FRAME F_InfoProducto.
  FRAME F_Consulta:HIDDEN = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ultima
&Scoped-define SELF-NAME Btn_OutUltima
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OutUltima wWin
ON CHOOSE OF Btn_OutUltima IN FRAME F_Ultima /* Button 153 */
DO:
  HIDE FRAME F_Ultima.
  HIDE FRAME F_Condicionada.
  ENABLE ALL WITH FRAME F_Creditos.
  DISABLE NomUsuario WITH FRAME F_Creditos.

  IF W_Ultima EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) THEN DO:
     DISABLE ALL WITH FRAME F_Creditos. 
     ENABLE Btn_Consulta Btn_ProInstancia Button-2 WITH FRAME F_Creditos. 
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Formalizar
&Scoped-define SELF-NAME Btn_OutUltima-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OutUltima-2 wWin
ON CHOOSE OF Btn_OutUltima-2 IN FRAME F_Formalizar /* Btn_outultima 2 */
DO:
  HIDE FRAME F_Formalizar.
  HIDE FRAME F_Condicionada.

  ENABLE ALL WITH FRAME F_Creditos.
  /*ASSIGN FRAME F_Creditos:SENSITIVE = TRUE.*/

  DISABLE NomUsuario WITH FRAME F_Creditos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Creditos
&Scoped-define SELF-NAME Btn_ProInstancia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_ProInstancia wWin
ON CHOOSE OF Btn_ProInstancia IN FRAME F_Creditos /* Procesar Instancia */
DO:
 
    IF W-fecEntLib EQ ? THEN DO:
       /* DISABLE Btn_ProInstancia.*/
        MESSAGE "favor digite la fecha del primer pago !" VIEW-AS ALERT-BOX INFO BUTTON OK.
         /*RETURN NO-APPLY.*/
        END.
      ELSE DO: 
           ptipo = 1.
          RUN periodos1.
        IF finicio >= TODAY THEN DO:           
               IF SwExiste EQ ?  THEN 
                 RETURN NO-APPLY.

          END.
        ELSE DO:
             DISABLE Btn_ProInstancia.
            MESSAGE "La Fecha de Inicio del Plan... menor a la de Hoy !"  "Fecha Inicio:" finicio "-  Fecha hoy :" TODAY  VIEW-AS ALERT-BOX INFO BUTTON OK.
            RETURN NO-APPLY.
        END.

      END.

    DEFI VAR W_RowidC AS ROWID.


 IF FRAME F_Solicitud:HIDDEN EQ YES OR Br_Consulta:NUM-ENTRIES IN FRAME F_Consulta EQ 0 THEN DO:
    MESSAGE "Al momento no existe ningun crédito al cual" SKIP
            "se le pueda procesar la instancia." VIEW-AS ALERT-BOX ERROR.
    APPLY "entry" TO Cmb_Instancias IN FRAME F_Creditos.
    RETURN NO-APPLY.
 END.

/*  IF W_VrADesemb LE 0 THEN DO:                                                                         */
/*     MESSAGE "El Valor a Desembolsar es Negativo o Cero(0), Debe Revisar los Valores a Cancelar." SKIP */
/*           VIEW-AS ALERT-BOX ERROR.                                                                    */
/*     RETURN NO-APPLY.                                                                                  */
/*  END.                                                                                                 */

 W_RowidC = ROWID(Clientes).
 IF AVAILABLE Creditos THEN FOR EACH Relaciones WHERE    
             Relaciones.Nit            EQ Creditos.Nit           AND
             INTEG(Relaciones.Cuenta)  EQ Creditos.Num_Solicitud AND
             Relaciones.Clase_Producto EQ 2                      AND
             Relaciones.Cod_Producto   EQ Creditos.Cod_Credito   AND
             Relaciones.Cod_Relacion   EQ 11                     AND
             Relaciones.Estado         EQ 1 NO-LOCK:
       FIND Clientes WHERE Clientes.Nit EQ Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
       IF AVAILABLE Clientes AND Clientes.Tip_Contrato EQ 5 THEN DO:
          FIND FIRST Ahorros WHERE Ahorros.Nit         EQ Relaciones.Nit_Relacion
                               AND Ahorros.Tip_Ahorro  EQ 4
                               AND Ahorros.Sdo_Disponi GT 0 NO-LOCK NO-ERROR.
          IF NOT AVAIL(Ahorros) THEN DO:
             MESSAGE "Codeudor Pensionado debe estar Afiliado a la Entidad..." SKIP
                     "No se permite procesar la instancia." VIEW-AS ALERT-BOX ERROR.
             APPLY "entry" TO Cmb_Instancias IN FRAME F_Creditos.
             RETURN NO-APPLY.
          END.
       END.
 END.

 FIND Clientes WHERE ROWID(Clientes) EQ W_RowidC NO-LOCK NO-ERROR.

 HIDE FRAME F_Consultas.
 DISABLE ALL WITH FRAME F_Creditos.
 FIND FIRST creditos WHERE creditos.nit = Creditos.nit:SCREEN-VALUE in FRAME f_solicitud AND 
                           creditos.num_credito = DECIMAL(creditos.num_credito:SCREEN-VALUE) AND 
                           creditos.num_solicitud   = DECIMAL(Creditos.num_solicitud:SCREEN-VALUE) no-error.
 /* MESSAGE "instancia " INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5))
         "nit   " Creditos.Nit           SKIP(0)                                             
         "#cred " Creditos.Num_Credito   SKIP(0)                                             
         "#solic" Creditos.Num_Solicitud   VIEW-AS ALERT-BOX INFO BUTTONS OK. */
 IF AVAILABLE Creditos THEN DO:
    FIND FIRST Mov_Instancias WHERE 
         Mov_Instancias.Instancia      EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) AND
         Mov_Instancias.Nit            EQ Creditos.Nit           AND
         INTEG(Mov_Instancias.Cuenta)  EQ Creditos.Num_Credito   AND
         Mov_Instancias.Num_Solicitud  EQ Creditos.Num_Solicitud AND
         Mov_Instancias.Estado         EQ NO NO-LOCK NO-ERROR.
    IF AVAILABLE Mov_Instancias THEN DO:
       DO WITH FRAME F_Instancias:
          ASSIGN Mov_Instancias.Estado:SCREEN-VALUE = STRING(Mov_Instancias.Estado)
                 Mov_Instancias.Fec_Ingreso:SCREEN-VALUE = STRING(Mov_Instancias.Fec_Ingreso)
                 WHora_Ingreso:SCREEN-VALUE = STRING(Mov_Instancia.Hora_Ingreso,"HH:MM:SS AM")
                 Mov_Instancia.Descripcion:SCREEN-VALUE = Mov_Instancia.Descripcion
                 Mov_Instancias.Fec_Retiro:SCREEN-VALUE = ""
                 W_instancia:SCREEN-VALUE = Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos
                 W_UsuarioInstancia:SCREEN-VALUE = NomUsuario:SCREEN-VALUE IN FRAME F_Creditos.

          Vigencia:SCREEN-VALUE = STRING(TODAY - Mov_Instancias.Fec_Ingreso) + " Dias".
          ENABLE Mov_Instancias.Estado Mov_Instancias.Descripcion
                 Btn_GraInstancia Btn_InsVolver WITH FRAME F_Instancias.
          RUN Buscar_Instancias_Cerradas.
       END.

       IF W_Ultima EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) THEN DO:
          ASSIGN Mov_Instancias.Descripcion:SCREEN-VALUE IN FRAME F_Instancias = "Desembolsa Usuario : "
                                                                                 + STRING(W_Usuario)
                 Mov_Instancias.Estado:SCREEN-VALUE = "YES".
          APPLY "CHOOSE" TO Btn_GraInstancia.
          RETURN NO-APPLY.
       END.
       ELSE DO:
          VIEW FRAME F_Instancias.
          APPLY "entry" TO Mov_Instancias.Estado IN FRAME F_Instancias.
          RETURN NO-APPLY.
       END.
    END.
    ELSE DO:
      MESSAGE "El Crédito no esta disponible" SKIP
              "o no se ha cerrado la instancia anterior" SKIP
              "escoja un Crédito de la lista de " SKIP
              "Creditos disponibles!" VIEW-AS ALERT-BOX WARNING.
      ENABLE ALL WITH FRAME F_Creditos.
      DISABLE NomUsuario WITH FRAME F_Creditos.
    END.
 END.
 ELSE
   MESSAGE "No esta disponible el Crèdito" VIEW-AS ALERT-BOX ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Solicitud
&Scoped-define SELF-NAME Btn_Proyectar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Proyectar wWin
ON CHOOSE OF Btn_Proyectar IN FRAME F_Solicitud /* Proyectar */
DO:
  W_TipoInforme = "Proyeccion".
  APPLY "choose" TO Btn_Imprimir IN FRAME F_Creditos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Admisible
&Scoped-define SELF-NAME Btn_SalAdm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_SalAdm wWin
ON CHOOSE OF Btn_SalAdm IN FRAME F_Admisible /* Salvar */
DO:
DEFINE VAR W_Contabilizada AS LOGICAL.
DO WITH FRAME F_Admisible:
  IF W_NvaAdm AND
     (Garantias.Identificacion_Bien:SCREEN-VALUE EQ "" OR
     Garantias.Nom_Bien:SCREEN-VALUE EQ "" OR
     Garantias.Val_Bien:SCREEN-VALUE EQ "0") THEN DO:
     MESSAGE "Para poder salvar una nueva Garantia " SKIP
             "debe digitarse al menos la siguiente información:" SKIP(1)
             "  - Identificación del Bien" SKIP
             "  - Nombre del Bien" SKIP
             "  - Valor del Bien" VIEW-AS ALERT-BOX ERROR.
     APPLY "entry" TO Garantias.Identificacion_Bien.
     RETURN NO-APPLY.
  END.

  IF INTEGER(Garantias.Tipo_Garantia:SCREEN-VALUE) LE 3 AND
     DATE(Garantias.Fec_FinSeguro:SCREEN-VALUE) EQ ? OR DATE(Garantias.Fec_FinSeguro:SCREEN-VALUE) LT W_fecha THEN DO:
     MESSAGE "Falta la fecha de vencimiento del seguro o Ya està vencida..." SKIP
             VIEW-AS ALERT-BOX WARNING.
     
     APPLY "entry" TO Garantias.Fec_FinSeguro.
  END.

  ENABLE Btn_IngAdm WITH FRAME F_Admisible.
  PuntGar = ROWID(Garantias).
  FOR EACH Garantias WHERE 
           Garantias.Identificacion_Bien EQ Garantias.Identificacion_Bien:SCREEN-VALUE NO-LOCK:
    IF Garantias.Contabilizada THEN
       W_Contabilizada = YES.
  END.

  IF W_NvaAdm THEN 
     CREATE Garantias.
  ELSE FIND Garantias WHERE ROWID(Garantias) EQ PuntGar EXCLUSIVE-LOCK NO-ERROR.

  ASSIGN Garantias.Agencia             = Creditos.Agencia
         Garantias.Tip_Credito         = Creditos.Tip_Credito
         Garantias.Cod_Credito         = INTEGER(SUBSTRING(Nom_Producto:SCREEN-VALUE IN FRAME F_Solicitud,1,3))
         Garantias.Nit                 = Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud
         Garantias.Num_Solicitud       = Creditos.Num_Solicitud
         Garantias.Num_Credito         = Creditos.Num_Credito
         Garantias.Pagare              = STRING(Creditos.Num_Credito)
         Garantias.Tipo_Garantia       = INTEGER(Garantias.Tipo_Garantia:SCREEN-VALUE)
         Garantias.Identificacion_Bien = Garantias.Identificacion_Bien:SCREEN-VALUE
         Garantias.Nom_Bien            = Garantias.Nom_Bien:SCREEN-VALUE
         Garantias.Descripcion_Bien    = Garantias.Descripcion_Bien:SCREEN-VALUE
         Garantias.Fec_Creacion        = DATE(Garantias.Fec_Creacion:SCREEN-VALUE)
         Garantias.Fec_Retiro          = DATE(Garantias.Fec_Retiro:SCREEN-VALUE).

  ASSIGN Garantias.Nit_Aseguradora     = Garantias.Nit_Aseguradora:SCREEN-VALUE
         Garantias.Nro_Seguro          = Garantias.Nro_Seguro:SCREEN-VALUE
         Garantias.Fec_IniSeguro       = DATE(Garantias.Fec_IniSeguro:SCREEN-VALUE)
         Garantias.Fec_FinSeguro       = DATE(Garantias.Fec_FinSeguro:SCREEN-VALUE)
         Garantias.Val_Asegurado       = DECIMAL(Garantias.Val_Asegurado:SCREEN-VALUE)
         Garantias.Usuario             = W_Usuario
         Garantias.Val_Bien            = DECIMAL(Garantias.Val_Bien:SCREEN-VALUE)
         Garantias.Nom_Impuesto        = Nom_Impuesto:SCREEN-VALUE
         Garantias.Fec_VctoImpuesto    = DATE(Garantias.Fec_VctoImpuesto:SCREEN-VALUE)
         Garantias.Val_Impuesto        = DECIMAL(Garantias.Val_Impuesto:SCREEN-VALUE)
         Garantias.Fec_UltAvaluo       = DATE(Garantias.Fec_UltAvaluo:SCREEN-VALUE)
         Garantias.Fec_ProxAvaluo      = DATE(Garantias.Fec_ProxAvaluo:SCREEN-VALUE)
         Garantias.Val_UltAvaluo       = DECIMAL(Garantias.Val_UltAvaluo:SCREEN-VALUE)
         Garantias.Descripcion_Bien2   = Garantias.Descripcion_Bien2:SCREEN-VALUE
         Garantias.Aprobada.

  IF Garantias.Tipo_Garantia EQ 4 THEN DO:
     FIND FIRST Ahorros WHERE (Ahorros.Tip_Ahorro EQ 2 OR Ahorros.Tip_Ahorro EQ 3)          AND
                        Ahorros.Nit                            EQ Garantias.Nit_Aseguradora AND
                        Ahorros.Cue_Ahorros                    EQ Garantias.Identificacion_Bien AND
                        Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje GT 0 NO-LOCK NO-ERROR.
     ASSIGN Garantias.Cod_AhorroNA = Ahorros.Cod_Ahorro WHEN AVAIL(Ahorros).
     ASSIGN Garantias.Id_Interna   = TRUE WHEN AVAIL(Ahorros).
  END.
         
  IF W_Contabilizada THEN 
     Garantias.Contabilizada = YES.

  RUN Halla_DispGaran.

  W_NvaAdm = NO.
  FIND CURRENT Garantias NO-LOCK NO-ERROR.
END.
W_Contabilizada = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Codeudores
&Scoped-define SELF-NAME Btn_SalCod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_SalCod wWin
ON CHOOSE OF Btn_SalCod IN FRAME F_Codeudores /* Salvar */
DO:
    /* harold por mensajes de precaucion
    RETURN.  

   DO WITH FRAME F_Codeudores:
 
    IF W_NitCodeudor:SCREEN-VALUE EQ "" THEN DO:                                                               
       MESSAGE "El nit a relacionar no puede ser blancos" SKIP                                                 
               "Digite el nit a relacionar." VIEW-AS ALERT-BOX WARNING.                                        
       APPLY "entry" TO W_NitCodeudor.                                                                         
       RETURN NO-APPLY.                                                                                        
    END.

    FIND FIRST Relaciones WHERE 
        Relaciones.Nit          EQ Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud AND
        Relaciones.Cuenta       EQ Creditos.Num_Credito:SCREEN-VALUE IN FRAME F_Solicitud AND
        Relaciones.Cod_Relacion EQ 11 AND
        Relaciones.Nit_Relacion EQ W_NitCodeudor:SCREEN-VALUE NO-ERROR.
                                                                                                               
    IF W_NvoCD AND NOT AVAIL(Relaciones) THEN                                                                  
       CREATE Relaciones. 
    ELSE IF NOT AVAIL(Relaciones) THEN 
       RETURN.

    ASSIGN Relaciones.Nit             = Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud                         
           Relaciones.Cod_Relacion    = 11                                                                     
           Relaciones.Nit_Relacion    = W_NitCodeudor:SCREEN-VALUE                                             
           Relaciones.Usuario         = W_Usuario                                                              
           Relaciones.Fec_Ingreso     = W_Fecha                                                                
           Relaciones.Estado          = 1                                                                      
           Relaciones.Clase_Producto  = 2                                                                      
           Relaciones.Cod_Producto    = INTEGER(SUBSTRING(Nom_Producto:SCREEN-VALUE IN FRAME F_Solicitud,1,3)) 
           Relaciones.Cuenta          = Creditos.Num_Credito:SCREEN-VALUE IN FRAME F_Solicitud                 
           Relaciones.Aprobada                                                                                 
           W_NvoCD                    = FALSE.                                                                 
                                                                                                               
    FIND CURRENT Relaciones NO-LOCK NO-ERROR.                                                                  
   
    FIND Clientes WHERE Clientes.Nit EQ Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
    IF AVAILABLE(Clientes) THEN DO:
       FIND FIRST Tcode WHERE TCode.TC_NitCode  = Relaciones.Nit_Relacion NO-ERROR.
       IF NOT AVAIL(TCode) THEN
          CREATE TCode.
       ASSIGN TCode.TC_AgeCode  = Clientes.Agencia
               TCode.TC_NitCode  = Relaciones.Nit_Relacion
               TCode.TC_NomCode  = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2
               TCode.TC_Aprob    = Relaciones.Aprobada
               TCode.TC_TelCdRs  = Clientes.Tel_Residencia
               TCode.TC_TelCdCo  = Clientes.Tel_Comercial
               TCode.TC_EmlCode  = Clientes.email.
    END.

    DISABLE W_NitCodeudor Btn_SalCod Btn_CanCod.
    OPEN QUERY BR_Codeudores FOR EACH TCode NO-LOCK INDEXED-REPOSITION.
    IF Br_Codeudores:NUM-SELECTED-ROWS IN FRAME F_Codeudores NE 0 THEN
       ASSIGN W_NitCodeudor:SCREEN-VALUE = TCode.TC_NitCode
              W_NomCodeudor:SCREEN-VALUE = TCode.TC_NomCode.
    ELSE
       ASSIGN W_NitCodeudor:SCREEN-VALUE = ""
              W_NomCodeudor:SCREEN-VALUE = "".
    ENABLE Btn_CreCod Btn_Activas.
END.
*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Formalizar
&Scoped-define SELF-NAME Btn_SalvaFormal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_SalvaFormal wWin
ON CHOOSE OF Btn_SalvaFormal IN FRAME F_Formalizar /* Salvar */
DO:
    vTime = TIME.

    ASSIGN W_fecini.

    IF Creditos.Estado:SCREEN-VALUE NE "2" AND Creditos.Estado:SCREEN-VALUE NE "4" THEN DO:
        MESSAGE "Debe Seleccionar Formalizado o Retirar sin Formalizar."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN.
    END.

    IF Rs_Desemb EQ 1 AND cta <> 4 THEN DO:
        MESSAGE "Debes escoger la cuenta A la vista a la cual se va a consignar el desembolso."
            VIEW-AS ALERT-BOX ERROR.

        RETURN.
    END.
    ELSE
        IF Rs_Desemb EQ 2 AND (W_CtaTerceros EQ " " OR W_NitTercero EQ " ") THEN DO:
            MESSAGE "Para Orden a terceros Indispensable Cta-Contable y Ced.Nit del Tercero."
                VIEW-AS ALERT-BOX ERROR.
            
            RETURN.
        END.

    RUN pdesembolso.

    FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ Creditos.Cod_Credito NO-LOCK NO-ERROR.

    RUN Valida_CfgRegCred (INPUT Creditos.Nit,
                           Creditos.Cod_Credito) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN.

    IF Clientes.Garantia EQ "No Admisible" AND NOT Pro_Creditos.Id_Extracto THEN DO:
        FIND FIRST Garantias WHERE Garantias.Agencia EQ Creditos.Agencia
                               AND Garantias.Nit EQ Creditos.Nit
                               AND Garantias.Tip_Credito EQ Creditos.Tip_Credito
                               AND Garantias.Cod_Credito EQ Creditos.Cod_Credito
                               AND Garantias.Tipo_Garantia GT 3
                               AND Garantias.Num_Solicitud EQ Creditos.Num_Solicitud
                               AND Garantias.Estado EQ 1
                               AND Garantias.Aprobada NO-LOCK NO-ERROR.
        IF NOT AVAIL(Garantias) THEN DO:
            MESSAGE "El Desembolso Exige Garantìa No-Admisible debidamente Aprobada..." SKIP
                    "                                No se permite la Operaciòn..."
                VIEW-AS ALERT-BOX ERROR.
            RETURN.
        END.
    END.

    IF Clientes.Garantia EQ "Inversion" AND NOT Pro_Creditos.Id_Extracto THEN DO:
        FIND FIRST Garantias WHERE Garantias.Agencia EQ Creditos.Agencia
                               AND Garantias.Nit EQ Creditos.Nit
                               AND Garantias.Tip_Credito EQ Creditos.Tip_Credito
                               AND Garantias.Cod_Credito EQ Creditos.Cod_Credito
                               AND Garantias.Tipo_Garantia EQ 3
                               AND Garantias.Num_Solicitud EQ Creditos.Num_Solicitud
                               AND Garantias.Estado EQ 1
                               AND Garantias.Aprobada NO-LOCK NO-ERROR.
        IF NOT AVAIL(Garantias) THEN DO:
            MESSAGE "El Desembolso Exige Garantìa Inversion, Admisible debidamente Aprobada..." SKIP
                    "                                No se permite la Operaciòn..."
                VIEW-AS ALERT-BOX ERROR.
            RETURN.
        END.
    END.

    IF (Clientes.Garantia EQ "Con Codeudor(es)" OR Clientes.Garantia EQ "Admisible y Codeudor") AND NOT Pro_Creditos.Id_Extracto THEN DO:
        FIND FIRST Relaciones WHERE Relaciones.Nit EQ Creditos.Nit
                                AND Relaciones.Cuenta EQ TRIM(STRING(Creditos.Num_Credito))
                                AND Relaciones.Clase_Producto EQ 2
                                AND Relaciones.Cod_Producto EQ Creditos.Cod_Credito
                                AND Relaciones.Cod_Relacion EQ 11
                                AND Relaciones.Estado EQ 1
                                AND Relaciones.Aprobada NO-LOCK NO-ERROR.
        IF NOT AVAIL(Relaciones) THEN DO:
            MESSAGE "El Desembolso exige Codeudor debidamente Aceptado..." SKIP
                    "                       No se permite la Operaciòn..."
                VIEW-AS ALERT-BOX ERROR.
            RETURN.
        END.
    END.

    IF Clientes.Garantia EQ "Prenda" AND NOT Pro_Creditos.Id_Extracto THEN DO:
        FIND FIRST Garantias WHERE Garantias.Agencia EQ Creditos.Agencia
                               AND Garantias.Nit EQ Creditos.Nit
                               AND Garantias.Tip_Credito EQ Creditos.Tip_Credito
                               AND Garantias.Cod_Credito EQ Creditos.Cod_Credito
                               AND Garantias.Tipo_Garantia EQ 2
                               AND Garantias.Num_Solicitud EQ Creditos.Num_Solicitud
                               AND Garantias.Estado EQ 1
                               AND Garantias.Aprobada NO-LOCK NO-ERROR.
        IF NOT AVAIL(Garantias) THEN DO:
            MESSAGE "El Desembolso Exige Garantìa Prenda, Admisible debidamente Aprobada..." SKIP
                    "                                No se permite la Operaciòn..."
                VIEW-AS ALERT-BOX ERROR.
            RETURN.
        END.
    END.

    IF Clientes.Garantia EQ "Admisible y Codeudor" OR
       Clientes.Garantia EQ "Hipoteca" OR
       Clientes.Garantia EQ "Prenda" OR
       Clientes.Garantia EQ "Inversion" THEN DO:
        IF Clientes.Garantia EQ "Hipoteca" AND NOT Pro_Creditos.Id_Extracto THEN DO:
            FIND FIRST Garantias WHERE Garantias.Agencia EQ Creditos.Agencia
                                   AND Garantias.Nit EQ Creditos.Nit
                                   AND Garantias.Tip_Credito EQ Creditos.Tip_Credito
                                   AND Garantias.Cod_Credito EQ Creditos.Cod_Credito
                                   AND Garantias.Tipo_Garantia EQ 1
                                   AND Garantias.Num_Solicitud EQ Creditos.Num_Solicitud
                                   AND Garantias.Estado EQ 1
                                   AND Garantias.Aprobada NO-LOCK NO-ERROR.
            IF NOT AVAIL(Garantias) THEN DO:
                MESSAGE "El Desembolso exige Garantìa HIPOTECA debidamente Aprobada..." SKIP
                        "                                No se permite la Operaciòn..."
                    VIEW-AS ALERT-BOX ERROR.
                RETURN.
            END.
        END.
        ELSE DO:
            FIND FIRST Garantias WHERE Garantias.Agencia EQ Creditos.Agencia
                                   AND Garantias.Nit EQ Creditos.Nit
                                   AND Garantias.Tip_Credito EQ Creditos.Tip_Credito
                                   AND Garantias.Cod_Credito EQ Creditos.Cod_Credito
                                   AND Garantias.Tipo_Garantia LE 3
                                   AND Garantias.Num_Solicitud EQ Creditos.Num_Solicitud
                                   AND Garantias.Estado EQ 1
                                   AND Garantias.Aprobada NO-LOCK NO-ERROR.
            IF NOT AVAIL(Garantias) AND NOT Pro_Creditos.Id_Extracto THEN DO:
                MESSAGE "El Desembolso exige Garantìa Admisible debidamente Aprobada..." SKIP
                        "                                No se permite la Operaciòn..."
                    VIEW-AS ALERT-BOX ERROR.
                RETURN.
            END.
        END.
    
        IF Creditos.Estado:SCREEN-VALUE EQ "2" AND (W_fecha - Creditos.fec_aprobacion GE 120) THEN DO:
            MESSAGE "La solicitud de credito no se puede desembolsar ...Oper.Rechazada." SKIP
                    "la fecha de aprobacion tiene mas de 4 meses"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN.
        END.

        IF NOT Pro_Creditos.Id_Extracto THEN DO:
            IF Garantias.Val_Asegurado LE 0 OR
               Garantias.Val_Bien LE 0 OR
               Garantias.Val_UltAvaluo LE 0 OR
               Garantias.Nro_Seguro LE " " OR
               Garantias.Nit_Aseguradora LE "0" OR
               Garantias.Fec_FinSeguro EQ ? OR
               Garantias.Fec_IniSeguro EQ ? OR
               Garantias.Fec_ProxAvaluo EQ ? OR
               Garantias.Fec_UltAvaluo EQ ? THEN DO:
                MESSAGE "La Garantìa Admisible tiene datos incompletos (del Seguro o del Avalùo)..."
                        "                   No se permite la Operaciòn..."
                    VIEW-AS ALERT-BOX ERROR.
                RETURN.
            END.

            IF Garantias.Fec_FinSeguro LT W_Fecha THEN DO:
                MESSAGE "La Garantìa Admisible tiene la Fecha del Seguro vencida..."
                        "                   No se permite la Operaciòn..."
                    VIEW-AS ALERT-BOX ERROR.
                RETURN.
            END.

            IF Garantias.Fec_UltAvaluo LT (W_Fecha - 90) THEN DO:
                MESSAGE "La Garantía Admisible tiene la Fecha del Ult-Avalúo con más de 90 días..."
                        "                   No se permite la Operaciòn..."
                    VIEW-AS ALERT-BOX ERROR.
                RETURN.
            END.
        END.
    END.

    IF Creditos.Pagare:SCREEN-VALUE EQ "" AND Creditos.Estado:SCREEN-VALUE EQ "2" THEN DO:
        MESSAGE "No se puede Formalizar el Crédito si no se" SKIP
                "ha digitado el número de pagaré preimpreso"
            VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO Creditos.Pagare.
        RETURN NO-APPLY.
    END.

    FIND FIRST Mov_Instancias WHERE Mov_Instancias.Agencia EQ Creditos.Agencia
                                AND Mov_Instancias.Instancia EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5))
                                AND Mov_Instancias.Usuario EQ W_Usuario
                                AND Mov_Instancias.Estado EQ NO
                                AND Mov_Instancias.Nit EQ creditos.Nit
                                AND INTEGER(Mov_Instancias.Cuenta) EQ creditos.Num_Credito NO-ERROR.
    IF NOT AVAIL(Mov_Instancias) THEN DO:
        MESSAGE "No se hallo el Mov-Instancias para el retiro de Esta Instancia."
                "               Operacion Rechazada."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    
    ASSIGN W_TipoInforme = "Desembolso"
           Creditos.Cuota = ROUND(Creditos.Cuota,0)
           Creditos.Pagare = STRING(Creditos.Num_Credito).

    IF Rs_Desemb EQ 3 THEN DO:
        MESSAGE "Está Seguro de Formalizarlo para Autorizar el Desembolso en la Caja...?" SKIP(1)
                "La Fecha de Inicio es : " W_FecIni SKIP(1) "W_Fecha " W_Fecha SKIP(1)
                "Pro_Creditos.Id_PerGracia " Pro_Creditos.Id_PerGracia SKIP(1)
                "Pro_Creditos.Dia_Gracia " Pro_Creditos.Dia_Gracia SKIP(1)
                "Los Intereses Anticipados a descontar del Crédito son: " W_IntAntic SKIP(1)
                "El Descuento para Aportes Diferidos es de $ : " W_FaltApor
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "CONFIRMAR-DESEMBOLSO" UPDATE W_SiFormal AS LOGICAL.

        IF NOT W_SiFormal THEN
            RETURN NO-APPLY.
    END.
    ELSE DO:
        MESSAGE "Está Seguro de Formalizar y DESEMBOLSAR el Credito...?" SKIP(1)
                "La Fecha de Inicio es : " W_FecIni SKIP(1) "W_Fecha " W_Fecha SKIP(1)
                "Pro_Creditos.Id_PerGracia " Pro_Creditos.Id_PerGracia SKIP(1)
                "Pro_Creditos.Dia_Gracia " Pro_Creditos.Dia_Gracia SKIP(1)
                "Los Intereses Anticipados a descontar del Crédito son: " W_IntAntic SKIP(1)
                "El Descuento para Aportes Diferidos es de $ : " W_FaltApor VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "CONFIRMAR-DESEMBOLSO" UPDATE W_SiForDes AS LOGICAL.
        
        IF NOT W_SiForDes THEN
            RETURN NO-APPLY.
    END.

    Creditos.Fec_PagAnti = W_FecIni.

    DEF VAR Listado AS CHA INIT "".
    Listado = W_PathSpl + W_Usuario + "PPlanPag.LST".
/*     {INCLUIDO\ImpPlanPag.I "Listado"} /*ojo*/ */

    IF Rs_Desemb EQ 1 THEN
        ASSIGN Credito.Desembolso = 3
               W_VrAlAhorro = W_NetoDesemb - W_IntAntic. /*nh*/
    ELSE
        IF Rs_Desemb EQ 2 THEN DO:
            FIND FIRST Cuentas WHERE Cuentas.cuenta EQ W_CtaTerceros AND Cuentas.Tipo EQ 2 AND Cuentas.Id_Nit NO-LOCK NO-ERROR.
            IF NOT AVAIL(Cuentas) THEN DO:
                MESSAGE "Debe digitar la Cta-Contable para Orden a Terceros y que Opera con Nit."
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
                RETURN NO-APPLY.
            END.

            FIND FIRST Clientes WHERE Clientes.Nit EQ W_NitTercero NO-LOCK NO-ERROR.
            IF NOT AVAIL(Clientes) THEN DO:
                MESSAGE "Debe digitar la Ced/Nit del Tercero"
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
                RETURN NO-APPLY.
            END.

            ASSIGN Credito.Desembolso = 4
                   W_NetoDesemb = W_NetoDesemb - W_IntAntic. /*nh*/
        END.

    IF Rs_Desemb NE 3 THEN DO:
        ASSIGN Creditos.Pagare:SCREEN-VALUE IN FRAME F_Ultima = STRING(Creditos.Num_Credito)
               Creditos.Pagare:SCREEN-VALUE IN FRAME F_Formalizar = STRING(Creditos.Num_Credito).

        RUN Grabar_Credito NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            MESSAGE "Desembolso con Inconsistencias, Operacion no realizada."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            ASSIGN Credito.Estado = 1
                   Creditos.Sdo_Capital = 0.

            RETURN NO-APPLY.
        END.
    END.
    ELSE DO:
        FIND FIRST Mov_Instancias WHERE Mov_Instancias.Agencia EQ Creditos.Agencia
                                    AND Mov_Instancias.Instancia EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5))
                                    AND Mov_Instancias.Usuario EQ W_Usuario
                                    AND Mov_Instancias.Estado EQ NO
                                    AND Mov_Instancias.Nit EQ creditos.Nit
                                    AND INTEGER(Mov_Instancias.Cuenta) EQ creditos.Num_Credito NO-ERROR.
        ASSIGN Mov_Instancias.Fec_Retiro = TODAY
               Mov_Instancias.Hora_Retiro = TIME
               Mov_Instancias.Estado = YES.
        
        RUN Asignar_Proxima_Instancia NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            MESSAGE "Se retiro de la Anterior, pero no se asigno por no hallar la proxima Instancia."  SKIP
                    "   Comunique al administrador del Aplicativo, Error en configuraciones de Instancias."
                VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.

        FOR EACH Consulta:
            DELETE Consulta.
        END.

        CLOSE QUERY Br_Consulta.

        ASSIGN FRAME F_Formalizar:VISIBLE = FALSE
               FRAME F_Creditos:SENSITIVE = TRUE.

        APPLY "choose" TO Btn_Consulta IN FRAME F_Creditos.
        ENABLE ALL WITH FRAME F_Creditos.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_SalvaFormal wWin
ON ENTRY OF Btn_SalvaFormal IN FRAME F_Formalizar /* Salvar */
DO:
  
  IF Creditos.Desembolso EQ 2 THEN DO:
     Nom_Beneficiario:SCREEN-VALUE IN FRAME F_Bancos = NomNit:SCREEN-VALUE IN FRAME F_Solicitud.
     VIEW FRAME F_Bancos.
     APPLY "entry" TO Num_Cheque IN FRAME F_Bancos.
     RETURN NO-APPLY.
  END.

  IF Creditos.Desembolso = 9 THEN DO:
      VIEW FRAME F_Bancos.
      num_cheque:SCREEN-VALUE = "TRANSF".
      num_cheque:SENSITIVE = FALSE.
      RETURN NO-APPLY.
  END.
  ELSE
      num_cheque:SENSITIVE = TRUE.

/* inicio agregados  harold */ 
  ASSIGN  w_fecIni = finicio
  W_DiasAnt  = 0
  W_IntAntic = 0.
   
IF Pro_Creditos.Id_PerGracia AND Pro_Creditos.Dia_Gracia GT 0 THEN DO:
MESSAGE "La linea del Credito es con Pdo.de Gracia, no se permite Modificar la Fec-Inicio."
  VIEW-AS ALERT-BOX INFO BUTTONS OK.
ASSIGN W_FecIni = W_Fecha + Pro_Creditos.Dia_Gracia
     W_FecIni:SCREEN-VALUE = STRING(W_fecini).
RETURN.
END.     

IF W_FecIni LT W_Fecha OR (W_FecIni - W_Fecha) GT 359 /* 29 */ THEN DO:
MESSAGE "La fecha de Inicio para Control de Pago y/o Vencimientos," SKIP
      "No puede ser anterior a Hoy, ni posterior superior a 360 días..."
  VIEW-AS ALERT-BOX INFO BUTTONS OK.
ASSIGN W_FecIni = W_Fecha
     W_FecIni:SCREEN-VALUE = STRING(W_Fecha).
END.

IF W_FecIni GT W_Fecha THEN DO:  /*Halla Int-Anticipados para descontar del Crédito*/
ASSIGN W_DiasAnt  = W_FecIni - W_Fecha
      W_DiasAnt               = 0
      W_IntAntic = ROUND(((Creditos.Monto) * Creditos.Tasa / 36000) * W_DiasAnt,0).

IF W_NetoDesemb - W_IntAntic LT 0 THEN DO:
  MESSAGE "El valor de los Int-Anticipados a descontar es : " W_IntAntic
          "El valor a Desembolsar no puede ser Negativo...? Revise por favor." SKIP
          "          No se permite la Operacion...!" VIEW-AS ALERT-BOX ERROR.
  ASSIGN 
         W_FecIni:SCREEN-VALUE   = STRING(finicio)
         W_DiasAnt               = 0
         W_IntAntic              = 0.
END. 

END.
/* no cobra deducible */
/* ASSIGN W_NetoAD              = W_VrADesemb 
  W_NetoAD:SCREEN-VALUE = STRING(W_NetoAD).*/

ASSIGN W_NetoAD              = W_VrADesemb - W_IntAntic
  W_NetoAD:SCREEN-VALUE = STRING(W_NetoAD).




/*fin agregados */


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_HojaVida
&Scoped-define SELF-NAME Btn_SalvaHV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_SalvaHV wWin
ON CHOOSE OF Btn_SalvaHV IN FRAME F_HojaVida /* Salvar */
DO:
    DISABLE Btn_SalvaHV WITH FRAME F_HojaVida.
    ENABLE Btn_NvoHV WITH FRAME F_HojaVida.

    IF W_NvaHv THEN DO:
        CREATE Hoja_Vida.
        ASSIGN Hoja_Vida.Tipo = 9 
               Hoja_Vida.Codigo = 1
               Hoja_Vida.Instancia = INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5))
               Hoja_Vida.DoctoRefer = INTEGER(Creditos.Num_Credito:SCREEN-VALUE IN FRAME F_Solicitud)
               Hoja_Vida.Nit = Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud
               Hoja_Vida.Usuario = W_Usuario
               Hoja_Vida.Fec_Grabacion = TODAY
               Hoja_Vida.Hora_Grabacion = TIME.

        W_NvaHv = NO.
        Hoja_Vida.Observacion:READ-ONLY = YES.
    END.
    ELSE
        FIND CURRENT Hoja_Vida EXCLUSIVE-LOCK.

    ASSIGN FRAME F_HojaVida
        Hoja_Vida.Asunto_Cumplido
        Hoja_Vida.Observacion.

    IF AVAILABLE Hoja_Vida AND Hoja_Vida.Asunto_Cumplido THEN
        APPLY "choose" TO Btn_HojaVida IN FRAME F_Solicitud.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ultima
&Scoped-define SELF-NAME Btn_SalvaUltima
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_SalvaUltima wWin
ON CHOOSE OF Btn_SalvaUltima IN FRAME F_Ultima /* S A L V A R */
DO:
    vTime = TIME.

    DO WITH FRAME F_Ultima:
        MESSAGE wnombres[1] wvlres[1] SKIP
                wnombres[2] wvlres[2] SKIP
                wnombres[3] wvlres[3] SKIP
                wnombres[4] wvlres[4] SKIP
                wnombres[5] wvlres[5] SKIP
                wnombres[6] wvlres[6] SKIP
                wnombres[7] wvlres[7] SKIP
                wnombres[8] wvlres[8] SKIP
                wnombres[9] wvlres[9] SKIP
                "Créditos a Canc: " W_VrCredACanc SKIP
                "Aportes Faltant: " W_FaltApor SKIP(1)
                "Total Deducibles = " Creditos.Deducible
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        Creditos.Pagare:SCREEN-VALUE = STRING(Creditos.Num_Credito).

        IF Creditos.Estado:SCREEN-VALUE NE "2" AND Creditos.Estado:SCREEN-VALUE NE "4" THEN DO:
            MESSAGE "Debe Seleccionar Desembolsar o Retirar sin Desembolso."
                VIEW-AS ALERT-BOX ERROR.

            RETURN.
        END.

        FIND FIRST Clientes WHERE Clientes.Nit EQ Creditos.Nit NO-LOCK NO-ERROR.

        IF Creditos.Estado:SCREEN-VALUE EQ "4" THEN DO:
            MESSAGE "El desembolso del Crédito no se realizará...?" SKIP
                    "Seleccionó Retirado sin Desembolso." SKIP
                    "       Continue solo si está SEGURo de Anularlo..."
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirme Anulación" UPDATE W_RptaD AS LOGICAL.

            IF NOT W_RptaD THEN
                RETURN.

            RUN Negar_Solicitud.

            RETURN.
        END.

        IF Creditos.Desembolso:SCREEN-VALUE = "8" AND W_VrADesemb > 0 THEN DO:
            MESSAGE "Debe Seleccionar una Opcion de Desembolso Correcta."
                VIEW-AS ALERT-BOX ERROR.

            RETURN.
        END.

        IF Creditos.Desembolso EQ 4 THEN DO:
            IF W_CtaTerceros LE "0" OR W_CtaTerceros EQ ? OR W_VrAlAhorro GT 0 OR W_VrCheque GT 0 OR W_VrEfectivo GT 0 THEN DO:
                MESSAGE "Falta la Cuenta Contable para Orden a Terceros...O" SKIP
                        "Existen valores a Desembolsar en Efectivo-Cheque o Vr.Al Ahorro." SKIP
                        "                      Oper.Rechazada."
                    VIEW-AS ALERT-BOX ERROR.

                APPLY "ENTRY" TO W_CtaTerceros.
                RETURN.
            END.
        END.
        ELSE
            IF (W_VrAlAhorro + W_VrCheque + W_VrEfectivo) NE W_NetoDesemb THEN DO:
                MESSAGE "Existen valores a Desembolsar en Efectivo-Cheque o Vr.Al Ahorro," SKIP
                        "Que sumados no corresponden con el Neto-Desembolso....Oper.Rechazada."
                    VIEW-AS ALERT-BOX ERROR.

                RETURN.
            END.

        IF (Creditos.Desembolso EQ 1 OR Creditos.Desembolso EQ 5 OR Creditos.Desembolso EQ 6) AND W_VrEfectivo < 0 THEN DO:
            MESSAGE "Los datos para desembolso en Efectivo deben estar completos," SKIP
                    "          No se permite la Operacion...!"
                VIEW-AS ALERT-BOX ERROR.

            RETURN.
        END.

        IF W_NetoDesemb LT 0 THEN DO:
            MESSAGE "El valor a Desembolsar no puede ser Negativo...? Revise por favor." SKIP
                    "          No se permite la Operacion...!"
                VIEW-AS ALERT-BOX ERROR.

            RETURN.
        END.

        IF Creditos.Desembolso EQ 3 OR (Creditos.Desembolso GE 6 AND creditos.desembolso <> 9) THEN DO:
            FIND FIRST Ahorros WHERE Ahorros.Agencia EQ Creditos.Age_Desembolso
                                 AND Ahorros.Cod_Ahorro EQ Creditos.Cod_Desembolso
                                 AND Ahorros.Cue_Ahorros EQ Creditos.Cue_Desembolso
                                 AND Ahorros.Estado EQ 1
                                 AND Ahorros.Tip_Ahorro EQ 1 NO-LOCK NO-ERROR.
            IF NOT AVAILABLE(Ahorros) OR W_VrAlAhorro < 0 THEN DO:
                MESSAGE "La Cuenta-Ahorros seleccionada para el Desembolso debe ser A la Vista y Activa, O" SKIP
                        "Falta el valor para la Consignacion a la Cta-Ahorros."
                        "          No se permite la Operacion...!"
                    VIEW-AS ALERT-BOX ERROR.

                RETURN.
            END.

            FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ Ahorros.Cod_Ahorro NO-LOCK NO-ERROR.

            MESSAGE "La cta-ahorros a consignarle es: " Ahorros.Cue_Ahorros SKIP
                    "La agencia de esta Cuenta es   : " Ahorros.Agencia SKIP
                    "Del producto                   : " Pro_Ahorros.Nom_Producto SKIP
                    "Está Seguro de Continuar...?"
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "CONFIRMAR-DESEMB.A CTA-AHORRO" UPDATE W_SiDesemCA AS LOGICAL.

            IF NOT W_SiDesemCA THEN
                RETURN.
        END.

        IF Creditos.Desembolso EQ 2 OR Creditos.Desembolso EQ 5 OR Creditos.Desembolso EQ 7 THEN DO:
            IF Num_Cheque LE "00000" OR W_CtaBanco LE " " OR W_CtaBanco = ? OR Nom_Beneficiario LE " " OR W_VrCheque < 0 THEN DO:
                MESSAGE "Los datos para desembolso en cheque deben estar completos," SKIP
                        "          No se permite la Operacion...!" SKIP
                        "num_cheque" num_cheque SKIP
                        "W_CtaBanco" W_CtaBanco SKIP
                        "Nom_Beneficiario" Nom_Beneficiario SKIP
                        "W_VrCheque" W_VrCheque
                    VIEW-AS ALERT-BOX ERROR.

                RETURN.
            END.

            MESSAGE "El desembolso es con No.Cheque   : " Num_Cheque SKIP
                    "                 con Cta-Banco   : " W_CtaBanco SKIP
                    "                 con Beneficiario: " Nom_Beneficiario SKIP
                    "Está Seguro de Continuar...?"
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "CONFIRMAR-DESEMB.CHEQUE" UPDATE W_SiDesemCh AS LOGICAL.

            IF NOT W_SiDesemCh THEN
                RETURN.
        END.

        IF Creditos.Desembolso = 9 THEN DO:
            IF W_CtaBanco <= " " OR W_CtaBanco = ? OR W_VrCheque < 0 THEN DO:
                MESSAGE "Los datos para desembolso por Transferencia." SKIP
                        "La operación de cancela...!"
                    VIEW-AS ALERT-BOX ERROR.

                RETURN.
            END.

            MESSAGE "El desembolso es en modalidad Transferencia del banco" W_CtaBanco SKIP
                    "Está Seguro de Continuar...?"
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "CONFIRMAR-DESEMB.CHEQUE" UPDATE flagDesembolsaTransf AS LOGICAL.

            IF NOT flagDesembolsaTransf THEN
                RETURN.
        END.

        IF Creditos.Estado:SCREEN-VALUE EQ "2" THEN DO:
            MESSAGE "La Fecha de Inicio es : " W_FecIni "," SKIP
                    "El Descuento para Aportes Diferidos es de $ : " W_FaltApor SKIP
                    "                        Valor en Efectivo : " W_VrEfectivo SKIP
                    "                        Valor en Cheque   : " W_VrCheque SKIP
                    "                        Valor Al Ahorro   : " W_VrAlAhorro SKIP
                    "Neto Desembolso $ " W_NetoDesemb SKIP
                    "                        Está Seguro de Realizar el Desembolso...?"
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "CONFIRMAR-DESEMBOLSO" UPDATE W_SiDesem AS LOGICAL.

            IF NOT W_SiDesem THEN
                RETURN.

            RUN Grabar_Credito NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                MESSAGE "Desembolso con Inconsistencias, Operacion no realizada."
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Solicitud
&Scoped-define SELF-NAME Btn_Scoring
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Scoring wWin
ON CHOOSE OF Btn_Scoring IN FRAME F_Solicitud /* Scoring */
DO:
    /* harold por mensajes de precaucion
   RETURN. 

  DISABLE ALL WITH FRAME F_Creditos.
  TOTAL_Puntaje = 0.
  ENABLE ALL EXCEPT TOTAL_Puntaje WITH FRAME F_Scoring.
  RUN Proceso_Scoring.
  VIEW FRAME F_Scoring.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME Buscar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Buscar wWin
ON LEAVE OF Buscar IN FRAME F_Consulta
DO:
  CASE R_Organizar:SCREEN-VALUE:
    WHEN "1" THEN DO:
      OPEN QUERY Br_Consulta FOR EACH Consulta WHERE
                Consulta.Num_Credito EQ DECIMAL(SELF:SCREEN-VALUE)
                NO-LOCK  BY Consulta.Num_Credito INDEXED-REPOSITION.
    END.
    WHEN "2" THEN DO:
      OPEN QUERY Br_Consulta FOR EACH Consulta WHERE
                Consulta.AgeCredito EQ DECIMAL(SELF:SCREEN-VALUE)
                NO-LOCK  BY Consulta.AgeCredito INDEXED-REPOSITION.
    END.
    WHEN "3" THEN DO:
      OPEN QUERY Br_Consulta FOR EACH Consulta WHERE
                Consulta.Nit EQ SELF:SCREEN-VALUE
                NO-LOCK  BY Consulta.Nit INDEXED-REPOSITION.
    END.
    WHEN "4" THEN DO:
      OPEN QUERY Br_Consulta FOR EACH Consulta WHERE
                Consulta.Nombre BEGINS SELF:SCREEN-VALUE
                NO-LOCK  BY Consulta.Nombre INDEXED-REPOSITION.
    END.
    WHEN "5" THEN DO:
      OPEN QUERY Br_Consulta FOR EACH Consulta WHERE
                Consulta.Fec_Ingreso EQ DATE(SELF:SCREEN-VALUE)
                NO-LOCK  BY Consulta.Fec_ingreso INDEXED-REPOSITION.
    END.
  END CASE.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Creditos
&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 wWin
ON CHOOSE OF BUTTON-1 IN FRAME F_Creditos /* Button 1 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Deducibles
&Scoped-define SELF-NAME BUTTON-101
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-101 wWin
ON CHOOSE OF BUTTON-101 IN FRAME F_Deducibles /* Button 101 */
DO:
  ENABLE ALL WITH FRAME F_Creditos.
  DISABLE NomUsuario WITH FRAME F_Creditos.
  HIDE FRAME F_Deducibles.
  FRAME F_Consulta:HIDDEN = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Solicitud
&Scoped-define SELF-NAME BUTTON-102
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-102 wWin
ON CHOOSE OF BUTTON-102 IN FRAME F_Solicitud /* Deducibles */
DO:
  OPEN QUERY Br_Deducibles FOR EACH TDeducc NO-LOCK INDEXED-REPOSITION.
  DISABLE ALL WITH FRAME F_Creditos.
  ENABLE ALL WITH FRAME F_Deducibles.
  VIEW FRAME F_Deducibles.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_InfoCliente
&Scoped-define SELF-NAME BUTTON-108
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-108 wWin
ON CHOOSE OF BUTTON-108 IN FRAME F_InfoCliente /* Button 108 */
DO:
  ENABLE ALL WITH FRAME F_Creditos.
  DISABLE NomUsuario WITH FRAME F_Creditos.
  HIDE FRAME F_InfoCliente.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Solicitud
&Scoped-define SELF-NAME BUTTON-120
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-120 wWin
ON CHOOSE OF BUTTON-120 IN FRAME F_Solicitud /* Producto */
DO:
 DO WITH FRAME F_Solicitud:
  DISABLE ALL WITH FRAME F_Creditos.
  ENABLE ALL WITH FRAME F_InfoProducto.
  VIEW FRAME F_InfoProducto.
 END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-134
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-134 wWin
ON CHOOSE OF BUTTON-134 IN FRAME F_Solicitud /* Garantías */
DO:
  HIDE FRAME F_Consultas.
  DISABLE ALL WITH FRAME F_Creditos.
  ENABLE ALL WITH FRAME F_Garantias.

  R_TipoGarantia:SCREEN-VALUE IN FRAME F_Garantias = "1".
  APPLY "value-changed" TO R_TipoGarantia IN FRAME F_Garantias.
  puntero = ROWID(Clientes).
  FOR EACH TCode: DELETE TCode. END.

IF AVAILABLE Creditos THEN DO:
  FOR EACH Relaciones WHERE 
           Relaciones.Nit            EQ Creditos.Nit        AND
           Relaciones.Cuenta         EQ TRIM(STRING(Creditos.Num_Credito)) AND
           Relaciones.Clase_Producto EQ 2                    AND
           Relaciones.Cod_Producto   EQ Creditos.Cod_Credito AND
           Relaciones.Cod_Relacion   EQ 11:
      FIND Clientes WHERE Clientes.Nit EQ Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
      IF AVAILABLE Clientes THEN DO:
        CREATE TCode.
        ASSIGN TCode.TC_AgeCode = Clientes.Agencia
               TCode.TC_NitCode = Clientes.Nit
               TCode.TC_NitDeud = Relaciones.Nit
               TCode.TC_NumSoli = DECIMAL(Relaciones.Cuenta)
               TCode.TC_NomCode = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2
               TCode.TC_Aprob   = Relaciones.Aprobada
               TCode.TC_TelCdRs = Clientes.Tel_Residencia
               TCode.TC_TelCdCo = Clientes.Tel_Comercial
               TCode.TC_EmlCode = Clientes.email
               TCode.TC_EstRela = Relaciones.Estado
               TCode.TC_FecCrea = Relaciones.Fec_Ingreso
               TCode.TC_FecReti = Relaciones.Fec_Inactividad.
      END.
  END.
  OPEN QUERY Br_Codeudores FOR EACH TCode WHERE TCode.TC_EstRela EQ 1 NO-LOCK INDEXED-REPOSITION.
  IF Br_Codeudores:NUM-SELECTED-ROWS IN FRAME F_Codeudores NE 0 THEN
     ASSIGN W_NitCodeudor:SCREEN-VALUE = TCode.TC_NitCode
            W_NomCodeudor:SCREEN-VALUE = TCode.TC_NomCode.
  ELSE
     ASSIGN W_NitCodeudor:SCREEN-VALUE = ""
            W_NomCodeudor:SCREEN-VALUE = "".
  VIEW FRAME F_Garantias.
  HIDE FRAME F_ConAdmisible.
END.
ELSE DO:
  MESSAGE "No hay una solicitud disponible" VIEW-AS ALERT-BOX INFORMATION.
END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Instancias
&Scoped-define SELF-NAME BUTTON-142
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-142 wWin
ON CHOOSE OF BUTTON-142 IN FRAME F_Instancias /* Button 142 */
DO:
  OPEN QUERY Br_Cerradas FOR EACH TCerradas NO-LOCK INDEXED-REPOSITION.
  ENABLE ALL WITH FRAME F_Cerradas.
  VIEW FRAME F_Cerradas.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_HojaVida
&Scoped-define SELF-NAME BUTTON-149
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-149 wWin
ON CHOOSE OF BUTTON-149 IN FRAME F_HojaVida /* Button 149 */
DO:
  ENABLE ALL WITH FRAME F_Creditos.
  DISABLE NomUsuario WITH FRAME F_Creditos.
  HIDE FRAME F_HojaVida.
  FRAME F_Consulta:HIDDEN = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-150
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-150 wWin
ON CHOOSE OF BUTTON-150 IN FRAME F_HojaVida /* Button 150 */
DO:
  ENABLE ALL WITH FRAME F_conHV.
  OPEN QUERY Br_ConHV FOR EACH Hoja_Vida WHERE
       Hoja_Vida.Tipo      EQ 9 AND Hoja_Vida.Codigo EQ 1 AND 
       Hoja_Vida.Instancia EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) AND
       Hoja_Vida.DoctoRef  EQ INTEGER(Creditos.Num_Credito:SCREEN-VALUE IN FRAME F_Solicitud) AND
       Hoja_Vida.Nit       EQ Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud AND
       Hoja_Vida.Usuario   EQ W_Usuario 
 INDEXED-REPOSITION.
 VIEW FRAME F_ConHV.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-152
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-152 wWin
ON CHOOSE OF BUTTON-152 IN FRAME F_HojaVida /* Cancelar */
DO:
  DISABLE Btn_SalvaHV WITH FRAME F_HojaVida.
  APPLY "choose" TO Btn_HojaVida IN FRAME F_Solicitud.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Agregar
&Scoped-define SELF-NAME BUTTON-153
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-153 wWin
ON CHOOSE OF BUTTON-153 IN FRAME F_Agregar /* Button 153 */
DO:
  HIDE FRAME F_Agregar.
  IF E_Agregar:SCREEN-VALUE NE "" THEN DO:
     IF Id_Agregar EQ "HV" THEN DO:
        Hoja_Vida.Observacion:SCREEN-VALUE IN FRAME F_HojaVida = Hoja_Vida.Observacion:SCREEN-VALUE + 
        ". Fecha: " + STRING(TODAY) + " :" + E_Agregar:SCREEN-VALUE IN FRAME F_Agregar. 
        ENABLE Btn_SalvaHV WITH FRAME F_HojaVida.
     END.
     IF Id_Agregar EQ "IN" THEN DO:
        Mov_Instancias.Descripcion:SCREEN-VALUE IN FRAME F_Instancias = 
            Mov_Instancias.Descripcion:SCREEN-VALUE IN FRAME F_Instancias + 
            ". Fecha: " + STRING(TODAY) + " :" + E_Agregar:SCREEN-VALUE IN FRAME F_Agregar. 
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-153 wWin
ON ENTRY OF BUTTON-153 IN FRAME F_Agregar /* Button 153 */
DO:
  ON RETURN RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-153 wWin
ON LEAVE OF BUTTON-153 IN FRAME F_Agregar /* Button 153 */
DO:
  ON RETURN TAB.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Cerradas
&Scoped-define SELF-NAME BUTTON-154
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-154 wWin
ON CHOOSE OF BUTTON-154 IN FRAME F_Cerradas /* Ver solo Instancia y Descripción */
DO:
  IF SELF:LABEL EQ "Ver solo Instancia y Descripción" THEN DO:
     TCerradas.Instancia:VISIBLE IN BROWSE br_cerradas = NO.
     TCerradas.INom_Instancia:VISIBLE IN BROWSE br_cerradas = YES.
     TCerradas.Usuario:VISIBLE IN BROWSE br_cerradas = NO.
     TCerradas.INom_Usuario:VISIBLE IN BROWSE br_cerradas = NO.
     TCerradas.Fec_Retiro:VISIBLE IN BROWSE br_cerradas = NO.
     TCerradas.Descripcion:VISIBLE IN BROWSE br_cerradas = YES.
     SELF:LABEL = "Todas las columnas".
  END.
  ELSE DO:
     TCerradas.Instancia:VISIBLE IN BROWSE br_cerradas = YES.
     TCerradas.INom_Instancia:VISIBLE IN BROWSE br_cerradas = YES.
     TCerradas.Usuario:VISIBLE IN BROWSE br_cerradas = YES.
     TCerradas.INom_Usuario:VISIBLE IN BROWSE br_cerradas = YES.
     TCerradas.Fec_Retiro:VISIBLE IN BROWSE br_cerradas = YES.
     TCerradas.Descripcion:VISIBLE IN BROWSE br_cerradas = YES.
     SELF:LABEL = "Ver solo Instancia y Descripción".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Codeudores
&Scoped-define SELF-NAME BUTTON-155
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-155 wWin
ON CHOOSE OF BUTTON-155 IN FRAME F_Codeudores /* i */
DO:
  FIND Clientes WHERE Clientes.Nit EQ W_NitCodeudor:SCREEN-VALUE IN FRAME F_Codeudores NO-LOCK NO-ERROR.
  IF AVAILABLE Clientes THEN RUN Llenar_InfoCliente.
  DISABLE ALL WITH FRAME F_Creditos.
  ENABLE ALL WITH FRAME F_InfoCliente.
  VIEW FRAME F_InfoCliente.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_InfoCliente
&Scoped-define SELF-NAME BUTTON-156
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-156 wWin
ON CHOOSE OF BUTTON-156 IN FRAME F_InfoCliente /* Ver Información Detallada */
DO:
  RUN W-ConsultaGeneral.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Admisible
&Scoped-define SELF-NAME BUTTON-161
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-161 wWin
ON CHOOSE OF BUTTON-161 IN FRAME F_Admisible /* Button 161 */
DO:
  HIDE FRAME F_Admisible.
  APPLY "choose" TO Btn_OutGarantias IN FRAME F_Garantias.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Partidas
&Scoped-define SELF-NAME BUTTON-163
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-163 wWin
ON CHOOSE OF BUTTON-163 IN FRAME F_Partidas /* Button 163 */
DO:
  HIDE FRAME F_Partidas.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Solicitud
&Scoped-define SELF-NAME BUTTON-19
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-19 wWin
ON CHOOSE OF BUTTON-19 IN FRAME F_Solicitud /* i */
DO:
  IF AVAILABLE Clientes THEN DO:
    DISABLE ALL WITH FRAME F_Creditos.
    ENABLE ALL WITH FRAME F_InfoCliente.
    RUN Llenar_InfoCliente.
    VIEW FRAME F_InfoCliente.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Creditos
&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 wWin
ON CHOOSE OF BUTTON-2 IN FRAME F_Creditos /* Salir */
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


&Scoped-define FRAME-NAME F_Deducibles
&Scoped-define SELF-NAME BUTTON-207
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-207 wWin
ON CHOOSE OF BUTTON-207 IN FRAME F_Deducibles /* Imprimir Deducibles */
DO:
  W_TipoInforme = "Deducibles".
  APPLY "choose" TO Btn_Imprimir IN FRAME F_Creditos.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Solicitud
&Scoped-define SELF-NAME BUTTON-208
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-208 wWin
ON CHOOSE OF BUTTON-208 IN FRAME F_Solicitud /* Inf_Previa */
DO:
  InputFile = "formatos\CC - 335.xls".
  RUN Abrir_Excel.
  IF SwExiste EQ ?  THEN 
      RETURN NO-APPLY.
  ELSE
  RUN Imprime_Aceptacion.
  /* RUN Cerrar_Excel. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-209
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-209 wWin
ON CHOOSE OF BUTTON-209 IN FRAME F_Solicitud /* Libranza */
DO:
  InputFile = "formatos\CC - 316.xls".
  RUN Abrir_Excel.
    IF SwExiste EQ ?  THEN 
      RETURN NO-APPLY.
  ELSE
  RUN Imprime_Libranza.
 /* RUN Cerrar_Excel.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-210
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-210 wWin
ON CHOOSE OF BUTTON-210 IN FRAME F_Solicitud /* Todo */
DO:
  DEFINE VARIABLE  fante AS DATE FORMAT "99/99/9999".    
       
        InputFile = "formatos\CC - 314.xls".
    ptipo = 1.

    IF W-fecEntLib EQ ? THEN DO:
        MESSAGE "favor digite la fecha del primer pago !" VIEW-AS ALERT-BOX INFO BUTTON OK.
         RETURN NO-APPLY.
        END.
      ELSE DO: 
          RUN periodos1.
        IF finicio >= TODAY THEN DO:           
          RUN Abrir_Excel.          
               IF SwExiste EQ ?  THEN 
                 RETURN NO-APPLY.
               ELSE  DO:
                  /* MESSAGE  Finicio "--" ndias "---" Finicio VIEW-AS ALERT-BOX INFO BUTTON OK.*/
                   w_fectra=finicio. 
                   IF tcuota =1 THEN w_fectra = w-fecentLib.
                   ELSE
                   DO indi= 1 TO (tcuota) BY 1:
                    RUN Halla_FecVcto.R (INPUT Finicio,ndias,W_Fectra,OUTPUT W_FecTra).
                      END.
                       RUN Imprime_Pagare. 
            /*  RUN Cerrar_Excel.*/
                   END.
            
        END.
        ELSE DO:
             DISABLE BUTTON-210.
            MESSAGE "La Fecha de Inicio del Plan... menor a la de Hoy !"  "Fecha Inicio:" finicio "-  Fecha hoy :" TODAY  VIEW-AS ALERT-BOX INFO BUTTON OK.
            RETURN NO-APPLY.
        END.

      END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-211
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-211 wWin
ON CHOOSE OF BUTTON-211 IN FRAME F_Solicitud /* Parcial */
DO:
      InputFile = "formatos\CC - 314.xls".
     ptipo = 2.
    RUN Abrir_Excel.
      IF SwExiste EQ ?  THEN 
      RETURN NO-APPLY.
  ELSE
  RUN Imprime_Pagare.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-212
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-212 wWin
ON CHOOSE OF BUTTON-212 IN FRAME F_Solicitud /* Blanco */
DO:
      InputFile = "formatos\CC - 314.xls".
     ptipo = 3. 
    RUN Abrir_Excel.
      IF SwExiste EQ ?  THEN 
      RETURN NO-APPLY.
  ELSE       
    RUN Imprime_Pagare.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-231
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-231 wWin
ON CHOOSE OF BUTTON-231 IN FRAME F_Solicitud /* Comp_Créd */
DO:
      InputFile = "formatos\CC - 309.xls".
    RUN Abrir_Excel.
      IF SwExiste EQ ?  THEN 
      RETURN NO-APPLY.
  ELSE
  RUN Imprime_compcredito.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-232
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-232 wWin
ON CHOOSE OF BUTTON-232 IN FRAME F_Solicitud /* Carta_Instr */
DO:

      InputFile = "formatos\CC - 307.xls".
    RUN Abrir_Excel.
      IF SwExiste EQ ?  THEN 
      RETURN NO-APPLY.
  ELSE
  RUN Imprime_autorizacion.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-233
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-233 wWin
ON CHOOSE OF BUTTON-233 IN FRAME F_Solicitud /* Deb_Autom */
DO:
  DO:
      InputFile = "formatos\AV - 307.xls".
    RUN Abrir_Excel.
      IF SwExiste EQ ?  THEN 
      RETURN NO-APPLY.
  ELSE
  RUN Imprime_DebitoA.

END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Scoring
&Scoped-define SELF-NAME BUTTON-99
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-99 wWin
ON CHOOSE OF BUTTON-99 IN FRAME F_Scoring /* Button 99 */
DO:
  ENABLE ALL WITH FRAME F_Creditos.
  DISABLE NomUsuario WITH FRAME F_Creditos.
  HIDE FRAME F_Scoring.  
  FRAME F_Consulta:HIDDEN = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Solicitud
&Scoped-define SELF-NAME Cmb_Agencias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Agencias wWin
ON LEAVE OF Cmb_Agencias IN FRAME F_Solicitud /* Agencia */
DO:
    IF W_Nuevo THEN DO:
        FIND FIRST Cfg_Instancias WHERE Cfg_instancias.Tipo_Instancia = 1
                                    AND Cfg_Instancias.Agencia = INTEGER(SUBSTRING(SELF:SCREEN-VALUE IN FRAME F_Solicitud,1,3))
                                    AND Cfg_Instancias.Instancia = W_Primera
                                    AND Cfg_Instancias.Usuario = W_Usuario
                                    AND Cfg_Instancias.Estado = 1 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Cfg_instancias THEN DO:
            MESSAGE "El usuario no tiene configurada la instancia" SKIP(1)
                    Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos SKIP(1)
                    "En la Agencia: " SELF:SCREEN-VALUE
                VIEW-AS ALERT-BOX ERROR.

            W_Nuevo = NO.
            HIDE FRAME F_Solicitud.
            APPLY "choose" TO Btn_Consulta IN FRAME F_Creditos.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Condicionada
&Scoped-define SELF-NAME Cmb_InsCon
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_InsCon wWin
ON VALUE-CHANGED OF Cmb_InsCon IN FRAME F_Condicionada /* Instancias */
DO:
    RUN Usuarios_X_Instancia.
    OPEN QUERY Br_Usuarios FOR EACH Tuxi NO-LOCK INDEXED-REPOSITION.
    APPLY "entry" TO Cmb_InsCon IN FRAME F_Condicionada.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Creditos
&Scoped-define SELF-NAME Cmb_Instancias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Instancias wWin
ON VALUE-CHANGED OF Cmb_Instancias IN FRAME F_Creditos /* Instancias */
DO:
  FIND Instancias WHERE Instancias.Instancia EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) NO-LOCK NO-ERROR.
  IF AVAILABLE Instancias THEN 
     ASSIGN W_VigIns = Instancias.TMI
            VG_Normal:SCREEN-VALUE IN FRAME F_Consulta  = "Prioridad Normal hasta: " + STRING(Instancias.TMI / 2) + " Dias"
            VG_Media:SCREEN-VALUE IN FRAME F_Consulta   = "Prioridad Media desde: " + STRING(Instancias.TMI / 2) + " Hasta : "
                                                          + STRING(Instancias.TMI) + " Días"
            VG_Alta:SCREEN-VALUE IN FRAME F_Consulta  = "Prioridad Alta desde: " + STRING(Instancias.TMI + 1) + " Días".
 
  RUN Creditos_X_Instancia.
  HIDE FRAME F_Solicitud.
  VIEW FRAME F_Consulta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Solicitud
&Scoped-define SELF-NAME Cmb_PerPago
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_PerPago wWin
ON VALUE-CHANGED OF Cmb_PerPago IN FRAME F_Solicitud /* Período */
DO:
DO WITH FRAME F_Solicitud:
/*  Solicitud.Cuota:SCREEN-VALUE = "0".
  ENABLE Btn_Liquidar.
  IF Solicitud.Plazo:SCREEN-VALUE NE "0" THEN DO:
     CASE INTEGER(SUBSTRING(Cmb_PerPago:SCREEN-VALUE,1,1)):
        WHEN 1 THEN Solicitud.Plazo:SCREEN-VALUE = STRING(ABS(Dias / 7)).
        WHEN 3 THEN Solicitud.Plazo:SCREEN-VALUE = STRING(ABS(Dias / 15)).
        WHEN 4 THEN Solicitud.Plazo:SCREEN-VALUE = STRING(ABS(Dias / 30)).
     END CASE.
  END.
  APPLY "leave" TO Solicitud.Plazo.*/
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Sistemas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Sistemas wWin
ON VALUE-CHANGED OF Cmb_Sistemas IN FRAME F_Solicitud /* Sistema */
DO:
DO WITH FRAME F_Solicitud:
 /* Solicitud.Cuota:SCREEN-VALUE IN FRAME F_Solicitud = "0".
  ENABLE Btn_Liquidar WITH FRAME F_Solicitud.
  DEFINE VARIABLE Sistema AS INTEGER FORMAT "99999".
  Sistema = INTEGER(SUBSTRING(Cmb_sistemas:SCREEN-VALUE,1,5)).
  IF Sistema GT 4 AND Sistema LT 9 THEN DO:
     ASSIGN Solicitud.Incremento:SENSITIVE   = YES
            Solicitud.Incremento:BGCOL       = 15
            Solicitud.Incremento:FGCOL       = 0
            Solicitud.Incremento:HIDDEN      = NO.
     IF Sistema EQ 5 THEN Solicitud.Incremento:LABEL = "Valor Incremento".
     IF Sistema EQ 6 OR Sistema EQ 7 THEN Solicitud.Incremento:LABEL = "Porcentaje Incremento".
     IF Sistema EQ 8 THEN Solicitud.Incremento:LABEL = "Periodo Gracia".
  END.
  ELSE DO:
    ASSIGN Solicitud.Incremento:SCREEN-VALUE = "0"
           Solicitud.Incremento:BGCOL        = 17
           Solicitud.Incremento:FGCOL        = 17
           Solicitud.Incremento:HIDDEN       = YES
           Solicitud.Incremento:SENSITIVE    = NO.
  END.
  APPLY "leave" TO Solicitud.Plazo IN FRAME F_Solicitud.*/
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Creditos
&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame wWin OCX.Tick
PROCEDURE CtrlFrame.PSTimer.Tick .
IF T_Refresh:SCREEN-VALUE IN FRAME F_Creditos EQ "YES" THEN DO:
  IF FRAME F_Consulta:HIDDEN EQ NO THEN
     APPLY "value-changed" TO Cmb_Instancias IN FRAME F_Creditos.
END.
  

/*IF FRAME F_Consulta:HIDDEN EQ NO THEN
    APPLY "value-changed" TO Cmb_Instancias IN FRAME F_Creditos.*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Admisible
&Scoped-define SELF-NAME Garantias.Descripcion_Bien
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Descripcion_Bien wWin
ON VALUE-CHANGED OF Garantias.Descripcion_Bien IN FRAME F_Admisible /* Descripcion_Bien */
DO:
  ENABLE Btn_SalAdm WITH FRAME F_Admisible.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Garantias.Descripcion_Bien2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Descripcion_Bien2 wWin
ON VALUE-CHANGED OF Garantias.Descripcion_Bien2 IN FRAME F_Admisible /* Descripcion_Bien2 */
DO:
  ENABLE Btn_SalAdm WITH FRAME F_Admisible. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ultima
&Scoped-define SELF-NAME Creditos.Desembolso
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Creditos.Desembolso wWin
ON MOUSE-SELECT-CLICK OF Creditos.Desembolso IN FRAME F_Ultima /* Tipo de Desembolso */
DO:
  ASSIGN Creditos.Desembolso
         W_CtaTerceros:SENSITIVE  = FALSE
         W_VrAlAhorro:SENSITIVE   = FALSE
         W_NetoDesemb:SENSITIVE   = FALSE
         FRAME F_Bancos:SENSITIVE = FALSE
         W_IntAntic:SENSITIVE     = FALSE.

  IF Creditos.Desembolso EQ 3 OR Creditos.Desembolso EQ 4 THEN DO:
    /* ASSIGN Creditos.Desembolso:SCREEN-VALUE = "8"
            Creditos.Desembolso.
     RETURN.*/
  END.

  IF Creditos.Desembolso EQ 8 THEN DO:
     /*MESSAGE "Seleccione una opcion correcta."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
     RETURN.
  END.

  IF Creditos.Desembolso EQ 1 THEN 
     ASSIGN W_VrEfectivo              = W_NetoDesemb
            W_VrEfectivo:SCREEN-VALUE = STRING(W_NetoDesemb)
            W_VrEfectivo:SENSITIVE    = FALSE
            W_VrCheque                = 0
            W_VrCheque:SCREEN-VALUE   = "0"
            W_VrCheque:SENSITIVE      = FALSE
            W_VrAlAhorro              = 0
            W_VrAlAhorro:SCREEN-VALUE = "0".
  ELSE IF Creditos.Desembolso EQ 2 THEN
     ASSIGN W_VrEfectivo              = 0
            W_VrEfectivo:SCREEN-VALUE = "0"
            W_VrEfectivo:SENSITIVE    = FALSE
            W_VrCheque                = W_NetoDesemb        
            W_VrCheque:SCREEN-VALUE   = STRING(W_NetoDesemb)
            W_VrCheque:SENSITIVE      = FALSE
            W_VrAlAhorro              = 0
            W_VrAlAhorro:SCREEN-VALUE = "0".
  ELSE IF Creditos.Desembolso = 9 THEN
     ASSIGN W_VrEfectivo              = 0
            W_VrEfectivo:SCREEN-VALUE = "0"
            W_VrEfectivo:SENSITIVE    = FALSE
            W_VrCheque                = W_NetoDesemb        
            W_VrCheque:SCREEN-VALUE   = STRING(W_NetoDesemb)
            W_VrCheque:SENSITIVE      = FALSE
            W_VrAlAhorro              = 0
            W_VrAlAhorro:SCREEN-VALUE = "0".
  ELSE IF Creditos.Desembolso EQ 3 THEN
     ASSIGN W_VrEfectivo              = 0
            W_VrEfectivo:SCREEN-VALUE = "0"
            W_VrEfectivo:SENSITIVE    = FALSE
            W_VrCheque                = 0
            W_VrCheque:SCREEN-VALUE   = "0"
            W_VrCheque:SENSITIVE      = FALSE
            W_VrAlAhorro              = W_NetoDesemb        
            W_VrAlAhorro:SCREEN-VALUE = STRING(W_NetoDesemb).
  ELSE IF Creditos.Desembolso EQ 4 THEN
     ASSIGN W_VrEfectivo              = 0
            W_VrEfectivo:SCREEN-VALUE = "0"
            W_VrEfectivo:SENSITIVE    = FALSE
            W_VrCheque                = 0
            W_VrCheque:SCREEN-VALUE   = "0"
            W_VrCheque:SENSITIVE      = FALSE
            W_VrAlAhorro              = 0        
            W_VrAlAhorro:SCREEN-VALUE = "0"
            W_CtaTerceros:SENSITIVE   = TRUE.
  ELSE IF Creditos.Desembolso EQ 5 OR Creditos.Desembolso EQ 6 THEN
     ASSIGN W_VrEfectivo              = W_NetoDesemb        
            W_VrEfectivo:SCREEN-VALUE = STRING(W_NetoDesemb)
            W_VrEfectivo:SENSITIVE    = TRUE
            W_VrCheque                = 0
            W_VrCheque:SCREEN-VALUE   = "0"
            W_VrCheque:SENSITIVE      = FALSE
            W_VrAlAhorro              = 0        
            W_VrAlAhorro:SCREEN-VALUE = "0".
  ELSE IF Creditos.Desembolso EQ 7 THEN
     ASSIGN W_VrEfectivo              = 0
            W_VrEfectivo:SCREEN-VALUE = "0"
            W_VrEfectivo:SENSITIVE    = FALSE
            W_VrCheque                = W_NetoDesemb        
            W_VrCheque:SCREEN-VALUE   = STRING(W_NetoDesemb)
            W_VrCheque:SENSITIVE      = TRUE
            W_VrAlAhorro              = 0        
            W_VrAlAhorro:SCREEN-VALUE = "0".

  IF Creditos.Desembolso EQ 3 OR (Creditos.Desembolso GE 6 AND creditos.desembolso <> 9) THEN DO:
     ASSIGN WWin:SENSITIVE = FALSE.
     RUN C-Ahorros.r (INPUT Creditos.Nit, OUTPUT A_Age, OUTPUT A_Pro, OUTPUT A_NitW, OUTPUT A_Cue).
     ASSIGN WWin:SENSITIVE = TRUE.
     WWin:MOVE-TO-TOP().

     FIND Ahorros WHERE Ahorros.Agencia       EQ A_Age  AND
                         Ahorros.Cod_Ahorro   EQ A_Pro  AND
                         Ahorros.Nit          EQ A_NitW AND
                         Ahorros.Cue_Ahorros  EQ A_Cue NO-LOCK NO-ERROR.
     IF AVAILABLE Ahorros AND Ahorros.Tip_Ahorro NE 1 THEN DO:
        MESSAGE "La cuenta seleccionada no es de tipo A la Vista, Escoja una de este" SKIP
                "Tipo para hacer el Desembolso a la Cta-Ahorros!" VIEW-AS ALERT-BOX WARNING.
        ASSIGN Creditos.Desembolso:SCREEN-VALUE = "8"
               Creditos.Desembolso.
     END.
     ELSE DO:
        IF AVAILABLE Ahorros THEN
           ASSIGN Creditos.Age_Desembolso = A_Age
                  Creditos.Cod_Desembolso = A_Pro
                  Creditos.Cue_Desembolso = A_Cue.
        ELSE 
           ASSIGN Creditos.Age_Desembolso = 0 
                  Creditos.Cod_Desembolso = 0 
                  Creditos.Cue_Desembolso = "".
     END.
  END.
  
  IF Creditos.Desembolso EQ 2 OR Creditos.Desembolso EQ 5 OR Creditos.Desembolso EQ 7 THEN DO:
     ASSIGN Nom_Beneficiario:SCREEN-VALUE IN FRAME F_Bancos = NomNit:SCREEN-VALUE IN FRAME F_Solicitud
            FRAME F_Bancos:SENSITIVE                        = TRUE.
     IF Creditos.Desembolso EQ 2 THEN 
        ASSIGN Num_Cheque:SCREEN-VALUE = "Transf".
     ELSE 
         ASSIGN Num_Cheque:SCREEN-VALUE = "".
     APPLY "ENTRY" TO Num_Cheque.     
  END.

  IF Creditos.Desembolso = 9 THEN DO:
      FRAME F_Bancos:SENSITIVE = TRUE.
      Num_Cheque:SCREEN-VALUE = "TRANSF".
      num_cheque:SENSITIVE = FALSE.
  END.
  ELSE
      num_cheque:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Creditos.Desembolso wWin
ON VALUE-CHANGED OF Creditos.Desembolso IN FRAME F_Ultima /* Tipo de Desembolso */
DO:
  APPLY "Mouse-Select-Click" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Creditos.Estado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Creditos.Estado wWin
ON MOUSE-SELECT-CLICK OF Creditos.Estado IN FRAME F_Ultima /* Estado */
DO:
  APPLY "Value-Changed" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Creditos.Estado wWin
ON VALUE-CHANGED OF Creditos.Estado IN FRAME F_Ultima /* Estado */
DO:
  CASE SELF:SCREEN-VALUE:
    WHEN "1" THEN DO:
      Creditos.Pagare:SCREEN-VALUE = "".
      DISABLE Creditos.Pagare WITH FRAME F_Ultima.
      W_MenDes:SCREEN-VALUE = "El Crédito se encuentra pendiente de desembolso".
    END.
    WHEN "2" THEN DO:
       ASSIGN Creditos.Pagare:SCREEN-VALUE = STRING(Creditos.Num_Credito)
              W_MenDes:SCREEN-VALUE        = "El Crèdito se Desembolsarà con este No.de Pagarè".
        
       IF Creditos.Desembolso EQ 3 THEN
          APPLY "ENTRY" TO W_CtaTerceros.
    END.
    WHEN "4" THEN DO:
      ASSIGN W_CtaTerceros:SCREEN-VALUE   = ""
             W_CtaTerceros
             Creditos.Pagare:SCREEN-VALUE = "".
      DISABLE Creditos.Pagare WITH FRAME F_Ultima.
      W_MenDes:SCREEN-VALUE = "El Crédito quedará permanentemente Retirado, no se desembolsa!".
    END.
  END CASE.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Formalizar
&Scoped-define SELF-NAME Creditos.Estado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Creditos.Estado wWin
ON MOUSE-SELECT-CLICK OF Creditos.Estado IN FRAME F_Formalizar /* Estado */
DO:
  APPLY "Value-Changed" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Creditos.Estado wWin
ON VALUE-CHANGED OF Creditos.Estado IN FRAME F_Formalizar /* Estado */
DO:
  CASE SELF:SCREEN-VALUE:
    WHEN "1" THEN DO:
      Creditos.Pagare:SCREEN-VALUE = "".
      DISABLE Creditos.Pagare WITH FRAME F_Ultima.
      W_MenDes:SCREEN-VALUE = "El Crédito se encuentra pendiente de desembolso".
    END.
    WHEN "2" THEN DO:
       ASSIGN Creditos.Pagare:SCREEN-VALUE = STRING(Creditos.Num_Credito)
              W_MenDes:SCREEN-VALUE        = "El Crèdito se Formalizarà con este No.de Pagarè".
        
       IF Creditos.Desembolso EQ 3 THEN
          APPLY "ENTRY" TO W_CtaTerceros.

       /*ENABLE Creditos.Pagare WITH FRAME F_Ultima.
       APPLY "entry" TO Creditos.Pagare IN FRAME F_Ultima.*/
    END.
    WHEN "4" THEN DO:
      ASSIGN W_CtaTerceros:SCREEN-VALUE   = ""
             W_CtaTerceros
             Creditos.Pagare:SCREEN-VALUE = "".
      DISABLE Creditos.Pagare WITH FRAME F_Ultima.
      W_MenDes:SCREEN-VALUE = "El Crédito quedará permanentemente Retirado, no se desembolsa!".
    END.
  END CASE.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Instancias
&Scoped-define SELF-NAME Mov_Instancias.Estado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Mov_Instancias.Estado wWin
ON VALUE-CHANGED OF Mov_Instancias.Estado IN FRAME F_Instancias /* Estado */
OR ANY-KEY OF SELF DO:
  APPLY "choose" TO Btn_AgregarTXT IN FRAME F_Instancias.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Admisible
&Scoped-define SELF-NAME Garantias.Fec_FinSeguro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Fec_FinSeguro wWin
ON LEAVE OF Garantias.Fec_FinSeguro IN FRAME F_Admisible /* Fecha Vencimiento */
DO:
  IF DATE(SELF:SCREEN-VALUE) LT DATE(Garantias.Fec_IniSeguro:SCREEN-VALUE) AND
     DATE(Fec_IniSeguro:SCREEN-VALUE) NE ? THEN DO:
     MESSAGE "La fecha de vencimiento del seguro no puede" SKIP
             "ser menor a la fecha de inicio del seguro" VIEW-AS ALERT-BOX ERROR.
     
     APPLY "entry" TO Garantias.Fec_FinSeguro.
     RETURN.
  END.

  IF DATE(SELF:SCREEN-VALUE) LT W_fecha THEN DO:
     MESSAGE "La fecha de vencimiento del seguro no puede estar vencida..." SKIP
             VIEW-AS ALERT-BOX ERROR.
     
     APPLY "entry" TO Garantias.Fec_FinSeguro.
     RETURN.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Fec_FinSeguro wWin
ON VALUE-CHANGED OF Garantias.Fec_FinSeguro IN FRAME F_Admisible /* Fecha Vencimiento */
DO:
  ENABLE Btn_SalAdm WITH FRAME F_Admisible.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Garantias.Fec_IniSeguro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Fec_IniSeguro wWin
ON VALUE-CHANGED OF Garantias.Fec_IniSeguro IN FRAME F_Admisible /* Fecha Inicio Seguro */
DO:
  ENABLE Btn_SalAdm WITH FRAME F_Admisible.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Garantias.Fec_ProxAvaluo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Fec_ProxAvaluo wWin
ON LEAVE OF Garantias.Fec_ProxAvaluo IN FRAME F_Admisible /* Fec.Próx.Avaluo */
DO:
  IF DATE(SELF:SCREEN-VALUE) LT DATE(Garantias.Fec_UltAvaluo:SCREEN-VALUE) AND
     DATE(Fec_UltAvaluo:SCREEN-VALUE) NE ? THEN DO:
     MESSAGE "La fecha de Proximo Avaluo no puede" SKIP
             "ser menor a la fecha de último avaluo" VIEW-AS ALERT-BOX WARNING.
     
     APPLY "entry" TO Garantias.Fec_ProxAvaluo.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Fec_ProxAvaluo wWin
ON VALUE-CHANGED OF Garantias.Fec_ProxAvaluo IN FRAME F_Admisible /* Fec.Próx.Avaluo */
DO:
    ENABLE Btn_SalAdm WITH FRAME F_Admisible. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Garantias.Fec_UltAvaluo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Fec_UltAvaluo wWin
ON LEAVE OF Garantias.Fec_UltAvaluo IN FRAME F_Admisible /* Fec.Últ.Avaluo */
DO:
  IF DATE(SELF:SCREEN-VALUE) GT DATE(Garantias.Fec_ProxAvaluo:SCREEN-VALUE) AND
     DATE(Fec_ProxAvaluo:SCREEN-VALUE) NE ? THEN DO:
     MESSAGE "La fecha de Proximo Avaluo no puede" SKIP
             "ser menor a la fecha de último avaluo" VIEW-AS ALERT-BOX WARNING.
     
     APPLY "entry" TO Garantias.Fec_ProxAvaluo.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Fec_UltAvaluo wWin
ON VALUE-CHANGED OF Garantias.Fec_UltAvaluo IN FRAME F_Admisible /* Fec.Últ.Avaluo */
DO:
    ENABLE Btn_SalAdm WITH FRAME F_Admisible. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Garantias.Fec_VctoImpuesto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Fec_VctoImpuesto wWin
ON VALUE-CHANGED OF Garantias.Fec_VctoImpuesto IN FRAME F_Admisible /* Fec.Vencimiento */
DO:
    ENABLE Btn_SalAdm WITH FRAME F_Admisible. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Garantias.Identificacion_Bien
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Identificacion_Bien wWin
ON LEAVE OF Garantias.Identificacion_Bien IN FRAME F_Admisible /* Identificación del Bien */
DO:
    IF Garantias.Tipo_Garantia:SCREEN-VALUE EQ "4"  THEN DO:
       MESSAGE "Seleccione El Cdat/Contractual de Garantia."  
           VIEW-AS ALERT-BOX INFO BUTTONS OK.

       FRAME F_Creditos:SENSITIVE = FALSE.
       RUN C-Ahorros.r (INPUT Creditos.Nit, OUTPUT A_Age, OUTPUT A_Pro, OUTPUT A_NitW, OUTPUT A_Cue).
       FIND Ahorros WHERE Ahorros.Agencia      EQ A_Age  AND
                          Ahorros.Cod_Ahorro   EQ A_Pro  AND
                          Ahorros.Nit          EQ A_NitW AND
                          Ahorros.Cue_Ahorros  EQ A_Cue  AND
                          Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje GT 0 NO-LOCK NO-ERROR.
       FRAME F_Creditos:SENSITIVE = TRUE.

       IF (AVAILABLE(Ahorros) AND (Ahorros.Tip_Ahorro EQ 1 OR Ahorros.Tip_Ahorro EQ 4))
       OR NOT AVAILABLE(Ahorros) THEN DO:
          MESSAGE "La cuenta seleccionada Debe ser Cdat/Contractual y Activa," SKIP
                  "Escoja una cuenta de este Tipo para la Garantia!" VIEW-AS ALERT-BOX WARNING.
          ASSIGN Garantias.Identificacion_Bien:SCREEN-VALUE = ""
                 Garantias.Val_Bien:SCREEN-VALUE            = "0".
       END.
       ELSE 
          ASSIGN Garantias.Identificacion_Bien:SCREEN-VALUE = Ahorros.Cue_Ahorros
                 Garantias.Val_Bien:SCREEN-VALUE            = STRING(Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje)
                 Garantias.Nom_Bien:SCREEN-VALUE  = "Cod.Ahorro " + STRING(Ahorros.Cod_Ahorro,"99") +
                                                    " del Asociado con Ced/Nit : " + A_NitW
                 Garantias.Fec_FinSeguro:SCREEN-VALUE       = STRING(Ahorros.Fec_Vencimiento)
                 Garantias.Nit_Aseguradora:SCREEN-VALUE     = Ahorros.Nit.      
    END.

    RUN Halla_DispGaran.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Solicitud
&Scoped-define SELF-NAME Creditos.Id_Adicionales
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Creditos.Id_Adicionales wWin
ON VALUE-CHANGED OF Creditos.Id_Adicionales IN FRAME F_Solicitud /* Deducciones */
DO:
/*DO WITH FRAME F_Solicitud:
  CASE SELF:SCREEN-VALUE:
    WHEN "1" THEN Creditos.Sdo_Capital:SCREEN-VALUE = STRING(DECIMAL(Creditos.Monto:SCREEN-VALUE) + DECIMAL(Creditos.Deducible:SCREEN-VALUE)).
    WHEN "2" THEN Creditos.Sdo_Capital:SCREEN-VALUE = Creditos.Monto:SCREEN-VALUE.
    WHEN "3" THEN Creditos.Sdo_Capital:SCREEN-VALUE = Creditos.Monto:SCREEN-VALUE.
  END CASE.
END.  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Creditos.Monto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Creditos.Monto wWin
ON LEAVE OF Creditos.Monto IN FRAME F_Solicitud /* Monto de Credito */
DO:
    DO WITH FRAME F_Solicitud:
        RUN Deducibles_Producto.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ultima
&Scoped-define SELF-NAME Creditos.Monto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Creditos.Monto wWin
ON LEAVE OF Creditos.Monto IN FRAME F_Ultima /* Monto del Credito */
DO:
   DO WITH FRAME F_Solicitud:
      RUN Deducibles_Producto.
      RUN Multiplicador_Ahorros.   /*No está operando, està comentariado*/
   END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Admisible
&Scoped-define SELF-NAME Garantias.Nit_Aseguradora
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Nit_Aseguradora wWin
ON LEAVE OF Garantias.Nit_Aseguradora IN FRAME F_Admisible /* Nit Aseguradora */
DO:
DO WITH FRAME F_Admisible:
   IF SELF:SCREEN-VALUE EQ Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud THEN DO:
      MESSAGE "No puede ser codeudor de si mismo" SKIP
              "Rectifique el nit del codeudor" SKIP
              "para el cliente: " Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud
              VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO SELF.
      RETURN NO-APPLY.
   END.
   FIND Clientes WHERE Clientes.Nit EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
   IF NOT AVAILABLE(Clientes) THEN DO:
      RUN C-Clientes.R(INPUT 1, INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_AgeCli).
      ASSIGN Nom_Aseguradora:SCREEN-VALUE = P_Nombre + " " + P_Apellido
             SELF:SCREEN-VALUE = P_Nit.
   END.
   ELSE
      ASSIGN Nom_Aseguradora:SCREEN-VALUE = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Nit_Aseguradora wWin
ON VALUE-CHANGED OF Garantias.Nit_Aseguradora IN FRAME F_Admisible /* Nit Aseguradora */
DO:
  ENABLE Btn_SalAdm WITH FRAME F_Admisible.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Bancos
&Scoped-define SELF-NAME Nom_Beneficiario
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Nom_Beneficiario wWin
ON ENTRY OF Nom_Beneficiario IN FRAME F_Bancos /* Nombre del Beneficiario */
DO:
  DO WITH FRAME Frame_NroCheque.
     ASSIGN Num_Cheque.
     IF Num_Cheque EQ "" THEN DO:
        APPLY "ENTRY":U TO Num_Cheque.
        RETURN NO-APPLY.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Nom_Beneficiario wWin
ON LEAVE OF Nom_Beneficiario IN FRAME F_Bancos /* Nombre del Beneficiario */
DO:
  DO WITH FRAME F_Bancos:
     ASSIGN Nom_Beneficiario = CAPS(Nom_Beneficiario:SCREEN-VALUE).
     IF Nom_Beneficiario LE "" THEN DO:
        MESSAGE "Se debe digitar el nombre del beneficiario"
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
        TITLE "Validación del Cheque.".
        APPLY "ENTRY":U TO Num_Cheque.
        RETURN NO-APPLY.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Admisible
&Scoped-define SELF-NAME Garantias.Nom_Impuesto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Nom_Impuesto wWin
ON VALUE-CHANGED OF Garantias.Nom_Impuesto IN FRAME F_Admisible /* Nombre */
DO:
    ENABLE Btn_SalAdm WITH FRAME F_Admisible. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Garantias.Nro_Seguro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Nro_Seguro wWin
ON VALUE-CHANGED OF Garantias.Nro_Seguro IN FRAME F_Admisible /* Número de Seguro */
DO:
  ENABLE Btn_SalAdm WITH FRAME F_Admisible.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Bancos
&Scoped-define SELF-NAME Num_Cheque
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Num_Cheque wWin
ON LEAVE OF Num_Cheque IN FRAME F_Bancos /* Número de Cheque */
DO:
  ASSIGN FRAME F_Bancos Num_Cheque Nom_Beneficiario.

  IF Nom_Beneficiario LE "" OR Num_Cheque LE "00000" THEN 
     MESSAGE "Se debe digitar No.Cheque y el nombre del beneficiario"
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
     
  IF Bancos:NUM-SELECTED-ROWS NE 0 AND Cuentas.Cuenta NE "?" AND Cuentas.Cuenta GT " " THEN
     ASSIGN W_CtaBanco = Cuentas.Cuenta.
  ELSE
     MESSAGE "Aunque el desembolso se debe hacer por cheque" SKIP
             "no se ha escogido un banco o digitado el número" SKIP
             "de cheque o digitado el nombre del beneficiario" SKIP(1)
             "Corrija por favor..." VIEW-AS ALERT-BOX ERROR.     
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Num_Cheque wWin
ON TAB OF Num_Cheque IN FRAME F_Bancos /* Número de Cheque */
DO:
  APPLY "entry" TO Nom_Beneficiario IN FRAME F_Bancos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_HojaVida
&Scoped-define SELF-NAME Hoja_Vida.Observacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Hoja_Vida.Observacion wWin
ON ENTRY OF Hoja_Vida.Observacion IN FRAME F_HojaVida /* Observación */
DO:
  longitud = SELF:LENGTH.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Hoja_Vida.Observacion wWin
ON LEAVE OF Hoja_Vida.Observacion IN FRAME F_HojaVida /* Observación */
DO:
  IF Longitud NE SELF:LENGTH THEN ENABLE Btn_SalvaHV WITH FRAME F_HojaVida.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Hoja_Vida.Observacion wWin
ON MOUSE-SELECT-CLICK OF Hoja_Vida.Observacion IN FRAME F_HojaVida /* Observación */
DO:
  IF NOT W_NvaHV THEN DO:
     Id_Agregar = "HV".
     E_Agregar:SCREEN-VALUE IN FRAME F_Agregar = "".
     MESSAGE "No se permite el cambio del texto seleccionado" SKIP
             "Desea agregar algun mensaje de texto al asunto?"
             VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO UPDATE choice.
     IF choice THEN DO:
        ENABLE ALL WITH FRAME F_Agregar.
        VIEW FRAME F_Agregar.
        APPLY "entry" TO E_Agregar IN FRAME F_Agregar.
        RETURN NO-APPLY.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Hoja_Vida.Observacion wWin
ON VALUE-CHANGED OF Hoja_Vida.Observacion IN FRAME F_HojaVida /* Observación */
DO:
  ENABLE Btn_SalvaHV WITH FRAME F_HojaVida.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Solicitud
&Scoped-define SELF-NAME Creditos.Plazo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Creditos.Plazo wWin
ON LEAVE OF Creditos.Plazo IN FRAME F_Solicitud /* Plazo */
DO:
    DEFINE VAR WMon AS DECIMAL.
    DEFINE VAR WMpr AS DECIMAL.
    DEFINE VAR MultEdad AS INTEGER.
    DEFINE VAR W_RowidC AS ROWID.
    DEFINE VAR P_ForPag AS INTEGER.
    DEFINE VAR P_CedNit AS CHARACTER.
    DEFINE VAR zrowid AS ROWID.
    DEFINE VAR j AS INTEGER.

    Dias = DECIMAL(SELF:SCREEN-VALUE).

    IF Creditos.Per_Pago = 0 THEN ASSIGN Dias = Dias * 1.
    IF Creditos.Per_Pago = 1 THEN ASSIGN Dias = Dias * 7.
    IF Creditos.Per_Pago = 2 THEN ASSIGN Dias = Dias * 10.
    IF Creditos.Per_Pago = 3 THEN ASSIGN Dias = Dias * 15.
    IF Creditos.Per_Pago = 4 THEN ASSIGN Dias = Dias * 30.
    IF Creditos.Per_Pago = 5 THEN ASSIGN Dias = Dias * 60.
    IF Creditos.Per_Pago = 6 THEN ASSIGN Dias = Dias * 90.
    IF Creditos.Per_Pago = 7 THEN ASSIGN Dias = Dias * 120.
    IF Creditos.Per_Pago = 8 THEN ASSIGN Dias = Dias * 180.
    IF Creditos.Per_Pago = 9 THEN ASSIGN Dias = Dias * 360.

    IF Creditos.Plazo:SCREEN-VALUE <> "0" THEN
        RUN Buscar_Indicadores.

    W_VrADesemb = Creditos.Monto - (WDed + W_VrCredACanc +  W_FaltApor).
    W_VrADesemb:SCREEN-VALUE = STRING(W_VrADesemb).

    ASSIGN Ded_Ahorros:SCREEN-VALUE = "0"
           w_VrCredAcanc = 0.

    W_RowidC = ROWID(Creditos).

    FOR EACH solicitudes_pagoCreditos WHERE solicitudes_pagoCreditos.cliente_id = solicitud.nit
                                        AND solicitudes_pagoCreditos.num_solicitud = solicitud.num_solicitud NO-LOCK:
        FIND FIRST Creditos WHERE Creditos.Nit = solicitudes_pagoCreditos.cliente_id
                              AND Creditos.Num_Credito = solicitudes_pagoCreditos.num_credito
                              AND Creditos.Estado = 2 NO-LOCK NO-ERROR.
        IF AVAILABLE creditos THEN DO:
            IF solicitudes_pagoCreditos.pagoTotal = TRUE THEN
                W_VrCredACanc = W_VrCredACanc + Creditos.Sdo_Capital + Creditos.Int_Corrientes + Creditos.Int_DifCobro + Creditos.Int_MoraDifCob + Creditos.Int_MorCobrar + Creditos.Costas + Creditos.Polizas + Creditos.Honorarios - Creditos.Int_Anticipado.
            ELSE DO:
                IF solicitudes_pagoCreditos.valorAbono < Creditos.Sdo_Capital + Creditos.Int_Corrientes + Creditos.Int_DifCobro + Creditos.Int_MoraDifCob + Creditos.Int_MorCobrar + Creditos.Costas + Creditos.Polizas + Creditos.Honorarios - Creditos.Int_Anticipado THEN
                    W_VrCredACanc = W_VrCredACanc + solicitudes_pagoCreditos.valorAbono.
                ELSE
                    W_VrCredACanc = W_VrCredACanc + Creditos.Sdo_Capital + Creditos.Int_Corrientes + Creditos.Int_DifCobro + Creditos.Int_MoraDifCob + Creditos.Int_MorCobrar + Creditos.Costas + Creditos.Polizas + Creditos.Honorarios - Creditos.Int_Anticipado.
            END.

            W_VrCredACanc:SCREEN-VALUE = STRING(W_VrCredACanc).
        END.
    END.

    FIND FIRST creditos WHERE ROWID(creditos) = W_RowidC EXCLUSIVE-LOCK.

    ASSIGN WMpr = 0
           Wded = 0
           MultEdad = 1.

    IF WK_Edad >= 80 THEN
        MultEdad = 0.
    ELSE
        IF WK_Edad >= 70 THEN
            MultEdad = 2.

    WFactorCod = 1.

    FOR EACH Relaciones WHERE Relaciones.Nit = Creditos.Nit
                          AND Relaciones.Cod_Relacion = 11
                          AND Relaciones.Clase_Producto = 2
                          AND Relaciones.Cod_Producto = Creditos.cod_credito
                          AND Relaciones.Cuenta = TRIM(STRING(Creditos.num_credito))
                          AND Relaciones.estado = 1 NO-LOCK:
        WfactorCod = WfactorCod + 1.
    END.

    ASSIGN p_linea = Pro_Creditos.Cod_Credito
           p_forpag = Creditos.For_Pago
           P_CedNit = Creditos.Nit
           WDed = 0
           wvlres = 0.

    ASSIGN wnombres[1] = 'Seguro de Cartera'
           wnombres[2] = 'Gasto de Papeleria'
           wnombres[3] = 'Gasto de Administracion'
           wnombres[4] = 'Delphy '
           wnombres[5] = 'Sifin'
           wnombres[6] = 'Cuota de Afiliacion '
           wnombres[7] = 'impuesto de timbre '
           wnombres[8] = 'Normal'
           wnombres[9] = 'Estudio Credito'.

    FOR EACH TDeducc WHERE TDeducc.Cod_Deducible <> ?:
        TDeducc.Valor_impuesto = 0.

        IF TDeducc.valor > 0 THEN DO:
            IF TDeducc.Id_Deducible = 0 THEN DO: /*financiados*/
                IF TDeducc.Cla_Deducible = 1 THEN DO: /*porcentaje*/
                    ASSIGN WDed = WDed + (Creditos.Monto * TDeducc.Valor)
                           wvlres[8] = (Creditos.Monto * TDeducc.Valor)
                           TDeducc.Valor_impuesto = (Creditos.Monto * TDeducc.Valor).
                END.
                ELSE DO:
                    ASSIGN WDed = WDed + TDeducc.Valor
                           wvlres[8] = TDeducc.Valor
                           TDeducc.Valor_impuesto = TDeducc.Valor.
                END.
            END.
            ELSE DO: /*Descontados*/
                IF TDeducc.Cla_Deducible = 1 THEN /*porcentaje*/ DO:
                    ASSIGN WDed = WDed + (Creditos.Monto * TDeducc.Valor)
                           wvlres[8] = (Creditos.Monto * TDeducc.Valor)
                           TDeducc.Valor_impuesto = (Creditos.Monto * TDeducc.Valor).
                END.
                ELSE DO:
                    ASSIGN WDed = WDed + TDeducc.Valor
                           wvlres[8] = TDeducc.Valor
                           TDeducc.Valor_impuesto = TDeducc.Valor.
                END.
            END.
        END.
    END.

    ASSIGN Creditos.Deducible = WDed + W_VrCredACanc +  W_FaltApor.
           Creditos.Deducible:SCREEN-VALUE = STRING(Creditos.Deducible).

    ASSIGN FRAME F_Solicitud Ded_Ahorros W_FaltApor.

    ASSIGN Creditos.Sdo_Capital:SCREEN-VALUE = STRING(Creditos.Monto)
           W_VrADesemb = Creditos.Monto - (WDed + W_VrCredACanc +  W_FaltApor)
           W_VrADesemb:SCREEN-VALUE = STRING(W_VrADesemb).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Codeudores
&Scoped-define SELF-NAME RActivas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RActivas wWin
ON VALUE-CHANGED OF RActivas IN FRAME F_Codeudores
DO:
  IF SELF:SCREEN-VALUE EQ "1" THEN
    OPEN QUERY Br_Codeudores FOR EACH TCode WHERE 
       TCode.TC_EstRela EQ 1 NO-LOCK INDEXED-REPOSITION.
  ELSE
    OPEN QUERY Br_Codeudores FOR EACH TCode WHERE 
       TCode.TC_EstRela EQ 2 NO-LOCK INDEXED-REPOSITION.
 
 IF Br_Codeudores:NUM-SELECTED-ROWS IN FRAME F_Codeudores NE 0 THEN
    ASSIGN W_NitCodeudor:SCREEN-VALUE = TCode.TC_NitCode
           W_NomCodeudor:SCREEN-VALUE = TCode.TC_NomCode.
 ELSE
    ASSIGN W_NitCodeudor:SCREEN-VALUE = ""
           W_NomCodeudor:SCREEN-VALUE = "".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Formalizar
&Scoped-define SELF-NAME Rs_Desemb
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Rs_Desemb wWin
ON MOUSE-SELECT-CLICK OF Rs_Desemb IN FRAME F_Formalizar
DO:
    ASSIGN Rs_Desemb.

    IF Rs_Desemb EQ 1 THEN DO:
        ASSIGN WWin:SENSITIVE = FALSE.

        RUN C-Ahorros.r(INPUT Creditos.Nit,
                        OUTPUT A_Age,
                        OUTPUT A_Pro,
                        OUTPUT A_NitW,
                        OUTPUT A_Cue).

        ASSIGN WWin:SENSITIVE = TRUE.

        WWin:MOVE-TO-TOP().

        FIND Ahorros WHERE Ahorros.Agencia EQ A_Age
                       AND Ahorros.Cod_Ahorro EQ A_Pro
                       AND Ahorros.Nit EQ A_NitW
                       AND Ahorros.Cue_Ahorros EQ A_Cue NO-LOCK NO-ERROR.
        IF AVAILABLE Ahorros AND Ahorros.cod_Ahorro NE 4 THEN DO:
            MESSAGE "La cuenta seleccionada no es de tipo A la vista, Escoja una de este" SKIP
                    "Tipo para hacer el Desembolso a la Cta-Ahorros!"
                VIEW-AS ALERT-BOX WARNING.

            ASSIGN Rs_Desemb:SCREEN-VALUE = "3"
                   Rs_Desemb.
        END.
        ELSE DO:
            IF AVAILABLE Ahorros THEN
                ASSIGN Creditos.Age_Desembolso = A_Age
                       Creditos.Cod_Desembolso = A_Pro
                       Creditos.Cue_Desembolso = A_Cue
                       cta = 4.
            ELSE
                ASSIGN Creditos.Age_Desembolso = 0
                       Creditos.Cod_Desembolso = 0
                       Creditos.Cue_Desembolso = "".
        END.
    END.
    ELSE
        IF Rs_Desemb EQ 2 THEN DO:
            ASSIGN W_CtaTerceros:SENSITIVE = TRUE
                   W_NitTercero:SENSITIVE = TRUE.

            APPLY "ENTRY" TO W_CtaTerceros.
            RETURN NO-APPLY.
        END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Rs_Desemb wWin
ON VALUE-CHANGED OF Rs_Desemb IN FRAME F_Formalizar
DO:
  ASSIGN Rs_Desemb.
  APPLY "Mouse-Select-Click" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_ImpCpte
&Scoped-define SELF-NAME Rs_SiNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Rs_SiNo wWin
ON MOUSE-SELECT-CLICK OF Rs_SiNo IN FRAME F_ImpCpte
DO:
    SESSION:SET-WAIT-STATE("General").

    /*impresion de cheque */
    IF Creditos.Desembolso EQ 2 OR Creditos.Desembolso EQ 5 OR Creditos.Desembolso EQ 7 THEN DO:
        RUN Imp_Cheque(INPUT W_VrCheque,
                       INPUT Nom_Beneficiario:SCREEN-VALUE IN FRAME F_Bancos,
                       INPUT W_Ciudad) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            MESSAGE "Error Imprimiendo Cheque."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.

    FIND FIRST Formatos WHERE Formatos.Agencia EQ Creditos.Agencia
                          AND Formatos.Cod_Formato EQ Comprobantes.Cod_Formato NO-LOCK NO-ERROR.
    IF AVAILABLE(Formatos) THEN DO:
        IF Formatos.Nom_Proceso EQ "" THEN DO:
            MESSAGE "EL formato encontrado no tiene asignado un programa" SKIP
                    "ejecutable. verifique en la tabla de formatos, el" SKIP
                    "nombre del proceso para la Agencia: " Creditos.Agencia SKIP
                    "El formato: " Formatos.Cod_Formato SKIP(1)
                    "Al momento este campo no tiene ningún nombre de ejecutable digitado"
                VIEW-AS ALERT-BOX ERROR.
        END.
        ELSE DO:
            RUN VALUE(Formatos.Nom_Proceso) (INPUT Comprobantes.Comprobante,
                                             INPUT Mov_Contable.Num_Documento, INPUT Mov_Contable.Num_Documento,
                                             INPUT Creditos.Agencia,
                                             INPUT w_fecha) NO-ERROR.
        END.

      IF ERROR-STATUS:ERROR THEN DO:
         MESSAGE "Error al llamar la rutina de impresion" SKIP
                "consulte con el administrador!" VIEW-AS ALERT-BOX.         
     END.
  END.
  ELSE DO:
     MESSAGE "No se encuentra el formato de impresión" SKIP
             "consulte con el administrador!" VIEW-AS ALERT-BOX.     
  END.
  
  ASSIGN FRAME F_ImpCpte:VISIBLE = FALSE.
  HIDE FRAME F_Formalizar.
  HIDE FRAME F_Condicionada.
  HIDE FRAME F_Ultima.
  ASSIGN FRAME F_Creditos:SENSITIVE = TRUE
         W_Des                      = NO.

  APPLY "choose" TO Btn_Consulta IN FRAME F_Creditos.

  SESSION:SET-WAIT-STATE("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Rs_SiNo wWin
ON VALUE-CHANGED OF Rs_SiNo IN FRAME F_ImpCpte
DO:
   APPLY "Mouse-Select-Click" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_ConAdmisible
&Scoped-define SELF-NAME R_ConAdm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_ConAdm wWin
ON VALUE-CHANGED OF R_ConAdm IN FRAME F_ConAdmisible
DO:
  ASSIGN R_ConAdm.

  RUN Consul_Gtias.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME R_Organizar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_Organizar wWin
ON VALUE-CHANGED OF R_Organizar IN FRAME F_Consulta
DO:
  CASE SELF:SCREEN-VALUE:
    WHEN "1" THEN DO:
      OPEN QUERY Br_Consulta FOR EACH Consulta
                NO-LOCK  BY Consulta.Num_Credito INDEXED-REPOSITION.
      Buscar:LABEL IN FRAME F_Consulta = "Buscar x Solicitud".
    END.
    WHEN "2" THEN DO:
      OPEN QUERY Br_Consulta FOR EACH Consulta
                NO-LOCK  BY Consulta.AgeCredito INDEXED-REPOSITION.
      Buscar:LABEL IN FRAME F_Consulta = "Buscar x Agencia".
    END.
    WHEN "3" THEN DO:
      OPEN QUERY Br_Consulta FOR EACH Consulta
                NO-LOCK  BY Consulta.Nit INDEXED-REPOSITION.
      Buscar:LABEL IN FRAME F_Consulta = "Buscar x Nit".
    END.
    WHEN "4" THEN DO:
      OPEN QUERY Br_Consulta FOR EACH Consulta
                NO-LOCK  BY Consulta.Nombre INDEXED-REPOSITION.
      Buscar:LABEL IN FRAME F_Consulta = "Buscar x Nombre".
    END.
    WHEN "5" THEN DO:
      OPEN QUERY Br_Consulta FOR EACH Consulta
                NO-LOCK  BY Consulta.Fec_ingreso INDEXED-REPOSITION.
      Buscar:LABEL IN FRAME F_Consulta = "Buscar x Fec.Ingreso".
    END.
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Garantias
&Scoped-define SELF-NAME R_TipoGarantia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_TipoGarantia wWin
ON VALUE-CHANGED OF R_TipoGarantia IN FRAME F_Garantias
DO:
  IF SELF:SCREEN-VALUE = "1" THEN DO:
     HIDE FRAME F_Admisible.
     VIEW FRAME F_Codeudores.
  END.
  ELSE DO:
     ASSIGN FRAME F_Admisible:TITLE = "Garantias Admisibles".

     Garantias.Tipo_Garantia:ENABLE("Propiedad").
     Garantias.Tipo_Garantia:ENABLE("Prenda").
     Garantias.Tipo_Garantia:ENABLE("Aportes").
     Garantias.Tipo_Garantia:ENABLE("Inversión").
     Garantias.Tipo_Garantia:DISABLE("Otras NoAd").
     Garantias.Tipo_Garantia:DISABLE("Cdat-Contrac NoAd").

     IF SELF:SCREEN-VALUE = "3" THEN DO:
        ASSIGN FRAME F_Admisible:TITLE = "Garantias No-Admisibles".

        Garantias.Tipo_Garantia:DISABLE("Propiedad").
        Garantias.Tipo_Garantia:DISABLE("Aportes").
        Garantias.Tipo_Garantia:DISABLE("Prenda").
        Garantias.Tipo_Garantia:DISABLE("Inversión").
        Garantias.Tipo_Garantia:ENABLE("Otras NoAd").
        Garantias.Tipo_Garantia:ENABLE("Cdat-Contrac NoAd").

        FIND FIRST Garantias WHERE Garantias.Agencia      EQ Creditos.Agencia     AND
                                  Garantias.Tip_Credito   EQ Creditos.Tip_Credito AND
                                  Garantias.Cod_Credito   EQ Creditos.Cod_Credito AND
                                  Garantias.Num_Credito   EQ Creditos.Num_Credito AND
                                  Garantias.Estado        EQ 1 AND
                                  Garantias.Tipo_Garantia GE 4 NO-ERROR.
     END.
     ELSE
        FIND FIRST Garantias WHERE Garantias.Agencia      EQ Creditos.Agencia AND
                                  Garantias.Tip_Credito   EQ Creditos.Tip_Credito AND   
                                  Garantias.Cod_Credito   EQ Creditos.Cod_Credito AND   
                                  Garantias.Num_Credito   EQ Creditos.Num_Credito AND 
                                  Garantias.Estado        EQ 1 AND                       
                                  Garantias.Tipo_Garantia LE 3 NO-ERROR.

     IF AVAILABLE Garantias THEN DO:
        RUN Mostrar_Admisible.
        W_NvaAdm = FALSE.
     END.
     ELSE DO:
        RUN Inicializar_Admisible.
        W_NvaAdm = YES.
     END.

     VIEW FRAME F_Admisible.

     Btn_SalAdm:SENSITIVE = TRUE.

     HIDE FRAME F_Codeudores.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Admisible
&Scoped-define SELF-NAME Garantias.Val_Asegurado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Val_Asegurado wWin
ON VALUE-CHANGED OF Garantias.Val_Asegurado IN FRAME F_Admisible /* Valor Asegurado */
DO:
  ENABLE Btn_SalAdm WITH FRAME F_Admisible.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Garantias.Val_Bien
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Val_Bien wWin
ON LEAVE OF Garantias.Val_Bien IN FRAME F_Admisible /* Valor del Bien */
DO:
  IF DEC(Garantias.Val_Bien:SCREEN-VALUE) LT Creditos.Monto THEN 
     MESSAGE "El valor de la Garantìa es Inferior al Monto del Crèdito..."
         VIEW-AS ALERT-BOX TITLE "INFORMATIVO".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Val_Bien wWin
ON VALUE-CHANGED OF Garantias.Val_Bien IN FRAME F_Admisible /* Valor del Bien */
DO:
  ENABLE Btn_SalAdm WITH FRAME F_Admisible.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Garantias.Val_Impuesto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Val_Impuesto wWin
ON VALUE-CHANGED OF Garantias.Val_Impuesto IN FRAME F_Admisible /* Valor */
DO:
    ENABLE Btn_SalAdm WITH FRAME F_Admisible. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Garantias.Val_UltAvaluo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Val_UltAvaluo wWin
ON VALUE-CHANGED OF Garantias.Val_UltAvaluo IN FRAME F_Admisible /* Valor Último Avaluo */
DO:
    ENABLE Btn_SalAdm WITH FRAME F_Admisible. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Solicitud
&Scoped-define SELF-NAME W-fecEntLib
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-fecEntLib wWin
ON ENTRY OF W-fecEntLib IN FRAME F_Solicitud /* Fecha Primer Pago */
DO:
  ASSIGN W_VrADesemb = Creditos.Monto - (WDed + W_VrCredACanc +  W_FaltApor)
         W_VrADesemb:SCREEN-VALUE IN FRAME F_Solicitud = STRING(W_VrADesemb).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-fecEntLib wWin
ON LEAVE OF W-fecEntLib IN FRAME F_Solicitud /* Fecha Primer Pago */
DO:
   ASSIGN W-FecEntLib.
   IF  STRING (w-Fecentlib) = "00/00/0000" THEN DO:
       MESSAGE " esta esta no es valida !" VIEW-AS ALERT-BOX INFOR BUTTON OK.
       ASSIGN w-fecentlib = ?.
           RETURN NO-APPLY.
   END.
   ELSE DO:
       ENABLE Btn_ProInstancia.
   ENABLE BUTTON-210.
   END.
         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ultima
&Scoped-define SELF-NAME W_CtaTerceros
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CtaTerceros wWin
ON LEAVE OF W_CtaTerceros IN FRAME F_Ultima /* Cta-Contable (Terceros) */
DO:
  DEFI VAR P_NatTra LIKE Cuentas.Natur.

  ASSIGN W_CtaTerceros
         NomCta:SCREEN-VALUE = "".

  FIND FIRST Cuentas WHERE Cuentas.cuenta EQ W_CtaTerceros 
                       AND Cuentas.Tipo   EQ 2 NO-LOCK NO-ERROR.
  IF NOT AVAIL(Cuentas) THEN 
     RUN C-Cuentas.R (OUTPUT W_CtaTerceros, OUTPUT NomCta, OUTPUT P_NatTra, OUTPUT P_NatTra, 
                      INPUT  "T").
  
  FIND FIRST Cuentas WHERE Cuentas.cuenta EQ W_CtaTerceros 
                       AND Cuentas.Tipo   EQ 2 NO-LOCK NO-ERROR.
  IF AVAIL(Cuentas) THEN
     ASSIGN W_CtaTerceros:SCREEN-VALUE = W_CtaTerceros
            NomCta:SCREEN-VALUE        = Cuentas.Nombre.
  ELSE ASSIGN W_CtaTerceros:SCREEN-VALUE = ""
              W_CtaTerceros.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Formalizar
&Scoped-define SELF-NAME W_CtaTerceros
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CtaTerceros wWin
ON LEAVE OF W_CtaTerceros IN FRAME F_Formalizar /* Cta-Contable (Terceros) */
DO:
  DEFI VAR P_NatTra LIKE Cuentas.Natur.

  ASSIGN W_CtaTerceros
         NomCta:SCREEN-VALUE = "".

  FIND FIRST Cuentas WHERE Cuentas.cuenta EQ W_CtaTerceros 
                       AND Cuentas.Tipo   EQ 2 
                       AND Cuentas.Id_NIT      NO-LOCK NO-ERROR.
  IF NOT AVAIL(Cuentas) THEN 
     RUN C-Cuentas.R (OUTPUT W_CtaTerceros, OUTPUT NomCta, OUTPUT P_NatTra, OUTPUT P_NatTra, 
                      INPUT  "T").
  
  FIND FIRST Cuentas WHERE Cuentas.cuenta EQ W_CtaTerceros 
                       AND Cuentas.Tipo   EQ 2
                       AND Cuentas.Id_NIT NO-LOCK NO-ERROR.
  IF AVAIL(Cuentas) THEN
     ASSIGN W_CtaTerceros:SCREEN-VALUE = W_CtaTerceros
            W_CtaTerceros
            NomCta:SCREEN-VALUE        = Cuentas.Nombre.
  ELSE ASSIGN W_CtaTerceros:SCREEN-VALUE = ""
              W_CtaTerceros
              Rs_Desemb                  = 3
              Rs_Desemb:SCREEN-VALUE     = "3".
  APPLY "Entry" TO W_NitTercero.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_FecIni
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_FecIni wWin
ON LEAVE OF W_FecIni IN FRAME F_Formalizar /* Fecha Inicio */
DO:
    /*
  ASSIGN W_FecIni
         W_DiasAnt  = 0
         W_IntAntic = 0.

  IF Pro_Creditos.Id_PerGracia AND Pro_Creditos.Dia_Gracia GT 0 THEN DO:
     MESSAGE "La linea del Credito es con Pdo.de Gracia, no se permite Modificar la Fec-Inicio."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     ASSIGN W_FecIni = W_Fecha + Pro_Creditos.Dia_Gracia
            W_FecIni:SCREEN-VALUE = STRING(W_FecIni).
     RETURN.
  END.     

  IF W_FecIni LT W_Fecha OR (W_FecIni - W_Fecha) GT 359 /* 29 */ THEN DO:
     MESSAGE "La fecha de Inicio para Control de Pago y/o Vencimientos," SKIP
             "No puede ser anterior a Hoy, ni posterior superior a 360 días..."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     ASSIGN W_FecIni = W_Fecha
            W_FecIni:SCREEN-VALUE = STRING(W_Fecha).
  END.

  IF W_FecIni GT W_Fecha THEN DO:  /*Halla Int-Anticipados para descontar del Crédito*/
      ASSIGN W_DiasAnt  = W_FecIni - W_Fecha
             W_IntAntic = ROUND(((Creditos.Monto) * Creditos.Tasa / 36000) * W_DiasAnt,0).

      IF W_NetoDesemb - W_IntAntic LT 0 THEN DO:
         MESSAGE "El valor de los Int-Anticipados a descontar es : " W_IntAntic
                 "El valor a Desembolsar no puede ser Negativo...? Revise por favor." SKIP
                 "          No se permite la Operacion...!" VIEW-AS ALERT-BOX ERROR.
         ASSIGN W_FecIni                = W_Fecha
                W_FecIni:SCREEN-VALUE   = STRING(W_Fecha)
                W_DiasAnt               = 0
                W_IntAntic              = 0.
      END. 
      
  END.
  /* no cobra deducible */
 /* ASSIGN W_NetoAD              = W_VrADesemb 
         W_NetoAD:SCREEN-VALUE = STRING(W_NetoAD).*/
  
  ASSIGN W_NetoAD              = W_VrADesemb - W_IntAntic
         W_NetoAD:SCREEN-VALUE = STRING(W_NetoAD).
         
         */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ultima
&Scoped-define SELF-NAME W_FecIni
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_FecIni wWin
ON LEAVE OF W_FecIni IN FRAME F_Ultima /* Fecha Primer Pago */
DO:
    
   /*
  ASSIGN W_FecIni.

  IF W_FecIni LT W_Fecha OR (W_FecIni - W_Fecha) GT 359 THEN DO:
     MESSAGE "La fecha de Inicio para Control de Pago y/o Vencimientos," SKIP
             "No puede ser anterior a Hoy, ni posterior superior a 360 días..."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     ASSIGN W_FecIni                  = W_Fecha
            W_FecIni:SCREEN-VALUE     = STRING(W_Fecha)
            W_DiasAnt                 = 0
            W_IntAntic                = 0
            W_IntAntic:SCREEN-VALUE   = "0"
            W_NetoDesemb              = W_NetoD
            W_NetoDesemb:SCREEN-VALUE = STRING(W_NetoD).
  END.

  IF W_FecIni GT W_Fecha THEN DO:  /*Halla Int-Anticipados para descontar del Crédito*/

/*    MESSAGE  "W_DiasAnt " W_DiasAnt 
               "W_IntAntic" W_IntAntic
               "ROUND(((Creditos.Monto ) * Creditos.Tasa / 36000) * W_DiasAnt,0)"
               "Creditos.Monto"  Creditos.Monto SKIP
               "Creditos.Tasa "  Creditos.Tasa  SKIP
               "W_DiasAnt     "  W_DiasAnt      
          VIEW-AS ALERT-BOX INFO BUTTONS OK. */


      /*-W_FecIni - W_Fecha*/
      ASSIGN W_DiasAnt  = 0  
             W_IntAntic = ROUND(((Creditos.Monto) * Creditos.Tasa / 36000) * W_DiasAnt,0)
             W_IntAntic:SCREEN-VALUE = STRING(W_IntAntic).
     /* ASSIGN W_IntAntic = 0
             W_IntAntic:SCREEN-VALUE = STRING(W_IntAntic). nh*/

      IF W_NetoD - W_IntAntic LT 0 THEN DO:
         MESSAGE "El valor de los Int-Anticipados a descontar es : " W_IntAntic
                 "El valor a Desembolsar no puede ser Negativo...? Revise por favor." SKIP
                 "          No se permite la Operacion...!" VIEW-AS ALERT-BOX ERROR.
         ASSIGN W_FecIni                = W_Fecha
                W_FecIni:SCREEN-VALUE   = STRING(W_Fecha)
                W_DiasAnt               = 0
                W_IntAntic              = 0
                W_IntAntic:SCREEN-VALUE = "0"
                W_NetoDesemb            = W_NetoD
                W_NetoDesemb:SCREEN-VALUE = STRING(W_NetoD).
               
      END.
      ELSE  
          ASSIGN W_NetoDesemb              = W_NetoD - W_IntAntic
                 W_NetoDesemb:SCREEN-VALUE = STRING(W_NetoDesemb). 
  END.
  ELSE ASSIGN W_FecIni                = W_Fecha
            W_FecIni:SCREEN-VALUE     = STRING(W_Fecha)
            W_DiasAnt                 = 0
            W_IntAntic                = 0
            W_IntAntic:SCREEN-VALUE   = "0"
            W_NetoDesemb              = W_NetoD
            W_NetoDesemb:SCREEN-VALUE = STRING(W_NetoD).

  APPLY "Mouse-Select-Click" TO Creditos.Desembolso.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Codeudores
&Scoped-define SELF-NAME W_NitCodeudor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_NitCodeudor wWin
ON LEAVE OF W_NitCodeudor IN FRAME F_Codeudores /* Nit Codeudor */
DO:
DO WITH FRAME F_Codeudores:
   IF SELF:SCREEN-VALUE EQ Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud THEN DO:
      MESSAGE "No puede ser codeudor de si mismo" SKIP
              "Rectifique el nit del codeudor" SKIP
              "para el cliente: " Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud
              VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO SELF.
      RETURN NO-APPLY.
   END.
   FIND Clientes WHERE Clientes.Nit EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
   IF NOT AVAILABLE(Clientes) THEN DO:
      RUN C-Clientes.R(INPUT 1, INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_AgeCli).
      ASSIGN W_NomCodeudor:SCREEN-VALUE = P_Nombre + " " + P_Apellido
             SELF:SCREEN-VALUE = P_Nit.
   END.
   ELSE
      ASSIGN W_NomCodeudor:SCREEN-VALUE = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
   FIND FIRST Relaciones WHERE 
        Relaciones.Nit          EQ Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud AND
        Relaciones.Cuenta       EQ Creditos.Num_Credito:SCREEN-VALUE IN FRAME F_Solicitud AND
        Relaciones.Cod_Relacion EQ 11 AND
        Relaciones.Nit_Relacion EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
   IF AVAILABLE(Relaciones) THEN DO:
      ASSIGN W_NitCodeudor:SCREEN-VALUE = ""
             W_NomCodeudor:SCREEN-VALUE = "".
      MESSAGE "Esta Relacion ya existe!" SKIP
              "Se Cancela la Operacion de Creado" VIEW-AS ALERT-BOX WARNING.
      DISABLE W_NitCodeudor Btn_SalCod.
      ENABLE Btn_CreCod Btn_Activas.
   END.
   ELSE
       APPLY 'entry' TO Btn_SalCod.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Formalizar
&Scoped-define SELF-NAME W_NitTercero
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_NitTercero wWin
ON LEAVE OF W_NitTercero IN FRAME F_Formalizar /* Ced.Nit del Tercero */
DO:
  DEFI VAR W_NomCte AS CHAR.
  DEFI VAR W_AgeCed LIKE Agencias.Agencia.

  ASSIGN W_NitTercero
         NomCed:SCREEN-VALUE = "".

  FIND FIRST Clientes WHERE Clientes.Nit EQ W_NitTercero NO-LOCK NO-ERROR.
  IF NOT AVAIL(Clientes) THEN DO:
     ASSIGN FRAME F_Formalizar:SENSITIVE = FALSE.                                                           
                                                                                                    
     RUN C-Clientes.R (INPUT  1,W_Agencia,                                                           
                       OUTPUT W_NitTercero, OUTPUT W_NomCte, OUTPUT W_NomCte, OUTPUT W_AgeCed).       
                                                                                                    
     ASSIGN FRAME F_Formalizar:SENSITIVE = TRUE.
  END.
  
  FIND FIRST Clientes WHERE Clientes.Nit EQ W_NitTercero NO-LOCK NO-ERROR.
  IF AVAIL(Clientes) THEN
     ASSIGN W_NitTercero:SCREEN-VALUE = W_NitTercero
            NomCed:SCREEN-VALUE       = TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2) +   
                                        " " + TRIM(Clientes.Nombre).
  ELSE ASSIGN W_NitTercero:SCREEN-VALUE = ""
              W_NitTercero
              NomCed:SCREEN-VALUE = ""              
              Rs_Desemb                  = 3
              Rs_Desemb:SCREEN-VALUE     = "3".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ultima
&Scoped-define SELF-NAME W_VrCheque
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_VrCheque wWin
ON LEAVE OF W_VrCheque IN FRAME F_Ultima /* En Cheque */
DO:
  ASSIGN W_VrCheque.

  IF ROUND(W_SMLV * 3,0) GT W_VrCheque THEN 
     MESSAGE "El valor en Cheque debe ser Minimo : " ROUND(W_SMLV * 3,0) SKIP
             "                    Rectifique por favor."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.

  IF (W_NetoDesemb - W_VrCheque) LE 0 THEN DO:
     MESSAGE "La Opcion seleccionada no le permite este Valor," SKIP
             "El valor restante debe ser Superior a Cero(0)."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     ASSIGN W_VrCheque              = W_NetoDesemb
            W_VrCheque:SCREEN-VALUE = STRING(W_NetoDesemb).
  END.

  IF Creditos.Desembolso EQ 7 THEN
     ASSIGN W_VrAlAhorro              = W_NetoDesemb - W_VrCheque
            W_VrAlAhorro:SCREEN-VALUE = STRING (W_VrAlAhorro).   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_VrEfectivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_VrEfectivo wWin
ON LEAVE OF W_VrEfectivo IN FRAME F_Ultima /* En Efectivo */
DO:
  ASSIGN W_VrEfectivo.

  IF (W_NetoDesemb - W_VrEfectivo) LE 0 THEN DO:
     MESSAGE "La Opcion seleccionada no le permite este Valor," SKIP
             "El valor restante debe ser Superior a Cero(0)."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     ASSIGN W_VrEfectivo = W_NetoDesemb
            W_VrEfectivo:SCREEN-VALUE = STRING(W_NetoDesemb).
  END.

  IF Creditos.Desembolso EQ 5 THEN
     ASSIGN W_VrCheque              = W_NetoDesemb - W_VrEfectivo
            W_VrCheque:SCREEN-VALUE = STRING (W_VrCheque).
  ELSE IF Creditos.Desembolso EQ 6 THEN
     ASSIGN W_VrAlAhorro              = W_NetoDesemb - W_VrEfectivo
            W_VrAlAhorro:SCREEN-VALUE = STRING (W_VrAlAhorro). 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Creditos
&Scoped-define BROWSE-NAME Bancos
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Abona_AporteDifer wWin 
PROCEDURE Abona_AporteDifer :
FIND FIRST Operacion WHERE STRING(Operacion.Cod_Operacion,"999999999") BEGINS "010101"
                       AND Operacion.Ctrl_EfeChe = 2
                       AND Operacion.Estado = 1
                       AND Operacion.Id_SYA = NO NO-LOCK NO-ERROR.
IF NOT AVAILABLE Operacion THEN DO:
    MESSAGE "No se encontró la operación de consignación" SKIP
            "de ahorros para Abonar aportes Diferidos." SKIP
            "Se cancela la operación de desembolso!"
        VIEW-AS ALERT-BOX ERROR.

    RETURN ERROR.
END.

FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Tip_Ahorro = 4
                         AND Pro_Ahorros.Estado = 1
                         AND Pro_Ahorros.Cod_Ahorro = 5 NO-LOCK NO-ERROR.

FIND FIRST CortoLargo WHERE CortoLargo.Agencia = Creditos.Agencia
                        AND CortoLargo.Cod_Producto = Pro_Ahorros.Cod_Ahorro
                        AND CortoLargo.Clase_Producto = 1
                        AND CortoLargo.Cta_AsoAd <> "" NO-LOCK NO-ERROR.
IF NOT AVAILABLE CortoLargo THEN DO:
    MESSAGE "No se halló CORTOLARGO para Aportes Diferidos" SKIP
            "Se cancela la operación de desembolso!"
        VIEW-AS ALERT-BOX ERROR.

    RETURN ERROR.
END.

FIND FIRST Ahorros WHERE Ahorros.Nit = Creditos.Nit
                     AND Ahorros.cod_ahorro = 5 NO-ERROR.
IF NOT AVAILABLE(Ahorros) THEN DO:
    CREATE Ahorros.
    Ahorros.Agencia = Creditos.Agencia.
    Ahorros.Nit = Creditos.Nit.
    Ahorros.Tip_Ahorro = 4.
    Ahorros.Cod_Ahorro = Pro_Ahorros.Cod_Ahorro.
    Ahorros.Cue_Ahorros = Creditos.Nit.
    Ahorros.Fec_Apertur = W_Fecha.
    Ahorros.Detalle_Estado = 2.
    Ahorros.Estado = 1.
    Ahorros.Sdo_Inicial = W_FaltApor.
    Ahorros.Plazo = Creditos.Plazo.
END.

w_ageaportes = Ahorros.agencia.
W_CtaSyA_aporte = CortoLargo.Cta_SYA.

IF Creditos.agencia <> w_ageaportes THEN DO:
    IF W_CtaSyA_aporte = "" THEN DO:
        MESSAGE "No se ha encontrado la configuración de Corto y largo de SyA" SKIP
                "Para el apalancamiento (el producto de aportes), Agencia:" w_agencia SKIP
                "Clase producto: 1 y Cod_producto = 5 " SKIP
                "Comunique esta inconsistencia al Administrador del Sistema"
            VIEW-AS ALERT-BOX ERROR.

        RETURN ERROR.
    END.
END.

IF Ahorros.Estado <> 1 OR Ahorros.Detalle_Estado <> 2 THEN DO:
    Ahorros.Detalle_Estado = 2.
    Ahorros.Estado = 1.
    Ahorros.Sdo_Inicial = W_FaltApor.
    Ahorros.Plazo = Creditos.Plazo.
END.

Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible + W_FaltApor.
Ahorros.Fec_Ulttransaccion = W_Fecha.

IF ahorros.tarjetaDB <> " " AND W_FaltApor > 0 THEN
    RUN grabar_TmpTarDeb(INPUT W_FaltApor,
                         INPUT "Proc_Desembolso: Abono Aportes Diferidos",
                         INPUT 3).

RUN Grabar_MovAhorros(INPUT Operacion.Cod_Operacion,
                      INPUT Ahorros.Cod_Ahorro,
                      INPUT Ahorros.Cue_Ahorros,
                      INPUT W_NumCbt,
                      INPUT Ahorros.Agencia,
                      INPUT W_Agencia,
                      INPUT Creditos.Agencia,
                      INPUT W_Usuario,
                      INPUT 0,
                      INPUT W_FaltApor,
                      INPUT Creditos.Nit) NO-ERROR.

IF ERROR-STATUS:ERROR THEN DO:
    MESSAGE "No se pudo grabar el movimiento de ahorros" SKIP
            "a la cuenta especificada." SKIP
            "Se cancela la operación de Desembolso"
        VIEW-AS ALERT-BOX ERROR.

    RETURN ERROR.
END.

Mov_Ahorros.Sdo_Dispon = Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje.
Mov_Ahorros.Val_Efectivo = W_FaltApor.
Mov_Ahorros.Val_Cheque = 0.

FIND CURRENT Ahorros NO-LOCK NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
    FIND Usuarios WHERE Usuarios.Usuario = W_Usuario        NO-LOCK NO-ERROR.
    FIND Clientes WHERE Clientes.Nit     = Creditos.Nit     NO-LOCK NO-ERROR.
    FIND Agencias WHERE Agencias.Agencia = Creditos.Agencia NO-LOCK NO-ERROR. 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Act_NumCre_Adm wWin 
PROCEDURE Act_NumCre_Adm :
FOR EACH Garantias WHERE
         Garantias.Agencia       EQ Creditos.Agencia       AND
         Garantias.Tip_Credito   EQ Creditos.Tip_Credito   AND
         Garantias.Cod_Credito   EQ Creditos.Cod_Credito   AND
         Garantias.Num_Credito   EQ 0                      AND
         Garantias.Num_Solicitud EQ Creditos.Num_Solicitud :

    ASSIGN Garantias.Num_Credito = Creditos.Num_Credito /*Marzo 1/05 GAER, estaba como parte del where*/
           Garantias.Nit         = Creditos.Nit. 
END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Asignar_Proxima_Instancia wWin 
PROCEDURE Asignar_Proxima_Instancia :
DEF VAR W_Ord AS INTEGER.
DEF VAR W_OrdSiguiente AS INTEGER.

IF Creditos.Estado = 1 THEN DO:
    FIND FIRST Cfg_RegCredito WHERE Cfg_RegCredito.Cod_Credito = Creditos.Cod_Credito NO-LOCK NO-ERROR.

    FIND FIRST Mov_Instancias WHERE Mov_Instancias.Cuenta = Creditos.Num_Credito:SCREEN-VALUE IN FRAME F_Solicitud
                                AND Mov_Instancias.Num_Solicitud = INT(Creditos.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud)
                                AND Mov_instancias.Nit = Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud
                                AND Mov_Instancias.Estado = NO NO-LOCK NO-ERROR.
    IF AVAILABLE Mov_Instancias THEN DO:
        MESSAGE "Para pasar a la siguiente instancia" SKIP
                "deben estar cerradas las instancias" SKIP
                "anteriores. al momento se encuentra" SKIP
                "abierta la instancia: " Mov_Instancias.Instancia SKIP
                "Vigente en el Usuario: " Mov_Instancias.Usuario
            VIEW-AS ALERT-BOX ERROR.

        ENABLE ALL WITH FRAME F_Creditos.
        DISABLE NomUsuario WITH FRAME F_Creditos.
        ENABLE ALL WITH FRAME F_Consulta.
        OPEN QUERY Br_Consulta FOR EACH Consulta NO-LOCK  BY Consulta.Num_Solicitud INDEXED-REPOSITION.
        Buscar:LABEL IN FRAME F_Consulta = "Buscar Solicitud".
        RETURN ERROR.
    END.

    DO:
        FIND Instancias WHERE Instancias.Tipo_Instancia = 3
                          AND Instancias.Instancia = INT(SUBSTR(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5))
                          AND Instancias.Tipo_Producto = 2
                          AND Instancias.Estado = 1 NO-LOCK NO-ERROR.
        IF AVAILABLE Instancias THEN DO:
            W_ord = Instancias.Orden_Instancia.

            FIND FIRST Instancias WHERE Instancias.Tipo_Instancia = 3
                                    AND Instancias.Instancia = 950
                                    AND Instancias.Tipo_Producto = 2
                                    AND Instancias.Estado = 1 NO-LOCK NO-ERROR.
            IF AVAILABLE Instancias THEN DO:
                EMPTY TEMP-TABLE TProIns.

                /*Encuentra los usuarios configurados para las instancias siguientes, y que cumplen los limites*/
                FOR EACH Cfg_Instancias WHERE Cfg_Instancias.Agencia = INTEGER(SUBSTR(Cmb_Agencias:SCREEN-VALUE IN FRAME F_Solicitud,1,3))
                                          AND Cfg_Instancias.Tipo_Instancia = Instancias.Tipo_Instancia
                                          AND Cfg_Instancias.Orden = Instancias.Orden_Instancia
                                          AND Cfg_Instancias.Plazo_Minimo <= Dias
                                          AND Cfg_Instancias.Plazo_Maximo >= Dias
                                          AND Cfg_Instancias.Monto_Minimo <= Creditos.Monto
                                          AND Cfg_Instancias.Monto_Maximo >= Creditos.Monto
                                          AND Cfg_Instancias.Estado = 1 NO-LOCK:
                    CREATE TProIns.
                    ASSIGN TProIns.TP_Orden = Cfg_Instancias.Orden
                           TProIns.TP_Instancia = Cfg_Instancias.Instancia
                           TProIns.TP_Usuario = Cfg_Instancias.Usuario
                           TProIns.Tp_Cantidad = 0
                           TProIns.TP_Agencia = Cfg_Instancias.Agencia.

                    FIND Instancias WHERE Instancias.Instancia = Cfg_Instancias.Instancia NO-LOCK NO-ERROR.
                    IF AVAILABLE Instancias THEN
                        TProIns.TP_NomInstan = Instancias.Nom_Instancia.
                    ELSE
                        TProIns.TP_NomInstan = "Instancia No encontrada".

                    FIND Usuarios WHERE Usuarios.Usuario = Cfg_Instancias.Usuario NO-LOCK NO-ERROR.
                    IF AVAILABLE Usuarios THEN
                        TProIns.TP_NomUsuar = Usuario.Nombre.
                    ELSE
                        TProIns.TP_NomUsuar = "Usuario No encontrado".
                END.

                FIND FIRST TProIns NO-ERROR.
                IF NOT AVAIL TProIns THEN DO:
                    MESSAGE "No se encontro ningún Usuario" SKIP
                            "Para asignar la solicitud a la" SKIP
                            "Proxima instancia."
                        VIEW-AS ALERT-BOX.

                    RETURN ERROR.
                END.

                /*Por cada usuario encuentra el numero de creditos que esta procesando para las instancias a seguir*/
                /*esto con el fin de escoger el que menos solicitudes este procesando y distribuir bien las solicitudes*/
                FOR EACH TproIns:
                    FOR EACH Mov_Instancias WHERE Mov_instancias.Instancia = TproIns.TP_Instancia
                                              AND mov_Instancias.Usuario = TProIns.TP_Usuario
                                              AND Mov_Instancias.Estado = NO NO-LOCK:
                        TproIns.TP_Cantidad = TproIns.TP_Cantidad + 1.
                    END.
                END.

                /*se crea registro en hoja de vida para usuario que procesa la isntancia actual*/
                CREATE Hoja_Vida.
                ASSIGN Hoja_Vida.Tipo = 9 
                    Hoja_Vida.Codigo = 1  
                    Hoja_Vida.Instancia = INT(SUBSTR(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5))
                    Hoja_Vida.DoctoRefer = INT(Creditos.Num_Credito:SCREEN-VALUE IN FRAME F_Solicitud)
                    Hoja_Vida.Nit = Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud
                    Hoja_Vida.Usuario = W_Usuario
                    Hoja_Vida.Fec_Grabacion = TODAY
                    Hoja_Vida.Hora_Grabacion = TIME
                    Hoja_Vida.Observacion = "Se cierra la instancia: " + Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos + " - Procesada por Usuario: " + W_Usuario + " - " + NomUsuario:SCREEN-VALUE IN FRAME F_Creditos
                    Hoja_Vida.Asunto_Cumplido = YES.

                FOR EACH TProIns BREAK BY TproIns.TP_Instancia
                                       BY TproIns.TP_Cantidad:
                    IF FIRST-OF(TProIns.TP_Instancia) AND FIRST-OF(TProIns.TP_Cantidad) THEN DO:
                        /* se crea registro en hoja de vida para el usuario al cual se le asigna la solicitud*/
                        CREATE Hoja_Vida.
                        ASSIGN Hoja_Vida.Tipo = 9
                               Hoja_Vida.Codigo = 1
                               Hoja_Vida.Instancia = TProIns.TP_Instancia
                               Hoja_Vida.DoctoRefer = INT(Creditos.Num_Credito:SCREEN-VALUE IN FRAME F_Solicitud)
                               Hoja_Vida.Nit = Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud
                               Hoja_Vida.Usuario = TProIns.TP_Usuario
                               Hoja_Vida.Fec_Grabacion = TODAY
                               Hoja_Vida.Hora_Grabacion = TIME
                               Hoja_Vida.Asunto_Cumplido = YES.

                        Hoja_Vida.Observacion = "Entra a Instancia: " + STRING(TProIns.TP_Instancia) + " - " + TProIns.TP_NomInstan + " - Usuario Responsable: " + TProIns.TP_Usuario + " - " + TProIns.TP_NomUsuar.

                        CREATE Mov_Instancias.
                        ASSIGN Mov_Instancias.Fec_Ingreso = TODAY
                               Mov_Instancias.Hora_Ingreso = TIME
                               Mov_Instancias.Nit = Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud
                               Mov_Instancias.Num_Solicitud = DECIMAL(Creditos.Num_Solicitud:SCREEN-VALUE)
                               Mov_Instancias.Usuario = TProIns.TP_Usuario
                               Mov_Instancias.Instancia = TProIns.TP_Instancia
                               Mov_Instancias.Cuenta = STRING(DECIMAL(Creditos.Num_Credito:SCREEN-VALUE IN FRAME F_Solicitud))
                               Mov_Instancias.Agencia = TProIns.TP_Agencia.
                    END.
                END.
            END.
        END.
    END.
END.

/*si la solicitud ha sido condicionada, para que se vaya a la ultima*/
IF AVAILABLE Creditos AND Creditos.Estado = 4 THEN DO:
    CREATE Hoja_Vida.

    ASSIGN Hoja_Vida.Tipo            = 9 
            Hoja_Vida.Codigo          = 1  
            Hoja_Vida.Instancia       = INT(SUBSTR(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5))
            Hoja_Vida.DoctoRefer      = INT(Creditos.Num_Credito:SCREEN-VALUE IN FRAME F_Solicitud)
            Hoja_Vida.Nit             = Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud
            Hoja_Vida.Usuario         = W_Usuario
            Hoja_Vida.Fec_Grabacion   = TODAY
            Hoja_Vida.Hora_Grabacion  = TIME
            Hoja_Vida.Asunto_Cumplido = YES.
     FIND Instancias WHERE Instancias.Instancia EQ INT(SUBSTR(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5))
                       AND Instancias.Tipo_Instancia EQ 1 NO-LOCK NO-ERROR.
     IF AVAIL Instancias THEN
         Hoja_Vida.Observacion = "Se cierra la instancia condicionada: " + Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos +
                             " - Procesada por Usuario: " + W_Usuario + " - " + NomUsuario:SCREEN-VALUE IN FRAME F_Creditos.
   /*se crea registro en hoja de vida para el usuario al cual se le asigna la solicitud*/
     FIND FIRST Mov_Instancias WHERE
                Mov_Instancias.Instancia     EQ W_Ultima
            AND Mov_Instancias.Num_Solicitud EQ INT(Creditos.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud)
            AND Mov_instancias.Nit           EQ Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud NO-ERROR.
   /*Validar esta instruccion, dado que al crearse un registro en blanco, origina error al llevar
     un usuario ? a la hoja de vida Atentamente JOHN MONCADA 12 FEB*/
     IF NOT AVAIL Mov_Instancias THEN CREATE Mov_Instancias.
     CREATE Hoja_Vida.
     ASSIGN Hoja_Vida.Tipo        = 9 
            Hoja_Vida.Codigo      = 1  
            Hoja_Vida.Instancia   = W_Ultima
            Hoja_Vida.DoctoRefer  = INT(Creditos.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud)
            Hoja_Vida.Nit         = Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud
            Hoja_Vida.Usuario     = Mov_Instancias.Usuario
            Hoja_Vida.Fec_Grabacion = TODAY
            Hoja_Vida.Hora_Grabacion = TIME
            Hoja_Vida.Asunto_Cumplido = YES.
     FIND Instancias WHERE Instancias.Tipo_Instancia EQ 1
                       AND Instancias.Ultima         EQ YES
                       AND Instancias.Estado         EQ 1
                       AND Instancias.Tipo_Producto  EQ 2 NO-LOCK NO-ERROR.
     IF AVAIL Instancias THEN
        Hoja_Vida.Observacion = "Entra a Ultima instancia: " + STRING(Instancias.Instancia,"999") 
                      + " - " + Instancias.Nom_Instancia + " - Usuario Responsable: " + STRING(Mov_Instancia.Usuario).
     ASSIGN Mov_Instancias.Fec_Ingreso   = TODAY.
            Mov_Instancias.Hora_Ingreso  = TIME.
            Mov_Instancias.Estado        = NO.
            Mov_Instancias.Fec_Retiro    = ?.
            Mov_Instancias.Hora_Retiro   = 0.
     ENABLE ALL WITH FRAME F_Creditos.
     DISABLE NomUsuario WITH FRAME F_Creditos.
     ENABLE ALL WITH FRAME F_Consulta.
     APPLY "choose" TO Btn_Consulta IN FRAME F_Creditos.   
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Asignar_ProxInstancia wWin 
PROCEDURE Asignar_ProxInstancia :
DEFINE VAR W_Ord           LIKE Instancias.Orden_Instancia.
DEFINE VAR W_OrdSiguiente  LIKE Instancias.Orden_Instancia.
DEFINE VAR W_AS AS LOGICAL INITIAL NO.
 DO: /* Igual al Asignar_Proxima_Instancia del programa de Solicitudes */
    FIND Instancias WHERE 
         Instancias.Tipo_Instancia EQ 3 AND
         Instancias.Instancia      EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) AND
         Instancias.Tipo_Producto  EQ 2 AND
         Instancias.Estado         EQ 1 NO-LOCK NO-ERROR.
    IF AVAILABLE Instancias THEN DO:
       W_ord = Instancias.Orden_Instancia.
       FIND NEXT Instancias WHERE
                 Instancias.Tipo_Instancia  EQ 3 AND
                 Instancias.Orden_Instancia GT W_Ord AND
                 Instancias.Tipo_Producto   EQ 2 AND
                 Instancias.Estado          EQ 1 USE-INDEX idx_orden NO-LOCK NO-ERROR.
       IF AVAILABLE Instancias THEN DO: 
          FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ Creditos.Cod_Credito NO-LOCK NO-ERROR.
          FOR EACH TProIns: DELETE TProIns. END.

          /*Encuentra los usuarios configurados para las instancias siguientes, y que cumplen los limites*/
          IF NOT Pro_Creditos.Id_AprobAgencia THEN DO:
              FOR EACH Cfg_Instancias WHERE       
                       Cfg_Instancias.Agencia        EQ INTEGER(SUBSTRING(Cmb_Agencias:SCREEN-VALUE IN FRAME F_Solicitud,1,3)) AND
                       Cfg_Instancias.Tipo_Instancia EQ Instancias.Tipo_Instancia AND
                       Cfg_Instancias.Orden          EQ Instancias.Orden_Instancia AND
                       Cfg_Instancias.Plazo_Minimo   LE Dias AND
                       Cfg_Instancias.Plazo_Maximo   GE Dias AND 
                       Cfg_Instancias.Monto_Minimo   LE Creditos.Monto AND
                       Cfg_Instancias.Monto_Maximo   GE Creditos.Monto AND 
                       Cfg_Instancias.Estado         EQ 1 NO-LOCK:
                   
                  /* NUEVO PARA AMARRARLOS SEGUN AGENCIA    */
                  IF  INTEGER(SUBSTRING(Cmb_Agencias:SCREEN-VALUE IN FRAME F_Solicitud,1,3)) GT 0 
/*                       INTEGER(SUBSTRING(Cmb_Agencias:SCREEN-VALUE IN FRAME F_Solicitud,1,3)) GT 3  AND */ /*aplica para agencias sin fabrica de creditos*/
/*                      INTEGER(SUBSTRING(Cmb_Agencias:SCREEN-VALUE IN FRAME F_Solicitud,1,3)) LT 11      */ /*aplica para agencias sin fabrica de creditos*/
                      THEN DO:
                     IF  Cfg_Instancias.Usuario EQ W_Usuario THEN NEXT.
                  END.
                  ELSE 
                  IF  Cfg_Instancias.Usuario NE W_Usuario THEN NEXT. /* Tomar mismo usuario */

                  FIND FIRST Usuarios WHERE Usuarios.Usuario EQ Cfg_Instancias.Usuario
                                        AND Usuarios.estado  EQ 1 NO-LOCK NO-ERROR.
                   CREATE TProIns.
                   ASSIGN TProIns.TP_Orden     = Cfg_Instancias.Orden
                          TProIns.TP_Instancia = Cfg_Instancias.Instancia
                          TProIns.TP_Usuario   = Cfg_Instancias.Usuario
                          TProIns.Tp_Cantidad  = 0
                          TProIns.TP_Agencia   = Cfg_Instancias.Agencia.

                   FIND Instancias WHERE Instancias.Instancia EQ Cfg_Instancias.Instancia NO-LOCK NO-ERROR.
                   IF AVAILABLE Instancias THEN 
                      TProIns.TP_NomInstan = Instancias.Nom_Instancia.
                   ELSE TProIns.TP_NomInstan = "Instancia No encontrada".

                   IF AVAILABLE Usuarios THEN 
                      TProIns.TP_NomUsuar  = Usuario.Nombre.
                   ELSE TProIns.TP_NomUsuar  = "Usuario No encontrado".
              END.
          END.
          ELSE DO:
              FOR EACH Cfg_Instancias WHERE       
                       Cfg_Instancias.Agencia        EQ INTEGER(SUBSTRING(Cmb_Agencias:SCREEN-VALUE IN FRAME F_Solicitud,1,3)) AND
                       Cfg_Instancias.Tipo_Instancia EQ Instancias.Tipo_Instancia AND
                       Cfg_Instancias.Orden          EQ Instancias.Orden_Instancia AND
                       Cfg_Instancias.Plazo_Minimo   LE Dias AND
                       Cfg_Instancias.Plazo_Maximo   GE Dias AND 
                       Cfg_Instancias.Monto_Minimo   LE Creditos.Monto AND
                       Cfg_Instancias.Monto_Maximo   GE Creditos.Monto AND 
                       Cfg_Instancias.Estado         EQ 1 NO-LOCK:

                  IF w_agencia GT  0
/*                       (w_agencia GT  0 AND w_agencia LT 18)    */ /*aplica para agencias sin fabrica de creditos*/
/*                       OR (w_agencia GT 29 AND w_agencia LT 40) */ /*aplica para agencias sin fabrica de creditos*/
                      THEN   DO:
                    FIND FIRST Usuarios WHERE Usuarios.Usuario EQ Cfg_Instancias.Usuario
                                          AND Usuarios.estado  EQ 1 NO-LOCK NO-ERROR.
                  END.
                  ELSE DO:
                    FIND FIRST Usuarios WHERE Usuarios.Usuario EQ Cfg_Instancias.Usuario
                                          AND Usuarios.Agencia EQ Cfg_Instancias.Agencia 
                                          AND Usuarios.estado  EQ 1 NO-LOCK NO-ERROR.
                    IF Cfg_Instancias.Usuario NE w_usuario THEN NEXT.
                  END.
                   IF AVAILABLE(usuarios) THEN DO: /* Agregue esta condicion antes de grabar */
                      CREATE TProIns.
                      ASSIGN TProIns.TP_Orden     = Cfg_Instancias.Orden
                             TProIns.TP_Instancia = Cfg_Instancias.Instancia
                             TProIns.TP_Usuario   = Cfg_Instancias.Usuario
                             TProIns.Tp_Cantidad  = 0
                             TProIns.TP_Agencia    = Cfg_Instancias.Agencia.
                   END.
                   ELSE NEXT.
                   FIND Instancias WHERE Instancias.Instancia EQ Cfg_Instancias.Instancia NO-LOCK NO-ERROR.
                   IF AVAILABLE Instancias THEN 
                      TProIns.TP_NomInstan = Instancias.Nom_Instancia.
                   ELSE TProIns.TP_NomInstan = "Instancia No encontrada".

                   IF AVAILABLE Usuarios THEN 
                      TProIns.TP_NomUsuar    = Usuario.Nombre.
                   ELSE TProIns.TP_NomUsuar  = "Usuario No encontrado".
              END.
          END.

          FIND FIRST TProIns NO-ERROR.
          IF NOT AVAILABLE TProIns THEN DO:
             MESSAGE "No encontro usuario para asignar a la proxima Instancia"
                 VIEW-AS ALERT-BOX INFO BUTTONS OK.
             FIND Mov_Instancias WHERE 
                  Mov_Instancias.Instancia     EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) AND
                  Mov_Instancias.Num_Solicitud EQ Creditos.Num_Solicitud NO-ERROR.
             IF AVAILABLE Mov_Instancias THEN DO:
                ASSIGN Mov_Instancias.Estado = NO
                       Mov_Instancias.Fec_Retiro = ?.

                ENABLE ALL WITH FRAME F_Creditos.
                DISABLE NomUsuario WITH FRAME F_Creditos.
                ENABLE ALL WITH FRAME F_Consulta.
                APPLY "choose" TO Btn_Consulta IN FRAME F_Creditos.
             END.
             RETURN ERROR.
          END.

          /*Por cada usuario encuentra el numero de creditos que esta
            procesando para las instancias a seguir esto con el fin de
            escoger el que menos solicitudes este procesando y
            distribuir bien las solicitudes*/
          FOR EACH TproIns:
              FOR EACH Mov_Instancias WHERE
                       Mov_instancias.Instancia EQ TproIns.TP_Instancia AND
                       Mov_Instancias.Usuario   EQ TProIns.TP_Usuario  AND
                       Mov_Instancias.Estado    EQ NO    NO-LOCK:
                  TproIns.TP_Cantidad = TproIns.TP_Cantidad + 1.
              END.
          END.

          CREATE Hoja_Vida.
          ASSIGN Hoja_Vida.Tipo       = 9 
                 Hoja_Vida.Codigo     = 1  
                 Hoja_Vida.Instancia  = INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5))
                 Hoja_Vida.DoctoRefer = INTEGER(Creditos.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud)
                 Hoja_Vida.Nit        = Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud
                 Hoja_Vida.Usuario    = W_Usuario
                 Hoja_Vida.Fec_Grabacion  = w_fecha
                 Hoja_Vida.Hora_Grabacion = TIME + 100
                 Hoja_Vida.Observacion    = 
                 "Se cierra la instancia: " + Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos +
                 " - Procesada por Usuario: " + W_Usuario
                 Hoja_Vida.Asunto_Cumplido = YES.
          
          FOR EACH TProIns BREAK BY TproIns.TP_Instancia BY TproIns.TP_Cantidad:
              
              IF FIRST-OF(TProIns.TP_Instancia) AND FIRST-OF(TProIns.TP_Cantidad) THEN DO:
                 /*se crea registro en hoja de vida para usuario que procesa la isntancia actual*/
                 /*se crea registro en hoja de vida para el usuario al cual se le asigna la solicitud*/
                 CREATE Hoja_Vida.
                 ASSIGN Hoja_Vida.Tipo        = 9 
                        Hoja_Vida.Codigo      = 1  
                        Hoja_Vida.Instancia   = TProIns.TP_Instancia
                        Hoja_Vida.DoctoRefer  = INTEGER(Creditos.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud)
                        Hoja_Vida.Nit         = Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud
                        Hoja_Vida.Usuario     = TProIns.TP_Usuario
                        Hoja_Vida.Fec_Grabacion = w_fecha
                        Hoja_Vida.Hora_Grabacion = TIME
                        Hoja_Vida.Asunto_Cumplido = YES.
                        Hoja_Vida.Observacion = "Entra a Instancia: " + STRING(TProIns.TP_Instancia) +
                           " - " + TProIns.TP_NomInstan +
                           " - Usuario Responsable: " + TProIns.TP_Usuario + " - " + TProIns.TP_NomUsuar.
                 FIND FIRST Mov_Instancias WHERE
                      Mov_Instancias.Instancia     EQ TProIns.TP_Instancia AND
                      Mov_Instancias.Num_Solicitud EQ integer(creditos.Num_Solicitud:SCREEN-VALUE) AND
                      Mov_Instancias.Nit           EQ Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud NO-LOCK NO-ERROR.
                 IF NOT AVAILABLE(mov_instancias) THEN
                    CREATE Mov_Instancias.
                 ASSIGN Mov_Instancias.Nit           = Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud
                      Mov_Instancias.Num_Solicitud = DECIMAL(Creditos.Num_Solicitud:SCREEN-VALUE)
                      Mov_Instancias.Instancia     = TProIns.TP_Instancia
                      Mov_Instancias.Cuenta        = STRING(Creditos.Num_Credito)
                      Mov_Instancias.Agencia       = TProIns.TP_Agencia
                      Mov_Instancias.Fec_Ingreso   = W_Fecha
                      Mov_Instancias.Hora_Ingreso  = TIME
                      Mov_Instancias.Usuario       = TProIns.TP_Usuario
                      Mov_Instancias.Estado        = NO.
              END.
          END.
          ENABLE ALL WITH FRAME F_Creditos.
          DISABLE NomUsuario WITH FRAME F_Creditos.
          ENABLE ALL WITH FRAME F_Consulta.
          RELEASE mov_instancias.
          RELEASE hoja_vida.
         /* APPLY "choose" TO Btn_Consulta IN FRAME F_Creditos.*/
          W_AS = YES.
         
       END.
       ELSE DO:
          MESSAGE "No encontro Configuracion para siguiente instancia, despues de la " w_ord
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
       END.
    END.
 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Buscar_Indicadores wWin 
PROCEDURE Buscar_Indicadores :
DO WITH FRAME F_Solicitud:
    IF Pro_Creditos.Id_Tasa = 1 THEN DO:
        FIND FIRST Indicadores WHERE Indicadores.Estado = 1
                                 AND Indicadores.Indicador = Pro_Creditos.Cod_Tasa NO-LOCK NO-ERROR.
        IF NOT AVAILABLE(Indicadores) THEN DO:
            MESSAGE "No exite un indicadores para el plazo, monto y linea" SKIP
                    "de producto de crédito. Consulte con el Administrador" SKIP
                    "del sistema acerca de esta inconsistencia"
                VIEW-AS ALERT-BOX ERROR.

            APPLY "ENTRY" TO Creditos.Monto.
            RETURN NO-APPLY.
        END.

        IF Indicadores.FecVcto < TODAY THEN DO:
            MESSAGE "El indicador para este producto se encuentra vencido" SKIP
                    "la tasa puede estar desactualizada. Consulte con" SKIP
                    "el administrador del sistema acerca de esta inconsistencia"
                VIEW-AS ALERT-BOX ERROR.

            APPLY "ENTRY" TO Creditos.Monto.
            RETURN NO-APPLY.
        END.

        NomIndicador:SCREEN-VALUE = Indicadores.Nombre.
    END.
    ELSE DO:
        IF Creditos.Tasa:SCREEN-VALUE = "0" THEN DO:
            MESSAGE "EL producto de Crèdito permite que el asesor" SKIP
                    "entre la tasa para la solicitud." SKIP(1)
                    "Digite la Tasa para la solicitud en pantalla"
                VIEW-AS ALERT-BOX INFORMATION.

            ASSIGN Creditos.Tasa:BGCOL = 15
                   Creditos.Tasa:FGCOL = 0
                   Creditos.Tasa:SENSITIVE = YES.

            APPLY "entry" TO Creditos.Tasa IN FRAME F_Solicitud.
            RETURN NO-APPLY.
        END.
    END.

    RUN Hallar_Tasa_Nominal.
    RUN Hallar_Tasa_Periodo.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Buscar_Instancias_Cerradas wWin 
PROCEDURE Buscar_Instancias_Cerradas :
FOR EACH TCerradas: DELETE TCerradas. END.
FOR EACH Mov_Instancias WHERE
         Mov_Instancias.Num_Solicitud EQ Creditos.Num_Solicitud AND
         Mov_Instancias.Nit           EQ Creditos.Nit NO-LOCK:
    CREATE TCerradas.
    ASSIGN TCerradas.Instancia      = Mov_Instancias.Instancia
           TCerradas.Estado         = Mov_Instancias.Estado
           TCerradas.Num_Solicitud   = Mov_Instancias.Num_Solicitud
           TCerradas.Usuario        = Mov_Instancias.Usuario
           TCerradas.Fec_Ingreso    = Mov_Instancias.Fec_Ingreso
           TCerradas.Fec_Retiro     = Mov_Instancias.Fec_Retiro
           TCerradas.Hora_Ingreso   = Mov_Instancias.Hora_Ingreso
           TCerradas.Hora_Retiro    = Mov_Instancias.Hora_Retiro
           TCerradas.Descripcion    = Mov_Instancias.Descripcion.
    FIND Instancias WHERE Instancias.Instancia EQ Mov_Instancias.Instancia NO-LOCK NO-ERROR.
    IF AVAILABLE Instancias THEN TCerradas.INom_Instancia = Instancias.Nom_Instancia.
    FIND Usuarios WHERE Usuarios.Usuario EQ Mov_Instancias.Usuario NO-LOCK NO-ERROR.
    IF AVAILABLE Usuario THEN TCerradas.INom_Usuario = Usuarios.Nombre.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Busca_Operacion_Ahorros wWin 
PROCEDURE Busca_Operacion_Ahorros :
IF Creditos.Desembolso EQ 3 OR Creditos.Desembolso EQ 6 OR Creditos.Desembolso EQ 7 THEN DO:
    FIND FIRST Operacion WHERE STRING(Operacion.Cod_Operacion,"999999999") BEGINS "010101"
                           AND Operacion.Ctrl_EfeChe EQ 2
                           AND Operacion.Estado EQ 1
                           AND Operacion.Id_SYA EQ NO NO-LOCK NO-ERROR.
    IF AVAILABLE Operacion THEN DO:
        RUN Grabar_MovAhorros(INPUT Operacion.Cod_Operacion,
                              INPUT Creditos.Cod_Desembolso,
                              INPUT Creditos.Cue_Desembolso,
                              INPUT W_NumCbt,
                              INPUT Creditos.Age_Desembolso,
                              INPUT W_Agencia,
                              INPUT Creditos.Age_Desembolso,
                              INPUT W_Usuario,
                              INPUT 0,
                              INPUT W_VrAlAhorro,
                              INPUT Creditos.Nit) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            MESSAGE "No se pudo grabar el movimiento de ahorros" SKIP
                    "a la cuenta especificada." SKIP(1)
                    "Se cancela la operación de Desembolso"
                VIEW-AS ALERT-BOX ERROR.

            RETURN ERROR.
        END.
    END.
    ELSE DO:
        MESSAGE "No se encontro la operación de Consignación" SKIP
                "de ahorros para el desembolso del crédito." SKIP(1)
                "Se cancela la operación de desembolso!"
            VIEW-AS ALERT-BOX ERROR.

        RETURN ERROR.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Calcular_Deducible wWin 
PROCEDURE Calcular_Deducible :
DEFINE INPUT PARAMETER Ptmo AS DECIMAL.

DEFINE VAR Tot_Aport AS DECIMAL.
DEFINE VAR W_PlaMes AS INTEGER.
DEFINE VAR MultEdad AS INTEGER INITIAL 1.

W_MontoCre = Creditos.Monto.
Tot_Deduc = 0.
val_deduc = 0.
W_Iva = 0.
MultEdad = 1.

IF WK_Edad >= 80 THEN
    MultEdad = 0.
ELSE
    IF WK_Edad >= 70 THEN
        MultEdad = 2.

FOR EACH TDeducc WHERE Tdeducc.valor_impuesto > 0 :
    IF W_Des AND Creditos.Id_Adicionales <> 3 AND Tdeducc.valor_impuesto > 0 THEN DO:
        CREATE TPartidas.
        Tpartidas.Tage = Creditos.Agencia.
        Tpartidas.TCta = TDeducc.Cuenta.
        TPartidas.TDsc = TDeducc.Nom_Deducible + " : " + STRING(Tdeducc.valor_impuesto).
        TPartidas.TDoc = Creditos.Pagare:SCREEN-VALUE IN FRAME F_Ultima.
        TPartidas.TTip = 3.
        TPartidas.TOpe = 0.
        TPartidas.TCre = Tdeducc.valor_impuesto.
        
        Val_Deduc = Val_Deduc + TDeducc.Valor_Impuesto.

        IF TDeducc.Id_Impuesto THEN DO:
            MESSAGE "F TDeducc.Id_Impuesto THEN DO: " "Impto : " + STRING(TDeducc.Valor_Impuesto)
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            CREATE TPartidas.
            Tpartidas.Tage = Creditos.Agencia.
            Tpartidas.TCta = TDeducc.Cuenta_Impuesto.
            TPartidas.TDsc = "Impto : " + STRING(TDeducc.Valor_Impuesto).
            TPartidas.TDoc = Creditos.Pagare:SCREEN-VALUE IN FRAME F_Ultima.
            TPartidas.TTip = 3.
            TPartidas.TOpe = 0.
            TPartidas.TCre = TDeducc.Valor_Impuesto.

            Tot_Deduc = Tot_Deduc + TDeducc.Valor_Impuesto.
            W_Iva = Tot_Deduc + TDeducc.Valor_Impuesto.
            W_VrImpto = W_VrImpto + TDeducc.Valor_Impuesto.
        END.
    END.
END.

Val_Deduc = Val_Deduc + W_FaltApor.
W_vrImpto = 0.
val_deduc = 0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Canc_Creditos wWin 
PROCEDURE Canc_Creditos :
DEFINE VAR W_RowIdCr AS ROWID.
DEFINE VAR NO_Vlr AS DECIMAL.
DEFINE VAR vSobrantePago AS DECIMAL.
DEFINE VAR vPagoCapital AS DECIMAL.
DEFINE VAR vAgenciaCredito AS INTEGER.
DEFINE VAR pError AS LOGICAL.
DEFINE VAR valorPago AS DECIMAL.
DEFINE VAR vMontoCredito AS DECIMAL.

W_RowIdCr = ROWID(Creditos).
W_CancCap = 0.
vAgenciaCredito = Creditos.agencia.
vMontoCredito = creditos.monto.

vPagoCreditosTotal = 0.

FOR EACH solicitudes_pagoCreditos WHERE solicitudes_pagoCreditos.cliente_id = solicitud.nit
                                    AND solicitudes_pagoCreditos.num_solicitud = solicitud.num_solicitud NO-LOCK:
    FIND FIRST Creditos WHERE Creditos.Nit = solicitudes_pagoCreditos.cliente_id
                          AND Creditos.Num_Credito = solicitudes_pagoCreditos.num_credito
                          AND Creditos.Estado = 2 NO-LOCK NO-ERROR.
    IF AVAILABLE creditos THEN DO:
        IF solicitudes_pagoCreditos.pagoTotal = TRUE THEN
            valorPago = Creditos.Sdo_Capital + Creditos.Int_Corrientes + creditos.Int_DifCobro + Creditos.Int_MoraDifCob + Creditos.Int_MorCobrar + Creditos.Costas + Creditos.Polizas + Creditos.Honorarios - Creditos.Int_Anticipado.
        ELSE DO:
            IF solicitudes_pagoCreditos.valorAbono < Creditos.Sdo_Capital + Creditos.Int_Corrientes + Creditos.Int_DifCobro + Creditos.Int_MoraDifCob + Creditos.Int_MorCobrar + Creditos.Costas + Creditos.Polizas + Creditos.Honorarios - Creditos.Int_Anticipado THEN
                valorPago = solicitudes_pagoCreditos.valorAbono.
            ELSE
                valorPago = Creditos.Sdo_Capital + Creditos.Int_Corrientes + Creditos.Int_DifCobro + Creditos.Int_MoraDifCob + Creditos.Int_MorCobrar + Creditos.Costas + Creditos.Polizas + Creditos.Honorarios - Creditos.Int_Anticipado.
        END.

        /* Revisamos si el monto del crédito alcanza para cubrir lo correspondiente a la obligación, y en caso que no se modifica el valor a abonar */
        IF vPagoCreditosTotal + valorPago > vMontoCredito THEN DO:
            MESSAGE "El valor a abonarse al crédito #" creditos.num_credito "era de" STRING(valorPago,"$>>>,>>>,>>9.99") "." SKIP
                    "Sin embargo, de acuerdo al valor a desembolsar únicamente se podrá abonar" STRING(vMontoCredito - vPagoCreditosTotal,"$>>>,>>>,>>9.99") "."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            valorPago = vMontoCredito - vPagoCreditosTotal.
        END.

        RUN reportaListaSuspendidos.

        RUN p-pagoCredito.r (INPUT YES,
                             INPUT Creditos.Cod_Credito,
                             INPUT Creditos.Nit,
                             INPUT Creditos.Num_Credito,
                             INPUT valorPago,
                             INPUT W_Cbte,
                             INTEGER(W_NumCbt),
                             INPUT 0,
                             INPUT 1,
                             INPUT w_fecha,
                             INPUT TRUE,
                             OUTPUT NO_Vlr,
                             OUTPUT NO_Vlr,
                             OUTPUT NO_Vlr,
                             OUTPUT NO_Vlr,
                             OUTPUT NO_Vlr,
                             OUTPUT NO_Vlr,
                             OUTPUT NO_Vlr,
                             OUTPUT NO_Vlr,
                             OUTPUT NO_Vlr,
                             OUTPUT NO_Vlr,
                             OUTPUT vPagoCapital,
                             OUTPUT vSobrantePago,
                             OUTPUT pError) NO-ERROR.

        vPagoCreditosTotal = vPagoCreditosTotal + valorPago - vSobrantePago.

        /* Si el crédito a cancelar se encuentra en una agencia distinta a la agencia donde se está haciendo el desembolso se hace las partidas correspondientes a S&A */
        IF vAgenciaCredito <> creditos.agencia THEN DO:
            CREATE TPartidas.
            TPartidas.TAge = creditos.agencia.
            TPartidas.TCta = W_CtaSyA.
            TPartidas.TDsc = "Sucursales y Agencias".
            TPartidas.TCed = STRING(vAgenciaCredito,"999").
            TPartidas.Tdeb = Creditos.Sdo_Capital + Creditos.Int_Corrientes + Creditos.Int_DifCobro + Creditos.Int_MoraDifCob + Creditos.Int_MorCobrar  + Creditos.Costas + Creditos.Polizas + Creditos.Honorarios - Creditos.Int_Anticipado.

            CREATE TPartidas.
            TPartidas.TAge = vAgenciaCredito.
            TPartidas.TCta = W_CtaSyA.
            TPartidas.TDsc = "Sucursales y Agencias".
            TPartidas.TCed = STRING(creditos.agencia,"999").
            TPartidas.TCre = Creditos.Sdo_Capital + Creditos.Int_Corrientes + Creditos.Int_DifCobro + Creditos.Int_MoraDifCob + Creditos.Int_MorCobrar  + Creditos.Costas + Creditos.Polizas + Creditos.Honorarios - Creditos.Int_Anticipado.
        END.

        W_CancCap = W_CancCap + vPagoCapital.

        FOR EACH Relaciones WHERE Relaciones.Nit = Creditos.Nit
                              AND Relaciones.Cuenta = TRIM(STRING(Creditos.Num_Credito))
                              AND Relaciones.Clase_Producto = 2
                              AND Relaciones.Cod_Producto = Creditos.Cod_Credito
                              AND Relaciones.Cod_Relacion = 11
                              AND Relaciones.Estado = 1:
            Relaciones.Estado = 2.
        END.
    END.
END.

FIND FIRST Creditos WHERE ROWID(Creditos) EQ W_RowIdCr NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cargaTexto wWin 
PROCEDURE cargaTexto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* ASSIGN vcCondiciones =                                                                                                                        */
/*     ffrmto("7.1 El  seguro  de  cartera  se  liquida sobre  el saldo de  manera anticipada. Únicamente ampara del riesgo de muerte o ",120) + */
/*     "incapacidad total y permanente del deudor solicitante de edad hasta 70 años y hasta 400 SMLMV  siempre  y  cuando cumpla " +             */
/*     "con las exigencias de la Compañía de Seguros." +                                                                                         */
/*     "~n" +                                                                                                                                    */
/*     "7.2 El seguro de cartera para solicitantes mayores de 70 años cubre créditos hasta 200 SMLMV. Para incapacidades cubre a " +             */
/*     "solicitantes  hasta  los 65  años, para solicitantes mayores a esta edad que deseen mayor cobertura deberán cancelar una " +             */
/*     "prima adicional." +                                                                                                                      */
/*     "~n" +                                                                                                                                    */
/*     "7.3 Por  política  interna  los créditos con  mora  igual  o  superior a 90  días son exigibles en forma inmediata, pero " +             */
/*     "Juriscoop se reserva el derecho de empezar la cobranza en cualquier momento del vencimiento" +                                           */
/*     "~n" +                                                                                                                                    */
/*     "7.4 El  deudor  se  obliga  a renovar y mantener vigentes las pólizas de seguros constituidas a favor de las garantías a " +             */
/*     "favor de Juriscoop." +                                                                                                                   */
/*     "~n" +                                                                                                                                    */
/*     "7.5 Los gastos ocasionados por el perfeccionamiento de las garantías, serán a cargo del solicitante." +                                  */
/*     "~n" +                                                                                                                                    */
/*     "7.6 Para los créditos que a  criterio  de Juriscoop o por normatividad requieran evaluación periódica, el solicitante se " +             */
/*     "compromete a actualizar la información financiera, legal y de la garantía." +                                                            */
/*     "~n" +                                                                                                                                    */
/*     "7.7 El deudor se  compromete  a  actualizar los  certificados  de  libertad de los bienes inmuebles y vehículos dados en " +             */
/*     "garantía, en caso contrario Juriscoop los actualizará y los gastos serán a cargo del deudor." +                                          */
/*     "~n" +                                                                                                                                    */
/*     "7.8 A los créditos  de  la línea Fidelización y Compra de Cartera que incurran en mora de mas de 30 días se les ajustará " +             */
/*     "la  tasa  a la  máxima vigente  para créditos  de consumo y ordinario  certificada por la Superintendencia Financiera de " +             */
/*     "Colombia. Como  consecuencia  de  lo anterior, me obligo a pagar a Juriscoop o a quien represente sus derechos, el valor " +             */
/*     "correspondiente  a  los  intereses remuneratorios y  moratorios, liquidados de conformidad a lo previsto en el manual de " +             */
/*     "crédito, aceptando de antemano las variaciones que pueda sufrir la tasa de interés en aplicación de las condiciones allí " +             */
/*     "estipuladas." +                                                                                                                          */
/*     "~n" +                                                                                                                                    */
/*     "Igualmente autorizo para que en el evento que resulten modificadas las condiciones del crédito, los descuentos mensuales " +             */
/*     "sean los que Juriscoop notifique a la pagaduría, de conformidad con la libranza que para el efecto he suscrito." +                       */
/*     "~n" +                                                                                                                                    */
/*     "7.9 En  el evento que  por disposición legal o reglamentaria se autorice a cobrar intereses inferiores a los pactados en " +             */
/*     "esta obligación tanto ordinarios como de mora, JURISCOOP los reajustará automáticamente, pero cuando dichos intereses se " +             */
/*     "vuelvan a incrementar se podrán reajustar hasta la tasa máxima pactada para esta obligación." +                                          */
/*     "~n" +                                                                                                                                    */
/*     "7.10 Para  operaciones amparadas  con  el  fondo  de garantías, Juriscoop renovará los certificados y los gastos serán a " +             */
/*     "cargo del deudor." +                                                                                                                     */
/*     "~n" +                                                                                                                                    */
/*     "7.11 Declaro  que conozco  y por autonomía de mi libertad, ACEPTO los términos estipulados en la 'Solicitud de Productos " +             */
/*     "Financieros y Actualización de Datos'." +                                                                                                */
/*     "~n" + "~n" +                                                                                                                             */
/*     "Estimado  Cliente: Usted  tienen  derecho a solicitar la última calificación otorgada a su crédito, así como los motivos " +             */
/*     "que  la  generaron, por  tal razón Juriscoop esta en la obligación de responder en un tiempo no mayor a quince (15) días " +             */
/*     "después  del  requerimiento  formal  y cancelado  los  costos administrativos y  de papelería. Los estamentos de control " +             */
/*     "interno verificarán este cumplimiento." +                                                                                                */
/*     "~n" + "~n" +                                                                                                                             */
/*     "Firma del Deudor                          Firma del Codeudor Solidario             Firma del Codeudor Solidario" +                       */
/*     "~n" + "~n" + "~n" + "~n" + "~n" +  "~n" + "~n" +                                                                                         */
/*     "_______________________                   ____________________________             ____________________________" +                       */
/*     "~n" +                                                                                                                                    */
/*     "Nombre:                                   Nombre:                                  Nombre:".                                             */
/*                                                                                                                                               */
/*                                                                                                                                               */
/*                                                                                                                                               */
/*                                                                                                                                               */
/*                                                                                                                                               */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cndciones wWin 
PROCEDURE Cndciones :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    RUN pCndciones.
    FOR EACH tmpi NO-LOCK:
            PUT UNFORMATTED tmpi.itexto SKIP.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Condicionar wWin 
PROCEDURE Condicionar :
IF E_Condicion:SCREEN-VALUE IN FRAME F_Condicionada EQ "" THEN DO:
    MESSAGE "Debe llenarse la condición para que" SKIP
            "el usuario que la reciba, pueda saber" SKIP
            "la acción a seguir. Rectifique!" VIEW-AS ALERT-BOX.
    APPLY "Entry" TO E_Condicion IN FRAME F_Condicionada.
    RETURN NO-APPLY.
 END.
 FIND Mov_Instancias WHERE 
      Mov_Instancias.Instancia EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) AND
      Mov_Instancias.Nit       EQ STRING(Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud) AND
      Mov_Instancias.Usuario   EQ W_Usuario AND
      Mov_Instancias.Cuenta    EQ Creditos.Num_Credito:SCREEN-VALUE IN FRAME F_Solicitud AND
      Mov_Instancias.Num_Solicitud EQ INTEGER(Creditos.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud) AND
      Mov_Instancias.Estado    EQ NO NO-ERROR.
 IF AVAILABLE Mov_Instancias THEN DO:
    ASSIGN Mov_Instancias.Fec_Retiro  = TODAY.
           Mov_Instancias.Hora_Retiro = TIME.
           Mov_Instancias.Estado      = YES. 
 END.
 ELSE DO:
    MESSAGE "No se encuentra el Mov_Instancias actual" VIEW-AS ALERT-BOX.
    APPLY "entry" TO Cmb_InsCon IN FRAME F_Condicionada.
 END.
 FIND CURRENT Mov_Instancias NO-LOCK NO-ERROR.
 CREATE Hoja_Vida.
 ASSIGN Hoja_Vida.Tipo       = 9 
        Hoja_Vida.Codigo     = 1  
        Hoja_Vida.Instancia  = INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5))
        Hoja_Vida.DoctoRefer = INTEGER(Creditos.Num_Credito:SCREEN-VALUE IN FRAME F_Solicitud)
        Hoja_Vida.Nit        = Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud
        Hoja_Vida.Usuario    = W_Usuario
        Hoja_Vida.Fec_Grabacion = TODAY
        Hoja_Vida.Hora_Grabacion = TIME
        Hoja_Vida.Observacion = 
        "Se Condiciona la aprobación: " + STRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos) +
        " - Procesada por Usuario: " + W_Usuario
        Hoja_Vida.Asunto_Cumplido = YES.
 /*se crea registro en hoja de vida para el usuario al cual se le asigna la solicitud*/
 CREATE Hoja_Vida.
 ASSIGN Hoja_Vida.Tipo        = 9 
        Hoja_Vida.Codigo      = 1  
        Hoja_Vida.Instancia   = INTEGER(SUBSTRING(Cmb_InsCon:SCREEN-VALUE IN FRAME F_Condicionada,1,3))
        Hoja_Vida.DoctoRefer  = INTEGER(Creditos.Num_Credito:SCREEN-VALUE IN FRAME F_Solicitud)
        Hoja_Vida.Nit         = Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud
        Hoja_Vida.Usuario     = Tuxi.Usuario
        Hoja_Vida.Fec_Grabacion = TODAY
        Hoja_Vida.Hora_Grabacion = TIME
        Hoja_Vida.Asunto_Cumplido = NO.
        Hoja_Vida.Observacion = "Condicion: " + E_Condicion:SCREEN-VALUE IN FRAME F_Condicionada 
                              + " - por usuario: " + W_Usuario.
 FIND FIRST Mov_Instancias WHERE
            Mov_Instancias.Cuenta        EQ Creditos.Num_Credito:SCREEN-VALUE IN FRAME F_Solicitud AND
            Mov_Instancias.Num_Solicitud EQ INTEGER(Creditos.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud) AND
            Mov_Instancias.Usuario       EQ Tuxi.Usuario AND
            Mov_instancias.Nit           EQ Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud
            NO-ERROR.
 IF NOT AVAILABLE Mov_Instancias THEN CREATE Mov_Instancias.
 ASSIGN Mov_Instancias.Fec_Ingreso   = TODAY.
        Mov_Instancias.Hora_Ingreso  = TIME.
        Mov_Instancias.Estado        = NO.
        Mov_Instancias.Nit           = Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud.
        Mov_Instancias.Num_Solicitud = DECIMAL(Creditos.Num_Solicitud:SCREEN-VALUE).
        Mov_Instancias.Cuenta        = Creditos.Num_Credito:SCREEN-VALUE IN FRAME F_Solicitud.
        Mov_Instancias.Usuario       = Tuxi.Usuario.
        Mov_Instancias.Instancia     = INTEGER(SUBSTRING(Cmb_InsCon:SCREEN-VALUE IN FRAME F_Condicionada,1,3)).
 FIND CURRENT Mov_Instancias NO-LOCK NO-ERROR.
 Solicitud.Estado = 4.
 ENABLE ALL WITH FRAME F_Creditos.
 DISABLE NomUsuario WITH FRAME F_Creditos.
 ENABLE ALL WITH FRAME F_Consulta.
 APPLY "choose" TO Btn_Consulta IN FRAME F_Creditos. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Consul_Gtias wWin 
PROCEDURE Consul_Gtias :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  OPEN QUERY Br_Admisible
       FOR EACH Garantias NO-LOCK WHERE Garantias.Agencia   EQ Creditos.Agencia AND
                                        Garantias.Cod_Credito   EQ Creditos.Cod_Credito AND
                                        Garantias.Tip_Credito   EQ Creditos.Tip_Credito AND
                                        Garantias.Num_Solicitud EQ Creditos.Num_Solicitud AND
                                        Garantias.Nit           EQ Creditos.Nit           AND
                                        Garantias.Estado        EQ R_ConAdm  INDEXED-REPOSITION.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Contabilizar_Partidas wWin 
PROCEDURE Contabilizar_Partidas :
DEFINE VAR numCheque AS INTEGER.

IF W_Cbte EQ 0 OR W_NumCbt EQ 0 THEN DO:
    MESSAGE "No se ha encontrado el Cpte o el número consecutivo" SKIP
            "se cancela la operación de desembolso!"
        VIEW-AS ALERT-BOX ERROR.

    RETURN ERROR.
END.

FOR EACH TPartidas WHERE TPartidas.tDeb > 0 OR TPartidas.TCre > 0:
    IF TPartidas.TAge LE 0 THEN DO:
        MESSAGE "Existe una Contrapartida sin Agencia, No se acepta la Operacion." SKIP
                "Cuenta " TPartidas.TCta "   Comprobante: " W_Cbte
            VIEW-AS ALERT-BOX ERROR.

        RETURN ERROR.
    END.

    CREATE Mov_Contable.
    ASSIGN Mov_Contable.Agencia = TPartidas.TAge
           Mov_Contable.Comprobante = W_Cbte
           Mov_Contable.Cuenta = TPartidas.TCta
           Mov_Contable.Fec_Contable = W_Fecha
           Mov_Contable.Comentario = TPartidas.TDsc
           Mov_Contable.Usuario = W_Usuario
           Mov_contable.Nit = Creditos.Nit
           Mov_Contable.Cen_Costos = 999
           Mov_Contable.Destino = W_Agencia
           Mov_Contable.Num_Documento = INTEGER(W_NumCbt)
           Mov_Contable.Doc_Referencia = Creditos.Pagare
           Mov_Contable.Enlace = string(Creditos.Num_Credito)
           Mov_Contable.Fec_Grabacion = TODAY
           Mov_Contable.Hora = TIME
           Mov_Contable.Estacion = W_Estacion NO-ERROR.

    IF TPartidas.TCta EQ W_CtaTerceros THEN
        Mov_contable.Nit = TPartidas.TCed.
    ELSE
        IF TPartidas.TCta EQ W_CtaSyA THEN
            Mov_contable.Nit = TPartidas.TCed.

    IF Mov_Contable.Comentario = "Aporte-Dif. x Desemb SyA" THEN
        Mov_contable.Nit = Tpartidas.TCed.

    IF TPartidas.TDeb GT 0 THEN
        ASSIGN Mov_Contable.DB = TPartidas.TDeb NO-ERROR.
    ELSE
        ASSIGN Mov_Contable.CR = TPArtidas.TCre NO-ERROR.

    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "Error Grabando Mov_Contable, en Proc.Contabilizar_Partidas."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN ERROR.
    END.

    numCheque = INTEGER(num_cheque:SCREEN-VALUE IN FRAME F_Bancos) NO-ERROR.

    IF numCheque > 0 OR num_cheque:SCREEN-VALUE = "TRANSF" THEN DO:
        mov_contable.doc_referencia = num_cheque:SCREEN-VALUE.
    END.

    ERROR-STATUS:ERROR = FALSE.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load wWin  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "W-Proceso_Desembolso.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "W-Proceso_Desembolso.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CortoLargoAhorros wWin 
PROCEDURE CortoLargoAhorros :
FIND FIRST CortoLargo WHERE CortoLargo.Agencia EQ Creditos.Age_Desembolso
                        AND CortoLargo.Cod_Producto EQ Creditos.Cod_Desembolso
                        AND CortoLargo.Clase_Producto EQ 1
                        AND CortoLargo.Cta_AsoAd NE ""
                        AND CortoLargo.Cta_NoAAD NE ""
                        AND CortoLargo.Comprobante NE 0 NO-LOCK NO-ERROR.
IF NOT AVAILABLE CortoLargo THEN DO:
    MESSAGE " ahorros No se ha encontrado la configuración de Corto y largo" SKIP
            "o existe algun tipo de inconsistencia en su configuración" SKIP
            "Comunique esta inconsistencia al Administrador del Sistema"
        VIEW-AS ALERT-BOX ERROR.

    RETURN ERROR.
END.
ELSE DO:
    CASE Clientes.Tipo_Vinculo:
        WHEN 1 THEN W_CtaCorAho = CortoLargo.Cta_AsoAd.
        WHEN 5 THEN W_CtaCorAho = CortoLargo.Cta_NoAAd.

        OTHERWISE DO:
            MESSAGE "La  persona identificada para el crédito no es Asociado/Empleado del" SKIP
                    "Fondo. No se pueden hacer créditos a personas diferentes a estos dos" SKIP
                    "tipo de vínculo. Se cancela la operación de desembolso!"
                VIEW-AS ALERT-BOX ERROR.

            RETURN ERROR.
        END.
    END CASE.

    /* oakley */
    
    IF W_CtaCorAho EQ "" THEN DO:
        MESSAGE "La cuenta de corto y largo no esta configurada" SKIP
                "para el producto de crédito. se cancela la operación" SKIP
                "de desembolso!" VIEW-AS ALERT-BOX ERROR. 
        RETURN ERROR.
     END.
     
     /*W_MontoCre = Creditos.Monto.
    FOR EACH TPartidas WHERE TPartidas.TTip EQ 3:
        IF Creditos.Id_Adicionales EQ 2 THEN
           W_MontoCre = W_MontoCre - TPartidas.TCre.
    END.*/

    CREATE TPartidas.
    ASSIGN Tpartidas.Tage = Creditos.Age_Desembolso   /*Tenía Creditos.Agencia, Feb.12/07*/
           Tpartidas.TCta = W_CtaCorAho
           TPartidas.TDsc = "Desemb.a Cta Ahorros Age: " + STRING(Creditos.Agencia) + "Pro: " + STRING(Creditos.Cod_Desembolso)
           TPartidas.TDoc = STRING(Creditos.Cue_Desembolso)
           TPartidas.TTip = 1
           TPartidas.TOpe = 0
           TPartidas.TCre = W_VrADesemb - W_IntAntic.        

    IF Creditos.Age_Desembolso NE Creditos.Agencia THEN DO:   /*Suc.Agenc Feb.12/07*/
       CREATE TPartidas.
       ASSIGN Tpartidas.Tage = Creditos.Age_Desembolso
           Tpartidas.TCta = CortoLargo.Cta_Sya
           TPartidas.TDsc = "Desemb.a Cta Ahorros de: " + STRING(Creditos.Agencia) + "Pro: " + STRING(Creditos.Cod_Desembolso)
           TPartidas.TDoc = STRING(Creditos.Cue_Desembolso)
           TPartidas.TTip = 1
           TPartidas.TOpe = 0
           Tpartidas.TCed = STRING(Creditos.Agencia,"999")
           TPartidas.TDeb = W_VrADesemb - W_IntAntic. 

       CREATE TPartidas.
       ASSIGN Tpartidas.Tage = Creditos.Agencia
           Tpartidas.TCta = CortoLargo.Cta_Sya
           TPartidas.TDsc = "Desemb.a Cta Ahorros Age: " + STRING(Creditos.Age_Desembolso) + "Pro: " + STRING(Creditos.Cod_Desembolso)
           TPartidas.TDoc = STRING(Creditos.Cue_Desembolso)
           TPartidas.TTip = 1
           TPartidas.TOpe = 0
           Tpartidas.TCed = STRING(Creditos.Age_Desembolso,"999")
           TPartidas.TCre = W_VrADesemb - W_IntAntic.     

    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CortoLargoCreditos wWin 
PROCEDURE CortoLargoCreditos :
FIND FIRST CortoLargo WHERE CortoLargo.Agencia = INTEGER(SUBSTRING(Cmb_Agencias:SCREEN-VALUE IN FRAME F_Solicitud,1,3))
                        AND CortoLargo.Clase_Producto = 2
                        AND CortoLargo.Cod_Producto = INTEGER(SUBSTRING(Nom_Producto:SCREEN-VALUE IN FRAME F_Solicitud,1,3))
                        AND CortoLargo.Cta_ContingenteDB <> ""
                        AND CortoLargo.Cta_ContingenteCR <> ""
                        AND CortoLargo.Comprobante <> 0 NO-LOCK NO-ERROR.
IF NOT AVAILABLE CortoLargo THEN DO:
    MESSAGE "No se ha encontrado la configuración de Corto y largo" SKIP
            "o existe algún tipo de inconsistencia en su configuración." SKIP
            "Comunique esta inconsistencia al Administrador del Sistema"
        VIEW-AS ALERT-BOX ERROR.

    RETURN ERROR.
END.
ELSE DO:
    W_CtaSyA = CortoLargo.Cta_SYA.

    IF NOT W_Anular THEN DO:
        RUN Partidas_GarAdmisible NO-ERROR. /* oakley */
        IF ERROR-STATUS:ERROR THEN
            RETURN ERROR.
    END.

    /*partidas de reversión de la aprobación*/
     /*W_Cbte = CortoLargo.Comprobante. Es el mismo para todo el desembolso*/
     CREATE TPartidas.
     ASSIGN Tpartidas.Tage = Creditos.Agencia
            Tpartidas.TCta = CortoLargo.Cta_ContingenteDB
            TPartidas.TDsc = "Reversión Monto Solicitud Ctas Orden"
            TPartidas.TDoc = STRING(Creditos.Num_Solicitud)
            TPartidas.TTip = 2
            TPartidas.TOpe = 0
            TPartidas.TCre = Creditos.Monto.

     CREATE TPartidas.
     ASSIGN Tpartidas.Tage = Creditos.Agencia
            Tpartidas.TCta = CortoLargo.Cta_ContingenteCR
            TPartidas.TDsc = "Reversión Monto Solicitud Ctas Orden"
            TPartidas.TDoc = STRING(Creditos.Num_Solicitud)
            TPartidas.TTip = 2
            TPartidas.TOpe = 0
            TPartidas.TDeb = Creditos.Monto.


     CASE Clientes.Tipo_Vinculo:
       WHEN 1 THEN W_CtaCorCre = CortoLargo.Cta_AsoAd.
       OTHERWISE DO:
           W_CtaCorCre = CortoLargo.Cta_NoAAd.
       END.
     END CASE.

     IF W_CtaCorCre EQ "" THEN DO:
        MESSAGE "La cuenta de corto y largo no esta configurada" SKIP
                "para el producto de crédito. se cancela la operación" SKIP
                "de desembolso!" VIEW-AS ALERT-BOX ERROR. 
        RETURN ERROR.
     END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Crea_HojaV wWin 
PROCEDURE Crea_HojaV :
/*------------------------------------------------------------------------------
  Purpose:     
  Notes:       
------------------------------------------------------------------------------*/
  CREATE Hoja_Vida.
  ASSIGN Hoja_Vida.Tipo           = 9 
         Hoja_Vida.Codigo         = 1  
         Hoja_Vida.Instancia      = INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5))
         Hoja_Vida.DoctoRefer     = INTEGER(Creditos.Num_Credito:SCREEN-VALUE IN FRAME F_Solicitud)
         Hoja_Vida.Nit            = Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud
         Hoja_Vida.Usuario        = W_Usuario
         Hoja_Vida.Fec_Grabacion  = TODAY
         Hoja_Vida.Hora_Grabacion = TIME
         Hoja_Vida.Observacion    = "Credito Desembolsado por el usuario: " + 
                                     W_Usuario + NomUsuario:SCREEN-VALUE IN FRAME F_Creditos
         Hoja_Vida.Asunto_Cumplido = YES.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Creditos_X_Instancia wWin 
PROCEDURE Creditos_X_Instancia :
EMPTY TEMP-TABLE Consulta NO-ERROR.
  FOR EACH Mov_Instancias WHERE
           Mov_Instancias.Instancia EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) AND
           Mov_Instancias.Usuario   EQ W_Usuario AND
           Mov_Instancias.Estado    EQ NO:
     FIND Creditos WHERE /* Creditos.Agencia      EQ W_Agencia AND */
                         Creditos.Nit          EQ Mov_Instancias.Nit AND
                         Creditos.Num_Credito  EQ INTEGER(Mov_Instancias.Cuenta) AND
                         Creditos.Estado       EQ 1 NO-LOCK NO-ERROR.

     IF AVAILABLE Creditos THEN DO:
       CREATE Consulta.
       ASSIGN Consulta.AgeCredito    = Creditos.Agencia
              Consulta.Num_Solicitud = Creditos.Num_Solicitud
              Consulta.Estado        = Creditos.Estado
              Consulta.Num_Credito   = DECIMAL(Mov_Instancias.Cuenta)
              Consulta.Fec_Ingreso   = Mov_Instancias.Fec_Ingreso
              Consulta.Hor_Ingreso   = STRING(Mov_Instancias.Hora_Ingreso,"HH:MM:SS am")
              Consulta.Monto         = Creditos.Monto
              Consulta.Vigencia      = TODAY - Mov_Instancias.Fec_Ingreso.

       ASSIGN W_VrADesemb = Creditos.Monto - (WDed + W_VrCredACanc +  W_FaltApor)
              W_VrADesemb:SCREEN-VALUE IN FRAME F_Solicitud = STRING(W_VrADesemb).

       FIND Clientes WHERE Clientes.Nit EQ Mov_Instancias.Nit NO-LOCK NO-ERROR.
       IF AVAILABLE Clientes THEN
          ASSIGN Consulta.Nit         = Clientes.Nit
                 Consulta.Nombre      = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
     END.    
  END.
  OPEN QUERY Br_Consulta FOR EACH Consulta NO-LOCK BY Consulta.Num_Credito INDEXED-REPOSITION.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Deducc_Final wWin 
PROCEDURE Deducc_Final :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFI VAR P_BaseGmf  LIKE Creditos.Sdo_Capital. 
  DEFI VAR Val_Deduc  LIKE Creditos.Sdo_Capital. 
  
  IF Creditos.Desembolso EQ 1 OR Creditos.Monto LE 1000000 THEN 
     P_BaseGmf = Creditos.Monto - W_Iva.
  ELSE 
     P_BaseGmf = Tot_Deduc + W_CancCap - W_Iva.

  FIND FIRST Deducible WHERE Deducible.Cod_Deducible EQ "6   " NO-LOCK NO-ERROR.
  ASSIGN Tot_Deduc = Tot_Deduc + ROUND(P_BaseGmf * Deducible.Valor,0) .   
         Val_Deduc = ROUND(P_BaseGmf * Deducible.Valor,0).    
               
  IF Val_Deduc GT 0 THEN DO: /*creo partidas para contabilizacion de deducibles*/
     CREATE TPartidas.
     ASSIGN Tpartidas.Tage = Creditos.Agencia
            Tpartidas.TCta = Deducible.Cuenta
            TPartidas.TDsc = Deducible.Nom_Deducible + " : " + STRING(Deducible.Valor)
            TPartidas.TDoc = Creditos.Pagare:SCREEN-VALUE IN FRAME F_Ultima
            TPartidas.TTip = 3
            TPartidas.TOpe = 0
            TPartidas.TCre = Val_Deduc.

     ASSIGN W_MontoCre   = W_MontoCre - Val_Deduc
            W_NetoDesemb = W_MontoCre.
  END.

  ASSIGN W_MontoCre   = W_MontoCre   - W_VrCredACanc
         W_NetoDesemb = W_NetoDesemb - W_VrCredACanc
         W_VrAlAhorro = W_NetoDesemb.

  IF Creditos.Desembolso EQ 3 THEN DO:
     ASSIGN Val_Deduc = ROUND(W_VrAlAhorro * Deducible.Valor,0).

     CREATE TPartidas.
     ASSIGN Tpartidas.Tage = Creditos.Agencia
            Tpartidas.TCta = Deducible.Cuenta
            TPartidas.TDsc = Deducible.Nom_Deducible + " : " + STRING(Deducible.Valor)
            TPartidas.TDoc = Creditos.Pagare:SCREEN-VALUE IN FRAME F_Ultima
            TPartidas.TTip = 3
            TPartidas.TOpe = 0
            TPartidas.TCre = Val_Deduc.

      ASSIGN W_VrAlAhorro = W_VrAlAhorro /*- Val_Deduc*/
             W_MontoCre   = W_MontoCre   - Val_Deduc
             W_NetoDesemb = W_NetoDesemb - Val_Deduc.
  END.

  ASSIGN W_VrAlAhorro = W_VrAlAhorro - W_IntAntic /*nh*/
         W_MontoCre   = W_MontoCre   - W_IntAntic
         W_NetoDesemb = W_NetoDesemb - W_IntAntic.
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Deducc_GMF wWin 
PROCEDURE Deducc_GMF :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:    Deducci¢n a cargo de la cooperativa  
------------------------------------------------------------------------------*/
  DEFI VAR  w_totbaseGMF  AS DECIMAL INITIAL 0.00.
  FIND FIRST Deducible WHERE TRIM(Deducible.Cod_Deducible) EQ "1" NO-LOCK NO-ERROR.
  IF Deducible.Valor GT 0 AND val_deduc GT 0 THEN DO: /*creo partidas para contabilizacion de deducibles*/
     val_GMFTotDed = ROUND( deducible.valor * ( val_deduc  + W_IntAntic + W_VrCredAcanc ), 0 ).
     IF val_GMFTotDed GT 0 THEN DO:
       w_totbaseGMF = w_totbaseGMF + ( val_deduc  + W_IntAntic + W_VrCredAcanc ).
       CREATE TPartidas.
       ASSIGN Tpartidas.Tage = Creditos.Agencia
              Tpartidas.TCta = Deducible.Cuenta
              TPartidas.TDsc = Deducible.Nom_Deducible + " : " + STRING(Deducible.Valor)
              TPartidas.TDoc = Creditos.Pagare:SCREEN-VALUE IN FRAME F_Ultima
              TPartidas.TTip = 3
              TPartidas.TOpe = 0
              TPartidas.TCre = Val_GMFTotDed.

       /* Contrapartida Gasto Cooperativa */
       CREATE TPartidas.
       ASSIGN Tpartidas.Tage = Creditos.Agencia
              Tpartidas.TCta = Deducible.Cuenta_Impuesto
              TPartidas.TDsc = Deducible.Nom_Deducible + " : " + STRING(Deducible.Valor)
              TPartidas.TDoc = Creditos.Pagare:SCREEN-VALUE IN FRAME F_Ultima
              TPartidas.TTip = 3
              TPartidas.TOpe = 0
              TPartidas.TDeb = Val_GMFTotDed.

    /*   ASSIGN w_vrEfectivo = w_vrEfectivo - Val_GMFTotDed
              w_vrAdesemb  = w_vrAdesemb  - Val_GMFTotDed. */
     END.
  END.

  IF Creditos.Desembolso EQ 1 THEN DO:
     ASSIGN Val_GMFEfec = ROUND(W_NetoDesemb * Deducible.Valor,0).
     IF Val_GMFEfec GT 0 THEN DO:
       w_totbaseGMF = w_totbaseGMF + W_NetoDesemb.
       CREATE TPartidas.
       ASSIGN Tpartidas.Tage = Creditos.Agencia
              Tpartidas.TCta = Deducible.Cuenta
              TPartidas.TDsc = Deducible.Nom_Deducible + " : " + STRING(Deducible.Valor)
              TPartidas.TDoc = Creditos.Pagare:SCREEN-VALUE IN FRAME F_Ultima
              TPartidas.TTip = 3
              TPartidas.TOpe = 0
              TPartidas.TCre = Val_GMFEfec.

       /* Contrapartida Gasto Cooperativa */
       CREATE TPartidas.
       ASSIGN Tpartidas.Tage = Creditos.Agencia
              Tpartidas.TCta = Deducible.Cuenta_Impuesto
              TPartidas.TDsc = Deducible.Nom_Deducible + " : " + STRING(Deducible.Valor)
              TPartidas.TDoc = Creditos.Pagare:SCREEN-VALUE IN FRAME F_Ultima
              TPartidas.TTip = 3
              TPartidas.TOpe = 0
              TPartidas.TDeb = Val_GMFEfec.

      /*  ASSIGN w_vrEfectivo = w_vrEfectivo - Val_GMFEfec. */
     END.
  END.
  IF (val_GMFTotDed + Val_GMFEfec ) GT 0  THEN DO:
     CREATE Mov_GMF.
     ASSIGN Mov_GMF.Agencia             = Creditos.agencia
            Mov_GMF.Agencia_Tx          = Creditos.agencia
            Mov_GMF.Documento           = STRING(W_NumCbt)
            Mov_GMF.Fecha               = W_Fecha
            Mov_GMF.Hora                = TIME
            Mov_GMF.Descrip             = "GMF Desembolso de Crédito" 
            Mov_GMF.Id_EfChTras         = 0
            Mov_GMF.Nit                 = Creditos.nit
            Mov_GMF.Porc_Impto          = deducible.valor
            Mov_GMF.Renglon             = 00
            Mov_GMF.Tipo_Pdcto          = 2
            Mov_GMF.VrAcum_RetMes       = 0 /* W_AcuRet No se sabe de que cuenta mostrar acumulado */
            Mov_GMF.Cod_Pdcto           = Creditos.cod_credito
            Mov_GMF.Cpte                = W_Cbte
            Mov_GMF.CtaDb_ImptoCliente  = Deducible.Cuenta_Impuesto
            Mov_GMF.CtaDb_ImptoEntidad  = ""
            Mov_GMF.Cta_ContableCr      = Deducible.Cuenta
            Mov_GMF.Cta_PdctoOContab    = STRING(Creditos.num_credito)
            Mov_GMF.VrBase_Cliente      = w_totbaseGMF
            Mov_GMF.VrBase_Entidad      = 0      
            Mov_GMF.VrBase_Exenta       = 0
            Mov_GMF.VrImpto_Cliente     = val_GMFTotDed + Val_GMFEfec.
            Mov_GMF.VrImpto_Entidad     = 0.

  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Deducibles_Producto wWin 
PROCEDURE Deducibles_Producto :
EMPTY TEMP-TABLE TDeducc.

FIND FIRST Pro_Creditos WHERE Pro_Creditos.Tip_Credito = W_Tippdt
                          AND Pro_Creditos.Cod_Credito = INTEGER(SUBSTRING(Nom_Producto:SCREEN-VALUE IN FRAME F_Solicitud,1,3)) NO-LOCK NO-ERROR.
IF AVAILABLE Pro_Creditos THEN DO:
    RUN Llenar_InfoProducto.

    DO i = 1 TO 10 BY 1:
        IF Pro_Creditos.Deducible[i] <> "" THEN DO:
            FIND FIRST Deducible WHERE Deducible.Cod_Deducible = Pro_Creditos.Deducible[i] NO-LOCK NO-ERROR.
            IF AVAILABLE Deducible THEN DO:
                CREATE TDeducc.
                BUFFER-COPY Deducible TO TDeducc.
            END.
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
  DISPLAY Cmb_Instancias T_Refresh NomUsuario 
      WITH FRAME F_Creditos IN WINDOW wWin.
  ENABLE RECT-2 RECT-3 Cmb_Instancias BUTTON-1 Btn_ProInstancia T_Refresh 
         Btn_Imprimir Btn_Consulta BUTTON-2 BUTTON-4 
      WITH FRAME F_Creditos IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Creditos}
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  DISPLAY Nom_Aseguradora Nom_UsuGarantia W_CredAval W_DispGaran 
      WITH FRAME F_Admisible IN WINDOW wWin.
  IF AVAILABLE Garantias THEN 
    DISPLAY Garantias.Descripcion_Bien2 Garantias.Tipo_Garantia 
          Garantias.Contabilizada Garantias.Estado Garantias.Identificacion_Bien 
          Garantias.Nom_Bien Garantias.Val_Bien Garantias.Fec_Creacion 
          Garantias.Fec_Retiro Garantias.Descripcion_Bien Garantias.Nro_Seguro 
          Garantias.Nit_Aseguradora Garantias.Fec_IniSeguro 
          Garantias.Fec_FinSeguro Garantias.Val_Asegurado Garantias.Nom_Impuesto 
          Garantias.Fec_VctoImpuesto Garantias.Val_Impuesto 
          Garantias.Fec_ProxAvaluo Garantias.Fec_UltAvaluo 
          Garantias.Val_UltAvaluo Garantias.Aprobada 
      WITH FRAME F_Admisible IN WINDOW wWin.
  ENABLE Garantias.Descripcion_Bien2 Garantias.Identificacion_Bien 
         Garantias.Nom_Bien Garantias.Val_Bien Garantias.Nro_Seguro 
         Garantias.Nit_Aseguradora Garantias.Fec_IniSeguro 
         Garantias.Fec_FinSeguro Garantias.Val_Asegurado Garantias.Nom_Impuesto 
         Garantias.Fec_VctoImpuesto Garantias.Val_Impuesto 
         Garantias.Fec_ProxAvaluo Garantias.Fec_UltAvaluo 
         Garantias.Val_UltAvaluo Btn_ConAdm BUTTON-161 Btn_SalAdm Btn_CanAdm 
         Btn_InaAdm RECT-290 RECT-291 RECT-292 RECT-296 RECT-301 
      WITH FRAME F_Admisible IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Admisible}
  DISPLAY W_NitCodeudor W_NomCodeudor RActivas 
      WITH FRAME F_Codeudores IN WINDOW wWin.
  IF AVAILABLE Relaciones THEN 
    DISPLAY Relaciones.Aprobada 
      WITH FRAME F_Codeudores IN WINDOW wWin.
  ENABLE RECT-297 BUTTON-155 RActivas Br_Codeudores 
      WITH FRAME F_Codeudores IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Codeudores}
  DISPLAY R_ConAdm 
      WITH FRAME F_ConAdmisible IN WINDOW wWin.
  ENABLE BR_Admisible Btn_OutConAdm R_ConAdm 
      WITH FRAME F_ConAdmisible IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_ConAdmisible}
  DISPLAY R_Organizar Buscar VG_Normal VG_Media VG_Alta 
      WITH FRAME F_Consulta IN WINDOW wWin.
  ENABLE RECT-223 RECT-287 RECT-288 RECT-289 Br_Consulta R_Organizar 
         Btn_OutConsulta Buscar 
      WITH FRAME F_Consulta IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Consulta}

  {&OPEN-QUERY-F_Solicitud}
  GET FIRST F_Solicitud.
  DISPLAY Cmb_Agencias NomNit Nom_Producto Texto1 Texto2 Cmb_Sistemas 
          Cmb_PerPago W_TasaNominal W_TasaPeriodo NomIndicador W_SdoApor 
          W_PromedDD W_ForPago W_ReqPtmo W_Desembolso W_FaltApor Ded_Ahorros 
          W_VrCredACanc W-fecEntLib W_VrADesemb W_Tipo_Credito 
      WITH FRAME F_Solicitud IN WINDOW wWin.
  IF AVAILABLE Creditos THEN 
    DISPLAY Creditos.Num_Credito Creditos.Num_Solicitud Creditos.Fec_Aprobacion 
          Creditos.Nit Creditos.For_Interes Creditos.Monto Creditos.Plazo 
          Creditos.Cuota Creditos.Deducible Creditos.Id_Adicionales 
          Creditos.Incremento Creditos.Sdo_Capital Creditos.Tasa 
      WITH FRAME F_Solicitud IN WINDOW wWin.
  ENABLE RECT-151 RECT-303 RECT-317 RECT-318 RECT-319 BUTTON-19 W-fecEntLib 
         BUTTON-210 BUTTON-211 BUTTON-212 BUTTON-208 BUTTON-209 BUTTON-233 
         BUTTON-231 BUTTON-232 
      WITH FRAME F_Solicitud IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Solicitud}
  DISPLAY NomAsoc W_FecIni W_CtaTerceros NomCta W_IntAntic W_VrEfectivo 
          W_VrCheque W_VrAlAhorro W_NetoDesemb W_NetoD W_MenDes 
      WITH FRAME F_Ultima IN WINDOW wWin.
  IF AVAILABLE Creditos THEN 
    DISPLAY Creditos.Desembolso Creditos.Estado Creditos.Pagare Creditos.Monto 
      WITH FRAME F_Ultima IN WINDOW wWin.
  ENABLE RECT-224 RECT-225 RECT-228 RECT-229 RECT-304 RECT-305 RECT-308 
         Creditos.Desembolso Btn_SalvaUltima W_FecIni W_VrCheque Btn_OutUltima 
      WITH FRAME F_Ultima IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Ultima}
  DISPLAY W_NetoAD W_FecIni W_CtaTerceros NomCta W_NitTercero NomCed Rs_Desemb 
          W_MenDes 
      WITH FRAME F_Formalizar IN WINDOW wWin.
  IF AVAILABLE Creditos THEN 
    DISPLAY Creditos.Pagare Creditos.Estado 
      WITH FRAME F_Formalizar IN WINDOW wWin.
  ENABLE RECT-227 RECT-306 RECT-309 Btn_SalvaFormal Btn_OutUltima-2 
         Creditos.Estado Rs_Desemb 
      WITH FRAME F_Formalizar IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Formalizar}
  DISPLAY Total_Puntaje 
      WITH FRAME F_Scoring IN WINDOW wWin.
  ENABLE BR_Scoring BUTTON-99 
      WITH FRAME F_Scoring IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Scoring}
  DISPLAY S_InfoCliente 
      WITH FRAME F_InfoCliente IN WINDOW wWin.
  ENABLE S_InfoCliente BUTTON-156 BUTTON-108 
      WITH FRAME F_InfoCliente IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_InfoCliente}
  DISPLAY R_TipoGarantia 
      WITH FRAME F_Garantias IN WINDOW wWin.
  ENABLE RECT-226 R_TipoGarantia Btn_OutGarantias 
      WITH FRAME F_Garantias IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Garantias}
  ENABLE B_Partidas BUTTON-163 
      WITH FRAME F_Partidas IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Partidas}
  DISPLAY S_InfoProducto 
      WITH FRAME F_InfoProducto IN WINDOW wWin.
  ENABLE S_InfoProducto Btn_OutScoring 
      WITH FRAME F_InfoProducto IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_InfoProducto}
  ENABLE Br_Cerradas Btn_OutCerradas BUTTON-154 
      WITH FRAME F_Cerradas IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Cerradas}
  DISPLAY WHora_Ingreso W_Instancia W_UsuarioInstancia Whora_Retiro Vigencia 
      WITH FRAME F_Instancias IN WINDOW wWin.
  IF AVAILABLE Mov_Instancias THEN 
    DISPLAY Mov_Instancias.Fec_Ingreso Mov_Instancias.Fec_Retiro 
          Mov_Instancias.Estado Mov_Instancias.Descripcion 
      WITH FRAME F_Instancias IN WINDOW wWin.
  ENABLE BUTTON-142 Mov_Instancias.Estado Mov_Instancias.Descripcion 
         Btn_GraInstancia Btn_AgregarTXT Btn_insVolver 
      WITH FRAME F_Instancias IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Instancias}
  DISPLAY Nom_Beneficiario Num_Cheque 
      WITH FRAME F_Bancos IN WINDOW wWin.
  ENABLE Bancos Nom_Beneficiario Num_Cheque 
      WITH FRAME F_Bancos IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Bancos}
  DISPLAY Rs_SiNo 
      WITH FRAME F_ImpCpte IN WINDOW wWin.
  ENABLE RECT-315 Rs_SiNo 
      WITH FRAME F_ImpCpte IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_ImpCpte}
  DISPLAY Cmb_InsCon E_Condicion 
      WITH FRAME F_Condicionada IN WINDOW wWin.
  ENABLE Cmb_InsCon Br_Usuarios E_Condicion 
      WITH FRAME F_Condicionada IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Condicionada}
  ENABLE Br_Deducibles BUTTON-207 BUTTON-101 
      WITH FRAME F_Deducibles IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Deducibles}
  DISPLAY E_Agregar 
      WITH FRAME F_Agregar IN WINDOW wWin.
  ENABLE E_Agregar BUTTON-153 
      WITH FRAME F_Agregar IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Agregar}
  IF AVAILABLE Hoja_Vida THEN 
    DISPLAY Hoja_Vida.Fec_Grabacion Hoja_Vida.Asunto_Cumplido 
          Hoja_Vida.Observacion 
      WITH FRAME F_HojaVida IN WINDOW wWin.
  ENABLE Hoja_Vida.Fec_Grabacion BUTTON-150 Hoja_Vida.Asunto_Cumplido 
         Hoja_Vida.Observacion Btn_SalvaHV Btn_NvoHv BUTTON-152 BUTTON-149 
      WITH FRAME F_HojaVida IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_HojaVida}
  ENABLE Br_ConHV Btn_OutConHV 
      WITH FRAME F_ConHV IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_ConHV}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Garantias_Imprimir wWin 
PROCEDURE Garantias_Imprimir :
/*codeudores a temporal de garantias*/
 DEFINE VARIABLE W_NomCode AS CHARACTER FORMAT "X(40)".
 DEFINE VARIABLE W AS INTEGER FORMAT "99" INITIAL 14. 
 FIND FIRST Relaciones WHERE
            Relaciones.Nit EQ Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud  AND
            Relaciones.Cuenta EQ Creditos.Num_Credito:SCREEN-VALUE IN FRAME F_Solicitud AND
            Relaciones.Clase_Producto EQ 2 AND
            Relaciones.Cod_Producto EQ INTEGER(SUBSTRING(Nom_Producto:SCREEN-VALUE IN FRAME F_Solicitud,1,3)) AND
            Relaciones.Cod_Relacion EQ 11 NO-LOCK NO-ERROR.
 IF AVAILABLE Relaciones THEN DO:
     /*GIOCAM Nov 26/07 - no se necesita en formato de impresión*/
/*     RUN TmpL (INPUT W, INPUT "").                             */
/*     CREATE TmpI.                                              */
/*     ASSIGN TmpI.ILinea  = W                                   */
/*            TmpI.ITexto = "GARANTIAS PERSONALES (Codeudores)". */
    FOR EACH Relaciones WHERE
             Relaciones.Nit EQ Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud  AND
             Relaciones.Cuenta EQ Creditos.Num_Credito:SCREEN-VALUE IN FRAME F_Solicitud AND
             Relaciones.Clase_Producto EQ 2 AND
             Relaciones.Cod_Producto EQ INTEGER(SUBSTRING(Nom_Producto:SCREEN-VALUE IN FRAME F_Solicitud,1,3)) AND
             Relaciones.Cod_Relacion EQ 11 NO-LOCK:
       FIND Clientes WHERE Clientes.Nit EQ Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
       IF AVAILABLE Clientes THEN DO:
          W_NomCode = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
          W = W + 1.
          CREATE TmpI.
          ASSIGN TmpI.ILinea = W
                 TmpI.ITexto = STRING(Clientes.Nit,"X(14)") + " - " + STRING(W_NomCode,"X(35)") + " Tel: " + STRING(Clientes.Tel_Residencia,"X(14)") 
               + " Dir: " + STRING(Clientes.DIR_Residencia,"X(30)").
       END.
    END.
 END.
 /*ADMISIBLES a temporal de garantias*/
 DEFINE VAR W_NomGar AS CHARACTER FORMAT "X(15)".
 FIND FIRST Garantias WHERE 
            Garantias.Agencia     EQ INTEGER(SUBSTRING(Cmb_Agencias:SCREEN-VALUE IN FRAME F_Solicitud,1,3)) AND
            Garantias.Tip_Credito EQ Creditos.Tip_Credito AND
            Garantias.Cod_Credito EQ Creditos.Cod_Credito AND
            Garantias.Num_Credito EQ INTEGER(Creditos.Num_Credito:SCREEN-VALUE IN FRAME F_Solicitud) AND
            Garantias.Estado      EQ 1 AND
            Garantias.Fec_Retiro  EQ ? NO-LOCK NO-ERROR.
  IF AVAILABLE Garantias THEN DO:
    W = W + 1.
    RUN TmpL (INPUT W, INPUT "").
    W = W + 1.
    CREATE TmpI.
    ASSIGN TmpI.ILinea  = W
           TmpI.ITexto = "GARANTIAS ADMISIBLES (Propiedades - Vehículos - Inversiones)".
     FOR EACH Garantias WHERE 
              Garantias.Agencia     EQ INTEGER(SUBSTRING(Cmb_Agencias:SCREEN-VALUE IN FRAME F_Solicitud,1,3)) AND
              Garantias.Tip_Credito EQ Creditos.Tip_Credito AND
              Garantias.Cod_Credito EQ Creditos.Cod_Credito AND
              Garantias.Num_Credito EQ INTEGER(Creditos.Num_Credito:SCREEN-VALUE IN FRAME F_Solicitud) AND
              Garantias.Estado      EQ 1 AND
              Garantias.Fec_Retiro  EQ ? NO-LOCK:
        CASE Garantias.Tipo_Garantia:
          WHEN 1 THEN W_NomGar = "Propiedad".
          WHEN 2 THEN W_NomGar = "Vehículo".
          WHEN 3 THEN W_NomGar = "Inversion".
        END CASE.
        W = W + 1.
        CREATE TmpI.
        ASSIGN TmpI.ILinea  = W
               TmpI.ITexto = STRING(W_NomGar,"X(10)") + " - " + STRING(Garantias.Nom_Bien,"X(35)") + "     Valor: $" + STRING(Garantias.Val_Bien,">>>,>>>,>>>,>>9").
     END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Gen_MovGMF wWin 
PROCEDURE Gen_MovGMF :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Grabar_AporteDif wWin 
PROCEDURE Grabar_AporteDif :
IF W_FaltApor > 0 THEN DO:
    RUN Abona_AporteDifer NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN ERROR.


    IF Creditos.agencia <> w_ageaportes THEN DO:
        CREATE TPartidas.
        Tpartidas.Tage = Creditos.agencia.
        Tpartidas.TCta = W_CtaSyA_aporte.
        TPartidas.TDsc = "Aporte-Dif. x Desemb SyA".
        TPartidas.TDoc = Creditos.Pagare:SCREEN-VALUE IN FRAME F_Ultima.
        TPartidas.TTip = 2.
        TPartidas.TOpe = 0.
        TPartidas.TCre = W_FaltApor.
        TCed = STRING(w_ageaportes,"999").

        CREATE TPartidas.
        Tpartidas.Tage = w_ageaportes.
        Tpartidas.TCta = W_CtaSyA_aporte.
        TPartidas.TDsc = "Aporte-Dif. x Desemb SyA".
        TPartidas.TDoc = Creditos.Pagare:SCREEN-VALUE IN FRAME F_Ultima.
        TPartidas.TTip = 2.
        TPartidas.TOpe = 0.
        TPartidas.TDeb = W_FaltApor.
        TCed = STRING(Creditos.agencia,"999").
    END.

    CREATE TPartidas.
    Tpartidas.Tage = w_ageaportes.
    Tpartidas.TCta = CortoLargo.Cta_AsoAd.
    TPartidas.TDsc = "Aporte-Dif. x Desemb SyA".
    TPartidas.TDoc = Creditos.Pagare:SCREEN-VALUE IN FRAME F_Ultima.
    TPartidas.TTip = 2.
    TPartidas.TOpe = 0.
    TPartidas.TCre = W_FaltApor.
    TCed = Creditos.nit.
END.
ELSE
    W_FaltApor = 0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Grabar_Credito wWin 
PROCEDURE Grabar_Credito :
DEFINE VAR W_CtaTemp AS CHARACTER.
DEFINE VAR W_CtaTCheq AS CHARACTER.
DEFINE VAR TTotal_Debito AS DECIMAL.
DEFINE VAR TTotal_Credito AS DECIMAL.

SESSION:SET-WAIT-STATE("General").

EMPTY TEMP-TABLE TPartidas NO-ERROR.

W_Des = YES.
W_DiasAnt = 0.
W_IntAntic = 0.

IF NOT AVAILABLE Creditos THEN DO:
    MESSAGE "No se encuentra el crédito a desembolsar" SKIP
            "rectifique o comuniquese con el administrador"
        VIEW-AS ALERT-BOX ERROR.

    RETURN ERROR.
END.

Desembolso:
DO TRANSACTION ON ERROR UNDO Desembolso:
    W_TipoInforme = "Desembolso".
    Creditos.Cuota = ROUND(Creditos.Cuota,0).
    W_CancCap = 0.
    W_VrImpto = 0.

    RUN Halla_Cpte NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        EMPTY TEMP-TABLE Tmp-tarjetadb.
        RETURN ERROR.
    END.

    IF W_VrCredAcanc > 0 THEN DO:
        RUN Canc_Creditos NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            RETURN ERROR.
    END.

    RUN Calcular_Deducible (INPUT Creditos.Monto) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN ERROR.

    RUN Grabar_aporteDif.
    RUN CortoLargoCreditos NO-ERROR. /* oakley */

    IF ERROR-STATUS:ERROR THEN DO:
        EMPTY TEMP-TABLE Tmp-tarjetadb NO-ERROR.
        RETURN ERROR.
    END.

    RUN deducc_GMF.

    IF Creditos.Desembolso EQ 1 OR Creditos.Desembolso EQ 5 OR Creditos.Desembolso EQ 6 THEN DO: /*desembolso tiene efectivo*/
        FIND FIRST Cuentas WHERE Cuentas.Cod_FlujoEfe EQ "D"
                             AND Cuentas.Car_Efectivo EQ 2
                             AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Cuentas OR AVAIL(Cuentas) AND Cuentas.Cuenta LE "0" THEN DO:
            FOR EACH Tmp-tarjetadb:
                DELETE Tmp-tarjetadb.
            END.

            MESSAGE "No se encuentra la cuenta de caja para" SKIP
                    "el desembolso en efectivo del crédito." SKIP(1)
                    "Se cancela la operación de desembolso!" SKIP
                    "Comuniquele al Administrador"
                VIEW-AS ALERT-BOX ERROR.
            RETURN ERROR.
        END.

        W_CtaTemp = Cuentas.Cuenta.
    END.

    IF Creditos.Desembolso EQ 2 OR Creditos.Desembolso EQ 5 OR Creditos.Desembolso EQ 7 THEN DO:  /*desembolso tiene cheque*/
        IF W_CtaBanco EQ "" OR W_CtaBanco EQ "?" OR Num_Cheque EQ "" OR Nom_Beneficiario EQ "" THEN DO:
            EMPTY TEMP-TABLE Tmp-tarjetadb NO-ERROR.

            MESSAGE "El banco no ha sido escogido o no se ha" SKIP
                    "digitado el nombre del beneficiario o el" SKIP
                    "número de cheque" SKIP(1)
                    "Se cancela la operación de desembolso!"
                VIEW-AS ALERT-BOX ERROR.
            RETURN ERROR.
        END.

        ASSIGN W_CtaTCheq = W_CtaBanco.
    END.

    IF Creditos.Desembolso = 9 THEN DO:
        IF W_CtaBanco EQ "" OR W_CtaBanco EQ "?" THEN DO:
            EMPTY TEMP-TABLE Tmp-tarjetadb NO-ERROR.

            MESSAGE "El banco no ha sido escogido." SKIP
                    "Se cancela la operación de desembolso...!"
                VIEW-AS ALERT-BOX ERROR.

            RETURN ERROR.
        END.

        ASSIGN W_CtaTCheq = W_CtaBanco.
    END.


    IF Creditos.Desembolso EQ 3 OR Creditos.Desembolso EQ 6 OR Creditos.Desembolso EQ 7 THEN DO: /*desembolso tiene a cuenta de ahorros*/
        RUN Verificar_Cuenta_Desembolso NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            EMPTY TEMP-TABLE Tmp-tarjetadb NO-ERROR.

            MESSAGE "No se encontró la cuenta de ahorros para el desembolso." SKIP
                    "se cancela la operación de desembolso del crédito." SKIP(1)
                    "Consulte con el administrador del sistema!"
                VIEW-AS ALERT-BOX ERROR.
            RETURN ERROR.
        END.

        RUN CortoLargoAhorros NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            EMPTY TEMP-TABLE Tmp-tarjetadb NO-ERROR.
            RETURN ERROR.
        END.
    END.
    ELSE
        IF Creditos.Desembolso EQ 4 THEN      /*A Cta-Terceros*/
            W_CtaTemp = W_CtaTerceros.

    IF Creditos.Desembolso NE 3 THEN DO:   /*Solo si es el total a la cta-ahorros ya esta el asiento*/
        IF Creditos.Desembolso EQ 4 THEN DO:
            CREATE TPartidas.
            ASSIGN Tpartidas.Tage = Creditos.Agencia
                   TPartidas.TDoc = Creditos.Pagare:SCREEN-VALUE IN FRAME F_Ultima
                   TPartidas.TTip = 2
                   TPartidas.TOpe = 0
                   TPartidas.TCre = W_VrADesemb - W_IntAntic - W_VrImpto  /* - W_FaltApor */
                   Tpartidas.TCta = W_CtaTemp
                   Tpartidas.TCed = W_NitTercero
                   TPartidas.TDsc = "Desemb.Ord.Terceros".
        END.
        ELSE DO:
            IF Creditos.Desembolso EQ 1 OR Creditos.Desembolso EQ 5 OR Creditos.Desembolso EQ 6 THEN DO:
                CREATE TPartidas.
                ASSIGN Tpartidas.Tage = W_Agencia
                       TPartidas.TDoc = Creditos.Pagare:SCREEN-VALUE IN FRAME F_Ultima 
                       TPartidas.TTip = 2
                       TPartidas.TOpe = 0
                       TPartidas.TCre = W_VrEfectivo - W_VrImpto - W_IntAntic   /* nh*/
                       Tpartidas.TCta = W_CtaTemp
                       TPartidas.TDsc = "Desemb.en efectivo".

                IF Creditos.Agencia NE W_Agencia THEN
                    RUN PartidaSya (INPUT W_VrEfectivo).

                CREATE TPartidas.
                ASSIGN Tpartidas.Tage = W_Agencia
                       TPartidas.TDoc = Creditos.Pagare:SCREEN-VALUE IN FRAME F_Ultima
                       TPartidas.TTip = 2
                       TPartidas.TOpe = 0
                       TPartidas.TCre = W_VrCheque
                       Tpartidas.TCta = W_CtaTCheq
                       TPartidas.TDsc = "Desemb.en Cheque".

                IF Creditos.Agencia NE W_Agencia THEN
                    RUN PartidaSya (INPUT W_VrCheque).
            END.
            ELSE
                IF Creditos.Desembolso EQ 2 OR Creditos.Desembolso EQ 5 OR Creditos.Desembolso EQ 7 OR creditos.desembolso = 9 THEN DO:
                    CREATE TPartidas.
                    ASSIGN Tpartidas.Tage = W_Agencia
                           TPartidas.TDoc = Creditos.Pagare:SCREEN-VALUE IN FRAME F_Ultima
                           TPartidas.TTip = 2
                           TPartidas.TOpe = 0
                           TPartidas.TCre = W_VrADesemb - W_IntAntic - W_VrImpto /* - W_FaltApor  */
                           Tpartidas.TCta = W_CtaTCheq
                           TPartidas.TDsc = "Desemb.en Cheque"
                           W_VrCheque = W_VrADesemb - W_IntAntic.

                    IF creditos.desembolso = 9 THEN
                        TPartidas.TDsc = "Desembolso por Transferencia".

                    IF Creditos.Agencia NE W_Agencia THEN
                        RUN PartidaSya (INPUT W_VrCheque).
                END.
        END.
    END.

    CREATE TPartidas.
    ASSIGN Tpartidas.Tage = Creditos.Agencia
           Tpartidas.TCta = W_CtaCorCre
           TPartidas.TDsc = "Desembolso del Credito Número: " + STRING(Creditos.Num_Credito)
           TPartidas.TDoc = Creditos.Pagare:SCREEN-VALUE IN FRAME F_Ultima
           TPartidas.TTip = 2
           TPartidas.TOpe = 0
           TPartidas.TDeb = Creditos.Monto.

    IF W_IntAntic GT 0 THEN DO:
        IF creditos.tip_credito LE 4 THEN DO:
            FIND FIRST Liqui_Int WHERE Liqui_Int.Clase_Producto EQ 2
                                   AND Liqui_Int.Cod_Producto EQ Creditos.Cod_Credito NO-LOCK NO-ERROR. 
            IF NOT AVAIL(Liqui_Int) OR (AVAIL(Liqui_Int) AND Liqui_Int.CtaInt_AntAso LE "0") THEN DO:
                EMPTY TEMP-TABLE Tmp-tarjetadb NO-ERROR.

                MESSAGE "Falta configurar en Liqui_Int pdcto: " Creditos.Cod_Credito SKIP
                        "Con cuenta Liqui_Int.CtaInt_AntAso, activa y de movto en Puc (Int-Anticipados)" SKIP
                        "No se permite la operaciòn..."
                    VIEW-AS ALERT-BOX ERROR.
                RETURN ERROR.
            END.
        END.

        CREATE TPartidas.
        ASSIGN Tpartidas.Tage = Creditos.Agencia
               Tpartidas.TCta = Liqui_Int.CtaInt_AntAso
               TPartidas.TDsc = "Int-Anticip.en el Desemb."
               TPartidas.TDoc = Creditos.Pagare:SCREEN-VALUE IN FRAME F_Ultima
               TPartidas.TTip = 2
               TPartidas.TOpe = 0
               TPartidas.TCre = W_IntAntic.
    END.

    FOR EACH TPartidas:
        IF TPartidas.TDeb GT 0 AND TPartidas.TCre EQ 0 THEN
            TTotal_Debito = TTotal_Debito + TPartidas.TDeb.
        ELSE
            TTotal_Credito = TTotal_Credito + TPartidas.TCre.
    END.

    IF ROUND(TTotal_Credito + W_VrCredAcanc,0) NE ROUND(TTotal_Debito,0) THEN DO:
        MESSAGE "El total de débitos contra Créditos"   SKIP
                "es diferente. Se cancela la operación" SKIP
                "Creditos   : " TTotal_Credito SKIP
                "Cred A Canc: " W_VrCredAcanc  SKIP
                "Total Cred    : " TTotal_Credito + W_VrCredAcanc  SKIP
                "Total Debitos : " TTotal_Debito                   SKIP
                "Diferencia    : " TTotal_Credito + W_VrCredAcanc - TTotal_Debito
            VIEW-AS ALERT-BOX TITLE "INFORMATIVO".
        RETURN ERROR.
    END.

    ASSIGN Creditos.Cta_Contable = W_CtaCorCre
           Creditos.Estado = 2
           Creditos.Fec_Desembolso = W_Fecha
           Creditos.Sdo_Capital = Creditos.Monto
           Creditos.Monto = Creditos.Sdo_Capital
           Creditos.Val_Desembolso = Creditos.Sdo_Capital
           Creditos.Pagare = STRING(Creditos.Num_Credito)
           Creditos.Sdo_Proyectado = Creditos.Sdo_Capital.

    /* Revisamos que el crédito tenga creado Control_Pagos */
    FIND FIRST CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                               AND CONTROL_pagos.num_credito = creditos.num_credito NO-LOCK NO-ERROR.
    IF NOT AVAILABLE CONTROL_pagos THEN DO:
        RUN CrearControlPagos.r(INPUT creditos.nit,
                                INPUT creditos.num_credito) NO-ERROR.
    END.

    FIND FIRST CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                               AND CONTROL_pagos.cod_credito = creditos.cod_credito
                               AND CONTROL_pagos.num_credito = creditos.num_credito
                               AND CONTROL_pagos.id_pdoMes = 1 NO-ERROR.

    Creditos.Fec_Pago = control_pagos.Fec_Vcto.

    RUN Mov_AhorroyCredito NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        EMPTY TEMP-TABLE Tmp-tarjetadb NO-ERROR.
        RETURN ERROR.
    END.

    ASSIGN Mov_Creditos.Sdo_Capital = Creditos.Sdo_Capital
           W_DesAho = "".

    MESSAGE creditos.desembolso W_VrAlAhorro W_VrImpto
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    IF AVAILABLE Ahorros AND (Creditos.Desembolso EQ 3 OR Creditos.Desembolso EQ 6 OR Creditos.Desembolso EQ 7) THEN DO:
        ASSIGN Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible + W_VrAlAhorro - W_VrImpto.
               Ahorros.Fec_Ulttransaccion = W_Fecha.

        IF Ahorros.Detalle_Estado EQ 1 THEN         /*APERTURA DE LA CUENTA:Primera Consignacion*/
            ASSIGN Ahorros.Detalle_Estado = 2
                   Ahorros.Sdo_Inicial = W_VrAlAhorro.

        IF AVAILABLE mov_ahorros THEN
            ASSIGN Mov_Ahorros.Sdo_Dispon = Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje
                   Mov_Ahorros.Descrip = "Desemb.de Credito No." + STRING(Creditos.Num_Credito).

        IF ahorros.tarjetaDB NE " " AND W_VrAlAhorro GT 0 THEN     /* jjmp  1 de septiembre de 2006 */
            RUN grabar_TmpTarDeb(W_VrAlAhorro,
                                 "Proc_Desembolso: Credito a Cuenta de Ahorros",
                                 3).
    END.

    /*Las cuentas de ahorro(Aportes) como bloqueadas por credito*/
    FOR EACH Ahorros WHERE Ahorros.Tip_Ahorro EQ 4
                       AND Ahorros.Nit EQ Creditos.Nit
                       AND Ahorros.Estado EQ 1:
        ASSIGN Ahorros.Detalle_Estado = 08.
    END.

    RUN Crea_HojaV.

    FIND FIRST Mov_Instancias WHERE Mov_Instancias.Instancia EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5))
                                AND Mov_Instancias.Nit EQ Creditos.Nit
                                AND Mov_Instancias.Cuenta EQ TRIM(STRING(Creditos.Num_Credito))
                                AND Mov_Instancias.Num_Solicitud EQ Creditos.Num_Solicitud
                                AND Mov_Instancias.Estado EQ NO NO-ERROR.
    IF AVAILABLE Mov_Instancias THEN
        ASSIGN Mov_Instancias.Estado = YES
               Mov_Instancias.Fec_Retiro = TODAY
               Mov_Instancias.Hora_Retiro = TIME.
    ELSE DO:
        EMPTY TEMP-TABLE Tmp-tarjetadb NO-ERROR.

        MESSAGE "No se encontro Mov_Instancias" SKIP
            VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
    END.

    FIND CURRENT Mov_Instancias NO-LOCK NO-ERROR.

    FOR EACH Garantias WHERE Garantias.Agencia = Creditos.Agencia
                         AND Garantias.Cod_Credito = Creditos.Cod_Credito
                         AND Garantias.Tip_Credito = Creditos.Tip_Credito
                         AND Garantias.Num_Credito = Creditos.Num_Credito
                         AND Garantias.Num_Solicitud = Creditos.Num_Solicitud
                         AND Garantias.Estado = 1
                         AND Garantias.Nit = Creditos.Nit:
        IF Garantias.Aprobada THEN DO:
            ASSIGN Garantias.Pagare = STRING(Creditos.Num_Credito).

            IF Garantias.Tipo_Garantia EQ 4 THEN DO:
                FIND FIRST Ahorros WHERE (Ahorros.Tip_Ahorro EQ 2 OR Ahorros.Tip_Ahorro EQ 3)
                                     AND Ahorros.Nit EQ Garantias.Nit_Aseguradora
                                     AND Ahorros.Cue_Ahorros EQ Garantias.Identificacion_Bien
                                     AND Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje GT 0 NO-ERROR.
                IF AVAIL(Ahorros) THEN
                    ASSIGN Ahorros.Detalle_Estado = 8.
            END.
        END.
        ELSE
            ASSIGN Garantias.Estado = 2.
    END.

    RUN Contabilizar_Partidas NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        EMPTY TEMP-TABLE Tmp-tarjetadb  NO-ERROR.
        RETURN ERROR.
    END.
    ELSE DO:
        RUN Grabar_tardebNew.
        APPLY "ENTRY" TO Cmb_Instancias IN FRAME F_Creditos.
    END.

    FIND CURRENT Creditos NO-LOCK NO-ERROR.
    FIND CURRENT Ahorros NO-LOCK NO-ERROR.
    FIND CURRENT Hoja_Vida NO-LOCK NO-ERROR.

    SESSION:SET-WAIT-STATE("").

    ASSIGN FRAME F_Creditos:SENSITIVE = FALSE
           FRAME F_ImpCpte:VISIBLE = TRUE.
END. /*Fin transaccion*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Grabar_MovAhorros wWin 
PROCEDURE Grabar_MovAhorros :
/*------------------------------------------------------------------------------
  Observaciones : Permite Gravar el Detalle de la Operación en Movimientos.       
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
  DEFINE INPUT PARAMETER M_Nit     LIKE Clientes.Nit.
  
  CREATE Mov_Ahorros.
  ASSIGN Mov_Ahorros.Cod_Operacion = M_CodOper
         Mov_ahorros.Cod_Ahorro    = M_CodPto
         Mov_Ahorros.Cue_Ahorros   = M_Cuenta
         Mov_ahorros.Nit           = M_Nit
         Mov_Ahorros.Fecha         = W_Fecha
         Mov_Ahorros.Hora          = TIME
         Mov_Ahorros.Num_Documento = M_Dto
         Mov_Ahorros.Agencia       = M_Agencia
         Mov_Ahorros.Age_Fuente    = M_OfiFte
         Mov_Ahorros.Age_Destino   = M_OfiDest
         Mov_Ahorros.Usuario       = M_Usuario
         Mov_Ahorros.Val_Cheque    = 0
         Mov_Ahorros.Val_Efectivo  = M_VlrEfe
         Mov_Ahorros.Descrip       = W_DesAho
         Mov_Ahorros.Cpte          = W_Cbte
         Mov_Ahorros.Sdo_Dispon    = Ahorros.Sdo_Dispon NO-ERROR.
  IF ERROR-STATUS:ERROR THEN 
     RETURN ERROR.

 /* IF Creditos.Id_Adicionales EQ 2 AND Creditos.Desembolso EQ 3 THEN DO:
     Mov_Ahorros.Val_Efectivo  = M_VlrEfe - Tot_Deduc.
     /*IF W_FaltApor GT 0 THEN
        Mov_Ahorros.Val_Efectivo  = Mov_Ahorros.Val_Efectivo - W_FaltApor. comentariado Agosto 2/05 GAER,
        ya estan restados al invocar el procedimiento*/
  END.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Grabar_MovCreditos wWin 
PROCEDURE Grabar_MovCreditos :
/*------------------------------------------------------------------------------
  Observaciones : Permite Gravar el Detalle de la Operación en Movimientos.       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER C_CodOper LIKE Mov_Creditos.Cod_Operacion.
  DEFINE INPUT PARAMETER C_CodPto  LIKE Mov_creditos.cod_credito.
  DEFINE INPUT PARAMETER C_NumCredito  LIKE Mov_Creditos.Num_Credito.
  DEFINE INPUT PARAMETER C_Dto     LIKE Mov_Creditos.Num_Documento.
  DEFINE INPUT PARAMETER C_Agencia LIKE Mov_Creditos.Agencia.
  DEFINE INPUT PARAMETER C_OfiFte  LIKE Mov_Creditos.Ofi_Fuente.
  DEFINE INPUT PARAMETER C_OfiDest LIKE Mov_Creditos.Ofi_Destino.
  DEFINE INPUT PARAMETER C_Usuario LIKE Mov_Creditos.Usuario.
  DEFINE INPUT PARAMETER C_VlrChe  LIKE Mov_Creditos.Val_Cheque.
  DEFINE INPUT PARAMETER C_VlrEfe  LIKE Mov_Creditos.Val_Efectivo.
  DEFINE INPUT PARAMETER C_Pagare  LIKE Mov_Creditos.Pagare.
  
  CREATE Mov_Creditos.
  ASSIGN Mov_Creditos.Cod_Operacion  = C_CodOper
         Mov_creditos.Cod_credito    = C_CodPto
         Mov_Creditos.Nit            = Creditos.nit
         Mov_Creditos.Fecha          = W_Fecha
         Mov_Creditos.Hora           = vTime
         Mov_Creditos.Num_Documento  = C_Dto
         Mov_Creditos.Agencia        = C_Agencia
         Mov_Creditos.Ofi_Fuente     = C_OfiFte
         Mov_Creditos.Ofi_Destino    = C_OfiDest
         Mov_Creditos.Num_Credito    = C_NumCredito
         Mov_Creditos.Usuario        = C_Usuario
         Mov_Creditos.Val_Cheque     = C_VlrChe
         Mov_Creditos.Val_Efectivo   = C_VlrEfe
         Mov_Creditos.Descrip        = "Desembolso Inicial"
         Mov_Creditos.Pagare         = C_Pagare
         Mov_Creditos.Cpte           = W_Cbte
         Mov_Creditos.Sdo_Capital    = C_VlrEfe.
          
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Grabar_TarDeb wWin 
PROCEDURE Grabar_TarDeb :
/*------------------------------------------------------------------------------
  Purpose:   Graba movimientos de Tarjeta debito  
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER wvlrTrans  LIKE ahorros.sdo_disponible.
DEFINE INPUT PARAMETER wdesTrans  LIKE Mov_contable.comentario.
DEFINE INPUT PARAMETER wtipoTrans AS INTEGER.
DEFINE VAR wvlrmonTD     AS CHARACTER INITIAL '0000000000000'.
DEFINE VAR wsecTD        AS CHARACTER INITIAL '0000000000000'.
DEFINE VAR wsdoTD        AS CHARACTER INITIAL '0000000000000'.
DEFINE VAR wdisTD        AS CHARACTER INITIAL '0000000000000'.
DEFINE VAR wretcode      AS INTEGER INITIAL -1.

IF wtipotrans NE 0 THEN DO:
   FIND CURRENT ahorros  NO-ERROR.
   CREATE tarjetadebito.
   ASSIGN tarjetadebito.Agencia         = w_agencia       
          tarjetadebito.Usuario         = w_usuario       
          tarjetadebito.Comprobante     = w_cbte          
          tarjetadebito.Num_Documento   = INTEGER(W_NumCbt)
          tarjetadebito.Fec_Contable    = w_fecha           
          tarjetadebito.Hora            = TIME            
          tarjetadebito.Comentario      = wdesTrans
          tarjetadebito.Aplicado        = NO
          tarjetadebito.ManBiometrico   = 1
          tarjetadebito.TipoTransaccion = wtipotrans
          tarjetadebito.Nit             = Ahorros.nit
          tarjetadebito.Cue_Ahorros     = Ahorros.cue_ahorros  /* SUBSTRING(Ahorros.TarjetaDB,10,7) */
          tarjetadebito.TarjetaDB       = Ahorros.TarjetaDB
          tarjetadebito.Monto           = wvlrTrans
          tarjetadebito.Secuencia       = "000000000000"
          tarjetadebito.SdoTotal        = 0
          tarjetadebito.SdoDispon       = 0
          tarjetadebito.RetCode         = -1.
   wvlrmonTD = STRING(wvlrtrans,'9999999999999').
   RUN TranWebCaja(1,wtipotrans,TRIM(ahorros.nit), TRIM(ahorros.Cue_ahorros), ahorros.TarjetaDB, wvlrmonTD, INPUT-OUTPUT wsecTD, INPUT-OUTPUT wsdoTD, INPUT-OUTPUT wdisTD, OUTPUT wretcode).
   IF wretcode = 0 THEN 
      ASSIGN tarjetadebito.Secuencia       = wsecTD 
             tarjetadebito.SdoTotal        = ROUND(decimal(wsdoTD) / 100,2)
             tarjetadebito.SdoDispon       = ROUND(decimal(wdisTD) / 100,2)
             tarjetadebito.RetCode         = wretcode
             tarjetadebito.Aplicado        = YES.
   ELSE 
      ASSIGN tarjetadebito.RetCode         = wretcode.
   RELEASE tarjetadebito.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Grabar_TarDebNew wWin 
PROCEDURE Grabar_TarDebNew :
/*------------------------------------------------------------------------------
  Purpose:   Graba movimientos de Tarjeta debito  
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR wvlrmonTD     AS CHARACTER INITIAL '0000000000000'.
  DEFINE VAR wsecTD        AS CHARACTER INITIAL '0000000000000'.
  DEFINE VAR wsdoTD        AS CHARACTER INITIAL '0000000000000'.
  DEFINE VAR wdisTD        AS CHARACTER INITIAL '0000000000000'.
  DEFINE VAR wretcode      AS INTEGER INITIAL -1.

  FOR EACH tmp-tarjetadb:
     CREATE tarjetadebito.
     BUFFER-COPY tmp-tarjetadb TO tarjetadebito.
     ASSIGN wvlrmonTD = STRING(tarjetadebito.Monto,'9999999999999').
     RUN TranWebCaja(1,tarjetadebito.TipoTransaccion,TRIM(tarjetadebito.Nit), TRIM(tarjetadebito.Cue_Ahorros), tarjetadebito.TarjetaDB, wvlrmonTD, INPUT-OUTPUT wsecTD, INPUT-OUTPUT wsdoTD, INPUT-OUTPUT wdisTD, OUTPUT wretcode).
     IF wretcode = 0 THEN 
        ASSIGN tarjetadebito.Secuencia       = wsecTD 
               tarjetadebito.SdoTotal        = ROUND(decimal(wsdoTD) / 100,2)
               tarjetadebito.SdoDispon       = ROUND(decimal(wdisTD) / 100,2)
               tarjetadebito.RetCode         = wretcode
               tarjetadebito.Aplicado        = YES.
     ELSE 
        ASSIGN tarjetadebito.RetCode         = wretcode.
     RELEASE tarjetadebito.
  END.
  /* Barrido de la tabla tarjetadb */
  FOR EACH tmp-tarjetadb:
      DELETE tmp-tarjetadb.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Grabar_TmpTarDeb wWin 
PROCEDURE Grabar_TmpTarDeb :
/*------------------------------------------------------------------------------
  Purpose:   Graba movimientos de Tarjeta debito  
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER wvlrTrans  LIKE ahorros.sdo_disponible.
DEFINE INPUT PARAMETER wdesTrans  LIKE Mov_contable.comentario.
DEFINE INPUT PARAMETER wtipoTrans AS INTEGER.
DEFINE VAR wvlrmonTD     AS CHARACTER INITIAL '0000000000000'.
DEFINE VAR wsecTD        AS CHARACTER INITIAL '0000000000000'.
DEFINE VAR wsdoTD        AS CHARACTER INITIAL '0000000000000'.
DEFINE VAR wdisTD        AS CHARACTER INITIAL '0000000000000'.
DEFINE VAR wretcode      AS INTEGER INITIAL -1.

IF wtipotrans NE 0 THEN DO:
   FIND CURRENT ahorros  NO-ERROR.
   CREATE Tmp-tarjetadb.
   ASSIGN Tmp-tarjetadb.Agencia         = w_agencia                                                                                 
          Tmp-tarjetadb.Usuario         = w_usuario                                                                                 
          Tmp-tarjetadb.Comprobante     = w_cbte                                                                                    
          Tmp-tarjetadb.Num_Documento   = INTEGER(W_NumCbt)                                                                         
          Tmp-tarjetadb.Fec_Contable    = w_fecha                                                                                   
          Tmp-tarjetadb.Hora            = TIME                                                                                      
          Tmp-tarjetadb.Comentario      = wdesTrans                                                                                 
          Tmp-tarjetadb.Aplicado        = NO                                                                                        
          Tmp-tarjetadb.ManBiometrico   = 1                                                                                         
          Tmp-tarjetadb.TipoTransaccion = wtipotrans                                                                                
          Tmp-tarjetadb.Nit             = Ahorros.nit                                                                               
          Tmp-tarjetadb.Cue_Ahorros     = Ahorros.cue_ahorros  /* SUBSTRING(Ahorros.TarjetaDB,10,7) */                              
          Tmp-tarjetadb.TarjetaDB       = Ahorros.TarjetaDB                                                                         
          Tmp-tarjetadb.Monto           = wvlrTrans                                                                                 
          Tmp-tarjetadb.Secuencia       = "000000000000"                                                                            
          Tmp-tarjetadb.SdoTotal        = 0                                                                                         
          Tmp-tarjetadb.SdoDispon       = 0                                                                                         
          Tmp-tarjetadb.RetCode         = -1.
   RELEASE Tmp-Tarjetadb.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Hallar_Rangos_IndicadorOk wWin 
PROCEDURE Hallar_Rangos_IndicadorOk :
DEF VAR wtotaportes AS DEC INIT 0.00.
  DO WITH FRAME F_Solicitud:
  /* ASSIGN W_PunTos:SENSITIVE = FALSE W_PNegocia = 0.*/
     IF Creditos.cod_credito NE 574 THEN 
        FIND FIRST Ran_Intereses WHERE  Ran_Intereses.Indicador          EQ Indicador.Indicador 
                                   AND (DEC(Creditos.Monto:SCREEN-VALUE) GE Ran_Intereses.Val_Inicial
                                   AND  DEC(Creditos.Monto:SCREEN-VALUE) LE Ran_Intereses.Val_Final)
                                   AND (Dias                             GE Ran_Intereses.Pla_Inicial  
                                   AND  Dias                             LE Ran_Intereses.Pla_Final) 
                                   AND  Ran_Interes.Estado               EQ 1 NO-LOCK NO-ERROR.
     ELSE DO:
        FOR EACH ahorros NO-LOCK WHERE Ahorros.nit = solicitud.nit AND Ahorros.Tip_ahorro = 4 AND Ahorros.estado = 1:
            wtotaportes = wtotaportes + ahorros.sdo_disponible.
        END.
        IF wtotaportes LE 0 THEN DO:
           MESSAGE "Esta persona no tiene Aportes,  por tanto no puede tomar esta linea"
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
           RETURN ERROR.
         /*APPLY "Mouse-Select-Click" TO Solicitud.Tip_Credito IN FRAME F_Producto.
           RETURN NO-APPLY.*/
        END.
        FIND FIRST Ran_Intereses WHERE  Ran_Intereses.Indicador EQ Indicador.Indicador 
                                   AND (wtotaportes             GE Ran_Intereses.Val_Inicial
                                   AND  wtotaportes             LE Ran_Intereses.Val_Final)
                                   AND  Ran_Interes.Estado      EQ 1 NO-LOCK NO-ERROR.
     END.
     IF AVAIL(Ran_Intereses) THEN DO:
        IF Clientes.Tipo_Vinculo EQ 1
           THEN TasaOk  = Indicadores.Tasa + Ran_Intereses.Puntos_Asoc.
           ELSE TasaOk  = Indicadores.Tasa + Ran_Intereses.Puntos.
        TasaOK = TasaOK - W_Tasa.
     END.
     ELSE TasaOk = Indicadores.Tasa - W_Tasa.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Hallar_Tasa_Efectiva wWin 
PROCEDURE Hallar_Tasa_Efectiva :
DEFINE VAR Periodo AS INTEGER FORMAT "999".
DEFINE VAR Tas_Efectiva AS DECIMAL.

DO WITH FRAME F_Solicitud:
    CASE INTEGER(SUBSTRING(Cmb_PerPago:SCREEN-VALUE,1,1)):
        WHEN 0 THEN Periodo = 360.
        WHEN 1 THEN Periodo = 52.
        WHEN 2 THEN Periodo = 36.
        WHEN 3 THEN Periodo = 24.
        WHEN 4 THEN Periodo = 12.
        WHEN 5 THEN Periodo = 6.
        WHEN 6 THEN Periodo = 4.
        WHEN 7 THEN Periodo = 3.
        WHEN 8 THEN Periodo = 2.
        WHEN 9 THEN Periodo = 1.
    END CASE.

    IF Creditos.FOR_Interes:SCREEN-VALUE = "1" THEN
        Tas_Efectiva = EXP ( 1 + ((decimal(Creditos.Tasa:SCREEN-VALUE) / 100) / PERIODO),PERIODO)  - 1.
    ELSE
        Tas_Efectiva = (1 / EXP(1 - ((decimal(Creditos.Tasa:SCREEN-VALUE) / 100) / PERIODO),Periodo)) - 1.

    Tas_Efectiva = Tas_Efectiva * 100.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Hallar_Tasa_Nominal wWin 
PROCEDURE Hallar_Tasa_Nominal :
DEFINE VAR Periodo AS INTEGER.
DEFINE VAR Tas_Nominal AS DECIMAL.

DO WITH FRAME F_Solicitud:
    CASE INTEGER(SUBSTRING(Cmb_PerPago:SCREEN-VALUE,1,1)):
        WHEN 0 THEN Periodo = 360.
        WHEN 1 THEN Periodo = 52.
        WHEN 2 THEN Periodo = 36.
        WHEN 3 THEN Periodo = 24.
        WHEN 4 THEN Periodo = 12.
        WHEN 5 THEN Periodo = 6.
        WHEN 6 THEN Periodo = 4.
        WHEN 7 THEN Periodo = 3.
        WHEN 8 THEN Periodo = 2.
        WHEN 9 THEN Periodo = 1.
    END CASE.

    IF Creditos.FOR_Interes:SCREEN-VALUE = "1" THEN
        RUN EFNV IN W_ManFin (INPUT (DECIMAL(Creditos.Tasa:SCREEN-VALUE) / 100),
                              INPUT Periodo,
                              OUTPUT Tas_Nominal).
    ELSE
        RUN EFNA IN W_ManFin (INPUT (DECIMAL(Creditos.Tasa:SCREEN-VALUE) / 100),
                              INPUT Periodo,
                              OUTPUT Tas_Nominal).

    ASSIGN Tas_Nominal = ((Tas_Nominal * Periodo) * 100)
           /*W_TasaNominal:SCREEN-VALUE = STRING(Tas_Nominal)*/
           w_tasaNominal:SCREEN-VALUE = string(creditos.tasa).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Hallar_Tasa_Periodo wWin 
PROCEDURE Hallar_Tasa_Periodo :
DEFINE VAR Tas_Periodo AS DECIMAL.

DO WITH FRAME F_Solicitud:
    RUN HallarTasPer IN W_ManFin (INPUT INTEGER(SUBSTRING(Cmb_PerPago:SCREEN-VALUE,1,1)),
                                  INPUT DECIMAL(Creditos.Tasa:SCREEN-VALUE),
                                  INPUT INTEGER(Creditos.FOR_Interes:SCREEN-VALUE),
                                  OUTPUT Tas_Periodo).

    /* Para los créditos Corto Plazo y Emergencia el periodo de liquidación es diario */
    IF INTEGER(SUBSTRING(nom_producto:SCREEN-VALUE IN FRAME F_Solicitud,1,3)) = 108 OR
       INTEGER(SUBSTRING(nom_producto:SCREEN-VALUE IN FRAME F_Solicitud,1,3)) = 113 OR
       INTEGER(SUBSTRING(nom_producto:SCREEN-VALUE IN FRAME F_Solicitud,1,3)) = 114 THEN DO:
        RUN HallarTasPer IN W_ManFin (INPUT 0, /* Periodo de liquidación diaria */
                                      INPUT DEC(creditos.Tasa:SCREEN-VALUE),
                                      INPUT INT(creditos.FOR_Interes:SCREEN-VALUE),
                                      OUTPUT Tas_Periodo).

        /*w_tasaNominal = tas_periodo * 36000.
        w_tasaNominal:SCREEN-VALUE = STRING(w_tasaNominal,">>9.999999").*/
        w_tasaNominal:SCREEN-VALUE = string(creditos.tasa).
    END.

    ASSIGN Tas_Periodo = (Tas_Periodo * 100)
           W_TasaPeriodo:SCREEN-VALUE = STRING(Tas_Periodo).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Halla_Cpte wWin 
PROCEDURE Halla_Cpte :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VAR  Tipo      LIKE Operacion.Ctrl_EfeChe INIT 1.
    
    IF Creditos.Desembolso EQ 2 OR creditos.desembolso = 9
    OR Creditos.Desembolso EQ 5
    OR Creditos.Desembolso EQ 7 THEN 
       Tipo = 2.

    FIND FIRST Operacion WHERE /*busca operacion de retiro de credito*/
        STRING(Operacion.Cod_Operacion,"999999999") BEGINS "020102" AND
               Operacion.Ctrl_EfeChe    EQ     Tipo AND 
               Operacion.Estado         EQ     1 AND 
               Operacion.Id_SYA         EQ     NO NO-LOCK NO-ERROR.
    IF AVAILABLE Operacion THEN DO:
       /*IF Creditos.Desembolso EQ 3 OR Creditos.Desembolso EQ 4  THEN
           FIND Comprobantes WHERE Comprobantes.Agencia     EQ Creditos.Agencia AND
                                   Comprobantes.Comprobante EQ 4 /*W_cbte*/ NO-ERROR.
       ELSE*/
       FIND Comprobantes WHERE Comprobantes.Agencia     EQ Creditos.Agencia AND
                               Comprobantes.Comprobante EQ Operacion.Comprobante /*W_cbte*/ NO-ERROR.
       IF NOT AVAILABLE Comprobantes THEN DO:
           MESSAGE "No se ha encontrado el comprobante para la contabilización" SKIP
                   "para el desembolso del crédito. en Operacion.Comprobante" SKIP 
                   "Rectifique con el Administrador!"
                   VIEW-AS ALERT-BOX ERROR. 
          RETURN ERROR.
       END.
      
       ASSIGN W_Cbte    = Comprobantes.Comprobante
              W_NumCbt  = Comprobantes.Secuencia + 1
              Comprobantes.Secuencia = Comprobantes.Secuencia + 1.
       
       FIND CURRENT Comprobantes NO-LOCK NO-ERROR.
    END.
    ELSE DO:
      MESSAGE "No se encontró la operación de crédito para" SKIP
              "para el desembolso. se cancela la operación" VIEW-AS ALERT-BOX ERROR. 
      RETURN ERROR.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Halla_DispGaran wWin 
PROCEDURE Halla_DispGaran :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFI VAR W_RowidC AS ROWID.
  DEFI VAR W_RowidG AS ROWID.

  DEFI VAR W_NitC   LIKE Creditos.Nit.
  DEFI VAR W_NumSol LIKE Solicitud.Num_Solicitud.
  
  ASSIGN W_CredAval = Creditos.Monto
         W_RowidC   = ROWID(Creditos)
         W_RowidG   = ROWID(Garantias)
         W_NitC     = Creditos.Nit
         W_NumSol   = Creditos.Num_Solicitud.

  SESSION:SET-WAIT-STATE("General").
  FOR EACH Creditos WHERE Creditos.Nit         EQ W_NitC
                      AND Creditos.Estado EQ 2 NO-LOCK:
      FIND FIRST Garantias WHERE Garantias.Nit               EQ Creditos.Nit
                           AND Garantias.Num_Credito         EQ Creditos.Num_Credito
                           AND Garantias.Identificacion_Bien EQ Garantias.Identificacion_Bien:SCREEN-VALUE IN FRAME F_Admisible
                           AND Garantias.Estado              EQ 1
                           AND Garantias.Num_Solicitud       NE W_NumSol NO-LOCK NO-ERROR.
      IF AVAIL(Garantias) THEN
         ASSIGN W_CredAval = W_CredAval + Creditos.Sdo_capital. 
  END.
  SESSION:SET-WAIT-STATE("").

  ASSIGN W_CredAval:SCREEN-VALUE IN FRAME F_Admisible = STRING(W_CredAval)
         W_DispGaran                                  = DEC(Garantias.Val_Bien:SCREEN-VALUE) - W_CredAval
         W_DispGaran:SCREEN-VALUE                     = STRING(W_DispGaran).

  FIND Creditos  WHERE ROWID(Creditos)  EQ W_RowidC NO-ERROR. 
  FIND Garantias WHERE ROWID(Garantias) EQ W_RowidG NO-ERROR. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Halla_IntAntic wWin 
PROCEDURE Halla_IntAntic :
FIND FIRST PlanPagos WHERE PlanPagos.Nit EQ Creditos.Nit
                       AND PlanPagos.Cod_Credito EQ Creditos.Cod_Credito
                       AND PlanPagos.Num_Credito EQ Creditos.Num_Credito
                       AND PlanPagos.Id_PdoMes EQ 1 NO-ERROR.
ASSIGN W_IntAntic = ROUND(((Creditos.Monto) * Creditos.Tasa / 36000) * W_DiasAnt,0) /* 0 nh*/
       Creditos.INT_Anticipado = W_IntAntic
       Creditos.Int_AntDesembolso = W_IntAntic
       Creditos.Sdo_IntPag = W_IntAntic
       Creditos.Fec_PagAnti = W_FecIni
       /*PlanPagos.Pagos_IntAcum = W_IntAntic
       PlanPagos.Pagos_IntPdo = W_IntAntic
       PlanPagos.Fec_Inic = W_Fecha*/.

/*FIND CURRENT PlanPagos NO-LOCK NO-ERROR.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE imprimeCondiciones wWin 
PROCEDURE imprimeCondiciones :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 FORM 
   vcCondiciones COLUMN-LABEL "" AT ROW 15 COL 1
                                 VIEW-AS EDITOR INNER-CHARS 118 INNER-LINES 1
   WITH FRAME FCond DOWN COLUMN 1 WIDTH 132
   NO-ATTR-SPACE NO-VALIDATE NO-BOX USE-TEXT STREAM-IO.

    DISPLAY 
        vcCondiciones
        WITH FRAME FCond.
    DOWN WITH FRAME FCond.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprime_Aceptacion wWin 
PROCEDURE Imprime_Aceptacion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE tmorae AS DECIMAL INITIAL 0.
    DEFINE  VARIABLE  tipo AS  CHAR INITIAL 0.

    ASSIGN ValCol = "W5"   Dato = Clientes.Nit.
    RUN Llenar_Celda.
    ValCol = "C55".
    RUN Llenar_Celda.
    ASSIGN ValCol = "I7"   Dato = Clientes.Apellido1 + " " + Clientes.Apellido2 + " " + Clientes.nombre.
    RUN Llenar_Celda.
    ValCol = "A53".
    RUN Llenar_Celda.
    ASSIGN ValCol = "I9"   Dato = STRING(Creditos.Monto,"->>>,>>>,>>>.99").
    RUN Llenar_Celda.
    ASSIGN ValCol = "Z9"   Dato = STRING(W_TasaNominal:SCREEN-VALUE IN FRAME F_Solicitud, "999999").
    RUN Llenar_Celda.
    ASSIGN ValCol = "I11"  Dato = STRING(Creditos.Plazo,"9999").
    RUN Llenar_Celda.
    ASSIGN ValCol = "Z11"  Dato = STRING(Creditos.Tasa:SCREEN-VALUE IN FRAME F_Solicitud).
    RUN Llenar_Celda.    
    ASSIGN ValCol = "H13"  Dato = "".
    RUN periodos1.
    RUN Llenar_Celda.
    ASSIGN ValCol = "I15" Dato = "".    
   CASE Creditos.For_Pago:
         WHEN 1 THEN Dato = "Caja".
         WHEN 2 THEN Dato = "Nomina".
         WHEN 3 THEN Dato = "Débito Automático".
         WHEN 4 THEN Dato = "Nomina Crecediario".
         WHEN 5 THEN Dato = "Prima".
    END CASE. 
    RUN Llenar_Celda.
    /*
    FIND FIRST Garantias WHERE Garantias.Agencia       = Creditos.Agencia
                           AND Garantias.Cod_Credito   = Creditos.Cod_Credito
                           AND Garantias.Tip_credito   = Creditos.Tip_Credito
                           AND Garantias.Num_Solicitud = Creditos.Num_Solicitud NO-LOCK NO-ERROR.
    IF AVAIL Garantias THEN DO:
    */
       ASSIGN ValCol = "Z17"  Dato = "".
       tipo =  STRING (R_TipoGarantia:SCREEN-VALUE IN FRAME F_garantias).
       CASE tipo:
            WHEN "1" THEN Dato = "Personal".
            WHEN "2" THEN Dato = "Admisible".
            WHEN "3" THEN Dato = "No Admisible".
       END CASE. 
       RUN Llenar_Celda.
    /* END. */
            FOR EACH  pro_creditos where  pro_creditos.tip_credito EQ creditos.tip_credito AND
                pro_creditos.cod_credito EQ creditos.cod_credito:            
                FIND indicadores WHERE  indicadores.indicador EQ pro_creditos.cod_tasamora  NO-LOCK NO-ERROR.
            IF AVAIL  indicadores THEN DO:
                ASSIGN ValCol = "Z15" dato = STRING(indicadores.Tasa,"99.99"). 
                RUN Llenar_celda.

/*     
              tmorae = DECIMAL(EXP( 1 + (DECIMAL(Indicadores.Tasa / 100)/ periodo),periodo) - 1) * 100. */
                RUN EFNV IN W_ManFin (INPUT (DECIMAL(indicadores.Tasa / 100)), Periodo, OUTPUT Tmorae). 
                tmorae = (tmorae * periodo )* 100.
                ASSIGN  ValCol = "Z13" dato = STRING(Tmorae,"99.99").
                   RUN Llenar_celda.

            END.
            END.
    
    FIND FIRST Relaciones WHERE Relaciones.Nit          = Creditos.Nit
                            AND Relaciones.Cuenta       = STRING(Creditos.Num_Credito)
                            AND Relaciones.Cod_Relacion = 11 NO-LOCK NO-ERROR.
    IF AVAIL Relaciones THEN DO:
       FIND Clientes WHERE Clientes.nit = Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
       ASSIGN ValCol = "M53" Dato = Clientes.Apellido1 + " " + Clientes.Apellido2 + " " + Clientes.nombre.
       RUN Llenar_Celda.
       ASSIGN ValCol = "O55" Dato = Clientes.Nit.
       RUN Llenar_Celda.
       FIND NEXT Relaciones WHERE Relaciones.Nit          = Creditos.Nit
                              AND Relaciones.Cuenta       = STRING(Creditos.Num_Credito)
                              AND Relaciones.Cod_Relacion = 11 NO-LOCK NO-ERROR.
       IF AVAIL Relaciones THEN DO:
          FIND Clientes WHERE Clientes.nit = Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
          ASSIGN ValCol = "Y53" Dato = Clientes.Apellido1 + " " + Clientes.Apellido2 + " " + Clientes.nombre.
          RUN Llenar_Celda.
          ASSIGN ValCol = "Z55" Dato = Clientes.Nit.
          RUN Llenar_Celda.
       END.
    END.
    ASSIGN ValCol = "AG56"  Dato = Usuario.Nombre.
    RUN Llenar_Celda.
    FIND Clientes WHERE Clientes.nit = Creditos.Nit NO-LOCK NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE imprime_autorizacion wWin 
PROCEDURE imprime_autorizacion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     DEF VAR Dato1   AS CHA NO-UNDO.
     DEF VAR Dato2   AS CHA NO-UNDO.
  

        FIND FIRST Ubicacion WHERE Ubicacion.Tipo = "C" AND Ubicacion.Ubicacion =(agencias.ciudad) 
        AND agencias.agencia = int(substr(cmb_agencias:SCREEN-VALUE IN FRAME f_solicitud,1,3)) NO-LOCK NO-ERROR.
    IF AVAIL Ubicacion THEN DO:
       ASSIGN ValCol = "C4" Dato = Ubicacion.Nombre.
       RUN Llenar_Celda.
    END.
           ASSIGN ValCol = "V4" Dato = w_nom_agencia.
       RUN Llenar_Celda.

       ASSIGN ValCol = "AG4" Dato = STRING(creditos.num_credito).
       RUN Llenar_Celda.

         ASSIGN ValCol = "AG29" Dato = Usuario.Nombre.
    RUN Llenar_Celda.

      ASSIGN dato1 = Clientes.Apellido1 + " " + Clientes.Apellido2 + " " + Clientes.nombre.

     FIND FIRST Relaciones WHERE Relaciones.Nit          = Creditos.Nit
                            AND Relaciones.Cuenta       = STRING(Creditos.Num_Credito)
                            AND Relaciones.Cod_Relacion = 11 NO-LOCK NO-ERROR.
    IF AVAIL Relaciones THEN DO:
         FIND Clientes WHERE Clientes.nit = Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
            IF AVAILABLE(clientes) THEN
                ASSIGN dato2 = Clientes.Apellido1 + " " + Clientes.Apellido2 + " " + Clientes.nombre.
            ELSE
                MESSAGE "NO se encontro en clientes el nombre del codeudor" VIEW-AS ALERT-BOX INFO BUTTON OK.

         FIND NEXT Relaciones WHERE Relaciones.Nit          = Creditos.Nit
                            AND Relaciones.Cuenta       = STRING(Creditos.Num_Credito)
                            AND Relaciones.Cod_Relacion = 11 NO-LOCK NO-ERROR.
                IF AVAIL Relaciones THEN DO:
                  FIND Clientes WHERE Clientes.nit = Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
               ASSIGN ValCol = "A9"   Dato  = "NOSOTROS : " + dato1 + "," +
                   dato2 + " y " + Clientes.Apellido1 + " " + Clientes.Apellido2 + " " + Clientes.nombre.

    
                END. 
                 ELSE
                     ASSIGN ValCol = "A9" dato = "NOSOTROS : " + dato1 + " y " + dato2.

         END.
         ELSE 
              ASSIGN ValCol = "A9"  dato = dato1.


    RUN Llenar_Celda.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprime_CompCredito wWin 
PROCEDURE Imprime_CompCredito :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
----find --------------------------------------------------------------------------*/
        IF creditos.Monto NE ? THEN 
        ASSIGN ValCol = "AA5" Dato = STRING(Creditos.Monto:SCREEN-VALUE IN FRAME  f_solicitud,"99999999999").
        ELSE
            ASSIGN ValCol = "AA15" Dato = "00".
    RUN Llenar_Celda.
        ASSIGN ValCol = "A5" Dato = Agencias.Nombre.
    RUN Llenar_Celda.

    FIND FIRST clientes WHERE clientes.nit = creditos.nit:SCREEN-VALUE IN FRAME f_solicitud NO-LOCK NO-ERROR.
    IF NOT AVAILABLE(clientes) THEN RETURN NO-APPLY.
    ELSE  DO:
        ASSIGN ValCol = "A7" Dato   = Clientes.Apellido1 + " " + Clientes.Apellido2 + " " + Clientes.nombre.
    RUN Llenar_Celda.
    ASSIGN ValCol = "N7" Dato = Clientes.Nit.
    RUN Llenar_Celda.        

    ASSIGN  valcol = "Q5" dato = SUBSTRING(STRING(nom_producto:SCREEN-VALUE IN FRAME f_solicitud),10,20).
    RUN Llenar_Celda.

    ASSIGN ValCol = "U7" Dato = Clientes.DIR_Residencia.
    RUN Llenar_Celda.
    ASSIGN ValCol = "A9" Dato = STRING(Clientes.Tel_Residencia).
    RUN Llenar_Celda.
    ASSIGN ValCol = "A11" Dato =  STRING(Clientes.dir_comercial).
    RUN Llenar_Celda.
    ASSIGN ValCol = "AC9" Dato = STRING (Clientes.Tel_comercial).
    RUN Llenar_Celda.
    ASSIGN ValCol = "AC11" Dato = STRING (Clientes.salario + clientes.Ing_arriendos + clientes.Ing_Honorarios + 
                                          Ing_Financieros + Ing_Otros,"->>>,>>>,>>>.99").
    RUN Llenar_Celda.

           FIND FIRST Ubicacion WHERE Ubicacion.Tipo = "C" AND Ubicacion.Ubicacion 
            BEGINS SUBSTR(Clientes.Lugar_Residencia,1,5) NO-LOCK NO-ERROR.
       IF AVAIL Ubicacion THEN 
          ASSIGN ValCol = "F9" Dato = Ubicacion.Nombre.
       ELSE ASSIGN ValCol = "F9" Dato = "no encontrado".
       RUN Llenar_Celda.

       /* ubicacion profesion */
           FIND  varios WHERE  varios.codigo EQ clientes.cod_profesion  AND varios.tipo = 1 NO-LOCK NO-ERROR.
    IF AVAILABLE(varios) THEN 
       ASSIGN ValCol = "T11"  Dato = varios.descripcion.       
    ELSE ASSIGN ValCol = "T11" Dato = "no encontrado".
    RUN Llenar_Celda.



       FOR EACH Empresas WHERE Empresas.Cod_Empresa = Clientes.Cod_Empresa NO-LOCK:
           FIND FIRST clientes WHERE clientes.nit = Empresas.nit NO-LOCK NO-ERROR.
           IF AVAIL(clientes) THEN DO:
       ASSIGN ValCol = "N9"  Dato = clientes.nombre.
       RUN Llenar_Celda.
                  FIND FIRST Ubicacion WHERE Ubicacion.Tipo = "C" AND Ubicacion.Ubicacion 
            BEGINS SUBSTR(Clientes.Lugar_Residencia,1,5) NO-LOCK NO-ERROR.
       IF AVAIL Ubicacion THEN 
          ASSIGN ValCol = "L11" Dato = Ubicacion.Nombre.
       ELSE ASSIGN ValCol = "L11" Dato = "no encontrado".
       RUN Llenar_Celda.

           END.
           
    END.


    END.

    FIND FIRST Relaciones WHERE Relaciones.Nit          = Creditos.Nit
                            AND Relaciones.Cuenta       = STRING(Creditos.Num_Credito)
                            AND Relaciones.Cod_Relacion = 11 NO-LOCK NO-ERROR.
    IF AVAIL Relaciones THEN DO:
       FIND Clientes WHERE Clientes.nit = Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
       ASSIGN ValCol = "A15"   Dato  = Clientes.Apellido1 + " " + Clientes.Apellido2 + " " + Clientes.nombre.
            
       RUN Llenar_Celda.      
       ASSIGN ValCol = "N15" Dato = Clientes.DIR_Residencia.
       RUN Llenar_Celda.
       ASSIGN ValCol = "AB15" Dato = Clientes.Tel_Residencia.
       RUN Llenar_Celda.       
    END.
       FIND NEXT Relaciones WHERE Relaciones.Nit          = Creditos.Nit
                              AND Relaciones.Cuenta       = STRING(Creditos.Num_Credito)
                              AND Relaciones.Cod_Relacion = 11 NO-LOCK NO-ERROR.
       IF AVAIL Relaciones THEN DO:
          FIND Clientes WHERE Clientes.nit = Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
          ASSIGN ValCol = "A17"   Dato  = Clientes.Apellido1 + " " + Clientes.Apellido2 + " " + Clientes.nombre.
               
          RUN Llenar_Celda.
          ASSIGN ValCol = "N17" Dato = Clientes.DIR_Residencia.
          RUN Llenar_Celda.
          ASSIGN ValCol = "AB17" Dato = Clientes.Tel_Residencia.
          RUN Llenar_Celda.
       END.
       
    ASSIGN ValCol = "AF27"  Dato = Usuario.Nombre.
    RUN Llenar_Celda.
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
             age1 = creditos.age_debAutomatico
             pro1 = creditos.cod_Debautomatico
             cue1 = creditos.cue_Debautomatico.
        
    ASSIGN ValCol = "D5" Dato = Agencias.Nombre.
    RUN Llenar_Celda.
    ASSIGN ValCol = "P23" Dato   = "" + Clientes.Apellido1 + " " + Clientes.Apellido2 + " " + Clientes.nombre.
     RUN Llenar_Celda.
     ASSIGN ValCol = "B32" Dato = Usuario.Nombre.
     RUN Llenar_Celda.
     ASSIGN valcol ="AG19" dato = STRING(creditos.agencia) + "-" + STRING(creditos.cod_credito) + "-" + STRING(creditos.num_credito).
    RUN Llenar_Celda.

     
            ASSIGN ValCol = "B19" Dato = STRING(creditos.age_debautomatico) + "-" +
        string(creditos.cod_debautomatico) + "-" + creditos.cue_debautomatico.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprime_Libranza wWin 
PROCEDURE Imprime_Libranza :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR Dato1 AS CHA NO-UNDO.
    ASSIGN ValCol = "D6" Dato = Agencia.Nombre.
    RUN Llenar_Celda.
    FIND FIRST Ahorros WHERE Ahorros.Nit = Creditos.Nit AND Ahorros.Tip_Ahorro = 4 NO-LOCK NO-ERROR.
    IF AVAIL Ahorros THEN DO:
       ASSIGN ValCol = "AD6" Dato = Ahorros.Cue_Ahorro.
       RUN Llenar_Celda.
    END.
    FIND Empresas WHERE Empresas.Cod_Empresa = Clientes.Cod_Empresa NO-LOCK NO-ERROR.
    IF AVAIL Empresas THEN DO:
       ASSIGN ValCol = "H7"  Dato = Empresas.Alias_Empresa.
       RUN Llenar_Celda.
       ASSIGN ValCol = "L11" Dato = Empresas.Alias_Empresa.
       RUN Llenar_Celda.
    END.
    ASSIGN ValCol = "F8"  Dato = Clientes.Apellido1 + " " + Clientes.Apellido2 + " " + Clientes.nombre.
    RUN Llenar_Celda.
    ValCol = "C44".
    RUN Llenar_Celda.
    ValCol = "G50".
    RUN Llenar_Celda.
    ASSIGN ValCol = "A11" Dato = Clientes.Tipo_Actividad.
    RUN Llenar_Celda.
    ASSIGN ValCol = "Z12" Dato = STRING(ROUND(Creditos.Cuota * Creditos.Plazo,0),"->>>,>>>,>>>.99").
    RUN Llenar_Celda.
    RUN MontoEsc.r (ROUND(Creditos.Cuota * Creditos.Plazo,0),INPUT 0,OUTPUT Dato).
    ValCol = "A13".
    RUN Llenar_Celda.
    RUN MontoEsc.r (Creditos.Plazo,INPUT 6,OUTPUT Dato).
    Dato = REPLACE(Dato,"cuota(s)","").
    ValCol = "L15".
    RUN Llenar_Celda.
    ASSIGN ValCol = "Z15" Dato = STRING(Creditos.Plazo,"->>>").
    RUN Llenar_Celda.
    ASSIGN ValCol = "I16" Dato = STRING(Creditos.Cuota,"->>>,>>>,>>>.99").
    RUN Llenar_Celda.
        
    ASSIGN ValCol = "AF15" Dato = "".
    RUN periodos1. 
    RUN Llenar_Celda.

    ASSIGN ValCol = "R6" Dato = STRING(Creditos.Num_credito,"99999999").
    RUN Llenar_Celda.

    ASSIGN ValCol = "Z18" Dato = STRING(Creditos.Tasa:SCREEN-VALUE IN FRAME F_Solicitud).
    RUN Llenar_Celda.
    Dato1 = "".
    FIND FIRST Relaciones WHERE Relaciones.Nit          = Creditos.Nit
                            AND Relaciones.Cuenta       = STRING(Creditos.Num_Credito)
                            AND Relaciones.Cod_Relacion = 11 NO-LOCK NO-ERROR.
    IF AVAIL Relaciones THEN DO:
       FIND Clientes WHERE Clientes.nit = Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
       ASSIGN ValCol = "O44" Dato  = Clientes.Apellido1 + " " + Clientes.Apellido2 + " " + Clientes.nombre
                             Dato1 = Dato.
       RUN Llenar_Celda.
       ASSIGN ValCol = "O45" Dato  = Clientes.Nit.
       RUN Llenar_Celda.
       FIND NEXT Relaciones WHERE Relaciones.Nit          = Creditos.Nit
                              AND Relaciones.Cuenta       = STRING(Creditos.Num_Credito)
                              AND Relaciones.Cod_Relacion = 11 NO-LOCK NO-ERROR.
       IF AVAIL Relaciones THEN DO:
          FIND Clientes WHERE Clientes.nit = Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
          ASSIGN ValCol = "AA44" Dato = Clientes.Apellido1 + " " + Clientes.Apellido2 + " " + Clientes.nombre
                                 Dato1 = Dato1 + " y " + Dato.
          RUN Llenar_Celda.
          ASSIGN ValCol = "AA45" Dato = Clientes.Nit.
          RUN Llenar_Celda.
       END.
    END.
    ASSIGN ValCol = "A24" Dato = Dato1.
    RUN Llenar_Celda.
    FIND Clientes WHERE Clientes.nit = Creditos.Nit NO-LOCK NO-ERROR.

        FIND FIRST Ubicacion WHERE Ubicacion.Tipo = "C" AND Ubicacion.Ubicacion =(agencias.ciudad) 
        AND agencias.agencia = int(substr(cmb_agencias:SCREEN-VALUE IN FRAME f_solicitud,1,3)) NO-LOCK NO-ERROR.
    IF AVAIL Ubicacion THEN DO:
       ASSIGN ValCol = "W39" Dato = Ubicacion.Nombre.
       RUN Llenar_Celda.
    END.

    ASSIGN ValCol = "AH39" Dato = STRING(DAY  (TODAY),"99").
    RUN Llenar_Celda.
    ASSIGN ValCol = "G40"  Dato =  W_Meses[MONTH(TODAY)].
    RUN Llenar_Celda.
    ASSIGN ValCol = "P40"  Dato = STRING(YEAR (TODAY),"9999").
    RUN Llenar_Celda.
    ASSIGN ValCol = "C45"  Dato = Clientes.Nit.
    RUN Llenar_Celda.
    ASSIGN ValCol = "AG54" Dato = Usuario.Nombre.
    RUN Llenar_Celda.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprime_Pagare wWin 
PROCEDURE Imprime_Pagare :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    DEF VAR Dato1   AS CHA NO-UNDO.


    FIND FIRST Ahorros WHERE Ahorros.Nit = Creditos.Nit AND Ahorros.Tip_Ahorro = 4 NO-LOCK NO-ERROR.
    IF AVAIL Ahorros THEN DO:
      ASSIGN ValCol = "AB5" Dato = Ahorros.Cue_Ahorro.
      RUN Llenar_Celda.
    END.
   
    ASSIGN ValCol = "D5" Dato = Agencias.Nombre.
    RUN Llenar_Celda.
    ASSIGN ValCol = "D44" Dato   = Clientes.Apellido1 + " " + Clientes.Apellido2 + " " + Clientes.nombre
           Dato1  = "NOSOTROS: " + Clientes.Apellido1 + " " + Clientes.Apellido2 + " " + Clientes.nombre.
    RUN Llenar_Celda.
    ASSIGN ValCol = "D45" Dato = Clientes.Nit.
    RUN Llenar_Celda.
    ASSIGN ValCol = "D46" Dato = Clientes.DIR_Residencia.
    RUN Llenar_Celda.
    ASSIGN ValCol = "D47" Dato = Clientes.Tel_Residencia.
    RUN Llenar_Celda.
    FIND FIRST Ubicacion WHERE Ubicacion.Tipo = "B" AND Ubicacion.Ubicacion = Clientes.Lugar_Residencia NO-LOCK NO-ERROR.
    IF AVAIL Ubicacion THEN DO:
       ASSIGN ValCol = "D48" Dato =  Ubicacion.Nombre +
              IF Ubicacion.Comuna < 1 THEN "" ELSE " - Comuna " + STRING(Ubicacion.Comuna).
       RUN Llenar_Celda.
    END.
       FIND FIRST Ubicacion WHERE Ubicacion.Tipo = "C" AND Ubicacion.Ubicacion 
            BEGINS SUBSTR(Clientes.Lugar_Residencia,1,5) NO-LOCK NO-ERROR.
       IF AVAIL Ubicacion THEN DO:
          ASSIGN ValCol = "D49" Dato = Ubicacion.Nombre.
          RUN Llenar_Celda.
       END.

    FIND FIRST Ubicacion WHERE Ubicacion.Tipo = "C" AND Ubicacion.Ubicacion =(agencias.ciudad) 
        AND agencias.agencia = int(substr(cmb_agencias:SCREEN-VALUE IN FRAME f_solicitud,1,3)) NO-LOCK NO-ERROR. 
         /*BEGINS SUBSTR(Clientes.Lugar_Residencia,1,5) NO-LOCK NO-ERROR. */
    IF AVAIL Ubicacion THEN DO:
       ASSIGN ValCol = "P40" Dato = Ubicacion.Nombre.
       RUN Llenar_Celda.
    END.


    FIND FIRST Relaciones WHERE Relaciones.Nit          = Creditos.Nit
                            AND Relaciones.Cuenta       = STRING(Creditos.Num_Credito)
                            AND Relaciones.Cod_Relacion = 11 NO-LOCK NO-ERROR.
    IF AVAIL Relaciones THEN DO:
       FIND Clientes WHERE Clientes.nit = Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
       ASSIGN ValCol = "V44"   Dato  = Clientes.Apellido1 + " " + Clientes.Apellido2 + " " + Clientes.nombre
              Dato1  = Dato1 + " , " + Clientes.Apellido1 + " " + Clientes.Apellido2 + " " + Clientes.nombre.
       RUN Llenar_Celda.
       ASSIGN ValCol = "V45" Dato = Clientes.Nit.
       RUN Llenar_Celda.
       ASSIGN ValCol = "V46" Dato = Clientes.DIR_Residencia.
       RUN Llenar_Celda.
       ASSIGN ValCol = "V47" Dato = Clientes.Tel_Residencia.
       RUN Llenar_Celda.       
       FIND FIRST Ubicacion WHERE Ubicacion.Tipo = "B" AND Ubicacion.Ubicacion = Clientes.Lugar_Residencia NO-LOCK NO-ERROR.
       IF AVAIL Ubicacion THEN DO:
          ASSIGN ValCol = "V48" Dato =  Ubicacion.Nombre +
                 IF Ubicacion.Comuna < 1 THEN "" ELSE " - Comuna " + STRING(Ubicacion.Comuna).
          RUN Llenar_Celda.
       END.
       FIND FIRST Ubicacion WHERE Ubicacion.Tipo = "C" AND Ubicacion.Ubicacion 
            BEGINS SUBSTR(Clientes.Lugar_Residencia,1,5) NO-LOCK NO-ERROR.
       IF AVAIL Ubicacion THEN DO:
          ASSIGN ValCol = "V49" Dato = Ubicacion.Nombre.
          RUN Llenar_Celda.
       END.
       FIND NEXT Relaciones WHERE Relaciones.Nit          = Creditos.Nit
                              AND Relaciones.Cuenta       = STRING(Creditos.Num_Credito)
                              AND Relaciones.Cod_Relacion = 11 NO-LOCK NO-ERROR.
       IF AVAIL Relaciones THEN DO:
          FIND Clientes WHERE Clientes.nit = Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
          ASSIGN ValCol = "D53"   Dato  = Clientes.Apellido1 + " " + Clientes.Apellido2 + " " + Clientes.nombre
                 Dato1  = Dato1 + " , " + Clientes.Apellido1 + " " + Clientes.Apellido2 + " " + Clientes.nombre.
          RUN Llenar_Celda.
          ASSIGN ValCol = "D54" Dato = Clientes.Nit.
          RUN Llenar_Celda.
          ASSIGN ValCol = "D55" Dato = Clientes.DIR_Residencia.
          RUN Llenar_Celda.
          ASSIGN ValCol = "D56" Dato = Clientes.Tel_Residencia.
          RUN Llenar_Celda.
          FIND FIRST Ubicacion WHERE Ubicacion.Tipo = "B" AND Ubicacion.Ubicacion = Clientes.Lugar_Residencia NO-LOCK NO-ERROR.
          IF AVAIL Ubicacion THEN DO:
             ASSIGN ValCol = "D57" Dato =  Ubicacion.Nombre +
                    IF Ubicacion.Comuna < 1 THEN "" ELSE " - Comuna " + STRING(Ubicacion.Comuna).
             RUN Llenar_Celda.
          END.
          FIND FIRST Ubicacion WHERE Ubicacion.Tipo = "C" AND Ubicacion.Ubicacion
               BEGINS SUBSTR(Clientes.Lugar_Residencia,1,5) NO-LOCK NO-ERROR.
          IF AVAIL Ubicacion THEN DO:
             ASSIGN ValCol = "D58" Dato = Ubicacion.Nombre.
             RUN Llenar_Celda.
          END.
       END.
    END.

    ASSIGN ValCol = "A9" Dato = Dato1.
    RUN Llenar_Celda.

     IF ptipo= 1 OR ptipo = 2 THEN DO:
         ASSIGN ValCol = "E7" Dato = STRING(Creditos.Agencia,"99")
            + "-" +  STRING(Creditos.Cod_Credito,"999") + "-" + STRING(Creditos.Num_Credito,"99999999").
    RUN Llenar_Celda.

        ASSIGN ValCol = "AC13" Dato = STRING(Creditos.Monto,"->>>,>>>,>>>.99").
    RUN Llenar_Celda. 

    ASSIGN ValCol = "Q5" Dato = STRING(Creditos.Num_credito,"99999999").
    RUN Llenar_Celda.

    ASSIGN ValCol = "P7" Dato = STRING(Creditos.Monto,"->>>,>>>,>>>.99").
    RUN Llenar_Celda.

    RUN MontoEsc.r (Creditos.Monto,INPUT 0,OUTPUT Dato).
    ValCol = "D13".
    RUN Llenar_Celda.
    
    ASSIGN ValCol = "AF15" Dato = STRING(Creditos.Plazo,"->>9").
    RUN Llenar_Celda.

    
    ASSIGN ValCol = "A16" Dato = "".
    RUN periodos1. 

    RUN Llenar_Celda.

    ASSIGN ValCol = "A17"  Dato = "".
    CASE Creditos.For_Interes:
         WHEN 1 THEN Dato = "Vencido".
         WHEN 2 THEN Dato = "Anticipado".
    END CASE.
    RUN Llenar_Celda.
    RUN MontoEsc.r (ROUND(decimal(Creditos.Tasa:SCREEN-VALUE IN FRAME f_solicitud),2),INPUT 1,OUTPUT Dato).
    ASSIGN Dato = REPLACE(Dato,"Pesos","Punto ")
           Dato = REPLACE(Dato,"Cvs.Moneda Legal.","").
    ValCol = "I17".
    RUN Llenar_Celda.

    ASSIGN ValCol = "X17" Dato = STRING(Creditos.Tasa:SCREEN-VALUE IN FRAME f_solicitud,"99999").
    RUN Llenar_Celda.
    ASSIGN ValCol = "W40" Dato = STRING(DAY(TODAY),"99") + " de " +
           W_Meses[MONTH(TODAY)] + " de " + STRING(YEAR(TODAY),"9999").
    RUN Llenar_Celda.
    ASSIGN ValCol = "A59" Dato = Usuario.Nombre.
    RUN Llenar_Celda.
/*
       ASSIGN  valcol =  "S38" dato =  STRING(finicio) + "-" + STRING(ndias) + "--" + STRING(creditos.plazo) + "--" +  string(mesan) + "--" +  STRING(W_FECTRA) + "--" +  STRING(w-fecentlib) + "--" + STRING(tcuota).
    RUN Llenar_Celda.
     ASSIGN  valcol =  "A39" dato =  STRING(finicio)   + "--" +  STRING(fefin).
     RUN Llenar_Celda.
*/
    IF  ptipo = 1 THEN DO: 
  
       ASSIGN ValCol = "ac7" Dato = STRING(DAY(W_FecTra),"99") + " de " +
           W_Meses[MONTH(W_FecTra)] + " de " + STRING(YEAR(W_FecTra),"9999").
   /* ASSIGN  valcol =  "AC7" dato = STRING(fefin). */
    RUN Llenar_Celda.
    ASSIGN ValCol = "S16" Dato = STRING(DAY(w-fecEntLib),"99") + " de " +  W_Meses[MONTH(w-fecEntLib)] + " de " + STRING(YEAR(w-fecEntLib),"9999").
    RUN Llenar_Celda.

     END. 
     END.
     FIND Clientes WHERE Clientes.nit = Creditos.Nit NO-LOCK NO-ERROR.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imp_Cheque wWin 
PROCEDURE Imp_Cheque :
/*------------------------------------------------------------------------------
  OBSERVACIONES : Permite Imprimir los Cheques de Egresos de la Entidad.       
------------------------------------------------------------------------------*/
  
  DEFINE INPUT PARAMETER C_Valor  LIKE Ahorros.Sdo_Disponible.
  DEFINE INPUT PARAMETER C_Benef  AS   CHARACTER FORMAT "X(50)".
  DEFINE INPUT PARAMETER C_Ciudad LIKE Ubicacion.Nombre.

  DEFINE VAR W_Cadena AS CHARACTER FORMAT "X(150)".
  DEFINE VAR W_Monto1 AS CHARACTER FORMAT "X(70)".
  DEFINE VAR W_Monto2 AS CHARACTER FORMAT "X(70)".
  DEFINE VAR W_Monto3 AS CHARACTER FORMAT "X(70)".
  DEFINE VAR W_Rpta   AS LOGICAL.
  DEFINE VAR vctipo   AS CHARAC FORMAT "X(1)".

/*  MESSAGE "Desea Sacar una Prueba para cuadrar" skip
         "El Formato del Cheque?"
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                    TITLE "Prueba de Cheque" UPDATE choice AS LOGICAL.
  IF CHOICE THEN
    RUN VALUE(W_ProFor)
             (INPUT "",INPUT "",INPUT "",INPUT "",INPUT "",INPUT "") NO-ERROR.*/
  MESSAGE "Desea Imprimir Ahora el Cheque?"
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                    TITLE "Impresion de Cheque" UPDATE choice2 AS LOGICAL.
  IF CHOICE2 THEN DO:
     ASSIGN vctipo   = "1".
            
     RUN MontoEsc.r (INPUT C_Valor,INPUT 0,OUTPUT W_Cadena).
     RUN PartirValor IN W_Manija (INPUT W_Cadena,INPUT 60,OUTPUT W_Monto1,OUTPUT W_Monto2,OUTPUT W_Monto3).
     

     RUN F-Cheque.p( INPUT W_monto1, 
                     INPUT W_monto2, 
                     INPUT C_benef, 
                     INPUT c_ciudad, 
                     INPUT C_Valor,
                     INPUT Mov_Contable.Comentario, 
                     INPUT Mov_Contable.Comprobante,
                     INPUT "",  /*INPUT wxctacon,*/ 
                     INPUT Mov_Contable.Num_Documento).

/*      RUN F-Cheque1.R (INPUT vctipo,INPUT vcnit,INPUT vinotran,                   */
/*                      INPUT W_Monto1,INPUT W_Monto2,INPUT C_Benef,INPUT W_Ciudad, */
/*                      INPUT C_Valor, INPUT "") NO-ERROR.                          */

    /* RUN F-Cheque1.p (INPUT vctipo,INPUT vcnit,INPUT vinotran, /* 13-Mayo-2008  Félix Vargas*/
                         INPUT W_Monto1,INPUT W_Monto2,INPUT W_NomBenCC,INPUT W_Ciudad,    /*<----Con el Nom-Benef: vble W_NomBenCC, capturado*/
                         INPUT Mov_Contable.Cr, INPUT Mov_Contable.Comentario) NO-ERROR.*/


     IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "Error al imprimir el Cheque" 
                 VIEW-AS ALERT-BOX ERROR. 
        RETURN ERROR.
     END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informacion_Credito wWin 
PROCEDURE Informacion_Credito :
DEFINE VARIABLE Linea AS CHARACTER FORMAT "X(125)".
DEFINE VAR T_Plazo AS CHARACTER FORMAT "X(30)".
DEFINE VAR T_Dedu AS CHARACTER FORMAT "X(30)".

DO WITH FRAME F_Solicitud:
    CREATE TTImpresion.

    T_Plazo = Creditos.Plazo:SCREEN-VALUE  + " (" + SUBSTRING(Cmb_PerPago:SCREEN-VALUE,5,15) + ")".

    CASE Creditos.Id_Adicionales:SCREEN-VALUE:
        WHEN "1" THEN T_Dedu = "Financiados".
        WHEN "2" THEN T_Dedu = "Descontados".
        WHEN "3" THEN T_Dedu = "Pagados por Caja".
    END CASE.

    Linea = "=============================================DATOS GENERALES DE LA SOLICITUD==============================================".

    RUN TmpL (INPUT 1, INPUT Linea).

    UPDATE TTImpresion.reg01 = Linea.

    Linea = "Agencia de Radicación       : " + STRING(Cmb_Agencias:SCREEN-VALUE,"X(30)") + "  Número del Crédito          : " + STRING(Creditos.Num_Credito:SCREEN-VALUE).

    RUN TmpL (INPUT 2, INPUT Linea).

    UPDATE TTImpresion.reg02 = Linea.

    Linea = "Número de Solicitud         : " + STRING(Creditos.Num_Solicitud:SCREEN-VALUE,"X(30)") + "  Fecha de Aprobación         : " + STRING(Creditos.Fec_Aprobacion:SCREEN-VALUE,"X(10)").

    RUN TmpL (INPUT 3, INPUT Linea).

    UPDATE TTImpresion.reg03 = Linea.

    Linea = "Producto de Crédito         : " + STRING(Nom_Producto:SCREEN-VALUE,"X(30)") + "  Tipo de Producto            : " + STRING(TRIM(W_Tipo_Credito:SCREEN-VALUE),"X(30)").

    RUN TmpL (INPUT 4, INPUT Linea).

    UPDATE TTImpresion.reg04 = Linea.

    Linea = "Instancia Actual            : " + STRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,"X(40)").

    RUN TmpL (INPUT 5, INPUT Linea).

    UPDATE TTImpresion.reg05 = Linea.

    Linea = "Forma de Pago de la Cuota   : " + STRING(W_ForPago:SCREEN-VALUE,"X(30)") + "  Desembolso del Crédito      : " + STRING(W_Desembolso:SCREEN-VALUE,"X(30)").

    RUN TmpL (INPUT 6, INPUT Linea).

    UPDATE TTImpresion.reg06 = Linea.

    Linea = "=============================================DETALLE DE VALORES DEL CREDITO==============================================".

    RUN TmpL (INPUT 7, INPUT Linea).

    UPDATE TTImpresion.reg07 = Linea.

    Linea = "Monto a Prestar             : " + STRING(Creditos.Monto:SCREEN-VALUE,"X(30)") + "  Tasa Efectiva Anual         : " + STRING(Creditos.Tasa:SCREEN-VALUE,"X(30)").

    RUN TmpL (INPUT 8, INPUT Linea).

    UPDATE TTImpresion.reg08 = Linea.

    Linea = "Plazo                       : " + STRING(T_Plazo,"X(30)") + "  Tasa Nomina Anual           : " + STRING(W_TasaNominal:SCREEN-VALUE,"X(30)").

    RUN TmpL (INPUT 9, INPUT Linea).

    UPDATE TTImpresion.reg09 = Linea.

    Linea = "Cuota del Período           : " + STRING(Creditos.Cuota:SCREEN-VALUE,"X(30)") + "  Tasa del Período            : " + STRING(W_TasaPeriodo:SCREEN-VALUE,"X(30)").

    RUN TmpL (INPUT 10, INPUT Linea).
    
    UPDATE TTImpresion.reg10 = Linea.

    Linea = "Valor a Deducir             : " + STRING(Creditos.Deducible:SCREEN-VALUE,"X(30)") + "  Pago de Valor a Deducir     : " + STRING(T_Dedu,"X(30)").

    RUN TmpL (INPUT 11, INPUT Linea).

    UPDATE TTImpresion.reg11 = Linea.

    Linea = "Valor del Incremento        : " + STRING(Creditos.Incremento:SCREEN-VALUE,"X(30)").

    RUN TmpL (INPUT 12, INPUT Linea).

    UPDATE TTImpresion.reg12 = Linea.

    Linea = "==============================================OTRAS CONDICIONES=================================================".

    RUN TmpL (INPUT 13, INPUT Linea).

    UPDATE TTImpresion.reg13 = Linea.

    UPDATE TTImpresion.condiciones = vcCondiciones.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informe wWin 
PROCEDURE Informe :
{Incluido\RepEncabezado.i}

    DEFINE VAR W_Cliente AS CHARACTER FORMAT "X(60)".
    DEFINE VAR T_Plazo   AS CHARACTER FORMAT "X(30)".
    DEFINE VAR T_Dedu    AS CHARACTER FORMAT "X(30)".
    
    ASSIGN W_Cliente = Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud + " - " + NomNit:SCREEN-VALUE IN FRAME F_Solicitud.
 
    W_Reporte   = "REPORTE   : CREDITO - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").

    
    IF W_TipoInforme NE "Deducibles" THEN
       W_EncColumna = "Cliente Solicitante         :   " + W_Cliente.
    ELSE
       W_EncColumna = "Nombre Deducible                                  Valor Deducible      Valor Impuesto Deducible".
    
    
       VIEW FRAME F-Encabezado.
       VIEW FRAME F-Ftr.
       /*MESSAGE W_tipoinforme VIEW-AS ALERT-BOX.*/
       IF W_TipoInforme EQ "Deducibles" THEN DO:
          FOR EACH TDeducc:
              DISPLAY TDeducc.Nom_Deducible TDeducc.Valor TDeducc.Valor_Impuesto
            WITH FRAME FIDed WIDTH 132 NO-LABELS USE-TEXT NO-BOX STREAM-IO.
    END.
    W_TipoInforme = "".
 END.
 ELSE DO:
     FOR EACH TmpI: DELETE TmpI. END.
     RUN Informacion_Credito.
/*      RUN Garantias_Imprimir. */
     FOR EACH TmpI BREAK BY TmpI.ILinea:
       DISPLAY TmpI.ITexto WITH FRAME FText WIDTH 132 NO-LABELS USE-TEXT NO-BOX STREAM-IO.
     END.
 END.
 PAGE.

       /************************************************************************/
/*  VIEW FRAME F-Encabezado.                                                                            */
/*  VIEW FRAME F-Ftr.                                                                                   */
/*  /*MESSAGE W_tipoinforme VIEW-AS ALERT-BOX.*/                                                        */
/*  IF W_TipoInforme EQ "Deducibles" THEN DO:                                                           */
/*     FOR EACH TDeducc:                                                                                */
/*         DISPLAY TDeducc.Nom_Deducible TDeducc.Valor TDeducc.Valor_Impuesto                           */
/*             WITH FRAME FIDed WIDTH 132 NO-LABELS USE-TEXT NO-BOX STREAM-IO.                          */
/*     END.                                                                                             */
/*     W_TipoInforme = "".                                                                              */
/*  END.                                                                                                */
/*  ELSE DO:                                                                                            */
/*      EMPTY TEMP-TABLE TmpI.                                                                          */
/*      EMPTY TEMP-TABLE TTCondiciones.                                                                 */
/*      CREATE TTCondiciones.                                                                           */
/*      UPDATE TTCondiciones.nit = creditos.nit:SCREEN-VALUE IN FRAME F_Solicitud                       */
/*          TTCondiciones.condiciones = vcCondiciones.                                                  */
/*                                                                                                      */
/*      RUN Informacion_Credito.                                                                        */
/* /*      RUN Garantias_Imprimir. */ /*giocam Nov 26/07 - no se está usando*/                          */
/*                                                                                                      */
/*                                                                                                      */
/*      FORM                                                                                            */
/*          ITexto                                                                                      */
/*        vcCondiciones COLUMN-LABEL "" AT ROW 15 COL 1                                                 */
/*                                      VIEW-AS EDITOR INNER-CHARS 118 INNER-LINES 1                    */
/*        WITH FRAME FText DOWN COLUMN 1 WIDTH 132                                                      */
/*        NO-ATTR-SPACE NO-VALIDATE NO-BOX USE-TEXT STREAM-IO.                                          */
/*                                                                                                      */
/*      FOR EACH TTCondiciones NO-LOCK:                                                                 */
/*          FOR EACH TmpI WHERE TmpI.nit EQ TTCondiciones.nit BREAK BY TmpI.ILinea:                     */
/*                 DISPLAY                                                                              */
/*                     ITexto                                                                           */
/* /*                     vcCondiciones */                                                              */
/*                     WITH FRAME FText.                                                                */
/*                                                                                                      */
/* /*            DISPLAY TmpI.ITexto WITH FRAME FText WIDTH 132 NO-LABELS USE-TEXT NO-BOX STREAM-IO. */ */
/*          END.                                                                                        */
/*          DISPLAY vcCondiciones                                                                       */
/*                 WITH FRAME FText.                                                                    */
/*             DOWN WITH FRAME FText.                                                                   */
/*                                                                                                      */
/*      END.                                                                                            */
/* /*      RUN imprimeCondiciones. */                                                                   */
/*                                                                                                      */
/*  END.                                                                                                */
/*  PAGE.                                                                                               */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar_Admisible wWin 
PROCEDURE Inicializar_Admisible :
DO WITH FRAME F_Admisible:
   ASSIGN Garantias.Tipo_Garantia:SCREEN-VALUE     = "1"
        Garantias.Identificacion_Bien:SCREEN-VALUE = ""
        Garantias.Nom_Bien:SCREEN-VALUE            = ""
        Garantias.Descripcion_Bien:SCREEN-VALUE    = ""
        Garantias.Descripcion_Bien2:SCREEN-VALUE    = ""
        Garantias.Fec_Creacion:SCREEN-VALUE        = STRING(W_Fecha)
        Garantias.Fec_Retiro:SCREEN-VALUE          = ""
        Garantias.Nit_Aseguradora:SCREEN-VALUE     = ""
        Garantias.Nro_Seguro:SCREEN-VALUE          = ""
        Garantias.Fec_IniSeguro:SCREEN-VALUE       = ""
        Garantias.Fec_FinSeguro:SCREEN-VALUE       = ""
        Garantias.Val_Asegurado:SCREEN-VALUE       = "".

   ASSIGN W_CredAval                               = 0
        W_CredAval:SCREEN-VALUE                    = "0"
        W_DispGaran                                = 0
        W_DispGaran:SCREEN-VALUE                   = "0" 
        Nom_Aseguradora:SCREEN-VALUE               = ""
        Nom_UsuGarantia:SCREEN-VALUE               = ""
        Garantias.Val_Bien:SCREEN-VALUE            = "0"
        Garantias.Nom_Impuesto:SCREEN-VALUE        = ""
        Garantias.Fec_VctoImpuesto:SCREEN-VALUE    = ""
        Garantias.Val_Impuesto:SCREEN-VALUE        = ""
        Garantias.Fec_UltAvaluo:SCREEN-VALUE       = ""
        Garantias.Fec_ProxAvaluo:SCREEN-VALUE      = ""
        Garantias.Val_UltAvaluo:SCREEN-VALUE       = ""
        Garantias.Aprobada:SCREEN-VALUE            = "No"
        Garantias.Estado:SCREEN-VALUE              = "1".

   IF R_TipoGarantia:SCREEN-VALUE IN FRAME F_Garantias= "3" THEN
      ASSIGN Garantias.Tipo_Garantia:SCREEN-VALUE = "5".

   FIND Usuarios WHERE Usuarios.Usuario EQ W_Usuario NO-LOCK NO-ERROR.
   IF AVAILABLE Usuarios THEN
      ASSIGN Nom_UsuGarantia:SCREEN-VALUE = Usuario.Usuario + " - " + Usuarios.Nombre.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar_Variables wWin 
PROCEDURE Inicializar_Variables :
DO WITH FRAME F_Solicitud:
   FIND Agencias WHERE Agencias.Agencia EQ W_Agencia NO-LOCK NO-ERROR.
   IF AVAILABLE Agencias THEN DO:
      Cmb_Agencias:SCREEN-VALUE = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.
      APPLY "value-changed" TO Cmb_Agencias.
   END.
   IF NOT W_Nuevo THEN Creditos.Num_Credito:SCREEN-VALUE = "".
   ASSIGN Creditos.Nit:SCREEN-VALUE = ""
          NomNit:SCREEN-VALUE        = ""
          Nom_Producto:SCREEN-VALUE  = ""
          Creditos.Fec_Aprobacion:SCREEN-VALUE = STRING(TODAY)
          Creditos.FOR_Interes:SCREEN-VALUE   = "1"
          Creditos.Monto:SCREEN-VALUE         = "0"
          Creditos.Plazo:SCREEN-VALUE         = "0"
          Creditos.Cuota:SCREEN-VALUE         = "0"
          Creditos.Incremento:SCREEN-VALUE    = "0"
          Creditos.Incremento:HIDDEN          = YES
          Creditos.Deducible:SCREEN-VALUE     = "0"
          Cmb_Perpago:SCREEN-VALUE             = "4 - Mensual"
          Creditos.Tasa:SCREEN-VALUE          = "0"
          W_TasaNominal:SCREEN-VALUE           = "0"
          W_TasaPeriodo:SCREEN-VALUE           = "0"
          W_Desembolso:SCREEN-VALUE            = "Efectivo"
          W_ForPago:SCREEN-VALUE               = "Nomina".
          /*Cmb_Sistemas:SCREEN-VALUE            = Cmb_Sistemas:ENTRY(1).*/
    FOR EACH TScoring: DELETE TScoring. END.
    OPEN QUERY Br_Scoring FOR EACH TScoring NO-LOCK INDEXED-REPOSITION.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
FIND Usuarios WHERE Usuarios.Usuario EQ W_Usuario NO-LOCK NO-ERROR.
    ASSIGN  NomUsuario = W_Usuario + " - " + Usuarios.Nombre.
            .
    DO WITH FRAME F_Solicitud:
        FOR EACH Agencias WHERE Agencia.Estado EQ 1 NO-LOCK:
            W_Ok = Cmb_Agencias:ADD-LAST(STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre).
        END.
        FOR EACH Varios WHERE Varios.Tipo EQ 20:
            W_Ok = Cmb_Sistemas:ADD-LAST(STRING(Varios.Codigo,"99999") + " - " + Varios.Descripcion).
        END.
        Cmb_Sistemas:SCREEN-VALUE = Cmb_Sistemas:ENTRY(1).
        /*IF AVAILABLE Solicitud THEN RUN Mostrar_Solicitud.
        ELSE*/ RUN Inicializar_Variables.
    END.                             
    RUN SUPER.
    FOR EACH Instancias 
        WHERE 
            Instancias.Tipo_Instancia EQ 3 
        AND Instancias.Estado         EQ 1 
        AND Instancias.Tipo_Producto  EQ 2 
        NO-LOCK BREAK BY Instancias.Orden:
        IF Instancias.Ultima 
        THEN W_Ultima = Instancias.Instancia.
        IF Instancias.Primera 
        THEN W_Primera = Instancias.Instancia.
        FIND FIRST Cfg_Instancias 
            WHERE
                /* Cfg_Instancias.Agencia EQ W_Agencia AND*/
                Cfg_Instancias.Tipo_Instancia EQ 3  
            AND Cfg_Instancias.Instancia EQ Instancias.Instancia 
            AND Cfg_Instancias.Usuario EQ W_Usuario 
            AND Cfg_Instancias.Estado  EQ 1 NO-LOCK NO-ERROR.
        IF AVAILABLE Cfg_Instancias 
        THEN DO:
            W_Ok = Cmb_Instancias:ADD-LAST(STRING(Instancias.Instancia,"99999") + " - " + Instancias.Nom_Instancia) IN FRAME F_Creditos.
            W_Ok = Cmb_InsCon:ADD-LAST(STRING(Instancias.Instancia,"99999") + " - " + Instancias.Nom_Instancia) IN FRAME F_Condicionada.
        END.
    END.
    IF W_Ultima EQ 0 
    THEN DO:
        MESSAGE "No se ha definido la ultima instancia en el proceso de" SKIP
                "Solicitud. define una instancia con el parametro ultimo" VIEW-AS ALERT-BOX.
    END.
    IF W_Primera EQ 0 
    THEN DO:
        MESSAGE "No se ha definido la primera instancia en el proceso de" SKIP
                "Solicitud. define una instancia con el parametro primera" VIEW-AS ALERT-BOX.
    END.
    Cmb_Instancias:SCREEN-VALUE = Cmb_Instancias:ENTRY(1).
    RUN Creditos_X_Instancia.
    APPLY "entry" TO Cmb_Instancias IN FRAME F_Creditos.
    RUN Inicializar_Variables.
    HIDE FRAME F_AsentarInstancia.
    HIDE FRAME F_Cerradas.
    HIDE FRAME F_Agregar.
    HIDE FRAME F_Bancos.
    WWin:TITLE = "Proceso de Desembolso - Agencia Actual: " + STRING(W_Agencia).
    Buscar:LABEL IN FRAME F_Consulta = "Buscar x Crédito".
    Creditos.Estado:DISABLE("NAp.") IN FRAME F_Ultima NO-ERROR.
    Creditos.Estado:DISABLE("N.Ap").
    IF W_Ultima EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) 
    THEN DO:
        DISABLE ALL WITH FRAME F_Creditos. 
        ENABLE Btn_Consulta Btn_ProInstancia Button-2 WITH FRAME F_Creditos. 
        APPLY "Choose" TO Btn_Consulta.
    END.
    RUN cargaTexto.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Liquidar wWin 
PROCEDURE Liquidar :
DEFINE VAR PlazoW AS INTEGER.
DEFINE VAR TotPtW AS DECIMAL.
DEFINE VAR CuotaW AS DECIMAL.
DEFINE VAR TasaW AS DECIMAL.
DEFINE VAR TinteW AS DECIMAL.
DEFINE VAR W_Resultado AS INTEGER.
DEFINE VAR Wimp_Coop AS INTEGER.

DO WITH FRAME F_Solicitud:
    W_Liquidar = FALSE.

    IF INTEGER(SUBSTRING(Cmb_Sistemas:SCREEN-VALUE,1,5)) = 2 OR
       INTEGER(SUBSTRING(Nom_producto:SCREEN-VALUE,1,3)) = 108 AND
       INTEGER(SUBSTRING(Nom_producto:SCREEN-VALUE,1,3)) = 113 THEN DO:
        ASSIGN TotPtW = Creditos.Monto
               TasaW = DEC(Creditos.Tasa:SCREEN-VALUE)
               PlazoW = DECIMAL(Creditos.Plazo:SCREEN-VALUE)
               W_perded = INTEGER(SUBSTRING(Cmb_PerPago:SCREEN-VALUE,1,1))
               W_InterPlazo = (TotPtW * TasaW) / 100.

        RUN Calculo_Cuota.R (INPUT-OUTPUT TotPtw,
                             INPUT-OUTPUT PlazoW,
                             INPUT-OUTPUT CuotaW,
                             INPUT-OUTPUT TInteW,
                             INPUT-OUTPUT TasaW,
                             INPUT W_Razon,
                             INPUT W_Gracia,
                             INPUT W_PerDed,
                             INPUT 3,
                             INPUT INT(Creditos.FOR_Interes:SCREEN-VALUE),
                             INPUT INT(SUBSTR(Cmb_Sistemas:SCREEN-VALUE,1,5))).

        IF CuotaW LE 0 THEN DO:
            MESSAGE "El Valor de la cuota debe ser mayor a cero. Rectifique!" cuotaW totptw
                VIEW-AS ALERT-BOX ERROR.

            APPLY "ENTRY" TO Cmb_Sistemas IN FRAME F_Solicitud.
            RETURN ERROR.
        END.

        /* Redondeo a múltiplo de 100 */
        cuotaW = ROUND(cuotaW / 100,0) * 100.

        ASSIGN Creditos.Cuota:SCREEN-VALUE = STRING(CuotaW)
               Creditos.Cuota = CuotaW.
                             END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Llenar_InfoCliente wWin 
PROCEDURE Llenar_InfoCliente :
DEFINE VARIABLE gtexto AS CHARACTER FORMAT "x(60)".
   DEFINE VARIABLE TTOTAL  AS DECIMAL FORMAT ">>,>>>,>>>,>>9".
   DEFINE VARIABLE TDISPO  AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
   DO i = 1 TO 100:
      W_Ok = S_InfoCliente:DELETE(1) IN FRAME F_InfoCliente.
   END.
   IF AVAILABLE Clientes THEN DO:
      CASE Clientes.Tipo_Vinculo:
        WHEN 1 THEN DO:
          gTexto = "Tipo de Vinculo        : Asociado".
          RUN SInfo (INPUT gtexto).
        END.
        WHEN 2 THEN DO:
          gTexto = "Tipo de Vinculo        : Cliente No Asociado".
          RUN SInfo (INPUT gtexto).
        END.
        WHEN 3 THEN DO:
          gTexto = "Tipo de Vinculo        : Tercero".
          RUN SInfo (INPUT gtexto).
        END.
        WHEN 3 THEN DO:
          gTexto = "Tipo de Vinculo        : Proveedor".
          RUN SInfo (INPUT gtexto).
        END.
      END CASE.
      TTotal = Clientes.Ing_Otros + Clientes.Ing_Financieros + Clientes.Ing_Honorarios + Clientes.Salario.
      IF TTotal GT 0 THEN DO:
         gTexto = "              - INGRESOS -".
         RUN SInfo (INPUT gtexto).
      END.
      IF Clientes.Salario GT 0 THEN DO:
         gTexto = "Salario                : " + STRING(Clientes.Salario,">>,>>>,>>>,>>9").
         RUN SInfo (INPUT gtexto).
      END.
      IF Clientes.Ing_arriendos GT 0 THEN DO:
         gtexto = "Ingresos Financieros   : " + STRING(Clientes.Ing_Financieros,">>,>>>,>>>,>>9").
         RUN Sinfo (INPUT gtexto).
      END.
      IF Clientes.Ing_Honorarios GT 0 THEN DO:
         gtexto = "Ingresos por Honorarios: " + STRING(Clientes.Ing_Honorarios,">>,>>>,>>>,>>9").
         RUN SInfo (INPUT gtexto).
      END.
      IF Clientes.Ing_Otros GT 0 THEN DO:
         gtexto = "Otros Ingresos         : " + STRING(Clientes.Ing_Otros,">>,>>>,>>>,>>9").
         RUN SInfo (INPUT gtexto).
      END.
      IF TTotal GT 0 THEN DO:
         gtexto = "Total Ingresos---------: " + STRING(TTotal,">>,>>>,>>>,>>9").
         RUN SInfo (INPUT gtexto).
         gtexto = "".
         RUN SInfo (INPUT gtexto).
      END.
      TDispo = TDispo + TTotal.
      TTotal = 0.
      
      TTotal = Clientes.Gto_Obligacion + Clientes.Gto_Familiar + Clientes.Gto_Arriendo.
      IF TTotal GT 0 THEN DO:
         gTexto = "              - GASTOS -".
         RUN SInfo (INPUT gtexto).
      END.
      IF Clientes.Gto_Arriendo GT 0 THEN DO:
         gtexto = "Gastos Arriendo        : " + STRING(Clientes.Gto_Arriendo,">>,>>>,>>>,>>9").
         RUN SInfo (INPUT gtexto).
      END.
      IF Clientes.Gto_Familiar GT 0 THEN DO:
         gtexto = "Gastos Familiares      : " + STRING(Clientes.Gto_Familiar,">>,>>>,>>>,>>9").
         RUN SInfo (INPUT gtexto).
      END.
      IF Clientes.Gto_Obligacion GT 0 THEN DO:
         gtexto = "Obligaciones           : " + STRING(Clientes.Gto_Obligacion,">>,>>>,>>>,>>9").
         RUN Sinfo (INPUT gtexto).
      END.

      IF TTotal GT 0 THEN DO:
         gtexto = "Total Egresos----------: " + STRING(TTotal,">>,>>>,>>>,>>9").
         RUN SInfo (INPUT gtexto).
         gtexto = "".
         RUN SInfo (INPUT gtexto).
      END.
      TDispo = TDispo - TTotal.
      IF TDispo GT 0 THEN DO:
         gTexto = "Disponible (Ing - Egre): " + STRING(TDispo,"->>,>>>,>>>,>>9").
         RUN SInfo (INPUT gtexto).
         gtexto = "".
         RUN SInfo (INPUT gtexto).
      END.
      TTotal = 0.
      TDispo = 0.
      TTotal = Clientes.Act_Vehiculo + Clientes.Act_Inversion + Clientes.Act_Casa.
      IF TTotal GT 0 THEN DO:
         gTexto = "              - ACTIVOS -".
         RUN SInfo (INPUT gtexto).
      END.
      IF Clientes.Act_Casa GT 0 THEN DO:
         gTexto = "Valor en Propiedades   : " + STRING(Clientes.Act_casa,">>,>>>,>>>,>>9").
         RUN SInfo (INPUT gtexto).
      END.
      IF Clientes.Act_Inversion GT 0 THEN DO:
         gtexto = "Valor Inversiones      : " + STRING(Clientes.Act_inversion,">>,>>>,>>>,>>9").
         RUN SInfo (INPUT gtexto).
      END.
      IF Clientes.Act_Vehiculo GT 0 THEN DO:
         gtexto = "Valor Vehiculo         : " + STRING(Clientes.Act_Vehiculo,">>,>>>,>>>,>>9").
         RUN SInfo (INPUT gtexto).
      END.
      IF TTotal GT 0 THEN DO:
         gtexto = "".
         RUN SInfo (INPUT gtexto).
         gtexto = "Total Egresos----------: " + STRING(TTotal,">>,>>>,>>>,>>9").
         RUN SInfo (INPUT gtexto).
         gtexto = "".
         RUN SInfo (INPUT gtexto).
      END.
      IF Clientes.Sdo_Obligaciones GT 0 THEN DO:
         gTexto = "              - PASIVOS -".
         RUN SInfo (INPUT gtexto).
         gTexto = "Saldo Obligaciones     : " + STRING(Clientes.Sdo_Obligaciones,">>,>>>,>>>,>>9").
         RUN SInfo (INPUT gtexto).
      END.
      
   /*Clientes.Calificacion
   Clientes.Cod_Empresa
   Clientes.Cod_Cargo
   Clientes.Cod_Profesion
   Clientes.Cod_Segmento
   Clientes.Cod_Zona
   Clientes.Con_Sospechosas
   Clientes.Est_Civil
   Clientes.Edad
   Clientes.Estrato
   Clientes.Fec_Asociacion
   Clientes.Fec_IngEmpresa
   Clientes.Per_Acargo
   
   Clientes.Tipo_Vivienda
   Clientes.Tip_Contrato*/
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Llenar_InfoProducto wWin 
PROCEDURE Llenar_InfoProducto :
DEFINE VAR gtexto AS CHARACTER FORMAT "X(60)".

DO WITH FRAME F_InfoProducto:
    DO i = 1 TO 100 BY 1:
        W_Ok = S_InfoProducto:DELETE(1) IN FRAME F_InfoProducto.
    END.

    IF Pro_Creditos.Id_MontoMinimo THEN DO:
        gtexto = "Monto Minimo       :  "  + STRING(Pro_Creditos.Val_MontoMinimo,">>>,>>>,>>>,>>9").
        W_Ok = S_InfoProducto:ADD-LAST(gtexto).
    END.

    IF Pro_Creditos.Id_MontoMaximo THEN DO:
        gtexto = "Monto Máximo       :  "  + STRING(Pro_Creditos.Val_MontoMaximo,">>>,>>>,>>>,>>9").
        W_Ok = S_InfoProducto:ADD-LAST(gtexto).
    END.

    IF Pro_Creditos.Id_Plazo THEN DO:
        gtexto = "Plazo Mínimo       :  "  + STRING(Pro_Creditos.Pla_Minimo / 30,">>>,>>9").
        W_Ok = S_InfoProducto:ADD-LAST(gtexto).
        gtexto = "Plazo Máximo       :  "  + STRING(Pro_Creditos.Pla_Maximo / 30,">>>,>>9").
        W_Ok = S_InfoProducto:ADD-LAST(gtexto).
    END.
END.
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MostrarAportes wWin 
PROCEDURE MostrarAportes :
FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ Creditos.Cod_Credito NO-LOCK NO-ERROR.

  RUN Halla_CapitAporte.R (INPUT Pro_Creditos.Cod_Credito,Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud, 
                           DEC(Creditos.Monto:SCREEN-VALUE),W_VrCredACanc,
                           OUTPUT W_SdoApor, OUTPUT W_ReqPtmo,OUTPUT W_PromedDD,OUTPUT W_FaltApor).
    
  ASSIGN W_FaltApor:SCREEN-VALUE = STRING(W_FaltApor)
         W_ReqPtmo:SCREEN-VALUE  = STRING(W_ReqPtmo)
         W_PromedDD:SCREEN-VALUE = STRING(W_PromedDD)
         W_SdoApor:SCREEN-VALUE  = STRING(W_SdoApor).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Admisible wWin 
PROCEDURE Mostrar_Admisible :
DO WITH FRAME F_Admisible:
 ASSIGN Garantias.Tipo_Garantia:SCREEN-VALUE = STRING(Garantias.Tipo_Garantia)
        Garantias.Identificacion_Bien:SCREEN-VALUE = Garantias.Identificacion_Bien
        Garantias.Nom_Bien:SCREEN-VALUE = Garantias.Nom_Bien
        Garantias.Descripcion_Bien:SCREEN-VALUE = Garantias.Descripcion_Bien
        Garantias.Descripcion_Bien2:SCREEN-VALUE = Garantias.Descripcion_Bien2
        Garantias.Fec_Creacion:SCREEN-VALUE = STRING(Garantias.Fec_Creacion)
        Garantias.Fec_Retiro:SCREEN-VALUE  = STRING(Garantias.Fec_Retiro)
        Garantias.Nit_Aseguradora:SCREEN-VALUE = Garantias.Nit_Aseguradora
        Garantias.Nro_Seguro:SCREEN-VALUE  = STRING(Garantias.Nro_Seguro)
        Garantias.Fec_IniSeguro:SCREEN-VALUE = STRING(Garantias.Fec_IniSeguro)
        Garantias.Fec_FinSeguro:SCREEN-VALUE = STRING(Garantias.Fec_FinSeguro)
        Garantias.Val_Asegurado:SCREEN-VALUE = STRING(Garantias.Val_Asegurado).

 ASSIGN Garantias.Val_Bien:SCREEN-VALUE      = STRING(Garantias.Val_Bien)
        Garantias.Nom_Impuesto:SCREEN-VALUE  = Garantias.Nom_Impuesto
        Garantias.Fec_VctoImpuesto:SCREEN-VALUE = STRING(Garantias.Fec_VctoImpuesto)
        Garantias.Val_Impuesto:SCREEN-VALUE     = STRING(Garantias.Val_Impuesto)
        Garantias.Fec_UltAvaluo:SCREEN-VALUE    = STRING(Garantias.Fec_UltAvaluo)
        Garantias.Fec_ProxAvaluo:SCREEN-VALUE   = STRING(Garantias.Fec_ProxAvaluo)
        Garantias.Val_UltAvaluo:SCREEN-VALUE    = STRING(Garantias.Val_UltAvaluo)
        Garantias.Aprobada:SCREEN-VALUE         = STRING(Garantias.Aprobada)
        Garantias.Estado:SCREEN-VALUE           = STRING(Garantias.Estado)
        W_CredAval:SCREEN-VALUE   = "0"
        W_DispGaran:SCREEN-VALUE  = "0"
        W_CredAval                = 0
        W_DispGaran               = 0.

 RUN Halla_DispGaran.

 FIND Clientes WHERE Clientes.Nit EQ Garantias.Nit_Aseguradora NO-LOCK NO-ERROR.
 IF AVAILABLE Clientes THEN 
    ASSIGN Nom_Aseguradora:SCREEN-VALUE = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
 ELSE
    ASSIGN Nom_Aseguradora:SCREEN-VALUE = "".
 FIND Usuarios WHERE Usuarios.Usuario EQ Garantias.Usuario NO-LOCK NO-ERROR.
 IF AVAILABLE Usuario THEN
    ASSIGN Nom_UsuGarantia:SCREEN-VALUE = Usuarios.Usuario + " - " + Usuarios.Nombre.
 ELSE
    ASSIGN Nom_UsuGarantia:SCREEN-VALUE = "".
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Credito wWin 
PROCEDURE Mostrar_Credito :
DEFINE VAR W_RowIdCr AS ROWID.
DEFINE VAR J AS INTEGER FORMAT "9".
DEFINE BUFFER BCreditos FOR Creditos.
DEFINE VAR flagCancelaCreditos AS LOGICAL.

FIND FIRST Pro_Creditos WHERE Pro_Creditos.Tip_Credito = Creditos.Tip_Credito
                          AND Pro_Creditos.Cod_Credito = Creditos.Cod_Credito NO-LOCK NO-ERROR.
IF AVAILABLE Pro_Creditos THEN DO:
    DO WITH FRAME F_Solicitud:
        CASE Pro_Creditos.Tip_Credito:
            WHEN 1 THEN
                ASSIGN W_Tipo_Credito:SCREEN-VALUE = " Consumo"
                       W_tippdt = 1.

            WHEN 2 THEN
                ASSIGN W_Tipo_Credito:SCREEN-VALUE = " Comercial"
                       W_Tippdt = 2.

            WHEN 3 THEN
                ASSIGN W_Tipo_Credito:SCREEN-VALUE = " Hipotecario"
                       W_Tippdt = 3.

            WHEN 4 THEN
                ASSIGN W_Tipo_Credito:SCREEN-VALUE = " Microcredito"
                       W_Tippdt = 4.
        END CASE.

        FIND FIRST Clientes WHERE Clientes.Nit = Creditos.Nit NO-LOCK NO-ERROR.
        IF AVAILABLE Clientes THEN
            NomNit:SCREEN-VALUE = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.

        RUN edad IN w_manija (INPUT clientes.fec_nacimiento,
                              OUTPUT Wk_edad).

        FIND FIRST Agencias WHERE Agencias.Agencia = Solicitud.Agencia NO-LOCK NO-ERROR.
        IF AVAILABLE Agencias THEN
            Cmb_Agencias:SCREEN-VALUE = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.

        FIND FIRST Relaciones WHERE Relaciones.Nit_Relacion = Creditos.Nit
                                AND Relaciones.Cod_Relacion = 14
                                AND Relaciones.Estado = 1 NO-LOCK NO-ERROR.

        ASSIGN Texto1 = ""
               Texto2 = ""
               W_Tasa = 0.

        IF AVAILABLE Relaciones THEN
            ASSIGN Texto1 = "Solicitud debe ser aprobada por una instancia superior   "
                   Texto2 = "Solicitante es referido de un Directivo de la Cooperativa".

        DISPLAY Texto1
                Texto2
            WITH FRAME F_Solicitud.

        RUN edad IN w_manija (INPUT clientes.fec_ingreso,
                              OUTPUT w_antiguedad).

        FIND FIRST Varios WHERE Varios.Tipo = 39
                            AND Varios.Codigo >= W_Antiguedad NO-LOCK NO-ERROR.
        IF AVAILABLE Varios THEN
            W_Tasa = Varios.Val_Inicial.

        ASSIGN Creditos.Num_Credito:SCREEN-VALUE = STRING(Creditos.Num_Credito)
               Creditos.Num_Solicitud:SCREEN-VALUE = STRING(Creditos.Num_Solicitud)
               Creditos.Nit:SCREEN-VALUE = Creditos.Nit
               Creditos.Fec_Aprobacion:SCREEN-VALUE = STRING(Creditos.Fec_Aprobacion)
               Creditos.FOR_Interes:SCREEN-VALUE = STRING(Creditos.FOR_Interes)
               Creditos.Monto:SCREEN-VALUE = STRING(Creditos.Monto)
               Creditos.Cuota:SCREEN-VALUE = STRING(Creditos.Cuota)
               Creditos.Deducible:SCREEN-VALUE = STRING(Creditos.Deducible)
               Creditos.Tasa:SCREEN-VALUE = STRING(Creditos.Tasa)
               Creditos.Plazo:SCREEN-VALUE = STRING(Creditos.Plazo)
               Creditos.Id_Adicionales:SCREEN-VALUE = STRING(Creditos.Id_Adicionales)
               Cmb_Sistemas:SCREEN-VALUE = Cmb_Sistemas:ENTRY(Creditos.Sistema)
               Nom_Producto:SCREEN-VALUE = STRING(Pro_Creditos.Cod_Credito,"999") + " - " + Pro_Creditos.Nom_Producto.

        CASE Creditos.Per_Pago:
            WHEN 0 THEN
                ASSIGN Cmb_PerPago:SCREEN-VALUE = "0 - Diario"
                       Dias = Creditos.Plazo * 1.

            WHEN 1 THEN
                ASSIGN Cmb_PerPago:SCREEN-VALUE = "1 - Semanal"
                       Dias = Creditos.Plazo * 7.

            WHEN 2 THEN
                ASSIGN Cmb_PerPago:SCREEN-VALUE = "2 - Decadal"
                       Dias = Creditos.Plazo * 10.

            WHEN 3 THEN
                ASSIGN Cmb_PerPago:SCREEN-VALUE = "3 - Quincenal"
                       Dias = Creditos.Plazo * 15.

            WHEN 4 THEN
                ASSIGN Cmb_PerPago:SCREEN-VALUE = "4 - Mensual"
                       Dias = Creditos.Plazo * 30.

            WHEN 5 THEN
                ASSIGN Cmb_PerPago:SCREEN-VALUE = "5 - Bimensual"
                       Dias = Creditos.Plazo * 60.

            WHEN 6 THEN
                ASSIGN Cmb_PerPago:SCREEN-VALUE = "6 - Trimestral"
                       Dias = Creditos.Plazo * 90.

            WHEN 7 THEN
                ASSIGN Cmb_PerPago:SCREEN-VALUE = "7 - Cuatrimensual"
                       Dias = Creditos.Plazo * 120.

            WHEN 8 THEN
                ASSIGN Cmb_PerPago:SCREEN-VALUE = "8 - Semestral"
                       Dias = Creditos.Plazo * 180.

            WHEN 9 THEN
                ASSIGN Cmb_PerPago:SCREEN-VALUE = "9 - Anual"
                       Dias = Creditos.Plazo * 360.
        END CASE.

        RUN Hallar_Tasa_Efectiva.
        RUN MostrarAportes.

        CASE Creditos.Desembolso:
            WHEN 1 THEN
                ASSIGN W_Desembolso:SCREEN-VALUE = "Efectivo"
                       W_DesAho = "Desembolso Efectivo Cred.Num: " + STRING(Creditos.Num_Credito).

            WHEN 2 THEN
                ASSIGN W_Desembolso:SCREEN-VALUE = "Cheque"
                       W_DesAho = "Desembolso en Cheque Cred.Num: " + STRING(Creditos.Num_Credito).

            WHEN 9 THEN
                ASSIGN W_Desembolso:SCREEN-VALUE = "Transferencia"
                       W_DesAho = "Desembolso por Transferencia - Cred.Num: " + STRING(Creditos.Num_Credito).

            WHEN 3 THEN DO:
                FIND FIRST Agencias WHERE Agencias.Agencia = Creditos.Age_Desembolso NO-LOCK NO-ERROR.
                IF AVAILABLE Agencias THEN
                    ASSIGN W_Desembolso = Agencias.Nombre.

                FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro = Solicitud.Cod_Desembolso NO-LOCK NO-ERROR.
                IF AVAILABLE Pro_Ahorros THEN
                    W_Desembolso = W_Desembolso + " / Producto: " + Pro_Ahorros.Nom_Producto.

                ASSIGN W_Desembolso = W_Desembolso + " / Cuenta: " + Creditos.Cue_Desembolso
                       W_DesAho = "Desembolso a Cta: Age." + STRING(Creditos.Age_Desembolso,"999") + " Cta:" + STRING(Creditos.Cue_Desembolso).

                W_Desembolso:SCREEN-VALUE = W_Desembolso.
            END.

            WHEN 4 THEN
                ASSIGN W_Desembolso:SCREEN-VALUE = "Orden a Terceros"
                       W_DesAho = "Desembolso Ord.Terceros. Cred.Num: " + STRING(Creditos.Num_Credito).
        END CASE.
        
        CASE Creditos.FOR_Pago:
            WHEN 1 THEN W_ForPago:SCREEN-VALUE = "Caja".
            WHEN 2 THEN W_ForPago:SCREEN-VALUE = "Nomina".
            
            WHEN 3 THEN DO:
                FIND FIRST Agencias WHERE Agencias.Agencia = Solicitud.Age_DebAutomatico NO-LOCK NO-ERROR.
                IF AVAILABLE Agencias THEN
                    W_ForPago = "Debito Automatico" + " - " + Agencias.Nombre.

                FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro = Creditos.Cod_DebAutomatico NO-LOCK NO-ERROR.
                IF AVAILABLE Pro_Ahorros THEN
                    W_ForPago = W_ForPago + " / Producto: " + Pro_Ahorros.Nom_Producto.

                W_ForPago = W_ForPago + " / Cuenta: " + Creditos.Cue_DebAutomatico.
                W_ForPago:SCREEN-VALUE = W_ForPago.
            END.

            WHEN 4 THEN W_ForPago:SCREEN-VALUE = "Nomina Crecediario".
            WHEN 5 THEN W_ForPago:SCREEN-VALUE = "Prima".
        END CASE.

        APPLY "leave" TO Creditos.Monto IN FRAME F_Solicitud.
        APPLY "leave" TO Creditos.Plazo IN FRAME F_Solicitud.
        APPLY "leave" TO Creditos.Nit.

        RUN Hallar_Tasa_Nominal.
        RUN Hallar_Tasa_Periodo.

        APPLY "value-changed" TO Cmb_Agencias.
        
        ENABLE {&List-1}.

        ASSIGN Creditos.Pagare:SCREEN-VALUE IN FRAME F_Formalizar = STRING(Creditos.Num_Credito)
               Creditos.Estado:SCREEN-VALUE IN FRAME F_Ultima = Creditos.Estado:SCREEN-VALUE IN FRAME F_Formalizar
               Creditos.Pagare:SCREEN-VALUE = STRING(Creditos.Num_Credito)
               Creditos.Desembolso:SCREEN-VALUE = STRING(Creditos.Desembolso)
               W_VrCredACanc:SCREEN-VALUE = "0"
               W_RowIdCr = ROWID(Creditos)
               W_VrCredACanc = 0
               W_CancCap = 0.

        FOR EACH solicitudes_pagoCreditos WHERE solicitudes_pagoCreditos.cliente_id = solicitud.nit
                                            AND solicitudes_pagoCreditos.num_solicitud = solicitud.num_solicitud NO-LOCK:
            FIND FIRST Creditos WHERE Creditos.Nit = solicitudes_pagoCreditos.cliente_id
                                  AND Creditos.Num_Credito = solicitudes_pagoCreditos.num_credito
                                  AND Creditos.Estado = 2 NO-LOCK NO-ERROR.
            IF AVAILABLE creditos THEN DO:
                IF solicitudes_pagoCreditos.pagoTotal = TRUE THEN DO:
                    W_VrCredACanc = W_VrCredACanc + Creditos.Sdo_Capital + Creditos.Int_Corrientes + Creditos.Int_DifCobro + Creditos.Int_MoraDifCob + Creditos.Int_MorCobrar + Creditos.Costas + Creditos.Polizas + Creditos.Honorarios - Creditos.Int_Anticipado.
                    flagCancelaCreditos = TRUE.
                END.
                ELSE DO:
                    IF solicitudes_pagoCreditos.valorAbono < Creditos.Sdo_Capital + Creditos.Int_Corrientes + Creditos.Int_DifCobro + Creditos.Int_MoraDifCob + Creditos.Int_MorCobrar + Creditos.Costas + Creditos.Polizas + Creditos.Honorarios - Creditos.Int_Anticipado THEN
                        W_VrCredACanc = W_VrCredACanc + solicitudes_pagoCreditos.valorAbono.
                    ELSE
                        W_VrCredACanc = W_VrCredACanc + Creditos.Sdo_Capital + Creditos.Int_Corrientes + Creditos.Int_DifCobro + Creditos.Int_MoraDifCob + Creditos.Int_MorCobrar + Creditos.Costas + Creditos.Polizas + Creditos.Honorarios - Creditos.Int_Anticipado.
                END.

                W_VrCredACanc:SCREEN-VALUE = STRING(W_VrCredACanc).
            END.
            ELSE DO:
                MESSAGE "Credito a cancelar/abonar con este desembolso no hallado. El neto a desembolsar se incrementa."
                    VIEW-AS ALERT-BOX TITLE "SOLO INFORMATIVO".

                FIND CURRENT Solicitud NO-LOCK NO-ERROR.
            END.
        END.

        ASSIGN FRAME F_Solicitud Ded_Ahorros.

        FIND FIRST Creditos WHERE ROWID(Creditos) = W_RowIdCr NO-ERROR.

        ASSIGN W_VrADesemb = Creditos.Monto - (WDed + W_VrCredACanc + w_faltapor)
               W_VrADesemb:SCREEN-VALUE = STRING(W_VrADesemb).

        IF W_VrADesemb < 0 THEN DO:
            MESSAGE "El valor a desembolsar es negativo o cero(0). Debe revisar los valores a cancelar."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            IF flagCancelacreditos = TRUE THEN DO:
                MESSAGE "El valor del crédito no alcanza a cubrir las obligaciones a cancelar." SKIP
                        "Revise  esta  novedad  con  el  área  de  Cartera para que se haga el" SKIP
                        "respectivo análisis. No se permite la operación."
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.

                btn_ProInstancia:SENSITIVE IN FRAME F_Creditos = FALSE.
            END.
        END.
        ELSE
            btn_ProInstancia:SENSITIVE IN FRAME F_Creditos = TRUE.
    END.

    IF AVAILABLE creditos THEN DO:
        /*W-fecEntLib = creditos.fec_paganti.*/
        
        W-fecEntLib = solicitud.fec_primerPago.
        DISPLAY w-fecentlib WITH FRAME f_solicitud.
        W-fecEntLib:SENSITIVE = FALSE.
    END.
END.
ELSE
    MESSAGE "Falta el Pro-Creditos, Ag-Tip-Pdcto:" Creditos.Agencia
            Creditos.Tip_Credito Creditos.Cod_Credito
        VIEW-AS ALERT-BOX ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mov_AhorroyCredito wWin 
PROCEDURE Mov_AhorroyCredito :
DEFINE VAR Tipo LIKE Operacion.Ctrl_EfeChe INIT 1.
DEFINE VAR W_RowidOp AS ROWID.

IF Creditos.Desembolso EQ 2 OR Creditos.Desembolso EQ 5 OR Creditos.Desembolso EQ 7 OR creditos.desembolso = 9 THEN
    Tipo = 2.

FIND FIRST Operacion WHERE STRING(Operacion.Cod_Operacion,"999999999") BEGINS "020102"
                       AND Operacion.Ctrl_EfeChe EQ Tipo
                       AND Operacion.Estado EQ 1
                       AND Operacion.Id_SYA EQ NO NO-LOCK NO-ERROR.
IF AVAILABLE Operacion THEN DO:
    IF (Creditos.Desembolso EQ 3 OR Creditos.Desembolso EQ 6 OR Creditos.Desembolso EQ 7) AND W_VrAlAhorro > 0 THEN DO:
        W_RowidOp = ROWID(Operacion).

        RUN Busca_Operacion_Ahorros NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            RETURN ERROR.
        
        FIND FIRST Operacion WHERE ROWID(Operacion) EQ W_RowidOp NO-LOCK NO-ERROR.
    END.

    IF W_IntAntic GT 0 THEN DO:
        RUN Grabar_MovCreditos(INPUT 010301001,
                               INPUT Creditos.Cod_Credito,
                               INPUT Creditos.Num_Credito,
                               INPUT W_NumCbt,
                               INPUT Creditos.Agencia,
                               INPUT W_Agencia,
                               INPUT Creditos.Agencia,
                               INPUT W_Usuario,
                               INPUT 0,
                               INPUT DECIMAL(Creditos.Sdo_Capital:SCREEN-VALUE IN FRAME F_Solicitud),
                               INPUT Creditos.Pagare:SCREEN-VALUE IN FRAME F_Ultima) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            RETURN ERROR.

        ASSIGN Mov_Creditos.Descrip = "Int-Anticip.en el Desembolso"
               Mov_Creditos.Val_Cheque = 0
               Mov_Creditos.Val_Efectivo = W_IntAntic
               Mov_Creditos.Sdo_Capital = DECIMAL(Creditos.Sdo_Capital:SCREEN-VALUE IN FRAME F_Solicitud).
    END.

    RUN Grabar_MovCreditos(INPUT Operacion.Cod_Operacion,
                           INPUT Creditos.Cod_Credito,
                           INPUT Creditos.Num_Credito,
                           INPUT W_NumCbt,
                           INPUT Creditos.Agencia,
                           INPUT W_Agencia,
                           INPUT Creditos.Agencia,
                           INPUT W_Usuario,
                           INPUT 0,
                           INPUT DECIMAL(Creditos.Sdo_Capital:SCREEN-VALUE IN FRAME F_Solicitud),
                           INPUT Creditos.Pagare:SCREEN-VALUE IN FRAME F_Ultima) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN ERROR.
END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Negar_Solicitud wWin 
PROCEDURE Negar_Solicitud :
/*-------------------------*/
 FOR EACH TPartidas: DELETE TPartidas. END.

 TransAnular:
 DO TRANS ON ERROR UNDO TransAnular:
    CREATE Hoja_Vida.
    ASSIGN Hoja_Vida.Tipo    = 9 
        Hoja_Vida.Codigo     = 1  
        Hoja_Vida.Instancia  = INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5))
        Hoja_Vida.DoctoRefer = INTEGER(Creditos.Num_Credito:SCREEN-VALUE IN FRAME F_Solicitud)
        Hoja_Vida.Nit        = Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud
        Hoja_Vida.Usuario    = W_Usuario
        Hoja_Vida.Fec_Grabacion   = TODAY
        Hoja_Vida.Hora_Grabacion  = TIME
        Hoja_Vida.Observacion     = "Credito Desaprobado por el Usuario: " + W_Usuario
        Hoja_Vida.Asunto_Cumplido = YES
        W_Anular                  = TRUE.

    RUN CortoLargoCreditos NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
       RETURN ERROR.

    W_Anular = FALSE.

    FIND Comprobantes WHERE 
             Comprobantes.Agencia     EQ Creditos.Agencia AND
             Comprobantes.Comprobante EQ 4 NO-ERROR.
    IF NOT AVAILABLE Comprobantes THEN DO:
       MESSAGE "No se ha encontrado el comprobante para la contabilización" SKIP
               "de la reversa del crédito. Configurado en corto y largo" SKIP 
               "Rectifique con el Administrador!"
                   VIEW-AS ALERT-BOX ERROR. 
       RETURN ERROR.
    END.
      
    ASSIGN W_Cbte    = Comprobantes.Comprobante
           W_NumCbt  = Comprobantes.Secuencia + 1
           Comprobantes.Secuencia = Comprobantes.Secuencia + 1.

    FIND CURRENT Comprobantes NO-LOCK NO-ERROR.

    RUN Contabilizar_Partidas NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
       RETURN ERROR.

    ASSIGN Creditos.Estado = 4.

    /*Abril 15/05 GAER Inactiva relaciones y garantias*/
    FOR EACH Relaciones WHERE                                                    
               Relaciones.Nit            EQ Creditos.Nit:                        
        IF     Relaciones.Cuenta         EQ TRIM(STRING(Creditos.Num_Solicitud)) AND          
               Relaciones.Clase_Producto EQ 2                      AND           
               Relaciones.Cod_Producto   EQ Creditos.Cod_Credito   AND           
               Relaciones.Cod_relacion   EQ 11 THEN                              
           ASSIGN Relaciones.Estado = 2.                                            
    END.                                                                         
                                                                                 
    FOR EACH Garantias WHERE Garantias.Agencia       EQ Creditos.Agencia        
                         AND Garantias.Cod_Credito   EQ Creditos.Cod_Credito    
                         AND Garantias.Tip_Credito   EQ Creditos.Tip_Credito    
                         AND Garantias.Num_Solicitud EQ Creditos.Num_Solicitud :
        IF Garantias.Nit EQ Creditos.Nit THEN                                   
           ASSIGN Garantias.Estado = 2.                                          
    END.                                                                         
    /*Fin Abril 15/05*/                                                          

    FOR EACH Mov_Instancias WHERE 
       Mov_Instancias.Instancia     EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) AND
       Mov_Instancias.Nit           EQ Creditos.Nit AND
       Mov_Instancias.Cuenta        EQ TRIM(STRING(Creditos.Num_Credito)) AND
       Mov_Instancias.Num_Solicitud EQ Creditos.Num_Solicitud:
       DELETE Mov_Instancias.       
    END.
 END.  /*Fin tx*/

 ENABLE ALL WITH FRAME F_Creditos.                   
 DISABLE NomUsuario WITH FRAME F_Creditos.           
 ENABLE ALL WITH FRAME F_Consulta.                   
 APPLY "choose" TO Btn_Consulta IN FRAME F_Creditos.      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PartidaSya wWin 
PROCEDURE PartidaSya :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFI INPUT PARAMETER P_Vlr LIKE Mov_Contable.Db.

  CREATE TPartidas.                                                       
  ASSIGN Tpartidas.Tage = W_Agencia                                     
         TPartidas.TDoc = Creditos.Pagare         
         TPartidas.TTip = 2                                                        
         TPartidas.TOpe = 0                                                        
         TPartidas.TDeb = P_Vlr
         TPartidas.TCre = 0
         Tpartidas.TCta = W_CtaSyA
         Tpartidas.TCed = STRING(Creditos.Agencia,"999")
         TPartidas.TDsc = "Desemb.Otra Agencia".                                  
  MESSAGE 22 Tpartidas.TCta
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
  CREATE TPartidas.                                                       
  ASSIGN Tpartidas.Tage = Creditos.Agencia                                     
         TPartidas.TDoc = Creditos.Pagare         
         TPartidas.TTip = 2                                                        
         TPartidas.TOpe = 0                                                        
         TPartidas.TDeb = 0
         TPartidas.TCre = P_Vlr
         Tpartidas.TCta = W_CtaSyA
         Tpartidas.TCed = STRING(W_Agencia,"999")
         TPartidas.TDsc = "Desemb.Otra Agencia".     
  MESSAGE 23 Tpartidas.TCta
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Partidas_GarAdmisible wWin 
PROCEDURE Partidas_GarAdmisible :
DEFINE VAR TotGarantias AS DECIMAL.

FOR EACH Garantias WHERE Garantias.Agencia = Creditos.Agencia
                     AND Garantias.Nit = Creditos.Nit
                     AND Garantias.Tip_Credito = Creditos.Tip_Credito
                     AND Garantias.Tipo_Garantia <= 3
                     AND Garantias.Cod_Credito = Creditos.Cod_Credito
                     AND Garantias.Num_Credito = Creditos.Num_Credito
                     AND Garantias.Num_Solicitud = Creditos.Num_Solicitud
                     AND Garantias.Estado = 1
                     AND Garantias.Aprobada BREAK BY Garantias.Num_Credito:
    IF FIRST-OF(Garantias.Num_Credito) THEN DO:
        IF CortoLargo.Cta_VigGarAd = "" OR CortoLargo.Cta_ContrapartidaGar = "" THEN DO:
            MESSAGE "Las cuentas para la contabilización de las" SKIP
                    "garantías admisibles, no se encuentran" SKIP
                    "configuradas en Corto y Largo" SKIP(1)
                    "Se cancela la operación de desembolso." SKIP
                    "Comuniquese con el Administrador"
                VIEW-AS ALERT-BOX ERROR.

            RETURN ERROR.
        END.
    END.

    /* oakley */

  ASSIGN Garantias.Contabilizada = TRUE
         W_RowidGar              = ROWID(Garantias)
         W_IdGar                 = Garantias.Identificacion_Bien
         W_NoContab              = FALSE.

  RUN ContabSiNo.   /*Procedimiento al final de este mismo procedimiento*/
  
  IF W_NoContab THEN 
     ASSIGN TotGarantias = TotGarantias + Garantias.Val_Bien.   
END.

IF TotGarantias GT 0 THEN DO:
  CREATE TPartidas.
  ASSIGN Tpartidas.Tage = Creditos.Agencia
         Tpartidas.TCta = CortoLargo.Cta_ContrapartidaGar
         TPartidas.TDsc = "Garantía Admisible"
         TPartidas.TDoc = Creditos.Pagare:SCREEN-VALUE IN FRAME F_Ultima
         TPartidas.TTip = 2
         TPartidas.TOpe = 0
         TPartidas.TCre = TotGarantias.
  MESSAGE 24 Tpartidas.TCta
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
  CREATE TPartidas.
  ASSIGN Tpartidas.Tage = Creditos.Agencia
         Tpartidas.TCta = CortoLargo.Cta_VigGarAd
         TPartidas.TDsc = "Garantía Admisible"
         TPartidas.TDoc = Creditos.Pagare:SCREEN-VALUE IN FRAME F_Ultima
         TPartidas.TTip = 2
         TPartidas.TOpe = 0
         TPartidas.TDeb = TotGarantias.     
  MESSAGE 25 Tpartidas.TCta
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

END PROCEDURE.

PROCEDURE ContabSiNo:          /*valida si la garantìa ya està o no Contabilizada*/
  FIND FIRST Garantias WHERE 
         Garantias.Nit                 EQ Creditos.Nit           AND
         Garantias.Identificacion_Bien EQ W_IdGar                AND
         Garantias.Estado              EQ 1                      AND
         Garantias.Aprobada                                      AND
         Garantias.Contabilizada                                 AND  /*Si la halla està contable*/
         ROWID(garantias)              NE W_RowidGar  NO-LOCK NO-ERROR.
  IF NOT AVAIL(Garantias) THEN
     W_NoContab = TRUE.   /*Solo retorna true si NO està contabilizada*/

  FIND Garantias WHERE ROWID(garantias) EQ W_RowidGar NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Partidas_Orden wWin 
PROCEDURE Partidas_Orden :
DEFINE VAR numCheque AS INTEGER.

CREATE Mov_Contable.
ASSIGN Mov_Contable.Agencia = INTEGER(SUBSTRING(Cmb_Agencias:SCREEN-VALUE IN FRAME F_Solicitud,1,3))
       Mov_Contable.Comprobante = Comprobantes.Comprobante
       Mov_Contable.Cuenta = CortoLargo.Cta_ContingenteDB
       Mov_Contable.Fec_Contable = W_Fecha
       Mov_Contable.Cen_Costos = W_CenCosGral
       Mov_Contable.CR = DECIMAL(Creditos.Monto)
       Mov_Contable.Comentario = "Desembolso de Crédito"
       Mov_Contable.Usuario = W_Usuario
       Mov_Contable.Nit = Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud
       Mov_Contable.Doc_Referencia = Creditos.Num_Credito:SCREEN-VALUE IN FRAME F_Solicitud
       Mov_Contable.Num_Documento = Comprobantes.Secuencia
       Mov_Contable.Fec_Grabacion = TODAY
       Mov_Contable.Hora = TIME NO-ERROR.

IF ERROR-STATUS:ERROR THEN DO:
    MESSAGE "Error al contabilizar primera partida" SKIP
            "consulte con el administrador"
        VIEW-AS ALERT-BOX ERROR.

    RETURN ERROR.
END.

numCheque = INTEGER(num_cheque:SCREEN-VALUE IN FRAME F_Bancos) NO-ERROR.

IF numCheque > 0 OR num_cheque:SCREEN-VALUE = "TRANSF" THEN DO:
    mov_contable.doc_referencia = num_cheque:SCREEN-VALUE.
END.

ERROR-STATUS:ERROR = FALSE.

CREATE Mov_Contable.
ASSIGN Mov_Contable.Agencia = INTEGER(SUBSTRING(Cmb_Agencias:SCREEN-VALUE IN FRAME F_Solicitud,1,3))
       Mov_Contable.Comprobante = Comprobantes.Comprobante
       Mov_Contable.Cuenta = CortoLargo.Cta_ContingenteCR
       Mov_Contable.Fec_Contable = W_Fecha
       Mov_Contable.Cen_Costos = W_CenCosGral
       Mov_Contable.DB = DECIMAL(Creditos.Monto)
       Mov_Contable.Comentario = "Desembolso de Crédito"
       Mov_Contable.Nit = Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud
       Mov_Contable.Doc_Referencia = Creditos.Num_Credito:SCREEN-VALUE IN FRAME F_Solicitud
       Mov_Contable.Usuario = W_Usuario
       Mov_Contable.Num_Documento = Comprobantes.Secuencia
       Mov_Contable.Fec_Grabacion = TODAY
       Mov_Contable.Hora = TIME NO-ERROR.

IF numCheque > 0 OR num_cheque:SCREEN-VALUE = "TRANSF" THEN DO:
    mov_contable.doc_referencia = num_cheque:SCREEN-VALUE.
END.

IF ERROR-STATUS:ERROR THEN DO:
    MESSAGE "Error al contabilizar segunda partida" SKIP
            "consulte con el administrador"
        VIEW-AS ALERT-BOX ERROR.

    RETURN ERROR.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCndciones wWin 
PROCEDURE pCndciones :
DEFINE VAR Linea AS CHARACTER FORMAT "X(1125)" NO-UNDO.
DEFINE VAR T_Plazo AS CHARACTER FORMAT "X(30)" NO-UNDO.
DEFINE VAR T_Dedu AS CHARACTER FORMAT "X(30)" NO-UNDO.
DEFINE VAR FPago AS CHARACTER FORMAT "X(30)" NO-UNDO.
DEFINE VAR FCon AS CHARACTER FORMAT "X(30)" NO-UNDO.
DEFINE VAR FFec AS CHARACTER FORMAT "X(30)" NO-UNDO.
DEFI VAR TDisp LIKE Ahorros.Sdo_Disponible NO-UNDO.
DEFI VAR W_RowIdCte AS ROWID NO-UNDO.
DEFI VAR W_RowIdCodeu AS ROWID NO-UNDO.
DEF VAR i AS INTEGER NO-UNDO.
DEF VAR tDdcbles AS DECIMAL NO-UNDO.
DEF VAR cl AS CHAR NO-UNDO.

DO WITH FRAME F_Solicitud:
    T_Plazo = string(Solicitud.Plazo / 30)  + " (" + SUBSTRING(Cmb_PerPago:SCREEN-VALUE,5,15) + ")".

    CASE Solicitud.Id_Adicionales:
        WHEN 1 THEN T_Dedu = "Financiados".
        WHEN 2 THEN T_Dedu = "Descontados".
        WHEN 3 THEN T_Dedu = "Pagados por Caja".
    END CASE.

    Linea = "=========================================CONDICIONES DE OTORGAMIENTO DE CREDITO========================================".

    RUN TmpL (INPUT 1, INPUT Linea).

    cl = w_nom_entidad.
    linea = cl + CHR(10).
    cl = "USUARIO".
    OVERLAY(cl,29,1) = ":".
    OVERLAY(cl,31) = w_usuario + " - " + fNmbreUsuario(w_usuario).
    OVERLAY(cl,83) = "FECHA:" + STRING(TODAY) + " - " + STRING(TIME,"hh:mm:ss").
    linea = LINEa + cl + CHR(10) + CHR(10).
    cl = "Cliente Solicitante".
    OVERLAY(cl,29,1) = ":".
    OVERLAY(cl,31) = creditos.nit:SCREEN-VALUE IN FRAME f_solicitud + " - " + nomnit:SCREEN-VALUE IN FRAME f_solicitud + "(" + fEdad(Clientes.Fec_Nacimiento) + " Años)".
    linea = LINEa + cl.

    RUN TmpL (INPUT 1, INPUT Linea).
    
    Linea = "=============================================DATOS GENERALES DE LA SOLICITUD===========================================".

    RUN TmpL (INPUT 1, INPUT Linea).

    Linea = "".
    Linea = "Agencia de Radicación       : " + Cmb_Agencias:SCREEN-VALUE IN FRAME F_Solicitud.
    OVERLAY(linea,62)   = " Número de Solicitud".
    OVERLAY(linea,91)   = ":".
    OVERLAY(linea,93)  = STRING(Solicitud.Num_Solicitud).
    
    RUN TmpL (INPUT 2, INPUT Linea).
    
    linea = "".
    Linea = "Fecha de Radicación         : " + STRING(Solicitud.Fec_Solicitud).
    OVERLAY(linea,62) = " Producto de Crédito".
    OVERLAY(linea,91)   = ":".
    OVERLAY(linea,93) = STRING(Nom_Producto:SCREEN-VALUE).

    RUN TmpL (INPUT 3, INPUT Linea).

    linea = "".
    Linea = "Tipo de Producto            : " + STRING(TRIM(W_Tipo_Credito:SCREEN-VALUE)).
    OVERLAY(linea,62) = " Instancia Actual".
    OVERLAY(linea,91) = ":".
    OVERLAY(linea,93) = STRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos).
    
    RUN TmpL (INPUT 4, INPUT Linea).

    linea = "".
    OVERLAY(linea,1) = " Forma de Pago de la Cuota".
    OVERLAY(linea,29,1) = ":".
    OVERLAY(linea,31) = STRING(W_ForPago:SCREEN-VALUE).
    
    RUN TmpL (INPUT 5, INPUT Linea).

    Linea = "=============================================DETALLE DE VALORES DE SOLICITUD===========================================".

    RUN TmpL (INPUT 7, INPUT Linea).

    linea = "".
    Linea = "Monto a Prestar             : " + STRING(Solicitud.Monto,"$>>>,>>>,>>9").
    OVERLAY(linea,62) = " Tasa Efectiva Anual".
    OVERLAY(linea,91) = ":".
    OVERLAY(linea,93) = STRING(Solicitud.Tasa).
    
    RUN TmpL (INPUT 8, INPUT Linea).

    linea = "".
    Linea = "Plazo                       : " + STRING(T_Plazo).
    OVERLAY(linea,62) = " Tasa Nomina Anual".
    OVERLAY(linea,91) = ":".
    OVERLAY(linea,93) = STRING(W_TasaNominal:SCREEN-VALUE,"X(30)").
    
    RUN TmpL (INPUT 9, INPUT Linea).         

    linea = "".
    Linea = "Cuota del Período           : " + STRING(Solicitud.Cuota,"$>>>,>>>,>>9").
    OVERLAY(linea,62) = " Tasa del Período".
    OVERLAY(linea,91) = ":".
    OVERLAY(linea,93) = STRING(W_TasaPeriodo:SCREEN-VALUE,"X(30)").
    
    RUN TmpL (INPUT 10, INPUT Linea).

    LINEa = "".
    linea = "Pago de Valor a Deducir     : " + STRING(T_Dedu,"X(30)").
    OVERLAY(linea,62) = " Destino".
    OVERLAY(linea,91) = ":".
    overlay(linea,93) = STRING(IF Solicitud.Destino = ? THEN "" ELSE Solicitud.Destino).

    RUN TmpL (INPUT 13, INPUT Linea).
    
    /* DEDUCIBLES */
    Linea = "=======================================================DEDUCIBLES=======================================================".
    
    RUN TmpL (INPUT 14, INPUT Linea).

    i = 0.
    linea = "".
    tDdcbles = 0.

    FOR EACH tpartidas:
        i = i + 1.
        OVERLAY(linea,i * 40 - 39,39)   = caps(entry(1,tpartidas.tdsc + ":",":")).
        OVERLAY(linea,i * 40 - 20,1)    = ":".
        OVERLAY(linea,i * 40 - 19,39)   = string(decimal(entry(2,tpartidas.tdsc + ":",":")),"$>>>,>>>,>>9").

        IF i = 3 THEN DO:
            RUN TmpL (INPUT 13, INPUT Linea).

            linea = "".
            i = 0.
        END.

        tDdcbles = tDdcbles + tpartidas.tcre.
    END.

    IF i = 3 THEN do:
        RUN TmpL (INPUT 13, INPUT Linea).

        linea = "".
        i = 0.
    END.

    i = i + 1.
    OVERLAY(linea,i * 40 - 39,39) = "TOTAL DEDUCIBLES ".
    OVERLAY(linea,i * 40 - 20,1) = ":".
    OVERLAY(linea,i * 40 - 19,39) = STRING(tDdcbles,"$>>>,>>>,>>9").

    RUN TmpL (INPUT 13, INPUT Linea).
    /* FIN DEDUCIBLES */

    Linea = "====================================================OTRAS CONDICIONES===================================================".

    RUN TmpL (INPUT 14, INPUT Linea).

    linea = fFrmto("1.   El seguro de cartera se liquida sobre el saldo de manera anticipada. Unicamente ampara del riesgo de muerte o incapacidad total y permanente del deudor solicitante de edad menor o igual a 69 años y 364 dias y hasta 400 SMLMV siempre y cuando cumpla con la exigencias de la Compañía de Seguros.",118).
    linea = linea + chr(10) + fFrmto("2.   El seguro de cartera para solicitantes mayores de 69 años y 364 dias hasta 85 años y 364 dias cubre créditos hasta 200 SMLMV. Para incapacidades cubre a solicitantes hasta los 65 años, para solicitantes mayores a esta edad que deseen mayor cobertura deberán cancelar una prima adicional.",118).
    linea = linea + chr(10) + fFrmto("3.   Por política interna los créditos con mora igual o superior a 90 días son exigibles en forma inmediata, pero Juriscoop se reserva el derecho de empezar la cobranza en cualquier momento del vencimiento.",118).
    linea = linea + chr(10) + fFrmto("4.   El deudor se obliga a renovar y mantener vigentes las pólizas de seguros constituídas sobre las garantías a favor de Juriscoop.",118).
    linea = linea + chr(10) + fFrmto("5.   Los gastos ocasionados por el perfeccionamiento de las garantías, serán a cargo del solicitante.",118).
    linea = linea + chr(10) + fFrmto("6.   Para los créditos que a criterio de Juriscoop o por normatividad requieran evaluación periodica, el solicitante se compromete a actualizar la información financiera, legal y de la garantía.",118).
    linea = linea + chr(10) + fFrmto("7.   El deudor se compromete a actualizar los certificados de libertad de los bienes inmuebles y vehículos dados en garantía, en caso contrario Juriscoop los actualizará y los gastos serán a cargo del deudor.",118).
    linea = linea + chr(10) + fFrmto("8.   A los Créditos de la línea Fidelización y Compra de Cartera que incurran en mora de mas de 30 días se les ajustará la tasa a la máxima vigente para créditos de consumo y ordinario certificada por la Superintendencia Financiera de Colombia. Como consecuencia de lo anterior, me obligo a pagar a Juriscoop o a quien represente sus derechos, el valor correspondiente a los intereses remuneratorios y moratorios, liquidados de conformidad a lo previsto en el manual de crédito, aceptando de antemano las variaciones que pueda sufrir la tasa de interés en aplicación de las condiciones allí estipuladas.",118). 
    linea = linea + chr(10) + fFrmto("Igualmente autorizo para que en el evento que resulten modificadas las condiciones del crédito, los descuentos mensuales sean los que Juriscoop notifique a la pagaduría, de conformidad con la libranza que para el efecto he suscrito.",118).
    linea = linea + chr(10) + fFrmto("9.   En el evento que por disposición legal o reglamentaria se autorice a cobrar intereses inferiores a los pactados en esta obligación tanto ordinarios como de mora, JURISCOOP los reajustara automáticamente, pero cuando dichos intereses se vuelvan a incrementar se podrán reajustar hasta la tasa máxima pactada para esta obligación",118).
    linea = linea + chr(10) + fFrmto("10.   Para operaciones amparadas con el Fondo de Garantías, Juriscoop renovará los certificados y los gastos serán a cargo del deudor.",118).
    linea = linea + chr(10) + fFrmto("11.   Declaro que conozco y por autonomía de mi libertad, ACEPTO los términos estipulados en la Solicitud de Productos Financieros y Actualización de Datos.",118).
        
    RUN TmpL (INPUT 14, INPUT Linea).

    Linea = "========================================================================================================================".

    RUN TmpL (INPUT 14, INPUT Linea).
    
    linea = fFrmto("Estimado cliente: Usted tiene derecho a solicitar la última calificación otorgada a su crédito, así como los motivos que la generaron, por tal razón Juriscoop esta en la obligación de responder en un tiempo no mayor a quince (15) días después del requerimiento formal y cancelado los costos administrativos y de papeleria. Los estamentos de control interno verificarán este cumplimiento.",118).

    RUN TmpL (INPUT 14, INPUT Linea).

    Linea = "========================================================================================================================".

    RUN TmpL (INPUT 14, INPUT Linea).

    linea = "".
    OVERLAY(linea,1,39) = "Firma Del Deudor".
    OVERLAY(linea,40,39) = "Firma Del Codeudor Solidario".
    OVERLAY(linea,80,39) = "Firma Del Codeudor Solidario".
    linea = linea + FILL(CHR(10),4).

    RUN TmpL (INPUT 15, INPUT Linea).

    linea = "".
    OVERLAY(linea,1,39) = "Nombre".
    OVERLAY(linea,40,39) = "Nombre".
    OVERLAY(linea,80,39) = "Nombre".
    RUN TmpL (INPUT 16, INPUT Linea).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pdesembolso wWin 
PROCEDURE Pdesembolso :
FIND Clientes WHERE Clientes.Nit EQ Creditos.Nit NO-LOCK NO-ERROR.

IF Creditos.Estado:SCREEN-VALUE IN FRAME f_formalizar EQ "4" THEN DO:
    MESSAGE "La Formalizacion del Desembolso del Crédito no se realizará...?" SKIP
            "Seleccionó Retirar sin Formalizar." SKIP
            "       Continue solo si está SEGURo de Anularlo..."
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirme Anulación" UPDATE W_RptaD AS LOGICAL.

    IF NOT W_RptaD THEN
        RETURN.

    RUN Negar_Solicitud.

    ASSIGN FRAME F_Formalizar:VISIBLE = FALSE
           FRAME F_Creditos:SENSITIVE = TRUE.

    APPLY "choose" TO Btn_Consulta IN FRAME F_Creditos.

    ENABLE ALL WITH FRAME F_Creditos.

    RETURN.
END.

IF Creditos.Estado:SCREEN-VALUE IN FRAME f_formalizar EQ "1" THEN DO:
    MESSAGE "Como es la última instancia, se espera que se tome una" SKIP
            "decisión acerca del crédito."
        VIEW-AS ALERT-BOX ERROR.
    APPLY "entry" TO Creditos.Estado.
    RETURN.
END.

IF Creditos.Estado:SCREEN-VALUE IN FRAME f_formalizar EQ "2" THEN DO:
    FIND FIRST Ahorros WHERE Ahorros.Nit EQ Creditos.Nit
                         AND Ahorros.Tip_ahorro EQ 4
                         AND Ahorros.Sdo_disponible + Ahorros.Sdo_Canje GT 0 NO-LOCK NO-ERROR.
    IF Clientes.Garantia EQ "Hipoteca" AND (W_fecha - Creditos.fec_aprobacion GT 120) THEN DO:
        MESSAGE "La solicitid de credito no se puede desembolsar ...Oper.Rechazada." SKIP          
                "La fecha de aprobacion tiene mas de 120 Dias - Por Hipoteca."
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.
    ELSE
        IF Clientes.Garantia NE "Hipoteca" AND (W_fecha - Creditos.fec_aprobacion GT 60) THEN DO:
            MESSAGE "La solicitid de credito no se puede desembolsar ...Oper.Rechazada." SKIP
                    "La fecha de aprobacion tiene mas de 60 Dias - No Hipoteca."
                VIEW-AS ALERT-BOX ERROR.
            RETURN.
        END.

    ASSIGN Creditos.Pagare:SCREEN-VALUE IN FRAME f_formalizar = STRING(Creditos.Num_Credito)
           W_MenDes:SCREEN-VALUE = "El Crèdito se Formalizarà con este No.de Pagarè".
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE periodos1 wWin 
PROCEDURE periodos1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR indi  AS INT INITIAL 0.
DEF VAR ndias2 AS INT INITIAL 30.

    
    CASE Creditos.Per_Pago:
        WHEN 0 THEN  ASSIGN Dato = "Diaria"
                             Periodo = 360
                             ndias = 1. 
        WHEN 1 THEN  ASSIGN Dato = "Semanal"
                             periodo = 52
                             ndias = 7.   
         WHEN 2 THEN  ASSIGN Dato = "Decadal"
                             periodo = 36   
                             ndias = 10.
         WHEN 3 THEN  ASSIGN Dato = "Quincenal"
                             periodo = 24
                             ndias = 15.
         WHEN 4 THEN  ASSIGN Dato = "Mensual"
                             periodo = 12   
                             ndias = 30.
         WHEN 5 THEN  ASSIGN Dato = "Bimestral"
                             periodo = 6   
                             ndias = 60.
         WHEN 6 THEN  ASSIGN Dato = "Trimestral"
                             periodo = 4   
                             ndias = 90.
         WHEN 7 THEN  ASSIGN Dato = "Cuatrimestral"
                             periodo = 3   
                             ndias = 120.
         WHEN 8 THEN  ASSIGN Dato = "Semestral"
                             periodo = 2   
                             ndias = 180.
         WHEN 9 THEN  ASSIGN Dato = "Anual"
                             periodo = 1   
                             ndias = 360.
   
    END CASE.

      
    IF  ptipo = 1 THEN DO: 
        tcuota = creditos.plazo.
            IF  integer(creditos.per_pago) > 3  THEN DO:
           /* tcuota = tcuota - ndias.*/
            ASSIGN mesan = (ndias / 30 ).
            finicio = W-FecEntLib.
/*
            finicio = ADD-INTERVAL(W-FecEntLib,- mesan,'months').  */
            END.
            ELSE  DO:  
             mesan = ndias.
               finicio = W-FecEntLib - ndias.
                IF  ndias =10 OR ndias=15 THEN DO:
                
                IF  MONTH(finicio)= MONTH(W-FecEntLib) AND  DAY(W-FecEntLib) = 31    THEN   mesan = mesan + 1.
                IF  MONTH(finicio)= MONTH(W-FecEntLib) AND  DAY(W-FecEntLib) = 29 AND MONTH(W-FecEntLib) = 2  THEN   mesan = mesan - 1.
                IF  MONTH(finicio)= MONTH(W-FecEntLib) AND  DAY(W-FecEntLib) = 28 AND MONTH(W-FecEntLib) = 2  THEN   mesan = mesan - 2.
                IF  MONTH(finicio) <  MONTH(W-FecEntLib) AND  DAY(finicio) < 30 AND ( MONTH(finicio) = 1 OR MONTH(finicio) = 3 OR MONTH(finicio) = 5  
                    OR MONTH(finicio) = 7  OR MONTH(finicio) = 8 OR MONTH(finicio) =10 OR MONTH(finicio) = 12) THEN  mesan = mesan + 1.
    
                IF  MONTH(finicio) < MONTH(W-FecEntLib) AND  DAY(finicio) < 29 AND MONTH(finicio) = 2  THEN mesan = mesan - 2.
                IF  MONTH(finicio) < MONTH(W-FecEntLib) AND  DAY(finicio) > 28 AND MONTH(finicio) = 2  THEN mesan = mesan - 1.
                  
                END.
            
          /*  tcuota = creditos.plazo * ndias.*/           
            /* fefin =   DATE(w-FecEntLib) + tcuota.*/
              finicio =  W-FecEntLib - mesan. 
            END. 
  
     END.

    END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir wWin 
PROCEDURE ProcesoImprimir :
CASE W_TipoInforme:
        WHEN "Proyeccion" THEN RUN Proyeccion.
        WHEN "Desembolso" THEN RUN ProyDes.
        WHEN "CNDCIONES"  THEN RUN cndciones.
        OTHERWISE RUN Informe.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proceso_Scoring wWin 
PROCEDURE Proceso_Scoring :
DO WITH FRAME F_Scoring:
   RUN Prc_LlenarScoring.r (INPUT Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud,
                            INPUT INTEG(Creditos.Num_Solicitud:SCREEN-VALUE)).
 
 FOR EACH TScoring: DELETE TScoring. END.
 FOR EACH Scoring WHERE Scoring.Nit EQ Creditos.Nit:SCREEN-VALUE AND Scoring.Fec_Scoring EQ TODAY BREAK BY Scoring.Codigo:
   TOTAL_Puntaje = TOTAL_Puntaje + Scoring.Puntaje.
   CREATE TScoring.
   ASSIGN TScoring.VarS = Scoring.VARIABLE
          TScoring.TabS = Scoring.Tabla
          TScoring.VVaS = Scoring.Valor_Variable
          TScoring.PunS = Scoring.Puntaje
          TScoring.FecS = Scoring.Fec_Scoring
          TScoring.CodS = Scoring.Codigo.
 END.
 OPEN QUERY Br_Scoring FOR EACH TScoring NO-LOCK BY TScoring.VarS INDEXED-REPOSITION.
 DISPLAY TOTAL_Puntaje WITH FRAME F_Scoring.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProyDes wWin 
PROCEDURE ProyDes :
DEF VAR W_Tot_Cre AS DEC.

DEF VAR W_NomPer AS CHAR FORMAT "X(15)".
DEF VAR W_PdoAno AS INT.
DEF VAR plazow AS INT.
DEF VAR W_NroDias AS INT.
DEF VAR P_NMeses  AS INT.

RUN HallarPeriodo IN W_ManFin (INPUT INT(SUBSTR(Cmb_PerPago:SCREEN-VALUE IN FRAME f_Solicitud,1,1)),
                               INPUT PlazoW,
                               OUTPUT W_NroDias,
                               OUTPUT P_NMeses,
                               OUTPUT W_PdoAno,
                               OUTPUT W_NomPer).

W_TotExt = 0.

FOR EACH Extras WHERE Extras.Nit EQ Creditos.Nit
                  AND Extras.Num_Solicitud EQ Creditos.Num_Solicitud NO-LOCK:
    IF Extras.Estado EQ 1 THEN
        ASSIGN W_TotExt = W_TotExt + Extras.Vr_CuoExtra.
END.

RUN Proyeccion_Desembolso.R (INPUT Creditos.Agencia,
                             INPUT DECIMAL(Creditos.Sdo_Capital:SCREEN-VALUE IN FRAME F_Solicitud),
                             INPUT DECIMAL(Creditos.Plazo:SCREEN-VALUE IN FRAME F_Solicitud),
                             INPUT DECIMAL(Creditos.Cuota:SCREEN-VALUE IN FRAME F_Solicitud),
                             INPUT W_TotExt,
                             INPUT Creditos.Fec_Aprobacion,
                             INPUT Creditos.Fec_PagAnti,
                             INPUT DECIMAL(W_TasaPeriodo:SCREEN-VALUE IN FRAME F_Solicitud) / 100,
                             INPUT DECIMAL(Creditos.Incremento:SCREEN-VALUE IN FRAME F_Solicitud),
                             INPUT 0, /*gracia*/
                             INPUT INTEGER(SUBSTRING(Cmb_PerPago:SCREEN-VALUE IN FRAME F_Solicitud,1,1)),
                             INPUT INTEGER(Creditos.FOR_Interes:SCREEN-VALUE IN FRAME F_Solicitud),
                             INPUT INTEGER(SUBSTRING(Cmb_Sistemas:SCREEN-VALUE IN FRAME F_Solicitud,1,5)),
                             INPUT Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud,
                             INPUT NomNit:SCREEN-VALUE IN FRAME F_Solicitud,
                             INPUT Creditos.Cod_Credito,   
                             INPUT SUBSTRING(Nom_Producto:SCREEN-VALUE IN FRAME F_Solicitud,7,30),
                             INPUT "S",
                             INPUT DECIMAL(Creditos.Num_Credito:SCREEN-VALUE IN FRAME F_Solicitud),
                             INPUT SUBSTRING(Cmb_Sistemas:SCREEN-VALUE IN FRAME F_Solicitud,9,30),
                             INPUT SUBSTRING(Cmb_PerPago:SCREEN-VALUE IN FRAME F_Solicitud,5,15),
                             INPUT 1,
                             INPUT Creditos.Monto).
                                                    
/*FIND FIRST PlanPagos WHERE PlanPagos.Nit EQ Creditos.Nit
                       AND PlanPagos.Cod_Credito EQ Creditos.Cod_Credito
                       AND PlanPagos.Num_Credito EQ Creditos.Num_Credito
                       AND PlanPagos.Id_PdoMes EQ 1 NO-ERROR.*/

ASSIGN W_TipoInforme = ""
       /*PlanPagos.Tasa = Creditos.Tasa
       PlanPagos.Tip_Credito = Creditos.Tip_Credito*/.

/*FIND CURRENT PlanPagos NO-LOCK NO-ERROR.*/

FOR EACH PlanPagos WHERE PlanPagos.Nit EQ Creditos.Nit
                     AND PlanPagos.Cod_Credito EQ Creditos.Cod_Credito
                     AND PlanPagos.Num_Credito EQ Creditos.Num_Credito
                     AND PlanPagos.Id_PdoMes NE 1:
    ASSIGN PlanPagos.Tasa = Creditos.Tasa
           PlanPagos.Tip_Credito = Creditos.Tip_Credito
           PlanPagos.Pagare = Creditos.Pagare.
END.

W_Tot_Cre = Creditos.monto.

FOR EACH CONTROL_pagos WHERE control_pagos.Nit EQ Creditos.Nit
                         and control_pagos.Cod_Credito EQ Creditos.Cod_Credito
                         and control_pagos.Num_Credito EQ Creditos.Num_Credito BY control_pagos.Nro_Cuota:
    IF control_pagos.Nro_Cuota = 1 THEN
        control_pagos.Pagos_IntAcum = ((creditos.monto * ((Creditos.Tasa / W_PdoAno) / 100)) / W_NroDias) * (Creditos.Fec_PagAnti - Creditos.Fec_Aprobacion).
    ELSE
        control_pagos.Pagos_IntAcum = ROUND(W_Tot_Cre * ((Creditos.Tasa / 100) / W_PdoAno), 0).
    
    control_pagos.pagos_capitalAcum = creditos.cuota - control_pagos.Pagos_IntAcum.

    FIND FIRST extras WHERE Extras.Nit EQ Creditos.Nit
                        AND extras.num_solicitud = creditos.num_solicitud
                        AND extras.cod_credito = creditos.cod_credito
                        AND Extras.Nro_Cuota EQ control_pagos.Nro_Cuota NO-LOCK NO-ERROR.
    IF AVAILABLE extras THEN
        W_Tot_Cre = W_Tot_Cre - control_pagos.pagos_capitalAcum - Extras.Vr_CuoExtra.
    ELSE
        W_Tot_Cre = W_Tot_Cre - control_pagos.pagos_capitalAcum.

    IF CONTROL_pagos.Id_PdoMes = 2 THEN DO:
        ASSIGN control_pagos.Cap_pagado = control_pagos.pagos_capitalAcum
               control_pagos.Int_pagado = control_pagos.Pagos_IntAcum.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proyeccion wWin 
PROCEDURE Proyeccion :
DEF VAR W_NomPer AS CHAR FORMAT "X(15)".
   DEF VAR W_PdoAno AS INT.
   DEF VAR plazow AS INT.
   DEF VAR W_NroDias AS INT.
   DEF VAR P_NMeses  AS INT.
   
      RUN HallarPeriodo IN W_ManFin (INPUT  INT(SUBSTR(Cmb_PerPago:SCREEN-VALUE IN FRAME f_Solicitud,1,1)),
                                     INPUT  PlazoW,
                                     OUTPUT W_NroDias,
                                     OUTPUT P_NMeses,
                                     OUTPUT W_PdoAno,
                                     OUTPUT W_NomPer).


RUN Proyeccion_Credito.R (INPUT DECIMAL(Creditos.Sdo_Capital:SCREEN-VALUE IN FRAME F_Solicitud),
      INPUT DECIMAL(Creditos.Plazo:SCREEN-VALUE IN FRAME F_Solicitud),
      INPUT DECIMAL(Creditos.Cuota:SCREEN-VALUE IN FRAME F_Solicitud),
      INPUT 0, /*total extras que ya no se utiliza*/
      INPUT W_Fecha, 
      INPUT creditos.fec_paganti,
      INPUT DECIMAL(W_TasaPeriodo:SCREEN-VALUE IN FRAME F_Solicitud) / 100,
      INPUT DECIMAL(Creditos.Incremento:SCREEN-VALUE IN FRAME F_Solicitud),
      INPUT 0, /*gracia*/
      INPUT INTEGER(SUBSTRING(Cmb_PerPago:SCREEN-VALUE IN FRAME F_Solicitud,1,1)),
      INPUT INTEGER(Creditos.FOR_Interes:SCREEN-VALUE IN FRAME F_Solicitud),
      INPUT INTEGER(SUBSTRING(Cmb_Sistemas:SCREEN-VALUE IN FRAME F_Solicitud,1,5)),
      INPUT Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud,
      INPUT NomNit:SCREEN-VALUE IN FRAME F_Solicitud,
      INPUT Creditos.Cod_Credito,   
      INPUT SUBSTRING(Nom_Producto:SCREEN-VALUE IN FRAME F_Solicitud,7,30),
      INPUT "S",
      INPUT DECIMAL(Creditos.Num_Credito:SCREEN-VALUE IN FRAME F_Solicitud),
      INPUT SUBSTRING(Cmb_Sistemas:SCREEN-VALUE IN FRAME F_Solicitud,9,30),
      INPUT SUBSTRING(Cmb_PerPago:SCREEN-VALUE IN FRAME F_Solicitud,5,15),
      INPUT 1,INPUT DECIMAL(Creditos.Monto:SCREEN-VALUE)).

W_TipoInforme = "".


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReportaListaSuspendidos wWin 
PROCEDURE ReportaListaSuspendidos :
FIND FIRST listaNegra WHERE listaNegra.nit = creditos.nit
                        AND (listaNegra.estado = 1 OR listaNegra.estado = 3) NO-ERROR.
IF creditos.dias_atraso > 30 OR AVAILABLE listaNegra THEN DO:
    IF NOT AVAILABLE listaNegra THEN DO:
        FIND FIRST clientes WHERE clientes.nit = creditos.nit NO-LOCK.

        CREATE listaNegra.
        ASSIGN listaNegra.codigo = 4
               listaNegra.nit = creditos.nit
               listaNegra.nombre = clientes.nombre
               listaNegra.apellido1 = clientes.apellido1
               listaNegra.apellido2 = clientes.apellido2
               listaNegra.usuario = w_usuario.
    END.

    listaNegra.observacion = STRING(creditos.cod_credito,"zzz") + " - " + STRING(creditos.fec_pago,"99/99/9999").
    listaNegra.fec_actualizacion = w_fecha.
    listaNegra.fec_exclusion = listaNegra.fec_actualizacion + (creditos.dias_atraso * 3).
    listaNegra.estado = 1.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SInfo wWin 
PROCEDURE SInfo :
DEFINE INPUT PARAMETER texto AS CHARACTER FORMAT "X(60)".
W_Ok = S_InfoCliente:ADD-LAST(texto) IN FRAME F_InfoCliente.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TmpL wWin 
PROCEDURE TmpL :
DEFINE INPUT PARAMETER Linea AS INTEGER FORMAT "99".
    DEFINE INPUT PARAMETER Texto AS CHARACTER FORMAT "X(125)".
    CREATE TmpI.
    ASSIGN TmpI.ILinea = Linea
           TmpI.ITexto = Texto
           TmpI.Nit     = Creditos.Nit.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Usuarios_X_Instancia wWin 
PROCEDURE Usuarios_X_Instancia :
FOR EACH TUxi: DELETE Tuxi. END.
  FOR EACH Cfg_Instancias WHERE
      Cfg_Instancias.Tipo_Instancia EQ 1  AND
      Cfg_Instancias.Instancia      EQ INTEGER(SUBSTRING(Cmb_InsCon:SCREEN-VALUE IN FRAME F_Condicionada,1,3)) AND
      Cfg_Instancias.Estado         EQ 1 NO-LOCK:
      FIND Usuarios WHERE Usuarios.Usuario EQ Cfg_Instancias.Usuario NO-LOCK NO-ERROR.
      IF AVAILABLE Usuarios THEN DO:
         CREATE TUxi.
         ASSIGN Tuxi.Agencia = Usuarios.Agencia
                Tuxi.Usuario = Usuarios.Usuario
                Tuxi.Nombre  = Usuarios.Nombre.
         FOR EACH Mov_Instancias WHERE
                  Mov_instancias.Instancia EQ Cfg_Instancias.Instancia AND
                  Mov_Instancias.Usuario   EQ Usuarios.Usuario AND
                  Mov_Instancias.Estado    EQ NO NO-LOCK:
            Tuxi.Cantidad = Tuxi.Cantidad + 1.
         END.
         FOR EACH Mov_Instancias WHERE 
                  Mov_Instancias.Instancia EQ Cfg_Instancias.Instancia AND
                  Mov_Instancias.Cuenta    EQ Creditos.Num_Credito:SCREEN-VALUE IN FRAME F_Solicitud AND
                  Mov_Instancias.Num_Solicitud EQ INTEGER(Creditos.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud) NO-LOCK:
                  FIND Tuxi WHERE TUxi.Usuario EQ Mov_Instancias.Usuario NO-ERROR.
                  IF AVAILABLE Tuxi THEN Tuxi.Proceso = YES.
         END.
      END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Validar_Operacion wWin 
PROCEDURE Validar_Operacion :
DEFINE INPUT  PARAMETER T_OfiVal  LIKE Agencias.Agencia.
  DEFINE INPUT  PARAMETER T_GrpVal  LIKE Grupos.Grupo.
  DEFINE INPUT  PARAMETER T_UsuVal  LIKE Usuarios.Usuario.
  DEFINE INPUT  PARAMETER T_OpeVal  LIKE Operacion.Cod_Operacion.
  DEFINE INPUT  PARAMETER T_Clave   LIKE Operacion.id_Clave.
  DEFINE INPUT  PARAMETER T_NomOpe  LIKE Operacion.Nom_Operacion.

  DEFINE VAR T_Validar AS LOGICAL INITIAL NO.

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

  IF T_Validar THEN DO:
   MESSAGE "Hay restricción para esta Operación...Cancelada" SKIP
            T_Nomope 
   VIEW-AS ALERT-BOX QUESTION BUTTONS OK.
   RETURN ERROR.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Validar_Solicitud wWin 
PROCEDURE Validar_Solicitud :
/*  IF AVAILABLE(Solicitud) THEN DO WITH FRAME {&FRAME-NAME}:
     W_Liquidar = FALSE.
     IF Pro_Creditos.Id_Tasa EQ 2 THEN DO:
        IF W_Sistema EQ 9 THEN DO:
           MESSAGE "Tasa Variable solo para Productos con Tasa por Producto" VIEW-AS ALERT-BOX.
           ASSIGN W_Sistema            = 1
                  W_CStma:SCREEN-VALUE = W_CStma:ENTRY(1).
           APPLY "ENTRY" TO W_CStma.
           RETURN ERROR. 
        END.
        IF W_TasEfe LE 0 THEN DO:
           RUN MostrarMensaje IN W_MANIJA (INPUT 301, OUTPUT W_Rpta).
           APPLY "ENTRY" TO Solicitud.Tasa.
           RETURN ERROR. 
        END.
     END.
     ELSE IF Indicadores.FecVcto LT TODAY THEN DO:
             RUN MostrarMensaje IN W_MANIJA (INPUT 359, OUTPUT W_Rpta).
             APPLY "ENTRY" TO W_CProCre.
             RETURN ERROR.
     END.
     IF W_ForPago EQ 3 OR W_Desembolso EQ 3 THEN DO:     
        IF W_ForPago EQ 3 THEN DO:
           IF Pro_Creditos.Id_AsociaCta EQ FALSE THEN DO:
              MESSAGE "El Producto de Créditos debe asociar Cuenta." VIEW-AS ALERT-BOX.
              APPLY "ENTRY" TO Solicitud.Cue_desembolso.
              RETURN ERROR.
           END.
           FIND Ahorros WHERE Ahorros.agencia          EQ W_agencia 
                       AND Ahorros.Cod_Producto        EQ Pro_Creditos.Cod_ProAhorro
                       AND Ahorros.Nit                 EQ W_Tercero
                       AND Ahorros.Cue_desembolso         EQ W_CueAho
                       AND Ahorros.Estado              EQ 1 
                       AND Ahorros.Fec_Cancelacion     EQ ? NO-LOCK NO-ERROR.
           IF NOT AVAILABLE(Ahorros) THEN DO:
              MESSAGE "Cuenta de Ahorros no es la que exige el Producto," SKIP
                      "La forma de pago no puede ser Débito Automático." VIEW-AS ALERT-BOX.   
              APPLY "ENTRY" TO Solicitud.Cue_desembolso.
              RETURN ERROR.
           END.
        END.
        IF W_Desembolso EQ 3 AND W_ForPago NE 3 THEN DO:
           FIND FIRST Ahorros WHERE Ahorros.agencia      EQ W_agencia 
                             AND Ahorros.Nit             EQ W_Tercero
                             AND Ahorros.Cue_desembolso     EQ W_CueAho
                             AND Ahorros.Estado          EQ 1 
                             AND Ahorros.cod_producto    EQ W_LinAho
                             AND Ahorros.Fec_Cancelacion EQ ? NO-LOCK NO-ERROR.
           IF AVAILABLE(Ahorros) THEN DO:
              W_LinAho = Ahorros.Cod_Producto.
              FIND Pro_Ahorros WHERE Pro_Ahorro.agencia  EQ W_agencia
                            AND Pro_Ahorros.Cod_Producto EQ Ahorros.Cod_Producto 
                            AND Pro_Ahorros.Estado       EQ 1
                            AND Pro_Ahorros.Tip_Producto EQ 1 NO-LOCK NO-ERROR. 
           END.
           IF NOT AVAILABLE(Pro_Ahorros)
           OR NOT AVAILABLE(Ahorros) THEN DO:
              MESSAGE "Cuenta de Ahorros no es Ahorro a la Vista," SKIP
                      "El Desembolso no puede ser Cuenta de Ahorros." VIEW-AS ALERT-BOX
                 TITLE "En P-Validar".
              APPLY "ENTRY" TO Solicitud.Cue_desembolso.
              RETURN ERROR.
           END.
        END.
     END.
     IF W_Sistema GT 4 AND W_Sistema LT 9 AND W_Incre EQ 0 THEN DO:
        MESSAGE "Falta Incremento para el sistema de amortización" VIEW-AS ALERT-BOX.
        APPLY "ENTRY" TO W_Incre.
        RETURN ERROR. 
     END.
     IF W_RForma NE 1 THEN DO:
        IF Pro_Creditos.Id_Montominimo EQ TRUE THEN DO:
           IF W_XMonto LT (Pro_Creditos.Val_Montominimo) OR W_XMonto EQ 0 THEN DO:
              RUN MostrarMensaje IN W_Manija (INPUT 257,OUTPUT W_Rpta).
              APPLY "ENTRY" TO W_Monto.
              RETURN ERROR.
           END.           
        END.
     END.
     
     IF W_Sistema EQ 8 AND W_Incre LT 0.0 
     OR W_Sistema EQ 8 AND INTEGER(W_Incre) GE W_Plazo THEN DO:
         RUN MostrarMensaje IN W_Manija (INPUT 128,OUTPUT W_Rpta).        
         W_Gracia =  0.
         APPLY "ENTRY" TO W_Incre.
         RETURN ERROR.
     END.
     RUN Periodicidad.
     IF W_Sistema EQ 7 THEN 
        IF W_Plazo MODULO W_Per NE 0 THEN DO:
           RUN MostrarMensaje IN W_Manija (INPUT 250,OUTPUT W_Rpta).
           APPLY "ENTRY" TO W_Plazo.
           RETURN ERROR.
        END.
  END.*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Valida_CfgRegCred wWin 
PROCEDURE Valida_CfgRegCred :
/*------------------------------------------------------------------------------
  Purpose: Valida Morosidad Solicitante y sus Codeudos.    
  Notes:   Nov.28/05 GAER.
  Leonardo G. Ocampo - 14/07/2010
------------------------------------------------------------------------------*/
DEFI INPUT PARAM P_CedNit LIKE Creditos.Nit.
DEFI INPUT PARAM piLinea LIKE Creditos.Cod_Credito.
DEFI VAR Nro_CredACanc LIKE Solicitud.Num_CredACanc INIT 0.
DEFINE VAR flagError AS LOGICAL.

FOR EACH CCfg_RegCredito:
    DELETE CCfg_RegCredito.
END.

DEFI VAR W_RowIdCr AS ROWID.

ASSIGN W_RowIdCr = ROWID(Creditos).

FIND FIRST Cfg_RegCredito WHERE Cfg_RegCredito.Cod_Credito EQ piLinea
                            AND Cfg_RegCredito.Id_Vigente EQ 0 NO-LOCK NO-ERROR.
IF NOT AVAIL(Cfg_RegCredito) THEN DO:
    MESSAGE "La linea no tiene Cfg_RegCredito Vigente." SKIP
            "                  Operacion Rechazada."
        VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
END.

FIND FIRST Cfg_OrigRegCredito WHERE Cfg_OrigRegCredito.Cod_Credito EQ piLinea  NO-LOCK NO-ERROR.

IF P_CedNit EQ Cfg_OrigRegCredito.CedNit_UnaVez THEN DO:
    CREATE CCfg_RegCredito.
    BUFFER-COPY Cfg_OrigRegCredito TO CCfg_RegCredito.
END.
ELSE DO:
    CREATE CCfg_RegCredito.
    BUFFER-COPY Cfg_RegCredito TO CCfg_RegCredito.
END.

RELEASE Cfg_OrigRegCredito.
RELEASE Cfg_RegCredito.

IF W_VrCredAcanc > 0 THEN DO:
    FOR EACH creditos WHERE Creditos.Nit = P_CedNit
                        AND Creditos.Estado = 2
                        AND Creditos.FOR_Pago <> 2
                        AND Creditos.Dias_Atraso > CCfg_RegCredito.Nro_DiasVencidos NO-LOCK:
        FIND FIRST solicitudes_pagoCreditos WHERE solicitudes_pagoCreditos.cliente_id = creditos.nit
                                              AND solicitudes_pagoCreditos.num_solicitud = solicitud.num_solicitud
                                              AND solicitudes_pagoCreditos.num_credito = creditos.num_credito NO-LOCK NO-ERROR.
        IF NOT AVAILABLE solicitudes_pagoCreditos THEN DO:
            MESSAGE "La línea sólo permite días vencidos máximo hasta:" CCfg_RegCredito.Nro_DiasVencidos SKIP
                    "No se permite el desembolso."
                VIEW-AS ALERT-BOX ERROR.

            flagError = TRUE.
        END.
    END.

    IF flagError = TRUE THEN DO:
        FIND FIRST Creditos WHERE ROWID(Creditos) = W_RowIdCr NO-ERROR.
        RETURN ERROR.
    END.
END.

FIND LAST Creditos WHERE Creditos.Nit = P_CedNit
                     AND Creditos.Estado = 2
                     AND Creditos.FOR_Pago = 2
                     AND Creditos.Dias_Atraso > 0 NO-LOCK NO-ERROR.
IF AVAILABLE(Creditos) AND CCfg_RegCredito.Nro_DiasVencidos < Creditos.Dias_Atraso THEN DO:
    MESSAGE "El solicitante tiene crédito por libranza con morosidad de días:" Creditos.Dias_Atraso SKIP
            "Puede continuar con el desembolso. Por favor informe al área de Cartera."
        VIEW-AS ALERT-BOX TITLE "Informativo Morosidad Libranza".
END.

FOR EACH Relaciones WHERE Relaciones.Nit_Relacion EQ P_CedNit
                      AND Relaciones.Clase_Producto EQ 2
                      AND Relaciones.Cod_Relacion EQ 11
                      AND Relaciones.Estado EQ 1 NO-LOCK:
    FIND FIRST Creditos WHERE Creditos.Nit EQ Relaciones.Nit
                          AND Creditos.Num_Credito EQ INTEG (Relaciones.Cuenta)
                          AND Creditos.FOR_Pago NE 2
                          AND Creditos.Sdo_Capital GT 0
                          AND Creditos.Dias_Atraso GT 0 NO-LOCK NO-ERROR.
    IF AVAIL(Creditos) AND Creditos.Dias_Atraso GT CCfg_RegCredito.DiasVencidos_Fiando THEN DO:
        MESSAGE "El Solicitante es CoDeudor de Un Asociado en Mora:" Relaciones.Nit SKIP
                "Con dias de Mora : " Creditos.Dias_Atraso SKIP
                "                      No se permite Desembolso."
            VIEW-AS ALERT-BOX ERROR.

        FIND Creditos WHERE ROWID(Creditos) EQ W_RowIdCr NO-ERROR.
        RETURN ERROR.
    END.
END.

FIND Creditos WHERE ROWID(Creditos) EQ W_RowIdCr NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Verificar_Cuenta_Desembolso wWin 
PROCEDURE Verificar_Cuenta_Desembolso :
FIND Ahorros WHERE Ahorros.Agencia      EQ Creditos.Age_Desembolso AND
                    Ahorros.Cod_Ahorro  EQ Creditos.Cod_Desembolso AND
                    Ahorros.Cue_Ahorros EQ Creditos.Cue_Desembolso AND 
                    Ahorros.Estado      NE 2 NO-ERROR.
 IF NOT AVAILABLE Ahorros THEN DO: 
    MESSAGE "Falta la Cuenta de Ahorros Activa para El Desembolso"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN ERROR.
 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fEdad wWin 
FUNCTION fEdad RETURNS CHARACTER
  (daFchaNcmiento AS DATE /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEF VAR ianos AS DECIMAL NO-UNDO.
    ianos = TRUNCATE((TODAY - daFchaNcmiento) / 360,0).
    RETURN STRING(ianos).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fFrmto wWin 
FUNCTION fFrmto RETURNS CHARACTER
  (c AS CHAR,l AS INTEGER /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    c = TEXTO A FORMATEAR
    l = Ancho Máximo De La Línea        
    ------------------------------------------------------------------------------*/


    DEF VAR c1 AS CHAR NO-UNDO.
    DEF VAR i AS INTEGER NO-UNDO.
    DEF VAR j AS INTEGER NO-UNDO.
    i = l.
    j = 1.
    DO WHILE i <= LENGTH(c):
        DO WHILE SUBSTRING(c,i,1) <> " " AND SUBSTRING(c,i,1) <> CHR(10) AND i > 1:
            i = i - 1.
        END.
        IF NOT i >= j * l 
        THEN DO:
            SUBSTRING(c,i,1) = CHR(10).
            j = j + 1.
            i = j * l.
        END.
        ELSE i = i - 1.
    END.
    RETURN c.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fNmbreEstcion wWin 
FUNCTION fNmbreEstcion RETURNS CHARACTER
  (c AS CHAR /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    FIND FIRST estaciones NO-LOCK
        WHERE
            estaciones.estacion = c NO-ERROR.
    RETURN IF AVAILABLE estaciones THEN estaciones.descripcion ELSE "ERROR:ESTACION NO DEFINIDA".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fNmbreUsuario wWin 
FUNCTION fNmbreUsuario RETURNS CHARACTER
  (c AS CHAR /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    FIND FIRST usuarios NO-LOCK
        WHERE
            usuarios.usuario = c NO-ERROR.
    RETURN IF AVAILABLE usuarios THEN usuarios.nombre ELSE "ERROR: USUARIO NO DEFINIDO".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

