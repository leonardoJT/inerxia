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
  DEFINE VAR W_Re AS INTEGER.
  DEFINE VAR Orden_InsPan LIKE Instancias.Orden.
  DEFINE VARIABLE W_Error       AS LOGICAL.
  DEFINE VARIABLE W_Autorizo  LIKE Usuarios.Usuario.
  DEFINE VARIABLE choice AS LOGICAL.
  DEFINE VARIABLE W_Grupo LIKE Grupos.Grupo.
  DEFINE VAR W_NvaAdm AS LOGICAL.
  DEFINE VAR W_NvaHV AS LOGICAL.
  DEFINE VAR W_NvoCD AS LOGICAL.
  DEFINE VAR W_Pdt AS CHARACTER FORMAT "X(80)".
  DEFINE VAR Puntero AS ROWID.
  DEFINE VAR PuntGar AS ROWID.
  DEFINE VAR Longitud AS DECIMAL.
  DEFINE VAR W_Ultima  LIKE Instancias.Instancia.
  DEFINE VAR W_Primera LIKE Instancias.Instancia.
  DEFINE VAR W_SucAgen LIKE Usuarios.Id_OpeOfi.
  DEFINE VAR Id_Agregar AS CHARACTER FORMAT "X(2)".
  DEFINE VAR W_Tippdt LIKE Creditos.Tip_Credito.
  DEFINE VAR W_TipoInforme AS CHARACTER FORMAT "X(10)".
  DEFINE VAR T_Operacion LIKE Operacion.Cod_Operacion.
  DEFINE VAR T_Deducible LIKE Operacion.Cod_Deducible.
  DEFINE VAR W_ProFor    LIKE Formatos.Nom_Proceso.
  DEFINE VAR W_CodChe    LIKE Cuentas.Cod_Formato.
  DEFINE VAR W_VigIns    LIKE Instancias.TMI.
  DEFINE VAR W_NumCbt    LIKE Comprobantes.Secuencia.
   
  DEFINE VAR W_CtaCorCre LIKE Cuentas.Cuenta.
  DEFINE VAR W_CtaCorAho LIKE Cuentas.Cuenta.
  DEFINE VAR W_CtaBanco  LIKE Cuentas.Cuenta.
  DEFINE VAR W_Des       AS LOGICAL.
  DEFINE VAR W_MontoCre LIKE Creditos.Sdo_Capital.
  DEFINE VAR W_MontoDeb LIKE Creditos.Sdo_Capital.
  DEFINE VAR W_Cbte     LIKE Comprobantes.Comprobante.
 
/*para buscar un cliente*/
  DEFINE VARIABLE P_Nit      LIKE Clientes.Nit.
  DEFINE VARIABLE p_Nombre   LIKE Clientes.Nombre.
  DEFINE VARIABLE P_Apellido LIKE Clientes.Apellido1.
  DEFINE VARIABLE P_AgeCli   LIKE Clientes.Agencia.
  DEFINE VARIABLE W_Nuevo    AS LOGICAL.
  
  DEFINE VAR W_Contenido AS CHARACTER FORMAT "X(400)".
  
 /*guarda las partidas de contabilizacion*/
  DEFINE TEMP-TABLE TPartidas
    FIELD TCta LIKE Cuentas.Cuenta
    FIELD TDoc LIKE Mov_Contable.Doc_Referencia
    FIELD TDsc LIKE Mov_Contable.Comentario
    FIELD TTip AS INTEGER FORMAT 9 /*1-Ahorro, 2-Credito*/
    FIELD TOpe LIKE Operacion.Cod_Operacion
    FIELD TDeb LIKE Ahorros.Sdo_Disponible
    FIELD TCre LIKE Ahorros.Sdo_Disponible.
  
 /*para guardar garantias a cambiar estado de contabilzadas*/ 
 DEFINE TEMP-TABLE TGar LIKE Garantias.
    
/*guarda los usuarios disponibles para la siguiente instancia*/
  DEFINE TEMP-TABLE TProIns
    FIELD TP_Agencia LIKE Agencias.Agencia
    FIELD TP_Orden LIKE Instancias.Orden_Instancia
    FIELD TP_Instancia LIKE Instancias.Instancia
    FIELD TP_NomInstan AS CHARACTER FORMAT "X(30)"
    FIELD TP_Usuario   LIKE Usuarios.Usuario
    FIELD TP_NomUsuar  AS CHARACTER FORMAT "X(30)"
    FIELD TP_Cantidad  AS INTEGER FORMAT "999".
    
/*contiene los codeudores de cada solicitud*/
  DEFINE TEMP-TABLE TCode
    FIELD TC_AgeCode  LIKE Agencias.Agencia
    FIELD TC_NitCode  LIKE Clientes.Nit
    FIELD TC_NitDeud  LIKE Clientes.Nit
    FIELD TC_NumSoli  LIKE Solicitud.Num_Solicitud
    FIELD TC_NomCode  AS CHARACTER FORMAT "X(60)"
    FIELD TC_TelCdRs  LIKE Clientes.Tel_Residencia
    FIELD TC_TelCdCo  LIKE Clientes.Tel_Comercial
    FIELD TC_EmlCode  LIKE Clientes.email
    FIELD TC_EstRela  LIKE Relaciones.Estado
    FIELD TC_FecCrea  LIKE Relaciones.Fec_Ingreso
    FIELD TC_FecReti  LIKE Relaciones.Fec_Ingreso.

/*Contiene los abogados de la primera instancia de abogados*/
  DEFINE TEMP-TABLE TAbo
    FIELD CodAbo LIKE Usuarios.Usuario
    FIELD NomAbo LIKE Usuarios.Nombre
    FIELD InsAbo LIKE Instancias.Instancia.
    

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
  
/*para buscar una cuenta de ahorro*/
  DEFINE VARIABLE A_Nit LIKE Ahorros.Nit.
  DEFINE VARIABLE A_Age LIKE Ahorros.Agencia.
  DEFINE VARIABLE A_Pro LIKE Ahorros.Cod_Ahorro.
  DEFINE VARIABLE A_NitW LIKE Ahorros.Nit.
  DEFINE VARIABLE A_Cue LIKE Ahorros.Cue_Ahorros.
  
  DEFINE VARIABLE i AS INTEGER.
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
      FIELD Vigencia      AS INTEGER FORMAT "9999"
      FIELD FecAcuerdo    AS DATE FORMAT "99/99/99"
      FIELD NumAcuerdo    LIKE Cobros.Nro_Cobro.

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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Acuerdo
&Scoped-define BROWSE-NAME Br_Admisibles

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Garantias TCerradas TCode Hoja_Vida Consulta ~
TDeducc Cobros Mov_Instancias Creditos

/* Definitions for BROWSE Br_Admisibles                                 */
&Scoped-define FIELDS-IN-QUERY-Br_Admisibles Garantias.Identificacion_Bien Garantias.Nom_Bien Garantias.Val_Bien   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Admisibles   
&Scoped-define SELF-NAME Br_Admisibles
&Scoped-define QUERY-STRING-Br_Admisibles FOR EACH Garantias WHERE    Garantias.Num_Credito EQ Creditos.Num_Credito AND    Garantias.Estado EQ 1 NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br_Admisibles OPEN QUERY {&SELF-NAME} FOR EACH Garantias WHERE    Garantias.Num_Credito EQ Creditos.Num_Credito AND    Garantias.Estado EQ 1 NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_Admisibles Garantias
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Admisibles Garantias


/* Definitions for BROWSE Br_Cerradas                                   */
&Scoped-define FIELDS-IN-QUERY-Br_Cerradas TCerradas.Instancia TCerradas.INom_Instancia TCerradas.Usuario TCerradas.INom_Usuario TCerradas.Fec_Retiro TCerradas.Descripcion   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Cerradas   
&Scoped-define SELF-NAME Br_Cerradas
&Scoped-define QUERY-STRING-Br_Cerradas FOR EACH TCerradas NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br_Cerradas OPEN QUERY {&SELF-NAME} FOR EACH TCerradas NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_Cerradas TCerradas
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Cerradas TCerradas


/* Definitions for BROWSE Br_Codeudores                                 */
&Scoped-define FIELDS-IN-QUERY-Br_Codeudores TCode.TC_AgeCode TCode.TC_NitCode TCode.TC_NomCode TCode.TC_TelCdRs TCode.TC_TelCdCo TCode.TC_emlCode   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Codeudores   
&Scoped-define SELF-NAME Br_Codeudores
&Scoped-define QUERY-STRING-Br_Codeudores FOR EACH TCode NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br_Codeudores OPEN QUERY {&SELF-NAME} FOR EACH TCode NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_Codeudores TCode
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Codeudores TCode


/* Definitions for BROWSE Br_ConHV                                      */
&Scoped-define FIELDS-IN-QUERY-Br_ConHV Hoja_Vida.Fec_Grabacion Hoja_Vida.Fec_Limite Hoja_Vida.Observacion   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_ConHV   
&Scoped-define SELF-NAME Br_ConHV
&Scoped-define QUERY-STRING-Br_ConHV FOR EACH Hoja_Vida WHERE        Hoja_Vida.Tipo      EQ 9 AND Hoja_Vida.Codigo EQ 2 AND        Hoja_Vida.Instancia EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos, ~
      1, ~
      5)) AND        Hoja_Vida.DoctoRef  EQ INTEGER(Creditos.Num_Credito:SCREEN-VALUE IN FRAME F_Solicitud) AND        Hoja_Vida.Nit       EQ Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud AND        Hoja_Vida.Asunto_Cumplido EQ NO AND        Hoja_Vida.Usuario   EQ W_Usuario   INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br_ConHV OPEN QUERY {&SELF-NAME} FOR EACH Hoja_Vida WHERE        Hoja_Vida.Tipo      EQ 9 AND Hoja_Vida.Codigo EQ 2 AND        Hoja_Vida.Instancia EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos, ~
      1, ~
      5)) AND        Hoja_Vida.DoctoRef  EQ INTEGER(Creditos.Num_Credito:SCREEN-VALUE IN FRAME F_Solicitud) AND        Hoja_Vida.Nit       EQ Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud AND        Hoja_Vida.Asunto_Cumplido EQ NO AND        Hoja_Vida.Usuario   EQ W_Usuario   INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_ConHV Hoja_Vida
&Scoped-define FIRST-TABLE-IN-QUERY-Br_ConHV Hoja_Vida


/* Definitions for BROWSE Br_Consulta                                   */
&Scoped-define FIELDS-IN-QUERY-Br_Consulta Consulta.Num_Credito Consulta.AgeCredito Consulta.Nit Consulta.Nombre Consulta.Fec_Ingreso Consulta.Hor_Ingreso Consulta.Monto Consulta.Vigencia Consulta.FecAcuerdo   
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


/* Definitions for BROWSE B_Acuerdos                                    */
&Scoped-define FIELDS-IN-QUERY-B_Acuerdos Cobros.Nro_Cobro Cobros.Usuario Cobros.Val_Compromiso Cobros.Fec_Compromiso Cobros.Val_Cumplido Cobros.Estado   
&Scoped-define ENABLED-FIELDS-IN-QUERY-B_Acuerdos   
&Scoped-define SELF-NAME B_Acuerdos
&Scoped-define QUERY-STRING-B_Acuerdos FOR EACH Cobros WHERE    Cobros.Num_Credito EQ INTEGER(Creditos.Num_Credito:SCREEN-VALUE IN FRAME F_Solicitud) AND    Cobros.Nit EQ Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-B_Acuerdos OPEN QUERY {&SELF-NAME} FOR EACH Cobros WHERE    Cobros.Num_Credito EQ INTEGER(Creditos.Num_Credito:SCREEN-VALUE IN FRAME F_Solicitud) AND    Cobros.Nit EQ Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-B_Acuerdos Cobros
&Scoped-define FIRST-TABLE-IN-QUERY-B_Acuerdos Cobros


/* Definitions for FRAME F_Acuerdo                                      */
&Scoped-define FIELDS-IN-QUERY-F_Acuerdo Cobros.Fec_Compromiso ~
Cobros.Val_Compromiso Cobros.Usuario Cobros.Age_Recaudo Cobros.Val_Cumplido ~
Cobros.Usu_Recaudo Cobros.Nro_Transaccion Cobros.Fec_Cumplimiento 
&Scoped-define ENABLED-FIELDS-IN-QUERY-F_Acuerdo Cobros.Fec_Compromiso ~
Cobros.Val_Compromiso 
&Scoped-define ENABLED-TABLES-IN-QUERY-F_Acuerdo Cobros
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-F_Acuerdo Cobros
&Scoped-define QUERY-STRING-F_Acuerdo FOR EACH Cobros SHARE-LOCK
&Scoped-define OPEN-QUERY-F_Acuerdo OPEN QUERY F_Acuerdo FOR EACH Cobros SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-F_Acuerdo Cobros
&Scoped-define FIRST-TABLE-IN-QUERY-F_Acuerdo Cobros


/* Definitions for FRAME F_Admisibles                                   */
&Scoped-define FIELDS-IN-QUERY-F_Admisibles Garantias.Tipo_Garantia ~
Garantias.Nit_Aseguradora Garantias.Estado Garantias.Nro_Seguro ~
Garantias.Fec_Creacion Garantias.Fec_Retiro Garantias.Fec_IniSeguro ~
Garantias.Descripcion_Bien Garantias.Fec_FinSeguro Garantias.Val_Asegurado ~
Garantias.Nom_Impuesto Garantias.Fec_VctoImpuesto Garantias.Val_Impuesto 
&Scoped-define QUERY-STRING-F_Admisibles FOR EACH Garantias SHARE-LOCK
&Scoped-define OPEN-QUERY-F_Admisibles OPEN QUERY F_Admisibles FOR EACH Garantias SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-F_Admisibles Garantias
&Scoped-define FIRST-TABLE-IN-QUERY-F_Admisibles Garantias


/* Definitions for FRAME F_Cerradas                                     */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Cerradas ~
    ~{&OPEN-QUERY-Br_Cerradas}

/* Definitions for FRAME F_Codeudores                                   */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Codeudores ~
    ~{&OPEN-QUERY-Br_Codeudores}

/* Definitions for FRAME F_ConAcuerdo                                   */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_ConAcuerdo ~
    ~{&OPEN-QUERY-B_Acuerdos}

/* Definitions for FRAME F_ConHV                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_ConHV ~
    ~{&OPEN-QUERY-Br_ConHV}

/* Definitions for FRAME F_Consulta                                     */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Consulta ~
    ~{&OPEN-QUERY-Br_Consulta}

/* Definitions for FRAME F_Deducibles                                   */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Deducibles ~
    ~{&OPEN-QUERY-Br_Deducibles}

/* Definitions for FRAME F_HojaVida                                     */
&Scoped-define FIELDS-IN-QUERY-F_HojaVida Hoja_Vida.Asunto_Cumplido ~
Hoja_Vida.Fec_Limite Hoja_Vida.Fec_Grabacion Hoja_Vida.Observacion 
&Scoped-define ENABLED-FIELDS-IN-QUERY-F_HojaVida Hoja_Vida.Asunto_Cumplido ~
Hoja_Vida.Fec_Grabacion Hoja_Vida.Observacion 
&Scoped-define ENABLED-TABLES-IN-QUERY-F_HojaVida Hoja_Vida
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-F_HojaVida Hoja_Vida
&Scoped-define QUERY-STRING-F_HojaVida FOR EACH Hoja_Vida SHARE-LOCK
&Scoped-define OPEN-QUERY-F_HojaVida OPEN QUERY F_HojaVida FOR EACH Hoja_Vida SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-F_HojaVida Hoja_Vida
&Scoped-define FIRST-TABLE-IN-QUERY-F_HojaVida Hoja_Vida


/* Definitions for FRAME F_Instancias                                   */
&Scoped-define FIELDS-IN-QUERY-F_Instancias Mov_Instancias.Fec_Ingreso ~
Mov_Instancias.Fec_Retiro Mov_Instancias.Estado Mov_Instancias.Descripcion 
&Scoped-define ENABLED-FIELDS-IN-QUERY-F_Instancias Mov_Instancias.Estado ~
Mov_Instancias.Descripcion 
&Scoped-define ENABLED-TABLES-IN-QUERY-F_Instancias Mov_Instancias
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-F_Instancias Mov_Instancias
&Scoped-define QUERY-STRING-F_Instancias FOR EACH Mov_Instancias SHARE-LOCK
&Scoped-define OPEN-QUERY-F_Instancias OPEN QUERY F_Instancias FOR EACH Mov_Instancias SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-F_Instancias Mov_Instancias
&Scoped-define FIRST-TABLE-IN-QUERY-F_Instancias Mov_Instancias


/* Definitions for FRAME F_Juzgado                                      */
&Scoped-define FIELDS-IN-QUERY-F_Juzgado Creditos.Nit_Juzgado ~
Creditos.Nom_Juzgado 
&Scoped-define ENABLED-FIELDS-IN-QUERY-F_Juzgado Creditos.Nit_Juzgado 
&Scoped-define ENABLED-TABLES-IN-QUERY-F_Juzgado Creditos
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-F_Juzgado Creditos
&Scoped-define QUERY-STRING-F_Juzgado FOR EACH Creditos SHARE-LOCK
&Scoped-define OPEN-QUERY-F_Juzgado OPEN QUERY F_Juzgado FOR EACH Creditos SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-F_Juzgado Creditos
&Scoped-define FIRST-TABLE-IN-QUERY-F_Juzgado Creditos


/* Definitions for FRAME F_Solicitud                                    */
&Scoped-define FIELDS-IN-QUERY-F_Solicitud Creditos.Nit ~
Creditos.Num_Credito Creditos.Num_Solicitud Creditos.Fec_Aprobacion ~
Creditos.Fec_Desembolso Creditos.Int_Corrientes Creditos.Fec_Pago ~
Creditos.Int_Anticipado Creditos.Monto Creditos.Fec_ProxLiquidacion ~
Creditos.Int_DifCobro Creditos.Plazo Creditos.Fec_UltPago ~
Creditos.Sdo_Capital Creditos.Cuota Creditos.For_Interes ~
Creditos.Fec_UltLiquidacion Creditos.Sdo_Proyectado Creditos.Cuo_Pagadas ~
Creditos.Tasa Creditos.Val_Atraso Creditos.Dias_Atraso Creditos.Cuo_Atraso ~
Creditos.Provision Creditos.Fec_Reestructurado Creditos.Reestructurado 
&Scoped-define QUERY-STRING-F_Solicitud FOR EACH Creditos SHARE-LOCK
&Scoped-define OPEN-QUERY-F_Solicitud OPEN QUERY F_Solicitud FOR EACH Creditos SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-F_Solicitud Creditos
&Scoped-define FIRST-TABLE-IN-QUERY-F_Solicitud Creditos


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Cobros.Fec_Compromiso Cobros.Val_Compromiso 
&Scoped-define ENABLED-TABLES Cobros
&Scoped-define FIRST-ENABLED-TABLE Cobros
&Scoped-Define ENABLED-OBJECTS RECT-296 BUTTON-178 Btn_SalAcuerdo ~
Btn_ImpAcuerdo BUTTON-177 BUTTON-175 
&Scoped-Define DISPLAYED-FIELDS Cobros.Fec_Compromiso Cobros.Val_Compromiso ~
Cobros.Usuario Cobros.Age_Recaudo Cobros.Val_Cumplido Cobros.Usu_Recaudo ~
Cobros.Nro_Transaccion Cobros.Fec_Cumplimiento 
&Scoped-define DISPLAYED-TABLES Cobros
&Scoped-define FIRST-DISPLAYED-TABLE Cobros


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_ImpAcuerdo 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Button 176" 
     SIZE 15 BY 1.88.

DEFINE BUTTON Btn_SalAcuerdo 
     LABEL "Salvar Acuerdo" 
     SIZE 15 BY 1.88.

DEFINE BUTTON BUTTON-175 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 175" 
     SIZE 15 BY 1.88.

DEFINE BUTTON BUTTON-177 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     LABEL "Button 177" 
     SIZE 15 BY 1.88.

DEFINE BUTTON BUTTON-178 
     LABEL "Crear Nuevo" 
     SIZE 15 BY 1.88.

DEFINE RECTANGLE RECT-296
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 42 BY 6.19.

DEFINE BUTTON Btn_OutAdmisibles 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 168" 
     SIZE 8 BY 1.62.

DEFINE BUTTON BUTTON-169 
     LABEL "i" 
     SIZE 3 BY .85
     FONT 0.

DEFINE RECTANGLE RECT-295
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 34 BY 5.38.

DEFINE BUTTON BUTTON-153 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 153" 
     SIZE 9 BY 1.88.

DEFINE VARIABLE E_Agregar AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 55 BY 5.38
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE BUTTON Btn_OutCerradas 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 143" 
     SIZE 8 BY 1.62.

DEFINE BUTTON BUTTON-154 
     LABEL "Ver solo Instancia y Descripción" 
     SIZE 25 BY 1.12.

DEFINE BUTTON BUTTON-166 
     LABEL "Ver Información Detallada del Codeudor Seleccionado" 
     SIZE 44 BY 1.12.

DEFINE BUTTON BUTTON-167 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 167" 
     SIZE 8 BY 1.62.

DEFINE VARIABLE R_EstadoCode AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Activos", 1,
"Inactivos", 2
     SIZE 22 BY .85 NO-UNDO.

DEFINE BUTTON Btn_OutConAcuerdo 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 179" 
     SIZE 9 BY 1.62.

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

DEFINE BUTTON Btn_ProIns 
     LABEL "Procesar Instancia" 
     SIZE 23 BY 1.12.

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

DEFINE VARIABLE WTUsura AS DECIMAL FORMAT ">>9.99999":U INITIAL 0 
     LABEL "Tasa de Usura" 
      VIEW-AS TEXT 
     SIZE 10 BY .81
     BGCOLOR 12 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 12 BY 5.38.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 12 BY 2.15.

DEFINE VARIABLE TAct AS LOGICAL INITIAL no 
     LABEL "Refrescar Automáticamente" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY .77 NO-UNDO.

DEFINE BUTTON BUTTON-101 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 101" 
     SIZE 9 BY 1.88.

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

DEFINE BUTTON BUTTON-108 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 108" 
     SIZE 7 BY 1.35.

DEFINE BUTTON BUTTON-156 
     LABEL "Ver Información Detallada" 
     SIZE 30 BY 1.12.

DEFINE VARIABLE S_InfoCliente AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 66 BY 12.38
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
     SIZE 15 BY 2.15.

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

DEFINE BUTTON BUTTON-179 
     LABEL "Salvar Juzgado" 
     SIZE 17 BY 1.62.

DEFINE BUTTON BUTTON-180 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 180" 
     SIZE 9 BY 1.62.

DEFINE BUTTON Btn_Acuerdo 
     LABEL "Acuerdo de Pago" 
     SIZE 36 BY 1.12.

DEFINE BUTTON Btn_GarAdm 
     LABEL "Información Garantias Admisibles" 
     SIZE 36 BY 1.12.

DEFINE BUTTON Btn_HojaVida 
     LABEL "Observaciones" 
     SIZE 15 BY 1.12.

DEFINE BUTTON Btn_InfoCode 
     LABEL "Información Codeudores" 
     SIZE 36 BY 1.12.

DEFINE BUTTON Btn_RadicarJuz 
     LABEL "Radicar Juzgado" 
     SIZE 15 BY 1.12.

DEFINE BUTTON Btn_Scoring 
     LABEL "XXX" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-19 
     LABEL "i" 
     SIZE 3 BY .81
     FONT 0.

DEFINE VARIABLE Cmb_Agencias AS CHARACTER FORMAT "X(256)":U 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 30 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_PerPago AS CHARACTER FORMAT "X(256)":U 
     LABEL "Periodo" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "1 - Semanal","3 - Quincenal","4 - Mensual" 
     DROP-DOWN-LIST
     SIZE 28 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_Sistemas AS CHARACTER FORMAT "X(60)":U 
     LABEL "Sistema" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 28 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomNit AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 37 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Nom_Producto AS CHARACTER FORMAT "X(70)":U 
     LABEL "Producto de Crédito" 
     VIEW-AS FILL-IN 
     SIZE 40 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE W_ForPago AS CHARACTER FORMAT "X(45)":U 
     LABEL "Pago" 
     VIEW-AS FILL-IN 
     SIZE 28 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_TasaNominal AS DECIMAL FORMAT ">>9.999999":U INITIAL 0 
     LABEL "Nominal" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_TasaPeriodo AS DECIMAL FORMAT ">>9.999999":U INITIAL 0 
     LABEL "Periodo" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Tipo_Credito AS CHARACTER FORMAT "X(20)":U 
     LABEL "Tipo de Producto" 
     VIEW-AS FILL-IN 
     SIZE 21 BY .81
     BGCOLOR 5 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-152
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 53 BY 1.62.

DEFINE RECTANGLE RECT-293
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 52 BY 12.12.

DEFINE RECTANGLE RECT-294
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 41 BY 10.5.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Br_Admisibles FOR 
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

DEFINE QUERY B_Acuerdos FOR 
      Cobros SCROLLING.

DEFINE QUERY F_Acuerdo FOR 
      Cobros SCROLLING.

DEFINE QUERY F_Admisibles FOR 
      Garantias SCROLLING.

DEFINE QUERY F_HojaVida FOR 
      Hoja_Vida SCROLLING.

DEFINE QUERY F_Instancias FOR 
      Mov_Instancias SCROLLING.

DEFINE QUERY F_Juzgado FOR 
      Creditos SCROLLING.

DEFINE QUERY F_Solicitud FOR 
      Creditos SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Br_Admisibles
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Admisibles wWin _FREEFORM
  QUERY Br_Admisibles NO-LOCK DISPLAY
      Garantias.Identificacion_Bien FORMAT "X(12)":U
      Garantias.Nom_Bien COLUMN-LABEL "Nombre" FORMAT "X(60)":U
            WIDTH 54
      Garantias.Val_Bien FORMAT ">>,>>>,>>>,>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 94 BY 4.04
         BGCOLOR 15 FONT 4 FIT-LAST-COLUMN.

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
      TCode.TC_AgeCode  COLUMN-LABEL "Age"
 TCode.TC_NitCode  COLUMN-LABEL "Nit"
 TCode.TC_NomCode  COLUMN-LABEL "Nombre" FORMAT "X(45)"
 TCode.TC_TelCdRs  COLUMN-LABEL "Tel.Residencia"
 TCode.TC_TelCdCo  COLUMN-LABEL "Tel.Comecial"
 TCode.TC_emlCode  COLUMN-LABEL "e-mail"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 96 BY 5.92
         BGCOLOR 15 FONT 4 FIT-LAST-COLUMN.

DEFINE BROWSE Br_ConHV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_ConHV wWin _FREEFORM
  QUERY Br_ConHV NO-LOCK DISPLAY
      Hoja_Vida.Fec_Grabacion FORMAT "99/99/9999":U
      Hoja_Vida.Fec_Limite    FORMAT "99/99/9999" COLUMN-LABEL "Fec.Limite"
      Hoja_Vida.Observacion FORMAT "X(400)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 95 BY 6.73
         BGCOLOR 15 FONT 4 FIT-LAST-COLUMN.

DEFINE BROWSE Br_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Consulta wWin _FREEFORM
  QUERY Br_Consulta NO-LOCK DISPLAY
      Consulta.Num_Credito FORMAT "999999999":U COLUMN-LABEL "Num.Credito"
    Consulta.AgeCredito COLUMN-LABEL "Age"
    Consulta.Nit FORMAT "X(12)":U
    Consulta.Nombre FORMAT "X(25)"
    Consulta.Fec_Ingreso FORMAT "99/99/9999":U COLUMN-LABEL "Fecha"
    Consulta.Hor_Ingreso COLUMN-LABEL "Hora"
    Consulta.Monto COLUMN-LABEL "Sdo.Capital"
    Consulta.Vigencia COLUMN-LABEL "Vig"
    Consulta.FecAcuerdo COLUMN-LABEL "Acuerdo"
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

DEFINE BROWSE B_Acuerdos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS B_Acuerdos wWin _FREEFORM
  QUERY B_Acuerdos NO-LOCK DISPLAY
      Cobros.Nro_Cobro COLUMN-LABEL "Nro.Cobro" FORMAT "9999999999":U
      Cobros.Usuario FORMAT "X(4)":U
      Cobros.Val_Compromiso COLUMN-LABEL "Val.Compromiso" FORMAT ">>>,>>>,>>9":U
      Cobros.Fec_Compromiso COLUMN-LABEL "Compromiso" FORMAT "99/99/9999":U
      Cobros.Val_Cumplido COLUMN-LABEL "Val.Cumplido" FORMAT ">>>,>>>,>>9":U
            WIDTH 17
      Cobros.Estado FORMAT "9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 82 BY 6.19
         BGCOLOR 15 FONT 5 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Creditos
     Cmb_Instancias AT ROW 1.27 COL 10 COLON-ALIGNED
     BUTTON-1 AT ROW 1.54 COL 103
     Btn_ProIns AT ROW 2.35 COL 12
     TAct AT ROW 2.62 COL 37
     Btn_Imprimir AT ROW 3.15 COL 103
     Btn_Consulta AT ROW 4.77 COL 103
     BUTTON-2 AT ROW 17.69 COL 103
     BUTTON-4 AT ROW 20.38 COL 105
     NomUsuario AT ROW 1.27 COL 55 COLON-ALIGNED NO-LABEL
     WTUsura AT ROW 2.62 COL 89 COLON-ALIGNED
     RECT-2 AT ROW 1.27 COL 102
     RECT-3 AT ROW 17.42 COL 102
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.72 BY 21.38
         BGCOLOR 17 FONT 5.

DEFINE FRAME F_ConAcuerdo
     B_Acuerdos AT ROW 1.27 COL 2
     Btn_OutConAcuerdo AT ROW 7.73 COL 75
     "Los Acuerdos que se encuentran en rojo estan pendientes de ser pagados" VIEW-AS TEXT
          SIZE 64 BY .62 AT ROW 7.73 COL 3
          FGCOLOR 12 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 6 ROW 10.42
         SIZE 85 BY 9.42
         BGCOLOR 17 FONT 5
         TITLE "Consulta de Acuerdos".

DEFINE FRAME F_Acuerdo
     Cobros.Fec_Compromiso AT ROW 1.54 COL 25 COLON-ALIGNED
          LABEL "Fecha de Compromiso"
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     BUTTON-178 AT ROW 1.81 COL 46
     Cobros.Val_Compromiso AT ROW 2.62 COL 25 COLON-ALIGNED
          LABEL "Valor Compromiso"
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     Cobros.Usuario AT ROW 3.69 COL 25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Btn_SalAcuerdo AT ROW 3.69 COL 46
     Btn_ImpAcuerdo AT ROW 5.58 COL 46
     Cobros.Age_Recaudo AT ROW 6.38 COL 25 COLON-ALIGNED
          LABEL "Agencia que Recaudó"
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Cobros.Val_Cumplido AT ROW 7.46 COL 25 COLON-ALIGNED
          LABEL "Valor Pagado"
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 18 FGCOLOR 15 
     BUTTON-177 AT ROW 7.46 COL 46
     Cobros.Usu_Recaudo AT ROW 8.54 COL 25 COLON-ALIGNED
          LABEL "Usuario Recaudo"
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Cobros.Nro_Transaccion AT ROW 9.62 COL 25 COLON-ALIGNED
          LABEL "Transacción"
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 18 FGCOLOR 15 
     BUTTON-175 AT ROW 10.15 COL 46
     Cobros.Fec_Cumplimiento AT ROW 10.69 COL 25 COLON-ALIGNED
          LABEL "Fecha de Pago"
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 18 FGCOLOR 15 
     "Información de Cumplimientos" VIEW-AS TEXT
          SIZE 27 BY .62 AT ROW 5.58 COL 3
          FGCOLOR 7 
     RECT-296 AT ROW 5.85 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 9.08
         SIZE 62 BY 12.38
         BGCOLOR 17 FONT 5
         TITLE "Acuerdo de Pago".

DEFINE FRAME F_Solicitud
     Cmb_Agencias AT ROW 1.27 COL 9 COLON-ALIGNED
     Creditos.Nit AT ROW 1.27 COL 43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 15 
     NomNit AT ROW 1.27 COL 54 COLON-ALIGNED NO-LABEL
     BUTTON-19 AT ROW 1.27 COL 93
     Creditos.Num_Credito AT ROW 2.35 COL 9 COLON-ALIGNED
          LABEL "Núm. Credito"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Num_Solicitud AT ROW 2.35 COL 28 COLON-ALIGNED
          LABEL "Solicitud"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Nom_Producto AT ROW 2.35 COL 54 COLON-ALIGNED
     Creditos.Fec_Aprobacion AT ROW 4.23 COL 10 COLON-ALIGNED
          LABEL "Aprobacion"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Fec_Desembolso AT ROW 4.23 COL 57 COLON-ALIGNED
          LABEL "Aprobacion"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Int_Corrientes AT ROW 4.23 COL 83 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Fec_Pago AT ROW 5.04 COL 57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Int_Anticipado AT ROW 5.04 COL 83 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Monto AT ROW 5.31 COL 10 COLON-ALIGNED
          LABEL "Monto"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 15 
     Creditos.Fec_ProxLiquidacion AT ROW 5.85 COL 57 COLON-ALIGNED
          LABEL "Inicio Periodiciad"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Int_DifCobro AT ROW 5.85 COL 83 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 12 FGCOLOR 15 
     Creditos.Plazo AT ROW 6.38 COL 10 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 15 
     Creditos.Fec_UltPago AT ROW 6.65 COL 57 COLON-ALIGNED
          LABEL "Ultimo Pago"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Sdo_Capital AT ROW 6.65 COL 83 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Cuota AT ROW 7.46 COL 10 COLON-ALIGNED
          LABEL "Cuota"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 15 
     Creditos.For_Interes AT ROW 7.46 COL 24 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Vencido", 1,
"Anticipado", 2
          SIZE 18 BY .81
     Creditos.Fec_UltLiquidacion AT ROW 7.46 COL 57 COLON-ALIGNED
          LABEL "Ultima Liquidación"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Sdo_Proyectado AT ROW 7.46 COL 83 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Cuo_Pagadas AT ROW 8.27 COL 83 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3.69
         SIZE 98 BY 18.31
         BGCOLOR 17 FONT 4.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F_Solicitud
     Creditos.Tasa AT ROW 8.54 COL 10 COLON-ALIGNED
          LABEL "Efectiva"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_TasaNominal AT ROW 9.62 COL 10 COLON-ALIGNED
     W_TasaPeriodo AT ROW 9.62 COL 28 COLON-ALIGNED
     Creditos.Val_Atraso AT ROW 9.88 COL 83 COLON-ALIGNED
          LABEL "Valor Atraso"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 12 FGCOLOR 15 
     Cmb_PerPago AT ROW 10.69 COL 10 COLON-ALIGNED
     Creditos.Dias_Atraso AT ROW 10.69 COL 83 COLON-ALIGNED
          LABEL "Días de Atraso"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 12 FGCOLOR 15 
     W_ForPago AT ROW 11.5 COL 10 COLON-ALIGNED
     Creditos.Cuo_Atraso AT ROW 11.5 COL 83 COLON-ALIGNED
          LABEL "Cuotas Atrasadas"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 12 FGCOLOR 15 
     Cmb_Sistemas AT ROW 12.31 COL 10 COLON-ALIGNED
     Creditos.Provision AT ROW 12.31 COL 83 COLON-ALIGNED
          LABEL "Provisión"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_Tipo_Credito AT ROW 13.12 COL 17 COLON-ALIGNED
     Creditos.Fec_Reestructurado AT ROW 13.65 COL 83 COLON-ALIGNED
          LABEL "Fecha de Reestructuración"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Btn_InfoCode AT ROW 14.46 COL 4
     Creditos.Reestructurado AT ROW 14.73 COL 64 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Reestructurado", 1,
"No Restructurado", 2
          SIZE 31 BY .81
     Btn_GarAdm AT ROW 15.81 COL 4
     Btn_HojaVida AT ROW 16.62 COL 47
     Btn_RadicarJuz AT ROW 16.62 COL 64.43
     Btn_Scoring AT ROW 16.62 COL 82
     Btn_Acuerdo AT ROW 17.15 COL 4
     "Información del Crédito" VIEW-AS TEXT
          SIZE 21 BY 1.08 AT ROW 3.15 COL 3
          FGCOLOR 7 FONT 5
     "Otras Variables de Estudio" VIEW-AS TEXT
          SIZE 29 BY 1.08 AT ROW 3.15 COL 46
          FGCOLOR 7 FONT 5
     RECT-152 AT ROW 16.35 COL 45
     RECT-293 AT ROW 3.69 COL 45
     RECT-294 AT ROW 3.69 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3.69
         SIZE 98 BY 18.31
         BGCOLOR 17 FONT 4
         TITLE "Información del Credito".

DEFINE FRAME F_HojaVida
     BUTTON-150 AT ROW 1.27 COL 86
     Hoja_Vida.Asunto_Cumplido AT ROW 1.54 COL 4
          VIEW-AS TOGGLE-BOX
          SIZE 18 BY .81
     Hoja_Vida.Fec_Limite AT ROW 1.54 COL 41 COLON-ALIGNED
          LABEL "Fecha Limite del Compromiso"
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 15 
     Hoja_Vida.Fec_Grabacion AT ROW 1.54 COL 70 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .81
          BGCOLOR 18 FGCOLOR 15 
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

DEFINE FRAME F_Admisibles
     Br_Admisibles AT ROW 1.54 COL 3
     Garantias.Tipo_Garantia AT ROW 6.12 COL 51 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Propiedad", 1,
"Vehiculo", 2,
"Inversión", 3
          SIZE 29 BY .65
     Garantias.Nit_Aseguradora AT ROW 6.92 COL 18 COLON-ALIGNED
          LABEL "Nit Aseguradora"
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 18 FGCOLOR 15 
     BUTTON-169 AT ROW 6.92 COL 33
     Garantias.Estado AT ROW 6.92 COL 51 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Activa", 1,
"Inactiva", 2
          SIZE 17 BY .81
     Garantias.Nro_Seguro AT ROW 8 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Garantias.Fec_Creacion AT ROW 8 COL 49 COLON-ALIGNED
          LABEL "Fecha de Creación"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Garantias.Fec_Retiro AT ROW 8 COL 74 COLON-ALIGNED
          LABEL "Fecha de Retiro"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Garantias.Fec_IniSeguro AT ROW 8.81 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Garantias.Descripcion_Bien AT ROW 9.08 COL 39 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 50 BY 5.62
          BGCOLOR 15 
     Garantias.Fec_FinSeguro AT ROW 9.62 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Garantias.Val_Asegurado AT ROW 10.42 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Garantias.Nom_Impuesto AT ROW 11.77 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Garantias.Fec_VctoImpuesto AT ROW 12.58 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Btn_OutAdmisibles AT ROW 13.12 COL 90
     Garantias.Val_Impuesto AT ROW 13.38 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 18 FGCOLOR 15 
     "Información del Seguro" VIEW-AS TEXT
          SIZE 21 BY 1.08 AT ROW 5.58 COL 4
          FGCOLOR 7 FONT 5
     RECT-295 AT ROW 6.12 COL 3
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 6.65
         SIZE 98 BY 15.35
         BGCOLOR 17 FONT 4
         TITLE "Garantias Admisibles".

DEFINE FRAME F_Codeudores
     Br_Codeudores AT ROW 1.27 COL 2
     BUTTON-167 AT ROW 7.46 COL 89
     R_EstadoCode AT ROW 7.73 COL 3 NO-LABEL
     BUTTON-166 AT ROW 7.73 COL 25
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 12.85
         SIZE 98 BY 9.15
         BGCOLOR 17 FGCOLOR 0 FONT 4
         TITLE "Codeudores".

DEFINE FRAME F_InfoCliente
     S_InfoCliente AT ROW 1.27 COL 2 NO-LABEL
     BUTTON-156 AT ROW 13.92 COL 2
     BUTTON-108 AT ROW 13.92 COL 61
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 6.65
         SIZE 69 BY 15.35
         BGCOLOR 17 FONT 5
         TITLE "Información del Cliente".

DEFINE FRAME F_Agregar
     E_Agregar AT ROW 1.27 COL 2 NO-LABEL
     BUTTON-153 AT ROW 6.92 COL 48
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 26 ROW 12.04
         SIZE 57 BY 8.88
         BGCOLOR 17 
         TITLE "Texto a ser Agregado".

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

DEFINE FRAME F_Deducibles
     Br_Deducibles AT ROW 1.27 COL 2
     BUTTON-101 AT ROW 9.35 COL 45
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 10.69
         SIZE 55 BY 11.31
         BGCOLOR 17 
         TITLE "Deducibles del Producto".

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
          SIZE 78 BY 10.5
          BGCOLOR 15 
     Btn_GraInstancia AT ROW 4.5 COL 82
     Btn_AgregarTXT AT ROW 6.38 COL 82
     Btn_insVolver AT ROW 12.85 COL 82
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 6.65
         SIZE 98 BY 15.35
         BGCOLOR 17 FONT 5
         TITLE "Procesar Instancias".

DEFINE FRAME F_Juzgado
     Creditos.Nit_Juzgado AT ROW 1.54 COL 10 COLON-ALIGNED
          LABEL "Nit"
          VIEW-AS FILL-IN 
          SIZE 15.43 BY .81
          BGCOLOR 15 
     Creditos.Nom_Juzgado AT ROW 2.62 COL 10 COLON-ALIGNED
          LABEL "Nombre"
          VIEW-AS FILL-IN 
          SIZE 51.43 BY .81
          BGCOLOR 18 FGCOLOR 15 
     BUTTON-179 AT ROW 3.69 COL 39
     BUTTON-180 AT ROW 3.69 COL 57
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 7.19
         SIZE 68 BY 5.65
         BGCOLOR 17 FONT 5
         TITLE "Radicación del Juzgado".


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
         TITLE              = "Proceso de Cobros de Cartera"
         HEIGHT             = 21.38
         WIDTH              = 113.72
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
ASSIGN FRAME F_Acuerdo:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_Admisibles:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_Agregar:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_Cerradas:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_Codeudores:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_ConAcuerdo:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_ConHV:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_Consulta:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_Deducibles:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_HojaVida:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_InfoCliente:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_Instancias:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_Juzgado:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_Solicitud:FRAME = FRAME F_Creditos:HANDLE.

/* SETTINGS FOR FRAME F_Acuerdo
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME F_Acuerdo:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN Cobros.Age_Recaudo IN FRAME F_Acuerdo
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Cobros.Fec_Compromiso IN FRAME F_Acuerdo
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Cobros.Fec_Cumplimiento IN FRAME F_Acuerdo
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Cobros.Nro_Transaccion IN FRAME F_Acuerdo
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Cobros.Usuario IN FRAME F_Acuerdo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Cobros.Usu_Recaudo IN FRAME F_Acuerdo
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Cobros.Val_Compromiso IN FRAME F_Acuerdo
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Cobros.Val_Cumplido IN FRAME F_Acuerdo
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FRAME F_Admisibles
   NOT-VISIBLE                                                          */
/* BROWSE-TAB Br_Admisibles RECT-295 F_Admisibles */
ASSIGN 
       FRAME F_Admisibles:HIDDEN           = TRUE.

/* SETTINGS FOR EDITOR Garantias.Descripcion_Bien IN FRAME F_Admisibles
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET Garantias.Estado IN FRAME F_Admisibles
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Garantias.Fec_Creacion IN FRAME F_Admisibles
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Garantias.Fec_FinSeguro IN FRAME F_Admisibles
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Garantias.Fec_IniSeguro IN FRAME F_Admisibles
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Garantias.Fec_Retiro IN FRAME F_Admisibles
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Garantias.Fec_VctoImpuesto IN FRAME F_Admisibles
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Garantias.Nit_Aseguradora IN FRAME F_Admisibles
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Garantias.Nom_Impuesto IN FRAME F_Admisibles
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Garantias.Nro_Seguro IN FRAME F_Admisibles
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET Garantias.Tipo_Garantia IN FRAME F_Admisibles
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Garantias.Val_Asegurado IN FRAME F_Admisibles
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Garantias.Val_Impuesto IN FRAME F_Admisibles
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Agregar
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Agregar:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_Cerradas
   NOT-VISIBLE                                                          */
/* BROWSE-TAB Br_Cerradas TEXT-3 F_Cerradas */
ASSIGN 
       FRAME F_Cerradas:HIDDEN           = TRUE.

ASSIGN 
       Br_Cerradas:ALLOW-COLUMN-SEARCHING IN FRAME F_Cerradas = TRUE.

/* SETTINGS FOR FRAME F_Codeudores
   NOT-VISIBLE                                                          */
/* BROWSE-TAB Br_Codeudores 1 F_Codeudores */
ASSIGN 
       FRAME F_Codeudores:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_ConAcuerdo
   NOT-VISIBLE                                                          */
/* BROWSE-TAB B_Acuerdos TEXT-9 F_ConAcuerdo */
ASSIGN 
       FRAME F_ConAcuerdo:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_ConHV
   NOT-VISIBLE                                                          */
/* BROWSE-TAB Br_ConHV TEXT-5 F_ConHV */
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
                                                                        */
/* SETTINGS FOR FILL-IN NomUsuario IN FRAME F_Creditos
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN WTUsura IN FRAME F_Creditos
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Deducibles
   NOT-VISIBLE                                                          */
/* BROWSE-TAB Br_Deducibles 1 F_Deducibles */
ASSIGN 
       FRAME F_Deducibles:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_HojaVida
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_HojaVida:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN Hoja_Vida.Fec_Limite IN FRAME F_HojaVida
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FRAME F_InfoCliente
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_InfoCliente:HIDDEN           = TRUE.

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
/* SETTINGS FOR FRAME F_Juzgado
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Juzgado:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN Creditos.Nit_Juzgado IN FRAME F_Juzgado
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Creditos.Nom_Juzgado IN FRAME F_Juzgado
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FRAME F_Solicitud
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Solicitud:HIDDEN           = TRUE.

/* SETTINGS FOR COMBO-BOX Cmb_Agencias IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX Cmb_PerPago IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX Cmb_Sistemas IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Cuota IN FRAME F_Solicitud
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Cuo_Atraso IN FRAME F_Solicitud
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Cuo_Pagadas IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Dias_Atraso IN FRAME F_Solicitud
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Fec_Aprobacion IN FRAME F_Solicitud
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Fec_Desembolso IN FRAME F_Solicitud
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Fec_Pago IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Fec_ProxLiquidacion IN FRAME F_Solicitud
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Fec_Reestructurado IN FRAME F_Solicitud
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Fec_UltLiquidacion IN FRAME F_Solicitud
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Fec_UltPago IN FRAME F_Solicitud
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR RADIO-SET Creditos.For_Interes IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Int_Anticipado IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Int_Corrientes IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Int_DifCobro IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Monto IN FRAME F_Solicitud
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Nit IN FRAME F_Solicitud
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
/* SETTINGS FOR FILL-IN Creditos.Provision IN FRAME F_Solicitud
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR RADIO-SET Creditos.Reestructurado IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Sdo_Capital IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Sdo_Proyectado IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Tasa IN FRAME F_Solicitud
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Val_Atraso IN FRAME F_Solicitud
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN W_ForPago IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_TasaNominal IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_TasaPeriodo IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Tipo_Credito IN FRAME F_Solicitud
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_Admisibles
/* Query rebuild information for BROWSE Br_Admisibles
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Garantias WHERE
   Garantias.Num_Credito EQ Creditos.Num_Credito AND
   Garantias.Estado EQ 1 NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is NOT OPENED
*/  /* BROWSE Br_Admisibles */
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
     _Query            is OPENED
*/  /* BROWSE Br_Codeudores */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_ConHV
/* Query rebuild information for BROWSE Br_ConHV
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Hoja_Vida WHERE
       Hoja_Vida.Tipo      EQ 9 AND Hoja_Vida.Codigo EQ 2 AND
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

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE B_Acuerdos
/* Query rebuild information for BROWSE B_Acuerdos
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Cobros WHERE
   Cobros.Num_Credito EQ INTEGER(Creditos.Num_Credito:SCREEN-VALUE IN FRAME F_Solicitud) AND
   Cobros.Nit EQ Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE B_Acuerdos */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Acuerdo
/* Query rebuild information for FRAME F_Acuerdo
     _TblList          = "bdCentral.Cobros"
     _Query            is OPENED
*/  /* FRAME F_Acuerdo */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Admisibles
/* Query rebuild information for FRAME F_Admisibles
     _TblList          = "bdCentral.Garantias"
     _Query            is OPENED
*/  /* FRAME F_Admisibles */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_HojaVida
/* Query rebuild information for FRAME F_HojaVida
     _TblList          = "bdCentral.Hoja_Vida"
     _Query            is OPENED
*/  /* FRAME F_HojaVida */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Instancias
/* Query rebuild information for FRAME F_Instancias
     _TblList          = "bdCentral.Mov_Instancias"
     _Query            is OPENED
*/  /* FRAME F_Instancias */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Juzgado
/* Query rebuild information for FRAME F_Juzgado
     _TblList          = "bdCentral.Creditos"
     _Query            is OPENED
*/  /* FRAME F_Juzgado */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Solicitud
/* Query rebuild information for FRAME F_Solicitud
     _TblList          = "bdCentral.Creditos"
     _Query            is OPENED
*/  /* FRAME F_Solicitud */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME F_Creditos:HANDLE
       ROW             = 1
       COLUMN          = 2
       HEIGHT          = 1.35
       WIDTH           = 4
       HIDDEN          = yes
       SENSITIVE       = yes.

PROCEDURE adm-create-controls:
      CtrlFrame:NAME = "CtrlFrame":U .
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */

END PROCEDURE.

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Proceso de Cobros de Cartera */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Proceso de Cobros de Cartera */
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
            Consulta.FecAcuerdo:BGCOL IN BROWSE Br_Consulta = 10
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
            Consulta.FecAcuerdo:BGCOL IN BROWSE Br_Consulta = 14
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
            Consulta.FecAcuerdo:BGCOL IN BROWSE Br_Consulta = 12
            Consulta.AgeCredito:BGCOL IN BROWSE Br_Consulta = 12 
            Consulta.Nit:BGCOL IN BROWSE Br_Consulta = 12
            Consulta.Nombre:BGCOL IN BROWSE Br_Consulta = 12
            Consulta.Fec_Ingreso:BGCOL IN BROWSE Br_Consulta = 12
            Consulta.Hor_Ingreso:BGCOL IN BROWSE Br_Consulta = 12
            Consulta.Monto:BGCOL IN BROWSE Br_Consulta = 12
            Consulta.Vigencia:BGCOL IN BROWSE Br_Consulta = 12.
     ASSIGN Consulta.Num_Credito:FGCOL IN BROWSE Br_Consulta = 15
            Consulta.FecAcuerdo:FGCOL IN BROWSE Br_Consulta = 15
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


&Scoped-define FRAME-NAME F_Solicitud
&Scoped-define SELF-NAME Btn_Acuerdo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Acuerdo wWin
ON CHOOSE OF Btn_Acuerdo IN FRAME F_Solicitud /* Acuerdo de Pago */
DO:
  FIND FIRST Cobros WHERE
             Cobros.Nro_Cobro   EQ Consulta.NumAcuerdo AND 
             Cobros.Num_Credito EQ INTEGER(Creditos.Num_Credito:SCREEN-VALUE IN FRAME F_Solicitud) AND
             Cobros.Nit         EQ Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud NO-LOCK NO-ERROR.
  IF AVAILABLE Cobros THEN DO:
     DO WITH FRAME F_Acuerdo:
        ASSIGN Fec_Compromiso:SCREEN-VALUE = STRING(Cobros.Fec_Compromiso)
               Val_Compromiso:SCREEN-VALUE = STRING(Cobros.Val_Compromiso)
               Cobros.Usuario:SCREEN-VALUE = Cobros.Usuario
               Age_Recaudo:SCREEN-VALUE = STRING(Cobros.Age_Recaudo)
               Val_Cumplido:SCREEN-VALUE = STRING(Cobros.Val_Cumplido)
               Usu_Recaudo:SCREEN-VALUE = Cobros.Usu_Recaudo
               Nro_Transaccion:SCREEN-VALUE = STRING(Cobros.Nro_Transaccion)
               Fec_Cumplimiento:SCREEN-VALUE = STRING(Cobros.Fec_Cumplimiento).
    
     END.
  END.
  ELSE ASSIGN Cobros.Fec_Compromiso:SCREEN-VALUE IN FRAME F_Acuerdo = "?"
              Cobros.Val_Compromiso:SCREEN-VALUE IN FRAME F_Acuerdo = "0"
              Cobros.Usuario:SCREEN-VALUE = W_Usuario 
              Age_Recaudo:SCREEN-VALUE = ""
              Val_Cumplido:SCREEN-VALUE = ""
              Usu_Recaudo:SCREEN-VALUE = ""
              Nro_Transaccion:SCREEN-VALUE = ""
              Fec_Cumplimiento:SCREEN-VALUE = "".
  VIEW FRAME F_Acuerdo.
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


&Scoped-define FRAME-NAME F_Solicitud
&Scoped-define SELF-NAME Btn_GarAdm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_GarAdm wWin
ON CHOOSE OF Btn_GarAdm IN FRAME F_Solicitud /* Información Garantias Admisibles */
DO:
  HIDE FRAME F_Consultas.
  FIND FIRST Garantias WHERE
             Garantias.Nit         EQ Creditos.Nit         AND
             Garantias.Num_Credito EQ Creditos.Num_Credito AND
             Garantias.Estado EQ 1 NO-LOCK NO-ERROR.
  IF NOT AVAILABLE Garantias THEN DO:
     MESSAGE "Este crédito no tiene Garantias Admisibles" SKIP
             "Asociadas!" VIEW-AS ALERT-BOX INFORMATION.
  END.
  ELSE DO:
    DISABLE ALL WITH FRAME F_Creditos.
    ENABLE Btn_OutAdmisibles WITH FRAME F_Admisibles.
    OPEN QUERY Br_Admisibles FOR EACH Garantias WHERE
     Garantias.Num_Credito EQ Creditos.Num_Credito AND
     Garantias.Estado EQ 1 NO-LOCK INDEXED-REPOSITION.
    RUN Mostrar_Admisible.
    VIEW FRAME F_Admisibles.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Instancias
&Scoped-define SELF-NAME Btn_GraInstancia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_GraInstancia wWin
ON CHOOSE OF Btn_GraInstancia IN FRAME F_Instancias /* Grabar */
DO:
  IF Mov_Instancias.Descripcion:SCREEN-VALUE IN FRAME F_Instancias EQ "" THEN DO:
     MESSAGE "No se puede grabar si no se ha entrado" SKIP
             "el concepto de la instancia. Entre el Concepto!" VIEW-AS ALERT-BOX ERROR.
     APPLY "entry" TO Mov_Instancias.Descripcion IN FRAME F_Instancias.
     RETURN NO-APPLY.
  END.
  FIND Mov_Instancias WHERE 
       Mov_Instancias.Instancia EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) AND
       Mov_Instancias.Nit       EQ STRING(Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud) AND
       Mov_Instancias.Cuenta    EQ STRING(DECIMAL(Creditos.Num_Credito:SCREEN-VALUE IN FRAME F_Solicitud)) AND
       Mov_Instancias.Num_Solicitud EQ INTEGER(Creditos.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud) 
       NO-ERROR.  
  IF AVAILABLE Mov_Instancias THEN DO:
    ASSIGN FRAME F_Instancias
                 Mov_Instancias.Descripcion.
    FIND TCerradas WHERE TCerradas.Instancia      EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) AND
                         TCerradas.Num_Solicitud  EQ INTEGER(Creditos.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud) NO-ERROR.
    IF AVAILABLE TCerradas THEN 
       ASSIGN TCerradas.Descripcion = Mov_Instancias.Descripcion:SCREEN-VALUE IN FRAME F_Instancias.
    
    ASSIGN Mov_Instancias.Fec_Retiro = ?
           Mov_Instancias.Hora_Retiro = 0.
    IF Mov_Instancias.Estado:SCREEN-VALUE IN FRAME F_Instancias EQ "YES" THEN DO:
      FIND FIRST Hoja_Vida WHERE 
                 Hoja_Vida.Tipo       EQ 9 AND Hoja_Vida.Codigo EQ 1 AND 
                 Hoja_Vida.Instancia  EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) AND
                 Hoja_Vida.DoctoRefer EQ INTEGER(Creditos.Num_Credito:SCREEN-VALUE IN FRAME F_Solicitud) AND
                 Hoja_Vida.Nit        EQ Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud AND
                 Hoja_Vida.Asunto_Cumplido EQ NO AND
                 Hoja_Vida.Usuario   EQ W_Usuario NO-ERROR.     
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
         RUN Asignar_Proxima_Instancia NO-ERROR.
         IF ERROR-STATUS:ERROR THEN DO: 
            UNDO.
         END.
         FIND Mov_Instancias WHERE 
             Mov_Instancias.Instancia EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) AND
             Mov_Instancias.Nit       EQ STRING(Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud) AND
             Mov_Instancias.Num_Solicitud EQ INTEGER(Creditos.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud) AND
             Mov_Instancias.Usuario   EQ W_Usuario
             NO-ERROR.  
         IF AVAILABLE Mov_Instancias THEN
            ASSIGN Mov_Instancias.Fec_Retiro = TODAY
                   Mov_Instancias.Hora_Retiro = TIME
                   Mov_Instancias.Estado = YES
                   Mov_Instancias.Agencia = INTEGER(SUBSTRING(Cmb_Agencias:SCREEN-VALUE IN FRAME F_Solicitud,1,3)).
          APPLY "value-changed" TO Cmb_Instancias IN FRAME F_Creditos.
          RELEASE Solicitud.
      END.
      HIDE FRAME F_Instancias.
/*      ASSIGN Mov_Instancias.Fec_Retiro = W_Fecha
             Mov_Instancias.Hora_Retiro = TIME
             Mov_Instancias.Estado = YES.
      APPLY "choose" TO Btn_Consulta IN FRAME F_Creditos.
      ENABLE ALL WITH FRAME F_Solicitud.*/
        /*devuelve el credito para taquilla*/
    END.
  END.                      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Solicitud
&Scoped-define SELF-NAME Btn_HojaVida
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_HojaVida wWin
ON CHOOSE OF Btn_HojaVida IN FRAME F_Solicitud /* Observaciones */
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
            Hoja_Vida.Fec_Grabacion:SCREEN-VALUE   = STRING(Hoja_Vida.Fec_Grabacion)
            Hoja_Vida.Fec_Limite:SCREEN-VALUE      = STRING(Hoja_Vida.Fec_Limite).
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


&Scoped-define FRAME-NAME F_Creditos
&Scoped-define SELF-NAME Btn_Imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imprimir wWin
ON CHOOSE OF Btn_Imprimir IN FRAME F_Creditos /* Button 8 */
DO:
  DEFINE VAR Listado     AS CHARACTER INITIAL "".
  Listado = W_PathSpl + "Proyeccion.LST".
  {INCLUIDO\Imprimir.I "Listado"}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Solicitud
&Scoped-define SELF-NAME Btn_InfoCode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_InfoCode wWin
ON CHOOSE OF Btn_InfoCode IN FRAME F_Solicitud /* Información Codeudores */
DO:
  HIDE FRAME F_Consultas.
  puntero = ROWID(Clientes).
  FOR EACH TCode: DELETE TCode. END.
IF AVAILABLE Creditos THEN DO:
  FOR EACH Relaciones WHERE 
           Relaciones.Nit            EQ Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud  AND
           Relaciones.Cuenta         EQ Creditos.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud AND
           Relaciones.Clase_Producto EQ 2 AND
           Relaciones.Cod_Producto   EQ INTEGER(SUBSTRING(Nom_Producto:SCREEN-VALUE IN FRAME F_Solicitud,1,3)) AND
           Relaciones.Cod_Relacion   EQ 11:
      FIND Clientes WHERE Clientes.Nit EQ Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
      IF AVAILABLE Clientes THEN DO:
        CREATE TCode.
        ASSIGN TCode.TC_AgeCode = Clientes.Agencia
               TCode.TC_NitCode = Clientes.Nit
               TCode.TC_NitDeud = Relaciones.Nit
               TCode.TC_NumSoli = DECIMAL(Relaciones.Cuenta)
               TCode.TC_NomCode = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2
               TCode.TC_TelCdRs = Clientes.Tel_Residencia
               TCode.TC_TelCdCo = Clientes.Tel_Comercial
               TCode.TC_EmlCode = Clientes.email
               TCode.TC_EstRela = Relaciones.Estado
               TCode.TC_FecCrea = Relaciones.Fec_Ingreso
               TCode.TC_FecReti = Relaciones.Fec_Inactividad.
      END.
  END.
  FIND FIRST TCode NO-LOCK NO-ERROR.
  IF NOT AVAILABLE TCode THEN DO:
     MESSAGE "El crédito no tiene ningún Codeudor Asociado" VIEW-AS ALERT-BOX INFORMATION.
  END.
  ELSE DO:
    DISABLE ALL WITH FRAME F_Creditos.
    ENABLE ALL WITH FRAME F_Codeudores.
    OPEN QUERY Br_Codeudores FOR EACH TCode WHERE TCode.TC_EstRela EQ 1 NO-LOCK INDEXED-REPOSITION.
    VIEW FRAME F_Codeudores.
  END.
END.
ELSE DO:
  MESSAGE "No hay una solicitud disponible" VIEW-AS ALERT-BOX INFORMATION.
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
  ENABLE Hoja_Vida.Fec_Limite Btn_SalvaHV Hoja_Vida.Asunto_Cumplido Hoja_Vida.Observacion WITH FRAME F_HojaVida.
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


&Scoped-define FRAME-NAME F_Admisibles
&Scoped-define SELF-NAME Btn_OutAdmisibles
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OutAdmisibles wWin
ON CHOOSE OF Btn_OutAdmisibles IN FRAME F_Admisibles /* Button 168 */
DO:
  FIND Clientes WHERE ROWID(Clientes) EQ puntero NO-ERROR.
  ENABLE ALL WITH FRAME F_Creditos.
  DISABLE NomUsuario WTUsura WITH FRAME F_Creditos.
  HIDE FRAME F_Admisibles.
  FRAME F_Consulta:HIDDEN = YES.
  FIND Clientes WHERE ROWID(Clientes) EQ puntero NO-ERROR.
  APPLY "entry" TO Btn_HojaVida IN FRAME F_Solicitud.
  RETURN NO-APPLY. 
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
          Mov_Instancias.Usuario EQ TCerradas.Usuario NO-ERROR.
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


&Scoped-define FRAME-NAME F_ConAcuerdo
&Scoped-define SELF-NAME Btn_OutConAcuerdo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OutConAcuerdo wWin
ON CHOOSE OF Btn_OutConAcuerdo IN FRAME F_ConAcuerdo /* Button 179 */
DO:
DO WITH FRAME F_Acuerdo:
  IF AVAILABLE Cobros AND BROWSE B_Acuerdos:NUM-SELECTED-ROWS NE 0 THEN
    ASSIGN Fec_Compromiso:SCREEN-VALUE = STRING(Cobros.Fec_Compromiso)
           Val_Compromiso:SCREEN-VALUE = STRING(Cobros.Val_Compromiso)
           Usuario:SCREEN-VALUE = Cobros.Usuario
           Age_Recaudo:SCREEN-VALUE = STRING(Cobros.Age_Recaudo)
           Val_Cumplido:SCREEN-VALUE = STRING(Cobros.Val_Cumplido)
           Usu_Recaudo:SCREEN-VALUE = Cobros.Usu_Recaudo
           Nro_Transaccion:SCREEN-VALUE = STRING(Cobros.Nro_Transaccion)
           Fec_Cumplimiento:SCREEN-VALUE = STRING(Cobros.Fec_Cumplimiento).
END.
  HIDE FRAME F_ConAcuerdo.
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
            Hoja_Vida.Fec_Grabacion:SCREEN-VALUE   = STRING(Hoja_Vida.Fec_Grabacion)
            Hoja_Vida.Fec_Limite:SCREEN-VALUE      = STRING(Hoja_Vida.Fec_Limite).
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
     FIND Creditos WHERE Creditos.Num_Credito EQ Consulta.Num_Credito AND
          Creditos.Nit EQ Consulta.Nit.
     IF LOCKED Creditos THEN DO:
        MESSAGE "El Crédito esta siendo accesada por otro" SKIP
                "asesor. por este motivo no podra ser modificada"
                VIEW-AS ALERT-BOX INFORMATION.
        APPLY "choose" TO Btn_Consulta IN FRAME F_Creditos.
     END.
     ELSE DO:
       IF AVAILABLE Creditos THEN  
          RUN Mostrar_Credito.
       ELSE
          MESSAGE "Creditos No disponible" VIEW-AS ALERT-BOX ERROR.
     END.
     HIDE FRAME F_Consulta.
     VIEW FRAME F_Solicitud.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Creditos
&Scoped-define SELF-NAME Btn_ProIns
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_ProIns wWin
ON CHOOSE OF Btn_ProIns IN FRAME F_Creditos /* Procesar Instancia */
DO:
 IF FRAME F_Solicitud:HIDDEN EQ YES OR Br_Consulta:NUM-ENTRIES IN FRAME F_Consulta EQ 0 THEN DO:
    MESSAGE "Al momento no existe ningun crédito a la cual" SKIP
            "se le pueda procesar la instancia." VIEW-AS ALERT-BOX INFORMATION.
    APPLY "entry" TO Cmb_Instancias IN FRAME F_Creditos.
    RETURN NO-APPLY.
 END.
 HIDE FRAME F_Consultas.
 DISABLE ALL WITH FRAME F_Creditos.
 IF AVAILABLE Creditos THEN DO:
    /*RUN Abogados_Disponibles.*/
    FIND Mov_Instancias WHERE 
         Mov_Instancias.Instancia EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) AND
         Mov_Instancias.Nit       EQ STRING(Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud) AND
         Mov_Instancias.Cuenta    EQ STRING(DECIMAL(Creditos.Num_Credito:SCREEN-VALUE IN FRAME F_Solicitud)) AND
         Mov_Instancias.Num_Solicitud EQ INTEGER(Creditos.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud) AND
         Mov_Instancias.Estado    EQ NO NO-ERROR.
    IF AVAILABLE Mov_Instancias THEN DO:
       DO WITH FRAME F_Instancias:
          ASSIGN Mov_Instancias.Estado:SCREEN-VALUE = STRING(Mov_Instancias.Estado)
                 Mov_Instancias.Fec_Ingreso:SCREEN-VALUE = STRING(Mov_Instancias.Fec_Ingreso)
                 WHora_Ingreso:SCREEN-VALUE = STRING(Mov_Instancia.Hora_Ingreso,"HH:MM:SS AM")
                 Mov_Instancia.Descripcion:SCREEN-VALUE = Mov_Instancia.Descripcion
                 Mov_Instancias.Fec_Retiro:SCREEN-VALUE = ""
                 W_instancia:SCREEN-VALUE = Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos
                 W_UsuarioInstancia:SCREEN-VALUE = NomUsuario:SCREEN-VALUE IN FRAME F_Creditos.
          Vigencia:SCREEN-VALUE = STRING(TODAY - Fec_Ingreso) + " Dias".
          ENABLE Mov_Instancias.Estado Mov_Instancias.Descripcion
                 Btn_GraInstancia Btn_InsVolver WITH FRAME F_Instancias.
          RUN Buscar_Instancias_Cerradas.
       END.
       VIEW FRAME F_Instancias.
    END.
    ELSE DO:
      MESSAGE "El Crédito no esta disponible o no" SKIP
              "se ha cerrado la instancia anterior" SKIP
              "escoja un Crédito de la lista de " SKIP
              "Créditos disponibles!" VIEW-AS ALERT-BOX WARNING.
      ENABLE ALL WITH FRAME F_Creditos.
      DISABLE NomUsuario WITH FRAME F_Creditos.
    END.
 END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Solicitud
&Scoped-define SELF-NAME Btn_RadicarJuz
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_RadicarJuz wWin
ON CHOOSE OF Btn_RadicarJuz IN FRAME F_Solicitud /* Radicar Juzgado */
DO:
  VIEW FRAME F_Juzgado.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Acuerdo
&Scoped-define SELF-NAME Btn_SalAcuerdo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_SalAcuerdo wWin
ON CHOOSE OF Btn_SalAcuerdo IN FRAME F_Acuerdo /* Salvar Acuerdo */
DO:
  CREATE Cobros.
  ASSIGN Cobros.Agencia           = INTEGER(SUBSTRING(Cmb_Agencias:SCREEN-VALUE IN FRAME F_Solicitud,1,3))
         Cobros.Estado            = 1
         Cobros.Fec_Compromiso    = DATE(Cobros.Fec_Compromiso:SCREEN-VALUE IN FRAME F_Acuerdo)
         Cobros.Fec_Acuerdo       = W_Fecha
         Cobros.Nit               = Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud
         Cobros.Nro_cobro         = NEXT-VALUE(Sec_NumCobro)
         Cobros.Num_Credito       = DECIMAL(Creditos.Num_Credito:SCREEN-VALUE IN FRAME F_Solicitud)
         Cobros.Usuario           = W_Usuario
         Cobros.Val_Compromiso    = DECIMAL(Val_Compromiso:SCREEN-VALUE IN FRAME F_Acuerdo).
         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_HojaVida
&Scoped-define SELF-NAME Btn_SalvaHV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_SalvaHV wWin
ON CHOOSE OF Btn_SalvaHV IN FRAME F_HojaVida /* Salvar */
DO:
  DISABLE Btn_SalvaHV Hoja_Vida.Fec_Limite WITH FRAME F_HojaVida.
  ENABLE  Btn_NvoHV WITH FRAME F_HojaVida.
  IF Hoja_Vida.Fec_Limite:SCREEN-VALUE  IN FRAME F_HojaVida = "?" OR 
     DATE(Hoja_Vida.Fec_Limite:SCREEN-VALUE IN FRAME F_HojaVida) LE TODAY  THEN DO:
     MESSAGE "El compromiso debe tener una fecha limite de cumplimiento" SKIP
             "y esta fecha debe ser mayor a la fecha de hoy" SKIP
             "entre la fecha y grabe!" VIEW-AS ALERT-BOX.
     APPLY "entry" TO Hoja_Vida.Fec_Limite IN FRAME F_HojaVida.
     RETURN NO-APPLY.
  END.
  IF W_NvaHv THEN DO:
     CREATE Hoja_Vida.
     ASSIGN Hoja_Vida.Tipo = 9 
            Hoja_Vida.Codigo = 2  
            Hoja_Vida.Fec_Limite = DATE(Hoja_Vida.Fec_Limite:SCREEN-VALUE IN FRAME F_HojaVida)
            Hoja_Vida.Instancia  = INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5))
            Hoja_Vida.DoctoRefer = INTEGER(Creditos.Num_Credito:SCREEN-VALUE IN FRAME F_Solicitud)
            Hoja_Vida.Nit        = Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud
            Hoja_Vida.Usuario    = W_Usuario
            Hoja_Vida.Fec_Grabacion = TODAY
            Hoja_Vida.Hora_Grabacion = TIME.
     W_NvaHv = NO.
     Hoja_Vida.Observacion:READ-ONLY = YES.
  END.
  ELSE FIND CURRENT Hoja_Vida EXCLUSIVE-LOCK.
  ASSIGN FRAME F_HojaVida Hoja_Vida.Asunto_Cumplido Hoja_Vida.Observacion.
  IF AVAILABLE Hoja_Vida AND Hoja_Vida.Asunto_Cumplido THEN
     APPLY "choose" TO Btn_HojaVida IN FRAME F_Solicitud.
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


&Scoped-define FRAME-NAME F_InfoCliente
&Scoped-define SELF-NAME BUTTON-108
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-108 wWin
ON CHOOSE OF BUTTON-108 IN FRAME F_InfoCliente /* Button 108 */
DO:
  ENABLE ALL WITH FRAME F_Creditos.
  DISABLE NomUsuario WTUsura WITH FRAME F_Creditos.
  HIDE FRAME F_InfoCliente.
  FRAME F_Consulta:HIDDEN = YES.
  APPLY "entry" TO Btn_HojaVida IN FRAME F_Solicitud.
  RETURN NO-APPLY.
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
       Hoja_Vida.Tipo      EQ 9 AND Hoja_Vida.Codigo EQ 2 AND 
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


&Scoped-define FRAME-NAME F_InfoCliente
&Scoped-define SELF-NAME BUTTON-156
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-156 wWin
ON CHOOSE OF BUTTON-156 IN FRAME F_InfoCliente /* Ver Información Detallada */
DO:
  RUN W-ConsultaGeneral.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Codeudores
&Scoped-define SELF-NAME BUTTON-166
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-166 wWin
ON CHOOSE OF BUTTON-166 IN FRAME F_Codeudores /* Ver Información Detallada del Codeudor Seleccionado */
DO:
   IF BROWSE Br_Codeudores:NUM-SELECTED-ROWS NE 0 THEN DO:
     ENABLE ALL WITH FRAME F_InfoCliente.
     FIND Clientes WHERE Clientes.Nit EQ TCode.TC_NitCode NO-LOCK NO-ERROR.
     RUN Llenar_InfoCliente.
     FRAME F_InfoCliente:TITLE = "Información del Codeudor: " + TCode.TC_NomCode.
     VIEW FRAME F_InfoCliente.  
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-167
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-167 wWin
ON CHOOSE OF BUTTON-167 IN FRAME F_Codeudores /* Button 167 */
DO:
  FIND Clientes WHERE ROWID(Clientes) EQ puntero NO-ERROR.
  ENABLE ALL WITH FRAME F_Creditos.
  DISABLE NomUsuario WTUsura WITH FRAME F_Creditos.
  HIDE FRAME F_Codeudores.
  FRAME F_Consulta:HIDDEN = YES.
  APPLY "entry" TO Btn_HojaVida IN FRAME F_Solicitud.
  RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Admisibles
&Scoped-define SELF-NAME BUTTON-169
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-169 wWin
ON CHOOSE OF BUTTON-169 IN FRAME F_Admisibles /* i */
DO:
  IF AVAILABLE Clientes AND Garantias.Nit_Aseguradora:SCREEN-VALUE IN FRAME F_Admisibles NE "" THEN DO:
    DISABLE ALL WITH FRAME F_Creditos.
    ENABLE ALL WITH FRAME F_InfoCliente.
    puntero = ROWID(Clientes).
    FIND Clientes WHERE Clientes.Nit EQ Garantias.Nit_Aseguradora:SCREEN-VALUE IN FRAME F_Admisibles NO-LOCK NO-ERROR.
    RUN Llenar_InfoCliente.
    FRAME F_InfoCliente:TITLE = "Información de la Aseguradora: " + Clientes.Nombre.
    VIEW FRAME F_InfoCliente.
  END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Acuerdo
&Scoped-define SELF-NAME BUTTON-175
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-175 wWin
ON CHOOSE OF BUTTON-175 IN FRAME F_Acuerdo /* Button 175 */
DO:
  HIDE FRAME F_Acuerdo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-177
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-177 wWin
ON CHOOSE OF BUTTON-177 IN FRAME F_Acuerdo /* Button 177 */
DO:
  VIEW FRAME F_ConAcuerdo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-178
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-178 wWin
ON CHOOSE OF BUTTON-178 IN FRAME F_Acuerdo /* Crear Nuevo */
DO:
  ASSIGN Cobros.Fec_Compromiso:SCREEN-VALUE IN FRAME F_Acuerdo = "?"
         Cobros.Val_Compromiso:SCREEN-VALUE IN FRAME F_Acuerdo = "0"
         Usuario:SCREEN-VALUE = W_Usuario 
         Age_Recaudo:SCREEN-VALUE = ""
         Val_Cumplido:SCREEN-VALUE = ""
         Usu_Recaudo:SCREEN-VALUE = ""
         Nro_Transaccion:SCREEN-VALUE = ""
         Fec_Cumplimiento:SCREEN-VALUE = "".
  APPLY "Entry" TO Cobros.Fec_Compromiso IN FRAME F_Acuerdo.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Juzgado
&Scoped-define SELF-NAME BUTTON-179
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-179 wWin
ON CHOOSE OF BUTTON-179 IN FRAME F_Juzgado /* Salvar Juzgado */
DO:
  FIND Creditos WHERE Creditos.Num_Credito EQ DECIMAL(Creditos.Num_Credito:SCREEN-VALUE IN FRAME F_Solicitud) AND
       Creditos.Nit EQ Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud NO-ERROR.
  ASSIGN Creditos.Nit_Juzgado = Creditos.Nit_Juzgado:SCREEN-VALUE IN FRAME F_Juzgado
         Creditos.Nom_Juzgado = Creditos.Nit_Juzgado:SCREEN-VALUE IN FRAME F_Juzgado.
  HIDE FRAME F_Juzgado.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-180
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-180 wWin
ON CHOOSE OF BUTTON-180 IN FRAME F_Juzgado /* Button 180 */
DO:
  HIDE FRAME F_Juzgado.
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
    FRAME F_InfoCliente:TITLE = "Información del Cliente".
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


&Scoped-define BROWSE-NAME B_Acuerdos
&Scoped-define FRAME-NAME F_ConAcuerdo
&Scoped-define SELF-NAME B_Acuerdos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_Acuerdos wWin
ON MOUSE-SELECT-DBLCLICK OF B_Acuerdos IN FRAME F_ConAcuerdo
DO:
  APPLY "choose" TO Btn_OutConAcuerdo IN FRAME F_ConAcuerdo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_Acuerdos wWin
ON ROW-DISPLAY OF B_Acuerdos IN FRAME F_ConAcuerdo
DO:
  IF Cobros.Estado EQ 1 THEN
     ASSIGN Nro_Cobro:fgcol in browse b_Acuerdos = 12
            Fec_Compromiso:fgcol in browse b_Acuerdos = 12
            Val_Compromiso:fgcol in browse b_Acuerdos = 12
            Usuario:fgcol in browse b_Acuerdos = 12
            Val_Cumplido:fgcol in browse b_Acuerdos = 12
            Estado:fgcol in browse b_Acuerdos = 12.
  ELSE
     ASSIGN Nro_Cobro:fgcol in browse b_Acuerdos = 0
            Fec_Compromiso:fgcol in browse b_Acuerdos = 0
            Val_Compromiso:fgcol in browse b_Acuerdos = 0
            Usuario:fgcol in browse b_Acuerdos = 0
            Val_Cumplido:fgcol in browse b_Acuerdos = 0
            Estado:fgcol in browse b_Acuerdos = 0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Solicitud
&Scoped-define SELF-NAME Cmb_Agencias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Agencias wWin
ON LEAVE OF Cmb_Agencias IN FRAME F_Solicitud /* Agencia */
DO:
IF W_Nuevo THEN DO:
  FIND Cfg_Instancias WHERE
       Cfg_instancias.Tipo_Instancia EQ 5 AND
       Cfg_Instancias.Agencia   EQ INTEGER(SUBSTRING(SELF:SCREEN-VALUE IN FRAME F_Solicitud,1,3)) AND
       Cfg_Instancias.Instancia EQ W_Primera AND
       Cfg_Instancias.Usuario   EQ W_Usuario AND
       Cfg_Instancias.Estado    EQ 1 NO-LOCK NO-ERROR.
  IF NOT AVAILABLE Cfg_instancias THEN DO:
     MESSAGE "El usuario no tiene configurada la instancia" SKIP(1)
             Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos SKIP(1)
             "En la Agencia: " SELF:SCREEN-VALUE VIEW-AS ALERT-BOX ERROR.
     W_Nuevo = NO.
     HIDE FRAME F_Solicitud.
     APPLY "choose" TO Btn_Consulta IN FRAME F_Creditos.
  END.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Creditos
&Scoped-define SELF-NAME Cmb_Instancias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Instancias wWin
ON VALUE-CHANGED OF Cmb_Instancias IN FRAME F_Creditos /* Instancias */
DO:
  FIND Instancias WHERE Instancias.Instancia EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) NO-LOCK NO-ERROR.
  IF AVAILABLE Instancias THEN DO:
     ASSIGN W_VigIns = Instancias.TMI
            VG_Normal:SCREEN-VALUE IN FRAME F_Consulta  = "Normal hasta: " + STRING(Instancias.TMI / 3) + " Dias"
            VG_Media:SCREEN-VALUE IN FRAME F_Consulta   = "Media desde: " + STRING((Instancias.TMI / 3) + 1) + " Hasta : "
                                                          + STRING((Instancias.TMI / 3) * 2) + " Días"
            VG_Alta:SCREEN-VALUE IN FRAME F_Consulta  = "Alta desde: " + 
                                                         STRING(((Instancias.TMI / 3) * 2) + 1) + " Hasta " +
                                                          STRING(Instancias.TMI) + " Días".
  END.
  RUN Creditos_X_Instancia.
  HIDE FRAME F_Solicitud.
  VIEW FRAME F_Consulta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Solicitud
&Scoped-define SELF-NAME Cmb_PerPago
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_PerPago wWin
ON VALUE-CHANGED OF Cmb_PerPago IN FRAME F_Solicitud /* Periodo */
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
IF TAct:SCREEN-VALUE IN FRAME F_Creditos EQ "YES" THEN DO:
  IF FRAME F_Consulta:HIDDEN EQ NO THEN
     APPLY "value-changed" TO Cmb_Instancias IN FRAME F_Creditos.
END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Instancias
&Scoped-define SELF-NAME Mov_Instancias.Estado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Mov_Instancias.Estado wWin
ON VALUE-CHANGED OF Mov_Instancias.Estado IN FRAME F_Instancias /* Estado */
DO:
  APPLY "choose" TO Btn_AgregarTXT IN FRAME F_Instancias.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Solicitud
&Scoped-define SELF-NAME Creditos.Monto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Creditos.Monto wWin
ON LEAVE OF Creditos.Monto IN FRAME F_Solicitud /* Monto */
DO:
 DO WITH FRAME F_Solicitud:
  RUN Multiplicador_Ahorros. 
  RUN Calcular_Deducible (INPUT DECIMAL(SELF:SCREEN-VALUE)).
 END.  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Juzgado
&Scoped-define SELF-NAME Creditos.Nit_Juzgado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Creditos.Nit_Juzgado wWin
ON LEAVE OF Creditos.Nit_Juzgado IN FRAME F_Juzgado /* Nit */
DO:
DO WITH FRAME F_Juzgado:
  IF SELF:SCREEN-VALUE EQ "" THEN DO:
     MESSAGE "Debe Entrarse el número de indentificación" SKIP
             "del juzgado. Rectifique!!" SKIP(1)
             "Desea abrir la Consulta de Clientes?" VIEW-AS ALERT-BOX ERROR BUTTONS YES-NO UPDATE choice.
     IF choice THEN DO:
       RUN C-Clientes.R(INPUT 1,INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_AgeCli).
       ASSIGN Creditos.Nom_Juzgado:SCREEN-VALUE = P_Nombre + " " + P_Apellido
              SELF:SCREEN-VALUE   = P_Nit.
     END.
  END.
  ELSE DO:
    FIND Clientes WHERE Clientes.Nit EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF AVAILABLE Clientes THEN 
       Creditos.Nom_Juzgado:SCREEN-VALUE = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
    ELSE DO:
       RUN C-Clientes.R(INPUT 1,INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_AgeCli).
       ASSIGN Creditos.Nom_Juzgado:SCREEN-VALUE = P_Nombre + " " + P_Apellido
              SELF:SCREEN-VALUE   = P_Nit.
       FIND Clientes WHERE Clientes.Nit EQ P_Nit NO-LOCK NO-ERROR.
    END.
  END.
END.
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
 IF Creditos.Plazo:SCREEN-VALUE NE "0" THEN RUN Buscar_Indicadores.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Codeudores
&Scoped-define SELF-NAME R_EstadoCode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_EstadoCode wWin
ON VALUE-CHANGED OF R_EstadoCode IN FRAME F_Codeudores
DO:
  IF SELF:SCREEN-VALUE EQ "1" THEN
    OPEN QUERY Br_Codeudores FOR EACH TCode WHERE TCode.TC_EstRela EQ 1 NO-LOCK INDEXED-REPOSITION.
  ELSE
    OPEN QUERY Br_Codeudores FOR EACH TCode WHERE TCode.TC_EstRela EQ 2 NO-LOCK INDEXED-REPOSITION.
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


&Scoped-define FRAME-NAME F_Acuerdo
&Scoped-define BROWSE-NAME Br_Admisibles
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Asignar_Proxima_Instancia wWin 
PROCEDURE Asignar_Proxima_Instancia :
DEFINE VAR W_Ord           LIKE Instancias.Orden_Instancia.
DEFINE VAR W_OrdSiguiente  LIKE Instancias.Orden_Instancia.
IF Solicitud.Estado EQ 1 THEN DO:
  FIND FIRST Mov_Instancias WHERE
             Mov_Instancias.Instancia     NE INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) AND
             Mov_Instancias.Num_Solicitud EQ INTEGER(Creditos.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud) AND
             Mov_instancias.Nit           EQ Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud AND
             Mov_Instancias.Estado        EQ NO NO-LOCK NO-ERROR.
  IF AVAILABLE Mov_Instancias THEN DO:
     FIND  Instancias WHERE
           Instancias.Tipo_Instancia  EQ 1 AND
           Instancias.Instancia       EQ Mov_Instancias.Instancia AND
           Instancias.Tipo_Producto   EQ 2 AND
           Instancias.Estado          EQ 1 NO-LOCK NO-ERROR.
     IF AVAILABLE Instancias AND Instancias.Orden_Instancia LT Orden_InsPan THEN DO:
        MESSAGE "Para pasar a la siguiente instancia" SKIP
                "deben estar cerradas las instancias" SKIP
                "anteriores. al momento se encuentra" SKIP
                "abierta la instancia: " Mov_Instancias.Instancia SKIP
                "Vigente en el Usuario: " Mov_Instancias.Usuario
                 VIEW-AS ALERT-BOX ERROR.
        ENABLE ALL WITH FRAME F_Creditos.
        DISABLE NomUsuario WITH FRAME F_Creditos.
        ENABLE ALL WITH FRAME F_Consulta.
        OPEN QUERY Br_Consulta FOR EACH Consulta
                   NO-LOCK  BY Consulta.Num_Solicitud INDEXED-REPOSITION.
        Buscar:LABEL IN FRAME F_Consulta = "Buscar Solicitud".
       RETURN ERROR.
     END.
  END.
  DO:
    FIND Instancias WHERE 
         Instancias.Tipo_Instancia EQ 1 AND
         Instancias.Instancia      EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) AND
         Instancias.Tipo_Producto  EQ 2 AND
         Instancias.Estado         EQ 1 NO-LOCK NO-ERROR.
    IF AVAILABLE Instancias THEN DO:
       W_ord = Instancias.Orden_Instancia.
       FIND NEXT Instancias WHERE
                 Instancias.Tipo_Instancia  EQ 1 AND
                 Instancias.Orden_Instancia GT W_Ord AND
                 Instancias.Tipo_Producto   EQ 2 AND
                 Instancias.Estado          EQ 1 USE-INDEX idx_orden NO-LOCK NO-ERROR.
       IF AVAILABLE Instancias THEN DO: 
          FOR EACH TProIns: DELETE TProIns. END.
          /*Encuentra los usuarios configurados para las instancias siguientes, y que cumplen los limites*/
          FOR EACH Cfg_Instancias WHERE 
                   Cfg_Instancias.Agencia        EQ INTEGER(SUBSTRING(Cmb_Agencias:SCREEN-VALUE IN FRAME F_Solicitud,1,3)) AND
                   Cfg_Instancias.Tipo_Instancia EQ Instancias.Tipo_Instancia AND
                   Cfg_Instancias.Orden          EQ Instancias.Orden_Instancia AND
                   Cfg_Instancias.Usuario        EQ W_Usuario AND
                   Cfg_Instancias.Plazo_Minimo   LE Dias AND
                   Cfg_Instancias.Plazo_Maximo   GE Dias AND
                   Cfg_Instancias.Monto_Minimo   LE Creditos.Monto AND
                   Cfg_Instancias.Monto_Maximo   GE Creditos.Monto AND 
                   Cfg_Instancias.Estado         EQ 1
                   NO-LOCK:
               CREATE TProIns.
               ASSIGN TProIns.TP_Orden     = Cfg_Instancias.Orden
                      TProIns.TP_Instancia = Cfg_Instancias.Instancia
                      TProIns.TP_Usuario   = Cfg_Instancias.Usuario
                      TProIns.Tp_Cantidad  = 0
                      TProIns.TP_Agencia   = Cfg_Instancias.Agencia.
               FIND Instancias WHERE Instancias.Instancia EQ Cfg_Instancias.Instancia NO-LOCK NO-ERROR.
               IF AVAILABLE Instancias THEN TProIns.TP_NomInstan = Instancias.Nom_Instancia.
               ELSE TProIns.TP_NomInstan = "Instancia No encontrada".
               FIND Usuarios WHERE Usuarios.Usuario EQ Cfg_Instancias.Usuario NO-LOCK NO-ERROR.
               IF AVAILABLE Usuarios THEN TProIns.TP_NomUsuar  = Usuario.Nombre.
               ELSE TProIns.TP_NomUsuar  = "Usuario No encontrado".
          END.
          FIND FIRST TProIns NO-ERROR.
          IF NOT AVAILABLE TProIns THEN DO:
             MESSAGE "No se encontro ningún Usuario" SKIP
                     "Para asignar la solicitud a la" SKIP
                     "Proxima instancia." VIEW-AS ALERT-BOX.
             FIND Mov_Instancias WHERE 
                  Mov_Instancias.Instancia     EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) AND
                  Mov_Instancias.Num_Solicitud EQ Solicitud.Num_Solicitud NO-LOCK NO-ERROR.
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
          /*Por cada usuario encuentra el numero de creditos que esta procesando para las instancias a seguir*/
          /*esto con el fin de escoger el que menos solicitudes este procesando y distribuir bien las solicitudes*/
          FOR EACH TproIns:
              FOR EACH Mov_Instancias WHERE
                       Mov_instancias.Instancia EQ TproIns.TP_Instancia AND
                       Mov_Instancias.Usuario   EQ TProIns.TP_Usuario AND
                       Mov_Instancias.Estado    EQ NO NO-LOCK:
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
                 Hoja_Vida.Fec_Grabacion = TODAY
                 Hoja_Vida.Hora_Grabacion = TIME + 100
                 Hoja_Vida.Observacion = 
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
                        Hoja_Vida.Fec_Grabacion = TODAY
                        Hoja_Vida.Hora_Grabacion = TIME
                        Hoja_Vida.Asunto_Cumplido = YES.
                        Hoja_Vida.Observacion = "Entra a Instancia: " + STRING(TProIns.TP_Instancia) +
                           " - " + TProIns.TP_NomInstan +
                           " - Usuario Responsable: " + TProIns.TP_Usuario + " - " + TProIns.TP_NomUsuar.
                 FIND FIRST Mov_Instancias WHERE
                      Mov_Instancias.Instancia     EQ TProIns.TP_Instancia AND
                      Mov_Instancias.Num_Solicitud EQ INTEGER(Creditos.Num_Solicitud:SCREEN-VALUE) AND
                      Mov_Instancias.Nit           EQ Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud NO-LOCK NO-ERROR.
                 IF NOT AVAILABLE Mov_Instancias THEN DO:
                   CREATE Mov_Instancias.
                   ASSIGN Mov_Instancias.Fec_Ingreso   = TODAY
                          Mov_Instancias.Hora_Ingreso  = TIME
                          Mov_Instancias.Nit           = Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud
                          Mov_Instancias.Num_Solicitud = DECIMAL(Creditos.Num_Solicitud:SCREEN-VALUE)
                          Mov_Instancias.Usuario       = TProIns.TP_Usuario
                          Mov_Instancias.Instancia     = TProIns.TP_Instancia
                          Mov_Instancias.Agencia       = TProIns.TP_Agencia.
                 END.
              END.
          END.
          ENABLE ALL WITH FRAME F_Creditos.
          DISABLE NomUsuario WITH FRAME F_Creditos.
          ENABLE ALL WITH FRAME F_Consulta.
          APPLY "choose" TO Btn_Consulta IN FRAME F_Creditos.
          Buscar:LABEL IN FRAME F_Consulta = "Buscar Solicitud".
       END.
    END.
  END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Buscar_Indicadores wWin 
PROCEDURE Buscar_Indicadores :
DO WITH FRAME F_Solicitud:
     IF Pro_Creditos.Id_Tasa EQ 1 THEN DO:
        FIND FIRST Indicadores WHERE Indicadores.Estado    EQ 1
                                 AND Indicadores.Indicador EQ Pro_Creditos.Cod_Tasa
                                 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE(Indicadores) THEN DO:
           MESSAGE "No exite un indicadores para el plazo, monto y linea" SKIP
                   "de producto de crédito. Consulte con el Administrador" SKIP
                   "del sistema acerca de esta inconsistencia" VIEW-AS ALERT-BOX ERROR.
           APPLY "ENTRY" TO Creditos.Monto.
           RETURN NO-APPLY.
        END.
        IF Indicadores.FecVcto LT TODAY THEN DO:
           MESSAGE "El indicador para este producto se encuentra Vencido" SKIP
                   "la tasa puede estar desactualizada. Consulte con" SKIP
                   "el administrador del sistema acerca de esta inconsistencia" VIEW-AS ALERT-BOX ERROR.
           APPLY "ENTRY" TO Creditos.Monto.
           RETURN NO-APPLY.
        END.
        IF NOT Indicadores.Rangos THEN 
           ASSIGN Creditos.Tasa:SCREEN-VALUE = STRING(Indicadores.Tasa).
        ELSE RUN Hallar_Rangos_Indicador.
     END.
     ELSE DO:
        IF Creditos.Tasa:SCREEN-VALUE = "0" THEN DO:
           MESSAGE "EL producto de Credito permite que el asesor" SKIP
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Calcular_Deducible wWin 
PROCEDURE Calcular_Deducible :
DEFINE INPUT PARAMETER Ptmo LIKE Solicitud.Monto.
DEFINE VARIABLE        Tot_Deduc LIKE Solicitud.Monto.
DEFINE VARIABLE        Val_Deduc LIKE Solicitud.Monto.
ASSIGN W_MontoCre = Ptmo
       W_MontoDeb = Ptmo.
FOR EACH TDeducc:
  
  IF TDeducc.Cla_Deducible EQ 1 THEN
     ASSIGN Tot_Deduc = Tot_Deduc + Ptmo * TDeducc.Valor
            Val_Deduc = Ptmo * TDeducc.Valor.
  ELSE
     ASSIGN Tot_Deduc = Tot_Deduc + TDeducc.Valor
            Val_Deduc = TDeducc.Valor.
  IF W_Des AND Creditos.Id_Adicionales NE 3 THEN DO: /*creo partidas para contabilizacion de deducibles*/
/*     CREATE TPartidas.
     ASSIGN Tpartidas.TCta = TDeducc.Cuenta
            TPartidas.TDsc = TDeducc.Nom_Deducible + " : " + STRING(Valor)
            TPartidas.TDoc = Creditos.Pagare:SCREEN-VALUE IN FRAME F_Ultima
            TPartidas.TTip = 3
            TPartidas.TOpe = 0
            TPartidas.TCre = Val_Deduc.*/
     IF Creditos.Id_Adicionales EQ 1 THEN
        W_MontoCre = W_MontoCre + Val_Deduc.
     IF Creditos.Id_Adicionales EQ 2 THEN
        W_MontoCre = W_MontoCre - Val_Deduc.
  END.
END.
IF Creditos.Id_Adicionales EQ 1 THEN W_MontoDeb = Creditos.Monto + Tot_Deduc.
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

OCXFile = SEARCH( "W-Proceso_Abogado.wrx":U ).
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
ELSE MESSAGE "W-Proceso_Abogado.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Creditos_X_Instancia wWin 
PROCEDURE Creditos_X_Instancia :
FOR EACH Consulta: DELETE Consulta. END.
  FOR EACH Mov_Instancias WHERE
           Mov_Instancias.Instancia EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) AND
           Mov_Instancias.Usuario   EQ W_Usuario AND
           Mov_Instancias.Estado    EQ NO:
     FIND Creditos WHERE Creditos.Num_Credito  EQ INTEGER(Mov_Instancias.Cuenta) AND
                         Creditos.Nit          EQ Mov_Instancias.Nit AND
                         Creditos.Estado       EQ 2 NO-LOCK NO-ERROR.
     IF AVAILABLE Creditos THEN DO:
       CREATE Consulta.
       ASSIGN Consulta.AgeCredito    = Creditos.Agencia
              Consulta.Num_Solicitud = Creditos.Num_Solicitud
              Consulta.Estado        = Creditos.Estado
              Consulta.Num_Credito   = DECIMAL(Mov_Instancias.Cuenta)
              Consulta.Fec_Ingreso   = Mov_Instancias.Fec_Ingreso
              Consulta.Monto         = Creditos.Sdo_Capital
              Consulta.Hor_Ingreso   = STRING(Mov_Instancias.Hora_Ingreso,"HH:MM:SS am")
              Consulta.Vigencia      = TODAY - Mov_Instancias.Fec_Ingreso.
       FIND Clientes WHERE Clientes.Nit EQ Mov_Instancias.Nit NO-LOCK NO-ERROR.
       IF AVAILABLE Clientes THEN
          ASSIGN Consulta.Nit         = Clientes.Nit
                 Consulta.Nombre      = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
       FIND FIRST Cobros WHERE
                  Cobros.Agencia     EQ Creditos.Agencia AND
                  Cobros.Num_Credito EQ Creditos.Num_Credito AND
                  Cobros.Nit         EQ Creditos.Nit AND
                  Cobros.Estado      EQ 1 NO-LOCK NO-ERROR.
       IF AVAILABLE Cobros THEN
          ASSIGN Consulta.FecAcuerdo = Cobros.Fec_Compromiso
                 Consulta.NumAcuerdo = Cobros.Nro_Cobro.
     END.
    /* ELSE DO:
       MESSAGE "El Movimiento de Instancias apunta a una Solicitud" SKIP
               "inexistente. Comunique esta anomalia al Administrador" SKIP
               "Num.Solicitud: " Mov_Instancias.Num_Solicitud SKIP
               "Nit: " Mov_Instancias.Nit 
               VIEW-AS ALERT-BOX.
     END.*/
  END.
  OPEN QUERY Br_Consulta FOR EACH Consulta NO-LOCK BY Consulta.Num_Credito INDEXED-REPOSITION.
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
  DISPLAY Cmb_Instancias TAct NomUsuario WTUsura 
      WITH FRAME F_Creditos IN WINDOW wWin.
  ENABLE RECT-2 RECT-3 Cmb_Instancias BUTTON-1 Btn_ProIns TAct Btn_Imprimir 
         Btn_Consulta BUTTON-2 BUTTON-4 
      WITH FRAME F_Creditos IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Creditos}

  {&OPEN-QUERY-F_Solicitud}
  GET FIRST F_Solicitud.
  DISPLAY Cmb_Agencias NomNit Nom_Producto W_TasaNominal W_TasaPeriodo 
          Cmb_PerPago W_ForPago Cmb_Sistemas W_Tipo_Credito 
      WITH FRAME F_Solicitud IN WINDOW wWin.
  IF AVAILABLE Creditos THEN 
    DISPLAY Creditos.Nit Creditos.Num_Credito Creditos.Num_Solicitud 
          Creditos.Fec_Aprobacion Creditos.Fec_Desembolso 
          Creditos.Int_Corrientes Creditos.Fec_Pago Creditos.Int_Anticipado 
          Creditos.Monto Creditos.Fec_ProxLiquidacion Creditos.Int_DifCobro 
          Creditos.Plazo Creditos.Fec_UltPago Creditos.Sdo_Capital 
          Creditos.Cuota Creditos.For_Interes Creditos.Fec_UltLiquidacion 
          Creditos.Sdo_Proyectado Creditos.Cuo_Pagadas Creditos.Tasa 
          Creditos.Val_Atraso Creditos.Dias_Atraso Creditos.Cuo_Atraso 
          Creditos.Provision Creditos.Fec_Reestructurado Creditos.Reestructurado 
      WITH FRAME F_Solicitud IN WINDOW wWin.
  ENABLE RECT-152 RECT-293 RECT-294 BUTTON-19 Btn_InfoCode Btn_GarAdm 
         Btn_HojaVida Btn_RadicarJuz Btn_Scoring Btn_Acuerdo 
      WITH FRAME F_Solicitud IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Solicitud}
  DISPLAY R_Organizar Buscar VG_Normal VG_Media VG_Alta 
      WITH FRAME F_Consulta IN WINDOW wWin.
  ENABLE RECT-223 RECT-287 RECT-288 RECT-289 Br_Consulta R_Organizar 
         Btn_OutConsulta Buscar 
      WITH FRAME F_Consulta IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Consulta}

  {&OPEN-QUERY-F_Admisibles}
  GET FIRST F_Admisibles.
  IF AVAILABLE Garantias THEN 
    DISPLAY Garantias.Tipo_Garantia Garantias.Nit_Aseguradora Garantias.Estado 
          Garantias.Nro_Seguro Garantias.Fec_Creacion Garantias.Fec_Retiro 
          Garantias.Fec_IniSeguro Garantias.Descripcion_Bien 
          Garantias.Fec_FinSeguro Garantias.Val_Asegurado Garantias.Nom_Impuesto 
          Garantias.Fec_VctoImpuesto Garantias.Val_Impuesto 
      WITH FRAME F_Admisibles IN WINDOW wWin.
  ENABLE RECT-295 Br_Admisibles BUTTON-169 Btn_OutAdmisibles 
      WITH FRAME F_Admisibles IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Admisibles}

  {&OPEN-QUERY-F_Instancias}
  GET FIRST F_Instancias.
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
  DISPLAY S_InfoCliente 
      WITH FRAME F_InfoCliente IN WINDOW wWin.
  ENABLE S_InfoCliente BUTTON-156 BUTTON-108 
      WITH FRAME F_InfoCliente IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_InfoCliente}

  {&OPEN-QUERY-F_Juzgado}
  GET FIRST F_Juzgado.
  IF AVAILABLE Creditos THEN 
    DISPLAY Creditos.Nit_Juzgado Creditos.Nom_Juzgado 
      WITH FRAME F_Juzgado IN WINDOW wWin.
  ENABLE Creditos.Nit_Juzgado BUTTON-179 BUTTON-180 
      WITH FRAME F_Juzgado IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Juzgado}
  ENABLE Br_Cerradas Btn_OutCerradas BUTTON-154 
      WITH FRAME F_Cerradas IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Cerradas}

  {&OPEN-QUERY-F_Acuerdo}
  GET FIRST F_Acuerdo.
  IF AVAILABLE Cobros THEN 
    DISPLAY Cobros.Fec_Compromiso Cobros.Val_Compromiso Cobros.Usuario 
          Cobros.Age_Recaudo Cobros.Val_Cumplido Cobros.Usu_Recaudo 
          Cobros.Nro_Transaccion Cobros.Fec_Cumplimiento 
      WITH FRAME F_Acuerdo IN WINDOW wWin.
  ENABLE RECT-296 Cobros.Fec_Compromiso BUTTON-178 Cobros.Val_Compromiso 
         Btn_SalAcuerdo Btn_ImpAcuerdo BUTTON-177 BUTTON-175 
      WITH FRAME F_Acuerdo IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Acuerdo}
  ENABLE B_Acuerdos Btn_OutConAcuerdo 
      WITH FRAME F_ConAcuerdo IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_ConAcuerdo}
  ENABLE Br_Deducibles BUTTON-101 
      WITH FRAME F_Deducibles IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Deducibles}

  {&OPEN-QUERY-F_HojaVida}
  GET FIRST F_HojaVida.
  IF AVAILABLE Hoja_Vida THEN 
    DISPLAY Hoja_Vida.Asunto_Cumplido Hoja_Vida.Fec_Limite Hoja_Vida.Fec_Grabacion 
          Hoja_Vida.Observacion 
      WITH FRAME F_HojaVida IN WINDOW wWin.
  ENABLE BUTTON-150 Hoja_Vida.Asunto_Cumplido Hoja_Vida.Fec_Grabacion 
         Hoja_Vida.Observacion Btn_SalvaHV Btn_NvoHv BUTTON-152 BUTTON-149 
      WITH FRAME F_HojaVida IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_HojaVida}
  ENABLE Br_ConHV Btn_OutConHV 
      WITH FRAME F_ConHV IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_ConHV}
  DISPLAY E_Agregar 
      WITH FRAME F_Agregar IN WINDOW wWin.
  ENABLE E_Agregar BUTTON-153 
      WITH FRAME F_Agregar IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Agregar}
  DISPLAY R_EstadoCode 
      WITH FRAME F_Codeudores IN WINDOW wWin.
  ENABLE Br_Codeudores BUTTON-167 R_EstadoCode BUTTON-166 
      WITH FRAME F_Codeudores IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Codeudores}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Hallar_Rangos_Indicador wWin 
PROCEDURE Hallar_Rangos_Indicador :
DO WITH FRAME F_Solicitud:
/*   ASSIGN W_PunTos:SENSITIVE = FALSE
          W_PNegocia         = 0.*/
  FIND FIRST Ran_Intereses WHERE 
             Ran_Intereses.Agencia                  EQ W_Agencia
         AND Ran_Intereses.Indicador                EQ Indicador.Indicador 
         AND (DECIMAL(Creditos.Monto:SCREEN-VALUE) GE Ran_Intereses.Val_Inicial
         AND  DECIMAL(Creditos.Monto:SCREEN-VALUE) LE Ran_Intereses.Val_Final)
         AND (Dias                                  GE Ran_Intereses.Pla_Inicial  
         AND  Dias                                  LE Ran_Intereses.Pla_Final) 
         AND Ran_Interes.Estado                     EQ 1
             NO-LOCK NO-ERROR.
  IF AVAILABLE(Ran_Intereses) THEN DO:
     IF Clientes.Tipo_Vinculo EQ 1 THEN 
        Creditos.Tasa:SCREEN-VALUE = STRING(Indicadores.Tasa + Ran_Intereses.Puntos_Asoc).
     ELSE 
        Creditos.Tasa:SCREEN-VALUE = STRING(Indicadores.Tasa + Ran_Intereses.Puntos).
  END.
  ELSE do: 
   ASSIGN Creditos.Tasa:SCREEN-VALUE = STRING(Indicadores.Tasa).
  END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Hallar_Tasa_Nominal wWin 
PROCEDURE Hallar_Tasa_Nominal :
DEFINE VARIABLE Periodo     AS INTEGER FORMAT "999".
DEFINE VARIABLE Tas_Nominal LIKE Creditos.Tasa.
DO WITH FRAME F_Solicitud:
   CASE INTEGER(SUBSTRING(Cmb_PerPago:SCREEN-VALUE,1,1)):
     WHEN 1 THEN Periodo = 52.
     WHEN 3 THEN Periodo = 24.
     WHEN 4 THEN Periodo = 12.
   END CASE.
   IF Creditos.FOR_Interes:SCREEN-VALUE EQ "1" THEN
     RUN EFNV IN W_ManFin  (INPUT (DECIMAL(Creditos.Tasa:SCREEN-VALUE) / 100), Periodo, OUTPUT Tas_Nominal).
   ELSE
     RUN EFNA IN W_ManFin (INPUT (DECIMAL(Creditos.Tasa:SCREEN-VALUE) / 100), Periodo, OUTPUT Tas_Nominal).
   ASSIGN Tas_Nominal              = ((Tas_Nominal * Periodo) * 100)
          W_TasaNominal:SCREEN-VALUE = STRING(Tas_Nominal).
 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Hallar_Tasa_Periodo wWin 
PROCEDURE Hallar_Tasa_Periodo :
DEFINE VAR Tas_Periodo LIKE Creditos.Tasa.
DO WITH FRAME F_Solicitud:
  RUN HallarTasPer IN W_ManFin 
     (INPUT INTEGER(SUBSTRING(Cmb_PerPago:SCREEN-VALUE,1,1)),
      INPUT DECIMAL(Creditos.Tasa:SCREEN-VALUE),
      INPUT INTEGER(Creditos.FOR_Interes:SCREEN-VALUE), OUTPUT Tas_Periodo).
  ASSIGN Tas_Periodo                = (Tas_Periodo * 100)
         W_TasaPeriodo:SCREEN-VALUE = STRING(Tas_Periodo).
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imp_Cheque wWin 
PROCEDURE Imp_Cheque :
/*------------------------------------------------------------------------------
  OBSERVACIONES : Permite Imprimir los Cheques de Egresoso de la Entidad.       
------------------------------------------------------------------------------*/
  
  DEFINE INPUT PARAMETER C_Valor  LIKE Ahorros.Sdo_Disponible.
  DEFINE INPUT PARAMETER C_Benef  AS   CHARACTER FORMAT "X(50)".
  DEFINE INPUT PARAMETER C_Ciudad LIKE Ubicacion.Nombre.

  DEFINE VAR W_Cadena AS CHARACTER FORMAT "X(150)".
  DEFINE VAR W_Monto1 AS CHARACTER FORMAT "X(70)".
  DEFINE VAR W_Monto2 AS CHARACTER FORMAT "X(70)".
  DEFINE VAR W_Monto3 AS CHARACTER FORMAT "X(70)".
  DEFINE VAR W_Rpta   AS LOGICAL.
   
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
  IF CHOICE2 THEN
   DO:
     RUN MontoEsc.r (INPUT C_Valor,INPUT 0,OUTPUT W_Cadena).
     RUN PartirValor IN W_Manija (INPUT W_Cadena,INPUT 60,OUTPUT W_Monto1,OUTPUT W_Monto2,OUTPUT W_Monto3).
     RUN MostrarMensaje IN W_Manija (INPUT 338,OUTPUT W_Rpta).
     RUN VALUE(W_ProFor)
              (INPUT W_Monto1,INPUT W_Monto2,INPUT C_Benef,INPUT C_Ciudad,
               INPUT C_Valor, INPUT "") NO-ERROR.
     IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "Error al imprimir el Cheque" SKIP
                "Se cancela la operacion de Desembolso" VIEW-AS ALERT-BOX ERROR. RETURN ERROR.
     END.
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
    W_EncColumna = "Cliente Solicitante         :   " + W_Cliente.
    
    
 VIEW FRAME F-Encabezado.
 VIEW FRAME F-Ftr.

 DO WITH FRAME F_Solicitud:
   T_Plazo = Creditos.Plazo:SCREEN-VALUE  + " (" + SUBSTRING(Cmb_PerPago:SCREEN-VALUE,5,15) + ")".
   DISPLAY 
  /* 1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890*/
     "=============================================DATOS GENERALES DE LA SOLICITUD==============================================" AT 1
     "Agencia de Radicación       : " AT 1
     Cmb_Agencias:SCREEN-VALUE        AT 33 FORMAT "X(30)"
     "Número del Crédito          : " AT 65
     STRING(Creditos.Num_Credito:SCREEN-VALUE) AT 98
     "Número de Solicitud         : " AT 1
     STRING(Creditos.Num_Solicitud:SCREEN-VALUE) AT 33
     "Fecha de Aprobación         : " AT 65
     Creditos.Fec_Aprobacion:SCREEN-VALUE  AT 98  FORMAT "X(10)"
     "Producto de Crédito         : " AT 1
     Nom_Producto:SCREEN-VALUE        AT 33 FORMAT "X(30)"
     "Tipo de Producto            : " AT 65
     TRIM(W_Tipo_Credito:SCREEN-VALUE) AT 98 FORMAT "X(30)"
     "Instancia Actual            : " AT 1
     Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos AT 33  FORMAT "X(30)"
     "Usuario Actualmente Procesa : " AT 65
     NomUsuario:SCREEN-VALUE IN FRAME F_Creditos AT 98  FORMAT "X(30)" 
     "Forma de Pago de la Cuota   : " AT 1
     W_ForPago:SCREEN-VALUE           AT 33 FORMAT "X(30)"
     "=============================================DETALLE DE VALORES DEL CREDITO==============================================" AT 1
     "Monto a Prestar             : " AT 1
     Creditos.Monto:SCREEN-VALUE     AT 33  FORMAT "X(30)"
     "Tasa Efectiva Anual         : " AT 65
     Creditos.Tasa:SCREEN-VALUE      AT 98  FORMAT "X(30)" SKIP(1)
     WITH FRAME F_Sol WIDTH 132 NO-BOX USE-TEXT NO-LABELS STREAM-IO.
 END.
  
 PAGE.
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
          Cmb_Perpago:SCREEN-VALUE             = "4 - Mensual"
          Creditos.Tasa:SCREEN-VALUE          = "0"
          W_TasaNominal:SCREEN-VALUE           = "0"
          W_TasaPeriodo:SCREEN-VALUE           = "0"
          W_ForPago:SCREEN-VALUE               = "Nomina".
          /*Cmb_Sistemas:SCREEN-VALUE            = Cmb_Sistemas:ENTRY(1).*/
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
FOR EACH Instancias WHERE Instancias.Tipo_Instancia EQ 5 AND
                          Instancias.Estado         EQ 1 AND
                          Instancias.Tipo_Producto  EQ 2 NO-LOCK BREAK BY Instancias.Orden:
     IF Instancias.Ultima THEN W_Ultima = Instancias.Instancia.
     IF Instancias.Primera THEN W_Primera = Instancias.Instancia.
     FIND FIRST Cfg_Instancias WHERE
          /* Cfg_Instancias.Agencia EQ W_Agencia AND*/
           Cfg_Instancias.Tipo_Instancia EQ 5  AND
           Cfg_Instancias.Instancia EQ Instancias.Instancia AND
           Cfg_Instancias.Usuario EQ W_Usuario AND
           Cfg_Instancias.Estado  EQ 1 NO-LOCK NO-ERROR.
     IF AVAILABLE Cfg_Instancias THEN DO:
        W_Ok = Cmb_Instancias:ADD-LAST(STRING(Instancias.Instancia,"99999") + " - " + Instancias.Nom_Instancia) IN FRAME F_Creditos.
     END.
  END.
  FIND Usuarios WHERE Usuarios.Usuario EQ W_Usuario NO-LOCK NO-ERROR.
  ASSIGN NomUsuario = W_Usuario + " - " + Usuarios.Nombre
         W_SucAgen  = Usuarios.Id_OpeOfi
         W_Grupo    = Usuarios.Grupo.
  
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
  
  FIND Entidad WHERE Entidad.Entidad EQ W_Entidad NO-LOCK NO-ERROR.
  IF AVAILABLE Entidad THEN DO:
     FIND Indicadores WHERE Indicadores.Indicador EQ Entidad.ind_Usura NO-LOCK NO-ERROR.
     IF AVAILABLE Indicadores THEN
        WTUsura:SCREEN-VALUE IN FRAME F_Creditos = STRING(Indicadores.Tasa).
     
     ELSE MESSAGE "No se ha encontrado la tasa de usura" SKIP
                  "revise la configuración de la entidad" SKIP
                  "y la configuración de los indicadores" VIEW-AS ALERT-BOX.
  END.
  
  Cmb_Instancias:SCREEN-VALUE = Cmb_Instancias:ENTRY(1).
  RUN Creditos_X_Instancia.
  APPLY "entry" TO Cmb_Instancias IN FRAME F_Creditos.
  RUN Inicializar_Variables.
  HIDE FRAME F_AsentarInstancia.
  HIDE FRAME F_Admisibles.
  HIDE FRAME F_Cerradas.
  HIDE FRAME F_Agregar.
  HIDE FRAME F_Bancos.
  HIDE FRAME F_Acuerdo.
  HIDE FRAME F_ConAcuerdo.
  HIDE FRAME F_Juzgado.
  WWin:TITLE = "Proceso de Administración de Abogados - Agencia Actual: " + STRING(W_Agencia).
  Buscar:LABEL IN FRAME F_Consulta = "Buscar x Crédito".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Liquidar wWin 
PROCEDURE Liquidar :
/* DEFINE VARIABLE PlazoW LIKE Solicitud.Plazo.
  DEFINE VARIABLE TotPtW LIKE Solicitud.Monto.
  DEFINE VARIABLE CuotaW LIKE Solicitud.Cuota.
  DEFINE VARIABLE TasaW  LIKE Solicitud.Tasa.
  DEFINE VARIABLE TinteW LIKE Solicitud.Monto.
  
  DEFINE VAR W_Resultado AS INTEGER INITIAL 0.
  DEFINE VAR Wimp_Coop   AS INTEGER INITIAL 0.   
   DO WITH FRAME F_RePro:
      W_Liquidar = FALSE.
      ASSIGN TotPtW   = DECIMAL(W_NvoSdoCap:SCREEN-VALUE) 
             PlazoW   = DECIMAL(W_NvoPlazo:SCREEN-VALUE)
             TasaW    = DECIMAL(Creditos.Tasa:SCREEN-VALUE IN FRAME F_Solicitud)
             W_perded = INTEGER(SUBSTRING(Cmb_PerPago:SCREEN-VALUE IN FRAME F_Solicitud,1,1)).
      
             W_InterPlazo  = (TotPtW * TasaW) / 100.
      RUN Calculo_Cuota.R (INPUT-OUTPUT TotPtw, INPUT-OUTPUT PlazoW, INPUT-OUTPUT CuotaW,
                      INPUT-OUTPUT TInteW, INPUT-OUTPUT TasaW, INPUT W_Razon,
                      INPUT W_Gracia,INPUT W_PerDed,INPUT 3,
                      INPUT INTEGER(Creditos.FOR_Interes:SCREEN-VALUE IN FRAME F_Solicitud),
                      INPUT INTEGER(SUBSTRING(Cmb_Sistemas:SCREEN-VALUE IN FRAME F_Solicitud,1,5))). 
      IF CuotaW LE 0 THEN DO:
         MESSAGE "El Valor de la cuota debe ser mayor a cero. Rectifique!" VIEW-AS ALERT-BOX ERROR.
         APPLY "ENTRY" TO Cmb_Sistemas IN FRAME F_Solicitud.
         RETURN ERROR.
      END.
      ASSIGN W_NvaCuota:SCREEN-VALUE = STRING(CuotaW).
 END.*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Llenar_InfoCliente wWin 
PROCEDURE Llenar_InfoCliente :
DEFINE VARIABLE gtexto AS CHARACTER FORMAT "x(60)".
   DEFINE VARIABLE TTOTAL  AS DECIMAL FORMAT ">>,>>>,>>>,>>9".
   DEFINE VARIABLE TDISPO  AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
   DO i = 1 TO 500:
      W_Ok = S_InfoCliente:DELETE(1) IN FRAME F_InfoCliente.
   END.
   IF AVAILABLE Clientes THEN DO:
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
      gTexto = "----------------------------------------------------------".
      RUN SInfo (INPUT gtexto).
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
      
      IF Clientes.Cod_Profesion NE 0 THEN DO:
         FIND Varios WHERE Varios.Tipo EQ 1 AND Varios.Codigo EQ Clientes.Cod_Profesion NO-LOCK NO-ERROR.
         IF AVAILABLE Varios THEN DO:
           gTexto = "Profesión              : " + STRING(Varios.Descripcion,"X(20)").
           RUN SInfo (INPUT gtexto).
         END.
      END.
      gTexto = "Telefono Residencia    : " + STRING(Clientes.Tel_Residencia,"X(14)").
      RUN SInfo (INPUT gtexto).
      gTexto = "Telefono Comercial     : " + STRING(Clientes.Tel_Comercial,"X(14)").
      RUN SInfo (INPUT gtexto).
      gTexto = "Telefono Celular       : " + STRING(Clientes.Celular,"X(14)").
      RUN SInfo (INPUT gtexto).
      gTexto = "Dirección Residencia   : " + STRING(Clientes.Dir_Residencia,"X(40)").
      RUN SInfo (INPUT gtexto).
      gTexto = "Dirección Comercial    : " + STRING(Clientes.Dir_Comercial,"X(40)").
      RUN SInfo (INPUT gtexto).
      gTexto = "e-mail                 : " + STRING(Clientes.Email,"X(40)").
      RUN SInfo (INPUT gtexto).
      
      gTexto = "".
      RUN SInfo (INPUT gtexto).
      IF Clientes.Cod_Empresa NE 0 THEN DO:
         gTexto = "Código Empresa         : " + STRING(Clientes.Cod_Empresa,"9999").
         RUN SInfo (INPUT gtexto).
      END.
      IF Clientes.Fec_IngEmpresa NE ? THEN DO:
         gTexto = "Fec.Ingreso Empresa    : " + STRING(Clientes.Fec_IngEmpresa,"99/99/9999").
         RUN SInfo (INPUT gtexto).
      END.
      IF Clientes.Cod_Cargo NE 0 THEN DO:
         FIND Varios WHERE Varios.Tipo EQ 2 AND Varios.Codigo EQ Clientes.Cod_Cargo NO-LOCK NO-ERROR.
         IF AVAILABLE Varios THEN DO:
           gTexto = "Cargo que Desempeña    : " + STRING(Varios.Descripcion,"X(40)").
           RUN SInfo (INPUT gtexto).
         END.
      END.
      CASE Clientes.Tip_Contrato:
        WHEN 1 THEN DO:
           gTexto = "Tipo de Contrato       : Termino Indefinido".
           RUN SInfo (INPUT gtexto).
        END.
        WHEN 2 THEN DO:
           gTexto = "Tipo de Contrato       : Termino Fijo".
           RUN SInfo (INPUT gtexto).
        END.
        WHEN 3 THEN DO:
           gTexto = "Tipo de Contrato       : Labor Contratada".
           RUN SInfo (INPUT gtexto).
        END.
        WHEN 4 THEN DO:
           gTexto = "Tipo de Contrato       : Prestación de Servicios".
           RUN SInfo (INPUT gtexto).
        END.
      END CASE.
      
      gTexto = "".
      RUN SInfo (INPUT gtexto).
      IF Clientes.Per_Acargo NE 0 THEN DO:
         gTexto = "Personas a Cargo       : " + STRING(Clientes.Per_Acargo,"99").
         RUN SInfo (INPUT gtexto).
      END.
      IF Clientes.Num_Hijos NE 0 THEN DO:
         gTexto = "Número de Hijos        : " + STRING(Clientes.Num_Hijos,"99").
         RUN SInfo (INPUT gtexto).
      END.
      gTexto = "Estado Civil           : " + STRING(Clientes.Est_Civil,"X(15)").
      RUN SInfo (INPUT gtexto).
      
   /*Clientes.Calificacion
   Clientes.Cod_Empresa
   Clientes.Cod_Cargo
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Admisible wWin 
PROCEDURE Mostrar_Admisible :
DO WITH FRAME F_Admisibles:
  ASSIGN Garantias.Nit_Aseguradora:SCREEN-VALUE = Garantias.Nit_Aseguradora
         Garantias.Fec_IniSeguro:SCREEN-VALUE   = STRING(Garantias.Fec_IniSeguro)
         Garantias.Fec_FinSeguro:SCREEN-VALUE   = STRING(Garantias.Fec_FinSeguro)
         Garantias.Val_Asegurado:SCREEN-VALUE   = STRING(Garantias.Val_Asegurado)
         Garantias.Nom_Impuesto:SCREEN-VALUE    = STRING(Garantias.Nom_Impuesto)
         Garantias.Fec_VctoImpuesto:SCREEN-VALUE = STRING(Garantias.Fec_VctoImpuesto)
         Garantias.Val_Impuesto:SCREEN-VALUE    = STRING(Garantias.Val_Impuesto)
         Garantias.Tipo_Garantia:SCREEN-VALUE   = STRING(Garantias.Tipo_Garantia)
         Garantias.Estado:SCREEN-VALUE          = STRING(Garantias.Estado)
         Garantias.Fec_Creacion:SCREEN-VALUE    = STRING(Garantias.Fec_Creacion)
         Garantias.Fec_Retiro:SCREEN-VALUE      = STRING(Garantias.Fec_Retiro)
         Garantias.Descripcion_Bien:SCREEN-VALUE = Garantias.Descripcion_Bien.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Credito wWin 
PROCEDURE Mostrar_Credito :
DEFINE VAR x1 AS CHARACTER FORMAT "X(40)".
FIND Pro_Creditos WHERE 
     Pro_Creditos.Tip_Credito EQ Creditos.Tip_Credito AND
     Pro_Creditos.Cod_Credito EQ Creditos.Cod_Credito NO-LOCK NO-ERROR.
IF AVAILABLE Pro_Creditos THEN DO:
 DO WITH FRAME F_Solicitud:
   CASE Pro_Creditos.Tip_Credito:
     WHEN 1 THEN ASSIGN W_Tipo_Credito:SCREEN-VALUE = " Consumo"
                        W_tippdt = 1.
     WHEN 2 THEN ASSIGN W_Tipo_Credito:SCREEN-VALUE = " Comercial"
                        W_Tippdt = 2.
     WHEN 3 THEN ASSIGN W_Tipo_Credito:SCREEN-VALUE = " Hipotecario"
                        W_Tippdt = 3.
     WHEN 4 THEN ASSIGN W_Tipo_Credito:SCREEN-VALUE = " Microcredito"
                        W_Tippdt = 4.
   END CASE.
   FIND Clientes WHERE Clientes.Nit EQ Creditos.Nit NO-LOCK NO-ERROR.
   IF AVAILABLE Clientes THEN NomNit:SCREEN-VALUE = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
   FIND Agencias WHERE Agencias.Agencia EQ Solicitud.Agencia NO-LOCK NO-ERROR.
   IF AVAILABLE Agencias THEN
      Cmb_Agencias:SCREEN-VALUE = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.
   ASSIGN Creditos.Num_Credito:SCREEN-VALUE   = STRING(Creditos.Num_Credito)
          Creditos.Num_Solicitud:SCREEN-VALUE = STRING(Creditos.Num_Solicitud)
          Creditos.Nit:SCREEN-VALUE           = Creditos.Nit
          Creditos.Fec_Aprobacion:SCREEN-VALUE = STRING(Creditos.Fec_Aprobacion)
          Creditos.FOR_Interes:SCREEN-VALUE   = STRING(Creditos.FOR_Interes)
          Creditos.Monto:SCREEN-VALUE         = STRING(Creditos.Monto)
          Creditos.Cuota:SCREEN-VALUE         = STRING(Creditos.Cuota)
          Creditos.Tasa:SCREEN-VALUE          = STRING(Creditos.Tasa)
          Creditos.Plazo:SCREEN-VALUE         = STRING(Creditos.Plazo)
          Creditos.Fec_Desembolso:SCREEN-VALUE = STRING(Creditos.Fec_Desembolso)
          Creditos.Fec_Pago:SCREEN-VALUE       = STRING(Creditos.Fec_Pago)
          Creditos.Fec_ProxLiquidacion:SCREEN-VALUE = STRING(Creditos.Fec_ProxLiquidacion)
          Creditos.Fec_UltPago:SCREEN-VALUE    = STRING(Creditos.Fec_UltPago)
          Creditos.Fec_UltLiquidacion:SCREEN-VALUE = STRING(Creditos.Fec_UltLiquidacion)
          Creditos.INT_Corrientes:SCREEN-VALUE = STRING(Creditos.INT_Corrientes)
          Creditos.INT_DifCobro:SCREEN-VALUE   = STRING(Creditos.INT_DifCobro)
          Creditos.INT_Anticipado:SCREEN-VALUE = STRING(Creditos.INT_Anticipado)
          Creditos.Sdo_Capital:SCREEN-VALUE    = STRING(Creditos.Sdo_Capital)
          Creditos.Sdo_Proyectado:SCREEN-VALUE = STRING(Creditos.Sdo_Proyectado)
          Creditos.Val_Atraso:SCREEN-VALUE     = STRING(Creditos.Val_Atraso)
          Creditos.Dias_Atraso:SCREEN-VALUE    = STRING(Creditos.Dias_Atraso)
          Creditos.Cuo_Atraso:SCREEN-VALUE     = STRING(Creditos.Cuo_Atraso)
          Creditos.Cuo_Pagadas:SCREEN-VALUE    = STRING(Creditos.Cuo_Pagadas)
          Creditos.Provision:SCREEN-VALUE      = STRING(Creditos.Provision)
          Creditos.Reestructurado:SCREEN-VALUE = STRING(Creditos.Reestructurado)
          Nom_Producto:SCREEN-VALUE = STRING(Pro_Creditos.Cod_Credito,"999") + " - " + Pro_Creditos.Nom_Producto.
   CASE Creditos.Per_Pago:
     WHEN 1 THEN 
          ASSIGN Cmb_PerPago:SCREEN-VALUE = "1 - Semanal"
                 Dias = Creditos.Plazo * 7.
     WHEN 3 THEN 
          ASSIGN Cmb_PerPago:SCREEN-VALUE = "3 - Quincenal"
                 Dias = Creditos.Plazo * 15.
     WHEN 4 THEN 
          ASSIGN Cmb_PerPago:SCREEN-VALUE = "4 - Mensual"
                 Dias = Creditos.Plazo * 30.
   END CASE.
   /*forma de pago de la cuota*/
   CASE Creditos.FOR_Pago:
      WHEN 1 THEN W_ForPago:SCREEN-VALUE = "Nomina".
      WHEN 2 THEN W_ForPago:SCREEN-VALUE = "Caja".
      WHEN 3 THEN DO:
        FIND Agencias WHERE Agencias.Agencia EQ Solicitud.Age_DebAutomatico NO-LOCK NO-ERROR.
        IF AVAILABLE Agencias THEN ASSIGN W_ForPago = "Agencia: " + Agencias.Nombre.
        FIND Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ Creditos.Cod_DebAutomatico NO-LOCK NO-ERROR.
        IF AVAILABLE Pro_Ahorros THEN ASSIGN W_ForPago = W_ForPago + " / Producto: " + Pro_Ahorros.Nom_Producto.
        W_ForPago = W_ForPago + " / Cuenta: " + Creditos.Cue_DebAutomatico.
        W_ForPago:SCREEN-VALUE = W_ForPago.
      END.
   END CASE.
   DO WITH FRAME F_Juzgado:
     ASSIGN Creditos.Nit_Juzgado:SCREEN-VALUE = Creditos.Nit_Juzgado
            Creditos.Nom_Juzgado:SCREEN-VALUE = Creditos.Nom_Juzgado.
   END.
   /*fin forma de pago de la cuota*/
   APPLY "leave" TO Creditos.Plazo IN FRAME F_Solicitud.
   APPLY "leave" TO Creditos.Monto IN FRAME F_Solicitud.
   APPLY "leave" TO Creditos.Nit.
   APPLY "value-changed" TO Cmb_Agencias.
   ENABLE {&List-1}.
 END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Multiplicador_Ahorros wWin 
PROCEDURE Multiplicador_Ahorros :
/*Verifica si el producto de credito debe estar acompañado de una cuenta de ahorros
  y el multiplicador de su saldo para poder ser radicada la solicitud*/     
  DEFINE VARIABLE W_Mon LIKE Ahorros.Sdo_Disponible.
  DEFINE VARIABLE choice AS LOGICAL.
  IF Pro_Creditos.Id_CreProAhorro = TRUE THEN DO:
     FOR EACH Ahorros WHERE Ahorros.Nit              EQ Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud
                        AND Ahorros.Cod_Ahorro       EQ Pro_Creditos.Lin_Multiplicador 
                        AND Ahorros.Estado           EQ 1 NO-LOCK:
                 W_Mon = W_Mon + Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje.
        END.
        ASSIGN W_Mon = (W_Mon * Pro_Creditos.Multiplicador).
        IF DECIMAL(Creditos.Monto:SCREEN-VALUE) NE 0 AND 
           DECIMAL(Creditos.Monto:SCREEN-VALUE) GT W_Mon AND 
           W_Mon GT 0 THEN DO:
           MESSAGE "Este producto tiene configurado la opcion de multiplicador" SKIP
                   "No se puede prestar mas de : $" STRING(W_Mon,">>>,>>>,>>>,>>9") SKIP(1)
                   "Desea poner como monto a prestar este valor?" VIEW-AS ALERT-BOX INFORMATION
                    BUTTONS YES-NO-CANCEL UPDATE choice.
           IF choice THEN
              ASSIGN Creditos.Monto:SCREEN-VALUE = STRING(W_Mon).
           ELSE ASSIGN Creditos.Monto:SCREEN-VALUE = "0".
        END.
        APPLY "Entry" TO Creditos.Monto.
  
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir wWin 
PROCEDURE ProcesoImprimir :
IF W_TipoInforme EQ "Proyeccion" THEN 
   RUN Proyeccion.
ELSE DO:
   RUN Informe.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proyeccion wWin 
PROCEDURE Proyeccion :
RUN CalAmort.R (INPUT DECIMAL(Creditos.Sdo_Capital:SCREEN-VALUE IN FRAME F_Solicitud),
                INPUT DECIMAL(Creditos.Plazo:SCREEN-VALUE IN FRAME F_Solicitud),
                INPUT DECIMAL(Creditos.Cuota:SCREEN-VALUE IN FRAME F_Solicitud),
                INPUT 0, /*total extras que ya no se utiliza*/
                INPUT TODAY, /*no se que fecha se pone o se pide*/
                INPUT DECIMAL(W_TasaPeriodo:SCREEN-VALUE IN FRAME F_Solicitud) / 100,
                INPUT 0,
                INPUT 0, /*gracia*/
                INPUT INTEGER(SUBSTRING(Cmb_PerPago:SCREEN-VALUE IN FRAME F_Solicitud,1,1)),
                INPUT INTEGER(Creditos.FOR_Interes:SCREEN-VALUE IN FRAME F_Solicitud),
                INPUT INTEGER(SUBSTRING(Cmb_Sistemas:SCREEN-VALUE IN FRAME F_Solicitud,1,5)),
                INPUT Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud,
                INPUT NomNit:SCREEN-VALUE IN FRAME F_Solicitud,
                INPUT Creditos.Cod_Credito,   
                INPUT Nom_Producto:SCREEN-VALUE IN FRAME F_Solicitud,
                INPUT "S",
                INPUT DECIMAL(Creditos.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud),
                INPUT SUBSTRING(Cmb_Sistemas:SCREEN-VALUE IN FRAME F_Solicitud,9,30),
                INPUT SUBSTRING(Cmb_PerPago:SCREEN-VALUE IN FRAME F_Solicitud,5,15),
                INPUT 1).                          
W_TipoInforme = "".
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

