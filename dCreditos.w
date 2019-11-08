&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS dTables 
/*------------------------------------------------------------------------

  File:  

  Description: from DATA.W - Template For SmartData objects in the ADM

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Modified:     February 24, 1999
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{incluido\iGetSdo.i}

DEFINE VARIABLE vcNombre     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vcDirRes     AS CHARACTER   FORMAT "X(40)" NO-UNDO.
DEFINE VARIABLE vcTelRes     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vcUbiRes     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vcDirCom     AS CHARACTER   FORMAT "X(40)" NO-UNDO.
DEFINE VARIABLE vcTelCom     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vcUbiCom     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vcNomCodA     AS CHARACTER   NO-UNDO. /*Nombre Codeudor*/
DEFINE VARIABLE vcDirResCodA AS CHARACTER   FORMAT "X(40)" NO-UNDO. /* Dirección Residencia Codeudor */
DEFINE VARIABLE vcTelResCodA AS CHARACTER   NO-UNDO. /* Telefono residencia Codeudor  */ 
DEFINE VARIABLE vcUbiResCodA AS CHARACTER   NO-UNDO. /* Ubicacion Residencia Codeudor */
DEFINE VARIABLE vcDirComCodA AS CHARACTER   FORMAT "X(40)" NO-UNDO. /* Dirección Comercial Codeudor  */ 
DEFINE VARIABLE vcTelComCodA AS CHARACTER   NO-UNDO. /* Telefono Comercial Codeudor   */  
DEFINE VARIABLE vcUbiComCodA AS CHARACTER   NO-UNDO. /* Ubicacion COmercial Codeudor  */ 
DEFINE VARIABLE vcNomCodB     AS CHARACTER   NO-UNDO. /*Nombre Codeudor*/
DEFINE VARIABLE vcDirResCodB AS CHARACTER   FORMAT "X(40)" NO-UNDO. /* Dirección Residencia Codeudor */
DEFINE VARIABLE vcTelResCodB AS CHARACTER   NO-UNDO. /* Telefono residencia Codeudor  */ 
DEFINE VARIABLE vcUbiResCodB AS CHARACTER   NO-UNDO. /* Ubicacion Residencia Codeudor */
DEFINE VARIABLE vcDirComCodB AS CHARACTER   FORMAT "X(40)" NO-UNDO. /* Dirección Comercial Codeudor  */ 
DEFINE VARIABLE vcTelComCodB AS CHARACTER   NO-UNDO. /* Telefono Comercial Codeudor   */  
DEFINE VARIABLE vcUbiComCodB AS CHARACTER   NO-UNDO. /* Ubicacion COmercial Codeudor  */ 
DEFINE VARIABLE vcNomCodC     AS CHARACTER   NO-UNDO. /*Nombre Codeudor*/
DEFINE VARIABLE vcDirResCodC AS CHARACTER   FORMAT "X(40)" NO-UNDO. /* Dirección Residencia Codeudor */
DEFINE VARIABLE vcTelResCodC AS CHARACTER   NO-UNDO. /* Telefono residencia Codeudor  */ 
DEFINE VARIABLE vcUbiResCodC AS CHARACTER   NO-UNDO. /* Ubicacion Residencia Codeudor */
DEFINE VARIABLE vcDirComCodC AS CHARACTER   FORMAT "X(40)" NO-UNDO. /* Dirección Comercial Codeudor  */ 
DEFINE VARIABLE vcTelComCodC AS CHARACTER   NO-UNDO. /* Telefono Comercial Codeudor   */  
DEFINE VARIABLE vcUbiComCodC AS CHARACTER   NO-UNDO. /* Ubicacion COmercial Codeudor  */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Global-define DATA-LOGIC-PROCEDURE .p

&Scoped-define PROCEDURE-TYPE SmartDataObject
&Scoped-define DB-AWARE yes

&Scoped-define ADM-SUPPORTED-LINKS Data-Source,Data-Target,Navigation-Target,Update-Target,Commit-Target,Filter-Target


/* Db-Required definitions. */
&IF DEFINED(DB-REQUIRED) = 0 &THEN
    &GLOBAL-DEFINE DB-REQUIRED TRUE
&ENDIF
&GLOBAL-DEFINE DB-REQUIRED-START   &IF {&DB-REQUIRED} &THEN
&GLOBAL-DEFINE DB-REQUIRED-END     &ENDIF


&Scoped-define QUERY-NAME Query-Main

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Creditos

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  Abogado Agencia Age_DebAutomatico Age_Desembolso Capital_Acum Categoria~
 CategoriaMes Cod_Califica Cod_CalificaMes Cod_Credito Cod_DebAutomatico~
 Cod_Desembolso Costas Cta_Contable Cue_DebAutomatico Cue_Desembolso Cuota~
 Cuo_Atraso Cuo_Pagadas Deducible Desembolso Destino Detalle_Estado~
 Dias_Atraso Estado Fec_Aprobacion Fec_Calificacion Fec_CanceTotal~
 Fec_Desembolso Fec_DifCobro Fec_PagAnti Fec_Pago Fec_ProxLiquidacion~
 Fec_Reestructurado Fec_UltLiquidacion Fec_UltPago For_Interes For_Pago~
 Honorarios Id_Adicionales Incremento Int_AntDesembolso Int_Anticipado~
 Int_Corrientes Int_DifCobro Int_LiqAcum Int_MoraDifCob Int_MorCobrar Monto~
 Nit Nit_Juzgado Nom_Juzgado Num_Credito Num_Solicitud Observaciones Pagare~
 Per_Gracia Per_Pago Plazo Poliza Polizas Provision Provision_Interes~
 Provision_Otros Reestructurado Sdo_Anuales1 Sdo_Anuales2 Sdo_Anuales3~
 Sdo_Anuales4 Sdo_Anuales5 Sdo_Anuales6 Sdo_Anuales7 Sdo_Anuales8~
 Sdo_Anuales9 Sdo_Anuales10 Sdo_Anuales11 Sdo_Anuales12 Sdo_Capital~
 Sdo_CapPag Sdo_IntMor Sdo_IntPag Sdo_Proyectado Sistema Tasa Tip_Credito~
 Usuario Val_Atraso Val_Desembolso
&Scoped-define ENABLED-FIELDS-IN-Creditos Abogado Agencia Age_DebAutomatico ~
Age_Desembolso Capital_Acum Categoria CategoriaMes Cod_Califica ~
Cod_CalificaMes Cod_Credito Cod_DebAutomatico Cod_Desembolso Costas ~
Cta_Contable Cue_DebAutomatico Cue_Desembolso Cuota Cuo_Atraso Cuo_Pagadas ~
Deducible Desembolso Destino Detalle_Estado Dias_Atraso Estado ~
Fec_Aprobacion Fec_Calificacion Fec_CanceTotal Fec_Desembolso Fec_DifCobro ~
Fec_PagAnti Fec_Pago Fec_ProxLiquidacion Fec_Reestructurado ~
Fec_UltLiquidacion Fec_UltPago For_Interes For_Pago Honorarios ~
Id_Adicionales Incremento Int_AntDesembolso Int_Anticipado Int_Corrientes ~
Int_DifCobro Int_LiqAcum Int_MoraDifCob Int_MorCobrar Monto Nit Nit_Juzgado ~
Nom_Juzgado Num_Credito Num_Solicitud Observaciones Pagare Per_Gracia ~
Per_Pago Plazo Poliza Polizas Provision Provision_Interes Provision_Otros ~
Reestructurado Sdo_Anuales1 Sdo_Anuales2 Sdo_Anuales3 Sdo_Anuales4 ~
Sdo_Anuales5 Sdo_Anuales6 Sdo_Anuales7 Sdo_Anuales8 Sdo_Anuales9 ~
Sdo_Anuales10 Sdo_Anuales11 Sdo_Anuales12 Sdo_Capital Sdo_CapPag Sdo_IntMor ~
Sdo_IntPag Sdo_Proyectado Sistema Tasa Tip_Credito Usuario Val_Atraso ~
Val_Desembolso 
&Scoped-Define DATA-FIELDS  Abogado Agencia Age_DebAutomatico Age_Desembolso Capital_Acum Categoria~
 CategoriaMes Cod_Califica Cod_CalificaMes Cod_Credito Cod_DebAutomatico~
 Cod_Desembolso Costas Cta_Contable Cue_DebAutomatico Cue_Desembolso Cuota~
 Cuo_Atraso Cuo_Pagadas Deducible Desembolso Destino Detalle_Estado~
 Dias_Atraso Estado Fec_Aprobacion Fec_Calificacion Fec_CanceTotal~
 Fec_Desembolso Fec_DifCobro Fec_PagAnti Fec_Pago Fec_ProxLiquidacion~
 Fec_Reestructurado Fec_UltLiquidacion Fec_UltPago For_Interes For_Pago~
 Honorarios Id_Adicionales Incremento Int_AntDesembolso Int_Anticipado~
 Int_Corrientes Int_DifCobro Int_LiqAcum Int_MoraDifCob Int_MorCobrar Monto~
 Nit Nit_Juzgado Nom_Juzgado Num_Credito Num_Solicitud Observaciones Pagare~
 Per_Gracia Per_Pago Plazo Poliza Polizas Provision Provision_Interes~
 Provision_Otros Reestructurado Sdo_Anuales1 Sdo_Anuales2 Sdo_Anuales3~
 Sdo_Anuales4 Sdo_Anuales5 Sdo_Anuales6 Sdo_Anuales7 Sdo_Anuales8~
 Sdo_Anuales9 Sdo_Anuales10 Sdo_Anuales11 Sdo_Anuales12 Sdo_Capital~
 Sdo_CapPag Sdo_IntMor Sdo_IntPag Sdo_Proyectado Sistema Tasa Tip_Credito~
 Usuario Val_Atraso Val_Desembolso FAgencia FCredito FNombre FDirResCli~
 FTelResCli FDirComCli FTelComCli FDirResCodA FTelResCodA FDirComCodA~
 FTelComCodA FUbiResCodA FUbiComCodA
&Scoped-define DATA-FIELDS-IN-Creditos Abogado Agencia Age_DebAutomatico ~
Age_Desembolso Capital_Acum Categoria CategoriaMes Cod_Califica ~
Cod_CalificaMes Cod_Credito Cod_DebAutomatico Cod_Desembolso Costas ~
Cta_Contable Cue_DebAutomatico Cue_Desembolso Cuota Cuo_Atraso Cuo_Pagadas ~
Deducible Desembolso Destino Detalle_Estado Dias_Atraso Estado ~
Fec_Aprobacion Fec_Calificacion Fec_CanceTotal Fec_Desembolso Fec_DifCobro ~
Fec_PagAnti Fec_Pago Fec_ProxLiquidacion Fec_Reestructurado ~
Fec_UltLiquidacion Fec_UltPago For_Interes For_Pago Honorarios ~
Id_Adicionales Incremento Int_AntDesembolso Int_Anticipado Int_Corrientes ~
Int_DifCobro Int_LiqAcum Int_MoraDifCob Int_MorCobrar Monto Nit Nit_Juzgado ~
Nom_Juzgado Num_Credito Num_Solicitud Observaciones Pagare Per_Gracia ~
Per_Pago Plazo Poliza Polizas Provision Provision_Interes Provision_Otros ~
Reestructurado Sdo_Anuales1 Sdo_Anuales2 Sdo_Anuales3 Sdo_Anuales4 ~
Sdo_Anuales5 Sdo_Anuales6 Sdo_Anuales7 Sdo_Anuales8 Sdo_Anuales9 ~
Sdo_Anuales10 Sdo_Anuales11 Sdo_Anuales12 Sdo_Capital Sdo_CapPag Sdo_IntMor ~
Sdo_IntPag Sdo_Proyectado Sistema Tasa Tip_Credito Usuario Val_Atraso ~
Val_Desembolso 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST   rowObject.Sdo_Anuales1 = Creditos.Sdo_Anuales[1]~
  rowObject.Sdo_Anuales2 = Creditos.Sdo_Anuales[2]~
  rowObject.Sdo_Anuales3 = Creditos.Sdo_Anuales[3]~
  rowObject.Sdo_Anuales4 = Creditos.Sdo_Anuales[4]~
  rowObject.Sdo_Anuales5 = Creditos.Sdo_Anuales[5]~
  rowObject.Sdo_Anuales6 = Creditos.Sdo_Anuales[6]~
  rowObject.Sdo_Anuales7 = Creditos.Sdo_Anuales[7]~
  rowObject.Sdo_Anuales8 = Creditos.Sdo_Anuales[8]~
  rowObject.Sdo_Anuales9 = Creditos.Sdo_Anuales[9]~
  rowObject.Sdo_Anuales10 = Creditos.Sdo_Anuales[10]~
  rowObject.Sdo_Anuales11 = Creditos.Sdo_Anuales[11]~
  rowObject.Sdo_Anuales12 = Creditos.Sdo_Anuales[12]
&Scoped-Define DATA-FIELD-DEFS "dcreditos.i"
&Scoped-Define DATA-TABLE-NO-UNDO NO-UNDO
&Scoped-define QUERY-STRING-Query-Main FOR EACH Creditos NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH Creditos NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main Creditos
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main Creditos


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDatosCliente dTables  _DB-REQUIRED
FUNCTION getDatosCliente RETURNS CHARACTER
  ( INPUT pcNit   AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDatosCodeudor dTables  _DB-REQUIRED
FUNCTION getDatosCodeudor RETURNS CHARACTER
  ( INPUT pcNit AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDirComCli dTables  _DB-REQUIRED
FUNCTION getDirComCli RETURNS CHARACTER
  ( INPUT pcNit   AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDirResCli dTables  _DB-REQUIRED
FUNCTION getDirResCli RETURNS CHARACTER
  ( INPUT pcNit   AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      Creditos SCROLLING.
&ANALYZE-RESUME
{&DB-REQUIRED-END}


/* ************************  Frame Definitions  *********************** */


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataObject
   Allow: Query
   Frames: 0
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER DB-AWARE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW dTables ASSIGN
         HEIGHT             = 1.62
         WIDTH              = 45.57.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB dTables 
/* ************************* Included-Libraries *********************** */

{src/adm2/data.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW dTables
  VISIBLE,,RUN-PERSISTENT                                               */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY Query-Main
/* Query rebuild information for SmartDataObject Query-Main
     _TblList          = "bdcentral.Creditos"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > bdcentral.Creditos.Abogado
"Abogado" "Abogado" ? ? "logical" ? ? ? ? ? ? yes ? no 8 yes ?
     _FldNameList[2]   > bdcentral.Creditos.Agencia
"Agencia" "Agencia" ? ? "integer" ? ? ? ? ? ? yes ? no 7.29 yes "Age"
     _FldNameList[3]   > bdcentral.Creditos.Age_DebAutomatico
"Age_DebAutomatico" "Age_DebAutomatico" ? ? "integer" ? ? ? ? ? ? yes ? no 18.43 yes ?
     _FldNameList[4]   > bdcentral.Creditos.Age_Desembolso
"Age_Desembolso" "Age_Desembolso" ? ? "integer" ? ? ? ? ? ? yes ? no 16.29 yes ?
     _FldNameList[5]   > bdcentral.Creditos.Capital_Acum
"Capital_Acum" "Capital_Acum" ? ? "decimal" ? ? ? ? ? ? yes ? no 18.86 yes ?
     _FldNameList[6]   > bdcentral.Creditos.Categoria
"Categoria" "Categoria" ? ? "character" ? ? ? ? ? ? yes ? no 8.86 yes ?
     _FldNameList[7]   > bdcentral.Creditos.CategoriaMes
"CategoriaMes" "CategoriaMes" "Cat. Mes" ? "character" ? ? ? ? ? ? yes ? no 8.86 yes ?
     _FldNameList[8]   > bdcentral.Creditos.Cod_Califica
"Cod_Califica" "Cod_Califica" "Calif. Cred." ? "integer" ? ? ? ? ? ? yes ? no 21.29 yes ?
     _FldNameList[9]   > bdcentral.Creditos.Cod_CalificaMes
"Cod_CalificaMes" "Cod_CalificaMes" "Calif. Cred. Mes" ? "integer" ? ? ? ? ? ? yes ? no 21.29 yes ?
     _FldNameList[10]   > bdcentral.Creditos.Cod_Credito
"Cod_Credito" "Cod_Credito" ? ? "integer" ? ? ? ? ? ? yes ? no 15.14 yes ?
     _FldNameList[11]   > bdcentral.Creditos.Cod_DebAutomatico
"Cod_DebAutomatico" "Cod_DebAutomatico" ? ? "integer" ? ? ? ? ? ? yes ? no 18.57 yes ?
     _FldNameList[12]   > bdcentral.Creditos.Cod_Desembolso
"Cod_Desembolso" "Cod_Desembolso" ? ? "integer" ? ? ? ? ? ? yes ? no 16.43 yes ?
     _FldNameList[13]   > bdcentral.Creditos.Costas
"Costas" "Costas" "Costas" ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[14]   > bdcentral.Creditos.Cta_Contable
"Cta_Contable" "Cta_Contable" "Cta_Contable" ? "character" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[15]   > bdcentral.Creditos.Cue_DebAutomatico
"Cue_DebAutomatico" "Cue_DebAutomatico" ? ? "character" ? ? ? ? ? ? yes ? no 18.57 yes ?
     _FldNameList[16]   > bdcentral.Creditos.Cue_Desembolso
"Cue_Desembolso" "Cue_Desembolso" ? ? "character" ? ? ? ? ? ? yes ? no 17 yes ?
     _FldNameList[17]   > bdcentral.Creditos.Cuota
"Cuota" "Cuota" "Cuota" ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[18]   > bdcentral.Creditos.Cuo_Atraso
"Cuo_Atraso" "Cuo_Atraso" ? ? "integer" ? ? ? ? ? ? yes ? no 10.57 yes ?
     _FldNameList[19]   > bdcentral.Creditos.Cuo_Pagadas
"Cuo_Pagadas" "Cuo_Pagadas" ? ? "integer" ? ? ? ? ? ? yes ? no 15.14 yes ?
     _FldNameList[20]   > bdcentral.Creditos.Deducible
"Deducible" "Deducible" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[21]   > bdcentral.Creditos.Desembolso
"Desembolso" "Desembolso" "Tipo Desembolso" ? "integer" ? ? ? ? ? ? yes ? no 19.14 yes ?
     _FldNameList[22]   > bdcentral.Creditos.Destino
"Destino" "Destino" ? ? "integer" ? ? ? ? ? ? yes ? no 7.14 yes ?
     _FldNameList[23]   > bdcentral.Creditos.Detalle_Estado
"Detalle_Estado" "Detalle_Estado" ? ? "integer" ? ? ? ? ? ? yes ? no 14.14 yes ?
     _FldNameList[24]   > bdcentral.Creditos.Dias_Atraso
"Dias_Atraso" "Dias_Atraso" ? ? "integer" ? ? ? ? ? ? yes ? no 11.29 yes ?
     _FldNameList[25]   > bdcentral.Creditos.Estado
"Estado" "Estado" ? ? "integer" ? ? ? ? ? ? yes ? no 6.43 yes ?
     _FldNameList[26]   > bdcentral.Creditos.Fec_Aprobacion
"Fec_Aprobacion" "Fec_Aprobacion" "Fec. Aprob." ? "date" ? ? ? ? ? ? yes ? no 10.29 yes ?
     _FldNameList[27]   > bdcentral.Creditos.Fec_Calificacion
"Fec_Calificacion" "Fec_Calificacion" "Fec. Calif." ? "date" ? ? ? ? ? ? yes ? no 16.86 yes ?
     _FldNameList[28]   > bdcentral.Creditos.Fec_CanceTotal
"Fec_CanceTotal" "Fec_CanceTotal" "Fec. Cancel. Tot" ? "date" ? ? ? ? ? ? yes ? no 25.43 yes ?
     _FldNameList[29]   > bdcentral.Creditos.Fec_Desembolso
"Fec_Desembolso" "Fec_Desembolso" "Fec. Desembolso" ? "date" ? ? ? ? ? ? yes ? no 16.43 yes ?
     _FldNameList[30]   > bdcentral.Creditos.Fec_DifCobro
"Fec_DifCobro" "Fec_DifCobro" "Fec. Dif. Cobro" ? "date" ? ? ? ? ? ? yes ? no 20.29 yes "Fec. Dif. Cobro"
     _FldNameList[31]   > bdcentral.Creditos.Fec_PagAnti
"Fec_PagAnti" "Fec_PagAnti" "Fec. Lim. Renueva Liq." ? "date" ? ? ? ? ? ? yes ? no 32 yes ?
     _FldNameList[32]   > bdcentral.Creditos.Fec_Pago
"Fec_Pago" "Fec_Pago" ? ? "date" ? ? ? ? ? ? yes ? no 10.86 yes "Fec. Pago"
     _FldNameList[33]   > bdcentral.Creditos.Fec_ProxLiquidacion
"Fec_ProxLiquidacion" "Fec_ProxLiquidacion" "Fec Ini Periodiciad" ? "date" ? ? ? ? ? ? yes ? no 22.14 yes ?
     _FldNameList[34]   > bdcentral.Creditos.Fec_Reestructurado
"Fec_Reestructurado" "Fec_Reestructurado" "Fec. Reestruct" ? "date" ? ? ? ? ? ? yes ? no 23.14 yes ?
     _FldNameList[35]   > bdcentral.Creditos.Fec_UltLiquidacion
"Fec_UltLiquidacion" "Fec_UltLiquidacion" ? ? "date" ? ? ? ? ? ? yes ? no 17.72 yes ?
     _FldNameList[36]   > bdcentral.Creditos.Fec_UltPago
"Fec_UltPago" "Fec_UltPago" "Fec. Ult.Pago" ? "date" ? ? ? ? ? ? yes ? no 17.43 yes "Fec. Ult.Pago"
     _FldNameList[37]   > bdcentral.Creditos.For_Interes
"For_Interes" "For_Interes" ? ? "integer" ? ? ? ? ? ? yes ? no 17.43 yes ?
     _FldNameList[38]   > bdcentral.Creditos.For_Pago
"For_Pago" "For_Pago" "Forma Pago" ? "integer" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[39]   > bdcentral.Creditos.Honorarios
"Honorarios" "Honorarios" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.43 yes ?
     _FldNameList[40]   > bdcentral.Creditos.Id_Adicionales
"Id_Adicionales" "Id_Adicionales" ? ? "integer" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[41]   > bdcentral.Creditos.Incremento
"Incremento" "Incremento" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[42]   > bdcentral.Creditos.Int_AntDesembolso
"Int_AntDesembolso" "Int_AntDesembolso" "Int Ant. Desemb" ? "decimal" ? ? ? ? ? ? yes ? no 22.72 yes ?
     _FldNameList[43]   > bdcentral.Creditos.Int_Anticipado
"Int_Anticipado" "Int_Anticipado" "Int. Anticipado" ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[44]   > bdcentral.Creditos.Int_Corrientes
"Int_Corrientes" "Int_Corrientes" "Int. Corrientes" ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[45]   > bdcentral.Creditos.Int_DifCobro
"Int_DifCobro" "Int_DifCobro" "Int. Dif. Cobro" ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[46]   > bdcentral.Creditos.Int_LiqAcum
"Int_LiqAcum" "Int_LiqAcum" "Int. Liq. Acum" ? "decimal" ? ? ? ? ? ? yes ? no 23.29 yes ?
     _FldNameList[47]   > bdcentral.Creditos.Int_MoraDifCob
"Int_MoraDifCob" "Int_MoraDifCob" "Int.Mora Dif.Cobro" ? "decimal" ? ? ? ? ? ? yes ? no 24.14 yes ?
     _FldNameList[48]   > bdcentral.Creditos.Int_MorCobrar
"Int_MorCobrar" "Int_MorCobrar" "Int.Mora por Cobrar" ? "decimal" ? ? ? ? ? ? yes ? no 24.86 yes ?
     _FldNameList[49]   > bdcentral.Creditos.Monto
"Monto" "Monto" "Monto" ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[50]   > bdcentral.Creditos.Nit
"Nit" "Nit" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[51]   > bdcentral.Creditos.Nit_Juzgado
"Nit_Juzgado" "Nit_Juzgado" ? ? "character" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[52]   > bdcentral.Creditos.Nom_Juzgado
"Nom_Juzgado" "Nom_Juzgado" ? ? "character" ? ? ? ? ? ? yes ? no 50 yes ?
     _FldNameList[53]   > bdcentral.Creditos.Num_Credito
"Num_Credito" "Num_Credito" "Num. Credito" ? "integer" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[54]   > bdcentral.Creditos.Num_Solicitud
"Num_Solicitud" "Num_Solicitud" "Num. Solic" ? "integer" ? ? ? ? ? ? yes ? no 11.86 yes "Num. Solic"
     _FldNameList[55]   > bdcentral.Creditos.Observaciones
"Observaciones" "Observaciones" ? ? "character" ? ? ? ? ? ? yes ? no 50 yes ?
     _FldNameList[56]   > bdcentral.Creditos.Pagare
"Pagare" "Pagare" ? ? "character" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[57]   > bdcentral.Creditos.Per_Gracia
"Per_Gracia" "Per_Gracia" "Per. Gracia" ? "integer" ? ? ? ? ? ? yes ? no 9.72 yes "Per. Gracia"
     _FldNameList[58]   > bdcentral.Creditos.Per_Pago
"Per_Pago" "Per_Pago" "Per. Pago" ? "integer" ? ? ? ? ? ? yes ? no 15.29 yes "Per. Pago"
     _FldNameList[59]   > bdcentral.Creditos.Plazo
"Plazo" "Plazo" ? ? "integer" ? ? ? ? ? ? yes ? no 5.29 yes ?
     _FldNameList[60]   > bdcentral.Creditos.Poliza
"Poliza" "Poliza" "Poliza" ? "integer" ? ? ? ? ? ? yes ? no 16.57 yes ?
     _FldNameList[61]   > bdcentral.Creditos.Polizas
"Polizas" "Polizas" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.43 yes ?
     _FldNameList[62]   > bdcentral.Creditos.Provision
"Provision" "Provision" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.43 yes ?
     _FldNameList[63]   > bdcentral.Creditos.Provision_Interes
"Provision_Interes" "Provision_Interes" ? ? "decimal" ? ? ? ? ? ? yes ? no 16.43 yes ?
     _FldNameList[64]   > bdcentral.Creditos.Provision_Otros
"Provision_Otros" "Provision_Otros" ? ? "decimal" ? ? ? ? ? ? yes ? no 26.57 yes ?
     _FldNameList[65]   > bdcentral.Creditos.Reestructurado
"Reestructurado" "Reestructurado" ? ? "integer" ? ? ? ? ? ? yes ? no 14.14 yes ?
     _FldNameList[66]   > bdcentral.Creditos.Sdo_Anuales[1]
"Sdo_Anuales[1]" "Sdo_Anuales1" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[67]   > bdcentral.Creditos.Sdo_Anuales[2]
"Sdo_Anuales[2]" "Sdo_Anuales2" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[68]   > bdcentral.Creditos.Sdo_Anuales[3]
"Sdo_Anuales[3]" "Sdo_Anuales3" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[69]   > bdcentral.Creditos.Sdo_Anuales[4]
"Sdo_Anuales[4]" "Sdo_Anuales4" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[70]   > bdcentral.Creditos.Sdo_Anuales[5]
"Sdo_Anuales[5]" "Sdo_Anuales5" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[71]   > bdcentral.Creditos.Sdo_Anuales[6]
"Sdo_Anuales[6]" "Sdo_Anuales6" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[72]   > bdcentral.Creditos.Sdo_Anuales[7]
"Sdo_Anuales[7]" "Sdo_Anuales7" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[73]   > bdcentral.Creditos.Sdo_Anuales[8]
"Sdo_Anuales[8]" "Sdo_Anuales8" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[74]   > bdcentral.Creditos.Sdo_Anuales[9]
"Sdo_Anuales[9]" "Sdo_Anuales9" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[75]   > bdcentral.Creditos.Sdo_Anuales[10]
"Sdo_Anuales[10]" "Sdo_Anuales10" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[76]   > bdcentral.Creditos.Sdo_Anuales[11]
"Sdo_Anuales[11]" "Sdo_Anuales11" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[77]   > bdcentral.Creditos.Sdo_Anuales[12]
"Sdo_Anuales[12]" "Sdo_Anuales12" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[78]   > bdcentral.Creditos.Sdo_Capital
"Sdo_Capital" "Sdo_Capital" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[79]   > bdcentral.Creditos.Sdo_CapPag
"Sdo_CapPag" "Sdo_CapPag" "Acum Capital Pago" ? "decimal" ? ? ? ? ? ? yes ? no 25.29 yes ?
     _FldNameList[80]   > bdcentral.Creditos.Sdo_IntMor
"Sdo_IntMor" "Sdo_IntMor" "Saldo Int. Mora" ? "decimal" ? ? ? ? ? ? yes ? no 19.86 yes ?
     _FldNameList[81]   > bdcentral.Creditos.Sdo_IntPag
"Sdo_IntPag" "Sdo_IntPag" "Saldo Int. pagados" ? "decimal" ? ? ? ? ? ? yes ? no 23.14 yes ?
     _FldNameList[82]   > bdcentral.Creditos.Sdo_Proyectado
"Sdo_Proyectado" "Sdo_Proyectado" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[83]   > bdcentral.Creditos.Sistema
"Sistema" "Sistema" "Sist. Liquid." ? "integer" ? ? ? ? ? ? yes ? no 18.86 yes ?
     _FldNameList[84]   > bdcentral.Creditos.Tasa
"Tasa" "Tasa" "Tasa" ? "decimal" ? ? ? ? ? ? yes ? no 15.14 yes ?
     _FldNameList[85]   > bdcentral.Creditos.Tip_Credito
"Tip_Credito" "Tip_Credito" "Tip Credito" ? "integer" ? ? ? ? ? ? yes ? no 24.14 yes ?
     _FldNameList[86]   > bdcentral.Creditos.Usuario
"Usuario" "Usuario" ? ? "character" ? ? ? ? ? ? yes ? no 7.29 yes ?
     _FldNameList[87]   > bdcentral.Creditos.Val_Atraso
"Val_Atraso" "Val_Atraso" ? ? "decimal" ? ? ? ? ? ? yes ? no 14.29 yes ?
     _FldNameList[88]   > bdcentral.Creditos.Val_Desembolso
"Val_Desembolso" "Val_Desembolso" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.72 yes ?
     _FldNameList[89]   > "_<CALC>"
"getAgencia(INPUT RowObject.Agencia)" "FAgencia" "Nom Agencia" "x(40)" "character" ? ? ? ? ? ? no ? no 40 no "Nom Agencia"
     _FldNameList[90]   > "_<CALC>"
"getCredito(INPUT RowObject.Cod_Credito)" "FCredito" "Nom. Credito" "x(35)" "character" ? ? ? ? ? ? no ? no 35 no "Nom. Credito"
     _FldNameList[91]   > "_<CALC>"
"getDatosCliente(Input RowObject.Nit)" "FNombre" "Nom. Cliente" "x(30)" "character" ? ? ? ? ? ? no ? no 30 no ?
     _FldNameList[92]   > "_<CALC>"
"getDirResCli(INPUT RowObject.Nit)" "FDirResCli" "Dir. Residencia" "x(40)" "character" ? ? ? ? ? ? no ? no 40 no ?
     _FldNameList[93]   > "_<CALC>"
"vcTelRes" "FTelResCli" "Tel. Resid" "x(10)" "character" ? ? ? ? ? ? no ? no 10 no ?
     _FldNameList[94]   > "_<CALC>"
"getDirComCli(INPUT RowObject.Nit)" "FDirComCli" "Dir. Comercial" "x(40)" "character" ? ? ? ? ? ? no ? no 40 no "Dir. Comercial"
     _FldNameList[95]   > "_<CALC>"
"vcTelCom" "FTelComCli" "Tel. Comercial" "x(10)" "character" ? ? ? ? ? ? no ? no 13.29 no "Tel. Comerc"
     _FldNameList[96]   > "_<CALC>"
"vcDirResCodA" "FDirResCodA" "Dir. Res. Cod1" "x(40)" "character" ? ? ? ? ? ? no ? no 40 no ?
     _FldNameList[97]   > "_<CALC>"
"vcTelResCodA" "FTelResCodA" "Tel. Res. Cod1" "X(10)" "character" ? ? ? ? ? ? no ? no 13 no ?
     _FldNameList[98]   > "_<CALC>"
"vcDirComCodA" "FDirComCodA" "Dir. Com. Cod1" "x(40)" "character" ? ? ? ? ? ? no ? no 40 no ?
     _FldNameList[99]   > "_<CALC>"
"vcTelComCodA" "FTelComCodA" "Tel. Com. Cod1" "x(10)" "character" ? ? ? ? ? ? no ? no 14 no ?
     _FldNameList[100]   > "_<CALC>"
"vcUbiResCodA" "FUbiResCodA" "Ubic. Res. Cod1" "x(20)" "character" ? ? ? ? ? ? no ? no 20 no ?
     _FldNameList[101]   > "_<CALC>"
"vcUbiComCodA" "FUbiComCodA" "Ubic. Com. Cod1" "x(20)" "character" ? ? ? ? ? ? no ? no 20 no ?
     _Design-Parent    is WINDOW dTables @ ( 1.27 , 2.29 )
*/  /* QUERY Query-Main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK dTables 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN initializeObject.
  &ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DATA.CALCULATE dTables  DATA.CALCULATE _DB-REQUIRED
PROCEDURE DATA.CALCULATE :
/*------------------------------------------------------------------------------
  Purpose:     Calculate all the Calculated Expressions found in the
               SmartDataObject.
  Parameters:  <none>
------------------------------------------------------------------------------*/
      ASSIGN 
         rowObject.FAgencia = (getAgencia(INPUT RowObject.Agencia))
         rowObject.FCredito = (getCredito(INPUT RowObject.Cod_Credito))
         rowObject.FDirComCli = (getDirComCli(INPUT RowObject.Nit))
         rowObject.FDirComCodA = (vcDirComCodA)
         rowObject.FDirResCli = (getDirResCli(INPUT RowObject.Nit))
         rowObject.FDirResCodA = (vcDirResCodA)
         rowObject.FNombre = (getDatosCliente(Input RowObject.Nit))
         rowObject.FTelComCli = (vcTelCom)
         rowObject.FTelComCodA = (vcTelComCodA)
         rowObject.FTelResCli = (vcTelRes)
         rowObject.FTelResCodA = (vcTelResCodA)
         rowObject.FUbiComCodA = (vcUbiComCodA)
         rowObject.FUbiResCodA = (vcUbiResCodA)
      .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE datosCodeudor dTables  _DB-REQUIRED
PROCEDURE datosCodeudor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER pcNit AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER pcNomCodA AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER pcDirResCodA AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER pcTelResCodA AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER pcDirComCodA AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER pcTelComCodA AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER pcUbiResCodA AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER pcUbiComCodA AS CHARACTER   NO-UNDO.

    DEFINE VARIABLE viCnt AS INTEGER     NO-UNDO.
    ASSIGN viCnt = 0.

    FOR EACH relaciones WHERE relaciones.nit EQ pcNit NO-LOCK:
        ASSIGN viCnt = 1.
        FIND FIRST clientes WHERE clientes.nit EQ relaciones.nit_relacion NO-LOCK NO-ERROR.
        IF AVAILABLE clientes THEN  DO:            
            CASE viCnt:
                WHEN 1 THEN DO:
                    ASSIGN 
                        vcNomCodA       = clientes.nombre + " " + clientes.apellido1 + " " + clientes.apellido2
                        vcDirResCodA    = clientes.Dir_residencia
                        vcTelResCodA    = clientes.Tel_residencia
                        vcDirComCodA    = clientes.Dir_residencia
                        vcTelComCodA    = clientes.Tel_residencia.
                    FIND FIRST ubicacion WHERE ubicacion.ubicacion EQ clientes.lugar_residencia NO-LOCK NO-ERROR.
                    IF AVAILABLE ubicacion THEN
                        ASSIGN vcUbiResCodA = ubicacion.nombre.
                    ELSE
                        ASSIGN vcUbiResCodA = "".

                    FIND FIRST ubicacion WHERE ubicacion.ubicacion EQ clientes.lugar_comercial NO-LOCK NO-ERROR.
                    IF AVAILABLE ubicacion THEN
                        ASSIGN vcUbiComCodA    = ubicacion.nombre.
                    ELSE
                        ASSIGN vcUbiComCodA    = "".
                END. /*WHEN 1*/
            END CASE. /*CASE viCnt*/
        END. /*IF AVAILABLE clientes*/
        ELSE DO:
            ASSIGN 
                vcNomCodA       = ""
                vcDirResCodA    = ""
                vcTelResCodA    = ""
                vcDirComCodA    = ""
                vcTelComCodA    = ""
                vcUbiResCodA    = ""
                vcUbiComCodA    = "".
        END.
        ASSIGN viCnt = viCnt + 1.
    END. /*FOR EACH relaciones*/

    ASSIGN
        pcNomCodA       = vcNomCodA   
        pcDirResCodA    = vcDirResCodA
        pcTelResCodA    = vcTelResCodA
        pcDirComCodA    = vcDirComCodA
        pcTelComCodA    = vcTelComCodA
        pcUbiResCodA    = vcUbiResCodA
        pcUbiComCodA    = vcUbiComCodA.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI dTables  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDatosCliente dTables  _DB-REQUIRED
FUNCTION getDatosCliente RETURNS CHARACTER
  ( INPUT pcNit   AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  devuelve valores para las variable de cliente
    Notes:  
------------------------------------------------------------------------------*/

    ASSIGN 
        vcNombre = ""
/*         vcDirRes = "" */
        vcTelRes = ""
        vcUbiRes = ""
/*         vcDirCom = "" */
        vcTelCom = ""
        vcUbiCom = "".

    FIND FIRST clientes WHERE clientes.nit EQ pcNit NO-LOCK NO-ERROR.
    IF AVAILABLE Clientes THEN DO:
        ASSIGN 
            vcNombre = clientes.nombre + " " + clientes.apellido1 + " " + clientes.apellido2
/*             vcDirRes = Clientes.Dir_Residencia */
            vcTelRes = Clientes.Tel_Residencia
            vcUbiRes = Clientes.Lugar_Residencia
/*             vcDirCom = Clientes.Dir_comercial */
            vcTelCom = Clientes.Tel_comercial
            vcUbiCom = Clientes.Lugar_comercial.
    END.
    ELSE DO:
        ASSIGN 
            vcNombre = ""
/*             vcDirRes = "" */
            vcTelRes = ""
            vcUbiRes = ""
/*             vcDirCom = "" */
            vcTelCom = ""
            vcUbiCom = "".
    END.


    RETURN vcNombre.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDatosCodeudor dTables  _DB-REQUIRED
FUNCTION getDatosCodeudor RETURNS CHARACTER
  ( INPUT pcNit AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

/*     DEFINE VARIABLE viCnt AS INTEGER     NO-UNDO.                                                                 */
/*     ASSIGN viCnt = 0.                                                                                             */
/*                                                                                                                   */
/*     FOR EACH relaciones WHERE relaciones.nit EQ pcNit NO-LOCK:                                                    */
/*         ASSIGN viCnt = 1.                                                                                         */
/*         FIND FIRST clientes WHERE clientes.nit EQ relaciones.nit_relacion NO-LOCK NO-ERROR.                       */
/*         IF AVAILABLE clientes THEN  DO:                                                                           */
/*             CASE viCnt:                                                                                           */
/*                 WHEN 1 THEN DO:                                                                                   */
/*                     ASSIGN                                                                                        */
/*                         vcNomCodA       = clientes.nombre + " " + clientes.apellido1 + " " + clientes.apellido2   */
/*                         vcDirResCodA    = clientes.Dir_residencia                                                 */
/*                         vcTelResCodA    = clientes.Tel_residencia                                                 */
/*                         vcDirComCodA    = clientes.Dir_residencia                                                 */
/*                         vcTelComCodA    = clientes.Tel_residencia.                                                */
/*                     FIND FIRST ubicacion WHERE ubicacion.ubicacion EQ clientes.lugar_residencia NO-LOCK NO-ERROR. */
/*                     IF AVAILABLE ubicacion THEN                                                                   */
/*                         ASSIGN vcUbiResCodA = ubicacion.nombre.                                                   */
/*                     ELSE                                                                                          */
/*                         ASSIGN vcUbiResCodA = "".                                                                 */
/*                                                                                                                   */
/*                     FIND FIRST ubicacion WHERE ubicacion.ubicacion EQ clientes.lugar_comercial NO-LOCK NO-ERROR.  */
/*                     IF AVAILABLE ubicacion THEN                                                                   */
/*                         ASSIGN vcUbiComCodA    = ubicacion.nombre.                                                */
/*                     ELSE                                                                                          */
/*                         ASSIGN vcUbiComCodA    = "".                                                              */
/*                 END. /*WHEN 1*/                                                                                   */
/*             END CASE. /*CASE viCnt*/                                                                              */
/*         END. /*IF AVAILABLE clientes*/                                                                            */
/*         ELSE DO:                                                                                                  */
/*             ASSIGN                                                                                                */
/*                 vcNomCodA       = ""                                                                              */
/*                 vcDirResCodA    = ""                                                                              */
/*                 vcTelResCodA    = ""                                                                              */
/*                 vcDirComCodA    = ""                                                                              */
/*                 vcTelComCodA    = ""                                                                              */
/*                 vcUbiResCodA    = ""                                                                              */
/*                 vcUbiComCodA    = "".                                                                             */
/*         END.                                                                                                      */
/*         ASSIGN viCnt = viCnt + 1.                                                                                 */
/*     END. /*FOR EACH relaciones*/                                                                                  */

    RUN datosCodeudor(INPUT  pcNit,
                      OUTPUT vcNomCodA,
                      OUTPUT vcDirResCodA,
                      OUTPUT vcTelResCodA,
                      OUTPUT vcDirComCodA,
                      OUTPUT vcTelComCodA,
                      OUTPUT vcUbiResCodA,
                      OUTPUT vcUbiComCodA).
    
/*     MESSAGE                                */
/*         pcNit           SKIP               */
/*         vcNomCodA       SKIP               */
/*         vcDirResCodA    SKIP               */
/*         vcTelResCodA    SKIP               */
/*         vcDirComCodA    SKIP               */
/*         vcTelComCodA    SKIP               */
/*         vcUbiResCodA    SKIP               */
/*         vcUbiComCodA                       */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK. */







    RETURN vcNomCodA.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDirComCli dTables  _DB-REQUIRED
FUNCTION getDirComCli RETURNS CHARACTER
  ( INPUT pcNit   AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  devuelve valores para las variable de cliente
    Notes:  
------------------------------------------------------------------------------*/
    FIND FIRST clientes WHERE clientes.nit EQ pcNit NO-LOCK NO-ERROR.
    IF AVAILABLE Clientes THEN
        ASSIGN 
            vcDirCom = Clientes.Dir_Comercial.
    ELSE
        ASSIGN 
            vcDirCom = "".

    RETURN vcDirCom.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDirResCli dTables  _DB-REQUIRED
FUNCTION getDirResCli RETURNS CHARACTER
  ( INPUT pcNit   AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  devuelve valores para las variable de cliente
    Notes:  
------------------------------------------------------------------------------*/
    FIND FIRST clientes WHERE clientes.nit EQ pcNit NO-LOCK NO-ERROR.
    IF AVAILABLE Clientes THEN
        ASSIGN 
            vcDirRes = Clientes.Dir_Residencia.
    ELSE
        ASSIGN 
            vcDirRes = "".

    RETURN vcDirRes.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

