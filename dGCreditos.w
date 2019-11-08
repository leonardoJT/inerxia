&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE GCreditos NO-UNDO LIKE Creditos
       fields Campo1 as character.



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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

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
&Scoped-define INTERNAL-TABLES GCreditos

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
&Scoped-define ENABLED-FIELDS-IN-GCreditos Abogado Agencia ~
Age_DebAutomatico Age_Desembolso Capital_Acum Categoria CategoriaMes ~
Cod_Califica Cod_CalificaMes Cod_Credito Cod_DebAutomatico Cod_Desembolso ~
Costas Cta_Contable Cue_DebAutomatico Cue_Desembolso Cuota Cuo_Atraso ~
Cuo_Pagadas Deducible Desembolso Destino Detalle_Estado Dias_Atraso Estado ~
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
 Usuario Val_Atraso Val_Desembolso
&Scoped-define DATA-FIELDS-IN-GCreditos Abogado Agencia Age_DebAutomatico ~
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
&Scoped-Define ASSIGN-LIST   rowObject.Sdo_Anuales1 = GCreditos.Sdo_Anuales[1]~
  rowObject.Sdo_Anuales2 = GCreditos.Sdo_Anuales[2]~
  rowObject.Sdo_Anuales3 = GCreditos.Sdo_Anuales[3]~
  rowObject.Sdo_Anuales4 = GCreditos.Sdo_Anuales[4]~
  rowObject.Sdo_Anuales5 = GCreditos.Sdo_Anuales[5]~
  rowObject.Sdo_Anuales6 = GCreditos.Sdo_Anuales[6]~
  rowObject.Sdo_Anuales7 = GCreditos.Sdo_Anuales[7]~
  rowObject.Sdo_Anuales8 = GCreditos.Sdo_Anuales[8]~
  rowObject.Sdo_Anuales9 = GCreditos.Sdo_Anuales[9]~
  rowObject.Sdo_Anuales10 = GCreditos.Sdo_Anuales[10]~
  rowObject.Sdo_Anuales11 = GCreditos.Sdo_Anuales[11]~
  rowObject.Sdo_Anuales12 = GCreditos.Sdo_Anuales[12]
&Scoped-Define DATA-FIELD-DEFS "dGCreditos.i"
&Scoped-Define DATA-TABLE-NO-UNDO NO-UNDO
&Scoped-define QUERY-STRING-Query-Main FOR EACH GCreditos NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH GCreditos NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main GCreditos
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main GCreditos


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      GCreditos SCROLLING.
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
   Temp-Tables and Buffers:
      TABLE: GCreditos T "?" NO-UNDO bdcentral Creditos
      ADDITIONAL-FIELDS:
          fields Campo1 as character
      END-FIELDS.
   END-TABLES.
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
         WIDTH              = 46.57.
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
     _TblList          = "Temp-Tables.GCreditos"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.GCreditos.Abogado
"Abogado" "Abogado" ? ? "logical" ? ? ? ? ? ? yes ? no 8 no ?
     _FldNameList[2]   > Temp-Tables.GCreditos.Agencia
"Agencia" "Agencia" ? ? "integer" ? ? ? ? ? ? yes ? no 7.29 no ?
     _FldNameList[3]   > Temp-Tables.GCreditos.Age_DebAutomatico
"Age_DebAutomatico" "Age_DebAutomatico" ? ? "integer" ? ? ? ? ? ? yes ? no 18.43 no ?
     _FldNameList[4]   > Temp-Tables.GCreditos.Age_Desembolso
"Age_Desembolso" "Age_Desembolso" ? ? "integer" ? ? ? ? ? ? yes ? no 16.29 no ?
     _FldNameList[5]   > Temp-Tables.GCreditos.Capital_Acum
"Capital_Acum" "Capital_Acum" ? ? "decimal" ? ? ? ? ? ? yes ? no 18.86 no ?
     _FldNameList[6]   > Temp-Tables.GCreditos.Categoria
"Categoria" "Categoria" ? ? "character" ? ? ? ? ? ? yes ? no 8.86 no ?
     _FldNameList[7]   > Temp-Tables.GCreditos.CategoriaMes
"CategoriaMes" "CategoriaMes" ? ? "character" ? ? ? ? ? ? yes ? no 8.86 no ?
     _FldNameList[8]   > Temp-Tables.GCreditos.Cod_Califica
"Cod_Califica" "Cod_Califica" ? ? "integer" ? ? ? ? ? ? yes ? no 21.29 no ?
     _FldNameList[9]   > Temp-Tables.GCreditos.Cod_CalificaMes
"Cod_CalificaMes" "Cod_CalificaMes" ? ? "integer" ? ? ? ? ? ? yes ? no 21.29 no ?
     _FldNameList[10]   > Temp-Tables.GCreditos.Cod_Credito
"Cod_Credito" "Cod_Credito" ? ? "integer" ? ? ? ? ? ? yes ? no 15.14 no ?
     _FldNameList[11]   > Temp-Tables.GCreditos.Cod_DebAutomatico
"Cod_DebAutomatico" "Cod_DebAutomatico" ? ? "integer" ? ? ? ? ? ? yes ? no 18.57 no ?
     _FldNameList[12]   > Temp-Tables.GCreditos.Cod_Desembolso
"Cod_Desembolso" "Cod_Desembolso" ? ? "integer" ? ? ? ? ? ? yes ? no 16.43 no ?
     _FldNameList[13]   > Temp-Tables.GCreditos.Costas
"Costas" "Costas" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 no ?
     _FldNameList[14]   > Temp-Tables.GCreditos.Cta_Contable
"Cta_Contable" "Cta_Contable" ? ? "character" ? ? ? ? ? ? yes ? no 14 no ?
     _FldNameList[15]   > Temp-Tables.GCreditos.Cue_DebAutomatico
"Cue_DebAutomatico" "Cue_DebAutomatico" ? ? "character" ? ? ? ? ? ? yes ? no 18.57 no ?
     _FldNameList[16]   > Temp-Tables.GCreditos.Cue_Desembolso
"Cue_Desembolso" "Cue_Desembolso" ? ? "character" ? ? ? ? ? ? yes ? no 17 no ?
     _FldNameList[17]   > Temp-Tables.GCreditos.Cuota
"Cuota" "Cuota" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 no ?
     _FldNameList[18]   > Temp-Tables.GCreditos.Cuo_Atraso
"Cuo_Atraso" "Cuo_Atraso" ? ? "integer" ? ? ? ? ? ? yes ? no 10.57 no ?
     _FldNameList[19]   > Temp-Tables.GCreditos.Cuo_Pagadas
"Cuo_Pagadas" "Cuo_Pagadas" ? ? "integer" ? ? ? ? ? ? yes ? no 15.14 no ?
     _FldNameList[20]   > Temp-Tables.GCreditos.Deducible
"Deducible" "Deducible" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 no ?
     _FldNameList[21]   > Temp-Tables.GCreditos.Desembolso
"Desembolso" "Desembolso" ? ? "integer" ? ? ? ? ? ? yes ? no 19.14 no ?
     _FldNameList[22]   > Temp-Tables.GCreditos.Destino
"Destino" "Destino" ? ? "integer" ? ? ? ? ? ? yes ? no 7.14 no ?
     _FldNameList[23]   > Temp-Tables.GCreditos.Detalle_Estado
"Detalle_Estado" "Detalle_Estado" ? ? "integer" ? ? ? ? ? ? yes ? no 14.14 no ?
     _FldNameList[24]   > Temp-Tables.GCreditos.Dias_Atraso
"Dias_Atraso" "Dias_Atraso" ? ? "integer" ? ? ? ? ? ? yes ? no 11.29 no ?
     _FldNameList[25]   > Temp-Tables.GCreditos.Estado
"Estado" "Estado" ? ? "integer" ? ? ? ? ? ? yes ? no 6.43 no ?
     _FldNameList[26]   > Temp-Tables.GCreditos.Fec_Aprobacion
"Fec_Aprobacion" "Fec_Aprobacion" ? ? "date" ? ? ? ? ? ? yes ? no 10.29 no ?
     _FldNameList[27]   > Temp-Tables.GCreditos.Fec_Calificacion
"Fec_Calificacion" "Fec_Calificacion" ? ? "date" ? ? ? ? ? ? yes ? no 16.86 no ?
     _FldNameList[28]   > Temp-Tables.GCreditos.Fec_CanceTotal
"Fec_CanceTotal" "Fec_CanceTotal" ? ? "date" ? ? ? ? ? ? yes ? no 25.43 no ?
     _FldNameList[29]   > Temp-Tables.GCreditos.Fec_Desembolso
"Fec_Desembolso" "Fec_Desembolso" ? ? "date" ? ? ? ? ? ? yes ? no 16.43 no ?
     _FldNameList[30]   > Temp-Tables.GCreditos.Fec_DifCobro
"Fec_DifCobro" "Fec_DifCobro" ? ? "date" ? ? ? ? ? ? yes ? no 20.29 no ?
     _FldNameList[31]   > Temp-Tables.GCreditos.Fec_PagAnti
"Fec_PagAnti" "Fec_PagAnti" ? ? "date" ? ? ? ? ? ? yes ? no 32 no ?
     _FldNameList[32]   > Temp-Tables.GCreditos.Fec_Pago
"Fec_Pago" "Fec_Pago" ? ? "date" ? ? ? ? ? ? yes ? no 10.86 no ?
     _FldNameList[33]   > Temp-Tables.GCreditos.Fec_ProxLiquidacion
"Fec_ProxLiquidacion" "Fec_ProxLiquidacion" ? ? "date" ? ? ? ? ? ? yes ? no 22.14 no ?
     _FldNameList[34]   > Temp-Tables.GCreditos.Fec_Reestructurado
"Fec_Reestructurado" "Fec_Reestructurado" ? ? "date" ? ? ? ? ? ? yes ? no 23.14 no ?
     _FldNameList[35]   > Temp-Tables.GCreditos.Fec_UltLiquidacion
"Fec_UltLiquidacion" "Fec_UltLiquidacion" ? ? "date" ? ? ? ? ? ? yes ? no 17.72 no ?
     _FldNameList[36]   > Temp-Tables.GCreditos.Fec_UltPago
"Fec_UltPago" "Fec_UltPago" ? ? "date" ? ? ? ? ? ? yes ? no 17.43 no ?
     _FldNameList[37]   > Temp-Tables.GCreditos.For_Interes
"For_Interes" "For_Interes" ? ? "integer" ? ? ? ? ? ? yes ? no 17.43 no ?
     _FldNameList[38]   > Temp-Tables.GCreditos.For_Pago
"For_Pago" "For_Pago" ? ? "integer" ? ? ? ? ? ? yes ? no 14 no ?
     _FldNameList[39]   > Temp-Tables.GCreditos.Honorarios
"Honorarios" "Honorarios" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.43 no ?
     _FldNameList[40]   > Temp-Tables.GCreditos.Id_Adicionales
"Id_Adicionales" "Id_Adicionales" ? ? "integer" ? ? ? ? ? ? yes ? no 12 no ?
     _FldNameList[41]   > Temp-Tables.GCreditos.Incremento
"Incremento" "Incremento" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 no ?
     _FldNameList[42]   > Temp-Tables.GCreditos.Int_AntDesembolso
"Int_AntDesembolso" "Int_AntDesembolso" ? ? "decimal" ? ? ? ? ? ? yes ? no 22.72 no ?
     _FldNameList[43]   > Temp-Tables.GCreditos.Int_Anticipado
"Int_Anticipado" "Int_Anticipado" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 no ?
     _FldNameList[44]   > Temp-Tables.GCreditos.Int_Corrientes
"Int_Corrientes" "Int_Corrientes" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 no ?
     _FldNameList[45]   > Temp-Tables.GCreditos.Int_DifCobro
"Int_DifCobro" "Int_DifCobro" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 no ?
     _FldNameList[46]   > Temp-Tables.GCreditos.Int_LiqAcum
"Int_LiqAcum" "Int_LiqAcum" ? ? "decimal" ? ? ? ? ? ? yes ? no 23.29 no ?
     _FldNameList[47]   > Temp-Tables.GCreditos.Int_MoraDifCob
"Int_MoraDifCob" "Int_MoraDifCob" ? ? "decimal" ? ? ? ? ? ? yes ? no 24.14 no ?
     _FldNameList[48]   > Temp-Tables.GCreditos.Int_MorCobrar
"Int_MorCobrar" "Int_MorCobrar" ? ? "decimal" ? ? ? ? ? ? yes ? no 24.86 no ?
     _FldNameList[49]   > Temp-Tables.GCreditos.Monto
"Monto" "Monto" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 no ?
     _FldNameList[50]   > Temp-Tables.GCreditos.Nit
"Nit" "Nit" ? ? "character" ? ? ? ? ? ? yes ? no 12 no ?
     _FldNameList[51]   > Temp-Tables.GCreditos.Nit_Juzgado
"Nit_Juzgado" "Nit_Juzgado" ? ? "character" ? ? ? ? ? ? yes ? no 14 no ?
     _FldNameList[52]   > Temp-Tables.GCreditos.Nom_Juzgado
"Nom_Juzgado" "Nom_Juzgado" ? ? "character" ? ? ? ? ? ? yes ? no 50 no ?
     _FldNameList[53]   > Temp-Tables.GCreditos.Num_Credito
"Num_Credito" "Num_Credito" ? ? "integer" ? ? ? ? ? ? yes ? no 12 no ?
     _FldNameList[54]   > Temp-Tables.GCreditos.Num_Solicitud
"Num_Solicitud" "Num_Solicitud" ? ? "integer" ? ? ? ? ? ? yes ? no 11.86 no ?
     _FldNameList[55]   > Temp-Tables.GCreditos.Observaciones
"Observaciones" "Observaciones" ? ? "character" ? ? ? ? ? ? yes ? no 50 no ?
     _FldNameList[56]   > Temp-Tables.GCreditos.Pagare
"Pagare" "Pagare" ? ? "character" ? ? ? ? ? ? yes ? no 14 no ?
     _FldNameList[57]   > Temp-Tables.GCreditos.Per_Gracia
"Per_Gracia" "Per_Gracia" ? ? "integer" ? ? ? ? ? ? yes ? no 9.72 no ?
     _FldNameList[58]   > Temp-Tables.GCreditos.Per_Pago
"Per_Pago" "Per_Pago" ? ? "integer" ? ? ? ? ? ? yes ? no 15.29 no ?
     _FldNameList[59]   > Temp-Tables.GCreditos.Plazo
"Plazo" "Plazo" ? ? "integer" ? ? ? ? ? ? yes ? no 5.29 no ?
     _FldNameList[60]   > Temp-Tables.GCreditos.Poliza
"Poliza" "Poliza" ? ? "integer" ? ? ? ? ? ? yes ? no 16.57 no ?
     _FldNameList[61]   > Temp-Tables.GCreditos.Polizas
"Polizas" "Polizas" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.43 no ?
     _FldNameList[62]   > Temp-Tables.GCreditos.Provision
"Provision" "Provision" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.43 no ?
     _FldNameList[63]   > Temp-Tables.GCreditos.Provision_Interes
"Provision_Interes" "Provision_Interes" ? ? "decimal" ? ? ? ? ? ? yes ? no 16.43 no ?
     _FldNameList[64]   > Temp-Tables.GCreditos.Provision_Otros
"Provision_Otros" "Provision_Otros" ? ? "decimal" ? ? ? ? ? ? yes ? no 26.57 no ?
     _FldNameList[65]   > Temp-Tables.GCreditos.Reestructurado
"Reestructurado" "Reestructurado" ? ? "integer" ? ? ? ? ? ? yes ? no 14.14 no ?
     _FldNameList[66]   > Temp-Tables.GCreditos.Sdo_Anuales[1]
"Sdo_Anuales[1]" "Sdo_Anuales1" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 no ?
     _FldNameList[67]   > Temp-Tables.GCreditos.Sdo_Anuales[2]
"Sdo_Anuales[2]" "Sdo_Anuales2" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 no ?
     _FldNameList[68]   > Temp-Tables.GCreditos.Sdo_Anuales[3]
"Sdo_Anuales[3]" "Sdo_Anuales3" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 no ?
     _FldNameList[69]   > Temp-Tables.GCreditos.Sdo_Anuales[4]
"Sdo_Anuales[4]" "Sdo_Anuales4" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 no ?
     _FldNameList[70]   > Temp-Tables.GCreditos.Sdo_Anuales[5]
"Sdo_Anuales[5]" "Sdo_Anuales5" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 no ?
     _FldNameList[71]   > Temp-Tables.GCreditos.Sdo_Anuales[6]
"Sdo_Anuales[6]" "Sdo_Anuales6" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 no ?
     _FldNameList[72]   > Temp-Tables.GCreditos.Sdo_Anuales[7]
"Sdo_Anuales[7]" "Sdo_Anuales7" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 no ?
     _FldNameList[73]   > Temp-Tables.GCreditos.Sdo_Anuales[8]
"Sdo_Anuales[8]" "Sdo_Anuales8" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 no ?
     _FldNameList[74]   > Temp-Tables.GCreditos.Sdo_Anuales[9]
"Sdo_Anuales[9]" "Sdo_Anuales9" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 no ?
     _FldNameList[75]   > Temp-Tables.GCreditos.Sdo_Anuales[10]
"Sdo_Anuales[10]" "Sdo_Anuales10" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 no ?
     _FldNameList[76]   > Temp-Tables.GCreditos.Sdo_Anuales[11]
"Sdo_Anuales[11]" "Sdo_Anuales11" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 no ?
     _FldNameList[77]   > Temp-Tables.GCreditos.Sdo_Anuales[12]
"Sdo_Anuales[12]" "Sdo_Anuales12" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 no ?
     _FldNameList[78]   > Temp-Tables.GCreditos.Sdo_Capital
"Sdo_Capital" "Sdo_Capital" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 no ?
     _FldNameList[79]   > Temp-Tables.GCreditos.Sdo_CapPag
"Sdo_CapPag" "Sdo_CapPag" ? ? "decimal" ? ? ? ? ? ? yes ? no 25.29 no ?
     _FldNameList[80]   > Temp-Tables.GCreditos.Sdo_IntMor
"Sdo_IntMor" "Sdo_IntMor" ? ? "decimal" ? ? ? ? ? ? yes ? no 19.86 no ?
     _FldNameList[81]   > Temp-Tables.GCreditos.Sdo_IntPag
"Sdo_IntPag" "Sdo_IntPag" ? ? "decimal" ? ? ? ? ? ? yes ? no 23.14 no ?
     _FldNameList[82]   > Temp-Tables.GCreditos.Sdo_Proyectado
"Sdo_Proyectado" "Sdo_Proyectado" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 no ?
     _FldNameList[83]   > Temp-Tables.GCreditos.Sistema
"Sistema" "Sistema" ? ? "integer" ? ? ? ? ? ? yes ? no 18.86 no ?
     _FldNameList[84]   > Temp-Tables.GCreditos.Tasa
"Tasa" "Tasa" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.14 no ?
     _FldNameList[85]   > Temp-Tables.GCreditos.Tip_Credito
"Tip_Credito" "Tip_Credito" ? ? "integer" ? ? ? ? ? ? yes ? no 24.14 no ?
     _FldNameList[86]   > Temp-Tables.GCreditos.Usuario
"Usuario" "Usuario" ? ? "character" ? ? ? ? ? ? yes ? no 7.29 no ?
     _FldNameList[87]   > Temp-Tables.GCreditos.Val_Atraso
"Val_Atraso" "Val_Atraso" ? ? "decimal" ? ? ? ? ? ? yes ? no 14.29 no ?
     _FldNameList[88]   > Temp-Tables.GCreditos.Val_Desembolso
"Val_Desembolso" "Val_Desembolso" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.72 no ?
     _Design-Parent    is WINDOW dTables @ ( 1.15 , 2.57 )
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE creaTemporal dTables  _DB-REQUIRED
PROCEDURE creaTemporal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    FOR EACH Creditos WHERE Creditos.estado EQ 2 NO-LOCK:
        CREATE GCreditos.
        BUFFER-COPY Creditos TO GCreditos.
    END.

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

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject dTables  _DB-REQUIRED
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

  RUN creaTemporal.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

