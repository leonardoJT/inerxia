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

&glob DATA-LOGIC-PROCEDURE .p

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
&Scoped-define INTERNAL-TABLES Pro_Ahorros

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  Abo_Cuenta Bas_Calculo Cod_ahorro Cod_CobroCheque Cod_CobroLibreta~
 Cod_Formato Cta_GtoGMF Cta_Revaloriz Cta_XPagarGMF Dia_Canje Dia_Gracia~
 Estado Fec_Matricula Fec_Retiro For_Liquidacion Id_AfeSdoMinimo Id_Asociado~
 Id_BloInactividad Id_CanCalIntereses Id_CobroTal Id_Consecutivo Id_Cuota~
 Id_Debito Id_Embargo Id_Extracto Id_ForLiquidacion Id_GMF Id_Linea~
 Id_MonApertura Id_Montomaximo Id_Montominimo Id_NumAlterno Id_PerGracia~
 Id_perliquidacion Id_Pignoracion Id_Plazo Id_RenVencimiento Id_Retparcial~
 Id_Revaloriza Id_Salminimo Id_SdoMinLiquidacion Id_Seguro Id_Sobregiro~
 Id_Sorteos Id_Talonario Id_Tasa Id_Vencimiento Id_VerSdoTaq Indicador~
 Mon_MinLiqidacion Nom_Producto Nro_CheqACobrar Num_Consecutivo Num_SMSeguro~
 Periodicidad Per_Liquidacion Per_Pago Pla_Maximo Pla_Minimo Porce_Embargo~
 Porce_Pignoracion Prioridad ProCre_Asociado Pro_Digito Tie_Inactividad~
 Tip_Ahorro Tip_Anualidad Tip_Interes Tip_Salminimo Tip_Vencimiento~
 Titulo_Valor Val_CadaCheque Val_Cuota Val_Embargo Val_MaxConsignacion~
 Val_MaxRetEfectivo Val_Minconsignacion Val_MinRetcheque Val_MinRetiro~
 Val_MonAper Val_Pignoracion Val_SdoMinimo Val_Talonario VrTope_ExentoEE
&Scoped-define ENABLED-FIELDS-IN-Pro_Ahorros Abo_Cuenta Bas_Calculo ~
Cod_ahorro Cod_CobroCheque Cod_CobroLibreta Cod_Formato Cta_GtoGMF ~
Cta_Revaloriz Cta_XPagarGMF Dia_Canje Dia_Gracia Estado Fec_Matricula ~
Fec_Retiro For_Liquidacion Id_AfeSdoMinimo Id_Asociado Id_BloInactividad ~
Id_CanCalIntereses Id_CobroTal Id_Consecutivo Id_Cuota Id_Debito Id_Embargo ~
Id_Extracto Id_ForLiquidacion Id_GMF Id_Linea Id_MonApertura Id_Montomaximo ~
Id_Montominimo Id_NumAlterno Id_PerGracia Id_perliquidacion Id_Pignoracion ~
Id_Plazo Id_RenVencimiento Id_Retparcial Id_Revaloriza Id_Salminimo ~
Id_SdoMinLiquidacion Id_Seguro Id_Sobregiro Id_Sorteos Id_Talonario Id_Tasa ~
Id_Vencimiento Id_VerSdoTaq Indicador Mon_MinLiqidacion Nom_Producto ~
Nro_CheqACobrar Num_Consecutivo Num_SMSeguro Periodicidad Per_Liquidacion ~
Per_Pago Pla_Maximo Pla_Minimo Porce_Embargo Porce_Pignoracion Prioridad ~
ProCre_Asociado Pro_Digito Tie_Inactividad Tip_Ahorro Tip_Anualidad ~
Tip_Interes Tip_Salminimo Tip_Vencimiento Titulo_Valor Val_CadaCheque ~
Val_Cuota Val_Embargo Val_MaxConsignacion Val_MaxRetEfectivo ~
Val_Minconsignacion Val_MinRetcheque Val_MinRetiro Val_MonAper ~
Val_Pignoracion Val_SdoMinimo Val_Talonario VrTope_ExentoEE 
&Scoped-Define DATA-FIELDS  Abo_Cuenta Bas_Calculo Cod_ahorro Cod_CobroCheque Cod_CobroLibreta~
 Cod_Formato Cta_GtoGMF Cta_Revaloriz Cta_XPagarGMF Dia_Canje Dia_Gracia~
 Estado Fec_Matricula Fec_Retiro For_Liquidacion Id_AfeSdoMinimo Id_Asociado~
 Id_BloInactividad Id_CanCalIntereses Id_CobroTal Id_Consecutivo Id_Cuota~
 Id_Debito Id_Embargo Id_Extracto Id_ForLiquidacion Id_GMF Id_Linea~
 Id_MonApertura Id_Montomaximo Id_Montominimo Id_NumAlterno Id_PerGracia~
 Id_perliquidacion Id_Pignoracion Id_Plazo Id_RenVencimiento Id_Retparcial~
 Id_Revaloriza Id_Salminimo Id_SdoMinLiquidacion Id_Seguro Id_Sobregiro~
 Id_Sorteos Id_Talonario Id_Tasa Id_Vencimiento Id_VerSdoTaq Indicador~
 Mon_MinLiqidacion Nom_Producto Nro_CheqACobrar Num_Consecutivo Num_SMSeguro~
 Periodicidad Per_Liquidacion Per_Pago Pla_Maximo Pla_Minimo Porce_Embargo~
 Porce_Pignoracion Prioridad ProCre_Asociado Pro_Digito Tie_Inactividad~
 Tip_Ahorro Tip_Anualidad Tip_Interes Tip_Salminimo Tip_Vencimiento~
 Titulo_Valor Val_CadaCheque Val_Cuota Val_Embargo Val_MaxConsignacion~
 Val_MaxRetEfectivo Val_Minconsignacion Val_MinRetcheque Val_MinRetiro~
 Val_MonAper Val_Pignoracion Val_SdoMinimo Val_Talonario VrTope_ExentoEE
&Scoped-define DATA-FIELDS-IN-Pro_Ahorros Abo_Cuenta Bas_Calculo Cod_ahorro ~
Cod_CobroCheque Cod_CobroLibreta Cod_Formato Cta_GtoGMF Cta_Revaloriz ~
Cta_XPagarGMF Dia_Canje Dia_Gracia Estado Fec_Matricula Fec_Retiro ~
For_Liquidacion Id_AfeSdoMinimo Id_Asociado Id_BloInactividad ~
Id_CanCalIntereses Id_CobroTal Id_Consecutivo Id_Cuota Id_Debito Id_Embargo ~
Id_Extracto Id_ForLiquidacion Id_GMF Id_Linea Id_MonApertura Id_Montomaximo ~
Id_Montominimo Id_NumAlterno Id_PerGracia Id_perliquidacion Id_Pignoracion ~
Id_Plazo Id_RenVencimiento Id_Retparcial Id_Revaloriza Id_Salminimo ~
Id_SdoMinLiquidacion Id_Seguro Id_Sobregiro Id_Sorteos Id_Talonario Id_Tasa ~
Id_Vencimiento Id_VerSdoTaq Indicador Mon_MinLiqidacion Nom_Producto ~
Nro_CheqACobrar Num_Consecutivo Num_SMSeguro Periodicidad Per_Liquidacion ~
Per_Pago Pla_Maximo Pla_Minimo Porce_Embargo Porce_Pignoracion Prioridad ~
ProCre_Asociado Pro_Digito Tie_Inactividad Tip_Ahorro Tip_Anualidad ~
Tip_Interes Tip_Salminimo Tip_Vencimiento Titulo_Valor Val_CadaCheque ~
Val_Cuota Val_Embargo Val_MaxConsignacion Val_MaxRetEfectivo ~
Val_Minconsignacion Val_MinRetcheque Val_MinRetiro Val_MonAper ~
Val_Pignoracion Val_SdoMinimo Val_Talonario VrTope_ExentoEE 
&Scoped-Define MANDATORY-FIELDS  Cod_ahorro Prioridad Tip_Ahorro
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "H:\prg\dPro_Ahorros.i"
&Scoped-Define DATA-TABLE-NO-UNDO NO-UNDO
&Scoped-define QUERY-STRING-Query-Main FOR EACH Pro_Ahorros NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH Pro_Ahorros NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main Pro_Ahorros
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main Pro_Ahorros


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      Pro_Ahorros SCROLLING.
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
         WIDTH              = 24.86.
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
     _TblList          = "bdcentral.Pro_Ahorros"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > bdcentral.Pro_Ahorros.Abo_Cuenta
"Abo_Cuenta" "Abo_Cuenta" ? ? "integer" ? ? ? ? ? ? yes ? no 12.72 yes ?
     _FldNameList[2]   > bdcentral.Pro_Ahorros.Bas_Calculo
"Bas_Calculo" "Bas_Calculo" ? ? "integer" ? ? ? ? ? ? yes ? no 17.57 yes ?
     _FldNameList[3]   > bdcentral.Pro_Ahorros.Cod_ahorro
"Cod_ahorro" "Cod_ahorro" ? ? "integer" ? ? ? ? ? ? yes ? yes 15.14 yes ?
     _FldNameList[4]   > bdcentral.Pro_Ahorros.Cod_CobroCheque
"Cod_CobroCheque" "Cod_CobroCheque" ? ? "integer" ? ? ? ? ? ? yes ? no 16.43 yes ?
     _FldNameList[5]   > bdcentral.Pro_Ahorros.Cod_CobroLibreta
"Cod_CobroLibreta" "Cod_CobroLibreta" ? ? "integer" ? ? ? ? ? ? yes ? no 16.43 yes ?
     _FldNameList[6]   > bdcentral.Pro_Ahorros.Cod_Formato
"Cod_Formato" "Cod_Formato" ? ? "integer" ? ? ? ? ? ? yes ? no 17.43 yes ?
     _FldNameList[7]   > bdcentral.Pro_Ahorros.Cta_GtoGMF
"Cta_GtoGMF" "Cta_GtoGMF" ? ? "character" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[8]   > bdcentral.Pro_Ahorros.Cta_Revaloriz
"Cta_Revaloriz" "Cta_Revaloriz" ? ? "character" ? ? ? ? ? ? yes ? no 16.29 yes ?
     _FldNameList[9]   > bdcentral.Pro_Ahorros.Cta_XPagarGMF
"Cta_XPagarGMF" "Cta_XPagarGMF" ? ? "character" ? ? ? ? ? ? yes ? no 15.14 yes ?
     _FldNameList[10]   > bdcentral.Pro_Ahorros.Dia_Canje
"Dia_Canje" "Dia_Canje" ? ? "integer" ? ? ? ? ? ? yes ? no 14.86 yes ?
     _FldNameList[11]   > bdcentral.Pro_Ahorros.Dia_Gracia
"Dia_Gracia" "Dia_Gracia" ? ? "integer" ? ? ? ? ? ? yes ? no 4.29 yes ?
     _FldNameList[12]   > bdcentral.Pro_Ahorros.Estado
"Estado" "Estado" ? ? "integer" ? ? ? ? ? ? yes ? no 6.43 yes ?
     _FldNameList[13]   > bdcentral.Pro_Ahorros.Fec_Matricula
"Fec_Matricula" "Fec_Matricula" ? ? "date" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[14]   > bdcentral.Pro_Ahorros.Fec_Retiro
"Fec_Retiro" "Fec_Retiro" ? ? "date" ? ? ? ? ? ? yes ? no 11.72 yes ?
     _FldNameList[15]   > bdcentral.Pro_Ahorros.For_Liquidacion
"For_Liquidacion" "For_Liquidacion" ? ? "integer" ? ? ? ? ? ? yes ? no 17.14 yes ?
     _FldNameList[16]   > bdcentral.Pro_Ahorros.Id_AfeSdoMinimo
"Id_AfeSdoMinimo" "Id_AfeSdoMinimo" ? ? "logical" ? ? ? ? ? ? yes ? no 18.86 yes ?
     _FldNameList[17]   > bdcentral.Pro_Ahorros.Id_Asociado
"Id_Asociado" "Id_Asociado" ? ? "integer" ? ? ? ? ? ? yes ? no 23 yes ?
     _FldNameList[18]   > bdcentral.Pro_Ahorros.Id_BloInactividad
"Id_BloInactividad" "Id_BloInactividad" ? ? "logical" ? ? ? ? ? ? yes ? no 22 yes ?
     _FldNameList[19]   > bdcentral.Pro_Ahorros.Id_CanCalIntereses
"Id_CanCalIntereses" "Id_CanCalIntereses" ? ? "logical" ? ? ? ? ? ? yes ? no 21.86 yes ?
     _FldNameList[20]   > bdcentral.Pro_Ahorros.Id_CobroTal
"Id_CobroTal" "Id_CobroTal" ? ? "logical" ? ? ? ? ? ? yes ? no 14.72 yes ?
     _FldNameList[21]   > bdcentral.Pro_Ahorros.Id_Consecutivo
"Id_Consecutivo" "Id_Consecutivo" ? ? "logical" ? ? ? ? ? ? yes ? no 11.43 yes ?
     _FldNameList[22]   > bdcentral.Pro_Ahorros.Id_Cuota
"Id_Cuota" "Id_Cuota" ? ? "logical" ? ? ? ? ? ? yes ? no 11.86 yes ?
     _FldNameList[23]   > bdcentral.Pro_Ahorros.Id_Debito
"Id_Debito" "Id_Debito" ? ? "logical" ? ? ? ? ? ? yes ? no 16.72 yes ?
     _FldNameList[24]   > bdcentral.Pro_Ahorros.Id_Embargo
"Id_Embargo" "Id_Embargo" ? ? "logical" ? ? ? ? ? ? yes ? no 15.72 yes ?
     _FldNameList[25]   > bdcentral.Pro_Ahorros.Id_Extracto
"Id_Extracto" "Id_Extracto" ? ? "logical" ? ? ? ? ? ? yes ? no 7.57 yes ?
     _FldNameList[26]   > bdcentral.Pro_Ahorros.Id_ForLiquidacion
"Id_ForLiquidacion" "Id_ForLiquidacion" ? ? "integer" ? ? ? ? ? ? yes ? no 17.14 yes ?
     _FldNameList[27]   > bdcentral.Pro_Ahorros.Id_GMF
"Id_GMF" "Id_GMF" ? ? "integer" ? ? ? ? ? ? yes ? no 7.14 yes ?
     _FldNameList[28]   > bdcentral.Pro_Ahorros.Id_Linea
"Id_Linea" "Id_Linea" ? ? "logical" ? ? ? ? ? ? yes ? no 15.57 yes ?
     _FldNameList[29]   > bdcentral.Pro_Ahorros.Id_MonApertura
"Id_MonApertura" "Id_MonApertura" ? ? "logical" ? ? ? ? ? ? yes ? no 21.43 yes ?
     _FldNameList[30]   > bdcentral.Pro_Ahorros.Id_Montomaximo
"Id_Montomaximo" "Id_Montomaximo" ? ? "logical" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[31]   > bdcentral.Pro_Ahorros.Id_Montominimo
"Id_Montominimo" "Id_Montominimo" ? ? "logical" ? ? ? ? ? ? yes ? no 20.57 yes ?
     _FldNameList[32]   > bdcentral.Pro_Ahorros.Id_NumAlterno
"Id_NumAlterno" "Id_NumAlterno" ? ? "logical" ? ? ? ? ? ? yes ? no 20.86 yes ?
     _FldNameList[33]   > bdcentral.Pro_Ahorros.Id_PerGracia
"Id_PerGracia" "Id_PerGracia" ? ? "logical" ? ? ? ? ? ? yes ? no 16.57 yes ?
     _FldNameList[34]   > bdcentral.Pro_Ahorros.Id_perliquidacion
"Id_perliquidacion" "Id_perliquidacion" ? ? "integer" ? ? ? ? ? ? yes ? no 14.29 yes ?
     _FldNameList[35]   > bdcentral.Pro_Ahorros.Id_Pignoracion
"Id_Pignoracion" "Id_Pignoracion" ? ? "logical" ? ? ? ? ? ? yes ? no 18.43 yes ?
     _FldNameList[36]   > bdcentral.Pro_Ahorros.Id_Plazo
"Id_Plazo" "Id_Plazo" ? ? "logical" ? ? ? ? ? ? yes ? no 5.29 yes ?
     _FldNameList[37]   > bdcentral.Pro_Ahorros.Id_RenVencimiento
"Id_RenVencimiento" "Id_RenVencimiento" ? ? "logical" ? ? ? ? ? ? yes ? no 30.29 yes ?
     _FldNameList[38]   > bdcentral.Pro_Ahorros.Id_Retparcial
"Id_Retparcial" "Id_Retparcial" ? ? "logical" ? ? ? ? ? ? yes ? no 12.57 yes ?
     _FldNameList[39]   > bdcentral.Pro_Ahorros.Id_Revaloriza
"Id_Revaloriza" "Id_Revaloriza" ? ? "logical" ? ? ? ? ? ? yes ? no 21.29 yes ?
     _FldNameList[40]   > bdcentral.Pro_Ahorros.Id_Salminimo
"Id_Salminimo" "Id_Salminimo" ? ? "logical" ? ? ? ? ? ? yes ? no 20.14 yes ?
     _FldNameList[41]   > bdcentral.Pro_Ahorros.Id_SdoMinLiquidacion
"Id_SdoMinLiquidacion" "Id_SdoMinLiquidacion" ? ? "logical" ? ? ? ? ? ? yes ? no 24 yes ?
     _FldNameList[42]   > bdcentral.Pro_Ahorros.Id_Seguro
"Id_Seguro" "Id_Seguro" ? ? "logical" ? ? ? ? ? ? yes ? no 23.86 yes ?
     _FldNameList[43]   > bdcentral.Pro_Ahorros.Id_Sobregiro
"Id_Sobregiro" "Id_Sobregiro" ? ? "logical" ? ? ? ? ? ? yes ? no 16.43 yes ?
     _FldNameList[44]   > bdcentral.Pro_Ahorros.Id_Sorteos
"Id_Sorteos" "Id_Sorteos" ? ? "logical" ? ? ? ? ? ? yes ? no 14.57 yes ?
     _FldNameList[45]   > bdcentral.Pro_Ahorros.Id_Talonario
"Id_Talonario" "Id_Talonario" ? ? "integer" ? ? ? ? ? ? yes ? no 8.72 yes ?
     _FldNameList[46]   > bdcentral.Pro_Ahorros.Id_Tasa
"Id_Tasa" "Id_Tasa" ? ? "integer" ? ? ? ? ? ? yes ? no 24.72 yes ?
     _FldNameList[47]   > bdcentral.Pro_Ahorros.Id_Vencimiento
"Id_Vencimiento" "Id_Vencimiento" ? ? "logical" ? ? ? ? ? ? yes ? no 18.72 yes ?
     _FldNameList[48]   > bdcentral.Pro_Ahorros.Id_VerSdoTaq
"Id_VerSdoTaq" "Id_VerSdoTaq" ? ? "logical" ? ? ? ? ? ? yes ? no 26.14 yes ?
     _FldNameList[49]   > bdcentral.Pro_Ahorros.Indicador
"Indicador" "Indicador" ? ? "integer" ? ? ? ? ? ? yes ? no 18.43 yes ?
     _FldNameList[50]   > bdcentral.Pro_Ahorros.Mon_MinLiqidacion
"Mon_MinLiqidacion" "Mon_MinLiqidacion" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.43 yes ?
     _FldNameList[51]   > bdcentral.Pro_Ahorros.Nom_Producto
"Nom_Producto" "Nom_Producto" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes ?
     _FldNameList[52]   > bdcentral.Pro_Ahorros.Nro_CheqACobrar
"Nro_CheqACobrar" "Nro_CheqACobrar" ? ? "integer" ? ? ? ? ? ? yes ? no 17.57 yes ?
     _FldNameList[53]   > bdcentral.Pro_Ahorros.Num_Consecutivo
"Num_Consecutivo" "Num_Consecutivo" ? ? "integer" ? ? ? ? ? ? yes ? no 19.29 yes ?
     _FldNameList[54]   > bdcentral.Pro_Ahorros.Num_SMSeguro
"Num_SMSeguro" "Num_SMSeguro" ? ? "integer" ? ? ? ? ? ? yes ? no 37.43 yes ?
     _FldNameList[55]   > bdcentral.Pro_Ahorros.Periodicidad
"Periodicidad" "Periodicidad" ? ? "integer" ? ? ? ? ? ? yes ? no 22.14 yes ?
     _FldNameList[56]   > bdcentral.Pro_Ahorros.Per_Liquidacion
"Per_Liquidacion" "Per_Liquidacion" ? ? "integer" ? ? ? ? ? ? yes ? no 14.43 yes ?
     _FldNameList[57]   > bdcentral.Pro_Ahorros.Per_Pago
"Per_Pago" "Per_Pago" ? ? "integer" ? ? ? ? ? ? yes ? no 15.29 yes ?
     _FldNameList[58]   > bdcentral.Pro_Ahorros.Pla_Maximo
"Pla_Maximo" "Pla_Maximo" ? ? "integer" ? ? ? ? ? ? yes ? no 13.29 yes ?
     _FldNameList[59]   > bdcentral.Pro_Ahorros.Pla_Minimo
"Pla_Minimo" "Pla_Minimo" ? ? "integer" ? ? ? ? ? ? yes ? no 12.72 yes ?
     _FldNameList[60]   > bdcentral.Pro_Ahorros.Porce_Embargo
"Porce_Embargo" "Porce_Embargo" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.57 yes ?
     _FldNameList[61]   > bdcentral.Pro_Ahorros.Porce_Pignoracion
"Porce_Pignoracion" "Porce_Pignoracion" ? ? "decimal" ? ? ? ? ? ? yes ? no 14.86 yes ?
     _FldNameList[62]   > bdcentral.Pro_Ahorros.Prioridad
"Prioridad" "Prioridad" ? ? "integer" ? ? ? ? ? ? yes ? yes 8.43 yes ?
     _FldNameList[63]   > bdcentral.Pro_Ahorros.ProCre_Asociado
"ProCre_Asociado" "ProCre_Asociado" ? ? "integer" ? ? ? ? ? ? yes ? no 15.86 yes ?
     _FldNameList[64]   > bdcentral.Pro_Ahorros.Pro_Digito
"Pro_Digito" "Pro_Digito" ? ? "integer" ? ? ? ? ? ? yes ? no 18.86 yes ?
     _FldNameList[65]   > bdcentral.Pro_Ahorros.Tie_Inactividad
"Tie_Inactividad" "Tie_Inactividad" ? ? "integer" ? ? ? ? ? ? yes ? no 17.57 yes ?
     _FldNameList[66]   > bdcentral.Pro_Ahorros.Tip_Ahorro
"Tip_Ahorro" "Tip_Ahorro" ? ? "integer" ? ? ? ? ? ? yes ? yes 12.72 yes ?
     _FldNameList[67]   > bdcentral.Pro_Ahorros.Tip_Anualidad
"Tip_Anualidad" "Tip_Anualidad" ? ? "logical" ? ? ? ? ? ? yes ? no 13.72 yes ?
     _FldNameList[68]   > bdcentral.Pro_Ahorros.Tip_Interes
"Tip_Interes" "Tip_Interes" ? ? "integer" ? ? ? ? ? ? yes ? no 13.86 yes ?
     _FldNameList[69]   > bdcentral.Pro_Ahorros.Tip_Salminimo
"Tip_Salminimo" "Tip_Salminimo" ? ? "logical" ? ? ? ? ? ? yes ? no 17.29 yes ?
     _FldNameList[70]   > bdcentral.Pro_Ahorros.Tip_Vencimiento
"Tip_Vencimiento" "Tip_Vencimiento" ? ? "integer" ? ? ? ? ? ? yes ? no 15.86 yes ?
     _FldNameList[71]   > bdcentral.Pro_Ahorros.Titulo_Valor
"Titulo_Valor" "Titulo_Valor" ? ? "logical" ? ? ? ? ? ? yes ? no 10.43 yes ?
     _FldNameList[72]   > bdcentral.Pro_Ahorros.Val_CadaCheque
"Val_CadaCheque" "Val_CadaCheque" ? ? "decimal" ? ? ? ? ? ? yes ? no 16.57 yes ?
     _FldNameList[73]   > bdcentral.Pro_Ahorros.Val_Cuota
"Val_Cuota" "Val_Cuota" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.43 yes ?
     _FldNameList[74]   > bdcentral.Pro_Ahorros.Val_Embargo
"Val_Embargo" "Val_Embargo" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[75]   > bdcentral.Pro_Ahorros.Val_MaxConsignacion
"Val_MaxConsignacion" "Val_MaxConsignacion" ? ? "decimal" ? ? ? ? ? ? yes ? no 28.72 yes ?
     _FldNameList[76]   > bdcentral.Pro_Ahorros.Val_MaxRetEfectivo
"Val_MaxRetEfectivo" "Val_MaxRetEfectivo" ? ? "decimal" ? ? ? ? ? ? yes ? no 26.86 yes ?
     _FldNameList[77]   > bdcentral.Pro_Ahorros.Val_Minconsignacion
"Val_Minconsignacion" "Val_Minconsignacion" ? ? "decimal" ? ? ? ? ? ? yes ? no 28.14 yes ?
     _FldNameList[78]   > bdcentral.Pro_Ahorros.Val_MinRetcheque
"Val_MinRetcheque" "Val_MinRetcheque" ? ? "decimal" ? ? ? ? ? ? yes ? no 28.72 yes ?
     _FldNameList[79]   > bdcentral.Pro_Ahorros.Val_MinRetiro
"Val_MinRetiro" "Val_MinRetiro" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[80]   > bdcentral.Pro_Ahorros.Val_MonAper
"Val_MonAper" "Val_MonAper" ? ? "decimal" ? ? ? ? ? ? yes ? no 16.86 yes ?
     _FldNameList[81]   > bdcentral.Pro_Ahorros.Val_Pignoracion
"Val_Pignoracion" "Val_Pignoracion" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[82]   > bdcentral.Pro_Ahorros.Val_SdoMinimo
"Val_SdoMinimo" "Val_SdoMinimo" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[83]   > bdcentral.Pro_Ahorros.Val_Talonario
"Val_Talonario" "Val_Talonario" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.86 yes ?
     _FldNameList[84]   > bdcentral.Pro_Ahorros.VrTope_ExentoEE
"VrTope_ExentoEE" "VrTope_ExentoEE" ? ? "decimal" ? ? ? ? ? ? yes ? no 17.14 yes ?
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

