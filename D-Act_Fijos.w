&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
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
&Scoped-define INTERNAL-TABLES Act_Fijo

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  Agencia Codigo Nombre Grupo Descripcion Nit_Proveedor Fec_Avaluo Fec_Compra~
 Fec_Contrato Fec_debaja Fec_FinPignora Fec_Garantia Fec_IniDepre~
 Fec_IniPignora Fec_Retiro Fec_VctoMante Fec_Venta Id_Pignorado Id_Prestamo~
 Mejoras Neto Nit_Arrendatario Nit_Avaluo Nit_Mantenimiento Nit_Pignorado~
 Nit_Responsable Nit_Seguro Nro_Factura Nro_Seguro Ord_Compra Per_Depreciado~
 Pol_Mantenimiento Sdo_Depre Sdo_Provision TipNro_contrato Anos_Adepreciar~
 Cen_Costos Cod_UsoBien Cos_Historico CtaIngresoArr CtaOtrosIng Estado~
 Fec_Asignacion Val_Arriendo Val_Avaluo Val_Comercial Val_Compra~
 Val_Garantia Val_Provision Val_Reposicion Val_Salvamento Val_Valorizacion~
 Vto_Seguro
&Scoped-define ENABLED-FIELDS-IN-Act_Fijo Agencia Codigo Nombre Grupo ~
Descripcion Nit_Proveedor Fec_Avaluo Fec_Compra Fec_Contrato Fec_debaja ~
Fec_FinPignora Fec_Garantia Fec_IniDepre Fec_IniPignora Fec_Retiro ~
Fec_VctoMante Fec_Venta Id_Pignorado Id_Prestamo Mejoras Neto ~
Nit_Arrendatario Nit_Avaluo Nit_Mantenimiento Nit_Pignorado Nit_Responsable ~
Nit_Seguro Nro_Factura Nro_Seguro Ord_Compra Per_Depreciado ~
Pol_Mantenimiento Sdo_Depre Sdo_Provision TipNro_contrato Anos_Adepreciar ~
Cen_Costos Cod_UsoBien Cos_Historico CtaIngresoArr CtaOtrosIng Estado ~
Fec_Asignacion Val_Arriendo Val_Avaluo Val_Comercial Val_Compra ~
Val_Garantia Val_Provision Val_Reposicion Val_Salvamento Val_Valorizacion ~
Vto_Seguro 
&Scoped-Define DATA-FIELDS  Agencia Codigo Nombre Grupo Descripcion Nit_Proveedor Fec_Avaluo Fec_Compra~
 Fec_Contrato Fec_debaja Fec_FinPignora Fec_Garantia Fec_IniDepre~
 Fec_IniPignora Fec_Retiro Fec_VctoMante Fec_Venta Id_Pignorado Id_Prestamo~
 Mejoras Neto Nit_Arrendatario Nit_Avaluo Nit_Mantenimiento Nit_Pignorado~
 Nit_Responsable Nit_Seguro Nro_Factura Nro_Seguro Ord_Compra Per_Depreciado~
 Pol_Mantenimiento Sdo_Depre Sdo_Provision TipNro_contrato Anos_Adepreciar~
 Cen_Costos Cod_UsoBien Cos_Historico CtaIngresoArr CtaOtrosIng Estado~
 Fec_Asignacion Val_Arriendo Val_Avaluo Val_Comercial Val_Compra~
 Val_Garantia Val_Provision Val_Reposicion Val_Salvamento Val_Valorizacion~
 Vto_Seguro
&Scoped-define DATA-FIELDS-IN-Act_Fijo Agencia Codigo Nombre Grupo ~
Descripcion Nit_Proveedor Fec_Avaluo Fec_Compra Fec_Contrato Fec_debaja ~
Fec_FinPignora Fec_Garantia Fec_IniDepre Fec_IniPignora Fec_Retiro ~
Fec_VctoMante Fec_Venta Id_Pignorado Id_Prestamo Mejoras Neto ~
Nit_Arrendatario Nit_Avaluo Nit_Mantenimiento Nit_Pignorado Nit_Responsable ~
Nit_Seguro Nro_Factura Nro_Seguro Ord_Compra Per_Depreciado ~
Pol_Mantenimiento Sdo_Depre Sdo_Provision TipNro_contrato Anos_Adepreciar ~
Cen_Costos Cod_UsoBien Cos_Historico CtaIngresoArr CtaOtrosIng Estado ~
Fec_Asignacion Val_Arriendo Val_Avaluo Val_Comercial Val_Compra ~
Val_Garantia Val_Provision Val_Reposicion Val_Salvamento Val_Valorizacion ~
Vto_Seguro 
&Scoped-Define MANDATORY-FIELDS  Codigo Nombre Cen_Costos
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "D-Act_Fijos.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH Act_Fijo NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH Act_Fijo NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main Act_Fijo
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main Act_Fijo


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      Act_Fijo SCROLLING.
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
         WIDTH              = 16.43.
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
     _TblList          = "bdcentral.Act_Fijo"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > bdcentral.Act_Fijo.Agencia
"Agencia" "Agencia" ? ? "integer" ? ? ? ? ? ? yes ? no 6.43 yes
     _FldNameList[2]   > bdcentral.Act_Fijo.Codigo
"Codigo" "Codigo" ? ? "character" ? ? ? ? ? ? yes ? yes 6.43 yes
     _FldNameList[3]   > bdcentral.Act_Fijo.Nombre
"Nombre" "Nombre" ? ? "character" ? ? ? ? ? ? yes ? yes 40 yes
     _FldNameList[4]   > bdcentral.Act_Fijo.Grupo
"Grupo" "Grupo" ? ? "integer" ? ? ? ? ? ? yes ? no 5.72 yes
     _FldNameList[5]   > bdcentral.Act_Fijo.Descripcion
"Descripcion" "Descripcion" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[6]   > bdcentral.Act_Fijo.Nit_Proveedor
"Nit_Proveedor" "Nit_Proveedor" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[7]   > bdcentral.Act_Fijo.Fec_Avaluo
"Fec_Avaluo" "Fec_Avaluo" ? ? "date" ? ? ? ? ? ? yes ? no 10.29 yes
     _FldNameList[8]   > bdcentral.Act_Fijo.Fec_Compra
"Fec_Compra" "Fec_Compra" ? ? "date" ? ? ? ? ? ? yes ? no 13.86 yes
     _FldNameList[9]   > bdcentral.Act_Fijo.Fec_Contrato
"Fec_Contrato" "Fec_Contrato" ? ? "date" ? ? ? ? ? ? yes ? no 14.43 yes
     _FldNameList[10]   > bdcentral.Act_Fijo.Fec_debaja
"Fec_debaja" "Fec_debaja" ? ? "date" ? ? ? ? ? ? yes ? no 13.29 yes
     _FldNameList[11]   > bdcentral.Act_Fijo.Fec_FinPignora
"Fec_FinPignora" "Fec_FinPignora" ? ? "date" ? ? ? ? ? ? yes ? no 14.43 yes
     _FldNameList[12]   > bdcentral.Act_Fijo.Fec_Garantia
"Fec_Garantia" "Fec_Garantia" ? ? "date" ? ? ? ? ? ? yes ? no 14 yes
     _FldNameList[13]   > bdcentral.Act_Fijo.Fec_IniDepre
"Fec_IniDepre" "Fec_IniDepre" ? ? "date" ? ? ? ? ? ? yes ? no 16 yes
     _FldNameList[14]   > bdcentral.Act_Fijo.Fec_IniPignora
"Fec_IniPignora" "Fec_IniPignora" ? ? "date" ? ? ? ? ? ? yes ? no 16.57 yes
     _FldNameList[15]   > bdcentral.Act_Fijo.Fec_Retiro
"Fec_Retiro" "Fec_Retiro" ? ? "date" ? ? ? ? ? ? yes ? no 14.57 yes
     _FldNameList[16]   > bdcentral.Act_Fijo.Fec_VctoMante
"Fec_VctoMante" "Fec_VctoMante" ? ? "date" ? ? ? ? ? ? yes ? no 11 yes
     _FldNameList[17]   > bdcentral.Act_Fijo.Fec_Venta
"Fec_Venta" "Fec_Venta" ? ? "date" ? ? ? ? ? ? yes ? no 14.14 yes
     _FldNameList[18]   > bdcentral.Act_Fijo.Id_Pignorado
"Id_Pignorado" "Id_Pignorado" ? ? "logical" ? ? ? ? ? ? yes ? no 9.43 yes
     _FldNameList[19]   > bdcentral.Act_Fijo.Id_Prestamo
"Id_Prestamo" "Id_Prestamo" ? ? "logical" ? ? ? ? ? ? yes ? no 8.29 yes
     _FldNameList[20]   > bdcentral.Act_Fijo.Mejoras
"Mejoras" "Mejoras" ? ? "decimal" ? ? ? ? ? ? yes ? no 18.86 yes
     _FldNameList[21]   > bdcentral.Act_Fijo.Neto
"Neto" "Neto" ? ? "decimal" ? ? ? ? ? ? yes ? no 16 yes
     _FldNameList[22]   > bdcentral.Act_Fijo.Nit_Arrendatario
"Nit_Arrendatario" "Nit_Arrendatario" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[23]   > bdcentral.Act_Fijo.Nit_Avaluo
"Nit_Avaluo" "Nit_Avaluo" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[24]   > bdcentral.Act_Fijo.Nit_Mantenimiento
"Nit_Mantenimiento" "Nit_Mantenimiento" ? ? "character" ? ? ? ? ? ? yes ? no 13.72 yes
     _FldNameList[25]   > bdcentral.Act_Fijo.Nit_Pignorado
"Nit_Pignorado" "Nit_Pignorado" ? ? "character" ? ? ? ? ? ? yes ? no 14.14 yes
     _FldNameList[26]   > bdcentral.Act_Fijo.Nit_Responsable
"Nit_Responsable" "Nit_Responsable" ? ? "character" ? ? ? ? ? ? yes ? no 12.29 yes
     _FldNameList[27]   > bdcentral.Act_Fijo.Nit_Seguro
"Nit_Seguro" "Nit_Seguro" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[28]   > bdcentral.Act_Fijo.Nro_Factura
"Nro_Factura" "Nro_Factura" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes
     _FldNameList[29]   > bdcentral.Act_Fijo.Nro_Seguro
"Nro_Seguro" "Nro_Seguro" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes
     _FldNameList[30]   > bdcentral.Act_Fijo.Ord_Compra
"Ord_Compra" "Ord_Compra" ? ? "character" ? ? ? ? ? ? yes ? no 16.14 yes
     _FldNameList[31]   > bdcentral.Act_Fijo.Per_Depreciado
"Per_Depreciado" "Per_Depreciado" ? ? "integer" ? ? ? ? ? ? yes ? no 20.29 yes
     _FldNameList[32]   > bdcentral.Act_Fijo.Pol_Mantenimiento
"Pol_Mantenimiento" "Pol_Mantenimiento" ? ? "character" ? ? ? ? ? ? yes ? no 11 yes
     _FldNameList[33]   > bdcentral.Act_Fijo.Sdo_Depre
"Sdo_Depre" "Sdo_Depre" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.43 yes
     _FldNameList[34]   > bdcentral.Act_Fijo.Sdo_Provision
"Sdo_Provision" "Sdo_Provision" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.43 yes
     _FldNameList[35]   > bdcentral.Act_Fijo.TipNro_contrato
"TipNro_contrato" "TipNro_contrato" ? ? "character" ? ? ? ? ? ? yes ? no 50 yes
     _FldNameList[36]   > bdcentral.Act_Fijo.Anos_Adepreciar
"Anos_Adepreciar" "Anos_Adepreciar" ? ? "integer" ? ? ? ? ? ? yes ? no 7.72 yes
     _FldNameList[37]   > bdcentral.Act_Fijo.Cen_Costos
"Cen_Costos" "Cen_Costos" ? ? "integer" ? ? ? ? ? ? yes ? yes 15.86 yes
     _FldNameList[38]   > bdcentral.Act_Fijo.Cod_UsoBien
"Cod_UsoBien" "Cod_UsoBien" ? ? "integer" ? ? ? ? ? ? yes ? no 8.57 yes
     _FldNameList[39]   > bdcentral.Act_Fijo.Cos_Historico
"Cos_Historico" "Cos_Historico" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.43 yes
     _FldNameList[40]   > bdcentral.Act_Fijo.CtaIngresoArr
"CtaIngresoArr" "CtaIngresoArr" ? ? "character" ? ? ? ? ? ? yes ? no 14 yes
     _FldNameList[41]   > bdcentral.Act_Fijo.CtaOtrosIng
"CtaOtrosIng" "CtaOtrosIng" ? ? "character" ? ? ? ? ? ? yes ? no 14 yes
     _FldNameList[42]   > bdcentral.Act_Fijo.Estado
"Estado" "Estado" ? ? "integer" ? ? ? ? ? ? yes ? no 6.43 yes
     _FldNameList[43]   > bdcentral.Act_Fijo.Fec_Asignacion
"Fec_Asignacion" "Fec_Asignacion" ? ? "date" ? ? ? ? ? ? yes ? no 14.57 yes
     _FldNameList[44]   > bdcentral.Act_Fijo.Val_Arriendo
"Val_Arriendo" "Val_Arriendo" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.43 yes
     _FldNameList[45]   > bdcentral.Act_Fijo.Val_Avaluo
"Val_Avaluo" "Val_Avaluo" ? ? "decimal" ? ? ? ? ? ? yes ? no 18.29 yes
     _FldNameList[46]   > bdcentral.Act_Fijo.Val_Comercial
"Val_Comercial" "Val_Comercial" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.43 yes
     _FldNameList[47]   > bdcentral.Act_Fijo.Val_Compra
"Val_Compra" "Val_Compra" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.43 yes
     _FldNameList[48]   > bdcentral.Act_Fijo.Val_Garantia
"Val_Garantia" "Val_Garantia" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.43 yes
     _FldNameList[49]   > bdcentral.Act_Fijo.Val_Provision
"Val_Provision" "Val_Provision" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.43 yes
     _FldNameList[50]   > bdcentral.Act_Fijo.Val_Reposicion
"Val_Reposicion" "Val_Reposicion" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.43 yes
     _FldNameList[51]   > bdcentral.Act_Fijo.Val_Salvamento
"Val_Salvamento" "Val_Salvamento" ? ? "decimal" ? ? ? ? ? ? yes ? no 16.29 yes
     _FldNameList[52]   > bdcentral.Act_Fijo.Val_Valorizacion
"Val_Valorizacion" "Val_Valorizacion" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.43 yes
     _FldNameList[53]   > bdcentral.Act_Fijo.Vto_Seguro
"Vto_Seguro" "Vto_Seguro" ? ? "date" ? ? ? ? ? ? yes ? no 18.43 yes
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

