&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}

/* Db-Required definitions. */
&IF DEFINED(DB-REQUIRED) = 0 &THEN
    &GLOBAL-DEFINE DB-REQUIRED TRUE
&ENDIF
&GLOBAL-DEFINE DB-REQUIRED-START   &IF {&DB-REQUIRED} &THEN
&GLOBAL-DEFINE DB-REQUIRED-END     &ENDIF



/* Temp-Table and Buffer definitions                                    */
{&DB-REQUIRED-START}
 DEFINE BUFFER CliAseguradora FOR Clientes.
{&DB-REQUIRED-END}
{&DB-REQUIRED-START}
 DEFINE BUFFER CliProveedor FOR Clientes.
{&DB-REQUIRED-END}
{&DB-REQUIRED-START}
 DEFINE BUFFER CliResponsable FOR Clientes.
{&DB-REQUIRED-END}



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

&Global-define DATA-LOGIC-PROCEDURE .p

&Scoped-define PROCEDURE-TYPE SmartDataObject
&Scoped-define DB-AWARE yes

&Scoped-define ADM-SUPPORTED-LINKS Data-Source,Data-Target,Navigation-Target,Update-Target,Commit-Target,Filter-Target


/* Note that Db-Required is defined before the buffer definitions for this object. */

&Scoped-define QUERY-NAME Query-Main

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Act_Fijo CliProveedor CliAseguradora ~
CliResponsable

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  Agencia Anos_Adepreciar Cen_Costos Codigo Cos_Historico Descripcion Estado~
 Fec_Avaluo Fec_Compra Fec_Contrato Fec_debaja Fec_Garantia Fec_IniDepre~
 Fec_Retiro Fec_Venta Grupo Mejoras Neto Nit_Proveedor Nit_Responsable~
 Fec_Asignacion Nit_Seguro Nombre Nro_Factura Nro_Seguro Ord_Compra~
 Per_Depreciado Sdo_Depre ValDepAcum ValDepMes Val_Compra Val_Garantia~
 Vto_Seguro Val_Avaluo Val_Comercial Val_Provision Val_Valorizacion~
 Sdo_Provision Id_Prestamo Apellido1Prov Apellido2Prov NitProv Nombre-2Prov~
 Apellido-2Ase Apellido-3Ase Nit-2Ase Nombre-3Ase Apellido-4Res~
 Apellido-5Res Nit-3Res Nombre-4Res
&Scoped-define ENABLED-FIELDS-IN-Act_Fijo Agencia Anos_Adepreciar ~
Cen_Costos Codigo Cos_Historico Descripcion Estado Fec_Avaluo Fec_Compra ~
Fec_Contrato Fec_debaja Fec_Garantia Fec_IniDepre Fec_Retiro Fec_Venta ~
Grupo Mejoras Neto Nit_Proveedor Nit_Responsable Fec_Asignacion Nit_Seguro ~
Nombre Nro_Factura Nro_Seguro Ord_Compra Per_Depreciado Sdo_Depre ~
ValDepAcum ValDepMes Val_Compra Val_Garantia Vto_Seguro Val_Avaluo ~
Val_Comercial Val_Provision Val_Valorizacion Sdo_Provision Id_Prestamo 
&Scoped-define ENABLED-FIELDS-IN-CliProveedor Apellido1Prov Apellido2Prov ~
NitProv Nombre-2Prov 
&Scoped-define ENABLED-FIELDS-IN-CliAseguradora Apellido-2Ase Apellido-3Ase ~
Nit-2Ase Nombre-3Ase 
&Scoped-define ENABLED-FIELDS-IN-CliResponsable Apellido-4Res Apellido-5Res ~
Nit-3Res Nombre-4Res 
&Scoped-Define DATA-FIELDS  Agencia Anos_Adepreciar Cen_Costos Codigo Cos_Historico Descripcion Estado~
 Fec_Avaluo Fec_Compra Fec_Contrato Fec_debaja Fec_Garantia Fec_IniDepre~
 Fec_Retiro Fec_Venta Grupo Mejoras Neto Nit_Proveedor Nit_Responsable~
 Fec_Asignacion Nit_Seguro Nombre Nro_Factura Nro_Seguro Ord_Compra~
 Per_Depreciado Sdo_Depre ValDepAcum ValDepMes Val_Compra Val_Garantia~
 Vto_Seguro Val_Avaluo Val_Comercial Val_Provision Val_Valorizacion~
 Sdo_Provision Id_Prestamo Apellido1Prov Apellido2Prov NitProv Nombre-2Prov~
 Apellido-2Ase Apellido-3Ase Nit-2Ase Nombre-3Ase Apellido-4Res~
 Apellido-5Res Nit-3Res Nombre-4Res
&Scoped-define DATA-FIELDS-IN-Act_Fijo Agencia Anos_Adepreciar Cen_Costos ~
Codigo Cos_Historico Descripcion Estado Fec_Avaluo Fec_Compra Fec_Contrato ~
Fec_debaja Fec_Garantia Fec_IniDepre Fec_Retiro Fec_Venta Grupo Mejoras ~
Neto Nit_Proveedor Nit_Responsable Fec_Asignacion Nit_Seguro Nombre ~
Nro_Factura Nro_Seguro Ord_Compra Per_Depreciado Sdo_Depre ValDepAcum ~
ValDepMes Val_Compra Val_Garantia Vto_Seguro Val_Avaluo Val_Comercial ~
Val_Provision Val_Valorizacion Sdo_Provision Id_Prestamo 
&Scoped-define DATA-FIELDS-IN-CliProveedor Apellido1Prov Apellido2Prov ~
NitProv Nombre-2Prov 
&Scoped-define DATA-FIELDS-IN-CliAseguradora Apellido-2Ase Apellido-3Ase ~
Nit-2Ase Nombre-3Ase 
&Scoped-define DATA-FIELDS-IN-CliResponsable Apellido-4Res Apellido-5Res ~
Nit-3Res Nombre-4Res 
&Scoped-Define MANDATORY-FIELDS  Cen_Costos Codigo Nombre
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST   rowObject.Apellido1Prov = CliProveedor.Apellido1~
  rowObject.Apellido2Prov = CliProveedor.Apellido2~
  rowObject.NitProv = CliProveedor.Nit~
  rowObject.Nombre-2Prov = CliProveedor.Nombre~
  rowObject.Apellido-2Ase = CliAseguradora.Apellido1~
  rowObject.Apellido-3Ase = CliAseguradora.Apellido2~
  rowObject.Nit-2Ase = CliAseguradora.Nit~
  rowObject.Nombre-3Ase = CliAseguradora.Nombre~
  rowObject.Apellido-4Res = CliResponsable.Apellido1~
  rowObject.Apellido-5Res = CliResponsable.Apellido2~
  rowObject.Nit-3Res = CliResponsable.Nit~
  rowObject.Nombre-4Res = CliResponsable.Nombre
&Scoped-Define DATA-FIELD-DEFS "dAct_Fijo.i"
&Scoped-Define DATA-TABLE-NO-UNDO NO-UNDO
&Scoped-define QUERY-STRING-Query-Main FOR EACH Act_Fijo NO-LOCK, ~
      EACH CliProveedor WHERE CliProveedor.Nit = Act_Fijo.Nit_Proveedor NO-LOCK, ~
      EACH CliAseguradora WHERE CliAseguradora.Nit = Act_Fijo.Nit_Seguro NO-LOCK, ~
      EACH CliResponsable WHERE CliResponsable.Nit = Act_Fijo.Nit_Responsable NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH Act_Fijo NO-LOCK, ~
      EACH CliProveedor WHERE CliProveedor.Nit = Act_Fijo.Nit_Proveedor NO-LOCK, ~
      EACH CliAseguradora WHERE CliAseguradora.Nit = Act_Fijo.Nit_Seguro NO-LOCK, ~
      EACH CliResponsable WHERE CliResponsable.Nit = Act_Fijo.Nit_Responsable NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main Act_Fijo CliProveedor ~
CliAseguradora CliResponsable
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main Act_Fijo
&Scoped-define SECOND-TABLE-IN-QUERY-Query-Main CliProveedor
&Scoped-define THIRD-TABLE-IN-QUERY-Query-Main CliAseguradora
&Scoped-define FOURTH-TABLE-IN-QUERY-Query-Main CliResponsable


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      Act_Fijo, 
      CliProveedor, 
      CliAseguradora, 
      CliResponsable SCROLLING.
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
      TABLE: CliAseguradora B "?" ? bdcentral Clientes
      TABLE: CliProveedor B "?" ? bdcentral Clientes
      TABLE: CliResponsable B "?" ? bdcentral Clientes
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
         HEIGHT             = 1.58
         WIDTH              = 47.29.
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
     _TblList          = "bdcentral.Act_Fijo,CliProveedor WHERE bdcentral.Act_Fijo ...,CliAseguradora WHERE bdcentral.Act_Fijo ...,CliResponsable WHERE bdcentral.Act_Fijo ..."
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _JoinCode[2]      = "CliProveedor.Nit = Act_Fijo.Nit_Proveedor"
     _JoinCode[3]      = "CliAseguradora.Nit = Act_Fijo.Nit_Seguro"
     _JoinCode[4]      = "CliResponsable.Nit = Act_Fijo.Nit_Responsable"
     _FldNameList[1]   > bdcentral.Act_Fijo.Agencia
"Agencia" "Agencia" ? ? "integer" ? ? ? ? ? ? yes ? no 6.43 yes ?
     _FldNameList[2]   > bdcentral.Act_Fijo.Anos_Adepreciar
"Anos_Adepreciar" "Anos_Adepreciar" ? ? "integer" ? ? ? ? ? ? yes ? no 7.72 yes ?
     _FldNameList[3]   > bdcentral.Act_Fijo.Cen_Costos
"Cen_Costos" "Cen_Costos" ? ? "integer" ? ? ? ? ? ? yes ? yes 15.86 yes ?
     _FldNameList[4]   > bdcentral.Act_Fijo.Codigo
"Codigo" "Codigo" ? "X(15)" "character" ? ? ? ? ? ? yes ? yes 15 yes ?
     _FldNameList[5]   > bdcentral.Act_Fijo.Cos_Historico
"Cos_Historico" "Cos_Historico" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.43 yes ?
     _FldNameList[6]   > bdcentral.Act_Fijo.Descripcion
"Descripcion" "Descripcion" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes ?
     _FldNameList[7]   > bdcentral.Act_Fijo.Estado
"Estado" "Estado" ? ? "integer" ? ? ? ? ? ? yes ? no 6.43 yes ?
     _FldNameList[8]   > bdcentral.Act_Fijo.Fec_Avaluo
"Fec_Avaluo" "Fec_Avaluo" ? ? "date" ? ? ? ? ? ? yes ? no 15.29 yes ?
     _FldNameList[9]   > bdcentral.Act_Fijo.Fec_Compra
"Fec_Compra" "Fec_Compra" ? ? "date" ? ? ? ? ? ? yes ? no 16.14 yes ?
     _FldNameList[10]   > bdcentral.Act_Fijo.Fec_Contrato
"Fec_Contrato" "Fec_Contrato" ? ? "date" ? ? ? ? ? ? yes ? no 16.72 yes ?
     _FldNameList[11]   > bdcentral.Act_Fijo.Fec_debaja
"Fec_debaja" "Fec_debaja" ? ? "date" ? ? ? ? ? ? yes ? no 13.29 yes ?
     _FldNameList[12]   > bdcentral.Act_Fijo.Fec_Garantia
"Fec_Garantia" "Fec_Garantia" ? ? "date" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[13]   > bdcentral.Act_Fijo.Fec_IniDepre
"Fec_IniDepre" "Fec_IniDepre" ? ? "date" ? ? ? ? ? ? yes ? no 12.14 yes ?
     _FldNameList[14]   > bdcentral.Act_Fijo.Fec_Retiro
"Fec_Retiro" "Fec_Retiro" ? ? "date" ? ? ? ? ? ? yes ? no 14.57 yes ?
     _FldNameList[15]   > bdcentral.Act_Fijo.Fec_Venta
"Fec_Venta" "Fec_Venta" ? ? "date" ? ? ? ? ? ? yes ? no 14.14 yes ?
     _FldNameList[16]   > bdcentral.Act_Fijo.Grupo
"Grupo" "Grupo" ? ? "integer" ? ? ? ? ? ? yes ? no 5.72 yes ?
     _FldNameList[17]   > bdcentral.Act_Fijo.Mejoras
"Mejoras" "Mejoras" ? ? "decimal" ? ? ? ? ? ? yes ? no 18.86 yes ?
     _FldNameList[18]   > bdcentral.Act_Fijo.Neto
"Neto" "Neto" ? ? "decimal" ? ? ? ? ? ? yes ? no 16 yes ?
     _FldNameList[19]   > bdcentral.Act_Fijo.Nit_Proveedor
"Nit_Proveedor" "Nit_Proveedor" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[20]   > bdcentral.Act_Fijo.Nit_Responsable
"Nit_Responsable" "Nit_Responsable" ? ? "character" ? ? ? ? ? ? yes ? no 12.29 yes ?
     _FldNameList[21]   > bdcentral.Act_Fijo.Fec_Asignacion
"Fec_Asignacion" "Fec_Asignacion" ? ? "date" ? ? ? ? ? ? yes ? no 16.43 yes ?
     _FldNameList[22]   > bdcentral.Act_Fijo.Nit_Seguro
"Nit_Seguro" "Nit_Seguro" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[23]   > bdcentral.Act_Fijo.Nombre
"Nombre" "Nombre" ? ? "character" ? ? ? ? ? ? yes ? yes 40 yes ?
     _FldNameList[24]   > bdcentral.Act_Fijo.Nro_Factura
"Nro_Factura" "Nro_Factura" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ?
     _FldNameList[25]   > bdcentral.Act_Fijo.Nro_Seguro
"Nro_Seguro" "Nro_Seguro" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ?
     _FldNameList[26]   > bdcentral.Act_Fijo.Ord_Compra
"Ord_Compra" "Ord_Compra" ? ? "character" ? ? ? ? ? ? yes ? no 14.43 yes ?
     _FldNameList[27]   > bdcentral.Act_Fijo.Per_Depreciado
"Per_Depreciado" "Per_Depreciado" ? ? "integer" ? ? ? ? ? ? yes ? no 20.29 yes ?
     _FldNameList[28]   > bdcentral.Act_Fijo.Sdo_Depre
"Sdo_Depre" "Sdo_Depre" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.43 yes ?
     _FldNameList[29]   > bdcentral.Act_Fijo.ValDepAcum
"ValDepAcum" "ValDepAcum" ? ? "decimal" ? ? ? ? ? ? yes ? no 25.29 yes ?
     _FldNameList[30]   > bdcentral.Act_Fijo.ValDepMes
"ValDepMes" "ValDepMes" ? ? "decimal" ? ? ? ? ? ? yes ? no 23.14 yes ?
     _FldNameList[31]   > bdcentral.Act_Fijo.Val_Compra
"Val_Compra" "Val_Compra" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.43 yes ?
     _FldNameList[32]   > bdcentral.Act_Fijo.Val_Garantia
"Val_Garantia" "Val_Garantia" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.43 yes ?
     _FldNameList[33]   > bdcentral.Act_Fijo.Vto_Seguro
"Vto_Seguro" "Vto_Seguro" ? ? "date" ? ? ? ? ? ? yes ? no 18.43 yes ?
     _FldNameList[34]   > bdcentral.Act_Fijo.Val_Avaluo
"Val_Avaluo" "Val_Avaluo" ? ? "decimal" ? ? ? ? ? ? yes ? no 18.29 yes ?
     _FldNameList[35]   > bdcentral.Act_Fijo.Val_Comercial
"Val_Comercial" "Val_Comercial" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.43 yes ?
     _FldNameList[36]   > bdcentral.Act_Fijo.Val_Provision
"Val_Provision" "Val_Provision" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.43 yes ?
     _FldNameList[37]   > bdcentral.Act_Fijo.Val_Valorizacion
"Val_Valorizacion" "Val_Valorizacion" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.43 yes ?
     _FldNameList[38]   > bdcentral.Act_Fijo.Sdo_Provision
"Sdo_Provision" "Sdo_Provision" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.43 yes ?
     _FldNameList[39]   > bdcentral.Act_Fijo.Id_Prestamo
"Id_Prestamo" "Id_Prestamo" ? ? "logical" ? ? ? ? ? ? yes ? no 8.29 yes ?
     _FldNameList[40]   > Temp-Tables.CliProveedor.Apellido1
"Apellido1" "Apellido1Prov" "Primer Apellido Prov" ? "character" ? ? ? ? ? ? yes ? no 15 no "Primer Apellido Prov"
     _FldNameList[41]   > Temp-Tables.CliProveedor.Apellido2
"Apellido2" "Apellido2Prov" "Segundo Apellido Prov" ? "character" ? ? ? ? ? ? yes ? no 15.43 no "Segundo Apellido Prov"
     _FldNameList[42]   > Temp-Tables.CliProveedor.Nit
"Nit" "NitProv" "Nit Prov" ? "character" ? ? ? ? ? ? yes ? no 12 no "Nit Prov"
     _FldNameList[43]   > Temp-Tables.CliProveedor.Nombre
"Nombre" "Nombre-2Prov" "Nombre Prov" ? "character" ? ? ? ? ? ? yes ? no 40 no "Nombre Prov"
     _FldNameList[44]   > Temp-Tables.CliAseguradora.Apellido1
"Apellido1" "Apellido-2Ase" "Primer Apellido Ase" ? "character" ? ? ? ? ? ? yes ? no 15 no "Primer Apellido Ase"
     _FldNameList[45]   > Temp-Tables.CliAseguradora.Apellido2
"Apellido2" "Apellido-3Ase" "Segundo Apellido Ase" ? "character" ? ? ? ? ? ? yes ? no 15.43 no "Segundo Apellido Ase"
     _FldNameList[46]   > Temp-Tables.CliAseguradora.Nit
"Nit" "Nit-2Ase" "Nit Ase" ? "character" ? ? ? ? ? ? yes ? no 12 no "Nit Ase"
     _FldNameList[47]   > Temp-Tables.CliAseguradora.Nombre
"Nombre" "Nombre-3Ase" "Nombre Ase" ? "character" ? ? ? ? ? ? yes ? no 40 no "Nombre Ase"
     _FldNameList[48]   > Temp-Tables.CliResponsable.Apellido1
"Apellido1" "Apellido-4Res" "Primer Apellido Res" ? "character" ? ? ? ? ? ? yes ? no 15 no "Primer Apellido Res"
     _FldNameList[49]   > Temp-Tables.CliResponsable.Apellido2
"Apellido2" "Apellido-5Res" "Segundo Apellido Res" ? "character" ? ? ? ? ? ? yes ? no 15.43 no "Segundo Apellido Res"
     _FldNameList[50]   > Temp-Tables.CliResponsable.Nit
"Nit" "Nit-3Res" "Nit Res" ? "character" ? ? ? ? ? ? yes ? no 12 no "Nit Res"
     _FldNameList[51]   > Temp-Tables.CliResponsable.Nombre
"Nombre" "Nombre-4Res" "Nombre Res" ? "character" ? ? ? ? ? ? yes ? no 40 no "Nombre Res"
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

