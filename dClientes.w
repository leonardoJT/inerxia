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
&Scoped-define INTERNAL-TABLES Clientes

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  Act_casa Act_inversion Act_vehiculo Agencia Anterior_Nit Apellido1~
 Apellido2 Aut_CentralRiesgo Calificacion Capacidad_Pago Carnet Celular~
 Codigo_CIIU Cod_Anterior Cod_Cargo Cod_Empresa Cod_Ingreso Cod_Profesion~
 Cod_Retiro Cod_Segmento Cod_Zona Conocimiento_Cliente Con_Sospechosas~
 Destino Dias_Sancion Dir_comercial Dir_Correspondencia Dir_Residencia Edad~
 Email Endeud_Indirecto Estado Estrato Est_Civil FecFin_NoSipla~
 FecIni_NoSipla Fec_ActFirma Fec_ActFoto Fec_Asociacion Fec_Calificacion~
 Fec_expedicion Fec_fallecido Fec_IngEmpresa Fec_Ingreso Fec_IniSancion~
 Fec_ModNit Fec_Nacimiento Fec_Retiro Fec_UltActualiza Firma fotografia~
 Garantia Gran_Contribuyente Grupo GtoFinanc_Indir Gto_Arriendo Gto_Familiar~
 Gto_obligacion Id_ExoneradoSipla Id_Preexistentes Id_Privilegiado~
 Id_PuedeCodeudar Id_Retencion Ing_arriendos Ing_financieros Ing_Honorarios~
 Ing_Otros Lugar_comercial Lugar_expedicion Lugar_Nacimiento~
 Lugar_Residencia Med_Publicitario Mora_Comercial Nit Niv_Educativo Nombre~
 Nom_Arrendatario Num_Hijos Per_Acargo Puntaje Reestructurado~
 Reportado_fiscalia Reportado_Procredito Reportado_Super Respaldo_Patrim~
 Salario Sancionado Sdo_Obligaciones Sexo Subgrupo Tel_Arrendatario~
 Tel_comercial Tel_Residencia Tiempo_Asociacion Tiempo_Empresa~
 Tipo_Actividad Tipo_Cliente Tipo_Identificacion Tipo_Vinculo Tipo_Vivienda~
 Tip_Contrato Ult_UsuImagen Usuario
&Scoped-define ENABLED-FIELDS-IN-Clientes Act_casa Act_inversion ~
Act_vehiculo Agencia Anterior_Nit Apellido1 Apellido2 Aut_CentralRiesgo ~
Calificacion Capacidad_Pago Carnet Celular Codigo_CIIU Cod_Anterior ~
Cod_Cargo Cod_Empresa Cod_Ingreso Cod_Profesion Cod_Retiro Cod_Segmento ~
Cod_Zona Conocimiento_Cliente Con_Sospechosas Destino Dias_Sancion ~
Dir_comercial Dir_Correspondencia Dir_Residencia Edad Email ~
Endeud_Indirecto Estado Estrato Est_Civil FecFin_NoSipla FecIni_NoSipla ~
Fec_ActFirma Fec_ActFoto Fec_Asociacion Fec_Calificacion Fec_expedicion ~
Fec_fallecido Fec_IngEmpresa Fec_Ingreso Fec_IniSancion Fec_ModNit ~
Fec_Nacimiento Fec_Retiro Fec_UltActualiza Firma fotografia Garantia ~
Gran_Contribuyente Grupo GtoFinanc_Indir Gto_Arriendo Gto_Familiar ~
Gto_obligacion Id_ExoneradoSipla Id_Preexistentes Id_Privilegiado ~
Id_PuedeCodeudar Id_Retencion Ing_arriendos Ing_financieros Ing_Honorarios ~
Ing_Otros Lugar_comercial Lugar_expedicion Lugar_Nacimiento ~
Lugar_Residencia Med_Publicitario Mora_Comercial Nit Niv_Educativo Nombre ~
Nom_Arrendatario Num_Hijos Per_Acargo Puntaje Reestructurado ~
Reportado_fiscalia Reportado_Procredito Reportado_Super Respaldo_Patrim ~
Salario Sancionado Sdo_Obligaciones Sexo Subgrupo Tel_Arrendatario ~
Tel_comercial Tel_Residencia Tiempo_Asociacion Tiempo_Empresa ~
Tipo_Actividad Tipo_Cliente Tipo_Identificacion Tipo_Vinculo Tipo_Vivienda ~
Tip_Contrato Ult_UsuImagen Usuario 
&Scoped-Define DATA-FIELDS  Act_casa Act_inversion Act_vehiculo Agencia Anterior_Nit Apellido1~
 Apellido2 Aut_CentralRiesgo Calificacion Capacidad_Pago Carnet Celular~
 Codigo_CIIU Cod_Anterior Cod_Cargo Cod_Empresa Cod_Ingreso Cod_Profesion~
 Cod_Retiro Cod_Segmento Cod_Zona Conocimiento_Cliente Con_Sospechosas~
 Destino Dias_Sancion Dir_comercial Dir_Correspondencia Dir_Residencia Edad~
 Email Endeud_Indirecto Estado Estrato Est_Civil FecFin_NoSipla~
 FecIni_NoSipla Fec_ActFirma Fec_ActFoto Fec_Asociacion Fec_Calificacion~
 Fec_expedicion Fec_fallecido Fec_IngEmpresa Fec_Ingreso Fec_IniSancion~
 Fec_ModNit Fec_Nacimiento Fec_Retiro Fec_UltActualiza Firma fotografia~
 Garantia Gran_Contribuyente Grupo GtoFinanc_Indir Gto_Arriendo Gto_Familiar~
 Gto_obligacion Id_ExoneradoSipla Id_Preexistentes Id_Privilegiado~
 Id_PuedeCodeudar Id_Retencion Ing_arriendos Ing_financieros Ing_Honorarios~
 Ing_Otros Lugar_comercial Lugar_expedicion Lugar_Nacimiento~
 Lugar_Residencia Med_Publicitario Mora_Comercial Nit Niv_Educativo Nombre~
 Nom_Arrendatario Num_Hijos Per_Acargo Puntaje Reestructurado~
 Reportado_fiscalia Reportado_Procredito Reportado_Super Respaldo_Patrim~
 Salario Sancionado Sdo_Obligaciones Sexo Subgrupo Tel_Arrendatario~
 Tel_comercial Tel_Residencia Tiempo_Asociacion Tiempo_Empresa~
 Tipo_Actividad Tipo_Cliente Tipo_Identificacion Tipo_Vinculo Tipo_Vivienda~
 Tip_Contrato Ult_UsuImagen Usuario FAgencia FCliente FTpVinculo FEmpresa~
 FProfesion FCargo FTpContrato FLugResidencia FLugComercial FLugNacimiento~
 FEnvCorresp FTotIngresos FTotEgresos FTotActivos FReestructurado~
 FPrivilegiado
&Scoped-define DATA-FIELDS-IN-Clientes Act_casa Act_inversion Act_vehiculo ~
Agencia Anterior_Nit Apellido1 Apellido2 Aut_CentralRiesgo Calificacion ~
Capacidad_Pago Carnet Celular Codigo_CIIU Cod_Anterior Cod_Cargo ~
Cod_Empresa Cod_Ingreso Cod_Profesion Cod_Retiro Cod_Segmento Cod_Zona ~
Conocimiento_Cliente Con_Sospechosas Destino Dias_Sancion Dir_comercial ~
Dir_Correspondencia Dir_Residencia Edad Email Endeud_Indirecto Estado ~
Estrato Est_Civil FecFin_NoSipla FecIni_NoSipla Fec_ActFirma Fec_ActFoto ~
Fec_Asociacion Fec_Calificacion Fec_expedicion Fec_fallecido Fec_IngEmpresa ~
Fec_Ingreso Fec_IniSancion Fec_ModNit Fec_Nacimiento Fec_Retiro ~
Fec_UltActualiza Firma fotografia Garantia Gran_Contribuyente Grupo ~
GtoFinanc_Indir Gto_Arriendo Gto_Familiar Gto_obligacion Id_ExoneradoSipla ~
Id_Preexistentes Id_Privilegiado Id_PuedeCodeudar Id_Retencion ~
Ing_arriendos Ing_financieros Ing_Honorarios Ing_Otros Lugar_comercial ~
Lugar_expedicion Lugar_Nacimiento Lugar_Residencia Med_Publicitario ~
Mora_Comercial Nit Niv_Educativo Nombre Nom_Arrendatario Num_Hijos ~
Per_Acargo Puntaje Reestructurado Reportado_fiscalia Reportado_Procredito ~
Reportado_Super Respaldo_Patrim Salario Sancionado Sdo_Obligaciones Sexo ~
Subgrupo Tel_Arrendatario Tel_comercial Tel_Residencia Tiempo_Asociacion ~
Tiempo_Empresa Tipo_Actividad Tipo_Cliente Tipo_Identificacion Tipo_Vinculo ~
Tipo_Vivienda Tip_Contrato Ult_UsuImagen Usuario 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "\\192.168.101.9\desarrollo\prg\dClientes.i"
&Scoped-Define DATA-TABLE-NO-UNDO NO-UNDO
&Scoped-define QUERY-STRING-Query-Main FOR EACH Clientes NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH Clientes NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main Clientes
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main Clientes


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      Clientes SCROLLING.
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
     _TblList          = "bdcentral.Clientes"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > bdcentral.Clientes.Act_casa
"Act_casa" "Act_casa" "Val. Propiedad" ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes "Val. Propiedad"
     _FldNameList[2]   > bdcentral.Clientes.Act_inversion
"Act_inversion" "Act_inversion" ? ? "decimal" ? ? ? ? ? ? yes ? no 18.29 yes ?
     _FldNameList[3]   > bdcentral.Clientes.Act_vehiculo
"Act_vehiculo" "Act_vehiculo" "Val. Vehiculo" ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes "Val. Vehiculo"
     _FldNameList[4]   > bdcentral.Clientes.Agencia
"Agencia" "Agencia" ? ? "integer" ? ? ? ? ? ? yes ? no 7.29 yes ?
     _FldNameList[5]   > bdcentral.Clientes.Anterior_Nit
"Anterior_Nit" "Anterior_Nit" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes "Nit Anterior"
     _FldNameList[6]   > bdcentral.Clientes.Apellido1
"Apellido1" "Apellido1" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ?
     _FldNameList[7]   > bdcentral.Clientes.Apellido2
"Apellido2" "Apellido2" ? ? "character" ? ? ? ? ? ? yes ? no 16.14 yes ?
     _FldNameList[8]   > bdcentral.Clientes.Aut_CentralRiesgo
"Aut_CentralRiesgo" "Aut_CentralRiesgo" ? "Si/No" "logical" ? ? ? ? ? ? yes ? no 25.29 yes ?
     _FldNameList[9]   > bdcentral.Clientes.Calificacion
"Calificacion" "Calificacion" ? ? "integer" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[10]   > bdcentral.Clientes.Capacidad_Pago
"Capacidad_Pago" "Capacidad_Pago" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.57 yes ?
     _FldNameList[11]   > bdcentral.Clientes.Carnet
"Carnet" "Carnet" ? ? "character" ? ? ? ? ? ? yes ? no 27 yes ?
     _FldNameList[12]   > bdcentral.Clientes.Celular
"Celular" "Celular" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes ?
     _FldNameList[13]   > bdcentral.Clientes.Codigo_CIIU
"Codigo_CIIU" "Codigo_CIIU" ? ? "integer" ? ? ? ? ? ? yes ? no 10.86 yes ?
     _FldNameList[14]   > bdcentral.Clientes.Cod_Anterior
"Cod_Anterior" "Cod_Anterior" ? ? "character" ? ? ? ? ? ? yes ? no 11.86 yes ?
     _FldNameList[15]   > bdcentral.Clientes.Cod_Cargo
"Cod_Cargo" "Cod_Cargo" ? ? "integer" ? ? ? ? ? ? yes ? no 15.86 yes ?
     _FldNameList[16]   > bdcentral.Clientes.Cod_Empresa
"Cod_Empresa" "Cod_Empresa" ? ? "integer" ? ? ? ? ? ? yes ? no 28.43 yes ?
     _FldNameList[17]   > bdcentral.Clientes.Cod_Ingreso
"Cod_Ingreso" "Cod_Ingreso" ? ? "integer" ? ? ? ? ? ? yes ? no 16.86 yes ?
     _FldNameList[18]   > bdcentral.Clientes.Cod_Profesion
"Cod_Profesion" "Cod_Profesion" ? ? "integer" ? ? ? ? ? ? yes ? no 21 yes ?
     _FldNameList[19]   > bdcentral.Clientes.Cod_Retiro
"Cod_Retiro" "Cod_Retiro" ? ? "integer" ? ? ? ? ? ? yes ? no 14.72 yes ?
     _FldNameList[20]   > bdcentral.Clientes.Cod_Segmento
"Cod_Segmento" "Cod_Segmento" "Segmento" ? "integer" ? ? ? ? ? ? yes ? no 19.72 yes "Segmento"
     _FldNameList[21]   > bdcentral.Clientes.Cod_Zona
"Cod_Zona" "Cod_Zona" ? ? "integer" ? ? ? ? ? ? yes ? no 20.86 yes ?
     _FldNameList[22]   > bdcentral.Clientes.Conocimiento_Cliente
"Conocimiento_Cliente" "Conocimiento_Cliente" ? ? "character" ? ? ? ? ? ? yes ? no 20.14 yes ?
     _FldNameList[23]   > bdcentral.Clientes.Con_Sospechosas
"Con_Sospechosas" "Con_Sospechosas" ? ? "integer" ? ? ? ? ? ? yes ? no 29.72 yes ?
     _FldNameList[24]   > bdcentral.Clientes.Destino
"Destino" "Destino" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes ?
     _FldNameList[25]   > bdcentral.Clientes.Dias_Sancion
"Dias_Sancion" "Dias_Sancion" ? ? "integer" ? ? ? ? ? ? yes ? no 25.86 yes ?
     _FldNameList[26]   > bdcentral.Clientes.Dir_comercial
"Dir_comercial" "Dir_comercial" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes ?
     _FldNameList[27]   > bdcentral.Clientes.Dir_Correspondencia
"Dir_Correspondencia" "Dir_Correspondencia" ? ? "logical" ? ? ? ? ? ? yes ? no 21.57 yes ?
     _FldNameList[28]   > bdcentral.Clientes.Dir_Residencia
"Dir_Residencia" "Dir_Residencia" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes ?
     _FldNameList[29]   > bdcentral.Clientes.Edad
"Edad" "Edad" ? ? "integer" ? ? ? ? ? ? yes ? no 4.72 yes ?
     _FldNameList[30]   > bdcentral.Clientes.Email
"Email" "Email" ? ? "character" ? ? ? ? ? ? yes ? no 50 yes ?
     _FldNameList[31]   > bdcentral.Clientes.Endeud_Indirecto
"Endeud_Indirecto" "Endeud_Indirecto" ? ? "decimal" ? ? ? ? ? ? yes ? no 16.14 yes ?
     _FldNameList[32]   > bdcentral.Clientes.Estado
"Estado" "Estado" ? ? "integer" ? ? ? ? ? ? yes ? no 6.43 yes ?
     _FldNameList[33]   > bdcentral.Clientes.Estrato
"Estrato" "Estrato" ? ? "integer" ? ? ? ? ? ? yes ? no 23 yes ?
     _FldNameList[34]   > bdcentral.Clientes.Est_Civil
"Est_Civil" "Est_Civil" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ?
     _FldNameList[35]   > bdcentral.Clientes.FecFin_NoSipla
"FecFin_NoSipla" "FecFin_NoSipla" ? ? "date" ? ? ? ? ? ? yes ? no 14.57 yes ?
     _FldNameList[36]   > bdcentral.Clientes.FecIni_NoSipla
"FecIni_NoSipla" "FecIni_NoSipla" ? ? "date" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[37]   > bdcentral.Clientes.Fec_ActFirma
"Fec_ActFirma" "Fec_ActFirma" ? ? "date" ? ? ? ? ? ? yes ? no 24.29 yes ?
     _FldNameList[38]   > bdcentral.Clientes.Fec_ActFoto
"Fec_ActFoto" "Fec_ActFoto" ? ? "date" ? ? ? ? ? ? yes ? no 23 yes ?
     _FldNameList[39]   > bdcentral.Clientes.Fec_Asociacion
"Fec_Asociacion" "Fec_Asociacion" ? ? "date" ? ? ? ? ? ? yes ? no 30.57 yes ?
     _FldNameList[40]   > bdcentral.Clientes.Fec_Calificacion
"Fec_Calificacion" "Fec_Calificacion" ? ? "date" ? ? ? ? ? ? yes ? no 16.86 yes ?
     _FldNameList[41]   > bdcentral.Clientes.Fec_expedicion
"Fec_expedicion" "Fec_expedicion" ? ? "date" ? ? ? ? ? ? yes ? no 16.43 yes ?
     _FldNameList[42]   > bdcentral.Clientes.Fec_fallecido
"Fec_fallecido" "Fec_fallecido" ? ? "date" ? ? ? ? ? ? yes ? no 17.43 yes ?
     _FldNameList[43]   > bdcentral.Clientes.Fec_IngEmpresa
"Fec_IngEmpresa" "Fec_IngEmpresa" ? ? "date" ? ? ? ? ? ? yes ? no 22 yes ?
     _FldNameList[44]   > bdcentral.Clientes.Fec_Ingreso
"Fec_Ingreso" "Fec_Ingreso" ? ? "date" ? ? ? ? ? ? yes ? no 10.29 yes ?
     _FldNameList[45]   > bdcentral.Clientes.Fec_IniSancion
"Fec_IniSancion" "Fec_IniSancion" ? ? "date" ? ? ? ? ? ? yes ? no 26.14 yes ?
     _FldNameList[46]   > bdcentral.Clientes.Fec_ModNit
"Fec_ModNit" "Fec_ModNit" ? ? "date" ? ? ? ? ? ? yes ? no 11.14 yes ?
     _FldNameList[47]   > bdcentral.Clientes.Fec_Nacimiento
"Fec_Nacimiento" "Fec_Nacimiento" ? ? "date" ? ? ? ? ? ? yes ? no 19.43 yes ?
     _FldNameList[48]   > bdcentral.Clientes.Fec_Retiro
"Fec_Retiro" "Fec_Retiro" ? ? "date" ? ? ? ? ? ? yes ? no 14.57 yes ?
     _FldNameList[49]   > bdcentral.Clientes.Fec_UltActualiza
"Fec_UltActualiza" "Fec_UltActualiza" ? ? "date" ? ? ? ? ? ? yes ? no 25 yes ?
     _FldNameList[50]   > bdcentral.Clientes.Firma
"Firma" "Firma" ? ? "logical" ? ? ? ? ? ? yes ? no 5.29 yes ?
     _FldNameList[51]   > bdcentral.Clientes.fotografia
"fotografia" "fotografia" ? ? "logical" ? ? ? ? ? ? yes ? no 9.29 yes ?
     _FldNameList[52]   > bdcentral.Clientes.Garantia
"Garantia" "Garantia" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes ?
     _FldNameList[53]   > bdcentral.Clientes.Gran_Contribuyente
"Gran_Contribuyente" "Gran_Contribuyente" ? "Si/No" "logical" ? ? ? ? ? ? yes ? no 17.86 yes ?
     _FldNameList[54]   > bdcentral.Clientes.Grupo
"Grupo" "Grupo" ? ? "integer" ? ? ? ? ? ? yes ? no 5.57 yes ?
     _FldNameList[55]   > bdcentral.Clientes.GtoFinanc_Indir
"GtoFinanc_Indir" "GtoFinanc_Indir" ? ? "decimal" ? ? ? ? ? ? yes ? no 20.72 yes ?
     _FldNameList[56]   > bdcentral.Clientes.Gto_Arriendo
"Gto_Arriendo" "Gto_Arriendo" ? ? "decimal" ? ? ? ? ? ? yes ? no 14.86 yes ?
     _FldNameList[57]   > bdcentral.Clientes.Gto_Familiar
"Gto_Familiar" "Gto_Familiar" ? ? "decimal" ? ? ? ? ? ? yes ? no 18.29 yes ?
     _FldNameList[58]   > bdcentral.Clientes.Gto_obligacion
"Gto_obligacion" "Gto_obligacion" ? ? "decimal" ? ? ? ? ? ? yes ? no 17 yes ?
     _FldNameList[59]   > bdcentral.Clientes.Id_ExoneradoSipla
"Id_ExoneradoSipla" "Id_ExoneradoSipla" ? "Si/No" "logical" ? ? ? ? ? ? yes ? no 17.57 yes ?
     _FldNameList[60]   > bdcentral.Clientes.Id_Preexistentes
"Id_Preexistentes" "Id_Preexistentes" ? "Si/No" "logical" ? ? ? ? ? ? yes ? no 15.72 yes ?
     _FldNameList[61]   > bdcentral.Clientes.Id_Privilegiado
"Id_Privilegiado" "Id_Privilegiado" ? ? "integer" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[62]   > bdcentral.Clientes.Id_PuedeCodeudar
"Id_PuedeCodeudar" "Id_PuedeCodeudar" ? "Si/No" "logical" ? ? ? ? ? ? yes ? no 17.57 yes ?
     _FldNameList[63]   > bdcentral.Clientes.Id_Retencion
"Id_Retencion" "Id_Retencion" ? "Si/No" "logical" ? ? ? ? ? ? yes ? no 21.29 yes ?
     _FldNameList[64]   > bdcentral.Clientes.Ing_arriendos
"Ing_arriendos" "Ing_arriendos" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[65]   > bdcentral.Clientes.Ing_financieros
"Ing_financieros" "Ing_financieros" ? ? "decimal" ? ? ? ? ? ? yes ? no 19.57 yes ?
     _FldNameList[66]   > bdcentral.Clientes.Ing_Honorarios
"Ing_Honorarios" "Ing_Honorarios" ? ? "decimal" ? ? ? ? ? ? yes ? no 19 yes ?
     _FldNameList[67]   > bdcentral.Clientes.Ing_Otros
"Ing_Otros" "Ing_Otros" ? ? "decimal" ? ? ? ? ? ? yes ? no 18.29 yes ?
     _FldNameList[68]   > bdcentral.Clientes.Lugar_comercial
"Lugar_comercial" "Lugar_comercial" ? ? "character" ? ? ? ? ? ? yes ? no 19 yes ?
     _FldNameList[69]   > bdcentral.Clientes.Lugar_expedicion
"Lugar_expedicion" "Lugar_expedicion" ? ? "character" ? ? ? ? ? ? yes ? no 16.14 yes ?
     _FldNameList[70]   > bdcentral.Clientes.Lugar_Nacimiento
"Lugar_Nacimiento" "Lugar_Nacimiento" ? ? "character" ? ? ? ? ? ? yes ? no 16.29 yes ?
     _FldNameList[71]   > bdcentral.Clientes.Lugar_Residencia
"Lugar_Residencia" "Lugar_Residencia" ? ? "character" ? ? ? ? ? ? yes ? no 19.14 yes ?
     _FldNameList[72]   > bdcentral.Clientes.Med_Publicitario
"Med_Publicitario" "Med_Publicitario" ? ? "character" ? ? ? ? ? ? yes ? no 16.72 yes ?
     _FldNameList[73]   > bdcentral.Clientes.Mora_Comercial
"Mora_Comercial" "Mora_Comercial" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes ?
     _FldNameList[74]   > bdcentral.Clientes.Nit
"Nit" "Nit" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[75]   > bdcentral.Clientes.Niv_Educativo
"Niv_Educativo" "Niv_Educativo" ? ? "character" ? ? ? ? ? ? yes ? no 14.57 yes ?
     _FldNameList[76]   > bdcentral.Clientes.Nombre
"Nombre" "Nombre" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes ?
     _FldNameList[77]   > bdcentral.Clientes.Nom_Arrendatario
"Nom_Arrendatario" "Nom_Arrendatario" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes ?
     _FldNameList[78]   > bdcentral.Clientes.Num_Hijos
"Num_Hijos" "Num_Hijos" ? ? "integer" ? ? ? ? ? ? yes ? no 15.57 yes ?
     _FldNameList[79]   > bdcentral.Clientes.Per_Acargo
"Per_Acargo" "Per_Acargo" ? ? "integer" ? ? ? ? ? ? yes ? no 16.57 yes ?
     _FldNameList[80]   > bdcentral.Clientes.Puntaje
"Puntaje" "Puntaje" ? ? "decimal" ? ? ? ? ? ? yes ? no 7 yes ?
     _FldNameList[81]   > bdcentral.Clientes.Reestructurado
"Reestructurado" "Reestructurado" ? ? "integer" ? ? ? ? ? ? yes ? no 14.14 yes ?
     _FldNameList[82]   > bdcentral.Clientes.Reportado_fiscalia
"Reportado_fiscalia" "Reportado_fiscalia" ? "Si/No" "logical" ? ? ? ? ? ? yes ? no 18.57 yes ?
     _FldNameList[83]   > bdcentral.Clientes.Reportado_Procredito
"Reportado_Procredito" "Reportado_Procredito" ? "Si/No" "logical" ? ? ? ? ? ? yes ? no 19.57 yes ?
     _FldNameList[84]   > bdcentral.Clientes.Reportado_Super
"Reportado_Super" "Reportado_Super" ? "Si/No" "logical" ? ? ? ? ? ? yes ? no 23.57 yes ?
     _FldNameList[85]   > bdcentral.Clientes.Respaldo_Patrim
"Respaldo_Patrim" "Respaldo_Patrim" ? ? "character" ? ? ? ? ? ? yes ? no 20.57 yes ?
     _FldNameList[86]   > bdcentral.Clientes.Salario
"Salario" "Salario" ? ? "decimal" ? ? ? ? ? ? yes ? no 18.29 yes ?
     _FldNameList[87]   > bdcentral.Clientes.Sancionado
"Sancionado" "Sancionado" ? "Si/No" "logical" ? ? ? ? ? ? yes ? no 10.86 yes ?
     _FldNameList[88]   > bdcentral.Clientes.Sdo_Obligaciones
"Sdo_Obligaciones" "Sdo_Obligaciones" ? ? "decimal" ? ? ? ? ? ? yes ? no 18.29 yes ?
     _FldNameList[89]   > bdcentral.Clientes.Sexo
"Sexo" "Sexo" ? ? "integer" ? ? ? ? ? ? yes ? no 4.72 yes ?
     _FldNameList[90]   > bdcentral.Clientes.Subgrupo
"Subgrupo" "Subgrupo" ? ? "integer" ? ? ? ? ? ? yes ? no 8.86 yes ?
     _FldNameList[91]   > bdcentral.Clientes.Tel_Arrendatario
"Tel_Arrendatario" "Tel_Arrendatario" ? ? "character" ? ? ? ? ? ? yes ? no 19.86 yes ?
     _FldNameList[92]   > bdcentral.Clientes.Tel_comercial
"Tel_comercial" "Tel_comercial" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes ?
     _FldNameList[93]   > bdcentral.Clientes.Tel_Residencia
"Tel_Residencia" "Tel_Residencia" ? ? "character" ? ? ? ? ? ? yes ? no 24.14 yes ?
     _FldNameList[94]   > bdcentral.Clientes.Tiempo_Asociacion
"Tiempo_Asociacion" "Tiempo_Asociacion" ? ? "integer" ? ? ? ? ? ? yes ? no 24.86 yes ?
     _FldNameList[95]   > bdcentral.Clientes.Tiempo_Empresa
"Tiempo_Empresa" "Tiempo_Empresa" ? ? "integer" ? ? ? ? ? ? yes ? no 29.29 yes ?
     _FldNameList[96]   > bdcentral.Clientes.Tipo_Actividad
"Tipo_Actividad" "Tipo_Actividad" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ?
     _FldNameList[97]   > bdcentral.Clientes.Tipo_Cliente
"Tipo_Cliente" "Tipo_Cliente" ? ? "integer" ? ? ? ? ? ? yes ? no 13.86 yes ?
     _FldNameList[98]   > bdcentral.Clientes.Tipo_Identificacion
"Tipo_Identificacion" "Tipo_Identificacion" ? ? "character" ? ? ? ? ? ? yes ? no 19.72 yes ?
     _FldNameList[99]   > bdcentral.Clientes.Tipo_Vinculo
"Tipo_Vinculo" "Tipo_Vinculo" ? ? "integer" ? ? ? ? ? ? yes ? no 17.14 yes ?
     _FldNameList[100]   > bdcentral.Clientes.Tipo_Vivienda
"Tipo_Vivienda" "Tipo_Vivienda" ? ? "integer" ? ? ? ? ? ? yes ? no 15.43 yes ?
     _FldNameList[101]   > bdcentral.Clientes.Tip_Contrato
"Tip_Contrato" "Tip_Contrato" ? ? "integer" ? ? ? ? ? ? yes ? no 12.29 yes ?
     _FldNameList[102]   > bdcentral.Clientes.Ult_UsuImagen
"Ult_UsuImagen" "Ult_UsuImagen" ? ? "character" ? ? ? ? ? ? yes ? no 28 yes ?
     _FldNameList[103]   > bdcentral.Clientes.Usuario
"Usuario" "Usuario" ? ? "character" ? ? ? ? ? ? yes ? no 7.29 yes ?
     _FldNameList[104]   > "_<CALC>"
"getAgencia(INPUT RowObject.Agencia)" "FAgencia" "Agencia" "x(40)" "character" ? ? ? ? ? ? no ? no 40 no "Agencia"
     _FldNameList[105]   > "_<CALC>"
"RowObject.Apellido1 + "" "" + RowObject.Apellido2 + "" "" + RowObject.Nombre" "FCliente" "Cliente" "x(40)" "character" ? ? ? ? ? ? no ? no 40 no "Cliente"
     _FldNameList[106]   > "_<CALC>"
"getTpVinculo(INPUT RowObject.Tipo_Vinculo)" "FTpVinculo" "Tp. Vínculo" "x(15)" "character" ? ? ? ? ? ? no ? no 15 no "Tp. Vínculo"
     _FldNameList[107]   > "_<CALC>"
"getEmpresa(INPUT RowObject.Cod_Empresa)" "FEmpresa" "Empresa" "x(60)" "character" ? ? ? ? ? ? no ? no 60 no "Empresa"
     _FldNameList[108]   > "_<CALC>"
"getProfesion(INPUT RowObject.Cod_Profesion)" "FProfesion" "Profesión" "x(30)" "character" ? ? ? ? ? ? no ? no 30 no "Profesión"
     _FldNameList[109]   > "_<CALC>"
"getCargo(INPUT RowObject.Cod_Cargo)" "FCargo" "Cargo" "x(30)" "character" ? ? ? ? ? ? no ? no 30 no "Cargo"
     _FldNameList[110]   > "_<CALC>"
"getTpContrato(INPUT RowObject.Tip_Contrato)" "FTpContrato" "Tp. Contrato" "x(15)" "character" ? ? ? ? ? ? no ? no 15 no "Tp. Contrato"
     _FldNameList[111]   > "_<CALC>"
"getUbicacion(INPUT RowObject.Lugar_Residencia)" "FLugResidencia" "Lug. Residencia" "x(50)" "character" ? ? ? ? ? ? no ? no 50 no "Lug. Residencia"
     _FldNameList[112]   > "_<CALC>"
"getUbicacion(INPUT RowObject.Lugar_comercial)" "FLugComercial" "Lug. Comercial" "x(50)" "character" ? ? ? ? ? ? no ? no 50 no "Lug. Comercial"
     _FldNameList[113]   > "_<CALC>"
"getUbicacion(INPUT RowObject.Lugar_Nacimiento)" "FLugNacimiento" "Lug. Nacimiento" "x(50)" "character" ? ? ? ? ? ? no ? no 50 no "Lug. Nacimiento"
     _FldNameList[114]   > "_<CALC>"
"IF (RowObject.Dir_Correspondencia EQ TRUE) THEN 
(""Oficina"") ELSE 
(""Residencia"")" "FEnvCorresp" "Envio Corresp." "x(12)" "character" ? ? ? ? ? ? no ? no 13.72 no "Envio Corresp."
     _FldNameList[115]   > "_<CALC>"
"RowObject.Salario + RowObject.Ing_arriendos + RowObject.Ing_financieros + RowObject.Ing_Honorarios + RowObject.Ing_Otros" "FTotIngresos" "Tot. Ingresos" ">>,>>>,>>>,>>9.99" "Decimal" ? ? ? ? ? ? no ? no 17.14 no "Tot. Ingresos"
     _FldNameList[116]   > "_<CALC>"
"RowObject.Gto_Arriendo + RowObject.Gto_Familiar + RowObject.GtoFinanc_Indir" "FTotEgresos" "Tot. Egresos" ">>>,>>>,>>>,>>9.99" "Decimal" ? ? ? ? ? ? no ? no 18.29 no "Tot. Egresos"
     _FldNameList[117]   > "_<CALC>"
"RowObject.Act_casa + RowObject.Act_inversion + RowObject.Act_vehiculo" "FTotActivos" "Tot. Activos" ">>>,>>>,>>>,>>9.99" "Decimal" ? ? ? ? ? ? no ? no 18.29 no "Tot. Activos"
     _FldNameList[118]   > "_<CALC>"
"IF (RowObject.Reestructurado = 0) THEN (""No Aplica"") 
ELSE (IF (RowObject.Reestructurado = 1) THEN (""Reestructurado"") ELSE (""No Reestructurado""))" "FReestructurado" "Reestructurado" "x(15)" "character" ? ? ? ? ? ? no ? no 15 no "Reestructurado"
     _FldNameList[119]   > "_<CALC>"
"IF (RowObject.Id_Privilegiado = 0) THEN (FALSE) ELSE (TRUE)" "FPrivilegiado" "Privilegiado" "Si/No" "Logical" ? ? ? ? ? ? no ? no 11.14 no "Privilegiado"
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DATA.CALCULATE dTables  DATA.CALCULATE _DB-REQUIRED
PROCEDURE DATA.CALCULATE :
/*------------------------------------------------------------------------------
  Purpose:     Calculate all the Calculated Expressions found in the
               SmartDataObject.
  Parameters:  <none>
------------------------------------------------------------------------------*/
      ASSIGN 
         rowObject.FAgencia = (getAgencia(INPUT RowObject.Agencia))
         rowObject.FCargo = (getCargo(INPUT RowObject.Cod_Cargo))
         rowObject.FCliente = (RowObject.Apellido1 + " " + RowObject.Apellido2 + " " + RowObject.Nombre)
         rowObject.FEmpresa = (getEmpresa(INPUT RowObject.Cod_Empresa))
         rowObject.FEnvCorresp = (IF (RowObject.Dir_Correspondencia EQ TRUE) THEN 
("Oficina") ELSE 
("Residencia"))
         rowObject.FLugComercial = (getUbicacion(INPUT RowObject.Lugar_comercial))
         rowObject.FLugNacimiento = (getUbicacion(INPUT RowObject.Lugar_Nacimiento))
         rowObject.FLugResidencia = (getUbicacion(INPUT RowObject.Lugar_Residencia))
         rowObject.FPrivilegiado = (IF (RowObject.Id_Privilegiado = 0) THEN (FALSE) ELSE (TRUE))
         rowObject.FProfesion = (getProfesion(INPUT RowObject.Cod_Profesion))
         rowObject.FReestructurado = (IF (RowObject.Reestructurado = 0) THEN ("No Aplica") 
ELSE (IF (RowObject.Reestructurado = 1) THEN ("Reestructurado") ELSE ("No Reestructurado")))
         rowObject.FTotActivos = (RowObject.Act_casa + RowObject.Act_inversion + RowObject.Act_vehiculo)
         rowObject.FTotEgresos = (RowObject.Gto_Arriendo + RowObject.Gto_Familiar + RowObject.GtoFinanc_Indir)
         rowObject.FTotIngresos = (RowObject.Salario + RowObject.Ing_arriendos + RowObject.Ing_financieros + RowObject.Ing_Honorarios + RowObject.Ing_Otros)
         rowObject.FTpContrato = (getTpContrato(INPUT RowObject.Tip_Contrato))
         rowObject.FTpVinculo = (getTpVinculo(INPUT RowObject.Tipo_Vinculo))
      .

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

