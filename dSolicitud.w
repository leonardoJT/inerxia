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
&Scoped-define INTERNAL-TABLES Solicitud Usuarios Clientes Agencias

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  Agencia Age_DebAutomatico Age_Desembolso Capacidad_Pago Cod_Credito~
 Cod_DebAutomatico Cod_Desembolso Cod_Formato Cod_Negacion Concepto~
 Conocimiento_Cliente Cue_DebAutomatico Cue_Desembolso Cuota Deducible~
 Desembolso Destino DestinoF Endeud_Indirecto Estado Fec_Aprobacion~
 Fec_Retiro Fec_Solicitud For_Interes For_Pago Garantia Id_Adicionales~
 Incremento Lin_Ahorro Monto Mora_Comercial Nit Num_Asesoria Num_CredACanc1~
 Num_CredACanc2 Num_CredACanc3 Num_CredACanc4 Num_CredACanc5 Num_Solicitud~
 Observaciones Pagare Per_Gracia Per_Pago Plazo Puntaje Pun_Negociables~
 Respaldo_Patrim Sistema Tasa Tipo_Actividad Tip_Credito Total_Prestamo~
 Usuario Verificacion
&Scoped-define ENABLED-FIELDS-IN-Solicitud Agencia Age_DebAutomatico ~
Age_Desembolso Capacidad_Pago Cod_Credito Cod_DebAutomatico Cod_Desembolso ~
Cod_Formato Cod_Negacion Concepto Conocimiento_Cliente Cue_DebAutomatico ~
Cue_Desembolso Cuota Deducible Desembolso Destino DestinoF Endeud_Indirecto ~
Estado Fec_Aprobacion Fec_Retiro Fec_Solicitud For_Interes For_Pago ~
Garantia Id_Adicionales Incremento Lin_Ahorro Monto Mora_Comercial Nit ~
Num_Asesoria Num_CredACanc1 Num_CredACanc2 Num_CredACanc3 Num_CredACanc4 ~
Num_CredACanc5 Num_Solicitud Observaciones Pagare Per_Gracia Per_Pago Plazo ~
Puntaje Pun_Negociables Respaldo_Patrim Sistema Tasa Tipo_Actividad ~
Tip_Credito Total_Prestamo Usuario Verificacion 
&Scoped-Define DATA-FIELDS  Agencia Fle Age_DebAutomatico Age_Desembolso Capacidad_Pago Cod_Credito~
 Cod_DebAutomatico Cod_Desembolso Cod_Formato Cod_Negacion Concepto~
 Conocimiento_Cliente Cue_DebAutomatico Cue_Desembolso Cuota Deducible~
 Desembolso Destino DestinoF Endeud_Indirecto Estado Fec_Aprobacion~
 Fec_Retiro Fec_Solicitud For_Interes For_Pago Garantia Id_Adicionales~
 Incremento Lin_Ahorro Monto Mora_Comercial Nit Num_Asesoria Num_CredACanc1~
 Num_CredACanc2 Num_CredACanc3 Num_CredACanc4 Num_CredACanc5 Num_Solicitud~
 Observaciones Pagare Per_Gracia Per_Pago Plazo Puntaje Pun_Negociables~
 Respaldo_Patrim Sistema Tasa Tipo_Actividad Tip_Credito Total_Prestamo~
 Usuario Verificacion Nombre Apellido1 Apellido2 Nombre-2 Nombre-3
&Scoped-define DATA-FIELDS-IN-Solicitud Agencia Age_DebAutomatico ~
Age_Desembolso Capacidad_Pago Cod_Credito Cod_DebAutomatico Cod_Desembolso ~
Cod_Formato Cod_Negacion Concepto Conocimiento_Cliente Cue_DebAutomatico ~
Cue_Desembolso Cuota Deducible Desembolso Destino DestinoF Endeud_Indirecto ~
Estado Fec_Aprobacion Fec_Retiro Fec_Solicitud For_Interes For_Pago ~
Garantia Id_Adicionales Incremento Lin_Ahorro Monto Mora_Comercial Nit ~
Num_Asesoria Num_CredACanc1 Num_CredACanc2 Num_CredACanc3 Num_CredACanc4 ~
Num_CredACanc5 Num_Solicitud Observaciones Pagare Per_Gracia Per_Pago Plazo ~
Puntaje Pun_Negociables Respaldo_Patrim Sistema Tasa Tipo_Actividad ~
Tip_Credito Total_Prestamo Usuario Verificacion 
&Scoped-define DATA-FIELDS-IN-Usuarios Nombre 
&Scoped-define DATA-FIELDS-IN-Clientes Apellido1 Apellido2 Nombre-2 
&Scoped-define DATA-FIELDS-IN-Agencias Nombre-3 
&Scoped-Define MANDATORY-FIELDS  Fec_Solicitud Num_Solicitud Nombre Nombre-3
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST   rowObject.Num_CredACanc1 = Solicitud.Num_CredACanc[1]~
  rowObject.Num_CredACanc2 = Solicitud.Num_CredACanc[2]~
  rowObject.Num_CredACanc3 = Solicitud.Num_CredACanc[3]~
  rowObject.Num_CredACanc4 = Solicitud.Num_CredACanc[4]~
  rowObject.Num_CredACanc5 = Solicitud.Num_CredACanc[5]~
  rowObject.Nombre-2 = Clientes.Nombre  rowObject.Nombre-3 = Agencias.Nombre
&Scoped-Define DATA-FIELD-DEFS "z:\prg\dsolicitud.i"
&Scoped-Define DATA-TABLE-NO-UNDO NO-UNDO
&Scoped-define QUERY-STRING-Query-Main FOR EACH Solicitud NO-LOCK, ~
      EACH Usuarios OF Solicitud NO-LOCK, ~
      EACH Clientes OF Solicitud NO-LOCK, ~
      EACH Agencias OF Solicitud NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH Solicitud NO-LOCK, ~
      EACH Usuarios OF Solicitud NO-LOCK, ~
      EACH Clientes OF Solicitud NO-LOCK, ~
      EACH Agencias OF Solicitud NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main Solicitud Usuarios Clientes ~
Agencias
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main Solicitud
&Scoped-define SECOND-TABLE-IN-QUERY-Query-Main Usuarios
&Scoped-define THIRD-TABLE-IN-QUERY-Query-Main Clientes
&Scoped-define FOURTH-TABLE-IN-QUERY-Query-Main Agencias


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      Solicitud, 
      Usuarios, 
      Clientes, 
      Agencias SCROLLING.
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
     _TblList          = "bdcentral.Solicitud,bdcentral.Usuarios OF bdcentral.Solicitud,bdcentral.Clientes OF bdcentral.Solicitud,bdcentral.Agencias OF bdcentral.Solicitud"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > bdcentral.Solicitud.Agencia
"Agencia" "Agencia" ? ? "integer" ? ? ? ? ? ? yes ? no 7.29 yes ?
     _FldNameList[2]   > "_<CALC>"
"RowObject.Observaciones" "Fle" "Vínculo" "x(255)" "character" ? ? ? ? ? ? no ? no 255 no "Vínculo"
     _FldNameList[3]   > bdcentral.Solicitud.Age_DebAutomatico
"Age_DebAutomatico" "Age_DebAutomatico" ? ? "integer" ? ? ? ? ? ? yes ? no 18.43 yes ?
     _FldNameList[4]   > bdcentral.Solicitud.Age_Desembolso
"Age_Desembolso" "Age_Desembolso" ? ? "integer" ? ? ? ? ? ? yes ? no 16.29 yes ?
     _FldNameList[5]   > bdcentral.Solicitud.Capacidad_Pago
"Capacidad_Pago" "Capacidad_Pago" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.57 yes ?
     _FldNameList[6]   > bdcentral.Solicitud.Cod_Credito
"Cod_Credito" "Cod_Credito" ? ? "integer" ? ? ? ? ? ? yes ? no 15.14 yes ?
     _FldNameList[7]   > bdcentral.Solicitud.Cod_DebAutomatico
"Cod_DebAutomatico" "Cod_DebAutomatico" ? ? "integer" ? ? ? ? ? ? yes ? no 18.57 yes ?
     _FldNameList[8]   > bdcentral.Solicitud.Cod_Desembolso
"Cod_Desembolso" "Cod_Desembolso" ? ? "integer" ? ? ? ? ? ? yes ? no 16.43 yes ?
     _FldNameList[9]   > bdcentral.Solicitud.Cod_Formato
"Cod_Formato" "Cod_Formato" ? ? "integer" ? ? ? ? ? ? yes ? no 17.43 yes ?
     _FldNameList[10]   > bdcentral.Solicitud.Cod_Negacion
"Cod_Negacion" "Cod_Negacion" ? ? "integer" ? ? ? ? ? ? yes ? no 18.57 yes ?
     _FldNameList[11]   > bdcentral.Solicitud.Concepto
"Concepto" "Concepto" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ?
     _FldNameList[12]   > bdcentral.Solicitud.Conocimiento_Cliente
"Conocimiento_Cliente" "Conocimiento_Cliente" ? ? "character" ? ? ? ? ? ? yes ? no 20.14 yes ?
     _FldNameList[13]   > bdcentral.Solicitud.Cue_DebAutomatico
"Cue_DebAutomatico" "Cue_DebAutomatico" ? ? "character" ? ? ? ? ? ? yes ? no 23.72 yes ?
     _FldNameList[14]   > bdcentral.Solicitud.Cue_Desembolso
"Cue_Desembolso" "Cue_Desembolso" ? ? "character" ? ? ? ? ? ? yes ? no 17 yes ?
     _FldNameList[15]   > bdcentral.Solicitud.Cuota
"Cuota" "Cuota" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.43 yes ?
     _FldNameList[16]   > bdcentral.Solicitud.Deducible
"Deducible" "Deducible" ? ? "decimal" ? ? ? ? ? ? yes ? no 16 yes ?
     _FldNameList[17]   > bdcentral.Solicitud.Desembolso
"Desembolso" "Desembolso" ? ? "integer" ? ? ? ? ? ? yes ? no 19.14 yes ?
     _FldNameList[18]   > bdcentral.Solicitud.Destino
"Destino" "Destino" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes ?
     _FldNameList[19]   > bdcentral.Solicitud.DestinoF
"DestinoF" "DestinoF" ? ? "integer" ? ? ? ? ? ? yes ? no 17.72 yes ?
     _FldNameList[20]   > bdcentral.Solicitud.Endeud_Indirecto
"Endeud_Indirecto" "Endeud_Indirecto" ? ? "decimal" ? ? ? ? ? ? yes ? no 16.14 yes ?
     _FldNameList[21]   > bdcentral.Solicitud.Estado
"Estado" "Estado" ? ? "integer" ? ? ? ? ? ? yes ? no 6.43 yes ?
     _FldNameList[22]   > bdcentral.Solicitud.Fec_Aprobacion
"Fec_Aprobacion" "Fec_Aprobacion" ? ? "date" ? ? ? ? ? ? yes ? no 19.29 yes ?
     _FldNameList[23]   > bdcentral.Solicitud.Fec_Retiro
"Fec_Retiro" "Fec_Retiro" ? ? "date" ? ? ? ? ? ? yes ? no 14.57 yes ?
     _FldNameList[24]   > bdcentral.Solicitud.Fec_Solicitud
"Fec_Solicitud" "Fec_Solicitud" ? ? "date" ? ? ? ? ? ? yes ? yes 14.14 yes "Fecha!Solitud"
     _FldNameList[25]   > bdcentral.Solicitud.For_Interes
"For_Interes" "For_Interes" ? ? "integer" ? ? ? ? ? ? yes ? no 11 yes ?
     _FldNameList[26]   > bdcentral.Solicitud.For_Pago
"For_Pago" "For_Pago" ? ? "integer" ? ? ? ? ? ? yes ? no 11.14 yes ?
     _FldNameList[27]   > bdcentral.Solicitud.Garantia
"Garantia" "Garantia" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes ?
     _FldNameList[28]   > bdcentral.Solicitud.Id_Adicionales
"Id_Adicionales" "Id_Adicionales" ? ? "integer" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[29]   > bdcentral.Solicitud.Incremento
"Incremento" "Incremento" ? ? "decimal" ? ? ? ? ? ? yes ? no 18.29 yes ?
     _FldNameList[30]   > bdcentral.Solicitud.Lin_Ahorro
"Lin_Ahorro" "Lin_Ahorro" ? ? "integer" ? ? ? ? ? ? yes ? no 11.72 yes ?
     _FldNameList[31]   > bdcentral.Solicitud.Monto
"Monto" "Monto" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.43 yes ?
     _FldNameList[32]   > bdcentral.Solicitud.Mora_Comercial
"Mora_Comercial" "Mora_Comercial" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes ?
     _FldNameList[33]   > bdcentral.Solicitud.Nit
"Nit" "Nit" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[34]   > bdcentral.Solicitud.Num_Asesoria
"Num_Asesoria" "Num_Asesoria" ? ? "integer" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[35]   > bdcentral.Solicitud.Num_CredACanc[1]
"Num_CredACanc[1]" "Num_CredACanc1" ? ? "integer" ? ? ? ? ? ? yes ? no 24.14 yes ?
     _FldNameList[36]   > bdcentral.Solicitud.Num_CredACanc[2]
"Num_CredACanc[2]" "Num_CredACanc2" ? ? "integer" ? ? ? ? ? ? yes ? no 24.14 yes ?
     _FldNameList[37]   > bdcentral.Solicitud.Num_CredACanc[3]
"Num_CredACanc[3]" "Num_CredACanc3" ? ? "integer" ? ? ? ? ? ? yes ? no 24.14 yes ?
     _FldNameList[38]   > bdcentral.Solicitud.Num_CredACanc[4]
"Num_CredACanc[4]" "Num_CredACanc4" ? ? "integer" ? ? ? ? ? ? yes ? no 24.14 yes ?
     _FldNameList[39]   > bdcentral.Solicitud.Num_CredACanc[5]
"Num_CredACanc[5]" "Num_CredACanc5" ? ? "integer" ? ? ? ? ? ? yes ? no 24.14 yes ?
     _FldNameList[40]   > bdcentral.Solicitud.Num_Solicitud
"Num_Solicitud" "Num_Solicitud" ? ? "integer" ? ? ? ? ? ? yes ? yes 11.86 yes "Número!Solicitud"
     _FldNameList[41]   > bdcentral.Solicitud.Observaciones
"Observaciones" "Observaciones" ? ? "character" ? ? ? ? ? ? yes ? no 50 yes ?
     _FldNameList[42]   > bdcentral.Solicitud.Pagare
"Pagare" "Pagare" ? ? "character" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[43]   > bdcentral.Solicitud.Per_Gracia
"Per_Gracia" "Per_Gracia" ? ? "integer" ? ? ? ? ? ? yes ? no 16.57 yes ?
     _FldNameList[44]   > bdcentral.Solicitud.Per_Pago
"Per_Pago" "Per_Pago" ? ? "integer" ? ? ? ? ? ? yes ? no 17.43 yes ?
     _FldNameList[45]   > bdcentral.Solicitud.Plazo
"Plazo" "Plazo" ? ? "integer" ? ? ? ? ? ? yes ? no 5.29 yes ?
     _FldNameList[46]   > bdcentral.Solicitud.Puntaje
"Puntaje" "Puntaje" ? ? "decimal" ? ? ? ? ? ? yes ? no 9.72 yes ?
     _FldNameList[47]   > bdcentral.Solicitud.Pun_Negociables
"Pun_Negociables" "Pun_Negociables" ? ? "decimal" ? ? ? ? ? ? yes ? no 18.57 yes ?
     _FldNameList[48]   > bdcentral.Solicitud.Respaldo_Patrim
"Respaldo_Patrim" "Respaldo_Patrim" ? ? "character" ? ? ? ? ? ? yes ? no 20.57 yes ?
     _FldNameList[49]   > bdcentral.Solicitud.Sistema
"Sistema" "Sistema" ? ? "integer" ? ? ? ? ? ? yes ? no 6.43 yes ?
     _FldNameList[50]   > bdcentral.Solicitud.Tasa
"Tasa" "Tasa" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.86 yes ?
     _FldNameList[51]   > bdcentral.Solicitud.Tipo_Actividad
"Tipo_Actividad" "Tipo_Actividad" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ?
     _FldNameList[52]   > bdcentral.Solicitud.Tip_Credito
"Tip_Credito" "Tip_Credito" ? ? "integer" ? ? ? ? ? ? yes ? no 24.14 yes ?
     _FldNameList[53]   > bdcentral.Solicitud.Total_Prestamo
"Total_Prestamo" "Total_Prestamo" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[54]   > bdcentral.Solicitud.Usuario
"Usuario" "Usuario" ? ? "character" ? ? ? ? ? ? yes ? no 7.29 yes "Usuario"
     _FldNameList[55]   > bdcentral.Solicitud.Verificacion
"Verificacion" "Verificacion" ? ? "character" ? ? ? ? ? ? yes ? no 90 yes ?
     _FldNameList[56]   > bdcentral.Usuarios.Nombre
"Nombre" "Nombre" "Nombre Usuario" ? "character" ? ? ? ? ? ? no ? yes 40 yes "Nombre Usuario"
     _FldNameList[57]   > bdcentral.Clientes.Apellido1
"Apellido1" "Apellido1" ? ? "character" ? ? ? ? ? ? no ? no 15 yes "Primer!Apellido"
     _FldNameList[58]   > bdcentral.Clientes.Apellido2
"Apellido2" "Apellido2" ? ? "character" ? ? ? ? ? ? no ? no 16.14 yes "Segundo!Apellido"
     _FldNameList[59]   > bdcentral.Clientes.Nombre
"Nombre" "Nombre-2" "Nombres" ? "character" ? ? ? ? ? ? no ? no 40 yes "Nombres"
     _FldNameList[60]   > bdcentral.Agencias.Nombre
"Nombre" "Nombre-3" "Agencia" ? "character" ? ? ? ? ? ? no ? yes 40 yes "Agencia"
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
         rowObject.Fle = (RowObject.Observaciones)
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

