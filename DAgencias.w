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

{incluido\igetSdo.i}

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
&Scoped-define INTERNAL-TABLES Agencias

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  Agencia Ciudad Datafono1 Datafono2 Datafono3 Datafono4 Direccion email~
 Entidad Estado Fax Fec_Cierre Fec_Creacion Fec_Retiro Fec_Trabajo Id_Cierre~
 Mensaje Modem Nit_Director Nombre Nom_logico PerNom_Actual~
 PorMax_ConcentracionInv Telefono Tip_Agencia ValMax_Bancos ValMax_Caja~
 ValMax_Inversiones Zona
&Scoped-define ENABLED-FIELDS-IN-Agencias Agencia Ciudad Datafono1 ~
Datafono2 Datafono3 Datafono4 Direccion email Entidad Estado Fax Fec_Cierre ~
Fec_Creacion Fec_Retiro Fec_Trabajo Id_Cierre Mensaje Modem Nit_Director ~
Nombre Nom_logico PerNom_Actual PorMax_ConcentracionInv Telefono ~
Tip_Agencia ValMax_Bancos ValMax_Caja ValMax_Inversiones Zona 
&Scoped-Define DATA-FIELDS  Agencia Ciudad Datafono1 Datafono2 Datafono3 Datafono4 Direccion email~
 Entidad Estado Fax Fec_Cierre Fec_Creacion Fec_Retiro Fec_Trabajo Id_Cierre~
 Mensaje Modem Nit_Director Nombre Nom_logico PerNom_Actual~
 PorMax_ConcentracionInv Telefono Tip_Agencia ValMax_Bancos ValMax_Caja~
 ValMax_Inversiones Zona PAgencia CodNom
&Scoped-define DATA-FIELDS-IN-Agencias Agencia Ciudad Datafono1 Datafono2 ~
Datafono3 Datafono4 Direccion email Entidad Estado Fax Fec_Cierre ~
Fec_Creacion Fec_Retiro Fec_Trabajo Id_Cierre Mensaje Modem Nit_Director ~
Nombre Nom_logico PerNom_Actual PorMax_ConcentracionInv Telefono ~
Tip_Agencia ValMax_Bancos ValMax_Caja ValMax_Inversiones Zona 
&Scoped-Define MANDATORY-FIELDS  Agencia Estado Id_Cierre Nombre Tip_Agencia
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST   rowObject.Datafono1 = Agencias.Datafono[1]~
  rowObject.Datafono2 = Agencias.Datafono[2]~
  rowObject.Datafono3 = Agencias.Datafono[3]~
  rowObject.Datafono4 = Agencias.Datafono[4]
&Scoped-Define DATA-FIELD-DEFS "dagencias.i"
&Scoped-Define DATA-TABLE-NO-UNDO NO-UNDO
&Scoped-define QUERY-STRING-Query-Main FOR EACH Agencias NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH Agencias NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main Agencias
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main Agencias


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
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
     _TblList          = "bdcentral.Agencias"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > bdcentral.Agencias.Agencia
"Agencia" "Agencia" ? ? "integer" ? ? ? ? ? ? yes ? yes 7.29 yes ?
     _FldNameList[2]   > bdcentral.Agencias.Ciudad
"Ciudad" "Ciudad" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes ?
     _FldNameList[3]   > bdcentral.Agencias.Datafono[1]
"Datafono[1]" "Datafono1" ? ? "character" ? ? ? ? ? ? yes ? no 8.29 yes ?
     _FldNameList[4]   > bdcentral.Agencias.Datafono[2]
"Datafono[2]" "Datafono2" ? ? "character" ? ? ? ? ? ? yes ? no 8.29 yes ?
     _FldNameList[5]   > bdcentral.Agencias.Datafono[3]
"Datafono[3]" "Datafono3" ? ? "character" ? ? ? ? ? ? yes ? no 8.29 yes ?
     _FldNameList[6]   > bdcentral.Agencias.Datafono[4]
"Datafono[4]" "Datafono4" ? ? "character" ? ? ? ? ? ? yes ? no 8.29 yes ?
     _FldNameList[7]   > bdcentral.Agencias.Direccion
"Direccion" "Direccion" ? ? "character" ? ? ? ? ? ? yes ? no 50 yes ?
     _FldNameList[8]   > bdcentral.Agencias.email
"email" "email" ? ? "character" ? ? ? ? ? ? yes ? no 50 yes ?
     _FldNameList[9]   > bdcentral.Agencias.Entidad
"Entidad" "Entidad" ? ? "integer" ? ? ? ? ? ? yes ? no 7 yes ?
     _FldNameList[10]   > bdcentral.Agencias.Estado
"Estado" "Estado" ? ? "integer" ? ? ? ? ? ? yes ? yes 6.43 yes ?
     _FldNameList[11]   > bdcentral.Agencias.Fax
"Fax" "Fax" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[12]   > bdcentral.Agencias.Fec_Cierre
"Fec_Cierre" "Fec_Cierre" ? ? "date" ? ? ? ? ? ? yes ? no 14.57 yes ?
     _FldNameList[13]   > bdcentral.Agencias.Fec_Creacion
"Fec_Creacion" "Fec_Creacion" ? ? "date" ? ? ? ? ? ? yes ? no 13.86 yes ?
     _FldNameList[14]   > bdcentral.Agencias.Fec_Retiro
"Fec_Retiro" "Fec_Retiro" ? ? "date" ? ? ? ? ? ? yes ? no 14.57 yes ?
     _FldNameList[15]   > bdcentral.Agencias.Fec_Trabajo
"Fec_Trabajo" "Fec_Trabajo" ? ? "date" ? ? ? ? ? ? yes ? no 16 yes ?
     _FldNameList[16]   > bdcentral.Agencias.Id_Cierre
"Id_Cierre" "Id_Cierre" ? ? "logical" ? ? ? ? ? ? yes ? yes 11.72 yes ?
     _FldNameList[17]   > bdcentral.Agencias.Mensaje
"Mensaje" "Mensaje" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes ?
     _FldNameList[18]   > bdcentral.Agencias.Modem
"Modem" "Modem" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[19]   > bdcentral.Agencias.Nit_Director
"Nit_Director" "Nit_Director" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[20]   > bdcentral.Agencias.Nombre
"Nombre" "Nombre" ? ? "character" ? ? ? ? ? ? yes ? yes 40 yes ?
     _FldNameList[21]   > bdcentral.Agencias.Nom_logico
"Nom_logico" "Nom_logico" ? ? "character" ? ? ? ? ? ? yes ? no 11 yes ?
     _FldNameList[22]   > bdcentral.Agencias.PerNom_Actual
"PerNom_Actual" "PerNom_Actual" ? ? "integer" ? ? ? ? ? ? yes ? no 14.14 yes ?
     _FldNameList[23]   > bdcentral.Agencias.PorMax_ConcentracionInv
"PorMax_ConcentracionInv" "PorMax_ConcentracionInv" ? ? "decimal" ? ? ? ? ? ? yes ? no 34.43 yes ?
     _FldNameList[24]   > bdcentral.Agencias.Telefono
"Telefono" "Telefono" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[25]   > bdcentral.Agencias.Tip_Agencia
"Tip_Agencia" "Tip_Agencia" ? ? "character" ? ? ? ? ? ? yes ? yes 14.72 yes ?
     _FldNameList[26]   > bdcentral.Agencias.ValMax_Bancos
"ValMax_Bancos" "ValMax_Bancos" ? ? "decimal" ? ? ? ? ? ? yes ? no 23.14 yes ?
     _FldNameList[27]   > bdcentral.Agencias.ValMax_Caja
"ValMax_Caja" "ValMax_Caja" ? ? "decimal" ? ? ? ? ? ? yes ? no 20.29 yes ?
     _FldNameList[28]   > bdcentral.Agencias.ValMax_Inversiones
"ValMax_Inversiones" "ValMax_Inversiones" ? ? "decimal" ? ? ? ? ? ? yes ? no 24.86 yes ?
     _FldNameList[29]   > bdcentral.Agencias.Zona
"Zona" "Zona" ? ? "integer" ? ? ? ? ? ? yes ? no 4.72 yes ?
     _FldNameList[30]   > "_<CALC>"
"getAgencia(INPUT RowObject.Agencia)" "PAgencia" "Agencia" "x(50)" "character" ? ? ? ? ? ? no ? no 50 no "Agencia"
     _FldNameList[31]   > "_<CALC>"
"TRIM( STRING(RowObject.Agencia)) +  "" - "" + RowObject.Nombre" "CodNom" "CodNom" "x(20)" "character" ? ? ? ? ? ? no ? no 20 no "CodNom"
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
         rowObject.CodNom = (TRIM( STRING(RowObject.Agencia)) +  " - " + RowObject.Nombre)
         rowObject.PAgencia = (getAgencia(INPUT RowObject.Agencia))
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

