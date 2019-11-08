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
&Scoped-define INTERNAL-TABLES Relaciones

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  Aprobada Clase_Producto Cod_Producto Cod_relacion Cuenta Descripcion Estado~
 Fec_Inactividad Fec_Ingreso Nit Nit_relacion Usuario Val_Autorizado
&Scoped-define ENABLED-FIELDS-IN-Relaciones Aprobada Clase_Producto ~
Cod_Producto Cod_relacion Cuenta Descripcion Estado Fec_Inactividad ~
Fec_Ingreso Nit Nit_relacion Usuario Val_Autorizado 
&Scoped-Define DATA-FIELDS  Aprobada Clase_Producto Cod_Producto Cod_relacion Cuenta Descripcion Estado~
 Fec_Inactividad Fec_Ingreso Nit Nit_relacion Usuario Val_Autorizado~
 FCliente FPersonaRelacion FDirecRelacion FTelRelacion FEstado FRelacion
&Scoped-define DATA-FIELDS-IN-Relaciones Aprobada Clase_Producto ~
Cod_Producto Cod_relacion Cuenta Descripcion Estado Fec_Inactividad ~
Fec_Ingreso Nit Nit_relacion Usuario Val_Autorizado 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dRelaciones.i"
&Scoped-Define DATA-TABLE-NO-UNDO NO-UNDO
&Scoped-define QUERY-STRING-Query-Main FOR EACH Relaciones NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH Relaciones NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main Relaciones
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main Relaciones


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDireccion dTables  _DB-REQUIRED
FUNCTION getDireccion RETURNS CHARACTER
    ( INPUT pcCodigo AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTelefono dTables  _DB-REQUIRED
FUNCTION getTelefono RETURNS CHARACTER
    ( INPUT pcCodigo AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      Relaciones SCROLLING.
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
         HEIGHT             = 1.5
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
     _TblList          = "bdcentral.Relaciones"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > bdcentral.Relaciones.Aprobada
"Aprobada" "Aprobada" ? "Si/no" "logical" ? ? ? ? ? ? yes ? no 8.72 yes "Aprobada"
     _FldNameList[2]   > bdcentral.Relaciones.Clase_Producto
"Clase_Producto" "Clase_Producto" "Clase_Prod." ? "integer" ? ? ? ? ? ? yes ? no 14.57 yes "Clase Prod."
     _FldNameList[3]   > bdcentral.Relaciones.Cod_Producto
"Cod_Producto" "Cod_Producto" "Cod. Producto" ? "integer" ? ? ? ? ? ? yes ? no 15.14 yes "Cod. Producto"
     _FldNameList[4]   > bdcentral.Relaciones.Cod_relacion
"Cod_relacion" "Cod_relacion" "Cod. Relació" ? "integer" ? ? ? ? ? ? yes ? no 14.14 yes "Cod. Relación"
     _FldNameList[5]   > bdcentral.Relaciones.Cuenta
"Cuenta" "Cuenta" "Cta/Pagaré" ? "character" ? ? ? ? ? ? yes ? no 15.29 yes "Cta/Pagaré"
     _FldNameList[6]   > bdcentral.Relaciones.Descripcion
"Descripcion" "Descripcion" "Descr." ? "character" ? ? ? ? ? ? yes ? no 15 yes "Descr."
     _FldNameList[7]   > bdcentral.Relaciones.Estado
"Estado" "Estado" ? ? "integer" ? ? ? ? ? ? yes ? no 6.43 yes "Estado"
     _FldNameList[8]   > bdcentral.Relaciones.Fec_Inactividad
"Fec_Inactividad" "Fec_Inactividad" "Fec. Inactividad" ? "date" ? ? ? ? ? ? yes ? no 14.57 yes "Fec. Inactividad"
     _FldNameList[9]   > bdcentral.Relaciones.Fec_Ingreso
"Fec_Ingreso" "Fec_Ingreso" "Fec. Ingreso" ? "date" ? ? ? ? ? ? yes ? no 10.29 yes "Fec. Ingreso"
     _FldNameList[10]   > bdcentral.Relaciones.Nit
"Nit" "Nit" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes "Nit"
     _FldNameList[11]   > bdcentral.Relaciones.Nit_relacion
"Nit_relacion" "Nit_relacion" "Nit Relación" ? "character" ? ? ? ? ? ? yes ? no 23.86 yes "Nit Relación"
     _FldNameList[12]   > bdcentral.Relaciones.Usuario
"Usuario" "Usuario" ? ? "character" ? ? ? ? ? ? yes ? no 7.29 yes "Usuario"
     _FldNameList[13]   > bdcentral.Relaciones.Val_Autorizado
"Val_Autorizado" "Val_Autorizado" "Val. Autorizado" ? "decimal" ? ? ? ? ? ? yes ? no 14.29 yes "Val. Autorizado"
     _FldNameList[14]   > "_<CALC>"
"getCliente(INPUT RowObject.Nit)" "FCliente" "Cliente" "x(30)" "character" ? ? ? ? ? ? no ? no 30 no "Cliente"
     _FldNameList[15]   > "_<CALC>"
"getCliente(INPUT RowObject.Nit_relacion)" "FPersonaRelacion" "Persona. Relación" "x(45)" "character" ? ? ? ? ? ? no ? no 45 no "Persona. Relación"
     _FldNameList[16]   > "_<CALC>"
"getDireccion(INPUT RowObject.Nit_relacion)" "FDirecRelacion" "Dir. Relación" "x(40)" "character" ? ? ? ? ? ? no ? no 40 no "Dir. Relación"
     _FldNameList[17]   > "_<CALC>"
"getTelefono(INPUT RowObject.Nit_relacion)" "FTelRelacion" "Tel. Relacion" "x(15)" "character" ? ? ? ? ? ? no ? no 15 no "Tel. Relacion"
     _FldNameList[18]   > "_<CALC>"
"IF (RowObject.Estado = 1) THEN (""Activa"") ELSE (""Inactiva"")" "FEstado" "Estado" "x(10)" "character" ? ? ? ? ? ? no ? no 10 no "Estado"
     _FldNameList[19]   > "_<CALC>"
"getRelacion(INPUT RowObject.Cod_relacion)" "FRelacion" "Relación" "x(30)" "character" ? ? ? ? ? ? no ? no 30 no "Relación"
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
         rowObject.FCliente = (getCliente(INPUT RowObject.Nit))
         rowObject.FDirecRelacion = (getDireccion(INPUT RowObject.Nit_relacion))
         rowObject.FEstado = (IF (RowObject.Estado = 1) THEN ("Activa") ELSE ("Inactiva"))
         rowObject.FPersonaRelacion = (getCliente(INPUT RowObject.Nit_relacion))
         rowObject.FRelacion = (getRelacion(INPUT RowObject.Cod_relacion))
         rowObject.FTelRelacion = (getTelefono(INPUT RowObject.Nit_relacion))
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

/* ************************  Function Implementations ***************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDireccion dTables  _DB-REQUIRED
FUNCTION getDireccion RETURNS CHARACTER
    ( INPUT pcCodigo AS CHARACTER ) :
  /*------------------------------------------------------------------------------
    Purpose:  
      Notes:  
  ------------------------------------------------------------------------------*/
      DEFINE VARIABLE vcReturn AS CHARACTER   NO-UNDO.
      FIND FIRST clientes WHERE clientes.nit EQ pcCodigo NO-LOCK NO-ERROR.
      IF AVAILABLE clientes THEN DO:
          ASSIGN vcReturn = TRIM(bdcentral.Clientes.Dir_Residencia).
          RETURN vcReturn.
      END.
      ELSE
          RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTelefono dTables  _DB-REQUIRED
FUNCTION getTelefono RETURNS CHARACTER
    ( INPUT pcCodigo AS CHARACTER ) :
  /*------------------------------------------------------------------------------
    Purpose:  
      Notes:  
  ------------------------------------------------------------------------------*/
      DEFINE VARIABLE vcReturn AS CHARACTER   NO-UNDO.
      FIND FIRST clientes WHERE clientes.nit EQ pcCodigo NO-LOCK NO-ERROR.
      IF AVAILABLE clientes THEN DO:
          ASSIGN vcReturn = TRIM(bdcentral.Clientes.Tel_Residencia).
          RETURN vcReturn.
      END.
      ELSE
          RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

