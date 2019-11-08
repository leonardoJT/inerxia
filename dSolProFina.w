&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tCreditos NO-UNDO LIKE Creditos.



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

EMPTY TEMP-TABLE tCreditos.

CREATE tCreditos.
ASSIGN tcreditos.agencia = 24.

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
&Scoped-define INTERNAL-TABLES tCreditos Agencias Ubicacion Clientes

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  Agencia TpoCliente OtroTpoCliente CptcionClccion PrdctoSlctar~
 OtroPrdctoSlctar Monto Plazo Grntia Linea reestrctrcion FrmaPgo dstncion~
 Cuota
&Scoped-define ENABLED-FIELDS-IN-tCreditos Agencia TpoCliente ~
OtroTpoCliente CptcionClccion PrdctoSlctar OtroPrdctoSlctar Monto Plazo ~
Grntia Linea reestrctrcion FrmaPgo dstncion Cuota 
&Scoped-Define DATA-FIELDS  Agencia NombreAgencia Ciudad NombreCiudad Fec_UltActualiza TpoCliente~
 OtroTpoCliente CptcionClccion PrdctoSlctar OtroPrdctoSlctar Monto Plazo~
 Grntia Linea reestrctrcion FrmaPgo dstncion Cuota
&Scoped-define DATA-FIELDS-IN-tCreditos Agencia TpoCliente OtroTpoCliente ~
CptcionClccion PrdctoSlctar OtroPrdctoSlctar Monto Plazo Grntia Linea ~
reestrctrcion FrmaPgo dstncion Cuota 
&Scoped-define DATA-FIELDS-IN-Agencias NombreAgencia Ciudad 
&Scoped-define DATA-FIELDS-IN-Ubicacion NombreCiudad 
&Scoped-define DATA-FIELDS-IN-Clientes Fec_UltActualiza 
&Scoped-Define MANDATORY-FIELDS  NombreAgencia
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST   rowObject.NombreAgencia = Agencias.Nombre~
  rowObject.NombreCiudad = Ubicacion.Nombre~
  rowObject.TpoCliente = tCreditos.Cod_Credito~
  rowObject.OtroTpoCliente = tCreditos.Observaciones~
  rowObject.CptcionClccion = tCreditos.Categoria~
  rowObject.PrdctoSlctar = tCreditos.Per_Pago~
  rowObject.OtroPrdctoSlctar = tCreditos.Pagare~
  rowObject.Grntia = tCreditos.Cue_Desembolso~
  rowObject.Linea = tCreditos.Cue_DebAutomatico~
  rowObject.reestrctrcion = tCreditos.Abogado~
  rowObject.FrmaPgo = tCreditos.CategoriaMes~
  rowObject.dstncion = tCreditos.Nit_Juzgado
&Scoped-Define DATA-FIELD-DEFS "dsolprofina.i"
&Scoped-Define DATA-TABLE-NO-UNDO NO-UNDO
&Scoped-define QUERY-STRING-Query-Main FOR EACH tCreditos NO-LOCK, ~
      EACH Agencias WHERE Agencias.Agencia = tCreditos.Agencia NO-LOCK, ~
      EACH Ubicacion WHERE Ubicacion.Ubicacion = Agencias.Ciudad NO-LOCK, ~
      EACH Clientes WHERE Clientes.Nit = tCreditos.Nit NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH tCreditos NO-LOCK, ~
      EACH Agencias WHERE Agencias.Agencia = tCreditos.Agencia NO-LOCK, ~
      EACH Ubicacion WHERE Ubicacion.Ubicacion = Agencias.Ciudad NO-LOCK, ~
      EACH Clientes WHERE Clientes.Nit = tCreditos.Nit NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main tCreditos Agencias Ubicacion ~
Clientes
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main tCreditos
&Scoped-define SECOND-TABLE-IN-QUERY-Query-Main Agencias
&Scoped-define THIRD-TABLE-IN-QUERY-Query-Main Ubicacion
&Scoped-define FOURTH-TABLE-IN-QUERY-Query-Main Clientes


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fHndle dTables  _DB-REQUIRED
FUNCTION fHndle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      tCreditos, 
      Agencias, 
      Ubicacion, 
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
   Temp-Tables and Buffers:
      TABLE: tCreditos T "?" NO-UNDO bdcentral Creditos
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
         WIDTH              = 43.29.
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
     _TblList          = "Temp-Tables.tCreditos,bdcentral.Agencias WHERE Temp-Tables.tCreditos ...,bdcentral.Ubicacion WHERE bdcentral.Agencias ...,bdcentral.Clientes WHERE Temp-Tables.tCreditos ..."
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _JoinCode[2]      = "bdcentral.Agencias.Agencia = Temp-Tables.tCreditos.Agencia"
     _JoinCode[3]      = "bdcentral.Ubicacion.Ubicacion = bdcentral.Agencias.Ciudad"
     _JoinCode[4]      = "bdcentral.Clientes.Nit = Temp-Tables.tCreditos.Nit"
     _FldNameList[1]   > Temp-Tables.tCreditos.Agencia
"Agencia" "Agencia" ? ? "integer" ? ? ? ? ? ? yes "Agencia" no 7.29 no "Agencia"
     _FldNameList[2]   > bdcentral.Agencias.Nombre
"Nombre" "NombreAgencia" "Nombre Agencia" ? "character" ? ? ? ? ? ? no "Nombre De La Agencia" yes 40 yes "Nombre!Agencia"
     _FldNameList[3]   > bdcentral.Agencias.Ciudad
"Ciudad" "Ciudad" ? ? "character" ? ? ? ? ? ? no "Ciudad" no 8 yes ?
     _FldNameList[4]   > bdcentral.Ubicacion.Nombre
"Nombre" "NombreCiudad" "Nombre Ciudad" ? "character" ? ? ? ? ? ? no ? no 40 yes "Nombre!Ciudad"
     _FldNameList[5]   > bdcentral.Clientes.Fec_UltActualiza
"Fec_UltActualiza" "Fec_UltActualiza" "Ultima Actualización" ? "date" ? ? ? ? ? ? no ? no 25 yes "Ultima!Actualización"
     _FldNameList[6]   > Temp-Tables.tCreditos.Cod_Credito
"Cod_Credito" "TpoCliente" "Tipo Cliente" ? "integer" ? ? ? ? ? ? yes "Tipo De Cliente" no 15.14 no "Tipo!Cliente"
     _FldNameList[7]   > Temp-Tables.tCreditos.Observaciones
"Observaciones" "OtroTpoCliente" "Otro Tipo Cliente" ? "character" ? ? ? ? ? ? yes ? no 50 no "Otro Tipo Cliente"
     _FldNameList[8]   > Temp-Tables.tCreditos.Categoria
"Categoria" "CptcionClccion" "Clase Producto" ? "character" ? ? ? ? ? ? yes ? no 8.86 no "Clase!Producto"
     _FldNameList[9]   > Temp-Tables.tCreditos.Per_Pago
"Per_Pago" "PrdctoSlctar" "Producto A Solicitar" ? "integer" ? ? ? ? ? ? yes "Producto A Solicitar" no 15.29 no "Producto!A Solicitar"
     _FldNameList[10]   > Temp-Tables.tCreditos.Pagare
"Pagare" "OtroPrdctoSlctar" "Otro Producto A Solicitar" ? "character" ? ? ? ? ? ? yes "Cual Otro Producto?" no 14 no "Otro Producto!A Solicitar"
     _FldNameList[11]   > Temp-Tables.tCreditos.Monto
"Monto" "Monto" "Monto" ? "decimal" ? ? ? ? ? ? yes "Monto" no 21.14 no "Monto"
     _FldNameList[12]   > Temp-Tables.tCreditos.Plazo
"Plazo" "Plazo" ? ? "integer" ? ? ? ? ? ? yes "Producto A Solicitar" no 5.29 no "Plazo"
     _FldNameList[13]   > Temp-Tables.tCreditos.Cue_Desembolso
"Cue_Desembolso" "Grntia" "Garantía" ? "character" ? ? ? ? ? ? yes ? no 17 no "Garantía"
     _FldNameList[14]   > Temp-Tables.tCreditos.Cue_DebAutomatico
"Cue_DebAutomatico" "Linea" "Línea" ? "character" ? ? ? ? ? ? yes ? no 18.57 no "Línea"
     _FldNameList[15]   > Temp-Tables.tCreditos.Abogado
"Abogado" "reestrctrcion" "Reestructuración" "S/N" "logical" ? ? ? ? ? ? yes ? no 15.72 no "Reestructuración"
     _FldNameList[16]   > Temp-Tables.tCreditos.CategoriaMes
"CategoriaMes" "FrmaPgo" "Forma De Pago" ? "character" ? ? ? ? ? ? yes ? no 8.86 no "Forma De Pago"
     _FldNameList[17]   > Temp-Tables.tCreditos.Nit_Juzgado
"Nit_Juzgado" "dstncion" "Destinación" ? "character" ? ? ? ? ? ? yes ? no 14 no "Destinación"
     _FldNameList[18]   > Temp-Tables.tCreditos.Cuota
"Cuota" "Cuota" "Cuota" ? "decimal" ? ? ? ? ? ? yes "Cuota" no 21.14 no "Cuota"
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

/* ************************  Function Implementations ***************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fHndle dTables  _DB-REQUIRED
FUNCTION fHndle RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN BUFFER tCreditos:HANDLE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

