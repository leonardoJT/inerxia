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
 DEFINE BUFFER B1PQR FOR PQR.
{&DB-REQUIRED-END}
DEFINE TEMP-TABLE T1PQR NO-UNDO LIKE PQR.



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

DEFINE BUFFER BPqr FOR PQR.

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
&Scoped-define INTERNAL-TABLES PQR

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  Agencia Area_Resp Canal_Servicio Clase_Producto Cod_Producto~
 Descrip_Anulado Descrip_PQR Descrip_Resp Fec_Anu Fec_Fin_PQR Fec_Grabacion~
 Fec_Resp Fec_Solicitud Hora_Anu Hora_Fin_PQR Hora_Grabacion Hora_Resp~
 Hora_Solicitud Motivo Nit Num_PQR Per_Resp Tip_Credito Tip_PQR Usuario~
 Usu_Anulacion Usu_Resp Estado Cod_Proceso Cod_Req
&Scoped-define ENABLED-FIELDS-IN-PQR Agencia Area_Resp Canal_Servicio ~
Clase_Producto Cod_Producto Descrip_Anulado Descrip_PQR Descrip_Resp ~
Fec_Anu Fec_Fin_PQR Fec_Grabacion Fec_Resp Fec_Solicitud Hora_Anu ~
Hora_Fin_PQR Hora_Grabacion Hora_Resp Hora_Solicitud Motivo Nit Num_PQR ~
Per_Resp Tip_Credito Tip_PQR Usuario Usu_Anulacion Usu_Resp Estado ~
Cod_Proceso Cod_Req 
&Scoped-Define DATA-FIELDS  Agencia Area_Resp Canal_Servicio Clase_Producto Cod_Producto~
 Descrip_Anulado Descrip_PQR Descrip_Resp Fec_Anu Fec_Fin_PQR Fec_Grabacion~
 Fec_Resp Fec_Solicitud Hora_Anu Hora_Fin_PQR Hora_Grabacion Hora_Resp~
 Hora_Solicitud Motivo Nit Num_PQR Per_Resp Tip_Credito Tip_PQR Usuario~
 Usu_Anulacion Usu_Resp Estado Cod_Proceso Cod_Req Fnom_producto FAgencia
&Scoped-define DATA-FIELDS-IN-PQR Agencia Area_Resp Canal_Servicio ~
Clase_Producto Cod_Producto Descrip_Anulado Descrip_PQR Descrip_Resp ~
Fec_Anu Fec_Fin_PQR Fec_Grabacion Fec_Resp Fec_Solicitud Hora_Anu ~
Hora_Fin_PQR Hora_Grabacion Hora_Resp Hora_Solicitud Motivo Nit Num_PQR ~
Per_Resp Tip_Credito Tip_PQR Usuario Usu_Anulacion Usu_Resp Estado ~
Cod_Proceso Cod_Req 
&Scoped-Define MANDATORY-FIELDS  Num_PQR
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dpqr.i"
&Scoped-Define DATA-TABLE-NO-UNDO NO-UNDO
&Scoped-define QUERY-STRING-Query-Main FOR EACH PQR NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH PQR NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main PQR
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main PQR


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getAgencia dTables  _DB-REQUIRED
FUNCTION getAgencia RETURNS CHARACTER
    ( INPUT pcCodigo AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getProcesoMisional dTables  _DB-REQUIRED
FUNCTION getProcesoMisional RETURNS CHARACTER
  /* parameter-definitions */
  ( INPUT piCodPM AS INTEGER  /*Codigo Proceso Misional*/ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getProducto dTables  _DB-REQUIRED
FUNCTION getProducto RETURNS CHARACTER
  /* parameter-definitions */
  ( INPUT piClase  AS INTEGER,   /* Clase de Producto Aho-Cred */
    INPUT piCodPro AS INTEGER,   /* Codigo Producto */ 
    INPUT piCodTip AS INTEGER    /* Codigo Tipo Consumo/comercial/microemp*/)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      PQR SCROLLING.
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
      TABLE: B1PQR B "?" ? bdcentral PQR
      TABLE: T1PQR T "?" NO-UNDO bdcentral PQR
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
         WIDTH              = 34.14.
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
     _TblList          = "bdcentral.PQR"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > bdcentral.PQR.Agencia
"Agencia" "Agencia" ? ? "integer" ? ? ? ? ? ? yes ? no 12 yes "Agencia"
     _FldNameList[2]   > bdcentral.PQR.Area_Resp
"Area_Resp" "Area_Resp" ? ? "integer" ? ? ? ? ? ? yes ? no 17 yes "Responsable"
     _FldNameList[3]   > bdcentral.PQR.Canal_Servicio
"Canal_Servicio" "Canal_Servicio" ? ? "integer" ? ? ? ? ? ? yes ? no 15.57 yes "Canal"
     _FldNameList[4]   > bdcentral.PQR.Clase_Producto
"Clase_Producto" "Clase_Producto" ? ? "integer" ? ? ? ? ? ? yes ? no 16.86 yes "Clase!Producto"
     _FldNameList[5]   > bdcentral.PQR.Cod_Producto
"Cod_Producto" "Cod_Producto" ? ? "integer" ? ? ? ? ? ? yes ? no 15.14 yes "Producto"
     _FldNameList[6]   > bdcentral.PQR.Descrip_Anulado
"Descrip_Anulado" "Descrip_Anulado" ? ? "character" ? ? ? ? ? ? yes ? no 250 yes ?
     _FldNameList[7]   > bdcentral.PQR.Descrip_PQR
"Descrip_PQR" "Descrip_PQR" ? "X(50)" "character" ? ? ? ? ? ? yes ? no 50 yes "Descripcion"
     _FldNameList[8]   > bdcentral.PQR.Descrip_Resp
"Descrip_Resp" "Descrip_Resp" ? "X(50)" "character" ? ? ? ? ? ? yes ? no 50 yes "Respuesta"
     _FldNameList[9]   > bdcentral.PQR.Fec_Anu
"Fec_Anu" "Fec_Anu" ? ? "date" ? ? ? ? ? ? yes ? no 19.86 yes "Fecha Anulacion"
     _FldNameList[10]   > bdcentral.PQR.Fec_Fin_PQR
"Fec_Fin_PQR" "Fec_Fin_PQR" ? ? "date" ? ? ? ? ? ? yes ? no 13.72 yes "Fecha Fin"
     _FldNameList[11]   > bdcentral.PQR.Fec_Grabacion
"Fec_Grabacion" "Fec_Grabacion" ? ? "date" ? ? ? ? ? ? yes ? no 23.14 yes "Fecha de Grabacion"
     _FldNameList[12]   > bdcentral.PQR.Fec_Resp
"Fec_Resp" "Fec_Resp" ? ? "date" ? ? ? ? ? ? yes ? no 23.72 yes "Fecha!Respuesta"
     _FldNameList[13]   > bdcentral.PQR.Fec_Solicitud
"Fec_Solicitud" "Fec_Solicitud" ? ? "date" ? ? ? ? ? ? yes ? no 14.14 yes "Fecha!Solicitud"
     _FldNameList[14]   > bdcentral.PQR.Hora_Anu
"Hora_Anu" "Hora_Anu" ? ? "character" ? ? ? ? ? ? yes ? no 18.72 yes "Hora Anulacion"
     _FldNameList[15]   > bdcentral.PQR.Hora_Fin_PQR
"Hora_Fin_PQR" "Hora_Fin_PQR" ? ? "character" ? ? ? ? ? ? yes ? no 12.57 yes "Hora Fin"
     _FldNameList[16]   > bdcentral.PQR.Hora_Grabacion
"Hora_Grabacion" "Hora_Grabacion" ? ? "character" ? ? ? ? ? ? yes ? no 19.14 yes "Hora Grabacion"
     _FldNameList[17]   > bdcentral.PQR.Hora_Resp
"Hora_Resp" "Hora_Resp" ? ? "character" ? ? ? ? ? ? yes ? no 19.72 yes "Hora Respuesta"
     _FldNameList[18]   > bdcentral.PQR.Hora_Solicitud
"Hora_Solicitud" "Hora_Solicitud" ? ? "character" ? ? ? ? ? ? yes ? no 13 yes ?
     _FldNameList[19]   > bdcentral.PQR.Motivo
"Motivo" "Motivo" ? ? "integer" ? ? ? ? ? ? yes ? no 6.29 yes ?
     _FldNameList[20]   > bdcentral.PQR.Nit
"Nit" "Nit" ? ? "character" ? ? ? ? ? ? yes ? no 16.86 yes "Nit Cliente"
     _FldNameList[21]   > bdcentral.PQR.Num_PQR
"Num_PQR" "Num_PQR" ? ? "integer" ? ? ? ? ? ? yes ? yes 9.14 yes "Número"
     _FldNameList[22]   > bdcentral.PQR.Per_Resp
"Per_Resp" "Per_Resp" ? ? "character" ? ? ? ? ? ? yes ? no 20.57 yes ?
     _FldNameList[23]   > bdcentral.PQR.Tip_Credito
"Tip_Credito" "Tip_Credito" ? ? "integer" ? ? ? ? ? ? yes ? no 24.14 yes "Tipo Credito"
     _FldNameList[24]   > bdcentral.PQR.Tip_PQR
"Tip_PQR" "Tip_PQR" ? ? "integer" ? ? ? ? ? ? yes ? no 15.43 yes "Tipo Solcitud"
     _FldNameList[25]   > bdcentral.PQR.Usuario
"Usuario" "Usuario" ? ? "character" ? ? ? ? ? ? yes ? no 17.29 yes ?
     _FldNameList[26]   > bdcentral.PQR.Usu_Anulacion
"Usu_Anulacion" "Usu_Anulacion" ? ? "character" ? ? ? ? ? ? yes ? no 16.86 yes ?
     _FldNameList[27]   > bdcentral.PQR.Usu_Resp
"Usu_Resp" "Usu_Resp" ? ? "character" ? ? ? ? ? ? yes ? no 17.86 yes ?
     _FldNameList[28]   > bdcentral.PQR.Estado
"Estado" "Estado" ? ? "integer" ? ? ? ? ? ? yes ? no 11.14 yes "Estado"
     _FldNameList[29]   > bdcentral.PQR.Cod_Proceso
"Cod_Proceso" "Cod_Proceso" ? ? "integer" ? ? ? ? ? ? yes ? no 17 yes ?
     _FldNameList[30]   > bdcentral.PQR.Cod_Req
"Cod_Req" "Cod_Req" ? ? "integer" ? ? ? ? ? ? yes ? no 14.72 yes ?
     _FldNameList[31]   > "_<CALC>"
"getProducto (INPUT RowObject.Clase_Producto, INPUT RowObject.Cod_Producto, INPUT RowObject.Tip_Credito)" "Fnom_producto" "Producto" "x(15)" "character" ? ? ? ? ? ? no ? no 15 no "Producto"
     _FldNameList[32]   > "_<CALC>"
"getAgencia(INPUT RowObject.Agencia)" "FAgencia" "Agencia" "x(45)" "character" ? ? ? ? ? ? no ? no 45 no ?
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE beginTransactionValidate dTables  _DB-REQUIRED
PROCEDURE beginTransactionValidate :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  FOR EACH rowObjUpd WHERE rowObjUpd.rowmod = "A" :
      FIND LAST BPqr USE-INDEX idx-001 NO-LOCK NO-ERROR.
     ASSIGN 
/*             rowObjUpd.Num_PQR        = NEXT-VALUE(Sec_Pqr) */
            rowObjUpd.Num_PQR = Bpqr.Num_PQR + 1
            rowObjUpd.Fec_Grabacion  = TODAY
            rowObjUpd.Hora_Grabacion = STRING(TIME,'HH:MM:SS')
            rowObjUpd.Hora_Solicitud = STRING(TIME,'HH:MM:SS')
            rowObjUpd.Fec_Anu = ?
            rowObjUpd.Fec_Fin_PQR = ?
            rowObjUpd.Fec_Resp = ?.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

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
         rowObject.Fnom_producto = (getProducto (INPUT RowObject.Clase_Producto, INPUT RowObject.Cod_Producto, INPUT RowObject.Tip_Credito))
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

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE endTransactionValidate dTables  _DB-REQUIRED
PROCEDURE endTransactionValidate :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  
  FIND LAST rowObjUpd NO-LOCK NO-ERROR.

  IF rowObjUpd.rowmod = "A" THEN
     MESSAGE "Su Solicitud queda registrada bajo el Numero " SKIP rowObjUpd.Num_PQR
           VIEW-AS ALERT-BOX INFO BUTTONS OK.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE preTransactionValidate dTables  _DB-REQUIRED
PROCEDURE preTransactionValidate :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

/* Validacion para nuevos */
    FOR EACH rowObjUpd WHERE CAN-DO("A,C",rowObjUpd.rowmod):
        IF rowObjUpd.Canal_Servicio = 0 THEN
            RETURN "Error, debe ingresar Canal de Recepcion".
        
        IF rowObjUpd.Cod_Proceso = 0 THEN
            RETURN "Error, debe ingresar Proceso".
        
        IF rowObjUpd.Cod_Producto = 0 THEN
            RETURN "Error, debe ingresar Producto".

        IF rowObjUpd.Area_Resp = 0 THEN
            RETURN "Error, debe ingresar Area Responsable".

        IF rowObjUpd.Cod_Req = 0 THEN
            RETURN "Error, debe ingresar Requerimiento".

        IF rowObjUpd.Descrip_PQR = "" THEN
           RETURN "Error, debe ingresar descripcion del reclamo".

        IF rowObjUpd.Agencia = 0 THEN
            RETURN "Error, Agencia igual a cero " .
        
        IF rowObjUpd.Usuario = "" THEN
            RETURN "Error, Usuario igual a Blancos!!! ".

    END. /* validacion para nuevos */

/* validacion para ACTUALIZACION * /
    FOR EACH rowObjUpd
        WHERE
            can-do("U",rowObjUpd.rowmod):
        RETURN "Error".
    END. /* validacion para ACTUALIZACION */

    FOR EACH rowObjUpd
        WHERE
            can-do("D",rowObjUpd.rowmod):
        IF rowObjUpd.nit = "" THEN
        RETURN "Error En Borrado".
    END. / * validacion para BORRADO */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

/* ************************  Function Implementations ***************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getAgencia dTables  _DB-REQUIRED
FUNCTION getAgencia RETURNS CHARACTER
    ( INPUT pcCodigo AS INTEGER) :
  /*------------------------------------------------------------------------------
    Purpose:  
      Notes:  
  ------------------------------------------------------------------------------*/

      DEFINE VARIABLE vcReturn AS CHARACTER   NO-UNDO.
      FIND FIRST agencias WHERE agencias.agencia EQ pcCodigo NO-LOCK NO-ERROR.
      IF AVAILABLE agencias THEN DO:
          ASSIGN vcReturn = TRIM(STRING(agencias.agencia)) + " - " + agencias.nombre.
          RETURN vcReturn.
      END.
      ELSE
          RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getProcesoMisional dTables  _DB-REQUIRED
FUNCTION getProcesoMisional RETURNS CHARACTER
  /* parameter-definitions */
  ( INPUT piCodPM AS INTEGER  /*Codigo Proceso Misional*/ ) :

/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  /* Tipo de la tabla Varios en este caso 44 que es fijo Proceso Misional PQR*/
  DEFINE VARIABLE vcValor AS CHARACTER FORMAT "X(30)"  NO-UNDO.

  FIND FIRST varios WHERE varios.tipo   = 44
                      AND varios.codigo = piCodPM
  NO-LOCK NO-ERROR.
  IF AVAILABLE varios THEN DO:
     ASSIGN vcValor = TRIM(STRING(Varios.Codigo)) + " - " + TRIM(Varios.descripcion).
  END.
  ELSE  vcValor = "".
  

  RETURN vcValor.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getProducto dTables  _DB-REQUIRED
FUNCTION getProducto RETURNS CHARACTER
  /* parameter-definitions */
  ( INPUT piClase  AS INTEGER,   /* Clase de Producto Aho-Cred */
    INPUT piCodPro AS INTEGER,   /* Codigo Producto */ 
    INPUT piCodTip AS INTEGER    /* Codigo Tipo Consumo/comercial/microemp*/) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE vcValor AS CHARACTER FORMAT "X(30)"  NO-UNDO.

  CASE piClase:
    WHEN 1 THEN DO:
        FIND FIRST pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ piCodPro NO-LOCK NO-ERROR.
        IF AVAILABLE Pro_Ahorros THEN 
            ASSIGN vcValor = TRIM(STRING(Pro_Ahorros.Cod_Ahorro)) + " - " + Pro_Ahorros.Nom_Producto.
        ELSE
            vcValor = "".
    END.
    WHEN 2 THEN DO:
        FIND FIRST pro_Creditos WHERE Pro_Creditos.Tip_Credito EQ piCodTip
                                  AND Pro_Creditos.Cod_credito EQ piCodPro 
        NO-LOCK NO-ERROR.
        IF AVAILABLE pro_Creditos THEN 
            ASSIGN vcValor = TRIM(STRING(pro_Credito.Cod_Credito)) + " - " + pro_Credito.Nom_Producto.
        ELSE
            vcValor = "".
    END.
  END CASE.


  RETURN vcValor.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

