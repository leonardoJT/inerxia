&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE TTAgencias NO-UNDO LIKE Mov_InsSipla.



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



{incluido\igetsdo.i}

DEFINE TEMP-TABLE TTMov_insSipla LIKE Mov_insSipla.

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
&Scoped-define INTERNAL-TABLES TTAgencias

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  Agencia Instancia UsuReporta
&Scoped-define ENABLED-FIELDS-IN-TTAgencias Agencia Instancia UsuReporta 
&Scoped-Define DATA-FIELDS  Agencia Instancia UsuReporta IAgencia ICant IDist
&Scoped-define DATA-FIELDS-IN-TTAgencias Agencia Instancia UsuReporta 
&Scoped-Define MANDATORY-FIELDS  UsuReporta
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dttagencias.i"
&Scoped-Define DATA-TABLE-NO-UNDO NO-UNDO
&Scoped-define QUERY-STRING-Query-Main FOR EACH TTAgencias NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH TTAgencias NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main TTAgencias
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main TTAgencias


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCntAgeInst dTables  _DB-REQUIRED
FUNCTION getCntAgeInst RETURNS INTEGER
    ( INPUT piInstancia   AS INTEGER,
      INPUT piAgencia     AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      TTAgencias SCROLLING.
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
      TABLE: TTAgencias T "?" NO-UNDO bdcentral Mov_InsSipla
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
         WIDTH              = 47.43.
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
     _TblList          = "Temp-Tables.TTAgencias"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.TTAgencias.Agencia
"Agencia" "Agencia" ? ? "integer" ? ? ? ? ? ? yes ? no 7.29 no "Agencia"
     _FldNameList[2]   > Temp-Tables.TTAgencias.Instancia
"Instancia" "Instancia" ? ? "integer" ? ? ? ? ? ? yes ? no 8.43 no ?
     _FldNameList[3]   > Temp-Tables.TTAgencias.UsuReporta
"UsuReporta" "UsuReporta" ? ? "character" ? ? ? ? ? ? yes ? yes 15.14 no ?
     _FldNameList[4]   > "_<CALC>"
"getAgencia(INPUT RowObject.Agencia)" "IAgencia" "Agencia" "x(40)" "character" ? ? ? ? ? ? no ? no 40 no "Agencia"
     _FldNameList[5]   > "_<CALC>"
"getCntAgeInst(INPUT RowObject.Instancia,
INPUT RowObject.Agencia)" "ICant" "Cant. Usu." ">,>>>,>>9" "Integer" ? ? ? ? ? ? no ? no 9.14 no "Cant. Usu."
     _FldNameList[6]   > "_<CALC>"
"99" "IDist" "%Dist." ">>9.99" "Decimal" ? ? ? ? ? ? no ? no 6.29 no "%Dist"
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE creaTTMov_insSipla dTables  _DB-REQUIRED
PROCEDURE creaTTMov_insSipla :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE INPUT  PARAMETER IPIAgencia      AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER IPIInstancia    AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER IPDTFecIni      AS DATE        NO-UNDO.
    DEFINE INPUT  PARAMETER IPDTFecFin      AS DATE        NO-UNDO.    
    DEFINE INPUT  PARAMETER IPLEstado       AS LOGICAL     NO-UNDO.

    EMPTY TEMP-TABLE TTMov_insSipla.
    EMPTY TEMP-TABLE TTAgencias.

    FOR EACH cfg_instancias WHERE usuario EQ "325" AND tipo_instancia EQ 6 NO-LOCK:
        FOR EACH mov_insSipla WHERE
            (Mov_insSipla.Agencia EQ IPIAgencia OR IPIAgencia EQ 0) AND
            (Mov_insSipla.instancia EQ cfg_instancias.instancia) AND
            (Mov_insSipla.Instancia EQ IPIInstancia OR IPIInstancia EQ 0) AND
            (Mov_insSipla.fecha_Transaccion GE IPDTFecIni AND Mov_insSipla.fecha_Transaccion LE IPDTFecFin) AND
            (Mov_insSipla.estado EQ IPLEstado OR IPLEstado EQ ?)
            NO-LOCK
            BREAK BY mov_insSIpla.Instancia
            BY mov_insSIpla.Agencia
            BY mov_insSIpla.usuGestiona:
            
            CREATE TTMov_insSipla.
            BUFFER-COPY mov_insSipla TO TTMov_insSipla.

            IF LAST-OF(mov_insSIpla.Agencia) THEN DO:
                CREATE TTAgencias.
                BUFFER-COPY Mov_insSipla TO TTAgencias.
            END.

        END.
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
         rowObject.IAgencia = (getAgencia(INPUT RowObject.Agencia))
         rowObject.ICant = (getCntAgeInst(INPUT RowObject.Instancia,
INPUT RowObject.Agencia))
         rowObject.IDist = (99)
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCntAgeInst dTables  _DB-REQUIRED
FUNCTION getCntAgeInst RETURNS INTEGER
    ( INPUT piInstancia   AS INTEGER,
      INPUT piAgencia     AS INTEGER) :
  /*------------------------------------------------------------------------------
    Purpose:  Devuelve la cantidad de usuarios que tienen instancias, por instancia-agencia
      Notes:  
  ------------------------------------------------------------------------------*/

      DEFINE VARIABLE viCnt AS INTEGER INITIAL 0 NO-UNDO.
      DEFINE VARIABLE viTot AS INTEGER INITIAL 0 NO-UNDO.

      FOR EACH TTMov_insSipla WHERE TTMov_insSipla.Instancia EQ piInstancia AND
          TTMov_insSipla.Agencia EQ piAgencia NO-LOCK
          BREAK BY TTMov_insSipla.Instancia
          BY TTMov_insSipla.Agencia
          BY mov_insSIpla.UsuGestiona:
/*         BY TTMov_insSipla.UsuReporta: */

/*           IF LAST-OF(TTMov_insSipla.UsuReporta) THEN  */
          IF LAST-OF(mov_insSIpla.UsuGestiona) THEN
              ASSIGN viCnt = viCnt +  1.

      END.
      
      RETURN viCnt.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

