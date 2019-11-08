&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE TTMov_InsSipla NO-UNDO LIKE Mov_InsSipla.



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

DEFINE VARIABLE vicnt AS INTEGER INITIAL 0 NO-UNDO. /*Contador de Registros*/

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
&Scoped-define INTERNAL-TABLES TTMov_InsSipla

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  Agencia CodAutoriza Descripcion Estado Fecha_Gestion Fecha_Transaccion~
 Fec_RepROSS Fec_RepUIAF Hora_Gestion Hora_Transaccion Id_Exonerada Id_NUD~
 Id_NUM Id_RepROSS Id_RepUIAF Id_Sospechosa Id_Traslado Instancia~
 Instancia_Anterior Nit Tipo_Registro UsuCajero UsuGestiona UsuReporta~
 Valor_RegManual
&Scoped-define ENABLED-FIELDS-IN-TTMov_InsSipla Agencia CodAutoriza ~
Descripcion Estado Fecha_Gestion Fecha_Transaccion Fec_RepROSS Fec_RepUIAF ~
Hora_Gestion Hora_Transaccion Id_Exonerada Id_NUD Id_NUM Id_RepROSS ~
Id_RepUIAF Id_Sospechosa Id_Traslado Instancia Instancia_Anterior Nit ~
Tipo_Registro UsuCajero UsuGestiona UsuReporta Valor_RegManual 
&Scoped-Define DATA-FIELDS  Agencia CodAutoriza Descripcion Estado Fecha_Gestion Fecha_Transaccion~
 Fec_RepROSS Fec_RepUIAF Hora_Gestion Hora_Transaccion Id_Exonerada Id_NUD~
 Id_NUM Id_RepROSS Id_RepUIAF Id_Sospechosa Id_Traslado Instancia~
 Instancia_Anterior Nit Tipo_Registro UsuCajero UsuGestiona UsuReporta~
 Valor_RegManual FUsu_reporta FUsu_Gestiona FAgencia FCliente FVNUD FVNUM~
 FInstancia FHorTrans FHorGestion
&Scoped-define DATA-FIELDS-IN-TTMov_InsSipla Agencia CodAutoriza ~
Descripcion Estado Fecha_Gestion Fecha_Transaccion Fec_RepROSS Fec_RepUIAF ~
Hora_Gestion Hora_Transaccion Id_Exonerada Id_NUD Id_NUM Id_RepROSS ~
Id_RepUIAF Id_Sospechosa Id_Traslado Instancia Instancia_Anterior Nit ~
Tipo_Registro UsuCajero UsuGestiona UsuReporta Valor_RegManual 
&Scoped-Define MANDATORY-FIELDS  UsuCajero UsuGestiona UsuReporta
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dttmov_inssipla.i"
&Scoped-Define DATA-TABLE-NO-UNDO NO-UNDO
&Scoped-define QUERY-STRING-Query-Main FOR EACH TTMov_InsSipla NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH TTMov_InsSipla NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main TTMov_InsSipla
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main TTMov_InsSipla


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      TTMov_InsSipla SCROLLING.
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
      TABLE: TTMov_InsSipla T "?" NO-UNDO bdcentral Mov_InsSipla
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
         WIDTH              = 48.29.
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
     _TblList          = "Temp-Tables.TTMov_InsSipla"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.TTMov_InsSipla.Agencia
"Agencia" "Agencia" ? ? "integer" ? ? ? ? ? ? yes ? no 7.29 no ?
     _FldNameList[2]   > Temp-Tables.TTMov_InsSipla.CodAutoriza
"CodAutoriza" "CodAutoriza" ? ? "integer" ? ? ? ? ? ? yes ? no 11.14 no ?
     _FldNameList[3]   > Temp-Tables.TTMov_InsSipla.Descripcion
"Descripcion" "Descripcion" ? "X(100)" "character" ? ? ? ? ? ? yes ? no 100 no ?
     _FldNameList[4]   > Temp-Tables.TTMov_InsSipla.Estado
"Estado" "Estado" ? "Gestionada/No Gestionada" "logical" ? ? ? ? ? ? yes ? no 13.72 no ?
     _FldNameList[5]   > Temp-Tables.TTMov_InsSipla.Fecha_Gestion
"Fecha_Gestion" "Fecha_Gestion" "Fec. Gestión" "99/99/9999" "date" ? ? ? ? ? ? yes ? no 11.57 no "Fec. Gestión"
     _FldNameList[6]   > Temp-Tables.TTMov_InsSipla.Fecha_Transaccion
"Fecha_Transaccion" "Fecha_Transaccion" "Fec. Trans." "99/99/9999" "date" ? ? ? ? ? ? yes ? no 10.29 no "Fec. Trans."
     _FldNameList[7]   > Temp-Tables.TTMov_InsSipla.Fec_RepROSS
"Fec_RepROSS" "Fec_RepROSS" "Fec. Rep. ROSS" ? "date" ? ? ? ? ? ? yes ? no 13.57 no "Fec. Rep. ROSS"
     _FldNameList[8]   > Temp-Tables.TTMov_InsSipla.Fec_RepUIAF
"Fec_RepUIAF" "Fec_RepUIAF" "Fec. Rep. UIAF" ? "date" ? ? ? ? ? ? yes ? no 12.43 no "Fec. Rep. UIAF"
     _FldNameList[9]   > Temp-Tables.TTMov_InsSipla.Hora_Gestion
"Hora_Gestion" "Hora_Gestion" ? ? "integer" ? ? ? ? ? ? yes ? no 12.72 no ?
     _FldNameList[10]   > Temp-Tables.TTMov_InsSipla.Hora_Transaccion
"Hora_Transaccion" "Hora_Transaccion" ? ? "integer" ? ? ? ? ? ? yes ? no 16.86 no ?
     _FldNameList[11]   > Temp-Tables.TTMov_InsSipla.Id_Exonerada
"Id_Exonerada" "Id_Exonerada" ? ? "logical" ? ? ? ? ? ? yes ? no 21.86 no ?
     _FldNameList[12]   > Temp-Tables.TTMov_InsSipla.Id_NUD
"Id_NUD" "Id_NUD" ? ? "logical" ? ? ? ? ? ? yes ? no 12.29 no ?
     _FldNameList[13]   > Temp-Tables.TTMov_InsSipla.Id_NUM
"Id_NUM" "Id_NUM" ? ? "logical" ? ? ? ? ? ? yes ? no 13.14 no ?
     _FldNameList[14]   > Temp-Tables.TTMov_InsSipla.Id_RepROSS
"Id_RepROSS" "Id_RepROSS" ? ? "logical" ? ? ? ? ? ? yes ? no 12 no ?
     _FldNameList[15]   > Temp-Tables.TTMov_InsSipla.Id_RepUIAF
"Id_RepUIAF" "Id_RepUIAF" ? ? "logical" ? ? ? ? ? ? yes ? no 17.57 no ?
     _FldNameList[16]   > Temp-Tables.TTMov_InsSipla.Id_Sospechosa
"Id_Sospechosa" "Id_Sospechosa" ? ? "logical" ? ? ? ? ? ? yes ? no 23.29 no ?
     _FldNameList[17]   > Temp-Tables.TTMov_InsSipla.Id_Traslado
"Id_Traslado" "Id_Traslado" ? ? "logical" ? ? ? ? ? ? yes ? no 11 no ?
     _FldNameList[18]   > Temp-Tables.TTMov_InsSipla.Instancia
"Instancia" "Instancia" ? ? "integer" ? ? ? ? ? ? yes ? no 8.43 no ?
     _FldNameList[19]   > Temp-Tables.TTMov_InsSipla.Instancia_Anterior
"Instancia_Anterior" "Instancia_Anterior" "Inst. Anterior" ? "integer" ? ? ? ? ? ? yes ? no 16.72 no "Inst. Anterior"
     _FldNameList[20]   > Temp-Tables.TTMov_InsSipla.Nit
"Nit" "Nit" ? ? "character" ? ? ? ? ? ? yes ? no 14 no ?
     _FldNameList[21]   > Temp-Tables.TTMov_InsSipla.Tipo_Registro
"Tipo_Registro" "Tipo_Registro" "Tp. Registro" ? "character" ? ? ? ? ? ? yes ? no 13 no "Tp. Registro"
     _FldNameList[22]   > Temp-Tables.TTMov_InsSipla.UsuCajero
"UsuCajero" "UsuCajero" ? ? "character" ? ? ? ? ? ? yes ? yes 6 no ?
     _FldNameList[23]   > Temp-Tables.TTMov_InsSipla.UsuGestiona
"UsuGestiona" "UsuGestiona" "Usu. Gestiona" ? "character" ? ? ? ? ? ? yes ? yes 16.14 no "Usu. Gestiona"
     _FldNameList[24]   > Temp-Tables.TTMov_InsSipla.UsuReporta
"UsuReporta" "UsuReporta" "Usu. Reporta" ? "character" ? ? ? ? ? ? yes ? yes 15.14 no "Usu. Reporta"
     _FldNameList[25]   > Temp-Tables.TTMov_InsSipla.Valor_RegManual
"Valor_RegManual" "Valor_RegManual" "Val. RegManual" ? "decimal" ? ? ? ? ? ? yes ? no 16.43 no "Val. RegManual"
     _FldNameList[26]   > "_<CALC>"
"getUsuario(INPUT RowObject.UsuReporta)" "FUsu_reporta" "Usu. Reporta" "x(40)" "character" ? ? ? ? ? ? no ? no 40 no "Usu. Reporta"
     _FldNameList[27]   > "_<CALC>"
"getUsuario(INPUT RowObject.UsuGestiona)" "FUsu_Gestiona" "Usu. Gestiona" "x(40)" "character" ? ? ? ? ? ? no ? no 40 no "Usu. Gestiona"
     _FldNameList[28]   > "_<CALC>"
"getAgencia(INPUT RowObject.Agencia)" "FAgencia" "Agencia" "x(40)" "character" ? ? ? ? ? ? no ? no 40 no "Agencia"
     _FldNameList[29]   > "_<CALC>"
"getCliente(INPUT RowObject.Nit)" "FCliente" "Cliente" "x(40)" "character" ? ? ? ? ? ? no ? no 40 no "Cliente"
     _FldNameList[30]   > "_<CALC>"
"getVNUD(INPUT RowObject.Nit,
INPUT RowObject.Fecha_Transaccion)" "FVNUD" "Val. N.U. Día" ">>>,>>>,>>9.99" "Decimal" ? ? ? ? ? ? no ? no 14.29 no "Val. N.U. Día"
     _FldNameList[31]   > "_<CALC>"
"getVNUM(INPUT RowObject.Nit,
INPUT RowObject.Fecha_Transaccion)" "FVNUM" "Val. N.U. Mes" ">>>,>>>,>>9.99" "Decimal" ? ? ? ? ? ? no ? no 14.29 no "Val. N.U. Mes"
     _FldNameList[32]   > "_<CALC>"
"getInstancia(INPUT RowObject.Instancia)" "FInstancia" "Instancia" "x(35)" "character" ? ? ? ? ? ? no ? no 35 no "Instancia"
     _FldNameList[33]   > "_<CALC>"
"STRING (RowObject.Hora_Transaccion,""HH:MM:SS"")" "FHorTrans" "HoraTrans" "x(8)" "character" ? ? ? ? ? ? no ? no 8 no ?
     _FldNameList[34]   > "_<CALC>"
"STRING (RowObject.Hora_Gestion,""HH:MM:SS"")" "FHorGestion" "HoraGest." "x(8)" "character" ? ? ? ? ? ? no ? no 8 no ?
     _Design-Parent    is WINDOW dTables @ ( 1.15 , 2.57 )
*/  /* QUERY Query-Main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK dTables 


/* ***************************  Main Block  *************************** */

/*     RUN creaTTMov_InsSipla(0, 0 ,(TODAY - DAY(TODAY - 1)), TODAY). */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN 
             
    RUN initializeObject.
  &ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE creaTTMov_InsSipla dTables  _DB-REQUIRED
PROCEDURE creaTTMov_InsSipla :
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

    FOR EACH cfg_instancias WHERE usuario EQ "325" AND tipo_instancia EQ 6 NO-LOCK:
        FOR EACH mov_insSipla WHERE
            (Mov_insSipla.Agencia EQ IPIAgencia OR IPIAgencia EQ 0) AND
            (Mov_insSipla.instancia EQ cfg_instancias.instancia) AND
            (Mov_insSipla.Instancia EQ IPIInstancia OR IPIInstancia EQ 0) AND
            (Mov_insSipla.fecha_Transaccion GE IPDTFecIni AND Mov_insSipla.fecha_Transaccion LE IPDTFecFin) AND
            (Mov_insSipla.estado EQ IPLEstado OR IPLEstado EQ ?) AND
            TRUE NO-LOCK
            BREAK BY mov_insSIpla.Instancia
            BY mov_insSIpla.Agencia
            BY mov_insSIpla.UsuGestiona:
/*             BY mov_insSIpla.usuReporta: /*giocam Feb/11/08*/ */ 
            
            CREATE TTMov_insSipla.
            BUFFER-COPY mov_insSipla TO TTMov_insSipla.

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
         rowObject.FAgencia = (getAgencia(INPUT RowObject.Agencia))
         rowObject.FCliente = (getCliente(INPUT RowObject.Nit))
         rowObject.FHorGestion = (STRING (RowObject.Hora_Gestion,"HH:MM:SS"))
         rowObject.FHorTrans = (STRING (RowObject.Hora_Transaccion,"HH:MM:SS"))
         rowObject.FInstancia = (getInstancia(INPUT RowObject.Instancia))
         rowObject.FUsu_Gestiona = (getUsuario(INPUT RowObject.UsuGestiona))
         rowObject.FUsu_reporta = (getUsuario(INPUT RowObject.UsuReporta))
         rowObject.FVNUD = (getVNUD(INPUT RowObject.Nit,
INPUT RowObject.Fecha_Transaccion))
         rowObject.FVNUM = (getVNUM(INPUT RowObject.Nit,
INPUT RowObject.Fecha_Transaccion))
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE preTransactionValidate dTables 
PROCEDURE preTransactionValidate :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

  /*LOOKUP(RowObjUpd.RowMod, "A,C,U":U)*/

  FOR EACH RowObjUpd WHERE LOOKUP(RowObjUpd.RowMod, "U":U) NE 0:    
      FIND mov_insSipla WHERE 
          mov_insSipla.Agencia              EQ  RowObjUpd.Agencia           AND 
          mov_insSipla.Fecha_Transaccion    EQ  RowObjUpd.Fecha_Transaccion AND
          mov_insSipla.Instancia            EQ  RowObjUpd.Instancia         AND
          mov_insSipla.Nit                  EQ  RowObjUpd.Nit               AND
          mov_insSipla.UsuCajero            EQ  RowObjUpd.UsuCajero         AND
          mov_insSipla.UsuGestiona          EQ  RowObjUpd.UsuGestiona       AND
          mov_insSipla.UsuReporta           EQ  RowObjUpd.UsuReporta
          EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE mov_insSipla THEN 
          ASSIGN mov_insSipla.descripcion = RowObjUpd.descripcion.
      FIND CURRENT mov_insSipla NO-LOCK NO-ERROR.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

