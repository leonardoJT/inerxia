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
DEFINE BUFFER BPer_Facturacion FOR per_facturacion.
DEFINE VARIABLE viPer_Factura AS INTEGER INITIAL 0.

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
&Scoped-define INTERNAL-TABLES Per_Facturacion

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  Per_Factura Fec_Inicial Fec_Final Fec_LimPago Estado
&Scoped-define ENABLED-FIELDS-IN-Per_Facturacion Per_Factura Fec_Inicial ~
Fec_Final Fec_LimPago Estado 
&Scoped-Define DATA-FIELDS  Per_Factura Fec_Inicial Fec_Final Fec_LimPago Estado
&Scoped-define DATA-FIELDS-IN-Per_Facturacion Per_Factura Fec_Inicial ~
Fec_Final Fec_LimPago Estado 
&Scoped-Define MANDATORY-FIELDS  Per_Factura
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "\\192.168.101.9\desarrollo\prg\dPer_Facturacion.i"
&Scoped-Define DATA-TABLE-NO-UNDO NO-UNDO
&Scoped-define QUERY-STRING-Query-Main FOR EACH Per_Facturacion NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH Per_Facturacion NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main Per_Facturacion
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main Per_Facturacion


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      Per_Facturacion SCROLLING.
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
         HEIGHT             = 1.65
         WIDTH              = 48.57.
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
     _TblList          = "bdcentral.Per_Facturacion"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > bdcentral.Per_Facturacion.Per_Factura
"Per_Factura" "Per_Factura" "Período" ? "integer" ? ? ? ? ? ? yes ? yes 13.57 yes "Período"
     _FldNameList[2]   > bdcentral.Per_Facturacion.Fec_Inicial
"Fec_Inicial" "Fec_Inicial" "Fec.Ini" ? "date" ? ? ? ? ? ? yes ? no 11.14 yes "Fec.Ini"
     _FldNameList[3]   > bdcentral.Per_Facturacion.Fec_Final
"Fec_Final" "Fec_Final" "Fec.Fin" ? "date" ? ? ? ? ? ? yes ? no 11.72 yes "Fec.Fin"
     _FldNameList[4]   > bdcentral.Per_Facturacion.Fec_LimPago
"Fec_LimPago" "Fec_LimPago" "Fec.Pago" ? "date" ? ? ? ? ? ? yes ? no 20 yes "Fec.Pago"
     _FldNameList[5]   > bdcentral.Per_Facturacion.Estado
"Estado" "Estado" ? ? "integer" ? ? ? ? ? ? yes ? no 6.43 yes ?
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

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE preTransactionValidate dTables  _DB-REQUIRED
PROCEDURE preTransactionValidate :
/*------------------------------------------------------------------------------
  Propósito: Mantenimiento de  Periodos de Facturacion
  Autor:Félix Vargas
  Fecha: 10-Enero-2008       
------------------------------------------------------------------------------*/
RUN SUPER.
FIND LAST BPer_Facturacion.
IF AVAILABLE(BPer_Facturacion) THEN 
    ASSIGN viPer_Factura = BPer_Facturacion.Per_Factura + 1.

FOR EACH RowObjUpd WHERE LOOKUP(RowObjUpd.RowMod,"A,C,D,U":U) NE 0:
    IF  LOOKUP(RowObjUpd.RowMod,"D":U) NE 0 THEN DO:
        FIND LAST BPer_Facturacion WHERE BPer_Facturacion.Estado EQ 3 AND
            Per_Facturacion.Per_Factura EQ rowobjupd.Per_Factura AND
            RowObjUpd.estado EQ 3 NO-ERROR.
        IF NOT AVAILABLE(BPer_Facturacion) THEN
            RETURN "Únicamente Permite Eliminar Último Período Futuro(3)".
    END.

    IF LOOKUP(RowObjUpd.RowMod,"U":U) NE 0 THEN DO:
        IF RowObjUpd.estado NE 3 THEN
            RETURN "No Es Posible Actualizar Las Fechas de facturación Del Periodo...".
        /** Nuevo **/
        ELSE DO:
          IF rowobjupd.fec_final LT rowobjupd.fec_inicial
             OR rowobjupd.Fec_LimPago        LT rowobjupd.fec_final
             OR rowobjupd.fec_inicial        EQ ?
             OR rowobjupd.fec_final          EQ ?
             OR rowobjupd.Fec_LimPago        EQ ? THEN
             RETURN "No Es Posible Actualizar Las Fechas. Revise las Fechas...". 
          FOR EACH BPer_Facturacion WHERE BPer_Facturacion.Per_Factura NE rowobjupd.Per_Factura NO-LOCK:
              IF  BPer_Facturacion.Fec_Final GT rowobjupd.fec_inicial
               OR BPer_Facturacion.Fec_LimPago GT rowobjupd.fec_final
               OR BPer_Facturacion.Fec_LimPago GT rowobjupd.Fec_LimPago THEN
               RETURN "No Es Posible Actualizar Las Fechas. Revise las Fechas...".
          END.
        END.
        /***********/
    END.

    IF LOOKUP(RowObjUpd.RowMod,"A,C":U) NE 0 THEN DO:
       FIND LAST BPer_Facturacion WHERE
                 BPer_Facturacion.Fec_Final   GT rowobjupd.fec_inicial
              OR BPer_Facturacion.Fec_LimPago GT rowobjupd.fec_final
              OR BPer_Facturacion.Fec_LimPago GT rowobjupd.Fec_LimPago
              OR rowobjupd.fec_final          LT rowobjupd.fec_inicial
              OR rowobjupd.Fec_LimPago        LT rowobjupd.fec_final
              OR rowobjupd.fec_inicial        EQ ?
              OR rowobjupd.fec_final          EQ ?
              OR rowobjupd.Fec_LimPago        EQ ? NO-ERROR.
       IF AVAILABLE(BPer_Facturacion) THEN
          RETURN "Revisar Las Fechas del Nuevo Período de Facturación.Intente de Nuevo...".
       ELSE 
         UPDATE rowobjupd.per_factura = viPer_Factura
                rowobjupd.estado = 3.
   END.

END.
END PROCEDURE.

/* /*          IF CAN-FIND (LAST per_facturacion WHERE                             */    */
/* /*                      per_facturacion.Fec_Final   GT rowobjupd.fec_inicial)   */    */
/* /*                   OR per_facturacion.Fec_LimPago GT rowobjupd.fec_final      */    */
/* /*                   OR per_facturacion.Fec_LimPago GT rowobjupd.Fec_LimPago    */    */
/* /*                   OR rowobjupd.fec_final         LT rowobjupd.fec_inicial    */    */
/* /*                   OR rowobjupd.Fec_LimPago       LT rowobjupd.fec_final THEN */    */
/* /*              RETURN "Revisar Fechas...".                                     */    */


/* RUN SUPER.                                                                                                        */
/* FIND LAST BPer_Facturacion.                                                                                       */
/* IF AVAILABLE(BPer_Facturacion) THEN                                                                               */
/*     ASSIGN viPer_Factura = BPer_Facturacion.Per_Factura + 1.                                                      */
/*                                                                                                                   */
/* FOR EACH RowObjUpd WHERE LOOKUP(RowObjUpd.RowMod,"A,C,U":U) NE 0:  /*D*/                                          */
/*     IF  LOOKUP(RowObjUpd.RowMod,"D":U) NE 0 THEN DO:                                                              */
/*         MESSAGE "Voy a Eliminar"                                                                                  */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                                    */
/*         FIND LAST BPer_Facturacion WHERE                                                                          */
/*             BPer_Facturacion.Estado     EQ 3 AND                                                                  */
/*             Per_Facturacion.Per_Factura EQ rowobjupd.Per_Factura AND                                              */
/*             RowObjUpd.estado            EQ 3                                                                      */
/*             NO-ERROR.                                                                                             */
/*         IF AVAILABLE(BPer_Facturacion) THEN                                                                       */
/*             RETURN "Únicamente Permite Eliminar Último Período Futuro(3)".                                        */
/*     END.                                                                                                          */
/*                                                                                                                   */
/*     IF LOOKUP(RowObjUpd.RowMod,"U":U) NE 0 THEN DO:                                                               */
/*        IF RowObjUpd.estado NE 3                                                                                   */
/*           THEN                                                                                                    */
/*             RETURN "No Es Posible Actualizar Un Periodo Vigente ó No Vigente...".                                 */
/*        /** Nuevo **/                                                                                              */
/*        ELSE DO:                                                                                                   */
/*          IF rowobjupd.fec_inicial GE rowobjupd.fec_final OR                                                       */
/*             rowobjupd.fec_final   GE rowobjupd.Fec_LimPago                                                        */
/*             THEN                                                                                                  */
/* /*               OR rowobjupd.fec_inicial        EQ ?      */                                                     */
/* /*               OR rowobjupd.fec_final          EQ ?      */                                                     */
/* /*               OR rowobjupd.Fec_LimPago        EQ ? THEN */                                                     */
/*               RETURN "No Es Posible Actualizar Las Fechas A Un Periodo Futuro".                                   */
/*          ELSE DO:                                                                                                 */
/*            FOR EACH BPer_Facturacion WHERE   /* Registros Anteriores*/                                            */
/*                BPer_Facturacion.Per_Factura LT rowobjupd.Per_Factura NO-LOCK:                                     */
/*                                                                                                                   */
/*                IF BPer_Facturacion.Fec_Inicial GE rowobjupd.fec_inicial OR                                        */
/*                   BPer_Facturacion.Fec_Inicial GE rowobjupd.fec_final   OR                                        */
/*                   BPer_Facturacion.Fec_Inicial GE rowobjupd.Fec_LimPago OR                                        */
/*                   BPer_Facturacion.Fec_Final   GE rowobjupd.fec_final   OR                                        */
/*                   BPer_Facturacion.Fec_Final   GE rowobjupd.Fec_LimPago OR                                        */
/*                   BPer_Facturacion.Fec_LimPago GE rowobjupd.Fec_LimPago                                           */
/*                   THEN                                                                                            */
/*                     RETURN "Fechas Inconsistentes Respecto a los Periodos Anteriores".                            */
/*                IF BPer_Facturacion.Per_Factura + 1 EQ rowobjupd.Per_Factura THEN                                  */
/*                   IF rowobjupd.Fec_Inicial NE BPer_Facturacion.Fec_Final + 1 THEN                                 */
/*                      RETURN "Fecha Inicial Debe Ser Un Día Mas Respecto a la Fecha Final del Periodo Anterior".   */
/*            END.                                                                                                   */
/*                                                                                                                   */
/*            FOR EACH BPer_Facturacion WHERE   /* Registros Posteriores*/                                           */
/*                BPer_Facturacion.Per_Factura GT rowobjupd.Per_Factura NO-LOCK:                                     */
/*                                                                                                                   */
/*                IF BPer_Facturacion.Fec_Inicial LE rowobjupd.fec_inicial OR                                        */
/*                   BPer_Facturacion.Fec_Inicial LE rowobjupd.fec_final   OR                                        */
/*                   BPer_Facturacion.Fec_Inicial LE rowobjupd.Fec_LimPago OR                                        */
/*                   BPer_Facturacion.Fec_Final   LE rowobjupd.fec_final   OR                                        */
/*                   BPer_Facturacion.Fec_Final   LE rowobjupd.Fec_LimPago OR                                        */
/*                   BPer_Facturacion.Fec_LimPago LE rowobjupd.Fec_LimPago                                           */
/*                   THEN                                                                                            */
/*                     RETURN "Fechas Inconsistentes Respecto a los Periodos Posteriores".                           */
/*                IF BPer_Facturacion.Per_Factura + 1 EQ rowobjupd.Per_Factura THEN                                  */
/*                   IF BPer_Facturacion.Fec_Inicial NE rowobjupd.Fec_Final + 1 THEN                                 */
/*                      RETURN "Fecha Inicial Debe Ser Un Día Mas Respecto a la Fecha Final del Periodo Siguiente".  */
/*            END.                                                                                                   */
/*          END.                                                                                                     */
/*        END.                                                                                                       */
/*     END.                                                                                                          */
/*                                                                                                                   */
/*     IF LOOKUP(RowObjUpd.RowMod,"A,C":U) NE 0 THEN DO:                                                             */
/*        FIND LAST BPer_Facturacion WHERE                                                                           */
/*             BPer_Facturacion.Fec_Inicial   GE rowobjupd.Fec_Inicial  OR                                           */
/*             BPer_Facturacion.Fec_Final     GE rowobjupd.Fec_Final    OR                                           */
/*             BPer_Facturacion.Fec_LimPago   GE rowobjupd.Fec_LimPago  OR                                           */
/*             BPer_Facturacion.Fec_Final + 1 NE rowobjupd.Fec_Inicial                                               */
/*             NO-ERROR.                                                                                             */
/*                                                                                                                   */
/* /*                    BPer_Facturacion.Fec_Final   LT rowobjupd.fec_inicial  */                                   */
/* /*                 OR BPer_Facturacion.Fec_LimPago LT rowobjupd.fec_final    */                                   */
/* /*                 OR BPer_Facturacion.Fec_LimPago LT rowobjupd.Fec_LimPago  */                                   */
/* /*                 OR rowobjupd.fec_final          LT rowobjupd.fec_inicial  */                                   */
/* /*                 OR rowobjupd.Fec_LimPago        LT rowobjupd.fec_final    */                                   */
/* /*                 OR rowobjupd.fec_inicial        EQ ?                      */                                   */
/* /*                 OR rowobjupd.fec_final          EQ ?                      */                                   */
/* /*                 OR rowobjupd.Fec_LimPago        EQ ? NO-ERROR.            */                                   */
/*        IF AVAILABLE(BPer_Facturacion) THEN                                                                        */
/*           RETURN "Revisar Las Fechas del Nuevo Período de Facturación.".                                          */
/*        ELSE                                                                                                       */
/*          UPDATE rowobjupd.per_factura = viPer_Factura                                                             */
/*                 rowobjupd.estado = 3.                                                                             */
/*    END.                                                                                                           */
/* END.                                                                                                              */
/* END PROCEDURE.                                                                                                    */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

