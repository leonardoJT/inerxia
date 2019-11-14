&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

 DEFINE        VARIABLE W_Val    AS   LOGICAL.
 DEFINE SHARED VARIABLE W_Manija AS   HANDLE.
 DEFINE        VARIABLE W_WkOfi  LIKE agencias.agencia.
 DEFINE        VARIABLE W_Rowid  AS   ROWID.
 RUN Encuentra_agencia.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME Brw-Pcto

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Pro_Creditos CarteraVencida

/* Definitions for BROWSE Brw-Pcto                                      */
&Scoped-define FIELDS-IN-QUERY-Brw-Pcto CarteraVencida.Cod_Producto Pro_Creditos.Nom_Producto CarteraVencida.Per_Inicial CarteraVencida.Per_Final   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Brw-Pcto   
&Scoped-define SELF-NAME Brw-Pcto
&Scoped-define QUERY-STRING-Brw-Pcto FOR EACH Pro_Creditos WHERE pro_creditos.estado = 1 NO-LOCK, ~
                               EACH CarteraVencida WHERE CarteraVencida.Cod_Producto EQ Pro_Creditos.Cod_Credito NO-LOCK
&Scoped-define OPEN-QUERY-Brw-Pcto OPEN QUERY Brw-Pcto FOR EACH Pro_Creditos WHERE pro_creditos.estado = 1 NO-LOCK, ~
                               EACH CarteraVencida WHERE CarteraVencida.Cod_Producto EQ Pro_Creditos.Cod_Credito NO-LOCK.
&Scoped-define TABLES-IN-QUERY-Brw-Pcto Pro_Creditos CarteraVencida
&Scoped-define FIRST-TABLE-IN-QUERY-Brw-Pcto Pro_Creditos
&Scoped-define SECOND-TABLE-IN-QUERY-Brw-Pcto CarteraVencida


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-Brw-Pcto}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-204 Brw-Pcto W_BPcto 
&Scoped-Define DISPLAYED-OBJECTS W_BPcto 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE W_BPcto AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Código Producto" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-204
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 73 BY 1.35.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Brw-Pcto FOR 
      Pro_Creditos, 
      CarteraVencida SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Brw-Pcto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Brw-Pcto B-table-Win _FREEFORM
  QUERY Brw-Pcto NO-LOCK DISPLAY
      CarteraVencida.Cod_Producto
      Pro_Creditos.Nom_Producto
      CarteraVencida.Per_Inicial   COLUMN-LABEL "Per. Ini"
      CarteraVencida.Per_Final     COLUMN-LABEL "Per. Fin"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SEPARATORS SIZE 73 BY 8.62
         BGCOLOR 15 FGCOLOR 7 FONT 5 ROW-HEIGHT-CHARS .65.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Brw-Pcto AT ROW 1.27 COL 2
     W_BPcto AT ROW 10.42 COL 62 COLON-ALIGNED
     RECT-204 AT ROW 10.15 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
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
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 10.62
         WIDTH              = 75.14.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB Brw-Pcto RECT-204 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Brw-Pcto
/* Query rebuild information for BROWSE Brw-Pcto
     _START_FREEFORM
OPEN QUERY Brw-Pcto FOR EACH Pro_Creditos WHERE pro_creditos.estado = 1 NO-LOCK,
                        EACH CarteraVencida WHERE CarteraVencida.Cod_Producto EQ Pro_Creditos.Cod_Credito NO-LOCK
     _END_FREEFORM
     _Options          = "NO-LOCK"
     _Query            is OPENED
*/  /* BROWSE Brw-Pcto */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME Brw-Pcto
&Scoped-define SELF-NAME Brw-Pcto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Brw-Pcto B-table-Win
ON ROW-ENTRY OF Brw-Pcto IN FRAME F-Main
DO:
      {src/adm/template/brsentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Brw-Pcto B-table-Win
ON ROW-LEAVE OF Brw-Pcto IN FRAME F-Main
DO:
    {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Brw-Pcto B-table-Win
ON VALUE-CHANGED OF Brw-Pcto IN FRAME F-Main
DO:
  {src/adm/template/brschnge.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_BPcto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_BPcto B-table-Win
ON LEAVE OF W_BPcto IN FRAME F-Main /* Código Producto */
DO:
    ASSIGN W_BPcto.

    FIND FIRST CarteraVencida WHERE CarteraVencida.Cod_Producto EQ W_BPcto NO-LOCK NO-ERROR.
    IF AVAILABLE CarteraVencida THEN DO:
        RUN Encuentra_agencia.

        OPEN QUERY Brw-Pcto FOR EACH Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ W_Bpcto
                                                    AND pro_creditos.estado = 1 NO-LOCK,
                                EACH CarteraVencida WHERE CarteraVencida.Cod_Producto EQ W_BPcto NO-LOCK.
    END.
    ELSE DO:
        MESSAGE "Producto no está matriculado en Cartera Vencida."
            VIEW-AS ALERT-BOX ERROR TITLE "Error".

        {&OPEN-QUERY-Brw-Pcto}
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available B-table-Win  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI B-table-Win  _DEFAULT-DISABLE
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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Encuentra_agencia B-table-Win 
PROCEDURE Encuentra_agencia :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 FIND agencias WHERE agencias.Estado      NE 3
                 AND agencias.Tip_Agencia EQ "C"
               NO-LOCK NO-ERROR.
 IF AVAILABLE agencias THEN
    ASSIGN W_WkOfi = agencias.agencia.                  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records B-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "Pro_Creditos"}
  {src/adm/template/snd-list.i "CarteraVencida"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed B-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/bstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

