&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject NO-UNDO
       {"dsolprofina.i"}.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS vTableWin 
/*------------------------------------------------------------------------

  File:

  Description: from viewer.w - Template for SmartDataViewer objects

  Input Parameters:
      <none>

  Output Parameters:
      <none>

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

{src/adm2/widgetprto.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "dsolprofina.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.TpoCliente RowObject.CptcionClccion ~
RowObject.PrdctoSlctar RowObject.Monto RowObject.Plazo RowObject.Grntia ~
RowObject.Cuota RowObject.Linea RowObject.reestrctrcion RowObject.FrmaPgo ~
RowObject.dstncion 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS RECT-325 
&Scoped-Define DISPLAYED-FIELDS RowObject.Agencia RowObject.NombreAgencia ~
RowObject.Ciudad RowObject.NombreCiudad RowObject.Fec_UltActualiza ~
RowObject.TpoCliente RowObject.OtroTpoCliente RowObject.CptcionClccion ~
RowObject.PrdctoSlctar RowObject.Monto RowObject.Plazo RowObject.Grntia ~
RowObject.Cuota RowObject.Linea RowObject.reestrctrcion RowObject.FrmaPgo ~
RowObject.OtroPrdctoSlctar RowObject.dstncion 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject


/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fEnviaDtos vTableWin 
FUNCTION fEnviaDtos RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fNONull vTableWin 
FUNCTION fNONull RETURNS CHARACTER
  (c AS CHAR /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-325
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 114 BY 8.35.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.Agencia AT ROW 1.27 COL 15 COLON-ALIGNED HELP
          "Agencia" NO-LABEL WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 4.86 BY 1 TOOLTIP "Código Agencia"
     RowObject.NombreAgencia AT ROW 1.27 COL 20 COLON-ALIGNED HELP
          "Nombre De La Agencia" NO-LABEL WIDGET-ID 20
          VIEW-AS FILL-IN 
          SIZE 21 BY 1 TOOLTIP "Nombre Agencia"
     RowObject.Ciudad AT ROW 1.27 COL 49 COLON-ALIGNED HELP
          "Ciudad" WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 5 BY 1 TOOLTIP "Código Ciudad"
     RowObject.NombreCiudad AT ROW 1.27 COL 54 COLON-ALIGNED HELP
          "Ingrese el nombre de la ubicación" NO-LABEL WIDGET-ID 22
          VIEW-AS FILL-IN 
          SIZE 21 BY 1 TOOLTIP "Nombre Ciudad"
     RowObject.Fec_UltActualiza AT ROW 1.27 COL 97 COLON-ALIGNED HELP
          "Fecha de actualización de datos" WIDGET-ID 10
          LABEL "Fecha De Entrega"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1 TOOLTIP "Fecha De Entrega"
     RowObject.TpoCliente AT ROW 2.35 COL 15 COLON-ALIGNED HELP
          "Tipo De Cliente" WIDGET-ID 26
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "Solicitante",1,
                     "Codeudor",2,
                     "Avalista",3,
                     "Otro",0
          DROP-DOWN-LIST
          SIZE 13 BY 1 TOOLTIP "Tipo De Cliente"
     RowObject.OtroTpoCliente AT ROW 2.35 COL 28 COLON-ALIGNED HELP
          "Especifique El Otro Tipo" NO-LABEL WIDGET-ID 30
          VIEW-AS FILL-IN 
          SIZE 26 BY 1 TOOLTIP "Cuál Otro Tipo De Cliente"
     RowObject.CptcionClccion AT ROW 2.35 COL 97 COLON-ALIGNED HELP
          "Clase De Producto" WIDGET-ID 32 FORMAT "X(15)"
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "Captación","Captación",
                     "Colocación","Colocación"
          DROP-DOWN-LIST
          SIZE 13 BY 1 TOOLTIP "Clase Producto"
     RowObject.PrdctoSlctar AT ROW 3.42 COL 17 HELP
          "Producto A Solicitar" NO-LABEL WIDGET-ID 58
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Crédito", 1,
"CDAT", 2,
"CDT", 3,
"Ahorro Programado", 4,
"Ahorro Permanente", 5,
"Ahorro Vista", 6,
"Otro", 0,
"Semestral", 8,
"Anual", 9
          SIZE 20 BY 4.58 TOOLTIP "Producto A Solicitar"
          FONT 5
     RowObject.Monto AT ROW 3.42 COL 42 COLON-ALIGNED HELP
          "Monto" WIDGET-ID 48
          VIEW-AS FILL-IN 
          SIZE 25.43 BY 1
     RowObject.Plazo AT ROW 3.42 COL 74 COLON-ALIGNED HELP
          "Plazo" WIDGET-ID 50
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     RowObject.Grntia AT ROW 3.42 COL 97 COLON-ALIGNED HELP
          "Garantía" WIDGET-ID 44
          VIEW-AS FILL-IN 
          SIZE 15.43 BY 1
     RowObject.Cuota AT ROW 4.5 COL 42 COLON-ALIGNED WIDGET-ID 78
          VIEW-AS FILL-IN 
          SIZE 22.43 BY 1
     RowObject.Linea AT ROW 6.38 COL 42 COLON-ALIGNED HELP
          "Línea" WIDGET-ID 46
          VIEW-AS FILL-IN 
          SIZE 15.43 BY 1
     RowObject.reestrctrcion AT ROW 6.38 COL 60 HELP
          "Reestructuración" WIDGET-ID 52
          LABEL "Reestructuración"
          VIEW-AS TOGGLE-BOX
          SIZE 19 BY 1 TOOLTIP "Reestructuración"
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 5 WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     RowObject.FrmaPgo AT ROW 6.38 COL 97.14 COLON-ALIGNED HELP
          "Forma De Pago" WIDGET-ID 54 FORMAT "X(8)"
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "Nómina","Nómina",
                     "Caja","Caja"
          DROP-DOWN-LIST
          SIZE 11 BY 1
     RowObject.OtroPrdctoSlctar AT ROW 8 COL 15 COLON-ALIGNED HELP
          "Cual Otro Producto?" NO-LABEL WIDGET-ID 42
          VIEW-AS FILL-IN 
          SIZE 15.43 BY 1 TOOLTIP "Cuál Otro Producto A Solicitar?"
     RowObject.dstncion AT ROW 8 COL 42 COLON-ALIGNED HELP
          "Destinación" WIDGET-ID 56 FORMAT "X(256)"
          VIEW-AS FILL-IN 
          SIZE 70 BY 1 TOOLTIP "Destinación"
     "Seccional:" VIEW-AS TEXT
          SIZE 10 BY 1 AT ROW 1.27 COL 2 WIDGET-ID 66
     "Solicitar" VIEW-AS TEXT
          SIZE 8 BY 1 AT ROW 6.38 COL 8 WIDGET-ID 76
     "A" VIEW-AS TEXT
          SIZE 2 BY 1 AT ROW 5.31 COL 8 WIDGET-ID 74
     "Producto" VIEW-AS TEXT
          SIZE 8 BY 1 AT ROW 4.23 COL 8 WIDGET-ID 68
     RECT-325 AT ROW 1 COL 1 WIDGET-ID 72
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 5 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dsolprofina.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" NO-UNDO  
      ADDITIONAL-FIELDS:
          {dsolprofina.i}
      END-FIELDS.
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
  CREATE WINDOW vTableWin ASSIGN
         HEIGHT             = 8.42
         WIDTH              = 114.29.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB vTableWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW vTableWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.Agencia IN FRAME F-Main
   NO-ENABLE EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN RowObject.Ciudad IN FRAME F-Main
   NO-ENABLE EXP-HELP                                                   */
/* SETTINGS FOR COMBO-BOX RowObject.CptcionClccion IN FRAME F-Main
   EXP-FORMAT EXP-HELP                                                  */
/* SETTINGS FOR FILL-IN RowObject.dstncion IN FRAME F-Main
   EXP-FORMAT EXP-HELP                                                  */
/* SETTINGS FOR FILL-IN RowObject.Fec_UltActualiza IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-HELP                                         */
/* SETTINGS FOR COMBO-BOX RowObject.FrmaPgo IN FRAME F-Main
   EXP-FORMAT EXP-HELP                                                  */
/* SETTINGS FOR FILL-IN RowObject.Grntia IN FRAME F-Main
   EXP-HELP                                                             */
/* SETTINGS FOR FILL-IN RowObject.Linea IN FRAME F-Main
   EXP-HELP                                                             */
/* SETTINGS FOR FILL-IN RowObject.Monto IN FRAME F-Main
   EXP-HELP                                                             */
/* SETTINGS FOR FILL-IN RowObject.NombreAgencia IN FRAME F-Main
   NO-ENABLE EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN RowObject.NombreCiudad IN FRAME F-Main
   NO-ENABLE EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN RowObject.OtroPrdctoSlctar IN FRAME F-Main
   NO-ENABLE EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN RowObject.OtroTpoCliente IN FRAME F-Main
   NO-ENABLE EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN RowObject.Plazo IN FRAME F-Main
   EXP-HELP                                                             */
/* SETTINGS FOR RADIO-SET RowObject.PrdctoSlctar IN FRAME F-Main
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR TOGGLE-BOX RowObject.reestrctrcion IN FRAME F-Main
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR COMBO-BOX RowObject.TpoCliente IN FRAME F-Main
   EXP-HELP                                                             */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME RowObject.PrdctoSlctar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.PrdctoSlctar vTableWin
ON MOUSE-SELECT-CLICK OF RowObject.PrdctoSlctar IN FRAME F-Main
DO:
    DEF VAR iV AS INTEGER NO-UNDO.
    iV = INTEGER(SELF:SCREEN-VALUE).

    DO WITH FRAME {&FRAME-NAME}:
        RowObject.OtroPrdctoSlctar:SENSITIVE = integer(SELF:SCREEN-VALUE) = 0.
        
        RowObject.dstncion:SENSITIVE            = iv = 1.
        RowObject.reestrctrcion:SENSITIVE       = iv = 1.
        RowObject.Grntia:SENSITIVE              = iv = 1. 
        RowObject.Plazo:SENSITIVE               = iv = 1 OR iv = 2 OR iv = 3  OR iv = 4 .
        RowObject.FrmaPgo:SENSITIVE             = iv = 1  OR iv = 2  OR iv = 3  OR iv = 4  OR iv = 5  OR iv = 6.
        RowObject.Linea:SENSITIVE               = iv = 1 .
        RowObject.Monto:SENSITIVE               = iv = 1 OR iv = 2 OR iv = 3 .
        RowObject.cuota:SENSITIVE               = iv = 4 OR iv = 5 OR iv = 6.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.TpoCliente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.TpoCliente vTableWin
ON VALUE-CHANGED OF RowObject.TpoCliente IN FRAME F-Main /* Tipo Cliente */
DO:
    DO WITH FRAME {&FRAME-NAME}:
        RowObject.OtroTpoCliente:SENSITIVE = integer(SELF:SCREEN-VALUE) = 0.
    END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK vTableWin 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN initializeObject.
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI vTableWin  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE displayFieldList vTableWin 
PROCEDURE displayFieldList :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER pcFieldList  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pcFromSource AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER phDataSource AS HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER pcColValues  AS CHARACTER NO-UNDO.
    
    DEF VAR i AS INTEGER NO-UNDO.
    DEF VAR c AS CHAR NO-UNDO.
    /* Code placed here will execute PRIOR to standard behavior. */
    
    RUN SUPER( INPUT pcFieldList, INPUT pcFromSource, INPUT phDataSource, INPUT pcColValues).
    
    /* Code placed here will execute AFTER standard behavior.    */
    DO WITH FRAME {&FRAME-NAME}:
        i = LOOKUP("tpocliente",pcFieldList,",").
        c  = ENTRY(i,pcColValues,CHR(1)).
        RowObject.OtroTpoCliente:SENSITIVE = c = "0".

        i = LOOKUP("prdctoslctar",pcFieldList,",").
        c  = ENTRY(i,pcColValues,CHR(1)).
        RowObject.OtroPrdctoSlctar:SENSITIVE = c = "0".
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject vTableWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
    DO WITH FRAME {&FRAME-NAME}:
         RowObject.PrdctoSlctar:RADIO-BUTTONS = 
             "Crédito,1,CDAT,2,CDT,3,Ahorro Programado,4,Ahorro Permanente,5,Ahorro Vista,6,Otro,0". 
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRcbeDtos vTableWin 
PROCEDURE pRcbeDtos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER c AS CHAR NO-UNDO.
    DEF VAR iagncia AS INTEGER NO-UNDO.
    iagncia = integer(ENTRY(1,c,CHR(1))).
    IF NOT CAN-FIND(FIRST agencias WHERE agencias.agencia = iagncia) THEN RETURN ERROR "ERROR: Agencia " + STRING(iAgncia) + " NO Existe".
    FIND FIRST agencias NO-LOCK
        WHERE
            agencias.agencia = iagncia NO-ERROR.
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN 
            RowObject.Agencia:SCREEN-VALUE = STRING(iAgncia)
            RowObject.NombreAgencia:SCREEN-VALUE = Agencias.Nombre.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fEnviaDtos vTableWin 
FUNCTION fEnviaDtos RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEF VAR c AS CHAR NO-UNDO.    
    DEF VAR c1 AS CHAR NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        c = FILL(CHR(1),17).
        entry(1,c,chr(1)) = "AGENCIA".
        entry(2,c,chr(1)) = "CIUDAD".
        entry(3,c,chr(1)) = "CptcionClccion".
        entry(4,c,chr(1)) = "Cuota".
        entry(5,c,chr(1)) = "dstncion".
        entry(6,c,chr(1)) = "Fec_UltActualiza".
        entry(7,c,chr(1)) = "FrmaPgo".
        entry(8,c,chr(1)) = "Grntia".
        entry(9,c,chr(1)) = "Linea".
        entry(10,c,chr(1)) = "Monto".
        entry(11,c,chr(1)) = "NombreAgencia".
        entry(12,c,chr(1)) = "NombreCiudad".
        entry(13,c,chr(1)) = "OtroPrdctoSlctar".
        entry(14,c,chr(1)) = "OtroTpoCliente".
        entry(15,c,chr(1)) = "Plazo".
        entry(16,c,chr(1)) = "PrdctoSlctar".
        entry(17,c,chr(1)) = "reestrctrcion".
        entry(18,c,chr(1)) = "TpoCliente".

        c1 = FILL(CHR(1),17).
        entry(1,c1,chr(1)) = fNONull(RowObject.Agencia:SCREEN-VALUE). 
        entry(2,c1,chr(1)) = fNONull(RowObject.Ciudad:SCREEN-VALUE). 
        entry(3,c1,chr(1)) = fNONull(RowObject.CptcionClccion:SCREEN-VALUE). 
        entry(4,c1,chr(1)) = fNONull(RowObject.Cuota:SCREEN-VALUE). 
        entry(5,c1,chr(1)) = fNONull(RowObject.dstncion:SCREEN-VALUE). 
        entry(6,c1,chr(1)) = fNONull(RowObject.Fec_UltActualiza:SCREEN-VALUE). 
        entry(7,c1,chr(1)) = fNONull(RowObject.FrmaPgo:SCREEN-VALUE). 
        entry(8,c1,chr(1)) = fNONull(RowObject.Grntia:SCREEN-VALUE). 
        entry(9,c1,chr(1)) = fNONull(RowObject.Linea:SCREEN-VALUE). 
        entry(10,c1,chr(1)) = fNONull(RowObject.Monto:SCREEN-VALUE). 
        entry(11,c1,chr(1)) = fNONull(RowObject.NombreAgencia:SCREEN-VALUE). 
        entry(12,c1,chr(1)) = fNONull(RowObject.NombreCiudad:SCREEN-VALUE). 
        entry(13,c1,chr(1)) = fNONull(RowObject.OtroPrdctoSlctar:SCREEN-VALUE). 
        entry(14,c1,chr(1)) = fNONull(RowObject.OtroTpoCliente:SCREEN-VALUE). 
        entry(15,c1,chr(1)) = fNONull(RowObject.Plazo:SCREEN-VALUE). 
        entry(16,c1,chr(1)) = fNONull(RowObject.PrdctoSlctar:SCREEN-VALUE). 
        entry(17,c1,chr(1)) = fNONull(RowObject.reestrctrcion:SCREEN-VALUE). 
        entry(18,c1,chr(1)) = fNONull(RowObject.TpoCliente:SCREEN-VALUE).
    END.
    RETURN c + CHR(10) + c1.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fNONull vTableWin 
FUNCTION fNONull RETURNS CHARACTER
  (c AS CHAR /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  
  RETURN IF c = ? THEN "" ELSE c.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

