&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject NO-UNDO
       {"dact_pasivos.i"}.



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

    DEF VAR cAlfbto AS CHAR NO-UNDO INITIAL " ,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z".
    DEF VAR cDgtos AS CHAR NO-UNDO INITIAL "1,2,3,4,5,6,7,8,9,0".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "dact_pasivos.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.Nombre RowObject.Modelo ~
RowObject.Placa RowObject.Val_Comercial RowObject.Prenda_Hipoteca 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS RECT-306 
&Scoped-Define DISPLAYED-FIELDS RowObject.Nombre RowObject.Modelo ~
RowObject.Placa RowObject.Val_Comercial RowObject.Prenda_Hipoteca 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject


/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fchar vTableWin 
FUNCTION fchar RETURNS LOGICAL
  (c AS CHAR  /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-306
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 67.29 BY 4.58.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.Nombre AT ROW 1.27 COL 10.29 COLON-ALIGNED WIDGET-ID 8
          LABEL "Marca"
          VIEW-AS FILL-IN 
          SIZE 41.43 BY .81
          BGCOLOR 15 
     RowObject.Modelo AT ROW 2.35 COL 10.29 COLON-ALIGNED WIDGET-ID 6 FORMAT "9999"
          VIEW-AS FILL-IN 
          SIZE 23.43 BY .81
          BGCOLOR 15 
     RowObject.Placa AT ROW 2.27 COL 45.86 COLON-ALIGNED WIDGET-ID 10 FORMAT "N(7)"
          VIEW-AS FILL-IN 
          SIZE 16.43 BY .81 TOOLTIP "Placa"
          BGCOLOR 15 
     RowObject.Val_Comercial AT ROW 3.46 COL 18.29 COLON-ALIGNED WIDGET-ID 14
          LABEL "Valor Comercial"
          VIEW-AS FILL-IN 
          SIZE 25.43 BY .81
          BGCOLOR 15 
     RowObject.Prenda_Hipoteca AT ROW 4.5 COL 18.29 COLON-ALIGNED WIDGET-ID 12
          LABEL "Prenda a Favor de"
          VIEW-AS FILL-IN 
          SIZE 46 BY .81
          BGCOLOR 15 
     RECT-306 AT ROW 1 COL 1 WIDGET-ID 38
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1
         SIZE 67.29 BY 4.58
         BGCOLOR 17 FONT 5 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dact_pasivos.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" NO-UNDO  
      ADDITIONAL-FIELDS:
          {dact_pasivos.i}
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
         HEIGHT             = 4.58
         WIDTH              = 67.29.
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
   NOT-VISIBLE FRAME-NAME Custom                                        */
ASSIGN 
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.Modelo IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN RowObject.Nombre IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Placa IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN RowObject.Prenda_Hipoteca IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Val_Comercial IN FRAME F-Main
   EXP-LABEL                                                            */
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

&Scoped-define SELF-NAME RowObject.Modelo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Modelo vTableWin
ON LEAVE OF RowObject.Modelo IN FRAME F-Main /* Modelo */
DO:
    IF LENGTH(SELF:SCREEN-VALUE) <> 4
    OR INTEGER(SELF:SCREEN-VALUE) > YEAR(TODAY) + 1
    OR INTEGER(SELF:SCREEN-VALUE) < 1940
    THEN DO: 
        MESSAGE "Modelo Incorrecto"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.Placa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Placa vTableWin
ON LEAVE OF RowObject.Placa IN FRAME F-Main /* Placa */
DO:
  /*  IF length(trim(ENTRY(1,SELF:SCREEN-VALUE,"-"))) < 3 OR
    length(trim(ENTRY(2,SELF:SCREEN-VALUE,"-"))) < 3
    THEN DO:
        MESSAGE "Placa Incorrecta"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    SELF:SCREEN-VALUE = caps(" " + trim(ENTRY(1,SELF:SCREEN-VALUE,"-"))  + trim(ENTRY(2,SELF:SCREEN-VALUE,"-"))).
    */
    SELF:SCREEN-VALUE = CAPS(trim(SELF:SCREEN-VALUE)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.Val_Comercial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Val_Comercial vTableWin
ON LEAVE OF RowObject.Val_Comercial IN FRAME F-Main /* Valor Comercial */
DO:
    IF decimal(SELF:SCREEN-VALUE) < 1000000
    THEN DO:
        MESSAGE "Valor Incorrecto"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
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

ON 'any-printable':U OF RowObject.Prenda_Hipoteca
DO:
    IF NOT fchar(KEYLABEL(LASTKEY))
    THEN RETURN NO-APPLY.
    RETURN.
END.
ON 'leave':U OF RowObject.Nombre, RowObject.Prenda_Hipoteca
DO:
    SELF:SCREEN-VALUE = CAPS(SELF:SCREEN-VALUE).
    RETURN.
END.

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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fchar vTableWin 
FUNCTION fchar RETURNS LOGICAL
  (c AS CHAR  /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  fchar
    Notes:  
------------------------------------------------------------------------------*/
    RETURN CAN-DO(cAlfbto,LC(c)).
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

