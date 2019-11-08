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

DEF VAR p_dir AS CHAR.
DEFINE VAR P_Ubi LIKE Ubicacion.Ubicacion.
DEFINE VAR P_NUbi AS CHARACTER FORMAT "X(80)".

DEF VAR cAlfbto AS CHAR NO-UNDO INITIAL " ,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z".
DEF VAR cDgtos AS CHAR NO-UNDO INITIAL "1,2,3,4,5,6,7,8,9,0".

DEF VAR cDrccion AS CHAR NO-UNDO.
DEF VAR hDrccion AS HANDLE NO-UNDO.

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
&Scoped-Define ENABLED-FIELDS RowObject.Nombre RowObject.Val_Comercial ~
RowObject.Matricula_inmobiliaria RowObject.Prenda_Hipoteca 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS RECT-306 Btn_Direccion Btn_Lugar 
&Scoped-Define DISPLAYED-FIELDS RowObject.Nombre RowObject.Dir_Bienes ~
RowObject.Lugar_Bienes RowObject.Val_Comercial ~
RowObject.Matricula_inmobiliaria RowObject.Prenda_Hipoteca 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fDgto vTableWin 
FUNCTION fDgto RETURNS LOGICAL
  (c AS CHAR /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_wgnrdordrccion AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Direccion 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     IMAGE-INSENSITIVE FILE "imagenes/arwdwn.gif":U
     LABEL "Btn_Direccion" 
     SIZE 3 BY .54.

DEFINE BUTTON Btn_Lugar 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     IMAGE-INSENSITIVE FILE "imagenes/arwdwn.gif":U
     LABEL "Btn_Lugar" 
     SIZE 3 BY .54.

DEFINE RECTANGLE RECT-306
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 67.29 BY 5.92.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.Nombre AT ROW 1.23 COL 18.29 COLON-ALIGNED WIDGET-ID 8
          LABEL "Tipo Bien"
          VIEW-AS FILL-IN 
          SIZE 42 BY .81
          BGCOLOR 15 
     RowObject.Dir_Bienes AT ROW 2.19 COL 18.29 COLON-ALIGNED WIDGET-ID 2
          LABEL "Dirección"
          VIEW-AS FILL-IN 
          SIZE 42 BY .81
          BGCOLOR 15 
     Btn_Direccion AT ROW 2.35 COL 63 WIDGET-ID 290
     RowObject.Lugar_Bienes AT ROW 3.12 COL 18.29 COLON-ALIGNED WIDGET-ID 4
          LABEL "Depar Ciu" FORMAT "X(256)"
          VIEW-AS FILL-IN 
          SIZE 42 BY .81
          BGCOLOR 15 
     Btn_Lugar AT ROW 3.23 COL 63.14 WIDGET-ID 292
     RowObject.Val_Comercial AT ROW 4 COL 18.29 COLON-ALIGNED WIDGET-ID 12
          LABEL "Valor Comercial" FORMAT ">>,>>>,>>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 26 BY .81
          BGCOLOR 15 
     RowObject.Matricula_inmobiliaria AT ROW 4.88 COL 18.29 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 26 BY .81
          BGCOLOR 15 
     RowObject.Prenda_Hipoteca AT ROW 5.77 COL 18.29 COLON-ALIGNED WIDGET-ID 10
          LABEL "Hipoteca a Favor de"
          VIEW-AS FILL-IN 
          SIZE 42 BY .81
          BGCOLOR 15 
     RECT-306 AT ROW 1 COL 1 WIDGET-ID 38
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1
         SIZE 67.29 BY 5.92
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
         HEIGHT             = 5.92
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
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.Dir_Bienes IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN RowObject.Lugar_Bienes IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN RowObject.Nombre IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Prenda_Hipoteca IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Val_Comercial IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
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

&Scoped-define SELF-NAME Btn_Direccion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Direccion vTableWin
ON CHOOSE OF Btn_Direccion IN FRAME F-Main /* Btn_Direccion */
DO:
    hDrccion = rowobject.DIR_bienes:HANDLE.
    RUN viewObject IN h_wgnrdordrccion.
    RUN CmbiarDrccion IN h_wgnrdordrccion(rowobject.DIR_bienes:SCREEN-VALUE).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Lugar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Lugar vTableWin
ON CHOOSE OF Btn_Lugar IN FRAME F-Main /* Btn_Lugar */
DO:
  
  RUN C-Ubicacion.r (OUTPUT P_Ubi,OUTPUT P_NUbi).
    
  ASSIGN RowObject.Lugar_Bienes:SCREEN-VALUE = LC(P_NUbi).
        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.Val_Comercial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Val_Comercial vTableWin
ON LEAVE OF RowObject.Val_Comercial IN FRAME F-Main /* Valor Comercial */
DO:
    IF DECIMAL(SELF:SCREEN-VALUE) < 1000000
    THEN DO:   
        MESSAGE "Valor Incorrecto" VIEW-AS ALERT-BOX INFO BUTTONS OK.
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

ON 'any-printable':U OF RowObject.Nombre
DO:
    IF NOT fchar(KEYLABEL(LASTKEY))
    THEN RETURN NO-APPLY.
    RETURN.
END.
ON 'leave':U OF RowObject.Dir_Bienes, RowObject.Lugar_Bienes, RowObject.Matricula_inmobiliaria, RowObject.Nombre, RowObject.Prenda_Hipoteca,RowObject.Lugar_Bienes
DO:
    SELF:SCREEN-VALUE = CAPS(SELF:SCREEN-VALUE).
END.
ON 'any-printable':U OF RowObject.Matricula_inmobiliaria
DO:
    IF NOT fchar(KEYLABEL(LASTKEY)) AND NOT fdgto(KEYLABEL(LASTKEY))
    THEN RETURN NO-APPLY.
    RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects vTableWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE currentPage  AS INTEGER NO-UNDO.

  ASSIGN currentPage = getCurrentPage().

  CASE currentPage: 

    WHEN 0 THEN DO:
       RUN constructObject (
             INPUT  'wgnrdordrccion.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_wgnrdordrccion ).
       /* Position in AB:  ( 4.12 , 59.00 ) */
       /* Size in AB:  ( 1.50 , 7.72 ) */

       /* Adjust the tab order of the smart objects. */
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DrccionCmbiada vTableWin 
PROCEDURE DrccionCmbiada :
/*------------------------------------------------------------------------------
  Purpose:   DrccionCmbiada  
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER c AS CHAR NO-UNDO.
    cDrccion = c.
    HIDE FRAME F_conyuge.
    RUN hideObject IN h_wgnrdordrccion.
    hDrccion:SCREEN-VALUE = c.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeDataObjects vTableWin 
PROCEDURE initializeDataObjects :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER plDeep AS LOGICAL NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER( INPUT plDeep).

  /* Code placed here will execute AFTER standard behavior.    */
  RUN hideObject IN h_wgnrdordrccion.
  SUBSCRIBE "DrccionCmbiada" IN h_wgnrdordrccion.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pEscndeDrccion vTableWin 
PROCEDURE pEscndeDrccion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    RUN hideObject IN h_wgnrdordrccion.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fDgto vTableWin 
FUNCTION fDgto RETURNS LOGICAL
  (c AS CHAR /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes: fDgto  
------------------------------------------------------------------------------*/

    RETURN CAN-DO(cdgtos,c).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

