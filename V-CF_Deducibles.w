&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
   DEFINE SHARED VARIABLE W_Agencia LIKE Agencias.Agencia.
   DEFINE SHARED VARIABLE W_Manija AS HANDLE.
   
   DEFINE VARIABLE W_CtaTra LIKE Cuentas.Cuenta.
   DEFINE VARIABLE W_NatTra LIKE Cuentas.Naturaleza.
   DEFINE VARIABLE W_CtrNat LIKE Cuentas.Ctr_Naturaleza.
   DEFINE VARIABLE W_Rpta     AS LOGICAL.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES Deducible
&Scoped-define FIRST-EXTERNAL-TABLE Deducible


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR Deducible.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Deducible.Nom_Deducible Deducible.Estado ~
Deducible.Cuenta Deducible.Cla_Deducible Deducible.Tip_Deducible ~
Deducible.Id_Deducible Deducible.Valor Deducible.Id_Impuesto ~
Deducible.Valor_Impuesto Deducible.Cuenta_Impuesto 
&Scoped-define ENABLED-TABLES Deducible
&Scoped-define FIRST-ENABLED-TABLE Deducible
&Scoped-Define ENABLED-OBJECTS RECT-209 RECT-210 RECT-302 
&Scoped-Define DISPLAYED-FIELDS Deducible.Cod_Deducible ~
Deducible.Nom_Deducible Deducible.Estado Deducible.Cuenta ~
Deducible.Cla_Deducible Deducible.Tip_Deducible Deducible.Id_Deducible ~
Deducible.Valor Deducible.Id_Impuesto Deducible.Valor_Impuesto ~
Deducible.Cuenta_Impuesto 
&Scoped-define DISPLAYED-TABLES Deducible
&Scoped-define FIRST-DISPLAYED-TABLE Deducible
&Scoped-Define DISPLAYED-OBJECTS W_NomCta W_NomCtaImp 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS Deducible.Cod_Deducible 
&Scoped-define ADM-ASSIGN-FIELDS Deducible.Cod_Deducible ~
Deducible.Id_Impuesto Deducible.Valor_Impuesto Deducible.Cuenta_Impuesto 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE W_NomCta AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 43 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomCtaImp AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 43.29 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-209
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 3 BY 2.69.

DEFINE RECTANGLE RECT-210
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 3 BY 2.69.

DEFINE RECTANGLE RECT-302
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 25 BY 2.96.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Deducible.Cod_Deducible AT ROW 1.27 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     Deducible.Nom_Deducible AT ROW 1.27 COL 37 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 43 BY .81
          BGCOLOR 15 
     Deducible.Estado AT ROW 2.08 COL 39 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Activo", 1,
"Inactivo", 2
          SIZE 20 BY .81
          BGCOLOR 17 
     Deducible.Cuenta AT ROW 3.42 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     W_NomCta AT ROW 3.42 COL 37 COLON-ALIGNED NO-LABEL
     Deducible.Cla_Deducible AT ROW 5.85 COL 21 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Porcentaje", 1,
"Valor", 2
          SIZE 12 BY 1.88
          BGCOLOR 17 
     Deducible.Tip_Deducible AT ROW 5.85 COL 41 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Fija", 1,
"Variable", 2
          SIZE 10 BY 1.88
          BGCOLOR 17 
     Deducible.Id_Deducible AT ROW 6.12 COL 62 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Financiado", 0,
"Descontado", 1
          SIZE 11.86 BY 2.15
     Deducible.Valor AT ROW 8.81 COL 17 COLON-ALIGNED
          LABEL "Valor o %"
          VIEW-AS FILL-IN 
          SIZE 17.14 BY .81
          BGCOLOR 15 
     Deducible.Id_Impuesto AT ROW 10.15 COL 16
          LABEL "Maneja Impuesto"
          VIEW-AS TOGGLE-BOX
          SIZE 14.57 BY .81
     Deducible.Valor_Impuesto AT ROW 11.23 COL 17 COLON-ALIGNED
          LABEL "Porcentaje"
          VIEW-AS FILL-IN 
          SIZE 19 BY .81
          BGCOLOR 15 
     Deducible.Cuenta_Impuesto AT ROW 12.31 COL 17 COLON-ALIGNED HELP
          "La Cuenta digitada debe manejar terceros."
          LABEL "Cuenta"
          VIEW-AS FILL-IN 
          SIZE 19 BY .81
          BGCOLOR 15 
     W_NomCtaImp AT ROW 12.31 COL 37 COLON-ALIGNED NO-LABEL
     "  Comportamiento del Deducible" VIEW-AS TEXT
          SIZE 23 BY .81 AT ROW 5.12 COL 57.14
          FGCOLOR 7 
     " Clase" VIEW-AS TEXT
          SIZE 6 BY 1.08 AT ROW 4.77 COL 20
          FGCOLOR 7 FONT 5
     " Tipo" VIEW-AS TEXT
          SIZE 5 BY 1.08 AT ROW 4.77 COL 40
          FGCOLOR 7 FONT 5
     RECT-209 AT ROW 5.31 COL 39
     RECT-210 AT ROW 5.31 COL 19
     RECT-302 AT ROW 5.58 COL 56
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: datos.Deducible
   Allow: Basic,DB-Fields
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
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 12.54
         WIDTH              = 83.43.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN Deducible.Cod_Deducible IN FRAME F-Main
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN Deducible.Cuenta_Impuesto IN FRAME F-Main
   2 EXP-LABEL EXP-HELP                                                 */
/* SETTINGS FOR TOGGLE-BOX Deducible.Id_Impuesto IN FRAME F-Main
   2 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN Deducible.Valor IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Deducible.Valor_Impuesto IN FRAME F-Main
   2 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN W_NomCta IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomCtaImp IN FRAME F-Main
   NO-ENABLE                                                            */
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

&Scoped-define SELF-NAME Deducible.Cla_Deducible
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Deducible.Cla_Deducible V-table-Win
ON VALUE-CHANGED OF Deducible.Cla_Deducible IN FRAME F-Main /* Clase de Deducible */
DO:
  IF Deducible.Cla_Deducible:SCREEN-VALUE EQ "1" THEN
     ASSIGN Deducible.Valor:LABEL          = "Porcentaje".
  ELSE
     ASSIGN Deducible.Valor:LABEL          = "Valor".
  APPLY "ENTRY":U TO Deducible.Valor.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Deducible.Cod_Deducible
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Deducible.Cod_Deducible V-table-Win
ON LEAVE OF Deducible.Cod_Deducible IN FRAME F-Main /* Código Deducible */
OR RETURN OF Deducible.Cod_Deducible DO:
  IF Deducible.Cod_Deducible:SCREEN-VALUE EQ "" OR Deducible.Cod_Deducible:SCREEN-VALUE EQ ? THEN    
     RETURN.
  APPLY "TAB":U TO SELF. 
  ON RETURN TAB.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Deducible.Cod_Deducible V-table-Win
ON TAB OF Deducible.Cod_Deducible IN FRAME F-Main /* Código Deducible */
OR MOUSE-SELECT-DBLCLICK OF Deducible.Cod_Deducible
OR RETURN OF Deducible.Cod_Deducible DO:
 ON RETURN RETURN.

  FIND Deducible WHERE Deducible.Cod_Deducible EQ INPUT Deducible.Cod_Deducible 
                 NO-LOCK NO-ERROR.
  IF AVAILABLE(Deducible) THEN DO:
     RUN MostrarMensaje IN W_Manija (INPUT 221,OUTPUT W_Rpta).
     APPLY "ENTRY":U TO Deducible.Cod_Deducible.
     RETURN NO-APPLY.
  END.
  ELSE
     IF TRIM(INPUT Deducible.Cod_Deducible) = "" THEN DO:
        RUN MostrarMensaje IN W_Manija (INPUT 18,OUTPUT W_Rpta).
        APPLY "ENTRY":U TO Deducible.Cod_Deducible.
        RETURN NO-APPLY.
     END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Deducible.Cuenta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Deducible.Cuenta V-table-Win
ON LEAVE OF Deducible.Cuenta IN FRAME F-Main /* Código de Cuenta */
DO:
  {Incluido\BTCANCEL.I}
  FIND Cuentas WHERE Cuentas.Cuenta EQ INPUT Deducible.Cuenta
                 AND Cuentas.Tipo   EQ 2     
               NO-LOCK NO-ERROR.
  IF NOT AVAILABLE(Cuentas) THEN DO:
     Deducible.Cuenta:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
     RUN C-Cuentas.R (OUTPUT W_CtaTra,OUTPUT W_NomCta,OUTPUT W_NatTra,OUTPUT W_CtrNat,INPUT "M").
     IF TRIM(W_CtaTra) NE "" THEN
        Deducible.Cuenta:SCREEN-VALUE IN FRAME {&FRAME-NAME} = W_CtaTra.
     ELSE DO:
         RUN MostrarMensaje IN W_Manija (INPUT 66,OUTPUT W_Rpta).
         APPLY "ENTRY":U TO Deducible.Cuenta.
         RETURN NO-APPLY.
     END.  
  END.
  ELSE
     W_NomCta = Cuentas.Nombre.
  DISPLAY W_NomCta WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Deducible.Cuenta V-table-Win
ON MOUSE-SELECT-DBLCLICK OF Deducible.Cuenta IN FRAME F-Main /* Código de Cuenta */
DO:
  RUN P-BrwCta.R (OUTPUT W_CtaTra,OUTPUT W_NomCta,OUTPUT W_NatTra,OUTPUT W_CtrNat,INPUT "M").
  IF TRIM(W_CtaTra) NE "" THEN
     ASSIGN Deducible.Cuenta:SCREEN-VALUE IN FRAME {&FRAME-NAME} = W_CtaTra
            W_NomCta:SCREEN-VALUE = W_NomCta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Deducible.Cuenta_Impuesto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Deducible.Cuenta_Impuesto V-table-Win
ON LEAVE OF Deducible.Cuenta_Impuesto IN FRAME F-Main /* Cuenta */
DO:
  /*{Incluido\BTCANCEL.I}*/
  IF Deducible.Id_Impuesto:SCREEN-VALUE EQ "Si" THEN DO:
     FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Deducible.Cuenta_Impuesto:SCREEN-VALUE
                          AND Cuentas.Tipo   EQ 2
                          AND Cuentas.Id_Nit EQ TRUE
                          AND cuentas.Estado EQ 1
                        NO-LOCK NO-ERROR.
     IF NOT AVAILABLE(Cuentas) THEN DO:
        Deducible.Cuenta_Impuesto:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
        RUN C-Cuentas.R (OUTPUT W_CtaTra, OUTPUT W_NomCta, OUTPUT W_NatTra, OUTPUT W_CtrNat,INPUT "M").
        IF TRIM(W_CtaTra) NE "" THEN DO:
           ASSIGN Deducible.Cuenta_Impuesto:SCREEN-VALUE = W_CtaTra
                  W_NomCtaImp:SCREEN-VALUE               = W_NomCta.
        END.
        ELSE DO:
           RUN MostrarMensaje IN W_Manija (INPUT 66,OUTPUT W_Rpta).
           APPLY "ENTRY":U TO Deducible.Cuenta_Impuesto.
           RETURN NO-APPLY.
        END.  
     END.
     ELSE
        W_NomCtaImp:SCREEN-VALUE = Cuentas.Nombre.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Deducible.Cuenta_Impuesto V-table-Win
ON MOUSE-SELECT-DBLCLICK OF Deducible.Cuenta_Impuesto IN FRAME F-Main /* Cuenta */
DO:
  RUN P-BRWSCT.R (OUTPUT W_CtaTra, OUTPUT W_NomCta, OUTPUT W_NatTra, OUTPUT W_CtrNat,INPUT "M","TERCEROS").
  IF TRIM(W_CtaTra) NE "" THEN
     ASSIGN Deducible.Cuenta_Impuesto:SCREEN-VALUE IN FRAME {&FRAME-NAME} = W_CtaTra
            W_NomCtaImp:SCREEN-VALUE = W_NomCta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Deducible.Id_Impuesto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Deducible.Id_Impuesto V-table-Win
ON VALUE-CHANGED OF Deducible.Id_Impuesto IN FRAME F-Main /* Maneja Impuesto */
DO:
    IF Deducible.Id_Impuesto:SCREEN-VALUE = "SI" THEN
       ASSIGN Deducible.Cuenta_Impuesto:SENSITIVE    = TRUE
              Deducible.Valor_Impuesto:SENSITIVE     = TRUE.
    ELSE
       ASSIGN Deducible.Cuenta_Impuesto:SCREEN-VALUE = "" 
              Deducible.Valor_Impuesto:SCREEN-VALUE  = "0"
              Deducible.Cuenta_Impuesto:SENSITIVE    = FALSE
              Deducible.Valor_Impuesto:SENSITIVE     = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Deducible.Nom_Deducible
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Deducible.Nom_Deducible V-table-Win
ON LEAVE OF Deducible.Nom_Deducible IN FRAME F-Main /* Nombre */
DO:
  {Incluido\BTCANCEL.I}
   IF TRIM(INPUT Deducible.Nom_Deducible) EQ "" THEN DO:
      RUN MostrarMensaje IN W_Manija (INPUT 58,OUTPUT W_Rpta).
      APPLY "ENTRY":U TO Deducible.Cod_Deducible.
      RETURN NO-APPLY.
   END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win  _ADM-ROW-AVAILABLE
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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "Deducible"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "Deducible"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record V-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN check-modified IN THIS-PROCEDURE ( INPUT 'clear':U ).
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE W_Evento AS CHAR INITIAL " Salvar ".
  
  IF Deducible.Cod_Deducible:SCREEN-VALUE IN FRAME {&FRAME-NAME} LE "0"
  OR Deducible.Cod_Deducible:SCREEN-VALUE                        EQ "?"
  OR Deducible.Nom_Deducible:SCREEN-VALUE                        LE " " THEN DO:
     RUN MostrarMensaje IN W_Manija (INPUT 204,OUTPUT W_Rpta).
     RETURN ERROR.
  END.
  IF Deducible.Cuenta:SCREEN-VALUE IN FRAME {&FRAME-NAME} LE " " 
  OR Deducible.Cuenta:SCREEN-VALUE EQ "?" THEN DO:
     RUN MostrarMensaje IN W_Manija (INPUT 267,OUTPUT W_Rpta).
     RETURN ERROR.
  END.
  IF   Deducible.Id_Impuesto:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "Si"
  AND (Deducible.Cuenta_Impuesto:SCREEN-VALUE LE " " 
       OR Deducible.Cuenta_Impuesto:SCREEN-VALUE EQ "?"
       OR DECIMAL(Deducible.Valor_Impuesto:SCREEN-VALUE) LE 0) THEN DO:
       RUN MostrarMensaje IN W_Manija (INPUT 140,OUTPUT W_Rpta).
       RETURN ERROR.
  END.
  
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
  RUN Get-Attribute IN THIS-PROCEDURE ("ADM-NEW-RECORD").
  IF RETURN-VALUE EQ "YES" THEN
     RUN P-GraLog IN W_Manija (INPUT "NOR: CREA Registro, Deducibles. Cod_Deducible: " + STRING(Deducible.Cod_Deducible)).
  ELSE
     RUN P-GraLog IN W_Manija (INPUT "NOR: SALVA Registro, Deducibles. Cod_Deducible: " + STRING(Deducible.Cod_Deducible)).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  
  RUN MostrarMensaje IN W_Manija (INPUT 270,OUTPUT W_Rpta).
  RUN P-GraLog IN W_Manija (INPUT "ADV: TRATA BORRAR Registro, Deducible. Cod_Deducible: " + STRING(Deducible.Cod_Deducible)).
  RETURN ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy V-table-Win 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN check-modified IN THIS-PROCEDURE ( INPUT 'clear':U ).
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ).
  DO WITH FRAME {&FRAME-NAME}:
     IF Deducible.Cla_Deducible EQ 1 THEN
        Deducible.Valor:LABEL = "Porcentaje".
     ELSE
        Deducible.Valor:LABEL = "Valor".
     RUN NombreCuenta IN W_Manija (INPUT Deducible.Cuenta,OUTPUT W_NomCta).
     DISPLAY W_NomCta.
     APPLY "VALUE-CHANGED":U TO Deducible.Cla_Deducible.
     APPLY "VALUE-CHANGED":U TO Deducible.Id_Impuesto.
     APPLY "LEAVE":U         TO Deducible.Cuenta_Impuesto.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-available V-table-Win 
PROCEDURE local-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN check-modified IN THIS-PROCEDURE ( INPUT 'clear':U ).
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ).

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "Deducible"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
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
      {src/adm/template/vstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

