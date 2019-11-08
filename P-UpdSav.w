&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
/* Procedure Description
"This SmartPanel sends update, add, 
copy, reset, delete, and cancel messages 
to its TABLEIO-TARGET. "
*/
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-WIn 
/*--------------------------------------------------------------------------
 ARCHIVO    : P-Updsav.w (Principal)
 DESCRIPCION: Panel de actualizacion para los programas de mantenimiento
 AUTOR      : ""
 FECHA      : 20 de Mayo de 1998.
 PROPIEDAD  : REDECOOP
 MODIFICADO: 
             24/06/1998 RCR Se habilita validacion de la prioridad de los
                            usuarios ( si se requiere inhabilitar esta
                            validacion solo basta colocar las variables que
                            estan shared, en Definitions,como new shared.)  
---------------------------------------------------------------------------*/                  
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

ON RETURN TAB.
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
&Scoped-define adm-attribute-dlg adm/support/u-paneld.w


DEFINE VARIABLE trans-commit AS LOGICAL   NO-UNDO.  
DEFINE VARIABLE panel-type   AS CHARACTER NO-UNDO INIT 'SAVE':U.
DEFINE VARIABLE add-active   AS LOGICAL   NO-UNDO INIT no.

/* DEFINE SHARED VAR W_Prioridad LIKE Usuarios.Prioridad INITIAL 5. */
/* DEFINE SHARED VAR W_Manija    AS   HANDLE.                       */
{Incluido/Variable.I}

DEFINE        VAR W_Rpta      AS   LOGICAL.

/*                                                                         */
/*               Variables definidas por REDECOOP                          */
/*                                                                         */

DEFINE VARIABLE W_EstSav AS LOGICAL.
DEFINE VARIABLE W_EstRes AS LOGICAL.
DEFINE VARIABLE W_EstAdd AS LOGICAL.
DEFINE VARIABLE W_EstDel AS LOGICAL.
DEFINE VARIABLE W_EstCan AS LOGICAL.
DEFINE VARIABLE W_EstDon AS LOGICAL.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartPanel
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS TableIO-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Panel-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn-Save Btn-Reset Btn-Add Btn-Delete ~
Btn-Cancel Btn_Done 

/* Custom List Definitions                                              */
/* Box-Rectangle,List-2,List-3,List-4,List-5,List-6                     */
&Scoped-define Box-Rectangle RECT-1 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn-Add 
     LABEL "&Ingresar" 
     SIZE 7.57 BY 1.08
     FONT 15.

DEFINE BUTTON Btn-Cancel 
     LABEL "Ca&ncelar" 
     SIZE 7.57 BY 1.08
     FONT 15.

DEFINE BUTTON Btn-Delete 
     LABEL "&Borrar" 
     SIZE 7.57 BY 1.08
     FONT 15.

DEFINE BUTTON Btn-Reset 
     LABEL "&Deshacer" 
     SIZE 7.57 BY 1.08
     FONT 15.

DEFINE BUTTON Btn-Save 
     LABEL "&Salvar" 
     SIZE 7.57 BY 1.08
     FONT 15.

DEFINE BUTTON Btn_Done AUTO-END-KEY DEFAULT 
     LABEL "S&alir" 
     SIZE 7.57 BY 1.08
     FONT 15.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 45 BY 1.77.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Panel-Frame
     Btn-Save AT ROW 1.27 COL 2 HELP
          "Permite Guardar los Cambios Realizados"
     Btn-Reset AT ROW 1.27 COL 9 HELP
          "Permite Eliminar la última modificación"
     Btn-Add AT ROW 1.27 COL 16 HELP
          "Permite Matricular un nuevo registro"
     Btn-Delete AT ROW 1.27 COL 23 HELP
          "Premite Eliminar un registro de datos"
     Btn-Cancel AT ROW 1.27 COL 30
     Btn_Done AT ROW 1.27 COL 37 HELP
          "Permite Regresar a la opción anterior"
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY NO-HELP 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartPanel
   Allow: Basic
   Frames: 1
   Add Fields to: NEITHER
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
  CREATE WINDOW C-WIn ASSIGN
         HEIGHT             = 2.04
         WIDTH              = 46.14.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-WIn 
/* ************************* Included-Libraries *********************** */

{src/adm/method/panel.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-WIn
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME Panel-Frame
   FRAME-NAME Size-to-Fit                                               */
ASSIGN 
       FRAME Panel-Frame:SCROLLABLE       = FALSE.

/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME Panel-Frame
   NO-ENABLE 1                                                          */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME Panel-Frame
/* Query rebuild information for FRAME Panel-Frame
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME Panel-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Btn-Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Add C-WIn
ON CHOOSE OF Btn-Add IN FRAME Panel-Frame /* Ingresar */
DO:
  ON RETURN TAB.
  IF W_Prioridad = 1 THEN
    DO:
      RUN MostrarMensaje IN W_Manija(INPUT 295,OUTPUT W_Rpta).
      RETURN NO-APPLY.
    END.
  ELSE
   IF W_Prioridad = 2 THEN
    DO:
      RUN MostrarMensaje IN W_Manija(INPUT 296,OUTPUT W_Rpta).
      RETURN NO-APPLY.
    END.
  ELSE
    DO:
      ASSIGN Btn_Done:SENSITIVE IN FRAME Panel-Frame = FALSE.
      add-active = yes.
      IF panel-type = 'UPDATE':U THEN 
        Btn-Save:LABEL = '&Salvar'.
      RUN notify ('add-record':U).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Add C-WIn
ON ENTRY OF Btn-Add IN FRAME Panel-Frame /* Ingresar */
DO:
  ON RETURN RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Add C-WIn
ON LEAVE OF Btn-Add IN FRAME Panel-Frame /* Ingresar */
DO:
  ON RETURN TAB.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Cancel C-WIn
ON CHOOSE OF Btn-Cancel IN FRAME Panel-Frame /* Cancelar */
DO:
  DO WITH FRAME Panel-Frame:
      ON RETURN TAB.  
      ASSIGN Btn_Done:SENSITIVE = TRUE.
      RUN notify ('cancel-record':U).
      add-active = no.
      IF panel-type = 'UPDATE':U THEN 
          Btn-Save:LABEL = '&Salvar'.            
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Cancel C-WIn
ON ENTRY OF Btn-Cancel IN FRAME Panel-Frame /* Cancelar */
DO:
  ON RETURN RETURN.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Cancel C-WIn
ON LEAVE OF Btn-Cancel IN FRAME Panel-Frame /* Cancelar */
DO:
  ON RETURN TAB.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Delete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Delete C-WIn
ON CHOOSE OF Btn-Delete IN FRAME Panel-Frame /* Borrar */
DO:
  ON RETURN TAB.
  IF W_Prioridad = 1 THEN
    DO:
      RUN MostrarMensaje IN W_Manija(INPUT 295,OUTPUT W_Rpta).
      RETURN NO-APPLY.
    END.
  ELSE
    IF W_Prioridad = 2 THEN
      DO:
        RUN MostrarMensaje IN W_Manija(INPUT 296,OUTPUT W_Rpta).
        RETURN NO-APPLY.
      END.
    ELSE
      IF W_Prioridad = 3 THEN 
        DO:
          RUN MostrarMensaje IN W_Manija(INPUT 297,OUTPUT W_Rpta).
          RETURN NO-APPLY.
        END.
     ELSE
      RUN notify ('delete-record':U).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Delete C-WIn
ON ENTRY OF Btn-Delete IN FRAME Panel-Frame /* Borrar */
DO:
  ON RETURN RETURN.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Delete C-WIn
ON LEAVE OF Btn-Delete IN FRAME Panel-Frame /* Borrar */
DO:
  ON RETURN TAB.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Reset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Reset C-WIn
ON CHOOSE OF Btn-Reset IN FRAME Panel-Frame /* Deshacer */
DO:
  ON RETURN TAB.
  RUN notify ('reset-record':U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Reset C-WIn
ON ENTRY OF Btn-Reset IN FRAME Panel-Frame /* Deshacer */
DO:
  ON RETURN RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Reset C-WIn
ON LEAVE OF Btn-Reset IN FRAME Panel-Frame /* Deshacer */
DO:
  ON RETURN TAB.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Save
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Save C-WIn
ON CHOOSE OF Btn-Save IN FRAME Panel-Frame /* Salvar */
DO:
    ON RETURN TAB.
        
    IF W_Prioridad = 1 THEN DO:
        RUN MostrarMensaje IN W_Manija(INPUT 295,OUTPUT W_Rpta).
        RETURN NO-APPLY.
    END.
    ELSE DO:
        &IF LOOKUP("Btn-Add":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
            /* If we're in a persistent add-mode then don't change any labels. Just make */
            /* a call to update the last record and then add another record.             */
            /*  RUN _SetCurs.p ("WAIT").*/
            SESSION:SET-WAIT-STATE("GENERAL").
            
        RUN get-attribute IN THIS-PROCEDURE ('AddFunction':U).
            
        IF (RETURN-VALUE = 'Multiple-Records':U) AND add-active THEN DO:
            RUN notify ('update-record':U).
                
            IF RETURN-VALUE NE "ADM-ERROR":U THEN
                RUN notify ('add-record':U).
        END.
        ELSE
        &ENDIF
        DO:
            IF panel-type = 'UPDATE':U THEN
                DO WITH FRAME Panel-Frame:
                    IF Btn-Save:LABEL = '&Update' THEN DO:
                        RUN new-state('update-begin':U).
                        
                        ASSIGN Btn-Save:LABEL = '&Save'
                               add-active = no.
                    END.
                    ELSE DO: /* Save */
                        RUN notify ('update-record':U).
                    END.
                END.
            ELSE DO: /* Normal 'Save'-style SmartPanel */
                RUN notify ('update-record':U).
                /*        Btn-Save:LABEL = '&Update'.*/
            END.
        END.
        
        ASSIGN Btn_Done:SENSITIVE IN FRAME Panel-Frame = TRUE.
        
        /*  RUN _SetCurs.p ("ARROW").*/
        SESSION:SET-WAIT-STATE("").
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Save C-WIn
ON ENTRY OF Btn-Save IN FRAME Panel-Frame /* Salvar */
DO:
  ON RETURN RETURN.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Save C-WIn
ON LEAVE OF Btn-Save IN FRAME Panel-Frame /* Salvar */
DO:
  ON RETURN TAB.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done C-WIn
ON CHOOSE OF Btn_Done IN FRAME Panel-Frame /* Salir */
DO:
  ON RETURN TAB.
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit') NO-ERROR.
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE NO-ERROR.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done C-WIn
ON ENTRY OF Btn_Done IN FRAME Panel-Frame /* Salir */
DO:
  ON RETURN RETURN.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done C-WIn
ON LEAVE OF Btn_Done IN FRAME Panel-Frame /* Salir */
DO:
  ON RETURN TAB.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-WIn 


/* ***************************  Main Block  *************************** */

  /* Set the default SmartPanel to the one that has the Commit push */
  /* button displayed (the TABLEIO-TARGETS are not enabled/disabled */
  /* automatically with this type of SmartPanel).                   */
  
  RUN set-attribute-list ("SmartPanelType=Save, 
                           Edge-Pixels=2,
                           AddFunction=One-Record":U). 
                           
  /* If the application hasn't enabled the behavior that a RETURN in a frame = GO,
     then enable the usage of the Save button as the default button. (Note that in
     8.0, the Save button was *always* the default button.) */
 
/*  IF SESSION:DATA-ENTRY-RETURN NE yes THEN 
  ASSIGN
      Btn-Save:DEFAULT IN FRAME {&FRAME-NAME} = yes
      FRAME {&FRAME-NAME}:DEFAULT-BUTTON = Btn-Save:HANDLE.*/

  SESSION:DATA-ENTRY-RETURN = TRUE.
  ON RETURN TAB.
      
  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Apply_BtnCancel C-WIn 
PROCEDURE Apply_BtnCancel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   APPLY "CHOOSE" TO Btn-Cancel IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-WIn  _DEFAULT-DISABLE
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
  HIDE FRAME Panel-Frame.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-deshabilita C-WIn 
PROCEDURE local-deshabilita :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
 ASSIGN W_EstSav = Btn-Save:SENSITIVE IN FRAME {&FRAME-NAME}
         W_EstRes = Btn-Reset:SENSITIVE
         W_EstAdd = Btn-Add:SENSITIVE
         W_EstDel = Btn-Delete:SENSITIVE
         W_EstCan = Btn-Cancel:SENSITIVE.
     /*    W_EstDon = Btn_Done:SENSITIVE.*/
         
  ASSIGN Btn-Save:SENSITIVE   = FALSE
         Btn-Reset:SENSITIVE  = FALSE
         Btn-Add:SENSITIVE    = FALSE
         Btn-Delete:SENSITIVE = FALSE
         Btn-Cancel:SENSITIVE = FALSE.
        /* Btn_Done:SENSITIVE   = FALSE.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-deshsinres C-WIn 
PROCEDURE local-deshsinres :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

ASSIGN W_EstSav = Btn-Save:SENSITIVE IN FRAME {&FRAME-NAME}
       W_EstRes = Btn-Reset:SENSITIVE
       W_EstAdd = Btn-Add:SENSITIVE
       W_EstDel = Btn-Delete:SENSITIVE
       W_EstCan = Btn-Cancel:SENSITIVE.
/*     W_EstDon = Btn_Done:SENSITIVE.*/
         
  ASSIGN Btn-Save:SENSITIVE   = FALSE
/*       Btn-Reset:SENSITIVE  = FALSE */
         Btn-Add:SENSITIVE    = FALSE
         Btn-Delete:SENSITIVE = FALSE
         Btn-Cancel:SENSITIVE = FALSE.
/*       Btn_Done:SENSITIVE   = FALSE.*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable C-WIn 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
  Purpose: The SmartPanel's buttons sensitivities are re-set to whatever
           state they were in when they were disabled. This state is de-
           termined from the variable adm-panel-state.
  Notes:       
------------------------------------------------------------------------------*/

  RUN dispatch ('enable':U).      /* Get all objects enabled to start. */
  RUN set-buttons (adm-panel-state).
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-habilita C-WIn 
PROCEDURE local-habilita :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN Btn-Save:SENSITIVE IN FRAME {&FRAME-NAME} = W_EstSav
         Btn-Reset:SENSITIVE  = W_EstRes
         Btn-Add:SENSITIVE    = W_EstAdd
         Btn-Delete:SENSITIVE = W_EstDel
         Btn-Cancel:SENSITIVE = W_EstCan.
        /* Btn_Done:SENSITIVE   = W_EstDon.*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize C-WIn 
PROCEDURE local-initialize :
/*--------------------------------------------------------------------------
  Purpose     : If the SmartPanel is type COMMIT, enable all the fields of
                the TABLEIO-TARGETS since they are defaulted to disabled.
  Notes       :
  ------------------------------------------------------------------------*/

  DEFINE VARIABLE query-position AS CHARACTER NO-UNDO.
  
  /* Insert pre-dispatch code here. */ 

  RUN dispatch IN THIS-PROCEDURE ( INPUT "adm-initialize":U ) .

  /* Insert post-dispatch code here. */

  RUN get-attribute IN THIS-PROCEDURE ('UIB-MODE':U).
  IF RETURN-VALUE <> 'DESIGN':U THEN DO:
     IF VALID-HANDLE (adm-broker-hdl) THEN DO:
       DEFINE VAR tab-target-link AS CHARACTER NO-UNDO.
       RUN get-link-handle IN adm-broker-hdl
           (INPUT THIS-PROCEDURE, 'TABLEIO-TARGET':U, OUTPUT tab-target-link).
       IF (tab-target-link EQ "":U) THEN
         adm-panel-state = 'disable-all':U.
       ELSE DO:
         RUN request-attribute IN adm-broker-hdl
            (INPUT THIS-PROCEDURE, INPUT 'TABLEIO-TARGET':U,
             INPUT 'Query-Position':U).
         query-position = RETURN-VALUE.
         IF query-position = 'no-record-available':U THEN 
           adm-panel-state = 'add-only':U.
         ELSE IF query-position = 'no-external-record-available':U THEN 
           adm-panel-state = 'disable-all':U.
         ELSE adm-panel-state = 'initial':U.
       END.
     END.
     RUN set-buttons (adm-panel-state).
  END.
 
  IF panel-type = 'SAVE':U AND /* Only enable a Save panel if there's a record */
    LOOKUP(query-position,'no-record-available,no-external-record-available':U) = 0
     THEN RUN notify ('enable-fields, TABLEIO-TARGET':U).
  /* otherwise disable in case they were already enabled during initialization*/
  ELSE RUN notify('disable-fields, TABLEIO-TARGET':U). 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-buttons C-WIn 
PROCEDURE set-buttons :
/*------------------------------------------------------------------------------
  Purpose: Sets the sensitivity of the panel's buttons depending upon what
           sort of action is occuring to the TABLEIO-TARGET(s) of the panel.
  Parameters:  Character string that denotes which action to set the button
               sensitivities.
               
               The values are: initial - the panel is in a state where no record
                                         changes are occuring; i.e. it is possible
                                         to  Update, Add, Copy, or Delete a record.
                               action-chosen - the panel is in the state where
                                               Update/Save, Add, or Copy has
                                               been pressed.
                               disable-all - the panel has all its buttons 
                                             disabled, in the case a link is
                                             deactivated.
                               add-only - for the time that there are no records
                                          in the query, and the action that can be
                                          taken is an add.
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER panel-state AS CHARACTER NO-UNDO.

DO WITH FRAME Panel-Frame:

  IF panel-state = 'disable-all':U THEN DO:

    /* All buttons are set to insensitive. This only should happen when */
    /* the link to the smartpanel is deactivated, but not destroyed.    */

&IF LOOKUP("Btn-Save":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
             Btn-Save:SENSITIVE = NO.
&ENDIF
&IF LOOKUP("Btn-Delete":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
             Btn-Delete:SENSITIVE = NO.
&ENDIF
&IF LOOKUP("Btn-Add":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
             Btn-Add:SENSITIVE = NO.
&ENDIF
&IF LOOKUP("Btn-Copy":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
             Btn-Copy:SENSITIVE = NO.
&ENDIF
&IF LOOKUP("Btn-Reset":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
             Btn-Reset:SENSITIVE = NO.
&ENDIF
&IF LOOKUP("Btn-Cancel":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
             Btn-Cancel:SENSITIVE = NO.
&ENDIF

  END. /* panel-state = 'disable-all' */
  
  ELSE IF panel-state = 'initial':U THEN DO:
  
    /* The panel is not actively changing any of its TABLEIO-TARGET(s). */

&IF LOOKUP("Btn-Save":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
             Btn-Save:SENSITIVE = YES.
             IF panel-type = 'UPDATE':U THEN
                 Btn-Save:LABEL = "&Salvar".
&ENDIF
&IF LOOKUP("Btn-Delete":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
             Btn-Delete:SENSITIVE = YES.
&ENDIF
&IF LOOKUP("Btn-Add":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
             Btn-Add:SENSITIVE = YES.
&ENDIF
&IF LOOKUP("Btn-Copy":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
             Btn-Copy:SENSITIVE = YES.
&ENDIF
&IF LOOKUP("Btn-Reset":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
       IF panel-type = 'Update':U THEN
             Btn-Reset:SENSITIVE = NO.
       ELSE
             Btn-Reset:SENSITIVE = YES.
&ENDIF
&IF LOOKUP("Btn-Cancel":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
             Btn-Cancel:SENSITIVE = NO.
             ON RETURN RETURN.
&ENDIF
      
  END. /* panel-state = 'initial' */

  ELSE IF panel-state = 'add-only':U THEN DO:

    /* All buttons are set to insensitive, except add. This only should */
    /* happen only when there are no records in the query and the only  */
    /* thing that can be done to it is add-record.                      */

&IF LOOKUP("Btn-Save":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
             Btn-Save:SENSITIVE = NO.
&ENDIF
&IF LOOKUP("Btn-Delete":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
             Btn-Delete:SENSITIVE = NO.
&ENDIF
&IF LOOKUP("Btn-Add":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
             Btn-Add:SENSITIVE = YES.
&ENDIF
&IF LOOKUP("Btn-Copy":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
             Btn-Copy:SENSITIVE = NO.
&ENDIF
&IF LOOKUP("Btn-Reset":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
             Btn-Reset:SENSITIVE = NO.
&ENDIF
&IF LOOKUP("Btn-Cancel":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
             Btn-Cancel:SENSITIVE = NO.
             ON RETURN RETURN.
&ENDIF

  END. /* panel-state = 'add-only' */
 
  ELSE DO: /* panel-state = action-chosen */ 
  
    /* The panel had one of the buttons capable of changing/adding a record */
    /* pressed. Always force the SAVE/UPDATE button to be sensitive in the  */
    /* the event that the smartpanel is disabled and later enabled prior to */
    /* the action being completed.                                          */

&IF LOOKUP("Btn-Save":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
             Btn-Save:SENSITIVE = YES.
             IF panel-type = 'UPDATE':U THEN
               Btn-Save:LABEL = "&Salvar".
&ENDIF    
&IF LOOKUP("Btn-Delete":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
             Btn-Delete:SENSITIVE = NO.
&ENDIF
&IF LOOKUP("Btn-Add":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
             Btn-Add:SENSITIVE = NO.
&ENDIF
&IF LOOKUP("Btn-Copy":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
             Btn-Copy:SENSITIVE = NO.
&ENDIF
&IF LOOKUP("Btn-Reset":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
             Btn-Reset:SENSITIVE = YES.
&ENDIF
&IF LOOKUP("Btn-Cancel":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
             Btn-Cancel:SENSITIVE = YES.
&ENDIF

  END. /* panel-state = action-chosen */

END. /* DO WITH FRAME */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-label C-WIn 
PROCEDURE set-label :
/*------------------------------------------------------------------------------
  Purpose: To change the label of the first button in the smartpanel when the
           smartpaneltype is changed from save to update, or vice versa,
           from outside the panel (e.g., from the Instance Attribute dialog. 
  Parameters: label-string - either "Save" or "Update".
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER label-string as CHARACTER NO-UNDO.

DO WITH FRAME panel-frame: 
  Btn-Save:LABEL = label-string.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed C-WIn 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/pustates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE use-smartpaneltype C-WIn 
PROCEDURE use-smartpaneltype :
/*------------------------------------------------------------------------------
  Purpose:     This procedure sets the value of the panel-type variable 
               whenever the SmartPanelType ADM attribute is set. This is
               used internally within this object to know whether the
               SmartPanel is in "Save" or "Update" mode.
  Parameters:  new attribute value.
  Notes:       This replaces code in local-initialize which set panel-type,
               but which did not always get executed early enough.
------------------------------------------------------------------------------*/
  define input parameter inval as character.
  panel-type = inval.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

