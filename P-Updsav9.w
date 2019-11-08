&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
/* Procedure Description
"This SmartPanel sends update, add, 
copy, reset, delete, and cancel messages 
to its TABLEIO-TARGET. "
*/
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-WIn 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation ("PSC"),       *
* 14 Oak Park, Bedford, MA 01730, and other contributors as listed   *
* below.  All Rights Reserved.                                       *
*                                                                    *
* The Initial Developer of the Original Code is PSC.  The Original   *
* Code is Progress IDE code released to open source December 1, 2000.*
*                                                                    *
* The contents of this file are subject to the Possenet Public       *
* License Version 1.0 (the "License"); you may not use this file     *
* except in compliance with the License.  A copy of the License is   *
* available as of the date of this notice at                         *
* http://www.possenet.org/license.html                               *
*                                                                    *
* Software distributed under the License is distributed on an "AS IS"*
* basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. You*
* should refer to the License for the specific language governing    *
* rights and limitations under the License.                          *
*                                                                    *
* Contributors:                                                      *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: pupdsav.w

        Version 9 Update Panel.
        This is the standard version of the database
        update SmartPanel. It uses the TABLEIO link
        to communicate with SmartViewers and Smart-
        Browsers.

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Modified    : February 17, 1999
     
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

  &GLOB ADM-Panel-Type Save

/* This is the procedure to execute to set InstanceProperties at design time. */
&IF DEFINED (ADM-PROPERTY-DLG) = 0 &THEN
  &SCOP ADM-PROPERTY-DLG adm2/support/u-paneld.w
&ENDIF

  &IF "{&xcInstanceProperties}":U NE "":U &THEN
    &GLOB xcInstanceProperties {&xcInstanceProperties},
  &ENDIF
  &GLOB xcInstanceProperties {&xcInstanceProperties}AddFunction

  DEFINE SHARED VAR W_Prioridad LIKE Usuarios.Prioridad INITIAL 5.
DEFINE SHARED VAR W_Manija    AS   HANDLE.
DEFINE        VAR W_Rpta      AS   LOGICAL.

/*                                                                         */
/*               Variables definidas por REDECOOP                          */
/*                                                                         */
DEFINE VARIABLE trans-commit AS LOGICAL   NO-UNDO.  
DEFINE VARIABLE panel-type   AS CHARACTER NO-UNDO INIT 'SAVE':U.
DEFINE VARIABLE add-active   AS LOGICAL   NO-UNDO INIT no.
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

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Panel-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn-Save Btn-Reset Btn-Add Btn-Delete ~
Btn-Cancel Btn_Done 

/* Custom List Definitions                                              */
/* Box-Rectangle,List-2,List-3,List-4,List-5,List-6                     */
&Scoped-define Box-Rectangle RECT-1 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD actionHideRule C-WIn 
FUNCTION actionHideRule RETURNS CHARACTER
  ( pcAction AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getAddFunction C-WIn 
FUNCTION getAddFunction RETURNS CHARACTER
  ( )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTableIOTarget C-WIn 
FUNCTION getTableIOTarget RETURNS CHARACTER
  (   )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTableIOTargetEvents C-WIn 
FUNCTION getTableIOTargetEvents RETURNS CHARACTER
  (   )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setAddFunction C-WIn 
FUNCTION setAddFunction RETURNS LOGICAL
  ( pcAddFunction AS CHARACTER  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setTableIOTarget C-WIn 
FUNCTION setTableIOTarget RETURNS LOGICAL
  ( pcObject AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setTableIOTargetEvents C-WIn 
FUNCTION setTableIOTargetEvents RETURNS LOGICAL
  ( pcEvents AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setUpdatingRecord C-WIn 
FUNCTION setUpdatingRecord RETURNS LOGICAL
  ( plUpdating AS LOGICAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn-Add 
     LABEL "&Ingresar" 
     SIZE 9 BY 1.35
     FONT 4.

DEFINE BUTTON Btn-Cancel 
     LABEL "Ca&ncelar" 
     SIZE 9 BY 1.35
     FONT 4.

DEFINE BUTTON Btn-Delete 
     LABEL "&Borrar" 
     SIZE 9 BY 1.35
     FONT 4.

DEFINE BUTTON Btn-Reset 
     LABEL "&Deshacer" 
     SIZE 9 BY 1.35
     FONT 4.

DEFINE BUTTON Btn-Save 
     LABEL "&Salvar" 
     SIZE 9 BY 1.35
     FONT 4.

DEFINE BUTTON Btn_Done AUTO-END-KEY DEFAULT 
     LABEL "S&alir" 
     SIZE 9 BY 1.35
     BGCOLOR 8 FONT 4.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 65.57 BY 2.15.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Panel-Frame
     Btn-Save AT ROW 1.35 COL 2
     Btn-Reset AT ROW 1.35 COL 11
     Btn-Add AT ROW 1.35 COL 20
     Btn-Delete AT ROW 1.35 COL 29
     Btn-Cancel AT ROW 1.35 COL 38
     Btn_Done AT ROW 1.35 COL 47
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY NO-HELP 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 .


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
         HEIGHT             = 3.65
         WIDTH              = 67.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-WIn 
/* ************************* Included-Libraries *********************** */

{src/adm2/panel.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-WIn
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME Panel-Frame
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME Panel-Frame:SCROLLABLE       = FALSE
       FRAME Panel-Frame:HIDDEN           = TRUE.

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
      PUBLISH ('add-record':U).
    END.
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
      PUBLISH ('cancel-record':U).
      add-active = no.
      IF panel-type = 'UPDATE':U THEN 
          Btn-Save:LABEL = '&Salvar'.            
   END.
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
      PUBLISH ('delete-record':U).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Reset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Reset C-WIn
ON CHOOSE OF Btn-Reset IN FRAME Panel-Frame /* Deshacer */
DO:
  PUBLISH 'resetRecord':U.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Save
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Save C-WIn
ON CHOOSE OF Btn-Save IN FRAME Panel-Frame /* Salvar */
DO:
  ON RETURN TAB.
  IF W_Prioridad = 1 THEN
   DO:
     RUN MostrarMensaje IN W_Manija(INPUT 295,OUTPUT W_Rpta).
     RETURN NO-APPLY.
   END.
ELSE
DO:
&IF LOOKUP("Btn-Add":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
  /* If we're in a persistent add-mode then don't change any labels. Just make */
  /* a call to update the last record and then add another record.             */
/*  RUN _SetCurs.p ("WAIT").*/
  SESSION:SET-WAIT-STATE("GENERAL").
  RUN get-attribute IN THIS-PROCEDURE ('AddFunction':U).
  IF (RETURN-VALUE = 'Multiple-Records':U) AND add-active THEN 
  DO:
     PUBLISH ('update-record':U).
     IF RETURN-VALUE NE "ADM-ERROR":U THEN
         PUBLISH ('add-record':U). 
  END.
  ELSE 
&ENDIF
  DO:
     IF panel-type = 'UPDATE':U THEN
     DO WITH FRAME Panel-Frame:
        IF Btn-Save:LABEL = '&Update' THEN 
        DO:
           RUN new-state('update-begin':U).
           ASSIGN Btn-Save:LABEL = '&Save' 
                  add-active = no.
        END.
        ELSE 
        DO: /* Save */
           PUBLISH ('update-record':U).
        END.                              
     END.
     ELSE 
     DO: /* Normal 'Save'-style SmartPanel */
        PUBLISH ('update-record':U).
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


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done C-WIn
ON CHOOSE OF Btn_Done IN FRAME Panel-Frame /* Salir */
DO:
  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
    &ELSE
      RUN exitObject.
    &ENDIF
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-WIn 


/* ***************************  Main Block  *************************** */

/* If the application hasn't enabled the behavior that a RETURN in a frame = GO,
     then enable the usage of the Save button as the default button. (Note that in
     8.0, the Save button was *always* the default button.) */
  IF SESSION:DATA-ENTRY-RETURN NE yes THEN 
  ASSIGN
      Btn-Save:DEFAULT IN FRAME {&FRAME-NAME} = yes
      FRAME {&FRAME-NAME}:DEFAULT-BUTTON = Btn-Save:HANDLE.
  
  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN initializeObject.        
  &ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addLink C-WIn 
PROCEDURE addLink :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER phSource AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER pcLink   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER phTarget AS HANDLE NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER( INPUT phSource, INPUT pcLink, INPUT phTarget).

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Apply_BtnCancel C-WIn 
PROCEDURE Apply_BtnCancel :
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject C-WIn 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cUIBMode     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cTableioType AS CHARACTER NO-UNDO.

  RUN loadPanel.  
  RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-Deshabilita C-WIn 
PROCEDURE Local-Deshabilita :
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-Deshsinres C-WIn 
PROCEDURE Local-Deshsinres :
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-Enable C-WIn 
PROCEDURE Local-Enable :
/* RUN dispatch ('enable':U).      /* Get all objects enabled to start. */
  RUN set-buttons (adm-panel-state).                                       */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-Habilita C-WIn 
PROCEDURE Local-Habilita :
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Label C-WIn 
PROCEDURE Set-Label :
DEFINE INPUT PARAMETER label-string as CHARACTER NO-UNDO.

DO WITH FRAME panel-frame: 
  Btn-Save:LABEL = label-string.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Smartpaneltype C-WIn 
PROCEDURE Set-Smartpaneltype :
define input parameter inval as character.
  panel-type = inval.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setButtons C-WIn 
PROCEDURE setButtons :
/*------------------------------------------------------------------------------
  Purpose: Sets the sensitivity of the panel's buttons depending upon what
           sort of action is occuring to the TABLEIO-TARGET(s) of the panel.
  Parameters:  Character string that denotes which action to set the button
               sensitivities.
               
               initial - the panel is in a state where no record
                         changes are occuring; i.e. it is possible
                         to  Update, Add, Copy, or Delete a record.
               disable-all  - the panel has all its buttons disabled, 
                              in the case a link is deactivated.
               add-only     - for the time that there are no records
                              in the query, and the action that can be
                              taken is an add.
              
               delete-only   - for the case where no fields are enabled
               update-only   - update only usually if changes diallows 
                               add/delete           
                                                             
      NB !  No longer used by the panel class only supported for backwards
            compatibility:                              
               action-chosen - the panel is in the state where
                               Update/Save, Add, or Copy has
                               been pressed. 
                               for backwards compatibility.  
      Replaced by the three following states:                                                       
               update        - Save,reset 
               modal-update  - Save,cancel
               modal-update-modified - Save,cancel,reset
                              
             if 'action-chosen' is needed, override setPanelState to 
             call super with 'action-chosen' instead of the approproiate or
             for all three for that matter. 
  Notes:  This procedure has been deprecated and is no longer part of the normal
          resetting of the action's states.          
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER pcPanelState AS CHARACTER NO-UNDO.

  DEFINE VARIABLE cPanelType AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lEnabled   AS LOGICAL   NO-UNDO.
 
  {get ObjectEnabled lEnabled}.   
  /* If the object has been disabled, don't reset any buttons. */
  IF NOT lEnabled THEN RETURN.
  
  DO WITH FRAME Panel-Frame:
    {get PanelType cPanelType}.
    IF pcPanelState = 'disable-all':U THEN 
    DO:
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
    END. /* pcPanelState = 'disable-all' */  
    ELSE IF pcPanelState = 'initial':U THEN 
    DO:
  
     /* The panel is not actively changing any of its TABLEIO-TARGET(s). */
            &IF LOOKUP("Btn-Save":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
     IF cPanelType = 'UPDATE':U THEN
     DO:
       Btn-Save:LABEL = "&Update".
       Btn-Save:SENSITIVE = YES.
     END.
     ELSE
       Btn-Save:SENSITIVE = NO.
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
       Btn-Reset:SENSITIVE = NO.
            &ENDIF
            &IF LOOKUP("Btn-Cancel":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
       Btn-Cancel:SENSITIVE = NO.
            &ENDIF
    END. /* pcPanelState = 'initial' */
    ELSE IF CAN-DO('add-only,delete-only,update-only':U,pcPanelState) THEN 
    DO:
      /* All buttons are set to insensitive, except add. This only should */
      /* happen only when there are no records in the query and the only  */
      /* thing that can be done to it is add-record.                      */
         &IF LOOKUP("Btn-Save":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
      IF cPanelType = 'UPDATE':U THEN
         Btn-Save:LABEL = "&Update":U.
      
      Btn-Save:SENSITIVE = Btn-Save:LABEL = "&Update" AND 
                           pcPanelState = 'update-only':U.
         &ENDIF
         &IF LOOKUP("Btn-Delete":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
      Btn-Delete:SENSITIVE = pcPanelState BEGINS 'delete-':U.
         &ENDIF
         &IF LOOKUP("Btn-Add":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
      Btn-Add:SENSITIVE =  pcPanelState BEGINS 'add-':U.
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
    END. /* pcPanelState = 'delete-only' */ 

    /* 'action-chosen' is no longer supported, but are kept for backwards compatibility
       since we don't know how it is used it enables more than the other  */ 
    ELSE IF pcPanelState = "action-chosen":U 
         OR pcPanelState = "update":U    
         OR pcPanelState BEGINS "modal-update":U  THEN
    DO: /* pcPanelState = action-chosen */ 
  
      /* The panel had one of the buttons capable of changing/adding a record */
      /* pressed. Always force the SAVE/UPDATE button to be sensitive in the  */
      /* the event that the smartpanel is disabled and later enabled prior to */
      /* the action being completed.                                          */
          &IF LOOKUP("Btn-Save":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
      Btn-Save:SENSITIVE = YES.
      IF cPanelType = 'UPDATE':U THEN
        Btn-Save:LABEL = "&Save".
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
      Btn-Reset:SENSITIVE =  pcPanelState = 'action-chosen':U 
                          OR pcPanelState = 'update':U 
                          OR pcPanelState = 'modal-update-modified':U.
          &ENDIF
          &IF LOOKUP("Btn-Cancel":U, "{&ENABLED-OBJECTS}":U," ":U) NE 0 &THEN
      Btn-Cancel:SENSITIVE = pcPanelState = 'action-chosen':U 
                          OR pcPanelState = 'modal-update':U
                          OR pcPanelState = 'modal-update-modified':U.
          &ENDIF
    END. /* pcPanelState = action-chosen */
  END. /* DO WITH FRAME */
  RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION actionHideRule C-WIn 
FUNCTION actionHideRule RETURNS CHARACTER
  ( pcAction AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Override hide rule for update/save in order to support that only 
           one of them is visible simultaneously.  
    Notes: This is NOT stored in the Repository or updated in initAction since 
           the rule only applies to the update panel and not to toolbars
------------------------------------------------------------------------------*/

 CASE pcAction:
   WHEN 'Save':U THEN
     RETURN 'ObjectMode=view and saveSource=no':U.
   WHEN 'Update':U THEN
     RETURN 'ObjectMode=modify,update or saveSource':U.
   OTHERWISE 
     RETURN SUPER(pcAction).

 END CASE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getAddFunction C-WIn 
FUNCTION getAddFunction RETURNS CHARACTER
  ( ) :
/*------------------------------------------------------------------------------
  Purpose:  Returns whether this Panel (if an Update Panel) is in 
            Multiple-Add mode.
   Params:  <none>
------------------------------------------------------------------------------*/

  DEFINE VARIABLE cAdd AS CHARACTER NO-UNDO.
  {get AddFunction cAdd}.
  RETURN cAdd.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTableIOTarget C-WIn 
FUNCTION getTableIOTarget RETURNS CHARACTER
  (   ) :
/*------------------------------------------------------------------------------
  Purpose:  Returns in CHARACTER form a list of the handles of the object's 
            TableIO Targets
   Params:  <none>
------------------------------------------------------------------------------*/

  DEFINE VARIABLE cTarget AS CHARACTER NO-UNDO.
  {get TableIOTarget cTarget}.
  RETURN cTarget.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTableIOTargetEvents C-WIn 
FUNCTION getTableIOTargetEvents RETURNS CHARACTER
  (   ) :
/*------------------------------------------------------------------------------
  Purpose:  Returns a comma-separated list of the events this object wants 
            to subscribe to in its TableIO Target
   Params:  <none>
------------------------------------------------------------------------------*/

  DEFINE VARIABLE cEvents AS CHARACTER NO-UNDO.
  {get TableIOTargetEvents cEvents}.
  RETURN cEvents.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setAddFunction C-WIn 
FUNCTION setAddFunction RETURNS LOGICAL
  ( pcAddFunction AS CHARACTER  ) :
/*------------------------------------------------------------------------------
  Purpose:  Sets whether the (update) panel is in Multiple-Record mode or 
            One-Record mode for Add operations.
   Params:  pcAddFunction AS CHARACTER -- 'Multiple-Records' or 'One-Record'
------------------------------------------------------------------------------*/
  IF pcAddFunction NE 'Multiple-Records':U AND pcAddFunction NE 'One-Record':U 
    THEN RETURN FALSE.
  ELSE DO:
    {set AddFunction pcAddFunction}.
    RETURN TRUE.
  END.
  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setTableIOTarget C-WIn 
FUNCTION setTableIOTarget RETURNS LOGICAL
  ( pcObject AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  Sets the TableIOTarget link value.
   Params:  pcObject AS CHARACTER -- handle or handles of object(s) which
              should be made TableIOTargets of the current object.1
    Notes:  Because this value can be a list, it should be changed using
              modifyListProperty
------------------------------------------------------------------------------*/

  {set TableIOTarget pcObject}.
  RETURN TRUE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setTableIOTargetEvents C-WIn 
FUNCTION setTableIOTargetEvents RETURNS LOGICAL
  ( pcEvents AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  Sets the list of events to subscribe to in the TableIOTarget.
   Params:  pcEvents AS CHARACTER -- CHARACTER string form of the event names.
    Notes:  Because the value can be a list, it should be changed using
              modifyListProperty
------------------------------------------------------------------------------*/

  {set TableIOTargetEvents pcEvents}.
  RETURN TRUE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setUpdatingRecord C-WIn 
FUNCTION setUpdatingRecord RETURNS LOGICAL
  ( plUpdating AS LOGICAL ) :
/*------------------------------------------------------------------------------
  Purpose:  Sets a flag indicating that an update is in Progress
    Notes:  NOTE: Not yet used in V9.
------------------------------------------------------------------------------*/

  {set UpdatingRecord plUpdating}.
  RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

