&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r11 GUI
/* Procedure Description
"Query Wizard"
*/
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
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

  File: _wizqry.w

  Description: Query wizard page 

  Input Parameters:
      hWizard (handle) - handle of Wizard dialog

  Output Parameters:
      <none>

  Author: Gerry Seidl 

  Created: 4/4/95 

  Modified: SLK Make wizard available to SmartDataObject

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

/* CREATE WIDGET-POOL. */

/* ***************************  Definitions  ************************** */
{ src/adm2/support/admhlp.i } /* ADM Help File Defs */

/* Parameters Definitions ---                                           */
DEFINE INPUT        PARAMETER hWizard   AS WIDGET-HANDLE NO-UNDO.

/* Shared Variable Definitions ---                                      */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE objtype    AS CHARACTER NO-UNDO.
DEFINE VARIABLE obj-recid  AS CHARACTER NO-UNDO.
DEFINE VARIABLE proc-recid AS CHARACTER NO-UNDO.
DEFINE VARIABLE hWizProc   AS HANDLE    NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS e_msg e_4GLQuery b_deftt b_Addq b_Helpq ~
RECT-3 
&Scoped-Define DISPLAYED-OBJECTS e_msg e_4GLQuery 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON b_Addq 
     LABEL "&Define Query" 
     SIZE 26 BY 1.1.

DEFINE BUTTON b_deftt 
     LABEL "Define &Temp-Tables" 
     SIZE 26 BY 1.1.

DEFINE BUTTON b_Helpq 
     LABEL "&Help on Queries" 
     SIZE 26 BY 1.1.

DEFINE VARIABLE e_4GLQuery AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 48 BY 7.62 NO-UNDO.

DEFINE VARIABLE e_msg AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 26 BY 4.76
     BGCOLOR 8  NO-UNDO.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 52 BY 9.05.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     e_msg AT ROW 1.61 COL 57 NO-LABEL
     e_4GLQuery AT ROW 2.57 COL 5 NO-LABEL
     b_deftt AT ROW 6.61 COL 57
     b_Addq AT ROW 8.04 COL 57
     b_Helpq AT ROW 9.47 COL 57
     RECT-3 AT ROW 1.61 COL 3
     "4GL Query:" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 1.85 COL 5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         THREE-D 
         AT COL 1 ROW 1
         SIZE 83.6 BY 10.42
         FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert title>"
         HEIGHT             = 10.67
         WIDTH              = 83.8
         MAX-HEIGHT         = 16.48
         MAX-WIDTH          = 107.2
         VIRTUAL-HEIGHT     = 16.48
         VIRTUAL-WIDTH      = 107.2
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
                                                                        */
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME
ASSIGN C-Win = CURRENT-WINDOW.




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   UNDERLINE                                                            */
ASSIGN 
       e_4GLQuery:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       e_msg:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Options          = ""
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert title> */
DO:
  /* These events will close the window and terminate the procedure.      */
  /* (NOTE: this will override any user-defined triggers previously       */
  /*  defined on the window.)                                             */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b_Addq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b_Addq C-Win
ON CHOOSE OF b_Addq IN FRAME DEFAULT-FRAME /* Define Query */
DO:
  DEFINE VARIABLE arg AS CHARACTER NO-UNDO.
  
  IF obj-recid = "":U THEN
  DO:
    RUN adeuib/_drwqry.p.            
    RUN adeuib/_uibinfo.p (INT(Proc-Recid), "PROCEDURE ?":U, 
         "CONTAINS QUERY RETURN CONTEXT":U, OUTPUT obj-recid).
  END.
  ELSE DO:
      ASSIGN arg = "QUERY-ONLY":U. /* Run QB on query only (no fields) */
      RUN adeuib/_uib_dlg.p (INT(obj-recid), "QUERY BUILDER":U, INPUT-OUTPUT arg).
  END. 
  
  RUN Display-Query.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b_deftt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b_deftt C-Win
ON CHOOSE OF b_deftt IN FRAME DEFAULT-FRAME /* Define Temp-Tables */
DO:
    RUN adeuib/_ttmaint.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b_Helpq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b_Helpq C-Win
ON CHOOSE OF b_Helpq IN FRAME DEFAULT-FRAME /* Help on Queries */
DO:
  RUN adecomm/_adehelp.p ("AB":U, "CONTEXT":U, {&Wiz_Queries}, ?).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

ASSIGN FRAME {&FRAME-NAME}:FRAME    = hwizard
       FRAME {&FRAME-NAME}:HIDDEN   = NO
       FRAME {&FRAME-NAME}:HEIGHT-P =  FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT-P
       FRAME {&FRAME-NAME}:WIDTH-P  =  FRAME {&FRAME-NAME}:VIRTUAL-WIDTH-P
       hWizProc                     = SOURCE-PROCEDURE
       .

/* Get context id of procedure */
RUN adeuib/_uibinfo.p (?, "PROCEDURE ?":U, "PROCEDURE":U, OUTPUT proc-recid).

/* Get procedure type (SmartQuery or SmartBrowser or SmartDataObject) */
RUN adeuib/_uibinfo.p (?, "PROCEDURE ?":U, "TYPE":U, OUTPUT objtype).

IF objtype = "SmartQuery":U THEN 
  RUN adeuib/_uibinfo.p (INT(proc-recid), "PROCEDURE ?":U, 
     "CONTAINS QUERY RETURN CONTEXT":U, OUTPUT obj-recid).
ELSE IF objtype = "SmartDataObject":U THEN
  RUN adeuib/_uibinfo.p (INT(proc-recid), "PROCEDURE ?":U, 
     "CONTAINS QUERY RETURN CONTEXT":U, OUTPUT obj-recid).
ELSE 
  RUN adeuib/_uibinfo.p (INT(proc-recid), "PROCEDURE ?":U, 
     "CONTAINS BROWSE RETURN CONTEXT":U, OUTPUT obj-recid).

ASSIGN e_msg = "You need to define the query ".
IF objtype = "SmartBrowse":U THEN
  e_msg = e_msg + "and database fields ".
ASSIGN e_msg = e_msg + "that will be used in this " + objtype
       e_msg = e_msg + IF SUBSTRING(objtype,LENGTH(objtype) - LENGTH("Object") + 1)
                                  = "Object":U THEN "." ELSE " object."
       e_msg = e_msg + CHR(10) + CHR(10) +
               "(If the query involves temp-tables, you need to define them first.)".

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO: 

  RUN ProcessPage NO-ERROR.
    
  IF ERROR-STATUS:ERROR THEN 
  DO:
    APPLY "U2" TO hWizard. /* ok to finish = false */
    RETURN NO-APPLY.
  END. 
  ELSE 
    APPLY "U1" TO hWizard.  /* ok to finish */
  RUN disable_UI.
END.


/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  RUN Display-Query.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  HIDE FRAME DEFAULT-FRAME.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Display-Query C-Win 
PROCEDURE Display-Query :
/*------------------------------------------------------------------------------
  Purpose:     Display 4GL query if any and sets button label.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE q-syntax AS CHARACTER NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
    IF obj-recid <> "":U THEN
      RUN adeuib/_uibinfo.p(INT(obj-recid), ?, "4GL-QUERY":U, OUTPUT q-syntax).
    
    IF q-syntax <> "":U AND q-syntax <> ? THEN DO:
        ASSIGN e_4GLQuery   = q-syntax
               b_Addq:LABEL = "&Modify Query".
        IF objtype = "SmartQuery":U THEN APPLY "U1":U TO hWizard. /* ok to finish */
    END.
    ELSE DO:
        ASSIGN e_4GLQuery   = "":U
               b_Addq:LABEL = "&Define Query". 
        IF objtype = "SmartQuery":U THEN APPLY "U2":U TO hWizard. /* not ok to finish */
    END.  
    DISPLAY e_4GLQuery WITH FRAME {&FRAME-NAME}.
END.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY e_msg e_4GLQuery 
      WITH FRAME DEFAULT-FRAME.
  ENABLE e_msg e_4GLQuery b_deftt b_Addq b_Helpq RECT-3 
      WITH FRAME DEFAULT-FRAME.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcessPage C-Win 
PROCEDURE ProcessPage :
/*------------------------------------------------------------------------------
  Purpose: Process the data on the page    
  Parameters:  <none>
  Notes:   called from ON CLOSE trigger in the main block.
           Which again is applied from all buttons in the Wizard.    
------------------------------------------------------------------------------*/
 IF DYNAMIC-FUNCTION ("GetLastButton":U IN hWizProc) = "NEXT":U THEN
 DO WITH FRAME {&FRAME-NAME}:    
   IF e_4GLQuery:SCREEN-VALUE = "":U THEN
   DO:
     MESSAGE 
          "You need to define a query" 
      VIEW-AS ALERT-BOX INFORMATION.
      RETURN ERROR.               
   END. 
 END.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

