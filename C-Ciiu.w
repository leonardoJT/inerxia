&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE OUTPUT PARAMETER P_Gru LIKE Ciiu.Grupo.
DEFINE OUTPUT PARAMETER P_Sub LIKE Ciiu.Subgrupo.
DEFINE OUTPUT PARAMETER P_Cod LIKE Ciiu.Codigo.
DEFINE OUTPUT PARAMETER P_Nom AS CHARACTER FORMAT "X(50)".

DEFINE VAR W_Ok AS LOGICAL.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Cmb_Grupos Cmb_Subgrupos Cmb_Codigos ~
BUTTON-101 
&Scoped-Define DISPLAYED-OBJECTS Cmb_Grupos Cmb_Subgrupos Cmb_Codigos 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-101 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 101" 
     SIZE 10 BY 1.88.

DEFINE VARIABLE Cmb_Codigos AS CHARACTER FORMAT "X(256)":U 
     LABEL "Codigos" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 49 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_Grupos AS CHARACTER FORMAT "X(256)":U 
     LABEL "Grupos" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 49 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_Subgrupos AS CHARACTER FORMAT "X(256)":U 
     LABEL "Subgrupos" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 49 BY 1
     BGCOLOR 15  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     Cmb_Grupos AT ROW 1.27 COL 11 COLON-ALIGNED
     Cmb_Subgrupos AT ROW 2.35 COL 11 COLON-ALIGNED
     Cmb_Codigos AT ROW 3.42 COL 11 COLON-ALIGNED
     BUTTON-101 AT ROW 4.5 COL 52
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 63 BY 5.81
         BGCOLOR 17 FONT 5.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Escoja el Codigo CIIU"
         HEIGHT             = 5.81
         WIDTH              = 63
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.14
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.14
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Escoja el Codigo CIIU */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Escoja el Codigo CIIU */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-101
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-101 wWin
ON CHOOSE OF BUTTON-101 IN FRAME fMain /* Button 101 */
DO:
  ASSIGN FRAME {&FRAME-NAME} Cmb_Grupos Cmb_Subgrupos Cmb_Codigos.
  ASSIGN P_Gru = INTEGER(SUBSTRING(Cmb_Grupos,1,2))
         P_Sub = INTEGER(SUBSTRING(Cmb_SubGrupos,1,2))
         P_Cod = INTEGER(SUBSTRING(Cmb_Codigos,1,4))
         P_Nom = SUBSTRING(Cmb_Grupos,5,15) + " - " + 
                 SUBSTRING(Cmb_SubGrupos,5,15) + " - " + 
                 SUBSTRING(Cmb_Codigos,7,20).
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


&Scoped-define SELF-NAME Cmb_Grupos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Grupos wWin
ON VALUE-CHANGED OF Cmb_Grupos IN FRAME fMain /* Grupos */
DO:
    Cmb_Subgrupos:LIST-ITEMS = "".
    FOR EACH Ciiu WHERE Ciiu.Tipo EQ 2 AND Ciiu.Grupo EQ INTEGER(SUBSTRING(Cmb_Grupos:SCREEN-VALUE,1,2)) 
             NO-LOCK BREAK BY Ciiu.Subgrupo:
        W_Ok = Cmb_Subgrupos:ADD-LAST(STRING(Ciiu.Subgrupo) + " - " + Ciiu.Descripcion).
    END.
    W_Ok = Cmb_SubGrupos:ADD-LAST("00 - No Configurados") IN FRAME {&FRAME-NAME}.
    Cmb_Subgrupos:SCREEN-VALUE = Cmb_Subgrupos:ENTRY(1).
    Cmb_Codigos:LIST-ITEMS = "".
    FOR EACH Ciiu WHERE Ciiu.Tipo     EQ 3 AND
                        Ciiu.Grupo    EQ INTEGER(SUBSTRING(Cmb_Grupos:SCREEN-VALUE,1,2)) AND
                        Ciiu.Subgrupo EQ INTEGER(SUBSTRING(Cmb_SubGrupos:SCREEN-VALUE,1,2)) NO-LOCK
                        BREAK BY Ciiu.Codigo:
        W_Ok = Cmb_Codigos:ADD-LAST(STRING(Ciiu.Codigo) + " - " + Ciiu.Descripcion).
    END.
    W_Ok = Cmb_Codigos:ADD-LAST("0000 - No Configurados").
    Cmb_Codigos:SCREEN-VALUE = Cmb_Codigos:ENTRY(1).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Subgrupos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Subgrupos wWin
ON VALUE-CHANGED OF Cmb_Subgrupos IN FRAME fMain /* Subgrupos */
DO:
    Cmb_Codigos:LIST-ITEMS = "".
    FOR EACH Ciiu WHERE Ciiu.Tipo     EQ 3 AND
                        Ciiu.Grupo    EQ INTEGER(SUBSTRING(Cmb_Grupos:SCREEN-VALUE,1,2)) AND
                        Ciiu.Subgrupo EQ INTEGER(SUBSTRING(Cmb_SubGrupos:SCREEN-VALUE,1,2)) NO-LOCK
                        BREAK BY Ciiu.Codigo:
        W_Ok = Cmb_Codigos:ADD-LAST(STRING(Ciiu.Codigo) + " - " + Ciiu.Descripcion).
    END.
    W_Ok = Cmb_Codigos:ADD-LAST("0000 - No configurados").
    Cmb_Codigos:SCREEN-VALUE = Cmb_Codigos:ENTRY(1).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
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
  DISPLAY Cmb_Grupos Cmb_Subgrupos Cmb_Codigos 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE Cmb_Grupos Cmb_Subgrupos Cmb_Codigos BUTTON-101 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
RUN SUPER.
  FOR EACH CIIU WHERE Ciiu.Tipo EQ 1 NO-LOCK BREAK BY Ciiu.Grupo:
      W_Ok = Cmb_Grupos:ADD-LAST(STRING(Ciiu.Grupo) + " - " + Ciiu.Descripcion) IN FRAME {&FRAME-NAME}.
  END.
  W_Ok = Cmb_Grupos:ADD-LAST("0 - No Configurados") IN FRAME {&FRAME-NAME}.
  Cmb_Grupos:SCREEN-VALUE = Cmb_Grupos:ENTRY(1).
  FOR EACH Ciiu WHERE Ciiu.Tipo EQ 2 AND Ciiu.Grupo EQ INTEGER(substring(Cmb_Grupos:SCREEN-VALUE,1,2)) 
           NO-LOCK BREAK BY Ciiu.Subgrupo:
      W_Ok = Cmb_Subgrupos:ADD-LAST(STRING(Ciiu.Subgrupo) + " - " + Ciiu.Descripcion).
  END.
  W_Ok = Cmb_SubGrupos:ADD-LAST("00 - No Configurados") IN FRAME {&FRAME-NAME}.
  Cmb_Subgrupos:SCREEN-VALUE = Cmb_Subgrupos:ENTRY(1).
  FOR EACH Ciiu WHERE Ciiu.Tipo     EQ 3 AND
                      Ciiu.Grupo    EQ INTEGER(SUBSTRING(Cmb_Grupos:SCREEN-VALUE,1,2)) AND
                      Ciiu.Subgrupo EQ INTEGER(SUBSTRING(Cmb_SubGrupos:SCREEN-VALUE,1,2)) NO-LOCK
                      BREAK BY Ciiu.Codigo:
      W_Ok = Cmb_Codigos:ADD-LAST(STRING(Ciiu.Codigo) + " - " + Ciiu.Descripcion).
  END.
  W_Ok = Cmb_Codigos:ADD-LAST("0000 - No Configurados") IN FRAME {&FRAME-NAME}.
  Cmb_Codigos:SCREEN-VALUE = Cmb_Codigos:ENTRY(1).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

