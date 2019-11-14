&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-Inf_Sipla
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Inf_Sipla 
/*-----------------------------------------------------------------------
  File: 
  Description: 
  Author: 
  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/


CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

ON RETURN TAB.
    
DEFI TEMP-TABLE TSipla 
     FIELD agen LIKE Agencias.Agencia
     FIELD Nit  LIKE Ahorros.Nit
     FIELD Cta  LIKE Ahorros.Cue_ahorro
     FIELD Nom  AS CHAR FORM "X(35)"
     FIELD Fec  AS DATE FORM "99/99/9999"
     FIELD EC   AS CHAR FORM "X(8)"
     FIELD tip  AS INTEG FORM "9"
     FIELD Vlr  LIKE Taquilla.Val_efect.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Sipla

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS W_Fini W_FFin Tope_Dia Tope_Mes Btn_ejec 
&Scoped-Define DISPLAYED-OBJECTS W_Fini W_FFin Tope_Dia Tope_Mes 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Inf_Sipla AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_ejec 
     LABEL "EJECUTAR" 
     SIZE 15 BY 1.12.

DEFINE VARIABLE Tope_Dia AS DECIMAL FORMAT "->>>>,>>>,>>9":U INITIAL 0 
     LABEL "Tope X Día" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE Tope_Mes AS DECIMAL FORMAT "->>>>,>>>,>>9":U INITIAL 0 
     LABEL "Tope X Mes" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE W_FFin AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Final" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE W_Fini AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Inicial" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Sipla
     W_Fini AT ROW 1.81 COL 18 COLON-ALIGNED
     W_FFin AT ROW 1.81 COL 49 COLON-ALIGNED
     Tope_Dia AT ROW 4.23 COL 61 COLON-ALIGNED
     Tope_Mes AT ROW 5.85 COL 61 COLON-ALIGNED
     Btn_ejec AT ROW 7.46 COL 32
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 8.65
         BGCOLOR 17 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Inf_Sipla ASSIGN
         HIDDEN             = YES
         TITLE              = "Informe Sipla, Programa W-Inf_Sipla.W"
         HEIGHT             = 8.65
         WIDTH              = 80
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Inf_Sipla
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F_Sipla
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Inf_Sipla)
THEN W-Inf_Sipla:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Sipla
/* Query rebuild information for FRAME F_Sipla
     _Query            is NOT OPENED
*/  /* FRAME F_Sipla */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Inf_Sipla
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Inf_Sipla W-Inf_Sipla
ON END-ERROR OF W-Inf_Sipla /* Informe Sipla, Programa W-Inf_Sipla.W */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Inf_Sipla W-Inf_Sipla
ON WINDOW-CLOSE OF W-Inf_Sipla /* Informe Sipla, Programa W-Inf_Sipla.W */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_ejec
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_ejec W-Inf_Sipla
ON CHOOSE OF Btn_ejec IN FRAME F_Sipla /* EJECUTAR */
DO:
  DEFI VAR VrDia LIKE Ahorros.Sdo_dispon INIT 0.
  DEFI VAR VrMes LIKE Ahorros.Sdo_dispon INIT 0.

  MESSAGE "El informe-archivo se genera en C:\InfRed\InfSipla 1 y 2.Txt."
      VIEW-AS ALERT-BOX INFO BUTTONS OK.

  SESSION:SET-WAIT-STATE("General").

  OUTPUT TO C:\INFO_utrahuilca\InfSipla1.Txt.
  
  FOR EACH Taquilla WHERE Taquilla.Fec_Transac           GE W_FIni
                      AND Taquilla.Fec_Transac           LE W_FFin
                      AND SUBSTRING(Taquilla.Cuenta,1,4) NE "1904" 
                      AND SUBSTRING(Taquilla.Cuenta,1,4) NE "2705" NO-LOCK
                           BREAK BY Taquilla.Nit BY Taquilla.Fec_Transac:
      ASSIGN VrDia = VrDia + Taquilla.Val_Efectivo + Val_Cheque
             VrMes = VrMes + Taquilla.Val_Efectivo + Val_Cheque.

      IF LAST-OF(Taquilla.Fec_Transac) THEN DO:
         IF VrDia GE Tope_Dia THEN DO:
            FIND FIRST Ahorros WHERE Ahorros.Nit        EQ Taquilla.Nit
                                 AND Ahorros.Cod_Ahorro EQ 5
                                 AND Sdo_dispon         GT 0 NO-LOCK NO-ERROR.
            FIND FIRST Clientes WHERE Clientes.Nit EQ Taquilla.Nit NO-LOCK NO-ERROR.
            DISPLAY Ahorros.Agencia WHEN AVAIL Ahorros
                    Taquilla.Nit
                    TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2) +    
                                       " " + TRIM(Clientes.Nombre) FORM "X(35)"
                    Taquilla.Fec_Transac
                    VrDia
                    "Tope Día"
                WITH DOWN WIDTH 120 FRAME f1 NO-BOX NO-LABELS USE-TEXT STREAM-IO.
                                            
            FIND FIRST TSipla WHERE TSipla.Nit EQ Taquilla.Nit NO-ERROR.
            IF NOT AVAIL(TSipla) THEN
               CREATE TSipla.
            ASSIGN TSipla.Nit = Taquilla.Nit.    
         END.            
         ASSIGN VrDia = 0.
      END.

      IF LAST-OF(Taquilla.Nit) THEN DO:
         IF VrMes GE Tope_Mes THEN DO:
            FIND FIRST Ahorros WHERE Ahorros.Nit        EQ Taquilla.Nit
                                 AND Ahorros.Cod_Ahorro EQ 5
                                 AND Sdo_dispon         GT 0 NO-LOCK NO-ERROR.
            FIND FIRST Clientes WHERE Clientes.Nit EQ Taquilla.Nit NO-LOCK NO-ERROR.     
            DISPLAY Ahorros.Agencia WHEN AVAIL Ahorros
                    Taquilla.Nit
                    TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2) +    
                                       " " + TRIM(Clientes.Nombre) FORM "X(35)"
                    Taquilla.Fec_Transac
                    VrMes
                    "Tope Mes"
                WITH DOWN WIDTH 120 FRAME f2 NO-BOX NO-LABELS USE-TEXT STREAM-IO.
                                            
            FIND FIRST TSipla WHERE TSipla.Nit EQ Taquilla.Nit NO-ERROR.
            IF NOT AVAIL(TSipla) THEN
               CREATE TSipla.
            ASSIGN TSipla.Nit = Taquilla.Nit.                                               
         END.            

         ASSIGN VrMes = 0.
      END.

  END.
  OUTPUT CLOSE.
    
  SESSION:SET-WAIT-STATE("").  
    
  RUN Detalle.  

  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Tope_Dia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Tope_Dia W-Inf_Sipla
ON LEAVE OF Tope_Dia IN FRAME F_Sipla /* Tope X Día */
DO:
  ASSIGN Tope_Dia.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Tope_Mes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Tope_Mes W-Inf_Sipla
ON LEAVE OF Tope_Mes IN FRAME F_Sipla /* Tope X Mes */
DO:
  ASSIGN Tope_Mes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_FFin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_FFin W-Inf_Sipla
ON LEAVE OF W_FFin IN FRAME F_Sipla /* Fecha Final */
DO:
  ASSIGN W_FFin.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Fini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Fini W-Inf_Sipla
ON LEAVE OF W_Fini IN FRAME F_Sipla /* Fecha Inicial */
DO:
  ASSIGN W_Fini.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Inf_Sipla 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Detalle W-Inf_Sipla 
PROCEDURE Detalle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  SESSION:SET-WAIT-STATE("General").

  OUTPUT TO C:\INFO_utrahuilca\InfSipla2.Txt.  
        
  FOR EACH TSipla BY TSipla.Nit:
      FOR EACH Taquilla WHERE Taquilla.Nit                   EQ TSipla.Nit
                          AND Taquilla.Fec_Transac           GE W_FIni
                          AND Taquilla.Fec_Transac           LE W_FFin
                          AND SUBSTRING(Taquilla.Cuenta,1,4) NE "1904" 
                          AND SUBSTRING(Taquilla.Cuenta,1,4) NE "2705" NO-LOCK
                                 BY Taquilla.Fec_Transac:  
          FIND FIRST Ahorros WHERE Ahorros.Nit          EQ Taquilla.Nit
                                 AND Ahorros.Cod_Ahorro EQ 5
                                 AND Sdo_dispon         GT 0 NO-LOCK NO-ERROR.
          FIND FIRST Clientes WHERE Clientes.Nit       EQ Taquilla.Nit      NO-LOCK NO-ERROR.
          FIND FIRST Operacio WHERE Operacion.Cod_Oper EQ Taquilla.Cod_oper NO-LOCK NO-ERROR.
          
          DISPLAY Taquilla.Cuenta          LABEL "Cuenta"
                  TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2) +    
                                       " " + TRIM(Clientes.Nombre) FORM "X(35)"
                                           LABEL "Nombre del Asociado"
                  Taquilla.Nit             LABEL "Ced./Nit"
                  Taquilla.Fec_Transac     LABEL "Fec-Transac"
                  Taquilla.Val_Efect       LABEL "Vr.Efectivo"
                  Taquilla.Val_Cheque      LABEL "Vr.Cheque"
                  Operacion.Tipo_Oper      LABEL "C/R"  WHEN AVAIL(Operacion)
                  Taquilla.Agencia         LABEL "Ag."
                WITH DOWN WIDTH 150 FRAME fdet NO-BOX NO-LABELS USE-TEXT STREAM-IO.   
      END.
  END.
  OUTPUT CLOSE.
    
  SESSION:SET-WAIT-STATE("").  
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Inf_Sipla  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Inf_Sipla)
  THEN DELETE WIDGET W-Inf_Sipla.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Inf_Sipla  _DEFAULT-ENABLE
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
  DISPLAY W_Fini W_FFin Tope_Dia Tope_Mes 
      WITH FRAME F_Sipla IN WINDOW W-Inf_Sipla.
  ENABLE W_Fini W_FFin Tope_Dia Tope_Mes Btn_ejec 
      WITH FRAME F_Sipla IN WINDOW W-Inf_Sipla.
  {&OPEN-BROWSERS-IN-QUERY-F_Sipla}
  VIEW W-Inf_Sipla.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

