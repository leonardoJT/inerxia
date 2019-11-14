&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME

&Scoped-define WINDOW-NAME W-Ret_Asoc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Ret_Asoc 

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

ON RETURN TAB.

{Incluido/Variable.I "SHARED"}
{Incluido/VARCON.I   "SHARED"}

DEFINE VAR W_NomApe AS CHARACTER.
DEFINE VAR W_Age AS INTEGER.
DEFINE VAR W_Ok AS LOGICAL INITIAL FALSE.
DEFINE VAR W_CauRet AS INTEGER.
DEFINE VAR W_Cta AS CHARACTER.
DEFINE VAR W_SiContab AS LOGICAL INITIAL FALSE.
DEFINE VAR W_Observ AS CHARACTER INITIAL " ".
DEFINE VAR W_Dscto AS DECIMAL.

DEFINE TEMP-TABLE Pdctos
    FIELD Age AS INTEGER

       /* oakley */

        FIELD Tip  AS CHAR FORMAT "X(8)"
        FIELD Nom  AS CHAR FORMAT "X(25)"
        FIELD Pto  LIKE Ahorros.Cod_Ahorro
        FIELD Fec  LIKE Ahorros.Fec_Apert
        FIELD Cta  LIKE Ahorros.Cue_Ahorro
        FIELD Sdo  LIKE Creditos.Sdo_Capital INIT 0
        FIELD Est  LIKE Creditos.Estado
        FIELD Err  AS LOG INIT FALSE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Ret
&Scoped-define BROWSE-NAME Br_Pdctos

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Pdctos

/* Definitions for BROWSE Br_Pdctos                                     */
&Scoped-define FIELDS-IN-QUERY-Br_Pdctos Pdctos.AGE Pdctos.Tip Pdctos.Nom Pdctos.Pto Pdctos.Sdo Pdctos.Fec Pdctos.Est   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Pdctos   
&Scoped-define SELF-NAME Br_Pdctos
&Scoped-define QUERY-STRING-Br_Pdctos FOR EACH Pdctos NO-LOCK
&Scoped-define OPEN-QUERY-Br_Pdctos OPEN QUERY Br_Pdctos FOR EACH Pdctos NO-LOCK.
&Scoped-define TABLES-IN-QUERY-Br_Pdctos Pdctos
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Pdctos Pdctos


/* Definitions for FRAME F_Ret                                          */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-314 W_NitUsu BUTTON-5 Cmb_CauRet ~
Cmb_Ctas Btn_Proc W_CtaIng Btn_Done Br_Pdctos 
&Scoped-Define DISPLAYED-OBJECTS W_NitUsu W_NomUsu Cmb_CauRet Cmb_Ctas ~
W_CtaIng NomCta Msaje 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-2 Msaje 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Ret_Asoc AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Done DEFAULT 
     LABEL "&Salir" 
     SIZE 10.43 BY 1.42
     BGCOLOR 8 .

DEFINE BUTTON Btn_Proc 
     LABEL "&Retirar" 
     SIZE 10.43 BY 1.46 TOOLTIP "Procesa el Retiro".

DEFINE BUTTON BUTTON-5 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 5" 
     SIZE 10.43 BY 1.62.

DEFINE VARIABLE Cmb_CauRet AS CHARACTER FORMAT "X(50)":U 
     LABEL "Causal del Retiro" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 42.29 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE Cmb_Ctas AS CHARACTER FORMAT "X(50)":U 
     LABEL "Cta.Contrapartida" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 42.29 BY 1 TOOLTIP "Seleccione la Cuenta Contrapartida para el traslado"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE Msaje AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 57.43 BY .85
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomCta AS CHARACTER FORMAT "X(35)":U 
     VIEW-AS FILL-IN 
     SIZE 42.29 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CtaIng AS CHARACTER FORMAT "X(14)":U INITIAL "41909010" 
     LABEL "Cuenta-Ingresos" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Cuenta contable por retiro antes de 3 meses"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_NitUsu AS CHARACTER FORMAT "X(12)":U 
     LABEL "C.C/Nit del Cliente" 
     VIEW-AS FILL-IN 
     SIZE 13.86 BY .81 TOOLTIP "C.C / Nit del Cliente"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_NomUsu AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 45.29 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-314
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 15.14 BY 6.15.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Br_Pdctos FOR 
      Pdctos SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Br_Pdctos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Pdctos W-Ret_Asoc _FREEFORM
  QUERY Br_Pdctos DISPLAY
      Pdctos.AGE        COLUMN-LABEL "Agen"   
 Pdctos.Tip        COLUMN-LABEL "Producto"
 Pdctos.Nom        COLUMN-LABEL "Descripciòn"
 Pdctos.Pto        COLUMN-LABEL "Pdcto"
 Pdctos.Sdo        COLUMN-LABEL "Saldo del Pdcto"
 Pdctos.Fec        COLUMN-LABEL "Fec-Apert"
 Pdctos.Est        COLUMN-LABEL "Est"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 82.72 BY 6.27
         BGCOLOR 15 FGCOLOR 18 FONT 5
         TITLE BGCOLOR 15 FGCOLOR 18 "Productos del Cliente" ROW-HEIGHT-CHARS .5 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Ret
     W_NitUsu AT ROW 1.73 COL 19.29 COLON-ALIGNED
     W_NomUsu AT ROW 1.73 COL 33.57 COLON-ALIGNED NO-LABEL
     BUTTON-5 AT ROW 2.88 COL 68
     Cmb_CauRet AT ROW 2.96 COL 19.14 COLON-ALIGNED
     Cmb_Ctas AT ROW 4.31 COL 19.14 COLON-ALIGNED
     Btn_Proc AT ROW 5.19 COL 67.86
     W_CtaIng AT ROW 5.85 COL 19.14 COLON-ALIGNED
     NomCta AT ROW 6.92 COL 19.14 COLON-ALIGNED NO-LABEL
     Btn_Done AT ROW 7.08 COL 67.86 HELP
          "Sale del proceso de Depreciación y Ajustes"
     Msaje AT ROW 8.19 COL 4 COLON-ALIGNED NO-LABEL
     Br_Pdctos AT ROW 9.23 COL 2.57
     "<--- Contrapartida Contable" VIEW-AS TEXT
          SIZE 27.72 BY .62 AT ROW 6.04 COL 35.57
          BGCOLOR 18 FGCOLOR 15 
     RECT-314 AT ROW 2.81 COL 65.72
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 85.86 BY 14.88
         BGCOLOR 17 FGCOLOR 0 FONT 5.


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
  CREATE WINDOW W-Ret_Asoc ASSIGN
         HIDDEN             = YES
         TITLE              = "Retiro de Asociados, Programa W-Ret_Asoc.W"
         HEIGHT             = 14.88
         WIDTH              = 85.86
         MAX-HEIGHT         = 26.69
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 26.69
         VIRTUAL-WIDTH      = 146.29
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
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
/* SETTINGS FOR WINDOW W-Ret_Asoc
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F_Ret
   FRAME-NAME                                                           */
/* BROWSE-TAB Br_Pdctos Msaje F_Ret */
ASSIGN 
       Br_Pdctos:SEPARATOR-FGCOLOR IN FRAME F_Ret      = 18.

/* SETTINGS FOR FILL-IN Msaje IN FRAME F_Ret
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN NomCta IN FRAME F_Ret
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomUsu IN FRAME F_Ret
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Ret_Asoc)
THEN W-Ret_Asoc:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_Pdctos
/* Query rebuild information for BROWSE Br_Pdctos
     _START_FREEFORM
OPEN QUERY Br_Pdctos FOR EACH Pdctos NO-LOCK.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE Br_Pdctos */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Ret
/* Query rebuild information for FRAME F_Ret
     _Query            is NOT OPENED
*/  /* FRAME F_Ret */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Ret_Asoc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Ret_Asoc W-Ret_Asoc
ON END-ERROR OF W-Ret_Asoc /* Retiro de Asociados, Programa W-Ret_Asoc.W */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Ret_Asoc W-Ret_Asoc
ON WINDOW-CLOSE OF W-Ret_Asoc /* Retiro de Asociados, Programa W-Ret_Asoc.W */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done W-Ret_Asoc
ON CHOOSE OF Btn_Done IN FRAME F_Ret /* Salir */
DO:
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Proc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Proc W-Ret_Asoc
ON CHOOSE OF Btn_Proc IN FRAME F_Ret /* Retirar */
DO:
    DEFINE VAR Listado AS CHAR FORM "X(40)".
    DEFINE VAR W_Rpta AS LOGICAL.

    SESSION:SET-WAIT-STATE("GENERAL").

    ASSIGN Msaje:SCREEN-VALUE = "      Espere un momento por favor, Procesando Retiro...".

    APPLY "LEAVE" TO W_NitUsu.

    IF Clientes.Estado EQ 2 THEN DO:
        MESSAGE "El cliente YA ESTÁ Retirado..."
            VIEW-AS ALERT-BOX ERROR.
        
        RETURN.
    END.

    FIND FIRST Pdctos WHERE Pdctos.Sdo NE 0 NO-ERROR.
    IF AVAIL(Pdctos) THEN
        APPLY "VALUE-CHANGED" TO Cmb_Ctas.

    APPLY "VALUE-CHANGED" TO Cmb_CauRet.

    IF NOT W_Ok THEN DO:
        MESSAGE "La  Causal  de  Retiro es : " SUBSTRING(Cmb_CauRet:SCREEN-VALUE,7,40) SKIP
                "La Cta-Contrapartida es : " SUBSTRING(Cmb_Ctas:SCREEN-VALUE,1,14) SKIP
                "Està Segura (o) de Efectuar el Retiro...?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirmar Retiro" UPDATE W_RptaRet AS LOGICAL.

        IF NOT W_RptaRet THEN
            RETURN.

        SESSION:SET-WAIT-STATE("GENERAL").

        RUN Proceso NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            ASSIGN Msaje:SCREEN-VALUE = "      NO se Procesò el Retiro...Revise por favor".

            SESSION:SET-WAIT-STATE("").

            RETURN.
        END.
        ELSE DO:
            ASSIGN Msaje:SCREEN-VALUE = "        El Proceso de Retiro terminò Exitosamente..."
                   Listado = W_PathSpl + W_Usuario + "RetAsoc.Lst".

            {Incluido\Imprimir.I "listado"}
            APPLY "ENTRY" TO W_NitUsu IN FRAME F_Ret.
        END.

        SESSION:SET-WAIT-STATE("").
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 W-Ret_Asoc
ON CHOOSE OF BUTTON-5 IN FRAME F_Ret /* Button 5 */
DO:
  RUN W-InfDia.R.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_CauRet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_CauRet W-Ret_Asoc
ON VALUE-CHANGED OF Cmb_CauRet IN FRAME F_Ret /* Causal del Retiro */
DO:
  ASSIGN W_CauRet = INTEG(SUBSTRING(Cmb_CauRet:SCREEN-VALUE,1,5)).

  IF W_CauRet LE 0 OR W_CauRet EQ ? THEN DO:
     MESSAGE "Debe seleccionar una causal de Retiro." VIEW-AS ALERT-BOX ERROR.
        
     W_Ok = TRUE.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Ctas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Ctas W-Ret_Asoc
ON VALUE-CHANGED OF Cmb_Ctas IN FRAME F_Ret /* Cta.Contrapartida */
DO:
  ASSIGN W_Cta = SUBSTRING(Cmb_Ctas:SCREEN-VALUE,1,14).

  IF W_Cta LE "0" OR W_Cta EQ ? THEN DO:
     MESSAGE "Debe seleccionar una Cuenta Contable para el Traslado." VIEW-AS ALERT-BOX ERROR.
    
     W_Ok = TRUE.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CtaIng
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CtaIng W-Ret_Asoc
ON LEAVE OF W_CtaIng IN FRAME F_Ret /* Cuenta-Ingresos */
DO:
  DEFI VAR W_Natur  LIKE Cuentas.Naturaleza.
  DEFI VAR W_CtrNat LIKE Cuentas.Ctr_Naturaleza.

  ASSIGN W_CtaIng.

  FIND Cuentas WHERE Cuentas.Cuenta EQ W_CtaIng
                 AND Cuentas.Tipo   EQ 2
                 AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
  IF NOT AVAIL(Cuentas) THEN DO:
     ASSIGN W-Ret_Asoc:SENSITIVE = FALSE.

     RUN C-Cuentas.r (OUTPUT W_CtaIng, OUTPUT NomCta, OUTPUT W_Natur, 
                      OUTPUT W_CtrNat, INPUT "M").

     ASSIGN W-Ret_Asoc:SENSITIVE = TRUE.
  END.

  ASSIGN W_CtaIng:SCREEN-VALUE = " "
         NomCta:SCREEN-VALUE   = " ".

  FIND Cuentas WHERE Cuentas.Cuenta EQ W_CtaIng
                 AND Cuentas.Tipo   EQ 2
                 AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
  IF AVAIL(Cuentas) THEN 
     ASSIGN W_CtaIng:SCREEN-VALUE = W_CtaIng
            NomCta:SCREEN-VALUE   = Cuentas.Nombre.
  ELSE ASSIGN W_CtaIng:SCREEN-VALUE = " "
              W_CtaIng
              NomCta:SCREEN-VALUE   = " ".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_NitUsu
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_NitUsu W-Ret_Asoc
ON LEAVE OF W_NitUsu IN FRAME F_Ret /* C.C/Nit del Cliente */
DO:
  DEFI VAR W_SdoContab LIKE Mov_Contable.Db INIT 0.
  DEFI VAR W_SdoTot    LIKE Mov_Contable.Db INIT 0.
  DEFI VAR W_SdoDeb    LIKE Sal_Cuenta.sal_Inicial.
  DEFI VAR W_Cuenta    AS INTEG FORM "99999999".

  ASSIGN W_NitUsu
         W_NomUsu:SCREEN-VALUE = ""
         W_Ok                  = FALSE
         Btn_Proc:SENSITIVE    = TRUE
         Msaje:SCREEN-VALUE    = ""
         W_SiContab            =  FALSE.
          
  FIND FIRST Clientes WHERE Clientes.Nit EQ W_NitUsu NO-LOCK NO-ERROR.
  IF NOT AVAIL(Clientes) THEN DO:
     ASSIGN FRAME F_Ret:SENSITIVE = FALSE.

     RUN C-Clientes.R (INPUT  1,W_Agencia,
                       OUTPUT W_NitUsu, OUTPUT W_NomUsu, OUTPUT W_NomApe, OUTPUT W_Age).

     ASSIGN FRAME F_Ret:SENSITIVE = TRUE.

     FIND FIRST Clientes WHERE Clientes.Nit EQ W_NitUsu NO-LOCK NO-ERROR.
     IF AVAIL(Clientes) THEN
        ASSIGN W_NitUsu:SCREEN-VALUE = W_NitUsu
               W_NomUsu:SCREEN-VALUE = TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2) + 
                                       " " + TRIM(Clientes.Nombre).
     ELSE
        MESSAGE "El Nit debe existir en Clientes" VIEW-AS ALERT-BOX.        
  END.
  ELSE ASSIGN W_NomUsu:SCREEN-VALUE = TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2) + 
                                      " " + TRIM(Clientes.Nombre).

  IF AVAIL(Clientes) THEN DO:
     CLOSE QUERY Br_Pdctos.

     IF Clientes.Tipo_Actividad EQ "Jubilado" THEN DO:
        FOR EACH Relaciones WHERE Relaciones.Nit_Relacion EQ W_NitUsu
                              AND Relaciones.Estado       EQ 1
                              AND Relaciones.Cod_Relac    EQ 11 NO-LOCK:
            FIND FIRST Creditos WHERE Creditos.Nit    EQ Relaciones.Nit
                                  AND creditos.num_credito EQ decimal(relaciones.cuenta)
                                  AND Creditos.Estado EQ 2 AND creditos.tip_credito < 7 NO-LOCK NO-ERROR.
            IF AVAIL(Creditos) THEN DO:
               MESSAGE "Asociado Jubilado, Es Codeudor de Credito Vigente,"
                       "                   No se permite el Retiro..."
                   VIEW-AS ALERT-BOX ERROR.            
               ASSIGN W_Ok               = TRUE
                      Btn_Proc:SENSITIVE = FALSE.
            END.
        END.
     END.

     FIND FIRST Ahorros WHERE Ahorros.Nit         EQ W_NitUsu
                          AND Ahorros.Agencia     NE W_agencia
                          AND Ahorros.Tip_Ahorro  EQ 4
                          AND (Ahorros.Sdo_Dispon NE 0 OR Ahorros.Sdo_Canje NE 0) NO-LOCK NO-ERROR.
     IF AVAIL(Ahorros) THEN DO:
        MESSAGE "El Cliente tiene Aportes en Otra Agencia" SKIP
                "                 No se permite el Retiro..." VIEW-AS ALERT-BOX ERROR.            
        ASSIGN W_Ok               = TRUE
               Btn_Proc:SENSITIVE = FALSE.
     END.

     FIND FIRST Ahorros WHERE Ahorros.Nit       EQ W_NitUsu
                          AND Ahorros.Sdo_Canje NE 0 NO-LOCK NO-ERROR.
     IF AVAIL(Ahorros) THEN DO:
        MESSAGE "El Cliente tiene Captaciones con SALDO-CANJE." SKIP
                "                 No se permite el Retiro..." VIEW-AS ALERT-BOX ERROR.            
        ASSIGN W_Ok               = TRUE
               Btn_Proc:SENSITIVE = FALSE.
     END.
                                  
     RUN LlenaPdctos.
        
     FIND FIRST Pdctos WHERE Pdctos.Err NO-ERROR.
     IF AVAIL(Pdctos) THEN DO:
        MESSAGE "El Cliente tiene Obligaciones Pendientes, O Ctas-Ahorro con Fec-Vencimiento." SKIP
                "                 No se permite el Retiro..." VIEW-AS ALERT-BOX ERROR.
            
        ASSIGN W_Ok               = TRUE
               Btn_Proc:SENSITIVE = FALSE.
     END.

     OPEN QUERY Br_Pdctos FOR EACH Pdctos NO-LOCK.   

     /*SE DESHABILITA LA LIMITACION DE CODUDOR 17/01/2004 ALEX MONCADA*/   
     /*FOR EACH Relaciones WHERE Relaciones.Nit_Relacion EQ W_NitUsu
                           AND Relaciones.Cod_Relacion EQ 11
                           AND Relaciones.Clase_Produc EQ 2
                           AND Relaciones.Estado       EQ 1 NO-LOCK:
         FIND FIRST Creditos WHERE Creditos.Nit         EQ Relaciones.Nit
                               AND Creditos.Cod_Credito EQ Relaciones.Cod_Producto
                               AND Creditos.Num_Credito EQ INTEG(Relaciones.Cuenta)
                               AND Creditos.Sdo_Capital NE 0 NO-LOCK NO-ERROR.
         IF AVAIL(Creditos) THEN DO:
            MESSAGE "El Cliente es AVALISTA de Obligaciones Pendientes." SKIP
                    "              No se permite el Retiro..." VIEW-AS ALERT-BOX ERROR.
                
            ASSIGN W_Ok               = TRUE
                   Btn_Proc:SENSITIVE = FALSE.    
         END.
     END.*/
     /*FIN CAMBIO*/

/*     DO W_Cuenta = 16050000 TO 16059999 BY 1: INHABILITADA POR EDILBERTO MARIÑO MOYA 27 SEP 2007*/

        FOR EACH cuentas fields(cuenta) NO-LOCK WHERE cuentas.cuenta BEGINS "1605":
            RUN HallarSdoTercero IN W_Manija 
                (INPUT W_NitUsu, INPUT 1, 999,  
                INPUT 0, 999, cuentas.cuenta /*STRING(W_Cuenta) */, YEAR(W_Fecha), MONTH(W_Fecha),              
                OUTPUT W_SdoDeb, OUTPUT W_SdoDeb, OUTPUT W_SdoContab). 
                W_SdoTot = W_SdoTot + W_SdoContab.
        END.


     IF W_SdoTot GT 0 THEN DO:
        MESSAGE "El Cliente tiene Ctas.X Pagar(PUC--->1605) Pendientes en la Entidad" SKIP
                    "              No se permite el Retiro..." VIEW-AS ALERT-BOX ERROR.                
        ASSIGN W_Ok               = TRUE  
               Btn_Proc:SENSITIVE = FALSE.       
     END.

     FIND FIRST Ahorros WHERE Ahorros.Nit      EQ W_NitUsu  
                        AND Ahorros.Cod_Ahorro EQ 5
                        AND Ahorros.Sdo_Dispon GT 0 NO-LOCK NO-ERROR.
     IF AVAIL(Ahorros) AND (W_Fecha - Ahorros.Fec_Apertura) LT 0 THEN DO:
        ASSIGN W_CtaIng.

        FIND Cuentas WHERE Cuentas.Cuenta EQ W_CtaIng
                       AND Cuentas.Tipo   EQ 2
                       AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
        IF NOT AVAIL(Cuentas) THEN DO:
           MESSAGE "El Cliente tiene Menos de 3 Meses de Asociado." SKIP
                   "Debe Digitar Cuenta-Ingresos válida para el descuento." SKIP
                    "              No se permite el Retiro..." VIEW-AS ALERT-BOX ERROR.                
           ASSIGN W_Ok               = TRUE  
                  Btn_Proc:SENSITIVE = FALSE.       
        END.
     END.
  END.
  ELSE W_Ok = TRUE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br_Pdctos
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Ret_Asoc 


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

  FOR EACH Varios WHERE Varios.Tipo   EQ 5
                    AND Varios.Estado EQ 1 NO-LOCK:
      Cmb_CauRet:ADD-LAST(STRING(Varios.Codigo,"99999") + "-" + STRING(Varios.Descrip,"X(40)")).
  END.
    
  FOR EACH Cuentas WHERE Cuentas.Id_Renuncia 
                     AND Cuentas.Tipo   EQ 2
                     AND Cuentas.Estado EQ 1 NO-LOCK:
      Cmb_Ctas:ADD-LAST(STRING(Cuentas.Cuenta,"X(14)") + "-" + STRING(Cuentas.Nombre,"X(35)")).
      Cmb_Ctas:SCREEN-VALUE = STRING(Cuentas.Cuenta,"X(14)") + "-" + STRING(Cuentas.Nombre,"X(35)").
  END.
  
  APPLY "Entry" TO W_NitUsu.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Contable W-Ret_Asoc 
PROCEDURE Contable :
/*------------------------------------------------------------------------------
  Purpose:     
  ------------------------------------------------------------------------------*/
  DEFINE VARIABLE ndia AS INTEGER.
  
  CREATE Mov_Contable.
  ASSIGN Mov_Contable.Agencia       = Ahorros.Agencia
        Mov_Contable.Cuenta         = CortoLargo.Cta_AsoAd           
        Mov_Contable.Nit            = W_NitUsu                        
        Mov_Contable.Fec_Contable   = W_Fecha                         
        Mov_Contable.Comentario     = "Traslado X Retiro"            
        Mov_Contable.Usuario        = W_Usuario                       
        Mov_Contable.Cen_Costos     = W_Cencosgral                    
        Mov_Contable.Destino        = W_Agencia                       
        Mov_Contable.Comprobante    = Comprobantes.Comprobante        
        Mov_Contable.Num_Documento  = Comprobantes.Secuencia          
        Mov_Contable.Fec_Grabacion  = TODAY                           
        Mov_Contable.Hora           = TIME                            
        Mov_Contable.Estacion       = W_Estacion                      
        Mov_Contable.Db             = (Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje). 

  IF (Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje) LE 515000 THEN ndia = 30.
  IF (Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje) gt 515000 AND 
     (Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje) le 2575000 THEN ndia = 60.
  IF (Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje) gt 2575000 AND 
     (Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje) le 10300000 THEN ndia = 90.
  IF (Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje) gt 10300000 THEN ndia = 120.


  CREATE Mov_Contable.                                            
  ASSIGN Mov_Contable.Agencia      = Ahorros.Agencia                   
       Mov_Contable.Cuenta         = W_Cta         
       Mov_Contable.Nit            = W_NitUsu
       Mov_Contable.Fec_Contable   = W_Fecha                           
       Mov_Contable.Comentario     = "Traslado X Retiro"               
       Mov_Contable.Usuario        = W_Usuario                         
       Mov_Contable.Cen_Costos     = W_Cencosgral                      
       Mov_Contable.Destino        = W_Agencia                         
       Mov_Contable.Comprobante    = Comprobantes.Comprobante          
       Mov_Contable.Num_Documento  = Comprobantes.Secuencia            
       Mov_Contable.Fec_Grabacion  = TODAY                             
       Mov_Contable.Hora           = TIME                              
       Mov_Contable.Estacion       = W_Estacion                        
       Mov_Contable.Cr             = (Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje)
       Mov_contable.det_plazo      = Ndia.

  FIND FIRST Operacion WHERE Operacion.Tipo_Prod   EQ 1 
                         AND Operacion.Tipo_Operac EQ 2 
                         AND Operacion.Estado      EQ 1 NO-LOCK NO-ERROR.   
  CREATE Mov_Ahorros.
  ASSIGN Mov_Ahorros.Agencia        = Ahorros.Agencia                              
         Mov_Ahorros.Age_Destino    = Ahorros.Agencia                          
         Mov_Ahorros.Age_Fuente     = W_Agencia                                
         Mov_Ahorros.Cod_Ahorro     = Ahorros.Cod_Ahorro                   
         Mov_Ahorros.Cue_Ahorros    = Ahorros.Cue_Ahorro                       
         Mov_Ahorros.Fecha          = W_Fecha                                    
         Mov_Ahorros.Hora           = TIME                                     
         Mov_Ahorros.Nit            = W_NitUsu                              
         Mov_Ahorros.Num_Documento  = STRING(Comprobantes.Secuencia)           
         Mov_Ahorros.Sdo_Disponible = 0                       
         Mov_Ahorros.Usuario        = W_Usuario                                
         Mov_Ahorros.Val_Efectivo   = (Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje)
         Mov_Ahorros.Cod_Operacion  = 040101018
         Mov_Ahorros.Descrip        = "Retiro de Asociado"
         Mov_Ahorros.Cpte           = Comprobantes.Comprobante.

  /*ASSIGN Mov_Ahorros.Cod_Operacion  = Operacion.Cod_Operac WHEN AVAIL(Operacion).*/
                                                                
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Contable_3Meses W-Ret_Asoc 
PROCEDURE Contable_3Meses :

  FIND FIRST CortoLargo WHERE CortoLargo.Agencia        EQ Ahorros.Agencia 
                          AND CortoLargo.Clase_Producto EQ 1                                           
                          AND CortoLargo.Cod_Producto   EQ Ahorros.Cod_Ahorro NO-LOCK NO-ERROR.
  IF AVAIL(CortoLargo) THEN DO:                                                                
     FIND Cuentas WHERE Cuentas.Cuenta EQ CortoLargo.Cta_AsoAd                                 
                    AND Cuentas.Estado EQ 1                                                    
                    AND Cuentas.Tipo   EQ 2 NO-LOCK NO-ERROR.                                  
     IF NOT AVAIL(Cuentas) THEN DO:                                                            
        MESSAGE "La Cuenta del Pdcto de Ahorros : " Pro_Ahorros.Cod_Ahorro ", de la Agencia: " 
                CortoLargo.Agencia SKIP                                                        
                "En CortoLargo.Cta_AsoAd no existe...Rectifique por favor"                     
                 VIEW-AS ALERT-BOX ERROR.                                                      
        RETURN ERROR.                                                                          
     END.                                                                                      
  END.                                                                                         
  ELSE DO:                                                                                     
     MESSAGE "Debe Existir CortoLargo con el Pdcto de ahorro: " Ahorros.Cod_Ahorro SKIP        
             "                             Proceso Rechazado..."                               
             VIEW-AS ALERT-BOX ERROR.                                                          
     RETURN ERROR.                                                                             
  END.                                                                                         
  
  CREATE Mov_Contable.                                            
  ASSIGN Mov_Contable.Agencia      = Ahorros.Agencia                   
       Mov_Contable.Cuenta         = W_CtaIng         
       Mov_Contable.Nit            = W_NitUsu
       Mov_Contable.Fec_Contable   = W_Fecha                           
       Mov_Contable.Comentario     = "Aprov.X Retiro 3 Meses"               
       Mov_Contable.Usuario        = W_Usuario                         
       Mov_Contable.Cen_Costos     = W_Cencosgral                      
       Mov_Contable.Destino        = W_Agencia                         
       Mov_Contable.Comprobante    = Comprobantes.Comprobante          
       Mov_Contable.Num_Documento  = Comprobantes.Secuencia            
       Mov_Contable.Fec_Grabacion  = TODAY                             
       Mov_Contable.Hora           = TIME                              
       Mov_Contable.Estacion       = W_Estacion                        
       Mov_Contable.Cr             = W_Dscto.

  CREATE Mov_Contable.
  ASSIGN Mov_Contable.Agencia       = Ahorros.Agencia
        Mov_Contable.Cuenta         = CortoLargo.Cta_AsoAd           
        Mov_Contable.Nit            = W_NitUsu                        
        Mov_Contable.Fec_Contable   = W_Fecha                         
        Mov_Contable.Comentario     = "Aprov.X Retiro 3 Meses"            
        Mov_Contable.Usuario        = W_Usuario                       
        Mov_Contable.Cen_Costos     = W_Cencosgral                    
        Mov_Contable.Destino        = W_Agencia                       
        Mov_Contable.Comprobante    = Comprobantes.Comprobante        
        Mov_Contable.Num_Documento  = Comprobantes.Secuencia          
        Mov_Contable.Fec_Grabacion  = TODAY                           
        Mov_Contable.Hora           = TIME                            
        Mov_Contable.Estacion       = W_Estacion                      
        Mov_Contable.Db             = W_Dscto.

  CREATE Mov_Ahorros.
  ASSIGN Mov_Ahorros.Agencia        = Ahorros.Agencia                              
         Mov_Ahorros.Age_Destino    = W_Agencia                          
         Mov_Ahorros.Age_Fuente     = W_Agencia                                
         Mov_Ahorros.Cod_Ahorro     = Ahorros.Cod_Ahorro                   
         Mov_Ahorros.Cue_Ahorros    = Ahorros.Cue_Ahorro                       
         Mov_Ahorros.Fecha          = W_Fecha                                    
         Mov_Ahorros.Hora           = TIME                                     
         Mov_Ahorros.Nit            = W_NitUsu                              
         Mov_Ahorros.Num_Documento  = STRING(Comprobantes.Secuencia)           
         Mov_Ahorros.Usuario        = W_Usuario                                
         Mov_Ahorros.Val_Efectivo   = W_Dscto
         Mov_Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible
         Mov_Ahorros.Cod_Operacion  = 040101018
         Mov_Ahorros.Descrip        = "Retiro antes de 3 Meses"
         Mov_Ahorros.Cpte           = Comprobantes.Comprobante.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Crea_ListaNegra W-Ret_Asoc 
PROCEDURE Crea_ListaNegra :
/*------------------------------------------------------------------------------
     Invocado desde Proc.Proceso.
  Purpose:  Crea tabla ListaNegra.
------------------------------------------------------------------------------*/
IF SUBSTRING(Cmb_CauRet:SCREEN-VALUE IN FRAME F_Ret,1,5) EQ "00011" OR 
   SUBSTRING(Cmb_CauRet:SCREEN-VALUE IN FRAME F_Ret,1,5) EQ "00012" OR
   SUBSTRING(Cmb_CauRet:SCREEN-VALUE IN FRAME F_Ret,1,5) EQ "00015" THEN DO:
    CREATE ListaNegra.
    ASSIGN ListaNegra.Codigo            = 3
           ListaNegra.Nit               = W_NitUsu
           ListaNegra.Nombre            = TRIM(Clientes.Nombre)
           ListaNegra.Apellido1         = TRIM(Clientes.Apellido1)
           ListaNegra.Apellido2         = TRIM(Clientes.Apellido2)
           ListaNegra.Fec_Actualizacion = W_Fecha
           ListaNegra.Fec_Exclusion     = W_Fecha + 1800
           ListaNegra.Estado            = 1
           ListaNegra.Observacion       = W_Observ
           ListaNegra.Usuario           = W_Usuario.
           /* ListaNegra.Fec_HaVenido 
           ListaNegra.Id_HaVenido    */
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Ret_Asoc  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Ret_Asoc)
  THEN DELETE WIDGET W-Ret_Asoc.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Ret_Asoc  _DEFAULT-ENABLE
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
  DISPLAY W_NitUsu W_NomUsu Cmb_CauRet Cmb_Ctas W_CtaIng NomCta Msaje 
      WITH FRAME F_Ret IN WINDOW W-Ret_Asoc.
  ENABLE RECT-314 W_NitUsu BUTTON-5 Cmb_CauRet Cmb_Ctas Btn_Proc W_CtaIng 
         Btn_Done Br_Pdctos 
      WITH FRAME F_Ret IN WINDOW W-Ret_Asoc.
  {&OPEN-BROWSERS-IN-QUERY-F_Ret}
  VIEW W-Ret_Asoc.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HojaVida W-Ret_Asoc 
PROCEDURE HojaVida :
/*------------------------------------------------------------------------------
  Purpose:       
------------------------------------------------------------------------------*/
  CREATE Hoja_Vida.
  ASSIGN Hoja_Vida.Nit           = W_NitUsu
         Hoja_Vida.Tipo          = 5       
         Hoja_Vida.Codigo        = W_CauRet
         Hoja_Vida.Fec_Grabacion = W_Fecha
         Hoja_Vida.Hora_Grabacio = TIME      
         Hoja_Vida.Observacion   = W_Observ             
         Hoja_Vida.Usuario       = W_Usuario. 
    
  ASSIGN Hoja_Vida.DoctoRefer = Comprobantes.Secuencia  WHEN AVAIL(Comprobantes).          
         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImpCpte W-Ret_Asoc 
PROCEDURE ImpCpte :
DEFI VAR TotD LIKE Mov_Contable.Db INIT 0.
DEFI VAR TotC LIKE Mov_Contable.Db INIT 0.
DEFI VAR TTotD LIKE Mov_Contable.Db INIT 0.
DEFI VAR TTotC LIKE Mov_Contable.Db INIT 0.

{Incluido\RepEncabezado.I}

ASSIGN W_Reporte = "Reporte   : Comprobante Resumen Traslado por Retiro     Fecha del Cpte: " + STRING(W_Fecha,"99/99/9999") + "     Hora :" + STRING(TIME,"HH:MM:SS").

VIEW FRAME F-Encabezado.
VIEW FRAME f-ftr.

DISPLAY SKIP(0)
        "ExAsociado: " + clientes.nit + " - " + clientes.nombre + " " + clientes.apellido1 + " " + clientes.apellido2 + "     " + "Num_Documento: " + STRING(Comprobantes.Comprobante,"99") + "-" + STRING(Comprobantes.Secuencia,"99999999") FORMAT "X(120)" SKIP(1)
        "--------------------------------------------------------------------------------------------------------"
    WITH DOWN WIDTH 140 FRAME F1 USE-TEXT NO-LABELS STREAM-IO NO-BOX. 

FOR EACH Mov_Contable WHERE Mov_Contable.Comprobante   EQ Comprobantes.Comprobante
                          AND Mov_Contable.Num_Documento EQ Comprobantes.Secuencia
                          AND Mov_Contable.Fec_Contable  EQ W_Fecha NO-LOCK
                              BREAK BY Mov_Contable.Agencia BY Mov_Contable.Cuenta:
      ASSIGN TotD  = TotD  + Mov_Contable.Db
             TTotD = TTotD + Mov_Contable.Db
             TotC  = TotC  + Mov_Contable.Cr
             TTotC = TTotC + Mov_Contable.Cr.

      IF LAST-OF(Mov_Contable.Cuenta) THEN DO:
         FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Mov_Contable.Cuenta
                              AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
         DISPLAY Mov_Contable.Agencia   LABEL "Ag."
                 Mov_Contable.Cuenta    LABEL "Cta-Contable"
                 Cuentas.Nombre         LABEL "Descripciòn de la Cuenta" WHEN AVAIL(Cuentas)
                 TotD                   LABEL "TOTAL DEBITOS"  FORM "->>>>>>,>>>,>>9.99"
                 TotC                   LABEL "TOTAL CREDITOS" FORM "->>>>>>,>>>,>>9.99"
             WITH DOWN WIDTH 140 FRAME F21 USE-TEXT NO-LABELS STREAM-IO NO-BOX.

         ASSIGN TotD  = 0
                TotC  = 0.
      END.

  END.

  DISPLAY SKIP(1)
          "                     TOTAL FINAL------------>                   ---------------- ----------------"
          SKIP
          "                                                        "
          TTotD      FORM "->>>>>>,>>>,>>9.99"
          TTotC      FORM "->>>>>>,>>>,>>9.99"
             WITH DOWN WIDTH 140 FRAME FT21T USE-TEXT NO-LABELS STREAM-IO NO-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LlenaPdctos W-Ret_Asoc 
PROCEDURE LlenaPdctos :
/*------------------------------------------------------------------------------
  Purpose:     
 ------------------------------------------------------------------------------*/
  FOR EACH Pdctos:  DELETE Pdctos.  END.
    
  FOR EACH Ahorros WHERE Ahorros.Nit EQ W_NitUsu NO-LOCK 
                      BY Ahorros.Cod_Ahorro BY Ahorros.Estado:
      FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ Ahorros.Cod_Ahorro
                               AND Pro_Ahorros.Estado     EQ 1 NO-LOCK NO-ERROR.
        
      CREATE Pdctos.
      ASSIGN Pdctos.AGE = Ahorros.Agencia 
             Pdctos.Tip = "Ahorro"
             Pdctos.Pto = Ahorros.Cod_Ahorro
             Pdctos.Cta = Ahorros.Cue_Ahorro
             Pdctos.Fec = Ahorros.Fec_Apert
             Pdctos.Sdo = Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje
             Pdctos.Est = Ahorros.Estado
             Pdctos.Nom = Pro_Ahorros.Nom_Prod WHEN AVAIL Pro_Ahorros.
                                                    
      IF  AVAIL (Pro_Ahorros) 
      AND Pro_Ahorros.Tip_Ahorro NE 1
      AND Pro_Ahorros.Tip_Ahorro NE 4
      AND pro_ahorros.cod_ahorro NE 3
      AND (Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje) NE 0 THEN   /*Solo a la vista y aportes no es error*/
          ASSIGN Pdctos.Err = TRUE.                                                                    
        
      IF (Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje) LT 0 THEN     /*Sobregiros*/
          ASSIGN Pdctos.Err = TRUE.                          
  END.
    
  FOR EACH Creditos WHERE Creditos.Nit EQ W_NitUsu AND creditos.tip_credito < 7 NO-LOCK 
                       BY Creditos.Cod_Credito BY Creditos.Estado:
      FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ Creditos.Cod_Credito
                                AND Pro_Creditos.Estado      EQ 1 NO-LOCK NO-ERROR.
      CREATE Pdctos.
      ASSIGN Pdctos.AGE = Creditos.Agencia                        
             Pdctos.Tip = "Credito"                               
             Pdctos.Pto = Creditos.Cod_Credito 
             Pdctos.Cta = STRING(Creditos.Num_Credito) /* Nuevo - 25-Marzo-2008 */
/*              Pdctos.Sdo = Creditos.Sdo_Capital */
             Pdctos.Sdo = Creditos.Honorarios     + Creditos.Costas         + Creditos.Polizas        + 
                          Creditos.Int_MorCobrar  + Creditos.Int_MoraDifCob + Creditos.Int_Corrientes + 
                          Creditos.Int_DifCobro   + Creditos.Sdo_Capital    - Creditos.Int_Anticipado   
             Pdctos.Fec = Creditos.Fec_Desemb
             Pdctos.Est = Creditos.Estado
             Pdctos.Nom = Pro_Creditos.Nom_Prod WHEN AVAIL Pro_Creditos.
                                                        
/*       IF Creditos.Sdo_Capital NE 0 THEN  */
/*          ASSIGN Pdctos.Err = TRUE.       */
      IF  Creditos.Honorarios     + Creditos.Costas         + Creditos.Polizas        +
          Creditos.Int_MorCobrar  + Creditos.Int_MoraDifCob + Creditos.Int_Corrientes +
          Creditos.Int_DifCobro   + Creditos.Sdo_Capital    - Creditos.Int_Anticipado  NE 0 THEN
          ASSIGN Pdctos.Err = TRUE.
  END.
                    
END PROCEDURE.

/* W-ProRec_Ordinario.p*/
/* W_SdoDeuda = Creditos.Honorarios     + Creditos.Costas         + Creditos.Polizas        +  */
/*              Creditos.Int_MorCobrar  + Creditos.Int_MoraDifCob + Creditos.Int_Corrientes +  */
/*              Creditos.Int_DifCobro   + Creditos.Sdo_Capital    - Creditos.Int_Anticipado    */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proceso W-Ret_Asoc 
PROCEDURE Proceso :
DEFINE VAR P_ImpAplic LIKE Ahorros.Sdo_Dispon.

W_Dscto = 0.

ActuCli:
REPEAT ON ERROR UNDO ActuCli, LEAVE ActuCli:
    W_Observ = " ".

    FIND FIRST Pdctos WHERE Pdctos.Sdo GT 0
                        AND Pdctos.Tip EQ "Ahorro" NO-ERROR.
    IF AVAIL(Pdctos) THEN DO:
        FIND FIRST Varios WHERE Varios.Tipo EQ 5
                            AND Varios.Codigo EQ W_CauRet
                            AND Varios.Comprobante GT 0
                            AND Varios.Estado EQ 1 NO-LOCK NO-ERROR.

        FIND FIRST Comprobantes WHERE Comprobantes.Agencia EQ W_Agencia
                                  AND Comprobantes.Comprob EQ Varios.Comprobante
                                  AND Comprobantes.Estado EQ 1 NO-ERROR.

        IF NOT AVAIL(Comprobantes) OR NOT AVAIL(varios) THEN DO:
            MESSAGE "El Comprobante-Fuente Contable para el proceso debe existir en :" SKIP
                    "Tipos (Causales de Retiro) y en Comprobantes." SKIP
                    "                          No se acepta la Operación."
                VIEW-AS ALERT-BOX ERROR.

            UNDO ActuCli, LEAVE ActuCli.
        END.

        ASSIGN Comprobantes.Secuencia = Comprobantes.Secuencia + 1
               W_Observ = "Saldos : "
               W_SiContab = TRUE.

        FIND CURRENT Comprobantes NO-LOCK NO-ERROR.

        FIND FIRST Ahorros WHERE Ahorros.Nit EQ W_NitUsu
                             AND Ahorros.Cod_Ahorro EQ 5
                             AND Ahorros.Sdo_Dispon GT 0 NO-ERROR.
        IF AVAIL(Ahorros) AND (W_Fecha - Ahorros.Fec_Apertura) LT 0 THEN DO:
            FIND FIRST Indicadores WHERE Indicadores.Indicador EQ 18
                                     AND Indicadores.Estado EQ 1 NO-LOCK NO-ERROR.
            IF NOT AVAIL(Indicadores) THEN DO:
                MESSAGE "El Indicador 18 SMMLV no existe...Rectifique por favor"
                    VIEW-AS ALERT-BOX ERROR.

                UNDO ActuCli, LEAVE ActuCli.
            END.

            W_Dscto = ROUND((Indicadores.Valor * 1) / 100,0).

            IF W_Dscto GT Ahorros.Sdo_Dispon THEN
                W_Dscto = Ahorros.Sdo_Dispon.

            Ahorros.Sdo_Dispon = Ahorros.Sdo_Dispon - W_Dscto.

            RUN Contable_3Meses NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                UNDO ActuCli, LEAVE ActuCli.
        END.

        FIND CURRENT Ahorros NO-LOCK NO-ERROR.

        FOR EACH Pdctos WHERE Pdctos.Sdo GT 0
                          AND Pdctos.Tip EQ "Ahorro" BY Pdctos.Age
                                                     BY Pdctos.Pto:
            FIND FIRST Ahorros WHERE Ahorros.Nit EQ W_NitUsu
                                 AND Ahorros.Cod_Ahorro EQ Pdctos.Pto
                                 AND Ahorros.Cue_ahorro EQ Pdctos.Cta
                                 AND (Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje) GT 0 NO-ERROR.
            IF AVAIL(Ahorros) THEN DO:
                FIND FIRST CortoLargo WHERE CortoLargo.Agencia EQ Pdctos.Age
                                        AND CortoLargo.Clase_Producto EQ 1
                                        AND CortoLargo.Cod_Producto EQ Ahorros.Cod_Ahorro NO-LOCK NO-ERROR.
                IF AVAIL(CortoLargo) THEN DO:
                    FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ CortoLargo.Cta_AsoAd
                                         AND Cuentas.Estado EQ 1
                                         AND Cuentas.Tipo EQ 2 NO-LOCK NO-ERROR.
                    IF NOT AVAIL(Cuentas) THEN DO:
                        MESSAGE "La Cuenta del Pdcto de Ahorros : " Pro_Ahorros.Cod_Ahorro ", de la Agencia:" CortoLargo.Agencia SKIP
                                "En CortoLargo.Cta_AsoAd no existe...Rectifique por favor"
                            VIEW-AS ALERT-BOX ERROR.

                        UNDO ActuCli, LEAVE ActuCli.
                    END.
                END.
                ELSE DO:
                    MESSAGE "Debe Existir CortoLargo con el Pdcto de ahorro: " Ahorros.Cod_Ahorro SKIP
                            "                             Proceso Rechazado..."
                        VIEW-AS ALERT-BOX ERROR.

                    UNDO ActuCli, LEAVE ActuCli.
                END.

                RUN RutGMF.R (INPUT TRUE,
                              INPUT W_Agencia,
                              INPUT Ahorros.Agencia,
                              INPUT 1,
                              INPUT Ahorros.Cod_Ahorro,
                              INPUT Ahorros.Nit,
                              INPUT Ahorros.Cue_Ahorro,
                              INPUT 010102001,
                              INPUT (Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje),
                              INPUT Comprobantes.Comprobante,
                              INPUT STRING(Comprobantes.Secuencia),
                              INPUT "Por Retiro Asoc.",
                              INPUT 0,
                              INPUT 0,
                              OUTPUT P_ImpAplic) NO-ERROR.
                IF ERROR-STATUS:ERROR THEN DO:
                    MESSAGE "El programa RutGMF.P...Retornò ERROR, no se permite la operaciòn."
                        VIEW-AS ALERT-BOX ERROR.

                    RETURN ERROR.
                END.

                RUN Contable NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                    UNDO ActuCli, LEAVE ActuCli.

                IF Ahorros.Tip_Ahorro EQ 1 AND Ahorros.INT_Causado GT 0 THEN
                    RUN Reversa_Causado NO-ERROR.

                IF ERROR-STATUS:ERROR THEN
                    UNDO ActuCli, LEAVE ActuCli.

                ASSIGN Ahorros.Estado = 2
                       Ahorros.Sdo_Revaloriza = Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje + W_Dscto
                       W_Observ = W_Observ + TRIM(Pdctos.Nom) + " $" + STRING(Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje,">>>>,>>>,>>9.99") + ","
                       Ahorros.Sdo_Dispon = 0
                       Ahorros.Sdo_Canje = 0
                       Ahorros.Fec_UltTr = W_Fecha
                       Ahorros.Fec_Cancel = W_Fecha.

                FIND CURRENT Ahorros NO-LOCK NO-ERROR.
            END.
        END.
    END.

    FIND FIRST Clientes WHERE Clientes.Nit EQ W_NitUsu NO-ERROR.
    
    RUN HojaVida.
    RUN Crea_ListaNegra.

    IF AVAIL(Clientes) AND Clientes.Estado EQ 1 THEN  DO:
        ASSIGN Clientes.Estado = 2
               Clientes.Usuario = W_Usuario
               Clientes.Cod_Retiro = W_CauRet
               Clientes.Tipo_Vinculo = 2
               Clientes.Fec_Retiro = W_Fecha.

        FOR EACH Pdctos WHERE Pdctos.Tip = "Credito"
                          AND Pdctos.Pto = 123
                          AND Pdctos.Sdo = 0 NO-LOCK:
            FIND FIRST Creditos WHERE Creditos.Nit EQ W_NitUsu
                                  AND Creditos.Num_Credito EQ INTEGER(Pdctos.Cta)
                                  AND Creditos.cod_credito EQ Pdctos.Pto EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
            IF LOCKED (Creditos) THEN DO:
                MESSAGE "Registro Usado Por Otro Usuario..." SKIP
                        "Intente de Nuevo Mas Tarde de Retirar el Asociado."
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.

                UNDO ActuCli, LEAVE ActuCli.
            END.

            IF AVAILABLE(Creditos) THEN DO:
                UPDATE Creditos.Estado = 3
                       Creditos.Usuario = W_Usuario
                       Creditos.Fec_CanceTotal = W_Fecha
                       creditos.fec_pago = ?
                       creditos.dias_atraso = 0
                       creditos.val_atraso = 0.

                FIND CURRENT Creditos NO-LOCK NO-ERROR.
            END.
        END.
    END.

    FIND CURRENT Clientes NO-LOCK NO-ERROR.

    LEAVE.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir W-Ret_Asoc 
PROCEDURE ProcesoImprimir :
/*------------------------------------------------------------------------------
  Purpose:     
------------------------------------------------------------------------------*/
   IF W_SiContab THEN DO:
   /* DO:
      RUN formatos.r (INPUT "NOTA2MUL", ROWID_con, 0, 0, "Retiro Asociado -Fecha" + STRING(W_Fecha,"99/99/9999") + " Hora:" + STRING(TIME,"HH:MM:SS"), 0, 0).
      APPLY "ENTRY" TO W_NitUsu IN FRAME F_Ret.
   END. */
     RUN ImpCpte.
     APPLY "ENTRY" TO W_NitUsu IN FRAME F_Ret.
   END.


     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Reversa_Causado W-Ret_Asoc 
PROCEDURE Reversa_Causado :
/*------------------------------------------------------------------------------
  Purpose:     Reversa causados para cancelaciones de a la vista.
  Notes:       Feb.1/06 GAER.
------------------------------------------------------------------------------*/
  FIND FIRST Liqui_Int WHERE Liqui_Int.Clase_Producto EQ 1
                         AND Liqui_Int.Cod_Producto   EQ Ahorros.Cod_Ahorro NO-LOCK NO-ERROR.
  IF NOT AVAIL(Liqui_Int)
  OR (AVAIL(Liqui_Int) AND 
     (Liqui_Int.Cta_CauCr LE "0" OR Liqui_Int.CtaDb_LiqAso LE "0"))  THEN DO:
     MESSAGE "Falta Liqui_Int Para el producto : " Ahorros.Cod_Ahorro   SKIP
             "Con cuentas para reversar los Int-Causados."
         VIEW-AS ALERT-BOX ERROR.
     RETURN ERROR.
  END.

  CREATE Mov_Ahorros.
  ASSIGN Mov_Ahorros.Cod_Operacion = 010101004
         Mov_ahorros.cod_ahorro    = Ahorros.Cod_ahorro
         Mov_Ahorros.Cue_Ahorros   = Ahorros.Cue_Ahorros
         Mov_ahorros.nit           = Ahorros.nit
         Mov_Ahorros.Fecha         = W_Fecha
         Mov_Ahorros.Hora          = TIME
         Mov_Ahorros.Num_Documento = STRING(Comprobantes.Secuencia)
         Mov_Ahorros.Cpte          = Comprobantes.Comprobante
         Mov_Ahorros.Agencia       = Ahorros.Agencia
         Mov_Ahorros.Age_Fuente    = W_Agencia
         Mov_Ahorros.Age_Destino   = W_Agencia
         Mov_Ahorros.Usuario       = W_Usuario
         Mov_Ahorros.Val_Efectivo  = Ahorros.INT_Causado
         Mov_Ahorros.Descrip       = "Reversa CausadoXRetiro"
         Mov_Ahorros.Sdo_Dispon    = Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje.

  CREATE Mov_Contable.
  ASSIGN Mov_Contable.Agencia        = Ahorros.Agencia
         Mov_Contable.Comprobante    = Comprobantes.Comprobante
         Mov_Contable.Num_Documento  = Comprobantes.Secuencia  
         Mov_Contable.Cuenta         = Liqui_Int.Cta_CauCr
         Mov_Contable.Fec_Contable   = W_Fecha
         Mov_Contable.Comentario     = "Reversa CausadoXRetiro"
         Mov_Contable.Usuario        = W_Usuario
         Mov_contable.Nit            = Ahorros.Nit
         Mov_Contable.Cen_Costos     = 999
         Mov_Contable.Destino        = W_Agencia         
         Mov_Contable.Doc_Referencia = SUBSTRING(Ahorros.Cue_Ahorros,1,10)
         Mov_Contable.Fec_Grabacion  = TODAY
         Mov_Contable.Hora           = TIME
         Mov_Contable.Estacion       = W_Estacion
         Mov_Contable.Db             = Ahorros.INT_Causado.

  CREATE Mov_Contable.
  ASSIGN Mov_Contable.Agencia        = Ahorros.Agencia
         Mov_Contable.Comprobante    = Comprobantes.Comprobante
         Mov_Contable.Num_Documento  = Comprobantes.Secuencia  
         Mov_Contable.Cuenta         = Liqui_Int.CtaDb_LiqAso
         Mov_Contable.Fec_Contable   = W_Fecha
         Mov_Contable.Comentario     = "Reversa Causado x Canc"
         Mov_Contable.Usuario        = W_Usuario
         Mov_contable.Nit            = Ahorros.Nit
         Mov_Contable.Cen_Costos     = 999
         Mov_Contable.Destino        = W_Agencia
         Mov_Contable.Doc_Referencia = SUBSTRING(Ahorros.Cue_Ahorros,1,10)
         Mov_Contable.Fec_Grabacion  = TODAY
         Mov_Contable.Hora           = TIME
         Mov_Contable.Estacion       = W_Estacion
         Mov_Contable.Cr             = Ahorros.INT_Causado.

  Ahorros.INT_Causado         = 0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

