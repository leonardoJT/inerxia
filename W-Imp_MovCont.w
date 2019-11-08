&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-Imp_MovCont
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Imp_MovCont 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* oakley */

 ON RETURN TAB.

 {Incluido/Variable.I "SHARED"}

 DEFI TEMP-TABLE Tmp
      FIELD Age  LIKE Agencias.Agencia
      FIELD Ced  LIKE Clientes.Nit
      FIELD Vr   LIKE Creditos.Sdo_Capital INIT 0
      FIELD VIng AS DECIMAL INIT 0.

 DEFI VAR W_Cpte LIKE Comprobantes.Comprobante.
 DEFI VAR W_Age  LIKE Agencias.Agencia.

 DEFI VAR W_Nat    LIKE Cuentas.Natur.
 DEFI VAR W_Ctr    LIKE Cuentas.Ctr_Natur.
 DEFI VAR W_OfiIni LIKE Agencias.Agencia.
 DEFI VAR W_OfiFin LIKE Agencias.Agencia.
 DEFI VAR Xsdo AS DECIMAL INIT 0.
 DEFI VAR W_TasaUs LIKE Indicadores.Tasa INIT 0.

 DEFI VAR CtaAnex_Det LIKE Cuentas.Cuenta.
 DEFI VAR NatAnex_Det LIKE Cuentas.Naturaleza.

 DEFI VAR CtaAnex_Tot LIKE Cuentas.Cuenta.
 DEFI VAR NatAnex_Tot LIKE Cuentas.Naturaleza.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F_Proc

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Cmb_Cptes W_CmbOfi Rs_DbCr W_CtaContab ~
W_CtaContra Tg_Coomeva Rs_DetTot W_NitCont F_Comentario W_CtaIva ~
Btn_Importar Btn_Contabilizar Btn_FinImp RECT-318 RECT-319 RECT-320 ~
RECT-321 
&Scoped-Define DISPLAYED-OBJECTS Cmb_Cptes W_CmbOfi Rs_DbCr W_CtaContab ~
NomCta1 W_CtaContra NomCta2 Tg_Coomeva Rs_DetTot W_CtaIngrBen NomBen ~
W_NitCont NomCte W_CtaIngreso NomIng W_NroImp W_VrImp W_CtaInteres ~
F_Comentario NomInt W_CtaIva NomIva 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Imp_MovCont AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Anexos 
     LABEL "Subir solo Anexos" 
     SIZE 45 BY 1.12.

DEFINE BUTTON Btn_Contabilizar 
     LABEL "&Contabilizar" 
     SIZE 13 BY 1.54 TOOLTIP "Abona la Distribución y Contabiliza".

DEFINE BUTTON Btn_FinImp 
     IMAGE-UP FILE "Imagenes/volver.bmp":U
     LABEL "&Regresar" 
     SIZE 12.72 BY 1.62 TOOLTIP "Regresar a la ventana principal".

DEFINE BUTTON Btn_Importar 
     LABEL "&Importar" 
     SIZE 13 BY 1.54 TOOLTIP "Abona la Distribución y Contabiliza".

DEFINE VARIABLE Cmb_Cptes AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 35.86 BY 1 TOOLTIP "Seleccione el Comprobante para los Asientos Contables"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CmbOfi AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS COMBO-BOX INNER-LINES 20
     DROP-DOWN-LIST
     SIZE 41.14 BY 1 TOOLTIP "Agencias Disponibles"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F_Comentario AS CHARACTER FORMAT "X(256)":U 
     LABEL "Comentario" 
     VIEW-AS FILL-IN 
     SIZE 34 BY .81
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE NomBen AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 36 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomCta1 AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 36.86 BY .88
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomCta2 AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 36.72 BY .88
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomCte AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 36.29 BY .88
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomIng AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 36 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomInt AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 36 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomIva AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 36 BY .88
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CtaContab AS CHARACTER FORMAT "X(14)":U 
     LABEL "Cuenta Débito/Crédito" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .85 TOOLTIP "Con Doble click consulta"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CtaContra AS CHARACTER FORMAT "X(14)":U 
     LABEL "Cuenta Contrapartida" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CtaIngrBen AS CHARACTER FORMAT "X(14)":U 
     LABEL "Cuenta Ingresos X Benef." 
     VIEW-AS FILL-IN 
     SIZE 12.72 BY .77 TOOLTIP "Con Doble click consulta"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CtaIngreso AS CHARACTER FORMAT "X(14)":U 
     LABEL "Cuenta Ingresos X Interés" 
     VIEW-AS FILL-IN 
     SIZE 12.72 BY .77 TOOLTIP "Con Doble click consulta"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CtaInteres AS CHARACTER FORMAT "X(14)":U 
     LABEL "Cuenta Interés por Cobrar" 
     VIEW-AS FILL-IN 
     SIZE 12.72 BY .92 TOOLTIP "Con Doble click consulta"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CtaIva AS CHARACTER FORMAT "X(14)":U 
     LABEL "Cuenta de Iva" 
     VIEW-AS FILL-IN 
     SIZE 12.72 BY .92 TOOLTIP "Con Doble click consulta"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NitCont AS CHARACTER FORMAT "X(12)":U 
     VIEW-AS FILL-IN 
     SIZE 13.72 BY .77
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NroImp AS INTEGER FORMAT ">>,>>9":U INITIAL 0 
     LABEL "No.Reg.Import." 
     VIEW-AS FILL-IN 
     SIZE 8.14 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_VrImp AS DECIMAL FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     LABEL "Por Vlr.de" 
     VIEW-AS FILL-IN 
     SIZE 13.43 BY .85
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Rs_DbCr AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Valores Débito", 1,
"Valores Crédito", 2
     SIZE 33.43 BY .58 NO-UNDO.

DEFINE VARIABLE Rs_DetTot AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Contrapartida Global", 1,
"En detalle", 2
     SIZE 34.72 BY .5 NO-UNDO.

DEFINE RECTANGLE RECT-318
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 35.72 BY 1.04.

DEFINE RECTANGLE RECT-319
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 36.14 BY .85.

DEFINE RECTANGLE RECT-320
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL 
     SIZE 47.43 BY 2.27.

DEFINE RECTANGLE RECT-321
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL 
     SIZE 41.29 BY 10.23.

DEFINE VARIABLE Tg_Coomeva AS LOGICAL INITIAL no 
     LABEL "Toggle 1" 
     VIEW-AS TOGGLE-BOX
     SIZE 2.29 BY .77 TOOLTIP "Marque Solo si el Proceso es para COOMEVA" NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Proc
     Cmb_Cptes AT ROW 1.65 COL 2.57 NO-LABEL
     W_CmbOfi AT ROW 2.19 COL 47.43 COLON-ALIGNED NO-LABEL
     Rs_DbCr AT ROW 2.96 COL 4 NO-LABEL
     W_CtaContab AT ROW 3.92 COL 22.43 COLON-ALIGNED
     NomCta1 AT ROW 3.92 COL 36.86 COLON-ALIGNED NO-LABEL
     W_CtaContra AT ROW 5.04 COL 22.29 COLON-ALIGNED
     NomCta2 AT ROW 5.04 COL 36.86 COLON-ALIGNED NO-LABEL
     Tg_Coomeva AT ROW 6.08 COL 82
     Rs_DetTot AT ROW 6.65 COL 2.86 NO-LABEL
     W_CtaIngrBen AT ROW 6.85 COL 73.43 COLON-ALIGNED
     NomBen AT ROW 7.65 COL 50.14 COLON-ALIGNED NO-LABEL
     W_NitCont AT ROW 7.73 COL 22.57 COLON-ALIGNED NO-LABEL
     NomCte AT ROW 8.69 COL 2.29 NO-LABEL
     W_CtaIngreso AT ROW 8.81 COL 73.29 COLON-ALIGNED
     NomIng AT ROW 9.62 COL 50 COLON-ALIGNED NO-LABEL
     W_NroImp AT ROW 9.88 COL 14.43 COLON-ALIGNED
     W_VrImp AT ROW 9.88 COL 33.43 COLON-ALIGNED
     W_CtaInteres AT ROW 10.69 COL 73.43 COLON-ALIGNED
     F_Comentario AT ROW 11.5 COL 11 COLON-ALIGNED
     NomInt AT ROW 11.65 COL 50.14 COLON-ALIGNED NO-LABEL
     W_CtaIva AT ROW 12.81 COL 73.43 COLON-ALIGNED
     Btn_Importar AT ROW 13.23 COL 3.86 HELP
          "Permite Realizar la contabilización de Depreciación"
     Btn_Contabilizar AT ROW 13.23 COL 18.86 HELP
          "Permite Realizar la contabilización de Depreciación"
     Btn_FinImp AT ROW 13.23 COL 34.14
     NomIva AT ROW 13.69 COL 50.14 COLON-ALIGNED NO-LABEL
     Btn_Anexos AT ROW 15.5 COL 3
     RECT-318 AT ROW 2.73 COL 2.72
     RECT-319 AT ROW 6.5 COL 2.14
     RECT-320 AT ROW 12.85 COL 2
     RECT-321 AT ROW 6.38 COL 49.29
     "Comprobante Contable" VIEW-AS TEXT
          SIZE 21.43 BY .5 AT ROW 1.08 COL 2.86
     "Agencia" VIEW-AS TEXT
          SIZE 7.72 BY .88 AT ROW 1.27 COL 50
     " Configuraciones COOMEVA" VIEW-AS TEXT
          SIZE 25 BY .62 AT ROW 6.12 COL 57
          BGCOLOR 17 FGCOLOR 7 
     "Nit Contrapartida Global :" VIEW-AS TEXT
          SIZE 21.43 BY .5 AT ROW 7.96 COL 2.43
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 91.57 BY 15.88
         BGCOLOR 17 FONT 5.


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
  CREATE WINDOW W-Imp_MovCont ASSIGN
         HIDDEN             = YES
         TITLE              = "Importar Mov-Contable, Programa W-Imp_MovCont.W"
         HEIGHT             = 15.88
         WIDTH              = 91.57
         MAX-HEIGHT         = 18.35
         MAX-WIDTH          = 91.57
         VIRTUAL-HEIGHT     = 18.35
         VIRTUAL-WIDTH      = 91.57
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
/* SETTINGS FOR WINDOW W-Imp_MovCont
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F_Proc
                                                                        */
/* SETTINGS FOR BUTTON Btn_Anexos IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX Cmb_Cptes IN FRAME F_Proc
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN NomBen IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN NomCta1 IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN NomCta2 IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN NomCte IN FRAME F_Proc
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN NomIng IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN NomInt IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN NomIva IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_CtaIngrBen IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_CtaIngreso IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_CtaInteres IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NroImp IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_VrImp IN FRAME F_Proc
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Imp_MovCont)
THEN W-Imp_MovCont:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Proc
/* Query rebuild information for FRAME F_Proc
     _Query            is NOT OPENED
*/  /* FRAME F_Proc */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Imp_MovCont
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Imp_MovCont W-Imp_MovCont
ON END-ERROR OF W-Imp_MovCont /* Importar Mov-Contable, Programa W-Imp_MovCont.W */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Imp_MovCont W-Imp_MovCont
ON WINDOW-CLOSE OF W-Imp_MovCont /* Importar Mov-Contable, Programa W-Imp_MovCont.W */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Anexos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Anexos W-Imp_MovCont
ON CHOOSE OF Btn_Anexos IN FRAME F_Proc /* Subir solo Anexos */
DO:
  RUN SoloAnexos.
  DISABLE Btn_Anexos WITH FRAME F_Proc.
  MESSAGE "Se han subido con exito los anexos".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Contabilizar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Contabilizar W-Imp_MovCont
ON CHOOSE OF Btn_Contabilizar IN FRAME F_Proc /* Contabilizar */
DO:
  DEFI VAR TContab LIKE Tmp.Vr INIT 0.
  DEFI VAR TIngr AS DECIMAL INIT 0.
  DEFI VAR TAgen AS DECIMAL INIT 0.
  DEFI VAR TAgInt  LIKE Mov_Contable.Db INIT 0.
  DEFINE VAR W_Sec10 LIKE Comprobantes.Secuencia.
  
  ASSIGN FRAME F_Proc F_Comentario.

  IF F_Comentario EQ "" THEN DO:
     MESSAGE "El Movimiento contable no debe llevar el campo" SKIP
             "de comentario vacio. ingrese el comentario" VIEW-AS ALERT-BOX WARNING.
     APPLY "entry" TO F_Comentario IN FRAME F_Proc.
     RETURN NO-APPLY.
  END.

  IF Rs_DetTot EQ 1 AND W_NitCont LE " " THEN
     MESSAGE "La contabilización es Contrapartida Global, No hay Nit-Contrapartida." SKIP
             "                          Revise por favor..."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
DO TRANSACTION:
  /*coge comprobante para la agencia 10*/
  FIND FIRST Comprobantes WHERE Comprobantes.Agencia     EQ 10
                            AND Comprobantes.Comprobante EQ W_Cpte
                            AND Comprobantes.Estado      EQ 1 NO-ERROR.
  IF AVAIL(Comprobantes) THEN DO:
     ASSIGN Comprobantes.Secuencia = Comprobantes.Secuencia + 1
            W_Sec10 = Comprobantes.Secuencia.
     FIND CURRENT Comprobantes NO-LOCK NO-ERROR.
  END.
  ELSE DO:
     MESSAGE "Falta El Comprobante activo para la Contabilización."
         VIEW-AS ALERT-BOX.
     RETURN.
  END.

  IF SUBSTRING(W_CmbOfi:SCREEN-VALUE IN FRAME F_Proc,1,3) NE "000" THEN DO:
      FIND FIRST Comprobantes WHERE Comprobantes.Agencia     EQ W_Agencia
                                AND Comprobantes.Comprobante EQ W_Cpte
                                AND Comprobantes.Estado      EQ 1 NO-ERROR.
      IF AVAIL(Comprobantes) THEN DO:
         ASSIGN Comprobantes.Secuencia = Comprobantes.Secuencia + 1.
         FIND CURRENT Comprobantes NO-LOCK NO-ERROR.
      END.
      ELSE DO:
         MESSAGE "Falta El Comprobante activo para la Contabilización."
             VIEW-AS ALERT-BOX.
         RETURN.
      END.
  END.

  SESSION:SET-WAIT-STATE("General").

  IF Tg_Coomeva THEN DO:
     MESSAGE "                    El proceso es COOMEVA..." SKIP
             "SOLO si Está Segura(o) de Liquidar Intereses sobre Saldos-Pendientes Continue con SI...?" 
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "CONFIRME LIQUIDAR INTERESES"
         UPDATE RptaCoom AS LOGICAL.
     IF RptaCoom THEN DO:
        RUN Proceso_Coomeva NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
           MESSAGE "La Liquidación de Intereses Coomeva presento errores Revise por favor..."
              VIEW-AS ALERT-BOX ERROR.
           RETURN.
        END.
     END.
  END.

  FIND FIRST Tmp NO-ERROR.
  IF NOT AVAIL(Tmp) THEN DO:
     MESSAGE "No hay Información en la Temporal para Contabilizar."
         VIEW-AS ALERT-BOX.
     RETURN.
  END.

  FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ W_CtaContab
                       AND Cuentas.Tipo   EQ 2
                       AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
  IF AVAIL(Cuentas) THEN
     FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ W_CtaContra
                          AND Cuentas.Tipo   EQ 2
                          AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.

  IF NOT AVAIL(Cuentas) OR W_CtaContab LE "0" OR W_CtaContra LE "0" 
  OR W_Cpte LE 0 THEN DO:
     MESSAGE "Faltan las Ctas-Contables o El Comprobante para la Contabilización."
         VIEW-AS ALERT-BOX.
     RETURN.
  END.
  ELSE DO:
     MESSAGE "        Contabilizar los Reg.Importados" SKIP
             "                     Está Segura(o) de Continuar...?" 
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "CONFIRME CONTABILIZAR"
         UPDATE RptaCont AS LOGICAL.
     IF NOT RptaCont THEN 
        RETURN.
  END.

  FOR EACH Tmp BREAK BY Tmp.Age:
      IF FIRST-OF(Tmp.Age) AND SUBSTRING(W_CmbOfi:SCREEN-VALUE IN FRAME F_Proc,1,3) EQ "000" THEN DO:
          FIND FIRST Comprobantes WHERE Comprobantes.Agencia     EQ Tmp.Age
                                    AND Comprobantes.Comprobante EQ W_Cpte
                                    AND Comprobantes.Estado      EQ 1 NO-ERROR.
          IF AVAIL(Comprobantes) THEN DO:
             ASSIGN Comprobantes.Secuencia = Comprobantes.Secuencia + 1.
             FIND CURRENT Comprobantes NO-LOCK NO-ERROR.
          END.
          ELSE DO:
             MESSAGE "Falta El Comprobante activo para la Contabilización" SKIP
                     "de la agencia: " Tmp.Age VIEW-AS ALERT-BOX.
             RETURN.
          END.
      END.
      CREATE Mov_Contable.
      ASSIGN Mov_Contable.Agencia        = Tmp.Age
             Mov_Contable.Cuenta         = W_CtaContab            
             Mov_Contable.Nit            = Tmp.Ced     
             Mov_Contable.Fec_Contable   = W_Fecha                     
             Mov_Contable.Comentario     = F_Comentario    
             Mov_Contable.Usuario        = W_Usuario                   
             Mov_Contable.Cen_Costos     = W_Cencosgral                
             Mov_Contable.Destino        = W_Agencia                   
             Mov_Contable.Comprobante    = Comprobantes.Comprobante    
             Mov_Contable.Num_Documento  = Comprobantes.Secuencia      
             Mov_Contable.Fec_Grabacion  = TODAY                       
             Mov_Contable.Hora           = TIME                        
             Mov_Contable.Estacion       = W_Estacion
             TContab                     = TContab + Tmp.Vr
             TAgen                       = TAgen + Tmp.Vr.

      IF (Rs_DbCr EQ 1 AND Tmp.Vr GT 0)
      OR (Rs_DbCr EQ 2 AND Tmp.Vr LT 0) THEN DO:
          Mov_Contable.Db = Tmp.Vr.
          
          IF Mov_Contable.Db LT 0 THEN DO:
              Mov_Contable.Db = Mov_Contable.Db * -1.
          END.
      END.
      ELSE DO:
          Mov_Contable.Cr = Tmp.Vr.
          
          IF Mov_Contable.Cr LT 0 THEN DO:
              Mov_Contable.Cr = Mov_Contable.Cr * -1.
          END.
      END.


      IF Tg_Coomeva AND Tmp.VIng GT 0 THEN DO:
         RUN ContabIngreso.
         ASSIGN TIngr  = TIngr  + Tmp.VIng
                TAgInt = TAgInt + Tmp.VIng.
      END.

      IF Rs_DetTot EQ 2 THEN DO:
         CREATE Mov_Contable.
         ASSIGN Mov_Contable.Agencia     = Tmp.Age
             Mov_Contable.Cuenta         = W_CtaContra            
             Mov_Contable.Nit            = Tmp.Ced     
             Mov_Contable.Fec_Contable   = W_Fecha                     
             Mov_Contable.Comentario     = "Importación Plano"    
             Mov_Contable.Usuario        = W_Usuario                   
             Mov_Contable.Cen_Costos     = W_Cencosgral                
             Mov_Contable.Destino        = W_Agencia                   
             Mov_Contable.Comprobante    = Comprobantes.Comprobante    
             Mov_Contable.Num_Documento  = Comprobantes.Secuencia      
             Mov_Contable.Fec_Grabacion  = TODAY                       
             Mov_Contable.Hora           = TIME                        
             Mov_Contable.Estacion       = W_Estacion.                  

         IF (Rs_DbCr EQ 1 AND Tmp.Vr GT 0)               
         OR (Rs_DbCr EQ 2 AND Tmp.Vr LT 0) THEN DO:      
             Mov_Contable.Cr = Tmp.Vr.                   
             
             IF Mov_Contable.Cr LT 0 THEN DO:               
                 Mov_Contable.Cr = Mov_Contable.Cr * -1.  
             END.
         END.                                            
         ELSE DO:                                        
             Mov_Contable.Db = Tmp.Vr.                   
             
             IF Mov_Contable.Db LT 0 THEN DO:                
                 Mov_Contable.Db = Mov_Contable.Db * -1.  
             END.
         END.                                            
      END.
      ELSE IF LAST-OF(Tmp.Age) THEN DO:
         RUN ContabSyA.

         Mov_Contable.Agencia = Tmp.Age.
         
         IF TAgen - TAgInt GT 0 THEN DO:
             Mov_Contable.Cr = TAgen - TAgInt. 
         END.
         ELSE DO: 
             Mov_Contable.Db = TAgInt - TAgen.
         END.

         RUN ContabSyA.
         ASSIGN Mov_Contable.Agencia = 010
                Mov_Contable.Nit     = STRING(Tmp.Age,"999")
                Mov_Contable.Num_Documento = W_Sec10.                

         IF TAgen - TAgInt GT 0 THEN DO:
             Mov_Contable.Db = TAgen - TAgInt.
         END.
         ELSE DO: 
             Mov_Contable.Cr = TAgInt - TAgen.
         END.

         ASSIGN TAgInt = 0
                TAgen  = 0.
      END.
  END.  /*Fin del for each*/

  IF Rs_DetTot EQ 1 THEN DO:
     CREATE Mov_Contable.                                       
     ASSIGN Mov_Contable.Agencia     = 010
         Mov_Contable.Cuenta         = W_CtaContra    
         Mov_Contable.Nit            = W_NitCont
         Mov_Contable.Fec_Contable   = W_Fecha                         
         Mov_Contable.Comentario     = "Importación Plano"          
         Mov_Contable.Usuario        = W_Usuario                       
         Mov_Contable.Cen_Costos     = W_Cencosgral                    
         Mov_Contable.Destino        = W_Agencia                       
         Mov_Contable.Comprobante    = Comprobantes.Comprobante        
         Mov_Contable.Num_Documento  = W_Sec10 /*Comprobantes.Secuencia          */
         Mov_Contable.Fec_Grabacion  = TODAY                           
         Mov_Contable.Hora           = TIME                            
         Mov_Contable.Estacion       = W_Estacion.                      
                                                                 
     IF (Rs_DbCr EQ 1 AND TContab GT 0)                              
     OR (Rs_DbCr EQ 2 AND TContab LT 0) THEN DO:                     
         Mov_Contable.Cr = TContab - TIngr.                                  
         IF Mov_Contable.Cr LT 0 THEN                               
            Mov_Contable.Cr = Mov_Contable.Cr * -1.                 
     END.                                                           
     ELSE DO:                                                       
         Mov_Contable.Db = TContab - TIngr.                                  
         IF Mov_Contable.Db LT 0 THEN                               
            Mov_Contable.Db = Mov_Contable.Db * -1.                 
     END.    
  END.  

  MESSAGE "Terminó la contabilización de los Registros Importados."
      VIEW-AS ALERT-BOX INFO BUTTONS OK.

  FOR EACH Tmp: DELETE Tmp. END.

  ASSIGN W_NroImp = 0                                   
         W_VrImp  = 0                                         
         W_NroImp:SCREEN-VALUE IN FRAME F_Proc = "0"          
         W_VrImp:SCREEN-VALUE  = "0".        
END.
SESSION:SET-WAIT-STATE("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_FinImp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_FinImp W-Imp_MovCont
ON CHOOSE OF Btn_FinImp IN FRAME F_Proc /* Regresar */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Importar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Importar W-Imp_MovCont
ON CHOOSE OF Btn_Importar IN FRAME F_Proc /* Importar */
DO:
  RUN Importar.
  ENABLE Btn_Anexos WITH FRAME F_Proc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Cptes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Cptes W-Imp_MovCont
ON VALUE-CHANGED OF Cmb_Cptes IN FRAME F_Proc
DO:
  ASSIGN W_Cpte = INTEG(SUBSTRING(Cmb_Cptes:SCREEN-VALUE,1,3)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Rs_DbCr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Rs_DbCr W-Imp_MovCont
ON MOUSE-SELECT-CLICK OF Rs_DbCr IN FRAME F_Proc
DO:
  ASSIGN Rs_DbCr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Rs_DetTot
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Rs_DetTot W-Imp_MovCont
ON MOUSE-SELECT-CLICK OF Rs_DetTot IN FRAME F_Proc
DO:
  ASSIGN Rs_DetTot.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Tg_Coomeva
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Tg_Coomeva W-Imp_MovCont
ON MOUSE-SELECT-CLICK OF Tg_Coomeva IN FRAME F_Proc /* Toggle 1 */
DO:
  ASSIGN Tg_Coomeva
         W_CtaIngreso:SENSITIVE = FALSE
         W_CtaIngrBen:SENSITIVE = FALSE
         W_CtaInteres:SENSITIVE = FALSE.

  IF Tg_Coomeva THEN
     ASSIGN W_CtaIngreso:SENSITIVE = TRUE
            W_CtaIngrBen:SENSITIVE = TRUE
            W_CtaInteres:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CmbOfi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbOfi W-Imp_MovCont
ON VALUE-CHANGED OF W_CmbOfi IN FRAME F_Proc
DO:
  ASSIGN W_Age = INTEG(SUBSTRING(W_CmbOfi:SCREEN-VALUE,1,3))
         W_OfiIni = W_Age
         W_OfiFin = W_Age.

  IF W_Age EQ 0 THEN
     ASSIGN W_OfiIni = 1
            W_OfiFin = 999.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CtaContab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CtaContab W-Imp_MovCont
ON LEAVE OF W_CtaContab IN FRAME F_Proc /* Cuenta Débito/Crédito */
DO:
  ASSIGN W_CtaContab
         NomCta1:SCREEN-VALUE = "".

  FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ W_CtaContab
                       AND Cuentas.Tipo   EQ 2
                       AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
  IF AVAIL(Cuentas) THEN
     ASSIGN NomCta1:SCREEN-VALUE = Cuentas.Nombre
            CtaAnex_Det = Cuentas.Cuenta
            NatAnex_Det = Cuentas.Naturaleza.
  ELSE APPLY "Mouse-Select-DblClick" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CtaContab W-Imp_MovCont
ON MOUSE-SELECT-DBLCLICK OF W_CtaContab IN FRAME F_Proc /* Cuenta Débito/Crédito */
DO:
     ASSIGN W-Imp_MovCont:SENSITIVE   = FALSE
            NomCta1:SCREEN-VALUE      = ""
            W_CtaContab:SCREEN-VALUE = ""
            W_CtaContab.

     RUN C-Cuentas.R (OUTPUT W_CtaContab,OUTPUT NomCta1, OUTPUT W_Nat, OUTPUT W_Ctr,                         
                      INPUT  2).  

     FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ W_CtaContab
                         AND Cuentas.Tipo   EQ 2
                         AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
     IF AVAIL(Cuentas) THEN
        ASSIGN NomCta1:SCREEN-VALUE     = Cuentas.Nombre
               W_CtaContab:SCREEN-VALUE = W_CtaContab
               NomCta1:SCREEN-VALUE = Cuentas.Nombre
               CtaAnex_Det = Cuentas.Cuenta
               NatAnex_Det = Cuentas.Naturaleza.
     ELSE W_CtaContab = "".

     ASSIGN W-Imp_MovCont:SENSITIVE = TRUE.  
     W-Imp_MovCont:MOVE-TO-TOP(). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CtaContra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CtaContra W-Imp_MovCont
ON LEAVE OF W_CtaContra IN FRAME F_Proc /* Cuenta Contrapartida */
DO:
  ASSIGN W_CtaContra
         NomCta2:SCREEN-VALUE = "".

  FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ W_CtaContra
                       AND Cuentas.Tipo   EQ 2
                       AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
  IF AVAIL(Cuentas) THEN
     ASSIGN NomCta2:SCREEN-VALUE = Cuentas.Nombre
            CtaAnex_Tot = Cuentas.Cuenta
            NatAnex_Tot = Cuentas.Naturaleza.
  ELSE APPLY "Mouse-Select-DblClick" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CtaContra W-Imp_MovCont
ON MOUSE-SELECT-DBLCLICK OF W_CtaContra IN FRAME F_Proc /* Cuenta Contrapartida */
DO:
     ASSIGN W-Imp_MovCont:SENSITIVE   = FALSE
            NomCta2:SCREEN-VALUE      = ""
            W_CtaContra:SCREEN-VALUE = ""
            W_CtaContra.

     RUN C-Cuentas.R (OUTPUT W_CtaContra,OUTPUT NomCta2, OUTPUT W_Nat, OUTPUT W_Ctr,                         
                      INPUT  2).  

     FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ W_CtaContra
                         AND Cuentas.Tipo    EQ 2
                         AND Cuentas.Estado  EQ 1 NO-LOCK NO-ERROR.
     IF AVAIL(Cuentas) THEN
        ASSIGN NomCta2:SCREEN-VALUE     = Cuentas.Nombre
               W_CtaContra:SCREEN-VALUE = W_CtaContra.
     ELSE W_CtaContra = "".

     ASSIGN W-Imp_MovCont:SENSITIVE = TRUE. 
     W-Imp_MovCont:MOVE-TO-TOP(). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CtaIngrBen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CtaIngrBen W-Imp_MovCont
ON LEAVE OF W_CtaIngrBen IN FRAME F_Proc /* Cuenta Ingresos X Benef. */
DO:
  ASSIGN W_CtaIngrBen
         NomBen:SCREEN-VALUE = "".

  FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ W_CtaIngrBen
                       AND Cuentas.Tipo   EQ 2
                       AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
  IF AVAIL(Cuentas) THEN
     NomBen:SCREEN-VALUE = Cuentas.Nombre.
  ELSE APPLY "Mouse-Select-DblClick" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CtaIngrBen W-Imp_MovCont
ON MOUSE-SELECT-DBLCLICK OF W_CtaIngrBen IN FRAME F_Proc /* Cuenta Ingresos X Benef. */
DO:
   ASSIGN W-Imp_MovCont:SENSITIVE   = FALSE
          NomBen:SCREEN-VALUE       = ""
          W_CtaIngrBen:SCREEN-VALUE = ""
          W_CtaIngrBen.
                                                                                                               
   RUN C-Cuentas.R (OUTPUT W_CtaIngrBen,OUTPUT NomBen, OUTPUT W_Nat, OUTPUT W_Ctr,                         
                    INPUT  2).  

   FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ W_CtaIngrBen
                       AND Cuentas.Tipo   EQ 2
                       AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
   IF AVAIL(Cuentas) THEN
      ASSIGN NomBen:SCREEN-VALUE       = Cuentas.Nombre
             W_CtaIngrBen:SCREEN-VALUE = W_CtaIngrBen.
   ELSE W_CtaIngrBen = "".
                                                                                                               
   ASSIGN W-Imp_MovCont:SENSITIVE = TRUE.  
   W-Imp_MovCont:MOVE-TO-TOP(). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CtaIngreso
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CtaIngreso W-Imp_MovCont
ON LEAVE OF W_CtaIngreso IN FRAME F_Proc /* Cuenta Ingresos X Interés */
DO:
  ASSIGN W_CtaIngreso
         NomIng:SCREEN-VALUE = "".

  FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ W_CtaIngreso
                       AND Cuentas.Tipo   EQ 2
                       AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
  IF AVAIL(Cuentas) THEN
     NomIng:SCREEN-VALUE = Cuentas.Nombre.
  ELSE APPLY "Mouse-Select-DblClick" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CtaIngreso W-Imp_MovCont
ON MOUSE-SELECT-DBLCLICK OF W_CtaIngreso IN FRAME F_Proc /* Cuenta Ingresos X Interés */
DO:
   ASSIGN W-Imp_MovCont:SENSITIVE   = FALSE
          NomIng:SCREEN-VALUE       = ""
          W_CtaIngreso:SCREEN-VALUE = ""
          W_CtaIngreso.
                                                                                                               
   RUN C-Cuentas.R (OUTPUT W_CtaIngreso,OUTPUT NomIng, OUTPUT W_Nat, OUTPUT W_Ctr,                         
                    INPUT  2).  

   FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ W_CtaIngreso
                       AND Cuentas.Tipo   EQ 2
                       AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
   IF AVAIL(Cuentas) THEN
      ASSIGN NomIng:SCREEN-VALUE       = Cuentas.Nombre
             W_CtaIngreso:SCREEN-VALUE = W_CtaIngreso.
   ELSE W_CtaIngreso = "".
                                                                                                               
   ASSIGN W-Imp_MovCont:SENSITIVE = TRUE.  
   W-Imp_MovCont:MOVE-TO-TOP(). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CtaInteres
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CtaInteres W-Imp_MovCont
ON LEAVE OF W_CtaInteres IN FRAME F_Proc /* Cuenta Interés por Cobrar */
DO:
  ASSIGN W_CtaInteres
         NomInt:SCREEN-VALUE = "".

  FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ W_CtaInteres
                       AND Cuentas.Tipo   EQ 2
                       AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
  IF AVAIL(Cuentas) THEN
     NomInt:SCREEN-VALUE = Cuentas.Nombre.
  ELSE APPLY "Mouse-Select-DblClick" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CtaInteres W-Imp_MovCont
ON MOUSE-SELECT-DBLCLICK OF W_CtaInteres IN FRAME F_Proc /* Cuenta Interés por Cobrar */
DO:
   ASSIGN W-Imp_MovCont:SENSITIVE   = FALSE
          NomInt:SCREEN-VALUE       = ""
          W_CtaInteres:SCREEN-VALUE = ""
          W_CtaInteres.
                                                                                                               
   RUN C-Cuentas.R (OUTPUT W_CtaInteres,OUTPUT NomInt, OUTPUT W_Nat, OUTPUT W_Ctr,                         
                    INPUT  2).  

   FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ W_CtaInteres
                       AND Cuentas.Tipo   EQ 2
                       AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
   IF AVAIL(Cuentas) THEN
      ASSIGN NomInt:SCREEN-VALUE       = Cuentas.Nombre
             W_CtaInteres:SCREEN-VALUE = W_CtaInteres.
   ELSE W_CtaInteres = "".
                                                                                                               
   ASSIGN W-Imp_MovCont:SENSITIVE = TRUE.
   W-Imp_MovCont:MOVE-TO-TOP(). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CtaIva
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CtaIva W-Imp_MovCont
ON LEAVE OF W_CtaIva IN FRAME F_Proc /* Cuenta de Iva */
DO:
  ASSIGN W_CtaIva
         NomIva:SCREEN-VALUE = "".

  FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ W_CtaIva
                       AND Cuentas.Tipo   EQ 2
                       AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
  IF AVAIL(Cuentas) THEN
     NomIva:SCREEN-VALUE = Cuentas.Nombre.
  ELSE APPLY "Mouse-Select-DblClick" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CtaIva W-Imp_MovCont
ON MOUSE-SELECT-DBLCLICK OF W_CtaIva IN FRAME F_Proc /* Cuenta de Iva */
DO:
   ASSIGN W-Imp_MovCont:SENSITIVE   = FALSE
          NomIva:SCREEN-VALUE       = ""
          W_CtaIva:SCREEN-VALUE = ""
          W_CtaIva.
                                                                                                               
   RUN C-Cuentas.R (OUTPUT W_CtaIva,OUTPUT NomIva, OUTPUT W_Nat, OUTPUT W_Ctr,                         
                    INPUT  2).  

   FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ W_CtaIva
                       AND Cuentas.Tipo   EQ 2
                       AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
   IF AVAIL(Cuentas) THEN
      ASSIGN NomIva:SCREEN-VALUE   = Cuentas.Nombre
             W_CtaIva:SCREEN-VALUE = W_CtaIva.
   ELSE W_CtaIva = "".
                                                                                                               
   ASSIGN W-Imp_MovCont:SENSITIVE = TRUE.
   W-Imp_MovCont:MOVE-TO-TOP(). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_NitCont
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_NitCont W-Imp_MovCont
ON LEAVE OF W_NitCont IN FRAME F_Proc
DO:
  ASSIGN W_NitCont
         NomCte:SCREEN-VALUE = "".

  FIND FIRST Clientes WHERE Clientes.Nit    EQ W_NitCont
                        AND Clientes.Estado EQ 1 NO-LOCK NO-ERROR.
  IF NOT AVAIL(Clientes) THEN
     APPLY "Mouse-Select-DblClick" TO SELF.
  ELSE
     ASSIGN W_NitCont:SCREEN-VALUE = W_NitCont                                                      
            NomCte:SCREEN-VALUE    = TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2) +   
                                    " " + TRIM(Clientes.Nombre).                                  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_NitCont W-Imp_MovCont
ON MOUSE-SELECT-DBLCLICK OF W_NitCont IN FRAME F_Proc
DO:
  DEFI VAR W_AgeC LIKE Agencias.Agencia.

  ASSIGN W-Imp_MovCont:SENSITIVE = FALSE.                                                           
                                                                                                    
  RUN C-Clientes.R (INPUT  1,W_Agencia,                                                           
                    OUTPUT W_NitCont, OUTPUT NomCte, OUTPUT NomCte, OUTPUT W_AgeC).       
                                                                                                    
  ASSIGN W-Imp_MovCont:SENSITIVE = TRUE
         W_NitCont:SCREEN-VALUE  = W_NitCont                                                      
         NomCte:SCREEN-VALUE     = NomCte.

  W-Imp_MovCont:MOVE-TO-TOP().

  FIND FIRST Clientes WHERE Clientes.Nit    EQ W_NitCont
                        AND Clientes.Estado EQ 1 NO-LOCK NO-ERROR.
  IF AVAIL(Clientes) THEN                          
     ASSIGN W_NitCont:SCREEN-VALUE = W_NitCont                                                      
            NomCte:SCREEN-VALUE    = TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2) +   
                                    " " + TRIM(Clientes.Nombre).                                  
  ELSE DO:                                                                                           
     MESSAGE "El Nit debe existir Activo en Clientes." VIEW-AS ALERT-BOX.    
     ASSIGN W_NitCont           = ""
            NomCte:SCREEN-VALUE = "".
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Imp_MovCont 


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

  FOR EACH Comprobantes WHERE Comprobantes.Agencia         EQ W_Agencia
                          AND Comprobantes.Estado          EQ 1
                          AND NOT Comprobantes.Id_Efectivo        NO-LOCK:
      Cmb_Cptes:ADD-LAST(STRING(Comprobantes.Comprobante,"999") + "-" + 
                         STRING(Comprobantes.Nombre,"X(25)")).
  END.

  W_CmbOfi:ADD-LAST("000-Consolidado").
  ASSIGN W_CmbOfi:SCREEN-VALUE = "000-Consolidado"
         W_OfiIni              = 1
         W_OfiFin              = 999.

  FOR EACH Agencias WHERE Agencias.Estado  EQ 1
                      AND Agencias.Agencia GT 0 NO-LOCK:
      W_CmbOfi:ADD-LAST(STRING(Agencias.Agencia,"999") + "-" + STRING(Agencias.Nombre,"X(25)")).
  END. 

  W-Imp_MovCont:MOVE-TO-TOP(). 

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ContabIngreso W-Imp_MovCont 
PROCEDURE ContabIngreso :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFI VAR W_Neto LIKE Tmp.VIng.

   ASSIGN W_Neto = ROUND(Tmp.VIng / 1.16,0).

   CREATE Mov_Contable.
   ASSIGN Mov_Contable.Agencia           = Tmp.Age
             Mov_Contable.Cuenta         = W_CtaIngrBen        
             Mov_Contable.Nit            = Tmp.Ced     
             Mov_Contable.Fec_Contable   = W_Fecha                     
             Mov_Contable.Comentario     = "Por Benef.Coomeva"    
             Mov_Contable.Usuario        = W_Usuario                   
             Mov_Contable.Cen_Costos     = W_Cencosgral                
             Mov_Contable.Destino        = W_Agencia                   
             Mov_Contable.Comprobante    = Comprobantes.Comprobante    
             Mov_Contable.Num_Documento  = Comprobantes.Secuencia      
             Mov_Contable.Fec_Grabacion  = TODAY                       
             Mov_Contable.Hora           = TIME                        
             Mov_Contable.Estacion       = W_Estacion
             Mov_Contable.Cr             = W_Neto.

   CREATE Mov_Contable.
   ASSIGN Mov_Contable.Agencia           = Tmp.Age
             Mov_Contable.Cuenta         = W_CtaIva        
             Mov_Contable.Nit            = Tmp.Ced     
             Mov_Contable.Fec_Contable   = W_Fecha                     
             Mov_Contable.Comentario     = "Por Benef.Coomeva"    
             Mov_Contable.Usuario        = W_Usuario                   
             Mov_Contable.Cen_Costos     = W_Cencosgral                
             Mov_Contable.Destino        = W_Agencia                   
             Mov_Contable.Comprobante    = Comprobantes.Comprobante    
             Mov_Contable.Num_Documento  = Comprobantes.Secuencia      
             Mov_Contable.Fec_Grabacion  = TODAY                       
             Mov_Contable.Hora           = TIME                        
             Mov_Contable.Estacion       = W_Estacion
             Mov_Contable.Cr             = Tmp.VIng - W_Neto.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ContabSyA W-Imp_MovCont 
PROCEDURE ContabSyA :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   CREATE Mov_Contable.
   ASSIGN Mov_Contable.Cuenta         = "19049501"       
          Mov_Contable.Nit            = "010"                   
          Mov_Contable.Fec_Contable   = W_Fecha                        
          Mov_Contable.Comentario     = "Por Cobro-Coomeva"        
          Mov_Contable.Usuario        = W_Usuario                      
          Mov_Contable.Cen_Costos     = W_Cencosgral                   
          Mov_Contable.Destino        = W_Agencia                      
          Mov_Contable.Comprobante    = Comprobantes.Comprobante       
          Mov_Contable.Num_Documento  = Comprobantes.Secuencia         
          Mov_Contable.Fec_Grabacion  = TODAY                          
          Mov_Contable.Hora           = TIME                           
          Mov_Contable.Estacion       = W_Estacion.                

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Imp_MovCont  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Imp_MovCont)
  THEN DELETE WIDGET W-Imp_MovCont.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Imp_MovCont  _DEFAULT-ENABLE
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
  DISPLAY Cmb_Cptes W_CmbOfi Rs_DbCr W_CtaContab NomCta1 W_CtaContra NomCta2 
          Tg_Coomeva Rs_DetTot W_CtaIngrBen NomBen W_NitCont NomCte W_CtaIngreso 
          NomIng W_NroImp W_VrImp W_CtaInteres F_Comentario NomInt W_CtaIva 
          NomIva 
      WITH FRAME F_Proc IN WINDOW W-Imp_MovCont.
  ENABLE Cmb_Cptes W_CmbOfi Rs_DbCr W_CtaContab W_CtaContra Tg_Coomeva 
         Rs_DetTot W_NitCont F_Comentario W_CtaIva Btn_Importar 
         Btn_Contabilizar Btn_FinImp RECT-318 RECT-319 RECT-320 RECT-321 
      WITH FRAME F_Proc IN WINDOW W-Imp_MovCont.
  {&OPEN-BROWSERS-IN-QUERY-F_Proc}
  VIEW W-Imp_MovCont.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Importar W-Imp_MovCont 
PROCEDURE Importar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR Archivo AS CHARACTER FORMAT "X(40)".
  DEFINE VAR Datos   AS CHARACTER FORMAT "X(80)".

  SESSION:SET-WAIT-STATE("GENERAL").
    
  FIND FIRST Tmp NO-ERROR.
  IF AVAIL(Tmp) THEN DO:
     MESSAGE "Existen Registros en la Temporal, pendientes por contabilizar..." SKIP
             "Desea Borrarlos......Teclee SI," SKIP
             "Desea adicionarles la nueva importación, Teclee NO," SKIP
             "No ejecutar ninguna acción, Teclee CANCEL."
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL TITLE "" UPDATE W_RptaA AS LOGICAL.
     IF W_RptaA THEN DO: 
        FOR EACH Tmp: DELETE Tmp. END.
        ASSIGN W_NroImp = 0
               W_VrImp  = 0
               W_NroImp:SCREEN-VALUE IN FRAME F_Proc = "0"
               W_VrImp:SCREEN-VALUE  = "0".
     END.
     ELSE IF NOT W_RptaA THEN.
     ELSE RETURN.
  END.

  SYSTEM-DIALOG GET-FILE Archivo
     TITLE      "Selecc.el Archivo...Diseño: Céd.X(12),Vlr-9999999999.99,Ag.999,IngBenef.9999999999"
     FILTERS    "Archivos Txt (*.Txt)"   "*.*"
     MUST-EXIST
     USE-FILENAME.
     
  INPUT FROM VALUE(Archivo).
  REPEAT:
    IMPORT UNFORMATTED Datos NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
       MESSAGE "El Archivo tiene Problemas...Revise por favor" VIEW-AS ALERT-BOX.
       LEAVE.
    END.

    CREATE Tmp.
    ASSIGN Tmp.Ced  = SUBSTRING(Datos,1,12)
           Tmp.Vr   = DEC(SUBSTRING(Datos,13,11))
           Tmp.Vr   = Tmp.Vr + (DEC(SUBSTRING(Datos,25,2)) / 100)
           Tmp.Age  = INTEG(SUBSTRING(Datos,27,3)) 
           Tmp.VIng = DEC(SUBSTRING(Datos,30,10))
           W_NroImp = W_NroImp + 1
           W_VrImp  = W_VrImp + Tmp.Vr
           W_NroImp:SCREEN-VALUE IN FRAME F_Proc = STRING(W_NroImp)
           W_VrImp:SCREEN-VALUE  = STRING(W_VrImp).

    FIND FIRST Clientes WHERE Clientes.Nit         EQ Tmp.Ced
                          AND Clientes.Estado      EQ 1 NO-LOCK NO-ERROR.                            
    IF NOT AVAIL(Clientes) OR Tmp.Vr EQ 0 THEN 
       MESSAGE "La cédula/Nit : " Tmp.Ced ", no existe como tercero, O" SKIP
               "El valor es 0(Cero)...Revise por favor.               " SKIP
               "Porque puede quedar en una agencia incorrecta!!"  VIEW-AS ALERT-BOX.
    ELSE
       Tmp.Age  = Clientes.Agencia.
  END.

  SESSION:SET-WAIT-STATE("").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proceso_Coomeva W-Imp_MovCont 
PROCEDURE Proceso_Coomeva :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
 ------------------------------------------------------------------------------*/
  DEFI VAR Xdb      LIKE Mov_Contable.Db  INIT 0.
  DEFI VAR Xcr      LIKE Mov_Contable.Db  INIT 0.  

  FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ W_CtaIngreso
                       AND Cuentas.Tipo   EQ 2
                       AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
  IF AVAIL(Cuentas) THEN DO:
     FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ W_CtaInteres
                       AND Cuentas.Tipo      EQ 2
                       AND Cuentas.Estado    EQ 1 NO-LOCK NO-ERROR.
     IF AVAIL(Cuentas) THEN DO:
        FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ W_CtaIngrBen
                       AND Cuentas.Tipo         EQ 2
                       AND Cuentas.Estado       EQ 1 NO-LOCK NO-ERROR.
        
        IF AVAIL(Cuentas) THEN
           FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ W_CtaIva
                          AND Cuentas.Tipo         EQ 2
                          AND Cuentas.Estado       EQ 1 NO-LOCK NO-ERROR.
     END.
  END.

  IF NOT AVAIL(Cuentas) THEN DO:
     MESSAGE "Las Cuentas de Ingresos, Iva e Intereses son indispensables y" SKIP
             "                      Activas en el PUC...Proceso cancelado."
         VIEW-AS ALERT-BOX ERROR.
     RETURN ERROR.
  END.

  FIND Entidad WHERE Entidad.Entidad EQ W_Entidad NO-LOCK NO-ERROR.
  IF AVAILABLE Entidad THEN DO:
     FIND FIRST Indicadores WHERE Indicadores.Indicador EQ Entidad.Ind_Usura
                              AND Indicadores.Estado    EQ 1 
                              AND Indicadores.FecVcto   GE W_Fecha NO-LOCK NO-ERROR.
     IF AVAILABLE Indicadores THEN
        W_TasaUs = Indicadores.Tasa.
     ELSE DO:
        FIND LAST Indicadores WHERE Indicadores.Indicador  EQ Entidad.Ind_Usura
                                AND Indicadores.Estado     EQ 1 NO-LOCK NO-ERROR.
        IF AVAILABLE Indicadores THEN
           W_TasaUs = Indicadores.Tasa.
        ELSE
           W_TasaUS = 0.
     END.
     
     IF W_TasaUS GT 0 THEN DO:
        RUN EFNV IN W_ManFin  (INPUT W_TasaUs / 100, 12, OUTPUT W_TasaUS).
        W_TasaUS = W_TasaUS.
     END.
     ELSE DO:
        MESSAGE "No hay Tasa de Usura, por lo tanto no hay Liquidación de Intereses-Coomeva." 
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
     END.
  END.

  FOR EACH Anexos WHERE Anexos.Agencia    GE W_OfiIni    AND
                        Anexos.Agencia    LE W_OfiFin    AND
                        Anexos.Cuenta     EQ W_CtaContab AND
                        Anexos.Ano        EQ YEAR(W_Fecha) NO-LOCK
           BREAK BY Anexos.Agencia BY Anexos.Nit:
      IF FIRST-OF(Anexos.Agencia) THEN DO:
          FIND FIRST Comprobantes WHERE Comprobantes.Agencia     EQ Anexos.Agencia
                                    AND Comprobantes.Comprobante EQ W_Cpte
                                    AND Comprobantes.Estado      EQ 1 NO-ERROR.
          IF AVAIL(Comprobantes) THEN DO:
             ASSIGN Comprobantes.Secuencia = Comprobantes.Secuencia + 1.
             FIND CURRENT Comprobantes NO-LOCK NO-ERROR.
          END.
          ELSE DO:
             MESSAGE "Falta El Comprobante activo para la Contabilización."
                 VIEW-AS ALERT-BOX.
             RETURN.
          END.
      END.
      IF FIRST-OF(Anexos.Nit) THEN DO:
         RUN HallarSdoTercero IN W_Manija (INPUT Anexos.Nit, INPUT Anexos.Agencia, Anexos.Agencia,
             INPUT 0, 999, W_CtaContab, YEAR(W_Fecha), MONTH(W_Fecha),
             OUTPUT Xdb, OUTPUT Xcr, OUTPUT Xsdo).
         IF XSdo GT 0 THEN DO:
            RUN GrabarContable.         /*Proc.al final de este*/
         END.
      END.  
  END.

  MESSAGE "Terminó la Contabilización de los Intereses-Coomeva"
      VIEW-AS ALERT-BOX INFO BUTTONS OK.

END PROCEDURE.

PROCEDURE GrabarContable.

   CREATE Mov_Contable.
   ASSIGN Mov_Contable.Agencia           = Anexos.Agencia
             Mov_Contable.Cuenta         = W_CtaInteres         
             Mov_Contable.Nit            = Anexos.Nit     
             Mov_Contable.Fec_Contable   = W_Fecha                     
             Mov_Contable.Comentario     = "Cargo-Intereses"    
             Mov_Contable.Usuario        = W_Usuario                   
             Mov_Contable.Cen_Costos     = W_Cencosgral                
             Mov_Contable.Destino        = W_Agencia                   
             Mov_Contable.Comprobante    = Comprobantes.Comprobante    
             Mov_Contable.Num_Documento  = Comprobantes.Secuencia      
             Mov_Contable.Fec_Grabacion  = TODAY                       
             Mov_Contable.Hora           = TIME                        
             Mov_Contable.Estacion       = W_Estacion
             Mov_Contable.Db             = ROUND(Xsdo * W_TasaUS,0).

   CREATE Mov_Contable.
   ASSIGN Mov_Contable.Agencia           = Anexos.Agencia
             Mov_Contable.Cuenta         = W_CtaIngreso        
             Mov_Contable.Nit            = Anexos.Nit     
             Mov_Contable.Fec_Contable   = W_Fecha                     
             Mov_Contable.Comentario     = "Cargo-Interés Coomeva"    
             Mov_Contable.Usuario        = W_Usuario                   
             Mov_Contable.Cen_Costos     = W_Cencosgral                
             Mov_Contable.Destino        = W_Agencia                   
             Mov_Contable.Comprobante    = Comprobantes.Comprobante    
             Mov_Contable.Num_Documento  = Comprobantes.Secuencia      
             Mov_Contable.Fec_Grabacion  = TODAY                       
             Mov_Contable.Hora           = TIME                        
             Mov_Contable.Estacion       = W_Estacion
             Mov_Contable.Cr             = ROUND(Xsdo * W_TasaUS,0).

END PROCE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SoloAnexos W-Imp_MovCont 
PROCEDURE SoloAnexos :
DEFINE VAR TotAge LIKE Ahorros.Sdo_Disponible.
  IF CtaAnex_Det EQ "" OR NatAnex_Det EQ "" OR CtaAnex_Tot EQ "" OR NatAnex_Tot EQ "" OR
     W_NitCont:SCREEN-VALUE IN FRAME F_Proc EQ "" THEN DO:
     MESSAGE "Falta por ingresar alguna cuenta o nit nit contrapartida no ingresado" SKIP
             "Revise todos los campos e intente de nuevo." VIEW-AS ALERT-BOX.
     RETURN ERROR.
  END.
  FIND FIRST Tmp NO-LOCK NO-ERROR.
  IF NOT AVAILABLE Tmp THEN DO:
     MESSAGE "No se ha realizado el proceso de importación" SKIP
             "haga clic en el boton IMPORTAR y vuelva a intentarlo" VIEW-AS ALERT-BOX.
     RETURN ERROR.
  END.
  FOR EACH Tmp /*WHERE tmp.ced EQ "43681205"*/ BREAK BY Tmp.Age:
      FIND Anexos WHERE Anexos.Agencia  EQ Tmp.Age         
                    AND Anexos.Cen_Costos EQ 999
                    AND Anexos.Cuenta     EQ CtaAnex_Det
                    AND Anexos.Nit        EQ Tmp.Ced
                    AND Anexos.Ano        EQ YEAR(W_Fecha)
                  EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
       IF NOT AVAILABLE(Anexos) THEN
          IF LOCKED(Anexos) THEN NEXT.
          ELSE DO:
            CREATE Anexos.
            ASSIGN Anexos.Agencia    = Tmp.Age
                   Anexos.Nit        = Tmp.Ced
                   Anexos.Cuenta     = CtaAnex_Det
                   Anexos.Ano        = YEAR(W_Fecha)
                   Anexos.Cen_Costos = 999.
          END.    
        IF NatAnex_Det EQ "DB" THEN
           Anexos.Db[MONTH(W_Fecha)] = Anexos.Db[MONTH(W_Fecha)] + Tmp.Vr.
        IF NatAnex_Det EQ "CR" THEN
           Anexos.Cr[MONTH(W_Fecha)] = Anexos.Cr[MONTH(W_Fecha)] + Tmp.Vr.
        TotAge = TotAge + Tmp.Vr.
        /*lleva al nit contrapartida global el total de lo importado*/
        IF LAST-OF(Tmp.Age) THEN DO:
            FIND Anexos WHERE Anexos.Agencia  EQ Tmp.Age
                          AND Anexos.Cen_Costos EQ 999
                          AND Anexos.Cuenta     EQ CtaAnex_Tot
                          AND Anexos.Nit        EQ W_NitCont:SCREEN-VALUE IN FRAME F_Proc
                          AND Anexos.Ano        EQ YEAR(W_Fecha)
                        EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
             IF NOT AVAILABLE(Anexos) THEN
                IF LOCKED(Anexos) THEN NEXT.
                ELSE DO:
                  CREATE Anexos.
                  ASSIGN Anexos.Agencia    = Tmp.Age
                         Anexos.Nit        = W_NitCont:SCREEN-VALUE IN FRAME F_Proc
                         Anexos.Cuenta     = CtaAnex_Tot
                         Anexos.Ano        = YEAR(W_Fecha)
                         Anexos.Cen_Costos = 999.
                END.    

              IF Rs_DbCr EQ 2 THEN
                 Anexos.Db[MONTH(W_Fecha)] = Anexos.Db[MONTH(W_Fecha)] + TotAge.
              ELSE
                 Anexos.Cr[MONTH(W_Fecha)] = Anexos.Cr[MONTH(W_Fecha)] + TotAge.
              TotAge = 0.
        END.
   END.
   RELEASE Anexos.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

