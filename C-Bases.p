  /**************************************************************************
  PROCEDIMIENTO: Permite consultar los codigos de base de retención y
                 seleccionarlos.  
  FECHA        : 11/02/1998
  **************************************************************************/
  
    DEFINE OUTPUT PARAMETER P_Codbase LIKE Base_Ret.Cod_Base.
    DEFINE OUTPUT PARAMETER P_Nombre  LIKE Base_Ret.Nombre.
    DEFINE OUTPUT PARAMETER P_Porcent LIKE Base_Ret.Porcentaje.
    DEFINE SHARED VAR       W_Manija AS   HANDLE.
    DEFINE        VAR       W_Rpta   AS   LOGICAL.
    DEFINE RECTANGLE RECT-31 EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL SIZE 49 BY 7.2.
    DEFINE BUTTON Btn_TerminarCon LABEL "&Terminar Consulta" SIZE 20 BY 0.81 FONT 4.       
    DEFINE QUERY  Qry_Base FOR Base_Ret FIELDS (Cod_Base Nombre Porcentaje) SCROLLING.
    DEFINE BROWSE Brw_Base QUERY Qry_Base DISPLAY Cod_Base Nombre WITH NO-ASSIGN 
         SEPARATORS SIZE 37 BY 4 BGCOLOR 15 FONT 4 LABEL-FGCOLOR 0 LABEL-BGCOLOR 8. 

    DEFINE VAR W_Codbase LIKE Base_Ret.Cod_Base LABEL "Código" VIEW-AS FILL-IN 
         SIZE 5 BY .81 BGCOLOR 15  NO-UNDO FONT 4.
    DEFINE VAR W_Nombre LIKE Base_Ret.Nombre  LABEL "Nombre"  VIEW-AS FILL-IN 
           SIZE 35 BY .81 BGCOLOR 15  NO-UNDO FONT 4.

    DEFINE FRAME Frm_Cod_Base
       Brw_Base    AT ROW 1.2 COL   8
       W_Codbase       AT ROW 5.5 COL  10 COLON-ALIGNED HELP "Ingrese la Cod_Base que desea Consultar"
       W_Nombre        AT ROW 6.9 COL  10 COLON-ALIGNED HELP "Ingrese el Nombre de Cod_Base que desea Consultar"
       Btn_TerminarCon AT ROW 8.5 COL  13 COLON-ALIGNED HELP "Permite Regresar a la Captura de Información"
       RECT-31 AT ROW 1 COL 1
    WITH  1 DOWN KEEP-TAB-ORDER OVERLAY 
           SIDE-LABELS NO-UNDERLINE THREE-D AT COL 27.5 ROW 5 SIZE 50.5 BY 9.5 
           SCROLLABLE TITLE "Consulta Bases de Retención" BGCOLOR 8 FGCOLOR 0 VIEW-AS DIALOG-BOX FONT 4.

    ON LEAVE OF W_Codbase IN FRAME Frm_Cod_Base OR RETURN OF W_Codbase DO:
      IF LASTKEY = KEYCODE("ENTER") THEN
        DO:      
          ASSIGN W_Codbase.
          FIND Base_Ret WHERE Base_Ret.Cod_Base = W_Codbase NO-LOCK NO-ERROR.
          IF AVAILABLE (Base_Ret) THEN
            DO:
              REPOSITION Qry_Base TO ROWID ROWID(Base_Ret).
              P_Codbase = Base_Ret.Cod_Base.
              P_Nombre = Base_Ret.Nombre. 
              P_Porcent = Base_Ret.Porcentaje.
            END.
          ELSE
            RUN MostrarMensaje IN W_Manija (INPUT 64, OUTPUT W_Rpta).
          APPLY "ENTRY" TO W_Codbase IN FRAME Frm_Cod_Base.
          RETURN NO-APPLY.
        END.
    END.

    ON LEAVE OF W_Nombre IN FRAME Frm_Cod_Base OR RETURN OF W_Nombre DO:
      IF LASTKEY = KEYCODE("ENTER") THEN
        DO:
          ASSIGN W_Nombre.
          FIND FIRST Base_Ret WHERE Base_Ret.Nombre BEGINS W_Nombre NO-LOCK NO-ERROR.
          IF AVAILABLE (Base_Ret) THEN
            DO:
              REPOSITION Qry_Base TO ROWID ROWID(Base_Ret).
              P_Codbase = Base_Ret.Cod_Base.
              P_Nombre = Base_Ret.Nombre. 
              P_Porcent = Base_Ret.Porcentaje.
            END.
          ELSE
            RUN MostrarMensaje IN W_Manija (INPUT 64, OUTPUT W_Rpta).
          APPLY "ENTRY" TO W_Nombre IN FRAME Frm_Cod_Base.
          RETURN NO-APPLY.
        END.
    END.

    ON VALUE-CHANGED OF Brw_Base IN FRAME Frm_Cod_Base
    DO:
      IF AVAILABLE (Base_Ret) THEN
         ASSIGN P_Codbase = Base_Ret.Cod_Base
                P_Nombre = Base_Ret.Nombre.
                P_Porcent = Base_Ret.Porcentaje.
    END.                                   
  
    ON ENTRY OF Btn_TerminarCon DO:
       ON RETURN RETURN.
    END.

    ON LEAVE OF Btn_TerminarCon DO:
       ON RETURN TAB.
    END.

    ON CHOOSE OF Btn_TerminarCon DO:
      HIDE FRAME Frm_Cod_Base.
    END.
                                     
    IF SELF:NAME = "W_Nombre" THEN
       OPEN QUERY Qry_Base FOR EACH Base_Ret WHERE Base_Ret.Nombre BEGINS W_Nombre AND
                                                       Base_Ret.Estado = 1 NO-LOCK.
    ELSE
       OPEN QUERY Qry_Base FOR EACH Base_Ret WHERE Base_Ret.Estado = 1 NO-LOCK.
    REPOSITION Qry_Base TO ROW 1.
    ENABLE ALL WITH FRAME Frm_Cod_Base.
  WAIT-FOR CHOOSE OF Btn_TerminarCon.
  ON RETURN TAB.
