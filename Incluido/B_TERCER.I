  DEFINE INPUT  PARAMETER P_Oficina LIKE Oficinas.Oficina.
  DEFINE OUTPUT PARAMETER P_Nit LIKE Terceros.Nit.
  DEFINE VARIABLE W_Apellido AS CHARACTER FORMAT "X(20)":U 
     LABEL "Apellido" 
     VIEW-AS FILL-IN 
     SIZE 19 BY .81
     BGCOLOR 15  NO-UNDO.

  DEFINE VARIABLE W_Nombre AS CHARACTER FORMAT "X(20)":U 
     LABEL "Nombre" 
     VIEW-AS FILL-IN 
     SIZE 19 BY .81
     BGCOLOR 15  NO-UNDO.

  DEFINE VARIABLE W_Tercero AS CHARACTER FORMAT "X(12)":U 
     LABEL "Nit" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .81
     BGCOLOR 15  NO-UNDO.

  DEFINE RECTANGLE RECT-31
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 33 BY 5.12.

  DEFINE BUTTON Btn_TerminarCon LABEL "&Terminar Consulta" SIZE 20 BY 0.81 FONT 4.       
  DEFINE QUERY Qry_Tercero FOR Terceros FIELDS (Nit Nombre Apellido) SCROLLING.
  DEFINE BROWSE Brw_Tercero QUERY Qry_Tercero
     DISPLAY Terceros.Nit Terceros.Nombre Terceros.Apellido
     WITH NO-ASSIGN SEPARATORS SIZE 40 BY 5.12 BGCOLOR 15 FONT 4
          LABEL-FGCOLOR 0 LABEL-BGCOLOR 8. 

  DEFINE FRAME Frm_Tercero
     Brw_Tercero AT ROW 1 COL 1
     W_Tercero   AT ROW 1.81 COL 46 COLON-ALIGNED
     W_Apellido  AT ROW 3.15 COL 46 COLON-ALIGNED
     W_Nombre    AT ROW 4.5 COL 46 COLON-ALIGNED
     RECT-31 AT ROW 1 COL 38
  WITH  1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 16.50 ROW 5.80 
         SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 FONT 4.
     
  DEFINE FRAME Frm_Consulta
     Btn_TerminarCon  AT ROW 6.65 COL 25 COLON-ALIGNED
  WITH  TITLE "Consulta de Terceros" 1 DOWN  KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 14.80 ROW 4.80 
         SIZE 74 BY 7.58 SCROLLABLE 
         BGCOLOR 7 FGCOLOR 15 FONT 4.
      
  ON LEAVE OF W_Apellido IN FRAME Frm_Tercero
  OR RETURN OF W_Apellido DO:
    ASSIGN W_Apellido.
    FIND FIRST Terceros WHERE Terceros.Apellido BEGINS W_Apellido AND
                              Terceros.Oficina = P_Oficina NO-ERROR.
    IF AVAILABLE (Terceros) THEN
      DO:
        REPOSITION Qry_Tercero TO ROWID ROWID (Terceros).
        P_NIT = Tercero.Nit.
        {brschnge.i}.
        APPLY "ENTRY" TO Btn_TerminarCon IN FRAME Frm_Consulta.
        RETURN NO-APPLY.
      END.
  END.

  ON LEAVE OF W_Nombre IN FRAME Frm_Tercero
  OR RETURN OF W_Nombre DO:
    ASSIGN W_Nombre.
    FIND FIRST Terceros WHERE Terceros.Nombre BEGINS W_Nombre AND
                              Terceros.Oficina = P_Oficina NO-ERROR.
    IF AVAILABlE (Terceros) THEN
      DO:
        REPOSITION Qry_Tercero TO ROWID ROWID(Terceros).
        P_NIT = Tercero.Nit.
        {brschnge.i}.
        APPLY "ENTRY" TO Btn_TerminarCon IN FRAME Frm_Consulta.
        RETURN NO-APPLY.
      END.     
  END.

  ON LEAVE OF W_Tercero IN FRAME Frm_Tercero
  OR RETURN OF W_Tercero DO:
    ASSIGN W_Tercero.
    FIND Terceros WHERE Terceros.Nit =  W_Tercero and Terceros.Oficina = P_Oficina NO-ERROR.
    IF AVAILABLE (Terceros) THEN
     DO:
      REPOSITION Qry_Tercero TO ROWID ROWID(Terceros).
      P_NIT = Tercero.Nit.
      {brschnge.i}.
      APPLY "ENTRY" TO Btn_TerminarCon IN FRAME Frm_Consulta.
      RETURN NO-APPLY.
     END.
  END.

  ON VALUE-CHANGED OF Brw_Tercero IN FRAME Frm_tercero
  DO:
    P_NIT = Tercero.Nit.
    {brschnge.i}
  END.

  ON CHOOSE OF Btn_TerminarCon
  DO:
    HIDE FRAME Frm_Tercero.
    HIDE FRAME Frm_Consulta.
  END.
  
  IF SELF:NAME = "W_Nombre" THEN
     OPEN QUERY Qry_Tercero FOR EACH Terceros WHERE Terceros.Nombre BEGINS W_Nombre NO-LOCK.
  ELSE
    IF SELF:NAME = "W_Apellido" THEN
      OPEN QUERY Qry_Tercero FOR EACH Terceros WHERE Terceros.Apellido BEGINS W_Apellido NO-LOCK.
    ELSE
      OPEN QUERY Qry_Tercero FOR EACH Terceros WHERE Terceros.Oficina = P_Oficina NO-LOCK.
  ENABLE ALL WITH FRAME Frm_consulta.
  ENABLE ALL WITH FRAME Frm_Tercero.
  APPLY "VALUE-CHANGED" TO Brw_Tercero.
  WAIT-FOR CHOOSE OF Btn_TerminarCon.  
