  DEFINE INPUT  PARAMETER P_Oficina LIKE Oficina.Oficina.
  DEFINE OUTPUT PARAMETER P_CenCos LIKE Cen_Costos.Cen_Costo.
  DEFINE VARIABLE W_Codigo AS INTEGER FORMAT "999":U 
     LABEL "C¢digo" 
     VIEW-AS FILL-IN 
     SIZE 4.8 BY .81
     BGCOLOR 15  NO-UNDO.

  DEFINE VARIABLE W_Nombre AS CHARACTER FORMAT "X(20)":U 
     LABEL "Nombre" 
     VIEW-AS FILL-IN 
     SIZE 19 BY .81
     BGCOLOR 15  NO-UNDO.

  DEFINE RECTANGLE RECT-31
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 33 BY 5.12.

  DEFINE BUTTON Btn_TerminarCon LABEL "&Terminar Consulta" SIZE 20 BY 0.81 FONT 4.       
  DEFINE QUERY Qry_Costos FOR Cen_Costos FIELDS (Cen_Costos Nombre) SCROLLING.
  DEFINE BROWSE Brw_Costos QUERY Qry_Costos
     DISPLAY Cen_Costos.Cen_Costos Cen_Costos.Nombre
     WITH NO-ASSIGN SEPARATORS SIZE 40 BY 5.12 BGCOLOR 15 FONT 4
          LABEL-FGCOLOR 0 LABEL-BGCOLOR 8. 

  DEFINE FRAME Frm_Costos
     Brw_Costos  AT ROW 1 COL 1
     W_Codigo    AT ROW 3.15 COL 46 COLON-ALIGNED
     W_Nombre    AT ROW 4.5 COL 46 COLON-ALIGNED
     RECT-31 AT ROW 1 COL 38
  WITH  1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 16.50 ROW 5.80 
         SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 FONT 4.
     
  DEFINE FRAME Frm_Consulta
     Btn_TerminarCon  AT ROW 6.65 COL 25 COLON-ALIGNED
  WITH  TITLE "Consulta de Centro de Costos" 1 DOWN  KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 14.80 ROW 4.80 
         SIZE 74 BY 7.58 SCROLLABLE 
         BGCOLOR 7 FGCOLOR 15 FONT 4.
      
  ON LEAVE OF W_Codigo IN FRAME Frm_Costos
  OR RETURN OF W_Codigo DO:
    ASSIGN W_Codigo.
    FIND Cen_Costos WHERE Cen_Costos.Cen_Costos = W_Codigo AND
                          Cen_Costos.Cen_Costos = P_Oficina NO-ERROR.
    IF AVAILABLE (Cen_Costos) THEN
      DO:
        REPOSITION Qry_Costos TO ROWID ROWID (Cen_Costos).
        P_CenCos = Cen_Costos.Cen_Costo.
        {brschnge.i}.
        APPLY "ENTRY" TO Btn_TerminarCon IN FRAME Frm_Consulta.
        RETURN NO-APPLY.
      END.
  END.

  ON LEAVE OF W_Nombre IN FRAME Frm_Costos
  OR RETURN OF W_Nombre DO:
    ASSIGN W_Nombre.
    FIND FIRST Cen_Costos WHERE Cen_Costos.Nombre BEGINS W_Nombre AND
                                Cen_Costos.Cen_Costos = P_Oficina NO-ERROR.
    IF AVAILABlE (Cen_Costos) THEN
      DO:
        REPOSITION Qry_Costos TO ROWID ROWID(Cen_Costos).
        P_CenCos = Cen_Costos.Cen_Costo.
       {brschnge.i}.
        APPLY "ENTRY" TO Btn_TerminarCon IN FRAME Frm_Consulta.
        RETURN NO-APPLY.
      END.     
  END.

  ON VALUE-CHANGED OF Brw_Costos IN FRAME Frm_Costos
  DO:
    P_CenCos = Cen_Costos.Cen_Costo.
    {brschnge.i}
  END.

  ON CHOOSE OF Btn_TerminarCon
  DO:
    HIDE FRAME Frm_Costos.
    HIDE FRAME Frm_Consulta.
  END.
  
  IF SELF:NAME = "W_Nombre" THEN
     OPEN QUERY Qry_Costos FOR EACH Cen_Costos WHERE Cen_costos.Nombre BEGINS W_Nombre  AND
                                                     Cen_costos.Oficina = P_Oficina NO-LOCK.
  ELSE
     OPEN QUERY Qry_Costos FOR EACH Cen_Costos WHERE Cen_Costos.Oficina = P_Oficina NO-LOCK.
  ENABLE ALL WITH FRAME Frm_consulta.
  ENABLE ALL WITH FRAME Frm_Costos.
  APPLY "VALUE-CHANGED" TO Brw_Costos.
  WAIT-FOR CHOOSE OF Btn_TerminarCon.  