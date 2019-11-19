
{incluido\iprmt_rpt.i}
{incluido\Variable.i "SHARED"}
        
IF P_NomArchivo = "DEFAULT" THEN
   P_NomArchivo = W_Pathspl + "\DESC_MULTI.txt".

OUTPUT TO VALUE(P_NomArchivo) PAGED PAGE-SIZE VALUE(P_NumLineas).
    
{incluido\RepHeader.i}

VIEW FRAME F-Encabezado.
W_Reporte   = "REPORTE   : " + P_Titulo + " (prCtas_Orden_Descuadre_Multi.p) " 
              + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").

DEF VAR tagencia     NO-UNDO LIKE MULTI.mov_contable.Agencia.
DEF VAR tfecha       NO-UNDO LIKE MULTI.mov_contable.fec_contab.
DEF VAR tcomprobante NO-UNDO LIKE MULTI.mov_contable.comprob.
DEF VAR tdocumento   NO-UNDO LIKE MULTI.mov_contable.num_docum.
DEF VAR tvalor       NO-UNDO LIKE MULTI.Mov_Contable.Db.
DEF VAR tcpte        NO-UNDO LIKE MULTI.Mov_Contable.Db.

FORM tagencia     COLUMN-LABEL "AGENCIA"  
     tfecha       COLUMN-LABEL "FECHA" 
     tcomprobante COLUMN-LABEL "COMPROBANTE"   
     tdocumento   COLUMN-LABEL "DOCUMENTO"
     tvalor       COLUMN-LABEL "VALOR"
     WITH FRAME a DOWN COLUMN 1 WIDTH 350
     NO-ATTR-SPA NO-VALIDATE NO-BOX USE-TEXT STREAM-IO.
    
FOR EACH MULTI.AGENCIA NO-LOCK.
    FOR EACH MULTI.Mov_contable NO-LOCK WHERE
             MULTI.MOV_CONTABLE.AGENCIA = AGENCIA.AGENCIA
        AND  MULTI.mov_contable.cuenta     >= PC01
        AND  MULTI.mov_contable.fec_contab >= PDT01
        AND  MULTI.mov_contable.fec_contab <= PDT02
        BREAK BY MULTI.mov_contable.Agencia BY MULTI.mov_contable.cuenta
              BY MULTI.mov_contable.comprob BY MULTI.mov_contable.num_docum:
        ASSIGN Tcpte = Tcpte + (MULTI.Mov_Contable.Db - MULTI.Mov_Contable.Cr).
        IF LAST-OF(MULTI.mov_contable.num_docum) THEN DO:
           IF Tcpte <> 0 THEN DO:
              ASSIGN tagencia     = MULTI.mov_contable.Agencia
                     tfecha       = MULTI.mov_contable.fec_contab
                     tcomprobante = MULTI.mov_contable.comprob 
                     tdocumento   = MULTI.mov_contable.num_docum
                     tvalor       = tcpte.
              DISP tagencia tfecha tcomprobante tdocumento tvalor WITH FRAME A.
              DOWN WITH FRAME a.
           END.
           Tcpte = 0.
        END.
    END.
END.
OUTPUT CLOSE.
