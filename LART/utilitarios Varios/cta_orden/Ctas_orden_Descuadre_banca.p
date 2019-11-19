
{incluido\iprmt_rpt.i}
{incluido\Variable.i "SHARED"}
        
IF P_NomArchivo = "DEFAULT" THEN
   P_NomArchivo = W_Pathspl + "\DESC_BANCA.txt".

OUTPUT TO VALUE(P_NomArchivo) PAGED PAGE-SIZE VALUE(P_NumLineas).
    
{incluido\RepHeader.i}

VIEW FRAME F-Encabezado.
W_Reporte   = "REPORTE   : " + P_Titulo + " (prCtas_Orden_Descuadre_Banca.p) " 
              + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").

DEF VAR tagencia     NO-UNDO LIKE BDCENTRAL.mov_contable.Agencia.
DEF VAR tfecha       NO-UNDO LIKE BDCENTRAL.mov_contable.fec_contab.
DEF VAR tcomprobante NO-UNDO LIKE BDCENTRAL.mov_contable.comprob.
DEF VAR tdocumento   NO-UNDO LIKE BDCENTRAL.mov_contable.num_docum.
DEF VAR tvalor       NO-UNDO LIKE BDCENTRAL.mov_contable.db.
DEF VAR tcpte        NO-UNDO LIKE BDCENTRAL.mov_contable.db.

FORM tagencia     COLUMN-LABEL "AGENCIA"  
     tfecha       COLUMN-LABEL "FECHA" 
     tcomprobante COLUMN-LABEL "COMPROBANTE"   
     tdocumento   COLUMN-LABEL "DOCUMENTO"
     tvalor       COLUMN-LABEL "VALOR"
     WITH FRAME a DOWN COLUMN 1 WIDTH 350
     NO-ATTR-SPA NO-VALIDATE NO-BOX USE-TEXT STREAM-IO.
    
FOR EACH BDCENTRAL.AGENCIA NO-LOCK.
    FOR EACH BDCENTRAL.Mov_contable NO-LOCK WHERE
             BDCENTRAL.MOV_CONTABLE.AGENCIA = AGENCIA.AGENCIA
        AND  BDCENTRAL.mov_contable.cuenta     >= PC01
        AND  BDCENTRAL.mov_contable.fec_contab >= PDT01
        AND  BDCENTRAL.mov_contable.fec_contab <= PDT02
        BREAK BY BDCENTRAL.mov_contable.Agencia BY BDCENTRAL.mov_contable.cuenta
              BY BDCENTRAL.mov_contable.comprob BY BDCENTRAL.mov_contable.num_docum:
        ASSIGN Tcpte = Tcpte + (BDCENTRAL.Mov_Contable.Db - BDCENTRAL.Mov_Contable.Cr).
        IF LAST-OF(BDCENTRAL.mov_contable.num_docum) THEN DO:
           IF Tcpte <> 0 THEN DO:
              ASSIGN tagencia     = BDCENTRAL.mov_contable.Agencia
                     tfecha       = BDCENTRAL.mov_contable.fec_contab
                     tcomprobante = BDCENTRAL.mov_contable.comprob 
                     tdocumento   = BDCENTRAL.mov_contable.num_docum
                     tvalor       = tcpte.
              DISP tagencia tfecha tcomprobante tdocumento tvalor WITH FRAME A.
              DOWN WITH FRAME a.
           END.
           Tcpte = 0.
        END.
    END.
END.
OUTPUT CLOSE.
