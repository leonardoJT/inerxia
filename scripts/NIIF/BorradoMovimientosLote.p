DISABLE TRIGGERS FOR LOAD OF mov_contable_NIIF.

DEFINE VAR cont AS INTEGER.

/*FOR EACH mov_contable_niif WHERE fec_contable <= 12/31/2015 NO-LOCK BY fec_contable:
    DISPLAY mov_contable_NIIF WITH WIDTH 300 1 COL.
    LEAVE.
END.*/

FOR EACH mov_contable_niif WHERE mov_contable_NIIF.fec_contable >= 01/01/2017
                             AND mov_contable_NIIF.fec_contable <= 01/31/2017 /*NO-LOCK*/:
      FIND FIRST mov_contable WHERE Mov_Contable.Comprobante = mov_contable_NIIF.comprobante
                                AND Mov_Contable.Cen_Costos = mov_contable_NIIF.cen_costos
                                AND Mov_Contable.agencia = Mov_Contable_NIIF.agencia
                                AND Mov_Contable.Fec_Contable = mov_contable_NIIF.fec_contable
                                AND Mov_Contable.Usuario = mov_contable_NIIF.usuario
                                AND Mov_Contable.Comentario = mov_contable_NIIF.comentario
                                AND Mov_Contable.Cr = mov_contable_NIIF.cr
                                AND Mov_Contable.Db = mov_contable_NIIF.db
                                AND Mov_Contable.Doc_Referencia = mov_contable_NIIF.doc_referencia
                                AND Mov_Contable.Fec_Grabacion = mov_contable_NIIF.fec_grabacion
                                AND Mov_Contable.Nit = mov_contable_NIIF.nit
                                AND Mov_Contable.Num_Documento = mov_contable_NIIF.num_documento NO-LOCK NO-ERROR.
      IF NOT AVAILABLE mov_contable THEN
          /*UPDATE mov_contable_NIIF WITH WIDTH 300 1 COL.*/
          DELETE mov_contable_NIIF.
      ELSE DO:
          DELETE mov_contable_NIIF.
          cont = cont + 1.
      END.
END.

MESSAGE "Fin" cont
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
