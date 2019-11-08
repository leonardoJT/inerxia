

FOR EACH Mov_cont_MUlt WHERE Mov_cont_MUlt.fec_contable <= DATE(03,31,2008) 
                        /*AND Mov_cont_MUlt.agencia = 5            */
                        /*AND Mov_cont_mult.cuenta = '14110511'*/
                        /*AND Mov_cont_MUlt.Num_Documento = 500169*/
                        /*AND Mov_cont_MUlt.comprobante       = 8*/
                        
  :
                                                 
   DISPLAY Mov_cont_MUlt EXCEPT comentario 
                                    Base 
                                    Cen_Costos 
                                    Conciliado 
                                    Destino 
                                    Det_Plazo 
                                    Det_ValAmortizacion 
                                    Doc_Referencia 
                                    Enlace 
                                    Estacion  
                                    Fec_ProntoPago 
                                    Hora Hora_TarjDB  
                                    Nro_Auditoria 
                                    Por_ProntoPago 
                                    usuario
                                    Fec_Grabacion. 

   
   
END.



MESSAGE 'final'
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
