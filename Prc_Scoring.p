DEFINE VARIABLE Wage LIKE Agencias.agencia.
DEFINE VARIABLE WNit LIKE clientes.nit.
DEFINE SHARED VARIABLE  Wvble LIKE scoring.VARIABLE.
DEFINE SHARED VARIABLE  WTbla LIKE scoring.Tabla.
DEFINE SHARED VARIABLE W_Usuario LIKE Usuarios.Usuario.
DEFINE VARIABLE Nit_Inicial AS CHARACTER FORMAT "x(14)".
DEFINE VARIABLE Nit_Final   AS CHARACTER FORMAT "x(14)".



/* 7 : 1-CHARACTER
       2-DATE
       3-INTEGER Y DECIMAL*/
       
IF {7} NE " " THEN 
  ASSIGN Nit_Inicial = {7} 
         Nit_Final   = {7}.
ELSE
  ASSIGN Nit_Inicial = "" 
         Nit_Final = "99999999999999".


IF WTbla = "Solicitud" THEN DO:
  /* DISPLAY {1} "Solicitud" {6} Solicitud.Num_Solicitud.*/
   /* IF {1} EQ "Solicitud" AND {6} NE Solicitud.Num_Solicitud THEN
       NEXT. */
END.
ELSE DO:
    FOR EACH {1} WHERE {1}.Nit GE Nit_Inicial AND {1}.Nit LE Nit_Final:
       ASSIGN Wage = Agencia
              Wnit = nit.
   
       IF {2} GE {3} AND {2} LE {4} THEN DO:
          CREATE SCORING.
          ASSIGN SCORING.Agencia  = wage    
              SCORING.Nit             = Wnit 
              SCORING.Num_Solicitud   = {6}
              SCORING.Fec_Scoring     = TODAY
              SCORING.Tabla           = WTbla
              SCORING.VARIABLE        = wvble   
              SCORING.puntaje         = {5} / 1000
              SCORING.Codigo          = INTEGER({8})
              SCORING.Valor_Variable  = STRING({2})
              Scoring.Usuario         = W_Usuario.
       END.
    END.
END.
 
