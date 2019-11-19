
OUTPUT TO "c:\info_fodun\clientesl.csv". 

FOR EACH clientes WHERE Clientes.Tipo_Cliente = 1  BY Clientes.Apellido1  :

    DISPLAY 
            Clientes.Nit VIEW-AS FILL-IN
            Clientes.Apellido1 VIEW-AS FILL-IN
            Clientes.Apellido2 VIEW-AS FILL-IN
            Clientes.Nombre    VIEW-AS FILL-IN
            Clientes.Estado  VIEW-AS TEXT
            Clientes.Tipo_Cliente VIEW-AS TEXT
         WITH WIDTH 250 .
    
END.
OUTPUT CLOSE.
