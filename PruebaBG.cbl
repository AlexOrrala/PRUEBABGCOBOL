      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  BANDERA-SALIR           PIC X VALUE "N".
       01  OPCION                  PIC X(2).
       01  IDEMPLEADO              PIC 9 VALUE 1.
       01  IDEMPLEADOTEMP          PIC 9 VALUE 1.
       01  CEDULABUSCA             PIC X(10).
       01  SALARIONETOBUSCA        PIC 9999V99.
       01  SALARIO-STRING          PIC X(10).

       01  EMPLEADO OCCURS 10 TIMES.
           05 CEDULA               PIC X(10).
           05 Nombre               PIC X(10).
           05 Salario              PIC 9999V99.
           05 DeduccionImpuestos   PIC 9999V99.
           05 DeduccionSeguro      PIC 9999V99.
           05 SalarioNeto          PIC 9999V99.
           05 DatosSocioeconomicos PIC X(10).



       01  DeduccionesGeneral      PIC 9999V99.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM UNTIL BANDERA-SALIR = "S"
            DISPLAY "[1] Ingresar nuevo empleado."
            DISPLAY "[2] Calcular salario neto."
            DISPLAY "[3] Mostrar información de empleado."
            DISPLAY "[4] Filtrar"
            DISPLAY "[5] Salir."
            ACCEPT OPCION
               EVALUATE OPCION
                   WHEN '1'
                       PERFORM INGRESAR-EMPLEADO
                   WHEN '2'
                       PERFORM CALCULAR-NETO
                   WHEN '3'
                       PERFORM MOSTRAR-INFO
                   WHEN '4'
                       DISPLAY "[1] Salario neto"
                       DISPLAY "[2] Nivel educativo"
                       DISPLAY "[3] Salario o nivel educativo"
                       ACCEPT OPCION
                        EVALUATE OPCION
                        WHEN '1'
                           PERFORM FILTRAR-NETO
                        WHEN OTHER
                        DISPLAY "Opción inválida. Intente de nuevo."
                   WHEN '5'
                       MOVE "S" TO BANDERA-SALIR
                       STOP RUN
                   WHEN OTHER
                       DISPLAY "Opción inválida. Intente de nuevo."
            END-PERFORM.
       VALIDAR-ENTRADA.
           IF CEDULABUSCA ALPHABETIC
               AND FUNCTION LENGTH(CEDULABUSCA)= 10
               DISPLAY "¡Error! La cédula no puede estar vacía."
               DISPLAY "Por favor ingrese nuevamente la cédula."
               ACCEPT CEDULABUSCA
               PERFORM VALIDAR-ENTRADA
           END-IF.

        VALIDAR-DECIMAL.
           IF SALARIO-STRING = SPACES
               DISPLAY "¡Error! El salario no puede estar vacío."
               PERFORM VALIDAR-DECIMAL
           ELSE
               IF SALARIO-STRING NUMERIC
                 MOVE FUNCTION NUMVAL(SALARIO-STRING)
                 TO SALARIO(IDEMPLEADO)
               ELSE
                   DISPLAY "El valor ingresado no es un número decimal."
                   PERFORM VALIDAR-DECIMAL
               END-IF.

       INGRESAR-EMPLEADO.

           IF IDEMPLEADO <=9
            DISPLAY "Ingrese ID del empleado:"
            ACCEPT CEDULABUSCA
            PERFORM VALIDAR-ENTRADA

            DISPLAY "Ingrese Nombre del empleado:"
            ACCEPT Nombre(IDEMPLEADO)

            DISPLAY "Ingrese Salario bruto (número decimal)."
            ACCEPT Salario(IDEMPLEADO)

            DISPLAY "Ingrese Deducción por impuestos (número decimal)."
            ACCEPT DeduccionImpuestos(IDEMPLEADO)
            DISPLAY "Ingrese Deducción por seguro (número decimal)."
            ACCEPT DeduccionSeguro(IDEMPLEADO)
            DISPLAY "Ingrese DatosSocioeconómicos (número decimal)."
            ACCEPT DatosSocioeconomicos(IDEMPLEADO)

            ADD 1 TO IDEMPLEADO
           END-IF.

       CALCULAR-NETO.

           DISPLAY "INGRESE LA CEDULA DEL USUARIO A CALCULAR"
           ACCEPT CEDULABUSCA.

           PERFORM VARYING IDEMPLEADOTEMP FROM 1 BY 1 UNTIL
           IDEMPLEADOTEMP > 10
               IF CEDULA(IDEMPLEADOTEMP) = CEDULABUSCA
                   MOVE IDEMPLEADOTEMP TO IDEMPLEADO
                   EXIT PERFORM
               END-IF
           END-PERFORM.

           ADD DeduccionSeguro(IDEMPLEADO)
           TO DeduccionImpuestos(IDEMPLEADO)
           GIVING DeduccionesGeneral.
           SUBTRACT Salario(IDEMPLEADO) FROM DeduccionesGeneral
           GIVING SalarioNeto(IDEMPLEADO).
           DISPLAY "El salario neto del usuario de ",
           CEDULA(IDEMPLEADO), " es:",SalarioNeto(IDEMPLEADO).


       MOSTRAR-INFO.
           DISPLAY "INGRESE LA CEDULA DEL USUARIO A CONSULTAR"
           ACCEPT CEDULABUSCA.

           PERFORM VARYING IDEMPLEADOTEMP FROM 1 BY 1 UNTIL
           IDEMPLEADOTEMP > 10
               IF CEDULA(IDEMPLEADOTEMP) = CEDULABUSCA
                   MOVE IDEMPLEADOTEMP TO IDEMPLEADO
                   EXIT PERFORM
               END-IF
           END-PERFORM.

           DISPLAY "ID del empleado:",CEDULA(IDEMPLEADO).

           DISPLAY "Nombre del empleado:",Nombre(IDEMPLEADO).

           DISPLAY "Salario bruto (número decimal)."
           ,Salario(IDEMPLEADO).
           DISPLAY "Deducción por impuestos (número decimal)."
           ,DeduccionImpuestos(IDEMPLEADO).
           DISPLAY "Deducción por seguro (número decimal)."
           ,DeduccionSeguro(IDEMPLEADO).
           DISPLAY "Salario Neto (número decimal)."
           ,SalarioNeto(IDEMPLEADO).
           DISPLAY "DatosSocioeconómicos (número decimal)."
           ,DatosSocioeconomicos(IDEMPLEADO).


       FILTRAR-NETO.
           DISPLAY "INGRESE EL NETO A FILTRAR"
           ACCEPT SALARIONETOBUSCA.

           PERFORM VARYING IDEMPLEADOTEMP FROM 1 BY 1 UNTIL
           IDEMPLEADOTEMP > 10
               IF SalarioNeto(IDEMPLEADOTEMP) = SALARIONETOBUSCA
                   DISPLAY "ID del empleado:",CEDULA(IDEMPLEADO)
                   DISPLAY "Nombre del empleado:",Nombre(IDEMPLEADO)
                   DISPLAY "Ingrese Salario bruto (número decimal)."
                   ,Salario(IDEMPLEADO)
                   DISPLAY "Deducción por impuestos (número decimal)."
                   ,DeduccionImpuestos(IDEMPLEADO)
                   DISPLAY "Deducción por seguro (número decimal)."
                   ,DeduccionSeguro(IDEMPLEADO)
                   DISPLAY "DatosSocioeconómicos (número decimal)."
                   ,DatosSocioeconomicos(IDEMPLEADO)
               END-IF
           END-PERFORM.



       STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
