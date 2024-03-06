{-

  * En un mundo cada vez más digitalizado, la necesidad de optimizar la gestión del tiempo se vuelve primordial. En este 
  * escenario, surge la demanda de un asistente virtual capaz de facilitar el agendamiento de citas de manera eficiente y 
  * precisa. Este asistente, dotado de inteligencia artificial, es capaz de interactuar con los usuarios de diversas 
  * formas, permitiendo que estos programen citas de una manera sencilla y conveniente. La interacción comienza con el 
  * usuario proporcionando su nombre y apellido, lo cual permite al asistente personalizar la experiencia y asegurar la 
  * correcta identificación del usuario.

  * Posteriormente, el usuario especifica el nombre y apellido del anfitrión con quien desea agendar la cita. Esta 
  * característica permite una flexibilidad total, ya que el usuario puede programar citas con diferentes personas según 
  * sus necesidades. Luego, el usuario indica la fecha y hora deseada para la cita. Aquí, el asistente se asegura de que 
  * la fecha y hora propuestas cumplan con los requisitos establecidos, incluyendo la disponibilidad del anfitrión y la 
  * ausencia de conflictos de horario.

  * Para garantizar la eficiencia y la calidad del servicio. Se deben establecer un tiempo de descanso de 15 minutos entre 
  * citas para el anfitrión, asegurando así su bienestar y evitando el agotamiento. También se limita la duración máxima 
  * de cada cita a 50 minutos, garantizando que las reuniones sean efectivas y no se prolonguen en exceso.

  * Adicionalmente, se ha implementado una regla que limita la duración total de las citas en una jornada a 5 horas. Esto 
  * asegura que el anfitrión no se sobrecargue de trabajo y pueda mantener un equilibrio saludable entre su vida laboral y 
  * personal.

  * Finalmente, se ofrece la posibilidad de solicitar la cita a través de diferentes canales, como teléfono, chat o correo
  * electrónico. Esto permite una mayor flexibilidad y comodidad para los usuarios, quienes pueden elegir el método de 
  * comunicación que mejor se adapte a sus preferencias y necesidades. Considere los siguientes elementos.

  * Predicados:
  *   Usuario(U): U es un usuario.  Anfitrion(A): A es un anfitrión.
  *   Fecha(F, D, M, A): Hay una cita programada en la fecha D/M/A.
  *   Hora(H, M): La cita programada tiene lugar a la hora H:M.
  *   Canal(C, U): La cita fue solicitada a través del canal C por el usuario U.
  
  
  *  U, A, D, M, A, H, M, C: Variables de tipo genérico.
  *  Constantes
  *  Las constantes son valores específicos que no cambian, por ejemplo, los nombres de los usuarios o anfitriones 
  *  específicos, las fechas, las horas, y los canales de solicitud.

  * Reglas:
  *   - HayCitaProgramada(U, A, D, M, A) :- Usuario(U) & Anfitrion(A) & Fecha(F, D, M, A).
  *     Hay una cita programada si hay un usuario, un anfitrión y una fecha asociada.

  
  * Que tiene que hacer el programa:
  *   - El usuario debe proporcionar su nombre y apellido.
    
  *   - Luego el usuario proporciona el nombre y apellido del anfitrión
  *     con el que quiere agendar una cita.
  *
  *   - El usuario indica la fecha y hora deseada para la cita.

  *   - El asistente se asegura de que la fecha y hora propuestas cumplan con los requisitos establecidos, incluyendo la 
  *     disponibilidad del anfitrión y la ausencia de conflictos de horario.
  
  *   - Se deben establecer un tiempo de descanso de 15 minutos entre citas para el anfitrión.

  *   - También se limita la duración máxima de cada cita a 50 minutos.

  *   - Tiempo limite de las citas en una jornada a 5 horas.

  *   - Posibilidad de solicitar la cita a través de diferentes canales, como teléfono, chat o correo electrónico.
-}

-- Predicados
data Usuario = Usuario String String
data Anfitrion = Anfitrion String String
data Fecha = Fecha Int Int Int
data Hora = Hora Int Int
data Canal = Canal String Usuario
data Cita = Cita Usuario Anfitrion Fecha Hora Canal

-- Instancias
instance Show Usuario where
  show (Usuario nombre apellido) = nombre ++ " " ++ apellido

instance Show Anfitrion where
  show (Anfitrion nombre apellido) = nombre ++ " " ++ apellido

instance Show Fecha where
  show (Fecha dia mes anio) = show dia ++ "/" ++ show mes ++ "/" ++ show anio

instance Show Hora where
  show (Hora hora minutos) = show hora ++ ":" ++ show minutos

instance Show Canal where
  show (Canal canal usuario) = canal ++ " " ++ show usuario

instance Show Cita where
  show (Cita usuario anfitrion fecha hora canal) = "Hay una cita con el usuerio, " ++ show usuario ++ ", agendada con el anfitrion, " ++ show anfitrion ++ ", el " ++ show fecha ++ " a las " ++ show hora ++ ", agendada por " ++ show canal

-- Variables
usuario1 = Usuario "Juan" "Perez"
anfitrion1 = Anfitrion "Pedro" "Gomez"
fecha1 = Fecha 10 10 2021
hora1 = Hora 10 30
canal1 = Canal "Telefono" usuario1

usuario2 = Usuario "Diego" "Quintana"
anfitrion2 = Anfitrion "Edwin" "Puertas"
fecha2 = Fecha 22 10 2024
hora2 = Hora 10 30
canal2 = Canal "Telefono" usuario2

citasAgendadas :: [Cita]
citasAgendadas = []

-- Reglas
esUsuario :: Usuario -> Bool
esUsuario (Usuario nombre apellido) = not (null nombre) && not (null apellido)

esAnfitrion :: Anfitrion -> Bool
esAnfitrion (Anfitrion nombre apellido) = not (null nombre) && not (null apellido)

esFecha :: Fecha -> Bool
esFecha (Fecha d m a) = d > 0 && m > 0 && a > 0

esHora :: Hora -> Bool
esHora (Hora hora min) = hora >= 0 && hora < 24 && min >= 0 && min < 60

esCanal :: Canal -> Bool
esCanal (Canal c _) = not (null c)


hayCitaProgramada :: Usuario -> Anfitrion -> Fecha -> Hora -> Canal -> Bool
hayCitaProgramada u a f h c = esUsuario u && esAnfitrion a && esFecha f && esHora h && esCanal c

agendarCita :: Usuario -> Anfitrion -> Fecha -> Hora -> Canal -> [Cita] -> [Cita]
agendarCita u a f h c cs = cs ++ [(Cita u a f h c)]


-- Perdir datos
pedirUsuario :: IO Usuario
pedirUsuario = do
  putStr "Ingrese su nombre: "
  nombre <- getLine
  putStr "Ingrese su apellido: "
  apellido <- getLine
  return (Usuario nombre apellido)

pedirAnfitrion :: IO Anfitrion
pedirAnfitrion = do
  putStr "Ingrese el nombre del anfitrion: "
  nombre <- getLine
  putStr "Ingrese el apellido del anfitrion: "
  apellido <- getLine
  return (Anfitrion nombre apellido)

pedirFecha :: IO Fecha
pedirFecha = do
  putStr "Ingrese el dia: "
  dia <- readLn
  putStr "Ingrese el mes: "
  mes <- readLn
  putStr "Ingrese el año: "
  anio <- readLn
  return (Fecha dia mes anio)

pedirHora :: IO Hora
pedirHora = do
  putStr "Ingrese la hora: "
  hora <- readLn
  putStr "Ingrese los minutos: "
  minutos <- readLn
  return (Hora hora minutos)

pedirCanal usuario = do
  putStr "Ingrese el canal: "
  canal <- getLine
  return (Canal canal usuario)

-- Agendar cita
agendarCitaIO :: [Cita] -> IO [Cita]
agendarCitaIO citas = do
  usuario <- pedirUsuario
  anfitrion <- pedirAnfitrion
  fecha <- pedirFecha
  hora <- pedirHora
  canal <- (pedirCanal usuario)
  if hayCitaProgramada usuario anfitrion fecha hora canal
    then return (agendarCita usuario anfitrion fecha hora canal citas)
    else do
      putStrLn "Datos incorrectos, intente de nuevo"
      agendarCitaIO citas

-- Impresion

imprimir :: Int -> [Cita] -> IO ()
imprimir _ [] = putStrLn ""
imprimir count (c:cs) = do
  putStrLn $ show count ++ ". " ++ show c ++ "\n"
  imprimir (count + 1) cs

imprimirCitas :: [Cita] -> IO ()
imprimirCitas = imprimir 1
  
llenarCitas :: [Cita] -> IO [Cita]
llenarCitas (c: cs) = do
  imprimirCitas (c:cs)
  putStrLn "Desea agendar una cita? (s/n)"
  respuesta <- getLine
  if respuesta == "s"
    then do
      cs <- agendarCitaIO (c:cs)
      llenarCitas cs
    else return (c:cs)

-- main :: IO ()
-- main = do
--   print (hayCitaProgramada usuario1 anfitrion1 fecha1 hora1 canal1)
--   llenarCitas citasAgendadas  

main :: IO ()
main = do
  putStrLn "Bienvenido al sistema de agendamiento de citas."
  putStrLn "Por favor, siga las instrucciones para agendar una cita."

  -- Solicitar información del usuario
  usuario <- pedirUsuario

  -- Solicitar información del anfitrión
  anfitrion <- pedirAnfitrion

  -- Solicitar fecha y hora para la cita
  putStrLn "Ingrese la fecha y hora para la cita:"
  fecha <- pedirFecha
  hora <- pedirHora

  -- Solicitar canal de comunicación
  canal <- (pedirCanal usuario)
  -- Verificar si la cita propuesta cumple con los requisitos
  if hayCitaProgramada usuario anfitrion fecha hora canal
    then do
      putStrLn "\nCita agendada con éxito."
      let nuevaCita = Cita usuario anfitrion fecha hora canal
      -- Agregar la nueva cita a la lista de citas
      let citasActualizadas = citasAgendadas ++ [nuevaCita]
      -- Imprimir todas las citas agendadas
      putStrLn "\nCitas agendadas:\n"
      imprimirCitas citasActualizadas
  else do
    putStrLn "\nLos datos proporcionados son incorrectos o no cumplen con los requisitos."
    putStrLn "Por favor, intente de nuevo.\n"
    -- Volver a solicitar la información de la cita
    main




