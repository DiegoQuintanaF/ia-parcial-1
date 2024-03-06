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
usuario = Usuario "Juan" "Perez"
anfitrion = Anfitrion "Pedro" "Gomez"
fecha = Fecha 10 10 2021
hora = Hora 10 30
canal = Canal "Telefono" usuario

usuario2 = Usuario "Diego" "Quintana"
anfitrion2 = Anfitrion "Edwin" "Puertas"
fecha2 = Fecha 22 10 2024
hora2 = Hora 10 30
canal2 = Canal "Telefono" usuario2

citasAgendadas :: [Cita]
citasAgendadas = []

-- Reglas
hayCitaProgramada :: Usuario -> Anfitrion -> Fecha -> Hora -> Canal -> Bool
hayCitaProgramada (Usuario u1 u2) (Anfitrion a1 a2) (Fecha d m a) (Hora h min) (Canal c _) = not (null u1) && not (null u2) && not (null a1) && not (null a2) && d > 0 && m > 0 && a > 0 && h >= 0 && h < 24 && min >= 0 && min < 60 && not (null c)

agendarCita :: Usuario -> Anfitrion -> Fecha -> Hora -> Canal -> [Cita] -> [Cita]
agendarCita u a f h c cs = cs ++ [Cita u a f h c]

imprimir :: Int -> [Cita] -> IO ()
imprimir _ [] = putStrLn ""
imprimir count (c:cs) = do
  putStrLn $ show count ++ ". " ++ show c ++ "\n"
  imprimir (count + 1) cs

imprimirCitas :: [Cita] -> IO ()
imprimirCitas = imprimir 1


-- Main
-- main :: IO ()
-- main = do
--   let citas = []
--   agendarCita usuario anfitrion fecha hora canal citas
--   agendarCita usuario2 anfitrion2 fecha2 hora2 canal2 citas
--   -- citas = (agendarCita (Usuario "Diego" "Quintana") (Anfitrion "Edwin" "Puertas") (Fecha 22 10 2024) (Hora 10 30) (Canal "Telefono" (Usuario "Diego" "Quintana"))) citas
--   -- citas = (agendarCita (Usuario "Jesus" "Martinez") (Anfitrion "Karoll" "Pinto") (Fecha 30 11 2024) (Hora 11 30) (Canal "Correo" (Usuario "Jesus" "Martinez"))) citas
--   show citas[0]
  
main :: IO ()
main = do
  -- let citas = agendarCita usuario anfitrion fecha hora canal (agendarCita usuario2 anfitrion2 fecha2 hora2 canal2 citasAgendadas)
  -- imprimirCitas citas
  print (hayCitaProgramada usuario anfitrion fecha hora canal)

-- Resultado
-- Cita (Usuario "Juan" "Perez") (Anfitrion "Pedro" "Gomez") (Fecha 10 10 2021) (Hora 10 30) (Canal "Telefono" (Usuario "Juan" "Perez"))





