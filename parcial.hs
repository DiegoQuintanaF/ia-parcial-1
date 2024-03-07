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
  *   - esCitaProgramable(U, A, D, M, A) :- Usuario(U) & Anfitrion(A) & Fecha(F, D, M, A).
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

-- Reglas

esUsuario :: Usuario -> Bool
esUsuario (Usuario nombre apellido) = not (null nombre) && not (null apellido) && length nombre > 1 && length apellido > 1

esAnfitrion :: Anfitrion -> Bool
esAnfitrion (Anfitrion nombre apellido) = not (null nombre) && not (null apellido) && length nombre > 1 && length apellido > 1

esFecha :: Fecha -> Bool
esFecha (Fecha dia mes anio)
  | anio < 1 = False
  | mes < 1 || mes > 12 = False
  | dia < 1 || dia > (diasMes mes anio) = False
  | otherwise = True

-- Función para obtener el número de días en un mes
diasMes :: Int -> Int -> Int
diasMes mes anio
  | mes == 2 && esBisiesto anio = 29
  | mes == 2 = 28
  | mes `elem` [4, 6, 9, 11] = 30
  | otherwise = 31

-- Función para verificar si un año es bisiesto
esBisiesto :: Int -> Bool
esBisiesto anio
  | anio `mod` 4 == 0 && anio `mod` 100 /= 0 = True
  | anio `mod` 400 == 0 = True
  | otherwise = False

esHora :: Hora -> Bool
esHora (Hora hora min) = hora >= 0 && hora < 24 && min >= 0 && min < 60

esCanal :: Canal -> Bool
esCanal (Canal c _) = not (null c) && (c == "Telefono" || c == "Chat" || c == "Correo")

-- Reglas
esCitaProgramable :: Usuario -> Anfitrion -> Fecha -> Hora -> Canal -> Bool
esCitaProgramable u a f h c = esUsuario u && esAnfitrion a && esFecha f && esHora h && esCanal c

agendarCita :: Usuario -> Anfitrion -> Fecha -> Hora -> Canal -> [Cita] -> [Cita]
agendarCita u a f h c cs = cs ++ [(Cita u a f h c)]

-- Obtener las citas agendadas de un anfitrión

nombreAnfitrion :: Cita -> String
nombreAnfitrion (Cita _ (Anfitrion nombre _) _ _ _) = nombre

apellidoAnfitrion :: Cita -> String
apellidoAnfitrion (Cita _ (Anfitrion _ apellido) _ _ _) = apellido

nombreUsuario :: Cita -> String
nombreUsuario (Cita (Usuario nombre _) _ _ _ _) = nombre

apellidoUsuario :: Cita -> String
apellidoUsuario (Cita (Usuario _ apellido) _ _ _ _) = apellido

anfitrionCita :: Cita -> Anfitrion
anfitrionCita (Cita _ anfitrion _ _ _) = anfitrion

usuarioCita :: Cita -> Usuario
usuarioCita (Cita usuario _ _ _ _) = usuario

fechaCita :: Cita -> Fecha
fechaCita (Cita _ _ fecha _ _) = fecha

horaCita :: Cita -> Hora
horaCita (Cita _ _ _ hora _) = hora

canalCita :: Cita -> Canal
canalCita (Cita _ _ _ _ canal) = canal

-- Cita Usuario Anfitrion Fecha Hora Canal
citasAnfitrion :: Anfitrion -> [Cita] -> [Cita]
citasAnfitrion _ [] = []
citasAnfitrion (Anfitrion nombre apellido) (c:cs) = do
  if nombre == nombreAnfitrion c && apellido == apellidoAnfitrion c
    then c : citasAnfitrion (Anfitrion nombre apellido) cs
    else citasAnfitrion (Anfitrion nombre apellido) cs
  
menosDeCincoHoras :: Int -> [Cita] -> Bool
menosDeCincoHoras _ [] = True
menosDeCincoHoras horas (c:cs) = do
  if horasTotalesAnfitrion 0 (c:cs) > 300
    then False
    else menosDeCincoHoras horas cs

horasTotalesAnfitrion :: Int -> [Cita] -> Int
horasTotalesAnfitrion horas [] = horas
horasTotalesAnfitrion horas (c:cs) = do
  horasTotalesAnfitrion (horas + 50) cs

tieneMenosDeCincoHoras :: [Cita] -> Bool
tieneMenosDeCincoHoras = menosDeCincoHoras 0 

-- Impresion
imprimir :: Int -> [Cita] -> IO ()
imprimir _ [] = putStrLn ""
imprimir count (c:cs) = do
  putStrLn $ show count ++ ". " ++ show c ++ "\n"
  imprimir (count + 1) cs

imprimirCitas :: [Cita] -> IO ()
imprimirCitas = imprimir 1

citasMismoDia :: Fecha -> [Cita] -> [Cita]
citasMismoDia _ [] = []
citasMismoDia fecha (c:cs) = do
  if esMismaFecha fecha (fechaCita c)
    then c : citasMismoDia fecha cs
    else citasMismoDia fecha cs

-- Horas a minutos
minutosEnHora :: Hora -> Int
minutosEnHora (Hora h m) = h * 60 + m

-- Comprobar si una hora está entre dos horas
estaEntre :: Hora -> Hora -> Bool
estaEntre citaNueva citaExistente = (minutosEnHora citaExistente) < (minutosEnHora citaNueva) && (minutosEnHora citaExistente) + 65 > (minutosEnHora citaNueva) 

-- Comprobar si son las mismas fechas
esMismaFecha :: Fecha -> Fecha -> Bool
esMismaFecha (Fecha d1 m1 a1) (Fecha d2 m2 a2) = d1 == d2 && m1 == m2 && a1 == a2

-- Comprobar si una cita choca con otra cita
chocaConOtraCita :: Cita -> [Cita] -> Bool
chocaConOtraCita _ [] = False
chocaConOtraCita nuevaCita (citaExistente:cs) = do
  if esMismaFecha (fechaCita nuevaCita) (fechaCita citaExistente) && estaEntre (horaCita nuevaCita) (horaCita citaExistente)
    then True
    else chocaConOtraCita nuevaCita cs
  
-- Variables
usuario1 = Usuario "Juan" "Perez"
anfitrion1 = Anfitrion "Pedro" "Gomez"
fecha1 = Fecha 10 11 2024
hora1 = Hora 10 30
canal1 = Canal "Telefono" usuario1
cita1 = Cita usuario1 anfitrion1 fecha1 hora1 canal1

usuario2 = Usuario "Diego" "Quintana"
anfitrion2 = Anfitrion "Edwin" "Puertas"
fecha2 = Fecha 10 10 2024
hora2 = Hora 11 00
canal2 = Canal "Telefono" usuario2
cita2 = Cita usuario2 anfitrion2 fecha2 hora2 canal2

usuario3 = Usuario "Luis" "Perez"
anfitrion3 = Anfitrion "Pedro" "Gomez"
fecha3 = Fecha 10 11 2024
hora3 = Hora 11 36
canal3 = Canal "Telefono" usuario3

cita3 = Cita usuario3 anfitrion3 fecha3 hora3 canal3

citas_agendadas = [cita1, cita2]

test :: IO ()
test = print ( cita3 `chocaConOtraCita` (citasAnfitrion anfitrion3 citas_agendadas)) -- True
test2 :: IO ()
test2 = print (horasTotalesAnfitrion 0 (citasAnfitrion anfitrion1 citas_agendadas)) -- 2
test3 :: IO ()
test3 = print (tieneMenosDeCincoHoras (citasAnfitrion anfitrion1 citas_agendadas)) -- True

test4 :: IO ()
test4 = print (citasMismoDia fecha1 citas_agendadas)

-- esPosibleAgendarCita :: Cita -> [Cita] -> Bool
-- esPosibleAgendarCita nCita citasAgendadas = do
--   let usuario = usuarioCita nCita
--   let anfitrion = anfitrionCita nCita
--   let fecha = fechaCita nCita
--   let hora = horaCita nCita
--   let canal = canalCita nCita
--   if esCitaProgramable usuario anfitrion fecha hora canal
--     then True
--     else False
--   if tieneMenosDeCincoHoras (citasAnfitrion anfitrion citasAgendadas) && not (nCita `chocaConOtraCita` (citasAnfitrion anfitrion citasAgendadas))
--     then True
--     else False 


esPosibleAgendarCita :: Cita -> [Cita] -> Bool
esPosibleAgendarCita nCita citasAgendadas =
  esCitaProgramable usuario anfitrion fecha hora canal &&
  tieneMenosDeCincoHoras (citasAnfitrion anfitrion elDia) &&
  not (nCita `chocaConOtraCita` elDia)
  where
    usuario = usuarioCita nCita
    anfitrion = anfitrionCita nCita
    fecha = fechaCita nCita
    hora = horaCita nCita
    canal = canalCita nCita
    elDia = citasMismoDia fecha citas_agendadas

test5 :: IO ()
test5 = print (esPosibleAgendarCita cita3 citas_agendadas) -- False


main :: IO ()
main = do
  putStrLn "Citas agendadas"
  imprimirCitas citas_agendadas