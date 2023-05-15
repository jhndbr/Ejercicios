data Bolita = Rojo | Azul | Verde | Negro | Vacio deriving (Show, Eq)
data Tablero = Tablero [[Bolita]] Posicion Tamaño deriving (Show, Eq)
type Posicion = (Int, Int)
type Tamaño = (Int, Int)
data Direccion = Arriba | Abajo | Izquierda| Derecha



creaTb :: Int -> Int-> Tablero
creaTb n b = Tablero (replicate (n*b) [Vacio]) (1,1) (n,b)

moverTabl :: Tablero -> Direccion -> Int -> Tablero
moverTabl (Tablero lst (x, y) (a, b)) dir val
  | x + val > a || x - val < a || y + val > b || y - val < b =
      error "El cabezal se cayó del tablero"
  | otherwise =
      case dir of
        Arriba    -> Tablero lst (x + val, y) (a, b)
        Abajo     -> Tablero lst (x - val, y) (a, b)
        Izquierda -> Tablero lst (x, y - val) (a, b)
        Derecha   -> Tablero lst (x, y + val) (a, b)

agregarBolita :: Tablero -> Bolita -> Tablero
agregarBolita (Tablero lst pos@(x, y) tam@(a, b)) bolita
  | x < 1 || x > a || y < 1 || y > b =
      error "Posición inválida en el tablero"
  | otherwise =
      let idx = (x - 1) * b + y - 1
          nuevaLista = take idx lst ++ [bolita] : drop (idx + 1) lst
      in Tablero nuevaLista pos tam