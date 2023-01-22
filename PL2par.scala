import scala.collection.parallel.immutable.{ParSeq}
import scala.annotation.tailrec
import scala.io.StdIn.readInt

object PL2Par extends App {

  //Método principal de juego, permite seleccionar la dificultad y si juega la ia o un jugador
  printf("Selecciona un tipo de juego:\n1.Modo1\n2.Modo2\n3.Modo3\n")
  val modo = readInt()
  modo match {
    case 1 =>
      println("Modo 1")
      val sizeX=9
      val sizeY=11
      val bombas =2
      val vidas=8
      val color =3
      printf("Selecciona el jugador:\n1.Humano\n2.Ia\n")
      val jugador = readInt()
      jugador match {
        case 1 =>
          println("Puntuación final: "+jugar(crearTablero(sizeX,sizeY,bombas,color),vidas,0,sizeX,sizeY,bombas,color))
        case 2 =>
          println("Puntuación final: "+jugarAI(crearTablero(sizeX,sizeY,bombas,color),vidas,0,sizeX,sizeY,bombas,color))
        case _ =>
          println("Opcion no valida")
      }
    case 2 =>
      println("Modo 2")
      val sizeX=12
      val sizeY=16
      val bombas =3
      val vidas=10
      val color =5
      printf("Selecciona el jugador:\n1.Humano\n2.Ia\n")
      val jugador = readInt()
      jugador match {
        case 1 =>
          println("Puntuación final: "+jugar(crearTablero(sizeX,sizeY,bombas,color),vidas,0,sizeX,sizeY,bombas,color))
        case 2 =>
          println("Puntuación final: "+jugarAI(crearTablero(sizeX,sizeY,bombas,color),vidas,0,sizeX,sizeY,bombas,color))
        case _ =>
          println("Opcion no valida")
      }
    case 3 =>
      println("Modo 3")
      val sizeX=25
      val sizeY=15
      val bombas=5
      val vidas=15
      val color =7
      printf("Selecciona el jugador:\n1.Humano\n2.Ia\n")
      val jugador = readInt()
      jugador match {
        case 1 =>
          println("Puntuación final: "+jugar(crearTablero(sizeX,sizeY,bombas,color),vidas,0,sizeX,sizeY,bombas,color))
        case 2 =>
          println("Puntuación final: "+jugarAI(crearTablero(sizeX,sizeY,bombas,color),vidas,0,sizeX,sizeY,bombas,color))
        case _ =>
          println("Opcion no valida")
      }
    case _ =>
      println("Opcion no valida")
  }

  //Cuenta cuantos elementos tiene la lista
  def longitudLista(lista : List[Any]): Int ={
    if(lista.isEmpty) 0
    else 1 + longitudLista(lista.tail)
  }

  //Appendea dos listas de listas
  @tailrec
  def appendTailRec(tablero : List[List[Int]], vacios : List[List[Int]], resultado : List[List[Int]] = Nil) : List[List[Int]] = {
    @tailrec
    def espejo(tablero : List[List[Int]], resultado : List[List[Int]] = Nil) : List[List[Int]] ={
      if(tablero.isEmpty) resultado
      else espejo(tablero.tail, tablero.head :: resultado)
    }
    if (vacios.isEmpty && tablero.isEmpty) espejo(resultado)
    else if (tablero.isEmpty) {
      if(vacios.head.isEmpty) appendTailRec(tablero, vacios.tail, resultado)
      else appendTailRec(tablero, vacios.tail, vacios.head :: resultado)
    }
    else {
      if(tablero.head.isEmpty) appendTailRec(tablero.tail, vacios, resultado)
      else appendTailRec(tablero.tail, vacios, tablero.head :: resultado )
    }
  }

  //Crea una lista de n ceros
  def crearListaCeros(n: Int): List[Int] = n match {
    case 0 => List[Int]()
    case _ => 0 +: crearListaCeros(n - 1)
  }

  //Comprueba si un elemento está en la lista
  def contiene[T](tablero : List[T], elemento : T) : Boolean = {
    tablero.par.filter(_ == elemento).nonEmpty
  }

  //Si hay 0 en una columna, se mueven hacia las primeras posiciones de la columna
  //intercambiandose con los elementos que no son 0
  def gravedad(tablero: List[List[Int]]): List[List[Int]] = {
    def gravedadColumna(columna: List[Int]): List[Int] = {
      def gravedadColumnaAux(columna: List[Int]): List[Int] = columna match {
        case Nil => List[Int]()
        case 0 :: tail => gravedadColumnaAux(tail)
        case head :: tail => head :: gravedadColumnaAux(tail)
      }

      val nuevaColumna = gravedadColumnaAux(columna)
      crearListaCeros(columna.length - nuevaColumna.length) ::: nuevaColumna
    }
    tablero.par.map(gravedadColumna).toList
  }

  // Si hay columnas llenas de 0(vacías), las columnas pasan no vacías pasan a través de las vacías
  def gravedadIzquierda(tablero: List[List[Int]]): List[List[Int]] = {
    def columnaSinFichas(columna: List[Int]): Boolean = columna match {
      case Nil => true
      case 0 :: tail => columnaSinFichas(tail)
      case _ => false
    }
    if (tablero.isEmpty)
      List[List[Int]]()
    else if (columnaSinFichas(tablero.head))
      gravedadIzquierda(tablero.tail)
    else
      tablero.head :: gravedadIzquierda(tablero.tail)
  }

  // Actualizar tablero
  def actualizar(tablero: List[List[Int]]): List[List[Int]] = {
    gravedadIzquierda(gravedad(tablero))
  }

  // Devuelve el elemento de un tablero que se encuentra en las coordenadas dadas
  def buscarElemento(tablero: List[List[Int]], x:Int, y:Int): Int = {
    def buscarElementoColumna(columna: List[Int], posicion: Int): Int = posicion match {
      case 0 => columna.head
      case _ => buscarElementoColumna(columna.tail, posicion - 1)
    }
    x match {
      case 0 => buscarElementoColumna(tablero.head, y)
      case _ => buscarElemento(tablero.tail, x-1, y)
    }
  }

  // Busca los bloques adyacentes del mismo bloque y los devuelve en una lista de
  // coordenadas
  def colindantes(tablero: List[List[Int]], x: Int, y: Int): List[List[Int]] ={
    def vecinos(sizeX:Int, sizeY:Int , x:Int, y:Int, visitados:List[List[Int]], frontera: List[List[Int]]): List[List[Int]] ={
      def vecinosAux(x2:Int , y2: Int): List[Int] ={
        if(x2>=0 && x2<sizeX && y2 >=0 && y2 < sizeY && !contiene(visitados,List(x2,y2)) && !contiene(frontera,List(x2,y2))) List(x2,y2)
        else Nil
      }
      vecinosAux(x-1,y) :: vecinosAux(x+1,y) :: vecinosAux(x,y-1) :: vecinosAux(x,y+1) :: Nil
    }
    @tailrec
    def colindantesAux(frontera: List[List[Int]],visitados: List[List[Int]], resultado: List[List[Int]] = Nil): List[List[Int]] = {
      if (frontera.isEmpty) resultado
      else {
        val x2 = frontera.head.head
        val y2 = frontera.head.tail.head
        if (buscarElemento(tablero, x, y) == buscarElemento(tablero, x2, y2)) {
          colindantesAux(appendTailRec(frontera.tail, vecinos(longitudLista(tablero), longitudLista(tablero.head), x2, y2, visitados, frontera)), List(x2, y2) :: visitados, List(x2, y2) :: resultado)
        } else {
          colindantesAux(frontera.tail, List(x2, y2) :: visitados, resultado)
        }
      }
    }
    colindantesAux(List(List(x,y)),Nil)
  }

  // Elimina los elementos del tablero que están en la lista de casillas
  def eliminarElementos(tablero: List[List[Int]], casillas: List[List[Int]]): List[List[Int]] ={
    def getCasillasColumnas(casillas: ParSeq[List[Int]], x : Int ): ParSeq[List[Int]] = {
      def filterColumn(coordenada: List[Int], x: Int): Boolean = {
        coordenada.head == x
      }
      x match {
        case 0 => ParSeq[List[Int]](casillas.filter(coordenada => filterColumn(coordenada,x)).map(_.tail.head).toList)
        case _ => getCasillasColumnas(casillas,x-1) :+ casillas.filter(coordenada => filterColumn(coordenada,x)).map(_.tail.head).toList
      }
    }
    def eliminarCasillasColumnas(x : ((List[Int],List[Int]))): List[Int] = {
      def eliminarCasillaColumna(columna: List[Int], y: Int): List[Int] = y match {
        case 0 => 0 :: columna.tail
        case _ => columna.head :: eliminarCasillaColumna(columna.tail, y-1)
      }
      x._2 match {
        case Nil => x._1
        case _ => eliminarCasillasColumnas((eliminarCasillaColumna(x._1,x._2.head),x._2.tail))
      }
    }
    actualizar((tablero zip getCasillasColumnas(casillas.par,tablero.length-1).toList).par.map(eliminarCasillasColumnas).toList)
  }


  // Borra del tablero una casilla dada y los elementos adyacentes
  def explosionBomba(tablero: List[List[Int]], x: Int, y: Int): List[List[Int]] = {
    def alcanceBomba(tablero: List[List[Int]], x: Int, y: Int): List[List[Int]] ={
      def vecinos(sizeX:Int, sizeY:Int , x:Int, y:Int, visitados:List[List[Int]], frontera: List[List[Int]]): List[List[Int]] ={
        def vecinosAux(x2:Int , y2: Int): List[Int] ={
          if(x2>=0 && x2<sizeX && y2 >=0 && y2 < sizeY && !contiene(visitados,List(x2,y2)) && !contiene(frontera,List(x2,y2))) List(x2,y2)
          else Nil
        }
        vecinosAux(x - 1, y - 1) :: vecinosAux(x, y - 1) :: vecinosAux(x + 1, y - 1) :: vecinosAux(x - 1, y) :: vecinosAux(x, y) :: vecinosAux(x + 1, y) :: vecinosAux(x - 1, y + 1) :: vecinosAux(x, y + 1) :: vecinosAux(x + 1, y + 1) :: Nil
      }
      @tailrec
      def alcanceBombaAux(frontera: List[List[Int]],visitados: List[List[Int]]): List[List[Int]] = {
        if (frontera.isEmpty) visitados
        else {
          val x2 = frontera.head.head
          val y2 = frontera.head.tail.head
          if (buscarElemento(tablero, x2, y2) == 8) {
            alcanceBombaAux(appendTailRec(frontera.tail, vecinos(longitudLista(tablero), longitudLista(tablero.head), x2, y2, visitados, frontera)), List(x2, y2) :: visitados)
          } else {
            alcanceBombaAux(frontera.tail, List(x2, y2) :: visitados)
          }
        }
      }
      alcanceBombaAux(List(List(x,y)),Nil)
    }
    eliminarElementos(tablero,alcanceBomba(tablero,x,y))
  }

  //Crea un tablero dadas las dimensiones, el núemro de bombas y los diferentes colores
  def crearTablero(sizeX: Int, sizeY: Int, bombas: Int, colores:Int): List[List[Int]] = {
    def crearTableroAux(sizeX: Int, sizeY: Int,x:Int,y:Int,bombCoords: List[List[Int]], resultado: List[List[Int]] = Nil): List[List[Int]] = {
      if(sizeX == 0)  resultado ::: crearColumna(sizeY,x,y,bombCoords) ::Nil
      else crearTableroAux(sizeX-1,sizeY,x+1,y,bombCoords, resultado ::: crearColumna(sizeY,x,y,bombCoords) ::Nil   )
    }
    def crearColumna(sizeY:Int, x:Int,y:Int , bombCoords:List[List[Int]],columna:List[Int]=Nil): List[Int]={
      val n= scala.util.Random.nextInt(colores)+1
      if(sizeY == 0){
        if(contiene(bombCoords,List(x,y))) columna::: 8::Nil
        else columna::: n::Nil
      }else if (contiene(bombCoords,List(x,y))) crearColumna(sizeY-1,x,y+1,bombCoords,columna ::: 8 :: Nil)
      else crearColumna(sizeY-1,x,y+1,bombCoords,columna ::: n::Nil)
    }
    def bombCoords(sizeX: Int, sizeY: Int, bombas: Int,coords:List[List[Int]]=Nil): List[List[Int]] = {
      if(bombas == 0) coords
      else {
        val x = scala.util.Random.nextInt(sizeX)
        val y = scala.util.Random.nextInt(sizeY)
        if(!contiene(coords,List(x,y))) bombCoords(sizeX, sizeY, bombas - 1, List(x,y) :: coords)
        else bombCoords(sizeX, sizeY, bombas, coords)
      }
    }
    val bombs= bombCoords(sizeX, sizeY, bombas)
    crearTableroAux(sizeX-1, sizeY-1,0,0, bombs)
  }

  //Muestra el tablero por consola
  def mostrarTablero(tablero: List[List[Int]]): Unit= {
    def mostrarContenido(tablero:List[List[Int]],contador: Int,sizeY:Int): Unit ={
      if(contador==0) ()
      else {
        val index = sizeY-contador
        print("\n")
        printf("%2d ", index);
        mostrarContenido(imprimirLinea(tablero),contador-1,sizeY)
      }
    }
    def imprimirLinea(tablero:List[List[Int]]):List[List[Int]]={
      if(tablero.isEmpty) Nil
      else{
        print(toLetter(tablero.head.head))
        tablero.head.tail :: imprimirLinea(tablero.tail)
      }
    }
    if(tablero.isEmpty) print("Tablero vacio")
    else {

      mostrarContenido(tablero,longitudLista(tablero.head),longitudLista(tablero.head))
    }
  }

  //Inicia el bucle de juego del jugador
  def jugar(tablero: List[List[Int]] ,vidas:Int,puntos:Int,sizeX:Int,sizeY:Int,bombas:Int,colores:Int,partidas:Int=1): Int ={
    def jugarAux(tablero: List[List[Int]] , x: Int, y:Int,vidas:Int,puntos:Int,partidas:Int): Int={
      val vecinos= colindantes(tablero,x,y)
      if(buscarElemento(tablero,x,y) == 8){
        jugar(explosionBomba(tablero,x,y),vidas,puntos,sizeX,sizeY, bombas, colores, partidas)
      }else if (longitudLista(vecinos)>=3) jugar(eliminarElementos(tablero,vecinos),vidas,puntos+longitudLista(vecinos)*10,sizeX,sizeY, bombas, colores, partidas)
      else jugar(eliminarElementos(tablero,List(List(x,y))),vidas-1,0,sizeX,sizeY,bombas,colores,partidas)
    }

    if(vidas>0 ) {
      if(!tablero.isEmpty) {
        print("   ")
        imprimirSecuencia(sizeX)
        mostrarTablero(tablero)
        print("\n")
        print(s"Vidas: $vidas Puntos: $puntos Partidas: $partidas\n")
        println("Introduce la coordenada x y despues la y: ")
        val x = readInt()
        val y = readInt()
        jugarAux(tablero, x, y, vidas, puntos,partidas)
      }else {
        val t =crearTablero(sizeX,sizeY,bombas,colores)
        print("   ")
        imprimirSecuencia(sizeX)
        mostrarTablero(t)
        print("\n")
        print(s"Vidas: $vidas Puntos: $puntos Partidas: $partidas\n")
        println("Introduce la coordenada x y despues la y: ")
        val x = readInt()
        val y = readInt()
        jugarAux(t, x, y, vidas, puntos,partidas+1)
      }
    }else puntos
  }


  // Inicia el bucle de juego de la ia
  def jugarAI(tablero: List[List[Int]] ,vidas:Int,puntos:Int,sizeX:Int,sizeY:Int,bombas:Int,colores:Int,partidas:Int=1): Int ={
    def jugarAIAux(tablero: List[List[Int]] , x: Int, y:Int,vidas:Int,puntos:Int,partidas:Int): Int={
      val vecinos= colindantes(tablero,x,y)
      if(buscarElemento(tablero,x,y) == 8){
        jugarAI(explosionBomba(tablero,x,y),vidas,puntos,sizeX,sizeY, bombas, colores, partidas)
      }else if (longitudLista(vecinos)>=3) jugarAI(eliminarElementos(tablero,vecinos),vidas,puntos+longitudLista(vecinos)*10,sizeX,sizeY, bombas, colores, partidas)
      else jugarAI(eliminarElementos(tablero,List(List(x,y))),vidas-1,0,sizeX,sizeY,bombas,colores,partidas)
    }

    def obtenerCoordenadasNoVacías(tablero: List[List[Int]], x:Int = 0, y:Int = 0): List[List[Int]]= {
      def obtenerCoordenadasNoVacíasColumna(columna: List[Int], x: Int, y:Int): List[List[Int]] = columna match {
        case Nil => List[List[Int]]()
        case 0::tail => obtenerCoordenadasNoVacíasColumna(tail, x, y+1)
        case _::tail => List(x, y) :: obtenerCoordenadasNoVacíasColumna(tail, x, y+1)
      }
      tablero match {
        case head::Nil => obtenerCoordenadasNoVacíasColumna(head, x, y )
        case head::tail => obtenerCoordenadasNoVacíasColumna(head, x,y) ::: obtenerCoordenadasNoVacías(tail, x+1,y)
      }
    }

    def coordenadasBomba(tablero: List[List[Int]], x:Int = 0, y:Int = 0): List[Int]= {
      def coordenadasBombaAux(columna: List[Int], x: Int, y:Int): List[Int] = columna match {
        case Nil => Nil
        case 8::tail => List(x,y)
        case _::tail => coordenadasBombaAux(tail, x, y+1)
      }
      if (!tablero.isEmpty){
        val bomba = coordenadasBombaAux(tablero.head,x,y)
        if (bomba.isEmpty) {
          coordenadasBomba(tablero.tail,x+1,y)
        } else {
          bomba
        }
      } else {
        Nil
      }
    }

    def decidirPosicionAux(tablero:List[List[Int]],coords:List[List[Int]]): List[Int] ={
      def colindantesAux(coord:List[Int], tablero:List[List[Int]]) = {
        colindantes(tablero, coord.head, coord.tail.head)
      }
      def obtenerListaMaxima(listaCoordenadas: List[List[List[Int]]]): List[List[Int]] = {
        if(listaCoordenadas.isEmpty)
          Nil
        else{
          val lista = obtenerListaMaxima(listaCoordenadas.tail)
          if(listaCoordenadas.head.length > lista.length)
            listaCoordenadas.head
          else
            lista
        }
      }
      val list = obtenerListaMaxima(coords.par.map(coord => colindantesAux(coord, tablero)).toList)
      if (list.length < 3) {
        val bomba = coordenadasBomba(tablero)
        if (bomba != Nil) bomba
        else list.head
      }
      else
        list.head
    }
    if(vidas>0 ) {
      if(!tablero.isEmpty) {
        print("   ")
        imprimirSecuencia(sizeX)
        mostrarTablero(tablero)
        print("\n")
        print(s"Vidas: $vidas Puntos: $puntos Partidas: $partidas\n")
        val jugada = decidirPosicionAux(tablero,obtenerCoordenadasNoVacías(tablero))
        jugarAIAux(tablero, jugada.head, jugada.tail.head, vidas, puntos,partidas)
      }else {
        val t =crearTablero(sizeX,sizeY,bombas,colores)
        print("   ")
        imprimirSecuencia(sizeX)
        mostrarTablero(t)
        print("\n")
        print(s"Vidas: $vidas Puntos: $puntos Partidas: $partidas\n")
        val jugada = decidirPosicionAux(t,obtenerCoordenadasNoVacías(t))
        jugarAIAux(t, jugada.head, jugada.tail.head, vidas, puntos,partidas+1)
      }
    }else puntos
  }

  // Imprime las coordenadas x al principio del tablero
  def imprimirSecuencia(n:Int,i:Int=0): Unit = {
    if (i < n) {
      printf(s"$i ")
      imprimirSecuencia(n, i + 1)
    }
    else if (i==n) (())
  }

  // Muestra la letra y el color en función del valor de la casilla
  def toLetter(n:Int): String= n match{
    case 0 => "  "
    case 1 => Console.BLUE_B+"A "+Console.RESET
    case 8 => Console.BLACK_B+"B "+Console.RESET
    case 7 => Console.WHITE_B+"G "+Console.RESET
    case 6 => Console.MAGENTA_B+"M "+Console.RESET
    case 3 => Console.YELLOW_B+"N "+Console.RESET
    case 5 => Console.CYAN_B+"P "+Console.RESET
    case 2 => Console.RED_B+"R "+Console.RESET
    case 4 => Console.GREEN_B+"V "+Console.RESET
  }
}