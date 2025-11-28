case class Publicacion(id: Int, autor: String, texto: String, reacciones: List[String])
val publicaciones: List[Publicacion] = List(
  Publicacion(
    1,
    "Ana",
    "¡Nuevo taller de programación funcional la próxima semana!",
    List("like", "love", "like", "wow", "like")
  ),
  Publicacion(
    2,
    "Juan",
    "Recordatorio: mañana es la fecha límite para el proyecto final.",
    List("like", "like", "haha")
  ),
  Publicacion(
    3,
    "María",
    "¿Alguien recomienda buenos recursos para aprender Scala desde cero?",
    List("like", "love", "love", "wow", "haha", "like")
  ),
  Publicacion(
    4,
    "Carlos",
    "Se suspenden las clases de hoy por mantenimiento del sistema.",
    List("angry", "angry", "wow", "like")
  ),
  Publicacion(
    5,
    "Lucía",
    "Compartiendo un repositorio con ejemplos de programación reactiva.",
    List("love", "love", "like", "like", "wow")
  ),
  Publicacion(
    6,
    "Pedro",
    "No entiendo bien la diferencia entre programación declarativa e imperativa.",
    List("like", "haha", "haha")
  ),
  Publicacion(
    7,
    "Sofía",
    "Felicitaciones al equipo que ganó el hackatón de este semestre.",
    List("love", "love", "love", "wow", "like", "like")
  ),
  Publicacion(
    8,
    "Diego",
    "Subí un video explicando cómo usar map, filter y reduce en Scala.",
    List("like", "like", "wow", "wow", "haha")
  ),
  Publicacion(
    9,
    "Elena",
    "Encuesta: ¿prefieren proyectos individuales o en grupo para el curso?",
    List("like", "like", "like", "haha")
  ),
  Publicacion(
    10,
    "Fernando",
    "Actualización: ya está disponible el material de la semana 5 en el aula virtual.",
    List("like", "wow")
  ),
  Publicacion(
    11,
    "Raúl",
    "Me parece injusto el sistema de evaluación del laboratorio.",
    List("angry", "angry", "like")
  ),
  Publicacion(
    12,
    "Patricia",
    "Gracias por todas las sugerencias de recursos, ¡me han servido mucho!",
    List("love", "love", "love", "wow", "like", "like", "haha")
  )
)


/*Deseo que ha este codigo, lo hagas mas funcional, que tenga las caracteristicas
de un codigo de scala bien estructurado, quiero que no utilices
sentencias avanzadas. Hagamoslo mas corto, pero que me lleve a la misma
respuesta*/


case class PublicacionConPuntaje(pub: Publicacion, puntaje: Int)

def publicacionMasImpactante(
                              publicaciones: List[Publicacion],
                              minLen: Int
                            ): Option[PublicacionConPuntaje] = {

  val valorReaccion = Map(
    "like"  -> 1,
    "love"  -> 3,
    "wow"   -> 2,
    "haha"  -> 1,
    "angry" -> -1
  )

  publicaciones
    .filter(_.texto.length >= minLen)

    .map { p =>
      val puntaje =
        p.reacciones
          .map(_.toLowerCase)
          .flatMap(valorReaccion.get)
          .sum

      PublicacionConPuntaje(p, puntaje)
    }

    .maxByOption(_.puntaje)
}


publicacionMasImpactante(publicaciones, 55).foreach { r =>
  println(s"ID: ${r.pub.id}, Autor: ${r.pub.autor}")
  println(s"Texto: ${r.pub.texto}")
  println(s"Puntaje: ${r.puntaje}")
}

