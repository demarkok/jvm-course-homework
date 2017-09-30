var b = 2
b match {
  case 2 => b = 3
  case 3 => b = 4
  case _ => b = 0
}

println(b)
