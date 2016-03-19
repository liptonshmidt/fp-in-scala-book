// Here's nice and funny example
// from Artem
// on gotchas beneath
// case classes nesting
case class Franchise(name: String) {
  case class Hero(name: String) {
    def be_friend_with(anotherHero: Hero): Unit = {
      println(s"$name and ${anotherHero.name} are friends now")
    }
  }
}


val marvel = new Franchise("Marvel")
val dc = new Franchise("DC")
val batman = new dc.Hero("Batman")
val joker = new dc.Hero("Joker")
batman.be_friend_with(joker)
val ironMan = new marvel.Hero("Iron Man")
// next(commented) statement won't compile:
// ironMan.be_friend_with(batman)

