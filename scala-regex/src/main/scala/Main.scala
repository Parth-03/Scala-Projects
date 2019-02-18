import scala.util.matching.Regex

object Regexes extends hw.regex.RegexLike 
{
  def notAlphanumeric: Regex = """\W*""".r
  def time: Regex = """([0-1]\d)|(2[0-3]):[0-5]\d""".r
  def phone: Regex = """\(\d{3}\) \d{3}-\d{4}""".r
  def zip: Regex = """(\d{5})|(\d{5}-\d{4})""".r
  def comment: Regex = """(\/\*).*(\*\/)""".r
  def numberPhrase: Regex = """(twenty|thirty|forty|fifty|sixty|seventy|eighty|ninety)(-(one|two|three|four|five|six|seven|eight|nine))?""".r
  def roman: Regex = """(X{0-3})(I{0-3}|IV|VI{0-3}|IX)""".r
  def date: Regex = """(\d{4}-((0[13578])|(1[02])-(0[1-9])|([12]\d)|(3[01]))|((0[469])|11-(0[1-9])|([12]\d)|30)|(02-(0[1-9])|(1\d)|(2[0-8])))|(\d{2}(0[48])|([2468][048])|([13579][26])-02-29)|(([02468][048])|([13579][26])00-02-29)""".r
  def evenParity: Regex = """([02468]*(([13579][13579])+[02468]*)*|([13579][02468]*[13579]))*""".r
}