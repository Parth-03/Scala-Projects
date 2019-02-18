
import java.nio.file._
import java.time.LocalDate
object PathImplicits 
{
  implicit class RichPath(path: Path)
  {
    def /(s: String): Path = path.resolve(s)
    def /(p: Path): Path= path.resolve(p)
    def write(s: String): Path= Files.write(path,s.getBytes)
    def read(): String= Files.readAllBytes(path).mkString
    def append(s: String): Path= 
    {
      if(Files.exists(path))
        Files.write(path,s.getBytes,StandardOpenOption.APPEND)
      else
        Files.write(path,s.getBytes)
    }
  }
  implicit class RichString(str: String)
  {
    def /(s: String): Path= Paths.get(str).resolve(s)
    def /(p: Path): Path = Paths.get(str).resolve(p)
  }
}

object TimeImplicits 
{
  implicit class RichInt(n: Int)
  {
    def jan(): LocalDate= LocalDate.of(LocalDate.now().getYear,1,n)
    def feb(): LocalDate= LocalDate.of(LocalDate.now().getYear,2,n)
    def mar(): LocalDate= LocalDate.of(LocalDate.now().getYear,3,n)
    def apr(): LocalDate= LocalDate.of(LocalDate.now().getYear,4,n)
    def may(): LocalDate= LocalDate.of(LocalDate.now().getYear,5,n)
    def jun(): LocalDate= LocalDate.of(LocalDate.now().getYear,6,n)
    def jul(): LocalDate= LocalDate.of(LocalDate.now().getYear,7,n)
    def aug(): LocalDate= LocalDate.of(LocalDate.now().getYear,8,n)
    def sep(): LocalDate= LocalDate.of(LocalDate.now().getYear,9,n)
    def oct(): LocalDate= LocalDate.of(LocalDate.now().getYear,10,n)
    def nov(): LocalDate= LocalDate.of(LocalDate.now().getYear,11,n)
    def dec(): LocalDate= LocalDate.of(LocalDate.now().getYear,12,n)

    def jan(year: Int): LocalDate= LocalDate.of(year,1,n)
    def feb(year: Int): LocalDate= LocalDate.of(year,2,n)
    def mar(year: Int): LocalDate= LocalDate.of(year,3,n)
    def apr(year: Int): LocalDate= LocalDate.of(year,4,n)
    def may(year: Int): LocalDate= LocalDate.of(year,5,n)
    def jun(year: Int): LocalDate= LocalDate.of(year,6,n)
    def jul(year: Int): LocalDate= LocalDate.of(year,7,n)
    def aug(year: Int): LocalDate= LocalDate.of(year,8,n)
    def sep(year: Int): LocalDate= LocalDate.of(year,9,n)
    def oct(year: Int): LocalDate= LocalDate.of(year,10,n)
    def nov(year: Int): LocalDate= LocalDate.of(year,11,n)
    def dec(year: Int): LocalDate= LocalDate.of(year,12,n)
    def days(): (Long, String)= (n,"days")
    def months(): (Long, String)= (n,"months")
    def years(): (Long, String)= (n,"years")
  }

  implicit class RichDate(date: LocalDate)
  {
    def +(n:(Long,String)): LocalDate=
    {
      if(n._2 == "days")
        date.plusDays(n._1)
      else if(n._2 == "months")
        date.plusMonths(n._1)
      else 
        date.plusYears(n._1)
    }     
  }

}