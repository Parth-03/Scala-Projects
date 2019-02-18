import hw.json._
import hw.wrangling.WranglingLike

object Wrangling extends WranglingLike {

  val data: List[Json] = JsonHelper.fromFile("yelp.json")

  def key(json: Json, key: String): Option[Json] = json match
{
	case JsonDict(aMap)=> aMap.get(JsonString(key))
	case _=> None
}




  def isFromState(datum: Json, state: String): Boolean= datum match
{
	case JsonDict(aMap) => aMap.get(JsonString("state")) match
	{
		case None=> false
		case Some(stateName)=> stateName match
		{
			case JsonString(x) => x==state
			case _=> false
		}
	}
	case _ => false
}

  def fromState(data: List[Json], state: String): List[Json] = 
{
	data.filter(datum=>isFromState(datum, state))
}





  def isRatingLT(datum: Json, rating: Double): Boolean = datum match
{
	case JsonDict(aMap) => aMap.get(JsonString("stars")) match
	{
		case None=> false
		case Some(ratingValue)=> ratingValue match
		{
			case JsonNumber(x) => x<=rating
			case _=> false
		}
	}
	case _ => false
}

  def ratingLT(data: List[Json], rating: Double): List[Json] = 
{
	data.filter(datum=>isRatingLT(datum, rating))
}





def isRatingGT(datum: Json, rating: Double): Boolean = datum match
{
	case JsonDict(aMap) => aMap.get(JsonString("stars")) match
	{
		case None=> false
		case Some(ratingValue)=> ratingValue match
		{
			case JsonNumber(x) => x>=rating
			case _=> false
		}
	}
	case _ => false
}

  def ratingGT(data: List[Json], rating: Double): List[Json] = 
{
	data.filter(datum=>isRatingGT(datum, rating))
}





def isCategory(datum: Json, category: String): Boolean= datum match
{
	case JsonDict(aMap) => aMap.get(JsonString("categories")) match
	{
		case None=> false
		case Some(categoryName)=> categoryName match
		{
			case JsonArray(alist) => alist.contains(category)
			case _=> false
		}
	}
	case _ => false
}

  def category(data: List[Json], category: String): List[Json] = 
{
	data.filter(datum=>isCategory(datum, category))
}






  def groupByState(data: List[Json]): Map[String, List[Json]] = 
{
	data.groupBy(datum=> datum match
	{
		case JsonDict(aMap)=> aMap.get(JsonString("state")) match
		{
			case Some(JsonString(stateCode))=> stateCode
			case _ =>"no state"
		}
		case _=>"no state"
	})
}





def categorizehelper(alist: List[Json]): List[String]= alist match
{
	case Nil=> Nil
	case head::tail=> head.toString :: categorizehelper(tail)
}

def categorize(json: Json): List[String]= json match
{
	case JsonDict(aMap)=> aMap.get(JsonString("categories")) match
	{
		case Some(JsonArray(alist))=> categorizehelper(alist)
		case _=> Nil
	}
	case _=> Nil
}

def dupCategory(json: Json): List[(String, Json)]= categorize(json).map(cat => (cat,json))

  def groupByCategory(data: List[Json]): Map[String, List[Json]] = 
{
	data.map(dupCategory).flatten.groupBy(tuple=> tuple._1).mapValues(data=> data.map(tuple=> tuple._2))
}




def rating(json: Json): Double= json match
{
	case JsonDict(aMap)=> aMap.get(JsonString("stars")) match
	{
		case Some(JsonNumber(x))=> x
		case _=> 0
	}
	case _=> 0
}

def reviewCount(json: Json): Double= json match 
{
	case JsonDict(aMap)=> aMap.get(JsonString("review_count")) match
	{
		case Some(JsonNumber(x))=> x
		case _=> 0
	}
	case _=> 0
}

def bestPlaceHelper(alist: List[Json], bestSoFar: Json): Json= alist match
{
	case Nil=> bestSoFar
	case head::tail=> 
	{
		if(rating(head) > rating(bestSoFar))
		{
			bestPlaceHelper(tail, head)
		}
		else if(rating(head)< rating(bestSoFar))
		{
			bestPlaceHelper(tail, bestSoFar)
		}
		else
		{
			if(reviewCount(head) > reviewCount(bestSoFar))
			{
				bestPlaceHelper(tail, head)
			}
			else
				bestPlaceHelper(tail, bestSoFar)
		}
	}
}

  def bestPlace(data: List[Json]): Option[Json] = data match
{
	case Nil=> None
	case head::tail=> Some(bestPlaceHelper(data, head))		
}






def doesHaveAmbience(datum: Json, ambience: String): Boolean= datum match
{
	case JsonDict(aMap)=> aMap.get(JsonString("attributes")) match
	{
		case None=> false
		case Some(attributeName)=> attributeName match
		{
			case JsonDict(aMap) => aMap.get(JsonString("ambience")) match
			{	
				case None=> false
				case Some(ambienceName)=> ambienceName match
				{
					case JsonString(x) => x==ambience
					case _=> false
				}
			}
			case _ => false
		}
	}
	case _=> false
}

  def hasAmbience(data: List[Json], ambience: String): List[Json] = 
{
	data.filter(datum=>doesHaveAmbience(datum, ambience))
}

}
