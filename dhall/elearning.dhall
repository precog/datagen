let List/map = https://raw.githubusercontent.com/dhall-lang/dhall-lang/v9.0.0/Prelude/List/map
let List/concat = https://raw.githubusercontent.com/dhall-lang/dhall-lang/v9.0.0/Prelude/List/concat

let Course = { id : Natural, course : Text, completed : Text }
let Courses = { courses: List Course }

let course1 = { id = 1, course = "Introduction campaign management", completed = "2017-03-14" }
let course2 = { id = 2, course = "Advanced campaign management", completed = "2017-07-04" }
let course3 = { id = 3, course = "Running efficient campaigns", completed = "2017-08-24" }
let course4 = { id = 4, course = "Campaign management for example.com", completed = "2018-02-01" }
let course5 = { id = 5, course = "Running an efficient cross platform marketing campaign", completed = "2018-10-13" }

let courses1 = [ course1 ]
let courses2 = List/concat Course [ courses1, [ course2 ] ]
let courses3 = List/concat Course [ courses2, [ course3 ] ]
let courses4 = List/concat Course [ courses3, [ course4 ] ]
let courses5 = List/concat Course [ courses4, [ course5 ] ]

let User = 
  { userName : Text
  , email : Text 
  }

let Business = 
  { business_id : Text
  , business_name: Text
  , business_street: Text
  , business_street_number: Natural
  , business_city: Text
  , business_state: Text
  , business_country: Text }

let business =
  { business_id = "2c7cab19-ae85-4bf0-b043-9d917a4b19d7" 
  , business_name = "Example Inc."
  , business_street = "42nd Sample Street"
  , business_street_number = 21
  , business_city = "Boulder"
  , business_state = "CO"
  , business_country = "USA"
  }

let mkUser 
  = λ(userName : Text)
  → { userName = userName
    , email = userName ++ "@example.com"
    } : User

let Desc = { user : Text, courses: List Course }
let Struct = { user : User ⩓ Business, courses: List Course }

let mk 
  = λ(desc: Desc) 
  → { user = mkUser desc.user } /\ { courses = desc.courses } /\ { user = business }: Struct

let desc =
  [ { user = "ihan", courses = courses1 }
  , { user = "odila", courses = courses5 }
  , { user = "jaap", courses = courses2 }
  , { user = "nazim", courses = courses5 }
  , { user = "ada", courses = courses3 }
  , { user = "alper", courses = courses5 }
  , { user = "saiid", courses = courses4 }
  , { user = "hisam", courses = courses5 }
  , { user = "uzoma", courses = courses2 }
  , { user = "calu", courses = courses5 }
  , { user = "ezichi", courses = courses3 }
  , { user = "brielle", courses = courses5 }
  , { user = "tam", courses = courses4 }
  , { user = "bryn", courses = courses5 }
  , { user = "husamettin", courses = courses2 }
  , { user = "delaney", courses = courses5 }
  , { user = "mary", courses = courses3 }
  , { user = "derwyn", courses = courses5 }
  , { user = "hadil", courses = courses4 }
  , { user = "dembe", courses = courses5 }
  ]

in (List/map Desc Struct mk desc)
