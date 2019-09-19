let List/map = https://raw.githubusercontent.com/dhall-lang/dhall-lang/v9.0.0/Prelude/List/map
let List/concat = https://raw.githubusercontent.com/dhall-lang/dhall-lang/v9.0.0/Prelude/List/concat

let Lesson = { completed: Text }
let Course = { id: Natural, lessons: List Lesson }
let CourseMap = List { mapKey: Text, mapValue: Course }
let Courses = { courses: List CourseMap }

let lesson1 = { completed = "2017-01-01" }
let lesson2 = { completed = "2017-02-01" }
let lesson3 = { completed = "2017-03-01" }
let lesson4 = { completed = "2017-04-01" }
let lesson5 = { completed = "2017-05-01" }
let lesson6 = { completed = "2017-06-01" }
let lesson7 = { completed = "2017-07-01" }
let lesson8 = { completed = "2017-08-01" }
let lesson9 = { completed = "2017-09-01" }
let lesson10 = { completed = "2017-10-01" }
let lesson11 = { completed = "2017-11-01" }
let lesson12 = { completed = "2017-12-01" }

let lessons1 = [ lesson1, lesson3, lesson4, lesson6, lesson7 ]
let lessons2 = [ lesson2, lesson5, lesson8, lesson9, lesson10 ]
let lessons3 = [ lesson3, lesson4, lesson9, lesson11, lesson12 ]
let lessons4 = [ lesson4, lesson7, lesson8, lesson10, lesson12 ]
let lessons5 = [ lesson5, lesson6, lesson7, lesson8, lesson11 ]

let course1 = toMap { `Introduction campaign management` = { id = 1, lessons = lessons1 } }
let course2 = toMap { `Advanced campaign management` = { id = 2, lessons = lessons2 } }
let course3 = toMap { `Running efficient campaigns` = { id = 3, lessons = lessons3 } }
let course4 = toMap { `Campaign management for example.com` = { id = 4, lessons = lessons4 } }
let course5 = toMap { `Running an efficient cross platform marketing campaign` = { id = 5, lessons = lessons5 } }

let courses0 =
  [ toMap
    { `Data Protection Act 2018` = { id = 101, lessons = lessons1 }
    , `General Data Protection` = { id = 102, lessons = lessons2 }
    , `Regulations (GDPR)` = { id = 103, lessons = lessons3 }
    , `Equality Act` = { id = 104, lessons = lessons4 }
    , `Bribery Act` = { id = 105, lessons = lessons5 }
    , `Criminal Finances Act` = { id = 106, lessons = lessons1 }
    , `Gender Pay` = { id = 107, lessons = lessons2 }
    , `Modern Slavery` = { id = 108, lessons = lessons3 }
    , `Health and Safety` = { id = 109, lessons = lessons4 }
    }
  ]
let courses1 = List/concat CourseMap [ courses0, [ course1 ] ]
let courses2 = List/concat CourseMap [ courses1, [ course2 ] ]
let courses3 = List/concat CourseMap [ courses2, [ course3 ] ]
let courses4 = List/concat CourseMap [ courses3, [ course4 ] ]
let courses5 = List/concat CourseMap [ courses4, [ course5 ] ]

let User =
  { userName: Text
  , email: Text
  }

let Business =
  { business_id: Text
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

let Salary =
  { scale: Natural
  , since: Text
  }

let salary1 = { scale = 1, since = "2019-01-01" }
let salary2 = { scale = 2, since = "2017-01-01" }
let salary3 = { scale = 3, since = "2017-01-01" }
let salary4 = { scale = 4, since = "2018-01-01" }
let salary5 = { scale = 5, since = "2016-01-01" }

let Other =
  { sickleave: List Text
  , holidays: List Text
  , promotions: List Text
  , bonuses: List Text
  , confidential: List Text
  , projects: List Text
  }

let other =
  { sickleave = [ "2017-03-24", "2018-11-12" ]
  , holidays = [ "2017-06-07 2017-07-02", "2018-07-12 2018-07-21"]
  , promotions = [ "2016-07-01", "2018-04-01" ]
  , bonuses = [ "2017-01-01", "$5000" ]
  , confidential = [ "secret" ]
  , projects =
    [ "3e3bde5e-65be-4bca-97db-8ce3784832b9"
    , "f2c979a6-f1f2-4298-954d-ace9f8f0372d"
    , "30cfaa89-1f1d-4acc-a83e-9b0914db900e"
    ]
  }

let mkUser
  = λ(userName: Text)
  → { userName = userName
    , email = userName ++ "@example.com"
    }
    : User

let Desc = { user: Text, salary: Salary } ⩓ Courses
let Struct = { user: User ⩓ Business, salary: Salary } ⩓ Courses ⩓ Other

let mk
  = λ(desc: Desc)
  → { user = mkUser desc.user }
  ∧ { user = business }
  ∧ { courses = desc.courses }
  ∧ { salary = desc.salary }
  ∧ other
  : Struct

let desc =
  [ { user = "ihan", courses = courses1, salary = salary1 }
  , { user = "odila", courses = courses5, salary = salary2 }
  , { user = "jaap", courses = courses2, salary = salary1 }
  , { user = "nazim", courses = courses5, salary = salary1 }
  , { user = "ada", courses = courses3, salary = salary3 }
  , { user = "alper", courses = courses5, salary = salary1 }
  , { user = "saiid", courses = courses4, salary = salary4 }
  , { user = "hisam", courses = courses5, salary = salary5 }
  , { user = "uzoma", courses = courses2, salary = salary1 }
  , { user = "calu", courses = courses5, salary = salary2 }
  , { user = "ezichi", courses = courses3, salary = salary2 }
  , { user = "brielle", courses = courses5, salary = salary1 }
  , { user = "tam", courses = courses4, salary = salary1 }
  , { user = "bryn", courses = courses5, salary = salary3 }
  , { user = "husamettin", courses = courses2, salary = salary1 }
  , { user = "delaney", courses = courses5, salary = salary4 }
  , { user = "mary", courses = courses3, salary = salary1 }
  , { user = "derwyn", courses = courses5, salary = salary1 }
  , { user = "hadil", courses = courses4, salary = salary2 }
  , { user = "dembe", courses = courses5, salary = salary1 }
  ]

in (List/map Desc Struct mk desc)
