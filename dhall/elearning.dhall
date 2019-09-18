let List/map
  : ∀(a : Type) → ∀(b : Type) → (a → b) → List a → List b
  = λ(a : Type)
  → λ(b : Type)
  → λ(f : a → b)
  → λ(xs : List a)
  → List/build
      b
      ( λ(list : Type)
      → λ(cons : b → list → list)
      → List/fold a xs list (λ(x : a) → cons (f x))
      )

let User = { userName : Text, email : Text }

let mkUser =
  λ(userName : Text)
  → { userName = userName
    , email = userName ++ "@example.com"
    } : User

let userNames = 
  [ "ihan", "odila", "jaap", "nazim", "ada", "alper", "saiid", "hisam", "uzoma", "calu", "ezichi"
  , "brielle", "tam", "bryn", "husamettin", "delaney", "mary", "derwyn", "hadil", "dembe"
  ]


in (List/map Text User mkUser userNames)
