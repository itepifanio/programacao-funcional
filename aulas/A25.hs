module A25 where

{- Revisão da aula 24 -}

data DbObject = DbString String 
              | DbInt    Int
              | DbBool   Bool
              deriving (Eq, Show)

counts :: [DbObject] -> (String, Int)
counts []                  = ("", 0)
counts (DbString s : objs) = (s ++ s', n')
    where (s', n') = counts objs 
counts (DbInt n : objs )   = (s', n + n')
    where (s', n') = counts objs
counts (_ : objs)          = counts objs

-- Implementar counts com foldr depois

{- Açucar sintático para melhor legibilidade  -}
type Name = String
type Age  = Int

{- Assim o acesso é feito sem problema de sequencia -}
{- data Person = Person 
    {age  :: Age
    ,name :: Name} -}
data Person = Person Name Age
    deriving (Show) 

age :: Person -> Age
age  (Person _ x) = x

name :: Person -> Name
name (Person x _) = x

italo :: Maybe Person
italo = mkPerson "italo" 21

noOne :: Maybe Person
noOne = mkPerson "" (-2)

{- Criando uma pessoa e validando -}
mkPerson :: Name -> Age -> Maybe Person
mkPerson x n
    | x == ""   = Nothing
    | n < 0     = Nothing
    | otherwise = Just (Person x n)

italo' :: Either String Person
italo' = mkPerson' "italo" 20

noOne' :: Either String Person
noOne' = mkPerson' "" (-2)

{- Criar uma lista de Either [PersonError] Person com todos os erros -}

data PersonError = InvalidAge | InvalidName

mkPerson' :: Name -> Age -> Either String Person
mkPerson' x n 
    | x == ""   = Left "nameless" -- [InvalidName]
    | n < 0     = Left "negative age" -- [InvalidAge]
    | otherwise = Right (Person x n)

{- Apenas exemplifica pattern matching
mail (Right name age)   = sendMail 
mail (Left InvalidName) = sendMail -}

succ' :: Maybe Int -> Maybe Int
succ' Nothing = Nothing
succ' (Just i) = Just (succ i)

even' :: Maybe Int -> Maybe Bool
even' Nothing  = Nothing
even' (Just n) = Just (even n)


