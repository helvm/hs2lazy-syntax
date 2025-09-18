module HS2Lazy.Syntax where
import Data.Char(chr, ord)
import Data.List(find, nub, union, intersect, (\\))
import HS2Lazy.SCC

type Id  = String
type Alt = ([Pat], Rhs)
type Expl = (Id, Scheme, [Alt])
type Impl   = (Id, [Alt])
type BindGroup  = ([Expl], [[Impl]])
type Program = [BindGroup]

data Kind  = Star | Kfun Kind Kind
             deriving (Eq, Show)

class Assoc a where
    assocKey :: a -> String
    assoc :: String -> [a] -> Maybe a
    assoc key [] = Nothing
    assoc key (x:xs) = if assocKey x == key then Just x else assoc key xs

-----------------------------------------------------------------------------
-- Type:		Types
-----------------------------------------------------------------------------

data Type  = TVar Tyvar
           | TCon Tycon
           | TAp Type Type
           | TGen Int
           | TSynonym Synonym [Type]
             deriving Eq



fromTAp :: Type -> [Type]
fromTAp (TAp t1 t2) = fromTAp t1 ++ [t2]
fromTAp t = [t]

data Tyvar = Tyvar Id Kind deriving Eq

data Tycon = Tycon { tyconName::Id,
                     tyconKind::Kind,
                     tyconNumCon::Int,
                     tyconArities::[Int]
                   } deriving Eq

instance Show Tycon where
    show tc = tyconName tc

data Synonym = Synonym Id Kind [Tyvar] Type deriving Eq

unsynonym :: Synonym -> [Type] -> Type
unsynonym (Synonym _ _ vs t) ts = apply s t
    where s = zip vs ts

tChar    = TCon (Tycon "Char" Star 0 [])
tInt     = TCon (Tycon "Int" Star 0 [])
tBool    = TCon (Tycon "Bool" Star 2 [0,0])
tUnit    = TCon (Tycon "()" Star 1 [0])
tList    = TCon (Tycon "[]" (Kfun Star Star) 2 [2,0])
tArrow   = TCon (Tycon "(->)" (Kfun Star (Kfun Star Star)) 0 [])

tString    :: Type
tString     = list tChar

preludeTycons :: [Tycon]
preludeTycons = [Tycon "()" Star 1 [0],
		 Tycon "Char" Star 0 [],
		 Tycon "Int" Star 0 [],
		 Tycon "Bool" Star 2 [0,0],
		 Tycon "[]" (Kfun Star Star) 2 [2,0],
		 Tycon "(->)" (Kfun Star (Kfun Star Star)) 0 []
                ]

preludeSynonyms :: [Synonym]
preludeSynonyms = [Synonym "String" Star [] (list tChar)
                  ]

preludeConstrs :: [Const]
preludeConstrs = [Const { conName = i,
                          conArity = a,
                          conTag = tag,
                          conTycon = tycon,
                          conScheme = quantifyAll' t }
                      | (i, a, tag, TCon tycon, t) <- constrs]
    where a = TVar (Tyvar "a" Star)
          constrs = [("True", 0, 1, tBool, tBool),
		     ("False", 0, 2, tBool, tBool),
                     (":", 2, 1, tList, a `fn` list a `fn` list a),
		     ("[]", 0, 2, tList, list a)]

eTrue = Con con
    where Just con = find (\c -> conName c == "True") preludeConstrs
eFalse = Con con
    where Just con = find (\c -> conName c == "False") preludeConstrs
eCons = Con con
    where Just con = find (\c -> conName c == ":") preludeConstrs
eNil = Con con
    where Just con = find (\c -> conName c == "[]") preludeConstrs
pCons x y = PCon con [x, y]
    where Just con = find (\c -> conName c == ":") preludeConstrs
pNil = PCon con []
    where Just con = find (\c -> conName c == "[]") preludeConstrs

infixr      4 `fn`
fn         :: Type -> Type -> Type
a `fn` b    = TAp (TAp tArrow a) b

list       :: Type -> Type
list t      = TAp tList t

pair       :: Type -> Type -> Type
pair a b    = TCon (tupTycon 2) `fn` a `fn` b

class HasKind t where
  kind :: t -> Kind
instance HasKind Tyvar where
  kind (Tyvar v k) = k
instance HasKind Tycon where
  kind tc = tyconKind tc
instance HasKind Synonym where
  kind (Synonym _ k _ _) = k
instance HasKind Type where
  kind (TCon tc) = kind tc
  kind (TVar u)  = kind u
  kind (TAp t _) = case (kind t) of
                     (Kfun _ k) -> k
  kind (TSynonym syn ts) = kind (unsynonym syn ts)

type Subst  = [(Tyvar, Type)]

class Types t where
  apply :: Subst -> t -> t
  tv    :: t -> [Tyvar]

instance Types Type where
  apply s (TVar u)  = case lookup u s of
                       Just t  -> t
                       Nothing -> TVar u
  apply s (TAp l r) = TAp (apply s l) (apply s r)
  apply s t         = t

  tv (TVar u)  = [u]
  tv (TAp l r) = tv l `union` tv r
  tv t         = []


-- Predicates
data Qual t = [Pred] :=> t
              deriving Eq

data Pred   = IsIn Id Type
              deriving Eq



-- Type schemes
data Scheme = Forall [Kind] (Qual Type)
              deriving Eq

instance Show Scheme where
    showsPrec _ (Forall _ qt) = shows qt

instance Types Scheme where
  apply s (Forall ks t) = Forall ks (apply s t)
  tv (Forall ks t)      = tv t

quantify      :: [Tyvar] -> Qual Type -> Scheme
quantify vs qt = Forall ks (apply s qt)
 where vs' = [ v | v <- tv qt, v `elem` vs ]
       ks  = map kind vs'
       s   = zip vs' (map TGen [0..])

quantifyAll :: Qual Type -> Scheme
quantifyAll t = quantify (tv t) t

quantifyAll' :: Type -> Scheme
quantifyAll' t = quantify (tv t) ([] :=> t)

toScheme      :: Type -> Scheme
toScheme t     = Forall [] ([] :=> t)

-- Assumptions
data Assump = Id :>: Scheme



findAssump :: MonadFail m => Id -> [Assump] -> m Scheme
findAssump id [] = fail ("unbound identifier: " ++ id)
findAssump id ((i:>:sc):as) = if i == id then return sc else findAssump id as

-- Literals
data Literal = LitInt  Int
             | LitChar String
             | LitStr  String
               deriving Eq



-- Patterns
data Pat  = PVar Id
          | PWildcard
          | PAs  Id Pat
          | PLit Literal
          | PCon Const [Pat]

data Expr = Var     Id
	  | Lit     Literal
	  | Con     Const
	  | Ap      Expr Expr
	  | Let     BindGroup Expr
	  | Case    Expr [(Pat, Rhs)]
	  | Lambda  Alt
	  | ESign   Expr Scheme
          | RecPH   Id
          | ClassPH Pred

data Rhs = Rhs Expr
         | Where BindGroup Rhs
         | Guarded [(Expr, Expr)]

data Const = Const { conName::Id,
                     conArity::Int,
                     conTag::Int,
                     conTycon::Tycon,
                     conScheme::Scheme }



ap :: Expr -> [Expr] -> Expr
ap = foldl Ap

bindings :: BindGroup -> [Impl]
bindings (es, iss) = [(i, as) | (i, _, as) <- es] ++ concat iss

class HasVar t where
    freeVars :: t -> [Id]





fvBindGroup :: BindGroup -> [Id]
fvBindGroup bg = fvAlts (concat altss) \\ is
    where (is, altss) = unzip (bindings bg)

fvAlts :: [Alt] -> [Id]
fvAlts alts = foldl1 union (map fvAlt alts)
fvAlt :: Alt -> [Id]
fvAlt (ps, rhs) = freeVars rhs \\ concat (map patVars ps)

patVars :: Pat -> [Id]
patVars (PVar i) = [i]
patVars (PAs i p) = i : patVars p
patVars (PCon _ ps) = concat (map patVars ps)
patVars _ = []

tupcon :: Int -> Const
tupcon n = Const "(,)" n 1 tycon sc
    where tycon = tupTycon n
          tuptype = foldl TAp (TCon tycon) tvars
{-
	  tvars = [TVar (Tyvar ('v' : show i) Star) | i <- [0..n-1]]
          scheme = quantifyAll (foldr fn tuptype tvars)
-}
	  tvars = [TGen i | i <- [0..n-1]]
          sc = Forall (replicate n Star) ([] :=> foldr fn tuptype tvars)


tupTycon :: Int -> Tycon
tupTycon n = Tycon "(,)" (foldr Kfun Star (replicate n Star)) 1 [0]

tuple :: [Expr] -> Expr
tuple es = foldl Ap (Con $ tupcon $ length es) es

tupleSelector :: String -> Int -> Int -> Impl
tupleSelector id k n = (id, [([pat], Rhs expr)])
    where pat = PCon (tupcon n) [PVar ('e' : show i) | i <- [0..n-1]]
          expr = Var ('e' : show k)

-- type class

type Class    = ([Id], [Inst], [Assump])
type Inst     = (Qual Pred, Expr)

data ClassEnv = ClassEnv { classes  :: Id -> Maybe Class,
                           defaults :: [Type],
                           impls    :: [Impl],
                           expls    :: [Expl],
                           assumps  :: [Assump] }

type EnvTransformer = ClassEnv -> Maybe ClassEnv
idEnvTransformer :: EnvTransformer
idEnvTransformer ce = Just ce

infixr 5 <:>
(<:>)       :: EnvTransformer -> EnvTransformer -> EnvTransformer
(f <:> g) ce = do ce' <- f ce
                  g ce'

-- SKI expression
data SKI = SAp  SKI SKI
	 | SLit Literal
	 | SVar Id
	 | SCon Int Int

sap :: SKI -> [SKI] -> SKI
sap = foldl SAp

dependency :: [Impl] -> [[Impl]]
dependency bs = (map . map) (\v -> (v, lookup' v bs)) (reverse vss)
    where vs = map fst bs
	  vss = scc [(v, fvAlts alts `intersect` vs) | (v, alts) <- bs]
	  lookup' key xs = case lookup key xs of
			   Just x -> x
			   Nothing -> error "cannot occur"
