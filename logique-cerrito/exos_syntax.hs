type Variables = String
data Connectors = V | F deriving Show

data Alphabet = Var Variables | Conn Connectors

data Formulas =
  Atomic Alphabet 
  | NOT Formulas
  | AND Formulas Formulas
  | OR Formulas Formulas
  | IMP Formulas Formulas
  | EQUI Formulas Formulas

instance Show Alphabet where
  show (Var v) = v
  show (Conn c) = show c

instance Show Formulas where
  show (Atomic f) = show f
  show (NOT f) = "¬" ++ show f
  show (AND f1 f2) = show f1 ++ " ∧ " ++ show f2
  show (OR f1 f2) = show f1 ++ " ∨ " ++ show f2
  show (IMP f1 f2) = show f1 ++ " → " ++ show f2
  show (EQUI f1 f2) = show f1 ++ " ↔︎ " ++ show f2

sub_formulas :: Formulas -> String
sub_formulas ff@(Atomic f) = "{" ++ show ff ++ "}"
sub_formulas ff@(NOT f) = "{" ++ show ff ++"} U " ++ sub_formulas f  
sub_formulas ff@(AND f1 f2) = "{" ++ show ff ++"} U " ++ sub_formulas f1 ++ sub_formulas f2
sub_formulas ff@(OR f1 f2) = "{" ++ show ff ++"} U " ++ sub_formulas f1 ++ sub_formulas f2
sub_formulas ff@(IMP f1 f2) = "{" ++ show ff ++"} U " ++ sub_formulas f1 ++ sub_formulas f2
sub_formulas ff@(EQUI f1 f2) = "{" ++ show ff ++"} U " ++ sub_formulas f1 ++ sub_formulas f2

-- >>> NOT (Atomic (Var "P"))
-- ¬P

-- >>> let formula = IMP (NOT (AND (Atomic (Var "P")) (Atomic (Var "Q")))) (Atomic (Var "A"))
-- >>> formula
-- ¬P ∧ Q → A

-- >>> sub_formulas formula
-- "{¬P ∧ Q → A} U {¬P ∧ Q} U {P ∧ Q} U {P}{Q}{A}"
