module Chawk.AST where


import qualified Data.Map as M

data Program = Program
    { rules     :: [Rule]
    , functions :: M.Map FunctionName Function
    }

type FunctionName = String

data Rule = Rule
    { pattern   :: Pattern
    , action    :: Action
    }

data Pattern
    = Pattern [Expression]
    | Always
    | Begin
    | End

type Action = [Statement]

data Statement
    = Break
    | Continue
    | Next
    | Delete Name [Expression]
    | Exit Expression
    | Return Expression
    | ExpressionStatement Expression
    | Print
        { format        :: Maybe Expression
        , fields        :: [Expression]
        , redirection   :: Maybe Redirection
        }
    | Loop
        { condition     :: Expression
        , body          :: Action
        , isDo          :: Bool
        }
    | If
        { condition     :: Expression
        , body          :: Action
        , alterative    :: Maybe Action
        }
    | For
        { initializer   :: Statement
        , condition     :: Expression
        , body          :: Action
        , increment     :: Statement
        }
    | ForEach
        { binding       :: Name
        , enumerand     :: Name
        , body          :: Action
        }

data Redirection
    = Truncate Expression
    | Append Expression
    | Pipe Expression

filename :: Redirection -> Maybe Expression
filename (Truncate e) = Just e
filename (Append e) = Just e
filename (Pipe {}) = Nothing

data LValue
    = DirectLValue Name
    | IndirectLValue Expression

data Expression
    = StringLiteral String
    | FloatLiteral Double
    | IntegerLiteral Integer
    | RegExpLiteral String
    | LValueReference LValue
    | Assignment AssignmentOperator LValue Expression
    | UnaryOperation UnaryOperator Expression
    | IncrementOperation IncrementOperator Expression
    | BinaryOperation
        { operator      :: BinaryOperator
        , isLazy        :: Bool
        , associativity :: Associativity
        , operands      :: [Expression]
        }
    | ConditionalExpression Expression Expression Expression
    | FunctionCall
        { callee    :: FunctionName
        , arguments :: [Expression]
        }

data Associativity
    = LeftAssociativity
    | RightAssociativity

data UnaryOperator
    = UnaryPlus
    | UnaryMinus
    | LogicalNot

data IncrementOperator
    = PreIncrement
    | PreDecrement
    | PostIncrement
    | PostDecrement

data BinaryOperator
    = Exponentiate
    | Times
    | Divide
    | Modulus
    | BinaryPlus
    | BinaryMinus
    | Concatenate
    | LessThan
    | LessThanOrEqualTo
    | EqualTo
    | NotEqualTo
    | GreaterThan
    | GreaterThanOrEqualTo
    | RegExpMatches
    | RegExpDoesNotMatch
    | In
    | MultiIn
    | LogicalAnd
    | LogicalOr
    | Lookup

data AssignmentOperator
    = SimpleAssignment
    | CompoundAssignment BinaryOperator

data Name
    = LocalName String
    | GlobalName String
    | WholeRecord
    | Field Int
    deriving (Eq)

data Function = Function
    { functionName  :: FunctionName
    , parameters    :: [String]
    , functionBody  :: Action
    }
