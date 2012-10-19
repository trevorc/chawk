module Chawk.AST where

import qualified Data.Map as M

data Program = Program
    { rules     :: [Rule]
    , functions :: M.Map FunctionName Function
    } deriving (Show)

type FunctionName = String

data Rule = Rule
    { pattern   :: Pattern
    , action    :: Action
    } deriving (Show)

data Pattern
    = Pattern Expression
    | PatternRange Expression Expression
    | Always
    | Begin
    | End
    deriving (Show)

newtype Action = Action
    { actionStatements :: [Statement]
    } deriving (Show)

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
        { isDo          :: Bool
        , condition     :: Expression
        , body          :: Action
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
    deriving (Show)

data Redirection
    = Truncate Expression
    | Append Expression
    | Pipe Expression
    deriving (Show)

filename :: Redirection -> Maybe Expression
filename (Truncate e) = Just e
filename (Append e) = Just e
filename (Pipe {}) = Nothing

data LValue
    = DirectLValue Name
    | IndirectLValue Expression
    deriving (Show)

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
    deriving (Show)

data Associativity
    = LeftAssociativity
    | RightAssociativity
    deriving (Show)

data UnaryOperator
    = UnaryPlus
    | UnaryMinus
    | LogicalNot
    deriving (Show)

data IncrementOperator
    = PreIncrement
    | PreDecrement
    | PostIncrement
    | PostDecrement
    deriving (Show)

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
    deriving (Show)

data AssignmentOperator
    = SimpleAssignment
    | CompoundAssignment BinaryOperator
    deriving (Show)

data Name
    = LocalName String
    | GlobalName String
    deriving (Eq, Show)

data Function = Function
    { functionName  :: FunctionName
    , parameters    :: [String]
    , functionBody  :: Action
    } deriving (Show)