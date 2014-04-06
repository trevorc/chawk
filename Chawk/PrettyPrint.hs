module Chawk.PrettyPrint (PrettyPrintable) where

class PrettyPrintable a where
  pretty :: a -> String

indent = unlines . map ('\t':) . lines

wrap open close middle = open ++ pretty middle ++ close

brackets = wrap "[" "]"
parens = wrap "(" ")"
commas = concatMap (++ ", " . pretty)
space = wrap " " " "
headerize str = (str ++) . space . parens

instance PrettyPrintable String where
  pretty = id

instance PrettyPrintable Program where
  pretty (Program rules funcs) = unlines ruleTxts ++ unlines funcTxts where
    ruleTxts = map pretty rules
    funcTxts = map pretty $ elems funcs

instance PrettyPrintable Rule where
  pretty (Rule patt act) = pretty patt ++ " " ++ pretty act

instance PrettyPrintable Pattern where
  pretty (Pattern exp) = pretty exp
  pretty (PatternRange exp1 exp2) = pretty exp1 ++ ", " ++ pretty exp2
  pretty Always = ""
  pretty Begin = "BEGIN"
  pretty End = "END"

instance PrettyPrintable Action where
  pretty (Action stmts) = "{" ++ indent contents ++ "}" where
    contents = unlines $ map pretty stmts

instance PrettyPrintable Redirection where
  pretty (Truncate expr) = ">" ++ space expr
  pretty (Append expr) = ">>" ++ space expr
  pretty (Pipe expr) = "|" ++ space expr

instance PrettyPrintable Statement where
  pretty Break = "break;"
  pretty Continue = "continue;"
  pretty Next = "next;"
  pretty (Delete nm exprs) = "delete" ++ space nm ++ brackets txt where
    txt = commas exprs
  pretty (Exit expr) = undefined
  pretty (Return e) = "return " ++ pretty e ++ ";"
  pretty (ExpressionStatement e) = pretty e ++ ";"
  pretty (Print Nothing flds red) = "print" ++ flds' ++ red' ++ ";" where
    flds' = space $ commas flds
    red' = fromMaybe "" $ liftM space red
  pretty (Loop False cond bdy) = header ++ pretty body where
    header = headerize "while" cond
  pretty (Loop True cond bdy) = "do " ++ pretty bdy ++ footer where
    footer = wrap "while" ";" $ space $ parens cond
  pretty (If cond bdy Nothing) = header ++ pretty bdy where
    header = headerize "if" cond
  pretty i@(If cond bdy (Just alt)) = noElseVersion ++ elseClause where
      noElseVersion = pretty $ i { alternative = Nothing }
      elseClause = " else " ++ pretty alt
  pretty (For init cond bdy incr) = header ++ pretty body where
    header = headerize "for" header'
    header' = concat $ intersperse ";" [ space init
                                       , space cond 
                                       , space incr
                                       ]
  pretty (ForEach bnd enum bdy) = header ++ pretty bdy where
    header = headerize "for" $ space bnd ++ "in" ++ space enum

instance PrettyPrint LValue where
  pretty (DirectLValue nm) = pretty nm
  pretty (IndirectLValue expr) = parens $ "$" ++ pretty expr

instance PrettyPrint Expression where
  pretty _ = undefined
