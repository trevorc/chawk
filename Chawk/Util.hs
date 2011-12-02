module Chawk.Util
( extractFst
) where


extractFst :: (a -> b) -> a -> (b, a)
extractFst f v = (f v, v)
