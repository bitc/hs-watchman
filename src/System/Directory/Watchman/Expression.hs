{-# LANGUAGE OverloadedStrings #-}

module System.Directory.Watchman.Expression
    ( Expression
    , renderExpression

    , true
    , false
    , System.Directory.Watchman.Expression.all
    , System.Directory.Watchman.Expression.any
    , (.&&)
    , (.||)
    , dirname
    , dirname'
    , empty
    , exists
    , match
    , match'
    , name
    , name'
    , System.Directory.Watchman.Expression.not
    , size
    , suffix
    , type_

    , caseSensitive
    , caseInsensitive
    , basename
    , wholename
    , depth
    , includeDotFiles
    , noEscape
    ) where

import Data.Foldable (foldl')
import Data.ByteString (ByteString)
import Data.Int
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq

import System.Directory.Watchman.FileType
import System.Directory.Watchman.WFilePath
import System.Directory.Watchman.BSER

data CaseSensitivity
    = CaseSensitive
    | CaseInsensitive
    deriving (Show, Eq, Ord)

class HasCaseSensitivityOption a where
    setCaseSensitivity :: CaseSensitivity -> a -> a

data PathScope
    = BaseName
    | WholeName
    deriving (Show, Eq, Ord)

class HasPathScopeOption a where
    setPathScope :: PathScope -> a -> a

data Expression
    = EAllOf ![Expression]
    | EAnyOf ![Expression]
    | EDirName !WFilePath !DirNameParams
    | ETrue
    | EFalse
    | EEmpty
    | EExists
    | EMatch !ByteString !MatchParams
    | EName ![WFilePath] !NameParams
    | ESince () -- TODO !!!
    | ENot !Expression
    | ESize !(Comparison Int64)
    | ESuffix !ByteString
    | EType !FileType
    deriving (Show, Eq, Ord)

data DirNameParams = DirNameParams
    { _DirNameParams_Depth :: !(Comparison Int)
    , _DirNameParams_CaseSensitivity :: !CaseSensitivity
    }
    deriving (Show, Eq, Ord)

defaultDirNameParams :: DirNameParams
defaultDirNameParams = DirNameParams
    { _DirNameParams_Depth = Ge 0
    , _DirNameParams_CaseSensitivity = CaseSensitive
    }

instance HasCaseSensitivityOption DirNameParams where
    setCaseSensitivity c x = x { _DirNameParams_CaseSensitivity = c }

data MatchParams = MatchParams
    { _MatchParams_CaseSensitivity :: !CaseSensitivity
    , _MatchParams_PathScope :: !PathScope
    , _MatchParams_IncludeDotFiles :: !Bool
    , _MatchParams_NoEscape :: !Bool
    }
    deriving (Show, Eq, Ord)

defaultMatchParams :: MatchParams
defaultMatchParams = MatchParams
    { _MatchParams_CaseSensitivity = CaseSensitive
    , _MatchParams_PathScope = BaseName
    , _MatchParams_IncludeDotFiles = False
    , _MatchParams_NoEscape = False
    }

instance HasCaseSensitivityOption MatchParams where
    setCaseSensitivity c x = x { _MatchParams_CaseSensitivity = c }

instance HasPathScopeOption MatchParams where
    setPathScope c x = x { _MatchParams_PathScope = c }

data NameParams = NameParams
    { _NameParams_CaseSensitivity :: !CaseSensitivity
    , _NameParams_PathScope :: !PathScope
    }
    deriving (Show, Eq, Ord)

defaultNameParams :: NameParams
defaultNameParams = NameParams
    { _NameParams_CaseSensitivity = CaseSensitive
    , _NameParams_PathScope = BaseName
    }

instance HasCaseSensitivityOption NameParams where
    setCaseSensitivity c x = x { _NameParams_CaseSensitivity = c }

instance HasPathScopeOption NameParams where
    setPathScope c x = x { _NameParams_PathScope = c }

true :: Expression
true = ETrue

false :: Expression
false = EFalse

all :: [Expression] -> Expression
all = EAllOf

any :: [Expression] -> Expression
any = EAnyOf

infixr 3 .&&
(.&&) :: Expression -> Expression -> Expression
lhs .&& rhs = EAllOf [lhs, rhs]

infixr 2 .||
(.||) :: Expression -> Expression -> Expression
lhs .|| rhs = EAnyOf [lhs, rhs]

dirname :: WFilePath -> Expression
dirname path = EDirName path defaultDirNameParams

dirname' :: WFilePath -> [DirNameParams -> DirNameParams] -> Expression
dirname' path modifiers = EDirName path (applyModifiers defaultDirNameParams modifiers)

empty :: Expression
empty = EEmpty

exists :: Expression
exists = EExists

match :: ByteString -> Expression
match pattern = EMatch pattern defaultMatchParams

match' :: ByteString -> [MatchParams -> MatchParams] -> Expression
match' pattern modifiers = EMatch pattern (applyModifiers defaultMatchParams modifiers)

name :: [WFilePath] -> Expression
name files = EName files defaultNameParams

name' :: [WFilePath] -> [NameParams -> NameParams] -> Expression
name' files modifiers = EName files (applyModifiers defaultNameParams modifiers)

not :: Expression -> Expression
not = ENot

size :: Comparison Int64 -> Expression
size = ESize

suffix :: ByteString -> Expression
suffix = ESuffix

type_ :: FileType -> Expression
type_ = EType


applyModifiers :: a -> [a -> a] -> a
applyModifiers def modifiers = foldl' (\x f -> f x) def modifiers

caseSensitive :: HasCaseSensitivityOption a => a -> a
caseSensitive = setCaseSensitivity CaseSensitive

caseInsensitive :: HasCaseSensitivityOption a => a -> a
caseInsensitive = setCaseSensitivity CaseInsensitive

basename :: HasPathScopeOption a => a -> a
basename = setPathScope BaseName

wholename :: HasPathScopeOption a => a -> a
wholename = setPathScope BaseName

depth :: Comparison Int -> DirNameParams -> DirNameParams
depth c x = x { _DirNameParams_Depth = c }

includeDotFiles :: MatchParams -> MatchParams
includeDotFiles x = x { _MatchParams_IncludeDotFiles = True }

noEscape :: MatchParams -> MatchParams
noEscape x = x { _MatchParams_NoEscape = True }

data Comparison a
    = Eq !a -- ^ Equal
    | Ne !a -- ^ Not Equal
    | Gt !a -- ^ Greater Than
    | Ge !a -- ^ Greater Than or Equal
    | Lt !a -- ^ Less Than
    | Le !a -- ^ Less Than or Equal
    deriving (Show, Eq, Ord)

renderPathScope :: PathScope -> BSERValue
renderPathScope BaseName = BSERString "basename"
renderPathScope WholeName = BSERString "wholename"

renderOperator :: Comparison a -> BSERValue
renderOperator (Eq _) = BSERString "eq"
renderOperator (Ne _) = BSERString "ne"
renderOperator (Gt _) = BSERString "gt"
renderOperator (Ge _) = BSERString "ge"
renderOperator (Lt _) = BSERString "lt"
renderOperator (Le _) = BSERString "le"

comparisonValue :: Integral n => Comparison n -> BSERValue
comparisonValue (Eq v) = compactBSERInt v
comparisonValue (Ne v) = compactBSERInt v
comparisonValue (Gt v) = compactBSERInt v
comparisonValue (Ge v) = compactBSERInt v
comparisonValue (Lt v) = compactBSERInt v
comparisonValue (Le v) = compactBSERInt v

renderExpression :: Expression -> BSERValue
renderExpression (EAllOf exprs) =
    BSERArray (BSERString "allof" Seq.<| Seq.fromList (map renderExpression exprs))
renderExpression (EAnyOf exprs) =
    BSERArray (BSERString "anyof" Seq.<| Seq.fromList (map renderExpression exprs))
renderExpression (EDirName (WFilePath p) (DirNameParams d caseSensitivity)) =
    BSERArray (Seq.fromList [BSERString exprName, BSERString p, BSERArray (Seq.fromList [BSERString "depth", renderOperator d, comparisonValue d])])
    where
    exprName = case caseSensitivity of { CaseSensitive -> "dirname"; CaseInsensitive -> "idirname" }
renderExpression ETrue = BSERString "true"
renderExpression EFalse = BSERString "false"
renderExpression EEmpty = BSERString "empty"
renderExpression EExists = BSERString "exists"
renderExpression (EMatch pattern (MatchParams caseSensitivity pathScope includeDotFiles_ noEscape_)) =
    BSERArray (Seq.fromList [BSERString exprName, BSERString pattern, renderPathScope pathScope] Seq.>< flags)
    where
    exprName = case caseSensitivity of { CaseSensitive -> "match"; CaseInsensitive -> "imatch" }
    flagsMap = M.unions
        [ if includeDotFiles_ then M.singleton "includedotfiles" (BSERBool True) else M.empty
        , if noEscape_ then M.singleton "noescape" (BSERBool True) else M.empty
        ]
    flags = if M.null flagsMap then Seq.empty else Seq.singleton (BSERObject flagsMap)
renderExpression (EName files (NameParams caseSensitivity pathScope)) =
    BSERArray (Seq.fromList [BSERString exprName, BSERArray (Seq.fromList (map (BSERString . toByteString) files)), renderPathScope pathScope])
    where
    exprName = case caseSensitivity of { CaseSensitive -> "name"; CaseInsensitive -> "iname" }
renderExpression (ESince _) = error "TODO 928352935423"
renderExpression (ENot expr) =
    BSERArray (Seq.fromList [BSERString "not", renderExpression expr])
renderExpression (ESize s) =
    BSERArray (Seq.fromList [BSERString "size", renderOperator s, comparisonValue s])
renderExpression (ESuffix s) =
    BSERArray (Seq.fromList [BSERString "suffix", BSERString s])
renderExpression (EType t) =
    BSERArray (Seq.fromList [BSERString "type", BSERString (fileTypeChar t)])
