{
module Prepo.RewriteRulesParser (rewriteRulesLexer, rewriteRulesParser, rewriteRulesFromString, rewriteRulesFromFile)

where

import Control.Monad
import Data.Maybe
import Data.List

import Prepo.AST
import Prepo.ASTRewrite
import Prepo.HyLoAST(HyLoNodeType(..))

import Prepo.RewriteRulesLexer
}

%name rewriteRulesParser
%tokentype { (RewriteRulesToken, FilePos) }

%token 
             at              { (TokenNode NodeAt , _) } 
             down            { (TokenNode NodeDown , _) }
             prop            { (TokenNodeVal NodeProp $$, _) }
             propVar         { (TokenLeafVar NodeProp $$, _) }
             nom             { (TokenNodeVal NodeNom $$, _) }
             nomVar          { (TokenLeafVar NodeNom $$, _) }
             svar            { (TokenNodeVal NodeVar $$ , _) }
             svarVar         { (TokenLeafVar NodeVar $$, _) }
             rel             { (TokenNodeVal NodeRel $$, _) }
             relVar          { (TokenLeafVar NodeRel $$, _) }
             termVar         { (TokenVar $$, _) }
             dia1            { (TokenNodeVal NodeDia $$, _) }
             dia2            { (TokenNode NodeDia, _) }
             box1            { (TokenNodeVal NodeBox $$, _) }
             box2            { (TokenNode NodeBox, _) }
             true            { (TokenNode NodeTrue, _) }
             false           { (TokenNode NodeFalse, _) }
             neg             { (TokenNode NodeNeg, _) }
             and             { (TokenNode NodeAnd, _) }
             or              { (TokenNode NodeOr, _) }
             dimp            { (TokenNode NodeDimp, _) }
             imp             { (TokenNode NodeImp, _) }
             ubox            { (TokenNode NodeUBox, _) }
             udia            { (TokenNode NodeUDia, _) }
             subIndex        { (TokenSubIndex $$, _) }
             '('             { (TokenOB, _) }
             ')'             { (TokenCB, _) }
             '['             { (TokenOSB, _) }
             ']'             { (TokenCSB, _) }
             '{'             { (TokenOCB, _) }
             '}'             { (TokenCCB, _) }
             ';'             { (TokenSC, _) }
             ','             { (TokenC, _) }
             dot             { (TokenDot, _) }
             '*'             { (TokenStar, _) }
             '+'             { (TokenPlus, _) }
             '~'             { (TokenTilde, _) }
             rewrite         { (TokenRewrite, _) }
             include         { (TokenInclude, _) }
             fileName        { (TokenFileName $$, _) }

%%

RulesDefinition :: { ([FilePath], [ASTRewriteRule HyLoNodeType]) }
RulesDefinition : 
    RewriteRules                                           { ([], $1) }
  | IncludeDirectives RewriteRules                         { ($1,$2) }

IncludeDirectives :: { [FilePath] }
IncludeDirectives : 
    IncludeDirective                                       { [$1] }
  | IncludeDirective IncludeDirectives                     { $1:$2 }

IncludeDirective :: { FilePath }
IncludeDirective : 
    include fileName                                       { $2 }

RewriteRules :: { [ASTRewriteRule HyLoNodeType] }

RewriteRules:  
    RewriteRule                                            { [$1] }
  | RewriteRule RewriteRules                               { $1:$2 }


RewriteRule :: { ASTRewriteRule HyLoNodeType }
RewriteRule :
    LeftPattern rewrite RightPattern ';'                   { makeRewriteRule $1 $3  }


LeftPattern :: { LeftPatternParseInfo }
LeftPattern :
    at '(' LeftAtSubs ',' LeftPattern ')'                  { binaryMatcher (binaryNodeMatcher NodeAt) $3 $5 }
  | down '(' LeftDownVar ',' LeftPattern ')'               { binaryMatcher (binaryNodeMatcher NodeDown) $3 $5 }
  | LeftProp                                               { $1 }
  | LeftNom                                                { $1 }
  | LeftSVar                                               { $1 }
  | LeftTermVar                                            { $1 }
  | dia1 '(' LeftPattern ')'                               { unaryMatcher (binaryNodeMatcher NodeDia (leafMatcher $ LV NodeRel $1)) $3 }
  | dia2 '(' LeftMod ',' LeftPattern ')'                   { binaryMatcher (binaryNodeMatcher NodeDia)  $3 $5 }
  | box1 '(' LeftPattern ')'                               { unaryMatcher (binaryNodeMatcher NodeBox (leafMatcher $ LV NodeRel $1)) $3 }
  | box2 '(' LeftMod ',' LeftPattern ')'                   { binaryMatcher (binaryNodeMatcher NodeBox) $3 $5 }
  | true                                                   { atomicMatcher (leafMatcher (L NodeTrue)) }
  | false                                                  { atomicMatcher (leafMatcher (L NodeFalse)) }
  | neg '(' LeftPattern ')'                                { unaryMatcher (unaryNodeMatcher NodeNeg) $3 }
  | and '(' LeftPattern ',' LeftPattern ')'                { binaryMatcher (binaryNodeMatcher NodeAnd) $3 $5 }
  | or  '(' LeftPattern ',' LeftPattern ')'                { binaryMatcher (binaryNodeMatcher NodeOr) $3 $5 }
  | dimp '(' LeftPattern ',' LeftPattern ')'               { binaryMatcher (binaryNodeMatcher NodeDimp) $3 $5 }
  | imp '(' LeftPattern ',' LeftPattern ')'                { binaryMatcher (binaryNodeMatcher NodeImp) $3 $5 }
  | ubox '(' LeftPattern ')'                               { unaryMatcher (unaryNodeMatcher NodeUBox) $3 }
  | udia '(' LeftPattern ')'                               { unaryMatcher (unaryNodeMatcher NodeUDia) $3 }
  | ContextRegExp'[' ThreeDots LeftPattern ThreeDots ']'   { contextMatcher (simpleRegExpMatcher $1) $1 $4 }
    
ThreeDots :: { () }
ThreeDots :
    dot dot dot                                            { () }    


LeftAtSubs :: { LeftPatternParseInfo }
LeftAtSubs : 
    LeftNom                                                { $1 }
  | LeftSVar                                               { $1 }
  | LeftTermVar                                            { $1 }

LeftDownVar :: { LeftPatternParseInfo }
LeftDownVar : 
    LeftSVar                                               { $1 }
  | LeftTermVar                                            { $1 }
  | LeftNom                                                { $1 }

LeftProp :: { LeftPatternParseInfo }
LeftProp : 
    prop                                                   { atomicMatcher (leafMatcher (LV NodeProp $1)) }
  | propVar                                                { matcherWithTermVar (leafVarMatcher NodeProp $1) $1 }

LeftNom :: { LeftPatternParseInfo }
LeftNom : 
    nom                                                    { atomicMatcher (leafMatcher (LV NodeNom $1)) }
  | nomVar                                                 { matcherWithTermVar (leafVarMatcher NodeNom $1) $1 }

LeftSVar :: { LeftPatternParseInfo }
LeftSVar : 
    svar                                                   { atomicMatcher (leafMatcher (LV NodeVar $1)) } 
  | svarVar                                                { matcherWithTermVar (leafVarMatcher NodeVar $1) $1 }

LeftTermVar :: { LeftPatternParseInfo }
LeftTermVar :
    termVar                                                { matcherWithTermVar (varMatcher $1) $1 }

LeftMod :: { LeftPatternParseInfo }
LeftMod : 
    rel                                                    { atomicMatcher (leafMatcher (LV NodeRel $1)) }
  | relVar                                                 { matcherWithTermVar (leafVarMatcher NodeRel $1) $1 }
  | LeftTermVar                                            { $1 }

   
RightPattern :: { (TermWriter HyLoNodeType, [Context HyLoNodeType]) }
RightPattern : 
    at '(' RightAtSubs ',' RightPattern ')'                { (writeBinaryNode NodeAt $3 (fst $5), snd $5) }
  | down '(' RightDownVar ',' RightPattern ')'             { (writeBinaryNode NodeDown $3 (fst $5), snd $5) }
  | RightProp                                              { ($1, []) }
  | RightNom                                               { ($1, []) }
  | RightSVar                                              { ($1, []) }
  | RightTermVar                                           { ($1, []) }
  | dia1 '(' RightPattern ')'                              { (writeBinaryNode NodeDia (writeLeaf NodeRel $ Just $1) (fst $3), snd $3) }
  | dia2 '(' RightMod ',' RightPattern ')'                 { (writeBinaryNode NodeDia $3 (fst $5), snd $5) }
  | box1 '(' RightPattern ')'                              { (writeBinaryNode NodeBox (writeLeaf NodeRel $ Just $1) (fst $3), snd $3) }
  | box2 '(' RightMod ',' RightPattern ')'                 { (writeBinaryNode NodeBox $3 (fst $5), snd $5) }
  | true                                                   { (writeLeaf NodeTrue Nothing, []) }
  | false                                                  { (writeLeaf NodeFalse Nothing, []) }
  | neg '(' RightPattern ')'                               { (writeUnaryNode NodeNeg (fst $3), snd $3) }
  | and '(' RightPattern ',' RightPattern ')'              { (writeBinaryNode NodeAnd (fst $3) (fst $5), (snd $3) ++ (snd $5)) }
  | or  '(' RightPattern ',' RightPattern ')'              { (writeBinaryNode NodeOr (fst $3) (fst $5), (snd $3) ++ (snd $5)) }
  | dimp '(' RightPattern ',' RightPattern ')'             { (writeBinaryNode NodeDimp (fst $3) (fst $5), (snd $3) ++ (snd $5)) }
  | imp '(' RightPattern ',' RightPattern ')'              { (writeBinaryNode NodeImp (fst $3) (fst $5), (snd $3) ++ (snd $5)) }
  | ubox '(' RightPattern ')'                              { (writeUnaryNode NodeUBox (fst $3), snd $3) }
  | udia '(' RightPattern ')'                              { (writeUnaryNode NodeUDia (fst $3), snd $3) }
  | ContextRegExp '[' ThreeDots RightPattern ThreeDots ']' { (writeInContext $1 (fst $4), snd $4) }
  
RightAtSubs :: { TermWriter HyLoNodeType }
RightAtSubs : 
    RightNom                                               { $1 }
  | RightSVar                                              { $1 }
  | RightTermVar                                           { $1 }

RightDownVar :: { TermWriter HyLoNodeType }
RightDownVar : 
    RightSVar                                              { $1 }
  | RightTermVar                                           { $1 }

RightProp :: { TermWriter HyLoNodeType }
RightProp :
    prop                                                   { writeLeaf NodeProp $ Just $1 }
  | propVar                                                { writeLeafVar NodeProp $1 }

RightNom :: { TermWriter HyLoNodeType }
RightNom : 
    nom                                                    { writeLeaf NodeNom $ Just $1 }
  | nomVar                                                 { writeLeafVar NodeNom $1 }

RightSVar :: { TermWriter HyLoNodeType }
RightSVar :
    svar                                                   { writeLeaf NodeVar $ Just $1 } 
  | svarVar                                                { writeLeafVar NodeVar $1 }

RightTermVar :: { TermWriter HyLoNodeType }
RightTermVar : 
    termVar                                                { writeVar $1 }

RightMod :: { TermWriter HyLoNodeType }
RightMod :
    rel                                                    { writeLeaf NodeRel $ Just $1 }
  | relVar                                                 { writeLeafVar NodeRel $1 }
  | RightTermVar                                           { $1 }

ContextRegExp :: { Context HyLoNodeType }
ContextRegExp :
    RegExps                                                { ($1, -1) }
  | RegExps subIndex                                       { ($1, $2) }
    
RegExps :: { ContextRegExp HyLoNodeType }
RegExps : 
    RegExp                                                 { $1 }
  | RegExp RegExps                                         { $1 ++ $2 }

RegExp :: { ContextRegExp HyLoNodeType }
RegExp : 
    NodeRegExp                                             { [Exactly $1] }
  | NodeRegExp '+'                                         { [Exactly $1, Star $1] }
  | NodeRegExp '*'                                         { [Star $1] }

NodeRegExp :: { AtomicExp HyLoNodeType }
NodeRegExp :
    dot                                                    { Anything }
  | RegExpConstant                                         { OneOf [$1] }  
  | '{' RegExpConstantList '}'                             { OneOf $2 }
  | '~' '{' RegExpConstantList '}'                         { NoneOf $3 }

RegExpConstant :: { HyLoNodeType }
RegExpConstant : 
    dia2                                                   { NodeDia }
  | box2                                                   { NodeBox }
  | udia                                                   { NodeUDia }
  | ubox                                                   { NodeUBox }           
  | down                                                   { NodeDown }
  | and                                                    { NodeAnd }
  | or                                                     { NodeOr }
  | dimp                                                   { NodeDimp }
  | imp                                                    { NodeImp }
  | neg                                                    { NodeNeg }

RegExpConstantList :: { [HyLoNodeType] }
RegExpConstantList :
    RegExpConstant                                         { [$1] }
  | RegExpConstant RegExpConstantList                      { $1:$2 }

{
happyError :: [(RewriteRulesToken, FilePos)] -> a
happyError ((_, fp):_) = error ("Parse error near line " ++ 
                                   (show $ line fp) ++ 
                                   ", col. " ++
                                   (show $ col fp))

data LeftPatternParseInfo = LPI{matcher :: forall m. MonadPlus m => TermMatcherM m HyLoNodeType,
                                contexts :: [Context HyLoNodeType],
                                termVars :: [TermVar]}

unaryMatcher :: (forall m. MonadPlus m => TermMatcherM m HyLoNodeType -> TermMatcherM m HyLoNodeType) -> LeftPatternParseInfo -> LeftPatternParseInfo
unaryMatcher um p  = LPI{matcher = um (matcher p),
                         contexts = contexts p,
                         termVars = termVars p}

atomicMatcher :: (forall m. MonadPlus m => TermMatcherM m HyLoNodeType) -> LeftPatternParseInfo
atomicMatcher m = LPI{matcher = m, contexts = [], termVars = []}

matcherWithTermVar :: (forall m. MonadPlus m => TermMatcherM m HyLoNodeType) -> TermVar -> LeftPatternParseInfo
matcherWithTermVar m v = LPI{matcher = m, contexts = [], termVars = [v]}

binaryMatcher :: (forall m. MonadPlus m => TermMatcherM m HyLoNodeType -> TermMatcherM m HyLoNodeType -> TermMatcherM m HyLoNodeType) -> 
                               LeftPatternParseInfo -> LeftPatternParseInfo -> LeftPatternParseInfo
binaryMatcher bm p1 p2 = LPI{matcher = bm (matcher p1) (matcher p2),
                             contexts = (contexts p1) ++ (contexts p2),
                             termVars = (termVars p1) ++ (termVars p2)}

contextMatcher :: (forall m. MonadPlus m => TermMatcherM m HyLoNodeType -> TermMatcherM m HyLoNodeType) -> 
                                 Context HyLoNodeType -> LeftPatternParseInfo -> LeftPatternParseInfo
contextMatcher um c p = LPI{matcher = um (matcher p),
                            contexts = c:(contexts p),
                            termVars = termVars p}


makeRewriteRule ::  LeftPatternParseInfo -> (TermWriter HyLoNodeType, [Context HyLoNodeType]) -> ASTRewriteRule HyLoNodeType
makeRewriteRule lpi (writer, rightPatterns)
    |containsRepetitions leftPatterns                  = error "Repeated context patterns on left hand of rule are not allowed"
    | not . null $ (nub rightPatterns) \\ leftPatterns = error "Context patterns occurring in right hand side of a rule must occur in left hand too"
    | termVarMatchedTwice                              = rewrite (firstMatch $ matcher lpi) writer  -- a term variable is matched twice
    | otherwise                                        = rewrite (matcher lpi) writer
        where leftPatterns        = contexts lpi
              leftTermVars        = termVars lpi
              termVarMatchedTwice = length leftTermVars /= (length . nub $ leftTermVars)


containsRepetitions :: Eq a => [a] -> Bool
containsRepetitions l = (length . nub $ l) /= length l


showPattern :: Show a => ContextRegExp a -> String
showPattern  []              = []
showPattern ((Exactly n):ps) = (show n) ++ showPattern ps
showPattern ((Star n):ps)    = (show n) ++ "*" ++ showPattern ps

rewriteRulesFromString :: String -> [ASTRewriteRule HyLoNodeType]
rewriteRulesFromString = discardFileInclusions . rewriteRulesParser . rewriteRulesLexer

discardFileInclusions :: ([FilePath], [ASTRewriteRule HyLoNodeType]) -> [ASTRewriteRule HyLoNodeType]
discardFileInclusions ([], rules) = rules
discardFileInclusions _           = error "Can't proccess .include directive when building rules from a string"

rewriteRulesFromFile :: FilePath -> IO [ASTRewriteRule HyLoNodeType]
rewriteRulesFromFile f = loadRules [f] []

loadRules :: [FilePath] -> [(FilePath, [ASTRewriteRule HyLoNodeType])] -> IO [ASTRewriteRule HyLoNodeType]
loadRules [] l     = return $ concatMap snd l
loadRules (f:fs) l =
    do
        if (isNothing $ lookup f l)
            then do
                    let lexAndParse = rewriteRulesParser . rewriteRulesLexer
                    fileContent <- readFile f
                    let (includes, rules) = lexAndParse fileContent
                    loadRules (includes ++ fs) $ (f,rules):l
            else loadRules fs l
}
