module FuncType (funcType, abstSig, genPropSig) where


import System.IO.Unsafe
import Data.List
import qualified Data.Text as DT
import qualified Control.Monad.State as CMS
import Language.Haskell.Exts
import Language.Haskell.Interpreter

import ParseMode


funcType :: String -> String -> String -> (String, Type SrcSpanInfo)
funcType path mdl func = case typ' of
                      Right typ -> typ
                      Left  ie  -> 
                        error ("\ninferType ::\n" ++
                          case ie of 
                            UnknownError err  -> err
                            WontCompile  errs -> foldl1 (\x y -> x ++ '\n': y) $
                                                   map errMsg errs
                            NotAllowed   err  -> err
                            GhcException err  -> err
                        )
  where typ' = unsafePerformIO $ runInterpreter $ inferType path mdl func

-- cited |http://d.hatena.ne.jp/keigoi/20100125/1264411453|
inferType :: String -> String -> String -> InterpreterT IO (String, Type SrcSpanInfo)
inferType path mdl func = do
  loadModules $ [path]
  setTopLevelModules [mdl]
  sig <- typeOf func
  let tree = fromParseResult (parseTypeWithMode ParseMode.mode sig)
  return (sig, tree)



nil :: SrcSpanInfo
nil = SrcSpanInfo {
         srcInfoSpan = SrcSpan {
           srcSpanFilename = "" ,
           srcSpanStartLine = 0 , 
           srcSpanStartColumn = 0 ,  
           srcSpanEndLine = 0 ,
           srcSpanEndColumn = 0
         } ,
         srcInfoPoints = []
       }


abstSig :: Type SrcSpanInfo -> (String, String, Int)
abstSig typ' = (genSig abTyp1, genSig abTyp2, snd abTyp1)
  where abTyp1 = abstType1 typ'
        abTyp2 = abstType2 typ'
        genSig typ = prettyPrint $ fst typ

abstType1 :: Type SrcSpanInfo -> (Type SrcSpanInfo, Int)
abstType1 typ' =
  case typ' of
    TyForall _ _ _ typ -> abstType1 typ
    TyFun _ typ1 typ2  -> let (abTyp1, arg1) = abstType1 typ1
                              (abTyp2, arg2) = abstType1 typ2
                          in ((TyFun nil abTyp1 abTyp2), arg1+arg2) 
    TyTuple _ a typs   -> varA
    TyList _  typ      -> varA
    TyParArray _ typ   -> varA
    TyApp _ a b        -> varA
    TyVar _ _          -> varA 
    TyCon _ qName      -> case qName of
                            UnQual _ (Ident _ "Bool") -> (typ', 1)
                            _                         -> varA
    TyParen _ typ      -> (TyParen nil (fst (abstType1 typ)), 1)
    TyInfix _ _ _ _    -> varA
    TyKind _ typ _     -> abstType1 typ
    TyPromoted _ _     -> varA
    TyEquals _ _ _     -> varA
    TySplice _ _       -> varA
    TyBang _ _ _ typ   -> abstType1 typ
    TyWildCard _ _     -> varA
    TyQuasiQuote _ _ _ -> varA
  where varA = (TyVar nil (Ident nil "a"), 1)

abstType2 :: Type SrcSpanInfo -> (Type SrcSpanInfo, Int)
abstType2 typ' = foldl (\(t, n) x -> rplcTyp2 x t) (typ', 0) $ vars ++ cons 
  where (vars, cons) = rplcList typ' 

rplcList :: Type SrcSpanInfo -> ([(String, String)], [(String, String)])
rplcList typ' = partition isVar $ nubByPre $ CMS.evalState (rplcList' typ') 'a'
  where isVar (c:cs, _) = 'a' <= c && c <= 'z'
        nubByPre = nubBy (\(f1, _) (f2, _) -> f1 == f2)

rplcList' :: Type SrcSpanInfo -> CMS.State Char [(String, String)]
rplcList' typ' =
    case typ' of
      TyForall _ _ _ typ -> rplcList' typ
      TyFun _ typ1 typ2  -> rplcList'' typ1 typ2
      TyTuple _ _ typs   -> fmap concat $ mapM rplcList' typs
      TyList _  typ      -> rplcList' typ
      TyParArray _ typ   -> rplcList' typ
      TyApp _ typ1 typ2  -> rplcList'' typ1 typ2
      TyVar _ name' ->
          case name' of
            Ident _ nm ->
                CMS.state (\varNm -> ([(nm, varNm:"")], succ varNm))
            _          -> return []
      TyCon _ qName ->
          case qName of
            UnQual _ (Ident _ nm) ->
                if nm == "Bool" then return []
                else CMS.state (\varNm -> ([(nm, varNm:"")], succ varNm))
            _                     -> return []
      TyParen _ typ      -> rplcList' typ
      TyKind _ typ _     -> rplcList' typ
      TyBang _ _ _ typ   -> rplcList' typ
      _                  -> return []
  where rplcList'' typ1 typ2 = do
          list1 <- rplcList' typ1
          list2 <- rplcList' typ2
          return $ list1 ++ list2

rplcTyp2 :: (String, String) -> Type SrcSpanInfo -> (Type SrcSpanInfo, Int)
rplcTyp2 (preNm, varNm) typ' =
  case typ' of
    TyForall _ _ _ typ -> rplcTyp2' typ
    TyFun _ typ1 typ2  ->
        let (abTyp1, termN1) = rplcTyp2' typ1
            (abTyp2, termN2) = rplcTyp2' typ2
        in  ((TyFun nil abTyp1 abTyp2), termN1 + termN2)
    TyTuple _ a typs   ->
        (TyTuple nil a (map (fst.rplcTyp2') typs), 1)
    TyList _  typ      ->
        (TyList nil (fst (rplcTyp2' typ)), 1)
    TyParArray _ typ   ->
        (TyParArray nil (fst (rplcTyp2' typ)), 1)
    TyApp _ typ1 typ2  ->
        (TyApp nil (fst (rplcTyp2' typ1)) (fst (rplcTyp2' typ2)), 1)
    TyVar _ name'      ->
        case name' of
          Ident _ nm   -> if nm == preNm then varX
                          else crntVar
          _            -> crntVar
    TyCon _ qName      ->
        case qName of
          UnQual _ (Ident _ nm) -> if nm == preNm then varX
                                   else crntVar
          _                     -> crntVar
    TyParen _ typ      -> (TyParen nil (fst (rplcTyp2' typ)), 1)
    TyInfix _ _ _ _    -> crntVar
    TyKind _ typ _     -> rplcTyp2' typ
    TyPromoted _ _     -> crntVar
    TyEquals _ _ _     -> crntVar
    TySplice _ _       -> crntVar
    TyBang _ _ _ typ   -> rplcTyp2' typ
    TyWildCard _ _     -> crntVar
    TyQuasiQuote _ _ _ -> crntVar
  where rplcTyp2' = rplcTyp2 (preNm, varNm)
        crntVar = (typ', 1)
        varX = (TyVar nil (Ident nil varNm), 1)


genPropSig :: Type SrcSpanInfo -> String
genPropSig typ' = prettyPrint $ toMonoType $ lastBool typ'


toMonoType :: Type SrcSpanInfo -> Type SrcSpanInfo
toMonoType typ' = rmTopPoly $ exe typ' asstList
  where asstList = classAssts typ'
        exe typ [] = typ 
        exe typ (x:xs) = exe (rename x typ) xs

classAssts :: Type SrcSpanInfo -> [Asst SrcSpanInfo]
classAssts (TyForall _ _ (Just context) typ') =
  case context of
    CxSingle _ asst  -> [asst]
    CxTuple  _ assts -> assts
    CxEmpty  _       -> []
classAssts _ = []


rename :: Asst SrcSpanInfo -> Type SrcSpanInfo -> Type SrcSpanInfo
rename (ClassA _ (UnQual _ (Ident _ nm)) typs) typ' = exe typ'
  where TyVar _ (Ident _ nm'') = head typs
        exe typ' =
          case typ' of
            TyForall _ a b typ    -> TyForall nil a b (exe typ)
            TyFun _ typ1 typ2     -> TyFun nil (exe typ1) (exe typ2) 
            TyTuple _ a typs      -> TyTuple nil a (map exe typs)
            TyList _ typ          -> TyList nil (exe typ)
            TyParArray _ typ      -> TyParArray nil (exe typ) 
            TyApp _ typ1 typ2     -> TyApp nil (exe typ1) (exe typ2)
            TyVar _ (Ident _ nm') -> 
              if nm' == nm''
              then TyCon nil (UnQual nil (Ident nil (classToType nm)))
              else typ'
            TyParen _ typ         -> TyParen nil (exe typ)
            TyInfix _ typ1 b typ2 -> TyInfix nil (exe typ1) b (exe typ2) 
            TyKind _ typ a        -> TyKind nil (exe typ) a
            TyPromoted _ promoted ->
              case promoted of
                PromotedList _ a typs -> TyPromoted nil (PromotedList nil a (map exe typs))
                PromotedTuple _ typs -> TyPromoted nil (PromotedTuple nil (map exe typs))
            TyEquals _ typ1 typ2  -> TyEquals nil (exe typ1) (exe typ2)  
            TyBang _ a b typ      -> TyBang nil a b (exe typ)
            _                     -> typ'
rename _  _ = error "Unsupported Class Assertion"

classToType :: String -> String
classToType nm = case nm of
                   "Show"       -> "Int"
                   "Read"       -> "Int"
                   "Eq"         -> "Int"
                   "Ord"        -> "Int"
                   "Bounded"    -> "Int"
                   "Enum"       -> "Int"
                   "Num"        -> "Int"
                   "Real"       -> "Int"
                   "Integral"   -> "Int"
                   "Fractional" -> "Float" 
                   "RealFrac"   -> "Float"
                   "Floating"   -> "Float"
                   "RealFloat"  -> "Float"
                   "Monad"      -> "[]"
                   "MonadPlus"  -> "[]"
                   "Functor"    -> "[]"
                   other        -> error $ other ++ " is Unsupported Type Class"

rmTopPoly :: Type SrcSpanInfo -> Type SrcSpanInfo
rmTopPoly typ' =
  case typ' of
            TyForall _ a b typ    -> rmTopPoly typ
            TyFun _ typ1 typ2     -> TyFun nil (rmTopPoly typ1) (rmTopPoly typ2) 
            TyTuple _ a typs      -> TyTuple nil a (map rmTopPoly typs)
            TyList _ typ          -> TyList nil (rmTopPoly typ)
            TyParArray _ typ      -> TyParArray nil (rmTopPoly typ) 
            TyApp _ typ1 typ2     -> TyApp nil (rmTopPoly typ1) (rmTopPoly typ2) 
            TyVar _ _             -> TyCon nil (UnQual nil (Ident nil "Int"))
            TyParen _ typ         -> TyParen nil (rmTopPoly typ)
            TyInfix _ typ1 b typ2 -> TyInfix nil (rmTopPoly typ1) b (rmTopPoly typ2) 
            TyKind _ typ a        -> TyKind nil (rmTopPoly typ) a
            TyPromoted _ promoted ->
              case promoted of
                PromotedList _ a typs -> TyPromoted nil (PromotedList nil a (map rmTopPoly typs))
                PromotedTuple _ typs -> TyPromoted nil (PromotedTuple nil (map rmTopPoly typs))
            TyEquals _ typ1 typ2  -> TyEquals nil (rmTopPoly typ1) (rmTopPoly typ2)  
            TyBang _ a b typ      -> TyBang nil a b (rmTopPoly typ)
            _                     -> typ'


lastBool :: Type SrcSpanInfo -> Type SrcSpanInfo
lastBool (TyForall _ a b typ) = TyForall nil a b (lastBool typ)
lastBool (TyFun _ typ1 typ2) = TyFun nil typ1 (lastBool typ2)
lastBool _ = TyCon nil (UnQual nil (Ident nil "Bool"))

