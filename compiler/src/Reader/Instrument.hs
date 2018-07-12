{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Reader.Instrument
  ( instrument
  )
  where


import Text.Show.Pretty (ppShow)
import Debug.Trace (trace)

import Control.Monad (replicateM)
import Data.Monoid ((<>))
import qualified Control.Monad.State as State
import qualified Data.Bag as Bag
import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified AST.Canonical as Can
import qualified AST.Module.Name as ModuleName
import qualified Elm.Name as N
import qualified Reader.Hooks as Hooks
import qualified Reader.SourceMap as SrcMap
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R



-- INSTRUMENT


instrument :: BS.ByteString -> Can.Module -> (Can.Module, SrcMap.Module)
instrument source module_ =
  let instrumented = instrumentModule source module_
      message =
        "elm-reader debug -- received canonical AST:\n"
        ++ ppShow module_ ++ "\n"
        ++ "transformed to:\n"
        ++ ppShow instrumented
  in
  trace message instrumented


instrumentModule :: BS.ByteString -> Can.Module -> (Can.Module, SrcMap.Module)
instrumentModule source module_ =
  let
    (newDecls, srcMaps) =
      instrumentDecls (Can._name module_) $ Can._decls module_

    modSrcMap =
      SrcMap.Module
        { SrcMap._frames = Map.fromList $ Bag.toList srcMaps
        , SrcMap._source = source
        }
  in
    (module_ { Can._decls = newDecls }, modSrcMap)


instrumentDecls :: ModuleName.Canonical -> Can.Decls -> (Can.Decls, Bag.Bag (SrcMap.FrameId, SrcMap.Frame))
instrumentDecls home decls =
  case decls of
    Can.Declare def rest ->
      let
        (newDef, defSrcMaps) =
          instrumentTopLevelDef home def

        (newRest, restSrcMaps) =
          instrumentDecls home rest
      in
        (Can.Declare newDef newRest, Bag.append defSrcMaps restSrcMaps)

    Can.DeclareRec defs rest ->
      let
        (newDefs, defSrcMaps) =
          unzip $ map (instrumentTopLevelDef home) defs

        (newRest, restSrcMaps) =
          instrumentDecls home rest
      in
        (Can.DeclareRec newDefs newRest, Bag.append restSrcMaps $ Bag.concat defSrcMaps)

    Can.SaveTheEnvironment ->
      (Can.SaveTheEnvironment, Bag.empty)


instrumentTopLevelDef :: ModuleName.Canonical -> Can.Def -> (Can.Def, Bag.Bag (SrcMap.FrameId, SrcMap.Frame))
instrumentTopLevelDef home def =
  let
    (name, body, typeInfo, isLambda) =
      normalizeDef def

    forbiddenVars =
      varsUsed body

    region =
      R.merge (A.toRegion name) (A.toRegion body)

    rootCtx =
      Context
        { _moduleName = home
        , _defName = A.toValue name
        , _varIds = Map.empty
        }
  in
  runWithIdState forbiddenVars $ do
    (newBody, bodySrcMaps) <- instrumentExpr rootCtx body

    (framedBody, frames) <-
      case isLambda of
        NoLambda ->
          do
            frameId <- freshFrameId rootCtx
            return $
              ( recordFrame frameId newBody
              , collectOuterFrame frameId region bodySrcMaps
              )

        YesLambda ->
          return (newBody, _frames bodySrcMaps)

    let newDef = rebuildDef name framedBody typeInfo

    return (newDef, frames)


instrumentExpr :: Context -> Can.Expr -> WithIdState (Can.Expr, Answers)
instrumentExpr ctx expr =
  do
    exprId <- freshExprId
    instrumentExprWithId ctx exprId expr


instrumentExprWithId :: Context -> SrcMap.ExprId -> Can.Expr -> WithIdState (Can.Expr, Answers)
instrumentExprWithId ctx exprId locExpr@(A.At region expr) =
  case expr of
    Can.VarLocal name ->
      case Map.lookup name $ _varIds ctx of
        Just priorId ->
          return (locExpr, makeExprRegion priorId region)

        Nothing ->
          trace "Warning: Encountered an unknown local variable during instrumentation" $
          return (locExpr, noAnswers)

    Can.VarTopLevel home name ->
      return (locExpr, makeQualified exprId region home name)

    Can.VarKernel _ _ ->
      -- TODO: How should we handle kernel variables?
      return (locExpr, noAnswers)

    Can.VarForeign home name _ ->
      return (locExpr, makeQualified exprId region home name)

    Can.VarCtor _ home name _ _ ->
      return (locExpr, makeQualified exprId region home name)

    Can.VarDebug home name _ ->
      return (locExpr, makeQualified exprId region home name)

    Can.VarOperator _ home name _ ->
      return (locExpr, makeQualified exprId region home name)

    Can.Chr _ ->
      return (locExpr, noAnswers)

    Can.Str _ ->
      return (locExpr, noAnswers)

    Can.Int _ ->
      return (locExpr, noAnswers)

    Can.Float _ ->
      return (locExpr, noAnswers)

    Can.List items ->
      do
        (newItems, itemSrcMaps) <- unzip <$> traverse (instrumentExpr ctx) items
        return (A.At region $ Can.List newItems, combineMany itemSrcMaps)

    Can.Negate inner ->
      do
        (newInner, innerSrcMap) <- instrumentExpr ctx inner

        let
          newNegate =
            recordExpr exprId $
            A.At region $ Can.Negate newInner

          srcMap =
            combine innerSrcMap $ makeExprRegion exprId region

        return (newNegate, srcMap)

    Can.Binop shortName home name typeAnnot left right ->
      do
        (newLeft, leftSrcMap) <- instrumentExpr ctx left
        (newRight, rightSrcMap) <- instrumentExpr ctx right

        let
          binopRegionSrcMap =
            makeExprRegion exprId region

          binopNameSrcMap =
            makeExprName exprId home name

          newBinop =
            recordExpr exprId $
            A.At region $ Can.Binop shortName home name typeAnnot newLeft newRight

          srcMap =
            combine leftSrcMap $ combine rightSrcMap $ combine binopRegionSrcMap binopNameSrcMap

        return (newBinop, srcMap)

    Can.Lambda args body ->
      do
        (argVars, argSrcMaps) <- unzip <$> traverse instrumentPattern args

        let
          allArgVars =
            Bag.toList $ Bag.concat argVars

          ctxWithVars =
            addVars allArgVars ctx

        (newBody, bodySrcMap) <- instrumentExpr ctxWithVars body

        frameId <- freshFrameId ctx

        let
          frameSrcMaps =
            makeFrames $ collectOuterFrame frameId region $ combineMany $ bodySrcMap : argSrcMaps

          lambdaSrcMap =
            makeExprRegion exprId region

          captures =
            Map.toList $ Map.restrictKeys (_varIds ctx) $ varsUsed body

          newLambda =
            recordExpr exprId $
            markInstrumented $
            A.At region $ Can.Lambda args $
            recordFrame frameId $
            recordVarsIn captures $
            recordVarsIn allArgVars $
            newBody

        return (newLambda, combine frameSrcMaps lambdaSrcMap)

    Can.Call func args ->
      do
        -- If the func expression is sufficiently simple, we don't want to clutter the source map
        -- with a separate cell just for the function.  Instead, we want to merge the func
        -- expression with its enclosing call expression.
        (newFunc, funcSrcMap) <-
          case A.toValue func of
            -- We specifically do *not* merge VarLocal expressions with the enclosing call
            -- expression, because if the function is stored in a local variable then it is only
            -- known dynamically at runtime, so the user may need to inspect it to know which
            -- function is actually being called.

            Can.VarTopLevel home name ->
              return (func, makeExprName exprId home name)

            Can.VarKernel _ _ ->
              -- TODO: How should we handle kernel variables?
              return (func, noAnswers)

            Can.VarForeign home name _ ->
              return (func, makeExprName exprId home name)

            Can.VarCtor _ home name _ _ ->
              return (func, makeExprName exprId home name)

            Can.VarDebug home name _ ->
              return (func, makeExprName exprId home name)

            _ ->
              instrumentExpr ctx func

        (newArgs, argSrcMaps) <- unzip <$> traverse (instrumentExpr ctx) args

        newCall <- recordCall exprId newFunc newArgs

        let callSrcMap = makeExprRegion exprId region

        return (newCall, combineMany $ callSrcMap : funcSrcMap : argSrcMaps)

    Can.If branches finally ->
      do
        let
          instrumentBranch (condition, branch) =
            do
              (newCondition, conditionSrcMap) <- instrumentExpr ctx condition
              (newBranch, branchSrcMap) <- instrumentExpr ctx branch
              return ((newCondition, newBranch), combine conditionSrcMap branchSrcMap)

        (newBranches, branchSrcMaps) <- unzip <$> traverse instrumentBranch branches
        (newFinally, finallySrcMap) <- instrumentExpr ctx finally

        let
          ifSrcMap =
            makeExprRegion exprId region

          newIf =
            recordExpr exprId $
            A.At region $ Can.If newBranches newFinally

        return (newIf, combineMany $ ifSrcMap : finallySrcMap : branchSrcMaps)

    Can.Let def body ->
      do
        varId <- freshExprId
        (newDef, defSrcMap) <- instrumentDef ctx varId def

        let varName = defName def
        let ctxWithVar = addVar varName varId ctx
        (newBody, bodySrcMap) <- instrumentExpr ctxWithVar body

        let
          newLet =
            A.At region $ Can.Let newDef $ recordVarsIn [(varName, varId)] newBody

        return (newLet, combine defSrcMap bodySrcMap)

    Can.LetRec defs body ->
      do
        varIds <- replicateM (length defs) freshExprId

        let
          vars =
            zip (map defName defs) varIds

          ctxWithVars =
            addVars vars ctx

        (newDefs, defSrcMaps) <-
          unzip <$> sequence (zipWith (instrumentDef ctxWithVars) varIds defs)

        (newBody, bodySrcMap) <- instrumentExpr ctxWithVars body

        let
          newLetRec =
            A.At region $ Can.LetRec newDefs $ recordVarsIn vars newBody

          srcMap =
            combineMany $ bodySrcMap : defSrcMaps

        return (newLetRec, srcMap)

    Can.LetDestruct pat val body ->
      do
        (patVars, patSrcMap) <- instrumentPattern pat
        (newVal, valSrcMap) <- instrumentExpr ctx val

        let ctxWithVars = addVars (Bag.toList patVars) ctx
        (newBody, bodySrcMap) <- instrumentExpr ctxWithVars body

        let
          newLetDestruct =
            A.At region $ Can.LetDestruct pat newVal $ recordVarsIn (Bag.toList patVars) newBody

          srcMap =
            combine patSrcMap $ combine valSrcMap bodySrcMap

        return (newLetDestruct, srcMap)

    Can.Case val branches ->
      do
        let
          instrumentBranch (Can.CaseBranch pat body) =
            do
              (patVars, patSrcMap) <- instrumentPattern pat
              let ctxWithVars = addVars (Bag.toList patVars) ctx
              (newBody, bodySrcMap) <- instrumentExpr ctxWithVars body

              let
                newBranch =
                  Can.CaseBranch pat $ recordVarsIn (Bag.toList patVars) newBody

                srcMap =
                  combine patSrcMap bodySrcMap

              return (newBranch, srcMap)

        (newVal, valSrcMap) <- instrumentExpr ctx val

        (newBranches, branchSrcMaps) <- unzip <$> traverse instrumentBranch branches

        let
          newCase =
            recordExpr exprId $
            A.At region $ Can.Case newVal newBranches

          srcMap =
            combineMany $ makeExprRegion exprId region : valSrcMap : branchSrcMaps

        return (newCase, srcMap)

    Can.Accessor _ ->
      return (locExpr, noAnswers)

    Can.Access record field ->
      do
        (newRecord, recordSrcMap) <- instrumentExpr ctx record

        let
          newAccess =
            recordExpr exprId $
            A.At region $ Can.Access newRecord field

          srcMap =
            combine recordSrcMap $ makeExprRegion exprId region

        return (newAccess, srcMap)

    Can.Update name record updates ->
      do
        let
          instrumentUpdate (field, Can.FieldUpdate reg val) =
            do
              (newVal, valSrcMap) <- instrumentExpr ctx val
              return ((field, Can.FieldUpdate reg newVal), valSrcMap)

        (newRecord, recordSrcMap) <- instrumentExpr ctx record

        (newUpdates, updateSrcMaps) <- unzip <$> traverse instrumentUpdate (Map.toList updates)

        let
          newUpdate =
            recordExpr exprId $
            A.At region $ Can.Update name newRecord $ Map.fromList newUpdates

          srcMap =
            combineMany $ makeExprRegion exprId region : recordSrcMap : updateSrcMaps

        return (newUpdate, srcMap)

    Can.Record fields ->
      do
        let
          instrumentField (field, val) =
            do
              (newVal, valSrcMap) <- instrumentExpr ctx val
              return ((field, newVal), valSrcMap)

        (newFields, fieldSrcMaps) <- unzip <$> traverse instrumentField (Map.toList fields)

        let
          newRecord =
            A.At region $ Can.Record $ Map.fromList newFields

          srcMap =
            combineMany fieldSrcMaps

        return (newRecord, srcMap)

    Can.Unit ->
      return (locExpr, noAnswers)

    Can.Tuple a b maybeC ->
      do
        (newA, aSrcMap) <- instrumentExpr ctx a
        (newB, bSrcMap) <- instrumentExpr ctx b
        (newC, cSrcMap) <-
          case maybeC of
            Just c ->
              do
                (newC, cSrcMap) <- instrumentExpr ctx c
                return (Just newC, cSrcMap)

            Nothing ->
              return (Nothing, noAnswers)

        let
          newTuple =
            A.At region $ Can.Tuple newA newB newC

          srcMap =
            combine aSrcMap $ combine bSrcMap cSrcMap

        return (newTuple, srcMap)

    Can.Shader _ _ _ ->
      return (locExpr, noAnswers)


instrumentDef :: Context -> SrcMap.ExprId -> Can.Def -> WithIdState (Can.Def, Answers)
instrumentDef ctx exprId def =
  do
    let
      (name, body, typeInfo, isLambda) =
        normalizeDef def

      nameSrcMap =
        case isLambda of
          NoLambda ->
            makeExprRegion exprId $ A.toRegion name

          YesLambda ->
            noAnswers

    (newBody, bodySrcMap) <- instrumentExprWithId ctx exprId body

    let
      defSrcMap =
        combine nameSrcMap bodySrcMap

      newDef =
        rebuildDef name newBody typeInfo

    return (newDef, defSrcMap)


-- Generate source maps from a pattern, along with a list of variables declared in that pattern and
-- their associated expression ids.
instrumentPattern :: Can.Pattern -> WithIdState (Bag.Bag (N.Name, SrcMap.ExprId), Answers)
instrumentPattern (A.At region pat) =
  case pat of
    Can.PAnything ->
      return (Bag.empty, noAnswers)

    Can.PVar name ->
      do
        exprId <- freshExprId
        return
          ( Bag.one (name, exprId)
          , makeExprRegion exprId region
          )

    Can.PRecord fields ->
      do
        let names = map A.toValue fields
        let regions = map A.toRegion fields
        exprIds <- replicateM (length fields) freshExprId
        let vars = Bag.fromList id $ zip names exprIds
        let exprRegions = combineMany $ zipWith makeExprRegion exprIds regions
        return (vars, exprRegions)

    Can.PAlias inner name ->
      do
        (innerVars, innerAnswers) <- instrumentPattern inner
        exprId <- freshExprId
        let vars = Bag.append innerVars $ Bag.one (name, exprId)
        let answers = combine innerAnswers $ makeExprRegion exprId region
        return (vars, answers)

    Can.PUnit ->
      return (Bag.empty, noAnswers)

    Can.PTuple a b maybeC ->
      do
        (aVars, aAnswers) <- instrumentPattern a
        (bVars, bAnswers) <- instrumentPattern b
        (cVars, cAnswers) <-
          case maybeC of
            Just c ->
              instrumentPattern c

            Nothing ->
              return (Bag.empty, noAnswers)

        let vars = Bag.append aVars $ Bag.append bVars cVars
        let answers = combine aAnswers $ combine bAnswers cAnswers
        return (vars, answers)


    Can.PList items ->
      do
        (vars, answers) <- unzip <$> traverse instrumentPattern items
        return (Bag.concat vars, combineMany answers)

    Can.PCons x xs ->
      do
        (xVars, xAnswers) <- instrumentPattern x
        (xsVars, xsAnswers) <- instrumentPattern xs
        return (Bag.append xVars xsVars, combine xAnswers xsAnswers)

    Can.PBool _ _ ->
      return (Bag.empty, noAnswers)

    Can.PChr _ ->
      return (Bag.empty, noAnswers)

    Can.PStr _ ->
      return (Bag.empty, noAnswers)

    Can.PInt _ ->
      return (Bag.empty, noAnswers)

    Can.PCtor { Can._p_home = home, Can._p_name = name, Can._p_args = args } ->
      do
        exprId <- freshExprId

        let
          ctorSrcMap =
            makeQualified exprId region home name

          instrumentArg (Can.PatternCtorArg _ _ arg) =
            instrumentPattern arg

        (argVars, argAnswers) <- unzip <$> traverse instrumentArg args

        return (Bag.concat argVars, combine ctorSrcMap $ combineMany argAnswers)



-- INSTRUMENTATION CALLS


recordExpr :: SrcMap.ExprId -> Can.Expr -> Can.Expr
recordExpr (SrcMap.ExprId index) wrapped =
  A.At R.zero $ Can.Call Hooks.recordExpr [A.At R.zero $ Can.Int index, wrapped]


elmSeq :: Can.Expr -> Can.Expr -> Can.Expr
elmSeq sideEffect expr =
  A.At R.zero $ Can.Call Hooks.seq [sideEffect, expr]


recordVarsIn :: [(N.Name, SrcMap.ExprId)] -> Can.Expr -> Can.Expr
recordVarsIn vars body =
  let
    recordVarIn (name, varId) rest =
      elmSeq (recordExpr varId $ A.At R.zero $ Can.VarLocal name) rest
  in
    foldr recordVarIn body vars


recordCall :: SrcMap.ExprId -> Can.Expr -> [Can.Expr] -> WithIdState Can.Expr
recordCall (SrcMap.ExprId index) func args =
  do
    funcVar <- freshHygienicVar
    argVars <- replicateM (length args) freshHygienicVar

    let
      tempVarExpr var =
        A.At R.zero $ Can.VarLocal var

      funcVarExpr =
        tempVarExpr funcVar

      argVarExprs =
        map tempVarExpr argVars

      callLambda =
        A.At R.zero $ Can.Lambda [A.At R.zero Can.PUnit] $
        A.At R.zero $ Can.Call funcVarExpr argVarExprs

      exprId =
        A.At R.zero $ Can.Int index

      recordedCall =
        A.At R.zero $ Can.Call Hooks.recordCall [exprId, funcVarExpr, callLambda]

      tempDef var val =
        Can.Def (A.At R.zero var) [] val

      tempDefs =
        tempDef funcVar func : zipWith tempDef argVars args

      tempLet def body =
        A.At R.zero $ Can.Let def body

    return $ foldr tempLet recordedCall tempDefs


recordFrame :: SrcMap.FrameId -> Can.Expr -> Can.Expr
recordFrame frameId body =
  let
    bodyLambda =
      A.At R.zero $ Can.Lambda [A.At R.zero Can.PUnit] body

    frameStr =
      A.At R.zero $ Can.Str $ SrcMap.frameIdToText frameId
  in
    A.At R.zero $ Can.Call Hooks.recordFrame [frameStr, bodyLambda]


markInstrumented :: Can.Expr -> Can.Expr
markInstrumented lam =
  A.At R.zero $ Can.Call Hooks.markInstrumented [lam]



-- SOURCE MAP ANSWERS


data Answers =
  Answers
    { _frames :: Bag.Bag (SrcMap.FrameId, SrcMap.Frame)
    , _exprRegions :: Bag.Bag (SrcMap.ExprId, R.Region)
    , _exprNames :: Bag.Bag (SrcMap.ExprId, (ModuleName.Canonical, N.Name))
    }


noAnswers :: Answers
noAnswers =
  Answers
    { _frames = Bag.empty
    , _exprRegions = Bag.empty
    , _exprNames = Bag.empty
    }


combine :: Answers -> Answers -> Answers
combine (Answers frames1 regions1 names1) (Answers frames2 regions2 names2) =
  Answers
    { _frames = Bag.append frames1 frames2
    , _exprRegions = Bag.append regions1 regions2
    , _exprNames = Bag.append names1 names2
    }


combineMany :: [Answers] -> Answers
combineMany =
  List.foldl' combine noAnswers


makeExprRegion :: SrcMap.ExprId -> R.Region -> Answers
makeExprRegion exprId region =
  Answers
    { _frames = Bag.empty
    , _exprRegions = Bag.one (exprId, region)
    , _exprNames = Bag.empty
    }


makeExprName :: SrcMap.ExprId -> ModuleName.Canonical -> N.Name -> Answers
makeExprName exprId home name =
  Answers
    { _frames = Bag.empty
    , _exprRegions = Bag.empty
    , _exprNames = Bag.one (exprId, (home, name))
    }


makeQualified :: SrcMap.ExprId -> R.Region -> ModuleName.Canonical -> N.Name -> Answers
makeQualified exprId region home name =
  let
    regionSrcMap =
      makeExprRegion exprId region

    nameSrcMap =
      makeExprName exprId home name
  in
    combine regionSrcMap nameSrcMap


makeFrames :: Bag.Bag (SrcMap.FrameId, SrcMap.Frame) -> Answers
makeFrames frames =
  Answers
    { _frames = frames
    , _exprRegions = Bag.empty
    , _exprNames = Bag.empty
    }


collectOuterFrame :: SrcMap.FrameId -> R.Region -> Answers -> Bag.Bag (SrcMap.FrameId, SrcMap.Frame)
collectOuterFrame frameId frameRegion (Answers frames regions names) =
  let
    frame =
      SrcMap.Frame
        { SrcMap._region = frameRegion
        , SrcMap._exprRegions = Bag.toList regions
        , SrcMap._exprNames = Map.fromList $ Bag.toList names
        }
  in
    Bag.append frames $ Bag.one (frameId, frame)



-- CONTEXT


data Context =
  Context
    { _moduleName :: ModuleName.Canonical
    , _defName :: N.Name
    , _varIds :: Map.Map N.Name SrcMap.ExprId
    }


freshFrameId :: Context -> WithIdState SrcMap.FrameId
freshFrameId ctx =
  do
    index <- freshFrameIndex
    return $ SrcMap.FrameId (_moduleName ctx) (_defName ctx) index


addVar :: N.Name -> SrcMap.ExprId -> Context -> Context
addVar name exprId ctx =
  let
    newVarIds =
      Map.insert name exprId $ _varIds ctx
  in
    ctx { _varIds = newVarIds }


addVars :: [(N.Name, SrcMap.ExprId)] -> Context -> Context
addVars vars origCtx =
  let
    addBinding ctx (name, exprId) =
      addVar name exprId ctx
  in
    List.foldl' addBinding origCtx vars



-- GENERATE SEQUENTIAL IDENTIFIERS


data IdState =
  IdState
    { _frameCount :: Int
    , _exprCount :: Int
    , _hygienicVars :: [N.Name] -- Infinite stream
    }


type WithIdState a =
  State.State IdState a


runWithIdState :: Set.Set N.Name -> WithIdState a -> a
runWithIdState forbiddenVars =
  let
    candidateVar i =
      N.fromText $ "temp" <> Text.pack (show i)

    candidateVars =
      map candidateVar [0 :: Int ..]

    hygienicVars =
      filter (flip Set.notMember forbiddenVars) candidateVars
  in
    flip State.evalState $ IdState 0 0 hygienicVars


freshFrameIndex :: WithIdState Int
freshFrameIndex =
  do
    s <- State.get
    let index = _frameCount s
    State.put $ s { _frameCount = index + 1 }
    return index


freshExprId :: WithIdState SrcMap.ExprId
freshExprId =
  do
    s <- State.get
    let index = _exprCount s
    State.put $ s { _exprCount = index + 1 }
    return $ SrcMap.ExprId index


freshHygienicVar :: WithIdState N.Name
freshHygienicVar =
  do
    s <- State.get
    let (var : rest) = _hygienicVars s
    State.put $ s { _hygienicVars = rest }
    return var



-- DETERMINE VARIABLES USED


varsUsed :: Can.Expr -> Set.Set N.Name
varsUsed root =
  let
    exprVars :: Can.Expr -> Bag.Bag N.Name
    exprVars (A.At _ expr) =
      case expr of
        Can.VarLocal name ->
          Bag.one name

        Can.List items ->
          Bag.concat $ map exprVars items

        Can.Negate wrapped ->
          exprVars wrapped

        Can.Binop _ _ _ _ left right ->
          Bag.append (exprVars left) (exprVars right)

        Can.Lambda args body ->
          Bag.concat $ exprVars body : map patternVars args

        Can.Call func args ->
          Bag.concat $ exprVars func : map exprVars args

        Can.If branches finally ->
          let
            branchVars (condition, branch) =
              Bag.append (exprVars condition) (exprVars branch)
          in
            Bag.concat $ exprVars finally : map branchVars branches

        Can.Let def body ->
          Bag.append (defVars def) (exprVars body)

        Can.LetRec defs body ->
          Bag.concat $ exprVars body : map defVars defs

        Can.LetDestruct pat val body ->
          Bag.append (patternVars pat) $ Bag.append (exprVars val) (exprVars body)

        Can.Case val branches ->
          let
            branchVars (Can.CaseBranch pat body) =
              Bag.append (patternVars pat) (exprVars body)
          in
            Bag.concat $ exprVars val : map branchVars branches

        Can.Access record _ ->
          exprVars record

        Can.Update _ record updates ->
          let
            updateVars (Can.FieldUpdate _ val) =
              exprVars val
          in
            Bag.concat $ exprVars record : map (updateVars . snd) (Map.toList updates)

        Can.Record fields ->
          Bag.concat $ map (exprVars . snd) $ Map.toList fields

        Can.Tuple a b maybeC ->
          let
            cVars =
              case maybeC of
                Just c ->
                  exprVars c

                Nothing ->
                  Bag.empty
          in
            Bag.append (exprVars a) $ Bag.append (exprVars b) cVars

        _ ->
          Bag.empty

    patternVars (A.At _ pat) =
      case pat of
        Can.PVar name ->
          Bag.one name

        Can.PRecord names ->
          Bag.fromList A.toValue names

        Can.PAlias wrapped name ->
          Bag.append (patternVars wrapped) $ Bag.one name

        Can.PList items ->
          Bag.concat $ map patternVars items

        Can.PCons x xs ->
          Bag.append (patternVars x) (patternVars xs)

        Can.PCtor { Can._p_args = args } ->
          let
            argVars (Can.PatternCtorArg _ _ arg) =
              patternVars arg
          in
            Bag.concat $ map argVars args

        _ ->
          Bag.empty

    defVars def =
      let
        (name, args, body) =
          case def of
            Can.Def name_ args_ body_ ->
              (name_, args_, body_)

            Can.TypedDef name_ _ args_ body_ _ ->
              (name_, map fst args_, body_)
      in
        Bag.concat $ Bag.one (A.toValue name) : exprVars body : map patternVars args
  in
    Set.fromList $ Bag.toList $ exprVars root



-- HELPERS


defName :: Can.Def -> N.Name
defName def =
  case def of
    Can.Def (A.At _ name) _ _ ->
      name

    Can.TypedDef (A.At _ name) _ _ _ _ ->
      name


data IsLambda = NoLambda | YesLambda


-- Convert definitions with parameters into lambda assignments
normalizeDef :: Can.Def -> (A.Located N.Name, Can.Expr, Maybe (Can.FreeVars, Can.Type), IsLambda)
normalizeDef def =
  case def of
    Can.Def name [] body ->
      (name, body, Nothing, NoLambda)

    Can.TypedDef name freeVars [] body typeAnnot ->
      (name, body, Just (freeVars, typeAnnot), NoLambda)

    Can.Def name args body ->
      let
        funcRegion =
          R.merge (A.toRegion name) (A.toRegion body)

        func =
          A.At funcRegion $ Can.Lambda args body
      in
        (name, func, Nothing, YesLambda)

    Can.TypedDef name freeVars args body retType ->
      let
        funcRegion =
          R.merge (A.toRegion name) (A.toRegion body)

        func =
          A.At funcRegion $ Can.Lambda (map fst args) body

        funcType =
          foldr Can.TLambda retType $ map snd args
      in
        (name, func, Just (freeVars, funcType), YesLambda)


rebuildDef :: A.Located N.Name -> Can.Expr -> Maybe (Can.FreeVars, Can.Type) -> Can.Def
rebuildDef name body typeInfo =
  case typeInfo of
    Nothing ->
      Can.Def name [] body

    Just (freeVars, typeAnnot) ->
      Can.TypedDef name freeVars [] body typeAnnot
