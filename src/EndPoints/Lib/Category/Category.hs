{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

-- pure function to category end points
module EndPoints.Lib.Category.Category
  ( checkLogicPathForAddCateg, -- use in EndPoints.AddOneCategory
    changePathForAddCateg, -- use in EndPoints.AddOneCategory
    validSyntaxPath, -- use in EndPoints.AddOneCategory
    toCategories, -- use in EndPoints.Lib.CategoryIO
    step1CurPathWithChildrenToZeroTEST,
    step2DeleteHoleTEST,
    step3MadeHoleTEST,
    step4ChangeZeroWhithChildrenToNewTEST,
    checkLogicPathForEditCateg, -- use in EndPoints.EditOneCategory
    changePathsForEditCateg, -- use in EndPoints.EditOneCategory
  )
where

import qualified Data.Char as Char
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified EndPoints.Lib.Category.CategoryHelpTypes as CategoryHelpTypes
import qualified EndPoints.Lib.ToText as ToText
import qualified Logger
import qualified News
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes

-- | toCategories - change one ( Path , Id, Name ) to one record type DataTypes.Category
toCategories ::
  (DataTypes.Path, DataTypes.Id, DataTypes.Name) -> DataTypes.Category
toCategories (category_path, category_id, category_name) =
  DataTypes.Category {..}

-- | validSyntaxPath - check path received from the user. Return True then path is such us "1.2.34.9"
validSyntaxPath :: DataTypes.Path -> Bool
validSyntaxPath ys =
  checkPoints ys && foldr ((&&) . checkPath) True (wordsPath ys)
  where
    -- checkPoints - checking for a dot at the end and any characters inside path

    checkPoints :: DataTypes.Path -> Bool
    checkPoints kx =
      not (not (null kx) && (last kx == '.' || head kx == '.'))
        && checkPoints' kx
      where
        checkPoints' :: DataTypes.Path -> Bool
        checkPoints' [] = True
        checkPoints' (x : xs)
          | x == '.' && head xs == '.' = False
          | otherwise = checkPoints' xs
    -- checkPath - check if any piece of path starts from zero
    checkPath :: String -> Bool
    checkPath zs =
      not (not (null zs) && head zs == '0') && checkPathOnlyDigit zs
    --  checkPathOnlyDigit - only numbers inside
    checkPathOnlyDigit :: String -> Bool
    checkPathOnlyDigit [] = True
    checkPathOnlyDigit (x : xs)
      | Char.isDigit x = checkPathOnlyDigit xs
      | otherwise = False

-- | checkLogicPathForAddCateg. If Path For Add Category not valid return Error, else  return (CreateCategoryRequest , [DataTypes.Category])
checkLogicPathForAddCateg ::
  Monad m =>
  News.Handle m ->
  DataTypes.CreateCategoryRequest ->
  Either ErrorTypes.AddEditCategoryError [DataTypes.Category] ->
  m
    ( Either
        ErrorTypes.AddEditCategoryError
        ( DataTypes.CreateCategoryRequest,
          [DataTypes.Category]
        )
    )
checkLogicPathForAddCateg _ _ (Left err) = return $ Left err
checkLogicPathForAddCateg h r@DataTypes.CreateCategoryRequest {..} (Right categories) =
  if validLogicPathForAddCateg categories r
    then return $ Right (r, categories)
    else do
      Logger.logError (News.hLogHandle h) $
        T.pack $
          show $
            ErrorTypes.InvalidValuePath $
              ErrorTypes.InvalidContent
                ( "checkLogicPath: BAD! Path is not valid! Must not hole in numbering! "
                    ++ path
                )
      return $ Left $ ErrorTypes.InvalidValuePath $ ErrorTypes.InvalidContent []

-- | checkLogicPathForEditCateg. If Path For Add Category not valid return Error, else  return (EditCategoryFullRequest , [DataTypes.Category])
checkLogicPathForEditCateg ::
  Monad m =>
  News.Handle m ->
  Int ->
  DataTypes.EditCategoryRequest ->
  Either ErrorTypes.AddEditCategoryError [DataTypes.Category] ->
  m
    ( Either
        ErrorTypes.AddEditCategoryError
        ( CategoryHelpTypes.EditCategoryFullRequest,
          [DataTypes.Category]
        )
    )
checkLogicPathForEditCateg _ _ _ (Left err) = return $ Left err
checkLogicPathForEditCateg h idCat r (Right categories) = do
  rez <- toFullReqiest h idCat r categories
  case rez of
    Left err -> return $ Left err
    Right editCatFullReq ->
      if checkParentChild editCatFullReq
        then return $ Right (editCatFullReq, categories)
        else do
          Logger.logError (News.hLogHandle h) $
            T.pack $
              show $
                ErrorTypes.InvalidValuePath $
                  ErrorTypes.InvalidContent
                    "checkParentChild: BAD! Parent DOES NOT become a child of himself "
          Logger.logDebug (News.hLogHandle h) $
            T.pack $ show $ ToText.toText editCatFullReq
          return $
            Left $ ErrorTypes.InvalidValuePath $ ErrorTypes.InvalidContent []
  where
    checkParentChild :: CategoryHelpTypes.EditCategoryFullRequest -> Bool
    checkParentChild req
      | CategoryHelpTypes.cur_path' req == CategoryHelpTypes.new_path' req =
        True
      | CategoryHelpTypes.cur_path' req
          == pathFromWords
            ( take
                (CategoryHelpTypes.cur_level' req)
                (wordsPath (CategoryHelpTypes.new_path' req))
            ) =
        False
      | otherwise = True

--------- ADD ONE CATEGORY-------------

-- | changePathForAddCateg Parent DOES NOT become a child of his child; does not contain holes in the numbering
changePathForAddCateg ::
  DataTypes.CreateCategoryRequest ->
  [DataTypes.Category] ->
  [CategoryHelpTypes.EditCategory]
changePathForAddCateg DataTypes.CreateCategoryRequest {..} categs =
  editCategoryRequests $
    changePath path (length $ wordsPath path) (mapCategory categs)

--------- EDIT ONE CATEGORY-------------

-- | changePathsForEditCateg we consider that a valid puff comes, if cur_path' == new_path' we change only the name
changePathsForEditCateg ::
  CategoryHelpTypes.EditCategoryFullRequest ->
  [DataTypes.Category] ->
  Maybe [CategoryHelpTypes.EditCategory]
changePathsForEditCateg req categ =
  if CategoryHelpTypes.cur_path' req == CategoryHelpTypes.new_path' req
    then Just []
    else
      changePathResultReq
        categ
        (step4ChangeZeroWhithChildrenToNewTEST req categ)

-- Step 1 (result of the first step)
step1CurPathWithChildrenToZeroTEST ::
  CategoryHelpTypes.EditCategoryFullRequest ->
  [DataTypes.Category] ->
  [DataTypes.Category]
step1CurPathWithChildrenToZeroTEST req categ =
  toCategoriesEmptyName $ curPathWithChildrenToZero req (mapCategory categ)

-- | changeCurPathToZero Change the current paf and its children to zero and delete them from the category table
curPathWithChildrenToZero ::
  CategoryHelpTypes.EditCategoryFullRequest ->
  Map.Map DataTypes.Path (DataTypes.Id, DataTypes.Path) ->
  Map.Map DataTypes.Path (DataTypes.Id, DataTypes.Path)
curPathWithChildrenToZero eq mapCateg =
  case Map.lookup (CategoryHelpTypes.cur_path' eq) mapCateg of -- change the current paf to zero
    Just (idCat, _) ->
      curPathChildrenToZeroChildren
        (CategoryHelpTypes.cur_path' eq)
        eq
        ( Map.delete (CategoryHelpTypes.cur_path' eq) $
            Map.insert "0" (idCat, "0") mapCateg
        )
    Nothing -> mapCateg
  where
    -- changeCurPathChildrenToZero : change chilren of curPath to childrenZero path and remove the children from the table

    curPathChildrenToZeroChildren ::
      DataTypes.Path ->
      CategoryHelpTypes.EditCategoryFullRequest ->
      Map.Map DataTypes.Path (DataTypes.Id, DataTypes.Path) ->
      Map.Map DataTypes.Path (DataTypes.Id, DataTypes.Path)
    curPathChildrenToZeroChildren cur_path e mapCat =
      case Map.lookupGT cur_path mapCat of -- find the children of the current path
        Nothing -> mapCat -- cur_path' was last in the table
        Just (childPath, (childId, _)) ->
          if levelPath childPath > CategoryHelpTypes.cur_level' e -- it's definitely a baby
            then
              curPathChildrenToZeroChildren childPath e $
                Map.delete childPath $
                  Map.insert
                    (childZeroPath childPath e)
                    (childId, childZeroPath childPath e)
                    mapCat
            else mapCat
    -- childZeroPath: Bite off the paf's head of the required length and attach it 0
    childZeroPath ::
      DataTypes.Path ->
      CategoryHelpTypes.EditCategoryFullRequest ->
      DataTypes.Path
    childZeroPath childPath req =
      pathFromWords
        ("0" : drop (CategoryHelpTypes.new_level' req) (wordsPath childPath))

-- Step 2 (the result of the first two steps)
step2DeleteHoleTEST ::
  CategoryHelpTypes.EditCategoryFullRequest ->
  [DataTypes.Category] ->
  [DataTypes.Category]
step2DeleteHoleTEST req categ =
  toCategoriesEmptyName $
    deleteHole
      (CategoryHelpTypes.cur_path' req)
      req
      (mapCategory (step1CurPathWithChildrenToZeroTEST req categ))

deleteHole ::
  DataTypes.Path ->
  CategoryHelpTypes.EditCategoryFullRequest ->
  Map.Map DataTypes.Path (DataTypes.Id, DataTypes.Path) ->
  Map.Map DataTypes.Path (DataTypes.Id, DataTypes.Path)
deleteHole curPath req mapCat =
  case Map.lookupGT curPath mapCat of
    Nothing -> mapCat
    Just (nexPath, (idNextPath, _)) ->
      if levelPath nexPath >= CategoryHelpTypes.cur_level' req
        then
          deleteHole nexPath req $
            Map.delete nexPath $
              Map.insert
                (previosPath nexPath (CategoryHelpTypes.cur_level' req))
                ( idNextPath,
                  previosPath nexPath (CategoryHelpTypes.cur_level' req)
                )
                mapCat
        else mapCat

-- Step 3 (the result of the first three steps)
step3MadeHoleTEST ::
  CategoryHelpTypes.EditCategoryFullRequest ->
  [DataTypes.Category] ->
  [DataTypes.Category]
step3MadeHoleTEST r cats =
  toCategoriesEmptyName $ madeHole r (mapCategory (step2DeleteHoleTEST r cats))

madeHole ::
  CategoryHelpTypes.EditCategoryFullRequest ->
  Map.Map DataTypes.Path (DataTypes.Id, DataTypes.Path) ->
  Map.Map DataTypes.Path (DataTypes.Id, DataTypes.Path)
madeHole req mapCateg =
  if CategoryHelpTypes.new_path_stay_after_id' req == 0
    then changePath "1" 1 mapCateg
    else case currentPathName
      (CategoryHelpTypes.new_path_stay_after_id' req)
      (toCategoriesEmptyName mapCateg) of
      Nothing -> mapCateg
      Just (newPath, _) ->
        let validNewPath =
              nextPath newPath (CategoryHelpTypes.new_level' req)
         in changePath
              validNewPath
              (CategoryHelpTypes.new_level' req)
              mapCateg

-- Step 4  (result of all four steps)
step4ChangeZeroWhithChildrenToNewTEST ::
  CategoryHelpTypes.EditCategoryFullRequest ->
  [DataTypes.Category] ->
  [DataTypes.Category]
step4ChangeZeroWhithChildrenToNewTEST req categ =
  toCategoriesEmptyName $
    changeZeroWhithChildrenToNew req (mapCategory (step3MadeHoleTEST req categ))

changeZeroWhithChildrenToNew ::
  CategoryHelpTypes.EditCategoryFullRequest ->
  Map.Map DataTypes.Path (DataTypes.Id, DataTypes.Path) ->
  Map.Map DataTypes.Path (DataTypes.Id, DataTypes.Path)
changeZeroWhithChildrenToNew req mapCat =
  if CategoryHelpTypes.new_path_stay_after_id' req == 0
    then
      changeZeroChildToNew "0" req $
        Map.delete "0" $ Map.insert "1" (CategoryHelpTypes.id' req, "1") mapCat
    else case currentPathName
      (CategoryHelpTypes.new_path_stay_after_id' req)
      (toCategoriesEmptyName mapCat) of
      Nothing -> mapCat
      Just (newPath, _) ->
        let validNewPath =
              pathFromWords $
                take
                  (CategoryHelpTypes.new_level' req)
                  ( wordsPath
                      (nextPath newPath (CategoryHelpTypes.new_level' req)) -- cut the puff to the desired length
                  )
         in changeZeroChildToNew "0" req $
              Map.delete "0" $
                Map.insert
                  validNewPath
                  (CategoryHelpTypes.id' req, validNewPath)
                  mapCat
  where
    changeZeroChildToNew ::
      DataTypes.Path ->
      CategoryHelpTypes.EditCategoryFullRequest ->
      Map.Map DataTypes.Path (DataTypes.Id, DataTypes.Path) ->
      Map.Map DataTypes.Path (DataTypes.Id, DataTypes.Path)
    changeZeroChildToNew path e mapCa =
      case Map.lookupGT path mapCa of -- find the children of the current path knowing that they are
        Nothing -> mapCa
        Just (childPath, (childId, _)) ->
          if levelPath childPath > 1
            then
              changeZeroChildToNew childPath e $
                Map.delete childPath $
                  Map.insert
                    (childFromZeroPath childPath e)
                    (childId, childFromZeroPath childPath e)
                    mapCa
            else mapCa
    childFromZeroPath ::
      DataTypes.Path ->
      CategoryHelpTypes.EditCategoryFullRequest ->
      DataTypes.Path
    childFromZeroPath childPath CategoryHelpTypes.EditCategoryFullRequest {..} =
      pathFromWords (wordsPath new_path' ++ drop 1 (wordsPath childPath))

toFullReqiest ::
  Monad m =>
  News.Handle m ->
  DataTypes.Id ->
  DataTypes.EditCategoryRequest ->
  [DataTypes.Category] ->
  m (Either ErrorTypes.AddEditCategoryError CategoryHelpTypes.EditCategoryFullRequest)
toFullReqiest
  h
  idCat
  DataTypes.EditCategoryRequest
    { DataTypes.new_path = mustbePath,
      DataTypes.new_category = mustbeName
    }
  categs =
    case currentPathName idCat categs of
      Just (curPath, curName) ->
        case newPathStayAfter (new curPath mustbePath) categs of
          Nothing -> do
            Logger.logError (News.hLogHandle h) $
              T.pack $
                show $
                  ErrorTypes.InvalidValuePath $
                    ErrorTypes.InvalidContent
                      "toFullReqiest BAD! Don`t do a hole in numbering"
            return $
              Left $ ErrorTypes.InvalidValuePath $ ErrorTypes.InvalidContent []
          Just idNewPathStayAfter ->
            return $ Right $ rez curPath curName idNewPathStayAfter
      Nothing -> do
        Logger.logError (News.hLogHandle h) $
          T.pack $
            show $
              ErrorTypes.InvalidValuePath $
                ErrorTypes.InvalidContent
                  "toFullReqiest BAD! LOGIC ERROR Id in TABLE category is not PRIMARY KEY"
        return $ Left $ ErrorTypes.InvalidValuePath $ ErrorTypes.InvalidContent []
    where
      -- new - if path or name don't change in request, new is equal current value

      new :: a -> Maybe a -> a
      new val Nothing = val
      new _ (Just val) = val
      rez ::
        DataTypes.Path ->
        DataTypes.Name ->
        DataTypes.Id ->
        CategoryHelpTypes.EditCategoryFullRequest
      rez curPath curName idNewPathStayAfter =
        CategoryHelpTypes.EditCategoryFullRequest
          { id' = idCat,
            cur_path' = curPath,
            cur_category' = curName,
            cur_level' = levelPath curPath,
            new_path' = new curPath mustbePath,
            new_category' = new curName mustbeName,
            new_level' = levelPath $ new curPath mustbePath,
            new_path_stay_after_id' = idNewPathStayAfter
          }
      -- newPathStayAfter - find path previos to new path, bесause new_path name must be changed
      newPathStayAfter ::
        DataTypes.Path -> [DataTypes.Category] -> Maybe DataTypes.Id
      newPathStayAfter "1" _ = Just 0
      newPathStayAfter path cats =
        case last $ wordsPath (previosPath path (levelPath path)) of
          "0" ->
            case Map.lookup
              (pathFromWords (init $ wordsPath path))
              (mapCategory cats) of
              Just (idCa, _) -> Just idCa
              Nothing -> Nothing
          _ ->
            case Map.lookup (previosPath path $ levelPath path) (mapCategory cats) of
              Just (idCa, _) ->
                Just
                  ( lastIdAllChildren
                      idCa
                      (previosPath path $ levelPath path)
                      (levelPath path)
                      (mapCategory cats)
                  )
              Nothing -> Nothing
      -- I return the puff of the last child, if there are no children, then the transferred puff
      lastIdAllChildren ::
        DataTypes.Id ->
        DataTypes.Path ->
        CategoryHelpTypes.Level ->
        Map.Map DataTypes.Path (DataTypes.Id, DataTypes.Path) ->
        DataTypes.Id
      lastIdAllChildren i p l mapcs =
        case Map.lookupGT p mapcs of
          Nothing -> i
          Just (p', (i', _)) ->
            -- looking for children Maybe (Path, (Id, Path))
            if levelPath p' > l -- found a child
              then lastIdAllChildren i' p' l mapcs
              else i

-- currentPathName - find path by id in category list
currentPathName ::
  DataTypes.Id ->
  [DataTypes.Category] ->
  Maybe (DataTypes.Path, DataTypes.Name)
currentPathName a cats =
  case filter (\x -> DataTypes.category_id x == a) cats of
    [x] -> Just (DataTypes.category_path x, DataTypes.category_name x)
    _ -> Nothing

nextPath :: DataTypes.Path -> CategoryHelpTypes.Level -> DataTypes.Path
nextPath x level =
  if level - length (wordsPath x) == 1
    then x ++ ".1"
    else pathFromWords $ take (level - 1) xs ++ [changed] ++ drop level xs
  where
    xs = wordsPath x
    changed = show (1 + read (xs !! (level - 1)) :: Int)

previosPath :: DataTypes.Path -> CategoryHelpTypes.Level -> DataTypes.Path
previosPath x level =
  pathFromWords $ take (level - 1) xs ++ [changed] ++ drop level xs
  where
    xs = wordsPath x
    changed = show (read (xs !! (level - 1)) - 1 :: Int)

pathFromWords :: [String] -> String
pathFromWords [] = []
pathFromWords [y] = y
pathFromWords (y : ys) = y ++ "." ++ pathFromWords ys

mapCategory ::
  [DataTypes.Category] ->
  Map.Map DataTypes.Path (DataTypes.Id, DataTypes.Path)
mapCategory xs = Map.fromList $ map help xs
  where
    help ::
      DataTypes.Category -> (DataTypes.Path, (DataTypes.Id, DataTypes.Path))
    help DataTypes.Category {..} = (category_path, (category_id, []))

toCategoriesEmptyName ::
  Map.Map DataTypes.Path (DataTypes.Id, DataTypes.Path) ->
  [DataTypes.Category]
toCategoriesEmptyName mapCateg = map toCategory $ Map.toList mapCateg
  where
    toCategory ::
      (DataTypes.Path, (DataTypes.Id, DataTypes.Path)) -> DataTypes.Category
    toCategory (path, (idCat, [])) =
      DataTypes.Category
        { category_id = idCat,
          category_path = path,
          category_name = ""
        }
    toCategory (_, (idCat, path)) =
      DataTypes.Category
        { category_id = idCat,
          category_path = path,
          category_name = ""
        }

-- | editCategoryRequests list for changes
editCategoryRequests ::
  Map.Map DataTypes.Path (DataTypes.Id, DataTypes.Path) ->
  [CategoryHelpTypes.EditCategory]
editCategoryRequests mapCateg =
  map toEditCategoryRequest $ deleteEmpty $ Map.toList mapCateg
  where
    deleteEmpty ::
      [(DataTypes.Path, (DataTypes.Id, DataTypes.Path))] ->
      [(DataTypes.Path, DataTypes.Id)]
    deleteEmpty [] = []
    deleteEmpty ((_, (_, [])) : xs) = deleteEmpty xs
    deleteEmpty ((_, (idCat, newPath)) : xs) = (newPath, idCat) : deleteEmpty xs
    toEditCategoryRequest ::
      (DataTypes.Path, DataTypes.Id) -> CategoryHelpTypes.EditCategory
    toEditCategoryRequest (path, idCat) =
      CategoryHelpTypes.EditCategory {_id = idCat, new_path = path}

wordsPath :: String -> [String]
wordsPath s =
  case dropWhile (== '.') s of
    "" -> []
    s' -> w : wordsPath s''
      where
        (w, s'') = break (== '.') s'

levelPath :: DataTypes.Path -> Int
levelPath p = length $ wordsPath p

-- | validLogicPathForAddCateg. Path For Add Category does not contain holes in the numbering
validLogicPathForAddCateg ::
  [DataTypes.Category] -> DataTypes.CreateCategoryRequest -> Bool
validLogicPathForAddCateg categories DataTypes.CreateCategoryRequest {DataTypes.path = "1"} =
  Map.member "1" (mapCategory categories)
    || Map.null (mapCategory categories)
    || False
validLogicPathForAddCateg categories DataTypes.CreateCategoryRequest {..}
  | last (wordsPath path) == "1" = Map.member levelUpPath catMap
  | otherwise =
    Map.member (previosPath path (length $ wordsPath path)) catMap
      || Map.member path catMap
  where
    catMap =
      mapCategory categories ::
        Map.Map
          DataTypes.Path
          ( DataTypes.Id,
            DataTypes.Path
          )
    levelUpPath = pathFromWords $ take (levelPath path - 1) (wordsPath path)

changePath ::
  DataTypes.Path -> -- path to add
  CategoryHelpTypes.Level -> -- path's level (1.2.4 - has level=3)
  Map.Map DataTypes.Path (DataTypes.Id, DataTypes.Path) -> -- [DataTypes.Category] representation in the  map for ease of search. Before changes the third element of the tuple = []
  Map.Map DataTypes.Path (DataTypes.Id, DataTypes.Path) -- [EditCategory] representation in the  map
changePath p level mapCat =
  case Map.lookup p mapCat of
    Just (idCurPath, _) ->
      case Map.lookupGT p mapCat of
        Just (newPath, (_, _)) ->
          if levelPath newPath < level
            then Map.insert p (idCurPath, nextPath p level) mapCat
            else
              changePath newPath level $
                Map.insert p (idCurPath, nextPath p level) mapCat
        Nothing -> Map.insert p (idCurPath, nextPath p level) mapCat
    Nothing -> mapCat

changePathResultReq ::
  [DataTypes.Category] ->
  [DataTypes.Category] ->
  Maybe [CategoryHelpTypes.EditCategory]
changePathResultReq old new =
  if length (pairsSortById old) == length (pairsSortById new)
    && length
      ( filter
          filterById
          (zipWith help1 (pairsSortById old) (pairsSortById new))
      )
    == length (pairsSortById new)
    then Just (help2 (zipWith help1 (pairsSortById old) (pairsSortById new)))
    else Nothing
  where
    toPair :: DataTypes.Category -> (DataTypes.Id, DataTypes.Path)
    toPair DataTypes.Category {..} = (category_id, category_path)
    pairsSortById :: [DataTypes.Category] -> [(DataTypes.Id, DataTypes.Path)]
    pairsSortById c = L.sortOn fst (map toPair c)
    help1 ::
      (DataTypes.Id, DataTypes.Path) ->
      (DataTypes.Id, DataTypes.Path) ->
      (DataTypes.Id, DataTypes.Id, DataTypes.Path, DataTypes.Path)
    help1 (id1, oldPath) (id2, newPath) = (id1, id2, oldPath, newPath)
    filterById ::
      (DataTypes.Id, DataTypes.Id, DataTypes.Path, DataTypes.Path) -> Bool
    filterById (id1, id2, _, _) = id1 == id2
    help2 ::
      [(DataTypes.Id, DataTypes.Id, DataTypes.Path, DataTypes.Path)] ->
      [CategoryHelpTypes.EditCategory]
    help2 [] = []
    help2 ((idCat, _, oldPath, newPath) : xs)
      | oldPath /= newPath =
        (CategoryHelpTypes.EditCategory {_id = idCat, new_path = newPath}) :
        help2 xs
      | otherwise = help2 xs
