-- pure function to category end points
module EndPoints.Lib.Category.Category
  ( checkLogicPathForAddCategory, -- use in EndPoints.AddOneCategory
    changePathForAddCategory, -- use in EndPoints.AddOneCategory
    validSyntaxPath, -- use in EndPoints.AddOneCategory
    toCategories, -- use in EndPoints.Lib.CategoryIO
    step1CurPathWithChildrenToZeroTEST,
    step2DeleteHoleTEST,
    step3MadeHoleTEST,
    step4ChangeZeroWithChildrenToNewTEST,
    checkLogicPathForEditCategory, -- use in EndPoints.EditOneCategory
    changePathsForEditCategory, -- use in EndPoints.EditOneCategory
  )
where

import qualified Data.Char as Char
import qualified Data.List as L
import qualified Data.Map as Map
import qualified EndPoints.Lib.Category.CategoryHelpTypes as CategoryHelpTypes
import qualified EndPoints.Lib.ToText as ToText
import Logger (logDebug, logError, (.<))
import qualified News
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes

-- | toCategories - change one ( Path , Id, Name ) to one record type DataTypes.Category
toCategories ::
  (DataTypes.Path, DataTypes.Id, DataTypes.Name) -> DataTypes.Category
toCategories (categoryPath, categoryId, categoryName) = DataTypes.Category {..}

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

-- | checkLogicPathForAddCategory. If Path For Add Category not valid return Error, else  return (CreateCategoryRequest , [DataTypes.Category])
checkLogicPathForAddCategory ::
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
checkLogicPathForAddCategory _ _ (Left err) = return $ Left err
checkLogicPathForAddCategory h r (Right categories) =
  if validLogicPathForAddCategory categories r
    then return $ Right (r, categories)
    else do
      Logger.logError
        (News.hLogHandle h)
        ( "ERROR "
            .< ErrorTypes.InvalidValuePath
              ( ErrorTypes.InvalidContent
                  "checkLogicPath: BAD! Path is not valid! Must not hole in numbering! "
              )
        )
      return $ Left $ ErrorTypes.InvalidValuePath $ ErrorTypes.InvalidContent []

-- | checkLogicPathForEditCategory. If Path For Add Category not valid return Error, else  return (EditCategoryFullRequest , [DataTypes.Category])
checkLogicPathForEditCategory ::
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
checkLogicPathForEditCategory _ _ _ (Left err) = return $ Left err
checkLogicPathForEditCategory h idCat r (Right categories) = do
  rez <- toFullRequest h idCat r categories
  case rez of
    Left err -> return $ Left err
    Right editCatFullReq ->
      if checkParentChild editCatFullReq
        then return $ Right (editCatFullReq, categories)
        else do
          Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.InvalidValuePath (ErrorTypes.InvalidContent "checkParentChild: BAD! Parent DOES NOT become a child of himself "))
          Logger.logDebug (News.hLogHandle h) $ ToText.toText editCatFullReq
          return $
            Left $ ErrorTypes.InvalidValuePath $ ErrorTypes.InvalidContent []
  where
    checkParentChild :: CategoryHelpTypes.EditCategoryFullRequest -> Bool
    checkParentChild req
      | CategoryHelpTypes.curPath' req == CategoryHelpTypes.newPath' req = True
      | CategoryHelpTypes.curPath' req
          == pathFromWords
            ( take
                (CategoryHelpTypes.curLevel' req)
                (wordsPath (CategoryHelpTypes.newPath' req))
            ) =
        False
      | otherwise = True

--------- ADD ONE CATEGORY-------------

-- | changePathForAddCategory Parent DOES NOT become a child of his child; does not contain holes in the numbering
changePathForAddCategory ::
  DataTypes.CreateCategoryRequest ->
  [DataTypes.Category] ->
  [CategoryHelpTypes.EditCategory]
changePathForAddCategory DataTypes.CreateCategoryRequest {..} categories =
  editCategoryRequests $
    changePath path (length $ wordsPath path) (mapCategory categories)

--------- EDIT ONE CATEGORY-------------

-- | changePathsForEditCategory we consider that a valid puff comes, if cur_path' == new_path' we change only the name
changePathsForEditCategory ::
  CategoryHelpTypes.EditCategoryFullRequest ->
  [DataTypes.Category] ->
  Maybe [CategoryHelpTypes.EditCategory]
changePathsForEditCategory req categories =
  if CategoryHelpTypes.curPath' req == CategoryHelpTypes.newPath' req
    then Just []
    else
      changePathResultReq
        categories
        (step4ChangeZeroWithChildrenToNewTEST req categories)

-- Step 1 (result of the first step)
step1CurPathWithChildrenToZeroTEST ::
  CategoryHelpTypes.EditCategoryFullRequest ->
  [DataTypes.Category] ->
  [DataTypes.Category]
step1CurPathWithChildrenToZeroTEST req categories =
  toCategoriesEmptyName $ curPathWithChildrenToZero req (mapCategory categories)

-- | changeCurPathToZero Change the current paf and its children to zero and delete them from the category table
curPathWithChildrenToZero ::
  CategoryHelpTypes.EditCategoryFullRequest ->
  Map.Map DataTypes.Path (DataTypes.Id, DataTypes.Path) ->
  Map.Map DataTypes.Path (DataTypes.Id, DataTypes.Path)
curPathWithChildrenToZero eq mapCategories =
  case Map.lookup (CategoryHelpTypes.curPath' eq) mapCategories of -- change the current paf to zero
    Just (idCat, _) ->
      curPathChildrenToZeroChildren
        (CategoryHelpTypes.curPath' eq)
        eq
        ( Map.delete (CategoryHelpTypes.curPath' eq) $
            Map.insert "0" (idCat, "0") mapCategories
        )
    Nothing -> mapCategories
  where
    -- changeCurPathChildrenToZero : change children of curPath to childrenZero path and remove the children from the table

    curPathChildrenToZeroChildren ::
      DataTypes.Path ->
      CategoryHelpTypes.EditCategoryFullRequest ->
      Map.Map DataTypes.Path (DataTypes.Id, DataTypes.Path) ->
      Map.Map DataTypes.Path (DataTypes.Id, DataTypes.Path)
    curPathChildrenToZeroChildren cur_path e mapCat =
      case Map.lookupGT cur_path mapCat of -- find the children of the current path
        Nothing -> mapCat -- cur_path' was last in the table
        Just (childPath, (childId, _)) ->
          if levelPath childPath > CategoryHelpTypes.curLevel' e -- it's definitely a baby
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
        ("0" : drop (CategoryHelpTypes.newLevel' req) (wordsPath childPath))

-- Step 2 (the result of the first two steps)
step2DeleteHoleTEST ::
  CategoryHelpTypes.EditCategoryFullRequest ->
  [DataTypes.Category] ->
  [DataTypes.Category]
step2DeleteHoleTEST req categories =
  toCategoriesEmptyName $
    deleteHole
      (CategoryHelpTypes.curPath' req)
      req
      (mapCategory (step1CurPathWithChildrenToZeroTEST req categories))

deleteHole ::
  DataTypes.Path ->
  CategoryHelpTypes.EditCategoryFullRequest ->
  Map.Map DataTypes.Path (DataTypes.Id, DataTypes.Path) ->
  Map.Map DataTypes.Path (DataTypes.Id, DataTypes.Path)
deleteHole curPath req mapCat =
  case Map.lookupGT curPath mapCat of
    Nothing -> mapCat
    Just (nexPath, (idNextPath, _)) ->
      if levelPath nexPath >= CategoryHelpTypes.curLevel' req
        then
          deleteHole nexPath req $
            Map.delete nexPath $
              Map.insert
                (previousPath nexPath (CategoryHelpTypes.curLevel' req))
                ( idNextPath,
                  previousPath nexPath (CategoryHelpTypes.curLevel' req)
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
madeHole req mapCategories =
  if CategoryHelpTypes.newPathStayAfterId' req == 0
    then changePath "1" 1 mapCategories
    else case currentPathName
      (CategoryHelpTypes.newPathStayAfterId' req)
      (toCategoriesEmptyName mapCategories) of
      Nothing -> mapCategories
      Just (newPath, _) ->
        let validNewPath =
              nextPath newPath (CategoryHelpTypes.newLevel' req)
         in changePath
              validNewPath
              (CategoryHelpTypes.newLevel' req)
              mapCategories

-- Step 4  (result of all four steps)
step4ChangeZeroWithChildrenToNewTEST ::
  CategoryHelpTypes.EditCategoryFullRequest ->
  [DataTypes.Category] ->
  [DataTypes.Category]
step4ChangeZeroWithChildrenToNewTEST req categories =
  toCategoriesEmptyName $
    changeZeroWithChildrenToNew
      req
      (mapCategory (step3MadeHoleTEST req categories))

changeZeroWithChildrenToNew ::
  CategoryHelpTypes.EditCategoryFullRequest ->
  Map.Map DataTypes.Path (DataTypes.Id, DataTypes.Path) ->
  Map.Map DataTypes.Path (DataTypes.Id, DataTypes.Path)
changeZeroWithChildrenToNew req mapCat =
  if CategoryHelpTypes.newPathStayAfterId' req == 0
    then
      changeZeroChildToNew "0" req $
        Map.delete "0" $ Map.insert "1" (CategoryHelpTypes.id' req, "1") mapCat
    else case currentPathName
      (CategoryHelpTypes.newPathStayAfterId' req)
      (toCategoriesEmptyName mapCat) of
      Nothing -> mapCat
      Just (newPath, _) ->
        let validNewPath =
              pathFromWords $
                take
                  (CategoryHelpTypes.newLevel' req)
                  ( wordsPath
                      (nextPath newPath (CategoryHelpTypes.newLevel' req)) -- cut the puff to the desired length
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
      case Map.lookupGT path mapCa of
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
      pathFromWords (wordsPath newPath' ++ drop 1 (wordsPath childPath))

toFullRequest ::
  Monad m =>
  News.Handle m ->
  DataTypes.Id ->
  DataTypes.EditCategoryRequest ->
  [DataTypes.Category] ->
  m (Either ErrorTypes.AddEditCategoryError CategoryHelpTypes.EditCategoryFullRequest)
toFullRequest
  h
  idCat
  DataTypes.EditCategoryRequest
    { DataTypes.newPath = mustBePath,
      DataTypes.newCategory = mustBeName
    }
  categories =
    case currentPathName idCat categories of
      Just (curPath, curName) ->
        case newPathStayAfter (new curPath mustBePath) categories of
          Nothing -> do
            Logger.logError
              (News.hLogHandle h)
              ( "ERROR "
                  .< ErrorTypes.InvalidValuePath
                    ( ErrorTypes.InvalidContent
                        "toFullRequest BAD! Don`t do a hole in numbering"
                    )
              )
            return $
              Left $ ErrorTypes.InvalidValuePath $ ErrorTypes.InvalidContent []
          Just idNewPathStayAfter ->
            return $ Right $ rez curPath curName idNewPathStayAfter
      Nothing -> do
        Logger.logError
          (News.hLogHandle h)
          ( "ERROR "
              .< ErrorTypes.InvalidValuePath
                ( ErrorTypes.InvalidContent
                    "toFullRequest BAD! LOGIC ERROR Id in TABLE category is not PRIMARY KEY"
                )
          )
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
            curPath' = curPath,
            curCategory' = curName,
            curLevel' = levelPath curPath,
            newPath' = new curPath mustBePath,
            newCategory' = new curName mustBeName,
            newLevel' = levelPath $ new curPath mustBePath,
            newPathStayAfterId' = idNewPathStayAfter
          }
      -- newPathStayAfter - find path previous to new path, because new_path name must be changed
      newPathStayAfter ::
        DataTypes.Path -> [DataTypes.Category] -> Maybe DataTypes.Id
      newPathStayAfter "1" _ = Just 0
      newPathStayAfter path cats =
        case last $ wordsPath (previousPath path (levelPath path)) of
          "0" ->
            case Map.lookup
              (pathFromWords (init $ wordsPath path))
              (mapCategory cats) of
              Just (idCa, _) -> Just idCa
              Nothing -> Nothing
          _ ->
            case Map.lookup
              (previousPath path $ levelPath path)
              (mapCategory cats) of
              Just (idCa, _) ->
                Just
                  ( lastIdAllChildren
                      idCa
                      (previousPath path $ levelPath path)
                      (levelPath path)
                      (mapCategory cats)
                  )
              Nothing -> Nothing
      -- lastIdAllChildren - I return the puff of the last child, if there are no children, then the transferred puff
      lastIdAllChildren ::
        DataTypes.Id ->
        DataTypes.Path ->
        CategoryHelpTypes.Level ->
        Map.Map DataTypes.Path (DataTypes.Id, DataTypes.Path) ->
        DataTypes.Id
      lastIdAllChildren i p l id' =
        case Map.lookupGT p id' of
          Nothing -> i
          Just (p', (i', _)) ->
            -- looking for children Maybe (Path, (Id, Path))
            if levelPath p' > l -- found a child
              then lastIdAllChildren i' p' l id'
              else i

-- currentPathName - find path by id in category list
currentPathName ::
  DataTypes.Id ->
  [DataTypes.Category] ->
  Maybe (DataTypes.Path, DataTypes.Name)
currentPathName a cats =
  case filter (\x -> DataTypes.categoryId x == a) cats of
    [x] -> Just (DataTypes.categoryPath x, DataTypes.categoryName x)
    _ -> Nothing

nextPath :: DataTypes.Path -> CategoryHelpTypes.Level -> DataTypes.Path
nextPath x level =
  if level - length (wordsPath x) == 1
    then x ++ ".1"
    else pathFromWords $ take (level - 1) xs ++ [changed] ++ drop level xs
  where
    xs = wordsPath x
    changed = show (1 + read (xs !! (level - 1)) :: Int)

previousPath :: DataTypes.Path -> CategoryHelpTypes.Level -> DataTypes.Path
previousPath x level =
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
    help DataTypes.Category {..} = (categoryPath, (categoryId, []))

toCategoriesEmptyName ::
  Map.Map DataTypes.Path (DataTypes.Id, DataTypes.Path) ->
  [DataTypes.Category]
toCategoriesEmptyName mapCategories = map toCategory $ Map.toList mapCategories
  where
    toCategory ::
      (DataTypes.Path, (DataTypes.Id, DataTypes.Path)) -> DataTypes.Category
    toCategory (path, (idCat, [])) =
      DataTypes.Category
        { categoryId = idCat,
          categoryPath = path,
          categoryName = ""
        }
    toCategory (_, (idCat, path)) =
      DataTypes.Category
        { categoryId = idCat,
          categoryPath = path,
          categoryName = ""
        }

-- | editCategoryRequests list for changes
editCategoryRequests ::
  Map.Map DataTypes.Path (DataTypes.Id, DataTypes.Path) ->
  [CategoryHelpTypes.EditCategory]
editCategoryRequests mapCategories =
  map toEditCategoryRequest $ deleteEmpty $ Map.toList mapCategories
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
      CategoryHelpTypes.EditCategory {permanentId = idCat, newPath = path}

wordsPath :: String -> [String]
wordsPath s =
  case dropWhile (== '.') s of
    "" -> []
    s' -> w : wordsPath s''
      where
        (w, s'') = break (== '.') s'

levelPath :: DataTypes.Path -> Int
levelPath p = length $ wordsPath p

-- | validLogicPathForAddCategory. Path For Add Category does not contain holes in the numbering
validLogicPathForAddCategory ::
  [DataTypes.Category] -> DataTypes.CreateCategoryRequest -> Bool
validLogicPathForAddCategory categories DataTypes.CreateCategoryRequest {DataTypes.path = "1"} =
  Map.member "1" (mapCategory categories)
    || Map.null (mapCategory categories)
    || False
validLogicPathForAddCategory categories DataTypes.CreateCategoryRequest {..}
  | last (wordsPath path) == "1" = Map.member levelUpPath catMap
  | otherwise =
    Map.member (previousPath path (length $ wordsPath path)) catMap
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
    toPair DataTypes.Category {..} = (categoryId, categoryPath)
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
        (CategoryHelpTypes.EditCategory {permanentId = idCat, newPath = newPath}) :
        help2 xs
      | otherwise = help2 xs
