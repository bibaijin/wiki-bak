--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.List (intercalate)
import           Data.Monoid         (mappend)
import qualified Data.Set            as S
import           Hakyll
import           System.FilePath     (takeBaseName, takeDirectory)
import           Text.Pandoc.Options


--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do
  match "images/**" $ do
    route   idRoute
    compile copyFileCompiler

  match "js/*" $ do
    route   idRoute
    compile copyFileCompiler

  categories <- buildCategories docsGlob (fromCapture "categories/*.html")

  match (fromList ["about.rst", "contact.markdown"]) $ do
    route   $ setExtension "html"
    compile $
      customPandocCompiler
        >>= loadAndApplyTemplate "templates/base.html" (siteCtxWithCategories categories)
        >>= relativizeUrls

  match docsGlob $ do
    route $ setExtension "html"
    compile $
      customPandocCompiler
        >>= loadAndApplyTemplate "templates/base.html" (siteCtxWithCategories categories)
        >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      articles <- recentFirst =<< loadAll "docs/*"
      let indexCtx =
            listField "articles" siteCtx (return articles) `mappend`
            constField "title" "首页" `mappend`
            -- categoryField "categories" categories `mappend`
            siteCtxWithCategories categories

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/base.html" indexCtx
        >>= relativizeUrls

  match "templates/**" $ compile templateCompiler


--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration { previewHost = "0.0.0.0" }

loadWriteOptions :: IO WriterOptions
loadWriteOptions = do
  pandocTemplate <- readFile "src/pandoc-template.html"
  return defaultHakyllWriterOptions { writerTableOfContents = True
                                     , writerTemplate = Just pandocTemplate
                                     }

customPandocCompiler :: Compiler (Item String)
customPandocCompiler = do
  writerOptions <- unsafeCompiler loadWriteOptions
  pandocCompilerWith readerOptions writerOptions
  where customExtensions = [Ext_east_asian_line_breaks]
        defaultExtensions = readerExtensions defaultHakyllReaderOptions
        newExtensions = foldr S.insert defaultExtensions customExtensions
        readerOptions =
          defaultHakyllReaderOptions { readerExtensions = newExtensions }

postCtx :: Context String
postCtx =
  dateField "date" "%Y 年 %m 月 %d 日" `mappend`
  siteCtx

siteCtx :: Context String
siteCtx =
  field "currentCategory"
    (return . takeBaseName . takeDirectory . toFilePath . itemIdentifier) `mappend`
  defaultContext

-- categoryCtx :: Context (String, [(String, String)])
categoryCtx =
  field "category" (return . fst . itemBody) `mappend`
  listFieldWith "articles"
    (field "title" (return . takeBaseName . toFilePath . itemBody) `mappend`
      field "url" (fmap (maybe "" toUrl) . getRoute . itemBody))
    (mapM makeItem . snd . itemBody)

siteCtxWithCategories :: Tags -> Context String
siteCtxWithCategories categories =
  listField "categories" categoryCtx items `mappend`
  siteCtx
  where
    tagsMapToItems = makeItem
    items = mapM tagsMapToItems (tagsMap categories)

docsGlob :: Pattern
docsGlob = "docs/**.md"
