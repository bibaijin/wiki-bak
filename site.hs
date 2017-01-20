--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid         (mappend)
import qualified Data.Set            as S
import           Hakyll
import           Text.Pandoc.Options


--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ customPandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "content/*" $ do
        route $ setExtension "html"
        compile $ customPandocCompiler
            >>= loadAndApplyTemplate "templates/article.html" postCtx
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            articles <- recentFirst =<< loadAll "content/*"
            let indexCtx =
                    listField "articles" defaultContext (return articles) `mappend`
                    constField "title" "首页" `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration { previewHost = "0.0.0.0" }

customPandocCompiler :: Compiler (Item String)
customPandocCompiler =
  pandocCompilerWith readerOptions defaultHakyllWriterOptions
  where customExtensions = [Ext_east_asian_line_breaks]
        defaultExtensions = readerExtensions defaultHakyllReaderOptions
        newExtensions = foldr S.insert defaultExtensions customExtensions
        readerOptions =
          defaultHakyllReaderOptions { readerExtensions = newExtensions }

postCtx :: Context String
postCtx =
  dateField "date" "%Y 年 %m 月 %d 日" `mappend`
  constField "baseUrl" "https://bibaijin.github.io/wiki" `mappend`
  defaultContext
