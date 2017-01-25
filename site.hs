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

    match "dist/*" $ do
        route   idRoute
        compile copyFileCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ customPandocCompiler
            >>= loadAndApplyTemplate "templates/base.html" siteCtx
            >>= relativizeUrls

    match "content/*" $ do
        route $ setExtension "html"
        compile $ customPandocCompiler
            >>= loadAndApplyTemplate "templates/base.html" siteCtx
            >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            articles <- recentFirst =<< loadAll "content/*"
            let indexCtx =
                    listField "articles" siteCtx (return articles) `mappend`
                    constField "title" "首页" `mappend`
                    siteCtx

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/base.html" indexCtx
                >>= relativizeUrls

    match "templates/**" $ compile templateCompiler


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
  siteCtx

siteCtx :: Context String
siteCtx =
  defaultContext
