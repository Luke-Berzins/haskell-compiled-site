module Main where

import Hakyll

main :: IO ()
main = hakyll $ do
    -- Copy static files
    match (fromGlob "content/images/*") $ do
        route   (gsubRoute "content/" (const ""))
        compile copyFileCompiler

    -- Compile posts
    match (fromGlob "content/posts/*") $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate (fromFilePath "templates/post.html")    postCtx
            >>= loadAndApplyTemplate (fromFilePath "templates/default.html") postCtx
            >>= relativizeUrls

    -- Render index page
    match (fromGlob "content/index.html") $ do
        route (constRoute "index.html")
        compile $ do
            posts <- recentFirst =<< loadAll (fromGlob "content/posts/*")
            let indexCtx =
                    listField "posts" postCtx (return posts) <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate (fromFilePath "templates/default.html") indexCtx
                >>= relativizeUrls

    -- Compile templates
    match (fromGlob "templates/*") $ compile templateBodyCompiler

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    defaultContext