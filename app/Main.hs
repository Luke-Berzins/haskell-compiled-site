module Main where

import Hakyll

main :: IO ()
main = hakyll $ do
    -- Copy static files
    match (fromGlob "content/images/*") $ do
        route   (gsubRoute "content/" (const ""))
        compile copyFileCompiler

    -- Build up tags (fixed pattern matching)
    tags <- buildTags (fromGlob "content/posts/*") (fromCapture (fromGlob "tags/*.html"))

    -- Compile posts (modified to use tags)
    match (fromGlob "content/posts/*") $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate (fromFilePath "templates/post.html")    (postCtxWithTags tags)
            >>= loadAndApplyTemplate (fromFilePath "templates/default.html") (postCtxWithTags tags)
            >>= relativizeUrls

    -- Create tag pages (new!)
    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title <>
                     listField "posts" (postCtxWithTags tags) (return posts) <>
                     defaultContext

            makeItem ""
                >>= loadAndApplyTemplate (fromFilePath "templates/tag.html") ctx
                >>= loadAndApplyTemplate (fromFilePath "templates/default.html") ctx
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

    create [fromFilePath "tags/index.html"] $ do
        route $ constRoute "tags/index.html"
        compile $ do
            let tagsList = map (\(tag, ids) -> (tag, length ids)) $ tagsMap tags
                ctx = listField "tags" 
                    (field "tag" (\item -> return $ itemBody item) <>
                     field "url" (\item -> return $ "/tags/" ++ (itemBody item) ++ ".html") <>
                     field "count" (\item -> return . show . snd . head $ 
                         filter ((== itemBody item) . fst) tagsList))
                    (mapM makeItem $ map fst tagsList) <>
                     constField "title" "All Tags" <>
                     defaultContext
            makeItem ""
                >>= loadAndApplyTemplate (fromFilePath "templates/tags.html") ctx
                >>= loadAndApplyTemplate (fromFilePath "templates/default.html") ctx
                >>= relativizeUrls
    -- Compile templates
    match (fromGlob "templates/*") $ compile templateBodyCompiler

-- Replace this entire postCtx definition:
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    defaultContext

-- With this new one:
postCtxWithTags :: Tags -> Context String
postCtxWithTags tags =
    tagsField "tags" tags <>
    dateField "date" "%B %e, %Y" <>
    defaultContext