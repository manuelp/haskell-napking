module TumblrRipper where

import Control.Monad (mapM)

type URL = String
data Post = Post [URL]
type BlogName = String
data Blog = Blog String Integer
data Page = Page Integer Integer

--
-- Basic primitives
--

fetchPage :: BlogName -> Page -> [Post]
fetchPage = undefined

download :: URL -> IO File
download = undefined

--
-- Synchronization
--

class BlogData a where
    addPost :: a -> Post -> a
    hasBeenDownloaded :: a -> URL -> a
    toDownload :: a -> [URL]

data InMemoryData = InMemoryData {mem.urls :: [URL],
                                  mem.downloaded :: [URL],
                                  mem.posts :: [Post]}

instance BlogData InMemoryData where
    addPost d p@(Post urls) = InMemoryData 
                              ((mem.urls d) ++ urls)
                              (mem.downloaded d)
                              ((mem.posts) ++ [p])
    hasBeenDownloaded d x = InMemoryData 
                            (mem.urls d)
                            ((mem.downloaded d) ++ [x])
                            (mem.posts)
    toDownload d = filter ((flip notElem) (mem.downloaded d)) (mem.urls d)

-- Step 1: Synchronize links
-- Here is all the logic for an efficient synchronization of the URLs
synchronize :: BlogData a => a -> BlogName -> a
synchronize = undefined

-- Step 2: Download missing (new or not previously downloaded)
downloadMissing :: BlogData a => a -> IO [File]
downloadMissing d = mapM download (toDownload a)
