{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign.C
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Control.Monad
import Data.String.Utils

data CConfigItem = CConfigItem
  {
      key :: CString,
      val :: CString
  } deriving Show

type ConfigItem = (String,String)

instance Storable CConfigItem where
  alignment _ = 8
  sizeOf _ = 16
  peek ptr = do
    a <- peekByteOff ptr 0
    b <- peekByteOff ptr 8
    return $ CConfigItem a b
  poke ptr (CConfigItem a b)= do
    pokeByteOff ptr 0 a
    pokeByteOff ptr 8 b

data CConfigSection = CConfigSection
  {
      cname           :: CString,
      citemsCount     :: CUInt,
      callocatedItems :: CUInt,
      citems          :: Ptr CConfigItem
  } deriving Show

type ConfigSection = (String, [ConfigItem])

instance Storable CConfigSection where
  alignment _ = 8
  sizeOf _ = 24
  peek ptr = do
    a <- peekByteOff ptr 0
    b <- peekByteOff ptr 8
    c <- peekByteOff ptr 12
    d <- peekByteOff ptr 16
    return $ CConfigSection a b c d
  poke ptr (CConfigSection a b c d) = do
    pokeByteOff ptr 0  a
    pokeByteOff ptr 8  b
    pokeByteOff ptr 12 c
    pokeByteOff ptr 16 d

data Config = Config 
  {
      sectionCount      :: CUInt,
      allocatedSections :: CUInt,
      sections          :: Ptr CConfigSection
  } deriving Show

instance Storable Config where
  alignment _ = 8
  sizeOf _ = 16
  peek ptr = do
    a <- peekByteOff ptr 0
    b <- peekByteOff ptr 4
    c <- peekByteOff ptr 8
    return $ Config a b c
  poke ptr (Config a b c) = do
    pokeByteOff ptr 0 a
    pokeByteOff ptr 4 b
    pokeByteOff ptr 8 c

foreign import ccall unsafe "config_init" initConfig :: Ptr Config -> IO()
foreign import ccall unsafe "config_load" loadConfig :: Ptr Config -> CString -> IO()
foreign import ccall unsafe "config_find_section" findSection :: Ptr Config -> CString -> IO(Ptr CConfigSection)

withConfig f = alloca $ \p -> initConfig p >> f p
withLoadedConfig s f = alloca $ \p -> do
  withCString s (\cstr -> loadConfig p cstr)
  f p

withSection c needle f = do
  withCString needle $ \n -> do
    section <- findSection c n
    if section == nullPtr then return ()
    else do
      s <- peek section
      name <- peekCString (cname s)
      arr <- peekArray (fromIntegral . citemsCount $ s) (citems s)
      --arr' <- mapM toHs arr
      f (name, arr)
  where toHs x = peekCString (key x) >>= \k -> peekCString (val x) >>= \v -> return $ (k,v)

main = do
  withLoadedConfig "test.conf" $ \c -> do
    withSection c "Account" $ \s -> do
      putStrLn $ show s
