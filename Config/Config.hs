{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign.C
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Control.Monad
import Data.String.Utils

data ConfigItem = ConfigItem
  {
      key :: CString,
      val :: CString
  } deriving Show

instance Storable ConfigItem where
  alignment _ = 8
  sizeOf _ = 8
  peek ptr = do
    a <- peekByteOff ptr 0
    b <- peekByteOff ptr 4
    return $ ConfigItem a b
  poke ptr (ConfigItem a b)= do
    pokeByteOff ptr 0 a
    pokeByteOff ptr 4 b

data ConfigSection = ConfigSection
  {
      name           :: CString,
      itemsCount     :: CUInt,
      allocatedItems :: CUInt,
      items          :: Ptr ConfigItem
  } deriving Show

instance Storable ConfigSection where
  alignment _ = 8
  sizeOf _ = 16
  peek ptr = do
    a <- peekByteOff ptr 0
    b <- peekByteOff ptr 4
    c <- peekByteOff ptr 8
    d <- peekByteOff ptr 12
    return $ ConfigSection a b c d
  poke ptr (ConfigSection a b c d) = do
    pokeByteOff ptr 0  a
    pokeByteOff ptr 4  b
    pokeByteOff ptr 8  c
    pokeByteOff ptr 12 d

data Config = Config 
  {
      sectionCount      :: CUInt,
      allocatedSections :: CUInt,
      sections          :: Ptr ConfigSection
  } deriving Show

instance Storable Config where
  alignment _ = 4
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
foreign import ccall unsafe "config_find_section" findSection :: Ptr Config -> CString -> IO(Ptr ConfigSection)

withConfig f = alloca $ \p -> initConfig p >> f p
withLoadedConfig s f = alloca $ \p -> do
  withCString s (\cstr -> loadConfig p cstr)
  conf <- peek p
  f conf

withSection c needle f = do
  alloca $ \ptr -> do
    poke ptr c
    withCString needle $ \n -> do
      section <- findSection ptr n
      if section == nullPtr then f Nothing
                            else peek section >>= \ps -> f (Just ps)

main = do
  withLoadedConfig "test.conf" $ \c -> do
    withSection c "Account" $ \s -> do
      case s of
        (Just s) -> peekCString (name s) >>= \str -> putStrLn str
        _        -> putStrLn "Section not found"
