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
      citems          :: Ptr (Ptr CConfigItem)
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

data CConfig = CConfig 
  {
      sectionCount      :: CUInt,
      allocatedSections :: CUInt,
      sections          :: Ptr (Ptr CConfigSection)
  } deriving Show

instance Storable CConfig where
  alignment _ = 8
  sizeOf _ = 16
  peek ptr = do
    a <- peekByteOff ptr 0
    b <- peekByteOff ptr 4
    c <- peekByteOff ptr 8
    return $ CConfig a b c
  poke ptr (CConfig a b c) = do
    pokeByteOff ptr 0 a
    pokeByteOff ptr 4 b
    pokeByteOff ptr 8 c

foreign import ccall unsafe "config_init" initConfig :: Ptr CConfig -> IO()
foreign import ccall unsafe "config_load" loadConfig :: Ptr CConfig -> CString -> IO()
foreign import ccall unsafe "config_find_section" findSection :: Ptr CConfig -> CString -> IO(Ptr CConfigSection)
foreign import ccall unsafe "config_add" configAdd :: Ptr CConfig -> CString -> CString -> CString -> IO()
foreign import ccall unsafe "config_free" freeConfig :: Ptr CConfig -> IO()
foreign import ccall unsafe "config_save" saveConfigInternal :: Ptr CConfig -> CString -> IO()

withConfig f = alloca $ \p -> initConfig p >> f p >> freeConfig p
withLoadedConfig s f = alloca $ \p -> do
  withCString s (\cstr -> loadConfig p cstr)
  f p
  freeConfig p

withSection c needle f = do
  withCString needle $ \n -> do
    section <- findSection c n
    if section == nullPtr then return ()
    else do
      s <- peek section
      name <- peekCString (cname s)
      itemptrs <- peekArray (fromIntegral . citemsCount $ s) (citems s)
      items <- mapM (peekvalues <=< peek) itemptrs
      f (name, items)
  where peekvalues x = peekCString (key x) >>= \k -> peekCString (val x) >>= \v -> return $ (k,v)

withNullableCString s f =
  case length s of
    0 -> f nullPtr
    _ -> withCString s $ \cs -> f cs

addToConfig c sect key val =
  withNullableCString sect $ \csect ->
    withNullableCString key $ \ckey ->
      withNullableCString val $ \cval ->
        configAdd c csect ckey cval

saveConfig c filename = do
  withCString filename $ \f -> do
    saveConfigInternal c f

main = do
  withLoadedConfig "test.conf" $ \c -> do
    addToConfig c "Account" "foo" "bar"
    withSection c "Account" $ \s -> putStrLn $ show s
