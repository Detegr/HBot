{-# LANGUAGE ForeignFunctionInterface #-}

module Config (withConfig,
               withLoadedConfig,
               addItem,
               saveConfig,
               saveConfigAs,
               getSection,
               getConfig,
               getItem,
               ConfigSection(..),
               ConfigItem,
               Config,
               getSectionKeys,
               ConfigM) where

import Foreign.C
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Control.Monad
import Data.String.Utils
import Control.Monad.Reader

type ConfigM a = ReaderT (Ptr CConfig, String) IO a

data CConfigItem = CConfigItem
  {
      key :: CString,
      val :: CString
  } deriving Show

type ConfigItem = (String, Maybe String)

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

data ConfigSection = ConfigSection {
                     sectionName  :: String,
                     sectionItems :: [ConfigItem]
                     }

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

type Config = [ConfigSection]

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
foreign import ccall unsafe "config_find_item" findItem :: Ptr CConfig -> CString -> CString -> IO(Ptr CConfigItem)
foreign import ccall unsafe "config_add" configAdd :: Ptr CConfig -> CString -> CString -> CString -> IO()
foreign import ccall unsafe "config_free" freeConfig :: Ptr CConfig -> IO()
foreign import ccall unsafe "config_save" saveConfigInternal :: Ptr CConfig -> CString -> IO()

withConfig :: (Ptr CConfig -> IO a) -> IO()
withConfig f = alloca $ \p -> initConfig p >> f p >> freeConfig p

withLoadedConfig :: String -> (ReaderT (Ptr CConfig, String) IO a) -> IO a
withLoadedConfig s f = alloca $ \p -> do
  withCString s (\cstr -> loadConfig p cstr)
  val <- runReaderT f (p,s)
  freeConfig p
  return val

cConfigItemToItem :: CConfigItem -> IO ConfigItem
cConfigItemToItem i = do
  k <- peekCString (key i)
  case val i == nullPtr of
    True  -> return $ (k, Nothing)
    False -> do
      v <- peekCString (val i)
      return $ (k, Just v)

cConfigSectionToConfigSection :: CConfigSection -> IO ConfigSection
cConfigSectionToConfigSection s = do
  name <- peekCString (cname s)
  itemptrs <- peekArray (fromIntegral . citemsCount $ s) (citems s)
  items <- mapM (cConfigItemToItem <=< peek) itemptrs
  return $ ConfigSection name items

getConfig :: ReaderT (Ptr CConfig, String) IO [ConfigSection]
getConfig = do
  (c,_) <- ask
  liftIO $ do
    arrptrs <- peek c >>= \pc -> peekArray (fromIntegral . sectionCount $ pc) $ sections pc
    mapM (cConfigSectionToConfigSection <=< peek) arrptrs

getSection :: String -> ReaderT (Ptr CConfig, String) IO (Maybe ConfigSection)
getSection needle = do
  (c,_) <- ask
  section <- liftIO $ withCString needle $ \n -> findSection c n
  if section == nullPtr
    then return Nothing
    else liftIO $ peek section >>= cConfigSectionToConfigSection >>= return . Just

getSectionKeys :: String -> ReaderT (Ptr CConfig, String) IO ([String])
getSectionKeys sect = do
  s <- getSection sect
  case s of
    Just s -> return $ map fst (sectionItems s)
    Nothing -> return []

getItem :: String -> Maybe String -> ReaderT (Ptr CConfig, String) IO (Maybe ConfigItem)
getItem i s = do
  (c,_) <- ask
  item <- liftIO $ withCString i $ \ci ->
          case s of
            Just s  -> withNullableCString s  $ \si -> findItem c ci si
            Nothing -> withNullableCString "" $ \si -> findItem c ci si
  if item == nullPtr
    then return Nothing
    else liftIO $ peek item >>= cConfigItemToItem >>= return . Just

withNullableCString :: String -> (CString -> IO a) -> IO a
withNullableCString s f =
  case length s of
    0 -> f nullPtr
    _ -> withCString s $ \cs -> f cs

addItem :: String -> String -> String -> ReaderT (Ptr CConfig, String) IO()
addItem sect key val = do
  (c,_) <- ask
  liftIO $ withNullableCString sect $ \csect ->
    withNullableCString key $ \ckey ->
      withNullableCString val $ \cval ->
        configAdd c csect ckey cval

saveConfig :: ReaderT (Ptr CConfig, String) IO()
saveConfig = do
  (c,filename) <- ask
  liftIO $ withCString filename $ \f -> do
    saveConfigInternal c f

saveConfigAs :: String -> ReaderT (Ptr CConfig, String) IO()
saveConfigAs filename = do
  (c,_) <- ask
  liftIO $ withCString filename $ \f -> do
    saveConfigInternal c f
