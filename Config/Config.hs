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
      key :: String,
      val :: String
  }

instance Show ConfigItem where
  show ci = (key ci) ++ " - " ++ (val ci)

instance Storable ConfigItem where
  alignment _ = 8
  sizeOf _ = 16
  peek ptr = do
    a <- peekCString (plusPtr ptr 0)
    b <- peekCString (plusPtr ptr 4)
    return $ ConfigItem a b

data ConfigSection = ConfigSection
  {
      name           :: String,
      itemsCount     :: Integer,
      allocatedItems :: Integer,
      items          :: [ConfigItem]
  }

instance Show ConfigSection where
  show (ConfigSection n ic ai i) = "[" ++ n ++ "]: ItemCount: " ++ (show ic) ++ "\n"

instance Storable ConfigSection where
  alignment _ = 8
  sizeOf _ = 16
  peek ptr = do
    a <- peekCString $ plusPtr ptr 0
    b <- (peekByteOff ptr 4) :: IO CUInt
    c <- (peekByteOff ptr 8) :: IO CUInt
    d <- peekArray (fromIntegral b) (plusPtr ptr 12)
    return $ ConfigSection a (fromIntegral b) (fromIntegral c) d

data Config = Config 
  {
      sectionCount      :: Integer,
      allocatedSections :: Integer,
      sections          :: [ConfigSection]
  }

instance Show Config where
  show (Config sc as s) = ("Config with " ++ (show sc) ++ " sections\n") ++ (Data.String.Utils.join [] (map show s))

instance Storable Config where
  alignment _ = 4
  sizeOf _ = 16
  peek ptr = do
    a <- (peekByteOff ptr 0) :: IO CUInt
    b <- (peekByteOff ptr 4) :: IO CUInt
    c <- peekArray (fromIntegral a) (plusPtr ptr 8)
    return $ Config (fromIntegral a) (fromIntegral b) c

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
    section <- findSection ptr needle
    if section == nullPtr then f Nothing
                          else f (Just . peek $ section)

main = do
  withLoadedConfig "test.conf" $ \c -> do
    putStrLn (show c)
