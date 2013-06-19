import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Alloc

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

data ConfigItem = ConfigItem
  {
      key :: CString,
      val :: CString
  }

data ConfigSection = ConfigSection
  {
      name  :: CString,
      items :: CUInt,
      sectionSize  :: CUInt,
      item  :: [Ptr ConfigItem]
  }
type ConfigSectionPtr = ConfigSection

data Config = Config 
  {
      sections :: CUInt,
      size     :: CUInt,
      section  :: [Ptr ConfigSection]
  }

instance Storable Config where
  alignment _ = #{alignment Config}
  sizeOf _ = #{size Config}
  peek ptr = do
    sections <- #{peek Config, sections} ptr
    size     <- #{peek Config, size}     ptr
    section  <- #{peek Config, section}  ptr

foreign import ccall unsafe "config_init" initConfig :: Ptr Config -> IO()

main = alloca $ \p -> initConfig p
