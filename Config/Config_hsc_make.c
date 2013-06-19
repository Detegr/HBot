#include "/usr/lib/ghc-7.6.3/template-hsc.h"
#line 5 "Config.hs"
#define hsc_alignment(t ) hsc_printf ( "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__));

int main (int argc, char *argv [])
{
    hsc_line (1, "Config.hs");
    hsc_fputs ("import Foreign.C\n"
           "", hsc_stdout());
    hsc_line (2, "Config.hs");
    hsc_fputs ("import Foreign.Ptr\n"
           "import Foreign.Marshal.Alloc\n"
           "\n"
           "", hsc_stdout());
    hsc_fputs ("\n"
           "", hsc_stdout());
    hsc_line (6, "Config.hs");
    hsc_fputs ("\n"
           "data ConfigItem = ConfigItem\n"
           "  {\n"
           "      key :: CString,\n"
           "      val :: CString\n"
           "  }\n"
           "\n"
           "data ConfigSection = ConfigSection\n"
           "  {\n"
           "      name  :: CString,\n"
           "      items :: CUInt,\n"
           "      sectionSize  :: CUInt,\n"
           "      item  :: [Ptr ConfigItem]\n"
           "  }\n"
           "type ConfigSectionPtr = ConfigSection\n"
           "\n"
           "data Config = Config \n"
           "  {\n"
           "      sections :: CUInt,\n"
           "      size     :: CUInt,\n"
           "      section  :: [Ptr ConfigSection]\n"
           "  }\n"
           "\n"
           "instance Storable Config where\n"
           "  alignment _ = ", hsc_stdout());
#line 30 "Config.hs"
    hsc_alignment (Config);
    hsc_fputs ("\n"
           "", hsc_stdout());
    hsc_line (31, "Config.hs");
    hsc_fputs ("  sizeOf _ = ", hsc_stdout());
#line 31 "Config.hs"
    hsc_size (Config);
    hsc_fputs ("\n"
           "", hsc_stdout());
    hsc_line (32, "Config.hs");
    hsc_fputs ("  peek ptr = do\n"
           "    sections <- ", hsc_stdout());
#line 33 "Config.hs"
    hsc_peek (Config, sections);
    hsc_fputs (" ptr\n"
           "", hsc_stdout());
    hsc_line (34, "Config.hs");
    hsc_fputs ("    size     <- ", hsc_stdout());
#line 34 "Config.hs"
    hsc_peek (Config, size);
    hsc_fputs ("     ptr\n"
           "", hsc_stdout());
    hsc_line (35, "Config.hs");
    hsc_fputs ("    section  <- ", hsc_stdout());
#line 35 "Config.hs"
    hsc_peek (Config, section);
    hsc_fputs ("  ptr\n"
           "", hsc_stdout());
    hsc_line (36, "Config.hs");
    hsc_fputs ("\n"
           "foreign import ccall unsafe \"config_init\" initConfig :: Ptr Config -> IO()\n"
           "\n"
           "main = alloca $ \\p -> initConfig p\n"
           "", hsc_stdout());
    return 0;
}
