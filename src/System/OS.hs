module System.OS where

import qualified System.Info

data OS
  = Linux
  | Windows
  | OSX
  | Solaris
  deriving (Eq, Show)

os :: Either String OS
os =
  case System.Info.os of
    "windows" -> Right Windows
    "mingw32" -> Right Windows
    "win32" -> Right Windows
    "cygwin32" -> Right Windows
    "osx" -> Right OSX
    "darwin" -> Right OSX
    "linux" -> Right Linux
    "solaris" -> Right Solaris
    "solaris2" -> Right Solaris
    a -> Left a

isLinux :: OS -> Bool
isLinux Linux = True
isLinux _ = False

isWindows :: OS -> Bool
isWindows Windows = True
isWindows _ = False

isOSX :: OS -> Bool
isOSX OSX = True
isOSX _ = False

isSolaris :: OS -> Bool
isSolaris Solaris = True
isSolaris _ = False

-- | 'Linux', 'OSX', and 'Solaris' are Unix systems
isUnix :: OS -> Bool
isUnix Linux = True
isUnix OSX = True
isUnix Solaris = True
isUnix _ = False