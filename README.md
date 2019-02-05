# DATX02-19-22
Creating Bugs in Haskell

Built with a fresh install of haskell-platform on an up-to-date version of Ubuntu 18.04 LTS:

#### 1. Downloading and unpacking MuCheck from Hackage
Update the package database with `cabal update`, then download and unpack MuCheck: `cabal unpack MuCheck`.

#### 2. Correcting the dependencies for MuCheck
Since MuCheck hasn't been updated in forever, it is no longer compatible with the latest version of `haskell-src-exts`.<br>
To correct this, navigate to the just unpacked MuCheck directory and edit the `MuCheck.cabal` file: <br>

Under the `build-depends:` section, modify the dependency for `haskell-src-exts` from <br>
`haskell-src-exts >=1.13` <br> 
to <br>
`haskell-src-exts < 1.16`. <br>

This should make cabal use a correct, compatible version of `haskell-src-exts` for building.

#### 3. Fixing compilation errors in MuCheck and installing

Due to this version of MuCheck expecting an older version of the `base` package, a small fix is needed on line 123 in `src/Tests/MuCheck/Mutation.hs`

`PatBind` requires 5 arguments but gets only 4; add an underscore to its arguments on line 123,<br>
from: <br>
`isFunctionD n (PatBind _ (PVar (Ident n')) _ _) = n == n'`
to: <br>
`isFunctionD n (PatBind _ (PVar (Ident n')) _ _ _) = n == n'`

Running `cabal install` now should download and install all dependencies, along with MuCheck.

It should now be possible to import `MuCheck` modules and `haskell-src-exts` modules.<br>
Example `Main.hs`:<br>
```
import Test.MuCheck
import Language.Haskell.Exts

main = do
  -- | Prompt the user for a filename and print the AST  
  putStrLn $ "Enter the path to a Haskell source file: "
  input <- getLine
  result <- parseFile input
  print result

  {- Some code involving MuCheck could go here -}

  putStrLn $ "Finished!"
```

Which could then be compiled and run:

![](https://i.imgur.com/qqPqp07.png)<br>
![](https://i.imgur.com/q9oyjKO.png)

### TLDR:
1. `cabal update; cabal unpack MuCheck`
2. Edit `MuCheck.cabal`, ensure: `haskell-src-exts < 1.16`
3. Edit line 123 in `src/Tests/MuCheck/Mutation.hs` to `isFunctionD n (PatBind _ (PVar (Ident n')) _ _ _) = n == n'`
4. `cabal install`
