{-# LANGUAGE CPP, FlexibleInstances #-}

-- |
-- Module    : Z3.Opts
-- Copyright : (c) Iago Abal, 2013-2015
--             (c) David Castro, 2013
-- License   : BSD3
-- Maintainer: Iago Abal <mail@iagoabal.eu>,
--             David Castro <david.castro.dcp@gmail.com>
--
-- Configuring Z3.
--
-- Z3 has plenty of configuration options and these had varied quite
-- a lot from Z3 3.x to Z3 4.x. We decided to keep things simple.
--
-- Configurations are essentially sets of 'String' pairs, assigning
-- values to parameters. We just provide a thin layer of abstraction
-- on top of this.
--
-- For instance, @opt \"proof\" True@ creates a configuration where
-- proof generation is enabled. The first argument of 'opt' is the
-- option name, the second is the option value. The same configuration
-- is created by @opt \"proof\" \"true\"@. We do not check option names,
-- and we do not assign types to options ---any 'String' is accepted
-- as a value. The 'OptValue' class is a specialization of 'Show' to
-- convert Haskell values into a proper 'String' representation for Z3.
--
-- Configurations can be combined with the '+?' operator, for instance,
-- @opt \"proof\" True +? opt \"model\" True@ is a configuration with both
-- proof and model generation enabled. Configurations are 'Monoid's,
-- and '+?' is just an alias for 'mappend'.
--
-- Configurations are set by 'setOpts' if you are using "Z3.Base", or
-- passing the configuration object (of type 'Opts') to a runner if you
-- are using the "Z3.Monad" interface.
--

module Z3.Opts
  ( -- * Z3 configuration
    Opts
  , setOpts
  , stdOpts
  , (+?)
    -- * Z3 options
  , opt
  , OptValue
  )
  where

import qualified Z3.Base as Base

import           Data.Fixed  ( Fixed )
import qualified Data.Fixed as Fixed
import           Data.Monoid ( Monoid(..) )

#if MIN_VERSION_base(4,9,0)
import           Data.Semigroup ( Semigroup (..) )
#endif

---------------------------------------------------------------------
-- Configuration

-- | Z3 configuration.
newtype Opts = Opts [Opt]

#if MIN_VERSION_base(4,9,0)
instance Semigroup Opts where
  Opts ps1 <> Opts ps2 = Opts (ps1 ++ ps2)
#endif

instance Monoid Opts where
  mempty = Opts []
#if MIN_VERSION_base(4,9,0)
  mappend = (<>)
#else
  Opts ps1 `mappend` Opts ps2 = Opts (ps1 ++ ps2)
#endif

singleton :: Opt -> Opts
singleton o = Opts [o]

-- | Default configuration.
stdOpts :: Opts
stdOpts = mempty

-- | Append configurations.
(+?) :: Opts -> Opts -> Opts
(+?) = mappend

-- | Set a configuration option.
opt :: OptValue val => String -> val -> Opts
opt oid val = singleton $ option oid val

-- | Set configuration.
--
-- If you are using the 'Z3.Monad' interface, you don't need to
-- call this function directly, just pass your 'Opts' to /evalZ3*/
-- functions.
setOpts :: Base.Config -> Opts -> IO ()
setOpts baseCfg (Opts params) = mapM_ (setOpt baseCfg) params

-------------------------------------------------
-- Options

-- | Pair option-value.
data Opt = Opt String  -- id
               String  -- value

-- | Set an option.
setOpt :: Base.Config -> Opt -> IO ()
setOpt baseCfg (Opt oid val) = Base.setParamValue baseCfg oid val

-- | Values for Z3 options.
--
-- Any 'OptValue' type can be passed to a Z3 option (and values will be
-- converted into an appropriate 'String').
--
-- /Since 0.4/ the 'Double' instance has been replaced by a new 'Fixed'
-- instance.
class OptValue val where
  option :: String -> val -> Opt

instance OptValue Bool where
  option oid = Opt oid . boolVal

instance OptValue Int where
  option oid = Opt oid . show

instance OptValue Integer where
  option oid = Opt oid . show

instance Fixed.HasResolution a => OptValue (Fixed a) where
  option oid = Opt oid . Fixed.showFixed True

instance OptValue [Char] where
  option = Opt

-------------------------------------------------
-- Utils

boolVal :: Bool -> String
boolVal True  = "true"
boolVal False = "false"
