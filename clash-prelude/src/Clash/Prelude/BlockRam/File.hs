{-|
Copyright  :  (C) 2015-2016, University of Twente,
                  2019     , Myrtle Software Ltd,
                  2017     , Google Inc.,
                  2021-2022, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

= Initializing a block RAM with a data file #usingramfiles#

Block RAM primitives that can be initialized with a data file. The BNF grammar
for this data file is simple:

@
FILE = LINE+
LINE = BIT+
BIT  = '0'
     | '1'
@

Consecutive @LINE@s correspond to consecutive memory addresses starting at @0@.
For example, a data file @memory.bin@ containing the 9-bit unsigned numbers
@7@ to @13@ looks like:

@
000000111
000001000
000001001
000001010
000001011
000001100
000001101
@

Such a file can be produced with 'E.memFile':

@
writeFile "memory.bin" (memFile Nothing [7 :: Unsigned 9 .. 13])
@

We can instantiate a block RAM using the contents of the file above like so:

@
f :: (HiddenClock dom, HiddenEnable dom)
  => Signal dom (Unsigned 3)
  -> Signal dom (Unsigned 9)
f rd = 'Clash.Class.BitPack.unpack' '<$>' 'blockRamFile' d7 \"memory.bin\" rd (pure Nothing)
@

In the example above, we basically treat the block RAM as a synchronous ROM.
We can see that it works as expected:

@
__>>> import qualified Data.List as L__
__>>> L.tail $ sampleN 4 $ f (fromList [3..5])__
[10,11,12]
@

However, we can also interpret the same data as a tuple of a 6-bit unsigned
number, and a 3-bit signed number:

@
g :: (HiddenClock dom, HiddenEnable dom)
   => Signal dom (Unsigned 3)
   -> Signal dom (Unsigned 6,Signed 3)
g clk rd = 'Clash.Class.BitPack.unpack' '<$>' 'blockRamFile' d7 \"memory.bin\" rd (pure Nothing)
@

And then we would see:

@
__>>> import qualified Data.List as L__
__>>> L.tail $ sampleN 4 $ g (fromList [3..5])__
[(1,2),(1,3)(1,-4)]
@

-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

{-# LANGUAGE Unsafe #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Prelude.BlockRam.File
  ( -- * Block RAM synchronized to an arbitrary clock
    blockRamFile
  , blockRamFilePow2
    -- * Producing files
  , E.memFile
  )
where

import GHC.TypeLits                           (KnownNat)
import GHC.Stack                              (HasCallStack, withFrozenCallStack)


import qualified Clash.Explicit.BlockRam.File as E
import           Clash.Promoted.Nat           (SNat)
import           Clash.Signal
  (HiddenClock, HiddenEnable, Signal, hideClock, hideEnable)
import           Clash.Sized.BitVector        (BitVector)
import           Clash.Sized.Unsigned         (Unsigned)
import           Clash.XException             (NFDataX)

-- | Create a block RAM with space for 2^@n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is /undefined/, reading it will throw an
-- 'Clash.XException.XException'
-- * __NB__: This function might not work for specific combinations of
-- code-generation backends and hardware targets. Please check the support table
-- below:
--
-- +----------------+----------+----------+---------------+
-- |                | VHDL     | Verilog  | SystemVerilog |
-- +================+==========+==========+===============+
-- | Altera/Quartus | Broken   | Works    | Works         |
-- +----------------+----------+----------+---------------+
-- | Xilinx/ISE     | Works    | Works    | Works         |
-- +----------------+----------+----------+---------------+
-- | ASIC           | Untested | Untested | Untested      |
-- +----------------+----------+----------+---------------+
--
-- === See also:
--
-- * See "Clash.Prelude.BlockRam#usingrams" for more information on how to use a
-- block RAM.
-- * Use the adapter 'Clash.Prelude.BlockRam.readNew' for obtaining write-before-read semantics like this: @'Clash.Prelude.BlockRam.readNew' ('blockRamFilePow2' file) rd wrM@.
-- * See "Clash.Prelude.BlockRam.File#usingramfiles" for more information on how
-- to instantiate a block RAM with the contents of a data file.
-- * See "Clash.Sized.Fixed#creatingdatafiles" for ideas on how to create your
-- own data files.
blockRamFilePow2
  :: forall dom  n m
   . KnownNat m
  => KnownNat n
  => HiddenClock dom
  => HiddenEnable dom
  => HasCallStack
  => FilePath
  -- ^ File describing the initial content of the BRAM
  -> Signal dom (Unsigned n)
  -- ^ Read address @r@
  -> Signal dom (Maybe (Unsigned n, BitVector m))
  -- ^ (write address @w@, value to write)
  -> Signal dom (BitVector m)
  -- ^ Value of the BRAM at address @r@ from the previous clock cycle
blockRamFilePow2 = \fp rd wrM -> withFrozenCallStack
  (hideEnable (hideClock E.blockRamFilePow2) fp rd wrM)
{-# INLINE blockRamFilePow2 #-}

-- | Create a block RAM with space for @n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is /undefined/, reading it will throw an
-- 'Clash.XException.XException'
-- * __NB__: This function might not work for specific combinations of
-- code-generation backends and hardware targets. Please check the support table
-- below:
--
-- +----------------+----------+----------+---------------+
-- |                | VHDL     | Verilog  | SystemVerilog |
-- +================+==========+==========+===============+
-- | Altera/Quartus | Broken   | Works    | Works         |
-- +----------------+----------+----------+---------------+
-- | Xilinx/ISE     | Works    | Works    | Works         |
-- +----------------+----------+----------+---------------+
-- | ASIC           | Untested | Untested | Untested      |
-- +----------------+----------+----------+---------------+
--
-- === See also:
--
-- * See "Clash.Prelude.BlockRam#usingrams" for more information on how to use a
-- block RAM.
-- * Use the adapter 'Clash.Prelude.BlockRam.readNew' for obtaining write-before-read semantics like this: @'Clash.Prelude.BlockRam.readNew' ('blockRamFile' size file) rd wrM@.
-- * See "Clash.Prelude.BlockRam.File#usingramfiles" for more information on how
-- to instantiate a block RAM with the contents of a data file.
-- * See "Clash.Sized.Fixed#creatingdatafiles" for ideas on how to create your
-- own data files.
blockRamFile
  :: KnownNat m
  => Enum addr
  => NFDataX addr
  => HiddenClock dom
  => HiddenEnable dom
  => HasCallStack
  => SNat n
  -- ^ Size of the BRAM
  -> FilePath
  -- ^ File describing the initial content of the BRAM
  -> Signal dom addr
  -- ^ Read address @r@
  -> Signal dom (Maybe (addr, BitVector m))
  -- ^ (write address @w@, value to write)
  -> Signal dom (BitVector m)
  -- ^ Value of the BRAM at address @r@ from the previous clock cycle
blockRamFile = \sz fp rd wrM -> withFrozenCallStack
  (hideEnable (hideClock E.blockRamFile) sz fp rd wrM)
{-# INLINE blockRamFile #-}
