-- | Module : StateModel
--
-- The `StateModel` type is a pure abstract representation of the application's
-- behavior.  This behavior essentially just comes down to pulling in traffic
-- from certain interfaces and sending it out of some other interfaces, so we
-- model our program's behavior as a set of endpoints, each tagged as an input
-- or output.
-- 
-- We can pull in some target `StateModel` from a config file, compare it to our
-- current state, and generate the exact sequence of `Instruction`s needed to
-- arrive in our new target state.
module Hyperpipe.StateModel where

import Data.List ((\\))

import Hyperpipe.EthFrame (EthFrame, VLANTag)

-- | Direction of traffic flowing over an interface
data FlowDir = Input | Output
  deriving (Eq, Show)

-- | Abstract representation of a function over an `EthFrame` (for now this just
-- supports adding or removing VLAN tags, but could eventually include filtering
-- and other transformations)
data FrameOp
  = SetVLAN VLANTag
  | StripVLAN
  deriving (Eq, Show)

-- | String type containing the name of a network interface
newtype IfaceName = IfaceName String
  deriving (Eq, Show, Ord)

-- | A network interface on which traffic is being received or sent, associated
-- with some operations that are performed on every frame.
data Endpoint = Endpoint
  { ifaceName  :: IfaceName -- ^ Name of the network interface
  , trafficDir :: FlowDir   -- ^ Whether traffic should be sent or received
  , frameOps   :: [FrameOp] -- ^ Operations to perform on each frame that passes
                            -- through the interface
  }
  deriving (Eq, Show)

-- | The application's behavior abstractly modelled as a set of `Endpoint`s.
newtype StateModel = StateModel [Endpoint]
  deriving (Eq, Show)

-- | Representation of an effect/action to be performed by the program
data Instruction
  = EnableEndpoint Endpoint
  | DisableEndpoint Endpoint
  deriving (Eq, Show)

-- | Generate the list of instructions needed to completely transition from one
-- state to another
stepsBetween :: StateModel -> StateModel -> [Instruction]
stepsBetween (StateModel old) (StateModel new) =
  let
    toAdd    = EnableEndpoint <$> (new \\ old)
    toRemove = DisableEndpoint <$> (old \\ new)
  in toRemove ++ toAdd
