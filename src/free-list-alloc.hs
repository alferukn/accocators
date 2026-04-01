module FreeListAllocator where

type Address = Int
type Size = Int

data Block = Block {
        bAddr :: Address,
        bSize :: Size
} deriving (Show, Eq)

data AllocatorState = AllocatorState {
        freeBlocks :: [Block]
} deriving (Show, Eq)

initAllocator :: Size -> AllocatorState
initAllocator maxSize = AllocatorState {
        freeBlocks = [Block 0 maxSize]
}

allocate :: Size -> AllocatorState -> (Maybe Address, AllocatorState)
allocate needSize (AllocatorState blocks) =
        let (before, found) = break (\b -> bSize b >= needSize) blocks in
        case found of
                [] -> (Nothing, AllocatorState blocks)
                (targetBlock : after) -> (Just addr, AllocatorState newFreeBlocks) where
                        addr = bAddr targetBlock
                        newFreeBlock = Block (addr + needSize) (bSize targetBlock - needSize)
                        newFreeBlocks = before ++ (if bSize newFreeBlock > 0 then [newFreeBlock] else []) ++ after

deallocate :: Block -> AllocatorState -> AllocatorState
deallocate (Block deAddr deSize) (AllocatorState freeBlocks) =
        let endAddr = deAddr + deSize
            (before, nextBlock : after) = break (\ b -> endAddr <= bAddr b) freeBlocks
            nextAddr = bAddr nextBlock
            nextSize = bSize nextBlock
        in if endAddr == nextAddr
                then AllocatorState (before ++ [Block deAddr (deSize + nextSize)] ++ after)
                else AllocatorState (before ++ [Block deAddr deSize] ++ [nextBlock] ++ after)
