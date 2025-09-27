// High-performance manual memory management for Lexicon text engine
// Two-tier allocator: BuddySystemAllocator (Tier 2) + SlabAllocator (Tier 1)

// Memory layout constants
const MEMORY_BASE: usize = 1024; // First 1024 bytes reserved for allocator metadata
const MIN_BLOCK_SIZE: u32 = 16;  // Minimum allocation size (2^4)
const MAX_BLOCK_SIZE: u32 = 65536; // Maximum block size (2^16 = 64KB)
const MAX_ORDER: u32 = 12; // log2(MAX_BLOCK_SIZE) - log2(MIN_BLOCK_SIZE)

// Get the order (power of 2) for a given size
function getOrder(size: u32): u32 {
  if (size <= MIN_BLOCK_SIZE) return 0;
  return 32 - clz(size - 1) - 4; // clz = count leading zeros
}

// Get size from order
function getSize(order: u32): u32 {
  return MIN_BLOCK_SIZE << order;
}

// Round up to next power of 2
function roundUpToPowerOf2(size: u32): u32 {
  if (size <= MIN_BLOCK_SIZE) return MIN_BLOCK_SIZE;
  return 1 << (32 - clz(size - 1));
}

// Free block structure for buddy allocator
@unmanaged
class FreeBlock {
  next: FreeBlock | null;
  prev: FreeBlock | null;
  
  constructor() {
    this.next = null;
    this.prev = null;
  }
}

// Buddy System Allocator (Tier 2)
@unmanaged
class BuddySystemAllocator {
  // Array of free lists for each order (power of 2 size)
  freeLists: StaticArray<FreeBlock | null>;
  baseAddress: usize;
  totalSize: usize;
  
  constructor(baseAddress: usize, totalSize: usize) {
    this.baseAddress = baseAddress;
    this.totalSize = totalSize;
    
    // Initialize free lists
    this.freeLists = new StaticArray<FreeBlock | null>(MAX_ORDER + 1);
    for (let i: u32 = 0; i <= MAX_ORDER; i++) {
      this.freeLists[i] = null;
    }
    
    // Add initial large block to appropriate free list
    const initialOrder = getOrder(totalSize as u32);
    const initialBlock = changetype<FreeBlock>(baseAddress);
    this.addToFreeList(initialBlock, initialOrder);
  }
  
  // Add block to free list for given order
  private addToFreeList(block: FreeBlock, order: u32): void {
    block.next = this.freeLists[order];
    block.prev = null;
    
    if (this.freeLists[order]) {
      this.freeLists[order]!.prev = block;
    }
    
    this.freeLists[order] = block;
  }
  
  // Remove block from free list
  private removeFromFreeList(block: FreeBlock, order: u32): void {
    if (block.prev) {
      block.prev!.next = block.next;
    } else {
      this.freeLists[order] = block.next;
    }
    
    if (block.next) {
      block.next!.prev = block.prev;
    }
  }
  
  // Get buddy address using XOR
  private getBuddyAddress(address: usize, size: u32): usize {
    return address ^ size;
  }
  
  // Check if address is aligned to size
  private isAligned(address: usize, size: u32): bool {
    return (address & (size - 1)) == 0;
  }
  
  // Allocate memory block
  alloc(size: u32): usize {
    if (size == 0) return 0;
    
    const order = getOrder(size);
    const actualSize = getSize(order);
    
    // Find available block of requested size or larger
    let currentOrder = order;
    while (currentOrder <= MAX_ORDER && !this.freeLists[currentOrder]) {
      currentOrder++;
    }
    
    if (currentOrder > MAX_ORDER) {
      return 0; // Out of memory
    }
    
    // Get block from free list
    const block = this.freeLists[currentOrder]!;
    this.removeFromFreeList(block, currentOrder);
    
    // Split block down to required size
    while (currentOrder > order) {
      currentOrder--;
      const buddySize = getSize(currentOrder);
      const buddyAddress = changetype<usize>(block) + buddySize;
      const buddy = changetype<FreeBlock>(buddyAddress);
      this.addToFreeList(buddy, currentOrder);
    }
    
    return changetype<usize>(block);
  }
  
  // Free memory block
  free(address: usize, size: u32): void {
    if (address == 0) return;
    
    let order = getOrder(size);
    let currentAddress = address;
    let currentSize = getSize(order);
    
    // Coalesce with buddy blocks
    while (order < MAX_ORDER) {
      const buddyAddress = this.getBuddyAddress(currentAddress, currentSize);
      
      // Check if buddy is free (simplified check - in real implementation
      // we'd maintain a bitmap or other tracking structure)
      let buddyFree = false;
      let buddy = this.freeLists[order];
      
      while (buddy) {
        if (changetype<usize>(buddy) == buddyAddress) {
          buddyFree = true;
          this.removeFromFreeList(buddy, order);
          break;
        }
        buddy = buddy.next;
      }
      
      if (!buddyFree) break;
      
      // Merge with buddy
      if (buddyAddress < currentAddress) {
        currentAddress = buddyAddress;
      }
      order++;
      currentSize = getSize(order);
    }
    
    // Add coalesced block to free list
    const block = changetype<FreeBlock>(currentAddress);
    this.addToFreeList(block, order);
  }
}

// Slab structure for slab allocator
@unmanaged
class Slab {
  next: Slab | null;
  prev: Slab | null;
  dataAddress: usize; // Address of the actual object storage area
  freeList: usize; // Intrusive free list head
  freeCount: u32;
  totalObjects: u32;
  
  constructor(totalObjects: u32, dataAddress: usize) {
    this.next = null;
    this.prev = null;
    this.dataAddress = dataAddress;
    this.freeList = 0;
    this.freeCount = totalObjects;
    this.totalObjects = totalObjects;
  }
}

// Cache for specific object types
@unmanaged
class SlabCache {
  objectSize: u32;
  slabSize: u32;
  objectsPerSlab: u32;
  
  slabsFull: Slab | null;
  slabsPartial: Slab | null;
  slabsFree: Slab | null;
  
  buddyAllocator: BuddySystemAllocator;
  
  constructor(objectSize: u32, buddyAllocator: BuddySystemAllocator) {
    this.buddyAllocator = buddyAllocator;
    this.objectSize = roundUpToPowerOf2(objectSize);
    this.slabSize = 4096; // 4KB slabs
    this.objectsPerSlab = this.slabSize / this.objectSize;
    
    this.slabsFull = null;
    this.slabsPartial = null;
    this.slabsFree = null;
  }
  
  // Add slab to specific list
  private addToSlabList(slab: Slab, list: Slab | null): Slab {
    slab.next = list;
    slab.prev = null;
    
    if (list) {
      list.prev = slab;
    }
    
    return slab;
  }
  
  // Remove slab from any list it's in
  private removeFromSlabList(slab: Slab): void {
    if (slab.prev) {
      slab.prev!.next = slab.next;
    }
    
    if (slab.next) {
      slab.next!.prev = slab.prev;
    }
  }
  
  // Create new slab
  private createSlab(): Slab | null {
    // Allocate memory for the slab data area
    const slabDataAddress = this.buddyAllocator.alloc(this.slabSize);
    if (slabDataAddress == 0) return null;
    
    // Allocate separate memory for the slab metadata
    const slabMetaAddress = this.buddyAllocator.alloc(sizeof<Slab>() as u32);
    if (slabMetaAddress == 0) {
      this.buddyAllocator.free(slabDataAddress, this.slabSize);
      return null;
    }
    
    const slab = changetype<Slab>(slabMetaAddress);
    slab.next = null;
    slab.prev = null;
    slab.dataAddress = slabDataAddress;
    slab.freeCount = this.objectsPerSlab;
    slab.totalObjects = this.objectsPerSlab;
    
    // Initialize intrusive free list in the data area
    let objectAddress = slabDataAddress;
    slab.freeList = objectAddress;
    
    for (let i: u32 = 0; i < this.objectsPerSlab - 1; i++) {
      const nextAddress = objectAddress + this.objectSize;
      store<usize>(objectAddress, nextAddress);
      objectAddress = nextAddress;
    }
    
    // Last object points to null
    store<usize>(objectAddress, 0);
    
    return slab;
  }
  
  // Allocate object from cache
  alloc(): usize {
    let slab: Slab | null = null;
    
    // Try partial slabs first
    if (this.slabsPartial) {
      slab = this.slabsPartial;
    } else if (this.slabsFree) {
      // Move free slab to partial
      slab = this.slabsFree;
      this.removeFromSlabList(slab!);
      this.slabsPartial = this.addToSlabList(slab!, this.slabsPartial);
    } else {
      // Create new slab
      slab = this.createSlab();
      if (!slab) return 0; // Out of memory
      
      this.slabsPartial = this.addToSlabList(slab!, this.slabsPartial);
    }
    
    // Get object from free list
    const objectAddress = slab!.freeList;
    slab!.freeList = load<usize>(objectAddress);
    slab!.freeCount--;
    
    // Move slab to full list if needed
    if (slab!.freeCount == 0) {
      this.removeFromSlabList(slab!);
      this.slabsFull = this.addToSlabList(slab!, this.slabsFull);
    }
    
    return objectAddress;
  }
  
  // Find slab that contains the given object address
  private findSlabForObject(objectAddress: usize): Slab | null {
    // Check all slab lists
    const lists = [this.slabsFull, this.slabsPartial, this.slabsFree];
    
    for (let listIdx = 0; listIdx < 3; listIdx++) {
      let current = lists[listIdx];
      while (current) {
        const dataStart = current.dataAddress;
        const dataEnd = dataStart + this.slabSize;
        
        if (objectAddress >= dataStart && objectAddress < dataEnd) {
          return current;
        }
        current = current.next;
      }
    }
    
    return null;
  }
  
  // Free object back to cache
  free(objectAddress: usize): void {
    const slab = this.findSlabForObject(objectAddress);
    if (!slab) return; // Object not found
    
    const wasFull = slab.freeCount == 0;
    
    // Add object back to free list
    store<usize>(objectAddress, slab.freeList);
    slab.freeList = objectAddress;
    slab.freeCount++;
    
    // Move slab between lists if needed
    if (wasFull) {
      // Move from full to partial
      this.removeFromSlabList(slab);
      this.slabsPartial = this.addToSlabList(slab, this.slabsPartial);
    } else if (slab.freeCount == slab.totalObjects) {
      // Move to free list
      this.removeFromSlabList(slab);
      this.slabsFree = this.addToSlabList(slab, this.slabsFree);
    }
  }
}

// Global memory manager instance
let buddyAllocator: BuddySystemAllocator | null = null;
let ropeNodeCache: SlabCache | null = null;

// Initialize memory management system
export function initMemoryManager(): void {
  const memorySize = memory.size() * 65536; // Pages to bytes
  const availableMemory = memorySize - <u32>MEMORY_BASE;
  
  buddyAllocator = changetype<BuddySystemAllocator>(
    MEMORY_BASE - sizeof<BuddySystemAllocator>()
  );
  buddyAllocator = new BuddySystemAllocator(MEMORY_BASE, availableMemory);
  
  // Initialize rope node cache (we'll define RopeNode size later)
  ropeNodeCache = new SlabCache(64, buddyAllocator!); // 64 byte objects
}

// Public allocation functions
export function allocBlock(size: u32): usize {
  if (!buddyAllocator) return 0;
  return buddyAllocator!.alloc(size);
}

export function freeBlock(address: usize, size: u32): void {
  if (!buddyAllocator) return;
  buddyAllocator!.free(address, size);
}

export function allocRopeNode(): usize {
  if (!ropeNodeCache) return 0;
  return ropeNodeCache!.alloc();
}

export function freeRopeNode(address: usize): void {
  if (!ropeNodeCache) return;
  ropeNodeCache!.free(address);
}