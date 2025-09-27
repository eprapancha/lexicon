// Test suite for memory management system

import { 
  initMemoryManager, 
  allocBlock, 
  freeBlock, 
  allocRopeNode, 
  freeRopeNode 
} from "../assembly/memory";

// Test basic buddy allocator functionality
function testBuddyAllocator(): void {
  console.log("Testing BuddySystemAllocator...");
  
  // Test basic allocation
  const ptr1 = allocBlock(32);
  assert(ptr1 != 0, "Should allocate 32 bytes");
  
  const ptr2 = allocBlock(64);
  assert(ptr2 != 0, "Should allocate 64 bytes");
  
  const ptr3 = allocBlock(128);
  assert(ptr3 != 0, "Should allocate 128 bytes");
  
  // Test that pointers are different
  assert(ptr1 != ptr2, "Pointers should be different");
  assert(ptr2 != ptr3, "Pointers should be different");
  assert(ptr1 != ptr3, "Pointers should be different");
  
  // Test freeing
  freeBlock(ptr1, 32);
  freeBlock(ptr2, 64);
  freeBlock(ptr3, 128);
  
  // Test reallocation after free
  const ptr4 = allocBlock(32);
  assert(ptr4 != 0, "Should reallocate after free");
  
  freeBlock(ptr4, 32);
  
  console.log("BuddySystemAllocator tests passed!");
}

// Test slab allocator functionality
function testSlabAllocator(): void {
  console.log("Testing SlabAllocator...");
  
  // Test rope node allocation
  const node1 = allocRopeNode();
  assert(node1 != 0, "Should allocate rope node");
  
  const node2 = allocRopeNode();
  assert(node2 != 0, "Should allocate second rope node");
  
  const node3 = allocRopeNode();
  assert(node3 != 0, "Should allocate third rope node");
  
  // Test that pointers are different
  assert(node1 != node2, "Node pointers should be different");
  assert(node2 != node3, "Node pointers should be different");
  assert(node1 != node3, "Node pointers should be different");
  
  // Test freeing
  freeRopeNode(node1);
  freeRopeNode(node2);
  freeRopeNode(node3);
  
  // Test reallocation after free
  const node4 = allocRopeNode();
  assert(node4 != 0, "Should reallocate after free");
  
  freeRopeNode(node4);
  
  console.log("SlabAllocator tests passed!");
}

// Test memory stress scenarios
function testMemoryStress(): void {
  console.log("Testing memory stress scenarios...");
  
  // Allocate many small blocks
  const ptrs: usize[] = [];
  for (let i = 0; i < 100; i++) {
    const ptr = allocBlock(16);
    if (ptr != 0) {
      ptrs.push(ptr);
    }
  }
  
  assert(ptrs.length > 50, "Should allocate at least 50 blocks");
  
  // Free every other block
  for (let i = 0; i < ptrs.length; i += 2) {
    freeBlock(ptrs[i], 16);
  }
  
  // Try to allocate larger blocks (should trigger coalescing)
  const largePtr = allocBlock(256);
  assert(largePtr != 0, "Should allocate large block after fragmentation");
  
  // Clean up
  freeBlock(largePtr, 256);
  for (let i = 1; i < ptrs.length; i += 2) {
    freeBlock(ptrs[i], 16);
  }
  
  console.log("Memory stress tests passed!");
}

// Test rope node cache stress
function testSlabStress(): void {
  console.log("Testing slab allocator stress...");
  
  // Allocate many rope nodes
  const nodes: usize[] = [];
  for (let i = 0; i < 1000; i++) {
    const node = allocRopeNode();
    if (node != 0) {
      nodes.push(node);
    }
  }
  
  assert(nodes.length > 500, "Should allocate at least 500 rope nodes");
  
  // Free all nodes
  for (let i = 0; i < nodes.length; i++) {
    freeRopeNode(nodes[i]);
  }
  
  // Reallocate to test cache reuse
  const newNode = allocRopeNode();
  assert(newNode != 0, "Should reallocate from cache");
  
  freeRopeNode(newNode);
  
  console.log("Slab stress tests passed!");
}

// Main test runner
export function runMemoryTests(): void {
  console.log("Starting memory management tests...");
  
  // Initialize the memory system
  initMemoryManager();
  
  // Run all tests
  testBuddyAllocator();
  testSlabAllocator();
  testMemoryStress();
  testSlabStress();
  
  console.log("All memory management tests passed!");
}