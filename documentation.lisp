#|
 This file is a part of lru-cache
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lru-cache)

(docs:define-docs
  (type lru-cache
    "A least-recently-used cache datastructure.

See MAKE-LRU-CACHE
See LRU-CACHE-SIZE
See LRU-CACHE-PUSH
See LRU-CACHE-POP
See LRU-CACHE-ID
See LRU-CACHE-EVICT
See LRU-CACHE-CLEAR
See LRU-CACHE-COUNT
See MAP-LRU-CACHE
See DO-LRU-CACHE")

  (function make-lru-cache
    "Create a new LRU-CACHE instance that can hold SIZE elements.

The TEST may be one of the arguments acceptable as the TEST of a
hash-table.

See CL:HASH-TABLE-TEST
See LRU-CACHE")

  (function lru-cache-size
    "Returns the size of the cache.

This operation is O(1).

See LRU-CACHE")

  (function lru-cache-push
    "Pushes an element to the cache.

The element is put into the cache as the most recently used. If the
element did not exist before and the cache is already full, the least
recently used element is evicted.

Returns the ID of the node the element was put into, if it was not
already in the cache. If the element was already in the cache, NIL is
returned.

This operation is O(1).

See LRU-CACHE
See LRU-CACHE-POP")
  
  (function lru-cache-pop
    "Pops an element out of the cache.

If the element exists in the cache, the ID of the node is returned. If
the element did not exist in the cache, NIL is returned.

This operation is O(1).

See LRU-CACHE
See LRU-CACHE-PUSH")
  
  (function lru-cache-id
    "Returns the ID of the node the element is in.

If the element is not in the cache, NIL is returned.

This operation is O(1).

See LRU-CACHE")
  
  (function lru-cache-evict
    "Evicts the least recently used element from the cache.

Returns the element and the ID of the evicted node, if any.

This operation is O(n).

See LRU-CACHE")
  
  (function lru-cache-clear
    "Evicts all elements in the cache and clears it.

This operation is O(n).

See LRU-CACHE")

  (function lru-cache-count
    "Counts the number of elements currently in the cache.

This operation is O(n).

See LRU-CACHE
See LRU-CACHE-SIZE")
  
  (function map-lru-cache
    "Iterates over the cached elements in order from most recent to least recent.

Calls FUNCTION with two values: the element and the ID of the node the
element occupies. Note that the ID does *not* necessarily correspond
to the index of the node in the cache sequence.

Returns the CACHE instance.

See LRU-CACHE
See DO-LRU-CACHE")
  
  (function do-lru-cache
    "Convenience macro to iterate over the cached elements in order from most recent to least recent.

An implicit NIL block is bound around BODY.
If BODY does not explicitly return, RESULT is evaluated and returned instead.

See LRU-CACHE
See MAP-LRU-CACHE"))
