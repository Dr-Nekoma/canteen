### Questions
1. Do we actually need to represent individual files as trees? (The answer is probably yes, but directories and such will be trees regardless.)
  * Pros: easy to do file concatenation and slicing, updating a small part of the file doesn't require hashing the entire file, can store different parts of the same file on different machines.
  * Cons: would need to come up with a robust chunking scheme, would need to come up with a stable tree structure to go with the chunking scheme, finding the relevant chunks to read/write requires additional steps.
2. Do we want to centralize Client communication with only the Book Keepers, or should the Client also speak directly to the File System? (Actually, does it even make sense for the Client to distinguish the two, as opposed to simply communicating with the Node?)

Details of who does what may change depending on answer to 2, but steps should remain roughly the same.

### World in which files are not chunked

How to read a file, given a file name:
* Client: send a message to the Book Keeper asking for the current value of the file name
* Book Keeper: look up current hash associated with the file name
* Book Keeper: send hash back to Client
* Client: ask Book Keeper for a list of File Systems that are known to have the file associated with the hash
* Book Keeper: look up File Systems known to have the file
* Book Keeper: send list to Client
* Client: select a convenient File System and ask it for the contents associated with the hash
* File System: use hash to find location of the file on disk, send it to Client
* Client: be happy

How to write to a file, given a file name, position and payload:
* Client: send a message to the Book Keeper containing the file name, position and payload
* Book Keeper: look up current hash of file name
* Book Keeper: look up File Systems known to have the file identified by the hash
* Book Keeper: ask File System to write a new file, using the hash as a starting point, given the position and payload to write
* File System: copy file identified by the hash to a new location on disk
* File System: on the new file, write the payload starting at the position
* File System: hash the new file
* File System: send new hash back to Book Keeper
* Book Keeper: update file name to point to new hash received from File System
* Book Keeper: notify other Book Keepers that the file now points to the new hash
* Book Keeper: tell Client that the file was modified, possibly sending it the new hash
* Client: be happy

### World in which files are chunked

_TODO_

(Will involve path-copying and such.)
