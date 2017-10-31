Jordan Alligood and Ryan Stillings

Our solution should work for any input, the distributed version might not be
correctly cleaning up actors between runs though.  The run()/1 function sets
up all of the actors (on the given node in the distributed version).  Then it
calls the supervisor function and starts the first election.  The supervisor is
an actor that runs on the main process.  It holds a list of all of the other
actor's PIDs, as well as handling the beginning and ending of each election.
Each actor behaves pretty much the way specified in the PDF.