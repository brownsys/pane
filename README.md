<pre>
     _______  ____     ____   ___ _______
    /  __  / / __ \    |   \  | | | _____|
   /  /_/ / / /__\ \   | |\ \ | | | |___
  / _____/ / ______ \  | | \ \| | |  __|
 / /      / /      \ \ | |  \ \ | | |____
/_/      /_/        \_\|_|   \__| |______|

    A PROTOTYPE PARTICIPATORY NETWORK

</pre>


PANE is a prototype OpenFlow controller which implements Participatory Networking, an API for end-users, hosts and applications to take part in network management. PANE allows these principals to directly contact the network control-plane to  place requests for resources, provide hints about future traffic, or query the state of the network. PANE divides and delegates authority for network management using a hierarchy of "shares," which are also managed by interacting with the PANE server.


Code Layout
-------------------------
`/src/`            (Haskell source code)

`/src/Main.hs`     (Execution entry point)

`/scripts/`        (Support scripts for PANE)

`/tests/`          (Relatively basic traces of client-PANE interaction)

`/mn-tests/`       (More sophisticated tests run using Mininet)

`/coq/`            (Correctness proofs developed with Coq proof assistant)

`/client-libs/`    (Client libraries, see client-libs/README.md for more info)


Building PANE 
-------------------------

PANE requires:
 * Haskell compiler (tested with GHC 7.0.4)
 * Brown's fork of nettle-openflow: https://github.com/brownsys/nettle-openflow
 * additional build dependencies from http://hackage.haskell.org as required by cabal

It can be built using the following commands:

<pre>
$ cabal configure --enable-test
$ cabal build
</pre>

which is equivalent to running `make` in the top-level directory. After compiling, the provided `pane` symlink will link to the build output.

PANE has been built successfully on Mac OS X (10.6 and up) and Ubuntu (10.04 and up). 


Research
-------------------------
PANE is part of the [participatory networking research project](http://pane.cs.brown.edu) at [Brown University](http://www.cs.brown.edu). While still a research prototype, our primary internet connectivity has been provided by a PANE-controlled OpenFlow network since early February 2012.


License
-------------------------
PANE is provided under the 3-clause BSD license. See the file LICENSE for more details.
