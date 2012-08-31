# PANE Client Libraries

These files provide client libraries for applications to join in the management
and configuration of the network using Participatory Networking.

http://pane.cs.brown.edu


## Build and Installation

The Java library -- edu.brown.cs.paneclient -- is provided as a Maven project.
To compile and install:

	mvn package
	mvn install


## Eclipse Development

As a simple Maven project, the client libraries can easily be developed using
Eclipse. It is necessary to first set the M2_REPO variable for your workspace
(This command only needs to be executed once per workspace):

	mvn -Declipse.workspace=<path-to-eclipse-workspace> eclipse:configure-workspace

Next, build the libraries and create the Eclipse project files:

	mvn install -DskipTests
	mvn eclipse:eclipse

You can now import the project into Eclipse using, File > Import > Existing
Projects into Workspace.

If you wish to view the source or JavaDocs of the libraries' dependencies, you
add `-DdownloadSources=true` or `-DdownloadJavadocs=true` when creating the
Eclipse project files in the final step.
