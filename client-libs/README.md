# PANE Client Libraries

Authors:
  Chen Liang (chen_liang@cs.brown.edu)
  Andrew Ferguson (adf@cs.brown.edu)

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

You can now import the project into eclipse using, File > Import > Existing
Projects into Workspace.
