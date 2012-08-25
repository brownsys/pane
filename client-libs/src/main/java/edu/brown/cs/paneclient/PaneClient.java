package edu.brown.cs.paneclient;

import java.io.IOException;
import java.util.List;


public interface PaneClient {
	

	public PaneUser addUser(PaneUser user) throws IOException, PaneException.InvalidUserException;

	public List<PaneShare> listShares(PaneUser user) throws IOException;
	
	public List<PaneShare> listSharesByFlowGroup(PaneFlowGroup flowgroup) throws IOException;
	
	public PaneShare getRootShare() throws IOException;
	
	public PaneUser authenticate(String username) throws IOException, PaneException.InvalidAuthenticateException;
	
	public String sendAndWait(String cmd) throws IOException;
	
	public void send(String cmd) throws IOException;
	
	public String toString();
}
