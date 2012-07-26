import java.io.IOException;
import java.util.LinkedList;


public interface PaneClient {
	

	public PaneUser addUser(String userName) throws IOException;

	public LinkedList<PaneShare> listShares(PaneUser user) throws IOException;
	
	public LinkedList<PaneShare> listSharesByFlowGroup(PaneFlowGroup flowgroup) throws IOException;
	
	public PaneShare getRootShare() throws IOException;
	
	public boolean authenticate(PaneUser user) throws IOException;
	
	public String sendAndWait(String cmd) throws IOException;
	
	public void send(String cmd) throws IOException;
	
	public String toString();
}
