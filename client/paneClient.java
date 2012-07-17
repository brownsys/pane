import java.io.IOException;
import java.net.InetAddress;
import java.util.LinkedList;


public interface paneClient {
	
	/**
	 * initialize contact information of server
	 * 
	 * @param serverIP
	 * @param serverPort
	 * @throws IOException 
	 */
	public void initialize(InetAddress serverIP, int serverPort) throws IOException;
	/**
	 * call 'addUser' command.
	 * 
	 * @param userName
	 *            name of the new user to be created.
	 * @return
	 * @throws IOException
	 */
	public paneUser addUser(String userName) throws IOException;

	
	/**
	 * call 'listShares' to list all shares associated to the given user
	 * @param user 
	 * @return
	 * @throws IOException
	 */
	public LinkedList<paneShare> listShares(paneUser user) throws IOException;
	
	public LinkedList<paneShare> listSharesByFlowGroup(paneFlowGroup flowgroup) throws IOException;
	
	public paneShare getRootShare() throws IOException;
}
