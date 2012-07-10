import java.io.IOException;
import java.util.LinkedList;


public interface paneClient {
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
	 * call 'Tick' command
	 * 
	 * @param time
	 * @return
	 * @throws IOException
	 */
	public boolean tick(int time) throws IOException;
	
	/**
	 * call 'listShares' to list all shares associated to the given user
	 * @param user 
	 * @return
	 * @throws IOException
	 */
	public LinkedList<paneShare> listShares(paneUser user) throws IOException;
	
	public LinkedList<paneShare> listSharesByFlowGroup(paneFlowGroup flowgroup) throws IOException;
}
