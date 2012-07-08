import java.io.IOException;
import java.util.LinkedList;


public interface paneClient {
	/**
	 * call 'addUser' command.
	 * 
	 * @param user
	 *            the new user to be added.
	 * @return
	 * @throws IOException
	 */
	public boolean addUser(paneUser user) throws IOException;
	
	/**
	 * call 'grant' command.
	 * 
	 * @param share
	 *            name of the share
	 * @param user
	 *            name of the user.
	 * @return
	 * @throws IOException          
	 */
	public boolean grant(paneShare share, paneUser user) throws IOException;
	
	/**
	 * call 'newShare' command.
	 * 
	 * @param share
	 *            the new share to be created
	 * @return
	 * @throws IOException
	 */
	public boolean newShare(paneShare share) throws IOException;
	
	/**
	 * call 'reserve' command.
	 * 
	 * @param resv
	 *            the name of the share that is asking for bandwidth
	 * @return
	 * @throws IOException
	 */
	public boolean reserve(paneReservation resv) throws IOException;
	
	/**
	 * call 'allow' command.
	 * @param flowgroup
	 *            the flow group to be allowed
	 * @return
	 * @throws IOException
	 */
	public boolean allow(paneFlowGroup flowgroup) throws IOException;
	
	
	/**
	 * call 'deny' command. 
	 * @param flowgroup
	 *            the flow group to be denied
	 * @return
	 * @throws IOException
	 */
	public boolean deny(paneFlowGroup flowgroup) throws IOException;
	
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
