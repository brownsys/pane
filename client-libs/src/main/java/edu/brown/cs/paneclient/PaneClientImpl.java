package edu.brown.cs.paneclient;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.Socket;
import java.util.List;

import edu.brown.cs.paneclient.PaneException.InvalidAuthenticateException;
import edu.brown.cs.paneclient.PaneException.InvalidUserException;


public class PaneClientImpl implements PaneClient {
	
	InetAddress _serverIP;
	int _serverPort;
	Socket _serverSock;
	
	public PaneClientImpl(InetAddress serverIP, int serverPort) throws IOException {
		_serverIP = serverIP;
		_serverPort = serverPort;
		_serverSock = new Socket(serverIP,serverPort);
		
		byte[] welcome = new byte[1000];
		_serverSock.getInputStream().read(welcome);
		System.out.println("welcome:" + new String(welcome));
	}
	
	public PaneUser addUser(PaneUser user) throws IOException, InvalidUserException {
		
		String cmd = "AddUser " + user.getName() + ".\n";
		String response = sendAndWait(cmd);		
		if (response.trim().equals("True")) {
			return user;
		} else {
			throw new PaneException.InvalidUserException(user.toString() + " -- response: " + response);
		}
		
	}

	public List<PaneShare> listShares(PaneUser user) throws IOException {
		return null;
	}
	
	public List<PaneShare> listSharesByFlowGroup(PaneFlowGroup flowgroup) throws IOException {
		return null;
	}

	@Override
	public PaneShare getRootShare() throws IOException {
		

		//---------------only for test purpose-------------------------
		PaneShare root = new PaneShare("rootShare",Integer.MAX_VALUE, null);
		root.setClient(this);
		//-------------------------------------------------------------
		
		return root;
	}

	@Override
	public PaneUser authenticate(String username) throws IOException, InvalidAuthenticateException {
		
		String response = sendAndWait(username + ".\n");
		if (response.trim().equals("logged in")) {	
			return new PaneUser(username, this);
		} else {
			throw new PaneException.InvalidAuthenticateException(username + " -- response: " + response);
		}
		
	}
	
	@Override
	public String sendAndWait(String cmd) throws IOException {

		byte[] ret = new byte[1000];
		OutputStream os = _serverSock.getOutputStream();
		InputStream is = _serverSock.getInputStream();
		
		os.write(cmd.getBytes());
		os.flush();
		/*
		 *occasionally, the returned value comes along with the prompt,
		 *for example, return value may be 'True \n root>' in one response,
		 *or may be 'True' and then there comes '\n root>' as another read, in 
		 *this case the latter string may be treated as the response for the
		 * next command. So the prompt needs to be considered.
		 */
		is.read(ret);
		String response = new String(ret);
		if (!new String(ret).contains(">")) {
			//eliminate prompt for this cmd, if prompt is
			//not in this response, it would be in the next read,
			//therefore, read one more time(but just discard the prompt)
			byte[] prompt = new byte[100];
			is.read(prompt);
		} else {
			//the prompt is in this read, just remove it
			
			String[] substrings = response.split(" |\n");
			for (String s : substrings) {
				if (s.contains(">"))
					response = response.replace(s, "");
			}
		}
		
		return response;
	}
	
	@Override
	public void send(String cmd) throws IOException {
		OutputStream os = _serverSock.getOutputStream();
		os.write(cmd.getBytes());
		os.flush();
	}
	
	
	@Override
	public String toString() {
		return "PaneClientImpl: serverIP: " + _serverIP + " serverPort: " + _serverPort;
	}

}
