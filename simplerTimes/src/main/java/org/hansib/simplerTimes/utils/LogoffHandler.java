/* Copyright (c) 2012 Tobias Wolf, All Rights Reserved
 * 
 * The contents of this file is dual-licensed under 2 
 * alternative Open Source/Free licenses: LGPL 2.1 or later and 
 * Apache License 2.0. (starting with JNA version 4.0.0).
 * 
 * You can freely decide which license you want to apply to 
 * the project.
 * 
 * You may obtain a copy of the LGPL License at:
 * 
 * http://www.gnu.org/licenses/licenses.html
 * 
 * A copy is also included in the downloadable source code package
 * containing JNA, in file "LGPL2.1".
 * 
 * You may obtain a copy of the Apache License at:
 * 
 * http://www.apache.org/licenses/
 * 
 * A copy is also included in the downloadable source code package
 * containing JNA, in file "AL2.0".
 */
package org.hansib.simplerTimes.utils;

import java.io.IOException;
import java.util.logging.FileHandler;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;

import org.apache.logging.log4j.LogManager;

import com.sun.jna.platform.win32.Kernel32;
import com.sun.jna.platform.win32.User32;
import com.sun.jna.platform.win32.WinDef.HMODULE;
import com.sun.jna.platform.win32.WinDef.HWND;
import com.sun.jna.platform.win32.WinDef.LPARAM;
import com.sun.jna.platform.win32.WinDef.LRESULT;
import com.sun.jna.platform.win32.WinDef.WPARAM;
import com.sun.jna.platform.win32.WinUser;
import com.sun.jna.platform.win32.WinUser.MSG;
import com.sun.jna.platform.win32.WinUser.WNDCLASSEX;
import com.sun.jna.platform.win32.WinUser.WindowProc;
import com.sun.jna.platform.win32.Wtsapi32;

public class LogoffHandler implements WindowProc {

	private static final org.apache.logging.log4j.Logger log = LogManager.getLogger();

	private static final int WM_ENDSESSION = 0x0016;
	private static final int WM_QUERYENDSESSION = 0x0011;

	/**
	 * Instantiates a new win32 window test.
	 */
	public LogoffHandler() {

		// define new window class
		String windowClass = "MyWindowClass";
		HMODULE hInst = Kernel32.INSTANCE.GetModuleHandle("");

		WNDCLASSEX wClass = new WNDCLASSEX();
		wClass.hInstance = hInst;
		wClass.lpfnWndProc = LogoffHandler.this;
		wClass.lpszClassName = windowClass;

		// register window class
		User32.INSTANCE.RegisterClassEx(wClass);

		// create new window
		HWND hWnd = User32.INSTANCE.CreateWindowEx(User32.WS_EX_TOPMOST, windowClass,
				"My hidden helper window, used only to catch the windows events", 0, 0, 0, 0, 0, null, // WM_DEVICECHANGE
																										// contradicts
																										// parent=WinUser.HWND_MESSAGE
				null, hInst, null);

		log.info("window sucessfully created! window hwnd: " + hWnd.getPointer().toString());

		Wtsapi32.INSTANCE.WTSRegisterSessionNotification(hWnd, Wtsapi32.NOTIFY_FOR_THIS_SESSION);

		MSG msg = new MSG();
		while (User32.INSTANCE.GetMessage(msg, hWnd, 0, 0) != 0) {
			User32.INSTANCE.TranslateMessage(msg);
			User32.INSTANCE.DispatchMessage(msg);
		}

		Wtsapi32.INSTANCE.WTSUnRegisterSessionNotification(hWnd);
		User32.INSTANCE.UnregisterClass(windowClass, hInst);
		User32.INSTANCE.DestroyWindow(hWnd);

		log.info("program exit!");
	}

	public LRESULT callback(HWND hwnd, int uMsg, WPARAM wParam, LPARAM lParam) {
		switch (uMsg) {
		case WM_ENDSESSION: {
			log.info("WM_ENDSESSION");
			return new LRESULT(1);
		}
		case WM_QUERYENDSESSION: {
			// This should stop shutdown, but does not work on Windows 10
			log.info("WM_QUERYENDSESSION");
			return new LRESULT(1);
		}
		case WinUser.WM_SESSION_CHANGE: {
			switch (wParam.intValue()) {
			case Wtsapi32.WTS_CONSOLE_CONNECT: {
				log.info("WTS_CONSOLE_CONNECT");
				break;
			}
			case Wtsapi32.WTS_CONSOLE_DISCONNECT: {
				log.info("WTS_CONSOLE_DISCONNECT");
				break;
			}
			case Wtsapi32.WTS_SESSION_LOGON: {
				log.info("WTS_SESSION_LOGON");
				break;
			}
			case Wtsapi32.WTS_SESSION_LOGOFF: {
				log.info("WTS_SESSION_LOGOFF");
				break;
			}
			case Wtsapi32.WTS_SESSION_LOCK: {
				log.info("WTS_SESSION_LOCK");
				break;
			}
			case Wtsapi32.WTS_SESSION_UNLOCK: {
				log.info("WTS_SESSION_UNLOCK");
				break;
			}
			}
			return new LRESULT(0);
		}
		default:
			return User32.INSTANCE.DefWindowProc(hwnd, uMsg, wParam, lParam);
		}
	}

	public static void main(String[] args) throws IOException {
		FileHandler fh = new FileHandler("c:/test.log");
		fh.setFormatter(new SimpleFormatter());
		Logger.getLogger("").addHandler(fh);
		new LogoffHandler();
	}
}
