package org.hansib.simplerTimes.utils;

import java.util.ArrayList;

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

public class Win32SystemMonitor implements WindowProc {

	/*
	 * Notifies applications that a power-management event has occurred.
	 */
	static final int WM_POWERBROADCAST = 536;

	/*
	 * Power status has changed.
	 */
	static final int PBT_APMPOWERSTATUSCHANGE = 10;
	static final int PBT_APMRESUMESUSPEND = 7;
	static final int PBT_APMSUSPEND = 4;
	static final int PBT_POWERSETTINGCHANGE = 32787;

	private static final ArrayList<IWin32SystemMonitorListener> LISTENERS = new ArrayList<>();
	public static final Win32SystemMonitor instance = new Win32SystemMonitor();

	public static void removeListener(IWin32SystemMonitorListener listener) {
		LISTENERS.remove(listener);
	}

	public static void addListener(IWin32SystemMonitorListener listener) {
		LISTENERS.add(listener);
	}

	private Win32SystemMonitor() {
		new Thread(() -> {
			HMODULE hInst = Kernel32.INSTANCE.GetModuleHandle("");

			String windowClass = "AnotherWindowClass";

			WNDCLASSEX wClass = new WNDCLASSEX();
			wClass.hInstance = hInst;
			wClass.lpfnWndProc = Win32SystemMonitor.this;
			wClass.lpszClassName = windowClass;

			// register window class
			User32.INSTANCE.RegisterClassEx(wClass);

			// create new window
			HWND hWnd = User32.INSTANCE.CreateWindowEx(User32.WS_EX_TOPMOST, windowClass,
					"My hidden helper window, used only to catch the windows events", 0, 0, 0, 0, 0, null, null, hInst,
					null);

			Wtsapi32.INSTANCE.WTSRegisterSessionNotification(hWnd, Wtsapi32.NOTIFY_FOR_THIS_SESSION);

			MSG msg = new MSG();
			while (User32.INSTANCE.GetMessage(msg, hWnd, 0, 0) != 0) {
				User32.INSTANCE.TranslateMessage(msg);
				User32.INSTANCE.DispatchMessage(msg);
			}

			Wtsapi32.INSTANCE.WTSUnRegisterSessionNotification(hWnd);
			User32.INSTANCE.UnregisterClass(windowClass, hInst);
			User32.INSTANCE.DestroyWindow(hWnd);
		}).start();
	}

	/*
	 * uMSG 689 => session change uMSG 536 => power change
	 */
	@Override
	public LRESULT callback(HWND hwnd, int uMsg, WPARAM wParam, LPARAM lParam) {
		switch (uMsg) {
		case WM_POWERBROADCAST: {
			this.onPowerChange(wParam, lParam);
			return new LRESULT(0);
		}
		case WinUser.WM_DESTROY: {
			User32.INSTANCE.PostQuitMessage(0);
			return new LRESULT(0);
		}
		case WinUser.WM_SESSION_CHANGE: {
			this.onSessionChange(wParam, lParam);
			return new LRESULT(0);
		}
		default:
			return User32.INSTANCE.DefWindowProc(hwnd, uMsg, wParam, lParam);
		}
	}

	/**
	 * On session change.
	 *
	 * @param wParam the w param
	 * @param lParam the l param
	 */
	protected void onSessionChange(WPARAM wParam, LPARAM lParam) {
		switch (wParam.intValue()) {
		case Wtsapi32.WTS_SESSION_LOGON -> this.onMachineLogon(lParam.intValue());
		case Wtsapi32.WTS_SESSION_LOGOFF -> this.onMachineLogoff(lParam.intValue());
		case Wtsapi32.WTS_SESSION_LOCK -> this.onMachineLocked(lParam.intValue());
		case Wtsapi32.WTS_SESSION_UNLOCK -> this.onMachineUnlocked(lParam.intValue());
		default -> this.onOther(wParam.intValue(), lParam.intValue());
		}
	}

	protected void onOther(int wParam, int lParam) {
		for (IWin32SystemMonitorListener l : LISTENERS) {
			l.onOther(wParam, lParam);
		}
	}

	protected void onPowerChange(WPARAM wParam, LPARAM lParam) {
		switch (wParam.intValue()) {
		case PBT_APMSUSPEND -> this.onMachineGoingToSuspend();
		default -> this.onOtherPowerChange(wParam.intValue(), lParam.intValue());
		}
	}

	protected void onOtherPowerChange(int wParam, int lParam) {
		for (IWin32SystemMonitorListener l : LISTENERS) {
			l.onOtherPowerChange(wParam, lParam);
		}
	}

	protected void onMachineGoingToSuspend() {
		for (IWin32SystemMonitorListener l : LISTENERS) {
			l.onMachineGoingToSuspend();
		}
	}

	protected void onMachineLocked(int sessionId) {
		for (IWin32SystemMonitorListener l : LISTENERS) {
			l.onMachineLocked();
		}
	}

	protected void onMachineUnlocked(int sessionId) {
		for (IWin32SystemMonitorListener l : LISTENERS) {
			l.onMachineUnlocked();
		}
	}

	protected void onMachineLogon(int sessionId) {
		for (IWin32SystemMonitorListener l : LISTENERS) {
			l.onMachineLogon();
		}
	}

	protected void onMachineLogoff(int sessionId) {
		for (IWin32SystemMonitorListener l : LISTENERS) {
			l.onMachineLogoff();
		}
	}
}
