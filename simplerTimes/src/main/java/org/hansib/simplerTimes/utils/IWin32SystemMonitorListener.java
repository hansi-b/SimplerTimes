package org.hansib.simplerTimes.utils;

public interface IWin32SystemMonitorListener {

	default void onMachineLogon() {
	}

	default void onMachineLogoff() {
	}

	default void onMachineUnlocked() {
	}

	default void onMachineLocked() {
	}

	default void onMachineGoingToSuspend() {
	}

	default void onOther(int wParam, int lParam) {
	}

	default void onOtherPowerChange(int wParam, int lParam) {
	}
}
