/**
 * 
 * This file was automatically generated by the Repast Simphony Agent Editor.
 * Please see http://repast.sourceforge.net/ for details.
 * 
 */

/**
 *
 * Set the package name.
 *
 */
package businessCycles;

import java.awt.Component;

import repast.simphony.context.Context;
import repast.simphony.engine.environment.RunEnvironment;
import repast.simphony.engine.schedule.*;
import repast.simphony.util.collections.IndexedIterable;
import static repast.simphony.essentials.RepastEssentials.*;

import javax.swing.*;

public class ModelInitializer {

	@ScheduledMethod(start = 0d)
	public static void initializeModel() {

		if (checkParam()) {
			@SuppressWarnings("unchecked")
			Context<Object> context = FindContext("businessCycles");

			// Previous supply manager should be eliminated
			IndexedIterable<Object> tmpSuppManList = context
					.getObjects(SupplyManager.class);
			if (tmpSuppManList.size() == 1) {
				context.remove(tmpSuppManList.get(0));
			}

			new SupplyManager(context);

			Firm.readParams();

			RunEnvironment.getInstance().endAt(
					(Double) GetParameter("stopAt")
							* (Integer) GetParameter("periods"));

		} else {
			EndSimulationRun();
		}

	}

	private static boolean checkParam() {
		/*
		 * Check consistency of Learning rate Mean
		 */
		double lRMean = (Double) GetParameter("learningRateMean");

		if (lRMean >= 1.0 || lRMean <= 0.5) {
			Component frame = null;
			JOptionPane.showMessageDialog(frame,
					"The Learning Rate Mean should be < 1 and > 0.5",
					"Inconsistent Learning Rate Parameter",
					JOptionPane.ERROR_MESSAGE);
			return false;
		}

		/*
		 * Check consistency of Recession Magnitude
		 */
		double dElast = (Double) GetParameter("demandElasticity");
		double sElast = (Double) GetParameter("supplyElasticity");
		double maxRecMag = (dElast + sElast) / (sElast * (1.0 + dElast));

		// Read magnitude of recessions
		String[] recMags = ((String) GetParameter("recessionMagnitude"))
				.split(":");
		for (int i = 0; i < recMags.length; i++) {
			if (Double.parseDouble(recMags[i]) >= maxRecMag) {
				Component frame = null;
				JOptionPane.showMessageDialog(
						frame,
						"The recession magnitude is too big.\n"
								+ String.format(
										"Recession magnitude should be < %.3f",
										maxRecMag),
						"Inconsistent Recession Magnitude Parameter",
						JOptionPane.ERROR_MESSAGE);
				return false;
			}
		}
		return true;
	}
}
