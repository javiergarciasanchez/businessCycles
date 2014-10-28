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

/**
 *
 * Import the needed packages.
 *
 */
import java.awt.Component;

import repast.simphony.context.Context;
import repast.simphony.engine.environment.RunEnvironment;
import repast.simphony.engine.schedule.*;
import repast.simphony.ui.probe.*;
import repast.simphony.util.collections.IndexedIterable;
import static repast.simphony.essentials.RepastEssentials.*;

import javax.swing.*;

/**
 * 
 * This is an agent.
 * 
 */
public class ModelInitializer {

	
	@ScheduledMethod(start = 0d, shuffle = true)
	public static void initializeModel() {


		if (checkParam()){
			@SuppressWarnings("unchecked")
			Context<Object> context = FindContext("businessCycles");

			// Previous supply manager should be eliminated
			IndexedIterable<Object> tmpSuppManList = context.getObjects(SupplyManager.class);
			if (tmpSuppManList.size()== 1){
				context.remove(tmpSuppManList.get(0));
			}

			new SupplyManager(context);
			RunEnvironment.getInstance().endAt((Double) GetParameter("stopAt"));
			
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

		return true;
	}

	@ProbeID()
	public String toString() {

		// Set the default agent identifier.
		return "ModelInitializer";

	}

}
