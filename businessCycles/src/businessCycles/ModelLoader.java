package businessCycles;

import static repast.simphony.essentials.RepastEssentials.EndSimulationRun;
import static repast.simphony.essentials.RepastEssentials.GetParameter;

import java.awt.Component;

import javax.swing.JOptionPane;

import repast.simphony.context.Context;
import repast.simphony.context.DefaultContext;
import repast.simphony.dataLoader.ContextBuilder;
import repast.simphony.engine.environment.RunEnvironment;
import repast.simphony.random.RandomHelper;
import repast.simphony.util.collections.IndexedIterable;

public class ModelLoader extends DefaultContext<Object> implements
		ContextBuilder<Object> {

	@Override
	public Context<Object> build(Context<Object> context) {

		context.setId("businessCycles");

		RandomHelper.setSeed((Integer) GetParameter("randomSeed"));

		if (checkParam()) {

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

		return context;

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
