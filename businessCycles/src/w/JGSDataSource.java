package w;

import java.lang.reflect.Method;

import businessCycles.Firm;
import repast.simphony.data2.AggregateDataSource;
import repast.simphony.data2.NonAggregateDataSource;

public class JGSDataSource implements AggregateDataSource,
		NonAggregateDataSource {

	String valueName = "";;
	int opLevCohort = 0;
	int timeCohort = 0;
	boolean count = false;

	@Override
	public String getId() {
		if (count)
			return "Count_OL:" + opLevCohort + "_T:" + timeCohort;
		else
			return valueName + "_OL:" + opLevCohort + "_T:" + timeCohort;
	}

	@Override
	public Class<?> getDataType() {
		return Double.class;
	}

	@Override
	public Class<?> getSourceType() {
		return Firm.class;
	}

	@Override
	public Object get(Object obj) {
		try {

			Method m = Firm.class.getDeclaredMethod("get" + valueName,
					(Class<?>[]) null);

			return m.invoke(obj, (Object[]) null);

		} catch (Exception e) {

			e.printStackTrace();
			return 0.0;

		}

	}

	/*
	 * It calculates the mean of the firms that belong to the specific time and
	 * operat lev cohorts.
	 */
	@Override
	public Object get(Iterable<?> objs, int size) {

		int i = 0;
		double sum = 0.0;

		for (Object obj : objs) {
			if (checkCohorts((Firm) obj)) {

				if (!count)
					try {
						Method m = Firm.class.getDeclaredMethod("get"
								+ valueName, (Class<?>[]) null);

						sum += (double) m.invoke(obj, (Object[]) null);

					} catch (Exception e) {

						e.printStackTrace();
						return 0.0;

					}

				i++;
			}

		}

		if (i != 0 && !count)
			return sum / i;
		else
			return i;

	}

	private boolean checkCohorts(Firm f) {

		if (timeCohort != 0 && timeCohort != f.getTimeCohort())
			return false;
		else if (opLevCohort != 0 && opLevCohort != f.getOpLevCohort())
			return false;
		else
			return true;

	}

	@Override
	public void reset() {

	}

}
