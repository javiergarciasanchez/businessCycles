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

import static java.lang.Math.*;
import static repast.simphony.essentials.RepastEssentials.*;
import repast.simphony.context.Context;

/**
 * 
 * This is an agent.
 * 
 */
public class Firm {

	public static SupplyManager supplyManager;
	
	// Local state variables - change every period
	private double quantity;
	private double capital = 0.0;
	private double nextCapital = 0.0;
	
	private double perPeriodPerformance = 0.0;
	private double acumQ = 0.0;
	private double acumProfit = 0.0;
	private double maxFunding = 0.0;
	private double invest = 0.0;
	private double flexibilityCost = 0.0;
	private double optimalMarkUp = 0.0;
	private double markUp = 0.0;

	// Local firm variables - stable along firm life
	private double firstUnitCost = 0.0;
	private double operatingLeverage = 0.0;
	private double expon = 0.0;
	private double born = 0.0;

	// Static Firm variables - identical for all firms
	private static double capitalProductivityPerPeriod;
	private static double minVarCost;
	private static double minCapital;
	private static double costOfCapitalPerPeriod;

	private static double perfWeight;
	private static double minPerformance;

	private static double demElast;
	private static double supElast;

	private static double opLevMean;
	private static double opLevStdDev;
	private static double flexCostMean;
	private static double flexCostStdDev;
	private static double deprecPerPeriod;
	private static double maxExtFundPerPeriod;
	private static double invParam;

	private static long agentIDCounter;
	private long agentID = agentIDCounter++;

	public Firm(Context<Object> context) {

		context.add(this);

		born = GetTickCount();

		// A minimum first unit cost is set to 10% of mean
		firstUnitCost = max(0.1 * (Double) GetParameter("firstUnitCostMean"),
				supplyManager.firstUnitCostNormal.nextDouble());

		capital = max((Double) GetParameter("minimumCapital"), supplyManager.iniKNormal.nextDouble());

		operatingLeverage = min(1.0, max(0.0, supplyManager.operatingLeverageNormal.nextDouble()));

		// Initial perfomance is set to minPerformance
		perPeriodPerformance = minPerformance;

		// 0.5 < learning rate <= 1.0
		double learningRate = min(1.0, max(supplyManager.learningRateDistrib.nextDouble(), 0.51));
		expon = log(learningRate) / log(2.0);

	}

	public static void readParams() {
		// Static Firm variables - identical for all firms

		int periods = (Integer) GetParameter("periods");

		capitalProductivityPerPeriod = (Double) GetParameter("capitalProductivity") / periods;
		minVarCost = (Double) GetParameter("minVarCost");
		minCapital = (Double) GetParameter("minimumCapital");
		costOfCapitalPerPeriod = (Double) GetParameter("costOfCapital") / periods;

		perfWeight = (Double) GetParameter("performanceWeight");
		minPerformance = (Double) GetParameter("minimumPerformance");

		demElast = (Double) GetParameter("demandElasticity");
		supElast = (Double) GetParameter("supplyElasticity");

		opLevMean = (Double) GetParameter("operatingLeverageMean");
		opLevStdDev = (Double) GetParameter("operatingLeverageStdDev") * opLevMean;
		flexCostMean = (Double) GetParameter("flexibilityCostMean");
		flexCostStdDev = (Double) GetParameter("flexibilityCostStdDev") * flexCostMean;
		deprecPerPeriod = (Double) GetParameter("depreciation") / periods;
		maxExtFundPerPeriod = (Double) GetParameter("maxExternalFunding") / periods;
		invParam = (Double) GetParameter("investmentParam");

		agentIDCounter = 1;
	}

	public boolean checkEntry() {

		double expectedQ = Demand.getCapacityUsed() * capital * capitalProductivityPerPeriod;
		
		double expectedAnnualReturn = profit(expectedQ, getPrice()) *
				supplyManager.periods / getCapital();

		return (expectedAnnualReturn >= minPerformance);

	}

	public double offer() {

		if (getBorn() < GetTickCount()) {
			capital = nextCapital;
		}

		quantity = Demand.getCapacityUsed() * capital * capitalProductivityPerPeriod;

		return quantity;

	}

	public double profit(double q, double p) {

		return p * q - totalProdCost(q) - totalNonProdCost();

	}

	/*
	 * Calculates cost using learning curve: cost of new accumulated Q minus old
	 * accumulated Q. See http://maaw.info/LearningCurveSummary.htm (Wright model)
	 */
	private double totalProdCost(double quantity) {

		// Get Learning curve cost + productive minimum cost
		double lc = firstUnitCost * (pow(acumQ + quantity, 1.0 + expon) - pow(acumQ, 1.0 + expon))
				+ minVarCost * quantity;

		// Return flexibility adjusted value
		return lc * operatingLeverageAdjustment();

	}

	/*
	 * Adds the cost of flexibility. Increases variable cost in firms that have
	 * lower fixed costs. If opLev = 100% (All costs depend on size) adjustment = 1
	 * If capacityUsed = 100% adjustment = flexibilityCost
	 */
	private double operatingLeverageAdjustment() {

		flexibilityCost = flexCostMean + (opLevMean - operatingLeverage) * flexCostStdDev / opLevStdDev;

		// Flexibility cost cannot be lower than 10% of mean
		flexibilityCost = max(0.1 * flexCostMean, flexibilityCost);

		return flexibilityCost * (operatingLeverage + (1 - operatingLeverage) * Demand.getCapacityUsed());

	}

	private double totalNonProdCost() {
		return (costOfCapitalPerPeriod + deprecPerPeriod) * capital;
	}

	/**
	 * 
	 * Process demand respond and returns false if firm exits the industry,
	 * otherwise returns true
	 * 
	 * @method processDemandResponse
	 * 
	 */
	public boolean processDemandResponse() {

		// Define the return value variable.
		boolean returnValue;

		// Calculates profit & Performance
		perPeriodPerformance = perfWeight * perPeriodPerformance + (1 - perfWeight) * getPerPeriodReturn();

		// if it is an exit, returns false
		double performance = perPeriodPerformance * supplyManager.periods;
		returnValue = !(performance < minPerformance || capital < minCapital);

		acumQ += quantity;
		acumProfit += getProfit();

		return returnValue;

	}

	public void plan() {

		maxFunding = getProfit() + deprecPerPeriod * capital
				+ ((SupplyManager.getRecesMagnitude() > 0.0) ? 0.0 : (maxExtFundPerPeriod * capital));

		invest = min(maxFunding, capital * (deprecPerPeriod + netInvestment()));

		invest = max(0.0, invest);

		nextCapital = capital * (1.0 - deprecPerPeriod) + invest;

	}

	/*
	 * It is derived maximizing economic profit (ie substracting cost of capital)
	 */
	private double marginalCost() {

		return (firstUnitCost * (1.0 + expon) * pow(acumQ + quantity, expon) + minVarCost)
				* operatingLeverageAdjustment()
				+ (costOfCapitalPerPeriod + deprecPerPeriod) / capitalProductivityPerPeriod;

	}

	private double netInvestment() {

		optimalMarkUp = (demElast + (1 - getMarketShare()) * supElast)
				/ (demElast + (1 - getMarketShare()) * supElast - getMarketShare());

		markUp = getPrice() / marginalCost();

		return invParam * (1 - optimalMarkUp / markUp);

	}

	/*
	 * Methods to probe firm state
	 */
	public double getMarketShare() {

		return getQuantity() / supplyManager.getTotalQuantity();

	}

	public double getPrice() {
		return supplyManager.price;
	}

	public double getQuantity() {
		return quantity;
	}

	public double getProfit() {
		return profit(getQuantity(), getPrice());
	}

	public double getPerformance() {
		return perPeriodPerformance;
	}

	private double getPerPeriodReturn() {
		return getProfit() / getCapital();
	}

	public double getReturn() {
		return getPerPeriodReturn() * supplyManager.periods;
	}

	public double getFirstUnitCost() {
		return firstUnitCost;
	}

	public double getAcumQuantity() {
		return acumQ;
	}

	public String toString() {
		return "Firm" + this.agentID;
	}

	public long getFirmNumID() {
		return agentID;
	}

	public double getBorn() {
		return born;
	}

	public double getAge() {
		return GetTickCount() - getBorn();
	}

	public double getExpon() {
		return expon;
	}

	public double getAcumProfit() {
		return acumProfit;
	}

	public double getMedCost() {
		return (getPrice() * getQuantity() - getProfit()) / getQuantity();
	}

	public double getTotalVarCost() {
		return totalProdCost(getQuantity());
	}

	public double getVarCost() {
		return getTotalVarCost() / getQuantity();
	}

	public double getVarCostWoAdj() {
		return getVarCost() / getOperatingLeverageAdjustment();
	}

	public double getTotalFixedCost() {
		return totalNonProdCost();
	}

	public double getFixedCostPerUnit() {
		return getTotalFixedCost() / getQuantity();
	}

	public double getOperatingLeverage() {
		return operatingLeverage;
	}

	public double getOperatingLeverageAdjustment() {
		return operatingLeverageAdjustment();
	}

	public double getCapital() {
		return capital;
	}

	public double getMaxFunding() {
		return maxFunding;
	}

	public double getInvest() {
		return invest;
	}

	public static double getOpLevMean() {
		return opLevMean;
	}

	public static double getOpLevStdDev() {
		return opLevStdDev;
	}

	public double getFlexibilityCost() {
		return flexibilityCost;
	}

	public static double getFlexCostMean() {
		return flexCostMean;
	}

	public static double getFlexCostStdDev() {
		return flexCostStdDev;
	}

	public double getOptimalMarkUp() {
		return optimalMarkUp;
	}

	public double getMarkUp() {
		return markUp;
	}

}
