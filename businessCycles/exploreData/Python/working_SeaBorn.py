# -*- coding: utf-8 -*-
"""
Created on Tue Mar 19 18:11:20 2019

@author: javie
"""

firmsPlot(F1, range(1,41), ["Quality"], rows=4)
firmsPlot(F1, range(1,41), ["Price"], rows=4)
firmsPlot(F1, range(1,41), ["Profit"], rows=4)
firmsPlot(F1, range(1,11), ["ExpectedLimit"], rows=4)
firmsPlot(F1, range(1,41), ["Demand"], rows=4, facet_kws=dict(sharey=False))

firmsPlot(F3, range(1,41), ["Quality"], rows=4)
firmsPlot(F3, range(1,41), ["Price"], rows=4)
firmsPlot(F3, range(1,41), ["Profit"], rows=4)
firmsPlot(F3, range(1,21), ["ExpectedLimit"], rows=4)
firmsPlot(F3, range(1,41), ["Demand"], rows=4, facet_kws=dict(sharey=False))

firmsPlot(F3, [6], ["Quality", "Price", "Demand", "Profit"], cols=2, facet_kws=dict(sharey=False))

QD = NQDvsQD.loc[NQDvsQD["scenario"]=="QD"]
firmsPlot(QD, range(1,41), ["Quality"], "scenGr", rows=4)
firmsPlot(QD, range(1,41), ["Price"], "scenGr", rows=4)
firmsPlot(QD, range(1,41), ["Profit"], "scenGr", rows=4)
firmsPlot(QD, range(1,41), ["Demand"], "scenGr", rows=4, facet_kws=dict(sharey=False))


firmsPlot(NQDvsQD, range(1,41), ["Quality"], "scenGr", rows=4)
firmsPlot(NQDvsQD, range(1,41), ["Price"], "scenGr", rows=4)
firmsPlot(NQDvsQD, range(1,41), ["Profit"], "scenGr", rows=4)
firmsPlot(NQDvsQD, range(1,41), ["Demand"], "scenGr", rows=4, facet_kws=dict(sharey=False))

firmsPlot(NQDvsQD, [1, 6, 7, 9], ["Quality", "Price", "Profit"], "scenGr", rows=4, facet_kws=dict(sharey=False))

firmsPlot(SS3_gr, range(1,21), ["Quality"], "scenGr", rows=4)
firmsPlot(SS3_gr, range(1,21), ["Demand"], "scenGr", rows=4)

firmsPlot(SS3, range(1,41), ["Profit"], "scenGr", rows=4)
firmsPlot(SS3, range(1,41), ["ExpectedLimit"], "scenGr", rows=4)
firmsPlot(SS3, range(1,41), ["Demand"], "scenGr", rows=4, facet_kws=dict(sharey=False))
firmsPlot(SS3, range(1,41), ["DemandShare"], "scenGr", rows=4, facet_kws=dict(sharey=False))


F20[F20["tick"]==100].groupby("randomSeed").count().describe()
F40[F40["tick"]==100].groupby("randomSeed").count().describe()
