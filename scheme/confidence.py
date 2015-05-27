import numpy as np
import scipy as sp
import scipy.stats
import sys

# http://stackoverflow.com/questions/15033511/compute-a-confidence-interval-from-sample-data
if __name__ == '__main__':
    confidence=0.95
    a = 1.0*np.array(eval(sys.argv[1]))
    n = len(a)
    m, se = np.mean(a), scipy.stats.sem(a)
    h = se * sp.stats.t._ppf((1+confidence)/2., n-1)
    print("%.4f\t%.4f" % (m, h))
