import matplotlib.pyplot as plt
import numpy as np


x = [5,10,15]
y = [1, 1.87, 2.54]
ym = [1, 2, 3]
e = [0, 0.18, 0.22]

plt.plot(x,y,'b.')
plt.hold(True)
plt.plot(x,ym,'k--')
plt.errorbar(x,y,yerr=e)

plt.show()
