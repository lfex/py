"""
Supplemental functions for NumPy
"""
import numpy as np


def norm1d(array):
    shifted = array - (array.min())
    return shifted / shifted.max()


def scale1d(array, min=0, max=1):
    scaled = max * norm1d(array)
    return scaled + min
