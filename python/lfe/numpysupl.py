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


class PolynomialLinearModel:
    "A convenience class for creating polynomial linear models."
    def __init__(self, xs, ys, degree):
        (self.xs, self.ys) = (xs, ys)
        self.degree = degree
        self.y_mean = self.get_y_mean()
        (self.results, self.model, self.ys_predicted,
         self.ss_tot, self.ss_reg, self.ss_res,
         self.r_squared) = (None, None, None, None, None, None, None)
        (self.coeffs, self.residuals, self.rank,
         self.singular_values, self.rcond) = (None, None, None, None, None)
        self.polyfit()

    def polyfit(self):
        (self.coeffs, self.residuals, self.rank,
        self.singular_values, self.rcond) = np.polyfit(
            self.xs, self.ys, self.degree, full=True)
        self.model = np.poly1d(self.coeffs)
        self.ys_predicted = self.model(self.xs)
        self.ss_tot = self.get_ss_tot()
        self.ss_reg = self.get_ss_reg()
        self.ss_res = self.get_ss_res()
        self.r_squared = self.get_r_squared()
        self.results = {
            "coeffs": self.coeffs.tolist(),
            "residuals": self.residuals.tolist(),
            "rank": self.rank,
            "singular-values": self.singular_values.tolist(),
            "rcond": float(self.rcond),
            "y-mean": float(self.y_mean),
            "ss-tot": float(self.ss_tot),
            "ss-reg": float(self.ss_reg),
            "ss-res": float(self.ss_res),
            "r-squared": float(self.r_squared)}

    def predict(self, xs):
        return self.model(xs)

    def get_y_mean(self):
        return self.ys.sum() / self.ys.size

    def get_ss_tot(self):
        return ((self.ys - self.get_y_mean()) ** 2).sum()

    def get_ss_reg(self):
        return ((self.ys_predicted - self.get_y_mean()) ** 2).sum()

    def get_ss_res(self):
        return ((self.ys - self.ys_predicted) ** 2).sum()

    def get_r_squared(self):
        return 1 - self.get_ss_res() / self.get_ss_tot()

    def __str__(self):
        return str(self.results)

    def __repr__(self):
        return self.__str__()
