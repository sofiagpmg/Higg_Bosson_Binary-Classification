# Higg_Bosson_Binary-Classification

Abstract: This is a classification problem to distinguish between a signal process which produces Higgs bosons and a background process which does not.


The data has been produced using Monte Carlo simulations. The first 21 features (columns 2-22) are kinematic properties measured by the particle detectors in the accelerator. The last seven features are functions of the first 21 features; these are high-level features derived by physicists to help discriminate between the two classes. There is an interest in using deep learning methods to obviate the need for physicists to manually develop such features. Benchmark results using Bayesian Decision Trees from a standard physics package and 5-layer neural networks are presented in the original paper. The last 500,000 examples are used as a test set.

class label (1 for signal, 0 for background)

source: https://archive.ics.uci.edu/ml/datasets/HIGGS#
