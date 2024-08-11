# ---
# jupyter:
#   jupytext:
#     text_representation:
#       extension: .py
#       format_name: percent
#       format_version: '1.3'
#       jupytext_version: 1.16.1
#   kernelspec:
#     display_name: Python 3 (ipykernel)
#     language: python
#     name: python3
# ---

# %% [markdown]
# # Decomposing a square matrix into eigenvectors and eigenvalues
#
#

# %%
import numpy as np

# %%
rng = np.random.default_rng()

# %% [markdown]
# ## Create random eigenvectors
#
# We'll start with a random 3x3 matrix, **X**, and compute its qr factorization, where q is orthonormal and r is upper-triangular. We'll use Q for our eigenvectors.

# %%
X = np.random.normal(size=(3,3))
print(X)

# %%
Q, R = np.linalg.qr(X)
print(Q)

# %% [markdown]
# ### Q should be invertable, but let's make sure

# %%
np.linalg.det(Q)

# %%
np.linalg.inv(Q)

# %% [markdown]
# $ \mathbf{Q}\mathbf{Q}^{-1} = \mathbf{I}$

# %%
Q @ np.linalg.inv(Q)

# %% [markdown]
# ## Create eigenvalues
#
# Let's create a vector of eigenvalues in decreasing order: [a, a/2, a/3] = [6/11, 3/11, 2/11]. 

# %%
eigen_vals = np.array([6/11, 3/11, 2/11])

# %% [markdown]
# ## Compose A
#
# $ \mathbf{A}=\mathbf{Q}\mathbf{\Lambda}\mathbf{Q}^{-1} $

# %%
A = Q @ np.diag(eigen_vals) @ np.linalg.inv(Q)
print(A)

# %% [markdown]
# ## Now do the decomposition
#
# Let's get back our eigenstuff.

# %%
eigen_vals, eigen_vectors = np.linalg.eig(A)

# %% [markdown]
# Annoyingly, it's not sorted, so let's sort it.

# %%
iis = np.flip(np.argsort(eigen_vals))
eigen_vals = eigen_vals[iis]
eigen_vectors = eigen_vectors[:,iis]

print(eigen_vals)
print(eigen_vectors)

# %% [markdown]
# ## Visualize eigenvectors in 2D

# %%
A = np.array(
    [[2.1, 1.4],
     [0.7, 1.3]]
)

# %%
v = np.array([[np.cos(theta), np.sin(theta)] for theta in np.linspace(0, 2*np.pi, num=100)])

# %%
import matplotlib.pyplot as plt

# %%
theta = np.linspace(0, 2*np.pi, num=100)
v = np.stack([np.cos(theta), np.sin(theta)])

# %%
v.shape

# %%
u = A @ v

# %%
eigen_vals, eigen_vectors = np.linalg.eig(A)

# %%
eigen_vectors, eigen_vals

# %%
plt.axes().set_aspect('equal')

plt.plot(v[0,:], v[1,:])
plt.plot(u[0,:], u[1,:])

ev = np.stack([(0,0), eigen_vectors[:,0]]) * eigen_vals[0]
plt.plot(ev[:,0], ev[:,1])

ev = np.stack([(0,0), eigen_vectors[:,1]]) * eigen_vals[1]
plt.plot(ev[:,0], ev[:,1])

# %%
